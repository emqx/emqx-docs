# MosquittoからEMQXへの移行

本ガイドでは、既存のEclipse Mosquitto環境をEMQXへ移行する手順を説明します。軽量な単一インスタンスのブローカーから、スケーラブルで分散型のMQTTプラットフォームへ移行したい管理者向けに設計されています。移行では、EMQXの標準MQTTプロトコル互換性を活用し、設定、認証情報、統合ロジックの移行方法を明確に示します。

## 移行の概要

移行プロセスは主に以下の3フェーズで構成されます。

1. **Mosquitto資産のインベントリ**：設定ファイル（`mosquitto.conf`）、セキュリティ関連ファイル（パスワードファイル、ACL、証明書）を収集し、現在のデータフローを把握します。
2. **EMQXの設定**：Mosquittoの設定をEMQXのHOCON形式設定ファイル（`emqx.conf`）に変換し、ユーザー認証情報をインポート、アクセス制御やデータ統合をルールエンジンで再構築します。
3. **デバイスおよび統合の更新**：デバイスをEMQXクラスターに切り替え（ポート互換性により多くの場合シームレス）、システム動作を検証します。

| パラメーター／アーティファクト | Mosquitto（例） | EMQX（例） | 備考 |
| :--- | :--- | :--- | :--- |
| **メイン設定ファイル** | `/etc/mosquitto/mosquitto.conf` | `/etc/emqx/emqx.conf` | EMQXは階層型のHOCON形式を使用します。 |
| **ネットワークポート** | `1883`（TCP）、`8883`（SSL） | `1883`（TCP）、`8883`（SSL） | 標準ポートは一致し、通常デバイスの再設定は不要です。 |
| **ユーザー認証情報** | `/etc/mosquitto/passwd` | 内蔵データベース（Mnesia） | 既存のパスワードハッシュをAPI経由でインポート可能。 |
| **アクセス制御** | `/etc/mosquitto/acl_file` | `/etc/emqx/acl.conf` | Allow/Denyルールの直接マッピング。 |
| **ブリッジ** | `connection bridge_name` | データコネクター＆ルール | 静的ブリッジを動的データルーティングに置換。 |
| **永続化** | `mosquitto.db` | `data/`（Mnesia + RocksDB） | EMQXはセッション永続化を自動管理します。 |

## フェーズ1：Mosquitto資産のインベントリ

### 設定ファイルと証明書の収集

主要な設定ファイルの場所を特定します。通常、`mosquitto.conf`に定義されています。

* **メイン設定:** `include_dir` またはデフォルトの `/etc/mosquitto/mosquitto.conf`
* **証明書:** `certfile`、`keyfile`、`cafile` のパスを確認
* **セキュリティ:** `password_file` と `acl_file` の場所を特定

証明書ファイル（`server.crt`、`server.key`、`ca.crt`）はEMQXノードの通常 `/etc/emqx/certs/` 配下にコピーしてください。

### 認証と認可の分析

認証方式を確認します：

* **パスワードファイル:** 最も一般的。EMQXの内部データベースに移行します。
* **プラグイン（mosquitto-auth-plug）:** SQLやLDAPを使用している場合は、対応するEMQX認証バックエンドを直接設定します。

## フェーズ2：MosquittoのベースラインをEMQXに再現

### MQTTリスナーの再作成

Mosquittoはリスナーを順次定義しますが、EMQXはタイプ別（TCP、SSL、WebSocket）にグループ化して`emqx.conf`で設定します。

**Mosquitto（`mosquitto.conf`）:**

```properties
# デフォルトリスナー
port 1883
max_connections -1

# SSLリスナー
listener 8883
certfile /etc/mosquitto/certs/server.crt
keyfile /etc/mosquitto/certs/server.key
```

**EMQX（`emqx.conf`）:**

```hocon
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_connections = infinity
}

listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    certfile = "/etc/emqx/certs/server.crt"
    keyfile  = "/etc/emqx/certs/server.key"
  }
}
```

### MQTT設定オプションのマッピング

クライアント動作を一貫させるため、主要なプロトコル設定を変換します。

| Mosquittoディレクティブ | EMQX HOCONパラメーター | 説明 |
| :--- | :--- | :--- |
| `max_queued_messages` | `mqtt.max_mqueue_len` | クライアントごとのオフラインメッセージ最大バッファ数。 |
| `persistent_client_expiration` | `mqtt.session_expiry_interval` | 切断後のセッション状態保持時間。 |
| `message_size_limit` | `mqtt.max_packet_size` | 許容されるMQTTパケット最大サイズ。 |
| `log_dest file` | `log.file.enable = true` | ファイルログを有効化。 |

**注意:** Mosquittoはセッション有効期限をグローバルに管理しますが、EMQX（MQTT 5.0）はクライアント単位のセッション有効期限をサポートします。MQTT 3.1.1クライアント向けには、EMQXでグローバルデフォルトを設定しMosquittoのポリシーに合わせられます。

### 認証の移行

EMQXは複数の認証バックエンドをサポートします。多くのMosquitto移行では、既存の認証情報を保持し、ユーザーのパスワードリセットを不要にすることが目標です。

#### オプション1：ユーザーの再作成（バッチインポート）

元の平文パスワードがある場合、EMQX HTTP APIを使って一括インポートできます。

**バッチインポートCSVフォーマット：**  
`users.csv`ファイルを以下の形式で作成します。

```csv
user_id,password,is_superuser
device001,secret123,false
admin,adminPass,true
```

**インポートコマンド：**  
`curl`でファイルをアップロードします。`type=plain`はEMQXにパスワードをインポート時にハッシュ化させる指定です。

```bash
curl -v -u admin:public -X POST \
  -H "Content-Type: multipart/form-data" \
  -F "filename=@users.csv" \
  "http://localhost:18083/api/v5/authentication/password_based:built_in_database/import_users?type=plain"
```

* `admin:public`はダッシュボードの認証情報に置き換えてください。
* 認証方式（`password_based:built_in_database`）が設定と一致していることを確認してください。

#### オプション2：Mosquittoパスワードファイルのインポート（上級者向け）

大量のユーザーがいて、ハッシュ化された`mosquitto.passwd`ファイルのみを持つ場合は、Erlangスクリプトを使いEMQXの内蔵データベースに直接インポート可能です。

**ステップ1：認証設定**

インポート前に、EMQXで[パスワードベース認証](../../guides/access-control/authn/pwoverview.md)を[内蔵データベース](../../guides/access-control/authn/mnesia.md)バックエンドで設定します。Mosquittoのデフォルトハッシュ方式に合わせるには以下の設定が必要です。

* **アルゴリズム:** `pbkdf2`
* **Mac関数:** `sha512`
* **イテレーション数:** 101
* **DK長:** 32

> **注意:** これらのパラメーター（101回のイテレーション、sha512）はMosquittoのデフォルトに完全一致します。EMQXの標準デフォルト（より強力なセキュリティ優先）とは異なりますが、インポートした認証情報を検証するために必要です。

**ステップ2：パスワードファイルのコピー**

`mosquitto.passwd`ファイルをEMQXサーバー（例：`/tmp/mosquitto.passwd`）にコピーし、`emqx`ユーザーが読み取り可能にしてください。

**ステップ3：インポートスクリプトの実行**

EMQXノード上で以下コマンドを実行します。ファイルを読み込み、Base64でエンコードされたソルトとハッシュをデコードし、ユーザー情報をデータベースに直接書き込みます。

```bash
emqx eval "
File = \"/tmp/mosquitto.passwd\",
{ok, Bin} = file:read_file(File),
Lines = binary:split(Bin, <<\"\n\">>, [global, trim]),
lists:foreach(fun(Line) ->
    case binary:split(Line, <<\":\">>) of
        [Username, <<\"\$7$\", Rest/binary>>] ->
            [_, SaltB64, HashB64] = binary:split(Rest, <<\"$\">>, [global]),
            Salt = base64:decode(SaltB64),
            Hash = binary:part(emqx_utils:bin_to_hexstr(base64:decode(HashB64), lower), 0, 64),
            Record = {user_info, {'mqtt:global', Username}, Hash, Salt, false},
            mnesia:dirty_write(emqx_authn_mnesia, Record);
        _ -> ok
    end
end, Lines)."
```

##### 代替案：外部データベース

既存のユーザー管理システムと統合が必要なエンタープライズ環境では、MySQLやPostgreSQLなどの外部SQLデータベースにユーザーを移行可能です。EMQXは動的SQLクエリをサポートし、さまざまなスキーマ形式に柔軟に対応します。

#### オプション3：相互TLS（mTLS）

MosquittoでX.509クライアント証明書（相互TLS）を用いた認証を使用している場合、EMQXリスナーでピア証明書の検証を設定します。

**Mosquitto設定例：**

```properties
require_certificate true
use_identity_as_username true
cafile /etc/mosquitto/ca.crt
```

**EMQX設定例：**

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    cacertfile = "/etc/emqx/certs/ca.crt"
    verify = verify_peer
    fail_if_no_peer_cert = true
  }
}
```

* Mosquittoで使用していたCA証明書（`ca.crt`）をEMQXにコピーしてください。
* `use_identity_as_username`が有効な場合、EMQXは`verify_peer`有効時にデフォルトでCommon Name（CN）をユーザー名として使用します。

### 認可（ACL）の移行

認証完了後、Mosquittoのトピックレベルアクセス制御をEMQXのポリシーに合わせて移行します。

MosquittoのACL構文はEMQXの`acl.conf`と非常に似ています。

**Mosquitto（`acl_file`）:**

```properties
user Alice
topic read sensors/#
pattern write devices/%u/data
```

**EMQX（`acl.conf`）:**

```erlang
{allow, {user, "Alice"}, subscribe, ["sensors/#"]}.
{allow, all, publish, ["devices/${username}/data"]}.
```

* `%u`は`${username}`（または`${clientid}`）に置換してください。
* `read`は`subscribe`、`write`は`publish`にマッピングします。

### データ統合の設定（ブリッジ＆スクリプトの置換）

Mosquittoはメッセージ転送にブリッジを、データ処理に外部スクリプト（Python/Node.js）を使用します。EMQXではこれらを内蔵の[ルールエンジン](../../develop/data-integration/rules.md)と[データ統合](../../develop/data-integration/data-bridges.md)で置き換えます。

> EMQXルールエンジンは、メッセージの選択、フィルタリング、変換を行い、コネクター経由で外部システムに転送できます。

**例：別ブローカーへのデータ転送**  
`mosquitto.conf`の`connection bridge_name`の代わりに：

1. EMQXダッシュボードで**MQTTブローカーコネクター**を作成。
2. ルールを作成し（例：`SELECT * FROM "#"`)、コネクターへ転送。

**例：Python処理スクリプトの置換**  
`sensors/+/temp`をサブスクライブし、30度以上の値をフィルタリングしてDBに書き込むスクリプトがある場合：

1. スクリプトを廃止。
2. ルールを作成：

    ```sql
    SELECT payload.temp as temperature, topic, timestamp
    FROM "sensors/+/temp"
    WHERE temperature > 30
    ```

3. アクションを追加し、InfluxDBやHTTPなどのデータ統合で結果を書き込み。

## フェーズ3：デバイスおよび統合の更新

### クライアント接続の更新

EMQXは標準MQTTポート（1883/8883）を使用するため、DNS経由で接続している多くのデバイスは設定変更不要です。DNSレコードを更新し、`mqtt.yourdomain.com`をEMQXクラスターのロードバランサーまたはIPに向けてください。

### 接続確認

EMQXダッシュボードでデバイス接続状況を監視します。

* **接続数**を確認

  > 以下コマンドでも接続を確認可能です：
  >
  > ```bash
  > emqx_ctl clients list
  > ```
  >
  > またはダッシュボードの**モニタリング** -> **クライアント**から確認できます。

* 認証エラーはログで確認してください（ハッシュアルゴリズムの不一致や証明書不足が多い原因です）。

## 高度な移行シナリオ

このセクションは任意で、ゼロダウンタイム移行が必要な場合に適用します。

### ブリッジ移行戦略（ゼロダウンタイム）

サービス停止なしで移行するには：

1. Mosquittoと並行してEMQXをデプロイ。
2. MosquittoからEMQXへ全メッセージを転送するブリッジを設定。

    ```properties
    # mosquitto.conf
    connection migrate_uplink
    address emqx-server:1883
    topic # out 0
    topic # in 0
    ```

3. バックエンドアプリケーションをEMQXに切り替え。両方のブローカーからデータを受信可能。
4. デバイスを段階的にEMQXエンドポイントへ切り替え。
5. Mosquittoの接続がなくなったらブリッジを削除し、Mosquittoを停止。

## 検証チェックリスト

本番トラフィック切替前に以下を確認してください。

- **リスナー:** TCP（1883）およびSSL（8883）ポートが開き、接続を受け入れている。
- **認証:** 既存認証情報でユーザーがログイン可能。
- **ACL:** ユーザーが許可されたトピックに制限されている。
- **データフロー:** デバイスからパブリッシュされたメッセージがサブスクライバーやバックエンドに届いている。
- **永続化:** ブローカー再起動後も保持メッセージが利用可能（`retain_available = true`を確認）。

## まとめ

MosquittoからEMQXへの移行は、プロトコル互換性を維持しつつ、スケーラビリティと信頼性を大幅に向上させます。既存設定をマッピングし、EMQXのルールエンジンで外部スクリプトやブリッジを置き換えることで、アーキテクチャを簡素化し、大規模成長に対応可能なインフラを構築できます。
