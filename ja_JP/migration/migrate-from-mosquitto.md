# Mosquitto から EMQX への移行

本ガイドでは、既存の Eclipse Mosquitto デプロイメントを EMQX に移行する手順を説明します。軽量で単一インスタンスのブローカーから、スケーラブルで分散型の MQTT プラットフォームへ移行を検討している管理者向けに設計されています。移行は EMQX の標準 MQTT プロトコル互換性を活用し、設定、認証情報、および統合ロジックの移行を明確に示します。

## 移行の概要

移行プロセスは以下の3つの主要フェーズで構成されます。

1. **Mosquitto 資産の棚卸し**：設定ファイル（`mosquitto.conf`）、セキュリティ関連ファイル（パスワードファイル、ACL、証明書）を収集し、現在のデータフローを把握します。
2. **EMQX の設定**：Mosquitto の設定を EMQX の HOCON 形式の設定ファイル（`emqx.conf`）に変換し、ユーザー認証情報をインポートし、ルールエンジンを使ってアクセス制御やデータ統合を再構築します。
3. **デバイスおよび統合の更新**：デバイスを EMQX クラスターにリダイレクト（ポート互換性により多くの場合シームレス）し、システムの動作を検証します。

| パラメーター / 資産 | Mosquitto（例） | EMQX（例） | 備考 |
| :--- | :--- | :--- | :--- |
| **メイン設定** | `/etc/mosquitto/mosquitto.conf` | `/etc/emqx/emqx.conf` | EMQX は階層型の HOCON 形式を使用。 |
| **ネットワークポート** | `1883`（TCP）、`8883`（SSL） | `1883`（TCP）、`8883`（SSL） | 標準ポートは一致。通常デバイスの再設定不要。 |
| **ユーザー認証情報** | `/etc/mosquitto/passwd` | 組み込みデータベース（Mnesia） | 既存のパスワードハッシュを API 経由でインポート可能。 |
| **アクセス制御** | `/etc/mosquitto/acl_file` | `/etc/emqx/acl.conf` | Allow/Deny ルールの直接マッピング。 |
| **ブリッジ** | `connection bridge_name` | データコネクター＆ルール | 静的ブリッジを動的データルーティングに置換。 |
| **パーシステンス** | `mosquitto.db` | `data/`（Mnesia + RocksDB） | EMQX はセッション永続化を自動管理。 |

## フェーズ 1：Mosquitto 資産の棚卸し

### 設定ファイルと証明書の収集

主要な設定ファイルの場所を特定します。通常は `mosquitto.conf` 内で定義されています。

* **メイン設定**：`include_dir` またはデフォルトの `/etc/mosquitto/mosquitto.conf`
* **証明書**：`certfile`、`keyfile`、`cafile` のパスを確認
* **セキュリティ**：`password_file` と `acl_file` の場所を特定

証明書ファイル（`server.crt`、`server.key`、`ca.crt`）は EMQX ノードの通常 `/etc/emqx/certs/` 配下にコピーします。

### 認証と認可の分析

認証方式を確認します：

* **パスワードファイル**：最も一般的です。EMQX の内部データベースに移行します。
* **プラグイン（mosquitto-auth-plug）**：SQL や LDAP を使用している場合は、対応する EMQX 認証バックエンドを直接設定します。

## フェーズ 2：Mosquitto のベースラインを反映した EMQX 設定

### MQTT リスナーの再作成

Mosquitto はリスナーを順次定義しますが、EMQX はタイプ別（TCP、SSL、WebSocket）にグループ化して `emqx.conf` に記述します。

**Mosquitto (`mosquitto.conf`)：**

```properties
# デフォルトリスナー
port 1883
max_connections -1

# SSL リスナー
listener 8883
certfile /etc/mosquitto/certs/server.crt
keyfile /etc/mosquitto/certs/server.key
```

**EMQX (`emqx.conf`)：**
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

### MQTT 設定オプションのマッピング

クライアントの挙動を一貫させるため、主要なプロトコル設定を変換します。

| Mosquitto ディレクティブ | EMQX HOCON パラメーター | 説明 |
| :--- | :--- | :--- |
| `max_queued_messages` | `mqtt.max_mqueue_len` | クライアントごとの最大オフラインメッセージ数。 |
| `persistent_client_expiration` | `mqtt.session_expiry_interval` | 切断後のセッション状態保持時間。 |
| `message_size_limit` | `mqtt.max_packet_size` | MQTT パケットの最大許容サイズ。 |
| `log_dest file` | `log.file.enable = true` | ファイルログを有効化。 |

**補足：** Mosquitto はセッションの有効期限をグローバルに管理しますが、EMQX（MQTT 5.0）はクライアント単位のセッション有効期限をサポートします。MQTT 3.1.1 のレガシークライアント向けには、EMQX でグローバルデフォルトを設定可能です。

### 認証の移行

EMQX は複数の認証バックエンドをサポートしています。多くの Mosquitto 移行では、既存の認証情報を保持し、ユーザーのパスワードリセットを不要にすることが目標です。

#### オプション 1：ユーザーの再作成（バッチインポート）

元の平文パスワードが利用可能な場合、EMQX HTTP API を使って一括インポートできます。

**バッチインポート用 CSV フォーマット：**
`users.csv` ファイルを以下のように作成します。

```csv
user_id,password,is_superuser
device001,secret123,false
admin,adminPass,true
```

**インポートコマンド：**
`curl` でファイルをアップロードします。`type=plain` パラメーターにより EMQX がパスワードをハッシュ化します。

```bash
curl -v -u admin:public -X POST \
  -H "Content-Type: multipart/form-data" \
  -F "filename=@users.csv" \
  "http://localhost:18083/api/v5/authentication/password_based:built_in_database/import_users?type=plain"
```

* `admin:public` はダッシュボードの認証情報に置き換えてください。
* 認証方式（`password_based:built_in_database`）が設定と一致していることを確認してください。

#### オプション 2：Mosquitto パスワードファイルのインポート（上級者向け）

大量のユーザーがいて、ハッシュ化済みの `mosquitto.passwd` ファイルのみを持つ場合、Erlang スクリプトで EMQX の組み込みデータベースに直接インポート可能です。

**ステップ 1：認証設定**

インポート前に、EMQX の [パスワードベース認証](../access-control/authn/pwoverview.md) を [組み込みデータベース](../access-control/authn/mnesia.md) バックエンドで設定します。Mosquitto のデフォルトハッシュ方式に合わせるため、以下の設定を使用してください。

* **アルゴリズム**：`pbkdf2`
* **Mac 関数**：`sha512`
* **反復回数**：101
* **DK 長さ**：32

> **注意：** これらのパラメーター（101回の反復、sha512）は Mosquitto のデフォルトと完全に一致します。EMQX の標準デフォルト（より強力なセキュリティ優先）とは異なりますが、インポートした認証情報の検証には必須です。

**ステップ 2：パスワードファイルのコピー**

`mosquitto.passwd` ファイルを EMQX サーバー（例：`/tmp/mosquitto.passwd`）にコピーし、`emqx` ユーザーが読み取り可能な権限を付与します。

**ステップ 3：インポートスクリプトの実行**

EMQX ノード上で以下のコマンドを実行します。このスクリプトはファイルを読み込み、Base64 エンコードされたソルトとハッシュをデコードし、ユーザー情報を直接データベースに書き込みます。

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

既存のユーザー管理システムと統合するエンタープライズ環境では、MySQL や PostgreSQL などの外部 SQL データベースにユーザーを移行することも可能です。EMQX は動的 SQL クエリをサポートし、多様なスキーマ形式に柔軟に対応します。

#### オプション 3：相互 TLS（mTLS）

Mosquitto で X.509 クライアント証明書（相互 TLS）を認証に使用している場合、EMQX のリスナー設定でピア証明書の検証を構成します。

**Mosquitto 設定：**
```properties
require_certificate true
use_identity_as_username true
cafile /etc/mosquitto/ca.crt
```

**EMQX 設定：**
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

* Mosquitto で使用していた CA 証明書（`ca.crt`）を EMQX にもコピーしてください。
* `use_identity_as_username` が有効な場合、`verify_peer` が有効な EMQX ではデフォルトで Common Name（CN）がユーザー名として使用されます。

### 認可（ACL）の移行

認証が完了したら、Mosquitto のトピックレベルのアクセス制御を EMQX のポリシーに合わせて移行します。

Mosquitto の ACL 構文は EMQX の `acl.conf` と非常に似ています。

**Mosquitto (`acl_file`)：**
```properties
user Alice
topic read sensors/#
pattern write devices/%u/data
```

**EMQX (`acl.conf`)：**
```erlang
{allow, {user, "Alice"}, subscribe, ["sensors/#"]}.
{allow, all, publish, ["devices/${username}/data"]}.
```

* `%u` は `${username}`（または `${clientid}`）に置き換えます。
* `read` は `subscribe`、`write` は `publish` にマッピングします。

### データ統合の設定（ブリッジ＆スクリプトの置換）

Mosquitto はメッセージ転送にブリッジを、データ処理に外部スクリプト（Python/Node.js）を使用します。EMQX ではこれらを組み込みの [ルールエンジン](../data-integration/rules.md) と [データ統合](../data-integration/data-bridges.md) で置き換えます。

> EMQX のルールエンジンは、メッセージの選択、フィルタリング、変換を行い、コネクター経由で外部システムに転送できます。

**シナリオ：別ブローカーへのデータ転送**  
`mosquitto.conf` の `connection bridge_name` の代わりに：

1. EMQX ダッシュボードで **MQTT ブローカーコネクター** を作成。
2. **ルール** を作成し（例：`SELECT * FROM "#"`)、コネクターにメッセージを転送。

**シナリオ：Python 処理スクリプトの置換**  
`sensors/+/temp` をサブスクライブし、30度以上の値をフィルタリングしてデータベースに書き込むスクリプトがある場合：

1. スクリプトを廃止。
2. **ルールを作成：**
    ```sql
    SELECT payload.temp as temperature, topic, timestamp
    FROM "sensors/+/temp"
    WHERE temperature > 30
    ```
3. **アクションを追加：** InfluxDB や HTTP などのデータ統合を設定し、結果を直接書き込み。

## フェーズ 3：デバイスおよび統合の更新

### クライアント接続の更新

EMQX は標準 MQTT ポート（1883/8883）を使用するため、DNS 経由で接続している多くのデバイスは設定変更不要です。DNS レコードを更新し、`mqtt.yourdomain.com` を EMQX クラスターのロードバランサーまたは IP に向けてください。

### 接続確認

EMQX ダッシュボードでデバイスの接続状況を監視します。

* **接続数**を確認。

  > 以下のコマンドでも接続を確認可能です：
  >
  > ```bash
  > emqx_ctl clients list
  > ```
  >
  > またはダッシュボードの **監視** -> **クライアント** で確認できます。

* 認証エラーがないか **ログ** をチェック（ハッシュアルゴリズムの不一致や証明書の欠如が原因となることが多いです）。

## 高度な移行シナリオ

このセクションは任意で、ダウンタイムゼロの移行が必要な場合に適用します。

### ブリッジ移行戦略（ダウンタイムゼロ）

サービス停止なしで移行するには：

1. Mosquitto と並行して EMQX をデプロイ。
2. Mosquitto を EMQX にブリッジ接続し、すべてのメッセージを転送。
    ```properties
    # mosquitto.conf
    connection migrate_uplink
    address emqx-server:1883
    topic # out 0
    topic # in 0
    ```
3. バックエンドアプリケーションを EMQX に切り替え。EMQX と Mosquitto 両方のデバイスからデータを受信。
4. デバイスを段階的に新しい EMQX エンドポイントに移行。
5. Mosquitto の接続がなくなったらブリッジを解除し、Mosquitto を停止。

## 検証チェックリスト

本番トラフィック切り替え前に以下を確認してください：

- **リスナー**：TCP（1883）および SSL（8883）ポートが開放され接続を受け入れている。
- **認証**：既存の認証情報でユーザーがログイン可能。
- **ACL**：ユーザーが指定されたトピックに制限されている。
- **データフロー**：デバイスからパブリッシュされたメッセージがサブスクライバーやバックエンドアプリに届いている。
- **パーシステンス**：ブローカー再起動後も保持メッセージが利用可能（`retain_available = true` を確認）。

## まとめ

Mosquitto から EMQX への移行は、スケーラビリティと信頼性の大幅な向上をもたらしつつ、プロトコル互換性を維持します。既存設定のマッピングと EMQX のルールエンジンを活用して外部スクリプトやブリッジを置き換えることで、アーキテクチャを簡素化し、将来的な大規模成長に備えたインフラを構築できます。
