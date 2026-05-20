# Mosquitto から EMQX への移行

本ガイドでは、既存の Eclipse Mosquitto デプロイメントを EMQX に移行する手順を説明します。軽量で単一インスタンスのブローカーから、スケーラブルで分散型の MQTT プラットフォームへ移行したい管理者向けに設計されています。移行では EMQX の標準 MQTT プロトコル互換性を活用し、設定、認証情報、統合ロジックの移行方法を明確に示します。

## 移行の概要

移行プロセスは主に以下の3フェーズで構成されます：

1. **Mosquitto 資産のインベントリ**：設定ファイル（`mosquitto.conf`）、セキュリティ関連ファイル（パスワードファイル、ACL、証明書）を収集し、現在のデータフローを把握します。
2. **EMQX の設定**：Mosquitto の設定を EMQX の HOCON 形式の設定ファイル（`emqx.conf`）に変換し、ユーザー認証情報をインポート、アクセス制御やデータ統合をルールエンジンで再現します。
3. **デバイスおよび統合の更新**：デバイスを EMQX クラスターにリダイレクト（ポート互換性により多くの場合シームレス）し、システム動作を検証します。

| パラメーター／アーティファクト | Mosquitto（例） | EMQX（例） | 備考 |
| :--- | :--- | :--- | :--- |
| **メイン設定ファイル** | `/etc/mosquitto/mosquitto.conf` | `/etc/emqx/emqx.conf` | EMQX は階層的な HOCON 形式を使用。 |
| **ネットワークポート** | `1883`（TCP）、`8883`（SSL） | `1883`（TCP）、`8883`（SSL） | 標準ポートは同じため、通常デバイスの再設定不要。 |
| **ユーザー認証情報** | `/etc/mosquitto/passwd` | 組み込みデータベース（Mnesia） | 既存のパスワードハッシュを API 経由でインポート可能。 |
| **アクセス制御** | `/etc/mosquitto/acl_file` | `/etc/emqx/acl.conf` | Allow/Deny ルールは直接マッピング可能。 |
| **ブリッジ** | `connection bridge_name` | データコネクター＆ルール | 静的ブリッジを動的データルーティングに置換。 |
| **パーシステンス** | `mosquitto.db` | `data/`（Mnesia + RocksDB） | EMQX はセッションの永続化を自動管理。 |

## フェーズ 1：Mosquitto 資産のインベントリ

### 設定ファイルと証明書の収集

主要な設定ファイルの場所を特定します。通常 `mosquitto.conf` 内で定義されています：

* **メイン設定:** `include_dir` またはデフォルトの `/etc/mosquitto/mosquitto.conf`。
* **証明書:** `certfile`、`keyfile`、`cafile` のパスを確認。
* **セキュリティ:** `password_file` と `acl_file` の場所を特定。

証明書ファイル（`server.crt`、`server.key`、`ca.crt`）は EMQX ノードの通常 `/etc/emqx/certs/` にコピーしてください。

### 認証方式の分析

認証方式を確認します：
*   **パスワードファイル:** 最も一般的。EMQX の内部データベースに移行します。
*   **プラグイン（mosquitto-auth-plug）:** SQL や LDAP を使用している場合は、対応する EMQX の認証バックエンドを直接設定します。

## フェーズ 2：Mosquitto の基本設定を EMQX に再現

### MQTT リスナーの再作成

Mosquitto はリスナーを順次定義しますが、EMQX はタイプ別（TCP、SSL、WebSocket）にグループ化して `emqx.conf` に記述します。

**Mosquitto (`mosquitto.conf`):**

```properties
# デフォルトリスナー
port 1883
max_connections -1

# SSL リスナー
listener 8883
certfile /etc/mosquitto/certs/server.crt
keyfile /etc/mosquitto/certs/server.key
```

**EMQX (`emqx.conf`):**
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

クライアントの動作を一貫させるため、主要なプロトコル設定を変換します。

| Mosquitto ディレクティブ | EMQX HOCON パラメーター | 説明 |
| :--- | :--- | :--- |
| `max_queued_messages` | `mqtt.max_mqueue_len` | クライアントごとの最大オフラインメッセージバッファ数。 |
| `persistent_client_expiration` | `mqtt.session_expiry_interval` | 切断後にセッション状態を保持する時間。 |
| `message_size_limit` | `mqtt.max_packet_size` | 許容される最大 MQTT パケットサイズ。 |
| `log_dest file` | `log.file.enable = true` | ファイルログを有効化。 |

**補足:** Mosquitto はセッション有効期限をグローバルに管理しますが、EMQX（MQTT 5.0）はクライアント毎に設定可能です。MQTT 3.1.1 のレガシークライアント向けには、EMQX でグローバルデフォルトを設定して Mosquitto のポリシーに合わせられます。

### 認証の移行

EMQX は複数の認証バックエンドをサポートしています。ほとんどの Mosquitto 移行では、既存の認証情報を保持しつつユーザーのパスワードリセットを不要にすることが目標です。

#### オプション 1：ユーザーの再作成（バッチインポート）

元の平文パスワードにアクセス可能な場合、EMQX HTTP API を使ってバッチインポートできます。

**バッチインポート用 CSV フォーマット:**
`users.csv` ファイルを以下のように作成します：
```csv
user_id,password,is_superuser
device001,secret123,false
admin,adminPass,true
```

**インポートコマンド:**
`curl` でファイルをアップロードします。`type=plain` パラメーターにより EMQX がパスワードをハッシュ化します。

```bash
curl -v -u admin:public -X POST \
  -H "Content-Type: multipart/form-data" \
  -F "filename=@users.csv" \
  "http://localhost:18083/api/v5/authentication/password_based:built_in_database/import_users?type=plain"
```
* `admin:public` はダッシュボードの認証情報に置き換えてください。
* 認証方式（`password_based:built_in_database`）が設定と一致していることを確認してください。

#### オプション 2：Mosquitto パスワードファイルのインポート（高度）

大量のユーザーがいて、`mosquitto.passwd` ファイル（ハッシュ済みパスワードのみ）しかない場合、Erlang スクリプトで直接 EMQX の組み込みデータベースにインポート可能です。

**ステップ 1：認証設定**

データインポート前に、EMQX で [パスワードベース認証](../access-control/authn/pwoverview.md) を [組み込みデータベース](../access-control/authn/mnesia.md) バックエンドで設定します。Mosquitto のデフォルトハッシュ方式に合わせるため、以下の設定を使用してください。

*   **アルゴリズム:** `pbkdf2`
*   **Mac 関数:** `sha512`
*   **反復回数:** 101
*   **DK 長さ:** 32

> **注意:** これらのパラメーター（101回の反復、sha512）は Mosquitto のデフォルトと完全に一致します。EMQX の標準デフォルト（より強力なセキュリティ優先）とは異なりますが、インポートした認証情報の検証には必須です。

**ステップ 2：パスワードファイルのコピー**

`mosquitto.passwd` ファイルを EMQX サーバー（例：`/tmp/mosquitto.passwd`）にコピーし、`emqx` ユーザーが読み取り可能にしてください。

**ステップ 3：インポートスクリプトの実行**

EMQX ノード上で以下のコマンドを実行します。このスクリプトはファイルを読み込み、Base64 エンコードされたソルトとハッシュをデコードし、ユーザー情報をデータベースに直接書き込みます。

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

既存のユーザー管理システムと統合するエンタープライズ環境では、ユーザーを外部 SQL データベース（MySQL、PostgreSQL）に移行することも可能です。EMQX は動的 SQL クエリをサポートし、多様なスキーマ形式に柔軟に対応します。

#### オプション 3：相互 TLS（mTLS）

Mosquitto で X.509 クライアント証明書（相互 TLS）認証を使用している場合は、EMQX のリスナー設定でピア証明書の検証を構成します。

**Mosquitto 設定:**
```properties
require_certificate true
use_identity_as_username true
cafile /etc/mosquitto/ca.crt
```

**EMQX 設定:**
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

* Mosquitto で使用していた同じ CA 証明書（`ca.crt`）を EMQX にコピーしてください。
* `use_identity_as_username` が有効な場合、EMQX は `verify_peer` が有効時にデフォルトで Common Name（CN）をユーザー名として使用します。

### 認可（ACL）の移行

認証が完了したら、トピックレベルのアクセス制御を元の Mosquitto ポリシーに合わせて移行します。

Mosquitto の ACL 構文は EMQX の `acl.conf` と非常に似ています。

**Mosquitto (`acl_file`):**
```properties
user Alice
topic read sensors/#
pattern write devices/%u/data
```

**EMQX (`acl.conf`):**
```erlang
{allow, {user, "Alice"}, subscribe, ["sensors/#"]}.
{allow, all, publish, ["devices/${username}/data"]}.
```

* `%u` は `${username}`（または `${clientid}`）に置換してください。
* `read` は `subscribe`、`write` は `publish` にマッピングします。

### データ統合の設定（ブリッジ＆スクリプトの置換）

Mosquitto はメッセージ転送にブリッジを、データ処理に外部スクリプト（Python/Node.js）を使用します。EMQX ではこれらを組み込みの [ルールエンジン](../data-integration/rules.md) と [データ統合](../data-integration/data-bridges.md) で置き換えます。

> EMQX のルールエンジンは、メッセージの選択、フィルタリング、変換を行い、コネクター経由で外部システムへ転送できます。

**例：別のブローカーへのデータ転送**  
`mosquitto.conf` の `connection bridge_name` の代わりに：

1. EMQX ダッシュボードで **MQTT ブローカーコネクター** を作成。
2. メッセージを選択する **ルール**（例：`SELECT * FROM "#"`) を作成し、コネクターへ転送。

**例：Python 処理スクリプトの置換**  
`sensors/+/temp` をサブスクライブし、30度以上の値をフィルタしてデータベースに書き込むスクリプトがある場合：

1. スクリプトを廃止。
2. 以下のようなルールを作成：
    ```sql
    SELECT payload.temp as temperature, topic, timestamp
    FROM "sensors/+/temp"
    WHERE temperature > 30
    ```
3. アクションを追加し、InfluxDB や HTTP などのデータ統合で直接書き込み。

## フェーズ 3：デバイスと統合の更新

### クライアント接続の更新

EMQX は標準 MQTT ポート（1883/8883）を使用するため、DNS 経由で接続している多くのデバイスは設定変更不要です。DNS レコードを更新し、`mqtt.yourdomain.com` を EMQX クラスターのロードバランサーまたは IP に向けてください。

### 接続の検証

EMQX ダッシュボードでデバイスの接続状況を監視します。
* **接続数**を確認。

  > 接続を確認するには以下を実行できます：
  >
  > ```bash
  > emqx_ctl clients list
  > ```
  >
  > またはダッシュボードの **モニタリング** -> **クライアント** を参照。

* 認証エラーがないか **ログ** をチェック（ハッシュアルゴリズムの不一致や証明書不足が原因の場合があります）。

## 高度な移行シナリオ

このセクションは任意で、ダウンタイムゼロの移行が必要な場合に適用します。

### ブリッジ移行戦略（ゼロダウンタイム）

サービス停止なしで移行するには：

1. Mosquitto と並行して EMQX をデプロイ。
2. Mosquitto から EMQX へのブリッジを設定し、すべてのメッセージを転送。
    ```properties
    # mosquitto.conf
    connection migrate_uplink
    address emqx-server:1883
    topic # out 0
    topic # in 0
    ```
3. バックエンドアプリケーションを EMQX にポイントし、両方のブローカーからデータを受信。
4. デバイスを段階的に新しい EMQX エンドポイントに切り替え。
5. Mosquitto の接続がなくなったらブリッジを削除し、Mosquitto を停止。

## 検証チェックリスト

本番トラフィック切替前に以下を確認してください：

- **リスナー:** TCP（1883）と SSL（8883）ポートが開いて接続を受け付けている。
- **認証:** 既存の認証情報でユーザーがログイン可能。
- **ACL:** ユーザーが指定されたトピックに制限されている。
- **データフロー:** デバイスがパブリッシュしたメッセージがサブスクライバーやバックエンドアプリに届いている。
- **パーシステンス:** ブローカー再起動後も保持メッセージが利用可能（`retain_available = true` を確認）。

## 結論

Mosquitto から EMQX への移行は、プロトコル互換性を維持しつつ、スケーラビリティと信頼性を大幅に向上させます。既存設定のマッピングと EMQX のルールエンジンによる外部スクリプト・ブリッジの置換により、アーキテクチャを簡素化し、大規模成長に対応可能なインフラを構築できます。
