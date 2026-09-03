# NATS プロトコルゲートウェイ

EMQX 5.10.0 以降、EMQX は [NATS プロトコル](https://docs.nats.io/reference/reference-protocols/nats-protocol) に基づく NATS プロトコルゲートウェイを導入しました。これにより、EMQX は NATS クライアントからの接続を受け入れ、MQTT とメッセージの相互運用を実現できます。本ドキュメントでは、その機能を説明し、NATS ゲートウェイの有効化および設定方法を案内します。

## 機能概要

NATS プロトコルゲートウェイは現在、以下の主要な機能をサポートしています。

### プロトコルサポート

- **NATS プロトコルのメッセージタイプを完全サポート**：
  - 接続およびセッション管理：`INFO`、`CONNECT`
  - メッセージのパブリッシュ／サブスクライブ：`PUB`、`HPUB`、`SUB`、`UNSUB`
  - メッセージ配信および応答：`MSG`、`HMSG`
  - ハートビートおよびステータス：`PING`、`PONG`、`+OK`、`-ERR`
- **冗長モード（Verbose mode）対応**：クライアントが `CONNECT verbose=true` で接続した際に応答確認を有効化。
- **豊富な認証サポート**：Token、NKey、JWT、ユーザー名／パスワード認証に対応。

### MQTT との相互運用性

- **MQTT との双方向メッセージ相互運用**：
  - NATS クライアントからパブリッシュされたメッセージは MQTT のパブリッシュに変換。
  - MQTT メッセージは対応するトピックをサブスクライブしている NATS クライアントに転送。
- **NATS のワイルドカードサブスクリプションをサポート**し、自動的に MQTT 互換のトピック形式に変換。
- **キューグループ共有サブスクリプションをサポート**：NATS のキューグループサブスクリプションは MQTT の共有サブスクリプション形式に変換。
- **リクエスト／リプライモードをサポート**：
  - NATS クライアントからのリクエストは MQTT リクエストに変換。
  - 対象トピックに MQTT サブスクライバーがいない場合、EMQX は迅速にエラー応答を返す。

### ネットワークおよび接続性

- **複数のトランスポートプロトコルをサポート**：TCP、TLS、WebSocket（WS）、および TLS 上の WebSocket（WSS）。

## NATS と MQTT 間のクロスプロトコルメッセージング

NATS プロトコルはパブリッシュ／サブスクライブモデルに完全対応しており、NATS ゲートウェイを介して MQTT メッセージと相互運用します。変換ルールは以下の通りです。

- **PUB および HPUB メッセージはパブリッシュ操作として扱う**：
  - トピックは PUB メッセージの `subject` フィールドから派生。例：`t.a` は MQTT トピック `t/a` に変換。
  - メッセージペイロードは PUB メッセージ本文から直接取得。
  - クライアントが `CONNECT verbose=1` で接続した場合、変換後の MQTT メッセージは QoS 1、それ以外は QoS 0。
- **SUB メッセージはサブスクリプション要求として扱う**：
  - トピックは SUB メッセージの `subject` フィールドから派生。例：`t.a` は MQTT トピック `t/a` に変換。
  - QoS は同様のルールで、`verbose=1` なら QoS 1、それ以外は QoS 0。
  - ワイルドカードをサポート。例：`*.b.>` は `+/b/#` に変換。
  - キューグループをサポート。SUB メッセージのキューグループ値は MQTT 共有サブスクリプションのグループ名に変換。
- **UNSUB メッセージはサブスクリプション解除要求として扱い**、サブスクリプション ID（sid）を用いて解除対象を特定。

::: tip

NATS ゲートウェイはパブリッシュ／サブスクライブ操作に対する独自のアクセス制御を実装していません。トピック権限は統一された[認可設定](../access-control/authz/authz.md)で管理してください。

:::

## NATS ゲートウェイの有効化

EMQX 5.10.0 以降、NATS ゲートウェイは以下の3つの方法で有効化できます。

- ダッシュボード経由
- REST API 経由
- `base.hocon` 設定ファイルの編集

::: tip

クラスター環境では、ダッシュボードや REST API で行った設定は全ノードに自動適用されます。特定ノードのみに設定を反映したい場合は、そのノードの `base.hocon` 設定ファイルを編集してください。

:::

### ダッシュボードでの有効化

EMQX ダッシュボードから NATS ゲートウェイを素早く有効化する手順：

1. 左メニューの **管理** -> **ゲートウェイ** に移動。
2. **ゲートウェイ** ページで **NATS** を探し、**操作** 列の **セットアップ** ボタンをクリックして **NATS 初期化** ウィザードを起動。
3. ウィザードの手順に従う：
   - **基本設定** でデフォルト値を受け入れ、**次へ** をクリック。
   - **リスナー** でリスナーを設定するかスキップして **次へ** をクリック。
     （詳細なリスナー設定は [リスナーの追加](#add-a-listener) を参照）
   - **有効化** をクリックして NATS ゲートウェイを起動。

有効化が完了すると、**ゲートウェイ** ページにリダイレクトされ、NATS ゲートウェイのステータスが **有効** と表示されます。

### REST API での有効化

以下の例は REST API を使って NATS ゲートウェイを有効化する方法です。

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateway/nats' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "nats",
  "enable": true,
  "mountpoint": "nats/",
  "listeners": [
    {
      "type": "tcp",
      "name": "default",
      "bind": "4222",
      "max_conn_rate": 1000,
      "max_connections": 1024000
    }
  ]
}'
```

### 設定ファイルでの有効化

以下の設定例は `base.hocon` を編集して NATS ゲートウェイを有効化する方法です。

```properties
gateway.nats {

  mountpoint = "nats/"

  listeners.tcp.default {
    bind = 4222
    acceptors = 16
    max_connections = 1024000
    max_conn_rate = 1000
  }
}
```

NATS ゲートウェイは TCP、SSL、WS、WSS タイプリスナーをサポートします。設定可能なパラメータの完全な一覧は、[EMQX Enterprise 設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) のゲートウェイ設定 - リスナーセクションを参照してください。

## NATS ゲートウェイのカスタマイズ

デフォルト設定に加え、EMQX は特定のビジネス要件に合わせて柔軟に設定できる各種オプションを提供しています。本セクションでは、**ゲートウェイ** ページで利用可能な設定項目を詳細に説明します。

### 基本設定

1. **ゲートウェイ** ページで **NATS** を探し、**操作** 列の **設定** ボタンをクリック。

2. **設定** タブで、ゲートウェイの接続パラメータ、マウントポイントプレフィックス、クライアント識別情報の上書きを設定可能。

   - **サーバー名**：ゲートウェイの内部参照用の一意識別子。デフォルトは `emq_nats_gateway`。
   - **マウントポイント**：ゲートウェイを通過するすべてのトピックに自動付加される文字列プレフィックス。プロトコル間のトピック分離に役立ちます。例：`nats/` を指定すると、クライアントが手動でプレフィックスを含めなくてもクロスプロトコルルーティングが可能。
   - **デフォルトハートビート間隔**：サーバーがクライアントの生存確認のために `PING` パケットを送信する間隔（秒）。デフォルトは `60` 秒。
   - **ハートビートタイムアウト閾値**：クライアントが応答しなかった場合に切断とみなす時間。
   - **最大ペイロードサイズ**：単一の `PUB` または `HPUB` メッセージペイロードの最大サイズ（バイト）。デフォルトは `1048576` バイト。
   - **アイドルタイムアウト**：非アクティブなクライアント接続を切断するまでの秒数。デフォルトは `30` 秒。
   - **統計情報の有効化**：このゲートウェイの統計収集および報告を有効にするかどうか。デフォルトは有効。
   - **クライアント情報の上書き**：`CONNECT` パケットから認証情報を抽出する方法を定義。

     ::: tip

     認証を有効にする場合は、`username` と `password` の正しいフィールドマッピングを設定し、認証情報が正しく処理されるようにしてください。

     :::

     - **ユーザー名**：`CONNECT` パケットの `user` フィールドにマッピング。
     - **パスワード**：`CONNECT` パケットの `pass` フィールドにマッピング。
     - **クライアント ID**：`${generated}` で自動生成、または特定のロジックでカスタマイズ可能。

3. **更新** をクリックして変更を適用。

### リスナーの追加

**リスナー** タブでリスナーの編集、削除、新規追加が可能です。

1. **リスナー** タブで **+ リスナー追加** をクリック。

2. **リスナー追加** ダイアログで以下のオプションを設定：

   **基本設定**

   - **名前**：リスナーを識別する一意の名前。
   - **タイプ**：リスナーの種類を選択。NATS では `tcp`、`ssl`、`ws`、`wss` がサポートされます。
   - **バインド**：リスナーが接続を受け付けるポート番号。

   **リスナー設定**

   - **最大接続数**：同時接続の最大数。デフォルトは `1024000`。
   - **最大接続レート（リスナー）**：1秒あたりに受け入れる新規接続の最大数。デフォルトは `1000`。
   - **プロキシプロトコル**：Proxy Protocol v1/v2 の有効化。デフォルトは `false`。
   - **プロキシプロトコルタイムアウト**：Proxy Protocol ヘッダー受信のタイムアウト。指定時間内にヘッダーが受信されない場合、接続を切断。デフォルトは `3` 秒。

   **ピア検証設定**（SSL および WSS リスナーのみ適用）

   相互 TLS はデフォルトで有効です。TLS 証明書、秘密鍵、CA 証明書を設定してください。これらはアップロードまたは直接フォームに貼り付け可能です。詳細は [SSL/TLS 接続の有効化](../network/emqx-mqtt-tls.md) を参照。

   - **TLS 証明書**：TLS 証明書ファイルパスまたは内容。
   - **TLS 秘密鍵**：TLS 秘密鍵ファイルパスまたは内容。
   - **CA 証明書**：CA 証明書ファイルパスまたは内容。
   - **ピア証明書検証の強制**：クライアント証明書検証を必須にするか。デフォルトは `true`。

3. **追加** をクリックしてリスナー作成を完了。

#### 転送クライアントアドレスの設定

EMQX 6.3.0 以降、`proxy_address_header` と `proxy_port_header` はデフォルトで空文字列 `""` となり、NATS WebSocket リスナーは明示的に転送ヘッダー名を設定しない限り TCP ピアアドレスとポートを使用します。

NATS リスナーが信頼できるプロキシの背後にあり、転送ヘッダーを書き換える場合は、`base.hocon` にヘッダー名を設定してください。例：

```hocon
gateway.nats.listeners.ws.default.websocket {
  proxy_address_header = "x-forwarded-for"
  proxy_port_header = "x-forwarded-port"
}
```

WSS リスナーの場合は `gateway.nats.listeners.wss.<listener-name>.websocket` を使用します。EMQX は設定された各ヘッダーの最初（左端）のエントリを使用します。ヘッダーが存在しないか無効な場合は、対応する TCP ピアアドレスまたはポートを使用します。

これらの設定は、信頼できるプロキシがクライアントからの値を書き換える場合のみ行ってください。そうでないと、クライアントが偽装した送信元アドレスを EMQX に使わせる可能性があります。

EMQX 6.3.0 ではゲートウェイ WebSocket リスナーのヘッダー名マッチングの不具合も修正されました。6.3.0 より前は設定した名前がリクエストヘッダーと一致せず、TCP ピアアドレスとポートが使用されていました。

### 認証の設定

NATS ゲートウェイは以下の2つの認証方式をサポートします。

- **ゲートウェイ認証（`authentication`）**：EMQX ゲートウェイ認証機構で、通常はユーザー名／パスワード形式のバックエンドに使用。
- **内部ゲートウェイ認証（`internal_authn`）**：NATS ネイティブのユーザー名／パスワード以外の認証。

両方が有効な場合、EMQX は以下の順序で認証を評価します。

1. `internal_authn` のメソッドを上から順に評価。
2. 必要な認証情報が不足していれば次のメソッドへ。
3. 認証情報があるが検証に失敗した場合は即座に接続拒否。
4. すべての内部メソッドがスキップされ、`authentication` が設定されていればゲートウェイ認証にフォールバック。
5. 内部メソッドもゲートウェイ認証も設定されていなければ、すべての NATS クライアントの接続を許可。

#### ゲートウェイ認証の設定

他のゲートウェイ同様、NATS ゲートウェイは標準の EMQX 認証機構と連携可能です。以下の認証バックエンドをサポートします。

- [組み込みデータベース認証](../access-control/authn/mnesia.md)
- [MySQL 認証](../access-control/authn/mysql.md)
- [MongoDB 認証](../access-control/authn/mongodb.md)
- [PostgreSQL 認証](../access-control/authn/postgresql.md)
- [Redis 認証](../access-control/authn/redis.md)
- [HTTP サーバー認証](../access-control/authn/http.md)
- [JWT 認証](../access-control/authn/jwt.md)
- [LDAP 認証](../access-control/authn/ldap.md)

ゲートウェイ認証では、NATS の `CONNECT` パケットから以下のフィールドを抽出します。

- **クライアント ID**：デフォルトで自動生成。
- **ユーザー名**：`user` フィールドの値。
- **パスワード**：`pass` フィールドの値。

MQTT プロトコルとは異なり、ゲートウェイ認証は単一の認証機構のみをサポートし、複数の認証機構のリスト（チェーン）はサポートしません。

##### ダッシュボードでの設定例

HTTP サーバーを用いたパスワード認証の設定例：

1. NATS ゲートウェイ設定の **認証** タブへ移動。
2. **+ 認証作成** をクリックし、メカニズムに **パスワードベース**、データソースに **HTTP サーバー** を選択。**次へ** をクリック。
3. 設定パラメータを入力。詳細は [HTTP パスワード認証](../access-control/authn/http.md) を参照。
4. **作成** をクリックし、設定内容を確認後 **更新** をクリック。

##### REST API での設定例

組み込みデータベース認証を REST API で設定する例：

```bash
curl -X 'POST' \
  'http://127.0.0.1:18083/api/v5/gateway/nats/authentication' \
  -u <your-application-key>:<your-security-key> \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{
  "backend": "built_in_database",
  "mechanism": "password_based",
  "password_hash_algorithm": {
    "name": "sha256",
    "salt_position": "suffix"
  },
  "user_id_type": "username"
}'
```

##### 設定ファイルでの設定例

組み込みデータベース認証を設定ファイルで指定する例：

```properties
gateway.nats {

  authentication {
    backend = built_in_database
    mechanism = password_based
    password_hash_algorithm {
      name = sha256
      salt_position = suffix
    }
    user_id_type = username
  }
}
```

その他の認証タイプについては、[EMQX 認証機構](../access-control/authn/authn.md#emqx-authenticators) のドキュメントを参照してください。

#### 内部認証（`internal_authn`）の設定

NATS ゲートウェイ固有の認証機能で、NATS サーバー標準の3つの認証方式をサポートします。

##### トークン認証

- NATS `CONNECT` パケットの `auth_token` フィールドを使用。
- プレイントークンおよび bcrypt ハッシュ（`$2a$`、`$2b$`、`$2y$`）をサポート。
- NATS 参照：[Token authentication](https://docs.nats.io/running-a-nats-service/configuration/securing_nats/auth_intro/tokens)

ダッシュボード設定例：

![nats-auth-token](assets/nats-auth-token.png)

設定ファイル例：

```properties
gateway.nats {
  internal_authn = [
    {
      type = token
      token = "nats_token"
    }
  ]
}
```

##### NKey 認証

- NATS `CONNECT` パケットの `nkey` + `sig` チャレンジ／レスポンスを使用。
- `nkeys` は有効な NATS ユーザーパブリックキー（`U...`）である必要あり。
- NATS 参照：[NKey authentication](https://docs.nats.io/running-a-nats-service/configuration/securing_nats/auth_intro/nkey_auth)

ダッシュボード設定例：

![nats-auth-nkey](assets/nats-auth-nkey.png)

設定ファイル例：

```properties
gateway.nats {
  internal_authn = [
    {
      type = nkey
      nkeys = [
        "Uxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
      ]
    }
  ]
}
```

##### JWT 認証（ACL サポート付き）

- NATS `CONNECT` パケットの `jwt` + `sig`（およびオプションの `nkey`）を使用。
- 信頼されたオペレーターリストと JWT プリロードリストの両方が必要。
- リゾルバータイプは現在 `memory` のみ対応。これは有効なアカウント JWT を設定で事前ロードする方式。
- NATS 参照：[JWT authentication](https://docs.nats.io/running-a-nats-service/configuration/securing_nats/auth_intro/jwt)

ダッシュボード設定例：

![nats-auth-jwt](assets/nats-auth-jwt.png)

設定ファイル例：

```properties
gateway.nats {
  internal_authn = [
    {
      type = jwt
      trusted_operators = [
        "Oxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
      ]
      resolver {
        type = memory
        resolver_preload = [
          {
            pubkey = "Axxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
            jwt = "<your-account-jwt>"
          }
        ]
      }
    }
  ]
}
```

JWT ユーザークレームは ACL ルールを含めることも可能です。EMQX は `permissions` および `nats.pub` / `nats.sub` クレームをサポートし、最終的な認可結果は JWT ACL と EMQX 認可ルールの共通部分（インターセクション）となります。

JWT ACL クレーム例：

```json
{
  "sub": "Uxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  "iss": "Axxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  "nats": {
    "pub": {
      "allow": ["sensors.>"],
      "deny": ["sensors.secret.>"]
    },
    "sub": {
      "allow": ["alerts.>"],
      "deny": ["alerts.internal.>"]
    }
  }
}
```

### ユーザーレベルのインターフェース設定

- 完全な設定リファレンスは：[NATS ゲートウェイ設定](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)
- REST API の詳細は：[ゲートウェイ REST API ドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)

## さらに詳しく

NATS プロトコルゲートウェイとそのユースケースについては、以下のブログ記事をご覧ください。  
[EMQX NATS Gateway: MQTT-NATS 双方向相互運用の実現](https://www.emqx.com/en/blog/emqx-nats-gateway)
