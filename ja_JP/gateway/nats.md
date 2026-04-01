# NATS プロトコルゲートウェイ

EMQX 5.10.0 以降、EMQX は [NATS プロトコル](https://docs.nats.io/reference/reference-protocols/nats-protocol) に基づく NATS プロトコルゲートウェイを導入しました。これにより、EMQX は NATS クライアントからの接続を受け入れ、MQTT とのメッセージ相互運用を実現します。本ドキュメントでは、その機能概要と NATS ゲートウェイの有効化および設定方法について説明します。

## 機能概要

NATS プロトコルゲートウェイは現在、以下の主要な機能をサポートしています。

### プロトコルサポート

- **NATS プロトコルのメッセージタイプを完全サポート**：
  - 接続およびセッション管理：`INFO`、`CONNECT`
  - メッセージのパブリッシュ／サブスクライブ：`PUB`、`HPUB`、`SUB`、`UNSUB`
  - メッセージ配信および応答：`MSG`、`HMSG`
  - ハートビートおよびステータス：`PING`、`PONG`、`+OK`、`-ERR`
- **冗長モード（Verbose mode）対応**：クライアントが `CONNECT verbose=true` で接続した場合に応答確認を有効化。
- **豊富な認証サポート**：Token、NKey、JWT、およびユーザー名/パスワード認証をサポート。

### MQTT との相互運用性

- **MQTT との双方向メッセージ相互運用**：
  - NATS クライアントからパブリッシュされたメッセージは MQTT のパブリッシュに変換されます。
  - MQTT メッセージは対応するトピックをサブスクライブしている NATS クライアントに転送されます。
- **NATS のワイルドカードサブスクリプションをサポート**し、MQTT 互換のトピック形式に自動変換。
- **Queue Group の共有サブスクリプションをサポート**：NATS Queue Group サブスクリプションは MQTT の共有サブスクリプション形式に変換されます。
- **リクエスト／レスポンスモードをサポート**：
  - NATS クライアントからのリクエストは MQTT リクエストに変換されます。
  - 対象トピックに MQTT サブスクライバーが存在しない場合、EMQX は速やかにエラー応答を返します。

### ネットワークおよび接続性

- **複数のトランスポートプロトコルをサポート**：TCP、TLS、WebSocket（WS）、および TLS 上の WebSocket（WSS）。

## NATS と MQTT 間のクロスプロトコルメッセージング

NATS プロトコルはパブリッシュ／サブスクライブメッセージングモデルに完全対応しており、NATS ゲートウェイを介して MQTT メッセージと相互運用します。変換ルールは以下の通りです。

- **PUB および HPUB メッセージはパブリッシュ操作として扱われます**：
  - トピックは PUB メッセージの `subject` フィールドから派生します。例：`t.a` は MQTT トピック `t/a` に変換されます。
  - メッセージのペイロードは PUB メッセージの本文から直接取得されます。
  - クライアントが `CONNECT verbose=1` で接続した場合、変換後の MQTT メッセージは QoS 1 を使用し、それ以外は QoS 0 となります。
- **SUB メッセージはサブスクリプション要求として扱われます**：
  - トピックは SUB メッセージの `subject` フィールドから派生します。例：`t.a` は MQTT トピック `t/a` に変換されます。
  - QoS は同様のルールに従い、`verbose=1` であれば QoS 1、それ以外は QoS 0。
  - ワイルドカードをサポートします。例：`*.b.>` は `+/b/#` に変換されます。
  - Queue Group をサポートします。SUB メッセージの Queue Group 値は MQTT 共有サブスクリプションのグループ名に変換されます。
- **UNSUB メッセージはサブスクリプション解除要求として扱われ、サブスクリプション ID（sid）で解除対象を特定します**。

::: tip

NATS ゲートウェイはパブリッシュ／サブスクライブ操作に対する独自のアクセス制御を実装していません。トピックの権限管理は統一された[認可設定](../access-control/authz/authz.md)で行う必要があります。

:::

## NATS ゲートウェイの有効化

EMQX 5.10.0 以降、NATS ゲートウェイは以下の3つの方法で有効化できます。

- ダッシュボードから
- REST API を使用して
- `base.hocon` 設定ファイルを編集して

::: tip

クラスター モードでは、ダッシュボードまたは REST API で行った設定はすべてのノードに自動的に適用されます。特定のノードのみに設定を適用したい場合は、そのノードの `base.hocon` 設定ファイルを使用してください。

:::

### ダッシュボードから有効化

EMQX ダッシュボードから NATS ゲートウェイを素早く有効化する手順：

1. 左メニューの **管理** -> **ゲートウェイ** に移動します。
2. **ゲートウェイ** ページで **NATS** を探し、**操作** 列の **セットアップ** ボタンをクリックして **NATS 初期化** ウィザードを開きます。
3. ウィザードの手順に従います：
   - **基本設定** ステップではデフォルト値を受け入れ、**次へ** をクリック。
   - **リスナー** ステップではリスナーを設定するかスキップして **次へ** をクリック。
     （詳細なリスナー設定は [リスナーの追加](#add-a-listener) を参照してください。）
   - **有効化** をクリックして NATS ゲートウェイを起動します。

有効化が完了すると、**ゲートウェイ** ページにリダイレクトされ、NATS ゲートウェイの状態が **有効** と表示されます。

### REST API で有効化

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

### 設定ファイルで有効化

`base.hocon` を編集して NATS ゲートウェイを有効化する例は以下の通りです。

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

NATS ゲートウェイは TCP、SSL、WS、WSS タイプのリスナーをサポートします。設定可能なパラメータの完全な一覧は、[EMQX Enterprise 設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) のゲートウェイ設定 - リスナーセクションを参照してください。

## NATS ゲートウェイのカスタマイズ

デフォルト設定に加え、EMQX は特定のビジネスニーズに合わせてさまざまな設定オプションを提供しています。本節では **ゲートウェイ** ページで利用可能な設定項目を詳細に説明します。

### 基本設定

1. **ゲートウェイ** ページで **NATS** を探し、**操作** 列の **設定** ボタンをクリックします。

2. **設定** タブで、ゲートウェイの接続パラメータ、マウントポイントプレフィックス、クライアント識別情報の上書きを設定できます。

   - **サーバー名**：ゲートウェイの内部参照用の一意識別子。デフォルトは `emq_nats_gateway`。

   - **マウントポイント**：ゲートウェイを通過するすべてのトピックに自動的に付加される文字列プレフィックス。プロトコル間のトピック分離に役立ちます。例として `nats/` を使用すると、クライアントが手動でプレフィックスを付けることなくクロスプロトコルルーティングが可能になります。

   - **デフォルトハートビート間隔**：サーバーがクライアントの生存確認のために `PING` パケットを送信する間隔（秒）。デフォルトは `60` 秒。

   - **ハートビートタイムアウト閾値**：クライアントが応答しない場合に切断とみなす時間。

   - **最大ペイロードサイズ**：単一の `PUB` または `HPUB` メッセージのペイロード最大サイズ（バイト）。デフォルトは `1048576` バイト。

   - **アイドルタイムアウト**：非アクティブなクライアント接続を切断するまでの秒数。デフォルトは `30` 秒。

   - **統計情報の有効化**：このゲートウェイの統計収集およびレポートを有効にするかどうか。デフォルトは有効。

   - **クライアント情報の上書き**：`CONNECT` パケットから認証情報を抽出する方法を定義。

     ::: tip

     認証が有効な場合は、`username` と `password` の正しいフィールドをマッピングして、適切に資格情報を処理できるようにしてください。

     :::

     - **ユーザー名**：`CONNECT` パケットの `user` フィールドにマッピング。
     - **パスワード**：`CONNECT` パケットの `pass` フィールドにマッピング。
     - **クライアント ID**：`${generated}` を指定すると自動生成されます。特定のロジックでカスタマイズも可能。

3. **更新** をクリックして変更を適用します。

### リスナーの追加

**リスナー** タブでリスナーの編集、削除、新規追加が可能です。

1. **リスナー** タブで **+ リスナー追加** をクリックします。

2. **リスナー追加** ダイアログで以下のオプションを設定します。

   **基本設定**

   - **名前**：リスナーを識別する一意の名前。
   - **タイプ**：リスナーの種類を選択。NATS では `tcp`、`ssl`、`ws`、`wss` がサポートされています。
   - **バインド**：リスナーが接続を受け付けるポート番号。

   **リスナー設定**

   - **最大接続数**：同時接続の最大数。デフォルトは `1024000`。
   - **最大接続レート（リスナー）**：1秒あたりに受け入れる新規接続の最大数。デフォルトは `1000`。
   - **プロキシプロトコル**：Proxy Protocol v1/v2 の有効化。デフォルトは `false`。
   - **プロキシプロトコルタイムアウト**：Proxy Protocol ヘッダー受信のタイムアウト。指定時間内にヘッダーが受信されない場合、接続を切断。デフォルトは `3` 秒。

   **ピア検証設定**（SSL および WSS リスナーのみ適用）

   相互 TLS はデフォルトで有効です。TLS 証明書、秘密鍵、CA 証明書を設定する必要があります。これらはアップロードまたは直接フォームに貼り付け可能です。詳細は [SSL/TLS 接続の有効化](../network/emqx-mqtt-tls.md) を参照してください。

   - **TLS 証明書**：TLS 証明書のファイルパスまたは内容。
   - **TLS キー**：TLS 秘密鍵のファイルパスまたは内容。
   - **CA 証明書**：CA 証明書のファイルパスまたは内容。
   - **ピア証明書の強制検証**：クライアント証明書検証を必須にするか。デフォルトは `true`。

3. **追加** をクリックしてリスナー作成を完了します。

### 認証の設定

NATS ゲートウェイは、以下2つの方式で認証をサポートします。

- **ゲートウェイ認証（`authentication`）**：EMQX ゲートウェイの汎用認証機構で、主にユーザー名／パスワード系バックエンドで利用します。
- **ゲートウェイ内部認証（`internal_authn`）**：NATS ネイティブの、ユーザー名/パスワード以外の認証です。

両方を有効化した場合、EMQX は次の順序で認証します。

1. `internal_authn` を先頭から順に評価します。
2. 方式に必要な資格情報が不足している場合は次の方式を試します。
3. 資格情報が存在し検証に失敗した場合は、その時点で接続を拒否します。
4. すべての内部認証方式がスキップされ、`authentication` が設定されている場合はゲートウェイ認証にフォールバックします。
5. 内部認証・ゲートウェイ認証とも未設定の場合は、すべての NATS クライアント接続を許可します。

#### ゲートウェイ認証の設定

他のゲートウェイと同様に、NATS ゲートウェイも EMQX 標準認証機構と統合できます。対応バックエンドは以下の通りです。

- [組み込みデータベース認証](../access-control/authn/mnesia.md)
- [MySQL 認証](../access-control/authn/mysql.md)
- [MongoDB 認証](../access-control/authn/mongodb.md)
- [PostgreSQL 認証](../access-control/authn/postgresql.md)
- [Redis 認証](../access-control/authn/redis.md)
- [HTTP サーバー認証](../access-control/authn/http.md)
- [JWT 認証](../access-control/authn/jwt.md)
- [LDAP 認証](../access-control/authn/ldap.md)

ゲートウェイ認証では、NATS ゲートウェイは `CONNECT` パケットから以下の項目を取り出して認証に使用します。

- **クライアント ID**：デフォルトで自動生成されます。
- **ユーザー名**：`user` フィールドの値。
- **パスワード**：`pass` フィールドの値。

MQTT とは異なり、ゲートウェイ認証は単一の認証機構のみサポートし、認証チェーンはサポートしません。

##### ダッシュボードでの設定

以下は HTTP サーバーを利用したパスワード認証の設定例です。

1. NATS ゲートウェイ設定の **認証** タブに移動します。
2. **+ 認証作成** をクリックし、メカニズムに **パスワードベース** を選択、データソースに **HTTP サーバー** を選択して **次へ** をクリック。
3. 設定パラメータを入力します。各オプションの詳細は [HTTP パスワード認証](../access-control/authn/http.md) を参照してください。
4. **作成** をクリックし、設定内容を確認後、**更新** をクリックして確定します。

##### REST API での設定

以下は組み込みデータベース認証を REST API で設定する例です。

```
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

##### 設定ファイルでの設定

以下は設定ファイルで組み込みデータベース認証を設定する例です。

```
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

#### ゲートウェイ内部認証（`internal_authn`）の設定

NATS ゲートウェイ固有の認証機能であり、NATS Server の標準 3 認証方式をサポートします。

##### Token 認証

- NATS `CONNECT` の `auth_token` を使用します。
- プレーンテキストと bcrypt ハッシュ（`$2a$`, `$2b$`, `$2y$`）をサポートします。
- NATS 参考: [Token authentication](https://docs.nats.io/running-a-nats-service/configuration/securing_nats/auth_intro/tokens)

ダッシュボード設定例:

![nats-auth-token](assets/nats-auth-token.png)

設定ファイル例:

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

- NATS `CONNECT` の `nkey` + `sig` challenge/response を使用します。
- `nkeys` は有効な NATS ユーザー公開鍵（`U...`）である必要があります。
- NATS 参考: [NKey authentication](https://docs.nats.io/running-a-nats-service/configuration/securing_nats/auth_intro/nkey_auth)

ダッシュボード設定例:

![nats-auth-nkey](assets/nats-auth-nkey.png)

設定ファイル例:

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

##### JWT 認証（ACL 対応）

- `jwt` + `sig`（必要に応じて `nkey`）で認証します。
- 「信頼済みオペレーター公開鍵リスト」と「JWT プリロードリスト」の両方が必須です。
- `resolver` の種類は現在 `memory` のみをサポートし、設定で Account JWT を事前定義します。
- NATS 参考: [JWT authentication](https://docs.nats.io/running-a-nats-service/configuration/securing_nats/auth_intro/jwt)

ダッシュボード設定例:

![nats-auth-jwt](assets/nats-auth-jwt.png)

設定ファイル例:

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

JWT のユーザークレームには ACL ルールを含めることもできます。EMQX は `permissions` と `nats.pub` / `nats.sub` クレームをサポートし、最終的な認可結果は JWT ACL と EMQX 認可ルールの積集合になります。

JWT ACL クレーム例:

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

### ユーザーレベルインターフェースの設定

- 完全な設定リファレンスは以下を参照してください： [NATS ゲートウェイ設定](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)
- REST API の詳細は以下を参照してください： [ゲートウェイ REST API ドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)
