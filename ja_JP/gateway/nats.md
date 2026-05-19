# NATS プロトコルゲートウェイ

EMQX 5.10.0 以降、EMQX は [NATS プロトコル](https://docs.nats.io/reference/reference-protocols/nats-protocol) に基づく NATS プロトコルゲートウェイを導入しました。これにより、EMQX は NATS クライアントからの接続を受け入れ、MQTT とのメッセージ相互運用を実現できます。本ドキュメントでは、その機能概要と NATS ゲートウェイの有効化および設定方法について説明します。

## 機能概要

NATS プロトコルゲートウェイは現在、以下の主要な機能をサポートしています。

### プロトコルサポート

- **NATS プロトコルのメッセージタイプを完全サポート**：
  - 接続およびセッション管理：`INFO`、`CONNECT`
  - メッセージのパブリッシュ／サブスクライブ：`PUB`、`HPUB`、`SUB`、`UNSUB`
  - メッセージ配信および応答：`MSG`、`HMSG`
  - ハートビートおよびステータス：`PING`、`PONG`、`+OK`、`-ERR`
- **冗長モード（Verbose mode）対応**：クライアントが `CONNECT verbose=true` で接続した場合に応答アックを有効化。

### MQTT との相互運用性

- **MQTT との双方向メッセージ相互運用**：
  - NATS クライアントからパブリッシュされたメッセージを MQTT パブリッシュに変換。
  - MQTT メッセージを対応するトピックをサブスクライブしている NATS クライアントに転送。
- **NATS のワイルドカードサブスクリプションをサポート**し、自動的に MQTT 互換のトピック形式に変換。
- **キューグループ共有サブスクリプションをサポート**：NATS のキューグループサブスクリプションを MQTT の共有サブスクリプション形式に変換。
- **リクエスト／リプライモードをサポート**：
  - NATS クライアントからのリクエストを MQTT リクエストに変換。
  - 対象トピックに MQTT サブスクライバーが存在しない場合、EMQX は迅速にエラー応答を返す。

### ネットワークおよび接続性

- **複数のトランスポートプロトコルをサポート**：TCP、TLS、WebSocket（WS）、および TLS 上の WebSocket（WSS）。

## NATS と MQTT 間のクロスプロトコルメッセージング

NATS プロトコルはパブリッシュ／サブスクライブメッセージングモデルと完全に互換性があり、NATS ゲートウェイを介して MQTT メッセージングと相互運用します。変換ルールは以下の通りです。

- **PUB および HPUB メッセージはパブリッシュ操作として扱う**：
  - トピックは PUB メッセージの `subject` フィールドから派生。例：`t.a` は MQTT トピック `t/a` に変換。
  - メッセージペイロードは PUB メッセージ本文から直接取得。
  - クライアントが `CONNECT verbose=1` で接続した場合、変換後の MQTT メッセージは QoS 1 を使用。そうでなければ QoS 0。
- **SUB メッセージはサブスクリプション要求として扱う**：
  - トピックは SUB メッセージの `subject` フィールドから派生。例：`t.a` は MQTT トピック `t/a` に変換。
  - QoS は同様のルールに従い、`verbose=1` なら QoS 1、そうでなければ QoS 0。
  - ワイルドカードをサポート。例：`*.b.>` は `+/b/#` に変換。
  - キューグループをサポート。SUB メッセージのキューグループ値は MQTT 共有サブスクリプションのグループ名に変換。
- **UNSUB メッセージはサブスクリプション解除要求として扱い**、サブスクリプション ID（sid）で解除対象を特定。

::: tip

NATS ゲートウェイはパブリッシュ／サブスクライブ操作に対する独自のアクセス制御を実装していません。トピック権限は統一された [認可設定](../access-control/authz/authz.md) で管理してください。

:::

## NATS ゲートウェイの有効化

EMQX 5.10.0 以降、NATS ゲートウェイは以下の3つの方法で有効化できます。

- ダッシュボード経由
- REST API 利用
- `base.hocon` 設定ファイル編集

::: tip

クラスター運用時、ダッシュボードまたは REST API での設定は全ノードに自動適用されます。特定ノードのみ設定を適用したい場合は、そのノードの `base.hocon` 設定ファイルを使用してください。

:::

### ダッシュボードでの有効化

EMQX ダッシュボードから NATS ゲートウェイを素早く有効化する手順：

1. 左メニューの **管理** -> **ゲートウェイ** に移動。
2. **ゲートウェイ** ページで **NATS** を見つけ、**操作** 列の **セットアップ** ボタンをクリックし、**NATS 初期化** ウィザードを開く。
3. ウィザードの指示に従う：
   - **基本設定** ステップでデフォルト値を受け入れ、**次へ** をクリック。
   - **リスナー** ステップでリスナーを設定するかスキップして **次へ** をクリック。
     （リスナーの詳細設定は [リスナーの追加](#add-a-listener) を参照）
   - **有効化** をクリックして NATS ゲートウェイを起動。

有効化完了後、**ゲートウェイ** ページにリダイレクトされ、NATS ゲートウェイのステータスが **有効** と表示されます。

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

以下の例は `base.hocon` で NATS ゲートウェイを有効化する設定例です。

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

NATS ゲートウェイは TCP、SSL、WS、WSS タイプリスナーをサポートしています。設定可能なパラメータの完全な一覧は、[EMQX Enterprise 設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) のゲートウェイ設定 - リスナーセクションを参照してください。

## NATS ゲートウェイのカスタマイズ

デフォルト設定に加え、EMQX はさまざまな設定オプションを提供し、特定のビジネス要件に合わせて調整可能です。本セクションでは **ゲートウェイ** ページで利用可能な設定項目を詳述します。

### 基本設定

1. **ゲートウェイ** ページで **NATS** を見つけ、**操作** 列の **設定** ボタンをクリック。

2. **設定** タブで、ゲートウェイの接続パラメータ、マウントポイントプレフィックス、クライアント識別情報の上書きを設定可能。

   - **サーバー名**：ゲートウェイの内部参照用の一意識別子。デフォルトは `emq_nats_gateway`。
   - **マウントポイント**：ゲートウェイを通過するすべてのトピックに自動付加される文字列プレフィックス。プロトコル間のトピック分離に役立ちます。例：`nats/` を指定すると、クライアントが手動でプレフィックスを含めなくてもクロスプロトコルルーティングが可能。
   - **デフォルトハートビート間隔**：サーバーがクライアントの生存確認のために送信する `PING` パケットの間隔（秒）。デフォルトは `60` 秒。
   - **ハートビートタイムアウト閾値**：クライアントがこの時間内に応答しなければ切断とみなす。
   - **最大ペイロードサイズ**：単一の `PUB` または `HPUB` メッセージペイロードの最大サイズ（バイト）。デフォルトは `1048576` バイト。
   - **アイドルタイムアウト**：非アクティブなクライアント接続を切断するまでの秒数。デフォルトは `30` 秒。
   - **統計有効化**：このゲートウェイの統計収集および報告を有効にするかどうか。デフォルトは有効。
   - **クライアント情報の上書き**：`CONNECT` パケットから認証情報を抽出する方法を定義。

     ::: tip

     認証を有効にしている場合は、`username` と `password` の正しいフィールドマッピングを設定し、資格情報が適切に処理されるようにしてください。

     :::

     - **ユーザー名**：`CONNECT` パケットの `user` フィールドにマッピング。
     - **パスワード**：`CONNECT` パケットの `pass` フィールドにマッピング。
     - **クライアント ID**：`${generated}` に設定すると自動生成されます。特定のロジックでカスタマイズも可能。

3. **更新** をクリックして変更を適用。

### リスナーの追加

**リスナー** タブで既存リスナーの編集、削除、新規追加が可能です。

1. **リスナー** タブで **+ リスナー追加** をクリック。

2. **リスナー追加** ダイアログで以下を設定：

   **基本設定**

   - **名前**：リスナーを識別する一意の名前。
   - **タイプ**：リスナーの種類を選択。NATS では `tcp`、`ssl`、`ws`、`wss` がサポートされています。
   - **バインド**：リスナーが接続を受け付けるポート番号。

   **リスナー設定**

   - **最大接続数**：同時接続の最大数。デフォルトは `1024000`。
   - **最大接続レート（リスナー）**：1秒あたりに受け入れる新規接続の最大数。デフォルトは `1000`。
   - **プロキシプロトコル**：Proxy Protocol v1/v2 の有効化。デフォルトは `false`。
   - **プロキシプロトコルタイムアウト**：Proxy Protocol ヘッダー受信のタイムアウト。指定時間内にヘッダーが受信できなければ接続を切断。デフォルトは `3` 秒。

   **ピア検証設定**（SSL および WSS リスナーのみ適用）

   相互 TLS はデフォルトで有効です。TLS 証明書、秘密鍵、CA 証明書を設定してください。これらはアップロードまたは直接フォームに貼り付け可能です。詳細は [SSL/TLS 接続の有効化](../network/emqx-mqtt-tls.md) を参照。

   - **TLS 証明書**：TLS 証明書ファイルパスまたは内容。
   - **TLS 秘密鍵**：TLS 秘密鍵ファイルパスまたは内容。
   - **CA 証明書**：CA 証明書ファイルパスまたは内容。
   - **ピア証明書検証の強制**：クライアント証明書検証を必須にするか。デフォルトは `true`。

3. **追加** をクリックしてリスナー作成を完了。

### 認証の設定

NATS プロトコルはユーザー名／パスワード認証やトークン認証など複数の認証方式をサポートします。NATS ゲートウェイは以下の認証バックエンドをサポートしています。

- [組み込みデータベース認証](../access-control/authn/mnesia.md)
- [MySQL 認証](../access-control/authn/mysql.md)
- [MongoDB 認証](../access-control/authn/mongodb.md)
- [PostgreSQL 認証](../access-control/authn/postgresql.md)
- [Redis 認証](../access-control/authn/redis.md)
- [HTTP サーバー認証](../access-control/authn/http.md)
- [JWT 認証](../access-control/authn/jwt.md)
- [LDAP 認証](../access-control/authn/ldap.md)

MQTT プロトコルとは異なり、ゲートウェイは単一の認証機構のみをサポートし、複数の認証機構のリスト（チェーン）はサポートしません。認証機構が有効でない場合、すべての NATS クライアントは認証なしで接続可能です。

NATS ゲートウェイは `CONNECT` パケットから認証情報を抽出します：

- **クライアント ID**：デフォルトで自動生成。
- **ユーザー名**：`user` フィールドの値。
- **パスワード**：`pass` フィールドの値。

#### ダッシュボードでの設定

以下は HTTP サーバーを利用したパスワード認証の設定例です。

1. NATS ゲートウェイ設定の **認証** タブに移動。
2. **+ 認証作成** をクリックし、認証方式に **パスワードベース** を選択、データソースに **HTTP サーバー** を選択して **次へ**。
3. 設定パラメータを入力。各オプションの詳細は [HTTP パスワード認証](../access-control/authn/http.md) を参照。
4. **作成** をクリックし、内容を確認後 **更新** をクリックして確定。

#### REST API での設定

以下は組み込みデータベース認証を REST API で設定する例です。

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

#### 設定ファイルでの設定

以下は設定ファイルで組み込みデータベース認証を設定する例です。

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

### ユーザーレベルインターフェースの設定

- 完全な設定リファレンスは：[NATS ゲートウェイ設定](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)
- REST API の詳細は：[ゲートウェイ REST API ドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)

## さらに詳しく

NATS プロトコルゲートウェイとそのユースケースについて詳しくは、ブログ記事 [EMQX NATS Gateway: Enabling MQTT-NATS Bidirectional Interoperability](https://www.emqx.com/en/blog/emqx-nats-gateway) をご覧ください。
