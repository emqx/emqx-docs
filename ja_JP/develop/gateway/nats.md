# NATS プロトコルゲートウェイ

EMQX 5.10.0 以降、EMQX は [NATS プロトコル](https://docs.nats.io/reference/reference-protocols/nats-protocol) に基づく NATS プロトコルゲートウェイを導入しました。これにより、EMQX は NATS クライアントからの接続を受け入れ、MQTT とのメッセージ相互運用を実現します。本ドキュメントでは、その機能概要と NATS ゲートウェイの有効化および設定方法について説明します。

## 機能概要

NATS プロトコルゲートウェイは現在、以下の主要機能をサポートしています。

### プロトコルサポート

- **NATS プロトコルのメッセージタイプを完全サポート**：
  - 接続およびセッション管理：`INFO`、`CONNECT`
  - メッセージのパブリッシュ／サブスクライブ：`PUB`、`HPUB`、`SUB`、`UNSUB`
  - メッセージ配信および応答：`MSG`、`HMSG`
  - ハートビートおよびステータス：`PING`、`PONG`、`+OK`、`-ERR`
- **冗長モード（Verbose mode）対応**：クライアントが `CONNECT verbose=true` で接続した場合に応答確認を有効化。

### MQTT との相互運用性

- **MQTT との双方向メッセージ相互運用**：
  - NATS クライアントからパブリッシュされたメッセージを MQTT パブリッシュに変換。
  - MQTT メッセージを対応するトピックにサブスクライブしている NATS クライアントへ転送。
- **NATS ワイルドカードサブスクリプション対応**：MQTT 互換のトピック形式に自動変換。
- **キューグループ共有サブスクリプション対応**：NATS のキューグループサブスクリプションを MQTT の共有サブスクリプション形式に変換。
- **リクエスト／リプライモード対応**：
  - NATS クライアントからのリクエストを MQTT リクエストに変換。
  - 対象トピックに MQTT サブスクライバーが存在しない場合は、EMQX が迅速にエラー応答を返却。

### ネットワークおよび接続性

- **複数のトランスポートプロトコルをサポート**：TCP、TLS、WebSocket（WS）、および TLS 上の WebSocket（WSS）。

## NATS と MQTT 間のクロスプロトコルメッセージング

NATS プロトコルはパブリッシュ／サブスクライブモデルに完全対応しており、NATS ゲートウェイを通じて MQTT メッセージと相互運用します。変換ルールは以下の通りです。

- **PUB および HPUB メッセージはパブリッシュ操作として扱う**：
  - トピックは PUB メッセージの `subject` フィールドから派生。例：`t.a` は MQTT トピック `t/a` に変換。
  - メッセージペイロードは PUB メッセージ本文から直接取得。
  - クライアントが `CONNECT verbose=1` で接続した場合、変換後の MQTT メッセージは QoS 1 を使用。そうでなければ QoS 0。
- **SUB メッセージはサブスクリプション要求として扱う**：
  - トピックは SUB メッセージの `subject` フィールドから派生。例：`t.a` は MQTT トピック `t/a` に変換。
  - QoS は同様に `verbose=1` で QoS 1、そうでなければ QoS 0。
  - ワイルドカードをサポート。例：`*.b.>` は `+/b/#` に変換。
  - キューグループもサポート。SUB メッセージのキューグループ値は MQTT 共有サブスクリプションのグループ名に変換。
- **UNSUB メッセージはサブスクリプション解除要求として扱い**、サブスクリプション ID（sid）で解除対象を特定。

::: tip

NATS ゲートウェイはパブリッシュ／サブスクライブ操作に対する独自のアクセス制御を実装していません。トピック権限は統一された[認可設定](../../guides/access-control/authz/authz.md)で管理してください。

:::

## NATS ゲートウェイの有効化

EMQX 5.10.0 以降、NATS ゲートウェイは以下の3つの方法で有効化できます。

- ダッシュボードから
- REST API を使用して
- `base.hocon` 設定ファイルを編集して

::: tip

クラスター モードでは、ダッシュボードまたは REST API で行った設定は全ノードに自動的に適用されます。特定ノードのみに設定を反映させたい場合は、そのノードの `base.hocon` 設定ファイルを使用してください。

:::

### ダッシュボードから有効化

EMQX ダッシュボードから NATS ゲートウェイを素早く有効化する手順：

1. 左メニューの **管理** -> **ゲートウェイ** に移動。
2. **ゲートウェイ** ページで **NATS** を探し、**操作** 列の **セットアップ** ボタンをクリックして **NATS 初期化** ウィザードを起動。
3. ウィザードの指示に従う：
   - **基本設定** ステップではデフォルト値を受け入れ、**次へ** をクリック。
   - **リスナー** ステップではリスナーを設定するかスキップして **次へ** をクリック。
     （リスナーの詳細設定は [リスナーの追加](#add-a-listener) を参照）
   - **有効化** をクリックして NATS ゲートウェイを起動。

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

以下の設定例は `base.hocon` で NATS ゲートウェイを有効化する方法です。

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

NATS ゲートウェイは TCP、SSL、WS、WSS タイプのリスナーをサポートしています。設定可能なパラメータの完全な一覧は、[EMQX Enterprise 設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) のゲートウェイ設定 - リスナーセクションを参照してください。

## NATS ゲートウェイのカスタマイズ

デフォルト設定に加え、EMQX は特定のビジネスニーズに合わせて様々な設定オプションを提供しています。このセクションでは、**ゲートウェイ** ページで利用可能な設定項目の詳細を説明します。

### 基本設定

1. **ゲートウェイ** ページで **NATS** を探し、**操作** 列の **設定** ボタンをクリック。

2. **設定** タブでゲートウェイの接続パラメータ、マウントポイントプレフィックス、クライアント識別情報のオーバーライドを設定可能。

   - **サーバー名**：ゲートウェイの内部参照用の一意識別子。デフォルトは `emq_nats_gateway`。

   - **マウントポイント**：ゲートウェイを通過するすべてのトピックに自動的に付加される文字列プレフィックス。プロトコル間のトピック分離に役立ちます。例：`nats/` を使うことでクライアントがプレフィックスを手動で付ける必要なくクロスプロトコルルーティングが可能。

   - **デフォルトハートビート間隔**：サーバーがクライアントの生存確認のために送信する `PING` パケットの間隔（秒）。デフォルトは `60` 秒。

   - **ハートビートタイムアウト閾値**：クライアントが応答しない場合に切断とみなす時間。

   - **最大ペイロードサイズ**：メッセージデータの最大許容サイズ。`PUB` ではペイロードに適用。`HPUB` ではヘッダー部およびヘッダー＋ペイロードの合計サイズがこの値を超えてはならない。EMQX は設定値を `INFO.max_payload` として広告。デフォルトは `1048576` バイト。

   - **最大コントロールライン長** <Badge type="tip" text="6.0.5+" />：CRLF 区切りを除く NATS プロトコルのコントロールラインの最大長。デフォルトは `4096` バイト。最小設定値は `1024` バイト。

   - **アイドルタイムアウト**：非アクティブなクライアント接続を切断するまでの秒数。デフォルトは `30` 秒。

   - **統計情報の有効化**：このゲートウェイの統計収集および報告を有効にするか。デフォルトは有効。

   - **クライアント情報オーバーライド**：`CONNECT` パケットから認証情報を抽出する方法を定義。

     ::: tip

     認証が有効な場合は、`username` と `password` の正しいフィールドマッピングを設定し、適切な資格情報処理を行ってください。

     :::

     - **ユーザー名**：`CONNECT` パケットの `user` フィールドにマッピング。
     - **パスワード**：`CONNECT` パケットの `pass` フィールドにマッピング。
     - **クライアント ID**：`${generated}` を指定すると自動生成されます。カスタムロジックでの設定も可能。

   コントロールラインが `max_control_line` を超える、または宣言されたメッセージサイズが `max_payload_size` を超える場合、EMQX はフレームを拒否して接続を切断します。メッセージ内容をバッファリングする前にサイズを検査します。

3. **更新** をクリックして変更を適用。

### リスナーの追加

**リスナー** タブで既存リスナーの編集、削除、新規追加が可能です。

1. **リスナー** タブで **+ リスナー追加** をクリック。

2. **リスナー追加** ダイアログで以下の項目を設定：

   **基本設定**

   - **名前**：リスナーを識別する一意の名前。
   - **タイプ**：リスナーの種類を選択。NATS でサポートされるのは `tcp`、`ssl`、`ws`、`wss`。
   - **バインド**：リスナーが接続を受け付けるポート番号。

   **リスナー設定**

   - **最大接続数**：同時接続の最大数。デフォルトは `1024000`。
   - **最大接続レート（リスナー）**：1秒あたりに受け入れる新規接続の最大数。デフォルトは `1000`。
   - **プロキシプロトコル**：Proxy Protocol v1/v2 の有効化。デフォルトは `false`。
   - **プロキシプロトコルタイムアウト**：Proxy Protocol ヘッダー受信のタイムアウト。指定時間内にヘッダーが受信されない場合は接続を切断。デフォルトは `3` 秒。

   **ピア検証設定**（SSL および WSS リスナーのみ適用）

   相互 TLS はデフォルトで有効。TLS 証明書、秘密鍵、CA 証明書を設定してください。ファイルアップロードまたは直接フォームに貼り付け可能です。詳細は [SSL/TLS 接続の有効化](../../guides/network/emqx-mqtt-tls.md) を参照。

   - **TLS 証明書**：TLS 証明書ファイルのパスまたは内容。
   - **TLS 秘密鍵**：TLS 秘密鍵ファイルのパスまたは内容。
   - **CA 証明書**：CA 証明書ファイルのパスまたは内容。
   - **ピア証明書検証の強制**：クライアント証明書検証を必須にするか。デフォルトは `true`。

3. **追加** をクリックしてリスナー作成を完了。

### 認証の設定

NATS プロトコルはユーザー名／パスワード認証やトークン認証など複数の認証方式をサポートします。NATS ゲートウェイは以下の認証バックエンドをサポートしています。

- [組み込みデータベース認証](../../guides/access-control/authn/mnesia.md)
- [MySQL 認証](../../guides/access-control/authn/mysql.md)
- [MongoDB 認証](../../guides/access-control/authn/mongodb.md)
- [PostgreSQL 認証](../../guides/access-control/authn/postgresql.md)
- [Redis 認証](../../guides/access-control/authn/redis.md)
- [HTTP サーバー認証](../../guides/access-control/authn/http.md)
- [JWT 認証](../../guides/access-control/authn/jwt.md)
- [LDAP 認証](../../guides/access-control/authn/ldap.md)

MQTT プロトコルとは異なり、ゲートウェイは認証器のリスト（チェーン）ではなく単一の認証器のみをサポートします。認証器が有効でない場合、すべての NATS クライアントは認証なしで接続可能です。

NATS ゲートウェイは `CONNECT` パケットから認証情報を抽出します。

- **クライアント ID**：デフォルトで自動生成。
- **ユーザー名**：`user` フィールドの値。
- **パスワード**：`pass` フィールドの値。

#### ダッシュボードで設定

以下は HTTP サーバーを使ったパスワード認証設定の例です。

1. NATS ゲートウェイ設定の **認証** タブに移動。
2. **+ 認証作成** をクリックし、メカニズムに **パスワードベース** を選択、データソースに **HTTP サーバー** を選択して **次へ**。
3. 設定パラメータを入力。詳細は [HTTP パスワード認証](../../guides/access-control/authn/http.md) を参照。
4. **作成** をクリックし、設定内容を確認後 **更新** をクリック。

#### REST API で設定

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

#### 設定ファイルで設定

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

その他の認証タイプについては、[EMQX 認証器](../../guides/access-control/authn/authn.md#emqx-authenticators)のドキュメントを参照してください。

### ユーザーレベルインターフェースの設定

- 完全な設定リファレンスは以下を参照してください： [NATS ゲートウェイ設定](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)
- REST API の詳細は以下を参照してください： [ゲートウェイ REST API ドキュメント](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs)

## さらに詳しく

NATS プロトコルゲートウェイとそのユースケースについて詳しくは、ブログ記事 [EMQX NATS Gateway: Enabling MQTT-NATS Bidirectional Interoperability](https://www.emqx.com/en/blog/emqx-nats-gateway) をご覧ください。
