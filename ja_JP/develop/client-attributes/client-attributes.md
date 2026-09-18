# MQTT クライアント属性

EMQX のクライアント属性は、開発者が異なるアプリケーションシナリオの要件に基づいて MQTT クライアントに追加の属性を定義・設定できる仕組みを提供します。これらの属性は、EMQX 内での認証、認可、データ統合、MQTT 拡張機能の強化に不可欠であり、柔軟な開発を促進します。クライアントのメタデータを活用することで、MQTT クライアント識別の柔軟なテンプレート化もサポートし、パーソナライズされたクライアント設定や認証プロセスの効率化に寄与し、開発の適応性と効率性を高めます。

## ワークフロー

クライアント属性の設定、保存、利用の流れは以下の通りです。

**1. クライアント属性の設定**

クライアントが EMQX に正常に接続すると、EMQX は接続および認証イベントをトリガーし、この過程であらかじめ定義された設定に基づいて[クライアント属性を設定](#クライアント属性の設定)します。

**2. クライアント属性の保存と破棄**

設定された属性はクライアントセッションの `client_attrs` フィールドにキー・バリュー形式で保存されます。クライアントセッションが終了すると、これらの属性は削除されます。

永続セッションの場合、クライアントが引き継ぐ際にセッション内のクライアント属性は置き換えられ上書きされます。それ以外にクライアント属性を変更または削除する方法はありません。

**3. クライアント属性の利用**

EMQX の他の機能では、関連設定項目内で `${client_attrs.NAME}` プレースホルダーを使用し、動的に属性値を抽出して設定やデータの一部として利用できます。

## クライアント属性の設定

クライアントが EMQX に正常に接続すると、接続および認証イベントがトリガーされ、あらかじめ定義された設定に基づいてクライアント属性が設定されます。現在、以下の2つの方法がサポートされています。

- クライアントメタデータからの抽出
- クライアント認証プロセス中の設定

### クライアントメタデータからの抽出

事前設定により、ユーザー名やクライアントIDなどのクライアント接続メタデータから部分文字列を抽出・加工し、クライアント属性として設定します。この抽出は認証プロセスの前に行われるため、HTTPリクエストボディのテンプレートや認証・認可リクエストのSQLテンプレートで利用可能な状態になります。

クライアント属性機能は設定ファイルまたはダッシュボードから設定可能です。ダッシュボードで属性抽出を設定するには、**Management** -> **MQTT Settings** をクリックし、**Client Attributes** で **Add** をクリックして属性名と属性式を追加します。

![client_attributes_config_ee](./assets/client_attributes_config_ee.png)

ここで、

- **Attribute** は属性名です。
- **Attribute Expression** は属性を抽出するための設定です。

属性式は[Variform 式](../../guides/configuration/configuration.md#variform-expressions)および[組み込み関数](../../guides/configuration/configuration.md#pre-defined-functions)を使用して動的に値を処理できます。例：

- ドット区切りのクライアントIDのプレフィックスを抽出：`nth(1, tokens(clientid, '.'))`
- ユーザー名の一部を切り出す：`substr(username, 0, 5)`

対応する設定ファイル例は以下の通りです。

```bash
mqtt {
    client_attrs_init = [
        {
            expression = "nth(1, tokens(clientid, '.'))"
            set_as_attr = clientid_prefix
        },
        {
            expression = "substr(username, 0, 5)"
            set_as_attr = sub_username
        }
    ]
}
```

属性式で設定可能な値は以下の通りです。

- `clientid`: クライアントID
- `username`: ユーザー名
- `cn`: TLS証明書のCNフィールド
- `dn`: TLS証明書のDNフィールド
- `user_property.*`: MQTT CONNECTパケットのUser-Propertyから属性値を抽出（例：`user_property.foo`）
- `zone`: MQTTリスナーから継承されたゾーン名

クライアント属性の詳細な設定方法は、[EMQX Enterprise 設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

### クライアント認証プロセス中の設定

クライアント認証プロセス中に、認証サーバーから返される情報に基づいてクライアント属性を設定できます。現在サポートされている方法は以下の通りです。

- [JWT 認証](../../guides/access-control/authn/jwt.md)：トークン発行時のペイロード内 `client_attrs` フィールドにクライアント属性を設定。
- [HTTP 認証](../../guides/access-control/authn/http.md)：HTTP認証成功時のレスポンス内 `client_attrs` フィールドにクライアント属性を設定。

属性のキーと値は文字列である必要があります。この方法により、認証結果に応じて動的に属性を設定でき、柔軟な運用が可能です。

### 認証データのマージ

両方の方法や複数の認証器を使用してクライアント属性を設定する場合、EMQX は属性名と設定順に基づいて属性をマージします。

- クライアントメタデータから抽出した属性は、認証器によって設定された属性で上書きされます。
- 認証チェーン内で複数の認証器が属性を設定した場合、後に設定された属性が先のものを上書きします。

## クライアント属性の活用

EMQX の他の機能では、`${client_attrs.NAME}` プレースホルダーを使ってクライアント属性を抽出し、設定やデータの一部として利用できます。現時点ではクライアント認証および認可でのみサポートされており、今後さらに機能拡充が予定されています。

### クライアント認証

SQL文、クエリコマンド、HTTPリクエストボディの動的パラメータとして[認証プレースホルダー](../../guides/access-control/authn/authn.md#authentication-placeholders)を使用します。例：

```sql
# MySQL/PostgreSQL - 認証クエリSQL
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE sn = ${client_attrs.sn} LIMIT 1

# HTTP - 認証リクエストボディ
{
 "sn": "${client_attrs.sn}",
 "password": "${password}"
}
```

具体的な使い方は各認証器のドキュメントをご参照ください。

::: tip

クライアント認証ではクライアントメタデータから設定された属性のみ使用可能です。

:::

### クライアント認可

SQL文、クエリコマンド、トピックに対して[データクエリプレースホルダー](../../guides/access-control/authz/authz.md#placeholders-in-data-queries)および[トピックプレースホルダー](../../guides/access-control/authz/authz.md#topic-placeholders)を使用します。

#### 例シナリオ：

クライアントごとに `role`、`productId`、`deviceId` といったクライアント属性を設定し、これらを認可チェックに利用します。

- **role**：クライアントのアクセス権限を制限し、`admin` ロールのクライアントのみが管理メッセージ（例：`admin/#` にマッチするトピック）のサブスクライブおよびパブリッシュを許可。
- **productId**：クライアントが現在の製品に関連するOTAメッセージ（例：`OTA/{productId}`）のみサブスクライブ可能に制限。
- **deviceId**：クライアントが自身に属するトピックのみパブリッシュおよびサブスクライブ可能に制限。
  - パブリッシュ：`up/{productId}/{deviceId}`
  - サブスクライブ：`down/{productId}/{deviceId}`

以下のルールを[認可 - 組み込みデータベース](../../guides/access-control/authz/mnesia.md)で設定して実現します。

| 権限       | 操作                 | トピック                                                     |
| ---------- | -------------------- | ------------------------------------------------------------ |
| 許可       | サブスクライブ＆パブリッシュ | `${client_attrs.role}/#`                                    |
| 許可       | サブスクライブ       | `OTA/${client_attrs.productId}`                              |
| 許可       | パブリッシュ         | `up/${client_attrs.productId}/${client_attrs.deviceId}`      |
| 許可       | サブスクライブ       | `down/${client_attrs.productId}/${client_attrs.deviceId}`    |

クライアントIDのような静的プロパティを直接使う場合と比べて、より柔軟にクライアント認可を管理できます。この柔軟性により、異なるロール、製品、デバイスに応じた細かなアクセス権限制御が可能となります。
