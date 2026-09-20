# MQTT クライアント属性

EMQX のクライアント属性は、開発者がさまざまなアプリケーションシナリオの要件に応じて MQTT クライアントに追加属性を定義・設定できる仕組みを提供します。これらの属性は、EMQX 内での認証、認可、データ統合、MQTT 拡張機能の強化に不可欠であり、柔軟な開発を促進します。クライアントのメタデータを活用することで、MQTT クライアント識別のための柔軟なテンプレート化もサポートし、個別化されたクライアント設定や認証プロセスの効率化を実現し、開発の適応性と効率性を高めます。

## ワークフロー

クライアント属性の設定、保存、利用の流れは以下の通りです。

**1. クライアント属性の設定**

クライアントが EMQX に正常に接続すると、EMQX は接続および認証イベントをトリガーし、この過程で事前定義された設定に基づき[クライアント属性が設定されます](#set-client-attributes)。

**2. クライアント属性の保存と破棄**

設定された属性はクライアントセッションの `client_attrs` フィールドにキー・バリュー形式で保存されます。クライアントセッション終了時にこれらの属性は削除されます。

永続セッションの場合、クライアントの引き継ぎ時にセッション内のクライアント属性は置き換えられ上書きされます。これ以外にクライアント属性を変更または削除する方法はありません。

**3. クライアント属性の利用**

EMQX の他の機能では、関連設定項目内で `${client_attrs.NAME}` プレースホルダーを使用し、属性値を動的に抽出して設定やデータの一部として利用できます。

## クライアント属性の設定

クライアントが EMQX に正常に接続すると、EMQX は接続および認証イベントをトリガーし、事前定義された設定に基づいてクライアント属性を設定します。現在、以下の2つの方法がサポートされています。

- クライアントメタデータからの抽出
- クライアント認証プロセス中の設定

### クライアントメタデータからの抽出

事前設定により、ユーザー名やクライアントIDなどのクライアント接続メタデータから部分文字列を抽出・加工し、クライアント属性として設定します。この抽出は認証プロセスの前に行われるため、認証・認可リクエストの HTTP ボディテンプレートや SQL テンプレートでの利用に備えられます。

クライアント属性機能は設定ファイルまたはダッシュボードから設定可能です。ダッシュボードで属性抽出を設定する場合は、**Management** -> **MQTT Settings** をクリックし、**Client Attributes** で **Add** をクリックして属性名と属性式を追加します。

![client_attributes_config_ee](./assets/client_attributes_config_ee.png)

ここで、

- **Attribute** は属性名です。
- **Attribute Expression** は属性抽出の設定式です。

属性式は [Variform 式](../../guides/configuration/configuration.md#variform-expressions) および [事前定義関数](../../guides/configuration/configuration.md#pre-defined-functions) を使用して値を動的に処理できます。例：

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

- `clientid`：クライアントID
- `username`：ユーザー名
- `cn`：TLS 証明書の CN フィールド
- `dn`：TLS 証明書の DN フィールド
- `user_property.*`：MQTT CONNECT パケットの User-Property から属性値を抽出（例：`user_property.foo`）
- `zone`：MQTT リスナーから継承されるゾーン名

クライアント属性設定の詳細は [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) を参照してください。

### クライアント認証プロセス中の設定

クライアント認証プロセス中に、認証結果に基づいてクライアント属性を設定できます。現在サポートされている方法は以下の通りです。

- [JWT 認証](../../guides/access-control/authn/jwt.md)：トークン発行時にトークンペイロードの `client_attrs` フィールドにクライアント属性を設定。
- [HTTP 認証](../../guides/access-control/authn/http.md)：HTTP 認証成功レスポンスの `client_attrs` フィールドにクライアント属性を設定。

属性のキーと値は文字列でなければなりません。この方法により、認証結果に応じて動的に属性を設定でき、柔軟な利用が可能です。

### 認証データのマージ

両方法でクライアント属性を設定する場合や複数認証器を使用する場合、EMQX は属性名と設定順に基づき属性をマージします。

- クライアントメタデータから抽出された属性は認証器によって設定された属性で上書きされます。
- 認証チェーン内で複数の認証器が属性を設定する場合、後から設定された属性が先のものを上書きします。

## クライアント属性の活用

EMQX の他機能では、`${client_attrs.NAME}` プレースホルダーを用いてクライアント属性を抽出し、設定やデータの一部として利用できます。現時点ではクライアント認証と認可でのみサポートされており、今後さらに機能拡張が予定されています。

### クライアント認証

SQL 文、クエリコマンド、HTTP リクエストボディの動的パラメータとして [認証プレースホルダー](../../guides/access-control/authn/authn.md#authentication-placeholders) を使用できます。例：

```sql
# MySQL/PostgreSQL - 認証クエリSQL
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE sn = ${client_attrs.sn} LIMIT 1

# HTTP - 認証リクエストボディ
{
 "sn": "${client_attrs.sn}",
 "password": "${password}"
}
```

具体的な使い方は各認証器のドキュメントを参照してください。

::: tip

クライアント認証ではクライアントメタデータから設定された属性のみ利用可能です。

:::

### クライアント認可

SQL 文、クエリコマンド、トピックの動的パラメータとして [データクエリプレースホルダー](../../guides/access-control/authz/authz.md#placeholders-in-data-queries) および [トピックプレースホルダー](../../guides/access-control/authz/authz.md#topic-placeholders) を使用できます。

#### 例シナリオ：

クライアントごとに `role`、`productId`、`deviceId` といったクライアント属性を設定し、認可チェックに利用します。

- **role**：クライアントのアクセス権限を制限し、`admin` ロールのクライアントのみが管理メッセージ（例：`admin/#` にマッチするトピック）をサブスクライブ・パブリッシュ可能とする。
- **productId**：クライアントが現在の製品に適用される OTA メッセージ（例：`OTA/{productId}`）のみをサブスクライブ可能とする。
- **deviceId**：クライアントが自身に属するトピックのみパブリッシュ・サブスクライブ可能とする。
  - パブリッシュ：`up/{productId}/{deviceId}`
  - サブスクライブ：`down/{productId}/{deviceId}`

[認可 - 組み込みデータベース](../../guides/access-control/authz/mnesia.md) を使い、以下のルールを設定して実現します。

| 権限     | 操作               | トピック                                                     |
| -------- | ------------------ | ------------------------------------------------------------ |
| 許可     | サブスクライブ＆パブリッシュ | `${client_attrs.role}/#`                                    |
| 許可     | サブスクライブ     | `OTA/${client_attrs.productId}`                             |
| 許可     | パブリッシュ       | `up/${client_attrs.productId}/${client_attrs.deviceId}`     |
| 許可     | サブスクライブ     | `down/${client_attrs.productId}/${client_attrs.deviceId}`   |

クライアントIDなどの静的プロパティを直接使うよりも、クライアント属性を用いることで認可管理がより柔軟になります。この柔軟性により、異なるロール、製品、デバイスに基づく細かなアクセス権限制御が可能です。
