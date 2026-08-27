# MQTT クライアント属性

EMQX のクライアント属性は、開発者が異なるアプリケーションシナリオの要件に応じて MQTT クライアントに追加属性を定義・設定できる仕組みを提供します。これらの属性は、EMQX 内での認証、認可、データ統合、MQTT 拡張機能の強化に不可欠であり、柔軟な開発を支援します。クライアントのメタデータを活用することで、MQTT クライアント識別の柔軟なテンプレート化も可能となり、個別のクライアント設定や認証プロセスの効率化に寄与し、開発の適応性と効率性を高めます。

## ワークフロー

クライアント属性の設定、保存、利用の流れは以下の通りです。

**1. クライアント属性の設定**

クライアントが EMQX に正常に接続すると、EMQX は接続および認証イベントをトリガーし、事前定義された設定に基づいて[クライアント属性を設定](#set-client-attributes)します。

**2. クライアント属性の保存と破棄**

設定された属性はクライアントセッションの `client_attrs` フィールドにキー・バリュー形式で保存されます。クライアントセッション終了時にこれらの属性は削除されます。

永続セッションの場合、クライアントが引き継ぐ際にセッション内のクライアント属性を置き換え上書きします。これ以外にクライアント属性を変更または削除する方法はありません。

**3. クライアント属性の利用**

EMQX の他の機能では、関連設定項目内で `${client_attrs.NAME}` プレースホルダーを使用し、属性値を動的に抽出して設定やデータの一部として利用できます。

## クライアント属性の設定

クライアントが EMQX に正常に接続すると、EMQX は接続および認証イベントをトリガーし、事前定義された設定に基づいてクライアント属性を設定します。現在、以下の2つの方法をサポートしています。

- クライアントメタデータからの抽出
- クライアント認証プロセス中の設定

### クライアントメタデータからの抽出

事前設定により、ユーザー名やクライアントIDなどのクライアント接続メタデータから部分文字列を抽出・加工し、クライアント属性として設定します。この抽出は認証プロセスの前に行われるため、認証・認可リクエストの HTTP ボディテンプレートや SQL テンプレートで使用可能な状態となります。

クライアント属性機能は設定ファイルまたはダッシュボードから設定できます。ダッシュボードで属性抽出を設定するには、**Management** -> **MQTT Settings** をクリックし、**Client Attributes** で **Add** をクリックして属性名と属性式を追加します。

![client_attributes_config_ee](./assets/client_attributes_config_ee.png)

ここで、

- **Attribute** は属性名です。
- **Attribute Expression** は属性を抽出するための設定式です。

属性式は [Variform 式](../configuration/configuration.md#variform-expressions) と [事前定義関数](../configuration/configuration.md#pre-defined-functions) を利用して動的に値を処理できます。例：

- クライアントIDのドット区切りの接頭辞を抽出：`nth(1, tokens(clientid, '.'))`
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
- `cn`: TLS 証明書の CN フィールド
- `dn`: TLS 証明書の DN フィールド
- `user_property.*`: MQTT CONNECT パケットの User-Property から属性値を抽出（例：`user_property.foo`）
- `zone`: MQTT リスナーから継承されるゾーン名

クライアント属性設定の詳細は [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) をご参照ください。

### クライアント認証プロセス中の設定

クライアント認証プロセス中に、認証結果に基づいてクライアント属性を設定できます。現在サポートされている方法は以下の通りです。

- [JWT 認証](../access-control/authn/jwt.md)：トークン発行時のペイロード内 `client_attrs` フィールドにクライアント属性を設定
- [HTTP 認証](../access-control/authn/http.md)：HTTP 認証成功レスポンスの `client_attrs` フィールドにクライアント属性を設定

属性のキーと値は文字列である必要があります。この方法により、認証結果に応じた動的な属性設定が可能となり、利用の柔軟性が向上します。

### 認証データのマージ

複数の方法や複数の認証器でクライアント属性を設定する場合、EMQX は属性名と設定順に基づきマージします。

- クライアントメタデータから抽出した属性は認証器によって設定された属性で上書きされます。
- 認証チェーン内で複数の認証器が属性を設定した場合、後から設定された属性が前の属性を上書きします。

## クライアント属性の活用

EMQX の他機能では `${client_attrs.NAME}` プレースホルダーを使ってクライアント属性を抽出し、設定やデータの一部として利用できます。現時点ではクライアント認証および認可でのみサポートされており、今後さらに機能拡充が予定されています。

### クライアント認証

SQL 文、クエリコマンド、HTTP リクエストボディの動的パラメータとして[認証プレースホルダー](../access-control/authn/authn.md#authentication-placeholders)を利用できます。例：

```sql
# MySQL/PostgreSQL - 認証クエリ SQL
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE sn = ${client_attrs.sn} LIMIT 1

# HTTP - 認証リクエスト Body
{ 
 "sn": "${client_attrs.sn}",
 "password": "${password}"
}
```

詳細な使い方は各認証器のドキュメントをご参照ください。

::: tip

クライアント認証ではクライアントメタデータから設定された属性のみ利用可能です。

:::

### クライアント認可

SQL 文、クエリコマンド、トピックで[データクエリプレースホルダー](../access-control/authz/authz.md#placeholders-in-data-queries)および[トピックプレースホルダー](../access-control/authz/authz.md#topic-placeholders)を利用できます。

#### 利用例

クライアントごとに `role`、`productId`、`deviceId` といったクライアント属性を設定し、認可チェックに利用します。

- **role**：クライアントのアクセス権限を制限し、`admin` ロールのクライアントのみ管理メッセージ（例：`admin/#` にマッチするトピック）をサブスクライブ・パブリッシュ可能とする。
- **productId**：クライアントが現在の製品に対応した OTA メッセージ（例：`OTA/{productId}`）のみサブスクライブ可能とする。
- **deviceId**：クライアントが自身に属するトピックのみパブリッシュ・サブスクライブ可能とする。
  - パブリッシュ：`up/{productId}/{deviceId}`
  - サブスクライブ：`down/{productId}/{deviceId}`

[認可 - 組み込みデータベース](../access-control/authz/mnesia.md)を使い、以下のルールを設定して実現します。

| 許可       | 操作                 | トピック                                                     |
| ---------- | -------------------- | ------------------------------------------------------------ |
| Allow      | Subscribe & Publish  | `${client_attrs.role}/#`                                     |
| Allow      | Subscribe            | `OTA/${client_attrs.productId}`                              |
| Allow      | Publish              | `up/${client_attrs.productId}/${client_attrs.deviceId}`      |
| Allow      | Subscribe            | `down/${client_attrs.productId}/${client_attrs.deviceId}`    |

クライアントIDなどの静的プロパティを直接使うよりも、クライアント属性を活用することで、より柔軟にクライアント認可を管理できます。この柔軟性により、異なるロール、製品、デバイスに基づく細かなアクセス権限制御が可能となります。
