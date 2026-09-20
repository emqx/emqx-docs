# MQTT クライアント属性

EMQX のクライアント属性は、開発者が異なるアプリケーションシナリオの要件に応じて MQTT クライアントに追加属性を定義・設定できる仕組みを提供します。これらの属性は、EMQX 内での認証、認可、データ統合、MQTT 拡張機能の強化に不可欠であり、柔軟な開発を促進します。クライアントのメタデータを活用することで、MQTT クライアント識別の柔軟なテンプレート化もサポートし、個別のクライアント設定や認証プロセスの効率化に寄与し、開発の適応性と効率性を高めます。

## ワークフロー

クライアント属性の設定、保存、利用の流れは以下の通りです。

**1. クライアント属性の設定**

クライアントが EMQX に正常に接続すると、EMQX は接続および認証イベントをトリガーし、この過程で事前定義された設定に基づき[クライアント属性を設定](#set-client-attributes)します。

**2. クライアント属性の保存と破棄**

設定された属性は、クライアントセッションの `client_attrs` フィールドにキー・バリュー形式で保存されます。クライアントセッションが終了すると、これらの属性は削除されます。

永続セッションの場合、クライアントが引き継ぐ際にセッション内のクライアント属性は置き換えられ上書きされます。これ以外にクライアント属性を変更・削除する方法はありません。

**3. クライアント属性の利用**

EMQX の他の機能では、関連設定項目内で `${client_attrs.NAME}` プレースホルダーを使用し、属性値を動的に抽出して設定やデータの一部として利用できます。

## クライアント属性の設定

クライアントが EMQX に正常に接続すると、EMQX は接続および認証イベントをトリガーし、事前定義された設定に基づいてクライアント属性を設定します。現在、以下の2つの方法がサポートされています。

- クライアントメタデータからの抽出
- クライアント認証プロセス中の設定

### クライアントメタデータからの抽出

事前設定により、ユーザー名やクライアントIDなどのクライアント接続メタデータから部分文字列を抽出・加工し、クライアント属性として設定します。この抽出は認証プロセスの前に行われ、HTTPリクエストボディテンプレートや認証・認可リクエストのSQLテンプレートでの利用など、後続の処理に備えられます。

クライアント属性機能は設定ファイルまたはダッシュボードから設定可能です。ダッシュボードで属性抽出を設定するには、**Management** -> **MQTT Settings** をクリックし、**Client Attributes** で **Add** をクリックして属性名と属性式を追加します。

![client_attributes_config_ee](./assets/client_attributes_config_ee.png)

ここで、

- **Attribute** は属性の名前です。
- **Attribute Expression** は属性を抽出するための設定式です。

属性式は[Variform式](../../guides/configuration/configuration.md#variform-expressions)および[組み込み関数](../../guides/configuration/configuration.md#pre-defined-functions)を使って動的に値を処理できます。例：

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

属性式で設定可能な値は以下です。

- `clientid`: クライアントID
- `username`: ユーザー名
- `cn`: TLS証明書のCNフィールド
- `dn`: TLS証明書のDNフィールド
- `cert_san.dns`: TLSクライアント証明書のDNS名
- `cert_san.ip`: TLSクライアント証明書のIPv4およびIPv6アドレス
- `cert_san.email`: TLSクライアント証明書のメールアドレス
- `cert_san.uri`: TLSクライアント証明書のURI
- `user_property.*`: MQTT CONNECTパケットのUser-Propertyから属性値を抽出（例：`user_property.foo`）
- `zone`: MQTTリスナーから継承されたゾーン名

クライアント属性の詳細な設定については、[EMQX Enterprise 設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

#### 証明書のSubject Alternative Namesからクライアント属性を初期化

EMQX Enterprise 6.3.0以降、`mqtt.client_attrs_init` 式で TLS接続がEMQXで終了する際にクライアント証明書のSubject Alternative Names（SANs）を抽出できます。各 `cert_san.*` 変数は配列です。Variformの配列関数で要素を選択するか、複数要素を結合して1つのクライアント属性にできます。

例として、以下の設定は `client_attrs.san_dns` に最初のDNS名を、`client_attrs.san_dns_all` にカンマ区切りの全DNS名を設定します。

```hocon
mqtt {
    client_attrs_init = [
        {
            expression = "nth(1, cert_san.dns)"
            set_as_attr = san_dns
        },
        {
            expression = "join_to_string(',', cert_san.dns)"
            set_as_attr = san_dns_all
        }
    ]
}
```

`cert_san.*` 変数はクライアント属性初期化時のみ利用可能です。認証、認可、その他サポート機能でSAN値を使う場合は、一度クライアント属性として保存し、`${client_attrs.NAME}` で参照してください。

証明書に要求されたSANタイプが含まれない場合、該当変数は空配列となります。`nth()` で存在しない要素を選択した場合、EMQXは対象のクライアント属性を設定しません。抽出されたSAN値に改行（`\r` や `\n`）などの非表示制御文字が含まれる場合、EMQXは接続を拒否します。

::: warning 重要なお知らせ
EMQXはTLS接続がEMQXで終了し、クライアントが証明書をEMQX TLSリスナーに提示した場合にのみSANを抽出可能です。Proxy Protocol v2はSAN情報を含みません。ロードバランサーがTLSを終了する場合、`cert_san.*` の値はEMQXで利用できません。ロードバランサーがTLS接続を終了せずEMQXに転送する場合は、EMQXはクライアント証明書からSANを抽出できます。
:::

### クライアント認証プロセス中の設定

クライアント認証プロセス中に、認証結果に基づきクライアント属性を設定できます。現在サポートされているのは以下です。

- [JWT認証](../../guides/access-control/authn/jwt.md)：トークン発行時にトークンペイロードの `client_attrs` フィールドにクライアント属性を設定
- [HTTP認証](../../guides/access-control/authn/http.md)：HTTP認証成功レスポンスの `client_attrs` フィールドにクライアント属性を設定

属性のキーと値は文字列である必要があります。この方法により認証結果に応じて動的に属性を設定でき、柔軟な利用が可能です。

### 認証データのマージ

両方法や複数認証器でクライアント属性を設定する場合、EMQXは属性名と設定順に基づいて属性をマージします。

- クライアントメタデータから抽出した属性は認証器によって設定された属性で上書きされます。
- 認証チェーン内で複数の認証器が属性を設定した場合、後から設定された属性が前のものを上書きします。

## クライアント属性の活用

EMQXの他機能では、`${client_attrs.NAME}` プレースホルダーを使ってクライアント属性を抽出し、設定やデータの一部として利用できます。現時点ではクライアント認証と認可でのみサポートされており、今後さらに機能拡張が予定されています。

### クライアント認証

SQL文、クエリコマンド、HTTPリクエストボディの動的パラメータとして[認証プレースホルダー](../../guides/access-control/authn/authn.md#authentication-placeholders)を使用できます。例：

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

クライアント認証で使用できる属性はクライアントメタデータから設定されたものに限られます。

:::

### クライアント認可

SQL文、クエリコマンド、トピックの動的パラメータとして[データクエリプレースホルダー](../../guides/access-control/authz/authz.md#placeholders-in-data-queries)および[トピックプレースホルダー](../../guides/access-control/authz/authz.md#topic-placeholders)を使用できます。

#### 例シナリオ：

`role`、`productId`、`deviceId` などのクライアント属性を各クライアントに設定し、認可チェックに利用します。

- **role**：クライアントのアクセス権限を制限し、`admin` ロールのクライアントのみが管理メッセージ（例：`admin/#` にマッチするトピック）をサブスクライブおよびパブリッシュ可能とする。
- **productId**：クライアントが現在の製品に該当するOTAメッセージ（例：`OTA/{productId}`）のみをサブスクライブ可能とする。
- **deviceId**：クライアントが自身に属するトピックのみパブリッシュおよびサブスクライブ可能とする。
  - パブリッシュ：`up/{productId}/{deviceId}`
  - サブスクライブ：`down/{productId}/{deviceId}`

[認可 - 組み込みデータベース](../../guides/access-control/authz/mnesia.md)を使い、以下のルールを設定して実現します。

| 権限     | 操作                 | トピック                                                     |
| -------- | -------------------- | ------------------------------------------------------------ |
| 許可     | サブスクライブ＆パブリッシュ | `${client_attrs.role}/#`                                    |
| 許可     | サブスクライブ       | `OTA/${client_attrs.productId}`                             |
| 許可     | パブリッシュ         | `up/${client_attrs.productId}/${client_attrs.deviceId}`     |
| 許可     | サブスクライブ       | `down/${client_attrs.productId}/${client_attrs.deviceId}`   |

クライアントIDなどの静的プロパティを直接使うよりも、クライアント属性を活用することでより柔軟な認可管理が可能です。この柔軟性により、異なるロール、製品、デバイスに基づく細かなアクセス権限制御が実現できます。
