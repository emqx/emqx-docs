# MQTT クライアント属性

EMQX のクライアント属性は、開発者が異なるアプリケーションシナリオの要件に基づいて MQTT クライアントに追加属性を定義・設定できる仕組みを提供します。これらの属性は、EMQX 内での認証、認可、データ統合、および MQTT 拡張機能の強化に不可欠であり、柔軟な開発を支援します。クライアントのメタデータを活用することで、MQTT クライアント識別の柔軟なテンプレート化も可能となり、個別化されたクライアント設定や認証プロセスの効率化に寄与し、開発の適応性と効率性を高めます。

## ワークフロー

クライアント属性の設定、保存、利用の流れは以下の通りです。

**1. クライアント属性の設定**

クライアントが EMQX に正常に接続すると、EMQX は接続および認証イベントをトリガーし、あらかじめ定義された設定に基づいて[クライアント属性を設定](#クライアント属性の設定)します。

**2. クライアント属性の保存と破棄**

設定された属性はクライアントセッションの `client_attrs` フィールドにキー・バリュー形式で保存されます。クライアントセッションが終了すると、これらの属性は削除されます。

永続セッションの場合、クライアントが引き継ぐ際にセッション内のクライアント属性を置き換え上書きします。それ以外にはクライアント属性を変更または削除する方法はありません。

**3. クライアント属性の利用**

EMQX の他の機能では、関連設定項目内で `${client_attrs.NAME}` プレースホルダーを使用し、属性値を動的に抽出して設定やデータの一部として利用できます。

## クライアント属性の設定

クライアントが EMQX に正常に接続すると、接続および認証イベントがトリガーされ、あらかじめ定義された設定に基づいてクライアント属性が設定されます。現在、以下の2つの方法がサポートされています。

- クライアントメタデータからの抽出
- クライアント認証プロセス中の設定

### クライアントメタデータからの抽出

事前設定により、ユーザー名やクライアントIDなどのクライアント接続メタデータから部分文字列を抽出・加工し、クライアント属性として設定します。この抽出は認証プロセスの前に行われるため、認証・認可リクエストの HTTP ボディテンプレートや SQL テンプレートで使用するなど、後続の処理で属性を利用可能です。

クライアント属性機能は設定ファイルまたはダッシュボードから設定できます。ダッシュボードで属性抽出を設定するには、**Management** -> **MQTT Settings** をクリックし、**Client Attributes** で **Add** をクリックして属性名と属性式を追加します。

![client_attributes_config_ee](./assets/client_attributes_config_ee.png)

ここで、

- **Attribute** は属性の名前です。
- **Attribute Expression** は属性を抽出するための設定式です。

属性式は [Variform 式](../configuration/configuration.md#variform-expressions) および [組み込み関数](../configuration/configuration.md#pre-defined-functions) をサポートし、値を動的に処理できます。例：

- ドット区切りのクライアントIDの接頭辞を抽出：`nth(1, tokens(clientid, '.'))`
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
- `cert_san.dns`：TLS クライアント証明書内の DNS 名
- `cert_san.ip`：TLS クライアント証明書内の IPv4 および IPv6 アドレス
- `cert_san.email`：TLS クライアント証明書内のメールアドレス
- `cert_san.uri`：TLS クライアント証明書内の URI
- `user_property.*`：MQTT CONNECT パケットの User-Property から属性値を抽出（例：`user_property.foo`）
- `zone`：MQTT リスナーから継承されるゾーン名

クライアント属性設定の詳細は [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) を参照してください。

#### 証明書の Subject Alternative Names からのクライアント属性初期化

EMQX Enterprise 6.3.0 以降、`mqtt.client_attrs_init` 式で TLS 接続が EMQX で終端される際にクライアント証明書の Subject Alternative Names（SAN）を抽出できます。各 `cert_san.*` 変数は配列であり、Variform の配列関数で要素を選択したり、複数要素を結合して 1 つのクライアント属性にできます。

例として、以下の設定は `client_attrs.san_dns` に最初の DNS 名を、`client_attrs.san_dns_all` にカンマ区切りのすべての DNS 名を設定します。

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

`cert_san.*` 変数はクライアント属性初期化時のみ利用可能です。認証、認可、その他対応機能で SAN 値を使用する場合は、まずクライアント属性として保存し、`${client_attrs.NAME}` で参照してください。

証明書に該当する SAN タイプが含まれない場合、対応する変数は空の配列となります。`nth()` で存在しない要素を選択した場合、EMQX は対象のクライアント属性を設定しません。抽出した SAN 値に改行コード（`\r`、`\n`）などの非表示制御文字が含まれる場合、EMQX は接続を拒否します。

::: warning 重要なお知らせ
EMQX は TLS 接続が EMQX で終端され、クライアントが証明書を EMQX の TLS リスナーに提示した場合にのみ SAN を抽出できます。Proxy Protocol v2 は SAN 情報を運びません。ロードバランサーが TLS を終端する場合、`cert_san.*` 値は EMQX で利用できません。ロードバランサーが TLS 接続を終端せずに EMQX に転送する場合は、EMQX はクライアント証明書から SAN を抽出可能です。
:::

### クライアント認証プロセス中の設定

クライアント認証プロセス中に、認証結果に基づいてクライアント属性を設定できます。現在サポートされている方法は以下の通りです。

- [JWT 認証](../access-control/authn/jwt.md)：トークン発行時のペイロード内 `client_attrs` フィールドに属性を設定
- [HTTP 認証](../access-control/authn/http.md)：HTTP 認証成功レスポンスの `client_attrs` フィールドに属性を設定

属性のキーと値は文字列である必要があります。この方法により、認証結果に応じて動的に属性を設定でき、柔軟な利用が可能です。

### 認証データのマージ

両方法でクライアント属性を設定した場合や複数の認証器を使用する場合、EMQX は属性名と設定順に基づいて属性をマージします。

- クライアントメタデータから抽出した属性は認証器によって設定された属性で上書きされます。
- 認証チェーン内で複数の認証器が属性を設定した場合、後に設定された属性が前の属性を上書きします。

## クライアント属性の応用

EMQX の他機能では、`${client_attrs.NAME}` プレースホルダーを使ってクライアント属性を抽出し、設定やデータの一部として利用できます。現在はクライアント認証および認可でのみサポートされており、今後さらに機能が拡充される予定です。

### クライアント認証

SQL 文、クエリコマンド、HTTP リクエストボディの動的パラメータとして[認証プレースホルダー](../access-control/authn/authn.md#authentication-placeholders)を使用できます。例：

```sql
# MySQL/PostgreSQL - 認証クエリ SQL
SELECT password_hash, salt, is_superuser FROM mqtt_user WHERE sn = ${client_attrs.sn} LIMIT 1

# HTTP - 認証リクエストボディ
{ 
 "sn": "${client_attrs.sn}",
 "password": "${password}"
}
```

具体的な使用方法は各認証器のドキュメントを参照してください。

::: tip

クライアント認証ではクライアントメタデータから設定された属性のみ使用可能です。

:::

### クライアント認可

SQL 文、クエリコマンド、トピックの動的パラメータとして[データクエリプレースホルダー](../access-control/authz/authz.md#placeholders-in-data-queries)および[トピックプレースホルダー](../access-control/authz/authz.md#topic-placeholders)を使用できます。

#### 例シナリオ：

クライアントごとに `role`、`productId`、`deviceId` といったクライアント属性を設定し、認可チェックに利用します。

- **role**：クライアントのアクセス権限を制限し、`admin` ロールのみが管理メッセージ（例：`admin/#` にマッチするトピック）のサブスクライブおよびパブリッシュを許可
- **productId**：クライアントが現在の製品に適用される OTA メッセージ（例：`OTA/{productId}`）のみをサブスクライブ可能に制限
- **deviceId**：クライアントが自身に属するトピックのみパブリッシュおよびサブスクライブ可能に制限
  - パブリッシュ：`up/{productId}/{deviceId}`
  - サブスクライブ：`down/{productId}/{deviceId}`

[認可 - 組み込みデータベース](../access-control/authz/mnesia.md) を使って以下のルールを設定し、実現します。

| 権限     | 操作               | トピック                                                     |
| -------- | ------------------ | ------------------------------------------------------------ |
| 許可     | サブスクライブ＆パブリッシュ | `${client_attrs.role}/#`                                  |
| 許可     | サブスクライブ     | `OTA/${client_attrs.productId}`                              |
| 許可     | パブリッシュ       | `up/${client_attrs.productId}/${client_attrs.deviceId}`      |
| 許可     | サブスクライブ     | `down/${client_attrs.productId}/${client_attrs.deviceId}`    |

クライアントIDなどの静的プロパティを直接使うよりも、クライアント属性を用いることで認可管理がより柔軟になります。これにより、異なるロールや製品、デバイスに基づく細かなアクセス制御が可能となります。
