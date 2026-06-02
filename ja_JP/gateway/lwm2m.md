# LwM2M ゲートウェイ

[LwM2M（Lightweight Machine-to-Machine）](https://lwm2m.openmobilealliance.org/) は、IoTデバイスおよびマシン間通信向けに設計されたプロトコルです。  
処理能力やメモリが限られたデバイスをサポートする軽量なプロトコルです。

<<<<<<< HEAD
EMQX の **LwM2M ゲートウェイ** は、LwM2M クライアントを受け入れ、それらのイベントやメッセージを MQTT のパブリッシュメッセージに変換します。

現時点の実装では、以下の制限があります：  
- UDP/DTLS ベースのトランスポート。  
- バージョン v1.0.2 のみサポート。v1.1.x および v1.2.x は未対応。  
- LwM2M ブートストラップサービスは含まれていません。  
=======
EMQX の **LwM2M ゲートウェイ** は、LwM2M クライアントを受け入れ、そのイベントやメッセージを MQTT パブリッシュメッセージに変換します。

現在の実装には以下の制限があります：
- UDP/DTLS ベースのトランスポートのみ対応。
- v1.0.2 のみサポート。v1.1.x および v1.2.x は未対応。
- LwM2M ブートストラップサービスは含まれていません。
>>>>>>> origin/release-5.10

## クイックスタート

EMQX 5.0 では、LwM2M ゲートウェイはダッシュボードから設定および有効化できます。

また、REST API や設定ファイルからも有効化可能です：

:::: tabs type:card

::: tab REST API

```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateways/lwm2m' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "lwm2m"
  "xml_dir": "etc/lwm2m_xml/",
  "qmode_time_window": "22s",
  "lifetime_min": "1s",
  "lifetime_max": "86400s",
  "auto_observe": true,
  "enable_stats": true,
  "update_msg_publish_condition": "contains_object_list",
  "mountpoint": "lwm2m/${endpoint_name}/",
  "translators": {
    "command": {"topic": "dn/#", "qos": 0},
    "response": {"topic": "up/resp", "qos": 0},
    "notify": {"topic": "up/notify", "qos": 0},
    "register": {"topic": "up/resp", "qos": 0},
    "update": {"topic": "up/update", "qos": 0}
  },
  "listeners": [
    {
      "type": "udp",
      "name": "default",
      "bind": "5783",
      "max_conn_rate": 1000,
      "max_connections": 1024000,
    }
  ],
}'
```
:::

::: tab Configuration

```properties
gateway.lwm2m {
  xml_dir = "etc/lwm2m_xml/"
  auto_observe = true
  enable_stats = true
  idle_timeout = "30s"
  lifetime_max = "86400s"
  lifetime_min = "1s"
  mountpoint = "lwm2m/${endpoint_namea}/"
  qmode_time_window = "22s"
  update_msg_publish_condition = "contains_object_list"
  translators {
    command {qos = 0, topic = "dn/#"}
    notify {qos = 0, topic = "up/notify"}
    register {qos = 0, topic = "up/resp"}
    response {qos = 0, topic = "up/resp"}
    update {qos = 0, topic = "up/update"}
  }
  listeners {
    udp {
      default {
        bind = "5783"
        max_conn_rate = 1000
        max_connections = 1024000
      }
    }
  }
}
```
:::

::::

::: tip
`base.hocon` でゲートウェイを設定する場合はノードごとに変更が必要ですが、ダッシュボードや REST API で設定するとクラスター全体に反映されます。
:::

<<<<<<< HEAD
LwM2M ゲートウェイは UDP および DTLS タイプのリスナーのみをサポートしています。  
設定可能なパラメータの完全なリストは以下を参照してください：  
[Gateway Configuration - Listeners](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/#V-gateway-S-gateway-lwm2m-S-lwm2m-listeners)

=======
LwM2M ゲートウェイは UDP および DTLS タイプのリスナーのみサポートしています。  
設定可能なパラメータの完全なリストについては、[Gateway Configuration - Listeners](../configuration/configuration-manual.html) を参照してください。
>>>>>>> origin/release-5.10

## 認証

LwM2M プロトコルはクライアントのエンドポイント名のみを提供し、ユーザー名とパスワードはありません。  
そのため、LwM2M ゲートウェイは [HTTP サーバー認証](../access-control/authn/http.md) のみをサポートしています。

<<<<<<< HEAD
例えば、REST API または設定ファイルを使って LwM2M ゲートウェイ用の HTTP 認証を作成する例：
=======
例えば、REST API または設定ファイルを使って LwM2M ゲートウェイの HTTP 認証を作成する例は以下の通りです：
>>>>>>> origin/release-5.10

:::: tabs type:card

::: tab REST API

```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway/lwm2m/authentication' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "method": "post",
  "url": "http://127.0.0.1:8080",
  "headers": {
    "content-type": "application/json"
  },
  "body": {
    "clientid": "${clientid}"
  },
  "pool_size": 8,
  "connect_timeout": "5s",
  "request_timeout": "5s",
  "enable_pipelining": 100,
  "ssl": {
    "enable": false,
    "verify": "verify_none"
  },
  "backend": "http",
  "mechanism": "password_based",
  "enable": true
}'
```
:::

::: tab Configuration

```properties
gateway.lwm2m {
  authentication {
    backend = "http"
    mechanism = "password_based"
    method = "post"
    connect_timeout = "5s"
    enable_pipelining = 100
    url = "http://127.0.0.1:8080"
    headers {
      "content-type" = "application/json"
    }
    body {
      clientid = "${clientid}"
    }
    pool_size = 8
    request_timeout = "5s"
    ssl.enable = false
  }
}
```
:::

::::

## メッセージフォーマット

LwM2M プロトコルのメッセージモデルは [リソースモデルと操作](https://technical.openmobilealliance.org/OMNA/LwM2M/LwM2MRegistry.html)に基づいています。  
これは MQTT プロトコルのパブリッシュ／サブスクライブモデルとは全く異なります。  
したがって、LwM2M ゲートウェイではこれらのメッセージモデルを互換させるためのメッセージフォーマットを作成する必要があります。

### クライアント登録インターフェース

#### Register

<<<<<<< HEAD
**Register** メッセージは LwM2M クライアントが LwM2M サーバーに自身を登録するために送信します。  
クライアントの情報や、エンドポイント名、ライフタイム、LwM2M バージョン、オブジェクト、オブジェクトインスタンスなどの機能情報を含みます。

Register メッセージはクライアントがサーバーとの通信を開始するために送る最初のメッセージです。

**Register** メッセージは LwM2M ゲートウェイによって以下の MQTT メッセージに変換されます。

**トピック**のフォーマットは以下の通りです：
=======
**Register** メッセージは LwM2M クライアントが LwM2M サーバーに対して自身を登録するために送信します。  
クライアントの情報や機能（エンドポイント名、ライフタイム、LwM2M バージョン、オブジェクト、オブジェクトインスタンスなど）を含みます。

Register メッセージはクライアントがサーバーとの通信を開始するために送信する最初のメッセージです。

**Register** メッセージは LwM2M ゲートウェイによって以下の MQTT メッセージに変換されます。

**トピック**の形式は以下の通りです：
>>>>>>> origin/release-5.10
```
{?mountpoint}{?translators.register.topic}
```

変数：  
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。  
- `{?translators.register.topic}` は LwM2M ゲートウェイ設定の `translators.register.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.register.topic` が `up/register` の場合、レスポンスメッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/register` となります。

<<<<<<< HEAD

**ペイロード**のフォーマットは以下の通りです：
=======
**ペイロード**の形式は以下の通りです：
>>>>>>> origin/release-5.10
```json
{
  "msgType": "register",
  "data": {
    "ep": {?EndpointName},
    "lwm2m": {?Version},
    "lt": {?LifetTime},
    "b": {?Binding},
    "objectList": {?ObjectList}
  }
}
```

<<<<<<< HEAD
変数：  
- `{?EndpointName}`: 文字列、LwM2M クライアントのエンドポイント名。  
- `{?Version}`: 文字列、LwM2M クライアントのプロトコルバージョン。  
- `{?LifeTime}`: 数値、LwM2M クライアントが要求するライフタイム。  
- `{?Binding}`: 列挙型、クライアントがサーバーとの通信に対応するバインディングタイプ。以下のいずれか：  
  * `"U"`: UDP  
  * `"UQ"`: データキューイング付きUDP  
- `{?ObjectList}`: 配列、LwM2M クライアントがサポートするオブジェクトおよびオブジェクトインスタンスのリスト。
=======
変数：
- `{?EndpointName}`: 文字列、LwM2M クライアントのエンドポイント名。
- `{?Version}`: 文字列、LwM2M クライアントのプロトコルバージョン。
- `{?LifeTime}`: 数値、LwM2M クライアントが要求するライフタイム。
- `{?Binding}`: 列挙型、クライアントがサーバーとの通信にサポートするバインディングタイプ。以下のいずれか：
  * `"U"`: UDP
  * `"UQ"`: データキューイング付き UDP
- `{?ObjectList}`: 配列、LwM2M クライアントがサポートするオブジェクトと利用可能なオブジェクトインスタンスのリスト。
>>>>>>> origin/release-5.10

例として、Register メッセージの完全な MQTT ペイロードは以下のようになります：
```json
{
  "msgType": "register",
  "data": {
    "objectList": ["/1/0", "/2/0", "/3/0", "/4/0", "/5/0", "/6/0", "/7/0"],
    "lwm2m": "1.0",
    "lt": 300,
    "ep": "testlwm2mclient",
    "b": "U"
  }
}
```

#### Update

**Update** メッセージは LwM2M クライアントが LwM2M サーバーに登録情報を更新するために送信します。  
Register メッセージに似ていますが、初回登録後に送信されます。  
<<<<<<< HEAD
Update メッセージはクライアントの機能や状態の変更（IPアドレスの変更や LwM2M オブジェクトのデータ更新など）を含みます。  
また、クライアントの登録期間を延長し、サーバーにクライアントがまだアクティブであることを知らせる役割もあります。
=======
Update メッセージはクライアントの機能や状態の変更（IPアドレスの変更や LwM2M オブジェクトによるデータの更新など）を含みます。  
また、クライアントの登録期間を延長し、クライアントがまだ利用可能かつアクティブであることをサーバーに知らせる手段です。
>>>>>>> origin/release-5.10

Update メッセージの送信頻度は Register メッセージで指定されたライフタイム値によって決まります。

**Update** メッセージは LwM2M ゲートウェイによって以下の MQTT メッセージに変換されます。

<<<<<<< HEAD
**トピック**のフォーマットは以下の通りです：
=======
**トピック**の形式は以下の通りです：
>>>>>>> origin/release-5.10
```
{?mountpoint}{?translators.update.topic}
```
変数：  
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。  
- `{?translators.update.topic}` は LwM2M ゲートウェイ設定の `translators.update.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.update.topic` が `up/update` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/update` となります。

<<<<<<< HEAD

**ペイロード**のフォーマットは以下の通りです：
=======
**ペイロード**の形式は以下の通りです：
>>>>>>> origin/release-5.10
```json
{
  "msgType": "update",
  "data": {
    "ep": {?EndpointName},
    "lwm2m": {?Version},
    "lt": {?LifetTime},
    "b": {?Binding},
    "objectList": {?ObjectList}
  }
}
```

Register メッセージと同じ変数です。

例として、Update メッセージの完全な MQTT ペイロードは以下のようになります：
```json
{
  "msgType": "update",
  "data": {
    "objectList": ["/7/0"],
    "lwm2m": "1.0",
    "lt": 300,
    "ep": "testlwm2mclient",
    "b": "U"
  }
}
```

### LwM2M デバイス管理およびサービス有効化インターフェース

<<<<<<< HEAD
このインターフェースは LwM2M サーバーが登録済み LwM2M クライアントのオブジェクトインスタンスやリソースにアクセスするために使用します。
=======
このインターフェースは、LwM2M サーバーが登録済みの LwM2M クライアントから利用可能なオブジェクトインスタンスやリソースにアクセスするために使用します。
>>>>>>> origin/release-5.10

「作成（Create）」「読み取り（Read）」「書き込み（Write）」「削除（Delete）」「実行（Execute）」「属性書き込み（Write-Attributes）」「探索（Discover）」の操作を通じてアクセスを提供します。

リソースがサポートする操作はオブジェクトテンプレートファイルを用いたオブジェクト定義で決まります。

LwM2M クライアントにコマンドを送信するには、EMQX に固定フォーマットの MQTT メッセージを送信します。  
これらのメッセージは LwM2M ゲートウェイによって正しい LwM2M メッセージに変換され、クライアントに送信されます。

<<<<<<< HEAD

コマンドリクエストの **トピック** は以下の通りです：
=======
**コマンドリクエストのトピック**は以下の通りです：
>>>>>>> origin/release-5.10
```
{?mountpoint}{?translators.command.topic}
```
変数：  
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。  
- `{?translators.command.topic}` は LwM2M ゲートウェイ設定の `translators.command.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.command.topic` が `dn/cmd` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/dn/cmd` となります。

<<<<<<< HEAD

コマンドリクエストの **ペイロード** フォーマットは以下の通りです：
=======
**コマンドリクエストのペイロード**の形式は以下の通りです：
>>>>>>> origin/release-5.10
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
<<<<<<< HEAD
変数：  
- `{?ReqID}`: 整数、リクエストID。レスポンスとの対応付けに使用。  
- `{?MsgType}`: 文字列、以下のいずれか：  
  - `"read"`: LwM2M Read  
  - `"discover"`: LwM2M Discover  
  - `"write"`: LwM2M Write  
  - `"write-attr"`: LwM2M Write Attributes  
  - `"execute"`: LwM2M Execute  
  - `"create"`: LwM2M Create  
  - `"delete"`: LwM2M Delete  
- `{?RequestData}`: JSON オブジェクト、`{?MsgType}` に依存し、次のセクションで説明します。

コマンドレスポンスの **トピック** は以下の通りです：
=======
変数：
- `{?ReqID}`: 整数、リクエストID。レスポンスとの対応付けに使用。
- `{?MsgType}`: 文字列、以下のいずれか：
  - `"read"`: LwM2M 読み取り
  - `"discover"`: LwM2M 探索
  - `"write"`: LwM2M 書き込み
  - `"write-attr"`: LwM2M 属性書き込み
  - `"execute"`: LwM2M 実行
  - `"create"`: LwM2M 作成
  - `"delete"`: LwM2M 削除
- `{?RequestData}`: JSON オブジェクト、`{?MsgType}` によって内容が異なり、以下のセクションで説明します。

**コマンドレスポンスのトピック**は以下の通りです：
>>>>>>> origin/release-5.10
```
{?mountpoint}{?translators.response.topic}
```
変数：  
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。  
- `{?translators.response.topic}` は LwM2M ゲートウェイ設定の `translators.response.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.response.topic` が `up/resp` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/resp` となります。

<<<<<<< HEAD

コマンドレスポンスの **ペイロード** フォーマットは以下の通りです：
=======
**コマンドレスポンスのペイロード**の形式は以下の通りです：
>>>>>>> origin/release-5.10
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
<<<<<<< HEAD
変数：  
- `{?ReqID}`: 整数、リクエストID。リクエストとの対応付けに使用。  
- `{?MsgType}`: 文字列、リクエストコマンドと同じ MsgType。  
=======
変数：
- `{?ReqID}`: 整数、リクエストID。リクエストとの対応付けに使用。
- `{?MsgType}`: 文字列、リクエストコマンドと同じ MsgType。
>>>>>>> origin/release-5.10
- `{?ResponseData}`: JSON オブジェクト、コマンドレスポンスの内容。

#### Read

「Read」操作は、リソース、リソースインスタンスの配列、オブジェクトインスタンス、またはオブジェクトの全インスタンスの値にアクセスするために使用します。

リクエストコマンドで **MsgType** が `"read"` の場合、**RequestData** の構造は以下の通りです：

```json
{
  "path": {?ResourcePath}
}
```
<<<<<<< HEAD
変数：  
- `{?ResourcePath}`: 文字列、要求されたリソースパス。以下の3つのシナリオがあります：  
  * オブジェクトIDのみ、例：`/3`。該当オブジェクトのすべてのインスタンスとリソースの値を読み取る。  
  * オブジェクトID/インスタンスID、例：`/3/0`。該当オブジェクトインスタンスのすべてのリソースの値を読み取る。  
  * フルパス（オブジェクトID/インスタンスID/リソースID）、例：`/3/0/1`。特定のリソースの値を読み取る。
=======
変数：
- `{?ResourcePath}`: 文字列、リクエストされたリソースパス。以下の3つのシナリオがあります：
  * オブジェクトIDのみ（例：`/3`）: そのオブジェクト配下の全インスタンスおよびリソースの値を読み取る。
  * オブジェクトID/インスタンスID（例：`/3/0`）: そのオブジェクトインスタンス配下の全リソースの値を読み取る。
  * フルパス（例：`/3/0/1`）: 特定のリソースの値を読み取る。
>>>>>>> origin/release-5.10

例として、Read コマンドの完全な MQTT ペイロードは以下の通りです：
```json
{
  "reqID": 1,
  "msgType": "read",
  "data": {
    "path": "/3/0/1"
  }
}
```

レスポンスでは、**ResponseData** の構造は以下の通りです：
```json
{
  "reqPath": {?ResourcePath},
  "code": {?ResponseCode},
  "codeMsg": {?ResponseMsg},
  "content": {?ReadResponseData}
}
```
<<<<<<< HEAD
変数：  
- `{?ResourcePath}`: 文字列、リクエストの `path` フィールドと同じ。  
- `{?ResponseCode}`: 文字列、LwM2M ステータスコード（例："2.01", "4.00" など）。  
- `{?ResponseMsg}`: 文字列、LwM2M レスポンスメッセージ（例："content", "bad_request"）。  
- `{?ReadResponseData}`: JSON オブジェクト、リクエストの値結果。リソース値の配列。
=======
変数：
- `{?ResourcePath}`: 文字列、リクエストの `path` フィールドと同じ。
- `{?ResponseCode}`: 文字列、LwM2M ステータスコード（例："2.01", "4.00" など）。
- `{?ResponseMsg}`: 文字列、LwM2M レスポンスメッセージ（例："content", "bad_request"）。
- `{?ReadResponseData}`: JSON オブジェクト、リクエストに対する値の結果。リソース値の配列。
>>>>>>> origin/release-5.10

例として、Read レスポンスの完全な MQTT ペイロードは以下の通りです：
```json
{
  "reqID": 1,
  "msgType": "read",
  "data": {
    "reqPath": "/3/0/1",
    "code": "2.05",
    "codeMsg": "content",
    "content": [
      {
        "value": "Lightweight M2M Client",
        "path": "/3/0/1"
      }
    ]
  }
}
```

#### Discover

<<<<<<< HEAD
"Discover" 操作はオブジェクト、オブジェクトインスタンス、リソースに付随する LwM2M 属性を探索するために使用します。  
この操作は特定のオブジェクトインスタンスにどのリソースがインスタンス化されているかを探索するために使えます。  
=======
「Discover」操作は、オブジェクト、オブジェクトインスタンス、リソースに付随する LwM2M 属性を探索するために使用します。  
この操作は、特定のオブジェクトインスタンスにどのリソースがインスタンス化されているかを探索するために利用できます。  
>>>>>>> origin/release-5.10
返されるペイロードは、対象のオブジェクト、オブジェクトインスタンス、リソースごとのアプリケーション/リンク形式の CoRE リンク [RFC6690] のリストです。

リクエストコマンドで **MsgType** が `"discover"` の場合、**RequestData** の構造は以下の通りです：

```json
{
  "path": {?ResourcePath}
}
```

<<<<<<< HEAD
**Read** メッセージと同じフォーマットです：  
* オブジェクトIDのみ、例：`/3`。該当オブジェクトのすべてのインスタンス、リソース、属性を探索。  
* オブジェクトID/インスタンスID、例：`/3/0`。該当オブジェクトインスタンスのすべてのリソースと属性を探索。  
* フルパス（オブジェクトID/インスタンスID/リソースID）、例：`/3/0/1`。特定のリソースのすべての属性を探索。
=======
**Read** メッセージと同様の形式です：
* オブジェクトIDのみ（例：`/3`）: そのオブジェクト配下の全インスタンス、リソース、属性を探索。
* オブジェクトID/インスタンスID（例：`/3/0`）: そのオブジェクトインスタンス配下の全リソース、属性を探索。
* フルパス（例：`/3/0/1`）: 特定リソースの全属性を探索。
>>>>>>> origin/release-5.10

例として、Discover コマンドの完全な MQTT ペイロードは以下の通りです：
```json
{
  "reqID": 2,
  "msgType": "discover",
  "data": {
    "path": "/3/0"
  }
}
```

レスポンスでは、**ResponseData** の構造は以下の通りです：
```json
{
  "reqPath": {?ResourcePath},
  "code": {?ResponseCode},
  "codeMsg": {?ResponseMsg},
  "content": {?DiscoverResponseData}
}
```
**Read** レスポンスと同じ変数ですが、`content` フィールドはリソースと属性の配列です。

例として、Discover レスポンスの完全な MQTT ペイロードは以下の通りです：
```json
{
  "reqID": 123,
  "msgType": "discover",
  "data": {
    "reqPath": "/3/0",
    "code": "2.05",
    "codeMsg": "content",
    "content": [
      "</3/0>;pmin=10",
      "</3/0/0>", "</3/0/1>", "</3/0/2>", "</3/0/3>", "</3/0/4>", "</3/0/5>",
      "</3/0/6>", "</3/0/7>", "</3/0/8>", "</3/0/9>", "</3/0/10>", "</3/0/11>",
      "</3/0/12>", "</3/0/13>", "</3/0/14>", "</3/0/15>", "</3/0/16>"
    ]
  }
}
```

#### Write

<<<<<<< HEAD
"Write" 操作はリソースの値、リソースインスタンス配列の値、またはオブジェクトインスタンスの複数リソースの値を変更するために使用します。
=======
「Write」操作は、リソースの値、リソースインスタンスの配列の値、またはオブジェクトインスタンスの複数リソースの値を変更するために使用します。
>>>>>>> origin/release-5.10

リクエストコマンドで **MsgType** が `"write"` の場合、**RequestData** は2つの構造が可能です。

単一リソースへの書き込みの場合：
```json
{
    "path": {?ResourcePath},
    "type": {?ValueType},
    "value": {?Value}
}
```
<<<<<<< HEAD
- `{?ResourcePath}`: 文字列、完全なリソースパス（例：`31024/11/1`）。  
- `{?ValueType}`: 文字列、"Time", "String", "Integer", "Float", "Boolean", "Opaque", "Objlnk" のいずれか。  
- `{?Value}`: リソースの値、`type` に依存。
=======
- `{?ResourcePath}`: 文字列、要求された完全なリソースパス（例：`31024/11/1`）。
- `{?ValueType}`: 文字列、"Time", "String", "Integer", "Float", "Boolean", "Opaque", "Objlnk" のいずれか。
- `{?Value}`: リソースの値で、`type` に依存。
>>>>>>> origin/release-5.10

例として、Write コマンドの完全な MQTT ペイロードは以下の通りです：
```json
{
  "reqID": 3,
  "msgType": "write",
  "data": {
    "path": "/31024/11/1",
    "type": "String",
    "value": "write_an_example_value"
  }
}
```

複数リソースへの書き込みの場合：
```json
{
  "basePath": {?BasePath},
  "content": [
    {
      "path": {?ResourcePath},
      "type": {?ValueType},
      "value": {?Value}
    }
  ]
}
```
フルパスは `{?BasePath}` と `"{ResourcePath}"` の連結です。

例として、Write コマンドの完全な MQTT ペイロードは以下の通りです：
```json
{
  "reqID": 3,
  "msgType": "write",
  "data": {
    "basePath": "/31024/11/",
    "content": [
      {
        "path": "1",
        "type": "String",
        "value": "write_the_1st_value"
      },
      {
        "path": "2",
        "type": "String",
        "value": "write_the_2nd_value"
      }
    ]
  }
}
```

#### Write-Attributes

<<<<<<< HEAD
LwM2M 1.0 では、"Write-Attributes" 操作で変更可能なのは `<NOTIFICATION>` クラスの属性のみです。
=======
LwM2M 1.0 では、"Write-Attributes" 操作で変更できるのは `<NOTIFICATION>` クラスの属性のみです。
>>>>>>> origin/release-5.10

この操作は複数の属性を同時に変更できます。

リクエストコマンドで **MsgType** が `"write-attr"` の場合、**RequestData** の構造は以下の通りです：

```json
{
  "path": {?ResourcePath},
  "pmin": {?PeriodMin},
  "pmax": {?PeriodMax},
  "gt": {?GreaterThan},
  "lt": {?LessThan},
  "st": {?Step}
}
```
<<<<<<< HEAD
変数：  
- `{?PeriodMin}`: 数値、通知の最小周期。  
- `{?PeriodMax}`: 数値、通知の最大周期。  
- `{?GreaterThan}`: 数値、リソース値がこの値より大きい場合に通知。  
- `{?LessThan}`: 数値、リソース値がこの値より小さい場合に通知。  
- `{?Step}`: 数値、リソース値の変化がこの値を超えた場合に通知。
=======
変数：
- `{?PeriodMin}`: 数値、通知の最小周期。
- `{?PeriodMax}`: 数値、通知の最大周期。
- `{?GreaterThan}`: 数値、リソース値がこの値を超えたときに通知。
- `{?LessThan}`: 数値、リソース値がこの値を下回ったときに通知。
- `{?Step}`: 数値、リソース値の変化がこの値を超えたときに通知。
>>>>>>> origin/release-5.10

#### Execute

<<<<<<< HEAD
"Execute" 操作は LwM2M サーバーがアクションを開始するために使用し、個別のリソースに対してのみ実行可能です。
=======
「Execute」操作は LwM2M サーバーがアクションを開始するために使用し、個別リソースに対してのみ実行可能です。
>>>>>>> origin/release-5.10

リクエストコマンドで **MsgType** が `"execute"` の場合、**RequestData** の構造は以下の通りです：
```json
{
  "path": {?ResourcePath},
  "args": {?Arguments}
}
```
<<<<<<< HEAD
変数：  
- `{?Arguments}`: 文字列、LwM2M Execute の引数。
=======
変数：
- `{?Arguments}`: 文字列、LwM2M Execute 引数。

#### Create
>>>>>>> origin/release-5.10

「Create」操作は LwM2M サーバーが LwM2M クライアント内にオブジェクトインスタンスを作成するために使用します。  
"Create" 操作はオブジェクトを対象としなければなりません。

リクエストコマンドで **MsgType** が `"create"` の場合、**RequestData** の構造は以下の通りです：

```json
{
  "basePath": "/{?ObjectID}",
  "content": [
    {
      "path": {?ResourcePath},
      "type": {?ValueType},
      "value": {?Value}
    }
  ]
}
```
<<<<<<< HEAD
変数：  
=======
変数：
>>>>>>> origin/release-5.10
- `{?ObjectID}`: 整数、LwM2M オブジェクトID。

#### Delete

「Delete」操作は LwM2M サーバーが LwM2M クライアント内のオブジェクトインスタンスを削除するために使用します。

リクエストコマンドで **MsgType** が `"delete"` の場合、**RequestData** の構造は以下の通りです：
```json
{
  "path": "{?ObjectID}/{?InstanceID}"
}
```
<<<<<<< HEAD
変数：  
=======
変数：
>>>>>>> origin/release-5.10
- `{?InstanceID}`: 整数、LwM2M オブジェクトインスタンスID。

### 情報報告インターフェース

<<<<<<< HEAD
このインターフェースは LwM2M サーバーが登録済み LwM2M クライアントのリソースの変更を監視し、新しい値が利用可能になると通知を受け取るために使用します。  
この監視関係はオブザーブ操作を LwM2M クライアントのオブジェクト、オブジェクトインスタンス、またはリソースに送信することで開始されます。  
監視はキャンセルオブザーブ操作が行われると終了します。

#### Observe（監視）および Cancel Observation（監視解除）

監視および監視解除リクエストの **トピック** は以下の通りです：
=======
このインターフェースは LwM2M サーバーが登録済み LwM2M クライアントのリソースの変化を監視し、新しい値が利用可能になった際に通知を受け取るために使用します。  
この監視関係は、オブジェクト、オブジェクトインスタンス、またはリソースに対して「Observe」操作を送信することで開始されます。  
監視は「Cancel Observation」操作が実行されると終了します。

#### Observe および Cancel Observation

Observe およびキャンセルリクエストの **トピック** は以下の通りです：
>>>>>>> origin/release-5.10
```
{?mountpoint}{?translators.command.topic}
```
変数：  
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。  
- `{?translators.command.topic}` は LwM2M ゲートウェイ設定の `translators.command.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.command.topic` が `dn/cmd` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/dn/cmd` となります。

<<<<<<< HEAD

監視および監視解除リクエストの **ペイロード** フォーマットは以下の通りです：
=======
Observe およびキャンセルリクエストの **ペイロード** 形式は以下の通りです：
>>>>>>> origin/release-5.10
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data":
    {
      "path": {?ResourcePath}
    }
}
```
<<<<<<< HEAD
変数：  
- `{?ReqID}`: 整数、リクエストID。リクエスト内の {?ReqID}。  
- `{?MsgType}`: 文字列、以下のいずれか：  
  * `"observe"`: LwM2M Observe  
  * `"cancel-observe"`: LwM2M Cancel Observe  
- `{?ResourcePath}`: 文字列、監視または監視解除対象の LwM2M リソース。完全なリソースパスのみサポート（例：`/3/0/1`）。
=======
変数：
- `{?ReqID}`: 整数、リクエストID。リクエスト内の {?ReqID}。
- `{?MsgType}`: 文字列、以下のいずれか：
  * `"observe"`: LwM2M Observe
  * `"cancel-observe"`: LwM2M Cancel Observe
- `{?ResourcePath}`: 文字列、監視またはキャンセル監視対象の LwM2M リソース。完全なリソースパスのみ対応（例：`/3/0/1`）。
>>>>>>> origin/release-5.10

例として、Observe コマンドの完全な MQTT ペイロードは以下の通りです：
```json
{
  "reqID": 10,
  "msgType": "observe",
  "data": {
    "path": "/31024/0/1"
  }
}
```

Observe レスポンスの **トピック** は以下の通りです：
```
{?mountpoint}{?translators.response.topic}
```
変数：  
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。  
- `{?translators.response.topic}` は LwM2M ゲートウェイ設定の `translators.response.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.response.topic` が `up/resp` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/resp` となります。

<<<<<<< HEAD

Observe レスポンスの **ペイロード** フォーマットは以下の通りです：
=======
Observe レスポンスの **ペイロード** 形式は以下の通りです：
>>>>>>> origin/release-5.10
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {
    "reqPath": {?RequestPath},
    "code": {?ResponseCode},
    "codeMsg": {?ResponseMsg},
    "content": [
      {
        "path": {?ResourcePath},
        "value": {?Value}
      }
    ]
  }
}
```
<<<<<<< HEAD
変数：  
- `{?ReqID}`: 整数、リクエストID。リクエストとの対応付けに使用。  
- `{?MsgType}`: 文字列、リクエストコマンドと同じ MsgType。  
- `{?RequestPath}`: 文字列、リクエストの `path` フィールドと同じ。  
- `{?ResponseCode}`: 文字列、LwM2M ステータスコード（例："2.01", "4.00" など）。  
- `{?ResponseMsg}`: 文字列、LwM2M レスポンスメッセージ（例："content", "bad_request"）。  
- `{?ResourcePath}`: 文字列、要求された完全なリソースパス（例：`31024/11/1`）。  
- `{?Value}`: 監視対象リソースの現在の値。
=======
変数：
- `{?ReqID}`: 整数、リクエストID。リクエストとの対応付けに使用。
- `{?MsgType}`: 文字列、リクエストコマンドと同じ MsgType。
- `{?RequestPath}`: 文字列、リクエストの `path` フィールドと同じ。
- `{?ResponseCode}`: 文字列、LwM2M ステータスコード（例："2.01", "4.00" など）。
- `{?ResponseMsg}`: 文字列、LwM2M レスポンスメッセージ（例："content", "bad_request"）。
- `{?ResourcePath}`: 文字列、要求された完全なリソースパス（例：`31024/11/1`）。
- `{?Value}`: 現在監視中のリソースの最新値。
>>>>>>> origin/release-5.10

#### Notify

<<<<<<< HEAD
"Notify" 操作は LwM2M クライアントから LwM2M サーバーへ、オブジェクトインスタンスやリソースの有効な監視中に送信されます。  
この操作はオブジェクトインスタンスやリソースの新しい値を含みます。
=======
「Notify」操作は、LwM2M クライアントから LwM2M サーバーへ、オブジェクトインスタンスまたはリソースの有効な監視中に送信されます。  
この操作にはオブジェクトインスタンスまたはリソースの新しい値が含まれます。
>>>>>>> origin/release-5.10

LwM2M クライアントからの通知は MQTT メッセージに変換されます。

通知メッセージの **トピック** は以下の通りです：
```json
{?mountpoint}{?translators.notify.topic}
```
変数：  
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。  
- `{?translators.notify.topic}` は LwM2M ゲートウェイ設定の `translators.notify.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.notify.topic` が `up/notify` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/notify` となります。

<<<<<<< HEAD

通知メッセージの **ペイロード** フォーマットは以下の通りです：
=======
通知メッセージの **ペイロード** 形式は以下の通りです：
>>>>>>> origin/release-5.10

```json
{
  "reqID": {?ReqID},
  "msgType": "notify",
  "seqNum": {?ObserveSeqNum},
  "data": {
    "code": {?ResponseCode},
    "codeMsg": {?ResponseMsg},
    "reqPath": {?RequestPath},
    "content": [
      {
        "path": {?ResourcePath},
        "value": {?Value}
      }
    ]
  }
}
```
<<<<<<< HEAD
変数：  
- `{?ReqID}`: 整数、リクエストID。リクエストとの対応付けに使用。  
- `{?ObserveSeqNum}`: 数値、CoAP メッセージの "Observe" オプションの値。  
- `{?ResponseCode}`: 文字列、LwM2M ステータスコード（例："2.01", "4.00" など）。  
- `{?ResponseMsg}`: 文字列、LwM2M レスポンスメッセージ（例："content", "bad_request"）。  
- `{?RequestPath}`: 文字列、リクエストの `path` フィールドと同じ。  
- `{?ResourcePath}`: 文字列、要求された完全なリソースパス（例：`31024/11/1`）。  
=======
変数：
- `{?ReqID}`: 整数、リクエストID。リクエストとの対応付けに使用。
- `{?ObserveSeqNum}`: 数値、CoAP メッセージの "Observe" オプションの値。
- `{?ResponseCode}`: 文字列、LwM2M ステータスコード（例："2.01", "4.00" など）。
- `{?ResponseMsg}`: 文字列、LwM2M レスポンスメッセージ（例："content", "bad_request"）。
- `{?RequestPath}`: 文字列、リクエストの `path` フィールドと同じ。
- `{?ResourcePath}`: 文字列、要求された完全なリソースパス（例：`31024/11/1`）。
>>>>>>> origin/release-5.10
- `{?Value}`: リソースの最新値。

## ユーザーインターフェース

- 詳細な設定オプション：[Gateway configuration - lwm2m (Opensource)](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) および [Gateway configuration - lwm2m (Enterprise)](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)  
- 詳細な HTTP API 説明：[REST API - Gateway](../admin/api.md)

## クライアントライブラリ

- [wakaama](https://github.com/eclipse/wakaama)
