# LwM2M ゲートウェイ

[LwM2M (Lightweight Machine-to-Machine)](https://lwm2m.openmobilealliance.org/) は、IoTデバイスおよびマシン間通信向けに設計されたプロトコルです。
処理能力やメモリが限られたデバイスをサポートする軽量なプロトコルです。

EMQXの**LwM2Mゲートウェイ**は、LwM2Mクライアントを受け入れ、そのイベントやメッセージをMQTTのパブリッシュメッセージに変換します。

現在の実装には以下の制限があります：
- UDP/DTLSベースのトランスポート
- v1.0.2のみサポート。v1.1.xおよびv1.2.xは未対応
- LwM2Mブートストラップサービスは含まれていません


## クイックスタート

EMQX 5.0では、LwM2Mゲートウェイはダッシュボードから設定および有効化できます。

REST APIや設定ファイルからも有効化可能です：

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
`base.hocon`でゲートウェイを設定する場合はノードごとに変更が必要ですが、ダッシュボードやREST API経由で設定するとクラスター全体に反映されます。
:::

LwM2MゲートウェイはUDPおよびDTLSタイプのリスナーのみをサポートしています。
設定可能なパラメータの完全な一覧は以下を参照してください：
[Gateway Configuration - Listeners](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/#V-gateway-S-gateway-lwm2m-S-lwm2m-listeners)


## 認証

LwM2MプロトコルはクライアントのEndpoint Nameのみを提供し、UsernameやPasswordはありません。
そのため、LwM2Mゲートウェイは[HTTPサーバー認証](../../guides/access-control/authn/http.md)のみをサポートしています。

例えば、REST APIや設定ファイルを使ってLwM2Mゲートウェイ用のHTTP認証を作成する例：

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

LwM2Mプロトコルのメッセージモデルは[リソースモデルと操作](https://technical.openmobilealliance.org/OMNA/LwM2M/LwM2MRegistry.html)に基づいており、
MQTTプロトコルのパブリッシュ／サブスクライブモデルとは全く異なります。
そのため、LwM2Mゲートウェイではこれらのメッセージモデルを互換性を持たせるためのメッセージフォーマットが必要です。

### クライアント登録インターフェース

#### Register（登録）

**Register**メッセージはLwM2MクライアントがLwM2Mサーバーに自身を登録するために送信します。
クライアントの情報や機能（エンドポイント名、ライフタイム、LwM2Mバージョン、オブジェクト、オブジェクトインスタンスなど）を含みます。

Registerメッセージはクライアントがサーバーとの通信を開始する最初のメッセージです。

**Register**メッセージはLwM2Mゲートウェイによって以下のMQTTメッセージに変換されます。

**Topic**のフォーマットは以下の通りです：
```
{?mountpoint}{?translators.register.topic}
```

変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値
- `{?translators.register.topic}` はLwM2Mゲートウェイ設定の `translators.register.topic` オプションの値

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、
`translators.register.topic` が `up/register` の場合、レスポンスメッセージのトピックは
`lwm2m/<実際のクライアントエンドポイント名>/up/register` になります。


**Payload**のフォーマットは以下の通りです：
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

変数：
- `{?EndpointName}`：文字列、LwM2Mクライアントのエンドポイント名
- `{?Version}`：文字列、LwM2Mクライアントのプロトコルバージョン
- `{?LifeTime}`：数値、LwM2Mクライアントが要求するライフタイム
- `{?Binding}`：列挙型、クライアントがサーバーとの通信に対応するバインディングタイプ。以下のいずれか：
  * `"U"`：UDP
  * `"UQ"`：データキューイング付きUDP
- `{?ObjectList}`：配列、LwM2Mクライアントがサポートするオブジェクトおよび利用可能なオブジェクトインスタンスのリスト

例として、Registerメッセージの完全なMQTTペイロードは以下のようになります：
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

#### Update（更新）

**Update**メッセージはLwM2MクライアントがLwM2Mサーバーに登録情報を更新するために送信します。
Registerメッセージに似ていますが、初回登録後に送信されます。
UpdateメッセージにはIPアドレスの変更やLwM2Mオブジェクトでモデル化されたデータの更新など、クライアントの状態や機能の変更情報が含まれます。
Updateメッセージはクライアントの登録期間を延長するため、クライアントがまだ利用可能かつアクティブであることをサーバーに知らせる手段です。

Updateメッセージの送信頻度はRegisterメッセージで指定されたライフタイム値によって決まります。

**Update**メッセージはLwM2Mゲートウェイによって以下のMQTTメッセージに変換されます。

**Topic**のフォーマットは以下の通りです：
```
{?mountpoint}{?translators.update.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値
- `{?translators.update.topic}` はLwM2Mゲートウェイ設定の `translators.update.topic` オプションの値

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、
`translators.update.topic` が `up/update` の場合、メッセージのトピックは
`lwm2m/<実際のクライアントエンドポイント名>/up/update` になります。


**Payload**のフォーマットは以下の通りです：
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

Registerメッセージと同じ変数です。

例として、Updateメッセージの完全なMQTTペイロードは以下のようになります：
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

このインターフェースはLwM2Mサーバーが登録済みLwM2Mクライアントのオブジェクトインスタンスやリソースにアクセスするために使用します。

"Create"、"Read"、"Write"、"Delete"、"Execute"、"Write-Attributes"、"Discover" の操作を通じてアクセスを提供します。

リソースがサポートする操作はオブジェクトテンプレートファイルを使ったオブジェクト定義で決まります。

LwM2Mクライアントにコマンドを送信するには、決まった形式のMQTTメッセージをEMQXに送信します。
これらのメッセージはLwM2Mゲートウェイによって正しいLwM2Mメッセージに変換され、クライアントに送信されます。

コマンドリクエストの**Topic**は以下の通りです：
```
{?mountpoint}{?translators.command.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値
- `{?translators.command.topic}` はLwM2Mゲートウェイ設定の `translators.command.topic` オプションの値

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、
`translators.command.topic` が `dn/cmd` の場合、メッセージのトピックは
`lwm2m/<実際のクライアントエンドポイント名>/dn/cmd` になります。


コマンドリクエストの**Payload**フォーマットは以下の通りです：
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
変数：
- `{?ReqID}`：整数、リクエストID。レスポンスとのマッチングに使用
- `{?MsgType}`：文字列、以下のいずれか：
  - `"read"`：LwM2M Read
  - `"discover"`：LwM2M Discover
  - `"write"`：LwM2M Write
  - `"write-attr"`：LwM2M Write Attributes
  - `"execute"`：LwM2M Execute
  - `"create"`：LwM2M Create
  - `"delete"`：LwM2M Delete
- `{?RequestData}`：JSONオブジェクト、`{?MsgType}`に応じて内容が異なり、後述します

コマンドレスポンスの**Topic**は以下の通りです：
```
{?mountpoint}{?translators.response.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値
- `{?translators.response.topic}` はLwM2Mゲートウェイ設定の `translators.response.topic` オプションの値

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、
`translators.response.topic` が `up/resp` の場合、メッセージのトピックは
`lwm2m/<実際のクライアントエンドポイント名>/up/resp` になります。


コマンドレスポンスの**Payload**フォーマットは以下の通りです：
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
変数：
- `{?ReqID}`：整数、リクエストID。リクエストとのマッチングに使用
- `{?MsgType}`：文字列、リクエストコマンドと同じMsgType
- `{?ResponseData}`：JSONオブジェクト、コマンドレスポンスの内容


#### Read（読み取り）

"Read"操作はリソース、リソースインスタンスの配列、オブジェクトインスタンス、またはオブジェクトのすべてのオブジェクトインスタンスの値にアクセスするために使用します。

リクエストコマンドで**MsgType**が `"read"` の場合、**RequestData**の構造は以下の通りです：

```json
{
  "path": {?ResourcePath}
}
```
変数：
- `{?ResourcePath}`：文字列、要求されたリソースパス。以下の3つのシナリオがあります：
  * オブジェクトIDのみ、例：`/3`。そのオブジェクトに属するすべてのインスタンスとリソースの値を読み取ることを意味します。
  * オブジェクトID/インスタンスID、例：`/3/0`。そのオブジェクトインスタンスに属するすべてのリソースの値を読み取ることを意味します。
  * フルパス（`{ObjectID}/{InstanceID}/{ResourceID}`）、例：`/3/0/1`。特定のリソースの値を読み取ることを意味します。

例として、Readコマンドの完全なMQTTペイロードは以下のようになります：
```json
{
  "reqID": 1,
  "msgType": "read",
  "data": {
    "path": "/3/0/1"
  }
}
```

レスポンスでは、**ResponseData**の構造は以下の通りです：
```json
{
  "reqPath": {?ResourcePath},
  "code": {?ResponseCode},
  "codeMsg": {?ResponseMsg},
  "content": {?ReadResponseData}
}
```
変数：
- `{?ResourcePath}`：文字列、リクエストの `path` フィールドと同じ
- `{?ResponseCode}`：文字列、LwM2Mステータスコード（例："2.01", "4.00"など）
- `{?ResponseMsg}`：文字列、LwM2Mレスポンスメッセージ（例："content", "bad_request"など）
- `{?ReadResponseData}`：JSONオブジェクト、リクエストに対する値の結果。リソース値の配列

例として、Readレスポンスの完全なMQTTペイロードは以下のようになります：
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

#### Discover（ディスカバー）

"Discover"操作はオブジェクト、オブジェクトインスタンス、リソースに付随するLwM2M属性を探索するために使用します。
この操作は特定のオブジェクトインスタンスにどのリソースがインスタンス化されているかを探索するために使われます。
返されるペイロードは対象のオブジェクト、オブジェクトインスタンス、リソースごとのアプリケーション／リンク形式のCoREリンクのリストです [RFC6690]。

リクエストコマンドで**MsgType**が `"discover"` の場合、**RequestData**の構造は以下の通りです：

```json
{
  "path": {?ResourcePath}
}
```

**Read**メッセージと同じフォーマットです：
* オブジェクトIDのみ、例：`/3`。そのオブジェクトに属するすべてのインスタンス、リソース、属性を探索
* オブジェクトID/インスタンスID、例：`/3/0`。そのオブジェクトインスタンスに属するすべてのリソースと属性を探索
* フルパス（`{ObjectID}/{InstanceID}/{ResourceID}`）、例：`/3/0/1`。特定のリソースのすべての属性を探索

例として、Discoverコマンドの完全なMQTTペイロードは以下のようになります：
```json
{
  "reqID": 2,
  "msgType": "discover",
  "data": {
    "path": "/3/0"
  }
}
```

レスポンスでは、**ResponseData**の構造は以下の通りです：
```json
{
  "reqPath": {?ResourcePath},
  "code": {?ResponseCode},
  "codeMsg": {?ResponseMsg},
  "content": {?DiscoverResponseData}
}
```
**Read**レスポンスと同じ変数ですが、`content`フィールドはリソースと属性の配列です。

例として、Discoverレスポンスの完全なMQTTペイロードは以下のようになります：
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

#### Write（書き込み）

"Write"操作はリソースの値、リソースインスタンスの配列の値、またはオブジェクトインスタンスの複数リソースの値を変更するために使用します。

リクエストコマンドで**MsgType**が `"write"` の場合、**RequestData**には2つの構造が考えられます。

単一リソースに値を書き込む場合：
```json
{
    "path": {?ResourcePath},
    "type": {?ValueType},
    "value": {?Value}
}
```
- `{?ResourcePath}`：文字列、完全なリソースパス（例：`31024/11/1`）
- `{?ValueType}`：文字列、"Time"、"String"、"Integer"、"Float"、"Boolean"、"Opaque"、"Objlnk" のいずれか
- `{?Value}`：リソースの値。`type`に依存

例として、Writeコマンドの完全なMQTTペイロードは以下のようになります：
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

複数リソースに書き込む場合：
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
完全なパスは `{?BasePath}` と `"{ResourcePath}` の連結です。

例として、Writeコマンドの完全なMQTTペイロードは以下のようになります：
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

#### Write-Attributes（属性書き込み）

LwM2M 1.0では、"Write-Attributes"操作は `<NOTIFICATION>` クラスの属性のみ変更可能です。

この操作では複数の属性を同時に変更できます。

リクエストコマンドで**MsgType**が `"write-attr"` の場合、**RequestData**の構造は以下の通りです：

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
変数：
- `{?PeriodMin}`：数値、通知の最小周期
- `{?PeriodMax}`：数値、通知の最大周期
- `{?GreaterThan}`：数値、リソース値がこの値より大きい場合に通知
- `{?LessThan}`：数値、リソース値がこの値より小さい場合に通知
- `{?Step}`：数値、リソース値の変化がこの値を超えた場合に通知

#### Execute（実行）

"Execute"操作はLwM2Mサーバーが特定のアクションを起動するために使用し、個別のリソースに対してのみ実行可能です。

リクエストコマンドで**MsgType**が `"execute"` の場合、**RequestData**の構造は以下の通りです：
```json
{
  "path": {?ResourcePath},
  "args": {?Arguments}
}
```
変数：
- `{?Arguments}`：文字列、LwM2MのExecute引数


#### Create（作成）

"Create"操作はLwM2MサーバーがLwM2Mクライアント内にオブジェクトインスタンスを作成するために使用します。
"Create"操作はオブジェクトをターゲットにしなければなりません。

リクエストコマンドで**MsgType**が `"create"` の場合、**RequestData**の構造は以下の通りです：

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
変数：
- `{?ObjectID}`：整数、LwM2MオブジェクトID

#### Delete（削除）

"Delete"操作はLwM2MサーバーがLwM2Mクライアント内のオブジェクトインスタンスを削除するために使用します。

リクエストコマンドで**MsgType**が `"create"` の場合、**RequestData**の構造は以下の通りです：
```json
{
  "path": "{?ObjectID}/{?InstanceID}"
}
```
変数：
- `{?InstanceID}`：整数、LwM2MオブジェクトインスタンスID

### 情報報告インターフェース

このインターフェースはLwM2Mサーバーが登録済みLwM2Mクライアントのリソースの変化を監視し、新しい値が利用可能になった際に通知を受け取るために使用します。
この監視関係はオブジェクト、オブジェクトインスタンス、リソースに対して"Observe"操作をLwM2Mクライアントに送信することで開始されます。
監視は"Cancel Observation"操作が行われると終了します。

#### Observe（監視）および Cancel Observation（監視キャンセル）

ObserveおよびCancel Observeリクエストの**Topic**は以下の通りです：
```
{?mountpoint}{?translators.command.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値
- `{?translators.command.topic}` はLwM2Mゲートウェイ設定の `translators.command.topic` オプションの値

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、
`translators.command.topic` が `dn/cmd` の場合、メッセージのトピックは
`lwm2m/<実際のクライアントエンドポイント名>/dn/cmd` になります。


ObserveおよびCancel Observeリクエストの**Payload**フォーマットは以下の通りです：
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
変数：
- `{?ReqID}`：整数、リクエストID。リクエストのID
- `{?MsgType}`：文字列、以下のいずれか：
  * `"observe"`：LwM2M Observe
  * `"cancel-observe"`：LwM2M Cancel Observe
- `{?ResourcePath}`：文字列、監視または監視キャンセルするLwM2Mリソース。完全なリソースパスのみサポート（例：`/3/0/1`）

例として、Observeコマンドの完全なMQTTペイロードは以下のようになります：
```json
{
  "reqID": 10,
  "msgType": "observe",
  "data": {
    "path": "/31024/0/1"
  }
}
```

Observeレスポンスの**Topic**は以下の通りです：
```
{?mountpoint}{?translators.response.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値
- `{?translators.response.topic}` はLwM2Mゲートウェイ設定の `translators.response.topic` オプションの値

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、
`translators.response.topic` が `up/resp` の場合、メッセージのトピックは
`lwm2m/<実際のクライアントエンドポイント名>/up/resp` になります。


Observeレスポンスの**Payload**フォーマットは以下の通りです：
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
変数：
- `{?ReqID}`：整数、リクエストID。リクエストとのマッチングに使用
- `{?MsgType}`：文字列、リクエストコマンドと同じMsgType
- `{?RequestPath}`：文字列、リクエストの `path` フィールドと同じ
- `{?ResponseCode}`：文字列、LwM2Mステータスコード（例："2.01", "4.00"など）
- `{?ResponseMsg}`：文字列、LwM2Mレスポンスメッセージ（例："content", "bad_request"など）
- `{?ResourcePath}`：文字列、要求された完全なリソースパス（例：`31024/11/1`）
- `{?Value}`：監視対象リソースの現在の値

#### Notify（通知）

"Notify"操作はLwM2MクライアントからLwM2Mサーバーへ、オブジェクトインスタンスやリソースの有効な監視中に送信されます。
この操作にはオブジェクトインスタンスやリソースの新しい値が含まれます。

LwM2Mクライアントからの通知はMQTTメッセージに変換されます。

通知メッセージの**Topic**は以下の通りです：
```json
{?mountpoint}{?translators.notify.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値
- `{?translators.notify.topic}` はLwM2Mゲートウェイ設定の `translators.notify.topic` オプションの値

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、
`translators.notify.topic` が `up/notify` の場合、メッセージのトピックは
`lwm2m/<実際のクライアントエンドポイント名>/up/notify` になります。


通知メッセージの**Payload**フォーマットは以下の通りです：

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
変数：
- `{?ReqID}`：整数、リクエストID。リクエストとのマッチングに使用
- `{?ObserveSeqNum}`：数値、CoAPメッセージの"Observe"オプションの値
- `{?ResponseCode}`：文字列、LwM2Mステータスコード（例："2.01", "4.00"など）
- `{?ResponseMsg}`：文字列、LwM2Mレスポンスメッセージ（例："content", "bad_request"など）
- `{?RequestPath}`：文字列、リクエストの `path` フィールドと同じ
- `{?ResourcePath}`：文字列、要求された完全なリソースパス（例：`31024/11/1`）
- `{?Value}`：リソースの最新値

## ユーザーインターフェース

- 詳細な設定オプション：[Gateway configuration - lwm2m (Opensource)](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) および [Gateway configuration - lwm2m (Enterprise)](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)
- 詳細なHTTP API説明：[REST API - Gateway](../../guides/api.md)

## クライアントライブラリ

- [wakaama](https://github.com/eclipse/wakaama)
