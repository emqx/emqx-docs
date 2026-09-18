# LwM2M ゲートウェイ

[LwM2M (Lightweight Machine-to-Machine)](https://lwm2m.openmobilealliance.org/) は、IoTデバイスおよびマシン間通信向けに設計されたプロトコルです。  
処理能力やメモリが限られたデバイスをサポートする軽量プロトコルです。

EMQXの**LwM2Mゲートウェイ**は、LwM2Mクライアントを受け入れ、彼らのイベントやメッセージをMQTTのパブリッシュメッセージに変換します。

現在の実装には以下の制限があります：
- UDP/DTLSベースのトランスポート。
- v1.0.2のみサポート。v1.1.xおよびv1.2.xはまだサポートされていません。
- LwM2Mブートストラップサービスは含まれていません。

## クイックスタート

EMQX 5.0では、LwM2Mゲートウェイはダッシュボードから設定および有効化できます。

また、REST APIや設定ファイルからも有効化可能です：

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
設定可能なパラメータの完全なリストは以下を参照してください：[Gateway Configuration - Listeners](../configuration/configuration-manual.html)

## 認証

LwM2Mプロトコルはクライアントのエンドポイント名のみを提供し、ユーザー名やパスワードはありません。  
そのため、LwM2Mゲートウェイは[HTTPサーバー認証](../../guides/access-control/authn/http.md)のみをサポートしています。

例えば、REST APIや設定ファイル経由でLwM2MゲートウェイのHTTP認証を作成する例：

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
そのため、LwM2Mゲートウェイではこれらのメッセージモデルを互換させるためのメッセージフォーマットが必要です。

### クライアント登録インターフェース

#### Register

**Register**メッセージはLwM2MクライアントがLwM2Mサーバーに自身を登録するために送信します。  
クライアントの情報やエンドポイント名、ライフタイム、LwM2Mバージョン、オブジェクトやオブジェクトインスタンスなどの能力を含みます。

Registerメッセージはクライアントがサーバーとの通信を開始する最初のメッセージです。

**Register**メッセージはLwM2Mゲートウェイによって以下のMQTTメッセージに変換されます。

**トピック**のフォーマットは：
```
{?mountpoint}{?translators.register.topic}
```

変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.register.topic}` はLwM2Mゲートウェイ設定の `translators.register.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、  
`translators.register.topic` が `up/register` の場合、レスポンスメッセージのトピックは  
`lwm2m/<実際のクライアントエンドポイント名>/up/register` となります。

**ペイロード**のフォーマットは：
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
- `{?EndpointName}`: 文字列、LwM2Mクライアントのエンドポイント名。
- `{?Version}`: 文字列、LwM2Mクライアントのプロトコルバージョン。
- `{?LifeTime}`: 数値、LwM2Mクライアントが要求するライフタイム。
- `{?Binding}`: 列挙型、クライアントがサーバーとの通信に対応するバインディングタイプ。以下のいずれか：
  * `"U"`: UDP
  * `"UQ"`: データキューイング付きUDP
- `{?ObjectList}`: 配列、LwM2Mクライアントがサポートするオブジェクトおよびオブジェクトインスタンスのリスト。

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

#### Update

**Update**メッセージはLwM2MクライアントがLwM2Mサーバーに登録情報を更新するために送信します。  
初回登録後に送信され、クライアントの能力や状態の変更（IPアドレスの変更やLwM2Mオブジェクトによるデータの更新など）を含みます。  
Updateメッセージはクライアントの登録期間を延長し、クライアントがまだ利用可能かつアクティブであることをサーバーに知らせる手段です。

Updateメッセージの送信頻度はRegisterメッセージで指定されたライフタイム値によって決まります。

**Update**メッセージはLwM2Mゲートウェイによって以下のMQTTメッセージに変換されます。

**トピック**のフォーマットは：
```
{?mountpoint}{?translators.update.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.update.topic}` はLwM2Mゲートウェイ設定の `translators.update.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、  
`translators.update.topic` が `up/update` の場合、メッセージのトピックは  
`lwm2m/<実際のクライアントエンドポイント名>/up/update` となります。

**ペイロード**のフォーマットは：
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

変数はRegisterメッセージと同様です。

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

### LwM2Mデバイス管理およびサービス有効化インターフェース

このインターフェースはLwM2Mサーバーが登録済みLwM2Mクライアントのオブジェクトインスタンスやリソースにアクセスするために使用します。

"Create"、"Read"、"Write"、"Delete"、"Execute"、"Write-Attributes"、"Discover"の各操作を通じてアクセスを提供します。

リソースがサポートする操作はオブジェクトテンプレートファイルを用いたオブジェクト定義で決まります。

LwM2Mクライアントにコマンドを送信するには、EMQXに決まったフォーマットのMQTTメッセージを送信します。  
これらのメッセージはLwM2Mゲートウェイによって正しいLwM2Mメッセージに変換され、クライアントに送信されます。

コマンドリクエストの**トピック**は：
```
{?mountpoint}{?translators.command.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.command.topic}` はLwM2Mゲートウェイ設定の `translators.command.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、  
`translators.command.topic` が `dn/cmd` の場合、メッセージのトピックは  
`lwm2m/<実際のクライアントエンドポイント名>/dn/cmd` となります。

コマンドリクエストの**ペイロード**フォーマットは：
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
変数：
- `{?ReqID}`: 整数、リクエストID。レスポンスとリクエストを対応付けるために使用。
- `{?MsgType}`: 文字列、以下のいずれか：
  - `"read"`: LwM2M Read
  - `"discover"`: LwM2M Discover
  - `"write"`: LwM2M Write
  - `"write-attr"`: LwM2M Write Attributes
  - `"execute"`: LwM2M Execute
  - `"create"`: LwM2M Create
  - `"delete"`: LwM2M Delete
- `{?RequestData}`: JSONオブジェクト、内容は`{?MsgType}`に依存し、次節で説明します。

コマンドレスポンスの**トピック**は：
```
{?mountpoint}{?translators.response.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.response.topic}` はLwM2Mゲートウェイ設定の `translators.response.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、  
`translators.response.topic` が `up/resp` の場合、メッセージのトピックは  
`lwm2m/<実際のクライアントエンドポイント名>/up/resp` となります。

コマンドレスポンスの**ペイロード**フォーマットは：
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
変数：
- `{?ReqID}`: 整数、リクエストID。リクエストとの対応付けに使用。
- `{?MsgType}`: 文字列、リクエストコマンドと同じMsgType。
- `{?ResponseData}`: JSONオブジェクト、コマンドレスポンスの内容。

#### Read

"Read"操作はリソースの値、リソースインスタンスの配列、オブジェクトインスタンス、またはオブジェクトのすべてのオブジェクトインスタンスの値にアクセスするために使用します。

リクエストコマンドで**MsgType**が `"read"` の場合、**RequestData**の構造は以下の通りです：

```json
{
  "path": {?ResourcePath}
}
```
変数：
- `{?ResourcePath}`: 文字列、要求されたリソースパス。以下の3つのシナリオがあります：
  * オブジェクトIDのみ、例：`/3`。該当オブジェクトのすべてのインスタンスとリソースの値を読み取る。
  * オブジェクトID/インスタンスID、例：`/3/0`。該当オブジェクトインスタンスのすべてのリソースの値を読み取る。
  * フルパス、`{ObjectID}/{InstanceID}/{ResourceID}`、例：`/3/0/1`。特定リソースの値を読み取る。

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
- `{?ResourcePath}`: 文字列、リクエストの`path`フィールドと同じ。
- `{?ResponseCode}`: 文字列、LwM2Mステータスコード、例："2.01"、"4.00"など。
- `{?ResponseMsg}`: 文字列、LwM2Mレスポンスメッセージ、例："content"、"bad_request"。
- `{?ReadResponseData}`: JSONオブジェクト、リクエストの結果値。リソース値の配列。

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

#### Discover

"Discover"操作はオブジェクト、オブジェクトインスタンス、リソースに付随するLwM2M属性を探索するために使用します。  
この操作は特定のオブジェクトインスタンスにどのリソースがインスタンス化されているかを発見するために使われます。  
返されるペイロードは対象のオブジェクト、オブジェクトインスタンス、リソースごとのアプリケーション/リンク形式のCoREリンク[RFC6690]のリストです。

リクエストコマンドで**MsgType**が `"discover"` の場合、**RequestData**の構造は以下の通りです：

```json
{
  "path": {?ResourcePath}
}
```

**Read**メッセージと同じフォーマットです：
* オブジェクトIDのみ、例：`/3`。該当オブジェクトのすべてのインスタンス、リソース、属性を探索。
* オブジェクトID/インスタンスID、例：`/3/0`。該当オブジェクトインスタンスのすべてのリソース、属性を探索。
* フルパス、`{ObjectID}/{InstanceID}/{ResourceID}`、例：`/3/0/1`。特定リソースのすべての属性を探索。

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
**Read**レスポンスと同じ変数ですが、`content`フィールドが異なり、  
`{?DiscoverResponseData}`はリソースと属性の配列です。

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

#### Write

"Write"操作はリソースの値、リソースインスタンスの配列の値、またはオブジェクトインスタンスの複数リソースの値を変更するために使用します。

リクエストコマンドで**MsgType**が `"write"` の場合、**RequestData**は2つの構造が考えられます。

単一リソースに値を書き込む場合：
```json
{
    "path": {?ResourcePath},
    "type": {?ValueType},
    "value": {?Value}
}
```
- `{?ResourcePath}`: 文字列、要求された完全なリソースパス、例：`31024/11/1`。
- `{?ValueType}`: 文字列、"Time"、"String"、"Integer"、"Float"、"Boolean"、"Opaque"、"Objlnk"のいずれか。
- `{?Value}`: リソースの値。`type`に依存。

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
完全なパスは `{?BasePath}` と `"{ResourcePath}"` の連結です。

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

#### Write-Attributes

LwM2M 1.0では、"Write-Attributes"操作は `<NOTIFICATION>` クラスの属性のみ変更可能です。

この操作は複数の属性を同時に変更できます。

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
- `{?PeriodMin}`: 数値、通知の最小期間。
- `{?PeriodMax}`: 数値、通知の最大期間。
- `{?GreaterThan}`: 数値、リソース値がこの値を超えた場合に通知。
- `{?LessThan}`: 数値、リソース値がこの値を下回った場合に通知。
- `{?Step}`: 数値、リソース値の変化がこの値を超えた場合に通知。

#### Execute

"Execute"操作はLwM2Mサーバーがアクションを開始するために使用し、個別のリソースに対してのみ実行可能です。

リクエストコマンドで**MsgType**が `"execute"` の場合、**RequestData**の構造は以下の通りです：
```json
{
  "path": {?ResourcePath},
  "args": {?Arguments}
}
```
変数：
- `{?Arguments}`: 文字列、LwM2M Executeの引数。

#### Create

"Create"操作はLwM2MサーバーがLwM2Mクライアント内にオブジェクトインスタンスを作成するために使用します。  
"Create"操作はオブジェクトを対象としなければなりません。

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
- `{?ObjectID}`: 整数、LwM2MオブジェクトID。

#### Delete

"Delete"操作はLwM2MサーバーがLwM2Mクライアント内のオブジェクトインスタンスを削除するために使用します。

リクエストコマンドで**MsgType**が `"delete"` の場合、**RequestData**の構造は以下の通りです：
```json
{
  "path": "{?ObjectID}/{?InstanceID}"
}
```
変数：
- `{?InstanceID}`: 整数、LwM2MオブジェクトインスタンスID。

### 情報報告インターフェース

このインターフェースはLwM2Mサーバーが登録済みLwM2Mクライアントのリソースの変化を監視し、新しい値が利用可能になった際に通知を受け取るために使用します。  
この監視関係はLwM2Mクライアントに対して"Observe"操作を送信することで開始されます。  
監視は"Cancel Observation"操作が実行されると終了します。

#### ObserveおよびCancel Observation

Observeおよびキャンセルリクエストの**トピック**は：
```
{?mountpoint}{?translators.command.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.command.topic}` はLwM2Mゲートウェイ設定の `translators.command.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、  
`translators.command.topic` が `dn/cmd` の場合、メッセージのトピックは  
`lwm2m/<実際のクライアントエンドポイント名>/dn/cmd` となります。

Observeおよびキャンセルリクエストの**ペイロード**フォーマットは：
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
- `{?ReqID}`: 整数、リクエストID。リクエストのID。
- `{?MsgType}`: 文字列、以下のいずれか：
  * `"observe"`: LwM2M Observe
  * `"cancel-observe"`: LwM2M Cancel Observe
- `{?ResourcePath}`: 文字列、監視または監視解除するLwM2Mリソース。完全なリソースパスのみサポート、例：`/3/0/1`。

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

Observeレスポンスの**トピック**は：
```
{?mountpoint}{?translators.response.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.response.topic}` はLwM2Mゲートウェイ設定の `translators.response.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、  
`translators.response.topic` が `up/resp` の場合、メッセージのトピックは  
`lwm2m/<実際のクライアントエンドポイント名>/up/resp` となります。

Observeレスポンスの**ペイロード**フォーマットは：
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
- `{?ReqID}`: 整数、リクエストID。リクエストとの対応付けに使用。
- `{?MsgType}`: 文字列、リクエストコマンドと同じMsgType。
- `{?RequestPath}`: 文字列、リクエストの`path`フィールドと同じ。
- `{?ResponseCode}`: 文字列、LwM2Mステータスコード、例："2.01"、"4.00"など。
- `{?ResponseMsg}`: 文字列、LwM2Mレスポンスメッセージ、例："content"、"bad_request"。
- `{?ResourcePath}`: 文字列、要求された完全なリソースパス、例：`31024/11/1`。
- `{?Value}`: 監視対象リソースの現在の値。

#### Notify

"Notify"操作はLwM2MクライアントからLwM2Mサーバーへ、オブジェクトインスタンスまたはリソースの有効な監視中に送信されます。  
この操作はオブジェクトインスタンスまたはリソースの新しい値を含みます。

LwM2Mクライアントからの通知はMQTTメッセージに変換されます。

通知メッセージの**トピック**は：
```json
{?mountpoint}{?translators.notify.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.notify.topic}` はLwM2Mゲートウェイ設定の `translators.notify.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、  
`translators.notify.topic` が `up/notify` の場合、メッセージのトピックは  
`lwm2m/<実際のクライアントエンドポイント名>/up/notify` となります。

通知メッセージの**ペイロード**フォーマットは：

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
- `{?ReqID}`: 整数、リクエストID。リクエストとの対応付けに使用。
- `{?ObserveSeqNum}`: 数値、CoAPメッセージの"Observe"オプションの値。
- `{?ResponseCode}`: 文字列、LwM2Mステータスコード、例："2.01"、"4.00"など。
- `{?ResponseMsg}`: 文字列、LwM2Mレスポンスメッセージ、例："content"、"bad_request"。
- `{?RequestPath}`: 文字列、リクエストの`path`フィールドと同じ。
- `{?ResourcePath}`: 文字列、要求された完全なリソースパス、例：`31024/11/1`。
- `{?Value}`: リソースの最新値。

## ユーザーインターフェース

- 詳細な設定オプション：[Gateway configuration - lwm2m (Opensource)](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) および [Gateway configuration - lwm2m (Enterprise)](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)
- 詳細なHTTP API説明：[REST API - Gateway](../../guides/api.md)

## クライアントライブラリ

- [wakaama](https://github.com/eclipse/wakaama)
