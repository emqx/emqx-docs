# LwM2M ゲートウェイ

[LwM2M（Lightweight Machine-to-Machine）](https://lwm2m.openmobilealliance.org/) は、IoTデバイスおよびマシン間通信向けに設計されたプロトコルです。  
処理能力やメモリが限られたデバイスをサポートする軽量プロトコルです。

EMQX の **LwM2M ゲートウェイ** は、LwM2M クライアントを受け入れ、そのイベントやメッセージを MQTT のパブリッシュメッセージに変換します。

現状の実装には以下の制限があります：
- UDP/DTLS ベースのトランスポート。
- v1.0.2 のみサポート。v1.1.x および v1.2.x は未対応。
- LwM2M ブートストラップサービスは含まれていません。


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

LwM2M ゲートウェイは UDP および DTLS タイプのリスナーのみをサポートしています。  
設定可能なパラメータの完全なリストは次を参照してください：[Gateway Configuration - Listeners](../configuration/configuration-manual.html)


## 認証

LwM2M プロトコルはクライアントのエンドポイント名のみを提供し、ユーザー名やパスワードはありません。  
そのため、LwM2M ゲートウェイは [HTTP サーバー認証](../access-control/authn/http.md) のみをサポートしています。

例えば、REST API または設定ファイルで LwM2M ゲートウェイ用の HTTP 認証を作成する例：

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

LwM2M プロトコルのメッセージモデルは [リソースモデルと操作](https://technical.openmobilealliance.org/OMNA/LwM2M/LwM2MRegistry.html) に基づいています。  
これは MQTT プロトコルのパブリッシュ／サブスクライブモデルとは全く異なります。  
そのため、LwM2M ゲートウェイではこれらのメッセージモデルを互換させるためのメッセージフォーマットが必要です。

### クライアント登録インターフェース

#### Register（登録）

**Register** メッセージは LwM2M クライアントが LwM2M サーバーに自身を登録するために送信します。  
クライアントの情報や機能（エンドポイント名、ライフタイム、LwM2M バージョン、オブジェクト、オブジェクトインスタンスなど）を含みます。

Register メッセージはクライアントがサーバーとの通信を開始する最初のメッセージです。

**Register** メッセージは LwM2M ゲートウェイによって以下の MQTT メッセージに変換されます。

**Topic** の形式は以下の通りです：
```
{?mountpoint}{?translators.register.topic}
```

変数：
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.register.topic}` は LwM2M ゲートウェイ設定の `translators.register.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、`translators.register.topic` が `up/register` の場合、レスポンスメッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/register` となります。


**Payload** の形式は以下の通りです：
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
- `{?EndpointName}`：文字列、LwM2M クライアントのエンドポイント名。
- `{?Version}`：文字列、LwM2M クライアントのプロトコルバージョン。
- `{?LifeTime}`：数値、LwM2M クライアントが要求するライフタイム。
- `{?Binding}`：列挙型、クライアントがサーバーとの通信に対応するバインディングタイプ。以下のいずれか：
  * `"U"`：UDP
  * `"UQ"`：データキューイング付きUDP
- `{?ObjectList}`：配列、LwM2M クライアントがサポートするオブジェクトおよびオブジェクトインスタンスのリスト。

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

#### Update（更新）

**Update** メッセージは LwM2M クライアントが LwM2M サーバーに登録情報を更新するために送信します。  
Register メッセージに似ていますが、初回登録後に送信されます。  
Update メッセージには、IPアドレスの変更や LwM2M オブジェクトでモデル化されたデータの更新など、クライアントの機能や状態の変更情報が含まれます。  
Update メッセージはクライアントの登録期間を延長し、クライアントがまだ利用可能かつアクティブであることをサーバーに知らせる役割を持ちます。

Update メッセージの送信頻度は Register メッセージで指定されたライフタイム値によって決まります。

**Update** メッセージは LwM2M ゲートウェイによって以下の MQTT メッセージに変換されます。

**Topic** の形式は以下の通りです：
```
{?mountpoint}{?translators.update.topic}
```
変数：
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.update.topic}` は LwM2M ゲートウェイ設定の `translators.update.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、`translators.update.topic` が `up/update` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/update` となります。


**Payload** の形式は以下の通りです：
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

変数は Register メッセージと同じです。

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

このインターフェースは、LwM2M サーバーが登録済みの LwM2M クライアントからオブジェクトインスタンスやリソースにアクセスするために使用します。

"Create"、"Read"、"Write"、"Delete"、"Execute"、"Write-Attributes"、"Discover" の各操作を通じてアクセスを提供します。

リソースがサポートする操作は、オブジェクトテンプレートファイルを用いたオブジェクト定義で決まります。

LwM2M クライアントにコマンドを送信するには、EMQX に固定フォーマットの MQTT メッセージを送信します。  
これらのメッセージは LwM2M ゲートウェイによって正しい LwM2M メッセージに変換され、クライアントに送信されます。


コマンド要求の **Topic** は以下の通りです：
```
{?mountpoint}{?translators.command.topic}
```
変数：
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.command.topic}` は LwM2M ゲートウェイ設定の `translators.command.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、`translators.command.topic` が `dn/cmd` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/dn/cmd` となります。


コマンド要求の **Payload** 形式は以下の通りです：
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
変数：
- `{?ReqID}`：整数、リクエストID。レスポンスとの照合に使用。
- `{?MsgType}`：文字列、以下のいずれか：
  - `"read"`：LwM2M Read
  - `"discover"`：LwM2M Discover
  - `"write"`：LwM2M Write
  - `"write-attr"`：LwM2M Write Attributes
  - `"execute"`：LwM2M Execute
  - `"create"`：LwM2M Create
  - `"delete"`：LwM2M Delete
- `{?RequestData}`：JSON オブジェクト。`{?MsgType}` に依存し、次節で説明。

コマンド応答の **Topic** は以下の通りです：
```
{?mountpoint}{?translators.response.topic}
```
変数：
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.response.topic}` は LwM2M ゲートウェイ設定の `translators.response.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、`translators.response.topic` が `up/resp` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/resp` となります。


コマンド応答の **Payload** 形式は以下の通りです：
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
変数：
- `{?ReqID}`：整数、リクエストID。リクエストとの照合に使用。
- `{?MsgType}`：文字列、リクエストコマンドと同じ MsgType。
- `{?ResponseData}`：JSON オブジェクト、コマンド応答の内容。


#### Read（読み取り）

"Read" 操作はリソース、リソースインスタンスの配列、オブジェクトインスタンス、またはオブジェクトのすべてのオブジェクトインスタンスの値にアクセスするために使用します。

リクエストコマンドで **MsgType** が `"read"` の場合、**RequestData** の構造は以下の通りです：

```json
{
  "path": {?ResourcePath}
}
```
変数：
- `{?ResourcePath}`：文字列、要求されたリソースパス。以下の3つのシナリオがあります：
  * オブジェクトIDのみ、例：`/3`。該当オブジェクトのすべてのインスタンスとリソースの値を読み取る。
  * オブジェクトID/インスタンスID、例：`/3/0`。該当オブジェクトインスタンスのすべてのリソースの値を読み取る。
  * フルパス（`{ObjectID}/{InstanceID}/{ResourceID}`）、例：`/3/0/1`。特定のリソースの値を読み取る。

例として、Read コマンドの完全な MQTT ペイロードは以下のようになります：
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
変数：
- `{?ResourcePath}`：文字列、リクエストの `path` フィールドと同じ。
- `{?ResponseCode}`：文字列、LwM2M ステータスコード。例："2.01"、"4.00" など。
- `{?ResponseMsg}`：文字列、LwM2M 応答メッセージ。例："content"、"bad_request"。
- `{?ReadResponseData}`：JSON オブジェクト、リクエストの値の結果。リソース値の配列。

例として、Read レスポンスの完全な MQTT ペイロードは以下のようになります：
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

#### Discover（探索）

"Discover" 操作はオブジェクト、オブジェクトインスタンス、リソースに付随する LwM2M 属性を探索するために使用します。  
この操作は特定のオブジェクトインスタンスにどのリソースがインスタンス化されているかを探索するために使われます。  
返されるペイロードは、対象のオブジェクト、オブジェクトインスタンス、リソースごとにアプリケーション/リンク形式の CoRE リンク [RFC6690] のリストです。

リクエストコマンドで **MsgType** が `"discover"` の場合、**RequestData** の構造は以下の通りです：

```json
{
  "path": {?ResourcePath}
}
```

**Read** メッセージと同じ形式です：
* オブジェクトIDのみ、例：`/3`。該当オブジェクトのすべてのインスタンス、リソース、属性を探索。
* オブジェクトID/インスタンスID、例：`/3/0`。該当オブジェクトインスタンスのすべてのリソースと属性を探索。
* フルパス（`{ObjectID}/{InstanceID}/{ResourceID}`）、例：`/3/0/1`。特定のリソースのすべての属性を探索。

例として、Discover コマンドの完全な MQTT ペイロードは以下のようになります：
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

例として、Discover レスポンスの完全な MQTT ペイロードは以下のようになります：
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

"Write" 操作はリソースの値、リソースインスタンスの配列の値、またはオブジェクトインスタンスの複数リソースの値を変更するために使用します。

リクエストコマンドで **MsgType** が `"write"` の場合、**RequestData** は2つの構造が考えられます。

単一リソースへの書き込みの場合：
```json
{
    "path": {?ResourcePath},
    "type": {?ValueType},
    "value": {?Value}
}
```
- `{?ResourcePath}`：文字列、完全なリソースパス。例：`31024/11/1`。
- `{?ValueType}`：文字列、"Time"、"String"、"Integer"、"Float"、"Boolean"、"Opaque"、"Objlnk" のいずれか。
- `{?Value}`：リソースの値。`type` に依存。

例として、Write コマンドの完全な MQTT ペイロードは以下のようになります：
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
完全なパスは `{?BasePath}` と `"{ResourcePath}"` の連結です。

例として、Write コマンドの完全な MQTT ペイロードは以下のようになります：
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

LwM2M 1.0 では、"Write-Attributes" 操作は `<NOTIFICATION>` クラスの属性のみ変更可能です。

この操作では複数の属性を同時に変更できます。

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
変数：
- `{?PeriodMin}`：数値、通知の最小周期。
- `{?PeriodMax}`：数値、通知の最大周期。
- `{?GreaterThan}`：数値、リソース値がこの値を超えたときに通知。
- `{?LessThan}`：数値、リソース値がこの値を下回ったときに通知。
- `{?Step}`：数値、リソース値の変化がこの値を超えたときに通知。

#### Execute（実行）

"Execute" 操作は LwM2M サーバーが特定のアクションを開始するために使用し、個別のリソースに対してのみ実行可能です。

リクエストコマンドで **MsgType** が `"execute"` の場合、**RequestData** の構造は以下の通りです：
```json
{
  "path": {?ResourcePath},
  "args": {?Arguments}
}
```
変数：
- `{?Arguments}`：文字列、LwM2M Execute の引数。


#### Create（作成）

"Create" 操作は LwM2M サーバーが LwM2M クライアント内にオブジェクトインスタンスを作成するために使用します。  
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
変数：
- `{?ObjectID}`：整数、LwM2M オブジェクトID

#### Delete（削除）

"Delete" 操作は LwM2M サーバーが LwM2M クライアント内のオブジェクトインスタンスを削除するために使用します。

リクエストコマンドで **MsgType** が `"create"` の場合、**RequestData** の構造は以下の通りです：
```json
{
  "path": "{?ObjectID}/{?InstanceID}"
}
```
変数：
- `{?InstanceID}`：整数、LwM2M オブジェクトインスタンスID

### 情報報告インターフェース

このインターフェースは LwM2M サーバーが登録済みの LwM2M クライアントのリソースの変化を監視し、新しい値が利用可能になると通知を受け取るために使用します。  
この監視関係はオブジェクト、オブジェクトインスタンス、またはリソースに対して "Observe" 操作を送信することで開始されます。  
監視は "Cancel Observation" 操作が行われると終了します。

#### Observe と Cancel Observation（監視および監視キャンセル）

Observe および Cancel Observe リクエストの **Topic** は以下の通りです：
```
{?mountpoint}{?translators.command.topic}
```
変数：
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.command.topic}` は LwM2M ゲートウェイ設定の `translators.command.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、`translators.command.topic` が `dn/cmd` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/dn/cmd` となります。


Observe および Cancel Observe リクエストの **Payload** 形式は以下の通りです：
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
- `{?ReqID}`：整数、リクエストID。リクエスト内の {?ReqID}。
- `{?MsgType}`：文字列、以下のいずれか：
  * `"observe"`：LwM2M Observe
  * `"cancel-observe"`：LwM2M Cancel Observe
- `{?ResourcePath}`：文字列、監視または監視キャンセル対象の LwM2M リソース。完全なリソースパスのみサポート。例：`/3/0/1`。

例として、Observe コマンドの完全な MQTT ペイロードは以下のようになります：
```json
{
  "reqID": 10,
  "msgType": "observe",
  "data": {
    "path": "/31024/0/1"
  }
}
```

Observe 応答の **Topic** は以下の通りです：
```
{?mountpoint}{?translators.response.topic}
```
変数：
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.response.topic}` は LwM2M ゲートウェイ設定の `translators.response.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、`translators.response.topic` が `up/resp` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/resp` となります。


Observe 応答の **Payload** 形式は以下の通りです：
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
- `{?ReqID}`：整数、リクエストID。リクエストとの照合に使用。
- `{?MsgType}`：文字列、リクエストコマンドと同じ MsgType。
- `{?RequestPath}`：文字列、リクエストの `path` フィールドと同じ。
- `{?ResponseCode}`：文字列、LwM2M ステータスコード。例："2.01"、"4.00" など。
- `{?ResponseMsg}`：文字列、LwM2M 応答メッセージ。例："content"、"bad_request"。
- `{?ResourcePath}`：文字列、要求された完全なリソースパス。例：`31024/11/1`。
- `{?Value}`：監視中のリソースの現在の値。

#### Notify（通知）

"Notify" 操作は LwM2M クライアントから LwM2M サーバーに対して、オブジェクトインスタンスまたはリソースの有効な監視中に送信されます。  
この操作にはオブジェクトインスタンスまたはリソースの新しい値が含まれます。

LwM2M クライアントからの通知は MQTT メッセージに変換されます。

通知メッセージの **Topic** は以下の通りです：
```json
{?mountpoint}{?translators.notify.topic}
```
変数：
- `{?mountpoint}` は LwM2M ゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.notify.topic}` は LwM2M ゲートウェイ設定の `translators.notify.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定されており、`translators.notify.topic` が `up/notify` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/notify` となります。


通知メッセージの **Payload** 形式は以下の通りです：

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
- `{?ReqID}`：整数、リクエストID。リクエストとの照合に使用。
- `{?ObserveSeqNum}`：数値、CoAP メッセージの "Observe" オプションの値。
- `{?ResponseCode}`：文字列、LwM2M ステータスコード。例："2.01"、"4.00" など。
- `{?ResponseMsg}`：文字列、LwM2M 応答メッセージ。例："content"、"bad_request"。
- `{?RequestPath}`：文字列、リクエストの `path` フィールドと同じ。
- `{?ResourcePath}`：文字列、要求された完全なリソースパス。例：`31024/11/1`。
- `{?Value}`：リソースの最新値。

## ユーザーインターフェース

- 詳細な設定オプション：[Gateway configuration - lwm2m (Opensource)](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) および [Gateway configuration - lwm2m (Enterprise)](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)
- 詳細な HTTP API 説明：[REST API - Gateway](../admin/api.md)

## クライアントライブラリ

- [wakaama](https://github.com/eclipse/wakaama)
