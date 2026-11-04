# LwM2M ゲートウェイ

[LwM2M (Lightweight Machine-to-Machine)](https://lwm2m.openmobilealliance.org/) は、IoTデバイスおよびマシン間通信向けに設計されたプロトコルです。処理能力やメモリが限られたデバイスをサポートする軽量プロトコルです。

EMQXの**LwM2Mゲートウェイ**は、LwM2Mクライアントを受け入れ、そのイベントやメッセージをMQTTのパブリッシュメッセージに変換します。

現時点の実装には以下の制限があります：
- UDP/DTLSベースのトランスポート。
- v1.0.2のみサポート。v1.1.xおよびv1.2.xは未対応。
- LwM2Mブートストラップサービスは含まれていません。

## クイックスタート

EMQX 5.0では、LwM2Mゲートウェイはダッシュボードから設定および有効化できます。

REST APIや設定ファイルからも有効化可能です：

:::: tabs type:card

::: tab REST API

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateways/lwm2m' \
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
`base.hocon`でゲートウェイを設定する場合はノードごとに変更が必要ですが、ダッシュボードやREST APIで設定するとクラスター全体に反映されます。
:::

LwM2MゲートウェイはUDPおよびDTLSタイプのリスナーのみをサポートしています。設定可能なパラメータの完全な一覧は以下を参照してください：[Gateway Configuration - Listeners](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/#V-gateway-S-gateway-lwm2m-S-lwm2m-listeners)

## 認証

LwM2Mプロトコルはクライアントのエンドポイント名のみを提供し、ユーザー名やパスワードはありません。
そのため、LwM2Mゲートウェイは[HTTPサーバ認証](../../guides/access-control/authn/http.md)のみをサポートしています。

例えば、REST APIや設定ファイルでLwM2MゲートウェイのHTTP認証を作成する例：

:::: tabs type:card

::: tab REST API

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateways/lwm2m/authentication' \
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

LwM2Mプロトコルのメッセージモデルは[リソースモデルと操作](https://technical.openmobilealliance.org/OMNA/LwM2M/LwM2MRegistry.html)に基づいており、MQTTのパブリッシュ/サブスクライブモデルとは全く異なります。そのため、LwM2Mゲートウェイではこれらのメッセージモデルを互換させるためのメッセージフォーマットを作成しています。

### クライアント登録インターフェース

#### Register（登録）

**Register**メッセージはLwM2MクライアントがLwM2Mサーバに自身を登録するために送信します。クライアントの情報や機能（エンドポイント名、ライフタイム、LwM2Mバージョン、オブジェクト、オブジェクトインスタンスなど）が含まれます。

Registerメッセージはクライアントがサーバとの通信を開始する最初のメッセージです。

**Register**メッセージはLwM2Mゲートウェイによって以下のMQTTメッセージに変換されます。

**トピック**の形式は以下の通りです：
```
{?mountpoint}{?translators.register.topic}
```

変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.register.topic}` はLwM2Mゲートウェイ設定の `translators.register.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.register.topic` が `up/register` の場合、レスポンスメッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/register` となります。

**ペイロード**の形式は以下の通りです：
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
- `{?LifeTime}`: 数値、LwM2Mクライアントが要求したライフタイム。
- `{?Binding}`: 列挙型、クライアントがサーバと通信するためにサポートするバインディングタイプ。以下のいずれか：
  * `"U"`: UDP
  * `"UQ"`: データキューイング付きUDP
- `{?ObjectList}`: 配列、LwM2Mクライアントがサポートするオブジェクトおよび利用可能なオブジェクトインスタンスのリスト。

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

**Update**メッセージはLwM2MクライアントがLwM2Mサーバに登録情報を更新するために送信します。Registerメッセージに似ていますが、初期登録後に送信されます。Updateメッセージはクライアントの機能や状態の変更（IPアドレスの変更やLwM2Mオブジェクトでモデル化されたデータの更新など）を含みます。Updateメッセージはクライアントの登録期間を延長し、クライアントがまだ利用可能でアクティブであることをサーバに知らせる手段です。

Updateメッセージの送信頻度はRegisterメッセージで指定されたライフタイム値によって決まります。

**Update**メッセージはLwM2Mゲートウェイによって以下のMQTTメッセージに変換されます。

**トピック**の形式は以下の通りです：
```
{?mountpoint}{?translators.update.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.update.topic}` はLwM2Mゲートウェイ設定の `translators.update.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.update.topic` が `up/update` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/update` となります。

**ペイロード**の形式は以下の通りです：
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

変数はRegisterメッセージと同じです。

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

このインターフェースはLwM2Mサーバが登録済みLwM2Mクライアントのオブジェクトインスタンスおよびリソースにアクセスするために使用します。

「作成(Create)」「読み取り(Read)」「書き込み(Write)」「削除(Delete)」「実行(Execute)」「属性書き込み(Write-Attributes)」「探索(Discover)」の操作を通じてアクセスを提供します。

リソースがサポートする操作はオブジェクトテンプレートファイルで定義されています。

LwM2Mクライアントにコマンドを送信するには、決まった形式のMQTTメッセージをEMQXに送信します。これらのメッセージはLwM2Mゲートウェイによって正しいLwM2Mメッセージに変換され、クライアントに送信されます。

**コマンド要求のトピック**は以下の通りです：
```
{?mountpoint}{?translators.command.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.command.topic}` はLwM2Mゲートウェイ設定の `translators.command.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.command.topic` が `dn/cmd` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/dn/cmd` となります。

**コマンド要求のペイロード**の形式は以下の通りです：
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
変数：
- `{?ReqID}`: 整数、リクエストID。レスポンスとの紐付けに使用。
- `{?MsgType}`: 文字列、以下のいずれか：
  - `"read"`: LwM2M読み取り
  - `"discover"`: LwM2M探索
  - `"write"`: LwM2M書き込み
  - `"write-attr"`: LwM2M属性書き込み
  - `"execute"`: LwM2M実行
  - `"create"`: LwM2M作成
  - `"delete"`: LwM2M削除
- `{?RequestData}`: JSONオブジェクト、`{?MsgType}`に応じた内容（後述）

**コマンド応答のトピック**は以下の通りです：
```
{?mountpoint}{?translators.response.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.response.topic}` はLwM2Mゲートウェイ設定の `translators.response.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.response.topic` が `up/resp` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/resp` となります。

**コマンド応答のペイロード**の形式は以下の通りです：
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
変数：
- `{?ReqID}`: 整数、リクエストID。リクエストとの紐付けに使用。
- `{?MsgType}`: 文字列、リクエストコマンドと同じMsgType。
- `{?ResponseData}`: JSONオブジェクト、コマンド応答の内容。

#### 保留中のダウンリンクコマンドキュー制限 <Badge type="tip" text="6.3.3+" />

各LwM2Mクライアントセッションは最大1,000件の保留中ダウンリンクコマンドを保持できます。この制限は固定で設定変更できません。EMQXは受け入れたコマンドを先入れ先出し（FIFO）でキューイングします。すでに送信中のコマンドは保留制限にカウントされません。

保留キューが1,000件に達すると、新規コマンドは拒否され、`translators.response.topic`にコード`4.29`のレスポンスがパブリッシュされます。また、`delivery.dropped`メトリクスがインクリメントされます。期限切れのコマンドがキューから削除されると、その分保留制限のカウントから除外されます。

この制限は保留中コマンド数を制御し、ペイロードの合計サイズではありません。Block-Wise容量が一時的に不足しても、受け入れたコマンドは保留キューに残ります。容量が利用可能になると、FIFO順に送信が再開されます。

セッション統計の`mqueue_len`は保留中コマンド数を、`mqueue_max`は固定制限の`1000`を報告します。

#### Read（読み取り）

"Read"操作はリソース、リソースインスタンスの配列、オブジェクトインスタンス、またはオブジェクトのすべてのオブジェクトインスタンスの値にアクセスするために使用します。

リクエストコマンドで**MsgType**が`"read"`の場合、**RequestData**の構造は以下の通りです：

```json
{
  "path": {?ResourcePath}
}
```
変数：
- `{?ResourcePath}`: 文字列、要求されたリソースパス。以下の3つのシナリオがあります：
  * オブジェクトIDのみ、例：`/3`。該当オブジェクトの全インスタンスとリソースの値を読み取る。
  * オブジェクトID/インスタンスID、例：`/3/0`。該当オブジェクトインスタンスの全リソースの値を読み取る。
  * フルパス（`{ObjectID}/{InstanceID}/{ResourceID}`）、例：`/3/0/1`。特定リソースの値を読み取る。

例として、Readコマンドの完全なMQTTペイロードは以下の通りです：
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
- `{?ResponseCode}`: 文字列、LwM2Mステータスコード（例："2.01", "4.00"など）。
- `{?ResponseMsg}`: 文字列、LwM2Mレスポンスメッセージ（例："content", "bad_request"など）。
- `{?ReadResponseData}`: JSONオブジェクト、リクエストに対する値の結果。リソース値の配列。

例として、Readレスポンスの完全なMQTTペイロードは以下の通りです：
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

"Discover"操作はオブジェクト、オブジェクトインスタンス、リソースに付随するLwM2M属性を探索するために使用します。この操作は特定のオブジェクトインスタンスにインスタンス化されているリソースを探索するために利用されます。返されるペイロードは対象のオブジェクト、オブジェクトインスタンス、リソースごとのアプリケーション/リンク形式のCoREリンク[RFC6690]のリストです。

リクエストコマンドで**MsgType**が`"discover"`の場合、**RequestData**の構造は以下の通りです：

```json
{
  "path": {?ResourcePath}
}
```

フォーマットは**Read**メッセージと同じです：
* オブジェクトIDのみ、例：`/3`。該当オブジェクトの全インスタンス、リソース、属性を探索。
* オブジェクトID/インスタンスID、例：`/3/0`。該当オブジェクトインスタンスの全リソース、属性を探索。
* フルパス（`{ObjectID}/{InstanceID}/{ResourceID}`）、例：`/3/0/1`。特定リソースの全属性を探索。

例として、Discoverコマンドの完全なMQTTペイロードは以下の通りです：
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

例として、Discoverレスポンスの完全なMQTTペイロードは以下の通りです：
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

リクエストコマンドで**MsgType**が`"write"`の場合、**RequestData**は2つの構造が考えられます。

単一リソースに値を書き込む場合：
```json
{
    "path": {?ResourcePath},
    "type": {?ValueType},
    "value": {?Value}
}
```
- `{?ResourcePath}`: 文字列、完全なリソースパス（例：`31024/11/1`）。
- `{?ValueType}`: 文字列、"Time", "String", "Integer", "Float", "Boolean", "Opaque", "Objlnk" のいずれか。
- `{?Value}`: リソースの値。`type`に依存。

例として、Writeコマンドの完全なMQTTペイロードは以下の通りです：
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
完全パスは `{?BasePath}` と `"{ResourcePath}"` の連結です。

例として、Writeコマンドの完全なMQTTペイロードは以下の通りです：
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

LwM2M 1.0では、`<NOTIFICATION>`クラスの属性のみが"Write-Attributes"操作で変更可能です。

この操作は複数の属性を同時に変更できます。

リクエストコマンドで**MsgType**が`"write-attr"`の場合、**RequestData**の構造は以下の通りです：

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
- `{?PeriodMin}`: 数値、通知の最小周期。
- `{?PeriodMax}`: 数値、通知の最大周期。
- `{?GreaterThan}`: 数値、リソース値がこの値を超えたときに通知。
- `{?LessThan}`: 数値、リソース値がこの値を下回ったときに通知。
- `{?Step}`: 数値、リソース値の変化がこの値を超えたときに通知。

#### Execute（実行）

"Execute"操作はLwM2Mサーバが特定のリソースに対してアクションを開始するために使用します。

リクエストコマンドで**MsgType**が`"execute"`の場合、**RequestData**の構造は以下の通りです：
```json
{
  "path": {?ResourcePath},
  "args": {?Arguments}
}
```
変数：
- `{?Arguments}`: 文字列、LwM2M実行引数。

#### Create（作成）

"Create"操作はLwM2MサーバがLwM2Mクライアント内にオブジェクトインスタンスを作成するために使用します。"Create"操作はオブジェクトを対象としなければなりません。

リクエストコマンドで**MsgType**が`"create"`の場合、**RequestData**の構造は以下の通りです：

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

#### Delete（削除）

"Delete"操作はLwM2MサーバがLwM2Mクライアント内のオブジェクトインスタンスを削除するために使用します。

リクエストコマンドで**MsgType**が`"create"`の場合、**RequestData**の構造は以下の通りです：
```json
{
  "path": "{?ObjectID}/{?InstanceID}"
}
```
変数：
- `{?InstanceID}`: 整数、LwM2MオブジェクトインスタンスID。

### 情報報告インターフェース

このインターフェースはLwM2Mサーバが登録済みLwM2Mクライアントのリソースの変化を監視し、新しい値が利用可能になると通知を受け取るために使用します。この監視関係はオブザーブ操作をLwM2Mクライアントに送信することで開始されます。監視はキャンセルオブザーブ操作で終了します。

#### Observe（監視）および Cancel Observation（監視キャンセル）

監視および監視キャンセル要求の**トピック**は以下の通りです：
```
{?mountpoint}{?translators.command.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.command.topic}` はLwM2Mゲートウェイ設定の `translators.command.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.command.topic` が `dn/cmd` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/dn/cmd` となります。

監視および監視キャンセル要求の**ペイロード**の形式は以下の通りです：
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
- `{?ReqID}`: 整数、リクエストID。
- `{?MsgType}`: 文字列、以下のいずれか：
  * `"observe"`: LwM2M監視
  * `"cancel-observe"`: LwM2M監視キャンセル
- `{?ResourcePath}`: 文字列、監視/監視キャンセル対象のLwM2Mリソース。完全なリソースパスのみサポート（例：`/3/0/1`）。

例として、Observeコマンドの完全なMQTTペイロードは以下の通りです：
```json
{
  "reqID": 10,
  "msgType": "observe",
  "data": {
    "path": "/31024/0/1"
  }
}
```

Observeレスポンスの**トピック**は以下の通りです：
```
{?mountpoint}{?translators.response.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.response.topic}` はLwM2Mゲートウェイ設定の `translators.response.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.response.topic` が `up/resp` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/resp` となります。

Observeレスポンスの**ペイロード**の形式は以下の通りです：
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
- `{?ReqID}`: 整数、リクエストID。リクエストとの紐付けに使用。
- `{?MsgType}`: 文字列、リクエストコマンドと同じMsgType。
- `{?RequestPath}`: 文字列、リクエストの`path`フィールドと同じ。
- `{?ResponseCode}`: 文字列、LwM2Mステータスコード（例："2.01", "4.00"など）。
- `{?ResponseMsg}`: 文字列、LwM2Mレスポンスメッセージ（例："content", "bad_request"など）。
- `{?ResourcePath}`: 文字列、要求された完全なリソースパス（例：`31024/11/1`）。
- `{?Value}`: 監視対象リソースの現在の値。

#### Notify（通知）

"Notify"操作はLwM2MクライアントからLwM2Mサーバに対し、有効な監視中にオブジェクトインスタンスまたはリソースの新しい値を送信するために使われます。

LwM2Mクライアントからの通知はMQTTメッセージに変換されます。

通知メッセージの**トピック**は以下の通りです：
```json
{?mountpoint}{?translators.notify.topic}
```
変数：
- `{?mountpoint}` はLwM2Mゲートウェイ設定の `mountpoint` オプションの値です。
- `{?translators.notify.topic}` はLwM2Mゲートウェイ設定の `translators.notify.topic` オプションの値です。

例えば、`mountpoint` が `lwm2m/${endpoint_name}/` に設定され、`translators.notify.topic` が `up/notify` の場合、メッセージのトピックは `lwm2m/<実際のクライアントエンドポイント名>/up/notify` となります。

通知メッセージの**ペイロード**の形式は以下の通りです：

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
- `{?ReqID}`: 整数、リクエストID。リクエストとの紐付けに使用。
- `{?ObserveSeqNum}`: 数値、CoAPメッセージの"Observe"オプションの値。
- `{?ResponseCode}`: 文字列、LwM2Mステータスコード（例："2.01", "4.00"など）。
- `{?ResponseMsg}`: 文字列、LwM2Mレスポンスメッセージ（例："content", "bad_request"など）。
- `{?RequestPath}`: 文字列、リクエストの`path`フィールドと同じ。
- `{?ResourcePath}`: 文字列、要求された完全なリソースパス（例：`31024/11/1`）。
- `{?Value}`: リソースの最新値。

## ブロック単位転送（Block-Wise Transfer）

LwM2Mプロトコルはトランスポート層にCoAPを使用します。CoAPはUDP上で動作するため、単一のデータグラムサイズはネットワークMTU（通常約1500バイト）に制限されます。送信データがこの制限を超える場合、単一のCoAPパケットで送信できません。例えば、数百KB〜数MBのファームウェアパッケージをデバイスにプッシュする場合や、多数のリソースを含むオブジェクトを読み取る場合などです。

この制限に対応するため、CoAPは[RFC 7959](https://datatracker.ietf.org/doc/html/rfc7959)でブロック単位転送機構を定義しています。この機構は大きなペイロードを固定サイズのブロックに分割し、複数のリクエスト/レスポンス交換で転送します。受信側はこれらのブロックを再構成して完全なペイロードを復元します。

EMQXのLwM2Mゲートウェイはブロック単位転送を完全にサポートしています。有効化すると、ゲートウェイは自動的にブロックの分割と再構成を処理します。MQTT側には完全なペイロードが透過的に配信され、ブロック単位の処理は内部で行われます。ピアが`blockwise.max_block_size`より大きいブロックサイズを提案した場合、EMQXは設定された最大値まで交渉します。

### 転送方向

ブロック単位転送は以下の2方向をサポートします：

- **Block1 (サーバ -> デバイス)**

  サーバが大きなデータを書き込む際（例：ファームウェア更新）、EMQXはペイロードを複数のBlock1セグメントに分割し、順次デバイスに送信します。例えば、256バイトのファームウェアペイロードを16バイトのブロックサイズで分割すると、16個のブロックに分かれて順次送信されます。

- **Block2 (デバイス -> サーバ)**

  デバイスが単一パケットサイズ制限を超えるレスポンスを生成する場合（例：デバイスオブジェクト`/3/0`の読み取り）、デバイスは複数のBlock2セグメントでレスポンスを送信します。EMQXはすべてのブロックを自動的に再構成し、MQTTに完全なメッセージを転送します。

### ブロック単位転送の設定

ブロック単位転送はREST APIまたは設定ファイルで有効化および設定できます。

:::: tabs type:card

::: tab REST API

```bash
curl -X 'PUT' 'http://127.0.0.1:18083/api/v5/gateways/lwm2m' \
  -u <your-application-key>:<your-security-key> \
  -H 'Content-Type: application/json' \
  -d '{
  "name": "lwm2m",
  "blockwise": {
    "enable": true,
    "max_block_size": 1024,
    "max_body_size": "4MB",
    "max_concurrent_exchanges": 16,
    "max_total_size": "16MB",
    "exchange_lifetime": "247s"
  }
}'
```

:::

::: tab Configuration

```hocon
gateway.lwm2m {
  blockwise {
    enable = true
    max_block_size = 1024
    max_body_size = "4MB"
    max_concurrent_exchanges = 16
    max_total_size = "16MB"
    exchange_lifetime = "247s"
  }
}
```

:::

::::

以下の表はブロック単位転送の設定項目を説明しています。EMQX 6.3.3以降、`blockwise.max_concurrent_exchanges`および`blockwise.max_total_size`は各クライアントセッションが保持するブロック単位状態の制限となります。

| 設定項目 | 型 | デフォルト | 説明 |
| --- | --- | --- | --- |
| `blockwise.enable` | ブール値 | `true` | ブロック単位転送を有効にするかどうか。 |
| `blockwise.max_block_size` | ブロックサイズ | `1024` | 最大ブロックサイズ（バイト単位）。利用可能な値：`16`, `32`, `64`, `128`, `256`, `512`, `1024`。 |
| `blockwise.max_body_size` | バイトサイズ | `"4MB"` | 再構成後のペイロードの最大サイズ。0より大きい必要があります。これを超えるペイロードはEMQXにより拒否されます。 |
| `blockwise.max_concurrent_exchanges` | 整数 | `16` | 1クライアントセッションが保持できるブロック単位交換の最大数。0より大きい必要があります。制限に達すると新規Block1アップロードは`4.29 Too Many Requests`を受け、ダウンリンクコマンドはセッションキューで待機します。 |
| `blockwise.max_total_size` | バイトサイズ | `"16MB"` | 1クライアントセッションが保持できるブロック単位交換の合計ペイロードサイズの最大値。0より大きい必要があります。制限に達すると新規Block1アップロードは`4.13 Request Entity Too Large`を受けます。容量が空くまでダウンリンクコマンドはキューイングされます。 |
| `blockwise.exchange_lifetime` | 期間 | `"247s"` | EMQXが一時的なブロック単位交換状態を保持する期間。0より大きい必要があります。状態が期限切れになると、対応するセッション容量が解放されます。 |

交換数および保持ペイロード制限は各クライアントセッションごとに独立して適用されます。リスナー全体で共有される制限ではありません。Block-Wise容量が一時的に不足しても、ダウンリンクコマンドはキューに残ります。容量が利用可能になると送信が再開されます。アクティブなダウンリンクが期限切れになると、EMQXはタイムアウトを報告し、トランスポートトランザクションをキャンセルして次のキューコマンドを送信します。`blockwise.max_total_size`より大きいペイロードのダウンリンクコマンドは無期限にキューに残ることなく失敗します。

EMQX 6.3.3以降、`blockwise.max_body_size`、`blockwise.max_total_size`、`blockwise.exchange_lifetime`は0より大きい必要があります。REST APIは非正の値を拒否し、保存済みのゲートウェイ設定を変更しません。

## ユーザーインターフェース

- 詳細な設定オプション：[Gateway configuration - lwm2m (Opensource)](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) および [Gateway configuration - lwm2m (Enterprise)](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)
- 詳細なHTTP API説明：[REST API - Gateway](../../guides/api.md)

## クライアントライブラリ

- [wakaama](https://github.com/eclipse/wakaama)
