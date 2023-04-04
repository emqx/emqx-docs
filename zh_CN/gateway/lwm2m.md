# LwM2M 协议网关

## 简介

[LwM2M (Lightweight Machine-to-Machine)](https://lwm2m.openmobilealliance.org/)
是为物联网设备和机器对机器通信设计的协议。它是一种轻量级协议，对处理能力和内存
受限的设备非常友好。

EMQX 中的 LwM2M 网关 可以接收 LwM2M 客户端连接并将其事件和消息转换为 MQTT Publish
消息。

在当前的实现中，它有以下限制：
- 基于 UDP/DTLS 传输。
- 仅支持 v1.0.2。暂时不支持 v1.1.x 和 v1.2.x。
- 不包括 LwM2M Bootstrap 服务。

## 快速开始

EMQX 5.0 中，可以通过 Dashboard 配置并启用 LwM2M 网关。

也可以通过 HTTP API 或 emqx.conf 来启用，例如：

:::: tabs type:card

::: tab HTTP API

```bash
curl -X 'POST' 'http://127.0.0.1:18083/api/v5/gateway/lwm2m' \
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
通过配置文件进行配置网关，需要在每个节点中进行配置；通过 Dashboard 或者 HTTP API 管理则会在整个集群中生效。
:::

LwM2M 网关支持 UDP、DTLS 类型的监听器，其完整可配置的参数列表参考：[网关配置 - 监听器](../configuration/configuration-manual.md)

## 认证

由于 LwM2M 协议仅提供了客户端的 Endpoint Name，而没有提供用户名和密码。因此，
LwM2M 网关仅支持 [HTTP Server 认证](../access-control/authn/http.md)。

例如，通过 HTTP API 或 emqx.conf 为 LwM2M 网关创建 HTTP 认证：

:::: tabs type:card

::: tab HTTP API

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
    enable = true
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


## 消息格式

LwM2M 协议的消息传递模型基于[Resources Model and Operations](https://technical.openmobilealliance.org/OMNA/LwM2M/LwM2MRegistry.html)。这与 MQTT 协议的发布/订阅模型完全不同。因此，
在 LwM2M 网关中，定义了一种消息格式来兼容这两种消息模型。

### Client Registration Interface

#### Register

**Register** 消息是由 LwM2M 客户端发送给 LwM2M 服务器的，以便将自己注册到服务器上。
它包含有关客户端及其功能的信息，如端点名称（Endpoint Name）、生命周期（Life Time）、
LwM2M 版本（Protocol Version）、对象（Objects）、对象实例（Object Instances）等。

**Register** 消息是客户端发送的第一条消息，用于启动与服务器的通信。

LwM2M 网关将把 **Register** 消息转换为以下格式的 MQTT 消息。

注册消息的 **主题（Topic）** 为：
```
{?mountpoint}{?translators.register.topic}
```
其中：
- `{?mountpoint}` 是 LwM2M 网关配置中 `mountpoint` 选项的值。
- `{?translators.register.topic}` 是 LwM2M 网关配置中的 `translators.register.topic` 选项的值。

例如，如果 `mountpoint` 被配置为 `lwm2m/${endpoint_name}/`，
`translators.register.topic` 被配置为 `up/register`，
那么注册消息的主题将是 `lwm2m/<your-real-client-endpoint-name>/up/register`。

注册消息的 **消息体（Payload）** 格式为：
```json
{
  "msgType": "register",
  "data": {
    "ep": {?EndpointName},
    "lwm2m": {?Version},
    "lt": {?LifeTime},
    "b": {?Binding},
    "objectList": {?ObjectList}
  }
}
```

其中：
- `{?EndpointName}`：String，LwM2M 客户端的端点名称（Endpoint Name）。
- `{?Version}`：String，LwM2M 客户端的协议版本。
- `{?LifeTime}`：Number，LwM2M 客户端请求的保活周期。
- `{?Binding}`：Enum，此参数指定客户端支持的与服务器通信的绑定类型。它可以是：
  * `"U"`：UDP
  * `"UQ"`：带队列模式的 UDP
- `{?ObjectList}`：Array，支持的对象（Objects）和 LwM2M 客户端上可用的对象实例（Object Instances）列表。

例如，Register 消息的完整 MQTT 消息体可以是：
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

**Update** 消息由 LwM2M 客户端发送到 LwM2M 服务器以更新注册信息。它类似于
**Register** 消息，但是在初始化注册后发送。

Update 消息包含有关客户端功能或状态的任何更改的信息，例如 IP 地址的更改或
对由 LwM2M 对象（Objects）的数据更新。

Update 消息也用于延长客户端的注册期限，因此客户端可以让服务器知道它仍然可用和活跃。

Update 消息的频率由 Register 消息中指定的生存期值确定。

Update 消息将由 LwM2M 网关转换为以下格式的 MQTT 消息。

Update 消息的 **主题（Payload）** 格式为：
```
{?mountpoint}{?translators.update.topic}
```
其中：
- `{?mountpoint}` 是 LwM2M 网关配置中 `mountpoint` 选项的值。
- `{?translators.update.topic}` 是 LwM2M 网关配置中的 `translators.update.topic` 选项的值。

例如，如果 `mountpoint` 被配置为 `lwm2m/${endpoint_name}/`，
`translators.update.topic` 被配置为 `up/update`，
那么注册消息的主题将是 `lwm2m/<your-real-client-endpoint-name>/up/update`。


Update 消息的 **消息体（Payload）** 格式为：
```json
{
  "msgType": "update",
  "data": {
    "ep": {?EndpointName},
    "lwm2m": {?Version},
    "lt": {?LifeTime},
    "b": {?Binding},
    "objectList": {?ObjectList}
  }
}
```

这些变量的格式和类型与注册消息中的一致。

例如，完整的 Update 消息的 MQTT 消息体可以是：
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

### LwM2M Device Management & Service Enablement Interface

此接口用于 LwM2M 服务器访问已注册的 LwM2M 客户端提供的对象实例（Objects）和资源 （Resources）。

该接口通过使用 “创建（Create）”、“读取（Read）”、“写入（Write）”、“删除（Delete）”、“执行（Execute）”、“写入属性（Write-Attributes）” 或 “发现（Discover）” 等操作语义进行访问。

每个资源支持的操作、数据格式等在对象模板文件（Object Template files）中进行定义。

要向 LwM2M 客户端发送命令，您需要以固定格式将 MQTT 消息发送到 EMQX。这些消息将由
 LwM2M 网关转换为正确的 LwM2M 消息并发送到 LwM2M 客户端。

请求的主题（Topic）格式为：
```
{?mountpoint}{?translators.command.topic}
```
其中：
- `{?mountpoint}` 是 LwM2M 网关配置中 `mountpoint` 选项的值。
- `{?translators.command.topic}` 是 LwM2M 网关配置中的 `translators.command.topic` 选项的值。

例如，如果 `mountpoint` 被配置为 `lwm2m/${endpoint_name}/`，
`translators.command.topic` 被配置为 `dn/cmd`，
那么注册消息的主题将是 `lwm2m/<your-real-client-endpoint-name>/dn/cmd`。


请求的消息体（Payload） 为：
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
其中：
- `{?ReqID}`：Integer，请求 ID，用于将响应与请求相互配对。
- `{?MsgType}`：String，可以为以下之一：
  - `"read"`：读操作。
  - `"discover"`：发现操作。
  - `"write"`：写操作。
  - `"write-attr"`：写属性操作。
  - `"execute"`：执行操作。
  - `"create"`：创建操作。
  - `"delete"`：删除操作。
- `{?RequestData}`：JSON 对象，它的值取决于 `{?MsgType}` 的值。具体会在下面的章节介绍。


响应的主题（Topic）格式为：
```
{?mountpoint}{?translators.response.topic}
```
其中：
- `{?mountpoint}` 是 LwM2M 网关配置中 `mountpoint` 选项的值。
- `{?translators.response.topic}` 是 LwM2M 网关配置中的 `translators.response.topic` 选项的值。

例如，如果 `mountpoint` 被配置为 `lwm2m/${endpoint_name}/`，
`translators.response.topic` 被配置为 `up/resp`，
那么注册消息的主题将是 `lwm2m/<your-real-client-endpoint-name>/up/resp`。


响应的消息体（Payload）格式为：
```json
{
  "reqID": {?ReqID},
  "msgType": {?MsgType},
  "data": {?Data}
}
```
其中：
- `{?ReqID}`：Integer，请求 ID，用于将响应与请求相互配对。
- `{?MsgType}`：String，消息类型与发起请求的消息类型相同。
- `{?ResponseData}`：JSON 对象，应答的消息体内容，具体取决于不同的消息类型。


#### Read

**读取（Read）** 操作用于访问单个资源的值（Resource），对象实例（Object Instance）下所有的资源，或对象（Object）的所有实例所有资源的值。

在请求命令中，当 **MsgType** 为 `"read"` 时，**RequestData** 的结构应如下所示：
```json
{
  "path": {?ResourcePath}
}
```
其中：
- `{?ResourcePath}`：String，需要读取的资源路径，可能有以下三种情况：
  * 仅 `ObjectID`，例如 `/3`， 它表示读取该对象下所有实例和资源的值。
  * `ObjectID/InstanceID`，例如 `/3/0`，它表示读取该对象实例下所有资源的值。
  * 全路径 `{ObjectID}/{InstanceID}/{ResourceID}`，例如 `/3/0/1`，它表示仅读取指定
    的该资源的值。

例如，Read 请求消息的完整 MQTT 消息体可能是：
```json
{
  "reqID": 1,
  "msgType": "read",
  "data": {
    "path": "/3/0/1"
  }
}
```

在响应中，**ResponseData** 的结构应该是：
```json
{
  "reqPath": {?ResourcePath},
  "code": {?ResponseCode},
  "codeMsg": {?ResponseMsg},
  "content": {?ReadResponseData}
}
```
其中：
- `{?ResourcePath}`：String，等于请求命令中的 `path` 字段。
- `{?ResponseCode}`：String，响应状态码，例如 "2.01"、"4.00" 等。
- `{?ResponseMsg}`：String，响应状态消息，例如 "content"、"bad_request" 等。
- `{?ReadResponseData}`：JSON 对象，读取成功后返回的值数组。


例如，Read 响应消息的完整 MQTT 消息体可能是：
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

**发现（Discover）** 操作用于发现附加到对象（Object），对象实例（Object Instance）
和资源（Resource）的 LwM2M 属性（Attributes）。此操作也用于发现在指定对象、或
对象实例中所有的资源列表。

该操作返回的内容是每个对象，对象实例或资源的的 `application/link-format`
CoRE Links [RFC6690] 的列表。

在请求命令中，当 MsgType 为 "discover" 时，RequestData 的结构应如下所示：

```json
{
  "path": {?ResourcePath}
}
```

它和读操作具有相同的请求格式：

* 仅 `ObjectID`，例如 `/3`，表示发现该对象下的所有的实例、资源、和属性。
* `ObjectID/InstanceID`，例如 `/3/0`，表示发现该对象实例下所有的资源、和属性。
* `{ObjectID}/{InstanceID}/{ResourceID}` 全路径，例如 `/3/0/1`，表示发现指定资源的所有属性。


例如，Discover 请求消息的完整 MQTT 消息体可能是：
```json
{
  "reqID": 2,
  "msgType": "discover",
  "data": {
    "path": "/3/0"
  }
}
```

在应答消息中，**ResponseData** 结构应该是：
```json
{
  "reqPath": {?ResourcePath},
  "code": {?ResponseCode},
  "codeMsg": {?ResponseMsg},
  "content": {?DiscoverResponseData}
}
```
除了 `content` 字段，它和 Read 的响应消息具有相同的格式。
`{?DiscoverResponseData}` 是以 CoRE Links 格式的数组。

例如，Discover 应答消息的完整 MQTT 消息体可能是：
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

**写入（Write）** 操作用于更改某一资源，或对象实例下多个资源的值。

在请求命令中，当 **MsgType** 为 `"write"` 时，**RequestData** 有两种可能的结构。

第一种，用于向单个资源写入一个值的结构如下：
```json
{
    "path": {?ResourcePath},
    "type": {?ValueType},
    "value": {?Value}
}
```
其中：
- `{?ResourcePath}`： String，请求修改的资源路径，例如 `31024/11/1`。
- `{?ValueType}`： String， 可为 "Time"、"String"、"Integer"、"Float"、"Boolean"、"Opaque"、"Objlnk"。
- `{?Value}`：要写入资源的值，类型和格式依赖于 `type`。

例如，Write 命令消息的完整 MQTT 消息体可能是：
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

第二种，用于向多个资源写入值的请求消息结构如下：
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
其中，每个资源的路径由 `{?BasePath}` 和 `"{ResourcePath}`。

例如，Write 命令消息的完整 MQTT 消息体可能是：
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

在 LwM2M 1.0 中，只有 `<NOTIFICATION>` 类中的属性可以使用 **写入属性（Write-Attributes）**
操作进行更改属性。

该操作允许在一次操作中修改多个属性。

在请求命令中，当 **MsgType** 为 `"write-attr"` 时，**RequestData** 的结构应如下所示：
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
其中：
- `{?PeriodMin}`: Number，控制通知的最小间隔时间。
- `{?PeriodMax}`: Number，控制通知的最大间隔时间。
- `{?GreaterThan}`: Number，当属性值大于该值时进行通知。
- `{?LessThan}`: Number，当属性值大于该值时进行通知。
- `{?Step}`: Number，当属性值变化步长大于该值时进行通知。


#### Execute

**执行（Execute）** 操作用于 LwM2M 服务器调用可执行资源，只能在单个资源上进行执行。

在请求命令中，当 **MsgType** 为 "execute" 时，有效负载的结构应包括以下参数:
```json
{
  "path": {?ResourcePath},
  "args": {?Arguments}
}
```
其中：
- `{?Arguments}`：String, 执行参数。


#### Create

**创建（Create）** 操作由 LwM2M 服务器发送给 LwM2M 客户端 用于创建创建
对象实例（Object Instance）。 创建操作必须作用于一个对象上。

在请求命令中，当 **MsgType** 为"create" 时，**RequestData** 的结构应如下：
```json
{
  "basePath": {?ObjectID},
  "content": [
    {
      "path": {?ResourcePath},
      "type": {?ValueType},
      "value": {?Value}
    }
  ]
}
```
其中：
- `{?ObjectID}`：String， 对象ID。
- `{?ResourcePath}`：String，资源 ID。
- `{?ValueType}`： String， 可为 "Time"、"String"、"Integer"、"Float"、"Boolean"、"Opaque"、"Objlnk"。
- `{?Value}`：资源的值，类型和格式依赖于 `type`。

#### Delete

**删除（Delete）** 操作用于LwM2M 服务器删除 LwM2M 客户端中的对象实例（Object Instance）。

在请求命令中，当 **MsgType** 为 "delete" 时，**RequestData** 的结构应如下：
```json
{
  "path": {?ObjectID}/{?InstanceID}
}
```
其中：
- `{?ObjectID}/{?InstanceID}`：String，为需要删除的对象实例 ID，例如 `/31024/0`。

### Information Reporting Interface

LwM2M 服务器使用该接口监测 LwM2M 客户端中资源的变化，并在有新值可用时接收通知。
这种观察关系是通过向一个对象（Object）、一个对象实例（Object Instance）或
一个资源（Resource）发送 **观测（Observe）** 操作来启动的。

LwM2M 服务器可以通过发送一个 **取消观测（Cancel Observation）**操作，来停止接收通知。

#### Observe and Cancel Observation

观测/取消观测请求的主题（Topic）格式为：
```
{?mountpoint}{?translators.command.topic}
```
其中：
- `{?mountpoint}` 是 LwM2M 网关配置中 `mountpoint` 选项的值。
- `{?translators.command.topic}` 是 LwM2M 网关配置中的 `translators.command.topic` 选项的值。

例如，如果 `mountpoint` 被配置为 `lwm2m/${endpoint_name}/`，
`translators.command.topic` 被配置为 `dn/cmd`，
那么注册消息的主题将是 `lwm2m/<your-real-client-endpoint-name>/dn/cmd`。


观测/取消观测请求的消息体格式为：
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
其中：
- `{?ReqID}`：Integer、请求 ID，用于将响应与请求相互配对。
- `{?MsgType}`：String、消息类型，可选值为：
  * `"observe"`：表示发起观测操作。
  * `"cancel-observe"`：表示发起取消观测操作。
- `{?ResourcePath}`：String、被观测/取消观测的资源全路径，例如 `/3/0/1`。


例如，Observe 命令消息的完整 MQTT 消息体可能是：
```json
{
  "reqID": 10,
  "msgType": "observe",
  "data": {
    "path": "/31024/0/1"
  }
}
```

Observe 命令的返回主题：
```
{?mountpoint}{?translators.response.topic}
```
其中：
- `{?mountpoint}` 是 LwM2M 网关配置中 `mountpoint` 选项的值。
- `{?translators.response.topic}` 是 LwM2M 网关配置中的 `translators.response.topic` 选项的值。

例如，如果 `mountpoint` 被配置为 `lwm2m/${endpoint_name}/`，
`translators.response.topic` 被配置为 `up/resp`，
那么注册消息的主题将是 `lwm2m/<your-real-client-endpoint-name>/up/resp`。

Observe 命令的消息体格式应该为：
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
其中：
- `{?ReqID}`：Integer、请求 ID，用于将响应与请求相互配对。
- `{?MsgType}`：String、消息类型与发起请求的消息类型相同。
- `{?RequestPath}`：String、与请求命令中的 `path` 字段相等。
- `{?ResponseCode}`：String，响应状态码，例如 "2.01"、"4.00" 等。
- `{?ResponseMsg}`：String，响应状态消息，例如 "content"、"bad_request" 等。
- `{?ResourcePath}`：String，被观测的资源 ID，例如 `31024/0/1`。
- `{?Value}`：被观测资源的当前值。

#### Notify

**通知（Notify）** 消息是在对象实例或资源上进行检测后，LwM2M客户端将被检测资源的值
通知到 LwM2M 服务器的消息。

通知消息的 **主题** 是：

```json
{?mountpoint}{?translators.notify.topic}
```
变量：
- `{?mountpoint}`：是 LwM2M 网关配置中 `mountpoint` 选项的值。
- `{?translators.notify.topic}`：是 LwM2M 网关配置中 `translators.notify.topic` 选项的值。

例如，如果将 `mountpoint` 配置为 `lwm2m/${endpoint_name}/`，
并且 `translators.notify.topic` 是 `up / notify`，那么消息的主题将是
`lwm2m/<your-real-client-endpoint-name>/up/notify`。

通知消息的消息体格式如下：
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
其中：
- `{?ReqID}`：Integer，请求 ID，用于将响应与请求相互配对。
- `{?ObserveSeqNum}`：Number，CoAP 消息中 "Observe" 选项的值。
- `{?ResponseCode}`，String，响应状态码，例如 "2.01"、"4.00" 等。
- `{?ResponseMsg}`，String，响应状态消息，例如 "content"、"bad_request" 等。
- `{?RequestPath}`，String，与请求命令中的 `path` 字段相等。
- `{?ResourcePath}`：String，请求的完全资源路径，即`31024/11/1`。
- `{?Value}`：资源的最新值。

## 用户层接口

- 详细配置说明参考：[网关配置 - LwM2M 网关](../configuration/configuration-manual.md)
- 详细 HTTP API 接口参考：[HTTP API - 网关](../admin/api.md)

## 客户端库

- [wakaama](https://github.com/eclipse/wakaama)
