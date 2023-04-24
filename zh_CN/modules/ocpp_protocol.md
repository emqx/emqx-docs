# OCPP 协议网关

## 协议介绍

[OCPP (Open Charge Point Protocol)](https://www.openchargealliance.org/)
是用于充电站（Charge Point）和 Central System 之间进行通信的协议，以实现对电动汽车（Electric Vehicle）
充电会话的监控和控制。

自从e4.4.18版本以来，EMQX 实现了基于[OCPP-J 1.6](https://www.openchargealliance.org/protocols/ocpp-16/)
标准的 `emqx_ocpp` 插件，提供了连接充电桩（Charge Point）设备的能力

需要注意的是，该插件的主要功能是接收 OCPP 客户端的连接、处理上下行消息的格式转换和转发。
任何与业务相关的实现（如启动充电和计费）必须由第三方系统处理。

此外，在这个版本中，OCPP 协议网关作为一个插件而不是模块运行。

## 快速开始

所有的 OCPP 网关的配置都位于 EMQX `etc` 目录下的 `plugins/emqx_ocpp.conf` 文件中。

执行以下命令，以默认配置文件启动 OCPP 插件：
```bash
emqx_ctl plugins load emqx_ocpp
```

### 连接到 OCPP 网关

插件成功加载后，您可以使用 [ocpp-go](https://github.com/lorenzodonini/ocpp-go) 模拟充电点进行连接测试。

请先按照 ocpp-go 文档提供的方式完成编译，然后执行以下命令即可连接到 EMQX 的 OCPP 网关。

注：请修改 `<host>` 为您真实的 EMQX 节点的地址。
```bash
CLIENT_ID=chargePointSim CENTRAL_SYSTEM_URL=ws://<host>:33033/ocpp go run example/1.6/cp/*.go
```

## 客户端连接

OCPP 网关仅提供以 Websocket 的方式接收客户端的链接。这也是 OCPP-J 1.6 协议中定义的方式。

在成功建立连接后，EMQX 将其看做一个标准的客户端。这意味着您可以通过 Client ID 在 Dashboard/HTTP-API/CLI
对该客户端进行管理。

### 认证

正如 OCPP-J 1.6 规范中提到的，Charge Point 可以使用 HTTP Basic 进行认证。
OCPP 网关从中提取 Username 和 Password，并通过 EMQX 的认证系统来判断是否该客户端是否具有接入权限。

也就是说，OCPP-J 网关使用 EMQX 的认证插件来授权 Charge Point 的登录。

你可以参考 [认证](../advanced/auth.md) 进行配置。

## 消息流程

以下流程图表示一个充电点与第三方服务之间通过 EMQX OCPP 网关进行通信的过程：

```
                                +--------------+  upstream publish     +-----------+
+--------------+   Req/Resp     | OCPP Gateway | --------------------> | 3rd-party |
| Charge Point | <------------> | over         |     over Topic        | Service   |
+--------------+   over ws/wss  | EMQX         | <-------------------- |           |
                                +--------------+  downstream publish   +-----------+
```

如图所示：
- Charge Point 和 OCPP 网关之间基于 Websocket 或 Websocket over TLS （ws/wss）进行通信。
- OCPP 网关会把 Charge Point 产生的消息转化为一个标准的 MQTT Publish 消息，其消息的主题、和消息的
  内容及格式，均由 OCPP 网关进行定义。
- 第三方系统通过订阅 Upstream 主题从充电点接收消息；
  通过向 Downstream 主题发送消息向充电点推送控制消息。

### OCPP 网关到三方系统的消息 (Upstream)

OCPP 网关将 Charge Point 所有的消息、事件通过 EMQX 进行发布。这个数据流称为 **Upstream**。

例如，一个默认的 Upstream 的主题可以配置为如下所示。OCPP 网关会将所有来自 Charge Point 的消息发布到该主题。
在消息处理过程中，占位符 `${cid}`（Charge Point ID）和 `${action}` （消息名称）会被替换为实际的值。

```hcl
## plugins/emqx_ocpp.conf
## OCPP gateway will publish all Charge Point messages to this topic.
ocpp.upstream.topic = ocpp/cp/${cid}/${action}

## Supports overriding the default topic by message name
ocpp.upstream.topic.BootNotification = ocpp/cp/${cid}/Notify/${action}
```

消息内容（Payload）为具有固定模式的 JSON 字符串，它包括字段：

| Field              | Type        | Required | Desc                                             |
| ------------------ | ----------- | -------- | ------------------------------------------------ |
| `MessageTypeId`    | MessageType | Y        | 消息类型，可选值：<br><br/>为 `2` 表示 Call 消息<br> `3` 表示 CallResult 消息<br>`4` 表示 CallError 消息 |
| `UniqueId`         | String      | Y        | 消息唯一 ID，用于匹配 Call 和 CallResult 消息    |
| `Action`           | String      | N        | OCPP 消息的消息名称，例如 "Authorize"            |
| `ErrorCode`        | ErrorType   | N        | 错误码，在 CallError 消息中为必填                |
| `ErrorDescription` | String      | N        | 错误描述                                         |
| `Payload`          | Object      | N        | 消息内容                                         |

例如，一条在 upstream 上的 `BootNotifiaction` 的消息格式为：

```json
Topic: ocpp/cp/CP001/Notify/BootNotifiaction
Payload:
  {"MessageTypeId": 2,
   "UniqueId": "1",
   "Payload": {"chargePointVendor":"vendor1","chargePointModel":"model1"}
  }
```

同样，对于 Charge Point 发送到 Central System 的应答消息和错误通知，
也可以定制其主题格式：

```hcl
## plugins/emqx_ocpp.conf

ocpp.upstream.reply_topic = ocpp/cp/Reply/${cid}
ocpp.upstream.error_topic = ocpp/cp/Error/${cid}
```

### 三方系统到 OCPP 网关的消息 (Downstream)

第三方系统可以利用在 OCPP 网关中配置的主题向充电点发出控制消息，这个数据流被称为 **Downstream**。

例如，默认的 Downstream 配置如下所示。其中，占位符 `${cid}`（Charge Point ID）会被替换为实际的值。
```
## plugins/emqx_ocpp.conf
## Topic to receive downstream control commands.
ocpp.dnstream.topic = ocpp/${cid}/+/+
```
注：在此示例中使用通配符 `+` 可提供主题结构的灵活性。但是，这不是必需的，可以根据您的要求进行调整。

下行消息的消息内容（Payload）为具有固定模式的 JSON 字符串，格式同 upstream。

例如，一条从三方系统发到网关的 BootNotifaction 的应答消息格式为：
```
Topic: ocpp/cp/CP001/Reply/BootNotification
Payload:
  {"MessageTypeId": 3,
   "UniqueId": "1",
   "Payload": {"currentTime": "2022-06-21T14:20:39+00:00", "interval": 300, "status": "Accepted"}
  }
```
