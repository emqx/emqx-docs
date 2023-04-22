# OCPP 协议网关

## 协议介绍

[OCPP (Open Charge Point Protocol)](https://www.openchargealliance.org/)
是用于充电站（Charge Points）和 Central System 之间进行通信的协议，以实现对电动汽车（Electric Vehicle）
充电会话的监控和控制。

自 e4.4.18 以来，EMQX 已经基于[OCCP-J 1.6](https://www.openchargealliance.org/protocols/ocpp-16/)
标准实现了 `emqx_ocpp` 插件，以提供连接 Charge Point 设备的能力。

::: note
当前实现仅作为插件提供，目前暂不支持模块。
:::

## 快速开始

所有的 OCPP 网关的配置都位于EMQX etc 目录下的 `plugins/emqx_ocpp.conf` 文件中。

以默认配置文件启动 OCPP 插件，只需执行以下命令：
```bash
emqx_ctl plugins load emqx_ocpp
```

### 连接到 OCPP 网关

插件成功加载后，您可以使用 [ocpp-go](https://github.com/lorenzodonini/ocpp-go) 模拟充电点进行连接测试。

请先按照 ocpp-go 文档提供的方式完成编译，然后执行以下命令即可连接到 EMQX 的 OCPP 网关。

注：`<host>` 需要修改为您真实的 EMQX 节点的地址。
```bash
CLIENT_ID=chargePointSim CENTRAL_SYSTEM_URL=ws://<host>:33033/ocpp go run example/1.6/cp/*.go
```

## 连接

OCPP 网关仅提供以 Websocket 的方式接收客户端的链接。这也是 OCPP-J 1.6 协议中定义的方式。

在成功建立连接后，EMQX 将其看做一个标准的客户端。这意味着您可以通过 Client ID 在 Dashboard/HTTP-API/CLI
对该客户端进行管理。

其中，客户端信息的映射关系如下：
- Client ID：Charge Point 的唯一标识。
- Username：由 Charge Point 在连接时提供的 HTTP Basic 认证解析得来。
- Password：同样由 HTTP Basic 认证解析得来。


### 认证

正如 OCPP-J 1.6 规范中提到的，Charge Point 可以使用 HTTP Basic 进行认证。
OCPP 网关从中提取 Username 和 Password，并通过 EMQX 的认证系统来判断是否该客户端是否具有接入权限。

也就是说，OCPP-J 网关使用 EMQX 的认证插件来授权 Charge Point 的登录。

你可以参考 [认证](../advanced/auth.md) 进行配置。

## 消息格式

```
                                +--------------+  upstream publish  +---------+
+--------------+   Req/Resp     | OCPP Gateway | -----------------> | Third   |
| Charge Point | <------------> | over         |     over Topic     | Service |
+--------------+   over ws/wss  | EMQX         | <----------------- |         |
                                +--------------+  dnstream publish  +---------+
```

如图所示：
- Charge Point 和 OCPP 网关之间通过 OCPP-J 规范、基于 Websocket 或 Websocket over TLS 进行通信。
- OCPP 网关会把 Charge Point 产生的消息转化为一个标准的 MQTT Publish 消息，其消息的主题、和消息的
  内容及格式，均由 OCPP 网关进行定义。
- 三方系统通过订阅上行流（Upstream）的主题来接收 Charge Point 的消息；也通过向下行流（Dnstream）的主题
  发生消息来向 Charge Point 推送控制消息。
- OCPP 网关仅负责处理上下行消息的格式转换，和转发。任何和业务相关的实现都需要三方系统来负责，例如
  启动充电和计费等。


### Up Stream (emqx-ocpp -> third-services)

OCPP 网关将 Charge Point 所有的消息、事件通过 EMQX 进行发布。这个数据流称为 **Up Stream**。

其主题支持按任意格式进行配置，例如：
```
## plugins/emqx_ocpp.conf
##
## 上行默认主题。emqx-ocpp 网关会将所有 Charge Point 的消息发布到该主题上。
##
## 可用占位符为：
## - cid: Charge Point ID
## - action: The Message Name for OCPP
##
ocpp.upstream.topic = ocpp/cp/${cid}/${action}

## 支持按消息名称对默认主题进行重载
##
ocpp.upstream.topic.BootNotification = ocpp/cp/${cid}/Notify/${action}
```

消息内容（Payload）为具有固定模式的 JSON 字符串，它包括字段：

| Field             | Type        | Required | Desc                                             |
| ----------------- | ----------- | -------- | ------------------------------------------------ |
| MessageTypeId     | MessageType | R        | 消息类型，为 "Call", "CallResult" 或 "CallError" |
| UniqueId          | String      | R        | 消息唯一 ID，用于匹配 Call 和 Call Result 消息   |
| Action            | String      | O        | OCPP 消息的消息名称，例如 "Authorize"            |
| ErrorCode         | ErrorType   | O        | 错误码                                           |
| ErrorDescription  | String      | O        | 详细错误描述                                     |
| Payload           | Object      | O        | 消息内容                                         |

例如，一条在 upstream 上的 BootNotifiaction.req 的消息格式为：

```
Topic: ocpp/cp/CP001/Notify/BootNotifiaction
Payload:
  {"MessageTypeId": 2,
   "UniqueId": "1",
   "Payload": {"chargePointVendor":"vendor1","chargePointModel":"model1"}
  }
```

同样，对于 Charge Point 发送到 Central System 的 `*.conf` 的应答消息和错误通知，
也可以定制其主题格式：

```
## plugins/emqx_ocpp.conf

ocpp.upstream.reply_topic = ocpp/cp/Reply/${cid}
ocpp.upstream.error_topic = ocpp/cp/Error/${cid}
```

### Down Stream (third-services -> emqx-ocpp)

三方系统可以通过在 OCPP 网关中配置的主题来下发对 Charge Point 的控制消息。
这个数据流被称为 **Down Stream**。

其主题支持按任意格式进行配置，例如：
```
## plugins/emqx_ocpp.conf
##
## 下行主题。网关会为每个连接的 Charge Point 网关自动订阅该主题，
## 以接收下行的控制命令。
##
## 可用占位符为：
## - cid: Charge Point ID
##
## 注：1. 为了区分每个 Charge Point，所以 ${cid} 是必须的
##     2. 通配符 `+` 不是必须的，此处仅是一个示例
ocpp.dnstream.topic = ocpp/${cid}/+/+
```

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
