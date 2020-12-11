---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: hjianbo
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# WebHook

WebHook 是由 [emqx_web_hook](https://github.com/emqx/emqx-web-hook) 插件提供的 **将 EMQ X 中的钩子事件通知到某个 Web 服务** 的功能。

WebHook 的内部实现是基于 [钩子](./hooks.md)，但它更靠近顶层一些。它通过在钩子上的挂载回调函数，获取到 EMQ X 中的各种事件，并转发至 emqx_web_hook 中配置的 Web 服务器。

以 客户端成功接入(client.connected) 事件为例，其事件的传递流程如下：

```bash
    Client      |    EMQ X     |  emqx_web_hook |   HTTP       +------------+
  =============>| - - - - - - -> - - - - - - - ->===========>  | Web Server |
                |    Broker    |                |  Request     +------------+
```

::: tip
WebHook 对于事件的处理是单向的，**它仅支持将 EMQ X 中的事件推送给 Web 服务，并不关心 Web 服务的返回**。
借助 Webhook 可以完成设备在线、上下线记录，订阅与消息存储、消息送达确认等诸多业务。
:::

## 配置项

Webhook 的配置文件位于 `etc/plugins/emqx_web_hook.conf`：

|  配置项            | 类型   | 可取值 | 默认值 | 说明               |
| ------------------ | ------ | ------ | ------ | ------------------ |
| api.url            | string | -      | http://127.0.0.1:8080 | 事件需要转发的目的服务器地址 |
| encode_payload     | enum   | `base64`, `base62` | undefined | 对**消息类事件中的 Payload 字段**进行编码，注释或其他则表示不编码 |

::: tip
当消息内容是不可见字符（如二进制数据）时，为了能够在 HTTP 协议中传输，使用 encode_payload 是十分有用的。
:::


## 触发规则

在 `etc/plugins/emqx_web_hooks.conf` 可配置触发规则，其配置的格式如下：

```bash
## 格式示例
web.hook.rule.<Event>.<Number> = <Rule>

## 示例值
web.hook.rule.message.publish.1 = {"action": "on_message_publish", "topic": "a/b/c"}
web.hook.rule.message.publish.2 = {"action": "on_message_publish", "topic": "foo/#"}
```

### Event 触发事件

目前支持以下事件：


| 名称                 | 说明         | 执行时机                                              |
| -------------------- | ------------ | ----------------------------------------------------- |
| client.connect       | 处理连接报文 | 服务端收到客户端的连接报文时                          |
| client.connack       | 下发连接应答 | 服务端准备下发连接应答报文时                          |
| client.connected     | 成功接入     | 客户端认证完成并成功接入系统后                        |
| client.disconnected  | 连接断开     | 客户端连接层在准备关闭时                              |
| client.subscribe     | 订阅主题     | 收到订阅报文后，执行 `client.check_acl` 鉴权前        |
| client.unsubscribe   | 取消订阅     | 收到取消订阅报文后                                    |
| session.subscribed   | 会话订阅主题 | 完成订阅操作后                                        |
| session.unsubscribed | 会话取消订阅 | 完成取消订阅操作后                                    |
| message.publish      | 消息发布     | 服务端在发布（路由）消息前                            |
| message.delivered    | 消息投递     | 消息准备投递到客户端前                                |
| message.acked        | 消息回执     | 服务端在收到客户端发回的消息 ACK 后                   |
| message.dropped      | 消息丢弃     | 发布出的消息被丢弃后                                  |

### Number

同一个事件可以配置多个触发规则，配置相同的事件应当依次递增。

### Rule

触发规则，其值为一个 JSON 字符串，其中可用的 Key 有：

- action：字符串，取固定值
- topic：字符串，表示一个主题过滤器，操作的主题只有与该主题匹配才能触发事件的转发

例如，我们只将与 `a/b/c` 和 `foo/#` 主题匹配的消息转发到 Web 服务器上，其配置应该为：

```bash
web.hook.rule.message.publish.1 = {"action": "on_message_publish", "topic": "a/b/c"}
web.hook.rule.message.publish.2 = {"action": "on_message_publish", "topic": "foo/#"}
```

这样 Webhook 仅会转发与 `a/b/c` 和 `foo/#` 主题匹配的消息，例如 `foo/bar` 等，而不是转发 `a/b/d` 或 `fo/bar`。


## Webhook 事件参数

事件触发时 Webhook 会按照配置将每个事件组成一个 HTTP 请求发送到 `api.url` 所配置的 Web 服务器上。其请求格式为：

```bash
URL: <api.url>      # 来自于配置中的 `api.url` 字段
Method: POST        # 固定为 POST 方法

Body: <JSON>        # Body 为 JSON 格式字符串
```

对于不同的事件，请求 Body 体内容有所不同，下表列举了各个事件中 Body 的参数列表：

**client.connect**

| Key        |  类型   | 说明  |
| ---------- | ------- | ----- |
| action     | string  | 事件名称<br>固定为："client_connect" |
| clientid   | string  | 客户端 ClientId |
| username   | string  | 客户端 Username，不存在时该值为 "undefined" |
| ipaddress  | string  | 客户端源 IP 地址 |
| keepalive  | integer | 客户端申请的心跳保活时间 |
| proto_ver  | integer | 协议版本号 |


**client.connack**

| Key        |  类型   | 说明  |
| ---------- | ------- | ----- |
| action     | string  | 事件名称<br>固定为："client_connack" |
| clientid   | string  | 客户端 ClientId |
| username   | string  | 客户端 Username，不存在时该值为 "undefined" |
| ipaddress  | string  | 客户端源 IP 地址 |
| keepalive  | integer | 客户端申请的心跳保活时间 |
| proto_ver  | integer | 协议版本号 |
| conn_ack   | string  | "success" 表示成功，其它表示失败的原因 |


**client.connected**

| Key         |  类型   | 说明  |
| ----------- | ------- | ----- |
| action      | string  | 事件名称<br>固定为："client_connected" |
| clientid    | string  | 客户端 ClientId |
| username    | string  | 客户端 Username，不存在时该值为 "undefined" |
| ipaddress   | string  | 客户端源 IP 地址 |
| keepalive   | integer | 客户端申请的心跳保活时间 |
| proto_ver   | integer | 协议版本号 |
| connected_at| integer | 时间戳(秒) |


**client.disconnected**

| Key         |  类型   | 说明  |
| ----------- | ------- | ----- |
| action      | string  | 事件名称<br>固定为："client_disconnected" |
| clientid    | string  | 客户端 ClientId |
| username    | string  | 客户端 Username，不存在时该值为 "undefined" |
| reason      | string  | 错误原因 |


**client.subscribe**

| Key         |  类型   | 说明  |
| ----------- | ------- | ----- |
| action      | string  | 事件名称<br>固定为："client_subscribe" |
| clientid    | string  | 客户端 ClientId |
| username    | string  | 客户端 Username，不存在时该值为 "undefined" |
| topic       | string  | 将订阅的主题 |
| opts        | json    | 订阅参数 |

opts 包含

| Key  | 类型 | 说明 |
| ---- | ---- | ---- |
| qos  | enum | QoS 等级，可取 `0` `1` `2` |


**client.unsubscribe**

| Key         |  类型   | 说明  |
| ----------- | ------- | ----- |
| action      | string  | 事件名称<br>固定为："client_unsubscribe" |
| clientid    | string  | 客户端 ClientId |
| username    | string  | 客户端 Username，不存在时该值为 "undefined" |
| topic       | string  | 取消订阅的主题 |


**session.subscribed**：同 `client.subscribe`，action 为 `session_subscribed`

**session.unsubscribed**：同 `client.unsubscribe`，action 为 `session_unsubscribe`

**session.terminated**： 同 `client.disconnected`，action 为 `session_terminated`

**message.publish**

| Key            |  类型   | 说明  |
| -------------- | ------- | ----- |
| action         | string  | 事件名称<br>固定为："message_publish" |
| from_client_id | string  | 发布端 ClientId |
| from_username  | string  | 发布端 Username，不存在时该值为 "undefined" |
| topic          | string  | 取消订阅的主题 |
| qos            | enum    | QoS 等级，可取 `0` `1` `2` |
| retain         | bool    | 是否为 Retain 消息 |
| payload        | string  | 消息 Payload |
| ts             | integer | 消息的时间戳(毫秒) |


**message.delivered**

| Key            |  类型   | 说明  |
| -------------- | ------- | ----- |
| action         | string  | 事件名称<br>固定为："message_delivered" |
| clientid       | string  | 接收端 ClientId |
| username       | string  | 接收端 Username，不存在时该值为 "undefined" |
| from_client_id | string  | 发布端 ClientId |
| from_username  | string  | 发布端 Username，不存在时该值为 "undefined" |
| topic          | string  | 取消订阅的主题 |
| qos            | enum    | QoS 等级，可取 `0` `1` `2` |
| retain         | bool    | 是否为 Retain 消息 |
| payload        | string  | 消息 Payload |
| ts             | integer | 消息时间戳(毫秒) |


**message.acked**

| Key            |  类型   | 说明  |
| -------------- | ------- | ----- |
| action         | string  | 事件名称<br>固定为："message_acked" |
| clientid       | string  | 接收端 ClientId |
| from_client_id | string  | 发布端 ClientId |
| from_username  | string  | 发布端 Username，不存在时该值为 "undefined" |
| topic          | string  | 取消订阅的主题 |
| qos            | enum    | QoS 等级，可取 `0` `1` `2` |
| retain         | bool    | 是否为 Retain 消息 |
| payload        | string  | 消息 Payload |
| ts             | integer | 消息时间戳(毫秒) |
