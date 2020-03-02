---
# 标题
title: WebHook
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
ref: undefined
---

# WebHook

WebHook 是由 [emqx_web_hook][] [插件][] 提供的 **将 EMQ X Broker 中的钩子事件通知到某个 Web 服务** 的功能。

WebHook 的内部实现是基于 [钩子][]，但它更靠近顶层一些。它通过在钩子上的挂载回调函数，获取到 EMQ X Broker 中的各种事件，并转发至 [emqx_web_hook][] 中配置的 Web 服务器。

以 `客户端成功接入(client.connected)` 事件为例，其事件的传递流程如下：

```
    Client      |    EMQ X     |  emqx_web_hook |   HTTP       +------------+
  =============>| - - - - - - -> - - - - - - - ->===========>  | Web Server |
                |    Broker    |                |  Request     +------------+

```

WebHook 对于事件的处理是单向的，**它仅支持将 EMQ X Broker 中的事件推送给 Web 服务，并不关心 Web 服务的返回**。

在使用 WebHook 时，首先需要知道如何启动和关闭插件，参见：[插件][]。


## 配置

`emqx_web_hook` 的配置文件位于 `etc/plugins/emqx_web_hook.conf`：

|  配置项            | 类型   | 可取值 | 默认值 | 说明               |
| ------------------ | ------ | ------ | ------ | ------------------ |
| api.url            | string | -      | http://127.0.0.1:8080 | 事件需要转发的目的服务器地址 |
| encode_payload     | enum   | `base64`, `base62` | undefined | 对消息类事件中的 Payload 字段进行编码，`undefined` 则表示不编码 |

`encode_payload` 用于对消息的 Payload 内容进行编码，然后在进行传输。当消息内容是不可见字符时，为了能够在 HTTP 协议中传输，这是十分有用的。


### 转发事件

在 `etc/plugins/emqx_web_hooks.conf` 可配置选择需要转发的事件，其配置的格式如下：

``` properties
web.hook.rule.<HookPoint>.<Number> = <Specification>
```


#### HookPoint

`HookPoint` 其含义可参见 [钩子 - 挂载点](hooks.md#hookpoint) 章节。目前仅支持以下事件：

- client.connect
- client.connack
- client.connected
- client.disconnected
- client.subscribe
- client.unsubscribe
- session.subscribed
- session.unsubscribed
- session.terminated
- message.publish
- message.delivered
- message.acked


#### Number

`Number` 为整型数字。

`emqx_web_hook` 支持为同一个事件添加不同的转发配置。所以需要用 `Number` 作为标识符来区分同一事件的不同 `Specification`。


#### Specification

`Specification` 决定了转发规则的参数配置详情，其值为一个 JSON 字符串，其中可用的 Key 有：

- action：字符串，取固定值。
- topic：字符串，表示一个主题过滤器，操作的主题只有与该主题匹配才能触发事件的转发。

例如，我们只将与 `a/b/c` 和 `foo/#` 主题匹配的消息转发到 Web 服务器上，其配置应该为：

``` properties
web.hook.rule.message.publish.1 = {"action": "on_message_publish", "topic": "a/b/c"}
web.hook.rule.message.publish.2 = {"action": "on_message_publish", "topic": "foo/#"}
```

这样 `emqx_web_hook` 仅会转发与 `a/b/c` 和 `foo/#` 主题匹配的消息。例如 `foo/bar` 等，而不是转发 `a/b/d` 或 `fo/bar`。

## Web 服务的实现

`emqx_web_hook` 会将每个事件组成一个 HTTP 请求发送到 `api.url` 所配置的 Web 服务器上。其请求格式为：

```
URL: <api.url>      # 来自于配置中的 `api.url` 字段
Method: POST        # 固定为 POST 方法

Body: <JSON>        # Body 为 JSON 格式字符串
```

对于不同的事件，`<Body>` 的内容有所不同，下表列举了各个事件中 Body 的参数列表

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


[emqx_web_hook]: https://github.com/emqx/emqx-web-hook "emqx-web-hook"
[插件]: plugins.md
[钩子]: hooks.md

