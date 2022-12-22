# Webhook

Webhook 是 EMQX 向 HTTP 服务发送消息的通道。通过 Webhook，用户可以选择某个本地主题，将消息
发送到远程 HTTP 服务，也可以将规则的输出发送到 HTTP 服务。

## 示例：使用配置文件创建 Webhook

在 `emqx.conf` 里，添加如下配置：

```js
bridges.webhook.my_webhook {
    enable = true
    url = "http://localhost:9901/${clientid}"
    local_topic = "a/#"
    method = post
    body = "${payload}"
    headers {
        "content-type": "application/json"
    }
}
```

这个 Webhook 的作用是，将发送到本节点的、主题能匹配到 `a/#` 的消息转发到 `http://localhost:9901/${clientid}`。
其中 `${clientid}` 是表示发送者客户端 ID 的占位符变量，
举例来说，如果客户端 ID 为 `steve`，那么消息将发送到 `http://localhost:9901/steve`。

除了 `url` 参数之外，下面几个参数都可以使用占位符：`method`，`body`，`headers`。
但注意在 `url` 参数里只能在路径部分使用占位符，而 `scheme://host:port` 部分不能使用占位符。

可用的占位符字段，详见：[规则 SQL 中的事件类型和字段](./rule-sql-events-and-fields.md#使用规则-sql-语句处理消息发布)。

## 示例：使用 Dashboard 创建 Webhook

详见 [数据桥接简介](./data-bridges.md)。

::: tip
注意 5.0.0 Dashboard 暂不支持脱离规则，单独使用 Webhook。请使用配置文件创建独立运行的 Webhook。
:::
