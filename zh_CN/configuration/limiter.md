# 速率限制器配置

速率限制器是在 EMQX 5.0 中引入的一项新功能，它是一种机制，用于限制客户端或主题在指定时间段内可以发布或订阅的消息数量。有关限制器及其工作原理的更多信息，请参见[速率限制](../rate-limit/rate-limit.md)。

目前，EMQX 使用以下几种类型的限制器来限制速率：

| **类型**        | Dashboard UI | **描述**                                     | **过载后行为**     |
| --------------- | ------------ | -------------------------------------------- | ------------------ |
| `bytes_rate`    | 数据发布速率 | 每个客户端每秒接收的消息大小（以字节为单位） | 暂停接收客户端消息 |
| `messages_rate` | 消息发布速率 | 每个客户端每秒接收的消息数量                 | 暂停接收客户端消息 |
| `max_conn_rate` | 最大连接速率 | 每个监听器每秒的连接数                       | 暂停接收新连接     |

例如，要为默认的 TCP 监听器设置一个限制器，您可以使用以下配置：

```bash
listeners.tcp.default {
  bind = "0.0.0.0:1883"
  max_conn_rate = "1000/s"
  messages_rate = "1000/s"
  bytes_rate = "1MB/s"
}
```

::: tip

EMQX 提供了更多配置项以更好地满足定制化需求。详情请参见 [EMQX 开源版配置手册](https://docs.emqx.com/zh/emqx/v@CE_VERSION@/hocon/)和 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

:::
