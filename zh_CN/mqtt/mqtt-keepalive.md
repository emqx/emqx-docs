# 连接保活心跳(KeepAlive)

为避免 TCP 可能出现的半连接问题导致无法正常通信，MQTT 协议设计了连接保活心跳(KeepAlive)机制，使客户端和 MQTT 服务器可以判定当前是否存在半连接问题，从而关闭对应连接。

客户端向服务器发起 CONNECT 请求时，通过 KeepAlive 参数设置保活周期。

客户端在无报文发送时，按 KeepAlive 周期定时发送 2 字节的 PINGREQ 心跳报文，服务端收到 PINGREQ 报文后，回复 2 字节的 PINGRESP 报文。

默认 EMQX 在 1.5 个心跳周期内，既没有收到客户端发布订阅报文，也没有收到 PINGREQ 心跳报文时，主动心跳超时断开客户端 TCP 连接。

::: tip

更多有关 MQTT 保活心跳机制内容请参考：

- [MQTT 协议 Keep Alive 详解](https://www.emqx.com/zh/blog/mqtt-keep-alive)

:::
