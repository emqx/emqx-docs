# EMQX 6.3 已知问题

## 6.3.1

| 始于版本 | 问题描述                                                     | 解决方法                                                     | 状态 |
| -------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ---- |
| 6.0.0    | **配置了 `tcp_options.recbuf` 或 `sndbuf` 的 `tcp_backend = socket` TCP 监听器接收消息缓慢**<br />对于配置为 `tcp_backend = socket` 的 TCP 监听器，`tcp_options.recbuf` 与 `tcp_options.sndbuf` 会在 TCP 握手完成后被设置到每个已接受的连接上，而不是像 `gen_tcp` 后端那样设置在监听套接字上。在 Linux 上，将已建立连接的接收缓冲区缩小到握手时协商的分段大小以下，会使 EMQX 通告零接收窗口，客户端每 200 毫秒（发送端的坚持定时器）只能送达约 `recbuf` 大小的数据。当 `recbuf = 4KB` 时，一条 256 KB 的消息约需 9 秒才能到达；缓冲区越小，接收越慢。使用默认 `tcp_backend = gen_tcp` 的监听器，以及未设置 `recbuf` 和 `sndbuf`（二者没有默认值）的 `socket` 监听器不受影响。 | 从使用 `tcp_backend = socket` 的监听器中删除 `tcp_options.recbuf` 和 `tcp_options.sndbuf`，由内核决定缓冲区大小；或将监听器切换为 `tcp_backend = gen_tcp`。如果必须固定缓冲区大小，请使用 `64KB` 或更大的值。 | 计划在 6.3.2 中修复（[emqx/emqx#19174](https://github.com/emqx/emqx/pull/19174)） |
