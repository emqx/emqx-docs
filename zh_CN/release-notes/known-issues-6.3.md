# EMQX 6.3 已知问题

## 6.3.1

| 始于版本 | 问题描述                                                     | 解决方法                                                     | 状态 |
| -------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ---- |
| 6.0.0    | **配置了 `tcp_backend = socket` 且 `tcp_options.recbuf` 较小的 TCP 监听器入站流量受限**<br />TCP 监听器若使用 `tcp_backend = socket`（自 6.3.0 起为 Linux 和 macOS 上的默认值），并同时将 `tcp_options.recbuf` 配置为小于操作系统默认接收缓冲区的值（例如 `4KB`），则无论消息大小如何，所有入站流量都会变得非常缓慢：当 `recbuf = 4KB` 时，每个连接的接收速率约为 20 KB/s，一条 256 KB 的消息需要数秒才能到达。使用 `tcp_backend = gen_tcp` 的监听器，以及未配置 `recbuf` 的 `socket` 监听器不受影响。 | 从使用 `tcp_backend = socket` 的监听器中删除 `tcp_options.recbuf`，或将监听器切换为 `tcp_backend = gen_tcp`。 | 计划在 6.3.2 中修复（[emqx/emqx#19190](https://github.com/emqx/emqx/pull/19190)） |
| 6.3.0    | **使用 `tcp_backend = socket` 的 TCP 监听器在订阅端处理滞后后可能持续占满 CPU 且投递缓慢**<br />自 6.3.0 起，Linux 和 macOS 上 TCP 监听器的默认 `tcp_backend` 为 `socket`。当此类监听器上的订阅端读取速度低于消息到达速度时（例如大量 QoS 1 消息突发期间），未投递的消息会在其连接中积压。此后该连接的大部分时间都消耗在垃圾回收上，投递速度低于接收速度，积压无法消减。即使发布已经停止，CPU 仍保持在上限，投递速率降至每秒数十条。使用 `tcp_backend = gen_tcp` 的 TCP 监听器以及 SSL、WebSocket 和 QUIC 监听器不受影响。 | 将 TCP 监听器的 `tcp_backend` 设置为 `gen_tcp`，例如 `listeners.tcp.default.tcp_backend = gen_tcp`。 | 修复中（[emqx/emqx#19201](https://github.com/emqx/emqx/issues/19201)） |
