# EMQX 6.3 已知问题

## 6.3.1

| 始于版本 | 问题描述                                                     | 解决方法                                                     | 状态 |
| -------- | ------------------------------------------------------------ | ------------------------------------------------------------ | ---- |
| 6.0.0    | **配置了 `tcp_backend = socket` 且 `tcp_options.recbuf` 较小的 TCP 监听器入站流量受限**<br />TCP 监听器若使用非默认的 `tcp_backend = socket`，并同时将 `tcp_options.recbuf` 配置为小于操作系统默认接收缓冲区的值（例如 `4KB`），则无论消息大小如何，所有入站流量都会变得非常缓慢：当 `recbuf = 4KB` 时，每个连接的接收速率约为 20 KB/s，一条 256 KB 的消息需要数秒才能到达。使用默认 `tcp_backend = gen_tcp` 的监听器，以及未配置 `recbuf` 的 `socket` 监听器不受影响。 | 从使用 `tcp_backend = socket` 的监听器中删除 `tcp_options.recbuf`，或将监听器切换为 `tcp_backend = gen_tcp`。 | 计划在 6.3.2 中修复（[emqx/emqx#19190](https://github.com/emqx/emqx/pull/19190)） |
