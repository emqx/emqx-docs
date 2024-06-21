# 常见使用问题解答

## License 到期没有及时续费会发生什么？

如果您是 EMQX 企业版用户，当您的 license 到期时，每次启动节点时都会出现警告以提醒您 license 的到期情况。根据您的 license 类型，可能会有不同的限制：

- **对于面向"小型"客户或试用的 license：** 即使连接总数低于 License 中指定的限制，也将不允许创建新的 MQTT 连接。现有连接不会被断开，但如果它们断开连接，将无法重新连接。
- **对于非"小型"客户或非试用的 license：** 只要总数保持在最大限制以下，仍然允许创建新的 MQTT 连接。

如果您不确定自己的 license 类型，请与您的客户经理确认。

## 如何更新 license？

您可以使用以下命令来更新您的 EMQX 企业版 license：

```bash
./bin/emqx ctl 

    license info             # 显示 license 信息 
    license update <License> # 更新 license，<License> 为 license 字符串
```

您还可以通过 Dashboard 来更新 license。有关如何申请 licnese 以及通过 Dashboard 更新许可证，请参见[使用 EMQX 企业版 License](../deploy/license.md)。

## 为什么共享订阅时收不到保留消息？

MQTT 协议规定了服务端不能发送保留消息给共享订阅。

## 为什么使用共享订阅时消息会偶尔丢失？

在共享订阅者的连接断开，但会话仍然存在的情况下，服务端会仍然向该共享订阅者投递消息，只是消息将被暂存至对应的会话中，这会导致其余在线的共享订阅者看起来没能完整地消费到所有消息。另外，如果该共享订阅者在重连时选择创建全新会话，那么缓存在旧会话中的消息就会永久丢失。

如果确认了不存在以上情况，而消息丢失的问题仍然存在，那么可以借助 [日志追踪](../observability/tracer.md) 功能来进行进一步的排查。

## SSL/TLS 连接失败该怎么排查原因？

通常 SSL/TLS 连接握手失败时，EMQX 会在 [日志](../observability/log.md) 中输出相应的失败原因，以下是一些日志中常见的关键字及其对应的含义：

- certificate_expired

   日志中出现 `certificate_expired` 关键字，说明证书已经过期，请及时续签。

- no_suitable_cipher

   日志中出现 `no_suitable_cipher` 关键字，说明握手过程中没有找到合适的密码套件，可能原因有证书类型与密码套件不匹配、没有找到服务端和客户端同时支持的密码套件等等。

- handshake_failure

   日志中出现 `handshake_failure` 关键字，原因有很多，可能要结合客户端的报错来分析，例如，可能是客户端发现连接的服务器地址与服务器证书中的域名不匹配。

- unknown_ca

   日志中出现 `unknown_ca` 关键字，意味着证书校验失败，常见原因有遗漏了中间 CA 证书、未指定 Root CA 证书或者指定了错误的 Root CA 证书。在双向认证中我们可以根据日志中的其他信息来判断是服务端还是客户端的证书配置出错。如果是服务端证书存在问题，那么报错日志通常为：

   ```bash
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify received CLIENT ALERT: Fatal - Unknown CA\n"}}}
   ```

   `CLIENT ALERT` 表示这是来自客户端的警告信息，服务端证书未能通过客户端的检查。

   如果是客户端证书存在问题，那么报错日志通常为：

   ```bash
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify at ssl_handshake.erl:1887 generated SERVER ALERT: Fatal - Unknown CA\n"}}}
   ```

   `SERVER ALERT` 表示服务端在检查客户端证书时发现该证书无法通过认证，而客户端将收到来自服务端的警告信息。

- protocol_version

   日志中出现 `protocol_version` 关键字，说明客户端与服务器支持的 TLS 协议版本不匹配。

## MQTT 客户端连接异常断开该怎么排查？

你可以在 Shell 中执行 `emqx ctl listeners`，该命令会返回每个监听器上连接断开的统计信息，包含断开原因与断开次数。例如：

```
$ ./bin/emqx ctl listeners
tcp:default
  listen_on       : 0.0.0.0:1883
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 9
  max_conns       : infinity
  shutdown_count  : [{keepalive_timeout,1},{idle_timeout,2}]
```

以下是一些常见的断开原因：

- `keepalive_timeout`：客户端心跳超时而被 EMQX 关闭连接。
- `tcp_closed`：客户端在未发送 DISCONNECT 报文的情况下直接关闭了 TCP 连接。
- `frame_too_large`: 客户端发送的报文大小超过了最大报文限制而被 EMQX 关闭连接。
- `protocol_error`：客户端的行为不符合协议规范而被 EMQX 关闭连接，例如客户端在同一个连接内发送了多个 CONNECT 报文。
- `idle_timeout`： TCP 连接建立的 15 秒内，EMQX 未收到客户端发送的 CONNECT 报文，因此关闭了连接。

你也可以使用 [日志追踪](../observability/tracer.md) 功能追踪所有与你指定的 Client ID、IP、Topic 相关的日志，然后你可以基于这些日志来分析客户端连接断开的原因。

## 性能测试时，连接数目和吞吐量总是上不去怎么办？

在性能测试的时候，除了要选用有足够计算能力的硬件，也需要对软件运行环境做一定的调优。比如修改修改操作系统的全局最大文件句柄数，允许用户打开的文件句柄数，TCP 的 backlog 和 buffer，Erlang 虚拟机的进程数限制等等。甚至包括需要在客户端上做一定的调优以保证客户端可以有足够的连接资源。

系统的调优在不同的需求下有不同的方式，在 EMQX 的 [系统调优](../performance/tune.md) 中对用于普通场景的调优有较详细的说明。

## 遇到客户端连接、发布、订阅相关的问题时，例如无法连接、异常断连等等，我该怎么排查？

EMQX 的 Debug 日志基本记录了所有的行为和现象，通过阅读 Debug 日志我们能够知道客户端何时发起了连接，连接时指定了哪些参数，连接是否通过，被拒绝连接的原因是什么等等。但是由于 Debug 日志记录的信息过多，会带来额外的资源消耗，并且不利于我们针对单个客户端或主题进行分析。

所以 EMQX 提供了[日志追踪](../observability/tracer.md)功能，我们可以指定想要追踪的客户端或主题，EMQX 会将所有与该客户端或主题相关的 Debug 日志都输出到指定日志文件中。这样不管是自己分析调试，还是寻求社区帮助，都会方便许多。

需要注意的是，如果客户端是因为网络原因而无法连接到 EMQX 的话，日志追踪功能也是无法提供帮助的，因为此时 EMQX 尚未收到任何报文。这种情况很多时候是因为防火墙、安全组等网络配置原因导致服务器端口没有开放，这在使用云主机部署 EMQX 时尤为常见。所以除了日志追踪，我们可以通过检查端口占用、监听情况，检查网络配置等手段来排除网络方面的原因。

## 为什么会出现 Client ID 为 CENSYS 或者是其他我不认识的客户端？

CENSYS 是一款互联网探测扫描工具，它会周期性扫描 IPv4 地址空间，探测 HTTP、SSH、MQTT 等协议的默认端口。所以如果你发现有 Client ID 为 CENSYS 的或者其他未知的客户端接入了你的 MQTT Broker，这意味你目前处于相对较低的安全性保障下。以下措施可以有效帮助你避免这个问题：

1. 不要使用默认配置，例如 EMQX 用于验证 HTTP API 访问权限的 AppID 与 AppSecret 等。
2. 启用认证，可以是用户名密码认证，也可以是 JWT 认证，避免只需要知道 IP 地址就可以登录的尴尬情况。
3. 启用 TLS 双向认证，只有持有有效证书的客户端才能接入系统。
4. 启用授权，避免非法设备登录后可以获取敏感数据。
5. 配置你的防火墙，尽量关闭一些不需要的端口。
