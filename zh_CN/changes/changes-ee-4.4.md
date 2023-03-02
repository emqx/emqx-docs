# 版本发布

## v4.4.15

*发布日期: 2023-03-03*

本次版本更新包含了 16 个增强和 21 个修复。比较重要的功能增强有：

- 升级 EMQX 的 MongoDB 客户端库，支持 MongoDB 5.1 及以上版本。
- Dashboard 支持 HAProxy 的 Proxy Protocol。
- 发布 Ubuntu 22.04 安装包。

### 增强

- 规则引擎支持 Kafka headers [#1727](https://github.com/emqx/emqx-enterprise/pull/1727)。

- 支持使用规则引擎写入数据到 IoTDB [#1691](https://github.com/emqx/emqx-enterprise/pull/1691)。

- JT/T 808 兼容非标准的位置上报报文。在用户使用保留 ID 来上报位置时 EMQX 会将其按 Base64 的格式透传，而不是断开客户端连接 [#1707](https://github.com/emqx/emqx-enterprise/pull/1707)。

- 在 emqx_modules 应用程序启动时，仅仅在本地创建 EMQX 模块 [#1712](https://github.com/emqx/emqx-enterprise/pull/1712)。
  在此之前，当 emqx_modules 应用程序启动时，我们向所有节点发送 RPC 以创建或重新创建模块，因此最终我们在所有节点上创建了 N^2 次模块（每个节点上创建N次）。

- 改进当 DynamoDB 动作找不到 `hash_key` 或者 `range_key` 时的错误日志打印 [#1721](https://github.com/emqx/emqx-enterprise/pull/1721)。

- HStreamDB 驱动程序更新以支持 HStreamDB ~> 0.12.0 [#1674](https://github.com/emqx/emqx-enterprise/pull/1674)。

- 编解码插件将作为规则引擎的可选功能默认启用 [#1737](https://github.com/emqx/emqx-enterprise/pull/1737)。

- 添加了 TLS 连接 HStreamDB 支持 [#1756](https://github.com/emqx/emqx-enterprise/pull/1756)

- MongoDB 库已升级至支持 MongoDB 5.1 及以上版本 [#9707](https://github.com/emqx/emqx/pull/9707)。

- 现在 Dashboard 支持 HAProxy 的 Proxy Protocol 了 [9803](https://github.com/emqx/emqx/pull/9803)。

- 发布 Ubuntu 22.04 安装包 [#9831](https://github.com/emqx/emqx/pull/9831)。

- 增强 `封禁` 和 `延迟消息` 这两个功能的集成性 [#9790](https://github.com/emqx/emqx/pull/9790)。
  现在发送延迟消息前，会先检查消息的来源客户端是否被封禁了，如果是，这条延迟消息将会被忽略。

- 增强 `保留消息` 的安全性 [#9790](https://github.com/emqx/emqx/pull/9790)。
  现在投递保留消息前，会先过滤掉来源客户端被封禁了的那些消息。

- 现在客户端通过 `clientid` 被封禁时将会踢掉对应的会话 [#9904](https://github.com/emqx/emqx/pull/9904)。

- 为认证和授权添加了更多调试日志 [#9943](https://github.com/emqx/emqx/pull/9943)。

- 将统计数据 `live_connections.count` 和 `live_connections.max` 公开给 Prometheus [#9929](https://github.com/emqx/emqx/pull/9929)。

### 修复

- 修复 `tlsv1.3` 在模块（Stomp 网关、GB/T 32960 网关、JT/T808 网关、扩展协议、TCP 网关、MQTT 订阅者）的 `tls_versions` 标签中缺失的问题 [#1732](https://github.com/emqx/emqx-enterprise/pull/1732)。

- 修复使用 Redis 离线消息功能时，EMQX 以相反顺序向客户端发送离线消息的问题 [#1739](https://github.com/emqx/emqx-enterprise/pull/1739)。

- 修复 EMQX 重启之后，初始化失败的模块被禁用的问题 [#1738](https://github.com/emqx/emqx-enterprise/pull/1738)。

- 修复资源和动作里的一些描述错误 [#1729](https://github.com/emqx/emqx-enterprise/pull/1729)。

- 修复热升级后，Oracle 资源没有办法自动重连的问题 [#1715](https://github.com/emqx/emqx-enterprise/pull/1715)。

- 修复通过规则引擎发送消息到 RocketMQ 集群失败的问题 [#1724](https://github.com/emqx/emqx-enterprise/pull/1724)。

- 在集群中使用 API 创建已存在的监听器时返回失败 [#1598](https://github.com/emqx/emqx-enterprise/pull/1598)。

- 在 `资源/模块/编解码` 删除时清理其文件目录以防止文件泄露 [#1754](https://github.com/emqx/emqx-enterprise/pull/1754)。

- 修复使用 `消息重发布` 动作转发带 User-Property 的 MQTT 消息时出错的问题 [#9942](https://github.com/emqx/emqx/pull/9942)。

- 修复资源、动作以及模块里的一些描述错误 [#9931](https://github.com/emqx/emqx/pull/9931)。

- 修复请求 JWKS 服务失败的时候，没有日志打印的问题 [#9931](https://github.com/emqx/emqx/pull/9931)。

- 使用 HTTP API `GET /api/v4/clients?_page=2&_limit=20` 请求客户端列表时，请求发送到不同的 EMQX 节点，返回的客户端列表可能不一致 [#9926](https://github.com/emqx/emqx/pull/9926)。

- 修复版本热升级之后，新的 MQTT TLS 连接建立失败的问题 [#9810](https://github.com/emqx/emqx/pull/9810)。
  详情见：[emqx/esockd#170](https://github.com/emqx/esockd/pull/170)

- 修复 MQTT 报文的日志打印格式的问题 [#9858](https://github.com/emqx/emqx/pull/9858)。
  在此修复之前，固定报文头的标志位（DUP）和后面的可变报文头的字段（ClientId）之间漏掉了一个逗号做分隔：
  ```
  2023-01-29T13:40:36.567692+08:00 [debug] 127.0.0.1:50393 [MQTT] RECV CONNECT(Q0, R0, D0ClientId=test_client, ... Password=undefined)
  ```

- 修复 CoAP 网关在收到负载均衡的心跳检查报文时产生的崩溃日志 [#9869](https://github.com/emqx/emqx/pull/9869)。

- 修复会话关闭后，其持有的排他订阅主题没有被释放的问题 [#9868](https://github.com/emqx/emqx/pull/9868)。

- 修复 Websocket 连接中断时日志报 `{case_clause,{error,closed}}` 错误的问题 [emqx/cowboy#8](https://github.com/emqx/cowboy/pull/8)。

- 修复某些情况下，重启 EMQX 后规则无法自动启用的问题 [#9911](https://github.com/emqx/emqx/pull/9911)。

- 修复停止 EMQX 的时候，日志出现 `{badarg,[{ets,lookup,[gproc,{shared, ...` 错误的问题 [#9919](https://github.com/emqx/emqx/pull/9919)。

- 修复当客户端连接禁用 keepalive 时, 通过 HTTP API 更新其 keepalive 会崩溃的问题 [#9933](https://github.com/emqx/emqx/pull/9933)。

- 在 `资源` 删除时清理其文件目录以防止文件泄露 [#10039](https://github.com/emqx/emqx/pull/10039)。

## e4.4.14

*发布日期: 2023-01-06*

### 增强

- 通过 API 添加、修改 Dashboard 用户时，增加对密码复杂度的要求。现在密码必须包含字母、数字以及特殊字符中的至少 2 种，并且长度范围必须是 8~64 个字符。

### 修复

- 修复通过 API 添加或者导入 Dashboard 用户时，对密码进行了错误的校验，导致复杂密码添加失败的问题。

- 修复 boostrap_apps_file 文件更新后没有同步更新至数据库,导致文件中新加 apps 未生效。

## e4.4.13

*发布日期: 2023-01-03*

### 修复

- 修复了测试 GCP PubSub 可能泄露内存的问题，以及其 JWT 令牌第二次刷新失败的问题。 [#9640](https://github.com/emqx/emqx/pull/9640)

## e4.4.12

*发布日期: 2022-12-29*

该版本包含了一个振奋人心的新功能：集群负载重平衡。
新增的 emqx_ctl rebalance 命令可以很好的支持如下两个通用场景：
- 如果大多数客户端都保持长连接，新加入集群，或新启重过的节点在很长一段时间内都会处于低负载状态。
- 因为维护需要重启节点的话，直接关闭服务会导致所有连接到该节点的客户端在短时间内重连，在重连过程中这会增加集群负载，影响稳定性。而且在该节点的持久会话也会丢失。

有了这个新功能之后，可以使用 rebalance 命令来触发客户端重连以达到负载均衡的目的。使用 --evacuation 选项时，则可以确保在重启服务前所有的客户端都已经迁移到了其他节点。
请参考 [集群负载重平衡](../advanced/rebalancing.md) 了解更多详情。

### 增强

- 为主题重写模块增加主题合法性检查，带有通配符的目标主题不允许被发布 [#1590](https://github.com/emqx/emqx-enterprise/pull/1590)。

- TDEngine 资源同时支持 TDEngine 2.x 和 3.x 两个版本的 API 返回格式 [emqx/tdengine-client-erl#7](https://github.com/emqx/tdengine-client-erl/pull/7)。
  在 HTTP 的返回中，TDEngine 2.x 使用 `status` 字段来代表请求成功或者失败，而 TDEngine 3.x 改为使用 `code` 字段。

- TDEngine 资源支持批量发送数据到 [TDEngine 子表](https://docs.taosdata.com/concept/#子表subtable) [#1593](https://github.com/emqx/emqx-enterprise/pull/1593)。

- 在启用规则的时候，Clickhouse 离线消息动作打印了一行 info 级别的日志：`Destroyed .. Successfully` [#1594](https://github.com/emqx/emqx-enterprise/pull/1594)。

- 现在即使对应的资源没有就绪的情况下，我们也能创建规则 [#1620](https://github.com/emqx/emqx-enterprise/pull/1620)。
  在此改动之前，如果资源没有连接好，我们就没办法创建规则。在此改动之后可以创建了，但是新创建的规则将处于 `禁用` 状态。

- 避免离线消息被重复删除 [#1522](https://github.com/emqx/emqx-enterprise/pull/1522)。
  当订阅者回复消息 PUBACK(Qos1) 或消息 PUBREC(Qos2) 时，EMQX 会将外部数据库中的这条离线消息删除。
  但当离线消息和保留消息功能同时启用时，一条 `retain = true` 的消息会被冗余存储两次 (Retainer 与外部数据库)。
  那么重复的 PUBACK 或 PUBREC 将触发两次删除外部数据库中离线消息的行为，并且会使 Rule-SQL 执行成功而使 action-metrics 增加。
  在多数情况下这不会产生任何异常或错误，仅有个别数据库在第二次删除时会报告要删除的消息不存在。
  此更改后，将避免冗余的离线消息删除操作。

- 用户可以在 EMQX Enterprise Helm Chart 中自定义 service 资源的 `externalTrafficPolicy` [#1638](https://github.com/emqx/emqx-enterprise/pull/1638)。

- 当控制台创建新用户时，密码格式为 `^[A-Za-z0-9]+[A-Za-z0-9-_]*$` [#1643](https://github.com/emqx/emqx-enterprise/pull/1643)。

### 修复

- 持久会话的 MQTT 客户端重新连接 emqx 之后，未被确认过的 QoS1/QoS2 消息不再周期性重发 [#1623](https://github.com/emqx/emqx-enterprise/pull/1623)。
  `zone.<zone-name>.retry_interval` 配置指定了没有被确认过的 QoS1/QoS2 消息的重发间隔，(默认为 30s)。在这个修复之前，
  当持久会话的 MQTT 客户端重新连接 emqx 之后，emqx 会将队列中缓存的未被确认过的消息重发一次，但是不会按配置的时间间隔重试。

- 持久会话的 MQTT 客户端断连之后，已经过期的 'awaiting_rel' 队列没有清除 [#1602](https://github.com/emqx/emqx-enterprise/pull/1602)。
  在这个改动之前，在客户端重连并且发布 QoS2 消息的时候，如果 'awaiting_rel' 队列已满，此客户端会被服务器以
  RC_RECEIVE_MAXIMUM_EXCEEDED(0x93) 错误码断开连接，即使这时候 'awaiting_rel' 队列里面的报文 ID 已经过期了。

- RocketMQ 资源的认证功能不能正常工作 [#1561](https://github.com/emqx/emqx-enterprise/pull/1561)。
  在这个改动里，我们把 `access_key`, `secret_key` 以及 `security_token` 三个配置项字段
  从 `data_to_rocket` 动作挪到了 `bridge_rocket` 资源的配置中。并且我们为阿里云的 RocketMQ
  服务新加了一个 `namespace` 字段。

- 为 Kafka 动作参数增加检查，确保 Segment Bytes 不会超过 Max Bytes [#1607](https://github.com/emqx/emqx-enterprise/pull/1607)。

- 为 Pulsar 动作参数增加检查，确保 Segment Bytes 不会超过 Max Bytes [#1625](https://github.com/emqx/emqx-enterprise/pull/1625)。

- 解决通过 emqx oracle 资源发送数据时，报 "ORA-01000: maximum open cursors exceeded" 错误的问题 [#1556](https://github.com/emqx/emqx-enterprise/pull/1556)。

- 修复了 EMQX Enterprise Helm Chart 部署的一些问题 [#1621](https://github.com/emqx/emqx-enterprise/pull/1621)
  - 修复了 EMQX Enterprise Helm Chart 部署时出现 `Discovery error: no such service` 错误，导致集群节点发现异常。
  - 修复了 EMQX Enterprise Helm Chart 无法配置 value 为 JSON 类型的 EMQX Enterprise 配置项。

- 修正了一个问题，即导入备份配置后，配置不会在集群的所有节点上重新加载 [#1612](https://github.com/emqx/emqx-enterprise/pull/1612)。

- 修正了一个问题，即当从一个不在其中的节点下载备份配置文件时，HTTP API将无法下载该文件 [#1612](https://github.com/emqx/emqx-enterprise/pull/1612)。

- 为 Kafka 资源的 SSL 连接配置增加 `SNI` 字段 [#1642](https://github.com/emqx/emqx-enterprise/pull/1642)。

- 修复了 MongoDB 资源在启用认证的情况下，连接过程很慢的问题 [#1655](https://github.com/emqx/emqx-enterprise/pull/1655)。

- 修复了在版本热升级之后，EMQX 偶尔会出现资源已断开的告警，并且告警无法自动清除的问题 [#1652](https://github.com/emqx/emqx-enterprise/pull/1652)。

- 修复仪表盘中的连接统计：将疏散后的客户端标记为断开连接 [#1680](https://github.com/emqx/emqx-enterprise/pull/1680)。


## e4.4.11

*发布日期: 2022-11-26*

本次版本更新包含了 23 个增强和 21 个修复。
在这些增强中，下面几个新功能值重点介绍：

- 规则引擎的数据桥接新增 Google PubSub 的集成。
- 新增 OCSP (Online Certificate Status Protocol) Stapling。
- 新增 CRL (Certificate Revocation List) cache。
- 规则引擎的 Pulsar 数据桥接支持缓存。
- OTP 从 24.1.5-3 升级到了 24.3.4.2-1。
- 新增了 客户端别名的支持，使得定制化认证和授权更加容易实现。

该版本仍支持从老版本的 e4.4 热升级上来，
但需要注意的是：如果需要在热升级后使用新版本提供的功能（例如 OCSP Stapling 和 CRL）
那么节点重启（配合配置更新）仍然不能避免。

### 增强

- 升级了 Pulsar 客户端到 0.7.0。
  升级后，Pulsar 可以配置支持消息缓存，当 Pulsar 服务器暂时下线时
  ，EMQX 可以把消息缓存一段（可以配置）时间，等恢复之后继续发送。

  同时也修复了 Pulsar 客户端连接发生异常后的日志，连接参数中的密码和 JWT 等敏感
  信息会在日志中进行隐藏。

  **注意**: 如果 EMQX 从原先的版本升级上来，然后再降级回去，可能会导致一些向 Pulsar
  发送消息的飞行窗口中的同步请求超时，导致影响这部分客户端异常下线。

- 为 GCP PubSub 添加了新的规则引擎桥和相应的规则操作
  [#1523](https://github.com/emqx/emqx-enterprise/pull/1523)。

- 增加了对 OCSP Stapling 和 CRL 检查/缓存的热配置支持 [#1528](https://github.com/emqx/emqx-enterprise/pull/1528)。

- 支持在规则引擎的 MongoDB 动作的 `Collection` 字段里使用 `${var}` 格式的占位符 [#1503](https://github.com/emqx/emqx-enterprise/pull/1503)。

- 添加针对规则引擎中的 InfluxDB 资源的 `host` 字段的格式检查；主机字段应该是不包括 scheme 和端口的 IP/域名 [#1426](https://github.com/emqx/emqx-enterprise/pull/1426)。
  相关的文档更新请查看 [emqx/emqx-docs#1368](https://github.com/emqx/emqx-docs/pull/1368)。

- OTP 升级: 从 24.1.5-3 至 24.3.4.2-1 [#9265](https://github.com/emqx/emqx/pull/9265)。
  重要更新:
    - Erlang/OTP [SSL库漏洞修复](https://nvd.nist.gov/vuln/detail/CVE-2022-37026)
    - 增加了对 OCSP (Online Certificate Status Protocol) Stapling 的支持
    - 增加了 CRL（证书吊销列表）缓存的自动刷新功能

- 增加了 OCSP stapling 和 CRL 缓存 [#9297](https://github.com/emqx/emqx/pull/9297)。

- 增加了可定制的 clientid 或 username 别名的回调模块 [#9297](https://github.com/emqx/emqx/pull/9297)。
  有了这个回调模块后，可以简单实现一个 Erlang 的回调函数用来给客户端增加别名，然后在认证和授权规则的占位符中使用这些别名
  （`%cida` 用作 clientid 别名，`%cna` 用作 用户名别名）。

- 增加了可定制的认证回调模块 [#9297](https://github.com/emqx/emqx/pull/9297)。
  对于一些简单的认证检查，不需要去实现一个完整的认证插件。

- 为规则引擎增加了一个 JWT 令牌管理，用于在规则引擎动作中创建和刷新 JWT 令牌 [#9241](https://github.com/emqx/emqx/pull/9241)。
  该功能现在仅用于 EMQX 企业版的 Google PubSub 集成中。
  后续会用于 webhook 集成的 JWT 认证。

- 检查监听器的 `tls_versions` 配置值是 `tlsv1`，`tlsv1.1`，`tlsv1.2`，`tlsv1.3` 中的一个或多个组合 [#9260](https://github.com/emqx/emqx/pull/9260)。

- 删除 Dashboard 监听器失败时日志中的无用信息 [#9260](https://github.com/emqx/emqx/pull/9260).

- 当 CoAP 网关给设备投递消息并收到设备发来的确认之后，回调 `'message.acked'` 钩子 [#9264](https://github.com/emqx/emqx/pull/9264)。
  有了这个改动，CoAP 网关可以配合 EMQX (企业版)的离线消息缓存功能，让 CoAP 设备重新上线之后，从数据库读取其离线状态下错过的消息。

- 支持在规则引擎的 Webhook 动作的 HTTP Headers 里使用 `${var}` 格式的占位符 [#9239](https://github.com/emqx/emqx/pull/9239)。

- 在 emqx 启动时，异步地刷新资源和规则 [#9199](https://github.com/emqx/emqx/pull/9199)。
  这个改动是为了避免因为一些资源连接建立过慢，而导致启动时间过长。

- 订阅时，如果 ACL 检查不通过，打印一个警告日志 [#9124](https://github.com/emqx/emqx/pull/9124)。
  该行为的改变主要是为了跟发布失败时的行为保持一致。

- 基于 JWT 的 ACL 支持 `all` 动作，指定同时适用于 `pub` 和 `sub` 两个动作的规则列表 [#9044](https://github.com/emqx/emqx/pull/9044)。

- 增强包含敏感数据的日志的安全性 [#9189](https://github.com/emqx/emqx/pull/9189)。
  如果日志中包含敏感关键词，例如 `password`，那么关联的数据回被模糊化处理，替换成 `******`。

- 增强 ACL 模块中的日志安全性，敏感数据将被模糊化 [#9242](https://github.com/emqx/emqx/pull/9242)。

- 增加 `management.bootstrap_apps_file` 配置，可以让 EMQX 初始化数据库时，从该文件批量导入一些 APP / Secret [#9273](https://github.com/emqx/emqx/pull/9273)。

- 增加了固化认证和 ACL 模块调用顺序的配置 [#9283](https://github.com/emqx/emqx/pull/9283)。
  这两个新的全局配置名称为 `auth_order` 和 `acl_order`。
  当有多个认证或 ACL 插件（或模块）开启时，没有该配置的话，模块调用的顺序取决于它们的启动顺序。
  例如，如果一个插件（或模块）在系统启动之后单独重启了，那么它就有可能排到其他插件（或模块）的后面去。
  有了这个配置之后，用户可以使用用逗号分隔的插件（或模块）的名字（或别名）来固化他们被调用的顺序。
  例如，`acl_order = jwt,http`，可以用于保证 `jwt` 这个模块总是排在 `http` 的前面，
  也就是说，在对客户端进行 ACL 检查时，如果 JWT 不存在（或者没有定义 ACL），那么回退到使用 HTTP。

- 为更多类型的 `client.disconnected` 事件（计数器触发）提供可配置项 [#9267](https://github.com/emqx/emqx/pull/9267)。
  此前，`client.disconnected` 事件及计数器仅会在客户端正常断开连接或客户端被系统管理员踢出时触发，
  但不会在旧 session 被新连接废弃时 (clean_session = true) ，或旧 session 被新连接接管时 (clean_session = false) 被触发。
  可将 `broker.client_disconnect_discarded` 和 `broker.client_disconnect_takovered` 选项设置为 `on` 来启用此场景下的客户端断连事件。

- 规则引擎资源创建失败后，第一次重试前增加一个延迟 [#9313](https://github.com/emqx/emqx/pull/9313)。
  在此之前，重试的延迟发生在重试失败之后。

### 修复

- 解决从 e4.4.5 以及更早的版本升级 emqx 的时候，Kafka 资源的认证类型从 `PLAIN` 变成了 `NONE` 的错误 [#1509](https://github.com/emqx/emqx-enterprise/pull/1509)。

- 修复 JWT 认证插件的一个热升级问题 [#1558](https://github.com/emqx/emqx-enterprise/pull/1558)。
  当从 e4.4.3 或更早的热升级上来时，EMQX 内部一个 Key 管理进程会需要重启，在重启过程中的 JWT 认证请求可能会失败。

- 修正了在 Kafka Consumer 中选择 `reset_by_subscriber` 偏移重置策略的选项 [#1463](https://github.com/emqx/emqx-enterprise/pull/1463)。

- 修复了热配置中的 `tls_versions` 配置项遗漏了 `tlsv1.3` 选项的问题 [#1532](https://github.com/emqx/emqx-enterprise/pull/1532)。

- 修复了 SQL Server 资源中，无法在 `server` 字段里使用除 `1433` 之外的端口的问题 [#1464](https://github.com/emqx/emqx-enterprise/pull/1464)。

- Schema-Registry 的 API 在 HTTP 请求 URI 中支持百分号编码的 `name` [#1497](https://github.com/emqx/emqx-enterprise/issues/1497)。
  注意在创建 Schema 时，`POST /api/v4/schemas` 请求中的 `name` 字段不应使用百分号编码，因为这是一个 JSON 字段。

- 修复 JWT 认证插件的一个热升级问题 [#1554](https://github.com/emqx/emqx-enterprise/pull/1554)。
  当从 e4.3.9 或更早的热升级上来时，EMQX 内部一个 Key 管理进程会需要重启，在重启过程中的 JWT 认证请求可能会失败。

- 修复日志追踪模块没开启时，GET Trace 列表接口报错的问题。[#9156](https://github.com/emqx/emqx/pull/9156)

- 修复创建追踪日志时偶尔会报`end_at time has already passed`错误，导致创建失败。[#9156](https://github.com/emqx/emqx/pull/9156)

- 修复若上传的备份文件名中包含非 ASCII 字符，`GET /data/export` HTTP 接口返回 500 错误 [#9224](https://github.com/emqx/emqx/pull/9224)。

- 改进规则的 "最大执行速度" 的计数，只保留小数点之后 2 位 [#9185](https://github.com/emqx/emqx/pull/9185)。
  避免在 dashboard 上展示类似这样的浮点数：`0.30000000000000004`。

- 修复在尝试连接 MongoDB 数据库过程中，如果认证失败会不停打印错误日志的问题 [#9184](https://github.com/emqx/emqx/pull/9184)。

- 修复 emqx-sn 插件在“空闲”状态下收到消息发布请求时可能崩溃的情况 [#9024](https://github.com/emqx/emqx/pull/9024)。

- 限速 “Pause due to rate limit” 的日志级别从原先的 `warning` 降级到 `notice` [#9134](https://github.com/emqx/emqx/pull/9134)。

- 保留老的 `emqx_auth_jwt` 模块的接口函数，保障热升级之前添加的回调函数在热升级之后也不会失效 [#9144](https://github.com/emqx/emqx/pull/9144)。

- 修正了 `/status` API 的响应状态代码 [#9210](https://github.com/emqx/emqx/pull/9210)。
  在修复之前，它总是返回 `200`，即使 EMQX 应用程序没有运行。 现在它在这种情况下返回 `503`。

- 修复规则引擎的消息事件编码失败 [#9226](https://github.com/emqx/emqx/pull/9226)。
  带消息的规则引擎事件，例如 `$events/message_delivered` 和 `$events/message_dropped`,
  如果消息事件是共享订阅产生的，在编码（到 JSON 格式）过程中会失败。
  影响到的版本：`v4.3.21`, `v4.4.10`, `e4.3.16` 和 `e4.4.10`。

- 使规则引擎 API 在 HTTP 请求路径中支持百分号编码的 `rule_id` 及 `resource_id` [#9190](https://github.com/emqx/emqx/pull/9190)。
  注意在创建规则或资源时，HTTP body 中的 `id` 字段仍为字面值，而不是编码之后的值。
  详情请参考 [创建规则](https://docs.emqx.com/zh/enterprise/v4.4/advanced/http-api.html#post-api-v4-rules) 和 [创建资源](https://docs.emqx.com/zh/enterprise/v4.4/advanced/http-api.html#post-api-v4-resources)。

- 修复调用 'DELETE /alarms/deactivated' 只在单个节点上生效的问题，现在将会删除所有节点上的非活跃警告 [#9280](https://github.com/emqx/emqx/pull/9280)。

- 在进行消息重发布或桥接消息到其他 mqtt broker 时，检查 topic 合法性，确定其不带有主题通配符 [#9291](https://github.com/emqx/emqx/pull/9291)。

- 关闭管理端口（默认为8081）上对 HTTP API `api/v4/emqx_prometheus` 的认证，Prometheus 对时序数据抓取不在需要配置认证 [#9294](https://github.com/emqx/emqx/pull/9294)。

## e4.4.10

*发布日期: 2022-10-14*

### 增强

- TLS 监听器内存使用量优化 [#9005](https://github.com/emqx/emqx/pull/9005)。
  新增了配置项 `listener.ssl.$NAME.hibernate_after` (默认不开启），该配置生效后，TLS 连接进程在空闲一段时间后会进入休眠。
  休眠可以较大程度减少内存占用，但是代价是 CPU 利用率会增加。
  测试中使用 '5s' （即 5 秒空闲后休眠）可以减少 50% 的内存使用量。

- 默认 TLS Socket 缓存大小设置为 4KB [#9007](https://github.com/emqx/emqx/pull/9007)
  这样可以有效的避免某些环境中操作系统提供的默认缓存过大而导致 TLS 连接内存使用量大的问题。

- 关闭对 HTTP API `api/v4/emqx_prometheus` 的认证 [#8955](https://github.com/emqx/emqx/pull/8955)。
  Prometheus 对时序数据抓取不在需要配置认证。

- 更严格的 flapping 检测，认证失败等原因也会进行计数 [#9045](https://github.com/emqx/emqx/pull/9045)。

- 当共享订阅的会话终结时候，把缓存的 QoS1 和 QoS2 的消息向订阅组的其他成员进行转发 [#9094](https://github.com/emqx/emqx/pull/9094)。
  在这个增强之前，可以通过设置配置项 `broker.shared_dispatch_ack_enabled` 为 `true` 来防止在共享订阅的会话中缓存消息,
  但是这种转发因为需要对每个消息进行应答，会增加额外的系统开销。

- 修复延迟发布可能因为修改系统时间而导致的延迟等问题 [#8908](https://github.com/emqx/emqx/pull/8908)。

### 修复

- 修复了 `load_modules` 被新加入集群的节点重置的问题。
  在之前的版本，如果 `load_modules` 中的默认模块状态改变了，例如从控制台停用，
  如果一个带默认配置的节点新加入集群，会导致其他节点同步默认节点的状态，导致原先停用的模块
  又被开启。修复后，新节点会尝试从现有集群中最老的那个节点复制一份 `loaded_modules` 文件
  然后再加入集群。

- 自动订阅数据源是外部数据库的情况下，例如 redis，在本次修复前，如果 QoS 不在有效值范围内,
  订阅仍然会成功，例如订阅了 QoS = -1，但后续消息发布到这个客户端时，会发生错误。
  本次修复尝试将合法范围之外的 QoS 值强制转换到 [0-2] 范围内。

- 修复订阅成功的计数问题。本次修复前，自动订阅数据源是 Redis 时，查询超时后订阅失败仍然被计数为成功。

- 修复规则引擎对 QoS 0 离线消息保存的计数问题。
  QoS 0 的消息不会被保存到后端数据库中而是会被丢弃。
  在此次修复前，消息虽然被丢弃了，但是保存成功的计数器仍然计数，已修复为失败计数。

- 规则引擎增加 SSL 连接 redis 时的 “是否校验服务器证书” 选项。

- 修复 Redis 资源探活逻辑。本次修复前，Redis 资源连接成功即被认为可用。
  本次修复方法是尝试做一个 PING 查询来检查资源是否可用。

- 修复 Redis 集群资源不可用产生大量日志的问题。

- 修复 Redis 集群资源内部 ID 冲突的问题。该冲突可能导致正在使用的资源被删除。

- 所有的密码字段都使用敏感信息遮盖输入框。

- 修复慢订阅追踪模块在 `stats_type` 为 `internal` 或 `response` 时，计算单位使用错误 [#8981](https://github.com/emqx/emqx/pull/8981)。

- 修复 HTTP 客户端库启用 SSL 后 Socket 可能会进入 passive 状态 [#9145](https://github.com/emqx/emqx/pull/9145)。

- 隐藏 redis 客户端错误日志中的密码参数 [#9071](https://github.com/emqx/emqx/pull/9071)。
  也包含了如下一些改进：
  - 修复一些其他可能导致密码泄漏的隐患 [eredis#19](https://github.com/emqx/eredis/pull/19)。
  - 修复了 eredis_cluster 中连接池命名冲突的问题 [eredis_cluster#22](https://github.com/emqx/eredis_cluster/pull/22)
    同时对这个库也进行了密码泄漏隐患对修复。

- 修复共享订阅消息转发逻辑 [#9094](https://github.com/emqx/emqx/pull/9094)。
  - QoS2 飞行窗口消息丢弃的日志过度打印问题
  - 对于通配符订阅的消息，会话中缓存的消息重发时，因为使用了发布主题（而非通配符主题）来进行重发，
    导致无法匹配到 共享订阅组内的其他成员。

- 修复共享订阅 `sticky` 策略下，客户端取消订阅后仍然可以收到消息的问题 [#9119](https://github.com/emqx/emqx/pull/9119)。
  这之前的版本中，仅处理了客户端会话终结，而没有处理取消订阅。

- 修复共享订阅 `sticky` 策略在某些情况下退化成 `random` 策略的问题 [#9122](https://github.com/emqx/emqx/pull/9122)。
  集群环境下，在之前的版本中, 共享订阅组成员的选择在最开始时会随机选取，最终会选中在本节点连接的客户端，并开始粘性转发。
  如果所有的订阅客户端都不在本节点，那么粘性策略就会退化成随机。
  这个修复后，粘性策略将应用于第一个随机选取的客户端，不论该客户端是不是在本节点。

- 修复规则引擎的备选（fallback）动作的计数重置失败的问题 [#9125](https://github.com/emqx/emqx/pull/9125)。

## e4.4.9

*发布日期: 2022-09-17*

### 增强

- JWT 认证中的 `exp`、`nbf` 和 `iat` 声明支持非整数时间戳

### 修复

- 修复规则引擎的更新行为，此前会尝试初始化已禁用的规则
- 修复操作系统时间更改导致延迟发布不准确的问题
- 修复 Dashboard 监听器绑定 IP 地址不生效的问题
- 修复 `shared_dispatch_ack_enabled` 配置为 true 时共享订阅可能陷入死循环的问题
- 修复规则引擎 SQL 对空值进行主题匹配时崩溃的问题

## e4.4.8

*发布日期: 2022-08-29*

### 增强

- 新增 `GET /trace/:name/detail` API 以查看日志追踪文件信息
- 改进 LwM2M 报文解析失败时的日志
- 改进规则引擎错误日志，动作执行失败时的日志中将包含规则 ID
- 改进 `loaded_modules` 和 `loaded_plugins` 文件不存在时的提醒日志
- Dashboard 新增修改默认密码的引导
- 改进 Protobuf Schema 文件的导入性能

### 修复

- 修复 `client.disconnected` 在某些情况下不会触发的问题
- 修复 JWKS 服务未及时就绪时可能导致 JWK 认证模块后续后无法启动的问题
- 修复通过环境变量设置监听器端口将导致无法停止任一监听器的问题
- 修复内置数据库认证未区分客户端 ID 和用户名的认证数据的分页统计的问题
- 修复热升级后模块状态会在 EMQX 重启后重置的问题
- 修复 Redis 驱动进程泄漏的问题
- 修复规则引擎 MQTT 桥接至 AWS IOT 连接超时的问题
- 修复监听器未就绪时 `GET /listener` 请求崩溃的问题
- 修复 e4.4.1 版本后规则引擎 SQL 中任意变量与空值比较总是返回 false 的问题
- 修复 ExHook 的执行优先级高于规则引擎时，被 ExHook Message Hook 过滤的主题将无法触发规则引擎的问题
- 修复 TDEngine 的写入请求可能因为对端关闭网络连接而失败的问题
- 修复 MQTT-SN 模块除监听器以外的配置都不会生效的问题
- 修复 ExHook 管理进程因 supervisor 关闭超时而被强制杀死的问题
- 修复 ExProto `client.connect` 钩子中 Client ID 参数未定义的问题
- 修复客户端被踢除时 ExProto 不会触发断开连接事件的问题

## e4.4.7

*发布日期: 2022-08-11*

### 重要变更

- 从 4.4.7 版本开始，我们将不再为 macOS 10 提供安装包

### 增强

- 允许配置连接进程在 TLS 握手完成后进行垃圾回收以减少内存占用，这可以使每个 SSL 连接减少大约 35% 的内存消耗，但相应地会增加 CPU 的消耗
- 允许配置 TLS 握手日志的日志等级以便查看详细的握手过程

### 修复

- 修复 ConfigMap 中取消挂载 `loaded_modules` 文件后，通过 Helm Chart 部署时 EMQX 无法启动的问题

## e4.4.6

*发布日期: 2022-07-29*

### 增强

- 规则引擎支持启用了 ACL 的 RocketMQ
- 支持对规则引擎中的规则进行搜索和分页
- 规则引擎中的 Kafka 现已支持 SASL/SCRAM 认证和 SASL/GSSAPI 认证，注意使用 SASL/GSSAPI 认证前需要安装 `cyrus-sasl-gssapi` 依赖
- 提供 CLI `./bin/emqx check_conf` 以主动检查配置是否正确
- 优化规则引擎中 TDEngine 的写入性能
- 支持在 Dashboard 上清除历史告警
- 优化共享订阅性能
- 规则引擎写入数据到 TDEngine 的动作中新增 `db_name` 字段以改善对超级表的支持

### 修复

- 修复规则引擎写入 TDEngine 时动作计数错误的问题
- 修复规则引擎写入 HStreamDB 时进程池大小设置不生效的问题
- 修复启用 GB/T 32960 插件后查询订阅列表报错的问题
- 修复启用热配置的情况下将 4.2 的备份恢复到 4.4 上时配置项不兼容的问题
- 修复热升级后一旦卸载了老版本 EMQX 将无法再次启动的问题
- 修复多语言协议扩展中对 UDP 客户端的保活检查错误导致客户端不会过期的问题
- 修复多语言协议扩展中客户端信息没有及时更新的问题
- 修复热升级到 e4.4.4 及更高版本以后运行时更新 License 不生效的问题
- 修复客户端指定 Clean Session 为 false 重连时，飞行窗口中的共享订阅消息会被尝试重新派发给旧会话进程的问题
- 修复新节点加入集群后没有使用集群 License 的问题
- 修复 `emqx_lua_hook` 插件无法取消消息发布的问题

## e4.4.5

*发布日期: 2022-06-30*

### 增强

- 规则引擎支持将数据持久化到 HStreamDB
- 规则引擎消息重发布动作中的 QoS 和保留消息标识现在可以使用占位符
- 支持排他订阅，即一个主题只允许存在一个订阅者
- 支持通过 CLI 一键更新集群 License
- 现在 Dashboard 和管理 API 的 HTTPS 监听器可以使用受密码保护的私钥文件，提供了 `key_password` 配置项
- 支持在主题重写规则中使用占位符 `%u` 和 `%c`
- 支持在消息发布的 API 请求中设置 MQTT 5.0 的 Properties，例如消息过期间隔、响应主题等
- 优化规则引擎资源创建时的 UI，例如折叠部分不常用的选项等
- 为 ExHook 底层的 gRPC 连接开放了 KeepAlive、TCP_NODELAY、SO_RCVBUF 和 SO_SNDBUF 共 4 个与 TCP 相关的配置项

### 修复

- 修复 Linux 系统中内存计算不准确的问题，并改为计算当前系统的内存占用，而不是 EMQX 的内存占用
- 修复 ExHook 在客户端重连时旧的断开连接事件会晚于新的连接事件触发的问题
- 改进 JWT 认证模块在集群环境下对启动状态的判断逻辑
- 延长规则引擎 DynamoDB 资源状态查询超时时间，以避免部分海外资源可能无法使用的问题
- 修复 PostgreSQL 数据存储插件持久化消息和更新消息消费情况时未更新时间戳的问题
- 修复规则引擎没有在创建 Tablestore、Lindorm 和 InfluxDB 资源时检查连接状态的问题
- 修复规则引擎在写入 TDEngine 资源失败时未正确更新计数的问题
- 修复 Cassandra 消息存储插件将保留消息持久化到多个表中导致消息重复的问题
- 修复从 4.3.0 版本之前的版本导出的 Kafka 资源（仅当 Produce 策略设置为 `first_key_dispatch` 时）无法导入 4.3.0 及以后版本的问题
- 修复主题重写与延迟发布执行顺序不固定的问题，现在固定为优先执行主题重写
- 改进规则引擎持久化数据到 InfluxDB 和 Tablestore 时的空值处理，现在空值将不会被写入
- 修复规则引擎无法编码 MQTT 5.0 用户属性的问题
- 修复客户端使用 MQTT v5.0 以下的协议版本接入时 `connack.auth_error` 计数不准确的问题
- 修复 LwM2M 和 CoAP 网关的 UDP 监听器无法绑定指定网络接口的问题
- 修复在配置文件中移除默认的 Dashboard 用户后 Dashboard 无法启动的问题
- 修复 `client.subscribe` 钩子无法拒绝订阅的问题
- 如果 ACL 规则中的占位符没有被替换，则客户端的发布或订阅操作将被拒绝
- 修复启用了 TLS 但实际并没有与 Pulsar 建立 TLS 连接的问题

## e4.4.4

*发布日期: 2022-06-01*

### 增强

- 为规则引擎 SQL 增加更多的时间转换函数
- 为规则引擎 SQL 增加 `float2str/2` 函数，支持指定浮点输出精度
- 规则引擎支持消息持久化到 Alibaba TableStore
- 规则引擎支持使用 Basic 和 JWT 认证连接 Pulsar
- 规则引擎 Oracle 资源新增 `service_name` 选项以支持 Oracle Database RAC
- 支持将 JWT 用于鉴权，现在 MQTT 客户端可以使用包含发布订阅白名单的特定声明进行授权
- 改进认证相关指标使更易理解，现在 `client.authenticate = client.auth.success + client.auth.failure`
- 支持将 REST API 的监听器绑定到指定的网络接口上
- 上传 License 将自动同步至整个集群，无需每个节点单独上传，提供 HTTP API
- 支持对使用内置数据库作为数据源的认证鉴权中的用户数据进行多条件查询和模糊查询
- 支持将消息队列长度以及丢弃消息数量作为条件查询客户端
- 支持配置日志时间格式以兼容旧版本中的时间格式
- 当 `use_username_as_clientid` 配置为 `true` 且客户端连接时未指定 `username`，现在将拒绝连接并返回 `0x85` 原因码
- App secret 从部分随机改为完全随机
- 通过 CLI 进行备份恢复时，不再要求备份文件必须位于 EMQX 数据目录的 `backup` 文件夹下
- 现在不兼容版本之间的热升级将被拒绝
- 允许 EMQX 的安装路径中有空格
- 引导脚本将在遇到无效的节点名称时快速失败，并提高错误消息的可读性

### 修复

- 修复使用 PostgreSQL 离线消息插件时客户端上线后获取不到消息的问题
- 修复某些情况下规则引擎无法与 Pulsar 成功建立 TLS 连接的问题
- 修复规则引擎 SQL 函数 `hexstr_to_bin/1` 无法处理半字节的问题
- 修复规则引擎资源删除时告警未被清除的问题
- 修复 Dashboard HTTPS 监听器的 `verify` 选项未生效的问题
- 修复共享订阅投递 QoS 1 消息过程中对端会话关闭导致消息丢失的问题
- 修复日志跟踪功能跟踪大报文时堆大小增长过快而触发连接进程强制关闭策略的问题
- 修复模块禁用时未正确卸载相关钩子导致功能异常的问题
- 修复 MQTT-SN 客户端重传 QoS 2 消息时会被断开连接的问题
- 修复备份文件中关闭的模块会在恢复备份后自动启用的问题
- 修复消息发布 API `api/v4/mqtt/publish` 中用户属性类型错误导致订阅端连接断开的问题
- 修复 DynamoDB 驱动未适配 OTP 24 导致不可用的问题
- 修复 PostgreSQL 驱动未适配 OTP 24 导致某些认证算法不可用的问题
- 修复对订阅进行多条件查询时返回结果与查询条件不符的问题
- 修复规则引擎资源连接测试不工作的问题
- 修复多项 Dashboard 显示问题

## e4.4.3

*发布日期: 2022-04-18*

### 增强

- 编解码现已支持使用 gRPC 服务将任意二进制有效负载解码为 JSON 数据
- 支持使用 TLS 连接到 Pulsar
- 规则引擎 SQL 新增 `mongo_date` 函数，支持将时间戳保存为 MongoDB Date 对象
- 规则引擎支持重置指定规则的统计指标
- 规则引擎新增连接确认和鉴权完成事件
- 规则引擎支持拷贝规则以快速复用
- 规则引擎 SQL 支持 zip、gzip 等压缩和解压缩函数
- 改进规则引擎在解析 Payload 失败时的错误提示
- 优化规则引擎部分资源的连接测试
- 支持为 ExHook 设置执行优先级
- ExHook 回调接口新增 `RequestMeta meta` Protobuf 字段用于返回 EMQX 集群名称
- 为共享订阅添加 `local` 策略，这将优先向消息流入的节点下的共享订阅者发送消息。在某些场景下会提升共享消息调度的效率，尤其是在 MQTT 桥接配置为共享订阅时
- 为 TLS 新增对 `RSA-PSK-AES256-GCM-SHA384`、`RSA-PSK-AES256-CBC-SHA384`、`RSA-PSK-AES128-GCM-SHA256`、`RSA-PSK-AES128-CBC-SHA256` 四个 PSK 加密套件的支持，从默认配置中移除 `PSK-3DES-EDE-CBC-SHA` 和 `PSK-RC4-SHA` 这两个不安全的加密套件
- 打印 Mnesia `wait_for_table` 诊断日志
  - 打印 Mnesia 内部统计的检查点
  - 打印每个表加载统计的检查点，帮助定位表加载时间长的问题
- 严格模式下禁止订阅为空的主题
- 当 `loaded_modules` 和 `loaded_plugins` 文件不存在时生成默认文件

### 修复

- 修复 TLS 配置项 `server_name_indication` 设置为 disable 不生效的问题
- 修复 MongoDB 驱动潜在的进程泄漏问题
- 修复通过 CLI 命令修改的 Dashboard 默认用户的密码会在节点离开集群后重置的问题
- 静默 `docker-entrypoint.sh` 中的 grep 和 sed 命令的运行错误日志
- 修复 API 路径包含 ISO8859-1 转义字符时，备份文件无法被正确删除和下载
- 修复 Redis 驱动在 DNS 解析失败等情况下会引发崩溃的问题
- 修复 MQTT Bridge 插件仅配置订阅主题但未配置 QoS 时无法启动的问题
- 创建规则时如果已经有使用相同 ID 的规则存在，现在规则引擎将报错而不是替换已有规则
- 修复 HTTP 驱动进程池可能无法删除的问题
- 修复模块参数更改报错后无法再次更新的问题
- 修复 Dashboard 中 GB/T 32960 接入网关模块部分字段类型错误问题
- 修复 Kafka、Pulsar 等 Bridge 资源的配置无法更新的问题
- 修复启用匿名认证时 JT/T 808 客户端认证失败的问题

## e4.4.2

*发布日期: 2022-04-01*

### 重要变更

- 对于 Docker 镜像，配置目录 `/opt/emqx/etc` 已经从 VOLUME 列表中删除，这使用户可以更容易地使用更改后的配置来重建镜像。
- CentOS 7 Erlang 运行系统在 OpenSSL-1.1.1n（之前是 1.0）上重建，在 v4.3.13 之前，客户端使用某些密码套件时，EMQX 将无法成功握手并触发 `malformed_handshake_data` 异常。
- CentOS 8 Erlang 运行时系统在 RockyLinux 8 上重新构建。 `centos8` 将继续保留在包名中以保持向后兼容。

### 增强

- 规则引擎桥接数据到 Pulsar 新增对 Pulsar proxy 的支持。
- 为 Kafka 生产者增加 OOM 保护。
- 新增命令行接口 `emqx_ctl pem_cache clean`，允许强制清除 x509 证书缓存，以在证书文件更新后立即重新加载。
- 重构 ExProto，以便匿名客户端也可以显示在 Dashboard 上。
- 桥接中的主题配置项现在可以使用 `${node}` 占位符。
- 严格模式下新增对 MQTT 报文中的 UTF-8 字符串有效性检查。设置为 `true` 时，无效的 UTF-8 字符串将导致客户端连接断开。
- MQTT-SN 网关支持会话恢复时主动同步注册主题。
- 将规则引擎浮点型数据的写入精度从为小数点后 10 位提升至 17 位。
- EMQX 将在启动时提示如何修改 Dashboard 的初始密码。

### 修复

- 修复 `MQTT Subscriber` 模块无法使用双向 SSL 连接的问题。
- 修复 `PSKFile` 模块启动失败的问题。
- 修复 `Kafka 消费组` 模块无法处理二进制数据的问题。
- 修复日志追踪功能无法停止的问题。
- 修复规则引擎持久化数据到 Oracle 和 Lindorm 的动作（仅限同步操作）执行失败时无法触发备选动作的问题。
- 修复规则引擎数据持久化到 Oracle 失败但成功计数仍然增加的问题。
- 修复部分 zone 配置无法清除的问题。
- 修复部分监控告警配置的修改在重启后失效的问题。
- 修复编解码功能在集群环境下不可用的问题。
- 修复集群环境下 LwM2M 客户端列表查询 API 返回数据错误导致无法访问 LwM2M 网关模块管理页面的问题。
- 修复 JT/T 808 位置报告报文解析错误的问题。
- 修复 el8 安装包在 Amazon Linux 2022 上无法启动的问题，错误内容为 `errno=13 Permission denied`。
- 修复某些情况下如果连接进程阻塞，客户端无法重连的问题，现在等待超过 15 秒无响应将强制关闭旧的连接进程。
- 修复规则引擎资源不可用时查询资源请求超时的问题。
- 修复热升级运行失败后再次运行出现 `{error, eexist}` 错误的问题。
- 修复向不存在的主题别名发布消息会导致连接崩溃的问题。
- 修复通过 HTTP API 在另一个节点上查询 lwm2m 客户端列表时的 500 错误。
- 修复主题订阅的 HTTP API 在传入非法的 QoS 参数时崩溃的问题。
- 修复通过多语言协议扩展功能接入的连接进程异常退出时未释放相关资源导致连接计数不更新的问题。
- 修复 `server_keepalive` 配置项的值会被错误应用于 MQTT v3.1.1 客户端的问题。
- 修复 Stomp 客户端无法触发 `$event/client_connection` 事件消息的问题。
- 修复 EMQX 启动时系统内存告警误激活的问题。
- 修复向 MQTT-SN 客户端成功注册主题时没有重传此前因未注册主题而投递失败的消息的问题。
- 修复 `loaded_plugins` 文件中配置了重复的插件时 EMQX 启动输出错误日志的问题。
- 修复 MongoDB 相关功能在配置不正确时输出过量错误日志的问题。
- 增加对 Dashboard User 与 AppID 的格式检查，不允许出现 `/` 等特殊字符。
- 将踢除客户端时返回的 DISCONNECT 报文中的原因码更正为 `0x98`。
- 代理订阅将忽略为空的主题。

## e4.4.1

*发布日期: 2022-02-18*

注意: 4.4.1 与 4.3.7 保持同步。

此更改集的比较基础是 4.4.0。

### 重要变更

- 我们在 4.4.1 中修复了 License 总连接数计算的 Bug，License 将正确地检查集群的总连接数，而非错误地仅检查每个节点上的连接数。**请计划升级的用户注意此变化可能导致的客户端达到 License 限制而无法连接的可能性。**
- 慢订阅功能改进，支持统计消息传输过程中花费的时间，并记录和展示耗时较高的客户端和主题。
- 规则引擎支持 Lindorm 数据库
- 支持客户端级别的消息丢弃统计指标
- 优化 Dashboard 上在线 Trace 日志显示，支持语法高亮

### 次要变更

- license 连接数预警，默认连接数达到证书允许的 80% 则告警，小于 75% 时解除告警。用户也可在 `emqx.conf` 中进行自定义：`license.connection_high_watermark_alarm` , `license.connection_low_watermark_alarm`
- license 过期预警，当有效期小于 30 天时，会告警提示
- 规则引擎支持为客户端消息异常丢失事件配置规则与动作，以增强用户在这一场景的自定义处理能力
- 改进规则引擎 SQL 匹配执行过程中的相关统计指标
- 客户端模糊搜索支持 `*`， `(`，`)` 等特殊字符
- 改进 ACL 相关统计指标，解决命中 ACL 缓存导致计数不增加的问题
- Webhook 事件通知中新增 `connected_at` 字段
- 在因持有锁太久而终止客户端状态之前记录客户端状态

### 修复

- 修复数据导入导出在某些情况下不可用的问题
- Module 更新机制改进，解决更新失败后 Module 不可用的问题
- 修复规则引擎在执行比较大小的语句时候未进行类型检查的问题
- 修复更新规则引擎动作后相关计数清零的问题
- 修复 Metrics 接口默认情况下不返回 client.acl.deny 等认证鉴权指标的问题
- 修复订阅查询接口未返回分页数据的问题
- 修复 STOMP 处理 TCP 粘包时解析失败的问题
- 修复客户端过滤查询时会话创建时间选项不可用的问题
- 修复重启后内存告警可能不会触发的问题
- 修复 `emqx_auth_mnesia` 插件中存在用户数据时导入数据崩溃的问题

## e4.4.0

*发布日期: 2021-12-21*

EMQX 4.4.0 现已正式发布，主要包含以下改动:

### 重要变更

- 从 4.4 开始，EMQX 的发行包命名将包含 Erlang/OTP 的版本号，例如 `emqx-ee-4.4.0-otp24.1.5-3-centos7-arm64.rpm`

- **对于 Debian/Ubuntu 用户**，Debian/Ubuntu 包 (deb) 安装的 EMQX 现在可以在 systemd 上运行，这是为了利用 systemd 的监督功能来确保 EMQX 服务在崩溃后重新启动。包安装服务从 init.d 升级到 systemd 已经过验证，但仍建议您在部署到生产环境之前再次验证确认，至少确保 systemd 在您的系统中可用

- 规则引擎 InfluxDB 集成新增对 InfluxDB v2 API 的支持，规则引擎现已支持 InfluxDB 2.0 与 InfluxDB Cloud

- 规则引擎新增对 SAP Event Mesh 的支持

- 规则引擎新增对超融合时空数据库 MatrixDB 的支持

- MongoDB 集成支持 DNS SRV 和 TXT Records 解析，可以与 MongoDB Altas 无缝对接

- 新增在线 Trace 功能，用户可以在 Dashboard 上完成对客户端和主题的追踪操作，以及查看或下载追踪日志

- 新增慢订阅统计功能，用以及时发现生产环境中消息堵塞等异常情况

- 支持动态修改 MQTT Keep Alive 以适应不同能耗策略

- 集群从 4.3 到 4.4 支持滚动升级。详情请见升级指南。

- 节点间的 RPC 链接支持配置 TLS. 详情请见[集群文档](../advanced/cluster.md#节点间RPC使用TLS)

### 次要变更

- Dashboard 支持查看客户端活跃连接数

- Dashboard 支持相对路径和自定义访问路径

- Dashboard 移除选项卡导航

- 支持配置是否将整型数据以浮点类型写入 InfluxDB

- 支持配置是否转发为 Payload 为空的保留消息，以适应仍在使用 MQTT v3.1 的用户，相关配置项为 `retainer.stop_publish_clear_msg`

- 多语言钩子扩展（exhook）支持动态取消客户端消息的后续转发

- 规则引擎 SQL 支持在 FROM 子句中使用单引号，例如：`SELECT * FROM 't/#'`

- 优化内置访问控制文件模块的使用交互

- 将 `max_topic_levels` 配置项的默认值更改为 128，以前它没有限制（配置为 0），这可能是潜在的 DoS 威胁

- 改进了接收到 Proxy Protocol 报文但 `proxy_protocol` 配置项未开启时的错误日志内容

- 为网关上报消息添加额外的消息属性。来自 CoAP, LwM2M，Stomp，ExProto 等网关的消息，在转换为 EMQX 的消息时，添加例如协议名称，协议版本，用户名，客户端 IP 等字段，可用于多语言钩子扩展

- HTTP 性能优化

- 将 openssl-1.1 添加到 RPM 依赖

### 修复

- 修复节点间 RPC 调用堵塞导致客户端进程失去响应的问题

- 修复锁管理进程 `ekka_locker` 在杀死挂起的锁持有者后 Crash 的问题

- 修复 RocketMQ 异步写入时数据乱码问题

- 修复 RocketMQ 统计指标不准的问题

- 修复集群节点数量超过七个时 Dashboard 监控页面的显示错误

- 修复规则引擎保存数据到 MySQL 时可能出现较高失败率的问题

- 修复规则引擎 Clickhouse 离线消息功能不可用的问题

- 修复规则引擎 MongoDB 离线消息功能中 Max Returned Count 选项无法使用的问题

- 修复规则引擎 WebHook Action 中的 Path 参数无法使用规则引擎变量的问题

- 修复 MongoDB 认证模块无法使用 Replica Set 模式等问题

- 修复集群间消息转发失序问题，相关配置项为 `rpc.tcp_client_num`

- 修复内存占用计算错误的问题

- 修复部分热配置失效的问题

- 修复远程主机无法访问时的 MQTT Bridge 故障问题（连接挂起）

- 修复 HTTP Headers 可能重复的问题
