# 版本发布

## 4.4.19

*发布日期: 2023-06-27*

### 增强

- 为 MQTT/TCP 和 MQTT/SSL 监听器增加 TCP Keep Alive 的支持 [#10854](https://github.com/emqx/emqx/pull/10854)。

  现在增加了一个配置项：`zone.<zone-name>.tcp_keepalive = Idle,Interval,Probes`，用户可以通过此配置来启用 TCP 层的 Keep Alive 功能并指定时间参数。此配置仅在 Linux 和 MacOS 系统上生效。

- 改进 Proxy Protocol 相关的错误日志 [emqx/esockd#177](https://github.com/emqx/esockd/pull/177)。

  改进之前的日志样例:

  ```
  2023-04-20T14:56:51.671735+08:00 [error] supervisor: 'esockd_connection_sup - <0.2537.0>', errorContext: connection_shutdown, reason: {invalid_proxy_info,<<"f\n">>}, offender: [{pid,<0.3192.0>},{name,connection},{mfargs,{...}}]

  2023-04-20T14:57:01.348275+08:00 [error] supervisor: 'esockd_connection_sup - <0.2537.0>', errorContext: connection_shutdown, reason: {proxy_proto_timeout,5000}, offender: [{pid,<0.3194.0>},{name,connection},{mfargs,{...}}]
  ```

  改进之后:

  ```
  2023-04-20T18:07:06.180134+08:00 [error] [esockd_proxy_protocol] The listener 127.0.0.1:8883 is working in proxy protocol mode, but received invalid proxy_protocol header, raw_bytes=<<"f\n">>

  2023-04-20T18:10:17.205436+08:00 [error] [esockd_proxy_protocol] The listener 127.0.0.1:8883 is working in proxy protocol mode, but timed out while waiting for proxy_protocol header
  ```

- 增加了一个新功能，用户可以在 TLS 监听器中启用“部分证书链验证”了 [#10553](https://github.com/emqx/emqx/pull/10553)。

  详情请查看 `emqx.conf` 配置文件中的 `listener.ssl.external.partial_chain` 配置项。

- 增加了一个新功能，用户可以在 TLS 监听器中启用“客户端证书扩展密钥用途验证”了 [#10669](https://github.com/emqx/emqx/pull/10669)。

  详情请查看 `emqx.conf` 配置文件中的 `listener.ssl.external.verify_peer_ext_key_usage` 配置项。

- 在 HTTP API `/api/v4/nodes` 的返回中增加 `live_connections` 字段 [#10859](https://github.com/emqx/emqx/pull/10859)。

  此前该接口中有一个 `connections` 字段，它代表当前节点上会话未过期的连接数量。这意味着即使 MQTT 连接已经断开，只要客户端保持了会话，它仍然会被统计在 `connections` 中。新增的 `live_connections` 字段则仅仅统计 MQTT 连接未断开的客户端数量。

- 规则引擎新增了三个随机函数 [#11113](https://github.com/emqx/emqx/pull/11113)。

  - random()：生成 0 到 1 之间的随机数 (0.0 =< X < 1.0)。
  - uuid_v4()：生成随机的 UUID (version4) 字符串。
  - uuid_v4_no_hyphen()：生成随机的不带连词符的 UUID (version4) 字符串。

- 为 `mqtt.max_clientid_len` 配置项增加数值范围校验 (23-65535) [#11096](https://github.com/emqx/emqx/pull/11096)。

### 修复

- 修复规则引擎无法在 `DO` 子句中访问 `FOREACH` 导出的变量的问题 [#10620](https://github.com/emqx/emqx/pull/10620)。

  给定消息：`{"date": "2023-05-06", "array": ["a"]}`，以及如下 SQL 语句：

  ```
  FOREACH payload.date as date, payload.array as elem
  DO date, elem
  FROM "t/#"
  ```

  修复前，以上 SQL 语句中 `FOREACH` 导出的 `date` 变量无法在 `DO` 子句中访问，导致以上 SQL 的输出为：
  `[{"elem": "a","date": "undefined"}]`。
  修复后，SQL 的输出为：`[{"elem": "a","date": "2023-05-06"}]`

- 修复在某些情况下规则的缓存没有更新到某些节点上的问题 [#11072](https://github.com/emqx/emqx/pull/11072)。

  修复前，手动更新规则之后，可能会出现缓存的更新没能同步到某些节点上的情况，这会导致规则在不同的节点上运行状态不一致。

- 修复 WebHook 插件执行 `on_client_connack` 钩子失败的问题 [#10710](https://github.com/emqx/emqx/pull/10710)。

  详见 <https://github.com/emqx/emqx/issues/10628。>

## 4.4.18

*发布日期: 2023-04-28*

### 增强

- 改进规则引擎的占位符语法 [#10470](https://github.com/emqx/emqx/pull/10470)。

  某些动作的参数支持使用占位符语法，来动态的填充字符串的内容，占位符语法的格式为 `${key}`。
  改进前，`${key}` 中的 `key` 只能包含字母、数字和下划线。改进后 `key` 支持任意的 UTF8 字符了。

## 4.4.17

*发布日期: 2023-04-13*

### 增强

- 启用了 `Proxy Protocol` 的监听器在收到 TCP 端口探测时，不再打印错误日志 [emqx/esockd#172](https://github.com/emqx/esockd/pull/172)。

  修复前，如果监听器已启用了代理协议（`listener.tcp.external.proxy_protocol=on`），但连接在 TCP 握手完成后、接收到代理信息之前断开，则会打印以下错误日志：

  ```
  [error] supervisor: 'esockd_connection_sup - <0.3265.0>', errorContext: connection_shutdown, reason: {recv_proxy_info_error,tcp_closed}, offender:
  ```

  修复后不再打印任何日志，但仍然可以通过 `emqx_ctl listeners` 命令来查看错误原因的统计。

- 改进监听器针对文件描述符耗尽的错误日志 [emqx/esockd#173](https://github.com/emqx/esockd/pull/173)。

  改进前的日志：

  ```
  [error] Accept error on 0.0.0.0:1883: emfile
  ```

  改进后的日志：

  ```
  [error] Accept error on 0.0.0.0:1883: EMFILE (Too many open files)
  ```

- 提升规则引擎在规则数量较多时的性能 [#10283](https://github.com/emqx/emqx/pull/10283)

  改进前，当规则数量较多时，规则引擎将需要耗费大量 CPU 在规则的查询和匹配上，并成为性能瓶颈。
  本次优化中，通过简单地给规则列表添加一个缓存，大幅提升了此场景下的规则执行效率。
  在我们的测试中，在一个 32 核 32G 的虚拟机上，我们创建了 700 条不执行任何动作的规则（绑定了 "do_nothing" 调试动作），
  并以 1000 条每秒的速度向 EMQX 发送 MQTT 消息（即，规则触发频率为 700 * 1000 次每秒），
  在上述场景下，优化后的规则引擎 CPU 使用率下降到了之前的 55% ~ 60%。

### 修复

- 修复 `Erlang distribution` 无法使用 TLS 的问题 [#9981](https://github.com/emqx/emqx/pull/9981)。

  关于 `Erlang distribution`, 详见 [这里](https://www.emqx.io/docs/zh/v4.4/advanced/cluster.html)。

- 修复 MQTT 桥接无法验证对端带通配符的 TLS 证书的问题 [#10094](https://github.com/emqx/emqx/pull/10094)。

- 修复当 retainer 中积压的消息过多时，EMQX 无法及时清除已掉线的 MQTT 连接信息的问题。[#10189](https://github.com/emqx/emqx/pull/10189)。

  修复前，`emqx_retainer` 插件和 EMQX 连接信息清理任务共用一个进程池，因此，
  如果该进程池被大量的 retain 消息下发任务阻塞时，许多已经掉线的 MQTT 连接信息将得不到及时清理。
  详见 [#9409](https://github.com/emqx/emqx/issues/9409)。
  修复后，`emqx_retainer` 插件使用单独的进程池，从而避免了该问题。

- 修复了 Helm Chart 中模板文件 `service-monitor.yaml` 路径不正确的问题。[#10229](https://github.com/emqx/emqx/pull/10229)

## 4.4.16

*发布日期: 2023-03-10*

### 增强

- 从命令行的输出里和插件的名字中，把 "EMQX" 改成 "EMQX" [#10099](https://github.com/emqx/emqx/pull/10099)。

### 修复

- 避免打印 debug 日志的时候改动 MQTT 消息的 Payload 的内容 [#10091](https://github.com/emqx/emqx/pull/10091)。
  在这个修复之前，如果 EMQX 收到一个 Payload 为 "e\ne\nc\nc\n2\n\n\n" 的消息，日志打印会变成这样：

  ```
  2023-03-08T13:28:04.320622+08:00 [debug] mqttx_e34bd582@127.0.0.1:54020 [MQTT] RECV PUBLISH(Q1, R0, D0, Topic=t/1, PacketId=39467, Payload=e, e, c, c, 2, , , )
  ```

  这是此修复之后的样子：

  ```
  2023-03-08T14:26:50.935575+08:00 [debug] mqttx_e34bd582@127.0.0.1:54020 [MQTT] RECV PUBLISH(Q1, R0, D0, Topic=t/1, PacketId=39467, Payload=<<"e\ne\nc\nc\n2\n\n\n">>)
  ```

## 4.4.15

*发布日期: 2023-03-03*

本次版本更新包含了 8 个增强和 13 个修复。比较重要的功能增强有：

- 升级 EMQX 的 MongoDB 客户端库，支持 MongoDB 5.1 及以上版本。
- Dashboard 支持 HAProxy 的 Proxy Protocol。
- 发布 Ubuntu 22.04 安装包。

### 增强

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

- 在 `资源` 删除时清理其文件目录以防止文件泄露 [#10039](https://github.com/emqx/emqx/pull/10039)。

## 4.4.11

*发布日期: 2022-11-26*

本次版本更新包含了 18 个增强和 14 个修复。
在这些增强中，下面几个新功能值得重点介绍：

- 新增 OCSP (Online Certificate Status Protocol) Stapling。
- 新增 CRL (Certificate Revocation List) cache。
- OTP 从 24.1.5-3 升级到了 24.3.4.2-1。
- 新增了 客户端别名的支持，使得定制化认证和授权更加容易实现。

该版本仍支持从老版本的 v4.4 热升级上来，
但需要注意的是：如果需要在热升级后使用新版本提供的功能（例如 OCSP Stapling 和 CRL）
那么节点重启（配合配置更新）仍然不能避免。

### 增强

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

## 4.4.10

*发布日期: 2022-10-14*

### 增强

- TLS 监听器内存使用量优化 [#9005](https://github.com/emqx/emqx/pull/9005)。
  新增了配置项 `listener.ssl.$NAME.hibernate_after` (默认不开启），该配置生效后，TLS 连接进程在空闲一段时间后会进入休眠。
  休眠可以较大程度减少内存占用，但是代价是 CPU 利用率会增加。
  测试中使用 '5s' （即 5 秒空闲后休眠）可以减少 50% 的内存使用量。

- 默认 TLS Socket 缓存大小设置为 4KB [#9007](https://github.com/emqx/emqx/pull/9007)
  这样可以有效的避免某些环境中操作系统提供的默认缓存过大而导致 TLS 连接内存使用量大的问题。

- 关闭了 HTTP API `api/v4/emqx_prometheus` 的认证 [#8955](https://github.com/emqx/emqx/pull/8955)。
  Prometheus 对时序数据抓取不在需要配置认证。

- 更严格的 flapping 检测，认证失败等原因也会进行计数 [#9045](https://github.com/emqx/emqx/pull/9045)。

- 当共享订阅的会话终结时候，把缓存的 QoS1 和 QoS2 的消息向订阅组的其他成员进行转发 [#9094](https://github.com/emqx/emqx/pull/9094)。
  在这个增强之前，可以通过设置配置项 `broker.shared_dispatch_ack_enabled` 为 `true` 来防止在共享订阅的会话中缓存消息,
  但是这种转发因为需要对每个消息进行应答，会增加额外的系统开销。

- 修复延迟发布可能因为修改系统时间而导致的延迟等问题 [#8908](https://github.com/emqx/emqx/pull/8908)。

### 修复

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

## 4.4.9

*发布日期: 2022-09-17*

### 增强

- JWT 认证中的 `exp`、`nbf` 和 `iat` 声明支持非整数时间戳

### 修复

- 修复规则引擎的更新行为，此前会尝试初始化已禁用的规则
- 修复 Dashboard 监听器绑定 IP 地址不生效的问题
- 修复 `shared_dispatch_ack_enabled` 配置为 true 时共享订阅可能陷入死循环的问题
- 修复规则引擎 SQL 对空值进行主题匹配时崩溃的问题

## 4.4.8

*发布日期: 2022-08-29*

### 增强

- 新增 `GET /trace/:name/detail` API 以查看日志追踪文件信息
- 改进 LwM2M 报文解析失败时的日志
- 改进规则引擎错误日志，动作执行失败时的日志中将包含规则 ID
- 改进 `loaded_modules` 和 `loaded_plugins` 文件不存在时的提醒日志
- Dashboard 新增修改默认密码的引导

### 修复

- 修复 `client.disconnected` 在某些情况下不会触发的问题
- 修复内置数据库认证未区分客户端 ID 和用户名的认证数据的分页统计的问题
- 修复 Redis 驱动进程泄漏的问题
- 修复规则引擎 MQTT 桥接至 AWS IOT 连接超时的问题
- 修复监听器未就绪时 `GET /listener` 请求崩溃的问题
- 修复 v4.4.1 版本后规则引擎 SQL 中任意变量与空值比较总是返回 false 的问题
- 修复错误地将 `emqx_modules` 应用作为插件管理的问题
- 修复 ExHook 的执行优先级高于规则引擎时，被 ExHook Message Hook 过滤的主题将无法触发规则引擎的问题
- 修复 ExHook 管理进程因 supervisor 关闭超时而被强制杀死的问题
- 修复 ExProto `client.connect` 钩子中 Client ID 参数未定义的问题
- 修复客户端被踢除时 ExProto 不会触发断开连接事件的问题

## 4.4.7

*发布日期: 2022-08-11*

### 重要变更

- 从 4.4.7 版本开始，我们将不再为 macOS 10 提供安装包

### 增强

- 允许配置连接进程在 TLS 握手完成后进行垃圾回收以减少内存占用，这可以使每个 SSL 连接减少大约 35% 的内存消耗，但相应地会增加 CPU 的消耗
- 允许配置 TLS 握手日志的日志等级以便查看详细的握手过程

## 4.4.6

*发布日期: 2022-07-29*

### 增强

- 支持对规则引擎中的规则进行搜索和分页
- 提供 CLI `./bin/emqx check_conf` 以主动检查配置是否正确
- 优化共享订阅性能

### 修复

- 修复热升级后一旦卸载了老版本 EMQX 将无法再次启动的问题
- 修复多语言协议扩展中对 UDP 客户端的保活检查错误导致客户端不会过期的问题
- 修复多语言协议扩展中客户端信息没有及时更新的问题
- 修复客户端指定 Clean Session 为 false 重连时，飞行窗口中的共享订阅消息会被尝试重新派发给旧会话进程的问题
- 修复 `emqx_lua_hook` 插件无法取消消息发布的问题

## 4.4.5

*发布日期: 2022-06-30*

### 增强

- 规则引擎消息重发布动作中的 QoS 和保留消息标识现在可以使用占位符
- 支持排他订阅，即一个主题只允许存在一个订阅者
- 现在 Dashboard 和管理 API 的 HTTPS 监听器可以使用受密码保护的私钥文件，提供了 `key_password` 配置项
- 支持在主题重写规则中使用占位符 `%u` 和 `%c`
- 支持在消息发布的 API 请求中设置 MQTT 5.0 的 Properties，例如消息过期间隔、响应主题等
- 优化规则引擎资源创建时的 UI，例如折叠部分不常用的选项等
- 为 ExHook 底层的 gRPC 连接开放了 KeepAlive、TCP_NODELAY、SO_RCVBUF 和 SO_SNDBUF 共 4 个与 TCP 相关的配置项

### 修复

- 修复 Linux 系统中内存计算不准确的问题，并改为计算当前系统的内存占用，而不是 EMQX 的内存占用
- 修复 ExHook 在客户端重连时旧的断开连接事件会晚于新的连接事件触发的问题
- 修复主题重写与延迟发布执行顺序不固定的问题，现在固定为优先执行主题重写
- 修复规则引擎无法编码 MQTT 5.0 用户属性的问题
- 修复客户端使用 MQTT v5.0 以下的协议版本接入时 `connack.auth_error` 计数不准确的问题
- 修复 LwM2M 和 CoAP 网关的 UDP 监听器无法绑定指定网络接口的问题
- 修复在配置文件中移除默认的 Dashboard 用户后 Dashboard 无法启动的问题
- 修复 `client.subscribe` 钩子无法拒绝订阅的问题
- 如果 ACL 规则中的占位符没有被替换，则客户端的发布或订阅操作将被拒绝

## 4.4.4

*发布日期: 2022-06-01*

### 增强

- 为规则引擎 SQL 增加更多的时间转换函数
- 为规则引擎 SQL 增加 `float2str/2` 函数，支持指定浮点输出精度
- 支持将 JWT 用于鉴权，现在 MQTT 客户端可以使用包含发布订阅白名单的特定声明进行授权
- 改进认证相关指标使更易理解，现在 `client.authenticate = client.auth.success + client.auth.failure`
- 支持 REST API 的监听器绑定到指定的网络接口上
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

- 修复规则引擎 SQL 函数 `hexstr_to_bin/1` 无法处理半字节的问题
- 修复规则引擎资源删除时告警未被清除的问题
- 修复 Dashboard HTTPS 监听器的 `verify` 选项未生效的问题
- 修复共享订阅投递 QoS 1 消息过程中对端会话关闭导致消息丢失的问题
- 修复日志跟踪功能跟踪大报文时堆大小增长过快而触发连接进程强制关闭策略的问题
- 修复 MQTT-SN 客户端重传 QoS 2 消息时会被断开连接的问题
- 修复消息发布 API `api/v4/mqtt/publish` 中用户属性类型错误导致订阅端连接断开的问题
- 修复 PostgreSQL 驱动未适配 OTP 24 导致某些认证算法不可用的问题
- 修复对订阅进行多条件查询时返回结果与查询条件不符的问题
- 修复规则引擎资源连接测试不工作的问题
- 修复多项 Dashboard 显示问题

## 4.4.3

*发布日期: 2022-04-18*

### 增强

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
- 修复规则引擎发送数据到 Web 服务动作中 Headers 字段配置不生效的问题
- 修复 MQTT Bridge 插件仅配置订阅主题但未配置 QoS 时无法启动的问题
- 创建规则时如果已经有使用相同 ID 的规则存在，现在规则引擎将报错而不是替换已有规则
- 修复 HTTP 驱动进程池可能无法删除的问题

## 4.4.2

*发布日期: 2022-04-01*

### 重要变更

- 对于 Docker 镜像，配置目录 `/opt/emqx/etc` 已经从 VOLUME 列表中删除，这使用户可以更容易地使用更改后的配置来重建镜像。
- CentOS 7 Erlang 运行系统在 OpenSSL-1.1.1n（之前是 1.0）上重建，在 v4.3.13 之前，客户端使用某些密码套件时，EMQX 将无法成功握手并触发 `malformed_handshake_data` 异常。
- CentOS 8 Erlang 运行时系统在 RockyLinux 8 上重新构建。 `centos8` 将继续保留在包名中以保持向后兼容。

### 增强

- Windows 包支持基于 Erlang/OTP 24 构建。
- 新增命令行接口 `emqx_ctl pem_cache clean`，允许强制清除 x509 证书缓存，以在证书文件更新后立即重新加载。
- 重构 ExProto，以便匿名客户端也可以显示在 Dashboard 上。
- 桥接中的主题配置项现在可以使用 `${node}` 占位符。
- 严格模式下新增对 MQTT 报文中的 UTF-8 字符串有效性检查。设置为 `true` 时，无效的 UTF-8 字符串将导致客户端连接断开。
- MQTT-SN 网关支持会话恢复时主动同步注册主题。
- 将规则引擎浮点型数据的写入精度从为小数点后 10 位提升至 17 位。
- EMQX 将在启动时提示如何修改 Dashboard 的初始密码。

### 修复

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

## 4.4.1

*发布日期: 2022-02-21*

此补丁版本仅包含 Windows 包的 CI 更改。

## 4.4.0

*发布日期: 2022-02-18*

注意:

- 4.4.0 与 4.3.12 保持同步。
- 当前版本 Windows 包的构建存在一些问题，我们会在下一版本中解决它

此更改集的比较基础是 4.3.12。

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

- 节点间的 RPC 链接支持配置 TLS.

- 新增慢订阅功能，支持统计消息传输过程中花费的时间，并记录和展示耗时较高的客户端和主题。

- 新增在线日志跟踪功能，支持实时跟踪客户端事件并在 Dashboard 上查看。

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

## 4.3.22

*发布日期: 2022-11-26*

这是 EMQX 开原版 v4.3 系列的最后一个版本。

### 增强

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
  详情请参考 [创建规则](https://www.emqx.io/docs/zh/v4.3/advanced/http-api.html#post-api-v4-rules) 和 [创建资源](https://www.emqx.io/docs/zh/v4.3/advanced/http-api.html#post-api-v4-resources)。

- 修复调用 'DELETE /alarms/deactivated' 只在单个节点上生效的问题，现在将会删除所有节点上的非活跃警告 [#9280](https://github.com/emqx/emqx/pull/9280)。

- 在进行消息重发布或桥接消息到其他 mqtt broker 时，检查 topic 合法性，确定其不带有主题通配符 [#9291](https://github.com/emqx/emqx/pull/9291)。

- 关闭管理端口（默认为8081）上对 HTTP API `api/v4/emqx_prometheus` 的认证，Prometheus 对时序数据抓取不在需要配置认证 [#9294](https://github.com/emqx/emqx/pull/9294)。

## 4.3.21

*发布日期: 2022-10-14*

### 增强

- TLS 监听器内存使用量优化 [#9005](https://github.com/emqx/emqx/pull/9005)。
  新增了配置项 `listener.ssl.$NAME.hibernate_after` (默认不开启），该配置生效后，TLS 连接进程在空闲一段时间后会进入休眠。
  休眠可以较大程度减少内存占用，但是代价是 CPU 利用率会增加。
  测试中使用 '5s' （即 5 秒空闲后休眠）可以减少 50% 的内存使用量。

- 默认 TLS Socket 缓存大小设置为 4KB [#9007](https://github.com/emqx/emqx/pull/9007)
  这样可以有效的避免某些环境中操作系统提供的默认缓存过大而导致 TLS 连接内存使用量大的问题。

- 关闭对 HTTP API `api/v4/emqx_prometheus` 的认证 [#8955](https://github.com/emqx/emqx/pull/8955) 。
  Prometheus 对时序数据抓取不在需要配置认证。

- 更严格的 flapping 检测，认证失败等也会进行计数 [#9045](https://github.com/emqx/emqx/pull/9045)。

- 当共享订阅的会话终结时候，把缓存的 QoS1 和 QoS2 的消息向订阅组的其他成员进行转发 [#9094](https://github.com/emqx/emqx/pull/9094)。
  在这个增强之前，可以通过设置配置项 `broker.shared_dispatch_ack_enabled` 为 `true` 来防止在共享订阅的会话中缓存消息，
  但是这种转发因为需要对每个消息进行应答，会增加额外的系统开销。

- 修复延迟发布可能因为修改系统时间而导致的延迟等问题 [#8908](https://github.com/emqx/emqx/pull/8908)。

### 修复

- 修复 HTTP 客户端库启用 SSL 后 Socket 可能会进入 passive 状态 [#9145](https://github.com/emqx/emqx/pull/9145)。

- 隐藏 redis 客户端错误日志中的密码参数 [#9071](https://github.com/emqx/emqx/pull/9071)
  也包含了如下一些改进：
  - 修复一些其他可能导致密码泄漏的隐患 [eredis#19](https://github.com/emqx/eredis/pull/19)。
  - 修复了 eredis_cluster 中连接池命名冲突的问题 [eredis_cluster#22](https://github.com/emqx/eredis_cluster/pull/22)
    同时对这个库也进行了密码泄漏隐患对修复。

- 修复共享订阅消息转发逻辑 [#9094](https://github.com/emqx/emqx/pull/9094)。
  - QoS2 飞行窗口消息丢弃的日志过度打印问题
  - 对于通配符订阅的消息，会话中缓存的消息重发时，因为使用了发布主题（而非通配符主题）来进行重发，
    导致无法匹配到共享订阅组内的其他成员。

- 修复共享订阅 `sticky` 策略下，客户端取消订阅后仍然可以收到消息的问题 [#9119](https://github.com/emqx/emqx/pull/9119) 。
  这之前的版本中，仅处理了客户端会话终结，而没有处理取消订阅。

- 修复共享订阅 `sticky` 策略在某些情况下退化成 `random` 策略的问题 [#9122](https://github.com/emqx/emqx/pull/9122) 。
  集群环境下，在之前的版本中, 共享订阅组成员的选择在最开始时会随机选取，最终会选中在本节点连接的客户端，并开始粘性转发。
  如果所有的订阅客户端都不在本节点，那么粘性策略就会退化成随机。
  这个修复后，粘性策略将应用于第一个随机选取的客户端，不论该客户端是不是在本节点。

- 修复规则引擎的备选（fallback）动作的计数重置失败的问题 [#9125](https://github.com/emqx/emqx/pull/9125)。

## 4.3.20

*发布日期: 2022-09-17*

### 增强

- JWT 认证中的 `exp`、`nbf` 和 `iat` 声明支持非整数时间戳

### 修复

- 修复规则引擎的更新行为，此前会尝试初始化已禁用的规则
- 修复 Dashboard 监听器绑定 IP 地址不生效的问题
- 修复 `shared_dispatch_ack_enabled` 配置为 true 时共享订阅可能陷入死循环的问题
- 修复规则引擎 SQL 对空值进行主题匹配时崩溃的问题

## 4.3.19

*发布日期: 2022-08-29*

### 增强

- 改进 LwM2M 报文解析失败时的日志
- 改进规则引擎错误日志，动作执行失败时的日志中将包含规则 ID（
- 改进 `loaded_modules` 和 `loaded_plugins` 文件不存在时的提醒日志
- Dashboard 新增修改默认密码的引导

### 修复

- 修复 `client.disconnected` 在某些情况下不会触发的问题
- 修复内置数据库认证未区分客户端 ID 和用户名的认证数据的分页统计的问题
- 修复 Redis 驱动进程泄漏的问题
- 修复规则引擎 MQTT 桥接至 AWS IOT 连接超时的问题
- 修复监听器未就绪时 `GET /listener` 请求崩溃的问题
- 修复 v4.3.12 版本后规则引擎 SQL 中任意变量与空值比较总是返回 false 的问题
- 修复错误地将 `emqx_modules` 应用作为插件管理的问题
- 修复 ExHook 的执行优先级高于规则引擎时，被 ExHook Message Hook 过滤的主题将无法触发规则引擎的问题
- 修复 ExHook 管理进程因 supervisor 关闭超时而被强制杀死的问题
- 修复 ExProto `client.connect` 钩子中 Client ID 参数未定义的问题
- 修复客户端被踢除时 ExProto 不会触发断开连接事件的问题

## 4.3.18

*发布日期: 2022-08-11*

### 重要变更

- 升级了使用的 OTP 版本，以解决 OTP Bug 导致的低概率出现随机进程失去响应的问题，建议仍在使用 4.3 的用户升级到此版本
- 从下一版本起，我们将停止对 macOS 10 的支持，转为提供 macOS 11 的安装包

### 增强

- 允许配置连接进程在 TLS 握手完成后进行垃圾回收以减少内存占用，这可以使每个 SSL 连接减少大约 35% 的内存消耗，但相应地会增加 CPU 的消耗
- 允许配置 TLS 握手日志的日志等级以便查看详细的握手过程

## 4.3.17

*发布日期: 2022-07-29*

### 增强

- 支持对规则引擎中的规则进行搜索和分页
- 提供 CLI `./bin/emqx check_conf` 以主动检查配置是否正确
- 优化共享订阅性能

### 修复

- 修复热升级后一旦卸载了老版本 EMQX 将无法再次启动的问题
- 修复多语言协议扩展中对 UDP 客户端的保活检查错误导致客户端不会过期的问题
- 修复多语言协议扩展中客户端信息没有及时更新的问题
- 修复客户端指定 Clean Session 为 false 重连时，飞行窗口中的共享订阅消息会被尝试重新派发给旧会话进程的问题
- 修复 `emqx_lua_hook` 插件无法阻止消息发布的问题

## 4.3.16

*发布日期: 2022-06-30*

### 增强

- 规则引擎消息重发布动作中的 QoS 和保留消息标识现在可以使用占位符
- 支持排他订阅，即一个主题只允许存在一个订阅者
- 现在 Dashboard 和管理 API 的 HTTPS 监听器可以使用受密码保护的私钥文件，提供了 `key_password` 配置项
- 支持在主题重写规则中使用占位符 `%u` 和 `%c`
- 支持在消息发布的 API 请求中设置 MQTT 5.0 的 Properties，例如消息过期间隔、响应主题等
- 优化规则引擎资源创建时的 UI，例如折叠部分不常用的选项等
- 为 ExHook 底层的 gRPC 连接开放了 KeepAlive、TCP_NODELAY、SO_RCVBUF 和 SO_SNDBUF 共 4 个与 TCP 相关的配置项

### 修复

- 修复 Linux 系统中内存计算不准确的问题，并改为计算当前系统的内存占用，而不是 EMQX 的内存占用
- 修复 ExHook 在客户端重连时旧的断开连接事件会晚于新的连接事件触发的问题
- 修复主题重写与延迟发布执行顺序不固定的问题，现在固定为优先执行主题重写
- 修复规则引擎无法编码 MQTT 5.0 用户属性的问题
- 修复客户端使用 MQTT v5.0 以下的协议版本接入时 `connack.auth_error` 计数不准确的问题
- 修复 LwM2M 和 CoAP 网关的 UDP 监听器无法绑定指定网络接口的问题
- 修复在配置文件中移除默认的 Dashboard 用户后 Dashboard 无法启动的问题
- 修复 `client.subscribe` 钩子无法拒绝订阅的问题
- 如果 ACL 规则中的占位符没有被替换，则客户端的发布或订阅操作将被拒绝

## 4.3.15

*发布日期: 2022-06-01*

### 增强

- 为规则引擎 SQL 增加更多的时间转换函数
- 为规则引擎 SQL 增加 `float2str/2` 函数，支持指定浮点输出精度
- 支持将 JWT 用于鉴权，现在 MQTT 客户端可以使用包含发布订阅白名单的特定声明进行授权
- 改进认证相关指标使更易理解，现在 `client.authenticate = client.auth.success + client.auth.failure`
- 支持 REST API 的监听器绑定到指定的网络接口上
- 支持对使用内置数据库作为数据源的认证鉴权中的用户数据进行多条件查询和模糊查询
- 支持将消息队列长度以及丢弃消息数量作为条件查询客户端
- 支持配置日志时间格式以兼容旧版本中的时间格式
- 当 `use_username_as_clientid` 配置为 `true` 且客户端连接时未指定 `username`，现在将拒绝连接并返回 `0x85` 原因码
- App secret 从部分随机改为完全随机
- 现在不兼容版本之间的热升级将被拒绝
- 允许 EMQX 的安装路径中有空格
- 引导脚本将在遇到无效的节点名称时快速失败，并提高错误消息的可读性

### 修复

- 修复规则引擎 SQL 函数 `hexstr_to_bin/1` 无法处理半字节的问题
- 修复规则引擎资源删除时告警未被清除的问题
- 修复 Dashboard HTTPS 监听器的 `verify` 选项未生效的问题
- 修复共享订阅投递 QoS 1 消息过程中对端会话关闭导致消息丢失的问题
- 修复日志跟踪功能跟踪大报文时堆大小增长过快而触发连接进程强制关闭策略的问题
- 修复 MQTT-SN 客户端重传 QoS 2 消息时会被断开连接的问题
- 修复对订阅进行多条件查询时返回结果与查询条件不符的问题
- 修复规则引擎资源连接测试不工作的问题
- 修复多项 Dashboard 显示问题

## 4.3.14

*发布日期: 2022-04-18*

### 增强

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
- 修复规则引擎发送数据到 Web 服务动作中 Headers 字段配置不生效的问题
- 修复 MQTT Bridge 插件仅配置订阅主题但未配置 QoS 时无法启动的问题
- 创建规则时如果已经有使用相同 ID 的规则存在，现在规则引擎将报错而不是替换已有规则
- 修复 HTTP 驱动进程池可能无法删除的问题

## 4.3.13

*发布日期: 2022-04-01*

### 重要变更

- 对于 Docker 镜像，配置目录 `/opt/emqx/etc` 已经从 VOLUME 列表中删除，这使用户可以更容易地使用更改后的配置来重建镜像。
- CentOS 7 Erlang 运行系统在 OpenSSL-1.1.1n（之前是 1.0）上重建，在 v4.3.13 之前，客户端使用某些密码套件时，EMQX 将无法成功握手并触发 `malformed_handshake_data` 异常。
- CentOS 8 Erlang 运行时系统在 RockyLinux 8 上重新构建。 `centos8` 将继续保留在包名中以保持向后兼容。

### 增强

- 新增命令行接口 `emqx_ctl pem_cache clean`，允许强制清除 x509 证书缓存，以在证书文件更新后立即重新加载。
- 重构 ExProto，以便匿名客户端也可以显示在 Dashboard 上。
- 桥接中的主题配置项现在可以使用 `${node}` 占位符。
- 严格模式下新增对 MQTT 报文中的 UTF-8 字符串有效性检查。设置为 `true` 时，无效的 UTF-8 字符串将导致客户端连接断开。
- MQTT-SN 网关支持会话恢复时主动同步注册主题。
- 将规则引擎浮点型数据的写入精度从为小数点后 10 位提升至 17 位。
- EMQX 将在启动时提示如何修改 Dashboard 的初始密码。

### 修复

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

## 4.3.12

*发布日期: 2022-02-11*

### 增强

- 规则引擎支持为客户端消息异常丢失事件配置规则与动作，以增强用户在这一场景的自定义处理能力
- 改进规则引擎 SQL 匹配执行过程中的相关统计指标
- 客户端模糊搜索支持 `*`， `(`，`)` 等特殊字符
- 改进 ACL 相关统计指标，解决命中 ACL 缓存导致计数不增加的问题
- Webhook 事件通知中新增 `connected_at` 字段
- 在因持有锁太久而终止客户端之前记录客户端状态

### 修复

- 修复 Metrics 接口默认情况下不返回 client.acl.deny 等认证鉴权指标的问题
- 修复订阅查询接口未返回分页数据的问题
- 修复 STOMP 处理 TCP 粘包时解析失败的问题
- 修复客户端过滤查询时会话创建时间选项不可用的问题
- 修复重启后内存告警可能不会触发的问题
- 修复 `emqx_auth_mnesia` 插件中存在用户数据时导入数据崩溃的问题

## 4.3.11

*发布日期: 2021-12-17*

### 增强

- 支持配置是否继续投递空的保留消息，以适应仍在使用 MQTT v3.1 协议的用户

### 修复

- 修复内存占用计算错误的问题
- 修复规则引擎 Webhook Action 的 Path 参数不支持使用 ${Variable} 的问题
- 修复某些情况下停止 MQTT Bridge 插件，会持续打印连接失败日志的问题

## 4.3.10

*发布日期: 2021-11-11*

### 修复

- 修复 STOMP 网关热升级失败

  Github PR: [emqx#6110](https://github.com/emqx/emqx/pull/6110)

- 修复通过 Dashboard 修改监听器配置后 emqx 将无法启动的问题

  Github PR: [emqx#6121](https://github.com/emqx/emqx/pull/6121)

### 增强

- 为 MQTT 客户端引入压力反馈

  Github PR: [emqx#6065](https://github.com/emqx/emqx/pull/6065)

## 4.3.9

*发布日期: 2021-11-02*

### 修复

- 修复集群间调用可能导致客户端进程失去响应的问题

  Github PR: [emqx#6062](https://github.com/emqx/emqx/pull/6062)

- 修复 WebHook TLS 不可用的问题

  Github PR: [emqx#5696](https://github.com/emqx/emqx/pull/5696)

- 修复 MongoDB 资源不支持域名的问题

  Github PR: [emqx#6035](https://github.com/emqx/emqx/pull/6035)

- 修复基于内置数据库的 ACL 的性能问题

  Github PR: [emqx#5885](https://github.com/emqx/emqx/pull/5885)

- 修复基于内置数据库的认证错误转码 HTTP 请求参数的问题

  Github PR: [emqx#5674](https://github.com/emqx/emqx/pull/5674)

- 修复规则引擎在集群环境下禁用规则后资源无法释放的问题

  Github PR: [emqx#5731](https://github.com/emqx/emqx/pull/5731)

- 修复 STOMP 网关若干问题

  Github PR: [emqx#6040](https://github.com/emqx/emqx/pull/6040)

- 修复包含 “\” 字符的 Client ID 无法进行模糊搜索的问题

  Github PR: [emqx#5978](https://github.com/emqx/emqx/pull/5978)

- 修复可变字节整数可能大于 4 字节的问题

  Github PR: [emqx#5826](https://github.com/emqx/emqx/pull/5826)

### 增强

- 改进客户端踢除机制

  Github PR: [emqx#6030](https://github.com/emqx/emqx/pull/6030)

- 为 LwM2M 网关添加新加密套件的支持

  Github PR: [emqx#5970](https://github.com/emqx/emqx/pull/5970)

- 支持优先级队列的交错（以避免低优先级队列枯竭）

  Github PR: [emqx#5666](https://github.com/emqx/emqx/pull/5666)

- 默认为 HTTP 认证插件关闭超级用户请求

  Github PR: [emqx#5567](https://github.com/emqx/emqx/pull/5567)

## 4.3.8

*发布日期: 2021-09-06*

### 修复

- 修复规则引擎规则导入失败的问题

  Github PR: [emqx#5512](https://github.com/emqx/emqx/pull/5512)

- 修复规则引擎 Webhook 动作中 Path 字段无法使用的问题

  Github PR: [emqx#5468](https://github.com/emqx/emqx/pull/5468)

- 修复 Force Shutdown 机制在进程挂起时无法生效的问题

  Github PR: [emqx#5460](https://github.com/emqx/emqx/pull/5460)

- 修复某些情况下 k8s 部署 EMQX 集群无法正确重启的问题

  Github PR: [emqx#5646](https://github.com/emqx/emqx/pull/5646), [emqx#5428](https://github.com/emqx/emqx/pull/5428)

- 修复 exproto 跨节点进程间调用的错误

  Github PR: [emqx#5436](https://github.com/emqx/emqx/pull/5436)

### 增强

- 为 exhook 增加自动重连机制以及请求超时的相关配置项，增强可靠性

  Github PR: [emqx#5447](https://github.com/emqx/emqx/pull/5447)

- 为 exproto 增加断连重试机制

  Github PR: [emqx#5436](https://github.com/emqx/emqx/pull/5436)

> 注: 此版本开始 CentoOS 7 要求使用 openssl 1.1.1，openssl 升级安装办法见：[FAQ - OpenSSL 版本不正确](https://docs.emqx.cn/broker/v4.3/faq/error.html#openssl-%E7%89%88%E6%9C%AC%E4%B8%8D%E6%AD%A3%E7%A1%AE)

## 4.3.7

*发布日期: 2021-08-09*

### 修复

- 修复当前 HTTP KeepAlive 行为可能导致某些服务器断开连接的问题

  Github PR: [emqx#5395](https://github.com/emqx/emqx/pull/5395)

- 修复命令行接口无法打印某些字符的问题

  Github PR: [emqx#5411](https://github.com/emqx/emqx/pull/5411)

- 修复 LwM2M 网关下发整型数字时编码错误的问题

  Github PR: [emqx#5425](https://github.com/emqx/emqx/pull/5425)

## 4.3.6

*发布日期: 2021-07-28*

### 增强

- 支持关闭 HTTP Pipelining

  Github PR: [emqx#5279](https://github.com/emqx/emqx/pull/5279)

- ACL 支持 IP 地址列表

  Github PR: [emqx#5328](https://github.com/emqx/emqx/pull/5328)

## 4.3.5

*发布日期: 2021-06-28*

### 修复

- 修复同一客户端建立多个共享订阅时可能在取消订阅后出现消息丢失的问题

  Github PR: [emqx#5098](https://github.com/emqx/emqx/pull/5098)

## 4.3.4

*发布日期: 2021-06-23*

### 修复

- 修复 CoAP 网关无法解析某些 URI 的问题

  Github Issue: [emqx#5062](https://github.com/emqx/emqx/issues/5062)
  Github PR: [emqx#5059](https://github.com/emqx/emqx/pull/5059)

- 修复多语言扩展钩子可能启动失败的问题

  Github PR: [emqx#5004](https://github.com/emqx/emqx/pull/5004)

- 修复规则引擎删除资源时如果存在依赖该资源的规则会导致 Crash 的问题

  Github PR: [emqx#4996](https://github.com/emqx/emqx/pull/4996)

- 修复 HTTP 认证与 Webhook 不支持 Query String 的问题

  Github PR: [emqx#4981](https://github.com/emqx/emqx/pull/4981)

- 保证默认配置下节点间报文的转发顺序

  Github PR: [emqx#4979](https://github.com/emqx/emqx/pull/4979)

## 4.3.3

*发布日期: 2021-06-05*

### 增强

- 数据转储支持从 HTTP 请求中获取导入数据

  Github PR: [emqx#4900](https://github.com/emqx/emqx/pull/4900)

### 修复

- 修复没有配置 JWKS 端点导致崩溃的问题

  Github PR: [emqx#4916](https://github.com/emqx/emqx/pull/4916)

- 修复 MQTT-SN 在集群环境下的订阅

  Github PR: [emqx#4915](https://github.com/emqx/emqx/pull/4915)

- 修复 Webhook 无法使用 TLS 的问题

  Github PR: [emqx#4908](https://github.com/emqx/emqx/pull/4908)

- 修复客户端多条件查询时参数错误可能导致崩溃的问题

  Github PR: [emqx#4916](https://github.com/emqx/emqx/pull/4916)

- 修复内存占用计算错误的问题

  Github PR: [emqx#4891](https://github.com/emqx/emqx/pull/4891)

## 4.3.2

*发布日期: 2021-05-27*

### 修复

- 修复主题指标监控无法在集群环境使用的问题

  Github PR: [emqx#4870](https://github.com/emqx/emqx/pull/4870)

- 修复报文解析时的一些问题

  Github PR: [emqx#4858](https://github.com/emqx/emqx/pull/4858)

- 修复 MQTT-SN 睡眠模式与 KeepAlive 机制的冲突问题

  Github PR: [emqx#4842](https://github.com/emqx/emqx/pull/4842)

- 修复客户端大量离线时可能出现崩溃的问题

  Github Issue: [emqx#4823](https://github.com/emqx/emqx/issues/4823)
  Github PR: [emqx#4824](https://github.com/emqx/emqx/pull/4824)

- 规则引擎刷新资源失败时将资源标记为不可用

  Github PR: [emqx#4821](https://github.com/emqx/emqx/pull/4821)

## 4.3.1

*发布日期: 2021-05-14*

### 修复

- 修复路由压缩之后性能消耗随主题层级数量指数级增长的问题

  Github PR: [emqx#4800](https://github.com/emqx/emqx/pull/4800)

- 修复处理大报文的性能问题

  Github Issue: [emqx#4787](https://github.com/emqx/emqx/issues/4787)
  Github PR: [emqx#4802](https://github.com/emqx/emqx/pull/4802)

- 修复新增的共享订阅策略不可用的问题

  Github Issue: [emqx#4808](https://github.com/emqx/emqx/issues/4808)
  Github PR: [emqx#4809](https://github.com/emqx/emqx/pull/4809)

- 修复了保留消息和延迟发布消息统计指标的错误实现

  Github PR: [emqx#4778](https://github.com/emqx/emqx/pull/4778), [emqx#4778](https://github.com/emqx/emqx/pull/4799)

- 确保 JSON 日志之间的换行

  Github PR: [emqx#4778](https://github.com/emqx/emqx/pull/4771)

## 4.3.0

*发布日期: 2021-05-06*

### 功能与改进

#### 构建

- 支持 Erlang/OTP 23
- 新安装包仅支持 macOS 10.14 及以上版本
- 项目调整为 umbrella 结构
- 支持使用 Elixir 编译插件

#### 性能改进

- 多语言扩展功能底层实现方式由 erlport 改为 gRPC
- 支持路由表压缩，减少内存占用，增强订阅性能，发布性能会略受影响，因此提供了关闭选项
- 优化通配符订阅性能
- 改进大量客户端离线时的处理性能

#### 安全性

- 保护 EMQX Broker 免受跨站点 WebSocket 劫持攻击
- SSL 支持 `verify` 与 `server_name_indication` 配置项
- SSL 支持证书链最大长度以及私钥文件密码配置项
- JWT 认证支持 JWKS

#### 其他

- 规则引擎新增更新资源逻辑
- 规则引擎 SQL 函数支持 unix 时间戳与 rfc3339 格式时间之间的转换
- 保持对 EMQX Broker 启动后连接失败的资源进行重试
- Websocket 监听器支持从 subprotocols 列表中选择支持的 subprotocol
- WebSocket 连接支持获取真实 IP 与 Port
- 支持 MySQL 8.0 的默认认证方法 caching_sha2_password
- 共享订阅分发策略配置为 `round_robin` 时随机选择起始点
- 共享订阅支持按源主题的 Hash 分发消息
- 支持 Mnesia 认证信息的导入导出
- 允许使用 Base64 编码的客户端证书或者客户端证书的 MD5 值作为用户名或者 Client ID
- 支持重启监听器
- 仅在正式版本中启用数据遥测功能
- 支持清除所有 ACL 缓存
- 支持 observer_cli
- Prometheus 支持集群指标
- Redis 哨兵模式支持 SSL 连接
- 支持单行日志输出，并支持 rfc3339 时间格式
- `emqx_auth_clientid` 与 `emqx_auth_usernmae` 合并为 `emqx_auth_mnesia`。请参考 [文档](https://docs.emqx.io/en/broker/v4.3/advanced/data-import-and-export.html) 将数据到旧版本导出，并导入到 4.3 中
- Docker 默认输出日志到控制台，设置 EMQX_LOG__TO=file 使日志输出到文件
- 支持输出 Json 格式的日志
- 支持 IPv6 自动探测
- 所有发行版都支持环境变量覆盖配置文件（以前仅适用于 Docker）
- 开源版支持 Dashboard 上传证书文件（以前仅适用于企业版）

### 修复

#### MQTT 协议

- 修复 MQTTT 心跳报文的处理
- 修复 MQTT 报文接收计数问题
- 限制飞行窗口的最大长度为 65535
- 修复 Server Keep Alive 生效情况下 Dashboard 中 Keep Alive 字段的值未同步的问题

#### 网关

- 修复 CoAP 连接中 ACL 配置不生效的问题
- 修复使用相同 ClientID 的 CoAP 客户端可以同时接入的问题
- 修复 MQTT-SN 睡眠模式不可用的问题
- 修复 MQTT-SN 网关在睡眠模式下会丢弃 DISCONNECT 报文的问题
- 修复 LwM2M 网关将数字编码、解码为无符号整型的问题

#### 资源

- 修复 MySQL 认证 SSL/TLS 连接功能不可用的问题
- 修复 Redis 重连失败问题

#### 其他修复

- 修复 ekka_locker 在极端条件下内存可能无限增长的问题
- 修复 MQTT 桥接功能中 `max_inflight_size` 配置项不生效的问题
- 修复 MQTT 桥接飞行窗口的问题
- 修复 MQTT 桥接功能中指标统计错误和 `retry_interval` 字段进行了多次单位转换的问题
- 修复告警持续时间计算错误的问题
- 修复过长的 Client ID 无法追踪的问题
- 修复查询客户端信息可能出现崩溃的问题
- 修复主题重写与 ACL 在发布订阅时执行顺序不一致的问题
- 修复 WebSocket 连接无法使用对端证书作为用户名的问题
- 修复认证数据无法导入的问题
- 修复 Docker 中 EMQX 可能启动失败的问题
- OOM 时快速杀死连接进程
- 修复 Clean Session 为 false 的 MQTT-SN 连接在非正常断开时没有发布遗嘱消息的问题

## 4.2.14

*发布日期: 2021-08-02*

EMQX 4.2.14 现已发布，主要包含以下改动:

**错误修复:**

- 修复热升级不可用的问题

## 4.2.13

*发布日期: 2021-06-28*

EMQX 4.2.13 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复同一客户端建立多个共享订阅时可能在取消订阅后出现消息丢失的问题

  GitHub PR: [emqx#5104](https://github.com/emqx/emqx/pull/5104)

### emqx-auth-http

**错误修复:**

- 修复请求超时后打印 crash 日志的问题

  GitHub PR: [emqx-auth-http#263](https://github.com/emqx/emqx-auth-http/pull/263)

- 支持查询字符串参数

  GitHub PR: [emqx-auth-http#264](https://github.com/emqx/emqx-auth-http/pull/264)

### emqx-web-hook

**错误修复:**

- 支持查询字符串参数

  GitHub PR: [emqx-web-hook#284](https://github.com/emqx/emqx-web-hook/pull/284)

## 4.2.12

*发布日期: 2021-05-07*

EMQX 4.2.12 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复了一个因等待 Mnesia 表超时导致 emqx 过早启动的问题

  GitHub PR: [emqx#4724](https://github.com/emqx/emqx/pull/4724)

**性能优化:**

- 优化了大批量客户端同时订阅和取消订阅的性能

  GitHub PR: [emqx#4732](https://github.com/emqx/emqx/pull/4732)
  GitHub PR: [emqx#4738](https://github.com/emqx/emqx/pull/4738)

## 4.2.11

*发布日期: 2021-04-16*

EMQX 4.2.11 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复 WebSocket 连接无法使用对端证书作为用户名的问题

  Github PR: [emqx#4574](https://github.com/emqx/emqx/pull/4574)

### emqx-management

**错误修复:**

- 修复认证数据导出导入问题

  Github PR: [emqx-management#320](https://github.com/emqx/emqx-management/pull/320)

## 4.2.10

*发布日期: 2021-04-12*

EMQX 4.2.10 现已发布，主要包含以下改动:

### emqx-management

**错误修复:**

- 导出数据时, 对 `emqx_auth_clientid` 的密码进行 base64 编码

  Github PR: [emqx-management#314](https://github.com/emqx/emqx-management/pull/314)

- MQTT 桥接飞行窗口中的消息ID引用的错误

  Github PR: [emqx-bridge-mqtt#132](https://github.com/emqx/emqx-bridge-mqtt/pull/132)

## 4.2.9

*发布日期: 2021-03-24*

EMQX 4.2.9 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复 MQTT 报文接收计数问题

  Github PR: [emqx#4425](https://github.com/emqx/emqx/pull/4425)

- 修复心跳报文的处理

  Github Issue: [emqx#4370](https://github.com/emqx/emqx/issues/4370)
  Github PR: [emqx#4425](https://github.com/emqx/emqx/pull/4425)

### emqx-auth-mnesia

**错误修复:**

- 修复了数据库存储问题与 CLI 问题

  Github PR: [emqx-auth-mnesia#54](https://github.com/emqx/emqx-auth-mnesia/pull/54)

- 修复 `password_hash` 配置项不生效的问题

  Github PR: [emqx-auth-mnesia#56](https://github.com/emqx/emqx-auth-mnesia/pull/56)

## 4.2.8

*发布日期: 2021-03-10*

EMQX 4.2.8 现已发布，主要修复了 MQTT 消息解析的问题。

## 4.2.7

*发布日期: 2021-01-29*

EMQX 4.2.7 现已发布，主要包含以下改动:

### emqx-auth-http

**错误修复:**

- 修复 HTTP 长连接在达到 Keepalive 超时时长或最大请求数时被断开导致请求丢失的情况

  Github PR: [emqx-auth-http#245](https://github.com/emqx/emqx-auth-http/pull/245)

### emqx-web-hook

**错误修复:**

- 修复 HTTP 长连接在达到 Keepalive 超时时长或最大请求数时被断开导致请求丢失的情况

  Github PR: [emqx-web-hook#272](https://github.com/emqx/emqx-web-hook/pull/272)

- 修复 SSL 证书配置问题

  Github PR: [emqx-web-hook#264](https://github.com/emqx/emqx-web-hook/pull/264)

### emqx-auth-redis

**错误修复:**

- 修复 Redis 重连失败的问题

  Github PR: [emqx-auth-redis#195](https://github.com/emqx/emqx-auth-redis/pull/195)

## 4.2.6

*发布日期: 2021-01-16*

EMQX 4.2.6 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复 remsh 使用的端口错误问题

  Github PR: [emqx#4016](https://github.com/emqx/emqx/pull/4016)

### emqx-auth-http

**功能增强:**

- HTTP 请求头部中的 host 字段使用用户配置的原始 URL

  Github PR: [emqx-auth-http#240](https://github.com/emqx/emqx-auth-http/pull/240)

**错误修复:**

- 修复 GET 请求不可用的问题

  Github PR: [emqx-auth-http#238](https://github.com/emqx/emqx-auth-http/pull/238)

### emqx-web-hook

**功能增强:**

- HTTP 请求头部中的 host 字段使用用户配置的原始 URL

  Github PR: [emqx-web-hook#256](https://github.com/emqx/emqx-web-hook/pull/256)

- URL 中未携带端口时使用默认端口

  Github PR: [emqx-web-hook#253](https://github.com/emqx/emqx-web-hook/pull/253)

**错误修复:**

- 修复 SSL 配置项解析错误问题

  Github PR: [emqx-web-hook#252](https://github.com/emqx/emqx-web-hook/pull/252)

- 修复 GET 请求不可用的问题

  Github PR: [emqx-web-hook#254](https://github.com/emqx/emqx-web-hook/pull/254)

### emqx-management

**错误修复:**

- 告警增加持续时间字段，修复前端可能由于时间不一致导致计算出错的问题

  Github PR: [emqx-management#304](https://github.com/emqx/emqx-management/pull/304)

### emqx-dashboard

**功能增强:**

- 改进告警持续时间显示

  Github PR: [emqx-dashboard#271](https://github.com/emqx/emqx-dashboard/pull/271)

### ehttpc

**错误修复:**

- 修复 gen_server:call/3 超时导致回复被送至调用进程的消息队列的问题

  Github PR: [ehttpc#2](https://github.com/emqx/ehttpc/pull/2)

## 4.2.5

*发布日期: 2020-12-23*

EMQX 4.2.5 现已发布，主要包含以下改动:

### emqx-auth-http

- 修复 HTTP 请求头部中错误的字段名

  Github PR: [emqx-auth-http#229](https://github.com/emqx/emqx-auth-http/pull/229)

### emqx-web-hook

- 更新底层 HTTP 客户端驱动，解决驱动卡死导致的连接进程失去响应的问题

  Github PR: [emqx-web-hook#240](https://github.com/emqx/emqx-web-hook/pull/240)

## 4.2.4

*发布日期: 2020-12-11*

EMQX 4.2.4 现已发布，主要包含以下改动:

### emqx

- 支持配置 SSL/TLS 证书链长度和密钥文件密码

  Github PR: [emqx#3901](https://github.com/emqx/emqx/pull/3901)

### emqx-auth-http

- 更新底层 HTTP 客户端驱动，解决驱动卡死导致的连接进程失去响应的问题

  Github PR: [emqx-auth-http#213](https://github.com/emqx/emqx-auth-http/pull/213)

### emqx-auth-mongo

- 修复没有匹配查询结果导致的类型错误问题

  Github PR: [emqx-auth-mongo#240](https://github.com/emqx/emqx-auth-mongo/pull/240)

### emqx-auth-redis

- 修复 redis 驱动在 cluster 模式下启动失败时未报错的问题

  Github PR: [emqx-auth-redis#187](https://github.com/emqx/emqx-auth-redis/pull/187)

### emqx-rel

- Helm chart 支持通过密钥拉取私有镜像

  Github PR: [emqx-rel#626](https://github.com/emqx/emqx-rel/pull/626)

- 增强安全性

  Github PR: [emqx-rel#612](https://github.com/emqx/emqx-rel/pull/612)

## 4.2.3

*发布日期: 2020-11-13*

EMQX 4.2.3 现已发布，主要包含以下改动:

### emqx-web-hook

- 支持在请求的 URL 路径中插入变量

  Github PR: [emqx-web-hook#225](https://github.com/emqx/emqx-web-hook/pull/225)
  Github Issue: [emqx-web-hook#224](https://github.com/emqx/emqx-web-hook/issues/224)

- 支持设置 Content Type

  Github PR: [emqx-web-hook#230](https://github.com/emqx/emqx-web-hook/pull/230)

### emqx-rel

- 修复 LC_ALL 不等于 en_US.UTF-8 时编译失败的问题

  Github PR: [emqx-rel#605](https://github.com/emqx/emqx-rel/pull/605)
  Github Issue: [emqx-rel#604](https://github.com/emqx/emqx-rel/issues/604)

- 支持在 emqx 容器启动前创建和运行其他服务

  Github PR: [emqx-rel#608](https://github.com/emqx/emqx-rel/pull/608)

- 允许为容器设置可变数量的配置项

  Github PR: [emqx-rel#609](https://github.com/emqx/emqx-rel/pull/609)

### emqx-sn

- 修复尝试发布遗嘱消息将导致崩溃的问题

  Github PR: [emqx-sn#169](https://github.com/emqx/emqx-sn/pull/169)

- 修复为 from 字段设置了不恰当的值的问题

  Github PR: [emqx-sn#170](https://github.com/emqx/emqx-sn/pull/170)

### emqx-rule-engine

- 保持向后兼容

  Github PR: [emqx-rule-engine#189](https://github.com/emqx/emqx-rule-engine/pull/189)

- 修复 `messages.received` 指标的统计

  Github PR: [emqx-rule-engine#193](https://github.com/emqx/emqx-rule-engine/pull/193)

### emqx-management

- 修复 `messages.received` 指标的统计

  Github PR: [emqx-management#284](https://github.com/emqx/emqx-management/pull/284)

- 使数据导入导出功能能够在集群环境下使用

  Github PR: [emqx-management#288](https://github.com/emqx/emqx-management/pull/288)

### emqx-redis

- 支持 Redis 6.0 的 SSL/TLS 单双向认证

  Github PR: [emqx-auth-redis#180](https://github.com/emqx/emqx-auth-redis/pull/180)

## 4.2.2

*发布日期: 2020-10-24*

EMQX 4.2.2 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复主题统计速率计算不准确的问题

  Github PR: [emqx#3784](https://github.com/emqx/emqx/pull/3784)

### emqx-web-hook

**功能增强:**

- 支持 `GET` 和 `DELETE` 方法

  Github PR: [emqx-web-hook#220](https://github.com/emqx/emqx-web-hook/pull/220)

- 增加 `node` 和 `disconnected_at` 字段

  Github PR: [emqx-web-hook#215](https://github.com/emqx/emqx-web-hook/pull/215)

### emqx-auth-pgsql

**错误修复:**

- 修复 `%a` 占位符不生效的问题

  Github PR: [emqx-auth-pgsql#208](https://github.com/emqx/emqx-auth-pgsql/pull/208)

### emqx-auth-mysql

**错误修复:**

- 修复 `%a` 占位符不生效的问题

  Github PR: [emqx-auth-mysql#245](https://github.com/emqx/emqx-auth-mysql/pull/245)

## 4.2.1

*发布日期: 2020-09-29*

EMQX 4.2.1 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复没有正确处理 No Local 逻辑导致消息堆积在飞行窗口和消息队列的问题

  Github PR: [emqx#3741](https://github.com/emqx/emqx/pull/3741)
  Github Issue: [emqx#3738](https://github.com/emqx/emqx/issues/3738)

### emqx-bridge-mqtt

**错误修复:**

- 修复规则引擎 MQTT 订阅无法接收消息的问题

  Github PR: [emqx-bridge-mqtt#108](https://github.com/emqx/emqx-bridge-mqtt/pull/108)

### emqx-dashboard

**错误修复:**

- 修复超长 Client ID 的显示错误问题

  Github PR: [emqx-dashboard#262](https://github.com/emqx/emqx-dashboard/pull/262)

### emqx-auth-mongo

**错误修复:**

- 修复 ACL 查询语句有多个匹配结果时仅处理了第一个匹配的问题

  Github PR: [emqx-auth-mongo#231](https://github.com/emqx/emqx-auth-mongo/pull/231)

## 4.2.0

*发布日期: 2020-09-05*

EMQX 4.2.0 现已发布，主要包含以下改动:

**功能:**

- 支持使用第三方语言编写扩展插件接入其他非 MQTT 协议，目前已支持 Java 和 Python 两种编程语言。访问 [Read Me](https://github.com/emqx/emqx-exproto/blob/master/README.md) 获取更多相关信息
- 支持修订版本间的热升级
- 新增遥测功能，收集有关 EMQX Broker 使用情况的信息以帮助我们改进产品，此功能默认开启，支持手动关闭。访问 [EMQX Telemetry](https://docs.emqx.io/broker/latest/en/advanced/telemetry.html) 获取更多遥测相关信息。
- 支持配额形式的消息流控

**增强:**

- 规则引擎支持为 MQTT 桥接创建订阅
- 规则引擎支持功能更加强大的 SQL 语法
- MySQL、PostgreSQL 等插件全面支持 IPv6、SSL/TLS
- 支持 CentOS 8、Ubuntu 20.04 操作系统和 ARM64 系统架构
- Webhook 支持配置自定义的 HTTP 头部
- 更加友好的告警机制，为开发者提供 HTTP API
- 优化保留消息性能

**调整:**

- 后续版本不再支持 Debian 8、Ubuntu 14.04 和 Raspbian 8 操作系统
- `emqx-statsd` 插件正式更名为 `emqx-prometheus`
- 发布与订阅支持独立配置主题重写规则
- 允许用户配置是否允许 WebSocket 消息包含多个 MQTT 报文，以兼容部分客户端
- 调整 RPC 端口发现策略
- ***不兼容改动：*** `emqx-auth-mnesia` 插件提供的 API 端口调整为 `api/v4/mqtt_user` 与 `api/v4/mqtt_acl`
- `emqx-auth-http` 插件默认关闭超级用户认证请求
- `emqx-bridge-mqtt` 默认关闭桥接模式

**错误修复:**

- 修复主题指标功能导致内存异常增长的问题
- 修复 LwM2M 插件没有正确获取协议版本的问题
- 修复一台机器上运行多个 emqx 实例时命令行接口无法使用的问题
- 修复 Websocket 连接不支持 IPv6 的问题

## 4.1.4

*发布日期: 2020-08-28*

EMQX 4.1.4 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复主题指标功能导致内存异常增长的问题

  Github PR: [emqx#3680](https://github.com/emqx/emqx/pull/3680)

### emqx-bridge-mqtt

**功能增强:**

- clientid 配置项支持 `${node}` 占位符，优化集群下的使用体验

  Github PR: [emqx-bridge-mqtt#99](https://github.com/emqx/emqx-bridge-mqtt/pull/99)

### emqx-management

**错误修复:**

- 修复数据迁移功能在 Windows 下不可用的问题

  Github PR: [emqx-management#262](https://github.com/emqx/emqx-management/pull/262)

### emqx-lua-hook

**错误修复:**

- 修复无法获取 Username 字段的问题

  Github PR: [emqx-lua-hook#115](https://github.com/emqx/emqx-lua-hook/pull/115)

## 4.1.3

*发布日期: 2020-08-04*

EMQX 4.1.3 现已发布，主要包含以下改动:

### emqx-management

**错误修复:**

- 为 PUBLISH API 的 payload 字段增加类型检查

  Github PR: [emqx/emqx-management#250](https://github.com/emqx/emqx-management/pull/250)

### emqx-retainer

**错误修复:**

- 修复订阅主题同时包含 '+' 和 '#' 不会下发保留消息的问题

  Github PR: [emqx/emqx-retainer#146](https://github.com/emqx/emqx-retainer/pull/146)

## 4.1.2

*发布日期: 2020-07-23*

EMQX 4.1.2 现已发布，主要包含以下改动:

### emqx

**错误修复:**

- 修复没有使用主题别名替代主题的问题

  Github PR: [emqx/emqx#3616](https://github.com/emqx/emqx/pull/3616)

- 修复某些操作占用过多 CPU 的问题

  Github PR: [emqx/emqx#3581](https://github.com/emqx/emqx/pull/3581)

### emqx-rel

**错误修复:**

- 修复以容器方式运行 emqx 时日志写满所有日志文件后控制台不再输出日志的问题

  Github PR: [emqx/emqx-rel#559](https://github.com/emqx/emqx-rel/pull/559)

## 4.1.1

*发布日期: 2020-07-03*

EMQX 4.1.1 现已发布，主要包含以下改动:

### emqx-retainer

**错误修复:**

- 修复性能问题

  Github PR: [emqx/emqx-retainer#141](https://github.com/emqx/emqx-retainer/pull/141)

### emqx-bridge-mqtt

**错误修复:**

- 将挂载点改为可选配置

  Github PR: [emqx/emqx-bridge-mqtt#84](https://github.com/emqx/emqx-bridge-mqtt/pull/84)

### emqx-rel

**错误修复:**

- 屏蔽 docker 运行时输出的控制台日志中的敏感信息

  Github Issue: [emqx/emqx-rel#524](https://github.com/emqx/emqx-rel/pull/524)

  Github PR: [emqx/emqx-rel#542](https://github.com/emqx/emqx-rel/pull/542)

  Thanks: [emqx/emqx-rel#525](https://github.com/emqx/emqx-rel/pull/525) - [daadu](https://github.com/daadu)

### emqx-lua-hook

**错误修复:**

- 修复插件卸载时没有卸载脚本和 CLI 的问题

  Github PR: [emqx/emqx-lua-hook#106](https://github.com/emqx/emqx-lua-hook/pull/106)

## 4.1.0

*发布日期: 2020-06-04*

EMQX 4.1.0 现已发布，主要包含以下改动：

**功能增强:**

- 支持多语言扩展并提供 SDK，已支持语言：Python, Java
- 支持基于主题的指标统计
- 支持插件启动时加载最新配置
- 支持消息转发时使用主题别名
- 代理订阅支持配置所有订阅选项
- 支持客户端列表的模糊查询和多条件查询
- 支持订阅列表的模糊查询
- 支持通过 Dashboard 添加简单的认证信息
- 支持跨版本数据迁移
- 支持 MQTT AUTH 报文，目前仅支持 SCRAM-SHA-1 认证机制，支持用户自行扩展
- 支持使用代理协议时获取网络地址与端口
- 增加基于 Mnesia 数据库的认证插件（在后续版本中完全替代 `emqx-auth-clientid` 与 `emqx-auth-username` 插件）
- 支持编辑规则引擎中的规则
- 通过 Docker 运行 EMQX 时支持注释配置项
- LwM2M 网关插件支持 IPv6 和同时监听多个端口
- CoAP 网关插件支持 IPv6
- JWT 认证插件支持配置 jwerl 签名格式

**错误修复:**

- 修复 `etc/emqx.conf` 为只读文件时 EMQX 无法启动的问题
- 修复连接进程在某些情况下出错崩溃的问题
- 修复浏览器不支持当前 SSL/TLS 证书的问题
- 修复 MQTT 桥接插件默认情况下不会发送心跳包的问题
- 修复异常登录检测功能没有删除过期数据导致内存增长的问题
- 修复内置 ACL 模块重新加载时没有清除 ACL 缓存的问题
- 修复 WebHook 插件中 `client.disconnected` 事件在某些情况下出错的问题
- 修复 MQTT-SN 网关插件不支持指定监听 IP 地址的问题并支持 IPv6

## 4.0.13

*发布日期: 2021-04-13*

EMQX 4.0.13 现已发布，主要包含以下改动

### emqx-management

**Bug fixes:**

- 修复数据导入导出时的问题

  Github PR: [emqx-management#319](https://github.com/emqx/emqx-management/pull/319), [emqx-management#321](https://github.com/emqx/emqx-management/pull/321)

## 4.0.11

*发布日期: 2021-04-10*

EMQX 4.0.11 现已发布，主要包含以下改动

### emqx-management

**Bug fixes:**

- 导出数据时, 对 `emqx_auth_clientid` 的密码进行 base64 编码

  Github PR: [emqx-management#316](https://github.com/emqx/emqx-management/pull/316)

## 4.0.9

*发布日期: 2021-03-12*

EMQX 4.0.9 现已发布，主要修复了 MQTT 消息解析的问题。

## 4.0.6

*发布日期: 2020-04-22*

EMQX 4.0.6 现已发布，主要包含以下改动：
  
### emqx

**错误修复:**

- 修复 flapping 检查没有删除过期数据的问题

  Github PR: [emqx/emqx#3407](https://github.com/emqx/emqx/pull/3407)
  
- 修复使用 WebSocket 时 Proxy Protocol 功能无法使用的问题

  Github PR: [emqx/emqx#3372](https://github.com/emqx/emqx/pull/3372)
  
### emqx-bridge-mqtt

**错误修复:**

- 修复默认情况不会发送心跳包的问题

  Github PR: [emqx/emqx-bridge-mqtt#67](https://github.com/emqx/emqx-bridge-mqtt/pull/67)

### emqx-rule-engine

**错误修复:**

- 修复规则引擎时间戳的错误类型

  Github Commit: [emqx/emqx-rule-engine#27ca37](https://github.com/emqx/emqx-rule-engine/commit/27ca3768602c107af71ea6b20f4518bb0f70404d)

- 修复规则引擎测试 SQL 语句功能

  Github Commit: [emqx/emqx-rule-engine#33fcba](https://github.com/emqx/emqx-rule-engine/commit/33fcba394e59fef495e2fe54883297c8d3d893e5)

## 4.0.5

*发布日期: 2020-03-17*

EMQX 4.0.5 现已发布。此版本主要进行了错误修复。

**错误修复:**

- 修复 GC 策略

  Github PR: [emqx/emqx#3317](https://github.com/emqx/emqx/pull/3317)
  
- 修复了 `Maximum-QoS` 属性的值设置错误的问题

  Github issue: [emqx/emqx#3304](https://github.com/emqx/emqx/issues/3304), [emqx/emqx#3315](https://github.com/emqx/emqx/issues/3315)
  Github PR: [emqx/emqx#3321](https://github.com/emqx/emqx/pull/3321)
  
- 修复了 EMQX 运行在 Docker 环境中时 CPU 占用率每隔 15 秒异常升高的问题

 Github issue: [emqx/emqx#3274](https://github.com/emqx/emqx/pull/3274)
  Github PR: [emqx/emqx-rel#462](https://github.com/emqx/emqx-rel/pull/462)
  
- 修复配置文件中 node.* 配置项不生效的问题

  Github issue: [emqx/emqx#3302](https://github.com/emqx/emqx/pull/3302)
  Github PR: [emqx/emqx-rel#463](https://github.com/emqx/emqx-rel/pull/463)

### emqx-rule-engine (plugin)

**错误修复:**

- 修复规则引擎不支持 Payload 为 utf-8 字符串的问题

  Github issue: [emqx/emqx#3287](https://github.com/emqx/emqx/issues/3287)
  Github PR: [emqx/emqx#3299](https://github.com/emqx/emqx/pull/3299)

### emqx-sn (plugin)

**错误修复:**

- 修复 MQTT-SN 订阅丢失的问题

  Github issue: [emqx/emqx#3275](https://github.com/emqx/emqx/issues/3275)
  Github PR: [emqx/emqx-sn#156](https://github.com/emqx/emqx-sn/pull/156)

## 4.0.4

*发布日期: 2019-03-06*

EMQX 4.0.4 现已发布。此版本主要进行了错误修复。

### emqx

**错误修复:**

- 修复 `acl_deny_action` 配置项不生效的问题

    Github issue:
    [emqx/emqx\#3266](https://github.com/emqx/emqx/issues/3266)

    Github PR: [emqx/emqx\#3286](https://github.com/emqx/emqx/pull/3286)

- 修复 `mountpoint` 配置项的错误类型

    Github issue:
    [emqx/emqx\#3271](https://github.com/emqx/emqx/issues/3271)

    Github PR: [emqx/emqx\#3272](https://github.com/emqx/emqx/pull/3272)

- 修复 `peer_cert_as_username` 配置项不生效的问题

    Github issue:
    [emqx/emqx\#3281](https://github.com/emqx/emqx/issues/3281)

    Github PR: [emqx/emqx\#3291](https://github.com/emqx/emqx/pull/3291)

- 修复连接正常关闭后仍打印错误日志的问题

    Github PR: [emqx/emqx\#3290](https://github.com/emqx/emqx/pull/3290)

### emqx-dashboard (plugin)

**错误修复:**

- 修复 Dashboard 节点下拉列表中显示空白的问题

    Github issue:
    [emqx/emqx\#3278](https://github.com/emqx/emqx/issues/3278)

    Github PR:
    [emqx/emqx-dashboard\#206](https://github.com/emqx/emqx-dashboard/pull/206)

### emqx-retainer (plugin)

**错误修复:**

- 保留消息达到最大存储数量后的行为由无法存储任何保留消息更正为可以替换已存在主题的保留消息

    Github PR:
    [emqx/emqx-retainer\#136](https://github.com/emqx/emqx-retainer/pull/136)

## 4.0.3

*发布日期: 2019-02-21*

EMQX 4.0.3 现已发布。此版本主要进行了错误修复。

### emqx

**功能增强:**

- 添加允许客户端绕过认证插件登录的选项

    Github PR: [emqx/emqx\#3253](https://github.com/emqx/emqx/pull/3253)

**错误修复:**

- 修复某些竞争条件下会打印不必要的错误日志的问题

    Github PR: [emqx/emqx\#3246](https://github.com/emqx/emqx/pull/3253)

### emqx-management (plugin)

**错误修复:**

- 移除不再使用的字段和函数以及修复字段值异常的问题

    Github PR:
    [emqx/emqx-management\#176](https://github.com/emqx/emqx-management/pull/176)

- 修复集群环境下无法获取客户端列表的问题

    Github PR:
    [emqx/emqx-management\#173](https://github.com/emqx/emqx-management/pull/173)

- 修复 HTTPS 监听选项

    Github PR:
    [emqx/emqx-management\#172](https://github.com/emqx/emqx-management/pull/172)

- 修复应用列表的返回格式

    Github PR:
    [emqx/emqx-management\#169](https://github.com/emqx/emqx-management/pull/169)

## 4.0.2

*发布日期: 2019-02-07*

EMQX 4.0.2 现已发布。此版本主要进行了错误修复和性能优化。

### emqx

**功能增强:**

- 提升 Json 编解码性能

    Github PR:
    [emqx/emqx\#3213](https://github.com/emqx/emqx/pull/3213),
    [emqx/emqx\#3230](https://github.com/emqx/emqx/pull/3230),
    [emqx/emqx\#3235](https://github.com/emqx/emqx/pull/3235)

- 压缩生成的项目大小

    Github PR: [emqx/emqx\#3214](https://github.com/emqx/emqx/pull/3214)

**错误修复:**

- 修复某些情况下没有发送 DISCONNECT 报文的问题

    Github PR: [emqx/emqx\#3208](https://github.com/emqx/emqx/pull/3208)

- 修复收到相同 PacketID 的 PUBLISH 报文时会断开连接的问题

    Github PR: [emqx/emqx\#3233](https://github.com/emqx/emqx/pull/3233)

### emqx-stomp (plugin)

**错误修复:**

- 修复最大连接数限制不生效的问题

    Github PR:
    [emqx/emqx-stomp\#93](https://github.com/emqx/emqx-stomp/pull/93)

### emqx-auth-redis (plugin)

**错误修复:**

- 修复内部模块启动失败的问题

    Github PR:
    [emqx/emqx-auth-redis\#151](https://github.com/emqx/emqx-auth-redis/pull/151)

### cowboy (dependency)

**错误修复:**

- 修复 Websocket 连接某些情况下不会发送遗嘱消息的问题

    Github Commit:
    [emqx/cowboy\#3b6bda](https://github.com/emqx/cowboy/commit/3b6bdaf4f2e3c5b793a0c3cada2c3b74c3d5e885)

## 4.0.1

*发布日期: 2019-01-17*

EMQX 4.0.1 现已发布。此版本主要进行了错误修复和性能优化。

### emqx

**功能增强:**

- force\_shutdown\_policy 默认关闭

    Github PR: [emqx/emqx\#3184](https://github.com/emqx/emqx/pull/3184)

- 支持定时全局 GC 并提供配置项

    Github PR: [emqx/emqx\#3190](https://github.com/emqx/emqx/pull/3190)

- 优化 `force_gc_policy` 的默认配置

    Github PR:
    [emqx/emqx\#3192](https://github.com/emqx/emqx/pull/3192),
    [emqx/emqx\#3201](https://github.com/emqx/emqx/pull/3201)

- 优化 Erlang VM 参数配置

    Github PR:
    [emqx/emqx\#3195](https://github.com/emqx/emqx/pull/3195),
    [emqx/emqx\#3197](https://github.com/emqx/emqx/pull/3197)

**错误修复:**

- 修复使用错误的单位导致黑名单功能异常的问题

    Github PR: [emqx/emqx\#3188](https://github.com/emqx/emqx/pull/3188)

- 修复对 `Retain As Publish` 标志位的处理并且在桥接模式下保持 `Retain` 标识位的值

    Github PR: [emqx/emqx\#3189](https://github.com/emqx/emqx/pull/3189)

- 修复无法使用多个 Websocket 监听端口的问题

    Github PR: [emqx/emqx\#3196](https://github.com/emqx/emqx/pull/3196)

- 修复会话 takeover 时 EMQX 可能不发送 DISCONNECT 报文的问题

    Github PR: [emqx/emqx\#3208](https://github.com/emqx/emqx/pull/3208)

### emqx-rule-engine

**功能增强:**

- 提供更多操作数组的 SQL 函数

    Github PR:
    [emqx/emqx-rule-engine\#136](https://github.com/emqx/emqx-rule-engine/pull/136)

- 减少未配置任何规则时对性能的影响

    Github PR:
    [emqx/emqx-rule-engine\#138](https://github.com/emqx/emqx-rule-engine/pull/138)

### emqx-web-hook

**错误修复:**

- 修复参数不匹配导致的崩溃问题

    Github PR:
    [emqx/emqx-web-hook\#167](https://github.com/emqx/emqx-web-hook/pull/167)

## 4.0.0

*发布日期: 2019-01-10*

EMQX 4.0.0 正式版现已发布。在这个版本中，我们通过重构 channel 和 session
显著地改进了吞吐性能，通过添加更多的钩子和统计指标增强了可扩展性，重新设计了规则引擎的
SQL，并优化 Edge 版本的性能表现。

### 常规

**功能增强:**

- 架构优化，大幅提高消息吞吐性能，降低了 CPU 与内存占用
- 改进 MQTT 5.0 报文处理流程
- 规则引擎支持全新的 SQL 语句
- 调整 metrics 命名并增加更多的 metrics
- 调整钩子参数并增加更多的钩子
- emqtt 提供发布与订阅的命令行接口

**错误修复:**

- 修复了 SSL 握手失败导致崩溃的问题
- 修复 `max_subscriptions` 配置不生效的问题
- 修复跨集群转发消息失序的问题
- 修复命令行接口无法获取单个主题的多条路由信息的问题

#### REST API

**功能增强:**

- 支持 IPv6
- REST API 默认监听端口由 8080 改为 8081，减少被其他应用占用的情况
- 移除所有 sessions 相关的接口
- connections 调整为 clients，并提供原先 sessions 的功能
- 支持订阅查询接口返回共享订阅的真实主题
- 支持配置默认的 AppID 与 AppSecret
- 发布消息的 REST API 支持使用 base64 编码的 payload

**错误修复:**

- 修复转码后的 URI 没有被正确处理的问题

#### 认证

**功能增强:**

- HTTP 认证插件支持用户配置自定义的 HTTP 请求头部
- clientid 与 username 认证插件重新支持用户通过配置文件配置默认的 clientid 与 username

## 3.2.7

*发布日期: 2019-12-03*

EMQX 3.2.7 版本发布。此版本主要重新支持了通过配置文件配置默认的 `username` 和 `clientid` 。

### emqx-auth-username (plugin)

功能增强:

- 重新支持了通过配置文件配置默认的 `username`

Github PR: [emqx/emqx-auth-username#127](https://github.com/emqx/emqx-auth-username/pull/127)

### emqx-auth-clientid (plugin)

功能增强:

- 重新支持了通过配置文件配置默认的 `clientid`

Github PR: [emqx/emqx-auth-clientid#123](https://github.com/emqx/emqx-auth-clientid/pull/123)

## 3.2.6

*发布日期: 2019-11-23*

EMQX 3.2.6 版本发布。此版本主要关注功能改进和错误修复。

### emqx (major)

错误修复:

- 修复通过 `gen_rpc` 向远程节点转发消息时可能失序的问题

Github PR: [emqx/emqx#3049](https://github.com/emqx/emqx/pull/3049)

- 修复认证插件崩溃会导致 `emqx`  崩溃的问题

Github PR: [emqx/emqx#3048](https://github.com/emqx/emqx/pull/3048)

## 3.2.5

*发布日期: 2019-11-15*

EMQX 3.2.5 版本发布。此版本主要进行了错误修复。

### emqx-rule-engine (plugin)

错误修复:

- 支持 SQL 关键字: FOREACH/DO/INCASE

Github Commit: [emqx/emqx-rule-engine#a962e3](https://github.com/emqx/emqx-rule-engine/commit/a962e364cfde9a7f9bbde3d4d6613625b8d00ce7)

- 支持 SQL 关键字: CASE/WHEN

Github Commit: [emqx/emqx-rule-engine#40e68e](https://github.com/emqx/emqx-rule-engine/commit/40e68e9607198613cc93d001488d40b2bfb4f23e)

- 支持在 SQL 的 WHERE 子句中比较原子与二进制

Github Commit: [emqx/emqx-rule-engine#b240cc](https://github.com/emqx/emqx-rule-engine/commit/b240cc0434815bafb5cfcd366692257336d26e8c)

- 修复 select 和 foreach 中的列验证失败

Github Commit: [emqx/emqx-rule-engine#6a1267](https://github.com/emqx/emqx-rule-engine/commit/6a1267cb1530d00972899ecb3abb7a3220e28175)

- 修复重建规则时出现竞争的问题

Github Commit: [emqx/emqx-rule-engine#af8967](https://github.com/emqx/emqx-rule-engine/commit/af8967793d4f554134955c620d9e31b8c3876445)

- 修复重发消息时没有确证设置标志的问题

Github Commit: [emqx/emqx-rule-engine#60e45c](https://github.com/emqx/emqx-rule-engine/commit/60e45c28596a6cb42437043fbba5509502a3cf41)

### minirest (plugin)

错误修复:

- 修复日志没有记录错误数据的问题

Github PR: [emqx/minirest#20](https://github.com/emqx/minirest/pull/20)

### emqx-web-hook (plugin)

错误修复:

- 修复错误的匹配

Github Commit: [emqx/emqx-web-hook#3dd041](https://github.com/emqx/emqx-web-hook/commit/3dd041afaf39eabe71ab473648d57f4b55735224)

## 3.2.4

*发布日期: 2019-10-28*

EMQX 3.2.4 版本发布。此版本主要为 Dashbaord 和 REST API 添加了 IPv6 支持，并修复了一些错误。

错误修复:

- 修复 max_subscriptions 配置不生效的问题

Github PR: [emqx/emqx#2922](https://github.com/emqx/emqx/pull/2922)

Github Issue: [emqx/emqx#2908](https://github.com/emqx/emqx/issues/2908)

### emqx-auth-mysql (plugin)

错误修复:

- 使用占位符时更安全地取值

Github PR: [emqx/emqx-auth-mysql#180](https://github.com/emqx/emqx-auth-mysql/pull/180)

Github Issue: [emqx/emqx#2937](https://github.com/emqx/emqx/issues/2937)

### emqx-dashboard (plugin)

功能增强:

- 支持使用 IPv6 访问 Dashbaord

Github PR: [emqx/emqx-dashboard#161](https://github.com/emqx/emqx-dashboard/pull/161)

### emqx-management (plugin)

功能增强:

- REST API 支持 IPv6

Github PR: [emqx/emqx-management#134](https://github.com/emqx/emqx-management/pull/134)

### emqx-delay-publish (plugin)

错误修复:

- 修复延迟发布消息失序的问题，感谢 [soldag](https://github.com/soldag) 的贡献

Github PR: [emqx/emqx-delay-publish#48](https://github.com/emqx/emqx-delay-publish/pull/48)

Github Issue: [emqx/emqx-delay-publish#15](https://github.com/emqx/emqx-delay-publish/issues/15)

### emqx-rule-engine (plugin)

功能增强:

- 优化规则引擎中 JSON Payload 解析语句

Github Repository: [emqx/emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

## 3.2.3

*发布日期: 2019-09-16*

EMQX 3.2.3 版本改动主要为错误修复。

错误修复:

- 修复 emqx 容器运行时 CPU 占用率告警异常的问题

GitHub Commit: [emqx/emqx#9cdaa7](https://github.com/emqx/emqx/commit/9cdaa71a66c44d6bfd7606f8e64bc6670f619cdf)

- 修复消息过期机制不生效的问题

Github Commit: [emqx/emqx#31671f](https://github.com/emqx/emqx/commit/31671f5ee5516e04ca6c648679f030b790c84fd9)

- 修复占位符在 mountpoint 中不生效的问题

Github Commit: [emqx/emqx#58ba22](https://github.com/emqx/emqx/commit/58ba22dfc79ce81ac74fffae60a624d2238585ca)

### emqx-dashboard (plugin)

错误修复:

- 修复 SSL 无法使用的问题

Github Commit: [emqx/emqx-dashboard#272a42](https://github.com/emqx/emqx-dashboard/commit/272a42b5ac7b28f52e5e71fae540e47278fac9d5)

## 3.2.2

*发布日期: 2019-08-03*

EMQX 3.2.2 版本改动主要为错误修复。

功能增强:

- 扩展 `gen_rpc` 配置

Github PR: [emqx/emqx#2732](https://github.com/emqx/emqx/pull/2732)

### emqx-rule-engine (plugin)

错误修复:

- 修复测试 URL 连通性的问题

Github PR: [emqx/emqx-rule-engine#88](https://github.com/emqx/emqx-rule-engine/pull/88)

### emqx-dashboard (plugin)

功能增强:

- 增加帮助页面

### ekka (dependency)

错误修复:

- 修复释放锁可能导致崩溃的问题

Github PR: [emqx/ekka#60](https://github.com/emqx/ekka/pull/60)

## 3.2.1

*发布日期: 2019-07-20*

EMQX 3.2.1 版本改动主要包括错误修复与性能增强。

功能增强:

- 优化 `gen_rpc` 的调用

Github PR: [emqx/emqx#2694](https://github.com/emqx/emqx/pull/2694)

- 支持使用 hostname 自动发现 k8s 集群

Github PR: [emqx/emqx#2699](https://github.com/emqx/emqx/pull/2699)

- 将默认 uptime 心跳时间改为 30s

Github PR: [emqx/emqx#2696](https://github.com/emqx/emqx/pull/2696)

错误修复:

- 修复 WebSocket 非正常下线时出现 crash 的问题

Github PR: [emqx/emqx#2697](https://github.com/emqx/emqx/pull/2697)

- 修复 Session 异常关闭时，ws_channel 仍然在线的问题

Github PR: [emqx/emqx#2704](https://github.com/emqx/emqx/pull/2704)

### emqx-rule-engine (plugin)

功能增强:

- 增强 republish 动作参数

Github PR: [emqx/emqx-rule-engine#81](https://github.com/emqx/emqx-rule-engine/pull/81)

错误修复:

- 修复使用 '.' 筛选 payload 字段失败的问题

Github PR: [emqx/emqx-rule-engine#83](https://github.com/emqx/emqx-rule-engine/pull/83)

### emqx-dashboard (plugin)

错误修复:

- 修复 Dashboard 资源列表在 Safari 下渲染错误的问题

Github PR: [emqx/emqx-dashboard#124](https://github.com/emqx/emqx-dashboard/pull/124) , [emqx/emqx-dashboard#125](https://github.com/emqx/emqx-dashboard/pull/125) , [emqx/emqx-dashboard#126](https://github.com/emqx/emqx-dashboard/pull/126)

### emqx-lwm2m (plugin)

功能增强:

- 兼容 LwM2M 1.1 版本客户端登录

Github Commit: [emqx/emqx-lwm2m#1c03bf](https://github.com/emqx/emqx-lwm2m/commit/1c03bf3b6a9cae7ed52f87ee219e9dd9d8824892)

### emqx-rel (build project)

功能增强:

- 内置 rebar3 脚本

Github PR: [emqx/emqx-rel#394](https://github.com/emqx/emqx-rel/pull/394)

- EMQX Windows 服务延迟启动

Github PR: [emqx/emqx-rel#395](https://github.com/emqx/emqx-rel/pull/395)

## 3.2.0

*发布日期: 2019-07-12*

EMQX 3.2.0 版本主要优化和改进了规则引擎。

### 规则引擎

改进规则引擎功能和规则管理界面(Dashboard)，支持更多动作。

### 项目构建

改用 rebar3 构建项目。

### MQTT Broker 桥接

将 MQTT bridge 从 emqx 项目分离出来作为一个独立的插件，并提升了 RPC bridge 的性能。

### HTTP 插件

支持 HTTPs。

### 集群 (ekka)

改善集群稳定性。

### 其他插件和依赖

修复 Windows 服务注册问题。

## 3.1.2

*发布日期: 2019-06-06*

EMQX 3.1.1 版本发布。此版本改动主要包括错误修复、稳定性增强。

### EMQX Core

Bug fixes:

- 修复 [emqx/emqx: issue #2595](https://github.com/emqx/emqx/issues/2595)

Github PR: [emqx/emqx#2601](https://github.com/emqx/emqx/pull/2601)

- 修复无法设置日志等级的问题

Github PR: [emqx/emqx#2600](https://github.com/emqx/emqx/pull/2600)

- 修复返回值不匹配的问题

Github PR: [emqx/emqx#2560](https://github.com/emqx/emqx/pull/2560)

- 热修复 `emqx_sn` 与 `emqx_coap` 插件

Github PR: [emqx/emqx#2556](https://github.com/emqx/emqx/pull/2556)

### emqx-coap (plugin)

错误修复:

- 修复无法发布消息的问题

Github PR: [emqx/emqx-coap#120](https://github.com/emqx/emqx-coap/pull/120)

### ekka (deps)

错误修复:

- 修复导致 `emqx_sm_locker` 崩溃的问题

Github PR: [emqx/ekka#54](https://github.com/emqx/ekka/pull/54)

- 修复 k8s 无法使用 dns 集群的问题

Github PR: [emqx/ekka#53](https://github.com/emqx/ekka/pull/53)

- 修复 etcd 集群不可用的问题

Github PR: [emqx/ekka#52](https://github.com/emqx/ekka/pull/52)

## 3.1.1

*发布日期: 2019-05-10*

EMQX 3.1.1 版本发布。此版本改动主要包括错误修复、稳定性增强。

功能增强:

- 增大单条日志可打印的最大字符数量

Github PR: [emqx/emqx#2509](https://github.com/emqx/emqx/pull/2509)

- `force_shutdown_policy` 将根据系统位数使用不同的默认值

Github PR: [emqx/emqx#2515](https://github.com/emqx/emqx/pull/2515)

错误修复:

- 正确地配置和使用 `long_gc` 与 `long_schedule`

Github PR: [emqx/emqx#2504](https://github.com/emqx/emqx/pull/2504) , [emqx/emqx#2513](https://github.com/emqx/emqx/pull/2513)

- 修复没有更新 `suboptions/count` 的问题

Github PR: [emqx/emqx#2507](https://github.com/emqx/emqx/pull/2507)

### emqx-lwm2m (plugin)

错误修复:

- 修复 mountpoint 没有生效的问题

Github PR: [emqx/emqx-lwm2m#34](https://github.com/emqx/emqx-lwm2m/pull/34)

- 修复消息无法被 `emqx-web-hook` 转发的问题

Github PR: [emqx/emqx-lwm2m#35](https://github.com/emqx/emqx-lwm2m/pull/35)

## 3.1.0

*发布日期: 2019-04-26*

EMQX 3.1.0 版本发布。此版本改动主要包括全面支持规则引擎、引入 storm 模块以支持 edge storm、 重构 flapping 代码。

功能改进:

- 添加 emqx_ct_helpers 依赖，并重构测试用例

Github PR: [emqx/emqx#2480](https://github.com/emqx/emqx/pull/2480)

- 重构 flapping 代码

Github PR: [emqx/emqx#2476](https://github.com/emqx/emqx/pull/2476)

### emqx-management (plugin)

问题修复:

- 修复 listeners acceptors 的值没有正确获取的问题

Github PR: [emqx/emqx-management#76](https://github.com/emqx/emqx-management/pull/76)

### emqx-rule-engine (plugin)

功能改进:

- 支持规则动作参数的验证

Github PR: [emqx/emqx-rule-engine#b28318](https://github.com/emqx/emqx-rule-engine/commit/b283184dcbb207e8d58ac308c027a093a4f4ab88)

- 删除资源时检查是否存在依赖

Github PR: [emqx/emqx-rule-engine#fa75b9](https://github.com/emqx/emqx-rule-engine/commit/fa75b952efb7951bc57242adc8e953dbbba6b2ed)

- 从 republish 动作中移除 `from` 参数

Github PR: [emqx/emqx-rule-engine#8721eb](https://github.com/emqx/emqx-rule-engine/commit/8721ebe583d5426f239b5b1f044fe381bf4ea0b7)

- 修复了 SQL where 子句不能处理整数的问题

Github PR: [emqx/emqx-rule-engine#c9c761](https://github.com/emqx/emqx-rule-engine/commit/c9c7616f86019657861dff408854e9c5238d666b)

### emqx-storm (plugin)

功能改进:

- 支持 edge storm

Github Repository: [emqx/emqx-storm](https://github.com/emqx/emqx-storm)

## 3.0.1

*发布日期: 2019-01-25*

EMQX 3.0.1 版本发布。此版本主要包含功能改进和错误修复。

功能改进:

- 为 emqx edge 增加 +L 虚拟机参数以减少内存

Github PR: [emqx/emqx#2110](https://github.com/emqx/emqx/pull/2110)

- 简化修改日志输出等级的命令

Github PR: [emqx/emqx#2115](https://github.com/emqx/emqx/pull/2115)

- 重构 bridge 代码; 支持 bridge 消息持久化

Github PR: [emqx/emqx#2160](https://github.com/emqx/emqx/pull/2160) , [emqx/emqx#2117](https://github.com/emqx/emqx/pull/2117) , [emqx/emqx#2113](https://github.com/emqx/emqx/pull/2113) , [emqx/emqx#2108](https://github.com/emqx/emqx/pull/2108) , [emqx/emqx#2053](https://github.com/emqx/emqx/pull/2053)

- 优化路由匹配

Github PR: [emqx/emqx#2124](https://github.com/emqx/emqx/pull/2124)

- 改进 'emqx_client' 模块设计

Github PR: [emqx/emqx#2137](https://github.com/emqx/emqx/pull/2137)

- 改进 'emqx_pool' 模块的设计

Github PR: [emqx/emqx#2138](https://github.com/emqx/emqx/pull/2138)

- 改进共享订阅调度实现

Github PR: [emqx/emqx#2144](https://github.com/emqx/emqx/pull/2144)

- 支持重启 emqx 时重新生成配置

Github PR: [emqx/emqx#2175](https://github.com/emqx/emqx/pull/2175)

问题修复:

- 修复对端关闭连接时崩溃的问题

Github PR: [emqx/emqx#2074](https://github.com/emqx/emqx/pull/2074)

- 修复客户端正常断开连接时依旧发送遗嘱消息的问题

Github PR: [emqx/emqx#2156](https://github.com/emqx/emqx/pull/2156)

### emqx-lwm2m

问题修复:

- 移除认证功能

GitHub PR: [emqx/emqx-lwm2m#14](https://github.com/emqx/emqx-lwm2m/pull/14)

### emqx-auth-username

问题修复:

- 支持可选的加密模式

GitHub PR: [emqx/emqx-auth-usernmae#64](https://github.com/emqx/emqx-auth-username/pull/64)

### emqx-auth-clientid

功能改进:

- 支持可选的加密模式

GitHub PR: [emqx/emqx-auth-clientid#52](https://github.com/emqx/emqx-auth-username/pull/52)

### emqx-management

功能改进:

- 增加 'plugins reload \<Name>' CLI 命令，支持重载插件时重新生成配置

Github PR: [emqx/emqx-management#30](https://github.com/emqx/emqx-management/pull/30)

## 3.0.0

*发布日期: 2018-12-22*

EMQX 3.0.0 版本，重新设计了订阅的 ETS 表，通过重构模块和调节 erlang 虚拟机参数提升了 EMQ 性能

功能改进:

- 将虚拟机参数移动到单独的 vm.args 文件

Github PR: [emqx/emqx#2033](https://github.com/emqx/emqx/pull/2033) , [emqx/emqx#2057](https://github.com/emqx/emqx/pull/2057) , [emqx/emqx#2070](https://github.com/emqx/emqx/pull/2070)

- 为遗嘱消息主题增加格式校验和 ACL 检查

Github PR: [emqx/emqx#2075](https://github.com/emqx/emqx/pull/2075)

- 增加 ACL 检查返回拒绝时是否断开客户端连接的配置选项

Github PR: [emqx/emqx#2059](https://github.com/emqx/emqx/pull/2059)

- 重构 session 监控树

Github PR: [emqx/emqx#2077](https://github.com/emqx/emqx/pull/2077)

- 增加 'active_n' 选项以优化 emqx_connection 的 CPU 占用率

Github PR: [emqx/emqx#2060](https://github.com/emqx/emqx/pull/2060)

- 支持客户端批量下线

Github PR: [emqx/emqx#2060](https://github.com/emqx/emqx/pull/2060)

- 增加订阅表分片机制

Github PR: [emqx/emqx#2044](https://github.com/emqx/emqx/pull/2044)

- 重构 'emqx_gc' 模块

Github PR: [emqx/emqx#2090](https://github.com/emqx/emqx/pull/2090)

问题修复:

- 修复 Topic Alias Maximum 的错误实现

Github PR: [emqx/emqx#2074](https://github.com/emqx/emqx/pull/2074)

- 修复部分情况下不会发送遗嘱消息的错误

Github PR: [emqx/emqx#2068](https://github.com/emqx/emqx/pull/2068)

### emqx-auth-ldap

功能改进:

- 更好的设计

GitHub PR: [emqx/emqx-auth-ldap#46](https://github.com/emqx/emqx-auth-ldap/pull/46)

### emqx-lua-hook

问题修复:

- 修复测试用例

GitHub PR: [emqx/emqx-lua-hook#45](https://github.com/emqx/emqx-lua-hook/pull/45)

### emqx-management

功能改进:

- 为 REST API 增加测试用例，并规范返回的响应格式

Github PR: [emqx/emqx-management#21](https://github.com/emqx/emqx-management/pull/21)

## 2.3.11

*发布日期: 2018-07-23*

### Bugfix and Enhancements

Fix the getting config REST API which throws exceptions.

Support to restart listeners when emqttd is running.

Specify a fixed tag for the dependency libraries.

### emq-auth-jwt

Fix token verification with jwerl 1.0.0

### emq-auth-mongo

Support $all variable in ACL query. (emq-auth-mongo#123)

Support both clientid and username variables in all queries. (emq-auth-mongo#123)

## 2.3.10

*发布日期: 2018-06-27*

### Bugfix and Enhancements

Upgrade the esockd library to v5.2.2

### emq-auth-http

Ignore auth on ignore in body, allows for chaining methods

## 2.3.9

*发布日期: 2018-05-20*

### Bugfix and Enhancements

Bugfix: check params for REST publish API (#1599)

Upgrade the mongodb library to v3.0.5

### esockd

Bugfix: proxy protocol - set socket to binary mode (#78)

## 2.3.8

*发布日期: 2018-05-11*

### Bugfix and Enhancements

Bugfix: unregister users CLI when unload emq_auth_username (#1588)

Bugfix: Should be an info level when change CleanSession (#1590)

Bugfix: emqttd_ctl crashed when emq_auth_usename doesn't exist (#1588)

### emq-auth-mongo

Improve: Support authentication database (authSource) (#116)

## 2.3.7

*发布日期: 2018-04-22*

### Bugfix and Enhancements

Bugfix: fixed spec of function setstats/3 (#1575)

Bugfix: clean dead persistent session on connect (#1575)

Bugfix: dup flag not set when re-deliver (#1575)

Bugfix: Upgrade the lager_console_backend config (#1575)

Improve: Support set k8s namespace (#1575)

Upgrade the ekka library to v0.2.3 (#1575)

Improve: move PIPE_DIR dir from /tmp/${WHOAMI}_erl_pipes/$NAME/ to /$RUNNER_DATA_DIR/${WHOAMI}\_erl_pipes/$NAME/ (emq-relx#188)

### emq-auth-http

Improve: Retry 3 times when httpc:request occurred socket_closed_remotely error (emq-auth-http#70)

## 2.3.6

*发布日期: 2018-03-25*

### Bugfix and Enhancements

Security: LWT message checking the ACL (#1524)

Bugfix: Retain msgs should not be sent to existing subscriptions (#1529)

### emq-auth-jwt

Validate JWT token using a expired field (#29)

## 2.3.5

*发布日期: 2018-03-03*

### Bugfix and Enhancements

Feature: Add etc/ssl_dist.conf file for erlang SSL distribution (emq-relx#178)

Feature: Add node.ssl_dist_optfile option and etc/ssl_dist.conf file (#1512)

Feature: Support Erlang Distribution over TLS (#1512)

Improve: Tune off the 'tune_buffer' option for external MQTT connections (#1512)

### emq-sn

Clean registered topics if mqtt-sn client send a 2nd CONNECT in connected state (#76)

Upgrade the esockd library to v5.2.1 (#76)

### emq-auth-http

Remove 'password' param from ACL and superuser requests (#66)

## 2.3.4

*发布日期: 2018-01-29*

### Bugfix and Enhancements

Feature: Forward real client IP using a reverse proxy for websocket (#1335)

Feature: EMQ node.name with link local ipv6 address not responding to ping (#1460)

Feature: Add PROTO_DIST_ARG flag to support clustering via IPv6 address. (#1460)

Bugfix: retain bit is not set when publishing to clients (when it should be set). (#1461)

Bugfix: Can't search topic on web dashboard (#1473)

### emq-sn

Bugfix: CONNACK is not always sent to the client (emq-sn#67)

Bugfix: Setting the port to ::1:2000 causes error (emq-sn#66)

## 2.3.3

*发布日期: 2018-01-08*

### Bugfix and Enhancements

Add a full documentation for emq.conf and plugins.

Repair a dead link in README - missing emq-lwm2m. (#1430)

Subscriber with wildcard topic does not receive retained messages with sub topic has $ sign (#1398)

Web Interface with NGINX Reverse Proxy not working. (#953)

### emq-dashboard

Add dashboard.default_user.login , dashboard.default_user.password options to support configuring default admin.

### emq-modules

The emq-modules rewrite config is not right. (#35)

### emq-docker

Upgrade alpine to 3.7 (#31)

### emq-packages

Support ARM Platform (#12)

## 2.3.2

*发布日期: 2017-12-26*

### Bugfix and Enhancements

Support X.509 certificate based authentication (#1388)

Add proxy_protocol, proxy_protocol_timeout options for ws/wss listener.

Cluster discovery etcd nodes key must be created manually. (#1402)

Will read an incorrect password at the last line of emq_auth_username.conf (#1372)

How can I use SSL/TLS certificate based client authentication? (#794)

Upgrade the esockd library to v5.2.

### esockd

Improve the parser of proxy protocol v2.

Add 'send_timeout', 'send_timeout_close' options.

Rename esockd_transport:port_command/2 function to async_send/2.

Add test case for esockd_transport:async_send/2 function.

Add esockd_transport:peer_cert_subject/1, peer_cert_common_name/1 functions.

### emq-auth-mysql

Update depends on emqtt/mysql-otp.

Fixed the issue that Cannot connect to MySQL 5.7 (#67).

### emq-relx

Fix mergeconf/3 appending line break error. (#152)

### emq-sn

Fix crash in emq_sn_gateway:transform() function which handles SUBACK. (#57)

Define macro SN_RC_MQTT_FAILURE. (#59)

### emq-web-hook

Filter auth_failure client for disconnected hook. (#30)

## 2.3.1

*发布日期: 2017-12-03*

### Bugfix and Enhancements

Remove the unnecessary transactions to optimize session management.

Should not exit arbitrarily when clientid conflicts in mnesia.

Change the default value of 'mqtt.session.enable_stats' to 'on'.

The DUP flag should be set to 0 for all QoS0 messages. (emqttd#1319)

Fix the 'no function clause' exception. (emqttd#1293)

The retained flags should be propagated for bridge. (emqttd#1293)

The management API should listen on 0.0.0.0:8080. (emqttd#1353)

Fast close the invalid websocket in init/1 function.

erlang:demonitor/1 the reference when erasing a monitor. (emqttd#1340)

### emq-retainer

Don't clean the retain flag after the retained message is stored.

Add three CLIs for the retainer plugin. (emq-retainer#38)

### emq-dashboard

Refactor(priv/www): improve the routing page. (emq-dashboard#185)

### emq-modules

Turn off the subscription module by default. (emq-modules#26)

### emq-sn

Add an integration test case for sleeping device.

Do not send will topic if client is kicked out.

Prevent crash information in log when emq_sn_gateway getting timeout, since it is a possible procedure.

### emq-relx

Support node cookie value with = characters. (emq-relx#146)

### mochiweb

Improve Req:get(peername) funciton to support x-forwarded-for and x-remote-port . (emqtt/mochiweb#9)

## 2.3.0 "Passenger's Log"

*发布日期: 2017-11-20*

EMQ 2.3.0 版本正式发布，改进了 PubSub 设计与消息路由性能，更新 EMQ 自带的自签名 SSL 证书，改进 Dashboard 界面与 API 设计。

### Bugfix and Enhancements

Fix the issue that Retained message is not sent for Subscribe to existing topic. (emqttd#1314)

Fix the issue that The DUP flag MUST be set to 0 for all QoS0 messages.(emqttd#1319)

Improve the pubsub design and fix the race-condition issue. (emqttd#PR1342)

Crash on macOS High Sierra (emqttd#1297)

### emq-dashboard Plugin (emq-dashboard#PR174)

Upgraded the 'subscriptions' RESTful API.

Improvement of the auth failure log. (emq-dashboard#59)

### emq-coap Plugin (emq-coap#PR61)

Replaced coap_client with er_coap_client.

Fix: correct the output format of coap_discover() to enable ".well-known/core".

Refactor the coap_discover method.

### emq-relx

Upgraded the bin/nodetool script to fix the rpcterms command.

### emq-web-hook Plugin

Fix the emq_web_hook plugin getting username from client.connected hook. (emq-web-hook#19)

### emq-auth-jwt Plugin(emq-auth-jwt#PR15)

Added test cases for emq_auth_jwt.

Fix jwt:decode/2 functions's return type.

### emq-auth-mongo Plugin(emq-auth-mongo#PR92)

Updated the default MongoDB server configuration.

## 2.2 正式版 "Nostalgia"

*发布日期: 2017-07-08*

*版本别名: Nostalgia*

EMQ-2.2.0 版本正式发布！EMQ R2.2 版本完整支持 CoAP(RFC 7252)、MQTT-SN 协议，支持 Web Hook、Lua Hook、Proxy Protocol V2，支持 Elixir 语言插件开发。

Feature: Add 'listeners restart/stop' CLI command (emqttd#1135)

Bugfix: Exit Code from emqttd_ctl (emqttd#1133)

Bugfix: Fix spec errors found by dialyzer (emqttd#1136)

Bugfix: Catch exceptions thrown from rpc:call/4 (emq-dashboard#128)

Bugfix: Topic has been decoded by gen-coap, no conversion needed (emq-coap#43)

## 2.1.2

*发布日期: 2017-04-21*

Fix emqttd_ctl sessions list CLI

Newline character in emq.conf causing error;(emqttd#1000)

Fix crash caused by duplicated PUBREC packet (emqttd#1004)

Unload the 'session.created' and 'session.teminated' hooks (emq-plugin-template)

## 2.1.1

*发布日期: 2017-04-14*

Localhost:8083/status returns 404 when AWS LB check the health of EMQ (emqttd#984)

Https listener not working in 2.1.0 as in 2.0.7 (emq-dashboard#105)

Fix mqtt-sn Gateway not working (emq-sn#12)

Upgrade emq-sn Plugin (emq-sn#11)

Upgrade emq-coap Plugin (emq-coap#21)

## 2.1.0

*发布日期: 2017-04-07*

The stable release of 2.1 version.

Trouble with auth.mysql.acl_query (emq-auth-mysql#38)

Filter the empty fields in ACL table (emq-auth-mysql#39)

## 2.0.7

*发布日期: 2017-01-20*

The Last Maintenance Release for EMQ 2.0, and support to build RPM/DEB Packages.

emq-auth-http#9: Update the priv/emq_auth_http.schema, cuttlefish:unset() if no super_req/acl_req config exists

emq-auth-mongo#31: cuttlefish:unset() if no ACL/super config exists

emq-dashboard#91: Fix the exception caused by binary payload

emq-relx#21: Improve the binemqttd.cmd batch script for windows

emqttd#873: Documentation: installing-from-source

emqttd#870: Documentation: The word in Documents is wrong

emqttd#864: Hook 'client.unsubscribe' need to handle 'stop'

emqttd#856: Support variables in etc/emq.conf: {{ runner_etc_dir }}, {{ runner_etc_dir }}, {{ runner_data_dir }}

## 2.0.6

*发布日期: 2017-01-08*

Upgrade the [esockd](https://github.com/emqtt/esockd) library to v4.1.1

esockd#41: Fast close the TCP socket if ssl:ssl_accept failed

emq-relx#15: The EMQ 2.0 broker cannot run on Windows.

emq-auth-mongo#31: Mongodb ACL Cannot work?

## 2.0.5

*发布日期: 2016-12-24*

emq-auth-http#9: Disable ACL support

emq-auth-mongo#29: Disable ACL support

emq-auth-mongo#30: {datatype, flag}

## 2.0.4

*发布日期: 2016-12-16*

emqttd#822: Test cases for SSL connections

emqttd#818: trap_exit to link WebSocket process

emqttd#799: Can't publish via HTTPS

## 2.0.3

*发布日期: 2016-12-12*

emqttd#796: Unable to forbidden tcp lisener

emqttd#814: Cannot remove a 'DOWN' node from the cluster

emqttd#813: Change parameters order

emqttd#795: Fix metrics of websocket connections

emq-dashboard#88: Rename the default topic from “/World” to “world”

emq-dashboard#86: Lookup all online clients

emq-dashboard#85: Comment the default listener port

emq-mod-retainer#3: Retained messages get lost after EMQTT broker restart.

## 2.0.2

*发布日期: 2016-12-05*

emqttd#787: Stop plugins before the broker stopped, clean routes when a node down

emqttd#790: Unable to start emqttd service if username/password contains special characters

emq-auth-clientid#4: Improve the configuration of emq_auth_clientid.conf to resolve emqttd#790

emq-auth-username#4: Improve the configuration of emq_auth_username.conf to resolve emqttd#790

## 2.0.1

*发布日期: 2016-11-30*

emqttd#781: 更新项目 README 到 2.0 版本

emq_dashboard#84: 显示节点集群状态

emq_dashboard#79: 集群节点采用 disc_copies 存储 mqtt_admin 表

emq_auth_clientid: 集群节点采用 disc_copies 存储 mqtt_auth_clientid 表

emq_auth_username: 集群节点采用 disc_copies 存储 mqtt_auth_username 表

emq_mod_subscription#3: 删除 emq_mod_subscription 表与 module.subscription.backend 配置

emq_plugin_template#5: 插件停止时注销认证/ACL 模块

## 2.0 正式版 "西湖以西"

*发布日期: 2016-11-24*

*版本别名: 西湖以西(West of West Lake)*

EMQ-2.0 版本正式发布！EMQ-1.0 版本产品环境下已支持 900K 并发连接，EMQ-2.0 版本重构了整个项目架构并正式支持共享订阅功能:

1. 支持共享订阅(Shared Subscription)与本地订阅(Local Subscription)，解决 MQTT 协议负载平衡消费问题；
2. 支持 CoAP(RFC 7252)、MQTT-SN 协议和网关，支持 CoAP、MQTT-SN 客户端与 MQTT 客户端互通；
3. 重构配置文件格式与加载方式，支持用户友好的'K = V'文件格式，支持操作系统环境变量；
4. 增加了扩展钩子和大量的认证插件，支持与大部分数据库或 NoSQL 的认证集成；
5. 支持全平台编译部署，Linux/Unix/Windows 以及 ARM 平台网关，支持 Docker 镜像制作。

### 共享订阅(Shared Subscription)

共享订阅(Shared Subscription)支持在多订阅者间采用分组负载平衡方式派发消息:

```bash
---------
|       | --Msg1--> Subscriber1
Publisher--Msg1,Msg2,Msg3-->|  EMQ  | --Msg2--> Subscriber2
|       | --Msg3--> Subscriber3
---------
```

使用方式: 订阅者在主题(Topic)前增加'$queue'或'$share/\<group>/'前缀。

### 本地订阅(Local Subscription)

本地订阅(Local Subscription)只在本节点创建订阅与路由表，不会在集群节点间广播全局路由，非常适合物联网数据采集应用。

使用方式: 订阅者在主题(Topic)前增加'$local/'前缀。

### erlang.mk 与 relx

2.0 版本分离 [emqttd](https://github.com/emqtt/emqttd) 主项目和发布项目 [emq-relx](https://github.com/emqtt/emq-relx) , 采用 [erlang.mk](https://erlang.mk) 和 [relx](https://github.com/erlware/relx) 编译发布工具替换 1.x 版本使用的 rebar，项目可以跨平台在 Linux/Unix/Windows 系统下编译。

### CoAP 协议支持

2.0 版本支持 CoAP 协议(RFC7252)，支持 CoAP 网关与 MQTT 客户端互通。

CoAP 插件: [https://github.com/emqtt/emq_coap](https://github.com/emqtt/emq_coap)

### MQTT-SN 协议支持

2.0 版本支持 MQTT-SN 协议，支持 MQTT-SN 网关与 MQTT 客户端互通。

MQTT-SN 插件: [https://github.com/emqtt/emq_sn](https://github.com/emqtt/emq_sn)

### 'K = V'格式配置文件

2.0 版本支持用户友好的'K = V'格式配置文件 etc/emq.conf:

```bash
node.name = emqttd@127.0.0.1

...

mqtt.listener.tcp = 1883

...
```

### 操作系统环境变量

2.0 版本支持操作系统环境变量。启动时通过环境变量设置 EMQ 节点名称、安全 Cookie 以及 TCP 端口号:

```bash
EMQ_NODE_NAME=emqttd@127.0.0.1
EMQ_NODE_COOKIE=emq_dist_cookie
EMQ_MAX_PORTS=65536
EMQ_TCP_PORT=1883
EMQ_SSL_PORT=8883
EMQ_HTTP_PORT=8083
EMQ_HTTPS_PORT=8084
```

### Docker 镜像支持

EMQ-2.0 版本支持 Docker 镜像制作，Dockerfile 开源在: [https://github.com/emqtt/emq_docker](https://github.com/emqtt/emq_docker)

### Windows 平台支持

2.0 版本完整支持 Windows 平台的编译、发布与运行，支持 Windows 平台下的'emqttd_ctl'控制命令，支持在 Windows 节点间的集群。

### 问题与改进

- 764: add mqtt.cache_acl option

- 667: Configuring emqttd from environment variables

- 722: mqtt/superuser calls two times emqtt_auth_http

- 754: "-heart" option for EMQ 2.0

- 741: emq_auth_redis cannot use hostname as server address

### 扩展插件

2.0 版本发布的认证与扩展插件列表:

| 插件                                                                    | 说明                       |
| ----------------------------------------------------------------------- | -------------------------- |
| [emq_dashboard](https://github.com/emqtt/emqttd_dashboard)            | Web 控制台插件(默认加载)   |
| [emq_auth_clientid](https://github.com/emqtt/emq_auth_clientid)       | ClientId 认证插件          |
| [emq_auth_username](https://github.com/emqtt/emq_auth_username)       | 用户名、密码认证插件       |
| [emq_auth_ldap](https://github.com/emqtt/emq_auth_ldap)               | LDAP 认证/访问控制         |
| [emq_auth_http](https://github.com/emqtt/emq_auth_http)               | HTTP 认证/访问控制         |
| [emq_auth_mysql](https://github.com/emqtt/emq_auth_mysql)             | MySQL 认证/访问控制        |
| [emq_auth_pgsql](https://github.com/emqtt/emq_auth_pgsql)             | PostgreSQL 认证/访问控制   |
| [emq_auth_redis](https://github.com/emqtt/emq_auth_redis)             | Redis 认证/访问控制        |
| [emq_auth_mongo](https://github.com/emqtt/emq_auth_mongo)             | MongoDB 认证/访问控制      |
| [emq_mod_rewrite](https://github.com/emqtt/emq_mod_rewrite)           | 重写主题(Topic)插件        |
| [emq_mod_retainer](https://github.com/emqtt/emq_mod_retainer)         | Retain 消息存储模块        |
| [emq_mod_presence](https://github.com/emqtt/emq_mod_presence)         | 客户端上下线状态消息发布   |
| [emq_mod_subscription](https://github.com/emqtt/emq_mod_subscription) | 客户端上线自动主题订阅     |
| [emq_coap](https://github.com/emqtt/emq_coap)                         | CoAP 协议支持              |
| [emq_sn](https://github.com/emqtt/emq_sn)                             | MQTT-SN 协议支持           |
| [emq_stomp](https://github.com/emqtt/emq_stomp)                       | Stomp 协议支持             |
| [emq_sockjs](https://github.com/emqtt/emq_sockjs)                     | Stomp over SockJS 协议支持 |
| [emq_recon](https://github.com/emqtt/emq_recon)                       | Recon 性能调试             |
| [emq_reloader](https://github.com/emqtt/emq_reloader)                 | Reloader 代码热加载插件    |
| [emq_plugin_template](https://github.com/emqtt/emq_plugin_template)   | 插件开发模版               |

## 1.1.3

*发布日期: 2016-08-19*

Support './bin/emqttd_ctl users list' CLI (#621)

Cannot publish payloads with a size of the order 64K using WebSockets (#643)

Optimize the procedures that retrieve the Broker version and Borker description in the tick timer (PR#627)

Fix SSL certfile, keyfile config (#651)

## 1.1.2

*发布日期: 2016-06-30*

Upgrade mysql-otp driver to 1.2.0 (#564, #523, #586, #596)

Fix WebSocket Client Leak (PR #612)

java.io.EOFException using paho java client (#551)

Send message from paho java client to javascript client (#552)

Compatible with the Qos0 PUBREL packet (#575)

Empty clientId with non-clean session accepted (#599)

Update docs to fix typos (#601, #607)

## 1.1.1

*发布日期: 2016-06-04*

Compatible with the Qos0 PUBREL packet (#575)

phpMqtt Client Compatibility (#572)

java.io.EOFException using paho java client (#551)

## 1.1

*发布日期: 2016-06-01*

1.1 版本升级 eSockd 库到 4.0，支持 IPv6 与监听特定 IP 地址。新增 MongoDB 认证插件、HTTP 认证插件与 Reloader 插件。升级 MySQL、PostgreSQL、Redis 认证插件，采用参数化查询避免 SQL 注入，并支持超级用户(superuser)认证。

### 问题与改进

Allow human-friendly IP addresses (PR#395)

File operation error: emfile (#445)

emqttd_plugin_mongo not found in emqttd (#489)

emqttd_plugin_mongo Error While Loading in emqttd (#505)

Feature request: HTTP Authentication (#541)

Compatible with the Qos0 PUBREL packet (#575)

Bugfix: function_clause exception occurs when registering a duplicated authentication module (#542)

Bugfix: ./emqttd_top msg_q result: {"init terminating in do_boot",{undef,[{etop,start,[],[]},{init,start_it,1,[]},{init,start_em,1,[]}]}} (#557)

### Dashboard 插件

WebSocket 连接页面支持 Clean Session, Qos, Retained 参数设置 (emqttd_dashboard#52)

升级 eSockd 库到 4.0 版本，Overview 页面显示 OTP 版本 (emqttd_dashboard#61)

Changing dashboard credentials for username authentication (emqttd_dashboard#56)

新增'./bin/emqttd_ctl admins'管理命令，支持通过命令行重新设置 admin 密码

### HTTP 认证插件

支持通过 HTTP API 认证/鉴权 MQTT 客户端: [https://github.com/emqtt/emqttd_auth_http](https://github.com/emqtt/emqttd_auth_http)

### MongoDB 认证插件

升级 Erlang Mongodb 驱动到 v1.0.0 (emqttd_plugin_mongo#1)

支持超级用户认证

支持基于 MongoDB 的 ACL (emqttd_plugin_mongo#3)

### MySQL 认证插件

支持超级用户认证

采用参数化查询避免 SQL 注入

### Postgre 认证插件

支持超级用户认证

采用参数化查询避免 SQL 注入

### Redis 认证插件

支持超级用户认证

支持 ClientId 认证/ACL (emqttd_plugin_redis#4)

### Reloader 插件

开发调试代码热升级插件: [https://github.com/emqtt/emqttd_reloader](https://github.com/emqtt/emqttd_reloader)

## 1.0.2

*发布日期: 2016-05-04*

Issue#534 - './bin/emqttd_ctl vm' - add 'port/count', 'port/limit' statistics

Issue#535 - emqttd_client should be terminated properly even if exception happened when sending data

PR#519 - The erlang '-name' requires the fully qualified host name

emqttd_reloader plugin - help reload modified modules during development.

## 1.0.1

*发布日期: 2016-04-16*

PR#515 - Fix '$queue' pubsub, add 'pubsub_queue' test and update docs

## 1.0 (七英里)

*发布日期: 2016-04-13*

*版本别名: 七英里(The Seven Mile Journey)*

经过两年开发，五十个版本迭代，我们正式发布 1.0(七英里)版本，和完整的中英文项目文档。

1.0 版本基本实现了设计目标: 稳定承载来自移动互联网或物联网终端的大量并发 MQTT 连接，并实现在大数量的终端间快速低延时的 MQTT 消息路由。

1. 完整支持 MQTT V3.1.1 协议，扩展支持 WebSocket、Stomp 或私有 TCP 等多协议。
2. 稳定承载大规模的并发 MQTT 客户端连接，单服务器节点支持 50 万到 100 万连接。
3. 分布式节点集群或桥接，快速低延时的消息路由，单集群支持 1000 万规模的路由。
4. 支持消息服务器内扩展，支持定制多种认证方式，插件方式存储消息到后端数据库。

### 问题与改进

1.0 版本主要发布完整项目文档，相比 0.17.1 版本很少代码变更:

Possible race condition using emqttd_cm (#486)

Improve the design of retained message expiration (#503)

Should not expire the retained messages from $SYS/# topics (#500)

### 项目文档

1.0 版本中文文档: [http://emqtt.com/docs/](http://emqtt.com/docs/) 或 [http://docs.emqtt.cn](http://docs.emqtt.cn)

1.0 版本英文文档: [https://developer.emqx.io/docs/emq/v1/en/index.html](https://developer.emqx.io/docs/emq/v1/en/index.html) 或 [http://docs.emqtt.com/](http://docs.emqtt.com/)

### 官方站点

中文站点: [http://emqtt.com](http://emqtt.com)

英文站点: [https://www.emqx.io/](https://www.emqx.io/)

### 致谢

爱立信与 Erlang/OTP 语言平台团队( [http://www.erlang.org/](http://www.erlang.org/) )!

贡献者(GitHub 帐户): @callbay @lsxredrain @hejin1026 @desoulter @turtleDeng @Hades32 @huangdan @phanimahesh @dvliman @Prots @joaohf

公司: 开源中国，鲁能电力，太极计算机，电信天翼云直播，研色科技，杭州华思

乐队: 七英里(The Seven Mile Journey)，腰乐队，万能青年旅店

## 0.2.0

*发布日期: 2014-12-07*

rewrite the project, integrate with esockd, mochiweb

support MQTT 3.1.1

support HTTP to publish message

## 0.1.5

*发布日期: 2013-01-05*

Bugfix: remove QOS_1 match when handle PUBREL request

Bugfix: reverse word in emqtt_topic:words/1 function

## 0.1.4

*发布日期: 2013-01-04*

Bugfix: fix "mosquitto_sub -q 2 ......" bug

Bugfix: fix keep alive bug

## 0.1.3

*发布日期: 2013-01-04*

Feature: support QOS2 PUBREC, PUBREL,PUBCOMP messages

Bugfix: fix emqtt_frame to encode/decoe PUBREC/PUBREL messages

## 0.1.2

*发布日期: 2012-12-27*

Feature: release support like riak

Bugfix: use ?INFO/?ERROR to print log in tcp_listener.erl

## 0.1.1

*发布日期: 2012-09-24*

Feature: use rebar to generate release

Feature: support retained messages

Bugfix: send will msg when network error

## 0.1.0

*发布日期: 2012-09-21*

The first public release.
