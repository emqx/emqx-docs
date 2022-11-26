# 版本发布

## e4.3.17

*发布日期: 2022-11-26*

### 增强

- 支持在规则引擎的 MongoDB 动作的 `Collection` 字段里使用 `${var}` 格式的占位符 [#1503](https://github.com/emqx/emqx-enterprise/pull/1503)。

- 添加针对规则引擎中的 InfluxDB 资源的 `host` 字段的格式检查；主机字段应该是不包括 scheme 和端口的 IP/域名 [#1426](https://github.com/emqx/emqx-enterprise/pull/1426)。
  相关的文档更新请查看 [emqx/emqx-docs#1368](https://github.com/emqx/emqx-docs/pull/1368)。

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

- 修正了在 Kafka Consumer 中选择 `reset_by_subscriber` 偏移重置策略的选项 [#1463](https://github.com/emqx/emqx-enterprise/pull/1463)。

- 修复了热配置中的 `tls_versions` 配置项遗漏了 `tlsv1.3` 选项的问题 [#1532](https://github.com/emqx/emqx-enterprise/pull/1532)。

- 修复了 SQL Server 资源中，无法在 `server` 字段里使用除 `1433` 之外的端口的问题 [#1464](https://github.com/emqx/emqx-enterprise/pull/1464)。

- Schema-Registry 的 API 在 HTTP 请求 URI 中支持百分号编码的 `name` [#1497](https://github.com/emqx/emqx-enterprise/issues/1497)。
  注意在创建 Schema 时，`POST /api/v4/schemas` 请求中的 `name` 字段不应使用百分号编码，因为这是一个 JSON 字段。

- 修复 JWT 认证插件的一个热升级问题 [#1554](https://github.com/emqx/emqx-enterprise/pull/1554)。
  当从 e4.3.9 或更早的热升级上来时，EMQX 内部一个 Key 管理进程会需要重启，在重启过程中的 JWT 认证请求可能会失败。

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

## e4.3.16

*发布日期: 2022-10-14*

### 增强

- 规则引擎增加了更多 Kafka 动作参数的检查。
  - TCP 发送缓存配置不再允许使用空值。
  - 转发策略是 'key_dispatch' 同时 "Key" 设置为 "none" 这个配置组合不再被允许。
    如果使用该组合，保存修改时会得到一个修改错误的提示。

- TLS 监听器内存使用量优化 [#9005](https://github.com/emqx/emqx/pull/9005)。
  新增了配置项 `listener.ssl.$NAME.hibernate_after` (默认不开启），该配置生效后，TLS 连接进程在空闲一段时间后会进入休眠。
  休眠可以较大程度减少内存占用，但是代价是 CPU 利用率会增加。
  测试中使用 '5s' （即 5 秒空闲后休眠）可以减少 50% 的内存使用量。

- 默认 TLS Socket 缓存大小设置为 4KB [#9007](https://github.com/emqx/emqx/pull/9007)。
  这样可以有效的避免某些环境中操作系统提供的默认缓存过大而导致 TLS 连接内存使用量大的问题。

- 关闭对 HTTP API `api/v4/emqx_prometheus` 的认证 [#8955](https://github.com/emqx/emqx/pull/8955)。
  Prometheus 对时序数据抓取不在需要配置认证。

- 更严格的 flapping 检测，认证失败等也会进行计数 [#9045](https://github.com/emqx/emqx/pull/9045)。

- 当共享订阅的会话终结时候，把缓存的 QoS1 和 QoS2 的消息向订阅组的其他成员进行转发[#9094](https://github.com/emqx/emqx/pull/9094) 。
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

## e4.3.15

*发布日期: 2022-09-17*

### 功能增强

- JWT 认证中的 `exp`、`nbf` 和 `iat` 声明支持非整数时间戳

### 错误修复

- 修复规则引擎的更新行为，此前会尝试初始化已禁用的规则
- 修复操作系统时间更改导致延迟发布不准确的问题
- 修复 Dashboard 监听器绑定 IP 地址不生效的问题
- 修复 `shared_dispatch_ack_enabled` 配置为 true 时共享订阅可能陷入死循环的问题
- 修复规则引擎 SQL 对空值进行主题匹配时崩溃的问题

## e4.3.14

*发布日期: 2022-08-29*

### 增强

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
- 修复 e4.3.7 版本后规则引擎 SQL 中任意变量与空值比较总是返回 false 的问题
- 修复 ExHook 的执行优先级高于规则引擎时，被 ExHook Message Hook 过滤的主题将无法触发规则引擎的问题
- 修复 TDEngine 的写入请求可能因为对端关闭网络连接而失败的问题
- 修复 MQTT-SN 模块除监听器以外的配置都不会生效的问题
- 修复 ExHook 管理进程因 supervisor 关闭超时而被强制杀死的问题
- 修复 ExProto `client.connect` 钩子中 Client ID 参数未定义的问题
- 修复客户端被踢除时 ExProto 不会触发断开连接事件的问题

## e4.3.13

*发布日期: 2022-08-11*

### 重要变更

- 升级了使用的 OTP 版本，以解决 OTP Bug 导致的低概率出现随机进程失去响应的问题，建议仍在使用 4.3 的用户升级到此版本
- 从下一版本起，我们将停止对 macOS 10 的支持，转为提供 macOS 11 的安装包

### 增强

- 允许配置连接进程在 TLS 握手完成后进行垃圾回收以减少内存占用，这可以使每个 SSL 连接减少大约 35% 的内存消耗，但相应地会增加 CPU 的消耗
- 允许配置 TLS 握手日志的日志等级以便查看详细的握手过程

### 修复

- 修复 ConfigMap 中取消挂载 `loaded_modules` 文件后，通过 Helm Chart 部署时 EMQX 无法启动的问题

## e4.3.12

*发布日期: 2022-07-29*

### 增强

- 规则引擎支持启用了 ACL 的 RocketMQ
- 支持对规则引擎中的规则进行搜索和分页
- 提供 CLI `./bin/emqx check_conf` 以主动检查配置是否正确
- 优化规则引擎中 TDEngine 的写入性能
- 优化共享订阅性能
- 规则引擎写入数据到 TDEngine 的动作中新增 `db_name` 字段以改善对超级表的支持

### 修复

- 修复规则引擎写入 TDEngine 时动作计数错误的问题
- 修复规则引擎写入 HStreamDB 时进程池大小设置不生效的问题
- 修复启用 GB/T 32960 插件后查询订阅列表报错的问题
- 修复启用热配置的情况下将 4.2 的备份恢复到 4.3 上时配置项不兼容的问题
- 修复热升级后一旦卸载了老版本 EMQX 将无法再次启动的问题
- 修复多语言协议扩展中对 UDP 客户端的保活检查错误导致客户端不会过期的问题
- 修复多语言协议扩展中客户端信息没有及时更新的问题
- 修复热升级到 e4.3.10 及更高版本以后运行时更新 License 不生效的问题
- 修复客户端指定 Clean Session 为 false 重连时，飞行窗口中的共享订阅消息会被尝试重新派发给旧会话进程的问题
- 修复新节点加入集群后没有使用集群 License 的问题
- 修复 `emqx_lua_hook` 插件无法取消消息发布的问题

## e4.3.11

*发布日期: 2022-06-30*

### 增强

- 规则引擎支持将数据持久化到 HStreamDB
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

## e4.3.10

*发布日期: 2022-06-01*

### 增强

- 为规则引擎 SQL 增加更多的时间转换函数
- 为规则引擎 SQL 增加 `float2str/2` 函数，支持指定浮点输出精度
- 规则引擎支持使用 Basic 和 JWT 认证连接 Pulsar
- 规则引擎 Oracle 资源新增 `service_name` 选项以支持 Oracle Database RAC
- 支持将 JWT 用于鉴权，现在 MQTT 客户端可以使用包含发布订阅白名单的特定声明进行授权
- 改进认证相关指标使更易理解，现在 `client.authenticate = client.auth.success + client.auth.failure`
- 支持 REST API 的监听器绑定到指定的网络接口上
- 上传 License 将自动同步至整个集群，无需每个节点单独上传，提供 HTTP API
- 支持对使用内置数据库作为数据源的认证鉴权中的用户数据进行多条件查询和模糊查询
- 支持将消息队列长度以及丢弃消息数量作为条件查询客户端
- 支持配置日志时间格式以兼容旧版本中的时间格式
- 当 `use_username_as_clientid` 配置为 `true` 且客户端连接时未指定 `username`，现在将拒绝连接并返回 `0x85` 原因码
- App secret 从部分随机改为完全随机
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
- 修复对订阅进行多条件查询时返回结果与查询条件不符的问题
- 修复规则引擎资源连接测试不工作的问题
- 修复多项 Dashboard 显示问题

## e4.3.9

*发布日期: 2022-04-18*

### 增强

- 编解码现已支持使用 gRPC 服务将任意二进制有效负载解码为 JSON 数据
- 支持使用 TLS 连接到 Pulsar
- 规则引擎 SQL 新增 `mongo_date` 函数，支持将时间戳保存为 MongoDB Date 对象
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

## e4.3.8

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

## e4.3.7

*发布日期: 2022-02-11*

### 重要变更

我们在 4.3.7 中修复了 License 总连接数计算的 Bug，License 将正确地检查集群的总连接数，而非错误地仅检查每个节点上的连接数。

请计划升级的用户注意此变化可能导致的客户端达到 License 限制而无法连接的可能性。

### 增强

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

## e4.3.6

*发布日期: 2021-12-17*

### 增强

- 规则引擎现已支持 Ali Lindorm 数据库
- 支持配置是否继续投递空的保留消息，以适应仍在使用 MQTT v3.1 协议的用户
- 优化内置访问控制文件模块的使用交互

### 修复

- 修复内存占用计算错误的问题
- 修复规则引擎 Webhook Action 的 Path 参数不支持使用 ${Variable} 的问题
- 修复 RocketMQ 异步写入时数据乱码的问题
- 修复 RocketMQ 统计指标不准的问题
- 修复某些情况下更新或删除 MQTT Bridge 和 MQTT Subscribe 资源，会持续打印连接失败日志的问题
- 修复规则引擎保存数据到 MySQL 时可能出现较高失败率的问题
- 修复规则引擎 Clickhouse 离线消息功能不可用的问题
- 修复规则引擎 MongoDB 离线消息功能中 Max Returned Count 选项无法使用的问题
- 修复部分热配置失效的问题

## e4.3.5

*发布日期: 2021-11-05*

### 增强

- 改进客户端踢除机制
- 为 LwM2M 网关添加新加密套件的支持
- 支持优先级队列的交错（以避免低优先级队列枯竭）
- 默认为 HTTP 认证插件关闭超级用户请求
- 优化 InfluxDB 写入性能
- InfluxDB 的 Tag Name 与 Field Name 支持使用占位符

### 修复

- 修复集群间调用可能导致客户端进程失去响应的问题
- 修复 WebHook TLS 不可用的问题
- 修复 MongoDB 资源不支持域名的问题
- 修复基于内置数据库的 ACL 的性能问题
- 修复基于内置数据库的认证错误转码 HTTP 请求参数的问题
- 修复 MySQL 认证模块可能启动失败的问题
- 修复 STOMP 网关若干问题
- 修复规则引擎 Kafka 和 MongoDB 资源状态不准确的问题
- 修复包含 “\” 字符的 Client ID 无法进行模糊搜索的问题
- 修复可变字节整数可能大于 4 字节的问题
- 修复可能添加重复模块的问题
- 修复无法在 Dashboard 上重启 Listener 的问题

## e4.3.4

*发布日期: 2021-09-18*

### 增强

- 规则引擎 数据桥接到pulsar 支持数据压缩
- 延长创建规则的超时间隔

### 修复

- 规则引擎 数据保存到InfluxDB 性能问题
- WebHook 无法配置 sni 导致某些情况下无法使用 HTTPS 的问题
- 规则引擎 关闭规则后资源无法释放的问题
- 规则引擎 离线消息 在某些情况下接收后无法删除的问题

## e4.3.3

*发布日期: 2021-08-16*

### 增强

- 离线消息保存到Redis 支持清除残留数据
- ExHook 增加自动重连机制，超时参数 和 备选动作

### 修复

- 规则引擎无法使用HTTPS 连接到 InfluxDB
- 规则引擎 数据保存到InfluxDB 动作无法使用占位符
- 规则引擎 数据桥接到WebServer 无法使用Path
- grpc-client 超时处理逻辑
- ExProto bug，增加重试逻辑，减少一些不必要的打印

## e4.3.2

*发布日期: 2021-07-17*

### 增强

- 增强 客户端上/下线消息增加更多的字段

### 修复

- LwM2M网关 管理页面无法打开的问题
- JT/T808网关 解析位置上报中自定义字段无法解析的问题
- acl.conf 文件格式错误导致ACL规则失效的问题
- 创建 auth_ldap 认证模块失败的问题
- 多语言协议解析 异常情况下无法停止的问题
- 规则引擎 无法创建Oracle 资源的问题
- 规则引擎 同步批量写入到SQL Server失败的问题

## e4.3.1

*发布日期: 2021-06-05*

### 增强

- 规则引擎 离线消息保存中 TimeRange 字段的说明

### 修复

- 规则引擎 数据保存到 OpenTSDB 异常情况下无法写入的问题
- 热升级后版本显示错误的问题
- MQTT-SN 协议 `cleansession=false` 的客户端在恢复 session 时 topicid 丢失的问题
- 预置模块在重启后修改配置丢失的问题
- Dashboard 规则引擎编辑显示错误问题
- Dashboard 导航面包屑显示问题

## e4.3.0

*发布日期: 2021-05-19*

### 增强

- 规则引擎消息桥接到 Kafka 分区支持动态扩容
- 规则引擎支持 ClickHouse 离线消息与代理订阅
- 规则引擎中支持批量操作动作默认启用批量异步模式
- 规则引擎数据保存到 InfluxDB 重构以增强性能
- 模块 消息下发 Kafka 消费组支持配置 Payload 格式

### 修复

- 规则引擎动作编辑数据不一致问题
- Dashboard 模块翻译问题
- 规则引擎 SQL语句支持 null 函数，undefined 转成 null
