# Broker 健康指标

本页是用于监控 EMQX broker 的最常用 Prometheus 指标的精选参考。请配合 [集成 Prometheus](./prometheus.md) 一起使用——后者介绍了如何暴露和抓取这些指标。

本页将健康指标分为四个领域：

1. **系统（System）** —— 操作系统和 Erlang 虚拟机资源。
2. **Broker** —— 连接与消息流量，以及 broker 状态。
3. **认证与授权（Authentication and authorization）** —— 连接时的身份校验和每条消息的 ACL 决策。
4. **数据集成（Data integration）** —— 规则、动作、连接器和桥接。

所有指标都通过 EMQX 的 Prometheus 端点（`/api/v5/prometheus/stats`、`/api/v5/prometheus/auth` 与 `/api/v5/prometheus/data_integration`）暴露。端点详情以及 `mode` 查询参数请参见 [集成 Prometheus](./prometheus.md#配置-pull-模式集成)。

::: tip 关于采集器默认值

带 `emqx_` 前缀的指标始终启用。带 `erlang_vm_` 前缀的、更丰富的 Erlang VM 指标来自上游 Prometheus Erlang exporter，在 EMQX 6.0 及更新版本中**默认关闭**。如果需要进程数、按分配器拆分的内存、GC / 调度器统计等指标，可将 `prometheus.collectors.vm_system_info`、`vm_memory`、`vm_statistics` 等设置为 `enabled` 启用对应采集器。

:::

## 系统

最贴近硬件层的信号。broker 出现异常时，通常这一类指标会最先发生变化。

### CPU

- `emqx_vm_cpu_use` —— CPU 使用率（百分比）。
- `emqx_vm_cpu_idle` —— CPU 空闲率（百分比）。

### 内存

- `emqx_vm_total_memory` —— 系统总内存（字节）。
- `emqx_vm_used_memory` —— 系统已用内存（字节）。
- `erlang_vm_memory_processes`、`erlang_vm_memory_atom`、`erlang_vm_memory_binary`、`erlang_vm_memory_ets`、`erlang_vm_memory_code`、`erlang_vm_memory_system` —— 按分配器拆分的内存（需启用 `vm_memory` 采集器）。

### 文件描述符

- `emqx_vm_max_fds` —— broker 进程的软 FD 上限（ulimit）。

### Erlang 进程与调度器负载

- `emqx_vm_run_queue` —— 当前调度器运行队列长度。长期不为零表示 CPU 饱和。
- `emqx_vm_process_messages_in_queues` —— 所有 Erlang 进程邮箱长度之和。数值持续偏大或增长，表明某个进程处理不过来。
- `erlang_vm_process_count` —— 当前 Erlang 进程数（需启用 `vm_system_info` 采集器）。
- `erlang_vm_process_limit` —— 配置的 Erlang 进程数上限。

### 内部邮箱看门狗

- `emqx_vm_mnesia_tm_mailbox_size` —— Mnesia 事务管理器邮箱深度；偏大表示事务竞争。
- `emqx_vm_broker_pool_max_mailbox_size` —— broker 派发进程池中最大的邮箱长度；偏大表示订阅侧出现背压。

### 运行时长

- `emqx_vm_uptime_ms` —— broker 已运行时间（毫秒）。突然降到很小值意味着节点重启。

### 集群复制健康（Mria）

- `emqx_mria_lag` —— 每个 replicant 节点的复制延迟。
- `emqx_mria_replicants` —— replicant 节点数量。
- `emqx_mria_bootstrap_time` —— 最近一次引导所用时间。
- `emqx_mria_message_queue_len` —— Mria 邮箱长度。

### 过载保护

- `emqx_overload_protection_new_conn` —— 因过载而被拒绝的连接数。
- `emqx_overload_protection_gc` —— 过载保护强制触发的 GC 次数。
- `emqx_overload_protection_hibernation` —— 触发的进程休眠次数。
- `emqx_overload_protection_delay_ok` —— 成功施加延迟的次数。
- `emqx_overload_protection_delay_timeout` —— 延迟尝试超时的次数。

## Broker

最关键的运行指标。重点关注消息相关计数器的速率，尤其是各种 `dropped` 序列。

### 集群拓扑

- `emqx_cluster_nodes_running` —— 集群中正在运行的节点数。
- `emqx_cluster_nodes_stopped` —— 集群中已停止的节点数。大于零时应报警。
- `emqx_conf_sync_txid` —— 最近一次应用的集群配置事务 ID。各节点之间不一致表示同步异常。

### License（企业版）

- `emqx_license_expiry_at` —— License 过期时间（UNIX epoch 秒）。
- `emqx_license_issued_at` —— License 签发时间。
- `emqx_license_max_sessions` —— License 允许的最大会话数。
- `emqx_cert_expiry_at` —— 监听器证书过期时间。

### 连接、会话与 Channel

- `emqx_connections_count` —— 当前连接数。
- `emqx_connections_max` —— 启动以来连接数峰值。
- `emqx_live_connections_count` —— 当前已建立（TCP 在线）的客户端数量。
- `emqx_live_connections_max` —— 在线连接峰值。
- `emqx_sessions_count` —— 活跃会话数（包含客户端当前已断开的持久会话）。
- `emqx_sessions_max` —— 会话峰值。
- `emqx_cluster_sessions_count` —— 集群范围内的会话数。
- `emqx_cluster_sessions_max` —— 集群范围内会话峰值。
- `emqx_channels_count` —— Channel 进程数（每个已连接客户端对应一个）。
- `emqx_channels_max` —— Channel 数量峰值。

### 订阅与主题

- `emqx_subscriptions_count` —— 订阅数。
- `emqx_subscriptions_max` —— 订阅数峰值。
- `emqx_subscriptions_shared_count` —— 共享订阅数。
- `emqx_subscriptions_shared_max` —— 共享订阅数峰值。
- `emqx_subscribers_count` —— 订阅者进程数。
- `emqx_topics_count` —— 不同主题数。
- `emqx_topics_max` —— 主题数峰值。
- `emqx_routes_count` —— 路由表大小。
- `emqx_routes_max` —— 路由表大小峰值。
- `emqx_durable_subscriptions_count` —— 持久会话订阅数。
- `emqx_durable_subscriptions_max` —— 持久会话订阅数峰值。

### 保留、延迟与禁用列表

- `emqx_retained_count` —— 保留消息数。
- `emqx_retained_max` —— 保留消息数峰值。
- `emqx_delayed_count` —— 延迟发布队列长度。
- `emqx_delayed_max` —— 延迟队列长度峰值。
- `emqx_banned_count` —— 被禁用的客户端 / 用户名 / IP 条目数。

### 消息

- `emqx_messages_received` —— 从客户端收到的应用层消息数。
- `emqx_messages_sent` —— 发送给客户端的应用层消息数。
- `emqx_messages_publish` —— 已分发的 PUBLISH 报文数。
- `emqx_messages_delivered` —— 投递给订阅者的次数（一条消息可能产生多次投递）。
- `emqx_messages_acked` —— 收到的订阅者确认数。
- `emqx_messages_forward` —— 跨节点转发的消息数。
- `emqx_messages_retained` —— 保留消息事件数。
- `emqx_messages_delayed` —— 延迟发布入队次数。

### 消息丢弃（最早出现的异常信号）

- `emqx_messages_dropped` —— 丢弃消息总数。
- `emqx_messages_dropped_expired` —— 因超过消息过期时间被丢弃。
- `emqx_messages_dropped_no_subscribers` —— 因没有匹配订阅者被丢弃。
- `emqx_messages_dropped_quota_exceeded` —— 因触发每客户端配额被丢弃。
- `emqx_messages_dropped_receive_maximum` —— 因订阅者 MQTT v5 的 receive-maximum 配额被丢弃。

### 单订阅者投递丢弃

- `emqx_delivery_dropped` —— 投递丢弃总数。
- `emqx_delivery_dropped_expired` —— 投递前消息已过期。
- `emqx_delivery_dropped_no_local` —— MQTT v5 No-Local 规则。
- `emqx_delivery_dropped_qos` —— 不支持的 QoS。
- `emqx_delivery_dropped_queue_full` —— 订阅者的消息队列已满。
- `emqx_delivery_dropped_too_large` —— 超出订阅者允许的最大报文大小。

### 字节

- `emqx_bytes_received` —— 累计接收字节数。
- `emqx_bytes_sent` —— 累计发送字节数。

### 报文层（适用于协议级排查仪表盘）

- `emqx_packets_received` / `emqx_packets_sent` —— 累计报文数。
- `emqx_packets_connect` —— 收到的 CONNECT 报文数。
- `emqx_packets_connack_sent` —— 发送的 CONNACK 报文数。
- `emqx_packets_connack_error` —— 带非零 Reason Code 的 CONNACK 数（大多数单客户端 AUTHN 失败会在此体现）。
- `emqx_packets_disconnect_received` / `emqx_packets_disconnect_sent` —— DISCONNECT 的收 / 发数。
- `emqx_packets_publish_received` / `emqx_packets_publish_sent` —— PUBLISH 的收 / 发数。
- `emqx_packets_publish_error` —— 未能被接收的 PUBLISH 数。
- `emqx_packets_publish_auth_error` —— 被授权拒绝的 PUBLISH 数。
- `emqx_packets_puback_*`、`emqx_packets_pubrec_*`、`emqx_packets_pubrel_*`、`emqx_packets_pubcomp_*` —— QoS 1 / 2 确认计数器。
- `emqx_packets_subscribe_received` / `emqx_packets_suback_sent` / `emqx_packets_subscribe_error` / `emqx_packets_subscribe_auth_error` —— SUBSCRIBE 接收 / 失败 / 被 AUTHZ 拒绝。
- `emqx_packets_unsubscribe_received` / `emqx_packets_unsuback_sent` / `emqx_packets_unsubscribe_error`。
- `emqx_packets_pingreq_received` / `emqx_packets_pingresp_sent` —— 保活活动。

### 客户端生命周期（钩子触发计数）

- `emqx_client_connect` —— 收到 CONNECT。
- `emqx_client_connack` —— 已发送 CONNACK。
- `emqx_client_connected` —— 触发了 `client.connected` 钩子。
- `emqx_client_disconnected` —— 触发了 `client.disconnected` 钩子。
- `emqx_client_disconnected_reason` —— 按原因聚合的断开次数。
- `emqx_client_subscribe` / `emqx_client_unsubscribe` —— 订阅 / 取消订阅钩子触发数。

### 会话生命周期

- `emqx_session_created` —— 创建的会话数。
- `emqx_session_resumed` —— 恢复的持久会话数。
- `emqx_session_takenover` —— 被新客户端接管的会话数。
- `emqx_session_discarded` —— 被丢弃的会话数（Clean Start 覆盖已有会话）。
- `emqx_session_terminated` —— 已终止的会话数。

## 认证与授权

当 HTTP、LDAP 或数据库后端处于认证链路上、需要判断到底是 broker 慢还是后端慢/失败时，这些指标特别有用。

### 连接时认证结果

- `emqx_authentication_success` —— 认证成功次数（不含匿名通过）。
- `emqx_authentication_success_anonymous` —— 匿名通过次数。
- `emqx_authentication_failure` —— 认证失败次数。

### 授权决策

- `emqx_authorization_allow` —— 决策为 allow 的次数。
- `emqx_authorization_deny` —— 决策为 deny 的次数。
- `emqx_authorization_nomatch` —— 没有匹配规则的次数（退回到 `no_match` 配置）。
- `emqx_authorization_matched_allow` —— 命中 allow 规则的次数。
- `emqx_authorization_matched_deny` —— 命中 deny 规则的次数。
- `emqx_authorization_cache_hit` —— 缓存命中次数。
- `emqx_authorization_cache_miss` —— 缓存未命中次数。
- `emqx_authorization_superuser` —— 超级用户旁路次数。

### 认证链状态

- `emqx_authn_total` —— 已配置的认证 provider 数量。
- `emqx_authn_enable` —— 每个 provider 的启用标志（0 / 1）。
- `emqx_authn_status` —— 每个 provider 的资源状态。
- `emqx_authn_users_count` —— 每个 provider 的用户记录数（适用于密码、mnesia 或基于数据库的 provider）。

### 认证 provider 运行时计数

- `emqx_authn_success` —— 每个 provider 命中成功的次数。
- `emqx_authn_failed` —— 每个 provider 失败的次数。
- `emqx_authn_nomatch` —— 每个 provider 选择忽略的次数（链路继续向下匹配）。
- `emqx_authn_latency` —— 每个 provider 的后端延迟。

### 授权数据源状态

- `emqx_authz_total` —— 已配置的授权数据源数量。
- `emqx_authz_enable` —— 每个数据源的启用标志（0 / 1）。
- `emqx_authz_status` —— 每个数据源的资源状态。
- `emqx_authz_rules_count` —— 每个数据源的规则记录数（适用于文件、mnesia 或基于数据库的数据源）。

### 授权数据源运行时计数

- `emqx_authz_allow` —— 每个数据源决策为 allow 的次数。
- `emqx_authz_deny` —— 每个数据源决策为 deny 的次数。
- `emqx_authz_nomatch` —— 每个数据源选择忽略的次数（链路继续向下匹配）。
- `emqx_authz_latency` —— 每个数据源的后端延迟。

### 内置数据库规模

- `emqx_authn_builtin_record_count` —— 内置认证数据库中的用户数。
- `emqx_authz_builtin_record_count` —— 内置授权数据库中的规则数。

## 数据集成

流水线视图：流量进入规则引擎，扇出到连接器与动作，最终落到外部系统。每一层都暴露各自的计数器；按顺序读取这些计数器，就能找出消息是在哪一层流失的。

### 资源清单

- `emqx_rules_count` —— 已配置的规则数。
- `emqx_actions_count` —— 已配置的动作数。
- `emqx_connectors_count` —— 已配置的连接器数。
- `emqx_schema_registrys_count` —— Schema Registry 条目数。

### 单资源状态

- `emqx_rule_enable` —— 规则启用标志（0 / 1）。
- `emqx_action_enable` —— 动作启用标志（0 / 1）。
- `emqx_action_status` —— 动作资源状态。
- `emqx_connector_enable` —— 连接器启用标志（0 / 1）。
- `emqx_connector_status` —— 连接器资源状态。

### 规则引擎 —— 单规则计数

- `emqx_rule_matched` —— 命中规则 WHERE 子句的消息数。
- `emqx_rule_passed` —— 通过规则的消息数。
- `emqx_rule_failed` —— 规则处理失败次数。
- `emqx_rule_failed_exception` —— 规则执行中抛出 Erlang 异常的次数。
- `emqx_rule_failed_no_result` —— SQL 没有产生结果的次数。

### 规则引擎 —— 动作子计数

- `emqx_rule_actions_total` —— 规则触发的动作调用总数。
- `emqx_rule_actions_success` —— 动作返回成功的次数。
- `emqx_rule_actions_failed` —— 动作失败次数。
- `emqx_rule_actions_failed_unknown` —— 未知原因失败次数。
- `emqx_rule_actions_failed_out_of_service` —— 下游资源不健康次数。
- `emqx_rule_actions_discarded` —— 动作被丢弃次数（例如被限流）。

### 动作吞吐

- `emqx_action_matched` —— 路由到该动作的消息数。
- `emqx_action_received` —— 进入动作队列的次数。
- `emqx_action_success` —— 动作调用成功次数。
- `emqx_action_failed` —— 动作调用失败次数。
- `emqx_action_late_reply` —— 响应在超时之后才到达的次数。
- `emqx_action_retried` —— 重试次数。
- `emqx_action_retried_success` —— 重试后成功的次数。
- `emqx_action_retried_failed` —— 重试用尽仍失败的次数。

### 动作队列与在飞请求

- `emqx_action_inflight` —— 正在处理的请求数。
- `emqx_action_queuing` —— 队列中（等待派发）的请求数。

### 动作丢弃

任一序列出现非零速率，都是下游系统不健康或动作配置不当的明显信号。

- `emqx_action_dropped` —— 动作层丢弃总数。
- `emqx_action_dropped_queue_full` —— 触发队列上限。
- `emqx_action_dropped_resource_stopped` —— 目标资源已停止。
- `emqx_action_dropped_resource_not_found` —— 目标资源缺失。
- `emqx_action_dropped_expired` —— 派发前消息已过期。
- `emqx_action_dropped_other` —— 其他原因。

## 最小化的"Broker 异常"面板

如果只能在一个页面里塞下少量曲线，下面这几条是负载承担最重的指标。大多数生产问题在事件发生后几秒内，都会让其中至少一条出现明显波动：

- `rate(emqx_messages_dropped[1m])` —— 不为零说明 broker 在拒绝或丢弃工作。
- `rate(emqx_action_dropped[1m])` —— 数据集成层在丢弃工作。
- `emqx_cluster_nodes_stopped` —— 大于零说明有节点掉线。
- `rate(emqx_overload_protection_new_conn[1m])` —— broker 正在主动拒绝新连接。
- `rate(emqx_authentication_failure[1m])` —— 认证失败速率突增通常意味着后端异常或受到攻击。
- `emqx_vm_run_queue` —— 持续大于零说明 CPU 饱和。
- `emqx_vm_process_messages_in_queues` —— 数值偏大说明进程邮箱出现积压。
- `emqx_mria_lag` —— 持续超过几秒说明复制开始落后。
- `emqx_license_expiry_at - time()`（企业版） —— License 到期倒计时。
