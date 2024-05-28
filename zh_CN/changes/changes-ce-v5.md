# v5 版本

## 5.7.0

*发布日期: 2024-05-27*

### 增强

#### MQTT

实现了会话持久化（Durable Sessions）。将 MQTT 持久会话（Persistent Session）及其消息存储到磁盘上，并在 EMQX 集群的多个节点之间持续复制会话元数据和 MQTT 消息，实现了有效的故障转移和恢复机制，确保服务的连续性和高可用性，从而提高系统的可靠性。

向 Prometheus 添加了与 EMQX 持久存储相关的指标：

- `emqx_ds_egress_batches`
- `emqx_ds_egress_batches_retry`
- `emqx_ds_egress_batches_failed`
- `emqx_ds_egress_messages`
- `emqx_ds_egress_bytes`
- `emqx_ds_egress_flush_time`
- `emqx_ds_store_batch_time`
- `emqx_ds_builtin_next_time`
- `emqx_ds_storage_bitfield_lts_counter_seek`
- `emqx_ds_storage_bitfield_lts_counter_next`
- `emqx_ds_storage_bitfield_lts_counter_collision`

注意：这些指标仅在启用会话持久化时可见。

Dashboard 上也增加了持久消息的数量。

更多关于会话持久化功能的详细信息，参见 [MQTT 会话持久化](../durability/durability_introduction.md)。

#### 安全

[#12947](https://github.com/emqx/emqx/pull/12947) 对于 JWT 认证，支持新的 `disconnect_after_expire` 选项。启用时，客户端将在 JWT token 过期后断开连接。


注意：这是一个不兼容变更。此选项默认启用，因此默认行为已更改。以前，带有实际 JWT 的客户端可以连接到服务器并在 JWT token 过期后保持连接。现在，客户端将在 JWT token 过期后断开连接。要保留以前的行为，请将 `disconnect_after_expire` 设置为 `false`。

#### 数据处理和集成

- [#12671](https://github.com/emqx/emqx/pull/12671) 在规则引擎 SQL 语言中添加了一个 `unescape` 函数，用于处理字符串中转义序列的展开。之所以添加这个功能，是因为 SQL 语言中的字符串字面量不支持任何转义码（例如 `\n` 和 `\t`）。这一增强功能使得在 SQL 表达式中对字符串进行更灵活的处理成为可能。


#### 扩展

- [#12872](https://github.com/emqx/emqx/pull/12872) 实现了客户端属性功能。允许使用键值对的方式为每个客户端设置额外的属性。属性值可以从 MQTT 客户端连接信息（如用户名、客户端 ID、TLS 证书）处理生成，也可以从认证成功返回的附带的数据中设置。属性可以用于 EMQX 的认证授权、数据集成和 MQTT 扩展功能等功能中。相较于直接使用客户端 ID 等静态属性，客户端属性能够更灵活的用在各类业务场景中，并简化开发流程，增强开发工作的适应性和效率。

  **初始化客户端属性**

  `client_attrs` 字段可以从以下 `clientinfo` 字段之一初始填充：

  - `cn`: TLS 客户端证书中的通用名称。
  - `dn`: TLS 客户端证书中的专有名称，即证书的 "Subject"。
  - `clientid`: 客户端提供的 MQTT 客户端 ID。
  - `username`: 客户端提供的用户名。
  - `user_property`: 从 MQTT CONNECT 数据包的 'User-Property' 中提取属性值。

  **通过认证响应扩展**

  可以从认证响应中合并额外的属性到 `client_attrs` 中。支持的认证后端包括：

  - **HTTP**：可以通过 `client_attrs` 字段将属性包含在 HTTP 响应体的 JSON 对象中。
  - **JWT**：可以通过 JWT 中的 `client_attrs` 声明包含属性。

  **在认证和授权中的使用**

  如果在认证之前初始化了 `client_attrs`，它可以在外部认证请求中使用。例如，`${client_attrs.property1}` 可以在请求模板中使用，用于指向 HTTP 服务器进行真实性验证。

  - `client_attrs` 可以在授权配置或请求模板中使用，增强灵活性和控制。例如：

    在 `acl.conf` 中，使用 `{allow, all, all, ["${client_attrs.namespace}/#"]}` 来基于 `namespace` 属性应用权限。

  - 在其他授权后端中，可以在请求模板中使用 `${client_attrs.namespace}` 动态包含客户端属性。

  更多关于客户端属性功能的详细信息，参见[客户端属性](../client-attributes/client-attributes.md)。

- [#12910](https://github.com/emqx/emqx/pull/12910) 添加了插件配置管理和 schema 验证功能。还可以使用元数据注释 schema，以便在 Dashboard 中进行 UI 渲染。更多详细信息请参见[插件模板](https://github.com/emqx/emqx-plugin-template/pull/126)和[插件文档](../extensions/plugins.md)。

#### 运维和管理

<!-- This is not ready to GA in 5.7

- [#12798](https://github.com/emqx/emqx/pull/12798) 新增 `GET /api/v5/clients_v2` API，该 API 使用游标代替页码进行分页。这样比旧的 API 端点更高效，因为旧的 API 会多次遍历表。

  -->


- [#12923](https://github.com/emqx/emqx/pull/12923) 在将错误格式导入内置认证数据库时提供了更具体的错误信息。

- [#12940](https://github.com/emqx/emqx/pull/12940) 向 `PUT /configs` API 添加了 `ignore_readonly` 参数。

  在此更改之前，如果原始配置包含只读根键（`cluster`、`rpc` 和 `node`），EMQX 将返回 400（BAD_REQUEST）。

  这一增强功能后，可以调用 `PUT /configs?ignore_readonly=true`，在这种情况下 EMQX 将忽略只读根配置键并应用其余配置。出于可观察性目的，如果丢弃了任何只读键，会记录一条信息级别的日志。

  还修复了配置中存在错误的 HOCON 语法时出现的异常（返回 500）。现在错误的语法将导致 API 返回 400（BAD_REQUEST）。

- [#12957](https://github.com/emqx/emqx/pull/12957) 开始为 macOS 14（Apple Silicon）和 Ubuntu 24.04 Noble Numbat（LTS）构建包。

### 修复

#### 安全

- [#12887](https://github.com/emqx/emqx/pull/12887) 修复了使用 SASL SCRAM 的 MQTT 增强型身份验证。
- [#12962](https://github.com/emqx/emqx/pull/12962) TLS 客户端现在可以使用通配符证书验证服务器主机名。例如，如果证书是为主机 `*.example.com` 颁发的，则 TLS 客户端可以验证类似 `srv1.example.com` 的服务器主机名。

#### MQTT

- [#12996](https://github.com/emqx/emqx/pull/12996) 修复了 `emqx_retainer` 应用程序中的进程泄漏问题。以前，当接收到保留消息时客户端断开连接，可能会导致进程泄漏。


#### 数据处理和集成

- [#12653](https://github.com/emqx/emqx/pull/12653) 规则引擎函数 `bin2hexstr` 现在支持位字符串输入的位数不是8的倍数。规则引擎函数 `subbits` 可能返回这样的位字符串。

- [#12657](https://github.com/emqx/emqx/pull/12657) 规则引擎基于 SQL 的语言以前不允许在数组文字中将任何表达式作为数组元素（只允许常量和变量引用）。现在已修复此问题，可以将任何表达式用作数组元素。

  例如，现在可以执行以下操作：

  ```bash
  select
  [21 + 21, abs(-abs(-2)), [1 + 1], 4] as my_array
  from "t/#"
  ```

<!-- This is a fix for not new feature in this release

- [#12707](https://github.com/emqx/emqx/pull/12707) 在数据库中保留持久客户端会话的 IP 地址和端口。

  -->

- [#12932](https://github.com/emqx/emqx/pull/12932) 以前，如果 HTTP 操作请求收到503（服务不可用）状态，它将被标记为失败，并且请求不会重试。现在已修复此问题，使得请求将重试配置的次数。

- [#12948](https://github.com/emqx/emqx/pull/12948) 修复了更新连接器后敏感的 HTTP 标头值（如 `Authorization`）被 `******` 替换的问题。

- [#13118](https://github.com/emqx/emqx/pull/13118) 修复了规则引擎模板渲染中的性能问题。

#### 可观测性

- [#12765](https://github.com/emqx/emqx/pull/12765) 确保统计信息 `subscribers.count` 和 `subscribers.max` 包含共享订阅者。先前它只包含非共享订阅者。


#### 运维和管理

- [#12812](https://github.com/emqx/emqx/pull/12812) 将资源健康检查操作改为非阻塞操作。这意味着更新或删除资源等操作不会被长时间运行的健康检查阻塞。
- [#12830](https://github.com/emqx/emqx/pull/12830) 将通道（操作/源）健康检查操作改为非阻塞操作。这意味着更新或删除一个动作/ source 数据集成等操作不会被长时间运行的健康检查阻塞。

<!-- This is a fix for not new feature in this release

- [#12874](https://github.com/emqx/emqx/pull/12874) 在会话重新连接前修改订阅时，确保持久消息重新发送的一致性：

  - 持久会话保存接收到的 QoS2 消息的飞行数据包 ID。

  - 确保持久会话与非持久会话在重叠订阅方面的行为一致。

  - 在 REST API 中列出持久订阅。

    -->

- [#12993](https://github.com/emqx/emqx/pull/12993) 修复了处理未知 zone 时的监听器配置更新 API。

  在此修复之前，当使用未知 zone 更新监听器配置时，例如 `{"zone": "unknown"}`，更改会被接受，导致所有客户端连接时崩溃。 在此修复后，使用未知 zone 名称更新监听器将获得 “Bad request” 响应。

- [#13012](https://github.com/emqx/emqx/pull/13012) MQTT 监听器配置选项 `access_rules` 已通过以下方式进行改进：

  - 如果配置了无效的访问规则，监听器不再以难以理解的错误消息崩溃。而是生成配置错误。
  - 现在可以通过逗号分隔在单个字符串中添加多个规则（例如，“allow 10.0.1.0/24，deny all”）。

- [#13041](https://github.com/emqx/emqx/pull/13041) 改进了 HTTP 认证错误日志消息。如果 POST 方法缺少 HTTP 内容类型标头，它现在会发出一个有意义的错误消息，而不是一个不太可读的带有堆栈跟踪的异常。

- [#13077](https://github.com/emqx/emqx/pull/13077) 此修复使 EMQX 只在连接器启动/重新启动时从全局配置中读取动作配置，并在连接器中存储动作的最新配置。之前更新到动作配置有时不会生效，需要禁用并启用动作才能生效。这意味着，即使动作配置看起来已成功更新，动作有时也可能使用旧的（先前的）配置运行。

- [#13090](https://github.com/emqx/emqx/pull/13090) 如果动作或 source 的连接器被停用，尝试启动它们时将不再尝试启动连接器本身。

#### 网关

- [#12909](https://github.com/emqx/emqx/pull/12909) 修复了 UDP 监听器进程在出现错误或关闭时的处理。修复确保 UDP 监听器在需要时能够干净地停止和重新启动。
- [#13001](https://github.com/emqx/emqx/pull/13001) 修复了 syskeeper 转发器在连接丢失时永远不会重新连接的问题。
- [#13010](https://github.com/emqx/emqx/pull/13010) 修复了 JT/T 808 网关在请求注册服务进行身份验证失败时无法正确回复 REGISTER_ACK 消息的问题。

## 5.6.1

*发布日期：2024-04-18*

### 修复

- [#12759](https://github.com/emqx/emqx/pull/12759) EMQX 现在会自动删除由于 shcema 验证错误而上传失败的无效备份文件。此修复确保只显示和存储有效的配置文件，提升系统可靠性。

- [#12766](https://github.com/emqx/emqx/pull/12766) 将 `message_queue_too_long` 错误原因重命名为 `mailbox_overflow`，与对应的配置参数 `force_shutdown.max_mailbox_size` 保持一致。

- [#12773](https://github.com/emqx/emqx/pull/12773) 升级了 HTTP 客户端库。

  HTTP 客户端库（`gun-1.3`）在标准端口（`http` 为 80 端口，`https` 为 443 端口）错误地在 `Host` 标头添加了 `:portnumber` 后缀。这可能导致与执行严格 `Host` 标头检查的服务器或网关（例如，AWS Lambda、阿里云 HTTP 网关）的兼容性问题，从而引发 `InvalidCustomDomain.NotFound` 或 "指定的 CustomDomain 不存在" 等错误。

- [#12802](https://github.com/emqx/emqx/pull/12802) 改进了 EMQX 通过 `emqx ctl cluster leave` 命令处理节点从集群中移除的方式。之前，如果配置的集群 `discovery_strategy` 不是 `manual`，节点可能会无意中重新加入同一个集群（除非它被停止）。最新的更新中，执行 `cluster leave` 命令现在会自动禁用节点的集群节点发现功能，防止它重新加入。要重新启用集群节点发现，请使用 `emqx ctl discovery enable` 命令或简单地重启节点。

- [#12814](https://github.com/emqx/emqx/pull/12814) 改进了 EMQX 中 `/clients/{clientid}/mqueue_messages` 和 `/clients/{clientid}/inflight_messages` API 的错误处理。这些更新包括：

  - **内部超时**：如果 EMQX 在默认的 5 秒超时内无法检索到 Inflight 或 Mqueue 消息列表（这通常在系统负载较重时发生），API 将返回 500 错误，响应为 `{"code":"INTERNAL_ERROR","message":"timeout"}`，并记录额外的信息以便排错。
  - **客户端关闭**：如果在 API 调用期间客户端连接被终止，API 现在将返回 404 错误，响应为 `{"code": "CLIENT_SHUTDOWN", "message": "Client connection has been shutdown"}`。这确保了在客户端连接中断时提供更清晰的反馈。

- [#12824](https://github.com/emqx/emqx/pull/12824) 更新了统计指标 `subscribers.count` 和 `subscribers.max`，以包括共享订阅者。之前，这些指标仅计算非共享订阅者。

- [#12826](https://github.com/emqx/emqx/pull/12826) 修复了 EMQX 中与数据集成 Source 和保留消息的导入功能相关的问题。在此更新之前：

  - 备份文件中指定的数据集成 Source 未被导入。这包括 `sources.mqtt` 类别下的特定连接器和参数，如 QoS 和主题。
  - 不支持导入用于保留消息的 `mnesia` 表。

- [#12843](https://github.com/emqx/emqx/pull/12843) 修复了在执行 `emqx ctl cluster leave` 命令后，在复制节点上的 `cluster_rpc_commit` 事务 ID 清理程序。以前，未能适当清除这些事务 ID 阻碍了核心节点上的配置更新。

- [#12885](https://github.com/emqx/emqx/pull/12885) 修复了 EMQX 中用户无法在 Dashboard 的 "监控" 菜单下查看 "保留消息" 的问题。

  "保留消息" 后端 API 使用 `qlc` 库。这个问题是由于权限问题引起的，`qlc` 库的 `file_sorter` 功能试图使用不可写的目录 `/opt/emqx` 存储临时文件，这是由于 Docker 部署中目录所有权权限的最近更改所致。

  此更新修改了 `/opt/emqx` 目录的所有权设置为 `emqx:emqx`，确保所有必要的操作，包括保留消息检索，可以在没有访问错误的情况下进行。

## 5.6.0

*发布日期: 2024-03-28*

### 增强

- [#12251](https://github.com/emqx/emqx/pull/12251) 优化了基于 RocksDB 的持久会话性能，减少了 RAM 使用和数据库请求频率。主要改进包括：

  - 引入了脏会话状态以避免频繁的 mria 事务。
  - 为持久消息引入了中间缓冲区。
  - 为 QoS1 和 QoS2 消息使用了不同的 PacketIds 轨迹。
  - 将每个流中连续未确认消息的数量限制为 1。

- [#12326](https://github.com/emqx/emqx/pull/12326) 通过使用注册历史增强了会话跟踪。EMQX 现在能够监控会话注册的历史，包括那些已过期的会话。通过配置 `broker.session_history_retain`，EMQX 保留了指定时间内过期会话的记录。

  - **会话计数 API**：使用 API `GET /api/v5/sessions_count?since=1705682238` 获取自给定 UNIX 纪元时间戳（精确到秒）以来保持活跃的集群内会话计数。这一增强有助于分析一段时间内的会话活动。

  - 使用集群会话指标进行扩展：添加了新的指标 `cluster_sessions`，以更好地跟踪集群内的会话数量。此指标也集成到 Prometheus 中以便于监控：

    ```
    # TYPE emqx_cluster_sessions_count gauge
    emqx_cluster_sessions_count 1234
    ```

    注意：请将此指标视为近似估计。由于数据收集和计算的异步性，精确度可能会有所不同。

- [#12338](https://github.com/emqx/emqx/pull/12338) 为基于 RocksDB 的持久会话后端引入了基于时间的垃圾收集机制。这一特性通过自动清理过时消息，确保了存储消息的更高效管理，优化了存储利用率和系统性能。

- [#12398](https://github.com/emqx/emqx/pull/12398) 在 Dashboard 配置中暴露了 `swagger_support` 选项，允许启用或禁用 Swagger API 文档。

- [#12467](https://github.com/emqx/emqx/pull/12467) 开始支持使用 AAAA DNS 记录类型进行集群发现。

- [#12483](https://github.com/emqx/emqx/pull/12483) 将 `emqx ctl conf cluster_sync tnxid ID` 重命名为 `emqx ctl conf cluster_sync inspect ID`。为了向后兼容，保留了 `tnxid`，但将在 5.7 版本中废弃。

- [#12499](https://github.com/emqx/emqx/pull/12499) 通过扩展规则增强了客户端封禁能力，包括：

  - 将 `clientid` 与指定的正则表达式进行匹配。
  - 将客户端的 `username` 与指定的正则表达式进行匹配。
  - 将客户端的对等地址与 CIDR 范围进行匹配。

  **重要提示**：添加大量广泛匹配规则（不特定于单个 clientid、username 或主机）可能会影响系统性能。建议谨慎使用这些扩展封禁规则以保持最佳系统效率。


- [#12509](https://github.com/emqx/emqx/pull/12509) 新增重新排序所有认证器/授权数据源 REST API 。


- [#12517](https://github.com/emqx/emqx/pull/12517) 升级了配置文件以适应多行字符串值，保留缩进以提高可读性和可维护性。这一改进使用 `"""~` 和 `~"""` 标记来引用缩进行，为定义复杂配置提供了一种结构化和清晰的方式。例如：

  ```
  rule_xlu4 {
    sql = """~
      SELECT
        *
      FROM
        "t/#"
    ~"""
  }
  ```

  有关详细信息，请参阅 [HOCON 0.42.0](https://github.com/emqx/hocon/releases/tag/0.42.0) 发行说明。

- [#12520](https://github.com/emqx/emqx/pull/12520) 实现了日志节流。该特性通过在配置的时间窗口内只保留事件第一次发生的记录，减少了可能泛滥系统的日志事件量。 对以下关键且容易重复的日志事件应用了日志节流：

  - `authentication_failure`
  - `authorization_permission_denied`
  - `cannot_publish_to_topic_due_to_not_authorized`
  - `cannot_publish_to_topic_due_to_quota_exceeded`
  - `connection_rejected_due_to_license_limit_reached`
  - `dropped_msg_due_to_mqueue_is_full`

- [#12561](https://github.com/emqx/emqx/pull/12561) 使用 HTTP API 获取客户端未确认消息和消息队列（mqueue）消息。这些 API 有助于深入了解和有效控制消息队列和未确认消息，确保消息处理和监控的效率。

  获取第一批数据：

  - `GET /clients/{clientid}/mqueue_messages?limit=100`
  - `GET /clients/{clientid}/inflight_messages?limit=100`

  或者，获取第一批数据而不指定起始位置：

  - `GET /clients/{clientid}/mqueue_messages?limit=100&position=none`
  - `GET /clients/{clientid}/inflight_messages?limit=100&position=none`

  获取下一批数据：

  - `GET /clients/{clientid}/mqueue_messages?limit=100&position={position}`
  - `GET /clients/{clientid}/inflight_messages?limit=100&position={position}`

  其中 `{position}` 是上一个响应中 `meta.position` 字段的值（不透明字符串令牌）。

  排序和优先级：

  - **Mqueue 消息**：这些消息基于它们在队列中的顺序（FIFO）进行排序和优先级划分，从高优先级到低优先级。默认情况下，mqueue 消息具有统一的优先级，为 0。
  - **未确认消息**：根据它们被插入未确认存储的时间戳进行排序，从最旧到最新。

- [#12590](https://github.com/emqx/emqx/pull/12590) 移除日志消息中的 `mfa` 元数据以提高清晰度。

- [#12641](https://github.com/emqx/emqx/pull/12641) 改进了文本日志格式化的字段顺序。新的字段顺序如下：

  `tag` > `clientid` > `msg` > `peername` > `username` > `topic` > [其他字段]

- [#12670](https://github.com/emqx/emqx/pull/12670) 在端点 `/monitor_current` 和 `/monitor_current/nodes/:node` 中添加了 `shared_subscriptions` 字段。

- [#12679](https://github.com/emqx/emqx/pull/12679) 将 Docker 镜像基础从 Debian 11 升级到 Debian 12。

- [#12700](https://github.com/emqx/emqx/pull/12700) 在 bytesize hocon 字段中开始支持 "b" 和 "B" 单位。

  例如，以下所有三个字段将具有 1024 字节的值：

  ```
  bytesize_field = "1024b"
  bytesize_field2 = "1024B"
  bytesize_field3 = 1024
  ```

- [#12719](https://github.com/emqx/emqx/pull/12719) `/clients` API 已升级，以同时容纳对多个 `clientid` 和 `username` 的查询，提供了一个更灵活、更强大的工具来监控客户端连接。此外，此更新引入了自定义 API 响应中包含哪些客户端信息字段的能力，为特定的监控需求进行了优化。

  多客户端/用户名查询示例：

  - 通过 ID 查询多个客户端：`/clients?clientid=client1&clientid=client2`
  - 查询多个用户：`/clients?username=user11&username=user2`
  - 在一个查询中组合多个客户端 ID 和用户名：`/clients?clientid=client1&clientid=client2&username=user1&username=user2`

  选择响应字段的示例：

  - 在响应中包含所有字段：`/clients?fields=all`（注意：省略 `fields` 参数默认返回所有字段。）
  - 只指定某些字段：`/clients?fields=clientid,username`

- [#12381](https://github.com/emqx/emqx/pull/12381) 新增 SQL 函数：`map_keys()`、`map_values()`、`map_to_entries()`、`join_to_string()`、`join_to_sql_values_string()`、`is_null_var()`、`is_not_null_var()`。

  有关函数及其使用的更多信息，请参阅文档：[内置 SQL 函数](../data-integration/rule-sql-builtin-functions)。

- [#12427](https://github.com/emqx/emqx/pull/12427) 实现了限制 Kafka 分区数量的功能以用于 Kafka 数据集成。

- [#12577](https://github.com/emqx/emqx/pull/12577) 更新了 GCP PubSub 生产者和消费者连接器的 `service_account_json` 字段，使其能够接受 JSON 编码的字符串。现在，可以将此字段设置为 JSON 编码字符串。仍然支持使用先前的格式（HOCON 映射），但不推荐。

- [#12581](https://github.com/emqx/emqx/pull/12581) 向模式注册中心添加了 JSON 架构。

  JSON Schema 支持 [Draft 03](http://tools.ietf.org/html/draft-zyp-json-schema-03)、[Draft 04](http://tools.ietf.org/html/draft-zyp-json-schema-04) 和 [Draft 06](https://datatracker.ietf.org/doc/html/draft-wright-json-schema-00)。

- [#12336](https://github.com/emqx/emqx/pull/12336) 性能提升。创建了一个专门的异步任务处理池来处理客户端会话清理任务。

- [#12725](https://github.com/emqx/emqx/pull/12725) 添加了用于列出可用的 source 类型的 REST API。

- [#12746](https://github.com/emqx/emqx/pull/12746) 添加了 `username` 日志字段。如果 MQTT 客户端以非空用户名连接，日志和追踪将包含 `username` 字段。

- [#12785](https://github.com/emqx/emqx/pull/12785) 日志处理进程增加了一个新的配置选项 `timestamp_format`，用于自定义日志时间戳格式。该选项支持以下设置：

  - `auto`: 根据所使用的日志格式类型自动确定时间戳格式。对于文本格式类型，使用 `rfc3339` 格式；对于 JSON 格式类型，则使用 `epoch`格式。

  - `epoch`: 时间戳以微秒精度的 Unix 纪元时间格式表示。
  - `rfc3339`: 时间戳使用符合 RFC3339 标准的日期时间字符串格式，格式示例为 `2024-03-26T11:52:19.777087+00:00`。


### 修复

- [#11868](https://github.com/emqx/emqx/pull/11868) 修复了会话接管后未发布遗嘱消息的问题。

- [#12347](https://github.com/emqx/emqx/pull/12347) 对 MQTT 出口数据桥接的规则 SQL 处理的消息进行了更新，确保即使在数据不完整或缺少某些在桥接配置中定义的占位符的情况下，消息也始终被视为有效。此调整防止了之前发生的消息被错误地视为无效并随后被 MQTT 出口数据桥接丢弃的情况。

  当 `payload` 和 `topic` 模板中的变量未定义时，现在它们被渲染为空字符串，而不是字面量 `undefined` 字符串。

- [#12472](https://github.com/emqx/emqx/pull/12472) 修复了在滚动升级过程中，某些读取操作在 `/api/v5/actions/` 和 `/api/v5/sources/` 端点可能导致返回 `500` 错误码的问题。

- [#12492](https://github.com/emqx/emqx/pull/12492) EMQX 现在在 MQTT v5 客户端的 `CONNACK` 消息中返回 `Receive-Maximum` 属性。此实现考虑了客户端的 `Receive-Maximum` 设置和服务器的 `max_inflight` 配置的最小值作为允许的未确认（unacknowledged）消息数量的限制。之前，确定的值未在 `CONNACK` 消息中发送回客户端。

- [#12500](https://github.com/emqx/emqx/pull/12500) 更新了 `GET /clients` 和 `GET /client/:clientid` HTTP API，以在其响应中包含断开的持久会话。

  注意：当前已知的问题是，由于包含了断开的会话，这些增强的 API 响应提供的总客户端计数可能超过实际的客户端数量。

- [#12505](https://github.com/emqx/emqx/pull/12505) 将 Kafka 生产者客户端 `wolff` 从版本 1.10.1 升级到 1.10.2。这个最新版本为每个连接器维持一个长期的元数据连接，通过减少为动作和连接器健康检查建立新连接的频率，优化了 EMQX 的性能。

- [#12513](https://github.com/emqx/emqx/pull/12513) 将几个可能导致日志泛滥的事件级别从 `warning` 改为 `info`。

- [#12530](https://github.com/emqx/emqx/pull/12530) 改进了 `frame_too_large` 事件和格式错误的 `CONNECT` 包解析失败的错误报告。这些更新现在提供了额外的信息，帮助故障排除。

- [#12541](https://github.com/emqx/emqx/pull/12541) 为基于 DNS 自动集群引入了新的配置验证步骤，以确保 `node.name` 和 `cluster.discover_strategy` 之间的兼容性。具体来说，当使用 `dns` 策略并带有 `a` 或 `aaaa` 记录类型时，所有节点必须使用（静态）IP 地址作为主机名。

- [#12562](https://github.com/emqx/emqx/pull/12562) 添加了一个新的配置根：`durable_storage`。此配置树包含与新的持久会话功能相关的设置。

- [#12566](https://github.com/emqx/emqx/pull/12566) 增强了 REST API 密钥的引导文件：

  - 文件中的空行现在将被跳过，消除了之前生成错误的行为。
  - 引导文件中指定的 API 密钥被赋予最高优先级。如果引导文件中的新密钥与现有密钥冲突，旧密钥将被自动删除，以确保引导密钥无问题地生效。

- [#12646](https://github.com/emqx/emqx/pull/12646) 修复了规则引擎日期时间字符串解析器的问题。之前，时区调整仅对以秒级精度指定的日期时间字符串有效。

- [#12652](https://github.com/emqx/emqx/pull/12652) 修复了文档中有描述但实际实现中缺失的带有 4 和 5 个参数的 subbits 函数的问题。这些函数现已被添加。

- [#12663](https://github.com/emqx/emqx/pull/12663) 确保通过 Prometheus 端点 `/prometheus/stats` 访问的 `emqx_vm_cpu_use` 和 `emqx_vm_cpu_idle` 指标准确反映当前的 CPU 使用和空闲情况，而不是自操作系统启动以来的平均 CPU 使用率，为监控目的提供更相关和及时的数据。

- [#12668](https://github.com/emqx/emqx/pull/12668) 使用 `calendar:datetime_to_gregorian_seconds/1` 重构了 SQL 函数 `date_to_unix_ts()`。此更改还为输入日期格式添加了验证。

- [#12672](https://github.com/emqx/emqx/pull/12672) 在生成节点启动配置的过程中加载 `{data_dir}/configs/cluster.hocon`。之前，通过Dashboard 进行的日志配置更改保存在 `{data_dir}/configs/cluster.hocon` 中，仅在使用 `etc/emqx.conf` 生成初始启动配置后应用，这可能导致在之后的重新配置中会丢失一些日志段文件。

  现在，创建启动配置时 `{data_dir}/configs/cluster.hocon` 和 `etc/emqx.conf` 同时加载，且 `emqx.conf` 中的设置优先。

- [#12696](https://github.com/emqx/emqx/pull/12696) 修复了尝试重新连接动作或 source 可能导致在 HTTP API 中返回错误的错误消息的问题。

- [#12714](https://github.com/emqx/emqx/pull/12714) 修复了 Prometheus API `/prometheus/stats` 端点报告的几个指标不准确的问题。更正适用于以下指标：

  - `emqx_cluster_sessions_count`
  - `emqx_cluster_sessions_max`
  - `emqx_cluster_nodes_running`
  - `emqx_cluster_nodes_stopped`
  - `emqx_subscriptions_shared_count`
  - `emqx_subscriptions_shared_max`

  此外，此修复纠正了 `/stats` 端点中 `subscriptions.shared.count` 和 `subscriptions.shared.max` 字段的问题。之前，这些值在客户端断开连接或取消订阅共享订阅后未能及时更新。

- [#12715](https://github.com/emqx/emqx/pull/12715) 解决了当入口数据集成 source 的连接器存在活动通道时，进行配置更新可能导致系统崩溃的问题。

- [#12740](https://github.com/emqx/emqx/pull/12740) 修复了无法踢出持久会话的问题。

- [#12768](https://github.com/emqx/emqx/pull/12768) 解决了 EMQX 5.4.0 及以后版本在启动时可能遇到的故障问题，特别是从 5.4.0 之前的版本进行滚动升级时。问题与当 v1 和 v2 路由表都为空时路由模式的初始化有关。

  现在，节点在启动时如果发现本地路由表为空，则会尝试从集群中检索正在使用的路由模式版本，而不是默认使用 v2 路由表。这种方法减少了潜在冲突的可能性，并降低了集群节点中路由存储模式分化的机会，尤其是在混合版本集群场景中。

  如果在运行中的集群检测到冲突，EMQX 将在日志中记录解决方法，作为级别为 `critical` 的错误消息的一部分。相同的错误消息和解决方法也会被记录在标准错误输出中，确保即使没有配置日志处理进程，消息也不会丢失。

- [#12786](https://github.com/emqx/emqx/pull/12786) 新增了一项严格的检查流程，防止复制节点连接到运行不同版本 EMQX 的核心节点。这项检查确保在滚动升级过程中，复制节点只有在至少有一个核心节点运行相同 EMQX 版本时才能工作。


## 5.5.1

*发布日期: 2024-03-06*

### 修复

- [#12471](https://github.com/emqx/emqx/pull/12471) 修复了在从 EMQX 版本 5.0.2 升级到新版本期间，数据集成配置未能正确加载的问题。

- [#12598](https://github.com/emqx/emqx/pull/12598) 修复了用户无法通过 HTTP API 订阅或取消订阅共享主题过滤器的问题。

  受影响的 API 包括：

  - `/clients/:clientid/subscribe`
  - `/clients/:clientid/subscribe/bulk`
  - `/clients/:clientid/unsubscribe`
  - `/clients/:clientid/unsubscribe/bulk`

- [#12601](https://github.com/emqx/emqx/pull/12601) 修复了 LDAP 驱动的日志没有被捕获的问题。现在，所有日志都以 `info` 级别被记录。

- [#12606](https://github.com/emqx/emqx/pull/12606) 修复了一个问题， 即当指定的 SSL 证书文件在给定路径中不存在时，Prometheus API 会崩溃。现在，如果缺少 SSL 证书文件，`emqx_cert_expiry_at` 指标会报告一个值为 0，表示证书不存在。

- [#12620](https://github.com/emqx/emqx/pull/12620) 在 HTTP 服务连接器中对授权头中的敏感信息进行了编辑，从调试级别日志中排除了认证和授权信息，以减少潜在的安全风险。

- [#12632](https://github.com/emqx/emqx/pull/12632) 修复了一个问题，即在闰年的3月1日起，规则引擎的 SQL 内置函数 `date_to_unix_ts` 会产生不正确的时间戳结果。

## 5.5.0

*发布日期: 2024-02-01*

### 增强

- [#12085](https://github.com/emqx/emqx/pull/12085) EMQX 已升级，以利用 OTP 版本 26.1.2-2 的功能。注意：Docker 镜像仍然使用 OTP 25.3.2 构建。

- [#12189](https://github.com/emqx/emqx/pull/12189) 增强了 EMQX JWT 认证中的[权限列表](../access-control/authn/jwt.md#权限列表)声明格式，使其具有更高的灵活性。更新后的格式现在支持数组结构，更加符合基于文件的 ACL 规则。

  例如：

  ```json
  [
  {
    "permission": "allow",
    "action": "pub",
    "topic": "${username}/#",
    "qos": [0, 1],
    "retain": true
  },
  {
    "permission": "allow",
    "action": "sub",
    "topic": "eq ${username}/#",
    "qos": [0, 1]
  },
  {
    "permission": "deny",
    "action": "all",
    "topics": ["#"]
  }
  ]
  ```

  在这种新格式中，找不到匹配的规则不会自动导致操作被拒绝。如果在 JWT 权限列表中找不到匹配项，授权链可以将请求交由下一授权检查器继续检查。如果在整个链中都找不到匹配项，最终检查结果将根据 `authorization.no_match` 中设置的默认权限决定。

- [#12267](https://github.com/emqx/emqx/pull/12267) 为 `cluster/:node/invite` 接口增加了一个新的 `timeout` 参数，解决了默认超时问题。之前设置的 5 秒默认超时，往往会导致 HTTP API 调用超时，因为 EMQX 加入集群通常需要更多时间。

  此外，EMQX 还添加了一个新的 API `/cluster/:node/invite_async`，支持以异步方式邀请节点加入集群，并通过新增的 `cluster/invitation` API 检查加入状态。

- [#12272](https://github.com/emqx/emqx/pull/12272) 对 EMQX 中的 `retain` API 进行了更新：

  - 增加了一个新的 API `DELETE /retainer/messages`，用于清除所有保留的消息。
  - 在 `GET /retainer/messages` API 的查询字符串中增加了一个可选的主题过滤器参数 `topic`。例如，使用 `topic=t/1` 可以过滤特定主题的保留消息，提高消息检索的效率。

- [#12277](https://github.com/emqx/emqx/pull/12277) 新增了 `mqtt/delayed/messages/:topic` API，用于按主题名称删除延迟消息。

- [#12278](https://github.com/emqx/emqx/pull/12278) 将 REST API 中支持分页的 API 的最大分页大小从 `3000` 调整到 `10000`。

- [#12289](https://github.com/emqx/emqx/pull/12289) 授权缓存支持排除特定的主题列表。对于指定的主题和主题过滤器列表，EMQX 将不会生成授权缓存。列表可以通过 `authorization.cache.excludes` 配置项或在 Dashboard 上设置。对于这些特定的主题权限检查将会始终实时进行，而不是依赖于之前的缓存结果，从而确保了授权结果的及时性。

- [#12329](https://github.com/emqx/emqx/pull/12329) 新增了 `broker.routing.batch_sync` 配置项。这个配置启用了一个专门的进程池，能够批量地将订阅信息与全局路由表同步，从而减少了可能由于网络延迟导致的跨节点通信的减慢。通过集中处理多个订阅更新，它不仅加速了集群中副本节点和核心节点之间的同步，而且还减轻了代理池的负载，从而最大限度地减少了过载的风险。

- [#12333](https://github.com/emqx/emqx/pull/12333) 为动作和连接器添加了一个 `tags` 字段。与 `description` 字段（即自由文本注释）类似，`tags` 可用于为动作和连接器添加注释，便于过滤和分组。

- [#12299](https://github.com/emqx/emqx/pull/12299) 公开了更多 EMQX 指标信息以提高可观测性：

  监控 API：

  - 在 `/api/v5/monitor_current` 中添加了 `retained_msg_count` 字段。
  - 在 `/api/v5/monitor_current` 中添加了 `license_quota` 字段。
  - 在 `/api/v5/monitor_current/nodes/{node}` 中添加了 `retained_msg_count` 和 `node_uptime` 字段。
  - 在 `/api/v5/monitor_current/nodes/{node}` 中添加了 `retained_msg_count`、`license_quota` 和 `node_uptime` 字段。

  Prometheus API：

  - 在 `/api/v5/prometheus/stats` 中添加了 `emqx_cert_expiry_at` 和 `emqx_license_expiry_at`，用于显示 TLS 监听器证书的过期时间和许可证的过期时间。
  - 添加了 `/api/v5/prometheus/auth` 端点，提供所有认证器和授权器的执行次数和运行状态等指标。
  - 添加了 `/api/v5/prometheus/data_integration` 端点，提供所有规则、动作和连接器的执行次数和状态等指标。

  限制： 

  Prometheus push gateway 仅支持 `/api/v5/prometheus/stats?mode=node` 中的内容。

  有关更多 API 详情和指标类型信息，请参阅 swagger api 文档。

- [#12196](https://github.com/emqx/emqx/pull/12196) 在路由清理过程中提高了网络效率。之前，当一个节点宕机时，所有其他存活节点之间必须交换针对该节点的每个路由的删除操作。在这次更改之后，所有存活节点之间只需交换一个 `match and delete`（匹配并删除）操作，这显著减少了所需的网络数据包数量并降低了集群间网络的负载。 这种优化对于地理分布式的 EMQX 部署尤为有用，在这些部署中网络延迟可能会非常高。

- [#12354](https://github.com/emqx/emqx/pull/12354) 支持并发创建和更新数据集成，大大提高了例如导入备份文件时的操作速度。

### 修复

- [#12232](https://github.com/emqx/emqx/pull/12232) 修复了节点被强制离开集群后集群提交日志表未被删除的问题。
- [#12243](https://github.com/emqx/emqx/pull/12243) 修复了一系列细微的竞争条件，这些条件可能导致全局路由状态不一致。
- [#12269](https://github.com/emqx/emqx/pull/12269) 改进了 `/clients` 接口的错误处理；现在在查询字符串验证失败时返回 400 状态和更详细的错误信息，而不是通用的 500。
- [#12285](https://github.com/emqx/emqx/pull/12285) 更新了 CoAP 网关，以支持短参数名，从而节省了数据报大小。例如，`clientid=bar` 可以写成 `c=bar`。
- [#12303](https://github.com/emqx/emqx/pull/12303) 修复了保留消息索引的问题。以前，具有通配符订阅的客户端可能会收到与其订阅主题不匹配的无关保留消息。
- [#12305](https://github.com/emqx/emqx/pull/12305) 修正了将不完整的客户端/连接信息传递到 `emqx_cm` 的问题，这可能导致内部不一致，并影响内存使用和节点疏散等操作。
- [#12306](https://github.com/emqx/emqx/pull/12306) 修复了通过 HTTP API 更新连接器密码参数后，连接器的连接测试无法正常工作的问题。
- [#12359](https://github.com/emqx/emqx/pull/12359) 修复了配置有某些类型数据桥接的节点重启时可能出现的错误消息问题。此外，这些桥接在节点重启时有进入失败状态的风险，需要手动重启以恢复功能。
- [#12404](https://github.com/emqx/emqx/pull/12404) 修复了一个问题，即在消息流量较大的情况下重启数据集成可能导致数据集成指标的收集停止。

## 5.4.1

*发布日期: 2024-01-09*


### 修复

- [#12234](https://github.com/emqx/emqx/pull/12234) 解决了 EMQX 5.4.0 之前版本在 `emqx.conf` 中定义的 Open Telemetry 配置的兼容性问题，确保最新 EMQX 发布版本能够平滑兼容旧版配置。
- [#12236](https://github.com/emqx/emqx/pull/12236) 修复了 MQTT 服务数据集成中客户端 ID 的生成方法，以符合 MQTT 3.1 规范的 23 字节限制。 客户端 ID 现在以用户分配的连接器名称为前缀，后跟节点名称的 SHA 哈希值和池成员 ID 的前 8 个字节。 如果生成的 ID 超过 23 字节，则会将 ID 重新使用 SHA 哈希，并取哈希的前 23 个字符以确保合规性。
- [#12238](https://github.com/emqx/emqx/pull/12238) 解决了EMQX 5.3.2 版本 HTTP Action 功能中引入的错误格式配置的兼容性问题。
- [#12246](https://github.com/emqx/emqx/pull/12246) 停止在 Docker 中默认暴露不再使用的 11883 端口，并从 Helm  Chart 中移除。
- [#12249](https://github.com/emqx/emqx/pull/12249) 修复了 `/configs` API 中尝试修改只读配置值导致响应消息乱码的问题。
- [#12264](https://github.com/emqx/emqx/pull/12264) 修复5.4副本节点在滚动升级过程中无法加入运行早于 5.4 版本的核心节点所在集群的问题。

## 5.4.0

*发布日期: 2023-12-23*

### 增强

- [#11884](https://github.com/emqx/emqx/pull/11884) 对 Prometheus API 及其配置进行了以下改进：

  - 重构了配置部分，将相关设置分组，提高了可读性和可维护性。
  - 引入了 `enable_basic_auth` 配置项，用于 scrape API 端点的基本认证，增强了安全性。
  - 在重构代码的同时保持了向后兼容性，避免了破坏性的变更。

- [#11896](https://github.com/emqx/emqx/pull/11896) 引入了在桥接配置中设置敏感认证字段（如密码、令牌和密钥）的增强功能。此改进允许使用以文件形式存储在文件系统中的秘密信息。这些秘密信息可以通过在配置文件中使用特殊的 `file://` 前缀安全地引用，从而增强了桥接配置中敏感数据处理的安全性。

- [#11921](https://github.com/emqx/emqx/pull/11921) 引入了 Open Telemetry 日志处理进程，该进程允许按照 Open Telemetry 日志数据模型格式化日志事件。此处理程序便于将格式化的日志事件导出到配置的 Open Telemetry 收集器或后端，从而增强了日志管理和集成能力。

- [#11935](https://github.com/emqx/emqx/pull/11935) 默认切换到新的`v2`路由存储模式。新模式提升了订阅和路由性能，尤其是在具有共同通配符前缀的主题过滤器的并发订阅场景中更为显著。但这也会带来轻微的内存使用增加。该模式还消除了对单独索引的需求，从而解决了在以往版本中偶尔遇到的路由状态不一致问题。

  如果集群是从旧版本进行滚动升级，那么集群将继续使用`v1`存储模式，直到发生全集群（非滚动）重启。

  用户仍可以通过将`broker.routing.storage_schema` 配置选项设置为`v1`来选择以前的模式。但是，这也需要完整的非滚动集群重启才能生效。

- [#11984](https://github.com/emqx/emqx/pull/11984)  实现了 Open Telemetry 分布式追踪特性。

- [#12017](https://github.com/emqx/emqx/pull/12017) 实现了一个专用的 REST API，用于配置和用户数据的导入和导出。

- [#12040](https://github.com/emqx/emqx/pull/12040) 升级了 QUIC 协议栈。

- [#12201](https://github.com/emqx/emqx/pull/12201) 添加了对 TCP/SSL/WS/WSS MQTT 监听器配置的热更新支持。这个功能允许您在无需重新启动监听器和断开客户端连接的情况下修改大多数配置参数。然而，目前有一些限制：

  - 对于 TCP/SSL 监听器，仍然需要重新启动监听器并重新连接客户端才能更改以下参数：
    - `bind`
    - `tcp_options.backlog`
  - 对于 WS/WSS（WebSocket）监听器，修改与传输相关的参数（如下所示）将导致监听套接字被重新打开，但已建立的连接将保持不间断。
    - `bind`
    - `tcp_options.*`
    - `ssl_options.*`

- [#11608](https://github.com/emqx/emqx/pull/11608) 客户端认证 LDAP 数据源支持通过绑定操作进行认证，提供了更多灵活性和安全性的用户认证方式。

- [#11766](https://github.com/emqx/emqx/pull/11766) 为 REST API 实现了初步的基于角色的访问控制。在这个版本中，有三个预定义的角色：

  - 管理员：此角色可以访问所有资源。
  - 查看者：此角色只能查看资源和数据，对应于 REST API 中的所有 GET 请求。
  - 发布者：专门为 MQTT 消息发布定制，此角色仅限于访问与消息发布相关的端点。

- [#11773](https://github.com/emqx/emqx/pull/11773) Dashboard 中添加了审计日志管理页面，用户可以使用该页面查看对 EMQX 设备和数据进行的所有更改操作，例如踢出设备、创建/删除规则等。

- [#11778](https://github.com/emqx/emqx/pull/11778) Dashboard 单点登录中的 SAML 协议支持与 Azure Entra ID 进行集成。


- [#11811](https://github.com/emqx/emqx/pull/11811) 优化了 REST API 密钥引导文件的格式，以支持使用角色初始化密钥。

  新的格式为：`api_key:api_secret:role`。

  其中 `role` 是可选的，默认值为 `administrator`。

- [#11852](https://github.com/emqx/emqx/pull/11852) 新增了 GB/T 32960 协议网关，使车辆能够通过 GB/T 32960 车联网协议与 EMQX 连接。

- [#11883](https://github.com/emqx/emqx/pull/11883) 新增了 JT/T808 协议网关，使车辆能够通过 JT/T 808 车联网协议与 EMQX 连接。

- [#11885](https://github.com/emqx/emqx/pull/11885) 新增了 OCPP 网关，使电动车（EV）充电站能够通过 OCPP (Open Charge Point Protocol) 协议访问 EMQX。

- [#11971](https://github.com/emqx/emqx/pull/11971) 将 `/api/v5/load_rebalance/availability_check` 接口设为公共接口，即不再需要进行身份验证。这一变更简化了负载均衡器的设置。

  此外，它还改善了等待健康检查阶段的负载均衡重平衡/疏散过程的流畅性。现在，在此阶段，不会禁止连接到被标记为要疏散的节点。这个调整是因为无法确定负载均衡器是否已将所有这些节点标记为不健康。禁止连接到它们可能会导致多次不成功的重新连接尝试。
  
- [#12013](https://github.com/emqx/emqx/pull/12013) 调整数据桥接设计，将其拆分为连接器与动作（Sink）。连接用于管理数据集成与外部系统的连接，可以在多个动作之间重复使用，动作仅用于配置数据操作方式。这个设计能够提供更大的灵活性和更好的可扩展性，实现更清晰的数据集成配置与管理。

  已调整的数据桥接有包括 PostgreSQL, Timescale 和 Matrix，现在拆分为连接器和动作 API，不过它们仍然与旧的数据桥接 API 兼容。

- [#12016](https://github.com/emqx/emqx/pull/12016) 增强了许可证密钥管理。

  EMQX 现在可以从指定文件加载许可证密钥。通过将 `license.key` 配置设置为文件路径，并使用 `"file://"` 作为前缀来启用此功能。 还添加了通过设置 `license.key = default` 来恢复到默认试用许可证的功能。此选项简化了在需要时返回试用许可证的过程。
  
- [#12129](https://github.com/emqx/emqx/pull/12129) 续期默认的 License，替换了 2023 年 1 月发布的旧 License。与此同时还将 License 规格从 100 并发连接调整为 25 个并发连接。

### 修复

- [#10976](https://github.com/emqx/emqx/pull/10976) 修复共享订阅中的主题过滤器重复处理问题。 在之前的实现中，订阅选项的存储方法没有充分适配共享订阅，这导致在特定的主题和流程下，”订阅-取消订阅” 期间消息路由失败并且节点之间的路由表出现泄漏问题。
- [#12048](https://github.com/emqx/emqx/pull/12048) 修复 COAP 网关忽略订阅选项的错误。
- [#12078](https://github.com/emqx/emqx/pull/12078) 升级了 grpc-erl 到版本 0.6.12。此更新解决了潜在的死锁问题，其中 grpc 客户端延迟启动了依赖的应用程序。
- [#12081](https://github.com/emqx/emqx/pull/12081) 更新了 `gen_rpc` 库到版本 3.3.1。这个新版本包括了一些性能改进：
  - 在某些情况下避免为数据包在发送到网络之前分配额外的内存。
  - 对于本地调用，绕过了网络层。
  - 避免敏感信息打印到日志中 [#12202](https://github.com/emqx/emqx/pull/12202)。
- [#12111](https://github.com/emqx/emqx/pull/12111) 修复了一个问题，该问题导致 API 令牌因为竞态条件在登录后立即不可用。
- [#12121](https://github.com/emqx/emqx/pull/12121) 修复了在不同节点同时更新配置时，集群中的节点偶尔会返回旧视图的问题。
- [#12158](https://github.com/emqx/emqx/pull/12158) 修复规则引擎无法连接到 [Upstash](https://upstash.com/) Redis 的问题。修复前，在与 Redis 服务建立 TCP 连接之后，EMQX 的 Redis 驱动程序使用 [inline commands](https://redis.io/docs/reference/protocol-spec/#inline-commands) 来发送 AUTH 和 SELECT 命令。但 Upstash Redis 服务不支持 inline commands，导致 EMQX 无法连接到 Upstash Redis 服务。 修复后，EMQX 的 Redis 驱动使用 RESP (Redis Serialization Protocol) 来发送 AUTH 和 SELECT 命令。
- [#12176](https://github.com/emqx/emqx/pull/12176) 无论之前是否成功建立连接，始终向 MQTT-SN 客户端发送 "DISCONNECT" 数据包的确认。
- [#12180](https://github.com/emqx/emqx/pull/12180) 修复了 MQTT-SN 网关因 DTLS 相关配置兼容性问题导致监听器无法启动的问题。
- [#12219](https://github.com/emqx/emqx/pull/12219) 修复了从 Dashboard 更新文件传输 S3 配置时密钥反混淆的问题。

## 5.3.2

*发布日期: 2023-12-01*

### 增强

- [#11752](https://github.com/emqx/emqx/pull/11752) 将 core-replica 数据库同步的默认 RPC 驱动从 `gen_rpc` 更改为 `rpc`。

  这提升了核心副本数据复制的速度。

- [#11785](https://github.com/emqx/emqx/pull/11785) 拥有“查看者”角色的用户具有更改自己密码的权限，但无权更改其他用户密码。

- [#11787](https://github.com/emqx/emqx/pull/11787) 提升了 `emqx` 命令的性能。

- [#11790](https://github.com/emqx/emqx/pull/11790) 为 Redis 授权数据源中的 Redis 命令添加了验证功能。此外，此次改进优化了认证和授权过程中 Redis 命令的解析，现在的解析符合 `redis-cli` 兼容性标准，并支持引号参数。

- [#11541](https://github.com/emqx/emqx/pull/11541) 文件传输能力得到了增强。现在，客户端可以使用异步方式，通过 `$file-async/...` 主题进行文件传输，并通过 `$file-response/{clientId}` 主题订阅命令执行结果。这一改进简化了文件传输功能的使用，尤其适用于 MQTT v3.1/v3.1.1 或使用了 MQTT 桥接的客户端。 更多详情请参阅 [EIP-0021](https://github.com/emqx/eip)。

### 修复

- [#11757](https://github.com/emqx/emqx/pull/11757) 修复了下载不存在的追踪文件时返回的错误响应码。现在，响应码会返回 `404` 而不是 `500`。

- [#11762](https://github.com/emqx/emqx/pull/11762) 修复了 EMQX 中 `built_in_database` 授权数据源的一个问题。通过这次修复，现在在删除数据源时，所有 ACL 记录都会被彻底移除。这解决了数据库残留的记录在重新创建授权数据源时仍然存在的问题。

- [#11771](https://github.com/emqx/emqx/pull/11771) 修复了通过 API/Dashboard 进行身份验证管理时 Bcrypt 盐轮次(salt rounds)的验证问题。

- [#11780](https://github.com/emqx/emqx/pull/11780) 修复了 `pbkdf2` 密码哈希算法中 `iterations` 字段的验证问题。现在，`iterations` 必须是严格正数。之前，`iterations` 可以被设置为 0，这会导致验证器无法正常工作。

- [#11791](https://github.com/emqx/emqx/pull/11791) 修复了 EMQX CoAP 网关中的一个问题，即心跳没有有效地维持连接的活跃状态。此修复确保心跳机制正确维持 CoAP 网关连接的活跃状态。

- [#11797](https://github.com/emqx/emqx/pull/11797) 修改了管理 `built_in_database` 授权数据源的 HTTP API 行为。如果未将 `built_in_database` 设置为授权数据源，这些 API 现在将返回 `404` 状态码，替换了以前的 `20X` 响应。

- [#11965](https://github.com/emqx/emqx/pull/11965) 优化了 EMQX 服务的终止过程，确保即使在存在不可用的 MongoDB 资源的情况下，也能够实现优雅停止。

- [#11975](https://github.com/emqx/emqx/pull/11975) 此修复解决了由于对端和服务器同时关闭套接字时发生竞争条件导致的冗余错误日志问题。以前，由操作系统和 EMQX 触发的并发套接字关闭事件会导致不必要的错误记录。通过改进事件处理，本次修复消除了不必要的错误信息。

- [#11987](https://github.com/emqx/emqx/pull/11987) 修复了在尝试设置 TCP/SSL 套接字的 `active_n` 选项时连接崩溃的问题。

  在此修复之前，如果在连接过程中尝试设置 `active_n` 选项时套接字已经关闭，会导致 `case_clause` 崩溃。

- [#11731](https://github.com/emqx/emqx/pull/11731) 为文件传输功能添加了热配置支持。

- [#11754](https://github.com/emqx/emqx/pull/11754) 改进了 Postgres 桥接的日志格式化功能，针对驱动程序返回的错误消息中的 Unicode 字符进行了处理。

## 5.3.1

*发布日期: 2023-11-14*

### 增强

- [#11637](https://github.com/emqx/emqx/pull/11637) 增加了额外的诊断检查，以帮助调试当 Mnesia 因等待表而停滞时出现的问题。更新依赖库：`ekka` 已升级至 0.15.15 版本，`mria` 已升级至 0.6.4 版本。
- [#11581](https://github.com/emqx/emqx/pull/11581) 功能预告：计划在 EMQX v5.4.0 版本中，在数据桥接的基础上新增*连接*与*动作*概念，并逐步迁移现有数据桥接到连接与动作。连接用于管理数据集成与外部系统的连接，动作仅用于配置数据操作方式，连接可以在多个动作之间重复使用，以提供更大的灵活性和更好的可扩展性。目前 Kafka 生产者与 Azure Event Hub 生产者已经完成迁移。
- Dashboard 为规则引擎消息重发布动作提供了 MQTT 5.0 发布属性设置，允许用户更灵活的发布消息。

### 修复

- [#11565](https://github.com/emqx/emqx/pull/11565) 将 jq 库从 v0.3.10 升级至 v0.3.11。在此版本中，jq_port 程序将按需启动，除非 EMQX 中使用 jq 功能，否则不会出现在用户的进程中。此外，空闲的 jq_port 程序将在设定的一段时间后自动终止。注意：大多数运行 NIF 模式下的 EMQX 用户不会受到此更新的影响。

- [#11676](https://github.com/emqx/emqx/pull/11676) 隐藏 DEBUG 级别的日志中的部分敏感信息。

- [#11697](https://github.com/emqx/emqx/pull/11697) 在 EMQX 后端网络 (`gen_rpc`) 中禁用了过时的 TLS 版本和密码套件。增加了对后端网络的 tlsv1.3 支持，并引入了新的配置参数：`EMQX_RPC__TLS_VERSIONS` 和 `EMQX_RPC__CIPHERS`。

  对应的 `gen_rpc` PR: https://github.com/emqx/gen_rpc/pull/36

- [#11734](https://github.com/emqx/emqx/pull/11734) 修复了 IPv6 网络中集群配置的问题。新增了新的配置项 `rpc.listen_address` 和 `rpc.ipv6_only`，以允许 EMQX 集群的 RPC 服务和客户端使用 IPv6。

- [#11747](https://github.com/emqx/emqx/pull/11747) 更新 QUIC 到 msquic 2.2.3 版本。

- [#11796](https://github.com/emqx/emqx/pull/11796) 修复了 RPC schema，以确保客户端和服务器使用相同的传输驱动程序。

- [#11798](https://github.com/emqx/emqx/pull/11798) 修复了在执行 `./bin/emqx data import [FILE]` 后节点无法启动的问题。

  同时增强了 `apikey_key` 和 `apikey_name` 之间的关联以提高一致性和唯一标识性：

  - `apikey_key`：通过 Dashboard 生成 API 密钥时，`apikey_key` 现在会根据提供的易读性较强的 `apikey_name` 创建一个唯一值。
  - `apikey_name`：相反，当使用引导文件生成 API 密钥时，`apikey_name` 将基于关联的 `apikey_key` 生成为唯一值。

- [#11813](https://github.com/emqx/emqx/pull/11813) 修复了 schema，确保 RPC 客户端 SSL 端口与配置的服务器端口一致。此修复还确保了RPC 端口在 Helm 图表中的被正确打开。

- [#11819](https://github.com/emqx/emqx/pull/11819) 升级了 OpenTelemetry 库至 v1.3.1-emqx。该版本修复了在导出的指标中指标时间戳无效的问题。

- [#11861](https://github.com/emqx/emqx/pull/11861) 修复了 remote shell 中打印过多警告信息的问题。

- [#11722](https://github.com/emqx/emqx/pull/11722) 修复了同步请求模式下的 Kafka 生产者桥接在`正在连接`状态下无法缓存消息的问题。

- [#11724](https://github.com/emqx/emqx/pull/11724) 修复了一个与统计指标相关的问题，即消息发送到 Kafka 时，由于内部缓存、即使后来被成功传输，仍然被计为发送失败。

- [#11728](https://github.com/emqx/emqx/pull/11728) 改进了 LDAP 过滤字符串解析器，具体改进如下：
  - 自动转义过滤字符串中的特殊字符。
  - 修复了先前阻止使用 `dn` 作为过滤值的错误。
  
- [#11733](https://github.com/emqx/emqx/pull/11733) 解决了一个不兼容性问题，该问题导致在会话接管或通道驱逐时，如果会话位于运行 EMQX v5.2.x 或更早版本的远程节点上，可能会导致崩溃。

- [#11750](https://github.com/emqx/emqx/pull/11750) 日志不再输出使用 HTTP 服务进行认证和 HTTP 服务数据桥接的请求 Body。

- [#11760](https://github.com/emqx/emqx/pull/11760) 简化了用于 Cassandra 数据桥接健康检查的 CQL 查询，之前该查询在 Cassandra 服务器日志中生成了警告。

- [#11886](https://github.com/emqx/emqx/pull/11886) 修复了一个向后的插件兼容性的问题。

  目前，EMQX 对钩子挂载点名称进行验证，无效的钩子挂载点不能用于注册钩子。但是旧版本的插件模板使用了一些拼写错误的钩子挂载点，实际使用中的插件也可能存在这种问题，为了兼容以前的插件，我们允许使用旧的钩子挂载点来注册钩子，但会发出已被弃用的警告，这些钩子与以前一样不会被调用。

- [#11897](https://github.com/emqx/emqx/pull/11897) 修复了当集群节点几乎在同一时间启动时，节点间配置同步的时候等待循环竞争条件的问题。


## 5.3.0

*发布日期: 2023-09-29*

### 增强

- [#11597](https://github.com/emqx/emqx/pull/11597)  将 Ekka 升级到 0.15.13，包括以下增强：

  - 升级 Mria 到 0.6.2。
  - 可以通过配置设置初始化阶段数据同步批量大小，[Mria PR](https://github.com/emqx/mria/pull/159)。
  - 提升了 mria_membership 进程的健壮性，[Mria PR](https://github.com/emqx/mria/pull/156)。
  - 修复日志消息格式错误。
  - EMQX 配置中添加了 `node.default_bootstrap_batch_size` 选项。 增加此选项的值可以极大地减少复制节点的启动时间，特别是当 EMQX 集群互连网络延迟较高且 EMQX 内置数据库包含大量数据时，例如订阅数较多的情况。

- [#11620](https://github.com/emqx/emqx/pull/11620)  添加一个新的规则引擎 SQL 函数 `bytesize` 以获取字节字符串的大小。例如：`SELECT * FROM "t/#" WHERE bytesize(payload) > 10`。

- [#11642](https://github.com/emqx/emqx/pull/11642) 将 quicer 升级到版本 0.0.200，为启用 OpenSSL3 对 QUIC 传输的支持做准备。

- [#11610](https://github.com/emqx/emqx/pull/11610) 在 Dashboard 中实施了初步基于角色的访问控制。

  在此版本中，有两个预定义的角色：

  - 管理员：此角色可以访问所有资源。
  - 查看者：此角色只能查看资源和数据，对应于 REST API 中的所有 GET 请求。

- [#11631](https://github.com/emqx/emqx/pull/11631) 添加了单点登录（SSO）功能并与 LDAP 集成。
- [#11656](https://github.com/emqx/emqx/pull/11656) 集成了 SAML 2.0 支持以实现单点登录（SSO）。
- [#11599](https://github.com/emqx/emqx/pull/11599) 支持审计日志，会将来自 CLI、REST API 和 Dashboard 的操作记录在独立的日志文件中。

### 修复

- [#11682](https://github.com/emqx/emqx/pull/11682) 修复了在文件日志处理程序上将“轮换大小”设置为`infinity`时日志记录停止的问题。

- [#11567](https://github.com/emqx/emqx/pull/11567) 改进了 EMQX 的优雅关闭（`emqx stop` 命令）：
  
  - 将超时时间从1分钟增加到2分钟。
  - 如果 EMQX 无法在配置的超时时间内优雅地停止，则打印错误消息。
  - 在 EMQX 关闭过程中定期打印状态消息。
  
- [#11584](https://github.com/emqx/emqx/pull/11584) 修复了在 Windows 上当 os_mon 模块不可用时的遥测报告错误。

- [#11605](https://github.com/emqx/emqx/pull/11605) 降低了 CMD_overridden 的日志严重程度，从警告（warning）降至信息（info）。

- [#11622](https://github.com/emqx/emqx/pull/11622) 升级了 RPC 库 `gen_rpc` 从版本 2.8.1 到 3.1.0。

- [#11623](https://github.com/emqx/emqx/pull/11623) 将 `esockd` 库从版本 5.9.6 升级到 5.9.7。此次升级包括以下内容：

  - 对代理协议错误和超时进行了增强。[esockd pr#178](https://github.com/emqx/esockd/pull/178)
  - 将 `ssl_error` 异常的日志级别降低为信息级别。[esockd pr#180](https://github.com/emqx/esockd/pull/180)
  - 将异常 MQTT 数据包解析的日志级别从 `error` 降低为 `info`。
  - 在 `emqx ctl listeners` 命令输出中，当 TLS 握手失败（`ssl_error`）或 MQTT 数据包格式错误（`frame_error`）发生时，会增加 `shutdown_count` 计数器。

- [#11661](https://github.com/emqx/emqx/pull/11661) 修复了文件日志格式类型配置 `log.HANDLER.formatter` 设置为 `json` 时的问题。

  该 bug 在 v5.0.4 中引入，导致日志行不再是有效的 JSON，而是以时间戳字符串和级别名称作为前缀。

- [#11627](https://github.com/emqx/emqx/pull/11627) 修复了 HStreamDB 桥接中的资源清理问题。在此修复之前，HStreamDB 桥接在桥接配置更新期间可能会报告错误，因为 hstreamdb 客户端/生产者没有被正确停止。

## 5.2.1

*发布日期: 2023-09-20*

### 增强

- [#11487](https://github.com/emqx/emqx/pull/11487) 将认证功能中，基于 bcrypt 算法的密码加密的计算强度因子 (work factor) 限制在5-10的范围内，因为较高的值会消耗太多 CPU 资源。Bcrypt 库已更新以允许并行哈希计算。
- [#11568](https://github.com/emqx/emqx/pull/11568) 在消息重发布规则动作中，支持设置 MQTT 5.0 发布属性与用户属性。目前配置接口暂未完全集成到 Dashboard，将在后续版本中提供支持。
- [#11612](https://github.com/emqx/emqx/pull/11612) 在节点疏散期间，疏散所有断开连接的会话，而不仅仅是那些以 `clean_start` 设置为 `false` 开始的会话。
- [#11532](https://github.com/emqx/emqx/pull/11532) 改进了解析无效数据包时的错误消息，以提供更清晰的错误提示。

### 修复

- [#11493](https://github.com/emqx/emqx/pull/11493) 修复了REST API 示例文档中关于 `/api/v5/publish` 错误请求响应的描述。之前的文档示例指出错误请求的响应可以在响应体中返回一个列表，但实际情况并非如此。
- [#11499](https://github.com/emqx/emqx/pull/11499) 升级 Erlang/OTP 至 25.3.2-2，此版本从 mnesia_hook 日志消息中排除了敏感数据。
- [#11506](https://github.com/emqx/emqx/pull/11506) 此前尝试下载不存在的跟踪日志文件时，会下载一个空的文件。在实施此修复后，尝试使用 GET 请求 `/api/v5/trace/clientempty/download` 下载不存在的跟踪日志文件时，服务器现在将返回 404 状态码以及以下 JSON 消息：`{"code":"NOT_FOUND","message":"Trace is empty"}`。
- [#11522](https://github.com/emqx/emqx/pull/11522) 在规则引擎的编解码功能中，改进了当 schema 名称超出允许的长度时出现的错误消息。
- [#11531](https://github.com/emqx/emqx/pull/11531) 修复了针对某个特定的客户端 ID，授权缓存清理 CLI 无法正常工作的问题。
- [#11564](https://github.com/emqx/emqx/pull/11564) 修复了集群分区自动恢复功能。实施了对分裂成多个分区的集群的自动恢复。
- [#11568](https://github.com/emqx/emqx/pull/11568) 修复了一个未明确定义的内置规则动作配置，以避免该配置被理解为自定义用户函数。
- [#11394](https://github.com/emqx/emqx/pull/11394) 将 Kafka 生产者客户端 `wolff` 从1.7.6版本升级到1.7.7版本。这个升级修复了一个潜在的竞态条件，可能会导致在有些 Kafka 生产者初始化失败时所有的 Kafka 生产者崩溃。
- [#11401](https://github.com/emqx/emqx/pull/11401) 修复了在 EMQX Dashboard 中对 SQL 语句进行测试时，规则 SQL 函数 `mongo_date` 的行为。规则 SQL 函数 `mongo_date` 现在在测试模式下返回具有格式 `ISODate(*)` 的字符串，其中 * 是 ISO 日期字符串。这个格式与 MongoDB 存储日期的方式保持一致。
- [#11547](https://github.com/emqx/emqx/pull/11547) 修复了几个 emqx_bridge 的问题：
  - 修复了 Cassandra 数据桥接在没有配置用户名/密码时出现连接错误的问题 （当配置为 `authenticator: AllowAllAuthenticator` 时，Cassandra 不需要用户凭据。）
  - 修复了因为空密码而导致 SQL Server 数据桥接连接错误的问题。
  - 将 Oracle 数据桥接中的 `username` 字段设置为必填项。
  - 修复了 IoTDB 数据桥接因未设置基础 URL 的模式（例如 `<host>:<port>`）而导致的错误。
- [#11630](https://github.com/emqx/emqx/pull/11630) 修复了核心节点可能会卡在 `mria_schema:bootstrap/0` 状态，导致新节点加入集群失败。

## 5.2.0

*发布日期: 2023-09-07*

### 增强

- [#10697](https://github.com/emqx/emqx/pull/10697) 此增强功能允许配置 StatefulSet 的 `minReadySeconds`，从而允许在升级或重新启动命令触发的每个 pod 重新启动之间引入时间间隔。

- [#11124](https://github.com/emqx/emqx/pull/11124) 发布了适用于 Amazon Linux 2023 的软件包。

- [#11289](https://github.com/emqx/emqx/pull/11289) 发布了适用于 Debian 12 的软件包。

- [#11290](https://github.com/emqx/emqx/pull/11290) 更新了 `jq` 依赖项至版本 0.3.10，其引用的 `oniguruma` 库更新至版本 6.9.8，修复了一些小的安全问题。

- [#11291](https://github.com/emqx/emqx/pull/11291) 通过 ekka 更新至版本 0.15.6，将 RocksDB 版本更新至 1.8.0-emqx-1。

- [#11390](https://github.com/emqx/emqx/pull/11390) 向 EMQX 配置添加了 `node.broker_pool_size`、`node.generic_pool_size` 和 `node.channel_cleanup_batch_size` 选项。如果集群互连网络延迟较高，调整这些选项可以显著提高性能。

- [#11429](https://github.com/emqx/emqx/pull/11429) 在 MondoDB 连接和桥接中添加了配置检测遗留协议的选项。

- [#11436](https://github.com/emqx/emqx/pull/11436) 添加了新的 REST API `DELETE /banned`，用于清除所有黑名单数据。

- [#11438](https://github.com/emqx/emqx/pull/11438) 将 `mqtt.max_packet_size` 的类型从字符串更改为 byteSize，以更好地表示有效的数字范围。仍然支持字符串以确保向后兼容性。

- [#11469](https://github.com/emqx/emqx/pull/11469) 支持在 Redis 认证中指定用户名。

- [#11496](https://github.com/emqx/emqx/pull/11496) 默认情况下禁用 Erlang VM Prometheus 导出器，以提高性能和安全性。

- [#11497](https://github.com/emqx/emqx/pull/11497) 通过添加新的消息、过载保护、授权、身份验证指标，改进 OpenTelemetry 的命名一致性，增强了指标可观测性。

- [#10647](https://github.com/emqx/emqx/pull/10647) 新增了 [GreptimeDB](https://github.com/GreptimeTeam/greptimedb) 数据集成。

- [#11261](https://github.com/emqx/emqx/pull/11261) 新增了 Amazon Kinesis Data Streams 生产者数据集成。

- [#11329](https://github.com/emqx/emqx/pull/11329) 新增了 Azure Event Hub 生产者数据集成。

- [#11363](https://github.com/emqx/emqx/pull/11363) 为 RabbitMQ 桥接添加了 TLS 连接支持。

- [#11367](https://github.com/emqx/emqx/pull/11367) 从 EMQX 4.4 迁移了 GCP IoT Hub 认证支持。

- [#11386](https://github.com/emqx/emqx/pull/11386) 认证器新增了 LDAP 数据源。

- [#11392](https://github.com/emqx/emqx/pull/11392) 授权管理器新增了 LDAP 数据源。

- [#11402](https://github.com/emqx/emqx/pull/11402)  Kafka 消费者桥接支持使用占位符动态设置 MQTT 主题。

- [#11403](https://github.com/emqx/emqx/pull/11403) 添加了支持定义 GCP PubSub 生产者桥接的消息属性和排序键模板。还更新了我们的 HOCON 库，以修复一个问题，即数组中的对象即使位于不同的行上也会被串联在一起。

- [#11459](https://github.com/emqx/emqx/pull/11459) 添加了配置 Kafka 桥接的健康检查间隔的选项。

- [#11478](https://github.com/emqx/emqx/pull/11478) 添加了对 HStreamDB 桥接的支持（允许 TCP 和 TLS 连接），并适配了 HStreamDB `v0.16.1`。

  在 [PR#11530](https://github.com/emqx/emqx/pull/11530) 中更新了驱动程序至 `0.4.5+v0.16.1`。

- [#11389](https://github.com/emqx/emqx/pull/11389) 通过利用 Mria 0.6.0 中引入的新 API 将多个索引更新操作合并为单个 Mnesia 事务来提高保留消息发布的速度。

- [#11396](https://github.com/emqx/emqx/pull/11396) 为规则引擎运行时引入了主题索引，提高了消息主题与规则 SQL 中的主题过滤器匹配的速度，避免了对规则集的全面扫描，大幅提升了 EMQX 在处理大量规则时的性能。

- [#11399](https://github.com/emqx/emqx/pull/11399) 改进了规则引擎中的占位符语法。发布操作支持使用占位符语法动态填充 payload 变量中的内容。 占位符语法的格式为 `\${key}`。 在此改进之前，`\${key}` 中只能包含字母、数字和下划线。现在，`\${key}` 支持任何 UTF8 字符。

- [#11405](https://github.com/emqx/emqx/pull/11405) 改进了 `date_to_unix_ts` 的错误原因以便理解。

- [#11490](https://github.com/emqx/emqx/pull/11490) 为各种认证后端添加了未定义密码的快速错误处理。这提高了认证过程的一致性和用户友好性。

### 修复

- [#11065](https://github.com/emqx/emqx/pull/11065) 修复了在 EMQX 关闭过程中防止日志记录无关错误消息的问题。
- [#11279](https://github.com/emqx/emqx/pull/11279) 修复了当在 EMQX 启用了 debug/trace 日志记录时客户端无法发送包含大型 payload 消息的问题。
- [#11296](https://github.com/emqx/emqx/pull/11296) 添加了从 EMQX 备份文件中使用 `emqx ctl import` 命令导入附加配置的支持：
  - rule_engine（以前由于错误而未导入）
  - topic_metrics（以前未实现）
  - slow_subs（以前未实现）
- [#11327](https://github.com/emqx/emqx/pull/11327) 更新了 ekka 到版本 0.15.8，mria 到版本 0.15.8，以及 optvar 到 1.0.5。 这修复了偶发的断言失败问题。
- [#11346](https://github.com/emqx/emqx/pull/11346) 更新了 ekka 到版本 0.15.9。 这修复了在获取锁定超时时出现的悬挂的 etcd 锁定问题。
- [#11347](https://github.com/emqx/emqx/pull/11347) 确保 OCSP 请求路径正确进行了 URL 编码。
- [#11352](https://github.com/emqx/emqx/pull/11352) 修复了在 Windows 或其他不支持 RocksDB 的平台上启动时出现的崩溃问题。
- [#11388](https://github.com/emqx/emqx/pull/11388) 增加了 `emqx_router_sup` 重启强度，以提高对在正常情况下发生偶发崩溃的容忍度，而无需关闭整个 EMQX 应用程序。 例如， 如果核心节点正在停止、重新启动或处于不可用状态，从复制节点委派给 `emqx_router_helper` 的 mria 写入/删除调用可能会失败。修改后的重启强度确保系统保持稳定运行。
- [#11424](https://github.com/emqx/emqx/pull/11424) 添加了对 API 中时间戳的最大值的检查，以确保它是有效的 Unix 时间戳。
- [#11445](https://github.com/emqx/emqx/pull/11445) 删除了 Windows 平台上的 os_mon 应用程序监控支持，以防止虚拟机崩溃。 该功能仍然适用于非 Windows 平台。
- [#11454](https://github.com/emqx/emqx/pull/11454) 修复了在调试/跟踪大型 payload 时出现的崩溃问题（在 [#11279](https://github.com/emqx/emqx/pull/11279) 中引入）。
- [#11456](https://github.com/emqx/emqx/pull/11456) 移除了对 CA 证书文件强制要求非空 PEM 的验证，允许 CA 证书文件 PEM 为空。
- [#11466](https://github.com/emqx/emqx/pull/11466) 修复了将 `ssl_options.ciphers` 配置选项设置为空字符串（""）时出现崩溃的问题。
- [#11480](https://github.com/emqx/emqx/pull/11480) 改进了规则引擎中当规则函数接收到错误参数时的错误处理和 SQL 函数测试。
- [#11520](https://github.com/emqx/emqx/pull/11520) 修复了在发送带有非零 `ack_flag` 的 CONNACK 数据包时未增加 `packets_connack_sent` 指标的问题。
- [#11523](https://github.com/emqx/emqx/pull/11523) 更正了在为 `/configs` API 指定无效证书/密钥时出现的令人误解的提示。
- [#11534](https://github.com/emqx/emqx/pull/11534) 修复了当桥接状态不健康时数据桥接统计数据的增量。现在，发送到不健康桥接的消息将被计算为丢弃的消息。
- [#11540](https://github.com/emqx/emqx/pull/11540) 在尝试创建具有无效名称的桥接时，改进了 HTTP 响应。
- [#11548](https://github.com/emqx/emqx/pull/11548) 修复了在整个集群中更新插件顺序的问题。
- [#11366](https://github.com/emqx/emqx/pull/11366) 修复了在使用 EMQX Operator 在 `bootstrapConfig` 中指定一些桥接配置时可能会阻止 pod 启动的问题。
- [#11453](https://github.com/emqx/emqx/pull/11453) 修复了测试 InfluxDB 桥接的连接时可能产生虚假负面结果的问题。
- [#11461](https://github.com/emqx/emqx/pull/11461) 将测试桥接连接的超时更加紧密地与配置的健康检查超时保持一致。
- [#11492](https://github.com/emqx/emqx/pull/11492) 修复了测试 GreptimeDB 桥接的连接时可能产生虚假负面结果的问题。
- [#11508](https://github.com/emqx/emqx/pull/11508) 修复了 Kafka 桥接中将 header 翻译为无效值时的错误处理。
- [#11513](https://github.com/emqx/emqx/pull/11513) 修复了一个错误，该错误导致 Kafka 生产者桥接无法使用正确的模板的来处理 `timestamp` 字段。
- [#11527](https://github.com/emqx/emqx/pull/11527) 修复了与 Kafka header 模板处理相关的问题。该问题发生在占位符解析为键值对数组时（例如：`[{"key": "foo", "value": "bar"}]`）。

## 5.1.1

*发布日期: 2023-07-27*

### 增强

- [#10667](https://github.com/emqx/emqx/pull/10667) 将 MongoDB 连接器和桥接重构为单独的应用程序，以改进代码结构。

- [#11115](https://github.com/emqx/emqx/pull/11115) 添加了信息日志，标示由于存活时间（TTL）过期而丢弃的缓冲消息。

- [#11133](https://github.com/emqx/emqx/pull/11133) 在 `retainer` 的配置中将 `deliver_rate` 重命名为 `delivery_rate`，兼容之前的 `deliver_rate`。

- [#11137](https://github.com/emqx/emqx/pull/11137) 重构了 Dashboard 监听器配置，使用嵌套的 `ssl_options` 字段来进行 SSL 设置。

- [#11138](https://github.com/emqx/emqx/pull/11138)  将k8s `api_server `的默认值从 `http://127.0.0.1:9091 ` 更改为 `https://kubernetes.default.svc:443`。

  - 当 `discovery_strategy=static` 时，`emqx_ctl conf show cluster` 不再显示不相关的配置项。 与`etcd/k8s/dns` 相关的配置信息将不再显示。
  - 从 `emqx_ctl conf show_keys` 中删除了 `zones`（已弃用的配置键）。

- [#11165](https://github.com/emqx/emqx/pull/11165) 从 `swagger.json` 中删除了 `/configs/limiter` API 文档，API 仍然保留。

- [#11166](https://github.com/emqx/emqx/pull/11166) 在规则引擎 SQL 中添加了 3 个随机函数：

  - `random()`: 生成 0 到 1 之间的随机数（0.0 =< X < 1.0）。
  - `uuid_v4()`: 生成随机 UUID（版本4）字符串。
  - `uuid_v4_no_hyphen()`: 生成无连字符的随机 UUID（版本4）字符串。

- [#11180](https://github.com/emqx/emqx/pull/11180) 添加了一个新的配置 API `/configs`（GET/PUT），支持重新加载 HOCON 格式的配置文件。

- [#11226](https://github.com/emqx/emqx/pull/11226) 统一监听器开关配置项为 `enable`，同时兼容之前的`enabled`。

- [#11249](https://github.com/emqx/emqx/pull/11249) 添加了支持通过 REST API 设置 License 连接使用配额的告警阈值。

- [#11251](https://github.com/emqx/emqx/pull/11251) 添加了 `GET /cluster/topology` REST API，用来返回集群拓扑，显示 RLOG 核心节点和复制节点之间的连接。

- [#11253](https://github.com/emqx/emqx/pull/11253) 将 Webhook/HTTP 桥接重构为单独的 Erlang 应用，为将来的使用提供了灵活性，并允许将桥接作为独立应用程序运行。

- [#11079](https://github.com/emqx/emqx/pull/11079) Kafka 桥接，生产者模式下为消息添加了自定义 headers 支持。

- [#11132](https://github.com/emqx/emqx/pull/11132) MQTT 授权检查添加了 QoS 级别和保留消息条件，现在 EMQX 可以验证客户端是否有权限使用指定的 QoS 级别进行发布/订阅，以及有权限发布保留消息。

- [#11207](https://github.com/emqx/emqx/pull/11207) 更新了多个数据桥接的驱动版本，以增强安全性并确保敏感数据不会泄露。包括：

  - TDengine
  - MongoDB
  - MySQL
  - Clickhouse

- [#11241](https://github.com/emqx/emqx/pull/11241) 将 Schema Registry 重构为单独的 Erlang 应用以提供灵活性。

- [#11020](https://github.com/emqx/emqx/pull/11020) 升级了 emqtt 依赖项，以防止调试日志中的敏感数据泄漏。

- [#11135](https://github.com/emqx/emqx/pull/11135) 改进了规则引擎中的时间偏移解析器，并返回统一的错误代码。

- [#11236](https://github.com/emqx/emqx/pull/11236) 改进了默认参数下 `GET /clients` REST API 客户端查询的速度。

### 修复

- [#11004](https://github.com/emqx/emqx/pull/11004) 主题重写不再允许在目标主题中使用通配符。

- [#11026](https://github.com/emqx/emqx/pull/11026) 解决了规则引擎中 `div` 和 `mod` 操作使用的一致性问题。之前，`div` 操作只能作为中缀操作使用，`mod ` 只能通过函数调用使用用。现在，`div ` 和 `mod` 都可以通过函数调用语法和中缀语法使用。

- [#11037](https://github.com/emqx/emqx/pull/11037) 启动 HTTP 连接器时，如果系统无法连接到远程目标系统，EMQX 现在会返回一个描述性错误。

- [#11039](https://github.com/emqx/emqx/pull/11039) 修复了 Redis 连接器的数据库验证问题。之前，负数被接受为有效的数据库。

- [#11074](https://github.com/emqx/emqx/pull/11074) 修复了有关 MQTT-5.0 [MQTT-3.8.3-4] 协议内容的问题。

- [#11077](https://github.com/emqx/emqx/pull/11077) 修复了更新监听器时使用非整数的端口可能发生崩溃的问题。

- [#11094](https://github.com/emqx/emqx/pull/11094) 修复了 Kafka 桥接生产者模式在重新连接时无法报告连接错误的问题。

- [#11103](https://github.com/emqx/emqx/pull/11103) 更新了 `erlcloud` 依赖项。

- [#11106](https://github.com/emqx/emqx/pull/11106) 添加了对桥接资源 `worker_pool_size` 的最大数量的验证。现在最大数量为 1024，以避免因不合理数量的工作进程导致大内存消耗。

- [#11118](https://github.com/emqx/emqx/pull/11118) 确保 REST API 响应中的验证错误信息更加明确。现在，如果有超出范围的错误，将呈现为`{"value": 42, "reason": {"expected": "1..10"}, ...}`，替换了先前使用的 `expected_type`，改为使用 `expected`。

- [#11126](https://github.com/emqx/emqx/pull/11126) 修复了规则下有异步模式的桥接时，规则的失败指标计数问题。

- [#11134](https://github.com/emqx/emqx/pull/11134) 修复了日志中敏感字段大写 `authorization` header 的值不被混淆的问题。

- [#11139](https://github.com/emqx/emqx/pull/11139) 将 Redis 桥接器重构为单独的 Erlang 应用，以改进代码结构。

- [#11145](https://github.com/emqx/emqx/pull/11145) 在 Ekka 和 Mria 中进行了几处修复和改进。

  Ekka:

  - 改进了集群发现日志消息，以一致地描述实际事件。 [Ekka PR](https://github.com/emqx/ekka/pull/204)
  - 删除了弃用的集群自动清理配置参数（已移动到 Mria）。[Ekka PR](https://github.com/emqx/ekka/pull/203)

  Mria:

  - 现在 Ping 只在复制节点上运行。之前，`mria_lb `尝试同时 ping 停止和运行中的复制节点，可能导致超时错误。 [Mria PR](https://github.com/emqx/mria/pull/146)
  - 在复制 `$mria_rlog_sync` 表时使用 `null_copies` 存储。 此修复对 EMQX 目前没有影响，因为 `$mria_rlog_sync`仅在 `mria:sync_transaction/2,3,4` 中使用，而这在 EMQX 中未被使用。 [Mria PR](https://github.com/emqx/mria/pull/144)

- [#11148](https://github.com/emqx/emqx/pull/11148) 修复当一个节点离开集群时，其他节点仍然尝试将配置更新操作同步到其中的问题。

- [#11150](https://github.com/emqx/emqx/pull/11150) 在启动 `emqx_psk` 应用程序时等待 Mria 表，确保即使没有初始化 PSK 文件，PSK 数据也能同步到 replicant 节点。

- [#11151](https://github.com/emqx/emqx/pull/11151) 将 MySQL 桥接重构为单独的 Erlang 应用，以改进代码结构。

- [#11158](https://github.com/emqx/emqx/pull/11158) 在 Mnesia 后端的 retainer 启动时等待 Mria  表，避免加入集群时出现错误。

- [#11162](https://github.com/emqx/emqx/pull/11162) 修复 Webhook 桥接异步模式下，4XX 和 5XX HTTP 状态码被统计为成功指标的问题。

- [#11164](https://github.com/emqx/emqx/pull/11164) 重新引入对嵌套占位符（例如：`${payload.a.b.c}`）的支持，对于 JSON 格式的 Payload，从规则动作与数据桥接中提取数据时无需先调用 `json_decode(payload)`。

- [#11172](https://github.com/emqx/emqx/pull/11172) 修复了特定情况下 `payload` 重复的问题：

  - 使用不带 `as` 子表达式的 `foreach` 语句，并选择所有字段，例如：

    ```
    FOREACH payload.sensors FROM "t/#"
    ```
  
  - 在选择 `payload` 字段和所有字段的情况下，例如：

    ```
    SELECT payload.sensors, * FROM "t/#"
    ```

- [#11174](https://github.com/emqx/emqx/pull/11174) 修复了 MQTT 桥接 Ingress 的 `server` 字段的编码问题。在修复之前，它被编码为 ASCII 字符对应的整数列表。

- [#11184](https://github.com/emqx/emqx/pull/11184) 配置项 `mqtt.max_packet_size` 最大值设置为协议定义 256MB。

- [#11192](https://github.com/emqx/emqx/pull/11192) 修复了在使用 atom 类型生成有效 HOCON 文件时，从 HOCON 文件中删除不必要的 `"`。

- [#11195](https://github.com/emqx/emqx/pull/11195) 修复了 REST API 可以为 Stomp 网关指定客户端创建重复订阅的问题。

- [#11206](https://github.com/emqx/emqx/pull/11206) 连接到 CoAP 网关时，不再要求客户端的 `username` 和 `password` 参数必填。

- [#11208](https://github.com/emqx/emqx/pull/11208) 修复了 LwM2M 客户端异常数据统计的问题。

- [#11211](https://github.com/emqx/emqx/pull/11211) `DELETE` 操作操作不存在的资源时，一致性地返回 `404` 状态码。

- [#11214](https://github.com/emqx/emqx/pull/11214) 修复了节点配置可能在加入集群时无法正确同步的问题。

- [#11229](https://github.com/emqx/emqx/pull/11229) 修复了在通过 `emqx ctl conf load` 更改配置后，插件无法启动/停止的问题。

- [#11237](https://github.com/emqx/emqx/pull/11237)  `/prometheus` API中 `headers` 的默认值应为对象而不是列表。

- [#11250](https://github.com/emqx/emqx/pull/11250) 修复了 WebSocket 数据包中包含多个 MQTT 数据包时其顺序被颠倒的问题。


- [#11271](https://github.com/emqx/emqx/pull/11271) 确保 REST API 与配置中百分比类型的范围为 0% 到 100%。比如 `sysom.os.sysmem_high_watermark=101%` 现在是无效的设置。

- [#11272](https://github.com/emqx/emqx/pull/11272) 修复了日志中的拼写错误，错误地将异常 `PUBREL` 数据包称为 `pubrec`。

- [#11281](https://github.com/emqx/emqx/pull/11281) 恢复对特殊的共享订阅主题前缀 `$queue/` 的支持。

- [#11294](https://github.com/emqx/emqx/pull/11294) 修复了`emqx_ctl cluster join`，`leave `和 `status `命令。

- [#11306](https://github.com/emqx/emqx/pull/11306) 修复了规则动作指标不一致的问题，未计算丢弃的请求。

- [#11309](https://github.com/emqx/emqx/pull/11309) 改进了 EMQX 应用程序的启动顺序。简化了构建脚本，并改进了代码重用。

- [#11322](https://github.com/emqx/emqx/pull/11322) 支持从 EMQX 备份文件（`emqx ctl import `命令）中导入了其他配置：

  - rule_engine（以前由于错误未导入）
  - topic_metrics（以前未实现）
  - slow_subs（以前未实现）。

- [#10645](https://github.com/emqx/emqx/pull/10645) 更改了对 Oracle、PostgreSQL、MySQL 和 Kafka 生产者数据桥接的健康检查，以确保目标表/主题存在。

- [#11107](https://github.com/emqx/emqx/pull/11107) 现在在测试 MongoDB 桥接时返回健康检查失败原因。

- [#11139](https://github.com/emqx/emqx/pull/11139) 将 Redis 桥接重构为单独的 Erlang 应用，以改进代码结构和易维护性。

- [#11151](https://github.com/emqx/emqx/pull/11151) 将 MySQL 桥接重构为单独的 Erlang 应用，以改进代码结构和易维护性。

- [#11163](https://github.com/emqx/emqx/pull/11163) 隐藏 MondoDB 桥接中的 `topology.pool_size`，并将其固定为 1 以避免混淆。

- [#11175](https://github.com/emqx/emqx/pull/11175) 现在，REST API 创建 MySQL 连接时如果使用不存在的主机名，将返回 400 错误而不是 503 错误。

- [#11198](https://github.com/emqx/emqx/pull/11198) 修复了复制节点上全局再平衡状态评估策略。之前，`/api/v5/load_rebalance/global_status` API method 可能在由复制节点处理时返回不完整的结果。

- [#11223](https://github.com/emqx/emqx/pull/11223) InfluxDB 桥接写入配置中，某个字段混用小数和整数时可能会导致 Influx Line Protocol 序列化失败，并且无法写入 InfluxDB 桥接（当小数位为 0 时小数点会被忽略，InfluxDB 误以为其是整数）。

  另请参阅：[InfluxDB v2.7 Line-Protocol](https://docs.influxdata.com/influxdb/v2.7/reference/syntax/line-protocol/#float)。

- [#11225](https://github.com/emqx/emqx/pull/11225) 修复了PostgreSQL/Timescale/MatrixDB 桥接没有验证 `username` 可能为空的问题。

- [#11242](https://github.com/emqx/emqx/pull/11242) 当节点加入集群时重新启动 emqx_ee_schema_registry。因为 emqx_ee_schema_registry 使用 Mria 表，所以节点加入集群需要重新启动此应用程序，以启动相关的 Mria 分片进程，确保在核心/复制节点模式下正确工作。

- [#11266](https://github.com/emqx/emqx/pull/11266) 修复和改进对 TDengine `insert `语法的支持：

  1. 支持在模板中插入多表。

     例如：

     `insert into table_1 values (${ts}, ${val}) into table_2 values (${ts}, ${val})`

  2. 支持在模版中混合前缀/后缀和占位符。

     例如：

     `insert into table_${topic} values (${ts}, '${id}', '${topic}')`

     注意：这是一个破坏性变更。此前，字符类型的占位符会被自动转义加上单引号，而现在需要手动加上单引号。

     例如：

     `insert into table values (${ts}, '${a_string}')`

- [#11307](https://github.com/emqx/emqx/pull/11307) 在 Oracle 桥接中检查表是否存在时返回更友好的错误信息。

- [#11316](https://github.com/emqx/emqx/pull/11316) 修复了在 Oracle 桥接中未考虑 Pool Size 值的问题。

- [#11326](https://github.com/emqx/emqx/pull/11326) 修复了在 Oracle 桥接中返回错误检查的问题。

### [已知问题](https://github.com/emqx/emqx-docs/blob/release-5.1/en_US/changes/known-issues-5.1.1.md)

## 5.1.0

*发布日期: 2023-06-21*

### 增强

-   [#11035](https://github.com/emqx/emqx/pull/11035) 升级 Cassandra 驱动以避免用户名密码在数据桥接日志中泄漏。
-   [#10584](https://github.com/emqx/emqx/pull/10584) 为 SSL 通信增加日志等级配置。
-   [#10678](https://github.com/emqx/emqx/pull/10678) 优化计数器递增调用以避免在递增为0的情况下计数。
-   [#10690](https://github.com/emqx/emqx/pull/10690) 为 Webhook 桥接添加了重试机制，旨在尝试提高吞吐量。
    这个优化让客户端可以在请求失败时进行重试，而不会阻塞缓冲层，从而在高消息传输率的情况下提高吞吐量。
-   [#10702](https://github.com/emqx/emqx/pull/10702) 引入了一个更直观的配置选项 `keepalive_multiplier`，并废弃了旧的 `keepalive_backoff` 配置。在改进之后，EMQX 通过将"客户端请求的 Keepalive 间隔"与 `keepalive_multiplier` 相乘来周期性地检查客户端的 Keepalive 超时状态。
-   [#10698](https://github.com/emqx/emqx/pull/10698) 优化了在运行时访问配置的内存使用。
-   [#10778](https://github.com/emqx/emqx/pull/10778) 重构 Pulsar 生产者桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10813](https://github.com/emqx/emqx/pull/10813) 重构了Kafka 生产者和消费者桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10858](https://github.com/emqx/emqx/pull/10858) 规则引擎 SQL 语言新增了一个实用函数 timezone_to_offset_seconds/1。该函数将时区字符串（例如"+02:00"、"Z"和"local"）转换为相应的偏移秒数。
-   [#10841](https://github.com/emqx/emqx/pull/10841) 为 Kafka 和 Pulsar 生产者桥接添加了参数校验，以确保在选择了 `key_dispatch` 策略时消息键参数不为空。
-   [#10754](https://github.com/emqx/emqx/pull/10754) 对 MQTT 桥接进行了增强，利用连接池和可用的并行性，大大提高了吞吐量。因此，单个 MQTT 桥接现在使用一组 `clientid` 连接到远程代理。
-   [#10782](https://github.com/emqx/emqx/pull/10782) 在保留器（retainer）配置中添加了一个新的 `deliver_rate` 选项，它可以限制保留器中每个会话的最大传递速率。
-   [#10877](https://github.com/emqx/emqx/pull/10877) 升级 RocketMQ 驱动程序以增强处理敏感数据的安全性。
-   [#10598](https://github.com/emqx/emqx/pull/10598) 在 ExProto 中提供了一种 Unary 类型的回调方法，以避免可能的消息乱序问题。
-   [#10895](https://github.com/emqx/emqx/pull/10895) 重构了大部分桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10790](https://github.com/emqx/emqx/pull/10790) 通过优化配置读取机制减少读取配置的开销。
-   [#10892](https://github.com/emqx/emqx/pull/10892) 在创建 Oracle 数据库桥接时要求设置 SID 或服务名称。
-   [#10910](https://github.com/emqx/emqx/pull/10910) 数据桥接资源选项 `auto_restart_interval` 已被弃用，改为使用 `health_check_interval`，而 `request_timeout` 则被重命名为 `request_ttl`。此外，默认的 `request_ttl` 值从 15 秒增加到了 45 秒。
    之前同时存在 `auto_restart_interval` 和 `health_check_interval` 会导致混淆，因为这两个参数都会影响数据桥接在故障下的恢复。这两个参数的配置不一致可能导致消息过期而无法重试。现在，`health_check_interval` 用于控制进行健康检查的间隔，健康检查会将数据桥接转换为 `disconnected` 或 `connecting` 状态，也可以让数据桥接从 `disconnected` 状态中进行恢复。
-   [#10929](https://github.com/emqx/emqx/pull/10929) 升级 Erland/OTP 到 25.3.2-1。
-   [#10909](https://github.com/emqx/emqx/pull/10909) 移除了网关已弃用的 HTTP API。
-   [#10908](https://github.com/emqx/emqx/pull/10908) 重构了 RocketMQ 桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10924](https://github.com/emqx/emqx/pull/10924) 重构了 Influxdb 桥接，避免在初始化阶段出现崩溃时资源泄漏。
-   [#10944](https://github.com/emqx/emqx/pull/10944) 改进了 GCP PubSub 桥接，以避免在重启节点时可能出现消息发送失败的潜在问题。
-   [#10933](https://github.com/emqx/emqx/pull/10933) 支持在 MQTT/TCP 和 MQTT/SSL 监听器中配置 TCP keep-alive。
-   [#10948](https://github.com/emqx/emqx/pull/10948) 在一些 HTTP API 中添加了 live_connections 字段，例如：
    -   `/monitor_current，/monitor_current/nodes/{node}`
    -   `/monitor/nodes/{node}，/monitor`
    -   `/node/{node}，/nodes`
-   [#10941](https://github.com/emqx/emqx/pull/10941) 设置 `prometheus.vm_dist_collector=disabled` 且度量指标 `erlang_vm_statistics_run_queues_length_total` 被重命名为`erlang_vm_statistics_run_queues_length`，提高 Prometheus 指标的收集速度。
-   [#10985](https://github.com/emqx/emqx/pull/10985) 将 `emqx ctl` 命令的名称从 `cluster_call` 更名为 `conf cluster_sync`。旧命令 `cluster_call` 仍然是一个有效的命令，但不包含在帮助信息中。
-   [#10988](https://github.com/emqx/emqx/pull/10988) 改进日志安全性，确保在数据桥接创建失败时敏感数据始终被模糊化。
-   [#10926](https://github.com/emqx/emqx/pull/10926) 允许在监听器的状态标志中使用 `enable` 和 "`enabled`。
    在此更改之前，可以通过在 `enabled` 配置上设置 `true` 或 `false` 来启用/禁用监听器。与系统中其他状态标志的命名略有不同。现在，添加了 `enable` 标志作为监听器的别名。
-   [#10970](https://github.com/emqx/emqx/pull/10970) 已向 Kafka 生产者桥接添加了一个 query_mode 参数。该参数允许您指定在向 Kafka 发送数据时桥接应该使用异步模式还是同步模式。默认为异步模式。
-   [#10676](https://github.com/emqx/emqx/pull/10676) 新增了用于导入/导出配置和用户数据的命令 `emqx ctl export ` 和 `emqx ctl import`， 允许从正在运行的 EMQX 集群中导出配置和内置数据库数据，然后将其导入到相同或另一个正在运行的 EMQX 集群中。
-   [#11003](https://github.com/emqx/emqx/pull/11003) 在 Kafka 数据桥接中添加一个配置 TCP keepalive 的选项。
-   [#10961](https://github.com/emqx/emqx/pull/10961) 通过允许在配置和 HTTP API 中的 `max_connections` 字段中使用无限大（infinity）作为有效值，为网关监听器添加了对无限制最大连接数的支持。
-   [#11019](https://github.com/emqx/emqx/pull/11019) 改进了 JWT 的日志安全性，现在在打印之前将进行模糊化处理。
-   [#11024](https://github.com/emqx/emqx/pull/11024) 添加了一个小的改进，以减少在创建/更新 Pulsar 生产者桥接时看到`connecting` 状态的机会。
-   [#11034](https://github.com/emqx/emqx/pull/11034) 隐藏 "broker" 配置， 并将 `broker.shared_subscription_strategy` 改为 `mqtt.shared_subscription_strategy` 因为它属于 mqtt 的特性。
-   [#11045](https://github.com/emqx/emqx/pull/11045) 监听器认证和分区相关 api 在 `5.1.0`版本中被正式移除。
-   [#11062](https://github.com/emqx/emqx/pull/11062) 将 `log.file.to` 更名为 `log.file.path`。

### 修复

-   [#11018](https://github.com/emqx/emqx/pull/11018) 修复了 Stomp 网关的多个问题，包括：
    -   修复了关于 is_superuser 无法正常工作的问题。
    -   修复了 mountpoint 在消息发送中没有被移除的问题。
    -   消息或订阅请求失败后，Stomp 客户端应该在回复错误信息后立即断开。
-   [#11051](https://github.com/emqx/emqx/pull/11051) 增加了对证书`层级`（监听器 SSL 选项）须为非负整数的验证。
-   [#10563](https://github.com/emqx/emqx/pull/10563) 修复了订阅时 no_local flag 无法正常工作的问题。
-   [#10653](https://github.com/emqx/emqx/pull/10653) 将网关认证的 TLS 证书和密钥保存到数据目录以修复内存泄漏问题。
-   [#10682](https://github.com/emqx/emqx/pull/10682) 修正了在会话创建时为遗嘱消息赋予时间戳这个错误，现在该时间戳为会话断开时间。
-   [#10701](https://github.com/emqx/emqx/pull/10701) Amazon Linux 2 的 EMQX RPM 软件包不支持 TLS v1.3，因为它是使用内置 openssl 1.0 的 Erlang/OTP 构建的。
-   [#10677](https://github.com/emqx/emqx/pull/10677) 修复了规则 API 中的问题：当尝试删除不存在的规则时，会响应 404 HTTP 错误代码。
-   [#10715](https://github.com/emqx/emqx/pull/10715) 支持在客户端连接钩子函数(client.connected hooks)中获取客户端证书。之前，为了减少内存消耗在建立连接后移除了该数据。
-   [#10737](https://github.com/emqx/emqx/pull/10737) 修复了网关的 HTTP API 接口无法处理包含特殊字符的 ClientID 的问题，例如：`!@#$%^&*()_+{}:"<>?/`。
-   [#10809](https://github.com/emqx/emqx/pull/10809) 解决节点关闭或重启时出现的 `** ERROR ** Mnesia post_commit hook failed: error:badarg` 错误信息。Mria pull request: [https://github.com/emqx/mria/pull/142](https://github.com/emqx/mria/pull/142)
-   [#10807](https://github.com/emqx/emqx/pull/10807) 在 debug 级别下，不再输出 license 检查相关的日志，该日志产生过于频繁，可能会干扰日志记录。
-   [#10818](https://github.com/emqx/emqx/pull/10818) 修复了 `emqx_ctl traces` 命令错误，其中 `emqx_mgmt_cli` 模块中的 `traces start `命令在某些过滤器下无法正常工作。
-   [#10600](https://github.com/emqx/emqx/pull/10600) 删除了 emqx_statsd 应用。
-   [#10820](https://github.com/emqx/emqx/pull/10820) 修复了集群更新 license 后，新加入的节点不会应用新 license 而是继续使用旧 license 的问题。
    有时新节点必须使用过时的 license。例如，在 license 过期后使用 emqx-operator 进行部署，并且需要扩展规模。此时，集群的 license 已经通过 API/CLI 更新，但新节点不会使用它。
-   [#10851](https://github.com/emqx/emqx/pull/10851) 在错误的 API 日志中对敏感数据进行了混淆处理。
-   [#10884](https://github.com/emqx/emqx/pull/10884) 修复了在节点加入集群时尝试获取规则信息或指标可能导致崩溃的问题。
-   [#10887](https://github.com/emqx/emqx/pull/10887) 修复了一个潜在问题，即对桥接器的请求可能需要很长时间才能进行重试。
    这只影响低吞吐量的情况，其中缓冲层可能需要很长时间才能检测到连接和驱动程序问题。
-   [#10878](https://github.com/emqx/emqx/pull/10878) 已修复了 RabbitMQ 桥接程序中的一个漏洞，该漏洞可能会将密码暴露到日志文件中。
-   [#10871](https://github.com/emqx/emqx/pull/10871) 修复了一个问题，即在 CoAP 连接断开后，Dashboard 仍显示连接存在，但删除和消息发布请求不生效。
-   [#10880](https://github.com/emqx/emqx/pull/10880) 新增了一个 REST API `POST /clients/kickout/bulk`，用于批量踢出多个客户端。
-   [#10913](https://github.com/emqx/emqx/pull/10913) 修复了某个节点离开集群后，其插件状态 REST API 仍会包含集群节点状态问题。
-   [#10923](https://github.com/emqx/emqx/pull/10923) 修复了通道信息注册中的竞态条件。
    在此修复之前，当系统负载较重时，可能出现客户端已断开连接（或会话已过期），但仍可在 Dashboard 的客户端页面中找到的情况。其中一个可能的原因是期间发生的竞态条件：连接在通道数据注册过程中被中断。
-   [#10930](https://github.com/emqx/emqx/pull/10930) 增加了对持续时间数据类型的 schema 验证，以避免使用无效值。
    在此修复之前，可以在 schema 中使用不合理的值，超出系统限制，从而导致崩溃。
-   [#10952](https://github.com/emqx/emqx/pull/10952) 如果设置了`verify = verify_none`，则禁止在监听器 SSL 选项中启用 `fail_if_no_peer_cert`。
    设置 `fail_if_no_peer_cert = true` 和 `verify = verify_none` 会导致连接错误，因为选项不兼容。此修复在创建或更新监听器时验证选项，以避免这些错误。

    注意：应用此修复后，任何具有 `fail_if_no_peer_cert = true` 和 `verify = verify_none` 的旧监听器配置将无法加载，并且必须手动修复。
-   [#10951](https://github.com/emqx/emqx/pull/10951) 修复了 MQTT-SN 网关中发布消息时挂载点未生效的问题。
-   [#10943](https://github.com/emqx/emqx/pull/10943) 弃用了集群发现的 UDP 组播机制。
    该功能自5.0版本以来一直计划弃用，主要是因为实际生产中缺乏使用。尽管该功能的代码在5.1版本中尚未移除，但文档接口已经被降级处理。
-   [#10902](https://github.com/emqx/emqx/pull/10902) 避免从运行较新版本的节点同步 `cluster.hocon` 文件。
    在集群滚动升级期间，如果旧版本节点由于任何原因需要重新启动，如果它从较新版本的节点复制 `cluster.hocon` 文件，可能会导致启动失败。在此修复后，旧版本节点将不会从较新版本的节点复制 `cluster.hocon` 文件，而是使用自己的 `cluster.hocon` 文件进行启动。
-   [#10967](https://github.com/emqx/emqx/pull/10967) 修复了重平衡 API 中错误消息的格式问题：之前它们可能以不清晰的 Erlang 内部结构转储的形式显示。
    在节点疏散的 CLI 和 API 中添加了 `wait_health_check` 选项。这是一个时间间隔，节点在此期间报告为"不健康状态"，但不会开始实际的疏散操作。我们需要这个选项来允许负载均衡器（如果有）将已疏散的节点从负载均衡中移除，并且不将（重新）连接的客户端转发到已疏散的节点。
-   [#10911](https://github.com/emqx/emqx/pull/10911) 修改了当尝试创建一个名称超过255个字节的桥接时出现的错误消息和日志条目的内容，使其更易于理解。
-   [#10983](https://github.com/emqx/emqx/pull/10983) 修复了一个问题，即当 MQTT 客户端尝试通过配置为仅使用 TLS v1.3 的监听器进行连接时，无法建立TLS连接。
    问题在于 TLS 连接尝试使用与 TLS v1.3 不兼容的选项。
-   [#10977](https://github.com/emqx/emqx/pull/10977) 修复了订阅计数指标更新延迟以及 Stomp 网关中的配置问题。
-   [#10950](https://github.com/emqx/emqx/pull/10950) 修复了在 MQTT-SN 网关中使 `enable_qos` 选项无效的问题。
-   [#10999](https://github.com/emqx/emqx/pull/10999) 更改了 Kafka 字段 "Partition Count Refresh Interval" 和 "Offset Commit Interval" 的 schema 验证，以避免接受超过最大允许值的值。
-   [#10997](https://github.com/emqx/emqx/pull/10997) ClickHouse 桥接存在一个问题，即当 ClickHouse 服务器在发送消息时关闭时，即使请求的 ttl（time to live）设置为无限大，也可能导致消息丢失。通过将由于连接关闭引起的错误视为可恢复错误修复了该问题。
-   [#10994](https://github.com/emqx/emqx/pull/10994) 在 HTTP 连接器中，对 `proxy-authorization headers` 进行了屏蔽处理，以防止将机密信息泄露到日志文件中。
-   [#10996](https://github.com/emqx/emqx/pull/10996) 对于任何未知的 HTTP/API 请求，默认返回404错误，而不是返回 Dashboard 的 index.html 页面。
-   [#11005](https://github.com/emqx/emqx/pull/11005) 修复了在 AuthN HTTP 的跟踪日志中无法正确打印方法字段的问题。
-   [#11006](https://github.com/emqx/emqx/pull/11006) 修复了 QUIC 监听器的默认证书文件路径。
    在此更改之前，默认的证书文件路径以环境变量 `${EMQX_ETC_DIR}` 为前缀，但在 QUIC 监听器中使用之前未进行插值处理。
-   [#10998](https://github.com/emqx/emqx/pull/10998) 不允许为 MongoDB 桥接资源设置 `batch_size` 选项。当前的 MongoDB 连接器不支持批量处理，如果提供了 `batch_size` 配置值，它将被强制设置为1。
-   [#10955](https://github.com/emqx/emqx/pull/10955) 修复了 MQTT-SN 网关中对预定义主题的配置删除无效的问题。
-   [#11025](https://github.com/emqx/emqx/pull/11025) 修复了Pulsar Producer 桥接中可能在竞态条件下引发的 `case_clause` 错误。
-   [#11030](https://github.com/emqx/emqx/pull/11030) 改进了在使用 Listeners HTTP API 时发生验证错误所产生的错误信息。
-   [#11033](https://github.com/emqx/emqx/pull/11033) 在 ExProto 网关中，弃用了 `AuthenticateRequest` 中的 `mountpoint` 字段。
    该字段在 e4.x 版本中引入，但实际上，在 e5.0 版本中，我们已经提供了 `gateway.exproto.mountpoint` 进行配置，因此无需通过 Authenticate 请求来覆盖它。

    此外，将 `subscriptions_max`、`inflight_max` 和 `mqueue_max` 的默认值更新为无限大。
-   [#11040](https://github.com/emqx/emqx/pull/11040) 修复了 Kafka 生产者桥接的健康检查问题，当与 Kafka 代理的连接断开时该问题可能会导致消息丢失。
-   [#11038](https://github.com/emqx/emqx/pull/11038) 修复了 Pulsar 生产者的健康检查问题，当与 Pulsar 代理的连接断开时该问题可能会导致消息丢失。
-   [#11042](https://github.com/emqx/emqx/pull/11042) 修复了当监听器的 max_connections 配置设为字符串时 REST API `GET /listeners` 崩溃的问题。
-   [#11028](https://github.com/emqx/emqx/pull/11028) 禁止在监听器配置中同时使用包括 tlsv1.3 但排除 tlsv1.2 的多个 TLS 版本。
    使用具有这种版本差异的 TLS 配置会导致连接错误。此外，删除和记录与所选 TLS 版本不兼容的 TLS 选项。

    注意：应用此修复后，任何包含上述版本差异的旧监听器配置将无法加载，必须手动修复。
-   [#11031](https://github.com/emqx/emqx/pull/11031) 修复了创建桥接和检查 InfluxDB 桥接时的认证信息验证问题。
-   [#11056](https://github.com/emqx/emqx/pull/11056) 修复了新创建的监听器有时无法正确启动的问题。当您删除一个名为 "default" 的系统默认监听器并添加一个新的同名监听器时，它将无法正确启动。
    -   修复了某些节点上的配置失败可能导致 Dashboard 无法使用的错误。
-   [#11070](https://github.com/emqx/emqx/pull/11070) 修复了 `cluster.autoclean` 配置项无法生效的问题。
-   [#11100](https://github.com/emqx/emqx/pull/11100) 修复了复制节点由于 `mria_lb:core_nodes()` 调用超时而无法连接到核心节点的问题。
    相关的 mria pull request: [https://github.com/emqx/mria/pull/143](https://github.com/emqx/mria/pull/143)
-   [#11092](https://github.com/emqx/emqx/pull/11092) 修复复制节点因超时无法连接到核心节点的问题。

### [已知问题](https://github.com/emqx/emqx-docs/blob/release-5.1/en_US/changes/known-issues-5.1.0.md)

## 5.0.26

*发布日期：2023-05-29*

### 增强

- [#10584](https://github.com/emqx/emqx/pull/10584) 为 SSL 通信添加日志级别配置
- [#10702](https://github.com/emqx/emqx/pull/10702) 引入更直观的配置选项 `keepalive_multiplier` 并废弃旧的 `keepalive_backoff` 配置。经过此次增强，EMQX 通过将“客户端请求的保活间隔”乘以 `keepalive_multiplier` 来检查客户端的保活超时状态。
- [#10713](https://github.com/emqx/emqx/pull/10713) 我们隐藏了 webhook 的 resource_option 中的 request_timeout，使其与 webhook 的 http request_timeout 保持一致。从现在开始，通过 API 或配置文件配置 webhook 时，不再需要配置资源的 request_timeout。只需配置 http request_timeout 即可，资源中的 request_timeout 将自动与 http request_timeout 保持一致。
- [#10511](https://github.com/emqx/emqx/pull/10511) 通过在数据中掩盖敏感信息，提高某些资源日志的安全性和隐私性。
- [#10678](https://github.com/emqx/emqx/pull/10678) 优化计数器增加调用，避免增量为零时的工作。
- [#10690](https://github.com/emqx/emqx/pull/10690) 为 webhook 桥接添加了重试机制，尝试提高吞吐量。这种优化在高消息率的情况下，通过不阻塞缓冲层重试请求失败，可以提高吞吐量。
- [#10698](https://github.com/emqx/emqx/pull/10698) 优化运行时访问配置时的内存使用。

### 修复

- [#10340](https://github.com/emqx/emqx/pull/10340) 修复了通过 systemd 停止 EMQX 时可能导致崩溃日志打印的问题。
- [#10563](https://github.com/emqx/emqx/pull/10563) 修正了 no_local 标志未正确工作的问题。
- [#10600](https://github.com/emqx/emqx/pull/10600) 删除了 emqx_statsd 应用。
- [#10653](https://github.com/emqx/emqx/pull/10653) 将网关认证 TLS 证书和密钥存储在数据目录中。
- [#10677](https://github.com/emqx/emqx/pull/10677) 在规则 API 中，尝试删除不存在的规则时，使用 404 HTTP 错误码响应。
- [#10682](https://github.com/emqx/emqx/pull/10682) 修复了遗嘱消息的时间戳错误地在会话创建时分配，现在这个时间戳是会话断开连接的时间。
- [#10701](https://github.com/emqx/emqx/pull/10701) Amazon Linux 2 的 RPM 包不支持 TLS v1.3，因为它是用 openssl 1.0 构建的 Erlang/OTP 组装的。
- [#10715](https://github.com/emqx/emqx/pull/10715) 将连接信息结构的修剪推迟到 `client.connected` 钩子执行之后。这些钩子再次可以访问客户端的对等证书。
- [#10717](https://github.com/emqx/emqx/pull/10717) 修复了当 inflight 窗口满时，缓冲层进程可能会使用大量 CPU 的问题。
- [#10724](https://github.com/emqx/emqx/pull/10724) 为 HTTP API 文档的所有端点添加了摘要（可在 "http://emqx_host_name:18083/api-docs" 访问）。
- [#10726](https://github.com/emqx/emqx/pull/10726) 验证健康检查间隔和自动重启间隔在 1ms 到 1 小时的范围内。
- [#10728](https://github.com/emqx/emqx/pull/10728) 修复了规则引擎无法访问 `FOREACH` 在 `DO` 子句中导出的变量的问题。
- [#10737](https://github.com/emqx/emqx/pull/10737) 修复了网关的 HTTP API 接口无法处理包含特殊字符的 ClientIDs 的问题，例如：`!@#$%^&*()_+{}:"<>?/`。
- [#10742](https://github.com/emqx/emqx/pull/10742) 在保存授权文件源之前检查规则的正确性。以前，保存错误的规则可能导致重启失败。
- [#10743](https://github.com/emqx/emqx/pull/10743) 修复了尝试获取桥接信息或指标可能导致在节点加入集群时崩溃的问题。
- [#10746](https://github.com/emqx/emqx/pull/10746) 在规则引擎测试 API `rule_test` 中添加缺失的事件 `$events/delivery_dropped` 支持。
- [#10747](https://github.com/emqx/emqx/pull/10747) 在规则引擎中重构日期和时间函数 `format_date` 和 `date_to_unix_ts`，以修复实现问题。
- [#10755](https://github.com/emqx/emqx/pull/10755) 修复了数据桥接资源更新的竞态条件。
- [#10760](https://github.com/emqx/emqx/pull/10760) 修复了有时在节点（重新）加入集群时更新桥接统计页面时发生的内部错误 500。
- [#10761](https://github.com/emqx/emqx/pull/10761) 修复了 Dashboard监听器的 SSL 证书默认值未正确插值的问题，导致在使用默认配置的 verify_peer 和 cacertfile 时 HTTPS 不可访问。
- [#10785](https://github.com/emqx/emqx/pull/10785) 确保 Windows 启动脚本设置了 `EMQX_LOG_DIR`。
- [#10801](https://github.com/emqx/emqx/pull/10801) 避免在 API `/topics/{topic}` 和 `/topics` 中重复解码主题名称。
- [#10809](https://github.com/emqx/emqx/pull/10809) 解决在节点关闭或重启期间发生的 `** ERROR ** Mnesia post_commit hook failed: error:badarg` 错误消息。Mria 拉取请求：https://github.com/emqx/mria/pull/142
- [#10817](https://github.com/emqx/emqx/pull/10817) 修复无法将 `auto_restart_interval` 配置为无限的错误
- [#10818](https://github.com/emqx/emqx/pull/10818) 修复 `emqx_ctl traces` 命令。
- [#10820](https://github.com/emqx/emqx/pull/10820) 如果集群在新节点加入前更新了许可证。新加入的节点将不会应用更新后的许可证。此更改后，新加入的节点将使用集群的许可证密钥。
- [#10833](https://github.com/emqx/emqx/pull/10833) 在遥测报告中只包括启用的认证器和授权器，而不是全部。
- [#10851](https://github.com/emqx/emqx/pull/10851) 在 API 日志记录中隐藏敏感数据。

## 5.0.25

*发布日期：2023-05-12*

### 增强

- [#10568](https://github.com/emqx/emqx/pull/10568) 在 `emqx ctl listeners` 命令中添加关闭计数器信息

- [#10571](https://github.com/emqx/emqx/pull/10571) 当 EMQX 停止时不产生无用的崩溃报告。以前，当 EMQX（特别是 `emqx_topic_metrics`）停止并移除底层表时，一些消息仍在处理中并导致崩溃。

- [#10588](https://github.com/emqx/emqx/pull/10588) 将跟踪日志的时间精度从秒增加到微秒。例如，从 `2023-05-02T08:43:50+00:00` 改为 `2023-05-02T08:43:50.237945+00:00`。

- [#10623](https://github.com/emqx/emqx/pull/10623) 在 `force_shutdown` 配置中将 `max_message_queue_len` 重命名为 `max_mailbox_size`。旧名称作为别名保留，因此此更改向后兼容。

- [#10417](https://github.com/emqx/emqx/pull/10417) 通过消除临时引用来提高获取配置性能。

- [#10525](https://github.com/emqx/emqx/pull/10525) 减少每个 MQTT 包处理的资源使用。

- [#10528](https://github.com/emqx/emqx/pull/10528) 在热代码路径中减少内存占用。

- [#10573](https://github.com/emqx/emqx/pull/10573) 当使用同步查询模式时，提高了 Webhook 桥接的性能。这也应该提高其他桥接在未配置批处理时的性能。

- \#10591

   改进限制器的配置。

  - 简化限制器配置的内存表示。
  - 确保当省略监听器的限制器配置时，节点级限制器确实可以工作。

- \#10625

   简化限制器配置。

  - 减少限制器配置的复杂性。例如，现在用户可以使用 `limiter.messages_rate = 1000/s` 快速设置消息发布的节点级限制。
  - 更新 `configs/limiter` API 以适应此重构。

### 修复

- [#10548](https://github.com/emqx/emqx/pull/10548) 修复了 HTTP 驱动中的一个竞态条件，该条件会导致错误而不是重试请求。相关驱动修复：https://github.com/emqx/ehttpc/pull/45
- [#10556](https://github.com/emqx/emqx/pull/10556) 如果在初始化时传递了 `Authorization` 头，则在 `emqx_connector_http` 中包装潜在敏感数据。
- [#10659](https://github.com/emqx/emqx/pull/10659) 修复了当 `sysmon.os.mem_check_interval` 被禁用时，emqx 无法启动的问题。

## 5.0.24

*发布日期：2023-04-26*

### 增强

- [#10457](https://github.com/emqx/emqx/pull/10457) 废弃与 StatsD 的集成。似乎没有用户使用 StatsD 集成，因此我们决定暂时隐藏此功能。我们将根据未来的需求决定是移除还是复活它。
- [#10458](https://github.com/emqx/emqx/pull/10458) 将插件配置选项的级别设置为低级别，在大多数情况下，用户只需要在 Dashboard上管理插件，无需手动修改，因此我们降低了级别。
- [#10491](https://github.com/emqx/emqx/pull/10491) 将 `etcd.ssl` 重命名为 `etcd.ssl_options` 以保持配置文件中所有 SSL 选项的一致性。
- [#10512](https://github.com/emqx/emqx/pull/10512) 改进数据文件中 Unicode 字符的存储格式，现在我们可以正常存储 Unicode 字符。例如："SELECT * FROM "t/1" WHERE clientid = "-测试专用-""
- [#10487](https://github.com/emqx/emqx/pull/10487) 优化速率为 `infinity` 的限制器实例以减少内存和 CPU 使用。
- [#10490](https://github.com/emqx/emqx/pull/10490) 移除了默认的连接速率限制，以前默认为 `1000/s`

### 修复

- [#10407](https://github.com/emqx/emqx/pull/10407) 通过使用 Mnesia 脏操作和避免从 `emqx_resource_manager` 不必要的调用来重新激活已激活的警报来改善 `emqx_alarm` 的性能。使用新的安全 `emqx_alarm` API 激活/停用警报，以确保 `emqx_resource_manager` 不会因为警报超时而崩溃。当以下条件同时发生时，可能会出现崩溃：

  - 相对较高数量的失败资源，例如桥接尝试在重复错误时激活警报；
  - 系统经历非常高的负载。

- [#10420](https://github.com/emqx/emqx/pull/10420) 修复在认证和授权模块中组合 HTTP 请求 URL 时处理 HTTP 路径的问题。

  - 避免不必要的 URL 规范化，因为我们不能假设外部服务器将原始 URL 和规范化 URL 视为等同。这导致了像 [#10411](https://github.com/emqx/emqx/issues/10411) 这样的错误。
  - 修复路径段可能被 HTTP 编码两次的问题。

- [#10422](https://github.com/emqx/emqx/pull/10422) 修复了独立节点集群中无法通过环境变量配置外部插件的错误。

- [#10448](https://github.com/emqx/emqx/pull/10448) 修复了 v5.0.23 引入的限制器配置兼容性问题，如果 `capacity` 为 `infinity`，则破坏了从以前版本升级的能力。在 v5.0.23 中，我们用 `burst` 替换了 `capacity`。此修复后，`capacity = infinity` 配置将自动转换为等价的 `burst = 0`。

- [#10449](https://github.com/emqx/emqx/pull/10449) 在创建认证 HTTP (`authn_http`) 时，验证 `ssl_options` 和头部配置。在此之前，错误的 `ssl` 配置可能导致成功创建但整个 authn 不可用。

- [#10455](https://github.com/emqx/emqx/pull/10455) 修复了可能导致日志中（否则无害的）噪声的问题。在一些特别慢的同步调用桥接期间，一些迟到的回复可能被发送到不再期待回复的连接进程，然后发出类似错误日志：

  ```yaml
  2023-04-19T18:24:35.350233+00:00 [error] msg: unexpected_info, mfa: emqx_channel:handle_info/2, line: 1278, peername: 172.22.0.1:36384, clientid: caribdis_bench_sub_1137967633_4788, info: {#Ref<0.408802983.1941504010.189402>,{ok,200,[{<<"cache-control">>,<<"max-age=0, ...">>}}
  ```

  这些日志是无害的，但它们可能无需地让用户担忧。

- [#10462](https://github.com/emqx/emqx/pull/10462) 废弃配置 `broker.shared_dispatch_ack_enabled`。这是设计来避免向已断开客户端的共享订阅会话分发消息。然而，自 v5.0.9 起，此功能不再有用，因为过期会话中的共享订阅消息将被重新分发给组中的其他会话。参见：https://github.com/emqx/emqx/pull/9104

- [#10463](https://github.com/emqx/emqx/pull/10463) 改进桥接 API 错误处理。如果 Webhook 桥接 URL 无效，桥接 API 将返回 '400' 错误而不是 '500'。

- [#10484](https://github.com/emqx/emqx/pull/10484) 修复了在滚动升级期间无法设置配置优先级的问题。例如，当在 v5.0.21 中修改授权然后通过滚动升级升级到 v5.0.23 时，授权将被恢复为默认设置。

- [#10495](https://github.com/emqx/emqx/pull/10495) 误删的限制器 API `/configs/limiter` 被加回。

- [#10500](https://github.com/emqx/emqx/pull/10500) 在 Mria 中添加了几个修复、增强和功能：

  - 使用全局锁保护 `mria:join/1,2`，以防止两个节点同时尝试加入对方时发生冲突 [Mria PR](https://github.com/emqx/mria/pull/137)
  - 实现新函数 `mria:sync_transaction/4,3,2`，该函数阻塞调用者直到事务被导入到本地节点（如果本地节点是副本，则行为完全相同如 `mria:transaction/3,2`）[Mria PR](https://github.com/emqx/mria/pull/136)
  - 优化 `mria:running_nodes/0` [Mria PR](https://github.com/emqx/mria/pull/135)
  - 当在副本节点上调用时，优化 `mria:ro_transaction/2` [Mria PR](https://github.com/emqx/mria/pull/134)。

- [#10518](https://github.com/emqx/emqx/pull/10518) 在 Mria 中添加以下修复和功能：

  - 在 mria_membership 中安全调用 `mria_rlog:role/1`，以确保如果 RPC 到另一个节点失败，mria_membership gen_server 不会崩溃 [Mria PR](https://github.com/emqx/mria/pull/139)
  - 在 ?rlog_sync 表中添加额外字段，以便于在未来扩展此功能 [Mria PR](https://github.com/emqx/mria/pull/138)。

## 5.0.23

*发布日期：2023-04-18*

### 增强

- [#10156](https://github.com/emqx/emqx/pull/10156) 更改配置的优先级：

  1. 如果是新安装的 EMQX，配置的优先级为 `ENV > emqx.conf > HTTP API`。
  2. 如果是从旧版本升级的 EMQX（即，EMQX 数据目录中仍存在 cluster-override.conf 文件），则配置优先级保持不变，即 `HTTP API > ENV > emqx.conf`。

  废弃 data/configs/local-override.conf。

  稳定 HTTP API 的热更新。

- [#10354](https://github.com/emqx/emqx/pull/10354) 当配置错误的 max_heap_size 值时，提供更具体的错误消息。当抛出 `message_queue_too_long` 错误时，记录当前值和最大值。

- [#10359](https://github.com/emqx/emqx/pull/10359) 在 API 处理程序不使用它们的地方，现在不会隐式收集指标。相反，单独的后台 RPC 收集集群范围内的指标。

- [#10373](https://github.com/emqx/emqx/pull/10373) 废弃 trace.payload_encode 配置。在通过 HTTP API 创建跟踪时添加 payload_encode=[text,hidden,hex] 选项。

- [#10389](https://github.com/emqx/emqx/pull/10389) 统一 `cluster.core_nodes` 和 `cluster.statics.seeds` 的配置格式。现在它们都支持数组格式 `["emqx1@127.0.0.1", "emqx2@127.0.0.1"]` 或分号分隔的字符串 `"emqx1@127.0.0.1,emqx2@127.0.0.1"`。

- [#10391](https://github.com/emqx/emqx/pull/10391) 隐藏大量高级选项以简化配置文件。包括 `rewrite`、`topic_metric`、`persistent_session_store`、`overload_protection`、`flapping_detect`、`conn_congestion`、`stats,auto_subscribe`、`broker_perf`、`shared_subscription_group`、`slow_subs`、`ssl_options.user_lookup_fun` 以及 `node` 和 `dashboard` 部分的一些高级项目，[#10358](https://github.com/emqx/emqx/pull/10358)、[#10381](https://github.com/emqx/emqx/pull/10381)、[#10385](https://github.com/emqx/emqx/pull/10385)。

- [#10392](https://github.com/emqx/emqx/pull/10392) 新增一个将格式化日期转换为整数时间戳的函数：date_to_unix_ts/3

- [#10404](https://github.com/emqx/emqx/pull/10404) 将缓冲工作队列的默认模式更改为 `memory_only`。此更改之前，默认队列模式为 `volatile_offload`。在高消息速率压力下，当资源无法跟上这种速率时，由于不断的磁盘操作，缓冲性能大幅降低。

- [#10426](https://github.com/emqx/emqx/pull/10426) 优化配置优先级机制，修复了重启 EMQX 后对 `etc/emqx.conf` 进行的配置更改不生效的问题。关于新机制的更多介绍：配置覆盖规则

- [#10376](https://github.com/emqx/emqx/pull/10376) 简化限制器功能的配置并优化一些代码

  - 将 `message_in` 重命名为 `messages`
  - 将 `bytes_in` 重命名为 `bytes`
  - 使用 `burst` 而不是 `capacity`
  - 隐藏非重要字段
  - 优化不同速率设置下的限制器实例

- [#10430](https://github.com/emqx/emqx/pull/10430) 简化 `retainer` 功能的配置。

  - 将 `flow_control` 标记为非重要字段。

### 修复

- [#10369](https://github.com/emqx/emqx/pull/10369) 修复 `/api/v5/monitor_current` API 端点的错误，该错误发生在一些 EMQX 节点宕机时。在此修复之前，有时请求返回 HTTP 代码 500 和以下消息：

  ```less
  {"code":"INTERNAL_ERROR","message":"error, badarg, [{erlang,'++',[{error,nodedown},[{node,'emqx@10.42.0.150'}]], ...
  ```

- [#10410](https://github.com/emqx/emqx/pull/10410) 修复在 emqx.conf 中配置网关时配置检查失败的问题。这个问题首次出现在 v5.0.22 中通过 [#10278](https://github.com/emqx/emqx/pull/10278)，启动时配置检查缺失。

## 5.0.22

*发布日期：2023-04-13*

### 增强

- [#10077](https://github.com/emqx/emqx/pull/10077) 添加对 QUIC TLS 密码保护证书文件的支持。
- [#10128](https://github.com/emqx/emqx/pull/10128) 为 SSL MQTT 监听器添加对 OCSP stapling 的支持。
- [#10164](https://github.com/emqx/emqx/pull/10164) 为 TLS MQTT 监听器添加 CRL 检查支持。
- [#10206](https://github.com/emqx/emqx/pull/10206) 将查询模式与缓冲工作者的底层调用模式解耦。
- [#10207](https://github.com/emqx/emqx/pull/10207) 在 OpenAPI 规范中使用 i18n 文件中的 'label' 作为 'summary'。
- [#10210](https://github.com/emqx/emqx/pull/10210) 当 Mria 停止时注销 Mnesia post commit 钩子。
- [#10224](https://github.com/emqx/emqx/pull/10224) 在 Helm 图表中添加自定义 `clusterIP` 选项的功能。
- [#10263](https://github.com/emqx/emqx/pull/10263) 添加命令 'eval-ex' 用于 Elixir 表达式评估。
- [#10278](https://github.com/emqx/emqx/pull/10278) 重构所有网关的目录结构。
- [#10306](https://github.com/emqx/emqx/pull/10306) 为大多数桥接添加对 `async` 查询模式的支持。
- [#10318](https://github.com/emqx/emqx/pull/10318) 规则引擎语言的 FROM 子句现支持双引号 (") 和单引号 (') 包围的字符串。
- [#10336](https://github.com/emqx/emqx/pull/10336) 添加 `/rule_engine` API 端点来管理规则引擎的配置。

### 修复

- [#10145](https://github.com/emqx/emqx/pull/10145) 修复 `bridges` API 以报告失败桥接的错误条件为 `status_reason`。
- [#10154](https://github.com/emqx/emqx/pull/10154) 更改桥接和连接器的默认 `resume_interval` 为 `health_check_interval` 和 `request_timeout / 3` 的最小值。
- [#10172](https://github.com/emqx/emqx/pull/10172) 修复错误的默认 ACL 规则。
- [#10174](https://github.com/emqx/emqx/pull/10174) 将库 `esockd` 从 5.9.4 升级到 5.9.6。
- [#10195](https://github.com/emqx/emqx/pull/10195) 为包含 HTML 的 API 架构添加标签，否则会破坏生成文档的格式。
- [#10196](https://github.com/emqx/emqx/pull/10196) 在生成的在线文档菜单中使用小写字母的架构摘要和描述。
- [#10209](https://github.com/emqx/emqx/pull/10209) 修复踢出被禁客户端时可能发布的遗嘱消息问题。
- [#10211](https://github.com/emqx/emqx/pull/10211) 隐藏 `broker.broker_perf` 配置和 API 文档。
- [#10225](https://github.com/emqx/emqx/pull/10225) 允许安装插件，如果其名称与另一个已安装插件名称的开头匹配。
- [#10226](https://github.com/emqx/emqx/pull/10226) `/bridges` API 验证错误时不崩溃，返回 `400`。
- [#10237](https://github.com/emqx/emqx/pull/10237) 确保 `/nodes/:node[/metrics|/stats]` API 未知节点名返回 `404` 状态码。
- [#10242](https://github.com/emqx/emqx/pull/10242) 修复日志数据字段名称冲突问题。
- [#10251](https://github.com/emqx/emqx/pull/10251) 将 `FROM` 规则子句中引用的桥接视为依赖项。
- [#10257](https://github.com/emqx/emqx/pull/10257) 修复 LwM2M 网关中 `auto_observe` 不工作的问题。
- [#10286](https://github.com/emqx/emqx/pull/10286) 增强启动失败时的日志行为。
- [#10297](https://github.com/emqx/emqx/pull/10297) 使 `eval` 命令与 v4 向后兼容，即使在 Elixir 节点上也只评估 Erlang 表达式。对于 Elixir 表达式，使用 `eval-ex` 命令。
- [#10300](https://github.com/emqx/emqx/pull/10300) 修复使用 Elixir 构建的版本在未手动创建 `plugins` 文件夹以接收上传文件之前无法接收上传的插件问题。
- [#10313](https://github.com/emqx/emqx/pull/10313) 确保核心节点或副本节点启动时，`cluster-override.conf` 文件仅从核心节点复制。
- [#10314](https://github.com/emqx/emqx/pull/10314) 修复 `/monitor_current` API 使其仅查看当前节点。
- [#10315](https://github.com/emqx/emqx/pull/10315) 修复 `/mqtt/delayed/messages` API 调用中检查 `limit` 和 `page` 参数时崩溃问题。
- [#10317](https://github.com/emqx/emqx/pull/10317) 在广泛验证之前不公开监听器级别的认证。
- [#10323](https://github.com/emqx/emqx/pull/10323) 出于安全原因，API 示例中的 `password` 字段值替换为 `******`。
- [#10327](https://github.com/emqx/emqx/pull/10327) 不在接收到不可恢复的桥接错误时增加 'actions.failed.unknown' 规则指标计数器。

## 5.0.21

*发布日期：2023-03-24*

### 增强

- [#10022](https://github.com/emqx/emqx/pull/10022) 开始发布 Rocky Linux 9（兼容 Enterprise Linux 9）和 MacOS 12 包
- [#10139](https://github.com/emqx/emqx/pull/10139) 在 EMQX Helm 图表中添加 `extraVolumeMounts`，使其能够将用户自己的文件挂载到 EMQX 实例中，例如，如 [#9052](https://github.com/emqx/emqx/issues/9052) 中提到的 ACL 规则文件
- [#9893](https://github.com/emqx/emqx/pull/9893) 使用 `clean_start=false` 连接时，EMQX 将过滤掉被禁止客户端发布的消息。以前，被禁止客户端发送的消息可能仍会在此场景中传递给订阅者
- [#9986](https://github.com/emqx/emqx/pull/9986) 对于 helm 图表，添加 MQTT 入站桥接；并删除过时的 `mgmt` 引用
- [#10123](https://github.com/emqx/emqx/pull/10123) 提高 `/bridges` API 的性能。之前，当集群中的节点数量较多或节点繁忙时，API 可能会出现请求超时
- [#9998](https://github.com/emqx/emqx/pull/9998) 出于安全原因，对认证错误日志中的 HTTP 请求体进行隐藏

### 修复

- [#10013](https://github.com/emqx/emqx/pull/10013) 修复 `/gateways/:name/clients` API 架构错误情况下的返回类型结构
- [#10014](https://github.com/emqx/emqx/pull/10014) 在 Dashboard API `/monitor(_current)/nodes/:node` 中，如果节点不存在，则返回 `404` 而不是 `400`
- [#10026](https://github.com/emqx/emqx/pull/10026) 现在仅通过 `/bridges/:id/metrics` 端点公开指标。在获取所有桥接列表或创建桥接时，不再在其他 API 操作中返回指标
- [#10027](https://github.com/emqx/emqx/pull/10027) 允许在 docker 中运行时从 `EMQX_NODE__NAME` 设置节点名称。在此修复之前，只允许 `EMQX_NODE_NAME`
- [#10050](https://github.com/emqx/emqx/pull/10050) 确保桥接 API 对不存在的资源一致返回 `404` 状态码
- [#10052](https://github.com/emqx/emqx/pull/10052) 改进守护进程模式启动失败的日志
- [#10055](https://github.com/emqx/emqx/pull/10055) 修复配置参数 `mqtt.max_awaiting_rel` 无效的问题
- [#10056](https://github.com/emqx/emqx/pull/10056) 修复 `/bridges` API 状态码
- [#10066](https://github.com/emqx/emqx/pull/10066) 改进 `/briges_probe` 和 `[/node/:node]/bridges/:id/:operation` API 调用的错误消息，使其更易读，并将 HTTP 状态码设置为 `400` 而不是 `500`
- [#10074](https://github.com/emqx/emqx/pull/10074) 检查 `PUT /authorization/sources/:type` 中的类型是否与请求体中给定的 `type` 匹配
- [#10079](https://github.com/emqx/emqx/pull/10079) 修复 `shared_subscription_strategy` 的描述
- [#10085](https://github.com/emqx/emqx/pull/10085) 对 `/authorization/sources/:source[/*]` 中所有不存在源的请求一致返回 `404`
- [#10098](https://github.com/emqx/emqx/pull/10098) 修复了 MongoDB 授权模块查询数据库时日志文件中出现的崩溃错误
- [#10100](https://github.com/emqx/emqx/pull/10100) 修复使用增强认证的慢客户端通道崩溃问题
- [#10107](https://github.com/emqx/emqx/pull/10107) 对 `bridges API` 操作，如果 `bridge-id` 未知，现在返回 `404` 而不是 `400`
- [#10117](https://github.com/emqx/emqx/pull/10117) 修复加入节点在集群中其他节点上安装的插件不存在时发生的错误
- [#10118](https://github.com/emqx/emqx/pull/10118) 修复手动将 EMQX 副本节点加入集群的相关问题
- [#10119](https://github.com/emqx/emqx/pull/10119) 修复当 `statsd.server` 设置为空字符串时的崩溃问题
- [#10124](https://github.com/emqx/emqx/pull/10124) 增加 MongoDB 的默认心跳周期，以减少 MongoDB 日志文件中过度记录的风险
- [#10130](https://github.com/emqx/emqx/pull/10130) 修复 Dashboard中由环境变量原始值导致的配置显示乱码问题
- [#10132](https://github.com/emqx/emqx/pull/10132) 修复 `systemctl stop emqx` 命令生成的一些错误日志
- [#10144](https://github.com/emqx/emqx/pull/10144) 在调用 `emqx ctl` 时添加 `-setcookie` 模拟器标志，以防止在家目录为只读时 emqx cli 出现问题
- [#10157](https://github.com/emqx/emqx/pull/10157) 修复创建新监听器时默认速率限制配置未正确应用的问题

## 5.0.20

*发布日期：2023-03-10*

### 增强

- [#10059](https://github.com/emqx/emqx/pull/10059) 规则引擎 API 返回的错误以更易读的方式格式化，而不是直接返回包含堆栈跟踪的原始错误。

### 修复

- [#10032](https://github.com/emqx/emqx/pull/10032) 当集群中某些节点上的资源仍处于“初始化/连接”状态时，由于这些资源缺少 Metrics 信息，`bridges/` API 将会崩溃。此修复将忽略没有 Metrics 信息的资源。
- [#10044](https://github.com/emqx/emqx/pull/10044) 修复集群中已停止节点的节点信息格式化问题。此 bug 由 v5.0.18 引入。
- [#10054](https://github.com/emqx/emqx/pull/10054) 修复在使用 `/bridges_probe` API 测试 Data-Bridge 连接时使用的是混淆后的密码的问题。
- [#10058](https://github.com/emqx/emqx/pull/10058) 废弃未使用的 QUIC TLS 选项。只保留以下 TLS 选项供 QUIC 监听器使用：
  - cacertfile
  - certfile
  - keyfile
  - verify
- [#10076](https://github.com/emqx/emqx/pull/10076) 修复 webhook 桥接错误处理：连接超时应该是一个可重试的错误。在此修复之前，连接超时被归类为不可恢复错误并导致请求被丢弃。
- [#10078](https://github.com/emqx/emqx/pull/10078) 修复无效的 QUIC 监听器设置可能导致段错误的问题。
- [#10084](https://github.com/emqx/emqx/pull/10084) 修复将运行不同 EMQX 版本的核心节点加入集群的问题。[Mria PR](https://github.com/emqx/mria/pull/127)
- [#10086](https://github.com/emqx/emqx/pull/10086) 将 HTTP 客户端 ehttpc 升级到 `0.4.7`。在此升级之前，如果 `body` 为空但设置了 content-type HTTP 头，用于认证、授权和 webhook 的 HTTP 客户端可能会崩溃。更多详情见 [ehttpc PR#44](https://github.com/emqx/ehttpc/pull/44)。

## 5.0.19

*发布日期：2023-03-01*

### 修复

- [#10032](https://github.com/emqx/emqx/pull/10032) 当资源管理器忙于尝试与远端建立连接时，资源可能还没有任何指标信息。在此修复之前，`bridges/` API 处理程序在这种情况下会崩溃。

- [#10037](https://github.com/emqx/emqx/pull/10037) 修复 Swagger API 文档渲染崩溃问题。在 5.0.18 版本中，引入了一个导致配置架构中字段名重复的错误。这反过来导致生成的 Swagger 架构变得无效。

- [#10041](https://github.com/emqx/emqx/pull/10041) 对 influxdb 桥接，向 `write_syntax` 文档添加整数值占位符注释提示。同时支持为 `timestamp` 字段设置常量值。

- [#10042](https://github.com/emqx/emqx/pull/10042) 改进 `replicant` 节点在 `core` 集群分区时的行为（例如当核心节点离开集群时）。以前，副本节点无法重新平衡到核心节点的连接，直到核心集群再次成为一个整体。这由错误消息表示：`[error] line: 182, mfa: mria_lb:list_core_nodes/1, msg: mria_lb_core_discovery divergent cluster`。[Mria PR](https://github.com/emqx/mria/pull/123/files)

- \#10043

   修复在 v5.0.18 中引入的两个错误。

  - 环境变量 `SSL_DIST_OPTFILE` 对于非启动命令设置不正确。
  - 当从环境变量覆盖 cookie 时，EMQX 节点无法启动。

- [#10044](https://github.com/emqx/emqx/pull/10044) 修复集群中已停止节点的节点信息格式化问题。

## 5.0.18

*发布日期：2023-02-24*

### 增强

- [#10019](https://github.com/emqx/emqx/pull/10019) 为 QUIC 监听器添加低级调优设置。
- [#9213](https://github.com/emqx/emqx/pull/9213) 在 helm 图表中添加 pod 破坏预算。
- [#9949](https://github.com/emqx/emqx/pull/9949) QUIC 传输多流支持和 QUIC TLS cacert 支持。
- [#9967](https://github.com/emqx/emqx/pull/9967) 新的通用 TLS 选项 'hibernate_after' 用于减少每个空闲连接的内存占用，默认值：5秒。

### 修复

- [#10009](https://github.com/emqx/emqx/pull/10009) 验证 `GET /trace/:name/log` 的 `bytes` 参数不超过有符号 32 位整数。
- [#10015](https://github.com/emqx/emqx/pull/10015) 为了防止因环境变量提供的错误 EMQX 节点 cookie 导致的错误，我们实现了快速失败机制。以前，当提供了错误的 cookie 时，命令仍会尝试 ping 节点，导致错误消息 'Node xxx not responding to pings'。新实现中，如果检测到 cookie 不匹配，将记录一条消息指示 cookie 错误，并且命令将以错误代码 1 终止，不尝试 ping 节点。
- [#10020](https://github.com/emqx/emqx/pull/10020) 在启用批处理 (`batch_size` > 1) 的异步模式下修复桥接指标。
- [#10021](https://github.com/emqx/emqx/pull/10021) 修复 `emqx_ctl cluster join` 命令的目标节点未运行时的错误消息。
- [#9939](https://github.com/emqx/emqx/pull/9939) 允许在 Mnesia 启动之前发出 'emqx ctl cluster' 命令。在此更改之前，EMQX `replicant` 无法使用 `manual` 发现策略。现在可以使用 'manual' 策略加入集群。
- [#9958](https://github.com/emqx/emqx/pull/9958) 修复在 `clients` API 中找不到客户端 ID 时的错误 HTTP 响应格式。
- [#9961](https://github.com/emqx/emqx/pull/9961) 在执行 bin/emqx 中的非启动命令时避免解析配置文件以获取节点名称和 cookie。
- [#9974](https://github.com/emqx/emqx/pull/9974) 使用与 Dashboard相同的数据源向 statsd 和 prometheus 报告内存使用情况。在此修复之前，内存使用数据源是从过时的源收集的，在容器中表现不佳。
- [#9978](https://github.com/emqx/emqx/pull/9978) 修复选择使用 SSL 连接 Postgres (`authn`、`authz` 和 bridge) 时的配置问题。从 5.0.13 升级到更新的 EMQX 版本后，之前正常工作的配置可能无法完成连接。
- [#9997](https://github.com/emqx/emqx/pull/9997) 修复 Swagger API 架构生成。`deprecated` 元数据字段现在始终为布尔值，如 [Swagger 规范](https://swagger.io/specification/) 建议。

## 5.0.17

*发布日期：2023-02-13*

### 增强

- [#9802](https://github.com/emqx/emqx/pull/9802) 对 HTTP API 支持 HAProxy 协议。
- [#9871](https://github.com/emqx/emqx/pull/9871) 允许在 `authz` 规则的主题中任何位置使用占位符。例如：`{allow, {username, "who"}, publish, ["t/foo${username}boo/${clientid}xxx"]}`。
- [#9910](https://github.com/emqx/emqx/pull/9910) 在桥接 API 中添加 `start` 操作，允许在失败后手动重新连接。
- [#9917](https://github.com/emqx/emqx/pull/9917) 停止构建基于 debian slim 的 -alpine docker 镜像，因为它的大小比常规的大。
- [#9930](https://github.com/emqx/emqx/pull/9930) 将统计数据 `live_connections.count` 和 `live_connections.max` 暴露给 Prometheus。
- [#9936](https://github.com/emqx/emqx/pull/9936) 默认情况下在发行版中禁用 disksup（os_mon 的一部分），当发生磁盘错误时不发出警告。
- [#9954](https://github.com/emqx/emqx/pull/9954) 提高桥接性能。

### 修复

- [#9864](https://github.com/emqx/emqx/pull/9864) 修复会话已经清理时独占主题未被移除的问题。
- [#9875](https://github.com/emqx/emqx/pull/9875) 如果从 HTTP API 上传的插件包损坏，返回 `400`，同时如果插件未被接受则进行清理。
- [#9916](https://github.com/emqx/emqx/pull/9916) 修复 MQTT 桥接无法验证 TLS 通配符服务器证书的问题。
- [#9922](https://github.com/emqx/emqx/pull/9922) 修复桥接资源缓冲区的问题，如果足够多的异步查询在失败前填满 inflight 窗口，则可能导致缓冲区卡住。
- [#9923](https://github.com/emqx/emqx/pull/9923) 当启动或关闭过程中发生错误时，修复 REPORT_CB/2 CRASH 错误日志。
- [#9938](https://github.com/emqx/emqx/pull/9938) 将一些出站 MQTT 桥接错误报告为可恢复的，因此可以重试。
- [#9946](https://github.com/emqx/emqx/pull/9946) 为 MQTT 桥接添加回 `reconnect_interval` 作为已弃用字段。该字段在 v5.0.16/e5.0.0 中被错误移除，导致新版本无法在旧配置上启动。现在它作为已弃用字段被添加回来（如果提供，则忽略配置值）。
- [#9951](https://github.com/emqx/emqx/pull/9951) 如果为所有节点调用桥接 API 上的操作（`start|stop|restart`），则传播错误。
- [#9952](https://github.com/emqx/emqx/pull/9952) 禁止对入站 MQTT 桥接使用 QoS 2 进行订阅。不过，允许用户为入站 MQTT 桥接配置 `clean_start` 选项。

## 5.0.16

*发布日期：2023-02-02*

### 修复

- [#9824](https://github.com/emqx/emqx/pull/9824) 如果一个主题有多个路由，`topics/{topic}` API 端点会返回 `500 - 内部错误`。通过返回路由列表修复了这个问题。
- [#9832](https://github.com/emqx/emqx/pull/9832) 改进 'sync' 模式下桥接超时未获得响应时的错误日志。
- [#9834](https://github.com/emqx/emqx/pull/9834) 允许将 `mqtt.idle_timeout` 设置为 `infinity`（无限）。
- [#9839](https://github.com/emqx/emqx/pull/9839) 确保用户为 webhook 桥接指定的授权头的内容不会被打印到日志文件中。
- [#9884](https://github.com/emqx/emqx/pull/9884) 不要在任何单个资源的成功健康检查后恢复所有缓冲工作器。以前，在任何成功的健康检查后，所有资源的所有缓冲工作器都会被恢复。

## 5.0.15

*发布日期：2023-01-20*

### 增强

- [#9569](https://github.com/emqx/emqx/pull/9569) 通过在路径中添加 `rules/` 重构 `/authorization/sources/built_in_database/`。
- [#9585](https://github.com/emqx/emqx/pull/9585) `/bridges_probe` API 端点用于测试创建新数据桥的参数。
- [#9586](https://github.com/emqx/emqx/pull/9586) 基本认证不再允许用于 API 调用，必须改用 API 密钥。
- [#9628](https://github.com/emqx/emqx/pull/9628) 暴露额外的资源配置参数：`start_after_created` 和 `start_timeout`。
- [#9722](https://github.com/emqx/emqx/pull/9722) 为推送指标到 Prometheus Push Gateway 添加以下配置选项：
  - `headers`：允许自定义 HTTP 请求头。
  - `job_name`：允许自定义推送到 Push Gateway 的作业名称。
- [#9725](https://github.com/emqx/emqx/pull/9725) 从 emqx_authz、emqx_authn 和数据桥组件中移除配置 `auto_reconnect`。这是因为我们有另一个具有类似功能的配置：`resource_opts.auto_restart_interval`。
- [#9736](https://github.com/emqx/emqx/pull/9736) 重构 /bridges API 以使其与其他 API 更一致：
  - 桥接启用/禁用现在通过端点 `/bridges/{id}/enable/[true,false]` 完成。
  - `/bridges/{id}/operation/{operation}` 端点现在为 `/bridges/{id}/{operation}`。
  - 指标从 GET `/bridges/{id}` 响应中移出，现在可以通过 `/bridges/{id}/metrics` 获取。
  - 端点 `bridges/{id}/reset_metrics` 现在为 `/bridges/{id}/metrics/reset`。
- [#9774](https://github.com/emqx/emqx/pull/9774) 在通过 API 添加或修改 Dashboard 用户时，添加密码复杂度要求。现在密码必须包含字母、数字和特殊字符中的至少两种，并且长度必须为 8 到 64 个字符。

### 修复

- [#9626](https://github.com/emqx/emqx/pull/9626) 返回带有默认值的授权设置。
- [#9680](https://github.com/emqx/emqx/pull/9680) 修复 Influxdb v1 写入 API 中用户名和密码认证是强制的问题。
- [#9726](https://github.com/emqx/emqx/pull/9726) 修复客户端模糊搜索 API 结果缺少信息的问题，该信息可以告知下一页是否有更多结果。
- [#9735](https://github.com/emqx/emqx/pull/9735) 从 http、ldap、mongo、mqtt、mysql、pgsql 和 redis 的信息日志消息中移除密码信息。
- [#9748](https://github.com/emqx/emqx/pull/9748) 未配置 `max_connections` 的监听器将导致集群 `/listeners` API 返回 500 错误。
- [#9749](https://github.com/emqx/emqx/pull/9749) 修复某些情况下搜索 API 响应中的 `count` 值不正确的问题。
- [#9750](https://github.com/emqx/emqx/pull/9750) 启动后重新加载覆盖配置。
- [#9751](https://github.com/emqx/emqx/pull/9751) 修复更新/删除监听器后废弃的证书文件不会被删除的问题。
- [#9763](https://github.com/emqx/emqx/pull/9763) 修复未提供密码时的认证异常。
- [#9765](https://github.com/emqx/emqx/pull/9765) 正确解析环境变量覆盖中作为密码的小数。
- [#9769](https://github.com/emqx/emqx/pull/9769) 修复 Erlang shell 提示符版本前缀。e5.0.15 -> v5.0.15
- [#9780](https://github.com/emqx/emqx/pull/9780) 为资源工作器创建磁盘队列目录时，将工作器 id 中的 ':' 替换为 '-'。
- [#9781](https://github.com/emqx/emqx/pull/9781) 创建下载的 zip 文件时，跟踪文件留在节点上。现在文件发送时会被删除，且并发下载不会再相互干扰。
- [#9785](https://github.com/emqx/emqx/pull/9785) 如果 `emqx_authentication` 提供了明确结果，则停止认证钩子链。
- [#9787](https://github.com/emqx/emqx/pull/9787) 修复 v5.0.12 之前创建的 `webhook` 桥接配置的兼容问题。

## 5.0.14

*发布日期：2023-01-11*

### 增强

- [#8329](https://github.com/emqx/emqx/pull/8329) MongoDB 库已升级，支持 MongoDB 5.1+。
- [#9593](https://github.com/emqx/emqx/pull/9593) 通过 API 查询桥接信息时，对敏感数据进行了脱敏处理。
- [#9614](https://github.com/emqx/emqx/pull/9614) 现在可以不加引号配置环境变量中的 host:port。之前，从环境变量覆盖 host:port 配置值时，必须加引号：`env EMQX_BRIDGES**MQTT**XYZ**SERVER='"localhost:1883"'`。现在可以不加引号设置：`env EMQX_BRIDGES**MQTT**XYZ**SERVER='localhost:1883'`。
- [#9642](https://github.com/emqx/emqx/pull/9642) 废弃桥接/资源的 enable_batch 和 enable_queue 选项。此更改后，桥接的队列始终启用，批处理由 batch_size 选项控制：batch_size > 1 表示将启用批处理。
- [#9671](https://github.com/emqx/emqx/pull/9671) 实现滑动窗口平均指标。
- [#9674](https://github.com/emqx/emqx/pull/9674) 使规则引擎行为与桥接行为在指标方面更一致：如果规则引擎被禁用，其指标现在将被重置。
- [#9675](https://github.com/emqx/emqx/pull/9675) HTTP 客户端库 ehttpc 从 0.4.2 升级到 0.4.3。管理 Redis 集群客户端的库 eredis_cluster 从 0.7.1 升级到 0.7.5。
- [#9713](https://github.com/emqx/emqx/pull/9713) 引入 api_key.bootstrap_file 以在启动时初始化 API 密钥。废弃 dashboard.bootstrap_users_file。将 API 密钥的最大数量限制为 100 而不是 30。

### 修复

- [#8648](https://github.com/emqx/emqx/pull/8648) 删除不存在的桥接时，服务器返回成功响应。现已修复，当用户尝试删除不存在的桥接时，服务器会给出错误响应。
- [#9637](https://github.com/emqx/emqx/pull/9637) 修复 clients HTTP API 的 expiry_interval 字段以秒为单位。
- [#9638](https://github.com/emqx/emqx/pull/9638) 修复 MySQL 驱动断开连接时的数据丢失和 bad match 问题。
- [#9641](https://github.com/emqx/emqx/pull/9641) 修复测试 GCP PubSub 可能导致内存泄露和 JWT 令牌第二次刷新失败的问题。
- [#9642](https://github.com/emqx/emqx/pull/9642) 修复可能导致错误桥接指标的一些问题。修复当 Kafka 或其连接断开时，可能导致消息丢失和错误指标的 Kafka 生产者桥接问题。修复使用批处理和批处理重试时，相同消息可能被多次交付的一些问题。
- [#9667](https://github.com/emqx/emqx/pull/9667) 移除 /publish 和 /publish/bulk HTTP API 设置 clientid 的可能性，以减少安全混淆的风险。
- [#9687](https://github.com/emqx/emqx/pull/9687) 修复因错误处理未配置 local_topic 字段的某些数据桥接而导致向数据桥接发送消息失败的问题。
- [#9689](https://github.com/emqx/emqx/pull/9689) 修复 HTTP 授权结果处理问题，当请求失败（例如：HTTP 资源宕机）可能导致 function_clause 错误。
- [#9703](https://github.com/emqx/emqx/pull/9703) 将 HTTP API /clients/:clientid/subscribe 的 qos 字段的默认值设置为 0。
- [#9705](https://github.com/emqx/emqx/pull/9705) 移除 Webhook 的默认值。
- [#9712](https://github.com/emqx/emqx/pull/9712) 修复从插件和数据桥处理 'client.connected' 事件时调用 HTTP API '/clients/:clientid/subscribe/bulk' 导致的 '404 Not Found' 问题。
- [#9714](https://github.com/emqx/emqx/pull/9714) 修复 /mqtt/auto_subscribe API 的错误 Swagger 架构，并确保 Swagger 始终检查架构是否正确。
- [#9716](https://github.com/emqx/emqx/pull/9716) MQTT 桥接配置兼容性修复。在 v5.0.12 之前创建的配置在升级到 v5.0.13 后可能遇到兼容性问题。
- [#9717](https://github.com/emqx/emqx/pull/9717) 在此修复之前，如果尝试连接桥接服务器总是超时，即使桥接被禁用也无法更改其他配置。

## 5.0.13

*发布日期：2022-12-27*

### 增强

- 添加 `limiter` 更新 API [#9133](https://github.com/emqx/emqx/pull/9133)。
- 在集群启动时同步数据目录时避免创建临时 zip 文件 [#9429](https://github.com/emqx/emqx/pull/9429)。
- 重构：将 `/mqtt/sys_topics` 移至通用 `/configs/sys_topics` [#9511](https://github.com/emqx/emqx/pull/9511)。
- 重构：对 `/users/{name}/change_pwd` 使用 `POST` 而不是 `PUT` [#9533](https://github.com/emqx/emqx/pull/9533)。
- 在规则引擎中添加压缩函数 `zip`、`gzip`、`zip_compress` 及相应的解压缩函数 [#9573](https://github.com/emqx/emqx/pull/9573)。
- 对 `PUT /authenticator/:id` 返回 `204` 而不是 `200` [#9434](https://github.com/emqx/emqx/pull/9434/)。
- 添加选项以自定义出站 MQTT 桥接的 clientid 前缀。[#9609](https://github.com/emqx/emqx/pull/9609)
- 确保 `banned` 的默认过期时间足够大 [#9599](https://github.com/emqx/emqx/pull/9599/)。

### 修复

- 当 QoS2 消息由客户端使用相同的包 ID 重新发送，或 `awaiting_rel` 队列已满时，触发 `message.dropped` 钩子 [#9487](https://github.com/emqx/emqx/pull/9487)。
- 修复共享订阅 'sticky' 策略 [#9578](https://github.com/emqx/emqx/pull/9578)。在此更改之前，'sticky' 订阅者在取消订阅后可能继续接收消息。
- 添加检查以确保给定的键在 mysql 连接器的查询中属于准备好的语句之一 [#9571](https://github.com/emqx/emqx/pull/9571)。
- 修复连接器中密码泄露到日志的问题 [#9608](https://github.com/emqx/emqx/pull/9608)。

## 5.0.12

*发布日期：2022-12-14*

### 亮点

- 此版本包括 MQTT 桥接配置的重构。从 v5.0.11 或更早版本创建的旧版本配置文件将根据新架构进行转换。请注意，配置 MQTT 桥接的 `/bridges` API 的请求体已以不兼容的方式更改。
- 开始为 Apple M1/M2 (MacOS-12) 发布软件包。
- 开始为 Amazon Linux 2 发布软件包（例如 emqx-5.0.12-amzn2-amd64.rpm）。
- 保留消息索引性能改进。

### 增强

- 通过 `node.global_gc_interval = disabled` 禁用全局垃圾回收 [#9418](https://github.com/emqx/emqx/pull/9418)。
- 改进 CLI 以避免在输入错误时浪费原子表 [#9416](https://github.com/emqx/emqx/pull/9416)。
- 开始为 Apple Silicon 硬件构建 MacOS 软件包 [#9423](https://github.com/emqx/emqx/pull/9423)。
- 移除使用非标准 `$queue` 功能设置共享订阅的支持 [#9412](https://github.com/emqx/emqx/pull/9412)。共享订阅现已成为 MQTT 规范的一部分。请改用 `$share`。
- 通过替换 `POST /authentication/{id}/move` 为 `PUT /authentication/{id}/position/{position}` 重构 authn API [#9419](https://github.com/emqx/emqx/pull/9419)。对 `/listeners/{listener_id}/authentication/id/...` 也做了相同处理。
- 重新设计 `/rules` API，使 `metrics` 成为专门的资源而不是包含在每个响应中 [#9461](https://github.com/emqx/emqx/pull/9461)。
- 增加更多 PSK 密码支持 [#9505](https://github.com/emqx/emqx/pull/9505)。
- 提高 `emqx_retainer` 写入性能：在写入时摆脱事务 [#9372](https://github.com/emqx/emqx/pull/9372)。
- HTTP 客户端库 `ehttpc` 从 `0.4.0` 升级到 `0.4.2` [#9520](https://github.com/emqx/emqx/pull/9520)。
- 为 MQTT SSL 监听器添加 `handshake_timeout` 选项 [#9502](https://github.com/emqx/emqx/pull/9502)。
- 将 Dashboard升级到 [v1.1.3](https://github.com/emqx/emqx-dashboard-web-new/releases/tag/v1.1.3)。
- 用户可以在 EMQX Helm 图表中定义服务的 `externalTrafficPolicy` [#9527](https://github.com/emqx/emqx/pull/9527)。
- 对 `POST /gateway/lwm2m/clients/{clientid}/{read,write,observe}` 返回 `204` 而不是 `200` [#9480](https://github.com/emqx/emqx/pull/9480)。
- 使完全从环境变量创建认证成为可能 [#9547](https://github.com/emqx/emqx/pull/9547)。例如，现在可以启用 MySQL 认证： `env EMQX_AUTHENTICATION__1='{mechanism="password_based",backend="mysql",server="localhost:3306",database="emqx",username="emqx",password="******",query="SELECT password_hash,salt FROM mqtt_user WHERE username=${username} LIMIT 1",enable=true}'`。
- 开始为 Amazon Linux 2 构建软件包 [#9537](https://github.com/emqx/emqx/pull/9537)。

### 修复

- 修复 ExHook 配置更新后过时的 SSL 文件未被删除的问题 [#9432](https://github.com/emqx/emqx/pull/9432)。
- 修复 `/trace` API 的文档和架构 [#9468](https://github.com/emqx/emqx/pull/9468)。
- 如果 `/telemetry/data` 被禁用，则返回 `404` [#9464](https://github.com/emqx/emqx/pull/9464)。
- 修复一些潜在的 MQTT 数据包解析错误 [#9477](https://github.com/emqx/emqx/pull/9477)。
- 修复 EMQX Helm 图表部署错误 [#9509](https://github.com/emqx/emqx/pull/9509)。
- 修复 `emqx_authenticator` 遮蔽 `'client.authenticate'` 回调的问题。现在，如果没有任何认证器匹配，`emqx_authenticator` 会将执行传递给更多的回调 [#9496](https://github.com/emqx/emqx/pull/9496)。
- 如果 `/trace/:id/download?node={node}` 中的查询参数 `node` 不是已知节点，则返回 `400` [#9478](https://github.com/emqx/emqx/pull/9478)。
- 如果出现重复，则 `POST /traces` 返回 `409` [#9494](https://github.com/emqx/emqx/pull/9494)。
- 修复桥接功能，当同时配置入站和出站桥接时，出站桥接不工作 [#9523](https://github.com/emqx/emqx/pull/9523)。
- 修复 EMQX Helm 图表在提供自定义凭据时使用错误的密钥值问题 [#9536](https://github.com/emqx/emqx/pull/9536)。

## 5.0.11

*发布日期：2022-11-27*

### 增强

- 保留消息的安全增强 [#9326](https://github.com/emqx/emqx/pull/9326)。 如果发布者客户端被禁止，则不会发布保留消息。
- `subscribe` API 的安全增强 [#9355](https://github.com/emqx/emqx/pull/9355)。
- 增强 `banned` 功能 [#9367](https://github.com/emqx/emqx/pull/9367)。 现在，当客户端通过 `clientid` 被禁止时，相应的会话将被踢出。
- 重新设计 `/gateways` API [9364](https://github.com/emqx/emqx/pull/9364)。 使用 `PUT /gateways/{name}` 替代 `POST /gateways`，如果需要，网关将自动加载。使用 `PUT /gateways/{name}/enable/{true|false}` 来启用或禁用网关。不再使用 `DELETE /gateways/{name}`。
- 支持 `statsd {tags: {"user-defined-tag" = "tag-value"}` 配置并提高 `emqx_statsd` 的稳定性 [#9363](http://github.com/emqx/emqx/pull/9363)。
- 改进节点名称生成规则，以避免潜在的原子表溢出风险 [#9387](https://github.com/emqx/emqx/pull/9387)。
- 将主题的最大级别的默认值设置为 128 [#9406](https://github.com/emqx/emqx/pull/9406)。
- 保持 MQTT v5 用户属性对从桥接接收的 MQTT 消息到桥接目标的转发 [#9398](https://github.com/emqx/emqx/pull/9398)。

### 修复

- 修复 helm 图表的 `ssl.existingName` 选项不工作的问题 [#9307](https://github.com/emqx/emqx/issues/9307)。
- 修复有时由于 `end_at` 时间已过而创建跟踪失败的问题。[#9303](https://github.com/emqx/emqx/pull/9303)
- 对 `/authenticator/{id}/status` 中未知认证器的状态返回 404 [#9328](https://github.com/emqx/emqx/pull/9328)。
- 修复仅当设置了 `exp` 声明时，JWT ACL 规则才会应用的问题 [#9368](https://github.com/emqx/emqx/pull/9368)。
- 修复 `/configs/global_zone` API 无法获取配置默认值的问题 [#9392](https://github.com/emqx/emqx/pull/9392)。
- 修复遗嘱消息的挂载点不工作问题 [#9399](https://github.com/emqx/emqx/pull/9399)。

## 5.0.10

*发布日期：2022-11-09*

由于 GitHub Action 发布包上传失败的问题，需要重新创建发布版本。 之前的提交：34a6c6c88

### 增强

- 提高 `/nodes` API 的响应性能 [#9221](https://github.com/emqx/emqx/pull/9221)。
- 改进 `banned` 和 `delayed` 功能的集成 [#9326](https://github.com/emqx/emqx/pull/9326)。 现在在发布延迟消息之前会先检查其来源客户端是否被禁止，如果是，则忽略此发布操作。
- 将 `gen_rpc` 库更新至 3.0 版本 [#9187](https://github.com/emqx/emqx/pull/9187)。
- 在引导副本节点时改善核心节点的内存使用 [#9236](https://github.com/emqx/emqx/pull/9236)。
- 提高 Prometheus Push Gateway 的稳定性，并在 POST 失败时记录错误 [#9235](http://github.com/emqx/emqx/pull/9235)。
- 现在可以选择不在 prometheus 统计中包含 VM 内部指标 [#9222](https://github.com/emqx/emqx/pull/9222)。 当系统负载较高时，报告过多的指标数据可能会导致 prometheus 统计 API 超时。
- 提高将 `binary`、`lists` 等类型转换为 `atom` 类型的安全性 [#9279](https://github.com/emqx/emqx/pull/9279), [#9286](https://github.com/emqx/emqx/pull/9286)。
- 添加 `/trace/:name/log_detail` HTTP API 以返回跟踪文件的大小和修改时间 [#9152](https://github.com/emqx/emqx/pull/9152)。
- 将 `/status` HTTP API 端点添加到 API 文档中 [#9230](https://github.com/emqx/emqx/pull/9230)。
- 所有平台的二进制包现在基于 Erlang/OTP 版本 24.3.4.2 构建 [#9293](https://github.com/emqx/emqx/pull/9293)。

### 修复

- 修复认证配置中缺少 `mechanism` 时的错误日志消息 [#8924](https://github.com/emqx/emqx/pull/8924)。
- 修复在 `/gateway` API 调用中使用未知的 `status` 参数时的 HTTP 500 问题 [#9225](https://github.com/emqx/emqx/pull/9225)。
- 修复 `/status` 端点的 HTTP 响应状态码 [#9211](https://github.com/emqx/emqx/pull/9211)。 在修复之前，即使 EMQX 应用程序没有运行，它始终返回 `200`。现在，在这种情况下会返回 `503`。
- 修复消息传递相关事件编码 [#9228](https://github.com/emqx/emqx/pull/9228)。 这个 bug 在 v5.0.9 中引入。对于规则引擎的输入事件，如 `$events/message_delivered` 和 `$events/message_dropped`，如果消息被传递给共享订阅，事件的编码（转为 JSON）将失败。
- 修复 `/gateways` API 的错误 HTTP 响应状态码，当网关名称未知时，应返回 `404` 而非 `400` [#9268](https://github.com/emqx/emqx/pull/9268)。
- 修复延迟消息的主题授权检查错误 [#9290](https://github.com/emqx/emqx/pull/9290)。 现在将确定延迟消息的实际主题，例如 `$delayed/1/t/foo` 将在授权检查中被视为 `t/foo`。
- 为 `/authentication/sources/:type` 的错误响应添加 `code` 属性 [9299](https://github.com/emqx/emqx/pull/9299)。
- 使 `/authentication/sources` 的文档与我们实际发送的内容保持一致 [9299](https://github.com/emqx/emqx/pull/9299)。
- 修复 `/configs` 资源中查询字符串参数 'node' 被忽略的问题，如果节点不存在则返回 404 [#9310](https://github.com/emqx/emqx/pull/9310/)。
- 避免在会话被踢出或被接管（到新会话）时重新分派共享订阅会话消息 [#9123](https://github.com/emqx/emqx/pull/9123)。

## 5.0.9

*发布日期：2022-10-24*

### 增强

- 为 authz_http 和 authz_mongo 添加 `cert_common_name` 和 `cert_subject` 占位符支持 [#8973](https://github.com/emqx/emqx/pull/8973)。
- 在 emqx_delayed 内部使用毫秒存储发布时间，提高精度 [#9060](https://github.com/emqx/emqx/pull/9060)。
- 更严格的拍动检查以提高系统稳定性 [#9136](https://github.com/emqx/emqx/pull/9136)。
- 消息发布 API 不再回显消息 [#9155](https://github.com/emqx/emqx/pull/9155)。 在此修复之前，消息发布 API（`api/v5/publish` 和 `api/v5/publish/bulk`）会将消息回显给客户端的 HTTP 正文中。 此更改修复它，只发送回消息 ID。

### 修复

- 在发布遗嘱消息之前检查 ACL [#8930](https://github.com/emqx/emqx/pull/8930)。
- 修复某些节点（在集群中）仍在加载配置时 GET /listeners API 崩溃的问题 [#9002](https://github.com/emqx/emqx/pull/9002)。
- 修复认证和授权中的空变量插值问题 [#8963](https://github.com/emqx/emqx/pull/8963)。 未定义变量的占位符现在呈现为空字符串，不再引起错误。
- 修复慢订阅统计的延迟统计错误 [#8986](https://github.com/emqx/emqx/pull/8986)。 在此更改之前，当 `stats_type` 是 `internal` 或 `response` 时，开始时间戳的精度错误。
- 修复共享订阅消息重新分发 [#9104](https://github.com/emqx/emqx/pull/9104)。
  - 丢弃 QoS 2 inflight 消息时，日志过多
  - 对于通配符交付，重新分发使用了错误的主题（发布主题，而不是订阅主题），导致分发时消息丢失。
- 将 HTTP 客户端 `gun` 从 1.3.7 升级到 [1.3.9](https://github.com/emqx/gun/tree/1.3.9) 在此修复之前，用于 HTTP 认证或 webhook 集成的长期 HTTPS 连接可能无限期停滞，导致 HTTP 请求大量超时。

## 5.0.8

*发布日期：2022-09-17*

### 值得一提的变化

### 增强

- 使用独立的 RPC 实现在节点之间转发共享订阅消息，而不是使用 Erlang 自己的 RPC，以减少在共享订阅负载高时的集群压力。[#8893](https://github.com/emqx/emqx/pull/8893)
- 当使用默认（不安全）的 Erlang cookie 启动时，打印警告消息。[#8905](https://github.com/emqx/emqx/pull/8905)
- `local-override.conf` 中的配置将不允许在运行时对整个集群进行同步更新。[#8851](https://github.com/emqx/emqx/pull/8851)
- 添加 `POST /listeners` 接口用于创建监听器。[#8876](https://github.com/emqx/emqx/pull/8876)
- 将 `/gateway` API 路径改为复数形式。[#8823](https://github.com/emqx/emqx/pull/8823)
- JWT 认证中的 `exp`、`nbf` 和 `iat` 声明支持非整数时间戳。[#8867](https://github.com/emqx/emqx/pull/8867)
- 提高 ExProto 和 gRPC 服务器的请求性能。[#8866](https://github.com/emqx/emqx/pull/8866)

### 修复

- 修复使用 Redis 作为数据源直接进行密码认证时，如果没有检索到认证数据就终止认证的问题。[#8934](https://github.com/emqx/emqx/pull/8934)
- 由于操作系统时间变化导致延迟发布不准确的问题。[#8926](https://github.com/emqx/emqx/pull/8926)
- 修复禁用保留消息功能后无法启动 EMQX 的问题。[#8911](https://github.com/emqx/emqx/pull/8911)
- 修复集群中一个节点宕机时更新配置的响应速度慢的问题。[#8857](https://github.com/emqx/emqx/pull/8857)
- 修复当没有匹配规则时，授权会终止执行 `client.authorize` 钩子的问题。[#8780](https://github.com/emqx/emqx/pull/8780)
- 修复 MQTT 桥接必须配置 Payload 的问题。[#8949](https://github.com/emqx/emqx/pull/8949)
- 修复日志目录无法通过环境变量配置的问题。[#8892](https://github.com/emqx/emqx/pull/8892)
- 修复 CoAP 网关在解析主题时引入额外的 `/` 前缀的问题。[#8658](https://github.com/emqx/emqx/pull/8658)
- 修复 MQTT 桥接在 API 响应中返回 TLS 文件内容的问题。[#8872](https://github.com/emqx/emqx/pull/8872), [#8958](https://github.com/emqx/emqx/pull/8958)
- 修复客户端认证失败会触发遗嘱消息释放的问题。[#8887](https://github.com/emqx/emqx/pull/8887)
- 修复 ExProto 不完善的保活检查机制可能导致客户端永不过期的问题。[#8866](https://github.com/emqx/emqx/pull/8866)

### 依赖升级

- `grpc-erl` 从 `0.6.6` 升级到 `0.6.7`，[#8866](https://github.com/emqx/emqx/pull/8866)

## 5.0.7

*发布日期: 2022-09-01*

### 增强

- 简化 TLS 密码套件配置
- 在 Dashboard 上关闭监听器之前添加确认提示
- 在 Dashboard 上统一 TLS 配置
- 支持在 Dashboard 概览页面查看 EMQX 版本和节点角色
- 增加对插件文件类型的限制

### Bug 修复

- 修复了当混合版本的节点形成集群时，复制节点的 Mria 事务无法执行的问题
- 修复了 Dashboard 的授权设置页面无法显示数据的问题
- 修复了在 Dashboard 上无法重置监控主题数据的问题

### 其他变更

- 在客户端查询接口的响应中移除了与遗嘱消息相关的字段

## 5.0.6

*发布日期: 2022-08-22*

### Bug 修复

- 修复了 Dashboard 上节点状态显示不正确的问题

## 5.0.4

*发布日期：2022-07-28*

### 增强

- 数据集成中的规则支持分页和搜索。请注意，更新后 `GET /rules` API 将返回分页元信息，即 `{"data": [RuleObj1, RuleObj2], "meta": {"count": 2, "limit": 100, "page": 1}}`。[#8472](https://github.com/emqx/emqx/pull/8472)
- 改进数据集成中 WebHook 的健康检查，如果启用了 TLS，现在会检查 TLS 握手是否成功。[#8443](https://github.com/emqx/emqx/pull/8443)
- 当 RocksDB 不可用时，回退使用 Mnesia 持久化会话。[#8528](https://github.com/emqx/emqx/pull/8528)
- 支持通过 HTTP API 在运行时更新警报的阈值。[#8532](https://github.com/emqx/emqx/pull/8532)
- 日志跟踪将显示详细的认证过程。[#8554](https://github.com/emqx/emqx/pull/8554)
- 支持监听 IPv6 地址，例如：`[::1]:1883` 或 `::1:1883`。[#8547](https://github.com/emqx/emqx/pull/8547)
- 更新监听器 API 的请求和响应格式，以与其他 API 的行为保持一致。这将引入一些不兼容的更新，详见 [#8571](https://github.com/emqx/emqx/pull/8571)。
-  Dashboard将提示更改默认密码。
- 优化数据集成的规则和数据桥的 Dashboard概览页面。
- 在 Dashboard上添加数据集成规则的结果统计。
- 优化 Dashboard首页的图表。
- 在 Dashboard的订阅列表中添加 MQTT 5.0 订阅选项显示。

### 修复

- 修复了日志类型格式设置为 `json` 时，单个日志的最大长度配置无效的问题。[#8518](http://github.com/emqx/emqx/pull/8518)
- 修复了当 EMQX 安装目录路径包含空格时，数据集成中的 `jq` 无法使用的问题。[#8455](https://github.com/emqx/emqx/pull/8455)
- 修复了超级用户不生效的问题。[#8452](https://github.com/emqx/emqx/pull/8452)
- 将系统主题格式的统计与指标对齐。[#8464](https://github.com/emqx/emqx/pull/8464)
- 修复了数据集成中规则创建时间不持久化的问题，导致每次都更新为 EMQX 启动时间。[#8443](https://github.com/emqx/emqx/pull/8443)
- 修复了通过 HTTP API 更新配置时出现错误，导致 `cluster-override.conf` 文件被清空，所有修改的配置都丢失的问题。[#8443](https://github.com/emqx/emqx/pull/8443)
- 修复了在认证和授权中使用 Redis 哨兵模式时，Sentinel 字段未被要求设置的问题。[#8458](https://github.com/emqx/emqx/pull/8458)
- 修复了 OpenAPI 文档中的格式错误。[#8517](https://github.com/emqx/emqx/pull/8517)
- 修复了多语言钩子扩展可能不按客户端事件触发的顺序分发的问题。[#8530](https://github.com/emqx/emqx/pull/8530)
- 修复了认证占位符 `cert_subject` 和 `cert_common_name` 不可用的问题。[#8531](https://github.com/emqx/emqx/pull/8531)
- 修复了 WebHook 中 TCP 连接进程泄漏的问题。[ehttpc#34](https://github.com/emqx/ehttpc/pull/34)，[#8580](https://github.com/emqx/emqx/pull/8580)
- 修复了 CLI 不打印仅监听端口的监听器的问题。[#8547](https://github.com/emqx/emqx/pull/8547)
- 修复了 JWKS 认证中 TLS 字段检查不正确的问题。[#8458](https://github.com/emqx/emqx/pull/8458)
- 修复了监听器 API 未在集群中所有节点上返回连接信息的问题。[#8538](https://github.com/emqx/emqx/pull/8538)
- 修复了副本节点可能未接收到 Mnesia 事件的问题，导致配置更新等操作失败。[#8502](https://github.com/emqx/emqx/pull/8502)

## 5.0.3

*发布日期：2022-07-07*

### 修复

- Websocket 监听器未能读取头部 `X-Forwarded-For` 和 `X-Forwarded-Port` [8415](https://github.com/emqx/emqx/pull/8415)
- 从 MQTT 桥接配置文档中删除了 `cluster_singleton`。在 5.0 中，此配置不再适用 [8407](https://github.com/emqx/emqx/pull/8407)
- 修复 `emqx/emqx:latest` Docker 镜像发布使用 Erlang 版本，而不是 Elixir 版本 [8414](https://github.com/emqx/emqx/pull/8414)
- 将 JWT 认证中的 `exp` 字段更改为可选而不是必需，以修复与 4.X 版本的向后兼容问题。[8425](https://github.com/emqx/emqx/pull/8425)

### 增强

- 提高了 Dashboard HTTP API 路由规则生成的速度，有时会导致超时 [8438](https://github.com/emqx/emqx/pull/8438)

## 5.0.2

*发布日期：2022-07-02*

公告：EMQX 团队已决定停止支持开源版的 relup 功能。今后，这将是一个仅限企业版的功能。

主要原因：relup 需要从所有以前的版本仔细编写升级说明。

例如，4.3现在是4.3.16，我们有`4.3.0->4.3.16`、`4.3.1->4.3.16`，...总共16条这样的升级路径需要维护。这一直是 EMQX 团队在快速交付增强和修复方面的最大障碍。

### 修复

- 修复了`bin/emqx`中的一个打字错误，该错误影响了尝试在 macOS 发布版上启用 TLS 上的 Erlang 分布 [8398](https://github.com/emqx/emqx/pull/8398)

## 5.0.1

*发布日期：2022-07-01*

### 增强

- 移除了 Prometheus 抓取端点 /api/v5/prometheus/stats 的管理 API 认证 [8299](https://github.com/emqx/emqx/pull/8299)
- 为 exhook (gRPC) 连接添加了更多 TCP 选项。[8317](https://github.com/emqx/emqx/pull/8317)
- 用于认证和授权的 HTTP 服务器现在将通过响应体指示结果。[8374](https://github.com/emqx/emqx/pull/8374) [8377](https://github.com/emqx/emqx/pull/8377)
- 批量订阅/取消订阅 API [8356](https://github.com/emqx/emqx/pull/8356)
- 添加了独占订阅功能 [8315](https://github.com/emqx/emqx/pull/8315)
- 提供认证计数器指标 [8352](https://github.com/emqx/emqx/pull/8352) [8375](https://github.com/emqx/emqx/pull/8375)
- 不允许管理员用户自我删除 [8286](https://github.com/emqx/emqx/pull/8286)
- 重启后，确保从具有最大 `tnxid` 的集群节点复制 `cluster-override.conf`。[8333](https://github.com/emqx/emqx/pull/8333)

### 修复

- 从 4.x 移植的一个错误修复：允许从 `client.subscribe` 钩子点回调结果中删除订阅。[8304](https://github.com/emqx/emqx/pull/8304) [8347](https://github.com/emqx/emqx/pull/8377)
- 修复了 TLS 上的 Erlang 分布问题 [8309](https://github.com/emqx/emqx/pull/8309)
- 可以从环境变量覆盖认证配置 [8323](https://github.com/emqx/emqx/pull/8309)
- 使 Mnesia 数据库中的认证密码向后兼容到 4.x，以便我们可以更好地支持数据迁移。[8351](https://github.com/emqx/emqx/pull/8351)
- 修复了 rpm/deb 安装的插件上传问题 [8379](https://github.com/emqx/emqx/pull/8379)
- 在新节点加入集群后，从集群节点同步 data/authz/acl.conf 和 data/certs [8369](https://github.com/emqx/emqx/pull/8369)
- 确保失败资源的自动重试 [8371](https://github.com/emqx/emqx/pull/8371)
- 修复了客户端使用低于 MQTT v5.0 协议版本访问时 `packets.connack.auth_error` 计数不准确的问题 [8178](https://github.com/emqx/emqx/pull/8178)

### 其他

- 目前速率限制器接口被隐藏，它将接受用户体验重设计。
- QUIC 库升级到 0.0.14。
- 现在默认包将不在包名中包含 otp 版本号。
- 在 `etc` 目录中重命名了配置示例文件名。

## 5.0.0

*发布日期：2022-06-17*

### 水平可扩展性

引入了对 Mnesia 数据库的扩展，命名为 `Mria`。

EMQX 节点可以像以前的版本一样继续形成集群（命名为 `core`）。

在版本5中，Mria 允许更多节点作为`副本`加入集群。

不同之处在于，副本节点异步应用路由表（和配置等）的更改，且副本节点可以在不影响数据一致性的情况下进行扩展或缩减。

这减少了形成全网状集群的网络开销。

在我们最近的测试中，我们设法实现了100万 MQTT 客户端连接到一个23节点的 Mria 集群。

与典型的版本4集群相比，集群大小增加了3倍，

与我们之前的[1000万成就](https://www.emqx.com/en/resources/emqx-v-4-3-0-ten-million-connections-performance-test-report)相比，容量增加了10倍。

### 可靠性

借助 Mria 集群拓扑，减少了由于网络分区导致的集群分裂视图的风险，并总体上提高了集群稳定性。

- 只有核心节点参与事务写入
- 数据完全复制到副本节点，因此它们可以从本地副本（即，从 RAM）读取
- 虽然核心节点是功能完备的 MQTT 代理，但也可以选择将它们专用作纯数据库节点

### 全新 Dashboard

版本5带来了 Dashboard 的全新设计。前端由`vue.js`驱动，后端使用 OpenAPI，提供了最易用的 MQTT 代理管理 UI。

与版本4相比的主要改进：

- 访问控制（认证和授权）管理
- 改进的规则引擎和动作管理UI
- 视觉辅助视图，显示数据流，例如，从 MQTT 主题到目标数据汇
- 在线配置更新
- 网关/扩展管理
- 更多诊断工具，如慢订阅和在线追踪

### 类型安全配置

从版本5开始，EMQX 将使用 [HOCON](https://lightbend.github.io/config/) 作为配置语法。

在顶部附加了一个架构，现在配置是类型检查的。

HOCON supports NGINX-like configuration layouts, and provides a native array syntax.

但不用担心，如果你喜欢像版本4那样保持一切扁平化，旧的 `cuttlefish` 语法仍然受支持。

EMQX 团队付出巨大努力提供合理的默认值，因此现在默认配置文件不包括注释的情况下少于100行。

并列提供了一个完整的示例配置（从架构生成），帮助我们快速找到示例进行复制。

### 可操作性

在版本5中，Dashboard 启用的管理 API 的接口描述遵循 OpenAPI 规范3.0。幕后，这些规范实际上是从配置架构生成的，这同样的架构用于检查文件。

现在我们有了一个单一的架构来源，用于保护配置文件和 HTTP 接口，并生成配置和 API 文档。

关于 Swagger 的另一好处是，它附带了 Swagger-UI，我们可以点击“尝试一下”按钮直接从网页浏览器测试 API，或复制粘贴 `curl` 命令示例到控制台进行测试。

与版本4相比，可操作性的另一个主要变化是更改会持久化回磁盘作为配置文件（使用 HOCON 语法）。

统一配置文件接口和 HTTP API 接口的直接好处是热配置。

热配置是 EMQX 企业版中的一个功能，它允许我们在运行时从 Dashboard 或通过API更改许多配置，而无需重启服务。

在企业版版本4中，热值存储在数据库中，而不是配置文件中，这对于希望从配置文件接口更改覆盖值的用户来说并不方便。

### 可观测性

 Dashboard 带来了更详细的监控指标。我们现在可以在 Dashboard 上查看长达7天的历史指标数据，并一键与 Prometheus 集成。添加日志跟踪和慢订阅诊断工具，有效提升了在排查和诊断异常客户端行为时的用户体验。

可观测性的另一个增强是引入了结构化日志，现在 EMQX 发出的大多数日志都有一个 `msg` 字段。这个字段的文本是下划线分隔的单词，不仅对用户搜索友好，也帮助日志索引工具索引日志。

### 数据集成（旧规则引擎）

正如你可能从 Dashboard 上注意到的，旧的'规则引擎'已被重命名为'数据集成'。

它包括两个主要功能：规则和数据桥。

规则是一种数据处理语言，使用 SQL 语法（另外还支持[`jq`语言](https://www.youtube.com/watch?v=_GwF8zvhNcQ)），帮助过滤和转换 IoT 消息。

数据桥提供了将数据摄入 EMQX 或导出 EMQX 外部的能力。

通过 Dashboard ，用户现在可以清楚地看到IoT数据是如何通过规则处理的，以及数据是如何从/到外部数据服务流动的。

另一个重要变化：现在规则和动作是配置（在配置文件中），而不是 Mnesia（内置数据库，对非 Erlang 开发人员来说相当不透明）中的数据，这意味着我们将能够使用各种部署/编排工具更容易地配置规则。

### 访问控制

现在我们可以从 Dashboard 管理认证和授权，而无需重启代理。

之前作为插件提供的认证和授权（ACL）配置分散在不同的文件中，更改通常会需要重启代理才能生效。 在版本5中，所有常用的安全配置都聚集在一起。 Dashboard 上还提供了一个用户管理 UI，允许您即时管理用户和访问规则。

我们甚至可以为每个监听器配置不同的认证规则。

### 网关

网关采用统一的设计语言重新实现，为不同的协议提供独立的管理接口和安全认证能力，具有不同客户端属性和生命周期的用户可以以更本地化的方式访问。由于每个网关都可以配置自己的独立认证，不同网关设备的认证信息现在可以彼此隔离，以满足更多场景的需求。

### MQTT over QUIC

QUIC [RFC 9000](https://datatracker.ietf.org/doc/html/rfc9000)，互联网的下一代传输层，为 IoT 带来了惊人的机遇（以及挑战）。

EMQX团队迫不及待地开始尝试在其上运行 MQTT。

随着5.0版本的发布，现在可以配置一个 QUIC 监听器来尝试 MQTT over QUIC。

### 扩展

EMQX 5.0保持并在一定程度上增强了插件扩展的能力。插件现在可以编译、作为独立包分发，然后上传到 Dashboard 完成整个集群的安装，因此它们不必为每个节点重复步骤。

### 你好，Elixir

EMQX 5仍然是一个 rebar3项目，但现在它与 mix 一起编译，如果你了解 Elixir，你就知道这意味着什么。快乐的黑客活动。
