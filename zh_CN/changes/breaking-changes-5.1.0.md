# 从 EMQX 4.4 到 EMQX 5.1 的不兼容变更

EMQX 5.0 系列引入了一些重大变更，可能会影响与旧版本 EMQX 的兼容性。

本章节旨在为计划从 EMQX 4.x 升级到 EMQX 5.1 版本的用户提供帮助，以明确并了解可能遇到的潜在问题，方便进行版本升级。

::: tip 提示

1. 建议升级到版本 5.1 之前，先升级到最新的 4.4 版本。
2. 如需升级到 5.0 系列更高版本，请先按照此文档完成 5.1 版本升级，之后您可以继续升级到更高版本。

:::

## 概述

与 EMQX 4.4 相比，升级到 EMQX 5.0 系列引入了重大变更，特别是在各种概念和机制方面，其程度超过了先前从 EMQX 2.x 到 EMQX 3.x 和从 EMQX 3.x 到 EMQX 4.x 的升级引起的变更。

总体来说有以下几点需要注意：

1. **配置和 HTTP API** 方面有重大变化。现有依赖这些接口的配置和代码需要进行迁移。
2. **核心 MQTT 协议功能**，包括发布/订阅、保留消息和共享订阅，与客户端程序完全兼容。但是管理接口可能会有轻微变化。
3. 其他与认证、授权、数据集成和协议访问相关的功能需要根据各自的功能进行迁移。
4. 某些概念发生了变化。例如，引入了新版本的**插件**，与旧版本有很大的差异。旧版本中的**模块**概念已经完全消除。
5. 注意**删除某些功能**，例如集群发现的 `mcast` 和数据集成的 `EMQX Bridges` 等。

## HTTP APIs

之前，API 认证凭证通过 **Dashboard** 中的**应用**来管理。现在，必须使用 **[API Key](../dashboard/system.md#api-key)** 来创建凭证。这些凭证由 API Key 和 Secret Key 组成，分别可以作为 HTTP 基本认证的用户名和密码来使用。创建凭证时，Secret Key 只显示一次，之后将无法再次获取。

- 端口 8081 已被关闭，所有 API 请求现在都使用端口 18083。
- 不能再使用用户名/密码来访问 HTTP API，必须使用 API Key。
- API 访问的基本路径从 `/api/v4` 切换到 `/api/v5`。通过端口 18083 和路径 `/api/v5` 调用 API。
- 与时间相关的字段将使用 [RFC3339](https://datatracker.ietf.org/doc/html/rfc3339) 格式与时区。

### 数据格式变化

当响应成功时，不再返回业务状态码 `code` 与数据，而是在发生错误时返回相应的 4xx/5xx HTTP 状态码和错误提示。 用户可以访问 `GET /error_codes` 来获取所有可能的错误代码。

::: details 响应格式对比示例:

**成功响应**

```shell
# 4.x
## HTTP StatusCode = 200
GET /api/v4/rules/my_rule
{ "code": 0, "data": { ... } }

# 5.1
## HTTP StatusCode = 200
GET /api/v5/rules/my_rule
{ ... }
```

**错误响应**

```bash
# 4.x
## HTTP StatusCode = 200
GET /api/v4/rules/my_rule
{ "code": 404, "message": "Not Found" }

# 5.1
## HTTP StatusCode = 404
GET /api/v5/rules/my_rule
{ "code": "NOT_FOUND", "message": "Rule Id Not Found" }
```

:::

### 主要 API 变化

API 经历了重大变化，其中一些 API 已经兼容。以下是常用 API 变化的比较表。

::: tip 兼容性说明

- 兼容：继续使用旧的 API 路径和参数，或保持旧的 API。
- 部分兼容：API 路径保持不变，但某些 API 字段已更改。
- 不兼容：API 路径和字段已更改。

:::

::: details API 兼容性对比表

| 4.x                             | 5.x                                         | Compatibility | 备注                       |
| ------------------------------- | ------------------------------------------- | ------------- | -------------------------- |
| **发布/订阅**                   |                                             |               |                            |
| `POST /mqtt/publish`            | `POST /publish`                             | 兼容          |                            |
| `POST /mqtt/publish_batch`      | `POST /publish/bulk`                        | 兼容          |                            |
| `POST /mqtt/subscribe`          | `POST /clients/{clientid}/subscribe`        | 兼容          |                            |
| `POST /mqtt/subscribe_batch`    | `POST /clients/{clientid}/subscribe/bulk`   | 兼容          |                            |
| `POST /mqtt/unsubscribe`        | `POST /clients/{clientid}/unsubscribe`      | 兼容          |                            |
| `POST /mqtt/unsubscribe_batch`  | `POST /clients/{clientid}/unsubscribe/bulk` | 兼容          |                            |
| **客户端/主题/订阅**            |                                             |               |                            |
| `GET /clients`                  | `GET /clients`                              | 部分兼容      |                            |
| `GET /routes{/topic}`           | `GET /topics{/topic}`                       | 不兼容        | `routes` 重命名为 `topics` |
| `GET /subscriptions`            | `GET /subscriptions`                        | 部分兼容      |                            |
| `GET /subscriptions/{clientid}` | `GET /clients/{clientid}/subscriptions`     | 不兼容        |                            |
| **节点/统计/指标**              |                                             |               |                            |
| `GET /nodes`                    | `GET /nodes`                                | 部分兼容      |                            |
| `GET /brokers`                  | -                                           | 不兼容        | 合并到 `GET /nodes`        |
| `GET /stats`                    | `GET /stats`                                | 部分兼容      |                            |
| `GET /metrics`                  | `GET /metrics`                              | 部分兼容      |                            |
| **用户/告警**                   |                                             |               |                            |
| `GET /users`                    | `GET /users`                                | 部分兼容      |                            |
| `GET /alarms{/activated}`       | `GET /alarms?activated={true,false}`        | 不兼容        |                            |
| `GET /alarms{/deactivated}`     | `GET /alarms?activated={true,false}`        | 不兼容        |                            |

:::

## 配置文件

- 格式:
  - EMQX 4.x: 扁平文本格式，使用 `path.to.key = value`。
  - EMQX 5.1: 支持嵌套格式，使用 `path{to{ key = value }}`。
- 来源:
  - EMQX 4.x:
    - 多个文件，例如 `emqx.conf`，`listeners.conf`，`zones.conf` 等。
    - 动态更新存储在 Mnesia 中。一旦启用，不能通过更新文件来进行配置更改。
  - EMQX 5.1:
    - 静态配置使用 `emqx.conf`。
    - 动态更新使用 `cluster.hocon`。

## 默认监听器

默认监听器的变更：

| 名称            | 描述                    | v4.4 端口 | 对应的 v5.x 端口               |
| --------------- | ----------------------- | --------- | ------------------------------ |
| MQTT-TCP        | 内部（后端）MQTT 监听器 | 11883     | -（已移除）                    |
| Management-HTTP | REST API（管理接口）    | 8081      | 18083（与 Dashboard 端口合并） |

## 插件

先前的官方插件已经迁移到 EMQX 作为内置功能。在 EMQX 5.x 中使用之前为版本 4.x 开发的自定义插件需要进行适应性调整。

::: details 官方插件与内置功能对比表格

| 4.x              | 5.x                                               |
| ---------------- | ------------------------------------------------- |
| emqx_auth_http   | AuthN/AuthZ - HTTP 数据源                         |
| emqx_auth_jwt    | AuthN/AuthZ - JWT                                 |
| emqx_auth_mnesia | AuthN/AuthZ - 内置数据库                          |
| emqx_auth_mongo  | AuthN/AuthZ - MongoDB 数据源                      |
| emqx_auth_mysql  | AuthN/AuthZ - MySQL 数据源                        |
| emqx_auth_pgsql  | AuthN/AuthZ - PostgreSQL 数据源                   |
| emqx_auth_redis  | AuthN/AuthZ - Redis 数据源                        |
| emqx_sasl        | AuthN/AuthZ - MQTT 5 增强认证                     |
| emqx_auth_ldap   | -                                                 |
| emqx_rule_engine | 数据集成                                          |
| emqx_bridge_mqtt | Sink - MQTT 桥接                              |
| emqx_web_hook    | Sink - HTTP 服务                              |
| emqx_coap        | CoAP 网关                                         |
| emqx_dashboard   | 仪表板                                            |
| emqx_exhook      | ExHook                                            |
| emqx_exproto     | ExProto 网关                                      |
| emqx_lwm2m       | LwM2M 网关                                        |
| emqx_sn          | MQTT-SN 网关                                      |
| emqx_stomp       | STOMP 网关                                        |
| emqx_lua_hook    | -                                                 |
| emqx_management  | Dashboard                                         |
| emqx_prometheus  | Prometheus 监控                                   |
| emqx_psk_file    | AuthN - PSK（`psk_authentication.enable = true`） |
| emqx_recon       | 旧特性在 CLI `emqx ctl observer` 中仍然可用       |
| emqx_retainer    | Retain                                            |
| <!--             | emqx_telemetry                                    |

:::

## 分布和集群

- 集群创建的 `mcast` 发现策略已经被弃用，并且正在等待删除。
- 服务发现的配置已经更改：`cluster.discovery` 改为 **cluster.discovery_strategy**。
- 新功能：[集群调用设置](https://docs.emqx.com/zh/enterprise/v5.0/configuration/configuration-manual.html#%E9%9B%86%E7%BE%A4%E8%B0%83%E7%94%A8%E8%AE%BE%E7%BD%AE)。
- 内部数据库中添加了可选的[最终一致性](https://docs.emqx.com/en/enterprise/v5.1/design/clustering.html#data-consistency)。

## MQTT

- 在 EMQX 5.0 中，MQTT 客户端不能再将 EMQX 集群视为单个黑盒，因为存在最终一致性。订阅者在订阅确认后可能收不到其他客户端发布的消息，也可能会收到。
- 在 EMQX 5.0 中，keepalive（接收 PING）需要一个完整的 MQTT 控制 Packet，而不是几个字节。
- 在 EMQX 5.0 中，TLS 监听器不支持 `partial_chain` 和 `verify_peer_ext_key_usage`。
- 重试间隔在 5.0 版本中是 30 秒，而在 4.4 版本中是禁用的（0）。版本 4.4 中的默认配置文件具有 30 秒的重试间隔。

## MQTT over QUIC

MQTT over QUIC 是 5.0 中的一个新功能，但默认情况下是禁用的。根据操作系统的不同，可能需要动态连接到 `libatomic`。

## 认证 / 授权

有关完整的兼容性报告，请参阅[认证/授权从 EMQX 4.4 到 EMQX 5.1 的不兼容变更](./auth-4.4-to-5.1-compatibility.md)。

所有认证器和授权检查器现在使用占位符而不是以前的格式。在 EMQX 5.x 中，使用占位符，如 `${clientid}`，而在 EMQX 4.x 中使用 `%c`。可用的占位符集也发生了变化。

### 概念的变更

Auth 被称为 **Authentication** and ACL 也被称为 **Authorization**.

### 数据迁移

我们保留了来自版本 4.x 的认证方法和支持的数据源，只对使用方式进行了一些更改。对于大多数认证器/授权检查器，可以在升级到版本 5.x 时继续使用 4.x 数据库，无需迁移现有数据。

### 固定执行顺序

当同时启用多个认证器或授权检查器时，不再根据启动顺序执行检查，而是根据固定的配置顺序进行检查。可以在配置文件和 Dashboard 中调整执行顺序。

### 变量插值语法

以前，Auth 插件可以使用 `%u` 语法来构造变量占位符，将客户端信息动态地插入到 SQL 语句、Redis 查询命令和 HTTP 请求中。现在 EMQX 使用新的 `${}` 语法，例如 `${username}`，`${clientid}`，与 SQL 的规则一致。

有关支持的占位符，请参考：

- [Authentication Placeholders](../access-control/authn/authn.md#authentication-placeholders)
- [Authorization Placeholders](../access-control/authz/authz.md#placeholders-in-data-queries)

::: details 使用示例

```shell
# 4.x
# etc/emqx_auth_mysql.conf
auth.mysql.auth_query = select password from mqtt_user where username = '%u' limit 1

# 5.x
# emqx.conf
authentication = [
  {
    ...
    mechanism = "password_based"
    backend = "mysql"
    query = "SELECT password_hash, salt FROM mqtt_user where username = ${username} LIMIT 1"
  }
]
```

:::

### 认证不兼容变更

- 移除了超级用户查询。应该返回具有散列凭据和 `is_superuser` 标志的单个查询。
- HTTP 认证
  - 在 EMQX 4.x 中，只使用 HTTP 状态码，而且 body 被丢弃（例如 `200` 表示 `allow`，`403`表示 `deny`）。
  - 在 EMQX 5.x 中，HTTP 认证经过重新设计，使用 HTTP body。更多详细信息请参阅 [HTTP 服务认证](../access-control/authn/http.md#http-request-and-response)。
- SCRAM 认证
  - 不再支持 SHA1 哈希模式（版本 4.4 中唯一支持的模式）。现在使用 SHA256/SHA512 哈希。
- 内置数据库
  - 不再可以在配置文件中直接提供凭证。
  - 凭证表现在只保持用户名或客户端 ID 类型的凭证，而不是同时保持两者。
- Redis
  - 只支持 `HMGET` 和 `HGET` 命令。
  - 不再使用 `query_timeout`。
- PostgreSQL
  - 不再使用 `query_timeout`。
  - 不再使用 `encoding`。

### 授权不兼容变更

- ACL 文件

  - ACL 规则 `{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"\]}` 在 EMQX 5.1 中不起作用。有关更多信息，请参阅问题[#10735](https://github.com/emqx/emqx/issues/10735)。

- HTTP

  - 在 EMQX 4.x 中，使用 HTTP 状态码，但 body 被丢弃（除了 “ignore” 案例）。例如，`200`表示 `allow`，`403`表示 `deny`。
  - 在 EMQX 5.0 中，HTTP 授权经过重新设计，使用 HTTP body。有关更多信息，请参阅 [HTTP 请求格式与返回结果](../access-control/authz/http.md#http-request-and-response)。

- MySQL, PostgreSQL

  - 存储模式已更改。
  - 在 EMQX 4.4 中，查询应该检索具有列 `[Allow, IpAddr, Username, ClientId, Access, Topic]` 的行，列名可以是任意名称，但必须按照此顺序排列。
  - 在 EMQX 5.1 中，查询应该检索具有列 `permission, action, topic` 的行，列名可以是任意顺序，但必须使用这些确切的列名。"who"部分（`IpAddr、Username、ClientId`）现在建议作为查询的一部分。

- MongoDB

  - 存储模式已更改。

  - 在 EMQX 4.4 中，生成的文档应该按照操作键列出主题列表，就像在 Redis 或 JWT 中一样：

    ```
    {
      "publish": ["t1", "t2"],
      "subscribe": ["t3", "t4"],
      "pubsub": ["t5", "t6"]
    }
    ```

  - 在 EMQX 5.1 版本中，文档应该包含具有 `permission, action, topics` 字段的个别规则。注意，`topics` 应该是一个主题数组。

## 规则引擎

Rule SQL 与 EMQX 4.x 语法完全兼容，但规则下的操作被分为内置操作（republish、console）和动作（Sink）两部分。

## 数据集成

在 EMQX 5.1 中，数据集成有以下这些概念上的改进：

- 规则和 SQL 模板完全兼容。
- 大多数资源和桥接的配置项名称和格式已更改。
- 之前的**规则** -> **操作** -> **资源**流程已改为**规则** -> **桥接**。
- **模块/消息发布**功能已移至桥接中。
- 移除了**[保存离线消息](https://docs.emqx.com/zh/enterprise/v4.4/rule/offline_msg_to_redis.html)**、**[获取订阅关系](https://docs.emqx.com/zh/enterprise/v4.4/rule/get_subs_from_redis.html)**和 EMQX Bridge 功能。
- 目前不支持 Tablestore、DolphinDB、Lindorm 和 SAP Event Mesh 动作。
- MQTT 桥接插件（`emqx_bridge_mqtt`）已移除。请使用数据集成中的内置 MQTT 动作。

有关完整的兼容性报告，请参阅[数据集成从 EMQX 5.1 到 EMQX 4.4 的不兼容变更](./data-integration-4.4-to-5.1-incompatibility.md)。

## HTTP 服务

WebHook 插件（`emqx_web_hook`）已转换为内置功能，现在称为 "HTTP 服务" 动作。

{% emqxee %}

## 离线消息

在 EMQX 4.x 中提供的[离线消息](https://docs.emqx.com/zh/enterprise/v4.4/rule/offline_msg_to_redis.html)基于外部数据库。EMQX 计划在以后的版本中提供基于内置数据库的本地离线消息支持，因此在版本 5.x 中不再支持外部数据库的离线消息。

即将推出的本地离线消息功能将提供更高的性能，并降低使用和维护成本。敬请关注更多更新。

## 自动订阅（服务器端订阅）

从版本 5.0.0 开始，EMQX 不再基于外部数据库提供[自动订阅](https://docs.emqx.com/zh/enterprise/v4.4/rule/get_subs_from_redis.html)（服务器端订阅）。

{% endemqxee %}

## 数据持久化

[MQTT 消息持久化](https://docs.emqx.com/zh/enterprise/v4.4/backend/backend.html#mqtt-message-persistence)在 EMQX 5.0 和 5.1 中未实现。计划在以后的版本中提供。

## 网关

在 EMQX 4.x 中，可以通过相应的插件和模块（仅适用于企业版）来配置各种协议。然而，在 EMQX 5.0 中，我们引入了 "网关" 这个新概念。

使用与 MQTT 协议不同的其他协议的客户端（如 LwM2M、CoAP、STOMP、MQTT-SN）不再显示在 Dashboard 的**连接**页面和 `GET /clients` API 中作为 MQTT 客户端，无法通过 Dashboard 客户端页面和 `GET /clients` API 获取。它们可以在**管理** -> **网关**中找到，或者通过 `GET /gateway/{name}/clients` API 列出。

- 从配置和管理方法的角度来看，这是完全不兼容的。EMQX 5.0 具有全新的配置格式和管理方法。
  - 新的配置格式。
  - 添加了新的 HTTP API 来管理网关和网关客户端。
  - 每个网关都有独立的身份验证方法。
- JT/T 808、GB/T 32960、TCP 和 OCPP 在 EMQX 5.1 中**不支持**。
- STOMP、MQTT-SN 和 ExProto 协议与 4.x 版本完全兼容，并在功能上有更多的改进。
- 尽管 CoAP 和 LwM2M 的网关在 5.1.0 版本中已实现，但由于设计和实现不完整，不建议在生产环境中使用。

有关完整的兼容性报告，请参阅[网关从 EMQX 4.4 到 EMQX 5.1 的不兼容变更](./gateway-4.4-to-5.1-incompatibility.md)。

## 日志文件格式

EMQX 5.1 中的日志文件可以是与 EMQX 4.4 中相同的平面日志文件格式，也可以是更适合索引的结构化 JSON 格式。

此外，现在大多数日志字段使用下划线作为单词分隔符，使其更易于搜索，例如：

`2022-06-29T16:58:53.235042+02:00 [info] foo: bar, msg: msg_for_human_to_read_but_also_easy_to_index`

更多详细信息，参阅[日志](../observability/log.md)。

## Prometheus

名为 `emqx_statsd` 的插件已被删除。名为 `emqx_prometheus` 的插件在 5.x 版本中已转换为内置功能。Prometheus 的抓取端点默认启用，无需身份验证即可抓取指标。

您可以使用 `curl` 命令检查指标：

```bash
curl -f "http://127.0.0.1:18083/api/v5/prometheus/stats"
```

如果要启用推送网关，请参考[集成 Prometheus](../observability/prometheus.md)。

::: details Prometheus 指标的变更

| 4.4.x                                        | 5.x                                             | Description |
| -------------------------------------------- | ----------------------------------------------- | ----------- |
| emqx_client_auth_success_anonymous           | emqx_client_auth_anonymous                      | 更名        |
| emqx_client_check_acl                        | emqx_client_authorize counter                   | 更名        |
| -                                            | emqx_mria_last_intercepted_trans                | 新增        |
| -                                            | emqx_mria_replicants                            | 新增        |
| -                                            | emqx_mria_server_mql                            | 新增        |
| -                                            | emqx_mria_weight                                | 新增        |
| emqx_routes_count                            | emqx_topics_count                               | 更名        |
| emqx_routes_max                              | emqx_topics_max                                 | 更名        |
| emqx_session_takeovered                      | emqx_session_takenover                          | 更名        |
| erlang_vm_ets_tables                         | -                                               | 移除        |
| -                                            | erlang_vm_memory_dets_tables                    | 新增        |
| -                                            | erlang_vm_memory_ets_tables                     | 新增        |
| -                                            | erlang_vm_msacc_alloc_seconds_total             | 新增        |
| -                                            | erlang_vm_msacc_aux_seconds_total               | 新增        |
| -                                            | erlang_vm_msacc_bif_seconds_total               | 新增        |
| -                                            | erlang_vm_msacc_busy_wait_seconds_total         | 新增        |
| -                                            | erlang_vm_msacc_check_io_seconds_total          | 新增        |
| -                                            | erlang_vm_msacc_emulator_seconds_total          | 新增        |
| -                                            | erlang_vm_msacc_ets_seconds_total               | 新增        |
| -                                            | erlang_vm_msacc_gc_full_seconds_total           | 新增        |
| -                                            | erlang_vm_msacc_gc_seconds_total                | 新增        |
| -                                            | erlang_vm_msacc_nif_seconds_total               | 新增        |
| -                                            | erlang_vm_msacc_other_seconds_total             | 新增        |
| -                                            | erlang_vm_msacc_port_seconds_total              | 新增        |
| -                                            | erlang_vm_msacc_send_seconds_total              | 新增        |
| -                                            | erlang_vm_msacc_sleep_seconds_total             | 新增        |
| -                                            | erlang_vm_msacc_timers_seconds_total            | 新增        |
| -                                            | erlang_vm_statistics_dirty_cpu_run_queue_length | 新增        |
| -                                            | erlang_vm_statistics_dirty_io_run_queue_length  | 新增        |
| erlang_vm_statistics_run_queues_length_total | erlang_vm_statistics_run_queues_length          | 更名        |
| -                                            | erlang_vm_wordsize_bytes                        | 新增        |

:::
