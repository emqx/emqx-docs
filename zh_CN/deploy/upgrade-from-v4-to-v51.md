# 从 v4 迁移到 v5.1.0

本章节提供了从 EMQX 4.x 版本迁移至 EMQX v5.1.0 版本的指南，尽管 EMQX 5.1.0 不保证向后兼容（Backward Compatibility），但绝大部分功能运行机制没有太大的变化，你仍然可以通过手动迁移的方式来实现升级。

<!-- 要了解我们在 EMQX 5.1 中增加的新功能，请查看 [全新功能](../getting-started/new-features.md)。 -->

## 日志

与 4.x 相比，日志中最重要的变化是格式。在 4.x 中，日志是扁平化格式的以便人类阅读。从 5.1 开始，我们在不影响可读性的前提下，转向使用结构化日志。
例如，多数日志字段使用下划线作为单词分隔符，使其更容易被检索，这种格式也有助于日志工具更有效地进行索引。

```bash
2022-06-29T16:58:53.235042+02:00 [info] foo: bar, msg: msg_for_human_to_read_but_also_easy_to_index
```

详细的格式请参照[日志 - 日志格式](../observability/log.md#日志格式)。

## 默认监听器

默认将不再提供以下监听器：

| 监听器               | 说明                               |
| -------------------- | ---------------------------------- |
| MQTT-TCP 11883       | 后端应用使用的 MQTT 接入点         |
| Management-HTTP 8081 | REST API 端口，已合并至 18083 端口 |

## 插件

此前的官方插件已迁移到 EMQX 中成为内置功能，用户在 4.x 版本上开发的插件需要重新适配。以下是官方插件与现有功能对照表：

| 4.x              | 5.1                                           |
| ---------------- | --------------------------------------------- |
| emqx_auth_http   | 认证/授权 - HTTP 数据源                       |
| emqx_auth_jwt    | 认证 - JWT                                    |
| emqx_auth_mnesia | 认证/授权 - 内置数据库                        |
| emqx_auth_mongo  | 认证/授权 - MongoDB 数据源                    |
| emqx_auth_mysql  | 认证/授权 - MySQL 数据源                      |
| emqx_auth_pgsql  | 认证/授权 - PostgreSQL 数据源                 |
| emqx_auth_redis  | 认证/授权 - Redis 数据源                      |
| emqx_sasl        | 认证/授权 - 增强认证                          |
| emqx_auth_ldap   | -                                             |
| emqx_rule_engine | 数据集成功能                                  |
| emqx_bridge_mqtt | 数据桥接 - MQTT 桥接                          |
| emqx_web_hook    | 数据桥接 - HTTP Server                        |
| emqx_coap        | CoAP 网关                                     |
| emqx_dashboard   | Dasboard                                      |
| emqx_exhook      | ExHook 功能                                   |
| emqx_exproto     | ExProto 网关                                  |
| emqx_lwm2m       | LwM2M 网关                                    |
| emqx_sn          | MQTT-SN 网关                                  |
| emqx_stomp       | STOMP 网关                                    |
| emqx_lua_hook    | -                                             |
| emqx_management  | Dashboard                                     |
| emqx_prometheus  | Prometheus 功能                               |
| emqx_psk_file    | 认证 PSK (`psk_authentication.enable = true`) |
| emqx_recon       | 通过 `emqx_ctl observer` 命令提供             |
| emqx_retainer    | Retain 功能                                   |
<!-- | emqx_telemetry   | 遥测功能                                      | -->

## HTTP API

此前 Dashboard 中的**应用 (Appication)** 用于管理 API 访问凭证，现已更名为 **API 密钥 (API Key)**，且 Secret Key 仅在创建成功时返回一次，后续无法再次获得。

1. 8081 端口已被合并至 18083 端口，所有 API 都通过 18083 端口提供。
2. 不能使用 Dashboard 的用户名和密码进行 API 访问，**必须**使用 API 密钥创建的访问凭证。
3. API 访问基础路径由 `/api/v4` 切换到 `/api/v5` 。请通过 18083 端口和 `/api/v5` 路径调用 API。
4. 时间相关的字段将使用带时区的 [RFC3339](https://datatracker.ietf.org/doc/html/rfc3339) 格式。

### 响应数据格式调整

成功响应时业务状态码 `code` 将不再随数据返回，出错时将返回对应的 4xx/5xx HTTP 状态码和错误提示，
用户可以访问 `GET /error_codes` 获取所有可能的错误码，以下是响应格式的对照示例：

**成功响应**

```shell
# 4.x
## HTTP StatusCode = 200
GET /rules/my_rule
{ "code": 0, "data": { ... } }

# 5.1
## HTTP StatusCode = 200
GET /rules/my_rule
{ ... }
```

**失败响应**

```bash
# 4.x
## HTTP StatusCode = 200
GET /rules/my_rule
{ "code": 404, "message": "Not Found" }

# 5.1
## HTTP StatusCode = 404
GET /rules/my_rule
{ "code": "NOT_FOUND", "message": "Rule Id Not Found" }
```

### 主要 API 变动

API 有较大变动，以下是常用的 API 变动对照表，现存的部分 API 我们做了兼容处理：

:::tip
兼容性说明：

- 兼容：沿用了旧版 API 路径跟参数，或者保留了旧版 API。
- 部分兼容：API 路径不变，但部分 API 字段发生改变。
- 不兼容：API 路径与字段都发生变化。

:::

| 4.x                             | 5.1                                         | 兼容性   | 说明                 |
| ------------------------------- | ------------------------------------------- | -------- | -------------------- |
| **发布/订阅操作**               |                                             |          |                      |
| `POST /mqtt/publish`            | `POST /publish`                             | 兼容     |                      |
| `POST /mqtt/publish_batch`      | `POST /publish/bulk`                        | 兼容     |                      |
| `POST /mqtt/subscribe`          | `POST /clients/{clientid}/subscribe`        | 兼容     |                      |
| `POST /mqtt/subscribe_batch`    | `POST /clients/{clientid}/subscribe/bulk`   | 兼容     |                      |
| `POST /mqtt/unsubscribe`        | `POST /clients/{clientid}/unsubscribe`      | 兼容     |                      |
| `POST /mqtt/unsubscribe_batch`  | `POST /clients/{clientid}/unsubscribe/bulk` | 兼容     |                      |
| **连接/主题/订阅**              |                                             |          |                      |
| `GET /clients`                  | `GET /clients`                              | 部分兼容 |                      |
| `GET /routes{/topic}`           | `GET /topics{/topic}`                       | 不兼容   | routes 更名为 topics |
| `GET /subscriptions`            | `GET /subscriptions`                        | 部分兼容 |                      |
| `GET /subscriptions/{clientid}` | `GET /clients/{clientid}/subscriptions`     | 不兼容   |                      |
| **节点/状态/指标**              |                                             |          |                      |
| `GET /nodes`                    | `GET /nodes`                                | 部分兼容 |                      |
| `GET /brokers`                  | -                                           | 不兼容   | 合并至 `GET /nodes`  |
| `GET /stats`                    | `GET /stats`                                | 部分兼容 |                      |
| `GET /metrics`                  | `GET /metrics`                              | 部分兼容 |                      |
| **用户/告警**                   |                                             |          |                      |
| `GET /users`                    | `GET /users`                                | 部分兼容 |                      |
| `GET /alarms{/activated}`       | `GET /alarms?activated={true,false}`        | 不兼容   |                      |
| `GET /alarms{/deactivated}`     | `GET /alarms?activated={true,false}`        | 不兼容   |                      |

## 认证/发布订阅 ACL

**功能入口**

认证授权插件 (emqx_auth_*) 已被移除，相关功能以内置功能的形式迁移到 EMQX 中，支持用户以 Dashboard 或配置文件的方式配置。

**概念调整**

发布订阅 ACL 更名为 **授权**，认证与发布订阅 ACL 功能进行了拆分。

**数据迁移**

我们保留了 4.x 版本中的身份验证方式和支持的数据源，仅对使用方式进行了一些调整。对于绝大多数认证/授权器，你可以在升级到 5.1 版本时继续使用 4.x 的数据库，无需迁移现有的数据。

### 认证/授权链顺序

同时启用多个认证器或授权检查器时，不再按照启动顺序而是按照固定的配置顺序来执行检查，可在配置文件跟 Dashboard 中调整的执行顺序。

### 占位符变量提取语法

此前，认证插件中可以使用 `%u` 类似的语法构造占位符以提取变量，将客户端信息动态拼接到如 SQL 语句、Redis 查询命令和 HTTP 请求中。现在 EMQX **所有**认证跟授权检查功能中使用了新的语法 `${}`，如 `${username}`、`${clientid}`，该语法与规则 SQL 一致。

支持的占位符请参考：

- [认证占位符](../access-control/authn/authn.md#认证占位符)
- [授权占位符](../access-control/authz/authz.md#占位符)

以下是前后版本配置对比：

```shell
# 4.x
# etc/emqx_auth_mysql.conf
auth.mysql.auth_query = select password from mqtt_user where username = '%u' limit 1

# 5.1
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

### 认证

#### 移除匿名认证机制

移除 `allow_anonymous` 配置项，默认允许所有客户端连接，**添加并启用**任意一个认证器后将对所有新连接进行认证检查。

当某个客户端在所有认证器中均匹配到认证数据时，将判定为拒绝连接。

移除 `bypass_auth_plugins` 配置项，某个监听器需要跳过认证时，可以通过 `listeners.{type}.{name}.enable_authn = true | false` 配置项进行设置。

#### 内置数据库 (Mnesia) 变动

1. 为方便理解，该数据源由 Mnesia 更名为内置数据库 (Built-in Database)；
2. 只能选定一种认证查找方式：基于用户名或基于客户端 ID；
3. 数据操作 REST API 有变动，新的 API 是 `POST /authentication/{id}/users`。

<!-- TODO: add migrate script -->

#### HTTP 变动

1. 使用响应 Body 里面的 JSON 字段而非响应状态码 (HTTP response status codes) 判断认证结果；
2. 移除单独的超级用户 (superuser) 请求，客户端的超级用户身份通过认证响应中的 body 设置。

**成功响应状态码:**

```shell
200 或 204
```

其他状态码或请求失败将忽略该认证器。

**成功响应 Body (JSON):**

| 名称          | 类型 | 必选  | 说明                    |
| ------------- | ---- | ----- | ----------------------- |
| result        | 枚举 | true  | `allow | deny | ignore` |
| is_supseruser | 布尔 | false | 默认为 false            |

```json
{
  "result": "allow",
  "is_supseruser": true
}
```

#### MySQL/PostgreSQL 变动

1. 查询结果中要求的密码字段由 `password` 更改为 `password_hash`，在不更改数据库列名的情况下可以在查询时使用 `as` 语法完成迁移；
2. 移除单独的超级用户 (superuser) 查询 SQL，如需超级用户功能请确保认证 SQL 结果包含 `is_superuser` 字段。

```sql
SELECT
  password as password_hash,
  salt,
  is_superuser 
FROM mqtt_user 
  where username = ${username} LIMIT 1
```

#### MongoDB 变动

1. 移除单独的超级用户 (superuser) 查询，如需超级用户功能请在 MongoDB 认证器配置指定 `is_superuser_field` 字段，并确保认证查询结果中包含超级用户信息。

```shell
authentication = [
  {
    ...
    mechanism = "password_based"
    backend = "mongodb"
    # is_superuser_field = "is_superuser"
  }
]
```

#### Redis 变动

1. 仅支持 [Redis Hashes](https://redis.io/docs/manual/data-types/#hashes) 数据结构与 `HGET`、`HMGET` 查询命令，必须使用 `password_hash` 或 `password`(兼容 4.x) 作为密码字段名；
2. 移除单独的超级用户 (superuser) 查询命令，如需超级用户功能请在 Redis 查询命令中添加 `is_superuser` 字段。

```shell
# bad
GET emqx_user:${username}
# bad
HMGET emqx_user:${username} passwd

# good
HMGET emqx_user:${username} password_hash

# good
HMGET emqx_user:${username} password_hash is_superuser
```

#### LDAP 变动

暂不支持 LDAP 认证。

#### JWT 变动

无变动。

### 授权（ACL）

#### ACL File 变动

1. 移除 `acl_file` 配置，基于文件的 ACL(acl.conf) 将作为授权检查器中的一种，默认添加到 EMQX 中；
2. `acl.conf` 文件中，几个关键字语法有所调整，变更情况见下表：

| 4.x    | 5.1.0    | 是否兼容 |
| ------ | -------- | -------- |
| user   | username | 是       |
| client | clientid | 是       |
| pubsub | all      | 否       |

```bash
# 4.x
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.
{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

# 5.1
{allow, {username, {re, "^dashboard$"}}, subscribe, ["$SYS/#"]}.
{allow, {ipaddr, "127.0.0.1"}, all, ["$SYS/#", "#"]}.
```

#### 内置数据库 (Mnesia) 变动

1. 为方便理解，该数据源由 Mnesia 更名为内置数据库 (Built-in Database)；
2. 数据格式与数据操作 REST API 有变动，新的 API 是 `/authorization/sources/built_in_database/rules{/clients,/users}`。

4.x 版本中的 ACL 数据可以通过 `./bin/emqx_ctl data export` 命令导出。用户可以将数据转换为符合 5.1 版本的格式，并通过相应的 REST API 导入。

<!-- TODO add migrate script -->

#### MySQL/PostgreSQL 变动

1. 查询结果中不再需要 `ipaddr/username/clientid` 字段，需要调整查询 SQL；
2. `access` 字段名变为 `action`，数据类型由整型变为字符或字符枚举，数值对应关系见下表；
3. `allow` 字段名变为 `permission`，数据类型由整型变为字符或字符枚举，数值对应关系见下表。

**access-action 字段数据类型映射**

| 4.x (int) | 5.1.0 (varchar/enum) | 对应动作 |
| --------- | -------------------- | -------- |
| 1         | subscribe            | 订阅     |
| 2         | publish              | 发布     |
| 3         | all                  | 发布订阅 |

**allow-permission 字段数据类型映射**

| 4.x (int) | 5.1.0 (varchar/enum) | 对应行为 |
| --------- | -------------------- | -------- |
| 0         | deny                 | 拒绝     |
| 1         | allow                | 允许     |

#### MongoDB 变动

1. MongoDB 数据源可用于黑/白名单模式下，此前仅支持白名单模式，要求设置 `authorization.no_match = deny`；
2. 需要从 MongoDB 中查询出包含 `action` `permission` `topics` 字段的数据列表，使用方式详见 [AuthZ-MongoDB](../access-control/authz/mongodb.md)。

如需继续使用 4.x 中的数据，请手动迁移适配。

**5.1 中数据示例**

```json
[
  {
      "username": "emqx_u",
      "clientid": "emqx_c",
      "ipaddress": "127.0.0.1",
      "permission": "allow",
      "action": "all",
      "topics": ["#"]
  }
]
```

#### Redis 变动

1. Redis 数据源仍然仅支持白名单模式，要求设置 `authorization.no_match = deny`；
2. `access` 字段名变为 `action`，数据由数字变为动作全称字符串，对应关系见下表。

| 4.x | 5.1.0     | 对应动作 |
| --- | --------- | -------- |
| 1   | subscribe | 订阅     |
| 2   | publish   | 发布     |
| 3   | all       | 发布订阅 |

如需继续使用 4.x 中的数据，请手动迁移适配。

**5.1 中数据示例**

```bash
HSET mqtt_acl:emqx_u t/# subscribe
HSET mqtt_acl:emqx_u # all
HSET mqtt_acl:emqx_u a/1 publish
```

#### HTTP 变动

1. 使用响应 Body 里面的 JSON 字段而非响应状态码 (HTTP response status codes) 判断认证结果。

**成功响应状态码:**

```shell
200 或 204
```

其他状态码或请求失败则忽略该授权检查器。

**成功响应 Body (JSON):**

| 名称   | 类型 | 必选 | 说明                    |
| ------ | ---- | ---- | ----------------------- |
| result | 枚举 | true | `allow | deny | ignore` |

```json
{
  "result": "deny"
}
```

## 规则引擎

规则引擎已更名为数据集成，包含规则与数据桥接功能。

规则 SQL 完全兼容 4.x 的语法，但规则下的动作拆分为内置动作 (republish、console) 与数据桥接 (HTTP Server、MQTT 桥接)。

{% emqxee %}

## 离线消息

目前 EMQX 4.x 版本中提供的[离线消息](https://docs.emqx.com/zh/enterprise/v4.4/rule/offline_msg_to_redis.html)是基于外置数据库实现的，EMQX 计划在后续版本提供原生的离线消息功能（基于内置数据库），因此从 5.0.0 版本中移除了外置数据库的离线消息功能。

后续的原生离线消息能够提供更高的性能，并有效降低使用和维护成本，敬请期待。

## 代理订阅（从数据库中获取订阅关系）

从 5.0.0 版本开始，EMQX 不再提供基于外置数据库的[代理订阅](https://docs.emqx.com/zh/enterprise/v4.4/rule/get_subs_from_redis.html)（从数据库中获取订阅关系）功能。

{% endemqxee %}

## WebHook

4.x 版本中的 WebHook 插件 (emqx_web_hook) 已被移除，请使用数据集成中的 HTTP Server 数据桥接功能替代。

## MQTT 桥接

MQTT 桥接插件 (emqx_bridge_mqtt) 已被移除，请使用数据集成中的 MQTT 数据桥接功能替代。

## Prometheus

旧的 Prometheus 插件 (emqx_prometheus) 已被移除。在 5.1 中，Prometheus 的数据拉取服务默认开启，且无需认证就可以拉取数据，可以使用 curl 命令来查看：

```bash
curl -f "http://127.0.0.1:18083/api/v5/prometheus/stats"
```

如果您想要使用 push-gateway，可参考[集成 Prometheus](../observability/prometheus.md)。

除了配置方式外，Prometheus 的指标也发生了变化：

| 4.4.x                              | 5.1                                             | 说明 |
| ---------------------------------- | ----------------------------------------------- | ---- |
| emqx_client_auth_success_anonymous | emqx_client_auth_anonymous                      | 更名 |
| emqx_client_check_acl              | emqx_client_authorize counter                   | 更名 |
| -                                  | emqx_mria_last_intercepted_trans                | 新增 |
| -                                  | emqx_mria_replicants                            | 新增 |
| -                                  | emqx_mria_server_mql                            | 新增 |
| -                                  | emqx_mria_weight                                | 新增 |
| emqx_routes_count                  | emqx_topics_count                               | 更名 |
| emqx_routes_max                    | emqx_topics_max                                 | 更名 |
| emqx_session_takeovered            | emqx_session_takenover                          | 更名 |
| erlang_vm_ets_tables               | -                                               | 移除 |
| -                                  | erlang_vm_memory_dets_tables                    | 新增 |
| -                                  | erlang_vm_memory_ets_tables                     | 新增 |
| -                                  | erlang_vm_msacc_alloc_seconds_total             | 新增 |
| -                                  | erlang_vm_msacc_aux_seconds_total               | 新增 |
| -                                  | erlang_vm_msacc_bif_seconds_total               | 新增 |
| -                                  | erlang_vm_msacc_busy_wait_seconds_total         | 新增 |
| -                                  | erlang_vm_msacc_check_io_seconds_total          | 新增 |
| -                                  | erlang_vm_msacc_emulator_seconds_total          | 新增 |
| -                                  | erlang_vm_msacc_ets_seconds_total               | 新增 |
| -                                  | erlang_vm_msacc_gc_full_seconds_total           | 新增 |
| -                                  | erlang_vm_msacc_gc_seconds_total                | 新增 |
| -                                  | erlang_vm_msacc_nif_seconds_total               | 新增 |
| -                                  | erlang_vm_msacc_other_seconds_total             | 新增 |
| -                                  | erlang_vm_msacc_port_seconds_total              | 新增 |
| -                                  | erlang_vm_msacc_send_seconds_total              | 新增 |
| -                                  | erlang_vm_msacc_sleep_seconds_total             | 新增 |
| -                                  | erlang_vm_msacc_timers_seconds_total            | 新增 |
| -                                  | erlang_vm_statistics_dirty_cpu_run_queue_length | 新增 |
| -                                  | erlang_vm_statistics_dirty_io_run_queue_length  | 新增 |
| -                                  | erlang_vm_wordsize_bytes                        | 新增 |

## 网关/多协议接入

其他协议（LwM2M、CoAP、STOMP、MQTT-SN）的客户端将不再映射为 MQTT 客户端，无法通过 Dashboard 客户端页面和 `GET /clients` API 获取。
用户可以前往网关页面详情页面或通过 `GET /gateway/{name}/clients` API 获取。

{% emqxce %}

## 遥测

遥测插件 (emqx_telemetry) 已被移除，请通过 `telemetry {}` 配置项或 Dashboard **系统设置** -> **设置** 页面进行配置。

{% endemqxce %}
