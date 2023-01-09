# 从 v4 迁移到 v5

本章节提供了从 EMQX 4.x 版本迁移至 5.0.0 版本的指南，尽管 EMQX 5.0 不保证向后兼容（Backward Compatibility），但绝大部分功能运行机制没有太大的变化，你仍然可以通过手动迁移的方式来实现升级。

要了解我们在 EMQX 5.0 中增加的新功能，请查看 [5.0.0 Release](https://www.emqx.com/zh/changelogs/broker/5.0.0)。

## 日志

日志输出格式进行了调整，请参照[日志](../observability/log.md)。

## 默认监听器

默认将不再提供以下监听器：

| 监听器               | 说明                               |
| -------------------- | ---------------------------------- |
| MQTT-TCP 11883       | 后端应用使用的 MQTT 接入点         |
| Management-HTTP 8081 | REST API 端口，已合并至 18083 端口 |

## 插件

此前的官方插件已迁移到 EMQX 中成为内置功能，用户在 4.x 版本上开发的插件需要重新适配。以下是官方插件与现有功能对照表：

| 4.x              | 5.0                                           |
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
| emqx_bridge_mqtt | 数据桥接 - MQTT Sink/MQTT Source              |
| emqx_web_hook    | 数据桥接 - WebHook                            |
| emqx_coap        | CoAP 网关                                     |
| emqx_dashboard   | Dasboard                                      |
| emqx_exhook      | ExHook 功能                                   |
| emqx_exproto     | ExProto 网关                                  |
| emqx_lwm2m       | LwM2M 网关                                    |
| emqx_sn          | MQTT-SN 网关                                  |
| emqx_stomp       | STOMP 网关                                    |
| emqx_lua_hook    | -                                             |
| emqx_management  | -                                             |
| emqx_prometheus  | Prometheus 功能                               |
| emqx_psk_file    | 认证 PSK (`psk_authentication.enable = true`) |
| emqx_recon       | 通过 `emqx_ctl observer` 命令提供             |
| emqx_retainer    | Retain 功能                                   |
| emqx_telemetry   | 遥测功能                                      |

## HTTP API

此前“应用”(Appication)用于管理 API 访问凭证，现已更名为“API 密钥”(API Key)，且 Secret Key 仅在创建成功时返回一次后续无法再次获得。

8081 端口已被合并至 18083 端口，API 访问基础路径由 `/api/v4` 切换到 `/api/v5`，请通过此端口和路径调用 API；时间相关的数据将使用 [RFC3339](https://datatracker.ietf.org/doc/html/rfc3339) 格式。

### 响应数据格式调整

成功响应时业务状态码 `code` 将不再随数据返回，出错时将返回对应的 4xx/5xx HTTP 状态码和错误提示，
用户可以访问 `GET /error_codes` 获取所有可能的错误码，以下是响应格式的对照示例：

```shell
# 4.x 成功响应
## HTTP StatusCode = 200
GET /rules/my_rule
{ "code": 0, "data": { ... } }

# 4.x 错误响应
## HTTP StatusCode = 200
GET /rules/my_rule
{ "code": 404, "message": "Not Found" }

# 5.0 成功响应
## HTTP StatusCode = 200
GET /rules/my_rule
{ ... }

# 5.0 错误响应
## HTTP StatusCode = 404
GET /rules/my_rule
{ "code": "NOT_FOUND", "message": "Rule Id Not Found" }
```

### 主要 API 变动

API 有较大变动，以下是常用的 API 变动对照表，现存的部分 API 我们做了兼容处理：

:::tip
兼容性说明：

- 兼容：沿用了旧版 API 路径跟参数，或者保留了旧版 API
- 部分兼容：API 路径不变，但部分 API 字段发生改变
- 不兼容：API 路径与字段都发生变化
:::

| 4.x                             | 5.0                                     | 兼容性   | 说明                 |
| ------------------------------- | --------------------------------------- | -------- | -------------------- |
| **发布/订阅操作**               |                                         |          |                      |
| `POST /mqtt/publish`            | `POST /publish`                         | 兼容     |                      |
| `POST /mqtt/publish_batch`      | `POST /publish/bulk`                    | 兼容     |                      |
| `POST /mqtt/subscribe`          | `POST /clients/{clientid}/subscribe`    | 兼容     |                      |
| `POST /mqtt/subscribe_batch`    | `POST /clients/{clientid}/subscribe/bulk` | 兼容     |                      |
| `POST /mqtt/unsubscribe`        | `POST /clients/{clientid}/unsubscribe`  | 兼容     |                      |
| `POST /mqtt/unsubscribe_batch`  | `POST /clients/{clientid}/unsubscribe/bulk` | 兼容     |                      |
| **连接/主题/订阅**              |                                         |          |                      |
| `GET /clients`                  | `GET /clients`                          | 部分兼容 |                      |
| `GET /routes{/topic}`           | `GET /topics{/topic}`                   | 不兼容   | routes 更名为 topics |
| `GET /subscriptions`            | `GET /subscriptions`                    | 部分兼容 |                      |
| `GET /subscriptions/{clientid}` | `GET /clients/{clientid}/subscriptions` | 不兼容   |                      |
| **节点/状态/指标**              |                                         |          |                      |
| `GET /nodes`                    | `GET /nodes`                            | 部分兼容 |                      |
| `GET /brokers`                  | -                                       | 不兼容   | 合并至 `GET /nodes`  |
| `GET /stats`                    | `GET /stats`                            | 部分兼容 |                      |
| `GET /metrics`                  | `GET /metrics`                          | 部分兼容 |                      |
| **用户/告警**                   |                                         |          |                      |
| `GET /users`                    | `GET /users`                            | 部分兼容 |                      |
| `GET /alarms{/activated}`       | `GET /alarms?activated={true,false}`    | 不兼容   |                      |
| `GET /alarms{/deactivated}`     | `GET /alarms?activated={true,false}`    | 不兼容   |                      |

## 认证/发布订阅 ACL

认证授权插件(emqx_auth_*)已被移除，相关能力以内置功能的形式迁移到 EMQX 中，支持用户以 Dashboard 或配置文件的方式配置。

发布订阅 ACL 更名为 **授权**，认证与发布订阅 ACL 功能进行了拆分。

我们保留了 4.x 中的认证方式与支持的数据源，但使用方式上有一定调整。

### 固定的执行顺序

同时启用多个认证器或授权检查器时，不再按照启动顺序而是按照固定的配置顺序来执行检查，可在配置文件跟 Dashboard 中调整的执行顺序。

### 更换占位符变量提取语法

此前认证插件中可以使用 `%u` 类似的语法构造占位符以提取变量，将客户端信息动态拼接到如 SQL 语句、Redis 查询命令和 HTTP 请求中，
现在 EMQX **所有**认证跟授权检查功能中使用了新的语法 `${}`，如 `${username}`、`${clientid}`，该语法与规则 SQL 一致。

以下是前后版本配置对比：

```shell
# 4.x
# etc/emqx_auth_mysql.conf
auth.mysql.auth_query = select password from mqtt_user where username = '%u' limit 1

# 5.0
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

移除 `bypass_auth_plugins` 配置项，某个监听器需要禁用认证时，可以通过 `listeners.{type}.{name}.enable_authn = true | false` 配置项进行设置。

#### 内置数据库 (Mnesia) 变动

1. 为方便理解，该数据源由 Mnesia 更名为内置数据库(Built-in Database)；
2. 只能选定一种认证查找方式：基于用户名或基于客户端 ID；
3. 数据格式与数据操作 REST API 有变动，详见 `POST /authentication/{id}/users`。

用户可以使用数据导入 API 将旧版本中的数据导入到 EMQX 5.0 中，详见 `POST /authentication/{id}/import_users`。

#### HTTP 变动

1. 使用响应 Body 里面的 JSON 字段而非响应状态码(HTTP response status codes)判断认证结果；
2. 移除超级用户(superuser)查询，如需超级用户功能请在响应 body 里面通过 JSON 设置。

**成功响应状态码:**

```shell
200 或 204
```

其他状态码或请求失败则忽略该认证器。

**成功响应 Body (JSON):**

| 名称          | 类型 | 必选  | 说明                        |
| ------------- | ---- | ----- | --------------------------- |
| result        | 枚举 | true  | `allow | deny | ignore` |
| is_supseruser | 布尔 | false | 默认为 false                |

```json
{
  "result": "allow",
  "is_supseruser": true
}
```

#### MySQL/PostgreSQL 变动

1. 查询结果中要求的密码字段由 `password` 更改为 `password_hash`，在不更改数据库列名的情况下可以在查询时使用 `as` 语法完成迁移；
2. 移除超级用户(superuser)查询 SQL，如需超级用户功能请确保认证 SQL 结果包含 `is_superuser` 字段。

```sql
SELECT password as password_hash, salt, is_superuser FROM mqtt_user where username = ${username} LIMIT 1
```

#### MongoDB 变动

1. 移除超级用户(superuser)查询，如需超级用户功能请在 MongoDB 认证器配置指定 `is_superuser_field` 字段，并确保认证查询结果中包含超级用户信息。

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

1. 仅支持 [Redis Hashes](https://redis.io/docs/manual/data-types/#hashes) 数据结构与 `HGET`、`HMGET` 查询命令，必须使用 `password_hash` 或 `password`(兼容 4.x)作为密码字段名；
2. 移除超级用户(superuser)查询，如需超级用户功能请在 Redis 查询命令中添加 `is_superuser` 字段。

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
2. `acl.conf` 文件从 `etc` 目录移动到 `data/authz` 目录，请确保 EMQX 具有该文件的读写权限；
3. `acl.conf` 数据文件语法有所调整，变更情况见下表：

| 4.x    | 5.0.0    | 是否兼容 |
| ------ | -------- | -------- |
| user   | username | 是       |
| client | clientid | 是       |
| pubsub | all      | 否       |

#### 内置数据库 (Mnesia) 变动

1. 为方便理解，该数据源由 Mnesia 更名为内置数据库(Built-in Database)；
2. 数据格式与数据操作 REST API 有变动，详见 `POST /authorization/built_in_database/clientid`。

4.x 中的数据导出命令 `./bin/emqx_ctl data export` 导出数据中包含 ACL 数据，用户可以将数据处理为 5.0 所需格式通过对应的 REST API 进行导入。

#### MySQL/PostgreSQL 变动

1. 查询结果中不再需要 `ipaddr/username/clientid` 字段；
2. `access` 字段名变为 `action`，数据类型由整型变为字符或字符枚举，数值对应关系见下表；
3. `allow` 字段名变为 `permission`，数据类型由整型变为字符或字符枚举，数值对应关系见下表。

**access-action 字段数据类型映射**

| 4.x (int) | 5.0.0 (varchar/enum) | 对应动作 |
| --------- | -------------------- | -------- |
| 1         | subscribe            | 订阅     |
| 2         | publish              | 发布     |
| 3         | all                  | 发布订阅 |

**allow-permission 字段数据类型映射**

| 4.x (int) | 5.0.0 (varchar/enum) | 对应行为 |
| --------- | -------------------- | -------- |
| 0         | deny                 | 拒绝     |
| 1         | allow                | 允许     |

#### MongoDB 变动

1. MongoDB 数据源可用于黑/白名单模式下，此前仅支持白名单模式，要求设置 `acl_nomatch = deny`；
2. 需要从 MongoDB 中查询出包含 `action` `permission` `topics` 字段的数据列表，使用方式详见 [AuthZ-MongoDB](../security/authz/mongodb.md)。

如需继续使用 4.x 中的数据，请手动迁移适配。

**5.0 中数据示例**

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

1. Redis 数据源仍然仅支持白名单模式，要求设置 `acl_nomatch = deny`；
2. `access` 字段名变为 `action`，数据由数字变为动作全称字符串，对应关系见下表。

| 4.x | 5.0.0     | 对应动作 |
| --- | --------- | -------- |
| 1   | subscribe | 订阅     |
| 2   | publish   | 发布     |
| 3   | all       | 发布订阅 |

如需继续使用 4.x 中的数据，请手动迁移适配。

**5.0 中数据示例**

```
HSET mqtt_acl:emqx_u t/# subscribe
HSET mqtt_acl:emqx_u # all
HSET mqtt_acl:emqx_u a/1 publish
```

#### HTTP 变动

1. 使用响应 Body 里面的 JSON 字段而非响应状态码(HTTP response status codes)判断认证结果。

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

规则引擎已更名为[数据集成](../data-integration/introduction.md)，包含规则与数据桥接功能。

规则 SQL 完全兼容 4.x 的语法，但规则下的动作拆分为内置动作(republish、console)与数据桥接(WebHook、MQTT Sink、MQTT Source)，以便实现"动作"的复用。

## WebHook

WebHook 插件(emqx_web_hook) 已被移除，请使用数据集成中的 WebHook 数据桥接功能替代。

## MQTT 桥接

MQTT 桥接插件(emqx_bridge_mqtt) 已被移除，请使用数据集成中的 MQTT Sink 和 MQTT Source 数据桥接功能替代。

## Prometheus

旧的 Prometheus 插件 (emqx_prometheus) 已被移除。
在5.0中，Prometheus 的数据拉取服务默认开启，且无需认证就可以拉取数据。
可以使用 curl 命令来查看：curl -f "127.0.0.1:18083/api/v5/prometheus/stats"

如果您想要使用 push-gateway，您可以在 prometheus {...} 配置块中启用它。
或者在仪表盘中修改配置并启用。

指标变动情况：

| 4.4.4                              | 5.0                                             | 说明 |
| ---------------------------------- | ----------------------------------------------- | ---- |
| emqx_client_auth_success_anonymous | emqx_client_auth_anonymous                      | 更名 |
| emqx_client_check_acl              | emqx_client_authorize counter                   | 更名 |
| -                                  | emqx_mria_last_intercepted_trans                | 新增 |
| -                                  | emqx_mria_replicants                            | 新增 |
| -                                  | emqx_mria_server_mql                            | 新增 |
| -                                  | emqx_mria_weight                                | 新增 |
| emqx_routes_count                  | emqx_topics_count                                               | 更名 |
| emqx_routes_max                    | emqx_topics_max                                               | 更名 |
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

## 遥测

遥测插件 (emqx_telemetry) 已被移除，请通过 `telemetry {}` 配置项或 Dashboard **系统设置** -> **设置** 页面进行配置。
