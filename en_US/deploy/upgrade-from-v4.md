# Migrate from V4

This document provides guidelines for migrating from EMQX 4.x to 5.0.
EMQX 5.0 is not backward compatible with 4.x in management APIs and clustering APIs. However, most of the functionality is not significantly changed.

To learn about the new features we added to EMQX 5.0, check [5.0.0 Release](https://www.emqx.com/en/changelogs/broker/5.0.0).

## Log

Compared with 4.x, the most significant change in the log is the format.
In 4.x, the log text is primarily plain, aiming for good human readability.
However, starting from 5.0, we moved to structured logging without losing readability.
For instance, most log fields use underscores as word separators,
making them more search-friendly. This format also helps log indexing tools to index the logs more effectively.

`2022-06-29T16:58:53.235042+02:00 [info] foo: bar, msg: msg_for_human_to_read_but_also_easy_to_index`

Find more details in [Log & Trace](../observability/log.md).

## Default listeners

By default, the following listeners will no longer be provided:

| Listeners            | Description                        |
| -------------------- | ---------------------------------- |
| MQTT-TCP 11883       | Listeners for backend applications |
| Management-HTTP 8081 | For REST API, merged to port 18083 |

## Plugins

The previous official plugins have been migrated to EMQX as built-in functions. Custom plugins developed for version 4.x need to be adapted before they can be used in EMQX 5.0. The following is a comparison table between official plugins and existing functions:

| 4.x              | 5.0                                                       |
| ---------------- | --------------------------------------------------------- |
| emqx_auth_http   | AuthN/AuthZ - HTTP data source                            |
| emqx_auth_jwt    | AuthN/AuthZ - JWT                                         |
| emqx_auth_mnesia | AuthN/AuthZ - Built-in database                           |
| emqx_auth_mongo  | AuthN/AuthZ - MongoDB data source                         |
| emqx_auth_mysql  | AuthN/AuthZ - MySQL data source                           |
| emqx_auth_pgsql  | AuthN/AuthZ - PostgreSQL data source                      |
| emqx_auth_redis  | AuthN/AuthZ - Redis data source                           |
| emqx_sasl        | AuthN/AuthZ - MQTT 5 Enhanced Authentication              |
| emqx_auth_ldap   | -                                                         |
| emqx_rule_engine | Data Integration                                          |
| emqx_bridge_mqtt | Data Bridge - MQTT Sink/MQTT Source                       |
| emqx_web_hook    | Data Bridge - WebHook                                     |
| emqx_coap        | CoAP Gateway                                              |
| emqx_dashboard   | Dasboard                                                  |
| emqx_exhook      | ExHook                                                    |
| emqx_exproto     | ExProto Gateway                                           |
| emqx_lwm2m       | LwM2M Gateway                                             |
| emqx_sn          | MQTT-SN Gateway                                           |
| emqx_stomp       | STOMP Gateway                                             |
| emqx_lua_hook    | -                                                         |
| emqx_management  | -                                                         |
| emqx_prometheus  | Prometheus                                                |
| emqx_psk_file    | AuthN - PSK (`psk_authentication.enable = true`)          |
| emqx_recon       | Old features still available from CLI `emqx_ctl observer` |
| emqx_retainer    | Retain                                                    |
| emqx_telemetry   | Telemetry                                                 |

## HTTP API

Previously, "Applications" dashboard section was used to manage API access credentials. Now "API Key" section should be used to create credentials. The credentials consist of API Key and Secret Key that can be respectively used as username and password in HTTP basic auth. Secret Key is only displayed once when credentials are created, but cannot be obtained again later.

Port 8081 has been merged into port 18083, and the basic path of API access has been switched from `/api/v4` to `/api/v5`, please call the API through this port and path.

String timestamps are now in [RFC3339](https://datatracker.ietf.org/doc/html/rfc3339) format.

### Data Format Changes

When the response is successful, the business status code `code` will no longer be returned with the data, and the corresponding 4xx/5xx HTTP status code and error prompt will be returned when an error occurs.
Users can access `GET /error_codes` to get all possible error codes. The following is a comparative example of the response format:

```shell
# 4.x successful response
## HTTP StatusCode = 200
GET /api/v4/rules/my_rule
{ "code": 0, "data": { ... } }

# 4.x error response
## HTTP StatusCode = 200
GET /api/v4/rules/my_rule
{ "code": 404, "message": "Not Found" }

# 5.0 successful response
## HTTP StatusCode = 200
GET /api/v5/rules/my_rule
{ ... }

# 5.0 error response
## HTTP StatusCode = 404
GET /api/v5/rules/my_rule
{ "code": "NOT_FOUND", "message": "Rule Id Not Found" }
```

### Major API Changes

The API has undergone significant changes, and some APIs have been made compatible. The following is a comparison table of commonly used API changes.

:::tip
Compatibility Notes:

- Compatible: Use the old API path and parameters, or keep the old API
- Partially compatible: The API path remains unchanged, but some API fields have changed
- Incompatible: API paths and fields have changed
:::

| 4.x                              | 5.0                                         | Compatibility        | Notes                        |
| -------------------------------- | ------------------------------------------- | -------------------- | ---------------------------- |
| **Publish/Subscribe**            |                                             |                      |                              |
| `POST /mqtt/publish`             | `POST /publish`                             | Compatible           |                              |
| `POST /mqtt/publish_batch`       | `POST /publish/bulk`                        | Compatible           |                              |
| `POST /mqtt/subscribe`           | `POST /clients/{clientid}/subscribe`        | Compatible           |                              |
| `POST /mqtt/subscribe_batch`     | `POST /clients/{clientid}/subscribe/bulk`   | Compatible           |                              |
| `POST /mqtt/unsubscribe`         | `POST /clients/{clientid}/unsubscribe`      | Compatible           |                              |
| `POST /mqtt/unsubscribe_batch`   | `POST /clients/{clientid}/unsubscribe/bulk` | Compatible           |                              |
| **Clients/Topics/Subscriptions** |                                             |                      |                              |
| `GET /clients`                   | `GET /clients`                              | Partially compatible |                              |
| `GET /routes{/topic}`            | `GET /topics{/topic}`                       | Incompatible         | `routes` renamed to `topics` |
| `GET /subscriptions`             | `GET /subscriptions`                        | Partially compatible |                              |
| `GET /subscriptions/{clientid}`  | `GET /clients/{clientid}/subscriptions`     | Incompatible         |                              |
| **Node/Stats/Metrics**           |                                             |                      |                              |
| `GET /nodes`                     | `GET /nodes`                                | Partially compatible |                              |
| `GET /brokers`                   | -                                           | Incompatible         | merged to `GET /nodes`       |
| `GET /stats`                     | `GET /stats`                                | Partially compatible |                              |
| `GET /metrics`                   | `GET /metrics`                              | Partially compatible |                              |
| **Users/Alarms**                 |                                             |                      |                              |
| `GET /users`                     | `GET /users`                                | Partially compatible |                              |
| `GET /alarms{/activated}`        | `GET /alarms?activated={true,false}`        | Incompatible         |                              |
| `GET /alarms{/deactivated}`      | `GET /alarms?activated={true,false}`        | Incompatible         |                              |

## Auth/ACL

The Auth and ACL plugins (emqx_auth_*) have been removed, and the corresponding functions are built into EMQX, and users can configure them through Dashboard or configuration files.

Auth was renamed to **Authentication**, and ACL was renamed **Authorization**. Now they are independent of each other.

Version 5.0 supports the authentication methods and most data sources in 4.x, but there are certain adjustments in the usage.

### Fixed Order of Execution

When multiple authenticators or authorization checkers are enabled simultaneously, the checks are no longer performed according to the startup order but a fixed configuration order, which can be adjusted in the configuration file and Dashboard.

### Variable Interpolation Syntax

Previously, the Auth plugins could use `%u` syntax to construct variable placeholders, dynamically interpolating client information into SQL statements, Redis query commands, and HTTP requests.
Now EMQX uses the new syntax `${}`, such as `${username}`, `${clientid}`, which is consistent with the rule SQL.

Usage example:

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

### Auth

#### Remove the Anonymous Mechanism

Remove the `allow_anonymous` configuration item. All client connections are allowed by default.
If **add and enable** any authenticator, EMQX will perform an authentication check for all new connections.

Remove the `bypass_auth_plugins` configuration item. When a listener needs to disable authentication, it can be set through the `listeners.{type}.{name}.enable_authn = true | false` configuration item.

#### Built-in database (Mnesia)

1. Mnesia is renamed to the built-in database;
2. Only two search methods are available: username-based or clientid-based;
3. The data format and REST API have changed. For more information, please refer to `POST /authentication/{id}/users`.

Users can use the data import API to import data from older versions into EMQX 5.0, see `POST /authentication/{id}/import_users` for details.

#### HTTP

1. Use the JSON fields inside the response body instead of the HTTP response status codes to identify the authentication result;
2. Remove the super user query configuration. If you need the super-user function, please set it through JSON in the response body of successful authentication.

**Success response status code:**

```shell
200 or 204
```

The authenticator will be ignored if the request fails or returns another status code.

**Success Response Body (JSON):**

| Name          | Type    | Required | Description                 |
| ------------- | ------- | -------- | --------------------------- |
| result        | Enum    | true     | `allow | deny | ignore` |
| is_supseruser | Boolean | false    |                             |

```json
{
  "result": "allow",
  "is_supseruser": true
}
```

#### MySQL/PostgreSQL

1. The password field required in the query result is changed from `password` to `password_hash`. If you do not want to change the database column name, you can use `as` to complete the migration;
2. Remove the super-user query SQL. If you need to give clients super-user permissions, please ensure that the authentication SQL result contains the `is_superuser` field.

```sql
SELECT password as password_hash, salt, is_superuser FROM mqtt_user where username = ${username} LIMIT 1
```

#### MongoDB

Remove the super-user query. If you need to give clients super-user permissions, please specify the `is_superuser_field` field.

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

#### Redis

1. Only supports [Redis Hashes](https://redis.io/docs/manual/data-types/#hashes) data structure and `HGET`, `HMGET` query commands. The commands must return `password_hash` as the password field name;
2. Remove the super-user query. If you need to give clients super-user permissions, please add the `is_superuser` field to the Redis query command.

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

#### LDAP

Not supported for now.

#### JWT

No changes.

### Authorization (ACL)

#### ACL File

1. Remove the `acl_file` configuration. The file-based ACL (acl.conf) will be used as one of the authorization sources and added to EMQX by default;
2. The `acl.conf` file is moved from the `etc` directory to the `data/authz` directory. Please ensure that EMQX has read and write permissions for this file;
3. `acl.conf` data file syntax has changed

| 4.x    | 5.0.0    | Compatibility |
| ------ | -------- | ------------- |
| user   | username | Yes           |
| client | clientid | Yes           |
| pubsub | all      | No            |

#### Built-in Database (Mnesia)

1. Mnesia renamed to the built-in database;
2. The data format and REST API have changed. For more information, please refer to `POST /authorization/built_in_database/clientid`.

4.x ACL data can be exported with `./bin/emqx_ctl data export`  command. Users may convert the data into 5.0 format and import it through the corresponding REST API.

#### MySQL/PostgreSQL

1. The `ipaddr/username/clientid` field is no longer required in the query result;
2. The `access` field name is changed to `action`, and its data type is changed from integer to character or character enumeration.
3. The `allow` field name is changed to `permission`, and its data type is changed from integer to character or character enumeration.

The correspondence between 4.x integer values and 5.0 character/enumeration values is shown in the following tables.

**access/action field mapping**

| 4.x (int) | 5.0.0 (varchar/enum) | action              |
| --------- | -------------------- | ------------------- |
| 1         | subscribe            | subscribe           |
| 2         | publish              | publish             |
| 3         | all                  | subscribe & publish |

**allow/permission field mapping**

| 4.x (int) | 5.0.0 (varchar/enum) | permission |
| --------- | -------------------- | ---------- |
| 0         | deny                 | deny       |
| 1         | allow                | allow      |

#### MongoDB

1. MongoDB data source can be used for both allow and deny rules. Previously, only white list mode was supported, and it was required to set `acl_nomatch = deny`;
2. You need to select documents containing the `action`, `permission` and `topics` fields from MongoDB. For details, see [AuthZ-MongoDB](../security/authz/mongodb.md).

If you want to continue using the data from in 4.x, please make necessary migrations manually.

**Data example in 5.0**

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

#### Redis

1. Redis data source still only supports white list mode, which requires setting `acl_nomatch = deny`;
2. The `access` field name changes to `action`, and the data changes from numbers to action strings. The correspondence is shown in the table below.

| 4.x | 5.0.0     | action              |
| --- | --------- | ------------------- |
| 1   | subscribe | subscribe           |
| 2   | publish   | publish             |
| 3   | all       | subscribe & publish |

If you want to continue using the data from in 4.x, please make necessary migrations manually.

**Data example in 5.0**

```
HSET mqtt_acl:emqx_u t/# subscribe
HSET mqtt_acl:emqx_u # all
HSET mqtt_acl:emqx_u a/1 publish
```

#### HTTP

1. Use the JSON fields in the response body instead of the HTTP response status codes to identify the authentication result.

**Success response status code:**

```shell
200 or 204
```

Other status codes or request failure will be treated as `ignore`.

**Success Response Body (JSON):**

| Name          | Type    | Required | Description                 |
| ------------- | ------- | -------- | --------------------------- |
| result        | Enum    | true     | `allow | deny | ignore` |

```json
{
  "result": "deny"
}
```

## Rule Engine

The rule engine has been renamed to [Data Integration](../data-integration/introduction.md), which includes rules and data bridge.

Rule SQL is fully compatible with 4.x syntax, but the actions under the rule are split into built-in actions (republish, console) and data bridges (WebHook, MQTT Sink, MQTT Source) to provide reuse of actions.

## WebHook

The WebHook plugin (emqx_web_hook) has been removed. Please use the WebHook data bridge in Data Integration instead.

## MQTT Bridge

The MQTT bridge plugin (emqx_bridge_mqtt) has been removed. Please use the MQTT Sink and MQTT Source data bridge in Data Integration instead.

## Prometheus

Prometheus scraping endpoint is enabled by default, and no authentication is required to scrap the metrics.

You can use `curl` command to inspect the metrics: `curl -f "127.0.0.1:18083/api/v5/prometheus/stats"`

If you wish to enable push-gateway, configure it in the dashboard or through `prometheus {...}` config block.

Changes:

| 4.4.4                              | 5.0                                             | Description |
| ---------------------------------- | ----------------------------------------------- | ----------- |
| emqx_client_auth_success_anonymous | emqx_client_auth_anonymous                      | Renamed     |
| emqx_client_check_acl              | emqx_client_authorize counter                   | Renamed     |
| -                                  | emqx_mria_last_intercepted_trans                | New         |
| -                                  | emqx_mria_replicants                            | New         |
| -                                  | emqx_mria_server_mql                            | New         |
| -                                  | emqx_mria_weight                                | New         |
| emqx_routes_count                  | emqx_topics_count                               | Renamed     |
| emqx_routes_max                    | emqx_topics_max                                 | Renamed     |
| emqx_session_takeovered            | emqx_session_takenover                          | Renamed     |
| erlang_vm_ets_tables               | -                                               | Removed     |
| -                                  | erlang_vm_memory_dets_tables                    | New         |
| -                                  | erlang_vm_memory_ets_tables                     | New         |
| -                                  | erlang_vm_msacc_alloc_seconds_total             | New         |
| -                                  | erlang_vm_msacc_aux_seconds_total               | New         |
| -                                  | erlang_vm_msacc_bif_seconds_total               | New         |
| -                                  | erlang_vm_msacc_busy_wait_seconds_total         | New         |
| -                                  | erlang_vm_msacc_check_io_seconds_total          | New         |
| -                                  | erlang_vm_msacc_emulator_seconds_total          | New         |
| -                                  | erlang_vm_msacc_ets_seconds_total               | New         |
| -                                  | erlang_vm_msacc_gc_full_seconds_total           | New         |
| -                                  | erlang_vm_msacc_gc_seconds_total                | New         |
| -                                  | erlang_vm_msacc_nif_seconds_total               | New         |
| -                                  | erlang_vm_msacc_other_seconds_total             | New         |
| -                                  | erlang_vm_msacc_port_seconds_total              | New         |
| -                                  | erlang_vm_msacc_send_seconds_total              | New         |
| -                                  | erlang_vm_msacc_sleep_seconds_total             | New         |
| -                                  | erlang_vm_msacc_timers_seconds_total            | New         |
| -                                  | erlang_vm_statistics_dirty_cpu_run_queue_length | New         |
| -                                  | erlang_vm_statistics_dirty_io_run_queue_length  | New         |
| -                                  | erlang_vm_wordsize_bytes                        | New         |

## Gateway/Multi-Protocol Support

Clients of other protocols than MQTT (LwM2M, CoAP, STOMP, MQTT-SN) are no longer listed as MQTT clients on the dashboard **Connections** page and in the `GET /clients` API.
They can be found in **Extentions** -> **Gateways** section or listed with `GET /gateway/{name}/clients` API.

## Telemetry

The telemetry plugin (emqx_telemetry) has been removed, please configure it through the `telemetry {}` configuration item or in the Dashboard **System Settings** -> **Settings** page.
