# Migrate from V4

This section provides guidelines for migrating from EMQX 4.4 to 5.1 or newer.
EMQX 5 is not backward compatible with 4.x in management APIs and clustering APIs. However, most of the functionality is not significantly changed.

<!-- To learn about the new features we added to EMQX 5.1, check [New Features](../getting-started/new-features.md). -->

## Log

Compared with 4.x, the most significant change in the log is the format. In 4.x, the log text is primarily plain, aiming for good human readability. Starting from 5, logging style is changed to be more structured, so it is more machine (log indexer).
The `msg` field value is a snake_case message, making them more search-friendly. This format also helps log indexing tools to index the logs more effectively, for instance:

```bash
2022-06-29T16:58:53.235042+02:00 [info] foo: bar, msg: msg_for_human_to_read_but_also_easy_to_index
```

Find more details in [Logs](../observability/log.md#log-examples).

## Default listeners

By default, the following listeners are no longer provided:

| Listeners            | Description                        |
| -------------------- | ---------------------------------- |
| MQTT-TCP 11883       | Listeners for backend applications |
| Management-HTTP 8081 | For REST API, merged to port 18083 |

## Plugins

The previous official plugins have been migrated to EMQX as built-in functions. Custom plugins developed for version 4.x need to be adapted before they can be used in EMQX 5.1. The following is a comparison table between official plugins and built-in features.

| 4.x              | 5.1                                                       |
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
| emqx_bridge_mqtt | Data Bridge - MQTT Bridge                                 |
| emqx_web_hook    | Data Bridge - HTTP Server                                 |
| emqx_coap        | CoAP Gateway                                              |
| emqx_dashboard   | Dasboard                                                  |
| emqx_exhook      | ExHook                                                    |
| emqx_exproto     | ExProto Gateway                                           |
| emqx_lwm2m       | LwM2M Gateway                                             |
| emqx_sn          | MQTT-SN Gateway                                           |
| emqx_stomp       | STOMP Gateway                                             |
| emqx_lua_hook    | -                                                         |
| emqx_management  | Dashboard                                                 |
| emqx_prometheus  | Prometheus                                                |
| emqx_psk_file    | AuthN - PSK (`psk_authentication.enable = true`)          |
| emqx_recon       | Old features still available from CLI `emqx_ctl observer` |
| emqx_retainer    | Retain                                                    |
<!-- | emqx_telemetry   | Telemetry                                                 | -->

## HTTP API

Previously, **Applications** menu on Dashboard was used to manage API access credentials. Now **API Key** is used to create credentials. The credentials consist of API Key and Secret Key that can be respectively used as username and password in HTTP basic authentication. Secret Key is only displayed once when credentials are created, but cannot be obtained again later.

1. Port 8081 has been merged into port 18083, and the HTTP API is accessed through port 18083.
2. Can not use Dashboard username/password to access HTTP API. **Must** use API Key instead.
3. The basic path of API access has been switched from `/api/v4` to `/api/v5`. Call the API through port 18083 and path `/api/v5`.
4. The time-related fields will use the [RFC3339](https://datatracker.ietf.org/doc/html/rfc3339) format with time zones.

### Data Format Changes

When the response is successful, the business status code `code` will no longer be returned with the data, and the corresponding 4xx/5xx HTTP status code and error prompt will be returned when an error occurs.
Users can access `GET /error_codes` to get all possible error codes. The following is a comparative example of the response format:

**Successful response**

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

**Error response**

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

### Major API Changes

The API has undergone significant changes, and some APIs have been made compatible. The following is a comparison table of commonly used API changes.

:::tip
Compatibility Notes:

- Compatible: Use the old API path and parameters, or keep the old API.
- Partially compatible: The API path remains unchanged, but some API fields have changed.
- Incompatible: API paths and fields have changed.
:::

| 4.x                              | 5.1                                         | Compatibility        | Notes                        |
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

## MQTT Client Authentication and Authorization


**Change of concept**

Auth is referred to as **Authentication**, and ACL is also referred to as **Authorization**.

**Data Migration**

We have kept the authentication methods and supported data sources from version 4.x, with only a few changes to the way they are used. For most Authenticator/Authorizer, you can continue to use the 4.x database when upgrading to version 5.1 without having to migrate existing data.

### Order of authentication/authorization chain

When multiple authenticators or authorization checkers are enabled simultaneously, the checks are no longer performed according to the startup order but a fixed order per configuration. The order can be adjusted from Dashboard.

### Variable Interpolation Syntax

Previously, the Auth plugins could use `%u` syntax to construct variable placeholders, dynamically interpolating client information into SQL statements, Redis query commands, and HTTP requests.
Now EMQX uses the new syntax `${}`, such as `${username}`, `${clientid}`, which is consistent with the rule SQL.

For supported placeholders, please refer to:

- [Authentication Placeholders](../access-control/authn/authn.md#authentication-placeholders)
- [Authorization Placeholders](../access-control/authz/authz.md#placeholders-in-data-queries)

Usage example:

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

### MQTT Client Authentication

#### Remove the Anonymous Mechanism

Remove the `allow_anonymous` configuration item. All client connections are allowed by default. If **add and enable** any authenticator, EMQX will perform an authentication check for all new connections.

When a client matches the authentication data in all authenticators, the connection is rejected.

Remove the `bypass_auth_plugins` configuration item. When a listener needs to skip authentication, it can be set through the `listeners.{type}.{name}.enable_authn = true | false` configuration item.

#### Built-in database (Mnesia)

1. Mnesia is renamed to the built-in database;
2. Only two search methods are available: username-based or clientid-based;
3. The data format and REST API have changed. For more information, refer to `POST /authentication/{id}/users`.

#### HTTP

1. Use the JSON fields inside the response body instead of the HTTP response status codes to identify the authentication result;
2. Remove standalone super-user request. The super-user identity of the client is established through authentication in the response body.

**Success response status code:**

```shell
200 or 204
```

The authenticator will be ignored if the request fails or returns another status code.

**Success Response Body (JSON):**

| Name          | Type    | Required | Description             |
| ------------- | ------- | -------- | ----------------------- |
| result        | Enum    | true     | `allow | deny | ignore` |
| is_supseruser | Boolean | false    |                         |

```json
{
  "result": "allow",
  "is_supseruser": true
}
```

#### MySQL/PostgreSQL

1. The password field required in the query result is changed from `password` to `password_hash`. If you do not want to change the database column name, you can use `as` to complete the migration;
2. Remove standalone super-user query SQL. If you need to give clients super-user permissions, please ensure that the authentication SQL result contains the `is_superuser` field.

```sql
SELECT 
  password as password_hash,
  salt,
  is_superuser 
FROM mqtt_user 
  where username = ${username} LIMIT 1
```

#### MongoDB

Remove standalone super-user query. If you need to give clients super-user permissions, specify the `is_superuser_field` field.

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

1. Only supports [Redis Hashes](https://redis.io/docs/manual/data-types/#hashes) data structure and `HGET`, `HMGET` query commands. The commands must return `password_hash` or `password` (compatible with 4.x) as the password field name;
2. Remove standalone super-user query command. If you need to give clients super-user permissions, please add the `is_superuser` field to the Redis query command.

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
2. In `acl.conf` a few keyword syntax has been changed:

| 4.x    | 5.1.0    | Compatibility |
| ------ | -------- | ------------- |
| user   | username | Yes           |
| client | clientid | Yes           |
| pubsub | all      | No            |

```bash
# 4.x
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.
{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

# 5.1
{allow, {username, {re, "^dashboard$"}}, subscribe, ["$SYS/#"]}.
{allow, {ipaddr, "127.0.0.1"}, all, ["$SYS/#", "#"]}.
```

#### Built-in Database (Mnesia)

1. Mnesia renamed to the Built-in Database;
2. The data format and REST API have changed. For more information, please refer to `/authorization/sources/built_in_database/rules{/clients,/users}`.

The ACL data in version 4.x can be exported with `./bin/emqx_ctl data export`  command. Users may convert the data into the format compatible with version 5.1 and import it through the corresponding REST API.

#### MySQL/PostgreSQL

1. The `ipaddr/username/clientid` field is no longer required in the query result, and you need to change the query SQL;
2. The `access` field name is changed to `action`, and its data type is changed from integer to character or character enumeration.
3. The `allow` field name is changed to `permission`, and its data type is changed from integer to character or character enumeration.

The correspondence between 4.x integer values and 5.1 character/enumeration values is shown in the following tables.

**access/action field mapping**

| 4.x (int) | 5.1.0 (varchar/enum) | action              |
| --------- | -------------------- | ------------------- |
| 1         | subscribe            | subscribe           |
| 2         | publish              | publish             |
| 3         | all                  | subscribe & publish |

**allow/permission field mapping**

| 4.x (int) | 5.1.0 (varchar/enum) | permission |
| --------- | -------------------- | ---------- |
| 0         | deny                 | deny       |
| 1         | allow                | allow      |

#### MongoDB

1. MongoDB data source can be used for both allow and deny rules. Previously, only white list mode was supported, and it was required to set `authorization.no_match = deny`;
2. You need to select documents containing the `action`, `permission` and `topics` fields from MongoDB. For details, see [AuthZ-MongoDB](../access-control/authz/mongodb.md).

If you want to continue using the data from in 4.x, please make necessary migrations manually.

**Data example in 5.1**

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

1. Redis data source still only supports white list mode, which requires setting `authorization.no_match = deny`;
2. The `access` field name changes to `action`, and the data changes from numbers to action strings. The correspondence is shown in the table below.

| 4.x | 5.1.0     | action              |
| --- | --------- | ------------------- |
| 1   | subscribe | subscribe           |
| 2   | publish   | publish             |
| 3   | all       | subscribe & publish |

If you want to continue using the data from in 4.x, please make necessary migrations manually.

**Data example in 5.1**

```bash
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

| Name   | Type | Required | Description             |
| ------ | ---- | -------- | ----------------------- |
| result | Enum | true     | `allow | deny | ignore` |

```json
{
  "result": "deny"
}
```

## Rule Engine

Rule SQL is fully compatible with 4.x syntax, but the actions under the rule are split into built-in actions (republish, console) and data bridges (HTTP Server, MQTT Bridge).

## HTTP Server

The WebHook plugin (`emqx_web_hook`) is converted to a native feature, and it is now referred to as "HTTP Server" bridge.

## MQTT Bridge

The MQTT bridge plugin (emqx_bridge_mqtt) has been removed. Use the MQTT Sink and MQTT Source data bridge in Data Integration instead.

{% emqxee %}

## Offline messages

The [offline messages](https://docs.emqx.com/en/enterprise/v4.4/rule/offline_msg_to_redis.html) provided in EMQX 4.x are based on an external database. EMQX plans to provide native offline messages (based on the built-in database) in future versions, so the offline messages for the external database is no longer supported in version 5.

The upcoming native offline messaging feature will provide improved performance and reduce usage and maintenance costs. Stay tuned for more updates.

## Auto subscription (Server side subscriptions)

As of version 5.0.0, EMQX no longer provides [Auto subscription](https://docs.emqx.com/en/enterprise/v4.4/rule/get_subs_from_redis.html) (server side subscriptions) based on an external database.

{% endemqxee %}

## Prometheus

The old plugin named `emqx_prometheus` has been converted to a native feature in version 5. Prometheus scraping endpoint is enabled by default, and no authentication is required to scrap the metrics.

You can use `curl` command to inspect the metrics:

```bash
curl -f "http://127.0.0.1:18083/api/v5/prometheus/stats"
```

If you want to enable push-gateway, please refer to [Integrate with Prometheus](../observability/prometheus.md).

In addition to the way it is configured, Prometheus' metrics have changed:

| 4.4.x                              | 5.1                                             | Description |
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
