# Upgrade Guide

This document provides guidelines for migrating from EMQX 4.x to 5.0.
EMQX 5.0 is not backward compatible with 4.x in management APIs and clustering APIs, however most of the functionalities are not significantly changed.

To learn about the new features we have added in EMQX 5.0, checkout [5.0.0 Release](https://www.emqx.com/en/changelogs/broker/5.0.0).

## Log

Comparing with 4.x, the biggest change in log is the format.
In 4.x, the log text is mostly free text aiming for good human readability.
Starting from 5.0, we moved to structured logging without losing readability.
For instance, most of the log fields uses underscore as word separators,
which makes it in general more search-friendly, also helps log indexing tools
to index the logs more effectively.

`2022-06-29T16:58:53.235042+02:00 [info] foo: bar, msg: msg_for_human_to_read_but_also_easy_to_index`

Find more details in [Log & Trace](../observability/log.md).

## Default listeners

By default, the following listeners will no longer be provided:

| Listeners            | Description                        |
| -------------------- | ---------------------------------- |
| MQTT-TCP 11883       | Listeners for backend applications |
| Management-HTTP 8081 | For REST API, merged to port 18083 |

## Plugins

The previous official plug-ins have been migrated to EMQX as built-in functions. Plug-ins developed by users on version 4.x need to be adapted before they can be used in EMQX 5.0. The following is a comparison table between official plug-ins and existing functions:

| 4.x              | 5.0                                          |
| ---------------- | -------------------------------------------- |
| emqx_auth_http   | AuthN/AuthZ - HTTP data source               |
| emqx_auth_jwt    | AuthN - JWT                                   |
| emqx_auth_mnesia | AuthN/AuthZ - Built-in database              |
| emqx_auth_mongo  | AuthN/AuthZ - MongoDB data source            |
| emqx_auth_mysql  | AuthN/AuthZ - MySQL data source              |
| emqx_auth_pgsql  | AuthN/AuthZ - PostgreSQL data source         |
| emqx_auth_redis  | AuthN/AuthZ - Redis data source              |
| emqx_sasl        | AuthN/AuthZ - MQTT 5 Enhanced Authentication |
| emqx_auth_ldap   | -                                            |
| emqx_rule_engine | Data Integration                             |
| emqx_bridge_mqtt | Data Bridge - MQTT Sink/MQTT Source          |
| emqx_web_hook    | Data Bridge - WebHook                        |
| emqx_coap        | CoAP Gateway                                 |
| emqx_dashboard   | Dasboard                                     |
| emqx_exhook      | ExHook                                       |
| emqx_exproto     | ExProto Gateway                              |
| emqx_lwm2m       | LwM2M Gateway                                |
| emqx_sn          | MQTT-SN Gateway                              |
| emqx_stomp       | STOMP Gateway                                |
| emqx_lua_hook    | -                                            |
| emqx_management  | -                                            |
| emqx_prometheus  | Prometheus                                   |
| emqx_psk_file    | AuthN - PSK (`psk_authentication.enable = true`)  |
| emqx_recon       | Old features still available from CLI `emqx_ctl observer`|
| emqx_retainer    | Retain                                       |
| emqx_telemetry   | Telemetry                                    |

## HTTP API

Previously, "Appication" was used to manage API access credentials, which has now been renamed to "API Key". And Secret Key is only returned once when it is successfully created, but cannot be obtained again later.

Port 8081 has been merged into port 18083, and the basic path of API access has been switched from `/api/v4` to `/api/v5`, please call the API through this port and path.

String timestamps are now in [RFC3339](https: //datatracker.ietf.org/doc/html/rfc3339) format.

### Data Format Changes

When the response is successful, the business status code `code` will no longer be returned with the data, and the corresponding 4xx/5xx HTTP status code and error prompt will be returned when an error occurs.
Users can access `GET /error_codes` to get all possible error codes, the following is a comparative example of the response format:

```shell
# 4.x successful response
## HTTP StatusCode = 200
GET /rules/my_rule
{ "code": 0, "data": { ... } }

# 4.x error response
## HTTP StatusCode = 200
GET /rules/my_rule
{ "code": 404, "message": "Not Found" }

# 5.0 successful response
## HTTP StatusCode = 200
GET /rules/my_rule
{ ... }

# 5.0 error response
## HTTP StatusCode = 404
GET /rules/my_rule
{ "code": "NOT_FOUND", "message": "Rule Id Not Found" }
```

### Major API Changes

The API has undergone major changes, and some APIs have been made compatible. The following is a comparison table of commonly used API changes.

:::tip
Compatibility Notes:

- Compatibility: Use the old API path and parameters, or keep the old API
- Partially compatible: API path remains unchanged, but some API fields have changed
- Incompatible: API paths and fields have changed
:::

| 4.x                              | 5.0                                     | Compatibility        | Notes                   |
| -------------------------------- | --------------------------------------- | -------------------- | ----------------------- |
| **Publish/Subscribe**            |                                         |                      |                         |
| `POST /mqtt/publish`             | `POST /publish`                         | Compatibility        |                         |
| `POST /mqtt/publish_batch`       | `POST /publish_bulk`                    | Compatibility        |                         |
| `POST /mqtt/subscribe`           | `POST /clients/{clientid}/subscribe`    | Compatibility        |                         |
| `POST /mqtt/subscribe_batch`     | -                                       | Compatibility        |                         |
| `POST /mqtt/unsubscribe`         | `POST /clients/{clientid}/unsubscribe`  | Compatibility        |                         |
| `POST /mqtt/unsubscribe_batch`   | -                                       | Compatibility        |                         |
| **Clients/Topics/Subscriptions** |                                         |                      |                         |
| `GET /clients`                   | `GET /clients`                          | Partially compatible |                         |
| `GET /routes{/topic}`            | `GET /topics{/topic}`                   | Incompatible         | routes rename to topics |
| `GET /subscriptions`             | `GET /subscriptions`                    | Partially compatible |                         |
| `GET /subscriptions/{clientid}`  | `GET /clients/{clientid}/subscriptions` | Incompatible         |                         |
| **Node/Stats/Metrics**           |                                         |                      |                         |
| `GET /nodes`                     | `GET /nodes`                            | Partially compatible |                         |
| `GET /brokers`                   | -                                       | Incompatible         | merged to `GET /nodes`  |
| `GET /stats`                     | `GET /stats`                            | Partially compatible |                         |
| `GET /metrics`                   | `GET /metrics`                          | Partially compatible |                         |
| **Users/Alarms**                 |                                         |                      |                         |
| `GET /users`                     | `GET /users`                            | Partially compatible |                         |
| `GET /alarms{/activated}`        | `GET /alarms?activated={true,false}`    | Incompatible         |                         |
| `GET /alarms{/deactivated}`      | `GET /alarms?activated={true,false}`    | Incompatible         |                         |

## Auth/ACL

The Auth and ACL plugins (emqx_auth_*) have been removed, and the corresponding functions are built into EMQX, and users can configure them through Dashboard or configuration files.

Auth was renamed **Authenticate** and ACL was renamed **Authorization**. Now they are independent of each other.

Version 5.0 supports the authentication methods and most data sources in 4.x, but there are certain adjustments in the usage.

### Fixed order of execution

When multiple authenticators or authorization checkers are enabled at the same time, the checks are no longer performed according to the startup order but a fixed configuration order, which can be adjusted in the configuration file and Dashboard.

### Replace variable extraction syntax

Previously, the Auth plugin could use `%u` similar syntax to construct placeholders to extract variables, dynamically splicing client information into SQL statements, Redis query commands and HTTP requests.
Now EMQX use a new syntax `${}`, such as `${username}`, `${clientid}`, which is consistent with the rule SQL.

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

#### Remove anonymous mechanism

Remove the `alow_anonymous` configuration item, all client connections are allowed by default.
If **add and enable** any authenticator, EMQX will perform authentication check on all new connections.

Remove the `bypass_auth_plugins` configuration item. When a listener needs to disable authentication, it can be set through the `listeners.{type}.{name}.enable_authn = true | false` configuration item.

#### Built-in database (Mnesia)

1. Mnesia renamed to built-in database;
2. Only one search method can be selected: based on user name or based on client ID;
3. The data format and REST API have changed. For more information, please refer to `POST /authentication/{id}/users`.

Users can use the data import API to import data from older versions into EMQX 5.0, see `POST /authentication/{id}/import_users` for details.

#### HTTP

1. The status code of the failure response is changed to `400 <= statusCode < 500`, the status code of the successful authentication response is `statusCode = 200` or `statusCode = 204`, and any other status code or request failure is `ignore`;
2. Remove the super user query configuration. If you need the super user function, please set it through JSON in the response body of successful authentication:

```json
# The key is is_supseruser, value can be true/false, 1/0 (other data or not set to false)
{
  "is_supseruser": true
}
```

#### MySQL/PostgreSQL

1. The password field required in the query result is changed from `password` to `password_hash`, if you do not want to change the database column name, you can use `as` to complete the migration;
2. Remove the superuser query SQL. If you need superuser, please make sure that the authentication SQL result contains the `is_superuser` field.

```sql
SELECT password as password_hash, salt, is_superuser FROM mqtt_user where username = ${username} LIMIT 1
```

#### MongoDB

1. Remove the superuser query. If you need a superuser, please specify the `is_superuser_field` field.

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

1. Only supports [Redis Hashes](https://redis.io/docs/manual/data-types/#hashes) data structure and `HGET`, `HMGET` query commands, must use `password` or `password_hash` as password field name;
2. Remove the superuser query, if you need the superuser, please add the `is_superuser` field to the Redis query command.

```shell
# bad
GET emqx_user:${username}
# bad
HMGET emqx_user:${username} passwd

# good
HMGET emqx_user:${username} password

# good
HMGET emqx_user:${username} password is_superuser
```

#### LDAP

Not supported for now.

#### JWT

No changes.

### ACL

#### ACL File

1. Remove the `acl_file` configuration, the file-based ACL (acl.conf) will be used as one of the authorization checkers and added to EMQX by default;
2. The `acl.conf` file is moved from the `etc` directory to the `data/authz` directory, please ensure that EMQX has read and write permissions for this file;
3. `acl.conf` data file syntax has changed

| 4.x    | 5.0.0    | Compatibility |
| ------ | -------- | ------------- |
| user   | username | Yes           |
| client | clientid | Yes           |
| pubsub | all      | No            |

#### Built-in database (Mnesia)

1. Mnesia renamed to built-in database;
2. The data format and REST API have changed. For more information, please refer to `POST /authorization/built_in_database/clientid`.

The data export command `./bin/emqx_ctl data export` in 4.x contains ACL data, and users can process the data into the format required by 5.0 and import it through the corresponding REST API.

#### MySQL/PostgreSQL

1. The `ipaddr/username/clientid` field is no longer required in the query result;
2. The `access` field name is changed to `action`, the data type is changed from integer to character or character enumeration, and the numerical correspondence is shown in the following table;
3. The `allow` field name is changed to `permission`, the data type is changed from integer to character or character enumeration, and the numerical correspondence is shown in the following table.

**access-action field mapping**

| 4.x (int) | 5.0.0 (varchar/enum) | action              |
| --------- | -------------------- | ------------------- |
| 1         | subscribe            | subscribe           |
| 2         | publish              | publish             |
| 3         | all                  | subscribe & publish |

**allow-permission field mapping**

| 4.x (int) | 5.0.0 (varchar/enum) | permission |
| --------- | -------------------- | ---------- |
| 0         | deny                 | deny       |
| 1         | allow                | allow      |

#### MongoDB

1. MongoDB data source can be used in black/white list mode. Previously, only white list mode was supported. It is required to set `acl_nomatch = deny`;
2. You need to query the data list containing the `action` `permission` `topics` fields from MongoDB. For details, see [AuthZ-MongoDB](../security/authz/mongodb.md).

If you want to continue using the data in 4.x, please migrate the adaptation manually.

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

1. Redis data source still only supports whitelist mode, which requires setting `acl_nomatch = deny`;
2. The `access` field name changes to `action`, and the data changes from numbers to action strings. The corresponding relationship is shown in the table below.

| 4.x | 5.0.0     | action              |
| --- | --------- | ------------------- |
| 1   | subscribe | subscribe           |
| 2   | publish   | publish             |
| 3   | all       | subscribe & publish |

If you want to continue using the data in 4.x, please migrate the adaptation manually.

**Data example in 5.0**

```
HSET mqtt_acl:emqx_u t/# subscribe
HSET mqtt_acl:emqx_u # all
HSET mqtt_acl:emqx_u a/1 publish
```

#### HTTP

No changes.

## Rule Engine

The rules engine has been renamed [Data Integration](../data-integration/introduction.md), which includes rules and data bridge.

Rule SQL is fully compatible with 4.x syntax, but the actions under the rule are split into built-in actions (republish, console) and data bridges (WebHook, MQTT Sink, MQTT Source), so as to realize the reuse of actions.

## WebHook

The WebHook plugin (emqx_web_hook) has been removed, please use the WebHook data bridge in Data Integration instead.

## MQTT Bridge

The MQTT bridge plugin (emqx_bridge_mqtt) has been removed, please use the MQTT Sink and MQTT Source data bridge in Data Integration instead.

## Prometheus

The Prometheus bridge plugin (emqx_prometheus) has been removed, please configure it through the `prometheus {}` configuration item or the Dashboard **Dashboard** page.

Changes:

| 4.4.4                              | 5.0                                             | Description |
| ---------------------------------- | ----------------------------------------------- | ----------- |
| emqx_client_auth_success_anonymous | emqx_client_auth_anonymous                      | Renamed     |
| emqx_client_check_acl              | emqx_client_authorize counter                   | Renamed     |
| -                                  | emqx_mria_last_intercepted_trans                | New         |
| -                                  | emqx_mria_replicants                            | New         |
| -                                  | emqx_mria_server_mql                            | New         |
| -                                  | emqx_mria_weight                                | New         |
| emqx_routes_count                  | -                                               | Removed     |
| emqx_routes_max                    | -                                               | Removed     |
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

## Gateway/Multi-Protocol Supports

Clients of other protocols (LwM2M, CoAP, STOMP, MQTT-SN) will no longer be mapped as MQTT clients and cannot be obtained through the Dashboard client page and the `GET /clients` API.
Users can go to the gateway page details page or get it through the `GET /gateway/{name}/clients` API.

## Telemetry

The telemetry plugin (emqx_telemetry) has been removed, please configure it through the `telemetry {}` configuration item or the Dashboard **System Settings** -> **Settings** page.
