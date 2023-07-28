# Incompatible Changes between EMQX 5.1 and EMQX 4.4

EMQX 5.1 introduces several changes that may affect compatibility with older versions of EMQX. These breaking changes are also documented in the EMQX 5.1 release notes.

This document is intended for EMQX Enterprise customers and internal teams who are planning to upgrade from EMQX 4.x to EMQX 5.1 and want to understand potential issues they may encounter.

::: tip

It is recommended to upgrade to the latest version of 4.4 before proceeding with the upgrade to version 5.1.

:::

## Summary

Compared to EMQX 4.4, the upgrade to EMQX 5.1 introduces significant changes, particularly in terms of various concepts and mechanisms, surpassing the level of changes seen in previous upgrades from EMQX 2.x to EMQX 3.x and from EMQX 3.x to EMQX 4.x.

In conclusion, there are several points that need to be noted:

1. There are significant changes in **Configurations and HTTP APIs**. Existing configurations and code that rely on these interfaces will require migration.
2. **Core MQTT protocol functionalities**, including Pub/Sub, Retainer, and Shared Subscription, remain fully compatible with client programs. However, there may be slight changes in the management interface.
3. Other functionalities related to Authentication, Authorization, Data Integration, and Protocol Access will require migration based on their respective functions.
4. Certain concepts have undergone changes. For example, new versions of **Plugins** have been introduced, which differ significantly from the old versions. The concept of **Modules** in the old version has been completely removed.
5. Pay attention to the **removal of certain features**, such as `mcast` for Cluster discovery and `EMQX Bridges` as a resource type for data integration, among others.

## HTTP APIs

Previously, **Applications** in Dashboard was used to manage API access credentials. Now, **[API Key](../dashboard/system.md#api-key)** should be used to create credentials. The credentials consist of API Key and Secret Key that can be respectively used as username and password in HTTP basic authentication. Secret Key is only displayed once when credentials are created, but cannot be obtained again later.

- Port 8081 has been closed, and all API requests now use port 18083.
- Username/password can not be used to access HTTP API, API Key **must** be used instead.
- The basic path of API access has been switched from `/api/v4` to `/api/v5`. Call the API through port 18083 and path `/api/v5`.
- The time-related fields will use the [RFC3339](https://datatracker.ietf.org/doc/html/rfc3339) format with time zones.

### Data Format Changes

When the response is successful, the business status code `code` will no longer be returned with the data, and the corresponding 4xx/5xx HTTP status code and error prompt will be returned when an error occurs.
Users can access `GET /error_codes` to get all possible error codes.

::: details Comparative example of the response format:

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

:::

### Major API Changes

The API has undergone significant changes, and some APIs have been made compatible. The following is a comparison table of commonly used API changes.

::: tip Compatibility Notes

- Compatible: Use the old API path and parameters, or keep the old API.
- Partially compatible: The API path remains unchanged, but some API fields have changed.
- Incompatible: API paths and fields have changed.

:::

::: details API compatibility table

| 4.x                              | 5.x                                         | Compatibility        | Notes                        |
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

:::

## Configuration Files

- Format:

  - EMQX 4.x: Flat format with `path.to.key = value`.
  - EMQX 5.1: Supports nested format with `path{to{ key = value }}`.
- Sources:
  - EMQX 4.x:
    - Multiple files, such as `emqx.conf`, `listeners.conf`, `zones.conf`, etc.
    - Dynamic updates are stored in Mnesia. Once enabled, files cannot be updated for config changes.
  - EMQX 5.1:
    - `emqx.conf` for static configs.
    - `cluster.hocon` for dynamic updates.

## Log File Format

Log files in EMQX 5.1 is either the same flat log file format as in EMQX 4.4 or the structured JSON format, which is more indexer-friendly.

## Distribution and Cluster

- The `mcast` discovery strategy for cluster creation has been deprecated and is pending removal.
- Configuration for service discovery has been changed: cluster.discovery is changed to **cluster.discovery_strategy**.
- New feature: [cluster call](https://www.emqx.io/docs/en/v5.0/configuration/configuration-manual.html#cluster-call).
- Optional [eventual consistency](https://www.emqx.io/docs/en/v5.0/design/clustering.html#data-consistency) has been added to the internal DB. 

## MQTT

- In EMQX 5.0, MQTT clients can no longer see EMQX cluster as a single black box due to eventual consistency. Subscribers may or may not receive published messages from other clients after the subscription is confirmed.
- In EMQX 5.0, keepalive (receiving PING) requires a full MQTT control Packet instead of a few bytes.
- In EMQX 5.0, the TLS listener does not support `partial_chain` and `verify_peer_ext_key_usage`.
- The retry interval is 30s in version 5.0 but disabled (0) in version 4.4. The default config file in version 4.4 has a retry interval of 30s.

## MQTT over QUIC

MQTT over QUIC is a new feature in 5.0 but is disabled by default. Depending on the OS, it might require dynamic linking to `libatomic`.

## Authentication / Authorization

For a complete compatibility report, see [Authentication / Authorization v4.4 to v5.1 Compatibility](./auth-4.4-to-5.1-compatibility.md). 

All authentication/authorization providers now use placeholders instead of the previous formatting. In EMQX 5.x, placeholders are used, such as `${clientid}`, while in EMQX 4.x, `%c` was used. The available set of placeholders has also changed.

### **Change of Concept**

Auth is referred to as **Authentication**, and ACL is also referred to as **Authorization**.

### **Data Migration**

We have kept the authentication methods and supported data sources from version 4.x, with only a few changes to the way they are used. For most Authenticator/Authorizer, you can continue to use the 4.x database when upgrading to version 5.x without having to migrate existing data.

### Fixed Order of Execution

When multiple authenticators or authorization checkers are enabled simultaneously, the checks are no longer performed according to the startup order but a fixed configuration order. The execution order can be adjusted in the configuration file and Dashboard.

### Variable Interpolation Syntax

Previously, the Auth plugins could use `%u` syntax to construct variable placeholders, dynamically interpolating client information into SQL statements, Redis query commands, and HTTP requests.
Now EMQX uses the new syntax `${}`, such as `${username}`, `${clientid}`, which is consistent with the rule SQL.

For supported placeholders, please refer to:

- [Authentication Placeholders](../access-control/authn/authn.md#authentication-placeholders)
- [Authorization Placeholders](../access-control/authz/authz.md#placeholders-in-data-queries)

::: details Usage example

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

### Authentication Incompatibilities

- The superuser query has been removed. There should be a single query returning hashed credentials and the `is_superuser` flag.
- HTTP authentication
  - In EMQX 4.x, only the HTTP status code was used, and the body was discarded (e.g., `200` for `allow` and `403` for `deny`).
  - In EMQX 5.0, HTTP authentication has been redesigned to make use of the HTTP body. Refer to [HTTP service authentication](https://docs.emqx.com/en/enterprise/v5.0/access-control/authn/http.html) for more detailed information.
- SCRAM authentication
  - The SHA1 hashing mode (the only one available in version 4.4) is not available. SHA256/SHA512 hashes are used.
- Built-in database
  - Credentials cannot be provided directly in the config file.
  - The credential table now keeps either username or clientid types of credentials, not both.
- Redis
  - Only `HMGET` and `HGET` commands are supported.
  - `query_timeout` is unavailable anymore.
- PostgreSQL
  - `query_timeout` is not used anymore.
  - `encoding` is not used anymore.

### Authorization

- File-based

  - The ACL rule `{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"\]}` is not working in EMQX 5.1. Refer to issue [#10735](https://github.com/emqx/emqx/issues/10735) for more information.

- HTTP 

  - In EMQX 4.x, HTTP status code was used, but the body was discarded (except for the "ignore" case). For example, `200` for `allow` and `403` for `deny`.
  - In EMQX 5.0, HTTP authorization has been redesigned to make use of the HTTP body. Refer to [Use HTTP Service](https://www.emqx.io/docs/en/v5.0/access-control/authz/http.html) for more information.

- MySQL, PostgreSQL

  - The storage schema has changed.
  - In EMQX 4.4, the query should fetch rows with columns `[Allow, IpAddr, Username, ClientId, Access, Topic]` under any name but exactly in this order.
  - In EMQX 5.1, the query should fetch rows with columns `permission, action, topic` in any order but under exactly these names. The "who" part (`IpAddr, Username, ClientId`) is now suggested to be a part of the query.

- MongoDB

  - The storage schema has changed.

  - In EMQX 4.4, resulting documents should contain topics lists by action key, like in Redis or JWT: 

    ```
    {
      "publish": ["t1", "t2"],
      "subscribe": ["t3", "t4"],
      "pubsub": ["t5", "t6"]
    }
    ```

  - In EMQX 5.1, the documents should contain individual rules with `permission, action, topics` fields. Note that `topics` should be an array of topics.

## Rule Engine

Rule SQL is fully compatible with 4.x syntax, but the actions under the rule are split into built-in actions (republish, console) and data bridges (HTTP Server, MQTT Bridge).

## Data Integration

In EMQX 5.1, there have been conceptual improvements to data integration:

- Full compatibility for Rule and SQL templates is ensured.
- Most of the configuration item names and formats for resources and bridging have changed.
- The previous **Rule** -> **Action** -> **Resources** process has been modified to **Rule** -> **Bridges**.
- The functionality of **Modules/Message Publish** has been moved into the Bridges.
- The [**Save Offline Message**](https://docs.emqx.com/en/enterprise/v4.4/rule/offline_msg_to_redis.html)**,** [**Get Subscriptions**](https://docs.emqx.com/en/enterprise/v4.4/rule/get_subs_from_redis.html), and EMQX Bridge features have been removed.
- Tablestore, DolphinDB, Lindorm, and SAP Event Mesh data bridges are not supported yet.
- The MQTT bridge plugin (`emqx_bridge_mqtt`) has been removed. Use the built-in MQTT data bridge in Data Integration instead.

For a complete compatibility report, see [Data Integration Incompatibility Between EMQX 5.1 with EMQX 4.4](./data-integration-4.4-to-5.1-incompatibility.md).

## HTTP Server

The WebHook plugin (`emqx_web_hook`) is converted to a native feature, and it is now referred to as "HTTP Server" bridge.

{% emqxee %}

## Offline Messages

The [offline messages](https://docs.emqx.com/en/enterprise/v4.4/rule/offline_msg_to_redis.html) provided in EMQX 4.x are based on an external database. EMQX plans to provide native offline messages (based on the built-in database) in future versions, so the offline messages for the external database is no longer supported in version 5.x.

The upcoming native offline messaging feature will provide improved performance and reduce usage and maintenance costs. Stay tuned for more updates.

## Auto Subscription (Server Side Subscriptions)

As of version 5.0.0, EMQX no longer provides [Auto subscription](https://docs.emqx.com/en/enterprise/v4.4/rule/get_subs_from_redis.html) (server side subscriptions) based on an external database.

{% endemqxee %}

## Data Persistence

- [MQTT Message Persistence](https://docs.emqx.com/en/enterprise/v4.4/backend/backend.html#mqtt-message-persistence) is not implemented in EMQX 5.0 and 5.1. It is planned for later versions.

## Prometheus

The old plugin named `emqx_prometheus` has been converted to a native feature in version 5.x. Prometheus scraping endpoint is enabled by default, and no authentication is required to scrape the metrics.

You can use `curl` command to inspect the metrics:

```bash
curl -f "http://127.0.0.1:18083/api/v5/prometheus/stats"
```

If you want to enable push-gateway, please refer to [Integrate with Prometheus](../observability/prometheus.md).

::: details Changes in Prometheus metrics

| 4.4.x                              | 5.x                                             | Description |
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

:::

## Gateway

In EMQX 4.x, various protocols can be configured through corresponding Plugins and Modules (only for the enterprise edition). However, in EMQX 5.0, we have introduced a new concept called Gateway.

Clients of other protocols than MQTT (LwM2M, CoAP, STOMP, MQTT-SN) are no longer listed as MQTT clients on the dashboard **Connections** page and in the `GET /clients` API. They can be found in **Management** -> **Gateways** or listed with `GET /gateway/{name}/clients` API.

- It is **completely incompatible** from the configuration and management approaches perspective. EMQX 5.0 has a brand new configuration format and management approach.
  - New configuration format.
  - Added a new HTTP API for managing gateways and clients of the gateway.
  - Each gateway has its own independent authentication method.
- JT/T 808, GB/T 32960, TCP and OCPP are **not supported on EMQX 5.1**.
- Stomp, MQTT-SN, and ExProto protocols are fully compatible with version 4.x and have even more improvements in functionality.
- Although the gateway for CoAP and LwM2M has been implemented in version 5.1.0, it is not recommended for use in a production environment due to incomplete design and implementation.

For the complete compatibility report, see [Gateway Incompatibility between e4.4  and e5.1](./gateway-4.4-to-5.1-incompatibility.md).

## Observability 

- Prometheus: The metric `erlang_vm_statistics_run_queues_length_total` has been renamed to `erlang_vm_statistics_run_queues_length`.
- StatsD: Removed.
