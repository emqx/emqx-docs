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
5. Pay attention to the **removal of certain features**, such as `mcast` for Cluster discovery and `EMQX Bridges` for Data integration, among others.

## HTTP APIs

- Port 8081 has been closed, and all API requests now use port 18083.
- It is no longer supported to use the username of the Dashboard, such as admin:public, to request the HTTP API. Instead, an API Key must be used.

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

Log files follow the same format, but the log content style has slight differences. The log format in EMQX 5.1 is more indexer-friendly.

## Distribution and Cluster

- The `mcast` discovery strategy for cluster creation has been deprecated and is pending removal.
- Configuration for service discovery has been changed: cluster.discovery is changed to **cluster.discovery_strategy**.
- New feature: [cluster call](https://www.emqx.io/docs/en/v5.0/configuration/configuration-manual.html#cluster-call).
- Optional [eventual consistency](https://www.emqx.io/docs/en/v5.0/design/clustering.html#data-consistency) has been added to the internal DB. 

## MQTT

- The `$queue` prefix for shared subscriptions is not supported.
- In EMQX 5.0, MQTT clients can no longer see EMQX cluster as a single black box due to eventual consistency. Subscribers may or may not receive published messages from other clients after the subscription is confirmed.
- In EMQX 5.0, keepalive (receiving PING) requires a full MQTT control Packet instead of a few bytes.
- In EMQX 5.0, the TLS listener does not support `partial_chain` and `verify_peer_ext_key_usage`.
- The retry interval is 30s in version 5.0 but disabled (0) in version 4.4. The default config file in version 4.4 has a retry interval of 30s.

## MQTT over QUIC

MQTT over QUIC is a new feature in 5.0 but is disabled by default. Depending on the OS, it might require dynamic linking to `libatomic`.

## Authentication / Authorization

For a complete compatibility report, see [Authentication / Authorization v4.4 to v5.1 Compatibility](./auth-4.4-to-5.1-compatibility.md). 

All authentication/authorization providers now use placeholders instead of the previous formatting. In EMQX 5.x, placeholders are used, such as `${clientid}`, while in EMQX 4.x, `%c` was used. The available set of placeholders has also changed.

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

## Data Integration

In EMQX 5.1, there have been conceptual improvements to data integration:

- Full compatibility for Rule and SQL templates is ensured.
- Most of the configuration item names and formats for resources and bridging have changed.
- The previous **Rule** -> **Action** -> **Resources** process has been modified to **Rule** -> **Bridges**.
- The functionality of **Modules/Message Publish** has been moved into the Bridges.
- The [**Save Offline Message**](https://docs.emqx.com/en/enterprise/v4.4/rule/offline_msg_to_redis.html)**,** [**Get Subscriptions**](https://docs.emqx.com/en/enterprise/v4.4/rule/get_subs_from_redis.html), and EMQX Bridge features have been removed.
- Tablestore, DolphinDB, Lindorm, and SAP Event Mesh are not supported.

For a complete compatibility report, see [Data Integration Incompatibility Between EMQX 5.1 with EMQX 4.4](./data-integration-4.4-to-5.1-incompatibility.md).

## Data Persistence

- [MQTT Message Persistence](https://docs.emqx.com/en/enterprise/v4.4/backend/backend.html#mqtt-message-persistence) is not implemented in EMQX 5.0 a5.1.

## Gateway

In EMQX 4.x, various protocols can be configured through corresponding Plugins and Modules (only for the enterprise edition). However, in E5.0, we have introduced a new concept called Gateway.

- It is **completely incompatible** from the configuration and management approaches perspective. E 5.0 has a brand new configuration format and management approach.
  - New configuration format.
  - Added a new HTTP API for managing gateways and clients of the gateway.
  - Each gateway has its own independent authentication method.
- JT/T 808, GB/T 32960, TCP and OCPP are **not supported on EMQX 5.1.**.
- Stomp, MQTT-SN, and ExProto protocols are fully compatible with version 4.x and have even more improvements in functionality.
- Although the gateway for CoAP and LwM2M has been implemented in version 5.1.0, it is not recommended for use in a production environment due to incomplete design and implementation.

For the complete compatibility report, see [Gateway Incompatibility between e4.4  and e5.1](./gateway-4.4-to-5.1-imcompatibility.md).

## Observability 

- Prometheus: The metric `erlang_vm_statistics_run_queues_length_total` has been renamed to `erlang_vm_statistics_run_queues_length`.
- StatsD: Removed.
