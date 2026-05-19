# EMQX Enterprise Version 6

## 6.2.0

*Release Date: 2026-03-31*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.2.0.

### Enhancements

#### AI Interoperability

- [#16840](https://github.com/emqx/emqx/pull/16840) Implemented Agent-to-Agent (A2A) Card Registry. This feature enables autonomous AI agents to discover and collaborate through a standardized, event-driven MQTT 5.0 mechanism.

- [#16958](https://github.com/emqx/emqx/pull/16958) Added focused `/api-spec.md` and `/api-spec.html` endpoints to support drill-down discovery of EMQX HTTP API context, especially for AI agents and other tools that benefit from fetching only the relevant API slices instead of a single bloated spec.

#### Core MQTT Functionalities

- [#16612](https://github.com/emqx/emqx/pull/16612) Introduced the `emqx_setopts` application for server-side option updates via `$SETOPTS` topics, including keepalive control and warning suppression for unknown `$SETOPTS/*` publishes.

- [#16887](https://github.com/emqx/emqx/pull/16887) Added optional subscription message filters, controlled by `mqtt.subscription_message_filter`.

  When enabled, clients can subscribe with a `?` suffix (for example, `sensor/+/temperature?location=roomA&value>25`), and EMQX will deliver only messages whose MQTT 5.0 User Properties satisfy the filter expression. When disabled, `?` is treated as part of the topic filter text and no additional filtering is applied.

  Messages dropped due to a subscription filter mismatch are reported via the existing `delivery.dropped` event with reason `subscription_filter`, and counted by the new `delivery.dropped.filter` metric.

- [#16929](https://github.com/emqx/emqx/pull/16929) Introduced two new limiter kinds: `delivery_messages` and `delivery_bytes`. Unlike the existing `messages` and `bytes` limiters (which limit messages published by a single client), these new limiters throttle messages received by a single client from any source. When a limit is reached, QoS 0 messages are dropped and QoS 1/2 messages are queued internally with a scheduled retry. The retry interval is derived from the limiter configuration.

  These new limiters are only supported for memory sessions (`durable_sessions.enable = false`). Default values are unlimited to maintain backward compatibility.

- [#16779](https://github.com/emqx/emqx/pull/16779) Improved handling of malformed first packets by classifying them as invalid CONNECT packets and providing more informative protocol hints in logs.

#### Data Integration

- [#16589](https://github.com/emqx/emqx/pull/16589) Updated the `jq` library used in the Rule Engine to version 1.8.1.

  Note that jq 1.8.1 introduces several subtle breaking changes compared to 1.6.1:

  - An empty string as a jq program is now an error; use `"."` instead. ([jq#2790](https://github.com/jqlang/jq/pull/2790))
  - String functions `indices/1`, `index/1`, and `rindex/1` now use code point indices instead of byte indices; use `utf8bytelength/0` to get the byte index. ([jq#3065](https://github.com/jqlang/jq/pull/3065))
  - `tonumber/0` rejects numbers with leading or trailing whitespace; use `trim/0` before calling `tonumber/0`. ([jq#3055](https://github.com/jqlang/jq/pull/3055), [jq#3195](https://github.com/jqlang/jq/pull/3195))
  - `last(empty)` now yields no output, consistent with `first(empty)`. ([jq#3179](https://github.com/jqlang/jq/pull/3179))
  - `limit/2` errors on a negative count instead of silently accepting it. ([jq#3181](https://github.com/jqlang/jq/pull/3181))
  - Tcl-style multiline comments are now supported, which may subtly affect parsing of existing code. ([jq#2989](https://github.com/jqlang/jq/pull/2989))
  - Decimal numbers are now converted to binary64 (double) instead of decimal64. ([jq#2949](https://github.com/jqlang/jq/pull/2949))
  - `nth/2` emits empty on an out-of-range index instead of erroring. ([jq#2674](https://github.com/jqlang/jq/pull/2674))
  - String multiplication by 0 or a value less than 1 now emits an empty string. ([jq#2142](https://github.com/jqlang/jq/pull/2142))

- [#16634](https://github.com/emqx/emqx/pull/16634) Added support for GET requests in external HTTP schema validation. Schema registry entries can now specify the HTTP method, with POST remaining the default.

- [#16647](https://github.com/emqx/emqx/pull/16647) In GreptimeDB and EMQX Tables actions, integer values without an `i` or `u` suffix are now automatically cast to `float64` before being sent to the database.

  In InfluxDB Write Syntax, float is the default numeric type and integers must be explicitly annotated. Previously, EMQX would interpret a non-annotated integer as a one-character string, causing insertion to fail if the target column was of type float.

- [#16707](https://github.com/emqx/emqx/pull/16707) EMQX supports data integration with Azure Event Grid.

- [#16750](https://github.com/emqx/emqx/pull/16750) Added support for Workload Identity Federation (WIF) authentication in GCP connectors (GCP PubSub Producer and Consumer, BigQuery) via Service Account Impersonation. Currently, only OIDC workload identity pool providers using the Client Credentials grant type are supported.

- [#16773](https://github.com/emqx/emqx/pull/16773) When using the MQTT connector with SSL enabled, the Server Name Indication (SNI) field is now automatically populated with the server's hostname if left unset.

- [#16893](https://github.com/emqx/emqx/pull/16893) EMQX supports data integration with QuasarDB.

- [#16962](https://github.com/emqx/emqx/pull/16962) Improved Kafka source polling behavior. Fetch requests now wait briefly for data instead of immediately returning empty batches when no records are available. This reduces unnecessary polling delays and helps Kafka consumers receive new records more consistently.

#### Access Control

- [#16597](https://github.com/emqx/emqx/pull/16597) Improved handling of disallowed and quoted variables in SQL templates for MySQL and PostgreSQL authentication and authorization.

- [#16616](https://github.com/emqx/emqx/pull/16616) Added new configurations to the SSO OIDC backend to support specifying `jq` expressions for extracting the desired role and namespace when creating new Dashboard users.

- [#16759](https://github.com/emqx/emqx/pull/16759) Added `timestamp_s` and `timestamp_ms` functions to Variform expressions to retrieve the current system time in seconds and milliseconds respectively (for example, to populate additional client attributes on connection).

- [#16817](https://github.com/emqx/emqx/pull/16817) Added REST API endpoints to reset authentication and authorization metrics counters:
  - `POST /authentication/:id/metrics/reset` resets counters for a specific authenticator.
  - `POST /authorization/sources/:type/metrics/reset` resets counters for a specific authorization source.

#### Management

- [#16958](https://github.com/emqx/emqx/pull/16958) Added `emqx ctl api_keys` CLI commands to list, show, add, delete, enable, and disable API keys from the command line.

#### Plugins

- [#16849](https://github.com/emqx/emqx/pull/16849) Added cookie-based authentication as a fallback for plugin API endpoints. Plugin UI iframes served by the Dashboard can now authenticate via the `emqx_auth` cookie when no `Authorization` header is present. This only applies to `/api/v5/plugin_api/...` paths.

#### Gateway

- [#16734](https://github.com/emqx/emqx/pull/16734) Added ordered `token`, `nkey`, and `jwt` internal authentication methods to the NATS Gateway to reduce the authentication feature gap with NATS Server.

#### Deployment and Security

- [#16653](https://github.com/emqx/emqx/pull/16653) Made the Erlang distribution listener address configurable via `node.dist_bind_address`.

  For example: `node.dist_bind_address = "10.0.1.5"`. Previously this required configuration in `vm.args` as `-kernel inet_dist_use_interface {10,0,1,5}`.

- [#16888](https://github.com/emqx/emqx/pull/16888) Refreshed the default TLS certificate bundle shipped with EMQX packages for local development and testing. The new server certificate is issued for `localhost` and loopback addresses only (`localhost`, `127.0.0.1`, `::1`). These default certificates must not be used in production.

- [#16916](https://github.com/emqx/emqx/pull/16916) The `emqx_cert_expiry_at` Prometheus metric now takes into account the expiry dates of certificates belonging to managed certificate bundles used in MQTT listeners.

#### Performance

- [#16500](https://github.com/emqx/emqx/pull/16500) Optimized idle memory usage and reduced the cost of maintaining rate-based metrics. Note: 5-minute average rate metrics are now computed as EWMAs rather than exact rolling averages.

- [#16547](https://github.com/emqx/emqx/pull/16547) Disabled TLS 1.2 session reuse by default to reduce TLS handshake overhead. The TLS 1.2 session cache is limited to 1000 entries and is local to each node, resulting in a very low reuse rate especially in large clusters with many connections.

- [#16794](https://github.com/emqx/emqx/pull/16794) Enabled node-level authentication and authorization caches by default. This reduces repeated backend lookups for repeated client checks, improving authentication and authorization performance in common deployments.

- [#16829](https://github.com/emqx/emqx/pull/16829) Optimized the NATS Gateway publish hot path to reduce per-message overhead in frame parsing, subject/topic handling, metrics updates, and ACK/message build steps.

- [#16911](https://github.com/emqx/emqx/pull/16911) Reduced Prometheus metrics collection overhead by avoiding repeated queries of Mria statistics.

- [#16550](https://github.com/emqx/emqx/pull/16550) Stopped caching subscribe ACL check results. MQTT subscription is mostly done once per connection lifecycle, so caching subscribe ACL results provides little benefit and wastes RAM.

### Bug Fixes

#### Core MQTT Functionalities

- [#16721](https://github.com/emqx/emqx/pull/16721) Fixed QoS 2 duplicate handling when `await_rel_timeout` has expired. Previously, if a client retried a QoS 2 PUBLISH with `DUP=1` after the broker had expired the pending PUBREL state (default 300 seconds), the message could be published to subscribers again. EMQX now treats this retransmission as a duplicate handshake packet and returns `PUBREC` without re-delivering the application message.
- [#16725](https://github.com/emqx/emqx/pull/16725) Disabled the TCP connection congestion alarm by default by setting `conn_congestion.enable_alarm = false` in the default zone/global configuration.
- [#16781](https://github.com/emqx/emqx/pull/16781) Fixed CONNECT validation when retained messages are unavailable. When `mqtt.retain_available` is set to `false`, CONNECT packets with Will Retain set are now correctly rejected with CONNACK reason `Retain not supported (0x9A)`.
- [#16783](https://github.com/emqx/emqx/pull/16783) Fixed MQTT v5 SUBSCRIBE validation for the `Subscription-Identifier` upper bound. EMQX now accepts `268435455` (`0x0FFFFFFF`), which is the maximum valid Subscription Identifier value defined by the MQTT spec.
- [#16974](https://github.com/emqx/emqx/pull/16974) Restored the previous retained-message behavior for resumed or taken-over sessions. In EMQX 6.1.1, if a session had subscribed to a topic filter with retained messages and was later resumed or taken over without re-subscribing, it would receive those retained messages again. Now, retained message iteration stops unless the session explicitly re-subscribes to the topic filter.
- [#16876](https://github.com/emqx/emqx/pull/16876) Renamed the log message `msg_publish_not_allowed` to `msg_not_routed_to_subscribers`.

#### Data Integration

- [#16803](https://github.com/emqx/emqx/pull/16803) Improved error reporting when configuring batch operations for MySQL actions.
- [#16796](https://github.com/emqx/emqx/pull/16796) Fixed handling of multiline SQL statements in connector actions.
- [#16936](https://github.com/emqx/emqx/pull/16936) Fixed an issue where the health check of an Azure Blob Storage Action in aggregate mode could timeout if the container contained too many blobs.
- [#16955](https://github.com/emqx/emqx/pull/16955) Eliminated Kafka producer action false health check warning logs. Previously if Kafka producer is idling for too long, Kafka may close the connection (typically default is 10 minutes), if Kafka producer action health-checks happen to be performed around the same moment, there could be a false warning message with message `"not_all_kafka_partitions_connected"`.
- [#16972](https://github.com/emqx/emqx/pull/16972) HTTP and GCP PubSub Actions were patched to treat transient connection errors with reason `closing` as recoverable errors, reducing log noise.
- [#16863](https://github.com/emqx/emqx/pull/16863) Added a warning log when an async reply is received for an already-expired request in async actions.
- [#16847](https://github.com/emqx/emqx/pull/16847) Fixed a crash when a non-ASCII Unicode string was used in a message transformation expression.
- [#16979](https://github.com/emqx/emqx/pull/16979) MQTT ingress bridges now support consuming from remote message queues `$queue/{name}/{bind-filter}`.

#### Access Control

- [#16780](https://github.com/emqx/emqx/pull/16780) Fixed an issue in authorization source validation where requests missing the `type` field could trigger an internal error. Now EMQX returns a clear `BAD_REQUEST` validation error for this case.
- [#16805](https://github.com/emqx/emqx/pull/16805) Added support for authz hook results to opt out of authorization cache storage.
- [#16865](https://github.com/emqx/emqx/pull/16865) Added `cert_common_name` and `cert_subject` as aliases for `mqtt.client_attrs_init` expressions, alongside the existing `cn` and `dn` variables.
- [#16868](https://github.com/emqx/emqx/pull/16868) Improved REST API authentication error messages for programmatic clients. Error responses now mention the `api_key.bootstrap_file` configuration option and the `POST /api_key` endpoint for creating persistent API keys.
- [#16928](https://github.com/emqx/emqx/pull/16928) Dashboard-created REST API keys are now generated randomly instead of being derived from the API key name.
- [#16939](https://github.com/emqx/emqx/pull/16939) Fixed the built-in database authenticator to no longer log a warning for a missing but default bootstrap file.

#### Durable Storage

- [#16874](https://github.com/emqx/emqx/pull/16874) Fixed a rare issue where Durable Storage backed by DS Raft could stop accepting new messages after a sequence of quick cluster leadership changes, requiring a node restart to recover.

#### Clustering

- [#16534](https://github.com/emqx/emqx/pull/16534) Lowered the default `net_ticktime` from 2 minutes to 1 minute to improve cluster node failure detection.

#### Plugins

- [#16842](https://github.com/emqx/emqx/pull/16842) Reduced noisy warning logs for plugin config fetches when no peer node has the config yet. Previously, on startup, a node logged warnings when fetching plugin config from peers even in the benign case where no peer had the config, such as when the plugin was first loaded. This case is now logged at debug level, while genuine errors such as RPC failures and timeouts remain warnings.
- [#16843](https://github.com/emqx/emqx/pull/16843) Fixed an issue where HTTP headers and query string parameters were not passed through to plugin API handlers, causing plugins to receive empty headers and missing query parameters.
- [#16904](https://github.com/emqx/emqx/pull/16904) Prevented multiple versions of the same plugin from being enabled or started at the same time. When a newer version is enabled, older configured versions are now automatically disabled. Management API actions also now return a clear error instead of reporting success while another version is still active.

#### Gateway

- [#16536](https://github.com/emqx/emqx/pull/16536) Fixed the CoAP Gateway when running in DTLS connection mode.

#### Observability

- [#16879](https://github.com/emqx/emqx/pull/16879) Added `log.audit.cache_size` as the primary configuration key for the audit log database cache size, while keeping `log.audit.max_filter_size` for backward compatibility.

#### Deployment

- [#16901](https://github.com/emqx/emqx/pull/16901) Fixed the RPM package OpenSSL dependency for RHEL 9.6 LTS: pinned `openssl >= 3.5.1` for RHEL >= 9.7 and `openssl >= 3.0.7` for older RHEL 9 versions.

#### ExHook

- [#16890](https://github.com/emqx/emqx/pull/16890) Fixed an ExHook issue where a successful reconnect reload could duplicate the same server name in the running list and trigger repeated callback dispatches.

#### Licensing

- [#16764](https://github.com/emqx/emqx/pull/16764) Refined license customer tier handling by introducing `STANDARD` and `VIP` tiers in enforcement logic and reducing the official-license `STANDARD` expiry grace period from 90 days to 15 days before new sessions are restricted.

## 6.1.1

*Release Date: 2026-02-27*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.1.1.

### Enhancements

#### Core MQTT Functionalities

- [#16637](https://github.com/emqx/emqx/pull/16637) Improved retained message delivery during session takeover. Previously, when a session was taken over during the delivery of retained messages from a wildcard subscription, the process would restart, causing duplicate messages. EMQX now resumes delivery from the last confirmed message of the previous session, significantly reducing duplication.

#### Durable Storage

- [#16704](https://github.com/emqx/emqx/pull/16704) Optimized disk space preallocation for RocksDB storage shards. Previously, each Durable Storage shard preallocated significant disk space by default. This led to high storage consumption, especially with the 16-shard default configuration. EMQX now prevents aggressive preallocation, reducing the initial disk footprint for Durable Storage databases.

#### Message Queue and Streams

- [#16551](https://github.com/emqx/emqx/pull/16551), [#16714](https://github.com/emqx/emqx/pull/16714) Refined Message Stream and Message Queue subscription interfaces.

  - **Named Streams:** Users must now use the `$stream` prefix and specify a name when subscribing. The syntax is `SUBSCRIBE $stream/<name>/<topic_filter>` or simply `SUBSCRIBE $stream/<name>` if the stream already exists. The starting point for consumption is defined via the `stream-offset` user subscription property.
  - **Named Queues:** Similarly, message queues now utilize the `$queue` prefix. Subscriptions require a name using the syntax `SUBSCRIBE $queue/<name>/<topic_filter>` or `SUBSCRIBE $queue/<name>` for existing queues.
  - **Naming Rules:** Names are restricted to alphanumeric characters, underscores, hyphens, and dots. Existing unnamed entities will automatically adopt their topic filter as their name (prepended with `/`).
  - **Compatibility:** The legacy `$q` (v6.0.0) and `$s` (v6.1.0) interfaces are deprecated but remain functional for compatibility. Note that when Message Queues are enabled, the `$queue` prefix can no longer be used for standard shared subscriptions.

- [#16820](https://github.com/emqx/emqx/pull/16820) Added shorter API path aliases `/queues/*` and `/streams/*` for the Message Queue and Message Stream management APIs.

  The previous `/message_queues/*` and `/message_streams/*` paths remain functional for backward compatibility but are no longer shown in the API documentation.

#### Gateway

- [#16719](https://github.com/emqx/emqx/pull/16719) Added Block-Wise Transfer support for CoAP and LwM2M gateways.

  - Added block-wise settings: `enable`, `max_block_size`, `max_body_size`, and `exchange_lifetime`.
  - Improved `POST /gateways/coap/clients/:clientid/request` and LwM2M downlink handling for large block-wise messages.

- [#16736](https://github.com/emqx/emqx/pull/16736) <!-- ported from PRs #16220, #16596, #16609, #16619, #16627, #16655 -->Enhanced JT/T 808 Gateway features and protocol support.

  - Added the `jt808.frame.parse_unknown_message` option, enabling the JT808 gateway to transparently forward unknown messages.

  - Added JT/T 808 protocol 2019 support.

  - Added GBK character encoding support for JT/T 808 gateway.

    The JT/T 808 protocol specifies GBK encoding for STRING type fields. A new `frame.string_encoding` configuration option is added:

    - `utf8` (default): Pass through strings as-is (backward-compatible)
    - `gbk`: Convert GBK-encoded strings from devices to UTF-8 for MQTT, and UTF-8 from MQTT to GBK for devices

    This affects both uplink parsing (GBK to UTF-8) and downlink serialization (UTF-8 to GBK), including string fields such as license plates, driver names, text messages, area names, and client parameters.

    MQTT payloads always use UTF-8 encoding regardless of this setting.

  - Added support for custom `msg_sn` in JT/T 808 gateway downlink messages.

    When a downlink MQTT message payload contains a `msg_sn` field in the header, the gateway will use that value instead of the auto-generated channel sequence number. This allows external systems to control message sequencing for specific use cases.

  - Fixed JT/T 808 gateway parameter setting (0x8103) and query response (0x0104) message handling for CAN bus ID parameters (0x0110~0x01FF), which should use BYTE[8] data type with base64 encoding in JSON instead of string type.

  - Fixed JT/T 808 0x0702 driver identity report message parsing.

#### Security

- [#16447](https://github.com/emqx/emqx/pull/16447) Added `force_delete` parameter to Certificate Management APIs. The `DELETE` methods for global and namespace-specific certificate endpoints now support a `force_delete` query parameter:

  - `DELETE /certs/global/name/:name`
  - `DELETE /certs/ns/:ns/name/:name`

  When this parameter is `false` (default), EMQX performs a safety check across all namespaces and prevents deletion if the certificate is currently referenced by any listener or configuration. Setting it to `true` bypasses these checks for immediate removal.

- [#16461](https://github.com/emqx/emqx/pull/16461) Support for TLS 1.3 Stateless Session Resumption. EMQX now supports stateless session tickets for TLS 1.3, enabling clients to resume secure sessions without requiring server-side state storage. This improves performance and reduces memory overhead during high-frequency client reconnections.

  **Configuration:**

  - **Global Secret:** Set `node.tls_stateless_tickets_seed` as the secret key seed used to encrypt session tickets across the node.
  - **Listener Settings:** Configure `listeners.ssl.<name>.ssl_options.session_tickets` with one of the following:
    - `disabled` (Default): Resumption is deactivated.
    - `stateless`: Enables resumption using stateless tickets.
    - `stateless_with_cert`: Enables resumption and includes client certificate information in the ticket.

  **Important Note:** To generate tickets, both a non-empty global seed and a listener-level enablement are required. If a listener is enabled while the global seed is missing, EMQX will log an error and tickets will not be issued.

#### Access Control

- [#16504](https://github.com/emqx/emqx/pull/16504) Added a new configuration option to specify which OIDC data source field (claim) is used to generate the EMQX Dashboard username during Single Sign-On (SSO) user creation.
- [#16741](https://github.com/emqx/emqx/pull/16741) <!-- ported from PRs #16625, #16639 -->Introduced `idp_signs_envelopes` and `idp_signs_assertions` options for the SAML SSO backend to precisely control signature verification behavior.
  - Resolved an issue where SAML signature verification failed because the IdP certificate fingerprint was not correctly extracted from metadata.
  - Both options default to `false` to maintain backward compatibility. Users with IdPs configured to sign SAML responses should explicitly set these to `true`.
- [#16684](https://github.com/emqx/emqx/pull/16684) The `mqtt.client_attrs_init` expressions now support the use of client passwords. This allows passwords to be processed by functions (e.g., `jwt_value`) to initialize custom client attributes during the connection phase.
- [#16730](https://github.com/emqx/emqx/pull/16730) Introduced a `compatibility_mode` setting for Redis authorization to support legacy data schemas from EMQX v4.
  - **Activation:** Set `compatibility_mode = v4` to enable.
  - **Legacy Mapping:** Automatically converts `%u/%c` placeholders and maps legacy ACL access values (`1`, `2`, `3`) to `subscribe`, `publish`, and `all`.
  - **Note:** This mode is disabled by default to ensure no impact on existing v5 configurations.

#### Data Integration

- [#16511](https://github.com/emqx/emqx/pull/16511) The IoTDB data integration now supports the Table Model, allowing for more structured data ingestion into Apache IoTDB.
- [#16516](https://github.com/emqx/emqx/pull/16516) Added two specific metrics to track the performance of Aggregated Upload Actions (compatible with S3, Azure Blob Storage, Snowflake, and S3 Tables):
  - `aggregated_upload.success`: Incremented upon successful aggregated delivery.
  - `aggregated_upload.failure`: Incremented when an aggregated delivery fails.
- [#16658](https://github.com/emqx/emqx/pull/16658) Updated EMQX Tables Connector defaults and error handling.
  - The default server port for the EMQX Tables Connector has been changed from `80` to `4001`.
  - Enhanced error messaging for SSL-enabled EMQX Tables Connectors. If `cacertfile`, `certfile`, or `keyfile` are missing from the configuration, the system now returns a more descriptive error message to assist in troubleshooting.

#### Rule Engine

- [#16524](https://github.com/emqx/emqx/pull/16524) Enhanced base64 encoding and decoding functions in rule engine SQL with support for padding and URL-safe options.

  The `base64_encode` and `base64_decode` functions now support optional parameters to control encoding behavior:

  - **`no_padding`**: Encode or decode without padding characters (`=`). Useful when you need to remove padding from encoded strings or decode strings that don't have padding.
  - **`urlsafe`**: Use URL-safe base64 encoding/decoding. Replaces `+` with `-` and `/` with `_`, making the encoded string safe to use in URLs without encoding.

  You can use these options individually or combine them. When combining options, the order doesn't matter.

  **Examples in rule SQL:**

  Encode without padding:

  ```sql
  SELECT base64_encode(payload, 'no_padding') as encoded FROM "t/#"
  ```

  Encode with URL-safe characters:

  ```sql
  SELECT base64_encode(payload, 'urlsafe') as encoded FROM "t/#"
  ```

  Encode with both options (no padding and URL-safe):

  ```sql
  SELECT base64_encode(payload, 'no_padding', 'urlsafe') as encoded FROM "t/#"
  ```

  Decode URL-safe base64:

  ```sql
  SELECT base64_decode(payload, 'urlsafe') as decoded FROM "t/#"
  ```

  Decode unpadded URL-safe base64:

  ```sql
  SELECT base64_decode(payload, 'urlsafe', 'no_padding') as decoded FROM "t/#"
  ```

- [#16533](https://github.com/emqx/emqx/pull/16533) Added `json_value` and `jwt_value` helper functions to Variform expression to extract values from JSON data and JWT tokens using dot-separated key paths.

  - **`json_value(json_string, path)`**: Parses a JSON-encoded binary string and navigates nested structures to extract a specific value.
    - *Example:* `json_value(username, 'shop.floor')` extracts the `floor` field from a nested `shop` object within the username string.
  - **`jwt_value(jwt_string, path)`**: Decodes a JWT token and retrieves specific claim values from its payload.
    - *Example:* `jwt_value(password, 'client_attrs.unitid')` extracts a custom `unitid` claim from a JWT provided in the password field.

- [#16539](https://github.com/emqx/emqx/pull/16539) Added support for Sparkplug B metric alias tracking in `spb_decode`. The Rule Engine's `spb_decode` function now automatically tracks and resolves metric aliases based on Sparkplug B Birth certificates.

  - **Dynamic Mapping:** When a device or Edge of Network (EoN) node publishes `DBIRTH` or `NBIRTH` messages, EMQX stores the included alias-to-name mappings.
  - **Automatic Resolution:** Subsequent `DDATA` or `NDATA` messages processed via `spb_decode` will use these stored mappings to populate the original metric names in the output payload.
  - **Limitation:** Metric mappings are not available within the execution environment of fallback actions. If a fallback action republishes an undecoded payload, the metric name fields will remain unpopulated.

- [#16581](https://github.com/emqx/emqx/pull/16581) Introduced `spb_zip_kvs` Rule SQL function for Sparkplug B data normalization.

  Added a new Rule Engine function, `spb_zip_kvs`, to simplify the structure of decoded Sparkplug B messages. This function merges separate `keys` and `values` arrays into a unified key-value map, making the data significantly easier to process in downstream integrations.

  **Key Transformations:**

  - `PropertySets`: Recursively "zips" `keys` and `values` fields. The original arrays are removed and replaced by a merged map.
  - `PropertySetLists`: Flattens the structure by removing the `propertyset` wrapper and replacing it with an array of transformed PropertySets.
  - `DataSets`: Merges `columns` and `rows` into a single object. Metadata fields like `types` and `num_of_columns` are stripped to provide a cleaner output.
  - Non-destructive: All other fields and values remain untouched.

  For example, given this input decoded Sparkplug B message:

  ```json
  {
    "metrics": [
      {
        "properties": {
          "values": [
            {"int_value": 99},
            {
              "propertyset_value": {
                "values": [{"int_value": 999}],
                "keys": ["inner"]
              }
            },
            {
              "propertysets_value": {
                "propertyset": [
                  {
                    "values": [{"int_value": 1}],
                    "keys": ["inner1"]
                  },
                  {
                    "values": [{"int_value": 2}],
                    "keys": ["inner2"]
                  }
                ]
              }
            }
          ],
          "keys": [
            "leaf",
            "nested_prop",
            "nested_prop_list"
          ]
        }
      },
      {
        "dataset_value": {
          "num_of_columns": 2,
          "types": [7, 12],
          "rows": [
            {
              "elements": [
                {"int_value": 3},
                {"string_value": "3"}
              ]
            },
            {
              "elements": [
                {"int_value": 4},
                {"string_value": "4"}
              ]
            }
          ],
          "columns": ["col1", "col2"]
        }
      }
    ]
  }
  ```

  Then, the output of `spb_zip_kvs` will be:

  ```json
  {
    "metrics": [
      {
        "properties": {
          "nested_prop_list": {
            "propertysets_value": [
              {"inner1": {"int_value": 1}},
              {"inner2": {"int_value": 2}}
            ]
          },
          "nested_prop": {
            "propertyset_value": {"inner": {"int_value": 999}}
          },
          "leaf": {"int_value": 99}
        }
      },
      {
        "dataset_value": {
          "col2": {"elements": [{"int_value": 4}, {"string_value": "4"}]},
          "col1": {"elements": [{"int_value": 3}, {"string_value": "3"}]}
        }
      }
    ]
  }
  ```

#### REST API

- [#16718](https://github.com/emqx/emqx/pull/16718) Refined the REST API specification to improve clarity and readability in the Swagger UI.

  Previously, summaries and descriptions of spec fields were mixed together. Now, summaries are brief, simple and punctuation-free, while descriptions provide all the details.

- [#16735](https://github.com/emqx/emqx/pull/16735) EMQX now supports plugin-defined HTTP API callbacks under the `/api/v5/plugin_api/{plugin}/...` path.

  This allows plugin authors to expose plugin-specific API endpoints through the dashboard API service, with consistent authentication and HTTP error handling.

#### Observability

- [#16656](https://github.com/emqx/emqx/pull/16656) Made system monitor reports such as `busy_port` and `long_schedule` more informative by including process labels for easier troubleshooting.

- [#16744](https://github.com/emqx/emqx/pull/16744) <!-- ported from PR #16324 -->Supported end-to-end tracing of messages published via HTTP API.

#### Performance

- [#16413](https://github.com/emqx/emqx/pull/16413) Improved subscription handling performance.

- [#16492](https://github.com/emqx/emqx/pull/16492) Slightly improved idle system memory usage.

- [#16757](https://github.com/emqx/emqx/pull/16757) Set `os_mon` to collect only system-wide memory statistics by default, reducing per-process memory scanning overhead.

### Bug Fixes

#### Core MQTT Functionalities

- [#16480](https://github.com/emqx/emqx/pull/16480) Fixed an issue where WebSocket connections could crash after the peer closed the connection, typically observed under moderate load.

  ```
  crasher: initial call: cowboy_tls:connection_process/4,
  error: {{case_clause,{error,closed}},[
  {cowboy_websocket_linger,websocket_send_close,2,[{file,"cowboy_websocket_linger.erl"},{line,752}]},
  {cowboy_websocket_linger,websocket_close,3,[{file,"cowboy_websocket_linger.erl"},{line,743}]},
  {proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}
  ]}
  messages: [
  {ssl,{sslsocket,{gen_tcp,#Port<...>,...},[...]},<<130,130,27,93,145,101,251,93>>},
  {ssl_closed,{sslsocket,{gen_tcp,#Port<...>,...},[...]}}
  ], ...
  ```

- [#16515](https://github.com/emqx/emqx/pull/16515) Fixed a bug that caused WebSocket connections to crash when receiving broker messages larger than the client's advertised `Maximum-Packet-Size`.

- [#16553](https://github.com/emqx/emqx/pull/16553) Fixed an issue where reaching the dispatch rate limit would cause some retained messages to be skipped during delivery. Now, if a client hits the rate limit while iterating through retained topics, the process will no longer terminate the delivery; instead, it will retry the iteration using an exponential back-off strategy (ranging from a minimum of 300 ms to a maximum of 10 seconds) until all messages are sent.

  Additionally, this update introduces configuration changes to the retainer’s flow control:

  - The `retainer.flow_control.batch_deliver_number` setting is now deprecated.
  - The `retainer.flow_control.batch_read_number` no longer supports a value of `0` (which previously indicated an unlimited batch size). If this parameter is set to `0`, it will now default to `1000` messages to prevent potential system instability from massive batch reads.

- [#16569](https://github.com/emqx/emqx/pull/16569) Fixed a rare race condition that could cause the supporting `emqx_flapping` process for flapping detection to crash under high system load.

- [#16651](https://github.com/emqx/emqx/pull/16651) Fixed a rare connection process crash during shutdown caused by operating on an already closed socket, typically under high system stress.
  Previously, such race conditions typically result in an error-level log saying `{badmatch,{ok,{sock_error,closed}...`.

- [#16675](https://github.com/emqx/emqx/pull/16675) Fixed timestamp ordering issue where `disconnected_at` could be later than `connected_at` during session takeover or discard scenarios.

  Previously, `disconnected_at` was recorded too late (in `ensure_disconnected`), after the new session's `connected_at` was already set. This caused a race condition where `disconnected_at > connected_at`, making it difficult to track client presence state externally.

  The system now captures the `disconnected_at` timestamp immediately at the onset of a session takeover or upon receiving a discard request. This adjustment ensures that disconnection events are always sequenced before the new session's connection time, providing reliable, ordered data for external state tracking and analytics.

- [#16715](https://github.com/emqx/emqx/pull/16715) Fixed an issue where retained `$SYS` messages (for example, broker/node identity topics) were stored without expiry, which could leave stale node identifiers visible in Dashboard views after StatefulSet rotation.

  Now, newly published retained `$SYS` messages include `Message-Expiry-Interval = 3600` (1 hour).

  For already existing stale retained `$SYS` entries created before this change, you can manually clear them by publishing an empty retained message to the stale topic:

  ```bash
  emqx eval 'emqx:publish(emqx_message:set_flag(retain, true, emqx_message:make(emqx_sys, <<"$SYS/brokers/emqx@127.0.0.1/sysdescr">>, <<>>))).'
  ```

  Replace the topic in the command with the stale `$SYS/...` topic you want to remove.

- [#16731](https://github.com/emqx/emqx/pull/16731) Fixed a crash in the `emqx ctl subscriptions list` command that occurred when shared subscriptions were present. Previously, listing subscriptions could fail for certain clients and return no output. The command now works reliably for both standard and shared subscriptions.

- [#16782](https://github.com/emqx/emqx/pull/16782) Fixed MQTT v5 protocol handling for invalid PUBLISH properties. If a client sends a PUBLISH packet containing `Subscription-Identifier`, EMQX now treats it as a protocol error and disconnects the client.

#### Gateway

- [#16603](https://github.com/emqx/emqx/pull/16603) Fixed the CoAP Gateway when running in DTLS connection mode.

- [#16670](https://github.com/emqx/emqx/pull/16670) NATS gateway now enforces the maximum allowed publish payload size and correctly honors the `echo` option to prevent local message delivery (loopback). Additionally, this update improves the validation of publish and subscribe topics and provides more descriptive error messages.

#### Access Control

- [#16423](https://github.com/emqx/emqx/pull/16423) Added support for verifying the `aud` (audience) claim within JWT authentication.

  When the `aud` claim is configured in `verify_claims`, the JWT token must include a valid `aud` claim. The verification supports both string and array formats:

  - If `aud` is a string, it must exactly match the expected value.
  - If `aud` is an array, at least one element in the array must match the expected value.
  - Empty string or empty array will fail verification.
  - Missing `aud` claim will fail verification when it is configured in `verify_claims`.

- [#16459](https://github.com/emqx/emqx/pull/16459) Fixed the issue in SCRAM authentication HTTP API. Previously, incorrect user ID was returned for the created user in the user creation API call.

#### Data Integration

- [#16507](https://github.com/emqx/emqx/pull/16507) Fixed an issue where an MQTT Source would stop receiving messages after its Connector reconnected.

  Previously, when an MQTT Source’s Connector recovered from a connection loss, its topics were not re-subscribed, causing the Source to stop working until the Connector was restarted. The Source now automatically re-subscribes upon reconnect.

- [#16542](https://github.com/emqx/emqx/pull/16542) Fixed an issue where Kafka producer connections could disconnect prematurely when Kafka was overloaded, leading to excessive produce request retries.

  The produce request timeout is now automatically set to at least twice the metadata request timeout, with a minimum of 30 seconds. This reduces unnecessary reconnections and retries when metadata requests take longer than expected, especially when the metadata request timeout is configured to a small value.

- [#16622](https://github.com/emqx/emqx/pull/16622) Fixed an issue where, if an Action used async query mode and its Connector was disconnected after more than one health check, its Fallback Actions could be triggered twice.

- [#16657](https://github.com/emqx/emqx/pull/16657) Fixed a configuration migration issue where data imported from older EMQX versions failed to undergo the necessary schema conversions for compatibility with newer versions.

  A notable example occurred when migrating MQTT Connectors with static ClientIDs from v5.10.0 to v6.0.0; the internal representation of credentials associated with ClientIDs changed between these versions, but the migration logic failed to apply the required transformation. This fix ensures that all imported configurations are passed through the appropriate converters, maintaining functional integrity.

- [#16659](https://github.com/emqx/emqx/pull/16659) Fixed an upgrade compatibility issue where MQTT Connectors migrated from v5.10.0 and earlier ignored root-level credentials when using static ClientIDs. Previously, the migration logic failed to pass root username and password fields to the individual ClientID entries, causing connection failures with remote brokers after an upgrade.

  Now, if there are username and/or password fields in the root Connector, those credentials are merged with any specific ones specified per clientid, the latter taking precedence.

- [#16723](https://github.com/emqx/emqx/pull/16723) Resolved a self-healing issue within the RabbitMQ Connector, Action, and Source components. Previously, if the underlying connection or channel processes terminated unexpectedly, the component would remain in a "Disconnected" state indefinitely, requiring a manual restart to restore functionality.

- [#16742](https://github.com/emqx/emqx/pull/16742) <!-- ported from PR #16585 -->Fixed the issue of GreptimeDB TLS connection failure.

#### Durable Storage

- [#16512](https://github.com/emqx/emqx/pull/16512) Improved the handling of recoverable errors in durable sessions. Durable sessions now retry the creation of durable storage iterators when the operation fails due to network issues, whereas previously the entire session would disconnect.

  Fixed an issue in the `emqx_ds_client` component's retry mechanism where the number of retry attempts for recoverable errors was previously limited.

  Fixed several issues related to shared subscriptions:

  - Fixed an issue where the shared subscription leader would not start after a node restart.
  - The shared subscription leader no longer advertises streams that have reached the end of replay to clients.
  - Added support for configuring the state checkpoint transaction options for the shared subscription leader.

- [#16614](https://github.com/emqx/emqx/pull/16614) Introduced improvements and bug fixes for the durable storage feature:

  - Improved handling of configuration discrepancies between nodes. Previously, inconsistent initial durable storage configurations prevented replica convergence. This change ensures that the shard leader's configuration is replicated to all replicas during storage initialization and subsequent updates.

    ::: warning Note

    This change is **not backward-compatible**. During a rolling upgrade, shards will pause until a majority of replicas are upgraded. Once the majority are upgraded, downgrading to previous EMQX versions is no longer possible.

    :::

  - Resolved an issue in the durable storage subscription mechanism where a subscription created with a new iterator could skip messages if their timestamp precisely matched the iterator's timestamp.

- [#16770](https://github.com/emqx/emqx/pull/16770) Improved stability of durable sessions during takeover and garbage collection.

#### Clustering

- [#16393](https://github.com/emqx/emqx/pull/16393) Improved the stability of the Cluster Linking route replication under unstable network conditions.

- [#16465](https://github.com/emqx/emqx/pull/16465) Upgraded `gen_rpc` to `3.5.1`.

  Before the `gen_rpc` upgrade, EMQX may experience a long tail of crash logs due to a connect timeout if a peer node is unreachable. The new version of gen_rpc no longer has the long tail and has converted crash logs to more readable error logs. Additionally, the frequent log `"failed_to_connect_server"` is also throttled to avoid spamming.

- [#16544](https://github.com/emqx/emqx/pull/16544) Improved the robustness of the cluster autoclean procedure. Previously, if the autoclean feature was disabled during the initial startup of a node, it would not be activated after subsequent configuration changes.

- [#16739](https://github.com/emqx/emqx/pull/16739) Improved cluster recovery time following a simultaneous restart of all nodes. The built-in Mria database management system no longer waits for the full synchronization of an internal table used to generate transaction synchronization events.

#### Observability

- [#16537](https://github.com/emqx/emqx/pull/16537) Fixed a formatter crash triggered by certain `gen_rpc` error messages.

  Previously, EMQX could crash with a “FORMATTER CRASH” error when `gen_rpc` logged specific errors (such as transmission timeouts). The formatter now safely handles these messages without crashing.

- [#16661](https://github.com/emqx/emqx/pull/16661) Improved logging for `topic_metrics` and `cluster_rpc` when processing invalid topic requests.

- [#16674](https://github.com/emqx/emqx/pull/16674) Updated the logging system to ensure the Erlang process identifier (PID) is explicitly included as a structured data field in log outputs. 

- [#16699](https://github.com/emqx/emqx/pull/16699) Improved the error handling and logging for the Rule Engine metrics worker. Previously, under certain race conditions, long and cryptic logs like the following could be printed:

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  Now, the system print more meaningful information to help debug the issue.

#### Security

- [#16545](https://github.com/emqx/emqx/pull/16545) Fixed `node.cookie` handling of `#` character. Previously, if the cookie contained `#`, only the prefix before `#` would take effect. For example, if `abc#d` was configured, only `abc` was used as the cookie.

  Added validation to reject problematic characters, including backslash, single quote, double quote, and space.

- [#16664](https://github.com/emqx/emqx/pull/16664) Previously, it was possible to upload managed certificate files associated with non-existent managed namespaces.  Now, namespace existence is checked before accepting the upload.

- [#16692](https://github.com/emqx/emqx/pull/16692) Fixed a CRL cache regression where `emqx_crl_cache:evict/1` did not fully clear internal URL state.
  After eviction, the same CRL URL now re-registers correctly on next use, restores its refresh timer, and avoids repeated HTTP fetches per connection.

#### Plugin

- [#16784](https://github.com/emqx/emqx/pull/16784) Reduced noisy plugin startup warnings in single-node deployments.

  EMQX no longer tries to fetch plugin config from the local node during cluster config sync, avoiding repeated `config_not_found_on_node` warnings at startup.

- [#16823](https://github.com/emqx/emqx/pull/16823) Fixed a Dashboard plugin management issue for preinstalled plugins.

  When a plugin package is unpacked into `plugins/` before node startup, starting it from the Dashboard no longer causes `Plugin Config Not Found` on the plugin config page.

#### Miscellaneous

- [#16620](https://github.com/emqx/emqx/pull/16620) Fixed CRC32C dynamic library load issue on aarch64.

## 6.1.0

*Release Date: 2025-12-30*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.1.0.

### Feature Highlights

EMQX 6.1.0 introduces MQTT Streams, enhanced namespace capabilities, new data integrations, and centralized certificate management.

**MQTT Streams**

MQTT Streams feature provide durable collections of messages identified by a topic filter, with explicit lifecycle management. Messages matching a stream's topic filter are automatically appended, enabling consumption with ordering guarantees and support for multiple consumers. Clients can subscribe to streams using the special topic format `$s/<timestamp>/topic/filter` to consume messages from a specific point in time.


**Enhanced Namespace Capabilities**

- Configurations for namespace and isolation settings are now grouped together in the dashboard.
- Expanded namespace functionality with namespaced metrics, authentication, and authorization.
- Namespaced metrics are now available for messages, sessions, and data integration operations, exposed via Prometheus endpoints.
- Built-in authentication and authorization backends now support namespace-specific users and rules, enabling better multi-tenant isolation.
- Added automatic topic isolation using client namespaces as mountpoints.

**New Data Integrations**

- AWS Timestream for InfluxDB connector
- EMQX Tables connector
- InfluxDB API v3 support for InfluxDB and AWS Timestream connectors
- OAuth authentication for Kafka and Confluent Producer connectors
- Parquet file support for Azure Blob Storage and S3 Actions in Aggregated mode

**Certificate Management**

Added centralized certificate management via HTTP API, allowing certificates to be managed independently and referenced in SSL options for listeners and connectors.

### Enhancements

#### Message Queue and MQTT Stream

- [#16326](https://github.com/emqx/emqx/pull/16326) Implemented MQTT Streams.

  MQTT Streams are durable collections of messages identified by a topic filter.
  They have an explicit lifecycle, and any published message that matches the Stream's topic filter is automatically appended to the stream.
  Streams allow consumption of messages with ordering guarantees and can be consumed multiple times.
  To consume messages from a stream, clients can subscribe to a special topic of the form
  `$s/<timestamp>/topic/filter`, where `topic/filter` refers to an existing stream. Subscribing with a timestamp allows consumption to begin at a specific point in time. The timestamp may be a Unix timestamp in microseconds or one of two special values: `earliest` or `latest`.

- [#16454](https://github.com/emqx/emqx/pull/16454) For Message Queues and MQTT Streams, reconfigured garbage collection interval is now applied immediately. Previously, the new interval was applied only after the next garbage collection cycle.

#### Core MQTT Functionalities

- [#16099](https://github.com/emqx/emqx/pull/16099) Added a new rule engine event: `$events/client/ping`. This is triggered when a client sends a `PINGREQ` packet.

#### Access Control

- [#16132](https://github.com/emqx/emqx/pull/16132) Added an HTTP API to manage certificates in a centralized manner.

- [#16154](https://github.com/emqx/emqx/pull/16154) Added support for referencing managed certificate files in SSL options of listeners and clients.

- [#16266](https://github.com/emqx/emqx/pull/16266) Added a new `authorization.include_mountpoint` configuration. When enabled, topics will be prefixed by the listener's mountpoint before being evaluated by authorization backends.

- [#16272](https://github.com/emqx/emqx/pull/16272) Added support for specifying namespaced rules when using the built-in authorization backend.  Now, MQTT clients that belong to a namespace will consider only their namespaced rules when authorizing actions.

- [#16345](https://github.com/emqx/emqx/pull/16345) Added support for specifying namespaced users when using the built-in authentication backend. Now, MQTT clients that belong to a namespace will consider only their namespaced data when authenticating.

#### Data Integration

- [#15905](https://github.com/emqx/emqx/pull/15905) Now, for the HTTP Action, the HTTP request timeout is taken to be the same as `resource_opts.request_ttl`.  Previously, it was a fixed, non-configurable value of 30 seconds.

- [#16169](https://github.com/emqx/emqx/pull/16169) Updated our `parquer` dependency to support encoding `timestamp` Iceberg types to Parquet files.

- [#16179](https://github.com/emqx/emqx/pull/16179) Added support for writing Parquet files when using the Aggregated mode in Azure Blob Storage and S3 Actions.

- [#16267](https://github.com/emqx/emqx/pull/16267) EMQX supports data integration with AWS Timestream for InfluxDB.

- [#16290](https://github.com/emqx/emqx/pull/16290) Added support for OAuth authentication when using Kafka and Confluent Producer Connectors.

- [#16316](https://github.com/emqx/emqx/pull/16316) Changed the default batch size and time for multiple actions. Actions that previously supported batch operations had their defaults increased, so that now batching is the default behavior for them.

- [#16372](https://github.com/emqx/emqx/pull/16372) Added support for InfluxDB API v3 to InfluxDB and AWS Timestream Connectors.

- [#16396](https://github.com/emqx/emqx/pull/16396) EMQX supports data integration with EMQX Tables.

#### Durable Storage

- [#16136](https://github.com/emqx/emqx/pull/16136) Improved resource management and performance for durable storage.

  Introduced a concept of durable storage database group. Certain resources (such as memtable size and disk usage quota) can be shared between the group members.

  Added the following new metrics (per DB group):

  - `emqx_ds_disk_usage`: Total size of SST files
  - `emqx_ds_write_buffer_memory_usage`: RocksDB memtable size
  - `emqx_ds_total_trash_size`: Disk usage by trash SST files

  Added the following group configurations:

  - `durable_storage.db_groups.<group>.storage_quota`: Soft quota for the SST files size
  - `durable_storage.db_groups.<group>.write_buffer_size`: Maximum memtable size
  - `durable_storage.db_groups.<group>.rocksdb_nthreads_high` and `durable_storage.db_groups.<group>.rocksdb_nthreads_low`: Size of RocksDB thread pools.

  Added a new alarm that is raised when the quota is exceeded: `db_storage_quota_exceeded:<DB>`. Please refer to the "Storage Quota" section of the documentation for more details.

  Default session checkpoint interval has been changed to 15s.

- [#16286](https://github.com/emqx/emqx/pull/16286) Optimized the default durable storage settings to reduce CPU load. This PR disables subscriptions for DBs that don't use them.

#### Namespace

- [#16211](https://github.com/emqx/emqx/pull/16211) Added initial support for namespaced metrics.

  - Messages received
  - Count
  - Bytes
  - Messages sent
  - Count
  - Bytes
  - Number of sessions
  - Data integration
  - Number of actions triggered
  - DB records
  - Number of AuthN records
  - Number of AuthZ records

  Clients in managed namespaces will bump the namespaced metrics above, as well as continue to bump the usual global metrics.

  These metrics are exposed in Prometheus format to be scraped from the `GET /prometheus/ns/stats` endpoint.  By specifying the `ns=NAMESPACE` query parameter, only data from `NAMESPACE` will be returned. Omitting this parameter causes data from all namespaces to be scraped. Namespaces are added as labels to metrics.

- [#16314](https://github.com/emqx/emqx/pull/16314) Now, global admin users will see resources from all namespaces (by default) when listing namespaced resources (connectors/sources/actions/rules). They may focus on one particular namespace when performing CRUD operations by passing the `ns=NS` query parameter. If they want to list only the global namespace resources, they omit `ns` and pass `only_global=true` query parameter. Namespaced resources now return the `namespace` field to denote where they come from, with `namespace` being `null` for global resources to distinguish them from a potential namespace called `"global"`.

- [#16360](https://github.com/emqx/emqx/pull/16360) Added a `GET /mt/ns/:ns/metrics` endpoint that will return namespace-specific metrics in JSON format.

- [#16472](https://github.com/emqx/emqx/pull/16472) Added a new configuration option `namespace_as_mountpoint` to enable automatic topic isolation using client namespaces.

  When enabled, EMQX uses the client's namespace (from `client_attrs.tns`) as a topic mountpoint if no mountpoint is configured on the listener.

  Topics are automatically prefixed with the namespace for PUBLISH, SUBSCRIBE, UNSUBSCRIBE, and Will messages, and the prefix is stripped when delivering messages to clients.

  This setting is ignored if the listener already has a mountpoint configured, ensuring existing configurations take precedence.

#### Observability

- [#16135](https://github.com/emqx/emqx/pull/16135) Added two new metrics and corresponding rates for the `GET /monitor_current` HTTP API: `rules_matched` and `actions_executed`.  They track the number of rules that matched and action execution rate (i.e., success + failure), respectively.

- [#16213](https://github.com/emqx/emqx/pull/16213) Added MQTT client ID as a process label so crash logs (including max-heap and force-shutdown errors) now include the client ID for easier troubleshooting.

#### Performance

- [#16368](https://github.com/emqx/emqx/pull/16368) Upgraded the underlying runtime system from Erlang/OTP 27 to Erlang/OTP 28.

- [#16377](https://github.com/emqx/emqx/pull/16377) Reduced the number of pre-allocated metrics counters, which should contribute to reduced memory usage, especially in clusters using lots of namespaces.

#### MQTT over QUIC

- [#16133](https://github.com/emqx/emqx/pull/16133) MQTT over QUIC: Added support for connection probing using datagrams. 

  EMQX now supports zero-length datagram packets sent by clients to test connectivity. Clients can also send non-zero-length datagram packets, but they will be ignored by EMQX.

### Bug Fixes

#### Core MQTT Functionalities

- [#16344](https://github.com/emqx/emqx/pull/16344) Fixed a crash in MQTT v5 connections caused by a type mismatch when processing the request-response-information property.

- [#16354](https://github.com/emqx/emqx/pull/16354) Backported the MQTT v5 `request-response-information` schema type fix to the 6.0.x release line.

#### Access Control

- [#16308](https://github.com/emqx/emqx/pull/16308) Fixed an issue where Multi-Factor Authentication (MFA) could not be enabled after upgrading EMQX from versions earlier than 5.3.0 due to incompatible login-user database records.

- [#16446](https://github.com/emqx/emqx/pull/16446) Fixed an issue with authenticator metrics when using SCRAM in which the 'Total' count would be incremented twice for each authentication attempt, and the 'Success' count would not be bumped.

#### Data Integration

- [#16265](https://github.com/emqx/emqx/pull/16265) The health check now verifies leader connectivity only for the partitions assigned to the current EMQX node, preventing unnecessary idle connections and false alarms.

  Previously, the Kafka source connector checked leader connectivity for all partitions. In clustered deployments, each node owns only a subset of partitions, leaving connections to unassigned partition leaders idle. Because Kafka closes idle connections after a timeout (10 minutes by default), this could result in false connectivity alarms.

- [#16352](https://github.com/emqx/emqx/pull/16352) Upgraded Apache Pulsar client to 2.1.2. When Pulsar producer action's `batch_size` is configured to `1`, the producer will now encode single messages instead of single-element batch. This should allow consumers to share load using Key Share strategy.

- [#16383](https://github.com/emqx/emqx/pull/16383) Previously, when using IoTDB Connector with its RestAPI driver, credentials would not be checked during health checks.  Now, we send a no-op query during IoTDB connector health-check. This enables early detection of misconfigured client credentials.

#### Message Queue

- [#16270](https://github.com/emqx/emqx/pull/16270) Fixed a shutdown handling issue in the EMQX message queue consumer.

#### Clustering

- [#16453](https://github.com/emqx/emqx/pull/16453) Upgraded `gen_rpc` to `3.5.1`. 

  Prior to the `gen_rpc` upgrade, EMQX may experience long tail of crash logs due to connect timeout if a peer node is unreachable. The new version `gen_rpc` no longer has the long tail and converted crash logs to more readable `error` logs,
  and the frequent log `"failed_to_connect_server"` is also throttled to avoid spamming.

#### Cluster Linking

- [#16269](https://github.com/emqx/emqx/pull/16269) Fixed an issue in the Cluster Link route replication protocol recovery sequence where re-bootstrapping was incorrectly skipped even though the remote side needed it.

- [#16317](https://github.com/emqx/emqx/pull/16317) Fixed an issue in Cluster Link garbage-collection logic that could accidentally remove live routes from the internal routing table in the process of cleaning up stale route replication state. This problem occurred only when multiple independent Cluster Links were set up, and some of these links went down for relatively long periods of time.

#### Observability

- [#16417](https://github.com/emqx/emqx/pull/16417) Reduced the volume of logs generated when a resource exception occurs (`resource_exception`).  These logs are now throttled, and some potentially large terms are redacted from them.

- [#16434](https://github.com/emqx/emqx/pull/16434) Now, clearing an alarm name will clear it from all nodes. Previously, using the HTTP API to force deactivate an alarm would not clear it from all nodes. 

#### Gateway

- [#16425](https://github.com/emqx/emqx/pull/16425) Improved the returned errors when creating or updating a Gateway via the HTTP API.

#### Miscellaneous

- [#16397](https://github.com/emqx/emqx/pull/16397) Added TLS certificate validation before listener start. Fail-fast if listener is misconfigured with invalid certificates.

- [#16311](https://github.com/emqx/emqx/pull/16311) Updated error codes to correct terminology from misspelled `REST_FAILED` to `RESET_FAILED`.

## 6.0.2

*Release Date: 2026-01-16*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.0.2.

### Enhancements

#### Security

- [#16461](https://github.com/emqx/emqx/pull/16461) EMQX now supports TLS 1.3 session resumption using stateless session tickets, allowing clients to resume TLS connections without requiring server-side session state.

  **Configuration**

  - **Node-level**: `node.tls_stateless_tickets_seed`

    Secret key seed used to generate TLS 1.3 stateless session tickets.

  - **Listener-level**: `listeners.ssl.<name>.ssl_options.session_tickets`

    Enables TLS 1.3 session resumption. Supported values:

    - `disabled` (default)
    - `stateless`
    - `stateless_with_cert` (includes certificate information in the ticket)

  **Notes**

  - Session tickets are generated only when `node.tls_stateless_tickets_seed` is configured (non-empty), and `session_tickets` is enabled in listener SSL options.
  - If `session_tickets` is enabled but `node.tls_stateless_tickets_seed` is empty, session tickets will not be generated and an error log will be emitted when starting the listener.

  This PR also included a fix for the TLS 1.2 session resumption configuration. Previously, the `reuse_sessions` option for SSL listener did not take effect, i.e. EMQX always tried to enable TLS 1.2 session resumption. It is now possible to turn it off. Please note that TLS 1.2 session resumption will be disabled by default starting version 6.2.0.

#### Rule Engine

- [#16524](https://github.com/emqx/emqx/pull/16524) Enhanced base64 encoding and decoding functions in rule engine SQL with support for padding and URL-safe options.

  The `base64_encode` and `base64_decode` functions now support optional parameters to control encoding behavior:

  - **`no_padding`**: Encode or decode without padding characters (`=`). Useful when you need to remove padding from encoded strings or decode strings that do not have padding.
  - **`urlsafe`**: Use URL-safe base64 encoding/decoding. Replaces `+` with `-` and `/` with `_`, making the encoded string safe to use in URLs without encoding.

  These options can be used individually or combined in any order.

  **Examples in rule SQL:**

  Encode without padding:

  ```sql
  SELECT base64_encode(payload, 'no_padding') as encoded FROM "t/#"
  ```

  Encode with URL-safe characters:

  ```sql
  SELECT base64_encode(payload, 'urlsafe') as encoded FROM "t/#"
  ```

  Encode with both options (no padding and URL-safe):

  ```sql
  SELECT base64_encode(payload, 'no_padding', 'urlsafe') as encoded FROM "t/#"
  ```

  Decode URL-safe base64:

  ```sql
  SELECT base64_decode(payload, 'urlsafe') as decoded FROM "t/#"
  ```

  Decode unpadded URL-safe base64:

  ```sql
  SELECT base64_decode(payload, 'urlsafe', 'no_padding') as decoded FROM "t/#"
  ```

- [#16533](https://github.com/emqx/emqx/pull/16533) Added two new variadic expression helper functions, `json_value` and `jwt_value`, for extracting values from JSON data and JWT tokens using dot-separated key paths.

  - `json_value` extracts values from JSON binary strings by navigating nested objects with a dot-separated key path.
  - `jwt_value` decodes the payload of a JWT and extracts claim values using the same dot-separated path syntax.

  **Examples**:

  - If `username` contains a JSON object, you can access a nested field with `json_value(username, 'shop.floor')`.
  - If `password` contains a JWT with a customized claim, you can access a nested value with `jwt_value(password, 'client_attrs.unitid')`.

- [#16539](https://github.com/emqx/emqx/pull/16539) Added support for tracking Sparkplug B metric aliases when using the `spb_decode` Rule Engine function.

  After a device or Edge of Network (EoN) node publishes its `NBIRTH` or `DBIRTH` messages, EMQX records the alias-to-name mappings defined in those messages. When `spb_decode` is later applied to `NDATA` or `DDATA` messages from the same session, the original metric names are automatically restored and included in the decoded output.

  Note: when executing fallback actions, the mapping is not available in the environment where they run. This means that, if a fallback action republishes the undecoded `DDATA`/`NDATA` payload to a Sparkplug B `DDATA`/`NDATA` topic, the metric `name` fields will not be populated by the alias mapping.

#### Durable Storage

- [#16136](https://github.com/emqx/emqx/pull/16136) Improved resource management and performance for durable storage.

  Introduced a concept of a durable storage database group. Certain resources (such as memtable size and disk usage quota) can be shared between the group members.

  Added the following new metrics (per DB group):

  - `emqx_ds_disk_usage`: Total size of SST files
  - `emqx_ds_write_buffer_memory_usage`: RocksDB memtable size
  - `emqx_ds_total_trash_size`: Disk usage by trash SST files

  Added the following group configurations:

  - `durable_storage.db_groups.<group>.storage_quota`: Soft quota for the SST files size
  - `durable_storage.db_groups.<group>.write_buffer_size`: Maximum memtable size
  - `durable_storage.db_groups.<group>.rocksdb_nthreads_high` and `durable_storage.db_groups.<group>.rocksdb_nthreads_low`: Size of RocksDB thread pools.

  Added a new alarm that is raised when the quota is exceeded: `db_storage_quota_exceeded:<DB>`. Please refer to the "Storage Quota" section of the documentation for more details.

  Default session checkpoint interval has been changed to 15s.

- [#16286](https://github.com/emqx/emqx/pull/16286) Optimized the default durable storage settings to reduce CPU load. This PR disables subscriptions for DBs that don't use them.

#### Performance

- [#16413](https://github.com/emqx/emqx/pull/16413) Improved subscription handling performance by reducing redundant monitoring of MQTT session processes.

### Bug Fixes

#### Core MQTT Functionalities

- [#16354](https://github.com/emqx/emqx/pull/16354) Fixed a crash in MQTT v5 connections caused by a type mismatch when processing the request-response-information property.

- [#16515](https://github.com/emqx/emqx/pull/16515) Fixed an issue where WebSocket connections could crash when the broker sent messages exceeding the client-advertised `Maximum-Packet-Size`.

- [#16569](https://github.com/emqx/emqx/pull/16569) Fixed a rare race condition that could cause the supporting `emqx_flapping` process for flapping detection to crash under high system load.

#### Data Integration

- [#16265](https://github.com/emqx/emqx/pull/16265) The health check now verifies leader connectivity only for the partitions assigned to the current EMQX node, preventing unnecessary idle connections and false alarms.

  Previously, the Kafka source connector checked leader connectivity for all partitions. In clustered deployments, each node owns only a subset of partitions, leaving connections to unassigned partition leaders idle. Because Kafka closes idle connections after a timeout (10 minutes by default), this could result in false connectivity alarms.

- [#16542](https://github.com/emqx/emqx/pull/16542) Fixed an issue where Kafka producer connections could disconnect prematurely when Kafka was overloaded, leading to excessive produce request retries.

  The produce request timeout is now automatically set to at least twice the metadata request timeout, with a minimum of 30 seconds. This reduces unnecessary reconnections and retries when metadata requests take longer than expected, especially when the metadata request timeout is configured to a small value.

- [#16352](https://github.com/emqx/emqx/pull/16352) Upgraded Apache Pulsar client to 2.1.2. When Pulsar producer action's `batch_size` is configured to `1`, the producer will now encode single messages instead of single-element batch. This should allow consumers to share load using Key Share strategy.

- [#16383](https://github.com/emqx/emqx/pull/16383) Improved the IoTDB Connector health check when using the REST API driver.

  Previously, client credentials were not validated during health checks. The health check now sends a lightweight no-op query, allowing misconfigured credentials to be detected early.

- [#16507](https://github.com/emqx/emqx/pull/16507) Fixed an issue where an MQTT Source would stop receiving messages after its Connector reconnected.

  Previously, when an MQTT Source’s Connector recovered from a connection loss, its topics were not re-subscribed, causing the Source to stop working until the Connector was restarted. The Source now automatically re-subscribes upon reconnect.


#### Clustering

- [#16269](https://github.com/emqx/emqx/pull/16269) Fixed an issue in the Cluster Linking route replication protocol recovery sequence where re-bootstrapping was incorrectly skipped even though the remote side needed it.

- [#16317](https://github.com/emqx/emqx/pull/16317) Fixed an issue in Cluster Linking garbage-collection logic that could incorrectly remove active routes from the internal routing table while cleaning up stale route replication state.

  This issue could occur only in setups with multiple independent Cluster Links, where some links remained down for extended periods.

- [#16465](https://github.com/emqx/emqx/pull/16465) Upgraded `gen_rpc` to `3.5.1`.

  Before the `gen_rpc` upgrade, EMQX may experience a long tail of crash logs due to a connect timeout if a peer node is unreachable. The new version of gen_rpc no longer has the long tail and has converted crash logs to more readable error logs. Additionally, the frequent log `"failed_to_connect_server"` is also throttled to avoid spamming.

- [#16544](https://github.com/emqx/emqx/pull/16544) Improved the robustness of the cluster autoclean procedure. Previously, if the autoclean feature was disabled during the initial startup of a node, it would not be activated after subsequent configuration changes.

#### Upgrade

- [#16308](https://github.com/emqx/emqx/pull/16308) Fixed an issue where Multi-Factor Authentication (MFA) could not be enabled after upgrading EMQX from versions earlier than 5.3.0 due to incompatible login-user database records.

#### Configuration Management

- [#16397](https://github.com/emqx/emqx/pull/16397) Added TLS certificate and key file validation before listener startup.

  EMQX now performs basic validation when parsing SSL listener configuration and emits error-level logs if invalid PEM files are detected (for example, `invalid_pem_file_ignored` and `bad_keyfile_ignored`). This makes troubleshooting easier as administrators can observe errors when starting/reconfiguring, instead of troubleshooting TLS handshake failures.

#### Access Control

- [#16423](https://github.com/emqx/emqx/pull/16423) Added support for verifying the JWT `aud` (audience) claim during authentication.

  When the `aud` claim is configured in `verify_claims`, the JWT must include a valid `aud` value. Both string and array formats are supported:

  - If `aud` is a string, it must exactly match the configured value.
  - If `aud` is an array, at least one element must match the configured value.
  - An empty string or empty array fails verification.
  - The verification also fails if the `aud` claim is missing when it is configured in `verify_claims`.

- [#16459](https://github.com/emqx/emqx/pull/16459) Fixed the issue in SCRAM authentication HTTP API. Previously, incorrect user ID was returned for the created user in the user creation API call.

#### Observability

- [#16417](https://github.com/emqx/emqx/pull/16417) Reduced log volume for `resource_exception` events. Logs generated when a resource exception occurs are now throttled, and potentially large terms are redacted to prevent excessive log output.

- [#16537](https://github.com/emqx/emqx/pull/16537) Fixed a formatter crash triggered by certain `gen_rpc` error messages.

  Previously, EMQX could crash with a “FORMATTER CRASH” error when `gen_rpc` logged specific errors (such as transmission timeouts). The formatter now safely handles these messages without crashing.

## 6.0.1

*Release Date: 2025-11-11*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.0.1.

### Enhancements

#### Message Queue

- [#16080](https://github.com/emqx/emqx/pull/16080) Added a configuration option to disable the Message Queues feature. Disabling Message Queues can slightly reduce the resource usage in the cluster. When Durable Sessions are also disabled, EMQX avoids maintaining Durable Storage, further reducing administrative overhead and improving performance.
- [#16096](https://github.com/emqx/emqx/pull/16096) Added support for automatic creation of message queues when clients subscribe to non-existent `$q/` topics. Now configuration options are available to enable auto-creation for both regular and last-value semantics queues.
- [#16097](https://github.com/emqx/emqx/pull/16097) Optimized message writing to regular message queues by replacing transactional appends with dirty append functions. For QoS 0 messages, asynchronous append operations are now used. These changes significantly improve the performance of message insertion into regular queues.
- [#16098](https://github.com/emqx/emqx/pull/16098) Added a maximum queue count configuration option to limit the total number of message queues in the system.
- [#16152](https://github.com/emqx/emqx/pull/16152) Introduced per-queue limits for maximum message count and total message size. Also added new metrics to monitor message append latency and help diagnose performance or queue-limiting issues.

#### Data Integration

- [#16121](https://github.com/emqx/emqx/pull/16121) Upgraded the GreptimeDB ingester client to [v0.2.3](https://github.com/GreptimeTeam/greptimedb-ingester-erl/releases/tag/v0.2.3), which fixes several bugs and introduces support for row-based gRPC protocol (the column-based protocol is now deprecated).

  Additionally, updated the CI image to the latest stable version of GreptimeDB.

- [#16127](https://github.com/emqx/emqx/pull/16127) Fixed an invalid string value issue in the GreptimeDB connector, following the changes introduced in [#16121](https://github.com/emqx/emqx/pull/16121).

#### Performance

- [#15949](https://github.com/emqx/emqx/pull/15949) Changed the default value of the `parse_unit` option in listener configuration from `chunk` to `frame`. This change can significantly reduce CPU usage when the payload size exceeds the socket buffer (default is 4 KB).

  **Note**: With `parse_unit = frame`, if a `PUBLISH` packet exceeds the maximum allowed size, EMQX will close the connection instead of sending a `DISCONNECT` packet.

- [#16165](https://github.com/emqx/emqx/pull/16165) Optimized the performance of the `GET /clients_v2` API. Previously, when the cluster had around 50,000 clients or more, API calls to retrieve the client list could be extremely slow or even time out.

### Bug Fixes

#### Core MQTT Functionalities

- [#15884](https://github.com/emqx/emqx/pull/15884) Resolve an issue where, in rare cases, the global routing table could indefinitely retain routing information for nodes that had long left the cluster.
- [#15518](https://github.com/emqx/emqx/pull/15518) Resolved a race condition that may lead to accumulating inconsistencies in the routing table and shared subscriptions state in the cluster when a large number of shared subscribers disconnect simultaneously.

#### Upgrade

- [#16047](https://github.com/emqx/emqx/pull/16047) Added support to perform rolling upgrade from EMQX Enterprise base version 5.8.0 and newer to 6.0. During the upgrade, legacy configurations are automatically migrated to the new format supported in 6.0. Specifically, the deprecated `bridges` configuration root is converted into the new `connectors`, `sources`, and `actions` roots.

  However, the GCP PubSub Consumer and Kafka Consumer sources will still require manual changes. If any source configuration still includes the deprecated `topic_mapping` field, it must be removed. Then, for each entry previously defined in `topic_mapping`, a separate "Source + Rule" pair must be created manually.


#### Security

- [#16156](https://github.com/emqx/emqx/pull/16156) Fixed an issue where some dependencies were missing default configurations compared to EMQX 5.10, potentially causing RSA signature verification failures. The missing defaults could lead to errors, such as the following log message:

  ```
  {sign_unsupported,[[{rsa_padding,rsa_pkcs1_padding}]]}, [{jose_jwa_unsupported,verify,5,[{file,"src/jwa/jose_jwa_unsupported.erl"},{line,55}]}
  ```

- [#16175](https://github.com/emqx/emqx/pull/16175) Fixed an issue with periodic TLS certificate garbage collection. Previously, the garbage collection process incorrectly deleted certificate files that were actively used by configurations in managed namespaces.

#### Access Control

- [#16081](https://github.com/emqx/emqx/pull/16081) Fixed an issue where clients using extended authentication and memory-based sessions could crash with a `session_stepdown_request_exception` caused by a `calling_self` error.

  <details> <summary>Example error log</summary>


  ```
  2025-09-24T07:13:08.973954+08:00 [error] clientid: someclientid, msg: session_stepdown_request_exception, peername: 127.0.0.1:41782, username: admin, error: exit, reason: calling_self, stacktrace: [{gen_server,call,3,[{file,"gen_server.erl"},{line,1222}]},{emqx_cm,request_stepdown,4,[{file,"emqx_cm.erl"},{line,427}]},{emqx_cm,do_takeover_begin,2,[{file,"emqx_cm.erl"},{line,398}]},{emqx_cm,takeover_session,2,[{file,"emqx_cm.erl"},{line,384}]},{emqx_cm,takeover_session_begin,2,[{file,"emqx_cm.erl"},{line,305}]},{emqx_session_mem,open,4,[{file,"emqx_session_mem.erl"},{line,210}]},{emqx_session,open,3,[{file,"emqx_session.erl"},{line,263}]},{emqx_cm,'-open_session/4-fun-1-',4,[{file,"emqx_cm.erl"},{line,290}]},{emqx_cm_locker,trans,2,[{file,"emqx_cm_locker.erl"},{line,32}]},{emqx_channel,post_process_connect,2,[{file,"emqx_channel.erl"},{line,575}]},{emqx_connection,with_channel,3,[{file,"emqx_connection.erl"},{line,852}]},{emqx_connection,process_msg,2,[{file,"emqx_connection.erl"},{line,470}]},{emqx_connection,process_msgs,2,[{file,"emqx_connection.erl"},{line,462}]},{emqx_connection,handle_recv,3,[{file,"emqx_connection.erl"},{line,406}]},{proc_lib,wake_up,3,[{file,"proc_lib.erl"},{line,340}]}], action: {takeover,'begin'}, ...
  ```

  </details>

#### Clustering

- [#16123](https://github.com/emqx/emqx/pull/16123) Fix a bug in the component managing Mria replication that could cause cluster joins to hang or remain incomplete in core-replicant clusters.

  During cluster changes involving adding new core nodes, those new core nodes could sometimes fail to start replication-related processes required by replicants. As a result, upgraded or newly added replicants could hang during startup.

  In Kubernetes deployments, this often caused readiness probes to fail, leading the controller to repeatedly restart the affected replicant pods.

  This issue typically affected upgrade rollouts involving the addition of new core and replicant nodes. For example, adding two cores and two replicants (running a newer EMQX version) to an existing cluster with 2 cores and 2 replicants.

#### Rule Engine

- [#16028](https://github.com/emqx/emqx/pull/16028) Fixed rule engine `jq` function memory leak.

  Previously if `jq` built-in function `index` is used (e.g. `.key | index("name")`), it would result in memory leak.

#### Data Integration

- [#16010](https://github.com/emqx/emqx/pull/16010) Fixed an issue where a Republish Fallback Action could fail with a `function_clause` error if the originating rule's SQL did not include the `metadata` field from the rule environment.

  Example error log:

  ```
  [error] tag: RESOURCE, msg: failed_to_trigger_fallback_action, reason: {error,function_clause}, fallback_kind: republish, primary_action_resource_id: <<"action:type:name:connector:type:name">>, republish_topic: <<"republish/topic">>
  ```

- [#16046](https://github.com/emqx/emqx/pull/16046) Fixed a potential out-of-memory (OOM) crash when loading or restarting a configuration containing a Connector with several hundred Actions.

- [#16140](https://github.com/emqx/emqx/pull/16140) Fix a Redis cluster failover issue that could cause the Connector to remain stuck in a "connecting" state.

  Previously, EMQX’s Redis cluster client only refreshed the cluster topology when regular queries (such as `GET`) failed. However, failures in periodic `PING` commands did not trigger a refresh. As a result, after a failover, the connector could continue using the outdated cluster topology if no other commands were issued, preventing recovery.

  With this fix, failed `PING` responses now trigger a cluster topology refresh, ensuring that the connector can detect failovers and recover promptly.

#### MQTT Durable Sessions

- [#16105](https://github.com/emqx/emqx/pull/16105) Durable storage performance optimization. In particular, this fix reduces the latency of `CONNACK` for clients using a durable session.
- [#16129](https://github.com/emqx/emqx/pull/16129) Durable storage transaction configuration can be changed in the runtime. Previously changing this configuration required a node restart.

#### Observability

- [#15963](https://github.com/emqx/emqx/pull/15963) Reduced excessive audit log entries generated during looped evaluations in the remote shell (`remsh`).

- [#15967](https://github.com/emqx/emqx/pull/15967) Fixed an issue where Mnesia transaction blocking during the cleanup of large volumes of audit logs could lead to rapid memory growth.

- [#16060](https://github.com/emqx/emqx/pull/16060) Fixed a logger formatter crash that could occur for some debug-level log messages containing deeply nested terms with non-ASCII characters.

  <details> <summary>Example error log</summary>


  ```
  2025-09-29T06:55:34.120640+00:00 debug: FORMATTER CRASH: {report,#{request => #{messages => [#{role => <<"user">>,content => <<"{\"msg\": \"hello\"}">>}],system => <<"将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"/utf8>>,model => <<"claude-3-haiku-20240307">>,max_tokens => 100},msg => emqx_ai_completion_request}}
  2025-09-29T06:55:34.120780+00:00 [debug] formatter_crashed: emqx_logger_textfmt, config: #{time_offset => [],chars_limit => unlimited,depth => 100,single_line => true,template => ["[",level,"] ",msg,"\n"],with_mfa => false,timestamp_format => auto,payload_encode => text}, log_event: #{meta => #{line => 44,pid => <0.281254.0>,time => 1759128934120640,file => "emqx_ai_completion_anthropic.erl",gl => <0.4317.0>,mfa => {emqx_ai_completion_anthropic,call_completion,3},report_cb => fun logger:format_otp_report/1,matched => <<"t/1">>,namespace => global,clientid => <<"c_emqx">>,trigger => <<"t/1">>,rule_id => <<"r1sczoo0">>,rule_trigger_ts => [1759128934120]},msg => {report,#{request => #{messages => [#{role => <<"user">>,content => <<"{\"msg\": \"hello\"}">>}],system => <<"将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"/utf8>>,model => <<"claude-3-haiku-20240307">>,max_tokens => 100},msg => emqx_ai_completion_request}},level => debug}, reason: {error,badarg,[{erlang,iolist_to_binary,[["[",[["messages",": ",[[91,[[35,123,[["role"," => ",[60,60,"\"user\"",62,62]],44,["content"," => ",[60,60,"\"{\\\"msg\\\": \\\"hello\\\"}\"",62,62]]],125]],93]]],", ",["system",": ","将输入的 JSON 数据中，值为数字的 value 相加起来，并输出，只需返回输出结果。"],", ",["model",": ","claude-3-haiku-20240307"],", ",["max_tokens",": ","100"]],"]"]],[{error_info,#{module => erl_erts_errors}}]},{emqx_trace_formatter,format_term,2,[{file,"emqx_trace_formatter.erl"},{line,126}]},{emqx_logger_textfmt,format_term,2,[{file,"emqx_logger_textfmt.erl"},{line,230}]},{emqx_logger_textfmt,try_encode_meta,4,[{file,"emqx_logger_textfmt.erl"},{line,206}]},{lists,foldl_1,3,[{file,"lists.erl"},{line,2151}]},{emqx_logger_textfmt,enrich_report,3,[{file,"emqx_logger_textfmt.erl"},{line,102}]},{emqx_logger_textfmt,format,2,[{file,"emqx_logger_textfmt.erl"},{line,24}]}]}
  ```

  </details>

- [#16134](https://github.com/emqx/emqx/pull/16134) Fixed a backward compatibility issue that could prevent new Log Traces from being created in some cases.

#### Rate Limit

- [#16160](https://github.com/emqx/emqx/pull/16160) Improved the rate limiting algorithm for individual client connections. Previously, clients could temporarily exceed their publish rate limits, particularly just after connecting or after periods of inactivity.

  This update makes the limiter behavior more predictable and consistent, ensuring rate limits are correctly enforced from the start of a connection.

## 6.0.0

*Release Date: 2025-09-30*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.0.0.

### Feature Highlights

EMQX Enterprise 6.0.0 is the first release of the EMQX Enterprise version 6 series, bringing significant architectural improvements and new capabilities.

#### Message Queue

The native Message Queue feature unifies real-time MQTT publish/subscribe with persistent asynchronous queuing. The server buffers messages that match a topic filter, retaining them even when subscribers are offline. Clients can consume these messages through the special `$q/{topic}` topic, ensuring reliable message delivery.

Message Queues support offline message storage, last-value retention, and flexible dispatch strategies, enhancing MQTT with both real-time and durable messaging capabilities.

#### Namespace

The Namespace feature improves multi-tenancy and observability with namespace-level roles in the Dashboard. Users are restricted to their own resources (e.g., Rules, Actions, and Connectors) with fine-grained permissions such as Administrator or Viewer, and roles can be managed via the Dashboard, API, or CLI, simplifying multi-tenant operations.

Session count tracking has also been optimized: counts refresh on demand when there are fewer than 1,000 connections, and every 5 seconds otherwise. During rolling upgrades from older versions, counts may temporarily appear inconsistent, but will stabilize once all nodes are updated.

#### MQTT Durable Sessions

Durable storage has been optimized by separating session data from the broker’s other metadata, significantly reducing RAM usage and improving storage efficiency.

New configuration options provide finer control over RocksDB memory usage and performance. In addition, the default serialization schema for stored messages has been updated to ASN.1, further enhancing efficiency.

#### New Data Integrations

- Google BigQuery
- AWS AlloyDB
- CockroachDB
- AWS Redshift

#### Enhanced Integration

- **AWS**:
  - Support for Instance Metadata Service v2 APIs from EC2 instances when using S3 or S3Tables data integration. This enables seamless access to S3 buckets without manual AWS credential configuration, leveraging IAM roles for better security.
  - Parquet format support for S3 Tables Action.

- **RabbitMQ**: Define custom Headers and Properties Templates in RabbitMQ Sink to enhance message routing and compatibility within RabbitMQ.
- **Snowflake**: Snowpipe Streaming upload mode for Snowflake Action (preview feature).
- **RocketMQ**: New `key` and `tag` template fields in Action, along with a `key_dispatch` option for the Produce Strategy, allowing greater customization of message metadata.

#### Elixir Support

All packages now ship with Elixir support through the Mix build system, opening EMQX to the Elixir community and enabling better tooling with IEx console.

#### Enhanced LDAP Support

LDAP authorization now supports extended ACL rules in JSON format, and LDAP authentication can fetch ACL rules directly from LDAP with client-side caching.

#### Improved Tracing

Configurable limits for maximum traces (`trace.max_traces`) and trace file sizes (`trace.max_file_size`).
After `max_file_size` is reached, the trace log will rotate to a new file instead of halting.

#### Cluster Management

New `cluster.description` configuration option allows users to set and display custom cluster descriptions in the EMQX Dashboard.

### Enhancements

#### Message Queue

- [#15789](https://github.com/emqx/emqx/pull/15789) Implemented Message Queues, which are collections of messages identified by `topic_filter`. Each queue has an explicit lifecycle and is automatically replenished with published messages matched with the queue's topic filter during the queue's lifetime. Clients can cooperatively consume messages from a queue by subscribing to a special topic in the format: `$q/{topic}`.

#### Core MQTT Functionalities

- [#15805](https://github.com/emqx/emqx/pull/15805) Introduced a dedicated worker pool for handling sharded fanout message delivery.
  Previously, the broker pool handled both subscription management and message dispatch, which could lead to scheduling contention. This change separates the fanout dispatch workload into its own pool to ensure more balanced and efficient handling of pub/sub operations.

#### Access Control

- [#15349](https://github.com/emqx/emqx/pull/15349) Optimize external resource management for authentication and authorization. Previously, EMQX could remain connected to a resource configured for a disabled authenticator or authorizer.

- [#15294](https://github.com/emqx/emqx/pull/15294) Enhanced LDAP authentication and authorization. LDAP authorization now supports extended ACL rules in JSON format. LDAP authentication can now fetch ACL rules from LDAP. These rules are cached in the client's metadata, so authorization is performed without additional LDAP queries.

- [#15730](https://github.com/emqx/emqx/pull/15730) Added support for overriding the client ID based on authentication results. If an authentication backend returns a `clientid_override` attribute upon successful authentication, it will replace the client’s original client ID.

  The following backends now support `clientid_override`:

  - HTTP
  - JWT
  - LDAP
  - MongoDB
  - MySQL
  - Postgres
  - Redis

- [#15820](https://github.com/emqx/emqx/pull/15820) Changed default value of config `authorization.no_match` from `allow` to `deny` for better security defaults.

#### Clustering

- [#15600](https://github.com/emqx/emqx/pull/15600) Introduced a new configuration option `cluster.description` that allows you to add a descriptive label to the EMQX cluster.  This description can be updated via `PUT /cluster`, and retrieved with the `GET /cluster` API.

#### LLM-Based MQTT Data Processing

- [#15467](https://github.com/emqx/emqx/pull/15467) Exposed transport configuration options for AI Completion Providers. Users can now configure connection timeouts and the maximum number of connections to AI Completion Providers. This helps prevent `checkout_timeout` errors when message throughput is high and the provider is under load.
- Flow designer supports integrating with the [Google Gemini model](https://docs.mqttce.com/en/emqx/v6.0/flow-designer/gemini-node-quick-start.html).

- [#15631](https://github.com/emqx/emqx/pull/15631) Added a new API endpoint to list all models available for an AI provider.
- [#15467](https://github.com/emqx/emqx/pull/15467) Exposed transport options for AI Completion Providers. These options allow configuring connection timeouts and maximum connections to an AI Completion Provider.
- [#15724](https://github.com/emqx/emqx/pull/15724) Introduced `openai_response` type for AI Completion Providers and completion profiles to use OpenAI's `response` API.

#### Data Integration

- [#15418](https://github.com/emqx/emqx/pull/15418) EMQX supports data integration with BigQuery.

- [#15401](https://github.com/emqx/emqx/pull/15401) Added support for the Snowpipe Streaming upload mode in the Snowflake Action.
  *Note: Snowpipe Streaming is currently a* [*preview feature*](https://docs.snowflake.com/en/release-notes/preview-features) *and is only available for Snowflake accounts hosted on AWS.*

- [#15387](https://github.com/emqx/emqx/pull/15387) Added rate limiting to Kinesis Producer Connector and Action health checks to comply with AWS API quotas and improve cluster behavior.

  - Health check calls to `ListStreams` and `DescribeStream` are now limited to 5/s and 10/s per Connector, respectively, matching AWS rate limits.
  - A distributed limiter is coordinated by a core node in the cluster to enforce these limits consistently.
  - If a health check is throttled or times out, the Connector or Action will now retain its previous status instead of being marked as disconnected.

  Also introduced a new `resource_opts.health_check_interval_jitter`, which adds a uniform random delay to `resource_opts.health_check_interval` to reduce the chance of multiple Actions under the same Connector running health checks at the same time.

- [#15176](https://github.com/emqx/emqx/pull/15176) Upgraded the GreptimeDB Connector client and supported an optional new parameter `ttl` to set the default time-to-live for automatically created tables.

- [#15649](https://github.com/emqx/emqx/pull/15649) EMQX supports data integration with AWS AlloyDB, CockroachDB, and AWS Redshift.

- [#15635](https://github.com/emqx/emqx/pull/15635) Added new `key` and `tag` template fields in the RocketMQ Action, allowing customization of the message's key and tag. Also, introduced a new `key_dispatch` option for the `Produce Strategy` field.

- [#15621](https://github.com/emqx/emqx/pull/15621) Now, `access_key_id` and `secret_access_key` are optional fields for the S3 Tables Connector.  If omitted, they'll be obtained from the Instance Metadata Service v2 APIs from the EC2 instance where EMQX is deployed.

- [#15628](https://github.com/emqx/emqx/pull/15628) Removed HStreamDB data integration.

- [#15544](https://github.com/emqx/emqx/pull/15544) Added Arrow Flight SQL NIF driver support for Datalayers Integration.

- [#15637](https://github.com/emqx/emqx/pull/15637) Added support for templating message headers and properties for the RabbitMQ Action.

- [#15864](https://github.com/emqx/emqx/pull/15864) Removed the deprecated "Bridges V1" APIs and configuration schemas. All endpoints under `/bridges/*` and configuration entries under the `bridges` root key are no longer available, as data integrations have fully migrated to the "Connectors/Actions/Sources" model.

- [#15583](https://github.com/emqx/emqx/pull/15583) Updated the `brod` client to version 4.4.4, expanding support for a wider range of Kafka APIs. This update addresses the deprecation of `JoinGroups` API versions `v0` - `v1`.

#### Smart Data Hub

- [#15525](https://github.com/emqx/emqx/pull/15525) Prevented deletion of internal schemas that are still in use. If a schema is referenced by a Schema Validation or Message Transformation, it can no longer be removed to avoid runtime errors and configuration inconsistencies.

#### Durable Storage

- [#15463](https://github.com/emqx/emqx/pull/15463) Improved durable storage RAM usage and storage efficiency.
  - Introduced the following configuration parameters for the durable storage to improve control over RocksDB memory usage and storage performance:
    - `durable_storage.messages.rocksdb.write_buffer_size`: RocksDB memtable size per shard.
    - `durable_storage.messages.rocksdb.cache_size`: RocksDB block size per shard.
    - `durable_storage.messages.rocksdb.max_open_files`: Limits the number of file descriptors used by RocksDB per shard.
    - `durable_storage.messages.layout.wildcard_thresholds`: Allows to tune wildcard thresholds for the `wildcard_optimized_v2` storage layout.
  - Additionally, the default `serialization_schema` for stored messages has been changed to `asn1`.

- [#16044](https://github.com/emqx/emqx/pull/16044) Some of config fields for durable sessions have been removed or renamed, and old values are marked as deprecated:

    - `durable_sessions.heartbeat_interval` has been renamed to `durable_sessions.checkpoint_interval`.
    - `durable_sessions.idle_poll_interval` and `durable_sessions.renew_streams_interval` have been removed, as sessions are now fully event-driven.
    - `durable_sessions.session_gc_interval` and `durable_sessions.session_gc_batch_size` have been removed as obsolete.

#### CLI

- [#15399](https://github.com/emqx/emqx/pull/15399) The `node_dump` tool now exports the current system configuration in HOCON format, with sensitive information (such as passwords and secrets) automatically redacted for security.

#### Namespace

- [#15841](https://github.com/emqx/emqx/pull/15841) Improved the refresh rate of the session count for namespaced sessions.

  - If a namespace has fewer than 1000 connections, its session count is now updated on demand.
  - For namespaces with 1000 or more connections, the count is updated every 5 seconds.

  During a rolling upgrade from versions prior to 6.0, session counts may appear inconsistent due to changes in the internal tracking tables. This is expected: as clients reconnect to upgraded nodes, the session counts will gradually stabilize and become accurate once all nodes are running version 6.0 or later.

#### Observability

- [#15594](https://github.com/emqx/emqx/pull/15594) Introduced a new configuration option `trace.max_traces` to control the maximum number of active cluster-wide traces. This limit does not apply to node-local traces managed using `emqx ctl trace`.

  This update also optimized tracing implementation to eliminate potential atom leaks per created trace.

- [#15556](https://github.com/emqx/emqx/pull/15556) Introduced a new configuration option `trace.max_file_size` to limit the maximum file size for each individual trace.

- [#15650](https://github.com/emqx/emqx/pull/15650) Implemented automatic trace log rotation.

  When a trace file size exceeds `trace.max_file_size`, EMQX no longer discards all subsequent events and emits an incomprehensible warning to `stderr`. Instead, portions of the oldest events are discarded while the most recent ones are retained.

  As such, this also implies that:

  * EMQX now maintains multiple trace log files per active trace. The layout of the trace directory has changed accordingly.
  * Trace API has been updated to reflect this behavior. The Log Stream API may return new errors, such as when a stream becomes stale due to a slow consumer.


- [#15904](https://github.com/emqx/emqx/pull/15904) Support viewing and updating of tracing configuration through Trace API.

#### Performance

- [#15451](https://github.com/emqx/emqx/pull/15451) Introduced an experimental `socket` backend for TCP listeners, aimed at improving message processing latency and reducing compute resource usage. The feature can be enabled with the new `tcp_backend` listener option.

#### Build and Tooling

- [#15484](https://github.com/emqx/emqx/pull/15484) Switched the build system to [Elixir](https://elixir-lang.org/)'s [Mix](https://hexdocs.pm/elixir/introduction-to-mix.html), enabling all packages to include native Elixir support. This change improves developer tooling, allows integration with Elixir dependencies when needed, and enables use of the [IEx](https://hexdocs.pm/iex/IEx.html) shell as a more powerful EMQX console.

#### License

- [#15921](https://github.com/emqx/emqx/pull/15921) Introduced a license alarm for cluster-wide maximum transactions per second (TPS).
  - Each node calculates TPS as the average number of MQTT messages sent and received over the past 10 seconds.
  - The total cluster TPS is aggregated every 5 seconds.
  - If the observed TPS exceeds the licensed limit, an alarm is triggered.
  - The alarm remains active until a license with a higher TPS allowance is applied.

#### MQTT over QUIC

- [#15997](https://github.com/emqx/emqx/pull/15997) Added support for disabling QUIC stack loading by setting the environment variable `QUICER_SKIP_NIF_LOAD=1.`

### Bug Fixes

#### Core MQTT Functionalities

- [#15396](https://github.com/emqx/emqx/pull/15396) Removed redundant cleanup operations for shared subscriptions of disconnected clients. These operations were prone to crashes under high disconnect volumes and could lead to inconsistencies in the global broker state.

- [#15361](https://github.com/emqx/emqx/pull/15361) Fixed a `function_clause` error when parsing a malformed `User-Property` pair with invalid (too short) length.

- [#15783](https://github.com/emqx/emqx/pull/15783) Ensure that any changes to connection rate limits take effect immediately after the listener update has completed. Previously, parts of internal limiter state were not directly affected by configuration changes. For example, after increasing the burst rate, the effective rate limit could appear stricter than expected.

#### Access Control

- [#15489](https://github.com/emqx/emqx/pull/15489) Fixed OIDC issuer URL validation in Single Sign-On (SSO) settings. Previously, issuer URLs containing a port number (for example,
  `https://xxxxxxxx:8443/webman/sso/.well-known/openid-configuration`) were rejected with a `bad_port_number` error. These URLs are now supported.

#### Rule Engine

- [#15569](https://github.com/emqx/emqx/pull/15569) Fixed an issue where a Republish Rule Action could fail if the `direct_dispatch` template was empty or resolved to a non-boolean value. In these cases, the default value `false` is now used.

#### Data Integration

- [#15522](https://github.com/emqx/emqx/pull/15522) Fixed an issue where Snowflake Connector would fail to start correctly if `username` was not provided.
- [#15476](https://github.com/emqx/emqx/pull/15476) Fixed a missing callback in `emqx_connector_aggreg_delivery` that caused a crash when formatting delivery process status for aggregated-mode Actions (e.g., Azure Blob Storage, Snowflake, S3 Tables).
  This occurred during failures or when inspecting delivery processes with `gen_server:format_status/1`. The issue is now resolved, and more detailed delivery status information will be logged.
- [#15394](https://github.com/emqx/emqx/pull/15394) Fixed a rare race condition where Action metrics could become inconsistent due to unexpected asynchronous replies.
- [#15647](https://github.com/emqx/emqx/pull/15647) Fixed an issue where a MongoDB Connector was marked as `Disconnected` if the MongoDB account specified in the connector configuration lacked privileges to perform `find` queries on the `foo` collection.
- [#15603](https://github.com/emqx/emqx/pull/15603) Fixed an issue in the MQTT bridge where a stale connection could be shown as `Connected` and would not automatically reconnect.
- [#15383](https://github.com/emqx/emqx/pull/15383) Fixed a potential resource leak in MQTT bridge. When a bridge failed to start, the topic index table was not properly cleaned up.
- [#15786](https://github.com/emqx/emqx/pull/15786) Fixed a potential atom leak when probing RocketMQ Connectors.
- [#15806](https://github.com/emqx/emqx/pull/15806) Improved validation for Oracle Actions during creation. Previously, in rare cases, an Action containing an invalid SQL statement could be added successfully.
- [#15848](https://github.com/emqx/emqx/pull/15848) Improved error reporting for the Oracle Connector. When the connector becomes disconnected, its status now includes a more specific reason, making diagnostics easier.
- [#15693](https://github.com/emqx/emqx/pull/15693) Fixed a resource leak in Postgres-based bridges. Under certain race conditions during pool initialization, deleting a Connector could leave its connection pool behind. This has been corrected to ensure connection pools are properly cleaned up.
- [#15543](https://github.com/emqx/emqx/pull/15543) Fixed an issue in HTTP Server data integration when sending large payloads. If the payload size was 10 MB or more, the HTTP request could fail.

#### Smart Data Hub

- [#15839](https://github.com/emqx/emqx/pull/15839) Fixed an encoding issue with Protobuf schemas that use `map<_, _>` fields.
  Previously, schemas containing `map<string, string>` fields could fail to encode valid payloads, resulting in cryptic runtime errors.

  Example schema:

  ```protobuf
  syntax = "proto3";
  
  message test {
  map<string, string> args = 1;
  }
  ```

  Example rule:

  ```sql
  SELECT
  schema_encode('xxx', json_decode(payload), 'test') as protobuf_test
  FROM
  "t/#"
  ```

  Example payload failed to be encoded:

  ```json
  {
  "args": {
  "env": "stag"
  }
  }
  ```

  Previous error similar to:

  ```
  2025-06-17T06:59:22.725785+00:00 [warning] tag: RULE_SQL_EXEC, clientid: c_emqx, msg: SELECT_clause_exception, reason: {error,{gpb_type_error,{bad_unicode_string,[{value,env},{path,"test.args.key"}]}},[{'$schema_parser_xxx',mk_type_error,3,[{file,"$schema_parser_xxx.erl"},{line,437}]},{'$schema_parser_xxx','-v_map<string,string>/3-lc$^0/1-0-',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx','v_map<string,string>',3,[{file,"$schema_parser_xxx.erl"},{line,429}]},{'$schema_parser_xxx',v_msg_test,3,[{file,"$schema_parser_xxx.erl"},{line,404}]},{'$schema_parser_xxx',encode_msg,3,[{file,"$schema_parser_xxx.erl"},{line,73}]},{emqx_schema_registry_serde,with_serde,2,[{file,"emqx_schema_registry_serde.erl"},{line,212}]}...
  ```

#### Observability

- [#15931](https://github.com/emqx/emqx/pull/15931) Resolved a bug where spurious but harmless error logs could appear during node startup:
    ```
    [error] Generic event handler emqx_alarm_handler crashed ...
    Reason: {aborted,{no_exists,[emqx_activated_alarm,runq_overload]}}
    ```

- [#15973](https://github.com/emqx/emqx/pull/15973) Fixed a bug where an alarm activation timeout could crash the connection process under certain conditions.

#### MQTT over QUIC

- [#15614](https://github.com/emqx/emqx/pull/15614) QUIC Listener: When TLS key logging (`SSLKEYLOGFILE`) is enabled, EMQX now dumps TLS keys even if the handshake fails.

#### Clustering

- [#16021](https://github.com/emqx/emqx/pull/16021) Fixed issues that occasionally prevented the DS Raft backend from functioning correctly when an existing node joined a new cluster and subsequently became member of DS replica sets.

#### Cluster Linking

- [#15894](https://github.com/emqx/emqx/pull/15894) Previously, when listing all cluster links via `GET /cluster/links`, disabled links would be returned having an `inconsistent` status. Now they are returned as `disconnected`.

#### Performance

- [#15696](https://github.com/emqx/emqx/pull/15696) Added connection rate limiting support for WebSocket (WS) and WebSocket Secure (WSS) listeners.
  The `max_conn_rate` and `max_conn_burst` configuration options are now enforced: incoming connections exceeding the defined rate are immediately closed upon acceptance, consistent with existing TCP listener behavior.

  Additionally, the behavior of `max_connections` has been updated. When the connection limit is exceeded, WS/WSS listeners now close connections immediately before any HTTP handshake, resulting in an abrupt socket close instead of returning an HTTP 429 response.

- [#15854](https://github.com/emqx/emqx/pull/15854) Reduced the default `active_n` value from `100` to `10` to improve MQTT client responsiveness, especially under high message rates with small payloads.

  The lower `active_n` introduces more backpressure at the TCP layer, stricter than the default `Receive-Maximum` of `32`, which helps in the following scenarios:

  - The client process is blocked by external authorization checks
  - Data integration operations are delaying message handling
  - The system is under heavy load or nearing resource limits

- [#15981](https://github.com/emqx/emqx/pull/15981) Prevented excessive memory growth caused by Mnesia transaction blocking during cleanup of large volumes of audit logs. This improves system stability and memory efficiency during heavy audit log maintenance operations.
