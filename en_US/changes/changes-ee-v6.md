# EMQX Enterprise Version 6

## 6.3.0

*Release Date: 2026-09-03*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.3.0.

### Enhancements

#### Core MQTT Functionalities

- [#16694](https://github.com/emqx/emqx/pull/16694) Added support for extracting peer certificates from QUIC connections, enabling `peer_cert_as_username` on QUIC mTLS listeners.

- [#17307](https://github.com/emqx/emqx/pull/17307) Added a per-client rate limiter for SUBSCRIBE packets on listeners. It is disabled by default. When configured with a finite rate, EMQX responds to packets that exceed the limit with a SUBACK containing the Quota Exceeded reason code and does not process them. Namespaces can configure independent rates, which override the listener-level rate.

- [#17546](https://github.com/emqx/emqx/pull/17546) [#18477](https://github.com/emqx/emqx/pull/18477) Added the `mqtt.max_session_expiry_interval` configuration to cap the session expiry interval an MQTT 5.0 client may request via its `Session-Expiry-Interval` property. If the client requests longer than this limit, the server silently clamps it and reflects the clamped value back in the CONNACK. The setting defaults to `infinity` (no clamp), preserving the previous behavior. It has no effect on MQTT 3.1.1/3.1 clients, whose session expiry remains fully server-controlled via `mqtt.session_expiry_interval`.

  The cap also applies to the Session Expiry Interval a client supplies in a DISCONNECT packet, so a client cannot extend its session expiry beyond the configured limit when disconnecting.

- [#17603](https://github.com/emqx/emqx/pull/17603) Added support for extracting subject alternative names from directly connected TLS client certificates into MQTT client attributes with `cert_san.dns`, `cert_san.ip`, `cert_san.email`, and `cert_san.uri` in `mqtt.client_attrs_init`.

- [#17854](https://github.com/emqx/emqx/pull/17854) Changed the default `tcp_backend` for MQTT TCP listeners to `socket` on Unix systems to improve message latency and resource usage. The `gen_tcp` backend remains available by setting `tcp_backend = gen_tcp`, and remains the default on Windows.

- [#17870](https://github.com/emqx/emqx/pull/17870) Improved in-memory session delivery behavior for slow or congested subscribers.

  - EMQX now tracks connection send-queue congestion and moves QoS 0 deliveries through the session message queue when needed, instead of continuing to push them directly to a congested connection.
  - The session message queue now prefers evicting older QoS 0 messages when it reaches capacity, helping QoS 1 and QoS 2 deliveries make progress during QoS 0 bursts.
  - Fixed delivery ordering when a session delivery rate limit is reached, so later messages do not overtake earlier queued messages.
  - Improved socket-backed connection `send_timeout` handling so the timeout is armed only after the socket queue is past its watermark.

#### Access Control

- [#17145](https://github.com/emqx/emqx/pull/17145) Authorization sources now support Variform-based preconditions. A source with a precondition is called only when the expression evaluates to `true`, allowing different authorization backends to be selected by client and request context such as client attributes, action, and topic.

- [#17487](https://github.com/emqx/emqx/pull/17487) Strengthened Dashboard administrator password and API key secret hashing.

  Dashboard administrator passwords are now hashed with PBKDF2-HMAC-SHA256 (600,000 iterations) and a 16-byte random salt. API key secrets are hashed in the same self-describing storage format with the same 16-byte random salt but without iteration stretching, so per-request HTTP API authentication remains cheap. The previous scheme (single-pass SHA-256 with a 2-byte salt) is replaced for both credential types.

  Existing stored hashes continue to be accepted for authentication. They are rewritten in the new format the next time the user changes their password or the API key is recreated.

- [#17671](https://github.com/emqx/emqx/pull/17671) Authentication and authorization rejection logs now include per-backend attribution.

  When EMQX rejects a client because an authenticator returns an error, it emits a warning log identifying the authenticator ID and provider that produced the rejection. When an authorization source denies an operation, EMQX logs the denial at warning level (previously only visible under a client trace) with the source type, module, topic, and action.

  This makes it possible to tell which backend produced a decision in deployments with multiple authenticators or authorization sources, without having to enable a client trace first. The new logs are throttled per authenticator and per authorization source to avoid log floods.

- [#18130](https://github.com/emqx/emqx/pull/18130) Added support for template variables in the URL host of HTTP authenticators and authorizers, for example `https://${client_attrs.tns}.auth.example.com/authn`, enabling per-tenant authentication and authorization endpoints without an external routing layer.

  The `hostname_resolution` setting controls how the URL host is handled. The default `static` preserves the previous behavior: a fixed hostname and a persistent connection pool established at configuration time. Setting it to `dynamic` (required when the URL host contains template placeholders) makes EMQX resolve the hostname for every request and send each check over a per-request connection, applying the configured TLS options (including peer verification) to that host; the `pool_size` setting then limits how many idle connections may be kept for reuse (`0` disables reuse), while pipelining options do not apply.

  For templated hosts, the new `allowed_hosts` setting must list the hostnames the host may render to, either as exact names or as `*.suffix` wildcard patterns; when the rendered hostname is not covered by this list, no request is made and the check fails. URLs with a literal hostname keep using the connection pool exactly as before.

- [#18239](https://github.com/emqx/emqx/pull/18239) [#18371](https://github.com/emqx/emqx/pull/18371) Flapping detection can now also act on the username and the source IP address of connecting clients, in addition to the client ID.

  Each dimension has its own detection window, connect-attempt threshold, and ban duration, configured under `flapping_detect.by_clientid`, `flapping_detect.by_username`, and `flapping_detect.by_peerhost` (username and source IP detection are disabled by default). The client ID dimension was previously configured with the flat `flapping_detect` fields (`enable`, `window_time`, `max_count`, `ban_time`); these are now deprecated but remain accepted and are automatically mapped onto `by_clientid`, so existing configurations keep working unchanged. When a username or source IP address exceeds its threshold within the detection window, it is temporarily banned: new connection attempts are rejected before authentication runs, while already-connected clients are not affected. Ban entries expire automatically and can be inspected or removed early via the `/banned` REST API; each entry carries the ban type (`as`: `clientid`, `username`, or `peerhost`) and `by` = `flapping detector`, and the list can be filtered by type.

  Counters are kept separately per zone and per dimension, so zones configured with different detection windows do not discard each other's still-active counters.

  New metrics: `flapping.detected.clientid`, `flapping.detected.username`, and `flapping.detected.peerhost` count flapping triggers per dimension, and `client.banned` counts connection attempts rejected due to an active ban entry.

#### Multi-tenancy

- [#17454](https://github.com/emqx/emqx/pull/17454) Scoped Prometheus data returned by `/api/v5/prometheus/data_integration` to the requesting actor's namespace. A global administrator can view data from all namespaces and filter by namespace.

#### Data Integration

- [#17129](https://github.com/emqx/emqx/pull/17129) Added Attached Service Account authentication for GCP-based connectors (GCP PubSub Producer, GCP PubSub Consumer, and BigQuery). When EMQX runs on a GCP VM with a service account attached to the instance, it can query the internal metadata endpoint to obtain a token for these connectors.

- [#17222](https://github.com/emqx/emqx/pull/17222) Added a Bigtable data integration that supports appending data to Bigtable.

- [#17547](https://github.com/emqx/emqx/pull/17547) Added support for AWS IAM Roles Anywhere in Kafka Producer and Consumer Connectors. The Connectors can be configured with the HTTP endpoint exposed by the credential helper process.

  The credential helper process must be running and accessible to EMQX. See the [AWS IAM Roles Anywhere credential helper documentation](https://docs.aws.amazon.com/rolesanywhere/latest/userguide/credential-helper.html#credential-helper-serve) for more information.

- [#17783](https://github.com/emqx/emqx/pull/17783) Added an `application_name` option to PostgreSQL-family connectors. It defaults to `emqx` and is sent as the PostgreSQL startup parameter so connector sessions can be identified in PostgreSQL activity views and logs. The value must be 1 to 63 bytes long and cannot contain zero bytes.

- [#18119](https://github.com/emqx/emqx/pull/18119) The Disk Log connector now supports time-based file rotation in addition to size-based rotation.

  A new optional `rotation` setting was added to the connector configuration:

  - `rotation.period`: `none` (default), `day`, or `hour`. When set to `day` or `hour`, the connector starts a separate set of log files at each period boundary, with the period's date stamp (`YYYYMMDDHH`) encoded in the file names (for example, `mqtt-trace-2026062400.log.1` for daily rotation of `mqtt-trace.log`). Size-based rotation (`max_file_size` / `max_file_number`) still applies within each period.
  - `rotation.retention_period`: how long to keep files from previous periods (for example, `30d`); date-stamped files outside the retention period are deleted automatically after each period rotation. Defaults to `infinity` (files are retained indefinitely).
  - `rotation.timezone`: timezone used to determine period boundaries: `UTC` (default), `local`, or a fixed offset such as `+02:00`.

  The default behavior (no `rotation` configured, or `rotation.period = none`) is unchanged.

- [#18319](https://github.com/emqx/emqx/pull/18319) Added support for specifying an action-specific project ID for BigQuery Actions.

- [#18624](https://github.com/emqx/emqx/pull/18624) Added `emqx ctl actions show` and `emqx ctl actions status` commands. They report action status for the local node only, in JSON, without REST API credentials or a network call.

  `status` prints a compact JSON array of `{"<type>:<name>": "<status>"}` entries; `show` prints the same information as `GET /api/v5/actions/{id}`, with connector secrets redacted, but for the local node only. Both accept `--name <type:name>` to select one action and `--ns <namespace>` to select a namespace, defaulting to every action in the global namespace.

  This suits a per-node readiness probe, where the REST API's cluster-aggregated `status` field cannot tell whether the local node's own actions are ready to accept traffic.

#### Observability

- [#17493](https://github.com/emqx/emqx/pull/17493) Added session buffer observability: client APIs now report `total_payload_bytes`, `sysmon.session.total_payload_bytes_high_watermark` can emit throttled warnings for sessions over a payload-byte threshold, and `emqx ctl session-top` can export the cluster top sessions by `total_payload_bytes` or `mqueue_length`.

- [#17582](https://github.com/emqx/emqx/pull/17582) Updated Prometheus metrics collection to use `prometheus.erl` version 6.1.2, improving performance and scalability.

  The `vm_dist`, `vm_statistics`, `vm_system_info`, and `vm_memory` collectors are now enabled by default. The obsolete `emqx_vm_process_messages_in_queues` metric was removed from Prometheus output.

- [#17607](https://github.com/emqx/emqx/pull/17607) [#17998](https://github.com/emqx/emqx/pull/17998) Added a v2 topic-metrics surface with named collections, wildcard topic filters, namespace ownership, REST CRUD and a Prometheus scrape endpoint.

  - New routes under `/api/v5/mqtt/topic_metrics2/:name` let operators register topic-metric collections by user-chosen name (`my-pressure`, `vehicle-events`, …) instead of using the topic itself as the identifier. Wildcards (`t/#`, `sensor/+/temp`) are now accepted as topic filters, and a single published message may match and increment several collections.
  - Collections are namespace-scoped: a collection created by a namespaced admin only counts publishers whose `client_attrs.tns` matches. Global collections (created by a non-namespaced admin) count every publisher. Namespaced admins see and modify only their own collections. A global administrator can address an individual namespaced collection on the per-collection endpoints (`GET`, `DELETE`, `PUT .../reset`) by passing an `ns` query parameter; a namespaced admin passing another namespace's name is rejected with `403 Forbidden`, and omitting `ns` keeps the actor's own namespace.
  - Counters are exposed in Prometheus exposition format at `/api/v5/prometheus/topic_metrics` with labels `name`, `topic_filter`, `namespace`. Rates can be derived via Prometheus `rate()`.
  - The v1 API (`/api/v5/mqtt/topic_metrics` and `/api/v5/mqtt/topic_metrics/:topic`) is unchanged and continues to work; it is marked deprecated in its Swagger spec, and integrations should use v2.

- [#18148](https://github.com/emqx/emqx/pull/18148) Added support for Dynatrace integration via OpenTelemetry. Supported signals are traces and logs. The integration uses OAuth2 tokens for authentication.

#### Deployment and Security

- [#17381](https://github.com/emqx/emqx/pull/17381) The OpenAPI specification endpoints now require authentication by default. This covers `GET /api-docs/swagger.json`, `GET /api-spec.json`, `GET /api-spec.md`, and `GET /api-spec/:tag[/:name]`.

  Unauthenticated requests receive a 401 with a `WWW-Authenticate` header and a minimal but valid OpenAPI document (or its Markdown equivalent for `/api-spec.md`) that lists the supported security schemes and the public bootstrap endpoints (`POST /api/v5/login`, `GET /api/v5/status`), so callers can discover how to authenticate without the dashboard exposing its full API surface anonymously.

  The dashboard's `api-spec.html` explorer continues to load anonymously and fetches the spec with the existing session cookie or token.

- [#17407](https://github.com/emqx/emqx/pull/17407) [#17808](https://github.com/emqx/emqx/pull/17808) Added Feature Gates.

  Added support for starting EMQX with a limited set of features specified by the `EMQX_FEATURES` environment variable. Invalid presets or feature names prevent the node from booting. Dependent features are enabled automatically.

  There are two presets available:

  - `FULL`: the default. Starts EMQX with all available features.
  - `ESSENTIAL`: starts EMQX with the minimum feature set: the core MQTT broker with authentication and authorization.

  The available features are:

  - `dashboard`: Dashboard UI (including SSO and RBAC), REST API.
  - `data_integration`: Connectors, Actions, Sources, and Rule Engine.
  - `message_transformation`: Message transformation.
  - `schema_validation`: Schema validation.
  - `schema_registry`: Schema registry.
  - `gateways`: Gateway protocols.
  - `cluster_link`: Cluster linking.
  - `multi_tenancy`: Multi-tenancy and namespacing.
  - `ai`: AI features (A2A registry, AI completion).
  - `metrics`: Prometheus metrics exporting.
  - `mqtt_extensions`: MQTT extensions: delayed publish, topic rewrite, Auto Subscribe, Slow Subscriptions, message queue, and streams.
  - `plugins`: Plugin framework for installing and managing third-party plugins.

  The following features cannot be enabled by themselves and are only enabled when using the full preset:

  - `file_transfer`: File transfer extension to MQTT.
  - `exhook`: External gRPC hooks.
  - `opentelemetry`: OpenTelemetry exporter.

- [#17768](https://github.com/emqx/emqx/pull/17768) Added support for sourcing `node.cookie` from a file using the `file://` URL form.

  Operators can now set `node.cookie = "file:///path/to/cookie"` (or point the `EMQX_NODE__COOKIE` environment variable at a `file://` URL) so the cluster secret is not stored as plain text in the configuration. The referenced path may be a regular file or a FIFO (named pipe); it is read once when the node boots. When a FIFO is used, the orchestrator must write the cookie to it on each boot, before any other `emqx` command is invoked (such as `emqx ctl`), because later commands obtain the cookie from the already-running node rather than re-reading the file.

  The resolved cookie is now passed to the Erlang VM directly and is no longer written to the generated `data/configs/vm.*.args` file, so the secret is not persisted to disk during boot.

- [#17803](https://github.com/emqx/emqx/pull/17803) When EMQX is booted with `EMQX_FEATURES=ESSENTIAL`, the Erlang code loading mode now defaults to `interactive` so that the `.beam` files of disabled features are loaded on demand instead of all at boot. This significantly reduces the resident memory footprint of an essential-mode node, since the modules belonging to skipped features never become resident. The mode can still be overridden by setting `CODE_LOADING_MODE` explicitly.

- [#18451](https://github.com/emqx/emqx/pull/18451) Added support for reading boot-time environment variables from `etc/emqx.env` (`/etc/emqx/emqx.env` on RPM and DEB installations, `/opt/emqx/etc/emqx.env` in the Docker image).

  The file lists `EMQX_FEATURES` and `EMQX_SECURITY_PROFILE` with their defaults commented out and a description of what each one does. These variables are read before `emqx.conf` is parsed, so they cannot be set in `emqx.conf`. The `emqx` command sources the file on every invocation, so a service start, a foreground start, and `emqx ctl` all see the same values. Values in the file override the inherited environment. Package upgrades keep edits to the file.

- [#18452](https://github.com/emqx/emqx/pull/18452) Added `security_profile` and `feature_preset` to the node information returned by `GET /nodes` and `GET /nodes/{node}`.

  `security_profile` is `legacy` or `hardened`. `feature_preset` is `full`, `essential`, or `custom`. Both values are fixed when the node boots, so the list view shows when nodes in a cluster run with different settings. Stopped nodes do not report these fields.

- [#18453](https://github.com/emqx/emqx/pull/18453) Added the `security_profile_divergence` alarm.

  Nodes running the `hardened` security profile (`EMQX_SECURITY_PROFILE`) periodically check the security profile of the other running nodes in the cluster, and raise the alarm when another running node runs the `legacy` profile. Nodes running the `legacy` profile do not run the check, and nodes running an older EMQX release without security profiles count as `legacy`. The alarm message names the `legacy` nodes, and the alarm details keep the current node list while the alarm is active. The alarm clears on its own once every running node runs the `hardened` profile, or once the last `legacy` node leaves the cluster.

  The alarm is expected for a short time during a rolling upgrade that changes the security profile. An alarm that stays active points to nodes that were not restarted with the new `EMQX_SECURITY_PROFILE` value.

- [#18471](https://github.com/emqx/emqx/pull/18471) The `node_dump` diagnostic script now includes the `EMQX_FEATURES` and `EMQX_SECURITY_PROFILE` settings from the boot-time environment file `etc/emqx.env`, when the file exists. Other variables in the file are not collected.

- [#18557](https://github.com/emqx/emqx/pull/18557) [#18609](https://github.com/emqx/emqx/pull/18609) Added the `node.default_listener_address` configuration option. It sets the address of MQTT listeners, gateway listeners, and the Dashboard HTTP listener when their `bind` has no explicit address, such as a bare-port bind (`bind = 1883`). Valid values: `loopback` (bind 127.0.0.1), `nodename` (bind the address in the host part of the Erlang node name, resolving it first when it is not an IP address), `all` (bind 0.0.0.0), a literal IPv4/IPv6 address, or a hostname to resolve at boot. When the option is not set, the security profile decides the default address as before. An explicit `IP:port` bind always wins. The option can also be set with the `EMQX_NODE__DEFAULT_LISTENER_ADDRESS` environment variable.

  The official Docker image sets `EMQX_NODE__DEFAULT_LISTENER_ADDRESS=all`, so defaulted listeners stay reachable through published container ports regardless of the security profile.

  Listener views now report `resolved_address`: the IP address a listener is actually bound to on the node it runs on, alongside the existing `bind` field. `bind` keeps showing the configured value, including the port; `resolved_address` shows the IP `bind` resolves to after the security profile or `node.default_listener_address` applies, without the port, which can differ from the address in `bind` for a bare-port bind such as `bind = 1883`.

  A second field, `resolved_address_from`, reports why `resolved_address` has its value: `bind` when the listener's own `bind` already sets an explicit address, `0.0.0.0` for all interfaces, `127.0.0.1` for loopback, `nodename`, or the literal `node.default_listener_address` value when it is a hostname or IP address.

  Both fields are node-local: `GET /api/v5/listeners/:id` reports the values for the node handling the request, and `GET /api/v5/listeners` reports them per node under `node_status`, since a listener with the same ID can resolve to different addresses on different nodes in the same cluster, for example when `node.default_listener_address` is set to `nodename`.

  `emqx ctl listeners` prints both fields alongside the existing `listen_on`.

- [#18628](https://github.com/emqx/emqx/pull/18628) Data backup exports now record the exporting node's security profile.

  Restoring a backup that was exported under the `legacy` security profile onto a node running the `hardened` security profile can carry over data and configuration that behaves differently once restored:

  - Bare-port MQTT, gateway, and Dashboard HTTP listener binds resolve to loopback instead of all interfaces.
  - An authenticator chain left empty or disabled starts denying every client instead of allowing them.
  - A restored Dashboard account still on the default password can no longer log in.
  - Authentication and authorization backend failures that were previously ignored now deny the operation.

  Importing such a backup now needs the `--allow-security-profile-mismatch` CLI flag, or the `allow_security_profile_mismatch` API parameter, so an operator gets a chance to review these differences instead of discovering them after clients stop connecting or logins start failing. A backup that predates this change is treated the same as one exported under `legacy`. Restoring into a node running `legacy` is never affected.

#### Plugins

- [#18455](https://github.com/emqx/emqx/pull/18455) Under the `hardened` security profile, `emqx ctl plugins allow <Name-Vsn>` now requires the `sha256:<hex>` argument. The grant binds the plugin package to the given SHA-256 digest. Only an upload whose bytes match the digest is installed. A grant without a digest is refused with a message that shows the required command. A node running the `hardened` profile also refuses a grant without a digest that a cluster peer sends to it.

  The `legacy` security profile is unchanged: the `sha256:<hex>` argument stays optional.

#### Packaging

- [#17335](https://github.com/emqx/emqx/pull/17335) After installation from an RPM or DEB package, the directory `/opt/emqx` is created and populated with convenience symlinks (`bin`, `data`, `etc`, `lib`, `log`, `plugins`, `releases`, `erts-*`) pointing at the scattered FHS paths used by the package. Operators can now use the same `/opt/emqx/...` paths as in the official Docker image, regardless of how EMQX was installed.

#### Performance

- [#17583](https://github.com/emqx/emqx/pull/17583) Improved JSON encoding and decoding performance.

  As part of this change, floating-point numbers in JSON output are now formatted consistently with the Erlang/OTP standard, which may differ slightly from previous releases (for example, switching to scientific notation a little earlier).

- [#18033](https://github.com/emqx/emqx/pull/18033) In `ESSENTIAL` feature mode (and any deployment with the dashboard/management client-info API disabled), EMQX no longer performs the periodic per-connection statistics reporting that only feeds the `GET /clients` endpoint, reducing per-connection overhead at high connection counts.

- [#18424](https://github.com/emqx/emqx/pull/18424) Plugin configuration schemas (`config_schema.avsc`) are now read from the installed plugin package each time a plugin configuration is validated, instead of being kept in memory for every installed plugin.

  When the schema file of an installed plugin is missing or unreadable, plugin configuration validation now reports the file error.

- [#18688](https://github.com/emqx/emqx/pull/18688) The number of dirty I/O scheduler threads (`+SDio`) can now be configured via `node.dirty_io_schedulers`. The default `auto` keeps the previous fixed value of 8 on nodes where `node.schedulers` resolves to more than 2, and uses 4 on smaller nodes (for example, a 2 vCPU container or cgroup). This reduces boot-time memory footprint on small nodes while keeping enough threads to overlap blocking I/O operations.

### Bug Fixes

#### Core MQTT Functionalities

- [#18010](https://github.com/emqx/emqx/pull/18010) Malformed MQTT packets sent by clients are no longer logged as broker errors.

  Such invalid input is now counted in the connection shutdown counters of the listener, reducing alert noise from port scanners, protocol fuzzers and misbehaving clients. Parse errors that carry packet-specific detail share a single `frame_error` counter, so malformed packets can no longer create new counter names.

  Details of the parse error, including the offending bytes, are reported through tracing: start a trace on the client ID, IP address or topic to inspect them.

- [#18027](https://github.com/emqx/emqx/pull/18027) Changed shared-subscription handling so EMQX disconnects clients that attempt a shared subscription while shared subscriptions are disabled.

  When `mqtt.shared_subscription` is set to `false` and a client sends a SUBSCRIBE containing a shared topic filter (`$share/...` or `$queue/...`), EMQX now closes the network connection, as required by the MQTT specification for protocol errors. MQTT 5.0 clients first receive a DISCONNECT packet with reason code `0x9E` (Shared Subscriptions not supported); MQTT 3.1/3.1.1 clients simply get the connection closed.

  Previously such a SUBSCRIBE was answered with a failure reason code in the SUBACK and the connection was kept open.

- [#18116](https://github.com/emqx/emqx/pull/18116) When `strict_mode` is enabled (the default), an MQTT v5 packet that includes a non-repeatable property more than once (for example, two `Session-Expiry-Interval` properties in a CONNECT packet) is now rejected as a protocol error, instead of silently using the last value. `User-Property`, which the MQTT specification allows to repeat, is not affected.

  Operators can restore the previous lenient behavior by setting `strict_mode = false` in the listener configuration.

- [#18438](https://github.com/emqx/emqx/pull/18438) Fixed an off-by-one in the size check applied to outgoing packets.

  A packet whose serialized size was exactly the client's `Maximum Packet Size` was discarded and logged as `frame_is_too_large`, instead of being delivered. Only packets larger than the limit are now discarded, which is what MQTT 5.0 requires. Packets below the limit are unaffected.

- [#18470](https://github.com/emqx/emqx/pull/18470) Fixed an issue where EMQX acknowledged but did not deliver a retransmitted QoS 2 PUBLISH packet when the original packet had not been received before the publisher disconnected.

  This fix partially reverts the QoS 2 duplicate-handling change introduced in [#16721](https://github.com/emqx/emqx/pull/16721). After the awaiting-PUBREL state expires, a retransmitted QoS 2 PUBLISH packet is again treated as a new QoS 2 exchange and may be delivered to subscribers more than once. Starting with EMQX 7.0.0, awaiting-PUBREL state expiration will be disabled by default, preventing such redelivery.

- [#18487](https://github.com/emqx/emqx/pull/18487) Reduced log volume for PUBACK, PUBREC, PUBREL, and PUBCOMP packets that carry an unknown Packet Identifier. These events are logged at debug level and remain available through client tracing.

- [#18523](https://github.com/emqx/emqx/pull/18523) Changed shutdown ordering so EMQX stops MQTT listeners before stopping applications.

  Previously, listeners kept accepting and processing client traffic while the applications behind the publish path were already stopped. Publishing clients could then trigger a burst of `hook_callback_exception` errors in the log, for example from the rule engine, until the listeners stopped a few seconds later. Listeners now stop first, so no client traffic is processed during application shutdown.

  The node now also reports itself as not running in `GET /status` as soon as shutdown begins, so load balancers stop routing new connections to it.

- [#18585](https://github.com/emqx/emqx/pull/18585) Fixed session takeover so EMQX ends a session that does not outlive its connection when another connection takes over the same client ID, as the MQTT specification requires. This covers MQTT 5.0 clients connecting with Session Expiry Interval 0 and MQTT 3.1.1 clients connecting with Clean Session 1.

  Before this fix, the new connection could inherit the old session's subscriptions and queued messages, and a will message with a Will Delay Interval greater than zero was silently dropped. Now the new connection starts a fresh session (CONNACK Session Present 0), the old connection receives DISCONNECT with reason code 0x8E (Session taken over), and its will message, if any, is published at the takeover.

#### Access Control

- [#18246](https://github.com/emqx/emqx/pull/18246) Added delayed-message authorization to the hardened security profile.

  - In the hardened security profile, delayed messages are reauthorized when replayed. Delayed messages from MQTT and gateway clients carry a restricted authorization context. EMQX checks current publish authorization rules and ban records before replay. A message that was authorized when scheduled can be dropped when replayed.
  - In the hardened security profile, pending delayed messages created before the upgrade are dropped when replayed because they do not contain an authorization context. The legacy profile continues to replay them.
  - Fixed mountpoint handling in multiple gateways. Gateways consistently pass logical, unmounted topics to authorization. When `authorization.include_mountpoint = false`, EMQX checks the logical topic. When it is `true`, EMQX applies the mountpoint once for the authorization check. In both cases, EMQX applies the mountpoint once before publishing or subscribing.
  - The GBT 32960, JT/T 808, LwM2M, NATS, and STOMP gateways no longer pass pre-mounted topics to publish authorization. This prevents authorization from checking a double-mounted topic when `authorization.include_mountpoint = true`.
  - GBT 32960 `dnstream`, JT/T 808 `proto.dn_topic`, and LwM2M command auto-subscriptions no longer apply the mountpoint before authorization.
  - JT/T 808 `proto.up_topic` and `proto.dn_topic` are now relative to the gateway mountpoint. Their defaults changed from `jt808/${clientid}/${phone}/up` and `jt808/${clientid}/${phone}/dn` to `${phone}/up` and `${phone}/dn`.
  - MQTT-SN idle QoS -1 publishes and will messages now apply the configured mountpoint; these paths previously published without it.
  - NATS publish authorization checks the MQTT topic converted from the NATS subject before applying the mountpoint. NATS JWT permissions and EMQX authorization no longer check a pre-mounted topic.
  - Fixed duplicate processing of delayed messages by other systems, such as bridges, the retainer, and the schema validator. EMQX processes delayed messages only during actual replay. Direct internal publishers (for example, plugins) must invoke the `message.ingress` hook to schedule delayed messages. Direct `emqx:publish/1` calls and management API publishes to `$delayed/...` bypass message ingress and fail to schedule delayed messages.

- [#18458](https://github.com/emqx/emqx/pull/18458) Upgraded `oidcc` to `3.2.3`.

  The upgrade fixed Dashboard SSO (OIDC) login timeouts (`INTERNAL_ERROR: exit,{timeout,{gen_server,call,[...]}}`) while the provider configuration worker was busy refreshing its cached configuration.

- [#18576](https://github.com/emqx/emqx/pull/18576) The OIDC SSO configuration API (`GET /api/v5/sso/oidc`) now returns `client_jwks` as `none` when no client JWKS is configured, matching the CLI output. Previously the value was masked as `******` even when nothing was configured. A configured client JWKS remains masked.

- [#18580](https://github.com/emqx/emqx/pull/18580) Redacted sensitive configuration values in the `conf.hocon` file produced by the `bin/node_dump` script.

  Values marked as sensitive in the configuration schema, such as `dashboard.default_password` and `license.key`, are now written as `******`. Before this fix, the script redacted only a fixed list of key names, so these values were written in plain text.

#### Multi-tenancy

- [#18423](https://github.com/emqx/emqx/pull/18423) Data Backup imports performed by a namespaced administrator now apply only that namespace's configuration. Cluster-wide settings found in the namespaced configuration, such as authentication, authorization, ExHook, or listeners, are skipped with a warning instead of being written to the global configuration.

- [#18466](https://github.com/emqx/emqx/pull/18466) Fixed listing of backup files for namespaces whose names contain special characters.

  Previously, the backup file list was empty for a namespace whose name contained characters such as `*`, `?`, `{`, `}`, `[` or `]`, even though the backup files existed on disk. The listing now treats the namespace name as a literal directory name.

- [#18539](https://github.com/emqx/emqx/pull/18539) Fixed the multi-tenancy client list not following a persistent session that reconnects under a different namespace.

  Previously, when a client resumed an existing session (`clean_start=false`) after its namespace changed, `GET /api/v5/mt/ns/{ns}/client_list` kept listing the client under the old namespace, and the new namespace's list did not include it. The client list and the per-namespace client count now always reflect the namespace the client connected with. This also fixes the client disappearing from the list after resuming a durable session.

#### Data Integration

- [#18300](https://github.com/emqx/emqx/pull/18300) Blank certificate file fields in a connector's TLS settings are now treated as not configured regardless of the `verify` mode. Previously, blank client certificate fields were rejected with a validation error when `verify` was set to `verify_peer`, although client certificates are optional for peer verification.

- [#18392](https://github.com/emqx/emqx/pull/18392) Fixed an issue where aggregated Actions (S3, S3Tables, Azure Blob Storage, Snowflake Aggregated) with the same name but in different namespaces would share the same working directory for their temporary files.

- [#18449](https://github.com/emqx/emqx/pull/18449) Fixed a rare race condition in which the PostgreSQL Action encountered a `sock_closed` error while writing data and incorrectly treated it as unrecoverable. EMQX treats the error as recoverable.

- [#18767](https://github.com/emqx/emqx/pull/18767) Fixed the RocketMQ connector being reported as belonging to a namespace it does not belong to.

  The RocketMQ connector has its own `namespace` configuration field, holding the RocketMQ instance namespace. Connector API responses returned that value under the same JSON key used for the EMQX namespace, so the Dashboard treated the connector as owned by a namespace of that name. It showed "Only the administrator of namespace <name> can perform operations on the connector", and opening the connector failed with "Managed namespace not found".

  The `namespace` field in connector API responses now always holds the EMQX namespace. The RocketMQ instance namespace is no longer returned, and it is kept unchanged when a connector is updated without it.

#### Rule Engine

- [#18527](https://github.com/emqx/emqx/pull/18527) Fixed repeated `badarg` errors in the log when a message was published while the schema validation, message transformation, or rule engine topic index table was unavailable. Such a publish now proceeds as if no validation, transformation, or rule matched the topic, and the broker logs a throttled `topic_index_table_missing` message instead of one error per publish. The index tables now also survive a restart of their owner process, and the hooks are removed before the tables during application shutdown, which removes the known windows where a publish could find a table missing.

#### Clustering

- [#18409](https://github.com/emqx/emqx/pull/18409) Fixed Cluster Linking for a link whose `server` field lists more than one address.

  Such a link now connects through all listed addresses: each connection prefers one of the addresses in turn and fails over to the others when it cannot connect. Before this change the link could not connect, and no link could be created, updated or deleted until the node was restarted.

- [#18447](https://github.com/emqx/emqx/pull/18447) Fixed an issue where changes to `base.hocon` could be ignored after a node synchronized configuration from another cluster member. Configuration synchronization no longer persists the peer's `base.hocon` values into `cluster.hocon`, so local `base.hocon` changes take effect after restart unless explicitly overridden by cluster configuration.

- [#18537](https://github.com/emqx/emqx/pull/18537) Fixed Cluster Linking to classify temporary message-forwarding connection errors as recoverable. Messages affected by transient network outages are now buffered and retried instead of being counted as failed.

#### Gateway

- [#18312](https://github.com/emqx/emqx/pull/18312) Fixed an issue in plaintext CoAP UDP listeners with connection mode enabled where a rejected request from another source could redirect subsequent downlink messages.

- [#18436](https://github.com/emqx/emqx/pull/18436) Fixed an issue where NATS Gateway internal JWT authentication did not enforce account JWT `exp`/`nbf` claims or account-level user revocations.

  Expired or not-yet-valid account JWTs and user JWTs revoked by the account are now rejected during authentication. Existing connections are disconnected when the earlier of the user JWT or account JWT expiration is reached. Malformed resolver-preloaded account JWTs are rejected during gateway configuration validation.

- [#18494](https://github.com/emqx/emqx/pull/18494) Fixed CoAP gateway clients reporting the internal keepalive check interval instead of the configured heartbeat interval.

  The client information returned by the gateway API and `emqx ctl gateway-clients list coap` now reports the configured heartbeat value in seconds.

- [#18504](https://github.com/emqx/emqx/pull/18504) Fixed STOMP frame parsing of escaped header characters and CRLF line endings.

  The STOMP gateway now decodes the escape sequences `\c`, `\r`, `\n`, and `\\` in header names and values, as required by STOMP 1.2. CONNECT and CONNECTED frames are exempt: STOMP 1.2 excludes them from header escaping for backward compatibility with STOMP 1.0, so their headers, including a password containing a colon or a backslash, pass through unchanged. In other frames, an undefined escape sequence is now rejected as a frame error.

  The gateway now also accepts CRLF (`\r\n`) line endings in frames and CRLF heartbeats. Before this fix, clients using CRLF line endings could not connect.

#### Plugins

- [#18188](https://github.com/emqx/emqx/pull/18188) Hardened the plugin framework package and runtime integrity checks.

  - Operators must run `emqx ctl plugins allow` before `emqx ctl plugins install`, enforcing the same allow gate as the HTTP upload API.
  - Plugin API callback responses are restricted to an allow-list of safe response headers; browser-sensitive headers (such as `set-cookie`, `location`, `access-control-*`, `content-security-policy`, and other authentication or security policy headers) and custom headers without the `x-plugin-` prefix are stripped.
  - Added the `plugins.package_limits` configuration to bound plugin package extraction: `max_package_size` (default `10MB`), `max_decompressed_size` (default `50MB`), `max_file_count` (default `10000`), `max_path_depth` (default `32`), and `max_extraction_time_ms` (default `60s`, also used as the RPC timeout for cluster package copying). Packages violating these limits are rejected before or during extraction. Tar entries escaping the install directory (path traversal) are also rejected.

- [#18468](https://github.com/emqx/emqx/pull/18468) The hot-upgrade (relup) plugin now validates the target version string and checks upgrade-path compatibility before it modifies any files. An incompatible or malformed upgrade package is rejected without deleting or overwriting the installed release.

- [#18540](https://github.com/emqx/emqx/pull/18540) Shipped the default configuration file (`priv/config.hocon`) in the `emqx_relup` plugin package. Installing the plugin no longer logs a repeated `failed_to_copy_plugin_default_hocon_config` warning.

#### ExHook

- [#18464](https://github.com/emqx/emqx/pull/18464) Fixed a rare crash in the ExHook manager when an ExHook server became unhealthy during a configuration update. The manager now keeps the configured server order and continues serving configuration changes while the server reconnects.

- [#18473](https://github.com/emqx/emqx/pull/18473) Fixed ExHook authentication and authorization behavior when no callback server is running. The legacy security profile now honors the configured `failed_action`, while the hardened security profile remains fail-closed.

#### Observability

- [#17602](https://github.com/emqx/emqx/pull/17602) Added configuration-backed `emqx ctl log outputs` commands so CLI changes to logger outputs stay consistent with logger configuration managed by the HTTP API and Dashboard.

- [#17912](https://github.com/emqx/emqx/pull/17912) Fixed a security vulnerability in OpenTelemetry W3C Baggage header extraction (GHSA-64w2-whjg-q7q7). Previously, the inbound `baggage` header was decoded with no byte or entry-count limit, and a malformed key-value pair would crash the process. Extraction is now capped at 8192 bytes and 180 entries as recommended by the W3C Baggage specification, and malformed pairs are skipped instead of crashing.

- [#18521](https://github.com/emqx/emqx/pull/18521) Added an identifying `label` to the connection shutdown report emitted when a connection exceeds a force-shutdown limit (`force_shutdown.max_mailbox_size` or `force_shutdown.max_heap_size`).

  For an established connection, the `label` holds the client ID. For a connection shut down before CONNECT completes, it holds the listener name and peer address. Previously, the report contained only the limit and the measured value, so the operator could not identify the affected connection.

- [#18696](https://github.com/emqx/emqx/pull/18696) Fixed an issue where querying the audit log could return an error for records created by SSO-authenticated users.

#### Management

- [#18289](https://github.com/emqx/emqx/pull/18289) Fixed an issue where JSON-compatible Unicode escape sequences in quoted HOCON strings and keys were left escaped instead of being decoded.

- [#18403](https://github.com/emqx/emqx/pull/18403) Fixed `emqx ctl` commands printing non-ASCII characters as `\x{...}` escapes or invalid bytes when the command runs in a shell without a UTF-8 locale (for example over non-interactive SSH, cron, or `LANG` unset).

  The Erlang VMs started by the `emqx` script (the node itself, `emqx ctl`, `emqx eval`, `emqx remote_console`, and `emqx escript`) now always read and write standard I/O as UTF-8.

- [#18444](https://github.com/emqx/emqx/pull/18444) Fixed the byte-size units `b` and `B` requiring quotes in configuration files.

  `max_packet_size = 1MB` was accepted, but `max_packet_size = 1B` failed to parse and had to be written as `"1B"`. All byte-size units are now accepted without quotes.

- [#18509](https://github.com/emqx/emqx/pull/18509) Fixed message paging in `GET /clients/{clientid}/mqueue_messages` and `GET /clients/{clientid}/inflight_messages`.

  These APIs limit the total payload size of one response page by the `max_payload_bytes` parameter (default 1MB). When this limit cut a page short, the returned `meta.position` pointed past the messages that were left out, so requesting the next page from that position skipped them. This could look like lost messages, for example a `mqueue_len` count higher than the number of messages the API returns. Now `meta.position` points at the last returned message, and the next page continues with the first message that was left out.

- [#18544](https://github.com/emqx/emqx/pull/18544) Fixed `GET /clients_v2` returning a cursor after returning all clients with memory sessions. Following that cursor returned an empty page. The API no longer returns a cursor when no more data is available.

- [#18558](https://github.com/emqx/emqx/pull/18558) Fixed `GET /clients_v2` ignoring the `fields` query parameter.

- [#18590](https://github.com/emqx/emqx/pull/18590) Fixed the output of `emqx stop` when the node is not running.

  The command reported `Node <name> not responding to pings.` twice and then failed with `Graceful shutdown failed PID=[]`. It now reports the unreachable node once and does not print a shutdown failure for a node it could not find. The exit code is unchanged.

- [#18619](https://github.com/emqx/emqx/pull/18619) Fixed `GET /nodes/{node}` returning a 500 Internal Server Error instead of a 400 Bad Request when the target node becomes unreachable between the API's liveness check and the RPC that fetches its info, for example when the node concurrently leaves the cluster.

#### Deployment and Security

- [#17921](https://github.com/emqx/emqx/pull/17921) Upgraded the `protobuf` dependency to v0.17.0. This dependency is used only for SBOM generation and is not part of the EMQX runtime. The upgrade picks up a fix for an unbounded-recursion denial of service when decoding deeply-nested messages (GHSA-rv48-qqj5-crxg) and Elixir 1.19/1.20 compiler warning fixes, and replaces the previously pinned development ref with the official release.

- [#18706](https://github.com/emqx/emqx/pull/18706) Avoid logging sensitive information in debug mode.

  Running `bin/emqx` commands with `DEBUG=1` or `DEBUG=2` no longer prints the Erlang cookie or the license key in the shell trace output.

## 6.2.3

*Release Date: 2026-08-21*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.2.3.

### Enhancements

#### Access Control

- [#17813](https://github.com/emqx/emqx/pull/17813) The Dashboard user and API key endpoints now reject scope lists that mix privilege scopes (`system`, `user_management`, `api_key_management`, `sso_management`) with other scopes. Each of the four privilege scopes is administrator-equivalent in effect, so combining them with a restricted scope list cannot meaningfully restrict the account. Use either a privilege-only scope list or a scope list that contains only non-privilege scopes, depending on whether the account should have administrator-equivalent capability. Pre-existing records with a mixed scope set continue to function until the next update; the next update must split the list to succeed.
- [#17980](https://github.com/emqx/emqx/pull/17980) In the hardened security profile, EMQX now applies topic validation, authorization, MQTT capability checks, and client subscribe hooks to server-initiated subscriptions.
- [#18002](https://github.com/emqx/emqx/pull/18002) Enabled SAML response and assertion signature verification by default in the hardened security profile.
- [#18296](https://github.com/emqx/emqx/pull/18296) Added a new `is_jwt(value)` function usable in authenticator `precondition` expressions (and anywhere variform expressions are accepted). It returns true only when the value is structurally a JWT (JWS compact form), without verifying the signature. This lets a JWT authenticator be skipped cleanly for clients that present a non-JWT password. This is useful for chains serving both JWT and legacy credential clients, especially under the hardened security profile where a non-JWT credential would otherwise be rejected by the JWT authenticator.

#### Multi-Tenancy

- [#17732](https://github.com/emqx/emqx/pull/17732) Added a `namespace` field to the API key creation and update endpoints, so operators no longer need to encode the namespace inside the `role` string (the existing `ns:<namespace>::<role>` form keeps working). When both forms are supplied they must agree.
- [#17855](https://github.com/emqx/emqx/pull/17855) Namespace-scoped dashboard administrators can now create, list, read, update, and delete API keys within their own namespace. They cannot create global API keys or keys in another namespace, and API keys outside their namespace are hidden from them.

#### Data Integration

- [#17933](https://github.com/emqx/emqx/pull/17933) RabbitMQ connector supports a multi-node `servers` list (e.g. `rmq1:5672,rmq2:5672`) with connect-time failover and rotated pool start offsets. Legacy `server`/`port` remain when `servers` is unset.

- [#17944](https://github.com/emqx/emqx/pull/17944) Added OAuth2 Client Credentials authentication to the HTTP connector and HTTP authentication/authorization. When enabled, EMQX obtains and refreshes an access token from the configured token endpoint and adds it to outbound requests as a Bearer authorization header.

  The connector health check reports `disconnected` when a token cannot be obtained. Configurations that enable OAuth2 and also provide an `Authorization` header are rejected.

  EMQX sends the client ID and client secret as form fields in the token request body. Sending the credentials in the HTTP Basic `Authorization` header is not supported.

- [#18014](https://github.com/emqx/emqx/pull/18014) Datalayers Arrow Flight connector now enables automatic rebuild of prepared statements. When the server loses a prepared statement (e.g., after restart), the client will automatically recreate it and retry the write operation, avoiding write failures.

- [#18042](https://github.com/emqx/emqx/pull/18042) Added AWS IAM role credential support to DynamoDB connectors.

  When both the access key ID and secret access key are omitted, EMQX obtains temporary credentials from an ECS task role or EC2 instance metadata and refreshes them before they expire.

- [#18081](https://github.com/emqx/emqx/pull/18081) Improved resilience of Snowflake Streaming actions. Under certain error types when appending rows, specifically when the channel's internal state becomes out of sync, the action retries the failed rows and attempts to re-open the channel without manual intervention.

- [#18085](https://github.com/emqx/emqx/pull/18085) Added new configuration options for the Kafka, Confluent, and Azure Event Hubs producers:

  - `max_batch_age` (action): drop messages that stay in the producer buffer longer than this duration instead of sending them; counted in the `dropped.expired` metric. Default: `infinity` (never drop).
  - `max_retries` (action): drop a message batch after this many failed retries; counted in the `failed` metric. The retry counter is incremented only when Kafka explicitly responds with an error code; resends after a connection loss do not increment it. Default: `infinity` (retry forever).
  - `reconnect_delay` (action): delay before the producer reconnects after a connection loss. Default: `2s` (previously hard-coded).
  - `request_timeout` (connector): how long to wait for a reply from Kafka before the connection is considered stale and gets re-established. Default: `30s`.

  Additionally, the Kafka client library upgrade (wolff 4.2.1) restores `max_linger_time` support for memory-mode buffers: an under-sized batch now waits up to `max_linger_time` for more messages, reducing the produce request rate; full batches are sent without delay.

- [#18110](https://github.com/emqx/emqx/pull/18110) Added support for JSON Schema drafts 2019-09 and 2020-12 in Schema Registry.

- [#18137](https://github.com/emqx/emqx/pull/18137) The GCP Pub/Sub producer and consumer now accept a fully-qualified topic path (`projects/<project-id>/topics/<topic-name>`) in the topic configuration, making it possible to publish to or consume from a topic that lives in a different GCP project than the service account's own. A bare topic name keeps resolving against the service account's project as before. For consumers, the subscription is still created in the service account's project; only the topic reference may point to another project.

#### Rule Engine

- [#18253](https://github.com/emqx/emqx/pull/18253) Added two Rule-Engine SQL functions: `map_to_range(value, min, max)` and `hash_to_range(value, min, max)`. They map a value (or its hash) into an inclusive integer range, which is useful for sharding or bucketing. For example, you can distribute a large device fleet across several rules by deriving a shard index from a topic segment: `hash_to_range(nth(2, tokens(topic, '/')), 0, 3)`.
- [#18306](https://github.com/emqx/emqx/pull/18306) Added the `lz4_compress` and `lz4_uncompress` rule functions for LZ4 Frame compression and decompression.

#### Plugins

- [#18012](https://github.com/emqx/emqx/pull/18012) Added the `emqx_sync_request` plugin for synchronous MQTT request/response flows through the EMQX REST API. It also provides node-local CLI diagnostics for request counters and current pending state.

- [#18353](https://github.com/emqx/emqx/pull/18353) Added a new plugin `emqx_maptabs` providing named mapping tables for rule SQL.

  Tables are seeded from JSON files and held in memory for fast lookups from the rule engine hot path. The new `maptab_lookup(Table, Key)`, `maptab_lookup(Table, Key, Field)`, and `maptab_lookup(Table, Key, Field, Default)` rule SQL functions turn long `CASE ... WHEN ... THEN` ladders into a single table lookup; the looked-up fields can drive the builtin `subbits` function directly to decode binary payloads.

  Tables are managed with the `emqx ctl maptabs` CLI: loading or deleting a table on one node replicates the change to every node in the cluster, and a node that was down during an update catches up automatically when it rejoins.

  The plugin configuration provides safety limits: `max_tables` (default 100), `max_rows_per_table` (default 10000), and `max_table_file_bytes` (default 10000000).

#### Packaging

- [#18037](https://github.com/emqx/emqx/pull/18037) Added Enterprise Linux 10 (EL10) packages, for Red Hat Enterprise Linux 10, Rocky Linux 10, and compatible distributions.
- [#18127](https://github.com/emqx/emqx/pull/18127) Started releasing macOS 26 (Tahoe) packages.

#### Performance

- [#18185](https://github.com/emqx/emqx/pull/18185) Improved deep-page queries in the subscriptions HTTP API by accumulating in-memory subscription rows on each target node, avoiding one RPC per pagination batch.
- [#18229](https://github.com/emqx/emqx/pull/18229) Reduced CPU overhead on the data-integration send path. The broker no longer builds a formatted error string for every message routed through a resource that is not an action or source (for example, cluster-link message forwarding), which could previously trigger long-scheduler warnings under high message volume.

### Bug Fixes

#### Core MQTT Functionalities

- [#17895](https://github.com/emqx/emqx/pull/17895) [#18062](https://github.com/emqx/emqx/pull/18062) Switching a TLS/WSS listener from a managed certificate bundle back to file-based certificates now succeeds even if the referenced bundle has already been removed, including when the request clears `managed_certs` by sending it as `null` (as the Dashboard does).

- [#17911](https://github.com/emqx/emqx/pull/17911) Allowed DTLS listeners to validate the `ECDHE-PSK-CHACHA20-POLY1305` cipher suite when the runtime OTP ssl application supports it.

- [#18102](https://github.com/emqx/emqx/pull/18102) Fixed an issue where MQTT clients could receive QoS 1 and QoS 2 messages out of order when a delivery rate limit was active. EMQX now keeps later messages queued until the blocked message can be sent.

  Affected versions: 6.1.2, 6.1.3, and 6.2.0 to 6.2.2. Only sessions with a delivery rate limit configured (`delivery_messages_rate` or `delivery_bytes_rate`) are affected; no delivery rate limit is configured by default.

- [#18108](https://github.com/emqx/emqx/pull/18108) Deleting a managed certificate bundle (or a single file in it) that is still referenced by some configuration now always fails with a clear error listing the referencing configurations; the `force_delete` query parameter no longer bypasses this check and has been removed from the API schema.

  Additionally, the Prometheus stats endpoint no longer fails entirely when a listener references a certificate bundle that is missing from disk; the affected listener is skipped in the certificate expiry metric and a warning is logged.

- [#18111](https://github.com/emqx/emqx/pull/18111) When `mqtt.strict_mode` is enabled, MQTT v3.1 CONNECT packets that set the password flag without the username flag are now rejected, matching the existing behavior for MQTT v3.1.1. The MQTT v3.1 specification states that it is not valid to supply a password without a user name.

  Additionally improved connection log readability: the CONNECT packet trace now prints `Password=undefined` when no password was supplied (previously indistinguishable from an empty password), and the `peername` field in logs is now always rendered as a plain string such as `10.0.0.1:54123`.

- [#18181](https://github.com/emqx/emqx/pull/18181) Fixed an issue where rate limiters configured with a burst value of `0` could still allow an extra burst of traffic. This made limits such as MQTT delivery message rate limits less strict than configured.

- [#18236](https://github.com/emqx/emqx/pull/18236) Fixed an issue where clients using socket-backed TCP listeners could be unexpectedly disconnected under high load, due to occasional readiness signals arriving for not-yet-ready sockets.

  ```
  [error] crasher: initial call: emqx_socket_connection:init/4, ..., error: {{case_clause,{select,{select_info,recv,#Ref<...>}}},[{emqx_socket_connection,handle_msg,2,[{file,"emqx_socket_connection.erl"},{line,827}]}, ...
  ```

- [#18293](https://github.com/emqx/emqx/pull/18293) Upgraded the QUIC stack to quicer-0.4.8 (msquic 2.5.7).

- [#18357](https://github.com/emqx/emqx/pull/18357) [#18375](https://github.com/emqx/emqx/pull/18375) MQTT connections are now refused until node startup completes, so listeners no longer serve traffic before authentication, authorization, and plugin hooks are active.

  The `GET /status` API now returns HTTP 503 until startup completes, so load balancers can route new connections to other nodes in the cluster.

  A cluster join request toward a node that has not finished starting is now refused with a message that asks to retry later.

#### Durable Storage

- [#18143](https://github.com/emqx/emqx/pull/18143) Fixed an issue where durable shared subscriptions could fail to communicate with the shared-subscription leader when subscribers were connected to a different node. This could cause an unexplained spike in CPU usage.

#### Rule Engine

- [#17957](https://github.com/emqx/emqx/pull/17957) Fixed an issue where multiple rule events (for example, `$events/client/connack`) would not trigger rules in the global namespace when `rule_engine.limit_selects_in_namespace = true`.
- [#18049](https://github.com/emqx/emqx/pull/18049) Fixed an issue where setting `rule_engine.limit_selects_in_namespace = true` would prevent alarm activated/deactivated-triggered global rules from firing.
- [#18388](https://github.com/emqx/emqx/pull/18388) Fixed the Rule Engine `republish` action for rules that belong to a namespace. When `rule_engine.limit_selects_in_namespace` is enabled (the default), the republished message is now published under the rule's namespace (`<namespace>/<topic>`). This makes the `republish` action follow the same namespace boundary as the rule itself. A rendered topic that already starts with `<namespace>/` is published unchanged, so republish templates that add the prefix themselves keep working. Previously, the message was published to the rendered topic without the namespace prefix. Setting `rule_engine.limit_selects_in_namespace = false` keeps the previous behavior.

#### Data Integration

- [#17859](https://github.com/emqx/emqx/pull/17859) Fixed the MQTT connector so it can connect to IPv6 brokers.

  Previously, configuring an MQTT connector to an IPv6 broker failed in two ways: an IPv6 literal such as `[::1]:1883` was rejected at save time with a `bad_host_port` validation error, and a hostname that only resolves to an IPv6 (`AAAA`) address failed to connect with a "Could not resolve host" error because the connection defaulted to IPv4.

  The server address parser now accepts bracketed IPv6 literals (for example `[::1]`, `[::1]:1883`, and `mqtt://[::1]:1883`), and the MQTT connector now enables IPv6 probing when connecting, so IPv6-only brokers can be reached.

  The MQTT connector and cluster link `server` address now accept the official MQTT URI schemes `mqtt` (plain TCP) and `mqtts` (TLS), for example `mqtt://broker:1883` and `mqtts://broker:8883`. A scheme-less `host:port` is still accepted. Any other scheme is now rejected with an `unsupported_scheme` validation error.

- [#17947](https://github.com/emqx/emqx/pull/17947) Fixed an issue where updating an HTTP connector could leave its action buffer workers blocked after the connector was recreated, causing messages to remain queued until the next retry interval.

- [#17955](https://github.com/emqx/emqx/pull/17955) Fixed GreptimeDB async batches that could remain unflushed after health checks at low write rates.

- [#17961](https://github.com/emqx/emqx/pull/17961) Fixed an issue where a Kafka or Pulsar Connector would transition to a `disconnected` state on health check timeouts, potentially recreating its internal queue. Now, they transition to `connecting`.

- [#17970](https://github.com/emqx/emqx/pull/17970) When SSRF protection is enabled, managing connectors is no longer disrupted by an existing connector whose address is now blocked by the policy.

  Previously, enabling SSRF protection (or extending its deny list) after connectors were created could make unrelated connector operations fail with an internal error, and deleting an affected connector could leave it behind after its actions and rules were already removed.

  SSRF protection now applies to HTTP and MQTT connectors and is enforced when a connector is created or updated: creating or updating such a connector with a blocked address is rejected. Enabling, disabling and deleting connectors are never blocked, and other connector types are not subject to the policy.

- [#17973](https://github.com/emqx/emqx/pull/17973) Fixed Kafka producer action retry metrics. The `retried`, `retried.success`, and `retried.failed` counters on an action's metrics now reflect messages that the internal buffer re-sends after a broker reconnect, so an operator can tell whether retried messages ultimately succeeded or failed. Previously these counters stayed at `0` regardless of how many internal retries occurred. The `success` and `failed` counters are unaffected and are not double-counted.

- [#17982](https://github.com/emqx/emqx/pull/17982) GCP PubSub Consumer now uses HTTP2 and cancels its pull request when it reaches the timeout. This signals more cleanly to the GCP server that it may lease the messages to a new pull request, reducing tail latencies.

- [#18055](https://github.com/emqx/emqx/pull/18055) Fixed an issue where Snowflake Streaming Actions on different nodes in a cluster would start to fail with the following error:

  ```
  {unrecoverable_error,#{body => <<"{\"code\":\"STALE_CONTINUATION_TOKEN_SEQUENCER\",\"message\":\"Channel sequencer in the continuation token is stale. Please reopen the channel\"}">>,...
  ```

- [#18110](https://github.com/emqx/emqx/pull/18110) Fixed an issue where using the `examples` annotation in a draft-06 JSON Schema in Schema Registry would result in valid data being rejected as invalid.

- [#18174](https://github.com/emqx/emqx/pull/18174) The MQTT connector now reports a clear error message when the server address scheme is inconsistent with the SSL settings, for example an `mqtts://` (TLS) address while SSL is disabled.

  Previously, such a configuration failed with an internal error and a noisy log, because the connector attempted a plain TCP connection to a TLS port and could not interpret the server's reply. Connection attempts that receive non-MQTT data from the server (for example, when the port expects TLS) now also produce a clear explanation instead of an internal error.

- [#18193](https://github.com/emqx/emqx/pull/18193) Fixed an issue where a running GCP Pub/Sub Consumer source could show as `disconnected` (with reason `timeout`) after "Test Connection" was used on a GCP Pub/Sub Consumer connector or source, and would stay that way until manually disabled and re-enabled.

  Affected versions: 6.1.3 and 6.2.2.

  The temporary worker pool created for the connection test shared its health-status bookkeeping with the pools of running sources, so cleaning up the test pool also discarded the running source's health status. Each pool now keeps its own bookkeeping, and testing a connection no longer affects running sources. A hot-upgrade hook is included so consumers started by older versions are restarted to pick up the new bookkeeping.

- [#18198](https://github.com/emqx/emqx/pull/18198) Fixed two JSON Schema Registry issues:

  - Schemas containing non-ASCII characters (for example Chinese property names or example values) can now be registered through the HTTP API. Previously, registration failed with an internal `badarg` error.
  - `$ref` references pointing at definition names containing non-ASCII characters now resolve correctly during validation and decoding, both in percent-encoded form (for example `#/definitions/%E5%A7%93%E5%90%8D%E7%B1%BB%E5%9E%8B`) and in raw UTF-8 form. Previously, such references failed to resolve, and decoding failed with an internal `badmatch` error.

  In addition, a payload that does not conform to its JSON schema now produces a clear schema validation error during Rule Engine decoding instead of an internal error.

- [#18242](https://github.com/emqx/emqx/pull/18242) Fixed Datalayers connectors failing with `function_clause` when database or credentials are left blank. A clear configuration error is reported instead.

- [#18270](https://github.com/emqx/emqx/pull/18270) Fixed GreptimeDB connectors that could fail to restart when a stale gRPC channel remained after a worker was force-stopped.

- [#18274](https://github.com/emqx/emqx/pull/18274) Fixed the Tablestore connector health check listing all timeseries tables on every check. Health checks now use a `DescribeTimeseriesTable` probe against the configured `probe_table_name`, falling back to listing all timeseries tables when it is unset.

- [#18299](https://github.com/emqx/emqx/pull/18299) Fixed an issue where the Snowflake connector's configured TLS (`ssl`) settings were not applied to its outbound HTTPS connections (both Streaming and Aggregated modes). Settings such as `verify`, `cacertfile`, client certificates, and `server_name_indication` were accepted and displayed but had no effect on the actual connections. The configured values are now honored. Connectors that never customized the `ssl` settings keep the previous connection behavior.

- [#18302](https://github.com/emqx/emqx/pull/18302) Elasticsearch action `index` and `id` values are now URL-encoded when composing the request path, so characters such as `#` or `/` in a templated value are treated as literal text within a single path segment instead of altering the request target. The JSON request body is not affected.

- [#18303](https://github.com/emqx/emqx/pull/18303) Sparkplug B alias-to-name mappings are now maintained only for messages published directly by MQTT clients. Messages arriving through bridges or other internal paths no longer share alias mappings, which prevents one publisher's mapping from being applied to another publisher's decoded metrics. As a consequence, `spb_decode` no longer resolves aliases to metric names for data messages ingested through an MQTT bridge.

#### Clustering

- [#17995](https://github.com/emqx/emqx/pull/17995) Fixed an issue that could terminate a node while it joined a cluster whose persisted `mqtt.max_packet_size` differed from its local configuration. EMQX now skips listener refresh side effects before listener startup and creates the listeners from the synchronized configuration when the EMQX application starts.

- [#17999](https://github.com/emqx/emqx/pull/17999) Fixed a startup crash-loop that could occur when a node using the community (single-node) license joins a cluster whose peers hold a clustering-capable license.

  Previously, if cluster membership was established before the peer's license was replicated to the joining node, the node would refuse to start with a `SINGLE_NODE_LICENSE` error and, under an automatic-restart supervisor, keep crash-looping. The node now waits a bounded grace period for the clustering license to sync before it starts. A cluster in which no node ever obtains a clustering license is still rejected after the grace period elapses.

- [#18077](https://github.com/emqx/emqx/pull/18077) Fixed a crash when a node received a `cluster join` request (CLI or API) before it had fully booted: joining restarts the internal database while applications are still starting, which could bring the whole node down. Such requests are now rejected with a clear error message; retry after the node is fully started.

- [#18277](https://github.com/emqx/emqx/pull/18277) Improved reliability of persisting configuration changes to `cluster.hocon`: the update is now written and synced to disk before atomically replacing the file, and a failure to read the previous file for backup no longer prevents the new configuration from being saved.

- [#18287](https://github.com/emqx/emqx/pull/18287) Improved REST API resilience when a cluster node becomes unreachable or fails while serving a request. When an RPC to a peer node did not complete, a number of endpoints previously returned an opaque 500 error or, in a few cases, reported success while part of the work had failed. These endpoints now return a descriptive error response, and cluster-wide reads degrade gracefully to the results from the reachable nodes.

  Affected areas include: listing and describing plugins, listing clients (v2), streaming and downloading trace logs, reading configuration in HOCON format from a specific node, deleting a delayed message on a specific node, resetting topic metrics, importing a data backup, per-node action/source operations, rule listing, file-transfer downloads, and deleting message queues. Retained-message reindexing and session takeover also now tolerate an unreachable peer node instead of aborting.

- [#18347](https://github.com/emqx/emqx/pull/18347) Fixed a problem with the Mnesia RocksDB backend that caused table inconsistency on core nodes when keys were deleted while a core node was down.

  From the EMQX point of view, this problem could lead to delayed release of dashboard login locks, as well as wasted disk space by the EMQX schema registry, since deletion of old schemas could be missed.

- [#18383](https://github.com/emqx/emqx/pull/18383) Fixed an issue where submitting a configuration containing an invalid Unicode escape sequence through `PUT /configs` returned an internal error. Such requests now return a validation error that identifies the invalid escape sequence.

#### Access Control

- [#17806](https://github.com/emqx/emqx/pull/17806) Aligned the data backup import and export endpoints with the principle of least privilege: Dashboard users whose scope set does not include both `user_management` and `api_key_management` can no longer import or export archives containing the `dashboard_users` or `api_keys` table sets. Global administrators and API key callers with the necessary scopes are unaffected.
- [#17853](https://github.com/emqx/emqx/pull/17853) Improved redaction of sensitive HTTP request headers in connector debug logs. `x-api-key`, `x-auth-token`, `api-key`, and `cookie` headers are now stored as secrets in connector state (matching the existing behavior for `Authorization` / `Proxy-Authorization`), so their values are not printed when connector state is emitted at trace / debug level. In addition, the shared header-redaction helper now recognizes header names that are stored as iolists (a shape produced by the connector's template parser), which previously slipped through the sensitivity check.
- [#17871](https://github.com/emqx/emqx/pull/17871) Creating a super-user in a non-global namespace is now rejected when importing built-in-database users in bulk or via a bootstrap file, matching the per-user management API. Such rows are reported as failed and are not stored.
- [#17974](https://github.com/emqx/emqx/pull/17974) Raw MQTT packet data is now redacted by default in connection logs; trusted client IP addresses can be allowlisted per listener for diagnostics.
- [#18005](https://github.com/emqx/emqx/pull/18005) Fixed an issue where CLI audit logs could store sensitive command arguments.
- [#18009](https://github.com/emqx/emqx/pull/18009) Made scope handling consistent for administrator and API key records that use their role's implicit default scopes (shown as `unset`). Reads and writes now accept the unset-equivalent scope list, and such records keep their forward-compatible implicit scopes instead of a frozen list, so they automatically gain scopes introduced in future releases.
  - Editing only the note (description) of the default administrator via the Dashboard user API no longer fails; the user API now treats a scope list that matches the role's implicit full set (and the `unset` value) as "no explicit scopes".
  - [#18196](https://github.com/emqx/emqx/pull/18196) The API key create and update requests accept the same unset-equivalent scope list, so re-submitting the value returned by a read no longer fails.
  - [#18221](https://github.com/emqx/emqx/pull/18221) The default administrator is no longer created with an explicit scope list at startup, and existing default administrator records that carry an explicit list are updated to the implicit form at boot.
- [#18146](https://github.com/emqx/emqx/pull/18146) Hardened scope-based authorization for the dashboard and management API so that access-control checks are applied consistently across equivalent request paths.
- [#18177](https://github.com/emqx/emqx/pull/18177) Fixed an issue where `frame_parse_error` logs could expose packet data in `received_prefix` when the client was not allowed by `allow_log_packet_data_from`.
- [#18204](https://github.com/emqx/emqx/pull/18204) Strengthened validation of data backup archives during import so a backup file's contents are restored only into the table it is meant for.
- [#18225](https://github.com/emqx/emqx/pull/18225) Improved the warning logged when an API key bootstrap file entry contains scopes that are dropped during loading. The warning now groups the dropped scope names by the reason they were dropped: an unknown scope name, a scope not allowed for the publisher role, or a privilege scope that cannot be combined with other scopes. Previously, every dropped scope was reported as an unknown scope name.
- [#18314](https://github.com/emqx/emqx/pull/18314) When reading GCP connectors (GCP PubSub Producer/Consumer, BigQuery) that use JSON Service Account authentication through the HTTP API, service account JSON values are now redacted.
- [#18330](https://github.com/emqx/emqx/pull/18330) Added more secret redaction to read-only REST endpoints, including listener, ExHook, and audit log endpoints.
- [#18344](https://github.com/emqx/emqx/pull/18344) Upgraded HOCON to 0.46.3. This release renders sensitive values inside array-typed config fields as `******` and no longer prints sensitive field values in config validation error logs.
- [#18386](https://github.com/emqx/emqx/pull/18386) Fixed password leak in logs for InfluxDB v1 connectors using query-string authentication (including the Datalayers connector). The password was logged in clear text as part of the client's `path` and `auth_path` fields.
- [#18391](https://github.com/emqx/emqx/pull/18391) Fixed an authentication cache key collision. Two different credentials whose field values produce the same byte sequence when concatenated could share a cache entry, letting one client receive another client's cached authentication result within the cache TTL.

#### Multi-tenancy

- [#17807](https://github.com/emqx/emqx/pull/17807) Namespaced administrators now have an isolated data backup space. Their exports, uploads, listings, downloads, imports and deletes through the data backup endpoints (`/data/export`, `/data/import`, `/data/files`, `/data/files/:filename`) only ever act on their own namespace's backups. A namespaced administrator can no longer see, download, or delete global backups or another namespace's backups.

  Global administrators continue to manage global backups by default (including any created before this change), and may additionally pass a `namespace` query parameter to `GET`/`DELETE /data/files` and `GET /data/files/:filename` to inspect or clean up a specific namespace's backups.

- [#17975](https://github.com/emqx/emqx/pull/17975) The `/tracing` configuration endpoint (`PUT /api/v5/tracing`) is now restricted to the global administrator. Namespaced dashboard administrators and API keys can no longer mutate the global `[trace]` configuration; such requests are rejected with HTTP 403.

- [#18008](https://github.com/emqx/emqx/pull/18008) Data backup: a global administrator can now import or upload a namespaced backup by passing the `namespace` query parameter, consistent with listing and downloading. Previously, importing a namespaced backup directly failed, while uploading it first silently moved it to the global scope and succeeded. The two operations now behave the same. Namespaced administrators remain confined to their own namespace on every operation.

- [#18117](https://github.com/emqx/emqx/pull/18117) Deleting a namespace now also removes the namespace's built-in database authentication users (both password-based and SCRAM) and authorization rules. Previously, these records persisted after namespace deletion and reappeared if a namespace with the same name was created later.

  Additionally, a new `emqx ctl mt purge_ns <namespace>` CLI command deletes a namespace and purges all data belonging to it. The command is idempotent and does not require the namespace to exist, so it can be used as a last resort to clean up leftover data if a previous namespace deletion was interrupted.

- [#18164](https://github.com/emqx/emqx/pull/18164) Improved backup import feedback when working within a namespace. Importing an archive that does not belong to the target namespace, for example one exported from a different namespace or a global backup, now returns a clear error instead of appearing to succeed while importing nothing. A global administrator can still restore a specific namespace's backup using the `namespace` query parameter.

  Global backups are now complete cluster snapshots: a global export also includes every namespace's configuration, and a global import restores each namespace's configuration back into its own namespace. A cluster without namespaces produces and reads exactly the same archives as before.

- [#18222](https://github.com/emqx/emqx/pull/18222) Namespaced administrator API keys now get the same default scope list as namespaced dashboard users. In particular, the default no longer includes the `publish` scope, which was misleading: the publish API is global-only and cannot be used by namespaced API keys. Creating a new namespaced API key with an explicit scope list containing `publish`, or including `publish` when changing an existing namespaced API key's scope list, now returns a validation error. Existing API keys are unaffected: previously stored scope lists (including ones containing `publish`) are kept as-is and continue to work exactly as before.

- [#18227](https://github.com/emqx/emqx/pull/18227) Fixed an issue where clients of a deleted managed namespace could temporarily publish without namespace rate limits while asynchronous client kicking was in progress.

- [#18339](https://github.com/emqx/emqx/pull/18339) Fixed a data backup import isolation issue where an uploaded archive could delete or write backup files that belong to other namespaces. Import now extracts and cleans up within the caller's own namespace directory. Backup archives that contain symlink or hardlink members are now rejected.

- [#18372](https://github.com/emqx/emqx/pull/18372) [#18378](https://github.com/emqx/emqx/pull/18378) Ensured that backup file and managed certificate bundle operations for a namespace always stay within that namespace's own directory. These operations are not available for a namespace whose name cannot be used as a directory name, such as `.`, `..`, an empty name, or a name containing a path separator.

#### Gateway

- [#17796](https://github.com/emqx/emqx/pull/17796) Fixed a crash in the MQTT-SN gateway when a new device connects from a UDP source port that was recently used by a disconnected device (common on loopback and behind NAT, where the OS or NAT box re-assigns the same port). The stale channel is now retired cleanly and the new connection is processed as a fresh session.

- [#17805](https://github.com/emqx/emqx/pull/17805) Fixed an issue where re-loading a gateway could fail with an `already_started` error after a previous load attempt aborted partway through (for example due to an invalid configuration or a busy listener port). The leftover locker process from the failed attempt is now reclaimed automatically, so the next `load` (or operator retry) starts from a clean state.

- [#17815](https://github.com/emqx/emqx/pull/17815) Fixed MQTT-SN UDP session routing when UDP source tuples change or are reused.

  MQTT-SN UDP listeners now route packets by the ClientId parsed from the packet through `esockd_udp_proxy`, allowing asleep sessions to resume from a different UDP source tuple while preventing a reused UDP source tuple from delivering another ClientId's packets to the old session.

- [#17888](https://github.com/emqx/emqx/pull/17888) Fixed an issue where the LwM2M gateway could include sensitive REGISTER query fields such as `password`, `secret`, `private_key`, and `access_token` in registration/update MQTT reports.

- [#18051](https://github.com/emqx/emqx/pull/18051) Fixed CoAP debug logs leaking sensitive URI-query values.

#### Plugins

- [#17861](https://github.com/emqx/emqx/pull/17861) Restored the previous plugin startup behavior by no longer deleting local plugin packages that are missing from the cluster plugin configuration when a node starts or rejoins the cluster.

- [#17884](https://github.com/emqx/emqx/pull/17884) Fixed plugin management HTTP APIs to ignore stale unpacked plugin directories that are not present in the cluster plugin config and are not running locally.

  Such stale packages no longer appear in plugin list/detail/config/schema responses, cannot be acted on by plugin operation APIs, and no longer block reinstalling the same package through the HTTP install API. Configured pre-installed plugins are still visible and continue to follow the documented pre-install workflow.

  EMQX now logs an error on startup and HTTP API access when a plugin package is unpacked but is neither enabled nor disabled in `plugins.states`.

- [#17932](https://github.com/emqx/emqx/pull/17932) Fixed noisy `failed_to_get_plugin_config_from_cluster` warning when installing plugins via CLI.

  The `emqx ctl plugins install` command now installs plugins in `fresh_install` mode (matching the HTTP API behavior), which skips the cluster config lookup for newly installed plugins, avoiding repeated `config_not_found_on_node` warnings on every node in the cluster.

  Added `--cluster` flag to `emqx ctl plugins install` for cluster-wide installation. When specified, the plugin package is distributed to and installed on all running nodes in a single command.

- [#18018](https://github.com/emqx/emqx/pull/18018) Fixed plugin package installation loading code before validating the package's application declarations, configuration schema, and default configuration.

- [#18153](https://github.com/emqx/emqx/pull/18153) [#18172](https://github.com/emqx/emqx/pull/18172) Fixed the plugin configuration API to return a readable validation error when the root JSON value has the wrong type, instead of returning `500 INTERNAL_ERROR`.

- [#18304](https://github.com/emqx/emqx/pull/18304) The UNS Governance plugin now validates message payloads on every publish to a governed topic. Previously, payload validation could be skipped for repeated publishes to the same topic while the authorization cache was warm.

- [#18333](https://github.com/emqx/emqx/pull/18333) Fixed plugin startup after a node restart for plugins that declare `emqx_plugins` in their application dependency list.

  Plugins start while the plugin subsystem itself is starting. A plugin that declared `emqx_plugins` as a dependency made its own startup wait for the plugin subsystem, so the plugin start timed out and the plugin was left enabled but not running after every node restart. EMQX now ignores this dependency declaration and logs a warning that asks the plugin author to remove it.

  When a plugin fails to start with a timeout, the error log now lists the declared dependency applications that were not running at that moment.

- [#18337](https://github.com/emqx/emqx/pull/18337) Started plugins after all EMQX applications have started. A plugin may now declare any EMQX application in its `applications` list. Previously, a plugin that declared an application which starts late in the boot sequence (for example `emqx_management`) failed to start after a node restart.

#### Observability

- [#17886](https://github.com/emqx/emqx/pull/17886) Exposed the publish quota-exceeded packet metric in Prometheus as `emqx_packets_publish_quota_exceeded`.

- [#18114](https://github.com/emqx/emqx/pull/18114) Fixed an issue where the dashboard metrics APIs (`GET /api/v5/monitor_current` and `GET /api/v5/monitor`) returned `500 INTERNAL_ERROR` while a node was joining the cluster.

  While a joining node is restarting its applications, sampling its metrics fails; this failure is now tolerated: the APIs return the aggregate of the remaining reachable nodes and log a warning, instead of failing the whole request.

  Also fixed a spurious `clear_monitor_metrics_rpc_errors` warning that was logged on every successful `DELETE /api/v5/monitor` request.

- [#18183](https://github.com/emqx/emqx/pull/18183) Fixed an issue where the Prometheus metrics collection could fail repeatedly (logging errors on every scrape) when the multi-tenancy feature is not enabled. Namespaced session, authentication, and authorization metrics are now simply omitted when their corresponding features are not active.

- [#18292](https://github.com/emqx/emqx/pull/18292) Fixed an issue where the `/prometheus/namespaced_stats` endpoint reported zero-valued metrics for a namespace that does not exist. When the requested namespace is not known, its metrics are now omitted from the output, consistent with the collection of metrics for all namespaces.

#### File Transfer

- [#18069](https://github.com/emqx/emqx/pull/18069) Fixed the file transfer files API (`GET /api/v5/file_transfer/files`) failing with a 500 error when listing files whose names contain non-ASCII characters.
- [#18315](https://github.com/emqx/emqx/pull/18315) MQTT File Transfer file listing and download REST endpoints are now available only to global (non-namespaced) Dashboard users and API keys. Namespaced users and API keys can no longer read files uploaded by clients outside their namespace.

#### Deployment

- [#17877](https://github.com/emqx/emqx/pull/17877) Fixed the `emqx-enterprise` Helm chart hardcoding `svc.cluster.local` in the node's host name. On a Kubernetes cluster whose DNS domain is not `cluster.local`, a node named itself with an unresolvable FQDN, so Erlang distribution could not start and the nodes failed to form a cluster. The host name now follows the chart's `clusterDomain` value, which already governed the DNS and Kubernetes discovery settings.

#### AI Interoperability

- [#17936](https://github.com/emqx/emqx/pull/17936) Fixed the formatting of A2A cards belonging to the global namespace in the HTTP API. Previously, they would show as the string `"global"`. Now, they are formatted as `null` to distinguish them from specific namespaces.

## 6.2.2

*Release Date: 2026-07-02*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.2.2.

### Enhancements

#### Clustering

- [#17530](https://github.com/emqx/emqx/pull/17530) Cluster linking now requires a non-community license. Under the default community license, configured links stay inactive (no message forwarding or route replication) and the REST API rejects attempts to enable a link with a clear hint to load a non-community license. Disabling and deleting links remain available so that legacy configuration can be tidied up. After upgrading the license, links can be enabled from the Dashboard or REST API without restarting the node.
- [#17549](https://github.com/emqx/emqx/pull/17549) Added the EMQX Backup Sync plugin to periodically synchronize selected configuration from a primary cluster to a secondary cluster using the Data Backup APIs. The plugin supports configurable TLS options for HTTPS calls to the primary cluster.
- [#17620](https://github.com/emqx/emqx/pull/17620) Added an operator-facing diagnostics module `emqx_router_tool` for inspecting and reconciling routing tables. The module is intended to be run via `emqx ctl eval` and provides three helpers:

  - `cluster_schema_view/0` reports the route storage schema each cluster node is running.
  - `scan_missing_routes/0,1` streams the local subscription table and reports topics whose route entry is missing for this node. The scan runs in two passes, is throttled, and tolerates concurrent subscribes and unsubscribes.
  - `reconcile_missing_routes/0,1` re-adds the missing routes via the existing `emqx_router:add_route/2` API.

  The module is schema-agnostic and safe to run on a live cluster.

#### Multi-Tenancy

- [#17665](https://github.com/emqx/emqx/pull/17665) Added per-namespace counters for dropped messages and dropped deliveries in the multi-tenancy app. These are exposed at `/api/v5/prometheus/namespaced_stats` with a `namespace` label, alongside the existing per-namespace metric families. Operators can now diagnose drop rates per tenant from Prometheus without resorting to log inspection.

  Known limitation: QoS 2 PUBREL await-timeout drops do not yet have per-namespace attribution because that drop path bumps the global counter without firing the `message.dropped` hook.

- [#17711](https://github.com/emqx/emqx/pull/17711) Made namespace selection consistent across the built-in database authentication user HTTP APIs, and allowed cleanup of records left over from a deleted namespace.

  Previously only user creation accepted a `namespace` field in the request body; updating and deleting a user accepted the target namespace only through the `ns` query parameter. The update and delete endpoints now also accept a `namespace` field in the request body. When both are provided, the `ns` query parameter takes precedence. Listing users continues to use the `ns` query parameter.

#### Access Control

- [#17564](https://github.com/emqx/emqx/pull/17564) Added fail-closed behavior for authorization backend failures in the hardened security profile. In hardened mode, backend failures and invalid backend responses now deny access; legacy mode preserves the existing ignore and fallback behavior.
- [#17589](https://github.com/emqx/emqx/pull/17589) Added fail-closed handling for access-control hook callback failures in the hardened security profile. Authentication or authorization hook callback crashes now deny access instead of being ignored.
- [#17674](https://github.com/emqx/emqx/pull/17674) Authentication backends now fail closed in hardened security profile when backend failures or malformed backend responses occur. Legacy behavior can be preserved with `authentication_settings.ignore_backend_failures`.
- [#17696](https://github.com/emqx/emqx/pull/17696) Hardened JWT authentication with JWKS by verifying the JWKS endpoint TLS certificate by default in the hardened security profile, rejecting presented JWTs when JWKS keys are unavailable, and denying missing JWT credentials in hardened mode.

#### Data Integration

- [#17481](https://github.com/emqx/emqx/pull/17481) Added a `retain_as_published` option to MQTT bridge ingress (source) subscriptions. When the bridge connects to the remote broker using MQTT 5.0 and `retain_as_published = true`, the original `retain` flag on forwarded messages is preserved instead of being cleared, allowing the bridge to faithfully republish retained messages from upstream. The default is `false` to keep existing behavior. The option has no effect when `proto_ver` is `v3` or `v4`.

  Also, the connector now emits a warning log when `bridge_mode = true` is configured together with `proto_ver = v5`, since the legacy bridge-mode flag has no effect under MQTT 5.0; set `retain_as_published` on individual subscriptions instead.

- [#17508](https://github.com/emqx/emqx/pull/17508) Set the PostgreSQL `application_name` startup parameter to `emqx` for PostgreSQL and TimescaleDB connector connections.

  This makes EMQX database sessions easier to identify in PostgreSQL logs and views such as `pg_stat_activity`.

- [#17576](https://github.com/emqx/emqx/pull/17576) Added TLS cipher suite configuration support for the GreptimeDB connector via the existing `ssl.ciphers` field. When a cipher list is specified, TLS negotiation is restricted to those suites. Unsupported ciphers are rejected at connector startup.

- [#17594](https://github.com/emqx/emqx/pull/17594) Added support for configuring Google Cloud Pub/Sub and BigQuery connector `service_account_json` values with `file://` secret files, so service account credentials can be injected from external files.

#### Observability

- [#17558](https://github.com/emqx/emqx/pull/17558) Added two new metrics and corresponding rates to the `GET /monitor_current` HTTP API: `rules_matched` and `actions_executed`. They track matched rules and action execution rates, including both successful and failed executions.

  This also fixes the `actions.executed` undercount in non-batch mode (`batch_size = 1`): the counter is now incremented once per action callback invocation, independently of the buffer-worker telemetry flush window.

- [#17712](https://github.com/emqx/emqx/pull/17712) Added `emqx_session_tool`, a diagnostic module that operators can call from the remote console. Use `emqx_session_tool:top_by(mqueue_len)` to find the top-K sessions by gauge or counter value in clusters with many connections. Other session metrics, such as `mqueue_dropped` and `inflight_cnt`, are also supported. This helps operators find the busiest sessions without paging through the client list manually.

  The scan streams the channel registry, keeps only a bounded top-K result, and reads cached per-session metrics without sending messages to connection processes. `emqx_session_tool:cluster_top_by/1` aggregates the result across all cluster nodes.

- [#17758](https://github.com/emqx/emqx/pull/17758) The Prometheus `emqx_messages_retained` counter now reports actual retained-message writes. Previously the metric was exposed but never incremented, so it always read 0. Each successful retained-message store now increments the counter.

### Bug Fixes

#### Core MQTT Functionalities

- [#17540](https://github.com/emqx/emqx/pull/17540) Fixed a bug where setting `password = "file://..."` on an SSL listener caused config validation to fail with `bad_password_or_invalid_keyfile` when the keyfile was encrypted. The `file://` reference is now resolved during validation, not only at runtime.

- [#17569](https://github.com/emqx/emqx/pull/17569) Reduced MQTT v5 user-property parsing cost from quadratic to linear.

  Previously a CONNECT, PUBLISH or SUBSCRIBE packet carrying many user-properties caused super-linear scheduler time on the owning connection process, because each parsed property was appended to the end of the accumulated list. Parsing now scales linearly with the number of entries while preserving their wire order.

- [#17731](https://github.com/emqx/emqx/pull/17731) Fixed a transient "address already in use" error that could occur when updating the options of a WS or WSS listener (for example when rotating TLS certificates). Updating such a listener rebinds its port, and the operating system may not have released the old socket yet; EMQX now retries the rebind briefly instead of failing the update.

- [#17798](https://github.com/emqx/emqx/pull/17798) Fixed an issue where retained messages could be delivered with the original publish QoS instead of the wildcard subscription QoS limit.

- [#17801](https://github.com/emqx/emqx/pull/17801) The `ssl_opts.ciphers` validator now accepts cipher names in either OpenSSL or IANA/RFC naming convention. Previously, only OpenSSL-format names were recognized, so a valid TLS 1.2 cipher supplied in its IANA name (for example, `TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384`) was rejected as `bad_ciphers` even though Erlang's `ssl` module would have accepted it. TLS 1.3 ciphers were unaffected because their IANA and OpenSSL names are identical.

#### Queue and Stream

- [#17515](https://github.com/emqx/emqx/pull/17515) Fixed an issue where Message Queue subscriptions using QoS 0 could stop receiving messages after the queue subscriber's local inflight window became full.
- [#17529](https://github.com/emqx/emqx/pull/17529) Fixed an issue where QoS 0 messages delivered through Message Queue subscriptions could remain unacknowledged internally, causing the queue subscriber to stop receiving more messages after reaching its local inflight limit.
- [#17733](https://github.com/emqx/emqx/pull/17733) Fixed an issue where Message Queue consumers could fail to restore an empty stream buffer after durable storage subscription recovery.

#### Rule Engine

- [#17725](https://github.com/emqx/emqx/pull/17725) Fixed a bug introduced in 6.0.3, 6.1.2 and 6.2.1 where a global rule could stop matching messages on its `FROM` topic when publishing clients carried a tenant namespace (`client_attrs.tns`).

  With `rule_engine.limit_selects_in_namespace` enabled (the default), global rules now retain system-wide visibility and match messages from any namespace. Rules created inside a namespace remain isolated to their own namespace. Operators who prefer to disable namespace restriction entirely can still set `rule_engine.limit_selects_in_namespace = false`.

#### Data Integration

- [#17568](https://github.com/emqx/emqx/pull/17568) Upgraded the Kafka client library `brod` to 4.5.5.

  Consumer group: respect the broker-assigned member ID when the join response carries the `member_id_required` error code (returned by older Kafka brokers, e.g. 2.2.0, that do not support static member instance IDs). Previously the member ID was discarded on error, preventing the retry from succeeding.

- [#17579](https://github.com/emqx/emqx/pull/17579) Fixed Redis Sentinel connectors to use isolated Sentinel managers per resource and clean them up when resources stop, avoiding shared Sentinel state across connectors.

- [#17584](https://github.com/emqx/emqx/pull/17584) Limited the amount of data returned during connector health checks of Snowflake aggregated connectors. This only has observable effects if the list of existing schemas was very large, in which case the health check will take far less time to execute.

- [#17588](https://github.com/emqx/emqx/pull/17588) Limited the amount of data returned during connector and action health checks of Kinesis integrations. This only has observable effects if the list of existing schemas was very large, in which case the health check will take far less time to execute.

- [#17595](https://github.com/emqx/emqx/pull/17595) Limited the amount of data returned during connector health checks of S3 and S3 Tables integrations. This only has observable effects if the list of existing buckets was very large, in which case the health check will take far less time to execute.

- [#17598](https://github.com/emqx/emqx/pull/17598) Fixed a connection failure to MongoDB 8.0+ when authentication is required. The driver previously queried `buildInfo` before authentication to pick the auth mechanism; MongoDB 8.0 restricted that command to authenticated callers. The driver now skips the probe and uses SCRAM-SHA-1 directly, which all supported MongoDB versions accept.

- [#17605](https://github.com/emqx/emqx/pull/17605) Fixed Oracle action prepare/status checks to parse action SQL without executing it, and reject unsupported top-level DDL/DCL/TCL statements. Also improved support for text payloads over 4000 bytes when the payload placeholder is the last bind parameter.

- [#17625](https://github.com/emqx/emqx/pull/17625) Fixed an issue with GCP PubSub Consumer source where, if a source was initially created with a service account lacking necessary permissions to create subscriptions for the configured topic, the source would fail to become `connected` even after granting the permissions to the service account.

- [#17633](https://github.com/emqx/emqx/pull/17633) Fixed an issue introduced in 6.1.2 and 6.2.1 where MQTT bridges and Cluster Link connections over TLS could stall after a short period of traffic. Affected nodes log a recurring error message like `unexpected_event ... ssl_passive ...` from the `emqtt` client. EMQX now bundles `emqtt` 1.15.3, which restores normal traffic flow after the bug was first reported in [#17617](https://github.com/emqx/emqx/issues/17617).

- [#17649](https://github.com/emqx/emqx/pull/17649) Improved the responsiveness of starting and stopping GCP PubSub Consumer connectors. Previously, if the connections were slow or busy, timeouts could leave the connectors running in a state inconsistent with the configuration.

- [#17681](https://github.com/emqx/emqx/pull/17681) Fixed PostgreSQL connector batch writes when prepared statements are disabled.

  Previously, concurrent batches on the same connection could interleave raw SQL parsing and fail with PostgreSQL protocol errors. Table-existence checks are also serialized through the connector worker to avoid interleaving with batch execution.

- [#17701](https://github.com/emqx/emqx/pull/17701) Fixed a confusing `badarith` error from PostgreSQL actions when a batched SQL template returns rows, for example `SELECT ...`.

  PostgreSQL action batching does not support row-returning SQL. EMQX now returns a clear unsupported SQL error instead of crashing the batch result handler.

- [#17717](https://github.com/emqx/emqx/pull/17717) Added an option to enable TLS peer verification for Confluent Producer connectors.

- [#17718](https://github.com/emqx/emqx/pull/17718) Added an option to enable TLS peer verification for GCP PubSub Producer/Consumer and BigQuery connectors.

#### Clustering

- [#17586](https://github.com/emqx/emqx/pull/17586) Periodically purge stale entries from the global session registry.

  Previously, if a session's owner process died without a clean unregister, the registry row could remain forever when the same client ID never reconnected. This could happen, for example, after a brief network split that prevented the unregister from replicating, or when one core node's consensus check timed out during down-event cleanup.

  A new throttled background sweep on each core node now removes such rows. The sweep is bounded to at most 500 registry rows per second per node and runs no more often than once every 10 minutes, so it does not measurably affect broker throughput even on registries holding millions of sessions.

- [#17773](https://github.com/emqx/emqx/pull/17773) Fixed configuration update commands (REST API and CLI) crashing with a `function_clause` crash report when the underlying cluster RPC layer aborted unexpectedly. For example, this could happen with `{no_exists, cluster_rpc_mfa}` when the cluster RPC tables were not yet available during node startup or recovery. Such failures are now returned to the caller as a structured error instead.

- [#17764](https://github.com/emqx/emqx/pull/17764) Fixed an issue where stale plugin entries could remain on a node after it rejoined the cluster if the plugin had been uninstalled while the node was offline. During plugin startup, EMQX now removes local plugin packages that are no longer present in the cluster plugin configuration.

#### Access Control

- [#17575](https://github.com/emqx/emqx/pull/17575) Fixed a race condition in the `emqx_username_quota` plugin that could cause the per-username session counter to become inconsistent with the actual number of tracked client records. The counter could be decremented past zero and then be deleted while a concurrent session registration incremented it, losing the increment permanently.

- [#17644](https://github.com/emqx/emqx/pull/17644) Fixed an issue where the `plain` password hash algorithm accepted passwords that differed only by letter case during authentication.

- [#17646](https://github.com/emqx/emqx/pull/17646) Fixed an HTTP/1.1 protocol-conformance issue in the JWKS retrieval client used by JWT authentication. Earlier versions sent an empty `TE:` header value due to a long-standing default in Erlang/OTP's `inets` HTTP client (fixed upstream in inets 9.4.2 / OTP 28.1). Some identity providers (notably PingFederate) reject such requests. EMQX now sends an explicit, valid `TE: trailers` header on JWKS fetches.

- [#17653](https://github.com/emqx/emqx/pull/17653) Fixed a security issue where the Prometheus configuration API returned stored `Authorization` header values in push gateway headers. The API now redacts these values in responses.

- [#17654](https://github.com/emqx/emqx/pull/17654) Fixed an issue where creating an authenticator via `POST /authentication` returned the new authenticator config without redacting provider secrets (such as JWT HMAC secrets, HTTP `Authorization` headers, and request body passwords). The creation response now applies the same redaction as the list and get endpoints.

- [#17657](https://github.com/emqx/emqx/pull/17657) Fixed a security issue where raw `authorization` and `cookie` headers were forwarded to plugin API callbacks. These credential-bearing headers are now redacted before reaching plugin code.

- [#17711](https://github.com/emqx/emqx/pull/17711) Creating or updating a built-in database user now fails with "Managed namespace not found" if the target namespace is not a known managed namespace. Previously, a user could be created with a nonexistent namespace when the namespace was supplied in the request body.

  In addition, global administrators can now delete built-in database users that belong to namespaces that have already been deleted, instead of receiving a "Managed namespace not found" error.

- [#17736](https://github.com/emqx/emqx/pull/17736) Restricted the JWT authenticator to verify tokens using only JWS algorithms consistent with the configured key type. HMAC-based authenticators now accept only `HS256`, `HS384`, and `HS512`. Public-key and JWKS authenticators accept `RS*`, `PS*`, `ES*`, and `EdDSA` algorithms. Tokens whose `alg` header does not match the configured key type, including `alg=none`, are rejected.

- [#17739](https://github.com/emqx/emqx/pull/17739) Improved redaction of sensitive data in logs, traces, and audit records.

- [#17787](https://github.com/emqx/emqx/pull/17787) Prevented HTTP connector error logs from including request headers when an `ehttpc` worker is terminated before a request returns.

  Previously, if the HTTP connector's `ehttpc` worker was terminated while a request was in flight (for example, by deleting the source before the request returned), the resulting EXIT reason carried the original `gen_server:call` arguments. Because those arguments include the request headers, the headers were written verbatim to the error log. EMQX now removes the call arguments from the reason before it is logged.

- [#17790](https://github.com/emqx/emqx/pull/17790) Stopped writing the TOTP shared secret to the `dashboard_login_failed` server log. The secret was previously included in this log entry during first-time MFA setup.

- [#17791](https://github.com/emqx/emqx/pull/17791) Improved log redaction so that JWT HMAC key bytes no longer appear in `cluster_rpc_apply_result` and `cluster_rpc_apply_ok` debug log lines emitted during configuration updates.

  The redactor now recognizes the internal JWK record shape and replaces it with a placeholder before logging, and also treats the `jwk` field as sensitive.

#### Multi-Tenancy

- [#17715](https://github.com/emqx/emqx/pull/17715) Fixed a multi-tenancy gating gap. When `multi_tenancy.post_auth_tns_expression` was configured and evaluated to an empty string or an error, the namespace gate (`allow_only_managed_namespaces` enforcement, session quota, etc.) was previously skipped, allowing the client through.

  Empty-string and error outcomes are now treated as "no namespace assigned" and pass through the same gate as clients that supplied no namespace before authentication. The client is rejected when `allow_only_managed_namespaces = true`, and accepted without a namespace when it is `false`. In this case, any namespace value carried in `client_attrs.tns` from before authentication is also cleared, so it is not retained when the expression declines to assign one.

- [#17757](https://github.com/emqx/emqx/pull/17757) Fixed `/prometheus/namespaced_stats` so that namespaced admins/API keys can only see data from their own namespace. Global admins/API keys can still see data from all namespaces.

#### Gateway

- [#17528](https://github.com/emqx/emqx/pull/17528) Fixed missing authorization checks in several gateway publishing and subscription flows. Authorization is now checked before the following operations: MQTT-SN Will message publishing; JT/T 808 upstream publishing and automatic downlink subscription; GBT32960 upstream publishing and automatic downlink subscription; and OCPP upstream publishing and automatic downlink subscription.

- [#17556](https://github.com/emqx/emqx/pull/17556) Fixed an issue where the OCPP gateway did not pass the listener `enable_authn` option to the shared authentication flow. This happened because the option was stored under a misspelled client-info key.

- [#17581](https://github.com/emqx/emqx/pull/17581) Fixed the JT/T 808 gateway to use the phone number accepted during authentication as the connection identity, rejecting mismatched registration-code authentication attempts and subsequent uplink frames with a different phone number.

- [#17604](https://github.com/emqx/emqx/pull/17604) Fixed GBT32960 gateway routing: vehicle responses to downstream commands (Parameter Query, Parameter Setting, Terminal Control) are now correctly published to `upstream/response` instead of `upstream/transparent`.

#### Observability

- [#17497](https://github.com/emqx/emqx/pull/17497) Fixed the `actions.executed` metric undercounting `actions.messages` for actions configured in non-batch mode (`batch_size = 1`).

  The previous implementation incremented `actions.executed` once per buffer-worker telemetry flush, which could aggregate many individual completions into one event, so `actions.executed` fell behind `actions.messages` even when no batching was configured.

  The two metrics are now incremented at independent call sites: `actions.executed` once per action callback invocation (one per batch in batch mode, one per message in single mode), `actions.messages` per message handled.

- [#17513](https://github.com/emqx/emqx/pull/17513) Fixed Prometheus matched authorization allow/deny metrics so they reflect real matched authorization decisions.

- [#17536](https://github.com/emqx/emqx/pull/17536) Documented the `file://` option in Dashboard tooltips for the SSL listener `password` and other secret-typed configuration fields (MQTT bridge password, cluster link password, Dashboard OIDC client secret, S3 secret access key, AI completion API key, Pulsar/RocketMQ credentials, etc.). The generic secret type description already mentioned this convention, but field-specific descriptions shadowed it in the Dashboard, causing users to assume the field accepted only literal values.

- [#17708](https://github.com/emqx/emqx/pull/17708) Fixed a logger JSON formatter crash that could replace some debug-level trace events with a `FORMATTER CRASH` line.

## 6.2.1

*Release Date: 2026-06-11*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.2.1.

### Enhancements

#### Security Hardening

- [#17040](https://github.com/emqx/emqx/pull/17040) Restricted API key access to Dashboard user-account management endpoints.

  Previously, an API key with the `administrator` role could call the Dashboard user management endpoints `POST/DELETE /users/:username/mfa` and `POST /users/:username/change_pwd` via HTTP Basic authentication. This meant an API key could reset or disable another Dashboard user's MFA, or change another Dashboard user's password, bypassing the intended separation between human Dashboard sessions and machine API keys.

  These endpoints now return `401 API_KEY_NOT_ALLOW` when accessed via an API key, consistent with the existing policy that blocks API key access to `/users`, `/users/:username`, `/logout`, and `/api_key`. Dashboard users can still manage their own MFA and password from the Dashboard UI using bearer-token (JWT) sessions.

- [#17065](https://github.com/emqx/emqx/pull/17065) Added SSRF protection for rule-engine-reachable connector and bridge configurations.

  When `rule_engine.ssrf.enable` is set to `true`, EMQX applies an outbound SSRF policy to connector, bridge, and action configurations. The policy evaluates each target as follows: exact matches in `rule_engine.ssrf.deny_hosts` are rejected immediately; resolved target IPs are then checked against `rule_engine.ssrf.allow_cidrs` before `rule_engine.ssrf.deny_cidrs`. The default denied ranges cover loopback, link-local (including cloud instance-metadata endpoints), RFC 1918, ULA, unspecified, and multicast ranges. The check runs at config-update time and covers HTTP `url` fields as well as `server`, `servers`, and `bootstrap_hosts` fields across all connector families.

  The feature is disabled by default to preserve compatibility with deployments whose connectors legitimately point at internal services. Operators in multi-tenant or externally-exposed setups are encouraged to enable it together with a network-layer egress firewall.

- [#17173](https://github.com/emqx/emqx/pull/17173) Restricted API keys from exporting or importing Dashboard accounts and API keys via the data backup endpoints.

  `POST /data/export` called with an API key now silently omits the `dashboard_users` and `api_keys` mnesia table sets from the resulting archive. `POST /data/import` called with an API key now returns `403 FORBIDDEN` when the uploaded backup contains either of those table sets.

  Dashboard bearer-token (login) callers are unaffected and continue to be able to back up and restore the full database, including Dashboard users and API keys.

  This closes a privilege-escalation gap. The existing `/users` and `/api_key` endpoints already deny API keys access to Dashboard login credentials and API key records, but an API key holder could bypass those restrictions by going through the data backup endpoints instead.

- [#17187](https://github.com/emqx/emqx/pull/17187) Removed the EMQX release version (`rel_vsn`) from the unauthenticated `GET /status?format=json` response to avoid disclosing the broker version to unauthenticated callers. The version remains available via the authenticated node-info APIs.

- [#17201](https://github.com/emqx/emqx/pull/17201) Hardened the plugin install endpoint against path traversal in uploaded tarballs and tightened the install allowlist.

  - The install path now refuses to extract any tarball whose entries would resolve outside the plugin install directory.
  - `emqx ctl plugins allow <name-vsn>` entries now expire 5 minutes after they are issued, and can be pinned to a SHA-256 hash of the package via `emqx ctl plugins allow <name-vsn> sha256:<HEX>`. Uploads whose contents do not match the pinned hash are rejected with `403 Forbidden`. When the optional `sha256:` argument is omitted, the previous behavior of accepting any payload named `<name-vsn>.tar.gz` is preserved.
  - A successful install via the HTTP plugin install endpoint (and the Dashboard upload that wraps it) immediately revokes the allow entry cluster-wide, preventing the same grant from being reused for a different tarball.

- [#17252](https://github.com/emqx/emqx/pull/17252) Published `.sha256` checksum sidecars alongside plugin packages on the official download site, allowing users to verify the integrity of downloaded plugin archives.

- [#17271](https://github.com/emqx/emqx/pull/17271) Hardened the official EMQX docker image to clear image-scanner findings:

  - Applied Debian security upgrades during the runtime image build, so the image picks up the latest patched `libssl3t64`.
  - Removed the unused `libgnutls30t64` package. EMQX talks TLS via OpenSSL through Erlang/OTP and never links GnuTLS, so it was only present as a transitive dependency of `curl` and showed up in scanner reports.
  - Replaced the Debian `curl` package with a statically-linked `curl` binary from [stunnel/static-curl](https://github.com/stunnel/static-curl) (OpenSSL, HTTP/2, HTTP/3; no RTMP, no GnuTLS). The Debian package would have transitively re-introduced `libgnutls30t64` via `librtmp1`; the static binary avoids this while keeping container health checks that call `curl` working unchanged.

- [#17309](https://github.com/emqx/emqx/pull/17309) Sanitized PROXY-Protocol v2 SSL Common Name and Subject fields to prevent control characters from being smuggled into client identity.

  When a listener is configured with `proxy_protocol = true`, the broker now rejects connections whose PROXY-Protocol SSL TLV bytes contain ASCII control characters (the same byte class already rejected for MQTT-ingested `clientid`, `username`, and `password`). This blocks attacker-controlled bytes from reaching outbound HTTP authentication, authorization, or rule-engine header values via `${cert_common_name}` and `${cert_subject}` templates.

  The HTTP authentication and authorization clients also now refuse to send a request when a rendered header name or value contains a CR, LF, or NUL byte.

- [#17315](https://github.com/emqx/emqx/pull/17315) Extended the byte-class check applied to MQTT clientid / username / password to other fields that feed `ClientInfo` and HTTP request templating:

  - `peersni` (TLS Server Name Indication; also accepted from the PROXY-Protocol v2 `authority` TLV) is now validated at the connection ingestion boundary. Control characters cause the connection to be rejected and a warning logged.
  - Client attribute values produced by `mqtt.client_attrs_init` Variform expressions are dropped (with a warning) when they contain control characters, so templates such as `${client_attrs.tns}` cannot carry injected bytes downstream.
  - HTTP action / bridge connector header rendering now drops any header whose rendered name or value contains NUL, CR, or LF.

- [#17440](https://github.com/emqx/emqx/pull/17440) Restricted `GET /api/v5/data/files/<filename>` (backup file download) to the global Dashboard administrator. Backup archives can contain Dashboard accounts (including password hashes and MFA/TOTP state) and API key records, so API key callers, Dashboard viewers, and namespaced administrators are no longer permitted to download them. Listing the backup directory (`GET /api/v5/data/files`) remains available to all roles that previously had access.

- [#17491](https://github.com/emqx/emqx/pull/17491) Fixed passwords and secrets being exposed in gateway authentication APIs, error paths, and debug logs. Gateway authentication API responses now redact secrets while preserving the raw configuration structure. The following log paths no longer print raw passwords or secrets: gateway authentication failures, listener start errors, ExProto authentication logs, CoAP token-required logs, and LwM2M invalid-register logs.

- [#17501](https://github.com/emqx/emqx/pull/17501) Blocked namespaced Dashboard users from reading MQTT message content across namespace boundaries.

  - The following endpoints now return `403 FORBIDDEN` for any non-global caller, because they can expose MQTT payloads outside the caller's namespace. Previously, a namespaced user could read or delete messages produced by other namespaces.

    - `GET /clients/:clientid/mqueue_messages`
    - `GET /clients/:clientid/inflight_messages`
    - `GET|DELETE /mqtt/retainer/messages`
    - `GET|DELETE /mqtt/retainer/message/:topic`
    - `GET /mqtt/delayed/messages`
    - `GET|DELETE /mqtt/delayed/messages/:node/:msgid`
    - `DELETE /mqtt/delayed/messages/:topic`

  - Trace APIs are now namespace-scoped: `GET /trace` lists only traces created by the caller's namespace. The per-trace endpoints (`/trace/:name`, `/trace/:name/download`, `/trace/:name/log`, `/trace/:name/log_detail`, `/trace/:name/stop`) return `404` when the trace belongs to a different namespace, preventing callers from discovering that other-namespace traces exist. The bulk `DELETE /trace` is reserved for the global administrator; namespaced callers receive `403`. Namespaced administrators retain full access to their own traces, including creating, listing, downloading, streaming, stopping, and deleting them.

#### Clustering

- [#17076](https://github.com/emqx/emqx/pull/17076) Introduced a new routing table synchronization mechanism. The routing table schema version has been stepped to `v3`, with backward compatibility for `v2` provided.

  With schema v3, each node (core or replicant) takes full ownership of the routing table entries pointing towards it, giving peer nodes only read-only access to these entries. This improves partition tolerance of the EMQX cluster, as peer nodes in a partitioned cluster cannot change the routing table on behalf of other nodes. It also improves `SUBACK` latency on replicant nodes.

  **Backward compatibility:** When a node supporting v3 joins a cluster of nodes that only support v2, it keeps using v2 for compatibility. To switch the cluster to v3, perform a full cluster restart after upgrade. To prevent the automatic switch, set `broker.routing.storage_schema` to `v2`.

  **Downgrade note:** After the cluster switches to v3, rolling downgrade is not possible.

  To check the current routing schema version on a node:

  ```
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17152](https://github.com/emqx/emqx/pull/17152) Added support for configuring Erlang inet port options for the distribution port, with a default `buffer` size of 1 MB.

  Previously, the Erlang distribution port used an extremely small default port buffer (1460 bytes, or ~9 KB on some platforms), which caused performance bottlenecks even when the distribution port buffer (`+zdbbl`) was configured to a much larger value (e.g., 32 MB). This affected cluster communication reliability and could manifest as `erpc timeout` errors, Mnesia transaction congestions, and degraded multi-core node support.

#### Observability

- [#17018](https://github.com/emqx/emqx/pull/17018) Reduced the number of calls to other nodes performed when calling the Prometheus scraping API endpoint. This makes the API call return faster and reduces the chance of it timing out when the cluster is under strain.

  Specifically, `emqx_mria_lag` metric that is of interest to replicant nodes is now refreshed periodically (every 10 seconds by default) instead of refreshed on demand for each API call.

- [#17162](https://github.com/emqx/emqx/pull/17162) Exposed per-node license info via Prometheus gauges (`emqx_license_max_sessions`, `emqx_license_expiry_at`, `emqx_license_issued_at`) so cluster-wide license consistency can be alerted on without per-node CLI checks.

  Operators can now alert on license inconsistencies across cluster nodes by comparing these gauges. The implementation fetches all three values from a single `emqx_license_checker:dump/0` gen_server call, eliminating a redundant round-trip on every Prometheus scrape.

- [#17176](https://github.com/emqx/emqx/pull/17176) Added `emqx_routes_count` and `emqx_routes_max` Prometheus metrics to export the number of route table entries per node.

- [#17329](https://github.com/emqx/emqx/pull/17329) Added two node-wide gauge metrics to the `/api/v5/prometheus/stats` endpoint:

  - `emqx_vm_uptime_ms` reports the EMQX node uptime in milliseconds.
  - `emqx_vm_max_fds` reports the maximum number of file descriptors available to the node.

- [#17031](https://github.com/emqx/emqx/pull/17031) Added session high-watermark history for license usage auditing.

  EMQX now records the daily peak session count and retains at least 24 months of history. Operators can query this data via `emqx ctl license history` with optional `--period daily|monthly` and `--json` flags. A new `license.high_watermark_timezone` config controls the day boundary for bucketing.

#### Access Control

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) Introduced fine-grained scope-based access control for both API keys and Dashboard login users.

  API keys can now be restricted to specific API path categories using scopes derived from OpenAPI tags. Keys without scopes retain full access (backward compatible). An empty scopes list denies all scoped API paths. The `publisher` API-key role is now constrained to `[publish]` only.

  Dashboard login users now also carry an optional `scopes` field; when set, requests are authorized against the same path-to-scope catalog used for API keys, layered on top of the existing role-based check. Four new scopes (`user_management`, `mfa_management`, `sso_management`, `api_key_management`) cover Dashboard-only endpoints and are admin-only except `mfa_management`, which any role may hold for self-exemption from forced MFA. API keys cannot hold any of the four login-only scopes. Both checks apply to the HTTP API and to bootstrap-file loading (incompatible scopes are dropped with a warning).

  New public catalog endpoints expose the scope vocabulary for UI consumption: `GET /api_key_scopes` and `GET /user_scopes`, both accessible to any bearer-authenticated caller. The `scopes` field is also surfaced in `GET /users`, `POST /users`, and `PUT /users/:username` responses; when not explicitly set, the response projects the role-default scope list.

  Additional behavior changes that follow from the new scope model:

  - The `dashboard.default_username` user is protected as a break-glass account. It cannot be deleted, demoted from administrator, or have its `scopes` field set; only its `description` may be changed. This guarantees an operator always retains administrative access if other administrators lose or misconfigure their scopes.
  - Self-service on a user's own record now respects scopes. Only the dedicated change-password and MFA self endpoints still bypass scope checks; other operations such as `PUT /users/:self` are subject to the user's scopes.
  - `PUT /users/:username` and `PUT /api_key/:name` validate role changes against the effective persisted scopes when the request body omits the `scopes` field. Demoting a user or changing an API key role is rejected if the persisted scopes are incompatible with the new role.
  - API key bootstrap files accept an optional fourth column for scopes (`key:secret:role:scopes`). Unknown or role-incompatible scope names are dropped with a warning rather than rejecting the whole file, so existing three-column bootstrap files remain loadable.
  - The SAML SP metadata endpoint (`GET /sso/saml/metadata`) is now reachable without authentication, matching `/sso/saml/acs`.

- [#16943](https://github.com/emqx/emqx/pull/16943) Added per-backend `force_mfa` option for SSO (OIDC/SAML/LDAP).

  When enabled, SSO users must complete TOTP MFA setup or verification before receiving a Dashboard token, regardless of IDP-side MFA settings. Supports three MFA states: `not_configured` (force setup), `enabled` (require verification), and `admin_disabled` (skip MFA). New API endpoints `POST /sso/mfa/setup` and `POST /sso/mfa/verify` handle the MFA flow.

  Existing users can be exempted or required individually by an administrator via DELETE/POST on `/users/:username/mfa`, and that decision overrides the live backend policy until the administrator changes it. SSO users on a `force_mfa = true` backend who disable their own MFA are required to set MFA up again on the next login; only an administrator-initiated disable exempts a user from the live policy.

- [#17178](https://github.com/emqx/emqx/pull/17178) The `emqx ctl api_keys add` CLI command now accepts a `--scopes <scope1,scope2,...>` option, matching the scope-based permission control already supported by the REST API.

- [#17218](https://github.com/emqx/emqx/pull/17218) Added an ACME client plugin (`emqx_acme`) that issues and renews TLS certificates from any RFC 8555 ACME CA (e.g. Let's Encrypt) into an EMQX managed certificate bundle, and rewrites the configured SSL/WSS and/or Dashboard HTTPS listeners to consume that bundle.

#### Multi-tenancy

- [#17053](https://github.com/emqx/emqx/pull/17053) Added a new multi-tenancy configuration option `multi_tenancy.post_auth_tns_expression`.

  When configured, it is a [Variform](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#variform-expressions) expression evaluated after the authentication chain completes. Its rendered value is written into `client_attrs.tns`, the tenant namespace key used by multi-tenancy quota and routing decisions.

  This lets operators derive the tenant namespace from authentication-response attributes (for example, a `tag` field returned by an HTTP auth backend) instead of relying only on pre-authentication `mqtt.client_attrs_init`. Example expressions: `client_attrs.tag`, or with a fallback `coalesce(client_attrs.tag, username)`.

  When the expression is empty (default), behavior is unchanged.

- [#17078](https://github.com/emqx/emqx/pull/17078) Inlined each managed namespace's configuration (session and limiter) in the response of `GET /api/v5/mt/managed_ns_list_details`, so management UIs can render a list of namespaces with their configuration in a single request instead of one additional call per namespace.

#### Gateway

- [#17013](https://github.com/emqx/emqx/pull/17013) Added GBT32960-2025 protocol support to the GBT32960 gateway.

  The gateway now automatically detects the protocol version by frame header (`##` for 2016, `$$` for 2025) and handles version-specific parsing and serialization, including:

  - New 2025 info types: Vehicle, DriveMotor, FuelCell, Engine, Location, Alarm, PowerBatteryVoltage/Temp, FuelCellStack, SuperCapacitor, SuperCapacitorExtreme, and digital Signature.
  - New command: Activation (0x09/0x0A).
  - Version-aware parameter sizes for parameter query/setting (0x02/0x03: BYTE in 2025 vs WORD in 2016).
  - 2025 vehicle login with BMS battery pack encoding fields.

#### Data Integration

- [#17011](https://github.com/emqx/emqx/pull/17011) Added `ts_column` and `ttl` configuration fields to the EMQX Tables (Rust NIF driver) connector.

  - `ts_column`: Specifies a custom timestamp column name for auto-created tables (defaults to `ts` if not set).
  - `ttl`: Sets the time-to-live hint for auto-created tables (e.g., `3 days`).

  These fields were already supported by the underlying `greptimedb-ingester-erlnif` driver (since 0.1.8) and are now exposed in the EMQX Tables connector configuration.

- [#17025](https://github.com/emqx/emqx/pull/17025) The way the InfluxDB database performs health checks and credential verification has been changed.

  It no longer performs checks by executing `SHOW DATABASES`, which could be falsely flagged as a system penetration by some auditing systems.

  See also [emqx/influxdb-client-erl#54](https://github.com/emqx/influxdb-client-erl/pull/54).

- [#17027](https://github.com/emqx/emqx/pull/17027) The A2A Registry HTTP APIs are now namespace-aware. Agent cards listed, deleted and upserted are restricted to the namespace of the API user.

- [#17046](https://github.com/emqx/emqx/pull/17046) Added a new metric `actions.messages` (and the corresponding `actions_messages_rate` in the Dashboard monitor API) that counts the total number of messages handled by rule-engine action executions.

  Because a single action execution may handle a batch of messages, `actions.messages` is greater than or equal to `actions.executed`, and `actions_messages_rate` reflects the true per-message throughput of actions.

- [#17089](https://github.com/emqx/emqx/pull/17089) MQTT ingress bridges now support consuming from remote message queues exposed as `$queue/{name}/{bind-filter}` when the remote broker supports MQTT 5 Subscription Identifiers. Queue subscriptions are rejected when Subscription Identifiers are unavailable, and regular topic subscriptions automatically retry without Subscription Identifiers if the remote broker does not accept them.

- [#17104](https://github.com/emqx/emqx/pull/17104) Added date-part placeholders to blob name templates in aggregated upload actions (Azure Blob Storage, Amazon S3, GCS, Snowflake, S3 Tables). Placeholders are rendered against the aggregation start time and default to UTC. This enables Hive-partitioned object layouts (e.g. `year=2025/month=04/day=22/hour=07/...`) directly consumable by Spark, Databricks, and Synapse.

  Supported placeholders:

  - `${datetime.YYYY}`
  - `${datetime.MM}`
  - `${datetime.DD}`
  - `${datetime.hh}`
  - `${datetime.mm}`
  - `${datetime.ss}`
  - `${datetime.DOY}` (day of year)

  Each placeholder can be prefixed with an explicit timezone:

  - `utc` (default): e.g. `${datetime.utc.YYYY}`
  - `local` (EMQX node's system timezone): e.g. `${datetime.local.YYYY}`

- [#17120](https://github.com/emqx/emqx/pull/17120) Added a new query string filter option to `GET /clients_v2`: `node`. When specified, online clients connected to the supplied node name will be returned, as well as disconnected clients last connected to them.

- [#17136](https://github.com/emqx/emqx/pull/17136) Added the `ping_with_auth` option for InfluxDB connectors. When enabled, health checks include the configured credentials for InfluxDB-compatible services that require authenticated health check requests. Also fixed the InfluxDB connector/action to preserve Unicode text when writing values from `write_syntax` literals or MQTT payloads.

- [#17165](https://github.com/emqx/emqx/pull/17165) Added the `resource_opts.dispatch_strategy` option for actions.

  The new option defaults to `per_clientid`, preserving the previous buffer worker dispatch behavior. Setting it to `random` makes queries without an explicit `pick_key` use a random dispatch key, which helps spread traffic across multiple buffer workers when a small number of clients publish a large amount of messages.

- [#17170](https://github.com/emqx/emqx/pull/17170) [#17282](https://github.com/emqx/emqx/pull/17282) [#17297](https://github.com/emqx/emqx/pull/17297) Added `tcp_opts` (`nodelay`, `sndbuf`, `recbuf`, `buffer`, `keepalive`, `delay_send`, `active_n`) to the MQTT bridge connector and Cluster Link configurations, so the outbound MQTT client TCP socket can be tuned per connection. Unset fields keep the operating system / `gen_tcp` defaults. `delay_send` (off by default) coalesces small writes for better throughput at the cost of a small latency increase.

#### Cluster Linking

- [#17221](https://github.com/emqx/emqx/pull/17221) Improved Cluster Linking diagnostics for MQTT message forwarding.

  When message forwarding connections experience connectivity issues, the link resource status and respective alarms now include the disconnect reason, making configuration problems easier to identify.

#### Deployment

- [#17079](https://github.com/emqx/emqx/pull/17079) Added `service.wsEnabled` option to the Helm chart to suppress the ws/wss Service port entries when MQTT WebSocket listeners are disabled. Defaults to `true` to preserve existing behavior.

### Bug Fixes

#### Core MQTT Functionalities

- [#17139](https://github.com/emqx/emqx/pull/17139) Restored `retainer.enable` as a real runtime switch for the retainer subsystem.

  This allows deployments to keep MQTT retained-message protocol support enabled while disabling retained-message storage, instead of relying on `mqtt.retain_available`, which can reject retained publishes at the protocol layer.

- [#17172](https://github.com/emqx/emqx/pull/17172) Fixed an issue where MQTT packets (such as PUBACK) sent by a client right before disconnecting could be lost when the connection process had pending outbound messages in its mailbox. Now the connection process correctly drains its mailbox before shutting down, ensuring that inbound packets are processed even after the socket is closed.

- [#17353](https://github.com/emqx/emqx/pull/17353) Fixed an issue in the `socket` TCP backend where outbound MQTT packets could be sent in the wrong order when a client connection experienced repeated send congestion. This scenario was practically very unlikely to occur.

- [#17383](https://github.com/emqx/emqx/pull/17383) After a session takeover, the channel info reflected by the Dashboard and REST API (`mqueue_len`, `inflight_cnt`) now updates immediately after the takeover replay completes, rather than waiting for the next 15-second stats refresh tick.

#### MQTT Stream

- [#17175](https://github.com/emqx/emqx/pull/17175) Fixed an issue where messages delivered from Streams did not apply subscription options such as Subscription Identifier from the stream subscription.

#### Rule Engine

- [#17211](https://github.com/emqx/emqx/pull/17211) Added the `connected_at` field to the `$events/client/connack` Rule Event, which was stated in the documentation but missing from the actual data.

#### Data Integration

- [#17001](https://github.com/emqx/emqx/pull/17001) Fixed an issue where MQTT source failed to receive messages from `$queue/` subscriptions when the remote broker has the Message Queue (mq) feature enabled.

  The root cause was that the MQ message delivery did not include the MQTT v5 Subscription-Identifier property in PUBLISH packets, which the MQTT bridge ingress relies on to route messages from queue subscriptions.

- [#17010](https://github.com/emqx/emqx/pull/17010) Now, `a2a-status` and `a2a-status-source` user properties present in A2A Agent Cards are overridden with EMQX's liveness information to avoid duplicate properties.

- [#17068](https://github.com/emqx/emqx/pull/17068) Fixed EMQX Tables TLS connector startup when `ssl.verify` is `verify_none` and cert file paths are left empty, and aligned Rust NIF TLS verify propagation with connector config.

- [#17084](https://github.com/emqx/emqx/pull/17084) Fixed an issue with MQTT Sources in which, if its Connector used `clean_start = false` and reconnected to a broker with a session containing messages, those messages would not trigger rule actions.

- [#17111](https://github.com/emqx/emqx/pull/17111) Fixed query execution for PostgreSQL connectors in disable prepared statements mode. Previously, concurrent queries could interleave and produce errors.

- [#17113](https://github.com/emqx/emqx/pull/17113) Fixed RocketMQ connector isolation: a misconfigured or unreachable RocketMQ connector no longer destabilizes other RocketMQ connectors on the same node. Previously, one connector with an unreachable broker could stall the shared client supervisor for up to 60 seconds, causing sibling connectors to flap with `resource_health_check_timed_out` and for Dashboard operations on them to hang.

  The default TCP/TLS connect timeout is also lowered from 60 seconds to 10 seconds so a misconfigured server surfaces as failed quickly instead of appearing stuck.

- [#17180](https://github.com/emqx/emqx/pull/17180) Fixed an issue where, under heavy load, a timed out call to a MongoDB process would be interpreted as an unrecoverable error and wouldn't be retried. Now, the message will be retried on such events.

- [#17216](https://github.com/emqx/emqx/pull/17216) Fixed Timescale/PostgreSQL actions to report a structured bad parameter error instead of crashing the database connection process when a quoted JSON numeric string is mapped to a `FLOAT` column.

- [#17250](https://github.com/emqx/emqx/pull/17250) Fixed Redis Sentinel connectors to support separate authentication settings for Redis data nodes and Sentinel nodes.

- [#17293](https://github.com/emqx/emqx/pull/17293) Fixed an issue where, when writing a Parquet file with an object containing a required key but with an `undefined`/`null` value, a corrupt file would be written instead of raising an error.

- [#17303](https://github.com/emqx/emqx/pull/17303) Upgraded Kafka client libraries: `brod` from 4.5.2 to 4.5.4 and `wolff` from 4.1.9 to 4.1.10.

  Notable fixes picked up from upstream:

  - `brod`: fix a race condition during Kafka connection re-authentication (via `kafka_protocol` 4.3.4).
  - `wolff`: under high-memory load control (`drop_if_highmem`), keep a minimum buffer reserve so the producer is not starved of in-flight data; only bytes exceeding the reserve are dropped.

- [#17343](https://github.com/emqx/emqx/pull/17343) Fixed a clustered-config replication bug where importing a data backup (or loading a HOCON config via `emqx ctl conf load` / `PUT /api/v5/configs`) that contained a `file`-type authorization source could leave peer nodes lagging with a `cluster_rpc_apply_failed` / `failed_to_read_acl_file` error.

  The importer used to write the ACL file locally and replace inline `rules` with a `path`, then ship the path-form config across the cluster. Peer nodes have no such file on disk and so could not apply the change. The config sent to the cluster now keeps `rules` inline, so each peer writes its own copy of the ACL file from the replicated content.

- [#17347](https://github.com/emqx/emqx/pull/17347) Upgraded the RocketMQ client dependency to `v0.7.2` to fix memory growth in async producer requests.

- [#17439](https://github.com/emqx/emqx/pull/17439) Fixed an issue where the health check of an Azure Blob Storage Connector could timeout, or generate large bandwidth costs, if the storage account contained too many containers.

- [#17450](https://github.com/emqx/emqx/pull/17450) Fixed an issue where the `/prometheus/data_integration` Prometheus endpoint could respond with a 500 status when using `mode=node`. This issue would only arise when the configuration for Actions and Connectors was manually edited and inconsistent, having an Action whose Connector does not exist.

- [#17474](https://github.com/emqx/emqx/pull/17474) Reduced the overhead of IoTDB REST API connector health checks by using a bounded version query instead of listing all databases on each check.

#### Clustering

- [#17055](https://github.com/emqx/emqx/pull/17055) Fixed an issue where the internal DS Raft upgrade mechanism could become stuck under specific circumstances during a rolling upgrade to EMQX 6.2.0 release, rendering Durable Storage temporarily unavailable until core nodes were restarted.

- [#17099](https://github.com/emqx/emqx/pull/17099) Fixed routing table inconsistency when a disconnected core node reconnects.

- [#17132](https://github.com/emqx/emqx/pull/17132) Fixed an issue where adding or removing topic metrics could fail on a replicate node when its raw config or runtime state had drifted, raising a `cluster_rpc_apply_failed` alarm and stalling cluster RPC replication. Duplicate-add and missing-remove are now rejected on the initiator only, while replicates apply the change idempotently.

- [#17182](https://github.com/emqx/emqx/pull/17182) Bumped emqx-OTP to 27.3.4.2-8 for mria.

  Without this change, during EMQX startup, Mria app boot may get stuck if it's not connected to the cluster.

- [#17198](https://github.com/emqx/emqx/pull/17198) Bumped OTP version to 28.4.1-3 and builder version to 6.1-4.

  Without this change, during EMQX startup, Mria app boot may get stuck if it's not connected to the cluster.

- [#17214](https://github.com/emqx/emqx/pull/17214) Removed cryptic error-level logging of disconnect events from Cluster Link message forwarding MQTT clients, in favor of more user-friendly messages with enough context for troubleshooting. Events similar to this one should no longer appear in the error logs:

  ```
  2026-05-06T03:00:48.738654+00:00 [error] [PoolWorker] unexpected info: {disconnected,141,#{}}
  ```

- [#17218](https://github.com/emqx/emqx/pull/17218) Avoid `bin/emqx` and `bin/emqx_ctl` invocations from triggering `nodeup`/`nodedown` events on the running broker, which previously surfaced as misleading `cm_registry_node_down` warnings in the broker log. The temporary helper nodes started by these scripts now register as hidden Erlang nodes, as intended.

- [#17269](https://github.com/emqx/emqx/pull/17269) Improved cluster recovery after a network partition.

  - Previously, part of the clients connected to the replicant nodes could be lost from the global registry. This could lead to inconsistent behavior during takeover and incorrect information displayed in the Dashboard.

    This fix adds a background process that re-registers the existing clients when network partition is healed. It also adds a new alarm: "Broker is recovering after a network partition", which is raised while the global registry is being rebuilt.

  - Introduced a new cluster auto-heal algorithm that can automatically recover overlapping network partitions.

- [#17342](https://github.com/emqx/emqx/pull/17342) Fixed cluster configuration import failing with a "required_field: node.cookie" schema check error when the exported `cluster.hocon` contained a partial `node` section. Read-only roots (`node`, `rpc`) are not part of the data import anyway, so they are now dropped from the imported config before the pre-flight schema check, letting the running node's own values be used for the validation.

- [#17348](https://github.com/emqx/emqx/pull/17348) Fixed noisy and misleading `emqx ctl conf cluster_sync status` diagnostics when clustered nodes have the same effective checked configuration but different raw configuration representations.

  The command now suppresses raw-only representation differences that do not correspond to checked configuration changes, while still warning when checked configuration is inconsistent. It also avoids crashing when a raw configuration key exists on one node but is missing from another node.

  It also ignores timestamp-only metadata differences in `created_at` and `last_modified_at` for actions, sources, bridges, and rule metadata. Data import or boot-time configuration loading can refresh these generated timestamps on only some nodes even when the effective runtime configuration is otherwise identical.

- [#17349](https://github.com/emqx/emqx/pull/17349) Improved responsiveness of a Cluster Link in situations when route replication was stuck connecting to an unresponsive target cluster. Now, deleting such Cluster Link should finish slightly sooner.

- [#17382](https://github.com/emqx/emqx/pull/17382) Fixed corruption of global channel registry that may occur when cluster experiences a network partition.

- [#17424](https://github.com/emqx/emqx/pull/17424) Fixed a global session registry leak that could leave duplicate or stale entries for the same client ID after a network partition followed by Mnesia autoheal.

  Discard and takeover-kick RPC handlers now also remove the registry row when the target process is no longer alive, and the registration throttle on the connect path now recognizes tombstone rows (no local channel state) and reaps them instead of blocking new connections for the same client ID indefinitely.

- [#17432](https://github.com/emqx/emqx/pull/17432) Fixed an issue where concurrent Cluster Link API requests could return generic error responses, instead of returning either success or not found.

- [#17469](https://github.com/emqx/emqx/pull/17469) Fixed the issue where warnings similar to those below are emitted when enabling or disabling an active Cluster Link.

  ```
  [warning] tag: RESOURCE, msg: handle_resource_metrics_failed, reason: {badkey, matched}, event: matched, ...
  ```

#### Access Control

- [#17045](https://github.com/emqx/emqx/pull/17045) Fixed password-based authentication backends to let the auth chain continue when the CONNECT packet has no password, instead of rejecting the connection immediately.

  Previously, if a client connected without a password, the first password-based authenticator (built-in database, MySQL, PostgreSQL, MongoDB, Redis, or LDAP) in the chain would return an error, blocking any subsequent authenticators from being tried.

- [#17064](https://github.com/emqx/emqx/pull/17064) Closed an authorization gap in the `/authentication/:id/users` REST endpoint so that a namespaced administrator can no longer list or create users in the global (or another tenant's) namespace by omitting the `ns` query parameter or the `namespace` body field. Authentication users in a non-global namespace can no longer be marked as `is_superuser`; requests to create or update such a user are rejected so that explicit ACL rules are always enforced for tenant MQTT clients.

- [#17100](https://github.com/emqx/emqx/pull/17100) Fixed OIDC SSO login failing with `provider_not_ready` when the identity provider returns a JWKS response whose `Content-Type` uses the `+json` structured syntax suffix (e.g. `application/jwk-set+json; charset=utf-8`). Such responses are now accepted as valid JWKS content.

- [#17122](https://github.com/emqx/emqx/pull/17122) Fixed Dashboard RBAC checks for SSO users with URL-encoded usernames such as email addresses, so viewer self-service MFA disable requests work correctly when `force_mfa` is disabled.

- [#17140](https://github.com/emqx/emqx/pull/17140) Fixed a silent failure when EMQX fetched a Certificate Revocation List (CRL) over HTTP from a server that returns a DER-encoded body (`Content-Type: application/pkix-crl`, the format mandated by RFC 5280 §5).

  Previously, EMQX only decoded PEM-encoded CRL bodies; a DER body was silently treated as zero CRLs and cached as an empty list, causing every TLS handshake on `enable_crl_check = true` listeners to fail with `bad_crls, no_relevant_crls` and no log line indicating what went wrong.

  EMQX now decodes both PEM and DER CRL bodies. When a fetched body is neither, a warning is logged with the URL so the misconfiguration is visible.

- [#17171](https://github.com/emqx/emqx/pull/17171) Fixed an RBAC issue that prevented namespaced Dashboard administrators from enabling or disabling MFA for their own account.

  Namespaced administrators remain restricted from managing MFA settings for other Dashboard users.

- [#17177](https://github.com/emqx/emqx/pull/17177) Dashboard-created REST API keys are now generated randomly instead of being derived from the API key name.

- [#17223](https://github.com/emqx/emqx/pull/17223) Fixed missing client certificate when a TCP-passthrough proxy (e.g. GCP TCP Proxy NLB, AWS NLB) is placed in front of an SSL listener with `proxy_protocol = true`. The TLS handshake at the listener was completing successfully and the client certificate was present, but it was not exposed to authentication or rule events. Functions, ACL rules, and authentication backends that depend on the client certificate (CN, subject, full PEM) now work correctly in this deployment shape.

- [#17330](https://github.com/emqx/emqx/pull/17330) Hardened the PROXY Protocol v2 TLV parser on TCP and SSL listeners with `proxy_protocol` enabled. Previously, a TLV whose declared length overran the buffer caused the parser to silently truncate the TLV stream, dropping any trailing fields. The parser is now strict: malformed TLV streams cause the connection to be rejected with a warning log entry instead of being accepted with a partially parsed PROXY header.

- [#17428](https://github.com/emqx/emqx/pull/17428) Fixed a Dashboard OIDC SSO crash that prevented EMQX from completing the OpenID provider discovery when the provider's `.well-known/openid-configuration` response included a `Cache-Control` header such as `max-age=0` (observed with Kanidm). The crash caused the OIDC supervisor to exhaust its restart budget after a single failure, leaving SSO unable to recover without a config re-save. The cache-control parser is now tolerant of these values, the worker no longer hard-crashes on a bad expiry, and the OIDC supervisor allows several restarts within a minute so transient failures retry cleanly.

#### Gateway

- [#17141](https://github.com/emqx/emqx/pull/17141) Fixed CoAP connection-mode token takeover so reconnecting UDP/DTLS clients can resume with a valid token while invalid token/clientid combinations are rejected. Also ensured required connection info fields are present before running CoAP takeover connected hooks.

- [#17258](https://github.com/emqx/emqx/pull/17258) Fixed an issue in the MQTT-SN gateway where a connected client sending a second CONNECT packet on the same session would crash its connection process. The gateway now responds with a DISCONNECT and closes the session gracefully.

- [#17287](https://github.com/emqx/emqx/pull/17287) Fixed MQTT-SN clients crash caused by packets received in unexpected connection or Will states, including `DISCONNECT` during connection setup, `REGISTER` before the Will handshake completes, and `WILLMSGUPD` before a Will topic exists.

- [#17419](https://github.com/emqx/emqx/pull/17419) Fixed CoAP gateway observe notifications to honor the `gateway.coap.notify_type` setting.

  Observe notifications now use a per-session confirmable in-flight window of 1 and a fixed pending queue of 100 entries shared by all observe tokens. When a confirmable notification is in flight, later observe notifications are queued instead of being silently lost. When the queue is full, the oldest pending notification is dropped, `delivery.dropped.queue_full` is incremented, and a throttled warning is logged.

  Cancelling an observe relation now also removes pending notifications for that observed topic/filter and observe token, so queued notifications are not delivered after the client has cancelled the observe, including wildcard observe filters.

- [#17507](https://github.com/emqx/emqx/pull/17507) Fixed several gateway paths that could reach publish or subscribe handling before authentication completed.

  MQTT-SN QoS -1 publishes now use the existing fixed negative-QoS client identity and must pass gateway authentication and publish authorization checks before delivery.

  NATS now honors the security profile when no authentication is configured. Under the hardened profile, anonymous publish, subscribe, and connect attempts are rejected unless listener authentication is explicitly disabled.

  STOMP now rejects SEND and SUBSCRIBE frames before CONNECT completes, including transactional SEND frames.

  CoAP connectionless `/ps` publish and observe requests now authenticate before entering pub/sub handling. Under the hardened profile, such requests are rejected when no authentication is configured unless listener authentication is explicitly disabled.

#### Observability

- [#16956](https://github.com/emqx/emqx/pull/16956) Log client connection termination at warning level instead of info when the reason is `emsgsize` (received packet exceeds `mqtt.max_packet_size`).

- [#17002](https://github.com/emqx/emqx/pull/17002) Updated `minirest` library to version 1.4.12. This version fixes a bug that caused EMQX API to produce malformed API responses with `204 No Content` status line, emitting invalid `content-length` header.

- [#17024](https://github.com/emqx/emqx/pull/17024) Dashboard HTTP listener now automatically uses IPv6 when the bind address is an IPv6 address, removing the need to explicitly set `inet6 = true`.

- [#17054](https://github.com/emqx/emqx/pull/17054) Fixed `GET /api/v5/configs?key=...` returning incomplete data when `Accept: application/json` was set.

  Previously, the JSON response ignored the `key` query parameter and always returned a fixed subset of root configurations, which excluded keys like `multi_tenancy`. The endpoint now honors the `key` parameter in JSON responses consistently with the hocon (text/plain) response.

- [#17118](https://github.com/emqx/emqx/pull/17118) Improved pagination on multi-tenancy list endpoints (`/mt/ns_list`, `/mt/ns_list_details`, `/mt/managed_ns_list`, `/mt/managed_ns_list_details`, `/mt/ns/{ns}/client_list`):

  - Added an RFC 8288 `Link: <?...>; rel="next"` response header. When more pages are available the header carries the query-only URI-reference of the next page; when absent, the current response is the last page. This removes the prior ambiguity where a full page (`len(results) == limit`) could not be distinguished from the exact-boundary "no more data" case without an extra request.
  - Added inclusive keyset cursor query parameters (`first_ns`, `first_clientid`) alongside the existing exclusive cursors (`last_ns`, `last_clientid`). The inclusive form supports exact-match lookup (e.g. `?first_ns=foo&limit=1`) and is preserved across paginated Link headers when the caller opts in. The two forms are mutually exclusive on a single request; supplying both returns HTTP 400.

- [#17134](https://github.com/emqx/emqx/pull/17134) Fixed `invalid json term` error returned by the banned clients listing API for client ID and username regex bans created before 6.2.0. The compiled regex retained in the database from the older release is now translated back to the original pattern string when serializing the response.

- [#17227](https://github.com/emqx/emqx/pull/17227) Cluster config file save errors now name the file and the underlying reason.

  When `cluster.hocon` (or its directory) is read-only, immutable, or otherwise unwritable (e.g. mounted read-only into a container), changing config via the Dashboard or REST API previously returned an opaque HTTP 400 with body `{config_update_crashed,{badmatch,{error,ebusy}}}` and only logged a badmatch crash that did not name the file.

  The error now:

  - Logs `failed_to_save_conf_file` with the actual file path and reason (`eacces`, `eperm`, `ebusy`, ...) plus a hint listing common operator-side causes.
  - Returns a structured HTTP 400 body that names both the file and the reason, so the cause is visible in the Dashboard without digging through node logs.

  Previously, when only the temporary file write failed (e.g. read-only directory), the API silently returned HTTP 200 even though the change was not persisted to disk. The API now correctly reports failure in this case as well.

- [#17246](https://github.com/emqx/emqx/pull/17246) Upgraded `jose` library from 1.11.10 to 1.11.12, picking up EC and EdDSA key fixes for newer OTP releases.

- [#17247](https://github.com/emqx/emqx/pull/17247) When a plugin's REST API callback crashes or runs over its timeout budget, the broker now logs the failing API method and path together with the configured timeout, so the offending call is identifiable in mixed-traffic logs. A timeout is logged as a warning (not an error) and includes a hint pointing at `plugins.api_endpoint.timeout`, the config key to raise when a plugin callback legitimately needs more time.

- [#17254](https://github.com/emqx/emqx/pull/17254) Improved memory-usage reporting inside containers. The broker now picks the most constraining memory reading among cgroup v2, cgroup v1, and the host's `/proc/meminfo` (smallest non-zero total wins, larger usage ratio breaks ties). Previously the reading could be misleading in two ways: on containers with a tight cgroup limit, the host view could indicate >70% while the cgroup limit was <10% (or the reverse); and on hosts where a cgroup is mounted with no memory limit set, the cgroup reading could collapse the reported usage ratio to ~0%. Overload-protection thresholds and the `Memory used` metric now reflect the limit that actually constrains the process.

- [#17319](https://github.com/emqx/emqx/pull/17319) `GET /api/v5/schemas/{hotconf,actions,connectors}` now returns the response with `Content-Type: application/json`. Previously the response body was valid JSON but the header was `text/plain; charset=utf-8`, which broke clients that dispatch on the response content type.

- [#17406](https://github.com/emqx/emqx/pull/17406) Now, events captured by a trace initiated by a namespaced admin are limited to the namespace of such admin, for traces of types topic, IP address, and clientid. Traces of type rule ID already had such behavior.

- [#17473](https://github.com/emqx/emqx/pull/17473) Lowered the log level of `unabled_to_stop_plugin_apps` from warning to info when the plugin's Erlang applications cannot be stopped because other running applications still depend on them. This is an expected, non-actionable condition during plugin unload and no longer raises a warning.

#### Deployment

- [#17311](https://github.com/emqx/emqx/pull/17311) Fixed Docker startup when the container hostname cannot be resolved. The entrypoint now falls back to the interface IP address before auto-generating the node name, and fails with a clear error if no node host can be determined.
- [#17369](https://github.com/emqx/emqx/pull/17369) Moved the Dashboard listener defaults (`http.bind` and the placeholder HTTPS `ssl_options`) from the user-editable `etc/emqx.conf` into the shipped `etc/base.hocon`. Previously, the hardcoded `emqx.conf` block silently reverted runtime updates to the default self-signed certificate on restart. Runtime updates made through the Dashboard, the REST API, or the `emqx_acme` plugin's automatic HTTPS configuration are now correctly preserved across restarts.

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

## 6.1.4

*Release Date: 2026-08-03*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.1.4.

### Enhancements

#### Multi-tenancy

- [#17732](https://github.com/emqx/emqx/pull/17732) Added a `namespace` field to the API key creation and update endpoints. Operators no longer need to encode the namespace in the `role` string. The `ns:<namespace>::<role>` format remains supported. When both forms are provided, their namespace values must match.
- [#17855](https://github.com/emqx/emqx/pull/17855) Enabled namespace-scoped EMQX Dashboard administrators to create, list, read, update, and delete API keys within their own namespace. They cannot create global API keys or keys in another namespace. API keys outside their namespace are not visible to them.

#### Access Control

- [#17813](https://github.com/emqx/emqx/pull/17813) Added validation to the Dashboard user and API key endpoints to reject scope lists that combine administrator-equivalent scopes (referred to as `privilege scopes` in EMQX validation messages), including `system`, `user_management`, `api_key_management`, and `sso_management`, with other scopes. Each of the listed scopes grants administrator-equivalent permissions, so adding other scopes does not restrict the account. Use either only administrator-equivalent scopes or only other scopes, depending on the required permissions.

  Pre-existing records subject to this validation continue to work with mixed scopes. When an explicit scope list is submitted during an update, it must contain either only administrator-equivalent scopes or only other scopes. Namespace-scoped Dashboard administrators are exempt from this mutual-exclusion rule and remain governed by namespace RBAC.

#### Data Integration

- [#17933](https://github.com/emqx/emqx/pull/17933) Added support for a multi-node `servers` list in the RabbitMQ connector, such as `rmq1:5672,rmq2:5672`. The connector provides connection-time failover and rotated starting offsets for connection pools. The `server` and `port` settings remain supported when `servers` is not set.

- [#17944](https://github.com/emqx/emqx/pull/17944) Added OAuth2 Client Credentials authentication to the HTTP connector and to HTTP-based authentication and authorization. When enabled, EMQX obtains and refreshes an access token from the configured token endpoint and adds it to outbound requests as a Bearer authorization header.

  The connector health check reports `disconnected` when a token cannot be obtained. Configurations that enable OAuth2 and also provide an `Authorization` header are rejected.

  EMQX sends the client ID and client secret as form fields in the token request body. Sending the credentials in the HTTP Basic `Authorization` header is not supported.

- [#18014](https://github.com/emqx/emqx/pull/18014) Added automatic prepared statement rebuilding to the Datalayers Arrow Flight connector. If the server loses a prepared statement, such as after a restart, the client automatically recreates the statement and retries the write operation, avoiding write failures caused by the missing statement.

- [#18042](https://github.com/emqx/emqx/pull/18042) Added AWS IAM role credential support to DynamoDB connectors.

  When both the access key ID and secret access key are omitted, EMQX obtains temporary credentials from an ECS task role or EC2 instance metadata and refreshes them before they expire.

- [#18081](https://github.com/emqx/emqx/pull/18081) Improved the resilience of the Snowflake Streaming Action. When a channel's internal state becomes out of sync while rows are being appended, the action treats the error as recoverable, retries the failed rows, and attempts to reopen the channel without manual intervention.

- [#18085](https://github.com/emqx/emqx/pull/18085) Added the following configuration options for Kafka, Confluent, and Azure Event Hubs producers:

  - `max_batch_age` (action): Maximum time that a message can remain in the producer buffer. EMQX drops messages that exceed this duration and increments both the `dropped` and `dropped.expired` metrics. Expired messages are not counted as `failed` or `success`. Default: `infinity` (messages do not expire while buffered).
  - `max_retries` (action): Number of failed Kafka retries allowed for a message batch. EMQX drops the batch after the configured number of retries. The affected messages are counted as `failed`, not `dropped`. The retry counter increases only when Kafka returns an error code; resends after a connection loss do not increase the counter. Default: `infinity` (retry indefinitely).
  - `reconnect_delay` (action): Delay before the producer reconnects after a connection loss. Default: `2s`, which was previously hard-coded.
  - `request_timeout` (connector): Time to wait for a Kafka response before considering the connection stale and reestablishing it. Default: `30s`.

  The Kafka client library was upgraded to `wolff` 4.2.1, restoring `max_linger_time` support for memory-mode buffers. An undersized batch waits up to `max_linger_time` for additional messages, reducing the produce request rate. Full batches are sent without delay.

- [#18110](https://github.com/emqx/emqx/pull/18110) Added support for JSON Schema drafts 2019-09 and 2020-12 in Schema Registry.

- [#18137](https://github.com/emqx/emqx/pull/18137) The GCP Pub/Sub producer and consumer now accept a fully-qualified topic path (`projects/<project-id>/topics/<topic-name>`) in the topic configuration, making it possible to publish to or consume from a topic that lives in a different GCP project than the service account's own. A bare topic name keeps resolving against the service account's project as before. For consumers, the subscription is still created in the service account's project; only the topic reference may point to another project.

#### Plugins

- [#18012](https://github.com/emqx/emqx/pull/18012) Added the `emqx_sync_request` plugin for synchronous MQTT request/response flows through the EMQX REST API. It also provides node-local CLI diagnostics for request counters and current pending state.

#### Packaging

- [#18037](https://github.com/emqx/emqx/pull/18037) Added Enterprise Linux 10 (EL10) packages for Red Hat Enterprise Linux 10, Rocky Linux 10, and compatible distributions.
- [#18127](https://github.com/emqx/emqx/pull/18127) Started releasing packages for macOS 26 (Tahoe).

### Bug Fixes

#### Core MQTT Functionalities

- [#17895](https://github.com/emqx/emqx/pull/17895) Switching a TLS listener from a managed certificate bundle back to file-based certificates now succeeds even if the referenced bundle has already been removed.

- [#17911](https://github.com/emqx/emqx/pull/17911) Added support for validating the `ECDHE-PSK-CHACHA20-POLY1305` cipher suite in DTLS listeners when the Erlang/OTP `ssl` application supports it.

- [#18062](https://github.com/emqx/emqx/pull/18062) Switching a TLS/WSS listener from a managed certificate bundle back to file-based certificates now succeeds when the request clears `managed_certs` by sending it as `null` (as the Dashboard does), even if the bundle has already been deleted.

- [#18102](https://github.com/emqx/emqx/pull/18102) Fixed an issue introduced in EMQX 6.1.2 and 6.2.0 where MQTT clients could receive QoS 1 and QoS 2 messages out of order. This issue affected only deployments with a delivery rate limit configured. EMQX now keeps later messages queued until the rate-limited message can be sent.

- [#18108](https://github.com/emqx/emqx/pull/18108) Deleting a managed certificate bundle (or a single file in it) that is still referenced by some configuration now always fails with a clear error listing the referencing configurations; the `force_delete` query parameter no longer bypasses this check and has been removed from the API schema.

  Additionally, the Prometheus stats endpoint no longer fails entirely when a listener references a certificate bundle that is missing from disk; the affected listener is skipped in the certificate expiry metric and a warning is logged.

- [#18111](https://github.com/emqx/emqx/pull/18111) Extended `mqtt.strict_mode` validation to MQTT v3.1 CONNECT packets. EMQX rejects packets that set the password flag without the username flag, matching its behavior for MQTT v3.1.1. The MQTT v3.1 specification does not allow a password without a user name.

  Improved connection log readability. The CONNECT packet trace uses `Password=undefined` to distinguish a missing password from an empty password. Logs also render the `peername` field as a plain string, such as `10.0.0.1:54123`.

#### Rule Engine

- [#17957](https://github.com/emqx/emqx/pull/17957) Fixed an issue where multiple rule events (for example, `$events/client/connack`) would not trigger rules in the global namespace when `rule_engine.limit_selects_in_namespace = true`.
- [#18049](https://github.com/emqx/emqx/pull/18049) Fixed an issue where setting `rule_engine.limit_selects_in_namespace = true` prevented global rules triggered by alarm activation or deactivation events from firing.

#### Data Integration

- [#17859](https://github.com/emqx/emqx/pull/17859) Fixed the MQTT connector so it can connect to IPv6 brokers.

  Previously, configuring an MQTT connector to an IPv6 broker failed in two ways: an IPv6 literal such as `[::1]:1883` was rejected at save time with a `bad_host_port` validation error, and a hostname that only resolves to an IPv6 (`AAAA`) address failed to connect with a "Could not resolve host" error because the connection defaulted to IPv4.

  The server address parser now accepts bracketed IPv6 literals (for example `[::1]`, `[::1]:1883`, and `mqtt://[::1]:1883`), and the MQTT connector now enables IPv6 probing when connecting, so IPv6-only brokers can be reached.

  The MQTT connector and cluster link `server` address now accept the official MQTT URI schemes `mqtt` (plain TCP) and `mqtts` (TLS), for example `mqtt://broker:1883` and `mqtts://broker:8883`. A scheme-less `host:port` is still accepted. Any other scheme is now rejected with an `unsupported_scheme` validation error.

- [#17947](https://github.com/emqx/emqx/pull/17947) Fixed an issue where updating an HTTP connector could leave its action buffer workers blocked after the connector was recreated, causing messages to remain queued until the next retry interval.

- [#17955](https://github.com/emqx/emqx/pull/17955) Fixed an issue that could leave GreptimeDB asynchronous batches unflushed after health checks at low write rates.

- [#17961](https://github.com/emqx/emqx/pull/17961) Fixed an issue where a Kafka or Pulsar connector could transition to `disconnected` after a health check timeout, which could cause its internal queue to be recreated. Kafka and Pulsar connectors transition to `connecting` after such timeouts.

- [#17970](https://github.com/emqx/emqx/pull/17970) When SSRF protection is enabled, managing connectors is no longer disrupted by an existing connector whose address is now blocked by the policy.

  Previously, enabling SSRF protection (or extending its deny list) after connectors were created could make unrelated connector operations fail with an internal error, and deleting an affected connector could leave it behind after its actions and rules were already removed.

  SSRF protection now applies to HTTP and MQTT connectors and is enforced when a connector is created or updated: creating or updating such a connector with a blocked address is rejected. Enabling, disabling and deleting connectors are never blocked, and other connector types are not subject to the policy.

- [#17973](https://github.com/emqx/emqx/pull/17973) Fixed Kafka producer action retry metrics. The `retried`, `retried.success`, and `retried.failed` counters on an action's metrics now reflect messages that the internal buffer re-sends after a broker reconnect, so an operator can tell whether retried messages ultimately succeeded or failed. Previously these counters stayed at `0` regardless of how many internal retries occurred. The `success` and `failed` counters are unaffected and are not double-counted.

- [#17982](https://github.com/emqx/emqx/pull/17982) Updated the GCP Pub/Sub consumer to use HTTP/2 and cancel an active pull request when it times out. Canceling the HTTP/2 stream gives the GCP server a clearer signal that the request has ended and may allow the messages to be leased to a subsequent pull request, reducing tail latency.

- [#18055](https://github.com/emqx/emqx/pull/18055) Fixed an issue that caused Snowflake Streaming Actions on different cluster nodes to fail with the following error:

  ```text
  {unrecoverable_error,#{body => <<"{\"code\":\"STALE_CONTINUATION_TOKEN_SEQUENCER\",\"message\":\"Channel sequencer in the continuation token is stale. Please reopen the channel\"}">>,...
  ```

- [#18110](https://github.com/emqx/emqx/pull/18110) Fixed an issue where using the `examples` annotation in a draft-06 JSON Schema in Schema Registry would result in valid data being rejected as invalid.

#### Clustering

- [#17995](https://github.com/emqx/emqx/pull/17995) Fixed an issue that could terminate a node while it joined a cluster whose persisted `mqtt.max_packet_size` differed from its local configuration. EMQX now skips listener refresh side effects before listener startup and creates the listeners from the synchronized configuration when the EMQX application starts.

- [#17999](https://github.com/emqx/emqx/pull/17999) Fixed a startup crash-loop that could occur when a node using the community (single-node) license joins a cluster whose peers hold a clustering-capable license.

  Previously, if cluster membership was established before the peer's license was replicated to the joining node, the node would refuse to start with a `SINGLE_NODE_LICENSE` error and, under an automatic-restart supervisor, keep crash-looping. The node now waits a bounded grace period for the clustering license to sync before it starts. A cluster in which no node ever obtains a clustering license is still rejected after the grace period elapses.

- [#18077](https://github.com/emqx/emqx/pull/18077) Fixed a crash when a node received a `cluster join` request (CLI or API) before it had fully booted: joining restarts the internal database while applications are still starting, which could bring the whole node down. Such requests are now rejected with a clear error message; retry after the node is fully started.

#### Access Control

- [#17806](https://github.com/emqx/emqx/pull/17806) Aligned the data backup import and export endpoints with the principle of least privilege: Dashboard users whose scope set does not include both `user_management` and `api_key_management` can no longer import or export archives containing the `dashboard_users` or `api_keys` table sets. Global administrators and API-key callers with the necessary scopes are unaffected.

- [#17853](https://github.com/emqx/emqx/pull/17853) Improved the redaction of sensitive HTTP request headers in connector debug logs. EMQX stores the `x-api-key`, `x-auth-token`, `api-key`, and `cookie` headers as secrets in connector state, as it does for `Authorization` and `Proxy-Authorization`. Their values are omitted when connector state is logged at trace or debug level.

  The shared header-redaction helper also recognizes header names stored as iolists, including names produced by the connector's template parser.

- [#17871](https://github.com/emqx/emqx/pull/17871) Creating a super-user in a non-global namespace is now rejected when importing built-in-database users in bulk or via a bootstrap file, matching the per-user management API. Such rows are reported as failed and are not stored.

- [#17974](https://github.com/emqx/emqx/pull/17974) Raw MQTT packet data is now redacted by default in connection logs; trusted client IP addresses can be allowlisted per listener for diagnostics.

- [#18005](https://github.com/emqx/emqx/pull/18005) Fixed an issue where CLI audit logs could store sensitive command arguments.

- [#18009](https://github.com/emqx/emqx/pull/18009) Made scope handling consistent for administrator and API key records that use their role's implicit default scopes (shown as `unset`). Updated read and write operations to accept unset-equivalent scope lists. These records retain their forward-compatible implicit scopes instead of a frozen list, so scopes added in later releases take effect automatically.

  - Fixed an issue where `PUT /api/v5/users/{username}` rejected a request that updated only the default administrator's note (description). EMQX no longer treats the default scope list included in such a request as an explicit assignment. If the submitted value is `unset` or a list that matches the administrator role's full default scope set, EMQX treats it as "no explicit scopes" and allows the note update.
  - [#18196](https://github.com/emqx/emqx/pull/18196) When creating or updating an API key, EMQX treats `unset` or a list that matches the role's default scopes as "no explicit scopes." As a result, the `scopes` value returned when retrieving the API key can be submitted unchanged without causing the request to fail.
  - [#18221](https://github.com/emqx/emqx/pull/18221) EMQX no longer creates the default administrator with an explicit scope list at startup. At boot, EMQX updates default administrator records that carry an explicit list to the implicit form.

#### Multi-tenancy

- [#17807](https://github.com/emqx/emqx/pull/17807) Added an isolated backup space for namespace-scoped administrators. Their export, upload, list, download, import, and delete operations through the data backup endpoints (`/data/export`, `/data/import`, `/data/files`, and `/data/files/:filename`) act only on backups in their own namespace. Namespace-scoped administrators cannot view, download, or delete global backups or backups from another namespace.

  Global administrators continue to manage global backups by default, including backups created before this change. They can pass a `namespace` query parameter to `GET /data/files`, `DELETE /data/files`, or `GET /data/files/:filename` to inspect or remove backups from a specific namespace.

- [#17975](https://github.com/emqx/emqx/pull/17975) Prevented namespace-scoped callers from updating the global tracing configuration through `PUT /api/v5/tracing`. Namespace-scoped Dashboard users and namespace-scoped API keys receive HTTP status code `403`. Global Dashboard administrators retain access to the endpoint.

- [#18008](https://github.com/emqx/emqx/pull/18008) Added support for global administrators to pass the optional `namespace` query parameter to `POST /api/v5/data/import` and `POST /api/v5/data/files` when importing or uploading a backup for a specific namespace. If the parameter is omitted, the operation uses the global backup scope.

  For namespace-scoped administrators, EMQX ignores the `namespace` query parameter and confines the operation to the caller's own namespace. This behavior is consistent with backup listing and download operations.

- [#18117](https://github.com/emqx/emqx/pull/18117) Deleting a namespace now also removes the namespace's built-in database authentication users (both password-based and SCRAM) and authorization rules. Previously, these records persisted after namespace deletion and reappeared if a namespace with the same name was created later.

  Additionally, a new `emqx ctl mt purge_ns <namespace>` CLI command deletes a namespace and purges all data belonging to it. The command is idempotent and does not require the namespace to exist, so it can be used as a last resort to clean up leftover data if a previous namespace deletion was interrupted.

#### Gateway

- [#17796](https://github.com/emqx/emqx/pull/17796) Fixed a crash in the MQTT-SN gateway when a new device connects from a UDP source port that was recently used by a disconnected device (common on loopback and behind NAT, where the OS or NAT box re-assigns the same port). The stale channel is now retired cleanly and the new connection is processed as a fresh session.

- [#17805](https://github.com/emqx/emqx/pull/17805) Fixed an issue where re-loading a gateway could fail with an `already_started` error after a previous load attempt aborted partway through (for example due to an invalid configuration or a busy listener port). The leftover locker process from the failed attempt is now reclaimed automatically, so the next `load` (or operator retry) starts from a clean state.

- [#17815](https://github.com/emqx/emqx/pull/17815) Fixed MQTT-SN UDP session routing when UDP source tuples change or are reused.

  MQTT-SN UDP listeners now route packets by the ClientId parsed from the packet through `esockd_udp_proxy`, allowing asleep sessions to resume from a different UDP source tuple while preventing a reused UDP source tuple from delivering another ClientId's packets to the old session.

- [#17888](https://github.com/emqx/emqx/pull/17888) Fixed an issue where the LwM2M gateway could include sensitive REGISTER query fields, such as `password`, `secret`, `private_key`, and `access_token`, in registration and update MQTT reports.

- [#18051](https://github.com/emqx/emqx/pull/18051) Fixed an issue where CoAP debug logs could expose sensitive URI query values.

#### Plugins

- [#17861](https://github.com/emqx/emqx/pull/17861) Restored the previous plugin startup behavior by no longer deleting local plugin packages that are missing from the cluster plugin configuration when a node starts or rejoins the cluster.

- [#17884](https://github.com/emqx/emqx/pull/17884) Fixed the plugin management HTTP APIs to ignore stale unpacked plugin directories that are absent from the cluster plugin configuration and are not running locally.

  Such stale packages do not appear in plugin list, detail, configuration, or schema responses. Plugin operation APIs cannot act on these packages, and the packages do not block reinstallation through the HTTP install API. Configured preinstalled plugins remain visible and continue to follow the documented preinstallation workflow.

  EMQX logs an error during startup or HTTP API access when a plugin package is unpacked but is neither enabled nor disabled in `plugins.states`.

- [#17932](https://github.com/emqx/emqx/pull/17932) Fixed a noisy `failed_to_get_plugin_config_from_cluster` warning when installing plugins through the CLI.

  The `emqx ctl plugins install` command uses `fresh_install` mode, consistent with the HTTP API behavior. This mode skips the cluster configuration lookup for newly installed plugins and prevents repeated `config_not_found_on_node` warnings on every cluster node.

  Added a `--cluster` flag to `emqx ctl plugins install` for cluster-wide installation. When the flag is specified, the command distributes and installs the plugin package on all running nodes.

- [#18018](https://github.com/emqx/emqx/pull/18018) Fixed an issue where plugin installation loaded code before validating the package's application declarations, configuration schema, and default configuration.

#### Observability

- [#17886](https://github.com/emqx/emqx/pull/17886) Exposed the publish quota-exceeded packet metric in Prometheus as `emqx_packets_publish_quota_exceeded`.

- [#18114](https://github.com/emqx/emqx/pull/18114) Fixed an issue where the Dashboard metrics APIs (`GET /api/v5/monitor_current` and `GET /api/v5/monitor`) returned `500 INTERNAL_ERROR` while a node was joining the cluster.

  If metrics cannot be sampled while a joining node restarts its applications, the APIs return aggregated metrics from the remaining reachable nodes and log a warning instead of failing the entire request.

  Also fixed a spurious `clear_monitor_metrics_rpc_errors` warning that was logged after every successful `DELETE /api/v5/monitor` request.

#### File Transfer

- [#18069](https://github.com/emqx/emqx/pull/18069) Fixed the file transfer files API (`GET /api/v5/file_transfer/files`) failing with a 500 error when listing files whose names contain non-ASCII characters (e.g. Chinese).

#### Deployment

- [#17877](https://github.com/emqx/emqx/pull/17877) Fixed the `emqx-enterprise` Helm chart hardcoding `svc.cluster.local` in the node's host name. On a Kubernetes cluster whose DNS domain is not `cluster.local`, a node named itself with an unresolvable FQDN, so Erlang distribution could not start and the nodes failed to form a cluster. The host name now follows the chart's `clusterDomain` value, which already governed the DNS and Kubernetes discovery settings.

## 6.1.3

*Release Date: 2026-07-01*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.1.3.

### Enhancements

#### Clustering

- [#17530](https://github.com/emqx/emqx/pull/17530) Cluster linking now requires a non-community license. Under the default community license, configured links stay inactive (no message forwarding or route replication) and the REST API rejects attempts to enable a link with a clear hint to load a non-community license. Disabling and deleting links remain available so that legacy configuration can be tidied up. After upgrading the license, links can be enabled from the Dashboard or REST API without restarting the node.
- [#17549](https://github.com/emqx/emqx/pull/17549) Added the EMQX Backup Sync plugin to periodically synchronize selected configuration from a primary cluster to a secondary cluster using the Data Backup APIs. The plugin supports configurable TLS options for HTTPS calls to the primary cluster.

- [#17620](https://github.com/emqx/emqx/pull/17620) Added an operator-facing diagnostics module `emqx_router_tool` for inspecting and reconciling routing tables. The module is intended to be run via `emqx ctl eval` and provides three helpers:

  - `cluster_schema_view/0` reports the route storage schema each cluster node is running.
  - `scan_missing_routes/0,1` streams the local subscription table and reports topics whose route entry is missing for this node. The scan runs in two passes, is throttled, and tolerates concurrent subscribes and unsubscribes.
  - `reconcile_missing_routes/0,1` re-adds the missing routes via the existing `emqx_router:add_route/2` API.

  The module is schema-agnostic and safe to run on a live cluster.

#### Multi-Tenancy

- [#17711](https://github.com/emqx/emqx/pull/17711) Made namespace selection consistent across the built-in database authentication user HTTP APIs, and allowed cleanup of records left over from a deleted namespace.

  Previously only user creation accepted a `namespace` field in the request body; updating and deleting a user accepted the target namespace only through the `ns` query parameter. The update and delete endpoints now also accept a `namespace` field in the request body. When both are provided, the `ns` query parameter takes precedence. Listing users continues to use the `ns` query parameter.

- [#17665](https://github.com/emqx/emqx/pull/17665) Added per-namespace counters for dropped messages and dropped deliveries in the multi-tenancy app. These are exposed at `/api/v5/prometheus/namespaced_stats` with a `namespace` label, alongside the existing per-namespace metric families. Operators can now diagnose drop rates per tenant from Prometheus without resorting to log inspection.

  Known limitation: QoS 2 PUBREL await-timeout drops do not yet have per-namespace attribution because that drop path bumps the global counter without firing the `message.dropped` hook.

#### Data Integration

- [#17481](https://github.com/emqx/emqx/pull/17481) Added a `retain_as_published` option to MQTT bridge ingress (source) subscriptions. When the bridge connects to the remote broker using MQTT 5.0 and `retain_as_published = true`, the original `retain` flag on forwarded messages is preserved instead of being cleared, allowing the bridge to faithfully republish retained messages from upstream. The default is `false` to keep existing behavior. The option has no effect when `proto_ver` is `v3` or `v4`.

  Also, the connector now emits a warning log when `bridge_mode = true` is configured together with `proto_ver = v5`, since the legacy bridge-mode flag has no effect under MQTT 5.0; set `retain_as_published` on individual subscriptions instead.

- [#17508](https://github.com/emqx/emqx/pull/17508) Set the PostgreSQL `application_name` startup parameter to `emqx` for PostgreSQL and TimescaleDB connector connections.

  This makes EMQX database sessions easier to identify in PostgreSQL logs and views such as `pg_stat_activity`.

- [#17576](https://github.com/emqx/emqx/pull/17576) Added TLS cipher suite configuration support for the GreptimeDB connector via the existing `ssl.ciphers` field. When a cipher list is specified, TLS negotiation is restricted to those suites. Unsupported ciphers are rejected at connector startup.

- [#17594](https://github.com/emqx/emqx/pull/17594) Added support for configuring Google Cloud Pub/Sub and BigQuery connector `service_account_json` values with `file://` secret files, so service account credentials can be injected from external files.

- [#17717](https://github.com/emqx/emqx/pull/17717) Added an option to enable TLS peer verification for Confluent Producer connectors.

- [#17718](https://github.com/emqx/emqx/pull/17718) Added an option to enable TLS peer verification for GCP PubSub Producer/Consumer and BigQuery connectors.

#### Observability

- [#17712](https://github.com/emqx/emqx/pull/17712) Added `emqx_session_tool`, a diagnostic module that operators can call from the remote console. Use `emqx_session_tool:top_by(mqueue_len)` to find the top-K sessions by gauge or counter value in clusters with many connections. Other session metrics, such as `mqueue_dropped` and `inflight_cnt`, are also supported. This helps operators find the busiest sessions without paging through the client list manually.

  The scan streams the channel registry, keeps only a bounded top-K result, and reads cached per-session metrics without sending messages to connection processes. `emqx_session_tool:cluster_top_by/1` aggregates the result across all cluster nodes.

- [#17558](https://github.com/emqx/emqx/pull/17558) Added two new metrics and corresponding rates to the `GET /monitor_current` HTTP API: `rules_matched` and `actions_executed`. They track the number of rules matched and the action execution rate (success + failure), respectively.

  Also fixed `actions.executed` undercounting action invocations in non-batch mode (`batch_size = 1`): the counter is now incremented once per action callback invocation, independently of the buffer-worker telemetry flush window.

### Bug Fixes

#### Core MQTT Functionalities

- [#17529](https://github.com/emqx/emqx/pull/17529) Fixed an issue where QoS 0 messages delivered through Message Queue subscriptions could remain unacknowledged internally, causing the queue subscriber to stop receiving more messages after reaching its local inflight limit.
- [#17540](https://github.com/emqx/emqx/pull/17540) Fixed a bug where setting `password = "file://..."` on an SSL listener caused config validation to fail with `bad_password_or_invalid_keyfile` when the keyfile was encrypted. The `file://` reference is now resolved during validation, not only at runtime.
- [#17569](https://github.com/emqx/emqx/pull/17569) Reduced MQTT v5 user-property parsing cost from quadratic to linear.

  Previously a CONNECT, PUBLISH or SUBSCRIBE packet carrying many user-properties caused super-linear scheduler time on the owning connection process, because each parsed property was appended to the end of the accumulated list. Parsing now scales linearly with the number of entries while preserving their wire order.

- [#17731](https://github.com/emqx/emqx/pull/17731) Fixed a transient "address already in use" error that could occur when updating the options of a WS or WSS listener (for example when rotating TLS certificates). Updating such a listener rebinds its port, and the operating system may not have released the old socket yet; EMQX now retries the rebind briefly instead of failing the update.

- [#17798](https://github.com/emqx/emqx/pull/17798) Fixed an issue where retained messages could be delivered with the original publish QoS instead of the wildcard subscription QoS limit.

- [#17801](https://github.com/emqx/emqx/pull/17801) The `ssl_opts.ciphers` validator now accepts cipher names in either OpenSSL or IANA/RFC naming convention. Previously, only OpenSSL-format names were recognized, so a valid TLS 1.2 cipher supplied in its IANA name (for example, `TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384`) was rejected as `bad_ciphers` even though Erlang's `ssl` module would have accepted it. TLS 1.3 ciphers were unaffected because their IANA and OpenSSL names are identical.

#### Queue and Stream

- [#17515](https://github.com/emqx/emqx/pull/17515) Fixed an issue where Message Queue subscriptions using QoS 0 could stop receiving messages after the queue subscriber's local inflight window became full.
- [#17733](https://github.com/emqx/emqx/pull/17733) Fixed an issue where Message Queue consumers could fail to restore an empty stream buffer after durable storage subscription recovery.

#### Rule Engine

- [#17725](https://github.com/emqx/emqx/pull/17725) Fixed a bug introduced in 6.0.3, 6.1.2 and 6.2.1 where a global rule could stop matching messages on its `FROM` topic when publishing clients carried a tenant namespace (`client_attrs.tns`).

  With `rule_engine.limit_selects_in_namespace` enabled (the default), global rules now retain system-wide visibility and match messages from any namespace. Rules created inside a namespace remain isolated to their own namespace. Operators who prefer to disable namespace restriction entirely can still set `rule_engine.limit_selects_in_namespace = false`.

#### Data Integration

- [#17568](https://github.com/emqx/emqx/pull/17568) Upgraded the Kafka client library `brod` to 4.5.5.

  Consumer group: respect the broker-assigned member ID when the join response carries the `member_id_required` error code (returned by older Kafka brokers, e.g. 2.2.0, that do not support static member instance IDs). Previously the member ID was discarded on error, preventing the retry from succeeding.

- [#17579](https://github.com/emqx/emqx/pull/17579) Fixed Redis Sentinel connectors to use isolated Sentinel managers per resource and clean them up when resources stop, avoiding shared Sentinel state across connectors.

- [#17584](https://github.com/emqx/emqx/pull/17584) Limited the amount of data returned during connector health checks of Snowflake aggregated connectors. This only has observable effects if the list of existing schemas was very large, in which case the health check will take far less time to execute.

- [#17588](https://github.com/emqx/emqx/pull/17588) Limited the amount of data returned during connector and action health checks of Kinesis integrations. This only has observable effects if the list of existing schemas was very large, in which case the health check will take far less time to execute.

- [#17595](https://github.com/emqx/emqx/pull/17595) Limited the amount of data returned during connector health checks of S3 and S3 Tables integrations. This only has observable effects if the list of existing buckets was very large, in which case the health check will take far less time to execute.

- [#17598](https://github.com/emqx/emqx/pull/17598) Fixed a connection failure to MongoDB 8.0+ when authentication is required. The driver previously queried `buildInfo` before authentication to pick the auth mechanism; MongoDB 8.0 restricted that command to authenticated callers. The driver now skips the probe and uses SCRAM-SHA-1 directly, which all supported MongoDB versions accept.

- [#17605](https://github.com/emqx/emqx/pull/17605) Fixed Oracle action prepare/status checks to parse action SQL without executing it, and reject unsupported top-level DDL/DCL/TCL statements. Also improved support for text payloads over 4000 bytes when the payload placeholder is the last bind parameter.

- [#17625](https://github.com/emqx/emqx/pull/17625) Fixed an issue with GCP PubSub Consumer source where, if a source was initially created with a service account lacking necessary permissions to create subscriptions for the configured topic, the source would fail to become `connected` even after granting the permissions to the service account.

- [#17633](https://github.com/emqx/emqx/pull/17633) Fixed an issue introduced in 6.1.2 and 6.2.1 where MQTT bridges and Cluster Link connections over TLS could stall after a short period of traffic. Affected nodes log a recurring error message like `unexpected_event ... ssl_passive ...` from the `emqtt` client. EMQX now bundles `emqtt` 1.15.3, which restores normal traffic flow after the bug was first reported in [#17617](https://github.com/emqx/emqx/issues/17617).

- [#17649](https://github.com/emqx/emqx/pull/17649) Improved the responsiveness of starting and stopping GCP PubSub Consumer connectors. Previously, if the connections were slow or busy, timeouts could leave the connectors running in a state inconsistent with the configuration.

- [#17681](https://github.com/emqx/emqx/pull/17681) Fixed PostgreSQL connector batch writes when prepared statements are disabled.

  Previously, concurrent batches on the same connection could interleave raw SQL parsing and fail with PostgreSQL protocol errors. Table-existence checks are also serialized through the connector worker to avoid interleaving with batch execution.

- [#17701](https://github.com/emqx/emqx/pull/17701) Fixed a confusing `badarith` error from PostgreSQL actions when a batched SQL template returns rows, for example `SELECT ...`.

  PostgreSQL action batching does not support row-returning SQL. EMQX now returns a clear unsupported SQL error instead of crashing the batch result handler.

#### Clustering

- [#17586](https://github.com/emqx/emqx/pull/17586) Periodically purge stale entries from the global session registry.

  Previously, if a session's owner process died without a clean unregister, the registry row could remain forever when the same client ID never reconnected. This could happen, for example, after a brief network split that prevented the unregister from replicating, or when one core node's consensus check timed out during down-event cleanup.

  A new throttled background sweep on each core node now removes such rows. The sweep is bounded to at most 500 registry rows per second per node and runs no more often than once every 10 minutes, so it does not measurably affect broker throughput even on registries holding millions of sessions.

- [#17773](https://github.com/emqx/emqx/pull/17773) Fixed configuration update commands (REST API and CLI) crashing with a `function_clause` crash report when the underlying cluster RPC layer aborted unexpectedly. For example, this could happen with `{no_exists, cluster_rpc_mfa}` when the cluster RPC tables were not yet available during node startup or recovery. Such failures are now returned to the caller as a structured error instead.

- [#17764](https://github.com/emqx/emqx/pull/17764) Fixed an issue where stale plugin entries could remain on a node after it rejoined the cluster if the plugin had been uninstalled while the node was offline. During plugin startup, EMQX now removes local plugin packages that are no longer present in the cluster plugin configuration.

#### Access Control

- [#17575](https://github.com/emqx/emqx/pull/17575) Fixed a race condition in the `emqx_username_quota` plugin that could cause the per-username session counter to become inconsistent with the actual number of tracked client records. The counter could be decremented past zero and then be deleted while a concurrent session registration incremented it, losing the increment permanently.

- [#17644](https://github.com/emqx/emqx/pull/17644) Fixed an issue where the `plain` password hash algorithm accepted passwords that differed only by letter case during authentication.

- [#17646](https://github.com/emqx/emqx/pull/17646) Fixed an HTTP/1.1 protocol-conformance issue in the JWKS retrieval client used by JWT authentication. Earlier versions sent an empty `TE:` header value due to a long-standing default in Erlang/OTP's `inets` HTTP client (fixed upstream in inets 9.4.2 / OTP 28.1). Some identity providers (notably PingFederate) reject such requests. EMQX now sends an explicit, valid `TE: trailers` header on JWKS fetches.

- [#17653](https://github.com/emqx/emqx/pull/17653) Fixed a security issue where the Prometheus configuration API returned stored `Authorization` header values in push gateway headers. The API now redacts these values in responses.

- [#17654](https://github.com/emqx/emqx/pull/17654) Fixed an issue where creating an authenticator via `POST /authentication` returned the new authenticator config without redacting provider secrets (such as JWT HMAC secrets, HTTP `Authorization` headers, and request body passwords). The creation response now applies the same redaction as the list and get endpoints.

- [#17657](https://github.com/emqx/emqx/pull/17657) Fixed a security issue where raw `authorization` and `cookie` headers were forwarded to plugin API callbacks. These credential-bearing headers are now redacted before reaching plugin code.

- [#17711](https://github.com/emqx/emqx/pull/17711) Creating or updating a built-in database user now fails with "Managed namespace not found" if the target namespace is not a known managed namespace. Previously, a user could be created with a nonexistent namespace when the namespace was supplied in the request body.

  In addition, global administrators can now delete built-in database users that belong to namespaces that have already been deleted, instead of receiving a "Managed namespace not found" error.

- [#17736](https://github.com/emqx/emqx/pull/17736) Restricted the JWT authenticator to verify tokens using only JWS algorithms consistent with the configured key type. HMAC-based authenticators now accept only `HS256`, `HS384`, and `HS512`. Public-key and JWKS authenticators accept `RS*`, `PS*`, `ES*`, and `EdDSA` algorithms. Tokens whose `alg` header does not match the configured key type, including `alg=none`, are rejected.

- [#17739](https://github.com/emqx/emqx/pull/17739) Improved redaction of sensitive data in logs, traces, and audit records.

- [#17787](https://github.com/emqx/emqx/pull/17787) Prevented HTTP connector error logs from including request headers when an `ehttpc` worker is terminated before a request returns.

  Previously, if the HTTP connector's `ehttpc` worker was terminated while a request was in flight (for example, by deleting the source before the request returned), the resulting EXIT reason carried the original `gen_server:call` arguments. Because those arguments include the request headers, the headers were written verbatim to the error log. EMQX now removes the call arguments from the reason before it is logged.

- [#17790](https://github.com/emqx/emqx/pull/17790) Stopped writing the TOTP shared secret to the `dashboard_login_failed` server log. The secret was previously included in this log entry during first-time MFA setup.

- [#17791](https://github.com/emqx/emqx/pull/17791) Improved log redaction so that JWT HMAC key bytes no longer appear in `cluster_rpc_apply_result` and `cluster_rpc_apply_ok` debug log lines emitted during configuration updates.

  The redactor now recognizes the internal JWK record shape and replaces it with a placeholder before logging, and also treats the `jwk` field as sensitive.

#### Multi-Tenancy

- [#17715](https://github.com/emqx/emqx/pull/17715) Fixed a multi-tenancy gating gap. When `multi_tenancy.post_auth_tns_expression` was configured and evaluated to an empty string or an error, the namespace gate (`allow_only_managed_namespaces` enforcement, session quota, etc.) was previously skipped, allowing the client through.

  Empty-string and error outcomes are now treated as "no namespace assigned" and pass through the same gate as clients that supplied no namespace before authentication. The client is rejected when `allow_only_managed_namespaces = true`, and accepted without a namespace when it is `false`. In this case, any namespace value carried in `client_attrs.tns` from before authentication is also cleared, so it is not retained when the expression declines to assign one.

- [#17757](https://github.com/emqx/emqx/pull/17757) Fixed `/prometheus/namespaced_stats` so that namespaced admins/API keys can only see data from their own namespace. Global admins/API keys can still see data from all namespaces.

#### Gateway

- [#17556](https://github.com/emqx/emqx/pull/17556) Fixed an issue where the OCPP gateway did not pass the listener `enable_authn` option to the shared authentication flow. This happened because the option was stored under a misspelled client-info key.
- [#17581](https://github.com/emqx/emqx/pull/17581) Fixed the JT/T 808 gateway to use the phone number accepted during authentication as the connection identity, rejecting mismatched registration-code authentication attempts and subsequent uplink frames with a different phone number.
- [#17604](https://github.com/emqx/emqx/pull/17604) Fixed GBT32960 gateway routing: vehicle responses to downstream commands (Parameter Query, Parameter Setting, Terminal Control) are now correctly published to `upstream/response` instead of `upstream/transparent`.
- [#17765](https://github.com/emqx/emqx/pull/17765) Fixed missing authorization checks in several gateway publishing and subscription flows. Authorization is now checked before the following operations: MQTT-SN Will message publishing; JT808 upstream publishing and automatic downlink subscription; GBT32960 upstream publishing and automatic downlink subscription; and OCPP upstream publishing and automatic downlink subscription.

#### Observability

- [#17497](https://github.com/emqx/emqx/pull/17497) Fixed the `actions.executed` metric undercounting `actions.messages` for actions configured in non-batch mode (`batch_size = 1`).

  The previous implementation incremented `actions.executed` once per buffer-worker telemetry flush, which could aggregate many individual completions into one event, so `actions.executed` fell behind `actions.messages` even when no batching was configured.

  The two metrics are now incremented at independent call sites: `actions.executed` once per action callback invocation (one per batch in batch mode, one per message in single mode), `actions.messages` per message handled.

- [#17513](https://github.com/emqx/emqx/pull/17513) Fixed Prometheus matched authorization allow/deny metrics so they reflect real matched authorization decisions.

- [#17536](https://github.com/emqx/emqx/pull/17536) Documented the `file://` option in Dashboard tooltips for the SSL listener `password` and other secret-typed configuration fields (MQTT bridge password, cluster link password, Dashboard OIDC client secret, S3 secret access key, AI completion API key, Pulsar/RocketMQ credentials, etc.). The generic secret type description already mentioned this convention, but field-specific descriptions shadowed it in the Dashboard, causing users to assume the field accepted only literal values.

- [#17708](https://github.com/emqx/emqx/pull/17708) Fixed a logger JSON formatter crash that could replace some debug-level trace events with a `FORMATTER CRASH` line.

## 6.1.2

*Release Date: 2026-06-09*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.1.2.

### Enhancements

#### Security Hardening

- [#17040](https://github.com/emqx/emqx/pull/17040) Restricted API key access to Dashboard user-account management endpoints.

  Previously, an API key with the `administrator` role could call the Dashboard user management endpoints `POST/DELETE /users/:username/mfa` and `POST /users/:username/change_pwd` via HTTP Basic authentication. This meant an API key could reset or disable another Dashboard user's MFA, or change another Dashboard user's password, bypassing the intended separation between human Dashboard sessions and machine API keys.

  These endpoints now return `401 API_KEY_NOT_ALLOW` when accessed via an API key, consistent with the existing policy that blocks API key access to `/users`, `/users/:username`, `/logout`, and `/api_key`. Dashboard users can still manage their own MFA and password from the Dashboard UI using bearer-token (JWT) sessions.

- [#17065](https://github.com/emqx/emqx/pull/17065) Added SSRF protection for rule-engine-reachable connector and bridge configurations.

  When `rule_engine.ssrf.enable` is set to `true`, EMQX applies an outbound SSRF policy to connector, bridge, and action configurations. The policy evaluates each target as follows: exact matches in `rule_engine.ssrf.deny_hosts` are rejected immediately; resolved target IPs are then checked against `rule_engine.ssrf.allow_cidrs` before `rule_engine.ssrf.deny_cidrs`. The default denied ranges cover loopback, link-local (including cloud instance-metadata endpoints), RFC 1918, ULA, unspecified, and multicast ranges. The check runs at config-update time and covers HTTP `url` fields as well as `server`, `servers`, and `bootstrap_hosts` fields across all connector families.

  The feature is disabled by default to preserve compatibility with deployments whose connectors legitimately point at internal services. Operators in multi-tenant or externally-exposed setups are encouraged to enable it together with a network-layer egress firewall.

- [#17173](https://github.com/emqx/emqx/pull/17173) Restricted API keys from exporting or importing Dashboard accounts and API keys via the data backup endpoints.

  `POST /data/export` called with an API key now silently omits the `dashboard_users` and `api_keys` mnesia table sets from the resulting archive. `POST /data/import` called with an API key now returns `403 FORBIDDEN` when the uploaded backup contains either of those table sets.

  Dashboard bearer-token (login) callers are unaffected and continue to be able to back up and restore the full database, including Dashboard users and API keys.

  This closes a privilege-escalation gap. The existing `/users` and `/api_key` endpoints already deny API keys access to Dashboard login credentials and API key records, but an API key holder could bypass those restrictions by going through the data backup endpoints instead.

- [#17187](https://github.com/emqx/emqx/pull/17187) Removed the EMQX release version (`rel_vsn`) from the unauthenticated `GET /status?format=json` response to avoid disclosing the broker version to unauthenticated callers. The version remains available via the authenticated node-info APIs.

- [#17201](https://github.com/emqx/emqx/pull/17201) Hardened the plugin install endpoint against path traversal in uploaded tarballs and tightened the install allowlist.

  - The install path now refuses to extract any tarball whose entries would resolve outside the plugin install directory.
  - `emqx ctl plugins allow <name-vsn>` entries now expire 5 minutes after they are issued, and can be pinned to a SHA-256 hash of the package via `emqx ctl plugins allow <name-vsn> sha256:<HEX>`. Uploads whose contents do not match the pinned hash are rejected with `403 Forbidden`. When the optional `sha256:` argument is omitted, the previous behavior of accepting any payload named `<name-vsn>.tar.gz` is preserved.
  - A successful install via the HTTP plugin install endpoint (and the Dashboard upload that wraps it) immediately revokes the allow entry cluster-wide, preventing the same grant from being reused for a different tarball.

- [#17252](https://github.com/emqx/emqx/pull/17252) Published `.sha256` checksum sidecars alongside plugin packages on the official download site, allowing users to verify the integrity of downloaded plugin archives.

- [#17271](https://github.com/emqx/emqx/pull/17271) Hardened the official EMQX Docker image to clear image-scanner findings:

  - Applied Debian security upgrades during the runtime image build, so the image picks up the latest patched `libssl3t64`.
  - Removed the unused `libgnutls30t64` package. EMQX talks TLS via OpenSSL through Erlang/OTP and never links GnuTLS, so it was only present as a transitive dependency of `curl` and showed up in scanner reports.
  - Replaced the Debian `curl` package with a statically-linked `curl` binary from [stunnel/static-curl](https://github.com/stunnel/static-curl) (OpenSSL, HTTP/2, HTTP/3; no RTMP, no GnuTLS). The Debian package would have transitively re-introduced `libgnutls30t64` via `librtmp1`; the static binary avoids this while keeping container health checks that call `curl` working unchanged.

- [#17309](https://github.com/emqx/emqx/pull/17309) Sanitized PROXY-Protocol v2 SSL Common Name and Subject fields to prevent control characters from being smuggled into client identity.

  When a listener is configured with `proxy_protocol = true`, the broker now rejects connections whose PROXY-Protocol SSL TLV bytes contain ASCII control characters (the same byte class already rejected for MQTT-ingested `clientid`, `username`, and `password`). This blocks attacker-controlled bytes from reaching outbound HTTP authentication, authorization, or rule-engine header values via `${cert_common_name}` and `${cert_subject}` templates.

  The HTTP authentication and authorization clients also now refuse to send a request when a rendered header name or value contains a CR, LF, or NUL byte.

- [#17315](https://github.com/emqx/emqx/pull/17315) Extended the byte-class check applied to MQTT clientid / username / password to other fields that feed `ClientInfo` and HTTP request templating:

  - `peersni` (TLS Server Name Indication; also accepted from the PROXY-Protocol v2 `authority` TLV) is now validated at the connection ingestion boundary. Control characters cause the connection to be rejected and a warning logged.
  - Client attribute values produced by `mqtt.client_attrs_init` Variform expressions are dropped (with a warning) when they contain control characters, so templates such as `${client_attrs.tns}` cannot carry injected bytes downstream.
  - HTTP action / bridge connector header rendering now drops any header whose rendered name or value contains NUL, CR, or LF.

- [#17440](https://github.com/emqx/emqx/pull/17440) Restricted `GET /api/v5/data/files/<filename>` (backup file download) to the global Dashboard administrator. Backup archives can contain Dashboard accounts (including password hashes and MFA/TOTP state) and API key records, so API key callers, Dashboard viewers, and namespaced administrators are no longer permitted to download them. Listing the backup directory (`GET /api/v5/data/files`) remains available to all roles that previously had access.

- [#17491](https://github.com/emqx/emqx/pull/17491) Fixed passwords and secrets being exposed in gateway authentication APIs, error paths, and debug logs. Gateway authentication API responses now redact secrets while preserving the raw configuration structure. The following log paths no longer print raw passwords or secrets: gateway authentication failures, listener start errors, ExProto authentication logs, CoAP token-required logs, and LwM2M invalid-register logs.

- [#17501](https://github.com/emqx/emqx/pull/17501) Blocked namespaced Dashboard users from reading MQTT message content across namespace boundaries.

  - The following endpoints now return `403 FORBIDDEN` for any non-global caller, because they can expose MQTT payloads outside the caller's namespace. Previously, a namespaced user could read or delete messages produced by other namespaces.

    - `GET /clients/:clientid/mqueue_messages`
    - `GET /clients/:clientid/inflight_messages`
    - `GET|DELETE /mqtt/retainer/messages`
    - `GET|DELETE /mqtt/retainer/message/:topic`
    - `GET /mqtt/delayed/messages`
    - `GET|DELETE /mqtt/delayed/messages/:node/:msgid`
    - `DELETE /mqtt/delayed/messages/:topic`

  - Trace APIs are now namespace-scoped: `GET /trace` lists only traces created by the caller's namespace. The per-trace endpoints (`/trace/:name`, `/trace/:name/download`, `/trace/:name/log`, `/trace/:name/log_detail`, `/trace/:name/stop`) return `404` when the trace belongs to a different namespace, preventing callers from discovering that other-namespace traces exist. The bulk `DELETE /trace` is reserved for the global administrator; namespaced callers receive `403`. Namespaced administrators retain full access to their own traces, including creating, listing, downloading, streaming, stopping, and deleting them.


#### Clustering

- [#17076](https://github.com/emqx/emqx/pull/17076) Introduced a new routing table synchronization mechanism. The routing table schema version has been stepped to `v3`, with backward compatibility for `v2` provided.

  With schema v3, each node (core or replicant) takes full ownership of the routing table entries pointing towards it, giving peer nodes only read-only access to these entries. This improves partition tolerance of the EMQX cluster, as peer nodes in a partitioned cluster cannot change the routing table on behalf of other nodes. It also improves `SUBACK` latency on replicant nodes.

  **Backward compatibility:** When a node supporting v3 joins a cluster of nodes that only support v2, it keeps using v2 for compatibility. To switch the cluster to v3, perform a full cluster restart after upgrade. To prevent the automatic switch, set `broker.routing.storage_schema` to `v2`.

  **Downgrade note:** After the cluster switches to v3, rolling downgrade is not possible.

  To check the current routing schema version on a node:

  ```
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17152](https://github.com/emqx/emqx/pull/17152) Added support for configuring Erlang inet port options for the distribution port, with a default `buffer` size of 1 MB.

  Previously, the Erlang distribution port used an extremely small default port buffer (1460 bytes, or ~9 KB on some platforms), which caused performance bottlenecks even when the distribution port buffer (`+zdbbl`) was configured to a much larger value (e.g., 32 MB). This affected cluster communication reliability and could manifest as `erpc timeout` errors, Mnesia transaction congestions, and degraded multi-core node support.

#### Observability

- [#16911](https://github.com/emqx/emqx/pull/16911) Reduced the overhead of Prometheus metrics collection by avoiding accidental repeated queries of Mria statistics.

- [#16916](https://github.com/emqx/emqx/pull/16916) Now, the `emqx_cert_expiry_at` Prometheus metric takes into account the expiry date of certificates that belong to managed certificate bundles, when they are used in MQTT listeners.

- [#16958](https://github.com/emqx/emqx/pull/16958) Added focused `/api-spec` endpoints and a Dashboard API spec explorer page for easier browsing of EMQX HTTP API documentation.

  The Dashboard now serves tag-scoped and drill-down OpenAPI slices, and these endpoints are disabled together with Swagger when `dashboard.swagger_support` is set to `false`. Added `emqx ctl api_keys` CLI commands to list, show, add, delete, enable, and disable API keys from the command line.

- [#17018](https://github.com/emqx/emqx/pull/17018) Reduced the number of calls to other nodes performed when calling the Prometheus scraping API endpoint. This makes the API call return faster and reduces the chance of it timing out when the cluster is under strain.

  Specifically, `emqx_mria_lag` metric that is of interest to replicant nodes is now refreshed periodically (every 10 seconds by default) instead of refreshed on demand for each API call.

- [#17162](https://github.com/emqx/emqx/pull/17162) Exposed per-node license info via Prometheus gauges (`emqx_license_max_sessions`, `emqx_license_expiry_at`, `emqx_license_issued_at`) so cluster-wide license consistency can be alerted on without per-node CLI checks.

  Operators can now alert on license inconsistencies across cluster nodes by comparing these gauges. The implementation fetches all three values from a single `emqx_license_checker:dump/0` gen_server call, eliminating a redundant round-trip on every Prometheus scrape.

- [#17176](https://github.com/emqx/emqx/pull/17176) Added `emqx_routes_count` and `emqx_routes_max` Prometheus metrics to export the number of route table entries per node.

- [#17329](https://github.com/emqx/emqx/pull/17329) Added two node-wide gauge metrics to the `/api/v5/prometheus/stats` endpoint:

  - `emqx_vm_uptime_ms` reports the EMQX node uptime in milliseconds.
  - `emqx_vm_max_fds` reports the maximum number of file descriptors available to the node.

- [#17031](https://github.com/emqx/emqx/pull/17031) Added session high-watermark history for license usage auditing.

  EMQX now records the daily peak session count and retains at least 24 months of history. Operators can query this data via `emqx ctl license history` with optional `--period daily|monthly` and `--json` flags. A new `license.high_watermark_timezone` config controls the day boundary for bucketing.

#### Access Control

- [#16849](https://github.com/emqx/emqx/pull/16849) Added cookie-based authentication fallback for plugin API endpoints.

  Plugin UI iframes served by the Dashboard can now authenticate via the `emqx_auth` cookie when no `Authorization` header is present. This only applies to `/api/v5/plugin_api/...` paths.

- [#16942](https://github.com/emqx/emqx/pull/16942) [#17235](https://github.com/emqx/emqx/pull/17235) Introduced fine-grained scope-based access control for both API keys and Dashboard login users.

  API keys can now be restricted to specific API path categories using scopes derived from OpenAPI tags. Keys without scopes retain full access (backward compatible). An empty scopes list denies all scoped API paths. The `publisher` API-key role is now constrained to `[publish]` only.

  Dashboard login users now also carry an optional `scopes` field; when set, requests are authorized against the same path-to-scope catalog used for API keys, layered on top of the existing role-based check. Four new scopes (`user_management`, `mfa_management`, `sso_management`, `api_key_management`) cover Dashboard-only endpoints and are admin-only except `mfa_management`, which any role may hold for self-exemption from forced MFA. API keys cannot hold any of the four login-only scopes. Both checks apply to the HTTP API and to bootstrap-file loading (incompatible scopes are dropped with a warning).

  New public catalog endpoints expose the scope vocabulary for UI consumption: `GET /api_key_scopes` and `GET /user_scopes`, both accessible to any bearer-authenticated caller. The `scopes` field is also surfaced in `GET /users`, `POST /users`, and `PUT /users/:username` responses; when not explicitly set, the response projects the role-default scope list.

  Additional behavior changes that follow from the new scope model:

  - The `dashboard.default_username` user is protected as a break-glass account. It cannot be deleted, demoted from administrator, or have its `scopes` field set; only its `description` may be changed. This guarantees an operator always retains administrative access if other administrators lose or misconfigure their scopes.
  - Self-service on a user's own record now respects scopes. Only the dedicated change-password and MFA self endpoints still bypass scope checks; other operations such as `PUT /users/:self` are subject to the user's scopes.
  - `PUT /users/:username` and `PUT /api_key/:name` validate role changes against the effective persisted scopes when the request body omits the `scopes` field. Demoting a user or changing an API key role is rejected if the persisted scopes are incompatible with the new role.
  - API key bootstrap files accept an optional fourth column for scopes (`key:secret:role:scopes`). Unknown or role-incompatible scope names are dropped with a warning rather than rejecting the whole file, so existing three-column bootstrap files remain loadable.
  - The SAML SP metadata endpoint (`GET /sso/saml/metadata`) is now reachable without authentication, matching `/sso/saml/acs`.

- [#16943](https://github.com/emqx/emqx/pull/16943) Added per-backend `force_mfa` option for SSO (OIDC/SAML/LDAP).

  When enabled, SSO users must complete TOTP MFA setup or verification before receiving a Dashboard token, regardless of IDP-side MFA settings. Supports three MFA states: `not_configured` (force setup), `enabled` (require verification), and `admin_disabled` (skip MFA). New API endpoints `POST /sso/mfa/setup` and `POST /sso/mfa/verify` handle the MFA flow.

  Existing users can be exempted or required individually by an administrator via DELETE/POST on `/users/:username/mfa`, and that decision overrides the live backend policy until the administrator changes it. SSO users on a `force_mfa = true` backend who disable their own MFA are required to set MFA up again on the next login; only an administrator-initiated disable exempts a user from the live policy.

- [#17178](https://github.com/emqx/emqx/pull/17178) The `emqx ctl api_keys add` CLI command now accepts a `--scopes <scope1,scope2,...>` option, matching the scope-based permission control already supported by the REST API.

- [#17218](https://github.com/emqx/emqx/pull/17218) Added an ACME client plugin (`emqx_acme`) that issues and renews TLS certificates from any RFC 8555 ACME CA (e.g. Let's Encrypt) into an EMQX managed certificate bundle, and rewrites the configured SSL/WSS and/or Dashboard HTTPS listeners to consume that bundle.

#### Multi-Tenancy

- [#17053](https://github.com/emqx/emqx/pull/17053) Added a new multi-tenancy configuration option `multi_tenancy.post_auth_tns_expression`.

  When configured, it is a [Variform](https://docs.emqx.com/en/emqx/latest/configuration/configuration.html#variform-expressions) expression evaluated after the authentication chain completes. Its rendered value is written into `client_attrs.tns`, the tenant namespace key used by multi-tenancy quota and routing decisions.

  This allows operators derive the tenant namespace from authentication-response attributes (for example, a `tag` field returned by an HTTP auth backend) instead of relying only on pre-authentication `mqtt.client_attrs_init`. Example expressions: `client_attrs.tag`, or with a fallback `coalesce(client_attrs.tag, username)`.

  When the expression is empty (default), behavior is unchanged.

- [#17078](https://github.com/emqx/emqx/pull/17078) Inlined each managed namespace's configuration (session and limiter) in the response of `GET /api/v5/mt/managed_ns_list_details`, so management UIs can render a list of namespaces with their configuration in a single request instead of one additional call per namespace.

#### Gateway

- [#17013](https://github.com/emqx/emqx/pull/17013) Added GBT32960-2025 protocol support to the GBT32960 gateway.

  The gateway now automatically detects the protocol version by frame header (`##` for 2016, `$$` for 2025) and handles version-specific parsing and serialization, including:

  - New 2025 info types: Vehicle, DriveMotor, FuelCell, Engine, Location, Alarm, PowerBatteryVoltage/Temp, FuelCellStack, SuperCapacitor, SuperCapacitorExtreme, and digital Signature.
  - New command: Activation (0x09/0x0A).
  - Version-aware parameter sizes for parameter query/setting (0x02/0x03: BYTE in 2025 vs WORD in 2016).
  - 2025 vehicle login with BMS battery pack encoding fields.

#### Data Integration

- [#16929](https://github.com/emqx/emqx/pull/16929) Two new limiter types are introduced: `delivery_messages` and `delivery_bytes`. In contrast to the existing `messages` and `bytes` limiters, which limit messages *published by a single client*, the new limiter throttle messages *received by a single client from any source*. If the limit is hit, QoS 0 messages are dropped, QoS > 0 are queued internally, and a retry is scheduled. The retry time is derived from the limiter's configuration.

  The new limiters are only supported for memory sessions (`durable_sessions.enable = false`).

  If unspecified, the default values are unlimited, thus keeping backward compatibility.

- [#16962](https://github.com/emqx/emqx/pull/16962) Improved Kafka source polling behavior by ensuring fetch requests wait briefly for data instead of returning empty batches immediately when no records are available. This reduces unnecessary polling delays and helps Kafka consumers receive new records more consistently.

- [#17011](https://github.com/emqx/emqx/pull/17011) Added `ts_column` and `ttl` configuration fields to the EMQX Tables (Rust NIF driver) connector.

  - `ts_column`: Specifies a custom timestamp column name for auto-created tables (defaults to `ts` if not set).
  - `ttl`: Sets the time-to-live hint for auto-created tables (e.g., `3 days`).

  These fields were already supported by the underlying `greptimedb-ingester-erlnif` driver (since 0.1.8) and are now exposed in the EMQX Tables connector configuration.

- [#17025](https://github.com/emqx/emqx/pull/17025) The way the InfluxDB database performs health checks and credential verification has been changed.

  It no longer performs checks by executing `SHOW DATABASES`, which could be falsely flagged as a system penetration by some auditing systems.

  See also [emqx/influxdb-client-erl#54](https://github.com/emqx/influxdb-client-erl/pull/54).

- [#17046](https://github.com/emqx/emqx/pull/17046) Added a new metric `actions.messages` (and the corresponding `actions_messages_rate` in the Dashboard monitor API) that counts the total number of messages handled by rule-engine action executions.

  Because a single action execution may handle a batch of messages, `actions.messages` is greater than or equal to `actions.executed`, and `actions_messages_rate` reflects the true per-message throughput of actions.

- [#17089](https://github.com/emqx/emqx/pull/17089) MQTT ingress bridges now support consuming from remote message queues exposed as `$queue/{name}/{bind-filter}` when the remote broker supports MQTT 5 Subscription Identifiers. Queue subscriptions are rejected when Subscription Identifiers are unavailable, and regular topic subscriptions automatically retry without Subscription Identifiers if the remote broker does not accept them.

- [#17104](https://github.com/emqx/emqx/pull/17104) Added date-part placeholders to blob name templates in aggregated upload actions (Azure Blob Storage, Amazon S3, GCS, Snowflake, S3 Tables). Placeholders are rendered against the aggregation start time and default to UTC. This enables Hive-partitioned object layouts (e.g. `year=2025/month=04/day=22/hour=07/...`) directly consumable by Spark, Databricks, and Synapse.

  Supported placeholders:

  - `${datetime.YYYY}`
  - `${datetime.MM}`
  - `${datetime.DD}`
  - `${datetime.hh}`
  - `${datetime.mm}`
  - `${datetime.ss}`
  - `${datetime.DOY}` (day of year)

  Each placeholder can be prefixed with an explicit timezone:

  - `utc` (default): e.g. `${datetime.utc.YYYY}`
  - `local` (EMQX node's system timezone): e.g. `${datetime.local.YYYY}`

- [#17120](https://github.com/emqx/emqx/pull/17120) Added a new query string filter option to `GET /clients_v2`: `node`. When specified, online clients connected to the supplied node name will be returned, as well as disconnected clients last connected to them.

- [#17136](https://github.com/emqx/emqx/pull/17136) Added the `ping_with_auth` option for InfluxDB connectors. When enabled, health checks include the configured credentials for InfluxDB-compatible services that require authenticated health check requests. Also fixed the InfluxDB connector/action to preserve Unicode text when writing values from `write_syntax` literals or MQTT payloads.

- [#17165](https://github.com/emqx/emqx/pull/17165) Added the `resource_opts.dispatch_strategy` option for actions.

  The new option defaults to `per_clientid`, preserving the previous buffer worker dispatch behavior. Setting it to `random` makes queries without an explicit `pick_key` use a random dispatch key, which helps spread traffic across multiple buffer workers when a small number of clients publish a large amount of messages.

- [#17170](https://github.com/emqx/emqx/pull/17170) [#17282](https://github.com/emqx/emqx/pull/17282) [#17297](https://github.com/emqx/emqx/pull/17297) Added `tcp_opts` (`nodelay`, `sndbuf`, `recbuf`, `buffer`, `keepalive`, `delay_send`, `active_n`) to the MQTT bridge connector and Cluster Link configurations, so the outbound MQTT client TCP socket can be tuned per connection. Unset fields keep the operating system / `gen_tcp` defaults. `delay_send` (off by default) coalesces small writes for better throughput at the cost of a small latency increase.

- [#17245](https://github.com/emqx/emqx/pull/17245) Added Chinese and English translations for the MQTT Disk-Queue bridge plugin's configuration UI in the Dashboard.

#### Cluster Linking

- [#17221](https://github.com/emqx/emqx/pull/17221) Improved Cluster Linking diagnostics for MQTT message forwarding.

  When message forwarding connections experience connectivity issues, the link resource status and respective alarms now include the disconnect reason, making configuration problems easier to identify.

#### Deployment

- [#17079](https://github.com/emqx/emqx/pull/17079) Added `service.wsEnabled` option to the Helm chart to suppress the ws/wss Service port entries when MQTT WebSocket listeners are disabled. Defaults to `true` to preserve existing behavior.

### Bug Fixes

#### Core MQTT Functionalities

- [#16779](https://github.com/emqx/emqx/pull/16779) Improved handling of malformed first packets by classifying them as invalid CONNECT packets and adding better protocol hints in logs.

- [#16781](https://github.com/emqx/emqx/pull/16781) Fixed CONNECT validation when retained messages are unavailable.

  When `mqtt.retain_available` is set to `false`, CONNECT packets with Will Retain set are now correctly rejected with CONNACK reason `Retain not supported (0x9A)`.

- [#16783](https://github.com/emqx/emqx/pull/16783) Fixed MQTT v5 SUBSCRIBE validation for `Subscription-Identifier` upper bound.

  EMQX now accepts `268435455` (0x0FFFFFFF), which is the maximum valid Subscription Identifier value defined by the MQTT spec.

- [#16847](https://github.com/emqx/emqx/pull/16847) Fixed a crash when non-ASCII unicode string is used in message transformation expression.

- [#16874](https://github.com/emqx/emqx/pull/16874) Fixed a rare issue where Durable Storage backed by DS Raft could stop accepting new messages after a sequence of quick cluster leadership changes, requiring a node restart to recover.

- [#16876](https://github.com/emqx/emqx/pull/16876) Changed log message `msg_publish_not_allowed` to `msg_not_routed_to_subscribers`.

- [#16974](https://github.com/emqx/emqx/pull/16974) In EMQX 6.1.1, when a session was subscribed to a topic filter containing retained messages and was later taken over or resumed without re-subscribing to the same topic filter, it would receive again the received messages. Now, the previous behavior is restored, meaning that, upon session resumption or takeover without explicit re-subscription, retained message iteration will cease.

- [#17139](https://github.com/emqx/emqx/pull/17139) Restored `retainer.enable` as a real runtime switch for the retainer subsystem.

  This allows deployments to keep MQTT retained-message protocol support enabled while disabling retained-message storage, instead of relying on `mqtt.retain_available`, which can reject retained publishes at the protocol layer.

- [#17172](https://github.com/emqx/emqx/pull/17172) Fixed an issue where MQTT packets (such as PUBACK) sent by a client right before disconnecting could be lost when the connection process had pending outbound messages in its mailbox. Now the connection process correctly drains its mailbox before shutting down, ensuring that inbound packets are processed even after the socket is closed.

- [#17175](https://github.com/emqx/emqx/pull/17175) Fixed an issue where messages delivered from Streams did not apply subscription options such as Subscription Identifier from the stream subscription.

- [#17353](https://github.com/emqx/emqx/pull/17353) Fixed an issue in the `socket` TCP backend where outbound MQTT packets could be sent in the wrong order when a client connection experienced repeated send congestion. This scenario was practically very unlikely to occur.

- [#17383](https://github.com/emqx/emqx/pull/17383) After a session takeover, the channel info reflected by the Dashboard and REST API (`mqueue_len`, `inflight_cnt`) now updates immediately after the takeover replay completes, rather than waiting for the next 15-second stats refresh tick.

#### Rule Engine

- [#16699](https://github.com/emqx/emqx/pull/16699) Previously, under certain race conditions, long and cryptic logs like the following could be printed:

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  Now, EMQX prints more meaningful information to help debug the issue.

- [#16780](https://github.com/emqx/emqx/pull/16780) Fixed an issue in authorization source validation where requests missing the `type` field could trigger an internal error.

  Now EMQX returns a clear `BAD_REQUEST` validation error for this case.

- [#16796](https://github.com/emqx/emqx/pull/16796) Fixed handling of multiline SQL statements in connector actions.

- [#17211](https://github.com/emqx/emqx/pull/17211) Added the `connected_at` field to the `$events/client/connack` Rule Event, which was stated in the documentation but missing from the actual data.

#### Data Integration

- [#16936](https://github.com/emqx/emqx/pull/16936) Fixed an issue where the health check of an Azure Blob Storage Action in aggregate mode could timeout if the container contained too many blobs.

- [#16955](https://github.com/emqx/emqx/pull/16955) Eliminated Kafka producer action false health check warning logs.

  Previously if Kafka producer is idling for too long, Kafka may close the connection (typically default is 10 minutes), if Kafka producer action health-checks happen to be performed around the same moment, there could be a false warning message with message "not_all_kafka_partitions_connected".

- [#16972](https://github.com/emqx/emqx/pull/16972) HTTP and GCP PubSub Actions were patched to treat transient connection errors with reason `closing` as recoverable errors, reducing log noise.

- [#17001](https://github.com/emqx/emqx/pull/17001) Fixed an issue where MQTT source failed to receive messages from `$queue/` subscriptions when the remote broker has the Message Queue (mq) feature enabled.

  The root cause was that the MQ message delivery did not include the MQTT v5 Subscription-Identifier property in PUBLISH packets, which the MQTT bridge ingress relies on to route messages from queue subscriptions.

- [#17068](https://github.com/emqx/emqx/pull/17068) Fixed EMQX Tables TLS connector startup when `ssl.verify` is `verify_none` and cert file paths are left empty, and aligned Rust NIF TLS verify propagation with connector config.

- [#17084](https://github.com/emqx/emqx/pull/17084) Fixed an issue with MQTT Sources in which, if its Connector used `clean_start = false` and reconnected to a broker with a session containing messages, those messages would not trigger rule actions.

- [#17111](https://github.com/emqx/emqx/pull/17111) Fixed query execution for PostgreSQL connectors in disable prepared statements mode. Previously, concurrent queries could interleave and produce errors.

- [#17113](https://github.com/emqx/emqx/pull/17113) Fixed RocketMQ connector isolation: a misconfigured or unreachable RocketMQ connector no longer destabilizes other RocketMQ connectors on the same node. Previously, one connector with an unreachable broker could stall the shared client supervisor for up to 60 seconds, causing sibling connectors to flap with `resource_health_check_timed_out` and for Dashboard operations on them to hang.

  The default TCP/TLS connect timeout is also lowered from 60 seconds to 10 seconds so a misconfigured server surfaces as failed quickly instead of appearing stuck.

- [#17180](https://github.com/emqx/emqx/pull/17180) Fixed an issue where, under heavy load, a timed out call to a MongoDB process would be interpreted as an unrecoverable error and wouldn't be retried. Now, the message will be retried on such events.

- [#17216](https://github.com/emqx/emqx/pull/17216) Fixed Timescale/PostgreSQL actions to report a structured bad parameter error instead of crashing the database connection process when a quoted JSON numeric string is mapped to a `FLOAT` column.

- [#17250](https://github.com/emqx/emqx/pull/17250) Fixed Redis Sentinel connectors to support separate authentication settings for Redis data nodes and Sentinel nodes.

- [#17293](https://github.com/emqx/emqx/pull/17293) Fixed an issue where, when writing a Parquet file with an object containing a required key but with an `undefined`/`null` value, a corrupt file would be written instead of raising an error.

- [#17303](https://github.com/emqx/emqx/pull/17303) Upgraded Kafka client libraries: `brod` from 4.5.2 to 4.5.4 and `wolff` from 4.1.9 to 4.1.10.

  Notable fixes picked up from upstream:

  - `brod`: fix a race condition during Kafka connection re-authentication (via `kafka_protocol` 4.3.4).
  - `wolff`: under high-memory load control (`drop_if_highmem`), keep a minimum buffer reserve so the producer is not starved of in-flight data; only bytes exceeding the reserve are dropped.

- [#17343](https://github.com/emqx/emqx/pull/17343) Fixed a clustered-config replication bug where importing a data backup (or loading a HOCON config via `emqx ctl conf load` / `PUT /api/v5/configs`) that contained a `file`-type authorization source could leave peer nodes lagging with a `cluster_rpc_apply_failed` / `failed_to_read_acl_file` error.

  The importer used to write the ACL file locally and replace inline `rules` with a `path`, then ship the path-form config across the cluster. Peer nodes have no such file on disk and so could not apply the change. The config sent to the cluster now keeps `rules` inline, so each peer writes its own copy of the ACL file from the replicated content.

- [#17347](https://github.com/emqx/emqx/pull/17347) Upgraded the RocketMQ client dependency to `v0.7.2` to fix memory growth in async producer requests.

- [#17439](https://github.com/emqx/emqx/pull/17439) Fixed an issue where the health check of an Azure Blob Storage Connector could timeout, or generate large bandwidth costs, if the storage account contained too many containers. Companion fix to #16935.

- [#17450](https://github.com/emqx/emqx/pull/17450) Fixed an issue where the `/prometheus/data_integration` Prometheus endpoint could respond with a 500 status when using `mode=node`. This issue would only arise when the configuration for Actions and Connectors was manually edited and inconsistent, having an Action whose Connector does not exist.

#### Clustering

- [#17132](https://github.com/emqx/emqx/pull/17132) Fixed an issue where adding or removing topic metrics could fail on a replicate node when its raw config or runtime state had drifted, raising a `cluster_rpc_apply_failed` alarm and stalling cluster RPC replication. Duplicate-add and missing-remove are now rejected on the initiator only, while replicates apply the change idempotently.

- [#17182](https://github.com/emqx/emqx/pull/17182) Bumped emqx-OTP to 27.3.4.2-8 for mria.

  Without this change, during EMQX startup, Mria app boot may get stuck if it's not connected to the cluster.

- [#17214](https://github.com/emqx/emqx/pull/17214) Removed cryptic error-level logging of disconnect events from Cluster Link message forwarding MQTT clients, in favor of more user-friendly messages with enough context for troubleshooting. Events similar to this one should no longer appear in the error logs:

  ```
  2026-05-06T03:00:48.738654+00:00 [error] [PoolWorker] unexpected info: {disconnected,141,#{}}
  ```

- [#17218](https://github.com/emqx/emqx/pull/17218) Avoid `bin/emqx` and `bin/emqx_ctl` invocations from triggering `nodeup`/`nodedown` events on the running broker, which previously surfaced as misleading `cm_registry_node_down` warnings in the broker log. The temporary helper nodes started by these scripts now register as hidden Erlang nodes, as intended.

- [#17269](https://github.com/emqx/emqx/pull/17269) Improved cluster recovery after a network partition.

  - Previously, part of the clients connected to the replicant nodes could be lost from the global registry. This could lead to inconsistent behavior during takeover and incorrect information displayed in the Dashboard.

    This fix adds a background process that re-registers the existing clients when network partition is healed. It also adds a new alarm: "Broker is recovering after a network partition", which is raised while the global registry is being rebuilt.

  - Introduced a new cluster auto-heal algorithm that can automatically recover overlapping network partitions.

- [#17342](https://github.com/emqx/emqx/pull/17342) Fixed cluster configuration import failing with a "required_field: node.cookie" schema check error when the exported `cluster.hocon` contained a partial `node` section. Read-only roots (`node`, `rpc`) are not part of the data import anyway, so they are now dropped from the imported config before the pre-flight schema check, letting the running node's own values be used for the validation.

- [#17348](https://github.com/emqx/emqx/pull/17348) Fixed noisy and misleading `emqx ctl conf cluster_sync status` diagnostics when clustered nodes have the same effective checked configuration but different raw configuration representations.

  The command now suppresses raw-only representation differences that do not correspond to checked configuration changes, while still warning when checked configuration is inconsistent. It also avoids crashing when a raw configuration key exists on one node but is missing from another node.

  It also ignores timestamp-only metadata differences in `created_at` and `last_modified_at` for actions, sources, bridges, and rule metadata. Data import or boot-time configuration loading can refresh these generated timestamps on only some nodes even when the effective runtime configuration is otherwise identical.

- [#17349](https://github.com/emqx/emqx/pull/17349) Improved responsiveness of a Cluster Link in situations when route replication was stuck connecting to an unresponsive target cluster. Now, deleting such Cluster Link should finish slightly sooner.

- [#17382](https://github.com/emqx/emqx/pull/17382) Fixed corruption of global channel registry that may occur when cluster experiences a network partition.

- [#17424](https://github.com/emqx/emqx/pull/17424) Fixed a global session registry leak that could leave duplicate or stale entries for the same client ID after a network partition followed by Mnesia autoheal.

  Discard and takeover-kick RPC handlers now also remove the registry row when the target process is no longer alive, and the registration throttle on the connect path now recognizes tombstone rows (no local channel state) and reaps them instead of blocking new connections for the same client ID indefinitely.

- [#17432](https://github.com/emqx/emqx/pull/17432) Fixed an issue where concurrent Cluster Link API requests could return generic error responses, instead of returning either success or not found.

- [#17469](https://github.com/emqx/emqx/pull/17469) Fixed the issue where warnings similar to those below are emitted when enabling or disabling an active Cluster Link.

  ```
  [warning] tag: RESOURCE, msg: handle_resource_metrics_failed, reason: {badkey, matched}, event: matched, ...
  ```

#### Access Control

- [#16805](https://github.com/emqx/emqx/pull/16805) Added support for authz hook results to opt out of authorization cache storage for dynamic ACL decisions.

- [#17045](https://github.com/emqx/emqx/pull/17045) Fixed password-based authentication backends to let the auth chain continue when the CONNECT packet has no password, instead of rejecting the connection immediately.

  Previously, if a client connected without a password, the first password-based authenticator (built-in database, MySQL, PostgreSQL, MongoDB, Redis, or LDAP) in the chain would return an error, blocking any subsequent authenticators from being tried.

- [#17064](https://github.com/emqx/emqx/pull/17064) Closed an authorization gap in the `/authentication/:id/users` REST endpoint so that a namespaced administrator can no longer list or create users in the global (or another tenant's) namespace by omitting the `ns` query parameter or the `namespace` body field. Authentication users in a non-global namespace can no longer be marked as `is_superuser`; requests to create or update such a user are rejected so that explicit ACL rules are always enforced for tenant MQTT clients.

- [#17100](https://github.com/emqx/emqx/pull/17100) Fixed OIDC SSO login failing with `provider_not_ready` when the identity provider returns a JWKS response whose `Content-Type` uses the `+json` structured syntax suffix (e.g. `application/jwk-set+json; charset=utf-8`). Such responses are now accepted as valid JWKS content.

- [#17122](https://github.com/emqx/emqx/pull/17122) Fixed Dashboard RBAC checks for SSO users with URL-encoded usernames such as email addresses, so viewer self-service MFA disable requests work correctly when `force_mfa` is disabled.

- [#17140](https://github.com/emqx/emqx/pull/17140) Fixed a silent failure when EMQX fetched a Certificate Revocation List (CRL) over HTTP from a server that returns a DER-encoded body (`Content-Type: application/pkix-crl`, the format mandated by RFC 5280 §5).

  Previously, EMQX only decoded PEM-encoded CRL bodies; a DER body was silently treated as zero CRLs and cached as an empty list, causing every TLS handshake on `enable_crl_check = true` listeners to fail with `bad_crls, no_relevant_crls` and no log line indicating what went wrong.

  EMQX now decodes both PEM and DER CRL bodies. When a fetched body is neither, a warning is logged with the URL so the misconfiguration is visible.

- [#17171](https://github.com/emqx/emqx/pull/17171) Fixed an RBAC issue that prevented namespaced Dashboard administrators from enabling or disabling MFA for their own account.

  Namespaced administrators remain restricted from managing MFA settings for other Dashboard users.

- [#17177](https://github.com/emqx/emqx/pull/17177) Dashboard-created REST API keys are now generated randomly instead of being derived from the API key name.

- [#17223](https://github.com/emqx/emqx/pull/17223) Fixed missing client certificate when a TCP-passthrough proxy (e.g. GCP TCP Proxy NLB, AWS NLB) is placed in front of an SSL listener with `proxy_protocol = true`. The TLS handshake at the listener was completing successfully and the client certificate was present, but it was not exposed to authentication or rule events. Functions, ACL rules, and authentication backends that depend on the client certificate (CN, subject, full PEM) now work correctly in this deployment shape.

- [#17330](https://github.com/emqx/emqx/pull/17330) Hardened the PROXY Protocol v2 TLV parser on TCP and SSL listeners with `proxy_protocol` enabled. Previously, a TLV whose declared length overran the buffer caused the parser to silently truncate the TLV stream, dropping any trailing fields. The parser is now strict: malformed TLV streams cause the connection to be rejected with a warning log entry instead of being accepted with a partially parsed PROXY header.

- [#17428](https://github.com/emqx/emqx/pull/17428) Fixed a Dashboard OIDC SSO crash that prevented EMQX from completing the OpenID provider discovery when the provider's `.well-known/openid-configuration` response included a `Cache-Control` header such as `max-age=0` (observed with Kanidm). The crash caused the OIDC supervisor to exhaust its restart budget after a single failure, leaving SSO unable to recover without a config re-save. The cache-control parser is now tolerant of these values, the worker no longer hard-crashes on a bad expiry, and the OIDC supervisor allows several restarts within a minute so transient failures retry cleanly.

#### Gateway

- [#17141](https://github.com/emqx/emqx/pull/17141) Fixed CoAP connection-mode token takeover so reconnecting UDP/DTLS clients can resume with a valid token while invalid token/clientid combinations are rejected. Also ensured required connection info fields are present before running CoAP takeover connected hooks.

- [#17258](https://github.com/emqx/emqx/pull/17258) Fixed an issue in the MQTT-SN gateway where a connected client sending a second CONNECT packet on the same session would crash its connection process. The gateway now responds with a DISCONNECT and closes the session gracefully.

- [#17287](https://github.com/emqx/emqx/pull/17287) Fixed MQTT-SN clients crash caused by packets received in unexpected connection or Will states, including `DISCONNECT` during connection setup, `REGISTER` before the Will handshake completes, and `WILLMSGUPD` before a Will topic exists.

- [#17419](https://github.com/emqx/emqx/pull/17419) Fixed CoAP gateway observe notifications to honor the `gateway.coap.notify_type` setting.

  Observe notifications now use a per-session confirmable in-flight window of 1 and a fixed pending queue of 100 entries shared by all observe tokens. When a confirmable notification is in flight, later observe notifications are queued instead of being silently lost. When the queue is full, the oldest pending notification is dropped, `delivery.dropped.queue_full` is incremented, and a throttled warning is logged.

  Cancelling an observe relation now also removes pending notifications for that observed topic/filter and observe token, so queued notifications are not delivered after the client has cancelled the observe, including wildcard observe filters.

#### Observability

- [#16842](https://github.com/emqx/emqx/pull/16842) Reduced noisy plugin config warning logs when no peer node has the plugin config yet.

  Previously, when a node tried to fetch plugin config from peer nodes during startup, it would log a warning even when all peers simply didn't have the config (e.g., first node to load the plugin). Now this benign case is logged at debug level, and only genuine errors (RPC failures, timeouts) remain as warnings.

- [#16843](https://github.com/emqx/emqx/pull/16843) Fixed an issue where HTTP headers and query string parameters were not passed through to plugin API handlers, causing plugins to receive empty headers and missing query parameters.

- [#16863](https://github.com/emqx/emqx/pull/16863) Added a warning log when an async reply is received for an already-expired request.

- [#16868](https://github.com/emqx/emqx/pull/16868) Improved REST API authentication error messages to guide programmatic clients toward using API keys (Basic auth) instead of repeatedly logging in for bearer tokens. Error responses now mention the `api_key.bootstrap_file` configuration option and the `POST /api_key` endpoint for creating persistent API keys.

- [#16879](https://github.com/emqx/emqx/pull/16879) Added `log.audit.cache_size` as the primary config key for the audit log DB cache size, while keeping `log.audit.max_filter_size` for backward compatibility.

- [#16890](https://github.com/emqx/emqx/pull/16890) Fixed an ExHook issue where successful reconnect reloads could duplicate the same server name in the running list and trigger repeated callback dispatches.

- [#16939](https://github.com/emqx/emqx/pull/16939) Fixed the built-in database authenticator so it no longer logs a warning when the default bootstrap file path is configured but the file does not exist.

- [#16956](https://github.com/emqx/emqx/pull/16956) Log client connection termination at warning level instead of info when the reason is `emsgsize` (received packet exceeds `mqtt.max_packet_size`).

- [#17002](https://github.com/emqx/emqx/pull/17002) Updated `minirest` library to version 1.4.12. This version fixes a bug that caused EMQX API to produce malformed API responses with `204 No Content` status line, emitting invalid `content-length` header.

- [#17024](https://github.com/emqx/emqx/pull/17024) Dashboard HTTP listener now automatically uses IPv6 when the bind address is an IPv6 address, removing the need to explicitly set `inet6 = true`.

- [#17054](https://github.com/emqx/emqx/pull/17054) Fixed `GET /api/v5/configs?key=...` returning incomplete data when `Accept: application/json` was set.

  Previously, the JSON response ignored the `key` query parameter and always returned a fixed subset of root configurations, which excluded keys like `multi_tenancy`. The endpoint now honors the `key` parameter in JSON responses consistently with the hocon (text/plain) response.

- [#17118](https://github.com/emqx/emqx/pull/17118) Improved pagination on multi-tenancy list endpoints (`/mt/ns_list`, `/mt/ns_list_details`, `/mt/managed_ns_list`, `/mt/managed_ns_list_details`, `/mt/ns/{ns}/client_list`):

  - Added an RFC 8288 `Link: <?...>; rel="next"` response header. When more pages are available the header carries the query-only URI-reference of the next page; when absent, the current response is the last page. This removes the prior ambiguity where a full page (`len(results) == limit`) could not be distinguished from the exact-boundary "no more data" case without an extra request.
  - Added inclusive keyset cursor query parameters (`first_ns`, `first_clientid`) alongside the existing exclusive cursors (`last_ns`, `last_clientid`). The inclusive form supports exact-match lookup (e.g. `?first_ns=foo&limit=1`) and is preserved across paginated Link headers when the caller opts in. The two forms are mutually exclusive on a single request; supplying both returns HTTP 400.

- [#17134](https://github.com/emqx/emqx/pull/17134) Fixed `invalid json term` error returned by the banned clients listing API for client ID and username regex bans created before 6.2.0. The compiled regex retained in the database from the older release is now translated back to the original pattern string when serializing the response.

- [#17227](https://github.com/emqx/emqx/pull/17227) Cluster config file save errors now name the file and the underlying reason.

  When `cluster.hocon` (or its directory) is read-only, immutable, or otherwise unwritable (e.g. mounted read-only into a container), changing config via the Dashboard or REST API previously returned an opaque HTTP 400 with body `{config_update_crashed,{badmatch,{error,ebusy}}}` and only logged a badmatch crash that did not name the file.

  The error now:

  - Logs `failed_to_save_conf_file` with the actual file path and reason (`eacces`, `eperm`, `ebusy`, ...) plus a hint listing common operator-side causes.
  - Returns a structured HTTP 400 body that names both the file and the reason, so the cause is visible in the Dashboard without digging through node logs.

  Previously, when only the temporary file write failed (e.g. read-only directory), the API silently returned HTTP 200 even though the change was not persisted to disk. The API now correctly reports failure in this case as well.

- [#17246](https://github.com/emqx/emqx/pull/17246) Upgraded `jose` library from 1.11.10 to 1.11.12, picking up EC and EdDSA key fixes for newer OTP releases.

- [#17247](https://github.com/emqx/emqx/pull/17247) When a plugin's REST API callback crashes or runs over its timeout budget, the broker now logs the failing API method and path together with the configured timeout, so the offending call is identifiable in mixed-traffic logs. A timeout is logged as a warning (not an error) and includes a hint pointing at `plugins.api_endpoint.timeout`, the config key to raise when a plugin callback legitimately needs more time.

- [#17254](https://github.com/emqx/emqx/pull/17254) Improved memory-usage reporting inside containers. The broker now picks the most constraining memory reading among cgroup v2, cgroup v1, and the host's `/proc/meminfo` (smallest non-zero total wins, larger usage ratio breaks ties). Previously the reading could be misleading in two ways: on containers with a tight cgroup limit, the host view could indicate >70% while the cgroup limit was <10% (or the reverse); and on hosts where a cgroup is mounted with no memory limit set, the cgroup reading could collapse the reported usage ratio to ~0%. Overload-protection thresholds and the `Memory used` metric now reflect the limit that actually constrains the process.

- [#17319](https://github.com/emqx/emqx/pull/17319) `GET /api/v5/schemas/{hotconf,actions,connectors}` now returns the response with `Content-Type: application/json`. Previously the response body was valid JSON but the header was `text/plain; charset=utf-8`, which broke clients that dispatch on the response content type.

- [#17406](https://github.com/emqx/emqx/pull/17406) Now, events captured by a trace initiated by a namespaced admin are limited to the namespace of such admin, for traces of types topic, IP address, and clientid. Traces of type rule ID already had such behavior.

- [#17473](https://github.com/emqx/emqx/pull/17473) Lowered the log level of `unabled_to_stop_plugin_apps` from warning to info when the plugin's Erlang applications cannot be stopped because other running applications still depend on them. This is an expected, non-actionable condition during plugin unload and no longer raises a warning.

#### Plugin

- [#16904](https://github.com/emqx/emqx/pull/16904) Prevent enabling or starting multiple versions of the same plugin at once. When a newer version is enabled, older configured versions of that plugin are automatically disabled, and management API actions now return a clear error instead of reporting success while another version is still active.

#### Deployment

- [#16901](https://github.com/emqx/emqx/pull/16901) Fixed RPM package OpenSSL dependency for RHEL 9.6 LTS: pinned `openssl >= 3.5.1` for RHEL >= 9.7 and `openssl >= 3.0.7` for older RHEL 9 versions.
- [#17311](https://github.com/emqx/emqx/pull/17311) Fixed Docker startup when the container hostname cannot be resolved. The entrypoint now falls back to the interface IP address before auto-generating the node name, and fails with a clear error if no node host can be determined.
- [#17369](https://github.com/emqx/emqx/pull/17369) Moved the Dashboard listener defaults (`http.bind` and the placeholder HTTPS `ssl_options`) from the user-editable `etc/emqx.conf` into the shipped `etc/base.hocon`. Previously, the hardcoded `emqx.conf` block silently reverted runtime updates to the default self-signed certificate on restart. Runtime updates made through the Dashboard, the REST API, or the `emqx_acme` plugin's automatic HTTPS configuration are now correctly preserved across restarts.

- [#17504](https://github.com/emqx/emqx/pull/17504) Fixed `bin/emqx` failing to detect a running node when its command line is wider than the terminal. The process discovery call was changed from `ps -ef` to `ps -efww`, preventing long `-root <path>` arguments from being truncated and ensuring the running EMQX process is reliably matched.

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

## 6.0.3

*Release Date: 2026-06-17*

Make sure to check the breaking changes and known issues before upgrading to EMQX 6.0.3.

### Enhancements

#### Security Hardening

- [#17040](https://github.com/emqx/emqx/pull/17040) Restricted API key access to Dashboard user-account management endpoints.

  Previously, an API key with the `administrator` role could call the Dashboard user management endpoints `POST/DELETE /users/:username/mfa` and `POST /users/:username/change_pwd` via HTTP Basic authentication. This meant an API key could reset or disable another Dashboard user's MFA, or change another Dashboard user's password, bypassing the intended separation between human Dashboard sessions and machine API keys.

  These endpoints now return `401 API_KEY_NOT_ALLOW` when accessed via an API key, consistent with the existing policy that blocks API key access to `/users`, `/users/:username`, `/logout`, and `/api_key`. Dashboard users can still manage their own MFA and password from the Dashboard UI using bearer-token (JWT) sessions.

- [#17065](https://github.com/emqx/emqx/pull/17065) Added SSRF protection for rule-engine-reachable connector and bridge configurations.

  When `rule_engine.ssrf.enable` is set to `true`, EMQX applies an outbound SSRF policy to connector, bridge, and action configurations. The policy evaluates each target as follows: exact matches in `rule_engine.ssrf.deny_hosts` are rejected immediately; resolved target IPs are then checked against `rule_engine.ssrf.allow_cidrs` before `rule_engine.ssrf.deny_cidrs`. The default denied ranges cover loopback, link-local (including cloud instance-metadata endpoints), RFC 1918, ULA, unspecified, and multicast ranges. The check runs at config-update time and covers HTTP `url` fields as well as `server`, `servers`, and `bootstrap_hosts` fields across all connector families.

  The feature is disabled by default to preserve compatibility with deployments whose connectors legitimately point at internal services. Operators in multi-tenant or externally-exposed setups are encouraged to enable it together with a network-layer egress firewall.

- [#17173](https://github.com/emqx/emqx/pull/17173) Restricted API keys from exporting or importing Dashboard accounts and API keys via the data backup endpoints.

  `POST /data/export` called with an API key now silently omits the `dashboard_users` and `api_keys` mnesia table sets from the resulting archive. `POST /data/import` called with an API key now returns `403 FORBIDDEN` when the uploaded backup contains either of those table sets.

  Dashboard bearer-token (login) callers are unaffected and continue to be able to back up and restore the full database, including Dashboard users and API keys.

  This closes a privilege-escalation gap. The existing `/users` and `/api_key` endpoints already deny API keys access to Dashboard login credentials and API key records, but an API key holder could bypass those restrictions by going through the data backup endpoints instead.

- [#17187](https://github.com/emqx/emqx/pull/17187) Removed the EMQX release version (`rel_vsn`) from the unauthenticated `GET /status?format=json` response to avoid disclosing the broker version to unauthenticated callers. The version remains available via the authenticated node-info APIs.

- [#17201](https://github.com/emqx/emqx/pull/17201) Hardened the plugin install endpoint against path traversal in uploaded tarballs and tightened the install allowlist.

  - The install path now refuses to extract any tarball whose entries would resolve outside the plugin install directory.
  - `emqx ctl plugins allow <name-vsn>` entries now expire 5 minutes after they are issued, and can be pinned to a SHA-256 hash of the package via `emqx ctl plugins allow <name-vsn> sha256:<HEX>`. Uploads whose contents do not match the pinned hash are rejected with `403 Forbidden`. When the optional `sha256:` argument is omitted, the previous behavior of accepting any payload named `<name-vsn>.tar.gz` is preserved.
  - A successful install via the HTTP plugin install endpoint (and the Dashboard upload that wraps it) immediately revokes the allow entry cluster-wide, preventing the same grant from being reused for a different tarball.

- [#17309](https://github.com/emqx/emqx/pull/17309) Sanitized PROXY-Protocol v2 SSL Common Name and Subject fields to prevent control characters from being smuggled into client identity.

  When a listener is configured with `proxy_protocol = true`, the broker now rejects connections whose PROXY-Protocol SSL TLV bytes contain ASCII control characters (the same byte class already rejected for MQTT-ingested `clientid`, `username`, and `password`). This blocks attacker-controlled bytes from reaching outbound HTTP authentication, authorization, or rule-engine header values via `${cert_common_name}` and `${cert_subject}` templates.

  The HTTP authentication and authorization clients also now refuse to send a request when a rendered header name or value contains a CR, LF, or NUL byte.

- [#17315](https://github.com/emqx/emqx/pull/17315) Extended the byte-class check applied to MQTT clientid / username / password to other fields that feed `ClientInfo` and HTTP request templating:

  - `peersni` (TLS Server Name Indication; also accepted from the PROXY-Protocol v2 `authority` TLV) is now validated at the connection ingestion boundary. Control characters cause the connection to be rejected and a warning logged.
  - Client attribute values produced by `mqtt.client_attrs_init` Variform expressions are dropped (with a warning) when they contain control characters, so templates such as `${client_attrs.tns}` cannot carry injected bytes downstream.
  - HTTP action / bridge connector header rendering now drops any header whose rendered name or value contains NUL, CR, or LF.

- [#17330](https://github.com/emqx/emqx/pull/17330) Hardened the PROXY Protocol v2 TLV parser on TCP and SSL listeners with `proxy_protocol` enabled. Previously, a TLV whose declared length overran the buffer caused the parser to silently truncate the TLV stream, dropping any trailing fields. The parser is now strict: malformed TLV streams cause the connection to be rejected with a warning log entry instead of being accepted with a partially parsed PROXY header.

- [#17440](https://github.com/emqx/emqx/pull/17440) Restricted `GET /api/v5/data/files/<filename>` (backup file download) to the global Dashboard administrator. Backup archives can contain Dashboard accounts (including password hashes and MFA/TOTP state) and API key records, so API key callers, Dashboard viewers, and namespaced administrators are no longer permitted to download them. Listing the backup directory (`GET /api/v5/data/files`) remains available to all roles that previously had access.

- [#17491](https://github.com/emqx/emqx/pull/17491) Fixed passwords and secrets being exposed in gateway authentication APIs, error paths, and debug logs. Gateway authentication API responses now redact secrets while preserving the raw configuration structure. The following log paths no longer print raw passwords or secrets: gateway authentication failures, listener start errors, ExProto authentication logs, CoAP token-required logs, and LwM2M invalid-register logs.

- [#17501](https://github.com/emqx/emqx/pull/17501) Blocked namespaced Dashboard users from reading MQTT message content across namespace boundaries.

  - The following endpoints now return `403 FORBIDDEN` for any non-global caller, because they can expose MQTT payloads outside the caller's namespace. Previously, a namespaced user could read or delete messages produced by other namespaces.
    - `GET /clients/:clientid/mqueue_messages`
    - `GET /clients/:clientid/inflight_messages`
    - `GET|DELETE /mqtt/retainer/messages`
    - `GET|DELETE /mqtt/retainer/message/:topic`
    - `GET /mqtt/delayed/messages`
    - `GET|DELETE /mqtt/delayed/messages/:node/:msgid`
    - `DELETE /mqtt/delayed/messages/:topic`
  - Trace APIs are now namespace-scoped: `GET /trace` lists only traces created by the caller's namespace. The per-trace endpoints (`/trace/:name`, `/trace/:name/download`, `/trace/:name/log`, `/trace/:name/log_detail`, `/trace/:name/stop`) return `404` when the trace belongs to a different namespace, preventing callers from discovering that other-namespace traces exist. The bulk `DELETE /trace` is reserved for the global administrator; namespaced callers receive `403`. Namespaced administrators retain full access to their own traces, including creating, listing, downloading, streaming, stopping, and deleting them.

#### Clustering

- [#17076](https://github.com/emqx/emqx/pull/17076) Introduced a new routing table synchronization mechanism. The routing table schema version has been stepped to `v3`, with backward compatibility for `v2` provided.

  With schema v3, each node (core or replicant) takes full ownership of the routing table entries pointing towards it, giving peer nodes only read-only access to these entries. This improves partition tolerance of the EMQX cluster, as peer nodes in a partitioned cluster cannot change the routing table on behalf of other nodes. It also improves `SUBACK` latency on replicant nodes.

  **Backward compatibility:** When a node supporting v3 joins a cluster of nodes that only support v2, it keeps using v2 for compatibility. To switch the cluster to v3, perform a full cluster restart after upgrade. To prevent the automatic switch, set `broker.routing.storage_schema` to `v2`.

  **Downgrade note:** After the cluster switches to v3, rolling downgrade is not possible.

  To check the current routing schema version on a node:

  ```
  emqx eval 'emqx_router:get_schema_vsn()'
  ```

- [#17152](https://github.com/emqx/emqx/pull/17152), [#17181](https://github.com/emqx/emqx/pull/17181) Added support for configuring Erlang inet port options (both connect and listen) for the distribution port, with a default `buffer` size of 1 MB.

  Previously, the Erlang distribution port used an extremely small default port buffer (1460 bytes, or ~9 KB on some platforms), which caused performance bottlenecks even when the distribution port buffer (`+zdbbl`) was configured to a much larger value (e.g., 32 MB). This affected cluster communication reliability and could manifest as `erpc timeout` errors, Mnesia transaction congestion, and degraded multi-core node support.

- [#17221](https://github.com/emqx/emqx/pull/17221) Improved Cluster Linking diagnostics for MQTT message forwarding.

  When message forwarding connections experience connectivity issues, the link resource status and respective alarms now include the disconnect reason, making configuration problems easier to identify.

- [#17530](https://github.com/emqx/emqx/pull/17530) Cluster linking now requires a non-community license. Under the default community license, configured links stay inactive (no message forwarding or route replication) and the REST API rejects attempts to enable a link with a clear hint to load a non-community license. Disabling and deleting links remain available so that legacy configuration can be tidied up. After upgrading the license, links can be enabled from the Dashboard or REST API without restarting the node.

#### Observability

- [#16656](https://github.com/emqx/emqx/pull/16656) Made system monitor reports such as `busy_port` and `long_schedule` more informative by including process labels for easier troubleshooting.

- [#16744](https://github.com/emqx/emqx/pull/16744) Added support for end-to-end tracing of messages published via the HTTP API.

- [#16757](https://github.com/emqx/emqx/pull/16757) Set `os_mon` to collect only system-wide memory statistics by default, reducing per-process memory scanning overhead.

- [#16911](https://github.com/emqx/emqx/pull/16911) Reduced the overhead of Prometheus metrics collection by avoiding accidental repeated queries of Mria statistics.

- [#17018](https://github.com/emqx/emqx/pull/17018) Reduced the number of calls to other nodes performed when calling the Prometheus scraping API endpoint. This makes the API call return faster and reduces the chance of it timing out when the cluster is under strain.

  Specifically, `emqx_mria_lag` metric that is of interest to replicant nodes is now refreshed periodically (every 10 seconds by default) instead of refreshed on demand for each API call.

- [#17031](https://github.com/emqx/emqx/pull/17031) Added session high-watermark history for license usage auditing.

  EMQX now records the daily peak session count and retains at least 24 months of history. Operators can query this data via `emqx ctl license history` with optional `--period daily|monthly` and `--json` flags. A new `license.high_watermark_timezone` config controls the day boundary for bucketing.

- [#17162](https://github.com/emqx/emqx/pull/17162) Exposed per-node license info via Prometheus gauges (`emqx_license_max_sessions`, `emqx_license_expiry_at`, `emqx_license_issued_at`) so cluster-wide license consistency can be alerted on without per-node CLI checks.

  Operators can now alert on license inconsistencies across cluster nodes by comparing these gauges. The implementation fetches all three values from a single `emqx_license_checker:dump/0` gen_server call, eliminating a redundant round-trip on every Prometheus scrape.

- [#17176](https://github.com/emqx/emqx/pull/17176) Added `emqx_routes_count` and `emqx_routes_max` Prometheus metrics to export the number of route table entries per node.

- [#17329](https://github.com/emqx/emqx/pull/17329) Added two node-wide gauge metrics to the `/api/v5/prometheus/stats` endpoint:

  - `emqx_vm_uptime_ms` reports the EMQX node uptime in milliseconds.
  - `emqx_vm_max_fds` reports the maximum number of file descriptors available to the node.

- [#17558](https://github.com/emqx/emqx/pull/17558) Added two new metrics and corresponding rates to the `GET /monitor_current` HTTP API: `rules_matched` and `actions_executed`. They track the number of rules matched and the action execution rate (success + failure), respectively.

  Also fixed `actions.executed` undercounting action invocations in non-batch mode (`batch_size = 1`): the counter is now incremented once per action callback invocation, independently of the buffer-worker telemetry flush window.

#### Access Control

- [#16741](https://github.com/emqx/emqx/pull/16741) Added configuration options `idp_signs_envelopes` and `idp_signs_assertions` to SAML SSO backend to control signature verification behavior.

  Previously, SAML signature verification was not working correctly because the IdP certificate fingerprint was not being extracted from metadata and passed to esaml for verification.

  Both options default to `false` for backward compatibility with existing configurations. Users who want to enable signature verification should explicitly set these to `true` when their IdP is configured to sign SAML responses.

- [#16942](https://github.com/emqx/emqx/pull/16942), [#17235](https://github.com/emqx/emqx/pull/17235) Introduced fine-grained scope-based access control for both API keys and Dashboard login users.

  API keys now support an optional `scopes` field. When set, requests are authorized against a fixed catalog of management scopes in addition to the role check. The `publisher` API key role is constrained to the `publish` scope only.

  Dashboard login users also support `scopes`, layered on top of role-based checks. Four login-only scopes (`user_management`, `mfa_management`, `sso_management`, `api_key_management`) cover Dashboard-only endpoints. `user_management`, `sso_management`, and `api_key_management` are administrator-only; `mfa_management` may be held by any role for self-exemption from forced MFA. API keys cannot use these login-only scopes.

  New catalog endpoints `GET /api_key_scopes` and `GET /user_scopes` expose the scope vocabulary to bearer-authenticated callers. `GET /users`, `POST /users`, and `PUT /users/:username` now include `scopes` in their responses; when not explicitly set, the response shows the role-default scopes.

  Behavior changes that follow from the new scope model:

  - The `dashboard.default_username` user is now a protected break-glass account. It cannot be deleted, demoted from administrator, or assigned scopes; only its `description` may be changed. The existing last-administrator check still applies to other administrators.
  - Self-service updates now respect scopes, except for the dedicated change-password and MFA self endpoints. For example, a viewer without `user_management` can still change their own password and manage their own MFA, but cannot edit other profile fields.
  - `PUT /users/:username` and `PUT /api_key/:name` validate role changes against persisted scopes when the request omits `scopes`; incompatible demotions or role changes are rejected.
  - API key bootstrap files accept an optional scopes column (`key:secret:role:scopes`). Unknown or role-incompatible scopes are dropped with a warning, so existing three-column files remain loadable.
  - The SAML SP metadata endpoint (`GET /sso/saml/metadata`) is now reachable without authentication, matching `/sso/saml/acs`.

- [#16943](https://github.com/emqx/emqx/pull/16943), [#17361](https://github.com/emqx/emqx/pull/17361) Added per-backend `force_mfa` enforcement for SSO (LDAP, OIDC, and SAML).

  When enabled, SSO users must complete TOTP MFA setup or verification before receiving a Dashboard token, regardless of IDP-side MFA settings. New API endpoints `POST /sso/mfa/setup` and `POST /sso/mfa/verify` handle the MFA flow.

  Administrators can exempt or require existing users individually via `POST` / `DELETE` on `/users/:username/mfa`, and that decision overrides the live backend policy until the administrator changes it. SSO users on a `force_mfa = true` backend who disable their own MFA must set it up again on the next login; only an administrator-initiated disable exempts a user from the live policy.

- [#17178](https://github.com/emqx/emqx/pull/17178) The `emqx ctl api_keys add` CLI command now accepts a `--scopes <scope1,scope2,...>` option, matching the scope-based permission control already supported by the REST API.

#### Gateway

- [#16736](https://github.com/emqx/emqx/pull/16736) Improved the JT/T 808 gateway with protocol updates, encoding support, and message handling fixes.

  - Added JT/T 808 protocol 2019 support.
  - Added the `jt808.frame.parse_unknown_message` option to transparently forward unknown messages.
  - Added GBK string encoding support through the new `frame.string_encoding` option. The default `utf8` mode keeps the existing pass-through behavior, while `gbk` converts GBK-encoded strings from devices to UTF-8 for MQTT and converts UTF-8 strings from MQTT to GBK for devices. This applies to fields such as license plates, driver names, text messages, area names, and client parameters. MQTT payloads always use UTF-8 regardless of this setting.
  - Added support for custom `msg_sn` values in downlink messages. When a downlink MQTT payload contains `msg_sn` in the header, the gateway uses it instead of the auto-generated channel sequence number.
  - Fixed JT/T 808 gateway parameter setting (0x8103) and query response (0x0104) message handling for CAN bus ID parameters (0x0110~0x01FF), which should use BYTE[8] data type with base64 encoding in JSON instead of string type.
  - Fixed JT/T 808 0x0702 driver identity report message parsing.

- [#17013](https://github.com/emqx/emqx/pull/17013) Added GBT32960-2025 protocol support to the GBT32960 gateway.

  The gateway now automatically detects the protocol version by frame header (`##` for 2016, `$$` for 2025) and handles version-specific parsing and serialization, including:

  - New 2025 info types: Vehicle, DriveMotor, FuelCell, Engine, Location, Alarm, PowerBatteryVoltage/Temp, FuelCellStack, SuperCapacitor, SuperCapacitorExtreme, and Digital Signature.
  - New command: Activation (0x09/0x0A).
  - Version-aware parameter sizes for parameter query/setting (0x02/0x03: BYTE in 2025 vs WORD in 2016).
  - 2025 vehicle login with BMS battery pack encoding fields.

#### Data Integration

- [#16511](https://github.com/emqx/emqx/pull/16511) Added support for the IoTDB Table Model in data integration.

- [#16962](https://github.com/emqx/emqx/pull/16962) Improved Kafka source polling behavior by ensuring fetch requests wait briefly for data instead of returning empty batches immediately when no records are available. This reduces unnecessary polling delays and helps Kafka consumers receive new records more consistently.

- [#17025](https://github.com/emqx/emqx/pull/17025) The way the InfluxDB database performs health checks and credential verification has been changed.

  It no longer performs checks by executing `SHOW DATABASES`, which could be falsely flagged as a system penetration by some auditing systems.

  See also [emqx/influxdb-client-erl#54](https://github.com/emqx/influxdb-client-erl/pull/54).

- [#17089](https://github.com/emqx/emqx/pull/17089) MQTT ingress bridges now support consuming from remote message queues exposed as `$queue/{name}/{bind-filter}` when the remote broker supports MQTT 5 Subscription Identifiers. Queue subscriptions are rejected when Subscription Identifiers are unavailable, and regular topic subscriptions automatically retry without Subscription Identifiers if the remote broker does not accept them.

- [#17104](https://github.com/emqx/emqx/pull/17104) Added date-part placeholders to blob name templates in aggregated upload actions (Azure Blob Storage, Amazon S3, GCS, Snowflake, S3 Tables). Placeholders are rendered against the aggregation start time and default to UTC. This enables Hive-partitioned object layouts (e.g. `year=2025/month=04/day=22/hour=07/...`) directly consumable by Spark, Databricks, and Synapse.

  Supported placeholders:

  - `${datetime.YYYY}`
  - `${datetime.MM}`
  - `${datetime.DD}`
  - `${datetime.hh}`
  - `${datetime.mm}`
  - `${datetime.ss}`
  - `${datetime.DOY}` (day of year)

  Each placeholder can be prefixed with an explicit timezone:

  - `utc` (default): e.g. `${datetime.utc.YYYY}`
  - `local` (EMQX node's system timezone): e.g. `${datetime.local.YYYY}`

- [#17136](https://github.com/emqx/emqx/pull/17136) Added the `ping_with_auth` option for InfluxDB connectors. When enabled, health checks include the configured credentials for InfluxDB-compatible services that require authenticated health check requests. Also fixed the InfluxDB connector/action to preserve Unicode text when writing values from `write_syntax` literals or MQTT payloads.

- [#17165](https://github.com/emqx/emqx/pull/17165) Added the `resource_opts.dispatch_strategy` option for actions.

  The new option defaults to `per_clientid`, preserving the previous buffer worker dispatch behavior. Setting it to `random` makes queries without an explicit `pick_key` use a random dispatch key, which helps spread traffic across multiple buffer workers when a small number of clients publish a large amount of messages.

- [#17170](https://github.com/emqx/emqx/pull/17170) [#17282](https://github.com/emqx/emqx/pull/17282) [#17297](https://github.com/emqx/emqx/pull/17297) Added `tcp_opts` (`nodelay`, `sndbuf`, `recbuf`, `buffer`, `keepalive`, `delay_send`, `active_n`) to the MQTT bridge connector and Cluster Link configurations, so the outbound MQTT client TCP socket can be tuned per connection. Unset fields keep the operating system / `gen_tcp` defaults. `delay_send` (off by default) coalesces small writes for better throughput at the cost of a small latency increase.

- [#17474](https://github.com/emqx/emqx/pull/17474) Reduced the overhead of IoTDB REST API connector health checks by using a bounded version query instead of listing all databases on each check.

- [#17481](https://github.com/emqx/emqx/pull/17481) Added a `retain_as_published` option to MQTT bridge ingress (source) subscriptions. When the bridge connects to the remote broker using MQTT 5.0 and `retain_as_published = true`, the original `retain` flag on forwarded messages is preserved instead of being cleared, allowing the bridge to faithfully republish retained messages from upstream. The default is `false` to keep existing behavior. The option has no effect when `proto_ver` is `v3` or `v4`.

  Also, the connector now emits a warning log when `bridge_mode = true` is configured together with `proto_ver = v5`, since the legacy bridge-mode flag has no effect under MQTT 5.0; set `retain_as_published` on individual subscriptions instead.

- [#17508](https://github.com/emqx/emqx/pull/17508) Set the PostgreSQL `application_name` startup parameter to `emqx` for PostgreSQL and TimescaleDB connector connections.

  This makes EMQX database sessions easier to identify in PostgreSQL logs and views such as `pg_stat_activity`.

- [#17594](https://github.com/emqx/emqx/pull/17594) Added support for configuring Google Cloud Pub/Sub and BigQuery connector `service_account_json` values with `file://` secret files, so service account credentials can be injected from external files.

#### Plugins

- [#16735](https://github.com/emqx/emqx/pull/16735) EMQX now supports plugin-defined HTTP API callbacks under `/api/v5/plugin_api/{plugin}/...`.

  This allows plugin authors to expose plugin-specific API endpoints through the Dashboard API service, with consistent authentication and HTTP error handling.

- [#16849](https://github.com/emqx/emqx/pull/16849) Added cookie-based authentication fallback for plugin API endpoints.

  Plugin UI iframes served by the Dashboard can now authenticate via the `emqx_auth` cookie when no `Authorization` header is present. This only applies to `/api/v5/plugin_api/...` paths.

- [#17549](https://github.com/emqx/emqx/pull/17549) Added the EMQX Backup Sync plugin to periodically synchronize selected configuration from a primary cluster to a secondary cluster using the Data Backup APIs. The plugin supports configurable TLS options for HTTPS calls to the primary cluster.

#### REST API

- [#16718](https://github.com/emqx/emqx/pull/16718) Improved the REST API Swagger specification.

  Previously, summaries and descriptions of specification fields were mixed together. Now, summaries are brief, simple, and punctuation-free, while descriptions provide the details.

- [#16958](https://github.com/emqx/emqx/pull/16958) Added focused `/api-spec` endpoints and a Dashboard API spec explorer page for easier browsing of EMQX HTTP API documentation.

  The Dashboard now serves tag-scoped and drill-down OpenAPI slices, and these endpoints are disabled together with Swagger when `dashboard.swagger_support` is set to `false`. Added `emqx ctl api_keys` CLI commands to list, show, add, delete, enable, and disable API keys from the command line.

#### Deployment

- [#17079](https://github.com/emqx/emqx/pull/17079) Added `service.wsEnabled` option to the Helm chart to suppress the ws/wss Service port entries when MQTT WebSocket listeners are disabled. Defaults to `true` to preserve existing behavior.

### Bug Fixes

#### Core MQTT Functionalities

- [#16651](https://github.com/emqx/emqx/pull/16651) Fixed a rare connection process crash during shutdown caused by operating on an already closed socket, typically under high system stress. Prior to this fix, this race condition typically resulted in an `error` level log saying `{badmatch,{ok,{sock_error,closed}...`.

- [#16675](https://github.com/emqx/emqx/pull/16675) Fixed timestamp ordering issue where `disconnected_at` could be later than `connected_at` during session takeover or discard scenarios.

  Previously, `disconnected_at` was recorded too late (in `ensure_disconnected`), after the new session's `connected_at` was already set. This caused a race condition where `disconnected_at > connected_at`, making it difficult to track client presence state externally.

  The fix records `disconnected_at` immediately when takeover begins or when discard is received, ensuring it's always earlier than the new session's `connected_at`. This ensures correct timestamp ordering for external presence state tracking systems.

- [#16684](https://github.com/emqx/emqx/pull/16684) Enabled `mqtt.client_attrs_init` expressions to use the password, for example by passing it to `jwt_value`, when initializing client attributes.

  Previously, `client_attrs_init` ran before password was added to the rendering context, so expressions depending on password could not be resolved.

- [#16715](https://github.com/emqx/emqx/pull/16715) Fixed an issue where retained `$SYS` messages (for example, broker/node identity topics) were stored without expiry, which could leave stale node identifiers visible in Dashboard views after StatefulSet rotation.

  Now, newly published retained `$SYS` messages include `Message-Expiry-Interval = 3600` (1 hour).

  For already existing stale retained `$SYS` entries created before this change, you can manually clear them by publishing an empty retained message to the stale topic:

  ```
  emqx eval 'emqx:publish(emqx_message:set_flag(retain, true, emqx_message:make(emqx_sys, <<"$SYS/brokers/emqx@127.0.0.1/sysdescr">>, <<>>))).'
  ```

  Replace the topic in the command with the stale `$SYS/...` topic you want to remove.

- [#16731](https://github.com/emqx/emqx/pull/16731) Fixed a crash in `emqx ctl subscriptions list` that could happen when shared subscriptions were present.

  Before this fix, listing subscriptions could fail for some clients and return no output.

  After this fix, `emqx ctl subscriptions list` works reliably with both regular and shared subscriptions.

- [#16779](https://github.com/emqx/emqx/pull/16779) Improved handling of malformed first packets by classifying them as invalid CONNECT packets and adding better protocol hints in logs.

- [#16781](https://github.com/emqx/emqx/pull/16781) Fixed CONNECT validation when retained messages are unavailable.

  When `mqtt.retain_available` is set to `false`, CONNECT packets with Will Retain set are now correctly rejected with CONNACK reason `Retain not supported (0x9A)`.

- [#16782](https://github.com/emqx/emqx/pull/16782) Fixed MQTT v5 protocol handling for invalid PUBLISH properties.

  If a client sends a PUBLISH packet containing `Subscription-Identifier`, EMQX now treats it as a protocol error and disconnects the client.

- [#16783](https://github.com/emqx/emqx/pull/16783) Fixed MQTT v5 SUBSCRIBE validation for `Subscription-Identifier` upper bound.

  EMQX now accepts `268435455` (0x0FFFFFFF), which is the maximum valid Subscription Identifier value defined by the MQTT spec.

- [#16956](https://github.com/emqx/emqx/pull/16956) Log client connection termination at warning level instead of info when the reason is `emsgsize` (received packet exceeds `mqtt.max_packet_size`).

- [#17139](https://github.com/emqx/emqx/pull/17139) Restored `retainer.enable` as a real runtime switch for the retainer subsystem.

  This allows deployments to keep MQTT retained-message protocol support enabled while disabling retained-message storage, instead of relying on `mqtt.retain_available`, which can reject retained publishes at the protocol layer.

- [#17172](https://github.com/emqx/emqx/pull/17172) Fixed an issue where MQTT packets (such as PUBACK) sent by a client right before disconnecting could be lost when the connection process had pending outbound messages in its mailbox. Now the connection process correctly drains its mailbox before shutting down, ensuring that inbound packets are processed even after the socket is closed.

- [#17353](https://github.com/emqx/emqx/pull/17353) Fixed an issue in the `socket` TCP backend where outbound MQTT packets could be sent in the wrong order when a client connection experienced repeated send congestion. This scenario was practically very unlikely to occur.

- [#17383](https://github.com/emqx/emqx/pull/17383) After a session takeover, the channel info reflected by the Dashboard and REST API (`mqueue_len`, `inflight_cnt`) now updates immediately after the takeover replay completes, rather than waiting for the next 15-second stats refresh tick.

- [#17515](https://github.com/emqx/emqx/pull/17515) Fixed an issue where Message Queue subscriptions using QoS 0 could stop receiving messages after the queue subscriber's local inflight window became full.

- [#17569](https://github.com/emqx/emqx/pull/17569) Reduced MQTT v5 user-property parsing cost from quadratic to linear.

  Previously a CONNECT, PUBLISH or SUBSCRIBE packet carrying many user-properties caused super-linear scheduler time on the owning connection process, because each parsed property was appended to the end of the accumulated list. Parsing now scales linearly with the number of entries while preserving their wire order.

#### Rule Engine

- [#16699](https://github.com/emqx/emqx/pull/16699) Previously, under certain race conditions, long and cryptic logs like the following could be printed:

  ```
  2026-02-03T13:53:54.576326+00:00 [error] Generic server <0.11323236.0> terminating. Reason: {{badkey,'actions.success'},[{erlang,map_get,['actions.success',#{}],[{error_info,#{module => erl_erts_errors}}]},{emqx_metrics_worker,idx_metric,4,[{file,"emqx_metrics_worker.erl"},{line,683}]},{emqx_metrics_worker,inc,4,[{file,"emqx_metrics_worker.erl"},{line,322}]},{emqx_rule_runtime,do_eval_action_reply_t...
  ```

  Now, EMQX prints more meaningful information to help debug the issue.

- [#16847](https://github.com/emqx/emqx/pull/16847) Fixed a crash when non-ASCII unicode string is used in message transformation expression.

- [#17211](https://github.com/emqx/emqx/pull/17211) Added the `connected_at` field to the `$events/client/connack` Rule Event, which was stated in the documentation but missing from the actual data.

#### Data Integration

- [#16622](https://github.com/emqx/emqx/pull/16622) Fixed an issue where, if an Action used async query mode and its Connector became disconnected after multiple health checks, its Fallback Actions could be triggered twice.

- [#16659](https://github.com/emqx/emqx/pull/16659) When using an older MQTT Connector configuration with static clientids (from 5.10.0 and earlier) on later EMQX versions, the username and password at the root of the configuration were ignored. This could cause issues when upgrading while keeping the same configuration, as the MQTT clients would stop using the credentials.

  Now, if there are username and/or password fields in the root Connector, those credentials are merged with any specific ones specified per clientid, the latter taking precedence.

- [#16685](https://github.com/emqx/emqx/pull/16685) Fixed an issue where the Sparkplug B metrics alias mapping feature could fail after an EMQX node was upgraded from 5.10.x to 6.0.y.

  Due to differences in how Protobuf code was generated before 6.0.0, if an EMQX node had started on an older version (< 6.0.0) with the same OTP version as the newer version, the cached Protobuf code was kept but no longer matched the newer code's expectations. This caused Sparkplug B alias mapping to fail after upgrading from 5.10.x to 6.0.y.

- [#16723](https://github.com/emqx/emqx/pull/16723) Fixed an issue with RabbitMQ Connector/Action/Source where, if some connection or channel processes died unexpectedly, the Connector/Action/Source would be reported as disconnected and would not recover without being restarted.

- [#16742](https://github.com/emqx/emqx/pull/16742) Fixed GreptimeDB TLS connection failures.

- [#16796](https://github.com/emqx/emqx/pull/16796) Fixed handling of multiline SQL statements in connector actions.

- [#16863](https://github.com/emqx/emqx/pull/16863) Added a warning log when an async reply is received for an already-expired request.

- [#16890](https://github.com/emqx/emqx/pull/16890) Fixed an ExHook issue where successful reconnect reloads could duplicate the same server name in the running list and trigger repeated callback dispatches.

- [#16936](https://github.com/emqx/emqx/pull/16936) Fixed an issue where the health check of an Azure Blob Storage Action in aggregate mode could timeout if the container contained too many blobs.

- [#16955](https://github.com/emqx/emqx/pull/16955) Eliminated Kafka producer action false health check warning logs.

  Previously if Kafka producer is idling for too long, Kafka may close the connection (typically default is 10 minutes), if Kafka producer action health-checks happen to be performed around the same moment, there could be a false warning message with message "not_all_kafka_partitions_connected".

- [#16972](https://github.com/emqx/emqx/pull/16972) HTTP and GCP PubSub Actions were patched to treat transient connection errors with reason `closing` as recoverable errors, reducing log noise.

- [#17084](https://github.com/emqx/emqx/pull/17084) Fixed an issue with MQTT Sources in which, if its Connector used `clean_start = false` and reconnected to a broker with a session containing messages, those messages would not trigger rule actions.

- [#17111](https://github.com/emqx/emqx/pull/17111) Fixed query execution for PostgreSQL connectors in disable prepared statements mode. Previously, concurrent queries could interleave and produce errors.

- [#17113](https://github.com/emqx/emqx/pull/17113) Fixed RocketMQ connector isolation: a misconfigured or unreachable RocketMQ connector no longer destabilizes other RocketMQ connectors on the same node. Previously, one connector with an unreachable broker could stall the shared client supervisor for up to 60 seconds, causing sibling connectors to flap with `resource_health_check_timed_out` and for Dashboard operations on them to hang.

  The default TCP/TLS connect timeout is also lowered from 60 seconds to 10 seconds so a misconfigured server surfaces as failed quickly instead of appearing stuck.

- [#17180](https://github.com/emqx/emqx/pull/17180) Fixed an issue where, under heavy load, a timed out call to a MongoDB process would be interpreted as an unrecoverable error and wouldn't be retried. Now, the message will be retried on such events.

- [#17216](https://github.com/emqx/emqx/pull/17216) Fixed Timescale/PostgreSQL actions to report a structured bad parameter error instead of crashing the database connection process when a quoted JSON numeric string is mapped to a `FLOAT` column.

- [#17250](https://github.com/emqx/emqx/pull/17250) Fixed Redis Sentinel connectors to support separate authentication settings for Redis data nodes and Sentinel nodes.

- [#17293](https://github.com/emqx/emqx/pull/17293) Fixed an issue where, when writing a Parquet file with an object containing a required key but with an `undefined`/`null` value, a corrupt file would be written instead of raising an error.

- [#17303](https://github.com/emqx/emqx/pull/17303) Upgraded Kafka client libraries: `brod` from 4.5.2 to 4.5.4 and `wolff` from 4.1.9 to 4.1.10.

  Notable fixes picked up from upstream:

  - `brod`: fix a race condition during Kafka connection re-authentication (via `kafka_protocol` 4.3.4).
  - `wolff`: under high-memory load control (`drop_if_highmem`), keep a minimum buffer reserve so the producer is not starved of in-flight data; only bytes exceeding the reserve are dropped.

- [#17347](https://github.com/emqx/emqx/pull/17347) Upgraded the RocketMQ client dependency to `v0.7.2` to fix memory growth in async producer requests.

- [#17439](https://github.com/emqx/emqx/pull/17439) Fixed an issue where the health check of an Azure Blob Storage Connector could timeout, or generate large bandwidth costs, if the storage account contained too many containers. Companion fix to #16935.

- [#17450](https://github.com/emqx/emqx/pull/17450) Fixed an issue where the `/prometheus/data_integration` Prometheus endpoint could respond with a 500 status when using `mode=node`. This issue would only arise when the configuration for Actions and Connectors was manually edited and inconsistent, having an Action whose Connector does not exist.

- [#17568](https://github.com/emqx/emqx/pull/17568) Upgraded the Kafka client library `brod` to 4.5.5.

  Consumer group: respect the broker-assigned member ID when the join response carries the `member_id_required` error code (returned by older Kafka brokers, e.g. 2.2.0, that do not support static member instance IDs). Previously the member ID was discarded on error, preventing the retry from succeeding.

- [#17579](https://github.com/emqx/emqx/pull/17579) Fixed Redis Sentinel connectors to use isolated Sentinel managers per resource and clean them up when resources stop, avoiding shared Sentinel state across connectors.

- [#17584](https://github.com/emqx/emqx/pull/17584) Limited the amount of data returned during Connector health checks of Snowflake Aggregated Connectors. This only has observable effects if the list of existing schemas was very large, in which case the health check will take far less time to execute.

- [#17588](https://github.com/emqx/emqx/pull/17588) Limited the amount of data returned during Connector and Action health checks of Kinesis integrations. This only has observable effects if the list of existing schemas was very large, in which case the health check will take far less time to execute.

- [#17595](https://github.com/emqx/emqx/pull/17595) Limited the amount of data returned during Connector health checks of S3 and S3 Tables integrations. This only has observable effects if the list of existing buckets was very large, in which case the health check will take far less time to execute.

#### Clustering

- [#16393](https://github.com/emqx/emqx/pull/16393) Improved the stability of the Cluster Link route replication under unstable network conditions.

- [#16739](https://github.com/emqx/emqx/pull/16739) Improved cluster recovery time after a simultaneous restart of all nodes.

  The built-in Mria database management system no longer waits for the full synchronization of an internal table used to generate transaction synchronization events.

- [#17132](https://github.com/emqx/emqx/pull/17132) Fixed an issue where adding or removing topic metrics could fail on a replicant node when its raw config or runtime state had drifted, raising a `cluster_rpc_apply_failed` alarm and stalling cluster RPC replication. Duplicate-add and missing-remove are now rejected on the initiator only, while replicants apply the change idempotently.

- [#17182](https://github.com/emqx/emqx/pull/17182) Bumped emqx-OTP to 27.3.4.2-8 for mria.

  Without this change, during EMQX startup, Mria app boot may get stuck if it's not connected to the cluster.

- [#17214](https://github.com/emqx/emqx/pull/17214) Removed cryptic error-level logging of disconnect events from Cluster Link message forwarding MQTT clients, in favor of more user-friendly messages with enough context for troubleshooting. Events similar to this one should no longer appear in the error logs:

  ```
  2026-05-06T03:00:48.738654+00:00 [error] [PoolWorker] unexpected info: {disconnected,141,#{}}
  ```

- [#17218](https://github.com/emqx/emqx/pull/17218) Avoid `bin/emqx` and `bin/emqx_ctl` invocations from triggering `nodeup`/`nodedown` events on the running broker, which previously surfaced as misleading `cm_registry_node_down` warnings in the broker log. The temporary helper nodes started by these scripts now register as hidden Erlang nodes, as intended.

- [#17269](https://github.com/emqx/emqx/pull/17269) Improved cluster recovery after a network partition.

  - Previously, part of the clients connected to the replicant nodes could be lost from the global registry. This could lead to inconsistent behavior during takeover and incorrect information displayed in the Dashboard.

    This fix adds a background process that re-registers the existing clients when network partition is healed. It also adds a new alarm: "Broker is recovering after a network partition", which is raised while the global registry is being rebuilt.

  - Introduced a new cluster auto-heal algorithm that can automatically recover overlapping network partitions.

- [#17343](https://github.com/emqx/emqx/pull/17343) Fixed a clustered-config replication bug where importing a data backup (or loading a HOCON config via `emqx ctl conf load` / `PUT /api/v5/configs`) that contained a `file`-type authorization source could leave peer nodes lagging with a `cluster_rpc_apply_failed` / `failed_to_read_acl_file` error.

  The importer used to write the ACL file locally and replace inline `rules` with a `path`, then ship the path-form config across the cluster. Peer nodes have no such file on disk and so could not apply the change. The config sent to the cluster now keeps `rules` inline, so each peer writes its own copy of the ACL file from the replicated content.

- [#17348](https://github.com/emqx/emqx/pull/17348) Fixed noisy and misleading `emqx ctl conf cluster_sync status` diagnostics when clustered nodes have the same effective checked configuration but different raw configuration representations.

  The command now suppresses raw-only representation differences that do not correspond to checked configuration changes, while still warning when checked configuration is inconsistent. It also avoids crashing when a raw configuration key exists on one node but is missing from another node.

  It also ignores timestamp-only metadata differences in `created_at` and `last_modified_at` for actions, sources, bridges, and rule metadata. Data import or boot-time configuration loading can refresh these generated timestamps on only some nodes even when the effective runtime configuration is otherwise identical.

- [#17349](https://github.com/emqx/emqx/pull/17349) Improved responsiveness of a Cluster Link in situations when route replication was stuck connecting to an unresponsive target cluster. Now, deleting such Cluster Link should finish slightly sooner.

- [#17382](https://github.com/emqx/emqx/pull/17382) Fixed corruption of global channel registry that may occur when cluster experiences a network partition.

- [#17424](https://github.com/emqx/emqx/pull/17424) Fixed a global session registry leak that could leave duplicate or stale entries for the same client ID after a network partition followed by Mnesia autoheal.

  Discard and takeover-kick RPC handlers now also remove the registry row when the target process is no longer alive, and the registration throttle on the connect path now recognizes tombstone rows (no local channel state) and reaps them instead of blocking new connections for the same client ID indefinitely.

- [#17432](https://github.com/emqx/emqx/pull/17432) Fixed an issue where concurrent Cluster Link API requests could return generic error responses, instead of returning either success or not found.

- [#17469](https://github.com/emqx/emqx/pull/17469) Fixed the issue where warnings similar to those below are emitted when enabling or disabling an active Cluster Link.

  ```
  [warning] tag: RESOURCE, msg: handle_resource_metrics_failed, reason: {badkey, matched}, event: matched, ...
  ```

- [#17586](https://github.com/emqx/emqx/pull/17586) Periodically purge stale entries from the global session registry. Previously, when a session's owner process died without a clean unregister (for example, after a brief network split that prevented the unregister from replicating, or when one core's consensus check timed out during the down-event cleanup), the registry row could remain forever if the same client ID never reconnected. A new throttled background sweep on each core node now removes such rows. The sweep is bounded to at most 500 registry rows per second per node and runs no more often than once every 10 minutes, so it does not measurably affect broker throughput even on registries holding millions of sessions.

#### Access Control

- [#16692](https://github.com/emqx/emqx/pull/16692) Fixed a CRL cache regression where `emqx_crl_cache:evict/1` did not fully clear internal URL state. After eviction, the same CRL URL now re-registers correctly on next use, restores its refresh timer, and avoids repeated HTTP fetches per connection.

- [#16780](https://github.com/emqx/emqx/pull/16780) Fixed an issue in authorization source validation where requests missing the `type` field could trigger an internal error.

  Now EMQX returns a clear `BAD_REQUEST` validation error for this case.

- [#16805](https://github.com/emqx/emqx/pull/16805) Added support for authz hook results to opt out of authorization cache storage for dynamic ACL decisions.

- [#16865](https://github.com/emqx/emqx/pull/16865) Added `cert_common_name` and `cert_subject` aliases for `mqtt.client_attrs_init` expressions, alongside the existing `cn` and `dn` variables.

- [#16868](https://github.com/emqx/emqx/pull/16868) Improved REST API authentication error messages to guide programmatic clients toward using API keys (Basic auth) instead of repeatedly logging in for bearer tokens. Error responses now mention the `api_key.bootstrap_file` configuration option and the `POST /api_key` endpoint for creating persistent API keys.

- [#16939](https://github.com/emqx/emqx/pull/16939) Fixed the built-in database authenticator so it no longer logs a warning when the default bootstrap file path is configured but the file does not exist.

- [#17045](https://github.com/emqx/emqx/pull/17045) Fixed password-based authentication backends to let the auth chain continue when the CONNECT packet has no password, instead of rejecting the connection immediately.

  Previously, if a client connected without a password, the first password-based authenticator (built-in database, MySQL, PostgreSQL, MongoDB, Redis, or LDAP) in the chain would return an error, blocking any subsequent authenticators from being tried.

- [#17100](https://github.com/emqx/emqx/pull/17100) Fixed OIDC SSO login failing with `provider_not_ready` when the identity provider returns a JWKS response whose `Content-Type` uses the `+json` structured syntax suffix (e.g. `application/jwk-set+json; charset=utf-8`). Such responses are now accepted as valid JWKS content.

- [#17122](https://github.com/emqx/emqx/pull/17122) Fixed Dashboard RBAC checks for SSO users with URL-encoded usernames such as email addresses, so viewer self-service MFA disable requests work correctly when `force_mfa` is disabled.

- [#17140](https://github.com/emqx/emqx/pull/17140) Fixed a silent failure when EMQX fetched a Certificate Revocation List (CRL) over HTTP from a server that returns a DER-encoded body (`Content-Type: application/pkix-crl`, the format mandated by RFC 5280 §5).

  Previously, EMQX only decoded PEM-encoded CRL bodies; a DER body was silently treated as zero CRLs and cached as an empty list, causing every TLS handshake on `enable_crl_check = true` listeners to fail with `bad_crls, no_relevant_crls` and no log line indicating what went wrong.

  EMQX now decodes both PEM and DER CRL bodies. When a fetched body is neither, a warning is logged with the URL so the misconfiguration is visible.

- [#17171](https://github.com/emqx/emqx/pull/17171) Fixed an RBAC issue that prevented namespaced Dashboard administrators from enabling or disabling MFA for their own account.

  Namespaced administrators remain restricted from managing MFA settings for other Dashboard users.

- [#17177](https://github.com/emqx/emqx/pull/17177) Dashboard-created REST API keys are now generated randomly instead of being derived from the API key name.

- [#17223](https://github.com/emqx/emqx/pull/17223) Fixed missing client certificate when a TCP-passthrough proxy (e.g. GCP TCP Proxy NLB, AWS NLB) is placed in front of an SSL listener with `proxy_protocol = true`. The TLS handshake at the listener was completing successfully and the client certificate was present, but it was not exposed to authentication or rule events. Functions, ACL rules, and authentication backends that depend on the client certificate (CN, subject, full PEM) now work correctly in this deployment shape.

- [#17428](https://github.com/emqx/emqx/pull/17428) Fixed a Dashboard OIDC SSO crash that prevented EMQX from completing the OpenID provider discovery when the provider's `.well-known/openid-configuration` response included a `Cache-Control` header such as `max-age=0` (observed with Kanidm). The crash caused the OIDC supervisor to exhaust its restart budget after a single failure, leaving SSO unable to recover without a config re-save. The cache-control parser is now tolerant of these values, the worker no longer hard-crashes on a bad expiry, and the OIDC supervisor allows several restarts within a minute so transient failures retry cleanly.

#### Gateway

- [#16603](https://github.com/emqx/emqx/pull/16603) Fixed the CoAP Gateway when running in DTLS connection mode.
- [#16670](https://github.com/emqx/emqx/pull/16670) NATS gateway now enforces the max publish payload, honors the `echo` option (no local delivery), and improves publish/subscribe subject handling and related error messages.
- [#17141](https://github.com/emqx/emqx/pull/17141) Fixed CoAP connection-mode token takeover so reconnecting UDP/DTLS clients can resume with a valid token while invalid token/clientid combinations are rejected. Also ensured required connection info fields are present before running CoAP takeover connected hooks.
- [#17258](https://github.com/emqx/emqx/pull/17258) Fixed an issue in the MQTT-SN gateway where a connected client sending a second CONNECT packet on the same session would crash its connection process. The gateway now responds with a DISCONNECT and closes the session gracefully.
- [#17287](https://github.com/emqx/emqx/pull/17287) Fixed MQTT-SN clients crash caused by packets received in unexpected connection or Will states, including `DISCONNECT` during connection setup, `REGISTER` before the Will handshake completes, and `WILLMSGUPD` before a Will topic exists.
- [#17581](https://github.com/emqx/emqx/pull/17581) Fixed the JT/T 808 gateway to use the phone number accepted during authentication as the connection identity, rejecting mismatched registration-code authentication attempts and subsequent uplink frames with a different phone number.

#### Multi-tenancy

- [#17118](https://github.com/emqx/emqx/pull/17118) Improved pagination on multi-tenancy list endpoints (`/mt/ns_list`, `/mt/ns_list_details`, `/mt/managed_ns_list`, `/mt/managed_ns_list_details`, `/mt/ns/{ns}/client_list`):
  - Added an RFC 8288 `Link: <?...>; rel="next"` response header. When more pages are available the header carries the query-only URI-reference of the next page; when absent, the current response is the last page. This removes the prior ambiguity where a full page (`len(results) == limit`) could not be distinguished from the exact-boundary "no more data" case without an extra request.
  - Added inclusive keyset cursor query parameters (`first_ns`, `first_clientid`) alongside the existing exclusive cursors (`last_ns`, `last_clientid`). The inclusive form supports exact-match lookup (e.g. `?first_ns=foo&limit=1`) and is preserved across paginated Link headers when the caller opts in. The two forms are mutually exclusive on a single request; supplying both returns HTTP 400.
- [#17406](https://github.com/emqx/emqx/pull/17406) Now, events captured by a trace initiated by a namespaced admin are limited to the namespace of such admin, for traces of types topic, IP address, and clientid. Traces of type rule ID already had such behavior.

#### Plugins

- [#16784](https://github.com/emqx/emqx/pull/16784) Reduced noisy plugin startup warnings in single-node deployments.

  EMQX no longer tries to fetch plugin config from the local node during cluster config sync, avoiding repeated `config_not_found_on_node` warnings at startup.

- [#16823](https://github.com/emqx/emqx/pull/16823) Fixed a Dashboard plugin management issue for preinstalled plugins.

  When a plugin package is unpacked into `plugins/` before node startup, starting it from the Dashboard no longer causes `Plugin Config Not Found` on the plugin config page.

- [#16842](https://github.com/emqx/emqx/pull/16842) Reduced noisy plugin config warning logs when no peer node has the plugin config yet.

  Previously, when a node tried to fetch plugin config from peer nodes during startup, it would log a warning even when all peers simply didn't have the config (e.g., first node to load the plugin). Now this benign case is logged at debug level, and only genuine errors (RPC failures, timeouts) remain as warnings.

- [#16843](https://github.com/emqx/emqx/pull/16843) Fixed an issue where HTTP headers and query string parameters were not passed through to plugin API handlers, causing plugins to receive empty headers and missing query parameters.

- [#16904](https://github.com/emqx/emqx/pull/16904) Prevent enabling or starting multiple versions of the same plugin at once. When a newer version is enabled, older configured versions of that plugin are automatically disabled, and management API actions now return a clear error instead of reporting success while another version is still active.

- [#17247](https://github.com/emqx/emqx/pull/17247) When a plugin's REST API callback crashes or runs over its timeout budget, the broker now logs the failing API method and path together with the configured timeout, so the offending call is identifiable in mixed-traffic logs. A timeout is logged as a warning (not an error) and includes a hint pointing at `plugins.api_endpoint.timeout`, the config key to raise when a plugin callback legitimately needs more time.

- [#17473](https://github.com/emqx/emqx/pull/17473) Lowered the log level of `unabled_to_stop_plugin_apps` from warning to info when the plugin's Erlang applications cannot be stopped because other running applications still depend on them. This is an expected, non-actionable condition during plugin unload and no longer raises a warning.

- [#17575](https://github.com/emqx/emqx/pull/17575) Fixed a race condition in the emqx_username_quota plugin that could cause the per-username session counter to become inconsistent with the actual number of tracked client records. The counter could be decremented past zero and then be deleted while a concurrent session registration incremented it, losing the increment permanently.

#### REST API

- [#17002](https://github.com/emqx/emqx/pull/17002) Updated `minirest` library to version 1.4.12. This version fixes a bug that caused EMQX API to produce malformed API responses with `204 No Content` status line, emitting invalid `content-length` header.

- [#17054](https://github.com/emqx/emqx/pull/17054) Fixed `GET /api/v5/configs?key=...` returning incomplete data when `Accept: application/json` was set.

  Previously, the JSON response ignored the `key` query parameter and always returned a fixed subset of root configurations, which excluded keys like `multi_tenancy`. The endpoint now honors the `key` parameter in JSON responses consistently with the hocon (text/plain) response.

- [#17319](https://github.com/emqx/emqx/pull/17319) `GET /api/v5/schemas/{hotconf,actions,connectors}` now returns the response with `Content-Type: application/json`. Previously the response body was valid JSON but the header was `text/plain; charset=utf-8`, which broke clients that dispatch on the response content type.

#### Observability

- [#16661](https://github.com/emqx/emqx/pull/16661) Improved `topic_metrics` and `cluster_rpc` logging when an invalid topic is requested.
- [#16674](https://github.com/emqx/emqx/pull/16674) Ensured that the Erlang PID is printed as a log data field.
- [#16876](https://github.com/emqx/emqx/pull/16876) Changed log message `msg_publish_not_allowed` to `msg_not_routed_to_subscribers`.
- [#16879](https://github.com/emqx/emqx/pull/16879) Added `log.audit.cache_size` as the primary config key for the audit log DB cache size, while keeping `log.audit.max_filter_size` for backward compatibility.
- [#17513](https://github.com/emqx/emqx/pull/17513) Fixed Prometheus matched authorization allow/deny metrics so they reflect real matched authorization decisions.

#### Deployment

- [#16545](https://github.com/emqx/emqx/pull/16545) Fixed `node.cookie` handling of `#` character. Previously, if the cookie contained `#`, only the prefix before `#` would take effect. For example, if `abc#d` was configured, only `abc` was used as the cookie.

  Added validation to reject problematic characters: backslash, single quote, double quote, and space.

- [#16620](https://github.com/emqx/emqx/pull/16620) Fixed a CRC32C dynamic library loading issue on aarch64.

- [#16657](https://github.com/emqx/emqx/pull/16657) Fixed an issue where, when importing configuration from an older node version into a newer one, values would not be upgraded according to newer code, leading to strange behavior.

  One such example is importing an MQTT Connector with static clientids from 5.10.0 into 6.0.0. In 5.10.0, usernames and passwords could not be associated with particular static clientids, and this was represented internally in a certain way. Later versions added the capability to create those associations with a different internal representation. This subtle internal representation conversion was missing when importing such configurations in previous EMQX versions.

- [#17024](https://github.com/emqx/emqx/pull/17024) Dashboard HTTP listener now automatically uses IPv6 when the bind address is an IPv6 address, removing the need to explicitly set `inet6 = true`.

- [#17227](https://github.com/emqx/emqx/pull/17227) Cluster config file save errors now name the file and the underlying reason.

  When `cluster.hocon` (or its directory) is read-only, immutable, or otherwise unwritable (e.g. mounted read-only into a container), changing config via the Dashboard or REST API previously returned an opaque HTTP 400 with body `{config_update_crashed,{badmatch,{error,ebusy}}}` and only logged a badmatch crash that did not name the file.

  The error now:

  - Logs `failed_to_save_conf_file` with the actual file path and reason (`eacces`, `eperm`, `ebusy`, ...) plus a hint listing common operator-side causes.
  - Returns a structured HTTP 400 body that names both the file and the reason, so the cause is visible in the Dashboard without digging through node logs.

  Previously, when only the temporary file write failed (e.g. read-only directory), the API silently returned HTTP 200 even though the change was not persisted to disk. The API now correctly reports failure in this case as well.

- [#17246](https://github.com/emqx/emqx/pull/17246) Upgraded `jose` library from 1.11.10 to 1.11.12, picking up EC and EdDSA key fixes for newer OTP releases.

- [#17252](https://github.com/emqx/emqx/pull/17252) Published `.sha256` checksum sidecars alongside plugin packages on the official download site, allowing users to verify the integrity of downloaded plugin archives.

- [#17254](https://github.com/emqx/emqx/pull/17254) Improved memory-usage reporting inside containers. The broker now picks the most constraining memory reading among cgroup v2, cgroup v1, and the host's `/proc/meminfo` (smallest non-zero total wins, larger usage ratio breaks ties). Previously the reading could be misleading in two ways: on containers with a tight cgroup limit, the host view could indicate >70% while the cgroup limit was <10% (or the reverse); and on hosts where a cgroup is mounted with no memory limit set, the cgroup reading could collapse the reported usage ratio to ~0%. Overload-protection thresholds and the `Memory used` metric now reflect the limit that actually constrains the process.

- [#17271](https://github.com/emqx/emqx/pull/17271) Hardened the official EMQX Docker image to clear image-scanner findings:

  - Applied Debian security upgrades during the runtime image build, so the image picks up the latest patched `libssl3t64`.
  - Removed the unused `libgnutls30t64` package. EMQX talks TLS via OpenSSL through Erlang/OTP and never links GnuTLS, so it was only present as a transitive dependency of `curl` and showed up in scanner reports.
  - Replaced the Debian `curl` package with a statically-linked `curl` binary from [stunnel/static-curl](https://github.com/stunnel/static-curl) (OpenSSL, HTTP/2, HTTP/3; no RTMP, no GnuTLS). The Debian package would have transitively re-introduced `libgnutls30t64` via `librtmp1`; the static binary avoids this while keeping container health checks that call `curl` working unchanged.

- [#17311](https://github.com/emqx/emqx/pull/17311) Fixed Docker startup when the container hostname cannot be resolved. The entrypoint now falls back to the interface IP address before auto-generating the node name, and fails with a clear error if no node host can be determined.

- [#17342](https://github.com/emqx/emqx/pull/17342) Fixed cluster configuration import failing with a "required_field: node.cookie" schema check error when the exported `cluster.hocon` contained a partial `node` section. Read-only roots (`node`, `rpc`) are not part of the data import anyway, so they are now dropped from the imported config before the pre-flight schema check, letting the running node's own values be used for the validation.

- [#17369](https://github.com/emqx/emqx/pull/17369) Moved the Dashboard listener defaults (`http.bind` and the placeholder HTTPS `ssl_options`) from the user-editable `etc/emqx.conf` into the shipped `etc/base.hocon`. Previously, the hardcoded `emqx.conf` block silently reverted runtime updates to the default self-signed certificate on restart. Runtime updates made through the Dashboard, the REST API, or the `emqx_acme` plugin's automatic HTTPS configuration are now correctly preserved across restarts.

- [#17536](https://github.com/emqx/emqx/pull/17536) Documented the `file://` option in Dashboard tooltips for the SSL listener `password` and other secret-typed configuration fields (MQTT bridge password, cluster link password, Dashboard OIDC client secret, S3 secret access key, AI completion API key, Pulsar/RocketMQ credentials, etc.). The generic secret type description already mentioned this convention, but field-specific descriptions shadowed it in the Dashboard, causing users to assume the field accepted only literal values.

- [#17540](https://github.com/emqx/emqx/pull/17540) Fixed a bug where setting `password = "file://..."` on an SSL listener caused config validation to fail with `bad_password_or_invalid_keyfile` when the keyfile was encrypted. The `file://` reference is now resolved during validation, not only at runtime.

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
