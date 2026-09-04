# Incompatible Changes in EMQX 6.3

## 6.3.0

- [#17185](https://github.com/emqx/emqx/pull/17185) The MQTT parser now runs in strict mode by default. To restore the previous lenient behavior, set `mqtt.strict_mode = false` (globally or per-zone).

  In strict mode, the broker validates incoming MQTT packets against the protocol specification and disconnects clients that send malformed packets. The validations enforced only in strict mode are:

  - **Fixed-header flags.** Reserved DUP/QoS/RETAIN bits must be zero for non-PUBLISH packets, and PUBREL/SUBSCRIBE/UNSUBSCRIBE must use QoS=1 (`bad_frame_header`).
  - **CONNECT reserved bit** must be zero (`reserved_connect_flag`).
  - **CONNECT Will flag consistency**: Will Flag=0 requires Will QoS=0 and Will Retain=0; Will Flag=1 requires Will QoS in {0,1,2} (`invalid_will_qos`, `invalid_will_retain`).
  - **CONNECT Password/Username flags (MQTT 3.1.1 only).** If Username Flag=0, Password Flag must also be 0, per `[MQTT-3.1.2-22]` (`invalid_password_flag`). MQTT 5.0 lifts this constraint and is unaffected.
  - **UTF-8 strings** (proto name, client ID, topic, username, password, will topic, MQTT 5 string properties) must be valid UTF-8 and must not contain control characters U+0000–U+001F or U+007F–U+009F (`utf8_string_invalid`).
  - **Packet identifiers** must be non-zero where required (PUBLISH QoS>0, PUBACK/REC/REL/COMP, SUBSCRIBE/SUBACK, UNSUBSCRIBE/UNSUBACK) (`bad_packet_id`).

  When a client violates one of these checks, the broker logs an `info`-level entry with `msg=frame_parse_error` and a structured `reason` (for example, `cause=invalid_password_flag`, `proto_ver`, or `received_prefix`) for troubleshooting. For MQTT 5.0 connections, the broker also responds with CONNACK/DISCONNECT carrying reason code `0x81 Malformed Packet` before closing; for MQTT 3.1/3.1.1, the connection is silently closed (no CONNACK reason code is defined for malformed packets in those versions).

- [#17215](https://github.com/emqx/emqx/pull/17215) Removed the bundled Swagger UI assets from the EMQX release package, reducing tarball size by approximately 11 MB.

  `/api-docs/swagger.json` continues to serve the full OpenAPI 3 JSON spec, so external Swagger UI deployments that load it by URL keep working. The legacy `/api-docs` URL responds with an HTTP 308 redirect to `/api-spec.html`, the in-tree spec explorer introduced in 6.3.0. Other `/api-docs/*` subpaths (the embedded Swagger UI assets) are no longer served and return 404.

- [#17267](https://github.com/emqx/emqx/pull/17267) The `node.max_ports` config now defaults to `auto`, which scales the Erlang VM port limit (`+Q`) with the number of logical CPU cores: 65536 ports per core for up to 8 cores, and 1048576 (the historical fixed default) above that. Explicit integer values are still accepted.

  This is a behavior change for nodes upgraded from earlier versions where `max_ports` defaulted to a fixed 1048576: hosts with 8 or fewer CPU cores will now boot with a smaller port table. Setups that rely on accepting more than `cores * 65536` connections must set `node.max_ports` explicitly (and restart the node) before upgrading.

  The hidden `node.process_limit` setting is reinstated as an override: when set to a value larger than the derived limit (`2 * max_ports`), it is respected; smaller values are ignored so the process table never under-sizes the port table.

  A new `node.schedulers` setting (default `auto`) controls the Erlang scheduler count (`+S`). With `auto`, the count is capped at the number of logical processors actually available to the VM (`sched_getaffinity` on Linux), so containers limited via `--cpuset-cpus` or Kubernetes CPU requests no longer spawn scheduler OS threads they cannot run in parallel. Set it to a positive integer to override the auto-detected value.

- [#17437](https://github.com/emqx/emqx/pull/17437) Prometheus scrape endpoints (`/api/v5/prometheus/*`) now require authentication by default. Set `prometheus.enable_basic_auth = false` explicitly to restore the previous unauthenticated behavior. Deployments that scrape these endpoints without credentials will need to either configure credentials on the scraper or set the config field. The recommended setup is a dedicated API key with the `monitoring` scope, used with Bearer auth in the scraper.

- [#17582](https://github.com/emqx/emqx/pull/17582) Prometheus VM and Mnesia collector metric names now use the `prometheus.erl` 6.x promtool-compliant names.

  Affected metric renames:

  - `erlang_mnesia_failed_transactions` -> `erlang_mnesia_failed_transactions_total`
  - `erlang_mnesia_committed_transactions` -> `erlang_mnesia_committed_transactions_total`
  - `erlang_mnesia_logged_transactions` -> `erlang_mnesia_logged_transactions_total`
  - `erlang_mnesia_restarted_transactions` -> `erlang_mnesia_restarted_transactions_total`
  - `erlang_vm_memory_atom_bytes_total` -> `erlang_vm_memory_atom_bytes`
  - `erlang_vm_memory_bytes_total` -> `erlang_vm_memory_bytes`
  - `erlang_vm_memory_processes_bytes_total` -> `erlang_vm_memory_processes_bytes`
  - `erlang_vm_memory_system_bytes_total` -> `erlang_vm_memory_system_bytes`
  - `erlang_vm_statistics_context_switches` -> `erlang_vm_statistics_context_switches_total`
  - `erlang_vm_statistics_garbage_collection_number_of_gcs` -> `erlang_vm_statistics_garbage_collection_number_of_gcs_total`
  - `erlang_vm_statistics_garbage_collection_words_reclaimed` -> `erlang_vm_statistics_garbage_collection_words_reclaimed_total`
  - `erlang_vm_statistics_garbage_collection_bytes_reclaimed` -> `erlang_vm_statistics_garbage_collection_bytes_reclaimed_total`
  - `erlang_vm_statistics_runtime_milliseconds` -> `erlang_vm_statistics_runtime_seconds_total`
  - `erlang_vm_statistics_wallclock_time_milliseconds` -> `erlang_vm_statistics_wallclock_time_seconds_total`
  - `erlang_vm_port_count` -> `erlang_vm_ports`
  - `erlang_vm_process_count` -> `erlang_vm_processes`
  - `erlang_vm_atom_count` -> `erlang_vm_atoms`

- [#17596](https://github.com/emqx/emqx/pull/17596) Added authorization options that forbid interpolation of `/`, `+`, and `#` symbols into topic filter templates in authorization rules. The new options are:

  ```hocon
  authorization.topic_template_allow {
    plus = false,
    hash = false,
    slash = false
  }
  ```

  With `false`, the corresponding symbol cannot be used in a value interpolated into a topic template. For example, if `plus = false`, then username `bad+user` is forbidden in a rule such as `{allow, all, publish, ["userspace/${username}"]}`. The outcome depends on the active security profile: with the legacy profile the rule will not match, and with the hardened profile the action will be denied.

- [#17677](https://github.com/emqx/emqx/pull/17677) Dropped support for the JSON output format in the Prometheus REST API.

  The endpoints under `/api/v5/prometheus` (`stats`, `auth`, `data_integration`, `schema_validation`, `message_transformation`) now only produce the Prometheus text format. Requests sending `Accept: application/json` are rejected with `400 Bad Request` ("only prometheus format is supported"); previously they returned a JSON representation of the metrics.

- [#17626](https://github.com/emqx/emqx/pull/17626) [#18123](https://github.com/emqx/emqx/pull/18123) Added a new configuration `multi_tenancy.deny_namespaces` holding namespace names that cannot be used as a namespace identifier, either as an admin namespace (dashboard roles, API keys, multi-tenancy management API) or as a per-client `client_attrs.tns`; a client whose `client_attrs.tns` resolves to a denied name is rejected.

  This is a breaking change: the default value `["global", "undefined", "null", "none"]` denies names that were previously accepted. These names collide with internal sentinels and would produce ambiguous log lines and dashboard output. Existing namespaces with these names are not migrated; rename them before upgrading, or set `multi_tenancy.deny_namespaces` to an empty list to lift the restriction.

  Additionally, when `multi_tenancy.post_auth_tns_expression` is configured and evaluates to an empty value or fails to evaluate, a client whose pre-authentication `client_attrs.tns` is a denied namespace name is now also rejected, consistent with the handling when the expression evaluates to a non-empty value.

- [#18228](https://github.com/emqx/emqx/pull/18228) The default authorization rules file (`acl.conf`) no longer grants clients connecting from `127.0.0.1` blanket publish/subscribe access to all topics (including `$SYS/#` and `#`).

  Clients connecting from localhost are now authorized by the same rules as any other client, and ultimately by the `authorization.no_match` setting. In particular, subscriptions to `$SYS/#` and the wildcard filters `#` and `+/#` are now denied for localhost clients by the default rules, regardless of the security profile.

  Deployments that relied on the built-in localhost allowance must add an explicit rule to `acl.conf`. The previous rule is retained in the file as a comment for easy re-enabling:

  ```erlang
  %% {allow, {ipaddr, "127.0.0.1"}, all, ["$SYS/#", "#"]}.
  ```

  Note: this applies to new installations and deployments that have not customized `acl.conf`; existing customized `acl.conf` files are not modified by upgrades.

- [#18244](https://github.com/emqx/emqx/pull/18244) The ExProto gateway has been removed.

- [#18271](https://github.com/emqx/emqx/pull/18271) [#18329](https://github.com/emqx/emqx/pull/18329) MQTT and gateway WebSocket listeners no longer read the client address and port from forwarded headers by default: the default value of `proxy_address_header` and `proxy_port_header` changed from `x-forwarded-for` / `x-forwarded-port` to empty, meaning the socket source address and port are always used. Deployments behind load balancers or reverse proxies that rely on forwarded headers must now configure the header names explicitly (for example, set `proxy_address_header` to `x-forwarded-for`). Setting an empty header name disables the forwarded-header lookup.

  This change also fixes the forwarded-header lookup for gateway WebSocket listeners. Previously, a configured header name was never matched against the request headers, so the socket source address and port were used even when the forwarded headers were present.

- [#18377](https://github.com/emqx/emqx/pull/18377) Managed namespace names are now validated when created. A name may contain only ASCII letters, digits, and the characters `.`, `-`, and `_`, with a length of 1 to 255 bytes; the names `.` and `..` are not accepted. Namespaces that already exist are not affected.

- [#18390](https://github.com/emqx/emqx/pull/18390) The `mqtt.clientid_override` expression no longer falls back to the client-supplied Client ID when it fails.

  When `mqtt.clientid_override` is configured and the expression raises an error (for example, it references an attribute the client did not provide) or renders an empty string, EMQX now refuses the connection with CONNACK reason code 0x85 (Client Identifier not valid; return code 2 for MQTT 3.1 and 3.1.1 clients). Previously such clients stayed connected under their original Client ID, so the override silently did not apply to them.

  Before upgrading, verify that every connecting client can render the configured expression to a non-empty string. Clients that could not render the expression connected with their original Client ID before the upgrade; after the upgrade they are refused until the expression or the client data is fixed.

- [#18419](https://github.com/emqx/emqx/pull/18419) Removed the Google Cloud IoT Core migration compatibility feature, including the GCP Device authenticator and device management APIs.

- [#18515](https://github.com/emqx/emqx/pull/18515) Updated the Azure Blob Storage Action's `blob` template field to use the same schema validation as the Aggregated S3 Action's `key` field. The validation rejects unsupported template bindings.

- [#18528](https://github.com/emqx/emqx/pull/18528) Added validation that requires the exporter endpoint of an OpenTelemetry integration to be a URL with an explicit scheme and port. Supported schemes are `http` and `https`.

- [#18627](https://github.com/emqx/emqx/pull/18627) Dashboard SAML SSO now verifies IdP signatures by default in all security profiles.

  Previously the default followed the security profile: the hardened profile verified signatures, but the legacy profile (the default until v7.0) did not, so it accepted an unsigned, forged SAMLResponse and issued a Dashboard session.

  If you intentionally run an unsigned IdP, set `sso.saml.idp_signs_envelopes = false` and `sso.saml.idp_signs_assertions = false` explicitly. If the IdP does sign but its metadata carries no certificate, the SAML backend now fails to start with `missing_idp_certificate`.
