# Version 5

## 5.8.0

*Release Date: 2024-08-28*

### Enhancements

#### Core MQTT Functionality

- [#13009](https://github.com/emqx/emqx/pull/13009) Updated the log level for message receiving pause due to rate limiting from `debug` to `warning`. The log message `socket_receive_paused_by_rate_limit` is throttled to avoid excessive logging.

#### Authentication and Authorization

- [#12418](https://github.com/emqx/emqx/pull/12418) Enhanced JWT authentication to support claims verification using a list of objects:
  
  ```
  [
    {
      name = "claim_name",
      value = "${username}"
    },
    ...
  ]
  ```
  
  Expected values are now treated as templates, consistent with other authenticators, allowing for arbitrary expressions such as `${username}` and `${clientid}`. Previousy, only fixed `"${username}"` `"${clientid}"` values were supported for interpolation.
  
  Improved the documentation for the `verify_claims` parameter.
  
- [#13534](https://github.com/emqx/emqx/pull/13534) Added trace logging to indicate when the superuser bypasses the authorization check.

#### Data Integrations

- [#13144](https://github.com/emqx/emqx/pull/13144) Changed the log level to `warning` and added throttling for the log message `data_bridge_buffer_overflow` when bridge buffers overflow and messages are dropped. Previously, these events were logged at the `info` level and were not visible with the default log settings.

- [#13492](https://github.com/emqx/emqx/pull/13492) Enhanced the `GET /connectors` and `GET /connectors/:id` APIs to include lists of actions and sources that depend on a specific connector. Additionally, the `GET /actions`, `GET /sources`, `GET /actions/:id`, and `GET /sources/:id` APIs now return the list of rules associated with a specific action or source.

- [#13505](https://github.com/emqx/emqx/pull/13505) Added the ability to filter rules in the HTTP API based on the IDs of data integration actions or sources used.

- [#13506](https://github.com/emqx/emqx/pull/13506) Introduced the `peername` field to all rule engine events that already include the `peerhost` field. The `peername` field is a string formatted as `IP:PORT`.

- [#13516](https://github.com/emqx/emqx/pull/13516) Added a `direct_dispatch` argument to the `republish` action.

  When `direct_dispatch` is set to `true` (or rendered as `true` from template) the message is dispatched directly to subscribers. This feature helps prevent the triggering of additional rules or the recursive activation of the same rule.

- [#13573](https://github.com/emqx/emqx/pull/13573) Introduced `client_attrs` to the SQL context for client connectivity events and the message `publish` event.
  Users can now access client attributes within rule SQL statements, such as `SELECT client_attrs.attr1 AS attribute1`, and utilize `${attribute1}` in data integration actions.

- [#13640](https://github.com/emqx/emqx/pull/13640) Added two new SQL functions for rules: `coalesce/2` and `coalesce_ne/2`.

  These functions simplify handling null values in rule SQL expressions. For instance, instead of using:

  ```
  SELECT
    CASE
      WHEN is_null(payload.path.to.value) THEN
        0
      ELSE
        payload.path.to.value
    END AS my_value
  ```

  you can now write a more concise expression: `SELECT coalesce(payload.path.to.value, 0) AS my_value`.


#### Operations

- [#13202](https://github.com/emqx/emqx/pull/13202) Introduced the `emqx_cli conf cluster_sync fix` command to address cluster configuration inconsistencies. This command synchronizes the configuration of all nodes with the configuration of the node that has the highest `tnx_id`, ensuring consistency across the cluster.

- [#13250](https://github.com/emqx/emqx/pull/13250) Added a new value for `cluster.discovery_strategy`: `singleton`.  By choosing this option, there will be effectively no clustering, and the node will reject connection attempts to and from other nodes.

- [#13370](https://github.com/emqx/emqx/pull/13370) Added a new version of `wildcard_optimized` storage layout for durable storage, offering the following improvements:
  - The new layout does not have an inherent latency.

  - MQTT messages are serialized into a more space-efficient format.

- [#13524](https://github.com/emqx/emqx/pull/13524) Added the `emqx ctl exclusive` CLI interface to manage exclusive topics more effectively. It allows administrators to better manage and troubleshoot exclusive topic subscriptions, ensuring that subscription states are accurately reflected and preventing unexpected failures.

- [#13597](https://github.com/emqx/emqx/pull/13597) Added thin wrapper functions for plugins to store and manage the certificate files used by the plugins themselves. This fix prevents plugin certificates from being inadvertently deleted by the certificate garbage collection (GC) function.

- [#13626](https://github.com/emqx/emqx/pull/13626) Added a new command `emqx ctl listeners enable <Identifier> <Bool>` to enable/disable a listener.

- [#13493](https://github.com/emqx/emqx/pull/13493) Upgraded the RPC library `gen_rpc` to version 3.4.0. This update changes the default RPC server socket option from `true` to `active-100`, which introduces back-pressure to peer nodes when the RPC server experiences heavy load. 

- [#13665](https://github.com/emqx/emqx/pull/13665) Added a new metric `emqx_actions_count` to the prometheus endpoint. It contains the number of all actions added by all rules, including Republish actions and Console Output actions.

### Bug Fixes

#### Core MQTT Functionality

- [#12944](https://github.com/emqx/emqx/pull/12944) Fixed an issue that caused a crash when clients with non-UTF8 client IDs attempted to connect with `strict_mode=false`.

- [#13006](https://github.com/emqx/emqx/pull/13006) Improved the validation of retained, delayed, and taken-over session messages to ensure they comply with banned client ID rules implemented through regular expression matching. Previously, certain messages, such as those delayed due to network issues or taken over by another session, could bypass the client ID bans set by regular expressions.

#### Authentication and Authorization

- [#13024](https://github.com/emqx/emqx/pull/13024) Added a default ACL deny rule to reject subscriptions to the `+/#` topic pattern. Since EMQX by default rejects subscriptions to `#` topic, for completeness, it should reject `+/#` as well.

- [#13040](https://github.com/emqx/emqx/pull/13040) Improved HTTP authentication:
  * Improved error logging for cases where the HTTP `Content-Type` header is missing or unrecognized, providing more detailed information.
  * Fixed an issue causing double encoding of query parameters in authentication HTTP requests
  * Enhanced error messages when a POST method with a JSON content type is configured for authentication requests but the JSON template fails to render into valid JSON. This can occur, for example, when a template contains a placeholder like `${password}` but receives a non-UTF8 password input, leading to better transparency and easier debugging for such scenarios.

- [#13196](https://github.com/emqx/emqx/pull/13196) Added a limit to the built-in authorization database, restricting the number of Access Control List (ACL) rules per client or user to a default of 100.

- [#13584](https://github.com/emqx/emqx/pull/13584) Fixed an issue when creating HTTP authorization with empty HTTP header list.

- [#13618](https://github.com/emqx/emqx/pull/13618) Enhanced type specification for the `authorization/sources` endpoint.

- [#13624](https://github.com/emqx/emqx/pull/13624) Fixed an issue when updating rules in the built-in authorizer for a client/user could lead to the total number of rules exceeding the `max_rules` limit.

- [#13678](https://github.com/emqx/emqx/pull/13678) This fix makes the deletion of authenticator in the chain an idempotent operation, meaning, deleting a non-existing authenticator always succeeeds.

#### Data Integrations

- [#13207](https://github.com/emqx/emqx/pull/13207) Improved the `republish` rule engine action to accurately reflect the success and failure of message publishing. Previously, the success metrics were incremented even when the republish action failed to deliver the message to any subscribers. Now, if the action detects that a message fails to reach any subscriber, the failure metrics are correctly incremented.

- [#13425](https://github.com/emqx/emqx/pull/13425) Improved the MQTT connector error log messages to provide clearer and more detailed information.

- [#13589](https://github.com/emqx/emqx/pull/13589) Fixed an issue where creating a rule with a string `"null"` for ID via the HTTP API was allowed, which could lead to an inconsistent configuration.

#### Operations

- [#13078](https://github.com/emqx/emqx/pull/13078) Improved validation and error handling in the EMQX Management API to ensure that requests with a JSON body include the `Content-Type: application/json` header. If the header is missing for APIs that expect JSON input, the server now correctly responds with a `415 Unsupported Media Type` status code instead of `400 Bad Request`.

- [#13225](https://github.com/emqx/emqx/pull/13225) Enhanced security in authentication and authorization APIs by redacting sensitive data such as passwords. Previously, the APIs could return the original password values in responses. With this update, sensitive information is replaced with `******` to prevent accidental exposure and protect user credentials.

#### Gateways

- [#13607](https://github.com/emqx/emqx/pull/13607) Fixed an issue where the QoS level for CoAP subscriptions displayed through the API did not match the actual QoS level being used. This discrepancy could cause confusion as successful subscriptions were not accurately reflected on the Dashboard.

## 5.7.2

*Release Date: 2024-08-07*

### Enhancements

- [#13317](https://github.com/emqx/emqx/pull/13317) Added a new per-authorization source metric type: `ignore`.  This metric increments when an authorization source attempts to authorize a request but encounters scenarios where the authorizer is not applicable or encounters an error, resulting in an undecidable outcome.


- [#13336](https://github.com/emqx/emqx/pull/13336) Added functionality to initialize authentication data in the built-in database of an empty EMQX node or cluster using a bootstrap file in CSV or JSON format. This feature introduces new configuration entries, `bootstrap_file` and `bootstrap_type`.

- [#13348](https://github.com/emqx/emqx/pull/13348) Added a new field `payload_encode` in the log configuration to determine the format of the payload in the log data. 

- [#13436](https://github.com/emqx/emqx/pull/13436) Added the option to add custom request headers to JWKS requests.

- [#13507](https://github.com/emqx/emqx/pull/13507) Introduced a new built-in function `getenv` in the rule engine and variform expression to facilitate access to environment variables. This function adheres to the following constraints:

  - Prefix `EMQXVAR_` is added before reading from OS environment variables. For example, `getenv('FOO_BAR')` is to read `EMQXVAR_FOO_BAR`.
  - These values are immutable once loaded from the OS environment.

- [#13521](https://github.com/emqx/emqx/pull/13521) Resolved an issue where LDAP query timeouts could cause the underlying connection to become unusable, potentially causing subsequent queries to return outdated results. The fix ensures the system reconnects automatically in case of a timeout.

- [#13528](https://github.com/emqx/emqx/pull/13528) Applied log throttling for the event of unrecoverable errors in data integrations.

- [#13548](https://github.com/emqx/emqx/pull/13548) EMQX now can optionally invoke the `on_config_changed/2` callback function when the plugin configuration is updated via the REST API. This callback function is assumed to be exported by the `<PluginName>_app` module.
  For example, if the plugin name and version are `my_plugin-1.0.0`, then the callback function is assumed to be `my_plugin_app:on_config_changed/2`.

- [#13386](https://github.com/emqx/emqx/pull/13386) Added support for initializing a list of banned clients on an empty EMQX node or cluster with a bootstrap file in CSV format. The corresponding config entry to specify the file path is `banned.bootstrap_file`. This file is a CSV file with `,` as its delimiter. The first line of this file must be a header line. All valid headers are listed here:

  - as :: required
  - who :: required
  - by  :: optional
  - reason :: optional
  - at :: optional
  - until :: optional

  See the [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) for details on each field.

  Each row in the rest of this file must contain the same number of columns as the header line, and the column can be omitted then its value is `undefined`.

### Bug Fixes

- [#13222](https://github.com/emqx/emqx/pull/13222) Resolved issues with flags checking and error handling associated with the Will message in the `CONNECT` packet.
  For detailed specifications, refer to:
  
  - MQTT-v3.1.1-[MQTT-3.1.2-13], MQTT-v5.0-[MQTT-3.1.2-11]
  - MQTT-v3.1.1-[MQTT-3.1.2-14], MQTT-v5.0-[MQTT-3.1.2-12]
  - MQTT-v3.1.1-[MQTT-3.1.2-15], MQTT-v5.0-[MQTT-3.1.2-13]
  
- [#13307](https://github.com/emqx/emqx/pull/13307) Updated `ekka` library to version 0.19.5. This version of `ekka` utilizes `mria` 0.8.8, enhancing auto-heal functionality. Previously, the auto-heal worked only when all core nodes were reachable. This update allows to apply auto-heal once the majority of core nodes are alive. For details, refer to the [Mria PR](https://github.com/emqx/mria/pull/180).

- [#13334](https://github.com/emqx/emqx/pull/13334) Implemented strict mode checking for the `PasswordFlag` in the MQTT v3.1.1 CONNECT packet to align with protocol specifications.

  Note: To ensure bug-to-bug compatibility, this check is performed only in strict mode.

- [#13344](https://github.com/emqx/emqx/pull/13344) Resolved an issue where the `POST /clients/:clientid/subscribe/bulk` API would not function correctly if the node receiving the API request did not maintain the connection to the specified `clientid`.

- [#13358](https://github.com/emqx/emqx/pull/13358) Fixed an issue when the `reason` in the `authn_complete_event` event was incorrectly displayed.
- [#13375](https://github.com/emqx/emqx/pull/13375) The value `infinity` has been added as default value to the listener configuration fields `max_conn_rate`, `messages_rate`, and `bytes_rate`.

- [#13382](https://github.com/emqx/emqx/pull/13382) Updated the `emqtt` library to version 0.4.14, which resolves an issue preventing `emqtt_pool`s from reusing pools that are in an inconsistent state.

- [#13389](https://github.com/emqx/emqx/pull/13389) Fixed an issue where the `Derived Key Length` for `pbkdf2` could be set to a negative integer.

- [#13389](https://github.com/emqx/emqx/pull/13389) Fixed an issue where topics in the authorization rules might be parsed incorrectly.

- [#13393](https://github.com/emqx/emqx/pull/13393) Fixed an issue where plugin applications failed to restart after a node joined a cluster, resulting in hooks not being properly installed and causing inconsistent states.

- [#13398](https://github.com/emqx/emqx/pull/13398) Fixed an issue where ACL rules were incorrectly cleared when reloading the built-in database for authorization using the command line.

- [#13403](https://github.com/emqx/emqx/pull/13403) Addressed a security issue where environment variable configuration overrides were inadvertently logging passwords. This fix ensures that passwords present in environment variables are not logged.

- [#13408](https://github.com/emqx/emqx/pull/13408) Resolved a `function_clause` crash triggered by authentication attempts with invalid salt or password types. This fix enhances error handling to better manage authentication failures involving incorrect salt or password types.

- [#13419](https://github.com/emqx/emqx/pull/13419) Resolved an issue where crash log messages from the `/configs` API were displaying garbled hints. This fix ensures that log messages related to API calls are clear and understandable.

- [#13422](https://github.com/emqx/emqx/pull/13422) Fixed an issue where the option `force_shutdown.max_heap_size` could not be set to 0 to disable this tuning.

- [#13442](https://github.com/emqx/emqx/pull/13442) Fixed an issue where the health check interval configuration for actions/sources was not being respected. Previously, EMQX ignored the specified health check interval for actions and used the connector's interval instead. The fix ensures that EMQX now correctly uses the health check interval configured for actions/sources, allowing for independent and accurate health monitoring frequencies.

- [#13503](https://github.com/emqx/emqx/pull/13503) Fixed an issue where connectors did not adhere to the configured health check interval upon initial startup, requiring an update or restart to apply the correct interval.

- [#13515](https://github.com/emqx/emqx/pull/13515) Fixed an issue where the same client could not subscribe to the same exclusive topic when the node was down for some reason.

- [#13527](https://github.com/emqx/emqx/pull/13527) Fixed an issue in the Rule Engine where executing a SQL test for the Message Publish event would consistently return no results when a `$bridges/...` source was included in the `FROM` clause.

- [#13541](https://github.com/emqx/emqx/pull/13541) Fixed an issue where disabling CRL checks for a listener required a listener restart to take effect.
- [#13552](https://github.com/emqx/emqx/pull/13552) Added a startup timeout limit for EMQX plugins with a default timeout of 10 seconds. Before this update, problematic plugins could cause runtime errors during startup, leading to potential issues where the main startup process might hang when EMQX is stopped and restarted.

## 5.7.1

*Release Date: 2024-06-26*

### Enhancements

- [#12983](https://github.com/emqx/emqx/pull/12983) Add new rule engine event `$events/client_check_authn_complete` for authentication completion event.

- [#13180](https://github.com/emqx/emqx/pull/13180) Improved client message handling performance when EMQX is running on Erlang/OTP 26 and increased message throughput by 10% in fan-in mode.

- [#13191](https://github.com/emqx/emqx/pull/13191) Upgraded EMQX Docker images to run on Erlang/OTP 26.

  EMQX had been running on Erlang/OTP 26 since v5.5 except for docker images which were on Erlang/OTP 25. Now all releases are on Erlang/OTP 26.

- [#13242](https://github.com/emqx/emqx/pull/13242) Significantly increased the startup speed of EMQX dashboard listener.

### Bug Fixes

- [#13156](https://github.com/emqx/emqx/pull/13156) Resolved an issue where the Dashboard Monitoring pages would crash following the update to EMQX v5.7.0. 

- [#13164](https://github.com/emqx/emqx/pull/13164) Fixed HTTP authorization request body encoding.

  Before this fix, the HTTP authorization request body encoding format was taken from the `accept` header. The fix is to respect the `content-type` header instead. Also added `access` templating variable for v4 compatibility. The access code of SUBSCRIBE action is `1` and PUBLISH action is `2`.

- [#13238](https://github.com/emqx/emqx/pull/13238) Improved the logged error messages when an HTTP authorization request with an unsupported content-type header is returned.

- [#13258](https://github.com/emqx/emqx/pull/13258) Fix an issue where the MQTT-SN gateway would not restart correctly due to incorrect startup order of gateway dependencies.

- [#13273](https://github.com/emqx/emqx/pull/13273) Fixed and improved handling of URIs in several configurations. The fix includes the following improvement details:

  * Authentication and authorization configurations: Corrected a previous error where valid pathless URIs such as `https://example.com?q=x` were mistakenly rejected. These URIs are now properly recognized as valid.
  * Connector configurations: Enhanced checks to ensure that URIs with potentially problematic components, such as user info or fragment parts, are no longer erroneously accepted. 

- [#13276](https://github.com/emqx/emqx/pull/13276) Fixed an issue in the durable message storage mechanism where parts of the internal storage state were not correctly persisted during the setup of new storage generations. The concept of "generation" is used internally and is crucial for managing message expiration and cleanup. This could have manifested as messages being lost after a restart of EMQX.

- [#13291](https://github.com/emqx/emqx/pull/13291) Fixed an issue where durable storage sites that were down being reported as up.

- [#13290](https://github.com/emqx/emqx/pull/13290) Fixed an issue where the command `$ bin/emqx ctl rules show rule_0hyd` would produce no output when used to display rules with a data integration action attached.

- [#13293](https://github.com/emqx/emqx/pull/13293) Improved the restoration process from data backups by automating the re-indexing of imported retained messages. Previously, re-indexing required manual intervention using the `emqx ctl retainer reindex start` CLI command after importing a data backup file. 

  This fix also extended the functionality to allow exporting retained messages to a backup file when the `retainer.backend.storage_type` is configured as `ram`. Previously, only setups with `disc` as the storage type supported exporting retained messages.

- [#13140](https://github.com/emqx/emqx/pull/13140) Fixed an issue that caused text traces for the republish action to crash and not display correctly.

- [#13148](https://github.com/emqx/emqx/pull/13148) Fixed an issue where a 500 HTTP status code could be returned by `/connectors/:connector-id/start` when there is a timeout waiting for the resource to be connected.

- [#13181](https://github.com/emqx/emqx/pull/13181) EMQX now forcefully shut down the connector process when attempting to stop a connector, if such operation times out. This fix also improved the clarity of error messages when disabling an action or source fails due to an unresponsive underlying connector.

- [#13216](https://github.com/emqx/emqx/pull/13216) Respect `clientid_prefix` config for MQTT bridges. Since EMQX v5.4.1, the MQTT client IDs are restricted to a maximum of 23 bytes. Previously, the system factored the `clientid_prefix` into the hash of the original, longer client ID, affecting the final shortened ID. The fix includes the following change details:

  - Without Prefix: The behavior remains unchanged. EMQX hashes the long client IDs (exceeding 23 bytes) to fit within the 23-byte limit.
  - With Prefix:
    - Prefix ≤ 19 bytes: The prefix is retained, and the remaining portion of the client ID is hashed into a 4-byte space, ensuring the total length does not exceed 23 bytes.
    - Prefix ≥ 20 bytes: EMQX will not attempt to shorten the client ID, fully preserving the configured prefix regardless of length.

## 5.7.0

*Release Date: 2024-05-27*

### Enhancements

#### Security

[#12947](https://github.com/emqx/emqx/pull/12947) For JWT authentication, support new `disconnect_after_expire` option. When enabled, the client will be disconnected after the JWT token expires.


Note: This is a breaking change. This option is enabled by default, so the default behavior is changed. Previously, the clients with actual JWTs could connect to the broker and stay connected even after the JWT token expired. Now, the client will be disconnected after the JWT token expires. To preserve the previous behavior, set `disconnect_after_expire` to `false`.

#### Data Processing and Integration

[#12671](https://github.com/emqx/emqx/pull/12671) An `unescape` function has been added to the rule engine SQL language to handle the expansion of escape sequences in strings. This addition has been done because string literals in the SQL language don't support any escape codes (e.g., `\n` and `\t`). This enhancement allows for more flexible string manipulation within SQL expressions.


#### Extensibility

- [#12872](https://github.com/emqx/emqx/pull/12872) Implemented the Client Attributes feature. It allows setting additional properties for each client using key-value pairs. Property values can be generated from MQTT client connection information (such as username, client ID, TLS certificate) or set from data accompanying successful authentication returns. Properties can be used in EMQX for authentication, authorization, data integration, and MQTT extension functions. Compared to using static properties like client ID directly, client properties offer greater flexibility in various business scenarios, simplifying the development process and enhancing adaptability and efficiency in development work.
  **Initialization of `client_attrs`**
  The `client_attrs` fields can be initially populated from one of the following `clientinfo` fields:
  
    - `cn`: The common name from the TLS client's certificate.
    - `dn`: The distinguished name from the TLS client's certificate, that is, the certificate "Subject".
    - `clientid`: The MQTT client ID provided by the client.
    - `username`: The username provided by the client.
    - `user_property`: Extract a property value from 'User-Property' of the MQTT CONNECT packet.
  
  **Extension through Authentication Responses**
  Additional attributes may be merged into `client_attrs` from authentication responses. Supported
  authentication backends include:
    - **HTTP**: Attributes can be included in the JSON object of the HTTP response body through a
      `client_attrs` field.
    - **JWT**: Attributes can be included via a `client_attrs` claim within the JWT.
  
  **Usage in Authentication and Authorization**
  If `client_attrs` is initialized before authentication, it can be used in external authentication
  requests. For instance, `${client_attrs.property1}` can be used within request templates
  directed at an HTTP server for authenticity validation.
    - The `client_attrs` can be utilized in authorization configurations or request templates, enhancing
      flexibility and control. Examples include: In `acl.conf`, use `{allow, all, all, ["${client_attrs.namespace}/#"]}` to apply permissions based on the `namespace` attribute.
    - In other authorization backends, `${client_attrs.namespace}` can be used within request templates to dynamically include client attributes.
  
  For more information about the Client Attributes feature, see [Client Attributes](../client-attributes/client-attributes.md).
  
- [#12910](https://github.com/emqx/emqx/pull/12910) Added plugin configuration management and schema validation. For EMQX enterprise edition, one can also annotate the schema with metadata to facilitate UI rendering in the Dashboard. See more details in the [plugin template](https://github.com/emqx/emqx-plugin-template/pull/126) and plugin [documentation](../extensions/plugins.md).

#### Operations and Management

<!-- This is not ready to GA in 5.7
- [#12798](https://github.com/emqx/emqx/pull/12798) Added new `GET /api/v5/clients_v2` API that uses cursors instead of page numbers for pagination.  This should be more efficient than the old API endpoint, which currently traverses tables multiple times.
-->
- [#12923](https://github.com/emqx/emqx/pull/12923) Provided more specific error when importing wrong format into builtin authenticate database.

- [#12940](https://github.com/emqx/emqx/pull/12940) Added `ignore_readonly` argument to `PUT /configs` API.
  Before this change, EMQX would return 400 (BAD_REQUEST) if the raw config included read-only root keys (`cluster`, `rpc`, and `node`).
  After this enhancement it can be called as `PUT /configs?ignore_readonly=true`, EMQX will in this case ignore readonly root config keys, and apply the rest. For observability purposes, an info level message is logged if any readonly keys are dropped.
  Also fixed an exception when config has bad HOCON syntax (returns 500). Now bad syntax will cause the API to return 400 (BAD_REQUEST).

- [#12957](https://github.com/emqx/emqx/pull/12957) Started building packages for macOS 14 (Apple Silicon) and Ubuntu 24.04 Noble Numbat (LTS).

### Bug Fixes

#### Security

- [#12887](https://github.com/emqx/emqx/pull/12887) Fixed MQTT enhanced auth with sasl scram.

- [#12962](https://github.com/emqx/emqx/pull/12962) TLS clients can now verify server hostname against wildcard certificate. For example, if a certificate is issued for host `*.example.com`, TLS clients is able to verify server hostnames like `srv1.example.com`.

#### MQTT

- [#12996](https://github.com/emqx/emqx/pull/12996) Fixed process leak in `emqx_retainer` application. Previously, client disconnection while receiving retained messages could cause a process leak.


#### Data Processing and Integration

- [#12653](https://github.com/emqx/emqx/pull/12653) The rule engine function `bin2hexstr` now supports bitstring inputs with a bit size that is not divisible by 8. Such bitstrings can be returned by the rule engine function `subbits`.

- [#12657](https://github.com/emqx/emqx/pull/12657) The rule engine SQL-based language previously did not allow putting any expressions as array elements in array literals (only constants and variable references were allowed). This has now been fixed so that one can use any expressions as array elements.
  The following is now permitted, for example:
  ```bash
  select
  [21 + 21, abs(-abs(-2)), [1 + 1], 4] as my_array
  from "t/#"
  ```
  <!-- This is a fix for not new feature in this release
  - [#12707](https://github.com/emqx/emqx/pull/12707) Keep IP and port of the durable client sessions in the database.
  -->

- [#12932](https://github.com/emqx/emqx/pull/12932) Previously, if a HTTP action request received a 503 (Service Unavailable) status, it was marked as a failure and the request was not retried. This has now been fixed so that the request is retried a configurable number of times.

- [#12948](https://github.com/emqx/emqx/pull/12948) Fixed an issue where sensitive HTTP header values like `Authorization` would be substituted by `******` after updating a connector.

- [#13118](https://github.com/emqx/emqx/pull/13118) Fix a performance issue in the rule engine template rendering.

#### Observability

- [#12765](https://github.com/emqx/emqx/pull/12765) Make sure stats `subscribers.count` `subscribers.max` contains shared-subscribers. It only contains non-shared subscribers previously.


#### Operations and Management

- [#12812](https://github.com/emqx/emqx/pull/12812) Made resource health checks non-blocking operations.  This means that operations such as updating or removing a resource won't be blocked by a lengthy running health check.

- [#12830](https://github.com/emqx/emqx/pull/12830) Made channel (action/source) health checks non-blocking operations. This means that operations such as updating or removing an action/source data integration won't be blocked by a lengthy running health check.

  <!-- This is a fix for not new feature in this release
  - [#12874](https://github.com/emqx/emqx/pull/12874) Ensure consistency of the durable message replay when the subscriptions are modified before session reconnects:
    - Persistent sessions save inflight packet IDs for the received QoS2 messages.
    - Ensuring consistent behavior between persistent and non-persistent sessions regarding overlapping subscriptions.
    - List persistent subscriptions in the REST API.
    -->

- [#12993](https://github.com/emqx/emqx/pull/12993) Fixed listener config update API when handling an unknown zone.
  Before this fix, when a listener config is updated with an unknown zone, for example `{"zone": "unknown"}`, the change would be accepted, causing all clients to crash whens connected.
  After this fix, updating the listener with an unknown zone name will get a "Bad request" response.

- [#13012](https://github.com/emqx/emqx/pull/13012) The MQTT listerners config option `access_rules` has been improved in the following ways:
  * The listener no longer crash with an incomprehensible error message if a non-valid access rule is configured. Instead a configuration error is generated.
  * One can now add several rules in a single string by separating them by comma (for example, "allow 10.0.1.0/24, deny all").

- [#13041](https://github.com/emqx/emqx/pull/13041) Improved HTTP authentication error log message. If HTTP content-type header is missing for POST method, it now emits a meaningful error message instead of a less readable exception with stack trace.

- [#13077](https://github.com/emqx/emqx/pull/13077) This fix makes EMQX only read action configurations from the global configuration when the connector starts or restarts, and instead stores the latest configurations for the actions in the connector. Previously, updates to action configurations would sometimes not take effect without disabling and enabling the action. This means that an action could sometimes run with the old (previous) configuration even though it would look like the action configuration has been updated successfully.

- [#13090](https://github.com/emqx/emqx/pull/13090) Attempting to start an action or source whose connector is disabled will no longer attempt to start the connector itself.

#### Gateways

- [#12909](https://github.com/emqx/emqx/pull/12909) Fixed UDP listener process handling on errors or closure, The fix ensures the UDP listener is cleanly stopped and restarted as needed if these error conditions occur.

- [#13001](https://github.com/emqx/emqx/pull/13001) Fixed an issue where the syskeeper forwarder would never reconnect when the connection was lost.

- [#13010](https://github.com/emqx/emqx/pull/13010) Fixed the issue where the JT/T 808 gateway could not correctly reply to the REGISTER_ACK message when requesting authentication from the registration service failed.

## 5.6.1

*Release Date: 2024-04-18*

### Bug Fixes

- [#12759](https://github.com/emqx/emqx/pull/12759) EMQX now automatically removes invalid backup files that fail during upload due to schema validation errors. This fix ensures that only valid configuration files are displayed and stored, enhancing system reliability.

- [#12766](https://github.com/emqx/emqx/pull/12766) Renamed `message_queue_too_long` error reason to `mailbox_overflow`

  `mailbox_overflow`. The latter is consistent with the corresponding config parameter: `force_shutdown.max_mailbox_size`.

- [#12773](https://github.com/emqx/emqx/pull/12773) Upgraded HTTP client libraries.

  The HTTP client library (`gun-1.3`) incorrectly appended a `:portnumber` suffix to the `Host` header for
  standard ports (`http` on port 80, `https` on port 443). This could cause compatibility issues with servers or gateways performing strict `Host` header checks (e.g., AWS Lambda, Alibaba Cloud HTTP gateways), leading to errors such as `InvalidCustomDomain.NotFound` or "The specified CustomDomain does not exist."

- [#12802](https://github.com/emqx/emqx/pull/12802) Improved how EMQX handles node removal from clusters via the `emqx ctl cluster leave` command. Previously, nodes could unintentionally rejoin the same cluster (unless it was stopped) if the configured cluster `discovery_strategy` was not `manual`. With the latest update, executing the `cluster leave` command now automatically disables cluster discovery for the node, preventing it from rejoining. To re-enable cluster discovery, use the `emqx ctl discovery enable` command or simply restart the node.

- [#12814](https://github.com/emqx/emqx/pull/12814) Improved error handling for the `/clients/{clientid}/mqueue_messages` and `/clients/{clientid}/inflight_messages` APIs in EMQX. These updates address:

  - **Internal Timeout**: If EMQX fails to retrieve the list of Inflight or Mqueue messages within the default 5-second timeout, likely under heavy system load, the API will return 500 error with the response `{"code":"INTERNAL_ERROR","message":"timeout"}`, and log additional details for troubleshooting.
  - **Client Shutdown**: Should the client connection be terminated during an API call, the API now returns a 404 error, with the response `{"code": "CLIENT_SHUTDOWN", "message": "Client connection has been shutdown"}`. This ensures clearer feedback when client connections are interrupted.

- [#12824](https://github.com/emqx/emqx/pull/12824) Updated the statistics metrics `subscribers.count` and `subscribers.max` to include shared subscribers. Previously, these metrics accounted only for non-shared subscribers. 

- [#12826](https://github.com/emqx/emqx/pull/12826) Fixed issues related to the import functionality of source data integrations and retained messages in EMQX. Before this update:

  - The data integration sources specified in backup files were not being imported. This included configurations under the `sources.mqtt` category with specific connectors and parameters such as QoS and topics.
  - Importing the `mnesia` table for retained messages was not supported.

- [#12843](https://github.com/emqx/emqx/pull/12843) Fixed `cluster_rpc_commit` transaction ID cleanup procedure on replicator nodes after executing the `emqx ctl cluster leave` command. Previously, failing to properly clear these transaction IDs impeded configuration updates on the core node. 

- [#12885](https://github.com/emqx/emqx/pull/12885) Fixed an issue in EMQX where users were unable to view "Retained Messages" under the "Monitoring" menu in the Dashboard. 

  The "Retained messages" backend API uses the `qlc` library. This problem was due to a permission issue where the `qlc` library's `file_sorter` function tried to use a non-writable directory, `/opt/emqx`, to store temporary files, resulting from recent changes in directory ownership permissions in Docker deployments. 

  This update modifies the ownership settings of the `/opt/emqx` directory to `emqx:emqx`, ensuring that all necessary operations, including retained messages retrieval, can proceed without access errors. 

## 5.6.0

*Release Date: 2024-03-28*

### Enhancements

- [#12251](https://github.com/emqx/emqx/pull/12251) Optimized the performance of the RocksDB-based persistent sessions, achieving a reduction in  RAM usage and database request frequency. Key improvements include:

  - Introduced dirty session state to avoid frequent mria transactions.
  - Introduced an intermediate buffer for the persistent messages.
  - Used separate tracks of PacketIds for QoS1 and QoS2 messages.
  - Limited the number of continuous ranges of inflight messages to 1 per stream.

- [#12326](https://github.com/emqx/emqx/pull/12326) Enhanced session tracking with registration history. EMQX now has the capability to monitor the history of session registrations, including those that have expired. By configuring `broker.session_history_retain`, EMQX retains records of expired sessions for a specified duration.

  - **Session count API**: Use the API `GET /api/v5/sessions_count?since=1705682238` to obtain a count of sessions across the cluster that remained active since the given UNIX epoch timestamp (with seconds precision). This enhancement aids in analyzing session activity over time.

  - **Metrics expansion with cluster sessions gauge**: A new gauge metric, `cluster_sessions`, is added to better track the number of sessions within the cluster. This metric is also integrated into Prometheus for easy monitoring:

    ```
    # TYPE emqx_cluster_sessions_count gauge
    emqx_cluster_sessions_count 1234
    ```

    NOTE: Please consider this metric as an approximate estimation. Due to the asynchronous nature of data collection and calculation, exact precision may vary.

- [#12338](https://github.com/emqx/emqx/pull/12338) Introduced a time-based garbage collection mechanism to the RocksDB-based persistent session backend. This feature ensures more efficient management of stored messages, optimizing storage utilization and system performance by automatically purging outdated messages.

- [#12398](https://github.com/emqx/emqx/pull/12398) Exposed the `swagger_support` option in the Dashboard configuration, allowing for the enabling or disabling of the Swagger API documentation.

- [#12467](https://github.com/emqx/emqx/pull/12467) Started supporting cluster discovery using AAAA DNS record type.


- [#12483](https://github.com/emqx/emqx/pull/12483) Renamed `emqx ctl conf cluster_sync tnxid ID` to `emqx ctl conf cluster_sync inspect ID`.

  For backward compatibility, `tnxid` is kept, but considered deprecated and will be removed in 5.7.

- [#12499](https://github.com/emqx/emqx/pull/12499) Enhanced client banning capabilities with extended rules, including:

  * Matching `clientid` against a specified regular expression.
  * Matching client's `username` against a specified regular expression.
  * Matching client's peer address against a CIDR range.

  **Important Notice**: Implementing a large number of broad matching rules (not specific to an individual clientid, username, or host) may affect system performance. It's advised to use these extended ban rules judiciously to maintain optimal system efficiency.

- [#12509](https://github.com/emqx/emqx/pull/12509) Implemented API to re-order all authenticators / authorization sources.

- [#12517](https://github.com/emqx/emqx/pull/12517) Configuration files have been upgraded to accommodate multi-line string values, preserving indentation for enhanced readability and maintainability. This improvement utilizes `"""~` and `~"""` markers to quote indented lines, offering a structured and clear way to define complex configurations. For example:

  ```
  rule_xlu4 {
    sql = """~
      SELECT
        *
      FROM
        "t/#"
    ~"""
  }
  ```

  See [HOCON 0.42.0](https://github.com/emqx/hocon/releases/tag/0.42.0) release notes for details.

- [#12520](https://github.com/emqx/emqx/pull/12520) Implemented log throttling. The feature reduces the volume of logged events that could potentially flood the system by dropping all but the first occurance of an event within a configured time window.
  Log throttling is applied to the following log events that are critical yet prone to repetition:

  - `authentication_failure`
  - `authorization_permission_denied`
  - `cannot_publish_to_topic_due_to_not_authorized`
  - `cannot_publish_to_topic_due_to_quota_exceeded`
  - `connection_rejected_due_to_license_limit_reached`
  - `dropped_msg_due_to_mqueue_is_full`

- [#12561](https://github.com/emqx/emqx/pull/12561) Implemented HTTP APIs to get the list of client's in-flight and message queue (mqueue) messages. These APIs facilitate detailed insights and effective control over message queues and in-flight messaging, ensuring efficient message handling and monitoring.

  To get the first chunk of data:

  - `GET /clients/{clientid}/mqueue_messages?limit=100`
  - `GET /clients/{clientid}/inflight_messages?limit=100`

  Alternatively, for the first chunks without specifying a start position:

  - `GET /clients/{clientid}/mqueue_messages?limit=100&position=none`
  - `GET /clients/{clientid}/inflight_messages?limit=100&position=none`

  To get the next chunk of data:

  - `GET /clients/{clientid}/mqueue_messages?limit=100&position={position}`
  - `GET /clients/{clientid}/inflight_messages?limit=100&position={position}`

  Where `{position}` is a value (opaque string token) of `meta.position` field from the previous response.

  Ordering and Prioritization:

  - **Mqueue Messages**: These are prioritized and sequenced based on their queue order (FIFO), from higher to lower priority. By default, mqueue messages carry a uniform priority level of 0.
  - **In-Flight Messages**: Sequenced by the timestamp of their insertion into the in-flight storage, from oldest to newest.

- [#12590](https://github.com/emqx/emqx/pull/12590) Removed `mfa` meta data from log messages to improve clarity.

- [#12641](https://github.com/emqx/emqx/pull/12641) Improved text log formatter fields order. The new fields order is as follows:

  `tag` > `clientid` > `msg` > `peername` > `username` > `topic` > [other fields]

- [#12670](https://github.com/emqx/emqx/pull/12670) Added field `shared_subscriptions` to endpoint `/monitor_current` and `/monitor_current/nodes/:node`.

- [#12679](https://github.com/emqx/emqx/pull/12679) Upgraded docker image base from Debian 11 to Debian 12.

- [#12700](https://github.com/emqx/emqx/pull/12700) Started supporting "b" and "B" unit in bytesize hocon fields. For example, all three fields below will have the value of 1024 bytes:

  ```
  bytesize_field = "1024b"
  bytesize_field2 = "1024B"
  bytesize_field2 = 1024
  ```

- [#12719](https://github.com/emqx/emqx/pull/12719) The `/clients` API has been upgraded to accommodate queries for multiple `clientid`s and `username`s simultaneously, offering a more flexible and powerful tool for monitoring client connections. Additionally, this update introduces the capability to customize which client information fields are included in the API response, optimizing for specific monitoring needs.

  Examples of Multi-Client/Username Queries:

  - To query multiple clients by ID: `/clients?clientid=client1&clientid=client2`
  - To query multiple users: `/clients?username=user11&username=user2`
  - To combine multiple client IDs and usernames in one query: `/clients?clientid=client1&clientid=client2&username=user1&username=user2`

  Examples of Selecting Fields for the Response:

  - To include all fields in the response: `/clients?fields=all` (Note: Omitting the `fields` parameter defaults to returning all fields.)
  - To specify only certain fields: `/clients?fields=clientid,username`

- [#12381](https://github.com/emqx/emqx/pull/12381) Added new SQL functions: `map_keys()`, `map_values()`, `map_to_entries()`, `join_to_string()`, `join_to_string()`, `join_to_sql_values_string()`, `is_null_var()`, `is_not_null_var()`.

  For more information on the functions and their usage, refer to [Built-in SQL Functions](../data-integration/rule-sql-builtin-functions) the documentation.

- [#12336](https://github.com/emqx/emqx/pull/12336) Performance enhancement. Created a dedicated async task handler pool to handle client session cleanup tasks.


- [#12725](https://github.com/emqx/emqx/pull/12725) Implemented REST API to list the available source types.

- [#12746](https://github.com/emqx/emqx/pull/12746) Added `username` log field. If MQTT client is connected with a non-empty username the logs and traces will include `username` field.

- [#12785](https://github.com/emqx/emqx/pull/12785) Added `timestamp_format` configuration option to log handlers. This new option allows for the following settings:

  - `auto`: Automatically determines the timestamp format based on the log formatter being used.
    Utilizes `rfc3339` format for text formatters, and `epoch` format for JSON formatters.

  - `epoch`: Represents timestamps in microseconds precision Unix epoch format.

  - `rfc3339`: Uses RFC3339 compliant format for date-time strings. For example, `2024-03-26T11:52:19.777087+00:00`.


### Bug Fixes

- [#11868](https://github.com/emqx/emqx/pull/11868) Fixed a bug where will messages were not published after session takeover.

- [#12347](https://github.com/emqx/emqx/pull/12347) Implemented an update to ensure that messages processed by the Rule SQL for the MQTT egress data bridge are always rendered as valid, even in scenarios where the data is incomplete or lacks certain placeholders defined in the bridge configuration. This adjustment prevents messages from being incorrectly deemed invalid and subsequently discarded by the MQTT egress data bridge, as was the case previously.

  When variables in `payload` and `topic` templates are undefined, they are now rendered as empty strings instead of the literal `undefined` string.

- [#12472](https://github.com/emqx/emqx/pull/12472) Fixed an issue where certain read operations on `/api/v5/actions/` and `/api/v5/sources/` endpoints might result in a `500` error code during the process of rolling upgrades.

- [#12492](https://github.com/emqx/emqx/pull/12492) EMQX now returns the `Receive-Maximum` property in the `CONNACK` message for MQTT v5 clients, aligning with protocol expectations. This implementation considers the minimum value of the client's `Receive-Maximum` setting and the server's `max_inflight` configuration as the limit for the number of inflight (unacknowledged) messages permitted. Previously, the determined value was not sent back to the client in the `CONNACK` message.

- [#12500](https://github.com/emqx/emqx/pull/12500) The `GET /clients` and `GET /client/:clientid` HTTP APIs have been updated to include disconnected persistent sessions in their responses.

  NOTE: A current known issue with these enhanced API responses is that the total client count provided may exceed the actual number of clients due to the inclusion of disconnected sessions.

- [#12513](https://github.com/emqx/emqx/pull/12513) Changed the level of several flooding log events from `warning` to `info`.

- [#12530](https://github.com/emqx/emqx/pull/12530) Improved the error reporting for `frame_too_large` events and malformed `CONNECT` packet parsing failures. These updates now provide additional information, aiding in the troubleshooting process.

- [#12541](https://github.com/emqx/emqx/pull/12541) Introduced a new configuration validation step for autocluster by DNS records to ensure compatibility between `node.name` and `cluster.discover_strategy`. Specifically, when utilizing the `dns` strategy with either `a` or `aaaa` record types, it is mandatory for all nodes to use a (static) IP address as the host name.

- [#12562](https://github.com/emqx/emqx/pull/12562) Added a new configuration root: `durable_storage`. This configuration tree contains the settings related to the new persistent session feature.

- [#12566](https://github.com/emqx/emqx/pull/12566) Enhanced the bootstrap file for REST API keys:

  - Empty lines within the file are now skipped, eliminating the previous behavior of generating an error.

  - API keys specified in the bootstrap file are assigned the highest precedence. In cases where a new key from the bootstrap file conflicts with an existing key, the older key will be automatically removed to ensure that the bootstrap keys take effect without issue.

- [#12646](https://github.com/emqx/emqx/pull/12646) Fixed an issue with the rule engine's date-time string parser. Previously, time zone adjustments were only effective for date-time strings specified with second-level precision.

- [#12652](https://github.com/emqx/emqx/pull/12652) Fixed a discrepancy where the subbits functions with 4 and 5 parameters, despite being documented, were missing from the actual implementation. These functions have now been added.

- [#12663](https://github.com/emqx/emqx/pull/12663) Fixed an issue where the `emqx_vm_cpu_use` and `emqx_vm_cpu_idle` metrics, accessible via the Prometheus endpoint `/prometheus/stats`, were inaccurately reflecting the average CPU usage since the operating system boot. This fix ensures that these metrics now accurately represent the current CPU usage and idle, providing more relevant and timely data for monitoring purposes.

- [#12668](https://github.com/emqx/emqx/pull/12668) Refactored the SQL function `date_to_unix_ts()` by using `calendar:datetime_to_gregorian_seconds/1`.
  This change also added validation for the input date format.

- [#12672](https://github.com/emqx/emqx/pull/12672) Changed the process for generating the node boot configuration by incorporating the loading of `{data_dir}/configs/cluster.hocon`. Previously, changes to logging configurations made via the Dashboard and saved in `{data_dir}/configs/cluster.hocon` were only applied after the initial boot configuration was generated using `etc/emqx.conf`, leading to potential loss of some log segment files due to late reconfiguration.

  Now, both `{data_dir}/configs/cluster.hocon` and `etc/emqx.conf` are loaded concurrently, with settings from `emqx.conf` taking precedence, to create the boot configuration.

- [#12696](https://github.com/emqx/emqx/pull/12696) Fixed an issue where attempting to reconnect an action or source could lead to wrong error messages being returned in the HTTP API.

- [#12714](https://github.com/emqx/emqx/pull/12714) Fixed inaccuracies in several metrics reported by the `/prometheus/stats` endpoint of the Prometheus API. The correction applies to the following metrics:

  - `emqx_cluster_sessions_count`
  - `emqx_cluster_sessions_max`
  - `emqx_cluster_nodes_running`
  - `emqx_cluster_nodes_stopped`
  - `emqx_subscriptions_shared_count`
  - `emqx_subscriptions_shared_max`

  Additionally, this fix rectified an issue within the `/stats` endpoint concerning the `subscriptions.shared.count` and `subscriptions.shared.max` fields. Previously, these values failed to update promptly following a client's disconnection or unsubscription from a Shared-Subscription.

- [#12715](https://github.com/emqx/emqx/pull/12715) Fixed a crash that could occur during configuration updates if the connector for the ingress data integration source had active channels.

- [#12740](https://github.com/emqx/emqx/pull/12740) Fixed an issue when durable sessions could not be kicked out.

- [#12768](https://github.com/emqx/emqx/pull/12768) Addressed a startup failure issue in EMQX version 5.4.0 and later, particularly noted during rolling upgrades from versions before 5.4.0. The issue was related to the initialization of the routing schema when both v1 and v2 routing tables were empty.

  The node now attempts to retrieve the routing schema version in use across the cluster instead of using the v2 routing table by default when local routing tables are found empty at startup. This approach mitigates potential conflicts and reduces the chances of diverging routing storage schemas among cluster nodes, especially in a mixed-version cluster scenario. 

  If conflict is detected in a running cluster, EMQX writes instructions on how to manually resolve it in the log as part of the error message with `critical` severity. The same error message and instructions will also be written on standard error to make sure this message will not get lost even if no log handler is configured.

- [#12786](https://github.com/emqx/emqx/pull/12786) Added a strict check that prevents replicant nodes from connecting to core nodes running with a different version of EMQX application.
  This check ensures that during the rolling upgrades, the replicant nodes can only work when at least one core node is running the same EMQX release version.


## 5.5.1

*Release Date: 2024-03-06*

### Bug Fixes

- [#12471](https://github.com/emqx/emqx/pull/12471) Fixed an issue that data integration configurations failed to load correctly during upgrades from EMQX version 5.0.2 to newer releases.

- [#12598](https://github.com/emqx/emqx/pull/12598) Fixed an issue that users were unable to subscribe to or unsubscribe from shared topic filters via HTTP API.

  The affected APIs include:

  - `/clients/:clientid/subscribe`
  - `/clients/:clientid/subscribe/bulk`

  - `/clients/:clientid/unsubscribe`
  - `/clients/:clientid/unsubscribe/bulk`

- [#12601](https://github.com/emqx/emqx/pull/12601) Fixed an issue where logs of the LDAP driver were not being captured. Now, all logs are recorded at the `info` level.

- [#12606](https://github.com/emqx/emqx/pull/12606) The Prometheus API experienced crashes when the specified SSL certificate file did not exist in the given path. Now, when an SSL certificate file is missing, the `emqx_cert_expiry_at` metric will report a value of 0, indicating the non-existence of the certificate.

- [#12620](https://github.com/emqx/emqx/pull/12620) Redacted sensitive information in HTTP headers to exclude authentication and authorization credentials from `debug` level logs in the HTTP Server connector, mitigating potential security risks.

- [#12632](https://github.com/emqx/emqx/pull/12632) Fixed an issue where the rule engine's SQL built-in function `date_to_unix_ts` produced incorrect timestamp results for dates starting from March 1st on leap years.

## 5.5.0

*Release Date: 2024-02-01*

### Enhancements

- [#12085](https://github.com/emqx/emqx/pull/12085) EMQX has been upgraded to leverage the capabilities of OTP version 26.1.2-2. NOTE: Docker images are still built with OTP 25.3.2.

- [#12189](https://github.com/emqx/emqx/pull/12189) Enhanced the [ACL](../access-control/authn/jwt.md#access-control-list-optional) claim format in EMQX JWT authentication for greater versatility. The updated format now supports an array structure, aligning more closely with the file-based ACL rules.

  For example:

  ```json
  [
  {
    "permission": "allow",
    "action": "pub",
    "topic": "${username}/#",
    "qos": [0, 1],
    "retain": true
  },
  {
    "permission": "allow",
    "action": "sub",
    "topic": "eq ${username}/#",
    "qos": [0, 1]
  },
  {
    "permission": "deny",
    "action": "all",
    "topics": ["#"]
  }
  ]
  ```

  In this new format, the absence of a matching rule does not result in an automatic denial of the action. The authorization chain can advance to other configured authorizers if a match is not found in the JWT ACL. If no match is found throughout the chain, the final decision defers to the default permission set in `authorization.no_match`.

- [#12267](https://github.com/emqx/emqx/pull/12267) Added a new `timeout` parameter to the `cluster/:node/invite` interface, addressing the issue of default timeouts.
  The previously set 5-second default timeout often led to HTTP API call timeouts because joining an EMQX cluster usually requires more time.

  In addition, EMQX added a new API `/cluster/:node/invite_async` to support an asynchronous way to invite nodes to join the cluster and introduced a new `cluster/invitation` API to inspect the join status.

- [#12272](https://github.com/emqx/emqx/pull/12272) Introduced updates to the `retain` API in EMQX:

  - Added a new API `DELETE /retainer/messages` to clean all retained messages.
  - Added an optional topic filter parameter `topic` in the query string for the API `GET /retainer/messages`. For example, using a query string `topic=t/1` filters the retained messages for a specific topic, improving the efficiency of message retrieval.

- [#12277](https://github.com/emqx/emqx/pull/12277) Added `mqtt/delayed/messages/:topic` API to remove delayed messages by topic name.

- [#12278](https://github.com/emqx/emqx/pull/12278) Adjusted the maximum pagination size for paginated APIs in the REST API from `3000` to `10000`.

- [#12289](https://github.com/emqx/emqx/pull/12289) Authorization caching now supports the exclusion of specific topics. For the specified list of topics and topic filters, EMQX will not generate an authorization cache. The list can be set through the `authorization.cache.excludes` configuration item or via the Dashboard. For these specific topics, permission checks will always be conducted in real-time rather than relying on previous cache results, thus ensuring the timeliness of authorization outcomes.

- [#12329](https://github.com/emqx/emqx/pull/12329) Added `broker.routing.batch_sync` configuration item to enable a dedicated process pool that synchronizes subscriptions with the global routing table in batches, thus reducing the frequency of cross-node communication that can be slowed down by network latency. Processing multiple subscription updates collectively, not only accelerates synchronization between replica nodes and core nodes in a cluster but also reduces the load on the broker pool, minimizing the risk of overloading.

- [#12333](https://github.com/emqx/emqx/pull/12333) Added a `tags` field for actions and connectors. Similar to the `description` field (which is a free text annotation), `tags` can be used to annotate actions and connectors for filtering and grouping.

- [#12299](https://github.com/emqx/emqx/pull/12299) Exposed more metrics to improve observability:

  Montior API:
  - Added `retained_msg_count` field to `/api/v5/monitor_current`.
  - Added `license_quota` field to `/api/v5/monitor_current`
  - Added `retained_msg_count` and `node_uptime` fields to `/api/v5/monitor_current/nodes/{node}`.
  - Added `retained_msg_count`, `license_quota` and `node_uptime` fields to `/api/v5/monitor_current/nodes/{node}`.

  Prometheus API:
  - Added `emqx_cert_expiry_at` and `emqx_license_expiry_at` to `/api/v5/prometheus/stats` to display TLS listener certificate expiration time and license expiration time.
  - Added `/api/v5/prometheus/auth` endpoint to provide metrics such as execution count and running status for all authenticatiors and authorizators.
  - Added `/api/v5/prometheus/data_integration` endpoint to provide metrics such as execution count and status for all rules, actions, and connectors.

  Limitations:
  Prometheus push gateway only supports the content in `/api/v5/prometheus/stats?mode=node`.

  For more API details and metric type information, please see swagger api docs.

- [#12196](https://github.com/emqx/emqx/pull/12196) Improved network efficiency during routes cleanup.

  Previously, when a node was down, a delete operation for each route to that node must be exchanged between all the other live nodes. After this change, only one `match and delete` operation is exchanged between all live nodes, significantly reducing the number of necessary network packets and decreasing the load on the inter-cluster network.
  This optimization must be especially helpful for geo-distributed EMQX deployments where network latency can be significantly high.

- [#12354](https://github.com/emqx/emqx/pull/12354) The concurrent creation and updates of data integrations are now supported, significantly increasing operation speeds, such as when importing backup files.

### Bug Fixes

- [#12232](https://github.com/emqx/emqx/pull/12232) Fixed an issue when cluster commit log table was not deleted after a node was forced to leave a cluster.

- [#12243](https://github.com/emqx/emqx/pull/12243) Fixed a family of subtle race conditions that could lead to inconsistencies in the global routing state.

- [#12269](https://github.com/emqx/emqx/pull/12269) Improved error handling in the `/clients` interface; now returns a 400 status with more detailed error messages, instead of a generic 500, for query string validation failures.

- [#12285](https://github.com/emqx/emqx/pull/12285) Updated the CoAP gateway to support short parameter names for slight savings in datagram size. For example, `clientid=bar` can be written as `c=bar`.

- [#12303](https://github.com/emqx/emqx/pull/12303) Fixed the message indexing in retainer. Previously, clients with wildcard subscriptions might receive irrelevant retained messages not matching their subscription topics.

- [#12305](https://github.com/emqx/emqx/pull/12305) Corrected an issue with incomplete client/connection information being passed into `emqx_cm`, which could lead to internal inconsistencies and affect memory usage and operations like node evacuation.

- [#12306](https://github.com/emqx/emqx/pull/12306) Fixed an issue preventing the connectivity test for the Connector from functioning correctly after updating the password parameter via the HTTP API.

- [#12359](https://github.com/emqx/emqx/pull/12359) Fixed an issue causing error messages when restarting a node configured with some types of data bridges.  Additionally, these bridges were at risk of entering a failed state upon node restart, requiring a manual restart to restore functionality.

- [#12404](https://github.com/emqx/emqx/pull/12404) Fixed an issue where restarting a data integration with heavy message flow could lead to a stop in the collection of data integration metrics.

## 5.4.1

*Release Date: 2024-01-09*


### Bug Fixes

- [#12234](https://github.com/emqx/emqx/pull/12234) Resolved compatibility issues with Open Telemetry configurations defined in `emqx.conf` from versions before EMQX 5.4.0, ensuring smooth integration of legacy configurations with the latest EMQX release.
- [#12236](https://github.com/emqx/emqx/pull/12236) Fixed client ID generation in MQTT broker data integration to comply with MQTT 3.1 specification of 23-byte limit. Client ID is now prefixed with user-assigned Connector name, followed by the first 8 bytes of node name's SHA hash and pool member ID. If the resulting ID exceeds 23 bytes, additional SHA hash and truncation for the first 23 characters are applied to ensure compliance.
- [#12238](https://github.com/emqx/emqx/pull/12238) Resolved compatibility issue with the error format configurations introduced in the HTTP Action feature of EMQX version 5.3.2.
- [#12246](https://github.com/emqx/emqx/pull/12246) Stopped exposing port 11883 by default in Docker and removed it from Helm charts, as this port is no longer in use.
- [#12249](https://github.com/emqx/emqx/pull/12249) Fixed an issue in the `/configs` API where attempting to modify a read-only configuration value resulted in a garbled response message.
- [#12264](https://github.com/emqx/emqx/pull/12264) Fixed an issue where version 5.4 replica nodes could not join clusters with core nodes running versions earlier than 5.4 during the rolling upgrade process.


## 5.4.0

*Release Date: 2023-12-23*

### Enhancements

- [#11884](https://github.com/emqx/emqx/pull/11884) Modified the Prometheus API and configuration to implement the following improvements:

  - Restructured configuration sections to group-related settings, improving readability and maintainability.
  - Introduced `enable_basic_auth` configuration for basic authentication on the scrape API endpoint, enhancing security.
  - Maintained backward compatibility while refactoring code, avoiding breaking changes.

- [#11896](https://github.com/emqx/emqx/pull/11896) Introduced an enhancement for configuring sensitive authentication fields in bridges, such as passwords, tokens, and secret keys. This improvement allows the use of secrets stored as files in the file system. These secrets can be securely referenced in configuration files using the special `file://` prefix, enhancing the security of sensitive data handling in bridge configurations.

- [#11921](https://github.com/emqx/emqx/pull/11921) Introduced Open Telemetry Logs Handler that allows to format log events in alignment with the Open Telemetry log data model. This handler facilitates the exportation of formatted log events to a configured Open Telemetry collector or back-end, thereby enhancing log management and integration capabilities.

- [#11935](https://github.com/emqx/emqx/pull/11935) Switched to the new `v2` routing store schema by default. The new schema improves both subscription and routing performance, especially in scenarios with concurrent subscriptions to topic filters sharing common wildcard prefixes. However, it does come with a minor increase in memory usage. This schema also eliminates the need for a separate index, thus inconsistencies in the routing state rarely encountered in previous versions should no longer be possible.

  If a cluster is rolling upgraded from an older version, the cluster will continue to use `v1` store until a full cluster (non-rolling) restart happens.

  Users can still opt for the previous schema by configuring the `broker.routing.storage_schema` option to `v1`. However, this also requires a complete, non-rolling restart of the cluster to take effect.

- [#11984](https://github.com/emqx/emqx/pull/11984) Implemented Open Telemetry distributed tracing feature.

- [#12017](https://github.com/emqx/emqx/pull/12017) Implemented a dedicated REST API for the import and export of configuration and user data.

- [#12040](https://github.com/emqx/emqx/pull/12040) Upgraded QUIC protocol stack.

- [#12201](https://github.com/emqx/emqx/pull/12201) Added support for hot updates to TCP/SSL/WS/WSS MQTT listener configurations. This feature allows you to modify most configuration parameters without restarting the listener and disconnecting the clients. However, there are some limitations:

  - For TCP/SSL listeners, changes to the following parameters will still require a listener restart and client reconnection:

    - `bind`
    - `tcp_options.backlog`

  - For WS/WSS (WebSocket) listeners, modifying transport-related parameters (listed below) will result in the listening socket being reopened, but established connections will remain uninterrupted.
    - `bind`
    - `tcp_options.*`

    - `ssl_options.*`

- [#11608](https://github.com/emqx/emqx/pull/11608) Integrated LDAP bind operation as a new authenticator, providing a more flexible and secure method for user authentication.

- [#11766](https://github.com/emqx/emqx/pull/11766) Implemented a preliminary Role-Based Access Control for the REST API. In this version, there are three predefined roles:

  - Administrator: This role can access all resources.

  - Viewer: This role can only view resources and data, corresponding to all GET requests in the REST API.

  - Publisher: Specifically tailored for MQTT message publishing, this role is confined to accessing endpoints related to message publication.

- [#11773](https://github.com/emqx/emqx/pull/11773) Implemented Dashboard support for audit log management. Users can utilize this page to view all change operations performed on EMQX devices and data, such as kicking out devices, creating/deleting rules, etc.

- [#11778](https://github.com/emqx/emqx/pull/11778) Integrated Microsoft Entra Identity (formerly known as Azure Active Directory) support into the SAML single sign-on (SSO) process.


- [#11811](https://github.com/emqx/emqx/pull/11811) Improved the format for the REST API key bootstrap file to support initializing key with a role.

  The new form is:`api_key:api_secret:role`.

  `role` is optional and its default value is `administrator`.

- [#11852](https://github.com/emqx/emqx/pull/11852) Introduced a new GB/T 32960 gateway, enabling vehicles to connect with EMQX via the GBT32960 vehicular networking protocol.

- [#11883](https://github.com/emqx/emqx/pull/11883) Introduced a new JT/T808 gateway, enabling vehicles to connect with EMQX via the JT/T 808 vehicular networking protocol.

- [#11885](https://github.com/emqx/emqx/pull/11885) Introduced a new OCPP gateway for Electric vehicle (EV) charging stations to access EMQX through the OCPP (Open Charge Point Protocol).

- [#11971](https://github.com/emqx/emqx/pull/11971) Made `/api/v5/load_rebalance/availability_check` public, meaning it no longer requires authentication. This change simplifies the setup of load balancers.

  It improved the gracefulness of the rebalance/evacuation process during the wait health check phase. The connections to nodes marked for eviction are now not prohibited during this phase.
  During this phase it is unknown whether these nodes are all marked unhealthy by the load balancer, so prohibiting connections to them may cause multiple unsuccessful reconnection attempts.

- [#12013](https://github.com/emqx/emqx/pull/12013) The data bridging design has been adjusted to split it into connectors and actions (Sinks). Connectors are used to manage the integration of data with external systems and can be reused across multiple actions, while actions are used to configure how data is processed. This design provides greater flexibility and scalability, resulting in clearer data integration configuration and management.

  The adjusted data bridges includes PostgreSQL, Timescale, and Matrix, which have now been split into connectors and actions APIs, but they remain backward compatible with the old data bridge API.

- [#12016](https://github.com/emqx/emqx/pull/12016) Enhanced license key management.

  EMQX can now load the license key from a specified file. This is enabled by setting the `license.key` configuration to a file path, which should be prefixed with `"file://"`.
  Also added the ability to revert to the default trial license by setting `license.key = default`. This option simplifies the process of returning to the trial license if needed.

- [#12129](https://github.com/emqx/emqx/pull/12129) Renewed the default license, replacing the old license issued in January 2023.  At the same time, the license capacity has been adjusted from 100 concurrent connections to 25 concurrent connections.

### Bug Fixes

- [#10976](https://github.com/emqx/emqx/pull/10976) Fixed topic-filter overlapping handling in shared subscription.
  In the previous implementation, the storage method for subscription options did not provide adequate support for shared subscriptions. This resulted in message routing failures and leakage of routing tables between nodes during the "subscribe-unsubscribe" process with specific order and topics.

- [#12048](https://github.com/emqx/emqx/pull/12048) Fixed COAP gateway bug that caused it to ignore subscription options.

- [#12078](https://github.com/emqx/emqx/pull/12078) Upgraded grpc-erl to 0.6.12. This update addresses a potential deadlock issue where the grpc client started dependent apps lazily.

- [#12081](https://github.com/emqx/emqx/pull/12081) Updated `gen_rpc` library to version 3.3.1. The new version includes several performance improvements:

  - Avoiding allocating extra memory for the packets before they are sent to the wire in some cases.

  - Bypassing network for the local calls.

  - Avoid senstive data leaking in debug logs [#12202](https://github.com/emqx/emqx/pull/12202)
- [#12111](https://github.com/emqx/emqx/pull/12111) Fixed an issue when API tokens were sometimes unavailable immediately after login due to race condition.

- [#12121](https://github.com/emqx/emqx/pull/12121) Fixed an issue where nodes in the cluster would occasionally return a stale view when updating configurations on different nodes concurrently.

- [#12158](https://github.com/emqx/emqx/pull/12158) Fixed an issue when the rule engine cannot connect to Redis hosted by Upstash.

  Before the fix, after establishing a TCP connection with the Redis service, the Redis driver of EMQX used [Inline Commands](https://redis.io/docs/reference/protocol-spec/#inline-commands) to send AUTH and SELECT commands. However, the `upstash` Redis service does not support Inline Commands, which causes the rule engine to fail to connect to the `upstash` Redis service.
  After the fix, the Redis driver of EMQX uses RESP (REdis Serialization Protocol) to send AUTH and SELECT commands.

- [#12176](https://github.com/emqx/emqx/pull/12176) Always acknowledge `DISCONNECT` packet to MQTT-SN client regardless of whether the connection has been successfully established before.

- [#12180](https://github.com/emqx/emqx/pull/12180) Fix an issue where DTLS enabled MQTT-SN gateways could not be started, caused by incompatibility of default listener configuration with the DTLS implementation.
- [#12219](https://github.com/emqx/emqx/pull/12219) Fix file transfer S3 config secret deobfuscation issue while performing config updates from dashboard.

## 5.3.2

*Release Date: 2023-12-01*

### Enhancements

- [#11752](https://github.com/emqx/emqx/pull/11752) Changed default RPC driver from `gen_rpc` to `rpc` for core-replica database synchronization.

  This improves core-replica data replication latency.

- [#11785](https://github.com/emqx/emqx/pull/11785) Allowed users with the "Viewer" role to change their own passwords. However, those with the "Viewer" role do not have permission to change the passwords of other users.
- [#11787](https://github.com/emqx/emqx/pull/11787) Improved the performance of the `emqx` command.

- [#11790](https://github.com/emqx/emqx/pull/11790) Added validation to Redis commands in Redis authorization source.
  Additionally, this improvement refines the parsing of Redis commands during authentication and authorization processes.  The parsing now aligns with `redis-cli` compatibility standards and supports quoted arguments.

- [#11541](https://github.com/emqx/emqx/pull/11541) Enhanced file transfer capabilities. Now, clients can use an asynchronous method for file transfer by sending commands to the `$file-async/...` topic and subscribing to command execution results on the `$file-response/{clientId}` topic. This improvement simplifies the use of the file transfer feature, particularly suitable for clients using MQTT v3.1/v3.1.1 or those employing MQTT bridging. For more details, please refer to [EIP-0021](https://github.com/emqx/eip).

### Bug Fixes

- [#11757](https://github.com/emqx/emqx/pull/11757) Fixed the error response code when downloading non-existent trace files. Now the response returns `404` instead of `500`.

- [#11762](https://github.com/emqx/emqx/pull/11762) Fixed an issue in EMQX's `built_in_database` authorization source. With this update, all Access Control List (ACL) records are completely removed when an authorization source is deleted. This resolves the issue of residual records remaining in the database when re-creating authorization sources.

- [#11771](https://github.com/emqx/emqx/pull/11771) Fixed validation of Bcrypt salt rounds in authentication management through the API/Dashboard.

- [#11780](https://github.com/emqx/emqx/pull/11780) Fixed validation of the `iterations` field of the `pbkdf2` password hashing algorithm. Now, `iterations` must be strictly positive. Previously, it could be set to 0, which led to a nonfunctional authenticator.

- [#11791](https://github.com/emqx/emqx/pull/11791) Fixed an issue in the EMQX CoAP Gateway where heartbeats were not effectively maintaining the connection's active status. This fix ensures that the heartbeat mechanism properly sustains the liveliness of CoAP Gateway connections.

- [#11797](https://github.com/emqx/emqx/pull/11797) Modified HTTP API behavior for APIs managing the `built_in_database` authorization source. They will now return a `404` status code if `built_in_database` is not set as the authorization source, replacing the former `20X` response.

- [#11965](https://github.com/emqx/emqx/pull/11965) Improved the termination of EMQX services to ensure a graceful stop even in the presence of an unavailable MongoDB resource.

- [#11975](https://github.com/emqx/emqx/pull/11975) This fix addresses an issue where redundant error logs were generated due to a race condition during simultaneous socket closure by a peer and the server. Previously, concurrent socket close events triggered by the operating system and EMQX resulted in unnecessary error logging. The implemented fix improves event handling to eliminate unnecessary error messages.

- [#11987](https://github.com/emqx/emqx/pull/11987) Fixed a bug where attempting to set the `active_n` option on a TCP/SSL socket could lead to a connection crash.

  The problem occurred if the socket had already been closed by the time the connection process attempted to apply the `active_n` setting, resulting in a `case_clause` crash.

- [#11731](https://github.com/emqx/emqx/pull/11731) Added hot configuration support for the file transfer feature.

- [#11754](https://github.com/emqx/emqx/pull/11754) Improved the log formatting specifically for the Postgres bridge in EMQX. It addresses issues related to Unicode characters in error messages returned by the driver.


## 5.3.1

*Release Date: 2023-11-14*

### Enhancements

- [#11637](https://github.com/emqx/emqx/pull/11637) Added extra diagnostic checks to help debug issues when mnesia is stuck waiting for tables. Library Updates: `ekka` has been upgraded to version 0.15.15, and `mria` to version 0.6.4.
- [#11581](https://github.com/emqx/emqx/pull/11581) Feature Preview: Planned for EMQX v5.4.0, introducing the concepts of *Connector* and *Action* base on data bridge. The existing data bridge will be gradually migrated to Connector and Action. Connector are designed to manage the integration with external systems, while Actions are solely used to configure the data processing methods. Connector can be reused across multiple Actions, providing greater flexibility and scalability. Currently, the migration has been completed for Kafka producer and Azure Event Hub producer.
- The Dashboard now supports MQTT 5.0 publish attribute settings for the rule engine's message republish action, allowing users more flexibility in publishing messages.

### Bug Fixes

- [#11565](https://github.com/emqx/emqx/pull/11565) Upgraded jq library from v0.3.10 to v0.3.11. In this version, jq_port programs are initiated on-demand and will not appear in users' processes unless the jq function in EMQX is used. Additionally, idle jq_port programs will auto-terminate after a set period. Note: Most EMQX users are running jq in NIF mode and will not be affected by this update.

- [#11676](https://github.com/emqx/emqx/pull/11676) Hid a few pieces of sensitive information from debug-level logs.

- [#11697](https://github.com/emqx/emqx/pull/11697) Disabled outdated TLS versions and cipher suites in the EMQX backplane network (`gen_rpc`). Added support for tlsv1.3 on the backplane and introduced new configuration parameters: `EMQX_RPC__TLS_VERSIONS` and `EMQX_RPC__CIPHERS`.

  The corresponding `gen_rpc` PR: https://github.com/emqx/gen_rpc/pull/36

- [#11734](https://github.com/emqx/emqx/pull/11734) Fixed clustering in IPv6 network. Added new configurations `rpc.listen_address` and `rpc.ipv6_only` to allow EMQX cluster RPC server and client to use IPv6.

- [#11747](https://github.com/emqx/emqx/pull/11747) Updated QUIC stack to msquic 2.2.3.


- [#11796](https://github.com/emqx/emqx/pull/11796) Fixed rpc schema to ensure that client/server uses same transport driver.


- [#11798](https://github.com/emqx/emqx/pull/11798) Fixed the issue where the node could not start after executing `./bin/emqx data import [FILE]`.

  The connection between `apikey_key` and `apikey_name` is also enhanced for better consistency and unique identification.
  - `apikey_key`: When generating an API key via the dashboard, `apikey_key` will now create a unique value derived from the provided human-readable `apikey_name`.
  - `apikey_name` Conversely, when using a bootstrap file to generate an API key, `apikey_name` will be generated as a unique value based on the associated `apikey_key`.

- [#11813](https://github.com/emqx/emqx/pull/11813) Fixed the schema to ensure that RPC client SSL port aligns with the configured server port. This fix also guarantees that the RPC ports are correctly opened in the Helm chart.

- [#11819](https://github.com/emqx/emqx/pull/11819) Upgraded opentelemetry library to v1.3.1-emqx. This opentelemetry release fixes invalid metrics timestamps in the exported metrics.

- [#11861](https://github.com/emqx/emqx/pull/11861) Fixed excessive warning message printed in remote console shell.

- [#11722](https://github.com/emqx/emqx/pull/11722) Fixed an issue where a Kafka Producer bridge with `sync` query mode would not buffer messages when in the `connecting` state.
- [#11724](https://github.com/emqx/emqx/pull/11724) Fixed a metrics-related issue where messages sent to Kafka would be counted as failed even when they were successfully transmitted afterward due to internal buffering.
- [#11728](https://github.com/emqx/emqx/pull/11728) Enhanced the LDAP filter string parser with the following improvements:
  - Automatic escaping of special characters within filter strings.
  - Fixed a bug that previously prevented the use of `dn` as a filter value.
- [#11733](https://github.com/emqx/emqx/pull/11733) Resolved an incompatibility issue that caused crashes during session takeover or channel eviction when the session was located on a remote node running EMQX v5.2.x or an earlier version.
- [#11750](https://github.com/emqx/emqx/pull/11750) Eliminated logging and tracing of HTTP request bodies in HTTP authentification and HTTP bridges.
- [#11760](https://github.com/emqx/emqx/pull/11760) Simplified the CQL query used for the Cassandra bridge health check, which was previously generating warnings in the Cassandra server logs.

- [#11886](https://github.com/emqx/emqx/pull/11886) Fixed backward plugin compatibility.

  Currently, EMQX validates hook point names, and invalid hook points cannot be used for hook registration. However, some older versions of plugin templates used misspelled hook points, and actual plugins in use may also have this issue. To maintain compatibility with these older plugins, we allow the use of the old hook points for hook registration, but we issue deprecated warnings for them. As before, these hooks will not be called.

- [#11897](https://github.com/emqx/emqx/pull/11897) Fixed the issue of waiting for a loop race condition during node configuration synchronization when cluster nodes are started approximately at the same time.

## 5.3.0

*Release Date: 2023-09-29*

### Enhancements

- [#11597](https://github.com/emqx/emqx/pull/11597) Upgraded ekka to 0.15.13, which incorporates the following changes:
  - Upgraded Mria to 0.6.2.
  - Introduced the ability to configure the bootstrap data sync batch size, as detailed in [Mria PR](https://github.com/emqx/mria/pull/159).
  - Enhanced the reliability of mria_membership processes, as described in [Mria PR](https://github.com/emqx/mria/pull/156).
  - Fix log message formatting error.
  - Added `node.default_bootstrap_batch_size` option to EMQX configuration.
  Increasing the value of this option can greatly reduce a replicant node startup time, especially when the EMQX cluster interconnect network latency is high and the EMQX built-in database holds a large amount of data, e.g. when the number of subscriptions is high.
- [#11620](https://github.com/emqx/emqx/pull/11620) Added a new rule-engine SQL function `bytesize` to get the size of a byte-string. e.g. `SELECT * FROM "t/#" WHERE bytesize(payload) > 10`.
- [#11642](https://github.com/emqx/emqx/pull/11642) Updated to quicer version 0.0.200 in preparation for enabling openssl3 support for QUIC transport.

- [#11610](https://github.com/emqx/emqx/pull/11610) Implemented a preliminary Role-Based Access Control for the Dashboard.

  In this version, there are two predefined roles:
  - Administrator: This role could access all resources.

  - Viewer: This role can only view resources and data, corresponding to all GET requests in the REST API.

- [#11631](https://github.com/emqx/emqx/pull/11631) Added Single Sign-On (SSO) feature and integrated with LDAP.

- [#11656](https://github.com/emqx/emqx/pull/11656) Integrated the SAML 2.0 Support for SSO.

- [#11599](https://github.com/emqx/emqx/pull/11599) Supported audit logs to record operations from CLI, REST API, and Dashboard in separate log files.

### Bug Fixes

- [#11682](https://github.com/emqx/emqx/pull/11682) Fixed an issue where logging would stop if "Rotation Size" would be set to `infinity` on file log handlers.
- [#11567](https://github.com/emqx/emqx/pull/11567) Improve EMQX graceful shutdown (`emqx stop` command):
  - Increase timeout from 1 to 2 minutes.
  - Printed an error message if EMQX can't stop gracefully within the configured timeout.
  - Print periodic status messages while EMQX is shutting down.
- [#11584](https://github.com/emqx/emqx/pull/11584) Fixed telemetry reporting error on Windows when os_mon module is unavailable.
- [#11605](https://github.com/emqx/emqx/pull/11605) Lowered CMD_overridden log severity from warning to info.
- [#11622](https://github.com/emqx/emqx/pull/11622) Upgraded rpc library gen_rpc from 2.8.1 to 3.1.0.
- [#11623](https://github.com/emqx/emqx/pull/11623) Upgraded library `esockd` from 5.9.6 to 5.9.7. This upgrade included:
  * Enhancements regarding proxy protocol error and timeout. [esockd pr#178](https://github.com/emqx/esockd/pull/178)
  * Lowered `ssl_error` exceptions to info-level logging. [esockd pr#180](https://github.com/emqx/esockd/pull/180)
  * Malformed MQTT packet parsing exception log level is lowered from `error` to `info`.
  * In command `emqx ctl listeners` output, the `shutdown_count` counter is incremented
  when TLS handshake failure (`ssl_error`) or Malformed packet (`frame_error`) happens.
- [#11661](https://github.com/emqx/emqx/pull/11661) Fixed log formatter when log.HANDLER.formatter is set to 'json'. The bug was introduced in v5.0.4 where the log line was no longer a valid JSON, but prefixed with timestamp string and level name.
- [#11627](https://github.com/emqx/emqx/pull/11627) Fixed resources cleanup in HStreamdB bridge. Prior to this fix, HStreamDB bridge might report errors during bridge configuration updates, since hstreamdb client/producer were not stopped properly.

## 5.2.1

*Release Date: 2023-09-20*

### Enhancements

- [#11487](https://github.com/emqx/emqx/pull/11487) The bcrypt work factor is limited to the range 5-10, because higher values consume too much CPU resources.
  Bcrypt library is updated to allow parallel hash evaluation.

- [#11568](https://github.com/emqx/emqx/pull/11568) Added support for defining templates for MQTT 5.0 publish properties and user properties in Republish rule action.

- [#11612](https://github.com/emqx/emqx/pull/11612) During node evacuation, evacuate all disconnected sessions, not only those started with `clean_start` set to `false`.

- [#11532](https://github.com/emqx/emqx/pull/11532) Improved error messaging for better clarity when parsing invalid packets.

### Bug Fixes

- [#11493](https://github.com/emqx/emqx/pull/11493) Fixed response examples for `/api/v5/publish` bad request in RESP API documentation. Previously the documentation example said that the bad request response could return a list in the body which was not actually the case.

- [#11499](https://github.com/emqx/emqx/pull/11499) Upgraded Erlang/OTP to version 25.3.2-2, which now excludes sensitive data from mnesia_hook log messages.

- [#11506](https://github.com/emqx/emqx/pull/11506) Previously, attempting to download a non-existent trace log file would result in downloading an empty file. After implementing this fix, when attempting to download an empty trace log file using the GET request `/api/v5/trace/clientempty/download`, the server will now respond with a 404 status code and the following JSON message: `{"code":"NOT_FOUND","message":"Trace is empty"}`. This response will be triggered if no events matching the trace condition are found in the log file.

- [#11522](https://github.com/emqx/emqx/pull/11522) Improved rule engine schema registry error message when schema name exceeds the permissible length.

- [#11531](https://github.com/emqx/emqx/pull/11531) Fixed an issue where authorization cache cleaning CLI was not working properly for specific client ID.

- [#11564](https://github.com/emqx/emqx/pull/11564) Fixed cluster partition autoheal functionality. Implemented autohealing for the clusters that split into multiple partitions.

- [#11568](https://github.com/emqx/emqx/pull/11568) Fixed an issue where an ill-defined built-in rule action config could be interpreted as a custom user function.

- [#11394](https://github.com/emqx/emqx/pull/11394) Upgraded Kafka producer client `wolff` from 1.7.6 to 1.7.7. This fixed a potential race condition that might cause all Kafka producers to crash if some failed to initialize.

- [#11401](https://github.com/emqx/emqx/pull/11401) Fixed the behavior of the rule SQL `mongo_date` function in SQL statement testing in the EMQX Dashboard. The rule SQL `mongo_date` function now returns a string with the format `ISODate(*)`, where * is an ISO date string when running rules in test mode. This format aligns with how MongoDB stores dates.

- [#11547](https://github.com/emqx/emqx/pull/11547) Fixed several emqx_bridge issues:
  - Fixed Cassandra bridge connect error occurring when the bridge is configured without username/password
  (Cassandra doesn't require user credentials when it is configured with `authenticator: AllowAllAuthenticator`.)
  - Fixed SQL Server bridge connect error caused by an empty password.
  - Made `username` a required field in Oracle bridge.
  - Fixed IoTDB bridge error caused by setting base URL without a scheme (e.g. `<host>:<port>`).

- [#11630](https://github.com/emqx/emqx/pull/11630) Fixed an issue where the core node could get stuck in the `mria_schema:bootstrap/0` state, preventing new nodes from joining the cluster.

## 5.2.0

*Release Date: 2023-09-07*

### Enhancements

- [#10697](https://github.com/emqx/emqx/pull/10697) This enhancement enables the configuration of the `minReadySeconds` for the StatefulSet. This feature allows for the introduction of a time gap between the restarts of individual pods triggered by upgrade or restart commands.

- [#11124](https://github.com/emqx/emqx/pull/11124) Released packages for Amazon Linux 2023.

- [#11289](https://github.com/emqx/emqx/pull/11289) Released packages for Debian 12.

- [#11290](https://github.com/emqx/emqx/pull/11290) Updated the `jq` dependency to version 0.3.10, which includes an update to the `oniguruma` library to version 6.9.8 with a few minor security fixes.

- [#11291](https://github.com/emqx/emqx/pull/11291) Updated RocksDB version to 1.8.0-emqx-1 via ekka update to 0.15.6.

- [#11390](https://github.com/emqx/emqx/pull/11390) Added `node.broker_pool_size`, `node.generic_pool_size`, `node.channel_cleanup_batch_size` options to EMQX configuration. Tuning these options can significantly improve the performance if cluster interconnect network latency is high.

- [#11429](https://github.com/emqx/emqx/pull/11429) Added an option to configure detection of the legacy protocol in MondoDB connectors and bridges.

- [#11436](https://github.com/emqx/emqx/pull/11436) Added a new API endpoint `DELETE/banned` for clearing all `banned` data.

- [#11438](https://github.com/emqx/emqx/pull/11438) Changed the type of the `mqtt.max_packet_size` from string to byteSize for a better representation of the valid numeric range. Strings will still be accepted for backward compatibility.

- [#11469](https://github.com/emqx/emqx/pull/11469) Added support for specifying username in Redis authentication.

- [#11496](https://github.com/emqx/emqx/pull/11496) Disabled the Erlang VM Prometheus exporter by default to improve performance and security.

- [#11497](https://github.com/emqx/emqx/pull/11497) Enhanced broker metrics collection and export by adding new metrics for messages, overload protection, authorization, authentication, and improving naming consistency for OpenTelemetry.

- [#10647](https://github.com/emqx/emqx/pull/10647) Implemented [GreptimeDB](https://github.com/GreptimeTeam/greptimedb) data integration.

- [#11261](https://github.com/emqx/emqx/pull/11261) Implemented Amazon Kinesis Data Streams producer data integration.

- [#11329](https://github.com/emqx/emqx/pull/11329) Implemented Azure Event Hub Producer data integration.

- [#11363](https://github.com/emqx/emqx/pull/11363) Added TLS connection support to the RabbitMQ bridge.

- [#11367](https://github.com/emqx/emqx/pull/11367) Ported GCP IoT Hub authentication support from EMQX 4.4.

- [#11386](https://github.com/emqx/emqx/pull/11386) Integrated LDAP as a new authenticator.

- [#11392](https://github.com/emqx/emqx/pull/11392) Integrated LDAP as an authorization source.

- [#11402](https://github.com/emqx/emqx/pull/11402) Added support for using placeholders to define MQTT Topic in Kafka Consumer bridge topic mappings. This allows dynamically setting the MQTT Topic.

- [#11403](https://github.com/emqx/emqx/pull/11403) Added support for defining message attributes and ordering key templates for GCP PubSub Producer bridge.

  Also updated our HOCON library to fix an issue where objects in an array were concatenated even if they were laid on different lines.

- [#11459](https://github.com/emqx/emqx/pull/11459) Added the option to configure health check interval for Kafka bridges.

- [#11478](https://github.com/emqx/emqx/pull/11478) Added HStreamDB bridge support (both TCP and TLS connection allowed), adapted to the HStreamDB `v0.16.1`.

  Updated driver to `0.4.5+v0.16.1` in [PR#11530](https://github.com/emqx/emqx/pull/11530).

- [#11389](https://github.com/emqx/emqx/pull/11389) Improved retained message publishing latency by consolidating multiple index update operations into a single Mnesia activity, leveraging the new APIs introduced in Mria 0.6.0.

- [#11396](https://github.com/emqx/emqx/pull/11396) Introduced topic index for the rule engine runtime to speed up matching messages' topics to topic filters configured in rule definitions by avoiding full scan of the rule set, significantly improving EMQX's performance when handling a substantial number of rules.

- [#11399](https://github.com/emqx/emqx/pull/11399) Improved the placeholder syntax in the rule engine. The republishing actions support placeholder syntax to
  dynamically fill in the content of strings in the payload variable. The format of the placeholder syntax is `\${key}`.
  Before this improvement, the `key` in `\${key}` could only contain letters, numbers, and underscores. Now the `key` supports any UTF8 characters.

- [#11405](https://github.com/emqx/emqx/pull/11405) Made the error message for `date_to_unix_ts` function more understandable.

- [#11490](https://github.com/emqx/emqx/pull/11490) Added fast error handling for undefined passwords in various authentication backends. This improves the consistency and user-friendliness of the authentication process.

### Bug Fixes

- [#11065](https://github.com/emqx/emqx/pull/11065) Silenced irrelevant error messages during EMQX shutdown.

- [#11279](https://github.com/emqx/emqx/pull/11279) Fixed an issue where clients could not send messages with large payloads when debug/trace logging was enabled in EMQX.

- [#11296](https://github.com/emqx/emqx/pull/11296) Added support for importing additional configurations from EMQX backup file using the `emqx ctl import` command):

  - rule_engine (previously not imported due to the bug)
  - topic_metrics (previously not implemented)
  - slow_subs (previously not implemented)

- [#11327](https://github.com/emqx/emqx/pull/11327) Updated ekka to version 0.15.8, mria to version 0.15.8, and optvar to 1.0.5. This fixes occasional assertion failures.

- [#11346](https://github.com/emqx/emqx/pull/11346) Updated ekka to version 0.15.9. This fixes dangling etcd locks that occurred when acquiring the lock failed with a timeout.

- [#11347](https://github.com/emqx/emqx/pull/11347) Ensured that OCSP request path is properly URL encoded.

- [#11352](https://github.com/emqx/emqx/pull/11352) Fixed a [crash issue](https://github.com/emqx/emqx/issues/11345) that occurred when starting on Windows or any other platform without RocksDB support.


- [#11388](https://github.com/emqx/emqx/pull/11388) Increased `emqx_router_sup` restart intensity to improve tolerance for occasional crashes that can occur under normal conditions, without necessitating the shutdown of the entire EMQX application.
  For example, mria write/delete call delegated from a replicant to a core node by `emqx_router_helper` may fail,
  if the core node undergoes stopping, restarting, or is in an unready state. The modified restart intensity ensures that the system remains stable and operational.


  This fixes issues found when trying to upgrade from 5.1.3 where that option was set in the configuration files or persisted in EMQX Operator settings.

- [#11424](https://github.com/emqx/emqx/pull/11424) Added a check for the maximum value of the timestamp in the API to ensure it is a valid Unix timestamp.

- [#11445](https://github.com/emqx/emqx/pull/11445) Removed os_mon application monitor support on Windows platforms to prevent VM crashes. Functionality remains on non-Windows platforms.

- [#11454](https://github.com/emqx/emqx/pull/11454) Fixed crashing when debugging/tracing with large payloads (introduced in [#11279](https://github.com/emqx/emqx/pull/11279)).

- [#11456](https://github.com/emqx/emqx/pull/11456) Removed validation that enforced non-empty PEM for the CA cert file, allowing the CA certificate file PEM to be empty.

- [#11466](https://github.com/emqx/emqx/pull/11466) Fixed a crash that occurred when setting the `ssl_options.ciphers` configuration option to an empty string ("").

- [#11480](https://github.com/emqx/emqx/pull/11480) Improves the error handling and testing of SQL functions in the rule engine when rule functions receive bad arguments.

- [#11520](https://github.com/emqx/emqx/pull/11520) Fixed issue where `packets_connack_sent` metric was not incremented on CONNACK packets sent with non-zero `ack_flag`.

- [#11523](https://github.com/emqx/emqx/pull/11523) Corrected a misleading prompt when specifying invalid certificates/keys for the `/configs` API.

- [#11534](https://github.com/emqx/emqx/pull/11534) Fixed the increment on data bridge statistics when the bridge is unhealthy. Now, messages sent to unhealthy bridges are counted as dropped messages.

- [#11540](https://github.com/emqx/emqx/pull/11540) Improved HTTP response when attempting to create a bridge with an invalid name.

- [#11548](https://github.com/emqx/emqx/pull/11548) Fixed an issue that prevented the plugin order from being updated across the entire cluster.

- [#11366](https://github.com/emqx/emqx/pull/11366) Fixed an issue that could prevent a pod from starting if some bridge configurations were specified in `bootstrapConfig` using EMQX Operator.

- [#11453](https://github.com/emqx/emqx/pull/11453) Fixed an issue that would yield false negatives when testing the connectivity of InfluxDB bridges.

- [#11461](https://github.com/emqx/emqx/pull/11461) Aligned the timeout for testing bridge connectivity more closely with the configured health check timeout.

- [#11492](https://github.com/emqx/emqx/pull/11492) Fixed an issue that would yield false negatives when testing the connectivity of GreptimeDB bridges.


- [#11508](https://github.com/emqx/emqx/pull/11508) Fixed error handling in Kafka bridge when headers are translated to an invalid value.

- [#11513](https://github.com/emqx/emqx/pull/11513) Fixed a bug that prevented the Kafka Producer bridge from using the correct template for the `timestamp` field.

- [#11527](https://github.com/emqx/emqx/pull/11527) Fixed an issue related to Kafka header template handling. The issue occurs when placeholders are resolved into an array of key-value pairs (e.g.: `[{"key": "foo", "value": "bar"}]`).

## 5.1.1

*Release Date: 2023-07-05*

### Enhancements

- [#10667](https://github.com/emqx/emqx/pull/10667) The MongoDB connector and bridge have been refactored into a separate app to improve the code structure.

- [#11115](https://github.com/emqx/emqx/pull/11115) Added info logs to indicate when buffered messages are dropped due to time-to-live (TTL) expiration.

- [#11133](https://github.com/emqx/emqx/pull/11133) Renamed `deliver_rate` to `delivery_rate` in the configuration of `retainer`, while being compatible with the previous `deliver_rate`.

- [#11137](https://github.com/emqx/emqx/pull/11137) Refactored the Dashboard listener configuration to use a nested `ssl_options` field for SSL settings.

- [#11138](https://github.com/emqx/emqx/pull/11138) Changed the default value of k8s `api_server` from `http://127.0.0.1:9091` to `https://kubernetes.default.svc:443`.

  - `emqx_ctl conf show cluster` no longer displays irrelevant configuration items when `discovery_strategy=static`.
  Configuration information related to `etcd/k8s/dns` will not be shown.
  - Removed `zones `(deprecated config key) from `emqx_ctl conf show_keys`.

- [#11165](https://github.com/emqx/emqx/pull/11165) Removed the `/configs/limiter` API from `swagger.json`. Only the API documentation was removed,
  and the `/configs/limiter` API functionalities remain unchanged.

- [#11166](https://github.com/emqx/emqx/pull/11166) Added 3 random SQL functions to the rule engine:

  - `random()`: Generates a random number between 0 and 1 (0.0 =< X < 1.0).
  - `uuid_v4()`: Generates a random UUID (version 4) string.
  - `uuid_v4_no_hyphen()`: Generates a random UUID (version 4) string without hyphens.

- [#11180](https://github.com/emqx/emqx/pull/11180) Added a new configuration API `/configs` (GET/PUT) that supports reloading the HOCON format configuration file.

- [#11226](https://github.com/emqx/emqx/pull/11226) Unified the listener switch to `enable`, while being compatible with the previous `enabled`.

- [#11249](https://github.com/emqx/emqx/pull/11249) Added `/license/setting` REST API endpoint to read and update licensed connections usage alarm watermark.

- [#11251](https://github.com/emqx/emqx/pull/11251) Added the `/cluster/topology` REST API endpoint:

  A `GET` request to this endpoint returns the cluster topology, showing connections between RLOG core and replicant nodes.

- [#11253](https://github.com/emqx/emqx/pull/11253) The Webhook/HTTP bridge has been refactored into its own Erlang application. This allows for more flexibility in the future and allows the bridge to be run as a standalone application.

- [#11079](https://github.com/emqx/emqx/pull/11079) Added support for custom headers in messages for Kafka bridge producer mode.

- [#11132](https://github.com/emqx/emqx/pull/11132) Added support for MQTT action authorization based on QoS level and Retain flag values.
  Now, EMQX can verify whether clients have the permission to publish/subscribe using specific QoS levels, and whether they have the permission to publish retained messages.

- [#11207](https://github.com/emqx/emqx/pull/11207) Updated the driver versions of multiple data bridges to enhance security and ensure that sensitive data will not be leaked. This includes:

  - TDengine
  - MongoDB
  - MySQL
  - Clickhouse

- [#11241](https://github.com/emqx/emqx/pull/11241) Schema Registry has been refactored into its own Erlang application. This allows for more flexibility in the future.

- [#11020](https://github.com/emqx/emqx/pull/11020) Upgraded emqtt dependency to prevent sensitive data leakage in the debug log.

- [#11135](https://github.com/emqx/emqx/pull/11135) Improved time offset parser in rule engine and return uniform error codes.

- [#11236](https://github.com/emqx/emqx/pull/11236) Improved the speed of clients querying in REST API `/clients` endpoint with default parameters.

### Bug Fixes

- [#11004](https://github.com/emqx/emqx/pull/11004) Wildcards are no longer allowed for the destination topic in topic rewrite.

- [#11026](https://github.com/emqx/emqx/pull/11026) Addressed an inconsistency in the usage of `div` and `mod` operations within the rule engine. Previously, the `div'` operation could only be used as an infix operation, and `mod` could only be applied through a function call. Now, both `div` and `mod` can be used via function call syntax and infix syntax.

- [#11037](https://github.com/emqx/emqx/pull/11037) When starting an HTTP connector, EMQX now returns a descriptive error in case the system is unable to connect to the remote target system.

- [#11039](https://github.com/emqx/emqx/pull/11039) Fixed database number validation for Redis connector. Previously, negative numbers were accepted as valid database numbers.

- [#11074](https://github.com/emqx/emqx/pull/11074) Fixed a bug to adhere to Protocol spec MQTT-5.0 [MQTT-3.8.3-4].

- [#11077](https://github.com/emqx/emqx/pull/11077) Fixed a crash when updating listener binding with a non-integer port.

- [#11094](https://github.com/emqx/emqx/pull/11094) Fixed an issue where connection errors in Kafka Producer would not be reported when reconnecting the bridge.

- [#11103](https://github.com/emqx/emqx/pull/11103) Updated `erlcloud` dependency.

- [#11106](https://github.com/emqx/emqx/pull/11106) Added validation for the maximum number of `worker_pool_size` of a bridge resource.

  Now the maximum amount is 1024 to avoid large memory consumption from an unreasonable number of workers.

- [#11118](https://github.com/emqx/emqx/pull/11118) Ensured that validation errors in REST API responses are slightly less confusing. Now, if there are out-of-range errors, they will be presented as `{"value": 42, "reason": {"expected": "1..10"}, ...}`, replacing the previous usage of `expected_type` with `expected`.

- [#11126](https://github.com/emqx/emqx/pull/11126) Rule metrics for async mode bridges will set failure counters correctly now.

- [#11134](https://github.com/emqx/emqx/pull/11134) Fixed the value of the uppercase `authorization` header not being obfuscated in the log.

- [#11139](https://github.com/emqx/emqx/pull/11139) The Redis connector has been refactored into its own Erlang application to improve the code structure.

- [#11145](https://github.com/emqx/emqx/pull/11145) Added several fixes and improvements in Ekka and Mria.

  Ekka:
  - Improved cluster discovery log messages to consistently describe actual events.
  [Ekka PR](https://github.com/emqx/ekka/pull/204)
  - Removed deprecated cluster auto-clean configuration parameter (it has been moved to Mria).
  [Ekka PR](https://github.com/emqx/ekka/pull/203)

  Mria:
  - Ping now only runs on replicant nodes. Previously, `mria_lb` was trying to ping both stopped and running
  replicant nodes, which could result in timeout errors.
  [Mria PR](https://github.com/emqx/mria/pull/146)
  - Used `null_copies` storage when copying `$mria_rlog_sync` table.
  This fix has no effect on EMQX for now, as `$mria_rlog_sync` is only used in `mria:sync_transaction/2,3,4`,
  which is not utilized by EMQX.
  [Mria PR](https://github.com/emqx/mria/pull/144)

- [#11148](https://github.com/emqx/emqx/pull/11148) Fixed an issue when nodes tried to synchronize configuration update operations to a node which has already left the cluster.

- [#11150](https://github.com/emqx/emqx/pull/11150) Wait for Mria table when emqx_psk app is being started to ensure that PSK data is synced to replicant nodes even if they don't have init PSK file.

- [#11151](https://github.com/emqx/emqx/pull/11151) The MySQL connector has been refactored into its own Erlang application to improve the code structure.

- [#11158](https://github.com/emqx/emqx/pull/11158) Wait for Mria table when the mnesia backend of retainer starts to avoid a possible error of the retainer when joining a cluster.

- [#11162](https://github.com/emqx/emqx/pull/11162) Fixed an issue in webhook bridge where, in async query mode, HTTP status codes like 4XX and 5XX would be treated as successes in the bridge metrics.

- [#11164](https://github.com/emqx/emqx/pull/11164) Reintroduced support for nested (i.e.: `${payload.a.b.c}`) placeholders for extracting data from rule action messages without the need for calling `json_decode(payload)` first.

- [#11172](https://github.com/emqx/emqx/pull/11172) Fixed the `payload` field in rule engine SQL being duplicated in the below situations:

  - When using a `foreach` sentence without the `as` sub-expression and selecting all fields (using the `*` or omitting the `do` sub-expression).

  For example:

  `FOREACH payload.sensors FROM "t/#"`
  - When selecting the `payload` field and all fields.

  For example:

  `SELECT payload.sensors, * FROM "t/#"`

- [#11174](https://github.com/emqx/emqx/pull/11174) Fixed the encoding of the `server` key coming from an ingress MQTT bridge.

  Before the fix, it was encoded as a list of integers corresponding to the ASCII characters of the server string.

- [#11184](https://github.com/emqx/emqx/pull/11184) Config value for `mqtt.max_packet_size` now has a max value of 256MB as defined by the protocol.

- [#11192](https://github.com/emqx/emqx/pull/11192) Fixed an issue with producing invalid HOCON file when an atom type was used. Also removed unnecessary `"` around keys and latin1 strings from HOCON file.

- [#11195](https://github.com/emqx/emqx/pull/11195) Fixed an issue where the REST API could create duplicate subscriptions for specified clients of the Stomp gateway.

- [#11206](https://github.com/emqx/emqx/pull/11206) Made the `username` and `password` params of CoAP client optional in connection mode.

- [#11208](https://github.com/emqx/emqx/pull/11208) Fixed the issue of abnormal data statistics for LwM2M clients.

- [#11211](https://github.com/emqx/emqx/pull/11211) HTTP API `DELETE` operations on non-existent resources now consistently returns `404`.

- [#11214](https://github.com/emqx/emqx/pull/11214) Fixed a bug where node configuration may fail to synchronize correctly when the node joins the cluster.

- [#11229](https://github.com/emqx/emqx/pull/11229) Fixed an issue that prevented plugins from starting/stopping after changing configuration via `emqx ctl conf load`.

- [#11237](https://github.com/emqx/emqx/pull/11237) The `headers` default value in /prometheus API should be a map instead of a list.

- [#11250](https://github.com/emqx/emqx/pull/11250) Fixed a bug when the order of MQTT packets withing a WebSocket packet will be reversed.


- [#11271](https://github.com/emqx/emqx/pull/11271) Ensured that the range of all percentage type configurations is from 0% to 100% in the REST API and configuration. For example, `sysom.os.sysmem_high_watermark=101%` is invalid now.

- [#11272](https://github.com/emqx/emqx/pull/11272) Fixed a typo in the log, where an abnormal `PUBREL` packet was mistakenly referred to as `pubrec`.

- [#11281](https://github.com/emqx/emqx/pull/11281) Restored support for the special `$queue/` shared subscription topic prefix.

- [#11294](https://github.com/emqx/emqx/pull/11294) Fixed `emqx ctl cluster join`, `leave`, and `status` commands.

- [#11306](https://github.com/emqx/emqx/pull/11306) Fixed rule action metrics inconsistency where dropped requests were not accounted for.

- [#11309](https://github.com/emqx/emqx/pull/11309) Improved startup order of EMQX applications. Simplified build scripts and improved code reuse.

- [#11322](https://github.com/emqx/emqx/pull/11322) Added support for importing additional configurations from EMQX backup file (`emqx ctl import` command):

  - rule_engine (previously not imported due to the bug)
  - topic_metrics (previously not implemented)
  - slow_subs (previously not implemented).

- [#10645](https://github.com/emqx/emqx/pull/10645) Changed health check for Oracle Database, PostgreSQL, MySQL and Kafka Producer data bridges to ensure target table/topic exists.

- [#11107](https://github.com/emqx/emqx/pull/11107) MongoDB bridge health check now returns the failure reason.

- [#11139](https://github.com/emqx/emqx/pull/11139) The Redis bridge has been refactored into its own Erlang application to improve the code structure and to make it easier to maintain.

- [#11151](https://github.com/emqx/emqx/pull/11151) The MySQL bridge has been refactored into its own Erlang application to improve the code structure and to make it easier to maintain.

- [#11163](https://github.com/emqx/emqx/pull/11163) Hid `topology.pool_size` in MondoDB bridges and fixed it to 1 to avoid confusion.

- [#11175](https://github.com/emqx/emqx/pull/11175) Now when using a nonexistent hostname for connecting to MySQL, a 400 error is returned rather than 503 in the REST API.

- [#11198](https://github.com/emqx/emqx/pull/11198) Fixed global rebalance status evaluation on replicant nodes. Previously, `/api/v5/load_rebalance/global_status` API method could return incomplete results if handled by a replicant node.

- [#11223](https://github.com/emqx/emqx/pull/11223) In InfluxDB bridging, mixing decimals and integers in a field may lead to serialization failure in the Influx Line Protocol, resulting in the inability to write to the InfluxDB bridge (when the decimal point is 0, InfluxDB mistakenly interprets it as an integer).

  See also: [InfluxDB v2.7 Line-Protocol](https://docs.influxdata.com/influxdb/v2.7/reference/syntax/line-protocol/#float).

- [#11225](https://github.com/emqx/emqx/pull/11225) The `username` field in PostgreSQL/Timescale/MatrixDB bridges configuration is now a required one.

- [#11242](https://github.com/emqx/emqx/pull/11242) Restarted emqx_ee_schema_registry when a node joins a cluster. As emqx_ee_schema_registry uses Mria tables, a node joining a cluster needs to restart this application in order to start relevant Mria shard processes, ensuring a correct behaviour in Core/Replicant mode.

- [#11266](https://github.com/emqx/emqx/pull/11266) Fixed and improved support for TDengine `insert` syntax:

  1. Added support for inserting into multi-table in the template.

     For example:

     `insert into table_1 values (${ts}, '${id}', '${topic}')
     table_2 values (${ts}, '${id}', '${topic}')`

  2. Added support for mixing prefixes/suffixes and placeholders in the template.

     For example:

     `insert into table_${topic} values (${ts}, '${id}', '${topic}')`

     Note: This is a breaking change. Previously, the values of string type were quoted automatically, but now they must be quoted explicitly.

     For example:

     `insert into table values (${ts}, '${a_string}')`

- [#11307](https://github.com/emqx/emqx/pull/11307) Fixed check for table existence to return a more friendly message in the Oracle bridge.

- [#11316](https://github.com/emqx/emqx/pull/11316) Fixed Pool Size value not being considered in Oracle Bridge.

- [#11326](https://github.com/emqx/emqx/pull/11326) Fixed return error checking on table validation in the Oracle bridge.


## 5.1.0

*Release Date: 2023-06-21*

### Enhancements

-   [#11035](https://github.com/emqx/emqx/pull/11035) Upgraded Cassandra driver to avoid username and password leakage in data bridge logs.
-   [#10584](https://github.com/emqx/emqx/pull/10584) Added log level configuration to SSL communication
-   [#10678](https://github.com/emqx/emqx/pull/10678) Optimized counter increment calls to avoid work if increment is zero.
-   [#10690](https://github.com/emqx/emqx/pull/10690) Added a retry mechanism to webhook bridge that attempts to improve throughput.
    This optimization retries request failures without blocking the buffering layer, which can improve throughput in situations of high messaging rate.
-   [#10702](https://github.com/emqx/emqx/pull/10702) Introduced a more straightforward configuration option `keepalive_multiplier` and deprecate the old `keepalive_backoff` configuration. After this enhancement, EMQX checks the client's keepalive timeout status period by multiplying the "Client Requested Keepalive Interval" with `keepalive_multiplier`.
-   [#10698](https://github.com/emqx/emqx/pull/10698) Optimized memory usage when accessing the configuration during runtime.
-   [#10778](https://github.com/emqx/emqx/pull/10778) Refactored Pulsar Producer bridge to avoid leaking resources in case bridge crashed during initialization phase.
-   [#10813](https://github.com/emqx/emqx/pull/10813) Refactored Kafka Producer and Consumer bridges to avoid leaking resources in case bridge crashed during initialization phase.
-   [#10858](https://github.com/emqx/emqx/pull/10858) A new utility function timezone_to_offset_seconds/1 has been added to the rule engine SQL language. This function converts a timezone string (for example, "+02:00", "Z" and "local") to the corresponding offset in seconds.
-   [#10841](https://github.com/emqx/emqx/pull/10841) Added a schema validation to ensure message key is not empty when "key_dispatch" strategy is selected in Kafka and Pulsar Producer bridges.
-   [#10754](https://github.com/emqx/emqx/pull/10754) The MQTT bridge has been enhanced to utilize connection pooling and leverage available parallelism, substantially improving throughput.
    As a consequence, single MQTT bridge now uses a pool of `clientid`s to connect to the remote broker.
-   [#10782](https://github.com/emqx/emqx/pull/10782) Added a new `deliver_rate` option to the retainer configuration, which can limit the maximum delivery rate per session in the retainer.
-   [#10877](https://github.com/emqx/emqx/pull/10877) Upgraded RocketMQ driver to enhance security for sensitive data.
-   [#10598](https://github.com/emqx/emqx/pull/10598) Provided a callback method of Unary type in ExProto to avoid possible message disorder issues.
-   [#10895](https://github.com/emqx/emqx/pull/10895) Refactored most of the bridges to avoid resource leaks in case bridge crashed during initialization phase.
-   [#10790](https://github.com/emqx/emqx/pull/10790) Optimized access to configuration in runtime by reducing overhead of reading configuration per zone.
-   [#10892](https://github.com/emqx/emqx/pull/10892) Added the requirement for setting SID or Service Name in Oracle Database bridge creation.
-   [#10910](https://github.com/emqx/emqx/pull/10910) The data bridge resource option `auto_restart_interval` was deprecated in favor of `health_check_interval`, and `request_timeout` was renamed to `request_ttl`. Also, the default `request_ttl` value went from 15 seconds to 45 seconds.
    The previous existence of both `auto_restart_interval` and `health_check_interval` was a source of confusion, as both parameters influenced the recovery of data bridges under failures. An inconsistent configuration of those two parameters could lead to messages being expired without a chance to retry. Now, `health_check_interval` is used both to control the interval of health checks that may transition the data bridge into `disconnected` or `connecting` states, as well as recovering from `disconnected`.
-   [#10929](https://github.com/emqx/emqx/pull/10929) Upgraded Erlang/OTP to 25.3.2-1.
-   [#10909](https://github.com/emqx/emqx/pull/10909) Removed the deprecated HTTP APIs for gateways.
-   [#10908](https://github.com/emqx/emqx/pull/10908) Refactored the RocketMQ bridge to avoid resources leaks in case bridge crashed during initialization phase.
-   [#10924](https://github.com/emqx/emqx/pull/10924) Refactored Influxdb bridge connector to avoid resource leaks in case bridge crashed during initialization phase.
-   [#10944](https://github.com/emqx/emqx/pull/10944) Improved the GCP PubSub bridge to avoid a potential issue that the bridge could fail to send messsages after node restart.
-   [#10933](https://github.com/emqx/emqx/pull/10933) Added support for configuring TCP keep-alive in MQTT/TCP and MQTT/SSL listeners.
-   [#10948](https://github.com/emqx/emqx/pull/10948) Added `live_connections` field for some HTTP APIs, i.e:
    -   `/monitor_current`, `/monitor_current/nodes/{node}`
    -   `/monitor/nodes/{node}`, `/monitor`
    -   `/node/{node}`, `/nodes`
-   [#10941](https://github.com/emqx/emqx/pull/10941) Improved the collection speed of Prometheus metrics when setting `prometheus.vm_dist_collector=disabled` and metric `erlang_vm_statistics_run_queues_length_total` is renamed to `erlang_vm_statistics_run_queues_length`
-   [#10985](https://github.com/emqx/emqx/pull/10985) Renamed `emqx ctl` command `cluster_call` to `conf cluster_sync`. The old command `cluster_call` is still a valid command, but not included in usage info.
-   [#10988](https://github.com/emqx/emqx/pull/10988) Improved log security when data bridge creation fails to ensure sensitive data is always obfuscated.
-   [#10926](https://github.com/emqx/emqx/pull/10926) Allowed `enable` as well as `enabled` as the state flag for listeners.
    Prior to this change, listener can be enable/disabled by setting the `true` or `false` on the `enabled` config. This is slightly different naming comparing to other state flags in the system. Now the `enable` flag is added as an alias in listener config.
-   [#10970](https://github.com/emqx/emqx/pull/10970) A query_mode parameter has been added to the Kafka producer bridge. This parameter allows you to specify if the bridge should use the asynchronous or synchronous mode when sending data to Kafka. The default is asynchronous mode.
-   [#10676](https://github.com/emqx/emqx/pull/10676) Added CLI commands `emqx ctl export` and `emqx ctl import` for importing/exporting configuration and user data. This allows exporting configurations and built-in database data from a running EMQX cluster and importing them into the same or another running EMQX cluster.
-   [#11003](https://github.com/emqx/emqx/pull/11003) Added an option to configure TCP keepalive in Kafka bridge.
-   [#10961](https://github.com/emqx/emqx/pull/10961) Added support for unlimited max connections for gateway listeners by allowing infinity as a valid value for the `max_connections` field in the configuration and HTTP API.
-   [#11019](https://github.com/emqx/emqx/pull/11019) Improved log security for JWT, now it will be obfuscated before print.
-   [#11024](https://github.com/emqx/emqx/pull/11024) Added a small improvement to reduce the chance of seeing the `connecting` state when creating/updating a Pulsar Producer bridge.
-   [#11034](https://github.com/emqx/emqx/pull/11034) Hid the broker config and changed the `broker.shared_subscription_strategy` to `mqtt.shared_subscription_strategy` as it belongs to `mqtt`.
-   [#11045](https://github.com/emqx/emqx/pull/11045) The listener's authentication and zone related apis have been officially removed in version `5.1.0`.
-   [#11062](https://github.com/emqx/emqx/pull/11062) Renamed config `log.file.to` to `log.file.path`.

### Bug Fixes

-   [#11018](https://github.com/emqx/emqx/pull/11018) Fixed multiple issues with the Stomp gateway, including:
    -   Fixed an issue where `is_superuser` was not working correctly.
    -   Fixed an issue where the mountpoint was not being removed in message delivery.
    -   After a message or subscription request fails, the Stomp client should be disconnected
        immediately after replying with an ERROR message.
-   [#11051](https://github.com/emqx/emqx/pull/11051) Added validation to ensure that certificate `depth` (listener SSL option) is a non negative integer.
-   [#10563](https://github.com/emqx/emqx/pull/10563) Corrected an issue where the no_local flag was not functioning correctly in subscription.
-   [#10653](https://github.com/emqx/emqx/pull/10653) Stored gateway authentication TLS certificates and keys in the data directory to fix the problem of memory leakage.
-   [#10682](https://github.com/emqx/emqx/pull/10682) Fixed the timestamp for the will message is incorrectly assigned at the session creation time, now this timestamp is the disconnected time of the session.
-   [#10701](https://github.com/emqx/emqx/pull/10701) RPM package for Amazon Linux 2 did not support TLS v1.3 as it was assembled with Erlang/OTP built with openssl 1.0.
-   [#10677](https://github.com/emqx/emqx/pull/10677) Fixed an issue in the Rule API where attempting to delete a non-existent rule resulted in a 404 HTTP error code response.
-   [#10715](https://github.com/emqx/emqx/pull/10715) Support for getting the client certificate in the client.connected hook. Previously, this data was removed after the connection was established to reduce memory usage.
-   [#10737](https://github.com/emqx/emqx/pull/10737) Fixed the issue where the HTTP API interface of Gateway cannot handle ClientIDs with special characters, such as: `!@#$%^&*()_+{}:"<>?/`.
-   [#10809](https://github.com/emqx/emqx/pull/10809) Addressed `** ERROR ** Mnesia post_commit hook failed: error:badarg` error messages happening during node shutdown or restart. Mria pull request: [https://github.com/emqx/mria/pull/142](https://github.com/emqx/mria/pull/142)
-   [#10807](https://github.com/emqx/emqx/pull/10807) The debug-level logs related to license checks will no longer be printed. These logs were generated too frequently and could interfere with log recording.
-   [#10818](https://github.com/emqx/emqx/pull/10818) Fixed `emqx_ctl traces` command error where the `traces start` command in the `emqx_mgmt_cli` module was not working properly with some filters.
-   [#10600](https://github.com/emqx/emqx/pull/10600) Deleted emqx_statsd application.
-   [#10820](https://github.com/emqx/emqx/pull/10820) Fixed the issue where newly added nodes in the cluster would not apply the new license after a cluster license update and would continue to use the old license.
    Sometimes the new node must start with a outdated license. e.g. use emqx-operator deployed and needed to scale up after license expired. At the time the cluster's license key already updated by API/CLI, but the new node won't use it.
-   [#10851](https://github.com/emqx/emqx/pull/10851) Obfuscated sensitive data in the bad API logging.
-   [#10884](https://github.com/emqx/emqx/pull/10884) Fixed an issue where trying to get rule info or metrics could result in a crash when a node is joining a cluster.
-   [#10887](https://github.com/emqx/emqx/pull/10887) Fixed a potential issue where requests to bridges might take a long time to be retried.
    This only affected low throughput scenarios, where the buffering layer could take a long time to detect connectivity and driver problems.
-   [#10878](https://github.com/emqx/emqx/pull/10878) Fixed a vulnerability in the RabbitMQ bridge, which could potentially expose passwords to log files.
-   [#10871](https://github.com/emqx/emqx/pull/10871) Fixed an issue where the Dashboard shows that the connection still exists after a CoAP connection is disconnected, but deletion and message posting requests do not take effect.
-   [#10880](https://github.com/emqx/emqx/pull/10880) Added a new REST API `POST /clients/kickout/bulk` for kicking out multiple clients in bulk.
-   [#10913](https://github.com/emqx/emqx/pull/10913) Fixed an issue where the plugin status REST API of a node would still include the cluster node status after the node left the cluster.
-   [#10923](https://github.com/emqx/emqx/pull/10923) Fixed a race-condition in channel info registration.
    Prior to this fix, when system is under heavy load, it might happen that a client is disconnected (or has its session expired) but still can be found in the clients page in dashboard. One of the possible reasons is a race condition fixed in this PR: the connection is killed in the middle of channel data registration.
-   [#10930](https://github.com/emqx/emqx/pull/10930) Added a schema validation for duration data type to avoid invalid values.
    Before this fix, it was possible to use absurd values in the schema that would exceed the system limit, causing a crash.
-   [#10952](https://github.com/emqx/emqx/pull/10952) Disallow enabling `fail_if_no_peer_cert` in listener SSL options if `verify = verify_none` is set.
    Setting `fail_if_no_peer_cert = true` and `verify = verify_none` caused connection errors due to incompatible options. This fix validates the options when creating or updating a listener to avoid these errors.

    Note: any old listener configuration with `fail_if_no_peer_cert = true` and `verify = verify_none` that was previously allowed will fail to load after applying this fix and must be manually fixed.
-   [#10951](https://github.com/emqx/emqx/pull/10951) Fixed the issue in MQTT-SN gateway when the `mountpoint` did not take effect on message publishing.
-   [#10943](https://github.com/emqx/emqx/pull/10943) Deprecated UDP mcast mechanism for cluster discovery.
    This feature has been planed for deprecation since 5.0 mainly due to the lack of actual production use. This feature code is not yet removed in 5.1, but the document interface is demoted.
-   [#10902](https://github.com/emqx/emqx/pull/10902) Avoid syncing cluser.hocon file from the nodes running a newer version than the self-node.
    During cluster rolling upgrade, if an older version node has to restart due to whatever reason, if it copies the `cluster.hocon` file from a newer version node, it may fail to start. After this fix, the older version node will not copy the `cluster.hocon` file from a newer, so it will use its own `cluster.hocon` file to start.
-   [#10967](https://github.com/emqx/emqx/pull/10967) Fixed error message formatting in rebalance API: previously they could be displayed as unclear dumps of internal Erlang structures.
    Added `wait_health_check` option to node evacuation CLI and API. This is a time interval when the node reports "unhealthy status" without beginning actual evacuation. We need this to allow a Load Balancer (if any) to remove the evacuated node from balancing and not forward (re)connecting clients to the evacuated node.
-   [#10911](https://github.com/emqx/emqx/pull/10911) The error message and log entry that appear when one tries to create a bridge with a name the exceeds 255 bytes is now easier to understand.
-   [#10983](https://github.com/emqx/emqx/pull/10983) Fixed the issue when mqtt clients could not connect over TLS if the listener was configured to use TLS v1.3 only.
    The problem was that TLS connection was trying to use options incompatible with TLS v1.3.
-   [#10977](https://github.com/emqx/emqx/pull/10977) Fixed the delay in updating subscription count metric and corrected configuration issues in Stomp gateway.
-   [#10950](https://github.com/emqx/emqx/pull/10950) Fixed the issue where the `enable_qos` option does not take effect in the MQTT-SN gateway.
-   [#10999](https://github.com/emqx/emqx/pull/10999) Changed schema validation for Kafka fields 'Partition Count Refresh Interval' and 'Offset Commit Interval' to avoid accepting values larger then maximum allowed.
-   [#10997](https://github.com/emqx/emqx/pull/10997) The ClickHouse bridge had a problem that could cause messages to be dropped when the ClickHouse server is closed while sending messages even when the request_ttl is set to infinity. This has been fixed by treating errors due to a closed connection as recoverable errors.
-   [#10994](https://github.com/emqx/emqx/pull/10994) Redacted `proxy-authorization` headers as used by HTTP connector to avoid leaking secrets into log files.
-   [#10996](https://github.com/emqx/emqx/pull/10996) For any unknown HTTP/API request, the default response is a 404 error rather than the dashboard's index.html.
-   [#11005](https://github.com/emqx/emqx/pull/11005) Fixed the issue where the `method` field cannot be correctly printed in the trace logs of AuthN HTTP.
-   [#11006](https://github.com/emqx/emqx/pull/11006) Fixed QUIC listeners's default cert file paths.
    Prior to this change, the default cert file paths are prefixed with environment variable `${EMQX_ETC_DIR}` which were not interpolated before used in QUIC listeners.
-   [#10998](https://github.com/emqx/emqx/pull/10998) Do not allow `batch_size` option for MongoDB bridge resource. MongoDB connector currently does not support batching, the `batch_size` config value is forced to be 1 if provided.
-   [#10955](https://github.com/emqx/emqx/pull/10955) Fixed the issue in MQTT-SN gateway where deleting Predefined Topics configuration does not work.
-   [#11025](https://github.com/emqx/emqx/pull/11025) Fixed a `case_clause` error that could arise in race conditions in Pulsar Producer bridge.
-   [#11030](https://github.com/emqx/emqx/pull/11030) Improved error messages when a validation error occurs while using the Listeners HTTP API.
-   [#11033](https://github.com/emqx/emqx/pull/11033) Deprecated the `mountpoint` field in `AuthenticateRequest` in ExProto gateway.
    This field was introduced in e4.x, but in fact, in e5.0 we have provided
    `gateway.exproto.mountpoint` for configuration, so there is no need to override
    it through the Authenticate request.

    Additionally, updates the default value of `subscriptions_max`, `inflight_max`,
    `mqueue_max` to `infinity`.
-   [#11040](https://github.com/emqx/emqx/pull/11040) Fixed a health check issue for Kafka Producer that could lead to loss of messages when the connection to Kafka's brokers were down.
-   [#11038](https://github.com/emqx/emqx/pull/11038) Fixed a health check issue for Pulsar Producer that could lead to loss of messages when the connection to Pulsar's brokers were down.
-   [#11042](https://github.com/emqx/emqx/pull/11042) Fixed crash on REST API `GET /listeners` when listener's `max_connections` is set to a string.
-   [#11028](https://github.com/emqx/emqx/pull/11028) Disallowed using multiple TLS versions in the listener config that include tlsv1.3 but exclude tlsv1.2.
    Using TLS configuration with such version gap caused connection errors.
    Additionally, drop and log TLS options that are incompatible with the selected TLS version(s).

    Note: any old listener configuration with the version gap described above will fail to load
    after applying this fix and must be manually fixed.
-   [#11031](https://github.com/emqx/emqx/pull/11031) Fixed credential validation when creating bridge and checking status for InfluxDB Bridges.
-   [#11056](https://github.com/emqx/emqx/pull/11056) Fixed the issue where newly created listeners sometimes do not start properly.
    When you delete a system default listener and add a new one named 'default', it will not start correctly.
    -   Fixed the bug where configuration failure on certain nodes can cause Dashboard unavailability.
-   [#11070](https://github.com/emqx/emqx/pull/11070) Fixed the problem that the `cluster.autoclean` configuration item does not take effect.
-   [#11092](https://github.com/emqx/emqx/pull/11092) and [#11100](https://github.com/emqx/emqx/pull/11100) Fixed problem when replicat nodes were unable to connect to the core node due to timeout in `mria_lb:core_nodes()` call.
    Relevant mria pull request: [https://github.com/emqx/mria/pull/143](https://github.com/emqx/mria/pull/143)




## 5.0.26

_Release Date: 2023-05-29_

### Enhancements

- [#10584](https://github.com/emqx/emqx/pull/10584) Add log level configuration to SSL communication

- [#10702](https://github.com/emqx/emqx/pull/10702) Introduce a more straightforward configuration option `keepalive_multiplier` and
  deprecate the old `keepalive_backoff` configuration.
  After this enhancement, EMQX checks the client's keepalive timeout status
  period by multiplying the "Client Requested Keepalive Interval" with `keepalive_multiplier`.

- [#10713](https://github.com/emqx/emqx/pull/10713) We hide the request_timeout in resource_option of the webhook to keep it consistent with the http request_timeout of the webhook.
  From now on, when configuring a webhook through API or configuration files,
  it is no longer necessary to configure the request_timeout of the resource. Only configuring the http request_timeout is sufficient, and the request_timeout in the resource will automatically be consistent with the http request_timeout.

- [#10511](https://github.com/emqx/emqx/pull/10511) Improve the security and privacy of some resource logs by masking sensitive information in the data.

- [#10678](https://github.com/emqx/emqx/pull/10678) Optimized counter increment calls to avoid work if increment is zero.

- [#10690](https://github.com/emqx/emqx/pull/10690) Added a retry mechanism to webhook bridge that attempts to improve throughput.

  This optimization retries request failures without blocking the buffering layer, which can improve throughput in situations of high messaging rate.

- [#10698](https://github.com/emqx/emqx/pull/10698) Optimize memory usage when accessing the configuration during runtime.

### Bug Fixes

- [#10340](https://github.com/emqx/emqx/pull/10340) Fixed the issue that could lead to crash logs being printed when stopping EMQX via systemd.


- [#10563](https://github.com/emqx/emqx/pull/10563) Corrected an issue where the no_local flag was not functioning correctly.

- [#10600](https://github.com/emqx/emqx/pull/10600) Deleted emqx_statsd application.

- [#10653](https://github.com/emqx/emqx/pull/10653) Store gateway authentication TLS certificates and keys in the data directory.

- [#10677](https://github.com/emqx/emqx/pull/10677) In Rule API, reapond with 404 HTTP error code when trying to delete a rule that does not exist.

- [#10682](https://github.com/emqx/emqx/pull/10682) Fix the timestamp for the will message is incorrectly assigned at the session creation time, now this timestamp is the disconnected time of the session.

- [#10701](https://github.com/emqx/emqx/pull/10701) RPM package for Amazon Linux 2 did not support TLS v1.3 as it was assembled with Erlang/OTP built with openssl 1.0.

- [#10715](https://github.com/emqx/emqx/pull/10715) Postpone trimming the connection information structure until after `client.connected` hooks have been executed. These hooks once again have access to the client's peer certificate.

- [#10717](https://github.com/emqx/emqx/pull/10717) Fixed an issue where the buffering layer processes could use a lot of CPU when inflight window is full.

- [#10724](https://github.com/emqx/emqx/pull/10724) A summary has been added for all endpoints in the HTTP API documentation (accessible at "http://emqx_host_name:18083/api-docs").

- [#10726](https://github.com/emqx/emqx/pull/10726) Validate Health Check Interval and Auto Restart Interval against the range from 1ms to 1 hour.

- [#10728](https://github.com/emqx/emqx/pull/10728) Fixed an issue where the rule engine was unable to access variables exported by `FOREACH` in the `DO` clause.

  Given a payload: `{"date": "2023-05-06", "array": ["a"]}`, as well as the following SQL statement:

  ```
  FOREACH payload.date as date, payload.array as elem
  DO date, elem
  FROM "t/#"
  ```

  Prior to the fix, the `date` variable exported by `FOREACH` could not be accessed in the `DO` clause of the above SQL, resulting in the following output for the SQL statement:
  `[{"elem": "a","date": "undefined"}]`.
  After the fix, the output of the SQL statement is: `[{"elem": "a","date": "2023-05-06"}]`

- [#10737](https://github.com/emqx/emqx/pull/10737) Fix the issue where the HTTP API interface of Gateway cannot handle ClientIDs with
  special characters, such as: `!@#$%^&*()_+{}:"<>?/`.

- [#10742](https://github.com/emqx/emqx/pull/10742) Check the correctness of the rules before saving the authorization file source.
  Previously, Saving wrong rules could lead to restart failure.

- [#10743](https://github.com/emqx/emqx/pull/10743) Fixes an issue where trying to get a bridge info or metrics could result in a crash when a node is joining a cluster.

- [#10746](https://github.com/emqx/emqx/pull/10746) Add missing support of the event `$events/delivery_dropped` into the rule engine test API `rule_test`.

- [#10747](https://github.com/emqx/emqx/pull/10747) Refactor date and time functions, `format_date` and `date_to_unix_ts`, in the rule engine to fix the implementation problem.

- [#10755](https://github.com/emqx/emqx/pull/10755) Fixed data bridge resource update race condition.

  In the 'delete + create' process for EMQX resource updates,
  long bridge creation times could cause dashboard request timeouts.
  If a bridge resource update was initiated before completion of its creation,
  it led to an erroneous deletion from the runtime, despite being present in the config file.

  This fix addresses the race condition in bridge resource updates,
  ensuring the accurate identification and addition of new resources,
  maintaining consistency between runtime and configuration file statuses.

- [#10760](https://github.com/emqx/emqx/pull/10760) Fix Internal Error 500 that occurred sometimes when bridge statistics page was updated while a node was (re)joining the cluster.

- [#10761](https://github.com/emqx/emqx/pull/10761) Fixing the issue where the default value of SSL certificate for Dashboard Listener was not correctly interpolated, which caused HTTPS to be inaccessible when verify_peer and cacertfile were using the default configuration.

- [#10785](https://github.com/emqx/emqx/pull/10785) Ensure `EMQX_LOG_DIR` is set by Windows boot script.

  The environment variable `EMQX_LOG_DIR` was missing in v5.0.25, caused EMQX Windows package fail to boot unless set by sysadmin.

- [#10801](https://github.com/emqx/emqx/pull/10801) Avoid duplicated percent decode the topic name in API `/topics/{topic}` and `/topics`.

- [#10809](https://github.com/emqx/emqx/pull/10809) Address `** ERROR ** Mnesia post_commit hook failed: error:badarg` error messages happening during node shutdown or restart.
  Mria pull request: https://github.com/emqx/mria/pull/142

- [#10817](https://github.com/emqx/emqx/pull/10817) Fix the error of not being able to configure `auto_restart_interval` as infinity

- [#10818](https://github.com/emqx/emqx/pull/10818) Fixing `emqx_ctl traces` command.

- [#10820](https://github.com/emqx/emqx/pull/10820) In case the cluster updated license before the new node join in. The new node will not apply the updated license.
  After this change, the new joined node will use the cluster's license key.

  Sometimes the new node must start with a outdated license.
  e.g. use emqx-operator deployed and needed to scale up after license expired.
  At the time the cluster's license key already updated by API/CLI, but the new node won't use it.

- [#10833](https://github.com/emqx/emqx/pull/10833) Only include enabled authenticators and authorizers in telemetry report, not all of them.

- [#10851](https://github.com/emqx/emqx/pull/10851) Obfuscated sensitive data in the bad API logging.

## 5.0.25

_Release Date: 2023-05-12_

### Enhancements

- [#10568](https://github.com/emqx/emqx/pull/10568) Add shutdown counter information to `emqx ctl listeners` command

- [#10571](https://github.com/emqx/emqx/pull/10571) Do not emit useless crash report when EMQX stops.
  Previously, when EMQX (and `emqx_topic_metrics` in particular) stopped and removed underlying tables, some messages were still being handled and crashed.

- [#10588](https://github.com/emqx/emqx/pull/10588) Increase the time precision of trace logs from second to microsecond.
  For example, change from `2023-05-02T08:43:50+00:00` to `2023-05-02T08:43:50.237945+00:00`.

- [#10623](https://github.com/emqx/emqx/pull/10623) Renamed `max_message_queue_len` to `max_mailbox_size` in the `force_shutdown` configuration. Old name is kept as an alias, so this change is backward compatible.

- [#10417](https://github.com/emqx/emqx/pull/10417) Improve get config performance by eliminating temporary references.

- [#10525](https://github.com/emqx/emqx/pull/10525) Reduce resource usage per MQTT packet handling.

- [#10528](https://github.com/emqx/emqx/pull/10528) Reduce memory footprint in hot code path.

- [#10573](https://github.com/emqx/emqx/pull/10573) Improved performance of Webhook bridge when using synchronous query mode.
  This also should improve the performance of other bridges when they are configured with no batching.

- [#10591](https://github.com/emqx/emqx/pull/10591) Improve the configuration of the limiter.

  - Simplify the memory representation of the limiter configuration.
  - Make sure the node-level limiter can really work when the listener's limiter configuration is omitted.

- [#10625](https://github.com/emqx/emqx/pull/10625) Simplify limiter configuration.
  - Reduce the complexity of the limiter's configuration.
    e.g. now users can use `limiter.messages_rate = 1000/s` to quickly set the node-level limit for the message publish.
  - Update the `configs/limiter` API to suit this refactor.

### Bug Fixes

- [#10548](https://github.com/emqx/emqx/pull/10548) Fixed a race condition in the HTTP driver that would result in an error rather than a retry of the request.
  Related fix in the driver: https://github.com/emqx/ehttpc/pull/45

- [#10556](https://github.com/emqx/emqx/pull/10556) Wrap potentially sensitive data in `emqx_connector_http` if `Authorization` headers are being passed at initialization.

- [#10659](https://github.com/emqx/emqx/pull/10659) Fix the issue where emqx cannot start when `sysmon.os.mem_check_interval` is disabled.

## 5.0.24

_Release Date: 2023-04-26_

### Enhancements

- [#10457](https://github.com/emqx/emqx/pull/10457) Deprecates the integration with StatsD.

  There seemd to be no user using StatsD integration, so we have decided to hide this feature
  for now. We will either remove or revive it based on requirements in the future.

- [#10458](https://github.com/emqx/emqx/pull/10458) Set the level of plugin configuration options to low level,
  in most cases, users only need to manage plugins on the dashboard
  without the need for manual modification, so we lowered the level.

- [#10491](https://github.com/emqx/emqx/pull/10491) Rename `etcd.ssl` to `etcd.ssl_options` to keep all of SSL options consistent in the configuration file.

- [#10512](https://github.com/emqx/emqx/pull/10512) Improved the storage format of Unicode characters in data files,
  Now we can store Unicode characters normally.
  For example: "SELECT \* FROM \"t/1\" WHERE clientid = \"-测试专用-\""

- [#10487](https://github.com/emqx/emqx/pull/10487) Optimize the instance of limiter for whose rate is `infinity` to reduce memory and CPU usage.

- [#10490](https://github.com/emqx/emqx/pull/10490) Remove the default limit of connect rate which used to be `1000/s`

### Bug Fixes

- [#10407](https://github.com/emqx/emqx/pull/10407) Improve 'emqx_alarm' performance by using Mnesia dirty operations and avoiding
  unnecessary calls from 'emqx_resource_manager' to reactivate alarms that have been already activated.
  Use new safe 'emqx_alarm' API to activate/deactivate alarms to ensure that emqx_resource_manager
  doesn't crash because of alarm timeouts.
  The crashes were possible when the following conditions co-occurred:

  - a relatively high number of failing resources, e.g. bridges tried to activate alarms on re-occurring errors;
  - the system experienced a very high load.

- [#10420](https://github.com/emqx/emqx/pull/10420) Fix HTTP path handling when composing the URL for the HTTP requests in authentication and authorization modules.

  - Avoid unnecessary URL normalization since we cannot assume that external servers treat original and normalized URLs equally. This led to bugs like [#10411](https://github.com/emqx/emqx/issues/10411).
  - Fix the issue that path segments could be HTTP encoded twice.

- [#10422](https://github.com/emqx/emqx/pull/10422) Fixed a bug where external plugins could not be configured via environment variables in a lone-node cluster.

- [#10448](https://github.com/emqx/emqx/pull/10448) Fix a compatibility issue of limiter configuration introduced by v5.0.23 which broke the upgrade from previous versions if the `capacity` is `infinity`.

  In v5.0.23 we have replaced `capacity` with `burst`. After this fix, a `capacity = infinity` config will be automatically converted to equivalent `burst = 0`.

- [#10449](https://github.com/emqx/emqx/pull/10449) Validate the ssl_options and header configurations when creating authentication http (`authn_http`).
  Prior to this, incorrect `ssl` configuration could result in successful creation but the entire authn being unusable.

- [#10455](https://github.com/emqx/emqx/pull/10455) Fixed an issue that could cause (otherwise harmless) noise in the logs.

  During some particularly slow synchronous calls to bridges, some late replies could be sent to connections processes that were no longer expecting a reply, and then emit an error log like:

  ```
  2023-04-19T18:24:35.350233+00:00 [error] msg: unexpected_info, mfa: emqx_channel:handle_info/2, line: 1278, peername: 172.22.0.1:36384, clientid: caribdis_bench_sub_1137967633_4788, info: {#Ref<0.408802983.1941504010.189402>,{ok,200,[{<<"cache-control">>,<<"max-age=0, ...">>}}
  ```

  Those logs are harmless, but they could flood and worry the users without need.

- [#10462](https://github.com/emqx/emqx/pull/10462) Deprecate config `broker.shared_dispatch_ack_enabled`.
  This was designed to avoid dispatching messages to a shared-subscription session which has the client disconnected.
  However since v5.0.9, this feature is no longer useful because the shared-subscrption messages in a expired session will be redispatched to other sessions in the group.
  See also: https://github.com/emqx/emqx/pull/9104

- [#10463](https://github.com/emqx/emqx/pull/10463) Improve bridges API error handling.
  If Webhook bridge URL is not valid, bridges API will return '400' error instead of '500'.

- [#10484](https://github.com/emqx/emqx/pull/10484) Fix the issue that the priority of the configuration cannot be set during rolling upgrade.
  For example, when authorization is modified in v5.0.21 and then upgraded v5.0.23 through rolling upgrade,
  the authorization will be restored to the default.

- [#10495](https://github.com/emqx/emqx/pull/10495) Add the limiter API `/configs/limiter` which was deleted by mistake back.

- [#10500](https://github.com/emqx/emqx/pull/10500) Add several fixes, enhancements and features in Mria:

  - protect `mria:join/1,2` with a global lock to prevent conflicts between
    two nodes trying to join each other simultaneously
    [Mria PR](https://github.com/emqx/mria/pull/137)
  - implement new function `mria:sync_transaction/4,3,2`, which blocks the caller until
    a transaction is imported to the local node (if the local node is a replicant, otherwise,
    it behaves exactly the same as `mria:transaction/3,2`)
    [Mria PR](https://github.com/emqx/mria/pull/136)
  - optimize `mria:running_nodes/0`
    [Mria PR](https://github.com/emqx/mria/pull/135)
  - optimize `mria:ro_transaction/2` when called on a replicant node
    [Mria PR](https://github.com/emqx/mria/pull/134).

- [#10518](https://github.com/emqx/emqx/pull/10518) Add the following fixes and features in Mria:
  - call `mria_rlog:role/1` safely in mria_membership to ensure that mria_membership
    gen_server won't crash if RPC to another node fails
    [Mria PR](https://github.com/emqx/mria/pull/139)
  - Add extra field to ?rlog_sync table to facilitate extending this functionality in future
    [Mria PR](https://github.com/emqx/mria/pull/138).

## 5.0.23

_Release Date: 2023-04-18_

### Enhancements

- [#10156](https://github.com/emqx/emqx/pull/10156) Change the priority of the configuration:

  1. If it is a new installation of EMQX, the priority of configuration is `ENV > emqx.conf > HTTP API`.
  2. If EMQX is upgraded from an old version (i.e., the cluster-override.conf file still exists in EMQX's data directory), then the configuration priority remains the same as before. That is, `HTTP API > ENV > emqx.conf`.

  Deprecated data/configs/local-override.conf.

  Stabilizing the HTTP API for hot updates.

- [#10354](https://github.com/emqx/emqx/pull/10354) More specific error messages when configure with bad max_heap_size value.
  Log current value and the max value when the `message_queue_too_long` error is thrown.

- [#10359](https://github.com/emqx/emqx/pull/10359) Metrics now are not implicitly collected in places where API handlers don't make any use of them. Instead, a separate backplane RPC gathers cluster-wide metrics.

- [#10373](https://github.com/emqx/emqx/pull/10373) Deprecate the trace.payload_encode configuration.
  Add payload_encode=[text,hidden,hex] option when creating a trace via HTTP API.

- [#10389](https://github.com/emqx/emqx/pull/10389) Unify the config formats for `cluster.core_nodes` and `cluster.statics.seeds`.
  Now they both support formats in array `["emqx1@127.0.0.1", "emqx2@127.0.0.1"]` or semicolon-separated string `"emqx1@127.0.0.1,emqx2@127.0.0.1"`.

- [#10391](https://github.com/emqx/emqx/pull/10391) Hide a large number of advanced options to simplify the configuration file.

  That includes `rewrite`, `topic_metric`, `persistent_session_store`, `overload_protection`,
  `flapping_detect`, `conn_congestion`, `stats,auto_subscribe`, `broker_perf`,
  `shared_subscription_group`, `slow_subs`, `ssl_options.user_lookup_fun` and some advance items
  in `node` and `dashboard` section, [#10358](https://github.com/emqx/emqx/pull/10358),
  [#10381](https://github.com/emqx/emqx/pull/10381), [#10385](https://github.com/emqx/emqx/pull/10385).

- [#10392](https://github.com/emqx/emqx/pull/10392) A new function to convert a formatted date to an integer timestamp has been added: date_to_unix_ts/3

- [#10404](https://github.com/emqx/emqx/pull/10404) Change the default queue mode for buffer workers to `memory_only`.
  Before this change, the default queue mode was `volatile_offload`. When under high message rate pressure and when the resource is not keeping up with such rate, the buffer performance degraded a lot due to the constant disk operations.

- [#10426](https://github.com/emqx/emqx/pull/10426) Optimize the configuration priority mechanism to fix the issue where the configuration
  changes made to `etc/emqx.conf` do not take effect after restarting EMQX.

  More introduction about the new mechanism: [Configure Override Rules](https://docs.emqx.com/en/enterprise/v5.0/configuration/configuration.html#configure-override-rules)

- [#10376](https://github.com/emqx/emqx/pull/10376) Simplify the configuration of the limiter feature and optimize some codes

  - Rename `message_in` to `messages`
  - Rename `bytes_in` to `bytes`
  - Use `burst` instead of `capacity`
  - Hide non-importance fields
  - Optimize limiter instances in different rate settings

- [#10430](https://github.com/emqx/emqx/pull/10430) Simplify the configuration of the `retainer` feature.
  - Mark `flow_control` as non-importance field.

### Bug Fixes

- [#10369](https://github.com/emqx/emqx/pull/10369) Fix error in `/api/v5/monitor_current` API endpoint that happens when some EMQX nodes are down.

  Prior to this fix, sometimes the request returned HTTP code 500 and the following message:

  ```
  {"code":"INTERNAL_ERROR","message":"error, badarg, [{erlang,'++',[{error,nodedown},[{node,'emqx@10.42.0.150'}]], ...
  ```

- [#10410](https://github.com/emqx/emqx/pull/10410) Fix config check failed when gateways are configured in emqx.conf.
  This issue was first introduced in v5.0.22 via [#10278](https://github.com/emqx/emqx/pull/10278), the boot-time config check was missing.

## 5.0.22

_Release Date: 2023-04-13_

### Enhancements

- [#10077](https://github.com/emqx/emqx/pull/10077) Add support for QUIC TLS password protected certificate file.

- [#10128](https://github.com/emqx/emqx/pull/10128) Add support for OCSP stapling for SSL MQTT listeners.

- [#10164](https://github.com/emqx/emqx/pull/10164) Add CRL check support for TLS MQTT listeners.

- [#10206](https://github.com/emqx/emqx/pull/10206) Decouple the query mode from the underlying call mode for buffer
  workers.

  Prior to this change, setting the query mode of a resource
  such as a bridge to `sync` would force the buffer to call the
  underlying connector in a synchronous way, even if it supports async
  calls.

- [#10207](https://github.com/emqx/emqx/pull/10207) Use 'label' from i18n file as 'summary' in OpenAPI spec.

- [#10210](https://github.com/emqx/emqx/pull/10210) Unregister Mnesia post commit hook when Mria is being stopped.
  This fixes hook failures occasionally occurring on stopping/restarting Mria.

  [Mria PR](https://github.com/emqx/mria/pull/133)

- [#10224](https://github.com/emqx/emqx/pull/10224) Add the option to customize `clusterIP` in Helm chart, so that a user may set it to a fixed IP.

- [#10263](https://github.com/emqx/emqx/pull/10263) Add command 'eval-ex' for Elixir expression evaluation.

- [#10278](https://github.com/emqx/emqx/pull/10278) Refactor the directory structure of all gateways.

- [#10306](https://github.com/emqx/emqx/pull/10306) Add support for `async` query mode for most bridges.

  Before this change, some bridges (Cassandra, MongoDB, MySQL, Postgres, Redis, RocketMQ, TDengine) were only allowed to be created with a `sync` query mode.

- [#10318](https://github.com/emqx/emqx/pull/10318) Now, the rule engine language's FROM clause supports both strings enclosed in double quotes (") and single quotes (').

- [#10336](https://github.com/emqx/emqx/pull/10336) Add `/rule_engine` API endpoint to manage configuration of rule engine.

### Bug Fixes

- [#10145](https://github.com/emqx/emqx/pull/10145) Fix `bridges` API to report error conditions for a failing bridge as
  `status_reason`. Also when creating an alarm for a failing resource we include
  this error condition with the alarm's message.

- [#10154](https://github.com/emqx/emqx/pull/10154) Change the default `resume_interval` for bridges and connectors to be
  the minimum of `health_check_interval` and `request_timeout / 3`.
  Also exposes it as a hidden configuration to allow fine tuning.

  Before this change, the default values for `resume_interval` meant
  that, if a buffer ever got blocked due to resource errors or high
  message volumes, then, by the time the buffer would try to resume its
  normal operations, almost all requests would have timed out.

- [#10172](https://github.com/emqx/emqx/pull/10172) Fix the incorrect default ACL rule, which was:

  ```
  {allow, {username, "^dashboard?"}, subscribe, ["$SYS/#"]}.
  ```

  However, it should use `{re, "^dashboard$"}` to perform a regular expression match:

  ```
  {allow, {username, {re,"^dashboard$"}}, subscribe, ["$SYS/#"]}.
  ```

- [#10174](https://github.com/emqx/emqx/pull/10174) Upgrade library `esockd` from 5.9.4 to 5.9.6.
  Fix an unnecessary error level logging when a connection is closed before proxy protocol header is sent by the proxy.

- [#10195](https://github.com/emqx/emqx/pull/10195) Add labels to API schemas where description contains HTML and breaks formatting of generated documentation otherwise.

- [#10196](https://github.com/emqx/emqx/pull/10196) Use lower-case for schema summaries and descritptions to be used in menu of generated online documentation.

- [#10209](https://github.com/emqx/emqx/pull/10209) Fix bug where a last will testament (LWT) message could be published
  when kicking out a banned client.

- [#10211](https://github.com/emqx/emqx/pull/10211) Hide `broker.broker_perf` config and API documents.
  The two configs `route_lock_type` and `trie_compaction` are rarely used and requires a full cluster restart to take effect. They are not suitable for being exposed to users.
  Detailed changes can be found here: https://gist.github.com/zmstone/01ad5754b9beaeaf3f5b86d14d49a0b7/revisions

- [#10225](https://github.com/emqx/emqx/pull/10225) Allow installing a plugin if its name matches the beginning of another (already installed) plugin name.
  For example: if plugin "emqx_plugin_template_a" is installed, it must not block installing plugin "emqx_plugin_template".

- [#10226](https://github.com/emqx/emqx/pull/10226) Don't crash on validation error in `/bridges` API, return `400` instead.

- [#10237](https://github.com/emqx/emqx/pull/10237) Ensure we return `404` status code for unknown node names in `/nodes/:node[/metrics|/stats]` API.

- [#10242](https://github.com/emqx/emqx/pull/10242) Fixed a log data field name clash.
  Piror to this fix, some debug logs may report a wrong Erlang PID which may affect troubleshooting session takeover issues.

- [#10251](https://github.com/emqx/emqx/pull/10251) Consider bridges referenced in `FROM` rule clauses as dependencies.

  Before this fix, when one tried to delete an ingress rule referenced in an action like `select * from "$bridges/mqtt:ingress"`, the UI would not trigger a warning about dependent rule actions.

- [#10257](https://github.com/emqx/emqx/pull/10257) Fixed the issue where `auto_observe` was not working in LwM2M Gateway.

  Before the fix, OBSERVE requests were sent without a token, causing failures
  that LwM2M clients could not handle.

  After the fix, LwM2M Gateway can correctly observe the resource list carried by
  client, furthermore, unknown resources will be ignored and printing the following
  warning log:

  ```
  2023-03-28T18:50:27.771123+08:00 [warning] msg: ignore_observer_resource, mfa: emqx_lwm2m_session:observe_object_list/3, line: 522, peername: 127.0.0.1:56830, clientid: testlwm2mclient, object_id: 31024, reason: no_xml_definition
  ```

- [#10286](https://github.com/emqx/emqx/pull/10286) Enhance logging behaviour during boot failure.
  When EMQX fails to start due to corrupted configuration files, excessive logging is eliminated and no crash dump file is generated.

- [#10297](https://github.com/emqx/emqx/pull/10297) Keeps `eval` command backward compatible with v4 by evaluating only Erlang expressions, even on Elixir node. For Elixir expressions, use `eval-ex` command.

- [#10300](https://github.com/emqx/emqx/pull/10300) Fixed an issue where a build made with Elixir could not receive uploaded plugins until the `plugins` folder was created manually to receive the uploaded files.

- [#10313](https://github.com/emqx/emqx/pull/10313) Ensure that when the core or replicant node starting, the `cluster-override.conf` file is only copied from the core node.
  Previously, when sorting nodes by startup time, the core node may have copied this file from the replicant node.

- [#10314](https://github.com/emqx/emqx/pull/10314) Fix /monitor_current API so that it only looks at the current node.
  Fix /stats API to not crash when one or more nodes in the cluster are down.

- [#10315](https://github.com/emqx/emqx/pull/10315) Fix crash checking `limit` and `page` parameters in `/mqtt/delayed/messages` API call.

- [#10317](https://github.com/emqx/emqx/pull/10317) Do not expose listener level authentications before extensive verification.

- [#10323](https://github.com/emqx/emqx/pull/10323) For security reasons, the value of the `password` field in the API examples is replaced with `******`.

- [#10327](https://github.com/emqx/emqx/pull/10327) Don't increment 'actions.failed.unknown' rule metrics counter upon receiving unrecoverable bridge errors.
  This counter is displayed on the dashboard's rule overview tab ('Action statistics' - 'Unknown').
  The fix is only applicable for synchronous bridges, as all rule actions for asynchronous bridges
  are counted as successful (they increment 'actions.success' which is displayed as 'Action statistics' - 'Success').

## 5.0.21

_Release Date: 2023-03-24_

### Enhancements

- [#10022](https://github.com/emqx/emqx/pull/10022) Start releasing Rocky Linux 9 (compatible with Enterprise Linux 9) and MacOS 12 packages

- [#10139](https://github.com/emqx/emqx/pull/10139) Add `extraVolumeMounts` to EMQX Helm Chart, it will have the ability to mount the user-own files into the EMQX instance, for example, ACL rule files as mentioned in [#9052](https://github.com/emqx/emqx/issues/9052)

  Done of [#10116](https://github.com/emqx/emqx/issues/10116)

- [#9893](https://github.com/emqx/emqx/pull/9893) When connecting with the flag `clean_start=false`, EMQX will filter out messages that published by banned clients.
  Previously, the messages sent by banned clients may still be delivered to subscribers in this scenario.

- [#9986](https://github.com/emqx/emqx/pull/9986) For helm charts, add MQTT ingress bridge; and removed stale `mgmt` references.

- [#10123](https://github.com/emqx/emqx/pull/10123) Improve the performance of `/bridges` API.
  Earlier, when the number of nodes in the cluster was large or the node was busy, the API may have a request timeout.

- [#9998](https://github.com/emqx/emqx/pull/9998) Redact the HTTP request body in the authentication error logs for security reasons.

### Bug Fixes

- [#10013](https://github.com/emqx/emqx/pull/10013) Fix return type structure for error case in API schema for `/gateways/:name/clients`.
  u
- [#10014](https://github.com/emqx/emqx/pull/10014) In dashboard API for `/monitor(_current)/nodes/:node` return `404` instead of `400` if node does not exist.

- [#10026](https://github.com/emqx/emqx/pull/10026) Metrics are now only exposed via the /bridges/:id/metrics endpoint. Metrics are no longer returned in other API operations such as getting the list of all bridges, or in the response when a bridge has been created.

- [#10027](https://github.com/emqx/emqx/pull/10027) Allow setting node name from `EMQX_NODE__NAME` when running in docker.
  Prior to this fix, only `EMQX_NODE_NAME` is allowed.

- [#10050](https://github.com/emqx/emqx/pull/10050) Ensure Bridge API returns `404` status code consistently for resources that don't exist.

- [#10052](https://github.com/emqx/emqx/pull/10052) Improve daemon mode startup failure logs.

  Before this change, it was difficult for users to understand the reason for EMQX 'start' command failed to boot the node.
  The only information they received was that the node did not start within the expected time frame,
  and they were instructed to boot the node with 'console' command in the hope of obtaining some logs.
  However, the node might actually be running, which could cause 'console' mode to fail for a different reason.

  With this new change, when daemon mode fails to boot, a diagnosis is issued. Here are the possible scenarios:

  - If the node cannot be found from `ps -ef`, the user is instructed to find information in log files `erlang.log.*`.
  - If the node is found to be running but not responding to pings, the user is advised to check if the host name is resolvable and reachable.
  - If the node is responding to pings, but the EMQX app is not running, it is likely a bug. In this case, the user is advised to report a Github issue.

- [#10055](https://github.com/emqx/emqx/pull/10055) Fix: configuration parameter `mqtt.max_awaiting_rel` had no effect.

- [#10056](https://github.com/emqx/emqx/pull/10056) Fix `/bridges` API status code.

  - Return `400` instead of `403` in case of removing a data bridge that is dependent on an active rule.
  - Return `400` instead of `403` in case of calling operations (start|stop|restart) when Data-Bridging is not enabled.

- [#10066](https://github.com/emqx/emqx/pull/10066) Improve error messages for `/briges_probe` and `[/node/:node]/bridges/:id/:operation` API calls to make them more readable. And set HTTP status code to `400` instead of `500`.

- [#10074](https://github.com/emqx/emqx/pull/10074) Check if type in `PUT /authorization/sources/:type` matches `type` given in body of request.

- [#10079](https://github.com/emqx/emqx/pull/10079) Fix description of `shared_subscription_strategy`.

- [#10085](https://github.com/emqx/emqx/pull/10085) Consistently return `404` for all requests on non existent source in `/authorization/sources/:source[/*]`.

- [#10098](https://github.com/emqx/emqx/pull/10098) A crash with an error in the log file that happened when the MongoDB authorization module queried the database has been fixed.

- [#10100](https://github.com/emqx/emqx/pull/10100) Fix channel crash for slow clients with enhanced authentication.
  Previously, when the client was using enhanced authentication, but the Auth message was sent slowly or the Auth message was lost, the client process would crash.

- [#10107](https://github.com/emqx/emqx/pull/10107) For operations on `bridges API` if `bridge-id` is unknown we now return `404`
  instead of `400`. Also a bug was fixed that caused a crash if that was a node
  operation. Additionally we now also check if the given bridge is enabled when
  doing the cluster operation `start` . Affected endpoints:

  - [cluster] `/bridges/:id/:operation`,
  - [node] `/nodes/:node/bridges/:id/:operation`, where `operation` is one of
    `[start|stop|restart]`.
    Moreover, for a node operation, EMQX checks if node name is in our cluster and
    return `404` instead of `501`.

- [#10117](https://github.com/emqx/emqx/pull/10117) Fix an error occurring when a joining node doesn't have plugins that are installed on other nodes in the cluster.
  After this fix, the joining node will copy all the necessary plugins from other nodes.

- [#10118](https://github.com/emqx/emqx/pull/10118) Fix problems related to manual joining of EMQX replicant nodes to the cluster.
  Previously, after manually executing joining and then leaving the cluster, the `replicant` node can only run normally after restarting the node after joining the cluster again.

  [Mria PR](https://github.com/emqx/mria/pull/128)

- [#10119](https://github.com/emqx/emqx/pull/10119) Fix crash when `statsd.server` is set to an empty string.

- [#10124](https://github.com/emqx/emqx/pull/10124) The default heartbeat period for MongoDB has been increased to reduce the risk of too excessive logging to the MongoDB log file.

- [#10130](https://github.com/emqx/emqx/pull/10130) Fix garbled config display in dashboard when the value is originally from environment variables.
  For example, `env EMQX_STATSD__SERVER='127.0.0.1:8124' . /bin/emqx start` results in unreadable string (not '127.0.0.1:8124') displayed in Dashboard's Statsd settings page.
  Related PR: [HOCON#234](https://github.com/emqx/hocon/pull/234).

- [#10132](https://github.com/emqx/emqx/pull/10132) Fix some error logs generated by `systemctl stop emqx` command.
  Prior to the fix, the command was not stopping jq and os_mon applications properly.

- [#10144](https://github.com/emqx/emqx/pull/10144) Add `-setcookie` emulator flag when invoking `emqx ctl` to prevent problems with emqx cli when home directory is read only. Fixes [#10142](https://github.com/emqx/emqx/issues/10142).

- [#10157](https://github.com/emqx/emqx/pull/10157) Fixed default rate limit configuration not being applied correctly when creating a new listener.

## 5.0.20

_Release Date: 2023-03-10_

### Enhancements

- [#10059](https://github.com/emqx/emqx/pull/10059) Errors returned by rule engine API are formatted in a more human readable way rather than dumping the raw error including the stacktrace.

### Bug Fixes

- [#10032](https://github.com/emqx/emqx/pull/10032) When resources on some nodes in the cluster are still in the 'initializing/connecting' state, the `bridges/` API will crash due to missing Metrics information for those resources. This fix will ignore resources that do not have Metrics information.

- [#10044](https://github.com/emqx/emqx/pull/10044) Fix node information formatter for stopped nodes in the cluster. The bug was introduced by v5.0.18.

- [#10054](https://github.com/emqx/emqx/pull/10054) Fix the problem that the obfuscated password is used when using the `/bridges_probe` API to test the connection in Data-Bridge.

- [#10058](https://github.com/emqx/emqx/pull/10058) Deprecate unused QUIC TLS options.
  Only following TLS options are kept for the QUIC listeners:

  - cacertfile
  - certfile
  - keyfile
  - verify

- [#10076](https://github.com/emqx/emqx/pull/10076) Fix webhook bridge error handling: connection timeout should be a retriable error.
  Prior to this fix, connection timeout was classified as unrecoverable error and led to request being dropped.

- [#10078](https://github.com/emqx/emqx/pull/10078) Fix an issue that invalid QUIC listener setting could casue segfault.

- [#10084](https://github.com/emqx/emqx/pull/10084) Fix problem when joining core nodes running different EMQX versions into a cluster.

  [Mria PR](https://github.com/emqx/mria/pull/127)

- [#10086](https://github.com/emqx/emqx/pull/10086) Upgrade HTTP client ehttpc to `0.4.7`.
  Prior to this upgrade, HTTP clients for authentication, authorization and webhook may crash
  if `body` is empty but content-type HTTP header is set.
  For more details see [ehttpc PR#44](https://github.com/emqx/ehttpc/pull/44).

## 5.0.19

_Release Date: 2023-03-01_

### Bug Fixes

- [#10032](https://github.com/emqx/emqx/pull/10032) When the resource manager is busy trying to establish a connection with the remote, the resource might yet lack any metrics information. Prior to this fix, the `bridges/` API handler crashed in such circumstances.

- [#10037](https://github.com/emqx/emqx/pull/10037) Fix Swagger API doc rendering crash.
  In version 5.0.18, a bug was introduced that resulted in duplicated field names in the configuration schema. This, in turn, caused the Swagger schema generated to become invalid.

- [#10041](https://github.com/emqx/emqx/pull/10041) For influxdb bridge, added integer value placeholder annotation hint to `write_syntax` documentation.
  Also supported setting a constant value for the `timestamp` field.

- [#10042](https://github.com/emqx/emqx/pull/10042) Improve behavior of the `replicant` nodes when the `core` cluster becomes partitioned (for example when a core node leaves the cluster).
  Previously, the replicant nodes were unable to rebalance connections to the core nodes, until the core cluster became whole again.
  This was indicated by the error messages: `[error] line: 182, mfa: mria_lb:list_core_nodes/1, msg: mria_lb_core_discovery divergent cluster`.

  [Mria PR](https://github.com/emqx/mria/pull/123/files)

- [#10043](https://github.com/emqx/emqx/pull/10043) Fixed two bugs introduced in v5.0.18.

  - The environment varialbe `SSL_DIST_OPTFILE` was not set correctly for non-boot commands.
  - When cookie is overridden from environment variable, EMQX node is unable to start.

- [#10044](https://github.com/emqx/emqx/pull/10044) Fix node information formatter for stopped nodes in the cluster.

## 5.0.18

_Release Date: 2023-02-24_

### Enhancements

- [#10019](https://github.com/emqx/emqx/pull/10019) Add low level tuning settings for QUIC listeners.

- [#9213](https://github.com/emqx/emqx/pull/9213) Add pod disruption budget to helm chart

- [#9949](https://github.com/emqx/emqx/pull/9949) QUIC transport Multistreams support and QUIC TLS cacert support.

- [#9967](https://github.com/emqx/emqx/pull/9967) New common TLS option 'hibernate_after' to reduce memory footprint per idle connecion, default: 5s.

### Bug Fixes

- [#10009](https://github.com/emqx/emqx/pull/10009) Validate `bytes` param to `GET /trace/:name/log` to not exceed signed 32bit integer.

- [#10015](https://github.com/emqx/emqx/pull/10015) To prevent errors caused by an incorrect EMQX node cookie provided from an environment variable,
  we have implemented a fail-fast mechanism.
  Previously, when an incorrect cookie was provided, the command would still attempt to ping the node,
  leading to the error message 'Node xxx not responding to pings'.
  With the new implementation, if a mismatched cookie is detected,
  a message will be logged to indicate that the cookie is incorrect,
  and the command will terminate with an error code of 1 without trying to ping the node.

- [#10020](https://github.com/emqx/emqx/pull/10020) Fix bridge metrics when running in async mode with batching enabled (`batch_size` > 1).

- [#10021](https://github.com/emqx/emqx/pull/10021) Fix error message when the target node of `emqx_ctl cluster join` command is not running.

- [#9939](https://github.com/emqx/emqx/pull/9939) Allow 'emqx ctl cluster' command to be issued before Mnesia starts.
  Prior to this change, EMQX `replicant` could not use `manual` discovery strategy.
  Now it's possible to join cluster using 'manual' strategy.

- [#9958](https://github.com/emqx/emqx/pull/9958) Fix bad http response format when client ID is not found in `clients` APIs

- [#9961](https://github.com/emqx/emqx/pull/9961) Avoid parsing config files for node name and cookie when executing non-boot commands in bin/emqx.

- [#9974](https://github.com/emqx/emqx/pull/9974) Report memory usage to statsd and prometheus using the same data source as dashboard.
  Prior to this fix, the memory usage data source was collected from an outdated source which did not work well in containers.

- [#9978](https://github.com/emqx/emqx/pull/9978) Fixed configuration issue when choosing to use SSL for a Postgres connection (`authn`, `authz` and bridge).
  The connection could fail to complete with a previously working configuration after an upgrade from 5.0.13 to newer EMQX versions.

- [#9997](https://github.com/emqx/emqx/pull/9997) Fix Swagger API schema generation. `deprecated` metadata field is now always boolean, as [Swagger specification](https://swagger.io/specification/) suggests.

## 5.0.17

_Release Date: 2023-02-13_

### Enhancements

- [#9802](https://github.com/emqx/emqx/pull/9802) Support HAProxy protocol for HTTP API.

- [#9871](https://github.com/emqx/emqx/pull/9871) Allow the placeholder to be anywhere in the topic for `authz` rules.
  e.g:
  `{allow, {username, "who"}, publish, ["t/foo${username}boo/${clientid}xxx"]}.`

- [#9910](https://github.com/emqx/emqx/pull/9910) Add `start` operation to bridges API to allow manual reconnect after failure.

- [#9917](https://github.com/emqx/emqx/pull/9917) Stop building -alpine docker image because it's size is larger than the regular one based on debian slim

- [#9930](https://github.com/emqx/emqx/pull/9930) Expose the stats `live_connections.count` and `live_connections.max` to Prometheus.

- [#9936](https://github.com/emqx/emqx/pull/9936) Disable disksup (part of os_mon) in releases by default, no warnings are issued when a disk error occurs.

- [#9954](https://github.com/emqx/emqx/pull/9954) Improve bridge performance

### Bug fixes

- [#9864](https://github.com/emqx/emqx/pull/9864) Fix the exclusive topics aren't removed when the session has already been cleaned.

- [#9875](https://github.com/emqx/emqx/pull/9875) Return `400` if a broken plugin package is uploaded from HTTP API, also cleanup if plugin is not accepted.

- [#9916](https://github.com/emqx/emqx/pull/9916) Fix MQTT bridge fails to verify TLS wildcard server certificate.

- [#9922](https://github.com/emqx/emqx/pull/9922) Fix the issue with the bridge resource buffer where it might become stuck if enough async queries fill the inflight window full before failing with retryable errors.

- [#9923](https://github.com/emqx/emqx/pull/9923) Fix REPORT_CB/2 CRASH error logs when errors happen during boot-up or shutdown.

- [#9938](https://github.com/emqx/emqx/pull/9938) Report some egress MQTT bridge errors as recoverable, and thus retryable.

- [#9946](https://github.com/emqx/emqx/pull/9946) Add back `reconnect_interval` as deprecated field for MQTT bridge.
  The field was removed from v5.0.16/e5.0.0 by mistake, caused new version unable to start on old config.
  Now it's added back as deprecated (config value is ignored if provided).

- [#9951](https://github.com/emqx/emqx/pull/9951) Propagate errors from operations (`start|stop|restart`) on bridges API if called for all nodes.

- [#9952](https://github.com/emqx/emqx/pull/9952) Disallow subscribing with QoS 2 for ingress MQTT bridges.
  Allow user to configure `clean_start` option for ingress MQTT bridges, however.

## 5.0.16

_Release Date: 2023-02-02_

### Bug fixes

- [#9824](https://github.com/emqx/emqx/pull/9824) The `topics/{topic}` API endpoint would return `500 - Internal Error` if a topic had multiple routes. This is fixed by returning a list of routes.

- [#9832](https://github.com/emqx/emqx/pull/9832) Improve error log when bridge in 'sync' mode timed out to get response.

- [#9834](https://github.com/emqx/emqx/pull/9834) Allow `mqtt.idle_timeout` to be set to `infinity`

- [#9839](https://github.com/emqx/emqx/pull/9839) Make sure that the content of an authorization header that users have specified for a webhook bridge is not printed to log files.

- [#9884](https://github.com/emqx/emqx/pull/9884) Do not resume all buffer workers on successful health check of any individual resource.
  Previously after any successful healthcheck, all buffer workers (for all resources) were resumed

## 5.0.15

_Release Date: 2023-01-20_

### Enhancements

- [#9569](https://github.com/emqx/emqx/pull/9569) Refactor `/authorization/sources/built_in_database/` by adding `rules/` to the path.

- [#9585](https://github.com/emqx/emqx/pull/9585) `/bridges_probe` API endpoint to test params for creating a new data bridge.

- [#9586](https://github.com/emqx/emqx/pull/9586) Basic auth is no longer allowed for API calls, must use API key instead.

- [#9628](https://github.com/emqx/emqx/pull/9628) Expose additional resource configuration parameters: `start_after_created` and `start_timeout`.

- [#9722](https://github.com/emqx/emqx/pull/9722) Add the following configuration options for Pushing metrics to Prometheus Push Gateway:

  - `headers`: Allows custom HTTP request headers.
  - `job_name`: allows to customize the name of the Job pushed to Push Gateway.

- [#9725](https://github.com/emqx/emqx/pull/9725) Remove the config `auto_reconnect` from the emqx_authz, emqx_authn and data-bridge componets.
  This is because we have another config with similar functions: `resource_opts.auto_restart_interval`。

  The functions of these two config are difficult to distinguish, which will lead to confusion.
  After this change, `auto_reconnect` will not be configurable (always be true), and the underlying
  drivers that support this config will automatically reconnect the abnormally disconnected
  connection every `2s`.

  And the config `resource_opts.auto_restart_interval` is still available for user.
  It is the time interval that emqx restarts the resource when the connection cannot be
  established for some reason.

- [#9736](https://github.com/emqx/emqx/pull/9736) Refactor of /bridges API to make it more consistent with other APIs:

  - bridge enable/disable is now done via the endpoint `/bridges/{id}/enable/[true,false]`
  - `/bridges/{id}/operation/{operation}` endpoints are now `/bridges/{id}/{operation}`
  - metrics are moved out from the GET `/bridges/{id}` response and can now be fetched via `/bridges/{id}/metrics`
  - the `bridges/{id}/reset_metrics` endpoint is now `/bridges/{id}/metrics/reset`

- [#9774](https://github.com/emqx/emqx/pull/9774) Add a password complexity requirement when adding or modifying Dashboard users via the API.
  Now password must contain at least 2 of alphabetic, numeric and special characters,
  and must be 8 to 64 characters long.

### Bug fixes

- [#9626](https://github.com/emqx/emqx/pull/9626) Return authorization settings with default values.
  The authorization cache is enabled by default, but due to the missing default value in `GET` response of `/authorization/settings`, it seemed to be disabled from the dashboard.

- [#9680](https://github.com/emqx/emqx/pull/9680) Fix the problem that username and password authentication is mandatory in Influxdb v1 write API.

- [#9726](https://github.com/emqx/emqx/pull/9726) Client fuzzy search API results were missing information which could tell if more results are available in the next pages, this is now fixed by providing `hasnext` flag in the response.

- [#9735](https://github.com/emqx/emqx/pull/9735) Password information has been removed from information log messages for http, ldap, mongo, mqtt, mysql, pgsql and redis.

- [#9748](https://github.com/emqx/emqx/pull/9748) Listeners not configured with `max_connections` will cause the cluster `/listeners` API to return 500 error.

- [#9749](https://github.com/emqx/emqx/pull/9749) In some cases search APIs could respond with an incorrect `count` value in the metadata, that is usually much bigger than expected, this is now fixed.

- [#9750](https://github.com/emqx/emqx/pull/9750) Reload overriding configs after boot.
  Prior to this change, two configs were allow to change from dashboard, but will not take effect after reboot:

  - Logging (such as level)
  - Prometheus configs

- [#9751](https://github.com/emqx/emqx/pull/9751) Fix that obsoleted cert file will not be deleted after the listener is updated/deleted

- [#9763](https://github.com/emqx/emqx/pull/9763) Fix an authentication exception when password is not provided

- [#9765](https://github.com/emqx/emqx/pull/9765) Parse decimals as password from environment variable overrides correctly.
  Prior to this change, config values for passwords are not allowed to be decimals.
  e.g. `EMQX_FOOBAR__PASSWORD=12344` or `emqx.foobar.password=1234`
  would result in a type check error, unless quoted as:
  `EMQX_FOOBAR__PASSWORD='"12344"'` or `emqx.foobar.password="1234"`.
  After this fix, the value does not have to be quoted.

- [#9769](https://github.com/emqx/emqx/pull/9769) Fix Erlang shell prompt version prefix. e5.0.15 -> v5.0.15

- [#9780](https://github.com/emqx/emqx/pull/9780) When creating disk queue directory for resource worker, substitute ':' with '-' in worker id.

- [#9781](https://github.com/emqx/emqx/pull/9781) Trace files were left on a node when creating a zip file for download. They are now removed when the file is sent. Also, concurrent downloads will no longer interfere with each other.

- [#9785](https://github.com/emqx/emqx/pull/9785) Stop authentication hook chain if `emqx_authentication` provides a definitive result.

- [#9787](https://github.com/emqx/emqx/pull/9787) Fix a compatible problem for the `webhook` bridge configuration which was created before the v5.0.12.

## 5.0.14

_Release Date: 2023-01-11_

### Enhancements

- https://github.com/emqx/emqx/pull/8329 The MongoDB library has been upgraded to support MongoDB 5.1+

- https://github.com/emqx/emqx/pull/9593 Obfuscated sensitive data in the response when querying bridges information by API.

- https://github.com/emqx/emqx/pull/9614 Make it possible to configure host:port from environment variables without quotes.
  Prior to this change, when overriding a host:port config value from the environment variable, one has to quote it as:
  env EMQX_BRIDGES**MQTT**XYZ**SERVER='"localhost:1883"'.
  Now it's possible to set it without quote as env EMQX_BRIDGES**MQTT**XYZ**SERVER='localhost:1883'.

- https://github.com/emqx/emqx/pull/9642 Deprecates enable_batch and enable_queue options for bridges/resources. After this change, queuing is always enabled for bridges, and batching is controlled by the batch_size option: batch_size > 1 means batching will be enabled.

- https://github.com/emqx/emqx/pull/9671 Implement sliding window average metrics.

- https://github.com/emqx/emqx/pull/9674 Made rule engine behavior more consistent with bridge behavior regarding metrics: if a rule engine is disabled, its metrics are now reset.

- https://github.com/emqx/emqx/pull/9675 HTTP client library ehttpc upgraded from 0.4.2 to 0.4.3.
  Library eredis_cluster which manages clients to Redis clusters upgraded from 0.7.1 to 0.7.5.

- https://github.com/emqx/emqx/pull/9713 Introduce api_key.bootstrap_file to initialize the api key at boot time.
  Deprecate dashboard.bootstrap_users_file.
  Limit the maximum number of api keys to 100 instead of 30.

### Bug fixes

- https://github.com/emqx/emqx/pull/8648 When deleting a non-existing bridge the server gave a successful response. This has been fixed so that the server instead gives an error response when the user attempts to delete a non-existing bridge.

- https://github.com/emqx/emqx/pull/9637 Fix the expiry_interval fields of the clients HTTP API to measure in seconds.

- https://github.com/emqx/emqx/pull/9638 Fix the problem of data loss and bad match when the MySQL driver is disconnected.

- https://github.com/emqx/emqx/pull/9641 Fix an issue where testing the GCP PubSub could leak memory and an issue where its JWT token would fail to refresh a second time.

- https://github.com/emqx/emqx/pull/9642 Fix some issues that could lead to wrong bridge metrics.
  Fix an issue that could lead to message loss and wrong metrics with the Kafka Producer bridge when Kafka or the connection to it is down.
  Fix some issues that could lead to the same message being delivered more than once when using batching for bridges and when the batch was retried.

- https://github.com/emqx/emqx/pull/9667 Remove the possibility to set clientid for /publish and /publish/bulk HTTP APIs. This is to reduce the risk of security confusion.

- https://github.com/emqx/emqx/pull/9687 Fix the problem that sending messages to data-bridges failed because of incorrect handling of some data-bridges without local_topic field configured.
  Before this change, if some bridges have configured the local_topic field but others have not, a function_clause error will occur when forwarding messages to the data-bridges.

- https://github.com/emqx/emqx/pull/9689 Fix handling of HTTP authorization result when a request failure (e.g.: HTTP resource is down) would cause a function_clause error.

- https://github.com/emqx/emqx/pull/9703 Set the default value of the qos field of the HTTP API /clients/:clientid/subscribe to 0.
  Before this fix, the qos field has no default value, which leads to a function_clause error
  when querying this API.

- https://github.com/emqx/emqx/pull/9705 Remove the default value of Webhook.
  Before this repair, the default value of the body field of Webhook is ${payload},
  but there is no payload field in the available fields of other events except the message
  publishing in the rule, so in this case, the webhook will send a string with the
  message body as "undefined" to the HTTP service.
  This fix removes the default value of the body field. When the body field is
  not configured, Webhook will send all available fields of the current event in
  the format of a JSON object.

- https://github.com/emqx/emqx/pull/9712 Fixed the problem of '404 Not Found' when calling the HTTP API '/clients/:clientid/subscribe/bulk'
  from the plug-ins and data-bridges on handling the 'client.connected' event.

- https://github.com/emqx/emqx/pull/9714 Fix /mqtt/auto_subscribe API's bad swagger schema, and make sure swagger always checks if the schema is correct.

- https://github.com/emqx/emqx/pull/9716 MQTT bridge config compatibility fix. The config created before v5.0.12 may encounter a compatibility issue after upgrading to v5.0.13.

- https://github.com/emqx/emqx/pull/9717 Prior to this fix, if it always times out when trying to connect a bridge server, it's not possible to change other configs even when the bridge is disabled.

## 5.0.13

_Release Date: 2022-12-27_

### Enhancements

- Add `limiter` update API [#9133](https://github.com/emqx/emqx/pull/9133).

- Avoid creating temporary zip files when syncing data directory during cluster startup [#9429](https://github.com/emqx/emqx/pull/9429).

- Refactor: move `/mqtt/sys_topics` to generic `/configs/sys_topics` [#9511](https://github.com/emqx/emqx/pull/9511).

- Refactor: use `POST` not `PUT` for `/users/{name}/change_pwd` [#9533](https://github.com/emqx/emqx/pull/9533).

- Add compression functions `zip`, `gzip`, `zip_compress` in Rule-Engine and corresponding decompression functions [#9573](https://github.com/emqx/emqx/pull/9573).

- Return `204` instead of `200` for `PUT /authenticator/:id` [#9434](https://github.com/emqx/emqx/pull/9434/).

- Added the option to customize the clientid prefix of egress MQTT bridges. [#9609](https://github.com/emqx/emqx/pull/9609)

- Ensure the default expiration time of `banned` is large enough [#9599](https://github.com/emqx/emqx/pull/9599/).

### Bug fixes

- Trigger `message.dropped` hook when QoS2 message is resend by client with a same packet id, or 'awaiting_rel' queue is full [#9487](https://github.com/emqx/emqx/pull/9487).

- Fix shared subscription 'sticky' strategy [#9578](https://github.com/emqx/emqx/pull/9578).
  Prior to this change, a 'sticky' subscriber may continue to receive messages after unsubscribing.

- Add check to ensure that a given key is among the prepared statements on query in the mysql connector [#9571](https://github.com/emqx/emqx/pull/9571).

- Fix password leak to logs for connectors [#9608](https://github.com/emqx/emqx/pull/9608).

## 5.0.12

_Release Date: 2022-12-14_

### Highlights

- This version included a refactoring of MQTT bridge config. The older version config file created from v5.0.11 or earlier will be converted to according to the new schema. Please note though, the request body of `/bridges` API to configure MQTT brdige is changed in a incompatible way.
- Start releasing packages for Apple M1/M2 (MacOS-12).
- Start releasing packages for Amazon Linux 2 (e.g. emqx-5.0.12-amzn2-amd64.rpm).
- Retained message index performance improvement.

### Enhancements

- Disable global garbage collection by `node.global_gc_interval = disabled` [#9418](https://github.com/emqx/emqx/pull/9418)。

- Improve the CLI to avoid waste atom table when typing erros [#9416](https://github.com/emqx/emqx/pull/9416).

- Start building MacOS packages for Apple Silicon hadrdware [#9423](https://github.com/emqx/emqx/pull/9423).

- Remove support for setting shared subscriptions using the non-standard `$queue` feature [#9412](https://github.com/emqx/emqx/pull/9412).
  Shared subscriptions are now part of the MQTT spec. Use `$share` instead.

- Refactor authn API by replacing `POST /authentication/{id}/move` with `PUT /authentication/{id}/position/{position}`. [#9419](https://github.com/emqx/emqx/pull/9419).
  Same is done for `/listeners/{listener_id}/authentication/id/...`.

- Redesign `/rules` API to make `metrics` a dedicated resources rather than being included with every response [#9461](https://github.com/emqx/emqx/pull/9461).

- Add more PSK ciphers support [#9505](https://github.com/emqx/emqx/pull/9505).

- Improve `emqx_retainer` write performance: get rid of transactions on write [#9372](https://github.com/emqx/emqx/pull/9372).

- HTTP client library `ehttpc` upgraded from `0.4.0` to `0.4.2` [#9520](https://github.com/emqx/emqx/pull/9520).

- Add `handshake_timeout` option to MQTT SSL listener [#9502](https://github.com/emqx/emqx/pull/9502).

- Upgrade dashboard to [v1.1.3](https://github.com/emqx/emqx-dashboard-web-new/releases/tag/v1.1.3).

- Users can define the `externalTrafficPolicy` of service in EMQX Helm Chart [#9527](https://github.com/emqx/emqx/pull/9527).

- Return `204` instead of `200` for `POST /gateway/lwm2m/clients/{clientid}/{read,write,observe}` [#9480](https://github.com/emqx/emqx/pull/9480).

- Make possible to create an authentication entirely from environment variable [#9547](https://github.com/emqx/emqx/pull/9547).
  As an example, one can now enable MySQL auth with:
  `env EMQX_AUTHENTICATION__1='{mechanism="password_based",backend="mysql",server="localhost:3306",database="emqx",username="emqx",password="******",query="SELECT password_hash,salt FROM mqtt_user WHERE username=${username} LIMIT 1",enable=true}'`.
  Prior to this change, overrides only work on top of existing authentication, for example, if there is already MySQL auth configured in `emqx.conf`
  but we want to disable it, we can do it with `env EMQX_AUTHENTICATION__1__ENABLE=false`.

- Start building packages for Amazon Linux 2 [#9537](https://github.com/emqx/emqx/pull/9537).

### Bug fixes

- Fix that the obsolete SSL files aren't deleted after the ExHook config update [#9432](https://github.com/emqx/emqx/pull/9432).

- Fix doc and schema for `/trace` API [#9468](https://github.com/emqx/emqx/pull/9468).

- Return `404` for `/telemetry/data` in case it's disabled [#9464](https://github.com/emqx/emqx/pull/9464).

- Fix some potential MQTT packet parse errors [#9477](https://github.com/emqx/emqx/pull/9477).

- Fixed EMQX Helm Chart deployment error [#9509](https://github.com/emqx/emqx/pull/9509).

  - Fixed the `Discovery error: no such service` error occurred during helm chart deployment, resulting in an abnormal discovery of cluster nodes.
  - Fixed issue that caused EMQX Helm Chart to fail when modifying some of EMQX's configuration items via environment variables.

- Fix shadowing `'client.authenticate'` callbacks by `emqx_authenticator`. Now `emqx_authenticator`
  passes execution to the further callbacks if none of the authenticators matches [#9496](https://github.com/emqx/emqx/pull/9496).

- Return `400` if query param `node` is not a known node in `/trace/:id/download?node={node}` [#9478](https://github.com/emqx/emqx/pull/9478).

- `POST /traces` to return `409` in case of duplicate [#9494](https://github.com/emqx/emqx/pull/9494).

- Fix bridging function, when both ingress and egress bridges are configured, egress bridge does not work [#9523](https://github.com/emqx/emqx/pull/9523).

- Fix EMQX Helm Chart using incorrect secret values when custom credentials are provided [#9536](https://github.com/emqx/emqx/pull/9536).

## 5.0.11

_Release Date: 2022-11-27_

### Enhancements

- Security enhancement for retained messages [#9326](https://github.com/emqx/emqx/pull/9326).
  The retained messages will not be published if the publisher client is banned.

- Security enhancement for the `subscribe` API [#9355](https://github.com/emqx/emqx/pull/9355).

- Enhance the `banned` feature [#9367](https://github.com/emqx/emqx/pull/9367).
  Now the corresponding session will be kicked when client is banned by `clientid`.

- Redesign `/gateways` API [9364](https://github.com/emqx/emqx/pull/9364).
  Use `PUT /gateways/{name}` instead of `POST /gateways`, gateway gets 'loaded'
  automatically if needed. Use `PUT /gateways/{name}/enable/{true|false}` to
  enable or disable gateway. No more `DELETE /gateways/{name}`.

- Support `statsd {tags: {"user-defined-tag" = "tag-value"}` configure and improve stability of `emqx_statsd` [#9363](http://github.com/emqx/emqx/pull/9363).

- Improve node name generation rules to avoid potential atom table overflow risk [#9387](https://github.com/emqx/emqx/pull/9387).

- Set the default value for the maximum level of a topic to 128 [#9406](https://github.com/emqx/emqx/pull/9406).

- Keep MQTT v5 User-Property pairs from bridge ingested MQTT messsages to bridge target [#9398](https://github.com/emqx/emqx/pull/9398).

### Bug fixes

- Fix `ssl.existingName` option of helm chart not working [#9307](https://github.com/emqx/emqx/issues/9307).

- Fix create trace sometime failed by end_at time has already passed. [#9303](https://github.com/emqx/emqx/pull/9303)

- Return 404 for status of unknown authenticator in `/authenticator/{id}/status` [#9328](https://github.com/emqx/emqx/pull/9328).

- Fix that JWT ACL rules are only applied if an `exp` claim is set [#9368](https://github.com/emqx/emqx/pull/9368).

- Fix that `/configs/global_zone` API cannot get the default value of the configuration [#9392](https://github.com/emqx/emqx/pull/9392).

- Fix mountpoint not working for will-msg [#9399](https://github.com/emqx/emqx/pull/9399).

## 5.0.10

_Release Date: 2022-11-09_

Release had to be recreated due to an issue in GitHub action which failed to upload/publish packages.
Previous commit: 34a6c6c88

### Enhancements

- Improve `/nodes` API responsiveness [#9221](https://github.com/emqx/emqx/pull/9221).

- Improve the integration of the `banned` and the `delayed` feature [#9326](https://github.com/emqx/emqx/pull/9326).
  Now when publishing a delayed message will check first if its source client is banned, if true, this publish will be ignored.

- Update `gen_rpc` library to version 3.0 [#9187](https://github.com/emqx/emqx/pull/9187).

- Improve memory usage on core nodes when bootstrapping a replicant [#9236](https://github.com/emqx/emqx/pull/9236).

- Improve stability of Prometheus Push Gateway and log errors when POST fails [#9235](http://github.com/emqx/emqx/pull/9235).

- Now it is possible to opt out VM internal metrics in prometheus stats [#9222](https://github.com/emqx/emqx/pull/9222).
  When system load is high, reporting too much metrics data may cause the prometheus stats API timeout.

- Improve security when converting types such as `binary` `lists` to `atom` types [#9279](https://github.com/emqx/emqx/pull/9279), [#9286](https://github.com/emqx/emqx/pull/9286).

- Add `/trace/:name/log_detail` HTTP API to return trace file's size and mtime [#9152](https://github.com/emqx/emqx/pull/9152).

- Add `/status` HTTP API endpoint to api documentation [#9230](https://github.com/emqx/emqx/pull/9230).

- Binary packages for all platforms are now built on Erlang/OTP version 24.3.4.2 [#9293](https://github.com/emqx/emqx/pull/9293).

### Bug fixes

- Fix error log message when `mechanism` is missing in authentication config [#8924](https://github.com/emqx/emqx/pull/8924).

- Fix HTTP 500 issue when unknown `status` parameter is used in `/gateway` API call [#9225](https://github.com/emqx/emqx/pull/9225).

- Fixed the HTTP response status code for the `/status` endpoint [#9211](https://github.com/emqx/emqx/pull/9211).
  Before the fix, it always returned `200` even if the EMQX application was not running. Now it returns `503` in that case.

- Fix message delivery related event encoding [#9228](https://github.com/emqx/emqx/pull/9228).
  This bug was introduced in v5.0.9. For Rule-Engine's input events like `$events/message_delivered`
  and `$events/message_dropped`, if the message was delivered to a shared-subscription,
  the encoding (to JSON) of the event will fail.

- Fix bad HTTP response status code for `/gateways` API, when Gateway name is unknown, it should return `404` instead of `400` [#9268](https://github.com/emqx/emqx/pull/9268).

- Fix incorrect topic authorize checking of delayed messages [#9290](https://github.com/emqx/emqx/pull/9290).
  Now will determine the actual topic of the delayed messages, e.g. `$delayed/1/t/foo` will be treated as `t/foo` in authorize checks.

- Add property `code` to error response for `/authentication/sources/:type` [9299](https://github.com/emqx/emqx/pull/9299).

- Align documentation for `/authentication/sources` with what we actually send [9299](https://github.com/emqx/emqx/pull/9299).

- Fix query string parameter 'node' to `/configs` resource being ignored, return 404 if node does not exist [#9310](https://github.com/emqx/emqx/pull/9310/).

- Avoid re-dispatching shared-subscription session messages when a session is kicked or taken-over (to a new session) [#9123](https://github.com/emqx/emqx/pull/9123).

## 5.0.9

_Release Date: 2022-10-24_

### Enhancements

- Add `cert_common_name` and `cert_subject` placeholder support for authz_http and authz_mongo [#8973](https://github.com/emqx/emqx/pull/8973).

- Use milliseconds internally in emqx_delayed to store the publish time, improving precision [#9060](https://github.com/emqx/emqx/pull/9060).

- More rigorous checking of flapping to improve stability of the system [#9136](https://github.com/emqx/emqx/pull/9136).

- No message(s) echo for the message publish APIs [#9155](https://github.com/emqx/emqx/pull/9155).
  Prior to this fix, the message publish APIs (`api/v5/publish` and `api/v5/publish/bulk`) echos the message back to the client in HTTP body.
  This change fixed it to only send back the message ID.

### Bug fixes

- Check ACLs for last will testament topic before publishing the message [#8930](https://github.com/emqx/emqx/pull/8930).

- Fix GET /listeners API crash when some nodes (in a cluster) is still loading the configs [#9002](https://github.com/emqx/emqx/pull/9002).

- Fix empty variable interpolation in authentication and authorization [#8963](https://github.com/emqx/emqx/pull/8963).
  Placeholders for undefined variables are rendered now as empty strings and do not cause errors anymore.

- Fix the latency statistics error of the slow subscription stats [#8986](https://github.com/emqx/emqx/pull/8986).
  Prior to this change when `stats_type` is `internal` or `response`, the begin time stamp was taken at wrong precision.

- Fix shared subscription message re-dispatches [#9104](https://github.com/emqx/emqx/pull/9104).

  - When discarding QoS 2 inflight messages, there were excessive logs
  - For wildcard deliveries, the re-dispatch used the wrong topic (the publishing topic,
    but not the subscribing topic), caused messages to be lost when dispatching.

- Upgrade http client `gun` from 1.3.7 to [1.3.9](https://github.com/emqx/gun/tree/1.3.9)
  Prior to this fix, long-lived HTTPS connections for HTTP auth or webhook integrations
  may stall indefinitely, causing massive timeouts for HTTP requests.

## 5.0.8

_Release Date: 2022-09-17_

### Changes Worth Mentioning

### Enhancements

- An independent RPC implementation is used between nodes to forward shared subscription messages instead of Erlang's own RPC to reduce the cluster pressure when the shared subscription load is high. [#8893](https://github.com/emqx/emqx/pull/8893)
- Print a warning message when boot with the default (insecure) Erlang cookie. [#8905](https://github.com/emqx/emqx/pull/8905)
- The configuration in `local-override.conf` will not allow synchronous updates in the cluster to the entire cluster at runtime. [#8851](https://github.com/emqx/emqx/pull/8851)
- Add `POST /listeners` interface for creating listeners. [#8876](https://github.com/emqx/emqx/pull/8876)
- Change `/gateway` API path to plural form. [#8823](https://github.com/emqx/emqx/pull/8823)
- The `exp`, `nbf` and `iat` claims in JWT authentication support non-integer timestamps. [#8867](https://github.com/emqx/emqx/pull/8867)
- Improve the request performance of ExProto and gRPC Server. [#8866](https://github.com/emqx/emqx/pull/8866)

### Bug Fixes

- Fix the issue that password authentication using Redis as the data source directly terminates the authentication when no authentication data is retrieved. [#8934](https://github.com/emqx/emqx/pull/8934)
- Fix inaccurate delayed publish due to OS time changes. [#8926](https://github.com/emqx/emqx/pull/8926)
- Fix the issue that EMQX could not be started after disabling the retained message feature. [#8911](https://github.com/emqx/emqx/pull/8911)
- Fix the slow response of updating the configuration when a node in the cluster is down. [#8857](https://github.com/emqx/emqx/pull/8857)
- Fix the issue that the authorization would terminate the execution of the `client.authorize` hook when no rules were matched. [#8780](https://github.com/emqx/emqx/pull/8780)
- Fix the issue that Payload must be configured in MQTT Bridge. [#8949](https://github.com/emqx/emqx/pull/8949)
- Fix the issue that the log directory cannot be configured through environment variables. [#8892](https://github.com/emqx/emqx/pull/8892)
- Fix the issue that the CoAP gateway introduced an extra `/` prefix when parsing topics. [#8658](https://github.com/emqx/emqx/pull/8658)
- Fix the issue that MQTT Bridge returned the content of the TLS file in the API response. [#8872](https://github.com/emqx/emqx/pull/8872), [#8958](https://github.com/emqx/emqx/pull/8958)
- Fix the issue that client authentication failure would trigger the release of will messages. [#8887](https://github.com/emqx/emqx/pull/8887)
- Fix ExProto's imperfect Keep Alive check mechanism that could cause the client to never expire. [#8866](https://github.com/emqx/emqx/pull/8866)

### Dependency Upgrades

- `grpc-erl` upgrades from `0.6.6` to `0.6.7`, [#8866](https://github.com/emqx/emqx/pull/8866)

## 5.0.7

_Release Date: 2022-09-01_

### Enhancements

- Simplify TLS cipher suite configuration
- Add confirmation before listener closes on Dashboard
- Unify the configuration of TLS on Dashboard
- Supports viewing EMQX version and node role on Dashboard overview page
- Increase restrictions on plugin file types

### Bug fixes

- Fix the issue that Mria transactions of replicated nodes could not be executed when nodes of mixed versions formed a cluster
- Fix the issue that the Authorization settings page of Dashboard could not display data
- Fix the issue that the monitoring topic data could not be reset on Dashboard

### Other changes

- Remove will message related fields in the response of the client query interface

## 5.0.6

_Release Date: 2022-08-22_

### Bug fixes

- Fix incorrect display of node status on Dashboard

## 5.0.4

_Release Date: 2023-05-26_

### Enhancements

- [#10389](https://github.com/emqx/emqx/pull/10389) Unified the configuration formats for `cluster.core_nodes` and `cluster.statics.seeds`. Now they both support formats in array `["emqx1@127.0.0.1", "emqx2@127.0.0.1"]` and the comma-separated string `"emqx1@127.0.0.1,emqx2@127.0.0.1"`.

- [#10392](https://github.com/emqx/emqx/pull/10392) Introduced a new function to convert a formatted date to an integer timestamp: date_to_unix_ts/3.

  `date_to_unix_ts(TimeUnit, FormatString, InputDateTimeString)`

- [#10426](https://github.com/emqx/emqx/pull/10426) Optimized the configuration priority mechanism to fix the issue where the configuration changes made to `etc/emqx.conf` do not take effect after restarting EMQX.

  More information about the new mechanism: [Configure Override Rules](https://docs.emqx.com/en/enterprise/v5.0/configuration/configuration.html#configure-override-rules)

- [#10457](https://github.com/emqx/emqx/pull/10457) Deprecated the integration with StatsD.

- [#10458](https://github.com/emqx/emqx/pull/10458) Set the level of plugin configuration options to low, users usually manage the plugins through the dashboard, rarely modify them manually, so we lowered the level.

- [#10491](https://github.com/emqx/emqx/pull/10491) Renamed `etcd.ssl` to `etcd.ssl_options` to keep all SSL options consistent in the configuration file.

- [#10512](https://github.com/emqx/emqx/pull/10512) Improved the storage format of Unicode characters in data files, Now we can store Unicode characters. For example: `SELECT * FROM "t/1" WHERE clientid = "-测试专用-"`.

- [#10568](https://github.com/emqx/emqx/pull/10568) Added `shutdown_count` printout to `emqx ctl listeners` command.

- [#10588](https://github.com/emqx/emqx/pull/10588) Increased the time precision of trace logs from second to microsecond. For example, change from `2023-05-02T08:43:50+00:00` to `2023-05-02T08:43:50.237945+00:00`.

- [#10623](https://github.com/emqx/emqx/pull/10623) Renamed `max_message_queue_len` to `max_mailbox_size` in the `force_shutdown` configuration. The old name is kept as an alias, so this change is backward compatible.

- [#10713](https://github.com/emqx/emqx/pull/10713) Hide the `resource_option.request_timeout` of the webhook and it will use the value of `http` `request_timeout`.

- [#10075](https://github.com/emqx/emqx/pull/10075) Added node rebalance/node evacuation functionality. See also: [EIP doc](https://github.com/emqx/eip/blob/main/active/0020-node-rebalance.md)

- [#10378](https://github.com/emqx/emqx/pull/10378) Implemented Pulsar Producer Bridge and only producer role is supported now.

- [#10408](https://github.com/emqx/emqx/pull/10408) Introduced 3 built-in functions in the rule engine SQL-like language for creating values of the MongoDB date type.

- [#10409](https://github.com/emqx/emqx/pull/10409) [#10337](#10337) Supported [Protocol Buffers](https://protobuf.dev/) and [Apache Avro](https://avro.apache.org/) schemas in Schema Registry.

- [#10425](https://github.com/emqx/emqx/pull/10425) Implemented OpenTSDB data bridge.

- [#10498](https://github.com/emqx/emqx/pull/10498) Implemented Oracle Database Bridge.

- [#10560](https://github.com/emqx/emqx/pull/10560) Added enterprise data bridge for Apache IoTDB.

- [#10417](https://github.com/emqx/emqx/pull/10417) Improved get config items performance by eliminating temporary references.

- [#10430](https://github.com/emqx/emqx/pull/10430) Simplified the configuration of the `retainer` feature. Marked `flow_control` as a non-importance field.

- [#10511](https://github.com/emqx/emqx/pull/10511) Improved the security and privacy of some resource logs by masking sensitive information in the log.

- [#10525](https://github.com/emqx/emqx/pull/10525) Reduced resource usage per MQTT packet handling.

- [#10528](https://github.com/emqx/emqx/pull/10528) Reduced memory footprint in hot code path. The hot path includes the code that is frequently executed in core functionalities such as message handling, connection management, authentication, and authorization.

- [#10591](https://github.com/emqx/emqx/pull/10591) [#10625](https://github.com/emqx/emqx/pull/10625) Improved the configuration of the limiter.

  - Reduced the complexity of the limiter's configuration.

  - Updated the `configs/limiter` API to suit this refactor.

  - Reduced the memory usage of the limiter configuration.

- [#10487](https://github.com/emqx/emqx/pull/10487) Optimized the instance of limiter for whose rate is `infinity` to reduce memory and CPU usage.

- [#10490](https://github.com/emqx/emqx/pull/10490) Removed the default limit of connect rate which used to be `1000/s`.

- [#10077](https://github.com/emqx/emqx/pull/10077) Added support for QUIC TLS password-protected certificate file.

### Bug Fixes

- [#10340](https://github.com/emqx/emqx/pull/10340) Fixed the issue that could lead to crash logs being printed when stopping EMQX via `systemd`.

- [#10369](https://github.com/emqx/emqx/pull/10369) Fixed error in `/api/v5/monitor_current` API endpoint that happens when some EMQX nodes are down.

  Prior to this fix, sometimes the request returned HTTP code 500 and the following message:

  `{"code":"INTERNAL_ERROR","message":"error, badarg, [{erlang,'++',[{error,nodedown},[{node,'emqx@10.42.0.150'}]], ...`

- [#10407](https://github.com/emqx/emqx/pull/10407) Fixed the crash issue of the alarm system.

  - Leverage Mnesia dirty operations and circumvent extraneous calls to enhance 'emqx_alarm' performance.

  - Use 'emqx_resource_manager' for reactivating alarms that have already been triggered.

  - Implement the newly developed, fail-safe 'emqx_alarm' API to control the activation and deactivation of alarms, thus preventing 'emqx_resource_manager' from crashing due to alarm timeouts.

  - The alarm system is susceptible to crashing under these concurrent conditions:

    - A significant number of resources fail, such as when bridges continuously attempt to trigger alarms due to recurring errors.

    - The system is under an extremely high load.

- [#10420](https://github.com/emqx/emqx/pull/10420) Fixed HTTP path handling when composing the URL for the HTTP requests in authentication and authorization modules.

  - Avoid unnecessary URL normalization since we cannot assume that external servers treat original and normalized URLs equally. This led to bugs like [#10411](https://github.com/emqx/emqx/issues/10411).

  - Fixed the issue that path segments could be HTTP encoded twice.

- [#10422](https://github.com/emqx/emqx/pull/10422) Fixed a bug where external plugins could not be configured via environment variables in a lone-node cluster.

- [#10448](https://github.com/emqx/emqx/pull/10448) Fixed a compatibility issue of limiter configuration introduced by e5.0.3 which broke the upgrade from previous versions if the `capacity` is `infinity`.

  In e5.0.3 we have replaced `capacity` with `burst`. After this fix, a `capacity = infinity` config will be automatically converted to equivalent `burst = 0`.

- [#10462](https://github.com/emqx/emqx/pull/10462) Deprecated config `broker.shared_dispatch_ack_enabled`. This was designed to avoid dispatching messages to a shared-subscription session that has the client disconnected. However, since e5.0.0, this feature is no longer helpful because the shared-subscription messages in an expired session will be redispatched to other sessions in the group. See also: <https://github.com/emqx/emqx/pull/9104> .

- [#10463](https://github.com/emqx/emqx/pull/10463) Improved bridges API error handling. If Webhook bridge URL is not valid, the bridges API will return '400' error instead of '500'.

- [#10484](https://github.com/emqx/emqx/pull/10484) Fixed the issue that the priority of the configuration cannot be set during the rolling upgrade. For example, when authorization is modified in e5.0.2 and then upgraded e5.0.3 through the rolling upgrade, the authorization will be restored to the default.

- [#10495](https://github.com/emqx/emqx/pull/10495) Added the limiter API `/configs/limiter` which was deleted by mistake back.

- [#10500](https://github.com/emqx/emqx/pull/10500) Added several fixes, enhancements, and features in Mria:

  - Protect `mria:join/1,2` with a global lock to prevent conflicts between two nodes trying to join each other simultaneously [Mria PR](https://github.com/emqx/mria/pull/137)

  - Implement new function `mria:sync_transaction/4,3,2`, which blocks the caller until a transaction is imported to the local node (if the local node is a replicant, otherwise, it behaves exactly the same as `mria:transaction/3,2`) [Mria PR](https://github.com/emqx/mria/pull/136)

  - Optimize `mria:running_nodes/0` [Mria PR](https://github.com/emqx/mria/pull/135)

  - Optimize `mria:ro_transaction/2` when called on a replicant node [Mria PR](https://github.com/emqx/mria/pull/134).

- [#10518](https://github.com/emqx/emqx/pull/10518) Added the following fixes and features in Mria:

  - Call `mria_rlog:role/1` safely in mria_membership to ensure that mria_membership gen_server won't crash if RPC to another node fails [Mria PR](https://github.com/emqx/mria/pull/139)

  - Add an extra field to `?rlog_sync` table to facilitate extending this functionality in future [Mria PR](https://github.com/emqx/mria/pull/138).

- [#10556](https://github.com/emqx/emqx/pull/10556) Wrapped potentially sensitive data in `emqx_connector_http` if `Authorization` headers are being passed at initialization.

- [#10571](https://github.com/emqx/emqx/pull/10571) Stopped emitting useless crash report when EMQX stops.

- [#10659](https://github.com/emqx/emqx/pull/10659) Fixed the issue where EMQX cannot start when `sysmon.os.mem_check_interval` is disabled.

- [#10717](https://github.com/emqx/emqx/pull/10717) Fixed an issue where the buffering layer processes could use a lot of CPU when inflight window is full.

- [#10724](https://github.com/emqx/emqx/pull/10724) A summary has been added for all endpoints in the HTTP API documentation (accessible at "http://<emqx_host_name\>:18083/api-docs").

- [#10726](https://github.com/emqx/emqx/pull/10726) Health Check Interval and Auto Restart Interval now support the range from 1ms to 1 hour.

- [#10728](https://github.com/emqx/emqx/pull/10728) Fixed an issue where the rule engine was unable to access variables exported by `FOREACH` - `DO` clause.

  Given a payload: `{"date": "2023-05-06", "array": ["a"]}`, as well as the following SQL statement:

  `FOREACH   payload.date as date, payload.array as elem DO   date, elem FROM "t/#"  -- {"date": "2023-05-06", "array": ["a"]}`

  Prior to the fix, the `date` variable exported by `FOREACH` could not be accessed in the `DO` clause of the above SQL, resulting in the following output for the SQL statement: `[{"elem": "a","date": "undefined"}]`.

- [#10742](https://github.com/emqx/emqx/pull/10742) Correctness check of the rules is enforced before saving the authorization file source. Previously, Saving wrong rules could lead to EMQX restart failure.

- [#10743](https://github.com/emqx/emqx/pull/10743) Fixed an issue where trying to get bridge info or metrics could result in a crash when a node is joining a cluster.

- [#10755](https://github.com/emqx/emqx/pull/10755) Fixed data bridge resource update race condition.

  In the 'delete + create' process for EMQX resource updates, long bridge creation times could cause dashboard request timeouts. If a bridge resource update was initiated before completion of its creation, it led to an erroneous deletion from the runtime, despite being present in the config file.

  This fix addresses the race condition in bridge resource updates, ensuring the accurate identification and addition of new resources, and maintaining consistency between runtime and configuration file statuses.

- [#10761](https://github.com/emqx/emqx/pull/10761) Fixed the issue where the default value of SSL certificate for Dashboard Listener was not correctly interpolated, which caused HTTPS to be inaccessible when `verify_peer` and `cacertfile` were using the default configuration.

- [#10672](https://github.com/emqx/emqx/pull/10672) Fixed the issue where the lack of a default value for `ssl_options` in listeners results in startup failure. For example, such command(`EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8089' ./bin/emqx console`) would have caused a crash before.

- [#10738](https://github.com/emqx/emqx/pull/10738) TDEngine data bridge now supports "Supertable" and "Create Tables Automatically". Before this fix, an insert with a supertable in the template will fail, like this:

  - `insert into ${clientid} using msg TAGS (${clientid}) values (${ts},${msg})`.

- [#10746](https://github.com/emqx/emqx/pull/10746) Add missing support of the event `$events/delivery_dropped` into the rule engine test API `rule_test`.

- [#10747](https://github.com/emqx/emqx/pull/10747) Ported some time formating fixes in Rule-Engine functions from version 4.4.

- [#10760](https://github.com/emqx/emqx/pull/10760) Fix "internal error 500" when getting bridge statistics page while a node is joining the cluster.

- [#10801](https://github.com/emqx/emqx/pull/10801) Avoid double percent-decode for topic name in API `/topics/{topic}` and `/topics`.

- [#10817](https://github.com/emqx/emqx/pull/10817) Fix a config value handling for bridge resource option `auto_restart_interval`, now it can be set to `infinity`.

## 5.0.4

_Release Date: 2022-07-28_

### Enhancements

- Rules in data Integration support paging and searching. Note that the `GET /rules` API will return the page meta information after the update, i.e. `{"data": [RuleObj1, RuleObj2], "meta": {"count": 2, "limit": 100, "page": 1}}`. [#8472](https://github.com/emqx/emqx/pull/8472)
- Improve the health check of WebHook in data integration, if TLS is enabled, it will now check if the TLS handshake was successful. [#8443](https://github.com/emqx/emqx/pull/8443)
- Falls back to using Mnesia to persist sessions when RocksDB is unavailable. [#8528](https://github.com/emqx/emqx/pull/8528)
- Support for updating thresholds of alarms at runtime via the HTTP API. [#8532](https://github.com/emqx/emqx/pull/8532)
- Log trace will show the detailed authentication process. [#8554](https://github.com/emqx/emqx/pull/8554)
- Supports listening on IPv6 addresses, for example: `[::1]:1883` or `::1:1883`. [#8547](https://github.com/emqx/emqx/pull/8547)
- Updated the Listener API's request and response formats to align with the behavior of other APIs. This will introduce some incompatible updates, see [#8571](https://github.com/emqx/emqx/pull/8571)。
- Dashboard will prompt to change the default password.
- Optimize Dashboard's overview page for rules and data bridges of data integration.
- Add result statistics for data integration rules on Dashboard.
- Optimize the charts in the Dashboard homepage.
- Add MQTT 5.0 subscription options display to subscription list on Dashboard.

### Bug fixes

- Fix the issue that when the log type format is set to `json`, the configuration of the maximum length of a single log is invalid. [#8518](http://github.com/emqx/emqx/pull/8518)
- Fix the issue that `jq` in data integration cannot be used when the path of the EMQX installation directory contains spaces. [#8455](https://github.com/emqx/emqx/pull/8455)
- Fix the issue that super user does not take effect. [#8452](https://github.com/emqx/emqx/pull/8452)
- Aligned system topic format for stats with metrics. [#8464](https://github.com/emqx/emqx/pull/8464)
- Fix the issue that the creation time of the rules in the data integration was not persistent, causing it to be updated to the EMQX startup time every time. [#8443](https://github.com/emqx/emqx/pull/8443)
- Fix an issue where the `cluster-override.conf` file would be emptied when there was an error updating the configuration via the HTTP API, causing all modified configurations to be lost. [#8443](https://github.com/emqx/emqx/pull/8443)
- Fix the issue that the Sentinel field was not required to be set when using Redis sentinel mode in authentication and authorization. [#8458](https://github.com/emqx/emqx/pull/8458)
- Fix formatting errors in OpenAPI documentation. [#8517](https://github.com/emqx/emqx/pull/8517)
- Fix the issue that multilingual hook extensions might not be dispatched in the order in which client events were fired. [#8530](https://github.com/emqx/emqx/pull/8530)
- Fix authentication placeholders `cert_subject` and `cert_common_name` not being available. [#8531](https://github.com/emqx/emqx/pull/8531)
- Fix TCP connection process leak in WebHook. [ehttpc#34](https://github.com/emqx/ehttpc/pull/34), [#8580](https://github.com/emqx/emqx/pull/8580)
- Fix CLI not printing listeners that only listen on ports. [#8547](https://github.com/emqx/emqx/pull/8547)
- Fix incorrect TLS field checking in JWKS authentication. [#8458](https://github.com/emqx/emqx/pull/8458)
- Fix listener API not returning connection information on all nodes in the cluster. [#8538](https://github.com/emqx/emqx/pull/8538)
- Fix the issue that replicant nodes might not receive Mnesia events, causing operations such as configuration updates to fail. [#8502](https://github.com/emqx/emqx/pull/8502)

## 5.0.3

_Release Date: 2023-05-08_

### Enhancements

- [#10128](https://github.com/emqx/emqx/pull/10128) Add support for OCSP stapling for SSL MQTT listeners.

- [#10156](https://github.com/emqx/emqx/pull/10156) Change the configuration overlay order:

  If it is a new installation of EMQX, `emqx.conf` + Environment variables overlays on top of API Updated Configs (`cluster.hocon`)

  If EMQX is upgraded from an older version (i.e., the `cluster-override.conf` file still exists in EMQX's `data` directory), then it’s the same as before, that is `cluster-override.conf` overlays on top of `emqx.conf` + Environment variables.

  Please note that `data/configs/cluster-override.conf` is considered deprecated. After upgrade, you are encouraged to update `emqx.conf` to delete configs which are overridden by `cluster-override.conf` and move the configs in `cluster-override.conf` to `cluster.hocon`.
  After upgrade, EMQX will continue to read `local-override.conf` (if it exists) as before, but you are encouraged to merge the configs to `emqx.conf`.

- [#10164](https://github.com/emqx/emqx/pull/10164) Add CRL check support for TLS MQTT listeners.

- [#10207](https://github.com/emqx/emqx/pull/10207) Improve OpenAPI (swagger) document readability. Prior to this change, there were a few `summary` docs which are lengthy and lack of translation, now it makes use of the more concise `label` field from schema i18n database instead.

- [#10210](https://github.com/emqx/emqx/pull/10210) Eliminated a few harmless error level logs.
  Prior to this change, there might be some Mnesia callback (hook) failures occasionally occurring when stopping/restarting Mria.
  Now the callbacks (hooks) are unregistered prior to stop. See also [Mria PR](https://github.com/emqx/mria/pull/133).

- [#10224](https://github.com/emqx/emqx/pull/10224) Add the option to customize `clusterIP` in Helm chart, so that a user may set it to a fixed IP.

- [#10263](https://github.com/emqx/emqx/pull/10263) Add command `eval-ex` for Elixir expression evaluation.

- [#10278](https://github.com/emqx/emqx/pull/10278) Refactor the directory structure of all gateways.

- [#10206](https://github.com/emqx/emqx/pull/10206) Support async query mode for all data bridges.

  Prior to this change, setting the query mode of a resource such as a bridge to sync would force the buffer to call the underlying connector in a synchronous way, even if it supports async calls.

- [#10306](https://github.com/emqx/emqx/pull/10306) Add support for async query mode for most bridges.

  This is a follow-up change after [#10206](https://github.com/emqx/emqx/pull/10206). Before this change, some bridges (Cassandra, MongoDB, MySQL, Postgres, Redis, RocketMQ, TDengine) were only allowed to be created with a sync query mode. Now async mode is also supported.

- [#10318](https://github.com/emqx/emqx/pull/10318) Prior to this enhancement, only double quotes (") were allowed in rule engine SQL language's FROM clause. Now it also supports single quotes (').

- [#10336](https://github.com/emqx/emqx/pull/10336) Add `/rule_engine` API endpoint to manage configuration of rule engine.

- [#10354](https://github.com/emqx/emqx/pull/10354) More specific error messages when configure with `bad max_heap_size` value. Log current value and the max value when the `message_queue_too_long` error is thrown.

- [#10358](https://github.com/emqx/emqx/pull/10358) Hide `flapping_detect/conn_congestion/stats` configuration. Deprecate `flapping_detect.enable`.

- [#10359](https://github.com/emqx/emqx/pull/10359) Metrics now are not implicitly collected in places where API handlers don't make any use of them. Instead, a separate backplane RPC gathers cluster-wide metrics.

- [#10373](https://github.com/emqx/emqx/pull/10373) Deprecate the `trace.payload_encode` configuration. Add `payload_encode=[text,hidden,hex]` option when creating a trace via HTTP API.

- [#10381](https://github.com/emqx/emqx/pull/10381) Hide the `auto_subscribe` configuration items so that they can be modified later only through the HTTP API.

- [#10391](https://github.com/emqx/emqx/pull/10391) Hide a large number of advanced options to simplify the configuration file.

  That includes `rewrite`, `topic_metric`, `persistent_session_store`, `overload_protection`,
  `flapping_detect`, `conn_congestion`, `stats,auto_subscribe`, `broker_perf`,
  `shared_subscription_group`, `slow_subs`, `ssl_options.user_lookup_fun` and some advance items
  in `node` and `dashboard` section, [#10358](https://github.com/emqx/emqx/pull/10358),
  [#10381](https://github.com/emqx/emqx/pull/10381), [#10385](https://github.com/emqx/emqx/pull/10385).

- [#10404](https://github.com/emqx/emqx/pull/10404) Change the default queue mode for buffer workers to `memory_only`. Before this change, the default queue mode was `volatile_offload`. When under high message rate pressure and when the resource is not keeping up with such rate, the buffer performance degraded a lot due to the constant disk operations.

- [#10140](https://github.com/emqx/emqx/pull/10140) Integrate Cassandra into bridges as a new backend. At the current stage only support Cassandra version 3.x, not yet 4.x.

- [#10143](https://github.com/emqx/emqx/pull/10143) Add RocketMQ data integration bridge.

- [#10165](https://github.com/emqx/emqx/pull/10165) Support escaped special characters in InfluxDB data bridge `write_syntax`. This update allows to use escaped special characters in string elements in accordance with InfluxDB line protocol.

- [#10211](https://github.com/emqx/emqx/pull/10211) Hide `broker.broker_perf` config and API documents. The two configs `route_lock_type` and `trie_compaction` are rarely used and requires a full cluster restart to take effect. They are not suitable for being exposed to users. Detailed changes can be found here: <https://gist.github.com/zmstone/01ad5754b9beaeaf3f5b86d14d49a0b7/revisions>.

- [#10294](https://github.com/emqx/emqx/pull/10294) When configuring a MongoDB bridge, you can now use the `${field}` syntax to reference fields in the message. This enables you to select the collection to insert data into dynamically.

- [#10363](https://github.com/emqx/emqx/pull/10363) Implement Microsoft SQL Server bridge.

- [#10573](https://github.com/emqx/emqx/pull/10573) Improved performance of Webhook bridge when using synchronous query mode. This also should improve the performance of other bridges when they are configured with no batching.

### Bug Fixes

- [#10145](https://github.com/emqx/emqx/pull/10145) Add field `status_reason` to `GET /bridges/:id` response in case this bridge is in status `disconnected` if internal health-check reports an error condition. Include this same error condition in message when creating an alarm for a failing bridge.

- [#10172](https://github.com/emqx/emqx/pull/10172) Fix the incorrect regular expression in default ACL rule to allow specify username(dashboard) to subscribe `$SYS/#`.

- [#10174](https://github.com/emqx/emqx/pull/10174) Upgrade library `esockd` from 5.9.4 to 5.9.6. Fix an unnecessary error level logging when a connection is closed before proxy protocol header is sent by the proxy.

- [#10195](https://github.com/emqx/emqx/pull/10195) Add labels to API schemas where description contains raw HTML, which would break formatting of generated documentation otherwise.

- [#10196](https://github.com/emqx/emqx/pull/10196) Use lower-case for schema summaries and descriptions to be used in menu of generated online documentation.

- [#10209](https://github.com/emqx/emqx/pull/10209) Fix bug where a last will testament (LWT) message could be published when kicking out a banned client.

- [#10225](https://github.com/emqx/emqx/pull/10225) Allow installing a plugin if its name matches the beginning of another (already installed) plugin name. For example: if plugin `emqx_plugin_template_a` is installed, it must not block installing plugin `emqx_plugin_template`.

- [#10226](https://github.com/emqx/emqx/pull/10226) Handle validation error in `/bridges` API and return `400` instead of `500`.

- [#10242](https://github.com/emqx/emqx/pull/10242) Fixed a log data field name clash. Prior to this fix, some debug logs may report a wrong Erlang PID which may affect troubleshooting session takeover issues.

- [#10257](https://github.com/emqx/emqx/pull/10257) Fixed the issue where `auto_observe` was not working in LwM2M Gateway.

  Before the fix, `OBSERVE` requests were sent without a token, causing failures that LwM2M clients could not handle.

  After the fix, LwM2M Gateway can correctly observe the resource list carried by client, furthermore, unknown resources will be ignored and printing the following warning log:

  ```
  2023-03-28T18:50:27.771123+08:00 [warning] msg: ignore_observer_resource, mfa: emqx_lwm2m_session:observe_object_list/3, line: 522, peername: 127.0.0.1:56830, clientid: testlwm2mclient, object_id: 31024, reason: no_xml_definition
  ```

- [#10286](https://github.com/emqx/emqx/pull/10286) Enhance logging behaviour during boot failure. When EMQX fails to start due to corrupted configuration files, excessive logging is eliminated and no crash dump file is generated.

- [#10297](https://github.com/emqx/emqx/pull/10297) Keeps `eval` command backward compatible with v4 by evaluating only Erlang expressions, even on Elixir node. For Elixir expressions, use `eval-ex` command.

- [#10300](https://github.com/emqx/emqx/pull/10300) Fixed issue with Elixir builds that prevented plugins from being configured via environment variables.

- [#10315](https://github.com/emqx/emqx/pull/10315) Fix crash checking `limit` and `page` parameters in `/mqtt/delayed/messages` API call.

- [#10317](https://github.com/emqx/emqx/pull/10317) Do not expose listener level authentications before extensive verification.

- [#10323](https://github.com/emqx/emqx/pull/10323) For security reasons, the value of the password field in the API examples is replaced with `******`.

- [#10410](https://github.com/emqx/emqx/pull/10410) Fix config check failed when gateways are configured in emqx.conf.
  This issue was first introduced in v5.0.22 via [#10278](https://github.com/emqx/emqx/pull/10278), the boot-time config check was missing.

- [#10533](https://github.com/emqx/emqx/pull/10533) Fixed an issue that could cause (otherwise harmless) noise in the logs.

  During some particularly slow synchronous calls to bridges, some late replies could be sent to connections processes that were no longer expecting a reply, and then emit an error log like:

  ```
  2023-04-19T18:24:35.350233+00:00 [error] msg: unexpected_info, mfa: emqx_channel:handle_info/2, line: 1278, peername: 172.22.0.1:36384, clientid: caribdis_bench_sub_1137967633_4788, info: {#Ref<0.408802983.1941504010.189402>,{ok,200,[{<<"cache-control">>,<<"max-age=0, ...">>}}
  ```

  Those logs are harmless, but they could flood and worry the users without need.

- [#10449](https://github.com/emqx/emqx/pull/10449) Validate the `ssl_options` and `header` configurations when creating authentication http (`authn_http`). Prior to this, incorrect `ssl` configuration could result in successful creation but the entire authn being unusable.

- [#10548](https://github.com/emqx/emqx/pull/10548) Fixed a race condition in the HTTP driver that would result in an error rather than a retry of the request.
  Related fix in the driver: [emqx/ehttpc#45](https://github.com/emqx/ehttpc/pull/45)

- [#10201](https://github.com/emqx/emqx/pull/10201) In TDengine data bridge, removed the redundant database name from the SQL template.

- [#10270](https://github.com/emqx/emqx/pull/10270) ClickHouse data bridge has got a fix that makes the error message better when users click the test button in the settings dialog.

- [#10324](https://github.com/emqx/emqx/pull/10324) Previously, when attempting to reconnect to a misconfigured ClickHouse bridge through the dashboard, users would not receive an error message. This issue is now resolved, and error messages will now be displayed.

- [#10438](https://github.com/emqx/emqx/pull/10438) Fix some configuration item terminology errors in the DynamoDB data bridge:

  - Changed `database` to `table`
  - Changed `username` to `aws_access_key_id`
  - Changed `password` to `aws_secret_access_key`

## 5.0.3

_Release Date: 2022-07-07_

### Bug fixes

- Websocket listener failed to read headers `X-Forwared-For` and `X-Forwarded-Port` [8415](https://github.com/emqx/emqx/pull/8415)
- Deleted `cluster_singleton` from MQTT bridge config document. This config is no longer applicable in 5.0 [8407](https://github.com/emqx/emqx/pull/8407)
- Fix `emqx/emqx:latest` docker image publish to use the Erlang flavor, but not Elixir flavor [8414](https://github.com/emqx/emqx/pull/8414)
- Changed the `exp` field in JWT auth to be optional rather than required to fix backwards compatability with 4.X releases. [8425](https://github.com/emqx/emqx/pull/8425)

### Enhancements

- Improve the speed of dashboard's HTTP API routing rule generation, which sometimes causes timeout [8438](https://github.com/emqx/emqx/pull/8438)

## 5.0.2

_Release Date: 2023-04-12_

### Enhancements

- [#10022](https://github.com/emqx/emqx/pull/10022) Release installation packages for Rocky Linux 9 (compatible with Red Hat Enterprise Linux 9) and macOS 12 for Intel platform.

- [#10139](https://github.com/emqx/emqx/pull/10139) Add `extraVolumeMounts` to EMQX Helm Chart, you can mount user's own files to EMQX instance, such as ACL rule files mentioned in [#9052](https://github.com/emqx/emqx/issues/9052).

- [#9893](https://github.com/emqx/emqx/pull/9893) When connecting with the flag `clean_start=false`, EMQX will filter out messages that published by clients banned by the blacklist feature in the session.
  Previously, messages sent by clients banned by the blacklist feature could still be delivered to subscribers in this case.

- [#9986](https://github.com/emqx/emqx/pull/9986) Add MQTT ingress to helm charts and remove obsolete mgmt references.

- [#9564](https://github.com/emqx/emqx/pull/9564) Implement Kafka Consumer Bridge, which supports consuming messages from Kafka and publishing them to MQTT topics.

- [#9881](https://github.com/emqx/emqx/pull/9881) Improve error logging related to health checks for InfluxDB connections.

- [#9985](https://github.com/emqx/emqx/pull/9985) Implement ClickHouse Data Bridge

- [#10123](https://github.com/emqx/emqx/pull/10123) Improve the performance of `/bridges` API.
  Earlier, when the number of nodes in the cluster was large or the node was busy, the API may had a request timeout.

- [#9998](https://github.com/emqx/emqx/pull/9998) Obfuscate request body in error log when using HTTP service for client authentication for security reasons.

- [#10026](https://github.com/emqx/emqx/pull/10026) Metrics are now only exposed via the `/bridges/:id/metrics` endpoint, and no longer returned in other API operations.

- [#10052](https://github.com/emqx/emqx/pull/10052) Improve startup failure logs in daemon mode.

### Bug Fixes

- [#10013](https://github.com/emqx/emqx/pull/10013) Fix return type structure for error case in API schema for `/gateways/:name/clients`.

- [#10014](https://github.com/emqx/emqx/pull/10014) Ensure Monitor API `/monitor(_current)/nodes/:node` returns `404` instead of `400` if node does not exist.

- [#10027](https://github.com/emqx/emqx/pull/10027) Allow setting node name via environment variable `EMQX_NODE__NAME` in Docker.

- [#10050](https://github.com/emqx/emqx/pull/10050) Ensure Bridge API returns `404` status code consistently for resources that don't exist.

- [#10055](https://github.com/emqx/emqx/pull/10055) The configuration parameter `mqtt.max_awaiting_rel` was not functional and has now been corrected.

- [#10056](https://github.com/emqx/emqx/pull/10056) Fix `/bridges` API status code.
  Return `400` instead of `403` in case of removing a data bridge that is dependent on an active rule.
  Return `400` instead of `403` in case of calling operations (start|stop|restart) when Data-Bridging is not enabled.

- [#10066](https://github.com/emqx/emqx/pull/10066) Improve error messages for `/briges_probe` and `[/node/:node]/bridges/:id/:operation` API calls to make them more readable. And set HTTP status code to `400` instead of `500`.

- [#10074](https://github.com/emqx/emqx/pull/10074) Check if type in `PUT /authorization/sources/:type` matches `type` given in the request body.

- [#10079](https://github.com/emqx/emqx/pull/10079) Fix wrong description about `shared_subscription_strategy`.

- [#10085](https://github.com/emqx/emqx/pull/10085) Consistently return `404` for all requests on non-existent source in `/authorization/sources/:source[/*]`.

- [#10098](https://github.com/emqx/emqx/pull/10098) Fix an issue where the MongoDB connector crashed when MongoDB authorization was configured.

- [#10100](https://github.com/emqx/emqx/pull/10100) Fix channel crash for slow clients with enhanced authentication.
  Previously, when the client was using enhanced authentication, but the Auth message was sent slowly or the Auth message was lost, the client process would crash.

- [#10107](https://github.com/emqx/emqx/pull/10107) For operations on Bridges API if `bridge-id` is unknown we now return `404` instead of `400`.

- [#10117](https://github.com/emqx/emqx/pull/10117) Fix an error occurring when a joining node doesn't have plugins that are installed on other nodes in the cluster.
  After this fix, the joining node will copy all the necessary plugins from other nodes.

- [#10118](https://github.com/emqx/emqx/pull/10118) Fix problems related to manual joining of EMQX replicant nodes to the cluster.

- [#10119](https://github.com/emqx/emqx/pull/10119) Fix crash when `statsd.server` is set to an empty string.

- [#10124](https://github.com/emqx/emqx/pull/10124) The default heartbeat period for MongoDB has been increased to reduce the risk of too excessive logging to the MongoDB log file.

- [#10130](https://github.com/emqx/emqx/pull/10130) Fix garbled config display in dashboard when the value is originally from environment variables.

- [#10132](https://github.com/emqx/emqx/pull/10132) Fix some error logs generated by `systemctl stop emqx` command.
  Prior to the fix, the command was not stopping `jq` and `os_mon` applications properly.

- [#10144](https://github.com/emqx/emqx/pull/10144) Fix an issue where emqx cli failed to set the Erlang cookie when the emqx directory was read-only.

- [#10154](https://github.com/emqx/emqx/pull/10154) Change the default `resume_interval` for bridges and connectors to be the minimum of `health_check_interval` and `request_timeout / 3` to resolve issue of request timeout.

- [#10157](https://github.com/emqx/emqx/pull/10157) Fix default rate limit configuration not being applied correctly when creating a new listener.

- [#10237](https://github.com/emqx/emqx/pull/10237) Ensure we return `404` status code for unknown node names in `/nodes/:node[/metrics|/stats]` API.

- [#10251](https://github.com/emqx/emqx/pull/10251) Fix an issue where rule dependencies were not prompted when deleting an ingress-type bridge in use.

- [#10313](https://github.com/emqx/emqx/pull/10313) Ensure that when the core or replicant node starting, the `cluster-override.conf` file is only copied from the core node.

- [#10327](https://github.com/emqx/emqx/pull/10327) Don't increase “actions.failed.unknown” rule metrics counter upon receiving unrecoverable data bridge errors.

- [#10095](https://github.com/emqx/emqx/pull/10095) Fix an issue where when the MySQL connector was in batch mode, clients would keep querying the server with unnecessary `PREPARE` statements on each batch, possibly causing server resource exhaustion.
  Footer

## 5.0.2

_Release Date: 2022-07-02_

Announcemnet: EMQX team has decided to stop supporting relup for opensouce edition.
Going forward, it will be an enterprise only feature.

Main reason: relup requires carefully crafted upgrade instructions from ALL previous versions.

For example, 4.3 is now at 4.3.16, we have `4.3.0->4.3.16`, `4.3.1->4.3.16`, ... 16 such upgrade paths in total to maintain.
This had been the biggest obstacle for EMQX team to act agile enought in deliverying enhancements and fixes.

### Bug fixes

- Fixed a typo in `bin/emqx` which affects macOS release when trying to enable Erlang distribution over TLS [8398](https://github.com/emqx/emqx/pull/8398)

## 5.0.1

_Release Date: 2023-03-10_

### Enhancements

- [#10019](https://github.com/emqx/emqx/pull/10019) Add low-level tuning settings for QUIC listeners.
- [#10059](https://github.com/emqx/emqx/pull/10059) Errors returned by rule engine API are formatted in a more human-readable way rather than dumping the raw error including the stack trace.
- [#9213](https://github.com/emqx/emqx/pull/9213) Add pod disruption budget to helm chart
- [#9949](https://github.com/emqx/emqx/pull/9949) QUIC transport Multistreams support and QUIC TLS ca-cert support.
- [#9932](https://github.com/emqx/emqx/pull/9932) Integrate `TDengine` into `bridges` as a new backend.
- [#9967](https://github.com/emqx/emqx/pull/9967) New common TLS option 'hibernate_after' to reduce memory footprint per idle connection, default: 5s.

### Bug Fixes

- [#10009](https://github.com/emqx/emqx/pull/10009) Validate `bytes` param to `GET /trace/:name/log` to not exceed signed 32bit integer.

- [#10015](https://github.com/emqx/emqx/pull/10015) To prevent errors caused by an incorrect EMQX node cookie provided from an environment variable,
  we have implemented a fail-fast mechanism.
  Previously, when an incorrect cookie was provided, the command would still attempt to ping the node,
  leading to the error message 'Node xxx not responding to pings'.
  With the new implementation, if a mismatched cookie is detected,
  a message will be logged to indicate that the cookie is incorrect,
  and the command will terminate with an error code of 1 without trying to ping the node.

- [#10020](https://github.com/emqx/emqx/pull/10020) Fix bridge metrics when running in async mode with batching enabled (`batch_size` > 1).

- [#10021](https://github.com/emqx/emqx/pull/10021) Fix error message when the target node of `emqx_ctl cluster join` command is not running.

- [#10032](https://github.com/emqx/emqx/pull/10032) When resources on some nodes in the cluster are still in the 'initializing/connecting' state, the `bridges/` API will crash due to missing Metrics information for those resources. This fix will ignore resources that do not have Metrics information.

- [#10041](https://github.com/emqx/emqx/pull/10041) For InfluxDB bridge, added integer value placeholder annotation hint to `write_syntax` documentation.
  Also supported setting a constant value for the `timestamp` field.

- [#10042](https://github.com/emqx/emqx/pull/10042) Improve behavior of the `replicant` nodes when the `core` cluster becomes partitioned (for example when a core node leaves the cluster).
  Previously, the replicant nodes were unable to rebalance connections to the core nodes, until the core cluster became whole again.
  This was indicated by the error messages: `[error] line: 182, mfa: mria_lb:list_core_nodes/1, msg: mria_lb_core_discovery divergent cluster`.

- [#10054](https://github.com/emqx/emqx/pull/10054) Fix the problem that the obfuscated password is used when using the `/bridges_probe` API to test the connection in Data-Bridge.

- [#10058](https://github.com/emqx/emqx/pull/10058) Deprecate unused QUIC TLS options.
  Only following TLS options are kept for the QUIC listeners:

  - cacertfile
  - certfile
  - keyfile
  - verify

- [#10076](https://github.com/emqx/emqx/pull/10076) Fix webhook bridge error handling: connection timeout should be a retriable error.
  Prior to this fix, connection timeout was classified as unrecoverable error and led to the request being dropped.

- [#10078](https://github.com/emqx/emqx/pull/10078) Fix an issue that invalid QUIC listener setting could cause a segfault.

- [#10084](https://github.com/emqx/emqx/pull/10084) Fix the problem when joining core nodes running different EMQX versions into a cluster.

  [Mria PR](https://github.com/emqx/mria/pull/127)

- [#10086](https://github.com/emqx/emqx/pull/10086) Upgrade HTTP client ehttpc to `0.4.7`.
  Prior to this upgrade, HTTP clients for authentication, authorization, and webhook may crash
  if `body` is empty but content-type HTTP header is set.
  For more details see [ehttpc PR#44](https://github.com/emqx/ehttpc/pull/44).

- [#9939](https://github.com/emqx/emqx/pull/9939) Allow 'emqx ctl cluster' command to be issued before Mnesia starts.
  Prior to this change, EMQX `replicant` could not use `manual` discovery strategy.
  Now it's possible to join cluster using 'manual' strategy.

- [#9958](https://github.com/emqx/emqx/pull/9958) Fix the error code and error message returned by the `clients` API when the Client ID does not exist.

- [#9961](https://github.com/emqx/emqx/pull/9961) Avoid parsing config files for node name and cookie when executing non-boot commands in bin/emqx.

- [#9974](https://github.com/emqx/emqx/pull/9974) Report memory usage to statsd and Prometheus using the same data source as dashboard.
  Prior to this fix, the memory usage data source was collected from an outdated source which did not work well in containers.

- [#9997](https://github.com/emqx/emqx/pull/9997) Fix Swagger API schema generation. `deprecated` metadata field is now always boolean, as [Swagger specification](https://swagger.io/specification/) suggests.

- [#10007](https://github.com/emqx/emqx/pull/10007) Change Kafka bridge's config `memory_overload_protection` default value from `true` to `false`.
  EMQX logs case when messages get dropped due to overload protection, and this is also reflected in counters.
  However, since there is by default no alerting based on the logs and counters,
  setting it to `true` may cause messages being dropped without notice.
  At the time being, the better option is to let sysadmin set it explicitly so they are fully aware of the benefits and risks.

- [#10087](https://github.com/emqx/emqx/pull/10087) Use default template `${timestamp}` if the `timestamp` config is empty (undefined) when inserting data in InfluxDB.
  Prior to this change, InfluxDB bridge inserted a wrong timestamp when template is not provided.

## 5.0.1

_Release Date: 2022-07-01_

### Enhancements

- Removed management API auth for prometheus scraping endpoint /api/v5/prometheus/stats [8299](https://github.com/emqx/emqx/pull/8299)
- Added more TCP options for exhook (gRPC) connections. [8317](https://github.com/emqx/emqx/pull/8317)
- HTTP Servers used for authentication and authorization will now indicate the result via the response body. [8374](https://github.com/emqx/emqx/pull/8374) [8377](https://github.com/emqx/emqx/pull/8377)
- Bulk subscribe/unsubscribe APIs [8356](https://github.com/emqx/emqx/pull/8356)
- Added exclusive subscription [8315](https://github.com/emqx/emqx/pull/8315)
- Provide authentication counter metrics [8352](https://github.com/emqx/emqx/pull/8352) [8375](https://github.com/emqx/emqx/pull/8375)
- Do not allow admin user self-deletion [8286](https://github.com/emqx/emqx/pull/8286)
- After restart, ensure to copy `cluster-override.conf` from the clustered node which has the greatest `tnxid`. [8333](https://github.com/emqx/emqx/pull/8333)

### Bug fixes

- A bug fix ported from 4.x: allow deleting subscriptions from `client.subscribe` hookpoint callback result. [8304](https://github.com/emqx/emqx/pull/8304) [8347](https://github.com/emqx/emqx/pull/8377)
- Fixed Erlang distribution over TLS [8309](https://github.com/emqx/emqx/pull/8309)
- Made possible to override authentication configs from environment variables [8323](https://github.com/emqx/emqx/pull/8309)
- Made authentication passwords in Mnesia database backward compatible to 4.x, so we can support data migration better. [8351](https://github.com/emqx/emqx/pull/8351)
- Fix plugins upload for rpm/deb installations [8379](https://github.com/emqx/emqx/pull/8379)
- Sync data/authz/acl.conf and data/certs from clustered nodes after a new node joins the cluster [8369](https://github.com/emqx/emqx/pull/8369)
- Ensure auto-retry of failed resources [8371](https://github.com/emqx/emqx/pull/8371)
- Fix the issue that the count of `packets.connack.auth_error` is inaccurate when the client uses a protocol version below MQTT v5.0 to access [8178](https://github.com/emqx/emqx/pull/8178)

### Others

- Rate limiter interface is hidden so far, it's subject to a UX redesign.
- QUIC library upgraded to 0.0.14.
- Now the default packages will be released withot otp version number in the package name.
- Renamed config exmpale file name in `etc` dir.

## 5.0.0

_Release Date: 2022-06-17_

### Horizontal Scalability

An extension to the Mnesia database was introduced, named 'Mria'.

EMQX nodes can continue to form a cluster (named 'core') exactly as in previous versions.

In version 5, Mria allows more nodes to join the cluster as 'replicants'.

The difference is, replicant nodes apply changes in the routing table (and configurations etc.) asynchronously, and the replicant nodes can be scaled up and down without affecting the data consistency.

This reduces the networking overheads of forming a full-mesh cluster.

In our most recent test, we managed to achieve 100 million MQTT clients connecting to a 23 nodes Mria cluster.

It's a 3 times larger cluster size compared to a typical version 4 cluster,

and 10 times more capacity comparing to our previous [10 million achievement](https://www.emqx.com/en/resources/emqx-v-4-3-0-ten-million-connections-performance-test-report)

### Reliability

With the help of the Mria clustering topology, the risk of suffering a split-view of the cluster due to network partitioning, and improves cluster stability in general.

- Only core nodes are involved in transactional writes
- Data is fully replicated to replicant nodes so they can read from local copy (i.e., from RAM)
- While core nodes are fully functional MQTT broker, one can also opt to dedicate them as pure data base nodes

### The new Dashboard

Version 5 comes with a fresh new design for the dashboard. Powered by `vue.js` at the frontend, and OpenAPI at the backend, it provides the most easy-to-use MQTT broker management UI.

Key improvements compared to version 4:

- Access control (authentication and authorization) management
- Improved rule engine and action management UI
- Visual aid view to display data flow e.g., from MQTT topics to destination data sink
- Online config update
- Gateway/Extension management
- More diagnosis tools, such as slow subscriptions and online tracing

### Typesafe Config

Starting from version 5, EMQX will use [HOCON](https://lightbend.github.io/config/) for the configuration syntax.

With a schema on top, the configs are now type-checked.

HOCON supports NGINX-like configuration layouts, and provides a native array syntax.

But don't worry, the old `cuttlefish` syntax is still supported if you prefer to keep everything flat like in version 4.

The EMQX team have put huge effort into providing sane-default values, as a result, the default configuration file is now less than 100 lines including comments.

Provided side-by-side, there is a full-blown example configuration (generated from the schema) to help us quickly find examples to copy from.

### Operability

In version 5, the interface descriptions for the Dashboard-enabled management APIs follow the OpenAPI specification 3.0. Behind the scene, the specification is actually generated from the configuration schema, the same schema which type-checks the files.

Now we have a single source of schema, which is used to guard both the configuration file and HTTP interface, also to generate config and API documents.

Another nice thing about Swagger, is that it comes with the Swagger-UI in which we can click a "try it out" button to test the API directly from the web browser, or copy-paste the `curl` command example to a console to test it out.

Another major change in operability compared to version 4 is that the changes are persisted back on disk as configuration file (in HOCON syntax).

The immediate benefit from unifying the config file interface and HTTP API interface is the hot-configuration.

Hot-configuration was a feature only available in EMQX Enterprise edition, it allows us to change a lot of configurations from Dashboard or through the APIs at runtime without having to restart the service.

In enterprise edition version 4, the hot-values were stored in the database, but not in config files, which was not very convenient for users who want to change the overridden values from the config file interface.

### Observability

The dashboard comes with more detailed monitoring metrics. We can now view up to 7 days of historical metrics data on Dashboard, and integrate with Prometheus with one click. Add log tracking and slow subscription diagnostic tools to effectively improve the user experience when troubleshooting and diagnosing abnormal client behaviors.

Another enhancement in observability is the introduction of structured logging, now most of the logs emitted from EMQX have a `msg` field. The text of this filed is an underscore-separated words, to make it more search-friendly for humans but also helps the log indexing tools to index the logs.

### Data Integration (The Old Rule Engine)

As you may notice from the dashboard, the old 'Rule Engine' has been renamed to 'Data integration'.

It includes two major functionalities: rules and data bridges.

Rule is a data processing language in SQL syntax (in addition a [`jq` language support](https://www.youtube.com/watch?v=_GwF8zvhNcQ)), which helps to filter and transform IoT messages.

Data bridge provides the ability to ingest data into EMQX or export data outside of EMQX.

Through the dashboard, users can now clearly see how IoT data is processed with rules and how the data flows from/to external data services.

Another important change: now rules and actions are configs (in config files), but not data in Mnesia (the built-in database, which is quite opaque for non-Erlang developers), meaning we'll be able to configure rules with various deployment/orchestration tools a lot easier.

### Access Control

Now we can manage authentication and authorization from the dashboard without having to restart the broker.

Previously provided as plugins, authentication and authorization (ACL) configurations are scattered in different files, and changes will often require a restart of the broker to take effect.
In version 5, all the commonly used security configurations are grouped together. There is also a user management UI provided in the dashboard, allowing you to manage users and access rules on the fly.

We can even configure different authentication rules per listener.

### Gateway

The gateway is re-implemented with a unified design language, providing independent management interfaces and security authentication capabilities for various protocols with different client attributes and life cycles, allowing users to access in a more native way. Since each gateway can be configured with its own independent authentication, authentication information of different gateway devices can now be isolated from each other to meet the needs of more scenarios.

### MQTT over QUIC

QUIC [RFC 9000](https://datatracker.ietf.org/doc/html/rfc9000), the next generation transport layer for the internet, brings amazing opportunities (and challenges too) to IoT.

EMQX team cannot wait to start experimenting MQTT on top of it.

Coming with the 5.0 release, one can configure a QUIC listener to experiment with MQTT over QUIC.

### Extension

EMQX 5.0 keeps and to a certain extent enhances the ability of plugin extensions. The plugins can now be compiled, distributed as a standalone package, and then uploaded on the Dashboard to complete the installation for the entire cluster, so they do not have to repeat the steps for each node.

### Hello Elixir

EMQX 5 is still a rebar3 project, but it now compiles with mix, if you know Elixir, you know what it means. Happy hacking.
