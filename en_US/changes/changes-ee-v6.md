# EMQX Enterprise Version 6

## 6.0.0

### Highlights

- **Major Version Release**: EMQX Enterprise 6.0.0 is the first release of the EMQX Enterprise version 6 series, bringing significant architectural improvements and new capabilities.

- **Enhanced AWS Integration**: Support for Instance Metadata Service v2 APIs from EC2 instances when using S3 or S3Tables data integration. This enables seamless access to S3 buckets without manual AWS credential configuration, leveraging IAM roles for better security.

- **Elixir Support**: All packages now ship with Elixir support through the Mix build system, opening EMQX to the Elixir community and enabling better tooling with IEx console.

- **New Data Integrations**:
  - BigQuery Connector and Action for appending data to Google BigQuery
  - Snowpipe Streaming upload mode for Snowflake Action (preview feature)
  - Parquet format support for S3Tables Action

- **Durable Storage Optimizations**: Significant RAM usage and storage efficiency improvements with new RocksDB configuration options and ASN1 serialization schema as default.

- **Enhanced LDAP Support**: LDAP authorization now supports extended ACL rules in JSON format, and LDAP authentication can fetch ACL rules directly from LDAP with client-side caching.

- **Improved Tracing**: Configurable limits for maximum traces (`trace.max_traces`) and trace file sizes (`trace.max_file_size`), with optimized implementation to prevent atom leaks.

- **Cluster Management**: New `cluster.description` configuration option allows users to set and display custom cluster descriptions in the EMQX Dashboard.

### Enhancements

#### Rule Engine

- [#15631](https://github.com/emqx/emqx/pull/15631) A new API endpoint is added to list all models available for an AI provider.

- [#15467](https://github.com/emqx/emqx/pull/15467) Expose transport options for AI Completion Providers.
  These options allow configuring connection timeouts and maximum connections to an AI Completion Provider.

#### Data Integration

- [#15635](https://github.com/emqx/emqx/pull/15635) Added new `key` and `tag` template fields for the RocketMQ Action which sets the message's key and tag, respectively.  Also, added a new `key_dispatch` value for the `strategy` field.

- [#15621](https://github.com/emqx/emqx/pull/15621) Now, `access_key_id` and `secret_access_key` are optional fields for the S3Tables Connector.  If omitted, they'll be obtained from the Instance Metadata Service v2 APIs from the EC2 instance where EMQX is deployed.

- [#15542](https://github.com/emqx/emqx/pull/15542) Upgraded our `erlcoud` library to `3.8.3.0`.  This allows one to setup a S3 Connector without specifying Access Key Id and Secret Access Key, so long as the EC2 instance EMQX is running in has the correct IAM permissions to read/write to the configured bucket(s).

- [#15418](https://github.com/emqx/emqx/pull/15418) Added new Connector and Action that appends data to BigQuery.

- [#15401](https://github.com/emqx/emqx/pull/15401) Added support for Snowpipe Streaming upload mode for Snowflake Action.  Note: this is currently a preview feature by Snowflake, and support for it is available to all accounts on AWS only.

- [#15387](https://github.com/emqx/emqx/pull/15387) Improved Kinesis Producer Connector and Action health checks to mitigate the occurrence of rate limiting when calling `ListStreams` and `DescribeStream` APIs.  Now, we limit the the calls per Connector to such APIs to 5/s and 10/s, respectively.  If a Connector or Action cannot call their health check API before timing out, they will simply maintain their current status.  If they receive a throttling response (e.g.: `LimitExceededException`), they will also maintain their current status.

  Introduced a new `resource_opts.health_check_interval_jitter` configuration to add an uniform random delay to `resource_opts.health_check_interval`, so that multiple Actions under the same Connector will seldom run their health checks simultaneously.

- [#15371](https://github.com/emqx/emqx/pull/15371) Added `tags` fields to the return of `GET /actions_summary` and to the fallback actions returned in `GET /actions/:id`.

- [#15360](https://github.com/emqx/emqx/pull/15360) Added support for writing data files in Parquet format for S3Tables Action.

- [#15176](https://github.com/emqx/emqx/pull/15176) Upgrade the GreptimeDB connector client and support an optional new parameter `ttl` to set the default time-to-live for automatically created tables.

- [#15585](https://github.com/emqx/emqx/pull/15585) Updated our `brod` client to version 4.4.4. This expands the supported Kafka API ranges, in particular due to the `JoinGroups` API `v0`-`v1` being deprecated.

- [#15628](https://github.com/emqx/emqx/pull/15628) Remove HStreamDB data integration.

#### Access Control

- [#15349](https://github.com/emqx/emqx/pull/15349) Optimize external resource management for authentication and authorization. Previously, EMQX could remain connected to a resource configured for a disabled authentication or authorization provider.

- [#15294](https://github.com/emqx/emqx/pull/15294) Enhance LDAP authentication and authorization.
  LDAP authorization now supports extended ACL rules in JSON format.
  LDAP authenticaton now can fetch ACL rules from LDAP. These rules are cached in the client's metadata, so authorization is performed without additional LDAP queries.

#### Smart Data Hub

- [#15525](https://github.com/emqx/emqx/pull/15525) Now, when attempting to remove an Internal Schema which is being referenced by either a Schema Validation or a Message Transformation, the removal will be denied.

#### Durable Storage

- [#15463](https://github.com/emqx/emqx/pull/15463) Durable storage RAM usage and storage efficiency optimizations.

  1. Added the following configuration parameters for the durable storage:

  - `durable_storage.messages.rocksdb.write_buffer_size`: RocksDB memtable size per shard.
  - `durable_storage.messages.rocksdb.cache_size`: RocksDB block size per shard.
  - `durable_storage.messages.rocksdb.max_open_files`: Limits the number of file descriptors used by RocksDB per shard.
  - `durable_storage.messages.layout.wildcard_thresholds`: Allows to tune wildcard thresholds for the `wildcard_optimized_v2` storage layout

  2. The default `serialization_schema` for the messages has been changed to `asn1`.

#### Observability

- [#15594](https://github.com/emqx/emqx/pull/15594) Exposed maximum number of traces allowed to exist in the cluster simultaneously as a configuration option `trace.max_traces`. This limit does not apply to node-local traces managed through `emqx ctl trace`.

  Optimized tracing implementation to eliminate potential atom leaks per created trace.

- [#15556](https://github.com/emqx/emqx/pull/15556) Exposed maximum trace file size limit per each individual trace as a configuraion option `trace.max_file_size`.

- [#15364](https://github.com/emqx/emqx/pull/15364) Add HTTP header configuration items to the OpenTelemetry integration to adapt to collectors with HTTP authentication.

#### Deployment

- [#15484](https://github.com/emqx/emqx/pull/15484) Changed our build system so that all packages are build with [Elixir](https://elixir-lang.org/)'s [Mix](https://hexdocs.pm/elixir/introduction-to-mix.html), so that all now ship with Elixir support.  This opens up EMQX to Elixir community and allows us to use Elixir dependencies when required, besides enabling use of [IEx](https://hexdocs.pm/iex/IEx.html) as a better EMQX console.

- [#15399](https://github.com/emqx/emqx/pull/15399) Now, `node_dump` will export the current system configuration in HOCON format with redacted secrets.

#### Clustering

- [#15600](https://github.com/emqx/emqx/pull/15600) Added a new `cluster.description` configuration that allows one to add a description to the EMQX cluster.  This description can be changed via `PUT /cluster`, and viewed in the `GET /cluster` response.

#### Performance

- [#15536](https://github.com/emqx/emqx/pull/15536) Disable the `node.global_gc_interval` configuration by default.

- [#15539](https://github.com/emqx/emqx/pull/15539) Optimize Erlang VM parameters.

  - Increase the buffer sizes for distributed channels to 32MB to avoid `busy_dist_port` alarms during intensive Mnesia operations: `+zdbbl 32768`
  - Disable scheduler busy-waiting to reduce CPU usage observed by the operating system: `+sbwt none +sbwtdcpu none +sbwtdio none`
  - Set scheduler binding type to `db` to reduce message latency: `+stbt db`

- [#15451](https://github.com/emqx/emqx/pull/15451) Introduce experimental `socket` backend for TCP listeners, designed to improve message processing latency and reduce compute resource usage. This can be enabled via the new `tcp_backend` listener option.

### Bug Fixes

#### Data Integration

- [#15647](https://github.com/emqx/emqx/pull/15647) Previously, if the user of a MongoDB Connector did not have sufficient privileges to perform `find` queries in a `foo` collection, it would be considered disconnected.  This has been fixed.

- [#15603](https://github.com/emqx/emqx/pull/15603) Fixed an issue with the MQTT bridge when a stale connection was displayed as `Connected' and the connection was not re-established.


- [#15522](https://github.com/emqx/emqx/pull/15522) Fixed an issue where Snowflake Connector would fail to start correctly is `username` was not provided.

- [#15476](https://github.com/emqx/emqx/pull/15476) When most of the Actions that use aggregated mode (Azure Blob Storage, Snowflake, S3Tables) had a delivery that failed, the following log would be printed:

  ```
  "emqx_connector_aggreg_delivery:format_status/1 crashed"
  ```

  This has been fixed, and more information about the delivery process will now be logged.

- [#15394](https://github.com/emqx/emqx/pull/15394) Fixed a very rare race condition in which Action metrics could end up in an inconsistent state.

- [#15383](https://github.com/emqx/emqx/pull/15383) Fix a potential resource leak in MQTT bridge when the bridge fails to start. Previously, the topic index table was not properly cleaned up when the bridge failed to start.

#### Rule Engine

- [#15569](https://github.com/emqx/emqx/pull/15569) Fixed an issue where a Republish Rule Action could fail if the template for `direct_dispatch` was an empty string or resolve to a non-boolean value.  Now, if such situations occur, the default value, `false`, will be used.

#### Core MQTT Functionalities

- [#15518](https://github.com/emqx/emqx/pull/15518) Resolve a race condition that may lead to accumulating inconsistencies in the routing table and shared subscriptions state in the cluster when a large number of shared subscribers disconnect simultaneously.

- [#15416](https://github.com/emqx/emqx/pull/15416) Fixed occasional warning-level log events and crashes during session expiration of WebSocket connections, introduced by recent WebSocket performance improvements. These had no impact on broker capacity, but produced log entries like the following:
  * `error: {function_clause,[{gen_tcp,send,[closed,[]],[{file,“gen_tcp.erl”},{line,966}]},{cowboy_websocket_linger,commands,3,[{file,“cowboy_websocket_linger.erl”},{line,665}]},...`
  * `message: {tcp,#Port<0.364>,<<136,130,...>>}, msg: emqx_session_mem_unknown_message`

- [#15396](https://github.com/emqx/emqx/pull/15396) Removed redundant cleanup operations for shared subscriptions of disconnected clients, which were prone to crashes under high disconnect volume, resulting in potential inconsistencies in the global broker state.

- [#15361](https://github.com/emqx/emqx/pull/15361) Fixed a function clause error when parsing a malformed `User-Property` pair where the pair length is wrong (too short).

#### Access Control

- [#15489](https://github.com/emqx/emqx/pull/15489) Fix OIDC issuer scheme validation.

  The previously unsupported issuer URLs listed below are now supported.

  - `https://xxxxxxxx:8443/webman/sso/.well-known/openid-configuration`
  - `hostname`

#### Gateway

- [#15342](https://github.com/emqx/emqx/pull/15342) Fixed NATS gateway crash when clientinfo override templates contain undefined packet fields by returning empty binary instead of undefined atom.

#### Security

- [#15581](https://github.com/emqx/emqx/pull/15581) Upgrade OTP version from 26.2.5.2 to 26.2.5.14

  This upgrade includes two TLS-related fixes relevant to EMQX:

  - Fixed a crash in TLS connections caused by a race condition during certificate renewal.
  - Added support for RSA certificates signed with PSS parameters. Previously TLS handshake may fail with `invalid_signature`.

#### Deployment

- [#15580](https://github.com/emqx/emqx/pull/15580) Add emqxLicenseSecretRef variable to EMQX Enterprise helm chart, allowing users to specify a Kubernetes secret containing the EMQX license key. This fixes the issue with defunct emqxLicenseSecretName variable.

- [#15553](https://github.com/emqx/emqx/pull/15553) Fixes an issue with helm chart when all nodes except one will be crashing if the chart is deployed with default values.

#### HTTP Server

- [#15547](https://github.com/emqx/emqx/pull/15547) Fixed error when an HTTP request with a large body is sent.
