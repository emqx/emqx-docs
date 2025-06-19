# Incompatible Changes in EMQX 5.8

## e5.8.6

- [#14802](https://github.com/emqx/emqx/pull/14802) Starting from this version, plugin installation via the REST API or Dashboard requires explicit permission. Users must obtain this permission using the following CLI command before installing.

  ```bash
  emqx ctl plugins allow NAME-VSN
  ```

  This change enhances security by preventing unauthorized plugin installations. Users managing plugins via the API or Dashboard must adjust their workflows accordingly.

## e5.8.5

- [#14703](https://github.com/emqx/emqx/pull/14703) Introduced a change to the maximum allowed value for `force_shutdown.max_heap_size`, which is now set to `128GB`. If the `max_heap_size` was previously set to a value exceeding 128GB, this could lead to issues after upgrading, such as during configuration reloading or updates.

## e5.8.4

- [#14360](https://github.com/emqx/emqx/pull/14360) When requesting Prometheus metrics in JSON format, the `client` top-level key will now always be an array of JSON objects, rather than a single JSON object. This change may affect how your monitoring tools process the data.

- [#14370](https://github.com/emqx/emqx/pull/14370) IoTDB data integration configuration changes:
  
  - The self-describing template has been removed. EMQX now only processes messages using the configured data templates and no longer attempts to extract the template from the message payload.
  
  - Each MQTT message can now only carry a single `payload`. Arrays of payloads are no longer supported. As a result, an MQTT message is processed as a single atomic insert operation (either a single or batch insert) into IoTDB. Generating multiple IoTDB operations from a single MQTT message is no longer possible.
  
  - The `data type` is now treated as a plain value rather than a template value.
  
  - The REST API driver now only supports IoTDB 1.3.x and later versions.
  
  - The Thrift driver now supports "batch" mode. 
  
    **Important**: To prevent overlapping timestamps in batch mode, it’s recommended to use the MQTT message timestamp (`${timestamp}`) or include a time field in the payload (e.g., `${payload.time}`).

## e5.8.3

- [#14305](https://github.com/emqx/emqx/pull/14305) Removed support of hashing algorithms `MD4`, `MD5`, and `RIPEMD-160` from authentication as they are not compliant with [NIST Secure Hash Standard](https://www.nist.gov/publications/secure-hash-standard).

## e5.8.2

- [#14004](https://github.com/emqx/emqx/pull/14004) Fixed an issue in Cluster Linking where overlapping topic filters in the `topics` configuration caused inconsistent and incomplete cross-cluster message routing. Each topic filter is now handled individually. Therefore, redundant topic filters (e.g. `t/1` and `t/+`) in the `topics` configuration are now considered invalid. The link will fail to start if such a configuration is detected.
  
- [#14015](https://github.com/emqx/emqx/pull/14015) Kafka/Confluent/Azure Event Hub Producers with a dynamic topic (i.e., a topic that contains placeholders) no longer support disk buffering. Only memory and hybrid modes are now supported.

- [#14106](https://github.com/emqx/emqx/pull/14106) Added a validation that forbids a single Kafka Consumer connector from containing sources with repeated Kafka topics. If you want to repeat topics, create a new connector and source(s).


## e5.8.1

- [#13792](https://github.com/emqx/emqx/pull/13792) The default expiration time for a banned item that is created without an `until` value is now `infinity` (previsouly capped at 1 year limit).

- [#13742](https://github.com/emqx/emqx/pull/13742) Fixed an issue when a client would receive retained messages for a topic starting with `$` when it subscribed to topic `#` or `+`.

  This fix satisfies the requirement of [MQTT-4.7.2-1](https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901246).

## e5.8.0

- [#13080](https://github.com/emqx/emqx/pull/13080) Updated the default value of the `mqtt.retry_interval` configuration from 30 seconds to `infinity`.

  Previously, EMQX would automatically retry message deliveries every 30 seconds by default. With the new default set to `infinity`, EMQX will no longer retry message deliveries automatically. This change aligns with MQTT specification standards, which generally do not recommend in-session message delivery retries.

  We understand that some users rely on the retry feature, so the ability to configure a specific retry interval is still available for backward compatibility.

- [#13190](https://github.com/emqx/emqx/pull/13190) Discontinued support for releases on CentOS 7 and Ubuntu 18. EMQX will no longer provide builds for these operating systems due to their end-of-life status.

- [#13248](https://github.com/emqx/emqx/pull/13248) Replaced the `builtin` durable storage backend with two new backends to provide better flexibility and scalability:

  - **`builtin_local`**: A durable storage backend that does not support replication, making it suitable for single-node deployments. This backend is available in both the open-source and enterprise editions of EMQX but is not compatible with multi-node clusters.
  - **`builtin_raft`**: A durable storage backend utilizing the Raft consensus algorithm for data replication across multiple nodes. This backend is exclusively available in the enterprise edition of EMQX, providing enhanced data durability and fault tolerance.

  Additionally, several Prometheus metrics have been renamed to better reflect their functions:

  - `emqx_ds_egress_batches` has been renamed to `emqx_ds_buffer_batches`
  - `emqx_ds_egress_batches_retry` has been renamed to `emqx_ds_buffer_batches_retry`
  - `emqx_ds_egress_batches_failed` has been renamed to `emqx_ds_buffer_batches_failed`
  - `emqx_ds_egress_messages` has been renamed to `emqx_ds_buffer_messages`
  - `emqx_ds_egress_bytes` has been renamed to `emqx_ds_buffer_bytes`
  - `emqx_ds_egress_flush_time` has been renamed to `emqx_ds_buffer_flush_time`

- [#13526](https://github.com/emqx/emqx/pull/13526) Removed the Core-replicant feature from the Open-Source Edition. Starting from release 5.8, all nodes running the Open-Source Edition will operate in the Core role. This change does not impact Enterprise Edition users, who will continue to have access to the Core-replicant functionality. Additionally, the obsolete `cluster.core_nodes` configuration parameter has been removed as it is no longer needed.

- [#13372](https://github.com/emqx/emqx/pull/13372) Now the number of connections accepted by gateways is governed by the licensing terms, ensuring compliance with the allowed connection limits.
