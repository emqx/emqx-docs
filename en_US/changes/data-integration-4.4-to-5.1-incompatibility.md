# Data Integration Compatibility between e5.1 and e4.4

e5.1 completely upgraded the concept of Data Integration

1. The previous process of ***Rule -> Action -> Resources*** has been modified to **Rule -> Bridges.** 

   1. In e5.1, When adding an Action for a certain rule in 5.1, you need to create a bridge first. and modify this SQL template of the bridge to adapt the output of the rules. But in e4.4, there is a configuration entity for Action here.

      e5.1: Configure the Action(Bridge) for a Rule

      <img src="./assets/config-action-for-rule.png" alt="config-action-for-rule" style="zoom:67%;" />

2. Move the Modules/Message Publish into the Bridges

   e4.4: The Message Publish Modules

   <img src="./assets/message-publish-modules.png" alt="message-publish-modules" style="zoom:67%;" />

3. Removed the “Save offline Message“ features

The Save Offline Message features in e4.4 docs

\4. Removed the “Get Subscriptions“ features

The Get Subscription features in e4.4 docs

\5. Tablestore, DolphinDB, Lindorm, SAP Event Mesh is not supported now

\6. Removed the “EMQX Bridge“ feature



## Common incompatibility issues

- all SSL-related confs (ssl , cafile, keyfile, certfile, verify) → changed to a unified structure and name. i.e: ssl.cacertfile ssl.certfile ssl.keyfile ssl.verify and more ssl options
- There’s no equivalent of the “save offline messages to” feature of fetching messages from external databases when a client subscribes to topics (i.e.: the $events/session_subscribed event + bridge rule action to read them).

## Incompatibility in functionality, configuration items

### Cassandra

https://emqx.atlassian.net/browse/EMQX-9925 

**Configuration Names:**

- Changed nodes to servers

### Kafka Producer

- Changed 
  - servers → bootstrap_hosts
  - authentication_mechanism → authentication
  - sync_timeout → sync_query_timeout
  - send_buffer → socket_opts.sndbuf
  - tcp_keepalive → socket.tcp_keepalive
  - strategy → partition_strategy
  - cache_mode → kafka.buffer.mode
  - Buffer mode enum memory+disk → hybrid
  - highmem_drop → kafka.buffer.memory_overload_protection
- No equivalent in e5.1
  - query_api_versions
  - kafka_ext_headers
- Nested replayq related options (e.g.: max_batch_bytes) under kafka key 
- Now, message key is templatable, whereas before it could be only a few option.

### Kafka Consumer

- Changed:
  - servers → bootstrap_hosts
  - max_bytes → kafka.max_batch_bytes
  - offset_reset_policy enum: {reset_to_latest, reset_by_subscriber} → {latest, earliest}
- There’s no pool_size in e5.1: the number of workers is set automatically by the lib depending on the number of partitions in the topic(s).
- In e4.4, only plain SASL was supported for authentication.  In e5.1, we support the same mechanisms as Kafka Producer.

### Pulsar Consumer

There’s no Pulsar Consumer in e5.1.0.

### Pulsar Producer

- In e5.1, the bridge only produces messages using the driver’s async API, without option for sync API.
- Now, message key is templatable, whereas before it could be only a few option.
- Changed:
  - Buffer mode enum memory+disk → hybrid
  - max_total_bytes → buffer.per_partition_limit
  - segment_bytes → buffer.segment_bytes

### Redis

#### Common to all 3 types

- cmd → command_template

#### Cluster

- There’s no database field in e5.1.
- There’s no equivalent for ttl (offline messages from e4.4) in e5.1.

### Postgres

- No differences in connector
- Action:
  - Batching configuration has moved to resource_opts.*.
    - enable_batch = true (e4.4) = resource_opts.batch_size > 1 (e5.1)
    - batch_time is hidden and defaults to 0 in e5.1
    - sql → prepare_statement

## MySQL

- Changed
  - user → username
- Action:
  - Batching configuration has moved to resource_opts.*.
    - enable_batch = true (e4.4) = resource_opts.batch_size > 1 (e5.1)
    - batch_time is hidden and defaults to 0 in e5.1
    - sql → prepare_statement

### MQTT

- Changed
  - address → server
  - pool_size → {egress,ingress}.pool_size
  - reconnect_interval → resource_opts.health_check_interval
- Without equivalent in e5.1:
  - append 
  - mountpoint
- disk_cache = on (e4.4) would be somewhat equivalent to resource_opts.buffer_mode =  volatile_offload, but the latter is a hidden configuration (it defaults to memory_only).
- There’s no RPC MQTT bridge equivalent in e5.1.
- Actions
  - forward_topic → egress.remote.topic
  - payload_tmpl → payload

### InfluxDB

#### Common to both API v1 and API v2

- host + port -> server
- https_enabled & ssl options like tls_version → ssl
- Actions
  - There’s no equivalent for int_suffix in e5.1; the type is directly specified in write_syntax.
  - measurement + timestamp + fields + tags → write_syntax

### Tablestore, DolphinDB, Lindorm, SAP Event Mesh

There are no equivalent bridges in e5.1.

### Clickhouse

- Changed
  - server → url
  - user → username
  - key → password

### Dynamo

- No equivalent in 5.1
  - region
- Now we have payload_template.

### HStreamDB

- Changed
  - server → url
- No equivalent in 5.1
  - grpc_timeout
  - partition_key
  - grpc_flush_timeout

### IoTDB

- Changed
  - host + rest_port -> base_url
  - request_timeout → resource_opts.request_ttl

### MongoDB

- Changed
  - login → username
  - connectTimeoutMS → connect_timeout_ms
  - rs_set_name → replica_set_name
  - payload_tmpl → payload_template

### OpenTSDB

- Changed
  - sync → resource_opts.query_mode = sync

### Oracle

- Changed
  - user → username

## SQLServer

No incompatibilities found.

### TDengine

- Changed
  - host + port → server
  - dbname → database

### GCP PubSub Producer

- Deprecated
  - flush_mode
  - flush_period_ms

### RabbitMQ Producer

- Changed
  - server → host + port
  - payload_tmpl → payload_template
  - durable  → delivery_mode
- No equivalent in 5.1
  - exchange_type

### RocketMQ

- No equivalent in 5.1
  - namespace
  - strategy
  - key
- Changed
  - type → resource_opts.query_mode
  - payload_tmpl → payload_template