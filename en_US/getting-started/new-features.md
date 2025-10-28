---
description: This section lists the new features introduced in EMQX 5.2 and later.
---

# New Features

This page highlights major new features supported in the current release. Note that it does not list every feature provided by EMQX.

## EMQX 6.0.0 (Latest)

### Message Queue

Message Queue unifies reliable real-time MQTT publish/subscribe with asynchronous message queueing within EMQX, without relying on external queuing services.

Unlike traditional MQTT, which depends on subscriber availability, Message Queues decouple publishers and subscribers by buffering messages on the server. Messages matching a specified topic filter are stored persistently and can be consumed later using a special topic format: `$q/{topic_filter}`. This ensures reliable message delivery, even when clients are offline or the network is unstable.

<img src="./assets/messsage_queque.png" alt="messsage_queque" style="zoom: 33%;" />

This feature enables MQTT to handle both real-time and delayed workloads, simplifying IoT system architecture by removing the need for external queuing systems like Kafka or RabbitMQ. It’s ideal for scenarios where message durability, reliable delivery, and offline buffering are critical.

#### Feature Highlights
- **Unified Messaging Model**: Combines lightweight MQTT with enterprise-grade queuing in a single system.
- **Offline Storage**: Messages are retained even when subscribers are disconnected.
- **Last-Value Retention**: Optionally keep only the latest message per key (e.g., device ID), perfect for fast-changing data like sensor readings.
- **Flexible Dispatch Strategies**: Choose from Random, Round Robin, or Least Inflight Subscriber to distribute messages efficiently.
- **Guaranteed Delivery**: Supports persistent storage and QoS 1 delivery to ensure no data is lost.

Learn more in the [Message Queue documentation](../message-queue/message-queue-concept.md).

### Multi-Tenancy with Namespaced Roles

EMQX 6.0.0 introduces namespaced roles to enhance [multi-tenancy](../multi-tenancy/namespace-overview.md) and access control across large-scale IoT deployments. With this feature, users are assigned to specific namespaces, isolated environments where they can manage their own resources such as Rules, Connectors, and Actions, without accessing or impacting other tenants.

Namespaced roles support fine-grained permissions such as Administrator and Viewer, and can be managed through the Dashboard, API, or CLI, making it easier to delegate responsibilities while ensuring secure isolation between teams, departments, or customers.

#### Feature Highlights
- **Secure Isolation**: Users can only access and manage resources within their assigned namespace (e.g., `ns:team_a::administrator`).
- **Granular Access Control**: Namespaced users have full access to namespace-specific resources, but cluster-level settings remain read-only unless granted global admin rights.
- **Simplified Operations**: Roles can be created and assigned easily during user creation.
- **Scalability for Enterprises**: Ideal for organizations offering multi-tenant MQTT services or managing internal business units with separate responsibilities.

#### Additional Enhancements
- **Improved Observability**: Dashboard views are filtered by namespace for better focus.
- **Optimized Session Tracking**: Namespaced session counts now refresh on demand (under 1,000 sessions) or every 5 seconds otherwise, ensuring better performance and accuracy.

For detailed instructions, see [Create a User with a Namespaced Role](../dashboard/system.md#create-a-user-with-a-namespaced-role.md).

### Optimized Durable Storage

EMQX 6.0.0 delivers significant improvements to durable storage, enhancing performance and scalability for high-throughput IoT workloads. By decoupling session data from other broker metadata, EMQX now consumes less RAM and achieves better storage efficiency, allowing more connections per node.

#### Optimized RocksDB Parameters

New configuration options give precise control over memory and performance:

- `durable_storage.messages.rocksdb.write_buffer_size`: Controls per-shard memtable size.
- `durable_storage.messages.rocksdb.cache_size`: Sets block cache size per shard.
- `durable_storage.messages.rocksdb.max_open_files`: Limits file descriptors per shard.
- `durable_storage.messages.layout.wildcard_thresholds`: Optimizes wildcard handling in storage layout.

#### Additional Enhancements

- **Efficient Serialization**: The default serialization format has changed to ASN.1, reducing storage size and improving processing speed.
- **Faster Access & Lower Overhead**: Improved storage layout leads to quicker message retrieval and reduced disk and memory overhead.

These enhancements make EMQX more capable of handling large-scale, persistent MQTT workloads with consistent performance.

### Expanded Support in Data Integration

EMQX 6.0.0 continues to strengthen its data integration capabilities, empowering users to seamlessly connect MQTT data with modern cloud and database ecosystems for real-time analytics, processing, and storage.

#### New Integrations

The following integrations have been newly added in EMQX 6.0.0:

- **[Google BigQuery](../data-integration/bigquery.md)**: Integrate MQTT data with BigQuery for large-scale data warehousing and advanced querying, enabling insights from massive IoT datasets.
- **[AWS AlloyDB](../data-integration/alloydb.md)**, **[CockroachDB](../data-integration/cockroachdb.md)**, and **[AWS Redshift](../data-integration/redshift.md)**: Stream MQTT data to these high-performance databases for real-time analytics and scalable storage. Perfect for enterprise-grade IoT analytics.

#### Enhanced Integrations

In addition to new integrations, EMQX 6.0.0 brings powerful enhancements to existing integrations, improving performance, usability, and cloud-native compatibility:

- **Snowflake Snowpipe Streaming**: Now supports low-latency data ingestion into Snowflake tables via Snowpipe Streaming (preview feature of Snowflake), available for AWS-hosted accounts.
- **RocketMQ Action**: Adds support for `key` and `tag` templates, plus a `key_dispatch` strategy for flexible message routing and metadata enrichment.
- **S3 Tables Connector**: `access_key_id` and `secret_access_key` are now optional, with automatic retrieval from EC2 Instance Metadata Service v2 APIs in AWS-hosted environments.
- **RabbitMQ Sink**: Allows customization of Headers and Properties Templates to enhance message routing and compatibility within RabbitMQ.

### Other Enhancements

#### Advanced LLM-Based MQTT Data Processing

EMQX 6.0.0 enhances LLM-based data processing with support for [Google Gemini models](../flow-designer/gemini-node-quick-start.md), alongside OpenAI and Anthropic Claude.

#### Enhanced LDAP Support

LDAP authorization now supports extended ACL rules in JSON format, and LDAP authentication can fetch ACL rules directly from LDAP with client-side caching.

#### Improved Tracing

Configurable limits for maximum traces (`trace.max_traces`) and trace file sizes (`trace.max_file_size`). After `max_file_size` is reached, the trace log will rotate to a new file instead of halting.

#### Cluster Management

New `cluster.description` configuration option allows users to set and display custom cluster descriptions in the EMQX Dashboard. For how to add the cluster description on the Dashboard, see [Dashboard -> Management -> Cluster Settings](../dashboard/cluster_settings.md#cluster).

### More Features

In addition to the highlights covered above, recent EMQX updates include many other new features and enhancements. For a complete list, see the [Release Notes](../changes/changes-ee-v6.md).

### Breaking Changes

For complete information about what's deprecated and breaking changes, see [Incompatible Changes between EMQX 5.x and EMQX 6.0](../changes/breaking-changes-6.0.md)

## EMQX 5.x Series

For a complete list of features and updates in EMQX 5.x, please refer to: [EMQX 5.10 Documentation – What's New](https://docs.emqx.com/en/emqx/5.10/getting-started/new-features.html).
