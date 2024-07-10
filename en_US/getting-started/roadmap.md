---
title: EMQX Product Roadmap
description: Dive into the roadmap of EMQX and stay updated with its latest features and improvements for a seamless IoT platform experience.
---

# EMQX Product Roadmap

## 2024 Roadmap

- **Durable Sessions**: Built-in high-availability durable sessions based on RocksDB.
- **Client Attributes**: Set additional attributes for each MQTT client for authentication, authorization, data integration, and MQTT extension features.
- **Kerberos Authentication**: Client access supports Kerberos authentication.
- **Message Queue**: Implement message queue functionality with a producer/consumer model using persistent queues.
- **Schema Validation**: Use Avro, Protobuf, and JSON Schema to validate messages against expected formats.
- **Message Transformation**: Transform, enrich, and restructure message content and format.
- **Rule Engine Debug and Tracing**: End-to-end testing and tracing of rules and data integration.
- **More Flexible Rule Engine**:
  - Error Actions
  - Fallback Actions
  - Conditional Actions
- **Global Multi-Region Distributed Cluster**: Deploy a cluster across different regions.
- **Cluster Linking**: Clusters in different regions can connect as a federated cluster in a unified namespace, enabling message replication.
- **MQTT Stream**: Persistently stores published messages into built-in streams, which other services can consume.
- **Hot Upgrades and Patches**: Perform incremental upgrades and patch installations via the Dashboard.
- **OIDC SSO**: The Dashboard supports the OIDC SSO protocol.
- **Multi-Tenancy**: Cluster reuse functionality within a unified namespace.
- **More Data Integrations**:
  - Elasticsearch Data Integration
  - Amazon S3 Data Integration
  - Azure Blob Storage Data Integration
  - CouchbaseDB Data Integration
  - Snowflake Data Integration
  - Message bridging supports SysKeeper firewall traversal.

## Future Versions

- Default release using Elixir.
- Splitting EMQX node roles.
- Using QUIC protocol in cluster communication.
- Supporting other languages and external runtimes (e.g., JavaScript, Python) in the rule engine.
