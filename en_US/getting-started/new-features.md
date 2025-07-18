---
description: This section lists the new features introduced in EMQX 5.2 and later.
---

# New Features

This page highlights major new features supported in the current release. Note that it does not list every feature provided by EMQX.

## Cluster Linking

Cluster Linking enables seamless, secure, and efficient message sharing between geographically distributed EMQX clusters. Unlike traditional MQTT bridges, which forward all messages and often require topic prefixes to prevent feedback loops, Cluster Linking transfers only relevant messages based on active subscriptions, minimizing bandwidth use, reducing latency, and increasing scalability.

The configuration and management of Cluster Linking are designed to be simple and flexible. You can create, modify, and monitor cluster links directly from the EMQX Dashboard, configuration file, or through REST APIs. EMQX also provides visual status indicators and link statistics for real-time visibility. 

To get started with the Cluster Linking, see [Quick Start with Cluster Linking](../cluster-linking/quick-start.md).

<img src="./assets/cluster_linking_feature.png" alt="cluster_linking_feature" style="zoom:80%;" />

## Namespace

Namespaces provide logical multi-tenancy within a single EMQX cluster, enabling you to isolate clients, topics, quotas, and configurations per tenant without creating separate clusters. Each namespace is identified using a special client attribute (`tns`), which can be derived from metadata like username or Server Name Indication (SNI), a flexible method that ensures accurate tenant attribution at connection time.

Namespaces support two creation modes:

- **Explicit**: Administrators define namespaces manually via the Dashboard or REST API.
- **Automatic**: EMQX generates namespaces dynamically by extracting the `tns` attribute from incoming client connections.

Currently, namespaces support tenant-level rate limiting configuration, allowing resource allocation and usage control per tenant. To learn more details about this feature and how to create and configure the namespaces, see the [Namespace](../multi-tenancy/namespace-overview.md) section.

## Smart Data Hub

The [Smart Data Hub](../data-integration/smart-data-hub.md) offers a unified solution for managing schema-based message validation and transformation across MQTT data streams. It simplifies the development of structured and reliable data flows through key components:

### Schema Registry

The Schema Registry now supports both internal schemas (like JSON, Avro, and Protobuf) and external schemas via HTTP services. For formats not natively supported, EMQX can delegate schema operations to [external HTTP services](../data-integration/schema-registry-example-external-http.md) via the `schema_encode` and `schema_decode` functions.

### Schema Validation

[Schema validation](../data-integration/schema-validation.md) ensures that only messages conforming to predefined formats are processed or delivered. EMQX supports JSON Schema, Protobuf, Avro, and rule engine SQL syntax for validation. Based on the outcome, users can configure actions such as dropping messages, disconnecting clients, or triggering rule engine events for failed validations.

### Message Transformation

[Message Transformation](../data-integration/message-transformation.md) allows users to define transformation pipelines that decode, modify, and re-encode messages before delivery or further processing. The system supports nested transformations, multiple encoders/decoders, and dynamic value assignments using [Variform expressions](../configuration/configuration.md#variform-expressions).

## LLM-Based MQTT Data Processing

EMQX 5.10.0 adds support for [LLM-based MQTT data processing](../flow-designer/llm-based-data-processing.md) in Flow Designer. It integrates models like OpenAI’s GPT or Anthropic’s Claude to process MQTT messages using natural language prompts. Processing nodes call the AI models via reusable completion profiles and return results for further actions such as republishing or storage. This feature is ideal for intelligent, contextual workflows in low-throughput scenarios.

## Expanded Support in Data Integration

Recent versions of EMQX have significantly enhanced the data integration capabilities. In addition to expanding support for more data sinks and services, data integration now includes fallback actions, a robust mechanism designed to increase reliability in real-time IoT data processing.

New data integrations supported in recent versions include, but are not limited to, the following:

- **[Amazon S3 Tables](../data-integration/s3-tables.md)**: Transforms MQTT data into Iceberg-formatted tables and streams them directly into S3. It eliminates the need for traditional databases while retaining SQL-like querying capabilities.
- **[Apache Doris](../data-integration/apache-doris.md)**: Processes the MQTT messages, maps them into structured data, and writes to Doris via HTTP or JDBC. You can query your IoT data in real time using standard SQL and build live dashboards with BI tools like Grafana.
- **[Snowflake](../data-integration/snowflake.md)**: Writes the processed data to the Snowflake Stage and loads it into a Snowflake table. Safely store IoT data in Snowflake for long-term archival and leverage Snowflake's data warehousing and analytics capabilities to perform real-time or batch analysis.

### Fallback Actions

Introduced in EMQX 5.9.0, fallback actions provide a way to handle failures during data delivery. If a primary action fails due to delivery errors, buffer overflow, or request timeouts, the system can automatically trigger one or more fallback actions.

This feature is critical for minimizing data loss, improving system resilience, and enabling better observability. To learn more about the feature, see [Fallback Actions](../data-integration/data-bridges.md#fallback-actions).

## Enhanced Security

Recent versions bring significant improvements to access control, ensuring that EMQX meets enterprise security standards while remaining flexible and easy to manage. These enhancements help protect data integrity, support regulatory compliance, and prevent unauthorized access across complex IoT environments.

In recent versions, EMQX supports more authentication and authorization methods, providing more flexible and fine-grained access control capabilities. Newly supported features include:

- **[Authenticator Preconditions](../access-control/authn/authn.md#authenticator-preconditions)**: Enable conditional execution of authenticators based on client metadata.
- **[LDAP Integration](../access-control/authn/ldap.md)**: Authenticate users against an external LDAP directory, supporting enterprise-grade user management.
- **[REST API-Based MQTT 5.0 SCRAM Authentication](../access-control/authn/scram_restapi.md)**: Leverage a RESTful API for SCRAM-based authentication in compliance with MQTT 5.0 standards.
- **[Kerberos Authentication](../access-control/authn/kerberos.md)**: Integrate with Kerberos-based SSO systems for secure, centralized user authentication.
- **[Client-Info Authentication](../access-control/authn/cinfo.md)**: Allow flexible access control based on client metadata like IP, device ID, or username.

EMQX Enterprise 5.9.0 introduces a suite of advanced security features to safeguard your deployments. 

- **[Multi-Factor Authentication](../multi-factor-authn/multi-factor-authentication.md)**: Adds a layer of login security by requiring verification beyond just username and password.
- **[Account Lockout and Unlock](../dashboard/introduction.md#account-lockout-and-unlock)**: Automatically disables user accounts after multiple failed login attempts, with options for manual or timed unlocking.
- **[Password Expiration](../dashboard/introduction.md#password-expiration)**: Enforces password rotation policies to reduce long-term credential risk and comply with organizational security policies.

## OpenTelemetry Integration for Metrics, Logs, and Traces

EMQX now supports OpenTelemetry, making it easier to monitor and troubleshoot your MQTT systems.

**Key Features:**

- **Metrics**: Exports real-time metrics to OpenTelemetry Collector, then view them in tools like Prometheus and Grafana.
- **Logs**: Sends structured logs with rich context (like trace IDs) to your log system for easier debugging.
- **Tracing**: Enables distributed tracing of MQTT message flows across EMQX nodes. Useful for finding delays, routing issues, or node-specific performance bottlenecks.
- **End-to-End Tracing Mode**: Tracks full message paths and client actions. Filter by client ID, topic, or QoS. Control sampling and export rate to manage system load.

OpenTelemetry helps you get full visibility into EMQX performance and message flows using open, standard tools. Refer to [Integrate with OpenTelemetry](../observability/opentelemetry/opentelemetry.md) for details.

## NATS Protocol Gateway

EMQX 5.10.0 introduces a native NATS protocol gateway, enabling bi-directional messaging between NATS and MQTT. This feature allows NATS clients to connect directly to EMQX and exchange messages with MQTT clients using topic-to-subject mapping. 

**Key Features:**

- **Full NATS Protocol Support**: Handles core message types like PUB, SUB, PING, and request/reply.
- **MQTT Interoperability**: Converts NATS subjects to MQTT topics, supports wildcards, and shared subscriptions.
- **Flexible Deployment**: Enable via Dashboard, REST API, or config file.
- **Transport Support**: Works over TCP, TLS, WebSocket, and secure WebSocket (WSS).
- **Authentication**: Supports multiple backends, including database, HTTP, JWT, and LDAP.

With this gateway, EMQX bridges MQTT with modern cloud-native environments where NATS is used, expanding integration possibilities in hybrid messaging systems. Refer to the [NATS Protocol Gateway](../gateway/nats.md) for details.

## More Features

In addition to the highlights covered above, recent EMQX updates include many other new features and enhancements. For a complete list, see the [Release Notes](../changes/changes-ee-v5.md).
