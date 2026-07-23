---
description: This section lists the new features introduced in EMQX 5.2 and later.
---

# New Features

This page highlights major new features supported in the current release. Note that it does not list every feature provided by EMQX.

## Cluster Linking

Cluster Linking enables seamless, secure, and efficient message sharing between geographically distributed EMQX clusters. Unlike traditional MQTT bridges, which forward all messages and often require topic prefixes to prevent feedback loops, Cluster Linking transfers only relevant messages based on active subscriptions, minimizing bandwidth use, reducing latency, and increasing scalability.

The configuration and management of Cluster Linking are designed to be simple and flexible. You can create, modify, and monitor cluster links directly from the EMQX Dashboard, configuration file, or through REST APIs. EMQX also provides visual status indicators and link statistics for real-time visibility.

To get started with the Cluster Linking, see [Quick Start with Cluster Linking](../develop/cluster-linking/quick-start.md).

![cluster_linking_feature](./assets/cluster_linking_feature.png)

## Schema Validation

[Schema Validation](../develop/data-integration/schema-validation.md) ensures that only messages conforming to predefined formats are processed or delivered. EMQX supports JSON Schema, Protobuf, Avro, and rule engine SQL syntax for validation. Based on the outcome, users can configure actions such as dropping messages, disconnecting clients, or triggering rule engine events for failed validations.

## Message Transformation

[Message Transformation](../develop/data-integration/message-transformation.md) allows users to define transformation pipelines that decode, modify, and re-encode messages before delivery or further processing. The system supports nested transformations, multiple encoders/decoders, and dynamic value assignments using [Variform expressions](../guides/configuration/configuration.md#variform-expressions).

## Expanded Support in Data Integration

Recent versions of EMQX have significantly enhanced the data integration capabilities. New data integrations supported in recent versions include, but are not limited to, the following:

- **[Snowflake](../develop/data-integration/snowflake.md)**: Writes the processed data to the Snowflake Stage and loads it into a Snowflake table. Safely store IoT data in Snowflake for long-term archival and leverage Snowflake's data warehousing and analytics capabilities to perform real-time or batch analysis.
- **[Azure Blob Storage](../develop/data-integration/azure-blob-storage.md)**: Seamlessly integrates EMQX with Microsoft Azure’s scalable object storage service. Ideal for archiving massive volumes of IoT telemetry and event data, it supports storing both structured and unstructured data in a durable, cost-effective way, similar to AWS S3.
- **[Datalayers](../develop/data-integration/data-bridge-datalayers.md)**: Datalayers is a distributed edge-cloud database platform designed for Industrial IoT, Internet of Vehicles (IoV), and energy systems. Through the integration with EMQX, users can ingest real-time data into Datalayers for time-series storage, key-value caching, and perform analytics.

## Enhanced Security

In recent versions, EMQX supports more authentication and authorization methods, providing more flexible and fine-grained access control capabilities. Newly supported features include:

- **[LDAP Integration](../guides/access-control/authn/ldap.md)**: Authenticate users against an external LDAP directory, supporting enterprise-grade user management.
- **[REST API-Based MQTT 5.0 SCRAM Authentication](../guides/access-control/authn/scram_restapi.md)**: Leverage a RESTful API for SCRAM-based authentication in compliance with MQTT 5.0 standards.
- **[Kerberos Authentication](../guides/access-control/authn/kerberos.md)**: Integrate with Kerberos-based SSO systems for secure, centralized user authentication.
- **[Client-Info Authentication](../guides/access-control/authn/cinfo.md)**: Allow flexible access control based on client metadata like IP, device ID, or username.

## OpenTelemetry Integration for Metrics, Logs, and Traces

EMQX now supports OpenTelemetry, making it easier to monitor and troubleshoot your MQTT systems.

**Key Features:**

- **Metrics**: Exports real-time metrics to OpenTelemetry Collector, then view them in tools like Prometheus and Grafana.
- **Logs**: Sends structured logs with rich context (like trace IDs) to your log system for easier debugging.
- **Tracing**: Enables distributed tracing of MQTT message flows across EMQX nodes. Useful for finding delays, routing issues, or node-specific performance bottlenecks.
- **End-to-End Tracing Mode**: Tracks full message paths and client actions. Filter by client ID, topic, or QoS. Control sampling and export rate to manage system load.

OpenTelemetry helps you get full visibility into EMQX performance and message flows using open, standard tools. Refer to [Integrate with OpenTelemetry](../guides/observability/opentelemetry/opentelemetry.md) for details.

## New Protocol Gateways

EMQX introduced several new protocol gateways to support industry-specific messaging standards, enabling direct integration with vertical systems in transportation, energy, and electric mobility.

- **[OCPP Gateway](../develop/gateway/ocpp.md)**: Provides native support for the Open Charge Point Protocol (OCPP) 1.6, allowing EMQX to connect directly with EV charging stations and convert OCPP messages into MQTT for unified communication.
- **[JT/T 808 Gateway](../develop/gateway/jt808.md)** Implements the JT/T 808 protocol for vehicle telematics, enabling EMQX to receive binary messages from GPS terminals and onboard units and forward them as hex-encoded payloads in MQTT messages for downstream decoding and analysis.
- **[GB/T 32960 Gateway](../develop/gateway/gbt32960.md)**: Enables EMQX to receive, decode, and forward structured diagnostic and telemetry data from new energy vehicles via MQTT, based on the GB/T 32960 protocol.

These gateways make it easier to bring industry-standard protocols into an MQTT-based data platform, supporting scenarios like smart charging, fleet monitoring, and EV data reporting.

## Optimized Dashboard Experience

The EMQX Dashboard has delivered a more intuitive and powerful interface for managing your MQTT broker since version 5.8. Here are some major enhancements:

**Usability Improvements**

- Added pagination, searching, and status filtering to Action and Source pages, making it simpler to manage rules and integrations at scale.
- Included a one-click cluster metrics reset, speeding up diagnostics and observation of cluster changes.

**Metrics Display Enhancements**

- Optimized the `/api/v5/monitor` endpoint with concurrent RPC calls to fetch cluster-wide metrics, eliminating timeouts in large deployments.
- Added key metrics like message rates directly on the homepage, reducing navigation for critical insights.

**Monitoring Tools**

Introduced a simplified webhook setup for alarm events, making it easier to automate monitoring and stay proactive. For more information, see [Integrate Webhook to Send Alarm Events](../guides/observability/alarms.md#integrate-webhook-to-send-alarm-events).

## More Features

In addition to the highlights covered above, recent EMQX updates include many other new features and enhancements. For a complete list, see the [Release Notes](../release-notes/changes-ee-v5.md).
