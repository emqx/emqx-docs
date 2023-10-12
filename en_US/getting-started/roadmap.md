---
title: EMQX Product Roadmap
description: Dive into the roadmap of EMQX and stay updated with its latest features and improvements for a seamless IoT platform experience.
---

# EMQX Product Roadmap

## 2023 Roadmap

{% emqxce %}

* Flow Designer for Data Integrations in EMQX Dashboard
* New Webhook
* [Session persistence with RocksDB](https://github.com/emqx/eip/blob/main/active/0023-rocksdb-message-persistence.md)
* New authorization mechanisms for clients
  * Control permissions on QoS and Retain flags
  * Support OAuth 2.0 authorization
* Operations (DevOps) and observability improvements
  * OpenTelemetry & Datadog Integration
  * End-to-end tracing of MQTT packets
  * Expose additional metrics

{% endemqxce %}

{% emqxee %}

* Dashboard improvements
  * Flow Designer for Data Integrations
  * Support LDAP authentication (MS Active Directory)
  * Support SAML (Okta)
* New Webhook
* [Session persistence with RocksDB](https://github.com/emqx/eip/blob/main/active/0023-rocksdb-message-persistence.md)
* Operations (DevOps) and observability improvements
  * OpenTelemetry & Datadog Integration
  * End-to-end tracing of MQTT packets
  * Expose additional metrics
* SparkplugB support in Rule Engine
* New authorization mechanisms for clients
  * Control permissions on QoS and Retain flags
  * Support OAuth 2.0 authorization
* New Data Integrations
  * AWS Kinesis
  * GCP PubSub
  * Azure EventHub
  * HStreamDB
  * SAP EventMesh
* Support for configuration and data migration from 4.4 to 5.1

{% endemqxee %}

## Future Versions

* Cross-datacenter Cluster Linking support
* Multi-cloud clusters
* Global Multi-Region Geo-Distributed clusters
* Elixir releases by default
* Use QUIC protocol for backplane traffic
* Support functions in other languages and external runtimes in Rule Engine (e.g. JS, Python)
