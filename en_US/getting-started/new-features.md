# What's New

This section lists the new features introduced in EMQX 5.0.

## Mria Cluster Architecture

EMQX 5.0 adopts a new [Mria cluster architecture](./mria-introduction.md). With this Mria architecture, one EMQX cluster can support up to [100 million concurrent MQTT connections](https://www.emqx.com/en/blog/reaching-100m-mqtt-connections-with-emqx-5-0), making it the world’s most scalable open-source MQTT broker.

<img src="./assets/100m-benckmark.png" alt="100m-benckmark" style="zoom:50%;" />

Besides this obvious scalability improvement, the Mria cluster architecture is also capable of reducing the risk of brain-splitting and its effects in large-scale deployments, to empower our clients with more stable and reliable IoT data access services.

To speedily get started, see [Create an EMQX Cluster](../deploy/cluster/create-cluster.md).

## MQTT over QUIC

EMQX 5.0 introduces QUIC support (MQTT over QUIC) as an experimental feature and designs a unique messaging mechanism and management approach. 

As the underlying transport protocol of the next-generation Internet protocol HTTP/3,  [QUIC](https://datatracker.ietf.org/doc/html/rfc9000) can provide connectivity for the modern mobile Internet with less connection overhead and message latency compared to TCP/TLS protocols. Therefore, EMQX attempted to replace the transport layer of MQTT with QUIC, which led to the MQTT over QUIC.

TO evaluate MQTT over QUIC and verify how it could improve network connectivity, you can continue to read [Use MQTT over QUIC ](../mqtt-over-quic/getting-started.md).



## Redesigned IoT data integration

Besides SQL, [jq](https://stedolan.github.io/jq/) is also supported with EMQX Rule Engine, so it is capable of handling more complex JSON data formats.

{% emqxce %}

Support sending data to WebHook or establishing bidirectional data bridging with external MQTT services.

{% endemqxce %}

{% emqxee %}

Leveraging the bidirectional data bridging feature, you can process and send your IoT data to over 40 cloud services and enterprise systems in real-time, or retrieve data from them and send it to designated MQTT topics after processing.

{% endemqxee %}

EMQX visualizes the data integration process with the Flows feature on Dashboard. Now you can easily check how the rules engine processes IoT data and how data flows to external data services or devices.

In future releases, EMQX also plans to support drag-and-drop orchestration of rules and data bridges with Flows. 

![flow-editor](./assets/flow-editor.png)

On different data bridging that EMQX supports and how to configure, see [Data Bridges](../data-integration/data-bridges.md).

## Flexible Authentication/Authorization

EMQX 5.0 has offered a built-in client authentication/authorization feature, users only need to do some simple configuration work before integrating with various data sources for user authentication and ensuring data security under various scenarios. 

**New features**

- Support using Dashboard for the authentication/authorization configuration on the cluster level;
- Support using Dashboard for configuration, commissioning and management;
- Support adjusting the running order of the authenticators and authorization checkers;
- Achieve complete observability with statistics on execution speed and number of times;
- Support authentication configuration on a listener-level, to provide more flexible access capabilities.

On how to run authentication/authorization configuration with EMQX Dashboard or configuration files, you can continue to read [Access Control](../access-control/overview.md).

## User-friendly EMQX Dashboard

In EMQX 5.0, we have redesigned the EMQX Dashboard with a new UI design style, enhancing the visual experience and supporting more powerful and user-friendly features. 

**New features**

- New UI/UX design: Great enhancement of real-time observability
- Optimized menu structure: Fast and direct access to contents
- Data monitoring and management: Important data at a glance
- Visualized access control: Out-of-the-box authentication/authorization management
- Powerful data integration capabilities: Using Flow Editor in a visualized way and supporting two-way data bridging
- Configuration updates during runtime: Hot update that takes effect immediately

## Cloud Native and EMQX Operator

Horizontal expansion and elastic clusters are features that a cloud-native application must support.

[EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator) lets you take full advantage of the Replicant node of EMQX 5.0. You can deploy a stateless EMQX node with Kubernetes Deployment and then build the EMQX cluster that supports large-scale MQTT connections and message throughput.

## New Gateway Framework

EMQX 5.0 presents a new extended gateway framework with reconstructed underlying architecture for multi-protocol access and a unified configuration format and management interface:

- **Unified statistic and monitoring indicators:** EMQX 5.0 provides the gateway/client-level statistic indicators, for example, number of bytes sent and received, messages, etc.
- **Independent connection and session management:** Different from EMQX 4.x, gateway clients are also managed under the MQTT client list, EMQX 5.0 has created an independent gateway page for each gateway, and one Client ID can be reused across gateways.
- **Independent client authentication**: Different from EMQX 4.x, where the gateway authentication is also managed under the MQTT client, EMQX 5.0 supports configuring a unique authentication mechanism for each gateway.
- **Easy to expand with clear specifications:** The framework provides a set of standard concepts and interfaces to make it easier to customize the gateways.

The new gateway framework further improves EMQX's usability by unifying access and managing multiple protocols. Now clients implementing third-party protocols can also leverage the advantages of EMQX, such as data integration, safe and reliable authentication/authorization, billion-level horizontal expansion capabilities, etc.

## **More feature updates**

**Simplified configuration**

The `emqx.conf` configuration file has been changed to a concise and readable HOCON format, and contains commonly used configuration items by default, to improve the readability and maintainability.

**Improved REST API**

Provide REST API compliant with the OpenAPI 3.0 specification, as well as clear and rich API documentation.

**Rapid troubleshooting**

Provide more diagnostic tools such as slow subscriptions and online tracing so users can quickly troubleshoot issues in production.

**Structured logs**

More user-friendly structured logs and JSON format is also supported. Error logs is flagged with 'msg' to facilitate locating the cause of the problem.

**Flexible expansion and customization**

Provide a new plugin architecture, with which users can compile, distribute, and install their extension plugins in the form of independent plugin packages to customize and extend the usage of EMQX.
