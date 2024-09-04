# Release Notes for EMQX Enterprise 5.0

*Release Date: 2023-02-03*

EMQX Enterprise 5.0 is a completely new release. In this version, we have implemented a new cluster architecture and refined the main features, as well as introduced many new features.

## New Core + Replica clustering architecture

EMQX Enterprise 5.0 adopts a brand-new Core + Replica clustering architecture, offering better scalability and reliability.

- A single cluster can now support up to 23 nodes and 100+ million MQTT connections, a [10x increase](https://www.emqx.com/en/blog/how-emqx-5-0-achieves-100-million-mqtt-connections) compared with EMQX Enterprise 4.x.
- The stateless nature of Replicant nodes ensures a stable cluster performance during dynamic scaling.
- Reduce the risk of split-brain under large-scale deployment and minimize the impact of split-brain on business.

The [EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator) has been adapted for this new clustering architecture so that you can deploy a scalable and stateless MQTT service with EMQX effortlessly.

## Support MQTT over QUIC

By adopting QUIC as a transport layer, EMQX Enterprise 5.0 now supports the MQTT over QUIC transmission protocol.

MQTT over QUIC (Quick UDP Internet Connections) is a new transport protocol that aims to provide low-latency, reliable, and secure data communication in IoT (Internet of Things) networks. As demonstrated from some performance tests, MQTT over QUIC can overcome some of the challenges of traditional networking protocols, such as slow and unreliable data transmission, high latency, and security vulnerabilities. Therefore, it is said that MQTT over QUIC has the potential to revolutionize the way how data is collected in the Internet of Vehicles (IoV) and mobile data acquisition scenarios.

With EMQX Enterprise 5.0, users can now create an MQTT over QUIC listener and use the EMQ SDK to connect to IoT devices. EMQ is submitting a draft to the MQTT protocol as a member of OASIS.

For more information, please refer to [MQTT over QUIC: Next-Generation IoT Standard Protocol](https://www.emqx.com/en/blog/getting-started-with-mqtt-over-quic-from-scratch).

## Visualized Rules Orchestration and Bidirectional Data Integration

EMQX Enterprise 5.0 offers real-time processing capability for IoT data and supports integration with third-party data systems in a more flexible and low-code way.

### Visualized Orchestration Rules with Flows Editor

EMQX Enterprise 5.0 has added a visualized Flows page in EMQX Dashboard to facilitate the management of rules. In the Flows editor, users can now easily view and monitor the data filtering, processing, and bridging flow. With this Flow editor to visualize the connection between the IoT hardware and data flow, developers can now focus on works with business significance.

EMQX plans to support rules orchestration and data bridge creation by drag and drop in future releases.

### More Flexible Bidirectional Data Integration

Besides bridging device data to external systems, EMQX Enterprise 5.0 also supports bridging data from external data systems to specific clients after rule processing, for example, other MQTT services and Kafka.

Bidirectional data integration is suitable for sending messages from the cloud to the device, with our support for real-time processing and delivering under large-scale messages, EMQX 5.0 offers more possibilities for IoT service application scenarios.

### Disk-Based Buffer Queue

EMQX Enterprise 5.0 also provides a buffer queue feature to better support our data bridging services. With this buffer queue feature, messages generated under abnormal connections can be cached for the moment and continue to be sent after the connection is resumed.

This buffer queue feature helps to ensure excellent reliability for data integration and greatly improves business availability.

### Supported Data Integrations

EMQX Enterprise 5.0 currently supports integration with the following data systems:

- Webhook
- MQTT
- Kafka
- InfluxDB
- MySQL
- Redis
- GCP Pub/Sub
- MongoDB

We plan to add support to more data systems, please stay tuned.

## Improved Security Management

### More Flexible Access Control

EMQX Enterprise 5.0 provides authentication options such as password-based authentication, LDAP, JWT, PSK, and X.509 certificates. It also provides authorization checking for message publishing and subscriptions.

Besides the configuration files, users can also configure their access control with EMQX Dashboard, a more flexible and user-friendly method. With this Dashboard configuration option, you can enable access control for EMQX clusters without rebooting.

EMQX also offers statistical metrics at both cluster and node levels to help our users better monitor access control running status, including:

- Allow: Number of authentication/authorization passed
- Deny: Number of authentication/authorization failed
- No match: Number of client authentication/authorization data not found
- Rate: Rate of request

### Overload Protection with Limiter

EMQX introduces the overload protection mechanism and a new Limiter feature. This Limiter delivers a more accurate and layered rate control and ensures that the system operates under the expected workload, because it supports limiting client behavior at the client, listener, or node levels.

The combination of these 2 features prevents the clients from becoming too busy or receiving excessive request traffic and ensures stable system operation.

## User-Friendly EMQX Dashboard with Better Bbservability

In EMQX Enterprise 5.0, we have redesigned the EMQX Dashboard with a new UI design style, enhancing the visual experience and supporting more powerful and user-friendly features. Users can manage client connections, authenticate/authorize various subscribe/publish requests, and integrate with different data systems via data bridges and rule engine with our brand-new EMQX Dashboard.

**The main improvements are as follows：**

- Access control management
- Introduce the Flows editor to visualize the data integration
- More powerful hot configuration
- More diagnostic tools, such as slow subscriptions and log trace
- Powerful data managing capability: users can manage retained messages or postpone the publishing schedules with Dashboard

## More Flexible Extensions

With EMQX Enterprise 5.0, users can now compile, distribute, and install extension plugins with standalone plugin packages. You can upload these packages via the Dashboard to finish the configuration with no need to reboot the EMQX cluster.

A standard plugin will come with complete documentation and a website URL, so users can easily follow the instructions to use the plugins and communicate with the developers.

### Easy-to-Use ExHook/gRPC

Users can create multiple ExHooks at the same time. With the relevant metrics, users can view the detailed usage statics and hooks (and their arguments) registered under each Exhook, so they can better understand the load of the ExHook extensions.

### More "Native" Multi-Protocol Connectivity

The gateway is re-implemented in a unified design layer. EMQX Enterprise 5.0 provides unique client management pages and security authentication configurations for each protocol feature, so users can manage the access in a more protocol-native manner.

As each gateway can be configured with its authentication methods, the authentication credentials of different gateway devices can now be isolated from each other to meet advanced security requirements.
