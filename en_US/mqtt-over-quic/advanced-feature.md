# Advanced Features

As MQTT brokers evolve, EMQX stays ahead of the curve, introducing new features and capabilities to meet the complex demands of IoT applications. This chapter delves into the following powerful features: 

- [Message Queue](../message-queue/message-queue-concept.md) introduces a built-in message queuing mechanism that enables reliable message buffering, decoupling of producers and consumers, and improved resilience for scenarios involving burst traffic or temporarily offline clients.
- [MQTT Streams](../mqtt-stream/mqtt-stream-concept.md) provides a stream-processing capability for MQTT data, allowing you to persist, replay, and process MQTT messages as ordered streams for real-time analytics and event-driven applications.
- [MQTT over QUIC](./introduction.md) introduces this groundbreaking feature in EMQX and explains how to enable it in EMQX.
- [Cluster Linking](../cluster-linking/introduction.md) introduces the feature that connects multiple, separate clusters and facilitates client communication on geographically dispersed clusters.
- [MQTT-based File Transfer](../file-transfer/introduction.md) provides insights into transferring large files to EMQX using the MQTT protocol.
- [Multi-Protocol Gateway](../gateway/gateway.md) covers the design and usage of several commonly used gateways, including Stomp, MQTT-SN, CoAP, and LwM2M.
- [MQTT Client Attributes](../client-attributes/client-attributes.md) allows developers to define and set additional attributes for MQTT clients, enhancing access control, data integration, and MQTT extension functionalities while supporting flexible templating for personalized client configurations and simplified authentication processes.

These features expand the capabilities of EMQX, allowing you to leverage additional protocols and enhance the connectivity and interoperability of your MQTT-based applications.

