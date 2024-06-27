# Concept FAQs

## Which products do we offer?

EMQX has [3 products](https://www.emqx.com/en/products/emqx) in total. The different products support different number of connections, features, services, etc.

- EMQX Broker: EMQX open source version. It supports the popular IoT protocols MQTT, CoAP and LwM2M.
- EMQX Enterprise: EMQX enterprise version. It is based on the open source version, and adds data persistence (support Redis, MySQL, MongoDB or PostgreSQL), data bridge to Kafka, LoRaWAN support, EMQX monitoring, Kubernetes deployment etc. It supports 1 million concurrent MQTT connections.
- EMQX Cloud: [EMQX Cloud](https://www.emqx.com/cloud) is an MQTT middleware for the IoT from EMQ. As the world's first fully managed MQTT 5.0 public cloud service, EMQX Cloud provides a one-stop O&M colocation and a unique isolated environment for MQTT services. In the era of Internet of Everything, EMQX Cloud can help you quickly build industry applications and easily realize the collection, transmission, computation and persistence of IoT data.

## What's a WebSocket? When to use a Websocket to connect to EMQX?

WebSocket is a full-duplex communication protocol with an API supported by modern web browsers. A user can use the WebSocket API to create a dual direction communication channel between a web browser and a server. Through a WebSocket, the server can push messages to the web browser. EMQX provides support for WebSocket. This means that users can publish to MQTT topics and subscribe to MQTT topics from browsers.

## How does the EMQX achieve high concurrency and high availability?

High concurrency and availability are design goals of EMQX. To achieve these goals, several technologies are applied:

- Making maximum use of the soft-realtime, high concurrent and fault-tolerant Erlang/OTP platform;
- Full asynchronous architecture;
- Layered design of connection, session, route and cluster;
- Separated messaging and control panel;

With the well design and implementation, a single EMQX node can handle 5 millions connections.

EMQX supports clustering. The EMQX performance can be scale-out with the increased number of nodes in cluster, and the MQTT service will not be interrupted when a single node is down.

## Can EMQX guarantee the original order when forwarding messages to subscribers?

EMQX ensures that messages with the same topic from the same client are forwarded in the order they were received, regardless of the QoS level. The message forwarding order remains consistent regardless of message loss or duplication, as per MQTT requirements.

However, EMQX does not guarantee the forwarding order of messages from different topics. These messages can be considered as entering separate channels. For example, if messages from topic A arrive at EMQX before messages from topic B, it is possible that messages from topic B will be forwarded earlier.