# Introduction to EMQ X Messaging Server

*EMQ X* (Erlang/Enterprise/Elastic MQTT Broker) is an open source IoT MQTT messaging server developed on the Erlang/OTP platform.

Erlang/OTP is an excellent soft-realtime, low-latency, and distributed language platform.

MQTT is a lightweight, PubSub IoT messaging protocol.

The design goal of EMQ X is to implementing highly reliable and to supporting carrying a large number of IoT terminal MQTT connections, and to support low-latency message routing between large numbers of IoT devices:

1. Steady hosting of massive MQTT client connections, with a single server node supporting 500,000 to 1 million connections.
2. Distributed node clusters, fast and low-latency message routing, and the routing that single cluster supports 10 million scales.
3. Extensions within the messaging server and support for customizing multiple authentication methods and efficient storage of messages in the back-end database.
4. Full IoT protocol support, MQTT, MQTT-SN, CoAP, LwM2M, WebSocket, or private protocol support.

**It is recommended that you read through the documents listed below before using them, other documents not listed can be viewed on- demand:**

## Get Started

  - [Install](getting-started/install.md)：Steps to download and install different types of installation packages on different operating systems.
  - [Enable EMQ X](getting-started/start.md)：Enable EMQ X and check the start-up status.
  - [Dashboard](getting-started/dashboard.md)：Manage EMQ X and online devices via Dashboard.




## Authentication

  - [Authentication Introduction](advanced/auth.md)：Select the built-in plugin, external database, JWT, or HTTP service as the authentication data source to verify the legality of the client's connection.

  - [Publish Subscription ACL](advanced/acl.md)：Select the built-in plugin, external database, JWT, or HTTP service as the ACL data source to verify the client's publish subscription authority.

    


## FAQs

[FAQs](faq/faq.md) regularly collects frequently asked questions and common errors encountered by EMQ X users, such as limitations on the number of Topics, differences between Open Source and Enterprise versions, charges for Enterprise services, etc., and how to store data in the Open Source version.


## Community Communication

 - [Resources](awesome/awesome.md): Community communication, including resources such as popular community tutorials and project showcases.

## HTTP API

The HTTP API is a frequently used function in IoT platform development and EMQ X operation and maintenance, enabling integration with external systems, such as querying and managing client information, proxy subscriptions, publishing messages, and creating rules.

  - [HTTP API](advanced/http-api.md)： Contains the HTTP API access point and access authentication method.
  - [Basic Information](advanced/http-api.md#endpoint-brokers)：Gets basic information such as EMQ X version and running status.
  - [Node](advanced/http-api.md#endpoint-nodes)：Get the information of EMQ X node.
  - [Client](advanced/http-api.md#endpoint-clients)：View the online client information, and support kicking out the client.
  - [Subscription Information](advanced/http-api.md#endpoint-subscriptions)：View the list of subscription topics and subscription relationships.
  - [Routing](advanced/http-api.md#endpoint-routes)：View the subscribed topics.
  - [Message Publishing](advanced/http-api.md#endpoint-publish)：Call EMQ X via HTTP to publish MQTT messages, a reliable way for the application to communicate with the client.
  - [Topic Subscription](advanced/http-api.md#endpoint-subscribe)：Dynamically manage client subscription lists without the need for clients to initiate subscriptions/unsubscriptions.
  - [Plugins](advanced/http-api.md#endpoint-plugins)：State management of plugins, start and stop operations.

More APIs can be found via the catalog on the left.

## Rule engine

The rule engine implements messaging data and enables the filtering, processing, and forwarding/storage of messages to external data sources including relational databases, message queues, web services, etc. through the rules engine.

  - [Rule Engine](rule/rule-engine.md)：The concept of the rule engine and the basic usage.
  - [Create a Rule](rule/rule-create.md)：How to create a rule.
  - [Usage Examples](rule/rule-example.md#发送数据到-web-服务)：Tutorials on the rule engine how to use various data sources.

## Data storage

This is a function unique to EMQ X Enterprise. The data storage records client online and offline status, subscription relationships, offline messages, message content, message acknowledgments sent upon message arrival, and other operations into various databases. The data storage contains both runtime data and message data, allowing data to be retained even after a service crash, or if a client goes offline abnormally.

  - [Data Storage](backend/backend.md)：Basic concepts and usage scenarios.
  - [Data Storage Configuration](backend/backend.md#redis-数据存储)：Data storage using different data sources.

## Message bridging

EMQ X Enterprise bridges and forwards MQTT messages to Kafka, RabbitMQ, Pulsar, RocketMQ, MQTT Broker, or other EMQ X nodes.

  - [MQTT Bridging](bridge/bridge.md#mqtt-桥接)：Enables cross-region, cross-cluster deployment.
  - [RPC Bridging](bridge/bridge.md#rpc-桥接)
  - [Kafka Bridging](bridge/bridge.md#kafka-桥接)
  - [RabbitMQ Bridging](bridge/bridge.md#rabbitmq-桥接)
  - [Pulsar Bridging](bridge/bridge.md#pulsar-桥接)
  - [RocketMQ Bridging](bridge/bridge.md#rocketmq-桥接)


## Operations and maintenance deployment

Contains official usage guidelines, best practices, and other information.

 - [Equipment Management](tutorial/device-management.md)
 - [System Tuning](tutorial/tune.md)
 - [Production Deployment](tutorial/deploy.md)
 - [Prometheus Monitoring and Alerting](tutorial/prometheus.md)
 - [Performance Testing](tutorial/benchmark.md)

## Introduction to the protocols

 - [MQTT Prtocol](development/protocol.md)
 - [MQTT-SN Protocol](development/protocol.md#mqtt-sn-协议)
 - [LwM2M Protocol](development/protocol.md#lwm2m-协议)
 - [Private TCP Protocol](development/protocol.md#私有-tcp-协议)
