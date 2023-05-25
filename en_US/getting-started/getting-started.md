# Get Started

Starting from download and installation, this document is to guide you to quickly get an EMQX node up and running.

{% emqxce %}

## Choose EMQX Software Edition or Service

EMQX is released in different editions.

{% emqxce %}

::: tab EMQX type:card
The world's most scalable distributed MQTT broker with a high-performance real-time message processing engine, powering event streaming for IoT devices at a massive scale.

- Fully open-sourced under APL 2.0
- MQTT 3.1.1 and MQTT 5.0 spec
- Highly available, masterless clustering
- High concurrency with low latency
- Extensible gateways and plugins
- Data integration by calling HTTP APIs

[Download](https://www.emqx.com/en/try?product=broker)
:::

::: tab EMQX Enterprise
The world’s leading **Cloud-Native IoT Messaging Platform** with an all-in-one
distributed MQTT broker and SQL-based IoT rule engine, powering high-performance,
reliable data transport, processing, and integration for business-critical IoT solutions.

- Support for multiple IoT protocols
- Powerful SQL-based rule engine
- Rich data persistence and bridging choices
- Management & monitoring center
- Global technical support team

[Try Free](https://www.emqx.com/en/try?product=enterprise)
:::

::::

You can customize the deployment to suit your business development plans the best and get started quickly.

{% endemqxce %}

{% emqxee %}

## Download EMQX

EMQX stands as the world's premier Cloud-Native IoT Messaging Platform. It incorporates a comprehensive distributed MQTT broker and a SQL-based IoT rule engine, all in one package. This powerful combination enables EMQX to facilitate robust, high-performance data transportation, processing, and integration, thereby driving business-critical IoT solutions.

- Support for multiple IoT protocols
- Powerful SQL-based rule engine
- Rich data persistence and bridging choices
- Management & monitoring center
- Global technical support team

[Try Free](https://www.emqx.com/en/try?product=enterprise)

{% endemqxee %}

## Install EMQX

### Deplot EMQX with Docker

{% emqxce %}

Learn more about the docker image on [Docker Hub](https://hub.docker.com/r/emqx/emqx).
Container deployment is the quickest way to start experimenting with EMQX

1. Get the Docker image

```bash
docker pull emqx/emqx:latest
```

2. Start Docker container

```bash
docker run -d --name emqx -p 1883:1883 -p 8081:8081 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx:latest
```

For more information about Docker installation and building a cluster quickly with Docker Compose,
please refer to [Running with Docker (including a simple docker-compose cluster)](./install.md#install-emq-x-in-docker-contain-a-simple-docker-compose-cluster).

{% endemqxce %}

<!-- enterprise -->
{% emqxee %}

Learn more about the docker image on [Docker Hub](https://hub.docker.com/r/emqx/emqx-ee).
Container deployment is the quickest way to start experimenting with EMQX

1. Get the Docker image

```bash
docker pull emqx/emqx-ee:latest
```

1. Start Docker container

```bash
docker run -d --name emqx-ee -p 1883:1883 -p 8081:8081 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-ee:latest
```

{% endemqxee %}

### Deploy EMQX with Kubernetes

For Kubernetes, EMQ offers [EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator).

EMQX Kubernetes Operator is an application orchestration tool based on Kubernetes native API,
which is used for the automatic deployment and lifecycle management of EMQX clusters.
You can refer to the [EMQX Operator Documentation](https://docs.emqx.com/en/emqx-operator/latest/getting-started/getting-started.html)
to learn how to deploy EMQX using the Operator.

### Deploy EMQX with Terraform

Deploy all infrastructure including EMQX Enterprise clusters on the mainstream public cloud with
one click through Terraform.

The fastest way to deploy EMQX Enterprise clusters on the public cloud

- [Deploy on AWS](https://github.com/emqx/tf-aws)

For more information about Terraform installation and deployment, please refer to
[EMQX Terraform](https://www.emqx.com/en/emqx-terraform)

### Run in a VM or on Bare Metal

EMQX can be deployed directly to bare metal servers or virtual machines.
A minimum of 2 cores and 4GB RAM can run the EMQX.

EMQX has prebuilt packages downloadable for Linux (RedHat, CentOS, Rockylinux) as well as Debian, Ubuntu, and MacOS.

{% emqxce %}

- [CentOS installation](./install.md#centos)
- [Ubuntu, Debian installation](./install.md#ubuntu-debian)
- [MacOS, Windows, Linux ZIP installation](./install.md#zip-linux-macos-windows)

For other platforms which are not in the prebuilt list,
you may [build the runnable from source code](./install.md#source-code-compilation-and-installation).

{% endemqxce %}

{% emqxee %}

For other platforms which are not in the prebuilt list, you may simply [contact EMQ](https://www.emqx.com/en/contact) for support.

{% endemqxee %}

{% emqxce %}
For other platforms which are not in the prebuilt list,
you may [build the runnables from source code](./install.md#source-code-compilation-and-installation) or
simply [contact EMQ](https://www.emqx.com/en/contact) for support.
{% endemqxce %}

## Start EMQX

After the installation, you can start EMQX through the command of systemctl or EMQX.

 {% emqxce %}
After EMQX is started successfully, you can visit [http://localhost:18083/](http://localhost:18083/) (replace localhost with your actual IP address) through a browser to access [EMQX Dashboard](./dashboard.md) for device connection and related indicator monitoring and management.

{% endemqxce %}

{% emqxee %}
After EMQX is started successfully, you can visit [http://localhost:18083/](http://localhost:18083/) (replace localhost with your actual IP address) through a browser to access [EMQX Dashboard](./dashboard-ee.md) for device connection and related indicator monitoring and management.

{% endemqxee %}

To start EMQX directly, run:

```bash
emqx start
```

After the startup is successful, you can use the `emqx ping` command to check the running status of the node.
If pong is returned, it means the running status is OK:

```bash
emqx ping
```

To start EMQX using systemctl, run:

```bash
sudo systemctl start emqx
```

Check if the service is working properly:

```bash
sudo systemctl status emqx
```

Start EMQX when installed with ZIP installation package

Switch to the EMQX decompression directory and execute the following command to start EMQX:

```bash
./bin/emqx start
```

In development mode, you can use the console command to start EMQX on the console and view
the startup and runtime logs printed to the console.

```bash
./bin/emqx console
```

## Quick Verification Using an MQTT client

After startup, you can quickly verify if EMQX is working by any MQTT client.
You can use the following client tools or client libraries to access EMQX

### Dashboard WebSocket Tool

EMQX dashboard comes with a built-in, WebSocket-based MQTT client.

Open Dashboard and enter the page of **Tools -> WebSocket**, where you can use the MQTT over WebSocket client to quickly access EMQX.

The WebSocket page provides you with a simple but effective WebSocket client tool, which can be used for publishing, subscribing, and inspecting messages.

![emqx-mqtt-websocket-tool-en](./assets/emqx-mqtt-websocket-tool-en.png)

### MQTTX desktop client tool

MQTTX is an elegant cross-platform MQTT 5.0 open source desktop client tool that supports running on macOS, Linux, and Windows.

MQTTX has many features, provides a concise graphical interface and operation logic, supports MQTT/MQTT over WebSocket access and one-way/two-way SSL authentication, and supports Payload format conversion, simulation of test data with a custom script, automatic subscription of $SYS topic, viewing Traffic statistics and so on.

For download and use, please refer to the [MQTTX website](https://mqttx.app).

![emqx-mqttx](./assets/emqx-mqttx.jpeg)

## EMQX Client Library

For developers, we have compiled a list of popular MQTT clients for your reference.

### MQTT Client Libraries

- [MQTT C client library](../development/c.md)
- [MQTT Java client library](../development/java.md)
- [MQTT Go client library](../development/go.md)
- [MQTT Erlang client library](../development/erlang.md)
- [MQTT JavaScript client library](../development/javascript.md)
- [MQTT Python client library](../development/python.md)

### Client Example Code

For MQTT client library example code, we try to cover as many mainstream programming languages
and platforms as possible, including

- [Android](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Android)
- [Csharp-MqttNet](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Csharp-MqttNet)
- [ESP32](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-ESP32)
- [ESP8266](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-ESP8266)
- [Electron](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Electron)
- [Flutter](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Flutter)
- [Go](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Go)
- [Java](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Java)
- [PHP](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-PHP)
- [Qt](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Qt)
- [SpringBoot](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-SpringBoot)
- [Vue.js](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Vue.js)
- [swift](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-swift)
- [wechat-miniprogram](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-wechat-miniprogram)

For a more detailed list, please refer to [MQTT-Client-Examples](https://github.com/emqx/MQTT-Client-Examples)

## Advanced Operation

After completing the installation, startup, and access test,
you can now continue to read the following documents for a proper in-production setup.

### Authentication

Authentication is an important part of most applications,
and enabling authentication can effectively fence off malicious client connections.

Authorization rules, or Access Control List (ACL) can be configured to ensure only
legit clients are permitted to publish or subscribe to certain topics.

- [Authentication](../advanced/auth.md): Select a built-in plugin, to integrate with an internal or external database, JWT, or HTTP service as the authentication data source.
- [Authorization / ACL](../advanced/acl.md): Select a built-in plugin, to integrate with an internal or external database, or HTTP service as the ACL data source.

### Rule Engine

The built-in SQL based rule-engine can extract, filter, enrich, and convert messages between devices and data platforms in real-time, and forward the data to Webhooks or other MQTT Brokers without having to implement MQTT clients to do the job.

EMQX enterprise edition can also integrate with Kafka, various SQL / NoSQL / time-series databases, and enterprise systems, such as SAP.

- [Rule Engine](../rule/rule-engine.md): The concept and basic usage of the rule engine.
- [Create Rule](../rule/rule-create.md): How to create a rule.
- [Example](../rule/rule-example.md): Tutorial on using various data sources for a rule engine.

### HTTP API

HTTP API is a frequently used function in IoT platform development and EMQX operation and maintenance. HTTP API can realize integration with external systems, such as querying and managing client information, broker subscription, publishing messages and creating rules.

- [HTTP API](../advanced/http-api.md): include HTTP API access point and access authentication method.
- [Basic Information](../advanced/http-api.md#endpoint-brokers): Get basic information such as EMQX version and running status.
- [Node](../advanced/http-api.md#endpoint-nodes): Get the information of EMQX node.
- [Client](../advanced/http-api.md#endpoint-clients): View online client information and support kicking out the client.
- [Subscription Information](../advanced/http-api.md#endpoint-subscriptions): View the subscription topic list and subscription relationship.
- [Routes](../advanced/http-api.md#endpoint-routes): View subscribed topics.
- [Message Publishing](../advanced/http-api.md#endpoint-publish): Call EMQX through HTTP to publish MQTT messages, with a reliable way for applications to communicate with clients.
- [Topic Subscription](../advanced/http-api.md#endpoint-subscribe): Dynamically manage the client subscription list, without the need for the client to actively initiate
  subscription/unsubscription.
- [Plugins](../advanced/http-api.md#endpoint-plugins): Status management of plugins with start and stop operations.

### Operation and Maintenance

It contains official usage guidelines and best practices.

- [System Tuning](../tutorial/tune.md)
- [Load balancing](../tutorial/deploy.md)
- [Prometheus Monitoring and alert](../tutorial/prometheus.md)
- [Performance Test](../tutorial/benchmark.md)

### FAQ

In [FAQ (Frequently Asked Questions)](../faq/faq.md), we regularly collect and sort out the common problems and frequently encountered errors of EMQX users, such as topic number limit, external resource connection errors, startup failure reasons, etc.

In addition, you can visit [EMQ Q&A Community](https://askemq.com/) to put forward and answer questions about the use of EMQX and EMQ-related products, and exchange experience of IoT-related technologies with EMQX users.
