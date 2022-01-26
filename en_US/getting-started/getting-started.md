---
# 编写日期
date: 2022-01-25 09:53:20
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref:
---

# Getting Started

Starting from download and installation, this article will guide you to get started quickly on the main functionalities and key features of EMQ X.

## Select EMQ X Version

EMQ X as a software is released in two different editions: Community and Enterprise. EMQ as a company offers a fully managed MQTT cloud service [EMQ X Cloud](https://www.emqx.com/en/try?product=cloud) which runs EMQ X Enterprise edition. You can customize the deployment to suit your business development plans the best and get started quickly.

:::: tabs type:card

::: tab EMQ X Community Edition 
The world's most scalable distributed MQTT broker with a high-performance real-time message processing engine, powering event streaming for IoT devices at a massive scale.

- Fully open-sourced under APL 2.0
- MQTT 3.1.1 and MQTT 5.0 spec
- Highly available, masterless clustering
- High concurrency with low latency
- Extensible gateways and plugins

[Download](https://www.emqx.com/en/try?product=broker)
:::

::: tab EMQ X Cloud
Connecting your IoT devices to everything with reliable, real-time IoT data transport, processing, and integration. Accelerating your IoT application development without the burden of self-managing the infrastructure.

- Fully managed MQTT 5.0 service
- Powerful SQL-based rule engine
- Integration with various databases
- Highly available, fault-tolerant
- Run anywhere, pay as you go

[Get Started Free](https://www.emqx.com/en/try?product=cloud)
:::

::: tab EMQ X Enterprise Edition 
The world’s leading **Cloud-Native IoT Messaging Platform** with an all-in-one distributed MQTT broker and SQL-based IoT rule engine, powering high-performance, reliable data transport, processing, and integration for business-critical IoT solutions.

- Support for multiple IoT protocols
- Powerful SQL-based rule engine
- Data persistence and bridging
- Management & monitoring center
- Global technical support team

[Try Free](https://www.emqx.com/en/try?product=enterprise)
:::

::::

## Install EMQ X

### Deploy in EMQ X Cloud

EMQ X Cloud is the first fully managed MQTT 5.0 public cloud service in the world. With the support of EMQ X Cloud, you can create an EMQ X cluster on the cloud and use all the features of EMQ X Enterprise. This allows you to spend more time on business connections and less time for EMQ X operation, maintenance, and management.

- [Create and login EMQ X cloud account](https://docs.emqx.com/en/cloud/latest/quick_start/introduction.html)
- [Create a free trial deployment](https://docs.emqx.com/en/cloud/latest/quick_start/create_free_trial.html)

### Running EMQ X in containers

Learn more about the docker image on [Docker Hub](https://hub.docker.com/r/emqx/emqx). Container deployment is the quickest way to start experimenting with EMQ X

1. Get the Docker image

```bash
docker pull emqx/emqx:latest
```

1. Start Docker container

```bash
docker run -d --name emqx -p 1883:1883 -p 8081:8081 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx:latest
```

For more information about Docker installation and building a cluster quickly with Docker Compose, please refer to [Running with Docker (including a simple docker-compose cluster)](./install.md#install-emq-x-in-docker-contain-a-simple-docker-compose-cluster).

### Install EMQ X on Kubernetes

For users using Kubernetes, EMQ X provides [EMQ X Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator). This Operator can help you quickly deploy an EMQ X cluster with complete functions that can be used in a production environment in Kubernetes environment.

EMQ X Kubernetes Operator is an application orchestration tool developed based on Kubernetes native API, which is used for automatic deployment and lifecycle management of EMQ X clusters. You can refer to the [documentation](https://github.com/emqx/emqx-operator/blob/main/docs/user-guides/get-started.md) of the EMQ X Kubernetes Operator to learn how to deploy EMQ X using the Operator.

### Install EMQ X on Terraform

Deploy all infrastructure including EMQ X Enterprise clusters on the mainstream public cloud with one click through Terraform. The fastest way to deploy EMQ X Enterprise clusters on the public cloud：

- [Deploy on AWS](https://github.com/emqx/tf-aws)
- [Deploy on Alibaba Cloud](https://github.com/emqx/tf-alicloud)

For more information about Terraform installation and deployment, please refer to [EMQ X Terraform](https://www.emqx.com/en/emqx-terraform) .

### Run in a virtual machine or physical machine

EMQ X Enterprise can be directly deployed on bare metal servers or virtual machines. A minimum of 2 core and 4GB machine can run the EMQ X Enterprise. It can support CentOS/Debian/Ubuntu/MacOS operating systems.

- [Shell script one-click installation (Linux)](./install.md#one-click-installation-of-shell-script-linux)
- [CentOS installation](./install.md#centos)
- [Ubuntu, Debian installation](./install.md#ubuntu-debian)
- [MacOS, Windows, Linux ZIP installation](./install.md#zip-linux、maxos、windows)

If you need FreeBSD, other hardware platforms or other Linux distribution installation packages, you can refer to [source code compilation and installation](./install.md#source-code-compilation-and-installation) or [contact us](https://www.emqx.com/en/contact) for support.

## Start EMQ X

After the installation, you can start EMQ X through the command of systemctl or emqx. For more startup methods and precautions, please refer to [Starting EMQ X](./start.md).

After EMQ X is started successfully, you can visit [http://localhost:18083/](http://localhost:18083/) (replace localhost with your actual IP address) through a browser to access [EMQ X Dashboard](./dashboard.html) management console for device connection and related indicator monitoring and management.

### Start EMQ X in the background

```bash
emqx start
```

After the startup is successful, you can use the emqx ping command to check the running status of the node. If pong is returned, it means the running status is OK:

```bash
emqx ping
```

### Start EMQ X using systemctl

```bash
sudo systemctl start emqx
```

Check if the service is working properly:

```bash
sudo systemctl status emqx
```

### Start EMQ X using ZIP installation package

Switch to the EMQ X decompression directory and execute the following command to start EMQ X:

```bash
./bin/emqx start
```

In development mode, you can use the console command to start EMQ X on the console and view the startup and running log of EMQ X in real-time:

```bash
./bin/emqx console
```

## Quick experience of EMQ X

EMQ X supports standard MQTT protocol. After startup, you can access the MQTT client. You can use the following client tools or client libraries to access EMQ X for message communication to complete the testing and verification of certain scenarios or functions.

### Dashboard Websocket tool

Open Dashboard and enter the page of **Tools -> Websocket**, where you can use the MQTT over Websokcet client to quickly access EMQ X.

The Websocket page provides you with a simple but effective WebSocket client tool, which includes the functions of connection, subscription, and publishing, as well as checking the packet data sent and received by yourself.

![emqx-mqtt-websocket-tool-en](./assets/emqx-mqtt-websocket-tool-en.png)

### MQTT X desktop client tool

MQTT X is an elegant cross-platform MQTT 5.0 open source desktop client tool that supports running on macOS, Linux, and Windows.

MQTT X has many features, provides a concise graphical interface and operation logic, supports MQTT/MQTT over Websocket access and one-way/two-way SSL authentication, and supports Payload format conversion, simulation of test data with a custom script, automatic subscription of $SYS topic, viewing Traffic statistics and so on.

For download and use, please refer to the [MQTT X website](https://mqttx.app).

![emqx-mqttx](./assets/emqx-mqttx.jpeg)

## EMQ X client library

The following is the introduction and description of popular MQTT client libraries in each programming language, as well as the basic function code examples of connection (including TLS connection), publishing, subscription, and unsubscription of each library.

### Introduction to the client library

- [MQTT C client library](../development/c.md)
- [MQTT Java client library](../development/java.md)
- [MQTT Go client library](../development/go.md)
- [MQTT Erlang client library](../development/erlang.md)
- [MQTT JavaScript client library](../development/javascript.md)
- [MQTT Python client library](../development/python.md)
- [MQTT WeChat miniprogram access](../development/wechat-miniprogram.md)

### Client library project engineering code example

For MQTT client library access example project code, it covers dozens of mainstream programming languages and technologies, including [Android](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Android), [Csharp-MqttNet](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Csharp-MqttNet), [ESP32](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-ESP32), [ESP8266](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-ESP8266), [Electron](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Electron), [Flutter](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Flutter), [Go](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Go), [Java](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Java), [PHP](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-PHP), [Qt](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Qt), [SpringBoot](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-SpringBoot), [Vue.js](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-Vue.js), [swift](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-swift), [wechat-miniprogram](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-wechat-miniprogram). For detailed lists, please refer to [MQTT-Client-Examples](https://github.com/emqx/MQTT-Client-Examples).

## Advanced operation

After completing the installation, startup, and access test, you can continue to read the following operation documents for advanced operation configuration.

### Authentication

Authentication is an important part of most applications, and enabling authentication can effectively prevent illegal client connections. Pub/Sub ACLs can control the permissions for Pub/Sub operations of the client.

- [Authentication Introduction](../advanced/auth.md): select built-in plugin, external database, JWT or HTTP service as the authentication data source to verify whether the client connection is legal.
- [Pub/Sub ACL](../advanced/acl.md): Select built-in plugin, external database, or HTTP service as the ACL data source to verify the client's Pub/Sub permissions.

### Rule Engine

The built-in rule engine based on SQL can extract, filter, enrich, and convert IoT data between devices and business systems in real-time, and forward IoT data to Webhooks and other MQTT Brokers without coding. For the enterprise version, it can also be integrated with Kafka, various SQL / NoSQL / time-series databases and enterprise systems, such as SAP.

- [Rule Engine](../rule/rule-engine.md): The concept and basic usage of rule engine.
- [Create Rule](../rule/rule-create.md): How to create a rule.
- [Example](../rule/rule-example.md#send data to-web-service): Tutorial on using various data sources for rule engine.

### HTTP API

HTTP API is a frequently used function in IoT platform development and EMQ X operation and maintenance. HTTP API can realize integration with external systems, such as querying and managing client information, broker subscription, publishing messages and creating rules.

- [HTTP API](../advanced/http-api.md): include HTTP API access point and access authentication method.
- [Basic Information](../advanced/http-api.md#endpoint-brokers): Get basic information such as EMQ X version and running status.
- [Node](../advanced/http-api.md#endpoint-nodes): Get the information of EMQ X node.
- [Client](../advanced/http-api.md#endpoint-clients): View online client information and support kicking out the client.
- [Subscription Information](../advanced/http-api.md#endpoint-subscriptions): View the subscription topic list and subscription relationship.
- [Routes](../advanced/http-api.md#endpoint-routes): View subscribed topics.
- [Message Publishing](../advanced/http-api.md#endpoint-publish): Call EMQ X through HTTP to publish MQTT messages, with a reliable way for applications to communicate with clients.
- [Topic Subscription](../advanced/http-api.md#endpoint-subscribe): Dynamically manage the client subscription list, without the need for the client to actively initiate subscription/unsubscription.
- [Plugins](../advanced/http-api.md#endpoint-plugins): Status management of plugins with start and stop operations.

For access authentication methods and more APIs, please refer to [HTTP API](../advanced/http-api.md#数据遥测).

### Operation, maintenance and deployment

It contains official usage guidelines and best practices.

- [Device Management](../tutorial/device-management.md)
- [System Tuning](../tutorial/tune.md)
- [Production Deployment](../tutorial/deploy.md)
- [Prometheus Monitoring and alert](../tutorial/prometheus.md)
- [Benchmark](../tutorial/benchmark.md)

### FAQ

In [FAQ (Frequently Asked Questions)](../faq/faq.md), we regularly collect and sort out the common problems and frequently encountered errors of EMQ X users, such as topic number limit, external resource connection errors, startup failure reasons, etc.

In addition, you can visit [EMQ Q&A Community](https://askemq.com/) to put forward and answer questions about the use of EMQ X and EMQ-related products, and exchange experience of IoT-related technologies with EMQ X users.
