# Getting Started

Starting from download and installation, this document guides you through quickly getting an EMQX node up and running.

## Select EMQX Edition

{% emqxce %}
EMQX as a software is released in different editions.

- [Enterprise](https://www.emqx.com/en/try?product=enterprise)
- [Opensource](https://www.emqx.com/en/try?product=broker)

{% emqxce %}

EMQ as a company offers a fully managed MQTT cloud service [EMQX Cloud](https://www.emqx.com/en/try?product=cloud) which runs EMQX Enterprise edition.
You can customize the deployment to perfectly suit your business development plans and get started quickly.

:::: tabs type:card

{% endemqxce %}
::: tab EMQX Opensource Edition
The world's most scalable distributed MQTT broker with a high-performance real-time message processing engine, powering event streaming for IoT devices at a massive scale.

- Fully open-sourced under APL 2.0
- MQTT 3.1.0, MQTT 3.1.1 and MQTT 5.0 spec, supports MQTT-SN
- Highly available, masterless clustering
- High concurrency with low latency
- Extensible gateways and plugins
- Data integration by calling HTTP APIs

[Download](https://www.emqx.com/en/try?product=broker)
:::
{% endemqxce %}

::: tab EMQX Cloud
Connect your IoT devices to everything with reliable, real-time IoT data transport, processing, and integration. Accelerate your IoT application development without the burden of self-managing the infrastructure.

- Fully managed MQTT 5.0 service
- Powerful SQL-based data integrations
- Integration with various databases
- Highly available, fault-tolerant
- Run anywhere, pay as you go

[Get Started Free](https://www.emqx.com/en/try?product=cloud)
:::

::: tab EMQX Enterprise Edition
The world’s leading **Cloud-Native IoT Messaging Platform** with an all-in-one
distributed MQTT broker and SQL-based IoT rule engine. It combines high-performance with
reliable data transport, processing, and integration for business-critical IoT solutions.

- Support for multiple IoT protocols
- Powerful SQL-based data integrations
- Rich data persistence and bridging choices
- Management & monitoring center
- Global technical support team

[Try Free](https://www.emqx.com/en/try?product=enterprise)
:::

::::

## Install EMQX

### EMQX Cloud -- Service hosted by EMQ

EMQX Cloud is the first fully managed MQTT 5.0 public cloud service in the world.
With the support of EMQX Cloud you can create an EMQX cluster in the cloud and use
all the features of EMQX Enterprise.

This allows you to spend more time on business connections and less time on EMQX operation,
maintenance, and management.

- [Create and login EMQX cloud account](https://docs.emqx.com/en/cloud/latest/quick_start/introduction.html)
- [Create a free trial deployment](https://docs.emqx.com/en/cloud/latest/quick_start/create_free_trial.html)

### Running EMQX in containers

Learn more about the docker image on [Docker Hub](https://hub.docker.com/r/emqx/emqx).
Container deployment is the quickest way to start experimenting with EMQX.

Start Docker container:

```bash
docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx:latest
```

For more information about Docker installation and building a cluster quickly with Docker Compose,
please refer to [Running with Docker (including a simple docker-compose cluster)](./deploy/install.md#running-emqx-in-docker).

### Running EMQX in Kubernetes

<!-- TODO @wivwiv Update K8s link when EMQX Operator 5.0 document ready -->
For Kubernetes, EMQ offers [EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator).

EMQX Kubernetes Operator is an application orchestration tool based on Kubernetes native API,
which is used for automatic deployment and life cycle management of EMQX clusters.

You can refer to the [documentation](https://github.com/emqx/emqx-operator/blob/main/docs/en_US/getting-started/getting-started.md) to learn how to deploy EMQX using the Operator.

### Deploy EMQX with Terraform

<!-- TODO @wivwiv Update K8s link when EMQX Terraform 5.0 document ready -->
Deploy all infrastructure including EMQX Enterprise clusters on the mainstream public cloud with
one click through Terraform.

The fastest way to deploy EMQX Enterprise clusters on the public cloud.

- [Deploy on AWS](https://github.com/emqx/tf-aws)
- [Deploy on Alibaba Cloud](https://github.com/emqx/tf-alicloud)

For more information about Terraform installation and deployment, please refer to
[EMQX Terraform](https://www.emqx.com/en/emqx-terraform)

### Run in a VM or on bare metal

EMQX can be deployed directly to bare metal servers or virtual machines.
A minimum of 2 cores and 4GB RAM can run an EMQX installation.

EMQX has prebuilt packages downloadable for Linux (RedHat, CentOS, RockyLinux)
as well as Debian, Ubuntu, and MacOS.

- [RedHat, CentOS, RockyLinux, AmazonLinux  installation](./deploy/install.md#centos)
- [Ubuntu, Debian installation](./deploy/install.md#ubuntu-debian)
- [MacOS, Windows, Linux tar.gz installation](./deploy/install.md#tgz)

For other platforms which are not in the prebuilt list,
you may [build the executables from source code](./deploy/install.md#source-code-compilation-and-installation) or
simply [contact EMQ](https://www.emqx.com/en/contact) for support.

## Start EMQX

After the installation, you can start EMQX through the command of systemctl or emqx.
For more startup methods and precautions, keep reading below.

After EMQX is started successfully, you can visit [http://localhost:18083/](http://localhost:18083/)
(replace localhost with your actual IP address) through a browser to access [EMQX Dashboard](./dashboard/introduction.md)
management console for device connection and related indicator monitoring and management.

### Start EMQX in the background

Below command starts EMQX in background (daemon mode)

```bash
emqx start
```

After the startup is successful, you can use the `emqx ping` command to check the running status of the node.
If pong is returned, it means the running status is OK:

```bash
emqx ping
```

### Start EMQX using systemctl

```bash
sudo systemctl start emqx
```

Check if the service is working properly:

```bash
sudo systemctl status emqx
```

### Start EMQX using tar.gz package

Switch to the directory where EMQX was unzipped and execute the following command to start EMQX:

```bash
./bin/emqx start
```

In development mode, you can use the console command to start EMQX on the console and view
the startup and runtime logs printed to the console.

```bash
./bin/emqx console
```

## Quick verification using an MQTT client

After startup, you can quickly verify if EMQX is working with any MQTT client.
You can use the following client tools or client libraries to access EMQX

### Dashboard WebSocket tool

EMQX dashboard comes with a built-in WebSocket based MQTT client.

Visit the dashboard URL in a web browser and enter the page of **Diagnose -> WebSocket Client**,
where you can use the MQTT over WebSocket client to quickly access EMQX.

The WebSocket page provides you with a simple but effective WebSocket client tool,
which can be used for publishing, subscribing, and inspecting the messages.

<!-- TODO @wivwiv Update screenshot -->
### MQTTX desktop client tool

MQTTX is an elegant cross-platform MQTT 5.0 open source desktop client tool that
supports running on macOS, Linux, and Windows.

MQTTX has many features. It provides a concise graphical interface with intuitive operational functionality.
It supports MQTT/MQTT over WebSocket access with one-way/two-way SSL authentication,
and supports Payload format conversion, simulation of test data with a custom scripts,
automatic subscription of $SYS topic, viewing Traffic statistics, and more.

For download and use, please refer to the [MQTT X website](https://mqttx.app).

![emqx-mqttx](./assets/emqx-mqttx.jpeg)

## EMQX client library

For developers, we have compiled a list of popular MQTT clients for your reference.

### MQTT Client libraries

- [MQTT C client library](./development/c.md)
- [MQTT Java client library](./development/java.md)
- [MQTT Go client library](./development/go.md)
- [MQTT Erlang client library](./development/erlang.md)
- [MQTT JavaScript client library](./development/javascript.md)
- [MQTT Python client library](./development/python.md)

### Client example code

For MQTT client library example code, we try to cover as many mainstream programming languages and platforms as possible, see [MQTT-Client-Examples](https://github.com/emqx/MQTT-Client-Examples).

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

<!--
- [wechat-miniprogram](https://github.com/emqx/MQTT-Client-Examples/tree/master/mqtt-client-wechat-miniprogram)
-->

## Advanced operation

After completing the installation, startup, and access test,
you can now continue to read the following documents for a proper in-production setup.

### Access Control

Authentication is an important part of most applications,
and enabling authentication can effectively prevent malicious client connections.

Authorization rules, or Access Control List (ACL) can be configured to ensure only
legitimate clients are permitted to publish or subscribe to certain topics.

- [Authentication Introduction](./security/authn/authn.md): Select a backend to integrate
  with internal or external database, JWT or HTTP service as the authentication data source.
- [Authorization / ACL](./security/authz/authz.md): Select a backend to integrate with
  internal or external database, or HTTP service as the ACL data source.

### Data Integration

Data integration is the data processing and distribution component of EMQX based on the publish-subscribe model. Data integration can easily integrate IoT data with Kafka, RDS, various SQL / NoSQL / time-series databases, and enterprise systems such as Oracle and SAP.

- [Data Integration](./data-integration/introduction.md).
- [Rule](./data-integration/rules.md).
- [Data Bridge](./data-integration/data-bridges.md).

### Management Interfaces

Manage clusters via Web and CLI, REST API.

- [Dashboard](./dashboard/introduction.md)：Dashboard User Manual.
- [CLI](./admin/cli.md)
- [REST API](./admin/api.md): REST API documentation for the OpenAPI 3.0 specification.
- [Configuration Files](./admin/cfg.md)

### Operation, maintenance and deployment

For official usage guidelines and best practices please read the following guides.

- [System Tuning](./deploy/tune.md)
- [Production Deployment](./deploy/install.md)
- [Prometheus Monitoring and alert](./observability/prometheus.md)
- [Benchmark](./verif/benchmark.md)
