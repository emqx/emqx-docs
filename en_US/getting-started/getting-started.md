---
description: This chapter gives you a tour of how to download and install EMQX and how to test the connecting and messaging services with our built-in WebSocket tool.
---

{% emqxee %}
# Get Started with EMQX Enterprise
{% endemqxee %}

{% emqxce %}
# Get Started with EMQX
{% endemqxce %}


EMQX is the world’s most scalable and reliable MQTT messaging platform that can help you to connect, move and process your business data reliably in real-time. With this all-in-one MQTT platform, you can easily build your Internet of Things (IoT) applications with significant business impacts. 

This chapter gives you a tour of how to download and install EMQX and how to test the connecting and messaging services with our built-in WebSocket tool.

::: tip
Besides the deployment methods introduced in this quickstart guide, you are also welcome to try our [EMQX Cloud](https://www.emqx.com/en/cloud), a fully managed MQTT service for IoT. You only need to [register for an account](https://www.emqx.com/en/signup?continue=https://www.emqx.com/en/cloud) before you can start your MQTT services and connect your IoT devices to any cloud with zero need for infrastructure maintenance. 
:::

{% emqxce %}

## Select Your EMQX Edition

EMQX is available in Open Source and Enterprise editions, you may click the link below to download the edition as your business needs. 

- [Open source edition](https://www.emqx.com/en/try?product=broker)
- [Enterprise edition](https://www.emqx.com/en/try?product=enterprise)

:::: tabs type:card

::: tab EMQX Open Source edition
The world's most scalable distributed MQTT broker with a high-performance real-time message processing engine, powering event streaming for IoT devices at a massive scale.

- Fully open-sourced under APL 2.0
- Support MQTT 3.1.0, MQTT 3.1.1 and MQTT 5.0, support MQTT-SN
- Highly available, masterless clustering
- High concurrency with low latency
- Extensible gateways and plugins
- Data integration by calling HTTP APIs

[Download](https://www.emqx.com/en/try?product=broker)
:::

::: tab EMQX Enterprise edition
The world’s leading **Cloud-Native IoT Messaging Platform** with an all-in-one
distributed MQTT broker and SQL-based IoT rule engine. It combines high performance with reliable data transport, processing, and integration for business-critical IoT solutions.

- Support for multiple IoT protocols
- Powerful SQL-based data integrations
- Rich data persistence and bridging choices
- Management & monitoring center
- Global technical support team

[Try Free](https://www.emqx.com/en/try?product=enterprise)

:::

::::

{% endemqxce %}

## Install EMQX

EMQX can be run with [Docker](../deploy/install-docker.md), installed with [EMQX Kubernetes Operator](https://www.emqx.com/en/emqx-kubernetes-operator), or installed on a computer or virtual machine (VM) via a download package. If you choose to install EMQX with a download package, the following operating systems are currently supported: 

- RedHat
- CentOS
- RockyLinux
- AmazonLinux
- Ubuntu
- Debian
- macOS
- Linux

{% emqxce %}
For other platforms not listed above, you can try to [build and install with source code](../deploy/install-source.md) or simply [contact EMQ](https://www.emqx.com/en/contact) for support.
{% endemqxce %}

{% emqxee %}
For other platforms not listed above, you can try to simply [contact EMQ](https://www.emqx.com/en/contact) for support.

{% endemqxee %}

<!-- TODO @wivwiv Update K8s link when EMQX Terraform 5.0 document ready -->

In addition, you can also deploy EMQX with one click through [EMQX Terraform](https://www.emqx.com/en/emqx-terraform) on the cloud, for example, [Alibaba Cloud](https://github.com/emqx/tf-alicloud) and [AWS](https://github.com/emqx/tf-aws).

<!-- TODO @wivwiv Update Terraform link when EMQX Terraform 5.0 document ready -->

### Install EMQX using Docker/Installation Package

This quick start guide shows you the easiest ways to install and run EMQX, either through Docker or using the installation package.

:::: tabs type:card

::: tab Run EMQX with Docker

Container deployment is the quickest way to start exploring EMQX. The instructions below show how to run EMQX with Docker. 

1. To download and start the latest version of EMQX, enter the command below.

   Ensure [Docker](https://www.docker.com/) is installed and running before you execute this command.

   {% emqxce %}
   
   ```bash
   docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx:latest
   ```

   {% endemqxce %}

   {% emqxee %}
   
   ```bash
   docker run -d --name emqx -p 1883:1883 -p 8083:8083 -p 8084:8084 -p 8883:8883 -p 18083:18083 emqx/emqx-enterprise:latest
   ```

   {% endemqxee %}
   


2. Start your web browser and enter `http://localhost:18083/` ( `localhost` can be substituted with your IP address) in the address bar to access the  [EMQX Dashboard](../dashboard/introduction.md), from where you can connect to your clients or check the running status.

   Default user name and password:

   `admin`

   `public`

   <!--后续补上 dashboard的截图-->

:::

::: tab Install EMQX with zip/tar.gz files

{% emqxce %}

You can also install EMQX with zip/tar.gz files on a computer or VM, so you can easily adjust the configurations or run performance tuning. The instructions below use [macOS](https://www.emqx.io/downloads?os=macOS) (macOS12 amd64) as an example to illustrate the installation steps.

Note: Considering all the runtime dependencies, it is recommended to use zip/tar.gz files for testing and hot upgrades, and **NOT** recommended in a production environment. 

1. To download the zip file, enter:

   ```bash
   wget https://www.emqx.com/en/downloads/broker/@CE_VERSION@/emqx-@CE_VERSION@-macos12-amd64.zip
   ```

2. To install EMQX, enter:

   ```bash
   mkdir -p emqx && unzip emqx-@CE_VERSION@-macos12-amd64.zip -d emqx
   ```

3. To run EMQX, enter:

   ```bash
   ./emqx/bin/emqx start
   ```

4. Start your web browser and enter `http://localhost:18083/` ( `localhost` can be substituted with your IP address) in the address bar to access the [EMQX Dashboard](../dashboard/introduction.md), from where you can connect to your clients or check the running status.

   The default user name and password are `admin` & `public`. You will be prompted to change the default password once logged in. 

5. To stop EMQX, enter:

   ```bash
   ./emqx/bin/emqx stop
   ```

To uninstall EMQX after your testing, simply delete the EMQX folder.

{% endemqxce %}

{% emqxee %}

You can also install EMQX with zip/tar.gz files on a computer or VM, so you can easily adjust the configurations or run performance tuning. The instructions below use [macOS](https://www.emqx.io/downloads?os=macOS) (macOS12 amd64) as an example to illustrate the installation steps.

Note: Considering all the runtime dependencies, it is recommended to use zip/tar.gz files for testing and hot upgrades, and **NOT** recommended in a production environment.

1. Go to the [official site for EMQX](https://www.emqx.com/en/try?product=enterprise).

2. Select `@EE_VERSION@` for **Version** and `macOS 12` for **OS**, and click the **Download** button.

3. On the Downloads and Install page, select `zip` as the **Install Method** and select the proper **CPU Architecture** that matches your system. 

4. Download and install the package. You can also refer to the command instructions on the page.

5. To run EMQX, enter:

   ```bash
   ./emqx/bin/emqx foreground
   ```
   This will start EMQX in an interactive shell. Closing the shell will stop EMQX.
   Alternatively (but not recommended), you can also start EMQX in the background with below command:

   ```bash
   ./emqx/bin/emqx start
   ```

6. Start your web browser and enter `http://localhost:18083/` ( `localhost` can be substituted with your IP address) in the address bar to access the [EMQX Dashboard](../dashboard/introduction.md), from where you can connect to your clients or check the running status.

   The default user name and password are `admin` & `public`. You will be prompted to change the default password once logged in. 

7. To stop EMQX, enter:

   ```bash
   ./emqx/bin/emqx stop
   ```

To uninstall EMQX after your testing, simply delete the EMQX folder.

{% endemqxee %}

:::

::::


## Use MQTTX to Verify Connection

Now that you have successfully started EMQX, you can continue to test the connection and message services with MQTTX.

[MQTTX](https://mqttx.app) is an elegant cross-platform MQTT 5.0 desktop client, running on macOS, Linux, and Windows. By utilizing a chat style of user interface, MQTTX allows users to quickly create connections and save multiple clients, which facilitates users to quickly test the MQTT/MQTTS connection, as well as the subscription and publication of MQTT messages.

This section introduces how to verify the connection with [MQTTX Web](https://mqttx.app/web), the browser-based MQTT 5.0 WebSocket client tool, with zero need to download or install any application. 

::: tip Prerequisites
The broker address and the port information should be prepared before testing the connection:

- **EMQX address**: The IP address of your server, in general.
- **Port**: Click **Management** -> **Listeners** from the left navigation menu in the Dashboard to get the port number.
:::

### Create a Connection

1. Click [MQTTX Web](http://www.emqx.io/online-mqtt-client#/recent_connections) to visit the browser-based MQTTX.

2. Configure and establish the MQTT connection. Click the **+ New Connection** button to enter the configure page: 

   - **Name**: Input a connection name, for example, `MQTTX_Test`.

   - **Host**

     - Select the protocol type via the drop-down list, for example, select `ws://` if the WebSockets protocol is adopted. MQTTX Web only supports WebSockets protocol. To test the SSL/TLS connection, download [MQTTX Desktop](https://mqttx.app/downloads).
     - Enter the EMQX address, for example, `emqx@127.0.0.1`.

   - **Port**: for example, `8083` is for the WebSockets protocol.

     Keep the default setting for the other fields or set it as your business needs. For a detailed explanation of different fields, see [MQTT User Manual - Connect](https://mqttx.app/docs/get-started).


3. Click the **Connect** button at the top right corner of the page.

### Publish and Subscribe to Topics

After the connection is successfully established, you can continue to subscribe to different topics and publish messages.

1. Click **+ New Subscription**. MQTTX Web has already filled in some fields, according to the setting, you will subscribe to topic `testtopic/#`  with QoS level of 0. You can repeat this step to subscribe to different topics, and MQTTX Web will differentiate topics with colors.

2. In the right corner of the chat area at the bottom, click the send icon to test the message publishing/receiving. The messages successfully sent will appear in the chat window. 

<img src="./assets/MQTTXWeb-test.png" alt="MQTT X Web test" style="zoom: 25%;" />

If you want to continue the testing, such as one-way/two-way SSL authentication, and simulate test data with customized scripts, you can continue to explore with [MQTTX](https://mqttx.app). 

### View Metrics on Dashboard

On the Cluster Overview page in the EMQX Dashboard, you can check metrics such as **Connections**, **Topics**, **Subscriptions**, **Incoming Messages**, **Outgoing messages**, and **Dropped Messages**.

{% emqxee}

![emqx-dashboard_ee](./assets/emqx-dashboard_ee.png)

{% endemqxee %}

{% emqxce %}

![EMQX dashboard](./assets/emqx-dashboard.png)

{% endemqxce %}

## Next Steps

So far, you have completed the installation, startup, and access test of EMQX, you can continue to try out more advanced capabilities of EMQX, such as [authentication and authorization](../access-control/authn/authn.md) and integration with [Rule Engine](../data-integration/rules.md).

## Contact Us

If you have any questions on the use of EMQX or EMQ products, you are warmly welcome to [contact us](https://www.emqx.com/en/contact) for professional support.
