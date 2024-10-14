# EMQX Dashboard

EMQX provides a built-in Dashboard management console for users to monitor and manage EMQX clusters and configure the required features via web pages. The new Dashboard comes with a fresh new design and provides the most easy-to-use MQTT broker management UI. 

The new UI / UX design of EMQX Dashboard optimizes the display and content of key data and metrics, enhancing the visual experience while providing more comprehensive, powerful and easy-to-use built-in features, such as authentication and permission management for connection, subscription and publishing, support for data integration transformation using data bridging and with the rules engine, etc. Quick and easy access using the browser provides users with the convenience of using EMQX for more IoT business development.

![image](./assets/dashboard-preview.png)

## Key Features

This section introduces various features of EMQX that you can configure and manage through the Dashboard.

### [Monitoring](./monitoring.md)

View overall information of the running EMQX cluster, including connection count, subscribed topics, message delivery counts, inbound rates, and outbound rates. It also includes node lists, node information, and various system metric information. Additionally, you can view and manage client connections and subscription data.

### Access Control

Add and configure authentication and authorization mechanisms in EMQX visually.

### Integration

Utilize a powerful SQL-based rule engine and data integration, or the Flow editor's visual capabilities, for low-code data processing and integration. This helps in real-time extraction, filtering, enrichment, transformation, storage, and validation of MQTT data.

### Management

#### Cluster Settings

Supports online modification and update of MQTT, log, listeners, and other configuration items, which take effect immediately after successful updates.

#### Advanced MQTT

Manage and configure topic rewriting, automatic subscription, delayed publishing, and file transfer functionalities.

#### Extensions

Custom plugin integration to extend connection protocols through built-in gateway management and configuration. Also, use Hooks to modify or extend system functionality by intercepting function calls, message passing and event passing between modules.

### Problem Analysis and Diagnostics

In addition to debugging through online MQTT over WebSocket client connections and topic metrics, support is also available for diagnostics and issue discovery using features like slow subscriptions and log trace.

### System

Manage and configure user accounts, audit logs, API keys, license settings, and single sign-on functionalities.

## Main Features

### Monitor and manage data in EMQX clusters

It supports viewing the number of connections, subscription topics, messages sent and received, and incoming and outgoing rates of the running EMQX cluster, including node list and information and some metrics data, as well as viewing and managing some client connection and subscription data.

### Access control (authentication and authorization) management

Supports visualization to add and configure authentication and authorization mechanisms in EMQX.

### Data integration

Low-code data processing and integration using a powerful SQL-based Rule Engine and Data Bridge or the visualization capabilities of the Flow Editor to help extract, filter, enrich, transform and store MQTT data in real-time.

### Online config update

Supports online modification and update of configuration including MQTT, logs, listeners, etc., with immediate effect after successful update.

### Gateway/Extension management

Support for custom plug-in integration, Extend EMQX connectivity protocols through built-in gateway management and configuration or use Hooks to modify or extend system functionality by intercepting function calls, message passing and event passing between modules.

### More powerful diagnosis tools

In addition to debugging through online MQTT over WebSocket client connections and publishing subscriptions, we also support diagnosing and finding issues using things like slow subscriptions and online logs tracing and alarms.

## Launch Dashboard

EMQX Dashboard is a web application that listens to port `18083` by default. After installing EMQX successfully, you can access and use the EMQX Dashboard by opening <http://localhost:18083/> (replace localhost with the actual IP address if deployed on a non-local machine) through your browser.

::: tip
EMQX can still be used normally without Dashboard enabled, Dashboard just provides the option for users to use it visually.
:::

### First Login

For users who have installed EMQX for the first time, you can use the default username `admin` and default password `public` to login web page after opening the Dashboard in your browser.

After logging in for the first time, the system will automatically detect that you are logging in with the default username and password, and will force you to change the default password, which is good for the security of accessing Dashboard, note that the changed password cannot be the same as the original password, and it is not recommended to use `public` as the login password again.

### Reset password

You can reset your Dashboard login password via the `admins` command. For details, see [CLI - admins](../admin/cli.md#admins).

```bash
./bin/emqx ctl admins passwd <Username> <Password>
```

## Configure Dashboard

Dashboard listens to the HTTP by default, and the default port number is 18083. Users can enable HTTPS or change the listener port. For more information on how to configure and modify the Dashboard settings,  refer to the [EMQX Open Source Configuration Manual](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) and [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

