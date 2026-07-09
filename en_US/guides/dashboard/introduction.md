# EMQX Dashboard

EMQX provides a built-in Dashboard management console for users to monitor and manage EMQX clusters and configure the required features via web pages. The new Dashboard comes with a fresh new design and provides the easy-to-use MQTT broker management UI.

The new UI / UX design of EMQX Dashboard optimizes the display and content of key data and metrics, enhancing the visual experience while providing more comprehensive, powerful and easy-to-use built-in features, such as authentication and permission management for connection, subscription and publishing, support for data integration transformation using data bridging and with the rules engine, etc. Quick and easy access using the browser provides users with the convenience of using EMQX for more IoT business development.

![image](./assets/dashboard_preview.png)

## Main Features

This section introduces various features of EMQX that you can configure and manage through the Dashboard.

### [Monitoring](./monitoring.md)

View overall information of the running EMQX cluster, including connection count, subscribed topics, message delivery counts, inbound rates, and outbound rates. It also includes node lists, node information, and various system metric information. Additionally, you can view and manage client connections and subscription data.

### [Access Control](./acloverview.md)

Add and configure authentication and authorization mechanisms in EMQX visually.

### [Integration](./data-integration.md)

Utilize a powerful SQL-based rule engine and data integration, or the Flow editor's visual capabilities, for low-code data processing and integration. This helps in real-time extraction, filtering, enrichment, transformation, storage, and validation of MQTT data.

### Management

#### Cluster Settings

Supports online modification and update of MQTT, log, listeners, and other configuration items, which take effect immediately after successful updates.

#### Advanced MQTT

Manage and configure topic rewriting, automatic subscription, delayed publishing, and file transfer functionalities.

#### Extensions

Custom plugin integration to extend connection protocols through built-in gateway management and configuration. Also, use Hooks to modify or extend system functionality by intercepting function calls, message passing, and event passing between modules.

### Problem Analysis and Diagnostics

In addition to debugging through online MQTT over WebSocket client connections and topic metrics, support is also available for diagnostics and issue discovery using features like slow subscriptions and log trace.

### System

Manage and configure user accounts, audit logs, API keys, license settings, and single sign-on functionalities.

## Launch Dashboard

EMQX Dashboard is a web application that listens to port `18083` by default. After installing EMQX successfully, you can access and use the EMQX Dashboard by opening <http://localhost:18083/> (replace localhost with the actual IP address if deployed on a non-local machine) through your browser.

::: tip
EMQX can still be used normally without the Dashboard enabled. The Dashboard just provides the option for users to use it visually.
:::

For information on first login, password management, account lockout, HTTPS access, and role-based access control, see [Dashboard Security](../dashboard-security.md).

## Configure Dashboard

Dashboard listens on HTTP by default, with the default port `18083`. You can enable HTTPS or change the listener port. For configuration details, see [Dashboard Configuration](../configuration/dashboard.md).

