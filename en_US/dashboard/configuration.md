# Configuration

EMQX provides hot configuration capabilities, which can dynamically modify the configuration at runtime without restarting the EMQX node. In the Dashboard page, we provide a visual configuration page for the hot configuration function, which can easily modify the configuration of EMQX. The following configuration items are currently provided:

- Listener
- MQTT
- Log
- Monitoring Integration

## Listener

Clicking on the listener under the configuration menu on the left will take you to the listener page, which is a list page of a listener by default. EMQX provides four common listeners by default:

- TCP listener using port 1883
- SSL/TLS secure connection listener using port 8883
- WebSocket listener using port 8083
- WebSocket secure listener using port 8084

We normally use the above default listeners, enter the corresponding port and protocol type. If you need to add other types of listeners, you can click the `Add Listener` button in the upper right corner to add a new listener.

### Add Listener

Click add listener button, you can see a form for adding a listener on the right panel. The form contains the basic configuration items of the listener. Enter a listener name to identify the listener, select a listener type, including tcp, ssl, ws, and wss types, enter the listener bind, you can enter the IP address and port number, using the IP address can limit the access range of the listener, or you can directly enter a port number, and then click the `Add` button.

![image](./assets/config-listener-add.png)

More about the listener configuration, please refer to [Listener Configuration](../admin/cfg.md#brokerlisteners).

### Manager Listener

After adding a listener, you can see the listener in the list, click the `Settings` button to modify the configuration of the listener. Note: the listener name, type and bind cannot be modified again in the settings. Click the `Delete` button to delete the listener. When deleting the listener, you need to enter the name of the listener you are currently deleting to confirm the deletion operation. In the list, we can also click the enable switch to enable or disable the listener. The number of connections under each listener can also be viewed in the list.

![image](./assets/config-listener-list.png)

::: warning
Modify and delete listeners are operations with a certain degree of risk, which need to be operated with caution. If you update or delete a listener, the client connections on that listener will be disconnected.
:::

## MQTT

Click the MQTT under the configuration menu on the left to go to the MQTT configuration page. In the MQTT configuration page, we can configure the MQTT configuration items, including:

### General

General menu is the general basic configuration items of the MQTT protocol, including similar configuration idle timeout, maximum packet size, maximum Client ID length, topic level and QoS, etc.

### Session

Session menu is the session related configuration items of the MQTT protocol, including session expiration interval (only supported by non-MQTT 5.0 connections, MQTT 5.0 connections need to be configured in the client), maximum subscription quantity, maximum flight window, whether to store QoS 0 messages, etc.

### Retainer

Retainer menu is the MQTT protocol related configuration items of the retained message, including the maximum number of retained messages, the storage type, etc. You can also view the retained messages in EMQX, view the topic, QoS, which client ID it comes from, the publish time, and click to view the specific Payload content, etc.

### Advanced

Advanced menu is the advanced configuration items of the MQTT protocol, including the configuration of the system topic $SYS, including the message publishing period, heartbeat period, etc. Topic rewrite, you can manually add the topic, action, original topic and target topic that needs to be rewritten. Proxy subscription, add the built-in rules of the proxy subscription. Finally, you can set the delayed publishing function.

More about the MQTT configuration, please refer to [MQTT Configuration](../admin/cfg.md#brokermqtt).

![image](./assets/config-mqtt.png)

## Log

Click the log under the configuration menu on the left to go to the log configuration page. The log configuration is divided into two parts, one is the Console Handler, and the other is the File Handler. In the log configuration page, you can set whether to enable the log handler, set the log level, the log formatter, text or json, and the log file can also set the path and name.

![image](./assets/config-log.png)

More about the log configuration, please refer to [Log Configuration](../admin/cfg.md#log).

## Monitoring

Clicking on Monitoring under the Configuration menu on the left will take you to the configuration page for monitoring integration. This menu page contains two tabs under.

- System: Depending on the user's needs, the settings for the [Alarms](./diagnose.md#alarms) function, such as alarm thresholds, check intervals, etc., can be adjusted to a certain extent according to user needs.
- Integration: Provides configuration for integration with third party monitoring platforms.

### System

If the default value of the current alarm trigger threshold or alarm monitoring check interval does not meet the actual needs of you, you can adjust the settings on this page. The current settings are divided into two modules: `Erlang VM` and `OS`, the default values and descriptions of each configuration item can be found in [alarms](../observability/alarms.md).

![image](./assets/monitoring-system.png)

### Integration

Click the monitoring integration under the configuration menu on the left to go to the monitoring integration configuration page. This page mainly provides integration configuration with third-party monitoring platforms. Currently, EMQX provides integration with Prometheus and StatsD. The configuration page can quickly enable this configuration and configure the URL of push gateway server and data reporting interval, etc.

::: tip
**Note**: There is a mutually exclusive configuration item component on the configuration page, but we can configure both `Prometheus` and `StatsD` at the same time.
:::

![image](./assets/config-data-monitor.png)

When using the third-party monitoring service `Prometheus`, we can directly use the API `/prometheus/stats` provided by EMQX to get monitoring data. When using this API, no authentication information is required. Please refer to [Prometheus](../observability/prometheus.md) for specific API.

In most cases, we don't need to use `Pushgateway` to monitor the metrics data of EMQX. And you can choose to configure a `Pushgateway` service address to push the monitoring data to `Pushgateway`, and then `Pushgateway` pushes the data to the `Prometheus` service. Click to view [When to use Pushgateway](https://prometheus.io/docs/practices/pushing/).

On the bottom of the page, click the "Help" button, select the default or use the `Pushgateway` method, configure the address or API information of the relevant service according to the provided usage steps, and then quickly generate the corresponding `Prometheus` configuration file. Finally, use this configuration file to start the `Prometheus` service.

![image](./assets/config-data-monitor-help.png)

Users can customize and modify the monitoring data in `Grafana` according to their needs. After starting the `Prometheus` service, you can click the `Download Grafana Template` button at the end of the help page to download the configuration file of the default dashboard provided by us. Import the file into `Grafana`, and we can view the monitoring data of EMQX through the visualization panel. Users can also download the template from the [Grafana official website](https://grafana.com/grafana/dashboards/17446-emqx/).

![image](./assets/emqx-grafana.jpg)
