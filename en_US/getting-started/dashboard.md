---
# 编写日期
date: 2020-02-25 17:15:26
# 作者 Github 名称
author: tigercl
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# Dashboard

## Introduction

EMQ X Broker provides Dashboard to facilitate users to manage equipment and monitor related indicators. Through Dashboard, you can view the basic information of the server, load and statistical data, you can view the connection status of a client and even disconnect it, and you can also dynamically load and unload specified plug-ins. In addition, EMQ X Dashboard also provides a visual operation interface of the rule engine, and also integrates a simple MQTT client tool for user testing.

## Start Dashboard

The EMQ X Dashboard function is implemented by the [emqx-dashboard](https://github.com/emqx/emqx-dashboard) plugin, which is enabled by default, and it will be automatically loaded when the EMQ X Broker starts. If you wish to disable the Dashboard function, you can modify `{emqx_dashboard, true}` in  `data/loaded_plugins` to `{emqx_dashboard, false} `.

```bash
{emqx_dashboard, true}.
```

## View Dashboard

EMQ X Dashboard is a web application, and you can access it directly through the browser without installing any other software.

When EMQ X Broker runs successfully on your local computer and EMQ X Dashboard is enabled by default, you can visit http://localhost:18083 to view your Dashboard. The default user name is admin and the password is public .

## Configure Dashboard

You can view or modify the configuration of EMQ X Dashboard in  `etc/plugins/emqx_dashboard.conf`. Note that `etc` here does not refer to the system directory, see [directory structure](directory.md#) for details.

EMQ X Dashboard configuration items can be divided into two parts of default user and listener:

**Default User** 

EMQ X Dashboard can configure multiple users, but only the default user can be configured in the configuration file.

It should be noted that once you change the password of the default user through Dashboard, the relevant information of the default user will be based on your latest changes on the Dashboard, and the default user configuration in the configuration file will be ignored.

**Listeners**

EMQ X Dashboard supports both HTTP and HTTPS Listeners, but only HTTP Listeners with a listening port of 18083 are enabled by default. For the introduction of Listeners, please refer to [Configuration Instructions](config.md#).

For the detailed configuration item description of Dashboard, please refer to [Configuration Item](../configuration/configuration.md).


:::: tabs

::: tab EMQ X Community
## Dashboard interface


In order to enable users to quickly locate and switch the current position during operation and browsing, EMQ X Dashboard adopts the mode of side navigation. By default, Dashboard includes the following first-level navigation items:

| Navigation  | Description                                                  |
| ----------- | ------------------------------------------------------------ |
| MONITORING  | Provide display pages for server and client monitoring information |
| RULE ENGINE | Provide a visual operation page of the rule engine           |
| MANAGEMENT  | Provide management pages for extensions of plugins and applications |
| TOOLS       | Provide WebSocket client tool and HTTP API quick check page  |
| ADMIN       | Provide Dashboard user management and display settings pages |

### MONITORING

EMQ X Dashboard provides a very rich data monitoring project, that completely covers the server and client. All of this information will be reasonably displayed to users on the page under `MONITORING`.

#### Overview

As the default display page of Dashboard, `Overview` provides detailed information of EMQ X Broker ’s current nodes and key information of other nodes in the cluster to help users quickly get the status of each node.

![image](../assets/dashboard-overview.png)

#### Clients

The `Clients` page provides a list of clients connected to the specified node, and also supports direct client search via `Client ID`. In addition to viewing the basic information of the client, you can also click the `Kick Out` button on the right side of each record to kick out the client. Note that this operation will disconnect the client and terminate its session.

The `Clients` page uses a snapshot to display the client list. Therefore, when the client status changes, the page does not automatically refresh, and you need to manually refresh the browser to get the latest client data.

![image](../assets/dashboard-clients.png)

If you can not get the information you need in the client list, you can click `Client ID` to view the detailed information of the client.

![image](../assets/dashboard-clients-basic-info.png)

We divided the various fields in the client details into connection, session and metrics. The following is the description of each field:

Connection

| Field             | Description                                                  |
| ----------------- | ------------------------------------------------------------ |
| Node              | Name of the node to which the client connects                |
| Client ID         | Client ID                                                    |
| Username          | The user name used when the client connects, for security reasons, the password will not be displayed |
| Protocol          | The protocol name and version used by the client             |
| IP Address        | The client's network IP address, which can be IPv4 or IPv6   |
| Port              | Client source port                                           |
| Is Bridge         | Indicate whether the client connects via bridge              |
| Connected At      | Client connection time                                       |
| Disconnected At   | Client offline time                                          |
| Connection Status | Client connection status                                     |
| Zone              | Indicates the configuration group used by the client         |
| Keepalive         | Keep-alive time, unit: second                                |

**Session**

| Field                         | Description                                   |
| ---------------------------------- | ------------------------------------------------ |
| Clean Session                      | Indicates whether the client has used a brand new session |
| Expiry Interval                    | Session expiration interval, unit: seconds                   |
| Created At                         | Session creation time                |
| Subscriptions Count                | Current number of subscriptions      |
| Maximum Subscriptions Count        | Maximum number of subscriptions allowed |
| Inflight Window Size                | Current inflight window size, please refer to [InFlight Window and Message Queue](../advanced/inflight-window-and-message-queue.md#) |
| Maximum Inflight Window Size        | Maximum size of inflight window  |
| Message Queue Size                 | Current message queue size, please refer to [InFlight Window and Message Queue](../advanced/inflight-window-and-message-queue.md#) |
| Maximum Message Queue Size         | Maximum size of message queue |
| Uncomfirmed PUBREC Packets         | Number of unconfirmed PUBREC messages |
| Maximum Uncomfirmed PUBREC Packets | Maximum number of unconfirmed PUBREC messages allowed |

Metric

| Field                         | Description                         |
| ---------------------------------- | -------------------------------------- |
| Number of Bytes Received           | Number of bytes received by EMQ X Broker (same below) |
| Number of TCP Packets Received     | Number of TCP Packets Received |
| Number of MQTT Packets Received    | Number of MQTT Packets Received |
| Number of PUBLISH Packets Received | Number of PUBLISH Packets Received |
| Number of Bytes Sent               | Number of Bytes Sent     |
| Number of TCP Packets Sent         | Number of TCP Packets Sent |
| Number of MQTT Packets Sent        | Number of MQTT Packets Sent |
| Number of PUBLISH Packets Sent     | Number of PUBLISH Packets Sent |

In the `Subscriptions` tab of the client details, you can view the subscription information of the current client, and create or cancel subscriptions:

![image](../assets/dashboard-clients-subscriptions.png)

#### Subscriptions

The `Subscriptions` page provides all subscription information under the specified node, and supports users to query all subscriptions of the specified client through the `Client ID`.

![image](../assets/dashboard-subscriptions.png)

### RULE ENGINE

The rule engine of EMQ X Broker can flexibly process messages and events, such as converting the message into a specified format and storing it in a database table or resending it to the message queue. In order to facilitate users to better use the rules engine, EMQ X Dashboard provides corresponding visual operation pages, and you can click the `RULE ENGINE` navigation item to access these pages.

Due to the complexity of the related concepts of the rule engine, the operations involved may occupy a considerable amount of space. Considering your reading experience, we will introduce the rule engine through additional documents of  [rule-engine](../rule/rule-engine.md#).

### MANAGEMENT

Currently, the EMQ X Dashboard's `MANAGEMENT` navigation item mainly includes an monitoring management page for extension plug-in  and management page of AppID and AppSerect for HTTP API authentication.

#### Plugins

The `Plugins` page lists all the plug-ins that EMQ X Broker can find, including the official plug-ins of EMQ X and the plug-ins that you developed in accordance with the official standards of EMQ X. You can check the current running status of the plug-ins and start and stop the plug-ins at any time on this page.

![image](../assets/dashboard-plugins.png)

You can see that in addition to [emqx-dashboard](https://github.com/emqx/emqx-dashboard), EMQ X Broker will also start  other 4 plugins by default:

![image](../assets/dashboard-running-plugins.png)

#### Applications

The `Applications` page lists the currently created applications. You can perform operations such as creating applications, temporarily disabling or starting access permissions for an application on this page. EMQ X Broker will create a default application with AppID of `admin` and AppSecret of `publish` for users to access for the first time:

![image](../assets/dashboard-applications.png)

You can click the `New App` button in the upper right corner of the `Application` page to create a new application, where AppID and AppSecret are required. After the creation is complete, you can click the `View` button to view the application details, and AppSecret will also be displayed in the details. The following is a description of the relevant fields:

| Field        | Description                                                  |
| ------------ | ------------------------------------------------------------ |
| AppID        | It is used to distinguish different applications, so it is not repeatable. When creating an application, Dashboard will automatically generate a random suggested application ID for you |
| AppName      | It can be repeated, but for your own convenience, we do not recommend using a duplicate application name |
| AppSecret    | The application key assigned by EMQ X Broker that can be viewed in the application details |
| Expired date | The expiration time of the application, the default is to never expire |
| Remark       | Your description of the application, facilitate later management |
| Status       | There are only two types of application status: Allowed and Denied. In Denied status, EMQ X Broker will deny the access request of HTTP API using the AppID and App Secret |

### TOOLS

Currently, the `TOOLS`  navigation item of EMQ X Dashboard mainly includes WebSocket client tool page and HTTP API quick check page.

#### Websocket

The `Websocket` page provides you with a simple but effective WebSocket client tool, which includes connection, subscription and publishing functions. At the same time, you can view the message data you send and receive, we hope it can help you quickly complete test verification of certain scenarios or functions:

![image](../assets/dashboard-websocket.png)

#### HTTP API

The `HTTP API` page lists all the HTTP APIs currently supported by EMQ X Broker and their descriptions:

![image](../assets/dashboard-http-api.png)

### ADMIN

#### Users

You can view and manage users who can access and operate Dashboard on the `Users` page:

![image](../assets/dashboard-users.png)

#### Settings

Currently, EMQ X Dashboard only supports the modification of the topic and language settings:

![image](../assets/dashboard-settings.png)

#### Help

If you encounter any problems in using EMQ X Broker, we provide you with links to FAQ and other documents on the `Help` page. If our existing documents still cannot solve your problem, you can go to our open source community on Github to consult our technical staff.

![image](../assets/dashboard-help.png)

:::

::: tab EMQ X Enterprise

## Quick Start

If EMQ X is installed on this machine, use your browser to open the
address <http://127.0.0.1:18083>. To log in, enter the default user name
`admin` and the default password `public` to log in to Dashboard. If you
forget to account information, click the **Forgot Password** button on
the login page and follow the instructions or use management commands to
reset or create a new account.

The Dashboard interface is shown in the following figure. It contains
the left navigation bar, top control bar, and content area. The top
control bar (red frame area) has three functions:

  - Alarm: EMQ X alarm info. The number of alarms triggered by excessive
    resource usage and EMQ X internal errors is displayed. Click to view
    the alarm list.
  - User: the currently logged in Dashboard user, which can be used to
    log out and change passwords;
  - I18n: Dashboard displays Chinese / English by default according to
    the user's browser language. Click to switch
languages.

![image-20200304160950998](./assets/dashboard-ee/image-20200304160950998.png)

## Monitor

On the monitoring page, you can view the running status of the current
cluster. The functional area from top to bottom of the interface is as
follows:

### Running Status

There are four cards at the top of the page, which includes the
message-out rate of the cluster, the message-in rate, the number of
subscriptions, and the current number of connections.

### Node

Click the node drop-down list to switch to view the basic information of
the node, including EMQ X version, runtime, resource occupation,
connection, and subscription data. Some data is defined as follows:

  - Memory: The current memory/maximum memory used by the Erlang VM,
    **where the maximum memory is automatically applied to the system by
    EMQ X depending on the resource usage**.
  - Max Fds: Allow the current session/process to open the number of
    file handles. If this value is too small, it will limit the EMQ X
    concurrency performance. When it is far less than the maximum number
    of connections authorized by the license, please refer to the tuning
    or contact the EMQ technical staff to modify;
  - Erlang Process、Connections、Topics、Subscriptions、Retained、Share
    Subscription: It is divided into two groups by `/` which are the
    current value and the maximum
value.

![image-20200304161123450](./assets/dashboard-ee/image-20200304161123450.png)

### Recent status

Click the button group on the right side of the node data area to switch
to the recent cluster data graph. The graph values are the actual values
during the sampling
period:

![image-20200304161147766](./assets/dashboard-ee/image-20200304161147766.png)

### Nde details

Click the **View More** button below the node data to open the node
details page, view the **basic information** of the current node, the
**listener** and connection status, and **metrics**.

#### Listener

The listener is the list of the current EMQ X listening network ports.
The field information is as follows:

  - Protocol: listening network/application protocols, including
    protocol and function info:
      - mqtt:ssl: MQTT TCP TLS protocols, the default is 102400
      - mqtt:tcp: MQTT TCP protocols, the default is 102400
      - <http:dashboard>: HTTP protocol used by Dashboard, the default
        is 512
      - <http:management>: HTTP protocol used by EMQ X REST API, the
        default is 512
      - mqtt:ws :MQTT over WebSocket, the default is 102400
      - mqtt:wss: MQTT over WebSocket TLS, the default is 102400
  - Address: Listen to the bound network address and port. Listen to all
    IP addresses by default;
  - Acceptors: listening the processor thread pool;
  - Connect: It contains a set of current/maximum values. The current
    value is the actual number of established connections. The maximum
    value is the maximum number of connections configured in the
    configuration file. **If any listener exceeds the maximum value, a
    new connection cannot be established.**

#### About the maximum number of connections

The actual maximum connection depends on the license and configuration:

1.  The number of connections per listening protocol in the node cannot
    exceed the maximum number of connections in the configuration file;
2.  The total number of MQTT/MQTT over WebSocket protocol connections in
    the cluster cannot exceed the upper limit of the license.

Of course, system tuning and hardware will also affect the maximum
number of connections. Please refer to tuning or contact EMQ technicians
for
confirmation.

![image-20200304161205291](./assets/dashboard-ee/image-20200304161205291.png)

![image-20200304161218417](./assets/dashboard-ee/image-20200304161218417.png)

### License

You can view the license information of the cluster by monitoring the
license card at the bottom of the page:

  - Customer: Name of the company or department of the same business
    contract customer.
  - License usage: License specification and current usage.
  - Issuance of email: Same email address as a business contract
    customer.
  - License Edition: License edition, trial or official.

EMQ will issue a mailbox through email notification before the
certificate expires. Please pay attention to receiving information so as
not to miss the renewal time, which will affect the
business.

![image-20200304161239961](./assets/dashboard-ee/image-20200304161239961.png)

## Connections

### Current Connections

The client list page displays a list of currently connected
clients.Several important information in the list is as follows:

  - Client ID、Username: MQTT client ID and MQTT username, respectively.
    Click the **Client ID** to view the client details and subscription
    info.
  - IP Address: The client address + port.
  - Disconnect/Clean Session: For an online client, the connection will
    be disconnected and the session will be cleared. If the client is
    not online, clicking Clear Session will clear the session such as
    the client's subscription
relationship.

![image-20200304161435785](./assets/dashboard-ee/image-20200304161435785.png)

### Basic Info

Click the **Client ID** to view the client details and subscription
list. The basic information includes the selected client connection info
and session info and includes key business information such as message
traffic and message
statistics.

![image-20200304161451854](./assets/dashboard-ee/image-20200304161451854.png)

### Subscription

The subscription contains information about the topics to which the
selected client has subscribed:

  - Unsubscribe: Clicking the Unsubscribe button will delete the
    subscription relationship between the device and the topic. This
    operation is insensitive to the device.
  - Add: Specify a topic for the selected client proxy
subscription.

![image-20200304161511410](./assets/dashboard-ee/image-20200304161511410.png)

## Rule

### Rule Engine

Use SQL to set rules to filter, encode, decode, and modify message data,
and seamlessly forward processed data to data destinations such as
databases, stream processing, and API gateways.

The Rule Engine not only provides a clear and flexible configurable
business integration solution, but also simplifies the business
development process, improves user usability, and reduces the degree of
coupling between business systems and EMQ X. Excellent infrastructure.

  - ID: Unique ID within the cluster, which can be used in CLI and REST
    API.
  - Topic: The MQTT topic or EMQ X event topic that the Rule matches.
  - Monitor: Click to display the execution statistics of the selected
    Rule, including the number of rule hits and executions, and the
    number of success/failed actions
triggered.

![image-20200304161544424](./assets/dashboard-ee/image-20200304161544424.png)

### Create Rule

EMQ X will trigger the Rule Engine when the message is published and the
event is triggered, and the rules meeting the triggering conditions will
execute their respective SQL statements to filter and process the
context information of the message and event.

With the Actions, the Rule Engine can store the message processing
results of a specified topic to the database, send them to the HTTP
Server, forward them to the Kafka or RabbitMQ, and republish them to a
new topic or another broker cluster like Azure IoT Hub. Each rule can
allocate multiple Actions.

1.  Select the messages published to t/\# and select all fields:

<!-- end list -->

``` sourceCode sql
SELECT * FROM "message.publish" WHERE topic =~ 't/#'
```

2.  Select the message published to the t/a topic, and select the "x"
    field from the message payload in JSON format:

<!-- end list -->

``` sourceCode sql
SELECT payload.x as x FROM "message.publish" WHERE topic =~ 't/a'
```

The Rule Engine uses the **Events** to process the built-in events of
EMQ X. the built-in events provide more sophisticated message control
and client action processing capabilities, which can be used in the
message arrival records of QoS 1 and QoS 2, the device up and down line
records and other businesses.

1.  Select the client connected event, filter the device with Username
    'emqx' and select the connection
information:

<!-- end list -->

``` sourceCode sql
SELECT clientid, connected_at FROM "client.connected" WHERE username = 'emqx'
```

![image-20200304161937421](./assets/dashboard-ee/image-20200304161937421.png)

## Resource

The resource instances (such as database instance and Web Server )
required by the Rule Engine action. Before creating a rule, you need to
create the resources required for the relevant action and ensure that
the resources are available.

### Resource list

  - ID: Unique ID within the cluster, which can be used in CLI and REST
    API.
  - Status: After the resource is created, each node in the cluster will
    establish a connection with the resource, click to expand the
    resource status on the node.
  - Delete: The resources being used by the Rule Engine cannot be
    deleted. Please delete the rules that depend on the selected
    resource before
deleting.

![image-20200304162110301](./assets/dashboard-ee/image-20200304162110301.png)

### Create Resource

Click the **Create** to open the resource creation dialog. Select the
resource type and enter the corresponding connection information to
create the resource. Click **Test** to check the resource connectivity
before
creation.

![image-20200304162128853](./assets/dashboard-ee/image-20200304162128853.png)

## Schema Registry

Schema Registry supports Protobuf, Avro, and private message encoding
parsing and processing, and can implement complex operations like
message encryption, message compression, and binary-JSON message
conversion.

## Alarm

The alarm shows the basic alarm information of EMQ X, including current
alarm and historical alarm. More advanced alarm, log and monitoring
management is provided by EMQ X Control Center, please contact EMQ
technicians if
necessary.

![image-20200304162147114](./assets/dashboard-ee/image-20200304162147114.png)

## Plugin

View the list of EMQ X built-in plugins.

Unlike the command line plugin management, the plugin starts and stop
operations on the Dashboard are synchronized to the cluster. If the
plugin fails to start, check whether the configuration of each node in
the cluster is correct. If any node fails to start, the plugin cannot be
successfully
started.

![image-20200304162200809](./assets/dashboard-ee/image-20200304162200809.png)

## Tool

It provides MQTT over WebScoket client test tool, which can realize the
publish and subscribe test of multiple mqtt connections at the same
time.

## Setting

Provides parameter configuration for the EMQ X cluster and supports hot
configuration. You can join and leave the cluster on the Dashboard.

### Basic

Some basic configuration items that can be hot updated in`emqx.conf` are
opened in the settings. You can complete most configuration items such
as whether to enable anonymous authentication, ACL cache events, and ACL
cache switches without restarting EMQ X.

The basic settings are organized in zones. By default, the external zone
is associated with the listener on port
1883.

<!-- ![image-20200201165159596](./assets/dashboard-ee/image-20200201165159596.png) -->

### Cluster

The cluster setting cannot change the cluster mode, but it can be used
for manual cluster invitation nodes to join the cluster, and change the
cluster parameter parameters such as static cluster and DNS cluster.

## General

### Application

In order to invoke the certificate of REST API, the application can
query and adjust EMQ X cluster information through REST API, and manage
and operate the equipment.

After the application is created successfully, click the Application ID
in the **AppID** column of the application list to view the AppID and
Secret. You can edit the application status and expiration time, and
create or delete an application.

### Users

Dashboard user account management, you can create, edit, delete users,
if you forget the user password, you can reset the password through CLI.
:::

::::