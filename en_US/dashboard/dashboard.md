# Dashboard Home Page

After successful login, you can access the home page of the EMQX Dashboard, specifically the **Cluster Overview** page. This page includes the following tabs:

- **Cluster Overview**: Displays an overview of the data across the entire cluster.
- **Nodes**: View the list of nodes and node-specific information within the cluster.
- **Metrics**: View all data metrics within the cluster or individual nodes.

## Cluster Overview

This page provides an overview of the data for the entire EMQX cluster in operation, including the following information.

### Message Rate

Messages are the key data in EMQX. Messages are the data content being transmitted by all MQTT clients or real devices connected to EMQX. Clients or devices send or receive messages through topics as a way to complete data communication between them.

The card in the upper left corner of this overview page provides a clearer and easier way to monitor the rate changes in the current volume of messages incoming and outgoing in the system (message rate is measured in how many messages per second) by visualizing the rate spectrum with real-time rate values.

![image](./assets/overview-3.png)

### Connections and Subscriptions

As an MQTT broker, the number of connections and topics subscribed to EMQX is one of the most important metrics to observe. The connections are the number of MQTT clients or real devices currently connected to EMQX, the number of subscriptions is the total number of topics currently subscribed to in each client, and topics are the unique subscriptions.

In the tab on the top right of the overview page, we can quickly see the number of connections, subscriptions, and topics in the cluster. The statistics in the card will be updated in real-time when a connection or a subscription topic is updated.

![image](./assets/overview-1.png)

::: tip
Subscriptions are differentiated by client, while topics are unique subscriptions and the same topic may be included in different clients.
:::

In addition to providing real-time statistics, the bottom of the page also provides a visual chart that allows you to view historical and current changes in the number of connections and subscriptions by time (the time format: YYYY/MM/DD HH:mm), which allows you to monitor the trend of the number of connections and subscriptions within the entire EMQX cluster more clearly and intuitively. Hovering over the chart and clicking the icon in the top right corner allows you to enlarge the chart.

![image](./assets/overview-2.png)

### Messages

The number of messages is the statistics of the number of data transferred between clients or devices, and on the page includes the incomming, outgoing and dropped messages.

In the chart at the bottom of the page, you can see a visual chart of the number of messages, and view the historical and current number of messages by time change (the time format: YYYY/MM/DD HH:mm), which allows users to better monitor dynamically the real-time changes of all messages within the current EMQX cluster. Hovering over the chart and clicking the icon in the top right corner allows you to enlarge the chart.

![image](./assets/overview-4.png)

::: tip
In the above, all charts over time are available in the top left corner: statistics for the last 1 hour, last 6 hours, last 12 hours, last 1 day, last 3 days and last 7 days
:::

## Nodes

EMQX, the most scalable MQTT broker for IoT, cluster deployment is supported, and each EMQX in a cluster is a node.

### Node Data

You can monitor the whole EMQX cluster by the card in the middle of the overview page, including the topology diagram to visualize the association and distribution of all nodes in the cluster.

Click on a single node in the topology diagram to view the basic information and operation status of the current node, including the node name and role, the number of connections, subscriptions and topics, the current EMQX version (click on the version number to view the change logs of the version, so as to quickly understand the content updates of the current version), and you can also view the CPU load and memory usage of the OS where the node is deployed (memory can only be showed in nodes deployed to Linux).

![image](./assets/overview-5.png)

::: tip
When the green node in the topology diagram turns gray, it means that the node is currently stopped
:::

### Node List

Clicking **View Nodes** in the Node Information section allows access to the Nodes page, or you can click the **Nodes** tab at the top to access it. The Nodes page lists all nodes currently in the EMQX cluster, providing a quick overview of each node's name, operational status, uptime, version information, Erlang process count, memory usage, CPU load, and other details. Clicking the **Refresh** button in the top right corner enables real-time updates of the latest information in the current node list.

![image](./assets/nodes.png)

### Node Details

The node list can only show partial basic information about nodes. To view the comprehensive information about a node, click the node name in the **Name** column to access the node's details page. On the details page, you will find the following cards:

- In the **Node Info** card, in addition to basic node information, you can also view details such as the maximum file handle count for the current node, system paths, and log paths (displaying log paths requires enabling file log processing in the configuration).
- In the **Node Statistics** card, you can see various statistics about the current node, including the number of connections, subscriptions, topics, retained messages, sessions, and shared subscriptions. The statistics values are divided into two parts separated by a slash ("/"): the left side shows real-time data and the right side shows the high watermark data, which represents the peak values reached by the current data.

![image](./assets/node-detail.png)

## Metrics

Clicking the **Metrics** tab at the top allows you to access the metrics page, where you can view all data metrics generated during the operation of the EMQX cluster or a specific node. This includes message information, message statistics, and traffic send/receive statistics. These metrics provide insights into the current service status.

In the dropdown menu at the top right corner, you can choose to view either cluster-wide data or data specific to a single node. Clicking the adjacent **Refresh** button enables real-time monitoring of metrics data on the current page.

For detailed explanations and comprehensive details of metric data, please visit [Metrics](../observability/metrics-and-stats.md).

### Connections, Sessions, and Access

The metric data covers 4 aspects: bytes, packets, messages, and events. In the card, you can view metric data related to events, including counts of events such as connection sessions and authentication and authorization events.

![image](./assets/metrics-1.png)

### Messaging

The four cards below provide statistics on the data generated during message transmission, such as statistics on traffic sent and received (in bytes), statistics on the number of packets, the number of messages and the number of messages delivered.

![image](./assets/metrics-2.png)
