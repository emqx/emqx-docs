# Integrate with Datadog

[Datadog](https://www.datadoghq.com/) is a cloud-based observability and security platform that offers automated infrastructure monitoring, application performance monitoring, log management, and real-user monitoring. It unifies these capabilities into a real-time application solution, enabling developers to easily monitor, analyze, and optimize performance and reliability.

EMQX 提供了开箱即用的 [Datadog 集成](https://docs.datadoghq.com/integrations/emqx/)，方便用户更好地了解当 EMQX 的状态，并监测和排除系统性能问题，更好地构建高效、可靠和实时数据传输的物联网应用。

## 工作原理

EMQX 与 Datadog 的集成不是一个新的功能，它充分利用了 EMQX 现有的功能，工作原理如下：

1. 在 EMQX 集群侧安装 Datadog Agent，并添加 [Datadog - EMQX 集成](https://docs.datadoghq.com/integrations/emqx/)，这是 Datadog 针对 EMQX 提供的开箱即用的扩展插件。

2. 通过修改集成预设的配置，让 Datadog Agent 周期性的从 EMQX 的 Prometheus pull REST API 拉取指标数据。指标数据经过 Datadog Agent 处理后上传到 Datadog 平台。

3. 在 Datadog 云平台中，可以通过集成预设的 Dashboard 图表查看各类指标数据。

接下来，我们将按照以上步骤进行设置。

## Install the Datadog Agent

Begin by creating a [Datadog](https://www.datadoghq.com/) account and logging in to the Datadog console.

Next, install the [Datadog Agent](https://docs.datadoghq.com/getting_started/agent/) on the server where EMQX is hosted. The Agent collects EMQX metrics and sends them to the Datadog cloud.

Deploy the Datadog Agent on the server where the EMQX cluster resides or on a server with access to the EMQX nodes. If you haven't installed the Agent yet, follow these steps:

1. Navigate to **Integrations** → **Agent** in the menu bar to access the Agent Installation Instructions page.

2. Choose your operating system version and follow the provided instructions.

![Install Datadog Agent](./assets/datadog-agent-install.png)

## Add EMQX Integration to Datadog

EMQX offers out-of-box [Datadog integration](https://docs.datadoghq.com/integrations/emqx/) that can be easily incorporated into your Datadog console by following these steps:

1. Open your Datadog console and navigate to **Integrations** → **Integrations** in the menu bar.

2. In the **Search Integrations** box, type "EMQX" to find the integration with the same name and author.

3. Click the **Install Integration** button in the upper right corner of the pop-up box to add the EMQX integration to Datadog.

![Install Datadog EMQX Integration](./assets/datadog-search-emqx-intergration.png)

4. After completing the installation, navigate to the **Configure** tab to access the configuration guidelines for the EMQX integration. The necessary configuration steps are carried out within the Datadog Agent.

![Config EMQX Datadog Integration](./assets/datadog-integration-configuration.png)

## Add and Enable EMQX Integration on Datadog Agent

Following the configuration guidelines, add the EMQX integration to the Datadog Agent to configure the collection and reporting of EMQX metrics.

1. Execute the following command on the server where the Datadog Agent is hosted to add the EMQX integration. Note that this example uses version 1.1.0; always refer to the latest guidelines for the appropriate version:

    ```bash
    datadog-agent integration install -t datadog-emqx==1.1.0
    ```

2. Once the installation is complete, proceed to modify the Agent configuration file to enable EMQX integration:

    Navigate to the Agent configuration directory (usually located at /opt/datadog-agent/etc/conf.d/). Locate the emqx.d directory within this directory. You'll find a sample configuration file named conf.yaml.example in the emqx.d directory.

    Create a copy of this file in the same directory and rename it to conf.yaml. Edit the conf.yaml file, adjusting the following configuration item:

    ```bash
    instances:
      - openmetrics_endpoint: http://localhost:18083/api/v5/prometheus/stats?mode=all_nodes_aggregated
    ```

    The `openmetrics_endpoint` specifies the address from which the Datadog Agent extracts metrics data in OpenMetrics format. In this case, it's set to the HTTP API address of EMQX. Make sure to replace this with an address accessible by the Datadog Agent.

    The API also allows specifying the range of metrics to pull via the `mode` query parameter. The meaning of each parameter is as follows:

| **Parameter** | **Description** |
| --- |  --- |
| node | Returns metrics for the current requested node. If no mode parameter is specified, this is used by default. |
| all_nodes_unaggregated | Returns metrics for each individual node in the cluster, maintaining the independence of metrics. The results include node names for differentiation. |
| all_nodes_aggregated | Returns aggregated metric values for all nodes in the cluster. |

For a unified view, use the `mode=all_nodes_aggregated` option. This ensures that the Datadog control sees values for the entire EMQX cluster.

3. To [restart the Datadog Agent](https://docs.datadoghq.com/agent/guide/agent-commands/#start-stop-and-restart-the-agent) on macOS, follow these steps:

    ```bash
    launchctl stop com.datadoghq.agent
    launchctl start com.datadoghq.agent
    ```

4. After rebooting your system, use the following command to verify if the EMQX integration is successfully enabled. If you see "Instance ID: ... \[OK\]", it indicates that the integration has been successfully enabled.

    ```bash
    $ datadog-agent status | grep emqx -A 4
        emqx (1.1.0)
        ------------
          Instance ID: emqx:1865f3a06d300ccc \[OK\]
          Configuration Source: file:/opt/datadog-agent/etc/conf.d/emqx.d/conf.yaml
          Total Runs: 17
          Metric Samples: Last Run: 166, Total: 2,822
          Events: Last Run: 0, Total: 0
          Service Checks: Last Run: 1, Total: 17
          Average Execution Time : 43ms
          Last Execution Date : 2024-05-11 17:35:41 CST / 2024-05-11 09:35:41 UTC (1715420141000)
          Last Successful Execution Date : 2024-05-11 17:35:41 CST / 2024-05-11 09:35:41 UTC (1715420141000)

    ```

At this point, you've completed all the necessary configurations on the Datadog Agent. The Agent will now periodically collect EMQX runtime data and send them to Datadog. Next, let's check the Datadog console to ensure that the metrics are being collected correctly.

## View EMQX Metrics on the Datadog Console

The Datadog Agent's EMQX integration provides a ready-to-use dashboard chart that displays node status, message status, and other more in-depth observability metrics. You can follow these steps to utilize it:

1. Open the Datadog console and navigate to **Integrations** → **Integrations** in the menu bar.

2. Locate the installed EMQX integration and click to open it.

3. Switch to the **Monitoring Resources** tab within the pop-up box to open the **EMQX Overview** charts under **Dashboards**.

    ![Monitoring Resources Tab](./assets/datadog-dashboard-overview.png)

**图表包含以下内容**

**The charts provide the following information:**

- OpenMetrics Health: The number of active metrics collectors.
- Total Connections: The overall count of connections, including those that remain the sessions despite being disconnected.
- NodeRunning: The quantity of running nodes within the cluster.
- Active Topics: The number of currently active topics.
- NodeStopped: The count of stopped nodes in the cluster.
- Connection
  - Total: The total number of connections, including those that maintain the session even when disconnected.
  - Live: The number of actively maintained TCP connections.
- Topic
  - Total: The overall number of topics.
  - Shared: The count of shared topics.
- Session: The total number of sessions.
- Erlang VM: The CPU, memory, and queue usage of the Erlang virtual machine.
- Retainer & Delayed
  - Retained: The number of retained messages.
  - Delayed: The count of delayed messages.
- Message
  - Sent & Received: The rate of sent and received messages.
  - Delayed & Retained: The rate of delayed and retained messages.
  - Publish & Delivered: The rate of message publishing and delivery.
  - Delivery Dropped: The number of delivered messages that were dropped.
- Client
  - Connected & Disconnected: The rate of connections being established and terminated.
  - Sub & UnSub: The subscription and unsubscription rates.
  - AuthN & AuthZ: Information on authentication and authorization rates.
  - Delivery Dropped: The number of dropped delivery messages.
- Mria: The total number of Mria transactions.

Below are screenshots of some of the charts; the values dynamically change based on load of EMQX and client activity.

![Metrics Overview](./assets/datadog-dashboard-detail.png)

![Connection, Topic, and Session](./assets/datadog-dashboard-conn.png)

![The Rate of Sent and Received Messages, the Number of Retained/Delayed/Dropped Messages](./assets/datadog-dashboard-msg-rate.png)

![Client Event](./assets/datadog-dashboard-events.png)

## Next Steps

The charts built into Datadog's EMQX integration show only some of the key metrics. You can also refer to [this document](https://docs.datadoghq.com/integrations/emqx/#metrics) to access all the reported EMQX metrics and create your own monitoring charts based on them.

You can configure alert rules in Datadog based on these metrics. When certain metrics reach preset thresholds or abnormal situations occur, Datadog will send notifications to remind you to take necessary actions promptly, minimizing the impact of system failures on your business.
