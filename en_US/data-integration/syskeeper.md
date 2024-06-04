# Bridge MQTT Data through Nari SysKeeper

::: tip

The Nari SysKeeper data integration is an EMQX Enterprise edition feature.

:::

Nari SysKeeper 2000 is a network physical isolation device. As a security management system, it is widely used in various industries, especially in fields requiring high-level security measures such as critical infrastructure and enterprise IT systems. EMQX supports data bridges between EMQX clusters deployed in different production zones. Production zones are divided into three security levels: I-III, where Security Zone I-II represents more secure, controlled areas, and Security Zone III is a less restrictive area, acting as a bridge between public-facing services and more secure internal areas. Typically, Security Zones I-II and III are isolated from each other. Through data bridging, MQTT messages can pass through the one-way SysKeeper network gate between Security Zones I-II and III, bridging with another EMQX cluster in a different security zone.

This page provides a comprehensive introduction to the data integration between EMQX and Nari SysKeeper with practical instructions on creating and validating the data integration.

## How It Works

The Nari SysKeeper data bridge is an out-of-the-box feature in EMQX, combining MQTT's real-time data capture and bridging capabilities with SysKeeper's powerful security isolation abilities. Through the built-in [rule engine](./rules.md) component, the integration simplifies the process of EMQX bridging through SysKeeper, eliminating the need for complex coding.

The diagram below illustrates a typical architecture of the data bridge between EMQX and SysKeeper. 

<img src="./assets/syskeeper_bridge_architecture.png" alt="syskeeper_bridge_architecture" style="zoom:67%;" />

The passthrough operation can be seen as a single-directional data bridge between two EMQX clusters deployed in Security Zone I-II and Security Zone III, with the following workflow:

1. **Create SysKeeper Proxy**: A SysKeeper Proxy needs to be created on EMQX in Security Zone III. The SysKeeper Proxy will start a special TCP listener to receive messages from the SysKeeper Forwarder.
2. **Message Publication and Reception**: Various devices in the power system connect directly to EMQX or through gateways (such as [NeuronEX](https://www.emqx.com/en/products/neuronex)) converting to MQTT protocol to successfully connect to EMQX. They send messages via MQTT based on their operational status, readings, or triggered events. When EMQX receives these messages, it initiates the matching process within its rule engine.
3. **Message Data Processing**: When a message arrives, it passes through the rule engine and is processed by the rules defined in EMQX. The rules determine which messages need to be bridged through SysKeeper to another EMQX cluster based on predefined criteria. If the rules specify data processing operations, these are applied, such as converting data formats, filtering out specific information, or enriching the message with additional context.
4. **Forwarding Through SysKeeper Forwarder**: The results from the rules are sent through the SysKeeper Forwarder, passing through the SysKeeper isolation device, to the SysKeeper Proxy created in EMQX in Security Zone III, thereby ingesting the messages into Security Zone III. When the SysKeeper Proxy is unavailable, EMQX provides an in-memory message buffer to prevent data loss. Data is temporarily held in the buffer and may be offloaded to disk to prevent memory overload. Note that data will not be preserved if the data integration or the EMQX node is restarted.
5. **Data Utilization**: In Security Zone III, the MQTT messages will be republished in their original form, and businesses can use the rule engine and data integration for further processing.

## Before You Start

This section describes the preparations you must complete before creating the Nari SysKeeper data bridge in the Dashboard.

### Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data integration](./data-bridges.md)

### Start a Nari SysKeeper Proxy in Security Zone III

To deliver MQTT messages through Nari SysKeeper, you need to enable a data proxy in Zone III to receive connections from the SysKeeper Forwarder in Zone I-II.

This section introduces how to start a Nari SysKeeper Proxy in Security Zone III.

1. Go to Dashboard, and click **Integration** -> **Connector**.

2. Click **Create** on the top right corner of the page. Select the **SysKeeper Proxy** and click **Next**:

3. Enter a name for the Connector. The name should combine upper/lower case letters or numbers, for example, `my_sysk_proxy`.

4. Set **Listen Address** to `Address:9002`, for example, `172.17.0.1:9002`. The SysKeeper Proxy will start a TCP listener. Make sure that the port is not occupied by other processes and that the firewall allows access to this port.

5. Leave the values of other configuration options as default.

6. Click the **Create** button.

Now you have created a Nari SysKeeper Proxy in the Security Zone III. Next, you need to create a Nari SysKeeper Forwarder.

## Create a Connector

This section demonstrates how to configure a Connector for Nari SysKeeper Forwarder in Security Zone I-II to forward the connections to the SysKeeper Proxy.

1. Go to EMQX Dashboard, and click **Integration** -> **Connector**.

2. Click **Create** on the top right corner of the page. Click to select the **SysKeeper Forwarder** and click **Next**.

3. Enter a name for the Connector. The name should combine upper/lower case letters or numbers, for example, `my_sysk`.

4. Set the **Server** to the address of the SysKeeper proxy server, for example, `172.17.0.1:9002`.

5. Before clicking **Create**, you can click **Test Connectivity** to test that the Connector can connect to the SysKeeper Proxy.

6. Click **Create** to complete the creation of the Connector. In the pop-up dialogue, you can click **Back to Connector List** or click **Create Rule** to continue to create a rule and a Sink for specifying the data to be forwarded to SysKeeper. For details steps, refer to [Create a Rule and Sink](#create-a-rule-and-sink).


## Create a Rule with SysKeeper Forwarder Sink

This section demonstrates how to create a rule in EMQX to process messages from the source MQTT topic `t/#` and send the processed results through the configured SysKeeper Forwarder Sink to the SysKeeper Proxy in another EMQX cluster.

1. Go to the EMQX Dashboard, and click **Integration -> Rules**.

2. Click **Create** on the top right corner of the page.

3. Enter a rule ID, for example, `my_rule`.

4. Enter the following statement in the SQL editor, which will forward the MQTT messages matching the topic pattern `t/#`:

   ```sql
   SELECT
     *
   FROM
     "t/#"
   
   ```

   ::: tip

   If you are a beginner user, click **SQL Examples** and **Enable Test** to learn and test the SQL rule.

   :::

5. Click the + **Add Action** button to define an action that will be triggered by the rule. With this action, EMQX sends the data processed by the rule to SysKeeper.

6. Select `SysKeeper Forwarder` from the **Type of Action** dropdown list. Keep the **Action** dropdown with the default `Create Action` value. You can also select a Sink if you have created one. This demonstration will create a new Sink.

7. Enter a name for the Sink. The name should combine upper/lower case letters and numbers.

8. Select the `my_sysk` just created from the **Connector** dropdown box.

9. Enter the configuration information:

   - **Topic**: The topic for the republished messages. The placeholders are supported, for example, `${topic}`.
   - **QoS**: The QoS for the republished messages.
   - **Message Template**: The payload template for the republished messages. The placeholders are supported, for example, `${payload}`.

10. Click **Create** to complete the Sink creation. Back on the **Create Rule** page, you will see the new Sink appear under the **Action Outputs** tab.

11. On the **Create Rule** page, verify the configured information. Click the **Create** button to generate the rule.

Now you have successfully created the rule and you can see the new rule appear on the **Rule** page. Click the **Actions(Sink)** tab, you can see the new SysKeeper Forwarder.

You can click **Integration** -> **Flow Designer** to view the topology. It can be seen that the messages under the topic `t/#` are published through SysKeeper Forwarder after parsing by the rule `my_rule`.

## Test the Rule

You can use the built-in WebSocket client in the Dashboard to test your SysKeeper Forwarder Sink and rule.

1. In Security Zone III, Click **Diagnose** -> **WebSocket Client** in the left navigation menu of the Dashboard.

2. Fill in the connection information for the current EMQX instance.

   - If you run EMQX locally, you can use the default value.
   - If you have changed EMQX's default configuration. For example, the configuration change on authentication can require you to type in a username and password.

3. Click **Connect** to connect the client to the EMQX instance.

4. Use this client to subscribe to the topic `t/test`.

5. In Security Zone I-II, repeat the above steps to create a client for publishing.

6. Scroll down to the publish area and type the following:

   - **Topic**: `t/test`

   - **Payload**:

     ```json
     {
       "hello": "I am from the Security Zone I-II"
     }
     ```

   - **QoS**: `1`

7. Click **Publish** to send the message.

8. In Security Zone III, you will see that the client has received this message if everything is correct.