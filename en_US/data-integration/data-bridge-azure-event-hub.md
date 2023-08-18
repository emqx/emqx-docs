# Stream Data into Azure Event Hub


[Azure Event Hub](https://azure.microsoft.com/en-us/products/event-hubs) is a real-time managed event streaming platform for data ingestion. EMQX's integration with Azure Event Hub offers users dependable data transport and processing capabilities in high-throughput situations.  EMQX currently supports Azure Event Hub integration via its Kafka protocol compatible endpoints, using SASL/PLAIN authentication.

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.
:::
{% endemqxce %}

:::tip Prerequisites

- Knowledge about EMQX data integration [rules](./rules.md)
- Knowledge about [data bridge](./data-bridges.md)

:::

## Feature List

- [Async mode](./data-bridges.md#async-mode)
- [Batch mode](./data-bridges.md#batch-mode)
- [Buffer queue](./data-bridges.md#buffer-queue)

## Quick Start Tutorial

This section introduces how to stream data into or from Kafka, covering topics like how to set up a Kafka server, how to create a bridge and a rule for forwarding data to the bridge and how to test the data bridge and rule.

### Setup Azure Event Hub

In order to use Azure Event Hub data integration, a Namespace and Event Hub must be set up in the Azure account.  The following links to the official documentation provide details on how to do the setup.

- [Quickstart: Create an event hub using Azure portal](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [Quickstart: Stream data with Azure Event Hubs and Apache Kafka](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - Follow the "Connection String" instructions, as that's what EMQX uses for connecting.
- [Get an Event Hubs connection string](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [What is Azure Event Hubs for Apache Kafka](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

### Create Azure Event Hub Data Bridge

This section demonstrates how to create Azure Event Hub producer data bridge via Dashboard.

1. Go to EMQX Dashboard, and click **Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **Azure Event Hubs**, and then click **Next**.

   :::: tabs type:card

   ::: tab Configure as Producer Role

   - Fill in the required fields (marked with an asterisk).

   - Input a name for the data bridge. The name should be a combination of upper/lower case letters and numbers.

   - Input the connection information. Input the Host Name for your Namespace in the **Bootstrap Hosts** field.  The default port is 9093.  For the other fields set as the actual condition.

   - Input the Connection String for the Namespace.  It can be found in the "connection string-primary key" of a Namespace shared access policy.

   - **Event Hub Name**: Input the name of the Event Hub to be used. Note: Variables are not supported here.

   - **Message Key**: Event hub message key. Insert a string here, either a plain string or a string containing placeholders (${var}).

   - **Message Value**: Event hub message value. Insert a string here, either a plain string or a string containing placeholders (${var}).

   - **TLS**: ensure TLS is enabled when connecting to Azure Event Hub.

   - Advanced settings (optional): Set the **Max Batch Bytes**, **Compression**, and **Partition Strategy** as your business needs.

   :::

   ::::

5. Before clicking **Create**, you can click **Test Connection** to test that the bridge can connect to the Azure Event Hub server.

6. Click **Create**, you'll be offered the option of creating an associated rule.

   - For the Azure Event Hub producer data bridge, click **Create Rule** to create an associated rule. For detailed operating steps, see [Create Rule for Azure Event Hub Producer Data Bridge](#create-rule-for-azure-event-hub-producer-data-bridge).

::: tip

Creating a rule allows Azure Event Hub messages matching the rule to be further transformed and filtered if needed, and then forwarded to other rule actions, like different bridges. Refer to the [Rules](./rules.md) for more information on creating rules.

:::

Now the Azure Event Hub data bridge should appear in the data bridge list (**Integration** -> **Data Bridge**) with **Resource Status** as **Connected**.

### Create Rule for Azure Event Hub Producer Data Bridge

1. Go to EMQX Dashboard, and click **Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input, for example, `my_rule` as the rule ID.

4.  Input the following statement in the **SQL Editor** if you want to save the MQTT messages under topic `t/#` to Azure Event Hub.

   Note: If you want to specify your own SQL syntax, make sure that you have included all fields required by the data bridge in the `SELECT` part.

```sql
SELECT
  *
FROM
  "t/#"
```

5. Click the **Add Action** button, select **Forwarding with Data Bridge** from the dropdown list and then select the data bridge you just created under **Data bridge**. Then click the **Add** button.

6. Click **Create** at the page bottom to finish the creation.

Now you have successfully created the data bridge to Azure Event Hub producer data bridge. You can click **Integration** -> **Flows** to view the topology. It can be seen that the messages under topic `t/#`  are sent and saved to Azure Event Hub after parsing by rule  `my_rule`.

### Test the Data Bridge and Rule

 Use MQTTX to send messages to topic  `t/1`:

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

Check the running status of the data bridge, there should be one new outgoing message.

Check whether messages are written into the configured Event Hub in the Azure Portal dashboard.
