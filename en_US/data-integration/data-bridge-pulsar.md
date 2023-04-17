# Stream Data into Apache Pulsar

[Apache Pulsar](https://pulsar.apache.org/) is a popular open-source
distributed event streaming platform. EMQX's integration with Apache
Pulsar offers users dependable data transport and processing
capabilities in high-throughput situations.

Streaming data into Apache Pulsar involves creating data bridges to
Pulsar in the producer role (sends messages to Pulsar).

{% emqxce %}
:::tip
EMQX Enterprise Edition features. EMQX Enterprise Edition provides
comprehensive coverage of key business scenarios, rich data
integration, product-level reliability, and 24/7 global technical
support. Experience the benefits of this [enterprise-ready MQTT
messaging platform](https://www.emqx.com/en/try?product=enterprise)
today.
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

This section introduces how to stream data into Pulsar, covering
topics like how to set up a Pulsar server, how to create a bridge and
a rule for forwarding data to the bridge and how to test the data
bridge and rule.

This tutorial assumes that you run both EMQX and Pulsar on the local
machine. If you have Pulsar and EMQX running remotely, please adjust
the settings accordingly.

### Install Pulsar

Run Pulsar in Docker.

```bash
docker run --rm -it -p 6650:6650 --name pulsar apachepulsar/pulsar:2.11.0 bin/pulsar standalone -nfw -nss
```

For detailed operation steps, you may refer to the [Quick Start
section in Pulsar
Documentation](https://pulsar.apache.org/docs/2.11.x/getting-started-home/).

### Create Pulsar Topics

Relevant Pulsar topics should be created before creating the data
bridge in EMQX. Use the commands below to create a topic called
`my-topic` in Pulsar under the `public` tenant, `default` namespace,
with 1 partition.

```bash
docker exec -it pulsar bin/pulsar-admin topics create-partitioned-topic persistent://public/default/my-topic -p 1
```

### Create Pulsar Data Bridge

This section demonstrates how to create a Pulsar producer data bridge
via the Dashboard.

1. Go to EMQX Dashboard, and click **Data Integration** -> **Data Bridge**.

2. Click **Create** on the top right corner of the page.

3. In the **Create Data Bridge** page, click to select **Pulsar**, and
   then click **Next**.

4. In **Bridge Role** field, select **Producer**. Click the
   corresponding tabs for the configuration of each role.

   :::: tabs type:card

   ::: tab Configure as Producer Role

   - Fill in the required fields (marked with an asterisk).

   - Input a name for the data bridge. The name should be a
     combination of upper/lower case letters and numbers.

   - Input the connection information. Input `pulsar://localhost:6650`
     for the **Servers**. For the other fields set as the actual
     condition.

   - **Source MQTT Topic**: Set the MQTT topics to create the data
     bridge. In this example, it is set to `t/#`, indicating all MQTT
     messages matching this topic will be sent to Pulsar. You can also
     leave it blank, and create a
     [rule](#create-rule-for-pulsar-producer-data-bridge) to specify
     data to be sent to Pulsar.

   - **Pulsar Topic Name**: Input
     `persistent://public/default/my-topic` (the Pulsar topic created
     before). Note: Variables are not supported here.

   - **Message Key**: Pulsar message key. Insert a string here, either
     a plain string or a string containing placeholders (${var}).

   - **Message Value**: Pulsar message value. Insert a string here,
     either a plain string or a string containing placeholders
     (${var}).

   - Advanced settings (optional): Set the **Max Batch Bytes**,
     **Compression**, and **Partition Strategy** as your business
     needs.

   :::

   ::::

5. Before clicking **Create**, you can click **Test Connection** to
   test that the bridge can connect to the Pulsar server.

6. Click **Create**, you'll be offered the option of creating an
   associated rule.

   - For the Pulsar producer data bridge, click **Create Rule** to
     create an associated rule. For detailed operating steps, see
     [Create Rule for Pulsar Producer Data
     Bridge](#create-rule-for-pulsar-producer-data-bridge).

::: tip

Creating a rule allows Pulsar messages matching the rule to be further
transformed and filtered if needed, and then forwarded to other rule
actions, like different bridges. Refer to the [Rules](./rules.md) for
more information on creating rules. The MQTT topics defined in
**Source MQTT Topic** will start having messages published to them
without further configuration.

:::

Now the Pulsar data bridge should appear in the data bridge list
(**Data Integration** -> **Data Bridge**) with **Resource Status** as
**Connected**.

### Create Rule for Pulsar Producer Data Bridge

1. Go to EMQX Dashboard, and click **Data Integration** -> **Rules**.

2. Click **Create** on the top right corner of the page.

3. Input, for example, `my_rule` as the rule ID.

4. Input the following statement in the **SQL Editor** if you want to
   save the MQTT messages under topic `t/#` to Pulsar.

   Note: If you want to specify your own SQL syntax, make sure that
   you have included all fields required by the data bridge in the
   `SELECT` part.

```sql
SELECT
  *
FROM
  "t/#"
```

5. Click the **Add Action** button, select **Forwarding with Data
   Bridge** from the dropdown list and then select the data bridge you
   just created under **Data bridge**. Then click the **Add** button.

6. Click **Create** at the page bottom to finish the creation.

Now you have successfully created the data bridge to Pulsar producer
data bridge. You can click **Data Integration** -> **Flows** to view
the topology. It can be seen that the messages under topic `t/#` are
sent and saved to Pulsar after parsing by rule `my_rule`.

### Test the Data Bridge and Rule

 Use MQTTX to send messages to topic  `t/1`:

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Pulsar" }'
```

Check the running status of the two data bridges, there should be one
new incoming and one new outgoing message.

Check whether messages are written into the topic
`persistent://public/default/my-topic` with the following Pulsar
command:

   ```bash
   docker exec -it pulsar bin/pulsar-client consume -n 0 -s mysubscriptionid -p Earliest persistent://public/default/my-topic
   ```
