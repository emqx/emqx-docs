# Shared Subscription

EMQX implements the shared subscription feature of MQTT. A shared subscription is a subscription mode to implement load balancing among multiple subscribers. Clients can be divided into multiple subscription groups, and messages are still forwarded to all subscription groups, but only one client within each subscription group receives the message at a time. You can add a `$share` prefix to the original topic to enable shared subscriptions for a group of subscribers.

You can use client tools to connect to EMQX and try this messaging service. This section introduces how to use the [MQTTX Client](https://mqttx.app/) and [MQTTX CLI](https://mqttx.app/cli) to simulate clients and try how messages are received through a shared subscription.

:::tip Prerequisites

- Knowledge about MQTT [Shared Subscription](./mqtt-concepts.md#shared-subscription)
- Basic publishing and subscribing operations using [MQTTX](./publish-and-subscribe.md)

:::

## Try Shared Subscription with MQTTX Client

The following procedure demonstrates how to form groups for multiple subscribers that can share the subscription to the same topic and how these subscribers will receive the messages from the shared subscription.

In this demonstration, you can create one client connection `demo` as a publisher to publish messages to the topic `t/1`. Then, you can create 4 client connections as subscribers, such as `Subscriber1`, `Subscriber2`, `Subscriber3`, and `Subscriber4`.  The subscribers can be divided into groups `a` and `b`, and both groups subscribe to the topic `t/1`.

1. Start EMQX and MQTTX Client. Click the **New Connection** to create a client connection as a publisher.

   - Enter `Demo` in the **Name** field.
   - Enter the localhost `127.0.0.1` in **Host** to use as an example in this demonstration.
   - Leave other settings as default and click **Connect**.

   ::: tip

   More detailed instructions on creating an MQTT connection are introduced in [MQTTX Client](./publish-and-subscribe.md#mqttx-client).

   :::

   <img src="./assets/Configure-new-connection-general.png" alt="Configure-new-connection-general" style="zoom:35%;" />

1. Click the **New Connection** to create 4 new connections as subscribers. Set **Name** set to `Subscriber1`, `Subscriber2,` `Subscriber3`, and `Subscriber4` respectively.

3. Select the `Subscriber` connections one at a time in the **Connections** pane and click **New Subscription** to create a shared subscription for each subscriber. Enter the correct topic in the **Topic** text box by referring to the rules below.

   To form a group for multiple subscribers, you need to add group name `{group}` before the subscribed topic `t/1`. To make them all subscribe to the same topic, you need to add the prefix `$share` before the group name.

   In the **New Subscription** window:

   - Set the **Topic** to `$share/a/t/1` for `Subscribe1` and `Subscriber2`.
   - Set the **Topic** to `$share/b/t/1` for `Subscriber3` and `Subscriber4`.

   In these example topics:

   - The prefix `$share` indicates this is a shared subscription.
   - `{group}` is `a` and `b`, but it can be any customized name.
   - `t/1` indicates the original topic name.

   Leave other settings as default. Click the **Confirm** button.

   <img src="./assets/New-shared-subscription.png" alt="New-shared-subscription" style="zoom:35%;" />

5. Click the connection `Demo` you created before.

   - Send a message with the topic `t/1`. The client `Subscriber1` in group `a` and `Subscriber4` in gourd `b` should receive the message.

     <img src="./assets/Receive-message-shared-subscription1.png" alt="Receive-message-shared-subscription1" style="zoom:35%;" />

   - Send the same message again. The client `Subscriber2` in group `a` and `Subscriber3` in group `b` should receive the message.

     <img src="./assets/Receive-message-shared-subscription2.png" alt="Receive-message-shared-subscription2" style="zoom:35%;" />

:::tip

When the message of the shared subscription is published, the EMQX forwards the message to different groups at the same time, but only one of the subscribers in the same group receives the message at a time.

:::

## Try Shared Subscription with MQTTX CLI

1. Four subscribers are divided into 2 groups and subscribe to topic  `t/1`:

   ```bash
   # Client A and B subscribe to topic `$share/my_group1/t/1`
   mqttx sub -t '$share/my_group1/t/1' -h 'localhost' -p 1883

   ## Client C and D subscribe to topic  `$share/my_group2/t/1`
   mqttx sub -t '$share/my_group2/t/1' -h 'localhost' -p 1883
   ```

2. Use a new client to publish 4 messages with payloads `1`, `2`, `3`, and `4` to the original topic `t/1`:

   ```bash
   mqttx pub -t 't/1' -m '1' -h 'localhost' -p 1883
   mqttx pub -t 't/1' -m '2' -h 'localhost' -p 1883
   mqttx pub -t 't/1' -m '3' -h 'localhost' -p 1883
   mqttx pub -t 't/1' -m '4' -h 'localhost' -p 1883
   ```

3. Check the message received by the clients within each subscription group:

   - Subscription group 1 (A and B) and subscription group 2 (C and D) simultaneously receive the messages.
   - Only one of the subscribers in the same group receives the message at a time.
