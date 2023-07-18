# Will Message

EMQX implements the will message feature of MQTT. If a will message is set for a client, EMQX sends the message to relevant subscribers when the client is accidentally disconnected, so that the subscribers can be informed and update the client status.

You can use client tools to try this messaging service in EMQX. This section introduces how to use the [MQTTX Desktop](https://mqttx.app/) and [MQTTX CLI](https://mqttx.app/cli) to simulate clients and see how a will message is published and received.

:::tip Prerequisites

- Knowledge about MQTT [Will Message](./mqtt-concepts.md#will-message)
- Basic publishing and subscribing operations using [MQTTX](./publish-and-subscribe.md)

:::

## Publish Will Message with MQTTX Desktop

1. Start EMQX and MQTTX Desktop. Click the **New Connection** to create a client connection as a publisher.

   - Enter `Demo` in the **Name** field.
   - Enter the localhost `127.0.0.1` in **Host** to use as an example in this demonstration.
   - Leave other settings as default and click **Connect**.

   ::: tip

   More detailed instructions on creating an MQTT connection are introduced in [MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop).

   :::

   <img src="./assets/Configure-new-connection-general.png" alt="Configure-new-connection-general" style="zoom:35%;" />

   Scroll down the page and in **Last Will and Testament** section, fill in the will message configuration.

   - **Last-Will Topic**: Enter `offline`.
   - **Last-Will QoS**: Set as the default value `0`.
   - **Last-Will Retain**: Set disabled as default. If enabled, the will message will also be a retained message.
   - **Last-Will Payload**: Enter `I'm offline`.
   - **Will Delay Intervals (s)**: Set `5` seconds.

   Leave the rest settings as default. Click the **Connect** button.

   <img src="./assets/Configure-new-connection-will.png" alt="Configure-new-connection-will" style="zoom:35%;" />

2. In the **Connections** pane, click **+** -> **New Connection** to create a new client connection. Set the connection **Name** as `Subscriber` and **Host** as `127.0.0.1`. Leave other settings as default and click **Connect**.

3. Click **New Subscription** in the **Subscriber** pane. Enter `offline` in the **Topic** textbox. Leave the other settings as default. Click the **Confirm** button.

   <img src="./assets/Subscribe-will-message.png" alt="Subscribe-will-message" style="zoom:35%;" />

4. Select the client connection named `Demo` in the **Connections** pane. Right-click and select **New Window**. In the new window, click the **Connect** button.

   <img src="./assets/Open-new-window.png" alt="Open-new-window" style="zoom:35%;" />

5. Close the new window and wait for 5 seconds. The client `Subscriber` receives a will message `I'm offline`.

   <img src="./assets/Receive-will-message.png" alt="Receive-will-message" style="zoom:35%;" />



## Publish Will Message with MQTTX CLI

1. Initiate a connection request with one client. Set the topic to `t/1` and payload to `A will message from MQTTX CLI`:

   ```bash
   $ mqttx conn -h 'localhost' -p 1883 --will-topic 't/1' --will-message 'A will message from MQTTX CLI'
   Connected
   ```

2. Subscribe to topic `t/1` with another client for receiving the will messages:

   ```bash
   mqttx sub -t 't/1' -h 'localhost' -p 1883 -v
   ```

3. Disconnect the client specified in step 1, then the client specified in step 2 will receive the will message:

   ```bash
   topic:  t/1
   payload:  A will message from MQTTX CLI
   ```

