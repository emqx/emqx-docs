# Exclusive Subscription

An exclusive subscription is an extended MQTT feature supported by EMQX. It allows mutually exclusive subscriptions to topics. Only one subscriber is allowed to subscribe to a topic at a time. Other subscribers will not be able to subscribe to the corresponding topic until the current subscriber unsubscribe from the subscription.

To make a subscription exclusive, you need to add a prefix to the topic heading. The table below shows an example:

| Example | Prefix | Real Topic Name |
| --------------- | ----------- | ------------ |
| $exclusive/t/1 | $exclusive/ | t/1 |

When client **A** subscribes to `$exclusive/t/1`, other clients will fail to subscribe to `$exclusive/t/1` until **A** cancels the subscription to `$exclusive/t/1`.

::: tip

Exclusive subscriptions must be prefixed with `$exclusive/`, in the above example, other clients can still successfully subscribe via `t/1`.

:::

## Configure Exclusive Subscription via Configuration File

::: tip 

Exclusive subscription can be configurated via the configuration file only. Configuration using the Dashboard is currently not supported.

:::

The exclusive subscription is disabled by default. You can enable this feature in `etc/emqx.conf`.

```bash
mqtt.exclusive_subscription {
    enable = true
}
```

## Try Exclusive Subscription with MQTTX Desktop

::: tip Prerequisites

- Basic publishing and subscribing operations using [MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop)
- Exclusive subscription is enabled.

:::

1. Start EMQX and MQTTX Desktop. Click the **New Connection** to create a client connection as a publisher.

   - Enter `Demo` in the **Name** field.
   - Enter the localhost `127.0.0.1` in **Host** to use as an example in this demonstration.
   - Leave other settings as default and click **Connect**.

   ::: tip

   More detailed instructions on creating an MQTT connection are introduced in [MQTTX Desktop](./publish-and-subscribe.md#mqttx-desktop).

   :::

   <img src="./assets/Configure-new-connection-general.png" alt="Configure-new-connection-general" style="zoom:35%;" />

2. Create another 2 MQTT connections. Configure them as `Subscriber1` and `Subscriber2` respectively.

3. Select the connection named `Subscriber1` in the **Connections** pane. Click the **New Subscription** button to create a subscription.  Type `$exclusive/t/1` in the **Topic** text box to subscribe to this topic. Click **Confirm**.

   <img src="./assets/subscribe-exclusive-topic.png" alt="subscribe-exclusive-topic" style="zoom:35%;" />

4. Select the connection named `Subscriber2` in the **Connections** pane. Click the **New Subscription** button to create a subscription.  Type `$exclusive/t/1` in the **Topic** text box to subscribe to this topic. Click **Confirm**.

   - An error message pops up.

   <img src="./assets/fail-to-exclusive-subscription.png" alt="fail-to-exclusive-subscription" style="zoom:35%;" />

## Try Exclusive Subscription with MQTTX CLI

::: tip Prerequisites

- Basic publishing and subscribing operations using [MQTTX CLI](./publish-and-subscribe.md#mqttx-cli)
- Exclusive subscription is enabled.

:::

1. Use the following command to make an exclusive subscription.

   ```bash
   mqttx sub -t "$exclusive/t/1"
   ```

2. Use the command in step 1 again to make another subscription to the topic `$exclusive/t/1`. It will return:

   ```bash
   subscription negated to t/2 with code 135
   ```

   Error codes of exclusive subscription:

   | Code | Reason                                                    |
   | ---- | --------------------------------------------------------- |
   | 0x8F | Use `$exclusive/` without exclusive subscription enabled. |
   | 0x97 | A client has already subscribed to this topic.            |

