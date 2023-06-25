# Retained Messages

With `Retained` message, you can flag one message published under certain topic as `Retained` and save them as persistent messages on the broker. So when new clients subscribe to this topic, they will receive the messages flagged as `Retained`.

Only one retained message can exist under each topic, if you want to flag another message as `Retained`, the existing message will be replaced by the new message.

For example, use MQTTX CLI to publish a `Retained` message to topic `a/b/c`:

```bash
mqttx pub -r -q 1 -t a/b/c -m 'hello'
```

When new MQTT clients subscribe to topic `a/b/c`, they can still receive the `Retained` message:

```bash
$ mqttx sub -t a/b/c -q 1
payload:  hello
```

Although the retained message is stored on the broker, they are not part of the session. That is, the `Retained` message will be kept even if the session that published this message is closed.

You can use either of the following methods to delete a `Retained` message:

1. Use the client to publish a black message to the topic with `Retained` message:

```bash
mqttx pub -r -q 1 -t a/b/c -m ''
```

2. Number of `Retained` messages exceeds the specified maximum `Retained` messages.
3. Use REST API to delete the `Retained` message.
4. For MQTT 5.0 compatible clients, set an expiration duration, then the `Retained` messages will be deleted if the duration expired.

::: tip

For more information, you may read [*The Beginner's Guide to MQTT Retained Messages*](https://www.emqx.com/en/blog/mqtt5-features-retain-message).

:::

By default, the Retained message feature is enabled on EMQX. You can modify `mqtt.retain_available` to `false` in `etc/emqx.conf` to disable the feature.
In this way, the client will be prohibited from sending PUBLISH packets with the Retain flag set to 1. Otherwise, the client will receive a DISCONNECT packet with a reason code of 0x9A (Retain not supported).

The service stores and manages retained messages sent by clients and sends them to the corresponding subscribers.

## Configure With Dashboard

Open the Dashboard, click **Management** -> **MQTT Settings** on the left navigation menu. Then click the **Retainer** tab, and you can customize the setting for Retained messages. 

<img src="./assets/retainer_1.png" alt="image-20230427113953764" style="zoom:50%;" />

Below are detailed descriptions of each field

| Configuration item       | Type  | Optional value      | Default value | Description                                               |
| ------------------------------ | -------- | ------------------------ | ------ | ------------------------------------------------------------ |
| Storage Type | - | Built-in Database | - | - |
| Storage Method | enum     | `ram`, `disc` | `ram` |`ram`: only stored in memory; <br/><br/> `disc`: stored in memory and hard disk. |
| Max Retained Messages | integer  | ≥ 0                   | 0 (Unlimited) | 0: Unlimit. <br>When you set a limit on the maximum number of retained messages, EMQX replaces existing messages once the limit is reached. However, you cannot store retained messages for new topics beyond the limit. |
| Max Payload Size      | bytesize |                          | 1MB    | Retain the maximum Payload value of the message. After the Payload value exceeds the maximum value, the EMQX will treat the retained reserved message as a normal message. |
| Expire       | duration |                          | 0    | The expiration time of the retained message, and 0 means never expire. If the message expiration interval is set in the PUBLISH packet, the message expiration interval in the PUBLISH packet shall prevail. |
| Clean Interval  | duration |                          | 0    | Interval to clean up expired messages. |

## Delivery Rate Limit

The retained message delivery rate can be controlled.

When a client subscribes to a wildcard topic, it may match a large number of topics having messages retained.
Without a limit, all matched messages will be copied into the subscriber's process memory space,
this may cause the subscriber Erlang process (the actor) to allocate an excessive amount of RAM and bring the risk of
forced shutdown following the `force_shutdown` policy.

To make it less aggressive, `retainer.delivery_rate` settings can be used, e.g:

```bash
# Each session subscribed to retain messages will get 1000 messages per second at the most
retainer.delivery_rate = "1000/s"
```


