# Retained Messages

With `Retained` message, you can flag one message published under certain topic as `Retained` and save them as persistent messages on the broker. So when new clients subscribe to this topic, they will receive the messages flagged as `Retained`.

Only one retained message can exist under each topic, if you want to flag another message as `Retained`, the existing message will be replaced by the new message. 

For example, use MQTT X CLI to publish a `Retained` message to topic `a/b/c`:

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

Open the Dashboard, click **Configuration** -> **MQTT** on the left navigation tree. Then click the **Retainer** tab, and you can customize the setting for Retained messages. 

![image](./assets/retainer_1.png)

## Configuration Items

| Configuration item       | Type  | Optional value      | Default value | Description                                               |
| ------------------------------ | -------- | ------------------------ | ------ | ------------------------------------------------------------ |
| Storage         | enum     | `ram`, `disc` | ram |ram: only stored in memory; <br /> disc: stored in memory and hard disk. |
| Max Retained Messages | integer  | \>= 0                    | 0      | The maximum number of retained messages, and 0 means no limit. After the number of retained messages exceeds the maximum limit, you can replace the existing retained messages, but cannot store retained messages for new topics. |
| Max Payload Size      | bytesize |                          | 1MB    | Retain the maximum Payload value of the message. After the Payload value exceeds the maximum value, the EMQX will treat the retained reserved message as a normal message. |
| Expire       | duration |                          | 0    | The expiration time of retaining message, and 0 means never expire. If the message expiration interval is set in the PUBLISH packet, the message expiration interval in the PUBLISH packet shall prevail. |
| Clean Interval  | duration |                          | 0    | Interval to clean up expired messages. |

## Flow Control

The message read and delivery rate can be controlled.

When a client subscribes to a wildcard topic, it may match a large number of topics having messages retained.
Without flow control, all matched messages will be copied into the subscriber's process memory space,
this may cause the subscriber Erlang process (the actor) to allocate excessive amount of RAM and bring the risk of
forced shutdown following the `force_shutdown` policy.

To make it less aggressive, `retainer.flow_control` settings can be used, e.g:

```bash
# Each session subscribed to retain messages will load 10 messages and deliver 10 messages at each time, the total delivery rate of all these sessions is limited to 100/s, and the dispatch rate of each worker process in the retained module is limited to 20/s (in most cases, it is not necessary to configure the client level)
retainer {
  enable = true
  flow_control {
    batch_read_number = 10
    batch_read_deliver = 10
    batch_read_limiter {
      rate = "100/s"
      capacity = 100
      client {
        rate = "20/s"
        capacity = 20
      }
    }
  }
}
```

Configuration items:

| Configuration Items   | Type    | Default   | Description                                           |
| :-------------------- | :------ | :-------- | :---------------------------------------------------- |
| batch_read_number     | int     | 0         | number of messages to read each time (0 means all)    |
| batch_deliver_number  | int     | 0         | number of messages to deliver each time (0 means all) |
| batch_deliver_limiter | limiter | undefined | message delivery rate limiter                         |

For detailed settings of rate limiter, please see the `Listener Level` and `Connection Level` in [Hierarchical Rate Limiter](../rate-limit/rate-limit.md)
