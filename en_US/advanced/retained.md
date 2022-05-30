# Retained

## Introduction

When the server receives a PUBLISH packet with a Retain flag of 1, it will treat the message as a retained message. In addition to being normally forwarded, the retained message will be stored on the server. There can only be one retained message under each topic. Therefore, if there is already a retained message of the same topic the previously retained message is replaced with the new one.

When a client establishes a subscription, and there are retained messages with matching topics on the server, these retained messages will be sent to the client immediately. With retained messages new subscribers can immediately get the latest status without waiting for an undetermined amount of time, which is very important in many scenarios.

EMQX Broker enables the of retaining messages by default. You can modify `mqtt.retain_available` to `false` in  `etc/emqx.conf` to disable retaining messages. If the EMQX Broker receives a retained message when the feature is disabled it will return the DISCONNECT packet with a reason code of 0x9A (retained message is not supported).

## Configuration

EMQX Broker's retained message feature is implemented by the `emqx_retainer` plugin, which is enabled by default. By modifying the configuration of the ` emqx_retainer` plugin, you can adjust the storage location of the retained messages in EMQX Broker, adjust the number of preserved retained messages and the maximum length of Payload, and adjust the expiration time of retained messages. For more information about the EMQX Broker plug-in, see [plugin](./plugins.md).

The `emqx_retainer` plugin is enabled by default, and the configuration path of the plugin is `etc/plugins/emqx_retainer.conf`.

| Configuration item       | Type  | Optional value      | Default value | Description                                               |
| ------------------------------ | -------- | ------------------------ | ------ | ------------------------------------------------------------ |
| retainer.storage_type          | enum     | `ram`, `disc`, `disc_only` | ram |ram: only stored in memory; <br /> disc: stored in memory and hard disk; <br /> disc_only: only stored in hard disk|
| retainer.max_retained_messages | integer  | \>= 0                    | 1      | The maximum number of retained messages, and 0 means no limit. After the number of retained messages exceeds the maximum limit, you can replace the existing retained messages, but cannot store retained messages for new topics. |
| retainer.max_payload_size      | bytesize |                          | 1MB    | Retain the maximum Payload value of the message. After the Payload value exceeds the maximum value, the EMQX broker will treat the retained reserved message as a normal message. |
| retainer.expiry_interval       | duration |                          | ０     | The expiration time of retaining message, and 0 means never expire. If the message expiration interval is set in the PUBLISH packet, the message expiration interval in the PUBLISH packet shall prevail. |

::: tip
EMQX Enterprise can store retained messages in various external databases
:::
