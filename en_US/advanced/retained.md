# Retained

## Introduction

When the server receives a PUBLISH packet with a Retain flag of 1, it will treat the message as a retained message. In addition to being normally forwarded, the retained message will be stored on the server. There can only be one retained message under each topic. Therefore, if there is already a retained message of the same topic the previously retained message is replaced with the new one.

When a client establishes a subscription, and there are retained messages with matching topics on the server, these retained messages will be sent to the client immediately. With retained messages new subscribers can immediately get the latest status without waiting for an undetermined amount of time, which is very important in many scenarios.

EMQX Broker enables the of retaining messages by default. You can modify `mqtt.retain_available` to `false` in  `etc/emqx.conf` to disable retaining messages. If the EMQX Broker receives a retained message when the feature is disabled it will return the DISCONNECT packet with a reason code of 0x9A (retained message is not supported).

## Flow control

The message read and deliver rate can be controlled. When a client subscribes to a wildcard topic, many retained messages may be loaded. If you don't want these message to be loaded into the memory all at once, you can use `retainer.flow_control` options to control this behaviour. The processing flow is as follows:
1. Load `batch_read_number` of retained message from storage
2. Deliver `batch_deliver_number` of messages
3. Repeat, until all retained messages are delivered

## Configuration

EMQX has the retainer enabled by default. By modifying the configuration in the `retainer` namespace, you can adjust the storage location of the retained messages in EMQX Broker, adjust the number of preserved retained messages and the size limit of the messages, and adjust the expiration time of retained messages.

The `emqx_retainer` plugin is enabled by default, and the configuration path of the plugin is `etc/plugins/emqx_retainer.conf`.

| Configuration item                         | Type     | Possible values             | Default value | Description                                                                                                                                                                                                                                                  |
| ------------------------------------------ | -------- | --------------------------  | ------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------                           |
| retainer.enable                            | boolean  | `false`, `true`             | `true`        | Enables retainer plugin                                                                                                                                                                                                                                      |
| retainer.msg_clear_interval                | time     |                             | `0s`          | Periodic interval for cleaning up expired messages, and 0 means never expire.                                                                                                                                                                                |
| retainer.msg_expiry_interval               | time     |                             | `0s`          | Message retention time. 0 means message will never be expired. If the message expiration interval is set in the PUBLISH packet, the message expiration interval in the PUBLISH packet shall prevail.                                                         |
| retainer.max_payload_size                  | bytesize |                             | `1MB`         | Retain messages with a Payload size less than the configured value. If the bytesize of the Payload exceeds the maximum value, the EMQX broker will treat the retained reserved message as a normal message.                                                  |
| retainer.stop_publish_clear_msg            | boolean  | `false`, `true`             | `false`       | When the retained flag of the PUBLISH message is set and Payload is empty, whether to continue to publish the message.                                                                                                                                       |
| retainer.flow_control.batch_read_number    | integer  |                             | 0             | How many messages to read from storage in a single batch. 0 means no limit.                                                                                                                                                                                  |
| retainer.flow_control.batch_deliver_number | integer  | from 0 to 1000              | 0             | The number of retained messages that can be delivered in one batch. Note that if this value is too big, it may cause performance issues.                                                                                                                     |
| retainer.backend.storage_type              | enum     | `ram`, `disc`               | `ram`         | ram: only stored in memory; <br /> disc: stored in memory and hard disk.                                                                                                                                                                                     |
| retainer.backend.max_retained_messages     | integer  | \>= 0                       | 1             | The maximum number of retained messages, and 0 means no limit. After the number of retained messages exceeds the maximum limit, you can replace the existing retained messages, but cannot store retained messages for new topics.                           |
