# Retained Messages

MQTT Retained messages in EMQX is handled by a component named 'retainer'.
This name is often used in EMQX documents and other technical posts.

## Introduction

When the broker receives a PUBLISH packet with a Retain flag of 1, it will treat the message as a retained message.
In addition to being normally forwarded, the retained message will be stored on the server.
There can only be one (and only one) retained message under each topic.
Therefore, if there is already a retained message of the same topic, the previously retained message is replaced with the new one.

When a client establishes a subscription, and there are retained messages with matching topics in the broker,
these retained messages will be sent to the client immediately.

With retained messages, new subscribers can immediately get the latest status without waiting for an undetermined amount of time,
which is very important in many scenarios. For instance sensors may only publish readings every few minutes, but the
subscribers may need get the latest reading immediately after subscribed, without having to wait for the next publish.

In EMQX, the retainer is enabled by default. To turn it off, you can set `mqtt.retain_available = false` globally for the entire broker
or `zones.$name.mqtt.retain_available = false` to turn it off only for a given config group.

::: warning
If EMQX receives a retained message when the feature is turned off,
it will reply a DISCONNECT message with reason code 0x9A (retained message is not supported).
:::

## Flow Control

The message read and deliver rate can be controlled.
When a client subscribes to a wildcard topic, it may match a large number of topics having messages retained.
Without flow control, the all matched messages will be copied into the subscriber's process memory space,
this may cause the subscriber Erlang process (the actor) to allocate excessive amount of RAM and risk at
shutdown forced by the `force_shutdown` policy.

To make it less aggressive, `retainer.flow_control` settings can be used.
The processing flow is as follows:

1. Load `batch_read_number` of retained message from the retainer storage
1. Deliver `batch_deliver_number` of messages
1. Repeat, until all retained messages are delivered

You may find more detailed information from configuration documents.
