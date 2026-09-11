# In-flight and Message Queue

## Introduction

To improve message throughput and reduce the impact of network fluctuations, EMQX allows multiple unacknowledged QoS 1 and QoS 2 messages in-flight at the same time. These sent but unconfirmed messages will be stored in the Inflight Window until acknowledgment is complete.

If the number of in-flight QoS 1 and QoS 2 messages reaches the maximum limit of the Inflight Window (see `max_inflight`), newly arrived messages are not forwarded immediately but are temporarily stored in the Message Queue.

When delivery resumes, EMQX dequeues messages in FIFO order if topic priorities are disabled. If topic priorities are enabled, EMQX schedules messages according to their topic priority and preserves FIFO order within each priority. Delivered QoS 1 and QoS 2 messages are added to the Inflight Window, while delivered QoS 0 messages do not enter the Inflight Window.

QoS 0 messages normally bypass the Inflight Window and are forwarded immediately. For in-memory sessions, EMQX puts subsequent QoS 0 messages in the Message Queue if the connection's send queue becomes congested. If the Inflight Window is full and `mqueue_store_qos0` is enabled, EMQX also queues QoS 0 messages so that they follow normal queue scheduling instead of bypassing the queue. EMQX resumes queued delivery after the connection recovers or space becomes available in the Inflight Window.

When the Message Queue for a topic priority reaches the length limit, EMQX first evicts the oldest QoS 0 message at that priority. If no QoS 0 message exists at that priority, EMQX evicts the oldest remaining message at that priority. When topic priorities are disabled, all messages share the same priority. This policy helps QoS 1 and QoS 2 messages make progress during QoS 0 bursts. Therefore, it is important to set a suitable Message Queue length limit. See `max_mqueue_len`.

The Message Queue also stores messages that arrive while the subscriber is offline but the session remains. EMQX delivers these messages when the subscriber reconnects. Set `mqueue_store_qos0` to `false` to exclude QoS 0 messages from offline storage.

Note that the Inflight Window and Message Queue are not global. EMQX will allocate a separate Inflight Window and Message Queue for each client connection.

## Inflight Window and Receive Maximum

The MQTT v5 protocol adds a `Receive Maximum`  attribute to CONNECT packets, and the official explanation for it is:

> The client uses this value to limit the maximum number of published messages with a QoS of1 and a QoS of 2 that the client is willing to process simultaneously. There is no mechanism to limit the published messages with a QoS of 0 that the server is trying to send.

That is, the server can send subsequent PUBLISH packets to the client with different message identifiers while waiting for acknowledgment, until the number of unacknowledged messages reaches the `Receive Maximum` limit.

It is not difficult to see that `Receive Maximum` is actually the same as the Inflight Window mechanism in EMQX. However, EMQX already provided this function to the accessed MQTT client before the MQTT v5.0 protocol was released. Now, the clients using the MQTT v5.0 protocol will set the maximum length of the Inflight Window according to the specification of the Receive Maximum, while clients with earlier versions of the MQTT protocol will still set it according to the configuration.

However, EMQX does not necessarily grant the `Receive Maximum` value requested in the CONNECT packet. Instead, the `Receive Maximum` granted in the CONNACK packet is capped by the `mqtt.max_inflight` configuration.

## Configuration Items

| Configuration Items    | Type    | Optional Value  | Default Value | Description                                                  |
| ---------------------- | ------- | --------------- | ------------- | ------------------------------------------------------------ |
| mqtt.max_inflight      | integer | (0, 65536)      | 32            | Inflight Window length limit, 0 means no limit               |
| mqtt.max_mqueue_len    | integer | [0, ∞)          | 1000          | Message Queue length limit. `0` means no limit.              |
| mqtt.mqueue_store_qos0 | enum    | `true`, `false` | true          | Whether EMQX stores QoS 0 messages in the Message Queue when the client is offline, an in-memory connection's send queue is congested, or the Inflight Window is full. |
