# Inflight Window and Message Queue

EMQX uses an Inflight Window and a Message Queue to improve message throughput and reduce the impact of network fluctuations. EMQX maintains these structures separately for each client connection.

- **Inflight Window**: Holds sent but unacknowledged QoS 1 and QoS 2 messages until EMQX receives the corresponding acknowledgments. EMQX can keep multiple messages in flight at the same time, and `max_inflight` limits their number. For MQTT 5.0 clients, EMQX limits the number of concurrently unacknowledged QoS 1 and QoS 2 messages to the smaller of `max_inflight` and the `Receive Maximum` reported by the client.
- **Message Queue**: Buffers messages that cannot be delivered immediately for an in-memory session. An in-memory session stores its state in an EMQX node's memory.

Messages may enter the Message Queue when:

- A client is disconnected and its session remains. EMQX delivers the queued messages when the client reconnects.
- QoS 1 and QoS 2 messages are waiting for inflight capacity or delivery quota.
- A connection is congested.

If `mqueue_store_qos0` is enabled, EMQX may also queue QoS 0 messages while the client is offline, while the connection is congested, or when necessary to preserve delivery order. Set it to `false` to exclude QoS 0 messages from buffering while offline.

## Message Queue Delivery Behavior

EMQX handles Message Queue delivery differently depending on whether the connection is congested:

- On an uncongested connection, EMQX pauses additional QoS 1 and QoS 2 delivery when the Inflight Window reaches the `max_inflight` limit. Delivery resumes when an inflight slot becomes available. Because QoS 0 messages do not enter the Inflight Window, EMQX can continue delivering queued QoS 0 messages, even when an earlier QoS 1 or QoS 2 message is waiting for a slot.
- On a congested connection, the available inflight slots limit the number of messages that EMQX can dequeue in response to client acknowledgments, regardless of QoS. EMQX resumes queued delivery when the congestion clears or more inflight slots become available.

EMQX preserves delivery order for messages with the same topic and QoS level, but does not guarantee strict FIFO order across different QoS levels. When topic priorities are enabled, EMQX schedules queued messages by topic priority. If the current priority queue has no deliverable QoS 0 messages, the current dequeue operation stops even if a lower-priority queue still contains QoS 0 messages.

When the Message Queue reaches `max_mqueue_len`, EMQX evicts the oldest QoS 0 message. If no QoS 0 message is available, EMQX evicts the oldest remaining message. When topic priorities are enabled, this limit and eviction policy apply independently to each priority queue. When many QoS 0 messages enter the queue in a short period, this policy helps maintain delivery progress for QoS 1 and QoS 2 messages.

## Inflight Window and Receive Maximum

The MQTT v5 protocol adds a `Receive Maximum`  attribute to CONNECT packets, and the official explanation for it is:

> The client uses this value to limit the maximum number of published messages with a QoS of1 and a QoS of 2 that the client is willing to process simultaneously. There is no mechanism to limit the published messages with a QoS of 0 that the server is trying to send.

That is, the server can send subsequent PUBLISH packets to the client with different message identifiers while waiting for acknowledgment, until the number of unacknowledged messages reaches the `Receive Maximum` limit.

It is not difficult to see that `Receive Maximum` is actually the same as the Inflight Window mechanism in EMQX. However, EMQX already provided this function to the accessed MQTT client before the MQTT v5.0 protocol was released. Now, the clients using the MQTT v5.0 protocol will set the maximum length of the Inflight Window according to the specification of the Receive Maximum, while clients with earlier versions of the MQTT protocol will still set it according to the configuration.

However, EMQX does not necessarily grant the `Receive Maximum` value requested in the CONNECT packet. Instead, the `Receive Maximum` granted in the CONNACK packet is capped by the `mqtt.max_inflight` configuration.

## Configuration Items

| Configuration Items    | Type    | Optional Value  | Default Value | Description                                                  |
| ---------------------- | ------- | --------------- | ------------- | ------------------------------------------------------------ |
| mqtt.max_inflight      | integer | [1, 65535]      | 32            | Maximum number of unacknowledged QoS 1 and QoS 2 messages that EMQX can deliver simultaneously. For MQTT 5.0 clients, a smaller `Receive Maximum` takes precedence. |
| mqtt.max_mqueue_len    | integer | [0, ∞)          | 1000          | Maximum number of messages in an in-memory session's Message Queue. `0` means no limit. |
| mqtt.mqueue_store_qos0 | enum    | `true`, `false` | true          | Whether EMQX stores QoS 0 messages in an in-memory session's Message Queue while the client is disconnected or the connection is congested. EMQX may also queue QoS 0 messages to preserve delivery order. |
