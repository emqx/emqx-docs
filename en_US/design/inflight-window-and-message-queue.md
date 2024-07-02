# In-flight and Message Queue

## Introduction

To improve message throughput and reduce the impact of network fluctuations, EMQX allows multiple unacknowledged QoS 1 and QoS 2 messages to exist on the network link at the same time. These sent but unconfirmed messages will be stored in the Inflight Window until acknowledgment is complete.

When the number of concurrently existing messages in the network exceeds the limit, that is, the length limit of Inflight Window is reached(see `max_inflight`), EMQX will no longer send subsequent messages, but will store these messages in the Message Queue. Once a message is acknowledged in the Inflight Window, the message in the Message Queue will be sent in first-in, first-out order and stored in the Inflight Window.

If the number of in-flight QoS 1, 2 messages reaches the maximum limit of the Inflight Window (see `max_inflight`), the newly arrived messages will not be forwarded immediately, but will be temporarily stored in the Message Queue. 

Only when previous messages are confirmed and removed from the Inflight Window, the messages in the Message Queue will be sent in a FIFO order and added to the Inflight Window. QoS 0 messages are not affected by this, they are always forwarded immediately.

If the Message Queue also reaches the length limit, subsequent messages will still be cached to the Message Queue, but the oldest message in the Message Queue will be discarded. Therefore, it is very important to set a suitable Message Queue length limit (see `max_mqueue_len`) 

The Message Queue is also used to store messages (including QoS 0 messages) that arrive while the subscriber is offline and that will be sent the next time the subscriber comes online. Considering that QoS 0 messages may have a lower importance, you can choose to disable EMQX from storing QoS 0 messages to the queue, see `mqueue_store_qos0`.

Note that the Inflight Window and Message Queue are not global. EMQX will allocate a separate Inflight Window and Message Queue for each client connection.

## Inflight Window and Receive Maximum

The MQTT v5.0 protocol adds a `Receive Maximum`  attribute to CONNECT packets, and the official explanation for it is:

> The client uses this value to limit the maximum number of published messages with a QoS of1 and a QoS of 2 that the client is willing to process simultaneously. There is no mechanism to limit the published messages with a QoS of 0 that the server is trying to send.

That is, the server can send subsequent PUBLISH packets to the client with different message identifiers while waiting for acknowledgment, until the number of unacknowledged messages reaches the `Receive Maximum` limit.

It is not difficult to see that `Receive Maximum` is actually the same as the Inflight Window mechanism in EMQX. However, EMQX already provided this function to the accessed MQTT client before the MQTT v5.0 protocol was released. Now, the clients using the MQTT v5.0 protocol will set the maximum length of the Inflight Window according to the specification of the Receive Maximum, while clients with earlier versions of the MQTT protocol will still set it according to the configuration.

## Configuration Items

| Configuration Items | Type    | Optional Value  | Default Value                              | Description                                                  |
| ------------------- | ------- | --------------- | ------------------------------------------ | ------------------------------------------------------------ |
| max_inflight        | integer | (0, 65536)   | 32 *(external)*,<br /> 128 *(internal)*    | Inflight Window length limit, 0 means no limit               |
| max_mqueue_len      | integer | [0, ∞)       | 1000 *(external)*,<br />10000 *(internal)* | Message Queue length limit, 0 means no limit                 |
| mqueue_store_qos0   | enum    | `true`, `false` | true                                       | Whether EMQX store QoS 0 messages to the Message Queue when the client is offline |
