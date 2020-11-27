# Message retransmission

Message retransmission is part of the MQTT protocol standard specification.

The protocol stipulates that the PUBLISH packets sent to the peer by the **server** and **client** as communication parties must meet their **Quality of Service levels** requirements , such as:

- QoS 1: it means that **the message is delivered at least once;** that is, the sender will always resend the message unless it receives confirmation from the peer. This means that the same QoS 1 message may be received multiple times in the upper layer(the application layer of the service) of the MQTT protocol.
- QoS 2: it means **the message is delivered exactly once;** that is, the message will only be received once at the upper layer.

Although PUBLISH packets of QoS 1 and QoS 2 will be resent at the MQTT protocol stack layer, you must remember:

- After retransmission of QoS 1 messages happens, these retransmitted PUBLISH packets will also be received at the upper layer of the MQTT protocol stack.
- No matter how QoS 2 message is retransmitted, only one PUBLISH packet will be received in the upper layer of the MQTT protocol stack,

## Basic configuration

There are two scenarios that will cause the message to be resent:

1. After the PUBLISH packet is sent to the peer, and no response is received within the specified time, the packet is resent.
2. While maintaining the session, after the client reconnects, EMQ X Broker will automatically resend the *unanswered message* to ensure the correct QoS process.

It can be configured in `etc/emqx.conf`:

| Configuration item | Type   | Optional value | Default value | Description |
| -------------- | --------- | ------ | ------- | -------------- |
| retry_interval | duration  | -      | 30s     | Wait for a timeout interval and retransmit the message if no response is received |

Generally speaking, you only need to care about the above content.

For more details on how EMQ X Broker handles the retransmission of the MQTT protocol, see the following of this article.

## Protocol specification and design

### Retransmitted objects

First, before understanding the retransmission mechanism design of EMQ X Broker, we need to ensure that you have understood the transmission process of QoS 1 and QoS 2 in the protocol, otherwise please refer to[MQTTv3.1.1 - QoS 1: At least once delivery](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718101) and [MQTTv3.1.1 - QoS 2: Exactly once delivery](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718102)

Only a brief review is made to illustrate what are the retransmitted objects under different QoS.

#### QoS 1

QoS 1 requires the message to be delivered at least once; therefore, the message may be continuously retransmitted in the MQTT protocol layer until the sender receives the confirmation message of the message.

The schematic diagram of the process is as follows:

```
               PUBLISH
#1 Sender  --------------->  Receiver       (*)
               PUBACK
#2 Sender  <---------------  Receiver
```

- Two packets are involved; there are two sending actions totally, one at the sender and one at the receiver; both packets hold the same PacketId.
- If the end of the line is marked with an *, it means that the sender may initiate a retransmission if the waiting for the confirmation message is time out.

It can be seen that **QoS 1 messages only need to retransmit PUBLISH messages** 

#### QoS 2

QoS 2 requires the message to be delivered only once; so when it is implemented, a more complicated process is required. The schematic diagram of the process is as follows:

```
               PUBLISH
#1 Sender  --------------->  Receiver       (*)
               PUBREC
#2 Sender  <---------------  Receiver
               PUBREL
#3 Sender  --------------->  Receiver       (*)
               PUBCOM
#4 Sender  <---------------  Receiver
```

- 4 packets are involved; there are 4 sending actions totally, 2 times for each of the sender and receiver; these 4 packets all hold the same PacketId.
- If the end of the line is marked with an *, it means that the sender may initiate a retransmission if the waiting for the confirmation message is time out.

It can be seen that **QoS 2 messages only need to retransmit PUBLISH packet and PUBREL packet** 

In summary:

- **Retransmission action** is triggered under the condition that the expected return is not received within **specified time** after sending the message, and not receiving.
- **Retransmission object** only contains the following three types:
    * QoS 1 PUBLISH packet
    * QoS 2 PUBLISH packet
    * QoS 2 PUBREL packet

When EMQ X Broker acts as the receiver of PUBLISH messages, it does not require the retransmission operation.


### Inflight window and maximum receiving value

For the definition and explanation of this concept, please refer to [Inflight Window and Message Queue](inflight-window-and-message-queue.md#)

The purpose of introducing these two concepts is to understand:

1. When EMQ X Broker is used as the sender, the retransmitted message must be the message stored in the inflight window.
2. When EMQ X Broker is used as the receiver, and the sender retransmits the message:
    - For QoS 1, EMQ X Broker directly reply PUBACK as response;
    - For QoS 2, EMQ X Broker will release the stored PUBLISH or PUBREL packet in the *maximum received message* queue.


### Message sequence

Of course, the above concepts only need to be understood. What you need to care about most is the change in message order after **messages are retransmitted, especially for QoS type 1 messages**. E.g:

Suppose that when the current inflight window is set to 2, EMQ X Broker plans to deliver 4 QoS 1 messages to a certain topic on the client. Assume that the client program or the network has experienced problems in the middle of the process, then the entire sending process will become:

```
#1  [4,3,2,1 || ]   ----->   []
#2  [4,3 || 2, 1]   ----->   [1, 2]
#3  [4 || 3, 2]     ----->   [1, 2, 3]
#4  [4 || 3, 2]     ----->   [1, 2, 3, 2, 3]
#5  [ || 4]         ----->   [1, 2, 3, 2, 3, 4]
#6  [ || ]          ----->   [1, 2, 3, 2, 3, 4]
```

There are 6 steps in the process; the left indicates the message queue and inflight window of EMQ X Broker,  which is separated by `||`; the right indicates the sequence of messages received by the client, where each step indicates:

1. Broker puts 4 messages into the message queue.
2. Broker sequentially sends `1` `2` and puts it in the **inflight window**; the client only responds to the message `1`; and at this time due to a problem with the client's sending stream, subsequent responses cannot be sen.
3. Broker receives the reply of the message `1`; removes the message` 1` from the inflight window; and sends out `3`; continues to wait for the reply of ` 2` `3`;
4. When the waiting for the response is time out, broker retransmitted the message  `2` `3`; the client received the retransmitted message `2` `3` and responded normally.
5. Broker removed the message  `2` `3` from the inflight window and sent the message `4`; the client received the message 4 and responded with a reply.
6. At this point, all message processing is complete. The sequence of messages received by the client is `[1, 2, 3, 2, 3, 4]`, and it is reported to the upper layer of the MQTT protocol stack in turn.

Although there are duplicate messages, this is in full compliance with the specifications of the protocol. The first appearance of each message is in order, and the message  `2` `3` repeatedly received will carry an identification bit, indicating that it is a retransmission message.

The MQTT protocol and EMQ X Broker regard this topic as an `Ordered Topic`. See: [MQTTv3.1.1 - Message ordering](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718105).

It ensures that under the same topic and QoS, messages are delivered and answered in order.

In addition, if the user expects that QoS 1 and QoS 2 messages under all topics are strictly ordered, the maximum length of the flight window needs to be set to 1, but it will reduce the client's throughput.


### Related configuration

This section lists all the configurations used in the above mechanism. They are all included in `etc/emqx.conf`:

| Configuration | Type  | Optional value | Default value | Description                                          |
| ----------------- | -------- | --------------- | ------ | ------------------------------------------------------- |
| mqueue_store_qos0 | bool     | `true`, `false` | true   | Whether to store QoS 0 messages in the message queue |
| max_mqueue_len    | integer  | >= 0            | 1000   | Message queue length                        |
| max_inflight      | integer  | >= 0            | 0      | Inflight window size; default `0` means no limit |
| max_awaiting_rel  | integer  | >= 0            | 0      | Maximum reception; default `0` means no limit |
| await_rel_timeout | durtaion | >  0            | 300s   | The maximum value of timeout in `Max Receive` to wait for release; if they are exceeded, the messages are discarded directly |

