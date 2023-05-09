# Features and Benefits of MQTT over QUIC 

EMQX 5.0 designs a unique message transmission mechanism and management method for MQTT over QUIC, providing a more efficient and secure way to transmit MQTT messages over modern complex networks, thus improving the performance of MQTT in certain scenarios. 

The current implementation of EMQX replaces the transport layer with a QUIC stream, where the client initiates the connection and creates a bi-directional stream. EMQX and the client interact on this stream. Considering the complex network environment, if the client fails to perform QUIC handshake for some reason, it will automatically fall back to traditional TCP to avoid communication failure with the server. The interaction between EMQX and the client has two modes: [single-stream mode](#single-stream-mode) and [multi-streams mode](#multi-streams-mode). The features and benefits of each mode are introduced below with some use cases. 

<img src="/Users/emqx/Documents/GitHub/emqx-docs/en_US/mqtt-over-quic/assets/mqtt-over-quic.png" alt="MQTT over QUIC" style="zoom:25%;" />

## Single-Stream Mode

This is a basic mode that encapsulates the MQTT packets in a single bi-directional QUIC stream. The client is required to initiate a bi-directional stream from the client to EMQX within the QUIC connection. All the MQTT packet exchanges are done over this stream.

### Features and Benefits

- Fast handshake

  QUIC connection between the Client and the EMQX can be established within one round trip.

- Ordered data delivery

  Like TCP, the delivery of MQTT packets also follows the order of the messages being sent in the stream, even if the underlying UDP datagram packets are received in the wrong order.

- Connection resumption, 0-RTT

  Using the 0-RTT method, the client is able to resume a connection and send application data to the server in the first QUIC packet or immediately after. There is no need to wait for the EMQX's reply to complete a round trip.
  This feature is particularly useful for quickly recovering a closed connection caused by network disturbances and bringing the application businesses back online.

- Client address migration

  Without disconnecting and establishing a new QUIC connection, the client is enabled to actively or passively migrate its local address to a new address due to Network Address Translation (NAT) rebinding. The QUIC connection could be kept without major disturbances, so to MQTT layer and above.

- Package loss detection and recovery

  QUIC is more responsive compared to other protocols in detecting packet loss and recovery. The behaviors can be tuned specifically for each use case.

## Multi-Streams Mode

It is common for a single MQTT connection to carry multiple topic data for different businesses. The topics can either be correlated or unrelated. The multi-streams mode expands on the single-stream mode by leveraging the stream multiplexing feature of QUIC to allow MQTT packets to be transported over multiple streams. The QUIC connection handshake between the MQTT client and EMQX is the same as the single-stream mode.

The initial stream that is established from the client to EMQX is referred to as the control stream. Its purpose is to handle the maintenance or update of the MQTT connection. Following this, the client can initiate one or multiple data streams to publish topics or subscribe to topics, per stream.

The client is free to choose how to map the stream, for example:

   - Use one stream per topic.
   - Use one stream for QoS 1 and another for QoS 0.
   - Use one stream for publishing and another for subscriptions. (Publish/Subscribe over the control stream is also allowed.)

 As the broker, EMQX does stream packets bindings:

   - It sends PUBACK packets over the stream where it receives the PUBLISH for QoS 1, so to the QoS 2 packets.
   - It sends PUBLISH packets over the stream where it gets the topic subscription and also expects PUBACK for QoS1 from the same stream.


::: tip

The order of data is maintained per stream, hence, if there are two topics whose data is correlated and ordering is crucial, they should be mapped to the same stream.
:::

### Features and Benefits

   - Decouple connection control and MQTT data exchange

     The MQTT connection control packets, such as CONNECT, CONNACK, and PING, are handled over the control stream, while data exchanges such as publishing and subscription are done over the data stream. Even if the data stream is slow, the connection can still be kept alive in terms of handling PINGREQ and PINGRESP.

   - Avoid head-of-line blocking issues among the topics

     MQTT over QUIC allows for multiple data streams for different topics, enabling the messages of different topics to be delivered independently.

   - Split uplink (publishing) and downlink (subscribe) data

     For example, a client can use one stream to publish QoS1 messages and handle PUBACK over that same stream, while receiving QoS 0 messages from its subscriptions on another stream from the broker.

   - Prioritize different data
     MQTT over QUIC also provides the ability to prioritize data from different MQTT topics through multi-streams. This means that topic data can be prioritized and delivered accordingly, improving the overall performance and responsiveness of the connection. 

   - Improve the parallelism of processing on the client/EMQX side

     With the use of data streams, EMQX is able to process multiple streams in parallel, improving the overall efficiency and responsiveness of the system.

   - More robust handling of MQTT data

     If a single data stream is aborted due to an application error, it will not cause the entire connection to close. Instead, the application is free to recreate the stream and recover the data. This allows for more reliable and resilient MQTT communication.

   - Flow control data streams

     Flow control can be applied per data stream, allowing for different flow control policies for different topics or QoS levels.

   - Reduce subscription latency

     A client does not need to wait for MQTT CONNACK before sending the subscribe or publish packets.

     However, EMQX will only begin processing them after the client has established a connection and while the connection is allowed.

####   