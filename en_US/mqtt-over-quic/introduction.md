# MQTT over QUIC

The MQTT protocol is designed to work on top of a transport protocol that provides a reliable, ordered, and lossless stream of bytes in both directions. This reliable transport protocol guarantees that messages are delivered accurately and in the order in which they were sent. Traditionally, Internet of Things (IoT) applications apply MQTT over TCP-based protocols, such as raw TCP, TCP/TLS (for security), and WebSocket (for web browser adaption). However, there are limitations in some scenarios.

EMQX 5.0 introduces a new protocol, MQTT-over-QUIC, that enables MQTT clients and EMQX to communicate over Quick UDP Internet Connections (QUIC). The protocol offers the same functionality as the existing MQTT protocol, but with the additional benefits of QUIC. 

<img src="./assets/mqtt-over-quic.png" alt="MQTT over QUIC" style="zoom:33%;" />

This chapter explains why MQTT over QUIC is implemented and introduces the features and benefits with implementation use cases. In [Use MQTT over QUIC](./getting-started.md), it demonstrates how to enable MQTT over QUIC in EMQX using client SDKs and tools.

::: tip

For now, MQTT over QUIC is still an experimental feature. EMQX is preparing a draft proposal about MQTT over QUIC for submission to the OASIS MQTT Technical Committee. 

:::

## Limitations of MQTT over TCP

MQTT has commonly used in messaging services of the IoT and Internet of Vehicles (IoV) applications. Sometimes the network conditions can be complex, such as high latency, high packet loss, and weak or spotty networks. In these conditions, MQTT over TCP has the following apparent shortcomings:

- Slow connection establishment for TCP/TLS
  The initial handshake between the client and server requires multiple round trips to establish a connection. The round trip time (RTT)  is critical for the connection establishment speed. A longer RTT can result in increased latency and slower connection establishment.
- A slow ramp-up of traffic due to a slow start using a congestion window <!--What does it mean?-->
- Head of line blocking
  When a packet is lost, the whole transmission is blocked until it is recovered. This increases the latency significantly.
- No awareness of the upper layer protocols
  TCP treats all data transmission equally, without distinguishing between different types of data or businesses that may be using the same network connection.

## Introduction of QUIC

QUIC was initially developed by Google was later adopted as a worldwide standard by the Internet Engineering Task Force (IETF). It is a new transport protocol that provides faster connection establishment.  By using a more efficient algorithm that allows for a quicker ramp-up of data transmission rates, it improves congestion control. Additionally, QUIC uses a stream-based multiplexing architecture that allows for independent transmission of data streams, which helps to avoid head-of-line blocking and can improve performance in high packet loss or delay scenarios.

QUIC is also the only transport of the latest version of the HTTP protocol, HTTP/3. The HTTP/3 is specifically designed to take the advantage of the performance benefits provided by QUIC.

## Features and Benefits of MQTT over QUIC 

MQTT-over-QUIC provides a more efficient and secure way of transmitting MQTT messages over the modern complex network, which improves the performance of MQTT in certain scenarios. The current implementation of EMQX replaces the transport layer with a QUIC stream, where the client initiates the connection and creates a bi-directional stream. EMQX and the client interact on it. The interaction has two modes: [single-stream mode](#single-stream-mode) and [multi-streams mode](multi-streams-mode). The features and benefits of each mode are introduced below with some use cases.

### Single-Stream Mode

This is a basic mode that encapsulates the MQTT packets in a single bi-directional QUIC stream. The client is required to initiate a bi-directional stream from the client to EMQX within the QUIC connection. All the MQTT packet exchanges are done over this stream.

#### Features

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

### Multi-Streams Mode

It is common for a single MQTT connection to carry multiple topic data for different businesses. The topics can either be correlated or unrelated. The multi-streams mode expands on the single-stream mode by leveraging the stream multiplexing feature of QUIC to allow MQTT packets to be transported over multiple streams. The QUIC connection handshake between the MQTT client and EMQX is the same as the single-stream mode.

The initial stream that is established from the client to EMQX is referred to as the control stream. Its purpose is to handle the maintenance or update of the MQTT connection. Following this, the client can initiate one or multiple data streams to publish topics or subscribe to topics, per stream.

The client is free to choose how to map the stream, for example:

   - Use one stream per topic.
   - Use one stream for QoS1 and another for QoS0.
   - Use one stream for publishing and another for subscriptions. (Publish/Subscribe over the control stream is also allowed.)

 As the broker, EMQX does stream packets bindings:

   - It sends PUBACK packets over the stream where it receives the PUBLISH for QoS1, so to the QoS2 packets.
   - It sends PUBLISH packets over the stream where it gets the topic subscription and also expects PUBACK for QoS1 from the same stream.


::: tip

The order of data is maintained per stream, hence, if there are two topics whose data is correlated and ordering is crucial, they should be mapped to the same stream.
:::

#### Features and usecases:

   - Decouple connection control and MQTT data exchange
   
     The MQTT connection control packets, such as CONNECT, CONNACK, and PING, are handled over the control stream, while data exchanges such as publishing and subscription are done over the data stream. Even if the data stream is slow, the connection can still be kept alive in terms of handling PINGREQ and PINGRESP.
     
   - Avoid head-of-line blocking issues among the topics
   
     MQTT-over-QUIC allows for multiple data streams for different topics, enabling the messages of different topics to be delivered independently.
     
   - Split uplink (publishing) and downlink (subscribe) data
   
     For example, a client can use one stream to publish QoS1 messages and handle PUBACK over that same stream, while receiving QoS0 messages from its subscriptions on another stream from the broker.
     
   - Prioritize different data
     MQTT over QUIC also provides the ability to prioritize data from different MQTT topics through multi-streams. This means that topic data can be prioritized and delivered accordingly, improving the overall performance and responsiveness of the connection. 
   
   - Improves the parallelism of processing on the client/EMQX side.
   
     With the use of data streams, EMQX is able to process multiple streams in parallel, improving the overall efficiency and responsiveness of the system.
     
   - More robust handling of MQTT data
     
     If a single data stream is aborted due to an application error, it will not cause the entire connection to close. Instead, the application is free to recreate the stream and recover the data. This allows for more reliable and resilient MQTT communication.
     
   - Flow control data streams
     
     Flow control can be applied per data stream, allowing for different flow control policies for different topics or QoS levels.
     
   - Reduce subscription latency
   
     A client does not need to wait for MQTT CONNACK before sending the subscribe or publish packets.
     
     However, EMQX will only begin processing them after the client has established a connection and while the connection is allowed.
     

  Limitations:

  - Preserving session state is currently not supported.
  
    This means that if a client needs to reconnect, it will have to resubscribe to the topics it was previously subscribed to over a data stream.
    @TODO also describe the QoS message states


## QUIC vs TCP/TLS Test Comparisons

In comparison with TCP/TLS testing, the performance of MQTT over QUIC is summarized as follows:

- When network latency is high, QUIC is able to establish connections and subscribe faster.
- After disconnecting, with 0-RTT, it could re-establish the connection much quicker than TCP/TLS.
- QUIC is better than TLS for both server CPU and memory usage when connecting/reconnecting on a large scale.
- When NAT rebinding, client reconnection response under TCP/TLS is very slow and the message transmission is broken, while QUIC handles it more smoothly and the messages are sent without any impact.
- In a weak network packet loss and packet transmission disorder environment, TLS shows message congestion and loss due to a poor network environment, while the QUIC server receives slightly jittery data but does not lose messages.

## Future Work

As of now, the MQTT over QUIC is ready for production, users are already testing it in depth and giving good feedback, see [Getting Started](./getting-started.md) to experience it now.

Still, EMQX has not utilized all the features provided by QUIC, such as broker-side stream prioritization, broker-side flow control, and unreliable datagram.

These features will be addressed in the later releases and, hopefully, become an OASIS standard.
