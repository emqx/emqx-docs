# MQTT over QUIC

The MQTT protocol has commonly used in messaging services of the IoT and Internet of Vehicles (IoV) applications. It is designed to work on top of a transport protocol that provides a reliable, ordered, and lossless stream of bytes in both directions. This reliable transport protocol guarantees that messages are delivered accurately and in the order in which they were sent. Traditionally, IoT applications apply MQTT over TCP-based protocols, such as raw TCP, TCP/TLS (for security), and WebSocket (for web browser adaption). However, there are limitations in some scenarios when the network conditions can be complex, such as high latency, high packet loss, and weak or spotty networks.

To overcome the limitations of MQTT over TCP, EMQX 5.0 innovatively introduces a new protocol, MQTT over QUIC, that enables MQTT clients and EMQX to communicate over Quick UDP Internet Connections (QUIC). The protocol offers the same functionality as the existing MQTT protocol but with the additional benefits of QUIC. 

This chapter explains why and how MQTT over QUIC is implemented in EMQX. In [Features and Benefits of MQTT over QUIC](./features-mqtt-over-quic.md), it introduces two interaction modes between clients and EMQX on the QUIC stream and the features and benefits of each mode. In [Use MQTT over QUIC](./getting-started.md), it demonstrates how to enable MQTT over QUIC in EMQX using client SDKs and tools.

::: tip

For now, MQTT over QUIC is still an experimental feature. EMQX is preparing a draft proposal about MQTT over QUIC for submission to the OASIS MQTT Technical Committee. 

:::

## Introduction of QUIC

QUIC, initially developed by Google, was later adopted as a worldwide standard by the Internet Engineering Task Force (IETF). It is a new transport protocol that provides faster connection establishment.  By using a more efficient algorithm that allows for a quicker ramp-up of data transmission rates, it improves congestion control. Additionally, QUIC uses a stream-based multiplexing architecture that allows for independent transmission of data streams, which helps to avoid head-of-line blocking and can improve performance in high packet loss or delay scenarios.

QUIC is also the only transport protocol of the latest version of the HTTP protocol, HTTP/3. Compared to TCP, as the underlying transport protocol for the next-generation internet protocol HTTP/3, QUIC can significantly improve overall throughput and mobile connection stability while reducing connection overhead and message delay. Therefore, it is more suitable than the TCP protocol for solving communication problems in complex network environments.

## Application Scenarios of MQTT over TCP

MQTT over QUIC is particularly suitable for businesses with high requirements for real-time and stable data transmission. For example, connected vehicles driving in mountains, mines, and tunnels, where connection can be interrupted when entering signal blind spots or switching base stations passively. With the advantages of QUIC, MQTT over QUIC can overcome the shortcomings of traditional MQTT over TCP in the following scenarios: 

- Slow connection establishment for TCP/TLS
  The initial handshake between the client and server requires multiple round trips to establish a connection. The round trip time (RTT)  is critical for the connection establishment speed. A longer RTT can result in increased latency and slower connection establishment.
- A slow ramp-up of traffic due to a slow start using a congestion window 
- Head of line blocking
  When a packet is lost, the whole transmission is blocked until it is recovered. This increases the latency significantly.
- No awareness of the upper layer protocols
  TCP treats all data transmission equally, without distinguishing between different types of data or businesses that may be using the same network connection.

For more information on the features and benefits of MQTT over QUIC to enhance the user experience in weak and unstable network environments, see [Features and Benefits of MQTT over QUIC](./features-mqtt-over-quic.md).

## QUIC vs TCP/TLS Test Comparisons

In comparison with TCP/TLS testing, the performance of MQTT over QUIC is summarized as follows:

- When network latency is high, QUIC is able to establish connections and subscribe faster.
- After disconnecting, with 0-RTT, it could re-establish the connection much quicker than TCP/TLS.
- QUIC is better than TLS for both server CPU and memory usage when connecting/reconnecting on a large scale.
- When NAT rebinding, client reconnection response under TCP/TLS is very slow and the message transmission is broken, while QUIC handles it more smoothly and the messages are sent without any impact.
- In a weak network packet loss and packet transmission disorder environment, TLS shows message congestion and loss due to a poor network environment, while the QUIC server receives slightly jittery data but does not lose messages.

## Limitations

Currently, MQTT over QUIC has the following limitations:

  - Preserving session state is currently not supported. This means that if a client needs to reconnect, it must resubscribe to the topics it previously subscribed to over a data stream.


- If the data stream is closed unexpectedly by either peer, the QoS 1 and QoS 2 message states are not preserved. 

## Future Work

As of now, the MQTT over QUIC is ready for production, users are already testing it in depth and giving good feedback, see [Getting Started](./getting-started.md) to experience it now.

Still, EMQX has not utilized all the features provided by QUIC, such as broker-side stream prioritization, broker-side flow control, and unreliable datagram. These features will be addressed in the later releases and, hopefully, become an OASIS standard.

Further investigation is also required on how to preserve the message states and resume the subscription without reconnection, as mentioned in [Limitations](#limitations). 
