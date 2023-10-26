# MQTT over QUIC

The MQTT is designed to work on top of TCP protocol that provides a reliable, ordered, and lossless stream of bytes in both directions. However, in the field of the Internet of Vehicles (IoV), there is an increasing demand for real-time and efficient message transmission, and the limitations of TCP are becoming apparent. With the increasing interconnection of vehicles, sensors, and infrastructure, overcoming the bottlenecks of TCP transmission is essential for building a safer, smarter, and more agile transportation ecosystem.

To address this, EMQX 5.0 introduced the MQTT over Quick UDP Internet Connections (QUIC) protocol. While retaining compatibility with all MQTT protocol features, this protocol allows IoT clients to establish connections and communicate with EMQX via QUIC. This brings significant advantages to client connections. Clients using QUIC can improve connection and message throughput performance while reducing latency, especially in common scenarios like Internet of Vehicles (IoV), where weak networks, frequently changing links, and unstable network environments are prevalent. 

This chapter explains why and how MQTT over QUIC is implemented in EMQX. In [Features and Benefits](./features-mqtt-over-quic.md), it introduces two interaction modes between clients and EMQX on the QUIC stream and the features and benefits of each mode. In [Use MQTT over QUIC](./getting-started.md), it demonstrates how to enable MQTT over QUIC in EMQX using client SDKs and tools.

::: tip

For now, MQTT over QUIC is not yet the standard protocol for MQTT, but it has the capability to be deployed in production, and EMQ is actively driving its standardization process within OASIS.
:::

## Introduction of QUIC

QUIC is a new transport protocol that offers faster connection establishment speeds. It was originally developed by Google, and designed to overcome specific inefficiencies in TCP and TLS, which are traditional building blocks of network communication so as to meet the increasing demand for more efficient, secure, and low-latency protocols from clients. Later, it was adopted as a global standard by the Internet Engineering Task Force (IETF).

By using a more efficient algorithm that allows for a quicker ramp-up of data transmission rates, it improves congestion control. Additionally, QUIC uses a stream-based multiplexing architecture that allows for independent transmission of data streams, which helps to avoid head-of-line blocking and can improve performance in high packet loss or delay scenarios.

As the next-generation internet transport protocol, it is the underlying transport protocol for HTTP/3. Compared to TCP, QUIC significantly reduces connection overhead and message latency while greatly improving overall throughput and the stability of mobile connections. Therefore, QUIC is also suitable for addressing communication challenges in complex network environments.

## Application Scenarios of MQTT over TCP

MQTT over QUIC is particularly suitable for businesses with high requirements for real-time and stable data transmission. For example, connected vehicles driving in mountains, mines, and tunnels, where connection can be interrupted when entering signal blind spots or switching base stations passively. With the advantages of QUIC, MQTT over QUIC can overcome the shortcomings of traditional MQTT over TCP in the following scenarios: 

- Slow connection establishment for TCP/TLS
  The initial handshake between the client and server requires multiple round trips to establish a connection. The round trip time (RTT)  is critical for the connection establishment speed. A longer RTT can result in increased latency and slower connection establishment.
- A slow ramp-up of traffic due to a slow start using a congestion window 
- Head of line blocking
  When a packet is lost, the whole transmission is blocked until it is recovered. This increases the latency significantly.
- No awareness of the upper layer protocols
  TCP treats all data transmission equally, without distinguishing between different types of data or businesses that may be using the same network connection.

For more information on the features and benefits of MQTT over QUIC to enhance the user experience in weak and unstable network environments, see [Features and Benefits](./features-mqtt-over-quic.md).

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
