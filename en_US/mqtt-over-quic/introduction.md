# MQTT over QUIC

In traditional IoT application scenarios, MQTT relies on TCP for messaging services. However, under complex network conditions with frequent connection interruptions and slow connection establishment, the user experience may be affected, for example, vehicles may run in mountainous areas, mines, tunnels, etc., which can cause connection interruptions when entering signal dead zones or passively switching base stations (also referred to as spotty networks). 

As the underlying transport protocol of the next-generation Internet protocol HTTP/3,  [QUIC](https://datatracker.ietf.org/doc/html/rfc9000) can provide connectivity for the modern mobile Internet with less connection overhead and message latency compared to TCP/TLS protocols. Therefore, it is highly suitable for IoT messaging scenarios. 

Therefore, EMQX 5.0 introduces QUIC support (MQTT over QUIC) and designs a unique messaging mechanism and management approach.

:::tip

For now, MQTT over QUIC is still an experimental feature, EMQX is preparing a draft proposal about MQTT over QUIC for submission to the OASIS MQTT Technical Committee.

:::

## Scenarios for MQTT over QUIC

MQTT over QUIC is suitable for complex network environments, for example:

- Frequent network switching that causes connection interruptions;
- Difficult to re-establish connection after disconnection: The operating system is slow to release resources after disconnection, the application layer cannot sense the disconnection status in time, and the Server/Client overhead is high when reconnecting;
- Weak spotty network: Data transmission is blocked by congestion, packet loss, and retransmission.

To sum up, MQTT over QUIC is suitable for services with high requirements for real-time data transmission and stability, such as the L4 driverless vehicle. 

In these scenarios, the low connection overhead and multi-path support of QUIC shows its strengths. After deeper exploration, we believe that MQTT over QUIC is a great solution to this dilemma - based on QUIC's 0 RTT/1 RTT reconnect/new capability and migration support, it can effectively improve user experience in weak networks and irregular network paths.

## Features and implementation

The current implementation of EMQX replaces the transport layer with a QUIC Stream, where the client initiates the connection and creates a bi-directional Stream. EMQX and the client interact on it.

Considering the complex network environment, if for some reason the client fails to complete QUIC connection handshake, it is recommended that the client automatically fall back to a traditional TCP connection to ensure connectivity.

<img src="./assets/mqtt-over-quic.png" alt="MQTT over QUIC" style="zoom:33%;" />

MQTT protocol can benefit from using QUIC as its transport as follows:

- **More advanced congestion control**: As verified by the tests, MQTT over QUIC can effectively reduce packet loss and enable continuous and stable data transmission despite network fluctuations;
- **Operation and maintenance friendly**: Reduce overhead (time overhead, client/server performance overhead) caused by massive reconnection and reduce system overload caused by unnecessary application layer state migration (0 RTT);
- **More flexible architectural innovations:** Adopt Direct server return (DSR) mode, where only ingress/request traffic passes through the LB and egress and response traffic will bypass the LB and goes directly back to the client, reducing bottlenecks in the LB;
- **Reduce handshake latency**: Reduce from 2 个 RTT Round trip time to 1 RTT.
- **Multi-path support for smooth connection migration**: For mobile devices where the network changes frequently, for example, handover from 4G to WIFI or NAT Rebinding, QUIC can maintain a connection via Connection ID with no need to reestablish the connection, thus reducing the network overhead and handshake latency. 
- **More agile development and deployment:** The QUIC protocol stack can be implemented in the userspace, enabling fast iterations, quick bugfix rollout, and reducing the lead time from PoC to production.
- **End-to-end encryption:** QUIC packet leaves minimal information unencrypted in the headers to make communication secure and uninterceptable by middleboxes.

There are also more opportunities to be explored:

- **Streams with different topics:** We could use parallel streams in the same connection to carry different topics to make sending/receiving process parallelized with different priorities and mitigate the HOL (Head Of Line) blocking issue.
- **Streams with different QoS:** For example, in "Flow Control", QoS 0 messages should give way to high QoS messages.
- **Separate control messages into different streams:** MQTT control messages can be sent in one or two directions. For example, the client can send UNSUBSCRIBE requests asynchronously through a short-lived unidirectional stream to request the server to stop sending data that is no longer of interest.
- **Finer-grained send and receive collaborative flow control:** Flow control is performed on a per-flow basis or across the entire connection, enabling finer-grained flow control.

## QUIC vs TCP/TLS Test Comparisons

In comparison with TCP/TLS testing, MQTT over QUIC performs as follows:

1. When network latency is high, QUIC is able to establish connections and subscribe faster.
2. After disconnecting, it takes less latency to re-establish the connection and reconnect to QUIC.
3. QUIC is better than TLS for both server CPU and memory usage when connecting/reconnecting on a large scale.
4. When NAT rebinding, client reconnection response under TCP/TLS is very slow and the message transmission is broken, while QUIC handles it more smoothly and the messages are sent without any impact.
5. In a weak network packet loss and packet transmission disorder environment, TLS shows message congestion and loss due to a poor network environment, while the QUIC server receives slightly jittery data but does not lose messages.

## Future work

As of now, the MQTT over QUIC is ready for production, users are already testing it in depth and giving good feedback, see [Getting Started](./getting-started.md) to experience it now.

Still, EMQX has not utilized all the features provided by QUIC, such as multi streams, stream prioritization, flow control and unreliable datagram.

These features will be addressed in the later releases and, hopefully, become an OASIS standard.
