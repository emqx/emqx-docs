# MQTT over QUIC

## MQTT and TCP

MQTT has been designed to work over a transport protocol that provides a reliable, ordered, and lossless stream of bytes in both directions. This reliable transport protocol ensures that messages are delivered correctly and in the order in which they were sent.

Traditonal application runs MQTT over TCP based protocol such as rasw TCP, TCP/TLS for security and WebSocket for web browser adaption. Howerver TCP has its limitations

- Slow connection establishment for TCP/TLS
  The round trip time (RTT) between the client and server is a critical factor that can affect the speed at which the connection is established. 
  Since the initial handshake requires multiple round trips between the client and server to establish the connection, a longer RTT can result in increased latency and slower connection establishment.
  
- Slow rampup of traffic due to slow start using congestion window

- Head of line blocking
  When a packet is lost, the whole transmission is blocked until it is recovered. 
  This increase the latency significantly.
  
- No awareness of the upper layer protocols
  TCP has no awareness of the upper layer protocols and applications that are using the same network connection. 
  In other words, TCP treats all data transmission equally, without distinguishing between different types of data or businesses that may be using the same network connection.


The shortcomings of TCP become even more apparent under complex network conditions with higher latency, higher packet loss, bad network coverage with weak spotty network, frequent 
network/network address switches. These conditions are often encountered in IoT (Internet of Things) and IoV (Internet of Vehicles) applications, where MQTT is commonly used. 

## QUIC and HTTP/3

To overcome these limitations, Google initially developed a new transport protocol known as QUIC, which was later adopted as a worldwide standard by the Internet Engineering Task Force (IETF). QUIC provides faster connection establishment and improved congestion control through the use of a more efficient algorithm that allows for quicker ramp-up of data transmission rates. 
Additionally, QUIC uses a stream-based multiplexing architecture that allows for independent transmission of data streams, which helps to avoid head-of-line blocking and 
can improve performance in high packet loss or delay scenarios.

Latest version of the HTTP protocol, HTTP/3 was specifically designed to take advantage of the performance benefits provided by QUIC, and QUIC is the only transport of HTTP/3.

## Introducing MQTT-over-QUIC

From EMQX 5.0, we introduced a new protocol called MQTT-over-QUIC that allows MQTT client and EMQX talks over QUIC. The protocol provides the same functionality as the existing MQTT protocol but with the benefits of QUIC. MQTT-over-QUIC provides a more efficient and secure way of transmitting MQTT messages over the morden complex network and can improve the performance of MQTT in certain scenarios.

:::tip

For now, MQTT over QUIC is still an experimental feature, EMQX is preparing a draft proposal about MQTT over QUIC for submission to the OASIS MQTT Technical Committee.

:::

## Features and Implementation

<img src="./assets/mqtt-over-quic.png" alt="MQTT over QUIC" style="zoom:33%;" />

MQTT-over-QUIC has two modes.

### Single Stream Mode

  This is a basic mode that encapsulate the MQTT packets in a single bidirectional QUIC stream.

  The client is required to initiate a bidirectional stream from the client to the broker (EMQX) within the QUIC connection then all the MQTT packets exchanges 
  are done over this stream.
  
  Features: 
  
  1. Fast handshake.
  
      QUIC connection between Client and the broker can be established within one round trip.
     
  1. Ordered data delivery
  
      Like TCP, the delivery of MQTT packets also follows the order of the messages being sent in the stream, even if the underlying UDP datagram packets are received in the wrong order.
    
  1. Connection Resumption, 0-RTT
  
      Using the 0-RTT method, the client is able to resume a connection and send application data 
  to the server in the first QUIC packet or immediately after, without having to wait for the broker's reply to complete a round trip.
    
      This feature is particularly useful for quickly recovering a closed connection caused by network disturbances and bring the application businesses back online.
         
  1. Client Address Migration
  
      Without the need to disconnect and establish a new QUIC connection, the client has the ability to migrate its local address to a new address actively or passively due to 
  NAT rebinding.The QUIC connecion could be kept without marjor disturbances, so to MQTT layer and above.
    
  1. Loss detection and loss recovery
  
      In terms of detecting packet loss and recovery, QUIC is more responsive compared to other protocols. And the behaviors could be tuned specifically
  for each usecases.

### Multi-streams Mode
   
   It is common for a single MQTT connection to carry multipule topic data for different businesses, the topics can either be correlated or unrelated.
   
   The Multi-streams Mode expands on the Single Stream Mode by leveraging the stream multiplexing feature of QUIC to allow MQTT packets to be transported over multiple streams.
   
   The QUIC connection handshake between the MQTT client and broker (EMQX) is the same as the single stream mode.
   
   The initial stream that is established from the client to the broker is referred to as the control stream. Its purpose is to handle the maintenance or update of the MQTT connection. Following this, the client can initiate one or multiple data streams to publish topic data or subscribe to topic data, per stream.
   
   The client is free to choice how to map the streams. 
   
   - Use one stream per topic.
   
   - Use one stream for QoS1 and another for QoS0.
   
   - Use one stream for publishing and another for subscriptions, receiving publishs.
   
   - Publish/Subscribe over the control stream is also allowed.
   
   EMQX as broker do stream packets bindings:
   
   - It send PUBACK over the stream where it received the PUBLISH for QoS1, so to the QoS2 packets
   - It send PUBLISH packets over the stream where it get the topic subscription and also expecting PUBACK for QoS1 from the same stream.
   
   
   :::tip

    The order of data is maintained per stream, hence, if there are two topics whose data is correlated and ordering is crucial, they should be mapped to the same stream.
    
   :::
   
   Features and usecases:
   
   - Decouple connection control and MQTT data exchange.
   
     The MQTT control packets related to the connection, such as CONNECT, CONNACK, and PING, are handled on the control stream, while data exchanges such as publishing and subscription are done over the data stream.
     Even if the data stream is slow, the connection can still be kept alive in terms of handling PINGREQ and PINGRESP.

   - Avoid head of line blocking issue among the topics.
   
     In order to avoid head-of-line blocking issues among the topics, MQTT-over-QUIC allows for multiple data stream for diffent topics
     enabling the messages of different topics to be delivered independently.
               
   - Split uplink (publishing) and downlink (subscribe) data
   
     For example, a client could use one stream to publish QoS1 messages and handle PUBACK over that same stream, while receiving QoS0 messages from its subscriptions on another stream from the broker.
     
   - Prioritize different data
     MQTT over QUIC also provides the ability to prioritize data from different MQTT topics through the mutli-streams.
   
     This means that topic data can be prioritized and delivered accordingly, improving the overall performance and responsiveness 
     of the connection. 
       
   - Improves the parallelism of processing on the client/EMQX side.
   
     With the use of data streams, EMQX is able to process multiple streams in parallel, improving the overall efficiency and responsiveness of the system.
       
   - More robust handling of MQTT data
     
     If a single data stream is aborted due to an application error, it will not cause the entire connection to close. Instead, the application is free to recreate the stream and recover the data. This allows for more reliable and resilient MQTT communication.
     
   - Flow control data streams
     
     Flow control can be applied per data stream, allowing for different flow control policies for different topics or QoS levels. 
     Thus we could have different flow control policy per topic, or per QoS.
            
   - Reduce subscription latency
   
     A client does not need to wait for MQTT CONNACK before sending the subscribe or publish packets.
     
     However, EMQX will only begin processing them after the client has established a connection and while the connection is allowed.
     
  Limitations:
  
  - Preserving session state is currently not supported.
  
    This means that if a client needs to reconnect, it will have to resubscribe to the topics it was previously subscribed to over a data stream.
    @TODO also describe the QoS message states
  
  
## QUIC vs TCP/TLS Test Comparisons

In comparison with TCP/TLS testing, MQTT over QUIC performs as follows:

1. When network latency is high, QUIC is able to establish connections and subscribe faster.
2. After disconnecting, with 0-RTT, it could re-establish the connection much quicker than TCP/TLS.
3. QUIC is better than TLS for both server CPU and memory usage when connecting/reconnecting on a large scale.
4. When NAT rebinding, client reconnection response under TCP/TLS is very slow and the message transmission is broken, while QUIC handles it more smoothly and the messages are sent without any impact.
5. In a weak network packet loss and packet transmission disorder environment, TLS shows message congestion and loss due to a poor network environment, while the QUIC server receives slightly jittery data but does not lose messages.

## Future Work

As of now, the MQTT over QUIC is ready for production, users are already testing it in depth and giving good feedback, see [Getting Started](./getting-started.md) to experience it now.

Still, EMQX has not utilized all the features provided by QUIC, such as, broker side stream prioritization, broker side flow control and unreliable datagram.

These features will be addressed in the later releases and, hopefully, become an OASIS standard.
