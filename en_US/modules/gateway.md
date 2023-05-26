# Multi-Protocol Gateway

This chapter provides an in-depth examination of various gateway protocols supported by EMQX, offering a comprehensive understanding of their operation, use cases, and potential benefits.

- The **Stomp Gateway** section investigates the Stomp protocol, a simple text-oriented messaging protocol. 
- The **MQTT-SN Gateway** section delves into the MQTT-SN protocol, specifically designed for wireless sensor networks. This part aims to shed light on how MQTT-SN can provide efficient and reliable data transfer in environments with limited bandwidth.
- The **CoAP Gateway** section introduces the CoAP protocol, designed for machine-to-machine (M2M) applications such as smart energy and building automation. 
- The **LwM2M Gateway** section focuses on the LwM2M protocol, an emerging standard for device management and telemetry in the Internet of Things (IoT) space. It provides insights into how the LwM2M gateway operates and its application in various scenarios.
- The **JT/T808 Gateway** section presents an overview of the JT/T808 protocol, commonly used in the Internet of Vehicles. 
- In the **TCP Gateway** section, we have implemented a simple application layer protocol based on the TCP protocol for transparent transmission. It accepts private protocol devices from users and transparently transmits their data to the EMQX Pub/Sub messaging system.
- Finally, the **Multilingual Extended Protocol Access** module allows other programming languages (such as Python, Java, etc.) to directly process byte data messages to achieve custom protocol analysis, and provides Pub/Sub interfaces to achieve message exchange with the system.