# File Transfer over MQTT

In the world of IoT applications, there's a growing need for diverse data transmission, where devices must swiftly and reliably transfer different types of data to and from the cloud. As IoT finds applications across more industries, a variety of use cases are emerging. Beyond real-time structured data like sensor readings and control commands, offline file data such as audio/video files, images, and diagnostic logs are playing an increasingly important role in the IoT field.

EMQX offers file transfer capabilities based on the standard MQTT protocol, ensuring efficiency and reliability in data transfer.

## Limitations of Traditional Methods

In the realm of file transfer, technologies like FTP and HTTP are well-established. Users can combine MQTT with these technologies – using HTTP/FTP for transferring file content and MQTT for transferring events and file location information.

This hybrid approach theoretically maximizes the advantages of each technology to enhance the efficiency and flexibility of file transfer. However, in the IoT environment, users might face the following challenges:

- **Difficulties in Overall Flow Control**: In IoT devices operating in environments with low bandwidth and complex, unreliable networks, it becomes challenging to manage overall data flow and prioritize tasks when multiple connections are simultaneously sending data to the cloud. This is particularly problematic when transferring large files via HTTP/FTP, which may consume bandwidth for extended periods, hindering the transmission of critical MQTT messages.
- **Coexistence of Multiple Technology Stacks**: IoT devices often have limited resources. Some devices may lack the space and processing power to incorporate additional technology stacks. MQTT, being lighter, is more adaptable than HTTP/FTP and can circumvent limitations to complete business development.
- **Additional Development and Management Costs**: Existing MQTT channels already have comprehensive device authentication and authorization systems. Introducing new technologies like FTP and HTTP requires reimplementing these security and management systems, adding extra development and management costs.
- **Performance Limitations**: HTTP/FTP technologies are not suitable for IoT applications that need to support a massive number of connections. Moreover, frequent reconnections and retransmissions under weak network conditions further amplify the disadvantages of HTTP/FTP file transfer, adding significant pressure to application development and operations.

## MQTT File Transfer in EMQX

The MQTT standard does not define a standard method for file transfer. EMQX extends the MQTT protocol to assist client devices in efficient and secure file transfers. It defines and implements a simple application-level protocol built on top of MQTT, enabling client devices to handle file transfers easily.

### Features

File transfer based on MQTT faces several challenges, including ensuring reliability to guarantee 100% file integrity, addressing file management and long-term storage, and adapting to various application file reading interfaces for better service integration.

To address these, the file transfer in EMQX has the following functionalities:

- Support for using the same MQTT connection as other business operations, fully leveraging the existing client management system.

- Support for chunked transfers, meaning lightweight clients can handle large files, and files exceeding MQTT's size limit (256MB) can be transferred.

- Support for resumable transfers, allowing client devices to pause file transfers for higher priority data transmission or to recover from network interruptions.

- Reliability assurance with QoS 1 level transmission, providing checksum and retransmission mechanisms to ensure file transfer integrity.

- Flexible storage layer configuration, with the ability to save uploaded files to a local directory or S3-compatible object storage for later use.

  <!-- "The Dashboard allows listing and downloading files uploaded to the broker." This is not implemented in Dashboard yet.-->

### Advantages

MQTT, as a lightweight and flexible messaging protocol, offers a more convenient and efficient file transfer solution for enterprises. By integrating structured data and file-type data transmission, the scope of achievable IoT business applications expands.

A unified MQTT data channel simplifies system architecture, reducing the complexity and maintenance costs of applications. By utilizing EMQX's MQTT file transfer functionality, enterprises can develop more convenient and diverse IoT applications.

Protocol-wise, EMQX file transfer is based on standard MQTT, with the entire transfer process being executed via specified MQTT topics and payloads, requiring no modifications to existing clients and applications. For a detailed transfer protocol, refer to [File Transfer Client Development](./client.md).

## Next Steps

Learn more about MQTT file transfer in EMQX:

- [File Transfer over MQTT: Transfer Large Payloads via One Protocol with Ease](https://www.emqx.com/en/blog/file-transfer-over-mqtt)

How to use the file transfer feature in EMQX:

- [Quick Start with MQTT File Transfer](./quick-start.md)
- [Configure File Transfer Server-Side Settings](./broker.md)
- [File Transfer Clients Development](./client.md)

