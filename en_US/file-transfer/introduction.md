# File Transfer over MQTT

The File Transfer over MQTT feature provided by EMQX enhances the capabilities of MQTT client devices by enabling the transfer of large files to EMQX using the MQTT protocol. This feature is particularly useful in IoT applications where file transfers are required, such as streaming video from smart cameras, sending vehicle log files to an analytics server, or transmitting captured images from warehouse robots to a cloud server for auditing. By using the same MQTT protocol clients use for other operations, this feature eliminates the need for implementing additional protocols like HTTP or FTP.

## Disadvantages of Traditional Approaches

Before the introduction of File Transfer over MQTT, IoT devices had to implement other protocols like HTTP or FTP for transferring files. However, the HTTP or FTP protocols cannot support large-scale connections and poses challenges in terms of development, operations, and etc. due to multiple connection channels. This conventional approach also had some other disadvantages, including the need to implement additional protocols with their associated security and authentication mechanisms. Managing complex states on the client devices and maintaining separate services to handle file transfers further added to the challenges.

## File Transfer over MQTT in EMQX

MQTT specification does not define any standard way to transfer files. Instead, EMQX extends the MQTT protocol to facilitate efficient and secure file transfers. It defines and implements a simple, application-level protocol on top of MQTT, eliminating the need for client devices to handle the complexities associated with file transfers.

### Main Features

The following features are the highlights of the File Transfer over MQTT capability:

* Supports sharing the same MQTT connection with other services, fully leveraging the existing client management system.

* Enables chunked transmission, allowing lightweight clients to handle large files and facilitating the transfer of files exceeding the MQTT protocol size limit (256MB).

* Supports resumable file transfer, allowing client devices to pause file transmission at any time for higher-priority data transfers or to resume transmission after a network interruption.

* Ensures reliability by using QoS 1 level messages for transmission, providing verification and retransmission mechanisms to ensure file transfer integrity.

* Offers flexible storage configuration, allowing uploaded files to be saved to a designated local directory or an S3-compatible object storage for convenient future use.

  <!-- "The Dashboard allows listing and downloading files uploaded to the broker." This is not implemented in Dashboard yet.-->
