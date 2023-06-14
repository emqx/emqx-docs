# File Transfer over MQTT

The File Transfer over MQTT feature provided by EMQX enhances the capabilities of MQTT client devices by enabling the transfer of large files to a broker within the MQTT protocol. This feature is particularly useful in IoT applications where file transfers are required, such as streaming video from smart cameras, sending vehicle log files to an analytics server, or transmitting captured images from warehouse robots to a cloud server for auditing. By using the same MQTT protocol clients use for other operations, this feature eliminates the need for implementing additional protocols like HTTP or FTP which cannot support large-scale of connections.

## Disadvantages of Traditional Approaches

Before the introduction of File Transfer over MQTT, IoT devices had to implement other protocols like HTTP or FTP for transferring files. However, the HTTP or FTP protocols cannot support large-scale connections and poses challenges in terms of development, operations, and etc. due to multiple connection channels. This old approach also had some other disadvantages, including the need to implement additional protocols with their associated security and authentication mechanisms. Managing complex states on the client devices and maintaining separate services to handle file transfers further added to the challenges.

## File Transfer over MQTT in EMQX

MQTT specification does not define any standard way to transfer files. Instead, EMQX extends the MQTT protocol to facilitate efficient and secure file transfers. It defines and implements a simple, application-level protocol on top of MQTT, eliminating the need for client devices to handle the complexities associated with file transfers.

### Main Features

The following features are the highlights of the File Transfer over MQTT capability:

* The entire file transfer process takes place over MQTT, using the same connection for other MQTT operations. No additional connections are required.

* The file transfer is chunked so that the process does not consume excessive memory, allowing file transfers to be performed even on lightweight clients. The client device can send the file in small chunks, and the broker will reassemble the chunks into the original file.

* The file transfer is resumable so that the client device can resume the transfer from where it was interrupted.

* The file transfer commands and finalization process are designed to be idempotent. This means that the client device can safely retry chunk transfers or the finalization command without worrying about creating duplicate files on EMQX. 

* The file transfer allows exporting uploaded files to a dedicated local directory on the EMQX or S3-compatible object storage.

  <!-- "The Dashboard allows listing and downloading files uploaded to the broker." This is not implemented in Dashboard yet.-->
