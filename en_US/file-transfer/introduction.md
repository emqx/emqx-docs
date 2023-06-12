# File Transfer over MQTT

While core MQTT protocol features are sufficient for most IoT applications and scenarios, some use cases require MQTT client devices to have the ability to transfer large files to a broker. Examples of such use cases are:
* A smart camera needs to transfer a video stream to cloud storage.
* A vehicle's log files must be sent to an analytics server.
* Warehouse robots need to send captured images to a cloud server for auditing.

For such use cases, client devices usually had to use protocols like HTTP or FTP. This approach has several disadvantages, namely:
* Need to implement other protocols, with their own security and authentication mechanisms, with complex states that should be maintained on the client device.
* Need to have a set of separate services to handle file transfers.

To alleviate these extra needs, EMQX provides a _File Transfer over MQTT_ feature that allows MQTT clients to transfer files to a broker using the same MQTT protocol they use for other operations. MQTT specification does not define any standard way to transfer files. Instead, EMQX defines and implements a simple, application-level protocol on top of MQTT that facilitates file transfers while relieving the client devices from much of the complexity.

The main features of the _File Transfer over MQTT_ are:

* The whole file transfer is done over MQTT, using the same connection for other MQTT operations.
No additional connections are required.
* The file transfer is chunked so that the client device does not need enough memory to hold the whole file in memory. The client device can send the file in small chunks, and the broker will reassemble the chunks into the original file.
* The file transfer is resumable so that the client device can resume the transfer from when it was interrupted.
* The file transfer is idempotent so that the client device can retry chunk transfer commands or the final command without worrying about duplicate files being created on the broker.
* The file transfer allows exporting uploaded files to a dedicated local directory on the broker or S3-compatible object storage.
* The dashboard allows listing and downloading files uploaded to the broker.
