# Commands for File Transfer Client Development

This page provides an overview of the file transfer process from the client's perspective along with detailed information on the commands utilized for uploading a file to EMQX. This information helps you to implement the file transfer feature for client development.

## The Common Flow

The typical flow for file transfer from the client side involves the following steps:

1. A client device selects a file to upload and generates a unique `file_id` for the transfer session. This `file_id` will be used to identify the file transfer session.
2. The client device publishes `init` command to the `$file/{file_id}/init` topic. The payload of the message contains the file metadata, including the file name, size, and checksum.
3. The client sends consecutive `segment` commands to the `$file/{file_id}/{offset}[/{checksum}]` topic. Each `segment` command carries a chunk of the file data at the specified offset. The `checksum` field is optional and can contain the checksum of the data.
4. The client sends `finish` command to the `$file/{file_id}/fin/{file_size}[/{checksum}]` topic. The payload of this message is not used. The `file_size` parameter represents the total size of the file, while the `checksum` parameter contains the checksum of the entire file.

All commands are published with QoS 1, ensuring reliability. The success status of each step is reported through the return code (RC) of the corresponding MQTT PUBACK message. If an error occurs, the client is typically required to restart the entire file transfer process. In case of a disconnection, the client can resume the file transfer by re-sending the unconfirmed commands.

The `finish` command may take considerable time to process because the EMQX needs to assemble the file from the received segments and export it to the configured storage. During this time, the client can continue sending other commands while waiting for the `finish` command to complete. If a disconnect happens during the `finish` command, the client can simply resend the command to resume the file transfer. If the file transfer has already been completed, EMQX will immediately respond with success.


## File Transfer Commands

The file transfer commands are regular MQTT PUBLISH messages with QoS 1 sent to specific topics.

### Init Command

The `init` command is used to initialize a file transfer session.

Topic: `$file/{file_id}/init`

Payload: a JSON object with the following fields:
```
{
  "name": "{name}",
  "size": {size},
  "checksum": "{checksum}"
  "expire_at": {expire_at}
  "segments_ttl": {segments_ttl}
  "user_data": {user_data}
}
```

* `file_id`: Unique identifier for the file transfer session.
* `name`: Name of the file. It will be sanitized using percent-encoding if it coincides with reserved file names (such as ".", "..") or if it contains characters like "/", "", "%", ":", or unprintable unicode characters. The binary length of the name should not exceed 240 bytes.
* `size`: Size of the file in bytes (informational field).
* `checksum`: SHA256 checksum of the file (optional). EMQX will verify the file's checksum if provided.
* `expire_at`: Timestamp (in seconds since the epoch) when the file may be deleted from storage.
* `segments_ttl`: Time-to-live of the file segments in seconds. This value is clamped to the range specified by the `minimum_segments_ttl` and `maximum_segments_ttl` settings configured in EMQX. Refer to [Segment Settings](./broker.md#segment-settings) for details.
* `user_data`: Arbitrary JSON object to store additional information about the file along with its metadata.

In the payload, the only required field is `name`.

Example:
```json
{
  "name": "ml-logs-data.log",
  "size": 12345,
  "checksum": "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef",
  "expire_at": 1696659943,
  "segments_ttl": 600
}
```

### Segment Command

The `segment` command is used to upload a chunk of the file.

Topic: `$file/{file_id}/{offset}[/{checksum}]`

Payload: the binary data of the file chunk.

* `file_id`: Unique identifier for the file transfer session.
* `offset`: Offset in bytes from the beginning of the file where the chunk should be written.

### Finish Command

The `finish` command is used to finish the file transfer session.

Topic: `$file/{file_id}/fin/{file_size}[/{checksum}]`

Payload: not used.

* `file_id`: Unique identifier for the file transfer session.
* `file_size`: Total size of the file in bytes.
* `checksum`: SHA256 checksum of the entire file. If specified, this value takes priority over the `checksum` field provided in the `init` command.

Upon receiving the `finish` command, EMQX verifies that it has received all the segments necessary to assemble the file. If the file is successfully exported and its checksum is valid, EMQX responds with a success return code (RC). In case of any errors, an appropriate error response is sent.
