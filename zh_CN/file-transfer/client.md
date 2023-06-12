# Client Side of File Transfer

Here we describe file transfer from the client's perspective, i.e. some device that wants to upload a file to the broker.

## The Common Flow

* A client device chooses a file to upload and generates a unique `file_id` for the upload. This `file_id` will be used to identify the file transfer session.
* The client device publishes `init` command to the `$file/{file_id}/init` topic. The message's payload contains the file metadata, such as file name, size, and checksum.
* The client sends consecutive `segment` commands to the `$file/{file_id}/{offset}[/{checksum}]` topic. The message's payload contains file data at the specified offset, and the `checksum` optionally contains the checksum of the data.
* The client sends `finish` command to the `$file/{file_id}/fin/{file_size}[/{checksum}]` topic. `file_size` is the size of the whole file, and `checksum` is the checksum of the whole file. The payload of the message is not used.

All commands are published with QOS 1. The success status of each step is reported via RC (return code) of the corresponding MQTT PUBACK message. If an error is reported, this generally means that the client should restart the whole file transfer.

If a client loses connection to the broker, it can resume the file transfer by sending all the unconfirmed commands again.

The final `finish` command may take significant time to proceed because the broker needs to reassemble the file from the chunks and export it to the configured storage. So the client may take advantage of the asynchronous nature of MQTT and continue sending other commands while waiting for the `finish` command to complete.

Also, if a disconnect happens during the `finish` command, the client can resume the file transfer by sending the `finish` command again. If the file transfer is already completed, the broker will immediately respond with success,
otherwise, the response will be sent when the file transfer is completed.


## File Transfer Commands

All file transfer commands are regular MQTT PUBLISH messages with QOS 1 sent to specific topics.

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

* `file_id` is the unique identifier of the file transfer session. The client device generates it, which must be unique across all file transfer sessions.
* `name` is the name of the file. It will be sanitized using percent-encoding if it coincides with reserved file names "." and ".."; if it contains "/", "\", "%", ":" or unprintable unicode characters. The binary length of the name must not exceed 240 bytes.
* `size` is the size of the file in bytes. This is an informational field and is not used by the broker.
* `checksum` is the SHA256 checksum of the file. It is optional, but if provided, the broker will verify the file's checksum after it is uploaded.
* `expire_at` is the timestamp in seconds since the epoch when the file may be deleted from the storage.
* `segments_ttl` is the time-to-live of the file segments in seconds. The value will be clamped to the range `minimum_segments_ttl` to `maximum_segments_ttl` configured in the broker.
* `user_data` is an arbitrary JSON object that will be stored along with the file metadata. It can be used to store any additional information about the file.

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

* `file_id` is the unique identifier of the file transfer session.
* `offset` is the offset in bytes from the beginning of the file where the chunk should be written.

### Finish Command

The `finish` command is used to finish the file transfer session.

Topic: `$file/{file_id}/fin/{file_size}[/{checksum}]`

Payload: not used.

* `file_id` is the unique identifier of the file transfer session.
* `file_size` is the size of the whole file in bytes.
* `checksum` is the SHA256 checksum of the whole file. If specified, this value has a greater priority than the `checksum` field in the `init` command.

The broker will verify that it has all the segments to assemble the file and start exporting to the configured exporter. If the file is successfully exported and its checksum is valid, the broker will respond with success RC. Otherwise, the broker will respond with an error.
