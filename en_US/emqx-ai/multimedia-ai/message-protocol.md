# Multimedia AI Messaging Protocol

This document describes the message protocol used for interaction between the multimedia server, clients (devices), and AI agents.

## WebRTC Signaling via MQTT

After establishing the MQTT connection, the client needs to use the following MQTT topic to set up the WebRTC connection:

- `$webrtc/<device_id>/multimedia_proxy`: The MQTT topic for signaling messages between the client and the multimedia proxy for WebRTC connection setup. The client should subscribe to this topic to receive signaling messages from the multimedia proxy.

- `$webrtc/<device_id>`: The MQTT topic for the device to receive signaling messages.

The client should send `offer` and `candidate` messages to the `$webrtc/<device_id>/multimedia_proxy` topic and wait for `answer` and `candidate` messages from the multimedia proxy on the `$webrtc/<device_id>` topic to establish the WebRTC connection.

The format of the signaling messages for setting up the WebRTC connections:

```json
{
    "type": "sdp_offer",
    "data": {
        "sdp": <payload of the SDP offer>,
        "type": "offer"
    }
}
```

```json
{
    "type": "sdp_answer",
    "data": {
        "sdp": <payload of the SDP answer>,
        "type": "answer"
    }
}
```

```json
{
    "type": "ice_candidate",
    "data": {
        "candidate": <payload of the ICE candidate>,
        "sdpMid": <sdpMid of the ICE candidate>,
        "sdpMLineIndex": <sdpMLineIndex of the ICE candidate>,
        "usernameFragment": <usernameFragment of the ICE candidate>
    }
}
```

The `data` field above can be generated using the [RTCPeerConnection API](https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection) in the browser, for example:

```javascript
// Create an offer
const offer = await pc.createOffer();
await pc.setLocalDescription(offer);
const message = {
    type: "sdp_offer",
    data: offer
};
// Send the message to the multimedia proxy via MQTT
mqttClient.publish(`$webrtc/${deviceId}/multimedia_proxy`, JSON.stringify(message));
```

```javascript
// Handle the answer from the multimedia proxy
mqttClient.on('message', (topic, message) => {
    const msg = JSON.parse(message.toString());
    if (msg.type === 'sdp_answer') {
        const answer = msg.data;
        pc.setRemoteDescription(new RTCSessionDescription(answer));
    } else if (msg.type === 'ice_candidate') {
        const candidate = new RTCIceCandidate(msg.data);
        pc.addIceCandidate(candidate);
    }
});
```

The multimedia proxy will send the `webrtc_terminated` message to the client when the WebRTC connection is terminated:

```json
{
    "type": "webrtc_terminated",
    "reason": "reason for termination"
}
```


## Send General Messages via MQTT

The multimedia server and devices exchange general messages through the following MQTT topics:

- **`$message/<device_id>`**: Topic for the multimedia server to send general messages to a device.
- **`$message/<device_id>/multimedia_proxy`**: Topic for a device to send arbitrary messages to the multimedia server. These messages are forwarded to the AI Agent via the `message_from_device` method.

### Messages Sent from Multimedia Server to Devices

The multimedia server can publish the following message types on the `$message/<device_id>` topic:

A `asr_response` message is sent when ASR results are available:

```json
{
    "type": "asr_response",
    "format": "merged" | "raw",
    "results": <Recognized text if merged or JSON array of ASR results if raw>
}
```

A `tts_begin` message is sent when a TTS task is started:

```json
{
    "type": "tts_begin",
    "task_id": "task_id"
}
```

A `tts_text` message is sent when a text is converted to speech and the text should also be sent to the device:

```json
{
    "type": "tts_text",
    "task_id": "task_id",
    "text": "text"
}
```

A `tts_complete` message is sent when the TTS task is completed:

```json
{
    "type": "tts_complete",
    "task_id": "task_id"
}
```

A `tts_terminate` message is sent when the TTS task is finished or terminated:

```json
{
    "type": "tts_terminate",
    "task_id": "task_id"
}
```

A `message` message is sent to device when the agent sends an arbitrary message to the device (by the `message_to_device` method):

```json
{
    "type": "message",
    "payload": <payload of any format>
}
```

### Messages Sent from Devices to the Multimedia Server

Devices can publish arbitrary messages to the **`$message/<device_id>/multimedia_proxy`** topic.
 The multimedia server will forward these messages to the AI Agent:

```json
{
    "type": "message",
    "payload": <payload of any format>
}
```


## Interaction Protocol between Multimedia Server and AI Agent

Using AI agents can can extend the capabilities of the Multimedia Server, for example by processing ASR results according to business logic or sending custom messages to devices.

The multimedia proxy interacts with AI agents using a simple JSON RPC 2.0 based protocol. The messages are sent over Standard Input/Output (STDIO). Messages are delimited by newlines (`\n`), and MUST NOT contain embedded newlines.

- **Initialization**:
    After the STDIO connection is established, the agent must send an initialization message to the multimedia proxy, to negotiate the protocol version and configuration:
    
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "method": "init",
        "params": {
            "protocol_version": "1.0",
            "configs": {
                "asr": {
                    // If enabled, multimedia proxy will send merged ASR text (based on the timestamps of the sentences) every time a new ASR result is available, otherwise it is the agent's responsibility to merge the ASR results.
                    "auto_merge": false
                }
            }
        }
    }
    ```
    
    The multimedia proxy will respond with an acknowledgment:
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "result": "ok"
    }
    ```
    
- **ASR Result**:
    The multimedia proxy sends the ASR results as notifications to the AI agents in the following format:
    ```json
    {
        "jsonrpc": "2.0",
        "method": "asr_result",
        "params": {
            // The current device ID
            "device_id": "device_id",
            "text": "Recognized text"
        }
    }
    ```

- **TTS and Send**:

    The AI Agent can request the Multimedia Server to perform **TTS** and send the audio to a target device.

    1. The agent sends a `tts_and_send_start` message to initiate the task.
    2. The agent sends one or more `tts_and_send` messages to provide the text to be synthesized.
       - Multiple texts for the same task can be sent in batches or sequentially, but must use the same `task_id`.
    3. Finally, the agent sends a `tts_and_send_finish` message to signal the end of the task.

    The start message:
    ```json
    {
        "jsonrpc": "2.0",
        "id": "3",
        "method": "tts_and_send_start",
        "params": {
            // The deivce ID to send the audio to
            "device_id": "device_id",
            // 
            "task_id": "aaa",
            "text": "Text to be converted to speech"
        }
    }
    ```

    The texts to be converted to speech can be sent in one batch:
    ```json
    [
        {
            "jsonrpc": "2.0",
            "id": "4",
            "method": "tts_and_send",
            "params": {
                // The deivce ID to send the audio to
                "device_id": "device_id",
                // 
                "task_id": "aaa",
                "text": "Text to be converted to speech"
            }
        },
        {
            "jsonrpc": "2.0",
            "id": "5",
            "method": "tts_and_send",
            "params": {
                // The deivce ID to send the audio to
                "device_id": "device_id",
                // 
                "task_id": "aaa",
                "text": ", and more text can be send in one batch"
            }
        },
        {
        "jsonrpc": "2.0",
        "id": "6",
        "method": "tts_and_send_finish",
        "params": {
            // The device ID to send the audio to
            "device_id": "device_id",
            // The task ID of the TTS task
            "task_id": "aaa"
        }
    }
    ]
    ```

    The `tts_and_send_start` and `tts_and_send_finish` messages can be sent either in the same batch as `tts_and_send` messages or separately.

    The Multimedia Server confirms each message with `"ok"` or returns an error:

    ```json
    [
        {
            "jsonrpc": "2.0",
            "id": "4",
            "result": "ok"
        },
        {
            "jsonrpc": "2.0",
            "id": "5",
            "result": "ok"
        },
        {
            "jsonrpc": "2.0",
            "id": "6",
            "result": "ok"
        }
    ]
    ```

- **Image Analysis**:
    The AI agents can request the multimedia proxy to perform image analysis:
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "method": "image_analysis",
        "params": {
            // The ID of the device to capture images from
            "device_id": "device_id",
            // The count of images to capture and analyze
            "image_count": 2,
            "capture_interval": 1000, // Interval in milliseconds between captures
            "image_format": "jpeg", // Format of the captured images
            "user_prompt": "Analyze the images and provide insights"
        }
    }
    ```

    The multimedia proxy will respond with the analysis results:
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "result": {
            "analysis_result": "Analysis result"
        }
    }
    ```

- **Forward Messages Received from Device**:
    The Multimedia Server forwards messages received on the `$message/<device_id>/multimedia_proxy` topic to the AI Agent using the `message_from_device` method:
    
    ```json
    {
        "jsonrpc": "2.0",
        "method": "message_from_device",
        "params": {
            // The ID of the device that sent the message
            "device_id": "device_id",
            "payload": "payload"
        }
    }
    ```
    
- **Send Message to Device**:
    The AI Agent can send arbitrary messages to devices through the Multimedia Server:
    
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "method": "message_to_device",
        "params": {
            // The ID of the device to send the message to
            // The message will be sent to the device via the `$message/<device_id>` MQTT topic
            "device_id": "device_id",
            // Or you can specify the topic manually to send message to any device
            // If specified, the `device_id` field will be ignored
            "topic": "topic/to/device",
            "payload": "payload"
        }
    }
    ```
    
    The Multimedia Server responds with a confirmation:
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "result": "ok"
    }
    ```
