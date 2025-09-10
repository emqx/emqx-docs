## Message Protocol

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

See [signaling_mqtt.js](https://github.com/emqx/emqx-multimedia-proxy/blob/main/apps/emqx_media_proxy_web/assets/js/signaling_mqtt.js) for a complete example of the client-side signaling implementation using MQTT. You can go to http://localhost:4000/webrtc_mqtt to try the demo.


## Normal Messages via MQTT

The multimedia proxy can send normal messages to the device via the following MQTT topic:

- `$message/<device_id>`: The MQTT topic for the device to receive normal messages from the multimedia proxy.
- `$message/<device_id>/multimedia_proxy`: The MQTT topic for the multimedia proxy to receive arbitrary messages from the device, will be sent to the AI agents using the `message_from_device` method.

### The format of the normal messages sent to the device:

The multimedia proxy can send the following types of messages to the device via the `$message/<device_id>` topic.

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

### The format of the arbitrary messages sent from the device to the multimedia proxy:

The device can send arbitrary messages to the multimedia proxy via the `$message/<device_id>/multimedia_proxy` topic. The format of the messages is:

```json
{
    "type": "message",
    "payload": <payload of any format>
}
```


## The Interaction Protocol between Multimedia Proxy and AI Agents

Using AI agents can enhance the capabilities of the multimedia proxy, such as processing ASR results according to specific business logic or sending messages in arbitrary formats to devices.

The multimedia proxy interacts with AI agents using a simple JSON RPC 2.0 based protocol. The messages are sent over STDIO (standard input/output). Messages are delimited by newlines (`\n`), and MUST NOT contain embedded newlines.

- **Initialization**:
    After the STDIO connection is established, the agent must send an initialization message to the multimedia proxy, to negotiate the protocol version and configs:
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

    The AI agents can request the multimedia proxy to perform TTS and send the audio back to the specific device.

    First the agent should send a `tts_and_send_start` message to start a TTS task, and then send one or more `tts_and_send` messages to send the texts to be converted to speech. The texts of the same task can be sent in one batch or in separate messages, but they must have the same `task_id`. Finally, the agent should send a `tts_and_send_finish` message to indicate the end of the TTS task.

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

    The `tts_and_send_start` and `tts_and_send_finish` messages canbe sent in the same batch with the `tts_and_send` messages, or in separate messages.

    The multimedia proxy will acknowledge the request with "ok" or errors:
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
    The multimedia proxy will send messages received from `$message/<device_id>/multimedia_proxy` topic to the AI agents via the `message_from_device` method:
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
    The AI agents can send arbitrary messages to the device via the multimedia proxy:
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

    The multimedia proxy will acknowledge the request with:
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "result": "ok"
    }
    ```
