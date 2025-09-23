# 多媒体 AI 消息协议

本文档描述了多媒体服务器与客户端（设备）和 AI 代理之间交互所使用的消息协议。

## 通过 MQTT 发送和接收 WebRTC 信令

建立 MQTT 连接后，客户端需使用以下 MQTT 主题来建立 WebRTC 连接：

- `$webrtc/<device_id>/multimedia_proxy`：客户端与多媒体服务器之间用于 WebRTC 连接建立的信令消息 MQTT 主题。客户端应订阅此主题以接收来自多媒体服务器的信令消息。
- `$webrtc/<device_id>`：设备用于接收信令消息的 MQTT 主题。

客户端应将 `offer` 和 `candidate` 消息发送到 `$webrtc/<device_id>/multimedia_proxy` 主题，并在 `$webrtc/<device_id>` 主题上等待来自多媒体服务器的 `answer` 和 `candidate` 消息，以建立 WebRTC 连接。

WebRTC 连接信令消息格式如下：

```json
{
    "type": "sdp_offer",
    "data": {
        "sdp": <SDP offer 的内容>,
        "type": "offer"
    }
}
```

```json
{
    "type": "sdp_answer",
    "data": {
        "sdp": <SDP answer 的内容>,
        "type": "answer"
    }
}
```

```json
{
    "type": "ice_candidate",
    "data": {
        "candidate": <ICE candidate 的内容>,
        "sdpMid": <ICE candidate 的 sdpMid>,
        "sdpMLineIndex": <ICE candidate 的 sdpMLineIndex>,
        "usernameFragment": <ICE candidate 的 usernameFragment>
    }
}
```

上述 `data` 字段可通过浏览器中的 [RTCPeerConnection API](https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection) 生成，例如：

```javascript
// 创建 offer
const offer = await pc.createOffer();
await pc.setLocalDescription(offer);
const message = {
    type: "sdp_offer",
    data: offer
};
// 通过 MQTT 向多媒体服务器发送消息
mqttClient.publish(`$webrtc/${deviceId}/multimedia_proxy`, JSON.stringify(message));
```

```javascript
// 处理来自多媒体服务器的 answer
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

当 WebRTC 连接终止时，多媒体服务器会向客户端发送 `webrtc_terminated` 消息：

```json
{
    "type": "webrtc_terminated",
    "reason": "终止原因"
}
```

## 通过 MQTT 发送普通消息

多媒体服务器可通过以下 MQTT 主题向设备发送普通消息：

- `$message/<device_id>`：多媒体服务器向设备发送普通消息的 MQTT 主题。
- `$message/<device_id>/multimedia_proxy`：设备向多媒体服务器发送任意消息的 MQTT 主题，消息会通过 `message_from_device` 方法转发给 AI 代理。

### 发送给设备的普通消息格式

多媒体服务器可通过 `$message/<device_id>` 主题向设备发送如下类型的消息：

当有 ASR 结果时，发送 `asr_response` 消息：

```json
{
    "type": "asr_response",
    "format": "merged" | "raw",
    "results": <如果为 merged，则为识别文本；如果为 raw，则为 ASR 结果的 JSON 数组>
}
```

当 TTS 任务开始时，发送 `tts_begin` 消息：

```json
{
    "type": "tts_begin",
    "task_id": "task_id"
}
```

当文本被转换为语音且文本也需发送给设备时，发送 `tts_text` 消息：

```json
{
    "type": "tts_text",
    "task_id": "task_id",
    "text": "text"
}
```

当 TTS 任务完成时，发送 `tts_complete` 消息：

```json
{
    "type": "tts_complete",
    "task_id": "task_id"
}
```

当 TTS 任务结束或被终止时，发送 `tts_terminate` 消息：

```json
{
    "type": "tts_terminate",
    "task_id": "task_id"
}
```

当代理向设备发送任意消息（通过 `message_to_device` 方法）时，发送 `message` 消息：

```json
{
    "type": "message",
    "payload": <任意格式的内容>
}
```

### 设备发送给多媒体服务器的任意消息格式

设备可通过 `$message/<device_id>/multimedia_proxy` 主题向多媒体服务器发送任意消息，格式如下：

```json
{
    "type": "message",
    "payload": <任意格式的内容>
}
```

## 多媒体服务器与 AI 代理的交互协议

通过 AI 代理可增强多媒体服务器的能力，例如根据业务逻辑处理 ASR 结果或向设备发送任意格式的消息。

多媒体服务器与 AI 代理之间通过简单的 JSON RPC 2.0 协议进行交互，消息通过 STDIO（标准输入输出）发送。消息以换行符（`\n`）分隔，且消息内容不得包含嵌入的换行符。

- **初始化**：
    建立 STDIO 连接后，代理需向多媒体服务器发送初始化消息，协商协议版本和配置：
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "method": "init",
        "params": {
            "protocol_version": "1.0",
            "configs": {
                "asr": {
                    // 若启用，服务器会在每次有新 ASR 结果时发送合并后的文本，否则由代理负责合并 ASR 结果
                    "auto_merge": false
                }
            }
        }
    }
    ```

    多媒体服务器会回复确认消息：
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "result": "ok"
    }
    ```

- **ASR 结果**：
    多媒体服务器会以通知形式向 AI 代理发送 ASR 结果，格式如下：
    ```json
    {
        "jsonrpc": "2.0",
        "method": "asr_result",
        "params": {
            // 当前设备 ID
            "device_id": "device_id",
            "text": "识别文本"
        }
    }
    ```

- **TTS 及发送**：

    AI 代理可请求多媒体服务器进行 TTS 并将音频发送给指定设备。

    首先代理需发送 `tts_and_send_start` 消息以启动 TTS 任务，然后发送一个或多个 `tts_and_send` 消息以发送待转换为语音的文本。相同任务的文本可批量或分多次发送，但需使用相同的 `task_id`。最后，代理需发送 `tts_and_send_finish` 消息表示 TTS 任务结束。

    启动消息：
    ```json
    {
        "jsonrpc": "2.0",
        "id": "3",
        "method": "tts_and_send_start",
        "params": {
            // 发送音频的设备 ID
            "device_id": "device_id",
            // 
            "task_id": "aaa",
            "text": "待转换为语音的文本"
        }
    }
    ```

    待转换文本可批量发送：
    ```json
    [
        {
            "jsonrpc": "2.0",
            "id": "4",
            "method": "tts_and_send",
            "params": {
                // 发送音频的设备 ID
                "device_id": "device_id",
                // 
                "task_id": "aaa",
                "text": "待转换为语音的文本"
            }
        },
        {
            "jsonrpc": "2.0",
            "id": "5",
            "method": "tts_and_send",
            "params": {
                // 发送音频的设备 ID
                "device_id": "device_id",
                // 
                "task_id": "aaa",
                "text": "，更多文本可批量发送"
            }
        },
        {
        "jsonrpc": "2.0",
        "id": "6",
        "method": "tts_and_send_finish",
        "params": {
            // 发送音频的设备 ID
            "device_id": "device_id",
            // TTS 任务的 task_id
            "task_id": "aaa"
        }
    }
    ]
    ```

    `tts_and_send_start` 和 `tts_and_send_finish` 消息可与 `tts_and_send` 消息一起批量发送，也可单独发送。

    多媒体服务器会以 "ok" 或错误进行确认：
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

- **图像分析**：
    AI 代理可请求多媒体服务器进行图像分析：
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "method": "image_analysis",
        "params": {
            // 要采集图像的设备 ID
            "device_id": "device_id",
            // 要采集和分析的图像数量
            "image_count": 2,
            "capture_interval": 1000, // 采集间隔（毫秒）
            "image_format": "jpeg", // 图像格式
            "user_prompt": "分析图像并提供见解"
        }
    }
    ```

    多媒体服务器会返回分析结果：
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "result": {
            "analysis_result": "分析结果"
        }
    }
    ```

- **转发设备消息**：
    多媒体服务器会将收到的 `$message/<device_id>/multimedia_proxy` 主题消息通过 `message_from_device` 方法转发给 AI 代理：
    ```json
    {
        "jsonrpc": "2.0",
        "method": "message_from_device",
        "params": {
            // 发送消息的设备 ID
            "device_id": "device_id",
            "payload": "内容"
        }
    }
    ```

- **发送消息到设备**：
    AI 代理可通过多媒体服务器向设备发送任意消息：
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "method": "message_to_device",
        "params": {
            // 要发送消息的设备 ID
            // 消息将通过 `$message/<device_id>` MQTT 主题发送给设备
            "device_id": "device_id",
            // 也可手动指定主题向任意设备发送消息
            // 若指定，则忽略 device_id 字段
            "topic": "topic/to/device",
            "payload": "内容"
        }
    }
    ```

    多媒体服务器会回复确认消息：
    ```json
    {
        "jsonrpc": "2.0",
        "id": "unique_id",
        "result": "ok"
    }
    ```
