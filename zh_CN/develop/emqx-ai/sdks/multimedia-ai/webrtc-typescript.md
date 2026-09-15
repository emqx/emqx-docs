# TypeScript WebRTC 示例

本文档演示如何使用 TypeScript 通过 MQTT 信令与 EMQX 多媒体服务进行 WebRTC 交互，实现音视频通话、语音识别和文字转语音功能。

EMQX 多媒体服务通过 MQTT 协议进行 WebRTC 信令交换，客户端可以轻松集成音视频通话功能，并享受 AI 驱动的语音识别（ASR）和文字转语音（TTS）服务。

## 核心架构

```shell
TypeScript Client ←→ MQTT 信令 ←→ EMQX 多媒体服务
                   ↓
              WebRTC P2P 连接
```

**关键 MQTT 主题**:
- `$webrtc/{device_id}` - 接收服务端的 SDP answer 和 ICE candidates
- `$webrtc/{device_id}/multimedia_proxy` - 发送 SDP offer 和 ICE candidates
- `$message/{device_id}` - 接收 ASR、TTS 和消息数据

## 核心实现

### WebRTC 信令类详解

`MqttWebRTCSignaling` 是核心的信令管理类，负责处理 MQTT 消息传递、WebRTC 连接建立和多媒体服务交互。该类封装了复杂的信令逻辑，提供简洁的 API 供开发者使用。

```typescript
import type { MqttClient } from 'mqtt'

interface SignalingMessage {
  type: 'sdp_offer' | 'sdp_answer' | 'ice_candidate' | 'webrtc_terminated'
      | 'asr_response' | 'tts_begin' | 'tts_text' | 'tts_complete' | 'tts_terminate'
      | 'chat' | 'message'
  data?: any
  payload?: any
  reason?: string
  results?: string
  text?: string
  task_id?: string
}

interface WebRTCCallbacks {
  onLocalStream?: (stream: MediaStream) => void
  onRemoteStream?: (stream: MediaStream) => void
  onConnectionStateChange?: (state: string) => void
  onASRResponse?: (text: string) => void
  onTTSText?: (text: string) => void
  onMessage?: (message: string) => void
  onError?: (error: Error) => void
}

export class MqttWebRTCSignaling {
  private mqttClient: MqttClient
  private pc: RTCPeerConnection | null = null
  private localStream: MediaStream | null = null
  private remoteStream: MediaStream | null = null
  private clientId: string
  private callbacks: WebRTCCallbacks
  private messageHandler: ((topic: string, message: Buffer) => void) | null = null
  private ttsText: string = ''

  constructor(options: {
    mqttClient: MqttClient
    clientId: string
    callbacks?: WebRTCCallbacks
  }) {
    this.mqttClient = options.mqttClient
    this.clientId = options.clientId
    this.callbacks = options.callbacks || {}
  }

  async connect(): Promise<void> {
    if (!this.mqttClient.connected) {
      throw new Error('MQTT client is not connected')
    }

    // 订阅主题
    await this.subscribeToTopics()

    // 设置消息处理
    this.setupMessageHandler()

    // 初始化 WebRTC
    await this.setupWebRTC()
  }

  private async subscribeToTopics(): Promise<void> {
    const topics = [
      `$webrtc/${this.clientId}`,
      `$message/${this.clientId}`
    ]

    return new Promise((resolve, reject) => {
      this.mqttClient.subscribe(topics, { qos: 0 }, (err) => {
        if (err) reject(err)
        else resolve()
      })
    })
  }

  private setupMessageHandler(): void {
    this.messageHandler = (topic: string, message: Buffer) => {
      if (topic === `$webrtc/${this.clientId}` || topic === `$message/${this.clientId}`) {
        try {
          const payload = JSON.parse(message.toString()) as SignalingMessage
          this.handleSignalingMessage(topic, payload)
        } catch (error) {
          console.error('Failed to parse message:', error)
        }
      }
    }

    this.mqttClient.on('message', this.messageHandler)
  }

  private async handleSignalingMessage(topic: string, message: SignalingMessage): Promise<void> {
    if (topic === `$webrtc/${this.clientId}`) {
      // WebRTC 信令处理
      switch (message.type) {
        case 'sdp_answer':
          if (this.pc && message.data) {
            const answer = new RTCSessionDescription({
              type: 'answer',
              sdp: message.data.sdp || message.data
            })
            await this.pc.setRemoteDescription(answer)
          }
          break

        case 'ice_candidate':
          if (this.pc && message.data) {
            const candidate = new RTCIceCandidate(message.data)
            await this.pc.addIceCandidate(candidate)
          }
          break

        case 'webrtc_terminated':
          this.callbacks.onError?.(new Error(`WebRTC terminated: ${message.reason}`))
          this.disconnect()
          break
      }
    } else if (topic === `$message/${this.clientId}`) {
      // 多媒体服务消息处理
      switch (message.type) {
        case 'asr_response':
          this.callbacks.onASRResponse?.(message.results || '')
          break

        case 'tts_begin':
          this.ttsText = ''
          break

        case 'tts_text':
          this.ttsText += message.text || ''
          this.callbacks.onTTSText?.(this.ttsText)
          break

        case 'tts_complete':
          console.log('TTS complete:', message.task_id)
          break

        case 'tts_terminate':
          console.log('TTS terminated:', message.task_id)
          break

        case 'message':
          this.callbacks.onMessage?.(message.payload || '')
          break
      }
    }
  }

  private async setupWebRTC(): Promise<void> {
    // 获取用户媒体
    this.localStream = await navigator.mediaDevices.getUserMedia({
      video: true,
      audio: {
        echoCancellation: true,
        noiseSuppression: true,
        autoGainControl: true,
        sampleRate: 48000
      }
    })

    this.callbacks.onLocalStream?.(this.localStream)

    // 创建 RTCPeerConnection
    this.pc = new RTCPeerConnection({
      iceServers: [{ urls: 'stun:stun.l.google.com:19302' }]
    })
    this.remoteStream = new MediaStream()

    // 添加本地流
    this.localStream.getTracks().forEach(track => {
      this.pc!.addTrack(track, this.localStream!)
    })

    // 设置事件处理
    this.setupPeerConnectionHandlers()

    // 创建并发送 offer
    const offer = await this.pc.createOffer()
    await this.pc.setLocalDescription(offer)
    this.sendSignal('sdp_offer', offer)
  }

  private setupPeerConnectionHandlers(): void {
    if (!this.pc) return

    // ICE 候选处理
    this.pc.onicecandidate = (event) => {
      if (event.candidate) {
        this.sendSignal('ice_candidate', event.candidate)
      }
    }

    // 连接状态变化
    this.pc.onconnectionstatechange = () => {
      if (this.pc) {
        this.callbacks.onConnectionStateChange?.(this.pc.connectionState)

        if (this.pc.connectionState === 'failed') {
          this.callbacks.onError?.(new Error('Connection failed'))
        }
      }
    }

    // 远程流处理
    this.pc.ontrack = (event) => {
      if (this.remoteStream) {
        this.remoteStream.addTrack(event.track)
        this.callbacks.onRemoteStream?.(this.remoteStream)
      }
    }
  }

  private sendSignal(type: string, data: any): void {
    const message = JSON.stringify({ type, data })
    const topic = `$webrtc/${this.clientId}/multimedia_proxy`

    this.mqttClient.publish(topic, message, { qos: 0 }, (err) => {
      if (err) {
        console.error(`Failed to send ${type}:`, err)
      }
    })
  }

  // 音视频控制
  toggleAudio(enabled?: boolean): void {
    if (!this.localStream) return

    this.localStream.getAudioTracks().forEach(track => {
      track.enabled = enabled !== undefined ? enabled : !track.enabled
    })
  }

  toggleVideo(enabled?: boolean): void {
    if (!this.localStream) return

    this.localStream.getVideoTracks().forEach(track => {
      track.enabled = enabled !== undefined ? enabled : !track.enabled
    })
  }

  // 发送聊天消息
  sendChatMessage(message: string): void {
    this.sendSignal('chat', message)
  }

  disconnect(): void {
    // 停止媒体流
    if (this.localStream) {
      this.localStream.getTracks().forEach(track => track.stop())
      this.localStream = null
    }

    if (this.remoteStream) {
      this.remoteStream.getTracks().forEach(track => track.stop())
      this.remoteStream = null
    }

    // 关闭 peer connection
    if (this.pc) {
      this.pc.close()
      this.pc = null
    }

    // 清理消息处理器
    if (this.mqttClient && this.messageHandler) {
      this.mqttClient.removeListener('message', this.messageHandler)
      this.messageHandler = null
    }

    // 取消订阅
    const topics = [`$webrtc/${this.clientId}`, `$message/${this.clientId}`]
    this.mqttClient.unsubscribe(topics)

    this.callbacks.onConnectionStateChange?.('disconnected')
  }
}
```

## 使用示例

以下是基础的使用示例，展示核心 API 的使用方法。该 WebRTC 信令类可以在各种前端环境中使用，包括 Vue、React 等框架，也可以直接在原生 JavaScript 和 HTML 中实现。

完整的实现示例可以查看：[MCP AI Companion Demo](https://github.com/emqx/mcp-ai-companion-demo) 项目中的 web 部分。

### 基础连接和回调设置

创建 WebRTC 连接需要先建立 MQTT 连接，然后配置各种回调函数来处理音视频流和 AI 服务响应。

```typescript
import mqtt from 'mqtt'
import { MqttWebRTCSignaling } from './mqtt-webrtc-signaling'

// 连接 MQTT
const mqttClient = mqtt.connect('ws://broker.emqx.io:8083/mqtt', {
  clientId: 'device_123',
  username: 'your-username',
  password: 'your-password'
})

// 创建 WebRTC 信令
const signaling = new MqttWebRTCSignaling({
  mqttClient,
  clientId: 'device_123',
  callbacks: {
    onLocalStream: (stream) => {
      document.getElementById('localVideo').srcObject = stream
    },
    onRemoteStream: (stream) => {
      document.getElementById('remoteVideo').srcObject = stream
    },
    onASRResponse: (text) => {
      console.log('语音识别:', text)
    },
    onTTSText: (text) => {
      console.log('AI 回复:', text)
    },
    onConnectionStateChange: (state) => {
      console.log('连接状态:', state)
    }
  }
})

// 开始连接
await signaling.connect()
```

### 多媒体功能控制

连接建立后，可以通过简单的 API 调用来控制音视频设备、发送消息和管理连接状态。

```typescript
// 音视频控制
signaling.toggleAudio(false)  // 静音
signaling.toggleVideo(true)   // 开启摄像头

// 发送聊天消息
signaling.sendChatMessage('Hello AI!')

// 断开连接
signaling.disconnect()
```

## 关键交互说明

### MQTT 信令交换流程

WebRTC 连接建立通过 MQTT 主题进行信令交换，遵循标准的 offer/answer 模式：

```
1. 客户端创建 offer → 发送到 $webrtc/{device_id}/multimedia_proxy
2. 服务端处理 offer → 返回 answer 到 $webrtc/{device_id}
3. 双方交换 ICE candidates → 通过上述两个主题进行
4. 建立 P2P 连接 → 开始音视频传输
```

### 多媒体 AI 服务

EMQX 多媒体服务提供三种核心 AI 功能，通过 `$message/{device_id}` 主题进行数据交换：

- **ASR (语音识别)**: 实时识别用户语音并返回文本结果，支持流式识别
- **TTS (文字转语音)**: 将文本转换为自然语音，支持流式文本推送
- **智能对话**: 结合 ASR 和 TTS，实现完整的语音交互体验

### 错误处理和最佳实践

在实际应用中，需要处理各种可能出现的错误情况：

```typescript
// 常见错误处理
callbacks: {
  onError: (error) => {
    if (error.message.includes('getUserMedia')) {
      alert('无法访问摄像头/麦克风，请检查权限')
    } else if (error.message.includes('Connection failed')) {
      // 自动重连逻辑
      setTimeout(() => signaling?.connect(), 2000)
    }
  }
}
```

**常见问题解决方案**：
- **媒体权限问题**: 确保浏览器已授权摄像头和麦克风访问
- **网络连接问题**: 实现自动重连机制，处理网络波动
- **信令超时**: 设置合理的连接超时时间，提供用户友好的错误提示

## 高级配置

### ICE 服务器配置

为确保 WebRTC 连接在各种网络环境下正常工作，需要配置适当的 ICE 服务器：

```typescript
const iceServers = [
  { urls: 'stun:stun.l.google.com:19302' },  // 公共 STUN 服务器
  {
    urls: 'turn:your-turn-server.com:3478',   // 私有 TURN 服务器（生产环境推荐）
    username: 'turnuser',
    credential: 'turnpass'
  }
]
```

### 媒体约束优化

根据应用场景调整音视频质量参数，平衡质量和带宽消耗：

```typescript
const mediaConstraints = {
  video: {
    width: { ideal: 1280 },    // 视频宽度
    height: { ideal: 720 },    // 视频高度
    frameRate: { ideal: 30 }   // 帧率
  },
  audio: {
    echoCancellation: true,    // 回声消除
    noiseSuppression: true,    // 噪声抑制
    autoGainControl: true,     // 自动增益控制
    sampleRate: 48000,         // 采样率
    sampleSize: 16,            // 采样位深
    channelCount: 2            // 声道数
  }
}
```

## 总结

通过 EMQX 多媒体服务的 TypeScript WebRTC 集成，你可以轻松实现：

- **音视频通话**: 高质量的实时音视频传输
- **AI 语音识别**: 实时语音转文字，支持多语言
- **智能语音合成**: 自然流畅的文字转语音
- **跨平台兼容**: 支持各种前端框架和原生 JavaScript

这为构建现代化的 AI 驱动通信应用提供了完整的技术基础。
