# TypeScript WebRTC Example

This document demonstrates how to use TypeScript to interact with the EMQX Multimedia Service via MQTT signaling for WebRTC, enabling audio/video calls, speech recognition (ASR), and text-to-speech (TTS).

The EMQX Multimedia Service uses the MQTT protocol to exchange WebRTC signaling. Clients can easily integrate real-time audio/video communication and leverage AI-powered services such as ASR and TTS.

## Core Architecture

```shell
TypeScript Client ←→ MQTT Signaling ←→ EMQX Multimedia Service
                   ↓
              WebRTC P2P Connection
```

**Key MQTT Topics**:

- `$webrtc/{device_id}` — receive SDP answers and ICE candidates from the server.
- `$webrtc/{device_id}/multimedia_proxy` — send SDP offers and ICE candidates.
- `$message/{device_id}` — receive ASR, TTS, and general messages.

## Core Implementation

### WebRTC Signaling Class

`MqttWebRTCSignaling` is the core signaling management class. It handles MQTT message exchange, WebRTC connection setup, and multimedia service integration. The class abstracts complex signaling logic into a simple API for developers.

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

    // Subscribe to topics
    await this.subscribeToTopics()

    // Set up message handler
    this.setupMessageHandler()

    // Initialize WebRTC
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
      // WebRTC signaling handling
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
      // Multimedia service message handling
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
    // Get user media
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

    // Create RTCPeerConnection
    this.pc = new RTCPeerConnection({
      iceServers: [{ urls: 'stun:stun.l.google.com:19302' }]
    })
    this.remoteStream = new MediaStream()

    // Add local stream
    this.localStream.getTracks().forEach(track => {
      this.pc!.addTrack(track, this.localStream!)
    })

    // Set event handlers
    this.setupPeerConnectionHandlers()

    // Create and send offer
    const offer = await this.pc.createOffer()
    await this.pc.setLocalDescription(offer)
    this.sendSignal('sdp_offer', offer)
  }

  private setupPeerConnectionHandlers(): void {
    if (!this.pc) return

    // ICE candidate handling
    this.pc.onicecandidate = (event) => {
      if (event.candidate) {
        this.sendSignal('ice_candidate', event.candidate)
      }
    }

    // Connection state changes
    this.pc.onconnectionstatechange = () => {
      if (this.pc) {
        this.callbacks.onConnectionStateChange?.(this.pc.connectionState)

        if (this.pc.connectionState === 'failed') {
          this.callbacks.onError?.(new Error('Connection failed'))
        }
      }
    }

    // Remote stream handling
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

  // Audio/video control
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

  // Send chat message
  sendChatMessage(message: string): void {
    this.sendSignal('chat', message)
  }

  disconnect(): void {
    // Stop media streams
    if (this.localStream) {
      this.localStream.getTracks().forEach(track => track.stop())
      this.localStream = null
    }

    if (this.remoteStream) {
      this.remoteStream.getTracks().forEach(track => track.stop())
      this.remoteStream = null
    }

    // Close peer connection
    if (this.pc) {
      this.pc.close()
      this.pc = null
    }

    // Clean up message handler
    if (this.mqttClient && this.messageHandler) {
      this.mqttClient.removeListener('message', this.messageHandler)
      this.messageHandler = null
    }

    // Unsubscribe
    const topics = [`$webrtc/${this.clientId}`, `$message/${this.clientId}`]
    this.mqttClient.unsubscribe(topics)

    this.callbacks.onConnectionStateChange?.('disconnected')
  }
}
```

## Usage Example

The following example shows basic usage of the API. The WebRTC signaling class can be used in any frontend environment, including Vue, React, or directly in vanilla JavaScript/HTML.

For a complete working demo, see the [MCP AI Companion Demo](https://github.com/emqx/mcp-ai-companion-demo) (web client).

### Basic Connection and Callback Setup

To establish a WebRTC connection:

1. First connect to MQTT.
2. Then configure callbacks for handling audio/video streams and AI service responses.

```typescript
import mqtt from 'mqtt'
import { MqttWebRTCSignaling } from './mqtt-webrtc-signaling'

// Connect to MQTT
const mqttClient = mqtt.connect('ws://broker.emqx.io:8083/mqtt', {
  clientId: 'device_123',
  username: 'your-username',
  password: 'your-password'
})

// Create WebRTC signaling
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
      console.log('Speech recognition:', text)
    },
    onTTSText: (text) => {
      console.log('AI reply:', text)
    },
    onConnectionStateChange: (state) => {
      console.log('Connection state:', state)
    }
  }
})

// Start connection
await signaling.connect()

```

### Multimedia Control

Once connected, you can easily control audio/video devices, send messages, and manage connection state:

```typescript
// Audio and video control
signaling.toggleAudio(false)  // Mute
signaling.toggleVideo(true)   // Enable camera

// Send a chat message
signaling.sendChatMessage('Hello AI!')

// Disconnect
signaling.disconnect()
```

## Key Interactions

### MQTT Signaling Workflow

WebRTC connections use MQTT topics for signaling, following the standard offer/answer model:

```
1. Client creates offer → sends to $webrtc/{device_id}/multimedia_proxy  
2. Server processes offer → returns answer on $webrtc/{device_id}  
3. Both sides exchange ICE candidates via these topics  
4. P2P connection established → audio/video streaming begins  
```

### Multimedia AI Services

The EMQX Multimedia Service provides three core AI features via the `$message/{device_id}` topic:

- **ASR (Automatic Speech Recognition)**: real-time speech-to-text, supports streaming recognition.
- **TTS (Text-to-Speech)**: converts text to natural speech, supports incremental text streaming.
- **Intelligent Dialogue**: combines ASR and TTS to enable end-to-end voice interaction.

## Error Handling and Best Practices

In real-world applications, you must handle potential errors gracefully:

```typescript
// Common error handling
callbacks: {
  onError: (error) => {
    if (error.message.includes('getUserMedia')) {
      alert('Unable to access camera/microphone. Please check permissions.')
    } else if (error.message.includes('Connection failed')) {
      // Auto-reconnect logic
      setTimeout(() => signaling?.connect(), 2000)
    }
  }
}
```

**Common solutions**:

- **Media permission issues**: Ensure camera and microphone permissions are granted.
- **Network instability**: Implement auto-reconnect to handle disruptions.
- **Signaling timeouts**: Configure connection timeouts and show user-friendly error messages.

## Advanced Configuration

### ICE Server Configuration

To ensure connectivity across different network environments, configure STUN/TURN servers:

```typescript
const iceServers = [
  { urls: 'stun:stun.l.google.com:19302' },  // Public STUN server
  {
    urls: 'turn:your-turn-server.com:3478',  // Private TURN server (recommended for production)
    username: 'turnuser',
    credential: 'turnpass'
  }
]
```

### Media Constraints Optimization

Adjust audio/video quality to balance performance and bandwidth usage:

```typescript
const mediaConstraints = {
  video: {
    width: { ideal: 1280 },    // Video width
    height: { ideal: 720 },    // Video height
    frameRate: { ideal: 30 }   // Frame rate
  },
  audio: {
    echoCancellation: true,    // Echo cancellation
    noiseSuppression: true,    // Noise suppression
    autoGainControl: true,     // Automatic gain control
    sampleRate: 48000,         // Sampling rate
    sampleSize: 16,            // Bit depth
    channelCount: 2            // Number of channels
  }
}
```

## Summary

By integrating TypeScript WebRTC with the EMQX Multimedia Service, you can easily enable:

- **Real-time Audio/Video Calls**: high-quality, low-latency communication.
- **AI Speech Recognition**: real-time multilingual speech-to-text.
- **AI Speech Synthesis**: natural text-to-speech conversion.
- **Cross-Platform Compatibility**: works with major frontend frameworks and native JavaScript.

This provides a complete foundation for building modern, AI-powered communication applications.
