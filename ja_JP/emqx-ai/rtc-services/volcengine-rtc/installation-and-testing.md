# Installation and Testing

This document explains how to integrate Volcano Engine speech services and complete basic testing. Volcano Engine provides SDKs for multiple platforms; this guide uses the Web SDK (`@volcengine/rtc`) as an example to illustrate the integration process.

## Prerequisites

Before starting integration, make sure you have enabled the required Volcano Engine services and configured credentials. For detailed steps, see
 Quick Start – Volcano Engine Credentials.

Required credentials:

| Credential                                   | Purpose                                  |
| -------------------------------------------- | ---------------------------------------- |
| `AppId` / `AppKey`                           | RTC room connection and token generation |
| `AccessKeyId` / `SecretKey`                  | OpenAPI request signing                  |
| `ASR AppId`                                  | Speech recognition service               |
| `TTS AppId` / `TTS Token` / `TTS ResourceId` | Speech synthesis service                 |

## Authentication Proxy Service

Clients need a token to join an RTC room, and the token is generated using the `AppKey`. Starting a voice session requires calling the `StartVoiceChat` API, which must be signed with the `AccessKey`. These credentials must not be exposed to clients, so an authentication proxy service is required.

The proxy service is responsible for:

- Generating RTC tokens using `AppKey`
- Calling Volcano Engine OpenAPI using `AccessKey`
- Returning tokens and room information to the client

### Generating an RTC Token

RTC tokens are generated using `AppKey` with the HMAC-SHA256 algorithm. Volcano Engine provides reference implementations in multiple languages:

| Language      | Reference Implementation                                     |
| ------------- | ------------------------------------------------------------ |
| Node.js / Bun | [token.ts](https://github.com/emqx/mcp-ai-companion-demo/tree/volcengine/rtc/volc-server/src/lib/token.ts) |

```
import { AccessToken, Privileges } from './rtctoken'

const token = new AccessToken(appId, appKey, roomId, userId)
token.addPrivilege(Privileges.PrivPublishStream, expireTime)
const tokenString = token.serialize() // Return to the client
```

### Calling Volcano Engine OpenAPI

APIs such as `StartVoiceChat` and `StopVoiceChat` must be signed using `AccessKeyId` and `SecretKey`. The official OpenAPI SDK handles signing automatically:

```
# Node.js / Bun
npm install @volcengine/openapi

# Python
pip install volcengine-python-sdk

# Go
go get github.com/volcengine/volc-sdk-golang
// Node.js example
import { Signer } from '@volcengine/openapi'

const signer = new Signer(
  {
    accessKeyId: process.env.ACCESS_KEY_ID,
    secretKey: process.env.SECRET_KEY,
  },
  'rtc'
)

const response = await signer.fetch('https://rtc.volcengineapi.com', {
  method: 'POST',
  query: { Action: 'StartVoiceChat', Version: '2024-12-01' },
  body: { AppId: appId, RoomId: roomId, ... },
})
// response includes Token, RoomId, UserId, etc., which are returned to the client
```

For detailed signing rules, see
 [Volcano Engine V4 Signature Algorithm](https://www.volcengine.com/docs/6369/67269).

### Example API Design

The proxy service should expose APIs for client use:

```
// Start a voice session – returns token and room info
POST /api/voice/start
Request:  { sceneId: string }
Response: { roomId: string, token: string, userId: string, appId: string }

// Stop a voice session
POST /api/voice/stop
Request:  { roomId: string }
Response: { success: boolean }
```

Internally, these endpoints call the Volcano Engine OpenAPI (`StartVoiceChat`, `StopVoiceChat`) and pass the returned token and related information back to the client.

## Web Integration

Volcano Engine provides the `@volcengine/rtc` SDK for Web integration. The interaction flow between client and server is shown below:

![Call Flow](https://lf3-static.bytednsdoc.com/obj/eden-cn/UJjvKJ%5BY/ljhwZthlaukjlkulzlp/1310560_plantuml_diagram2.png)

### Install the SDK

```
npm install @volcengine/rtc
```

For AI noise reduction, the SDK includes the `@volcengine/rtc/extension-ainr` extension.

### Basic Integration Flow

#### 1. Call the Server API to Get a Token

Before using the RTC SDK, call the server API to start a voice session and retrieve the token and room information:

```
const response = await fetch('/api/voice/start', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ sceneId: 'your-scene-id' }),
})

const { appId, roomId, token, userId } = await response.json()
```

#### 2. Create the RTC Engine

```
import VERTC, { RoomProfileType, MediaType } from '@volcengine/rtc'

const engine = VERTC.createEngine(appId)
```

#### 3. Register Event Listeners

```
engine.on(VERTC.events.onError, (event) => {
  console.error('RTC error:', event.errorCode)
})

engine.on(VERTC.events.onUserPublishStream, async (event) => {
  const { userId, mediaType } = event
  await engine.subscribeStream(userId, mediaType)
})

engine.on(VERTC.events.onRoomBinaryMessageReceived, (event) => {
  const { message } = event
  // message is an ArrayBuffer in TLV format
  // contains ASR results, TTS text, agent status, etc.
})
```

#### 4. Join the Room

```
await engine.joinRoom(
  token,
  roomId,
  {
    userId,
    extraInfo: JSON.stringify({
      call_scene: 'RTC-AIGC',
      user_name: userId,
    }),
  },
  {
    isAutoPublish: false,
    isAutoSubscribeAudio: false,
    roomProfileType: RoomProfileType.chat,
  }
)
```

#### 5. Start the Microphone and Publish Audio

```
await engine.startAudioCapture()
await engine.publishStream(MediaType.AUDIO)
```

At this point, voice interaction begins. User speech is recognized by ASR, processed by the LLM, and played back via TTS.

#### 6. Leave the Room

```
await engine.unpublishStream(MediaType.AUDIO)
await engine.stopAudioCapture()
await engine.leaveRoom()
VERTC.destroyEngine(engine)

await fetch('/api/voice/stop', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ roomId }),
})
```

### AI Noise Reduction (Optional)

The RTC SDK includes built-in AI noise reduction:

```
import RTCAIAnsExtension, { AnsMode } from '@volcengine/rtc/extension-ainr'

const aiAnsExtension = new RTCAIAnsExtension()
engine.registerExtension(aiAnsExtension)

const supported = await aiAnsExtension.isSupported()
if (supported) {
  await aiAnsExtension.setAnsMode(AnsMode.MEDIUM)
  aiAnsExtension.enable()
}
```

### Receiving Remote Audio Streams

After subscribing to a remote stream, you can obtain a `MediaStream` for playback:

```
import { StreamIndex } from '@volcengine/rtc'

const audioTrack = engine.getRemoteStreamTrack(
  userId,
  StreamIndex.STREAM_INDEX_MAIN,
  'audio'
)

const stream = new MediaStream()
if (audioTrack) {
  stream.addTrack(audioTrack)
}

const audioElement = document.querySelector('audio')
audioElement.srcObject = stream
```

## SDKs for Other Platforms

Volcano Engine RTC SDK supports both software applications and hardware devices.

### Software Applications

See: [Integrate Real-Time Conversational AI (Software Applications)](https://www.volcengine.com/docs/6348/1310560)

| Platform | SDK               | Documentation                                                |
| -------- | ----------------- | ------------------------------------------------------------ |
| Web      | `@volcengine/rtc` | [Web SDK Docs](https://www.volcengine.com/docs/6348/104398)  |
| iOS      | VolcEngineRTC     | [iOS SDK Docs](https://www.volcengine.com/docs/6348/70080)   |
| Android  | VolcEngineRTC     | [Android SDK Docs](https://www.volcengine.com/docs/6348/70082) |
| Windows  | VolcEngineRTC     | [Windows SDK Docs](https://www.volcengine.com/docs/6348/70084) |
| macOS    | VolcEngineRTC     | [macOS SDK Docs](https://www.volcengine.com/docs/6348/70086) |
| Linux    | VolcEngineRTC     | [Linux SDK Docs](https://www.volcengine.com/docs/6348/113623) |
| Flutter  | volc_engine_rtc   | [Flutter SDK Docs](https://www.volcengine.com/docs/6348/113661) |
| Electron | @volcengine/rtc   | [Electron SDK Docs](https://www.volcengine.com/docs/6348/112063) |

### Hardware Devices

See: [Integrate Real-Time Conversational AI (Embedded Hardware)](https://www.volcengine.com/docs/6348/1438400)

Embedded Linux, RTOS, Android, and other hardware platforms are supported. Hardware SDKs must be obtained by contacting Volcano Engine technical support.

## Testing and Validation

### Verify RTC Connection

```
engine.on(VERTC.events.onUserJoined, (event) => {
  console.log('User joined:', event.userInfo.userId)
})
```

### Verify Speech Recognition

Speak into the microphone and receive binary messages via `onRoomBinaryMessageReceived`. Messages use TLV encoding and include:

- Subtitle messages: ASR results and LLM response text
- Status messages: agent state (listening / thinking / speaking)
- Function calls: tool invocation requests

### Verify Speech Synthesis

AI responses are played via the remote audio stream. Ensure that:

1. `onUserPublishStream` is handled
2. `subscribeStream` is called
3. The audio track is bound to an `<audio>` element

### Common Issues

#### Connection and Authentication

| Issue                            | Possible Cause                             | Solution                                                     |
| -------------------------------- | ------------------------------------------ | ------------------------------------------------------------ |
| Invalid token (`token_error`)    | Token expired or parameter mismatch        | Ensure UserId and RoomId used for token generation match those used to join the room, or regenerate the token |
| Cannot join room                 | Network issue or incorrect AppId           | Check network connectivity and confirm AppId                 |
| `Invalid 'Authorization' header` | Incorrect AK/SK configuration              | Verify AccessKeyId and SecretKey on the server               |
| Cross-service call failure       | Cross-service authorization not configured | Complete cross-service authorization in the RTC console      |

#### Agent Startup

| Issue                                   | Possible Cause                                               | Solution                                                     |
| --------------------------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| StartVoiceChat fails                    | Signature error or missing parameters                        | Verify API signature and required parameters                 |
| `The task has been started` error       | Repeated calls with fixed RoomId/UserId                      | Call StopVoiceChat first, then StartVoiceChat again          |
| Stuck at “AI preparing”                 | Permissions missing / parameter errors / insufficient balance | 1) Check console permissions 2) Verify parameter types and casing 3) Ensure services are enabled and account balance is sufficient |
| Digital avatar stuck in preparing state | Concurrency limit or configuration error                     | Verify avatar AppId/Token and ensure concurrency limits are not exceeded |

#### Devices and Media

| Issue                          | Possible Cause                               | Solution                                                     |
| ------------------------------ | -------------------------------------------- | ------------------------------------------------------------ |
| Microphone/camera cannot start | Insecure context                             | Ensure the page is accessed via `localhost` or `https`       |
| Device permission denied       | Browser not authorized                       | See [Web Device Permission Troubleshooting](https://www.volcengine.com/docs/6348/1169947) |
| No ASR result                  | Microphone not authorized or ASR not enabled | Check browser microphone permission and confirm ASR service is enabled |
| No TTS audio                   | Remote audio not subscribed                  | Ensure `subscribeStream` is called for the remote audio stream |

#### Model Configuration

| Issue                                | Solution                                                     |
| ------------------------------------ | ------------------------------------------------------------ |
| Using third-party models or Coze Bot | Configure model parameters in `LLMConfig`, set `Mode` to `CustomLLM`, and provide the callback URL |
| No response in conversation          | Verify LLM configuration and ensure the CustomLLM callback service is running |

## Related Resources

- [Volcano Engine RTC Quick Start](https://www.volcengine.com/docs/6348/1310553)
- [Volcano Engine OpenAPI Signature Specification](https://www.volcengine.com/docs/6369/67269)
- [RTC SDK Downloads](https://www.volcengine.com/docs/6348/75707)
- [Official Volcano Engine Real-Time Conversational AI Demo](https://github.com/volcengine/rtc-aigc-demo)
