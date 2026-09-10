# API Reference

This document describes the core APIs of Volcano Engine Real-Time Conversational AI, including `StartVoiceChat`, `UpdateVoiceChat`, `StopVoiceChat`, and related configuration parameters.

## API Overview

| API | Description |
|-----|-------------|
| [StartVoiceChat](#startvoicechat) | Start an AI voice session, creating an AI agent in the specified room |
| [StopVoiceChat](#stopvoicechat) | Stop a voice session and release AI agent resources |
| [UpdateVoiceChat](#updatevoicechat) | Update an ongoing voice session (interrupt, custom announcements, etc.) |

All APIs require V4 signing with AccessKey. See [Authentication Proxy Service](./installation-and-testing.md#authentication-proxy-service).

## StartVoiceChat

Starts an AI voice session and creates an AI agent in the specified room.

**Request endpoint**: `POST https://rtc.volcengineapi.com?Action=StartVoiceChat&Version=2024-12-01`

### Request Parameters

| Parameter     | Type   | Required | Description                                               |
| ------------- | ------ | -------- | --------------------------------------------------------- |
| `AppId`       | string | Yes      | RTC application ID                                        |
| `RoomId`      | string | Yes      | Room ID                                                   |
| `TaskId`      | string | Yes      | Task ID used to identify the session                      |
| `AgentConfig` | object | Yes      | Agent configuration, see [AgentConfig](#agentconfig)      |
| `Config`      | object | Yes      | Session configuration, including ASR, TTS, LLM parameters, see [Config](#config) |

### AgentConfig

Agent configuration:

| Parameter                         | Type     | Required | Description                        |
| --------------------------------- | -------- | -------- | ---------------------------------- |
| `TargetUserId`                    | string[] | Yes      | Target user ID list (client user IDs) |
| `UserId`                          | string   | Yes      | Agent user ID (AI Bot identifier)  |
| `WelcomeMessage`                  | string   | No       | Welcome message, auto-played at session start |
| `EnableConversationStateCallback` | boolean  | No       | Enable conversation state callback for listening/thinking/speaking states |
| `AnsMode`                         | number   | No       | AI noise reduction mode (0: off, 1: low, 2: medium, 3: high, recommended 3) |
| `VoicePrint`                      | object   | No       | Voiceprint recognition: `Mode` (0: off, 1: on), `IdList` (voiceprint ID list) |

### Config

Session configuration:

| Parameter       | Type   | Description                                                 |
| --------------- | ------ | ----------------------------------------------------------- |
| `ASRConfig`     | object | Speech recognition configuration, see [ASRConfig](#asrconfig) |
| `TTSConfig`     | object | Speech synthesis configuration, see [TTSConfig](#ttsconfig) |
| `LLMConfig`     | object | Large language model configuration, see [LLMConfig](#llmconfig) |
| `InterruptMode` | number | Interrupt mode (0: semantic interrupt, 1: manual interrupt) |

### Response

```json
{
  "ResponseMetadata": {
    "RequestId": "20250104123456789abcdef01234567",
    "Action": "StartVoiceChat",
    "Version": "2024-12-01",
    "Service": "rtc",
    "Region": "cn-north-1"
  },
  "Result": {}
}
```

On success, `Result` is an empty object. On failure, `ResponseMetadata.Error` contains the error information.

::: tip Note
`StartVoiceChat` is used to start an AI agent in an existing room.
:::

Official documentation: [StartVoiceChat](https://www.volcengine.com/docs/6348/1404673)

## StopVoiceChat

Stops a voice session and releases AI agent resources.

**Request endpoint**: `POST https://rtc.volcengineapi.com?Action=StopVoiceChat&Version=2024-12-01`

### Request Parameters

| Parameter | Type   | Required | Description        |
| --------- | ------ | -------- | ------------------ |
| `AppId`   | string | Yes      | RTC application ID (same as StartVoiceChat) |
| `RoomId`  | string | Yes      | Room ID (same as StartVoiceChat) |
| `TaskId`  | string | Yes      | Task ID (same as StartVoiceChat) |

### Response

```json
{
  "ResponseMetadata": {
    "RequestId": "20250104123456789abcdef01234567",
    "Action": "StopVoiceChat",
    "Version": "2024-12-01"
  },
  "Result": {}
}
```

On success, `Result` is an empty object. On failure, `ResponseMetadata.Error` contains the error information.

Official documentation: [StopVoiceChat](https://www.volcengine.com/docs/6348/1404672)

## UpdateVoiceChat

Updates an ongoing voice session. Supports interruption, function calling, and custom announcements.

**Request endpoint**: `POST https://rtc.volcengineapi.com?Action=UpdateVoiceChat&Version=2024-12-01`

### Request Parameters

| Parameter       | Type   | Required | Description                            |
| --------------- | ------ | -------- | -------------------------------------- |
| `AppId`         | string | Yes      | RTC application ID                     |
| `RoomId`        | string | Yes      | Room ID                                |
| `TaskId`        | string | Yes      | Task ID                                |
| `Command`       | string | Yes      | Command type                           |
| `Message`       | string | No       | Announcement text (max 200 characters) |
| `InterruptMode` | number | No       | Announcement priority                  |

### Command Types

| Command                | Description                    |
| ---------------------- | ------------------------------ |
| `Interrupt`            | Interrupt current agent output |
| `ExternalTextToSpeech` | Custom text-to-speech playback |
| `FunctionCallResult`   | Return function calling result |

### InterruptMode Priority

Used with `ExternalTextToSpeech` to specify announcement priority:

| Value | Description                                                  |
| ----- | ------------------------------------------------------------ |
| 1     | High priority: stop current interaction and play immediately |
| 2     | Medium priority: play after current interaction ends         |
| 3     | Low priority: drop if interaction is in progress             |

### Examples

**Interrupt the agent**:

```json
{
  "AppId": "your-app-id",
  "RoomId": "room-uuid",
  "TaskId": "task-id",
  "Command": "Interrupt"
}
```

**Custom announcement**:

```json
{
  "AppId": "your-app-id",
  "RoomId": "room-uuid",
  "TaskId": "task-id",
  "Command": "ExternalTextToSpeech",
  "Message": "You have a new message",
  "InterruptMode": 1
}
```

### Response

```json
{
  "ResponseMetadata": {
    "RequestId": "20250104123456789abcdef01234567",
    "Action": "UpdateVoiceChat",
    "Version": "2024-12-01"
  },
  "Result": {}
}
```

On success, `Result` is an empty object. On failure, `ResponseMetadata.Error` contains the error information.

Official documentation: [UpdateVoiceChat](https://www.volcengine.com/docs/6348/1404671)

## Configuration Details

The following configurations are used in the `Config` parameter of `StartVoiceChat`.

### ASRConfig

Speech recognition configuration:

| Parameter           | Type   | Required | Description                            |
| ------------------- | ------ | -------- | -------------------------------------- |
| `Provider`          | string | Yes      | Service provider, fixed as `volcano`   |
| `ProviderParams`    | object | Yes      | Provider-specific parameters           |
| `VADConfig`         | object | No       | Voice activity detection configuration |
| `VolumeGain`        | number | No       | Volume gain (0.0–1.0), default `0.5`   |
| `TurnDetectionMode` | number | No       | Turn detection mode                    |
| `InterruptConfig`   | object | No       | Interrupt configuration                |

**ProviderParams**:

| Parameter           | Type   | Description                                            |
| ------------------- | ------ | ------------------------------------------------------ |
| `AppId`             | string | ASR application ID                                     |
| `Mode`              | string | Recognition mode: `smallmodel` or `bigmodel`           |
| `Cluster`           | string | Service cluster, default `volcengine_streaming_common` |
| `context`           | string | Hotword context (JSON format)                          |
| `boosting_table_id` | string | Hotword table ID                                       |
| `correct_table_id`  | string | Correction table ID                                    |

**VADConfig** (Voice Activity Detection):

| Parameter     | Type    | Description                                    |
| ------------- | ------- | ---------------------------------------------- |
| `SilenceTime` | number  | Silence duration threshold (ms), default `600` |
| `SpeechTime`  | number  | Speech duration threshold (ms)                 |
| `PrefixTime`  | number  | Prefix duration (ms)                           |
| `SuffixTime`  | number  | Suffix duration (ms)                           |
| `Sensitivity` | number  | Sensitivity                                    |
| `AIVAD`       | boolean | Enable AI VAD                                  |

**InterruptConfig**:

| Parameter                 | Type     | Description                                   |
| ------------------------- | -------- | --------------------------------------------- |
| `InterruptSpeechDuration` | number   | Interrupt speech duration (ms), default `400` |
| `InterruptKeywords`       | string[] | Semantic interrupt keyword list               |

**Example configuration**:

```json
{
  "Provider": "volcano",
  "ProviderParams": {
    "AppId": "your-asr-app-id",
    "Mode": "smallmodel",
    "Cluster": "volcengine_streaming_common"
  },
  "VADConfig": {
    "SilenceTime": 600
  },
  "VolumeGain": 0.5,
  "TurnDetectionMode": 0,
  "InterruptConfig": {
    "InterruptSpeechDuration": 400,
    "InterruptKeywords": ["stop", "wait"]
  }
}
```

### TTSConfig

Speech synthesis configuration:

| Parameter           | Type     | Required | Description                          |
| ------------------- | -------- | -------- | ------------------------------------ |
| `Provider`          | string   | Yes      | Service provider, fixed as `volcano` |
| `ProviderParams`    | object   | Yes      | Provider-specific parameters         |
| `IgnoreBracketText` | number[] | No       | Bracket types to ignore              |

**ProviderParams**:

| Parameter    | Type   | Description        |
| ------------ | ------ | ------------------ |
| `app`        | object | Application config |
| `audio`      | object | Audio config       |
| `ResourceId` | string | TTS resource ID    |
| `Additions`  | object | Additional config  |

**app configuration**:

| Parameter | Type   | Description                            |
| --------- | ------ | -------------------------------------- |
| `appid`   | string | TTS application ID                     |
| `token`   | string | TTS application token                  |
| `cluster` | string | Service cluster, default `volcano_tts` |

**audio configuration**:

Parameters vary slightly by TTS mode:

| Parameter          | Type   | Description      | Applicable Mode |
| ------------------ | ------ | ---------------- | --------------- |
| `voice_type`       | string | Voice type       | All modes       |
| `volume_ratio`     | number | Volume (0.5–2.0) | All modes       |
| `speed_ratio`      | number | Speech rate (0.5–2.0) | standard   |
| `pitch_ratio`      | number | Pitch (0.5–2.0)  | standard        |
| `speech_ratio`     | number | Speech rate (0.5–2.0) | bigtts     |
| `pitch_rate`       | number | Pitch rate       | bigtts          |
| `speech_rate`      | number | Speech rate      | bidirection     |
| `emotion`          | string | Emotion: `happy`, `sad`, `angry`, `neutral` | Voices with emotion support |
| `emotion_strength` | number | Emotion strength (0.0–1.0) | With emotion |

::: tip TTS Modes
- `standard`: Standard mode, uses `speed_ratio`, `pitch_ratio`
- `bigtts`: Large model TTS, uses `speech_ratio`, `pitch_rate`
- `bidirection`: Bidirectional streaming, uses `speech_rate`, supports `Additions` config
:::

**Common voices**:

| Voice ID          | Description    |
| ----------------- | -------------- |
| `BV033_streaming` | Female, gentle |
| `BV001_streaming` | Male, magnetic |
| `BV700_streaming` | Female, sweet  |
| `BV406_streaming` | Male, calm     |

More voices: [Volcano Engine TTS Voice List](https://www.volcengine.com/docs/6561)

**Example configuration**:

```json
{
  "Provider": "volcano",
  "ProviderParams": {
    "app": {
      "appid": "your-tts-app-id",
      "token": "your-tts-token",
      "cluster": "volcano_tts"
    },
    "audio": {
      "voice_type": "BV033_streaming",
      "speed_ratio": 1.2,
      "pitch_ratio": 1.1,
      "volume_ratio": 1.0,
      "emotion": "happy",
      "emotion_strength": 0.8
    },
    "ResourceId": "your-resource-id"
  }
}
```

### LLMConfig

Large language model configuration:

| Parameter        | Type     | Required       | Description                                     |
| ---------------- | -------- | -------------- | ----------------------------------------------- |
| `Mode`           | string   | Yes            | Mode: `ArkV3` (Ark) or `CustomLLM` (custom)     |
| `Url`            | string   | CustomLLM only | CustomLLM callback URL                          |
| `APIKey`         | string   | No             | API authentication key                          |
| `EndPointId`     | string   | ArkV3 only     | Ark model endpoint ID                           |
| `ModelName`      | string   | No             | Model name                                      |
| `SystemMessages` | string[] | No             | System prompts                                  |
| `UserPrompts`    | object[] | No             | Preset conversation history                     |
| `Temperature`    | number   | No             | Sampling temperature (0.0–1.0), default `0.5`   |
| `TopP`           | number   | No             | Top-p sampling (0.0–1.0), default `0.9`         |
| `MaxTokens`      | number   | No             | Max tokens, default `256`                       |
| `HistoryLength`  | number   | No             | Number of history turns to keep, default `15`   |
| `EnableRoundId`  | boolean  | No             | Enable round ID                                 |
| `VisionConfig`   | object   | No             | Vision understanding: `Enable` (boolean), `SnapshotConfig` (object) |
| `Custom`         | string   | No             | Custom parameters (JSON string), passed through to CustomLLM |

**UserPrompts** (Preset conversation history):

```json
[
  { "Role": "assistant", "Content": "Hello! How can I help you?" },
  { "Role": "user", "Content": "Hello" }
]
```

**CustomLLM mode example**:

```json
{
  "Mode": "CustomLLM",
  "Url": "https://your-server.com/chat-stream",
  "APIKey": "your-api-key",
  "ModelName": "qwen-flash",
  "Temperature": 0.5,
  "TopP": 0.9,
  "MaxTokens": 256,
  "HistoryLength": 15,
  "EnableRoundId": true,
  "VisionConfig": {
    "Enable": false
  },
  "UserPrompts": [
    { "Role": "assistant", "Content": "Hi, I'm your assistant. Nice to meet you!" }
  ]
}
```

**ArkV3 mode example**:

```json
{
  "Mode": "ArkV3",
  "EndPointId": "your-endpoint-id",
  "Temperature": 0.7,
  "MaxTokens": 512
}
```

## CustomLLM Callback

When using CustomLLM mode, Volcano Engine sends user speech recognition results to your custom service.

### Callback Flow

```
User speech → Volcano Engine ASR → CustomLLM service → Volcano Engine TTS → User
```

### Request Format

Request from Volcano Engine to your CustomLLM service:

```http
POST /chat-stream HTTP/1.1
Authorization: Bearer YOUR_API_KEY
Content-Type: application/json

{
  "messages": [
    {"role": "system", "content": "You are an intelligent assistant"},
    {"role": "user", "content": "Hello"}
  ],
  "stream": true,
  "temperature": 0.7,
  "max_tokens": 256,
  "device_id": "custom-device-id"
}
```

**Request fields**:

| Field         | Description                                    |
| ------------- | ---------------------------------------------- |
| `messages`    | Conversation history in OpenAI format          |
| `stream`      | Fixed `true`, requires streaming response      |
| `temperature` | Sampling temperature                           |
| `max_tokens`  | Maximum generation length                      |
| `device_id`   | Custom parameter, passed through from `LLMConfig.Custom` |

### Response Format

Response must follow OpenAI SSE format:

```
data: {"id":"resp-1","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"role":"assistant","content":""},"finish_reason":null}],"model":"qwen-flash","created":1704355200}

data: {"id":"resp-1","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"content":"Hello"},"finish_reason":null}],"model":"qwen-flash","created":1704355200}

data: {"id":"resp-1","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"content":"!"},"finish_reason":"stop"}],"model":"qwen-flash","created":1704355200}

data: [DONE]
```

**Response requirements**:

- Must return SSE streaming response
- Content-Type: `text/event-stream`
- Each line starts with `data: `
- Last line must be `data: [DONE]`

Official documentation: [CustomLLM Integration](https://www.volcengine.com/docs/6348/1399966)

## RTC Token

Clients need a token to join an RTC room. Tokens are generated server-side using `AppKey`.

### Token Structure

```
Token = Version + AppId + Base64(Message + Signature)
```

- **Version**: fixed value `001`
- **AppId**: 24-character application identifier
- **Message**: binary-encoded payload (RoomId, UserId, expiry time, privileges)
- **Signature**: HMAC-SHA256 signature using AppKey

### Token Privileges

| Privilege             | Description          |
| --------------------- | -------------------- |
| `PrivPublishStream`   | Publish audio/video  |
| `PrivSubscribeStream` | Subscribe to streams |

### Validity

Default validity is 24 hours (86,400 seconds). Must be regenerated after expiry.

### Example

```typescript
import { AccessToken } from './rtctoken'

const token = new AccessToken(appId, appKey, roomId, userId)
const expireAt = Math.floor(Date.now() / 1000) + 24 * 3600
token.addPrivilege('PrivPublishStream', expireAt)
token.addPrivilege('PrivSubscribeStream', expireAt)
token.expireTime(expireAt)
const tokenString = token.serialize()
```

For token generation libraries, see [Installation and Testing - Generating an RTC Token](./installation-and-testing.md#generating-an-rtc-token).

## Error Codes

### Response Format

```json
{
  "ResponseMetadata": {
    "RequestId": "xxx",
    "Action": "StartVoiceChat",
    "Error": {
      "Code": "InvalidParameter",
      "Message": "Parameter AppId must not be empty"
    }
  }
}
```

### Common Error Codes

| Error Code               | HTTP Status | Description                     |
| ------------------------ | ----------- | ------------------------------- |
| `MissingParameter`       | 400         | Missing required parameter      |
| `InvalidParameter`       | 400         | Invalid parameter format        |
| `MissingRequestInfo`     | 400         | Missing request info            |
| `InvalidTimestamp`       | 400         | Invalid or expired timestamp    |
| `InvalidAuthorization`   | 400         | Invalid Authorization header    |
| `InvalidCredential`      | 400         | Invalid credential format       |
| `InvalidAccessKey`       | 401         | Invalid AccessKey               |
| `SignatureDoesNotMatch`  | 401         | Signature verification failed   |
| `InvalidSecretToken`     | 401         | Invalid or expired STS token    |
| `AccessDenied`           | 403         | Insufficient IAM permissions    |
| `ServiceNotFound`        | 404         | Service not found               |
| `InvalidActionOrVersion` | 404         | Invalid API Action or Version   |
| `FlowLimitExceeded`      | 429         | Rate limit exceeded             |
| `InternalError`          | 500         | Internal error                  |
| `InternalServiceError`   | 502         | Gateway error                   |
| `ServiceUnavailableTemp` | 503         | Service temporarily unavailable |
| `InternalServiceTimeout` | 504         | Service timeout                 |

### Business Error Codes

| Error Code     | Description                     |
| -------------- | ------------------------------- |
| `RoomNotExist` | Room does not exist             |
| `TaskNotExist` | Task does not exist             |
| `InvalidToken` | RTC token is invalid or expired |

Official documentation: [Common Error Codes](https://www.volcengine.com/docs/6369/68677)

## Related Resources

- [Installation and Testing](./installation-and-testing.md)
- [Volcano Engine Real-Time Conversational API Documentation](https://www.volcengine.com/docs/6348/1315560) - Official complete documentation
- [Volcano Engine Real-Time Audio and Video Documentation](https://www.volcengine.com/docs/6348)
