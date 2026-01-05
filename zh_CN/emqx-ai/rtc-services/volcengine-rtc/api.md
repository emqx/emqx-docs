# API 参考文档

本文档介绍火山引擎实时对话式 AI 的核心 API，包括 StartVoiceChat、UpdateVoiceChat、StopVoiceChat 及相关配置参数。

## API 概览

| API | 说明 |
|-----|------|
| [StartVoiceChat](#startvoicechat) | 启动 AI 语音会话，在指定房间中创建 AI 智能体 |
| [StopVoiceChat](#stopvoicechat) | 停止语音会话，释放 AI 智能体资源 |
| [UpdateVoiceChat](#updatevoicechat) | 更新进行中的语音会话（打断、自定义播报等） |

所有 API 均需要使用 AccessKey 进行 V4 签名，参考 [认证代理服务](./installation-and-testing.md#认证代理服务)。

## StartVoiceChat

启动 AI 语音会话，在指定房间中创建 AI 智能体。

**请求地址**：`POST https://rtc.volcengineapi.com?Action=StartVoiceChat&Version=2024-12-01`

### 请求参数

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `AppId` | string | 是 | RTC 应用 ID |
| `RoomId` | string | 是 | 房间 ID |
| `TaskId` | string | 是 | 任务 ID，用于标识会话 |
| `AgentConfig` | object | 是 | 智能体配置，见下方 |
| `Config` | object | 是 | 会话配置，包含 ASR、TTS、LLM 等参数，见下方 |

### AgentConfig

智能体配置：

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `TargetUserId` | string[] | 是 | 目标用户 ID 列表，即客户端用户 ID |
| `UserId` | string | 是 | 智能体用户 ID（AI Bot 的身份标识） |
| `WelcomeMessage` | string | 否 | 欢迎语，会话开始时自动播报 |
| `EnableConversationStateCallback` | boolean | 否 | 启用会话状态回调，用于接收 listening/thinking/speaking 状态 |
| `AnsMode` | number | 否 | AI 降噪模式（0: 关闭, 1: 低, 2: 中, 3: 高，推荐 3） |
| `VoicePrint` | object | 否 | 声纹识别配置：`Mode`（0: 关闭, 1: 开启）、`IdList`（声纹 ID 列表） |

### Config

会话配置：

| 参数 | 类型 | 说明 |
|------|------|------|
| `ASRConfig` | object | 语音识别配置，详见 [ASRConfig](#asrconfig) |
| `TTSConfig` | object | 语音合成配置，详见 [TTSConfig](#ttsconfig) |
| `LLMConfig` | object | 大模型配置，详见 [LLMConfig](#llmconfig) |
| `InterruptMode` | number | 打断模式（0: 语义打断, 1: 手动打断） |

### 响应

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

成功时 `Result` 为空对象，失败时 `ResponseMetadata.Error` 包含错误信息。

::: tip 注意
`StartVoiceChat` 用于在已有房间中启动 AI 智能体，**不返回** RTC Token。Token 需要服务端使用 `AppKey` 自行生成，参考 [生成 RTC Token](./installation-and-testing.md#生成-rtc-token)。
:::

官方文档：[StartVoiceChat](https://www.volcengine.com/docs/6348/1404673)

## StopVoiceChat

停止语音会话，释放 AI 智能体资源。

**请求地址**：`POST https://rtc.volcengineapi.com?Action=StopVoiceChat&Version=2024-12-01`

### 请求参数

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `AppId` | string | 是 | RTC 应用 ID，与 StartVoiceChat 一致 |
| `RoomId` | string | 是 | 房间 ID，与 StartVoiceChat 一致 |
| `TaskId` | string | 是 | 任务 ID，与 StartVoiceChat 一致 |

### 响应

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

成功时 `Result` 为空对象，失败时 `ResponseMetadata.Error` 包含错误信息。

官方文档：[StopVoiceChat](https://www.volcengine.com/docs/6348/1404672)

## UpdateVoiceChat

更新进行中的语音会话，支持打断、Function calling、自定义播报等操作。

**请求地址**：`POST https://rtc.volcengineapi.com?Action=UpdateVoiceChat&Version=2024-12-01`

### 请求参数

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `AppId` | string | 是 | RTC 应用 ID |
| `RoomId` | string | 是 | 房间 ID |
| `TaskId` | string | 是 | 任务 ID |
| `Command` | string | 是 | 操作命令 |
| `Message` | string | 否 | 播报内容（最长 200 字符） |
| `InterruptMode` | number | 否 | 播报优先级 |

### Command 命令类型

| Command | 说明 |
|---------|------|
| `Interrupt` | 打断当前智能体输出 |
| `ExternalTextToSpeech` | 自定义文本转语音播报 |
| `FunctionCallResult` | 返回 Function calling 结果 |

### InterruptMode 优先级

用于 `ExternalTextToSpeech` 命令时指定播报优先级：

| 值 | 说明 |
|----|------|
| 1 | 高优先级：终止当前交互，立即播放 |
| 2 | 中优先级：等待当前交互结束后播放 |
| 3 | 低优先级：如果正在交互则丢弃 |

### 使用示例

**打断智能体**：

```json
{
  "AppId": "your-app-id",
  "RoomId": "room-uuid",
  "TaskId": "task-id",
  "Command": "Interrupt"
}
```

**自定义播报**：

```json
{
  "AppId": "your-app-id",
  "RoomId": "room-uuid",
  "TaskId": "task-id",
  "Command": "ExternalTextToSpeech",
  "Message": "您有一条新消息",
  "InterruptMode": 1
}
```

### 响应

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

成功时 `Result` 为空对象，失败时 `ResponseMetadata.Error` 包含错误信息。

官方文档：[UpdateVoiceChat](https://www.volcengine.com/docs/6348/1404671)

## 配置参数详解

以下配置用于 `StartVoiceChat` 的 `Config` 参数。

### ASRConfig

语音识别配置：

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `Provider` | string | 是 | 服务提供商，固定 `volcano` |
| `ProviderParams` | object | 是 | 提供商参数 |
| `VADConfig` | object | 否 | 语音活动检测配置 |
| `VolumeGain` | number | 否 | 音量增益（0.0-1.0），默认 `0.5` |
| `TurnDetectionMode` | number | 否 | 轮次检测模式 |
| `InterruptConfig` | object | 否 | 打断配置 |

**ProviderParams**：

| 参数 | 类型 | 说明 |
|------|------|------|
| `AppId` | string | ASR 应用 ID |
| `Mode` | string | 识别模式：`smallmodel`（小模型）、`bigmodel`（大模型） |
| `Cluster` | string | 服务集群，默认 `volcengine_streaming_common` |
| `context` | string | 热词上下文（JSON 格式） |
| `boosting_table_id` | string | 热词表 ID |
| `correct_table_id` | string | 纠错表 ID |

**VADConfig**（语音活动检测）：

| 参数 | 类型 | 说明 |
|------|------|------|
| `SilenceTime` | number | 静音判定时长（毫秒），默认 `600` |
| `SpeechTime` | number | 语音判定时长（毫秒） |
| `PrefixTime` | number | 前缀时长（毫秒） |
| `SuffixTime` | number | 后缀时长（毫秒） |
| `Sensitivity` | number | 灵敏度 |
| `AIVAD` | boolean | 是否启用 AI VAD |

**InterruptConfig**（打断配置）：

| 参数 | 类型 | 说明 |
|------|------|------|
| `InterruptSpeechDuration` | number | 打断语音时长（毫秒），默认 `400` |
| `InterruptKeywords` | string[] | 语义打断关键词列表 |

**示例配置**：

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
    "InterruptKeywords": ["停", "暂停", "等一下", "stop", "wait"]
  }
}
```

### TTSConfig

语音合成配置：

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `Provider` | string | 是 | 服务提供商，固定 `volcano` |
| `ProviderParams` | object | 是 | 提供商参数 |
| `IgnoreBracketText` | number[] | 否 | 忽略的括号类型 |

**ProviderParams**：

| 参数 | 类型 | 说明 |
|------|------|------|
| `app` | object | 应用配置 |
| `audio` | object | 音频配置 |
| `ResourceId` | string | TTS 资源 ID |
| `Additions` | object | 附加配置 |

**app 配置**：

| 参数 | 类型 | 说明 |
|------|------|------|
| `appid` | string | TTS 应用 ID |
| `token` | string | TTS 应用 Token |
| `cluster` | string | 服务集群，默认 `volcano_tts` |

**audio 配置**：

不同 TTS 模式下的参数略有不同：

| 参数 | 类型 | 说明 | 适用模式 |
|------|------|------|----------|
| `voice_type` | string | 音色类型 | 所有模式 |
| `volume_ratio` | number | 音量比例（0.5-2.0） | 所有模式 |
| `speed_ratio` | number | 语速比例（0.5-2.0） | standard |
| `pitch_ratio` | number | 音调比例（0.5-2.0） | standard |
| `speech_ratio` | number | 语速比例（0.5-2.0） | bigtts |
| `pitch_rate` | number | 音调变化率 | bigtts |
| `speech_rate` | number | 语速变化率 | bidirection |
| `emotion` | string | 情感类型：`happy`、`sad`、`angry`、`neutral` | 支持情感的音色 |
| `emotion_strength` | number | 情感强度（0.0-1.0） | 配合 emotion 使用 |

::: tip TTS 模式说明
- `standard`：标准模式，使用 `speed_ratio`、`pitch_ratio`
- `bigtts`：大模型 TTS，使用 `speech_ratio`、`pitch_rate`
- `bidirection`：双向流式，使用 `speech_rate`，支持 `Additions` 配置
:::

**常用音色**：

| 音色 ID | 说明 |
|---------|------|
| `BV033_streaming` | 女声，温柔 |
| `BV001_streaming` | 男声，磁性 |
| `BV700_streaming` | 女声，甜美 |
| `BV406_streaming` | 男声，沉稳 |

更多音色参考：[火山引擎 TTS 音色列表](https://www.volcengine.com/docs/6561)

**示例配置**：

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

大模型配置：

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `Mode` | string | 是 | 模式：`ArkV3`（方舟）或 `CustomLLM`（自定义） |
| `Url` | string | CustomLLM 必填 | CustomLLM 回调地址 |
| `APIKey` | string | 否 | API 认证密钥 |
| `EndPointId` | string | ArkV3 必填 | 方舟模型端点 ID |
| `ModelName` | string | 否 | 模型名称 |
| `SystemMessages` | string[] | 否 | 系统提示词列表 |
| `UserPrompts` | object[] | 否 | 预设对话历史 |
| `Temperature` | number | 否 | 采样温度（0.0-1.0），默认 `0.5` |
| `TopP` | number | 否 | 核采样概率（0.0-1.0），默认 `0.9` |
| `MaxTokens` | number | 否 | 最大生成 token 数，默认 `256` |
| `HistoryLength` | number | 否 | 保留的历史轮数，默认 `15` |
| `EnableRoundId` | boolean | 否 | 启用轮次 ID |
| `VisionConfig` | object | 否 | 视觉理解配置：`Enable`（是否启用）、`SnapshotConfig`（截图配置） |
| `Custom` | string | 否 | 自定义参数（JSON 字符串），透传给 CustomLLM |

**UserPrompts**（预设对话历史）：

```json
[
  { "Role": "assistant", "Content": "你好！有什么可以帮助你的？" },
  { "Role": "user", "Content": "你好" }
]
```

**CustomLLM 模式示例**：

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
    { "Role": "assistant", "Content": "嗨～我是助手，很高兴见到你！" }
  ]
}
```

**ArkV3 模式示例**：

```json
{
  "Mode": "ArkV3",
  "EndPointId": "your-endpoint-id",
  "Temperature": 0.7,
  "MaxTokens": 512
}
```

## CustomLLM 回调

使用 CustomLLM 模式时，火山引擎会将用户语音识别结果回调到自定义服务。

### 回调流程

```
用户语音 → 火山引擎 ASR → CustomLLM 服务 → 火山引擎 TTS → 用户
```

### 请求格式

火山引擎发送到 CustomLLM 服务的请求：

```http
POST /chat-stream HTTP/1.1
Authorization: Bearer YOUR_API_KEY
Content-Type: application/json

{
  "messages": [
    {"role": "system", "content": "你是一个智能助手"},
    {"role": "user", "content": "你好"}
  ],
  "stream": true,
  "temperature": 0.7,
  "max_tokens": 256,
  "device_id": "custom-device-id"
}
```

**请求字段**：

| 字段 | 说明 |
|------|------|
| `messages` | 对话历史，OpenAI 格式 |
| `stream` | 固定 `true`，流式响应 |
| `temperature` | 采样温度 |
| `max_tokens` | 最大生成长度 |
| `device_id` | 自定义参数，从 `LLMConfig.Custom` 透传 |

### 响应格式

响应需遵循 OpenAI SSE 格式：

```
data: {"id":"resp-1","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"role":"assistant","content":""},"finish_reason":null}],"model":"qwen-flash","created":1704355200}

data: {"id":"resp-1","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"content":"你好"},"finish_reason":null}],"model":"qwen-flash","created":1704355200}

data: {"id":"resp-1","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"content":"！"},"finish_reason":"stop"}],"model":"qwen-flash","created":1704355200}

data: [DONE]
```

**响应要点**：

- 必须返回 SSE 流式响应
- Content-Type: `text/event-stream`
- 每行以 `data: ` 开头
- 最后一行为 `data: [DONE]`

官方文档：[CustomLLM 接入](https://www.volcengine.com/docs/6348/1399966)

## RTC Token

客户端加入 RTC 房间需要 Token。Token 由服务端使用 AppKey 生成。

### Token 结构

```
Token = Version + AppId + Base64(Message + Signature)
```

- **Version**：固定值 `001`
- **AppId**：24 位应用标识符
- **Message**：二进制编码的负载（RoomId、UserId、过期时间、权限等）
- **Signature**：使用 AppKey 进行 HMAC-SHA256 签名

### Token 权限

| 权限 | 说明 |
|------|------|
| `PrivPublishStream` | 发布音视频流 |
| `PrivSubscribeStream` | 订阅音视频流 |

### 有效期

默认 24 小时（86400 秒）。过期后需重新生成。

### 生成示例

```typescript
import { AccessToken } from './rtctoken'

const token = new AccessToken(appId, appKey, roomId, userId)
const expireAt = Math.floor(Date.now() / 1000) + 24 * 3600
token.addPrivilege('PrivPublishStream', expireAt)
token.addPrivilege('PrivSubscribeStream', expireAt)
token.expireTime(expireAt)
const tokenString = token.serialize()
```

Token 生成库参考 [安装与测试 - 生成 RTC Token](./installation-and-testing.md#生成-rtc-token)。

## 错误码

### 响应格式

```json
{
  "ResponseMetadata": {
    "RequestId": "xxx",
    "Action": "StartVoiceChat",
    "Error": {
      "Code": "InvalidParameter",
      "Message": "参数 AppId 不能为空"
    }
  }
}
```

### 公共错误码

| 错误码 | HTTP 状态码 | 说明 |
|--------|------------|------|
| `MissingParameter` | 400 | 缺少必要参数（如 Action、Version） |
| `InvalidParameter` | 400 | 参数格式错误或无效 |
| `MissingRequestInfo` | 400 | 缺少请求信息（如 X-Date 头） |
| `InvalidTimestamp` | 400 | 请求过期或时间戳无效，检查 UTC 时间格式 |
| `InvalidAuthorization` | 400 | Authorization 头格式错误 |
| `InvalidCredential` | 400 | Credential 格式错误 |
| `InvalidAccessKey` | 401 | AccessKey 无效或格式错误 |
| `SignatureDoesNotMatch` | 401 | 签名验证失败，检查 SecretKey |
| `InvalidSecretToken` | 401 | STS 临时凭证过期或无效 |
| `AccessDenied` | 403 | IAM 权限不足 |
| `ServiceNotFound` | 404 | 服务不存在 |
| `InvalidActionOrVersion` | 404 | API Action 或 Version 不存在 |
| `FlowLimitExceeded` | 429 | 请求频率超限，降低 QPS |
| `InternalError` | 500 | 内部错误 |
| `InternalServiceError` | 502 | 服务网关错误 |
| `ServiceUnavailableTemp` | 503 | 服务暂时不可用 |
| `InternalServiceTimeout` | 504 | 服务超时 |

### 业务错误码

| 错误码 | 说明 |
|--------|------|
| `RoomNotExist` | 房间不存在 |
| `TaskNotExist` | 任务不存在 |
| `InvalidToken` | RTC Token 无效或过期 |

官方文档：[公共错误码](https://www.volcengine.com/docs/6369/68677)

## 相关资源

- [安装与测试](./installation-and-testing.md)
- [火山引擎实时对话式 API 文档](https://www.volcengine.com/docs/6348/1315560) - 官方完整文档
- [火山引擎实时音视频文档](https://www.volcengine.com/docs/6348)
