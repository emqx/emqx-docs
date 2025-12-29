# 安装与测试

本文档介绍如何集成火山云语音服务并完成基础测试。火山引擎提供多端 SDK，本文以 Web 端（`@volcengine/rtc`）为例说明集成流程。

## 前置准备

开始集成前，请确保已完成火山引擎服务开通和凭证配置。详细步骤参考[快速开始 - 火山引擎凭证](./quick-start.md#4-火山引擎凭证)。

需要准备的凭证：

| 凭证 | 用途 |
|------|------|
| `AppId` / `AppKey` | RTC 房间连接和 Token 生成 |
| `AccessKeyId` / `SecretKey` | OpenAPI 请求签名 |
| `ASR AppId` | 语音识别服务 |
| `TTS AppId` / `TTS Token` / `TTS ResourceId` | 语音合成服务 |

## 认证代理服务

客户端加入 RTC 房间需要 Token，而 Token 由 `AppKey` 生成；启动语音会话需要调用 `StartVoiceChat` API，而该 API 需要 `AccessKey` 签名。这些密钥都不能暴露给客户端，因此需要搭建一个代理服务来处理认证。

代理服务负责：
- 使用 `AppKey` 生成 RTC Token
- 使用 `AccessKey` 调用火山引擎 OpenAPI
- 将 Token 和房间信息返回给客户端

### 生成 RTC Token

Token 使用 `AppKey` 通过 HMAC-SHA256 算法生成。火山引擎提供各语言的生成库：

| 语言 | 参考实现 |
|------|----------|
| Go | [AccessToken.go](https://github.com/volcengine/rtc-aigc-demo/blob/main/server/rtc-aigc-demo/internal/pkg/rtctoken/token.go) |
| Python | [access_token.py](https://github.com/volcengine/rtc-aigc-demo/blob/main/server/rtc-aigc-demo-python/src/rtctoken/access_token.py) |
| Node.js / Bun | [token.ts](https://github.com/emqx/mcp-ai-companion-demo/tree/volcengine/rtc/volc-server/src/lib/token.ts) |

```typescript
import { AccessToken, Privileges } from './rtctoken'

const token = new AccessToken(appId, appKey, roomId, userId)
token.addPrivilege(Privileges.PrivPublishStream, expireTime)
const tokenString = token.serialize()  // 返回给客户端
```

### 调用火山引擎 OpenAPI

`StartVoiceChat`、`StopVoiceChat` 等 API 需要使用 `AccessKeyId` 和 `SecretKey` 签名。使用官方 OpenAPI SDK 可以自动处理签名：

```bash
# Node.js / Bun
npm install @volcengine/openapi

# Python
pip install volcengine-python-sdk

# Go
go get github.com/volcengine/volc-sdk-golang
```

```typescript
// Node.js 示例
import { Signer } from '@volcengine/openapi'

const signer = new Signer({
  accessKeyId: process.env.ACCESS_KEY_ID,
  secretKey: process.env.SECRET_KEY,
}, 'rtc')

const response = await signer.fetch('https://rtc.volcengineapi.com', {
  method: 'POST',
  query: { Action: 'StartVoiceChat', Version: '2024-12-01' },
  body: { AppId: appId, RoomId: roomId, ... },
})
// response 包含 Token、RoomId、UserId 等信息，返回给客户端
```

详细签名规范参考：[火山引擎 V4 签名算法](https://www.volcengine.com/docs/6369/67269)

### API 设计示例

代理服务需要暴露接口供客户端调用：

```typescript
// 启动语音会话 - 返回 Token 和房间信息
POST /api/voice/start
Request:  { sceneId: string }
Response: { roomId: string, token: string, userId: string, appId: string }

// 停止语音会话
POST /api/voice/stop
Request:  { roomId: string }
Response: { success: boolean }
```

服务端实现中，这些接口内部调用火山引擎 OpenAPI（`StartVoiceChat`、`StopVoiceChat`），并将返回的 Token 等信息透传给客户端。

## Web 端集成

火山引擎提供 `@volcengine/rtc` SDK 用于 Web 端集成。客户端与服务端的交互流程如下：

![调用流程](https://lf3-static.bytednsdoc.com/obj/eden-cn/UJjvKJ%5BY/ljhwZthlaukjlkulzlp/1310560_plantuml_diagram2.png)

### 安装 SDK

```bash
npm install @volcengine/rtc
```

如需 AI 降噪功能，SDK 已内置 `@volcengine/rtc/extension-ainr` 扩展。

### 基础集成流程

#### 1. 调用服务端 API 获取 Token

使用 RTC SDK 前，先调用服务端接口启动语音会话，获取 Token 和房间信息：

```typescript
// 调用服务端 API 获取认证信息
const response = await fetch('/api/voice/start', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ sceneId: 'your-scene-id' }),
})

const { appId, roomId, token, userId } = await response.json()
```

#### 2. 创建 RTC 引擎

```typescript
import VERTC, { RoomProfileType, MediaType } from '@volcengine/rtc'

// 使用服务端返回的 appId 创建引擎实例
const engine = VERTC.createEngine(appId)
```

#### 3. 注册事件监听

```typescript
// 监听错误
engine.on(VERTC.events.onError, (event) => {
  console.error('RTC error:', event.errorCode)
})

// 监听远端用户发布流（AI 语音回复）
engine.on(VERTC.events.onUserPublishStream, async (event) => {
  const { userId, mediaType } = event
  // 订阅远端音频流
  await engine.subscribeStream(userId, mediaType)
})

// 监听二进制消息（字幕、状态等）
engine.on(VERTC.events.onRoomBinaryMessageReceived, (event) => {
  const { message } = event
  // message 为 ArrayBuffer，需解析 TLV 格式
  // 包含 ASR 识别结果、TTS 文本、智能体状态等
})
```

#### 4. 加入房间

```typescript
// 使用步骤 1 获取的 token、roomId、userId 加入房间
await engine.joinRoom(
  token,
  roomId,
  {
    userId: userId,
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

#### 5. 开启麦克风并发布音频

```typescript
// 开启麦克风采集
await engine.startAudioCapture()

// 发布音频流到房间
await engine.publishStream(MediaType.AUDIO)
```

此时语音交互已经开始，用户说话会被 ASR 识别，LLM 处理后通过 TTS 播放回复。

#### 6. 离开房间

```typescript
// 停止发布
await engine.unpublishStream(MediaType.AUDIO)

// 停止采集
await engine.stopAudioCapture()

// 离开房间
await engine.leaveRoom()

// 销毁引擎
VERTC.destroyEngine(engine)

// 调用服务端 API 停止语音会话
await fetch('/api/voice/stop', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ roomId }),
})
```

### AI 降噪（可选）

火山引擎 RTC SDK 内置 AI 降噪扩展，可有效过滤环境噪音：

```typescript
import RTCAIAnsExtension, { AnsMode } from '@volcengine/rtc/extension-ainr'

// 创建并注册扩展
const aiAnsExtension = new RTCAIAnsExtension()
engine.registerExtension(aiAnsExtension)

// 检查是否支持
const supported = await aiAnsExtension.isSupported()
if (supported) {
  // 设置降噪模式：LOW / MEDIUM / HIGH
  await aiAnsExtension.setAnsMode(AnsMode.MEDIUM)
  // 启用降噪
  aiAnsExtension.enable()
}
```

### 获取远端音频流

订阅远端流后，可获取 MediaStream 用于播放：

```typescript
import { StreamIndex } from '@volcengine/rtc'

// 获取远端用户的音频轨道
const audioTrack = engine.getRemoteStreamTrack(userId, StreamIndex.STREAM_INDEX_MAIN, 'audio')

// 创建 MediaStream 并播放
const stream = new MediaStream()
if (audioTrack) {
  stream.addTrack(audioTrack)
}

// 绑定到 audio 元素播放
const audioElement = document.querySelector('audio')
audioElement.srcObject = stream
```

## 其他端 SDK

火山引擎 RTC SDK 支持软件应用和硬件设备两类场景。

### 软件应用

详见：[集成实时对话式 AI（软件应用）](https://www.volcengine.com/docs/6348/1310560)

| 平台 | SDK | 文档 |
|------|-----|------|
| Web | `@volcengine/rtc` | [Web SDK 文档](https://www.volcengine.com/docs/6348/104398) |
| iOS | VolcEngineRTC | [iOS SDK 文档](https://www.volcengine.com/docs/6348/70080) |
| Android | VolcEngineRTC | [Android SDK 文档](https://www.volcengine.com/docs/6348/70082) |
| Windows | VolcEngineRTC | [Windows SDK 文档](https://www.volcengine.com/docs/6348/70084) |
| macOS | VolcEngineRTC | [macOS SDK 文档](https://www.volcengine.com/docs/6348/70086) |
| Linux | VolcEngineRTC | [Linux SDK 文档](https://www.volcengine.com/docs/6348/113623) |
| Flutter | volc_engine_rtc | [Flutter SDK 文档](https://www.volcengine.com/docs/6348/113661) |
| Electron | @volcengine/rtc | [Electron SDK 文档](https://www.volcengine.com/docs/6348/112063) |

### 硬件设备

详见：[集成实时对话式 AI（嵌入式硬件）](https://www.volcengine.com/docs/6348/1438400)

支持嵌入式 Linux、RTOS、Android 等硬件平台。硬件端 SDK 需联系火山引擎技术支持获取。

## 测试验证

### 验证 RTC 连接

成功加入房间后，可通过事件确认：

```typescript
engine.on(VERTC.events.onUserJoined, (event) => {
  console.log('User joined:', event.userInfo.userId)
})
```

### 验证语音识别

对着麦克风说话，通过 `onRoomBinaryMessageReceived` 事件接收二进制消息。消息采用 TLV 格式编码，包含：

- **字幕消息**：ASR 识别结果和 LLM 回复文本
- **状态消息**：智能体状态（listening / thinking / speaking）
- **Function Call**：工具调用请求

### 验证语音合成

AI 回复会通过远端音频流播放。确保：

1. 已监听 `onUserPublishStream` 事件
2. 已调用 `subscribeStream` 订阅远端音频
3. 已将音频轨道绑定到 `<audio>` 元素

### 常见问题

#### 连接与认证

| 问题 | 可能原因 | 解决方案 |
|------|----------|----------|
| Token 无效 (`token_error`) | Token 过期或参数不匹配 | 检查生成 Token 时的 UserId、RoomId 是否与使用时一致，或重新生成 Token |
| 无法加入房间 | 网络问题或 AppId 错误 | 检查网络连接，确认 AppId 正确 |
| `Invalid 'Authorization' header` | AK/SK 配置错误 | 检查服务端的 AccessKeyId 和 SecretKey 是否正确 |
| 跨服务调用失败 | 未配置跨服务授权 | 在 RTC 控制台完成跨服务授权配置 |

#### 智能体启动

| 问题 | 可能原因 | 解决方案 |
|------|----------|----------|
| StartVoiceChat 失败 | 签名错误或参数缺失 | 检查服务端 API 签名，确认必需参数已填写 |
| `The task has been started` 报错 | RoomId/UserId 固定时重复调用 | 先调用 StopVoiceChat，再重新调用 StartVoiceChat |
| 一直停留在 "AI 准备中" | 权限未授予 / 参数错误 / 服务欠费 | 1. 检查控制台权限配置<br>2. 检查参数大小写、类型<br>3. 确认服务已开通且余额充足 |
| 数字人一直停留在准备中 | 并发限制或配置错误 | 1. 检查数字人 AppId/Token 是否正确<br>2. 确认未超过并发限制 |

#### 设备与媒体

| 问题 | 可能原因 | 解决方案 |
|------|----------|----------|
| 麦克风/摄像头开启失败 | 非安全上下文 | 确保页面使用 `localhost` 或 `https` 协议访问 |
| 设备权限获取失败 | 浏览器未授权 | 参考 [Web 排查设备权限问题](https://www.volcengine.com/docs/6348/1169947) |
| ASR 无识别结果 | 麦克风未授权或服务未开通 | 检查浏览器麦克风权限，确认 ASR 服务已开通 |
| TTS 无声音 | 未订阅远端音频 | 确保调用了 `subscribeStream` 订阅远端音频流 |

#### 模型配置

| 问题 | 解决方案 |
|------|----------|
| 如何使用第三方模型 / Coze Bot | 在 LLMConfig 中配置对应的模型参数，Mode 设为 `CustomLLM` 并填写回调地址 |
| 对话无反馈 | 检查 LLM 配置是否正确，CustomLLM 模式需确保回调服务正常运行 |

## 相关资源

- [火山引擎实时音视频快速入门](https://www.volcengine.com/docs/6348/1310553)
- [火山引擎 OpenAPI 签名规范](https://www.volcengine.com/docs/6369/67269)
- [RTC SDK 下载](https://www.volcengine.com/docs/6348/75707)
- [火山引擎官方实时对话式 AI Demo](https://github.com/volcengine/rtc-aigc-demo)
