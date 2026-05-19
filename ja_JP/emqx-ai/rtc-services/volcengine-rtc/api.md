# APIリファレンス

本ドキュメントでは、Volcano Engine リアルタイム会話型AIのコアAPIである `StartVoiceChat`、`UpdateVoiceChat`、`StopVoiceChat` および関連する設定パラメータについて説明します。

## API概要

| API | 説明 |
|-----|------|
| [StartVoiceChat](#startvoicechat) | 指定したルームでAI音声セッションを開始し、AIエージェントを作成します |
| [StopVoiceChat](#stopvoicechat) | 音声セッションを停止し、AIエージェントのリソースを解放します |
| [UpdateVoiceChat](#updatevoicechat) | 進行中の音声セッションを更新します（割り込み、カスタムアナウンスなど） |

すべてのAPIはAccessKeyによるV4署名が必要です。詳細は[認証プロキシサービス](./installation-and-testing.md#authentication-proxy-service)をご参照ください。

## StartVoiceChat

AI音声セッションを開始し、指定したルームにAIエージェントを作成します。

**リクエストエンドポイント**：`POST https://rtc.volcengineapi.com?Action=StartVoiceChat&Version=2024-12-01`

### リクエストパラメータ

| パラメータ     | 型       | 必須    | 説明                                               |
| ------------- | -------- | ------- | -------------------------------------------------- |
| `AppId`       | string   | 必須    | RTCアプリケーションID                              |
| `RoomId`      | string   | 必須    | ルームID                                           |
| `TaskId`      | string   | 必須    | セッション識別用のタスクID                         |
| `AgentConfig` | object   | 必須    | エージェント設定、詳細は[AgentConfig](#agentconfig)参照 |
| `Config`      | object   | 必須    | セッション設定（ASR、TTS、LLMパラメータ含む）、詳細は[Config](#config)参照 |

### AgentConfig

エージェント設定：

| パラメータ                         | 型         | 必須    | 説明                                      |
| --------------------------------- | ---------- | ------- | ----------------------------------------- |
| `TargetUserId`                    | string[]   | 必須    | 対象ユーザーIDリスト（クライアントユーザーID） |
| `UserId`                          | string     | 必須    | エージェントユーザーID（AI Botの識別子）   |
| `WelcomeMessage`                  | string     | 任意    | セッション開始時に自動再生されるウェルカムメッセージ |
| `EnableConversationStateCallback` | boolean    | 任意    | 聞き取り／思考／発話状態の会話状態コールバックを有効化 |
| `AnsMode`                         | number     | 任意    | AIノイズリダクションモード（0: オフ、1: 低、2: 中、3: 高、推奨は3） |
| `VoicePrint`                      | object     | 任意    | ボイスプリント認識：`Mode`（0: オフ、1: オン）、`IdList`（ボイスプリントIDリスト） |

### Config

セッション設定：

| パラメータ       | 型       | 説明                                               |
| --------------- | -------- | -------------------------------------------------- |
| `ASRConfig`     | object   | 音声認識設定、詳細は[ASRConfig](#asrconfig)参照  |
| `TTSConfig`     | object   | 音声合成設定、詳細は[TTSConfig](#ttsconfig)参照  |
| `LLMConfig`     | object   | 大規模言語モデル設定、詳細は[LLMConfig](#llmconfig)参照 |
| `InterruptMode` | number   | 割り込みモード（0: セマンティック割り込み、1: 手動割り込み） |

### レスポンス

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

成功時は `Result` が空オブジェクトです。失敗時は `ResponseMetadata.Error` にエラー情報が含まれます。

::: tip 注意
`StartVoiceChat` は既存のルーム内でAIエージェントを開始するために使用します。
:::

公式ドキュメント: [StartVoiceChat](https://www.volcengine.com/docs/6348/1404673)

## StopVoiceChat

音声セッションを停止し、AIエージェントのリソースを解放します。

**リクエストエンドポイント**：`POST https://rtc.volcengineapi.com?Action=StopVoiceChat&Version=2024-12-01`

### リクエストパラメータ

| パラメータ | 型       | 必須    | 説明                                   |
| --------- | -------- | ------- | -------------------------------------- |
| `AppId`   | string   | 必須    | RTCアプリケーションID（StartVoiceChatと同じ） |
| `RoomId`  | string   | 必須    | ルームID（StartVoiceChatと同じ）       |
| `TaskId`  | string   | 必須    | タスクID（StartVoiceChatと同じ）       |

### レスポンス

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

成功時は `Result` が空オブジェクトです。失敗時は `ResponseMetadata.Error` にエラー情報が含まれます。

公式ドキュメント: [StopVoiceChat](https://www.volcengine.com/docs/6348/1404672)

## UpdateVoiceChat

進行中の音声セッションを更新します。割り込み、関数呼び出し、カスタムアナウンスに対応しています。

**リクエストエンドポイント**：`POST https://rtc.volcengineapi.com?Action=UpdateVoiceChat&Version=2024-12-01`

### リクエストパラメータ

| パラメータ       | 型       | 必須    | 説明                                   |
| --------------- | -------- | ------- | -------------------------------------- |
| `AppId`         | string   | 必須    | RTCアプリケーションID                  |
| `RoomId`        | string   | 必須    | ルームID                              |
| `TaskId`        | string   | 必須    | タスクID                              |
| `Command`       | string   | 必須    | コマンドタイプ                        |
| `Message`       | string   | 任意    | アナウンステキスト（最大200文字）     |
| `InterruptMode` | number   | 任意    | アナウンスの優先度                    |

### コマンドタイプ

| コマンド               | 説明                            |
| ---------------------- | ------------------------------- |
| `Interrupt`            | 現在のエージェント出力を割り込み |
| `ExternalTextToSpeech` | カスタムテキスト読み上げ再生     |
| `FunctionCallResult`   | 関数呼び出し結果を返す           |

### InterruptModeの優先度

`ExternalTextToSpeech` と併用し、アナウンスの優先度を指定します：

| 値   | 説明                                              |
| ---- | ------------------------------------------------- |
| 1    | 高優先度：現在の対話を停止して即時再生             |
| 2    | 中優先度：現在の対話終了後に再生                    |
| 3    | 低優先度：対話中の場合は破棄                         |

### 例

**エージェントを割り込み停止する例**：

```json
{
  "AppId": "your-app-id",
  "RoomId": "room-uuid",
  "TaskId": "task-id",
  "Command": "Interrupt"
}
```

**カスタムアナウンスの例**：

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

### レスポンス

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

成功時は `Result` が空オブジェクトです。失敗時は `ResponseMetadata.Error` にエラー情報が含まれます。

公式ドキュメント: [UpdateVoiceChat](https://www.volcengine.com/docs/6348/1404671)

## 設定詳細

以下は `StartVoiceChat` の `Config` パラメータで使用される設定です。

### ASRConfig

音声認識設定：

| パラメータ           | 型       | 必須    | 説明                                   |
| ------------------- | -------- | ------- | -------------------------------------- |
| `Provider`          | string   | 必須    | サービスプロバイダー、固定値 `volcano` |
| `ProviderParams`    | object   | 必須    | プロバイダー固有のパラメータ           |
| `VADConfig`         | object   | 任意    | 音声活動検出設定                      |
| `VolumeGain`        | number   | 任意    | 音量ゲイン（0.0～1.0）、デフォルト `0.5` |
| `TurnDetectionMode` | number   | 任意    | ターン検出モード                      |
| `InterruptConfig`   | object   | 任意    | 割り込み設定                         |

**ProviderParams**:

| パラメータ           | 型       | 説明                                      |
| ------------------- | -------- | ----------------------------------------- |
| `AppId`             | string   | ASRアプリケーションID                     |
| `Mode`              | string   | 認識モード：`smallmodel` または `bigmodel` |
| `Cluster`           | string   | サービスクラスター、デフォルト `volcengine_streaming_common` |
| `context`           | string   | ホットワードコンテキスト（JSON形式）       |
| `boosting_table_id` | string   | ホットワードテーブルID                     |
| `correct_table_id`  | string   | 補正テーブルID                            |

**VADConfig**（音声活動検出）:

| パラメータ     | 型       | 説明                                   |
| ------------- | -------- | -------------------------------------- |
| `SilenceTime` | number   | 無音検出閾値（ms）、デフォルト `600`  |
| `SpeechTime`  | number   | 発話検出閾値（ms）                    |
| `PrefixTime`  | number   | プレフィックス時間（ms）               |
| `SuffixTime`  | number   | サフィックス時間（ms）                 |
| `Sensitivity` | number   | 感度                                  |
| `AIVAD`       | boolean  | AI VADを有効化                        |

**InterruptConfig**:

| パラメータ                 | 型         | 説明                                   |
| ------------------------- | ---------- | -------------------------------------- |
| `InterruptSpeechDuration` | number     | 割り込み発話時間（ms）、デフォルト `400` |
| `InterruptKeywords`       | string[]   | セマンティック割り込みキーワードリスト |

**設定例**:

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

音声合成設定：

| パラメータ           | 型         | 必須    | 説明                                   |
| ------------------- | ---------- | ------- | -------------------------------------- |
| `Provider`          | string     | 必須    | サービスプロバイダー、固定値 `volcano` |
| `ProviderParams`    | object     | 必須    | プロバイダー固有のパラメータ           |
| `IgnoreBracketText` | number[]   | 任意    | 無視する括弧の種類                     |

**ProviderParams**:

| パラメータ    | 型       | 説明                 |
| ------------ | -------- | -------------------- |
| `app`        | object   | アプリケーション設定  |
| `audio`      | object   | オーディオ設定       |
| `ResourceId` | string   | TTSリソースID        |
| `Additions`  | object   | 追加設定             |

**app設定**:

| パラメータ | 型       | 説明                    |
| --------- | -------- | ----------------------- |
| `appid`   | string   | TTSアプリケーションID   |
| `token`   | string   | TTSアプリケーショントークン |
| `cluster` | string   | サービスクラスター、デフォルト `volcano_tts` |

**audio設定**:

パラメータはTTSモードにより若干異なります：

| パラメータ          | 型       | 説明                     | 対応モード          |
| ------------------ | -------- | ------------------------ | ------------------- |
| `voice_type`       | string   | 音声タイプ               | 全モード            |
| `volume_ratio`     | number   | 音量（0.5～2.0）         | 全モード            |
| `speed_ratio`      | number   | 話速（0.5～2.0）         | standard            |
| `pitch_ratio`      | number   | ピッチ（0.5～2.0）       | standard            |
| `speech_ratio`     | number   | 話速（0.5～2.0）         | bigtts              |
| `pitch_rate`       | number   | ピッチレート             | bigtts              |
| `speech_rate`      | number   | 話速                     | bidirection         |
| `emotion`          | string   | 感情：`happy`、`sad`、`angry`、`neutral` | 感情対応音声        |
| `emotion_strength` | number   | 感情強度（0.0～1.0）     | 感情対応時          |

::: tip TTSモード
- `standard`：標準モード、`speed_ratio`、`pitch_ratio`を使用
- `bigtts`：大規模モデルTTS、`speech_ratio`、`pitch_rate`を使用
- `bidirection`：双方向ストリーミング、`speech_rate`を使用し`Additions`設定をサポート
:::

**代表的な音声**:

| 音声ID             | 説明             |
| ------------------ | ---------------- |
| `BV033_streaming`  | 女性、やさしい声 |
| `BV001_streaming`  | 男性、魅力的な声 |
| `BV700_streaming`  | 女性、甘い声     |
| `BV406_streaming`  | 男性、落ち着いた声 |

詳細は[Volcano Engine TTS音声一覧](https://www.volcengine.com/docs/6561)をご参照ください。

**設定例**:

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

大規模言語モデル設定：

| パラメータ        | 型         | 必須         | 説明                                                      |
| ---------------- | ---------- | ------------ | --------------------------------------------------------- |
| `Mode`           | string     | 必須         | モード：`ArkV3`（Ark）または `CustomLLM`（カスタム）      |
| `Url`            | string     | CustomLLMのみ | CustomLLMコールバックURL                                  |
| `APIKey`         | string     | 任意         | API認証キー                                               |
| `EndPointId`     | string     | ArkV3のみ    | ArkモデルエンドポイントID                                 |
| `ModelName`      | string     | 任意         | モデル名                                                  |
| `SystemMessages` | string[]   | 任意         | システムプロンプト                                        |
| `UserPrompts`    | object[]   | 任意         | 事前設定された会話履歴                                    |
| `Temperature`    | number     | 任意         | サンプリング温度（0.0～1.0）、デフォルト `0.5`           |
| `TopP`           | number     | 任意         | Top-pサンプリング（0.0～1.0）、デフォルト `0.9`          |
| `MaxTokens`      | number     | 任意         | 最大トークン数、デフォルト `256`                          |
| `HistoryLength`  | number     | 任意         | 保持する履歴ターン数、デフォルト `15`                     |
| `EnableRoundId`  | boolean    | 任意         | ラウンドIDを有効化                                        |
| `VisionConfig`   | object     | 任意         | ビジョン理解設定：`Enable`（boolean）、`SnapshotConfig`（object） |
| `Custom`         | string     | 任意         | カスタムパラメータ（JSON文字列）、CustomLLMへ透過的に渡される |

**UserPrompts**（事前設定された会話履歴）例：

```json
[
  { "Role": "assistant", "Content": "Hello! How can I help you?" },
  { "Role": "user", "Content": "Hello" }
]
```

**CustomLLMモード例**：

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

**ArkV3モード例**：

```json
{
  "Mode": "ArkV3",
  "EndPointId": "your-endpoint-id",
  "Temperature": 0.7,
  "MaxTokens": 512
}
```

## CustomLLMコールバック

CustomLLMモード使用時、Volcano Engineはユーザーの音声認識結果をカスタムサービスに送信します。

### コールバックフロー

```
ユーザー音声 → Volcano Engine ASR → CustomLLMサービス → Volcano Engine TTS → ユーザー
```

### リクエスト形式

Volcano EngineからCustomLLMサービスへのリクエスト例：

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

**リクエストフィールド**：

| フィールド     | 説明                                   |
| ------------- | ------------------------------------- |
| `messages`    | OpenAI形式の会話履歴                   |
| `stream`      | 固定値 `true`、ストリーミング応答が必要 |
| `temperature` | サンプリング温度                      |
| `max_tokens`  | 最大生成長                           |
| `device_id`   | カスタムパラメータ、`LLMConfig.Custom`から透過的に渡される |

### レスポンス形式

レスポンスはOpenAI SSE形式に準拠する必要があります：

```
data: {"id":"resp-1","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"role":"assistant","content":""},"finish_reason":null}],"model":"qwen-flash","created":1704355200}

data: {"id":"resp-1","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"content":"Hello"},"finish_reason":null}],"model":"qwen-flash","created":1704355200}

data: {"id":"resp-1","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"content":"!"},"finish_reason":"stop"}],"model":"qwen-flash","created":1704355200}

data: [DONE]
```

**レスポンス要件**：

- SSEストリーミングレスポンスを返すこと
- Content-Typeは `text/event-stream`
- 各行は `data: ` で始まること
- 最終行は `data: [DONE]` であること

公式ドキュメント: [CustomLLM連携](https://www.volcengine.com/docs/6348/1399966)

## RTCトークン

クライアントはRTCルームに参加するためにトークンが必要です。トークンはサーバー側で `AppKey` を使って生成します。

### トークン構造

```
Token = Version + AppId + Base64(Message + Signature)
```

- **Version**：固定値 `001`
- **AppId**：24文字のアプリケーション識別子
- **Message**：バイナリエンコードされたペイロード（RoomId、UserId、有効期限、権限）
- **Signature**：AppKeyを用いたHMAC-SHA256署名

### トークン権限

| 権限                 | 説明                 |
| -------------------- | -------------------- |
| `PrivPublishStream`   | 音声・映像のパブリッシュ |
| `PrivSubscribeStream` | ストリームのサブスクライブ |

### 有効期限

デフォルトの有効期限は24時間（86,400秒）です。期限切れ後は再生成が必要です。

### 例

```typescript
import { AccessToken } from './rtctoken'

const token = new AccessToken(appId, appKey, roomId, userId)
const expireAt = Math.floor(Date.now() / 1000) + 24 * 3600
token.addPrivilege('PrivPublishStream', expireAt)
token.addPrivilege('PrivSubscribeStream', expireAt)
token.expireTime(expireAt)
const tokenString = token.serialize()
```

トークン生成ライブラリについては[インストールとテスト - RTCトークンの生成](./installation-and-testing.md#generating-an-rtc-token)をご参照ください。

## エラーコード

### レスポンス形式

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

### よくあるエラーコード

| エラーコード             | HTTPステータス | 説明                           |
| ------------------------ | -------------- | ------------------------------ |
| `MissingParameter`       | 400            | 必須パラメータが不足している   |
| `InvalidParameter`       | 400            | パラメータ形式が不正           |
| `MissingRequestInfo`     | 400            | リクエスト情報が不足している   |
| `InvalidTimestamp`       | 400            | タイムスタンプが無効または期限切れ |
| `InvalidAuthorization`   | 400            | Authorizationヘッダーが無効    |
| `InvalidCredential`      | 400            | 認証情報の形式が不正           |
| `InvalidAccessKey`       | 401            | AccessKeyが無効                |
| `SignatureDoesNotMatch`  | 401            | 署名検証に失敗                |
| `InvalidSecretToken`     | 401            | STSトークンが無効または期限切れ |
| `AccessDenied`           | 403            | IAM権限が不足している           |
| `ServiceNotFound`        | 404            | サービスが見つからない          |
| `InvalidActionOrVersion` | 404            | APIアクションまたはバージョンが無効 |
| `FlowLimitExceeded`      | 429            | レート制限超過                 |
| `InternalError`          | 500            | 内部エラー                    |
| `InternalServiceError`   | 502            | ゲートウェイエラー             |
| `ServiceUnavailableTemp` | 503            | サービス一時的に利用不可       |
| `InternalServiceTimeout` | 504            | サービスタイムアウト           |

### ビジネスエラーコード

| エラーコード     | 説明                         |
| ---------------- | ---------------------------- |
| `RoomNotExist`   | ルームが存在しない           |
| `TaskNotExist`   | タスクが存在しない           |
| `InvalidToken`   | RTCトークンが無効または期限切れ |

公式ドキュメント: [共通エラーコード](https://www.volcengine.com/docs/6369/68677)

## 関連リソース

- [インストールとテスト](./installation-and-testing.md)
- [Volcano Engine リアルタイム会話APIドキュメント](https://www.volcengine.com/docs/6348/1315560) - 公式完全ドキュメント
- [Volcano Engine リアルタイム音声・映像ドキュメント](https://www.volcengine.com/docs/6348)
