# インストールとテスト

本ドキュメントでは、Volcano Engineの音声サービスを統合し、基本的なテストを完了する方法を説明します。Volcano Engineは複数のプラットフォーム向けにSDKを提供しており、本ガイドではWeb SDK（`@volcengine/rtc`）を例に統合手順を解説します。

## 前提条件

統合を開始する前に、必要なVolcano Engineサービスを有効化し、認証情報を設定していることを確認してください。詳細な手順は[クイックスタート – Volcano Engine認証情報](./quick-start.md#4-volcano-engine-credentials)を参照してください。

必要な認証情報：

| 認証情報                                     | 用途                                   |
| -------------------------------------------- | -------------------------------------- |
| `AppId` / `AppKey`                           | RTCルーム接続およびトークン生成         |
| `AccessKeyId` / `SecretKey`                  | OpenAPIリクエスト署名                   |
| `ASR AppId`                                  | 音声認識サービス                       |
| `TTS AppId` / `TTS Token` / `TTS ResourceId` | 音声合成サービス                       |

## 認証プロキシサービス

クライアントはRTCルームに参加するためにトークンが必要であり、そのトークンは`AppKey`を用いて生成されます。音声セッションを開始するには`StartVoiceChat` APIを呼び出す必要があり、こちらは`AccessKey`で署名する必要があります。これらの認証情報はクライアントに公開してはならないため、認証プロキシサービスが必要です。

プロキシサービスの役割：

- `AppKey`を用いたRTCトークンの生成
- `AccessKey`を用いたVolcano Engine OpenAPIの呼び出し
- クライアントへの`Token`およびルーム情報の返却

### RTCトークンの生成

`Token`はHMAC-SHA256アルゴリズムを用いて`AppKey`から生成されます。

| 言語        | 参照実装                                                     |
| ----------- | ------------------------------------------------------------ |
| Node.js / Bun | [token.ts](https://github.com/emqx/mcp-ai-companion-demo/tree/volcengine/rtc/volc-server/src/lib/token.ts) |

```typescript
import { AccessToken, Privileges } from './rtctoken'

const token = new AccessToken(appId, appKey, roomId, userId)
token.addPrivilege(Privileges.PrivPublishStream, expireTime)
const tokenString = token.serialize()  // クライアントに返却
```

### Volcano Engine OpenAPIの呼び出し

`StartVoiceChat`や`StopVoiceChat`などのAPIは、`AccessKeyId`と`SecretKey`を用いたV4署名が必要です。公式OpenAPI SDKには署名用の`Signer`クラスが提供されています。

```bash
# Node.js / Bun
npm install @volcengine/openapi

# Python
pip install volcengine-python-sdk

# Go
go get github.com/volcengine/volc-sdk-golang
```

```typescript
// Node.jsの例
import { Signer } from '@volcengine/openapi'

const body = { AppId: appId, RoomId: roomId, /* ... */ }

// リクエストデータの構築
const openApiRequestData = {
  region: 'cn-north-1',
  method: 'POST',
  params: {
    Action: 'StartVoiceChat',
    Version: '2024-12-01',
  },
  headers: {
    Host: 'rtc.volcengineapi.com',
    'Content-Type': 'application/json',
  },
  body,
}

// Signerを作成し認証ヘッダーを追加
const signer = new Signer(openApiRequestData, 'rtc')
signer.addAuthorization({
  accessKeyId: process.env.ACCESS_KEY_ID,
  secretKey: process.env.SECRET_KEY,
})

// リクエスト送信（ヘッダーに署名が含まれる）
const response = await fetch(
  'https://rtc.volcengineapi.com?Action=StartVoiceChat&Version=2024-12-01',
  {
    method: 'POST',
    headers: openApiRequestData.headers,
    body: JSON.stringify(body),
  }
)
```

詳細な署名ルールは[Volcano Engine V4署名アルゴリズム](https://www.volcengine.com/docs/6369/67269)を参照してください。

### API設計例

プロキシサービスはクライアント向けに以下のAPIを公開することが推奨されます。

```typescript
// シーン設定取得 – Tokenとルーム情報を返す
GET /api/scenes
Response: {
  scenes: [{
    id: string,
    rtcConfig: { appId: string, roomId: string, userId: string, token: string }
  }]
}

// 音声セッション開始
POST /api/voice/start
Request:  { sceneId: string }
Response: { success: boolean }

// 音声セッション停止
POST /api/voice/stop
Request:  { sceneId: string }
Response: { success: boolean }
```

サーバー側実装のポイント：

- **シーン設定**：サーバーは初期化時に各シーンごとに`roomId`（UUID）と`userId`を生成し、`AppKey`を用いて対応するRTCトークン（有効期限24時間）を作成します。クライアントは`/api/scenes`経由でこれら情報を取得し、RTCルームに参加します。
- **トークン利用**：クライアントはRTC SDKの`joinRoom`メソッドにトークンを渡して認証します。
- **音声セッション開始/停止**：サーバーは`sceneId`でシーン設定を参照し、`roomId`などを取得後、Volcano Engine OpenAPIの`StartVoiceChat`や`StopVoiceChat`を呼び出します。

## Web統合

Volcano EngineはWeb統合向けに`@volcengine/rtc` SDKを提供しています。クライアントとサーバー間のやり取りの流れは以下の通りです。

![通話フロー](https://lf3-static.bytednsdoc.com/obj/eden-cn/UJjvKJ%5BY/ljhwZthlaukjlkulzlp/1310560_plantuml_diagram2.png)

### SDKのインストール

```bash
npm install @volcengine/rtc
```

AIノイズリダクションには`@volcengine/rtc/extension-ainr`拡張機能が含まれています。

### 基本的な統合フロー

#### 1. シーン設定の取得

RTC SDKを使用する前に、サーバーAPIを呼び出してシーン設定（Tokenやルーム情報）を取得します。

```typescript
// サーバーAPIを呼び出してシーン設定を取得
const response = await fetch('/api/scenes')
const { scenes } = await response.json()

// 対象シーンを選択
const scene = scenes.find(s => s.id === 'your-scene-id') || scenes[0]
const { appId, roomId, token, userId } = scene.rtcConfig
```

#### 2. RTCエンジンの作成

```typescript
import VERTC, { RoomProfileType, MediaType } from '@volcengine/rtc'

// サーバーから取得したappIdを用いてエンジンインスタンスを作成
const engine = VERTC.createEngine(appId)
```

#### 3. イベントリスナーの登録

```typescript
// エラー検知
engine.on(VERTC.events.onError, (event) => {
  console.error('RTCエラー:', event.errorCode)
})

// リモートユーザーのストリーム公開検知（AI音声応答）
engine.on(VERTC.events.onUserPublishStream, async (event) => {
  const { userId, mediaType } = event
  // リモートの音声ストリームをサブスクライブ
  await engine.subscribeStream(userId, mediaType)
})

// バイナリメッセージ受信（字幕、状態など）
engine.on(VERTC.events.onRoomBinaryMessageReceived, (event) => {
  const { message } = event
  // messageはTLV形式のArrayBuffer
  // ASR結果、TTSテキスト、エージェント状態などを含む
})
```

#### 4. ルームへの参加

```typescript
// ステップ1で取得したtoken、roomId、userIdを用いてルームに参加
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

#### 5. マイクの起動と音声のパブリッシュ

```typescript
// マイクキャプチャ開始
await engine.startAudioCapture()

// 音声ストリームをルームにパブリッシュ
await engine.publishStream(MediaType.AUDIO)
```

#### 6. 音声セッションの開始

音声ストリームをパブリッシュ後、サーバーAPIを呼び出してAI音声セッションを開始します。

```typescript
// 音声セッション開始
await fetch('/api/voice/start', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ sceneId: scene.id }),
})
```

これで音声対話が開始されます。ユーザーの発話はASRで認識され、LLMで処理され、TTSで再生されます。

#### 7. ルームからの退出

```typescript
// パブリッシュ停止
await engine.unpublishStream(MediaType.AUDIO)

// キャプチャ停止
await engine.stopAudioCapture()

// ルーム退出
await engine.leaveRoom()

// エンジン破棄
VERTC.destroyEngine(engine)

// サーバーAPIを呼び出して音声セッション停止
await fetch('/api/voice/stop', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ sceneId: scene.id }),
})
```

### AIノイズリダクション（任意）

Volcano Engine RTC SDKには環境ノイズを効果的に除去するAIノイズリダクション拡張機能が含まれています。

```typescript
import RTCAIAnsExtension, { AnsMode } from '@volcengine/rtc/extension-ainr'

// 拡張機能の作成と登録
const aiAnsExtension = new RTCAIAnsExtension()
engine.registerExtension(aiAnsExtension)

// サポート確認
const supported = await aiAnsExtension.isSupported()
if (supported) {
  // ノイズリダクションモード設定：LOW / MEDIUM / HIGH
  await aiAnsExtension.setAnsMode(AnsMode.MEDIUM)
  // ノイズリダクション有効化
  aiAnsExtension.enable()
}
```

### リモート音声ストリームの受信

リモートストリームをサブスクライブ後、`MediaStream`を取得して再生できます。

```typescript
import { StreamIndex } from '@volcengine/rtc'

// リモートユーザーの音声トラック取得
const audioTrack = engine.getRemoteStreamTrack(userId, StreamIndex.STREAM_INDEX_MAIN, 'audio')

// MediaStream作成と再生
const stream = new MediaStream()
if (audioTrack) {
  stream.addTrack(audioTrack)
}

// audio要素にバインドして再生
const audioElement = document.querySelector('audio')
audioElement.srcObject = stream
```

## 他プラットフォーム向けSDK

Volcano Engine RTC SDKはソフトウェアアプリケーションとハードウェアデバイスの両方をサポートしています。

### ソフトウェアアプリケーション

参照：[リアルタイム対話型AI統合（ソフトウェアアプリケーション）](https://www.volcengine.com/docs/6348/1310560)

| プラットフォーム | SDK               | ドキュメント                                               |
| ---------------- | ----------------- | ---------------------------------------------------------- |
| Web              | `@volcengine/rtc` | [Web SDKドキュメント](https://www.volcengine.com/docs/6348/104398)  |
| iOS              | VolcEngineRTC     | [iOS SDKドキュメント](https://www.volcengine.com/docs/6348/70080)   |
| Android          | VolcEngineRTC     | [Android SDKドキュメント](https://www.volcengine.com/docs/6348/70082) |
| Windows          | VolcEngineRTC     | [Windows SDKドキュメント](https://www.volcengine.com/docs/6348/70084) |
| macOS            | VolcEngineRTC     | [macOS SDKドキュメント](https://www.volcengine.com/docs/6348/70086) |
| Linux            | VolcEngineRTC     | [Linux SDKドキュメント](https://www.volcengine.com/docs/6348/113623) |
| Flutter          | volc_engine_rtc   | [Flutter SDKドキュメント](https://www.volcengine.com/docs/6348/113661) |
| Electron         | @volcengine/rtc   | [Electron SDKドキュメント](https://www.volcengine.com/docs/6348/112063) |

### ハードウェアデバイス

参照：[リアルタイム対話型AI統合（組み込みハードウェア）](https://www.volcengine.com/docs/6348/1438400)

Embedded Linux、RTOS、Androidなどのハードウェアプラットフォームをサポートしています。ハードウェアSDKはVolcano Engine技術サポートに問い合わせて入手してください。

## テストと検証

### RTC接続の確認

ルーム参加成功後、以下のイベントで確認できます。

```typescript
engine.on(VERTC.events.onUserJoined, (event) => {
  console.log('ユーザー参加:', event.userInfo.userId)
})
```

### 音声認識の確認

マイクに向かって話し、`onRoomBinaryMessageReceived`でバイナリメッセージを受信します。メッセージはTLVエンコードされており、以下を含みます。

- 字幕メッセージ：ASR結果およびLLM応答テキスト
- 状態メッセージ：エージェント状態（リスニング／思考中／発話中）
- 関数呼び出し：ツール呼び出しリクエスト

### 音声合成の確認

AI応答はリモート音声ストリームで再生されます。以下を確認してください。

1. `onUserPublishStream`が処理されている
2. `subscribeStream`が呼ばれている
3. 音声トラックが`<audio>`要素にバインドされている

### よくある問題

#### 接続と認証

| 問題                             | 考えられる原因                          | 対策                                                         |
| -------------------------------- | ------------------------------------- | ------------------------------------------------------------ |
| 無効なトークン（`token_error`） | トークン期限切れまたはパラメータ不一致 | トークン生成時のUserIdとRoomIdが参加時と一致しているか確認、またはトークンを再生成 |
| ルームに参加できない             | ネットワーク問題またはAppId誤り       | ネットワーク接続を確認し、AppIdが正しいか検証                 |
| `Invalid 'Authorization' header` | AK/SK設定誤り                        | サーバー側のAccessKeyIdとSecretKeyを確認                     |
| クロスサービス呼び出し失敗       | クロスサービス認可未設定               | RTCコンソールでクロスサービス認可を完了                       |

#### エージェント起動

| 問題                             | 考えられる原因                            | 対策                                                         |
| -------------------------------- | --------------------------------------- | ------------------------------------------------------------ |
| StartVoiceChat失敗               | 署名エラーまたはパラメータ不足           | API署名と必須パラメータを検証                                 |
| `The task has been started`エラー | 固定のRoomId/UserIdで繰り返し呼び出し    | 先にStopVoiceChatを呼び出し、その後StartVoiceChatを再度呼び出す |
| 「AI準備中」で停止               | 権限不足／パラメータエラー／残高不足      | 1) コンソールの権限確認 2) パラメータの型・大文字小文字確認 3) サービス有効化と残高確認 |
| デジタルアバターが準備中のまま  | 同時接続数制限または設定エラー            | アバターのAppId/Tokenを確認し、同時接続制限を超えていないか検証 |

#### デバイスとメディア

| 問題                         | 考えられる原因                | 対策                                                         |
| ---------------------------- | ----------------------------- | ------------------------------------------------------------ |
| マイク／カメラが起動しない   | セキュアでないコンテキスト    | ページが`localhost`または`https`でアクセスされているか確認   |
| デバイス許可が拒否される     | ブラウザの許可がない          | [Webデバイス許可トラブルシューティング](https://www.volcengine.com/docs/6348/1169947)を参照 |
| ASR結果がない                | マイク許可なしまたはASR無効  | ブラウザのマイク許可を確認し、ASRサービスが有効か検証         |
| TTS音声が出ない              | リモート音声をサブスクライブしていない | リモート音声ストリームに対して`subscribeStream`が呼ばれているか確認 |

#### モデル設定

| 問題                             | 対策                                                         |
| -------------------------------- | ------------------------------------------------------------ |
| サードパーティモデルやCoze Botを使用 | `LLMConfig`でモデルパラメータを設定し、`Mode`を`CustomLLM`にしてコールバックURLを指定 |
| 会話に応答がない                 | LLM設定を検証し、CustomLLMコールバックサービスが稼働しているか確認 |

## 関連リソース

- [Volcano Engine RTCクイックスタート](https://www.volcengine.com/docs/6348/1310553)
- [Volcano Engine OpenAPI署名仕様](https://www.volcengine.com/docs/6369/67269)
- [RTC SDKダウンロード](https://www.volcengine.com/docs/6348/75707)
- [公式Volcano Engineリアルタイム対話型AIデモ](https://github.com/volcengine/rtc-aigc-demo)
