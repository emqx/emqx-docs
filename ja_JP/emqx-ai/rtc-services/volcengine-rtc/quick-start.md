# EMQX + Volcano Engine RTCでリアルタイム音声エージェントを構築する

本ドキュメントでは、Docker Composeを用いてAIエージェントのデモをデプロイする方法を説明します。本デモはブラウザ上の知能人形をスマートデバイスのシミュレーションとして使用し、[Volcano Engine RTC](https://www.volcengine.com/product/veRTC/ConversationalAI)を活用した低レイテンシ音声対話、MCPを介したMQTTプロトコルによるデバイス側機能（写真撮影、表情切替、音量制御など）の呼び出し、そしてVolcano Engineの`CustomLLM`モードを用いたカスタムAIエージェントサービスとの多ターン会話およびツール呼び出しの統合を実演します。音声対話からデバイス制御までの一連のワークフローを紹介します。

[デモ動画](https://www.bilibili.com/video/BV1P2WTzBEu4/)もぜひご覧ください。

## アーキテクチャ概要

### コンポーネント

システムは以下の3つのコアコンポーネントで構成されています：

| コンポーネント | 役割                  | ポート | 主な責務                                                     |
| -------------- | --------------------- | ------ | ------------------------------------------------------------ |
| volc-server    | Volcano Engineプロキシ | 3002   | RTCルーム・トークン管理、Volcano EngineがアプリにコールバックするCustomLLMコールバックアドレスの設定 |
| web            | MCPサーバー            | 8080   | フロントエンドUI、ハードウェア制御ツール（カメラ／表情／音量）を公開 |
| app            | MCPクライアント＋AIエージェント | 8081   | `/chat-stream`エンドポイントを提供、LLM/VLM推論およびMCPツール呼び出しを処理 |

### 通信フロー

```mermaid
sequenceDiagram
    autonumber
    participant WebUI as Web UI
    participant Volc as volc-server
    participant RTC as Volcano Cloud RTC
    participant App as App
    participant Cloud as Volcano Cloud

    WebUI ->> Volc: シナリオ設定とRTC認証情報の要求

    WebUI ->> RTC: リアルタイム音声/映像接続の確立
    RTC ->> WebUI: リアルタイム音声/映像接続の確立
    note over WebUI, RTC: ASR / TTS

    Cloud ->> App: CustomLLMの/cchat-streamへのコールバック（SSEストリーミング応答）

    App ->> WebUI: MQTT経由でMCPツールを呼び出し
    WebUI ->> App: カメラ／表情など

    Cloud ->> WebUI: TTS合成音声の再生

```

コア機能：

- MCP over MQTT：EMQX ブローカーを介したネットワーク間のツール呼び出し。AIエージェントがデバイス機能（カメラ、表情、音量）を制御
- マルチモーダル理解：VLMを統合し、「今何を持っている？」などの視覚ユースケースに対応
- リアルタイム音声対話：Volcano Engine RTC + ASR/TTSによるエンドツーエンドの低レイテンシ音声認識・合成
- 並列処理アーキテクチャ：ツール呼び出しと音声合成を非同期で実行し、よりスムーズなユーザー体験を提供

## 前提条件

### 1. Docker環境

Docker 24以上（`docker --version`で確認）。

### 2. MQTTブローカー

本プロジェクトでは、webサービス（MCPサーバー）とappサービス（MCPクライアント＋AIエージェント）コンテナが接続可能なEMQXブローカーが必要です。

導入方法（いずれかを選択）：

- 自己ホスティング：[EMQXインストールガイド](https://docs.emqx.com/zh/emqx/latest/deploy/install.html)を参照
- マネージドサービス：[EMQX Cloud](https://docs.emqx.com/zh/cloud/latest/)を利用

設定例：

```
MQTT_BROKER_HOST=localhost        # EMQXブローカーのホスト
MQTT_BROKER_PORT=1883             # MQTTポート
MQTT_USERNAME=your_username       # 認証有効時のユーザー名
MQTT_PASSWORD=your_password       # 認証有効時のパスワード
```

### 3. LLM APIキー

本プロジェクトはVolcano Engine CustomLLMモードを通じてカスタムAIエージェントを統合します。デフォルトではAlibaba Cloud Bailianの`qwen-flash`モデルを使用します。

#### Alibaba Cloud Bailianの有効化

1. [Alibaba Cloud Bailianコンソール](https://bailian.console.aliyun.com)にアクセス
2. 上部に有効化プロンプトがあればクリックしてサービスを有効化（無料枠内は無料、有料枠超過時に課金）
3. 必要に応じて本人確認を完了

#### APIキーの作成

1. [API-KEY管理](https://bailian.console.aliyun.com/#/api-key)にアクセス
2. API-Keyタブで「API-KEYを作成」をクリック
3. アカウントとワークスペース（通常はデフォルト）を選択し、説明を入力して確定
4. APIキーの横にあるコピーアイコンをクリックしてシークレットを取得
5. `app/.env`の`DASHSCOPE_API_KEY`にAPIキーを設定

#### 他モデルサービスの利用（任意）

他のOpenAI互換モデルサービスを利用する場合は、`app/.env`を以下のように更新：

```
LLM_API_BASE=https://your-model-service.com/v1  # モデルサービスのベースURL
LLM_API_KEY=your_api_key                        # モデルサービスのAPIキー
LLM_MODEL=your_model_name                       # モデル名
```

代表的なモデルサービスエンドポイント：

- OpenAI: `https://api.openai.com/v1`
- DeepSeek: `https://api.deepseek.com/v1`
- その他互換サービス：各プロバイダーのドキュメント参照

LLMサービスによってレイテンシやコストが大きく異なります。要件に応じて選択してください。低レイテンシを重視する場合は、デフォルトのAlibaba Cloud Bailian `qwen-flash`がおすすめです。

### 4. Volcano Engine認証情報

本プロジェクトでは複数のVolcano Engineサービスを使用します。[Volcano Engineコンソール](https://console.volcengine.com/home)で登録・ログインしてください。

有効化が必要なサービス：

1. RTCサービス — [有効化ガイド](https://www.volcengine.com/docs/6348/69865)
   - 有効化後、`VOLC_RTC_APP_ID`と`VOLC_RTC_APP_KEY`を取得
   - 取得場所：[RTCコンソール](https://console.volcengine.com/rtc/aigc/listRTC)
2. ASR/TTS音声サービス — [Doubao Speechコンソール](https://console.volcengine.com/speech/app)
   - アプリ作成時に以下を選択：
     - ASR：ストリーミング音声認識
     - TTS：音声合成
   - 以下の認証情報を取得：
     - `VOLC_ASR_APP_ID` - ASRアプリID
     - `VOLC_TTS_APP_ID` - TTSアプリID
     - `VOLC_TTS_APP_TOKEN` - TTSアプリトークン
     - `VOLC_TTS_RESOURCE_ID` - TTSリソースID（選択した音声に依存）
3. アカウント認証情報 — [キー管理](https://console.volcengine.com/iam/keymanage/)
   - `VOLC_ACCESS_KEY_ID` - アクセスキーID
   - `VOLC_SECRET_KEY` - シークレットアクセスキー

#### 権限設定

必須：RTCコンソールでクロスサービス認可を設定してください。設定しないとエージェントはASR/TTS/LLMサービスを正しく呼び出せません。

メインアカウント呼び出し（推奨、簡単）：

1. メインアカウントで[RTCコンソール](https://console.volcengine.com/rtc)にログイン
2. [クロスサービス認可](https://console.volcengine.com/rtc/aigc/iam)へ移動
3. 「ワンクリックでクロスサービス認可を有効化」をクリックし、`VoiceChatRoleForRTC`ロールを設定
4. メインアカウントのAK/SKでサービスを呼び出す

サブアカウント呼び出し（任意、追加設定が必要）：

1. メインアカウントで[RTCコンソール](https://console.volcengine.com/rtc)にログイン
2. [クロスサービス認可](https://console.volcengine.com/rtc/aigc/iam)で「サブアカウントに権限付与」をクリック
3. サブアカウントを検索して権限を付与

完全な有効化手順：[リアルタイム会話AIの前提条件](https://www.volcengine.com/docs/6348/1315561)

#### LLM設定

本プロジェクトはCustomLLMモードを使用し、Volcano EngineがアプリのカスタムAIエージェントサービスにコールバックしてLLM応答を取得します。

主な設定：

- `VOLC_LLM_URL` - アプリの`/chat-stream`エンドポイントを指す
  - ローカルデプロイ：`http://app:8081/chat-stream`（コンテナネットワーク内）
  - 本番環境：`https://your-domain.com/chat-stream`（公開アクセス可能であること）
- `VOLC_LLM_API_KEY` - カスタム認証キー。アプリの`CUSTOM_LLM_API_KEY`と一致させる必要があります（「ステップ2: 環境変数設定」参照）

オプションのモデルソース：

- Volcano Ark：[Arkコンソール](https://console.volcengine.com/ark/region:ark+cn-beijing/endpoint)で推論エンドポイントまたはアプリを作成
- Cozeプラットフォーム：[Coze](https://www.coze.cn)でAgentを作成 — [ガイド](https://www.coze.cn/open/docs/guides/quickstart)
- サードパーティモデル：OpenAI互換サービスURLを準備 — [要件](https://www.volcengine.com/docs/6348/1399966)

注：本プロジェクトのアプリサービスはすでにCustomLLMプロトコルを実装済みです。APIキー（`DASHSCOPE_API_KEY`など）を設定するだけで、追加のモデルサービスデプロイは不要です。

#### パラメータの素早い取得

推奨：公式Volcano Engineデモを使い設定を素早く検証可能です。

1. [リアルタイム会話AIデモ](https://console.volcengine.com/rtc/aigc/run)を開く
2. デモ実行後、右上の「Access API」ボタンをクリック
3. パラメータ設定スニペットをコピーし、必要な認証情報を抽出

### 5. ネットワーク要件

開放すべきポート（デフォルト、Composeファイルで調整可能）：

- `8080` - Web UI
- `8081` - Appバックエンド（SSEエンドポイント）
- `3002` - volc-serverプロキシ（Volcano Engineサービス設定）

アクセス要件：

重要：本プロジェクトでMCP over MQTTを完全に体験するには、appサービスの`/chat-stream`エンドポイントを公開可能なHTTPS環境にデプロイし、Volcano Engineがコールバックできるようにする必要があります。

- 本番環境（推奨）：appを公開HTTPS URL（例：`https://your-domain.com/chat-stream`）にデプロイし、SSEストリームが`data: [DONE]`で正しく終了することを確認
- ローカルテスト：非公開環境ではLLM推論とMCP over MQTTツール呼び出しAPIのみテスト可能。Volcano Engine音声対話は完全には体験できません

## クイックチュートリアル：10分で音声対話＋デバイス制御デモを動かす

前提条件をすべて満たしたら、以下の手順でAIエージェントデモを素早くセットアップできます（「デバイス」はWeb UIでシミュレーションされています）。

### ステップ1：コードを取得

```bash
git clone -b volcengine/rtc https://github.com/emqx/mcp-ai-companion-demo.git
cd mcp-ai-companion-demo
```

### ステップ2：環境変数を設定

最も重要なステップです。前提条件で取得した認証情報を3つのサービスの設定ファイルに正確に入力してください。各項目の説明と取得元をよく確認してください。

#### 2.1 appサービス（AIエージェントバックエンド）の設定

設定ファイルを作成：

```bash
cp app/.env.example app/.env
```

`app/.env`を編集し、以下を入力：

```bash
# ===== LLM設定 =====
# 取得元：前提条件「3. LLM APIキー」
# 目的：AIエージェントがLLMを呼び出して会話推論を行う
DASHSCOPE_API_KEY=sk-xxxxxxxxxxxxx  # Alibaba Cloud BailianのAPIキーに置換

# 他モデルサービスを使う場合は以下も設定：
# LLM_API_BASE=https://api.openai.com/v1
# LLM_MODEL=gpt-4

# ===== CustomLLM認証キー =====
# 取得元：自分で生成（強力なランダム文字列を推奨）
# 目的：Volcano Engineがコールバックリクエストを検証するために使用
# 要件：volc-serverのVOLC_LLM_API_KEYと完全一致させること
CUSTOM_LLM_API_KEY=your-strong-random-secret-key-here

# 生成例（ターミナルで実行）：
# openssl rand -base64 32
# またはオンラインツール：https://www.random.org/strings/

# ===== MQTTブローカー設定 =====
# 取得元：前提条件「2. MQTTブローカー」
# 目的：MCP over MQTT通信のためEMQXブローカーに接続
MQTT_BROKER_HOST=localhost        # EMQXブローカーのホスト
MQTT_BROKER_PORT=1883             # MQTTポート

# EMQX認証有効時：
MQTT_USERNAME=your_mqtt_username  # EMQXユーザー名（任意）
MQTT_PASSWORD=your_mqtt_password  # EMQXパスワード（任意）

# ===== 任意設定 =====
MCP_TOOLS_WAIT_SECONDS=5          # MCPツール登録待機秒数
PHOTO_UPLOAD_DIR=uploads          # 写真アップロードディレクトリ
# APP_SSL_CERTFILE=/path/to/cert  # HTTPS証明書（本番用）
# APP_SSL_KEYFILE=/path/to/key    # HTTPS秘密鍵（本番用）
```

補足：

- `DASHSCOPE_API_KEY`と`CUSTOM_LLM_API_KEY`の違い：

  - `DASHSCOPE_API_KEY`：アプリがAlibaba Cloud Bailian（または他のLLMサービス）を呼び出す際に使用
  - `CUSTOM_LLM_API_KEY`：Volcano Engineからのコールバック認証に使用（APIゲートウェイトークンのような役割）

- `CUSTOM_LLM_API_KEY`の生成方法（いずれか）：

  ```bash
  # 方法1：openssl推奨
  openssl rand -base64 32
  
  # 方法2：Python
  python3 -c "import secrets; print(secrets.token_urlsafe(32))"
  
  # 方法3：オンラインツール
  # https://www.random.org/strings/ （長さ32、英数字）
  ```

#### 2.2 volc-serverサービス（Volcano Engineプロキシ）の設定

設定ファイルを作成：

```bash
cp volc-server/.env.example volc-server/.env
```

`volc-server/.env`を編集し、Volcano Engine認証情報を入力：

```bash
# ===== Volcano Engineアカウント認証情報 =====
# 取得元：前提条件「4. Volcano Engine認証情報 > アカウント認証情報」
# 取得場所：https://console.volcengine.com/iam/keymanage/
VOLC_ACCESS_KEY_ID=AKLT*********************
VOLC_SECRET_KEY=************************************

# ===== RTCサービス認証情報 =====
# 取得元：前提条件「4. Volcano Engine認証情報 > RTCサービス」
# 取得場所：https://console.volcengine.com/rtc/aigc/listRTC
VOLC_RTC_APP_ID=your_rtc_app_id
VOLC_RTC_APP_KEY=your_rtc_app_key

# ===== ASR/TTS音声サービス認証情報 =====
# 取得元：前提条件「4. Volcano Engine認証情報 > ASR/TTS音声サービス」
# 取得場所：https://console.volcengine.com/speech/app
VOLC_ASR_APP_ID=your_asr_app_id
VOLC_TTS_APP_ID=your_tts_app_id
VOLC_TTS_APP_TOKEN=your_tts_app_token
VOLC_TTS_RESOURCE_ID=your_tts_resource_id

# ===== CustomLLM設定 =====
# 目的：Volcano EngineにLLM応答取得先エンドポイントを通知

# VOLC_LLM_URL - appの/chat-streamエンドポイント
# ローカルテスト：Dockerコンテナネットワークを利用
# VOLC_LLM_URL=http://app:8081/chat-stream
# 本番環境：公開HTTPS URLである必要あり（Volcano Engineコールバック用）
VOLC_LLM_URL=https://your-domain.com/chat-stream

# VOLC_LLM_API_KEY - CustomLLM認証キー
# 要件：app/.envのCUSTOM_LLM_API_KEYと完全一致
VOLC_LLM_API_KEY=your-strong-random-secret-key-here  # appと一致させる
```

設定チェックリスト：

| 項目                       | 設定例                                                      | 取得元                                               |
| -------------------------- | ------------------------------------------------------------ | ---------------------------------------------------- |
| Volcano Engine認証情報     | `VOLC_ACCESS_KEY_ID`, `VOLC_SECRET_KEY`                      | Volcano Engineコンソール                               |
| RTCアプリ設定              | `VOLC_RTC_APP_ID`, `VOLC_RTC_APP_KEY`                        | RTCコンソール                                          |
| 音声サービス設定          | `VOLC_ASR_APP_ID`, `VOLC_TTS_APP_ID`, `VOLC_TTS_APP_TOKEN`, `VOLC_TTS_RESOURCE_ID` | Doubao Speechコンソール                                |
| LLMキー整合性              | `VOLC_LLM_API_KEY`                                           | `app/.env`の`CUSTOM_LLM_API_KEY`と完全一致させる       |
| 権限設定                  | クロスサービス認可                                          | 前提条件「権限設定」を完了すること                      |

#### 2.3 webサービス（フロントエンドUI）の設定

webサービスはビルド時環境変数を使用します。通常、ローカル開発用のデフォルト設定で十分です：

```bash
VITE_AIGC_PROXY_HOST=http://localhost:3002  # volc-serverプロキシのアドレス
```

カスタマイズが必要なのは以下の場合のみ：

- volc-serverがリモートホストにある場合
- volc-serverが3002以外のポートを使う場合

カスタマイズ例（起動前にexport）：

```bash
export VITE_AIGC_PROXY_HOST=http://your-remote-host:3002
```

#### 設定対応表まとめ

```text
前提条件                             設定ファイル場所
├─ 3. LLM APIキー               ──►  app/.env (DASHSCOPE_API_KEY)
├─ 4. Volcano Engine認証情報
│  ├─ アカウント認証情報         ──►  volc-server/.env (VOLC_ACCESS_KEY_ID/SECRET_KEY)
│  ├─ RTCサービス               ──►  volc-server/.env (VOLC_RTC_APP_ID/APP_KEY)
│  ├─ ASR/TTSサービス           ──►  volc-server/.env (VOLC_ASR_*/VOLC_TTS_*)
│  └─ LLM設定                  ──►  volc-server/.env (VOLC_LLM_URL/API_KEY)
└─ 2. MQTTブローカー           ──►  app/.env (MQTT_BROKER_HOST/PORT/USERNAME/PASSWORD)

自己生成
└─ CUSTOM_LLM_API_KEY           ──►  app/.env + volc-server/.env (一致必須)
```

ポイント：

1. `CUSTOM_LLM_API_KEY`は唯一自分で生成し、`app/.env`と`volc-server/.env`で完全一致させる必要があります。
2. `DASHSCOPE_API_KEY`はLLM呼び出し用、`CUSTOM_LLM_API_KEY`はVolcano Engineコールバック認証用です。
3. 本番環境では`VOLC_LLM_URL`を公開HTTPS URLに変更しないと、Volcano Engineがコールバックできません。

### ステップ3：サービスを起動

Docker Composeで全サービスを起動：

```bash
docker compose -f docker/docker-compose.web-volc.yml up --build
```

起動手順：

1. イメージをビルド：`mcp-app`、`mcp-volc-server`、`mcp-web`
2. コンテナ起動、以下ポートで待機：
   - `8080` - Web UI
   - `8081` - AIエージェントバックエンド
   - `3002` - Volcano Engineプロキシ

初回起動は依存関係のダウンロードやイメージビルドに数分かかる場合があります。

ログ確認（任意）：

```bash
# 全サービスのログを追跡
docker compose -f docker/docker-compose.web-volc.yml logs -f

# 特定サービスのログを追跡
docker compose -f docker/docker-compose.web-volc.yml logs -f app
```

### ステップ4：動作確認

#### 4.1 Web UIを開く

ブラウザで http://localhost:8080 にアクセス。

チャットボットのアバター、マイク、カメラボタンなどがある仮想デバイス画面が表示されます。

#### 4.2 MQTT接続設定（初回のみ）

1. 画面右上の設定アイコンをクリック
2. 設定パネルでEMQXブローカー情報を入力：
   - ブローカー：`ws://localhost:8083/mqtt`（MQTTポート1883ではなくWebSocketポート8083を使用）
   - ユーザー名：EMQX認証有効時に入力
   - パスワード：EMQX認証有効時に入力
3. 「保存」をクリック
4. 確認ダイアログで「確認」をクリックするとページが自動更新され、新設定が適用されてMQTT接続が自動確立されます

補足：

- デバイスIDは自動生成されます（形式：`web-ui-hardware-controller/{randomID}`）。手動設定不要です。
- MQTT接続成功後、MCPツールが自動登録され、AIエージェントから呼び出せるようになります。
- 接続失敗時はEMQXのWebSocketリスナーが有効か（デフォルト8083）を確認してください。

#### 4.3 音声対話を開始

画面下部のマイクボタンをクリックし、マイク使用許可を与えます。RTC接続が自動で確立され、成功するとマイクボタンが紫色に変わり、話しかけられる状態になります。

推奨テスト：

- 「こんにちは」や「話をして」と話しかけて基本会話を試す
- 「今何を持っている？」と言って写真撮影と視覚認識をトリガー
- 「音量を80%にして」や「嬉しい表情にして」と言ってデバイス制御を試す

#### 4.4 成功条件

- 音声対話：ASRの文字起こしが正確、LLMが応答をストリーミング、TTS再生が正常に動作
- MCPツール呼び出し：写真撮影、表情切替、音量調整が反映される
- ログにエラーなし：app、volc-server、ブラウザコンソールにエラーが表示されない

#### 4.5 部分機能テスト

UIとVolcano Engine設定のみ検証したい場合（カスタムAIエージェントを使わない）：

```
docker compose -f docker/docker-compose.web-volc.yml up --build volc-server web
```

モードの特徴：

- 利用可能：ASR、TTS、基本会話
- 利用不可：MCPツール呼び出し（カメラ、表情、音量制御など）

Volcano ArkプラットフォームのLLMを会話に使う場合：

1. [Arkコンソール](https://console.volcengine.com/ark)で推論エンドポイントまたはAgentアプリを作成
2. `EndpointId`（推論エンドポイント）または`BotId`（Agentアプリ）を取得
3. `volc-server/src/config.ts`でLLMを設定：

   ```typescript
   llm: {
     mode: 'ArkV3',                    // ArkプラットフォームLLMを使用
     endpointId: 'ep-xxx',             // オプション1：推論エンドポイントID（どちらか一方）
     // botId: 'bot-xxx',               // オプション2：AgentアプリID（どちらか一方）
     systemMessages: [
       { role: 'system', content: 'You are a friendly voice assistant' }
     ],
     historyLength: 5,                 // コンテキスト履歴ターン数
   }
   ```

4. volc-serverサービスを再起動し、ArkプラットフォームLLMを会話に使用

ヒント：よりスムーズな対話には、深い思考を要さないモデル（例：Doubao-proシリーズ）を使うと良いです。詳細な設定パラメータは[Volcano Engineドキュメント](https://www.volcengine.com/docs/6348/1581714)を参照してください。

### ステップ5：サービス停止

```bash
docker compose -f docker/docker-compose.web-volc.yml down
```

## FAQとトラブルシューティング

### 設定調整

#### ポート競合

ポートが使用中の場合、`docker/docker-compose.web-volc.yml`のポートマッピングを変更：

```yaml
services:
  web:
    ports:
      - "8888:8080"  # Web UIポート変更例
  app:
    ports:
      - "8082:8081"  # appポート変更例
  volc-server:
    ports:
      - "3003:3002"  # volc-serverポート変更例
```

注意：volc-serverポートを変更した場合、`VITE_AIGC_PROXY_HOST`環境変数も更新してください。

#### HTTPS有効化（本番環境）

1. 証明書ファイル（`fullchain.pem`、`privkey.pem`）を準備

   重要：単一証明書ファイルではなくフルチェーン（完全な証明書チェーン）を使用してください。Volcano Engineのコールバックはフルチェーン検証を行い、チェーン不完全だとSSLハンドシェイクに失敗します。

   - Let’s Encryptの場合：`fullchain.pem`（証明書＋中間証明書含む）を使用
   - その他CAの場合：サーバー証明書＋中間証明書を含むフルチェーンファイルを用意

2. プロジェクトディレクトリ（例：`certs/`）に証明書ファイルを配置
3. `app/.env`に証明書パスを設定：

   ```bash
   APP_SSL_CERTFILE=./certs/fullchain.pem  # フルチェーン必須
   APP_SSL_KEYFILE=./certs/privkey.pem
   ```

4. `volc-server/.env`の`VOLC_LLM_URL`をHTTPSアドレス（例：`https://your-domain.com:8081`）に更新

#### イメージを個別にビルド

特定サービスのイメージをビルドする場合：

```bash
docker build -t mcp-web:local ./web
docker build -t mcp-app:local ./app
docker build -t volc-server:local ./volc-server
```

### よくある問題

#### サービス起動問題

| 問題                           | 原因候補               | 解決策                                                       |
| ------------------------------ | ---------------------- | ------------------------------------------------------------ |
| コンテナが起動しない           | ポートが既に使用中     | 1) `lsof -i :8080`でプロセス特定 2) Composeのポート設定変更 3) `docker compose up --build`再実行 |
| 環境変数が反映されない         | `.env`ファイル未読み込み | 1) `.env`が正しいディレクトリにあるか確認 2) ファイル権限確認 3) イメージ再ビルド |

#### Volcano Engineサービス問題

| 問題                           | 原因候補                         | 解決策                                                       |
| ------------------------------ | -------------------------------- | ------------------------------------------------------------ |
| 「AI準備中」で停止             | クロスサービス認可未設定          | 1) 「権限設定」を完了 2) サービス有効化と残高確認 3) パラメータの大文字小文字確認 |
| 401/403エラー                 | AK/SKまたはトークン誤り          | 1) `VOLC_ACCESS_KEY_ID`/`VOLC_SECRET_KEY`確認 2) トークン有効期限確認 3) クロスサービス認可確認 |
| サブアカウントのクォータ制限 | デフォルトクォータ不足            | [クォータセンター](https://console.volcengine.com/quota/productList/ParameterList?ProviderCode=iam)で増加申請 |

#### LLMリクエスト問題

| 問題                           | 原因候補                   | 解決策                                                       |
| ------------------------------ | -------------------------- | ------------------------------------------------------------ |
| LLMリクエスト失敗             | APIキー誤り               | 1) `DASHSCOPE_API_KEY`が正しいか確認 2) ネットワーク接続確認 3) ログ確認：`docker compose logs app` |
| CustomLLMコールバック失敗    | 認証キー不一致             | 1) `CUSTOM_LLM_API_KEY`が両者で一致しているか確認 2) `VOLC_LLM_URL`確認 3) volc-serverからappへの到達性確認 |
| HTTPSコールバック失敗         | 証明書チェーン不完全       | フルチェーン証明書を使用。`APP_SSL_CERTFILE`は`fullchain.pem`を指す必要あり。単一`cert.pem`は不可。Volcano Engineコールバックはフルチェーン検証を必須とするためSSLハンドシェイク失敗 |

#### MCPツール呼び出し問題

| 問題                           | 原因候補                     | 解決策                                                       |
| ------------------------------ | ---------------------------- | ------------------------------------------------------------ |
| ツールが利用できない           | MQTT接続またはdevice_id問題  | 1) ブラウザコンソールでMQTT状態確認 2) Device IDが一致しているか確認 3) `MCP_TOOLS_WAIT_SECONDS=10`に増やす |
| カメラ写真撮影失敗             | 権限未許可                   | 1) ブラウザのカメラ権限確認 2) 許可をクリック 3) ページをリロード |

#### MQTT接続問題

| 問題                           | 原因候補                   | 解決策                                                       |
| ------------------------------ | -------------------------- | ------------------------------------------------------------ |
| MQTT接続失敗                   | ブローカー設定誤り         | 1) EMQXブローカーが起動しているか確認 2) `MQTT_BROKER_HOST`/`PORT`確認 3) 認証情報確認 4) ネットワーク疎通確認 |
| Web UIが接続できない           | WebSocketポート未開放       | 1) WebSocketリスナーが有効か確認（デフォルト8083） 2) `ws://`スキームを使用（例：`ws://localhost:8083/mqtt`） |

### ログの確認

```
# 全サービスのログを追跡
docker compose -f docker/docker-compose.web-volc.yml logs -f

# 特定サービスのログを追跡
docker compose -f docker/docker-compose.web-volc.yml logs -f app

# 最後の100行を表示
docker compose -f docker/docker-compose.web-volc.yml logs --tail=100 app
```

### パフォーマンス最適化

- LLMレイテンシ：低レイテンシモデルを使用（推奨：Alibaba Cloud Bailian `qwen-flash`）
- 音声品質：`volc-server/src/config.ts`でASRのVAD閾値やTTS音声選択を調整
- ツール呼び出しレイテンシ：appとweb間のネットワーク接続を良好に保ち、MQTTレイテンシを低減（同一LANや低レイテンシ環境でのデプロイ推奨）

ローカル開発（非Docker）：

- web：`pnpm dev`
- app：`uv run ...`
- volc-server：`bun run dev`
