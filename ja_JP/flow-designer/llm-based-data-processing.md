# LLMベースのMQTTデータ処理

EMQX 5.10.0以降、FlowデザイナーはOpenAI GPT、Anthropic Claude、Google Geminiなどの大規模言語モデル（LLM）との統合をサポートしています。この機能により、ユーザーはログの要約、センサーデータの分類、MQTTメッセージの拡充、リアルタイムインサイトの生成など、自然言語プロンプトを用いたインテリジェントなメッセージフローを構築できます。

## 機能概要

FlowデザイナーのLLMベース処理ノードは、外部のLLM APIに接続してメッセージ内容を処理するAI搭載コンポーネントです。これらのノードを使うことで、MQTTデータを`gpt-4o`や`claude-3-sonnet`などのモデルに送信し、応答を受け取り、フロー内の下流に渡すことが可能です。

::: tip 注意

LLMの呼び出しとデータ処理には時間がかかります。モデルの応答速度によっては数秒から10秒以上かかる場合があります。そのため、LLM処理ノードは高メッセージスループット（TPS）が求められるシナリオには適していません。

:::

### 主要な概念

- **LLMプロバイダー**：AIサービス（OpenAI、Anthropic、Gemini）の名前付き設定。
- **Completion Profile**：LLMモデルパラメータ（モデルID、システムプロンプト、トークン制限など）の再利用可能なバンドル。
- **AI Completion Node**：入力をLLMに送信し、その結果をユーザー定義のエイリアスとして保存するフローコンポーネント。
- `ai_completion`：テキストやバイナリデータをLLMに送信し、応答を返すRule SQL関数。

### 動作の仕組み

FlowデザイナーでMQTTメッセージを受信すると、AIコンプリーションノードは内部的に組み込みのSQL関数`ai_completion/2,3`を呼び出して、設定されたLLMにデータを送信します。

```mermaid
graph LR
  A[MQTTメッセージ] --> B[Flowデザイナーノード]
  B --> C["Rule SQL (ai_completion)"]
  C --> D[LLM API]
  D --> E[LLM応答]
  E --> F[下流ノード]
```

1. メッセージは**Messages**ノード（例：トピックのサブスクライブ）を通じてフローに入ります。
2. **データ処理**ノード（任意）で`device_id`、`payload`、`timestamp`などのフィールドを抽出または変換できます。
3. **AI Completion Node**（OpenAI、Anthropic、Gemini）は背後で`ai_completion`関数を使い、以下を実行します：

     - 選択された**Completion Profile**（プロバイダー情報、モデル名、システムメッセージ、その他パラメータ）を参照。
     - 選択した入力（例：`payload`）をLLMに送信。
     - LLM APIからの応答（例：要約や分類結果）を受信。
4. 応答は**Output Result Alias**に格納され、以下のような下流ノードで利用可能になります：

     - **Republish**（別トピックへのパブリッシュ）

     - **Database**（PostgreSQL、MongoDBなどへの結果挿入）

     - **Bridge**（リモートブローカーやクラウドサービスへの転送）

### 対応LLMプロバイダー

EMQX 5.10.0は以下のプロバイダーをサポートしています：

- **OpenAI**：GPT-4.1、o4-miniなど
- **Anthropic**：claude-3-5-haiku、claude-3-7-sonnet、claude-sonnet-4など
- **Gemini**：gemini-2.0-flash、gemini-2.5-flash、gemini-2.5-proなど

::: tip 互換性について

公式にリストされたプロバイダーに加え、EMQXはOpenAIプラットフォームとAPI互換性のある任意のLLMサービスもサポートしています。

:::

## LLMベース処理ノードの設定

FlowデザイナーでLLMを使用するには、OpenAIまたはAnthropicのいずれかの専用処理ノードを設定する必要があります。各ノードでは、MQTTメッセージデータをLLMに送信する方法（入力フィールドの指定、システムプロンプトによるモデルの振る舞い設定、AI生成結果の保存先など）を定義できます。設定後は、これらのノードが背後で`ai_completion`関数を呼び出し、選択したLLMを使ってデータ処理を行います。

### OpenAIノードの設定

OpenAIノードを使用するには：

1. **Processing**パネルから**OpenAI**ノードをドラッグします。

2. ソースまたは前処理ノードに接続します。

3. 以下の項目を設定します：

   - **Input**：入力フィールドをタイプまたは選択します。選択肢は`event`、`id`、`clientid`、`username`、`payload`などです。

   - **System Message**：AIモデルに期待される出力を生成させるためのプロンプトメッセージを入力します。例：「入力JSONデータの数値キーの値を合計し、その結果のみを出力してください」。

   - **Model**：LLMプロバイダーを選択します。例：`gpt-4o`、`gpt-3.5-turbo`。

   - **API Key**：OpenAIのAPIキーを入力します。

   - **Base URL**：任意のカスタムエンドポイントを入力します。空欄の場合はOpenAIのデフォルトエンドポイントを使用します。

     ::: tip

     このフィールドにプロバイダーのAPIベースURLとAPIキーを入力することで、OpenAI互換の他サービスに接続可能です。

     :::

   - **Output Result Alias**：LLM出力を格納する変数名です。アクションや後続処理で結果を参照するために使用します。例：`summary`。
   
     ::: tip
   
     エイリアスに英数字とアンダースコア以外の文字が含まれる場合、数字で始まる場合、またはSQLキーワードの場合は、エイリアスをダブルクォーテーションで囲んでください。
   
     :::


4. **保存**をクリックして設定を適用します。

### Anthropicノードの設定

Anthropicノードを使用するには：

1. **Processing**パネルから**Anthropic**ノードをドラッグします。

2. メッセージ入力またはデータ処理ノードに接続します。

3. 以下の項目を入力します：

   - **Input**：入力フィールドをタイプまたは選択します。選択肢は`event`、`id`、`clientid`、`username`、`payload`などです。

   - **System Message**：AIモデルに期待される出力を生成させるためのプロンプトメッセージを入力します。例：「入力JSONデータの数値キーの値を合計し、その結果のみを出力してください」。

   - **Model**：LLMプロバイダーを選択します。例：`claude-3-sonnet-20240620`。

   - **Max Tokens**：応答の長さを制御します（デフォルト：`100`）。

   - **Anthropic Version**：Anthropicのバージョンを選択します（デフォルト：`2023-06-01`）。

   - **API Key**：AnthropicのAPIキーを入力します。

   - **Base URL**：任意のカスタムエンドポイントを入力します。空欄の場合はAnthropicのデフォルトエンドポイントを使用します。

   - **Output Result Alias**：LLM出力を格納する変数名です。アクションや後続処理で結果を参照するために使用します。例：`summary`。

     ::: tip

     エイリアスに英数字とアンダースコア以外の文字が含まれる場合、数字で始まる場合、またはSQLキーワードの場合は、エイリアスをダブルクォーテーションで囲んでください。

     :::


4. **保存**をクリックして設定を適用します。

### Geminiノードの設定

Geminiノードを使用するには：

1. **Processing**パネルから**Gemini**ノードをドラッグします。

2. ソースまたは前処理ノードに接続します。

3. 以下の項目を設定します：

   - **Input**：ソースフィールドを入力または選択します。選択肢は`event`、`id`、`clientid`、`username`、`payload`などです。

   - **System Message**：AIモデルに期待される出力を生成させるためのプロンプトメッセージを入力します。例：「入力JSONデータの数値キーの値を合計し、その結果のみを出力してください」。

   - **Model**：LLMプロバイダーを選択します。例：`gemini-2.0-flash`、`gemini-2.5-pro`。

   - **API Key**：GeminiのAPIキーを入力します。

   - **Base URL**：任意のカスタムエンドポイントを入力します。空欄の場合はGeminiのデフォルトエンドポイントを使用します。

   - **Output Result Alias**：LLM出力を格納する変数名です。アクションや後続処理で結果を参照するために使用します。例：`summary`。

     ::: tip

     エイリアスに英数字とアンダースコア以外の文字が含まれる場合、数字で始まる場合、またはSQLキーワードの場合は、エイリアスをダブルクォーテーションで囲んでください。

     :::


4. **保存**をクリックして設定を適用します。

## クイックスタート

以下の2つの例は、EMQXでLLMベース処理ノードを使ったフローの迅速な構築とテスト方法を示しています：

- [OpenAIノードを使ったフロー作成](./openai-node-quick-start.md)：GPTモデルを使ってMQTTメッセージを要約または変換します。
- [Anthropicノードを使ったフロー作成](./anthropic-node-quick-start.md)：Claudeモデルを使ってMQTTメッセージ内の数値を処理します。
- [Geminiノードを使ったフロー作成](./gemini-node-quick-start.md)：Geminiモデルを使ってMQTTメッセージのプロンプトに基づくコンテキスト応答を生成し、MQTTクライアントIDを用いてクライアントごとのトピックにルーティングします。

## 詳細情報

LLM搭載のMQTTデータ処理機能の詳細は、ブログ記事をご覧ください：[Real-Time AI for IoT: Introducing LLM Integration in EMQX 5.10](https://www.emqx.com/en/blog/introducing-llm-integration-in-emqx-5-10)。
