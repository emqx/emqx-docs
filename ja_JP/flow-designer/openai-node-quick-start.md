# クイックスタート：OpenAIノードを使ったFlowの作成

このセクションでは、実用的なユースケースを通じて、FlowデザイナーでLLMベースのFlowを素早く作成・テストする方法を説明します。

このデモでは、MQTTトピックからセンサーデータを受信し、LLM（例：OpenAI GPT）を使ってデータを解釈し、その意味を自然言語で要約するワークフローを構築します。生成された要約は新しいトピック `ai/summary` に再パブリッシュされ、下流で利用可能になります。

## シナリオ説明

デバイスが温度と湿度の読み取り値をMQTTトピック `sensors/temp_humid` に報告すると仮定します。各メッセージはJSON形式の生データを含みます。EMQX Flowは以下の処理を行います。

- **データ処理**：デバイスIDとセンサー値を抽出します。
- **LLMベースの処理**：OpenAIモデルを使ってセンサーの読み取り値を要約します。
- **メッセージ再パブリッシュ**：AI生成の要約を新しいトピック `ai/summary` にパブリッシュします。

**サンプルメッセージ：**

```json
{
  "device_id": "device123",
  "temperature": 38.2,
  "humidity": 75,
  "timestamp": 1717568000000
}
```

**期待される出力（AI生成）：**

```
Device device123 reported a temperature of 38.2°C and 75% humidity.
```

## Flowの作成

::: tip 前提条件

有効なOpenAI APIキーを用意してください。

:::

1. **Flows**ページで**Create Flow**ボタンをクリックします。

2. **Messages**ノードを追加します。

   - ソースパネルから**Messages**ノードをドラッグします。
   - トピックを`sensors/temp_humid`に設定します。
   - **保存**をクリックします。

3. **Data Processing**ノードを追加します。

   - **Processing**セクションから**Data Processing**ノードをドラッグします。
   - 以下のマッピングを追加します：
     - `payload.device_id` → エイリアス `device_id`
     - `payload.temperature` → エイリアス `temperature`
     - `payload.humidity` → エイリアス `humidity`
   - **保存**をクリックします。

4. **OpenAI**ノードを追加します。

   - **Processing**セクションから**OpenAI**ノードをドラッグし、Data Processingノードに接続します。
   - ノードを設定します：
     - **Input**：`payload`を入力します。
     - **System Message**：`Generate a short summary of the device’s sensor readings in human-readable format` と入力します。
     - **Model**：`gpt-4o`を選択します。
     - **API Key**：OpenAI APIキーを入力します。
     - **Base URL**：空欄のままにします。
     - **Output Result Alias**：`summary`と入力します。
   - **保存**をクリックします。

5. **Republish**ノードを追加します。

   - **Sink**セクションから**Republish**ノードをドラッグし、OpenAIノードに接続します。
   - トピックを`ai/summary`に設定します。
   - ペイロードを`${summary}`に設定します。
   - **保存**をクリックします。

6. すべてのノードを接続し、右上の**保存**をクリックしてFlowを保存します。

   ![openai_node_flow](./assets/openai_node_flow.png)

   Flowとルールは相互運用可能です。ルールページでSQLや関連ルール設定も確認できます。

   ![openai_node_rule_page](./assets/openai_node_rule_page.png)

## Flowのテスト

1. MQTTクライアントをEMQXに接続します。

   Flowを素早くテストするには、ダッシュボードの**診断ツール** → **WebSocketクライアント**を使ってMQTTクライアントをシミュレートできます。あるいは、[MQTTX](https://mqttx.app/)などのツールや実際のMQTTクライアントを使用しても構いません。

   - EMQXサーバーに接続します。
   - トピック`ai/summary`をサブスクライブします。

2. テストを開始します。

   - Flowデザイナーで任意のノードをクリックして編集パネルを開きます。
   - **編集**をクリックし、続いて**テスト開始**をクリックして画面下部にテストパネルを表示します。
   - **シミュレートデータ入力**をクリックし、以下のメッセージをトピック`sensors/temp_humid`にパブリッシュするために**テスト送信**をクリックします。

     ```json
     {
       "device_id": "device123",
       "temperature": 38.2,
       "humidity": 75
     }
     ```

3. 結果を確認します。

   - Flowの実行結果が成功したことを確認できます。

     ![openai_node_test_result](./assets/openai_node_test_result.png)

   - **WebSocketクライアント**ページに戻ると、以下のようなAI生成の要約メッセージが受信できます。

     > “The sensor readings from device "device123" indicate that the current temperature is 38.2°C and the humidity level is 75%.”

   - テストが失敗した場合は、エラーメッセージが表示されます。

   - **OpenAI**ノードの実行統計やメトリクスを確認するには、ノードをクリックして編集パネルを開き、**概要**タブをクリックしてください。

     ![openai_node_statistics](./assets/openai_node_statistics.png)
