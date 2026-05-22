# クイックスタート：Geminiノードを使ったFlowの作成

このセクションでは、Geminiノードを使った実践的なユースケースを通じて、FlowデザイナーでLLMベースのFlowを素早く作成しテストする方法を説明します。

この例では、構造化されたセンサーデータを含むMQTTデバイスのメッセージを処理しつつ、ルーティングのために`clientid`を保持するFlowの構築方法を示します。Geminiノードはメッセージのペイロードに基づいて返信を生成し、RepublishノードがAIの返信をクライアントごとのトピック`devices/${clientid}/reply`に送信することで、各デバイスにカスタマイズされた返信を届けます。

## シナリオの説明

産業用モニタリングのシナリオでは、各デバイスが定期的にJSON形式の構造化センサーデータをトピック`devices/<device_id>`にパブリッシュします。従来のルールベースのアラート（例：温度の閾値超過）では、隠れたパターンや異常指標の組み合わせを見逃す可能性があります。

このFlowはGeminiを活用し、振動、温度、圧力など複数のフィールドの全体的な文脈を分析し、潜在的な機械故障を示唆する複雑な異常を検出します。例えば、振動と温度が同時に高い場合、Geminiはより深刻なリスク（例：ベアリングの過負荷）を推論し、正確かつ説明可能なアラートを出力します。

- **データ処理**：ペイロードからデバイスの計測値を抽出し、後続で利用できるように`clientid`（例：`device_1`）を公開します。
- **LLMベースの処理**：全フィールドにわたる包括的な解析のためにペイロード全体をGeminiに送信します。
- **メッセージ再パブリッシュ**：AI生成のアラートをクライアントごとのトピック`devices/<district_id>/reply`にパブリッシュします。

**受信メッセージの例（`devices/device_1`宛）：**

```json
{
  "vibration": 9.5,
  "temperature": 85,
  "pressure": 1.2
}
```

**期待される再パブリッシュ出力（`devices/device_1/reply`宛）：**

```
Critical Alert: Simultaneous severe vibration and high temperature detected, indicating an immediate critical equipment malfunction risk.
```

## Flowの作成

::: tip 前提条件

有効なGemini APIキーを用意してください。

:::

1. **Flows**ページで**Create Flow**ボタンをクリックします。

2. **Messages**ノードを追加します。

   - ソースパネルから**Messages**ノードをドラッグします。
   - トピックを`devices/+`に設定します。
   - **Save**をクリックします。

3. **Data Processing**ノードを追加します。

   - **Processing**セクションから**Data Processing**ノードをドラッグします。
   - 以下の設定でフォームを入力します。この設定により、後続ノードで利用可能なように`clientid`を公開します（例：再パブリッシュのトピック内で`${clientid}`を使用可能にします）。
     
     - **Field**: `clientid`
     - **Transform**: 空欄のまま
     - **Alias**: `clientid`
   - **Save**をクリックします。
   
4. **Gemini**ノードを追加します。

   - **Processing**セクションから**Gemini**ノードをドラッグします。

   - ノードを設定します：

     - **Input**: `payload`と入力します。

     - **System Message**: 以下のプロンプトを入力します：

       ```
       You are an industrial anomaly detection assistant.
       Analyze the incoming sensor data (vibration, temperature, pressure) as a whole.
       If multiple indicators exceed risk thresholds at the same time, for example, if vibration > 8 and temperature > 80 in the same reading, the combined risk is significantly higher than a single abnormal value. In such cases, generate a precise, high-priority alert.
       Only return a single alert sentence—no extra explanation.
       ```
       
     - **Model**: デフォルトの`gemini-2.0-flash`のままで構いません。

     - **API Key**: GeminiのAPIキーを入力します。

     - **Base URL**: 空欄のままにしてGeminiのデフォルトエンドポイントを使用します。

     - **Output Result Alias**: `ai_reply`と入力します。

   - **Save**をクリックします。

5. **Republish**ノードを追加します。

   - **Sink**セクションから**Republish**ノードをドラッグします。
   - トピックを`devices/${clientid}/reply`に設定します。
   - ペイロードを`${ai_reply}`に設定します。
   - **Save**をクリックします。

6. すべてのノードを接続し、右上の**Save**をクリックしてFlowを保存します。

   ![openai_node_flow](./assets/gemini_node_flow.png)

   Flowとフォームルールは連携可能です。ルールページでSQLや関連ルール設定を確認できます。

   ![openai_node_rule_page](./assets/gemini_node_rule_page.png)

## Flowのテスト

1. MQTTクライアントをEMQXに接続します。

   Flowを素早くテストするには、ダッシュボードの**Diagnostic Tools** → **WebSocket Client**を使ってMQTTクライアントをシミュレートできます。あるいは、[MQTTX](https://mqttx.app/)などのツールや実際のMQTTクライアントも利用可能です：

   - EMQXサーバーに接続します。
   - 例えば`devices/device_1/reply`トピックをサブスクライブします。

2. テストを開始します。

   - Flowデザイナーで任意のノードをクリックし、編集パネルを開きます。

   - **Edit**をクリックし、続けて**Start Test**をクリックすると、画面下部にテストパネルが開きます。

   - **Input Simulated Data**をクリックし、以下のメッセージをトピック`devices/device_1`にパブリッシュするため**Submit Test**をクリックします：

     ```json
     {
       "vibration": 9.5,
       "temperature": 85,
       "pressure": 1.2
     }
     ```
   
3. 結果を確認します。

   - Flowの実行結果が成功したことを確認できます。

     ![openai_node_test_result](./assets/gemini_node_test_result.png)

   - **WebSocket Client**ページに戻ると、以下のようなAI生成の要約メッセージを受信できます：

     > 「High-priority alert: Simultaneous high vibration and high temperature detected.」

   - テストが失敗した場合は、エラーメッセージが表示されます。

   - **Gemini**ノードの稼働状況やメトリクスを確認するには、編集ページを閉じ、ノードをクリックして編集パネルを開き、**Overview**タブをクリックします。

     ![openai_node_statistics](./assets/gemini_node_statistics.png)
