# クイックスタート：Anthropicノードを使ったFlowの作成

このページでは、Claude 3 Sonnetを使用してフォールト分類を行い、受信したテレメトリに基づいて是正推奨を生成する方法を示します。これは、スマートファクトリーやスマートビルディングなどのIoTシステムがデバイスからのステータスメッセージを受信し、それらの問題を自動かつインテリジェントに解釈する実際のシナリオをシミュレートしています。

## シナリオの説明

多くの産業用またはスマートビルディングのシナリオでは、IoTデバイスが単一のMQTTメッセージで複数のメトリクスを報告します。例えば、電力監視デバイスは、複数の回路にわたる電力消費を1つのペイロードで送信することがあります。

各メッセージはトピック `devices/power_report` にパブリッシュされ、以下を含みます：

- `device_id`：デバイスの識別子
- `circuit_1`、`circuit_2`、`circuit_3` などの複数の数値メトリクス
- `status` や `timestamp` のような数値以外の追加フィールド

このメッセージ内のすべての数値の合計（すなわち回路全体の総電力消費量）をLLM（Claude 3 Sonnet）で計算し、数値の合計のみを下流処理や課金のために再パブリッシュすることが目的です。

## サンプルメッセージ

```json
{
  "device_id": "pmu-1008",
  "circuit_1": 120.5,
  "circuit_2": 98.7,
  "circuit_3": 103.2,
  "status": "nominal",
  "timestamp": "2025-06-06T10:00:00Z"
}
```

## 期待される出力（Claudeから）

```
322.4
```

この値は、すべての数値回路読み取り値の合計です。

## Flowの作成

::: tip 前提条件

有効な**Anthropic APIキー**を用意し、正しいAPIバージョン（例：`2023-06-01`）を設定してください。

:::

1. **Flows**ページで**Create Flow**ボタンをクリックします。

2. **Messages**ノードを追加します。

   - ソースパネルから**Messages**ノードをドラッグします。
   - トピックを`devices/power_report`に設定します。
   - **Save**をクリックします。

3. **Anthropic**ノードを追加します。

   - 処理セクションから**Anthropic**ノードをドラッグし、データ処理ノードに接続します。
   - ノードを設定します：
     - **Input**：`payload`を入力します。
     - **System Message**：以下のような動的プロンプトを入力できます。  
       
       ```
       あなたは電力消費計算機です。様々なキーを持つ入力JSONオブジェクトが与えられた場合、すべての数値（例：回路の読み取り値）を合計し、合計値のみを返してください。
       ```
     - **Model**：`claude-3-sonnet-20240620`を選択します。
     - **Max Tokens**：`50`を入力します。
     - **Anthropic Version**：`2023-06-01`を入力します。
     - **API Key**：AnthropicのAPIキーを入力します。
     - **Base URL**：空欄のままにします。
     - **Output Result Alias**：`total_power`を入力します。
   - **Save**をクリックします。

4. **Republish**ノードを追加します。

   - シンクセクションから**Republish**ノードをドラッグし、Anthropicノードに接続します。
   - トピックを`devices/power_total`に設定します。
   - ペイロードを`${total_power}`に設定します。
   - **Save**をクリックします。

5. 右上の**Save**をクリックしてFlowを保存します。

   ![anthropic_node_flow](./assets/anthropic_node_flow.png)

6. Flowとフォームルールは相互運用可能です。ルールページでSQLや関連ルール設定も確認できます。

   ![anthropic_node_rule_page](./assets/anthropic_node_rule_page.png)

## Flowのテスト

1. MQTTクライアントをEMQXに接続します。

   Flowを素早くテストするには、ダッシュボードの**Diagnostic Tools** -> **WebSocket Client**を使ってMQTTクライアントをシミュレートできます。または、[MQTTX](https://mqttx.app/)ツールや実際のMQTTクライアントを使用することも可能です：

   - EMQXサーバーに接続します。
   - トピック`devices/power_total`をサブスクライブします。

2. テストを開始します。

   - Flowデザイナーで任意のノードをクリックし、編集パネルを開きます。

   - **Edit**をクリックし、続けて**Start Test**をクリックして画面下部にテストパネルを開きます。

   - **Input Simulated Data**をクリックし、以下のメッセージをトピック`devices/power_report`にパブリッシュするために**Submit Test**をクリックします：

     ```json
     {
       "device_id": "pmu-1008",
       "circuit_1": 120.5,
       "circuit_2": 98.7,
       "circuit_3": 103.2,
       "status": "nominal",
       "timestamp": "2025-06-06T10:00:00Z"
     }
     ```

3. 結果とノードの処理メトリクスを確認します。

   - Flowの正常な実行結果を確認できます。

     ![anthropic_node_test_result](./assets/anthropic_node_test_result.png)

   - **WebSocket Client**ページに戻ると、AIが生成した要約（例：  
     
     > 322.4  
     
     ）を受信できます。

   - テスト結果が失敗した場合は、エラーメッセージが表示されます。

   - **Anthropic**ノードの稼働状況やメトリクスを確認するには、編集ページを終了し、ノードをクリックして編集パネルを開き、**Overview**タブをクリックしてください。

     ![anthropic_node_statis](./assets/anthropic_node_statistics.png)
