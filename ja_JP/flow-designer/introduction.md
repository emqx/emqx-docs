# Flowデザイナー

Flowデザイナーは、従来のビジュアルツールであるFlowsの機能を拡張し、データ処理ワークフロー（Flows）の作成および編集機能を追加した強力なビジュアルツールです。この拡張により、データ処理および統合の設定がより簡単かつ効率的になります。EMQX v5.8.0以降では、作成したデータ処理ワークフローのテストも可能です。

Data IntegrationとFlowデザイナーで作成されたルールは相互運用可能です。つまり、Flowデザイナーでルールを作成し、そのSQLや関連設定をData Integrationで確認したり、Data IntegrationのSQLエディターでルールを作成し、そのデータフロー処理設定をFlowデザイナーで確認したりできます。

![flow-designer](./assets/flow-designer.png)

## 主な機能

EMQXダッシュボードの左メニューから **Integrations** -> **Flow Designer** をクリックするとFlowsページにアクセスできます。すでにルールやデータ統合が作成されている場合は、複数のノードで構成された有向非巡回グラフが表示されます。各ノードは、トピックやイベント、[Source](../data-integration/data-bridges.md#source)からのデータ読み込み、ルールによるデータ変換、アクションや[Sinks](../data-integration/data-bridges.md#source)を使ったデータ転送などのデータ処理ステップを表します。

Flowsページには、Rules、Webhook、Flowデザイナーで作成されたすべてのデータ処理ワークフローが表示されます。Flowsを通じて、デバイスやクライアントからルール処理を経て外部データシステムへ、またはその逆に外部データシステムからルール処理を経てデバイスへと流れるデータの流れを可視化できます。ページを更新すると、ルールやデータ統合の最新の変更が反映されます。

**Create Flow** ボタンをクリックすると、Flow作成ページに入り、ビジュアルでの設定が可能です。必要なノードをドラッグ＆ドロップで選択し、接続してワークフローを実装します。

![drag_node](./assets/drag_node.png)

### Source

データ入力は、メッセージ、イベント、または外部データシステムから流れるメッセージをサポートします。Flowには最低1つのSourceノードが必要で、複数のデータ入力ノードを同時にサポート可能です。現在サポートされているSourceは以下の通りです。

- **Messages:** クライアントがパブリッシュするメッセージのトピックおよびトピックワイルドカードで指定。
- **Event:** EMQX内のすべてのクライアントイベントをサポート。詳細は[Client Events](../data-integration/rule-sql-events-and-fields.md#mqtt-events)を参照。
- **外部データシステム**:
  - [MQTTサービス](../data-integration/data-bridge-mqtt.md)
  - [Kafkaコンシューマー](../data-integration/data-bridge-kafka.md)
  - [GCP PubSubコンシューマー](../data-integration/data-bridge-gcp-pubsub.md)
  - [RabbitMQ](../data-integration/data-bridge-rabbitmq.md)

### Processing

データ処理およびフィルタリングには、関数ノードとフィルタノードを使用します。このステップは任意で、Flowは最大1つの関数ノードと1つのフィルタノードをサポートします。

- **データ処理:** ルールエンジンのすべての[SQL組み込み関数](../data-integration/rule-sql-builtin-functions.md)をサポート。
- **フィルタ:** Sourceから来るデータフィールドの比較フィルタリングをサポート。サポートされる比較演算子は `>, <, <=, >=, <>, !=, =, =~` です。

ビジュアルフォーム編集に加え、ProcessingノードはルールSQL構文を用いた式モードへの切り替えもサポートします。フィルタノードは関数ノードの後にのみ接続可能であり、データはまず処理されてからフィルタリングされます。

### Sink

データソースおよび処理結果を特定のノードや外部データシステムに出力します。Flowには最低1つのSinkノードが必要で、サポートされるSinkノードは以下の通りです。

- **Republish:** ローカルの指定されたMQTTトピックにメッセージをパブリッシュ。
- **Console Output:** デバッグ用にメッセージをログに出力。
- **外部データシステム:** MySQLやKafkaなど40種類以上のデータシステムをサポート。詳細は[Data Integration](../data-integration/data-bridges.md)を参照。

### Flowの編集とテスト

Flowを作成すると、システムがランダムにIDを生成します。ID横の編集アイコンをクリックすると、Flowの名前と説明を変更できます。

Flow内のノードを削除するには、ノードにカーソルを合わせて右上の削除アイコンをクリックします。ノードをクリックすると編集モードに入り、設定内容を変更して保存できます。全体のFlowは右上の **Save** ボタンで保存します。 **Start Test** ボタンをクリックすると、シミュレートデータの入力や実際のクライアントを使ったテストが可能で、正しく実行されるかを検証できます。

## 利点

Flowデザイナーは多機能かつ使いやすいツールで、ユーザーがより効率的にデータ処理と統合を行い、ビジネスのイノベーションを促進し、データ管理の可視性と制御性を向上させます。主な特徴と利点は以下の通りです。

- **直感的なビジュアルインターフェース:** ドラッグ＆ドロップによる簡単な操作でデータ処理ワークフローを作成・調整・カスタマイズでき、プログラミング経験がなくても複雑なデータ統合ロジックを扱えます。
- **高速なリアルタイム処理:** メッセージやイベントのリアルタイム処理ワークフローを数分で構築可能。ビジネスの変化に迅速に対応し、リアルタイムのビジネスニーズを支援します。
- **豊富な統合機能:** 40種類以上のデータシステムとシームレスに統合し、柔軟なデータ接続と交換を実現します。
- **統合管理と監視:** 統一ビューでデータ統合全体を管理でき、各処理ノードの状態やパフォーマンスを把握可能。リアルタイムの監視とトラッキングにより、高い信頼性とデータの整合性を確保します。
- **EMQXのデータ処理能力:** EMQXのルールSQLおよびSink/Source機能を活用し、堅牢なデータ処理性能を継承。UIとSQLエディターを切り替え可能で、SQL編集の柔軟性とより簡単で高速なユーザー体験を両立し、EMQXルールSQLの深い知識なしにビジネスのイノベーションとデータ駆動型意思決定を促進します。

## クイックスタート

このセクションでは、サンプルユースケースを通じてFlowデザイナーでのFlowの迅速な作成とテスト方法を説明します。

高温アラートを処理するデータ処理ワークフローの作成例を示します。このワークフローは、温度・湿度センサーからMQTTトピック経由でデータを受け取り、データのフィルタリングと変換ルールを設定し、温度が40℃を超えた場合にアラートメッセージを新しいトピック `alert` にパブリッシュします。また、ルールの有効性とデータ処理結果をテストで検証する方法も示します。

### シナリオ説明

デバイスに温度・湿度センサーが搭載されており、5秒ごとにMQTTトピック `sensor/temperature` にデータを送信するとします。EMQXルールエンジンはこのデータを以下の手順で処理します。

1. **データフィルタリング:** 温度が40℃を超えるデータのみ処理。
2. **データ変換**:
   - デバイスIDを抽出。
   - 温度情報を抽出。
   - ペイロード内のタイムスタンプを組み込み関数で読みやすい日付形式に変換。
3. **メッセージ再パブリッシュ:** 処理済みデータをアラートメッセージに整形し、新しいトピック `alert` にパブリッシュ。

再パブリッシュされるサンプルデータ:

```json
{
  "device_id": "device123",
  "temperature": 22.5,
  "humidity": 60
}
```

### Flowの作成

1. Flowsページで **Create Flow** ボタンをクリック。

2. **Source** セクションから **Messages** ノードをキャンバスにドラッグし、メッセージソースのトピック（例：`sensor/temperature`）を設定して **Save** をクリック。これはクライアントがパブリッシュするメッセージのソースを指定するステップです。

   ![messages_node](./assets/messages_node.png)

3. **Processing** セクションから **Data Processing** ノードをドラッグし、以下のフィールドをメッセージから抽出するデータ処理ルールを設定します。

   - `payload.device_id` を `device_id` としてエイリアス。
   - `payload.temperature` を `temperature` としてエイリアス。
   - `timestamp` を `format_date` 関数で読みやすい日時形式に変換し、`date` としてエイリアス。
     - `Time Unit`: `millisecond` を選択。
     - `Time Offset`: `+08:00` を入力。
     - `Data Format`: `%Y-%m-%d %H:%M:%S.%6N%z` を入力。詳細は[日時変換関数](../data-integration/rule-sql-builtin-functions.md#format-date-unit-string-offset-string-integer-formatstring-string-time-integer-string)を参照。
     - `Timestamp`: `timestamp` を入力。

   設定後、**Save** をクリック。

   ![data_processing_node](./assets/data_processing_node.png)

4. **Processing** から **Filter** ノードをドラッグし、データフィルタリングルールを設定します。フィルタ項目を追加し、`payload.temperature` を入力、演算子に `>=` を選択、値に `40` を入力して **Save** をクリック。

   ![filter_rule](./assets/filter_rule.png)

5. **Sink** から **Republish** ノードを選択し、メッセージ転送先のトピックを `alert` に設定。処理・変換済みデータを以下のペイロード形式でアラートメッセージに整形します。

   ```bash
   ${device_id} device reported a high temperature of ${temperature}°C at ${date}.
   ```

   設定後、**Save** をクリック。

   ![republish_node](./assets/republish_node.png)

6. 新しく作成されたFlowがページに表示されます。右上の **Save** をクリックしてFlowを保存します。

   ![flow_created](./assets/flow_created.png)
   
   Flowとフォームルールは相互運用可能です。以前に作成したルールのSQLや関連設定もルールページで確認できます。
   
   ![rule_in_sql_editor](./assets/rule_in_sql_editor.png)

### Flowのテスト

1. Flowデザイナーで任意のノードをクリックし編集パネルを開きます。パネル下部の **Edit Flow** ボタンをクリック。

2. **Save** ボタン横の **Start Test** をクリックすると下部にポップアップが開きます。

   **Input Simulated Data** をクリックしてシミュレーションデータを入力するか、実際のクライアントでメッセージをパブリッシュして結果を確認できます。このデモでは[MQTTX](https://mqttx.app)を使って実データをパブリッシュします。

   ![start_test](./assets/start_test.png)

3. [MQTTX Web](https://mqttx.app/web-client#/recent_connections)を開き、**New Connection** をクリックしてパブリッシャークライアント接続を作成します。以下の項目を設定します。

   - **Name**: `device1` と入力。
   - **Host**: EMQXサーバーの接続アドレスを入力。
   - **Port**: `8084` を入力。
   - **Username** と **Password**: **Access Control** -> **Authentication** ページで設定した認証情報を入力。

   その他の設定はデフォルトのままにして **Connect** をクリック。

4. 新しいサブスクリプションを作成し、トピックを `alert` に設定。

5. 温度が40℃未満のメッセージをパブリッシュすると、条件を満たさずルールSQLは実行されません。

   ![message_publish_1](./assets/message_publish_1.png)

6. 温度が40℃を超えるメッセージをパブリッシュすると、`alert` トピックでアラートメッセージを受信できます。

   ![message_publish_2](./assets/message_publish_2.png)

7. テストページに戻り、テスト成功の結果を確認します。

   ![test_success](./assets/test_success.png)

   テストが失敗した場合は、エラーメッセージが表示されます。

   ![test_fail](./assets/test_fail.png)
