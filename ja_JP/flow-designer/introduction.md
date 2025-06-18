# Flow Designer

Flow Designer は、従来のビジュアルツールである Flows の機能を拡張し、データ処理ワークフロー（Flows）の作成および編集機能を追加した強力なビジュアルツールです。この強化により、データ処理および統合の設定が簡素化され、効率化されます。EMQX v5.8.0 以降では、作成したデータ処理ワークフローのテストも可能です。

Data Integration と Flow Designer で作成されたルールは相互運用可能です。つまり、Flow Designer でルールを作成すると、Data Integration でその SQL や関連設定を確認でき、逆に Data Integration の SQL エディターでルールを作成すると、Flow Designer でそのデータフロー処理設定を確認できます。

![flow-designer](./assets/flow-designer.png)

## 主な機能

EMQX ダッシュボードの左メニューから **Integrations** -> **Flow Designer** をクリックすると、Flows ページにアクセスできます。すでにルールやデータ統合を作成している場合、複数のノードで構成される有向非巡回グラフが表示されます。各ノードは、トピックやイベント、[Source](../data-integration/data-bridges.md#source) からのデータ読み取り、ルールによるデータ変換、アクションや [Sink](../data-integration/data-bridges.md#source) を使ったデータ転送など、データ処理の各ステップを表しています。

Flows ページでは、Rules、Webhook、Flow Designer を使って作成されたすべてのデータ処理ワークフローが表示されます。Flows を通じて、デバイスやクライアントからルール処理を経て外部データシステムへ、あるいはその逆に外部データシステムからルール処理を経てデバイスへとデータがどのように流れるかを可視化できます。ページを更新すると、ルールやデータ統合の最新の変更が反映されます。

**Create Flow** ボタンをクリックすると、Flow 作成ページに入り、ビジュアルでの設定が可能です。必要なノードをドラッグ＆ドロップで選択し、それらを接続してワークフローを実装します。

![drag_node](./assets/drag_node.png)

### Source

データ入力は、メッセージ、イベント、または外部データシステムから流れるメッセージをサポートします。Flow には少なくとも1つの Source ノードが必要で、複数のデータ入力ノードを同時にサポート可能です。現在サポートされている Source は以下の通りです：

- **Messages:** クライアントがパブリッシュしたメッセージのトピックおよびトピックワイルドカードで指定。
- **Event:** EMQX 内のすべてのクライアントイベントをサポート。詳細は [Client Events](../data-integration/rule-sql-events-and-fields.md#mqtt-events) を参照。
- **外部データシステム**:
  - [MQTT Service](../data-integration/data-bridge-mqtt.md)
  - [Kafka Consumer](../data-integration/data-bridge-kafka.md)
  - [GCP PubSub Consumer](../data-integration/data-bridge-gcp-pubsub.md)
  - [RabbitMQ](../data-integration/data-bridge-rabbitmq.md)

### Processing

データ処理およびフィルタリングには、function ノードと filter ノードを使用します。このステップは任意で、Flow は最大で1つの function ノードと1つの filter ノードをサポートします：

- **データ処理:** ルールエンジンのすべての [SQL 組み込み関数](../data-integration/rule-sql-builtin-functions.md) をサポート。
- **フィルター:** Source からのデータフィールドに対する比較フィルタリングをサポート。サポートされる比較演算子は `>, <, <=, >=, <>, !=, =, =~` です。

ビジュアルフォーム編集に加え、Processing ノードは Rule SQL 構文で編集可能な式モードへの切り替えもサポートします。Filter ノードは function の後にのみ接続可能であり、データはまず処理されてからフィルタリングされます。

### Sink

データソースおよび処理結果を特定のノードや外部データシステムに出力します。Flow には少なくとも1つの Sink ノードが必要で、サポートされている Sink ノードは以下の通りです：

- **Republish:** ローカルで指定した MQTT トピックにメッセージをパブリッシュ。
- **Console Output:** デバッグ用にログにメッセージを出力。
- **外部データシステム:** MySQL や Kafka など40種類以上のデータシステムをサポート。詳細は [Data Integration](../data-integration/data-bridges.md) を参照。

### Flow の編集とテスト

Flow 作成時にシステムがランダムに ID を生成します。ID の横にある編集アイコンをクリックすると、Flow の名前や説明を変更できます。

Flow 内のノードを削除するには、ノードにカーソルを合わせて右上の削除アイコンをクリックします。ノードをクリックすると編集モードに入り、設定内容を変更して保存できます。全体の Flow は **Save** ボタンで保存します。**Start Test** ボタンをクリックすると、シミュレーションデータの入力や実際のクライアントを使ったテストが可能で、正しく実行されるか検証できます。

## 利点

Flow Designer は多機能で使いやすいツールとして、ユーザーがより効率的にデータ処理と統合を行い、ビジネスのイノベーションを促進し、データ管理の可視化と制御を向上させます。主な特徴と利点は以下の通りです：

- **直感的なビジュアルインターフェース:** ドラッグ＆ドロップの簡単な操作でデータ処理ワークフローを作成・調整・カスタマイズでき、プログラミング経験がなくても複雑なデータ統合ロジックを扱えます。
- **高速なリアルタイム処理:** メッセージやイベントのリアルタイム処理ワークフローを数分で構築可能。これにより、ビジネスは新たなデータやイベントに迅速に対応でき、リアルタイムのビジネスニーズを支援します。
- **豊富な統合機能:** 40種類以上のデータシステムとシームレスに統合し、柔軟なデータ接続と交換を実現します。
- **統合管理と監視:** 統一されたビューでデータ統合の全プロセスを明確に管理でき、各処理ノードの状態やパフォーマンスを把握可能。リアルタイムでデータフローを監視・追跡し、高い信頼性とデータの完全性を確保します。
- **EMQX のデータ処理能力:** EMQX のルール SQL と Sink/Source 機能を活用し、堅牢なデータ処理性能を継承。UI と SQL エディターを切り替えられ、SQL 編集の柔軟性とより簡単で高速なユーザー体験を両立。EMQX ルール SQL の深い知識がなくても、ビジネスのイノベーションやデータ駆動の意思決定を促進します。

## クイックスタート

このセクションでは、サンプルユースケースを通じて Flow Designer での Flow の迅速な作成とテスト方法を説明します。

ここでは、高温アラートを処理するデータ処理ワークフローの作成例を示します。このワークフローは、温度・湿度センサーから MQTT トピック経由でデータを受信し、データのフィルタリングと変換ルールを設定し、温度が40℃を超えた場合にアラートメッセージを新しいトピック `alert` にパブリッシュします。また、ルールの有効性とデータ処理結果をテストで検証する方法も示します。

### シナリオ説明

デバイスに温度・湿度センサーが搭載されており、5秒ごとに MQTT トピック `sensor/temperature` にデータを送信すると仮定します。EMQX ルールエンジンは以下のステップでこのデータを処理します：

1. **データフィルタリング:** 温度が40℃を超えるデータのみ処理。
2. **データ変換**:
   - デバイスIDを抽出。
   - 温度情報を抽出。
   - ペイロード内のタイムスタンプを組み込み関数で読みやすい日付形式に変換。
3. **メッセージ再パブリッシュ:** 処理済みデータをアラートメッセージに整形し、新しいトピック `alert` にパブリッシュ。

再パブリッシュされるサンプルデータ：

```json
{
  "device_id": "device123",
  "temperature": 22.5,
  "humidity": 60
}
```

### Flow の作成

1. Flows ページで **Create Flow** ボタンをクリック。

2. **Source** セクションから **Messages** ノードをキャンバスにドラッグし、メッセージのソーストピック（例：`sensor/temperature`）を設定して **Save** をクリック。これはクライアントがパブリッシュするメッセージのソースを指定します。

   ![messages_node](./assets/messages_node.png)

3. **Processing** セクションから **Data Processing** ノードをドラッグし、以下のフィールドをメッセージから抽出するデータ処理ルールを設定：

   - `payload.device_id`: エイリアスを `device_id` に設定。
   - `payload.temperature`: エイリアスを `temperature` に設定。
   - `timestamp`: `format_date` 関数を使ってメッセージのタイムスタンプを読みやすい日時形式に変換し、エイリアスを `date` に設定。
     - `Time Unit`: `millisecond` を選択。
     - `Time Offset`: `+08:00` を入力。
     - `Data Format`: `%Y-%m-%d %H:%M:%S.%6N%z` を入力。詳細は [Date and Time Conversion Functions](../data-integration/rule-sql-builtin-functions.md#format-date-unit-string-offset-string-integer-formatstring-string-time-integer-string) を参照。
     - `Timestamp`: `timestamp` を入力。

   設定完了後、**Save** をクリック。

   ![data_processing_node](./assets/data_processing_node.png)

4. **Processing** から **Filter** ノードをドラッグし、データフィルタリングルールを設定。フィルター項目を追加し、`payload.temperature` を入力、演算子に `>=` を選択、値に `40` を入力して **Save** をクリック。

   ![filter_rule](./assets/filter_rule.png)

5. **Sink** から **Republish** ノードを選択し、メッセージ転送先のトピックを `alert` に設定。処理・変換済みデータを以下のペイロード形式でアラートメッセージに整形：

   ```bash
   ${device_id} device reported a high temperature of ${temperature}°C at ${date}.
   ```

   **Save** をクリック。

   ![republish_node](./assets/republish_node.png)

6. ページに新たに作成された Flow が表示されます。右上の **Save** をクリックして Flow を保存。

   ![flow_created](./assets/flow_created.png)
   
   Flows とフォームルールは相互運用可能で、以前に作成したルールの SQL や関連設定をルールページで確認できます。
   
   ![rule_in_sql_editor](./assets/rule_in_sql_editor.png)

### Flow のテスト

1. Flow Designer で任意のノードをクリックし、編集パネルを開きます。パネル下部の **Edit Flow** ボタンをクリック。

2. **Save** ボタンの隣にある **Start Test** をクリックすると、画面下部にポップアップが表示されます。

   **Input Simulated Data** をクリックしてシミュレーションデータを入力するか、実際のクライアントからメッセージをパブリッシュして結果を確認できます。このデモでは [MQTTX](https://mqttx.app) を使って実データをパブリッシュします。

   ![start_test](./assets/start_test.png)

3. [MQTTX Web](https://mqttx.app/web-client#/recent_connections) を開き、**New Connection** をクリックしてパブリッシャーとしてクライアント接続を作成。以下の項目を設定：

   - **Name**: `device1` と入力。
   - **Host**: EMQX サーバーの接続アドレスを入力。
   - **Port**: `8084` を入力。
   - **Username** と **Password**: **Access Control** -> **Authentication** ページで設定した認証情報を入力。

   他の設定はデフォルトのままにして **Connect** をクリック。

4. 新規サブスクリプションを作成し、トピックを `alert` に設定。

5. 温度が40℃未満のメッセージをパブリッシュすると、条件を満たさないためルール SQL は実行されません。

   ![message_publish_1](./assets/message_publish_1.png)

6. 温度が40℃以上のメッセージをパブリッシュすると、`alert` トピックにアラートメッセージが届きます。

   ![message_publish_2](./assets/message_publish_2.png)

7. テストページに戻ると、テスト成功の結果が表示されます。

   ![test_success](./assets/test_success.png)

   テストが失敗した場合は、エラーメッセージが表示されます。

   ![test_fail](./assets/test_fail.png)
