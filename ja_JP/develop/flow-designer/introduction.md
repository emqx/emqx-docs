# Flowデザイナー

Flowデザイナーは、従来のビジュアルツールであるFlowsの機能を拡張し、データ処理ワークフロー（Flows）の作成および編集機能を追加した強力なビジュアルツールです。この拡張により、データ処理および統合の設定が簡素化され、効率化されます。EMQX v5.8.0以降では、作成したデータ処理ワークフローのテストも可能です。

Data IntegrationとFlowデザイナーで作成したルールは相互運用可能です。つまり、Flowデザイナーでルールを作成すると、そのSQLや関連設定をData Integrationで確認でき、逆にData IntegrationのSQLエディターでルールを作成すると、そのデータフロー処理設定をFlowデザイナーで閲覧できます。

![flow-designer](./assets/flow-designer.png)

## 主な機能

EMQXダッシュボードのFlowsページにアクセスするには、左メニューの**Integrations** -> **Flow Designer**をクリックします。すでにルールやデータ統合を作成している場合は、複数のノードで構成された有向非巡回グラフが表示されます。各ノードは、トピックやイベント、[Source](../data-integration/data-bridges.md#source)からのデータ読み取り、ルールによるデータ変換、アクションや[Sinks](../data-integration/data-bridges.md#source)を使ったデータ転送などのデータ処理ステップを表します。

Flowsページは、Rules、Webhook、Flowデザイナーで作成されたすべてのデータ処理ワークフローを表示します。Flowsを通じて、デバイスやクライアントからルール処理を経て外部データシステムへ、またはその逆に外部データシステムからルール処理を経てデバイスへとデータがどのように流れるかを可視化できます。ページを更新すると、ルールやデータ統合の最新変更が反映されます。

**Create Flow**ボタンをクリックすると、Flow作成ページに入り、ビジュアル設定が可能です。各ステップに必要なノードをドラッグ＆ドロップで選択し、接続してワークフローを構築します。

![drag_node](./assets/drag_node.png)

### Source

データ入力はメッセージ、イベント、または外部データシステムから流れるメッセージをサポートします。Flowには少なくとも1つのSourceノードが必要で、複数のデータ入力ノードを同時にサポート可能です。現在サポートされているSourceは以下の通りです：

- **Messages:** クライアントがパブリッシュしたメッセージのトピックおよびトピックワイルドカードで指定。
- **Event:** EMQX内のすべてのクライアントイベントをサポート。詳細は[Client Events](../data-integration/rule-sql-events-and-fields.md#mqtt-events)を参照。
- **外部データシステム**:
  - [MQTT Service](../data-integration/data-bridge-mqtt.md)
  - [Kafka Consumer](../data-integration/data-bridge-kafka.md)
  - [GCP PubSub Consumer](../data-integration/data-bridge-gcp-pubsub.md)
  - [RabbitMQ](../data-integration/data-bridge-rabbitmq.md)

### Processing

データ処理およびフィルタリングには、関数ノードとフィルタノードを使用します。このステップは任意であり、Flowは最大で1つの関数ノードと1つのフィルタノードをサポートします：

- **データ処理:** ルールエンジンのすべての[SQL組み込み関数](../data-integration/rule-sql-builtin-functions.md)をサポート。
- **フィルタ:** Sourceからのデータフィールドに対する比較フィルタリングをサポート。サポートされる比較演算子は`>, <, <=, >=, <>, !=, =, =~`です。

ビジュアルフォーム編集に加え、ProcessingノードはRule SQL構文で編集可能な式モードへの切り替えもサポートします。フィルタノードは関数の後にのみ接続可能であり、データはまず処理されてからフィルタリングされます。

### Sink

データソースおよび処理結果を特定のノードや外部データシステムに出力します。Flowには少なくとも1つのSinkノードが必要で、サポートされているSinkノードは以下の通りです：

- **Republish:** ローカルで指定したMQTTトピックにメッセージをパブリッシュ。
- **Console Output:** デバッグ用にメッセージをログに出力。
- **外部データシステム:** MySQLやKafkaなど40種類以上のデータシステムをサポート。詳細は[Data Integration](../data-integration/data-bridges.md)を参照。

### Flowの編集とテスト

Flow作成時にシステムがランダムにIDを生成します。ID横の編集アイコンをクリックすると、Flowの名前と説明を変更できます。

Flow内のノードを削除するには、ノードにカーソルを合わせて右上の削除アイコンをクリックします。ノードをクリックすると編集モードに入り、設定内容を変更して保存可能です。全体のFlowは**Save**をクリックして保存します。**Start Test**ボタンをクリックすると、シミュレートデータの入力や実際のクライアントを使ったFlowの動作確認ができます。

## 利点

Flowデザイナーは、多機能で使いやすいツールとして、ユーザーがより効率的にデータ処理と統合を行い、ビジネスの革新を促進し、データ管理の可視性と制御性を向上させることを支援します。主な特徴と利点は以下の通りです：

- **直感的なビジュアルインターフェース:** ユーザーはドラッグ＆ドロップの簡単な操作でデータ処理ワークフローを作成、調整、カスタマイズでき、プログラミング経験がなくても複雑なデータ統合ロジックを扱えます。
- **高速なリアルタイム処理:** Flowデザイナーにより、メッセージやイベントのリアルタイム処理ワークフローを数分で構築可能。これにより、ビジネスは新たなデータやイベントに迅速に対応でき、リアルタイムのビジネスニーズを支援します。
- **幅広い統合機能:** 40種類以上のデータシステムとシームレスに統合し、柔軟なデータ接続と交換を実現します。
- **統合管理と監視:** ユーザーは統一ビューでデータ統合全体を明確に管理でき、各処理ノードの状態やパフォーマンスを把握可能。これにより、データフローのリアルタイム監視と追跡が可能となり、高い信頼性とデータの完全性を確保します。
- **EMQXのデータ処理能力:** EMQXのルールSQLおよびSink/Source機能を活用し、堅牢なデータ処理とパフォーマンスの利点を継承。UIとSQLエディターの切り替えが可能で、SQL編集の柔軟性とより簡単で高速なユーザー体験を両立し、EMQXルールSQL構文の深い知識がなくてもビジネスの革新とデータ駆動型の意思決定を促進します。

## クイックスタート

このセクションでは、サンプルユースケースを通じてFlowデザイナーでのFlowの迅速な作成とテスト方法を説明します。

このデモでは、高温アラートを処理するデータ処理ワークフローの作成方法を示します。ワークフローは、温度・湿度センサーからMQTTトピック経由でデータを受信し、データのフィルタリングと変換ルールを設定し、温度が40℃を超えた場合にアラートメッセージを新しいトピック`alert`にパブリッシュします。また、ルールの有効性とデータ処理結果をテストで検証する方法も示します。

### シナリオ説明

デバイスに温度・湿度センサーがあり、5秒ごとにMQTTトピック`sensor/temperature`にデータを送信すると仮定します。EMQXルールエンジンは以下のステップでこのデータを処理します：

1. **データフィルタリング:** 温度が40℃を超えるデータのみ処理。
2. **データ変換**:
   - デバイスIDを抽出。
   - 温度情報を抽出。
   - ペイロード内のタイムスタンプを組み込み関数で読みやすい日付形式に変換。
3. **メッセージ再パブリッシュ:** 処理済みデータをアラートメッセージにフォーマットし、新しいトピック`alert`にパブリッシュ。

再パブリッシュされるサンプルデータ：

```json
{
  "device_id": "device123",
  "temperature": 22.5,
  "humidity": 60
}
```

### Flowの作成

1. Flowsページで**Create Flow**ボタンをクリック。

2. **Source**セクションから**Messages**ノードをキャンバスにドラッグし、メッセージソースのトピック（例：`sensor/temperature`）を設定して**Save**をクリック。このステップはクライアントがパブリッシュするメッセージのソースを指定します。

   ![messages_node](./assets/messages_node.png)

3. **Processing**セクションから**Data Processing**ノードをドラッグし、メッセージから以下のフィールドを抽出するデータ処理ルールを設定：

   - `payload.device_id`: `device_id`としてエイリアス。
   - `payload.temperature`: `temperature`としてエイリアス。
   - `timestamp`: `format_date`関数を使い、メッセージのタイムスタンプを読みやすい日時形式に変換し、`date`としてエイリアス。
     - `Time Unit`: `millisecond`を選択。
     - `Time Offset`: `+08:00`を入力。
     - `Data Format`: `%Y-%m-%d %H:%M:%S.%6N%z`を入力。詳細は[日時変換関数](../data-integration/rule-sql-builtin-functions.md#format-date-unit-string-offset-string-integer-formatstring-string-time-integer-string)を参照。
     - `Timestamp`: `timestamp`を入力。

   設定完了後、**Save**をクリック。

   ![data_processing_node](./assets/data_processing_node.png)

4. **Processing**から**Filter**ノードをドラッグし、データフィルタリングルールを設定。フィルタ項目を追加し、`payload.temperature`を入力、演算子`>=`を選択、値`40`を入力して**Save**をクリック。

   ![filter_rule](./assets/filter_rule.png)

5. **Sink**から**Republish**ノードを選択し、メッセージ転送先のトピックを`alert`に設定。処理・変換済みデータを以下のペイロード形式でアラートメッセージにフォーマット：

   ```bash
   ${device_id} device reported a high temperature of ${temperature}°C at ${date}.
   ```

   **Save**をクリック。

   ![republish_node](./assets/republish_node.png)

6. ページに新しく作成されたFlowが表示されます。右上の**Save**をクリックしてFlowを保存します。

   ![flow_created](./assets/flow_created.png)

Flowsとフォームルールは相互運用可能であり、ルールページで以前作成したルールのSQLや関連設定も確認できます。

   ![rule_in_sql_editor](./assets/rule_in_sql_editor.png)

### Flowのテスト

1. FlowデザイナーでFlow内の任意のノードをクリックし、編集パネルを開きます。パネル下部の**Edit Flow**ボタンをクリック。

2. **Save**ボタン横の**Start Test**をクリックし、下部のポップアップを開きます。

   ポップアップで**Input Simulated Data**をクリックしてシミュレートデータを入力するか、実際のクライアントを使ってメッセージをパブリッシュし結果を確認できます。このデモでは[MQTTX](https://mqttx.app)を使って実データをパブリッシュします。

   ![start_test](./assets/start_test.png)

3. [MQTTX Web](https://mqttx.app/web-client#/recent_connections)を開き、**New Connection**をクリックしてパブリッシャーとしてクライアント接続を作成。以下の項目を設定：

   - **Name**: `device1`を入力。
   - **Host**: EMQXサーバーの接続アドレスを入力。
   - **Port**: `8084`を入力。
   - **Username**および**Password**: **Access Control** -> **Authentication**ページで設定した認証情報を入力。

   他の設定はデフォルトのままにして**Connect**をクリック。

4. 新規サブスクリプションを作成。トピックを`alert`に設定。

5. 40℃未満の温度でメッセージをパブリッシュ。条件を満たさないため、ルールSQLは実行されません。

   ![message_publish_1](./assets/message_publish_1.png)

6. 40℃以上の温度でメッセージをパブリッシュ。`alert`トピックでアラートメッセージを受信します。

   ![message_publish_2](./assets/message_publish_2.png)

7. テストページに戻り、成功したテスト結果を確認。

   ![test_success](./assets/test_success.png)

   テストが失敗した場合は、エラーメッセージが表示されます。

   ![test_fail](./assets/test_fail.png)
