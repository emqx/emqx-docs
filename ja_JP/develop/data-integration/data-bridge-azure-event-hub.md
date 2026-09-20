# Stream MQTT Data into Azure Event Hubs

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、データ取り込みのためのリアルタイム管理イベントストリーミングプラットフォームです。EMQXのAzure Event Hubとの統合により、ユーザーは高スループット環境での信頼性の高いデータ転送および処理機能を利用できます。Azure Event Hubsは、EMQXとAzureの豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoTデータをAzure Blob Storage、Azure Stream Analytics、およびAzure仮想マシン上に展開された各種アプリケーションやサービスに統合できます。現在、EMQXはSASL/PLAIN認証およびApache Kafkaプロトコル互換のエンドポイントを通じてAzure Event Hubとの統合をサポートしています。

本ページでは、EMQXとAzure Event Hubs間のデータ統合について包括的に紹介し、ルールおよびSinkの作成と検証に関する実践的な手順を提供します。

## 動作の仕組み

Azure Event Hubsデータ統合は、EMQXの標準機能として提供されており、ユーザーがMQTTデータストリームをAzure Event Hubsとシームレスに統合し、IoTアプリケーション開発における豊富なサービスと機能を活用できるよう支援します。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQXはルールエンジンとSinkを介してMQTTデータをAzure Event Hubsに転送します。全体の流れは以下の通りです。

1. **IoTデバイスがメッセージをパブリッシュ**: デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**: 内蔵のルールエンジンは、特定のソースからのMQTTメッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報の付加などの処理を行います。
3. **Azure Event Hubsへのブリッジング**: ルールはメッセージをAzure Event Hubsに転送するアクションをトリガーし、データプロパティ、オーダーキー、MQTTトピックとAzure Event Hubsヘッダーのマッピングを簡単に設定できます。これにより、データ統合におけるより豊富なコンテキスト情報と順序保証が提供され、柔軟なIoTデータ処理が可能になります。

MQTTメッセージデータがAzure Event Hubsに書き込まれた後、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：強力なAzure Event Hubsのデータ処理・分析ツールおよびストリーミング機能を活用し、メッセージデータのリアルタイム処理と分析を行い、価値ある洞察や意思決定支援を得られます。
- イベント駆動型機能：Azureのイベント処理をトリガーし、動的かつ柔軟な機能の起動と処理を実現します。
- データの保存と共有：メッセージデータをAzure Event Hubsのストレージサービスに送信し、大量データの安全な保存と管理を行います。これにより、他のAzureサービスと連携してデータを共有・分析し、多様なビジネスニーズに対応できます。

## 特徴と利点

EMQXとAzure Event Hubs間のデータ統合は、以下の機能と利点をビジネスにもたらします。

- **高性能な大量メッセージスループット**：EMQXは膨大な数のMQTTクライアント接続をサポートし、毎秒数百万件のメッセージを継続的にAzure Event Hubsに取り込めます。これにより非常に低いメッセージ伝送および保存のレイテンシを実現し、Azure Event Hubsの保持時間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：設定されたAzure Event Hubsを通じて、MQTTトピックとAzure Event Hubsイベントセンター間の柔軟なマッピングが可能です。また、MQTTユーザープロパティをAzure Event Hubsヘッダーにマッピングすることもサポートし、データ統合におけるより豊かなコンテキスト情報と順序保証を提供します。
- **弾力的なスケーラビリティ対応**：EMQXとAzure Event Hubsの両方が弾力的なスケーラビリティをサポートし、アプリケーションの仕様に応じて数MBから数TBまでのIoTデータ規模を容易に拡張できます。
- **豊富なエコシステム**：標準のMQTTプロトコルを採用し、各種主流IoT伝送プロトコルをサポートすることで、EMQXは多様なIoTデバイスとの接続を実現します。さらにAzure Event HubsはAzure Functions、各種プログラミング言語SDK、Kafkaエコシステムをサポートし、デバイスからクラウドまでのIoTデータアクセスと処理をシームレスに促進します。

これらの機能により統合能力と柔軟性が向上し、ユーザーは大量のIoTデバイスデータをAzureに迅速に接続できます。クラウドコンピューティングによるデータ分析・インテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションの構築を支援します。

## はじめる前に

このセクションでは、EMQXダッシュボードでAzure Event Hubデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Azure Event Hubのセットアップ

Azure Event Hubデータ統合を利用するには、AzureアカウントでNamespaceとEvent Hubをセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ方法の詳細があります。

- [クイックスタート：Azureポータルを使用してイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート：Azure Event HubsとApache Kafkaでデータをストリームする](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQXは「Connection String」の手順に従って接続します。
- [Event Hubsの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka用Azure Event Hubsとは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubsデータ統合を作成するには、Azure Event Hubs SinkとAzure Event Hubsを接続するコネクターを作成する必要があります。

1. EMQXダッシュボードで **Integrations** -> **Connectors** を開きます。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせとし、例として `my-azure-event-hubs` などを指定します。
5. 接続情報を設定します。
   - **Bootstrap Host**：Namespaceのホスト名を入力します。デフォルトポートは `9093` です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：Namespaceの共有アクセスポリシーの「Connection string - primary key」から取得した接続文字列を入力します。詳細は[Event Hubsの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**：Azure Event Hubへの接続時はTLSがデフォルトで有効です。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
   - **Request Timeout**：EMQXがAzure Event Hubsからの応答を待つ最大時間（秒）を指定します。デフォルトは `30` 秒です。タイムアウトを超えるとEMQXは接続を古いものとみなし再接続します。値が小さすぎると、Azure Event Hubsはプロデュース要求を受け入れても応答を遅延させることがあり、再接続後にEMQXが同じバッチを再送し、重複メッセージや下流の過剰なデータ量が発生する可能性があります。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。

これで、Azure Event Hubsがコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となります。次に、ルールとSinkを作成し、Azure Event Hubsにストリームするデータを指定します。

## Azure Event Hubs Sinkを追加したルールの作成

このセクションでは、Azure Event Hubs Sinkを追加したルールの作成方法を説明します。

1. EMQXダッシュボードで **Integration** -> **Rules** を開きます。

2. ページ右上の **Create** をクリックします。

3. ルールIDに例として `my_rule` を入力します。

4. MQTTメッセージをトピック `t/#` でAzure Event Hubsに保存したい場合は、**SQL Editor** に以下のステートメントを入力します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択し、ルールで処理されたデータをAzure Event Hubsに送信するようにします。

   **Action** ドロップダウンは `Create Action` のままにするか、既存のAzure Event Hubsアクションを選択できます。この例では新しいSinkを作成し、ルールに追加します。

6. Sinkの名前と説明を **Name** および **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。ドロップダウン横のボタンから新しいコネクターを作成することも可能です。設定パラメーターの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink情報を設定します。
   - **Event Hub Name**：使用するEvent Hubの名前を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートしています。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hubにパブリッシュされるメッセージに追加されるヘッダーとして使用されるプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。`none` または `json` が選択可能です。
   - **Extra Azure Event Hub headers**：**Add** をクリックして、Azure Event Hubsヘッダーの追加のキー・バリューを指定できます。
   - **Message Key**：Event Hubのメッセージキーを指定します。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Message Value**：Event Hubのメッセージ値を指定します。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Partition Strategy**：プロデューサーがメッセージをAzure Event Hubsのパーティションに振り分ける方法を指定します。
     - `random`：メッセージごとにランダムにパーティションを選択します。
     - `key_dispatch`：Azure Event Hubsメッセージキーをハッシュしてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーが送信可能な最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **高度な設定（任意）**：必要に応じて **sync** または **async** クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックしてSinkの設定を完了します。**Create Rule** ページに戻ると、新しいSinkが **Action Outputs** タブに表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを作成します。作成したルールはルール一覧に表示されます。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しいAzure Event Hubs Sinkが確認できます。

また、**Integration** -> **Flow Designer** を開くとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Azure Event Hubsに送信・保存されている様子が見られます。

## ルールのテスト

Azure Event Hubsデータ統合が期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/) を使ってクライアントをシミュレートし、EMQXにMQTTメッセージをパブリッシュします。

1. MQTTXでトピック `t/1` にメッセージを送信します。

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sinkの稼働状況をチェックし、新しい送信メッセージが1件あることを確認してください。
3. Kafka互換のコンシューマーを使い、設定したEvent Hubにメッセージが書き込まれているか確認します。Kafka CLIの使用方法については[Azure Event Hubs for Apache Kafka EcosystemでのKafka CLIによるメッセージ送受信](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 高度な設定

このセクションでは、コネクターおよびSinkのパフォーマンス最適化のための高度なオプションについて説明します。対応するオブジェクト作成時に **Advanced Settings** を展開して設定してください。

### コネクター設定

| 項目                           | 説明                                                                                                                       | 推奨値             |
| ------------------------------ | -------------------------------------------------------------------------------------------------------------------------- | ------------------ |
| Allow Auto Topic Creation       | （プロデューサーのみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際に存在しないKafkaトピックを自動作成します。 | `Disabled`         |
| Connect Timeout                 | TCP接続確立の最大待機時間（認証時間含む）                                                                                   | `5` 秒             |
| Start Timeout                   | 自動起動したリソースが正常状態になるまで待機する最大秒数。SinkがConfluentクラスターなどの接続先リソースの準備完了を確認してから処理を進めるための設定。 | `5` 秒             |
| Health Check Interval           | コネクターの稼働状態をチェックする間隔                                                                                     | `15` 秒            |
| Health Check Timeout            | Azure Event Hubsとの接続に対する自動ヘルスチェックのタイムアウト時間                                                        | `60` 秒            |
| Min Metadata Refresh Interval   | クライアントがAzure Event Hubs Kafkaブローカーおよびトピックのメタデータを更新する際の最小間隔。短すぎるとKafkaサーバー負荷が増加する可能性あり。 | `3` 秒             |
| Metadata Request Timeout        | Kafkaからメタデータを要求する際の最大待機時間                                                                               | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能を最適化するためのソケットバッファサイズ                                                               | `1` MB             |
| No Delay                      | システムカーネルがTCPソケットを即時送信するか遅延送信するかの設定。オンで即時送信（デフォルト40msの遅延がなくなる）。           | `Enabled`          |
| TCP Keepalive                 | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間のアイドルによる接続切断を防止。値は `Idle, Interval, Probes` の3つの数値のカンマ区切りで指定。<br>Idle: アイドル状態が続く秒数（Linuxデフォルト7200秒）<br>Interval: キープアライブプローブ間隔（Linuxデフォルト75秒）<br>Probes: 最大プローブ回数（Linuxデフォルト9回）<br>例：`240,30,5,` は、240秒アイドル後に30秒間隔で5回プローブを送り応答がなければ切断と判断。 | `none`             |

### Azure Event Hubs プロデューサーSink設定

| 項目               | 説明                                                                                                                       | 推奨値             |
| ------------------ | -------------------------------------------------------------------------------------------------------------------------- | ------------------ |
| Max Batch Age      | メッセージがプロデューサーバッファに滞留できる最大時間。超過したバッチは破棄される。切断中のバッファリングや応答待ちメッセージも含む。破棄されたメッセージは `dropped.expired` メトリクスにカウント。デフォルトは `infinity` で期限切れなし。バッファオーバーフロー時は破棄される可能性あり。 | `infinity`         |
| Max Retries        | Azure Event Hubsがリトライ可能なエラー（例：パーティションリーダー変更）で応答した場合の最大リトライ回数。すべて失敗するとバッチは破棄され、各メッセージは `failed` メトリクスにカウント。明示的なAzure Event Hubsエラー応答のみリトライ回数に加算。接続断による再送は加算されず、`max_batch_age` によって制限。デフォルトは無制限の `infinity`。 | `infinity`         |
| Reconnect Delay    | 接続断後にAzure Event Hubsへ再接続を試みるまでの待機時間。切断中もメッセージはバッファに蓄積され、バッファ制限および `max_batch_age` の対象となる。デフォルトは `2` 秒。 | `2` 秒             |
| Max Linger Time    | パーティションごとのプロデューサーがより大きなバッチを作成するためにメッセージを蓄積する最大待機時間。すべてのバッファモードに適用。デフォルトは `0`（待機なし）でメッセージレイテンシ最適化。小さな遅延を許容するとリクエスト数削減可能。バッチが満杯になると早期に送信。ディスクバッファリング時はバッチ書き込み前に待機。ディスクIOPS削減のため最低 `5ms` の設定推奨。 | `0` ミリ秒        |
| Max Linger Bytes   | パーティションごとのプロデューサーが蓄積する最大バイト数。これを超えると待機を止めてバッチを送信。                                           | `10` MB            |
