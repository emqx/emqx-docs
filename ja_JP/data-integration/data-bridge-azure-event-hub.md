# Stream MQTT Data into Azure Event Hubs

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データ取り込みに利用されます。EMQXのAzure Event Hubとの統合により、高スループット環境において信頼性の高いデータ転送および処理機能をユーザーに提供します。Azure Event Hubsは、EMQXとAzureの豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoTデータをAzure Blob Storage、Azure Stream Analytics、Azure仮想マシン上に展開された各種アプリケーションやサービスへ統合できます。現在、EMQXはSASL/PLAIN認証およびApache Kafkaプロトコル互換のエンドポイントを通じてAzure Event Hub統合をサポートしています。

本ページでは、EMQXとAzure Event Hubs間のデータ統合について包括的に紹介し、ルールとSinkの作成および検証手順を実践的に解説します。

## 仕組み

Azure Event Hubsデータ統合は、EMQXの標準機能として提供されており、ユーザーがMQTTデータストリームをAzure Event Hubsとシームレスに統合し、IoTアプリケーション開発における豊富なサービスと機能を活用できるよう支援します。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQXはルールエンジンとSinkを介してMQTTデータをAzure Event Hubsに転送します。全体の流れは以下の通りです：

1. **IoTデバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは特定のトピックにマッチするMQTTメッセージを処理します。ルールにマッチしたメッセージは、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
3. **Azure Event Hubsへブリッジング**：ルールはメッセージをAzure Event Hubsへ転送するアクションをトリガーし、データプロパティや順序キーの設定、MQTTトピックとAzure Event Hubsヘッダーのマッピングを容易に構成できます。これにより、データ統合における豊富なコンテキスト情報と順序保証が実現し、柔軟なIoTデータ処理が可能となります。

MQTTメッセージデータがAzure Event Hubsに書き込まれた後、以下のような柔軟なアプリケーション開発が可能です：

- リアルタイムデータ処理と分析：強力なAzure Event Hubsのデータ処理・分析ツールとストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、価値ある洞察や意思決定支援を得られます。
- イベント駆動型機能：Azureのイベント処理をトリガーし、動的かつ柔軟な関数の起動と処理を実現します。
- データ保存と共有：メッセージデータをAzure Event Hubsのストレージサービスに送信し、大量データの安全な保存・管理を行います。これにより他のAzureサービスとデータを共有・分析し、多様なビジネスニーズに対応可能です。

## 特長と利点

EMQXとAzure Event Hubs間のデータ統合は、以下の機能とメリットをビジネスにもたらします：

- **高性能な大量メッセージスループット**：EMQXは膨大な数のMQTTクライアント接続をサポートし、毎秒数百万件のメッセージをAzure Event Hubsに継続的に取り込みます。これにより極めて低いメッセージ転送および保存のレイテンシを実現し、Azure Event Hubsの保持時間設定でメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：Azure Event Hubsの設定を通じて、MQTTトピックとAzure Event Hubsのイベントセンター間の柔軟なマッピングが可能です。MQTTユーザープロパティをAzure Event Hubsヘッダーにマッピングすることもサポートし、データ統合における豊かなコンテキスト情報と順序保証を提供します。
- **弾力的なスケーリング対応**：EMQXとAzure Event Hubsの両方が弾力的なスケーリングをサポートし、アプリケーション仕様に応じて数MBから数TBまでのIoTデータ規模を容易に拡張可能です。
- **豊富なエコシステム**：標準MQTTプロトコルの採用により、多様な主流IoT伝送プロトコルをサポートし、多種多様なIoTデバイスとの接続を実現します。さらにAzure Event HubsはAzure Functions、各種プログラミング言語SDK、Kafkaエコシステムをサポートし、デバイスからクラウドまでのIoTデータアクセスと処理をシームレスに促進します。

これらの機能は統合能力と柔軟性を高め、ユーザーが大量のIoTデバイスデータとAzureの接続を迅速に実装できるよう支援します。クラウドコンピューティングによるデータ分析・インテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションの構築を可能にします。

## はじめる前に

本節では、EMQXダッシュボードでAzure Event Hubデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Azure Event Hubのセットアップ

Azure Event Hubデータ統合を利用するには、AzureアカウントでNamespaceとEvent Hubをセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ手順が記載されています。

- [クイックスタート：Azureポータルを使ってイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート：Azure Event HubsとApache Kafkaでデータをストリームする](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQXは「Connection String」の手順に従って接続します。
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka向けAzure Event Hubsとは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubsデータ統合を作成するには、Azure Event Hubs SinkとAzure Event Hubsを接続するコネクターを作成する必要があります。

1. EMQXダッシュボードにアクセスし、**Integrations** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせとし、例として `my-azure-event-hubs` を使用します。
5. 接続情報を設定します。
   - **Bootstrap Host**：Namespaceのホスト名を入力します。デフォルトポートは `9093` です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：Namespaceの共有アクセス ポリシーの「Connection string - primary key」にある接続文字列を入力します。詳細は[イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**：Azure Event Hubへの接続時はTLSがデフォルトで有効です。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
   - **Request Timeout**：EMQXがAzure Event Hubsからの応答を待つ最大時間（秒）を指定します。デフォルトは `30` 秒です。タイムアウトを超えると接続が古くなったと判断され再接続されます。値が小さすぎると、Azure Event Hubsがリクエストを受理しても応答を遅延させる場合があり、EMQXが再接続後に同じバッチを再送し、重複メッセージや下流の過剰データ量が発生する可能性があります。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。

これで、Azure Event Hubsがコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、ルールとSinkを作成してAzure Event Hubsへストリームするデータを指定します。

## Azure Event Hubs Sink付きルールの作成

本節では、Azure Event Hubs Sinkを追加したルールの作成方法を説明します。

1. EMQXダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. 例として、ルールIDに `my_rule` を入力します。

4. **SQL Editor** に以下のステートメントを入力します。トピック `t/#` のMQTTメッセージをAzure Event Hubsに保存する例です。

   注：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックし、ルールによってトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択し、EMQXがルールで処理したデータをAzure Event Hubsに送信するように設定します。

   **Action** ドロップダウンは `Create Action` のままにします。既存のAzure Event Hubsアクションを選択することも可能ですが、本デモでは新しいSinkを作成してルールに追加します。

6. Sinkの名前と説明を **Name** と **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。ドロップダウン横のボタンから新規コネクター作成も可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink情報を設定します。
   - **Event Hub Name**：使用するEvent Hubの名前を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートしています。詳細は[Kafkaの動的トピック設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hubにパブリッシュされるメッセージに追加されるヘッダーのプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。選択肢は `none` または `json` です。
   - **Extra Azure Event Hub headers**：**Add** をクリックしてAzure Event Hubsヘッダーのキー・バリューのペアを追加できます。
   - **Message Key**：Event Hubのメッセージキー。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Message Value**：Event Hubのメッセージ値。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Partition Strategy**：プロデューサーがメッセージをAzure Event Hubsのパーティションに振り分ける方法を指定します。
     - `random`：メッセージごとにランダムにパーティションを選択します。
     - `key_dispatch`：Azure Event Hubsのメッセージキーをハッシュし、パーティション番号を決定します。
   - **Partitions Limit**：プロデューサーがメッセージを送信可能なパーティションの最大数を制限します。デフォルトでは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて同期（sync）または非同期（async）クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックしてSink設定を完了します。**Create Rule** ページの **Action Outputs** タブに新しいSinkが表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示されます。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しいAzure Event Hubs Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Azure Event Hubsに送信・保存されている様子が確認できます。

## ルールのテスト

Azure Event Hubsデータ統合が期待通り動作するかテストするために、[MQTTX](https://mqttx.app/)を使ってクライアントをシミュレートし、EMQXにMQTTメッセージをパブリッシュします。

1. MQTTXを使ってトピック `t/1` にメッセージを送信します：

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sinkの稼働状況をチェックし、新規の送信メッセージが1件あることを確認します。
3. Kafka互換のコンシューマーを使って、設定したEvent Hubにメッセージが書き込まれているか確認します。Kafka CLIの使用方法は[Apache Kafkaエコシステム向けAzure Event Hubsでのメッセージ送受信にKafka CLIを使う](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 高度な設定

本節では、コネクターおよびSinkのパフォーマンス最適化のための高度なオプションについて説明します。対応するオブジェクト作成時に **Advanced Settings** を展開して設定してください。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producerのみ）クライアントからメタデータ取得要求時にKafkaトピックが存在しない場合、自動的にトピックを作成するかどうかを指定します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証時間含む）を秒単位で指定します。 | `5` 秒             |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数です。これにより、Confluentクラスターなどのリソースが完全に稼働し、データ処理準備が整うまでSinkの操作が進まないようにします。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状態をチェックする間隔（秒）です。            | `15` 秒            |
| Health Check Timeout              | Azure Event Hubsとの接続に対する自動ヘルスチェックのタイムアウト時間（秒）を指定します。 | `60` 秒            |
| Min Metadata Refresh Interval     | クライアントがAzure Event Hubs Kafkaブローカーおよびトピックのメタデータを更新する際の最小間隔（秒）です。値が小さすぎるとKafkaサーバーへの負荷が増加する可能性があります。 | `3` 秒             |
| Metadata Request Timeout          | Kafkaからメタデータを取得する際の最大待機時間（秒）です。      | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能最適化のためのソケット送受信バッファサイズを指定します。 | `1` MB             |
| No Delay                          | システムカーネルがTCPソケットを即時送信するか遅延送信するかを選択します。オンにすると即時送信されます。オフの場合、送信内容が少量のときに約40ミリ秒の遅延が発生します。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続にTCPキープアライブを有効化し、長時間のアイドル状態による接続切断を防止します。値は `Idle, Interval, Probes` の3つの数値をカンマ区切りで指定します。<br>Idle：サーバーがキープアライブプローブを開始するまでのアイドル秒数（Linuxデフォルト7200秒）<br>Interval：各キープアライブプローブ間の秒数（Linuxデフォルト75秒）<br>Probes：応答がない場合に接続を切断と判断するまでの最大プローブ回数（Linuxデフォルト9回）<br>例：`240,30,5` は240秒アイドル後にプローブ開始、30秒間隔で最大5回プローブ実施し応答がなければ切断。 | `none`             |

### Azure Event Hubs プロデューサー Sink 設定

| 項目               | 説明                                                         | 推奨値             |
| ------------------ | ------------------------------------------------------------ | ------------------ |
| Max Batch Age      | プロデューサーバッファ内のメッセージが送信されずに保持される最大期間です。この期間を超えたバッチは破棄されます。切断中にバッファされたメッセージや接続喪失時にアック待ちのメッセージも含まれます。破棄されたメッセージは `dropped.expired` メトリクスにカウントされます。デフォルトの `infinity` はメッセージの有効期限切れを防止しますが、バッファオーバーフロー時は破棄される可能性があります。 | `infinity`         |
| Max Retries        | Azure Event Hubsがリトライ可能なエラー（例：パーティションリーダー変更）を返した場合の最大リトライ回数です。初回試行と全リトライが失敗するとバッチは破棄され、各メッセージは `failed` メトリクスにカウントされます。明示的なエラー応答のみリトライ回数にカウントされ、接続喪失による再送は含まれません。再送は `max_batch_age` によって制限されます。デフォルトの `infinity` は無制限リトライを許可します。 | `infinity`         |
| Reconnect Delay    | 接続喪失後にAzure Event Hubsへの再接続を試みるまでの遅延時間です。切断中もメッセージはバッファに蓄積され、バッファ制限と `max_batch_age` の影響を受けます。デフォルトは `2` 秒です。 | `2` 秒             |
| Max Linger Time    | パーティションごとのプロデューサーがより大きなバッチを形成するために待機する最大時間です。すべてのバッファモードに適用されます。デフォルトの `0` は待機なしでメッセージングのレイテンシを最適化します。多少の遅延が許容される場合は設定することでAzure Event Hubsへのリクエスト数を削減できます。バッチが満杯になると早期に送信されます。ディスクバッファリング時はバッチ書き込み前の待機時間となり、ディスクIOPS削減のため最低 `5ms` の設定を推奨します。 | `0` ミリ秒         |
| Max Linger Bytes   | パーティションごとのプロデューサーがバッチ送信前に蓄積する最大バイト数です。 | `10` MB            |
