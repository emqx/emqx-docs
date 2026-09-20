# Stream MQTT Data into Azure Event Hubs

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、データ取り込みのためのリアルタイム管理イベントストリーミングプラットフォームです。EMQXのAzure Event Hubとの統合により、ユーザーは高スループット環境で信頼性の高いデータ転送および処理機能を利用できます。Azure Event Hubsは、EMQXとAzureの豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoTデータをAzure Blob Storage、Azure Stream Analytics、さらにはAzure仮想マシン上に展開された各種アプリケーションやサービスに統合できます。現在、EMQXはSASL/PLAIN認証およびApache Kafkaプロトコル互換のエンドポイントを通じてAzure Event Hubとの統合をサポートしています。

本ページでは、EMQXとAzure Event Hubs間のデータ統合について包括的に紹介し、ルールおよびSinkの作成と検証に関する実践的な手順を提供します。

## 仕組み

Azure Event Hubsデータ統合は、EMQXの標準機能として提供されており、ユーザーがMQTTデータストリームをAzure Event Hubsとシームレスに統合し、IoTアプリケーション開発における豊富なサービスと機能を活用できるよう支援します。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQXはルールエンジンとSinkを介してMQTTデータをAzure Event Hubsに転送します。全体の流れは以下の通りです。

1. **IoTデバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のトピックに基づきMQTTメッセージを処理します。ルールエンジンは対応するルールとマッチし、データ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報の付加などを行います。
3. **Azure Event Hubsへのブリッジング**：ルールはメッセージをAzure Event Hubsに転送するアクションをトリガーし、データプロパティ、順序キー、MQTTトピックからAzure Event Hubsヘッダーへのマッピングを簡単に設定できます。これにより、データ統合におけるより豊かなコンテキスト情報と順序保証が提供され、柔軟なIoTデータ処理が可能になります。

MQTTメッセージデータがAzure Event Hubsに書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：強力なAzure Event Hubsのデータ処理・分析ツールやストリーミング機能を活用し、メッセージデータのリアルタイム処理と分析を行い、有益な洞察や意思決定支援を得られます。
- イベント駆動型機能：Azureのイベントハンドリングをトリガーし、動的かつ柔軟な機能トリガーと処理を実現します。
- データの保存と共有：メッセージデータをAzure Event Hubsのストレージサービスに送信し、大量データの安全な保存と管理を行います。これにより、他のAzureサービスとデータを共有・分析し、多様なビジネスニーズに対応できます。

## 特徴と利点

EMQXとAzure Event Hubs間のデータ統合は、以下の機能とメリットをビジネスにもたらします。

- **高性能な大量メッセージスループット**：EMQXは膨大な数のMQTTクライアントとの接続をサポートし、毎秒数百万件のメッセージを継続的にAzure Event Hubsに取り込みます。これにより非常に低いメッセージ伝送および保存レイテンシが実現され、Azure Event Hubsの保持時間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：設定したAzure Event Hubsを通じて、MQTTトピックとAzure Event Hubsのイベントセンター間で柔軟なマッピングが可能です。また、MQTTユーザープロパティをAzure Event Hubsヘッダーにマッピングすることもサポートし、データ統合における豊かなコンテキスト情報と順序保証を提供します。
- **弾力的なスケーリング対応**：EMQXとAzure Event Hubsは共に弾力的なスケーリングをサポートし、アプリケーションの仕様に応じて数MBから数TBまでのIoTデータ規模を容易に拡張できます。
- **豊富なエコシステム**：標準MQTTプロトコルの採用により、多様な主流IoT伝送プロトコルをサポートし、様々なIoTデバイスとの接続を実現します。さらにAzure Event HubsのAzure Functions対応、各種プログラミング言語SDK、Kafkaエコシステムのサポートにより、デバイスからクラウドまでのIoTデータアクセスと処理をシームレスに促進します。

これらの機能は統合能力と柔軟性を高め、ユーザーが大量のIoTデバイスデータをAzureに迅速に接続できるよう支援します。クラウドコンピューティングによるデータ分析とインテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションの構築を可能にします。

## はじめる前に

本セクションでは、EMQXダッシュボードでAzure Event Hubデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Azure Event Hubのセットアップ

Azure Event Hubデータ統合を利用するには、AzureアカウントでNamespaceとEvent Hubをセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ方法の詳細があります。

- [クイックスタート: Azureポータルを使用してイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート: Azure Event HubsとApache Kafkaでデータをストリームする](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQXは「Connection String」の手順に従って接続します。
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka向けAzure Event Hubsとは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubsデータ統合を作成するには、Azure Event Hubs SinkをAzure Event Hubsに接続するコネクターを作成する必要があります。

1. EMQXダッシュボードで **Integrations** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は英大文字・小文字と数字の組み合わせにしてください。例：`my-azure-event-hubs`
5. 接続情報を設定します。
   - **Bootstrap Host**：Namespaceのホスト名を入力します。デフォルトポートは`9093`です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：Namespaceの共有アクセス ポリシーの「Connection string - primary key」から取得した接続文字列を入力します。詳細は[イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**：Azure Event Hub接続時はTLSがデフォルトで有効です。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
   - **Request Timeout**：Azure Event Hubsからの応答を待つ最大時間を秒単位で指定します。デフォルトは`30`秒です。タイムアウトを超えるとEMQXは接続が切れたとみなし再接続します。値が小さすぎるとAzure Event Hubsはリクエストを受け入れても応答を遅延させる場合があり、EMQXは再接続後に同じバッチを再送し、重複メッセージや過剰な下流データ量が発生する可能性があります。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。

これでAzure Event Hubsがコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、ルールとSinkを作成してAzure Event Hubsにストリームするデータを指定します。

## Azure Event Hubs Sink付きルールの作成

本セクションでは、Azure Event Hubs Sinkを追加したルールの作成方法を示します。

1. EMQXダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. 例として、ルールIDに `my_rule` と入力します。

4. **SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` のMQTTメッセージをAzure Event Hubsに保存する例です。

   注：独自のSQL構文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択すると、EMQXはルールで処理したデータをAzure Event Hubsに送信します。

   **Action** ドロップダウンは `Create Action` のままにします。既存のAzure Event Hubsアクションを選択することも可能です。本デモでは新しいSinkを作成してルールに追加します。

6. **Name** と **Description** テキストボックスにSinkの名前と説明を入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink情報を設定します。
   - **Event Hub Name**：使用するEvent Hubの名前を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートしています。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hubにパブリッシュされるメッセージに追加されるヘッダーとして使用するプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。`none` または `json` が選択可能です。
   - **Extra Azure Event Hub headers**：**Add** ボタンをクリックして、Azure Event Hubsヘッダーの追加のキー・バリューを指定できます。
   - **Message Key**：Event Hubのメッセージキー。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Message Value**：Event Hubのメッセージ値。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Partition Strategy**：プロデューサーがメッセージをAzure Event Hubsのパーティションに割り当てる方法を指定します。
     - `random`：各メッセージに対してランダムにパーティションを選択します。
     - `key_dispatch`：Azure Event Hubsのメッセージキーをハッシュしてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**同期（sync）**または**非同期（async）**クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックしてSink設定を完了します。**Create Rule** ページの **Action Outputs** タブに新しいSinkが表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示されます。

これでルールの作成が完了し、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しいAzure Event Hubs Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Azure Event Hubsに送信・保存されていることがわかります。

## ルールのテスト

Azure Event Hubsデータ統合が期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/)を使ってクライアントをシミュレートし、EMQXにMQTTメッセージをパブリッシュします。

1. MQTTXを使ってトピック `t/1` にメッセージを送信します：

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sinkの稼働状況をチェックし、新しい送信メッセージが1件あることを確認します。
3. Kafka互換のコンシューマーを使って、設定したEvent Hubにメッセージが書き込まれているか確認します。Kafka CLIの使用方法は[Azure Event Hubs for Apache Kafka EcosystemでのKafka CLIによるメッセージ送受信](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 高度な設定

本セクションでは、コネクターおよびSinkのパフォーマンス最適化のための高度なオプションについて説明します。対応するオブジェクト作成時に**Advanced Settings**を展開して設定してください。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （プロデューサーのみ）有効にすると、クライアントがメタデータ取得リクエストを送信した際に存在しないKafkaトピックを自動作成します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証時間含む）。                    | `5` 秒             |
| Start Timeout                     | 自動起動したリソースが正常状態になるまで待機する最大時間（秒）。Confluentクラスターなどのリソースが完全に稼働し、データ処理可能になるまでSinkの操作を待機させるための設定です。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状態チェック間隔。                             | `15` 秒            |
| Health Check Timeout              | Azure Event Hubsとの接続に対して自動ヘルスチェックを行うタイムアウト時間。 | `60` 秒            |
| Min Metadata Refresh Interval     | Azure Event Hubs Kafkaブローカーおよびトピックのメタデータ更新の最短間隔。短すぎるとKafkaサーバーに過負荷をかける可能性があります。 | `3` 秒             |
| Metadata Request Timeout          | Kafkaからメタデータを取得する際の最大待機時間。                 | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能最適化のためのソケットバッファサイズ管理。  | `1` MB             |
| No Delay                          | システムカーネルがTCPソケットを即時送信するか遅延送信するかの設定。トグルをオンにすると即時送信されます。オフの場合、送信内容が少ないと約40ミリ秒の遅延が発生します。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間の非通信による接続切断を防止します。値は `Idle, Interval, Probes` の3つの数値をカンマ区切りで指定します。<br />Idle：接続がアイドル状態になる秒数（Linuxデフォルト7200秒）<br />Interval：キープアライブプローブ間隔秒数（Linuxデフォルト75秒）<br />Probes：応答なしで接続切断とみなすまでのプローブ回数（Linuxデフォルト9回）<br />例：`240,30,5` は240秒アイドル後にプローブ開始、30秒間隔で5回応答なしで切断と判定。 | `none`             |

### Azure Event Hubs プロデューサーSink設定

| 項目                 | 説明                                                         | 推奨値             |
| -------------------- | ------------------------------------------------------------ | ------------------ |
| Max Batch Age        | プロデューサーバッファ内のメッセージが送信されずに保持される最大時間。超過したバッチは破棄されます。切断中のバッファリングメッセージや接続喪失時の未アックメッセージも対象。破棄されたメッセージは `dropped.expired` メトリクスにカウントされます。デフォルトの `infinity` はメッセージの期限切れを防止します。バッファオーバーフロー時は破棄される場合があります。 | `infinity`         |
| Max Retries          | Azure Event Hubsがリトライ可能なエラー（例：パーティションリーダー変更）を返した際の最大リトライ回数。初回試行とリトライがすべて失敗するとバッチは破棄され、各メッセージは `failed` メトリクスにカウントされます。明示的なAzure Event Hubsエラー応答のみリトライ回数に加算され、接続喪失による再送は加算されず、`max_batch_age` によって制限されます。デフォルトの `infinity` は無制限リトライを許可します。 | `infinity`         |
| Reconnect Delay      | 接続喪失後にプロデューサーがAzure Event Hubsに再接続を試みるまでの遅延時間。切断中もメッセージはバッファに蓄積され、バッファ制限および `max_batch_age` の対象となります。デフォルトは `2` 秒です。 | `2` 秒             |
| Max Linger Time      | パーティションごとのプロデューサーがより大きなバッチを作成するために待機する最大時間。すべてのバッファモードに適用。デフォルトの `0` は待機なしでメッセージ遅延を最小化します。小さな遅延を許容するとAzure Event Hubsへのリクエスト数を減らせます。バッチが満たされると早期に送信されます。ディスクバッファリング時はバッチ書き込み前の待機時間で、IOPS削減のため最低 `5ms` の設定推奨。 | `0` ミリ秒        |
| Max Linger Bytes     | パーティションごとのプロデューサーがバッチ送信前に蓄積する最大バイト数。 | `10` MB            |
