# Azure Event HubsへのMQTTデータストリーム

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs)は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データの取り込みに利用されます。EMQXのAzure Event Hubsとの統合により、高スループット環境下での信頼性の高いデータ転送と処理が可能になります。Azure Event Hubsは、EMQXとAzureの豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoTデータをAzure Blob Storage、Azure Stream Analytics、およびAzure仮想マシン上に展開された各種アプリケーションやサービスと連携させることができます。現在、EMQXはSASL/PLAIN認証およびApache Kafkaプロトコル互換のエンドポイントを通じてAzure Event Hubsとの統合をサポートしています。

本ページでは、EMQXとAzure Event Hubs間のデータ統合について包括的に紹介し、ルールとSinkの作成および検証方法を実践的に解説します。

## 動作の仕組み

Azure Event Hubsのデータ統合はEMQXの標準機能として提供されており、ユーザーがMQTTデータストリームをAzure Event Hubsとシームレスに統合し、IoTアプリケーション開発における豊富なサービスと機能を活用できるよう支援します。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQXはルールエンジンとSinkを介してMQTTデータをAzure Event Hubsに転送します。全体の流れは以下の通りです。

1. **IoTデバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のトピックに基づいてMQTTメッセージを処理します。ルールエンジンは対応するルールにマッチし、データ形式の変換、特定情報のフィルタリング、文脈情報の付加などの処理を行います。
3. **Azure Event Hubsへの転送**：ルールはメッセージをAzure Event Hubsに転送するアクションをトリガーし、データプロパティや順序キーの設定、MQTTトピックとAzure Event Hubsヘッダーのマッピングを柔軟に構成できます。これにより、データ統合における豊富な文脈情報と順序保証が実現し、柔軟なIoTデータ処理が可能になります。

MQTTメッセージデータがAzure Event Hubsに書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理・分析：Azure Event Hubsの強力なデータ処理・分析ツールやストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、有益な洞察や意思決定支援を得られます。
- イベント駆動機能：Azureのイベントハンドリングをトリガーし、動的かつ柔軟な機能の起動や処理を実現します。
- データの保存・共有：メッセージデータをAzure Event Hubsのストレージサービスに送信し、大量データの安全な保存・管理を行います。これにより、他のAzureサービスと連携してデータの共有や分析を行い、多様なビジネスニーズに対応できます。

## 特長と利点

EMQXとAzure Event Hubsのデータ統合は、以下の機能と利点をビジネスにもたらします。

- **高性能な大量メッセージスループット**：EMQXは膨大な数のMQTTクライアント接続をサポートし、毎秒数百万件のメッセージをAzure Event Hubsに継続的に取り込むことが可能です。これにより、極めて低いメッセージ伝送および保存レイテンシを実現し、Azure Event Hubsの保持時間設定によってメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：Azure Event Hubsの設定を通じて、MQTTトピックとAzure Event Hubsのイベントセンター間の柔軟なマッピングが可能です。また、MQTTのユーザープロパティをAzure Event Hubsヘッダーにマッピングすることもサポートし、データ統合における豊富な文脈情報と順序保証を提供します。
- **弾力的なスケーラビリティ対応**：EMQXおよびAzure Event Hubsは共に弾力的なスケーラビリティをサポートし、アプリケーションの仕様に応じて数MBから数TBまでのIoTデータサイズを容易に拡張できます。
- **豊富なエコシステム**：標準のMQTTプロトコルを採用し、主要なIoT通信プロトコルをサポートすることで、EMQXは多様なIoTデバイスとの接続を実現します。さらに、Azure Event HubsのAzure Functions対応や各種プログラミング言語SDK、Kafkaエコシステムのサポートにより、デバイスからクラウドまでのIoTデータアクセスと処理をシームレスに行えます。

これらの機能は統合能力と柔軟性を高め、大量のIoTデバイスデータとAzureの接続を迅速に実現します。ユーザーはクラウドコンピューティングによるデータ分析・インテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションを構築できます。

## はじめる前に

このセクションでは、EMQXダッシュボードでAzure Event Hubデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXのデータ統合[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Azure Event Hubのセットアップ

Azure Event Hubデータ統合を利用するには、AzureアカウントでNamespaceとEvent Hubをセットアップする必要があります。以下の公式ドキュメントリンクに詳細なセットアップ手順があります。

- [クイックスタート：Azureポータルを使ってイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート：Azure Event HubsとApache Kafkaでデータをストリームする](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQXは「接続文字列」の手順に従って接続します。
- [Event Hubsの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka用Azure Event Hubsとは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubsデータ統合を作成するには、Azure Event Hubs SinkをAzure Event Hubsに接続するコネクターを作成する必要があります。

1. EMQXダッシュボードにアクセスし、**Integrations** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで、コネクタータイプとして**Azure Event Hubs**を選択し、**Next**をクリックします。
4. コネクターの名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせとし、例として`my-azure-event-hubs`などが適切です。
5. 接続情報を設定します。
   - **Bootstrap Host**：Namespaceのホスト名を入力します。デフォルトポートは`9093`です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：Namespaceの共有アクセスポリシーの「Connection string - primary key」から取得した接続文字列を入力します。詳細は[Event Hubsの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**：Azure Event Hubへの接続時はTLSがデフォルトで有効です。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
6. ページ下部の**Create**ボタンをクリックしてコネクターの作成を完了します。

これで、Azure Event Hubsがコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status**が**Connected**となっているはずです。次に、ルールとSinkを作成して、Azure Event Hubsにストリームするデータを指定します。

## Azure Event Hubs Sink付きルールの作成

このセクションでは、Azure Event Hubs Sinkを追加したルールの作成方法を説明します。

1. EMQXダッシュボードで、**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. 例として、ルールIDに`my_rule`を入力します。

4. MQTTメッセージのうちトピック`t/#`のメッセージをAzure Event Hubsに保存したい場合、**SQL Editor**に以下のステートメントを入力します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドが`SELECT`部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action**ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action**のドロップダウンリストから`Azure Event Hubs`を選択すると、EMQXはルールで処理したデータをAzure Event Hubsに送信します。

   **Action**のドロップダウンは`Create Action`のままにします。既存のAzure Event Hubsアクションを選択することも可能ですが、この例では新しいSinkを作成してルールに追加します。

6. Sinkの**Name**と**Description**に名前と説明を入力します。

7. **Connector**のドロップダウンから先ほど作成した`my-azure-event-hubs`を選択します。ドロップダウン横のボタンから新しいコネクターを作成することも可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink情報を設定します。
   - **Event Hub Name**：使用するEvent Hubの名前を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定にも対応しています。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hubにパブリッシュする際にメッセージに追加されるヘッダーのプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。`none`または`json`が選択可能です。
   - **Extra Azure Event Hub headers**：**Add**をクリックして、Azure Event Hubsヘッダーのキーと値のペアを追加できます。
   - **Message Key**：Event Hubのメッセージキーを入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が利用可能です。
   - **Message Value**：Event Hubのメッセージ値を入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が利用可能です。
   - **Partition Strategy**：メッセージをAzure Event Hubsのパーティションに割り当てる方法を指定します。
     - `random`：メッセージごとにランダムにパーティションを選択します。
     - `key_dispatch`：Azure Event Hubsのメッセージキーをハッシュしてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にこれらのアクションがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**ボタンをクリックしてSinkの設定を完了します。**Create Rule**ページに戻ると、新しいSinkが**Action Outputs**タブに表示されます。

12. **Create Rule**ページで設定内容を確認し、**Create**ボタンをクリックしてルールを作成します。作成したルールはルール一覧に表示されます。

これでルールが正常に作成され、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいAzure Event Hubs Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で処理された後にAzure Event Hubsに送信・保存されていることが確認できます。

## ルールのテスト

Azure Event Hubsデータ統合が期待通りに動作するかテストするために、[MQTTX](https://mqttx.app/)を使ってクライアントをシミュレートし、EMQXにMQTTメッセージをパブリッシュできます。

1. MQTTXを使ってトピック`t/1`にメッセージを送信します。

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule**ページでルール名をクリックし、統計情報を確認します。Sinkの稼働状況をチェックし、新しい送信メッセージが1件あることを確認してください。
3. Kafka互換のコンシューマーを使って、設定したEvent Hubにメッセージが書き込まれているか確認します。Kafka CLIの使用方法については、[Azure Event Hubs for Apache KafkaエコシステムでKafka CLIを使ってメッセージ送受信](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 高度な設定

このセクションでは、コネクターのパフォーマンス最適化や特定のシナリオに応じたカスタマイズ操作のための高度な設定オプションを説明します。対応するオブジェクト作成時に**Advanced Settings**を展開し、以下の設定をビジネスニーズに応じて構成できます。

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （プロデューサーのみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際に存在しないKafkaトピックを自動作成します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証が有効な場合は認証時間も含む） | `5`秒              |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大時間（秒）。Confluentクラスターなどのリソースが完全に稼働し、データ処理準備が整うまでSinkの操作を待機させるための設定。 | `5`秒              |
| Health Check Interval             | コネクターの稼働状況をチェックする間隔時間                      | `15`秒             |
| Health Check Timeout              | Azure Event Hubsとの接続に対する自動ヘルスチェックのタイムアウト時間 | `60`秒             |
| Min Metadata Refresh Interval     | クライアントがAzure Event Hubs Kafkaブローカーおよびトピックのメタデータを更新する際の最小間隔時間。短すぎるとKafkaサーバーへの負荷が増加する可能性あり。 | `3`秒              |
| Metadata Request Timeout          | Kafkaからメタデータを要求する際の最大待機時間                   | `5`秒              |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能を最適化するためのソケットバッファサイズの管理 | `1`MB              |
| No Delay                          | システムカーネルがTCPソケットを即時送信するか遅延送信するかを選択。トグルをオンにすると「No Delay」が有効になり即時送信。オフの場合、送信内容が少量の際に約40ミリ秒の遅延が発生する可能性あり。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間の非アクティブ状態による接続切断を防止。値は`Idle, Interval, Probes`の3つの数値をカンマ区切りで指定。<br>Idle：接続がアイドル状態となってからキープアライブプローブを開始するまでの秒数（Linuxデフォルト7200秒）。<br>Interval：各キープアライブプローブ間の秒数（Linuxデフォルト75秒）。<br>Probes：応答がない場合に送信する最大プローブ数（Linuxデフォルト9回）。<br>例：`240,30,5,`は、240秒のアイドル後にプローブを開始し、30秒間隔でプローブを送り、5回応答がなければ接続を切断とみなす設定。 | `none`             |
