# Azure Event Hubs に MQTT データをストリームする

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データの取り込みを行います。EMQX の Azure Event Hubs との統合により、高スループット環境での信頼性の高いデータ転送および処理機能をユーザーに提供します。Azure Event Hubs は、EMQX と Azure の豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoT データを Azure Blob Storage、Azure Stream Analytics、および Azure 仮想マシン上に展開された各種アプリケーションやサービスに統合できます。現在、EMQX は SASL/PLAIN 認証および Apache Kafka プロトコル互換のエンドポイントを通じて Azure Event Hubs との統合をサポートしています。

本ページでは、EMQX と Azure Event Hubs 間のデータ統合について包括的に紹介し、ルールおよび Sink の作成と検証に関する実践的な手順を提供します。

## 動作の仕組み

Azure Event Hubs とのデータ統合は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームを Azure Event Hubs とシームレスに統合し、IoT アプリケーション開発における豊富なサービスや機能を活用できるよう支援します。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Azure Event Hubs に転送します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のソースからの MQTT メッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールにマッチし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **Azure Event Hubs へのブリッジング**：ルールはメッセージを Azure Event Hubs に転送するアクションをトリガーし、データプロパティ、オーダーキー、MQTT トピックと Azure Event Hubs ヘッダーのマッピングを簡単に設定できます。これにより、データ統合におけるより豊かなコンテキスト情報と順序保証が提供され、柔軟な IoT データ処理が可能になります。

MQTT メッセージデータが Azure Event Hubs に書き込まれた後、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：強力な Azure Event Hubs のデータ処理・分析ツールおよびストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、価値あるインサイトや意思決定支援を得られます。
- イベント駆動型機能：Azure のイベント処理をトリガーし、動的かつ柔軟な機能トリガーおよび処理を実現します。
- データ保存と共有：メッセージデータを Azure Event Hubs のストレージサービスに送信し、大量データの安全な保存・管理を行います。これにより、他の Azure サービスと連携してデータの共有や分析を行い、多様なビジネスニーズに対応できます。

## 特長と利点

EMQX と Azure Event Hubs のデータ統合は、以下の機能と利点をビジネスにもたらします。

- **高性能な大量メッセージスループット**：EMQX は膨大な数の MQTT クライアントとの接続をサポートし、毎秒数百万のメッセージを継続的に Azure Event Hubs に取り込みます。これにより、極めて低いメッセージ伝送および保存のレイテンシを実現し、Azure Event Hubs の保持時間設定によってメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：設定した Azure Event Hubs を通じて、MQTT トピックと Azure Event Hubs のイベントセンター間の柔軟なマッピングが可能です。また、MQTT ユーザープロパティを Azure Event Hubs ヘッダーにマッピングすることもサポートし、データ統合におけるより豊かなコンテキスト情報と順序保証を提供します。
- **弾力的なスケーリング対応**：EMQX と Azure Event Hubs の両方が弾力的なスケーリングをサポートし、アプリケーションの仕様に応じて拡張可能で、IoT データサイズを数 MB から数 TB まで容易にスケールアップできます。
- **豊富なエコシステム**：標準 MQTT プロトコルの採用により、さまざまな主流 IoT 通信プロトコルをサポートし、多様な IoT デバイスとの接続を実現します。さらに、Azure Event Hubs が Azure Functions、各種プログラミング言語 SDK、Kafka エコシステムをサポートすることで、デバイスからクラウドまでのシームレスな IoT データアクセスと処理を促進します。

これらの機能により、統合能力と柔軟性が向上し、ユーザーは大量の IoT デバイスデータと Azure の接続を迅速に実装できます。クラウドコンピューティングによるデータ分析・インテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションの構築を支援します。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Azure Event Hub データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Azure Event Hub のセットアップ

Azure Event Hub データ統合を利用するには、Azure アカウントで Namespace と Event Hub をセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ方法の詳細があります。

- [クイックスタート：Azure ポータルを使用してイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート：Azure Event Hubs と Apache Kafka でデータをストリームする](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQX は接続に「Connection String」の手順に従います。
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Azure Event Hubs for Apache Kafka とは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubs データ統合を作成するには、Azure Event Hubs Sink を Azure Event Hubs に接続するコネクターを作成する必要があります。

1. EMQX ダッシュボードにアクセスし、**Integrations** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせとし、例として `my-azure-event-hubs` などを使用します。
5. 接続情報を設定します。
   - **Bootstrap Host**：Namespace のホスト名を入力します。デフォルトポートは `9093` です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：Namespace の共有アクセス ポリシーの「Connection string - primary key」から取得した接続文字列を入力します。詳細は[イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**：Azure Event Hub への接続時は TLS がデフォルトで有効です。TLS 接続の詳細設定は[外部リソースアクセスのための TLS 暗号化の有効化](../network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
6. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。

これで Azure Event Hubs がコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、ルールと Sink を作成し、Azure Event Hubs にストリームするデータを指定します。

## Azure Event Hubs Sink を含むルールの作成

このセクションでは、Azure Event Hubs Sink を追加したルールの作成方法を説明します。

1. EMQX ダッシュボードで、**Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に例として `my_rule` を入力します。

4. `t/#` トピックの MQTT メッセージを Azure Event Hubs に保存したい場合は、**SQL Editor** に以下のステートメントを入力します。

   注意：独自の SQL 構文を指定する場合、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択すると、EMQX はルールで処理されたデータを Azure Event Hubs に送信します。

   **Action** ドロップダウンは `Create Action` のままにします。あるいは、既に作成済みの Azure Event Hubs アクションを選択することも可能です。本例では新しい Sink を作成し、ルールに追加します。

6. Sink の名前と説明を **Name** と **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。ドロップダウン横のボタンから新しいコネクターを作成することもできます。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink の情報を設定します。
   - **Event Hub Name**：使用する Event Hub の名前を入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートしています。詳細は[Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hub にパブリッシュされるメッセージに追加されるヘッダーとして使用するプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。`none` または `json` が選択可能です。
   - **Extra Azure Event Hub headers**：**Add** ボタンをクリックして、Azure Event Hubs ヘッダーの追加のキー・バリュー ペアを指定できます。
   - **Message Key**：Event Hub のメッセージキーを指定します。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力可能です。
   - **Message Value**：Event Hub のメッセージ値を指定します。こちらもプレーン文字列またはプレースホルダーを含む文字列を入力可能です。
   - **Partition Strategy**：プロデューサーがメッセージを Azure Event Hubs のパーティションに割り振る方法を指定します。
     - `random`：各メッセージに対してランダムにパーティションを選択します。
     - `key_dispatch`：Azure Event Hubs メッセージキーをハッシュしてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（オプション）**：メッセージ配信失敗時の信頼性向上のために、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **高度な設定（オプション）**：必要に応じて **sync** または **async** クエリモードを選択します。詳細は[Sink の機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックして Sink の設定を完了します。**Create Rule** ページに戻ると、**Action Outputs** タブに新しい Sink が表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示されます。

これでルールの作成が完了し、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Azure Event Hubs Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、`t/#` トピックのメッセージがルール `my_rule` によって解析され、Azure Event Hubs に送信・保存されていることが確認できます。

## ルールのテスト

Azure Event Hubs とのデータ統合が期待通りに動作するかテストするために、[MQTTX](https://mqttx.app/) を使ってクライアントをシミュレートし、EMQX に MQTT メッセージをパブリッシュできます。

1. MQTTX を使ってトピック `t/1` にメッセージを送信します。

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sink の稼働状況をチェックし、新しい送信メッセージが 1 件あることを確認してください。
3. Kafka 互換のコンシューマーを使って、設定した Event Hub にメッセージが書き込まれているか確認します。Kafka CLI の使用方法については、[Azure Event Hubs for Apache Kafka エコシステムでの Kafka CLI を使ったメッセージ送受信](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 高度な設定

このセクションでは、コネクターのパフォーマンスを最適化し、特定のシナリオに応じて動作をカスタマイズできる高度な設定オプションについて説明します。対応するオブジェクト作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （プロデューサーのみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際に Kafka トピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP 接続確立の最大待機時間（認証有効時は認証時間を含む）       | `5` 秒             |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大時間（秒）。Confluent クラスターなどのリソースが完全に稼働し、データ処理準備が整うまで Sink が操作を進めないようにします。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状況をチェックする間隔                         | `15` 秒            |
| Health Check Timeout              | Azure Event Hubs との接続に対する自動ヘルスチェックのタイムアウト時間 | `60` 秒            |
| Min Metadata Refresh Interval     | クライアントが Azure Event Hubs Kafka ブローカーおよびトピックのメタデータを更新する際の最小間隔。短すぎると Kafka サーバーの負荷が増加する可能性があります。 | `3` 秒             |
| Metadata Request Timeout          | Kafka からメタデータを要求する際の最大待機時間                 | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能を最適化するためのソケットバッファサイズ   | `1` MB             |
| No Delay                          | システムカーネルが TCP ソケットを即時送信するか遅延送信するかを選択。トグルをオンにすると「No Delay」が有効になり、即時送信されます。オフの場合、送信内容が少ないときに遅延（デフォルト40ミリ秒）が発生する可能性があります。 | `Enabled`          |
| TCP Keepalive                     | Kafka ブリッジ接続の TCP キープアライブ機能を有効にし、長時間のアイドルによる接続切断を防止します。値は `Idle, Interval, Probes` の形式でカンマ区切りの3つの数値を指定します。<br>Idle：接続がアイドル状態となってからキープアライブプローブ送信までの秒数（Linux デフォルト 7200 秒）<br>Interval：各キープアライブプローブ間の秒数（Linux デフォルト 75 秒）<br>Probes：応答なしと判断するまでの最大プローブ送信回数（Linux デフォルト 9 回）<br>例：`240,30,5` は、240秒のアイドル後にプローブを開始し、30秒間隔で最大5回送信し応答がなければ接続を切断します。 | `none`             |
