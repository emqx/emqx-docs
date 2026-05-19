# Azure Event Hubs への MQTT データストリーム

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データ取り込みに最適です。EMQX の Azure Event Hubs 連携は、高スループット環境において信頼性の高いデータ転送および処理機能をユーザーに提供します。Azure Event Hubs は、EMQX と Azure の豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoT データを Azure Blob Storage、Azure Stream Analytics、さらには Azure 仮想マシン上に展開された各種アプリケーションやサービスに統合できます。現在、EMQX は SASL/PLAIN 認証および Apache Kafka プロトコル互換のエンドポイントを介した Azure Event Hubs 連携をサポートしています。

本ページでは、EMQX と Azure Event Hubs 間のデータ統合について包括的に紹介し、ルールおよび Sink の作成と検証に関する実践的な手順を提供します。

## 動作の仕組み

Azure Event Hubs データ統合は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームを Azure Event Hubs とシームレスに統合し、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう設計されています。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Azure Event Hubs に転送します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のトピックに基づいて MQTT メッセージを処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **Azure Event Hubs へのブリッジング**：ルールはメッセージを Azure Event Hubs に転送するアクションをトリガーします。データプロパティ、順序キーの設定、MQTT トピックから Azure Event Hubs ヘッダーへのマッピングを簡単に設定でき、データ統合におけるより豊かなコンテキスト情報と順序保証を提供し、柔軟な IoT データ処理を可能にします。

MQTT メッセージデータが Azure Event Hubs に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：強力な Azure Event Hubs のデータ処理・分析ツールとストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、貴重な洞察や意思決定支援を得られます。
- イベント駆動型機能：Azure のイベント処理をトリガーし、動的かつ柔軟な機能の起動と処理を実現します。
- データ保存と共有：メッセージデータを Azure Event Hubs のストレージサービスに送信し、大量データの安全な保存・管理を行います。これにより、他の Azure サービスと連携してデータを共有・分析し、多様なビジネスニーズに対応できます。

## 特長とメリット

EMQX と Azure Event Hubs のデータ統合は、以下の機能と利点をビジネスにもたらします。

- **高性能な大量メッセージスループット**：EMQX は膨大な数の MQTT クライアント接続をサポートし、毎秒数百万件のメッセージを継続的に Azure Event Hubs に取り込みます。これにより極めて低いメッセージ伝送および保存のレイテンシを実現し、Azure Event Hubs の保持期間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：設定された Azure Event Hubs を介して、MQTT トピックと Azure Event Hubs イベントセンター間の柔軟なマッピングが可能です。MQTT ユーザープロパティを Azure Event Hubs ヘッダーにマッピングすることもサポートし、データ統合におけるより豊かなコンテキスト情報と順序保証を提供します。
- **弾力的なスケーリング対応**：EMQX と Azure Event Hubs は共に弾力的なスケーリングをサポートし、アプリケーションの仕様に応じて数MBから数TBまでの IoT データ規模を容易に拡張できます。
- **豊富なエコシステム**：標準 MQTT プロトコルの採用と各種主流 IoT 伝送プロトコルのサポートにより、EMQX は多様な IoT デバイスとの接続を実現します。さらに、Azure Event Hubs の Azure Functions、各種プログラミング言語 SDK、Kafka エコシステムのサポートと組み合わせることで、デバイスからクラウドまでの IoT データアクセスと処理をシームレスに行えます。

これらの機能は統合能力と柔軟性を高め、大量の IoT デバイスデータを Azure に迅速に接続することを支援します。ユーザーはクラウドコンピューティングによるデータ分析とインテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションを構築できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Azure Event Hub データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### Azure Event Hub のセットアップ

Azure Event Hub データ統合を利用するには、Azure アカウント内で Namespace と Event Hub をセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ方法が記載されています。

- [クイックスタート: Azure ポータルを使用してイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート: Azure Event Hubs と Apache Kafka を使ったデータストリーム](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQX は接続に「接続文字列」を使用するため、「Connection String」の手順に従ってください。
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka 用 Azure Event Hubs とは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubs データ統合を作成するには、Azure Event Hubs Sink を Azure Event Hubs に接続するためのコネクターを作成する必要があります。

1. EMQX ダッシュボードにアクセスし、**Integrations** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせとし、例として `my-azure-event-hubs` などを推奨します。
5. 接続情報を設定します。
   - **Bootstrap Host**：Namespace のホスト名を入力します。デフォルトポートは `9093` です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：Namespace の共有アクセス ポリシーの「Connection string - primary key」から取得した接続文字列を入力します。詳細は [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string) を参照してください。
   - **Enable TLS**：Azure Event Hub への接続時は TLS がデフォルトで有効です。TLS 接続オプションの詳細は [外部リソースアクセスの TLS 有効化](../network/overview.md#enable-tls-encryption-for-accessing-external-resources) をご覧ください。
6. ページ下部の **Create** ボタンをクリックし、コネクターの作成を完了します。

これで Azure Event Hubs がコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、Azure Event Hubs にストリームするデータを指定するためのルールと Sink を作成します。

## Azure Event Hubs Sink を含むルールの作成

このセクションでは、Azure Event Hubs Sink を追加したルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に例として `my_rule` を入力します。

4. `t/#` トピック配下の MQTT メッセージを Azure Event Hubs に保存したい場合、**SQL Editor** に以下のステートメントを入力します。

   注意：独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックし、ルールによってトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択すると、EMQX はルールで処理されたデータを Azure Event Hubs に送信します。

   **Action** ドロップダウンは `Create Action` のままにするか、既存の Azure Event Hubs アクションを選択できます。この例では新しい Sink を作成してルールに追加します。

6. Sink の名前と説明を **Name** および **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。ドロップダウン横のボタンから新しいコネクターを作成することも可能です。設定パラメータの詳細は [コネクターの作成](#コネクターの作成) を参照してください。

8. Sink 情報を設定します。
   - **Event Hub Name**：使用する Event Hub の名前を入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートしています。詳細は [Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics) を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hub にパブリッシュされるメッセージに追加されるヘッダーとして使用するプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。選択肢は `none` または `json` です。
   - **Extra Azure Event Hub headers**：**Add** をクリックして、Azure Event Hubs ヘッダーの追加のキー・バリューを設定できます。
   - **Message Key**：Event Hub のメッセージキーを入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が可能です。
   - **Message Value**：Event Hub のメッセージ値を入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が可能です。
   - **Partition Strategy**：プロデューサーがメッセージを Azure Event Hubs のパーティションに振り分ける方法を指定します。
     - `random`：メッセージごとにランダムにパーティションを選択します。
     - `key_dispatch`：Event Hubs メッセージキーをハッシュしてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

10. **詳細設定（任意）**：必要に応じて **sync** または **async** クエリモードを選択します。詳細は [Sink の機能](./data-bridges.md#features-of-sink) をご覧ください。

11. **Create** ボタンをクリックして Sink の設定を完了します。**Create Rule** ページに戻ると、**Action Outputs** タブに新しい Sink が表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示されます。

これでルールの作成が完了し、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Azure Event Hubs Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` 配下のメッセージがルール `my_rule` によって解析され、Azure Event Hubs に送信・保存されていることがわかります。

## ルールのテスト

Azure Event Hubs データ統合が期待通りに動作するかテストするために、[MQTTX](https://mqttx.app/) を使用してクライアントをシミュレートし、EMQX に MQTT メッセージをパブリッシュできます。

1. MQTTX を使ってトピック `t/1` にメッセージを送信します。

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sink の稼働状況をチェックし、新しい送信メッセージが 1 件あるはずです。
3. Kafka 互換のコンシューマーを使用して、設定した Event Hub にメッセージが書き込まれているか確認します。Kafka CLI の使用方法については、[Azure Event Hubs for Apache Kafka エコシステムでのメッセージ送受信に Kafka CLI を使う](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli) を参照してください。

## 高度な設定

このセクションでは、コネクターのパフォーマンス最適化や特定シナリオに応じたカスタマイズ操作が可能な高度な設定オプションについて説明します。該当オブジェクト作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （プロデューサーのみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際に Kafka トピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP 接続確立の最大待機時間（認証有効時は認証時間も含む）       | `5` 秒             |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。これにより、Confluent クラスターなどの接続リソースが完全に稼働しデータ処理準備が整うまで Sink の操作を保留できます。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状態をチェックする間隔時間                   | `15` 秒            |
| Health Check Timeout              | Azure Event Hubs との接続に対する自動ヘルスチェックのタイムアウト時間 | `60` 秒            |
| Min Metadata Refresh Interval     | クライアントが Azure Event Hubs Kafka ブローカーおよびトピックのメタデータを更新する最小間隔。小さすぎると Kafka サーバーへの負荷が増加します。 | `3` 秒             |
| Metadata Request Timeout          | ブリッジが Kafka からメタデータを要求する際の最大待機時間     | `5` 秒             |
| Socket Send / Receive Buffer Size | ソケットバッファサイズを管理し、ネットワーク伝送性能を最適化   | `1` MB             |
| No Delay                          | システムカーネルが TCP ソケットを即時送信するか遅延送信するか選択。トグルをオンにすると「No Delay」が有効となり即時送信されます。オフの場合、送信内容が少量の際に約 40 ミリ秒の遅延が発生する可能性があります。 | `Enabled`          |
| TCP Keepalive                     | Kafka ブリッジ接続に対して TCP キープアライブ機構を有効にし、長時間の非通信による接続切断を防止します。値はカンマ区切りの 3 つの数値（`Idle, Interval, Probes`）で指定します。<br />Idle：接続がアイドル状態となってからキープアライブプローブを開始するまでの秒数（Linux デフォルトは 7200 秒）。<br />Interval：各キープアライブプローブ間の秒数（Linux デフォルトは 75 秒）。<br />Probes：応答なしと判断するまでの最大プローブ送信回数（Linux デフォルトは 9 回）。<br />例：`240,30,5,` と設定すると、240 秒のアイドル後にプローブを開始し、30 秒間隔で送信、5 回応答がなければ接続を切断と見なします。 | `none`             |
