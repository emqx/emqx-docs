# Azure Event Hubs への MQTT データストリーム

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データの取り込みに利用されます。EMQX の Azure Event Hub との統合により、高スループット環境での信頼性の高いデータ転送および処理が可能になります。Azure Event Hubs は、EMQX と Azure の豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoT データを Azure Blob Storage、Azure Stream Analytics、さらには Azure 仮想マシン上に展開された各種アプリケーションやサービスへ統合できます。現在、EMQX は SASL/PLAIN 認証および Apache Kafka プロトコル互換のエンドポイントを通じて Azure Event Hub との統合をサポートしています。

本ページでは、EMQX と Azure Event Hubs 間のデータ統合について包括的に紹介し、ルールと Sink の作成および検証方法を実践的に解説します。

## 動作概要

Azure Event Hubs とのデータ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Azure Event Hubs とシームレスに連携させ、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう設計されています。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Azure Event Hubs に転送します。処理の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のトピックにマッチする MQTT メッセージを処理します。ルールエンジンは該当するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報付加などを行います。
3. **Azure Event Hubs への転送**：ルールによりメッセージを Azure Event Hubs に転送するアクションがトリガーされます。データプロパティ、オーダーキー、MQTT トピックと Azure Event Hubs ヘッダーのマッピングを柔軟に設定可能で、データ統合における豊富なコンテキスト情報と順序保証を実現し、柔軟な IoT データ処理を可能にします。

MQTT メッセージデータが Azure Event Hubs に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理・分析：強力な Azure Event Hubs のデータ処理・分析ツールとストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、有益なインサイトや意思決定支援を得られます。
- イベント駆動機能：Azure のイベントハンドリングをトリガーし、動的かつ柔軟な関数の起動および処理を実現します。
- データ保存・共有：メッセージデータを Azure Event Hubs のストレージサービスに送信し、大量データの安全な保存・管理を行います。これにより、他の Azure サービスと連携してデータの共有や分析を行い、多様なビジネスニーズに対応可能です。

## 特長と利点

EMQX と Azure Event Hubs のデータ統合は、以下の機能とメリットをビジネスにもたらします。

- **高性能な大量メッセージスループット**：EMQX は膨大な数の MQTT クライアント接続をサポートし、毎秒数百万件のメッセージを Azure Event Hubs に継続的に取り込みます。これにより極めて低いメッセージ転送・保存レイテンシを実現し、Azure Event Hubs の保持時間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：Azure Event Hubs の設定を通じて、MQTT トピックと Azure Event Hubs のイベントセンター間の柔軟なマッピングが可能です。MQTT ユーザープロパティを Azure Event Hubs ヘッダーにマッピングすることもでき、データ統合における豊富なコンテキスト情報と順序保証を提供します。
- **弾力的なスケーラビリティ対応**：EMQX と Azure Event Hubs はともに弾力的なスケーラビリティをサポートし、アプリケーションの仕様に応じて数 MB から数 TB までの IoT データサイズを容易に拡張可能です。
- **豊富なエコシステム**：標準 MQTT プロトコルを採用し、各種主流の IoT 伝送プロトコルをサポートすることで、多様な IoT デバイスとの接続を実現します。さらに、Azure Event Hubs は Azure Functions、各種プログラミング言語 SDK、Kafka エコシステムをサポートし、デバイスからクラウドまでのシームレスな IoT データアクセスと処理を促進します。

これらの機能は統合能力と柔軟性を高め、ユーザーが大量の IoT デバイスデータを迅速に Azure と接続できるよう支援します。クラウドコンピューティングによるデータ分析・インテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションの構築を可能にします。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Azure Event Hub データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX のデータ統合に関する [ルール](./rules.md) の知識
- [データ統合](./data-bridges.md) の知識

### Azure Event Hub のセットアップ

Azure Event Hub データ統合を利用するには、Azure アカウント内で Namespace と Event Hub をセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ手順が記載されています。

- [クイックスタート：Azure ポータルを使用してイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート：Azure Event Hubs と Apache Kafka を使用してデータをストリームする](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQX は「接続文字列」の手順に従って接続します。
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka 用 Azure Event Hubs とは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubs データ統合を作成するには、Azure Event Hubs Sink と Azure Event Hubs を接続するコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integrations** -> **Connectors** をクリックします。
2. 画面右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は英数字の組み合わせにしてください。例：`my-azure-event-hubs`
5. 接続情報を設定します。
   - **Bootstrap Host**：Namespace のホスト名を入力します。デフォルトポートは `9093` です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：Namespace の共有アクセス ポリシーの「接続文字列 - プライマリキー」を入力します。詳細は [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string) を参照してください。
   - **Enable TLS**：Azure Event Hub への接続時は TLS がデフォルトで有効です。TLS 接続オプションの詳細は [外部リソースアクセスのための TLS 暗号化の有効化](../network/overview.md#enable-tls-encryption-for-accessing-external-resources) をご覧ください。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。

これで、Azure Event Hubs がコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、ルールと Sink を作成して、Azure Event Hubs へストリームするデータを指定します。

## Azure Event Hubs Sink を持つルールの作成

このセクションでは、Azure Event Hubs Sink を追加したルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. 画面右上の **Create** をクリックします。

3. ルール ID に例として `my_rule` を入力します。

4. **SQL Editor** に、トピック `t/#` の MQTT メッセージを Azure Event Hubs に保存する場合、以下のステートメントを入力します。

   注意：独自の SQL 文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択すると、EMQX はルールで処理されたデータを Azure Event Hubs に送信します。

   **Action** ドロップダウンは `Create Action` のままにしておくか、既存の Azure Event Hubs アクションを選択できます。この例では新しい Sink を作成してルールに追加します。

6. Sink の名前と説明を **Name** と **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。新しいコネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータの詳細は [コネクターの作成](#コネクターの作成) を参照してください。

8. Sink の情報を設定します。
   - **Event Hub Name**：使用する Event Hub の名前を入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定にも対応しています。詳細は [Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics) を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hub にパブリッシュされるメッセージに追加されるヘッダーのプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。選択肢は `none` または `json` です。
   - **Extra Azure Event Hub headers**：**Add** をクリックして、Azure Event Hubs ヘッダーのキー・バリューのペアを追加できます。
   - **Message Key**：Event Hub のメッセージキーを入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が利用可能です。
   - **Message Value**：Event Hub のメッセージ値を入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が利用可能です。
   - **Partition Strategy**：プロデューサーがメッセージを Azure Event Hubs のパーティションにどのように割り当てるかを指定します。
     - `random`：メッセージごとにランダムにパーティションを選択します。
     - `key_dispatch`：Azure Event Hubs メッセージキーのハッシュ値に基づいてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらのアクションはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

10. **詳細設定（任意）**：必要に応じて **sync** または **async** クエリモードを選択します。詳細は [Sink の機能](./data-bridges.md#features-of-sink) をご覧ください。

11. **Create** ボタンをクリックして Sink の設定を完了します。**Create Rule** ページの **Action Outputs** タブに新しい Sink が表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを作成します。作成したルールはルール一覧に表示されます。

これでルールの作成が完了し、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Azure Event Hubs Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Azure Event Hubs に送信・保存されている様子が確認できます。

## ルールのテスト

Azure Event Hubs とのデータ統合が期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/) を使ってクライアントをシミュレートし、EMQX に MQTT メッセージをパブリッシュします。

1. MQTTX を使ってトピック `t/1` にメッセージを送信します。

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sink の稼働状況をチェックし、新たに 1 件の送信メッセージがあることを確認してください。
3. Kafka 互換のコンシューマーを使って、設定した Event Hub にメッセージが書き込まれているか確認します。Kafka CLI の使用方法については [Azure Event Hubs for Apache Kafka Ecosystem で Kafka CLI を使ったメッセージ送受信](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli) を参照してください。

## 高度な設定

このセクションでは、コネクターのパフォーマンスを最適化し、特定のシナリオに応じたカスタマイズ操作を行うための高度な設定オプションを説明します。該当オブジェクト作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （プロデューサーのみ）有効にすると、クライアントがメタデータ取得要求を送信した際に、存在しない Kafka トピックを自動作成します。 | `Disabled`         |
| Connect Timeout                   | TCP 接続確立の最大待機時間（認証が有効な場合は認証時間も含む） | `5` 秒             |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。これにより、Confluent クラスターなどの接続リソースが完全に稼働しデータ処理可能になるまで Sink の操作を保留できます。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状態をチェックする間隔                         | `15` 秒            |
| Health Check Timeout              | Azure Event Hubs との接続に対する自動ヘルスチェックのタイムアウト時間 | `60` 秒            |
| Min Metadata Refresh Interval     | クライアントが Azure Event Hubs Kafka ブローカーおよびトピックのメタデータを更新する際の最小間隔。短すぎると Kafka サーバーへの負荷が増加します。 | `3` 秒             |
| Metadata Request Timeout          | Kafka からメタデータを要求する際の最大待機時間                 | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク転送性能最適化のためのソケットバッファサイズ管理   | `1` MB             |
| No Delay                          | TCP ソケットを即時送信するか遅延送信するかを選択。オンにすると即時送信されます。オフの場合、送信内容が少ないときに約 40 ミリ秒の遅延が発生する可能性があります。 | `Enabled`          |
| TCP Keepalive                     | Kafka ブリッジ接続に対する TCP キープアライブ機能を有効化し、長時間の非アクティブ状態による接続切断を防止します。値は `Idle, Interval, Probes` の形式でカンマ区切りの3つの数値を指定します。<br />Idle：接続がアイドル状態となってからキープアライブプローブを送信開始するまでの秒数（Linux のデフォルトは 7200 秒）<br />Interval：キープアライブプローブ間の秒数（Linux のデフォルトは 75 秒）<br />Probes：応答なしと判断するまでの最大プローブ回数（Linux のデフォルトは 9 回）<br />例：`240,30,5` と設定すると、240 秒のアイドル後にプローブを開始し、30 秒間隔で最大 5 回プローブを送信、応答がなければ接続を切断と判断します。 | `none`             |
