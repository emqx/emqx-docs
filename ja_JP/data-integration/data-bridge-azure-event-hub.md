# Azure Event Hubs に MQTT データをストリーム送信する

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データの取り込みに利用されます。EMQX の Azure Event Hubs との統合により、ユーザーは高スループット環境で信頼性の高いデータ転送および処理機能を利用できます。Azure Event Hubs は、EMQX と Azure の豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoT データを Azure Blob Storage、Azure Stream Analytics、さらに Azure 仮想マシン上に展開されたさまざまなアプリケーションやサービスに統合できます。現在、EMQX は SASL/PLAIN 認証および Apache Kafka プロトコル互換のエンドポイントを通じて Azure Event Hubs との統合をサポートしています。

本ページでは、EMQX と Azure Event Hubs 間のデータ統合について包括的に紹介し、ルールと Sink の作成および検証方法を実践的に解説します。

## 動作概要

Azure Event Hubs とのデータ統合は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームを Azure Event Hubs とシームレスに統合し、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう設計されています。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Azure Event Hubs に転送します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリおよびステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のトピックに基づいて MQTT メッセージを処理します。ルールは対応する条件にマッチし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **Azure Event Hubs への転送**：ルールはメッセージを Azure Event Hubs に転送するアクションをトリガーします。データプロパティの設定、オーダーキーの指定、MQTT トピックと Azure Event Hubs ヘッダーのマッピングが容易に行え、データ統合における豊富なコンテキスト情報と順序保証を提供し、柔軟な IoT データ処理を実現します。

MQTT メッセージデータが Azure Event Hubs に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：Azure Event Hubs の強力なデータ処理・分析ツールおよびストリーミング機能を活用し、メッセージデータのリアルタイム処理と分析を行い、貴重な洞察や意思決定支援を得られます。
- イベント駆動型機能：Azure のイベント処理をトリガーし、動的かつ柔軟な機能の起動と処理を実現します。
- データの保存と共有：メッセージデータを Azure Event Hubs のストレージサービスに送信し、大量データの安全な保存と管理を行います。これにより、他の Azure サービスとデータを共有・分析し、多様なビジネスニーズに対応できます。

## 特長とメリット

EMQX と Azure Event Hubs のデータ統合は、以下の機能と利点をビジネスにもたらします。

- **高性能な大量メッセージスループット**：EMQX は膨大な数の MQTT クライアント接続をサポートし、毎秒数百万件のメッセージを Azure Event Hubs に継続的に取り込みます。これにより極めて低いメッセージ転送および保存レイテンシを実現し、Azure Event Hubs の保持時間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：Azure Event Hubs の設定を通じて、MQTT トピックと Azure Event Hubs イベントセンター間の柔軟なマッピングが可能です。さらに MQTT ユーザープロパティを Azure Event Hubs ヘッダーにマッピングでき、データ統合における豊富なコンテキスト情報と順序保証を提供します。
- **弾力的なスケーリング対応**：EMQX と Azure Event Hubs の両方が弾力的なスケーリングをサポートし、アプリケーションの仕様に応じて数 MB から数 TB までの IoT データサイズを容易に拡張可能です。
- **豊富なエコシステム**：標準 MQTT プロトコルを採用し、多様な主流 IoT 伝送プロトコルをサポートする EMQX は、多種多様な IoT デバイスとの接続を実現します。さらに Azure Event Hubs は Azure Functions、各種プログラミング言語 SDK、Kafka エコシステムをサポートし、デバイスからクラウドまでの IoT データアクセスと処理をシームレスに促進します。

これらの機能は統合能力と柔軟性を高め、ユーザーが大量の IoT デバイスデータを迅速に Azure に接続できるよう支援します。クラウドコンピューティングによるデータ分析やインテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションの構築を可能にします。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Azure Event Hub データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Azure Event Hub のセットアップ

Azure Event Hub データ統合を利用するには、Azure アカウントでネームスペースと Event Hub をセットアップする必要があります。以下の公式ドキュメントへのリンクに詳細なセットアップ方法が記載されています。

- [クイックスタート: Azure ポータルを使用してイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート: Azure Event Hubs と Apache Kafka を使用してデータをストリームする](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQX は接続に「接続文字列」を使用するため、「Connection String」の手順に従ってください。
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka 用 Azure Event Hubs とは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubs データ統合を作成するには、Azure Event Hubs Sink と Azure Event Hubs を接続するためのコネクターを作成する必要があります。

1. EMQX ダッシュボードにアクセスし、**Integrations** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせとし、例として `my-azure-event-hubs` などが適切です。
5. 接続情報を設定します。
   - **Bootstrap Host**：ネームスペースのホスト名を入力します。デフォルトポートは `9093` です。その他のフィールドは実際の環境に合わせて設定してください。
   - **Connection String**：ネームスペースの共有アクセス ポリシーの「Connection string - primary key」から取得した接続文字列を入力します。詳細は[イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**：Azure Event Hub への接続時は TLS がデフォルトで有効です。TLS 接続オプションの詳細は[外部リソースアクセスのための TLS 暗号化の有効化](../network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
6. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。

これで、Azure Event Hubs がコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、ルールと Sink を作成して、Azure Event Hubs にストリーム送信するデータを指定します。

## Azure Event Hubs Sink を持つルールの作成

このセクションでは、Azure Event Hubs Sink を追加したルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. 例として、ルール ID に `my_rule` を入力します。

4. **SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` の MQTT メッセージを Azure Event Hubs に保存する例です。

   注意：独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択すると、EMQX はルールで処理されたデータを Azure Event Hubs に送信します。

   **Action** ドロップダウンは `Create Action` のままにするか、既に作成済みの Azure Event Hubs アクションを選択できます。この例では新しい Sink を作成してルールに追加します。

6. Sink の名前と説明を **Name** と **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。ドロップダウン横のボタンから新しいコネクターを作成することも可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink の情報を設定します。
   - **Event Hub Name**：使用する Event Hub の名前を入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定にも対応しています。詳細は[Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hub にパブリッシュされるメッセージに追加されるヘッダーとして使用するプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。選択肢は `none` または `json` です。
   - **Extra Azure Event Hub headers**：**Add** をクリックして、Azure Event Hubs ヘッダーのキーと値のペアを追加できます。
   - **Message Key**：Event Hub のメッセージキーです。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Message Value**：Event Hub のメッセージ値です。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力します。
   - **Patrition Strategy**：プロデューサーがメッセージを Azure Event Hubs のパーティションに割り当てる方法を指定します。
     - `random`：メッセージごとにランダムにパーティションを選択します。
     - `key_dispatch`：Event Hubs メッセージキーのハッシュ値に基づいてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて **sync** または **async** クエリモードを選択します。詳細は[Sink の機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックして Sink の設定を完了します。**Create Rule** ページに戻ると、**Action Outputs** タブに新しい Sink が表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを作成します。作成したルールはルール一覧に表示されます。

これでルールの作成が完了し、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Azure Event Hubs Sink を確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Azure Event Hubs に送信・保存されていることがわかります。

## ルールのテスト

Azure Event Hubs データ統合が期待通りに動作するかテストするために、[MQTTX](https://mqttx.app/) を使用してクライアントをシミュレートし、EMQX に MQTT メッセージをパブリッシュできます。

1. MQTTX を使ってトピック `t/1` にメッセージを送信します。

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sink の稼働状況をチェックし、新しい送信メッセージが 1 件あることを確認してください。
3. Kafka 互換のコンシューマーを使って、設定した Event Hub にメッセージが書き込まれているか確認します。Kafka CLI の使用方法については、[Azure Event Hubs for Apache Kafka Ecosystem で Kafka CLI を使ってメッセージの送受信を行う](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 詳細設定

このセクションでは、コネクターのパフォーマンス最適化や特定シナリオに応じたカスタマイズ操作が可能な詳細設定オプションについて説明します。対応するオブジェクト作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （プロデューサー専用）有効にすると、クライアントがメタデータ取得リクエストを送信した際に存在しない Kafka トピックを自動作成します。 | `Disabled`         |
| Connect Timeout                   | TCP 接続確立の最大待機時間（認証有効時は認証時間も含む）       | `5` 秒             |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。これにより、Confluent クラスターなどのリソースが完全に稼働しデータ処理可能になるまで Sink の操作を保留します。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状態をチェックする間隔時間                   | `15` 秒            |
| Health Check Timeout              | Azure Event Hubs との接続に対する自動ヘルスチェックのタイムアウト時間 | `60` 秒            |
| Min Metadata Refresh Interval     | クライアントが Azure Event Hubs Kafka ブローカーおよびトピックのメタデータを更新する際の最小間隔。短すぎると Kafka サーバーへの負荷が増加します。 | `3` 秒             |
| Metadata Request Timeout          | Kafka からメタデータを要求する際の最大待機時間               | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能を最適化するためのソケットバッファサイズ | `1` MB             |
| No Delay                          | システムカーネルが TCP ソケットを即時送信するか遅延送信するかの設定。オンにすると即時送信されます。オフの場合、送信内容が少ないと最大 40 ミリ秒の遅延が発生します。 | `Enabled`          |
| TCP Keepalive                     | Kafka ブリッジ接続の TCP キープアライブ機能を有効にし、長時間の非アクティブ状態による接続切断を防止します。値は `Idle, Interval, Probes` の形式でカンマ区切りの3つの数値を指定します。<br />Idle：接続がアイドル状態となってからキープアライブプローブを開始するまでの秒数（Linux のデフォルトは 7200 秒）。<br />Interval：各キープアライブプローブ間の秒数（Linux のデフォルトは 75 秒）。<br />Probes：応答がない場合に接続を切断とみなすまでの最大プローブ回数（Linux のデフォルトは 9 回）。<br />例：`240,30,5,` は、240 秒のアイドル後にプローブを開始し、30 秒間隔で最大 5 回プローブを送信し応答がなければ接続を切断します。 | `none`             |
