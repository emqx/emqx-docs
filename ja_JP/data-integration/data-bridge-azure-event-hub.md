# Azure Event Hubs への MQTT データストリーム

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データの取り込みに利用されます。EMQX の Azure Event Hub 連携は、高スループット環境において信頼性の高いデータ転送と処理機能をユーザーに提供します。Azure Event Hubs は、EMQX と Azure の豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoT データを Azure Blob Storage、Azure Stream Analytics、Azure 仮想マシン上に展開された各種アプリケーションやサービスに統合できます。現在、EMQX は SASL/PLAIN 認証および Apache Kafka プロトコル互換のエンドポイントを通じて Azure Event Hub 連携をサポートしています。

本ページでは、EMQX と Azure Event Hubs 間のデータ連携について包括的に紹介し、ルールと Sink の作成および検証手順を実践的に解説します。

## 動作概要

Azure Event Hubs とのデータ連携は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームを Azure Event Hubs とシームレスに統合し、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう支援します。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Azure Event Hubs に転送します。全体の流れは以下の通りです：

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のトピックに基づいて MQTT メッセージを処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **Azure Event Hubs への転送**：ルールはメッセージを Azure Event Hubs に転送するアクションをトリガーします。データプロパティ、オーダーキーの設定や MQTT トピックから Azure Event Hubs ヘッダーへのマッピングを簡単に設定でき、より豊富なコンテキスト情報とデータ統合の順序保証を提供し、柔軟な IoT データ処理を可能にします。

MQTT メッセージデータが Azure Event Hubs に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です：

- リアルタイムデータ処理・分析：強力な Azure Event Hubs のデータ処理・分析ツールやストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、価値あるインサイトや意思決定支援を得られます。
- イベント駆動型機能：Azure のイベントハンドリングをトリガーし、動的かつ柔軟な関数の起動と処理を実現します。
- データの保存・共有：メッセージデータを Azure Event Hubs のストレージサービスに送信し、大量データの安全な保存・管理を行います。これにより他の Azure サービスとデータを共有・分析し、多様なビジネスニーズに対応可能です。

## 特長とメリット

EMQX と Azure Event Hubs のデータ連携は、以下の機能と利点をビジネスにもたらします：

- **高性能な大量メッセージスループット**：EMQX は多数の MQTT クライアントとの接続をサポートし、毎秒数百万メッセージを Azure Event Hubs に継続的に取り込みます。これにより極めて低いメッセージ伝送および保存レイテンシを実現し、Azure Event Hubs の保持時間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：Azure Event Hubs の設定を通じて、MQTT トピックと Azure Event Hubs イベントセンター間の柔軟なマッピングが可能です。さらに MQTT ユーザープロパティを Azure Event Hubs ヘッダーにマッピングでき、より豊かなコンテキスト情報と順序保証を提供します。
- **弾力的なスケーリング対応**：EMQX と Azure Event Hubs の両方が弾力的スケーリングをサポートし、アプリケーション仕様に応じて数 MB から数 TB までの IoT データ規模を容易に拡張可能です。
- **豊富なエコシステム**：標準 MQTT プロトコルの採用により、様々な主流 IoT 伝送プロトコルをサポートし、多様な IoT デバイスとの接続を実現します。さらに Azure Event Hubs は Azure Functions、各種プログラミング言語 SDK、Kafka エコシステムをサポートし、デバイスからクラウドまでの IoT データアクセスと処理をシームレスに促進します。

これらの機能は統合能力と柔軟性を高め、ユーザーが大量の IoT デバイスデータを迅速に Azure と接続できるよう支援します。クラウドコンピューティングによるデータ分析・インテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションの構築を可能にします。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Azure Event Hub データ連携を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ連携の [ルール](./rules.md) に関する知識
- [データ連携](./data-bridges.md) に関する知識

### Azure Event Hub のセットアップ

Azure Event Hub データ連携を利用するには、Azure アカウント上で Namespace と Event Hub をセットアップする必要があります。以下の公式ドキュメントリンクにてセットアップ方法の詳細を確認できます。

- [クイックスタート: Azure ポータルを使用してイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート: Azure Event Hubs と Apache Kafka を使ったデータストリーミング](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQX は「Connection String」の手順に従って接続します。
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka 用 Azure Event Hubs とは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

Azure Event Hubs データ連携を作成するには、Azure Event Hubs Sink と Azure Event Hubs を接続するためのコネクターを作成する必要があります。

1. EMQX ダッシュボードにアクセスし、**Integrations** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで、コネクタータイプとして **Azure Event Hubs** を選択し、**Next** をクリックします。
4. コネクターの名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせとし、例として `my-azure-event-hubs` を使用します。
5. 接続情報を設定します。
   - **Bootstrap Host**：Namespace のホスト名を入力します。デフォルトポートは `9093` です。その他のフィールドは実際の環境に合わせて設定してください。
   - **Connection String**：Namespace の共有アクセスポリシーの「Connection string - primary key」から取得した接続文字列を入力します。詳細は [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string) を参照してください。
   - **Enable TLS**：Azure Event Hub への接続時は TLS がデフォルトで有効です。TLS 接続オプションの詳細は [外部リソースアクセスの TLS 有効化](../network/overview.md#enable-tls-encryption-for-accessing-external-resources) をご覧ください。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。

これで、Azure Event Hubs がコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、ルールと Sink を作成し、Azure Event Hubs にストリームするデータを指定します。

## Azure Event Hubs Sink を追加したルールの作成

このセクションでは、Azure Event Hubs Sink を追加したルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に例として `my_rule` を入力します。

4. **SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` の MQTT メッセージを Azure Event Hubs に保存する例です。

   注意：独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

5. **+ Add Action** ボタンをクリックし、ルールによってトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択し、EMQX がルールで処理したデータを Azure Event Hubs に送信するようにします。

   **Action** のドロップダウンは `Create Action` のままにするか、既存の Azure Event Hubs アクションを選択できます。この例では新しい Sink を作成してルールに追加します。

6. **Name** と **Description** テキストボックスに Sink の名前と説明を入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。新しいコネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータの詳細は [コネクターの作成](#コネクターの作成) を参照してください。

8. Sink の情報を設定します。
   - **Event Hub Name**：使用する Event Hub の名前を入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートしています。詳細は [Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics) を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hub にパブリッシュする際にメッセージに追加されるヘッダーとして使用するプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。選択肢は `none` または `json` です。
   - **Extra Azure Event Hub headers**：**Add** ボタンをクリックして、Azure Event Hubs ヘッダーの追加のキー・バリューを指定できます。
   - **Message Key**：Event Hub のメッセージキーを入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が利用可能です。
   - **Message Value**：Event Hub のメッセージ値を入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が利用可能です。
   - **Partition Strategy**：プロデューサーがメッセージを Azure Event Hubs のパーティションに振り分ける方法を指定します。
     - `random`：各メッセージのパーティションをランダムに選択します。
     - `key_dispatch`：Azure Event Hubs メッセージキーをハッシュしてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) をご覧ください。

10. **詳細設定（任意）**：必要に応じて **sync** または **async** クエリモードを選択します。詳細は [Sink の機能](./data-bridges.md#features-of-sink) を参照してください。

11. **Create** ボタンをクリックして Sink の設定を完了します。**Create Rule** ページに戻ると、**Action Outputs** タブに新しい Sink が表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを作成します。作成したルールはルール一覧に表示されます。

これでルールの作成が完了し、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Azure Event Hubs Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Azure Event Hubs に送信・保存されていることが確認できます。

## ルールのテスト

Azure Event Hubs データ連携が期待通りに動作するかテストするために、[MQTTX](https://mqttx.app/) を使ってクライアントをシミュレートし、EMQX に MQTT メッセージをパブリッシュできます。

1. MQTTX を使ってトピック `t/1` にメッセージを送信します：

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sink の稼働状況をチェックし、新しい送信メッセージが 1 件あることを確認してください。

3. Kafka 互換のコンシューマーを使い、設定した Event Hub にメッセージが書き込まれているか確認します。Kafka CLI の使用方法については、[Azure Event Hubs for Apache Kafka Ecosystem でのメッセージ送受信に Kafka CLI を使う](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli) を参照してください。
