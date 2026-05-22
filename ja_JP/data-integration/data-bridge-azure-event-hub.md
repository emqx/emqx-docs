<<<<<<< HEAD
# Azure Event Hubs への MQTT データストリーミング

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs) は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データ取り込みに適しています。EMQX の Azure Event Hub 連携は、高スループット環境において信頼性の高いデータ転送および処理機能をユーザーに提供します。Azure Event Hubs は、EMQX と Azure の豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoT データを Azure Blob Storage、Azure Stream Analytics、さらには Azure 仮想マシン上に展開された各種アプリケーションやサービスと統合します。現時点で EMQX は、SASL/PLAIN 認証および Apache Kafka プロトコル互換のエンドポイントを通じて Azure Event Hub 連携をサポートしています。

本ページでは、EMQX と Azure Event Hubs 間のデータ統合について包括的に解説し、ルールおよび Sink の作成と検証に関する実践的な手順を提供します。

## 動作概要

Azure Event Hubs とのデータ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Azure Event Hubs とシームレスに連携させ、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう設計されています。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Azure Event Hubs に転送します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のトピックに基づいて MQTT メッセージを処理します。ルールエンジンは対応するルールにマッチし、データ形式の変換、特定情報のフィルタリング、メッセージのコンテキスト情報付加などを行います。
3. **Azure Event Hubs へのブリッジング**：ルールはメッセージを Azure Event Hubs に転送するアクションをトリガーし、データプロパティやオーダーキーの設定、MQTT トピックと Azure Event Hubs ヘッダーのマッピングを簡単に設定できます。これにより、データ統合におけるより豊かなコンテキスト情報と順序保証が提供され、柔軟な IoT データ処理が可能となります。

MQTT メッセージデータが Azure Event Hubs に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：強力な Azure Event Hubs のデータ処理・分析ツールおよびストリーミング機能を活用し、メッセージデータのリアルタイム処理と分析を行い、価値あるインサイトや意思決定支援を得られます。
- イベント駆動型機能：Azure のイベントハンドリングをトリガーし、動的かつ柔軟な機能の起動と処理を実現します。
- データの保存と共有：メッセージデータを Azure Event Hubs のストレージサービスに送信し、大量データの安全な保存と管理を行います。これにより、他の Azure サービスと連携してデータの共有や分析を行い、多様なビジネスニーズに対応できます。
=======
# Azure Event HubsへのMQTTデータストリーム

[Azure Event Hubs](https://azure.microsoft.com/en-us/products/event-hubs)は、リアルタイムのマネージドイベントストリーミングプラットフォームであり、データ取り込みに利用されます。EMQXのAzure Event Hubsとの統合により、高スループット環境下で信頼性の高いデータ転送および処理機能をユーザーに提供します。Azure Event Hubsは、EMQXとAzureの豊富なクラウドサービスアプリケーション間のデータチャネルとして機能し、IoTデータをAzure Blob Storage、Azure Stream Analytics、およびAzure仮想マシン上に展開された各種アプリケーションやサービスに統合可能です。現在、EMQXはSASL/PLAIN認証およびApache Kafkaプロトコル互換のエンドポイントを通じてAzure Event Hubsとの統合をサポートしています。

本ページでは、EMQXとAzure Event Hubs間のデータ統合について包括的に解説し、ルールおよびSinkの作成と検証に関する実践的な手順を提供します。

## 動作概要

Azure Event Hubsデータ統合は、EMQXの標準機能として提供されており、ユーザーがMQTTデータストリームをAzure Event Hubsとシームレスに連携し、IoTアプリケーション開発における豊富なサービスと機能を活用できるよう支援します。

![emqx-integration-azure](./assets/emqx-integration-azure.jpg)

EMQXはルールエンジンとSinkを介してMQTTデータをAzure Event Hubsに転送します。全体の流れは以下の通りです：

1. **IoTデバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンを用いて、特定のトピックにマッチするMQTTメッセージを処理します。ルールエンジンは対応するルールに基づき、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **Azure Event Hubsへのブリッジング**：ルールはメッセージをAzure Event Hubsに転送するアクションをトリガーし、データプロパティやオーダーキーの設定、MQTTトピックとAzure Event Hubsヘッダーのマッピングを容易に行えます。これにより、より豊富なコンテキスト情報と順序保証を伴うデータ統合が実現し、柔軟なIoTデータ処理が可能となります。

MQTTメッセージデータがAzure Event Hubsに書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です：

- リアルタイムデータ処理と分析：強力なAzure Event Hubsのデータ処理・分析ツールおよびストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、有益な洞察や意思決定支援を得られます。
- イベント駆動型機能：Azureのイベントハンドリングをトリガーし、動的かつ柔軟な機能トリガーと処理を実現します。
- データの保存と共有：メッセージデータをAzure Event Hubsのストレージサービスに送信し、大量データの安全な保存と管理を行います。これにより、他のAzureサービスと連携してデータの共有や分析が可能となり、多様なビジネスニーズに対応します。
>>>>>>> origin/release-6.1

## 特長と利点

<<<<<<< HEAD
EMQX と Azure Event Hubs のデータ統合は、以下の機能と利点をビジネスにもたらします。

- **高性能な大量メッセージスループット**：EMQX は膨大な数の MQTT クライアント接続をサポートし、毎秒数百万件のメッセージを継続的に Azure Event Hubs に取り込みます。これにより、非常に低いメッセージ伝送および保存レイテンシを実現し、Azure Event Hubs の保持時間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：設定された Azure Event Hubs を通じて、MQTT トピックと Azure Event Hubs のイベントセンター間で柔軟なマッピングが可能です。また、MQTT のユーザープロパティを Azure Event Hubs ヘッダーにマッピングすることもサポートし、データ統合におけるより豊かなコンテキスト情報と順序保証を提供します。
- **弾力的なスケーリング対応**：EMQX と Azure Event Hubs は共に弾力的なスケーリングをサポートし、アプリケーションの仕様に応じて数MBから数TB規模の IoT データサイズに容易に対応可能です。
- **豊富なエコシステム**：標準 MQTT プロトコルを採用し、各種主流の IoT 伝送プロトコルをサポートすることで、多様な IoT デバイスとの接続を実現します。さらに、Azure Event Hubs は Azure Functions、各種プログラミング言語 SDK、Kafka エコシステムをサポートしており、デバイスからクラウドまでの IoT データアクセスと処理をシームレスに促進します。

これらの機能は統合能力と柔軟性を高め、大量の IoT デバイスデータと Azure の接続を迅速に実現します。ユーザーはクラウドコンピューティングがもたらすデータ分析とインテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションを構築できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Azure Event Hub データ統合を作成する前に準備すべき事項を説明します。

### 前提条件

- EMQX のデータ統合に関する[ルール](./rules.md)の知識
=======
EMQXとAzure Event Hubs間のデータ統合は、以下の機能とメリットをビジネスにもたらします：

- **高性能な大規模メッセージスループット**：EMQXは膨大な数のMQTTクライアント接続をサポートし、毎秒数百万件のメッセージを継続的にAzure Event Hubsに取り込みます。これにより、極めて低いメッセージ転送および保存レイテンシを実現し、Azure Event Hubsの保持時間設定によりメッセージ量の制御も可能です。
- **柔軟なデータマッピング**：Azure Event Hubsの設定を通じて、MQTTトピックとAzure Event Hubsのイベントセンター間で柔軟なマッピングを実現します。また、MQTTユーザープロパティをAzure Event Hubsヘッダーにマッピングすることも可能で、より豊富なコンテキスト情報と順序保証を伴うデータ統合を提供します。
- **弾力的なスケーラビリティ対応**：EMQXとAzure Event Hubsは共に弾力的なスケーリングをサポートし、アプリケーション仕様に応じて数MBから数TBまでのIoTデータ規模を容易に拡張可能です。
- **豊富なエコシステム**：標準MQTTプロトコルを採用し、各種主流IoT通信プロトコルに対応するEMQXは、多様なIoTデバイスとの接続を実現します。さらに、Azure Event HubsはAzure Functionsや各種プログラミング言語SDK、Kafkaエコシステムをサポートし、デバイスからクラウドまでのIoTデータアクセスと処理をシームレスに促進します。

これらの機能は統合能力と柔軟性を高め、ユーザーが大規模なIoTデバイスデータのAzure接続を迅速に実装できるよう支援します。クラウドコンピューティングがもたらすデータ分析およびインテリジェンス機能をより便利に活用し、強力なデータ駆動型アプリケーションの構築を可能にします。

## はじめる前に

このセクションでは、EMQXダッシュボードでAzure Event Hubデータ統合を作成する前に完了すべき準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
>>>>>>> origin/release-6.1
- [データ統合](./data-bridges.md)に関する知識

### Azure Event Hubのセットアップ

<<<<<<< HEAD
Azure Event Hub データ統合を利用するには、Azure アカウント上で Namespace と Event Hub をセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ手順が記載されています。

- [クイックスタート: Azure ポータルでイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート: Azure Event Hubs と Apache Kafka を使ったデータストリーム](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQX は「接続文字列」の手順に従って接続します。
=======
Azure Event Hubデータ統合を利用するには、Azureアカウント内でNamespaceとEvent Hubをセットアップする必要があります。以下の公式ドキュメントリンクにセットアップ方法の詳細があります。

- [クイックスタート：Azureポータルを使ってイベントハブを作成する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-create)
- [クイックスタート：Azure Event HubsとApache Kafkaでデータをストリームする](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-quickstart-kafka-enabled-event-hubs?tabs=connection-string)
  - EMQXが接続に使用するため、「接続文字列」の手順に従ってください。
>>>>>>> origin/release-6.1
- [イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)
- [Apache Kafka向けAzure Event Hubsとは](https://learn.microsoft.com/en-us/azure/event-hubs/azure-event-hubs-kafka-overview)

## コネクターの作成

<<<<<<< HEAD
Azure Event Hubs データ統合を作成するには、Azure Event Hubs Sink を Azure Event Hubs に接続するためのコネクターを作成する必要があります。
=======
Azure Event Hubsデータ統合を作成するには、Azure Event Hubs SinkをAzure Event Hubsに接続するためのコネクターを作成する必要があります。
>>>>>>> origin/release-6.1

1. EMQXダッシュボードにアクセスし、**Integrations** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで、コネクタータイプとして**Azure Event Hubs**を選択し、**Next**をクリックします。
4. コネクターの名前と説明を入力します。名前は英数字の組み合わせで、例：`my-azure-event-hubs`。
5. 接続情報を設定します。
<<<<<<< HEAD
   - **Bootstrap Host**：Namespace のホスト名を入力します。デフォルトポートは `9093` です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：Namespace の共有アクセスポリシーの「接続文字列 - プライマリキー」を入力します。詳細は[イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**：Azure Event Hub への接続時は TLS がデフォルトで有効です。TLS 接続オプションの詳細は[外部リソースアクセスの TLS 有効化](../network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。

これで、Azure Event Hubs がコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status** が **Connected** となっているはずです。次に、ルールと Sink を作成して、Azure Event Hubs にストリーミングするデータを指定します。
=======
   - **Bootstrap Host**：Namespaceのホスト名を入力します。デフォルトポートは`9093`です。その他の項目は実際の環境に合わせて設定してください。
   - **Connection String**：Namespaceの共有アクセスポリシーの「接続文字列 - プライマリキー」を入力します。詳細は[イベントハブの接続文字列を取得する](https://learn.microsoft.com/en-us/azure/event-hubs/event-hubs-get-connection-string)を参照してください。
   - **Enable TLS**：Azure Event Hub接続時はTLSがデフォルトで有効です。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
6. ページ下部の**Create**ボタンをクリックし、コネクター作成を完了します。

これでAzure Event Hubsがコネクター一覧（**Integration** -> **Connector**）に表示され、**Connection Status**が**Connected**となっているはずです。次に、ルールとSinkを作成し、Azure Event Hubsにストリームするデータを指定します。
>>>>>>> origin/release-6.1

## Azure Event Hubs Sink付きルールの作成

<<<<<<< HEAD
このセクションでは、Azure Event Hubs Sink を追加したルールの作成方法を示します。
=======
このセクションでは、Azure Event Hubs Sinkを追加したルールの作成方法を示します。
>>>>>>> origin/release-6.1

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. 例として、ルールIDに`my_rule`を入力します。

<<<<<<< HEAD
4. **SQL Editor** に以下の文を入力します。これはトピック `t/#` の MQTT メッセージを Azure Event Hubs に保存する例です。

   注意：独自の SQL 文を指定する場合は、Sink が必要とするすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。
=======
4. **SQL Editor**に、トピック`t/#`のMQTTメッセージをAzure Event Hubsに保存する場合、以下のステートメントを入力します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`部分に含めていることを確認してください。
>>>>>>> origin/release-6.1

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

<<<<<<< HEAD
5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Azure Event Hubs` を選択し、EMQX がルールで処理したデータを Azure Event Hubs に送信するようにします。

   **Action** ドロップダウンは `Create Action` のままにするか、既存の Azure Event Hubs アクションを選択できます。この例では新しい Sink を作成してルールに追加します。

6. Sink の名前と説明を **Name** と **Description** テキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-azure-event-hubs` を選択します。新しいコネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink 情報を設定します。
   - **Event Hub Name**：使用する Event Hub の名前を入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートしています。詳細は[Kafka 動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hub にパブリッシュされるメッセージに追加されるヘッダーとして使用するプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。選択肢は `none` または `json` です。
   - **Extra Azure Event Hub headers**：**Add** をクリックして、Azure Event Hubs ヘッダーの追加のキー・バリューペアを指定できます。
   - **Message Key**：Event Hub のメッセージキーを指定します。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力可能です。
   - **Message Value**：Event Hub のメッセージ値を指定します。プレーン文字列またはプレースホルダー（${var}）を含む文字列を入力可能です。
   - **Partition Strategy**：プロデューサーがメッセージを Azure Event Hubs のパーティションに振り分ける方法を指定します。
     - `random`：メッセージごとにランダムにパーティションを選択します。
     - `key_dispatch`：Azure Event Hubs メッセージキーのハッシュ値に基づいてパーティション番号を決定します。
   - **Partitions Limit**：プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のために、1つ以上のフォールバックアクションを定義できます。これらのアクションは、プライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて **sync** または **async** のクエリモードを選択します。詳細は[Sink の機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックして Sink の設定を完了します。**Create Rule** ページの **Action Outputs** タブに新しい Sink が表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示されます。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しい Azure Event Hubs Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Azure Event Hubs に送信・保存されていることがわかります。

## ルールのテスト

Azure Event Hubs データ統合が期待通りに動作するかをテストするには、[MQTTX](https://mqttx.app/) を使ってクライアントをシミュレートし、EMQX に MQTT メッセージをパブリッシュします。

1. MQTTX を使ってトピック `t/1` にメッセージを送信します。
=======
5. **+ Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action**のドロップダウンリストから`Azure Event Hubs`を選択し、EMQXがルールで処理したデータをAzure Event Hubsに送信するようにします。

   **Action**ドロップダウンは`Create Action`のままにするか、既存のAzure Event Hubsアクションを選択できます。ここでは新しいSinkを作成し、ルールに追加します。

6. Sinkの名前と説明を**Name**および**Description**テキストボックスに入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-azure-event-hubs`を選択します。新しいコネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sink情報を設定します。
   - **Event Hub Name**：使用するEvent Hubの名前を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定にも対応しています。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Azure Event Hub Headers**：Azure Event Hubにパブリッシュされるメッセージに追加するヘッダーのプレースホルダーを入力します。
   - **Azure Event Hub Header value encode mode**：ヘッダーの値のエンコードモードを選択します。選択肢は`none`または`json`です。
   - **Extra Azure Event Hub headers**：**Add**をクリックしてAzure Event Hubsヘッダーの追加のキー・バリューを設定できます。
   - **Message Key**：Event Hubのメッセージキーを入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が使用可能です。
   - **Message Value**：Event Hubのメッセージ値を入力します。プレーン文字列またはプレースホルダー（${var}）を含む文字列が使用可能です。
   - **Partition Strategy**：プロデューサーがAzure Event Hubsのパーティションにメッセージを割り当てる方法を指定します。
     - `random`：各メッセージに対してランダムにパーティションを選択します。
     - `key_dispatch`：Event Hubsメッセージキーのハッシュ値をパーティション番号として使用します。
   - **Partitions Limit**：プロデューサーがメッセージを送信できる最大パーティション数を制限します。デフォルトは無効で、すべてのパーティションに送信可能です。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にこれらのアクションがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**ボタンをクリックしてSink設定を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

12. **Create Rule**ページで設定内容を確認し、**Create**ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示されます。

これでルールが正常に作成され、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいAzure Event Hubs Sinkを確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを表示でき、トピック`t/#`のメッセージがルール`my_rule`で解析され、Azure Event Hubsに送信・保存されていることが確認できます。

## ルールのテスト

Azure Event Hubsデータ統合が期待通りに動作するかテストするため、[MQTTX](https://mqttx.app/)を使ってクライアントをシミュレートし、EMQXにMQTTメッセージをパブリッシュします。

1. MQTTXでトピック`t/1`にメッセージを送信します：
>>>>>>> origin/release-6.1

```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Azure Event Hub" }'
```

<<<<<<< HEAD
2. **Rule** ページでルール名をクリックし、統計情報を確認します。Sink の稼働状況をチェックし、新しい送信メッセージが 1 件あることを確認してください。
3. Kafka 互換のコンシューマーを使って、設定した Event Hub にメッセージが書き込まれているか確認します。Kafka CLI の使用方法は[Azure Event Hubs for Apache Kafka エコシステムでの Kafka CLI を使ったメッセージ送受信](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 高度な設定

このセクションでは、コネクターのパフォーマンス最適化や特定シナリオに応じたカスタマイズ操作のための高度な設定オプションを説明します。対応するオブジェクト作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （プロデューサーのみ）有効にすると、クライアントがメタデータ取得要求を送信した際に存在しない Kafka トピックを自動作成します。 | `Disabled`         |
| Connect Timeout                   | TCP 接続確立の最大待機時間（認証有効時は認証時間も含む）       | `5` 秒             |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。これにより、Confluent クラスターなどの接続先リソースが完全に稼働しデータ処理可能になるまで Sink の操作を保留します。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状況をチェックする間隔時間                   | `15` 秒            |
| Health Check Timeout              | Azure Event Hubs との接続に対する自動ヘルスチェックのタイムアウト時間 | `60` 秒            |
| Min Metadata Refresh Interval     | クライアントが Azure Event Hubs Kafka ブローカーおよびトピックのメタデータを更新する際の最小間隔。小さすぎると Kafka サーバーの負荷が増加します。 | `3` 秒             |
| Metadata Request Timeout          | Kafka からメタデータを要求する際の最大待機時間               | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能最適化のためのソケットバッファサイズ管理 | `1` MB             |
| No Delay                          | システムカーネルが TCP ソケットを即時送信するか遅延送信するかを選択。トグルオンで「No Delay」有効、即時送信。オフの場合、送信内容が少ないと最大 40 ミリ秒の遅延が発生する可能性あり。 | `Enabled`          |
| TCP Keepalive                     | Kafka ブリッジ接続の TCP キープアライブ機構を有効化し、長時間の非アクティブ状態による接続切断を防止します。値は `Idle, Interval, Probes` の3つの数値をカンマ区切りで指定します。<br />Idle: 接続がアイドル状態となってからキープアライブプローブを送信開始するまでの秒数（Linux デフォルト 7200 秒）<br />Interval: 各キープアライブプローブ間の秒数（Linux デフォルト 75 秒）<br />Probes: 応答なしと判断するまでの最大プローブ送信回数（Linux デフォルト 9 回）<br />例：`240,30,5,` は、240 秒のアイドル後にプローブ開始、30 秒間隔で送信、5 回応答なしで接続切断と判断。 | `none`             |
=======
2. **Rule**ページでルール名をクリックし、統計情報を確認します。Sinkの稼働状況をチェックし、新しい送信メッセージが1件あることを確認してください。
3. Kafka互換のコンシューマーを使って、設定したEvent Hubにメッセージが書き込まれているか確認します。Kafka CLIの使用方法については、[Azure Event Hubs for Apache Kafka EcosystemでのKafka CLIによるメッセージ送受信](https://github.com/Azure/azure-event-hubs-for-kafka/tree/master/quickstart/kafka-cli)を参照してください。

## 高度な設定

このセクションでは、コネクターのパフォーマンス最適化や特定シナリオに応じたカスタマイズ操作のための高度な設定オプションを説明します。該当オブジェクト作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （プロデューサーのみ）有効にすると、クライアントがメタデータ取得リクエストを送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証有効時は認証時間も含む）          | `5` 秒             |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大時間（秒）。Confluentクラスターなどの接続リソースが完全に稼働しデータ処理可能になるまで待機し、リソース作成要求に応答します。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状況チェック間隔                               | `15` 秒            |
| Health Check Timeout              | Azure Event Hubsとの接続に対する自動ヘルスチェックのタイムアウト時間 | `60` 秒            |
| Min Metadata Refresh Interval     | クライアントがAzure Event Hubs Kafkaブローカーおよびトピックのメタデータを更新する際の最小間隔。短すぎるとKafkaサーバーへの負荷が増加する可能性があります。 | `3` 秒             |
| Metadata Request Timeout          | Kafkaからメタデータを要求する際の最大待機時間                   | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能最適化のためのソケットバッファサイズ管理     | `1` MB             |
| No Delay                          | システムカーネルがTCPソケットを即時送信するか遅延送信するかの設定。トグルをオンにすると「No Delay」が有効となり即時送信されます。オフの場合、送信内容が少量の際に約40ミリ秒の遅延が発生します。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間の非アクティブ状態による接続切断を防止します。値は`Idle, Interval, Probes`の3つの数値をカンマ区切りで指定します。<br />Idle：接続がアイドル状態となってからキープアライブプローブを送信開始するまでの秒数（Linuxデフォルトは7200秒）。<br />Interval：各キープアライブプローブ間の秒数（Linuxデフォルトは75秒）。<br />Probes：応答なしと判断するまでの最大プローブ送信回数（Linuxデフォルトは9回）。<br />例：`240,30,5,`は240秒のアイドル後にプローブ開始、30秒間隔で送信し、5回連続応答なしで接続切断と判断します。 | `none`             |
>>>>>>> origin/release-6.1
