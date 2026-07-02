# GCP Pub/Sub に MQTT データを取り込む

[Google Cloud Pub/Sub](https://cloud.google.com/pubsub?hl=en-us) は、非常に高い信頼性とスケーラビリティを実現するために設計された非同期メッセージングサービスです。EMQX は、MQTT データのリアルタイム抽出、処理、分析のために Google Cloud Pub/Sub とのシームレスな統合をサポートしています。Cloud Functions、App Engine、Cloud Run、Kubernetes Engine、Compute Engine などのさまざまな Google Cloud サービスにデータをプッシュできます。また、Google Cloud から MQTT へのデータ配信も可能であり、ユーザーは GCP 上で迅速に IoT アプリケーションを構築できます。

本ページでは、EMQX と GCP Pub/Sub 間のデータ統合について、作成および検証の実践的な手順を含めて包括的に紹介します。

## 動作概要

GCP Pub/Sub データ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Google Cloud とシームレスに連携させ、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう支援します。

![GCP_bridge_architect](./assets/gcp_pubsub/GCP_bridge_architect.png)

EMQX はルールエンジンと Sink を介して MQTT データを GCP Pub/Sub に転送します。ここでは GCP Pub/Sub のプロデューサー役割の例を示します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のトピックにマッチする MQTT メッセージを処理します。ルールエンジンは対応するルールをマッチさせ、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **GCP Pub/Sub へのブリッジング**：ルールはメッセージを GCP Pub/Sub に転送するアクションをトリガーし、データプロパティ、オーダーキー、MQTT トピックと GCP Pub/Sub トピックのマッピングを簡単に設定できます。これにより、より豊富なコンテキスト情報と順序保証を伴うデータ統合が可能となり、柔軟な IoT データ処理を実現します。

MQTT メッセージデータが GCP Pub/Sub に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：Dataflow、BigQuery、Pub/Sub のストリーミング機能など、強力な Google Cloud のデータ処理・分析ツールを活用し、メッセージデータのリアルタイム処理と分析を行い、価値あるインサイトや意思決定支援を得られます。
- イベント駆動型機能：Cloud Functions や Cloud Run などの Google Cloud のイベント処理をトリガーし、動的かつ柔軟な機能の起動と処理を実現します。
- データ保存と共有：Cloud Storage や Firestore などの Google Cloud ストレージサービスにメッセージデータを送信し、大量データの安全な保存と管理を行います。これにより他の Google Cloud サービスと連携してデータの共有や分析が可能となり、多様なビジネスニーズに対応します。

## 特長とメリット

GCP Pub/Sub とのデータ統合は、以下のような特長とメリットを提供します。

- **堅牢なメッセージングサービス**：EMQX と GCP Pub/Sub は共に高可用性とスケーラビリティを備え、大規模なメッセージストリームの受信、配信、処理を確実に行います。IoT データの順序保証、メッセージの品質保証、パーシステンスをサポートし、信頼性の高いメッセージ伝送と処理を実現します。
- **柔軟なルールエンジン**：組み込みのルールエンジンにより、特定の送信元メッセージやイベントをトピックマッチングに基づいて処理できます。メッセージのデータ形式変換、特定情報のフィルタリング、コンテキスト情報の付加などが可能です。これと GCP Pub/Sub を組み合わせることで、さらに高度な処理と分析が可能となります。
- **豊富なコンテキスト情報**：GCP Pub/Sub データ統合を通じて、メッセージにより豊富なコンテキスト情報を付加できます。クライアント属性を Pub/Sub 属性にマッピングしたり、ソートキーを設定したりすることで、後続のアプリケーション開発やデータ処理においてより精密な分析と処理を支援します。

まとめると、EMQX と GCP Pub/Sub の統合により、高信頼かつスケーラブルなメッセージ配信と、データ分析・統合のための豊富なツールとサービスを活用可能となります。これにより、堅牢な IoT アプリケーションの構築やイベント駆動型の柔軟なビジネスロジックの実装が実現できます。

## はじめる前に

本節では、GCP Pub/Sub データ統合の作成を開始する前に完了すべき準備について説明します。

### 前提条件

- EMQX のデータ統合に関する [ルール](./rules.md) の知識
- [データ統合](./data-bridges.md) に関する知識

### GCP でサービスアカウントキーを作成する

GCP Pub/Sub サービスを利用するには、サービスアカウントとサービスアカウントキーを作成する必要があります。

1. GCP アカウントで [サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount) を作成します。サービスアカウントには、対象トピックへのメッセージの検査／読み取りおよびパブリッシュ権限（例：Pub/Sub Editor ロール）が付与されていることを確認してください。

2. 作成したサービスアカウントのメールアドレスをクリックし、**Key** タブを開きます。**Add key** のドロップダウンリストから **Create new key** を選択し、そのアカウントのサービスアカウントキーを JSON 形式で作成・ダウンロードします。

   ::: tip

   サービスアカウントキーは後で使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCP でトピックを作成・管理する

EMQX で GCP Pub/Sub データ統合を設定する前に、トピックを作成し、GCP での基本的な管理操作に慣れておく必要があります。

1. Google Cloud コンソールで **Pub/Sub** -> **Topics** ページに移動します。詳細な手順は [トピックの作成と管理](https://cloud.google.com/pubsub/docs/create-topic) を参照してください。

   ::: tip

   サービスアカウントには、そのトピックへのパブリッシュ権限が必要です。

   :::

2. **Topic ID** フィールドにトピックの ID を入力し、**Create topic** をクリックします。

   <img src="./assets/gcp_pubsub/create-topic-GCP-console.png" alt="GCP コンソールでのトピック作成" style="zoom:50%;" />

3. **Subscriptions** ページに移動し、リストの中から作成したトピックの **Topic ID** をクリックします。トピックに対してサブスクリプションを作成します。

   - **Delivery type** で **Pull** を選択します。
   - **Message retention duration** は `7` 日を選択します。

   詳細は [GCP Pub/Sub サブスクリプション](https://cloud.google.com/pubsub/docs/subscriber) を参照してください。

   <img src="./assets/gcp_pubsub/add-subscription-to-topic.png" alt="トピックへのサブスクリプション追加" style="zoom:50%;" />

4. **Subscription ID** をクリックし、**Messages** -> **Pull** を選択すると、そのトピックに送信されたメッセージを確認できます。

   <img src="./assets/gcp_pubsub/subscriptions-id.png" alt="サブスクリプションID" style="zoom:50%;" />

   <img src="./assets/gcp_pubsub/subscriptions-id-pull.png" alt="サブスクリプションIDのメッセージプル" style="zoom:50%;" />

## GCP Pub/Sub プロデューサーコネクターを作成する

GCP Pub/Sub プロデューサー Sink アクションを追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するための GCP Pub/Sub プロデューサーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Producer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubproducer`）。この名前は GCP Pub/Sub プロデューサー Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **GCP Service Account Credentials** にて、[GCP でサービスアカウントキーを作成する](#gcp-でサービスアカウントキーを作成する) でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして Sink を指定するルールの作成を続行できます。詳細は [GCP Pub/Sub プロデューサー Sink を使ったルール作成](#create-a-rule-with-gcp-pub-sub-producer-sink) を参照してください。

## GCP Pub/Sub プロデューサー Sink を使ったルールを作成する

本節では、GCP Pub/Sub に保存するデータを指定するルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` を入力します。

4. **SQL Editor** でルールを設定します。たとえば、トピック `/devices/+/events` の MQTT メッセージを GCP Pub/Sub に保存したい場合は、以下の SQL 文を使用します。

   注意：独自の SQL 文を指定する場合は、Sink のペイロードテンプレートで必要なすべてのフィールドを `SELECT` 部分に含めるようにしてください。

   ```sql
   SELECT
     *
   FROM
     "/devices/+/events"
   ```

   注意：初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールを学習・テストできます。

5. **+ Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。**Type of Action** のドロップダウンリストから `Google PubSub Producer` を選択すると、EMQX はルールで処理されたデータを GCP Pub/Sub に送信します。

6. **Action** ドロップダウンボックスは `Create Action` のままにするか、以前に作成した GCP Pub/Sub プロデューサー Sink を選択できます。この例では新しい Sink を作成してルールに追加します。

7. **Name** フィールドに Sink の名前を入力します。名前は英数字の組み合わせで指定してください。

8. **Connector** ドロップダウンボックスから先ほど作成した `my_pubsubprodcer` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメーターについては [コネクターの作成](#create-a-connector) を参照してください。

9. **GCP PubSub Topic** に、[GCP でトピックを作成・管理する](#gcp-でトピックを作成・管理する) で作成したトピック ID `my-iot-core` を入力します。

10. **Payload Template** にテンプレートを定義するか、空欄のままにします。

    - 空欄の場合、クライアントID、トピック、ペイロードなど MQTT メッセージのすべての可視入力を JSON 形式でエンコードします。
    - 定義済みテンプレートを使用する場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応する値で置き換えられます。例：`${topic}` は MQTT メッセージのトピックが `my/topic` なら `my/topic` に置換されます。

11. **Attributes Template** および **Ordering Key Template** にて、送信メッセージの属性やオーダーキーのフォーマットテンプレートを定義します（任意）。

    - **Attributes** では、キーと値の両方に `${variable_name}` 形式のプレースホルダーを使用可能で、MQTT コンテキストから値を抽出します。キーのテンプレートが空文字列に解決された場合、そのキーは GCP Pub/Sub 送信メッセージから省略されます。
    - **Ordering Key** も `${variable_name}` 形式のプレースホルダーを使用可能で、空文字列に解決された場合は GCP Pub/Sub 送信メッセージの `orderingKey` フィールドは設定されません。

12. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

13. **詳細設定（任意）**：詳細は [Sink の機能](./data-bridges.md#features-of-sink) を参照してください。

14. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。

15. **Create** ボタンをクリックして Sink の設定を完了すると、新しい Sink が **Action Outputs** タブに表示されます。

16. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成します。

これでルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Google PubSub プロデューサー Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `/devices/+/events` のメッセージがルール `my_rule` によって解析され、GCP Pub/Sub に送信・保存されていることが視覚的に確認できます。

## プロデューサールールのテスト

1. MQTTX を使ってトピック `/devices/+/events` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t /devices/+/events -m '{ "msg": "hello GCP PubSub" }'
   ```

2. Sink の稼働状況を確認し、新規の受信メッセージと送信メッセージがそれぞれ 1 件あることを確認します。

3. GCP の **Pub/Sub** -> **Subscriptions** に移動し、**MESSAGES** タブをクリックするとメッセージが確認できます。

## GCP Pub/Sub コンシューマーコネクターを作成する

GCP Pub/Sub コンシューマー Sink を追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するための GCP Pub/Sub コンシューマーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Consumer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubconsumer`）。この名前は GCP Pub/Sub コンシューマー Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **GCP Service Account Credentials** にて、[GCP でサービスアカウントキーを作成する](#gcp-でサービスアカウントキーを作成する) でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして GCP Pub/Sub コンシューマーソースを使ったルールの作成を続行できます。詳細は [GCP Pub/Sub コンシューマーソースを使ったルール作成](#create-a-rule-with-gcp-pub-sub-cconsumer-source) を参照してください。

## GCP Pub/Sub コンシューマーソースを使ったルールを作成する

本節では、GCP Pub/Sub からのメッセージを消費し、EMQX に転送するルールの作成方法を示します。Google PubSub コンシューマーソースを作成・設定し、ルールのデータ入力として追加します。また、メッセージを EMQX に転送するために Republish アクションをルールに追加します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule_source` を入力します。

4. 右側の **Data Inputs** タブで、デフォルトの入力 `Messages` を削除し、**Add Input** をクリックします。

5. **Input Type** のドロップダウンから `Google PubSub Consumer` を選択します。

6. **Source** ドロップダウンはデフォルトのまま `Create Source` を選択します。この例では新しいソースを作成し、ルールに追加します。

7. ソースの **Name** と（任意で）**Description** を入力します。名前は英数字の組み合わせで、例：`my-gcppubsub-source`。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubconsumer` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメーターについては [コネクターの作成](#create-a-connector) を参照してください。

9. GCP Pub/Sub から EMQX にメッセージを消費するための以下の情報を設定します。

   - **GCP PubSub Topic**：消費対象の GCP Pub/Sub トピック名を入力します（例：`my-iot-core`）。
   - **Maximum Messages to Pull**：1 回のプル要求で GCP Pub/Sub から取得する最大メッセージ数を指定します。実際の取得数は指定値より少ない場合があります。

10. 詳細設定（任意）：詳細は [Sink の機能](./data-bridges.md#features-of-sink) を参照してください。

11. **Create** をクリックする前に、**Test Connectivity** をクリックして GCP Pub/Sub サーバーへの接続が成功するかテストできます。

12. **Create** をクリックしてソースの作成を完了します。ソースはルールの **Data Inputs** タブに追加され、**SQL Editor** のルールは以下のようになります。

    ```sql
    SELECT
      *
    FROM
      "$bridges/gcppubsub:my-gcppubsub-source"
    ```

    注意：初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールを学習・テストできます。

    `my-gcppubsub-source` からのルール SQL は、以下の GCP Pub/Sub から MQTT トピックへのマッピング表に示す GCP Pub/Sub メッセージフィールドにアクセスできます。ルール SQL を調整してデータ処理を行うことが可能です。この例ではデフォルトの SQL を使用します。

    | フィールド名           | 説明                                                        |
    | ---------------------- | ----------------------------------------------------------- |
    | `attributes`           | （任意）文字列のキー・バリューのペアを含むオブジェクト（存在する場合） |
    | `message_id`           | GCP Pub/Sub がこのメッセージに割り当てたメッセージ ID       |
    | `ordering_key`         | （任意）メッセージの順序付けキー（存在する場合）             |
    | `publishing_time`      | GCP Pub/Sub によって定義されたメッセージのタイムスタンプ     |
    | `topic`                | 発信元の GCP Pub/Sub トピック                               |
    | `value`                | （任意）メッセージのペイロード（存在する場合）               |

    **注意**：各 GCP Pub/Sub から MQTT トピックへのマッピングは、ユニークな GCP Pub/Sub トピック名を含む必要があります。つまり、同一の GCP Pub/Sub トピックが複数のマッピングに存在してはなりません。

これで GCP Pub/Sub コンシューマーソースの作成は完了しましたが、メッセージはまだ直接 EMQX にパブリッシュされません。次に、[ルールに Republish アクションを追加する](#add-republish-action-to-the-rule) の手順を続けて、Republish アクションを作成しルールに追加してください。

### ルールに Republish アクションを追加する

本節では、GCP Pub/Sub コンシューマーソースから消費したメッセージを転送し、EMQX トピック `t/1` にパブリッシュするためにルールに Republish アクションを追加する方法を示します。

1. ページ右側の **Action Output** タブを選択し、**Add Action** ボタンをクリックして、**Type of Action** ドロップダウンリストから `Republish` アクションを選択します。

2. メッセージの再パブリッシュ設定を入力します。

   - **Topic**：MQTT にパブリッシュするトピックを入力します。ここでは `t/1` とします。

   - **QoS**：`0`、`1`、`2`、`${qos}` のいずれかを選択、または他のフィールドから QoS を設定するためのプレースホルダーを入力します。`${qos}` を選択すると元のメッセージの QoS に従います。

   - **Retain**：`true` または `false` を選択します。メッセージをリテインメッセージとしてパブリッシュするかどうかを決定します。プレースホルダーを使って他のフィールドからリテインフラグを設定することも可能です。この例では `false` を選択します。

   - **Payload**：転送するメッセージペイロードのテンプレートを設定します。空欄の場合はルールの出力結果をそのまま転送します。ここでは `${payload}` と入力してペイロードのみを転送することを示します。

     MQTT ペイロードテンプレートのデフォルト値は `${.}` で、すべての利用可能なデータを JSON オブジェクトとしてエンコードします。例えば、すべての任意フィールドを含む GCP Pub/Sub メッセージに対して `${.}` をテンプレートに選択すると、以下のような JSON が生成されます。

     ```json
     {
       "attributes": {"attribute_key": "attribute_value"},
       "message_id": "1679665968238",
       "ordering_key": "my-ordering-key",
       "topic": "my-pubsub-topic",
       "publishing_time": "2023-08-18T14:15:18.470Z",
       "value": "my payload"
     }
     ```

     GCP Pub/Sub メッセージのサブフィールドはドット表記でアクセス可能です。例えば `${.value}` は GCP Pub/Sub メッセージの値に解決され、`${.attributes.h1}` は存在すれば `h1` メッセージ属性キーの値に解決されます。存在しない値は空文字列に置換されます。

   - **MQTT 5.0 メッセージプロパティ**：デフォルトで無効です。詳細設定は [Republish アクションの追加](./rule-get-started.md#add-republish-action) を参照してください。

3. **Create** をクリックしてアクションの作成を完了します。作成に成功するとルール作成ページに戻り、Republish アクションが **Action Outputs** タブに追加されます。

4. ルール作成ページで **Create** ボタンをクリックしてルール全体の作成を完了します。

これでルールの作成が完了しました。**Rules** ページで新規作成したルールを確認できます。**Sources** タブでは新規作成した GCP Pub/Sub コンシューマーソースが表示されます。

また、**Integrate** -> **Flow Designer** をクリックするとトポロジーを確認でき、GCP Pub/Sub コンシューマーソースからのメッセージが Republish を経由して `t/1` にパブリッシュされる様子を直感的に把握できます。

## <!--Test the Consumer Rule-->
