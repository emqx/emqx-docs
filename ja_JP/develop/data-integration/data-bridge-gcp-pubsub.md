# GCP Pub/Sub への MQTT データ取り込み

[Google Cloud Pub/Sub](https://cloud.google.com/pubsub?hl=en-us) は、非常に高い信頼性とスケーラビリティを実現する非同期メッセージングサービスです。EMQX は Google Cloud Pub/Sub とシームレスに統合し、MQTT データのリアルタイム抽出、処理、分析をサポートします。Cloud Functions、App Engine、Cloud Run、Kubernetes Engine、Compute Engine などのさまざまな Google Cloud サービスへデータをプッシュできます。また、Google Cloud から MQTT へのデータ配信も可能で、ユーザーが GCP 上で迅速に IoT アプリケーションを構築できるよう支援します。

本ページでは、EMQX と GCP Pub/Sub 間のデータ統合について包括的に紹介し、データ統合の作成と検証に関する実践的な手順を提供します。

## 動作概要

GCP Pub/Sub データ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Google Cloud とシームレスに連携させ、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう設計されています。

![GCP_bridge_architect](./assets/gcp_pubsub/GCP_bridge_architect.png)

EMQX はルールエンジンと Sink を通じて MQTT データを GCP Pub/Sub に転送します。GCP Pub/Sub のプロデューサー役割の例を挙げると、全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは特定のトピックに基づいて MQTT メッセージを処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報の付加などを行います。
3. **GCP Pub/Sub へのブリッジング**：ルールはメッセージを GCP Pub/Sub に転送するアクションをトリガーします。データプロパティ、オーダーキー、MQTT トピックと GCP Pub/Sub トピックのマッピングを簡単に設定でき、より豊富なコンテキスト情報と順序保証を提供し、柔軟な IoT データ処理を可能にします。

MQTT メッセージデータが GCP Pub/Sub に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：Dataflow、BigQuery、Pub/Sub のストリーミング機能などの強力な Google Cloud データ処理・分析ツールを活用し、メッセージデータのリアルタイム処理と分析を行い、価値ある洞察や意思決定支援を得られます。
- イベント駆動型機能：Cloud Functions や Cloud Run などの Google Cloud イベント処理をトリガーし、動的かつ柔軟な関数の起動と処理を実現します。
- データ保存と共有：Cloud Storage や Firestore などの Google Cloud ストレージサービスにメッセージデータを送信し、大量データの安全な保存と管理を行います。これにより他の Google Cloud サービスとデータを共有・分析し、多様なビジネスニーズに対応できます。

## 特長と利点

GCP Pub/Sub とのデータ統合は以下の特長と利点を提供します。

- **堅牢なメッセージングサービス**：EMQX と GCP Pub/Sub は共に高可用性とスケーラビリティを備え、大規模なメッセージストリームの信頼性の高い受信、配信、処理を保証します。IoT データの順序管理、メッセージ品質保証、パーシステンスをサポートし、メッセージの確実な伝送と処理を実現します。
- **柔軟なルールエンジン**：組み込みのルールエンジンにより、特定のソースメッセージやイベントをトピックマッチングに基づいて処理できます。メッセージのデータ形式変換、特定情報のフィルタリング、コンテキスト情報の付加などが可能です。これと GCP Pub/Sub を組み合わせることで、さらなる処理と分析が可能になります。
- **豊富なコンテキスト情報**：GCP Pub/Sub データ統合を通じて、メッセージにより豊かなコンテキスト情報を付加できます。クライアント属性を Pub/Sub 属性やソートキーにマッピングすることで、その後のアプリケーション開発やデータ処理においてより精密な分析と処理を支援します。

まとめると、EMQX と GCP Pub/Sub の統合により、高信頼性かつスケーラブルなメッセージ配信が可能となり、データ分析や統合のための豊富なツールとサービスを活用できます。これにより、堅牢な IoT アプリケーションの構築やイベント駆動型の柔軟なビジネスロジックの実装が実現します。

## はじめる前に

このセクションでは、GCP Pub/Sub データ統合の作成を開始する前に完了すべき準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GCP でサービスアカウントキーを作成する

GCP PubSub サービスを利用するには、サービスアカウントとサービスアカウントキーを作成する必要があります。

1. GCP アカウントで[サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)を作成します。サービスアカウントには対象トピックへのメッセージの検査/読み取りおよびパブリッシュ権限（例：Pub/Sub Editor ロール）が必要です。

2. 作成したサービスアカウントのメールアドレスをクリックし、**Key** タブを開きます。**Add key** のドロップダウンリストから **Create new key** を選択し、そのアカウントのサービスアカウントキーを JSON 形式で作成・ダウンロードします。

   ::: tip

   サービスアカウントキーは後で使用するため安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCP でトピックを作成・管理する

EMQX で GCP Pub/Sub データ統合を設定する前に、トピックを作成し、GCP での基本的な管理操作に慣れておく必要があります。

1. Google Cloud コンソールで **Pub/Sub** -> **Topics** ページに移動します。詳細な手順は[トピックの作成と管理](https://cloud.google.com/pubsub/docs/create-topic)を参照してください。

   ::: tip

   サービスアカウントにはそのトピックへのパブリッシュ権限が必要です。

   :::

2. **Topic ID** フィールドにトピックの ID を入力し、**Create topic** をクリックします。

   <img src="./assets/gcp_pubsub/create-topic-GCP-console.png" alt="GCP コンソールでのトピック作成" style="zoom:50%;" />

3. **Subscriptions** ページに移動し、リスト内の **Topic ID** をクリックして、そのトピックにサブスクリプションを作成します。

   - **Delivery type** で **Pull** を選択します。
   - **Message retention duration** は `7` 日を選択します。

   詳細は[GCP Pub/Sub サブスクリプション](https://cloud.google.com/pubsub/docs/subscriber)を参照してください。

   <img src="./assets/gcp_pubsub/add-subscription-to-topic.png" alt="トピックへのサブスクリプション追加" style="zoom:50%;" />

4. **Subscription ID** -> **Messages** -> **Pull** をクリックすると、トピックに送信されたメッセージを確認できます。

   <img src="./assets/gcp_pubsub/subscriptions-id.png" alt="サブスクリプションID" style="zoom:50%;" />

   <img src="./assets/gcp_pubsub/subscriptions-id-pull.png" alt="サブスクリプションIDのメッセージプル" style="zoom:50%;" />

## GCP Pub/Sub プロデューサーコネクターを作成する

GCP Pub/Sub プロデューサー Sink アクションを追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するための GCP Pub/Sub プロデューサーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. 画面右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Producer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubproducer`）。名前は GCP Pub/Sub プロデューサー Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **GCP Service Account Credentials** にて、[GCP でサービスアカウントキーを作成する](#gcp-でサービスアカウントキーを作成する)でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
5. **Advanced Settings** を展開し、必要に応じてオプション設定を行います。詳細は[コネクターの高度な設定](#connector-advanced-settings)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
7. 画面下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** をクリックして、Sink を指定して GCP Pub/Sub に転送するデータを設定するルール作成に進めます。詳細は[Create a Rule with GCP Pub/Sub Producer Sink](#create-a-rule-with-gcp-pubsub-producer-sink)を参照してください。

## GCP Pub/Sub プロデューサー Sink を用いたルールを作成する

このセクションでは、GCP Pub/Sub に保存するデータを指定するルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. 画面右上の **Create** をクリックします。

3. ルール ID に `my_rule` を入力します。

4. **SQL Editor** でルールを設定します。例えば、トピック `/devices/+/events` の MQTT メッセージを GCP Pub/Sub に保存したい場合、以下の SQL 文を使用します。

   注意：独自の SQL 文を指定する場合、Sink のペイロードテンプレートで必要なすべてのフィールドを `SELECT` 部分に含める必要があります。

   ```sql
   SELECT
     *
   FROM
     "/devices/+/events"
   ```

   注意：初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールの学習とテストを行えます。

5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action** のドロップダウンリストから `Google PubSub Producer` を選択し、EMQX がルールで処理したデータを GCP Pub/Sub に送信するようにします。

6. **Action** ドロップダウンは `Create Action` のままにします。既存の GCP Pub/Sub プロデューサー Sink を選択することも可能です。この例では新しい Sink を作成してルールに追加します。

7. **Name** フィールドに Sink の名前を入力します。名前は英数字の組み合わせにしてください。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubprodcer` を選択します。隣のボタンで新しいコネクターを作成することも可能です。設定パラメーターは[Create a Connector](#create-a-connector)を参照してください。

9. **GCP PubSub Topic** に以下のいずれかの値を入力します。

   - [Create and Manage Topics in GCP](#create-and-manage-topics-in-gcp) で作成したトピック名（例：`my-iot-core`）。EMQX は設定されたサービスアカウントに関連付けられたプロジェクト内でトピックを解決します。
   - 完全修飾トピックパス（例：`projects/<project-id>/topics/<topic-name>`）。異なる GCP プロジェクトのトピックにメッセージをパブリッシュする場合に使用します。そのプロジェクトのトピックに対してサービスアカウントに必要な Pub/Sub 権限を付与してください。

10. **Payload Template** にテンプレートを定義するか空欄のままにします。

    - 空欄の場合、クライアントID、トピック、ペイロードなど MQTT メッセージのすべての可視入力を JSON 形式でエンコードします。
    - 定義したテンプレートを使用する場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応値で置換されます。例：`${topic}` は MQTT メッセージのトピック `my/topic` に置き換わります。

11. **Attributes Template** と **Ordering Key Template** で送信メッセージの属性やオーダーキーのフォーマットテンプレートを定義します（任意）。

    - **Attributes** はキーと値の両方に `${variable_name}` 形式のプレースホルダーを使用でき、MQTT コンテキストから値を抽出します。キーのテンプレートが空文字列に解決された場合、そのキーは GCP Pub/Sub 送信メッセージから省略されます。
    - **Ordering Key** は `${variable_name}` 形式のプレースホルダーを使用できます。解決値が空文字列の場合、GCP Pub/Sub 送信メッセージの `orderingKey` フィールドは設定されません。

12. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

13. **Advanced Settings** を展開し、必要に応じてオプション設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照してください。

14. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。

15. **Create** ボタンをクリックして Sink の設定を完了すると、新しい Sink が **Action Outputs** タブに表示されます。

16. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成します。

これでルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると新しい Google PubSub Producer Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックしてトポロジーを確認すると、トピック `/devices/+/events` のメッセージがルール `my_rule` によって解析され、GCP Pub/Sub に送信・保存されていることが視覚的に確認できます。

## プロデューサールールのテスト

1. MQTTX を使ってトピック `/devices/+/events` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t /devices/+/events -m '{ "msg": "hello GCP PubSub" }'
   ```

2. Sink の稼働状況を確認し、新しい受信メッセージと送信メッセージがそれぞれ1件ずつあることを確認します。

3. GCP の **Pub/Sub** -> **Subscriptions** に移動し、**MESSAGES** タブをクリックするとメッセージが確認できます。

## GCP Pub/Sub コンシューマーコネクターを作成する

GCP Pub/Sub コンシューマー Source を追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するための GCP Pub/Sub コンシューマーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. 画面右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Consumer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubconsumer`）。名前は GCP Pub/Sub コンシューマー Source とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **GCP Service Account Credentials** にて、[GCP でサービスアカウントキーを作成する](#gcp-でサービスアカウントキーを作成する)でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
5. **Advanced Settings** を展開し、必要に応じてオプション設定を行います。詳細は[コネクターの高度な設定](#connector-advanced-settings)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
7. 画面下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** をクリックして、GCP Pub/Sub コンシューマー Source を使って GCP Pub/Sub からデータを取得し EMQX に転送するルール作成に進めます。詳細は[Create a Rule with GCP Pub/Sub Consumer Source](#create-a-rule-with-gcp-pubsub-consumer-source)を参照してください。

## GCP Pub/Sub コンシューマー Source を用いたルールを作成する

このセクションでは、GCP Pub/Sub からのメッセージを消費し EMQX に転送するルールの作成方法を説明します。Google PubSub Consumer Source を作成・設定し、ルールのデータ入力として追加します。また、Republish アクションをルールに追加して、GCP Pub/Sub から受信したメッセージを EMQX に転送します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. 画面右上の **Create** をクリックします。

3. ルール ID に `my_rule_source` を入力します。

4. 右側の **Data Inputs** タブで、デフォルトの Input `Messages` を削除し、**Add Input** をクリックします。

5. **Input Type** のドロップダウンから `Google PubSub Consumer` を選択します。

6. **Source** ドロップダウンはデフォルトの `Create Source` のままにします。この例では新しい Source を作成してルールに追加します。

7. Source の **Name** と（任意の）**Description** を入力します。名前は英数字の組み合わせにしてください（例：`my-gcppubsub-source`）。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubconsumer` を選択します。隣のボタンで新しいコネクターを作成することも可能です。設定パラメーターは[Create a Connector](#create-a-connector)を参照してください。

9. GCP Pub/Sub から EMQX へメッセージを消費するために、以下の情報を設定します。

   - **GCP PubSub Topic**：トピック名（例：`my-iot-core`）または完全修飾トピックパス（例：`projects/<project-id>/topics/<topic-name>`）を入力します。トピック名は設定されたサービスアカウントに関連付けられたプロジェクト内で解決されます。異なる GCP プロジェクトのトピックから消費する場合は完全修飾パスを入力し、サービスアカウントにそのトピックへの必要な Pub/Sub 権限を付与してください。コンシューマーサブスクリプションはサービスアカウントのプロジェクトに作成され、トピック参照のみ他プロジェクトを指します。
   - **Maximum Messages to Pull**：1 回のプルリクエストで GCP PubSub から取得する最大メッセージ数を指定します。実際の取得数は指定値未満の場合があります。

10. **Advanced Settings** を展開し、必要に応じてオプション設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照してください。

11. **Create** をクリックする前に、**Test Connectivity** をクリックして GCP Pub/Sub サーバーへの接続が成功するかテストできます。

12. **Create** をクリックして Source の作成を完了します。Source はルールの **Data Inputs** タブに追加され、**SQL Editor** のルールは以下のようになります。

    ```sql
    SELECT
      *
    FROM
      "$bridges/gcppubsub:my-gcppubsub-source"
    ```

    注意：初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールの学習とテストを行えます。

    `my-gcppubsub-source` からのルール SQL は、以下の GCP Pub/Sub から MQTT トピックへのマッピングテーブルに示す GCP Pub/Sub メッセージフィールドにアクセスできます。データ処理のためにルール SQL を調整可能です。この例ではデフォルトの SQL を使用します。

    | フィールド名          | 説明                                                         |
    | --------------------- | ------------------------------------------------------------ |
    | `attributes`          | （任意）文字列のキーと値のペアを含むオブジェクト（存在する場合） |
    | `message_id`          | GCP Pub/Sub がこのメッセージに割り当てたメッセージ ID       |
    | `ordering_key`        | （任意）メッセージの順序付けキー（存在する場合）             |
    | `publishing_time`     | GCP Pub/Sub によって定義されたメッセージのタイムスタンプ     |
    | `topic`               | 発信元の GCP Pub/Sub トピック                                |
    | `value`               | （任意）メッセージペイロード（存在する場合）                 |

    **注意**：各 GCP Pub/Sub から MQTT トピックへのマッピングは一意の GCP Pub/Sub トピック名を含む必要があります。つまり、同じ GCP Pub/Sub トピックが複数のマッピングに存在してはなりません。

GCP Pub/Sub コンシューマー Source の作成は完了しましたが、メッセージはまだ直接 EMQX にパブリッシュされません。次に、[Add Republish Action to the Rule](#add-republish-action-to-the-rule) の手順に従い、Republish アクションを作成してルールに追加してください。

### ルールに Republish アクションを追加する

このセクションでは、GCP Pub/Sub コンシューマー Source から消費したメッセージを転送し、EMQX トピック `t/1` にパブリッシュするための Republish アクションの追加方法を説明します。

1. 画面右側の **Action Output** タブを選択し、**Add Action** ボタンをクリックして、**Type of Action** ドロップダウンリストから `Republish` アクションを選択します。

2. メッセージ再パブリッシュの設定を入力します。

   - **Topic**：MQTT にパブリッシュするトピックを入力します。ここでは `t/1` とします。

   - **QoS**：`0`、`1`、`2`、`${qos}` のいずれかを選択、または他のフィールドから QoS を設定するためのプレースホルダーを入力します。`${qos}` を選択すると元のメッセージの QoS に従います。

   - **Retain**：`true` または `false` を選択します。メッセージをリテインメッセージとしてパブリッシュするかどうかを決定します。プレースホルダーを入力して他のフィールドからリテインフラグを設定することも可能です。この例では `false` を選択します。

   - **Payload**：転送するメッセージペイロードのテンプレートを設定します。空欄の場合はルールの出力結果を転送します。`${.value}` と入力すると GCP Pub/Sub メッセージのペイロードのみを転送します。

     MQTT ペイロードテンプレートのデフォルト値は `${.}` で、利用可能なすべてのデータを JSON オブジェクトとして含みます。例えば、すべてのオプションフィールドを含む GCP Pub/Sub メッセージに対して `${.}` をテンプレートに選択すると、以下のようになります。

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

     GCP Pub/Sub メッセージのサブフィールドはドット表記でアクセス可能です。例：`${.value}` は GCP Pub/Sub メッセージの値に解決され、`${.attributes.h1}` は `h1` メッセージ属性キーの値に解決されます（存在する場合）。値が存在しない場合は空文字列に置換されます。

   - **MQTT 5.0 メッセージプロパティ**：デフォルトで無効です。詳細設定は[Republish アクションの追加](./rule-get-started.md#add-republish-action)を参照してください。

3. **Create** をクリックしてアクションの作成を完了します。作成成功後、ルール作成ページに戻り、Republish アクションが **Action Outputs** タブに追加されます。

4. ルール作成ページで **Create** ボタンをクリックし、ルール全体の作成を完了します。

これでルールの作成が完了しました。**Rules** ページで新規作成したルールを確認できます。**Sources** タブで新しい GCP Pub/Sub コンシューマー Source を確認できます。

また、**Integration** -> **Flow Designer** をクリックしてトポロジーを確認すると、GCP Pub/Sub コンシューマー Source からのメッセージが Republish を経てトピック `t/1` にパブリッシュされる様子が直感的に把握できます。

## GCP Pub/Sub コンシューマールールのテスト

以下の手順で、GCP Pub/Sub コンシューマー Source が GCP Pub/Sub からメッセージを消費し、EMQX の MQTT トピック `t/1` に再パブリッシュする動作を検証します。

1. MQTTX CLI を使って EMQX の MQTT トピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. Google Cloud コンソールで **Pub/Sub** -> **Topics** に移動し、`my-iot-core` トピックをクリックして以下のメッセージをパブリッシュします。

   ```json
   {"msg":"hello GCP PubSub"}
   ```

3. MQTTX がトピック `t/1` で以下のメッセージを受信することを確認します。

   ```text
   topic: t/1
   payload: {"msg":"hello GCP PubSub"}
   ```

## 高度な設定

このセクションでは、GCP Pub/Sub コネクター、プロデューサー Sink、およびコンシューマー Source の高度な設定について説明します。

### コネクターの高度な設定

GCP Pub/Sub プロデューサーおよびコンシューマーコネクターは同じ高度な設定を使用します。

| フィールド名               | 説明                                                                                 | デフォルト値    |
| -------------------------- | ------------------------------------------------------------------------------------ | -------------- |
| **HTTP Pipelining**        | 各レスポンスを待たずに送信できる最大 HTTP リクエスト数。`1` に設定すると次のリクエスト送信前にレスポンスを待ちます。 | `100`          |
| **Connection Pool Size**   | コネクションプールで維持する接続数。                                                 | `8`            |
| **Connect Timeout**        | HTTP 接続確立の最大待機時間。                                                        | `15` 秒        |
| **Max Inactive**           | HTTP クライアントが再接続を試みるまでの最大無活動時間。                              | `10` 秒        |
| **Max Retries**            | リクエスト送信時にエラー発生後の最大リトライ回数。                                  | `2`            |
| **Start Timeout**          | コネクター作成後に正常状態になるまでの最大待機時間。                                | `5` 秒         |
| **Health Check Interval**  | コネクターのヘルスチェック間隔。                                                    | `15` 秒        |
| **Health Check Timeout**   | ヘルスチェック結果が返るまでの最大時間。タイムアウトするとコネクターは切断とみなされます。 | `60` 秒        |

### プロデューサー Sink とコンシューマー Source 共通の高度な設定

プロデューサー Sink とコンシューマー Source は以下の高度な設定を共有します。デフォルトの **Health Check Interval** は異なります。

| フィールド名                     | 説明                                                                                     | プロデューサー Sink デフォルト | コンシューマー Source デフォルト |
| ------------------------------ | ---------------------------------------------------------------------------------------- | ------------------------------ | -------------------------------- |
| **Request TTL**                | リクエストがバッファに入ってからレスポンスまたはアックを受け取るまでの最大時間。期間内に応答がなければリクエストは期限切れとなります。 | `45` 秒                       | `45` 秒                         |
| **Health Check Interval**      | Sink または Source のヘルスチェック間隔。                                              | `15` 秒                       | `30` 秒                         |
| **Health Check Interval Jitter** | ヘルスチェック間隔に加える一様ランダム遅延。コネクターを共有するアクションやソースが同時にヘルスチェックを開始しないようにします。 | `0` ミリ秒                    | `0` ミリ秒                      |
| **Health Check Timeout**       | ヘルスチェック結果が返るまでの最大時間。タイムアウトすると Sink または Source は切断とみなされます。 | `60` 秒                       | `60` 秒                         |

### プロデューサー Sink 固有の高度な設定

GCP Pub/Sub プロデューサー Sink は以下の追加の高度な設定を提供します。

| フィールド名               | 説明                                                                                              | デフォルト値    |
| -------------------------- | ------------------------------------------------------------------------------------------------- | -------------- |
| **Buffer Pool Size**       | GCP Pub/Sub へ送信する前にデータを保存・処理するためのバッファワーカーの数。                       | `16`           |
| **Dispatch Strategy**      | 明示的なピックキーがないリクエストをバッファワーカーに割り当てる戦略。`Per Client ID` は同一クライアントのリクエストを同一ワーカーに保持し、`Random` はワーカー間で分散します。 | `Per Client ID` |
| **Max Buffer Queue Size**  | 各バッファワーカーが保持できる最大データ量。                                                     | `256` MB       |
| **Batch Size**             | 1 バッチあたりの最大リクエスト数。`1` に設定するとバッチ処理を無効化します。                      | `1`            |
| **Query Mode**             | リクエストを同期または非同期で送信するモード。`Async` モードでは EMQX は GCP Pub/Sub の応答を待たずにメッセージ処理を続行します。 | `Async`        |
| **Inflight Window**        | **Query Mode** が `Async` の場合、応答を受け取らずに送信可能な最大リクエスト数。MQTT クライアントからのメッセージを厳密な順序で処理する必要がある場合は `1` に設定します。 | `100`          |

### コンシューマー Source 固有の高度な設定

GCP Pub/Sub コンシューマー Source は以下の追加の高度な設定を提供します。

| フィールド名               | 説明                                                                                              | デフォルト値    |
| -------------------------- | ------------------------------------------------------------------------------------------------- | -------------- |
| **Ack Deadline**           | GCP Pub/Sub が配信済みメッセージのアックを Source から受け取るまでの待機時間の目安。期限切れ後はメッセージが再配信される可能性があります。サポートされる範囲は `10` ～ `600` 秒です。 | `60` 秒        |
