# GCP Pub/Sub への MQTT データ取り込み

[Google Cloud Pub/Sub](https://cloud.google.com/pubsub?hl=en-us) は、非常に高い信頼性とスケーラビリティを実現するために設計された非同期メッセージングサービスです。EMQX は、MQTT データのリアルタイム抽出、処理、分析のために Google Cloud Pub/Sub とのシームレスな統合をサポートしています。Cloud Functions、App Engine、Cloud Run、Kubernetes Engine、Compute Engine などのさまざまな Google Cloud サービスへデータをプッシュすることが可能です。また、Google Cloud から MQTT へのデータ配信も可能で、ユーザーが GCP 上で迅速に IoT アプリケーションを構築できるよう支援します。

本ページでは、EMQX と GCP Pub/Sub 間のデータ統合について包括的に紹介し、データ統合の作成および検証手順を実践的に解説します。

## 動作概要

GCP Pub/Sub データ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Google Cloud とシームレスに統合し、豊富なサービスと機能を活用して IoT アプリケーション開発を支援します。

![GCP_bridge_architect](./assets/gcp_pubsub/GCP_bridge_architect.png)

EMQX はルールエンジンと Sink を通じて MQTT データを GCP Pub/Sub に転送します。GCP Pub/Sub のプロデューサー役割の例で、全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンを用いて、特定のトピックにマッチする MQTT メッセージを処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **GCP Pub/Sub へのブリッジング**：ルールはメッセージを GCP Pub/Sub に転送するアクションをトリガーします。データプロパティ、オーダーキー、MQTT トピックと GCP Pub/Sub トピックのマッピングを簡単に設定可能です。これにより、より豊富なコンテキスト情報と順序保証を伴うデータ統合が可能となり、柔軟な IoT データ処理を実現します。

MQTT メッセージデータが GCP Pub/Sub に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：Dataflow、BigQuery、Pub/Sub のストリーミング機能など、強力な Google Cloud のデータ処理・分析ツールを活用し、メッセージデータのリアルタイム処理・分析を行い、有益なインサイトや意思決定支援を得られます。
- イベント駆動型機能：Cloud Functions や Cloud Run などの Google Cloud イベント処理をトリガーし、動的かつ柔軟な機能トリガーと処理を実現します。
- データ保存と共有：Cloud Storage や Firestore などの Google Cloud ストレージサービスにメッセージデータを送信し、大量データの安全な保存・管理を行います。これにより他の Google Cloud サービスとデータを共有・分析し、多様なビジネスニーズに対応可能です。

## 特徴と利点

GCP Pub/Sub とのデータ統合は以下の特徴と利点を提供します。

- **堅牢なメッセージングサービス**：EMQX と GCP Pub/Sub は共に高可用性とスケーラビリティを備え、大規模なメッセージストリームの信頼性の高い受信、配信、処理を保証します。IoT データの順序付け、メッセージの品質保証、パーシステンスをサポートし、メッセージの確実な伝送と処理を実現します。
- **柔軟なルールエンジン**：組み込みのルールエンジンにより、特定の送信元メッセージやイベントをトピックマッチングに基づいて処理可能です。データ形式変換、特定情報のフィルタリング、コンテキスト情報の付加などの操作が可能で、GCP Pub/Sub と組み合わせてさらなる処理・分析が行えます。
- **豊富なコンテキスト情報**：GCP Pub/Sub データ統合を通じて、メッセージにより豊かなコンテキスト情報を付加できます。クライアント属性を Pub/Sub 属性やソートキーにマッピングすることで、後続のアプリケーション開発やデータ処理でより精緻な分析・処理が可能になります。

まとめると、EMQX と GCP Pub/Sub の統合により、高信頼性かつスケーラブルなメッセージ配信が可能となり、データ分析や統合のための豊富なツール・サービスを活用できます。これにより、堅牢な IoT アプリケーションの構築やイベント駆動型の柔軟なビジネスロジックの実装が実現します。

## はじめる前に

このセクションでは、GCP Pub/Sub データ統合の作成を開始する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GCP でのサービスアカウントキーの作成

GCP Pub/Sub サービスを利用するには、サービスアカウントとサービスアカウントキーを作成する必要があります。

1. GCP アカウントで[サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)を作成します。サービスアカウントには、対象トピックへのメッセージの検査/読み取りおよびパブリッシュ権限（例：Pub/Sub Editor ロール）が付与されていることを確認してください。

2. 作成したサービスアカウントのメールアドレスをクリックし、**Key** タブを開きます。**Add key** のドロップダウンリストから **Create new key** を選択し、サービスアカウントキーを JSON 形式で作成・ダウンロードします。

   ::: tip

   サービスアカウントキーは後で使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCP でのトピック作成と管理

EMQX で GCP Pub/Sub データ統合を設定する前に、トピックを作成し、GCP での基本的な管理操作に慣れておく必要があります。

1. Google Cloud コンソールで **Pub/Sub** -> **Topics** ページに移動します。詳細な手順は[トピックの作成と管理](https://cloud.google.com/pubsub/docs/create-topic)を参照してください。

   ::: tip

   サービスアカウントには、該当トピックへのパブリッシュ権限が必要です。

   :::

2. **Topic ID** フィールドにトピックの ID を入力し、**Create topic** をクリックします。

   <img src="./assets/gcp_pubsub/create-topic-GCP-console.png" alt="GCP コンソールでのトピック作成" style="zoom:50%;" />

3. **Subscriptions** ページに移動し、リスト内の作成したトピックの **Topic ID** をクリックします。トピックに対するサブスクリプションを作成します。

   - **Delivery type** で **Pull** を選択します。
   - **Message retention duration** は `7` 日を選択します。

   詳細は[GCP Pub/Sub サブスクリプション](https://cloud.google.com/pubsub/docs/subscriber)を参照してください。

   <img src="./assets/gcp_pubsub/add-subscription-to-topic.png" alt="トピックへのサブスクリプション追加" style="zoom:50%;" />

4. **Subscription ID** -> **Messages** -> **Pull** をクリックすると、トピックに送信されたメッセージを確認できます。

   <img src="./assets/gcp_pubsub/subscriptions-id.png" alt="サブスクリプションID" style="zoom:50%;" />

   <img src="./assets/gcp_pubsub/subscriptions-id-pull.png" alt="サブスクリプションIDのメッセージプル" style="zoom:50%;" />

## GCP Pub/Sub プロデューサーコネクターの作成

GCP Pub/Sub プロデューサー Sink アクションを追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するためにプロデューサーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Producer** を選択して **Next** をクリックします。
3. `my-pubsubproducer` などの名前と説明を入力します。名前は GCP Pub/Sub プロデューサー Sink とコネクターを紐付けるために使用され、クラスター内で一意である必要があります。
4. **GCP Service Account Credentials** にて、[GCP でのサービスアカウントキーの作成](#gcp-でのサービスアカウントキーの作成)でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
5. **Advanced Settings** を展開し、必要に応じてオプション設定を行います。詳細は[コネクターの詳細設定](#connector-advanced-settings)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして Sink を指定したルールの作成に進むことができます。詳細は[Google Pub/Sub プロデューサー Sink を使ったルール作成](#create-a-rule-with-gcp-pubsub-producer-sink)を参照してください。

## GCP Pub/Sub プロデューサー Sink を使ったルールの作成

このセクションでは、GCP Pub/Sub に保存するデータを指定するルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` と入力します。

4. **SQL Editor** にルールを設定します。例えば、トピック `/devices/+/events` の MQTT メッセージを GCP Pub/Sub に保存したい場合、以下の SQL を使用します。

   注意：独自の SQL を指定する場合、Sink のペイロードテンプレートで必要なすべてのフィールドを `SELECT` 部分に含める必要があります。

   ```sql
   SELECT
     *
   FROM
     "/devices/+/events"
   ```

   注意：初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

5. **+ Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action** のドロップダウンリストから `Google PubSub Producer` を選択し、ルールで処理したデータを GCP Pub/Sub に送信するようにします。

6. **Action** ドロップダウンは `Create Action` のままにするか、既存の GCP Pub/Sub プロデューサー Sink を選択できます。この例では新しい Sink を作成してルールに追加します。

7. **Name** フィールドに Sink の名前を入力します。名前は英数字の組み合わせにしてください。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubprodcer` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

9. **GCP PubSub Topic** に以下のいずれかを入力します。

   - [GCP でのトピック作成と管理](#gcp-でのトピック作成と管理)で作成したトピック名（例：`my-iot-core`）。EMQX は設定されたサービスアカウントのプロジェクト内でトピックを解決します。
   - フルクオリファイドトピックパス（`projects/<project-id>/topics/<topic-name>`）形式。異なる GCP プロジェクトのトピックにパブリッシュする場合はこちらを使用し、該当プロジェクトのトピックに対してサービスアカウントに必要な Pub/Sub 権限を付与してください。

10. **Payload Template** にテンプレートを定義するか空欄のままにします。

    - 空欄の場合、MQTT メッセージのクライアント ID、トピック、ペイロードなどの可視入力すべてを JSON 形式でエンコードします。
    - 定義済みテンプレートを使う場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応する値で置換されます。例：`${topic}` は MQTT メッセージのトピック `my/topic` に置き換わります。

11. **Attributes Template** と **Ordering Key Template** で送信メッセージの属性やオーダーキーのフォーマットテンプレートを定義します（任意）。

    - **Attributes** はキーと値の両方に `${variable_name}` 形式のプレースホルダーを使え、MQTT コンテキストから値を抽出します。キーが空文字列に解決された場合、そのキーは GCP Pub/Sub 送信メッセージから省略されます。
    - **Ordering Key** も `${variable_name}` プレースホルダーを使え、空文字列に解決された場合は `orderingKey` フィールドは設定されません。

12. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。プライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

13. **Advanced Settings** を展開し、必要に応じてオプション設定を行います。詳細は[詳細設定](#advanced-settings)を参照してください。

14. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテスト可能です。

15. **Create** ボタンをクリックして Sink 設定を完了すると、**Action Outputs** タブに新しい Sink が表示されます。

16. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成します。

これでルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブで新しい Google PubSub Producer Sink を確認可能です。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを表示でき、トピック `/devices/+/events` のメッセージがルール `my_rule` によって解析され、GCP Pub/Sub に送信・保存される様子を確認できます。

## プロデューサールールのテスト

1. MQTTX を使い、トピック `/devices/+/events` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t /devices/+/events -m '{ "msg": "hello GCP PubSub" }'
   ```

2. Sink の稼働状況を確認し、1件の新規受信メッセージと1件の新規送信メッセージがあることを確認します。

3. GCP の **Pub/Sub** -> **Subscriptions** に移動し、**MESSAGES** タブをクリックするとメッセージが確認できます。

## GCP Pub/Sub コンシューマーコネクターの作成

GCP Pub/Sub コンシューマー Source を追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するためにコンシューマーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Consumer** を選択して **Next** をクリックします。
3. `my-pubsubconsumer` などの名前と説明を入力します。名前は GCP Pub/Sub コンシューマー Source とコネクターを紐付けるために使用され、クラスター内で一意である必要があります。
4. **GCP Service Account Credentials** にて、[GCP でのサービスアカウントキーの作成](#gcp-でのサービスアカウントキーの作成)でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
5. **Advanced Settings** を展開し、必要に応じてオプション設定を行います。詳細は[コネクターの詳細設定](#connector-advanced-settings)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして GCP Pub/Sub コンシューマー Source を使ったルール作成に進むことができます。詳細は[Google Pub/Sub コンシューマー Source を使ったルール作成](#create-a-rule-with-gcp-pubsub-consumer-source)を参照してください。

## GCP Pub/Sub コンシューマー Source を使ったルールの作成

このセクションでは、GCP Pub/Sub からメッセージを消費し、EMQX に転送するルールの作成方法を説明します。Google PubSub コンシューマー Source を作成・設定し、ルールのデータ入力として追加します。また、Republish アクションを追加して GCP Pub/Sub から EMQX へのメッセージ転送を行います。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule_source` と入力します。

4. 右側の **Data Inputs** タブで、デフォルトの Input `Messages` を削除し、**Add Input** をクリックします。

5. **Input Type** のドロップダウンから `Google PubSub Consumer` を選択します。

6. **Source** ドロップダウンはデフォルトの `Create Source` のままにします。この例では新しい Source を作成しルールに追加します。

7. Source の **Name** と（任意で）**Description** を入力します。名前は英数字の組み合わせで、例：`my-gcppubsub-source`。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubconsumer` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

9. GCP Pub/Sub から EMQX へメッセージを消費するため、以下の情報を設定します。

   - **GCP PubSub Topic**：トピック名（例：`my-iot-core`）またはフルクオリファイドトピックパス（`projects/<project-id>/topics/<topic-name>`）を入力します。トピック名は設定されたサービスアカウントのプロジェクト内で解決されます。異なる GCP プロジェクトのトピックを消費する場合はフルクオリファイドパスを入力し、サービスアカウントに該当トピックの Pub/Sub 権限を付与してください。コンシューマーサブスクリプションはサービスアカウントのプロジェクト内に作成され、トピック参照のみ別プロジェクトを指します。
   - **Maximum Messages to Pull**：1回のプルリクエストで GCP PubSub から取得する最大メッセージ数を指定します。実際の取得数は指定値より少ない場合があります。

10. **Advanced Settings** を展開し、必要に応じてオプション設定を行います。詳細は[詳細設定](#advanced-settings)を参照してください。

11. **Create** をクリックする前に、**Test Connectivity** をクリックして GCP Pub/Sub サーバーへの接続が成功するかテスト可能です。

12. **Create** をクリックして Source 作成を完了します。Source はルールの **Data Inputs** タブに追加され、**SQL Editor** のルールは以下のようになります。

    ```sql
    SELECT
      *
    FROM
      "$bridges/gcppubsub:my-gcppubsub-source"
    ```

    注意：初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

    `my-gcppubsub-source` からは、以下の GCP Pub/Sub から MQTT トピックへのマッピングテーブルに示すメッセージフィールドにアクセスできます。ルール SQL を調整してデータ処理が可能です。この例ではデフォルトの SQL を使用します。

    | フィールド名           | 説明                                                         |
    | --------------------- | ------------------------------------------------------------ |
    | `attributes`          | （任意）文字列のキーと値のペアを含むオブジェクト（存在する場合） |
    | `message_id`          | GCP Pub/Sub がこのメッセージに割り当てたメッセージ ID       |
    | `ordering_key`        | （任意）メッセージの順序付けキー（存在する場合）             |
    | `publishing_time`     | GCP Pub/Sub によるメッセージのタイムスタンプ                 |
    | `topic`               | 発信元の GCP Pub/Sub トピック                                |
    | `value`               | （任意）メッセージのペイロード（存在する場合）               |

    **注意**：各 GCP Pub/Sub から MQTT トピックへのマッピングは一意の GCP Pub/Sub トピック名を含む必要があります。つまり、同じ GCP Pub/Sub トピックが複数のマッピングに存在してはなりません。

これで GCP Pub/Sub コンシューマー Source の作成は完了しましたが、メッセージはまだ EMQX に直接パブリッシュされません。次に、[ルールに Republish アクションを追加する](#add-republish-action-to-the-rule) 手順を続けて、Republish アクションを作成しルールに追加してください。

### ルールに Republish アクションを追加する

このセクションでは、GCP Pub/Sub コンシューマー Source から消費したメッセージを転送し、EMQX トピック `t/1` にパブリッシュするための Republish アクションをルールに追加する方法を説明します。

1. ページ右側の **Action Output** タブを選択し、**Add Action** ボタンをクリックします。**Type of Action** ドロップダウンリストから `Republish` アクションを選択します。

2. メッセージ再パブリッシュの設定を入力します。

   - **Topic**：MQTT にパブリッシュするトピック。ここでは `t/1` と入力します。

   - **QoS**：`0`、`1`、`2`、または `${qos}` を選択、もしくは他のフィールドから QoS を設定するためのプレースホルダーを入力します。`${qos}` を選択すると元メッセージの QoS に従います。

   - **Retain**：`true` または `false` を選択します。メッセージをリテインメッセージとしてパブリッシュするかどうかを決定します。プレースホルダーを入力して他のフィールドからリテインフラグを設定することも可能です。この例では `false` を選択します。

   - **Payload**：転送するメッセージペイロードのテンプレートを設定します。空欄の場合はルールの出力結果をそのまま転送します。`${.value}` と入力すると GCP Pub/Sub メッセージのペイロード部分のみを転送します。

     MQTT ペイロードテンプレートのデフォルト値は `${.}` で、利用可能なすべてのデータを JSON オブジェクトとして含みます。例えば、すべてのオプションフィールドを含む GCP Pub/Sub メッセージの場合、以下のようになります。

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

     GCP Pub/Sub メッセージのサブフィールドはドット表記でアクセス可能です。例：`${.value}` はメッセージの値に解決され、`${.attributes.h1}` は存在すれば `h1` 属性の値に解決されます。存在しない値は空文字列に置換されます。

   - **MQTT 5.0 メッセージプロパティ**：デフォルトで無効です。詳細設定は[Republish アクションの追加](./rule-get-started.md#add-republish-action)を参照してください。

3. **Create** をクリックしてアクション作成を完了します。作成成功後、ルール作成ページに戻り、Republish アクションが **Action Outputs** タブに追加されます。

4. ルール作成ページで **Create** ボタンをクリックし、ルール全体の作成を完了します。

これでルールの作成が完了しました。**Rules** ページで新規作成したルールを確認できます。**Sources** タブで新しい GCP Pub/Sub コンシューマー Source を確認可能です。

また、**Integration** -> **Flow Designer** をクリックしてトポロジーを表示できます。トポロジーを通じて、GCP Pub/Sub コンシューマー Source からのメッセージが Republish を経て `t/1` にパブリッシュされる様子を直感的に確認できます。

## GCP Pub/Sub コンシューマールールのテスト

以下の手順で、GCP Pub/Sub コンシューマー Source が GCP Pub/Sub からメッセージを消費し、EMQX の MQTT トピック `t/1` に再パブリッシュすることを検証します。

1. MQTTX CLI を使い、EMQX の MQTT トピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. Google Cloud コンソールで **Pub/Sub** -> **Topics** に移動し、`my-iot-core` トピックをクリックして以下のメッセージをパブリッシュします。

   ```json
   {"msg":"hello GCP PubSub"}
   ```

3. MQTTX でトピック `t/1` に以下のメッセージが届くことを確認します。

   ```text
   topic: t/1
   payload: {"msg":"hello GCP PubSub"}
   ```

## 詳細設定

このセクションでは、GCP Pub/Sub コネクター、プロデューサー Sink、コンシューマー Source の詳細設定について説明します。

### コネクターの詳細設定

GCP Pub/Sub プロデューサーおよびコンシューマーコネクターは共通の詳細設定を使用します。

| フィールド名               | 説明                                                                                      | デフォルト値       |
| ------------------------- | ----------------------------------------------------------------------------------------- | ------------------ |
| **HTTP Pipelining**       | 各レスポンスを待たずに送信できる HTTP リクエストの最大数。`1` に設定すると次のリクエスト送信前にレスポンスを待つ。 | `100`              |
| **Connection Pool Size**  | コネクションプールで維持する接続数。                                                      | `8`                |
| **Connect Timeout**       | HTTP 接続確立の最大待機時間。                                                             | `15` 秒            |
| **Max Inactive**          | アクティビティなしで再接続を試みるまでの最大時間。                                         | `10` 秒            |
| **Max Retries**           | リクエスト送信中にエラー発生時の最大リトライ回数。                                        | `2`                |
| **Start Timeout**         | コネクター作成後、正常状態になるまでの最大待機時間。                                      | `5` 秒             |
| **Health Check Interval** | コネクターのヘルスチェック間隔。                                                         | `15` 秒            |
| **Health Check Timeout**  | ヘルスチェック結果が返るまでの最大時間。タイムアウト時はコネクターが切断と見なされる。      | `60` 秒            |

### プロデューサー Sink とコンシューマー Source 共通の詳細設定

プロデューサー Sink とコンシューマー Source は以下の詳細設定を共有します。**Health Check Interval** のデフォルト値は異なります。

| フィールド名                    | 説明                                                                                         | プロデューサー Sink デフォルト | コンシューマー Source デフォルト |
| ------------------------------ | -------------------------------------------------------------------------------------------- | ------------------------------ | -------------------------------- |
| **Request TTL**                | リクエストがバッファに入ってからレスポンスまたはアックを受け取るまでの最大時間。期間内に応答がなければリクエストは期限切れとなる。 | `45` 秒                       | `45` 秒                         |
| **Health Check Interval**      | Sink または Source のヘルスチェック間隔。                                                    | `15` 秒                       | `30` 秒                         |
| **Health Check Interval Jitter** | ヘルスチェック間隔に加える一様ランダム遅延。アクションやソースが同時にヘルスチェックを開始しないようにする。 | `0` ミリ秒                    | `0` ミリ秒                      |
| **Health Check Timeout**       | ヘルスチェック結果が返るまでの最大時間。タイムアウト時は Sink または Source が切断と見なされる。 | `60` 秒                       | `60` 秒                         |

### プロデューサー Sink 固有の詳細設定

GCP Pub/Sub プロデューサー Sink は以下の追加詳細設定を提供します。

| フィールド名                 | 説明                                                                                                  | デフォルト値    |
| --------------------------- | ----------------------------------------------------------------------------------------------------- | --------------- |
| **Buffer Pool Size**        | GCP Pub/Sub へ送信する前にデータを格納・処理するバッファワーカーの数。                                  | `16`            |
| **Dispatch Strategy**       | 明示的なピックキーなしのリクエストをバッファワーカーに割り当てる戦略。`Per Client ID` は同一クライアントのリクエストを同一ワーカーに割り当て、`Random` はランダムに分散。 | `Per Client ID` |
| **Max Buffer Queue Size**   | 各バッファワーカーが保持可能な最大データ量。                                                          | `256` MB        |
| **Batch Size**              | 1バッチあたりの最大リクエスト数。`1` に設定するとバッチ処理を無効化。                                  | `1`             |
| **Query Mode**              | リクエストを同期または非同期で送信するかを制御。`Async` モードでは GCP Pub/Sub の応答を待たずに処理を継続。 | `Async`         |
| **Inflight Window**         | **Query Mode** が `Async` の場合、応答を待たずに送信可能な最大リクエスト数。MQTT クライアントのメッセージを厳密な順序で処理する場合は `1` に設定。 | `100`           |

### コンシューマー Source 固有の詳細設定

GCP Pub/Sub コンシューマー Source は以下の追加詳細設定を提供します。

| フィールド名        | 説明                                                                                       | デフォルト値    |
| ------------------ | ------------------------------------------------------------------------------------------ | --------------- |
| **Ack Deadline**   | GCP Pub/Sub が配信したメッセージのアックを Source が返すまでの待機時間の目安。期限切れ後はメッセージが再配信される可能性あり。サポート範囲は `10` ～ `600` 秒。 | `60` 秒         |
