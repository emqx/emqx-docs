# GCP Pub/Sub への MQTT データ取り込み

[Google Cloud Pub/Sub](https://cloud.google.com/pubsub?hl=en-us) は、非常に高い信頼性とスケーラビリティを実現する非同期メッセージングサービスです。EMQX は、MQTT データのリアルタイム抽出、処理、分析のために Google Cloud Pub/Sub とのシームレスな統合をサポートしています。Cloud Functions、App Engine、Cloud Run、Kubernetes Engine、Compute Engine などのさまざまな Google Cloud サービスにデータをプッシュできます。また、Google Cloud から MQTT へのデータ配信も可能で、ユーザーが GCP 上で迅速に IoT アプリケーションを構築できるよう支援します。

本ページでは、EMQX と GCP Pub/Sub 間のデータ統合について、作成および検証の実践的な手順とともに包括的に紹介します。

## 動作概要

GCP Pub/Sub データ統合は、EMQX の標準機能であり、MQTT データストリームを Google Cloud とシームレスに統合し、IoT アプリケーション開発のための豊富なサービスと機能を活用できるよう設計されています。

![GCP_bridge_architect](./assets/gcp_pubsub/GCP_bridge_architect.png)

EMQX はルールエンジンと Sink を通じて MQTT データを GCP Pub/Sub に転送します。GCP Pub/Sub のプロデューサー役割の例を挙げると、全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のソースからの MQTT メッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などを行います。
3. **GCP Pub/Sub へのブリッジング**：ルールはメッセージを GCP Pub/Sub に転送するアクションをトリガーし、データプロパティ、オーダーキー、MQTT トピックと GCP Pub/Sub トピックのマッピングを簡単に設定できます。これにより、より豊富なコンテキスト情報と順序保証を持つデータ統合が可能となり、柔軟な IoT データ処理を実現します。

MQTT メッセージデータが GCP Pub/Sub に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：Dataflow、BigQuery、Pub/Sub のストリーミング機能など、強力な Google Cloud のデータ処理・分析ツールを活用し、メッセージデータのリアルタイム処理と分析を行い、有益な洞察と意思決定支援を得られます。
- イベント駆動型機能：Cloud Functions や Cloud Run などの Google Cloud イベント処理をトリガーし、動的かつ柔軟な関数トリガーと処理を実現します。
- データ保存と共有：Cloud Storage や Firestore などの Google Cloud ストレージサービスにメッセージデータを送信し、大量データの安全な保存と管理を行います。これにより、他の Google Cloud サービスとデータを共有・分析し、多様なビジネスニーズに対応できます。

## 特長とメリット

GCP Pub/Sub とのデータ統合は、以下のような特長とメリットを提供します。

- **堅牢なメッセージングサービス**：EMQX と GCP Pub/Sub はともに高可用性とスケーラビリティを備え、大規模なメッセージストリームの信頼性の高い受信、配信、処理を保証します。IoT データの順序付け、メッセージ品質保証、パーシステンス（永続化）をサポートし、メッセージの確実な伝送と処理を実現します。
- **柔軟なルールエンジン**：組み込みのルールエンジンにより、特定のソースメッセージやイベントをトピックマッチングに基づいて処理できます。データ形式変換、特定情報のフィルタリング、コンテキスト情報の付加などの操作が可能です。これを GCP Pub/Sub と組み合わせることで、さらなる処理と分析を行えます。
- **豊富なコンテキスト情報**：GCP Pub/Sub データ統合を通じて、クライアント属性を Pub/Sub 属性やソートキーにマッピングするなど、メッセージにより豊かなコンテキスト情報を付加できます。これにより、後続のアプリケーション開発やデータ処理でより精密な分析と処理が可能になります。

まとめると、EMQX と GCP Pub/Sub の統合により、高信頼かつスケーラブルなメッセージ配信と、データ分析・統合のための豊富なツールとサービスを利用できます。これにより、堅牢な IoT アプリケーションの構築と、イベント駆動型の柔軟なビジネスロジックの実装が可能となります。

## はじめる前に

このセクションでは、GCP Pub/Sub データ統合の作成を開始する前に完了すべき準備について説明します。

### 前提条件

- EMQX のデータ統合に関する [ルール](./rules.md) の知識
- [データ統合](./data-bridges.md) に関する知識

### GCP でサービスアカウントキーを作成する

GCP Pub/Sub サービスを利用するには、サービスアカウントとサービスアカウントキーを作成する必要があります。

1. GCP アカウントで [サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount) を作成します。サービスアカウントには、対象トピックへのメッセージの検査／読み取りおよびパブリッシュ権限（例：Pub/Sub Editor ロール）が付与されていることを確認してください。

2. 作成したサービスアカウントのメールアドレスをクリックし、**Key** タブを選択します。**Add key** のドロップダウンリストから **Create new key** を選択し、そのサービスアカウントのキーを JSON 形式で作成してダウンロードします。

   ::: tip

   サービスアカウントキーは後で使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCP で Workload Identity Federation を設定する

Workload Identity Federation（WIF）により、EMQX は長期間有効なサービスアカウントキーを使わずに GCP リソースにアクセスできます。代わりに、EMQX は外部 ID プロバイダー（例：Microsoft Azure）からのトークンを GCP の Security Token Service 経由で一時的な GCP トークンに交換し、それを使って GCP サービスアカウントをなりすまします。トークンの更新は自動で行われます。

WIF を使用するには、コネクター作成前に GCP プロジェクトで以下を完了してください。

1. Google Cloud コンソールで **IAM & Admin** -> **Workload Identity Federation** に移動し、ワークロードアイデンティティプールを作成し、**Pool ID** と **Project Number** を控えます。

2. プールにプロバイダーを追加し、**Provider ID** を控えます。OIDC ベースの認証の場合、外部 ID プロバイダーから OAuth 2.0 クライアント認証情報（クライアント ID、クライアントシークレット、トークンエンドポイント URI）を取得します。

3. Pub/Sub トピックにアクセスできる GCP サービスアカウントをなりすます権限をワークロードアイデンティティプールに付与します。コネクター設定時にサービスアカウントのメールアドレスが必要です。

   ::: tip

   詳細な手順は [Workload Identity Federation の設定](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers) を参照してください。

   :::

**例：Microsoft Azure (Entra ID)**

[Microsoft Entra ID](https://portal.azure.com/) で API を公開するアプリケーションを登録し、クライアントシークレットを作成します。コネクター設定時に以下の値を使用します。

| コネクター項目 | 値 |
|---|---|
| **Endpoint URI** | `https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token` |
| **OAuth Client ID** | `api://<application-id>` 形式のアプリケーション（クライアント）ID |
| **OAuth Client Secret** | アプリケーション用に生成したクライアントシークレット |
| **OAuth Request Scope** | `api://<application-id>/.default` |

::: tip 注意

`scope` はアプリケーションのオーディエンス（`aud`）と正確に一致する必要があります。そうでないと GCP STS とのトークン交換に失敗します。詳細は Microsoft ドキュメントの [OAuth 2.0 クライアント認証フロー](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-client-creds-grant-flow) を参照してください。

サービスアカウントに WIF プールへのアクセス権を付与する際は、Subject 値に **Object ID**（アプリケーション ID ではない）を使用してください。Object ID は Azure ポータルのアプリケーション概要ページの **Enterprise applications** に表示されます。

:::

### GCP でトピックを作成・管理する

EMQX で GCP Pub/Sub データ統合を設定する前に、トピックを作成し、GCP での基本的な管理操作に慣れておく必要があります。

1. Google Cloud コンソールで **Pub/Sub** -> **Topics** ページに移動します。詳細は [トピックの作成と管理](https://cloud.google.com/pubsub/docs/create-topic) を参照してください。

   ::: tip

   サービスアカウントには該当トピックへのパブリッシュ権限が必要です。

   :::

2. **Topic ID** フィールドにトピックの ID を入力し、**Create topic** をクリックします。

   <img src="./assets/gcp_pubsub/create-topic-GCP-console.png" alt="GCP コンソールでのトピック作成" style="zoom:50%;" />

3. **Subscriptions** ページに移動し、リストの中から作成した **Topic ID** をクリックします。トピックにサブスクリプションを作成します。

   - **Delivery type** で **Pull** を選択します。
   - **Message retention duration** は `7` 日を選択します。

   詳細は [GCP Pub/Sub サブスクリプション](https://cloud.google.com/pubsub/docs/subscriber) を参照してください。

   <img src="./assets/gcp_pubsub/add-subscription-to-topic.png" alt="トピックへのサブスクリプション追加" style="zoom:50%;" />

4. **Subscription ID** -> **Messages** -> **Pull** をクリックすると、トピックに送信されたメッセージを確認できます。

   <img src="./assets/gcp_pubsub/subscriptions-id.png" alt="サブスクリプション ID" style="zoom:50%;" />

   <img src="./assets/gcp_pubsub/subscriptions-id-pull.png" alt="サブスクリプションメッセージのプル" style="zoom:50%;" />

## GCP Pub/Sub プロデューサーコネクターの作成

GCP Pub/Sub プロデューサー Sink アクションを追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するための GCP Pub/Sub プロデューサーコネクターを作成します。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Producer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubproducer`）。名前は GCP Pub/Sub プロデューサー Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **Authentication** ドロップダウンから以下の認証方法のいずれかを選択し、対応するフィールドに入力します。
   - **Service Account JSON**：前述の [GCP でサービスアカウントキーを作成する](#gcp-でサービスアカウントキーを作成する) でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
   - **Workload Identity Federation (WIF)**：以下のフィールドに入力します。前提条件は [GCP で Workload Identity Federation を設定する](#gcp-で-workload-identity-federation-を設定する) を参照してください。
     - **GCP Project ID**：コネクターがアクセスするリソースのプロジェクト ID。
     - **GCP Project Number**：コネクターがアクセスするリソースのプロジェクト番号。
     - **Service Account Email**：なりすますサービスアカウントのメールアドレス。
     - **Workload Identity Pool ID**：WIF トークン交換に使用するワークロードアイデンティティプールの ID。
     - **Workload Identity Provider ID**：WIF トークン交換に使用するワークロードアイデンティティプロバイダーの ID。
     - **Initial Token Configuration** で認証情報タイプを選択し、対応するフィールドを入力します。現在サポートされているのは **OIDC with Client Credentials Grant Type** のみです。
       - **Endpoint URI**：OIDC プロバイダーの OAuth トークンエンドポイント URI。
       - **OAuth Client ID**：OAuth サーバーからトークンを要求するためのクライアント ID。
       - **OAuth Client Secret**：OAuth サーバーからトークンを要求するためのクライアントシークレット。
       - **OAuth Request Scope**：OAuth アクセストークンを要求する際に提供する `scope`（プロバイダーによって必要な場合）。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして GCP Pub/Sub Producer Sink を使ったルール作成に進めます。詳細は [GCP Pub/Sub Producer Sink を使ったルール作成](#create-a-rule-with-gcp-pub-sub-producer-sink) を参照してください。

## GCP Pub/Sub プロデューサー Sink を使ったルール作成

このセクションでは、GCP Pub/Sub に保存するデータを指定するルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` を入力します。

4. **SQL Editor** にルールを設定します。例えば、トピック `/devices/+/events` の MQTT メッセージを GCP Pub/Sub に保存する場合、以下の SQL を使用します。

   注意：独自の SQL を指定する場合、Sink のペイロードテンプレートで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "/devices/+/events"
   ```

   注意：初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストができます。

5. **+ Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンから `Google PubSub Producer` を選択すると、EMQX はルールで処理したデータを GCP Pub/Sub に送信します。

6. **Action** ドロップダウンはデフォルトの `Create Action` のままにします。既存の GCP Pub/Sub Producer Sink を選択することも可能です。この例では新しい Sink を作成してルールに追加します。

7. **Name** フィールドに Sink の名前を入力します。名前は英数字の組み合わせにしてください。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubprodcer` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメータは [コネクター作成](#create-a-connector) を参照してください。

9. **GCP PubSub Topic** に、[GCP でトピックを作成・管理する](#gcp-でトピックを作成・管理する) で作成したトピック ID `my-iot-core` を入力します。

10. **Payload Template** にテンプレートを定義するか空欄のままにします。

    - 空欄の場合、MQTT メッセージのクライアント ID、トピック、ペイロードなどの可視入力すべてを JSON 形式でエンコードします。
    - 定義済みテンプレートを使う場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応値に置き換えられます。例えば `${topic}` は MQTT メッセージのトピック `my/topic` に置き換わります。

11. **Attributes Template** と **Ordering Key Template** に、送信メッセージの属性およびオーダーキーのフォーマットテンプレートを定義します（任意）。

    - **Attributes** はキーと値の両方に `${variable_name}` 形式のプレースホルダーを使えます。これらは MQTT コンテキストから抽出されます。キーのテンプレートが空文字列になる場合、そのキーは GCP Pub/Sub 送信メッセージから省略されます。
    - **Ordering Key** は `${variable_name}` 形式のプレースホルダーを使えます。解決後の値が空文字列の場合、GCP Pub/Sub 送信メッセージの `orderingKey` フィールドは設定されません。

12. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

13. **詳細設定（任意）**：詳細は [Sink の機能](./data-bridges.md#features-of-sink) を参照してください。

14. **Create** をクリックする前に **Test Connectivity** をクリックして、コネクターが GCP Pub/Sub サーバーに接続できるかテストできます。

15. **Create** ボタンをクリックして Sink の設定を完了すると、新しい Sink が **Action Outputs** タブに表示されます。

16. **Create Rule** ページで **Create** をクリックしてルールを作成します。

これでルールが正常に作成されました。**Integration** -> **Rules** ページで新しいルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Google PubSub Producer Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `/devices/+/events` のメッセージがルール `my_rule` によって解析され、GCP Pub/Sub に送信・保存されていることが視覚的に確認できます。

## プロデューサールールのテスト

1. MQTTX を使ってトピック `/devices/+/events` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t /devices/+/events -m '{ "msg": "hello GCP PubSub" }'
   ```

2. Sink の稼働状況を確認し、新しい受信メッセージと送信メッセージがそれぞれ1件あることを確認します。

3. GCP の **Pub/Sub** -> **Subscriptions** に移動し、**MESSAGES** タブをクリックするとメッセージが表示されます。

## GCP Pub/Sub コンシューマーコネクターの作成

GCP Pub/Sub コンシューマー Sink を追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するための GCP Pub/Sub コンシューマーコネクターを作成します。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Consumer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubconsumer`）。名前は GCP Pub/Sub コンシューマー Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **Authentication** ドロップダウンから以下の認証方法のいずれかを選択し、対応するフィールドに入力します。
   - **Service Account JSON**：前述の [GCP でサービスアカウントキーを作成する](#gcp-でサービスアカウントキーを作成する) でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
   - **Workload Identity Federation (WIF)**：以下のフィールドに入力します。前提条件は [GCP で Workload Identity Federation を設定する](#gcp-で-workload-identity-federation-を設定する) を参照してください。
     - **GCP Project ID**：コネクターがアクセスするリソースのプロジェクト ID。
     - **GCP Project Number**：コネクターがアクセスするリソースのプロジェクト番号。
     - **Service Account Email**：なりすますサービスアカウントのメールアドレス。
     - **Workload Identity Pool ID**：WIF トークン交換に使用するワークロードアイデンティティプールの ID。
     - **Workload Identity Provider ID**：WIF トークン交換に使用するワークロードアイデンティティプロバイダーの ID。
     - **Initial Token Configuration** で認証情報タイプを選択し、対応するフィールドを入力します。現在サポートされているのは **OIDC with Client Credentials Grant Type** のみです。
       - **Endpoint URI**：OIDC プロバイダーの OAuth トークンエンドポイント URI。
       - **OAuth Client ID**：OAuth サーバーからトークンを要求するためのクライアント ID。
       - **OAuth Client Secret**：OAuth サーバーからトークンを要求するためのクライアントシークレット。
       - **OAuth Request Scope**：OAuth アクセストークンを要求する際に提供する `scope`（プロバイダーによって必要な場合）。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして GCP Pub/Sub コンシューマーソースを使ったルール作成に進めます。詳細は [GCP Pub/Sub コンシューマーソースを使ったルール作成](#create-a-rule-with-gcp-pub-sub-cconsumer-source) を参照してください。

## GCP Pub/Sub コンシューマーソースを使ったルール作成

このセクションでは、GCP Pub/Sub からメッセージを受信し、EMQX に転送するルールの作成方法を説明します。Google PubSub Consumer ソースを作成・設定し、ルールのデータ入力として追加します。さらに、Republish アクションをルールに追加して、GCP Pub/Sub から受信したメッセージを EMQX に転送します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule_source` を入力します。

4. 右側の **Data Inputs** タブで、デフォルトの入力 `Messages` を削除し、**Add Input** をクリックします。

5. **Input Type** ドロップダウンから `Google PubSub Consumer` を選択します。

6. **Source** ドロップダウンはデフォルトの `Create Source` のままにします。この例では新しいソースを作成してルールに追加します。

7. ソースの **Name** と（任意で）**Description** を入力します。名前は英数字の組み合わせにしてください（例：`my-gcppubsub-source`）。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubconsumer` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメータは [コネクター作成](#create-a-connector) を参照してください。

9. GCP Pub/Sub から EMQX にメッセージを受信するためのソース情報を設定します。

   - **GCP PubSub Topic**：受信する GCP Pub/Sub トピック名を入力します（例：`my-iot-core`）。
   - **Maximum Messages to Pull**：1 回のプルリクエストで取得する最大メッセージ数を指定します。実際の取得数は指定値より少ない場合があります。

10. 詳細設定（任意）：詳細は [Sink の機能](./data-bridges.md#features-of-sink) を参照してください。

11. **Create** をクリックする前に **Test Connectivity** をクリックして、GCP Pub/Sub サーバーへの接続が成功するかテストできます。

12. **Create** をクリックしてソース作成を完了します。ソースはルールの **Data Inputs** タブに追加され、**SQL Editor** のルールは以下のようになります。

    ```sql
    SELECT
      *
    FROM
      "$bridges/gcppubsub:my-gcppubsub-source"
    ```

    注意：初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストができます。

    `my-gcppubsub-source` から、ルールの SQL は以下の GCP Pub/Sub から MQTT トピックへのマッピングテーブルに示す GCP Pub/Sub メッセージフィールドにアクセスできます。ルール SQL を調整してデータ処理を行うことも可能です。この例ではデフォルトの SQL を使用します。

    | フィールド名        | 説明                                                        |
    | ----------------- | ------------------------------------------------------------ |
    | `attributes`      | （任意）文字列のキーと値のペアを含むオブジェクト（存在する場合） |
    | `message_id`      | GCP Pub/Sub がメッセージに割り当てたメッセージ ID             |
    | `ordering_key`    | （任意）メッセージの順序付けキー（存在する場合）               |
    | `publishing_time` | GCP Pub/Sub によって定義されたメッセージのタイムスタンプ        |
    | `topic`           | 発信元の GCP Pub/Sub トピック名                              |
    | `value`           | （任意）メッセージペイロード（存在する場合）                   |

    **注意**：各 GCP Pub/Sub から MQTT へのトピックマッピングは一意の GCP Pub/Sub トピック名を含む必要があります。つまり、同じ GCP Pub/Sub トピックが複数のマッピングに存在してはいけません。

これで GCP Pub/Sub コンシューマーソースを正常に作成しましたが、メッセージはまだ直接 EMQX にパブリッシュされません。次に、[ルールに Republish アクションを追加する](#add-republish-action-to-the-rule) 手順を続けて、Republish アクションを作成しルールに追加してください。

### ルールに Republish アクションを追加する

このセクションでは、GCP Pub/Sub コンシューマーソースから受信したメッセージを転送し、EMQX トピック `t/1` にパブリッシュするための Republish アクションの追加方法を説明します。

1. ページ右側の **Action Output** タブを選択し、**Add Action** ボタンをクリックして、**Type of Action** ドロップダウンから `Republish` アクションを選択します。

2. メッセージ再パブリッシュの設定を入力します。

   - **Topic**：MQTT にパブリッシュするトピックを入力します。ここでは `t/1` とします。

   - **QoS**：`0`、`1`、`2`、`${qos}` のいずれかを選択、または他のフィールドから QoS を設定するためのプレースホルダーを入力します。`${qos}` を選択すると元のメッセージの QoS に従います。

   - **Retain**：`true` または `false` を選択します。メッセージをリテインメッセージとしてパブリッシュするかどうかを決定します。プレースホルダーを使って他のフィールドからリテインフラグを設定することも可能です。この例では `false` を選択します。

   - **Payload**：転送メッセージのペイロード生成テンプレートを設定します。空欄の場合はルールの出力結果を転送します。ここでは `${payload}` を入力し、ペイロードのみを転送することを示します。

     MQTT ペイロードテンプレートのデフォルト値は `${.}` で、利用可能なすべてのデータを JSON オブジェクトとしてエンコードします。例えば、すべてのオプションフィールドを含む GCP Pub/Sub メッセージに対して `${.}` をテンプレートに選択すると、以下のようになります。

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

     GCP Pub/Sub メッセージのサブフィールドにはドット記法でアクセスできます。例えば `${.value}` は GCP Pub/Sub メッセージの値に解決され、`${.attributes.h1}` は `h1` メッセージ属性キーの値に解決されます。存在しない値は空文字列に置き換えられます。

   - **MQTT 5.0 メッセージプロパティ**：デフォルトで無効です。詳細設定は [Republish アクションの追加](./rule-get-started.md#add-republish-action) を参照してください。

3. **Create** をクリックしてアクション作成を完了します。作成成功後、ルール作成ページに戻り、Republish アクションが **Action Outputs** タブに追加されます。

4. ルール作成ページで **Create** ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成されました。**Rules** ページで新しいルールを確認できます。**Sources** タブには新しく作成した GCP Pub/Sub コンシューマーソースが表示されます。

また、**Integrate** -> **Flow Designer** をクリックするとトポロジーを確認でき、GCP Pub/Sub コンシューマーソースからのメッセージが Republish を通じてトピック `t/1` にパブリッシュされる様子を直感的に把握できます。

## <!--Test the Consumer Rule-->
