# GCP Pub/Sub への MQTT データ取り込み

[Google Cloud Pub/Sub](https://cloud.google.com/pubsub?hl=en-us) は、非常に高い信頼性とスケーラビリティを実現する非同期メッセージングサービスです。EMQX は、MQTT データをリアルタイムで抽出、処理、分析するために Google Cloud Pub/Sub とシームレスに統合できます。Cloud Functions、App Engine、Cloud Run、Kubernetes Engine、Compute Engine などのさまざまな Google Cloud サービスへデータをプッシュ可能です。また、Google Cloud から MQTT へのデータ配信も可能で、ユーザーが GCP 上で迅速に IoT アプリケーションを構築するのに役立ちます。

本ページでは、EMQX と GCP Pub/Sub 間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に解説します。

## 動作概要

GCP Pub/Sub データ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Google Cloud とシームレスに連携させ、豊富なサービスと機能を活用して IoT アプリケーション開発を支援します。

![GCP_bridge_architect](./assets/gcp_pubsub/GCP_bridge_architect.png)

EMQX はルールエンジンと Sink を介して MQTT データを GCP Pub/Sub に転送します。GCP Pub/Sub のプロデューサー役割の例を挙げると、全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**: デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**: 内蔵のルールエンジンは、特定のトピックにマッチする MQTT メッセージを処理します。ルールにマッチしたメッセージは、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
3. **GCP Pub/Sub へのブリッジング**: ルールがトリガーされると、メッセージを GCP Pub/Sub に転送するアクションが実行されます。データのプロパティ設定、オーダーキーの指定、MQTT トピックと GCP Pub/Sub トピックのマッピングが容易に設定可能です。これにより、より豊富なコンテキスト情報と順序保証を持つデータ統合が実現し、柔軟な IoT データ処理が可能となります。

MQTT メッセージデータが GCP Pub/Sub に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理・分析：Dataflow、BigQuery、Pub/Sub のストリーミング機能など、Google Cloud の強力なデータ処理・分析ツールを活用し、メッセージデータのリアルタイム処理や分析を行い、貴重な洞察や意思決定支援を得られます。
- イベント駆動型機能：Cloud Functions や Cloud Run などの Google Cloud イベント処理をトリガーし、動的かつ柔軟な機能トリガーと処理を実現します。
- データ保存・共有：Cloud Storage や Firestore などの Google Cloud ストレージサービスにメッセージデータを送信し、大量データの安全な保存・管理を行います。これにより他の Google Cloud サービスと連携してデータ共有や分析を行い、多様なビジネスニーズに対応可能です。

## 特長とメリット

GCP Pub/Sub とのデータ統合は以下のような特長とメリットを提供します。

- **堅牢なメッセージングサービス**：EMQX と GCP Pub/Sub は共に高可用性とスケーラビリティを備え、大規模なメッセージストリームの信頼性の高い受信、配信、処理を保証します。IoT データの順序性、メッセージ品質保証、パーシステンスをサポートし、メッセージの確実な伝送と処理を実現します。
- **柔軟なルールエンジン**：内蔵のルールエンジンにより、特定の送信元メッセージやイベントをトピックマッチングに基づいて処理可能です。データ形式変換、特定情報のフィルタリング、コンテキスト情報の付加などが行え、GCP Pub/Sub と組み合わせてさらなる処理や分析が可能です。
- **豊富なコンテキスト情報**：GCP Pub/Sub データ統合を通じて、クライアント属性を Pub/Sub 属性にマッピングしたり、ソートキーを設定したりすることで、メッセージにより豊かなコンテキスト情報を付加できます。これにより、後続のアプリケーション開発やデータ処理でより精密な分析・処理が可能となります。

まとめると、EMQX と GCP Pub/Sub の統合により、高信頼かつスケーラブルなメッセージ配信と、データ分析・統合のための豊富なツール・サービスを活用できます。これにより、堅牢な IoT アプリケーションの構築や、イベント駆動型の柔軟なビジネスロジックの実装が可能となります。

## はじめる前に

このセクションでは、GCP Pub/Sub データ統合の作成を開始する前に必要な準備について説明します。

### 前提条件

- EMQX のデータ統合に関する [ルール](./rules.md) の知識
- [データ統合](./data-bridges.md) に関する知識

### GCP でのサービスアカウントキーの作成

GCP Pub/Sub サービスを利用するには、サービスアカウントとサービスアカウントキーの作成が必要です。

1. GCP アカウントで [サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount) を作成します。サービスアカウントには、対象トピックへのメッセージの検査/読み取りおよびパブリッシュ権限（例：Pub/Sub Editor ロール）があることを確認してください。

2. 作成したサービスアカウントのメールアドレスをクリックし、**Key** タブを開きます。**Add key** のドロップダウンリストから **Create new key** を選択し、そのアカウント用のサービスアカウントキーを作成して JSON 形式でダウンロードします。

   ::: tip

   サービスアカウントキーは後で使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### ワークロード ID 連携の設定

ワークロード ID 連携（WIF）を使用すると、EMQX は長期有効なサービスアカウント キーファイルなしで GCP リソースにアクセスできます。EMQX は外部 ID プロバイダー（Microsoft Azure など）から取得した token を GCP Security Token Service 経由で一時的な GCP token に交換し、指定したサービスアカウントを借用します。Token の更新は EMQX が自動的に処理します。

WIF を使用するには、コネクターを作成する前に GCP プロジェクトで以下の設定を行ってください。

1. Google Cloud コンソールで **IAM と管理** -> **ワークロード ID 連携** に移動し、ワークロード ID プールを作成して、**プール ID** と**プロジェクト番号**を控えておいてください。

2. プールにプロバイダーを追加して**プロバイダー ID** を控えます。OIDC ベースの認証を使用する場合は、外部 ID プロバイダーから OAuth 2.0 クライアント資格情報（クライアント ID、クライアント シークレット、トークン エンドポイント URI）を取得してください。

3. Pub/Sub トピックへのアクセス権を持つ GCP サービスアカウントを借用できるよう、ワークロード ID プールに権限を付与します。コネクターの設定時にサービスアカウントのメールアドレスが必要です。

   ::: tip

   詳細な設定手順については、[ワークロード ID 連携の設定](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers) を参照してください。

   :::

**例：Microsoft Azure（Entra ID）**

[Microsoft Entra ID](https://portal.azure.com/) で API を公開するアプリケーションを登録し、クライアント シークレットを作成してください。コネクターの設定時には以下の値を使用します。

| コネクターフィールド | 値 |
|---|---|
| **Endpoint URI** | `https://login.microsoftonline.com/<テナント ID>/oauth2/v2.0/token` |
| **OAuth Client ID** | アプリケーション（クライアント）ID（`api://<アプリケーション ID>` の形式） |
| **OAuth Client Secret** | アプリケーションに対して生成したクライアント シークレット |
| **OAuth Request Scope** | `api://<アプリケーション ID>/.default` |

::: note

**OAuth Request Scope** はアプリケーションのオーディエンス（`aud`）と完全に一致している必要があります。一致しない場合、GCP STS とのトークン交換が失敗します。詳細は Microsoft ドキュメントの [OAuth 2.0 クライアント資格情報フロー](https://learn.microsoft.com/ja-jp/entra/identity-platform/v2-oauth2-client-creds-grant-flow) を参照してください。

:::

### GCP でのトピックの作成と管理

EMQX で GCP Pub/Sub データ統合を設定する前に、トピックを作成し、GCP での基本的な管理操作に慣れておく必要があります。

1. Google Cloud コンソールで **Pub/Sub** -> **Topics** ページに移動します。詳細な手順は [トピックの作成と管理](https://cloud.google.com/pubsub/docs/create-topic) を参照してください。

   ::: tip

   サービスアカウントには、そのトピックへのパブリッシュ権限が必要です。

   :::

2. **Topic ID** フィールドにトピックの ID を入力し、**Create topic** をクリックします。

   <img src="./assets/gcp_pubsub/create-topic-GCP-console.png" alt="GCP コンソールでのトピック作成" style="zoom:50%;" />

3. **Subscriptions** ページに移動し、リストの **Topic ID** をクリックします。トピックに対するサブスクリプションを作成します。

   - **Delivery type** で **Pull** を選択します。
   - **Message retention duration** で `7` 日を選択します。

   詳細は [GCP Pub/Sub サブスクリプション](https://cloud.google.com/pubsub/docs/subscriber) を参照してください。

   <img src="./assets/gcp_pubsub/add-subscription-to-topic.png" alt="トピックへのサブスクリプション追加" style="zoom:50%;" />

4. **Subscription ID** -> **Messages** -> **Pull** をクリックすると、トピックに送信されたメッセージを確認できます。

   <img src="./assets/gcp_pubsub/subscriptions-id.png" alt="サブスクリプションID" style="zoom:50%;" />

   <img src="./assets/gcp_pubsub/subscriptions-id-pull.png" alt="サブスクリプションIDのメッセージプル" style="zoom:50%;" />

## GCP Pub/Sub プロデューサーコネクターの作成

GCP Pub/Sub プロデューサー Sink アクションを追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するため、GCP Pub/Sub プロデューサーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Producer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubproducer`）。この名前は GCP Pub/Sub プロデューサー Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **Authentication** ドロップダウンで認証方式を選択し、対応するフィールドを入力します。
   - **Service Account JSON**：[GCP でのサービスアカウントキーの作成](#gcp-でのサービスアカウントキーの作成) でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
   - **Workload Identity Federation (WIF)**：以下のフィールドを入力します。前提条件については [ワークロード ID 連携の設定](#ワークロード-id-連携の設定) を参照してください。
     - **GCP Project ID**：コネクターがアクセスするリソースのプロジェクト ID。
     - **GCP Project Number**：コネクターがアクセスするリソースのプロジェクト番号。
     - **Service Account Email**：借用するサービスアカウントのメールアドレス。
     - **Workload Identity Pool ID**：WIF トークン交換で使用するワークロード ID プールの ID。
     - **Workload Identity Provider ID**：WIF トークン交換で使用するワークロード ID プロバイダーの ID。
     - **Initial Token Configuration** の下で、資格情報の種類を選択して対応するフィールドを入力します。現在は **OIDC with Client Credentials Grant Type** のみをサポートしています。
       - **Endpoint URI**：OIDC プロバイダーの OAuth トークン エンドポイント URI。
       - **OAuth Client ID**：OAuth サーバーにトークンを要求するためのクライアント ID。
       - **OAuth Client Secret**：OAuth サーバーにトークンを要求するためのクライアント シークレット。
       - **OAuth Request Scope**：OAuth アクセス トークンを要求する際に指定する `scope`（プロバイダーが要求する場合に入力）。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテスト可能です。
6. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして Sink を指定したルール作成を続行できます。詳細は [GCP Pub/Sub プロデューサー Sink を用いたルール作成](#create-a-rule-with-gcp-pub-sub-producer-sink) を参照してください。

## GCP Pub/Sub プロデューサー Sink を用いたルールの作成

このセクションでは、GCP Pub/Sub に保存するデータを指定するルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` を入力します。

4. **SQL Editor** でルールを設定します。例えば、トピック `/devices/+/events` の MQTT メッセージを GCP Pub/Sub に保存したい場合、以下の SQL 文を使用します。

   注意：独自の SQL 文を指定する場合、`SELECT` 部分に Sink のペイロードテンプレートで必要なすべてのフィールドが含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "/devices/+/events"
   ```

   注意：初心者の場合は、**SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Google PubSub Producer` を選択すると、EMQX はルールで処理されたデータを GCP Pub/Sub に送信します。

6. **Action** ドロップダウンは `Create Action` のままにします。あるいは、以前に作成した GCP Pub/Sub プロデューサー Sink を選択することも可能です。本デモでは新しい Sink を作成してルールに追加します。

7. **Name** フィールドに Sink の名前を入力します。名前は英数字の組み合わせで指定してください。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubprodcer` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメータの詳細は [コネクターの作成](#create-a-connector) を参照してください。

9. **GCP PubSub Topic** に、[GCP でのトピックの作成と管理](#gcp-でのトピックの作成と管理) で作成したトピック ID `my-iot-core` を入力します。

10. **Payload Template** にテンプレートを定義するか空欄のままにします。

    - 空欄の場合、MQTT メッセージのクライアントID、トピック、ペイロードなどの可視入力をすべて JSON 形式でエンコードします。
    - 定義したテンプレートを使用する場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応する値で置換されます。例：`${topic}` は MQTT メッセージのトピックが `my/topic` なら `my/topic` に置換されます。

11. **Attributes Template** および **Ordering Key Template** にて、送信メッセージの属性やオーダーキーのフォーマットテンプレートを定義します（任意）。

    - **Attributes** はキーと値の両方に `${variable_name}` 形式のプレースホルダーを使用可能で、MQTT コンテキストから値が抽出されます。キーのテンプレートが空文字列になる場合、そのキーは GCP Pub/Sub 送信メッセージから除外されます。
    - **Ordering Key** は `${variable_name}` 形式のプレースホルダーを使用可能で、解決後の値が空文字列の場合は GCP Pub/Sub 送信メッセージの `orderingKey` フィールドは設定されません。

12. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

13. **詳細設定（任意）**：詳細は [Sink の機能](./data-bridges.md#features-of-sink) を参照してください。

14. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテスト可能です。

15. **Create** ボタンをクリックして Sink の設定を完了すると、新しい Sink が **Action Outputs** タブに表示されます。

16. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成します。

これでルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Google PubSub Producer Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `/devices/+/events` のメッセージがルール `my_rule` によって解析され、GCP Pub/Sub に送信・保存されていることが確認できます。

## プロデューサールールのテスト

1. MQTTX を使ってトピック `/devices/+/events` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t /devices/+/events -m '{ "msg": "hello GCP PubSub" }'
   ```

2. Sink の稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあるはずです。

3. GCP の **Pub/Sub** -> **Subscriptions** に移動し、**MESSAGES** タブをクリックするとメッセージが確認できます。

## GCP Pub/Sub コンシューマーコネクターの作成

GCP Pub/Sub コンシューマー Sink を追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するため、GCP Pub/Sub コンシューマーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Consumer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubconsumer`）。この名前は GCP Pub/Sub コンシューマーコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **Authentication** ドロップダウンで認証方式を選択し、対応するフィールドを入力します。
   - **Service Account JSON**：[GCP でのサービスアカウントキーの作成](#gcp-でのサービスアカウントキーの作成) でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
   - **Workload Identity Federation (WIF)**：以下のフィールドを入力します。前提条件については [ワークロード ID 連携の設定](#ワークロード-id-連携の設定) を参照してください。
     - **GCP Project ID**：コネクターがアクセスするリソースのプロジェクト ID。
     - **GCP Project Number**：コネクターがアクセスするリソースのプロジェクト番号。
     - **Service Account Email**：借用するサービスアカウントのメールアドレス。
     - **Workload Identity Pool ID**：WIF トークン交換で使用するワークロード ID プールの ID。
     - **Workload Identity Provider ID**：WIF トークン交換で使用するワークロード ID プロバイダーの ID。
     - **Initial Token Configuration** の下で、資格情報の種類を選択して対応するフィールドを入力します。現在は **OIDC with Client Credentials Grant Type** のみをサポートしています。
       - **Endpoint URI**：OIDC プロバイダーの OAuth トークン エンドポイント URI。
       - **OAuth Client ID**：OAuth サーバーにトークンを要求するためのクライアント ID。
       - **OAuth Client Secret**：OAuth サーバーにトークンを要求するためのクライアント シークレット。
       - **OAuth Request Scope**：OAuth アクセス トークンを要求する際に指定する `scope`（プロバイダーが要求する場合に入力）。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテスト可能です。
6. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして GCP Pub/Sub コンシューマーソースを用いたルール作成を続行できます。詳細は [GCP Pub/Sub コンシューマーソースを用いたルール作成](#create-a-rule-with-gcp-pub-sub-cconsumer-source) を参照してください。

## GCP Pub/Sub コンシューマーソースを用いたルールの作成

このセクションでは、GCP Pub/Sub からメッセージを消費し、EMQX に転送するルールの作成方法を説明します。Google PubSub コンシューマーソースを作成・設定し、ルールのデータ入力として追加します。また、メッセージを EMQX に転送するために Republish アクションをルールに追加します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule_source` を入力します。

4. 右側の **Data Inputs** タブでデフォルトの Input `Messages` を削除し、**Add Input** をクリックします。

5. **Input Type** ドロップダウンから `Google PubSub Consumer` を選択します。

6. **Source** ドロップダウンはデフォルトの `Create Source` のままにします。本デモでは新しいソースを作成してルールに追加します。

7. ソースの **Name** と（任意の）**Description** を入力します。名前は英数字の組み合わせで、例：`my-gcppubsub-source`。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubconsumer` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメータの詳細は [コネクターの作成](#create-a-connector) を参照してください。

9. GCP Pub/Sub から EMQX へメッセージを消費するため、以下の情報を設定します。

   - **GCP PubSub Topic**：消費対象の GCP Pub/Sub トピック名を入力します（例：`my-iot-core`）。
   - **Maximum Messages to Pull**：1 回のプルリクエストで取得する最大メッセージ数を指定します。実際の取得数は指定値未満の場合があります。

10. 詳細設定（任意）：詳細は [Sink の機能](./data-bridges.md#features-of-sink) を参照してください。

11. **Create** をクリックする前に、**Test Connectivity** をクリックして GCP Pub/Sub サーバーへの接続が成功するかテスト可能です。

12. **Create** をクリックしてソースの作成を完了します。ソースはルールの **Data Inputs** タブに追加され、**SQL Editor** のルールは以下のようになります。

    ```sql
    SELECT
      *
    FROM
      "$bridges/gcppubsub:my-gcppubsub-source"
    ```

    注意：初心者の場合は、**SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

    `my-gcppubsub-source` から、ルール SQL は以下の GCP Pub/Sub から MQTT トピックへのマッピングテーブルに示す GCP Pub/Sub メッセージフィールドにアクセス可能です。ルール SQL を調整してデータ処理を行えます。本例ではデフォルトの SQL を使用します。

    | フィールド名           | 説明                                                         |
    | ---------------------- | ------------------------------------------------------------ |
    | `attributes`           | （任意）文字列のキー・バリューのペアを含むオブジェクト（存在する場合） |
    | `message_id`           | GCP Pub/Sub がこのメッセージに割り当てたメッセージ ID       |
    | `ordering_key`         | （任意）メッセージの順序キー（存在する場合）                 |
    | `publishing_time`      | GCP Pub/Sub によるメッセージのタイムスタンプ                 |
    | `topic`                | 元の GCP Pub/Sub トピック名                                   |
    | `value`                | （任意）メッセージのペイロード（存在する場合）               |

    **注意**：各 GCP Pub/Sub から MQTT トピックへのマッピングは、ユニークな GCP Pub/Sub トピック名を含む必要があります。つまり、同じ GCP Pub/Sub トピックが複数のマッピングに存在してはなりません。

これで GCP Pub/Sub コンシューマーソースの作成は完了しましたが、メッセージはまだ直接 EMQX にパブリッシュされません。次に、[ルールへの Republish アクションの追加](#add-republish-action-to-the-rule) を続けて、Republish アクションを作成しルールに追加してください。

### ルールへの Republish アクションの追加

このセクションでは、GCP Pub/Sub コンシューマーソースから消費したメッセージを転送し、EMQX トピック `t/1` にパブリッシュするための Republish アクションをルールに追加する方法を説明します。

1. ページ右側の **Action Output** タブを選択し、**Add Action** ボタンをクリックして、**Type of Action** ドロップダウンリストから `Republish` アクションを選択します。

2. メッセージ再パブリッシュの設定を入力します。

   - **Topic**：MQTT にパブリッシュするトピックを指定します。ここでは `t/1` と入力します。

   - **QoS**：`0`、`1`、`2`、`${qos}` のいずれかを選択、または他のフィールドから QoS を設定するためのプレースホルダーを入力します。`${qos}` を選択すると元のメッセージの QoS に従います。

   - **Retain**：`true` または `false` を選択します。メッセージをリテインメッセージとしてパブリッシュするかどうかを決定します。プレースホルダーも使用可能です。本例では `false` を選択します。

   - **Payload**：転送するメッセージペイロードのテンプレートを設定します。空欄の場合はルールの出力結果をそのまま転送します。ここでは `${payload}` を入力し、ペイロードのみを転送します。

     MQTT ペイロードテンプレートのデフォルト値は `${.}` で、利用可能なすべてのデータを JSON オブジェクトとしてエンコードします。例えば、すべてのオプションフィールドを含む GCP Pub/Sub メッセージに対して `${.}` をテンプレートに指定すると、以下のような JSON が生成されます。

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

     GCP Pub/Sub メッセージのサブフィールドはドット表記でアクセス可能です。例：`${.value}` は GCP Pub/Sub メッセージの値に解決され、`${.attributes.h1}` は `h1` というメッセージ属性キーの値に解決されます。存在しない値は空文字列に置換されます。

   - **MQTT 5.0 メッセージプロパティ**：デフォルトで無効です。詳細設定は [Republish アクションの追加](./rule-get-started.md#add-republish-action) を参照してください。

3. **Create** をクリックしてアクション作成を完了します。作成成功後、ルール作成ページに戻り、Republish アクションが **Action Outputs** タブに追加されます。

4. ルール作成ページで **Create** ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成されました。**Rules** ページで新規作成したルールを確認できます。**Sources** タブで新しく作成した GCP Pub/Sub コンシューマーソースを確認できます。

また、**Integrate** -> **Flow Designer** をクリックするとトポロジーが表示され、GCP Pub/Sub コンシューマーソースからのメッセージが Republish を経由して `t/1` にパブリッシュされる様子を直感的に確認できます。

## <!--Test the Consumer Rule-->
