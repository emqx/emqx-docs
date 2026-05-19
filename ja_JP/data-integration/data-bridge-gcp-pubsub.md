# GCP Pub/Sub への MQTT データ取り込み

[Google Cloud Pub/Sub](https://cloud.google.com/pubsub?hl=en-us) は、非常に高い信頼性とスケーラビリティを実現するために設計された非同期メッセージングサービスです。EMQX は、MQTT データのリアルタイム抽出、処理、分析のために Google Cloud Pub/Sub とのシームレスな統合をサポートしています。Cloud Functions、App Engine、Cloud Run、Kubernetes Engine、Compute Engine などのさまざまな Google Cloud サービスへデータをプッシュできます。また、Google Cloud から MQTT へのデータ配信も可能で、ユーザーが GCP 上で迅速に IoT アプリケーションを構築できるよう支援します。

本ページでは、EMQX と GCP Pub/Sub 間のデータ統合について包括的に紹介し、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作概要

GCP Pub/Sub データ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Google Cloud とシームレスに統合し、豊富なサービスや機能を活用して IoT アプリケーション開発を支援します。

![GCP_bridge_architect](./assets/gcp_pubsub/GCP_bridge_architect.png)

EMQX はルールエンジンと Sink を通じて MQTT データを GCP Pub/Sub に転送します。ここでは GCP Pub/Sub のプロデューサー役割の例を示します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**: デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**: 組み込みのルールエンジンは、特定のソースからの MQTT メッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、文脈情報の付加などの処理を行います。
3. **GCP Pub/Sub へのブリッジ**: ルールはメッセージを GCP Pub/Sub に転送するアクションをトリガーし、データプロパティ、オーダーキー、MQTT トピックと GCP Pub/Sub トピックのマッピングを簡単に設定できます。これにより、より豊富な文脈情報と順序保証を持つデータ統合が可能となり、柔軟な IoT データ処理を実現します。

MQTT メッセージデータが GCP Pub/Sub に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- **リアルタイムデータ処理と分析**: Dataflow、BigQuery、Pub/Sub のストリーミング機能など、Google Cloud の強力なデータ処理・分析ツールを活用し、メッセージデータのリアルタイム処理と分析を行い、有益なインサイトや意思決定支援を得られます。
- **イベント駆動型機能**: Cloud Functions や Cloud Run などの Google Cloud のイベント処理をトリガーし、動的かつ柔軟な関数の起動と処理を実現します。
- **データ保存と共有**: Cloud Storage や Firestore などの Google Cloud ストレージサービスにメッセージデータを送信し、大量データの安全な保存と管理を行います。これにより、他の Google Cloud サービスと連携してデータを共有・分析し、多様なビジネスニーズに対応できます。

## 特長とメリット

GCP Pub/Sub とのデータ統合は、以下のような特長とメリットを提供します。

- **堅牢なメッセージングサービス**: EMQX と GCP Pub/Sub は共に高可用性とスケーラビリティを備え、大規模なメッセージストリームの信頼性の高い受信、配信、処理を保証します。IoT データのシーケンス管理、メッセージ品質保証、パーシステンスをサポートし、メッセージの確実な送受信を実現します。
- **柔軟なルールエンジン**: 組み込みのルールエンジンにより、特定のソースメッセージやイベントをトピックマッチングに基づいて処理可能です。データ形式変換、特定情報のフィルタリング、文脈情報の付加などの操作が可能で、GCP Pub/Sub と組み合わせてさらなる処理・分析が行えます。
- **豊富な文脈情報**: GCP Pub/Sub データ統合を通じて、メッセージにより豊かな文脈情報を付加できます。クライアント属性を Pub/Sub 属性やソートキーにマッピングすることで、後続のアプリケーション開発やデータ処理においてより精緻な分析・処理を支援します。

まとめると、EMQX と GCP Pub/Sub の統合により、高信頼かつスケーラブルなメッセージ配信が可能となり、データ分析や統合のための豊富なツールとサービスを活用できます。これにより、堅牢な IoT アプリケーションの構築や、イベント駆動型の柔軟なビジネスロジックの実装が可能となります。

## はじめる前に

ここでは、GCP Pub/Sub データ統合の作成を始める前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GCP でのサービスアカウントキーの作成

GCP Pub/Sub サービスを利用するには、サービスアカウントとサービスアカウントキーを作成する必要があります。

1. GCP アカウントで[サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)を作成します。サービスアカウントには、対象トピックへのメッセージの検査/読み取りおよびパブリッシュ権限（例：Pub/Sub Editor ロール）が付与されていることを確認してください。

2. 作成したサービスアカウントのメールアドレスをクリックし、**Key** タブを選択します。**Add key** のドロップダウンリストから **Create new key** を選択し、そのアカウントのサービスアカウントキーを JSON 形式で作成・ダウンロードします。

   ::: tip

   サービスアカウントキーは後で使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCP での Workload Identity Federation の設定

Workload Identity Federation（WIF）を利用すると、EMQX は長期間有効なサービスアカウントキーを使わずに GCP リソースへアクセスできます。EMQX は外部 ID プロバイダー（例：Microsoft Azure）からのトークンを GCP の Security Token Service 経由で一時的な GCP トークンに交換し、そのトークンを使って GCP サービスアカウントを代行します。トークンの更新は自動で行われます。

WIF を利用するには、コネクター作成前に GCP プロジェクトで以下を完了してください。

1. Google Cloud コンソールで **IAM & Admin** -> **Workload Identity Federation** に移動し、ワークロードアイデンティティプールを作成します。**Pool ID** と **Project Number** を控えておきます。

2. プールにプロバイダーを追加し、**Provider ID** を控えます。OIDC ベースの認証の場合は、外部 ID プロバイダーから OAuth 2.0 クライアント資格情報（クライアント ID、クライアントシークレット、トークンエンドポイント URI）を取得します。

3. Pub/Sub トピックにアクセス可能な GCP サービスアカウントを代行する権限をワークロードアイデンティティプールに付与します。コネクター設定時にサービスアカウントのメールアドレスが必要です。

   ::: tip

   詳細な手順は[Workload Identity Federation の設定](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers)をご参照ください。

   :::

**例：Microsoft Azure (Entra ID)**

[Microsoft Entra ID](https://portal.azure.com/)で API を公開するアプリケーションを登録し、クライアントシークレットを作成します。コネクター設定時には以下の値を使用します。

| コネクター項目 | 値 |
|---|---|
| **Endpoint URI** | `https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token` |
| **OAuth Client ID** | `api://<application-id>` 形式のアプリケーション（クライアント）ID |
| **OAuth Client Secret** | アプリケーション用に生成したクライアントシークレット |
| **OAuth Request Scope** | `api://<application-id>/.default` |

::: tip 注意

`scope` はアプリケーションのオーディエンス（`aud`）と完全に一致する必要があります。そうでない場合、GCP STS とのトークン交換に失敗します。詳細は Microsoft のドキュメントの[OAuth 2.0 クライアント資格情報フロー](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-client-creds-grant-flow)を参照してください。

サービスアカウントに WIF プールへのアクセス権を付与する際は、Subject 値にアプリケーション ID ではなく **Object ID** を使用してください。Object ID は Azure ポータルのアプリケーションの概要ページの **Enterprise applications** に表示されます。

:::

### GCP でのトピック作成と管理

EMQX で GCP Pub/Sub データ統合を設定する前に、トピックを作成し、GCP での基本的な管理操作に慣れておく必要があります。

1. Google Cloud コンソールで **Pub/Sub** -> **Topics** ページに移動します。詳細は[トピックの作成と管理](https://cloud.google.com/pubsub/docs/create-topic)を参照してください。

   ::: tip

   サービスアカウントには該当トピックへのパブリッシュ権限が必要です。

   :::

2. **Topic ID** フィールドにトピックの ID を入力し、**Create topic** をクリックします。

   <img src="./assets/gcp_pubsub/create-topic-GCP-console.png" alt="GCP コンソールでのトピック作成" style="zoom:50%;" />

3. **Subscriptions** ページに移動し、リストの中から作成したトピックの **Topic ID** をクリックします。トピックに対してサブスクリプションを作成します。

   - **Delivery type** で **Pull** を選択します。
   - **Message retention duration** は `7` 日を選択します。

   詳細は[GCP Pub/Sub サブスクリプション](https://cloud.google.com/pubsub/docs/subscriber)を参照してください。

   <img src="./assets/gcp_pubsub/add-subscription-to-topic.png" alt="トピックへのサブスクリプション追加" style="zoom:50%;" />

4. **Subscription ID** をクリックし、**Messages** -> **Pull** でトピックに送信されたメッセージを確認できます。

   <img src="./assets/gcp_pubsub/subscriptions-id.png" alt="サブスクリプション ID" style="zoom:50%;" />

   <img src="./assets/gcp_pubsub/subscriptions-id-pull.png" alt="サブスクリプションのメッセージプル" style="zoom:50%;" />

## GCP Pub/Sub プロデューサーコネクターの作成

GCP Pub/Sub プロデューサー Sink アクションを追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するための GCP Pub/Sub プロデューサーコネクターを作成します。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択ページで **Google PubSub Producer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubproducer`）。この名前は GCP Pub/Sub プロデューサー Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **Authentication** ドロップダウンから以下の認証方法のいずれかを選択し、対応するフィールドを入力します。
   - **Service Account JSON**: [GCP でのサービスアカウントキーの作成](#gcp-でのサービスアカウントキーの作成)でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
   - **Workload Identity Federation (WIF)**: 以下のフィールドを入力します。前提条件は[Workload Identity Federation の設定](#gcp-での-workload-identity-federation-の設定)を参照してください。
     - **GCP Project ID**: コネクターがアクセスするリソースのプロジェクト ID。
     - **GCP Project Number**: コネクターがアクセスするリソースのプロジェクト番号。
     - **Service Account Email**: 代行するサービスアカウントのメールアドレス。
     - **Workload Identity Pool ID**: WIF トークン交換で使用するワークロードアイデンティティプールの ID。
     - **Workload Identity Provider ID**: WIF トークン交換で使用するワークロードアイデンティティプロバイダーの ID。
     - **Initial Token Configuration** で認証情報タイプを選択し、対応するフィールドを入力します。現在サポートされているのは **OIDC with Client Credentials Grant Type** のみです。
       - **Endpoint URI**: OIDC プロバイダーの OAuth トークンエンドポイント URI。
       - **OAuth Client ID**: OAuth サーバーからトークンを要求するためのクライアント ID。
       - **OAuth Client Secret**: OAuth サーバーからトークンを要求するためのクライアントシークレット。
       - **OAuth Request Scope**: OAuth アクセストークン要求時に提供する `scope`。プロバイダーによっては必須。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして GCP Pub/Sub Producer Sink を指定するルール作成に進めます。詳細は[Create a Rule with GCP Pub/Sub Producer Sink](#create-a-rule-with-gcp-pub-sub-producer-sink)を参照してください。

## GCP Pub/Sub プロデューサー Sink を使ったルールの作成

ここでは、GCP Pub/Sub に保存するデータを指定するルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` と入力します。

4. **SQL Editor** でルールを設定します。例えば、トピック `/devices/+/events` の MQTT メッセージを GCP Pub/Sub に保存したい場合、以下の SQL 文を使用できます。

   注意: 独自の SQL 文を指定する場合は、Sink のペイロードテンプレートで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "/devices/+/events"
   ```

   注意: 初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンから `Google PubSub Producer` を選択すると、EMQX がルールで処理したデータを GCP Pub/Sub に送信します。

6. **Action** ドロップダウンは `Create Action` のままにするか、既存の GCP Pub/Sub プロデューサー Sink を選択できます。ここでは新しい Sink を作成し、ルールに追加します。

7. **Name** フィールドに Sink の名前を入力します。名前は英数字の組み合わせとしてください。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubprodcer` を選択します。隣のボタンで新しいコネクターを作成することも可能です。設定パラメーターの詳細は[Create a Connector](#create-a-connector)を参照してください。

9. **GCP PubSub Topic** に、[GCP でのトピック作成と管理](#gcp-でのトピック作成と管理)で作成したトピック ID `my-iot-core` を入力します。

10. **Payload Template** にテンプレートを定義するか、空欄のままにします。

    - 空欄の場合、MQTT メッセージのクライアント ID、トピック、ペイロードなどの可視入力を JSON 形式でエンコードします。
    - テンプレートを使用する場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応する値で置き換えられます。例えば `${topic}` は MQTT メッセージのトピック `my/topic` に置き換わります。

11. **Attributes Template** および **Ordering Key Template** で送信メッセージの属性やオーダーキーのフォーマットテンプレートを定義します（任意）。

    - **Attributes** ではキーと値の両方に `${variable_name}` 形式のプレースホルダーを使用可能で、MQTT コンテキストから値が抽出されます。キーのテンプレートが空文字列になる場合、そのキーは GCP Pub/Sub 送信メッセージから省略されます。
    - **Ordering Key** も `${variable_name}` 形式のプレースホルダーを使用可能で、解決結果が空文字列の場合は GCP Pub/Sub 送信メッセージの `orderingKey` フィールドは設定されません。

12. **Fallback Actions（任意）**: メッセージ配信失敗時の信頼性向上のために、1つ以上のフォールバックアクションを定義できます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

13. **高度な設定（任意）**: 詳細は[Features of Sink](./data-bridges.md#features-of-sink)を参照してください。

14. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテスト可能です。

15. **Create** ボタンをクリックして Sink の設定を完了すると、**Action Outputs** タブに新しい Sink が表示されます。

16. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成します。

これでルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Google PubSub Producer Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、ルール `my_rule` によってトピック `/devices/+/events` のメッセージが解析され、GCP Pub/Sub に送信・保存されていることが視覚的に把握できます。

## プロデューサールールのテスト

1. MQTTX を使ってトピック `/devices/+/events` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t /devices/+/events -m '{ "msg": "hello GCP PubSub" }'
   ```

2. Sink の稼働状況を確認し、新しい受信メッセージと送信メッセージがそれぞれ 1 件ずつあることを確認します。

3. GCP の **Pub/Sub** -> **Subscriptions** に移動し、**MESSAGES** タブをクリックするとメッセージを確認できます。

## GCP Pub/Sub コンシューマーコネクターの作成

GCP Pub/Sub コンシューマー Sink を追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するための GCP Pub/Sub コンシューマーコネクターを作成します。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択ページで **Google PubSub Consumer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubconsumer`）。この名前は GCP Pub/Sub コンシューマー Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **Authentication** ドロップダウンから以下の認証方法のいずれかを選択し、対応するフィールドを入力します。
   - **Service Account JSON**: [GCP でのサービスアカウントキーの作成](#gcp-でのサービスアカウントキーの作成)でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
   - **Workload Identity Federation (WIF)**: 以下のフィールドを入力します。前提条件は[Workload Identity Federation の設定](#gcp-での-workload-identity-federation-の設定)を参照してください。
     - **GCP Project ID**: コネクターがアクセスするリソースのプロジェクト ID。
     - **GCP Project Number**: コネクターがアクセスするリソースのプロジェクト番号。
     - **Service Account Email**: 代行するサービスアカウントのメールアドレス。
     - **Workload Identity Pool ID**: WIF トークン交換で使用するワークロードアイデンティティプールの ID。
     - **Workload Identity Provider ID**: WIF トークン交換で使用するワークロードアイデンティティプロバイダーの ID。
     - **Initial Token Configuration** で認証情報タイプを選択し、対応するフィールドを入力します。現在サポートされているのは **OIDC with Client Credentials Grant Type** のみです。
       - **Endpoint URI**: OIDC プロバイダーの OAuth トークンエンドポイント URI。
       - **OAuth Client ID**: OAuth サーバーからトークンを要求するためのクライアント ID。
       - **OAuth Client Secret**: OAuth サーバーからトークンを要求するためのクライアントシークレット。
       - **OAuth Request Scope**: OAuth アクセストークン要求時に提供する `scope`。プロバイダーによっては必須。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして GCP Pub/Sub コンシューマーソースを使用するルール作成に進めます。詳細は[Create a Rule with GCP Pub/Sub Consumer Source](#create-a-rule-with-gcp-pub-sub-cconsumer-source)を参照してください。

## GCP Pub/Sub コンシューマーソースを使ったルールの作成

ここでは、GCP Pub/Sub からメッセージを消費し、EMQX に転送するルールの作成方法を示します。Google PubSub コンシューマーソースを作成・設定し、ルールのデータ入力として追加します。また、メッセージを EMQX に転送するために Republish アクションをルールに追加します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule_source` と入力します。

4. 右側の **Data Inputs** タブで、デフォルトの Input `Messages` を削除し、**Add Input** をクリックします。

5. **Input Type** ドロップダウンから `Google PubSub Consumer` を選択します。

6. **Source** ドロップダウンはデフォルトの `Create Source` のままにします。ここでは新しいソースを作成し、ルールに追加します。

7. ソースの **Name** と（任意で）**Description** を入力します。名前は英数字の組み合わせで、例として `my-gcppubsub-source` などとします。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubconsumer` を選択します。隣のボタンで新しいコネクターを作成することも可能です。設定パラメーターの詳細は[Create a Connector](#create-a-connector)を参照してください。

9. GCP Pub/Sub から EMQX へメッセージを消費するためのソース情報を設定します。

   - **GCP PubSub Topic**: 消費対象の GCP Pub/Sub トピック名を入力します（例：`my-iot-core`）。
   - **Maximum Messages to Pull**: 1 回のプルリクエストで取得する最大メッセージ数を指定します。実際の取得数は指定値より少ない場合があります。

10. 高度な設定（任意）: 詳細は[Features of Sink](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** をクリックする前に、**Test Connectivity** をクリックして GCP Pub/Sub サーバーへの接続が成功するかテスト可能です。

12. **Create** をクリックしてソース作成を完了します。ソースはルールの **Data Inputs** タブに追加され、**SQL Editor** のルールは以下のようになります。

    ```sql
    SELECT
      *
    FROM
      "$bridges/gcppubsub:my-gcppubsub-source"
    ```

    注意: 初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

    `my-gcppubsub-source` からは、以下の GCP Pub/Sub から MQTT トピックへのマッピングテーブルに示すフィールドをルールの SQL で参照できます。必要に応じてデータ処理用に SQL を調整可能です。ここではデフォルトの SQL を使用します。

    | フィールド名         | 説明                                                         |
    | -------------------- | ------------------------------------------------------------ |
    | `attributes`         | （任意）文字列のキーと値のペアを含むオブジェクト（存在する場合） |
    | `message_id`         | GCP Pub/Sub がこのメッセージに割り当てたメッセージ ID       |
    | `ordering_key`       | （任意）メッセージの順序付けキー（存在する場合）             |
    | `publishing_time`    | GCP Pub/Sub によって定義されたメッセージのタイムスタンプ      |
    | `topic`              | 発信元の GCP Pub/Sub トピック                                |
    | `value`              | （任意）メッセージのペイロード（存在する場合）                |

    **注意**: 各 GCP Pub/Sub から MQTT トピックへのマッピングは、ユニークな GCP Pub/Sub トピック名を含む必要があります。つまり、同じ GCP Pub/Sub トピックが複数のマッピングに存在してはなりません。

これで GCP Pub/Sub コンシューマーソースが正常に作成されましたが、メッセージはまだ直接 EMQX にパブリッシュされません。次に、[Add Republish Action to the Rule](#add-republish-action-to-the-rule) の手順に従い、Republish アクションを作成してルールに追加してください。

### ルールへの Republish アクションの追加

ここでは、GCP Pub/Sub コンシューマーソースから消費したメッセージを転送し、EMQX トピック `t/1` にパブリッシュするための Republish アクションの追加方法を示します。

1. ページ右側の **Action Output** タブを選択し、**Add Action** ボタンをクリックします。**Type of Action** ドロップダウンから `Republish` アクションを選択します。

2. メッセージ再パブリッシュの設定を入力します。

   - **Topic**: MQTT にパブリッシュするトピック。ここでは `t/1` を入力します。

   - **QoS**: `0`、`1`、`2`、または `${qos}` を選択、もしくは他のフィールドから QoS を設定するためのプレースホルダーを入力します。`${qos}` を選択すると元のメッセージの QoS に従います。

   - **Retain**: `true` または `false` を選択します。メッセージをリテインメッセージとしてパブリッシュするかどうかを決定します。プレースホルダーを使って他のフィールドからリテインフラグを設定することも可能です。この例では `false` を選択します。

   - **Payload**: 転送するメッセージペイロードのテンプレートを設定します。空欄の場合はルールの出力結果をそのまま転送します。ここでは `${payload}` を入力し、ペイロードのみを転送することを示します。

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

     GCP Pub/Sub メッセージのサブフィールドはドット表記でアクセス可能です。例えば `${.value}` はメッセージのペイロード値に解決され、`${.attributes.h1}` は `h1` という属性キーの値に解決されます。存在しない値は空文字列に置き換えられます。

   - **MQTT 5.0 メッセージプロパティ**: デフォルトで無効です。詳細設定は[Republish アクションの追加](./rule-get-started.md#add-republish-action)を参照してください。

3. **Create** をクリックしてアクションの作成を完了します。作成に成功するとルール作成ページに戻り、Republish アクションが **Action Outputs** タブに追加されます。

4. ルール作成ページで **Create** ボタンをクリックし、ルール全体の作成を完了します。

これでルールが正常に作成されました。**Rules** ページで新規作成したルールを確認できます。**Sources** タブには新しい GCP Pub/Sub コンシューマーソースが表示されます。

また、**Integrate** -> **Flow Designer** をクリックしてトポロジーを確認できます。トポロジーを通じて、GCP Pub/Sub コンシューマーソースからのメッセージが Republish を介してトピック `t/1` にパブリッシュされる様子を直感的に把握できます。

## <!--Test the Consumer Rule-->
