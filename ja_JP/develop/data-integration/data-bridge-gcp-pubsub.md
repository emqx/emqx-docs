# GCP Pub/Sub への MQTT データ取り込み

[Google Cloud Pub/Sub](https://cloud.google.com/pubsub?hl=en-us) は、非常に高い信頼性とスケーラビリティを実現するために設計された非同期メッセージングサービスです。EMQX は Google Cloud Pub/Sub とシームレスに統合でき、MQTT データのリアルタイム抽出、処理、分析を可能にします。Cloud Functions、App Engine、Cloud Run、Kubernetes Engine、Compute Engine などのさまざまな Google Cloud サービスにデータをプッシュできます。また、Google Cloud から MQTT へのデータ配信も可能で、ユーザーが GCP 上で迅速に IoT アプリケーションを構築できるよう支援します。

本ページでは、EMQX と GCP Pub/Sub 間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に解説します。

## 動作概要

GCP Pub/Sub データ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Google Cloud とシームレスに連携させ、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう設計されています。

![GCP_bridge_architect](./assets/gcp_pubsub/GCP_bridge_architect.png)

EMQX はルールエンジンと Sink を介して MQTT データを GCP Pub/Sub に転送します。ここでは GCP Pub/Sub のプロデューサー役割の例を示します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**: デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**: 組み込みのルールエンジンは、特定のトピックにマッチする MQTT メッセージを処理します。ルールにマッチしたメッセージは、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
3. **GCP Pub/Sub へのブリッジング**: ルールはメッセージを GCP Pub/Sub に転送するアクションをトリガーします。データプロパティ、オーダーキー、MQTT トピックから GCP Pub/Sub トピックへのマッピングを簡単に設定でき、より豊富なコンテキスト情報や順序保証を提供し、柔軟な IoT データ処理を実現します。

MQTT メッセージデータが GCP Pub/Sub に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- **リアルタイムデータ処理と分析**: Dataflow、BigQuery、Pub/Sub のストリーミング機能など、強力な Google Cloud のデータ処理・分析ツールを活用し、メッセージデータのリアルタイム処理・分析を行い、有益なインサイトや意思決定支援を得られます。
- **イベント駆動型機能**: Cloud Functions や Cloud Run などの Google Cloud イベント処理をトリガーし、動的かつ柔軟な関数の起動と処理を実現します。
- **データの保存と共有**: Cloud Storage や Firestore などの Google Cloud ストレージサービスにメッセージデータを送信し、大量データの安全な保存・管理を行います。これにより、他の Google Cloud サービスと連携してデータの共有や分析が可能となり、さまざまなビジネスニーズに対応できます。

## 特長とメリット

GCP Pub/Sub とのデータ統合は以下の特長とメリットを提供します。

- **堅牢なメッセージングサービス**: EMQX と GCP Pub/Sub はいずれも高可用性とスケーラビリティを備え、大規模なメッセージストリームの信頼性の高い受信、配信、処理を保証します。IoT データの順序性、メッセージの QoS（サービス品質）、パーシステンス（永続化）をサポートし、メッセージの確実な伝送と処理を実現します。
- **柔軟なルールエンジン**: 組み込みのルールエンジンにより、特定の送信元メッセージやイベントをトピックマッチングに基づいて処理可能です。データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの操作が行え、GCP Pub/Sub と組み合わせてさらなる処理や分析が可能です。
- **豊富なコンテキスト情報**: GCP Pub/Sub データ統合を通じて、クライアント属性を Pub/Sub 属性やソーティングキーにマッピングするなど、より豊富なコンテキスト情報をメッセージに付加できます。これにより、後続のアプリケーション開発やデータ処理でより精密な分析や処理が可能となります。

まとめると、EMQX と GCP Pub/Sub の統合により、高信頼性かつスケーラブルなメッセージ配信が可能となり、データ分析や統合のための豊富なツールとサービスを活用できます。これにより、堅牢な IoT アプリケーションの構築やイベント駆動型の柔軟なビジネスロジックの実装が可能となります。

## はじめる前に

このセクションでは、GCP Pub/Sub データ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### GCP でサービスアカウントキーを作成する

**Service Account JSON** 認証を使用する場合、GCP でサービスアカウントを作成し、JSON 形式のキーを生成してください。

1. GCP アカウントで [サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount) を作成します。サービスアカウントには、対象トピックへのメッセージの検査/読み取りおよびパブリッシュ権限（例：Pub/Sub Editor ロール）が必要です。

2. 作成したサービスアカウントのメールアドレスをクリックし、**Key** タブを開きます。**Add key** のドロップダウンリストから **Create new key** を選択し、サービスアカウントキーを作成して JSON 形式でダウンロードします。

   ::: tip

   サービスアカウントキーは後で使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCP で Workload Identity Federation を設定する

Workload Identity Federation（WIF）を利用すると、EMQX は長期間有効なサービスアカウントキーを使わずに GCP リソースにアクセスできます。EMQX は外部 ID プロバイダー（例：Microsoft Azure）からのトークンを GCP の Security Token Service 経由で一時的な GCP トークンに交換し、そのトークンを使ってサービスアカウントを代行します。トークンの更新は自動で行われます。

WIF を利用するには、コネクター作成前に GCP プロジェクトで以下を完了してください。

1. Google Cloud コンソールで **IAM & Admin** -> **Workload Identity Federation** に移動し、ワークロードアイデンティティプールを作成します。**Pool ID** と **Project Number** を控えておきます。

2. プールにプロバイダーを追加し、**Provider ID** を控えます。OIDC ベースの認証の場合、外部 ID プロバイダーから OAuth 2.0 クライアント認証情報（クライアント ID、クライアントシークレット、トークンエンドポイント URI）を取得します。

3. Pub/Sub トピックにアクセスできる GCP サービスアカウントを代行する権限をワークロードアイデンティティプールに付与します。コネクター設定時にサービスアカウントのメールアドレスが必要です。

   ::: tip

   詳細は [Workload Identity Federation の設定](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers) を参照してください。

   :::

**例：Microsoft Azure (Entra ID)**

[Microsoft Entra ID](https://portal.azure.com/) で API を公開するアプリケーションを登録し、クライアントシークレットを作成します。コネクター設定時に以下の値を使用します。

| コネクター項目 | 値 |
|---|---|
| **Endpoint URI** | `https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token` |
| **OAuth Client ID** | アプリケーション（クライアント）ID、形式は `api://<application-id>` |
| **OAuth Client Secret** | アプリケーション用に生成したクライアントシークレット |
| **OAuth Request Scope** | `api://<application-id>/.default` |

::: tip 注意

`scope` はアプリケーションのオーディエンス（`aud`）と完全に一致する必要があります。そうでないと GCP STS とのトークン交換が失敗します。詳細は Microsoft の [OAuth 2.0 クライアント認証フロー](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-client-creds-grant-flow) を参照してください。

サービスアカウントに WIF プールのアクセス権を付与する際は、**Application ID** ではなく **Object ID** を Subject 値として使用してください。Object ID は Azure ポータルのアプリケーションの概要ページの **Enterprise applications** で確認できます。

:::

### Attached Service Account の前提条件

**Attached Service Account** 認証を使用する場合、EMQX はサービスアカウントがアタッチされた GCP Compute Engine インスタンス上で動作している必要があります。インスタンスの OAuth アクセススコープが Pub/Sub へのアクセスを許可していることを確認してください。Google は `cloud-platform` スコープ（`https://www.googleapis.com/auth/cloud-platform`）の使用を推奨し、サービスアカウントの権限は IAM ロールで制限することを推奨しています。サービスアカウントは対象の Pub/Sub トピックおよびサブスクリプションにアクセスできる権限を持つ必要があります。詳細は Google Cloud ドキュメントの [サービスアカウント](https://cloud.google.com/compute/docs/access/service-accounts) を参照してください。

対象の Pub/Sub トピックおよびサブスクリプションは、Compute Engine インスタンスに関連付けられた GCP プロジェクト内に存在する必要があります。EMQX クラスターの場合、すべてのノードがこれらの要件を満たし、そのプロジェクトの Compute Engine インスタンス上で動作している必要があります。

コネクター起動時に EMQX はインスタンスメタデータエンドポイントから GCP プロジェクト ID とアクセストークンを自動取得します。サービスアカウントキーのアップロードは不要です。

### GCP でトピックの作成と管理

EMQX で GCP Pub/Sub データ統合を設定する前に、トピックを作成し、GCP での基本的な管理操作に慣れておく必要があります。

1. Google Cloud コンソールで **Pub/Sub** -> **Topics** ページに移動します。詳細は [トピックの作成と管理](https://cloud.google.com/pubsub/docs/create-topic) を参照してください。

   ::: tip

   サービスアカウントには該当トピックへのパブリッシュ権限が必要です。

   :::

2. **Topic ID** フィールドにトピックの ID を入力し、**Create topic** をクリックします。

   <img src="./assets/gcp_pubsub/create-topic-GCP-console.png" alt="GCP コンソールでのトピック作成" style="zoom:50%;" />

3. **Subscriptions** ページに移動し、リストから作成したトピックの **Topic ID** をクリックします。トピックに対するサブスクリプションを作成します。

   - **Delivery type** で **Pull** を選択します。
   - **Message retention duration** は `7` 日を選択します。

   詳細は [GCP Pub/Sub サブスクリプション](https://cloud.google.com/pubsub/docs/subscriber) を参照してください。

   <img src="./assets/gcp_pubsub/add-subscription-to-topic.png" alt="トピックへのサブスクリプション追加" style="zoom:50%;" />

4. **Subscription ID** -> **Messages** -> **Pull** で、トピックに送信されたメッセージを確認できます。

   <img src="./assets/gcp_pubsub/subscriptions-id.png" alt="サブスクリプション ID" style="zoom:50%;" />

   <img src="./assets/gcp_pubsub/subscriptions-id-pull.png" alt="メッセージのプル表示" style="zoom:50%;" />

## GCP Pub/Sub プロデューサーコネクターの作成

GCP Pub/Sub プロデューサー Sink アクションを追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するための GCP Pub/Sub プロデューサーコネクターを作成します。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Producer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubproducer`）。名前は GCP Pub/Sub プロデューサー Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **Authentication** リストから以下の認証方法のいずれかを選択し、対応する項目を設定します。
   - **Service Account JSON**: [GCP でサービスアカウントキーを作成する](#gcp-でサービスアカウントキーを作成する) でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
   - **Workload Identity Federation (WIF)**: 以下の項目を入力します。前提条件は [GCP で Workload Identity Federation を設定する](#gcp-で-workload-identity-federation-を設定する) を参照してください。
     - **GCP Project ID**: コネクターがアクセスするリソースのプロジェクト ID。
     - **GCP Project Number**: コネクターがアクセスするリソースのプロジェクト番号。
     - **Service Account Email**: 代行するサービスアカウントのメールアドレス。
     - **Workload Identity Pool ID**: WIF トークン交換に使用するワークロードアイデンティティプールの ID。
     - **Workload Identity Provider ID**: WIF トークン交換に使用するワークロードアイデンティティプロバイダーの ID。
     - **Initial Token Configuration** で認証情報タイプを選択し、対応する項目を入力します。現在は **OIDC with Client Credentials Grant Type** のみサポートされています。
       - **Endpoint URI**: OIDC プロバイダーの OAuth トークンエンドポイント URI。
       - **OAuth Client ID**: OAuth サーバーからトークンを要求するためのクライアント ID。
       - **OAuth Client Secret**: OAuth サーバーからトークンを要求するためのクライアントシークレット。
       - **OAuth Request Scope**: OAuth アクセストークンを要求する際に必要な場合の `scope`。
   - **Attached Service Account**: 追加の項目は不要です。EMQX はインスタンスメタデータエンドポイントから GCP プロジェクト ID とアクセストークンを自動取得します。前提条件は [Attached Service Account の前提条件](#attached-service-account-の前提条件) を参照してください。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして Sink を指定するルールの作成を続行できます。詳細は [GCP Pub/Sub プロデューサー Sink を使ったルール作成](#create-a-rule-with-gcp-pub-sub-producer-sink) を参照してください。

## GCP Pub/Sub プロデューサー Sink を使ったルール作成

このセクションでは、GCP Pub/Sub に保存するデータを指定するルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` と入力します。

4. **SQL Editor** でルールを設定します。例えば、トピック `/devices/+/events` の MQTT メッセージを GCP Pub/Sub に保存したい場合、以下の SQL を使用します。

   注意: 独自の SQL を指定する場合は、Sink のペイロードテンプレートで必要なすべてのフィールドを `SELECT` 部分に含めるようにしてください。

   ```sql
   SELECT
     *
   FROM
     "/devices/+/events"
   ```

   注意: 初心者の方は **SQL Examples** をクリックし、**Enable Test** を有効にして SQL ルールを学習・テストできます。

5. **+ Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Google PubSub Producer` を選択し、EMQX がルールで処理したデータを GCP Pub/Sub に送信するようにします。

6. **Action** ドロップダウンは `Create Action` のままにするか、既存の GCP Pub/Sub プロデューサー Sink を選択できます。この例では新しい Sink を作成してルールに追加します。

7. **Name** フィールドに Sink の名前を入力します。名前は英数字の組み合わせにしてください。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubprodcer` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメーターの詳細は [コネクターの作成](#create-a-connector) を参照してください。

9. **GCP PubSub Topic** に以下のいずれかを入力します。

   - 例：`my-iot-core` のように、[GCP でトピックの作成と管理](#gcp-でトピックの作成と管理) で作成したトピック名。EMQX は設定されたサービスアカウントのプロジェクト内でトピックを解決します。
   - `projects/<project-id>/topics/<topic-name>` の形式の完全修飾トピックパス。異なる GCP プロジェクトのトピックにパブリッシュする場合に使用します。そのプロジェクトのトピックに対してサービスアカウントに必要な Pub/Sub 権限を付与してください。

10. **Payload Template** にテンプレートを定義するか空欄のままにします。

    - 空欄の場合、MQTT メッセージのクライアント ID、トピック、ペイロードなどのすべての可視入力を JSON 形式でエンコードします。
    - テンプレートを使用する場合、`${variable_name}` 形式のプレースホルダーが MQTT コンテキストの対応する値で置換されます。例：`${topic}` は MQTT メッセージのトピックが `my/topic` なら `my/topic` に置換されます。

11. **Attributes Template** と **Ordering Key Template** に、送信メッセージの属性やオーダーキーのフォーマット用テンプレートを定義します（任意）。

    - **Attributes** はキーと値の両方に `${variable_name}` 形式のプレースホルダーを使用可能で、MQTT コンテキストから値を抽出します。キーのテンプレートが空文字列になる場合、そのキーは GCP Pub/Sub 送信メッセージから省略されます。
    - **Ordering Key** も `${variable_name}` 形式のプレースホルダーを使用可能で、解決結果が空文字列の場合は GCP Pub/Sub 送信メッセージに `orderingKey` フィールドが設定されません。

12. **フォールバックアクション（任意）**: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

13. **Advanced Settings** を展開し、必要に応じてオプション設定を行います。詳細は [詳細設定](#advanced-settings) を参照してください。

14. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。

15. **Create** ボタンをクリックして Sink の設定を完了すると、新しい Sink が **Action Outputs** タブに表示されます。

16. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成します。

これでルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブで新しい Google PubSub プロデューサー Sink を確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `/devices/+/events` のメッセージがルール `my_rule` によって解析され、GCP Pub/Sub に送信・保存されていることが直感的に確認できます。

## プロデューサールールのテスト

1. MQTTX を使ってトピック `/devices/+/events` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t /devices/+/events -m '{ "msg": "hello GCP PubSub" }'
   ```

2. Sink の稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ 1 件ずつあるはずです。

3. GCP の **Pub/Sub** -> **Subscriptions** に移動し、**MESSAGES** タブをクリックするとメッセージを確認できます。

## GCP Pub/Sub コンシューマーコネクターの作成

GCP Pub/Sub コンシューマー Source を追加する前に、EMQX と GCP Pub/Sub 間の接続を確立するための GCP Pub/Sub コンシューマーコネクターを作成します。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Google PubSub Consumer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-pubsubconsumer`）。名前は GCP Pub/Sub コンシューマー Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
4. **Authentication** リストから以下の認証方法のいずれかを選択し、対応する項目を設定します。
   - **Service Account JSON**: [GCP でサービスアカウントキーを作成する](#gcp-でサービスアカウントキーを作成する) でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
   - **Workload Identity Federation (WIF)**: 以下の項目を入力します。前提条件は [GCP で Workload Identity Federation を設定する](#gcp-で-workload-identity-federation-を設定する) を参照してください。
     - **GCP Project ID**: コネクターがアクセスするリソースのプロジェクト ID。
     - **GCP Project Number**: コネクターがアクセスするリソースのプロジェクト番号。
     - **Service Account Email**: 代行するサービスアカウントのメールアドレス。
     - **Workload Identity Pool ID**: WIF トークン交換に使用するワークロードアイデンティティプールの ID。
     - **Workload Identity Provider ID**: WIF トークン交換に使用するワークロードアイデンティティプロバイダーの ID。
     - **Initial Token Configuration** で認証情報タイプを選択し、対応する項目を入力します。現在は **OIDC with Client Credentials Grant Type** のみサポートされています。
       - **Endpoint URI**: OIDC プロバイダーの OAuth トークンエンドポイント URI。
       - **OAuth Client ID**: OAuth サーバーからトークンを要求するためのクライアント ID。
       - **OAuth Client Secret**: OAuth サーバーからトークンを要求するためのクライアントシークレット。
       - **OAuth Request Scope**: OAuth アクセストークンを要求する際に必要な場合の `scope`。
   - **Attached Service Account**: 追加の項目は不要です。EMQX はインスタンスメタデータエンドポイントから GCP プロジェクト ID とアクセストークンを自動取得します。前提条件は [Attached Service Account の前提条件](#attached-service-account-の前提条件) を参照してください。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが GCP Pub/Sub サーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして GCP Pub/Sub コンシューマー Source を使ったルールの作成を続行できます。詳細は [GCP Pub/Sub コンシューマー Source を使ったルール作成](#create-a-rule-with-gcp-pub-sub-consumer-source) を参照してください。

## GCP Pub/Sub コンシューマー Source を使ったルール作成

このセクションでは、GCP Pub/Sub からメッセージを消費し、EMQX に転送するルールの作成方法を説明します。Google PubSub Consumer Source を作成・設定し、ルールのデータ入力として追加します。また、Republish アクションをルールに追加し、GCP Pub/Sub から受信したメッセージを EMQX に転送します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule_source` と入力します。

4. 右側の **Data Inputs** タブでデフォルトの Input `Messages` を削除し、**Add Input** をクリックします。

5. **Input Type** ドロップダウンから `Google PubSub Consumer` を選択します。

6. **Source** ドロップダウンはデフォルトの `Create Source` のままにします。この例では新しい Source を作成しルールに追加します。

7. Source の **Name** と（任意で）**Description** を入力します。名前は英数字の組み合わせにしてください（例：`my-gcppubsub-source`）。

8. **Connector** ドロップダウンから先ほど作成した `my_pubsubconsumer` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメーターの詳細は [コネクターの作成](#create-a-connector) を参照してください。

9. GCP Pub/Sub から EMQX へメッセージを消費するため、以下の情報を設定します。

   - **GCP PubSub Topic**: トピック名（例：`my-iot-core`）または `projects/<project-id>/topics/<topic-name>` の形式の完全修飾トピックパスを入力します。トピック名は設定されたサービスアカウントのプロジェクト内で解決されます。異なる GCP プロジェクトのトピックから消費する場合は完全修飾パスを入力し、そのトピックに対してサービスアカウントに必要な Pub/Sub 権限を付与してください。コンシューマーサブスクリプションはサービスアカウントのプロジェクト内に作成され、トピック参照のみ他プロジェクトを指します。
   - **Maximum Messages to Pull**: 1 回のプルリクエストで GCP Pub/Sub から取得する最大メッセージ数を指定します。実際の取得数は指定値より少ない場合があります。

10. **Advanced Settings** を展開し、必要に応じてオプション設定を行います。詳細は [詳細設定](#advanced-settings) を参照してください。

11. **Create** をクリックする前に、**Test Connectivity** をクリックして GCP Pub/Sub サーバーへの接続が成功するかテストできます。

12. **Create** をクリックして Source の作成を完了します。Source はルールの **Data Inputs** タブに追加され、**SQL Editor** のルールは以下のようになります。

    ```sql
    SELECT
      *
    FROM
      "$bridges/gcppubsub:my-gcppubsub-source"
    ```

    注意: 初心者の方は **SQL Examples** をクリックし、**Enable Test** を有効にして SQL ルールを学習・テストできます。

    `my-gcppubsub-source` からのルール SQL は、以下の GCP Pub/Sub から MQTT トピックへのマッピングテーブルに示す GCP Pub/Sub メッセージフィールドにアクセスできます。データ処理のためにルール SQL を調整可能です。この例ではデフォルトの SQL を使用します。

    | フィールド名           | 説明                                                         |
    | --------------------- | ------------------------------------------------------------ |
    | `attributes`          | （任意）文字列のキーと値のペアを含むオブジェクト（存在する場合） |
    | `message_id`          | GCP Pub/Sub がこのメッセージに割り当てたメッセージ ID       |
    | `ordering_key`        | （任意）メッセージの順序付けキー（存在する場合）             |
    | `publishing_time`     | GCP Pub/Sub によって定義されたメッセージのタイムスタンプ     |
    | `topic`               | 発信元の GCP Pub/Sub トピック                                |
    | `value`               | （任意）メッセージのペイロード（存在する場合）               |

    **注意**: 各 GCP Pub/Sub から MQTT トピックへのマッピングは一意の GCP Pub/Sub トピック名を含む必要があります。つまり、同じトピックが複数のマッピングに存在してはいけません。

これで GCP Pub/Sub コンシューマー Source の作成は完了しましたが、メッセージはまだ直接 EMQX にパブリッシュされません。次に [ルールに Republish アクションを追加する](#add-republish-action-to-the-rule) 手順を続けて、Republish アクションを作成しルールに追加してください。

### ルールに Republish アクションを追加する

このセクションでは、GCP Pub/Sub コンシューマー Source から消費したメッセージを転送し、EMQX のトピック `t/1` にパブリッシュするための Republish アクションをルールに追加する方法を説明します。

1. ページ右側の **Action Output** タブを選択し、**Add Action** ボタンをクリックします。**Type of Action** ドロップダウンリストから `Republish` アクションを選択します。

2. メッセージ再パブリッシュの設定を入力します。

   - **Topic**: MQTT にパブリッシュするトピック。ここでは `t/1` を入力します。

   - **QoS**: `0`、`1`、`2`、`${qos}` のいずれかを選択、または他のフィールドから QoS を設定するためのプレースホルダーを入力します。`${qos}` を選択すると元のメッセージの QoS に従います。

   - **Retain**: `true` または `false` を選択します。メッセージをリテインメッセージとしてパブリッシュするかどうかを決定します。他のフィールドからリテインフラグを設定するためのプレースホルダーも使用可能です。この例では `false` を選択します。

   - **Payload**: 転送するメッセージペイロードを生成するテンプレートを設定します。空欄の場合はルールの出力結果をそのまま転送します。`${.value}` と入力すると GCP Pub/Sub メッセージのペイロードのみを転送します。

     MQTT ペイロードテンプレートのデフォルト値は `${.}` で、利用可能なすべてのデータを JSON オブジェクトとして含みます。例えば、すべてのオプションフィールドを含む GCP Pub/Sub メッセージに対して `${.}` をテンプレートに選択すると以下のようになります。

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

     GCP Pub/Sub メッセージのサブフィールドはドット表記でアクセス可能です。例：`${.value}` は GCP Pub/Sub メッセージの値に展開され、`${.attributes.h1}` は `h1` というメッセージ属性キーの値に展開されます。存在しない値は空文字列に置換されます。

   - **MQTT 5.0 メッセージプロパティ**: デフォルトで無効です。詳細設定は [Republish アクションの追加](./rule-get-started.md#add-republish-action) を参照してください。

3. **Create** をクリックしてアクションの作成を完了します。作成成功後、ルール作成ページに戻り、Republish アクションが **Action Outputs** タブに追加されます。

4. ルール作成ページで **Create** ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成されました。**Rules** ページで新規作成したルールを確認できます。**Sources** タブで新しい GCP Pub/Sub コンシューマー Source を確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、GCP Pub/Sub コンシューマー Source からのメッセージが Republish を経由して `t/1` にパブリッシュされる様子を直感的に確認できます。

## GCP Pub/Sub コンシューマールールのテスト

GCP Pub/Sub コンシューマー Source が GCP Pub/Sub からメッセージを消費し、EMQX の MQTT トピック `t/1` に再パブリッシュすることを検証する手順です。

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

## 詳細設定

このセクションでは、GCP Pub/Sub コネクター、プロデューサー Sink、コンシューマー Source の詳細設定について説明します。

### コネクターの詳細設定

GCP Pub/Sub プロデューサーおよびコンシューマーコネクターは同じ詳細設定を使用します。

| 項目名 | 説明 | デフォルト値 |
| --- | --- | --- |
| **HTTP Pipelining** | 各レスポンスを待たずに送信できる HTTP リクエストの最大数。`1` に設定すると次のリクエスト送信前にレスポンスを待ちます。 | `100` |
| **Connection Pool Size** | コネクションプールに保持する接続数。 | `8` |
| **Connect Timeout** | HTTP 接続確立の最大待機時間。 | `15` 秒 |
| **Max Inactive** | アクティビティがない最大時間。HTTP クライアントが再接続を試みます。 | `10` 秒 |
| **Max Retries** | リクエスト送信時にエラー発生後の最大リトライ回数。 | `2` |
| **Start Timeout** | コネクター作成後、正常状態になるまでの最大待機時間。 | `5` 秒 |
| **Health Check Interval** | コネクターのヘルスチェック間隔。 | `15` 秒 |
| **Health Check Timeout** | ヘルスチェック結果の最大待機時間。タイムアウト時は切断とみなします。 | `60` 秒 |

### プロデューサー Sink とコンシューマー Source 共通の詳細設定

プロデューサー Sink とコンシューマー Source は以下の詳細設定を共有します。**Health Check Interval** のデフォルト値は異なります。

| 項目名 | 説明 | プロデューサー Sink デフォルト | コンシューマー Source デフォルト |
| --- | --- | --- | --- |
| **Request TTL** | リクエストがバッファに入ってからレスポンスまたはアックを受け取るまでの最大時間。この間に応答がなければリクエストは期限切れとなります。 | `45` 秒 | `45` 秒 |
| **Health Check Interval** | Sink または Source のヘルスチェック間隔。 | `15` 秒 | `30` 秒 |
| **Health Check Interval Jitter** | ヘルスチェック間隔に加える一様ランダム遅延。複数のアクションやソースが同時にヘルスチェックを開始しないようにします。 | `0` ミリ秒 | `0` ミリ秒 |
| **Health Check Timeout** | ヘルスチェック結果の最大待機時間。タイムアウト時は切断とみなします。 | `60` 秒 | `60` 秒 |

### プロデューサー Sink 固有の詳細設定

GCP Pub/Sub プロデューサー Sink は以下の追加の詳細設定を提供します。

| 項目名 | 説明 | デフォルト値 |
| --- | --- | --- |
| **Buffer Pool Size** | GCP Pub/Sub 送信前にデータを保持・処理するバッファワーカーの数。 | `16` |
| **Dispatch Strategy** | ピックキーなしリクエストをバッファワーカーに割り当てる戦略。`Per Client ID` は同一クライアントのリクエストを同一ワーカーに割り当て、`Random` はワーカー間で分散します。 | `Per Client ID` |
| **Max Buffer Queue Size** | 各バッファワーカーが保持可能な最大データ量。 | `256` MB |
| **Batch Size** | 1 バッチあたりの最大リクエスト数。`1` に設定するとバッチ処理を無効化します。 | `1` |
| **Query Mode** | リクエストを同期または非同期で送信するかを制御。`Async` モードでは EMQX は GCP Pub/Sub の応答を待たずに処理を継続します。 | `Async` |
| **Inflight Window** | **Query Mode** が `Async` の場合、応答を受け取らずに送信可能な最大リクエスト数。同一 MQTT クライアントのメッセージを厳密な順序で処理する場合は `1` に設定します。 | `100` |

### コンシューマー Source 固有の詳細設定

GCP Pub/Sub コンシューマー Source は以下の追加の詳細設定を提供します。

| 項目名 | 説明 | デフォルト値 |
| --- | --- | --- |
| **Ack Deadline** | Source が配信済みメッセージをアックするまでの GCP Pub/Sub の待機時間の目安。期限切れ後はメッセージが再配信される可能性があります。サポートされる範囲は `10` ～ `600` 秒です。 | `60` 秒 |
