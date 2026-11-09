# BigQuery に MQTT データを取り込む

[BigQuery](https://cloud.google.com/bigquery?hl=en) は、大量のリレーショナル構造化データ向けのエンタープライズデータウェアハウスです。大規模かつアドホックな SQL ベースの分析およびレポーティングに最適化されており、組織の洞察を得るのに最適です。EMQX は、MQTT データのリアルタイム抽出、処理、分析のために BigQuery とのシームレスな統合をサポートしています。

本ページでは、EMQX と BigQuery 間のデータ統合について包括的に紹介し、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作の仕組み

BigQuery データ統合は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームを Google Cloud とシームレスに統合し、IoT アプリケーション開発のための豊富なサービスや機能を活用できるよう設計されています。

![bigquery_architecture](./assets/bigquery_architecture.png)

EMQX はルールエンジンと Sink を介して MQTT データを BigQuery に転送します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュする**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理する**：組み込みのルールエンジンを使用して、特定のソースからの MQTT メッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **BigQuery へのブリッジング**：ルールがメッセージを BigQuery に転送するアクションをトリガーし、データプロパティ、オーダーキー、MQTT トピックと BigQuery テーブルのマッピングを簡単に設定できます。これにより、データ統合におけるより豊かなコンテキスト情報と順序保証が提供され、柔軟な IoT データ処理が可能になります。

## 特長とメリット

EMQX と BigQuery の統合は、MQTT データのための堅牢でスケーラブルかつリアルタイムなデータパイプラインを提供します。以下の特長とメリットにより、IoT 分析やデータ駆動型の意思決定を簡素化します。

- **リアルタイムデータ取り込み**：EMQX から BigQuery へ低レイテンシで MQTT メッセージをシームレスにストリーム送信します。即時処理と分析が必要な時間依存型アプリケーションに対応します。
- **柔軟なデータマッピング**：MQTT トピックやメッセージペイロードを BigQuery のテーブルやフィールドにカスタマイズしてマッピング可能です。
- **スケーラブルかつサーバレスな分析**：BigQuery のフルマネージドかつサーバレスなアーキテクチャを活用し、大規模な IoT データ分析を実現します。
- **Google Cloud エコシステムとの簡単な統合**：Data Studio、Looker、AI Platform などの Google Cloud サービスとネイティブに連携し、可視化や機械学習を簡単に行えます。データ収集から洞察生成までのエンドツーエンドパイプライン構築を容易にします。

## はじめる前に

このセクションでは、BigQuery データ統合の作成を開始する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GCP でサービスアカウントキーを作成する

EMQX が BigQuery に接続できるように、Google Cloud でサービスアカウントを作成し、JSON 形式のキーを生成する必要があります。

1. GCP アカウントで[サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)を作成します。サービスアカウントには、使用するデータセットやテーブルへのアクセス権限が必要です。たとえば、「BigQuery Data Editor」ロールを付与し、対象データセットやテーブルの読み書き権限を持たせるか、少なくともデータの読み書き権限を確保してください。

2. 作成したサービスアカウントのメールアドレスをクリックします。

3. **Key** タブをクリックし、**Add key** のドロップダウンリストから **Create new key** を選択して、サービスアカウントキーを作成し、JSON 形式でダウンロードします。

   ::: tip

   ダウンロードしたサービスアカウントキーは、後で EMQX の認証に使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCP で Workload Identity Federation を設定する

Workload Identity Federation（WIF）は、長期間有効なサービスアカウントキーを使わずに EMQX が GCP リソースにアクセスできるようにする仕組みです。EMQX は外部 ID プロバイダー（例：Microsoft Azure）からトークンを取得し、それを GCP のセキュリティトークンサービス（STS）に渡して一時的な GCP トークンと交換し、そのトークンで GCP サービスアカウントを代行します。トークンの更新は自動で行われます。

WIF を使用するには、コネクター作成前に GCP プロジェクトで以下を完了してください。

1. Google Cloud コンソールで **IAM & Admin** -> **Workload Identity Federation** に移動し、ワークロードアイデンティティプールを作成し、**Pool ID** と **Project Number** を控えます。

2. プールにプロバイダーを追加し、**Provider ID** を控えます。OIDC 認証の場合は、外部 ID プロバイダーから OAuth 2.0 クライアント認証情報（クライアント ID、クライアントシークレット、トークンエンドポイント URI）を取得します。

3. BigQuery のデータセットやテーブルにアクセスできる GCP サービスアカウントを代行する権限をワークロードアイデンティティプールに付与します。コネクター設定時にサービスアカウントのメールアドレスが必要です。

   ::: tip

   詳細な手順は[Workload Identity Federation の設定](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers)を参照してください。

   :::

**例：Microsoft Azure（Entra ID）**

[Microsoft Entra ID](https://portal.azure.com/)で API を公開するアプリケーションを登録し、クライアントシークレットを作成します。コネクター設定時には以下の値を使用します。

| コネクター項目 | 値 |
|---|---|
| **Endpoint URI** | `https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token` |
| **OAuth Client ID** | `api://<application-id>` 形式のアプリケーション（クライアント）ID |
| **OAuth Client Secret** | アプリケーション用に生成したクライアントシークレット |
| **OAuth Request Scope** | `api://<application-id>/.default` |

::: tip 補足

`scope` はアプリケーションのオーディエンス（aud）と完全に一致させる必要があります。そうしないと GCP STS とのトークン交換が失敗します。詳細は Microsoft の[OAuth 2.0 クライアント認証フロー](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-client-creds-grant-flow)を参照してください。

サービスアカウントに WIF プールへのアクセス権を付与する際は、**アプリケーション ID ではなく Object ID** を Subject として使用してください。Object ID は Azure ポータルのアプリケーションの概要ページの **Enterprise applications** セクションで確認できます。

:::

### GCP でデータセットとテーブルを作成・管理する

EMQX で BigQuery データ統合を設定する前に、GCP で必要なデータセットとテーブルを作成してください。

1. Google Cloud コンソールで **BigQuery** -> **Studio** ページに移動します。詳細は[データのロードとクエリ](https://cloud.google.com/bigquery/docs/quickstarts/load-data-console)のクイックスタートガイドを参照してください。

   ::: tip

   使用するサービスアカウントには、対象テーブルへの書き込み権限が必要です。

   :::

2. **Explorer** ペインでケバブメニュー（⋮）をクリックし、**Create dataset** を選択します。データセット名を指定して **Create dataset** をクリックします。

3. データセット作成後、**Explorer** ペインでデータセットをクリックし、**(+) Create table** をクリックします。

   - ソースは「Empty Table」を選択します。

   - テーブル名を入力します。

   - テーブルスキーマを定義します。例として、**Edit as text** トグルをクリックし、以下のスキーマ定義をテキストフィールドに貼り付けます。

     ```
     clientid:string,payload:bytes,topic:string,publish_received_at:timestamp
     ```

   - **Create table** をクリックして設定を完了します。

4. EMQX が書き込みできるように権限を設定します。

   - データセットを選択し、**Share** をクリックします。

   - サービスアカウントのメールアドレスをプリンシパルとして追加します。

   - 以下のような適切なロールを割り当てます。

     - データセットに対して「BigQuery Data Viewer」（読み取りアクセス）

     - テーブルに対して「Editor」（読み書きアクセス）

5. テーブル作成後、クエリを実行して確認できます。

   - テーブルをクリックし、**Query** をクリックします。

   - 以下のような簡単な SQL を実行してテーブルにアクセスできることを確認します。

   ```sql
   SELECT * FROM `my_project.my_dataset.my_tab` LIMIT 1000
   ```

## BigQuery コネクターを作成する

BigQuery Producer Sink アクションを追加する前に、EMQX と BigQuery 間の接続を確立するための BigQuery コネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックし、コネクター選択ページで **BigQuery** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例：`my_bigquery`。名前は BigQuery Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. **Authentication** ドロップダウンから以下の認証方法のいずれかを選択し、対応する項目を入力します。

   - **Service Account JSON**：前述の[サービスアカウントキー作成](#gcp-でサービスアカウントキーを作成する)でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。

   - **Workload Identity Federation (WIF)**：以下の項目を入力します。この方法はサービスアカウント JSON ファイルを使用しません。前提条件は[Workload Identity Federation の設定](#gcp-で-workload-identity-federation-を設定する)を参照してください。

     - **GCP Project ID**：コネクターがアクセスするリソースのプロジェクト ID。

     - **GCP Project Number**：コネクターがアクセスするリソースのプロジェクト番号。

     - **Service Account Email**：代行するサービスアカウントのメールアドレス。

     - **Workload Identity Pool ID**：WIF トークン交換に使用するワークロードアイデンティティプールの ID。

     - **Workload Identity Provider ID**：WIF トークン交換に使用するワークロードアイデンティティプロバイダーの ID。

     - **Initial Token Configuration** で認証情報の種類を選択し、対応する項目を入力します。現在サポートされているのは **OIDC with Client Credentials Grant Type** のみです。

       - **Endpoint URI**：OIDC プロバイダーの OAuth トークンエンドポイント URI。

       - **OAuth Client ID**：OAuth サーバーからトークンを取得するためのクライアント ID。

       - **OAuth Client Secret**：OAuth サーバーからトークンを取得するためのクライアントシークレット。

       - **OAuth Request Scope**：OAuth アクセストークン取得時に必要な場合の `scope`。

5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが BigQuery サーバーに接続できるかテストできます。

6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして BigQuery Sink を指定するルール作成に進めます。詳細は[BigQuery Sink を使ったルールの作成](#create-a-rule-with-bigquery-sink)を参照してください。

## BigQuery Sink を使ったルールの作成

このセクションでは、BigQuery に保存するデータを指定するルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` を入力します。

4. **SQL Editor** でルールを設定します。たとえば、トピック `t/bq` の MQTT メッセージを BigQuery に保存したい場合、以下の SQL 文を使用します。

   注意：独自の SQL 文を指定する場合は、Sink のペイロードテンプレートで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     clientid,
     topic,
     base64_encode(payload) AS payload,
     timestamp/1000 AS publish_received_at
   FROM
     "t/bq"
   ```

   ::: tip 注意

   BigQuery テーブルのカラムに存在するフィールドのみを選択してください。存在しないフィールドがあると BigQuery が認識できません。

   :::

   ::: tip

   初心者の方は **SQL Examples** をクリックし、**Enable Test** を有効にして SQL ルールを学習・テストできます。

   :::

5. **Add Action** ボタンをクリックして、ルールによりトリガーされるアクションを定義します。**Type of Action** ドロップダウンから `BigQuery` を選択し、ルールで処理されたデータを BigQuery に送信するようにします。

6. **Action** ドロップダウンは `Create Action` のままにするか、既存の BigQuery Sink を選択できます。この例では新しい Sink を作成してルールに追加します。

7. **Name** フィールドに Sink の名前を入力します。名前は英数字の組み合わせで指定してください。

8. **Connector** ドロップダウンから先ほど作成した `my_bigquery` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメータの詳細は[コネクターの作成](#create-a-connector)を参照してください。

9. **Dataset** と **Table** に、それぞれ[データセットとテーブルの作成・管理](#create-and-manage-datasets-and-tables-in-gcp)で作成した名前を入力します。

12. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリ Sink がメッセージ処理に失敗した場合にこれらがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

13. **詳細設定（任意）**：必要に応じて詳細設定オプションを構成します。詳細は[詳細設定](#advanced-settings)を参照してください。

14. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが BigQuery サーバーに接続できるかテストできます。

15. **Create** ボタンをクリックして Sink の設定を完了すると、新しい Sink が **Action Outputs** タブに表示されます。

16. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成します。

これでルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると新しい BigQuery Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/bq` のメッセージがルール `my_rule` によって解析され BigQuery に送信・保存されている様子を確認できます。

## ルールのテスト

1. MQTTX を使用してトピック `t/bq` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/bq -m '{ "msg": "hello BigQuery" }'
   ```

2. Sink の稼働状況を確認し、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあることを確認します。

3. GCP の **BigQuery** -> **Studio** に移動し、テーブルをクリックして **Query** をクリックします。クエリを実行するとメッセージが確認できます。

## 詳細設定

このセクションでは、BigQuery Producer Sink の詳細設定オプションについて説明します。ダッシュボードで Sink を設定する際に、**Advanced Settings** を展開して以下のパラメータをニーズに応じて調整できます。

| フィールド名                       | 説明                                                  | デフォルト値   |
| -------------------------------- | ----------------------------------------------------- | -------------- |
| **Buffer Pool Size**             | EMQX と BigQuery 間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへの送信を最適化し、スムーズなデータ転送を保証します。 | `16`           |
| **Request TTL**                  | リクエストがバッファに入ってから有効とみなされる最大時間（秒）を指定します。このタイマーはリクエストがバッファに入った時点で開始されます。TTL を超えてバッファに滞留するか、送信後に BigQuery から適時の応答やアック（ACK）が得られない場合、リクエストは期限切れとみなされます。 | `45` 秒        |
| **Health Check Interval**        | Sink が BigQuery との接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒        |
| **Health Check Interval Jitter** | 複数ノードが同時にヘルスチェックを開始する可能性を減らすため、基本のヘルスチェック間隔に加える一様ランダム遅延です。複数のアクションやソースが同じコネクターを共有する場合、ジッターを有効にするとヘルスチェックがわずかに異なるタイミングで開始されます。 | `0` ミリ秒     |
| **Health Check Timeout**         | コネクターが BigQuery との接続状態をヘルスチェックする際のタイムアウト時間を指定します。 | `60` 秒        |
| **Max Buffer Queue Size**        | BigQuery Sink の各バッファワーカーがバッファリングできる最大バイト数を指定します。バッファワーカーはデータを一時的に保持し、BigQuery への送信を効率化します。システム性能やデータ転送要件に応じて調整してください。 | `256`          |
| **Query Mode**                   | メッセージ送信を最適化するために、`synchronous`（同期）または `asynchronous`（非同期）のリクエストモードを選択できます。非同期モードでは、BigQuery への書き込みが MQTT メッセージのパブリッシュ処理をブロックしません。ただし、クライアントがメッセージを BigQuery 到着前に受信する可能性があります。 | `Async`        |
| **Batch Size**                   | EMQX から BigQuery へ一度に送信するデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率と性能を最適化できます。`Batch Size` を `1` に設定すると、データレコードはバッチ化せず個別に送信されます。 | `1000`         |
| **Inflight Window**              | 「インフライトキューリクエスト」とは、送信済みで応答やアック（ACK）をまだ受け取っていないリクエストを指します。この設定は Sink と BigQuery 間の通信で同時に存在できるインフライトリクエストの最大数を制御します。`Request Mode` が `asynchronous` の場合に特に重要です。同一 MQTT クライアントからのメッセージを厳密に順序処理したい場合は、この値を `1` に設定してください。 | `100`          |
