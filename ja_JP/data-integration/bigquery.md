# BigQueryへのMQTTデータ取り込み

[BigQuery](https://cloud.google.com/bigquery?hl=en) は、大量のリレーショナル構造化データ向けのエンタープライズデータウェアハウスです。大規模かつアドホックなSQLベースの分析やレポーティングに最適化されており、組織の洞察を得るのに最適です。EMQXは、MQTTデータのリアルタイム抽出、処理、分析のためにBigQueryとのシームレスな統合をサポートしています。

本ページでは、EMQXとBigQuery間のデータ統合について包括的に紹介し、データ統合の作成と検証に関する実践的な手順を提供します。

## 動作概要

BigQueryデータ統合は、EMQXの標準機能として提供されており、ユーザーがMQTTデータストリームをGoogle Cloudとシームレスに統合し、IoTアプリケーション開発のための豊富なサービスと機能を活用できるように設計されています。

![bigquery_architecture](./assets/bigquery_architecture.png)

EMQXはルールエンジンとSinkを介してMQTTデータをBigQueryに転送します。全体の流れは以下の通りです。

1. **IoTデバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンを使用して、特定のソースからのMQTTメッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **BigQueryへのブリッジング**：ルールがトリガーされると、メッセージをBigQueryに転送するアクションが実行されます。データプロパティ、オーダーキー、MQTTトピックからBigQueryテーブルへのマッピングを簡単に設定でき、より豊富なコンテキスト情報と順序保証を提供し、柔軟なIoTデータ処理を可能にします。

## 特長とメリット

EMQXとBigQueryの統合により、MQTTデータの堅牢でスケーラブルかつリアルタイムなデータパイプラインが実現します。以下の特長とメリットにより、IoT分析やデータ駆動型の意思決定が簡素化されます。

- **リアルタイムデータ取り込み**：EMQXからBigQueryへ低レイテンシでMQTTメッセージをシームレスにストリームします。即時処理と分析が必要なタイムセンシティブなアプリケーションに対応します。
- **柔軟なデータマッピング**：MQTTトピックやメッセージペイロードをBigQueryのテーブルやフィールドにカスタマイズしてマッピング可能です。
- **スケーラブルでサーバーレスな分析**：BigQueryの完全マネージドかつサーバーレスなアーキテクチャを活用し、大規模なIoTデータの分析が可能です。
- **Google Cloudエコシステムとの容易な統合**：Data Studio、Looker、AI PlatformなどGoogle Cloudサービスとネイティブに連携し、可視化や機械学習を簡単に実現。データ収集から洞察生成までのエンドツーエンドパイプライン構築を簡素化します。

## はじめる前に

このセクションでは、BigQueryデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GCPでサービスアカウントキーを作成する

EMQXがBigQueryに接続できるように、Google Cloudでサービスアカウントを作成し、JSON形式のキーを生成する必要があります。

1. GCPアカウントで[サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)を作成します。サービスアカウントには、使用するデータセットおよびテーブルへのアクセス権限が必要です。例えば、「BigQuery Data Editor」ロールを付与して対象データセットやテーブルの読み書きを許可するか、少なくともデータの読み書き権限を持つようにしてください。

2. 作成したサービスアカウントのメールアドレスをクリックします。

3. **Key** タブをクリックし、**Add key** のドロップダウンリストから **Create new key** を選択して、サービスアカウントキーを作成し、JSON形式でダウンロードします。

   ::: tip

   ダウンロードしたサービスアカウントキーは後でEMQXの認証に使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCPでWorkload Identity Federationを設定する

Workload Identity Federation（WIF）を使うと、長期的なサービスアカウントキーを使わずにEMQXがGCPリソースにアクセスできます。EMQXは外部IDプロバイダー（例：Microsoft Azure）からのトークンをGCPのSecurity Token Service経由で一時的なGCPトークンに交換し、そのトークンでGCPサービスアカウントを代行します。トークンの更新は自動で行われます。

WIFを利用するには、コネクター作成前にGCPプロジェクトで以下を完了してください。

1. Google Cloudコンソールで **IAM & Admin** -> **Workload Identity Federation** に移動し、ワークロードアイデンティティプールを作成し、**Pool ID** と **Project Number** を控えます。

2. プールにプロバイダーを追加し、**Provider ID** を控えます。OIDC認証の場合は、外部IDプロバイダーからOAuth 2.0クライアント認証情報（クライアントID、クライアントシークレット、トークンエンドポイントURI）を取得します。

3. ワークロードアイデンティティプールに、BigQueryデータセットとテーブルにアクセス可能なGCPサービスアカウントを代行する権限を付与します。コネクター設定時にサービスアカウントのメールアドレスが必要です。

   ::: tip

   詳細な手順は[Workload Identity Federationの設定](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers)を参照してください。

   :::

**例：Microsoft Azure (Entra ID)**

[Microsoft Entra ID](https://portal.azure.com/)でAPIを公開するアプリケーションを登録し、クライアントシークレットを作成します。コネクター設定時に以下の値を使用します。

| コネクター項目 | 値 |
|---|---|
| **Endpoint URI** | `https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token` |
| **OAuth Client ID** | アプリケーション（クライアント）ID、形式は `api://<application-id>` |
| **OAuth Client Secret** | アプリケーション用に生成したクライアントシークレット |
| **OAuth Request Scope** | `api://<application-id>/.default` |

::: tip 注意

`scope` はアプリケーションのaudience（aud）と完全に一致させる必要があります。そうしないとGCP STSとのトークン交換が失敗します。詳細はMicrosoftの[OAuth 2.0クライアント認証フロー](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-client-creds-grant-flow)を参照してください。

サービスアカウントにWIFプールへのアクセス権を付与する際は、Subject値に**Object ID**（アプリケーションIDではない）を使用してください。Object IDはAzureポータルのアプリケーションの概要ページの**Enterprise applications**で確認できます。

:::

### GCPでデータセットとテーブルを作成・管理する

EMQXでBigQueryデータ統合を設定する前に、GCPで必要なデータセットとテーブルを作成してください。

1. Google Cloudコンソールで **BigQuery** -> **Studio** ページに移動します。詳細は[データのロードとクエリ](https://cloud.google.com/bigquery/docs/quickstarts/load-data-console)のクイックスタートガイドを参照してください。

   ::: tip

   使用するサービスアカウントには、対象テーブルの書き込み権限が必要です。

   :::

2. **Explorer** ペインでケバブメニュー（⋮）をクリックし、**Create dataset** を選択します。データセット名を定義し、**Create dataset** をクリックします。

3. データセット作成後、**Explorer** ペインでデータセットをクリックし、**(+) Create table** をクリックします。

   - ソースは「Empty Table」を選択します。

   - テーブル名を入力します。

   - テーブルスキーマを定義します。例えば、**Edit as text** トグルをクリックし、以下のスキーマ定義をテキストフィールドに貼り付けます。

     ```
     clientid:string,payload:bytes,topic:string,publish_received_at:timestamp
     ```

   - **Create table** をクリックして設定を完了します。

4. EMQXが書き込みできるように権限を設定します。

   - データセットを選択し、**Share** をクリックします。

   - サービスアカウントのメールアドレスをプリンシパルとして追加します。

   - 以下のような適切なロールを割り当てます。

     - データセットに対して「BigQuery Data Viewer」（読み取りアクセス）

     - テーブルに対して「Editor」（読み書きアクセス）

5. テーブル作成後、クエリを実行して確認できます。

   - テーブルをクリックし、**Query** をクリックします。

   - 以下のような簡単なSQL文を実行してテーブルにアクセスできることを確認します。

     ```sql
     SELECT * FROM `my_project.my_dataset.my_tab` LIMIT 1000
     ```

## BigQueryコネクターを作成する

BigQuery Producer Sinkアクションを追加する前に、EMQXとBigQuery間の接続を確立するためのBigQueryコネクターを作成する必要があります。

1. EMQXダッシュボードで **Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックし、コネクター選択ページで **BigQuery** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例：`my_bigquery`。この名前はBigQuery Sinkとコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. **Authentication** ドロップダウンで以下の認証方法のいずれかを選択し、対応する項目を入力します。

   - **Service Account JSON**：前述の[サービスアカウントキー作成](#gcpでサービスアカウントキーを作成する)でエクスポートしたJSON形式のサービスアカウント認証情報をアップロードします。

   - **Workload Identity Federation (WIF)**：以下の項目を入力します。この方法はサービスアカウントJSONファイルを使用しません。前提条件は[Workload Identity Federationの設定](#gcpでworkload-identity-federationを設定する)を参照してください。

     - **GCP Project ID**：コネクターがアクセスするリソースのプロジェクトID

     - **GCP Project Number**：コネクターがアクセスするリソースのプロジェクト番号

     - **Service Account Email**：代行するサービスアカウントのメールアドレス

     - **Workload Identity Pool ID**：WIFトークン交換に使用するワークロードアイデンティティプールのID

     - **Workload Identity Provider ID**：WIFトークン交換に使用するワークロードアイデンティティプロバイダーのID

     - **Initial Token Configuration**：認証情報の種類を選択し、対応する項目を入力します。現在サポートされているのは**OIDC with Client Credentials Grant Type**のみです。

       - **Endpoint URI**：OIDCプロバイダーのOAuthトークンエンドポイントURI

       - **OAuth Client ID**：OAuthサーバーからトークンを取得するためのクライアントID

       - **OAuth Client Secret**：OAuthサーバーからトークンを取得するためのクライアントシークレット

       - **OAuth Request Scope**：OAuthアクセストークン要求時に必要な場合のスコープ

5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターがBigQueryサーバーに接続できるかテストできます。

6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックしてSink付きルールの作成に進めます。詳細は[BigQuery Sink付きルールの作成](#create-a-rule-with-bigquery-sink)を参照してください。

## BigQuery Sink付きルールを作成する

このセクションでは、BigQueryに保存するデータを指定するルールの作成方法を示します。

1. EMQXダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` と入力します。

4. **SQL Editor** にルールを設定します。例えば、トピック `t/bq` のMQTTメッセージをBigQueryに保存したい場合、以下のSQLを使用します。

   注意：独自のSQLを指定する場合、Sinkのペイロードテンプレートで必要なすべてのフィールドを`SELECT`文に含めてください。

   ```sql
   SELECT
     clientid,
     topic,
     payload,
     publish_received_at
   FROM
     "t/bq"
   ```

   ::: tip 注意

   BigQueryテーブルのカラムに対応するフィールドのみを選択してください。未定義のフィールドはBigQueryで認識されません。

   :::

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストが可能です。

   :::

5. **Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンから `BigQuery` を選択し、EMQXがルールで処理したデータをBigQueryに送信するようにします。

6. **Action** ドロップダウンは `Create Action` のままにするか、既存のBigQuery Sinkを選択できます。ここでは新しいSinkを作成してルールに追加します。

7. **Name** フィールドにSinkの名前を入力します。英数字の組み合わせにしてください。

8. **Connector** ドロップダウンから先ほど作成した `my_bigquery` を選択します。隣のボタンから新しいコネクターを作成することも可能です。設定パラメーターは[コネクター作成](#bigqueryコネクターを作成する)を参照してください。

9. **Dataset** と **Table** に、[GCPでデータセットとテーブルを作成・管理する](#gcpでデータセットとテーブルを作成・管理する)で作成したデータセット名とテーブル名をそれぞれ入力します。

12. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

13. **詳細設定（任意）**：必要に応じて詳細設定オプションを構成します。詳細は[詳細設定](#advanced-settings)を参照してください。

14. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターがBigQueryサーバーに接続できるかテストできます。

15. **Create** ボタンをクリックしてSink設定を完了すると、**Action Outputs** タブに新しいSinkが表示されます。

16. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成します。

これでルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると新しいBigQuery Sinkが表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/bq` のメッセージがルール `my_rule` によって解析され、BigQueryに送信・保存されていることが確認できます。

## ルールのテスト

1. MQTTXを使ってトピック `t/bq` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/bq -m '{ "msg": "hello BigQuery" }'
   ```

2. Sinkの稼働状況を確認すると、1件の新規受信メッセージと1件の新規送信メッセージがあるはずです。

3. GCPの **BigQuery** -> **Studio** に移動し、テーブルをクリックして **Query** をクリックします。クエリを実行するとメッセージが確認できます。

## 詳細設定

このセクションでは、BigQuery Producer Sinkの詳細設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、用途に応じて以下のパラメーターを調整できます。

| フィールド名                     | 説明                                                                                     | デフォルト値       |
|---------------------------------|------------------------------------------------------------------------------------------|--------------------|
| **Buffer Pool Size**             | EMQXとBigQuery間のデータフローを管理するバッファワーカープロセスの数を指定します。これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへの送信を最適化し、スムーズなデータ転送を保証します。 | `16`               |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。バッファに入った時点からカウントが始まり、このTTLを超えてバッファに残るか、送信後にBigQueryからの応答やアックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45`秒             |
| **Health Check Interval**        | SinkがBigQueryとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`秒             |
| **Health Check Interval Jitter** | 複数のノードが同時にヘルスチェックを開始する確率を減らすために、基本のヘルスチェック間隔に加える一様ランダム遅延時間（ミリ秒）です。複数のアクションやソースが同じコネクターを共有する場合、ジッターを有効にするとヘルスチェックがずれて実行されます。 | `0`ミリ秒          |
| **Health Check Timeout**         | コネクターがBigQueryとの接続ヘルスチェックを行う際のタイムアウト時間（秒）を指定します。 | `60`秒             |
| **Max Buffer Queue Size**        | BigQuery Sinkの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時的に保持し、効率的にデータストリームを処理します。システム性能やデータ転送要件に応じて調整してください。 | `256`              |
| **Query Mode**                   | メッセージ送信を最適化するために、`synchronous`（同期）または`asynchronous`（非同期）のリクエストモードを選択できます。非同期モードでは、BigQueryへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがBigQuery到達前にメッセージを受信する可能性があります。 | `Async`（非同期）   |
| **Batch Size**                   | EMQXからBigQueryへ単一転送操作で送信するデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率と性能を最適化できます。`Batch Size`を`1`に設定すると、データレコードはバッチ化せずに個別に送信されます。 | `1000`             |
| **Inflight Window**              | 「インフライトキューリクエスト」とは、開始されたがまだ応答やアックが返ってきていないリクエストを指します。この設定は、SinkがBigQueryと通信する際に同時に存在できるインフライトリクエストの最大数を制御します。**Request Mode**が`asynchronous`の場合に特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理する必要がある場合は、この値を`1`に設定してください。 | `100`              |
