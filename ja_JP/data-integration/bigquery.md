# BigQueryへのMQTTデータ取り込み

[BigQuery](https://cloud.google.com/bigquery?hl=en)は、大量のリレーショナル構造化データを扱うエンタープライズ向けデータウェアハウスです。大規模かつアドホックなSQLベースの分析やレポートに最適化されており、組織の洞察を得るのに適しています。EMQXは、MQTTデータのリアルタイム抽出、処理、分析のためにBigQueryとのシームレスな統合をサポートしています。

本ページでは、EMQXとBigQuery間のデータ統合について包括的に紹介し、データ統合の作成と検証に関する実践的な手順を提供します。

## 動作概要

BigQueryデータ統合は、EMQXの標準機能であり、ユーザーがMQTTデータストリームをGoogle Cloudとシームレスに統合し、IoTアプリケーション開発のために豊富なサービスと機能を活用できるよう設計されています。

![bigquery_architecture](./assets/bigquery_architecture.png)

EMQXはルールエンジンとSinkを介してMQTTデータをBigQueryに転送します。全体の流れは以下の通りです：

1. **IoTデバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンを使い、特定のトピックにマッチするMQTTメッセージを処理します。ルールエンジンは対応するルールをマッチングし、データフォーマットの変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **BigQueryへのブリッジング**：ルールによりメッセージをBigQueryに転送するアクションがトリガーされ、データプロパティ、オーダーキー、MQTTトピックとBigQueryトピックのマッピングを簡単に設定できます。これにより、データ統合のための豊富なコンテキスト情報と順序保証が提供され、柔軟なIoTデータ処理が可能になります。

## 特長とメリット

EMQXとBigQueryの統合は、MQTTデータのための堅牢でスケーラブルかつリアルタイムなデータパイプラインを提供します。以下の特長とメリットにより、IoT分析やデータ駆動型の意思決定を簡素化します：

- **リアルタイムデータ取り込み**：EMQXからBigQueryへMQTTメッセージを低レイテンシでシームレスにストリーム送信。即時処理と分析が必要な時間敏感なアプリケーションに対応。
- **柔軟なデータマッピング**：MQTTトピックとメッセージペイロードをBigQueryのテーブルやフィールドにカスタマイズしてマッピング可能。
- **スケーラブルかつサーバレスな分析**：BigQueryのフルマネージドかつサーバレスなアーキテクチャを活用し、IoTデータを大規模に分析。
- **Google Cloudエコシステムとの容易な統合**：Data Studio、Looker、AI PlatformなどのGoogle Cloudサービスとネイティブに連携し、可視化や機械学習を実現。データ収集から洞察生成までのエンドツーエンドパイプライン構築を簡素化。

## はじめる前に

このセクションでは、BigQueryデータ統合の作成を開始する前に完了すべき準備について説明します。

### 前提条件

- EMQXのデータ統合[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GCPでのサービスアカウントキー作成

EMQXがBigQueryに接続できるように、Google Cloudでサービスアカウントを作成し、JSON形式のキーを生成する必要があります。

1. GCPアカウントで[サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)を作成します。サービスアカウントには、使用するデータセットやテーブルへのアクセス権限が必要です。例えば、「BigQuery Data Editor」ロールを付与して対象データセットやテーブルの読み書きを許可するか、少なくともデータへの読み書き権限を持たせてください。

2. 作成したサービスアカウントのメールアドレスをクリックします。

3. **Key**タブをクリックし、**Add key**のドロップダウンから**Create new key**を選択してサービスアカウントキーを作成し、JSON形式でダウンロードします。

   ::: tip

   ダウンロードしたサービスアカウントキーは後でEMQXのBigQuery認証に使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCPでのWorkload Identity Federation設定

Workload Identity Federation（WIF）を使うと、長期間有効なサービスアカウントキーを使わずにEMQXがGCPリソースにアクセスできます。EMQXは外部IDプロバイダー（例：Microsoft Azure）からのトークンをGCPのSecurity Token Service経由で一時的なGCPトークンに交換し、そのトークンでGCPサービスアカウントを代行します。トークンの更新は自動で行われます。

WIFを利用するには、コネクター作成前にGCPプロジェクトで以下を完了してください。

1. Google Cloudコンソールで**IAM & Admin** -> **Workload Identity Federation**に移動し、ワークロードアイデンティティプールを作成。**Pool ID**と**Project Number**を控えます。

2. プールにプロバイダーを追加し、**Provider ID**を控えます。OIDC認証の場合は、外部IDプロバイダーからOAuth 2.0クライアント認証情報（クライアントID、クライアントシークレット、トークンエンドポイントURI）を取得します。

3. ワークロードアイデンティティプールに、BigQueryデータセットやテーブルにアクセス可能なGCPサービスアカウントを代行する権限を付与します。コネクター設定時にサービスアカウントのメールアドレスが必要です。

   ::: tip

   詳細は[Workload Identity Federationの設定](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers)を参照してください。

   :::

**例：Microsoft Azure (Entra ID)**

[Microsoft Entra ID](https://portal.azure.com/)でAPIを公開するアプリケーションを登録し、クライアントシークレットを作成します。コネクター設定時には以下の値を使用します：

| コネクター項目 | 値 |
|---|---|
| **Endpoint URI** | `https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token` |
| **OAuth Client ID** | アプリケーション（クライアント）ID、形式は `api://<application-id>` |
| **OAuth Client Secret** | アプリケーション用に生成したクライアントシークレット |
| **OAuth Request Scope** | `api://<application-id>/.default` |

::: tip 注意

`scope`はアプリケーションのaudience（aud）と完全に一致させる必要があります。そうしないとGCP STSとのトークン交換が失敗します。詳細はMicrosoftの[OAuth 2.0クライアントクレデンシャルフロー](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-client-creds-grant-flow)を参照してください。

サービスアカウントにWIFプールのアクセス権を付与する際は、Subject値に**Object ID**（アプリケーションIDではなく）を使用してください。Object IDはAzureポータルの**Enterprise applications**のアプリ概要ページで確認できます。

:::

### GCPでのデータセットとテーブルの作成・管理

EMQXでBigQueryデータ統合を設定する前に、GCPで必要なデータセットとテーブルを作成してください。

1. Google Cloudコンソールで**BigQuery** -> **Studio**ページに移動します。詳細は[データのロードとクエリ](https://cloud.google.com/bigquery/docs/quickstarts/load-data-console)のクイックスタートガイドを参照してください。

   ::: tip

   使用するサービスアカウントは、対象テーブルに対する書き込み権限を持っている必要があります。

   :::

2. **Explorer**ペインでケバブメニュー（⋮）をクリックし、**Create dataset**を選択。データセット名を指定して**Create dataset**をクリックします。

3. データセット作成後、**Explorer**ペインでデータセットを選択し、**(+) Create table**をクリック。

   - ソースは「Empty Table」を選択。
   - テーブル名を入力。
   - テーブルスキーマを定義。例として、**Edit as text**を切り替えて以下のスキーマをテキストフィールドに貼り付けます。

     ```
     clientid:string,payload:bytes,topic:string,publish_received_at:timestamp
     ```

   - **Create table**をクリックして設定を完了。

4. EMQXが書き込み可能なように権限を設定：

   - データセットを選択し、**Share**をクリック。
   - サービスアカウントのメールアドレスをプリンシパルとして追加。
   - 適切なロールを割り当てます。例：
     - データセットに対して「BigQuery Data Viewer」（読み取りアクセス）
     - テーブルに対して「Editor」（読み書きアクセス）

5. テーブル作成後、クエリを実行して確認可能：

   - テーブルを選択し、**Query**をクリック。
   - 以下のようなSQL文を実行してテーブルにアクセスできることを確認します。

   ```sql
   SELECT * FROM `my_project.my_dataset.my_tab` LIMIT 1000
   ```

## BigQueryコネクターの作成

BigQuery Producer Sinkアクションを追加する前に、EMQXとBigQuery間の接続を確立するためのBigQueryコネクターを作成します。

1. EMQXダッシュボードで**Integration** -> **Connector**をクリック。
2. 画面右上の**Create**をクリックし、コネクター選択ページで**BigQuery**を選択して**Next**をクリック。
3. 名前と説明を入力します（例：`my_bigquery`）。この名前はBigQuery Sinkとコネクターを紐付けるために使用され、クラスター内で一意である必要があります。
4. **Authentication**ドロップダウンから以下の認証方法のいずれかを選択し、対応する項目を入力します：
   - **Service Account JSON**：前述の[サービスアカウントキー作成](#gcpでのサービスアカウントキー作成)でエクスポートしたJSON形式のサービスアカウント認証情報をアップロード。
   - **Workload Identity Federation (WIF)**：以下の項目を入力します。この方法はサービスアカウントJSONファイルを使用しません。詳細は[Workload Identity Federation設定](#gcpでのworkload-identity-federation設定)を参照。
     - **GCP Project ID**：コネクターがアクセスするリソースのプロジェクトID。
     - **GCP Project Number**：同上のプロジェクト番号。
     - **Service Account Email**：代行するサービスアカウントのメールアドレス。
     - **Workload Identity Pool ID**：WIFトークン交換に使用するワークロードアイデンティティプールのID。
     - **Workload Identity Provider ID**：WIFトークン交換に使用するワークロードアイデンティティプロバイダーのID。
     - **Initial Token Configuration**では、認証情報タイプを選択し、対応する項目を入力します。現在サポートされているのは**OIDC with Client Credentials Grant Type**のみ：
       - **Endpoint URI**：OIDCプロバイダーのOAuthトークンエンドポイントURI。
       - **OAuth Client ID**：OAuthサーバーからトークン取得に使用するクライアントID。
       - **OAuth Client Secret**：同じくクライアントシークレット。
       - **OAuth Request Scope**：OAuthアクセストークン取得時に指定する`scope`（プロバイダーによっては必須）。
5. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがBigQueryサーバーに接続可能かテストできます。
6. **Create**ボタンをクリックしてコネクター作成を完了。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSink付きのルール作成に進めます。詳細は[BigQuery Sink付きルールの作成](#create-a-rule-with-bigquery-sink)を参照。

## BigQuery Sink付きルールの作成

ここでは、BigQueryに保存するデータを指定するルールの作成方法を説明します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリック。
2. 画面右上の**Create**をクリック。
3. ルールIDに`my_rule`を入力。
4. **SQL Editor**でルールを設定します。例えば、トピック`t/bq`のMQTTメッセージをBigQueryに保存する場合、以下のSQL文を使用します。

   注意：独自のSQL文を指定する場合、Sinkのペイロードテンプレートで必要なすべてのフィールドを`SELECT`部分に含めてください。

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

   BigQueryテーブルのカラムに存在しないフィールドは選択しないでください。BigQueryが認識できません。

   :::

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習とテストが可能です。

   :::

5. **Add Action**ボタンをクリックし、ルールによってトリガーされるアクションを定義します。**Type of Action**ドロップダウンから`BigQuery`を選択し、ルールで処理したデータをBigQueryに送信するように設定します。
6. **Action**ドロップダウンは`Create Action`のままにするか、既存のBigQuery Sinkを選択できます。ここでは新しいSinkを作成しルールに追加します。
7. **Name**欄にSinkの名前を入力。英数字の組み合わせで指定してください。
8. **Connector**ドロップダウンから先ほど作成した`my_bigquery`を選択。隣のボタンで新規コネクター作成も可能です。設定パラメータは[コネクター作成](#bigqueryコネクターの作成)を参照してください。
9. 以下のBigQueryリソースパラメータを設定：
   - **Project ID**（任意）：対象データセットとテーブルが存在するGCPプロジェクトのID。指定すると、選択したコネクターの認証設定から取得したプロジェクトIDを上書きし、このSinkにのみ適用されます。空欄の場合は認証設定のプロジェクトIDが使用されます。
   - **Dataset**および**Table**：[GCPでのデータセットとテーブルの作成・管理](#gcpでのデータセットとテーブルの作成・管理)で作成したデータセット名とテーブル名をそれぞれ入力。
10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
11. **詳細設定（任意）**：必要に応じて詳細設定オプションを構成します。詳細は[詳細設定](#advanced-settings)を参照してください。
12. **Create**をクリックする前に、**Test Connectivity**でコネクターがBigQueryサーバーに接続可能かテストできます。
13. **Create**ボタンをクリックしてSink設定を完了すると、**Action Outputs**タブに新しいSinkが表示されます。
14. **Create Rule**ページに戻り、**Create**をクリックしてルールを作成します。

これでルールの作成が完了しました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると新しいBigQuery Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/bq`のメッセージがルール`my_rule`で解析されてBigQueryに送信・保存されていることが確認できます。

## ルールのテスト

1. MQTTXを使ってトピック`t/bq`にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/bq -m '{ "msg": "hello BigQuery" }'
   ```

2. Sinkの稼働状況を確認し、新規の受信メッセージと送信メッセージが1件ずつあることを確認します。

3. GCPの**BigQuery** -> **Studio**に移動し、テーブルをクリックして**Query**をクリック。クエリを実行するとメッセージが確認できます。

## 詳細設定

このセクションでは、BigQuery Producer Sinkの詳細な設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、必要に応じて以下のパラメータを調整できます。

| フィールド名                     | 説明                                                         | デフォルト値     |
| -------------------------------- | ------------------------------------------------------------ | --------------- |
| **Buffer Pool Size**             | EMQXとBigQuery間のデータフローを管理するバッファワーカープロセスの数を指定します。これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへの送信を最適化しスムーズなデータ伝送を実現します。 | `16`            |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがこのTTLを超えてバッファ内に滞留するか、BigQueryからの応答やアックが期限内に得られない場合、リクエストは期限切れと見なされます。 | `45`秒          |
| **Health Check Interval**        | SinkがBigQueryとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`秒          |
| **Health Check Interval Jitter** | 複数ノードが同時にヘルスチェックを開始するのを防ぐため、基本間隔に加える一様ランダム遅延です。複数のActionやSourceが同じコネクターを共有する場合に有効です。 | `0`ミリ秒       |
| **Health Check Timeout**         | コネクターがBigQueryとの接続のヘルスチェックを行う際のタイムアウト時間を指定します。 | `60`秒          |
| **Max Buffer Queue Size**        | BigQuery Sinkの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時的に保持し、効率的なデータストリーム処理を行います。システム性能やデータ伝送要件に応じて調整してください。 | `256`           |
| **Query Mode**                   | メッセージ送信を最適化するために`synchronous`または`asynchronous`のリクエストモードを選択できます。非同期モードではBigQueryへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがBigQuery到着前にメッセージを受信する可能性があります。 | `Async`         |
| **Batch Size**                   | EMQXからBigQueryへ単一転送操作で送信するデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率と性能を最適化できます。`1`に設定すると、データレコードはバッチ化せず個別に送信されます。 | `1000`          |
| **Inflight Window**              | 「インフライトキューリクエスト」とは、送信済みでまだ応答やアックを受け取っていないリクエストを指します。この設定はSinkとBigQuery間の通信時に同時に存在可能なインフライトキューリクエストの最大数を制御します。**Request Mode**が`asynchronous`の場合に特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`           |
