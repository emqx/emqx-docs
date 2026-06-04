# BigQueryへのMQTTデータ取り込み

[BigQuery](https://cloud.google.com/bigquery?hl=en)は、大量のリレーショナル構造化データ向けのエンタープライズデータウェアハウスです。大規模なアドホックSQLベースの分析およびレポーティングに最適化されており、組織の洞察を得るのに最適です。EMQXは、MQTTデータのリアルタイム抽出、処理、分析のためにBigQueryとのシームレスな統合をサポートしています。

本ページでは、EMQXとBigQuery間のデータ統合について包括的に紹介し、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作の仕組み

BigQueryデータ統合は、EMQXの標準機能として提供されており、ユーザーがMQTTデータストリームをGoogle Cloudとシームレスに統合し、IoTアプリケーション開発のための豊富なサービスや機能を活用できるよう設計されています。

![bigquery_architecture](./assets/bigquery_architecture.png)

EMQXはルールエンジンとSinkを介してMQTTデータをBigQueryに転送します。全体のプロセスは以下の通りです。

1. **IoTデバイスがメッセージをパブリッシュする**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理する**：組み込みのルールエンジンを使用し、特定のソースからのMQTTメッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、文脈情報によるメッセージの付加などの処理を行います。
3. **BigQueryへのブリッジング**：ルールはメッセージをBigQueryに転送するアクションをトリガーし、データプロパティ、オーダーキー、MQTTトピックとBigQueryトピックのマッピングを簡単に設定できます。これにより、データ統合におけるより豊かなコンテキスト情報と順序保証が提供され、柔軟なIoTデータ処理が可能になります。

## 特長と利点

EMQXとBigQueryの統合は、MQTTデータの堅牢でスケーラブルかつリアルタイムなデータパイプラインを提供します。以下の特長と利点により、IoT分析とデータ駆動型の意思決定を簡素化します。

- **リアルタイムデータ取り込み**：低レイテンシでEMQXからBigQueryへMQTTメッセージをシームレスにストリームします。即時処理と分析が必要な時間依存型アプリケーションに対応します。
- **柔軟なデータマッピング**：MQTTトピックおよびメッセージペイロードをBigQueryのテーブルやフィールドにカスタマイズしてマッピング可能です。
- **スケーラブルでサーバレスな分析**：BigQueryの完全管理型サーバレスアーキテクチャを活用し、大規模なIoTデータを分析できます。
- **Google Cloudエコシステムとの簡単な統合**：Data Studio、Looker、AI PlatformなどのGoogle Cloudサービスとネイティブに連携し、可視化や機械学習を簡単に実現。データ収集から洞察生成までのエンドツーエンドパイプライン構築を簡素化します。

## はじめる前に

本セクションでは、BigQueryデータ統合を作成する前に準備すべき事項を説明します。

### 前提条件

- EMQXのデータ統合[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GCPでのサービスアカウントキーの作成

EMQXがBigQueryに接続できるように、Google Cloudでサービスアカウントを作成し、JSON形式のキーを生成する必要があります。

1. GCPアカウントで[サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)を作成します。サービスアカウントには、使用するデータセットやテーブルへのアクセス権限が必要です。例えば、「BigQuery Data Editor」ロールを付与して対象データセットやテーブルの読み書きを許可するか、少なくともデータの読み書きアクセス権を持たせてください。

2. 作成したサービスアカウントのメールアドレスをクリックします。

3. **Key**タブをクリックし、**Add key**のドロップダウンから**Create new key**を選択してサービスアカウントキーを作成し、JSON形式でダウンロードします。

   ::: tip

   ダウンロードしたサービスアカウントキーは後でEMQXのBigQuery認証に使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCPでのWorkload Identity Federationの設定

Workload Identity Federation（WIF）を利用すると、EMQXは長期間有効なサービスアカウントキーを使わずにGCPリソースにアクセスできます。代わりに、EMQXは外部IDプロバイダー（例：Microsoft Azure）から取得したトークンをGCPのSecurity Token Service経由で一時的なGCPトークンに交換し、そのトークンを使ってGCPサービスアカウントを代理します。トークンの更新は自動で行われます。

WIFを利用するには、コネクター作成前にGCPプロジェクトで以下を完了してください。

1. Google Cloudコンソールで**IAM & Admin** -> **Workload Identity Federation**に移動し、ワークロードアイデンティティプールを作成し、**Pool ID**と**Project Number**を控えます。

2. プールにプロバイダーを追加し、**Provider ID**を控えます。OIDC認証の場合、外部IDプロバイダーからOAuth 2.0クライアント認証情報（クライアントID、クライアントシークレット、トークンエンドポイントURI）を取得します。

3. ワークロードアイデンティティプールにBigQueryのデータセットやテーブルにアクセス可能なGCPサービスアカウントの代理権限を付与します。コネクター設定時にサービスアカウントのメールアドレスが必要です。

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

`scope`はアプリケーションのaudience（aud）と完全に一致させる必要があります。そうしないとGCP STSとのトークン交換が失敗します。詳細はMicrosoftの[OAuth 2.0クライアント認証フロー](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-client-creds-grant-flow)を参照してください。

サービスアカウントにWIFプールへのアクセス権を付与する際は、Subject値に**Object ID**（アプリケーションIDではない）を使用してください。Object IDはAzureポータルのアプリケーションの概要ページの**Enterprise applications**に表示されます。

:::

### GCPでのデータセットおよびテーブルの作成と管理

EMQXでBigQueryデータ統合を設定する前に、GCPで必要なデータセットとテーブルを作成してください。

1. Google Cloudコンソールで**BigQuery** -> **Studio**ページに移動します。詳細は[データのロードとクエリ](https://cloud.google.com/bigquery/docs/quickstarts/load-data-console)クイックスタートガイドを参照してください。

   ::: tip

   使用するサービスアカウントには、対象テーブルへの書き込み権限が必要です。

   :::

2. **Explorer**ペインでケバブアイコン（⋮）をクリックし、**Create dataset**を選択します。データセット名を定義し、**Create dataset**をクリックします。

3. データセット作成後、**Explorer**ペインでデータセットをクリックし、**(+) Create table**をクリックします。

   - ソースは「Empty Table」を選択します。

   - テーブル名を入力します。

   - テーブルスキーマを定義します。例えば、**Edit as text**を切り替え、以下のスキーマ定義をテキストフィールドに貼り付けます。

     ```
     clientid:string,payload:bytes,topic:string,publish_received_at:timestamp
     ```

   - **Create table**をクリックして設定を完了します。

4. EMQXが書き込みできるように権限を設定します。

   - データセットを選択し、**Share**をクリックします。

   - サービスアカウントのメールアドレスをプリンシパルとして追加します。

   - 以下のような適切なロールを割り当てます。

     - データセットに対して「BigQuery Data Viewer」（読み取りアクセス）

     - テーブルに対して「Editor」（読み書きアクセス）

5. テーブル作成後、クエリを実行して確認できます。

   - テーブルをクリックし、**Query**をクリックします。

   - 以下のようなシンプルなSQLを実行してテーブルにアクセスできることを確認します。

     ```sql
     SELECT * FROM `my_project.my_dataset.my_tab` LIMIT 1000
     ```

## BigQueryコネクターの作成

BigQuery Producer Sinkアクションを追加する前に、EMQXとBigQuery間の接続を確立するためのBigQueryコネクターを作成する必要があります。

1. EMQXダッシュボードで**Integration** -> **Connector**をクリックします。

2. ページ右上の**Create**をクリックし、コネクター選択ページで**BigQuery**を選択して**Next**をクリックします。

3. 名前と説明を入力します（例：`my_bigquery`）。この名前はBigQuery Sinkとコネクターを紐付けるために使用され、クラスター内で一意である必要があります。

4. **Authentication**ドロップダウンから以下の認証方法のいずれかを選択し、対応する項目を入力します。

   - **Service Account JSON**：前述の[サービスアカウントキーの作成](#gcpでのサービスアカウントキーの作成)でエクスポートしたJSON形式のサービスアカウント認証情報をアップロードします。

   - **Workload Identity Federation (WIF)**：以下の項目を入力します。この方法はサービスアカウントJSONファイルを使用しません。前提条件は[Workload Identity Federationの設定](#gcpでのworkload-identity-federationの設定)を参照してください。

     - **GCP Project ID**：コネクターがアクセスするリソースのプロジェクトID

     - **GCP Project Number**：コネクターがアクセスするリソースのプロジェクト番号

     - **Service Account Email**：代理するサービスアカウントのメールアドレス

     - **Workload Identity Pool ID**：WIFトークン交換に使用するワークロードアイデンティティプールのID

     - **Workload Identity Provider ID**：WIFトークン交換に使用するワークロードアイデンティティプロバイダーのID

     - **Initial Token Configuration**では認証情報の種類を選択し、対応する項目を入力します。現在サポートされているのは**OIDC with Client Credentials Grant Type**のみです。

       - **Endpoint URI**：OIDCプロバイダーのOAuthトークンエンドポイントURI

       - **OAuth Client ID**：OAuthサーバーからトークンを取得するためのクライアントID

       - **OAuth Client Secret**：OAuthサーバーからトークンを取得するためのクライアントシークレット

       - **OAuth Request Scope**：OAuthアクセストークン取得時に指定する`scope`（プロバイダーによっては必須）

5. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがBigQueryサーバーに接続できるかテストできます。

6. ページ下部の**Create**ボタンをクリックしてコネクターを作成します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを指定するルール作成に進めます。詳細は[BigQuery Sink付きルールの作成](#create-a-rule-with-bigquery-sink)を参照してください。

## BigQuery Sink付きルールの作成

本セクションでは、BigQueryに保存するデータを指定するルールの作成方法を示します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力します。

4. **SQL Editor**でルールを設定します。例えば、トピック`t/bq`のMQTTメッセージをBigQueryに保存したい場合、以下のSQL文を使用します。

   注意：独自のSQL文を指定する場合は、Sinkのペイロードテンプレートで必要なすべてのフィールドを`SELECT`に含めるようにしてください。

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

   BigQueryテーブルのカラムに存在しないフィールドは選択しないでください。BigQueryが未知のフィールドとして認識しません。

   :::

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールの学習とテストを行ってください。

   :::

5. **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action**ドロップダウンから`BigQuery`を選択すると、EMQXはルールで処理したデータをBigQueryに送信します。

6. **Action**ドロップダウンは`Create Action`のままにするか、既存のBigQuery Sinkを選択できます。本例では新しいSinkを作成してルールに追加します。

7. **Name**欄にSinkの名前を入力します。名前は英数字の組み合わせで指定してください。

8. **Connector**ドロップダウンから先ほど作成した`my_bigquery`を選択します。新しいコネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメーターの詳細は[コネクターの作成](#bigqueryコネクターの作成)を参照してください。

9. **Dataset**および**Table**に、[GCPでのデータセットおよびテーブルの作成と管理](#gcpでのデータセットおよびテーブルの作成と管理)で作成したデータセット名とテーブル名をそれぞれ入力します。

12. **フォールバックアクション（オプション）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にこれらがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

13. **詳細設定（オプション）**：必要に応じて詳細設定オプションを構成します。詳細は[詳細設定](#advanced-settings)を参照してください。

14. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがBigQueryサーバーに接続できるかテストできます。

15. **Create**ボタンをクリックしてSinkの設定を完了すると、新しいSinkが**Action Outputs**タブに表示されます。

16. **Create Rule**ページに戻り、**Create**をクリックしてルールを作成します。

これでルールの作成が完了しました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると新しいBigQuery Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認でき、トピック`t/bq`のメッセージがルール`my_rule`で解析されてBigQueryに送信・保存されていることがわかります。

## ルールのテスト

1. MQTTXを使ってトピック`t/bq`にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/bq -m '{ "msg": "hello BigQuery" }'
   ```

2. Sinkの稼働状況を確認し、新しい受信メッセージと送信メッセージが1件ずつあることを確認します。

3. GCPの**BigQuery** -> **Studio**に移動し、テーブルをクリックして**Query**をクリックします。クエリを実行するとメッセージが確認できます。

## 詳細設定

本セクションでは、BigQuery Producer Sinkの詳細な設定オプションについて説明します。ダッシュボードでSinkを設定する際に**Advanced Settings**を展開し、用途に応じて以下のパラメーターを調整できます。

| 項目名                           | 説明                                                         | デフォルト値    |
| -------------------------------- | ------------------------------------------------------------ | --------------- |
| **Buffer Pool Size**             | EMQXとBigQuery間のデータフローを管理するバッファワーカープロセスの数を指定します。これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへ送信します。パフォーマンス最適化とスムーズなデータ送信に重要です。 | `16`            |
| **Request TTL**                  | リクエストTTL（Time To Live）は、リクエストがバッファに入ってから有効とみなされる最大時間（秒）を指定します。TTLを超えてバッファに滞留するか、送信後にBigQueryからの応答やアックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45`秒          |
| **Health Check Interval**        | SinkがBigQueryとの接続状態を自動でヘルスチェックする間隔（秒）を指定します。 | `15`秒          |
| **Health Check Interval Jitter** | 複数のノードが同時にヘルスチェックを開始するのを防ぐため、基本のヘルスチェック間隔に加える一様ランダム遅延です。複数のアクションやソースが同じコネクターを共有する場合、ジッターを有効にするとヘルスチェックの開始時刻がずれます。 | `0`ミリ秒       |
| **Health Check Timeout**         | コネクターがBigQueryとの接続のヘルスチェックを行う際のタイムアウト時間を指定します。 | `60`秒          |
| **Max Buffer Queue Size**        | BigQuery Sinkの各バッファワーカーがバッファリングできる最大バイト数を指定します。バッファワーカーはデータを一時的に保持し、BigQueryへの送信を効率化します。システム性能やデータ送信要件に応じて調整してください。 | `256`           |
| **Query Mode**                   | メッセージ送信を最適化するために、`synchronous`（同期）または`asynchronous`（非同期）のリクエストモードを選択できます。非同期モードではBigQueryへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがBigQuery到達前にメッセージを受け取る可能性があります。 | `Async`         |
| **Batch Size**                   | EMQXからBigQueryへ一度に転送するデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率と性能を最適化できます。`Batch Size`を`1`に設定すると、データレコードはバッチ化せず個別に送信されます。 | `1000`          |
| **Inflight Window**              | 「インフライトキューリクエスト」とは、送信済みでまだ応答やアックを受け取っていないリクエストのことです。この設定はSinkとBigQuery間の通信中に同時に存在できる最大インフライトキューリクエスト数を制御します。**Request Mode**が`asynchronous`の場合に特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は、この値を`1`に設定してください。 | `100`           |
