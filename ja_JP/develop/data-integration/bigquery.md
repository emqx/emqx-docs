# BigQueryへのMQTTデータ取り込み

[BigQuery](https://cloud.google.com/bigquery?hl=en)は、大量のリレーショナル構造化データ向けのエンタープライズデータウェアハウスです。大規模かつアドホックなSQLベースの分析やレポートに最適化されており、組織の洞察を得るのに最適です。EMQXは、MQTTデータのリアルタイム抽出、処理、分析のためにBigQueryとのシームレスな統合をサポートしています。

本ページでは、EMQXとBigQuery間のデータ統合について包括的に紹介し、データ統合の作成と検証に関する実践的な手順を提供します。

## 動作概要

BigQueryデータ統合は、EMQXの標準機能として提供されており、ユーザーがMQTTデータストリームをGoogle Cloudとシームレスに統合し、IoTアプリケーション開発における豊富なサービスと機能を活用できるよう設計されています。

![bigquery_architecture](./assets/bigquery_architecture.png)

EMQXはルールエンジンとSinkを介してMQTTデータをBigQueryに転送します。全体の流れは以下の通りです。

1. **IoTデバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンを使用し、特定のソースからのMQTTメッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールにマッチし、データ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報の付加などを行います。
3. **BigQueryへのブリッジング**：ルールがメッセージをBigQueryに転送するアクションをトリガーし、データプロパティ、オーダーキー、MQTTトピックとBigQueryトピックのマッピングを簡単に設定できます。これにより、より豊富なコンテキスト情報と順序保証が提供され、柔軟なIoTデータ処理が可能になります。

## 特長と利点

EMQXとBigQueryの統合は、MQTTデータ向けの堅牢でスケーラブルなリアルタイムデータパイプラインを提供します。以下の特長と利点により、IoT分析やデータ駆動型の意思決定を簡素化します。

- **リアルタイムデータ取り込み**：低レイテンシでEMQXからBigQueryへMQTTメッセージをシームレスにストリーミング。即時処理・分析が必要な時間依存型アプリケーションに対応。
- **柔軟なデータマッピング**：MQTTトピックやメッセージペイロードをBigQueryのテーブルやフィールドにカスタマイズしてマッピング可能。
- **スケーラブルかつサーバレスな分析**：BigQueryのフルマネージドでサーバレスなアーキテクチャを活用し、大規模なIoTデータ分析を実現。
- **Google Cloudエコシステムとの容易な統合**：Data Studio、Looker、AI PlatformなどGoogle Cloudサービスとネイティブに連携し、可視化や機械学習を簡単に実装。データ収集から洞察生成までのエンドツーエンドパイプライン構築を支援。

## はじめる前に

このセクションでは、BigQueryデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXのデータ統合[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GCPでサービスアカウントキーを作成する

EMQXがBigQueryに接続できるように、Google Cloudでサービスアカウントを作成し、JSON形式のキーを生成する必要があります。

1. GCPアカウントで[サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)を作成します。サービスアカウントには、使用するデータセットやテーブルへのアクセス権限が必要です。例えば、「BigQuery Data Editor」ロールを付与して対象データセットやテーブルの読み書きを許可するか、少なくともデータへの読み書きアクセス権を持たせてください。

2. 作成したサービスアカウントのメールアドレスをクリックします。

3. **Key**タブをクリックし、**Add key**のドロップダウンリストから**Create new key**を選択してサービスアカウントキーを作成し、JSON形式でダウンロードします。

   ::: tip

   ダウンロードしたサービスアカウントキーは、後でEMQXのBigQuery認証に使用するため、安全に保管してください。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCPでWorkload Identity Federationを設定する

Workload Identity Federation（WIF）を使うと、長期間有効なサービスアカウントキーを使わずにEMQXがGCPリソースにアクセスできます。EMQXは外部IDプロバイダー（例：Microsoft Azure）からのトークンをGCPのSecurity Token Service経由で一時的なGCPトークンに交換し、そのトークンでGCPサービスアカウントを代行します。トークンの更新は自動で行われます。

WIFを利用するには、コネクター作成前にGCPプロジェクトで以下を完了してください。

1. Google Cloudコンソールで**IAM & Admin** -> **Workload Identity Federation**に移動し、ワークロードアイデンティティプールを作成し、**Pool ID**と**Project Number**を控えます。

2. プールにプロバイダーを追加し、**Provider ID**を控えます。OIDC認証の場合は、外部IDプロバイダーからOAuth 2.0クライアント認証情報（クライアントID、クライアントシークレット、トークンエンドポイントURI）を取得してください。

3. BigQueryデータセットやテーブルにアクセスできるGCPサービスアカウントを代行できるよう、ワークロードアイデンティティプールに権限を付与します。コネクター設定時にサービスアカウントのメールアドレスが必要です。

   ::: tip

   詳細は[Workload Identity Federationの設定](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers)をご参照ください。

   :::

**例：Microsoft Azure（Entra ID）**

[Microsoft Entra ID](https://portal.azure.com/)でAPIを公開するアプリケーションを登録し、クライアントシークレットを作成します。コネクター設定時に以下の値を使用します。

| コネクター項目 | 値 |
|---|---|
| **Endpoint URI** | `https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token` |
| **OAuth Client ID** | アプリケーション（クライアント）ID、形式は `api://<application-id>` |
| **OAuth Client Secret** | アプリケーション用に生成したクライアントシークレット |
| **OAuth Request Scope** | `api://<application-id>/.default` |

::: tip 補足

`scope`はアプリケーションのaudience（aud）と完全に一致させる必要があります。そうしないとGCP STSとのトークン交換が失敗します。詳細はMicrosoftの[OAuth 2.0クライアント認証フロー](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-client-creds-grant-flow)を参照してください。

サービスアカウントにWIFプールへのアクセス権を付与する際は、Subject値に**Application IDではなくObject ID**を使用してください。Object IDはAzureポータルのアプリケーションの概要ページの**Enterprise applications**で確認できます。

:::

### GCPでデータセットとテーブルを作成・管理する

EMQXでBigQueryデータ統合を設定する前に、GCPで必要なデータセットとテーブルを作成してください。

1. Google Cloudコンソールの**BigQuery** -> **Studio**ページに移動します。詳細は[データのロードとクエリ](https://cloud.google.com/bigquery/docs/quickstarts/load-data-console)のクイックスタートガイドを参照してください。

   ::: tip

   使用するサービスアカウントには対象テーブルへの書き込み権限が必要です。

   :::

2. **Explorer**ペインでケバブメニュー（⋮）をクリックし、**Create dataset**を選択。データセット名を定義し、**Create dataset**をクリックします。

3. データセット作成後、**Explorer**ペインでデータセットをクリックし、**(+) Create table**をクリック。

   - ソースは「Empty Table」を選択。
   - テーブル名を入力。
   - テーブルスキーマを定義。例えば、**Edit as text**を切り替えて以下のスキーマ定義をテキストフィールドに貼り付けます。

     ```
     clientid:string,payload:bytes,topic:string,publish_received_at:timestamp
     ```

   - **Create table**をクリックして設定を完了。

4. EMQXが書き込み可能なように権限を設定：

   - データセットを選択し、**Share**をクリック。
   - サービスアカウントのメールアドレスをプリンシパルとして追加。
   - 適切なロールを割り当てる（例：データセットに対して「BigQuery Data Viewer」（読み取り）、テーブルに対して「Editor」（読み書き）など）。

5. テーブル作成後、クエリを実行して確認可能：

   - テーブルをクリックし、**Query**をクリック。
   - 以下のような簡単なSQLを実行してテーブルにアクセスできることを確認。

   ```sql
   SELECT * FROM `my_project.my_dataset.my_tab` LIMIT 1000
   ```

## BigQueryコネクターの作成

BigQuery Producer Sinkアクションを追加する前に、EMQXとBigQuery間の接続を確立するためにBigQueryコネクターを作成します。

1. EMQXダッシュボードで**Integration** -> **Connector**をクリック。
2. 画面右上の**Create**をクリックし、コネクター選択画面で**BigQuery**を選択して**Next**をクリック。
3. 名前と説明を入力（例：`my_bigquery`）。名前はBigQuery Sinkとコネクターを紐づけるために使用され、クラスター内で一意である必要があります。
4. **Authentication**ドロップダウンから以下の認証方法のいずれかを選択し、対応する項目を入力：
   - **Service Account JSON**：前述の[サービスアカウントキー作成](#gcpでサービスアカウントキーを作成する)でエクスポートしたJSON形式のサービスアカウント認証情報をアップロード。
   - **Workload Identity Federation (WIF)**：以下の項目を入力。この方法はサービスアカウントJSONファイルを使用しません。詳細は[Workload Identity Federationの設定](#gcpでworkload-identity-federationを設定する)を参照。
     - **GCP Project ID**：コネクターがアクセスするリソースのプロジェクトID。
     - **GCP Project Number**：コネクターがアクセスするリソースのプロジェクト番号。
     - **Service Account Email**：代行されるサービスアカウントのメールアドレス。
     - **Workload Identity Pool ID**：WIFトークン交換に使用するワークロードアイデンティティプールのID。
     - **Workload Identity Provider ID**：WIFトークン交換に使用するワークロードアイデンティティプロバイダーのID。
     - **Initial Token Configuration**では認証情報タイプを選択し、対応する項目を入力。現在は**OIDC with Client Credentials Grant Type**のみサポート：
       - **Endpoint URI**：OIDCプロバイダーのOAuthトークンエンドポイントURI。
       - **OAuth Client ID**：OAuthサーバーからトークンを要求するためのクライアントID。
       - **OAuth Client Secret**：OAuthサーバーからトークンを要求するためのクライアントシークレット。
       - **OAuth Request Scope**：OAuthアクセストークン要求時に必要な場合の`scope`。
5. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがBigQueryサーバーに接続できるかテスト可能。
6. 画面下部の**Create**ボタンをクリックしてコネクター作成を完了。ポップアップで**Back to Connector List**または**Create Rule**を選択可能。ルール作成とSink設定については[BigQuery Sink付きルールの作成](#create-a-rule-with-bigquery-sink)を参照。

## BigQuery Sink付きルールの作成

このセクションでは、BigQueryに保存するデータを指定するルールの作成方法を説明します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリック。

2. 画面右上の**Create**をクリック。

3. ルールIDに`my_rule`を入力。

4. **SQL Editor**でルールを設定。例えば、トピック`t/bq`のMQTTメッセージをBigQueryに保存したい場合、以下のSQLを使用します。

   注意：独自のSQLを指定する場合は、Sinkのペイロードテンプレートで必要な全フィールドを`SELECT`に含めてください。

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

   BigQueryテーブルのカラムに存在するフィールドのみを選択してください。存在しないフィールドはBigQueryで認識されません。

   :::

   ::: tip

   初心者の方は**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習とテストが可能です。

   :::

5. **Add Action**ボタンをクリックし、ルールによってトリガーされるアクションを定義。**Type of Action**ドロップダウンから`BigQuery`を選択し、ルールで処理したデータをBigQueryに送信するように設定。

6. **Action**ドロップダウンは`Create Action`のままにするか、既存のBigQuery Sinkを選択可能。本例では新規Sinkを作成してルールに追加。

7. **Name**欄にSinkの名前を入力。英数字の組み合わせが望ましい。

8. **Connector**ドロップダウンから先ほど作成した`my_bigquery`を選択。新規作成も可能。設定パラメータは[コネクター作成](#bigqueryコネクターの作成)を参照。

9. **Dataset**と**Table**に、[GCPでのデータセットとテーブル作成](#gcpでデータセットとテーブルを作成・管理する)で作成した名前をそれぞれ入力。

12. **Fallback Actions（オプション）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照。

13. **高度な設定（オプション）**：必要に応じて高度な設定オプションを調整可能。詳細は[高度な設定](#advanced-settings)を参照。

14. **Create**をクリックする前に、**Test Connectivity**でコネクターがBigQueryに接続できるかテスト可能。

15. **Create**ボタンをクリックしてSink設定を完了すると、新しいSinkが**Action Outputs**タブに表示されます。

16. **Create Rule**画面に戻り、**Create**をクリックしてルールを作成。

これでルールの作成が完了しました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると新しいBigQuery Sinkが表示されます。

また、**Integration** -> **Flow Designer**を開くとトポロジーが表示され、トピック`t/bq`のメッセージがルール`my_rule`で解析されBigQueryに送信・保存されていることが確認できます。

## ルールのテスト

1. MQTTXを使ってトピック`t/bq`にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/bq -m '{ "msg": "hello BigQuery" }'
   ```

2. Sinkの稼働状況を確認し、新規の受信メッセージと送信メッセージが1件ずつあることを確認。

3. GCPの**BigQuery** -> **Studio**に移動し、テーブルをクリックして**Query**をクリック。クエリを実行するとメッセージが確認できます。

## 高度な設定

このセクションでは、BigQuery Producer Sinkの高度な設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、必要に応じて以下のパラメーターを調整できます。

| 項目名                          | 説明                                                         | デフォルト値     |
|---------------------------------|--------------------------------------------------------------|------------------|
| **Buffer Pool Size**             | EMQXとBigQuery間のデータフローを管理するバッファワーカープロセスの数を指定します。これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへの送信を最適化しスムーズなデータ伝送を保証します。 | `16`             |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。バッファに入った時点からカウントが始まり、このTTLを超えるか、BigQueryからの応答やアックがタイムリーに得られない場合、リクエストは期限切れと見なされます。 | `45`秒           |
| **Health Check Interval**        | SinkがBigQueryとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`秒           |
| **Health Check Interval Jitter** | 基本のヘルスチェック間隔に加える一様乱数遅延（ミリ秒）です。複数のノードが同時にヘルスチェックを開始する確率を減らします。複数のアクションやソースが同じコネクターを共有する場合、ジッターを有効にするとヘルスチェックが少しずつずれて実行されます。 | `0`ミリ秒        |
| **Health Check Timeout**         | コネクターがBigQueryとの接続ヘルスチェックを行う際のタイムアウト時間を指定します。 | `60`秒           |
| **Max Buffer Queue Size**        | BigQuery Sinkの各バッファワーカーが保持可能な最大バイト数を指定します。バッファワーカーはデータを一時的に保持し、BigQueryへの送信を効率化します。システム性能やデータ伝送要件に応じて調整してください。 | `256`            |
| **Query Mode**                   | メッセージ送信の最適化のため、`synchronous`（同期）または`asynchronous`（非同期）のリクエストモードを選択可能です。非同期モードではBigQueryへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがBigQuery到達前にメッセージを受信する可能性があります。 | `Async`          |
| **Batch Size**                   | EMQXからBigQueryへ一度に転送するデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率と性能を最適化可能です。`Batch Size`を`1`に設定すると、データレコードはバッチ化されず個別に送信されます。 | `1000`           |
| **Inflight Window**              | 「インフライトキューリクエスト」とは、開始されたがまだ応答やアックが得られていないリクエストを指します。この設定はSinkがBigQueryと通信中に同時に存在可能なインフライトキューリクエストの最大数を制御します。**Request Mode**が`asynchronous`の場合に特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は、この値を`1`に設定してください。 | `100`            |
