# BigQuery に MQTT データを取り込む

[BigQuery](https://cloud.google.com/bigquery?hl=en) は、大量のリレーショナル構造化データ向けのエンタープライズデータウェアハウスです。大規模でアドホックな SQL ベースの分析やレポーティングに最適化されており、組織の洞察を得るのに適しています。EMQX は、MQTT データのリアルタイム抽出、処理、分析のために BigQuery とのシームレスな統合をサポートしています。

本ページでは、EMQX と BigQuery 間のデータ統合について包括的に紹介し、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作の仕組み

BigQuery データ統合は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームを Google Cloud とシームレスに統合し、IoT アプリケーション開発のための豊富なサービスや機能を活用できるよう設計されています。

![bigquery_architecture](./assets/bigquery_architecture.png)

EMQX は、ルールエンジンと Sink を介して MQTT データを BigQuery に転送します。全体のプロセスは以下の通りです：

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンを使用して、特定のソースからの MQTT メッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などを行います。
3. **BigQuery へのブリッジング**：ルールがトリガーされると、メッセージを BigQuery に転送するアクションが実行されます。データプロパティ、オーダーキー、MQTT トピックと BigQuery トピックのマッピングを簡単に設定でき、データ統合における豊富なコンテキスト情報と順序保証を提供し、柔軟な IoT データ処理を可能にします。

## 特長と利点

EMQX と BigQuery の統合は、MQTT データのための堅牢でスケーラブルかつリアルタイムなデータパイプラインを提供します。以下の特長と利点により、IoT 分析やデータ駆動型の意思決定を簡素化します：

- **リアルタイムデータ取り込み**：EMQX から BigQuery へ低レイテンシで MQTT メッセージをシームレスにストリームします。即時処理と分析が必要な時間敏感なアプリケーションに対応します。
- **柔軟なデータマッピング**：MQTT トピックおよびメッセージペイロードを BigQuery のテーブルやフィールドにカスタマイズしてマッピング可能です。
- **スケーラブルでサーバレスな分析**：BigQuery の完全管理型サーバレスアーキテクチャを活用し、大規模な IoT データの分析を実現します。
- **Google Cloud エコシステムとの簡単な統合**：Data Studio、Looker、AI Platform などの Google Cloud サービスとネイティブに連携し、可視化や機械学習を簡単に実現。データ収集から洞察生成までのエンドツーエンドパイプライン構築を簡素化します。

## はじめる前に

このセクションでは、BigQuery データ統合を作成する前に完了すべき準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### GCP でサービスアカウントキーを作成する

EMQX が BigQuery に接続できるように、Google Cloud でサービスアカウントを作成し、JSON 形式のキーを生成する必要があります。

1. GCP アカウントで[サービスアカウント](https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount)を作成します。サービスアカウントには、使用するデータセットやテーブルへのアクセス権限が必要です。例えば、対象のデータセットやテーブルの読み書き権限を持つ「BigQuery Data Editor」ロールを付与するか、少なくとも読み書きアクセスがあることを確認してください。

2. 作成したサービスアカウントのメールアドレスをクリックします。

3. **Key** タブをクリックし、**Add key** のドロップダウンリストから **Create new key** を選択して、サービスアカウントキーを作成し、JSON 形式でダウンロードします。

   ::: tip

   ダウンロードしたサービスアカウントキーのファイルは安全に保管してください。後で EMQX が BigQuery に認証する際に使用します。

   :::

   <img src="./assets/gcp_pubsub/service-account-key.png" alt="サービスアカウントキー" style="zoom:50%;" />

### GCP でワークロードアイデンティティフェデレーションを設定する

ワークロードアイデンティティフェデレーション（WIF）を使うと、長期間有効なサービスアカウントキーを使わずに EMQX が GCP リソースにアクセスできます。EMQX は外部アイデンティティプロバイダー（例：Microsoft Azure）からトークンを受け取り、GCP のセキュリティトークンサービス経由で一時的な GCP トークンと交換し、そのトークンでサービスアカウントを代行します。トークンの更新も自動で行われます。

WIF を使用するには、コネクター作成前に GCP プロジェクトで以下を完了してください。

1. Google Cloud コンソールで **IAM & Admin** -> **Workload Identity Federation** に移動し、ワークロードアイデンティティプールを作成し、**Pool ID** と **Project Number** を控えます。

2. プールにプロバイダーを追加し、**Provider ID** を控えます。OIDC ベースの認証の場合、外部アイデンティティプロバイダーから OAuth 2.0 クライアント認証情報（クライアント ID、クライアントシークレット、トークンエンドポイント URI）を取得します。

3. ワークロードアイデンティティプールに、BigQuery のデータセットやテーブルにアクセスできる GCP サービスアカウントを代行する権限を付与します。コネクター設定時にサービスアカウントのメールアドレスが必要です。

   ::: tip

   詳細な手順は [ワークロードアイデンティティフェデレーションの設定](https://cloud.google.com/iam/docs/workload-identity-federation-with-other-providers) を参照してください。

   :::

**例：Microsoft Azure (Entra ID)**

[Microsoft Entra ID](https://portal.azure.com/) で API を公開するアプリケーションを登録し、クライアントシークレットを作成します。コネクター設定時には以下の値を使用します：

| コネクター項目 | 値 |
|---|---|
| **Endpoint URI** | `https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token` |
| **OAuth Client ID** | `api://<application-id>` 形式のアプリケーション（クライアント）ID |
| **OAuth Client Secret** | アプリケーション用に生成したクライアントシークレット |
| **OAuth Request Scope** | `api://<application-id>/.default` |

::: tip 補足

`scope` はアプリケーションのオーディエンス（aud）と完全に一致させる必要があります。そうしないと GCP STS とのトークン交換が失敗します。詳細は Microsoft のドキュメントの [OAuth 2.0 クライアント認証フロー](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-client-creds-grant-flow) を参照してください。

サービスアカウントに WIF プールへのアクセスを付与する際は、Subject 値にアプリケーション ID ではなく **Object ID** を使用してください。Object ID は Azure ポータルのアプリケーションの概要ページの **Enterprise applications** に表示されます。

:::

### GCP でデータセットとテーブルを作成・管理する

EMQX で BigQuery データ統合を設定する前に、GCP で必要なデータセットとテーブルを作成してください。

1. Google Cloud コンソールで **BigQuery** -> **Studio** ページに移動します。詳細は [データのロードとクエリ](https://cloud.google.com/bigquery/docs/quickstarts/load-data-console) のクイックスタートガイドを参照してください。

   ::: tip

   使用するサービスアカウントには、対象テーブルへの書き込み権限が必要です。

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

4. EMQX が書き込みできるように権限を設定します：

   - データセットを選択し、**Share** をクリックします。  
   - サービスアカウントのメールアドレスをプリンシパルとして追加します。
   - 以下のような適切なロールを割り当てます：
     - データセットに対して「BigQuery Data Viewer」（読み取りアクセス）
     - テーブルに対して「Editor」（読み書きアクセス）

5. テーブル作成後、クエリを実行して確認できます：

   - テーブルをクリックし、**Query** をクリックします。

   - 以下のようなシンプルな SQL 文を実行してテーブルにアクセスできることを確認します：

     ```sql
     SELECT * FROM `my_project.my_dataset.my_tab` LIMIT 1000
     ```

## BigQuery コネクターを作成する

BigQuery Producer Sink アクションを追加する前に、EMQX と BigQuery 間の接続を確立するための BigQuery コネクターを作成する必要があります。

1. EMQX ダッシュボードに移動し、**Integration** -> **Connector** をクリックします。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **BigQuery** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my_bigquery`）。この名前は BigQuery Sink とコネクターを紐付けるために使用され、クラスター内で一意である必要があります。
4. **Authentication** ドロップダウンから以下の認証方法のいずれかを選択し、対応する項目を入力します：
   - **Service Account JSON**：前述の「GCP でサービスアカウントキーを作成する」でエクスポートした JSON 形式のサービスアカウント認証情報をアップロードします。
   - **Workload Identity Federation (WIF)**：以下の項目を入力します。この方法はサービスアカウント JSON ファイルを使用しません。詳細は「GCP でワークロードアイデンティティフェデレーションを設定する」を参照してください。
     - **GCP Project ID**：コネクターがアクセスするリソースのプロジェクト ID
     - **GCP Project Number**：コネクターがアクセスするリソースのプロジェクト番号
     - **Service Account Email**：代行するサービスアカウントのメールアドレス
     - **Workload Identity Pool ID**：WIF トークン交換に使用するワークロードアイデンティティプールの ID
     - **Workload Identity Provider ID**：WIF トークン交換に使用するワークロードアイデンティティプロバイダーの ID
     - **Initial Token Configuration** で資格情報タイプを選択し、対応する項目を入力します。現在サポートされているのは **OIDC with Client Credentials Grant Type** のみです：
       - **Endpoint URI**：OIDC プロバイダーの OAuth トークンエンドポイント URI
       - **OAuth Client ID**：OAuth サーバーからトークンを要求するためのクライアント ID
       - **OAuth Client Secret**：OAuth サーバーからトークンを要求するためのクライアントシークレット
       - **OAuth Request Scope**：OAuth アクセストークン要求時に指定する `scope`（プロバイダーによって必要な場合）
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが BigQuery サーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして Sink を含むルール作成に進むことができます。詳細は [BigQuery Sink を使ったルールの作成](#create-a-rule-with-bigquery-sink) を参照してください。

## BigQuery Sink を使ったルールの作成

このセクションでは、BigQuery に保存するデータを指定するルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` と入力します。

4. **SQL Editor** でルールを設定します。例えば、トピック `t/bq` の MQTT メッセージを BigQuery に保存したい場合、以下の SQL 構文を使用します。

   注意：独自の SQL 構文を指定する場合、Sink のペイロードテンプレートで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

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

   BigQuery テーブルのカラムに該当するフィールドのみを選択してください。そうでないと BigQuery が未知のフィールドとして認識しません。

   :::

   ::: tip

   初心者の方は **SQL Examples** をクリックし、**Enable Test** を有効にして SQL ルールを学習・テストできます。

   :::

5. **Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。**Type of Action** ドロップダウンから `BigQuery` を選択すると、EMQX はルールで処理されたデータを BigQuery に送信します。

6. **Action** ドロップダウンは `Create Action` のままにするか、既存の BigQuery Sink を選択できます。この例では新しい Sink を作成し、ルールに追加します。

7. **Name** フィールドに Sink の名前を入力します。名前は英数字の組み合わせにしてください。

8. **Connector** ドロップダウンから先ほど作成した `my_bigquery` を選択します。新しいコネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータの詳細は [コネクターの作成](#create-a-connector) を参照してください。

9. **Dataset** と **Table** に、[GCP でデータセットとテーブルを作成・管理する](#create-and-manage-datasets-and-tables-in-gcp) で作成したデータセット名とテーブル名をそれぞれ入力します。

12. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のために、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

13. **詳細設定（任意）**：必要に応じて詳細設定オプションを構成します。詳細は [詳細設定](#advanced-settings) を参照してください。

14. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが BigQuery サーバーに接続できるかテストできます。

15. **Create** ボタンをクリックして Sink の設定を完了すると、**Action Outputs** タブに新しい Sink が表示されます。

16. **Create Rule** ページに戻り、**Create** をクリックしてルールを作成します。

これでルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい BigQuery Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/bq` のメッセージがルール `my_rule` によって解析され、BigQuery に送信・保存されていることが確認できます。

## ルールのテスト

1. MQTTX を使ってトピック `t/bq` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/bq -m '{ "msg": "hello BigQuery" }'
   ```

2. Sink の稼働状況を確認し、新しい受信メッセージと送信メッセージがそれぞれ 1 件ずつあることを確認します。

3. GCP の **BigQuery** -> **Studio** に移動し、テーブルをクリックして **Query** をクリックします。クエリを実行するとメッセージが確認できます。

## 詳細設定

このセクションでは、BigQuery Producer Sink の詳細設定オプションについて説明します。ダッシュボードで Sink を設定する際に **Advanced Settings** を展開し、用途に応じて以下のパラメータを調整できます。

| フィールド名                      | 説明                                                         | デフォルト値    |
| -------------------------------- | ------------------------------------------------------------ | -------------- |
| **Buffer Pool Size**             | EMQX と BigQuery 間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへの送信を最適化し、スムーズなデータ伝送を保証するために重要です。 | `16`           |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがこの TTL を超えてバッファ内に留まるか、送信後に BigQuery から適時の応答やアックを受け取れなかった場合、リクエストは期限切れと見なされます。 | `45` 秒        |
| **Health Check Interval**        | Sink が BigQuery との接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒        |
| **Health Check Interval Jitter** | 複数のノードが同時にヘルスチェックを開始するのを防ぐため、基本間隔に加える一様ランダム遅延です。複数のアクションやソースが同じコネクターを共有している場合、ジッターを有効にするとヘルスチェックの開始タイミングがずれます。 | `0` ミリ秒     |
| **Health Check Timeout**         | コネクターが BigQuery との接続ヘルスチェックを行う際のタイムアウト時間を指定します。 | `60` 秒        |
| **Max Buffer Queue Size**        | BigQuery Sink の各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時的に保持し、効率的にデータストリームを処理します。システム性能やデータ伝送要件に応じて調整してください。 | `256`          |
| **Query Mode**                   | メッセージ送信の最適化のため、`synchronous`（同期）または `asynchronous`（非同期）リクエストモードを選択できます。非同期モードでは、BigQuery への書き込みが MQTT メッセージのパブリッシュ処理をブロックしません。ただし、クライアントがメッセージを BigQuery 到着前に受信する可能性があります。 | `Async`        |
| **Batch Size**                   | EMQX から BigQuery へ一度に転送するデータの最大バッチサイズを指定します。サイズを調整することで、データ転送の効率と性能を最適化できます。`Batch Size` を `1` に設定すると、データレコードはバッチ化せず個別に送信されます。 | `1000`         |
| **Inflight Window**              | 「インフライトキューリクエスト」とは、送信済みでまだ応答やアックを受け取っていないリクエストを指します。この設定は、Sink と BigQuery 間の通信中に同時に存在できるインフライトキューリクエストの最大数を制御します。**Request Mode** が `asynchronous` の場合に特に重要です。同一 MQTT クライアントからのメッセージを厳密に順序処理する必要がある場合は、この値を `1` に設定してください。 | `100`          |
