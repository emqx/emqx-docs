# Amazon S3 TablesへのMQTTデータ取り込み

<<<<<<< HEAD
[Amazon S3 Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html)は、分析ワークロードに最適化された専用のストレージソリューションです。IoTセンサーの計測値などの表形式データをApache Icebergフォーマットで高性能かつスケーラブル、かつ安全に保存できます。

EMQXはAmazon S3 Tablesとのシームレスな連携をサポートし、MQTTメッセージを効率的にS3テーブルバケットに格納できます。この連携により、柔軟でスケーラブルなIoTデータストレージが可能となり、Amazon Athena、Amazon Redshift、Amazon EMRなどのAWSサービスを活用した高度な分析や処理が容易になります。
=======
[Amazon S3 Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) は、分析ワークロードに最適化された専用のストレージソリューションです。Apache Iceberg フォーマットで IoT センサーの読み取り値などの表形式データを高性能かつスケーラブル、かつセキュアに保存できます。

EMQX は Amazon S3 Tables とのシームレスな連携をサポートし、MQTT メッセージを効率的に S3 テーブルバケットに格納できます。この連携により、柔軟かつスケーラブルな IoT データストレージが可能となり、Amazon Athena、Amazon Redshift、Amazon EMR などの AWS サービスを活用した高度な分析や処理が促進されます。
>>>>>>> origin/release-5.10

本ページでは、EMQXとAmazon S3 Tables間のデータ統合について詳しく解説し、ルールおよびSinkの作成方法について実践的なガイダンスを提供します。

## 動作概要

<<<<<<< HEAD
EMQXのAmazon S3 Tables連携は標準搭載機能です。この連携はEMQXのルールエンジンとS3 Tables Sinkを活用し、MQTTメッセージを変換してApache Iceberg形式のテーブルに直接ストリーミングし、S3テーブルバケットに保存して長期保管および下流分析を実現します。

典型的なIoTシナリオでは以下のように動作します：

- **EMQX**はMQTTブローカーとして機器の接続管理、メッセージルーティング、データ処理を担当します。
- **Amazon S3 Tables**はMQTTメッセージデータを表形式で耐久的かつクエリ可能なストレージとして提供します。
- **Amazon Athena**はIcebergテーブルの定義や格納データに対するSQLクエリの実行に使用されます。
=======
EMQX の Amazon S3 Tables 連携は標準機能として提供されています。この連携は EMQX のルールエンジンと S3 Tables Sink を活用し、MQTT メッセージを変換したうえで Apache Iceberg フォーマットのテーブルとして S3 テーブルバケットに直接ストリーミングし、長期保存および下流分析に利用します。

典型的な IoT シナリオでは以下のように動作します：

- **EMQX** は MQTT ブローカーとして機能し、デバイス接続、メッセージルーティング、データ処理を担当します。
- **Amazon S3 Tables** は MQTT メッセージデータを表形式で耐久的かつクエリ可能なストレージ先として提供します。
- **Amazon Athena** は Iceberg テーブルの定義や保存データに対する SQL クエリ実行に使用されます。
>>>>>>> origin/release-5.10

![emqx-integration-s3-tables](./assets/emqx-integration-s3-tables.png)

ワークフローは以下の通りです：

<<<<<<< HEAD
1. **デバイスのEMQX接続**：IoTデバイスがMQTTでEMQXに接続し、テレメトリデータをパブリッシュします。
2. **メッセージルーティングとルールマッチング**：EMQXのルールエンジンが受信したMQTTメッセージを定義済みトピックに照合し、特定のフィールドや値を抽出します。
3. **データ変換**：EMQXのルールでペイロードのフィルタリング、変換、付加処理を行い、ターゲットのIcebergテーブルのスキーマに合わせます。
4. **Amazon S3 Tablesへの書き込み**：ルールがS3 Tables Sinkアクションをトリガーし、変換済みデータをバッチ処理してIceberg互換の書き込みAPIでS3 Tablesに送信します。データはIcebergテーブルのパーティション下にParquetファイルとして永続化されます。
5. **クエリと分析**：取り込まれたデータはAmazon Athenaでクエリ可能となり、他のデータセットと結合したり、Redshift Spectrum、Amazon EMR、PrestoやTrinoなどのサードパーティ分析エンジンで分析できます。
=======
1. **デバイスの EMQX への接続**：IoT デバイスが MQTT 経由で EMQX に接続し、テレメトリデータをパブリッシュします。
2. **メッセージルーティングとルールマッチング**：EMQX のルールエンジンが受信した MQTT メッセージを定義済みトピックに照合し、特定のフィールドや値を抽出します。
3. **データ変換**：EMQX のルールでメッセージペイロードをフィルタリング、変換、または拡充し、ターゲットの Iceberg テーブルのスキーマに合わせます。
4. **Amazon S3 Tables への書き込み**：ルールが S3 Tables Sink アクションをトリガーし、変換済みデータをバッチ処理して Iceberg 互換の書き込み API を使い S3 Tables に送信します。データは Iceberg テーブルのパーティション下に Parquet ファイルとして永続化されます。
5. **クエリと分析**：取り込まれたデータは Amazon Athena でクエリ可能となり、他のデータセットと結合したり、Redshift Spectrum、Amazon EMR、Presto、Trino などのサードパーティ分析エンジンで分析できます。
>>>>>>> origin/release-5.10

## 特長とメリット

<<<<<<< HEAD
EMQXのAmazon S3 Tablesデータ統合を利用することで、以下の特長と利点を得られます：

- **リアルタイムストリーム処理**：EMQXのルールエンジンにより、MQTTメッセージをリアルタイムに抽出・変換・条件付きルーティングしてS3 Tablesに届けられます。
- **IcebergベースのS3ストレージ**：メッセージはApache Icebergテーブルに書き込まれ、従来のデータベース不要でSQLライクなアクセスが可能です。
- **分析ツールとの簡単連携**：S3 Tablesに格納後はAmazon Athena（SQL）、Amazon EMR、Redshift Spectrum、Presto、Trino、Snowflakeなどでクエリや分析が行えます。
- **柔軟かつコスト効率の高いストレージ**：Amazon S3は高耐久かつ低コストのオブジェクトストレージを提供し、アーカイブ、コンプライアンス、時系列分析に最適です。

## はじめる前に

このセクションでは、EMQXでAmazon S3 Tables Sinkを作成するための準備について説明します。

### 前提条件

以下の内容に慣れていることを推奨します：
=======
EMQX で Amazon S3 Tables データ統合を利用することで、以下の特長と利点が得られます：

- **リアルタイムストリーム処理**：EMQX のルールエンジンにより、MQTT メッセージをリアルタイムに抽出、変換、条件付きルーティングして S3 Tables に配信可能です。
- **Iceberg ベースの S3 ストレージ**：メッセージは Apache Iceberg テーブルに書き込まれ、従来のデータベース不要で SQL ライクなアクセスが可能です。
- **分析ツールとの簡単連携**：データが S3 Tables に入ると、Amazon Athena（SQL）、Amazon EMR、Redshift Spectrum、Presto、Trino、Snowflake などでクエリ可能です。
- **柔軟かつコスト効率の高いストレージ**：Amazon S3 は高耐久かつ低コストのオブジェクトストレージを提供し、アーカイブ、コンプライアンス、時系列分析に最適です。

## はじめる前に

このセクションでは EMQX で Amazon S3 Tables Sink を作成するための準備について説明します。

### 前提条件

作業を進める前に、以下の内容を理解していることを推奨します：
>>>>>>> origin/release-5.10

#### EMQXの概念：

<<<<<<< HEAD
- [ルールエンジン](./rules.md)：MQTTメッセージからデータを抽出・変換するロジックの定義方法を理解する。
- [データ統合](./data-bridges.md)：EMQXのコネクターとSinkの概念を理解する。
=======
- [ルールエンジン](./rules.md)：MQTT メッセージからデータを抽出・変換するロジックの定義方法を理解します。
- [データ統合](./data-bridges.md)：EMQX のコネクターおよび Sink の概念を理解します。
>>>>>>> origin/release-5.10

#### AWSの概念：

<<<<<<< HEAD
AWS S3 Tablesが初めての場合は以下の用語を確認してください：

- **EC2**：AWSの仮想マシンサービス（コンピュートインスタンス）。
- **IAM**：AWS Identity and Access Management。インスタンスロールはそのインスタンス上で動作するプログラムに一時的な認証情報を発行可能。
- **IMDSv2**：EC2のインスタンスメタデータサービスv2。トークンベースでより安全にメタデータや一時認証情報を取得。
- **Table Bucket**：S3 TablesでIcebergベースのテーブルデータとメタデータを保存する専用のS3バケット。
- **Amazon Athena**：Amazon S3に保存されたデータに対して直接SQLクエリを実行できるサーバーレスクエリエンジン。DDLステートメント（`CREATE TABLE`など）を使ってスキーマや構造を定義可能。
- **Catalog**：Athenaのメタデータコンテナで、データベース（ネームスペース）やテーブルを管理。
- **Database (Namespace)**：Catalog内の論理的なテーブルグループ。
- **Iceberg Table**：高性能でトランザクショナルなデータレイク向けテーブルフォーマット。スキーマ進化、パーティションプルーニング、タイムトラベルクエリをサポート。
=======
AWS S3 Tables に不慣れな場合は、以下の主要用語を確認してください：

- **EC2**：AWS の仮想マシンサービス（コンピュートインスタンス）。
- **IAM**：AWS Identity and Access Management。インスタンスロールはそのインスタンス上のプログラムに一時的な認証情報を発行できます。
- **IMDSv2**：EC2 のインスタンスメタデータサービス v2。トークンベースでより安全にメタデータや一時認証情報を取得します。
- **Table Bucket**：S3 Tables で Iceberg ベースのテーブルデータとメタデータを格納するための専用 S3 バケット。
- **Amazon Athena**：Amazon S3 に保存されたデータに対して直接 SQL クエリを実行できるサーバーレスクエリエンジン。`CREATE TABLE` などの DDL 文もサポートし、スキーマや構造の定義が可能です。
- **Catalog**：Athena のメタデータコンテナで、データベース（ネームスペース）やテーブルを整理します。
- **Database (Namespace)**：Catalog 配下の論理的なテーブルグループ。
- **Iceberg Table**：高性能でトランザクション対応のデータレイク用テーブルフォーマット。スキーマ進化、パーティションプルーニング、タイムトラベルクエリをサポートします。

### デプロイ前提条件と認証情報の取得方法

S3 Tables コネクターは認証情報の取得方法を2通りサポートしています。EMQX のデプロイ環境に応じて選択してください：

- **オプション1：アクセスキーを手動設定**
  [コネクター作成](#create-a-connector)時に **Access Key ID** と **Secret Access Key** を指定します。これらの認証情報は対象の S3 Tables と Athena に必要な権限を持つ必要があります。ローカル環境、コンテナ、Kubernetes、非 AWS クラウド、またはインスタンスロールが付与されていない EC2 に適します。

  IAM ユーザーのアクセスキーの作成・管理は [AWS ドキュメント（アクセスキーの管理）](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html) を参照してください。

- **オプション2：一時認証情報を自動取得（EC2 のみ）**
  EMQX が AWS EC2 インスタンス上で動作し、そのインスタンスに必要な権限を持つ IAM ロールが付与されている場合、コネクターの **Access Key ID** と **Secret Access Key** を空欄にできます。EMQX は IMDSv2 API を使ってそのロールに紐づく一時認証情報を取得します。

  EC2 インスタンスに IAM ロールを割り当てる方法は [AWS ドキュメント（Amazon EC2 の IAM ロール）](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2.html) を参照してください。

::: tip 注意事項

- インスタンスロールに対象の S3 Tables（バケット／テーブル）および Athena への十分な権限があることを確認してください。そうでないと **Test Connectivity** が失敗します。
- 一時認証情報の管理には EC2 インスタンスに付与された IAM ロールの使用を推奨します。EC2 以外の環境やロールが付与されていない場合は、**オプション1** でアクセスキーを手動入力してください。

:::
>>>>>>> origin/release-5.10

### デプロイ前提条件と認証情報の取得方法

<<<<<<< HEAD
S3 Tablesコネクターは認証情報を取得する方法が2通りあります。EMQXのデプロイ環境に応じて選択してください：

- **オプション1：アクセスキーを手動設定**
  コネクター作成時に**Access Key ID**と**Secret Access Key**を入力します。これらの認証情報は対象のS3 TablesおよびAthenaへの必要な権限を持つ必要があります。ローカル環境、コンテナ、Kubernetes、非AWSクラウド、またはインスタンスロールが割り当てられていないEC2で適用可能です。
=======
EMQX で Sink を作成する前に、AWS S3 Tables の MQTT データ格納先を準備します。以下が必要です：

- 実際のデータファイルを格納する Table Bucket
- 関連テーブルを論理的にグループ化する Namespace
- 構造化された MQTT データを受け取る Iceberg ベースの Table
>>>>>>> origin/release-5.10

  IAMユーザーのアクセスキーの作成・管理については[AWSのアクセスキー管理ドキュメント](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html)を参照してください。

<<<<<<< HEAD
- **オプション2：一時認証情報を自動取得（EC2のみ）**
  EMQXがAWS EC2インスタンス上で動作し、かつそのインスタンスに必要な権限を持つIAMロールが割り当てられている場合、コネクター作成時に**Access Key ID**と**Secret Access Key**を空欄にできます。EMQXはIMDSv2 APIを使い、そのロールに紐づく一時認証情報を取得します。

  EC2インスタンスにIAMロールを割り当てる方法は[AWSのEC2用IAMロールドキュメント](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2.html)を参照してください。

::: tip 注意事項

- インスタンスロールには対象のS3 Tables（バケット／テーブル）およびAthenaへの十分な権限が必要です。権限不足の場合、**Test Connectivity**が失敗します。
- 一時認証情報の管理にはEC2インスタンスに割り当てたIAMロールの利用を推奨します。EC2以外の環境やロール未割り当ての場合は**オプション1**でアクセスキーを手動設定してください。

:::

### S3 Tablesバケットの準備

EMQXでSinkを作成する前に、AWS S3 TablesのMQTTデータ受け入れ先を準備する必要があります。以下を用意してください：

- 実際のデータファイルを格納するTable Bucket
- 関連テーブルを論理的にグループ化するNamespace
- 構造化されたMQTTデータを受け取るIcebergベースのテーブル

1. AWSマネジメントコンソールにログインします。

2. S3サービスに移動し、左のナビゲーションペインで**Table buckets**をクリックします。

3. **Create table bucket**をクリックし、テーブルバケット名（例：`mybucket`）を入力して**Create table bucket**をクリックします。

4. バケット作成後、クリックしてテーブル一覧に移動します。

5. **Create table with Athena**をクリックすると、Namespaceの入力を求めるポップアップが表示されます。

6. **Create a namespace**を選択し、Namespace名を入力して作成を確定します。

7. Namespace作成後、再度**Create table with Athena**をクリックします。

8. Icebergテーブルのスキーマを定義します：

   - **Query table with Athena**をクリックし、**Query editor**で以下を設定します：

     - **Catalog**セレクターから、作成したバケット名に対応するCatalog（例：`s3tablescatalog/mybucket`）を選択。
     - **Database**セレクターから先ほど作成したNamespaceを選択。

   - 以下のDDLを実行し、テーブルタイプが`ICEBERG`であることを確認してテーブルを作成します。例：
=======
1. S3 サービスに移動し、左ナビゲーションペインで **Table buckets** をクリックします。

1. **Create table bucket** をクリックし、テーブルバケット名（例：`mybucket`）を入力して **Create table bucket** をクリックします。

1. バケット作成後、バケット名をクリックしてテーブル一覧に移動します。

1. **Create table with Athena** をクリックすると、Namespace の入力を求めるポップアップが表示されます。

1. **Create a namespace** を選択し、ネームスペース名を入力して作成を確定します。

1. ネームスペース作成後、再度 **Create table with Athena** をクリックします。

1. Iceberg テーブルのスキーマを定義します：

   - **Query table with Athena** をクリックし、**Query editor** で：

     - **Catalog** セレクターからバケット名に対応する Catalog（例：`s3tablescatalog/mybucket`）を選択。
     - **Database** セレクターから先ほど作成したネームスペースを選択。

   - 以下の DDL を実行し、テーブルタイプが `ICEBERG` であることを指定します。例：
>>>>>>> origin/release-5.10

     ```sql
     CREATE TABLE testtable (
       c_str string,
       c_long int )
     TBLPROPERTIES ('table_type' = 'ICEBERG');
     ```

<<<<<<< HEAD
     これはEMQXからの構造化MQTTデータを格納するIcebergテーブルを定義します。

9. テーブルが正常に作成されて空であることを確認するため、以下のクエリを実行します：
=======
     これは EMQX からの構造化 MQTT データを格納する Iceberg ベースのテーブルを定義します。

9. テーブルが正常に作成され、空であることを確認するために以下を実行します：
>>>>>>> origin/release-5.10

   ```sql
   select * from testtable
   ```

   ::: tip

<<<<<<< HEAD
   AthenaでSQLを実行する前に、正しいCatalogおよびDatabase（Namespace）が選択されていることを必ず確認してください。これにより、意図したS3 Table Bucket内にテーブルが作成されます。
=======
   Athena で SQL を実行する前に、正しい Catalog と Database（ネームスペース）が選択されていることを必ず確認してください。これによりテーブルが意図した S3 テーブルバケットに作成されます。
>>>>>>> origin/release-5.10

   :::

## コネクターの作成

S3 Tables Sinkを追加する前に、対応するコネクターを作成します。

<<<<<<< HEAD
1. ダッシュボードの**Integration** -> **Connector**ページに移動します。

2. 右上の**Create**ボタンをクリックします。

3. コネクタータイプとして**S3 Tables**を選択し、次へ進みます。

4. コネクター名を入力します。名前は英数字で始まり、英数字、ハイフン、アンダースコアを含めることができます。この例では`my-s3-tables`と入力します。

5. 必要な接続情報を入力します：

   - **S3Tables ARN**：S3 Table BucketのAmazonリソースネーム(ARN)を入力します。AWSコンソールのTable bucketsセクションで確認可能です。
   - **Access Key ID と Secret Access Key**（任意）：
     - **手動設定の場合**：S3 TablesとAthenaにアクセス権限を持つIAMユーザーまたはロールの認証情報を入力します。
     - **自動取得の場合**：EMQXがAWS EC2インスタンス上で動作し、必要な権限を持つIAMロールが割り当てられている場合は空欄のままにします。EMQXがIMDSv2経由で一時認証情報を取得します。詳細は[デプロイ前提条件と認証情報の取得方法](#デプロイ前提条件と認証情報の取得方法)を参照してください。
   - **Enable TLS**：S3 Tables接続時はTLSがデフォルトで有効です。TLS接続オプションの詳細は[TLSによる外部リソースアクセス](../network/overview.md#enable-tls-encryption-for-accessing-external-resources)を参照してください。
   - **Health Check Timeout**：コネクターがS3 Tablesとの接続状態を自動でヘルスチェックする際のタイムアウト時間を指定します。

7. その他の設定はデフォルト値を使用します。

8. **Create**をクリックする前に、**Test Connectivity**を押してS3 Tablesサービスへの接続確認を行えます。

9. 最後に**Create**ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、次にルールとSinkを作成してS3 Tablesへのデータ書き込みを指定します。
=======
1. ダッシュボードの **Integration** -> **Connector** ページに移動します。
2. 右上の **Create** ボタンをクリックします。
3. コネクタータイプとして **S3 Tables** を選択し、次へ進みます。
4. コネクター名を入力します。名前は英数字で始まり、英数字、ハイフン、アンダースコアを含めることができます。例として `my-s3-tables` と入力します。
5. 必要な接続情報を入力します：
   - **S3Tables ARN**：S3 テーブルバケットの Amazon リソースネーム（ARN）を入力します。AWS コンソールの Table buckets セクションで確認可能です。
   - **Access Key ID と Secret Access Key**（任意）：
     - **手動設定の場合**：S3 Tables と Athena へのアクセス権限を持つ IAM ユーザーまたはロールに紐づく AWS 認証情報を入力します。
     - **自動取得の場合**：EMQX が AWS EC2 インスタンス上で動作し、必要な権限を持つ IAM ロールが割り当てられている場合は空欄にできます。EMQX は IMDSv2 を通じて一時認証情報を自動取得します。詳細は [デプロイ前提条件と認証情報の取得方法](#デプロイ前提条件と認証情報の取得方法) を参照してください。
   - **Enable TLS**：S3 Tables への接続時は TLS がデフォルトで有効です。TLS 接続オプションの詳細は [TLS for External Resource Access](../network/overview.md#enable-tls-encryption-for-accessing-external-resources) を参照してください。
   - **Health Check Timeout**：コネクターが S3 Tables との接続に対して自動ヘルスチェックを行う際のタイムアウト時間を指定します。
7. その他の設定はデフォルト値を使用します。
8. **Create** をクリックする前に、**Test Connectivity** を押してコネクターが S3 Tables サービスに接続可能かテストできます。
9. 最後に **Create** ボタンをクリックし、コネクターの作成を完了します。

これでコネクターの作成が完了しました。次に、S3 Tables サービスに書き込むデータを指定するルールと Sink を作成します。
>>>>>>> origin/release-5.10

## Amazon S3 Tables Sinkを使ったルールの作成

このセクションでは、ソースMQTTトピック`t/#`からメッセージを処理し、処理結果をS3 Tablesの`mybucket`バケットに書き込むルール作成手順を示します。

1. ダッシュボードの**Integration** -> **Rules**ページに移動します。

2. 右上の**Create**ボタンをクリックします。

<<<<<<< HEAD
3. ルールIDに`my_rule`を入力し、SQLエディターに以下のルールSQLを入力します：
=======
3. ルール ID に `my_rule` を入力し、SQL エディターに以下のルール SQL を入力します：
>>>>>>> origin/release-5.10

   ```sql
   SELECT
     payload.str as c_str,
     payload.int as c_long
   FROM
       "t/#"
   ```

   ::: tip

<<<<<<< HEAD
   SQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックしてルールSQLの学習や結果のテストが可能です。
=======
   SQL に不慣れな場合は、**SQL Examples** をクリックし、**Enable Debug** を有効にしてルール SQL の結果を学習・テストできます。
>>>>>>> origin/release-5.10

   :::

   ::: tip

   出力フィールドがIcebergテーブルのスキーマと一致していることを必ず確認してください。必須カラムの欠落や誤った名前はデータのテーブルへの追加失敗を招きます。

   :::

<<<<<<< HEAD
4. アクションを追加し、**Action Type**ドロップダウンから`S3 Tables`を選択します。アクションドロップダウンはデフォルトの`create action`のままにするか、既存のS3 Tablesアクションを選択します。ここでは新規Sinkを作成してルールに追加します。

5. Sink名と任意の説明を入力します。

6. **Connector**ドロップダウンから先ほど作成した`my-s3-tables`コネクターを選択します。新規コネクターを素早く定義したい場合はドロップダウン横の**Create**ボタンをクリックしてください。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

7. Sink設定を行います：

   - **Namespace**：テーブルが存在するNamespace。複数セグメントの場合はドット区切りで指定（例：`my.name.space`）。
   - **Table**：データを追加するIcebergテーブル名（例：`testtable`）。
   - **Max Records**：S3に書き込む前にバッチ処理する最大レコード数。到達すると即座にバッチをフラッシュしてアップロード。
   - **Time Interval**：Max Recordsに達しなくても、指定した時間（ミリ秒）経過後にバッチをフラッシュ。
   - **Data File Format**：S3に保存するバッチデータのファイル形式。サポート値：
     - `avro`：（デフォルト）行ベースのAvro形式。ストリーミングデータやスキーマ進化に適する。
     - `parquet`：列指向のApache Parquet形式。大規模データの分析クエリに最適。

8. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

9. **Advanced Settings**を展開し、必要に応じて詳細設定を行います（任意）。詳細は[高度な設定](#高度な設定)を参照してください。

10. 残りの設定はデフォルト値を使用し、**Create**ボタンをクリックしてSink作成を完了します。作成成功後、ルール作成画面に戻り、新規Sinkがルールアクションに追加されます。

11. ルール作成画面で**Create**ボタンをクリックし、ルール全体の作成を完了します。

これでルールの作成が完了しました。**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブで新しいS3 Tables Sinkを確認できます。

また、**Integration** -> **Flow Designer**を開くとトポロジーが視覚的に表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されてS3 Tablesに書き込まれる流れが確認できます。

## ルールのテスト

このセクションでは、S3 Tables Sinkを設定したルールのテスト方法を示します。

1. MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージをパブリッシュします：
=======
4. アクションを追加し、**Action Type** ドロップダウンから `S3 Tables` を選択します。アクションのドロップダウンはデフォルトの `create action` のままにするか、既存の S3 Tables アクションを選択します。ここでは新しい Sink を作成してルールに追加します。

5. Sink 名と任意の説明を入力します。

6. **Connector** ドロップダウンから先ほど作成した `my-s3-tables` コネクターを選択します。新しいコネクターを素早く定義したい場合は、ドロップダウン横の **Create** ボタンをクリックしてください。必要な設定パラメータは [コネクターの作成](#コネクターの作成) を参照してください。

7. Sink の設定を行います：

   - **Namespace**：テーブルが存在するネームスペース。複数セグメントの場合はドット区切りで指定（例：`my.name.space`）。
   - **Table**：データを追加する Iceberg テーブル名（例：`testtable`）。
   - **Max Records**：S3 へ書き込む前にバッチ処理する最大レコード数。この数に達すると即座にバッチをフラッシュしてアップロードします。
   - **Time Interval**：Max Records に達していなくても、指定ミリ秒経過でバッチをフラッシュします。
   - **Data File Format**：S3 にバッチ化した MQTT メッセージを保存するデータファイルのフォーマット。サポート値：
     - `avro`：（デフォルト）レコードを Avro フォーマットで保存。行ベースでストリーミングデータやスキーマ進化に適します。
     - `parquet`：Apache Parquet フォーマットで保存。列ベースで大規模データの分析クエリに最適化されています。

8. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

9. **Advanced Settings** を展開し、必要に応じて詳細設定を行います（任意）。詳細は [Advanced Settings](#advanced-settings) を参照してください。

10. その他の設定はデフォルト値のままにし、**Create** ボタンをクリックして Sink 作成を完了します。作成成功後はルール作成画面に戻り、新しい Sink がルールアクションに追加されます。

11. ルール作成画面で **Create** ボタンをクリックし、ルール全体の作成を完了します。

これでルールが正常に作成されました。**Rules** ページで新規ルールを確認でき、**Actions (Sink)** タブで新しい S3 Tables Sink を確認できます。

また、**Integration** -> **Flow Designer** でトポロジーを表示可能です。トポロジーはトピック `t/#` のメッセージがルール `my_rule` によって解析され、S3 Tables に書き込まれる流れを視覚的に示します。

## ルールのテスト

このセクションでは、S3 Tables Sink を設定したルールのテスト方法を示します。

1. MQTTX を使い、トピック `t/1` にメッセージをパブリッシュします：
>>>>>>> origin/release-5.10

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "str": "hello S3 Tables", "int": 123 }'
   ```

<<<<<<< HEAD
   このメッセージは`payload.str`と`payload.int`フィールドを含み、ルールSQLおよびテーブルスキーマと一致しています。

2. **Rules**ページでルールのメトリクスとSinkの状態を監視します。新規の受信メッセージと送信メッセージがそれぞれ1件ずつ増えているはずです。

3. Athenaのクエリエディターを開き、正しい**Catalog**（例：`s3tablescatalog/mybucket`）と**Database**（Namespace）が選択されていることを確認します。

4. 以下のSQLを実行します：
=======
   このメッセージは `payload.str` と `payload.int` フィールドを含み、ルール SQL とテーブルスキーマに一致しています。

2. **Rules** ページでルールのメトリクスや Sink の状態を監視します。新規の受信メッセージと送信メッセージがそれぞれ1件ずつ増えているはずです。

3. Athena クエリエディターを開き、正しい **Catalog**（例：`s3tablescatalog/mybucket`）と **Database**（ネームスペース）が選択されていることを確認します。

4. 以下の SQL クエリを実行します：
>>>>>>> origin/release-5.10

   ```sql
   SELECT * FROM testtable
   ```

   以下のような行が表示されるはずです：

   | c_str           | c_long |
   | --------------- | ------ |
   | hello S3 Tables | 123    |

<<<<<<< HEAD
## 高度な設定

このセクションでは、S3 Tables Sinkの高度な設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開すると以下のパラメータを調整可能です。

| フィールド名                      | 説明                                                                                   | デフォルト値     |
| -------------------------------- | -------------------------------------------------------------------------------------- | --------------- |
| **Min Part Size**                | マルチパートアップロードの最小パートサイズ。<br/>このサイズに達するまでアップロードデータはメモリに蓄積されます。 | `5` MB          |
| **Max Part Size**                | マルチパートアップロードの最大パートサイズ。<br/>このサイズを超えるパートはアップロードされません。 | `5` GB          |
| **Buffer Pool Size**             | バッファワーカーの数を指定します。EMQXとS3 Tables間のデータフローを管理し、一時的にデータを保持・処理します。パフォーマンス最適化とスムーズなデータ送信に重要です。 | `16`            |
| **Request TTL**                  | リクエストの有効期限（秒）を指定します。リクエストがバッファに入ってからこの時間を超えるか、S3 Tablesから応答・アックが得られない場合、リクエストは期限切れとみなされます。 | `45` 秒         |
| **Health Check Interval**        | SinkがS3 Tablesとの接続状態を自動ヘルスチェックする間隔（秒）を指定します。 | `15` 秒         |
| **Health Check Interval Jitter** | 複数ノードが同時にヘルスチェックを行うのを防ぐため、基本間隔に加える一様ランダム遅延（ミリ秒）です。複数のアクションやソースが同じコネクターを共有する場合に有効です。 | `0` ミリ秒      |
| **Health Check Timeout**         | コネクターがS3 Tablesとの接続ヘルスチェックを行う際のタイムアウト時間を指定します。 | `60` 秒         |
| **Max Buffer Queue Size**        | S3 Tables Sinkの各バッファワーカーが一時的に保持できる最大バイト数を指定します。データ送信の効率化のためにバッファワーカーがデータを中継します。システム性能やデータ送信要件に応じて調整してください。 | `256` MB        |
| **Batch Size**                   | EMQXからS3 Tablesへ一度に送信するデータバッチの最大レコード数を指定します。サイズ調整によりデータ転送の効率とパフォーマンスを最適化可能です。`1`に設定するとレコードを個別送信します。 | 1000            |
| **Query Mode**                   | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードではS3 Tablesへの書き込みがMQTTパブリッシュをブロックしませんが、クライアントがS3 Tables到達前にメッセージを受信する可能性があります。 | `Asynchronous`  |
| **In-flight  Window**            | 送信済みだが応答・アック未受信のリクエスト数の最大値を指定します。<br/>`asynchronous`モード時に重要で、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`           |
=======
## Advanced Settings

このセクションでは、S3 Tables Sink の詳細設定オプションについて説明します。ダッシュボードの Sink 設定画面で **Advanced Settings** を展開し、用途に応じて以下のパラメータを調整できます。

| フィールド名                       | 説明                                                                                                              | デフォルト値     |
| ---------------------------------- | ----------------------------------------------------------------------------------------------------------------- | --------------- |
| **Min Part Size**                  | マルチパートアップロードの最小パートサイズ。<br/>このサイズに達するまでデータはメモリ内に蓄積されます。                     | `5` MB          |
| **Max Part Size**                  | マルチパートアップロードの最大パートサイズ。<br/>このサイズを超えるパートはアップロードされません。                       | `5` GB          |
| **Buffer Pool Size**               | バッファワーカーのプロセス数を指定します。これらのワーカーは EMQX と S3 Tables 間のデータフローを管理し、一時的にデータを保持・処理します。パフォーマンス最適化とスムーズなデータ送信に重要です。 | `16`            |
| **Request TTL**                   | リクエスト TTL（Time To Live）設定は、リクエストがバッファに入ってから有効とみなされる最大秒数を指定します。<br/>TTL を超えたリクエストや、送信後に S3 Tables からの応答やアックがタイムリーに得られない場合、そのリクエストは期限切れとみなされます。 | `45` 秒         |
| **Health Check Interval**          | Sink が S3 Tables との接続に対して自動ヘルスチェックを行う間隔（秒）を指定します。                                         | `15` 秒         |
| **Health Check Interval Jitter**   | 基本のヘルスチェック間隔に加算される一様ランダム遅延（ミリ秒）です。複数ノードが同時にヘルスチェックを開始する可能性を減らします。複数のアクションやソースが同じコネクターを共有する場合に有効です。 | `0` ミリ秒      |
| **Health Check Timeout**           | コネクターが S3 Tables との接続に対して自動ヘルスチェックを行う際のタイムアウト時間を指定します。                             | `60` 秒         |
| **Max Buffer Queue Size**          | S3 Tables Sink の各バッファワーカープロセスがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時的に保持し、S3 Tables への送信を効率化します。システム性能やデータ送信要件に応じて調整してください。 | `256` MB        |
| **Batch Size**                    | EMQX から S3 Tables へ一度に送信するデータバッチの最大レコード数を指定します。サイズを調整することでデータ転送の効率と性能を最適化できます。`1` に設定するとレコードをバッチ化せず個別に送信します。 | 1000            |
| **Query Mode**                   | `synchronous` または `asynchronous` のリクエストモードを選択し、メッセージ送信を最適化します。<br/>非同期モードでは S3 Tables への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを受信してから S3 Tables に到達するまでの遅延が発生する可能性があります。 | `Asynchronous`  |
| **In-flight Window**             | 「インフライトキューリクエスト」とは開始済みでまだ応答やアックを受け取っていないリクエストのことです。Sink と S3 Tables 間の通信で同時に存在可能なインフライトリクエストの最大数を制御します。<br/>**Request Mode** が `asynchronous` の場合、この設定は特に重要です。同一 MQTT クライアントからのメッセージを厳密に順序処理する必要がある場合は、この値を `1` に設定してください。 | `100`           |
>>>>>>> origin/release-5.10
