# Amazon S3 Tables への MQTT データ取り込み

[Amazon S3 Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) は、分析ワークロードに最適化された専用のストレージソリューションです。Apache Iceberg フォーマットで IoT センサーの読み取り値などの表形式データを高性能かつスケーラブルかつ安全に保存できます。

EMQX は Amazon S3 Tables とのシームレスな統合をサポートし、MQTT メッセージを効率的に S3 テーブルバケットに保存できるようになりました。この統合により、柔軟かつスケーラブルな IoT データストレージが可能となり、Amazon Athena、Amazon Redshift、Amazon EMR などの AWS サービスを用いた高度な分析や処理が容易になります。

本ページでは、EMQX と Amazon S3 Tables 間のデータ統合について詳細に解説し、ルールおよび Sink の作成方法を実践的に案内します。

## 動作概要

EMQX の Amazon S3 Tables 統合は標準機能として提供されています。この統合は EMQX のルールエンジンと S3 Tables Sink を活用し、MQTT メッセージを変換して Apache Iceberg 形式のテーブルに直接ストリームし、S3 テーブルバケットに保存します。これにより長期保存および下流分析が可能になります。

典型的な IoT シナリオでは：

- **EMQX** は MQTT ブローカーとして動作し、デバイスの接続管理、メッセージルーティング、データ処理を行います。
- **Amazon S3 Tables** は MQTT メッセージデータを表形式で耐久的かつクエリ可能なストレージとして提供します。
- **Amazon Athena** は Iceberg テーブルを定義し、保存されたデータに対して SQL クエリを実行します。

![emqx-integration-s3-tables](./assets/emqx-integration-s3-tables.png)

ワークフローは以下の通りです：

1. **デバイスが EMQX に接続**：IoT デバイスは MQTT 経由で EMQX に接続し、テレメトリデータをパブリッシュし始めます。
2. **メッセージルーティングとルールマッチング**：EMQX は組み込みのルールエンジンを使い、受信した MQTT メッセージを定義済みのトピックにマッチさせ、特定のフィールドや値を抽出します。
3. **データ変換**：EMQX のルールでメッセージペイロードをフィルタリング、変換、または拡張し、ターゲットの Iceberg テーブルのスキーマに合わせます。
4. **Amazon S3 Tables への書き込み**：ルールが S3 Tables Sink アクションをトリガーし、変換済みデータをバッチ処理して Iceberg 互換の書き込み API を使い Amazon S3 Tables に送信します。データは Iceberg テーブルのパーティション下に Parquet ファイルとして永続化されます。
5. **クエリと分析**：取り込まれたデータは Amazon Athena でクエリ可能となり、他のデータセットと結合したり、Redshift Spectrum、Amazon EMR、Presto、Trino などのサードパーティ分析エンジンで分析できます。

## 特長と利点

EMQX で Amazon S3 Tables データ統合を利用することで、以下の特長とメリットが得られます：

- **リアルタイムストリーム処理**：EMQX のルールエンジンにより、MQTT メッセージをリアルタイムに抽出、変換、条件付きルーティングし、S3 Tables に配信できます。
- **Iceberg ベースの S3 ストレージ**：メッセージは Apache Iceberg テーブルに書き込まれ、従来型データベース不要で SQL ライクなアクセスが可能です。
- **分析ツールとの簡単統合**：データが S3 Tables に入ると、Amazon Athena（SQL）、Amazon EMR、Redshift Spectrum、Presto、Trino、Snowflake などでクエリや分析が可能です。
- **柔軟かつコスト効率の良いストレージ**：Amazon S3 は高耐久かつ低コストのオブジェクトストレージを提供し、アーカイブ、コンプライアンス、時系列分析に最適です。

## はじめる前に

このセクションでは、EMQX で Amazon S3 Tables Sink を作成するための準備について説明します。

### 前提条件

進める前に、以下の内容に慣れていることを確認してください。

#### EMQX の概念：

- [ルールエンジン](./rules.md)：MQTT メッセージからデータを抽出・変換するロジックを定義する方法を理解してください。
- [データ統合](./data-bridges.md)：EMQX のコネクターとシンクの概念を理解してください。

#### AWS の概念：

AWS S3 Tables が初めての場合は、以下の主要用語を確認してください：

- **EC2**：AWS の仮想マシンサービス（コンピュートインスタンス）。
- **IAM**：AWS Identity and Access Management。インスタンスロールはそのインスタンス上で動作するプログラムに一時的な認証情報を発行できます。
- **IMDSv2**：EC2 のインスタンスメタデータサービス v2。トークンベースでより安全にメタデータや一時認証情報を取得します。
- **Table Bucket**：S3 Tables で Iceberg ベースのテーブルデータとメタデータを保存するための特殊な S3 バケット。
- **Amazon Athena**：Amazon S3 に保存されたデータに対して直接 SQL クエリを実行できるサーバーレスクエリエンジン。DDL ステートメント（例：`CREATE TABLE`）をサポートし、スキーマや構造を定義可能。
- **Catalog**：Athena のメタデータコンテナで、データベース（ネームスペース）やテーブルを管理。
- **Database (Namespace)**：Catalog 配下の論理的なテーブルグループ。
- **Iceberg Table**：データレイク向けの高性能かつトランザクショナルなテーブルフォーマット。スキーマ進化、パーティションプルーニング、タイムトラベルクエリをサポート。

### デプロイ前提条件と認証情報の取得方法

S3 Tables コネクターは認証情報の取得方法を2通りサポートしています。EMQX のデプロイ環境に応じて選択してください。

- **オプション1：アクセスキーを手動設定**
  [コネクター作成](#create-a-connector)時に **Access Key ID** と **Secret Access Key** を入力します。これらの認証情報は対象の S3 Tables と Athena に必要な権限を持っている必要があります。ローカル、コンテナ、Kubernetes、非 AWS クラウド、またはインスタンスロールが割り当てられていない EC2 に適しています。

  IAM ユーザーのアクセスキー作成・管理方法は [AWS ドキュメント（アクセスキーの管理）](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html) を参照してください。

- **オプション2：一時認証情報を自動取得（EC2 のみ）**
  EMQX が AWS EC2 インスタンス上で動作し、必要な権限を持つ IAM ロールがインスタンスに割り当てられている場合、コネクターの **Access Key ID** と **Secret Access Key** を空欄にできます。EMQX は IMDSv2 API を使ってそのロールに紐づく一時認証情報を取得します。

  EC2 インスタンスに IAM ロールを割り当てる方法は [AWS ドキュメント（EC2 の IAM ロール）](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2.html) を参照してください。

::: tip 注意事項

- インスタンスロールは対象の S3 Tables（バケット／テーブル）および Athena への十分な権限を持っている必要があります。そうでない場合、**Test Connectivity** が失敗する可能性があります。
- 一時認証情報の管理には EC2 インスタンスに割り当てた IAM ロールの利用を推奨します。EC2 以外の環境やロールが割り当てられていない場合は、**オプション1** の手動設定を利用してください。

:::

### S3 Tables バケットの準備

EMQX で Sink を作成する前に、AWS S3 Tables で MQTT データの送信先を準備します。以下が必要です：

- 実際のデータファイルを格納する Table Bucket
- 関連テーブルを論理的にグループ化する Namespace
- 構造化された MQTT データを受け取る Iceberg ベースの Table

1. AWS マネジメントコンソールにログインします。

1. S3 サービスに移動し、左のナビゲーションペインで **Table buckets** をクリックします。

1. **Create table bucket** をクリックし、テーブルバケット名（例：`mybucket`）を入力して **Create table bucket** をクリックします。

1. バケット作成後、クリックしてテーブル一覧に移動します。

1. **Create table with Athena** をクリックします。ポップアップで Namespace の指定を求められます。

1. **Create a namespace** を選択し、ネームスペース名を入力して作成を確定します。

1. ネームスペース作成後、再度 **Create table with Athena** をクリックします。

1. Iceberg テーブルのスキーマを定義します：

   - **Query table with Athena** をクリックし、**Query editor** で：

     - **Catalog** セレクターからバケット名に対応するカタログ（例：`s3tablescatalog/mybucket`）を選択。
     - **Database** セレクターから先ほど作成したネームスペースを選択。

   - 以下の DDL を実行してテーブルを作成し、テーブルタイプが `ICEBERG` であることを確認します。例：

     ```sql
     CREATE TABLE testtable (
       c_str string,
       c_long int )
     TBLPROPERTIES ('table_type' = 'ICEBERG');
     ```

     これは EMQX からの構造化された MQTT データを格納する Iceberg ベースのテーブルを定義します。

9. テーブルの検証として、以下のクエリを実行し、テーブルが空で正常に作成されていることを確認します：

   ```sql
   select * from testtable
   ```

   ::: tip

   Athena で SQL を実行する前に、正しい Catalog と Database（ネームスペース）が選択されていることを必ず確認してください。これにより、テーブルが意図した S3 テーブルバケットに作成されます。

   :::

## コネクターの作成

S3 Tables Sink を追加する前に、対応するコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。
2. 右上の **Create** ボタンをクリックします。
3. コネクタータイプとして **S3 Tables** を選択し、次へ進みます。
4. コネクター名を入力します。名前は英数字で始まり、英数字、ハイフン、アンダースコアを含めることができます。例として `my-s3-tables` と入力します。
5. 必須の接続情報を入力します：
   - **S3Tables ARN**：S3 テーブルバケットの Amazon リソースネーム（ARN）を入力します。AWS コンソールの Table buckets セクションで確認可能です。
   - **Access Key ID と Secret Access Key**（任意）：
     - **手動設定**：S3 Tables と Athena へのアクセス権限を持つ IAM ユーザーまたはロールの AWS 認証情報を入力します。
     - **自動取得**：EMQX が AWS EC2 インスタンス上で動作し、必要な権限を持つ IAM ロールが割り当てられている場合は空欄にできます。EMQX は IMDSv2 を通じて一時認証情報を自動取得します。詳細は [デプロイ前提条件と認証情報の取得方法](#デプロイ前提条件と認証情報の取得方法) を参照してください。
   - **Enable TLS**：S3 Tables への接続時は TLS がデフォルトで有効です。TLS 接続オプションの詳細は [TLS for External Resource Access](../../guides/network/overview.md#enable-tls-encryption-for-accessing-external-resources) を参照してください。
   - **Health Check Timeout**：コネクターが S3 Tables との接続状態を自動チェックする際のタイムアウト時間を指定します。
7. その他の設定はデフォルト値を使用します。
8. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが S3 Tables サービスに接続可能かテストできます。
9. 最後に **Create** ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、続いてルールと Sink を作成して S3 Tables への書き込みデータを指定します。

## Amazon S3 Tables Sink を使ったルールの作成

このセクションでは、EMQX でソース MQTT トピック `t/#` からメッセージを処理し、処理結果を S3 Tables の `mybucket` バケットに書き込むルールの作成方法を示します。

1. ダッシュボードの **Integration** -> **Rules** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. ルール ID に `my_rule` を入力し、SQL エディターに以下のルール SQL を入力します：

   ```sql
   SELECT
     payload.str as c_str,
     payload.int as c_long
   FROM
       "t/#"
   ```

   ::: tip

   SQL に不慣れな場合は、**SQL Examples** をクリックし、**Enable Debug** を有効にしてルール SQL の結果を学習・テストできます。

   :::

   ::: tip

   出力フィールドは Iceberg テーブルのスキーマと一致していることを確認してください。必須カラムが欠落または誤った名前の場合、データのテーブル追加に失敗する可能性があります。

   :::

4. アクションを追加し、**Action Type** ドロップダウンから `S3 Tables` を選択します。アクションドロップダウンはデフォルトの `create action` のままにするか、既存の S3 Tables アクションを選択します。ここでは新しい Sink を作成してルールに追加します。

5. Sink 名と任意で説明を入力します。

6. **Connector** ドロップダウンから先ほど作成した `my-s3-tables` コネクターを選択します。新しいコネクターを素早く定義したい場合は、ドロップダウン横の **Create** ボタンをクリックしてください。設定パラメータは [コネクターの作成](#コネクターの作成) を参照してください。

7. Sink 設定を構成します：

   - **Namespace**：テーブルが存在するネームスペース。複数セグメントの場合はドット区切りで指定（例：`my.name.space`）。
   - **Table**：データを追加する Iceberg テーブル名（例：`testtable`）。
   - **Max Records**：S3 へ書き込む前にバッチ処理する最大レコード数。到達すると即座にバッチをフラッシュしてアップロードします。
   - **Time Interval**：Max Records に達していなくても、指定ミリ秒経過後にデータをフラッシュする最大待機時間。
   - **Data File Format**：S3 に保存するバッチ化された MQTT メッセージのデータファイル形式。サポート値：
     - `avro`：（デフォルト）Avro 形式。行ベースでストリーミングデータやスキーマ進化に適しています。
     - `parquet`：Apache Parquet 形式。列ベースで大規模データの分析クエリに最適化されています。

8. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

9. **Advanced Settings** を展開し、必要に応じて詳細設定を行います（任意）。詳細は [詳細設定](#advanced-settings) を参照してください。

10. 残りの設定はデフォルト値を使用し、**Create** ボタンをクリックして Sink 作成を完了します。作成成功後はルール作成画面に戻り、新しい Sink がルールアクションに追加されます。

11. ルール作成画面で **Create** ボタンをクリックし、ルール全体の作成を完了します。

これでルールの作成が完了しました。**Rules** ページで新規作成したルールを確認でき、**Actions (Sink)** タブで新しい S3 Tables Sink も確認できます。

また、**Integration** -> **Flow Designer** を開くとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、S3 Tables に書き込まれる流れを視覚的に確認できます。

## ルールのテスト

このセクションでは、S3 Tables Sink を設定したルールのテスト方法を示します。

1. MQTTX を使ってトピック `t/1` にメッセージをパブリッシュします：

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "str": "hello S3 Tables", "int": 123 }'
   ```

   このメッセージは `payload.str` と `payload.int` フィールドを含み、ルール SQL とテーブルスキーマに一致しています。

2. **Rules** ページでルールのメトリクスと Sink のステータスを監視します。新規の着信メッセージと送信メッセージがそれぞれ1件ずつあるはずです。

3. Athena クエリエディターを開き、正しい **Catalog**（例：`s3tablescatalog/mybucket`）と **Database**（ネームスペース）が選択されていることを確認します。

4. 以下の SQL クエリを実行します：

   ```sql
   SELECT * FROM testtable
   ```

   以下のような行が表示されるはずです：

   | c_str           | c_long |
   | --------------- | ------ |
   | hello S3 Tables | 123    |

## 詳細設定

このセクションでは、S3 Tables Sink の詳細設定オプションについて説明します。ダッシュボードの Sink 設定画面で **Advanced Settings** を展開し、用途に応じて以下のパラメータを調整できます。

| フィールド名                      | 説明                                                                                           | デフォルト値     |
| -------------------------------- | ---------------------------------------------------------------------------------------------- | --------------- |
| **Min Part Size**                | マルチパートアップロードの最小パートサイズ。<br/>このサイズに達するまでアップロードデータはメモリに蓄積されます。 | `5` MB          |
| **Max Part Size**                | マルチパートアップロードの最大パートサイズ。<br/>このサイズを超えるパートはアップロードされません。 | `5` GB          |
| **Buffer Pool Size**             | EMQX と S3 Tables 間のデータフローを管理するバッファワーカープロセス数。<br/>これらのワーカーはデータを一時的に保持・処理し、性能最適化とスムーズなデータ送信を支えます。 | `16`            |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）。<br/>この時間を超えてバッファに滞留するか、送信後に S3 Tables からの応答やアックが得られない場合、リクエストは期限切れと判断されます。 | `45` 秒         |
| **Health Check Interval**        | Sink が S3 Tables との接続状態を自動チェックする間隔（秒）。                                      | `15` 秒         |
| **Health Check Interval Jitter** | 複数ノードが同時にヘルスチェックを開始しないよう、基本間隔に加える一様ランダム遅延。<br/>複数のアクションやソースが同一コネクターを共有する場合に有効です。 | `0` ミリ秒      |
| **Health Check Timeout**         | コネクターが S3 Tables との接続ヘルスチェックを行う際のタイムアウト時間。                         | `60` 秒         |
| **Max Buffer Queue Size**        | S3 Tables Sink の各バッファワーカーがバッファリング可能な最大バイト数。<br/>ワーカーはデータを一時保持し、効率的なデータストリーム処理を実現します。 | `256` MB        |
| **Batch Size**                   | EMQX から S3 Tables へ一度に転送するデータバッチの最大レコード数。<br/>サイズを調整することで転送効率と性能を最適化可能。`1` に設定するとバッチ化せず個別送信となります。 | 1000            |
| **Query Mode**                   | `synchronous` または `asynchronous` のリクエストモードを選択し、メッセージ送信を最適化。<br/>非同期モードでは S3 Tables への書き込みが MQTT パブリッシュ処理をブロックしませんが、クライアントがメッセージを受信してから S3 Tables に到達するまでにタイムラグが生じる可能性があります。 | `Asynchronous`  |
| **In-flight  Window**            | 送信済みだが応答やアックをまだ受け取っていない「インフライト」リクエストの最大数。<br/>`asynchronous` モードで特に重要で、同一 MQTT クライアントからのメッセージを厳密に順序処理したい場合は `1` に設定します。 | `100`           |
