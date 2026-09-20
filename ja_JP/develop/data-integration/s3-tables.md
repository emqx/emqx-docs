# Amazon S3 Tables への MQTT データ取り込み

[Amazon S3 Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) は、分析ワークロードに最適化された専用のストレージソリューションです。Apache Iceberg フォーマットで IoT センサーの読み取り値などの表形式データを高性能かつスケーラブルかつ安全に保存できます。

EMQX は Amazon S3 Tables とのシームレスな統合をサポートし、MQTT メッセージを効率的に S3 テーブルバケットに保存できます。この統合により、柔軟でスケーラブルな IoT データストレージが可能となり、Amazon Athena、Amazon Redshift、Amazon EMR などの AWS サービスを用いた高度な分析や処理が促進されます。

本ページでは、EMQX と Amazon S3 Tables のデータ統合について詳しく紹介し、ルールおよび Sink の作成方法を実践的に解説します。

## 動作概要

EMQX の Amazon S3 Tables 統合は標準機能として提供されています。この統合は EMQX のルールエンジンと S3 Tables Sink を活用し、MQTT メッセージを変換して Apache Iceberg フォーマットのテーブルに直接ストリーミングし、S3 テーブルバケットに保存します。これにより長期保存や下流の分析が可能になります。

典型的な IoT シナリオでは：

- **EMQX** は MQTT ブローカーとして機能し、デバイスの接続管理、メッセージルーティング、データ処理を行います。
- **Amazon S3 Tables** は MQTT メッセージデータの耐久性のあるクエリ可能な表形式ストレージの宛先となります。
- **Amazon Athena** は Iceberg テーブルの定義と保存データに対する SQL クエリの実行に使用されます。

![emqx-integration-s3-tables](./assets/emqx-integration-s3-tables.png)

ワークフローは以下の通りです：

1. **デバイスの EMQX への接続**：IoT デバイスが MQTT 経由で EMQX に接続し、テレメトリデータをパブリッシュし始めます。
2. **メッセージルーティングとルールマッチング**：EMQX は組み込みのルールエンジンを使い、受信した MQTT メッセージを定義済みトピックと照合し、特定のフィールドや値を抽出します。
3. **データ変換**：EMQX のルールでメッセージペイロードをフィルター、変換、または拡張し、ターゲットの Iceberg テーブルのスキーマに合わせます。
4. **Amazon S3 Tables への書き込み**：ルールが S3 Tables Sink アクションをトリガーし、変換済みデータをバッチ処理して Iceberg 互換の書き込み API を使い Amazon S3 Tables に送信します。データは Iceberg テーブルのパーティション下に Parquet ファイルとして永続化されます。
5. **クエリと分析**：取り込まれたデータは Amazon Athena でクエリ可能となり、他のデータセットと結合したり、Redshift Spectrum、Amazon EMR、Presto、Trino などのサードパーティ分析エンジンで分析できます。

## 特長と利点

EMQX で Amazon S3 Tables データ統合を利用することで、以下の特長とメリットが得られます：

- **リアルタイムストリーム処理**：EMQX のルールエンジンにより、MQTT メッセージをリアルタイムに抽出・変換・条件付きルーティングし、S3 Tables へ送信可能です。
- **Iceberg ベースの S3 ストレージ**：メッセージは Apache Iceberg テーブルに書き込まれ、従来のデータベースを不要にしつつ SQL ライクなアクセスパターンを実現します。
- **分析ツールとの簡単な統合**：データが S3 Tables に入ると、Amazon Athena（SQL）、Amazon EMR、Redshift Spectrum、Presto、Trino、Snowflake などでクエリや分析が可能です。
- **柔軟かつコスト効率の高いストレージ**：Amazon S3 は高耐久かつ低コストのオブジェクトストレージを提供し、アーカイブ、コンプライアンス、デバイス生成データの時系列分析に最適です。

## はじめる前に

このセクションでは、EMQX で Amazon S3 Tables Sink を作成するための準備について説明します。

### 前提条件

作業を進める前に、以下の内容を理解していることを推奨します。

#### EMQX の概念：

- [ルールエンジン](./rules.md)：MQTT メッセージからデータを抽出・変換するロジックを定義する方法。
- [データ統合](./data-bridges.md)：EMQX のコネクターとシンクの概念。

#### AWS の概念：

AWS S3 Tables に不慣れな場合は、以下の主要用語を確認してください：

- **EC2**：AWS の仮想マシンサービス（コンピュートインスタンス）。
- **IAM**：AWS Identity and Access Management。インスタンスロールはそのインスタンス上で動作するプログラムに一時的な認証情報を発行できます。
- **IMDSv2**：EC2 のインスタンスメタデータサービス v2。トークンベースでより安全にメタデータや一時認証情報を取得します。
- **Table Bucket**：S3 Tables で Iceberg ベースのテーブルデータとメタデータを保存するための特殊な S3 バケット。
- **Amazon Athena**：Amazon S3 に保存されたデータに対して直接 SQL クエリを実行できるサーバーレスクエリエンジン。DDL（`CREATE TABLE` など）をサポートし、スキーマ定義や構造設定が可能。
- **Catalog**：Athena のメタデータコンテナで、データベース（ネームスペース）やテーブルを整理します。
- **Database (Namespace)**：Catalog 配下のテーブルの論理的なグループ。
- **Iceberg Table**：データレイク向けの高性能かつトランザクショナルなテーブルフォーマット。スキーマ進化、パーティションプルーニング、タイムトラベルクエリをサポート。

### デプロイ前提条件と認証情報の取得方法

S3 Tables コネクターは認証情報の取得方法として以下の2通りをサポートしています。EMQX のデプロイ環境に応じて選択してください。

- **オプション1：アクセスキーを手動設定する場合**  
  [コネクター作成](#create-a-connector)時に **Access Key ID** と **Secret Access Key** を指定します。これらの認証情報は対象の S3 Tables と Athena に必要な権限を持つ IAM ユーザーまたはロールに紐づけられている必要があります。ローカル環境、コンテナ、Kubernetes、非 AWS クラウド、あるいはインスタンスロールが付与されていない EC2 での利用に適しています。  
  IAM ユーザーのアクセスキーの作成・管理については [AWS ドキュメント（アクセスキーの管理）](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html) を参照してください。

- **オプション2：一時認証情報を自動取得（EC2 のみ）**  
  EMQX が AWS EC2 インスタンス上で動作し、そのインスタンスに必要な権限を持つ IAM ロールが付与されている場合、コネクター設定で **Access Key ID** と **Secret Access Key** を空欄にできます。EMQX は IMDSv2 API を使ってそのロールに紐づく一時認証情報を自動取得します。  
  EC2 インスタンスに IAM ロールを割り当てる方法は [AWS ドキュメント（EC2 用 IAM ロール）](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2.html) をご覧ください。

::: tip 注意事項

- インスタンスロールに対象の S3 Tables（バケット・テーブル）および Athena への十分な権限が付与されていることを確認してください。権限不足の場合、**Test Connectivity** が失敗する可能性があります。
- 一時認証情報の管理には EC2 インスタンスに付与した IAM ロールの利用を推奨します。EC2 以外の環境やロールが付与されていない場合はオプション1でアクセスキーを手動入力してください。

:::

### S3 Tables バケットの準備

EMQX で Sink を作成する前に、AWS S3 Tables 側で MQTT データの保存先を準備します。以下が必要です：

- 実際のデータファイルを保存する Table Bucket
- 関連テーブルを論理的にグループ化する Namespace
- 構造化された MQTT データを受け取る Iceberg ベースの Table

1. AWS マネジメントコンソールにログインします。

2. S3 サービスに移動し、左側のナビゲーションペインで **Table buckets** をクリックします。

3. **Create table bucket** をクリックし、テーブルバケット名（例：`mybucket`）を入力して **Create table bucket** をクリックします。

4. バケット作成後、そのバケットをクリックして Tables 一覧に移動します。

5. **Create table with Athena** をクリックすると、Namespace の入力を求めるポップアップが表示されます。

6. **Create a namespace** を選択し、Namespace 名を入力して作成を確定します。

7. Namespace 作成後、再度 **Create table with Athena** をクリックします。

8. Iceberg テーブルのスキーマを定義します：

   - **Query table with Athena** をクリックし、**Query editor** で以下を設定します：

     - **Catalog** セレクターからバケット名に対応する Catalog（例：`s3tablescatalog/mybucket`）を選択
     - **Database** セレクターから先ほど作成した Namespace を選択

   - 以下の DDL を実行し、テーブルタイプが `ICEBERG` であることを指定してテーブルを作成します。例：

     ```sql
     CREATE TABLE testtable (
       c_str string,
       c_long int )
     TBLPROPERTIES ('table_type' = 'ICEBERG');
     ```

     これは EMQX からの構造化 MQTT データを格納する Iceberg テーブルを定義します。

9. テーブルの検証として、以下のクエリを実行し、テーブルが空であることを確認します：

   ```sql
   select * from testtable
   ```

   ::: tip

   Athena で SQL を実行する前に、正しい Catalog と Database（Namespace）が選択されていることを必ず確認してください。これにより、意図した S3 テーブルバケットにテーブルが作成されます。

   :::

## コネクターの作成

S3 Tables Sink を追加する前に、対応するコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. コネクタータイプとして **S3 Tables** を選択し、次へ進みます。

4. コネクター名を入力します。名前は英数字で始まり、英数字、ハイフン、アンダースコアを含めることができます。例として `my-s3-tables` を入力します。

5. 接続に必要な情報を入力します：

   - **S3Tables ARN**：AWS コンソールの Table buckets セクションで確認できる S3 テーブルバケットの Amazon Resource Name (ARN) を入力します。
   - **Access Key ID と Secret Access Key**（任意）：
     - **手動設定の場合**：S3 Tables と Athena へのアクセス権限を持つ IAM ユーザーまたはロールに紐づく AWS 認証情報を入力します。
     - **自動取得の場合**：EMQX が AWS EC2 インスタンス上で動作し、必要な権限を持つ IAM ロールが付与されている場合は空欄にできます。EMQX は IMDSv2 を通じて一時認証情報を自動取得します。詳細は [デプロイ前提条件と認証情報の取得方法](#デプロイ前提条件と認証情報の取得方法) を参照してください。
   - **Enable TLS**：S3 Tables への接続時は TLS がデフォルトで有効です。TLS 接続の詳細設定は [外部リソースアクセスの TLS 有効化](../../guides/network/overview.md#enable-tls-encryption-for-accessing-external-resources) を参照してください。
   - **Health Check Timeout**：コネクターが S3 Tables との接続状態を自動チェックする際のタイムアウト時間を指定します。

7. 残りの設定はデフォルト値のままで構いません。

8. **Create** をクリックする前に、**Test Connectivity** ボタンを押してコネクターが S3 Tables に正常に接続できるか確認できます。

9. **Create** ボタンをクリックしてコネクター作成を完了します。

これでコネクターの作成が完了し、次にルールと Sink を作成して S3 Tables へのデータ書き込みを指定します。

## Amazon S3 Tables Sink を使ったルールの作成

このセクションでは、EMQX で MQTT トピック `t/#` からのメッセージを処理し、処理結果を S3 Tables の `mybucket` バケットに書き込むルールの作成方法を示します。

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

   出力フィールドが Iceberg テーブルのスキーマと一致していることを必ず確認してください。必要なカラムが欠落または誤っていると、テーブルへのデータ追加に失敗する可能性があります。

   :::

4. アクションを追加し、**Action Type** ドロップダウンから `S3 Tables` を選択します。アクションのドロップダウンはデフォルトの `create action` のままにするか、既存の S3 Tables アクションを選択します。ここでは新しい Sink を作成してルールに追加します。

5. Sink 名と任意の説明を入力します。

6. **Connector** ドロップダウンから先ほど作成した `my-s3-tables` コネクターを選択します。もしくは、隣の **Create** ボタンをクリックして新しいコネクターを素早く定義できます。設定パラメータの詳細は [コネクターの作成](#コネクターの作成) を参照してください。

7. Sink の設定を行います：

   - **Namespace**：テーブルが存在するネームスペース。複数セグメントの場合はドット区切りで指定（例：`my.name.space`）。
   - **Table**：データを追加する Iceberg テーブル名（例：`testtable`）。
   - **Max Records**：S3 へ書き込む前にバッチ処理する最大レコード数。到達すると即座にバッチをフラッシュしてアップロードします。
   - **Time Interval**：Max Records に達していなくても、指定ミリ秒経過でバッチをフラッシュします。
   - **Data File Format**：S3 に保存するバッチデータのファイルフォーマット。サポートされる値：
     - `avro`：（デフォルト）Avro 形式で保存。行ベースでストリーミングデータやスキーマ進化に適します。
     - `parquet`：Apache Parquet 形式で保存。列ベースで大規模分析クエリに最適化されています。

8. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

9. **Advanced Settings** を展開し、必要に応じて詳細設定を行います（任意）。詳細は [高度な設定](#advanced-settings) を参照してください。

10. 残りの設定はデフォルト値のままにし、**Create** ボタンをクリックして Sink 作成を完了します。作成成功後、ルール作成画面に戻り、新しい Sink がルールアクションに追加されます。

11. ルール作成画面で **Create** ボタンをクリックし、ルール作成を完了します。

これでルール作成が完了し、**Rules** ページで新規ルールを確認でき、**Actions (Sink)** タブで新しい S3 Tables Sink を確認できます。

また、**Integration** -> **Flow Designer** からトポロジーを表示できます。トポロジーはトピック `t/#` のメッセージがルール `my_rule` によって解析され、S3 Tables に書き込まれる流れを視覚的に示します。

## ルールのテスト

このセクションでは、S3 Tables Sink を設定したルールのテスト方法を示します。

1. MQTTX を使い、トピック `t/1` にメッセージをパブリッシュします：

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "str": "hello S3 Tables", "int": 123 }'
   ```

   このメッセージはルール SQL とテーブルスキーマに対応する `payload.str` と `payload.int` フィールドを含みます。

2. **Rules** ページでルールのメトリクスと Sink の状態を監視します。新規の受信メッセージと送信メッセージがそれぞれ1件ずつ増えているはずです。

3. Athena クエリエディターを開き、正しい **Catalog**（例：`s3tablescatalog/mybucket`）と **Database**（ネームスペース）が選択されていることを確認します。

4. 以下の SQL クエリを実行します：

   ```sql
   SELECT * FROM testtable
   ```

   以下のような行が表示されるはずです：

   | c_str           | c_long |
   | --------------- | ------ |
   | hello S3 Tables | 123    |

## 高度な設定

このセクションでは、S3 Tables Sink の高度な設定オプションについて説明します。ダッシュボードの Sink 設定画面で **Advanced Settings** を展開し、用途に応じて以下のパラメータを調整できます。

| フィールド名                     | 説明                                                                                     | デフォルト値    |
| ------------------------------- | ---------------------------------------------------------------------------------------- | --------------- |
| **Min Part Size**                | マルチパートアップロード時の最小パートサイズ。<br/>このサイズに達するまでデータはメモリに蓄積されます。 | `5` MB          |
| **Max Part Size**                | マルチパートアップロード時の最大パートサイズ。<br/>このサイズを超えるパートはアップロードされません。 | `5` GB          |
| **Buffer Pool Size**             | EMQX と S3 Tables 間のデータフローを管理するバッファワーカープロセスの数。<br/>これらのワーカーはデータを一時的に保存・処理し、性能最適化とスムーズなデータ送信を実現します。 | `16`            |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）。<br/>この TTL を超えてバッファに滞留するか、送信後に S3 Tables からの応答やアックが得られない場合、リクエストは期限切れと判定されます。 | `45` 秒         |
| **Health Check Interval**        | Sink が S3 Tables との接続状態を自動チェックする間隔（秒）を指定します。                     | `15` 秒         |
| **Health Check Interval Jitter** | 基本のヘルスチェック間隔に加える一様ランダム遅延（ミリ秒）。<br/>複数ノードが同時にヘルスチェックを開始するのを防ぎます。複数のアクションやソースが同じコネクターを共有する場合に有効です。 | `0` ミリ秒      |
| **Health Check Timeout**         | コネクターが S3 Tables との接続状態を自動チェックする際のタイムアウト時間を指定します。         | `60` 秒         |
| **Max Buffer Queue Size**        | S3 Tables Sink の各バッファワーカーがバッファリング可能な最大バイト数。<br/>ワーカーはデータを一時保存し、効率的にデータストリームを処理します。システム性能やデータ送信要件に応じて調整してください。 | `256` MB        |
| **Batch Size**                   | EMQX から S3 Tables へ一度に送信するデータバッチの最大レコード数。<br/>サイズを調整することで転送効率と性能を最適化できます。`1` に設定するとバッチ化せず個別送信になります。 | 1000            |
| **Query Mode**                   | 同期（`synchronous`）または非同期（`asynchronous`）のリクエストモードを選択し、メッセージ送信を最適化します。<br/>非同期モードでは S3 Tables への書き込みが MQTT メッセージのパブリッシュをブロックしませんが、クライアントがメッセージを受信してから S3 Tables に到達するまでにタイムラグが生じる可能性があります。 | `Asynchronous`  |
| **In-flight  Window**            | 「インフライトキューリクエスト」とは、送信済みで応答やアックをまだ受け取っていないリクエストのことです。<br/>この設定は Sink と S3 Tables 間の同時インフライトリクエストの最大数を制御します。<br/>**Request Mode** が `asynchronous` の場合に重要で、同一 MQTT クライアントからのメッセージを厳密に順序処理したい場合は `1` に設定してください。 | `100`           |
