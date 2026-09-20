# Amazon S3 Tables への MQTT データ取り込み

[Amazon S3 Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) は、分析ワークロードに最適化された専用のストレージソリューションです。IoT センサーの読み取り値などの表形式データを Apache Iceberg フォーマットで高性能かつスケーラブル、かつ安全に保存できます。

EMQX は Amazon S3 Tables とのシームレスな統合をサポートし、MQTT メッセージを効率的に S3 テーブルバケットに保存できます。この統合により、柔軟でスケーラブルな IoT データストレージが可能となり、Amazon Athena、Amazon Redshift、Amazon EMR などの AWS サービスを用いた高度な分析や処理が容易になります。

本ページでは、EMQX と Amazon S3 Tables 間のデータ統合について詳しく解説し、ルールおよび Sink の作成方法を実践的に案内します。

## 動作概要

EMQX の Amazon S3 Tables 統合は標準機能として提供されています。この統合は EMQX のルールエンジンと S3 Tables Sink を活用し、MQTT メッセージを変換して Apache Iceberg フォーマットのテーブルに直接ストリーミングし、S3 テーブルバケットに保存します。これにより長期保存および下流分析が可能となります。

典型的な IoT シナリオでは：

- **EMQX** は MQTT ブローカーとして機能し、デバイスの接続管理、メッセージルーティング、データ処理を行います。
- **Amazon S3 Tables** は MQTT メッセージデータを表形式で耐久的かつクエリ可能なストレージとして提供します。
- **Amazon Athena** は Iceberg テーブルの定義や保存データに対する SQL クエリ実行に使用されます。

![emqx-integration-s3-tables](./assets/emqx-integration-s3-tables.png)

ワークフローは以下の通り進行します：

1. **デバイスの EMQX への接続**：IoT デバイスが MQTT 経由で EMQX に接続し、テレメトリデータをパブリッシュし始めます。
2. **メッセージルーティングとルールマッチング**：EMQX は内蔵のルールエンジンを用いて、受信した MQTT メッセージを定義済みのトピックにマッチさせ、特定のフィールドや値を抽出します。
3. **データ変換**：EMQX のルールでメッセージペイロードをフィルタリング、変換、または拡張し、ターゲットの Iceberg テーブルのスキーマに合わせます。
4. **Amazon S3 Tables への書き込み**：ルールが S3 Tables Sink アクションをトリガーし、変換済みデータをバッチ処理して Iceberg 互換の書き込み API を使い Amazon S3 Tables に送信します。データは Iceberg テーブルのパーティション下に Parquet ファイルとして永続化されます。
5. **クエリと分析**：取り込まれたデータは Amazon Athena でクエリ可能となり、他のデータセットと結合したり、Redshift Spectrum、Amazon EMR、Presto や Trino などのサードパーティ分析エンジンで分析できます。

## 特長とメリット

EMQX で Amazon S3 Tables データ統合を利用することで、以下の特長と利点が得られます：

- **リアルタイムストリーム処理**：EMQX のルールエンジンにより、MQTT メッセージをリアルタイムで抽出・変換・条件付きルーティングし、S3 Tables へ配信可能です。
- **Iceberg ベースの S3 ストレージ**：メッセージは Apache Iceberg テーブルに書き込まれ、従来のデータベース不要で SQL ライクなアクセスが可能です。
- **分析ツールとの簡単統合**：データが S3 Tables に入ると、Amazon Athena（SQL）、Amazon EMR、Redshift Spectrum、Presto、Trino、Snowflake などでクエリや分析ができます。
- **柔軟かつコスト効率の高いストレージ**：Amazon S3 は高耐久で低コストのオブジェクトストレージを提供し、アーカイブ、コンプライアンス、時系列分析に最適です。

## はじめる前に

このセクションでは、EMQX で Amazon S3 Tables Sink を作成するための準備について説明します。

### 前提条件

作業を進める前に、以下の内容を理解していることを推奨します。

#### EMQX の概念：

- [ルールエンジン](./rules.md)：MQTT メッセージからデータを抽出・変換するロジックを定義する方法を理解してください。
- [データ統合](./data-bridges.md)：EMQX のコネクターとシンクの概念を理解してください。

#### AWS の概念：

AWS S3 Tables が初めての場合は、以下の主要用語を確認してください：

- **EC2**：AWS の仮想マシンサービス（コンピュートインスタンス）。
- **IAM**：AWS Identity and Access Management。インスタンスロールはそのインスタンス上で動作するプログラムに一時的な認証情報を発行できます。
- **IMDSv2**：EC2 のインスタンスメタデータサービス v2。トークンベースでより安全にメタデータや一時認証情報を取得します。
- **Table Bucket**：S3 Tables で Iceberg ベースのテーブルデータとメタデータを格納するための専用 S3 バケット。
- **Amazon Athena**：Amazon S3 に保存されたデータに対して直接 SQL クエリを実行できるサーバレスクエリエンジン。`CREATE TABLE` などの DDL ステートメントをサポートし、スキーマ定義とクエリ構造を管理します。
- **Catalog**：Athena のメタデータコンテナで、データベース（ネームスペース）やテーブルを整理します。
- **Database (Namespace)**：Catalog 配下の論理的なテーブルグループ。
- **Iceberg Table**：高性能でトランザクション対応のデータレイク用テーブルフォーマット。スキーマ進化、パーティションプルーニング、タイムトラベルクエリをサポートします。

### デプロイ前提条件と認証情報の取得方法

S3 Tables コネクターは認証情報の取得方法を2通りサポートしています。EMQX のデプロイ環境に応じて選択してください：

- **オプション1：アクセスキーを手動設定する場合**  
  [コネクター作成](#create-a-connector)時に **Access Key ID** と **Secret Access Key** を入力します。これらの認証情報は対象の S3 Tables と Athena に必要な権限を持っている必要があります。ローカル環境、コンテナ、Kubernetes、非 AWS クラウド、またはインスタンスロールが割り当てられていない EC2 での利用に適しています。

  IAM ユーザーのアクセスキー作成・管理については、[AWS ドキュメントのアクセスキー管理](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html)を参照してください。

- **オプション2：一時認証情報を自動取得する場合（EC2 のみ）**  
  EMQX が AWS EC2 インスタンス上で稼働し、そのインスタンスに必要な権限を持つ IAM ロールが割り当てられている場合、コネクターの **Access Key ID** と **Secret Access Key** を空欄にできます。EMQX は IMDSv2 API を使ってそのロールに紐づく一時認証情報を自動取得します。

  EC2 インスタンスに IAM ロールを割り当てる方法は、[AWS ドキュメントの IAM ロールの使用](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2.html)を参照してください。

::: tip 注意事項

- インスタンスロールが対象の S3 Tables（バケット/テーブル）および Athena に対して十分な権限を持っていることを確認してください。そうでないと **Test Connectivity** が失敗します。
- 一時認証情報の管理には EC2 インスタンスに割り当てた IAM ロールの利用を推奨します。EC2 以外やロール未割当の場合はオプション1でアクセスキーを手動入力してください。

:::

### S3 Tables バケットの準備

EMQX で Sink を作成する前に、AWS S3 Tables 上に MQTT データの送信先を準備します。以下が必要です：

- 実際のデータファイルを格納する Table Bucket
- 関連テーブルを論理的にグループ化する Namespace
- 構造化された MQTT データを受け取る Iceberg ベースのテーブル

1. AWS マネジメントコンソールにログインします。

2. S3 サービスに移動し、左側のナビゲーションペインで **Table buckets** をクリックします。

3. **Create table bucket** をクリックし、テーブルバケット名（例：`mybucket`）を入力して **Create table bucket** をクリックします。

4. バケット作成後、そのバケットをクリックしてテーブル一覧に移動します。

5. **Create table with Athena** をクリックします。ポップアップが表示され、Namespace の指定を求められます。

6. **Create a namespace** を選択し、ネームスペース名を入力して作成を確定します。

7. ネームスペース作成後、再度 **Create table with Athena** をクリックします。

8. Iceberg テーブルのスキーマを定義します：

   - **Query table with Athena** をクリックし、**Query editor** で以下を設定します：

     - **Catalog** セレクターから、作成したバケット名に対応するカタログ（例：`s3tablescatalog/mybucket`）を選択。
     - **Database** セレクターから、作成したネームスペースを選択。

   - 以下の DDL を実行してテーブルを作成し、テーブルタイプが `ICEBERG` であることを指定します。例：

     ```sql
     CREATE TABLE testtable (
       c_str string,
       c_long int )
     TBLPROPERTIES ('table_type' = 'ICEBERG');
     ```

     これは EMQX からの構造化 MQTT データを格納する Iceberg ベースのテーブルを定義します。

9. テーブルが正しく作成されているか確認します。空の状態を確認するために以下を実行します：

   ```sql
   select * from testtable
   ```

   ::: tip

   Athena で SQL を実行する際は、必ず正しい Catalog と Database（ネームスペース）が選択されていることを確認してください。これにより意図した S3 テーブルバケットにテーブルが作成されます。

   :::

## コネクターの作成

S3 Tables Sink を追加する前に、対応するコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。  
2. 右上の **Create** ボタンをクリックします。  
3. コネクタータイプとして **S3 Tables** を選択し、次へ進みます。  
4. コネクター名を入力します。名前は英数字で始まり、英数字、ハイフン、アンダースコアを含めることができます。例として `my-s3-tables` と入力します。  
5. 必要な接続情報を入力します：  
   - **S3Tables ARN**：S3 テーブルバケットの Amazon リソースネーム（ARN）を入力します。AWS コンソールの Table buckets セクションで確認可能です。  
   - **Access Key ID と Secret Access Key**（任意）：  
     - **手動設定**：S3 Tables と Athena へのアクセス権限を持つ IAM ユーザーまたはロールの AWS 認証情報を入力します。  
     - **自動取得**：EMQX が AWS EC2 インスタンス上で稼働し、必要な権限を持つ IAM ロールが割り当てられている場合は空欄のままで構いません。EMQX は IMDSv2 経由で一時認証情報を自動取得します。詳細は [デプロイ前提条件と認証情報の取得方法](#デプロイ前提条件と認証情報の取得方法) を参照してください。  
   - **Enable TLS**：S3 Tables への接続時は TLS がデフォルトで有効です。詳細は [TLS for External Resource Access](../../guides/network/overview.md#enable-tls-encryption-for-accessing-external-resources) を参照してください。  
   - **Health Check Timeout**：S3 Tables との接続の自動ヘルスチェックのタイムアウト時間を指定します。  
7. 残りの設定はデフォルト値を使用します。  
8. **Create** をクリックする前に **Test Connectivity** を押して、コネクターが S3 Tables サービスに接続できるか確認できます。  
9. 最後に **Create** ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成は完了です。次に、S3 Tables に書き込むデータを指定するルールと Sink を作成します。

## Amazon S3 Tables Sink を使ったルール作成

このセクションでは、EMQX でソース MQTT トピック `t/#` のメッセージを処理し、処理結果を設定済みの S3 Tables バケット `mybucket` に書き込むルールの作成方法を示します。

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

   出力フィールドは Iceberg テーブルのスキーマと一致していることを必ず確認してください。必須カラムが欠落または誤った名前の場合、データのテーブル追加に失敗する可能性があります。

   :::

4. アクションを追加し、**Action Type** ドロップダウンから `S3 Tables` を選択します。アクションのドロップダウンはデフォルトの `create action` のままにするか、既存の S3 Tables アクションを選択します。ここでは新しい Sink を作成してルールに追加します。

5. Sink 名と任意の説明を入力します。

6. **Connector** ドロップダウンから先ほど作成した `my-s3-tables` コネクターを選択します。新しいコネクターを素早く作成したい場合は、ドロップダウン横の **Create** ボタンをクリックしてください。[コネクター作成](#コネクターの作成)を参照してください。

7. Sink の設定を行います：

   - **Namespace**：テーブルが存在するネームスペース。複数セグメントの場合はドット区切りで指定（例：`my.name.space`）。
   - **Table**：データを追加する Iceberg テーブル名（例：`testtable`）。
   - **Max Records**：S3 への書き込み前にバッチ処理する最大レコード数。到達時に即座にバッチをフラッシュしてアップロードします。
   - **Time Interval**：Max Records に達していなくても、指定した時間（ミリ秒）経過後にバッチをフラッシュします。
   - **Data File Format**：S3 に保存するバッチデータのファイル形式。サポート値：
     - `avro`：（デフォルト）Avro フォーマットで保存。行ベースでストリーミングデータやスキーマ進化に適します。
     - `parquet`：Apache Parquet フォーマットで保存。列指向で大規模分析クエリに最適です。

8. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

9. **Advanced Settings** を展開し、必要に応じて詳細設定を行います（任意）。詳細は [Advanced Settings](#advanced-settings) を参照してください。

10. 残りの設定はデフォルト値を使用し、**Create** ボタンをクリックして Sink 作成を完了します。作成成功後、ルール作成画面に戻り、新しい Sink がルールアクションに追加されます。

11. ルール作成画面で **Create** ボタンをクリックし、ルール作成全体を完了します。

これでルール作成は完了です。**Rules** ページで新規ルールを確認でき、**Actions (Sink)** タブで新しい S3 Tables Sink を確認できます。

また、**Integration** -> **Flow Designer** でトポロジーを表示できます。トポロジーはトピック `t/#` のメッセージがルール `my_rule` によって解析され、S3 Tables に書き込まれる流れを視覚的に示します。

## ルールのテスト

このセクションでは、S3 Tables Sink を設定したルールのテスト方法を示します。

1. MQTTX を使ってトピック `t/1` にメッセージをパブリッシュします：

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "str": "hello S3 Tables", "int": 123 }'
   ```

   このメッセージはルール SQL とテーブルスキーマに対応する `payload.str` と `payload.int` フィールドを含みます。

2. **Rules** ページでルールのメトリクスと Sink の状態を監視します。新規の受信メッセージと送信メッセージがそれぞれ1件ずつ増えているはずです。

3. Athena のクエリエディターを開き、正しい **Catalog**（例：`s3tablescatalog/mybucket`）と **Database**（ネームスペース）が選択されていることを確認します。

4. 以下の SQL クエリを実行します：

   ```sql
   SELECT * FROM testtable
   ```

   以下のような行が表示されるはずです：

   | c_str           | c_long |
   | --------------- | ------ |
   | hello S3 Tables | 123    |

## Advanced Settings

このセクションでは、S3 Tables Sink の詳細設定オプションについて説明します。ダッシュボードの Sink 設定画面で **Advanced Settings** を展開すると、以下のパラメーターを用途に応じて調整できます。

| フィールド名                      | 説明                                                                                         | デフォルト値     |
| -------------------------------- | -------------------------------------------------------------------------------------------- | --------------- |
| **Min Part Size**                | マルチパートアップロードの最小パートサイズ。<br/>このサイズに達するまでデータはメモリに蓄積されます。 | `5` MB          |
| **Max Part Size**                | マルチパートアップロードの最大パートサイズ。<br/>このサイズを超えるパートはアップロードされません。 | `5` GB          |
| **Buffer Pool Size**             | バッファワーカープロセスの数を指定します。EMQX と S3 Tables 間のデータフローを管理し、一時的にデータを格納・処理します。パフォーマンス最適化とスムーズなデータ送信に重要です。 | `16`            |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。TTL を超えたリクエストや、送信後に S3 Tables からの応答やアックがタイムリーに得られない場合、そのリクエストは期限切れとみなされます。 | `45` 秒         |
| **Health Check Interval**        | Sink が S3 Tables との接続状態を自動ヘルスチェックする間隔（秒）を指定します。 | `15` 秒         |
| **Health Check Interval Jitter** | ヘルスチェック間隔に加える一様ランダム遅延（ミリ秒）です。複数ノードが同時にヘルスチェックを開始するのを防ぎます。複数のアクションやソースが同一コネクターを共有する場合に有効です。 | `0` ミリ秒      |
| **Health Check Timeout**         | コネクターが S3 Tables との接続の自動ヘルスチェックを行う際のタイムアウト時間を指定します。 | `60` 秒         |
| **Max Buffer Queue Size**        | S3 Tables Sink の各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータ送信前に一時的にデータを保持し、効率的なデータストリーム処理を実現します。システム性能やデータ送信要件に応じて調整してください。 | `256` MB        |
| **Batch Size**                   | EMQX から S3 Tables へ一度に送信するデータバッチの最大レコード数を指定します。サイズを調整することでデータ転送の効率とパフォーマンスを最適化できます。`1` に設定すると、レコードをバッチ化せず個別に送信します。 | 1000            |
| **Query Mode**                   | `synchronous`（同期）または `asynchronous`（非同期）のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードでは S3 Tables への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを受信してから S3 Tables に到達するまでにタイムラグが生じる可能性があります。 | `Asynchronous`  |
| **In-flight  Window**            | 「インフライトキューリクエスト」とは、送信済みだがまだ応答やアックを受け取っていないリクエストを指します。この設定は Sink と S3 Tables 間の同時インフライトリクエスト数の最大値を制御します。<br/>`Request Mode` が `asynchronous` の場合に特に重要です。同一 MQTT クライアントからのメッセージを厳密に順序処理したい場合、この値は `1` に設定してください。 | `100`           |
