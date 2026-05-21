# Amazon S3 Tables への MQTT データ取り込み

[Amazon S3 Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) は、分析ワークロードに最適化された専用のストレージソリューションです。Apache Iceberg フォーマットで IoT センサーの読み取り値などの表形式データを高性能かつスケーラブルかつ安全に保存できます。

EMQX は Amazon S3 Tables とのシームレスな統合をサポートし、MQTT メッセージを効率的に S3 テーブルバケットに保存可能です。この統合により、柔軟でスケーラブルな IoT データストレージが実現し、Amazon Athena、Amazon Redshift、Amazon EMR などの AWS サービスを用いた高度な分析や処理が可能になります。

本ページでは EMQX と Amazon S3 Tables 間のデータ統合について詳しく解説し、ルールおよび Sink の作成方法を実践的に案内します。

## 動作概要

EMQX の Amazon S3 Tables 統合は標準機能として提供されています。この統合は EMQX のルールエンジンと S3 Tables Sink を活用し、MQTT メッセージを変換して Apache Iceberg フォーマットのテーブルに直接ストリームし、S3 テーブルバケットに保存します。これにより長期保存および下流分析が可能です。

典型的な IoT シナリオでは以下のように動作します：

- **EMQX** は MQTT ブローカーとして機能し、デバイスの接続管理、メッセージルーティング、データ処理を担当します。
- **Amazon S3 Tables** は MQTT メッセージデータを表形式で耐久的かつクエリ可能なストレージとして提供します。
- **Amazon Athena** は Iceberg テーブルの定義と保存データに対する SQL クエリ実行に使用されます。

![emqx-integration-s3-tables](./assets/emqx-integration-s3-tables.png)

ワークフローは以下の通りです：

1. **デバイスが EMQX に接続**：IoT デバイスが MQTT 経由で EMQX に接続し、テレメトリデータをパブリッシュします。
2. **メッセージルーティングとルールマッチング**：EMQX のルールエンジンが受信した MQTT メッセージを定義済みトピックに照合し、特定のフィールドや値を抽出します。
3. **データ変換**：EMQX のルールでメッセージペイロードをフィルタリング、変換、または拡張し、ターゲットの Iceberg テーブルのスキーマに合わせます。
4. **Amazon S3 Tables への書き込み**：ルールが S3 Tables Sink アクションをトリガーし、変換済みデータをバッチ処理して Iceberg 互換の書き込み API を使い S3 Tables に送信します。データは Iceberg テーブルのパーティション下に Parquet ファイルとして永続化されます。
5. **クエリと分析**：取り込まれたデータは Amazon Athena でクエリ可能となり、他のデータセットと結合したり、Redshift Spectrum、Amazon EMR、Presto や Trino などのサードパーティ分析エンジンで分析できます。

## 特長と利点

EMQX で Amazon S3 Tables データ統合を利用することで、以下の特長と利点が得られます：

- **リアルタイムストリーム処理**：EMQX のルールエンジンにより、MQTT メッセージをリアルタイムに抽出・変換・条件付きルーティングし、S3 Tables へ配信可能です。
- **Iceberg ベースの S3 ストレージ**：メッセージは Apache Iceberg テーブルに書き込まれ、従来のデータベース不要で SQL ライクなアクセスパターンを実現します。
- **分析ツールとの簡単統合**：データが S3 Tables に入ると、Amazon Athena（SQL）、Amazon EMR、Redshift Spectrum、Presto、Trino、Snowflake などでクエリ可能です。
- **柔軟かつコスト効率の良いストレージ**：Amazon S3 は高耐久で低コストのオブジェクトストレージを提供し、アーカイブ、コンプライアンス、時系列分析に最適です。

## はじめる前に

このセクションでは EMQX で Amazon S3 Tables Sink を作成するための準備事項を紹介します。

### 前提条件

作業を進める前に以下を理解していることを確認してください：

#### EMQX の概念：

- [ルールエンジン](./rules.md)：MQTT メッセージからデータを抽出・変換するロジックの定義方法を理解します。
- [データ統合](./data-bridges.md)：EMQX のコネクターと Sink の概念を理解します。

#### AWS の概念：

AWS S3 Tables を初めて利用する場合は、以下の用語を確認してください：

- **EC2**：AWS の仮想マシンサービス（コンピュートインスタンス）。
- **IAM**：AWS Identity and Access Management。インスタンスロールはそのインスタンス上で動作するプログラムに一時的な認証情報を発行できます。
- **IMDSv2**：EC2 のインスタンスメタデータサービス v2。トークンベースでより安全にメタデータや一時認証情報を取得します。
- **Table Bucket**：S3 Tables で Iceberg ベースのテーブルデータとメタデータを格納する専用の S3 バケット。
- **Amazon Athena**：Amazon S3 に保存されたデータに対して直接 SQL クエリを実行できるサーバーレスクエリエンジン。`CREATE TABLE` などの DDL 文もサポートします。
- **Catalog**：Athena のメタデータコンテナで、データベース（ネームスペース）やテーブルを管理します。
- **Database (Namespace)**：Catalog 配下の論理的なテーブルグループ。
- **Iceberg Table**：高性能でトランザクション対応のデータレイク向けテーブルフォーマット。スキーマ進化、パーティションプルーニング、タイムトラベルクエリをサポートします。

### デプロイ前提条件と認証情報の取得方法

S3 Tables コネクターは認証情報の取得方法を2通りサポートしています。EMQX のデプロイ環境に応じて選択してください：

- **オプション1：アクセスキーを手動設定する場合**  
  [コネクター作成](#create-a-connector)時に **Access Key ID** と **Secret Access Key** を指定します。これらの認証情報は対象の S3 Tables と Athena への必要な権限を持つ必要があります。ローカル環境、コンテナ、Kubernetes、非 AWS クラウド、またはインスタンスロールが割り当てられていない EC2 で適しています。  
  IAM ユーザーのアクセスキー作成・管理については [AWS ドキュメント](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html)を参照してください。

- **オプション2：一時認証情報を自動取得（EC2 のみ）**  
  EMQX が AWS EC2 インスタンス上で動作し、そのインスタンスに必要な権限を持つ IAM ロールが割り当てられている場合、コネクターの **Access Key ID** と **Secret Access Key** を空欄にできます。EMQX は IMDSv2 API を使い、そのロールに紐づく一時認証情報を自動取得します。  
  EC2 インスタンスに IAM ロールを割り当てる方法は [AWS ドキュメント](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2.html)を参照してください。

::: tip 注意事項

- インスタンスロールが対象の S3 Tables（バケット/テーブル）および Athena への十分な権限を持っていることを確認してください。権限不足の場合、**Test Connectivity** が失敗します。  
- 一時認証情報の管理には EC2 に割り当てた IAM ロールの利用を推奨します。EC2 以外やロール未割当の場合はオプション1を利用し、アクセスキーを手動入力してください。

:::

### S3 Tables バケットの準備

EMQX で Sink を作成する前に、AWS S3 Tables 側で MQTT データの送信先を準備します。具体的には：

- 実際のデータファイルを格納する Table Bucket
- 関連テーブルを論理的にグループ化する Namespace
- 構造化された MQTT データを受け取る Iceberg ベースのテーブル

準備手順：

1. AWS マネジメントコンソールにログインします。

2. S3 サービスに移動し、左側ナビゲーションペインで **Table buckets** をクリックします。

3. **Create table bucket** をクリックし、テーブルバケット名（例：`mybucket`）を入力して **Create table bucket** をクリックします。

4. バケット作成後、バケット名をクリックしてテーブル一覧画面に移動します。

5. **Create table with Athena** をクリックします。ポップアップが表示され、Namespace の指定を求められます。

6. **Create a namespace** を選択し、Namespace 名を入力して作成を確定します。

7. Namespace 作成後、再度 **Create table with Athena** をクリックします。

8. Iceberg テーブルのスキーマを定義します：

   - **Query table with Athena** をクリックし、クエリエディターを開きます。

     - **Catalog** セレクターから、作成したバケット名に対応する Catalog（例：`s3tablescatalog/mybucket`）を選択します。
     - **Database** セレクターから、先ほど作成した Namespace を選択します。

   - 以下の DDL を実行し、テーブルタイプが `ICEBERG` であることを確認してテーブルを作成します。例：

     ```sql
     CREATE TABLE testtable (
       c_str string,
       c_long int )
     TBLPROPERTIES ('table_type' = 'ICEBERG');
     ```

     これは EMQX からの構造化 MQTT データを格納する Iceberg ベースのテーブル定義です。

9. テーブルが正常に作成され、空であることを確認するために以下を実行します：

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

4. コネクター名を入力します。名前は英数字で始まり、英数字、ハイフン、アンダースコアを含めることができます。例として `my-s3-tables` と入力します。

5. 接続情報を入力します：

   - **S3Tables ARN**：S3 テーブルバケットの Amazon Resource Name (ARN) を入力します。AWS コンソールの Table buckets セクションで確認可能です。
   - **Access Key ID と Secret Access Key**（任意）：
     - **手動設定の場合**：S3 Tables と Athena へのアクセス権限を持つ IAM ユーザーまたはロールの認証情報を入力します。
     - **自動取得の場合**：EMQX が AWS EC2 インスタンス上で動作し、必要権限を持つ IAM ロールが割り当てられていれば空欄にできます。EMQX は IMDSv2 経由で一時認証情報を取得します。詳細は [デプロイ前提条件と認証情報の取得方法](#デプロイ前提条件と認証情報の取得方法) を参照してください。
   - **Enable TLS**：S3 Tables への接続はデフォルトで TLS 有効です。TLS 接続オプションの詳細は [外部リソースアクセスの TLS](../network/overview.md#enable-tls-encryption-for-accessing-external-resources) を参照してください。
   - **Health Check Timeout**：コネクターが S3 Tables との接続の自動ヘルスチェックを行う際のタイムアウト時間を指定します。

7. その他の設定はデフォルト値を使用します。

8. **Create** をクリックする前に、**Test Connectivity** を押して S3 Tables への接続確認を行うことができます。

9. 最後に **Create** ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、次にルールと Sink を作成して S3 Tables への書き込み対象を指定します。

## Amazon S3 Tables Sink を用いたルール作成

このセクションでは、MQTT トピック `t/#` からのメッセージを処理し、処理結果を S3 Tables の `mybucket` バケットに書き込むルール作成方法を示します。

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

   SQL に不慣れな場合は、**SQL Examples** と **Enable Debug** をクリックしてルール SQL の学習やテストが可能です。

   :::

   ::: tip

   出力フィールドは Iceberg テーブルのスキーマと一致させてください。必須カラムが欠落または誤った名前の場合、テーブルへのデータ追加に失敗する可能性があります。

   :::

4. アクションを追加し、**Action Type** ドロップダウンから `S3 Tables` を選択します。アクションドロップダウンはデフォルトの `create action` のままか、既存の S3 Tables アクションを選択します。ここでは新規 Sink を作成してルールに追加します。

5. Sink 名と任意の説明を入力します。

6. **Connector** ドロップダウンから先ほど作成した `my-s3-tables` コネクターを選択します。新規コネクターを素早く作成したい場合はドロップダウン横の **Create** ボタンをクリックし、[コネクターの作成](#コネクターの作成) を参照してください。

7. Sink 設定を構成します：

   - **Namespace**：テーブルが存在するネームスペース。複数セグメントの場合はドット区切りで指定（例：`my.name.space`）。
   - **Table**：データを追加する Iceberg テーブル名（例：`testtable`）。
   - **Max Records**：S3 への書き込み前にバッチ処理する最大レコード数。この数に達すると即座にバッチがフラッシュされアップロードされます。
   - **Time Interval**：Max Records に達していなくても、指定したミリ秒数経過後にデータをフラッシュする最大待機時間。
   - **Data File Format**：S3 に保存するバッチ化された MQTT メッセージのデータファイル形式。サポート値：
     - `avro`：（デフォルト）行ベースの Avro フォーマット。ストリーミングデータやスキーマ進化に適しています。
     - `parquet`：列指向の Apache Parquet フォーマット。大規模データの分析クエリに最適化されています。

8. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

9. **Advanced Settings** を展開し、必要に応じて詳細設定を行います（任意）。詳細は [詳細設定](#advanced-settings) を参照してください。

10. その他の設定はデフォルト値のままにして、**Create** ボタンをクリックし Sink 作成を完了します。作成成功後、ルール作成画面に戻り、新しい Sink がルールアクションに追加されます。

11. ルール作成画面で **Create** をクリックし、ルール作成を完了します。

これでルールが正常に作成されました。**Rules** ページで新規ルールを確認でき、**Actions (Sink)** タブで新しい S3 Tables Sink を確認できます。

また、**Integration** -> **Flow Designer** を開くとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` で解析され、S3 Tables に書き込まれる流れを視覚的に確認できます。

## ルールのテスト

このセクションでは、S3 Tables Sink を設定したルールのテスト方法を説明します。

1. MQTTX を使い、トピック `t/1` にメッセージをパブリッシュします：

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "str": "hello S3 Tables", "int": 123 }'
   ```

   このメッセージは `payload.str` と `payload.int` フィールドを含み、ルール SQL とテーブルスキーマに一致しています。

2. **Rules** ページでルールのメトリクスと Sink のステータスを監視します。新規の受信メッセージと送信メッセージがそれぞれ1件ずつ増えているはずです。

3. Athena クエリエディターを開き、正しい **Catalog**（例：`s3tablescatalog/mybucket`）と **Database**（Namespace）が選択されていることを確認します。

4. 以下の SQL クエリを実行します：

   ```sql
   SELECT * FROM testtable
   ```

   以下のような行が表示されるはずです：

   | c_str           | c_long |
   | --------------- | ------ |
   | hello S3 Tables | 123    |

## 詳細設定

このセクションでは、S3 Tables Sink の詳細設定オプションについて説明します。ダッシュボードの Sink 設定画面で **Advanced Settings** を展開し、用途に応じて以下のパラメーターを調整可能です。

| 項目名                          | 説明                                                                                         | デフォルト値     |
| ------------------------------- | -------------------------------------------------------------------------------------------- | --------------- |
| **Min Part Size**                | マルチパートアップロードの最小パートサイズ。<br/>このサイズに達するまでデータはメモリに蓄積されます。 | `5` MB          |
| **Max Part Size**                | マルチパートアップロードの最大パートサイズ。<br/>このサイズを超えるパートはアップロードされません。 | `5` GB          |
| **Buffer Pool Size**             | EMQX と S3 Tables 間のデータフローを管理するバッファワーカーの数。これらのワーカーはデータを一時的に保存・処理し、性能最適化とスムーズなデータ送信を支えます。 | `16`            |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）。この時間を超えるか、送信後にタイムリーな応答やアックを受け取れない場合、リクエストは期限切れと判定されます。 | `45` 秒         |
| **Health Check Interval**        | Sink が S3 Tables との接続状態を自動ヘルスチェックする間隔（秒）を指定します。 | `15` 秒         |
| **Health Check Interval Jitter** | 基本のヘルスチェック間隔に加える一様ランダム遅延（ミリ秒）。複数ノードが同時にヘルスチェックを開始する確率を減らします。複数のアクションやソースが同一コネクターを共有する場合に有効です。 | `0` ミリ秒      |
| **Health Check Timeout**         | コネクターが S3 Tables との接続の自動ヘルスチェックを行う際のタイムアウト時間を指定します。 | `60` 秒         |
| **Max Buffer Queue Size**        | 各バッファワーカーが S3 Tables Sink 内で一時的にバッファリング可能な最大バイト数。バッファワーカーはデータ送信前の中継役として機能し、システム性能やデータ送信要件に応じて調整します。 | `256` MB        |
| **Batch Size**                   | EMQX から S3 Tables へ一度に転送するデータバッチの最大レコード数。サイズを調整することでデータ転送効率と性能を最適化できます。`1` に設定するとバッチ化せず個別に送信します。 | 1000            |
| **Query Mode**                   | 同期（`synchronous`）または非同期（`asynchronous`）のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードでは S3 Tables への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを受信してから S3 Tables に到達するまでに遅延が生じる可能性があります。 | `Asynchronous`  |
| **In-flight Window**             | 「インフライトキューリクエスト」とは、送信済みで応答やアックをまだ受け取っていないリクエストを指します。この設定は Sink と S3 Tables 間の同時インフライトリクエストの最大数を制御します。<br/>`Request Mode` が `asynchronous` の場合に重要で、同一 MQTT クライアントからのメッセージを厳密に順序処理したい場合は `1` に設定してください。 | `100`           |
