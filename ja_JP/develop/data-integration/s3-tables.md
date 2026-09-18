# Amazon S3 Tables への MQTT データ取り込み

[Amazon S3 Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) は、分析ワークロードに最適化された専用のストレージソリューションです。Apache Iceberg フォーマットで IoT センサーの読み取り値などの表形式データを高性能かつスケーラブルかつ安全に保存できます。

EMQX は Amazon S3 Tables とのシームレスな連携をサポートし、MQTT メッセージを効率的に S3 テーブルバケットに保存可能です。この連携により柔軟でスケーラブルな IoT データストレージが実現し、Amazon Athena、Amazon Redshift、Amazon EMR などの AWS サービスを用いた高度な分析や処理が可能になります。

本ページでは EMQX と Amazon S3 Tables 間のデータ統合について詳細に解説し、ルールおよび Sink の作成手順を案内します。

## 動作の仕組み

EMQX の Amazon S3 Tables 連携は標準機能として提供されています。この連携は EMQX のルールエンジンと S3 Tables Sink を活用し、MQTT メッセージを変換して直接 Apache Iceberg フォーマットのテーブルにストリーミングし、S3 テーブルバケットに保存して長期保管や下流分析に利用します。

典型的な IoT シナリオでは：

- **EMQX** は MQTT ブローカーとして動作し、デバイス接続、メッセージルーティング、データ処理を担当します。
- **Amazon S3 Tables** は MQTT メッセージデータを表形式で耐久的かつクエリ可能なストレージとして受け入れます。
- **Amazon Athena** は Iceberg テーブルを定義し、保存されたデータに対して SQL クエリを実行します。

![emqx-integration-s3-tables](./assets/emqx-integration-s3-tables.png)

ワークフローは以下の通りです：

1. **デバイスが EMQX に接続**：IoT デバイスが MQTT 経由で EMQX に接続し、テレメトリデータをパブリッシュします。
2. **メッセージルーティングとルールマッチング**：EMQX は組み込みのルールエンジンで受信した MQTT メッセージを定義済みトピックにマッチングし、特定のフィールドや値を抽出します。
3. **データ変換**：EMQX のルールでペイロードをフィルタリング、変換、または拡張し、ターゲットの Iceberg テーブルのスキーマに合わせます。
4. **Amazon S3 Tables への書き込み**：ルールが S3 Tables Sink アクションをトリガーし、変換済みデータをバッチ処理して Iceberg 互換の書き込み API を使い S3 Tables に送信します。データは Iceberg テーブルのパーティション下に Parquet ファイルとして永続化されます。
5. **クエリと分析**：取り込まれたデータは Amazon Athena でクエリ可能で、他のデータセットと結合したり、Redshift Spectrum、Amazon EMR、Presto、Trino などのサードパーティ分析エンジンで分析できます。

## 特長と利点

EMQX で Amazon S3 Tables データ統合を利用すると、以下の特長と利点があります：

- **リアルタイムストリーム処理**：EMQX のルールエンジンにより、MQTT メッセージをリアルタイムに抽出・変換・条件付きルーティングしてから S3 Tables に送信可能です。
- **Iceberg ベースの S3 ストレージ**：メッセージは Apache Iceberg テーブルに書き込まれ、従来のデータベース不要で SQL ライクなアクセスが可能です。
- **分析ツールとの簡単連携**：データが S3 Tables に入ると、Amazon Athena（SQL）、Amazon EMR、Redshift Spectrum、Presto、Trino、Snowflake などでクエリ可能です。
- **柔軟かつコスト効率の良いストレージ**：Amazon S3 は高耐久で低コストのオブジェクトストレージを提供し、アーカイブ、コンプライアンス、時系列分析に最適です。

## はじめる前に

このセクションでは EMQX で Amazon S3 Tables Sink を作成するための準備について説明します。

### 前提条件

作業を始める前に以下を理解していることを推奨します：

#### EMQX の概念：

- [ルールエンジン](./rules.md)：MQTT メッセージからデータを抽出・変換するロジックを定義する仕組み。
- [データ統合](./data-bridges.md)：EMQX におけるコネクターと Sink の概念。

#### AWS の概念：

AWS S3 Tables に不慣れな場合は以下の用語を確認してください：

- **EC2**：AWS の仮想マシン（コンピュートインスタンス）サービス。
- **IAM**：AWS Identity and Access Management。インスタンスロールはそのインスタンス上で動作するプログラムに一時的な認証情報を発行可能。
- **IMDSv2**：EC2 のインスタンスメタデータサービス v2。トークンベースでより安全にメタデータや一時認証情報を取得。
- **Table Bucket**：S3 Tables で Iceberg ベースのテーブルデータとメタデータを格納する専用の S3 バケット。
- **Amazon Athena**：Amazon S3 に保存されたデータに対して SQL クエリを実行できるサーバーレスクエリエンジン。`CREATE TABLE` などの DDL 文もサポート。
- **Catalog**：Athena のメタデータコンテナで、データベース（ネームスペース）やテーブルを管理。
- **Database (Namespace)**：Catalog 配下の論理的なテーブルグループ。
- **Iceberg Table**：高性能でトランザクション対応のデータレイク用テーブルフォーマット。スキーマ進化、パーティションプルーニング、タイムトラベルクエリをサポート。

### デプロイ前提条件と認証情報の取得方法

S3 Tables コネクターは認証情報の取得方法を2通りサポートしています。EMQX のデプロイ環境に応じて選択してください：

- **オプション1：アクセスキーを手動設定**
  [コネクター作成時](#create-a-connector)に **Access Key ID** と **Secret Access Key** を入力します。これらの認証情報は対象の S3 Tables および Athena へのアクセス権限を持つ必要があります。ローカル、コンテナ、Kubernetes、非 AWS クラウド、またはインスタンスロールが割り当てられていない EC2 で適しています。

  IAM ユーザーのアクセスキーの作成・管理については [AWS のアクセスキー管理ドキュメント](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html)を参照してください。

- **オプション2：一時認証情報を自動取得（EC2 のみ）**
  EMQX を AWS EC2 インスタンス上で実行し、そのインスタンスに必要な権限を持つ IAM ロールが割り当てられている場合、コネクターの **Access Key ID** と **Secret Access Key** を空欄にできます。EMQX は IMDSv2 API を使い、そのロールに紐づく一時認証情報を自動取得します。

  EC2 インスタンスに IAM ロールを割り当てる方法は [AWS の IAM ロール for EC2 ドキュメント](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2.html)を参照してください。

::: tip 注意事項

- インスタンスロールに対象の S3 Tables（バケット／テーブル）および Athena への十分な権限があることを確認してください。そうでないと **Test Connectivity** が失敗します。
- 一時認証情報管理には EC2 インスタンスに割り当てた IAM ロールの利用を推奨します。EC2 以外やロール未割当の場合はオプション1でアクセスキーを手動入力してください。

:::

### S3 Tables バケットの準備

EMQX で Sink を作成する前に、AWS S3 Tables 側で MQTT データの保存先を準備します。準備内容は以下の通りです：

- 実際のデータファイルを格納する Table Bucket
- 関連テーブルを論理的にまとめる Namespace
- 構造化された MQTT データを受け取る Iceberg ベースの Table

1. AWS マネジメントコンソールにログインします。

2. S3 サービスに移動し、左ナビゲーションペインの **Table buckets** をクリックします。

3. **Create table bucket** をクリックし、テーブルバケット名（例：`mybucket`）を入力して **Create table bucket** をクリックします。

4. バケット作成後、そのバケット名をクリックしてテーブル一覧に移動します。

5. **Create table with Athena** をクリックします。ポップアップでネームスペースの入力を求められます。

6. **Create a namespace** を選択し、ネームスペース名を入力して作成を確定します。

7. ネームスペース作成後、再度 **Create table with Athena** をクリックします。

8. Iceberg テーブルのスキーマを定義します：

   - **Query table with Athena** をクリックし、クエリエディターを開きます。

     - **Catalog** セレクターで作成したバケットに対応するカタログ（例：`s3tablescatalog/mybucket`）を選択。
     - **Database** セレクターで先ほど作成したネームスペースを選択。

   - 以下の DDL を実行し、テーブルタイプが `ICEBERG` であることを指定してテーブルを作成します。例：

     ```sql
     CREATE TABLE testtable (
       c_str string,
       c_long int )
     TBLPROPERTIES ('table_type' = 'ICEBERG');
     ```

     これは EMQX からの構造化 MQTT データを格納する Iceberg ベースのテーブルを定義します。

9. テーブルが正常に作成されて空であることを確認するため、以下を実行します：

   ```sql
   select * from testtable
   ```

   ::: tip

   Athena で SQL を実行する前に、正しい Catalog と Database（ネームスペース）が選択されていることを必ず確認してください。これにより意図した S3 Table Bucket 内にテーブルが作成されます。

   :::

## コネクターの作成

S3 Tables Sink を追加する前に、対応するコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. コネクタータイプで **S3 Tables** を選択し、次へ進みます。

4. コネクター名を入力します。名前は英数字で始まり、英数字、ハイフン、アンダースコアを含めることができます。ここでは例として `my-s3-tables` と入力します。

5. 必要な接続情報を入力します：

   - **S3Tables ARN**：AWS コンソールの Table buckets セクションで確認できる S3 Table Bucket の Amazon Resource Name (ARN) を入力します。
   - **Access Key ID と Secret Access Key**（任意）：
     - **手動設定の場合**：S3 Tables と Athena へのアクセス権限を持つ IAM ユーザーまたはロールの認証情報を入力します。
     - **自動取得の場合**：EMQX が AWS EC2 インスタンス上で動作し、必要な権限を持つ IAM ロールが割り当てられていれば空欄にできます。EMQX が IMDSv2 経由で一時認証情報を取得します。詳細は [デプロイ前提条件と認証情報の取得方法](#デプロイ前提条件と認証情報の取得方法) を参照してください。
   - **Enable TLS**：S3 Tables への接続時に TLS はデフォルトで有効です。TLS 接続オプションの詳細は [TLS for External Resource Access](../../guides/network/overview.md#enable-tls-encryption-for-accessing-external-resources) を参照してください。
   - **Health Check Timeout**：S3 Tables との接続に対する自動ヘルスチェックのタイムアウト時間を指定します。

7. その他の設定はデフォルト値を使用します。

8. **Create** をクリックする前に、**Test Connectivity** を押してコネクターが S3 Tables サービスに接続できるかテスト可能です。

9. 最後に **Create** ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、次にルールと Sink を作成して S3 Tables へのデータ書き込みを指定します。

## Amazon S3 Tables Sink を使ったルールの作成

このセクションでは、EMQX でトピック `t/#` からのメッセージを処理し、処理結果を S3 Tables の `mybucket` バケットに書き込むルールの作成方法を示します。

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

   SQL に不慣れな場合は **SQL Examples** と **Enable Debug** をクリックしてルール SQL の結果を学習・テストできます。

   :::

   ::: tip

   出力フィールドは Iceberg テーブルのスキーマと一致させてください。必須カラムが欠落または誤記されるとテーブルへのデータ追加に失敗します。

   :::

4. アクションを追加し、**Action Type** ドロップダウンから `S3 Tables` を選択します。アクションのドロップダウンはデフォルトの `create action` のままか、既存の S3 Tables アクションを選択します。ここでは新規 Sink を作成してルールに追加します。

5. Sink 名と任意で説明を入力します。

6. **Connector** ドロップダウンから先に作成した `my-s3-tables` コネクターを選択します。新規コネクターを素早く定義したい場合はドロップダウン横の **Create** ボタンをクリックしてください。設定パラメーターは [コネクターの作成](#コネクターの作成) を参照してください。

7. Sink の設定を行います：

   - **Namespace**：テーブルが存在するネームスペース。複数セグメントの場合はドット区切りで指定（例：`my.name.space`）。
   - **Table**：データを追加する Iceberg テーブル名（例：`testtable`）。
   - **Max Records**：S3 に書き込む前にバッチ処理する最大レコード数。到達すると即座にバッチをフラッシュしてアップロードします。
   - **Time Interval**：Max Records に達していなくても、指定ミリ秒経過後にバッチをフラッシュします。
   - **Data File Format**：S3 に保存するバッチデータファイルのフォーマット。指定可能な値：
     - `avro`：（デフォルト）Avro フォーマットで保存。行ベースでストリーミングデータやスキーマ進化に適します。
     - `parquet`：Apache Parquet フォーマットで保存。列ベースで大規模分析クエリに最適化されています。

8. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

9. **Advanced Settings** を展開し、必要に応じて詳細設定を行います。詳細は [Advanced Settings](#advanced-settings) を参照してください。

10. 残りの設定はデフォルト値を使用し、**Create** ボタンをクリックして Sink 作成を完了します。作成成功後はルール作成画面に戻り、新しい Sink がルールアクションに追加されます。

11. ルール作成画面で **Create** ボタンをクリックし、ルール全体の作成を完了します。

これでルールの作成が完了しました。**Rules** ページで新規ルールを確認でき、**Actions (Sink)** タブで新しい S3 Tables Sink を確認できます。

また、**Integration** -> **Flow Designer** でトポロジーを表示可能です。トポロジーはトピック `t/#` のメッセージがルール `my_rule` によって解析され、S3 Tables に書き込まれる流れを視覚的に示します。

## ルールのテスト

このセクションでは、S3 Tables Sink を設定したルールのテスト方法を示します。

1. MQTTX を使い、トピック `t/1` にメッセージをパブリッシュします：

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "str": "hello S3 Tables", "int": 123 }'
   ```

   このメッセージは `payload.str` と `payload.int` フィールドを含み、ルール SQL とテーブルスキーマに合致しています。

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

このセクションでは、S3 Tables Sink の詳細設定オプションについて説明します。ダッシュボードで Sink 設定時に **Advanced Settings** を展開し、用途に応じて以下のパラメーターを調整可能です。

| フィールド名                      | 説明                                                         | デフォルト値    |
| -------------------------------- | ------------------------------------------------------------ | -------------- |
| **Min Part Size**                | マルチパートアップロードの最小パートサイズ。<br/>このサイズに達するまでアップロードデータはメモリに蓄積されます。 | `5` MB         |
| **Max Part Size**                | マルチパートアップロードの最大パートサイズ。<br/>S3 アップローダーはこのサイズを超えるパートのアップロードを試みません。 | `5` GB         |
| **Buffer Pool Size**             | EMQX と S3 Tables 間のデータフローを管理するバッファワーカープロセス数。<br/>これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへの送信を最適化しスムーズなデータ伝送を保証します。 | `16`           |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）。<br/>リクエストがこの TTL を超えてバッファに滞留するか、送信後に S3 Tables からの応答やアックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45` 秒        |
| **Health Check Interval**        | Sink が S3 Tables との接続に対して自動ヘルスチェックを行う間隔（秒）。 | `15` 秒        |
| **Health Check Interval Jitter** | ヘルスチェック間隔に加える一様ランダム遅延（ミリ秒）。<br/>複数ノードが同時にヘルスチェックを開始する確率を減らすため、同じコネクターを共有する複数のアクションやソースで有効にします。 | `0` ミリ秒     |
| **Health Check Timeout**         | コネクターが S3 Tables との接続に対して自動ヘルスチェックを行う際のタイムアウト時間。 | `60` 秒        |
| **Max Buffer Queue Size**        | S3 Tables Sink の各バッファワーカーがバッファリング可能な最大バイト数。<br/>バッファワーカーはデータを一時保持し、効率的にデータストリームを処理します。システム性能やデータ伝送要件に応じて調整してください。 | `256` MB       |
| **Batch Size**                   | EMQX から S3 Tables へ一度に転送するデータバッチの最大レコード数。<br/>サイズを調整することでデータ転送の効率と性能を最適化可能です。`1` に設定するとレコードを個別に送信し、バッチ処理しません。 | 1000           |
| **Query Mode**                   | メッセージ送信の最適化のため、`synchronous` と `asynchronous` のリクエストモードを選択可能。<br/>非同期モードでは S3 Tables への書き込みが MQTT メッセージのパブリッシュをブロックしませんが、クライアントがメッセージを受信してから S3 Tables に到達するまでに遅延が生じる可能性があります。 | `Asynchronous` |
| **In-flight  Window**            | 未応答または未アックのリクエスト数の最大値。<br/>Sink と S3 Tables 間の通信における同時進行中のリクエスト数を制御します。<br/>`Request Mode` が `asynchronous` の場合に特に重要で、同一 MQTT クライアントからのメッセージを厳密に順序処理したい場合は `1` に設定してください。 | `100`          |
