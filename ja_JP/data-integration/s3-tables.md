# Amazon S3 Tables への MQTT データ取り込み

[Amazon S3 Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html) は、分析ワークロードに最適化された専用のストレージソリューションです。Apache Iceberg フォーマットで IoT センサーの読み取り値などの表形式データを高性能かつスケーラブルかつ安全に保存できます。

EMQX は Amazon S3 Tables とのシームレスな連携をサポートしており、MQTT メッセージを効率的に S3 テーブルバケットに保存可能です。この連携により、柔軟でスケーラブルな IoT データストレージが実現し、Amazon Athena、Amazon Redshift、Amazon EMR などの AWS サービスを利用した高度な分析や処理が容易になります。

本ページでは、EMQX と Amazon S3 Tables 間のデータ統合の詳細な紹介と、ルールおよび Sink 作成の実践的な手順を説明します。

## 動作概要

EMQX の Amazon S3 Tables 連携は標準機能として提供されています。この連携は EMQX のルールエンジンと S3 Tables Sink を活用し、MQTT メッセージを変換して Apache Iceberg フォーマットのテーブルに直接ストリームし、S3 テーブルバケットに保存します。これにより長期保存と下流分析が可能になります。

典型的な IoT シナリオでは：

- **EMQX** は MQTT ブローカーとして機能し、デバイスの接続管理、メッセージルーティング、データ処理を担当します。
- **Amazon S3 Tables** は MQTT メッセージデータを表形式で耐久的かつクエリ可能なストレージとして提供します。
- **Amazon Athena** は Iceberg テーブルの定義と保存データに対する SQL クエリ実行に使用されます。

![emqx-integration-s3-tables](./assets/emqx-integration-s3-tables.png)

ワークフローは以下の通りです：

1. **デバイスの EMQX への接続**：IoT デバイスが MQTT 経由で EMQX に接続し、テレメトリデータをパブリッシュし始めます。
2. **メッセージルーティングとルールマッチング**：EMQX は組み込みのルールエンジンを使い、受信した MQTT メッセージを定義済みトピックにマッチさせ、特定のフィールドや値を抽出します。
3. **データ変換**：EMQX のルールでメッセージペイロードをフィルタリング、変換、または拡張し、ターゲットの Iceberg テーブルのスキーマに合わせます。
4. **Amazon S3 Tables への書き込み**：ルールが S3 Tables Sink アクションをトリガーし、変換済みデータをバッチ処理して Iceberg 互換の書き込み API を使い Amazon S3 Tables に送信します。データは Iceberg テーブルのパーティション下に Parquet ファイルとして永続化されます。
5. **クエリと分析**：取り込まれたデータは Amazon Athena でクエリ可能で、他のデータセットと結合したり、Redshift Spectrum、Amazon EMR、Presto、Trino などのサードパーティ分析エンジンで分析できます。

## 特長とメリット

EMQX で Amazon S3 Tables データ統合を利用することで、以下の特長と利点が得られます：

- **リアルタイムストリーム処理**：EMQX のルールエンジンにより、MQTT メッセージをリアルタイムに抽出・変換・条件付きルーティングし、S3 Tables へ配信できます。
- **Iceberg ベースの S3 ストレージ**：メッセージは Apache Iceberg テーブルに書き込まれ、従来のデータベース不要で SQL ライクなアクセスが可能です。
- **分析ツールとの簡単連携**：データが S3 Tables にあるため、Amazon Athena（SQL）、Amazon EMR、Redshift Spectrum、Presto、Trino、Snowflake などでクエリや分析が可能です。
- **柔軟かつコスト効率の良いストレージ**：Amazon S3 は高耐久で低コストなオブジェクトストレージを提供し、アーカイブ、コンプライアンス、時系列分析に最適です。

## はじめる前に

このセクションでは、EMQX で Amazon S3 Tables Sink を作成するための準備事項を紹介します。

### 前提条件

作業を進める前に、以下の内容に慣れていることを推奨します：

#### EMQX の概念：

- [ルールエンジン](./rules.md)：MQTT メッセージからデータを抽出・変換するロジックとしてのルールの理解。
- [データ統合](./data-bridges.md)：EMQX のコネクターと Sink の概念理解。

#### AWS の概念：

AWS S3 Tables に不慣れな場合は、以下の主要用語を確認してください：

- **EC2**：AWS の仮想マシンサービス（コンピュートインスタンス）。
- **IAM**：AWS Identity and Access Management。インスタンスロールはそのインスタンス上で動作するプログラムに一時的な認証情報を発行可能。
- **IMDSv2**：EC2 のインスタンスメタデータサービス v2。トークンベースでより安全にメタデータや一時認証情報を取得。
- **Table Bucket**：S3 Tables で Iceberg ベースのテーブルデータとメタデータを格納する専用の S3 バケット。
- **Amazon Athena**：Amazon S3 に保存されたデータに対して直接 SQL クエリを実行できるサーバーレスクエリエンジン。DDL ステートメント（`CREATE TABLE` など）もサポート。
- **Catalog**：Athena でデータベース（ネームスペース）やテーブルを管理するメタデータコンテナ。
- **Database (Namespace)**：Catalog 配下の論理的なテーブルグループ。
- **Iceberg Table**：高性能かつトランザクショナルなデータレイク向けテーブルフォーマット。スキーマ進化、パーティションプルーニング、タイムトラベルクエリをサポート。

### デプロイ前提条件と認証情報の取得方法

S3 Tables コネクターは認証情報の取得方法を2通りサポートしています。EMQX のデプロイ環境に応じて選択してください：

- **オプション1：アクセスキーを手動設定する場合**  
  [コネクター作成](#create-a-connector)時に **Access Key ID** と **Secret Access Key** を入力します。これらの認証情報は対象の S3 Tables と Athena への必要な権限を持つ必要があります。ローカル環境、コンテナ、Kubernetes、非 AWS クラウド、またはインスタンスロールが割り当てられていない EC2 に適しています。  

  IAM ユーザーのアクセスキーの作成・管理方法は [AWS のアクセスキー管理ドキュメント](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html)を参照してください。

- **オプション2：一時認証情報を自動取得（EC2 のみ）**  
  EMQX が AWS EC2 インスタンス上で動作し、そのインスタンスに必要な権限を持つ IAM ロールが割り当てられている場合、コネクターの **Access Key ID** と **Secret Access Key** を空欄にできます。EMQX は IMDSv2 API を使い、そのロールに紐づく一時認証情報を取得します。  

  EC2 インスタンスに IAM ロールを割り当てる方法は [AWS の EC2 向け IAM ロールドキュメント](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2.html)を参照してください。

::: tip 注意事項

- インスタンスロールに対象の S3 Tables（バケット/テーブル）と Athena への十分な権限があることを確認してください。そうでない場合、**Test Connectivity** が失敗する可能性があります。
- 一時認証情報の管理には EC2 インスタンスに割り当てた IAM ロールの利用を推奨します。EC2 以外やロール未割当の場合はオプション1の手動設定を利用してください。

:::

### S3 Tables バケットの準備

EMQX で Sink を作成する前に、AWS S3 Tables に MQTT データの保存先を準備します。準備するものは以下の通りです：

- 実際のデータファイルを格納する Table Bucket
- 関連テーブルを論理的にグループ化する Namespace
- 構造化された MQTT データを受け取る Iceberg ベースの Table

1. AWS マネジメントコンソールにログインします。

2. S3 サービスに移動し、左ナビゲーションペインの **Table buckets** をクリックします。

3. **Create table bucket** をクリックし、テーブルバケット名（例：`mybucket`）を入力して **Create table bucket** をクリックします。

4. バケット作成後、そのバケットをクリックして Tables 一覧に移動します。

5. **Create table with Athena** をクリックすると、Namespace の入力を求めるポップアップが表示されます。

6. **Create a namespace** を選択し、ネームスペース名を入力して作成を確定します。

7. ネームスペース作成後、再度 **Create table with Athena** をクリックします。

8. Iceberg テーブルのスキーマを定義します：

   - **Query table with Athena** をクリックし、**Query editor** で以下を設定：

     - **Catalog** セレクターでバケット名に対応するカタログ（例：`s3tablescatalog/mybucket`）を選択。
     - **Database** セレクターで先ほど作成したネームスペースを選択。

   - 以下の DDL を実行してテーブルを作成し、テーブルタイプが `ICEBERG` であることを指定します。例：

     ```sql
     CREATE TABLE testtable (
       c_str string,
       c_long int )
     TBLPROPERTIES ('table_type' = 'ICEBERG');
     ```

     これにより、EMQX からの構造化された MQTT データを格納する Iceberg ベースのテーブルが定義されます。

9. テーブル作成を検証します。テーブルが正常に作成され、空であることを確認するために以下を実行します：

   ```sql
   select * from testtable
   ```

   ::: tip

   Athena で SQL を実行する前に、正しい Catalog と Database（ネームスペース）が選択されていることを必ず確認してください。これにより、意図した S3 テーブルバケット内にテーブルが作成されます。

   :::

## コネクターの作成

S3 Tables Sink を追加する前に、対応するコネクターを作成します。

1. ダッシュボードの **Integration** -> **Connector** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. コネクタータイプで **S3 Tables** を選択し、次へ進みます。

4. コネクター名を入力します。名前は英数字で始まり、英数字、ハイフン、アンダースコアを含めることができます。例として `my-s3-tables` を入力します。

5. 必要な接続情報を入力します：

   - **S3Tables ARN**：S3 テーブルバケットの Amazon Resource Name (ARN) を入力します。AWS コンソールの Table buckets セクションで確認可能です。
   - **Access Key ID と Secret Access Key**（任意）：
     - **手動設定の場合**：S3 Tables と Athena へのアクセス権限を持つ IAM ユーザーまたはロールの認証情報を入力します。
     - **自動取得の場合**：EMQX が AWS EC2 インスタンス上で動作し、必要な権限を持つ IAM ロールが割り当てられている場合は空欄にできます。EMQX は IMDSv2 を使い一時認証情報を自動取得します。詳細は [デプロイ前提条件と認証情報の取得方法](#デプロイ前提条件と認証情報の取得方法) を参照してください。
   - **Enable TLS**：S3 Tables への接続時は TLS がデフォルトで有効です。TLS 接続オプションの詳細は [外部リソースアクセスの TLS 有効化](../network/overview.md#enable-tls-encryption-for-accessing-external-resources) を参照してください。
   - **Health Check Timeout**：S3 Tables との接続の自動ヘルスチェックのタイムアウト時間を指定します。

7. その他の設定はデフォルト値を使用します。

8. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが S3 Tables サービスに接続可能かテストできます。

9. 最後に **Create** ボタンをクリックしてコネクター作成を完了します。

これでコネクター作成が完了し、次にルールと Sink を作成して S3 Tables へのデータ書き込みを指定します。

## Amazon S3 Tables Sink を使ったルール作成

このセクションでは、EMQX でソース MQTT トピック `t/#` からメッセージを処理し、処理結果を S3 Tables の `mybucket` バケットに書き込むルールの作成方法を示します。

1. ダッシュボードの **Integration** -> **Rules** ページに移動します。

2. 右上の **Create** ボタンをクリックします。

3. ルール ID に `my_rule` を入力し、SQL エディタに以下のルール SQL を入力します：

   ```sql
   SELECT
     payload.str as c_str,
     payload.int as c_long
   FROM
       "t/#"
   ```

   ::: tip

   SQL に不慣れな場合は、**SQL Examples** と **Enable Debug** をクリックしてルール SQL の学習や結果のテストが可能です。

   :::

   ::: tip

   出力フィールドは Iceberg テーブルのスキーマと一致させてください。必須カラムが欠落または名前が異なると、データのテーブルへの追加に失敗する可能性があります。

   :::

4. アクションを追加し、**Action Type** ドロップダウンから `S3 Tables` を選択します。アクションのドロップダウンはデフォルトの `create action` のままにするか、既存の S3 Tables アクションを選択できます。ここでは新しい Sink を作成し、ルールに追加します。

5. Sink 名と任意の説明を入力します。

6. **Connector** ドロップダウンから先ほど作成した `my-s3-tables` コネクターを選択します。新しいコネクターをすばやく定義したい場合は、ドロップダウン横の **Create** ボタンをクリックしてください。設定パラメータは [コネクターの作成](#コネクターの作成) を参照してください。

7. Sink 設定を構成します：

   - **Namespace**：テーブルが存在するネームスペース。複数セグメントの場合はドット区切り（例：`my.name.space`）を使用。
   - **Table**：データを追加する Iceberg テーブル名（例：`testtable`）。
   - **Max Records**：S3 へ書き込む前にバッチ処理する最大レコード数。到達すると即座にバッチをフラッシュしてアップロード。
   - **Time Interval**：Max Records に達しなくても、指定ミリ秒経過後にバッチをフラッシュする最大待機時間。
   - **Data File Format**：S3 に保存するバッチ化された MQTT メッセージのデータファイル形式。サポート値：
     - `avro`：（デフォルト）Avro 形式。行ベースでストリーミングデータやスキーマ進化に最適。
     - `parquet`：Apache Parquet 形式。列ベースで大規模分析クエリに最適化。

8. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

9. **Advanced Settings** を展開し、必要に応じて詳細設定を行います（任意）。詳細は [詳細設定](#advanced-settings) を参照してください。

10. 残りの設定はデフォルト値を使用し、**Create** ボタンをクリックして Sink 作成を完了します。作成成功後、ルール作成画面に戻り、新しい Sink がルールアクションに追加されます。

11. ルール作成画面で **Create** ボタンをクリックし、ルール作成全体を完了します。

これでルールの作成が完了しました。**Rules** ページで新規ルールを確認でき、**Actions (Sink)** タブで新しい S3 Tables Sink を確認できます。

また、**Integration** -> **Flow Designer** を開くとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、S3 Tables に書き込まれる流れが視覚的に確認できます。

## ルールのテスト

このセクションでは、S3 Tables Sink を設定したルールのテスト方法を示します。

1. MQTTX を使い、トピック `t/1` にメッセージをパブリッシュします：

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "str": "hello S3 Tables", "int": 123 }'
   ```

   このメッセージは `payload.str` と `payload.int` フィールドを含み、ルール SQL とテーブルスキーマに一致しています。

2. **Rules** ページでルールのメトリクスと Sink の状態を監視します。新しい受信メッセージと送信メッセージがそれぞれ1件ずつあるはずです。

3. Athena のクエリエディタを開き、正しい **Catalog**（例：`s3tablescatalog/mybucket`）と **Database**（ネームスペース）が選択されていることを確認します。

4. 以下の SQL クエリを実行します：

   ```sql
   SELECT * FROM testtable
   ```

   以下のような行が表示されるはずです：

   | c_str           | c_long |
   | --------------- | ------ |
   | hello S3 Tables | 123    |

## 詳細設定

このセクションでは、S3 Tables Sink の詳細設定オプションについて説明します。ダッシュボードの Sink 設定画面で **Advanced Settings** を展開すると、以下のパラメータをニーズに応じて調整可能です。

| フィールド名                     | 説明                                                         | デフォルト値     |
| -------------------------------- | ------------------------------------------------------------ | --------------- |
| **Min Part Size**                | マルチパートアップロードの最小パートサイズ。<br>このサイズに達するまでアップロードデータはメモリに蓄積されます。 | `5` MB          |
| **Max Part Size**                | マルチパートアップロードの最大パートサイズ。<br>S3 アップローダーはこのサイズを超えるパートをアップロードしません。 | `5` GB          |
| **Buffer Pool Size**             | EMQX と S3 Tables 間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保持・処理し、パフォーマンス最適化とスムーズなデータ送信を支えます。 | `16`            |
| **Request TTL**                  | リクエストの有効期限（秒）を指定します。リクエストがバッファに入ってからの経過時間がこの値を超えるか、送信後に S3 Tables からの応答やアックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45` 秒         |
| **Health Check Interval**        | Sink が S3 Tables との接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒         |
| **Health Check Interval Jitter** | 複数ノードが同時にヘルスチェックを開始するのを防ぐため、基本間隔に加える一様ランダム遅延（ミリ秒）です。複数のアクションやソースが同一コネクターを共有する場合に有効です。 | `0` ミリ秒      |
| **Health Check Timeout**         | コネクターが S3 Tables との接続ヘルスチェックを行う際のタイムアウト時間を指定します。 | `60` 秒         |
| **Max Buffer Queue Size**        | S3 Tables Sink の各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保持し、効率的なデータストリーム処理を実現します。システム性能やデータ送信要件に応じて調整してください。 | `256` MB        |
| **Batch Size**                   | EMQX から S3 Tables へ一度に送信するデータバッチの最大レコード数を指定します。サイズ調整により転送効率と性能を最適化可能です。`1` に設定するとバッチ化せず個別送信となります。 | 1000            |
| **Query Mode**                   | `synchronous` または `asynchronous` のリクエストモードを選択し、メッセージ送信を要件に合わせて最適化します。非同期モードでは S3 Tables への書き込みが MQTT パブリッシュ処理をブロックしませんが、クライアントがメッセージ受信後に S3 Tables への到達が遅れる可能性があります。 | `Asynchronous`  |
| **In-flight  Window**            | 「インフライトキューリクエスト」は開始済みで応答やアックをまだ受け取っていないリクエストを指します。この設定は Sink と S3 Tables 間の同時インフライトリクエストの最大数を制御します。<br>**Request Mode** が `asynchronous` の場合、同一 MQTT クライアントからのメッセージを厳密に順次処理したい場合はこの値を `1` に設定してください。 | `100`           |
