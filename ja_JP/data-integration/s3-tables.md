# Amazon S3 TablesへのMQTTデータ取り込み

[Amazon S3 Tables](https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-tables.html)は、分析ワークロードに最適化された専用のストレージソリューションです。IoTセンサーの計測値などの表形式データをApache Icebergフォーマットで高性能かつスケーラブル、かつ安全に保存できます。

EMQXはAmazon S3 Tablesとのシームレスな連携をサポートし、MQTTメッセージを効率的にS3テーブルバケットに格納できます。この連携により、柔軟でスケーラブルなIoTデータストレージが可能となり、Amazon Athena、Amazon Redshift、Amazon EMRなどのAWSサービスを活用した高度な分析や処理が容易になります。

本ページでは、EMQXとAmazon S3 Tables間のデータ統合について詳しく解説し、ルールおよびSinkの作成方法について実践的なガイダンスを提供します。

## 動作概要

EMQXのAmazon S3 Tables連携は標準搭載機能です。この連携はEMQXのルールエンジンとS3 Tables Sinkを活用し、MQTTメッセージを変換してApache Iceberg形式のテーブルに直接ストリーミングし、S3テーブルバケットに保存して長期保管および下流分析を実現します。

典型的なIoTシナリオでは以下のように動作します：

- **EMQX**はMQTTブローカーとして機器の接続管理、メッセージルーティング、データ処理を担当します。
- **Amazon S3 Tables**はMQTTメッセージデータを表形式で耐久的かつクエリ可能なストレージとして提供します。
- **Amazon Athena**はIcebergテーブルの定義や格納データに対するSQLクエリの実行に使用されます。

![emqx-integration-s3-tables](./assets/emqx-integration-s3-tables.png)

ワークフローは以下の通りです：

1. **デバイスのEMQX接続**：IoTデバイスがMQTTでEMQXに接続し、テレメトリデータをパブリッシュします。
2. **メッセージルーティングとルールマッチング**：EMQXのルールエンジンが受信したMQTTメッセージを定義済みトピックに照合し、特定のフィールドや値を抽出します。
3. **データ変換**：EMQXのルールでペイロードのフィルタリング、変換、付加処理を行い、ターゲットのIcebergテーブルのスキーマに合わせます。
4. **Amazon S3 Tablesへの書き込み**：ルールがS3 Tables Sinkアクションをトリガーし、変換済みデータをバッチ処理してIceberg互換の書き込みAPIでS3 Tablesに送信します。データはIcebergテーブルのパーティション下にParquetファイルとして永続化されます。
5. **クエリと分析**：取り込まれたデータはAmazon Athenaでクエリ可能となり、他のデータセットと結合したり、Redshift Spectrum、Amazon EMR、PrestoやTrinoなどのサードパーティ分析エンジンで分析できます。

## 特長とメリット

EMQXのAmazon S3 Tablesデータ統合を利用することで、以下の特長と利点を得られます：

- **リアルタイムストリーム処理**：EMQXのルールエンジンにより、MQTTメッセージをリアルタイムに抽出・変換・条件付きルーティングしてS3 Tablesに届けられます。
- **IcebergベースのS3ストレージ**：メッセージはApache Icebergテーブルに書き込まれ、従来のデータベース不要でSQLライクなアクセスが可能です。
- **分析ツールとの簡単連携**：S3 Tablesに格納後はAmazon Athena（SQL）、Amazon EMR、Redshift Spectrum、Presto、Trino、Snowflakeなどでクエリや分析が行えます。
- **柔軟かつコスト効率の高いストレージ**：Amazon S3は高耐久かつ低コストのオブジェクトストレージを提供し、アーカイブ、コンプライアンス、時系列分析に最適です。

## はじめる前に

このセクションでは、EMQXでAmazon S3 Tables Sinkを作成するための準備について説明します。

### 前提条件

以下の内容に慣れていることを推奨します：

#### EMQXの概念：

- [ルールエンジン](./rules.md)：MQTTメッセージからデータを抽出・変換するロジックの定義方法を理解する。
- [データ統合](./data-bridges.md)：EMQXのコネクターとSinkの概念を理解する。

#### AWSの概念：

AWS S3 Tablesが初めての場合は以下の用語を確認してください：

- **EC2**：AWSの仮想マシンサービス（コンピュートインスタンス）。
- **IAM**：AWS Identity and Access Management。インスタンスロールはそのインスタンス上で動作するプログラムに一時的な認証情報を発行可能。
- **IMDSv2**：EC2のインスタンスメタデータサービスv2。トークンベースでより安全にメタデータや一時認証情報を取得。
- **Table Bucket**：S3 TablesでIcebergベースのテーブルデータとメタデータを保存する専用のS3バケット。
- **Amazon Athena**：Amazon S3に保存されたデータに対して直接SQLクエリを実行できるサーバーレスクエリエンジン。DDLステートメント（`CREATE TABLE`など）を使ってスキーマや構造を定義可能。
- **Catalog**：Athenaのメタデータコンテナで、データベース（ネームスペース）やテーブルを管理。
- **Database (Namespace)**：Catalog内の論理的なテーブルグループ。
- **Iceberg Table**：高性能でトランザクショナルなデータレイク向けテーブルフォーマット。スキーマ進化、パーティションプルーニング、タイムトラベルクエリをサポート。

### デプロイ前提条件と認証情報の取得方法

S3 Tablesコネクターは認証情報を取得する方法が2通りあります。EMQXのデプロイ環境に応じて選択してください：

- **オプション1：アクセスキーを手動設定**
  コネクター作成時に**Access Key ID**と**Secret Access Key**を入力します。これらの認証情報は対象のS3 TablesおよびAthenaへの必要な権限を持つ必要があります。ローカル環境、コンテナ、Kubernetes、非AWSクラウド、またはインスタンスロールが割り当てられていないEC2で適用可能です。

  IAMユーザーのアクセスキーの作成・管理については[AWSのアクセスキー管理ドキュメント](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html)を参照してください。

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

     ```sql
     CREATE TABLE testtable (
       c_str string,
       c_long int )
     TBLPROPERTIES ('table_type' = 'ICEBERG');
     ```

     これはEMQXからの構造化MQTTデータを格納するIcebergテーブルを定義します。

9. テーブルが正常に作成されて空であることを確認するため、以下のクエリを実行します：

   ```sql
   select * from testtable
   ```

   ::: tip

   AthenaでSQLを実行する前に、正しいCatalogおよびDatabase（Namespace）が選択されていることを必ず確認してください。これにより、意図したS3 Table Bucket内にテーブルが作成されます。

   :::

## コネクターの作成

S3 Tables Sinkを追加する前に、対応するコネクターを作成します。

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

## Amazon S3 Tables Sinkを使ったルールの作成

このセクションでは、ソースMQTTトピック`t/#`からメッセージを処理し、処理結果をS3 Tablesの`mybucket`バケットに書き込むルール作成手順を示します。

1. ダッシュボードの**Integration** -> **Rules**ページに移動します。

2. 右上の**Create**ボタンをクリックします。

3. ルールIDに`my_rule`を入力し、SQLエディターに以下のルールSQLを入力します：

   ```sql
   SELECT
     payload.str as c_str,
     payload.int as c_long
   FROM
       "t/#"
   ```

   ::: tip

   SQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックしてルールSQLの学習や結果のテストが可能です。

   :::

   ::: tip

   出力フィールドがIcebergテーブルのスキーマと一致していることを必ず確認してください。必須カラムの欠落や誤った名前はデータのテーブルへの追加失敗を招きます。

   :::

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

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "str": "hello S3 Tables", "int": 123 }'
   ```

   このメッセージは`payload.str`と`payload.int`フィールドを含み、ルールSQLおよびテーブルスキーマと一致しています。

2. **Rules**ページでルールのメトリクスとSinkの状態を監視します。新規の受信メッセージと送信メッセージがそれぞれ1件ずつ増えているはずです。

3. Athenaのクエリエディターを開き、正しい**Catalog**（例：`s3tablescatalog/mybucket`）と**Database**（Namespace）が選択されていることを確認します。

4. 以下のSQLを実行します：

   ```sql
   SELECT * FROM testtable
   ```

   以下のような行が表示されるはずです：

   | c_str           | c_long |
   | --------------- | ------ |
   | hello S3 Tables | 123    |

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
