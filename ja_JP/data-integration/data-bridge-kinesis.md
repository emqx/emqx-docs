# Amazon Kinesis への MQTT データストリーム

[AWS Kinesis](https://aws.amazon.com/cn/kinesis/) は、AWS 上で提供されるフルマネージドのリアルタイムストリーミングデータ処理サービスであり、ストリーミングデータの収集、処理、分析を容易にします。あらゆる規模のストリーミングデータをリアルタイムかつ経済的かつ効率的に処理でき、高い柔軟性を持ち、数十万のソースからの大量ストリーミングデータを低レイテンシで処理可能です。

EMQX は [Amazon Kinesis Data Streams](https://aws.amazon.com/kinesis/data-streams/) とのシームレスな連携をサポートしており、大量の IoT デバイスを接続してリアルタイムのメッセージ収集と送信を実現します。このデータ統合を通じて、Amazon Kinesis Data Streams に接続し、リアルタイムのデータ分析や複雑なストリーム処理を行うことが可能です。

本ページでは、EMQX と Amazon Kinesis 間のデータ統合について包括的に紹介し、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作概要

Amazon Kinesis とのデータ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Amazon Kinesis とシームレスに連携させ、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう設計されています。

![emqx-integration-aws](./assets/emqx-integration-aws.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Amazon Kinesis に転送します。全体の流れは以下の通りです：

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンを用いて、特定のトピックにマッチする MQTT メッセージを処理します。ルールエンジンは対応するルールに基づき、データ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報の付加などを行います。
3. **Amazon Kinesis への転送**：ルールによってトリガーされるアクションでメッセージを Amazon Kinesis に転送します。パーティションキーや書き込み先のデータストリーム、メッセージフォーマットをカスタマイズ可能で、柔軟なデータ統合が可能です。

MQTT メッセージデータが Amazon Kinesis に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です：

- リアルタイムデータ処理と分析：Amazon Kinesis の強力なデータ処理・分析ツールやストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、価値あるインサイトや意思決定支援を得られます。
- イベント駆動型機能：Amazon のイベント処理をトリガーし、動的かつ柔軟な機能の起動や処理を実現します。
- データの保存と共有：メッセージデータを Amazon Kinesis のストレージサービスに送信し、大量データの安全な保存・管理を行います。これにより他の Amazon サービスと連携してデータの共有や分析が可能となり、多様なビジネスニーズに対応できます。

## 特長とメリット

EMQX と AWS Kinesis Data Streams 間のデータ統合は、以下の機能と利点をビジネスにもたらします：

- **信頼性の高いデータ送信と順序保証**：EMQX と AWS Kinesis Data Streams はいずれも信頼性の高いデータ送信機構を備えています。EMQX は MQTT プロトコルを通じてメッセージの確実な送信を保証し、AWS Kinesis Data Streams はパーティションとシーケンス番号を用いてメッセージの順序を保証します。これにより、デバイスから送信されたメッセージが正確に目的地に届き、正しい順序で処理されることを確実にします。
- **リアルタイムデータ処理**：デバイスからの高頻度データは、EMQX のルール SQL による事前のリアルタイム処理を経て、MQTT メッセージのフィルタリング、抽出、付加、変換が容易に行えます。AWS Kinesis Data Streams へデータ送信後は、AWS Lambda や AWS 管理の Apache Flink と組み合わせてさらなるリアルタイム分析が可能です。
- **弾力的なスケーラビリティ対応**：EMQX は数百万の IoT デバイス接続を容易に実現し、弾力的なスケーラビリティを提供します。一方、AWS Kinesis Data Streams はオンデマンドの自動リソース割り当てと拡張を行います。両者を組み合わせたアプリケーションは接続数やデータ量の増加に応じてスケールし、ビジネスの成長に継続的に対応できます。
- **永続的なデータ保存**：AWS Kinesis Data Streams は永続的なデータ保存機能を提供し、毎秒数百万件のデバイスデータストリームを信頼性高く保存します。必要に応じて過去データの取得が可能で、オフライン分析や処理にも対応します。

AWS Kinesis Data Streams を利用したストリーミングデータパイプラインの構築は、EMQX と AWS プラットフォームの統合の難易度を大幅に低減し、ユーザーにより豊富で柔軟なデータ処理ソリューションを提供します。これにより、EMQX ユーザーは AWS 上で機能的に充実し高性能なデータ駆動型アプリケーションを構築できます。

## はじめる前に

本節では、Amazon Kinesis データ統合の作成を開始する前に必要な準備事項について説明します。Kinesis サービスの設定やデータストリームサービスのエミュレーション方法も含みます。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### Amazon Kinesis Data Streams でのストリーム作成

以下の手順で AWS マネジメントコンソールからストリームを作成します（詳細は [こちらのチュートリアル](https://docs.aws.amazon.com/streams/latest/dev/how-do-i-create-a-stream.html) を参照）。

1. AWS マネジメントコンソールにサインインし、[Kinesis コンソール](https://console.aws.amazon.com/kinesis) を開きます。

2. ナビゲーションバーのリージョンセレクターを展開し、リージョンを選択します。

3. **Create data stream** を選択します。

4. **Create Kinesis stream** ページでデータストリーム名を入力し、**On-demand** キャパシティモードを選択します。

### Amazon Kinesis Data Streams のローカルエミュレーション

開発やテストを容易にするため、[LocalStack](https://localstack.cloud/) を利用して Amazon Kinesis Data Streams サービスをローカルでエミュレートできます。LocalStack により、リモートクラウドプロバイダーに接続せずにローカルマシン上で AWS アプリケーションを完全に実行可能です。

1. Docker イメージを使ってインストール・起動します：

   ```bash
   # LocalStack Docker イメージをローカルで起動
   docker run --name localstack -p '4566:4566' -e 'KINESIS_LATENCY=0' -d localstack/localstack:2.1
   
   # コンテナにアクセス
   docker exec -it localstack bash
   ```

2. シャード数 1 のストリーム `my_stream` を作成します：

   ```bash
   awslocal kinesis create-stream --stream-name "my_stream" --shard-count 1
   ```

## コネクターの作成

本節では、Sink を Amazon Kinesis Data Streams サービスに接続するためのコネクター作成方法を説明します。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **Amazon Kinesis** を選択し、**Next** をクリックします。

4. **Configuration** ステップで以下を設定します：

   - コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_kinesis`。
   - **Amazon Kinesis Endpoint**：Kinesis サービスの [エンドポイント](https://docs.aws.amazon.com/general/latest/gr/ak.html) を入力します。LocalStack を利用する場合は `http://localhost:4566` を入力してください。
   - **AWS Access Key ID**：[アクセスキーID](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html) を入力します。LocalStack 利用時は任意の値で構いません。
   - **AWS Secret Access Key**：[シークレットアクセスキー](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html) を入力します。LocalStack 利用時は任意の値で構いません。

5. **Create** をクリックする前に、**Test Connectivity** を押してコネクターが Amazon Kinesis Data Streams に接続可能かテストできます。

6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックしてルールと Sink の作成に進めます。詳細は [Amazon Kinesis Sink を使ったルール作成](#create-a-rule-with-amazon-kinesis-sink) を参照してください。

## Amazon Kinesis Sink を使ったルールの作成

本節では、ソース MQTT トピック `t/#` からのメッセージを処理し、処理結果を設定した Sink アクションを通じて Amazon Kinesis データストリーム `my_stream` にストリーミングするルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` と入力します。

4. **SQL Editor** でルールを設定します。トピック `t/#` の MQTT メッセージを Amazon Kinesis Data Streams に保存したい場合、以下の SQL 文を使用できます。

   注意：独自の SQL 文を指定する場合、Sink のペイロードテンプレートで必要な全フィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールの学習とテストを行うことを推奨します。

   :::

5. + **Add Action** ボタンをクリックして、ルールによりトリガーされるアクションを定義します。このアクションにより、EMQX はルールで処理したデータを Kinesis に送信します。

6. **Type of Action** ドロップダウンから `Amazon Kinesis` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みの Sink があれば選択可能ですが、本デモでは新規 Sink を作成します。

7. Sink の名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせにしてください。

8. **Connector** ドロップダウンから先に作成した `my_kinesis` を選択します。新規コネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータは [コネクターの作成](#create-a-connector) を参照してください。

9. 以下の情報を入力します：

   - **Amazon Kinesis Stream**：[Amazon Kinesis Data Streams でのストリーム作成](#create-stream-in-amazon-kinesis-data-streams) で作成したストリーム名を入力します。
   - **Partition Key**：このストリームに送信されるレコードに関連付けるパーティションキーを入力します。`${variable_name}` 形式のプレースホルダーも使用可能です（次のステップで例を示します）。

10. **Payload Template** フィールドは空欄のままにするか、テンプレートを定義します。

    - 空欄の場合、MQTT メッセージの clientid、topic、payload などの可視フィールドを JSON 形式でエンコードします。
    - 定義済みテンプレートを使用する場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応する値に置換されます。例えば、`${topic}` は MQTT メッセージのトピックが `my/topic` ならば `my/topic` に置き換わります。

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した際にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

12. **詳細設定（任意）**：必要に応じて詳細設定オプションを構成します。詳細は [詳細設定](#advanced-settings) を参照してください。

13. **Create** をクリックする前に、**Test Connectivity** を押して Sink が Amazon Kinesis Data Streams に接続可能かテストできます。

14. **Create** ボタンをクリックして Sink 設定を完了します。新しい Sink が **Action Outputs** に追加されます。

15. **Create Rule** ページに戻り、設定内容を確認後、**Create** ボタンをクリックしてルールを生成します。

これで、Amazon Kinesis Sink を通じてデータ転送を行うルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成ルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Amazon Kinesis Sink が表示されます。

また、**Integration** -> **Flow Designer** を開くとトポロジーが確認でき、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Amazon Kinesis Data Streams に送信・保存されている様子が確認できます。

## ルールのテスト

1. MQTTX を使ってトピック `t/my_topic` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

2. Sink の稼働状況を確認し、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあることを確認します。

3. [Amazon Kinesis Data Viewer](https://docs.aws.amazon.com/streams/latest/dev/data-viewer.html) にアクセスし、レコード取得時にメッセージが表示されることを確認します。

### LocalStack を使った確認

LocalStack を利用している場合、以下の手順で受信データを確認できます。

1. メッセージ送信前に、以下のコマンドで *ShardIterator* を取得します。

   ```bash
   awslocal kinesis get-shard-iterator --stream-name my_stream --shard-id shardId-000000000000 --shard-iterator-type LATEST
   {
   "ShardIterator": "AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   }
   ```

2. MQTTX を使ってトピック `t/my_topic` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

3. レコードを読み取り、受信データをデコードします。

   ```bash
   awslocal kinesis get-records --shard-iterator="AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   {
       "Records": [
           {
               "SequenceNumber": "49642650476690467334495639799144299020426020544120356866",
               "ApproximateArrivalTimestamp": 1689389148.261,
               "Data": "eyAibXNnIjogImhlbGxvIEFtYXpvbiBLaW5lc2lzIiB9",
               "PartitionKey": "key",
               "EncryptionType": "NONE"
           }
       ],
       "NextShardIterator": "AAAAAAAAAAFj5M3+6XUECflJAlkoSNHV/LBciTYY9If2z1iP+egC/PtdVI2t1HCf3L0S6efAxb01UtvI+3ZSh6BO02+L0BxP5ssB6ONBPfFgqvUIjbfu0GOmzUaPiHTqS8nNjoBtqk0fkYFDOiATdCCnMSqZDVqvARng5oiObgigmxq8InciH+xry2vce1dF9+RRFkKLBc0=",
       "MillisBehindLatest": 0
   }
   
   echo 'eyAibXNnIjogImhlbGxvIEFtYXpvbiBLaW5lc2lzIiB9' | base64 -d
   { "msg": "hello Amazon Kinesis" }
   ```

## 詳細設定

本節では、Amazon Kinesis Sink の詳細設定オプションについて説明します。ダッシュボードで Sink を設定する際、**Advanced Settings** を展開して以下のパラメータをニーズに応じて調整できます。

| フィールド名                     | 説明                                                         | デフォルト値  |
| -------------------------------- | ------------------------------------------------------------ | ------------- |
| **Buffer Pool Size**             | EMQX と Kinesis 間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保存・処理し、ターゲットサービスへの送信を最適化しスムーズなデータ転送を確保するために重要です。 | `16`          |
| **Request TTL**                  | 「Request TTL」（Time To Live）は、リクエストがバッファに入ってから有効とみなされる最大時間（秒）を指定します。このタイマーはリクエストがバッファに入った時点で開始されます。TTL を超えてバッファ内に滞留するか、送信後に Kinesis からの応答やアックがタイムリーに得られない場合、リクエストは期限切れと見なされます。 | `45` 秒      |
| **Health Check Interval**        | Sink が Kinesis との接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒      |
| **Health Check Interval Jitter** | 複数ノードが同時にヘルスチェックを開始する確率を減らすために、基本のヘルスチェック間隔に加える一様ランダム遅延です。複数のアクションやソースが同一コネクターを共有する場合、ジッターを有効にするとヘルスチェック開始時刻がずれて実行されます。 | `15` 秒      |
| **Health Check Timeout**         | コネクターが Kinesis との接続ヘルスチェックを行う際のタイムアウト時間（秒）を指定します。 | `60` 秒      |
| **Max Buffer Queue Size**        | Kinesis Sink の各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、Kinesis への送信を効率化する仲介役です。システム性能やデータ送信要件に応じて調整してください。 | `256`        |
| **Query Mode**                   | メッセージ送信を最適化するために、`synchronous`（同期）または `asynchronous`（非同期）のリクエストモードを選択できます。非同期モードでは Kinesis への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを受信してから Kinesis に到達するまでにタイムラグが生じる可能性があります。 | `Async`      |
| **Batch Size**                   | EMQX から Kinesis へ一度に転送するデータバッチの最大サイズを指定します。サイズを調整することで EMQX と Kinesis 間のデータ転送効率やパフォーマンスを微調整できます。<br />「Batch Size」が「1」の場合、データレコードはバッチ化されず個別に送信されます。 | `1`          |
| **Inflight Window**              | 「インフライトキューリクエスト」は、送信済みでまだ応答やアックを受け取っていないリクエストを指します。この設定は Sink と Kinesis 間の通信時に同時に存在可能なインフライトキューリクエストの最大数を制御します。<br/>**Request Mode** が `asynchronous` の場合、このパラメータは特に重要です。同一 MQTT クライアントからのメッセージを厳密に順序通り処理する必要がある場合は、この値を `1` に設定してください。 | `100`        |
