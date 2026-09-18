# Amazon KinesisへのMQTTデータストリーミング

[AWS Kinesis](https://aws.amazon.com/cn/kinesis/)は、AWS上のフルマネージドなリアルタイムストリーミングデータ処理サービスであり、ストリーミングデータの収集、処理、分析を容易に行えます。あらゆる規模のストリーミングデータをリアルタイムかつ経済的かつ効率的に処理でき、高い柔軟性を持ち、数十万のソースからの大量のストリーミングデータを低レイテンシで処理可能です。

EMQXは[Amazon Kinesis Data Streams](https://aws.amazon.com/kinesis/data-streams/)とのシームレスな連携をサポートし、大量のIoTデバイスを接続してリアルタイムのメッセージ収集・送信を実現します。このデータ統合により、Amazon Kinesis Data Streamsと接続してリアルタイムデータ分析や複雑なストリーム処理が可能になります。

本ページでは、EMQXとAmazon Kinesis間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に解説します。

## 動作の仕組み

Amazon Kinesisデータ統合は、EMQXの標準機能として提供されており、ユーザーがMQTTデータストリームをAmazon Kinesisにシームレスに統合し、IoTアプリケーション開発のための豊富なサービスと機能を活用できるよう設計されています。

![emqx-integration-aws](./assets/emqx-integration-aws.jpg)

EMQXはルールエンジンとSinkを介してMQTTデータをAmazon Kinesisに転送します。全体の流れは以下の通りです。

1. **IoTデバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは特定のソースからのMQTTメッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データフォーマットの変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **Amazon Kinesisへのブリッジング**：ルールによってトリガーされたアクションがメッセージをAmazon Kinesisに転送します。パーティションキー、書き込み先のデータストリーム、メッセージフォーマットのカスタム設定が可能で、柔軟なデータ統合を実現します。

MQTTメッセージデータがAmazon Kinesisに書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理・分析：強力なAmazon Kinesisのデータ処理・分析ツールとストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、価値あるインサイトや意思決定支援を得ることができます。
- イベント駆動型機能：Amazonのイベントハンドリングをトリガーし、動的かつ柔軟な機能の起動・処理を実現します。
- データ保存・共有：メッセージデータをAmazon Kinesisのストレージサービスに送信し、大量データの安全な保存・管理を行います。これにより他のAmazonサービスとデータを共有・分析し、多様なビジネスニーズに対応可能です。

## 特長とメリット

EMQXとAWS Kinesis Data Streams間のデータ統合は、以下の機能と利点をビジネスにもたらします。

- **信頼性の高いデータ伝送と順序保証**：EMQXとAWS Kinesis Data Streamsは共に信頼性の高いデータ伝送メカニズムを提供します。EMQXはMQTTプロトコルを通じてメッセージの信頼性を確保し、AWS Kinesis Data Streamsはパーティションとシーケンス番号でメッセージの順序を保証します。これにより、デバイスから送信されたメッセージが正確に目的地に届き、正しい順序で処理されることを保証します。
- **リアルタイムデータ処理**：デバイスからの高頻度データはEMQXのルールSQLで事前にリアルタイム処理が可能で、MQTTメッセージのフィルタリング、抽出、付加、変換を容易に行えます。データをAWS Kinesis Data Streamsに送信後は、AWS LambdaやAWS管理のApache Flinkと組み合わせてさらなるリアルタイム分析が実現できます。
- **弾力的なスケーラビリティ対応**：EMQXは数百万のIoTデバイスを容易に接続でき、弾力的なスケーラビリティを提供します。一方、AWS Kinesis Data Streamsはオンデマンドの自動リソース割り当てと拡張を採用しています。両者を組み合わせたアプリケーションは接続数やデータ量に応じてスケールし、ビジネスの成長に継続的に対応可能です。
- **パーシステンスなデータ保存**：AWS Kinesis Data Streamsはパーシステンスなデータ保存機能を備え、毎秒数百万のデバイスデータストリームを信頼性高く保存します。必要に応じて過去データの取得が可能で、オフライン分析や処理を支援します。

AWS Kinesis Data Streamsを利用したストリーミングデータパイプラインの構築は、EMQXとAWSプラットフォームの統合の難易度を大幅に軽減し、ユーザーにより豊富で柔軟なデータ処理ソリューションを提供します。これにより、EMQXユーザーはAWS上で機能的に充実した高性能なデータ駆動型アプリケーションを構築できます。

## はじめる前に

このセクションでは、Amazon Kinesisデータ統合の作成を開始する前に必要な準備、Kinesisサービスのセットアップおよびデータストリームサービスのエミュレーション方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Amazon Kinesis Data Streamsでのストリーム作成

以下の手順に従い、AWSマネジメントコンソールからストリームを作成します（詳細は[こちらのチュートリアル](https://docs.aws.amazon.com/streams/latest/dev/how-do-i-create-a-stream.html)を参照）。

1. AWSマネジメントコンソールにサインインし、[Kinesisコンソール](https://console.aws.amazon.com/kinesis)を開きます。

2. ナビゲーションバーでリージョンセレクターを展開し、リージョンを選択します。

3. **Create data stream** を選択します。

4. **Create Kinesis stream** ページでデータストリーム名を入力し、**On-demand** キャパシティモードを選択します。

### Amazon Kinesis Data Streamsのローカルエミュレーション

開発やテストを容易にするため、[LocalStack](https://localstack.cloud/)を使ってAmazon Kinesis Data Streamsサービスをローカルでエミュレートできます。LocalStackを使うと、リモートクラウドプロバイダーに接続せずにローカルマシン上でAWSアプリケーションを完全に実行可能です。

1. Dockerイメージを使ってインストール・起動します。

   ```bash
   # LocalStackのDockerイメージをローカルで起動
   docker run --name localstack -p '4566:4566' -e 'KINESIS_LATENCY=0' -d localstack/localstack:2.1

   # コンテナにアクセス
   docker exec -it localstack bash
   ```

2. シャード数1で `my_stream` という名前のストリームを作成します。

   ```bash
   awslocal kinesis create-stream --stream-name "my_stream" --shard-count 1
   ```

## コネクターの作成

このセクションでは、SinkをAmazon Kinesis Data Streamsサービスに接続するためのコネクター作成方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **Amazon Kinesis** を選択し、**Next** をクリックします。

4. **Configuration** ステップで以下の情報を設定します。

   - コネクター名を入力します。英数字の組み合わせとしてください（例：`my_kinesis`）。
   - **Amazon Kinesis Endpoint**：Kinesisサービスの[エンドポイント](https://docs.aws.amazon.com/general/latest/gr/ak.html)を入力します。[LocalStack](#amazon-kinesis-data-streamsのローカルエミュレーション)を使う場合は `http://localhost:4566` を入力してください。
   - **AWS Access Key ID**：[アクセスキーID](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html)を入力します。[LocalStack](#amazon-kinesis-data-streamsのローカルエミュレーション)利用時は任意の値で構いません。
   - **AWS Secret Access Key**：[シークレットアクセスキー](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html)を入力します。[LocalStack](#amazon-kinesis-data-streamsのローカルエミュレーション)利用時は任意の値で構いません。

5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターがAmazon Kinesis Data Streamsサービスに接続できるかテスト可能です。

6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックしてルールとSinkの作成を続行できます。詳細は[Amazon Kinesis Sinkを使ったルール作成](#create-a-rule-with-amazon-kinesis-sink)を参照してください。

## Amazon Kinesis Sinkを使ったルール作成

このセクションでは、ソースMQTTトピック `t/#` からのメッセージを処理し、処理結果をAmazonのデータストリーム `my_stream` にSinkを介してストリーミングするルールの作成方法を示します。

1. EMQXダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` と入力します。

4. **SQL Editor** でルールを設定します。トピック `t/#` のMQTTメッセージをAmazon Kinesis Data Streamsに保存したい場合は、以下のSQL構文を使用します。

   注意：独自のSQL構文を指定する場合は、Sinkのペイロードテンプレートで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストが可能です。

   :::

5. + **Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションによりEMQXはルールで処理したデータをKinesisに送信します。

6. **Type of Action** のドロップダウンリストから `Amazon Kinesis` を選択します。**Action** はデフォルトの `Create Action` のままにします。既に作成済みのSinkがあれば選択可能ですが、この例では新規Sinkを作成します。

7. Sinkの名前と説明を入力します。名前は英数字の組み合わせで指定してください。

8. **Connector** ドロップダウンから先に作成した `my_kinesis` を選択します。新規コネクター作成はドロップダウン横のボタンから可能です。設定パラメーターは[コネクター作成](#コネクターの作成)を参照してください。

9. 以下の情報を入力します。

   - **Amazon Kinesis Stream**：[Amazon Kinesis Data Streamsでのストリーム作成](#amazon-kinesis-data-streamsでのストリーム作成)で作成したストリーム名を入力します。
   - **Partition Key**：このストリームに送信されるレコードに関連付けるパーティションキーを入力します。`${variable_name}` の形式のプレースホルダーも使用可能です（次のステップで例を示します）。

10. **Payload Template** フィールドは空欄のままにするか、テンプレートを定義します。

    - 空欄の場合、MQTTメッセージのクライアントID、トピック、ペイロードなどの可視入力すべてをJSON形式でエンコードします。
    - 定義済みテンプレートを使う場合、`${variable_name}` 形式のプレースホルダーはMQTTコンテキストの対応値で置換されます。例えば `${topic}` はMQTTメッセージのトピックが `my/topic` なら `my/topic` に置換されます。

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：バッファキューやバッチモードの使用有無を必要に応じて選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

13. **Create** をクリックする前に、**Test Connectivity** をクリックしてSinkがAmazon Kinesis Data Streamsサービスに接続できるかテスト可能です。

14. **Create** ボタンをクリックしてSink設定を完了します。新しいSinkが **Action Outputs** に追加されます。

15. **Create Rule** ページに戻り、設定内容を確認後、**Create** ボタンをクリックしてルールを生成します。

これでAmazon Kinesis Sinkを介してデータを転送するルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると新しいAmazon Kinesis Sinkが表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Amazon Kinesis Data Streamsに送信・保存されていることが確認できます。

## ルールのテスト

1. MQTTXを使ってトピック `t/my_topic` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

2. Sinkの稼働状況を確認し、新規の受信メッセージと送信メッセージが1件ずつあることを確認します。

3. [Amazon Kinesis Data Viewer](https://docs.aws.amazon.com/streams/latest/dev/data-viewer.html)にアクセスし、レコード取得時にメッセージが表示されることを確認します。

### LocalStackを使った確認

LocalStackを利用している場合は、以下の手順で受信データを確認します。

1. メッセージ送信前に以下のコマンドで*ShardIterator*を取得します。

   ```bash
   awslocal kinesis get-shard-iterator --stream-name my_stream --shard-id shardId-000000000000 --shard-iterator-type LATEST
   {
   "ShardIterator": "AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   }
   ```

2. MQTTXを使ってトピック `t/my_topic` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

3. レコードを読み込み、受信データをデコードします。

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
