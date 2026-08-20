# Amazon KinesisへのMQTTデータストリーム

[AWS Kinesis](https://aws.amazon.com/cn/kinesis/)は、AWS上のフルマネージドリアルタイムストリーミングデータ処理サービスであり、ストリーミングデータの収集、処理、分析を容易に行えます。あらゆる規模のストリーミングデータを経済的かつ効率的にリアルタイムで処理でき、高い柔軟性を持ち、数十万のソースからの大量のストリーミングデータを低レイテンシで処理可能です。

EMQXは[Amazon Kinesis Data Streams](https://aws.amazon.com/kinesis/data-streams/)とのシームレスな統合をサポートしており、大規模なIoTデバイスのリアルタイムメッセージ収集と送信を実現します。このデータ統合により、Amazon Kinesis Data Streamsと接続してリアルタイムデータ分析や複雑なストリーム処理が可能です。

本ページでは、EMQXとAmazon Kinesis間のデータ統合について包括的に紹介し、実際の作成および検証手順を解説します。

## 動作原理

Amazon Kinesisデータ統合は、EMQXの標準機能として提供されており、ユーザーがMQTTデータストリームをAmazon Kinesisにシームレスに統合し、その豊富なサービスと機能を活用してIoTアプリケーションを開発できるよう支援します。

![emqx-integration-aws](./assets/emqx-integration-aws.jpg)

EMQXはルールエンジンとSinkを介してMQTTデータをAmazon Kinesisに転送します。全体の流れは以下の通りです。

1. **IoTデバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは特定のトピックにマッチするMQTTメッセージを処理します。ルールにマッチしたメッセージは、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
3. **Amazon Kinesisへのブリッジング**：ルールによってトリガーされたアクションでメッセージをAmazon Kinesisに転送します。パーティションキー、書き込み先のデータストリーム、メッセージフォーマットをカスタマイズ可能で、柔軟なデータ統合を実現します。

MQTTメッセージデータがAmazon Kinesisに書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理・分析：Amazon Kinesisの強力なデータ処理・分析ツールとストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、有益なインサイトや意思決定支援を得られます。
- イベント駆動型機能：Amazonのイベント処理をトリガーし、動的かつ柔軟な機能トリガーと処理を実現します。
- データ保存・共有：メッセージデータをAmazon Kinesisのストレージサービスに送信し、大量データの安全な保存・管理を行います。これにより他のAmazonサービスと連携してデータの共有・分析が可能となり、多様なビジネスニーズに対応します。

## 特長とメリット

EMQXとAWS Kinesis Data Streamsのデータ統合は、以下の機能と利点をビジネスにもたらします。

- **信頼性の高いデータ送信と順序保証**：EMQXとAWS Kinesis Data Streamsは共に信頼性の高いデータ送信機構を備えています。EMQXはMQTTプロトコルを通じてメッセージの確実な送信を保証し、AWS Kinesis Data Streamsはパーティションとシーケンス番号によりメッセージの順序を保証します。これにより、デバイスから送信されたメッセージが正確に目的地に届き、正しい順序で処理されます。
- **リアルタイムデータ処理**：デバイスからの高頻度データはEMQXのルールSQLで事前にリアルタイム処理が可能で、MQTTメッセージのフィルタリング、抽出、付加、変換を容易に行えます。AWS Kinesis Data Streamsに送信後は、AWS LambdaやAWS管理のApache Flinkと組み合わせてさらなるリアルタイム分析が実現できます。
- **弾力的なスケーラビリティ対応**：EMQXは数百万のIoTデバイス接続を容易に実現し、弾力的なスケーラビリティを提供します。一方、AWS Kinesis Data Streamsはオンデマンドの自動リソース割り当てと拡張を行います。両者を組み合わせたアプリケーションは接続数やデータ量の増加に応じてスケールし、ビジネスの成長に継続的に対応します。
- **パーシステンス（永続化）されたデータ保存**：AWS Kinesis Data Streamsはパーシステンス機能を備え、毎秒数百万のデバイスデータストリームを確実に保存します。必要に応じて過去データの取得が可能で、オフライン分析や処理にも対応します。

AWS Kinesis Data Streamsを利用したストリーミングデータパイプラインの構築により、EMQXとAWSプラットフォームの統合の難易度が大幅に低減され、ユーザーにより豊富で柔軟なデータ処理ソリューションを提供します。これにより、EMQXユーザーはAWS上で機能的に充実した高性能なデータ駆動型アプリケーションを構築できます。

## はじめる前に

本節では、Amazon Kinesisデータ統合の作成を始める前に必要な準備事項を説明します。Kinesisサービスのセットアップやデータストリームサービスのエミュレーション方法も含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Amazon Kinesis Data Streamsでのストリーム作成

以下の手順に従い、AWSマネジメントコンソールからストリームを作成します（詳細は[こちらのチュートリアル](https://docs.aws.amazon.com/streams/latest/dev/how-do-i-create-a-stream.html)を参照）。

1. AWSマネジメントコンソールにサインインし、[Kinesisコンソール](https://console.aws.amazon.com/kinesis)を開きます。

2. ナビゲーションバーのリージョンセレクターを展開し、リージョンを選択します。

3. **Create data stream**を選択します。

4. **Create Kinesis stream**ページでデータストリーム名を入力し、**On-demand**キャパシティモードを選択します。

### Amazon Kinesis Data Streamsのローカルエミュレーション

開発やテストを容易にするため、[LocalStack](https://localstack.cloud/)を使ってAmazon Kinesis Data Streamsサービスをローカルでエミュレートできます。LocalStackを使うと、リモートクラウドに接続せずにローカルマシン上でAWSアプリケーションを完全に実行可能です。

1. Dockerイメージを使ってインストール・起動します。

   ```bash
   # LocalStackのDockerイメージをローカルで起動
   docker run --name localstack -p '4566:4566' -e 'KINESIS_LATENCY=0' -d localstack/localstack:2.1
   
   # コンテナにアクセス
   docker exec -it localstack bash
   ```

2. シャード数1で`my_stream`という名前のストリームを作成します。

   ```bash
   awslocal kinesis create-stream --stream-name "my_stream" --shard-count 1
   ```

## コネクターの作成

本節では、SinkをAmazon Kinesis Data Streamsサービスに接続するコネクターの作成方法を説明します。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックします。

3. **Create Connector**ページで**Amazon Kinesis**を選択し、**Next**をクリックします。

4. **Configuration**ステップで以下を設定します。

   - コネクター名を入力します。英数字の組み合わせで、例：`my_kinesis`。

   - **Amazon Kinesis Endpoint**：Kinesisサービスの[エンドポイント](https://docs.aws.amazon.com/general/latest/gr/ak.html)を入力します。LocalStackを使用する場合は`http://localhost:4566`を入力してください。

   - **AWS Access Key ID**：[アクセスキーID](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html)を入力します。LocalStack使用時は任意の値で構いません。

   - **AWS Secret Access Key**：[シークレットアクセスキー](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html)を入力します。LocalStack使用時は任意の値で構いません。

5. **Create**をクリックする前に、**Test Connectivity**を押してコネクターがAmazon Kinesis Data Streamsに接続可能かテストできます。

6. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてルールとSinkの作成を続行できます。詳細は[Amazon Kinesis Sink付きルールの作成](#create-a-rule-with-amazon-kinesis-sink)を参照してください。

## Amazon Kinesis Sink付きルールの作成

本節では、ソースMQTTトピック`t/#`からのメッセージを処理し、処理結果をAmazonデータストリーム`my_stream`にストリーミングするルールの作成方法を示します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力します。

4. **SQL Editor**でルールを設定します。トピック`t/#`のMQTTメッセージをAmazon Kinesis Data Streamsに保存したい場合、以下のSQL文を使用できます。

   注意：独自のSQL文を指定する場合は、Sinkのペイロードテンプレートで必要なすべてのフィールドを`SELECT`に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習とテストが可能です。

   :::

5. + **Add Action**ボタンをクリックし、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをKinesisに送信します。

6. **Type of Action**ドロップダウンから`Amazon Kinesis`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既に作成済みのSinkを選択することも可能ですが、本例では新規Sinkを作成します。

7. Sinkの名前と説明を入力します。名前は英数字の組み合わせにしてください。

8. **Connector**ドロップダウンから先に作成した`my_kinesis`を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは[コネクターの作成](#create-a-connector)を参照。

9. 以下の情報を入力します。

   - **Amazon Kinesis Stream**：[Amazon Kinesis Data Streamsでのストリーム作成](#create-stream-in-amazon-kinesis-data-streams)で作成したストリーム名を入力。

   - **Partition Key**：このストリームに送信されるレコードに関連付けるパーティションキーを入力。`${variable_name}`形式のプレースホルダーも使用可能（次ステップの例参照）。

10. **Payload Template**欄は空白のままにするか、テンプレートを定義します。

    - 空白の場合、MQTTメッセージのクライアントID、トピック、ペイロードなどの可視入力をすべてJSON形式でエンコードします。

    - 定義したテンプレートを使う場合、`${variable_name}`形式のプレースホルダーはMQTTコンテキストの対応値に置換されます。例：`${topic}`はMQTTメッセージのトピックが`my/topic`なら`my/topic`に置換されます。

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：必要に応じて詳細設定オプションを構成します。詳細は[詳細設定](#advanced-settings)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**を押してSinkがAmazon Kinesis Data Streamsに接続可能かテストできます。

14. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

15. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでAmazon Kinesis Sinkを介してデータを転送するルールが正常に作成されました。**Integration** -> **Rules**ページで新規作成ルールを確認できます。**Actions(Sink)**タブをクリックすると新しいAmazon Kinesis Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されAmazon Kinesis Data Streamsに送信・保存されている様子が確認できます。

## ルールのテスト

1. MQTTXを使い、トピック`t/my_topic`にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

2. Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

3. [Amazon Kinesis Data Viewer](https://docs.aws.amazon.com/streams/latest/dev/data-viewer.html)にアクセスし、レコード取得時にメッセージが表示されることを確認します。

### LocalStackでの確認方法

LocalStackを利用している場合は、以下の手順で受信データを確認します。

1. メッセージ送信前に、以下のコマンドで*ShardIterator*を取得します。

   ```bash
   awslocal kinesis get-shard-iterator --stream-name my_stream --shard-id shardId-000000000000 --shard-iterator-type LATEST
   {
   "ShardIterator": "AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   }
   ```

2. MQTTXを使い、トピック`t/my_topic`にメッセージを送信します。

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

本節では、Amazon Kinesis Sinkの詳細設定オプションについて説明します。ダッシュボードのSink設定画面で**Advanced Settings**を展開し、用途に応じて以下のパラメータを調整できます。

| フィールド名                     | 説明                                                         | デフォルト値  |
| -------------------------------- | ------------------------------------------------------------ | ------------ |
| **Buffer Pool Size**             | EMQXとKinesis間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保持・処理し、ターゲットサービスへの送信を最適化しスムーズなデータ転送を保証します。 | `16`         |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがこのTTLを超えてバッファ内にある、または送信後にKinesisからの応答やアックを受け取れなかった場合、リクエストは期限切れと判断されます。 | `45`秒       |
| **Health Check Interval**        | SinkがKinesisとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15`秒       |
| **Health Check Interval Jitter** | 複数ノードが同時にヘルスチェックを開始する確率を減らすため、基本のヘルスチェック間隔に加える一様ランダム遅延です。複数のアクションやソースが同じコネクターを共有する場合、ジッターを有効にするとヘルスチェックが少しずつずれて実行されます。 | `15`秒       |
| **Health Check Timeout**         | コネクターがKinesisとの接続ヘルスチェックを行う際のタイムアウト時間（秒）を指定します。 | `60`秒       |
| **Max Buffer Queue Size**        | Kinesis Sinkの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時的に保持し、Kinesisへの送信を効率化します。システム性能やデータ転送要件に応じて調整してください。 | `256`        |
| **Query Mode**                   | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードではKinesisへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがKinesis到達前にメッセージを受信する可能性があります。 | `Async`      |
| **Batch Size**                   | EMQXからKinesisへ一度に送信するデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率と性能を細かく制御できます。<br />「Batch Size」が「1」の場合、データレコードはバッチ化されず個別に送信されます。 | `1`          |
| **Inflight Window**             | 「インフライトキューリクエスト」とは、送信済みだが応答やアックをまだ受け取っていないリクエストのことです。この設定はSinkがKinesisと通信中に同時に存在可能なインフライトリクエストの最大数を制御します。<br/>**Request Mode**が`asynchronous`の場合、このパラメータは特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は、この値を`1`に設定してください。 | `100`        |
