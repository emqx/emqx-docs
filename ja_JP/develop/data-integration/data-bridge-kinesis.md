# Amazon Kinesis への MQTT データストリーム

[AWS Kinesis](https://aws.amazon.com/cn/kinesis/) は、AWS 上のフルマネージドリアルタイムストリーミングデータ処理サービスであり、ストリーミングデータの収集、処理、分析を簡単に行えます。あらゆる規模のストリーミングデータをリアルタイムかつ経済的に効率よく処理でき、高い柔軟性を持ち、数十万のソースからの大量データを低レイテンシで処理可能です。

EMQX は [Amazon Kinesis Data Streams](https://aws.amazon.com/kinesis/data-streams/) とシームレスに統合でき、大規模な IoT デバイスのリアルタイムメッセージ収集と送信を実現します。このデータ連携により、Amazon Kinesis Data Streams と接続してリアルタイムデータ分析や複雑なストリーム処理が可能になります。

本ページでは、EMQX と Amazon Kinesis 間のデータ連携について包括的に紹介し、データ連携の作成および検証方法を実践的に説明します。

## 動作概要

Amazon Kinesis とのデータ連携は EMQX の標準機能であり、MQTT データストリームを Amazon Kinesis とシームレスに統合し、豊富なサービスや機能を活用して IoT アプリケーション開発を支援します。

![emqx-integration-aws](./assets/emqx-integration-aws.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Amazon Kinesis に転送します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンが特定のソースからの MQTT メッセージをトピックマッチングに基づいて処理します。ルールにマッチしたメッセージは、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などが行われます。
3. **Amazon Kinesis への転送**：ルールによってトリガーされたアクションでメッセージを Amazon Kinesis に転送します。パーティションキーや書き込み先のデータストリーム、メッセージフォーマットのカスタム設定が可能で、柔軟なデータ連携を実現します。

MQTT メッセージデータが Amazon Kinesis に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理と分析：Amazon Kinesis の強力なデータ処理・分析ツールとストリーミング機能を活用し、メッセージデータをリアルタイムに処理・分析して価値ある洞察や意思決定支援を得られます。
- イベント駆動機能：Amazon のイベント処理をトリガーし、動的かつ柔軟な機能トリガーや処理を実現します。
- データ保存と共有：メッセージデータを Amazon Kinesis のストレージサービスに送信し、大量データを安全に保存・管理します。他の Amazon サービスと連携してデータ共有や分析も可能です。

## 特長とメリット

EMQX と AWS Kinesis Data Streams のデータ連携は、以下の機能や利点をビジネスにもたらします。

- **信頼性の高いデータ送信と順序保証**：EMQX と AWS Kinesis Data Streams は共に信頼性の高いデータ送信機構を提供します。EMQX は MQTT プロトコルでメッセージの確実な送信を保証し、AWS Kinesis Data Streams はパーティションとシーケンス番号でメッセージ順序を保証します。これにより、デバイスから送信されたメッセージが正確に届き、正しい順序で処理されます。
- **リアルタイムデータ処理**：デバイスからの高頻度データは EMQX のルール SQL でリアルタイムに一次処理でき、MQTT メッセージのフィルタリング、抽出、付加、変換が容易です。AWS Kinesis Data Streams へ送信後は、AWS Lambda や AWS 管理の Apache Flink と組み合わせてさらにリアルタイム分析が可能です。
- **弾力的なスケーラビリティ対応**：EMQX は数百万台の IoT デバイス接続と弾力的なスケーラビリティを提供し、AWS Kinesis Data Streams はオンデマンドの自動リソース割り当てと拡張を行います。両者を組み合わせたアプリケーションは接続数やデータ量に応じてスケールし、ビジネスの成長に継続的に対応可能です。
- **永続的なデータ保存**：AWS Kinesis Data Streams は永続的なデータ保存機能を備え、毎秒数百万のデバイスデータストリームを確実に保存します。必要に応じて過去データの取得やオフライン分析・処理が行えます。

AWS Kinesis Data Streams を利用したストリーミングデータパイプラインの構築は、EMQX と AWS プラットフォームの統合の難易度を大幅に下げ、より豊富で柔軟なデータ処理ソリューションをユーザーに提供します。これにより、EMQX ユーザーは AWS 上で機能的に充実した高性能なデータ駆動型アプリケーションを構築できます。

## はじめる前に

本節では、Amazon Kinesis データ連携の作成に先立ち、Kinesis サービスのセットアップやデータストリームサービスのエミュレートなど、準備すべき事項を説明します。

### 前提条件

- EMQX データ連携の [ルール](./rules.md) に関する知識
- [データ連携](./data-bridges.md) に関する知識

### Amazon Kinesis Data Streams でストリームを作成する

以下の手順で AWS マネジメントコンソールからストリームを作成します（詳細は [こちらのチュートリアル](https://docs.aws.amazon.com/streams/latest/dev/how-do-i-create-a-stream.html) を参照）。

1. AWS マネジメントコンソールにサインインし、[Kinesis コンソール](https://console.aws.amazon.com/kinesis) を開きます。

2. ナビゲーションバーでリージョンセレクターを展開し、リージョンを選択します。

3. **Create data stream** を選択します。

4. **Create Kinesis stream** ページでデータストリーム名を入力し、**On-demand** キャパシティモードを選択します。

### Amazon Kinesis Data Streams をローカルでエミュレートする

開発やテストを容易にするため、[LocalStack](https://localstack.cloud/) を使って Amazon Kinesis Data Streams サービスをローカルでエミュレートできます。LocalStack により、リモートクラウドに接続せずにローカルマシン上で AWS アプリケーションを完全に実行可能です。

1. Docker イメージを使ってインストール・起動します。

   ```bash
   # LocalStack の Docker イメージをローカルで起動
   docker run --name localstack -p '4566:4566' -e 'KINESIS_LATENCY=0' -d localstack/localstack:2.1
   
   # コンテナにアクセス
   docker exec -it localstack bash
   ```

2. シャード数 1 のストリーム `my_stream` を作成します。

   ```bash
   awslocal kinesis create-stream --stream-name "my_stream" --shard-count 1
   ```

## コネクターを作成する

本節では、Sink を Amazon Kinesis Data Streams サービスに接続するコネクターの作成方法を説明します。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。

2. 画面右上の **Create** をクリックします。

3. **Create Connector** ページで **Amazon Kinesis** を選択し、**Next** をクリックします。

4. **Configuration** ステップで以下を設定します。

   - コネクター名を入力します。英数字の大文字・小文字の組み合わせとしてください。例：`my_kinesis`。
   - **Amazon Kinesis Endpoint**：Kinesis サービスの [エンドポイント](https://docs.aws.amazon.com/general/latest/gr/ak.html) を入力します。[LocalStack](#amazon-kinesis-data-streams-をローカルでエミュレートする) を使う場合は `http://localhost:4566` と入力します。
   - **AWS Access Key ID**：[アクセスキーID](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html) を入力します。[LocalStack](#amazon-kinesis-data-streams-をローカルでエミュレートする) 利用時は任意の値で構いません。
   - **AWS Secret Access Key**：[シークレットアクセスキー](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html) を入力します。[LocalStack](#amazon-kinesis-data-streams-をローカルでエミュレートする) 利用時は任意の値で構いません。

5. **Create** をクリックする前に、**Test Connectivity** を押してコネクターが Amazon Kinesis Data Streams サービスに接続できるかテストできます。

6. 画面下部の **Create** ボタンを押してコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、続けてルールと Sink を作成して Amazon Kinesis へ転送するデータを指定できます。詳細は [Amazon Kinesis Sink を使ったルール作成](#create-a-rule-with-amazon-kinesis-sink) を参照してください。

## Amazon Kinesis Sink を使ったルール作成

本節では、ソース MQTT トピック `t/#` からのメッセージを処理し、処理結果を Amazon データストリーム `my_stream` にストリーミングするルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. 画面右上の **Create** をクリックします。

3. ルール ID に `my_rule` を入力します。

4. **SQL Editor** にルールを設定します。トピック `t/#` の MQTT メッセージを Amazon Kinesis Data Streams に保存したい場合、以下の SQL 文を使用できます。

   注意：独自の SQL 文を指定する場合は、Sink のペイロードテンプレートで必要なフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** をクリックし、**Enable Test** で SQL ルールを学習・テストできます。

   :::

5. + **Add Action** ボタンを押して、ルールによりトリガーされるアクションを定義します。このアクションで EMQX はルール処理済みデータを Kinesis に送信します。

6. **Type of Action** ドロップダウンから `Amazon Kinesis` を選択します。**Action** はデフォルトの `Create Action` のままにします。既に作成済みの Sink があれば選択可能ですが、ここでは新規 Sink を作成します。

7. Sink の名前と説明を入力します。名前は英数字の大文字・小文字の組み合わせにしてください。

8. **Connector** ドロップダウンから先ほど作成した `my_kinesis` を選択します。新規コネクターはドロップダウン横のボタンから作成可能です。設定パラメータは [コネクター作成](#create-a-connector) を参照してください。

9. 以下の情報を入力します。

   - **Amazon Kinesis Stream**：[Amazon Kinesis Data Streams でストリームを作成する](#create-stream-in-amazon-kinesis-data-streams) で作成したストリーム名を入力します。
   - **Partition Key**：このストリームに送信されるレコードに関連付けるパーティションキーを入力します。`${variable_name}` 形式のプレースホルダーも利用可能です（次のステップで例を示します）。

10. **Payload Template** フィールドは空欄のままにするかテンプレートを定義します。

    - 空欄の場合、MQTT メッセージのクライアントID、トピック、ペイロードなどの可視フィールドを JSON 形式でエンコードします。
    - 定義したテンプレートを使う場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応する値に置き換えられます。例：`${topic}` は MQTT メッセージのトピックが `my/topic` なら `my/topic` に置換されます。

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。プライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

12. **詳細設定（任意）**：バッファキューやバッチモードの使用有無を選択します。詳細は [Sink の機能](./data-bridges.md#features-of-sink) を参照してください。

13. **Create** をクリックする前に、**Test Connectivity** を押して Sink が Amazon Kinesis Data Streams サービスに接続できるかテスト可能です。

14. **Create** ボタンを押して Sink 設定を完了します。新しい Sink が **Action Outputs** に追加されます。

15. **Create Rule** ページに戻り、設定内容を確認して **Create** ボタンを押しルールを生成します。

これで Amazon Kinesis Sink を介してデータを転送するルールが正常に作成されました。**Integration** -> **Rules** ページで新規ルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Amazon Kinesis Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され Amazon Kinesis Data Streams に送信・保存されている様子を確認できます。

## ルールのテスト

1. MQTTX を使い、トピック `t/my_topic` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

2. Sink の稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ 1 件ずつあるはずです。

3. [Amazon Kinesis Data Viewer](https://docs.aws.amazon.com/streams/latest/dev/data-viewer.html) にアクセスし、レコード取得時にメッセージが確認できます。

### LocalStack を使った確認

LocalStack を利用している場合、以下の手順で受信データを確認します。

1. EMQX にメッセージを送信する前に、*ShardIterator* を取得します。

   ```bash
   awslocal kinesis get-shard-iterator --stream-name my_stream --shard-id shardId-000000000000 --shard-iterator-type LATEST
   {
   "ShardIterator": "AAAAAAAAAAG3YjBK9sp0uSIFGTPIYBI17bJ1RsqX4uJmRllBAZmFRnjq1kPLrgcyn7RVigmH+WsGciWpImxjXYLJhmqI2QO/DrlLfp6d1IyJFixg1s+MhtKoM6IOH0Tb2CPW9NwPYoT809x03n1zL8HbkXg7hpZjWXPmsEvkXjn4UCBf5dBerq7NLKS3RtAmOiXVN6skPpk="
   }
   ```

2. MQTTX でトピック `t/my_topic` にメッセージを送信します。

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
