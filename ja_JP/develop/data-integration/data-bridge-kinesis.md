# Amazon Kinesis への MQTT データストリーム

[AWS Kinesis](https://aws.amazon.com/cn/kinesis/) は、AWS 上で提供されるフルマネージドのリアルタイムストリーミングデータ処理サービスであり、ストリーミングデータの収集、処理、分析を容易にします。あらゆる規模のストリーミングデータを経済的かつ効率的にリアルタイム処理でき、高い柔軟性を持ち、数十万のソースからの大量のストリーミングデータを低レイテンシで処理可能です。

EMQX は [Amazon Kinesis Data Streams](https://aws.amazon.com/kinesis/data-streams/) とシームレスに統合でき、大規模な IoT デバイスのリアルタイムメッセージ収集と送信を実現します。このデータ統合を通じて、Amazon Kinesis Data Streams に接続し、リアルタイムデータ分析や複雑なストリーム処理を行うことが可能です。

本ページでは、EMQX と Amazon Kinesis 間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に解説します。

## 動作の仕組み

Amazon Kinesis とのデータ統合は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームを Amazon Kinesis とシームレスに連携し、IoT アプリケーション開発における豊富なサービスと機能を活用できるよう支援します。

![emqx-integration-aws](./assets/emqx-integration-aws.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Amazon Kinesis に転送します。全体の流れは以下の通りです：

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のトピックにマッチする MQTT メッセージを処理します。ルールエンジンは対応するルールに基づき、データ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報付加などを行います。
3. **Amazon Kinesis へのブリッジング**：ルールでトリガーされたアクションにより、メッセージが Amazon Kinesis に転送されます。パーティションキーや書き込み先のデータストリーム、メッセージフォーマットなどをカスタマイズ可能で、柔軟なデータ統合を実現します。

MQTT メッセージデータが Amazon Kinesis に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です：

- リアルタイムデータ処理・分析：Amazon Kinesis の強力なデータ処理・分析ツールとストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、価値あるインサイトや意思決定支援を得られます。
- イベント駆動型機能：Amazon のイベント処理をトリガーし、動的かつ柔軟な関数の起動・処理を実現します。
- データの保存・共有：メッセージデータを Amazon Kinesis のストレージサービスに送信し、大量データを安全に保存・管理します。これにより他の Amazon サービスと連携してデータの共有や分析を行い、多様なビジネスニーズに対応可能です。

## 特長とメリット

EMQX と AWS Kinesis Data Streams 間のデータ統合は、以下の機能と利点をビジネスにもたらします：

- **信頼性の高いデータ伝送と順序保証**：EMQX と AWS Kinesis Data Streams は共に信頼性の高いデータ伝送機構を提供します。EMQX は MQTT プロトコルを通じてメッセージの確実な送信を保証し、AWS Kinesis Data Streams はパーティションとシーケンス番号によりメッセージの順序を保証します。これにより、デバイスから送信されたメッセージが正確に目的地に届き、正しい順序で処理されます。
- **リアルタイムデータ処理**：デバイスからの高頻度データは EMQX のルール SQL による事前リアルタイム処理を経て、MQTT メッセージのフィルタリング、抽出、付加、変換を容易に行えます。AWS Kinesis Data Streams へデータ送信後は、AWS Lambda や AWS 管理の Apache Flink と組み合わせてさらなるリアルタイム分析が可能です。
- **弾力的なスケーラビリティ対応**：EMQX は数百万の IoT デバイス接続を容易に実現し、弾力的なスケーラビリティを備えています。一方、AWS Kinesis Data Streams はオンデマンドの自動リソース割り当てと拡張を行います。両者を組み合わせたアプリケーションは接続数やデータ量の増加に応じてスケールし、ビジネスの成長に継続的に対応します。
- **パーシステンスなデータ保存**：AWS Kinesis Data Streams はパーシステンス機能を備え、毎秒数百万のデバイスデータストリームを信頼性高く保存します。必要に応じて過去データの取得が可能で、オフライン分析や処理を支援します。

AWS Kinesis Data Streams を利用したストリーミングデータパイプラインの構築は、EMQX と AWS プラットフォームの統合の難易度を大幅に軽減し、ユーザーにより豊かで柔軟なデータ処理ソリューションを提供します。これにより、EMQX ユーザーは AWS 上で機能的に充実し高性能なデータ駆動型アプリケーションを構築できます。

## はじめる前に

本セクションでは、Amazon Kinesis データ統合の作成を開始する前に必要な準備、Kinesis サービスのセットアップおよびデータストリームサービスのエミュレーション方法について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Amazon Kinesis Data Streams でストリームを作成する

以下の手順に従い、AWS マネジメントコンソールからストリームを作成します（詳細は[こちらのチュートリアル](https://docs.aws.amazon.com/streams/latest/dev/how-do-i-create-a-stream.html)を参照してください）。

1. AWS マネジメントコンソールにサインインし、[Kinesis コンソール](https://console.aws.amazon.com/kinesis)を開きます。

2. ナビゲーションバーのリージョンセレクターを展開し、リージョンを選択します。

3. **Create data stream** を選択します。

4. **Create Kinesis stream** ページで、データストリーム名を入力し、**On-demand** キャパシティモードを選択します。

### Amazon Kinesis Data Streams をローカルでエミュレートする

開発やテストを容易にするため、[LocalStack](https://localstack.cloud/) を利用して Amazon Kinesis Data Streams サービスをローカルでエミュレートできます。LocalStack により、リモートクラウドプロバイダーに接続せずにローカルマシン上で AWS アプリケーションを完全に実行可能です。

1. Docker イメージを使ってインストールおよび起動します：

   ```bash
   # LocalStack の Docker イメージをローカルで起動
   docker run --name localstack -p '4566:4566' -e 'KINESIS_LATENCY=0' -d localstack/localstack:2.1
   
   # コンテナにアクセス
   docker exec -it localstack bash
   ```

2. シャード数 1 のストリーム `my_stream` を作成します：

   ```bash
   awslocal kinesis create-stream --stream-name "my_stream" --shard-count 1
   ```

## コネクターを作成する

本セクションでは、Sink を Amazon Kinesis Data Streams サービスに接続するためのコネクター作成方法を示します。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **Amazon Kinesis** を選択し、**Next** をクリックします。

4. **Configuration** ステップで以下を設定します：
   - コネクター名を入力します。英数字の組み合わせで、例：`my_kinesis`。
   - **Amazon Kinesis Endpoint**：Kinesis サービスの[エンドポイント](https://docs.aws.amazon.com/general/latest/gr/ak.html)を入力します。[LocalStack](#amazon-kinesis-data-streams-をローカルでエミュレートする)を使う場合は `http://localhost:4566` を入力してください。
   - **AWS Access Key ID**：[アクセスキーID](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html)を入力します。[LocalStack](#amazon-kinesis-data-streams-をローカルでエミュレートする)利用時は任意の値で構いません。
   - **AWS Secret Access Key**：[シークレットアクセスキー](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html)を入力します。[LocalStack](#amazon-kinesis-data-streams-をローカルでエミュレートする)利用時は任意の値で構いません。

5. **Create** をクリックする前に、**Test Connectivity** を押してコネクターが Amazon Kinesis Data Streams に接続可能かテストできます。

6. ページ下部の **Create** ボタンをクリックし、コネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックしてルールと Sink の作成を続行できます。詳細は [Amazon Kinesis Sink を使ったルールの作成](#create-a-rule-with-amazon-kinesis-sink) を参照してください。

## Amazon Kinesis Sink を使ったルールの作成

本セクションでは、ソース MQTT トピック `t/#` からのメッセージを処理し、処理結果を Amazon データストリーム `my_stream` にストリーミングするルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` を入力します。

4. **SQL Editor** でルールを設定します。トピック `t/#` の MQTT メッセージを Amazon Kinesis Data Streams に保存したい場合、以下の SQL 文を使用できます。

   注意：独自の SQL 文を指定する場合、Sink のペイロードテンプレートで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールの学習とテストを行うことをおすすめします。

   :::

5. + **Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQX はルールで処理したデータを Kinesis に送信します。

6. **Type of Action** ドロップダウンリストから `Amazon Kinesis` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みの Sink があれば選択可能ですが、本デモでは新規 Sink を作成します。

7. Sink の名前と説明を入力します。名前は英数字の組み合わせとしてください。

8. **Connector** ドロップダウンから、先ほど作成した `my_kinesis` を選択します。新規コネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータは [コネクターの作成](#コネクターを作成する) を参照してください。

9. 以下の情報を入力します：

   - **Amazon Kinesis Stream**：[Amazon Kinesis Data Streams でストリームを作成する](#amazon-kinesis-data-streams-でストリームを作成する) で作成したストリーム名を入力します。
   - **Partition Key**：このストリームに送信されるレコードに関連付けるパーティションキーを入力します。`${variable_name}` 形式のプレースホルダーも使用可能です（次のステップで例を示します）。

10. **Payload Template** フィールドは空欄のままにするか、テンプレートを定義します。

    - 空欄の場合、クライアントID、トピック、ペイロードなど MQTT メッセージの可視入力をすべて JSON 形式でエンコードします。
    - 定義したテンプレートを使用する場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応する値で置換されます。例えば `${topic}` は MQTT メッセージのトピックが `my/topic` なら `my/topic` に置き換わります。

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

12. **詳細設定（任意）**：必要に応じて詳細設定を行います。詳細は [詳細設定](#advanced-settings) をご覧ください。

13. **Create** をクリックする前に、**Test Connectivity** を押して Sink が Amazon Kinesis Data Streams に接続できるかテスト可能です。

14. **Create** ボタンをクリックし、Sink の設定を完了します。新しい Sink が **Action Outputs** に追加されます。

15. **Create Rule** ページに戻り、設定内容を確認します。**Create** ボタンをクリックしてルールを生成します。

これで Amazon Kinesis Sink を介してデータを転送するルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Amazon Kinesis Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Amazon Kinesis Data Streams に送信・保存されていることが確認できます。

## ルールのテスト

1. MQTTX を使ってトピック `t/my_topic` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

2. Sink の稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあるはずです。

3. [Amazon Kinesis Data Viewer](https://docs.aws.amazon.com/streams/latest/dev/data-viewer.html) にアクセスし、レコードを取得するとメッセージが確認できます。

### LocalStack を使った確認方法

LocalStack を利用している場合は、以下の手順で受信データを確認してください。

1. EMQX にメッセージを送信する前に、以下のコマンドで *ShardIterator* を取得します。

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

## 詳細設定

本セクションでは、Amazon Kinesis Sink の詳細設定オプションについて説明します。ダッシュボードの Sink 設定時に **Advanced Settings** を展開し、用途に応じて以下のパラメータを調整できます。

| フィールド名                     | 説明                                                         | デフォルト値    |
| -------------------------------- | ------------------------------------------------------------ | --------------- |
| **Buffer Pool Size**             | EMQX と Kinesis 間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保存・処理し、ターゲットサービスへの送信を最適化し、スムーズなデータ伝送を確保します。 | `16`            |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがこの TTL を超えてバッファに滞留するか、送信後に Kinesis から適時の応答やアックを受け取れない場合、リクエストは期限切れと判断されます。 | `45` 秒         |
| **Health Check Interval**        | Sink が Kinesis との接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒         |
| **Health Check Interval Jitter** | 複数ノードが同時にヘルスチェックを開始する確率を減らすため、基本のヘルスチェック間隔に加える一様ランダム遅延です。複数のアクションやソースが同一コネクターを共有する場合、ジッターを有効にするとヘルスチェックの開始タイミングが分散されます。 | `15` 秒         |
| **Health Check Timeout**         | コネクターが Kinesis との接続状態をヘルスチェックする際のタイムアウト時間を指定します。 | `60` 秒         |
| **Max Buffer Queue Size**        | Kinesis Sink の各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、Kinesis への送信を効率化するための仲介役を担います。システム性能やデータ伝送要件に応じて調整してください。 | `256`           |
| **Query Mode**                   | メッセージ送信の最適化のため、`synchronous`（同期）または `asynchronous`（非同期）リクエストモードを選択できます。非同期モードでは Kinesis への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを受信するタイミングが Kinesis 到達前になる可能性があります。 | `Async`         |
| **Batch Size**                   | EMQX から Kinesis へ一度に転送するデータバッチの最大サイズを指定します。サイズ調整により EMQX と Kinesis 間のデータ転送効率や性能を微調整可能です。<br />「Batch Size」が「1」の場合、データレコードはバッチ化されず個別に送信されます。 | `1`             |
| **Inflight Window**             | 「インフライトキューリクエスト」とは、送信済みだがまだ応答やアックを受け取っていないリクエストのことです。この設定は Sink と Kinesis 間の通信で同時に存在可能なインフライトリクエストの最大数を制御します。<br/>**Request Mode** が `asynchronous` の場合、このパラメータは特に重要です。同一 MQTT クライアントからのメッセージを厳密に順序通り処理する必要がある場合は、この値を `1` に設定してください。 | `100`           |
