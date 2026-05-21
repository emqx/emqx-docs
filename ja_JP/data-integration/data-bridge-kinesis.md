# Amazon Kinesis への MQTT データストリーミング

[AWS Kinesis](https://aws.amazon.com/cn/kinesis/) は、AWS 上で提供されるフルマネージドのリアルタイムストリーミングデータ処理サービスであり、ストリーミングデータの収集、処理、分析を容易にします。あらゆる規模のストリーミングデータをリアルタイムかつ経済的かつ効率的に処理でき、高い柔軟性を持ち、数十万のソースからの大量ストリーミングデータを低レイテンシで処理可能です。

EMQX は [Amazon Kinesis Data Streams](https://aws.amazon.com/kinesis/data-streams/) とのシームレスな統合をサポートし、大量の IoT デバイスを接続してリアルタイムのメッセージ収集と送信を実現します。このデータ統合により、Amazon Kinesis Data Streams と接続してリアルタイムデータ分析や複雑なストリーム処理が可能になります。

本ページでは、EMQX と Amazon Kinesis 間のデータ統合について包括的に紹介し、実際の作成および検証手順を解説します。

## 動作の仕組み

Amazon Kinesis とのデータ統合は、EMQX の標準機能として提供されており、ユーザーが MQTT データストリームを Amazon Kinesis とシームレスに連携し、IoT アプリケーション開発における豊富なサービスや機能を活用できるよう設計されています。

![emqx-integration-aws](./assets/emqx-integration-aws.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Amazon Kinesis に転送します。全体の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリやステータスデータをパブリッシュし、ルールエンジンをトリガーします。
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンは、特定のソースからの MQTT メッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、メッセージのコンテキスト情報による拡充などの処理を行います。
3. **Amazon Kinesis へのブリッジング**：ルールによってトリガーされたアクションでメッセージを Amazon Kinesis に転送します。パーティションキー、書き込み先のデータストリーム、メッセージフォーマットなどをカスタム設定可能で、柔軟なデータ統合を実現します。

MQTT メッセージデータが Amazon Kinesis に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理・分析：強力な Amazon Kinesis のデータ処理・分析ツールおよびストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、有益な洞察や意思決定支援を得られます。
- イベント駆動型機能：Amazon のイベント処理をトリガーし、動的かつ柔軟な関数の起動・処理を実現します。
- データの保存・共有：メッセージデータを Amazon Kinesis のストレージサービスに送信し、大量データの安全な保存・管理を行います。これにより、他の Amazon サービスと連携してデータの共有や分析が可能となり、多様なビジネスニーズに対応します。

## 特徴と利点

EMQX と AWS Kinesis Data Streams 間のデータ統合は、以下の機能とメリットをビジネスにもたらします。

- **信頼性の高いデータ送信と順序保証**：EMQX と AWS Kinesis Data Streams は共に信頼性の高いデータ送信機構を備えています。EMQX は MQTT プロトコルを通じてメッセージの確実な送信を保証し、AWS Kinesis Data Streams はパーティションとシーケンス番号を用いてメッセージの順序を保証します。これにより、デバイスから送信されたメッセージが正確に目的地に届き、正しい順序で処理されることが保証されます。
- **リアルタイムデータ処理**：デバイスからの高頻度データは、EMQX のルール SQL によるリアルタイムの一次処理を経て、MQTT メッセージのフィルタリング、抽出、拡充、変換を容易に行えます。AWS Kinesis Data Streams へ送信後は、AWS Lambda や AWS 管理の Apache Flink と組み合わせてさらなるリアルタイム分析が可能です。
- **弾力的なスケーラビリティ対応**：EMQX は数百万の IoT デバイス接続を容易に実現し、弾力的なスケーラビリティを提供します。一方、AWS Kinesis Data Streams はオンデマンドの自動リソース割り当てと拡張を行います。両者を組み合わせたアプリケーションは接続数やデータ量に応じてスケールし、ビジネスの成長に継続的に対応可能です。
- **パーシステンスなデータ保存**：AWS Kinesis Data Streams はパーシステンスなデータ保存機能を持ち、毎秒数百万件のデバイスデータストリームを信頼性高く保存します。必要に応じて過去データの取得が可能で、オフライン分析や処理を支援します。

AWS Kinesis Data Streams を活用したストリーミングデータパイプラインの構築は、EMQX と AWS プラットフォームの統合の難易度を大幅に軽減し、ユーザーにより豊富で柔軟なデータ処理ソリューションを提供します。これにより、EMQX ユーザーは AWS 上で機能的に充実した高性能なデータ駆動型アプリケーションを構築できます。

## はじめる前に

本節では、Amazon Kinesis データ統合の作成を開始する前に完了すべき準備について説明します。Kinesis サービスのセットアップやデータストリームサービスのエミュレーション方法も含みます。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Amazon Kinesis Data Streams でのストリーム作成

以下の手順に従い、AWS マネジメントコンソールからストリームを作成してください（詳細は[こちらのチュートリアル](https://docs.aws.amazon.com/streams/latest/dev/how-do-i-create-a-stream.html)を参照）。

1. AWS マネジメントコンソールにサインインし、[Kinesis コンソール](https://console.aws.amazon.com/kinesis)を開きます。

2. ナビゲーションバーのリージョンセレクターを展開し、リージョンを選択します。

3. **Create data stream** を選択します。

4. **Create Kinesis stream** ページで、データストリーム名を入力し、**On-demand** キャパシティモードを選択します。

### Amazon Kinesis Data Streams のローカルエミュレーション

開発やテストを容易にするため、[LocalStack](https://localstack.cloud/) を使って Amazon Kinesis Data Streams サービスをローカルでエミュレートできます。LocalStack を利用すると、リモートのクラウドプロバイダーに接続せずにローカルマシン上で AWS アプリケーションを完全に実行可能です。

1. Docker イメージを用いてインストール・起動します。

   ```bash
   # LocalStack の Docker イメージをローカルで起動
   docker run --name localstack -p '4566:4566' -e 'KINESIS_LATENCY=0' -d localstack/localstack:2.1
   
   # コンテナにアクセス
   docker exec -it localstack bash
   ```

2. シャード数1のストリーム `my_stream` を作成します。

   ```bash
   awslocal kinesis create-stream --stream-name "my_stream" --shard-count 1
   ```

## コネクターの作成

この節では、Sink を Amazon Kinesis Data Streams サービスに接続するためのコネクター作成方法を説明します。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **Amazon Kinesis** を選択し、**Next** をクリックします。

4. **Configuration** ステップで以下の情報を設定します。

   - コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_kinesis`。
   - **Amazon Kinesis Endpoint**：Kinesis サービスの[エンドポイント](https://docs.aws.amazon.com/general/latest/gr/ak.html)を入力します。[LocalStack](#amazon-kinesis-data-streams-のローカルエミュレーション)を使用する場合は `http://localhost:4566` と入力してください。
   - **AWS Access Key ID**：[アクセスキーID](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html)を入力します。[LocalStack](#amazon-kinesis-data-streams-のローカルエミュレーション)利用時は任意の値で構いません。
   - **AWS Secret Access Key**：[シークレットアクセスキー](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html)を入力します。[LocalStack](#amazon-kinesis-data-streams-のローカルエミュレーション)利用時は任意の値で構いません。

5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが Amazon Kinesis Data Streams サービスに接続可能かテストできます。

6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックしてコネクター一覧に戻るか、**Create Rule** をクリックしてルールと Sink の作成を続行できます。詳細は [Amazon Kinesis Sink を使ったルール作成](#create-a-rule-with-amazon-kinesis-sink) を参照してください。

## Amazon Kinesis Sink を使ったルール作成

この節では、ソース MQTT トピック `t/#` からのメッセージを処理し、処理結果を Amazon データストリーム `my_stream` にストリーミングするルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` と入力します。

4. **SQL Editor** でルールを設定します。トピック `t/#` の MQTT メッセージを Amazon Kinesis Data Streams に保存したい場合、以下の SQL 文を使用できます。

   注意：独自の SQL 文を指定する場合は、Sink のペイロードテンプレートで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールを学習・テストしてください。

   :::

5. + **Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQX はルールで処理したデータを Kinesis に送信します。

6. **Type of Action** ドロップダウンリストから `Amazon Kinesis` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。すでに Sink を作成済みの場合はそれを選択することも可能です。本例では新しい Sink を作成します。

7. Sink の名前と説明を入力します。名前は大文字・小文字の英数字の組み合わせとしてください。

8. **Connector** ドロップダウンから、先に作成した `my_kinesis` を選択します。ドロップダウン横のボタンから新規コネクター作成も可能です。設定パラメータは [コネクターの作成](#コネクターの作成) を参照してください。

9. 以下の情報を入力します。

   - **Amazon Kinesis Stream**：[Amazon Kinesis Data Streams でのストリーム作成](#amazon-kinesis-data-streams-でのストリーム作成) で作成したストリーム名を入力します。
   - **Partition Key**：このストリームに送信されるレコードに関連付けるパーティションキーを入力します。`${variable_name}` 形式のプレースホルダーも使用可能です（次のステップで例を示します）。

10. **Payload Template** フィールドは空欄のままにするか、テンプレートを定義します。

    - 空欄の場合、クライアントID、トピック、ペイロードなど MQTT メッセージのすべての可視入力を JSON 形式でエンコードします。
    - 定義済みテンプレートを使う場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応する値に置き換えられます。例えば `${topic}` は MQTT メッセージのトピックが `my/topic` なら `my/topic` に置換されます。

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

12. **詳細設定（任意）**：必要に応じて詳細設定オプションを構成します。詳細は [詳細設定](#advanced-settings) を参照してください。

13. **Create** をクリックする前に、**Test Connectivity** をクリックして Sink が Amazon Kinesis Data Streams サービスに接続可能かテストできます。

14. **Create** ボタンをクリックして Sink の設定を完了します。新しい Sink が **Action Outputs** に追加されます。

15. **Create Rule** ページに戻り、設定内容を確認します。**Create** ボタンをクリックしてルールを生成します。

これで Amazon Kinesis Sink を介してデータを転送するルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Amazon Kinesis Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Amazon Kinesis Data Streams に送信・保存されていることが確認できます。

## ルールのテスト

1. MQTTX を使ってトピック `t/my_topic` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

2. Sink の稼働状況を確認し、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあることを確認します。

3. [Amazon Kinesis Data Viewer](https://docs.aws.amazon.com/streams/latest/dev/data-viewer.html) にアクセスし、レコード取得時にメッセージが表示されることを確認します。

### LocalStack を使った確認方法

LocalStack を利用している場合は、以下の手順で受信データを確認できます。

1. メッセージ送信前に *ShardIterator* を取得します。

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

本節では、Amazon Kinesis Sink の詳細設定オプションについて説明します。ダッシュボードの Sink 設定画面で **Advanced Settings** を展開し、ニーズに応じて以下のパラメータを調整できます。

| フィールド名                      | 説明                                                         | デフォルト値  |
| -------------------------------- | ------------------------------------------------------------ | ------------ |
| **Buffer Pool Size**             | EMQX と Kinesis 間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保存・処理し、ターゲットサービスへの送信を最適化し、スムーズなデータ転送を確保します。 | `16`         |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがこの TTL を超えてバッファ内に留まるか、Kinesis からの応答やアックを受け取れなかった場合、リクエストは期限切れと判断されます。 | `45` 秒      |
| **Health Check Interval**        | Sink が Kinesis との接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒      |
| **Health Check Interval Jitter** | 複数ノードが同時にヘルスチェックを開始する可能性を減らすため、基本のヘルスチェック間隔に加える一様ランダム遅延です。複数のアクションやソースが同一コネクターを共有する場合、ジッターを有効にするとヘルスチェックの開始時刻がずれます。 | `15` 秒      |
| **Health Check Timeout**         | Kinesis との接続状態の自動ヘルスチェックにおけるタイムアウト時間を指定します。 | `60` 秒      |
| **Max Buffer Queue Size**        | Kinesis Sink の各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、効率的なデータストリーム処理を行います。システム性能やデータ転送要件に応じて調整してください。 | `256`        |
| **Query Mode**                   | メッセージ送信の最適化のため、`synchronous`（同期）または `asynchronous`（非同期）リクエストモードを選択できます。非同期モードでは Kinesis への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを Kinesis 到着前に受信する可能性があります。 | `Async`      |
| **Batch Size**                   | EMQX から Kinesis へ一度に転送するデータバッチの最大サイズを指定します。サイズを調整することで、EMQX と Kinesis 間のデータ転送効率と性能を最適化できます。<br />「Batch Size」を「1」に設定すると、データレコードはバッチ化されず個別に送信されます。 | `1`          |
| **Inflight Window**             | 「インフライトキューリクエスト」とは、送信済みだがまだ応答やアックを受け取っていないリクエストを指します。この設定は、Sink と Kinesis 間の通信において同時に存在可能なインフライトキューリクエストの最大数を制御します。<br/>**Request Mode** が `asynchronous` の場合、このパラメータは特に重要です。同一 MQTT クライアントからのメッセージを厳密に順序通り処理する必要がある場合は、この値を `1` に設定してください。 | `100`        |
