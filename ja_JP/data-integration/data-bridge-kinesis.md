# Amazon Kinesis への MQTT データストリーム送信

[AWS Kinesis](https://aws.amazon.com/cn/kinesis/) は、AWS 上のフルマネージドリアルタイムストリーミングデータ処理サービスであり、ストリーミングデータの収集、処理、分析を容易にします。あらゆる規模のストリーミングデータをリアルタイムかつ経済的に効率よく処理でき、高い柔軟性を持ち、数十万のソースからの大量のストリーミングデータを低レイテンシで処理可能です。

EMQX は [Amazon Kinesis Data Streams](https://aws.amazon.com/kinesis/data-streams/) とのシームレスな統合をサポートし、大量の IoT デバイスを接続してリアルタイムのメッセージ収集と送信を実現します。このデータ統合により、Amazon Kinesis Data Streams に接続してリアルタイムデータ分析や複雑なストリーム処理が可能です。

本ページでは、EMQX と Amazon Kinesis 間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に解説します。

## 動作概要

Amazon Kinesis とのデータ統合は、EMQX の標準機能として提供されており、MQTT データストリームを Amazon Kinesis とシームレスに連携させ、IoT アプリケーション開発における豊富なサービスや機能を活用できるよう設計されています。

![emqx-integration-aws](./assets/emqx-integration-aws.jpg)

EMQX はルールエンジンと Sink を介して MQTT データを Amazon Kinesis に転送します。処理の流れは以下の通りです。

1. **IoT デバイスがメッセージをパブリッシュ**：デバイスは特定のトピックを通じてテレメトリや状態データをパブリッシュし、ルールエンジンをトリガーします。  
2. **ルールエンジンがメッセージを処理**：組み込みのルールエンジンが、特定のソースからの MQTT メッセージをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データフォーマットの変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。  
3. **Amazon Kinesis へのブリッジング**：ルールによってトリガーされたアクションでメッセージを Amazon Kinesis に転送します。パーティションキーや書き込み先のデータストリーム、メッセージフォーマットをカスタマイズ可能で、柔軟なデータ統合が実現します。

MQTT メッセージデータが Amazon Kinesis に書き込まれた後は、以下のような柔軟なアプリケーション開発が可能です。

- リアルタイムデータ処理・分析：Amazon Kinesis の強力なデータ処理・分析ツールやストリーミング機能を活用し、メッセージデータのリアルタイム処理・分析を行い、有益なインサイトや意思決定支援を得られます。  
- イベント駆動型機能：Amazon のイベント処理をトリガーし、動的かつ柔軟な関数の起動・処理を実現します。  
- データ保存・共有：メッセージデータを Amazon Kinesis のストレージサービスに送信し、大量データを安全に保存・管理します。これにより他の Amazon サービスと連携してデータの共有・分析が可能となり、多様なビジネスニーズに対応できます。

## 特長とメリット

EMQX と AWS Kinesis Data Streams のデータ統合により、以下の機能と利点がビジネスにもたらされます。

- **信頼性の高いデータ送信と順序保証**：EMQX と AWS Kinesis Data Streams は共に信頼性の高いデータ送信機構を備えています。EMQX は MQTT プロトコルを通じてメッセージの確実な送信を保証し、AWS Kinesis Data Streams はパーティションとシーケンス番号によりメッセージの順序を保証します。これにより、デバイスから送信されたメッセージが正確に目的地に届き、正しい順序で処理されます。  
- **リアルタイムデータ処理**：デバイスからの高頻度データは EMQX のルール SQL による事前リアルタイム処理を経て、MQTT メッセージのフィルタリング、抽出、付加、変換が容易に行えます。AWS Kinesis Data Streams へ送信後は、AWS Lambda や AWS 管理の Apache Flink と組み合わせてさらなるリアルタイム分析が可能です。  
- **弾力的なスケーラビリティ対応**：EMQX は数百万の IoT デバイス接続を容易に実現し、弾力的なスケーラビリティを提供します。一方、AWS Kinesis Data Streams はオンデマンドの自動リソース割り当てと拡張を行います。両者を組み合わせたアプリケーションは接続数やデータ量の増加に応じてスケールし、ビジネスの成長に継続的に対応できます。  
- **パーシステンスなデータ保存**：AWS Kinesis Data Streams はパーシステンスなデータ保存機能を備え、毎秒数百万のデバイスデータストリームを確実に保存します。必要に応じて過去データの取得やオフライン分析・処理が可能です。

AWS Kinesis Data Streams を利用したストリーミングデータパイプラインの構築により、EMQX と AWS プラットフォームの統合の難易度が大幅に低減し、ユーザーにより豊富で柔軟なデータ処理ソリューションを提供します。これにより、EMQX ユーザーは AWS 上で機能的に充実し高性能なデータ駆動型アプリケーションを構築できます。

## はじめる前に

本セクションでは、Amazon Kinesis データ統合の作成を始める前に必要な準備について説明します。Kinesis サービスのセットアップやデータストリームサービスのエミュレーション方法も含みます。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識  
- [データ統合](./data-bridges.md) に関する知識  

### Amazon Kinesis Data Streams でのストリーム作成

以下の手順に従い、AWS マネジメントコンソールからストリームを作成します（詳細は [こちらのチュートリアル](https://docs.aws.amazon.com/streams/latest/dev/how-do-i-create-a-stream.html) を参照）。

1. AWS マネジメントコンソールにサインインし、[Kinesis コンソール](https://console.aws.amazon.com/kinesis) を開きます。  
2. ナビゲーションバーでリージョンセレクターを展開し、リージョンを選択します。  
3. **Create data stream** を選択します。  
4. **Create Kinesis stream** ページでデータストリーム名を入力し、**On-demand** キャパシティモードを選択します。  

### Amazon Kinesis Data Streams のローカルエミュレーション

開発やテストを容易にするため、[LocalStack](https://localstack.cloud/) を利用して Amazon Kinesis Data Streams サービスをローカルでエミュレートできます。LocalStack により、リモートクラウドプロバイダーに接続せずにローカルマシン上で AWS アプリケーションを完全に実行可能です。

1. Docker イメージを使ってインストールおよび起動します。

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

## コネクターの作成

このセクションでは、Sink を Amazon Kinesis Data Streams サービスに接続するためのコネクター作成方法を示します。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。  
2. ページ右上の **Create** をクリックします。  
3. **Create Connector** ページで **Amazon Kinesis** を選択し、**Next** をクリックします。  
4. **Configuration** ステップで以下を設定します。  
   - コネクター名を入力します。英数字の組み合わせとしてください（例：`my_kinesis`）。  
   - **Amazon Kinesis Endpoint**：Kinesis サービスの [エンドポイント](https://docs.aws.amazon.com/general/latest/gr/ak.html) を入力します。LocalStack を使用する場合は `http://localhost:4566` と入力してください。  
   - **AWS Access Key ID**：[アクセスキーID](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html) を入力します。LocalStack 使用時は任意の値で構いません。  
   - **AWS Secret Access Key**：[シークレットアクセスキー](https://docs.aws.amazon.com/powershell/latest/userguide/pstools-appendix-sign-up.html) を入力します。LocalStack 使用時は任意の値で構いません。  
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが Amazon Kinesis Data Streams サービスに接続可能かテストできます。  
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックしてルールと Sink の作成に進めます。詳細は [Amazon Kinesis Sink を使ったルール作成](#create-a-rule-with-amazon-kinesis-sink) を参照してください。

## Amazon Kinesis Sink を使ったルール作成

このセクションでは、ソース MQTT トピック `t/#` からのメッセージを処理し、処理結果を Amazon データストリーム `my_stream` にストリーミングするルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。  
2. ページ右上の **Create** をクリックします。  
3. ルール ID に `my_rule` を入力します。  
4. **SQL Editor** にルールを設定します。トピック `t/#` の MQTT メッセージを Amazon Kinesis Data Streams に保存する場合、以下の SQL 文を使用できます。

   注意：独自の SQL 文を指定する場合、`SELECT` 部分に Sink のペイロードテンプレートで必要なすべてのフィールドを含めるようにしてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** をクリックし、**Enable Test** を有効にして SQL ルールの学習とテストが可能です。

   :::

5. + **Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQX はルールで処理したデータを Kinesis に送信します。  
6. **Type of Action** ドロップダウンリストから `Amazon Kinesis` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みの Sink があれば選択可能ですが、本デモでは新規 Sink を作成します。  
7. Sink の名前と説明を入力します。名前は英数字の組み合わせとしてください。  
8. **Connector** ドロップダウンから先ほど作成した `my_kinesis` を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは [コネクター作成](#create-a-connector) を参照してください。  
9. 以下の情報を入力します。

   - **Amazon Kinesis Stream**： [Amazon Kinesis Data Streams でのストリーム作成](#create-stream-in-amazon-kinesis-data-streams) で作成したストリーム名を入力します。  
   - **Partition Key**：このストリームに送信されるレコードに関連付けるパーティションキーを入力します。`${variable_name}` 形式のプレースホルダーも使用可能です（次のステップの例を参照）。  

10. **Payload Template** フィールドは空白のままにするかテンプレートを定義します。

    - 空白の場合、クライアントID、トピック、ペイロードなど MQTT メッセージの可視入力をすべて JSON 形式でエンコードします。  
    - 定義済みテンプレートを使用する場合、`${variable_name}` 形式のプレースホルダーは MQTT コンテキストの対応値で置換されます。例えば `${topic}` は MQTT メッセージのトピックが `my/topic` であれば `my/topic` に置き換わります。  

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。  
12. **詳細設定（任意）**：必要に応じて詳細設定オプションを構成します。詳細は [詳細設定](#advanced-settings) を参照してください。  
13. **Create** をクリックする前に、**Test Connectivity** をクリックして Sink が Amazon Kinesis Data Streams サービスに接続可能かテストできます。  
14. **Create** ボタンをクリックして Sink 設定を完了します。新しい Sink が **Action Outputs** に追加されます。  
15. **Create Rule** ページに戻り、設定内容を確認します。**Create** ボタンをクリックしてルールを生成します。

これで Amazon Kinesis Sink を介したデータ転送ルールの作成が完了しました。**Integration** -> **Rules** ページで新規ルールを確認できます。**Actions(Sink)** タブをクリックすると新しい Amazon Kinesis Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Amazon Kinesis Data Streams に送信・保存されていることが確認できます。

## ルールのテスト

1. MQTTX を使ってトピック `t/my_topic` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/my_topic -m '{ "msg": "hello Amazon Kinesis" }'
   ```

2. Sink の稼働状況を確認し、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあることを確認します。

3. [Amazon Kinesis Data Viewer](https://docs.aws.amazon.com/streams/latest/dev/data-viewer.html) にアクセスし、レコード取得時にメッセージが確認できるはずです。

### LocalStack を使った確認方法

LocalStack を使用している場合、以下の手順で受信データを確認します。

1. メッセージ送信前に *ShardIterator* を取得します。

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

本セクションでは、Amazon Kinesis Sink の詳細設定オプションについて説明します。ダッシュボードの Sink 設定時に **Advanced Settings** を展開し、以下のパラメータをニーズに応じて調整できます。

| フィールド名                      | 説明                                                         | デフォルト値    |
| -------------------------------- | ------------------------------------------------------------ | -------------- |
| **Buffer Pool Size**             | EMQX と Kinesis 間のデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に保存・処理し、ターゲットサービスへの送信を最適化し、スムーズなデータ転送を保証します。 | `16`           |
| **Request TTL**                  | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがこの TTL を超えてバッファに滞留するか、送信後に Kinesis からの応答やアックを受け取れない場合、リクエストは期限切れと判断されます。 | `45` 秒        |
| **Health Check Interval**        | Sink が Kinesis との接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒        |
| **Health Check Interval Jitter** | 複数のノードが同時にヘルスチェックを開始するのを防ぐため、基本のヘルスチェック間隔に加える一様ランダム遅延時間です。複数のアクションやソースが同じコネクターを共有する場合、ジッターを有効にするとヘルスチェックがずれて実行されます。 | `15` 秒        |
| **Health Check Timeout**         | コネクターが Kinesis との接続状態をヘルスチェックする際のタイムアウト時間を指定します。 | `60` 秒        |
| **Max Buffer Queue Size**        | Kinesis Sink の各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、効率的なデータストリーム処理を担います。システム性能やデータ送信要件に応じて調整してください。 | `256`          |
| **Query Mode**                   | `synchronous`（同期）または `asynchronous`（非同期）のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードでは Kinesis への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントが Kinesis 到着前にメッセージを受信する可能性があります。 | `Async`        |
| **Batch Size**                   | EMQX から Kinesis へ一度に送信するデータバッチの最大サイズを指定します。サイズを調整することで EMQX と Kinesis 間のデータ転送効率と性能を微調整可能です。<br />「Batch Size」が「1」の場合、データレコードはバッチ化せず個別に送信されます。 | `1`            |
| **Inflight Window**             | 「インフライトキューリクエスト」とは、送信済みで応答やアックをまだ受け取っていないリクエストを指します。この設定は Sink と Kinesis 間の通信で同時に存在可能なインフライトキューリクエストの最大数を制御します。<br/>`Request Mode` が `asynchronous` の場合、同一 MQTT クライアントからのメッセージを厳密に順序処理したい場合はこの値を `1` に設定してください。 | `100`          |
