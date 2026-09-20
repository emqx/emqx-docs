# Apache Doris に MQTT データを取り込む

[Apache Doris](https://doris.apache.org/) は、高い同時接続数、高性能、使いやすさで知られる最新の大規模並列処理（MPP）分析データベースシステムです。リアルタイム分析やデータウェアハウジングのシナリオに特に適しています。EMQX 5.10.0 では、MQTT データを Apache Doris と連携させることで、効率的な保存、リアルタイム分析、強力なデータ可視化を実現できます。

本ガイドでは、EMQX と Apache Doris 間のデータ統合の設定および検証方法について実践的な手順を提供します。

::: tip 注意

EMQX における Apache Doris データ統合は、Apache Doris バージョン 2.1.7 以降をサポートしています。

:::

## 動作概要

Apache Doris データ統合は EMQX の標準機能であり、シンプルな設定で複雑なビジネス開発を可能にします。典型的な IoT アプリケーションでは、EMQX が IoT プラットフォームとしてデバイス接続とメッセージの送受信を担当し、Apache Doris はデータストレージプラットフォームとしてデバイスの状態やメタデータ、メッセージデータの保存および分析を担います。

<img src="./assets/doris-integration.png" alt="doris-integration" style="zoom:67%;" />

EMQX はルールエンジンと Sink を通じてデバイスのイベントやデータを Apache Doris に転送します。アプリケーションは Apache Doris 内のデータを読み取り、デバイスの状態を把握したり、デバイスのオンライン・オフライン記録を取得したり、デバイスデータを分析したりできます。具体的なワークフローは以下の通りです。

- **IoT デバイスが EMQX に接続**：IoT デバイスが MQTT プロトコルを通じて正常に接続されると、オンラインイベントがトリガーされます。イベントにはデバイス ID、送信元 IP アドレスなどの情報が含まれます。
- **メッセージのパブリッシュと受信**：デバイスは特定のトピックにテレメトリや状態データをパブリッシュします。EMQX はこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
- **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンにより、特定のソースからのメッセージやイベントをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、メッセージのコンテキスト情報付加などを行います。
- **Apache Doris への書き込み**：ルールによりメッセージの Apache Doris への書き込みがトリガーされます。SQL テンプレートを利用してルール処理結果からデータを抽出し、SQL を構築して Apache Doris に送信、メッセージの特定フィールドを対応するテーブル・カラムに書き込むまたは更新します。

イベントおよびメッセージデータが Apache Doris に書き込まれた後は、Apache Doris に接続して柔軟なアプリケーション開発が可能です。例えば：

- Grafana などの可視化ツールに接続し、データに基づくグラフを生成してデータ変化を表示。
- デバイス管理システムに接続し、デバイス一覧や状態を確認、異常動作を検知して潜在的な問題を早期に解決。

## 特長とメリット

Apache Doris とのデータ統合により、以下の特長と利点が得られます。

- **柔軟なイベント処理**：EMQX のルールエンジンを通じて、Apache Doris はデバイスのライフサイクルイベントを処理でき、IoT アプリケーション実装に必要な各種管理・監視タスクの開発を大幅に支援します。イベントデータを分析することで、デバイスの故障や異常動作、傾向変化を迅速に検知し、適切な対応が可能です。
- **メッセージ変換**：メッセージは EMQX ルールを通じて広範囲に処理・変換されてから Apache Doris に書き込まれるため、保存や利用がより便利になります。
- **リアルタイムデータ取り込み**：Apache Doris は HTTP や JDBC インターフェースによるリアルタイムデータ取り込みをサポートします。EMQX と連携することで、MQTT データを低レイテンシで直接 Doris テーブルに書き込め、即時クエリや分析が必要なシナリオに最適です。
- **ストリーミング同期**：Apache Doris は Flink、Kafka、トランザクションデータベースなどのリアルタイムデータストリームの取り込みもサポートします。これにより、EMQX の MQTT データと他のストリーミングデータを統合した統一パイプラインを構築し、包括的なリアルタイム分析が可能です。
- **標準 SQL とエコシステム互換性**：Doris は MySQL 構文に完全対応し、標準 SQL をサポートするため、新たな言語を学ぶことなく強力な分析クエリを実行できます。BI ツールやクライアントアプリケーションとの連携も容易で、ダッシュボード、レポート、ワークフロー自動化に活用できます。
- **ランタイムメトリクス**：各 Sink の総メッセージ数、成功・失敗数、現在のレートなどのランタイムメトリクスの閲覧をサポートします。

柔軟なイベント処理、広範なメッセージ変換、柔軟なデータ操作、リアルタイム監視・分析機能を通じて、効率的で信頼性が高くスケーラブルな IoT アプリケーションを構築し、ビジネスの意思決定や最適化に役立てられます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Apache Doris データ統合を作成する前に必要な準備、Apache Doris サーバーのインストールおよびデータテーブルの作成方法について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Apache Doris サーバーのインストール

[公式ガイド](https://doris.apache.org/docs/dev/gettingStarted/quick-start#use-docker-for-quick-deployment)に従い、Docker Compose を使ってローカル環境で Doris を起動してください。

### データテーブルの作成

MySQL クライアントを使って Doris Frontend に接続し、コマンドを発行します。詳細は[公式ドキュメント](https://doris.apache.org/docs/dev/gettingStarted/quick-start#run-queries)を参照してください。

例：

```sh
mysql -uroot -P9030 -h127.0.0.1
```

Apache Doris に以下のデータベースと2つのテーブルを作成する必要があります。

- `emqx_messages` データテーブル：クライアント ID、トピック、ペイロード、作成日時を保存します。
- `emqx_client_events` データテーブル：クライアント ID、イベントタイプ、作成日時を保存します。

```sql
create database mqtt;
use mqtt;

create table if not exists
  emqx_messages(
    clientid varchar,
    topic string,
    payload string,
    created_at datetime
  )
  properties (replication_num = 1);

create table if not exists
  emqx_client_events(
    clientid varchar,
    event varchar,
    created_at datetime)
  properties (replication_num = 1);
```

## コネクターの作成

このセクションでは、Sink を Apache Doris サーバーに接続するためのコネクター作成方法を示します。

以下の手順は、EMQX と Apache Doris をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Doris** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下を設定します：
   - **Connector name**：コネクター名を英数字の組み合わせで入力します（例：`my_doris`）。
   - **Server Host**：`127.0.0.1:9030` または Apache Doris サーバーの実際のホスト名を入力します。
   - **Database Name**：`mqtt` を入力します。
   - **Username**：`root` を入力します。
   - **Password**：`public` を入力します。
5. 高度な設定（任意）：[高度な設定](#advanced-configurations)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが Apache Doris サーバーに接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックしてルール作成に進み、Apache Doris へのデータ転送やクライアントイベントの記録を行う Sink を指定します。詳細は [Create a Rule with Apache Doris Sink for Message Storage](#create-a-rule-with-apache-doris-sink-for-message-storage) および [Create a Rule with Apache Doris Sink for Events Recording](#create-a-rule-with-apache-doris-sink-for-events-recording) を参照してください。

## Apache Doris Sink を使ったメッセージ保存ルールの作成

このセクションでは、ソース MQTT トピック `t/#` のメッセージを処理し、処理結果を設定済み Sink を通じて Apache Doris の `emqx_messages` データテーブルに保存するルールの作成方法を示します。

EMQX と Apache Doris をローカルマシンで実行していることを前提としています。リモート実行の場合は設定を調整してください。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。
2. ページ右上の **Create** をクリックします。
3. ルール ID に `my_rule` を入力し、**SQL Editor** に以下の文を設定します。これはトピック `t/#` 以下の MQTT メッセージを Apache Doris に保存することを意味します。

   注意：独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

   :::

4. + **Add Action** ボタンをクリックし、ルールによりトリガーされるアクションを定義します。このアクションにより、EMQX はルール処理済みデータを Apache Doris に送信します。
5. **Type of Action** ドロップダウンから `Apache Doris` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既存の Sink があれば選択可能ですが、この例では新規 Sink を作成します。
6. Sink の名前を入力します。英数字の組み合わせで指定してください。
7. **Connector** ドロップダウンから先ほど作成した `my_mysql` を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。
8. 利用する機能に応じて **SQL Template** を設定します。

   注意：これは前処理済みの SQL なので、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
   INSERT INTO emqx_messages(clientid, topic, payload, created_at) VALUES(
     ${clientid},
     ${topic},
     ${payload},
     FROM_UNIXTIME(${timestamp}/1000)
   )
   ```

   SQL テンプレート内でプレースホルダー変数が未定義の場合、**SQL template** 上部の **Undefined Vars as Null** スイッチでルールエンジンの動作を切り替えられます。

   - **Disabled**（デフォルト）：ルールエンジンは未定義変数に文字列 `undefined` を挿入します。
   - **Enabled**：未定義変数の場合に `NULL` を挿入します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効にするのは後方互換性確保のためのみです。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
10. **高度な設定（任意）**：[高度な設定](#advanced-configurations)を参照してください。
11. **Create** ボタンをクリックして Sink 設定を完了します。新しい Sink が **Action Outputs** に追加されます。
12. **Create Rule** ページに戻り、設定内容を確認して **Create** ボタンをクリックしルールを生成します。

これでルールが正常に作成されました。**Integration** -> **Rules** ページで新規ルールを確認できます。**Actions(Sink)** タブをクリックすると新しい Apache Doris Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` 以下のメッセージが Apache Doris に送信・保存されていることが確認できます。

## Apache Doris Sink を使ったイベント記録ルールの作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済み Sink を通じて Apache Doris の `emqx_client_events` テーブルに保存するルールの作成方法を示します。

ルール作成手順は[メッセージ保存ルールの作成](#apache-doris-sink-を使ったメッセージ保存ルールの作成)とほぼ同様ですが、SQL ルール構文と SQL テンプレートが異なります。

オンライン／オフライン状態記録用の SQL Editor には以下の文を入力してください。

```sql
SELECT
  *
FROM
  "$events/client/connected", "$events/client/disconnected"
```

クライアントイベントデータをテーブルに挿入する SQL テンプレートは以下の通りです。

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  FROM_UNIXTIME(${timestamp}/1000)
)
```

## ルールのテスト

MQTTX を使ってトピック `t/1` にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello Apache Doris" }'
```

2つの Sink の稼働状況を確認してください。新規の受信メッセージと送信メッセージが1件ずつ、イベントレコードが2件あるはずです。

`emqx_messages` データテーブルにデータが書き込まれているか確認します。

```bash
mysql> select * from emqx_messages;
+----------+-------+--------------------------+---------------------+
| clientid | topic | payload                  | created_at          |
+----------+-------+--------------------------+---------------------+
| emqx_c   | t/1   | { "msg": "hello Apache Doris" } | 2022-12-09 08:44:07 |
+----------+-------+--------------------------+---------------------+
1 row in set (0.01 sec)
```

`emqx_client_events` テーブルにデータが書き込まれているか確認します。

```bash
mysql> select * from emqx_client_events;
+----------+---------------------+---------------------+
| clientid | event               | created_at          |
+----------+---------------------+---------------------+
| emqx_c   | client.connected    | 2022-12-09 08:44:07 |
| emqx_c   | client.disconnected | 2022-12-09 08:44:07 |
+----------+---------------------+---------------------+
2 rows in set (0.00 sec)
```

## 高度な設定

このセクションでは、Apache Doris コネクターおよび Sink の高度な設定オプションについて詳述します。ダッシュボードでコネクターと Sink を設定する際に、**Advanced Settings** に移動して以下のパラメータをニーズに合わせて調整できます。

| **項目**                   | **説明**                                                                                                   | **推奨値**           |
| -------------------------- | ---------------------------------------------------------------------------------------------------------- | -------------------- |
| **Connection Pool Size**   | Apache Doris サービスとインターフェースする際に、コネクションプール内で維持可能な同時接続数を指定します。このオプションは、EMQX と Apache Doris 間のアクティブな接続数を制御し、アプリケーションのスケーラビリティとパフォーマンス管理に役立ちます。<br/>**注意**：適切な接続プールサイズはシステムリソース、ネットワークレイテンシ、アプリケーションのワークロードなど複数の要因に依存します。大きすぎるとリソース枯渇を招き、小さすぎるとスループットが制限されます。 | `8`                  |
| **Start Timeout**          | コネクターが自動起動したリソースが正常な状態になるまで待機する最大時間（秒）を指定します。この設定により、Apache Doris のデータベースインスタンスなどの接続先リソースが完全に稼働し、データトランザクションの処理準備が整うまで操作を進めないようにします。 | `5` 秒               |
| **Buffer Pool Size**       | EMQX と Apache Doris 間の egress タイプ Sink におけるデータフロー管理用のバッファワーカープロセス数を指定します。これらのワーカーは、ターゲットサービスに送信する前のデータを一時的に保存・処理します。パフォーマンス最適化やスムーズなデータ送信に関係する設定です。ingress（受信）専用の Sink では適用されないため、"0" に設定可能です。 | `16`                 |
| **Request TTL**            | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがバッファに入ってからこの TTL を超えた場合、または送信後に Apache Doris からの応答やアックがタイムリーに得られなかった場合、そのリクエストは期限切れと判断されます。 | `45` 秒              |
| **Health Check Interval**  | コネクターが Apache Doris への接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒              |
| **Max Buffer Queue Size**  | コネクター内の各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーは Apache Doris へ送信する前のデータを一時的に保存し、データフローを効率的に処理します。システムのパフォーマンスやデータ転送要件に応じて調整してください。 | `256` MB             |
| **Max Batch Size**         | EMQX から Apache Doris へ一度に転送するデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率とパフォーマンスを最適化できます。<br />`1` に設定すると、データレコードはバッチ化されず個別に送信されます。 | `1`                  |
| **Query Mode**             | メッセージ送信を最適化するために `asynchronous` または `synchronous` のクエリモードを選択できます。非同期モードでは Apache Doris への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを Apache Doris への書き込み前に受信する可能性があります。 | `Async`              |
| **Inflight Window**        | 「インフライトクエリ」とは、開始されたがまだ応答やアックを受け取っていないクエリのことです。この設定は、コネクターが Apache Doris と通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode** が `async`（非同期）の場合、このパラメータは特に重要です。同一 MQTT クライアントからのメッセージを厳密な順序で処理する必要がある場合は、この値を 1 に設定してください。 | `100`                |
