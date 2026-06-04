# Apache Doris に MQTT データを取り込む

[Apache Doris](https://doris.apache.org/) は、高い同時接続性、高性能、使いやすさで知られる最新の大規模並列処理（MPP）分析データベースシステムです。リアルタイム分析やデータウェアハウジングのシナリオに特に適しています。EMQX 5.10.0 では、MQTT データを Apache Doris と統合でき、効率的な保存、リアルタイム分析、強力なデータ可視化が可能になります。

本ガイドでは、EMQX と Apache Doris 間のデータ統合の設定および検証方法について実践的な手順を提供します。

::: tip 注意事項

EMQX における Apache Doris データ統合は、Apache Doris バージョン 2.1.7 以降をサポートしています。

:::

## 動作概要

Apache Doris データ統合は EMQX の標準機能であり、シンプルな設定で複雑なビジネス開発を可能にします。典型的な IoT アプリケーションでは、EMQX が IoT プラットフォームとしてデバイス接続とメッセージの送受信を担当し、Apache Doris がデータ保存プラットフォームとしてデバイスの状態やメタデータ、メッセージデータの保存および分析を担当します。

<img src="./assets/doris-integration.png" alt="doris-integration" style="zoom:67%;" />

EMQX はルールエンジンと Sink を通じてデバイスのイベントやデータを Apache Doris に転送します。アプリケーションは Apache Doris のデータを読み取り、デバイスの状態を把握したり、デバイスのオンライン・オフライン記録を取得したり、デバイスデータを分析したりできます。具体的なワークフローは以下の通りです：

- **IoT デバイスが EMQX に接続**：IoT デバイスが MQTT プロトコルで正常に接続されると、オンラインイベントがトリガーされます。イベントにはデバイスID、送信元IPアドレスなどの情報が含まれます。
- **メッセージのパブリッシュと受信**：デバイスは特定のトピックにテレメトリや状態データをパブリッシュします。EMQX はこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
- **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンにより、特定のトピックにマッチしたメッセージやイベントを処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
- **Apache Doris への書き込み**：ルールによりメッセージを Apache Doris に書き込む処理がトリガーされます。SQL テンプレートを利用してルール処理結果からデータを抽出し、SQL を構築して Apache Doris に送信、メッセージの特定フィールドを対応するテーブルやカラムに書き込んだり更新したりします。

イベントやメッセージデータが Apache Doris に書き込まれた後は、Apache Doris に接続してデータを読み取り、柔軟なアプリケーション開発が可能です。例えば：

- Grafana などの可視化ツールに接続し、データに基づくチャートを生成してデータ変化を表示する。
- デバイス管理システムに接続し、デバイス一覧や状態を確認、異常動作を検知して潜在的な問題を早期に解決する。

## 特長とメリット

Apache Doris とのデータ統合により、以下の特長と利点をビジネスにもたらします：

- **柔軟なイベント処理**：EMQX ルールエンジンを通じて、Apache Doris はデバイスのライフサイクルイベントを処理可能であり、IoT アプリケーション実装に必要な各種管理・監視タスクの開発を大幅に容易にします。イベントデータを分析することで、デバイス障害や異常動作、傾向変化を即座に検知し、適切な対策を講じられます。
- **メッセージ変換**：メッセージは EMQX ルールを介して多様な処理・変換が可能であり、Apache Doris への保存や利用をより便利にします。
- **リアルタイムデータ取り込み**：Apache Doris は HTTP や JDBC インターフェースによるリアルタイムデータ取り込みをサポートします。EMQX と統合することで、MQTT データを低レイテンシで直接 Doris テーブルに書き込め、即時クエリや分析が求められるシナリオに最適です。
- **ストリーミング同期**：Apache Doris は Flink、Kafka、トランザクションデータベースなどのリアルタイムデータストリーム取り込みもサポートし、EMQX の MQTT データと他のストリーミングデータを統合した包括的なリアルタイム分析パイプライン構築に適しています。
- **標準 SQL とエコシステム互換性**：Doris は MySQL 構文に完全互換で標準 SQL をサポートし、新言語を学ばずに強力な分析クエリを実行可能です。BI ツールやクライアントアプリケーションと容易に統合でき、ダッシュボード、レポート、自動化ワークフローに活用できます。
- **ランタイムメトリクス**：各 Sink の総メッセージ数、成功／失敗数、現在のレートなどのランタイムメトリクスの閲覧をサポートします。

柔軟なイベント処理、多様なメッセージ変換、柔軟なデータ操作、リアルタイム監視・分析機能により、効率的で信頼性が高くスケーラブルな IoT アプリケーションを構築し、ビジネスの意思決定や最適化に役立てられます。

## はじめる前に

本節では、EMQX ダッシュボードで Apache Doris データ統合を作成する前に必要な準備、Apache Doris サーバーのインストールやデータテーブルの作成について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Apache Doris サーバーのインストール

[公式ガイド](https://doris.apache.org/docs/dev/gettingStarted/quick-start#use-docker-for-quick-deployment)に従い、Docker Compose を使ってローカルで Doris を起動してください。

### データテーブルの作成

MySQL クライアントを使って Doris フロントエンドに接続し、コマンドを発行します。詳細は[公式ドキュメント](https://doris.apache.org/docs/dev/gettingStarted/quick-start#run-queries)を参照してください。

例：

```sh
mysql -uroot -P9030 -h127.0.0.1
```

Apache Doris にデータベースと2つのテーブルを作成する必要があります：

- `emqx_messages` テーブルは、クライアントID、トピック、ペイロード、作成日時を格納します。
- `emqx_client_events` テーブルは、クライアントID、イベントタイプ、作成日時を格納します。

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

本節では、Sink を Apache Doris サーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQX と Apache Doris をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Doris** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下を設定します：
   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_doris`
   - **Server Host**：`127.0.0.1:9030` またはリモートの Apache Doris サーバーのホスト名を入力
   - **Database Name**：`mqtt`
   - **Username**：`root`
   - **Password**：`public`
5. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations) を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが Apache Doris サーバーに接続できるかテスト可能です。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** をクリックして、Apache Doris に転送するデータやクライアントイベントの記録を指定する Sink を含むルール作成を続行できます。詳細は [Create a Rule with Apache Doris Sink for Message Storage](#create-a-rule-with-apache-doris-sink-for-message-storage) および [Create a Rule with Apache Doris Sink for Events Recording](#create-a-rule-with-apache-doris-sink-for-events-recording) を参照してください。

## Apache Doris Sink を使ったメッセージ保存用ルールの作成

本節では、ソース MQTT トピック `t/#` からのメッセージを処理し、処理結果を Apache Doris のデータテーブル `emqx_messages` に保存するルールをダッシュボードで作成する方法を説明します。

ローカルマシンで EMQX と Apache Doris を実行していることを前提としています。リモート環境の場合は設定を調整してください。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` を入力し、**SQL Editor** に以下のステートメントを設定します。これはトピック `t/#` 以下の MQTT メッセージを Apache Doris に保存することを意味します。

   注意：独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** をクリックし、**Enable Test** を有効にして SQL ルールを学習・テストできます。

   :::

4. + **Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQX はルールで処理したデータを Apache Doris に送信します。

5. **Type of Action** ドロップダウンリストから `Apache Doris` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みの Sink があれば選択可能ですが、本デモでは新規 Sink を作成します。

6. Sink の名前を入力します。英数字の組み合わせで指定してください。

7. **Connector** ドロップダウンボックスから先ほど作成した `my_mysql` を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメータは [Create a Connector](#create-a-connector) を参照してください。

8. 利用する機能に応じて **SQL Template** を設定します：

   注意：これは前処理済みの SQL なので、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
   INSERT INTO emqx_messages(clientid, topic, payload, created_at) VALUES(
     ${clientid},
     ${topic},
     ${payload},
     FROM_UNIXTIME(${timestamp}/1000)
   )
   ```

   SQL テンプレート内でプレースホルダー変数が未定義の場合は、**SQL template** 上部の **Undefined Vars as Null** スイッチでルールエンジンの動作を切り替えられます：

   - **Disabled**（デフォルト）：ルールエンジンは文字列 `undefined` をデータベースに挿入します。
   - **Enabled**：変数が未定義の場合、ルールエンジンは `NULL` を挿入します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した際にトリガーされます。詳細は [Fallback Actions](./data-bridges.md#fallback-actions) を参照してください。

10. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations) を参照してください。

11. **Create** ボタンをクリックして Sink 設定を完了します。新しい Sink が **Action Outputs** に追加されます。

12. **Create Rule** ページに戻り、設定内容を確認後、**Create** ボタンをクリックしてルールを生成します。

これでルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Apache Doris Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` 以下のメッセージが Apache Doris に送信・保存されていることが確認できます。

## Apache Doris Sink を使ったイベント記録用ルールの作成

本節では、クライアントのオンライン／オフライン状態を記録し、イベントデータを Apache Doris のテーブル `emqx_client_events` に保存するルールの作成方法を説明します。

ルール作成手順は [メッセージ保存用ルールの作成](#apache-doris-sink-を使ったメッセージ保存用ルールの作成) とほぼ同様で、SQL ルール構文と SQL テンプレートのみ異なります。

オンライン／オフライン状態記録用ルールの SQL エディターには以下のステートメントを入力します：

```sql
SELECT
  *
FROM
  "$events/client/connected", "$events/client/disconnected"
```

クライアントイベントデータをデータテーブルに挿入するための SQL テンプレートは以下の通りです：

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

2つの Sink の稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ1件ずつ、イベントレコードが2件あるはずです。

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

## 詳細設定

本節では、Apache Doris コネクターおよび Sink の詳細設定オプションについて説明します。ダッシュボードでコネクターや Sink を設定する際、**Advanced Settings** に進み、以下のパラメータをニーズに合わせて調整してください。

| **項目**                   | **説明**                                                                                         | **推奨値**           |
| -------------------------- | ------------------------------------------------------------------------------------------------ | -------------------- |
| **Connection Pool Size**   | Apache Doris サービスとの接続プールで維持可能な同時接続数を指定します。このオプションは、EMQX と Apache Doris 間のアクティブ接続数を制御し、アプリケーションのスケーラビリティやパフォーマンス管理に役立ちます。<br/>**注意**：適切な接続プールサイズは、システムリソース、ネットワークレイテンシ、アプリケーションのワークロードなど複数要因に依存します。大きすぎるとリソース枯渇の恐れがあり、小さすぎるとスループットが制限されます。 | `8`                  |
| **Start Timeout**          | コネクターが自動起動したリソースの正常状態到達を待機する最大時間（秒）を指定します。この設定により、Apache Doris のデータベースインスタンスなど接続先リソースが完全に稼働し、データ処理準備が整うまで処理を進めないようにします。 | `5` 秒               |
| **Buffer Pool Size**       | EMQX と Apache Doris 間の egress タイプ Sink におけるデータフロー管理のために割り当てるバッファワーカー数を指定します。これらのワーカーは、ターゲットサービスに送信する前のデータを一時的に保存・処理します。ingress（受信）専用の Sink には適用されず、「0」に設定可能です。 | `16`                 |
| **Request TTL**            | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがバッファ内にこの TTL を超えて滞留するか、送信後に Apache Doris からの応答やアックがタイムリーに得られない場合、リクエストは期限切れと判断されます。 | `45` 秒              |
| **Health Check Interval**  | コネクターが Apache Doris への接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒              |
| **Max Buffer Queue Size**  | コネクター内の各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーは Apache Doris への送信前にデータを一時保存し、データフローの効率化を図ります。システム性能やデータ転送要件に応じて調整してください。 | `256` MB             |
| **Max Batch Size**         | EMQX から Apache Doris へ一度に転送するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送の効率やパフォーマンスを最適化可能です。<br />「1」に設定すると、データレコードはバッチ化せず個別に送信されます。 | `1`                  |
| **Query Mode**             | メッセージ送信の最適化のため、`asynchronous` または `synchronous` のクエリモードを選択できます。非同期モードでは Apache Doris への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを Apache Doris 到着前に受信する可能性があります。 | `Async`              |
| **Inflight Window**        | 「インフライトクエリ」とは、開始されたがまだ応答やアックを受け取っていないクエリを指します。コネクターが Apache Doris と通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode** が `async`（非同期）に設定されている場合、このパラメータは特に重要です。同一 MQTT クライアントからのメッセージを厳密な順序で処理する必要がある場合は、この値を 1 に設定してください。 | `100`                |
