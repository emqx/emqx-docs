# Apache Doris に MQTT データを取り込む

[Apache Doris](https://doris.apache.org/) は、高い同時実行性、高性能、使いやすさで知られる最新のMassively Parallel Processing（MPP）分析データベースシステムです。リアルタイム分析やデータウェアハウジングのシナリオに特に適しています。EMQX 5.10.0 では、MQTT データを Apache Doris と連携させることで、効率的なデータ保存、リアルタイム分析、強力なデータ可視化を実現できます。

本ガイドでは、EMQX と Apache Doris 間のデータ統合の設定および検証方法について実践的な手順を提供します。

::: tip 注意

EMQX における Apache Doris データ統合は、Apache Doris バージョン 2.1.7 以降をサポートしています。

:::

## 動作概要

Apache Doris データ統合は EMQX の標準機能として提供されており、シンプルな設定で複雑な業務開発を可能にします。典型的な IoT アプリケーションでは、EMQX が IoT プラットフォームとしてデバイス接続およびメッセージ送信を担当し、Apache Doris がデータ保存プラットフォームとしてデバイスの状態やメタデータ、メッセージデータの保存および分析を担います。

<img src="./assets/doris-integration.png" alt="doris-integration" style="zoom:67%;" />

EMQX はルールエンジンと Sink を介してデバイスイベントとデータを Apache Doris に転送します。アプリケーションは Apache Doris 内のデータを読み取り、デバイスの状態を把握し、オンライン・オフラインの記録を取得し、デバイスデータを分析できます。具体的なワークフローは以下の通りです：

- **IoT デバイスが EMQX に接続**：IoT デバイスが MQTT プロトコルを通じて正常に接続されると、オンラインイベントがトリガーされます。イベントにはデバイスID、送信元IPアドレスなどの情報が含まれます。
- **メッセージのパブリッシュと受信**：デバイスは特定のトピックにテレメトリおよび状態データをパブリッシュします。EMQX はこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
- **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンにより、特定のソースからのメッセージやイベントをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールにマッチし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
- **Apache Doris への書き込み**：ルールによりメッセージを Apache Doris に書き込む処理がトリガーされます。SQL テンプレートを利用して、ルール処理結果からデータを抽出し、SQL を構築して Apache Doris に送信し、メッセージの特定フィールドを対応するテーブル・カラムに書き込んだり更新したりします。

イベントおよびメッセージデータが Apache Doris に書き込まれた後は、Apache Doris に接続してデータを読み取り、以下のような柔軟なアプリケーション開発が可能です：

- Grafana などの可視化ツールと接続し、データに基づくグラフを生成してデータ変化を表示。
- デバイス管理システムと連携し、デバイス一覧や状態を閲覧、異常動作を検知して潜在的な問題を迅速に解消。

## 特長と利点

Apache Doris とのデータ統合により、以下の特長とメリットをビジネスにもたらします：

- **柔軟なイベント処理**：EMQX のルールエンジンを通じて、Apache Doris はデバイスのライフサイクルイベントを処理でき、IoT アプリケーション実装に必要な各種管理・監視タスクの開発を大幅に容易にします。イベントデータを分析することで、デバイスの故障や異常動作、トレンド変化を迅速に検知し、適切な対応が可能です。
- **メッセージ変換**：メッセージは EMQX ルールを介して多様な処理や変換を経て Apache Doris に書き込まれるため、保存や利用がより便利になります。
- **リアルタイムデータ取り込み**：Apache Doris は HTTP や JDBC インターフェースによるリアルタイムデータ取り込みをサポートします。EMQX と連携することで、MQTT データを低レイテンシで Doris テーブルに直接書き込め、即時クエリや分析が求められるシナリオに最適です。
- **ストリーミング同期**：Apache Doris は Flink、Kafka、トランザクションデータベースなどのリアルタイムデータストリーム取り込みもサポートし、EMQX の MQTT データと他のストリーミングデータを統合した包括的なリアルタイム分析パイプライン構築に適しています。
- **標準 SQL とエコシステム互換性**：Doris は MySQL 構文に完全対応し標準 SQL をサポートするため、新言語を習得せずに強力な分析クエリを実行可能です。BI ツールやクライアントアプリケーションと容易に統合でき、ダッシュボード、レポート、自動化ワークフローに活用できます。
- **ランタイムメトリクス**：各 Sink のランタイムメトリクス（総メッセージ数、成功／失敗数、現在のレートなど）を確認可能です。

柔軟なイベント処理、多様なメッセージ変換、柔軟なデータ操作、リアルタイム監視・分析機能を通じて、効率的で信頼性が高くスケーラブルな IoT アプリケーションを構築し、ビジネスの意思決定や最適化に役立てられます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Apache Doris データ統合を作成する前に必要な準備について説明します。Apache Doris サーバーのインストールやデータテーブルの作成を含みます。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### Apache Doris サーバーのインストール

[公式ガイド](https://doris.apache.org/docs/dev/getting-started/quick-start/#quick-deployment-with-docker) に従い、Docker Compose を使ってローカルで Doris を起動してください。

### データテーブルの作成

MySQL クライアントを使って Doris Frontend に接続し、コマンドを発行します。詳細は[公式ドキュメント](https://doris.apache.org/docs/dev/getting-started/quick-start/#run-your-first-query)を参照してください。

例：

```sh
mysql -uroot -P9030 -h127.0.0.1
```

Apache Doris には以下のデータベースと2つのテーブルを作成する必要があります：

- `emqx_messages` テーブル：クライアントID、トピック、ペイロード、作成日時を保存します。
- `emqx_client_events` テーブル：クライアントID、イベントタイプ、作成日時を保存します。

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

このセクションでは、Sink を Apache Doris サーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQX と Apache Doris をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. 画面右上の **Create** をクリックします。
3. **Create Connector** ページで **Doris** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下を設定します：
   - **Connector name**：コネクター名を英数字の組み合わせで入力（例：`my_doris`）。
   - **Server Host**：`127.0.0.1:9030` または Apache Doris サーバーの実際のホスト名。
   - **Database Name**：`mqtt` と入力。
   - **Username**：`root` と入力。
   - **Password**：`public` と入力。
5. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations) を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** を押してコネクターが Apache Doris に接続できるかテスト可能です。
7. 画面下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップで **Back to Connector List** または **Create Rule** を選択できます。ルール作成に進み、Apache Doris へのデータ転送やクライアントイベント記録を設定できます。詳細は [Create a Rule with Apache Doris Sink for Message Storage](#create-a-rule-with-apache-doris-sink-for-message-storage) および [Create a Rule with Apache Doris Sink for Events Recording](#create-a-rule-with-apache-doris-sink-for-events-recording) を参照してください。

## メッセージ保存用 Apache Doris Sink を用いたルール作成

このセクションでは、MQTT トピック `t/#` からのメッセージを処理し、処理済みデータを設定済み Sink 経由で Apache Doris の `emqx_messages` テーブルに保存するルールの作成方法を説明します。

EMQX と Apache Doris をローカルマシンで実行していることを前提としています。リモート環境の場合は設定を調整してください。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。
2. 画面右上の **Create** をクリックします。
3. ルールIDに `my_rule` を入力し、**SQL Editor** に以下の文を設定します。これはトピック `t/#` 配下の MQTT メッセージを Apache Doris に保存することを意味します。

   注意：独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールを学習・テストできます。

   :::

4. + **Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQX はルールで処理したデータを Apache Doris に送信します。
5. **Type of Action** ドロップダウンから `Apache Doris` を選択します。**Action** はデフォルトの `Create Action` のままにします。既存の Sink があれば選択可能ですが、この例では新しい Sink を作成します。
6. Sink の名前を英数字の組み合わせで入力します。
7. **Connector** ドロップダウンから先ほど作成した `my_mysql` を選択します。新しいコネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは [Create a Connector](#create-a-connector) を参照してください。
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

   SQL テンプレート内でプレースホルダー変数が未定義の場合、**SQL template** 上部の **Undefined Vars as Null** スイッチでルールエンジンの挙動を切り替えられます：

   - **Disabled**（デフォルト）：ルールエンジンは文字列 `undefined` をデータベースに挿入します。
   - **Enabled**：変数が未定義の場合、ルールエンジンは `NULL` を挿入します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は [Fallback Actions](./data-bridges.md#fallback-actions) を参照してください。
10. 詳細設定（任意）：[Advanced Configurations](#advanced-configurations) を参照してください。
11. **Create** ボタンをクリックして Sink 設定を完了します。新しい Sink が **Action Outputs** に追加されます。
12. **Create Rule** ページに戻り、設定内容を確認して **Create** ボタンを押してルールを生成します。

これでルールが正常に作成されました。**Integration** -> **Rules** ページで新規ルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Apache Doris Sink を確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` 配下のメッセージが Apache Doris に送信・保存されている様子を確認できます。

## イベント記録用 Apache Doris Sink を用いたルール作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済み Sink 経由で Apache Doris の `emqx_client_events` テーブルに保存するルール作成方法を説明します。

ルール作成手順は [メッセージ保存用ルール作成](#メッセージ保存用-apache-doris-sink-を用いたルール作成) とほぼ同様ですが、SQL ルール構文と SQL テンプレートが異なります。

オンライン／オフライン状態記録用の SQL エディターには以下の文を入力します：

```sql
SELECT
  *
FROM
  "$events/client/connected", "$events/client/disconnected"
```

クライアントイベントデータをテーブルに挿入するための SQL テンプレートは以下の通りです：

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

2つの Sink の稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつ、イベントレコードが2件あるはずです。

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

このセクションでは、Apache Doris コネクターおよび Sink の詳細設定オプションについて説明します。ダッシュボードでコネクターや Sink を設定する際、**Advanced Settings** にて以下のパラメータをニーズに合わせて調整できます。

| **項目**                   | **説明**                                                                                       | **推奨値**          |
| -------------------------- | ---------------------------------------------------------------------------------------------- | ------------------- |
| **Connection Pool Size**   | Apache Doris サービスとの接続プール内で同時に維持可能な接続数を指定します。EMQX と Apache Doris 間のアクティブ接続数を制御し、スケーラビリティやパフォーマンスを管理します。<br/>**注意**：適切な接続プールサイズはシステムリソース、ネットワークレイテンシ、アプリケーションの負荷に依存します。大きすぎるとリソース枯渇、小さすぎるとスループット制限の原因となります。 | `8`                 |
| **Start Timeout**          | コネクターが自動起動したリソースが正常状態になるまで待機する最大時間（秒）を指定します。これにより、Apache Doris のデータベースインスタンスなどのリソースが完全に稼働し、データ取引が可能になるまで処理を進めません。 | `5` 秒              |
| **Buffer Pool Size**       | EMQX と Apache Doris 間の egress タイプ Sink におけるデータフロー管理用のバッファワーカープロセス数を指定します。これらのワーカーはデータ送信前に一時的にデータを保持・処理します。ingress のみの Sink では無効で、"0" に設定可能です。 | `16`                |
| **Request TTL**            | バッファに入ったリクエストの有効期限（秒）を指定します。リクエストがこの TTL を超えてバッファに滞留するか、Apache Doris からの応答やアックが遅延した場合、リクエストは期限切れと見なされます。 | `45` 秒             |
| **Health Check Interval**  | コネクターが Apache Doris への接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒             |
| **Max Buffer Queue Size**  | コネクター内の各バッファワーカーが保持可能な最大バイト数を指定します。バッファワーカーは Apache Doris へのデータ送信前にデータを一時保存し、データフローを効率化します。システム性能やデータ転送要件に応じて調整してください。 | `256` MB            |
| **Max Batch Size**         | EMQX から Apache Doris へ一度に送信するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送の効率やパフォーマンスを最適化可能です。<br />`1` に設定すると、レコードをバッチ化せず個別に送信します。 | `1`                 |
| **Query Mode**             | メッセージ送信の最適化のため、`asynchronous`（非同期）または `synchronous`（同期）モードを選択可能です。非同期モードでは Apache Doris への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントが Apache Doris 到着前にメッセージを受信する可能性があります。 | `Async`             |
| **Inflight Window**        | "インフライトクエリ" とは、開始されたが応答やアックをまだ受け取っていないクエリのことです。コネクターが Apache Doris と通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode** が `async` の場合、このパラメータは重要です。同一 MQTT クライアントからのメッセージを厳密に順序処理したい場合は、この値を 1 に設定してください。 | `100`               |
