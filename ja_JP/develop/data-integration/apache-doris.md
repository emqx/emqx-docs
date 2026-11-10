# Apache Doris に MQTT データを取り込む

[Apache Doris](https://doris.apache.org/) は、高い同時接続数、高性能、使いやすさで知られる最新のMassively Parallel Processing（MPP）分析データベースシステムです。リアルタイム分析やデータウェアハウジングのシナリオに特に適しています。EMQX 5.10.0 では、MQTT データを Apache Doris と統合でき、効率的なストレージ、リアルタイム分析、強力なデータ可視化を実現します。

本ガイドでは、EMQX と Apache Doris 間のデータ統合の設定および検証方法について実践的な手順を提供します。

::: tip 注意

EMQX における Apache Doris データ統合は Apache Doris バージョン 2.1.7 以降をサポートしています。

:::

## 動作概要

Apache Doris データ統合は EMQX の標準機能であり、シンプルな設定で複雑なビジネス開発を可能にします。典型的な IoT アプリケーションでは、EMQX が IoT プラットフォームとしてデバイス接続とメッセージ伝送を担当し、Apache Doris がデータストレージプラットフォームとしてデバイスの状態やメタデータ、メッセージデータの保存および分析を担います。

<img src="./assets/doris-integration.png" alt="doris-integration" style="zoom:67%;" />

EMQX はルールエンジンと Sink を通じてデバイスイベントやデータを Apache Doris に転送します。アプリケーションは Apache Doris 内のデータを読み取り、デバイスの状態を把握し、デバイスのオンライン・オフライン記録を取得し、デバイスデータを分析できます。具体的なワークフローは以下の通りです：

- **IoT デバイスが EMQX に接続**：IoT デバイスが MQTT プロトコルで正常に接続されると、オンラインイベントがトリガーされます。イベントにはデバイスID、送信元IPアドレスなどの情報が含まれます。
- **メッセージのパブリッシュと受信**：デバイスは特定のトピックにテレメトリや状態データをパブリッシュします。EMQX はこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
- **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンにより、特定のソースからのメッセージやイベントをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、メッセージのコンテキスト情報付加などの処理を行います。
- **Apache Doris への書き込み**：ルールによりメッセージの Apache Doris への書き込みがトリガーされます。SQL テンプレートを利用して、ルール処理結果からデータを抽出し SQL を構築、Apache Doris に送信して実行します。これによりメッセージの特定フィールドをデータベースの対応テーブル・カラムに書き込んだり更新したりできます。

イベントおよびメッセージデータが Apache Doris に書き込まれた後は、Apache Doris に接続してデータを読み取り、以下のような柔軟なアプリケーション開発が可能です：

- Grafana などの可視化ツールに接続し、データに基づくチャートを生成してデータ変化を表示する。
- デバイス管理システムに接続し、デバイス一覧や状態を閲覧、異常動作を検知して潜在的な問題を早期に解消する。

## 特長とメリット

Apache Doris とのデータ統合により、以下の特長と利点をビジネスにもたらします：

- **柔軟なイベント処理**：EMQX のルールエンジンを通じて、Apache Doris はデバイスのライフサイクルイベントを処理でき、IoT アプリケーション実装に必要な各種管理・監視タスクの開発を大幅に容易にします。イベントデータを分析することで、デバイス故障や異常動作、トレンド変化を迅速に検知し適切な対応が可能です。
- **メッセージ変換**：メッセージは EMQX ルールを介して広範囲に処理・変換されてから Apache Doris に書き込まれるため、ストレージや利用がより便利になります。
- **リアルタイムデータ取り込み**：Apache Doris は HTTP および JDBC インターフェースによるリアルタイムデータ取り込みをサポートします。EMQX と統合することで、MQTT データを低レイテンシで Doris テーブルに直接書き込め、即時クエリや分析が必要なシナリオに最適です。
- **ストリーミング同期**：Apache Doris は Flink、Kafka、トランザクションデータベースなどからのリアルタイムストリーム取り込みもサポートします。これにより、EMQX の MQTT データと他のストリーミングデータソースを統合した統一パイプライン構築が可能で、包括的なリアルタイム分析を実現します。
- **標準 SQL とエコシステム互換性**：Doris は MySQL 構文に完全互換で標準 SQL をサポートし、新言語習得なしで強力な分析クエリが可能です。BI ツールやクライアントアプリケーションと容易に統合でき、ダッシュボード、レポート、ワークフロー自動化に活用できます。
- **ランタイムメトリクス**：各 Sink の総メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスの閲覧をサポートします。

柔軟なイベント処理、広範なメッセージ変換、柔軟なデータ操作、リアルタイム監視・分析機能を通じて、効率的で信頼性が高くスケーラブルな IoT アプリケーションを構築でき、ビジネスの意思決定や最適化に役立ちます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Apache Doris データ統合を作成する前に必要な準備、Apache Doris サーバーのインストールやデータテーブルの作成について説明します。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### Apache Doris サーバーのインストール

[公式ガイド](https://doris.apache.org/docs/dev/getting-started/quick-start/#quick-deployment-with-docker) に従い、Docker Compose を使ってローカルで Doris を起動してください。

### データテーブルの作成

MySQL クライアントを使って Doris Frontend に接続し、コマンドを発行します。詳細は [公式ドキュメント](https://doris.apache.org/docs/dev/getting-started/quick-start/#run-your-first-query) を参照してください。

例：

```sh
mysql -uroot -P9030 -h127.0.0.1
```

Apache Doris に以下のデータベースと2つのテーブルを作成します：

- `emqx_messages` テーブル：クライアントID、トピック、ペイロード、作成日時を格納します。
- `emqx_client_events` テーブル：クライアントID、イベントタイプ、作成日時を格納します。

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

このセクションでは、Sink を Apache Doris サーバーに接続するためのコネクター作成手順を示します。

以下の手順は EMQX と Apache Doris をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Doris** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下を設定します：
   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_doris`
   - **Server Host**：`127.0.0.1:9030` または Apache Doris サーバーの実際のホスト名
   - **Database Name**：`mqtt`
   - **Username**：`root`
   - **Password**：`public`
5. 高度な設定（任意）：[高度な設定](#advanced-configurations)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが Apache Doris に接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** をクリックして、Apache Doris に転送するデータやクライアントイベントを記録する Sink を指定するルールの作成に進めます。詳細は [メッセージ保存用 Apache Doris Sink のルール作成](#create-a-rule-with-apache-doris-sink-for-message-storage) および [イベント記録用 Apache Doris Sink のルール作成](#create-a-rule-with-apache-doris-sink-for-events-recording) を参照してください。

## メッセージ保存用 Apache Doris Sink のルール作成

このセクションでは、ソース MQTT トピック `t/#` からのメッセージを処理し、処理済みデータを設定済みの Sink を通じて Apache Doris のデータテーブル `emqx_messages` に保存するルールの作成方法を示します。

EMQX と Apache Doris をローカルマシンで実行していることを前提としています。リモート環境の場合は設定を調整してください。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID に `my_rule` を入力し、**SQL Editor** に以下の文を設定します。これはトピック `t/#` 以下の MQTT メッセージを Apache Doris に保存することを意味します。

   注：独自の SQL 構文を指定する場合は、Sink が要求するすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の場合は **SQL Examples** と **Enable Test** をクリックして SQL ルールを学習・テストできます。

   :::

4. + **Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQX はルール処理済みデータを Apache Doris に送信します。

5. **Type of Action** ドロップダウンリストから `Apache Doris` を選択します。**Action** はデフォルトの `Create Action` のままにします。既に作成済みの Sink があれば選択可能ですが、ここでは新規 Sink を作成します。

6. Sink の名前を入力します。英数字の組み合わせで指定してください。

7. **Connector** ドロップダウンから先ほど作成した `my_mysql` を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは [コネクターの作成](#create-a-connector) を参照してください。

8. 利用する機能に応じて **SQL Template** を設定します：

   バッチモードが無効の場合、Apache Doris はプリペアドステートメントを使用します。プレースホルダーを引用符で囲んだり、ステートメントの末尾にセミコロンを付けたりしないでください。

   ::: warning 重要なお知らせ

   EMQX 6.3.1 以降、バッチモードが有効な場合、EMQX は Sink 作成時に SQL テンプレートを解析し、Doris 互換の構文でテキストおよびバイナリのプレースホルダー値をエスケープし、サポートされないテンプレートは拒否します。テンプレートは単一の Apache Doris `INSERT INTO ... VALUES` ステートメントで、1 行のみ設定可能です。プレースホルダーは完全な値としておよび通常・生文字列リテラル内でサポートされます。SQL コメント、動的識別子、複数行設定、`INSERT SELECT`、行エイリアス、`ON DUPLICATE KEY UPDATE` はサポートされません。

   この検証により、以前のバージョンで許容されていたテンプレートが拒否される場合があります。アップグレード前に互換性のないテンプレートを修正してください。

   :::

   ```sql
   INSERT INTO emqx_messages(clientid, topic, payload, created_at) VALUES(
     ${clientid},
     ${topic},
     ${payload},
     FROM_UNIXTIME(${timestamp}/1000)
   )
   ```

   SQL テンプレート内でプレースホルダー変数が未定義の場合、**SQL template** 上部の **Undefined Vars as Null** スイッチでルールエンジンの挙動を設定できます：

   - **無効**（デフォルト）：ルールエンジンは文字列 `undefined` をデータベースに挿入します。
   - **有効**：変数が未定義の場合、ルールエンジンは `NULL` を挿入します。

     ::: tip

     可能な限りこのオプションは常に有効にしてください。無効化は後方互換性確保のためのみ使用します。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した際にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

10. **高度な設定（任意）**：[高度な設定](#advanced-configurations) を参照してください。

11. **Create** ボタンをクリックして Sink 設定を完了します。新しい Sink が **Action Outputs** に追加されます。

12. **Create Rule** ページに戻り、設定内容を確認して **Create** ボタンをクリックしルールを生成します。

これでルールが正常に作成されました。**Integration** -> **Rules** ページで新規ルールを確認できます。**Actions(Sink)** タブをクリックすると新しい Apache Doris Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックしてトポロジーを確認すると、トピック `t/#` 以下のメッセージが Apache Doris に送信・保存されていることがわかります。

## イベント記録用 Apache Doris Sink のルール作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済み Sink を通じて Apache Doris のテーブル `emqx_client_events` に保存するルールの作成方法を示します。

ルール作成手順は [メッセージ保存用 Apache Doris Sink のルール作成](#メッセージ保存用-apache-doris-sink-のルール作成) とほぼ同様ですが、SQL ルール構文と SQL テンプレートが異なります。

バッチモードが有効な場合、前述の SQL テンプレート制約がこのテンプレートにも適用されます。

オンライン／オフライン状態記録用のルール作成には、**SQL Editor** に以下の文を入力します：

```sql
SELECT
  *
FROM
  "$events/client/connected", "$events/client/disconnected"
```

クライアントイベントデータをデータテーブルに挿入するには、以下の SQL テンプレートを使用します：

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

2つの Sink の稼働状況を確認してください。新規の受信メッセージと送信メッセージがそれぞれ1件ずつ、イベント記録が2件あるはずです。

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

このセクションでは、Apache Doris コネクターおよび Sink の高度な設定オプションについて詳述します。ダッシュボードでコネクターや Sink を設定する際、**Advanced Settings** に進み、以下のパラメータをニーズに合わせて調整してください。

| **項目**                  | **説明**                                                                                                         | **推奨値**           |
| ------------------------- | ---------------------------------------------------------------------------------------------------------------- | --------------------- |
| **Connection Pool Size**  | Apache Doris サービスとの接続プールで維持可能な同時接続数を指定します。このオプションは EMQX と Apache Doris 間のアクティブ接続数を制御し、アプリケーションのスケーラビリティやパフォーマンス管理に役立ちます。<br/>**注意**：適切な接続プールサイズはシステムリソース、ネットワークレイテンシ、ワークロードに依存します。大きすぎるとリソース枯渇、小さすぎるとスループット制限の原因となります。 | `8`                   |
| **Start Timeout**         | コネクターが自動起動されたリソースが正常状態になるまで待機する最大時間（秒）を指定します。この設定により、Apache Doris のデータベースインスタンスなど接続先リソースが完全に稼働し準備完了するまで処理を進めないようにします。 | `5` 秒                |
| **Buffer Pool Size**      | EMQX と Apache Doris 間の egress タイプ Sink におけるデータフロー管理用のバッファワーカープロセス数を指定します。これらのワーカーはデータ送信前の一時保管と処理を担当し、パフォーマンス最適化やスムーズなデータ転送に寄与します。ingress（受信）専用 Sink では不要なため「0」に設定可能です。 | `16`                  |
| **Request TTL**           | バッファに入ったリクエストが有効とみなされる最大期間（秒）を指定します。リクエストはバッファリング開始時からカウントされ、TTL 超過または Apache Doris からの応答・アックがタイムリーに得られない場合は期限切れと判断されます。 | `45` 秒               |
| **Health Check Interval** | コネクターが Apache Doris への接続状態を自動的にヘルスチェックする間隔（秒）を指定します。 | `15` 秒               |
| **Max Buffer Queue Size** | コネクターの各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーは Apache Doris へのデータ送信前に一時的にデータを保管し、データフローの効率化を図ります。システム性能やデータ転送要件に応じて調整してください。 | `256` MB              |
| **Max Batch Size**        | EMQX から Apache Doris へ単一転送で送信するデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率やパフォーマンスを最適化できます。<br />`1` に設定すると、データはバッチ化されず個別に送信されます。 | `1`                   |
| **Query Mode**            | メッセージ送信の要件に応じて `asynchronous`（非同期）または `synchronous`（同期）クエリモードを選択できます。非同期モードでは Apache Doris への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを Apache Doris への到達前に受信する可能性があります。 | `Async`               |
| **Inflight Window**       | 「インフライトクエリ」とは、開始されたがまだ応答やアックを受け取っていないクエリのことです。コネクターが Apache Doris と通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode** が `async`（非同期）の場合、このパラメータは特に重要です。同一 MQTT クライアントからのメッセージを厳密に順序処理したい場合は、この値を 1 に設定してください。 | `100`                 |
