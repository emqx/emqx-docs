# Apache Doris に MQTT データを取り込む

[Apache Doris](https://doris.apache.org/) は、高い同時接続性、高性能、使いやすさで知られる最新のMassively Parallel Processing（MPP）分析データベースシステムです。リアルタイム分析やデータウェアハウジングのシナリオに特に適しています。EMQX 5.10.0 では、MQTT データを Apache Doris と統合でき、効率的なストレージ、リアルタイム分析、強力なデータ可視化を実現します。

本ガイドでは、EMQX と Apache Doris 間のデータ統合の設定および検証方法について実践的な手順を提供します。

::: tip 注意

EMQX の Apache Doris データ統合は Apache Doris バージョン 2.1.7 以降をサポートしています。

:::

## 動作の仕組み

Apache Doris データ統合は EMQX の標準機能として提供されており、シンプルな設定で複雑なビジネス開発を可能にします。典型的な IoT アプリケーションでは、EMQX が IoT プラットフォームとしてデバイスの接続とメッセージの送受信を担当し、Apache Doris がデータストレージプラットフォームとしてデバイスの状態やメタデータ、メッセージデータの保存および分析を担当します。

<img src="./assets/doris-integration.png" alt="doris-integration" style="zoom:67%;" />

EMQX はルールエンジンと Sink を通じてデバイスのイベントとデータを Apache Doris に転送します。アプリケーションは Apache Doris のデータを読み取り、デバイスの状態を把握したり、デバイスのオンライン・オフライン記録を取得したり、デバイスデータを分析したりできます。具体的なワークフローは以下の通りです：

- **IoT デバイスが EMQX に接続**：IoT デバイスが MQTT プロトコルを介して正常に接続されると、オンラインイベントがトリガーされます。イベントにはデバイスID、送信元IPアドレスなどの属性情報が含まれます。
- **メッセージのパブリッシュと受信**：デバイスは特定のトピックにテレメトリや状態データをパブリッシュします。EMQX はこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
- **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンにより、特定のソースからのメッセージやイベントをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチングし、データ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報の付加などを行います。
- **Apache Doris への書き込み**：ルールがトリガーされるとメッセージを Apache Doris に書き込みます。SQL テンプレートを利用して、ルール処理結果からデータを抽出し SQL を構築して Apache Doris に送信し、メッセージの特定フィールドを対応するテーブルやカラムに書き込んだり更新したりします。

イベントおよびメッセージデータが Apache Doris に書き込まれた後、Apache Doris に接続してデータを読み取り、以下のような柔軟なアプリケーション開発が可能です：

- Grafana などの可視化ツールに接続し、データに基づくチャートを生成してデータ変化を表示する。
- デバイス管理システムに接続し、デバイス一覧や状態を確認、異常なデバイス挙動を検知し、潜在的な問題を迅速に解消する。

## 特長と利点

Apache Doris とのデータ統合により、以下の特長と利点をビジネスにもたらします：

- **柔軟なイベント処理**：EMQX ルールエンジンを通じて、Apache Doris はデバイスのライフサイクルイベントを処理でき、IoT アプリケーションの実装に必要な各種管理・監視タスクの開発を大幅に容易にします。イベントデータを分析することで、デバイスの故障や異常挙動、傾向変化を迅速に検知し、適切な対策を講じられます。
- **メッセージ変換**：メッセージは EMQX ルールを通じて広範な処理・変換が可能で、Apache Doris への書き込み前にデータの整形やフィルタリングを行い、保存および活用を容易にします。
- **リアルタイムデータ取り込み**：Apache Doris は HTTP や JDBC インターフェースによるリアルタイムデータ取り込みをサポートします。EMQX と統合することで、MQTT データを低レイテンシで直接 Doris テーブルに書き込め、即時のクエリや分析に適しています。
- **ストリーミング同期**：Apache Doris は Flink、Kafka、トランザクションデータベースなど他のストリーミングデータソースからのリアルタイムデータ取り込みもサポートし、EMQX の MQTT データと組み合わせた統合パイプライン構築に最適です。
- **標準 SQL とエコシステム互換性**：Doris は MySQL 構文に完全対応し標準 SQL をサポートするため、新たな言語を学ぶことなく強力な分析クエリを実行可能です。BI ツールやクライアントアプリケーションとの連携も容易で、ダッシュボード、レポート、自動化ワークフローに活用できます。
- **ランタイム指標**：各 Sink のランタイム指標（総メッセージ数、成功/失敗数、現在の処理レートなど）を閲覧可能です。

柔軟なイベント処理、広範なメッセージ変換、柔軟なデータ操作、リアルタイム監視・分析機能を活用し、効率的で信頼性が高くスケーラブルな IoT アプリケーションを構築し、ビジネスの意思決定や最適化に役立てられます。

## はじめる前に

本節では、EMQX ダッシュボードで Apache Doris データ統合を作成する前に必要な準備について説明します。Apache Doris サーバーのインストールやデータテーブルの作成を含みます。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### Apache Doris サーバーのインストール

[公式ガイド](https://doris.apache.org/docs/dev/gettingStarted/quick-start#use-docker-for-quick-deployment) に従い、Docker Compose を使ってローカルで Doris を起動してください。

### データテーブルの作成

MySQL クライアントを使って Doris Frontend に接続し、コマンドを発行します。詳細は [公式ドキュメント](https://doris.apache.org/docs/dev/gettingStarted/quick-start#run-queries) を参照してください。

例：

```sh
mysql -uroot -P9030 -h127.0.0.1
```

Apache Doris に以下のデータベースと2つのテーブルを作成する必要があります：

- `emqx_messages` テーブル：クライアントID、トピック、ペイロード、作成日時を保存するためのデータテーブル
- `emqx_client_events` テーブル：クライアントID、イベントタイプ、作成日時を保存するためのデータテーブル

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

本節では、Sink を Apache Doris サーバーに接続するためのコネクターの作成方法を説明します。

以下の手順は、EMQX と Apache Doris をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQX ダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Doris** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_doris`
   - **Server Host**：`127.0.0.1:9030` または Apache Doris サーバーがリモートの場合は実際のホスト名を入力
   - **Database Name**：`mqtt`
   - **Username**：`root`
   - **Password**：`public`
5. 高度な設定（任意）：[高度な設定](#advanced-configurations)を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが Apache Doris サーバーに接続できるかテスト可能です。
7. ページ下部の **Create** ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックしてルール作成に進み、Apache Doris へのデータ転送やクライアントイベントの記録を指定できます。詳細は [メッセージ保存用 Apache Doris Sink のルール作成](#create-a-rule-with-apache-doris-sink-for-message-storage) および [イベント記録用 Apache Doris Sink のルール作成](#create-a-rule-with-apache-doris-sink-for-events-recording) を参照してください。

## メッセージ保存用 Apache Doris Sink のルール作成

本節では、MQTT トピック `t/#` からのメッセージを処理し、処理済みデータを設定済み Sink を通じて Apache Doris の `emqx_messages` データテーブルに保存するルールの作成方法を示します。

EMQX と Apache Doris をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を調整してください。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。
2. ページ右上の **Create** をクリックします。
3. ルールID に `my_rule` を入力し、**SQL Editor** に以下のステートメントを設定します。これはトピック `t/#` 配下の MQTT メッセージを Apache Doris に保存することを意味します。

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

4. + **Add Action** ボタンをクリックして、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQX はルールで処理したデータを Apache Doris に送信します。
5. **Type of Action** ドロップダウンリストから `Apache Doris` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みの Sink があれば選択可能です。本デモでは新しい Sink を作成します。
6. Sink の名前を入力します。名前は大文字・小文字の英数字の組み合わせで指定してください。
7. **Connector** ドロップダウンから先ほど作成した `my_mysql` を選択します。ドロップダウン横のボタンから新しいコネクターを作成することも可能です。設定パラメータは [コネクターの作成](#create-a-connector) を参照してください。
8. 使用する機能に応じて **SQL Template** を設定します：

   注意：これは前処理済みの SQL なので、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
   INSERT INTO emqx_messages(clientid, topic, payload, created_at) VALUES(
     ${clientid},
     ${topic},
     ${payload},
     FROM_UNIXTIME(${timestamp}/1000)
   )
   ```

   SQL テンプレート内に未定義のプレースホルダー変数がある場合、**SQL template** 上部の **Undefined Vars as Null** スイッチでルールエンジンの動作を切り替えられます：

   - **Disabled**（デフォルト）：ルールエンジンは文字列 `undefined` をデータベースに挿入します。
   - **Enabled**：変数が未定義の場合、`NULL` を挿入することを許可します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効にするのは後方互換性を確保する場合のみです。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。
10. **高度な設定（任意）**：[高度な設定](#advanced-configurations) を参照してください。
11. **Create** ボタンをクリックして Sink の設定を完了します。新しい Sink が **Action Outputs** に追加されます。
12. **Create Rule** ページに戻り、設定内容を確認して **Create** ボタンをクリックしルールを生成します。

これでルールの作成が完了しました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい Apache Doris Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックしてトポロジーを確認すると、トピック `t/#` 配下のメッセージが Apache Doris に送信・保存されていることがわかります。

## イベント記録用 Apache Doris Sink のルール作成

本節では、クライアントのオンライン/オフライン状態を記録し、イベントデータを設定済み Sink を通じて Apache Doris の `emqx_client_events` テーブルに保存するルールの作成方法を示します。

ルール作成手順は [メッセージ保存用 Apache Doris Sink のルール作成](#メッセージ保存用-apache-doris-sink-のルール作成) とほぼ同様ですが、SQL ルール構文と SQL テンプレートが異なります。

オンライン/オフライン状態記録用のルールは、**SQL Editor** に以下のステートメントを入力します：

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

MQTTX を使ってトピック `t/1` にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello Apache Doris" }'
```

2つの Sink の稼働状況を確認すると、新規の受信メッセージ数と送信メッセージ数が1つずつ増加し、イベントレコードが2件追加されているはずです。

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

本節では、Apache Doris コネクターおよび Sink の高度な設定オプションについて詳述します。ダッシュボードでコネクターや Sink を設定する際、**Advanced Settings** に移動して以下のパラメータをニーズに応じて調整してください。

| **項目**                   | **説明**                                                                                                   | **推奨値**           |
| -------------------------- | ---------------------------------------------------------------------------------------------------------- | -------------------- |
| **Connection Pool Size**   | Apache Doris サービスとの接続プール内で維持可能な同時接続数を指定します。この設定により、EMQX と Apache Doris 間のアクティブ接続数を制御し、アプリケーションのスケーラビリティとパフォーマンスを管理します。<br/>**注意**：適切な接続プールサイズはシステムリソース、ネットワークレイテンシ、アプリケーションの負荷などに依存します。大きすぎるとリソース枯渇の恐れがあり、小さすぎるとスループットが制限されます。 | `8`                  |
| **Start Timeout**          | コネクターが自動起動したリソース（例：Apache Doris のデータベースインスタンス）が正常状態になるまで待機する最大時間（秒）を指定します。この設定により、リソースが完全に稼働しデータ取引可能になるまで処理を進めないようにします。 | `5` 秒               |
| **Buffer Pool Size**       | EMQX と Apache Doris 間の egress タイプ Sink でデータフローを管理するバッファワーカーの数を指定します。これらのワーカーはデータを一時的に格納・処理し、ターゲットサービスへの送信を円滑にします。ingress（受信）専用の Sink では適用されないため、"0" に設定可能です。 | `16`                 |
| **Request TTL**            | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストはバッファリング開始時からカウントされ、TTL を超えるか、Apache Doris からの応答やアックがタイムリーに得られない場合、期限切れとみなされます。 | `45` 秒              |
| **Health Check Interval**  | コネクターが Apache Doris への接続のヘルスチェックを自動的に実施する間隔（秒）を指定します。 | `15` 秒              |
| **Max Buffer Queue Size**  | コネクター内の各バッファワーカーがバッファリング可能な最大バイト数を指定します。バッファワーカーは Apache Doris への送信前にデータを一時保管し、データフローの効率化に寄与します。システム性能やデータ転送要件に応じて調整してください。 | `256` MB             |
| **Max Batch Size**         | EMQX から Apache Doris へ一度に転送するデータバッチの最大サイズを指定します。サイズを調整することでデータ転送の効率とパフォーマンスを最適化できます。<br />`1` に設定すると、データレコードはバッチ化せず個別に送信されます。 | `1`                  |
| **Query Mode**             | メッセージ送信要件に応じて、`asynchronous`（非同期）または `synchronous`（同期）クエリモードを選択できます。非同期モードでは Apache Doris への書き込みが MQTT メッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを Apache Doris 到着前に受信する可能性があります。 | `Async`              |
| **Inflight Window**        | "インフライトクエリ" とは、開始されたがまだ応答やアックを受け取っていないクエリを指します。コネクターが Apache Doris と通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode** が `async` の場合、このパラメータは特に重要です。同一 MQTT クライアントからのメッセージを厳密な順序で処理する必要がある場合は、値を 1 に設定してください。 | `100`                |
