# TDengineへのMQTTデータ取り込み

[TDengine](https://tdengine.com/) は、特にモノのインターネット（IoT）および産業用モノのインターネット（IIoT）シナリオ向けに設計・最適化されたビッグデータプラットフォームです。中核には高性能な時系列データベースがあり、クラスター指向のアーキテクチャ、クラウドネイティブ設計、ミニマリスティックなアプローチが特徴です。EMQXはTDengineとの連携をサポートしており、多数のデバイスやデータコレクターからの大量データの送信、保存、分析、配信を可能にします。これにより、ビジネス運用状態のリアルタイム監視や早期警告を実現し、リアルタイムのビジネスインサイトを提供します。

本ページでは、EMQXとTDengine間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に説明します。

## 動作概要

TDengineデータ統合はEMQXの組み込み機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからTDengineへのデータ取り込みが簡素化され、複雑なコーディングは不要です。EMQXはルールエンジンとSinkを介してデバイスデータをTDengineに転送します。TDengineデータ統合を通じて、MQTTメッセージやクライアントイベントをTDengineに保存できます。さらに、TDengine内のデータ更新や削除はイベントによってトリガーされ、デバイスのオンライン状態や過去のオンライン/オフラインイベントの記録が可能です。

以下の図は、産業用IoTにおけるEMQXとTDengineのデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration TDengine](./assets/emqx-integration-tdengine.png)

産業用エネルギー消費管理シナリオを例に、ワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用デバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的にエネルギー消費データをパブリッシュします。このデータには生産ライン識別子やエネルギー消費値が含まれます。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンは、トピックマッチングに基づき特定のソースからのメッセージを処理します。メッセージ到着時にルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、メッセージへのコンテキスト情報の付加などが含まれます。
3. **TDengineへのデータ取り込み**：ルールエンジンで定義されたルールが、メッセージをTDengineに書き込む操作をトリガーします。TDengine SinkはSQLテンプレートを提供し、特定のメッセージフィールドをTDengineの対応テーブルおよびカラムに柔軟に書き込むデータ形式を定義可能です。

エネルギー消費データがTDengineに書き込まれた後は、標準SQLと強力な時系列拡張機能を用いてリアルタイム分析が可能であり、多数のサードパーティのバッチ分析、リアルタイム分析、レポーティングツール、AI/MLツール、可視化ツールとシームレスに連携できます。例えば：

- Grafanaなどの可視化ツールに接続し、エネルギー消費データのチャート表示を行う。
- ERPやPower BIなどのアプリケーションシステムに接続し、生産分析や生産計画の調整を行う。
- ビジネスシステムに接続し、リアルタイムのエネルギー使用分析を実施し、データ駆動型のエネルギー管理を支援する。

## 特長と利点

TDengineデータ統合は、ビジネスに以下の特長と利点をもたらします：

- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを効率的に処理できます。TDengineはデータの書き込み、保存、クエリに優れており、IoTシナリオのデータ処理ニーズをシステムに負荷をかけずに満たします。
- **メッセージ変換**：メッセージはEMQXのルール内で豊富な処理・変換が可能であり、その後TDengineに書き込まれます。
- **クラスターとスケーラビリティ**：EMQXとTDengineはクラスター機能をサポートし、クラウドネイティブアーキテクチャ上に構築されているため、クラウドプラットフォームの弾力的なストレージ、計算、ネットワーク資源を最大限活用でき、ビジネスの成長に応じて柔軟な水平スケーリングが可能です。
- **高度なクエリ機能**：TDengineはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから正確なインサイトを抽出できます。

## はじめる前に

このセクションでは、TDengineデータ統合の作成を開始する前に必要な準備、TDengineサーバーのセットアップおよびデータテーブルの作成方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### TDengineの起動とデータベース作成

TDengineを起動またはTDengineサービスに接続し、データベースを作成するには以下の2つの方法があります。

:::: tabs

::: tab Docker

```bash
# TDengineのDockerイメージを起動する
docker run --name TDengine -p 6041:6041 tdengine/tdengine

# コンテナにアクセス
docker exec -it TDengine bash

# コンテナ内でTDengineサーバーを起動
taos

# データベースを作成し選択
CREATE DATABASE mqtt;

use mqtt;
```

:::

::: tab TDengine Cloud

[TDengine Cloud](https://cloud.tdengine.com/)を利用している場合は、コンソールにログインし、インスタンスを選択して左側の**Explorer**をクリックし、SQL実行画面に入ります。以下のステートメントを実行してデータベースを作成してください。

```bash
# データベース作成と選択

CREATE DATABASE mqtt;

use mqtt;
```

![create database](./assets/tdengine_cloud_create_db.jpg)

:::

::::

### TDengineでのデータテーブル作成

メッセージ保存と状態記録のために、TDengineデータベースに2つのデータテーブルを作成する必要があります。

1. 以下のSQL文を使って、メッセージのクライアントID、トピック、ペイロード、作成時間を保存するデータテーブル `t_mqtt_msg` を作成します。

```sql
   CREATE TABLE t_mqtt_msg (
       ts timestamp,
       msgid NCHAR(64),
       mqtt_topic NCHAR(255),
       qos TINYINT,
       payload BINARY(1024),
       arrived timestamp
     );
```

2. 以下のSQL文を使って、クライアントID、イベントタイプ、作成時間を保存するデータテーブル `emqx_client_events` を作成します。

```sql
     CREATE TABLE emqx_client_events (
         ts timestamp,
         clientid VARCHAR(255),
         event VARCHAR(255)
       );
```

## コネクターの作成

このセクションでは、SinkをTDengineサーバーに接続するためのコネクターの作成方法を説明します。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **TDengine** を選択し、**Next** をクリックします。

4. **Configuration** ステップで、接続先に応じて以下の情報を設定します。

   :::: tabs

   ::: tab TDengineに接続

   以下の設定は、EMQXとTDengineをローカルマシンで実行している場合の例です。リモートで実行している場合は適宜設定を調整してください。

   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_tdengine`
   - **Server Host**：`http://127.0.0.1:6041` またはリモートのTDengineサーバーの実際のURL
   - **Database Name**：`mqtt`
   - **Username**：`root`
   - **Password**：`taosdata`
   - **Token**：空欄のまま。コネクターは**Username**と**Password**で認証を試みます。

   :::

   ::: tab TDengine Cloudに接続

   1. TDengine Cloudコンソールページで正しい**Instance**を選択します。

   2. 左メニューの**Programming**に移動し、**REST API**接続方法を選択します。以下の画像のように接続URLとTokenを取得します。

      ![url and token](./assets/tdengine_cloud_url_and_token.png)

   3. 以下のコネクター設定情報を入力します：

      - **Connector name**：英数字の組み合わせで名前を入力（例：`my_tdengine`）
      - **Server Host**：TDengine Cloudが提供する`TDENGINE_CLOUD_URL`の値（例：`https://gw.***.cloud.tdengine.com`）
      - **Database Name**：`mqtt`
      - **Username**：空欄
      - **Password**：空欄
      - **Token**：TDengine Cloudが提供する`TDENGINE_CLOUD_TOKEN`の値（例：`a2ba69cc6****f0c18cd`）

      :::

      ::::

5. 高度な設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがTDengineサーバーに接続できるかテストできます。

7. ページ下部の**Create**ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを使ったルール作成に進めます。詳細は[メッセージ保存用のTDengine Sinkを使ったルール作成](#create-a-rule-with-tdengine-sink-for-message-storage)および[イベント記録用のTDengine Sinkを使ったルール作成](#create-a-rule-with-tdengine-sink-for-events-recording)を参照してください。

## メッセージ保存用のTDengine Sinkを使ったルール作成

このセクションでは、ソースMQTTトピック `t/#` からのメッセージを処理し、処理済みデータを設定済みのSinkを介してTDengineのデータテーブル `t_mqtt_msg` に保存するルールの作成方法を示します。

1. EMQXダッシュボードで、**Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` と入力し、**SQL Editor**でメッセージ保存用のルールを作成します。例えば、以下のステートメントはトピック `t/#` 配下のMQTTメッセージをTDengineに保存します。

   注意：独自のSQL文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`句に含めてください。

   ```sql
     SELECT
       *,
       now_timestamp('millisecond')  as ts
     FROM
       "t/#"
   ```

   ::: tip

   初心者の方は、**SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストを行うことをおすすめします。

   :::

4. + **Add Action** ボタンをクリックし、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理されたデータをTDengineに送信します。

5. **Type of Action** ドロップダウンリストから `TDengine` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みのTDengine Sinkを選択することも可能ですが、本デモでは新規Sinkを作成します。

6. Sinkの名前を入力します。英数字の組み合わせで指定してください。

7. **Connector** ドロップダウンから先ほど作成した `my_tdengine` を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメーターは[コネクターの作成](#create-a-connector)を参照してください。

8. Sinkの**SQL Template**を設定します。以下のSQLを使ってデータ挿入を完了できます。CSVファイルによるバッチ設定もサポートしています。詳細は[バッチ設定](#batch-setting)を参照してください。

   ::: warning 重要なお知らせ

   EMQX 5.10.5以降、Sink作成時にSQLテンプレートを解析し、SQLコンテキストに基づいてプレースホルダー値をエスケープし、サポートされていない構文を拒否します。テンプレートは単一のTDengine `INSERT`文でなければなりません。複数テーブルの`VALUES`挿入や`USING ... TAGS`句をサポートします。ターゲットテーブル名はプレースホルダーを含む素の名前またはバッククォート付き名前が可能で、例えば `test_${clientid}` のように指定できます。EMQXは完全なターゲットを1つの引用識別子としてレンダリングします。SQLコメント、`FILE`入力、追加ステートメントはサポートされません。

   この検証により、以前のバージョンで許容されていたテンプレートが拒否される場合があります。アップグレード前に互換性のないテンプレートを修正してください。

   :::

   ::: tip

   EMQX 5.1.1以前は文字列プレースホルダー値に自動で引用符を付けていました。EMQX 5.1.1から5.10.4までは手動で引用符を追加する必要がありました。EMQX 5.10.5以降は、プレースホルダーは完全な値としても文字列リテラル内でも使用でき、SQLコンテキストに応じて値がエスケープされます。

   :::

   ```sql
   INSERT INTO t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived)
       VALUES (${ts}, '${id}', '${topic}', ${qos}, '${payload}', ${timestamp})
   ```

   SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQL template**上の**Undefined Vars as Null**スイッチでルールエンジンの動作を設定できます：

   - **Disabled**（デフォルト）：ルールエンジンは文字列 `undefined` をデータベースに挿入します。
   - **Enabled**：変数が未定義の場合、ルールエンジンは `NULL` を挿入します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **高度な設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがTDengineに接続できるかテストできます。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックし、ルールを生成します。

これでTDengine Sink用のルール作成が完了しました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると新しいTDengine Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック `t/#` 配下のメッセージがルール `my_rule` によって解析され、TDengineに送信・保存されていることが確認できます。

### バッチ設定

TDengineでは1つのデータエントリに数百のデータポイントを含むことがあり、SQL文の作成が困難になる場合があります。この問題に対応するため、EMQXはSQLのバッチ設定機能を提供しています。

SQLテンプレート編集時に、バッチ設定機能を使ってCSVファイルから挿入用フィールドをインポートできます。

1. **SQL Template**下の**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従ってバッチ設定テンプレートファイルをダウンロードし、テンプレート内のフィールドのキー・バリューを入力します。デフォルトのテンプレート内容は以下の通りです：

   | Field      | Value             | Char Value | 備考（任意）        |
   | ---------- | ----------------- | ---------- | ------------------ |
   | ts         | now               | FALSE      | 例示備考           |
   | msgid      | ${id}             | TRUE       |                    |
   | mqtt_topic | ${topic}          | TRUE       |                    |
   | qos        | ${qos}            | FALSE      |                    |
   | temp       | ${payload.temp}   | FALSE      |                    |
   | hum        | ${payload.hum}    | FALSE      |                    |
   | status     | ${payload.status} | FALSE      |                    |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数または`${var}`形式のプレースホルダーをサポート。SQLでは文字列型は引用符で囲む必要がありますが、テンプレートファイル内では不要です。文字列型かどうかは`Char Value`列で指定します。
   - **Char Value**：フィールドが文字列型かどうかを指定し、インポート時にSQL生成時に引用符を付加します。文字列型の場合は`TRUE`または`1`、そうでなければ`FALSE`または`0`を記入します。
   - **備考**：CSVファイル内の注釈用で、EMQXへのインポート対象ではありません。

   バッチ設定用CSVファイルのデータ行数は2048行を超えないようにしてください。

3. 入力済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックし、バッチ設定を完了します。

4. インポート後、**SQL Template**でテーブル名の設定やSQLコードの整形などをさらに調整できます。

## イベント記録用のTDengine Sinkを使ったルール作成

このセクションでは、クライアントのオンライン/オフライン状態を記録し、イベントデータを設定済みのSinkを介してTDengineテーブル `emqx_client_events` に保存するルール作成方法を示します。

ルール作成手順は[メッセージ保存用のTDengine Sinkを使ったルール作成](#メッセージ保存用のtdengine-sinkを使ったルール作成)に類似していますが、SQLルール文およびSQLテンプレートが異なります。

オンライン/オフライン状態記録用のSQLルール文は以下の通りです：

```sql
SELECT
      *,
      now_timestamp('millisecond')  as ts
    FROM
      "$events/client_connected", "$events/client_disconnected"
```

Sink用のSQLテンプレートは以下の通りです：

以下のテンプレートはシングルクォート内の文字列プレースホルダーを使用しています。SQL文の末尾にセミコロン（`;`）を付けないでください。

```sql
INSERT INTO emqx_client_events(ts, clientid, event) VALUES (
      ${ts},
      '${clientid}',
      '${event}'
    )
```

## ルールのテスト

MQTTXを使ってトピック `t/1` にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello TDengine" }'
```

2つのSinkの稼働状況を確認すると、1件の新規受信メッセージと1件の新規送信メッセージ、2件のイベントレコードが存在するはずです。

`t_mqtt_msg` データテーブルにデータが書き込まれているか確認します。

```bash
taos> select * from t_mqtt_msg;
           ts            |             msgid              |           mqtt_topic           | qos  |            payload             |         arrived         |
==============================================================================================================================================================
 2023-02-13 06:10:53.787 | 0005F48EB5A83865F440000014F... | t/1                            |    0 | { "msg": "hello TDengine" }    | 2023-02-13 06:10:53.787 |
Query OK, 1 row(s) in set (0.002968s)
```

`emqx_client_events` テーブル：

```bash
taos> select * from emqx_client_events;
           ts            |            clientid            |             event              |
============================================================================================
 2023-02-13 06:10:53.777 | emqx_c                         | client.connected               |
 2023-02-13 06:10:53.791 | emqx_c                         | client.disconnected            |
Query OK, 2 row(s) in set (0.002327s)
```
