# TDengineへのMQTTデータ取り込み

[TDengine](https://tdengine.com/)は、IoTおよび産業用IoT（IIoT）シナリオ向けに設計・最適化されたビッグデータプラットフォームです。中核には高性能な時系列データベースがあり、クラスター指向のアーキテクチャ、クラウドネイティブ設計、ミニマリスティックなアプローチが特徴です。EMQXはTDengineとの統合をサポートしており、多数のデバイスやデータコレクターからの大量データの送信、保存、分析、配信を可能にします。これにより、ビジネス運用状態のリアルタイム監視や早期警告を提供し、リアルタイムのビジネスインサイトを実現します。

本ページでは、EMQXとTDengine間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に説明します。

## 動作概要

TDengineデータ統合はEMQXの組み込み機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからTDengineへのデータ取り込みが簡素化され、複雑なコーディングが不要になります。EMQXはルールエンジンとSinkを通じてデバイスデータをTDengineに転送します。TDengineデータ統合により、MQTTメッセージやクライアントイベントをTDengineに保存できます。さらに、TDengine内のデータ更新や削除はイベントによってトリガー可能であり、デバイスのオンライン状態や過去のオンライン／オフラインイベントの記録が可能です。

以下の図は、産業用IoTにおけるEMQXとTDengineのデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration TDengine](./assets/emqx-integration-tdengine.png)

産業用エネルギー消費管理シナリオを例に、ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用デバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的にエネルギー消費データをパブリッシュします。このデータには生産ライン識別子やエネルギー消費値が含まれます。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンは、トピックマッチングに基づいて特定のソースからのメッセージを処理します。メッセージが到着するとルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などが含まれます。
3. **TDengineへのデータ取り込み**：ルールエンジンで定義されたルールが、メッセージをTDengineに書き込む操作をトリガーします。TDengine SinkはSQLテンプレートを提供し、特定のメッセージフィールドをTDengineの対応するテーブルやカラムに柔軟に書き込むデータ形式を定義可能です。

エネルギー消費データがTDengineに書き込まれた後、標準SQLと強力な時系列拡張機能を用いてリアルタイムにデータ分析が可能となり、多数のサードパーティのバッチ分析、リアルタイム分析、レポートツール、AI/MLツール、可視化ツールとシームレスに統合できます。例えば：

- Grafanaなどの可視化ツールに接続し、エネルギー消費データのチャートを生成・表示。
- ERPやPower BIなどのアプリケーションシステムに接続し、生産分析や生産計画の調整を実施。
- ビジネスシステムに接続し、リアルタイムのエネルギー使用分析を行い、データ駆動型のエネルギー管理を支援。

## 特長と利点

TDengineデータ統合は、以下の特長と利点をビジネスにもたらします。

- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを効率的に処理可能です。TDengineはデータの書き込み、保存、クエリに優れており、IoTシナリオのデータ処理ニーズをシステムに過負荷をかけずに満たします。
- **メッセージ変換**：メッセージはEMQXのルール内で豊富な処理や変換を経てからTDengineに書き込まれます。
- **クラスターとスケーラビリティ**：EMQXとTDengineはクラスター機能をサポートし、クラウドネイティブアーキテクチャ上に構築されているため、クラウドプラットフォームの弾力的なストレージ、計算、ネットワークリソースを最大限に活用し、ビジネスの成長に応じて柔軟な水平スケールが可能です。
- **高度なクエリ機能**：TDengineはタイムスタンプデータの効率的なクエリと分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから正確なインサイトを抽出可能です。

## はじめる前に

このセクションでは、TDengineデータ統合の作成を開始する前に必要な準備、TDengineサーバーのセットアップやデータテーブルの作成方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### TDengineの起動とデータベース作成

TDengineを起動またはTDengineサービスに接続し、データベースを作成するには以下の2つの方法があります。

:::: tabs

::: tab Docker

```bash
# TDengineのDockerイメージを起動
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

[TDengine Cloud](https://cloud.tdengine.com/)を利用している場合は、コンソールにログインし、インスタンスを選択して左側の**Explorer**をクリックしSQL実行ページに入ります。以下のコマンドを実行してデータベースを作成してください。

```bash
# データベース作成と選択

CREATE DATABASE mqtt;

use mqtt;
```

![create database](./assets/tdengine_cloud_create_db.jpg)

:::

::::

### TDengineでのデータテーブル作成

メッセージ保存とステータス記録用に、TDengineデータベース内に2つのデータテーブルを作成する必要があります。

1. 以下のSQL文を使用して、`t_mqtt_msg`テーブルを作成します。このテーブルは各メッセージのクライアントID、トピック、ペイロード、作成時間を保存します。

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

2. 以下のSQL文を使用して、`emqx_client_events`テーブルを作成します。このテーブルは各イベントのクライアントID、イベントタイプ、作成時間を保存します。

```sql
     CREATE TABLE emqx_client_events (
         ts timestamp,
         clientid VARCHAR(255),
         event VARCHAR(255)
       );
```

## コネクターの作成

このセクションでは、SinkをTDengineサーバーに接続するためのコネクター作成方法を説明します。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**ページで**TDengine**を選択し、**Next**をクリックします。

4. **Configuration**ステップで、接続先に応じて以下の情報を設定します。

   :::: tabs

   ::: tab TDengineへの接続

   以下の設定はEMQXとTDengineをローカルマシンで実行している場合の例です。リモートで実行している場合は適宜調整してください。

   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_tdengine`
   - **Server Host**：`http://127.0.0.1:6041` またはリモートのTDengineサーバーURL
   - **Database Name**：`mqtt`
   - **Username**：`root`
   - **Password**：`taosdata`
   - **Token**：空欄のまま。コネクターは**Username**と**Password**で認証を試みます。

   :::

   ::: tab TDengine Cloudへの接続

   1. TDengine Cloudコンソールページで正しい**Instance**を選択。

   2. 左メニューの**Programming**から**REST API**接続方法を選択し、以下の画像のように接続URLとTokenを取得します。

      ![url and token](./assets/tdengine_cloud_url_and_token.png)

   3. 以下のコネクター設定情報を入力します。

      - **Connector name**：英数字の組み合わせで名前を入力（例：`my_tdengine`）
      - **Server Host**：TDengine Cloudから提供された`TDENGINE_CLOUD_URL`（例：`https://gw.***.cloud.tdengine.com`）
      - **Database Name**：`mqtt`
      - **Username**：空欄
      - **Password**：空欄
      - **Token**：TDengine Cloudから提供された`TDENGINE_CLOUD_TOKEN`（例：`a2ba69cc6****f0c18cd`）

      :::

      ::::

5. 高度な設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがTDengineサーバーに接続できるかテストできます。

7. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてルール作成に進み、Sinkを使ってTDengineに転送するデータやクライアントイベントの記録を指定します。詳細は[メッセージ保存用のTDengine Sinkルール作成](#create-a-rule-with-tdengine-sink-for-message-storage)および[イベント記録用のTDengine Sinkルール作成](#create-a-rule-with-tdengine-sink-for-events-recording)を参照してください。

## メッセージ保存用のTDengine Sinkルール作成

このセクションでは、ダッシュボードでMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みのSinkを通じてTDengineの`t_mqtt_msg`テーブルに保存するルール作成方法を説明します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. 画面右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**でメッセージ保存用のルールを作成します。例えば、以下のステートメントはトピック`t/#`配下のMQTTメッセージをTDengineに保存します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要な全てのフィールドを`SELECT`句に含めていることを確認してください。

   ```sql
     SELECT
       *,
       now_timestamp('millisecond')  as ts
     FROM
       "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**や**Enable Test**をクリックしてSQLルールを学習・テストしてください。

   :::

4. + **Add Action**ボタンをクリックし、ルール発動時にトリガーされるアクションを定義します。このアクションによりEMQXはルールで処理したデータをTDengineに送信します。

5. **Type of Action**ドロップダウンから`TDengine`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのTDengine Sinkがあれば選択可能です。本デモでは新規Sinkを作成します。

6. Sinkの名前を入力します。英数字の組み合わせで指定してください。

7. **Connector**ドロップダウンから先ほど作成した`my_tdengine`を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメータは[コネクター作成](#create-a-connector)を参照してください。

8. Sinkの**SQL Template**を設定します。以下のSQLを使ってデータ挿入を完了できます。CSVファイルによるバッチ設定もサポートしています。詳細は[バッチ設定](#batch-setting)を参照してください。

   ::: warning 重要なお知らせ

   EMQX 6.3.1以降、Sink作成時にSQLテンプレートを解析し、SQLコンテキストに基づいてプレースホルダー値をエスケープし、サポートされない構文を拒否します。テンプレートは単一のTDengine `INSERT`文でなければなりません。複数テーブルの`VALUES`挿入や`USING ... TAGS`句をサポートします。ターゲットテーブル名は生の名前またはバッククォート付きでプレースホルダーを含めることができ、例：`test_${clientid}`。EMQXは完全なターゲットを1つの引用識別子としてレンダリングします。SQLコメント、`FILE`入力、追加文はサポートされません。

   この検証により、以前のバージョンで受け入れられたテンプレートが拒否される場合があります。アップグレード前に互換性のないテンプレートを修正してください。

   :::

   ::: tip

   EMQX 6.3.0では文字列プレースホルダー値を手動で引用符で囲む必要がありましたが、6.3.1以降はプレースホルダーを完全な値または文字列リテラル内で使用でき、EMQXがSQLコンテキストに応じて値をエスケープします。

   :::

   ```sql
   INSERT INTO t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) 
       VALUES (${ts}, '${id}', '${topic}', ${qos}, '${payload}', ${timestamp})
   ```

   SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQL template**上の**Undefined Vars as Null**スイッチでルールエンジンの動作を定義可能です。

   - **Disabled**（デフォルト）：ルールエンジンは未定義変数として文字列`undefined`をデータベースに挿入可能。
   - **Enabled**：未定義変数の場合、ルールエンジンは`NULL`を挿入。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **高度な設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択可能です。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**でSinkがTDengineに接続できるかテスト可能です。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認後、**Create**をクリックしてルールを作成します。

これでTDengine Sink用ルールの作成が完了しました。**Integration** -> **Rules**ページで新規ルールを確認できます。**Actions(Sink)**タブで新しいTDengine Sinkを確認可能です。

また、**Integration** -> **Flow Designer**でトポロジーを確認すると、トピック`t/#`配下のメッセージがルール`my_rule`で解析され、TDengineに送信・保存されていることが分かります。

### バッチ設定

TDengineでは1つのデータエントリに数百のデータポイントを含むことがあり、SQL文の作成が困難です。これに対応するため、EMQXはSQLのバッチ設定機能を提供しています。

SQLテンプレート編集時に、バッチ設定機能を使ってCSVファイルから挿入用フィールドをインポートできます。

1. **SQL Template**下の**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従いバッチ設定テンプレートファイルをダウンロードし、テンプレート内のフィールドのキー・値ペアを入力します。デフォルトのテンプレート内容は以下の通りです。

   | Field      | Value             | Char Value | 備考（任意）         |
   | ---------- | ----------------- | ---------- | -------------------- |
   | ts         | now               | FALSE      | 例                   |
   | msgid      | ${id}             | TRUE       |                      |
   | mqtt_topic | ${topic}          | TRUE       |                      |
   | qos        | ${qos}            | FALSE      |                      |
   | temp       | ${payload.temp}   | FALSE      |                      |
   | hum        | ${payload.hum}    | FALSE      |                      |
   | status     | ${payload.status} | FALSE      |                      |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数または`${var}`形式のプレースホルダーをサポート。SQLでは文字列は引用符で囲む必要がありますが、テンプレートファイル内では不要。文字列かどうかは`Char Value`列で指定。
   - **Char Value**：フィールドが文字列型かどうかを指定。SQL生成時に引用符を付加。文字列型なら`TRUE`または`1`、そうでなければ`FALSE`または`0`を入力。
   - **備考**：CSVファイル内の注釈用で、EMQXへのインポート対象外。

   CSVファイルのバッチ設定データは2048行を超えないようにしてください。

3. 入力済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックしバッチ設定を完了します。

4. インポート後、**SQL Template**内でテーブル名設定やSQLコードの整形などをさらに調整可能です。

## イベント記録用のTDengine Sinkルール作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みSinkを通じてTDengineの`emqx_client_events`テーブルに保存するルール作成方法を説明します。

ルール作成手順は[メッセージ保存用のTDengine Sinkルール作成](#メッセージ保存用のtdengine-sinkルール作成)とほぼ同様で、SQLルール構文とSQLテンプレートのみ異なります。

オンライン／オフライン状態記録用のSQLルール構文は以下の通りです。

```sql
SELECT
      *,
      now_timestamp('millisecond')  as ts
    FROM 
      "$events/client_connected", "$events/client_disconnected"
```

SinkのSQLテンプレートは以下の通りです。

上記のSQLテンプレート制限はこのテンプレートにも適用されます。

以下のテンプレートはシングルクォート内の文字列プレースホルダーを使用しています。SQL文の末尾にセミコロン（`;`）を付けないでください。

```sql
INSERT INTO emqx_client_events(ts, clientid, event) VALUES (
      ${ts},
      '${clientid}',
      '${event}'
    )
```

## ルールのテスト

MQTTXを使ってトピック`t/1`にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello TDengine" }'
```

2つのSinkの稼働状況を確認すると、1件の新規受信メッセージと1件の新規送信メッセージ、2件のイベントレコードがあるはずです。

`t_mqtt_msg`データテーブルにデータが書き込まれているか確認します。

```bash
taos> select * from t_mqtt_msg;
           ts            |             msgid              |           mqtt_topic           | qos  |            payload             |         arrived         |
==============================================================================================================================================================
 2023-02-13 06:10:53.787 | 0005F48EB5A83865F440000014F... | t/1                            |    0 | { "msg": "hello TDengine" }    | 2023-02-13 06:10:53.787 |
Query OK, 1 row(s) in set (0.002968s)
```

`emqx_client_events`テーブル：

```bash
taos> select * from emqx_client_events;
           ts            |            clientid            |             event              |
============================================================================================
 2023-02-13 06:10:53.777 | emqx_c                         | client.connected               |
 2023-02-13 06:10:53.791 | emqx_c                         | client.disconnected            |
Query OK, 2 row(s) in set (0.002327s)
```
