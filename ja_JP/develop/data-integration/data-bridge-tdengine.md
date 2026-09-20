# TDengineへのMQTTデータ取り込み

[TDengine](https://tdengine.com/)は、IoTおよび産業用IoT（IIoT）シナリオ向けに設計・最適化されたビッグデータプラットフォームです。中核には高性能な時系列データベースがあり、クラスター指向のアーキテクチャ、クラウドネイティブ設計、ミニマリスティックなアプローチを特徴としています。EMQXはTDengineとの連携をサポートしており、多数のデバイスやデータコレクターからの大量データの送信、保存、分析、配信を可能にします。これにより、ビジネス運用状態のリアルタイム監視や早期警告を実現し、リアルタイムのビジネスインサイトを提供します。

本ページでは、EMQXとTDengine間のデータ統合について包括的に紹介し、実際の作成および検証手順を説明します。

## 動作概要

TDengineデータ統合はEMQXに組み込まれた機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、EMQXからTDengineへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。EMQXはルールエンジンとSinkを介してデバイスデータをTDengineに転送します。TDengineデータ統合により、MQTTメッセージやクライアントイベントをTDengineに保存可能です。さらに、TDengine内のデータ更新や削除はイベントによりトリガーされ、デバイスのオンライン状態や過去のオンライン/オフラインイベントの記録が可能となります。

以下の図は、産業用IoTにおけるEMQXとTDengineの典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration TDengine](./assets/emqx-integration-tdengine.png)

産業用エネルギー消費管理シナリオを例にとると、ワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用デバイスはMQTTプロトコルを介してEMQXに正常に接続し、定期的に生産ライン識別子やエネルギー消費値を含むエネルギー消費データをパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージを処理します。メッセージ到着時にルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの補強などが含まれます。
3. **TDengineへのデータ取り込み**：ルールエンジンで定義されたルールがメッセージのTDengineへの書き込み操作をトリガーします。TDengine SinkはSQLテンプレートを提供し、特定のメッセージフィールドを対応するテーブルやカラムに柔軟に書き込むデータ形式を定義可能です。

エネルギー消費データがTDengineに書き込まれた後は、標準SQLと強力な時系列拡張機能を用いてリアルタイムにデータ分析が可能となり、多数のサードパーティ製バッチ分析、リアルタイム分析、レポーティングツール、AI/MLツール、可視化ツールとシームレスに連携できます。例えば：

- Grafanaなどの可視化ツールに接続し、エネルギー消費データのグラフを生成・表示。
- ERPやPower BIなどのアプリケーションシステムに接続し、生産分析や生産計画の調整。
- ビジネスシステムに接続し、リアルタイムのエネルギー使用分析を実施、データ駆動型のエネルギー管理を促進。

## 特徴と利点

TDengineデータ統合は以下の特徴と利点をビジネスにもたらします：

- **効率的なデータ処理**：EMQXは大量のIoTデバイス接続とメッセージスループットを効率的に処理可能です。TDengineはデータの書き込み、保存、クエリに優れ、IoTシナリオのデータ処理ニーズをシステムに負荷をかけずに満たします。
- **メッセージ変換**：メッセージはEMQXのルール内で豊富な処理や変換を経てからTDengineに書き込まれます。
- **クラスターとスケーラビリティ**：EMQXとTDengineはクラスター機能をサポートし、クラウドネイティブアーキテクチャ上に構築されています。これによりクラウドプラットフォームの弾力的なストレージ、計算、ネットワーク資源をフル活用でき、ビジネスの成長に応じた柔軟な水平スケーリングが可能です。
- **高度なクエリ機能**：TDengineはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから精緻なインサイトを抽出できます。

## はじめる前に

本節では、TDengineデータ統合の作成を始める前に必要な準備、TDengineサーバーのセットアップおよびデータテーブルの作成方法を説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### TDengineの起動およびデータベース作成

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

[TDengine Cloud](https://cloud.tdengine.com/)を利用している場合は、コンソールにログインし、インスタンスを選択して左メニューの**Explorer**をクリックしSQL実行ページに入ります。以下のSQL文を実行してデータベースを作成します。

```bash
# データベース作成と選択

CREATE DATABASE mqtt;

use mqtt;
```

![create database](./assets/tdengine_cloud_create_db.jpg)

:::

::::

### TDengineでのデータテーブル作成

メッセージ保存と状態記録のため、TDengineデータベース内に2つのデータテーブルを作成します。

1. 以下のSQL文で、クライアントID、トピック、ペイロード、作成時間を保存するデータテーブル`t_mqtt_msg`を作成します。

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

2. 以下のSQL文で、クライアントID、イベントタイプ、作成時間を保存するデータテーブル`emqx_client_events`を作成します。

```sql
     CREATE TABLE emqx_client_events (
         ts timestamp,
         clientid VARCHAR(255),
         event VARCHAR(255)
       );
```

## コネクターの作成

本節では、SinkをTDengineサーバーに接続するためのコネクター作成方法を説明します。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックします。

3. **Create Connector**ページで**TDengine**を選択し、**Next**をクリックします。

4. **Configuration**ステップで、接続先に応じて以下の情報を設定します。

   :::: tabs

   ::: tab TDengineへの接続

   以下の設定はEMQXとTDengineをローカルマシンで稼働させている場合の例です。リモートで稼働している場合は適宜調整してください。

   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_tdengine`
   - **Server Host**：`http://127.0.0.1:6041` またはリモートの場合は実際のURL
   - **Database Name**：`mqtt`
   - **Username**：`root`
   - **Password**：`taosdata`
   - **Token**：空欄のまま。コネクターは**Username**と**Password**で認証を試みます。

   :::

   ::: tab TDengine Cloudへの接続

   1. TDengine Cloudコンソールで正しい**Instance**を選択します。

   2. 左メニューの**Programming**から**REST API**接続方法を選択します。以下の画像のように接続URLとTokenを取得できます。

      ![url and token](./assets/tdengine_cloud_url_and_token.png)

   3. 以下のコネクター設定情報を入力します：

      - **Connector name**：英数字の組み合わせでコネクター名（例：`my_tdengine`）
      - **Server Host**：TDengine Cloudが提供する`TDENGINE_CLOUD_URL`（例：`https://gw.***.cloud.tdengine.com`）
      - **Database Name**：`mqtt`
      - **Username**：空欄
      - **Password**：空欄
      - **Token**：TDengine Cloudが提供する`TDENGINE_CLOUD_TOKEN`（例：`a2ba69cc6****f0c18cd`）

      :::
   
      ::::

5. 高度な設定（任意）：詳細は[Sinkの特徴](./data-bridges.md#features-of-sink)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがTDengineサーバーに接続できるかテスト可能です。

7. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択できます。後者を選ぶと、Sinkを指定してTDengineに転送するデータやクライアントイベントの記録を行うルール作成に進めます。詳細は[メッセージ保存用のTDengine Sinkルール作成](#create-a-rule-with-tdengine-sink-for-message-storage)および[イベント記録用のTDengine Sinkルール作成](#create-a-rule-with-tdengine-sink-for-events-recording)を参照してください。

## メッセージ保存用のTDengine Sinkルール作成

本節では、MQTTトピック`t/#`のメッセージを処理し、処理済みデータを設定済みのSinkを介してTDengineデータテーブル`t_mqtt_msg`に保存するルールをダッシュボードで作成する方法を示します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**でメッセージ保存用のルールを作成します。例えば、以下のSQL文はトピック`t/#`配下のMQTTメッセージをTDengineに保存することを意味します。

   注意：独自のSQL構文を指定する場合は、Sinkが必要とする全てのフィールドを`SELECT`句に含めていることを確認してください。

   ```sql
     SELECT
       *,
       now_timestamp('millisecond')  as ts
     FROM
       "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールを学習・テストしてください。

   :::

4. + **Add Action**ボタンをクリックし、ルールにトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをTDengineに送信します。

5. **Type of Action**ドロップダウンリストから`TDengine`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのTDengine Sinkがあれば選択可能ですが、本デモでは新規Sinkを作成します。

6. Sinkの名前を入力します。英数字の組み合わせで指定してください。

7. **Connector**ドロップダウンから先ほど作成した`my_tdengine`を選択します。新規コネクターはドロップダウン横のボタンから作成可能です。設定パラメータは[コネクター作成](#create-a-connector)を参照してください。

8. Sinkの**SQL Template**を設定します。以下のSQLでデータ挿入が可能です。CSVファイルによるバッチ設定もサポートします。詳細は[バッチ設定](#batch-setting)を参照してください。

   ::: tip

   EMQX 5.1.1で破壊的変更があります。これ以前のバージョンでは文字列型値は自動的に引用符で囲まれていましたが、5.1.1以降はユーザーが手動で引用符を付ける必要があります。

   :::

   ```sql
   INSERT INTO t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) 
       VALUES (${ts}, '${id}', '${topic}', ${qos}, '${payload}', ${timestamp})
   ```

   SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQL template**上部の**Undefined Vars as Null**スイッチでルールエンジンの動作を切り替えられます：

   - **Disabled**（デフォルト）：ルールエンジンは文字列`undefined`をDBに挿入可能。
   - **Enabled**：変数が未定義の場合、ルールエンジンは`NULL`をDBに挿入。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **高度な設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの特徴](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**でSinkがTDengineに接続できるかテスト可能です。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**ボタンをクリックしルールを生成します。

これでTDengine Sink用のルール作成が完了しました。**Integration** -> **Rules**ページで新規ルールを確認できます。**Actions(Sink)**タブをクリックすると新しいTDengine Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`配下のメッセージがルール`my_rule`で解析されTDengineに送信・保存されている様子が確認できます。

### バッチ設定

TDengineでは1つのデータエントリーに数百のデータポイントが含まれることがあり、SQL文の作成が困難になる場合があります。これに対応するため、EMQXはSQLのバッチ設定機能を提供しています。

SQLテンプレート編集時に、バッチ設定機能を使ってCSVファイルから挿入操作用のフィールドをインポート可能です。

1. **SQL Template**下の**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従いバッチ設定テンプレートファイルをダウンロードし、テンプレート内のフィールドのキーと値のペアを記入します。デフォルトのテンプレート内容は以下の通りです：

   | Field      | Value             | Char Value | 備考（任意）       |
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
   - **Char Value**：フィールドが文字列型かどうかを指定し、インポート時にSQL生成時に引用符を付加します。文字列型なら`TRUE`または`1`、そうでなければ`FALSE`または`0`を記入。
   - **備考**：CSVファイル内の注釈用で、EMQXへのインポート対象外です。

   バッチ設定用CSVファイルの行数は2048行を超えないようにしてください。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックしバッチ設定を完了します。

4. インポート後、**SQL Template**内でテーブル名の設定やSQLコードの整形など、さらにSQLを調整可能です。

## イベント記録用のTDengine Sinkルール作成

本節では、クライアントのオンライン/オフライン状態を記録し、イベントデータを設定済みのSinkを介してTDengineテーブル`emqx_client_events`に保存するルール作成方法を示します。

ルール作成手順は[メッセージ保存用のTDengine Sinkルール作成](#メッセージ保存用のTDengine-sinkルール作成)とほぼ同様で、SQLルール構文とSQLテンプレートのみ異なります。

オンライン/オフライン状態記録用のSQLルール構文は以下の通りです：

```sql
SELECT
      *,
      now_timestamp('millisecond')  as ts
    FROM 
      "$events/client_connected", "$events/client_disconnected"
```

Sink用のSQLテンプレートは以下の通りです：

注意：フィールドは引用符を含めず、SQL文の末尾にセミコロン（`;`）を付けないでください。

```sql
INSERT INTO emqx_client_events(ts, clientid, event) VALUES (
      ${ts},
      '${clientid}',
      '${event}'
    )
```

## ルールのテスト

MQTTXを使用してトピック`t/1`にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello TDengine" }'
```

2つのSinkの稼働状況を確認すると、1件の新規受信メッセージ、1件の新規送信メッセージ、2件のイベント記録があるはずです。

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
