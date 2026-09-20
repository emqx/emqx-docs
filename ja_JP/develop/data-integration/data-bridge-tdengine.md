# TDengineへのMQTTデータ取り込み

[TDengine](https://tdengine.com/) は、IoTおよび産業用IoT（IIoT）シナリオ向けに設計・最適化されたビッグデータプラットフォームです。中核には高性能な時系列データベースがあり、クラスター指向のアーキテクチャ、クラウドネイティブ設計、ミニマリスティックなアプローチが特徴です。EMQXはTDengineとの連携をサポートしており、多数のデバイスやデータコレクターからの大量データの送信、保存、分析、配信を可能にします。これにより、ビジネス運用状態のリアルタイム監視や早期警告が実現し、リアルタイムのビジネスインサイトを提供します。

本ページでは、EMQXとTDengineのデータ統合について包括的に紹介し、データ統合の作成と検証に関する実践的な手順を説明します。

## 動作概要

TDengineデータ統合はEMQXに組み込まれた機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからTDengineへのデータ取り込みが簡素化され、複雑なコーディングが不要になります。EMQXはルールエンジンとSinkを介してデバイスデータをTDengineに転送します。TDengineデータ統合により、MQTTメッセージやクライアントイベントをTDengineに保存可能です。さらに、TDengine内のデータ更新や削除をイベントでトリガーできるため、デバイスのオンライン状態や過去のオンライン／オフラインイベントの記録が可能となります。

以下の図は、産業用IoTにおけるEMQXとTDengineデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration TDengine](./assets/emqx-integration-tdengine.png)

産業用エネルギー消費管理シナリオを例にすると、ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用デバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的にエネルギー消費データをMQTTプロトコルでパブリッシュします。このデータには生産ラインの識別子やエネルギー消費値が含まれます。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンは、トピックマッチングに基づき特定のソースからのメッセージを処理します。メッセージが到着すると、ルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などが含まれます。
3. **TDengineへのデータ取り込み**：ルールエンジンで定義されたルールは、メッセージをTDengineに書き込む操作をトリガーします。TDengine SinkはSQLテンプレートを提供し、特定のメッセージフィールドを対応するテーブルやカラムに柔軟に書き込むデータ形式を定義できます。

エネルギー消費データがTDengineに書き込まれた後は、標準SQLと強力な時系列拡張機能を用いてリアルタイムにデータ分析が可能であり、多数のサードパーティのバッチ分析、リアルタイム分析、レポーティングツール、AI/MLツール、可視化ツールとシームレスに統合できます。例えば：

- Grafanaなどの可視化ツールに接続してチャートを生成し、エネルギー消費データを表示。
- ERPやPower BIなどのアプリケーションシステムに接続し、生産分析や生産計画の調整を実施。
- ビジネスシステムに接続してリアルタイムのエネルギー使用分析を行い、データ駆動型のエネルギー管理を促進。

## 特長とメリット

TDengineデータ統合は、ビジネスに以下の特長と利点をもたらします。

- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを効率的に処理可能です。TDengineはデータ書き込み、保存、クエリに優れており、IoTシナリオのデータ処理ニーズをシステムに過負荷をかけずに満たします。
- **メッセージ変換**：メッセージはEMQXルール内で豊富な処理や変換を経てからTDengineに書き込まれます。
- **クラスターとスケーラビリティ**：EMQXとTDengineはクラスター機能をサポートし、クラウドネイティブアーキテクチャ上に構築されています。これによりクラウドプラットフォームの弾力的なストレージ、コンピューティング、ネットワーク資源を最大限に活用でき、ビジネスの成長に応じて柔軟な水平スケーリングが可能です。
- **高度なクエリ機能**：TDengineはタイムスタンプデータの効率的なクエリと分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから精緻なインサイトを抽出できます。

## はじめる前に

このセクションでは、TDengineデータ統合の作成を開始する前に必要な準備、TDengineサーバーのセットアップおよびデータテーブルの作成方法を説明します。

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

[TDengine Cloud](https://cloud.tdengine.com/)を利用している場合は、コンソールにログインし、インスタンスを選択して左側の**Explorer**をクリックしSQL実行ページに入ります。以下の文を実行してデータベースを作成してください。

```bash
# データベース作成と選択

CREATE DATABASE mqtt;

use mqtt;
```

![create database](./assets/tdengine_cloud_create_db.jpg)

:::

::::

### TDengineでのデータテーブル作成

メッセージ保存と状態記録用に、TDengineデータベース内に2つのデータテーブルを作成する必要があります。

1. 以下のSQL文で、クライアントID、トピック、ペイロード、作成時刻を保存するデータテーブル`t_mqtt_msg`を作成します。

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

2. 以下のSQL文で、クライアントID、イベント種別、作成時刻を保存するデータテーブル`emqx_client_events`を作成します。

```sql
     CREATE TABLE emqx_client_events (
         ts timestamp,
         clientid VARCHAR(255),
         event VARCHAR(255)
       );
```

## コネクターの作成

このセクションでは、SinkをTDengineサーバーに接続するためのコネクター作成方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**ページで**TDengine**を選択し、**Next**をクリックします。

4. **Configuration**ステップで、接続先に応じて以下の情報を設定します。

   :::: tabs

   ::: tab TDengineへの接続

   以下の設定はEMQXとTDengineをローカルマシンで動作させている場合の例です。リモートで稼働している場合は適宜調整してください。

   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_tdengine`。
   - **Server Host**：`http://127.0.0.1:6041` またはTDengineサーバーの実際のURL。
   - **Database Name**：`mqtt` を入力。
   - **Username**：`root` を入力。
   - **Password**：`taosdata` を入力。
   - **Token**：空欄のまま。コネクターは**Username**と**Password**で認証を試みます。

   :::

   ::: tab TDengine Cloudへの接続

   1. TDengine Cloudコンソールページで正しい**Instance**を選択します。

   2. 左メニューの**Programming**に移動し、**REST API**接続方法を選択します。以下の画像のように接続用URLとTokenが取得できます。

      ![url and token](./assets/tdengine_cloud_url_and_token.png)

   3. 以下のコネクター設定情報を入力します。

      - **Connector name**：英数字の組み合わせで名前を入力（例：`my_tdengine`）。
      - **Server Host**：TDengine Cloudから提供された`TDENGINE_CLOUD_URL`を入力。例：`https://gw.***.cloud.tdengine.com`。
      - **Database Name**：`mqtt` を入力。
      - **Username**：空欄。
      - **Password**：空欄。
      - **Token**：TDengine Cloudから提供された`TDENGINE_CLOUD_TOKEN`を入力。例：`a2ba69cc6****f0c18cd`。
   
      :::
   
      ::::

5. 詳細設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがTDengineサーバーに接続できるかテストできます。

7. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを用いたルール作成に進めます。詳細は[メッセージ保存用のTDengine Sinkを使ったルール作成](#create-a-rule-with-tdengine-sink-for-message-storage)および[イベント記録用のTDengine Sinkを使ったルール作成](#create-a-rule-with-tdengine-sink-for-events-recording)を参照してください。

## メッセージ保存用のTDengine Sinkを使ったルール作成

このセクションでは、ダッシュボードでMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定したSink経由でTDengineのデータテーブル`t_mqtt_msg`に保存するルールの作成方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules**をクリックします。

2. 画面右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**でメッセージ保存用のルールを作成します。例えば、以下の文を入力するとトピック`t/#`配下のMQTTメッセージがTDengineに保存されます。

   注意：独自のSQL構文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`部に含めてください。

   ```sql
     SELECT
       *,
       now_timestamp('millisecond')  as ts
     FROM
       "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**と**Enable Test**をクリックしてSQLルールの学習とテストを行うことを推奨します。

   :::

4. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをTDengineに送信します。

5. **Type of Action**ドロップダウンから`TDengine`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのTDengine Sinkを選択することも可能ですが、この例では新規Sinkを作成します。

6. Sinkの名前を入力します。英数字の組み合わせで指定してください。

7. **Connector**ドロップダウンから先ほど作成した`my_tdengine`を選択します。隣のボタンで新規コネクターを作成することも可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

8. Sinkの**SQL Template**を設定します。以下のSQLを用いてデータ挿入を完了できます。CSVファイルによるバッチ設定もサポートしています。詳細は[バッチ設定](#batch-setting)を参照してください。

   ::: tip

   EMQX 5.1.1で破壊的変更があります。それ以前のバージョンでは文字列型の値は自動的に引用符で囲まれていましたが、5.1.1以降はユーザーが手動で引用符を付ける必要があります。

   :::

   ```sql
   INSERT INTO t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) 
       VALUES (${ts}, '${id}', '${topic}', ${qos}, '${payload}', ${timestamp})
   ```

   SQLテンプレート内でプレースホルダー変数が未定義の場合、**SQL template**上部の**Undefined Vars as Null**スイッチでルールエンジンの動作を切り替えられます。

   - **Disabled**（デフォルト）：ルールエンジンは未定義変数に文字列`undefined`を挿入します。
   - **Enabled**：未定義変数に対して`NULL`を挿入します。

     ::: tip

     可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

     :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**でSinkがTDengineに接続できるかテスト可能です。

12. **Create**ボタンをクリックしSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでTDengine Sink用のルール作成が完了しました。**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブで新しいTDengine Sinkも表示されます。

また、**Integration** -> **Flow Designer**でトポロジーを確認すると、トピック`t/#`配下のメッセージがルール`my_rule`で解析され、TDengineに送信・保存されていることがわかります。

### バッチ設定

TDengineでは1つのデータエントリに数百のデータポイントを含むことがあり、SQL文の作成が困難になる場合があります。これに対応するため、EMQXはSQLのバッチ設定機能を提供しています。

SQLテンプレート編集時に、バッチ設定機能を使ってCSVファイルから挿入操作用のフィールドをインポートできます。

1. **SQL Template**下の**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従いバッチ設定テンプレートファイルをダウンロードし、テンプレート内のフィールドのキーと値を記入します。テンプレートファイルのデフォルト内容は以下の通りです。

   | Field      | Value             | Char Value | 備考（任意）       |
   | ---------- | ----------------- | ---------- | ------------------ |
   | ts         | now               | FALSE      | 例示               |
   | msgid      | ${id}             | TRUE       |                    |
   | mqtt_topic | ${topic}          | TRUE       |                    |
   | qos        | ${qos}            | FALSE      |                    |
   | temp       | ${payload.temp}   | FALSE      |                    |
   | hum        | ${payload.hum}    | FALSE      |                    |
   | status     | ${payload.status} | FALSE      |                    |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数または`${var}`形式のプレースホルダーをサポート。SQLでは文字列型は引用符で囲む必要がありますが、テンプレートファイルでは不要。文字列型かどうかは`Char Value`列で指定します。
   - **Char Value**：フィールドが文字列型かどうかを指定し、インポート時にSQL生成で引用符を付加。文字列型なら`TRUE`または`1`、そうでなければ`FALSE`または`0`を記入。
   - **備考**：CSVファイル内のメモ用で、EMQXへのインポートには含まれません。

   CSVファイルの行数は2048行を超えないようにしてください。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックしバッチ設定を完了します。

4. インポート後、**SQL Template**内のSQLをさらに調整可能です。テーブル名の設定やSQLコードの整形などが行えます。

## イベント記録用のTDengine Sinkを使ったルール作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定したSink経由でTDengineテーブル`emqx_client_events`に保存するルール作成方法を示します。

ルール作成手順は[メッセージ保存用のTDengine Sinkを使ったルール作成](#メッセージ保存用のtdengine-sinkを使ったルール作成)とほぼ同様ですが、SQLルール構文とSQLテンプレートが異なります。

オンライン／オフライン状態記録用のSQLルール構文は以下の通りです。

```sql
SELECT
      *,
      now_timestamp('millisecond')  as ts
    FROM 
      "$events/client_connected", "$events/client_disconnected"
```

SinkのSQLテンプレートは以下の通りです。

注意：フィールドは引用符を含めず、SQL文の末尾にセミコロン（`;`）を付けないでください。

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

2つのSinkの稼働状況を確認すると、1件の新規受信メッセージと1件の新規送信メッセージ、2件のイベント記録があるはずです。

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
