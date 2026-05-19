# MQTTデータをDatalayersに取り込む

Datalayersは、産業用IoT、IoV、エネルギーなどの分野向けに設計されたマルチモーダルかつハイパーコンバージドなデータベースです。高いデータスループットと安定したパフォーマンスにより、IoTアプリケーションに最適です。EMQXは現在、Sinkを介してDatalayersにメッセージやデータを保存することをサポートしており、データ分析や可視化を容易にしています。

本ページでは、EMQXとDatalayersのデータ統合について詳細に解説し、ルールとSinkの作成方法を実践的に案内します。

## 動作概要

Datalayersデータ統合はEMQXの標準機能であり、デバイスからのMQTTメッセージをシームレスにDatalayersに転送して保存・分析できます。ルールとSinkを設定することで、処理済みのMQTTデータを柔軟にDatalayersへルーティング可能です。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとDatalayersの典型的な統合アーキテクチャを示しています。

![MQTT to Datalayers](./assets/mqtt-to-datalayers.jpg)

このアーキテクチャでは、EMQXがデバイスの接続管理、メッセージ転送、ルールベースの処理を担い、Datalayersがデータの保存、分析、可視化を担当します。両者が連携することで、エネルギー消費のリアルタイムデータを効率的に収集・分析するスケーラブルなIoTプラットフォームを構築できます。

EMQX 6.0.0以降、DatalayersはApache Arrowベースの高性能バイナリ通信プロトコルであるArrow Flight SQLをサポートしています。従来のInfluxDB Line Protocolと比較して、Arrow Flight SQLはより効率的なデータ転送と構造化データ書き込みの強化を実現します。

::: warning 注意

Arrow FlightドライバーはRustで実装され、Erlang VMにNative Implemented Function（NIF）を介して統合されています。本機能は現在実験的であり、テスト環境での利用を推奨します。

:::

具体的なワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**  
   デバイスはMQTTでEMQXに接続し、電力、電流、電圧などのエネルギー関連メトリクスを定期的にパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジンに渡します。

2. **ルールエンジンによるメッセージ処理**  
   EMQXの組み込みルールエンジンはトピックパターンに基づきメッセージをマッチングし、ペイロード変換、フィールドフィルタリング、コンテキスト情報の付加などの処理を行います。

3. **Datalayersへの書き込み**  
   ルールがトリガーされると、処理済みデータをDatalayersに書き込むSinkアクションが実行されます。SinkはSQLテンプレートをカスタマイズ可能で、Datalayersのテーブルやカラムへのフィールドマッピングを定義できます。

   EMQXは以下の2つの書き込み方式をサポートしています。

   - InfluxDB Line Protocol
   - Arrow Flight SQLドライバー

   Sinkの設定は選択した方式によって異なります。

エネルギー貯蔵データがDatalayersに書き込まれた後は、以下のようなツールを活用して柔軟に分析できます。

- Grafanaなどの可視化ツールに接続し、エネルギー貯蔵データのチャート表示を行う。
- 業務システムに接続し、エネルギー貯蔵装置の状態監視やアラート発報を行う。

## 特長とメリット

Datalayersデータ統合は以下の特長と利点を提供します。

- **効率的なデータ処理**  
  EMQXは多数のIoTデバイス接続とメッセージスループットを処理可能であり、Datalayersはデータ書き込み、保存、クエリに優れているため、IoTシナリオのデータ処理要件をシステム負荷を抑えて満たせます。

- **メッセージ変換**  
  EMQXルール内でメッセージの高度な処理・変換が可能であり、Datalayersへの書き込み前に柔軟にデータを整形できます。

- **スケーラビリティ**  
  EMQXとDatalayersはクラスター機能を備えており、ビジネスの成長に応じて水平スケールが可能です。

- **豊富なクエリ機能**  
  Datalayersはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから価値ある洞察を抽出できます。

- **効率的なストレージ**  
  Datalayersは高圧縮エンコーディング方式を採用し、ストレージコストを大幅に削減します。また、データ保持期間をカスタマイズ可能で、不要なデータがストレージを占有するのを防げます。

## はじめに

このセクションでは、EMQXでDatalayers Sinkを作成する前に必要な準備として、Datalayersのインストール、データベース作成、テーブル構造の定義について説明します。

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解
- 利用予定のドライバーに応じて、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)または[Arrow Flight SQL](https://arrow.apache.org/docs/format/FlightSql.html#arrow-flight-sql)の理解

### Datalayersのインストールとセットアップ

1. Dockerを使ってDatalayersをインストール・起動します。詳細手順は[Install Datalayers](https://docs.datalayers.cn/datalayers/latest/getting-started/docker.html)を参照してください。

   ```bash
   # Datalayersコンテナ起動
   docker run -d --name datalayers -p 8360:8360 -p 8361:8361 datalayers/datalayers:latest
   ```

   - ポート`8360`はArrow Flight SQLのgRPCデフォルトポートです。
   - ポート`8361`はHTTPポートで、主にLine Protocol書き込みや管理APIに使用されます。

2. Datalayersサービス起動後、デフォルトのユーザー名・パスワード`admin`/`public`でCLIにログインし、データベースを作成します。

   - Datalayersコンテナにアクセス：

     ```bash
     docker exec -it datalayers bash
     ```

   - Datalayers CLIに入る：

     ```bash
     dlsql -u admin -p public
     ```

   - データベース作成（例：`mqtt`）：

     ```sql
     create database mqtt
     ```

4. Arrow Flight SQLドライバーを使用する場合は、対象テーブルを事前に作成する必要があります。

   ::: tip 注意

   InfluxDB Line Protocolを使用する場合は、テーブルの事前作成は不要です。Datalayersは受信したLine Protocolの`measurement`やフィールド定義に基づき自動でテーブルを作成します。

   :::

   例として、`t_mqtt_msg`というテーブルを以下のSQLで作成します。

   ```sql
   CREATE TABLE IF NOT EXISTS `t_mqtt_msg` (
       time TIMESTAMP(3) NOT NULL,
       msgid STRING NOT NULL,
       sender STRING NOT NULL,
       topic STRING NOT NULL,
       qos INT8 NOT NULL,
       payload STRING,
       arrived TIMESTAMP(3) NOT NULL,
       timestamp key(time)
   ) PARTITION BY HASH (msgid, sender) PARTITIONS 1
   ENGINE = TimeSeries WITH (ttl = '14d');
   ```

## Datalayersコネクターの作成

このセクションでは、EMQXでDatalayersサーバーに接続するためのコネクター作成手順を説明します。

以下の手順はEMQXとDatalayersがローカルで稼働していることを前提としています。別環境やリモート環境にデプロイしている場合は接続設定を適宜更新してください。

1. EMQXダッシュボードで、**Integration** -> **Connectors**を開きます。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**ページで**Datalayers**を選択し、**Next**をクリックします。

5. **Configuration**ページでコネクターの詳細を入力します。

   - **Connector Name**：英数字で始まり、英数字、ハイフン、アンダースコアのみ使用可能。例：`my_datalayers`
   - **Description**（任意）：コネクター識別用の説明を入力

   Datalayersサーバー接続設定：

   - **Driver Type**：

     - `InfluxDB Line Protocol`：InfluxDB互換のLine Protocolでデータ取り込み。テーブルは自動作成されます。

     - `Arrow Flight`：SQLテンプレートを用いた高性能な構造化データ書き込み。スキーマ管理が厳密で書き込みスループットが高い場合に適しています。

       ::: warning 注意

       Arrow FlightドライバーはRustで実装され、Erlang VMにNIFで統合されています。現在実験的機能であり、テスト環境での評価を推奨します。

       :::

   - **Server Host**：

     - デフォルト：`127.0.0.1:8361`
     - `Arrow Flight`ドライバー使用時はgRPC通信でポート`8360`を使用します。

   - **Database Name**：Datalayersの対象データベース名（例：`mqtt`）

   - **Username / Password**：Datalayersアクセス用の認証情報（例：`admin` / `public`）

   - **Enable TLS**（任意）：暗号化接続を有効化。証明書パスや検証設定を行えます。詳細は[外部リソースアクセスのTLS設定](../network/overview.md#tls-for-external-resource-access)を参照してください。

     ::: tip 注意

     Arrow Flight SQLプロトコル使用時は証明書検証をスキップできません（`verify_none`非対応）。gRPCサーバー証明書のCNがサーバーホストと一致している必要があります。

     :::

5. `Arrow Flight`ドライバー選択時は、**Enable Prepared Statements**オプションが表示されます。これはSQLテンプレートによるデータ挿入をSinkが利用できるかを制御し、デフォルトで有効です。

6. **Create**をクリックする前に、**Test Connectivity**でDatalayersサーバーへの接続確認が可能です。

7. 画面下部の**Create**をクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**か**Create Rule**を選択できます。ルールとSinkの作成手順は[Create a Datalayers Rule](#create-a-datalayers-rule)を参照してください。

## Datalayersルールの作成

このセクションでは、EMQXでソーストピック`t/#`からのMQTTメッセージを処理し、設定済みのSinkを通じてDatalayersに送信するルール作成方法を示します。

### SQLを定義したルールの作成

1. EMQXダッシュボードの左メニューから**Data Integration** -> **Rules**を開きます。

2. **Rules**ページ右上の**Create**ボタンをクリックします。

3. ルール作成フォームでルールIDを入力します（例：`my_rule`）。

4. **SQL Editor**にルールロジックを定義します。トピック`t/#`にパブリッシュされたMQTTメッセージをDatalayersに保存するには、以下のSQLを使用します。

   ::: tip 注意

   カスタムSQLルールを書く場合、Sinkテンプレートで参照する変数（例：`${clientid}`, `${payload.temp}`）は必ずルールの`SELECT`句に含めてください。

   :::

   ```
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   EMQXのSQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックしてサンプルクエリを確認・テストできます。

   :::

5. ルールにDatalayers Sinkを追加し、処理結果をDatalayersに書き込みます。

   - **InfluxDB Line Protocol**を使用する場合は、[Add an InfluxDB Line Protocol Sink](#add-an-influxdb-line-protocol-sink)を参照してください。
   - **Arrow Flight SQLドライバー**を使用する場合は、[Add an Arrow Flight SQL Sink](#add-an-arrow-flight-sql-sink)を参照してください。

6. **Create Rule**ページで設定を確認し、**Save**をクリックしてルールを作成します。

作成したルールは**Rules**一覧に表示されます。ルール詳細の**Actions (Sink)**タブで関連付けられたDatalayers Sinkを確認できます。

また、**Integrations** -> **Flow Designer**でトポロジーグラフを表示すると、トピック`t/#`のメッセージが`my_rule`ルールで処理されDatalayersに書き込まれる様子が可視化されます。

### InfluxDB Line Protocol Sinkの追加

このセクションでは、InfluxDB Line Protocolを用いて処理済みデータをDatalayersに書き込むSinkの追加方法を説明します。

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルール条件に合致した際に実行されるアクションを定義します。このアクションで処理済みメッセージをDatalayersに転送します。

2. **Type of Action**ドロップダウンから`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のDatalayers Sinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sinkの名前を入力します（例：`dl_sink_influx`）。英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンから、`InfluxDB Line Protocol`ドライバーで設定済みのコネクターを選択します。未作成の場合は隣のボタンから新規作成可能です。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照してください。

5. **Time Precision**はデフォルトでミリ秒に設定します。

6. Datalayersへのデータ解析・書き込み用の**Data Format**と内容を定義します。`JSON`か`Line Protocol`を選択可能です。

   - **JSON**：

     **Measurement**、**Fields**、**Timestamp**、**Tags**を指定します。キー・値は定数または`${payload.temp}`のような変数プレースホルダーをサポートします。書式ルールは[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     **Fields**はCSVファイルによる一括設定も可能です。[Use CSV to Batch Configure Fields](#use-csv-to-batch-configure-fields)を参照してください。

   - **Line Protocol**：

     テーブル、フィールド、タイムスタンプ、タグを含む単一のLine Protocol文字列を定義できます。キー・値は定数またはプレースホルダー変数をサポートします。書式は[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     ::: tip

     Datalayersに書き込むデータはInfluxDB v1のLine Protocolと完全互換のため、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)を参考に設定できます。

     例えば、符号付き整数値を入力するには、プレースホルダーの後に`i`を付けます（例：`${payload.int}i`）。詳細は[InfluxDB 1.8で整数値を書き込む方法](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。

     :::

     Line Protocolの例：

     ```sql
     devices,clientid=${clientid} temp=${payload.temp},hum=${payload.hum},precip=${payload.precip}i ${timestamp}
     ```

7. **Fallback Actions**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

8. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照してください。

9. **Create**をクリックする前に**Test Connectivity**でDatalayersサーバーへの接続確認が可能です。

10. **Create**をクリックしてSink作成を完了します。**Create Rule**ページの**Action Outputs**タブに新規Sinkが表示されます。

#### CSVを使ったフィールド一括設定

::: tip

この機能は**InfluxDB Line Protocol**のSinkで、データフォーマットが`JSON`の場合のみ利用可能です。フィールド設定を一括でインポートできます。

:::

Datalayersのデータエントリは数百のフィールドを含むことが多く、設定が煩雑になるため、EMQXはバッチ設定機能を提供しています。

JSONフォーマット設定時に、CSVファイルからフィールドのキー・値ペアを一括インポート可能です。

1. **Fields**テーブルの**Batch Settings**ボタンをクリックし、**Import Batch Settings**ポップアップを開きます。

2. 指示に従いテンプレートファイルをダウンロードし、フィールドのキー・値ペアを入力します。テンプレートのデフォルト内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | 値の後に`i`を付けるとDatalayersは整数型として保存します。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダー。Line Protocolに従い型識別子を付加可能。
   - **備考**：CSV内のコメント用でEMQXにはインポートされません。

   バッチ設定CSVファイルは最大2048行までです。

3. 入力済みテンプレートファイルを保存し、**Import Batch Settings**ポップアップにアップロードして**Import**をクリックし、一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・値ペアをさらに調整可能です。

### Arrow Flight SQL Sinkの追加

このセクションでは、**Arrow Flight SQL**ドライバーを用いてSQL挿入文でDatalayersにデータを書き込むSinkの追加方法を説明します。

::: warning 注意

Arrow Flight SQLドライバーは現在実験的です。商用環境での利用は慎重に行ってください。

:::

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルールマッチ時に実行されるアクションを定義します。このアクションで処理済みデータをDatalayersに転送します。

2. **Type of Action**ドロップダウンから`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存Sinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sink名を設定します（例：`dl_sink_arrow`）。英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンから`Arrow Flight`ドライバーで設定済みのコネクターを選択します。未作成の場合は隣のボタンから新規作成可能です。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照してください。

5. データ挿入先テーブルへの書き込み方法を定義する**SQL**テンプレートを設定します。

   ::: tip

   これは[プリプロセスSQL](./data-bridges.md#prepared-statement)テンプレートです。フィールド名をクォートで囲まず、SQL文の末尾にセミコロン`;`を含めないでください。  
   `${}`プレースホルダーはルールSQLで選択したフィールドと一致させる必要があります。

   :::

   ::: tip

   コネクター設定のデータベース以外に書き込みたい場合は、SQLテンプレート内で明示的に対象データベース名を指定してください。  
   ただし、コネクターは対象データベースの存在確認を行います。

   :::

   例：

   ```sql
   insert into t_mqtt_msg(time, msgid, sender, topic, qos, payload, arrived) values (${timestamp}, ${id}, ${clientid}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```

6. **Fallback Actions**（任意）：信頼性向上のため、Sink処理失敗時に実行されるフォールバックアクションを設定可能です。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

7. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリック前に**Test Connection**でDatalayersサーバーへの接続確認が可能です。

9. **Create**をクリックしてSink作成を完了します。**Create Rule**ページの**Action Outputs**タブに新規Sinkが表示されます。

## ルールとSinkのテスト

ルールとSinkを設定後、テスト用MQTTメッセージをパブリッシュしてDatalayersへのデータ書き込みを検証できます。

1. [MQTTX](https://mqttx.app/)を使い、トピック`t/1`にメッセージを送信します。セッションイベント（クライアントのオンライン/オフライン）もトリガーされる可能性があります。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "temp": "23.5", "hum": "62", "precip": 2 }'
   ```

   このメッセージによりルールエンジンが起動し、設定済みのDatalayers Sinkに転送されます。ルールにクライアント接続・切断などのセッションイベントが含まれている場合も同様にトリガーされます。

2. Sinkの実行統計を確認します。EMQXダッシュボードの**Rules**ページで対象ルールを探し、**Actions (Sink)**タブに切り替えます。対象Sinkの**Matched**および**Success**カウントが1増加していることを確認してください。

3. CLIでDatalayers内のデータを検証します。

   Datalayersコンテナにアクセスし、CLIツールを起動します。

   ```bash
   docker exec -it datalayers bash
   dlsql -u admin -p public
   ```

   使用した書き込み方式に応じてSQLを実行します。

   - InfluxDB Line Protocolの場合、Sink設定の`measurement`（例：`devices`）がテーブル名のデフォルトです。

     ```sql
     use mqtt
     select * from devices
     ```

   - Arrow Flight SQLの場合、事前作成した対象テーブル（例：`t_mqtt_msg`）をクエリします。

     ```sql
     use mqtt
     select * from t_mqtt_msg
     ```

## 詳細設定

このセクションでは、DatalayersコネクターおよびSinkの詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際に、**Advanced Settings**を展開して以下のパラメータを用途に応じて調整できます。

| 項目名                 | 説明                                                                                                                         | デフォルト値 |
| ---------------------- | ---------------------------------------------------------------------------------------------------------------------------- | ------------ |
| Buffer Pool Size       | EMQXとDatalayers間のegressタイプSinkのデータフローを管理するバッファワーカープロセスの数を指定します。これらのプロセスは送信前にデータを一時的に保持・処理します。パフォーマンス最適化やスムーズなデータ送信に重要です。ingressのみのブリッジでは`0`に設定可能です。 | `4`          |
| Request TTL            | バッファに入ったリクエストの有効期限（秒）を指定します。リクエストがこのTTLを超えてバッファに滞留するか、Datalayersからの応答・アックを受け取れない場合、リクエストは期限切れとみなされます。 | `45`         |
| Health Check Interval  | SinkがDatalayersとの接続状態を自動的にチェックする間隔（秒）を指定します。                                                         | `15`         |
| Max Buffer Queue Size  | Datalayers Sinkの各バッファワーカープロセスが保持可能な最大バイト数を指定します。バッファワーカーはデータ送信前に一時的にデータを保持し、データストリームを効率的に処理します。システム性能やデータ送信要件に応じて調整してください。 | `1`          |
| Batch Size             | EMQXからDatalayersへ一度に転送するデータバッチの最大サイズを指定します。この値を調整することで転送効率やパフォーマンスを最適化できます。`1`に設定するとバッチ化せず個別送信となります。 | `100`        |
| Query Mode             | 同期（`synchronous`）または非同期（`asynchronous`）のリクエストモードを選択可能です。非同期モードではDatalayersへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがDatalayers到達前にメッセージを受信する可能性があります。 | `Asynch`     |
| Inflight Window        | 送信済みだが応答・アックをまだ受け取っていないリクエストの最大数を制御します。`Request Mode`が`asynchronous`の場合に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`        |
