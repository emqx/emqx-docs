# MQTTデータをDatalayersに取り込む

Datalayersは、産業用IoT、IoV、エネルギーなどの分野向けに設計されたマルチモーダルかつハイパーコンバージドなデータベースです。高いデータスループットと安定したパフォーマンスを備えており、IoTアプリケーションに最適です。EMQXは現在、Sinkを介してDatalayersにメッセージやデータを保存することをサポートしており、データ分析や可視化を容易にしています。

本ページでは、EMQXとDatalayersのデータ統合について詳しく解説し、ルールおよびSinkの作成方法を実践的に案内します。

## 動作概要

Datalayersとのデータ統合はEMQXの標準機能であり、デバイスからのMQTTメッセージをシームレスにDatalayersへ転送し、保存および分析を行えます。ルールとSinkを設定することで、処理済みのMQTTデータを柔軟にDatalayersへルーティング可能です。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとDatalayersの統合アーキテクチャの典型例を示しています。

![MQTT to Datalayers](./assets/mqtt-to-datalayers.jpg)

このアーキテクチャでは、EMQXがデバイスの接続管理、メッセージ伝送、ルールベースの処理を担当し、Datalayersがデータの保存、分析、可視化を担います。両者が連携することで、エネルギー消費のリアルタイムデータを効率的に収集・分析するスケーラブルなIoTプラットフォームを構築できます。

EMQX 6.0.0以降、DatalayersはApache Arrowをベースとした高性能バイナリ通信プロトコルであるArrow Flight SQLをサポートしています。従来のInfluxDB Line Protocolと比べて、より効率的なデータ転送と構造化データの書き込みに強みがあります。

::: warning 注意

Arrow FlightドライバーはRustで実装され、Erlang VMにNative Implemented Function（NIF）として統合されています。本機能は現在実験的であり、テスト環境での利用を推奨します。

:::

具体的なワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**  
   デバイスはMQTTでEMQXに接続し、電力、電流、電圧などのエネルギー関連メトリクスを定期的にパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジンに渡します。

2. **ルールエンジンによるメッセージ処理**  
   EMQXのルールエンジンはトピックパターンに基づいてメッセージをマッチングし、ペイロードの変換、フィールドのフィルタリング、コンテキスト情報の付加などの処理を行います。

3. **Datalayersへの書き込み**  
   ルールがトリガーされると、Sinkアクションを実行して処理済みデータをDatalayersに書き込みます。SinkはSQLテンプレートをカスタマイズ可能で、Datalayersのテーブルやカラムへのマッピングを定義できます。

   EMQXは以下の2つの書き込み方式をサポートしています。

   - InfluxDB Line Protocol
   - Arrow Flight SQLドライバー

   Sinkの設定は選択した方式によって異なります。

エネルギー貯蔵データがDatalayersに書き込まれた後は、対応するツールを用いて柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続し、エネルギー貯蔵データのチャートを作成・表示する。
- 業務システムに連携し、エネルギー貯蔵装置の状態監視やアラートを実施する。

## 特長と利点

Datalayersデータ統合は以下の特長とメリットを提供します。

- **効率的なデータ処理**  
  EMQXは多数のIoTデバイス接続とメッセージスループットを処理可能であり、Datalayersはデータの書き込み、保存、クエリに優れています。これにより、IoTシナリオのデータ処理ニーズをシステム負荷を抑えて満たせます。

- **メッセージ変換**  
  メッセージはEMQXのルール内で多様な処理・変換を経てからDatalayersに書き込まれます。

- **スケーラビリティ**  
  EMQXおよびDatalayersはクラスター機能を備え、ビジネスの成長に応じて柔軟な水平スケールが可能です。

- **豊富なクエリ機能**  
  Datalayersはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから有益な洞察を抽出できます。

- **効率的なストレージ**  
  Datalayersは高圧縮エンコーディングを用いてストレージコストを大幅に削減し、不要なデータがストレージを占有しないようカスタマイズ可能なデータ保持期間を設定できます。

## はじめる前に

本節では、EMQXでDatalayers Sinkを作成する前に必要な準備として、Datalayersのインストール、データベース作成、テーブル構造定義について説明します。

### 前提条件

- [ルール](./rules.md)の基本知識
- [データ統合](./data-bridges.md)の基本知識
- 書き込みに使用するドライバーに応じて、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)または[Arrow Flight SQL](https://arrow.apache.org/docs/format/FlightSql.html#arrow-flight-sql)の理解

### Datalayersのインストールとセットアップ

1. Dockerを使ってDatalayersをインストール・起動します。詳細は[Install Datalayers](https://docs.datalayers.cn/datalayers/latest/getting-started/docker.html)を参照してください。

   ```bash
   # Datalayersコンテナを起動
   docker run -d --name datalayers -p 8360:8360 -p 8361:8361 datalayers/datalayers:latest
   ```

   - ポート`8360`はArrow Flight SQL用のデフォルトgRPCポートです。
   - ポート`8361`はHTTPポートで、主にLine Protocol書き込みや管理APIに使用されます。

2. Datalayersサービス起動後、デフォルトのユーザー名・パスワード`admin`/`public`でCLIにログインし、データベースを作成します。

   - Datalayersコンテナにアクセス：

     ```bash
     docker exec -it datalayers bash
     ```

   - Datalayers CLIを起動：

     ```bash
     dlsql -u admin -p public
     ```

   - データベースを作成（例：`mqtt`）：

     ```sql
     create database mqtt
     ```

4. Arrow Flight SQLドライバーを使用する場合は、対象テーブルを事前に作成する必要があります。

   ::: tip 注意

   InfluxDB Line Protocolを使用する場合はテーブルの事前作成は不要です。Datalayersは受信したLine Protocolデータの`measurement`やフィールド定義に基づいて自動的にテーブルを作成します。

   :::

   例えば、以下のSQLで`t_mqtt_msg`テーブルを作成します。

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

本節では、EMQXでDatalayersサーバーに接続するコネクターを作成する方法を説明します。

以下の手順はEMQXとDatalayersがローカルで稼働していることを前提としています。別環境やリモートにデプロイしている場合は接続設定を適宜変更してください。

1. EMQXダッシュボードで、**Integration** -> **Connectors**をクリックします。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**ページで**Datalayers**を選択し、**Next**をクリックします。

5. **Configuration**ページでコネクターの詳細を入力します。

   - **Connector Name**：英数字で始まり、英数字、ハイフン、アンダースコアのみ使用可能。例：`my_datalayers`
   - **Description**（任意）：後で識別しやすい説明を入力

   Datalayersサーバー接続設定：

   - **Driver Type**：

     - `InfluxDB Line Protocol`：InfluxDB互換のLine Protocolでデータを取り込みます。テーブル作成は自動です。

     - `Arrow Flight`：SQLテンプレートを用いた高性能な構造化データ書き込みを可能にします。スキーマ管理や高い書き込みスループットが必要な場合に適しています。

       ::: warning 注意

       Arrow FlightドライバーはRustで実装され、Erlang VMにNIFとして統合されています。現在実験的機能であり、テスト環境での評価を推奨します。

       :::

   - **Server Host**：

     - デフォルト：`127.0.0.1:8361`
     - `Arrow Flight`ドライバー使用時はgRPC通信でポート`8360`を使用

   - **Database Name**：Datalayersの対象データベース名（例：`mqtt`）

   - **Username / Password**：Datalayersアクセス用の認証情報（例：`admin` / `public`）

   - **Enable TLS**（任意）：暗号化接続を有効化。証明書パスや検証設定を行えます。詳細は[外部リソースアクセスのTLS設定](../network/overview.md#tls-for-external-resource-access)を参照。

     ::: tip 注意

     Arrow Flight SQLプロトコル使用時は証明書検証をスキップできません（`verify_none`非対応）。gRPCサーバー証明書のCommon Name（CN）がサーバーホスト名と一致している必要があります。

     :::

5. ドライバーに`Arrow Flight`を選択した場合、**Enable Prepared Statements**オプションが表示されます。SinkがSQLテンプレートを利用してデータ挿入を行うかを指定し、デフォルトで有効です。

6. **Create**をクリックする前に、**Test Connectivity**でDatalayersサーバーへの接続確認が可能です。

7. 画面下部の**Create**をクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択できます。ルールとSinkの作成手順は[Create a Datalayers Rule](#create-a-datalayers-rule)を参照してください。

## Datalayersルールの作成

本節では、EMQXでトピック`t/#`からのMQTTメッセージを処理し、設定済みのSinkを使ってDatalayersに送信するルールを作成する方法を説明します。

### SQLを定義したルールの作成

1. EMQXダッシュボードの左メニューから**Data Integration** -> **Rules**に移動します。

2. **Rules**ページ右上の**Create**をクリックします。

3. ルール作成フォームでルールIDを入力します（例：`my_rule`）。

4. **SQL Editor**にルールロジックを定義します。トピック`t/#`にパブリッシュされたMQTTメッセージをDatalayersに保存するには、以下のSQLを使用します。

   ::: tip 注意

   カスタムSQLルールを書く場合、Sinkテンプレートで参照するすべての変数（例：`${clientid}`, `${payload.temp}`）がルールの`SELECT`句に含まれていることを確認してください。

   :::

   ```
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   EMQXのSQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックしてサンプルクエリを試し、出力を確認できます。

   :::

5. ルールにDatalayers Sinkを追加し、処理結果をDatalayersに書き込みます。

   - **InfluxDB Line Protocol**を使用する場合は、[Add an InfluxDB Line Protocol Sink](#add-an-influxdb-line-protocol-sink)を参照。
   - **Arrow Flight SQLドライバー**を使用する場合は、[Add an Arrow Flight SQL Sink](#add-an-arrow-flight-sql-sink)を参照。

6. **Create Rule**ページで設定を確認し、**Save**をクリックしてルールを作成します。

作成したルールは**Rules**一覧に表示されます。**Actions (Sink)**タブをクリックすると、このルールに紐づくDatalayers Sinkを確認できます。

また、**Integrations** -> **Flow Designer**でトポロジーグラフを表示すると、トピック`t/#`のメッセージが`my_rule`ルールで処理されDatalayersに書き込まれる様子が可視化されます。

### InfluxDB Line Protocol Sinkの追加

本節では、InfluxDB Line Protocolを使って処理済みデータをDatalayersに書き込むSinkをルールに追加する方法を説明します。

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルール条件に合致した際にトリガーされるアクションを定義します。このアクションで処理済みメッセージをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のDatalayers Sinkを選択することも可能ですが、本例では新規作成を想定します。

3. Sinkの名前を入力します（例：`dl_sink_influx`）。英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンで、`InfluxDB Line Protocol`ドライバーで設定済みのコネクターを選択します。コネクターがなければ隣のボタンから作成可能です。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照してください。

5. **Time Precision**はデフォルトでミリ秒に設定します。

6. Datalayersへのデータ解析・書き込み用に**Data Format**と内容を定義します。`JSON`か`Line Protocol`を選択可能です。

   - **JSON**：

     **Measurement**、**Fields**、**Timestamp**、**Tags**を指定します。キーと値は定数または`${payload.temp}`のような変数プレースホルダーが利用可能です。書式は[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     **Fields**はCSVファイルを使った一括設定もサポートします。[Use CSV to Batch Configure Fields](#use-csv-to-batch-configure-fields)を参照。

   - **Line Protocol**：

     テーブル、フィールド、タイムスタンプ、タグを含む単一のLine Protocol文字列を定義します。キーと値は定数またはプレースホルダーが利用可能です。書式は[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     ::: tip

     Datalayersに書き込むデータはInfluxDB v1のLine Protocolと完全互換です。設定時は[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)を参考にしてください。

     例えば、符号付き整数値を入力する際は`${payload.int}i`のように`i`を型識別子として付加します。[InfluxDB 1.8で整数値を記述する方法](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照。

     :::

     Line Protocolの例：

     ```sql
     devices,clientid=${clientid} temp=${payload.temp},hum=${payload.hum},precip=${payload.precip}i ${timestamp}
     ```

7. **Fallback Actions**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

8. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照。

9. **Create**をクリックする前に、**Test Connectivity**でSinkがDatalayersサーバーに接続可能かテストできます。

10. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新規Sinkが表示されます。

#### CSVを使ったフィールド一括設定

::: tip

この機能は、**InfluxDB Line Protocol**ドライバーかつデータフォーマットが`JSON`のSinkでのみ利用可能です。フィールド設定を一括インポートできます。

:::

Datalayersのデータ項目は数百フィールドに及ぶことが多く、手動設定は困難です。EMQXはCSVファイルを用いたバッチ設定機能を提供しています。

JSONフォーマット設定時に、CSVファイルからキー・バリューのペアを一括インポート可能です。

1. **Fields**テーブルの**Batch Settings**ボタンをクリックし、**Import Batch Settings**ポップアップを開きます。

2. 指示に従いテンプレートファイルをダウンロードし、フィールドのキー・バリューを入力します。テンプレートのデフォルト内容例：

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | 値の後に`i`を付加し、Datalayersで整数型として保存されます。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダー対応。
   - **Value**：フィールド値。定数またはプレースホルダー。Line Protocolに準じた型識別子の付加も可能。
   - **備考**：CSV内のコメント用で、EMQXへのインポート対象外。

   CSVファイルは2048行を超えないようにしてください。

3. 入力済みテンプレートファイルを保存し、**Import Batch Settings**ポップアップにアップロード後、**Import**をクリックして一括設定を完了します。

4. インポート後、**Fields**設定テーブルで個別に調整可能です。

### Arrow Flight SQL Sinkの追加

本節では、Arrow Flight SQLドライバーを用いてSQL挿入文でDatalayersにデータを書き込むSinkをルールに追加する方法を説明します。

::: warning 注意

Arrow Flight SQLドライバーは現在実験的機能です。商用環境での利用は慎重に行ってください。

:::

1. ルール編集画面右側の**Add Action**をクリックし、ルールマッチ時にトリガーされるアクションを定義します。このアクションで処理済みデータをDatalayersに転送します。

2. **Type of Action**で`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存Sinkを選択することも可能ですが、本例では新規作成を想定します。

3. Sinkの名前を入力します（例：`dl_sink_arrow`）。英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンで、`Arrow Flight`ドライバーで設定済みのコネクターを選択します。存在しない場合は隣のボタンから作成可能です。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照してください。

5. データ挿入方法を定義する**SQL**テンプレートを設定します。

   ::: tip

   これは[プリプロセスSQL](./data-bridges.md#prepared-statement)テンプレートです。フィールド名は引用符で囲まず、SQL文の末尾にセミコロン`;`を付けないでください。`${}`のプレースホルダーはルールSQLで選択したフィールドと一致させる必要があります。

   :::

   ::: tip

   コネクターで設定したデータベース以外にデータを挿入する場合は、SQLテンプレート内で対象データベース名を明示的に指定してください。なお、コネクターは対象データベースの存在をチェックします。

   :::

   例：

   ```sql
   insert into t_mqtt_msg(time, msgid, sender, topic, qos, payload, arrived) values (${timestamp}, ${id}, ${clientid}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```

6. **Fallback Actions**（任意）：信頼性向上のため、Sinkがメッセージ処理に失敗した際にトリガーされるフォールバックアクションを1つ以上設定可能です。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

7. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connection**でSinkがDatalayersサーバーに接続可能か確認できます。

9. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新規Sinkが表示されます。

## ルールとSinkの動作確認

ルールとSinkの設定後、テスト用MQTTメッセージをパブリッシュしてDatalayersへの書き込みが成功しているか確認できます。

1. [MQTTX](https://mqttx.app/)を使い、トピック`t/1`にメッセージを送信します。セッションイベント（クライアントのオンライン/オフラインなど）もトリガーされる場合があります。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "temp": "23.5", "hum": "62", "precip": 2 }'
   ```

   このメッセージはルールエンジンをトリガーし、設定済みのDatalayers Sinkに転送されます。ルールにクライアント接続・切断などのセッションイベントが含まれている場合、それらも同時にトリガーされます。

2. Sinkの実行統計を確認します。EMQXダッシュボードの**Rules**ページで対象ルールを探し、**Actions (Sink)**タブに切り替えます。対象Sinkの**Matched**および**Success**カウントが1増加していることを確認してください。

3. CLIからDatalayersのデータを確認します。

   Datalayersコンテナにアクセスし、CLIツールを起動します。

   ```bash
   docker exec -it datalayers bash
   dlsql -u admin -p public
   ```

   書き込み方式に応じてSQLを実行します。

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

本節では、DatalayersコネクターおよびSinkの詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際に、**Advanced Settings**を展開して以下のパラメーターをニーズに応じて調整できます。

| フィールド名               | 説明                                                                                                                             | デフォルト値 |
| -------------------------- | -------------------------------------------------------------------------------------------------------------------------------- | ------------ |
| Buffer Pool Size           | バッファワーカープロセスの数を指定します。これらのプロセスはEMQXとDatalayers Sink間のデータフローを管理し、送信前にデータを一時的に保持・処理します。特にegressタイプのSinkでパフォーマンス最適化とスムーズなデータ送信に重要です。ingressのみのブリッジでは`0`に設定可能です。 | `4`          |
| Request TTL                | リクエストの有効期限（秒）を指定します。リクエストがバッファに入った時点でタイマーが開始され、TTLを超えてバッファに滞留するか、Datalayersからの応答・アックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45`         |
| Health Check Interval      | SinkがDatalayersとの接続状態を自動的に監視する間隔（秒）を指定します。                                                                | `15`         |
| Max Buffer Queue Size      | Datalayers Sinkの各バッファワーカープロセスが一時的に保持可能な最大バイト数を指定します。バッファワーカーはデータ送信前の中継役として機能し、システム性能やデータ送信要件に応じて調整可能です。                     | `1`          |
| Batch Size                 | EMQXからDatalayersへ一度に転送するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送の効率とパフォーマンスを最適化できます。`1`に設定すると単一レコードずつ送信され、バッチ化されません。         | `100`        |
| Query Mode                 | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信の最適化を図れます。非同期モードではDatalayersへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがDatalayers到達前にメッセージを受信する可能性があります。 | `Asynch`     |
| Inflight Window            | 送信済みだが応答・アック未受領のリクエスト数の最大値を制御します。`Request Mode`が`asynchronous`の場合に重要で、同一MQTTクライアントからのメッセージを厳密に順序処理する必要がある場合は`1`に設定してください。 | `100`        |
