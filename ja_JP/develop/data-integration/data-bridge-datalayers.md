# MQTTデータをDatalayersに取り込む

Datalayersは、産業用IoT、IoV、エネルギーなどの分野向けに設計されたマルチモーダルかつハイパーコンバージドなデータベースです。高いデータスループットと安定したパフォーマンスを備えており、IoTアプリケーションに最適です。EMQXは現在、Sinkを介してDatalayersへのメッセージおよびデータの保存をサポートしており、データ分析や可視化を容易にしています。

本ページでは、EMQXとDatalayersのデータ統合の詳細な概要を提供し、ルールとSinkの作成方法について実践的なガイドを示します。

## 動作概要

Datalayersデータ統合はEMQXの標準機能であり、デバイスからのMQTTメッセージをシームレスにDatalayersへ転送して保存・分析できます。ルールとSinkを設定することで、処理済みのMQTTデータを柔軟にDatalayersへルーティング可能です。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとDatalayersの統合アーキテクチャの典型例を示しています：

![MQTT to Datalayers](./assets/mqtt-to-datalayers.jpg)

このアーキテクチャでは、EMQXがデバイスの接続管理、メッセージ転送、ルールベースの処理を担当し、Datalayersがデータの保存、分析、可視化を担います。両者が連携することで、エネルギー消費のリアルタイムデータを効率的に収集・分析するスケーラブルなIoTプラットフォームを構築します。

EMQX 6.0.0以降、DatalayersはApache Arrowに基づく高性能バイナリ通信プロトコルであるArrow Flight SQLをサポートしています。従来のInfluxDB Line Protocolと比較して、Arrow Flight SQLはより効率的なデータ転送と構造化データ書き込みの強力なサポートを提供します。

::: warning 注意

Arrow FlightドライバーはRustで実装され、Erlang VMにNative Implemented Function（NIF）として統合されています。本機能は現在実験的であり、テスト環境での利用を推奨します。

:::

具体的なワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：デバイスはMQTTを介してEMQXに接続し、電力、電流、電圧などのエネルギー関連メトリクスを定期的にパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジンに渡します。

2. **ルールエンジンによるメッセージ処理**：EMQXの組み込みルールエンジンはトピックパターンに基づきメッセージをマッチングし、ペイロードの変換、フィールドのフィルタリング、コンテキスト情報の付加などの処理を行います。

3. **Datalayersへの書き込み**：ルールがトリガーされると、処理済みデータをDatalayersに書き込むSinkアクションが実行されます。SinkはフィールドをDatalayersのテーブルやカラムにマッピングするカスタマイズ可能なSQLテンプレートをサポートします。

   EMQXは以下の2つの書き込み方式をサポートしています：

   - InfluxDB Line Protocol
   - Arrow Flight SQLドライバー

   Sinkの設定は選択した方式により異なります。

エネルギー貯蔵データがDatalayersに書き込まれた後は、対応ツールを用いて柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続し、チャートを生成してエネルギー貯蔵データを表示。
- 業務システムに接続し、エネルギー貯蔵デバイスの状態監視やアラート発報。

## 特長と利点

Datalayersデータ統合は以下の特長と利点を提供します：

- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを処理可能であり、Datalayersはデータ書き込み、保存、クエリに優れているため、IoTシナリオのデータ処理要件をシステムに負荷をかけずに満たせます。
- **メッセージ変換**：メッセージはEMQXのルール内で多様な処理・変換が可能であり、Datalayersへの書き込み前に柔軟に加工できます。
- **スケーラビリティ**：EMQXとDatalayersは共にクラスタリング機能を備え、ビジネスの成長に応じて水平スケールが可能です。
- **豊富なクエリ機能**：Datalayersはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから価値ある洞察を抽出します。
- **効率的なストレージ**：Datalayersは高圧縮エンコード方式を採用し、ストレージコストを大幅に削減します。また、カスタマイズ可能なデータ保持期間により不要なデータのストレージ占有を防止します。

## はじめる前に

本節では、EMQXでDatalayers Sinkを作成する前に必要な準備として、Datalayersのインストール、データベース作成、テーブル構造定義について説明します。

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解
- 使用するドライバーに応じて、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)または[Arrow Flight SQL](https://arrow.apache.org/docs/format/FlightSql.html#arrow-flight-sql)の理解

### Datalayersのインストールとセットアップ

1. Dockerを使用してDatalayersをインストールし起動します。詳細は[Install Datalayers](https://docs.datalayers.cn/datalayers/latest/getting-started/docker.html)を参照してください。

   ```bash
   # Datalayersコンテナを起動
   docker run -d --name datalayers -p 8360:8360 -p 8361:8361 datalayers/datalayers:latest
   ```

   - ポート`8360`はArrow Flight SQL用のデフォルトgRPCポートです。
   - ポート`8361`はHTTPポートで、主にLine Protocol書き込みや管理APIに使用されます。

2. Datalayersサービス起動後、デフォルトのユーザー名・パスワード`admin`/`public`でDatalayers CLIにログインし、データベースを作成します。

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

   InfluxDB Line Protocolを使用する場合はテーブルの事前作成は不要です。Datalayersは受信したLine Protocolデータの`measurement`およびフィールド定義に基づき自動でテーブルを作成します。

   :::

   例えば、以下のSQLで`t_mqtt_msg`テーブルを作成します：

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

本節では、EMQXでSinkをDatalayersサーバーに接続するためのコネクター作成方法を示します。

以下の手順はEMQXとDatalayersがローカルで稼働している前提です。別環境やリモート環境にデプロイしている場合は接続設定を適宜変更してください。

1. EMQXダッシュボードで、**Integration** -> **Connectors**をクリック。

2. ページ右上の**Create**をクリック。

3. **Create Connector**ページで**Datalayers**を選択し、**Next**をクリック。

5. **Configuration**ページでコネクターの詳細を入力：

   - **Connector Name**：英数字で始まり、英数字、ハイフン、アンダースコアのみ使用可能。例：`my_datalayers`
   - **Description**（任意）：後で識別しやすい説明を追加可能。

   Datalayersサーバー接続設定：

   - **Driver Type**：

     - `InfluxDB Line Protocol`：InfluxDB互換のLine Protocolでデータ取り込み。テーブル作成は自動。

     - `Arrow Flight`：SQLテンプレートを用いた高性能な構造化データ書き込み。スキーマ管理や高スループットが必要な場合に適する。

       ::: warning 注意

       Arrow FlightドライバーはRust実装でErlang VMにNIFとして統合されています。現在実験的機能であり、テスト環境での評価を推奨します。

       :::

   - **Server Host**：

     - デフォルト：`127.0.0.1:8361`
     - `Arrow Flight`ドライバー使用時はgRPC通信のためポート`8360`を使用。

   - **Database Name**：Datalayersの対象データベース名（例：`mqtt`）。

   - **Username / Password**：Datalayersアクセス用認証情報（例：`admin` / `public`）。

   - **Enable TLS**（任意）：暗号化接続を有効化。証明書パスや検証オプションの設定が可能。詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#tls-for-external-resource-access)を参照。

     ::: tip 注意

     Arrow Flight SQLプロトコル使用時は証明書検証をスキップできません（`verify_none`非対応）。gRPCサーバー証明書のCommon Name（CN）がサーバーホストと一致している必要があります。

     :::

5. `Arrow Flight`ドライバー選択時は**Enable Prepared Statements**オプションが表示されます。SinkがSQLテンプレートを用いてデータ挿入を行うかどうかを設定し、デフォルトで有効です。

6. **Create**をクリックする前に、**Test Connectivity**でDatalayersサーバーへの接続テストが可能です。

7. ページ下部の**Create**をクリックしてコネクター作成を完了。ポップアップで**Back to Connector List**または**Create Rule**を選択可能。ルールとSinkの作成手順は[Create a Datalayers Rule](#create-a-datalayers-rule)を参照してください。

## Datalayersルールの作成

本節では、EMQXでソーストピック`t/#`からのMQTTメッセージを処理し、処理結果を設定済みのDatalayers Sinkに送信するルール作成方法を示します。

### SQLを定義したルールの作成

1. EMQXダッシュボードの左メニューから**Data Integration** -> **Rules**に移動。

2. **Rules**ページ右上の**Create**をクリック。

3. ルール作成フォームでルールIDを入力（例：`my_rule`）。

4. **SQL Editor**にルールロジックを定義。トピック`t/#`にパブリッシュされたMQTTメッセージをDatalayersに保存するには以下のSQLを使用可能です：

   ::: tip 注意

   カスタムSQLルールを書く際は、Sinkテンプレートで参照するすべての変数（例：`${clientid}`, `${payload.temp}`）がルールの`SELECT`句に含まれていることを確認してください。

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

5. 処理結果をDatalayersに書き込むため、ルールにDatalayers Sinkを追加します。

   - **InfluxDB Line Protocol**を使用する場合は：[Add an InfluxDB Line Protocol Sink](#add-an-influxdb-line-protocol-sink)を参照。
   - **Arrow Flight SQLドライバー**を使用する場合は：[Add an Arrow Flight SQL Sink](#add-an-arrow-flight-sql-sink)を参照。

6. **Create Rule**ページで設定内容を確認し、**Save**をクリックしてルールを作成。

作成したルールは**Rules**一覧に表示されます。該当ルールの**Actions (Sink)**タブをクリックすると、このルールに紐づくDatalayers Sinkを確認可能です。

また、**Integrations** -> **Flow Designer**でトポロジーグラフを表示すると、トピック`t/#`のメッセージが`my_rule`ルールで処理され、Datalayersに書き込まれる様子が可視化されます。

### InfluxDB Line Protocol Sinkの追加

本節では、InfluxDB Line Protocolを用いて処理済みデータをDatalayersに書き込むSinkをルールに追加する方法を示します。

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルール条件に合致した際にトリガーされるアクションを定義。処理済みメッセージをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択。**Action**はデフォルトの`Create Action`のままにします。既存のDatalayers Sinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sink名を入力（例：`dl_sink_influx`）。英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンで、`InfluxDB Line Protocol`ドライバーで設定済みのコネクターを選択。未作成の場合は隣のボタンから作成可能です。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照。

5. **Time Precision**はデフォルトでミリ秒に設定。

6. Datalayersへのデータ解析・書き込み用の**Data Format**と内容を定義。`JSON`または`Line Protocol`から選択可能：

   - **JSON**：

     **Measurement**、**Fields**、**Timestamp**、**Tags**を指定。キー・値は定数または変数プレースホルダー（例：`${payload.temp}`）をサポート。書式ルールは[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照。

     **Fields**はCSVファイルを用いた一括設定も可能です。[Use CSV to Batch Configure Fields](#use-csv-to-batch-configure-fields)を参照。

   - **Line Protocol**：

     テーブル、フィールド、タイムスタンプ、タグを含む単一のLine Protocol文字列を定義可能。キー・値は定数またはプレースホルダーをサポート。書式は[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照。

     ::: tip

     Datalayersに書き込むデータはInfluxDB v1のLine Protocolと完全互換のため、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)を参照して設定可能です。

     例えば、符号付き整数値を入力する場合はプレースホルダーの後に`i`を付加します（例：`${payload.int}i`）。詳細は[InfluxDB 1.8で整数値を書き込む方法](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照。

     :::

     Line Protocolの例：

     ```sql
     devices,clientid=${clientid} temp=${payload.temp},hum=${payload.hum},precip=${payload.precip}i ${timestamp}
     ```

7. **Fallback Actions**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照。

8. **Advanced Settings**を展開し、必要に応じて詳細設定を行います（任意）。詳細は[Advanced Settings](#advanced-settings)を参照。

9. **Create**をクリックする前に、**Test Connectivity**でSinkがDatalayersサーバーに接続可能かテスト可能。

10. **Create**をクリックしてSink作成を完了。**Create Rule**ページに戻ると、**Action Outputs**タブに新規Sinkが表示されます。

#### CSVを使ったフィールド一括設定

::: tip

この機能は**InfluxDB Line Protocol**のSinkで、データフォーマットが`JSON`の場合のみ利用可能です。フィールド設定を一括でインポートできます。

:::

Datalayersのデータエントリは数百のフィールドを含むことが多く、データフォーマット設定が煩雑になるため、EMQXは一括フィールド設定機能を提供しています。

JSONフォーマット設定時に、CSVファイルからフィールドのキー・値ペアを一括インポート可能です。

1. **Fields**テーブルの**Batch Settings**ボタンをクリックし、**Import Batch Settings**ポップアップを開く。

2. 指示に従いテンプレートファイルをダウンロードし、フィールドのキー・値ペアを入力。テンプレートのデフォルト内容例：

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | フィールド値の後に`i`を付けるとDatalayersは整数型として保存 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポート。Line Protocolに従い型識別子を付加可能。
   - **備考**：CSV内のコメント用。EMQXへのインポート対象外。

   CSVファイルは最大2048行まで。

3. 入力済みテンプレートを保存し、**Import Batch Settings**ポップアップでアップロード後、**Import**をクリックして一括設定を完了。

4. インポート後、**Fields**設定テーブルでキー・値ペアをさらに調整可能。

### Arrow Flight SQL Sinkの追加

本節では、**Arrow Flight SQL**ドライバーを用いてSQL挿入文でDatalayersにデータを書き込むSinkをルールに追加する方法を示します。

::: warning 注意

Arrow Flight SQLドライバーは現在実験的機能です。運用環境での利用は慎重に行ってください。

:::

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルールマッチ時にトリガーされるアクションを定義。処理済みデータをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択。**Action**はデフォルトの`Create Action`のままにします。既存Sinkの選択も可能ですが、本例では新規作成を想定。

3. Sink名を入力（例：`dl_sink_arrow`）。英数字の組み合わせ推奨。

4. **Connector**ドロップダウンで`Arrow Flight`ドライバー設定済みのコネクターを選択。未作成の場合は隣のボタンから作成可能です。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照。

5. データ挿入方法を定義する**SQL**テンプレートを設定。

   ::: tip

   これは[Preprocessing SQL](./data-bridges.md#prepared-statement)テンプレートです。フィールド名をクォートで囲まず、SQL文末にセミコロン`;`を含めないでください。`${}`プレースホルダーはルールSQLで選択したフィールドと一致させる必要があります。

   :::

   ::: tip

   コネクター設定以外のデータベースに挿入する場合は、SQLテンプレート内で明示的に対象データベース名を指定してください。コネクターは対象データベースの存在確認を行います。

   :::

   例：

   ```sql
   insert into t_mqtt_msg(time, msgid, sender, topic, qos, payload, arrived) values (${timestamp}, ${id}, ${clientid}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```

6. **Fallback Actions**（任意）：信頼性向上のため、Sink処理失敗時にトリガーされるフォールバックアクションを1つ以上設定可能。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照。

7. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照。

8. **Create**をクリック前に、**Test Connection**でSinkがDatalayersサーバーに接続可能か検証可能。

9. **Create**をクリックしてSink作成を完了。**Create Rule**ページに戻ると、**Action Outputs**タブに新規Sinkが表示されます。

## ルールとSinkのテスト

ルールとSinkを設定後、テスト用MQTTメッセージをパブリッシュしてDatalayersへのデータ書き込みが成功しているか確認できます。

1. [MQTTX](https://mqttx.app/)を使い、トピック`t/1`にメッセージを送信します。これによりセッションイベント（クライアントのオンライン/オフラインなど）がトリガーされる場合もあります。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "temp": "23.5", "hum": "62", "precip": 2 }'
   ```

   このメッセージはルールエンジンをトリガーし、設定済みのDatalayers Sinkに転送されます。ルールにクライアント接続・切断などのセッションイベントが含まれる場合も同様にトリガーされます。

2. Sink実行統計を確認。EMQXダッシュボードの**Rules**ページで該当ルールを選択し、**Actions (Sink)**タブに切り替えます。対象Sinkの**Matched**および**Success**カウントが1増加していることを確認してください。

3. CLIでDatalayers内のデータを検証。

   DatalayersコンテナにアクセスしCLIツールを起動：

   ```bash
   docker exec -it datalayers bash
   dlsql -u admin -p public
   ```

   使用した書き込み方式に応じてSQLクエリを実行：

   - InfluxDB Line Protocol使用時はSink設定の`measurement`（例：`devices`）がテーブル名のデフォルト：

     ```sql
     use mqtt
     select * from devices
     ```

   - Arrow Flight SQL使用時は事前作成した対象テーブル（例：`t_mqtt_msg`）をクエリ：

     ```sql
     use mqtt
     select * from t_mqtt_msg
     ```

## 詳細設定

本節では、DatalayersコネクターおよびSinkで利用可能な詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**を展開して以下のパラメーターを用途に応じて調整できます。

| フィールド名              | 説明                                                                                                                       | デフォルト値 |
| ------------------------- | -------------------------------------------------------------------------------------------------------------------------- | ------------ |
| Buffer Pool Size          | バッファワーカープロセスの数を指定します。これらのプロセスはEMQXとDatalayers Sink間のデータフローを管理し、送信前のデータの一時保存・処理を担当します。特にegressタイプのSinkでパフォーマンス最適化とスムーズなデータ送信に重要です。ingressのみのBridgeでは`0`に設定可能です。 | `4`          |
| Request TTL               | リクエストの有効期限（秒）を指定します。リクエストがバッファに入ってからの経過時間がこのTTLを超えるか、Datalayersからの応答・アックが期限内に得られない場合、リクエストは期限切れと見なされます。 | `45`         |
| Health Check Interval     | SinkがDatalayersとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。                                         | `15`         |
| Max Buffer Queue Size     | Datalayers Sinkの各バッファワーカープロセスがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータ送信前の一時保存を担当し、データストリームの効率的処理に寄与します。システム性能やデータ送信要件に応じて調整してください。 | `1`          |
| Batch Size                | EMQXからDatalayersへ一度に転送するデータバッチの最大サイズを指定します。このサイズを調整することでデータ転送の効率とパフォーマンスを最適化可能です。<br />`1`に設定するとデータはバッチ化されず個別に送信されます。 | `100``       |
| Query Mode                | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信を最適化します。非同期モードではDatalayersへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージを受信してもDatalayersへの書き込みが完了していない場合があります。 | `Asynch`     |
| Inflight Window           | 未応答または未アックのキュー内リクエスト数の最大値を指定します。<br/>`Request Mode`が`asynchronous`の場合に重要で、同一MQTTクライアントからのメッセージを厳密に順次処理したい場合は`1`に設定してください。 | `100`        |
