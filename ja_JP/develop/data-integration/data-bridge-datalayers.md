# MQTTデータをDatalayersに取り込む

Datalayersは、産業用IoT、IoV、エネルギーなどの分野向けに設計されたマルチモーダルかつハイパーコンバージドなデータベースです。高いデータスループットと安定したパフォーマンスを備えており、IoTアプリケーションに最適です。EMQXは現在、Sinkを介してDatalayersにメッセージやデータを保存することをサポートしており、データ分析や可視化を容易にしています。

本ページでは、EMQXとDatalayersのデータ統合について詳しく解説し、ルールとSinkの作成方法について実践的な手順を提供します。

## 動作の仕組み

Datalayersとのデータ統合はEMQXの標準機能であり、デバイスからのMQTTメッセージをシームレスにDatalayersへ転送して保存・分析を可能にします。ルールとSinkを設定することで、処理済みのMQTTデータを柔軟にDatalayersへルーティングできます。

以下の図は、エネルギー蓄電シナリオにおけるEMQXとDatalayersの統合アーキテクチャの典型例を示しています。

![MQTT to Datalayers](./assets/mqtt-to-datalayers.jpg)

このアーキテクチャでは、EMQXがデバイスの接続管理、メッセージ転送、ルールベースの処理を担当し、Datalayersがデータの保存、分析、可視化を担います。両者が連携することで、エネルギー消費のリアルタイムデータを効率的に収集・分析するスケーラブルなIoTプラットフォームを構築できます。

EMQX 6.0.0以降、DatalayersはApache Arrowベースの高性能バイナリ通信プロトコルであるArrow Flight SQLをサポートしています。従来のInfluxDB Line Protocolと比較して、Arrow Flight SQLはより効率的なデータ転送と構造化データの書き込みに強みがあります。

::: warning 注意

Arrow FlightドライバーはRustで実装され、Erlang VMにNative Implemented Function（NIF）を介して統合されています。本機能は現在実験的であり、テスト環境での利用を推奨します。

:::

具体的なワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：デバイスはMQTT経由でEMQXに接続し、電力、電流、電圧などのエネルギー関連メトリクスを定期的にパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジンに渡します。

2. **ルールエンジンによるメッセージ処理**：EMQXの組み込みルールエンジンはトピックパターンに基づいてメッセージをマッチングし、ペイロードの変換、フィールドのフィルタリング、コンテキスト情報の付加などの処理を行います。

3. **Datalayersへの書き込み**：ルールがトリガーされると、処理済みデータをDatalayersに書き込むSinkアクションが実行されます。SinkはフィールドをDatalayersのテーブルやカラムにマッピングするためのカスタマイズ可能なSQLテンプレートをサポートしています。

   EMQXは以下の2つの書き込み方法をサポートしています：

   - InfluxDB Line Protocol
   - Arrow Flight SQLドライバー

   Sinkの設定は選択した方法に応じて異なります。

エネルギー蓄電データがDatalayersに書き込まれた後は、対応ツールを使って柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続し、蓄電データのチャートを生成・表示する。
- 業務システムに接続し、蓄電デバイスの状態監視やアラートを実施する。

## 特長とメリット

Datalayersとのデータ統合は以下の特長と利点を提供します：

- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを処理可能であり、Datalayersはデータの書き込み、保存、クエリに優れているため、IoTシナリオのデータ処理要件をシステムに負荷をかけずに満たせます。
- **メッセージ変換**：EMQXのルール内でメッセージを多様に処理・変換してからDatalayersに書き込めます。
- **スケーラビリティ**：EMQXとDatalayersは共にクラスタリング機能を持ち、ビジネスの成長に応じて柔軟に水平スケール可能です。
- **豊富なクエリ機能**：Datalayersはタイムスタンプデータの効率的なクエリと分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから価値ある洞察を抽出できます。
- **効率的なストレージ**：Datalayersは高圧縮エンコーディング方式を採用し、ストレージコストを大幅に削減します。また、カスタマイズ可能なデータ保持期間により不要データのストレージ占有を防止します。

## はじめる前に

このセクションでは、EMQXでDatalayers Sinkを作成する前に必要な準備として、Datalayersのインストール、データベース作成、テーブル構造の定義について説明します。

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解
- 使用するドライバーに応じて、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)または[Arrow Flight SQL](https://arrow.apache.org/docs/format/FlightSql.html#arrow-flight-sql)の理解

### Datalayersのインストールとセットアップ

1. Dockerを使ってDatalayersをインストール・起動します。詳細手順は[Install Datalayers](https://docs.datalayers.cn/datalayers/latest/getting-started/docker.html)を参照してください。

   ```bash
   # Datalayersコンテナを起動
   docker run -d --name datalayers -p 8360:8360 -p 8361:8361 datalayers/datalayers:latest
   ```

   - ポート`8360`はArrow Flight SQL用のデフォルトgRPCポートです。
   - ポート`8361`はHTTPポートで、主にLine Protocol書き込みや管理APIに使用されます。

2. Datalayersサービス起動後、デフォルトのユーザー名・パスワード`admin`/`public`でDatalayers CLIに入ります。CLIでデータベースを作成する手順は以下の通りです：

   - Datalayersコンテナにアクセス：

     ```bash
     docker exec -it datalayers bash
     ```

   - Datalayers CLIに入る：

     ```bash
     dlsql -u admin -p public
     ```

   - データベースを作成（例：`mqtt`）：

     ```sql
     create database mqtt
     ```

4. Arrow Flight SQLドライバーを使用する場合は、対象テーブルを事前に作成する必要があります。

   ::: tip 注意

   InfluxDB Line Protocolを使用する場合はテーブルの事前作成は不要です。Datalayersは受信したLine Protocolデータの`measurement`とフィールド定義に基づいて自動的にテーブルを作成します。

   :::

   例として、`t_mqtt_msg`というテーブルを以下のSQLで作成します：

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

このセクションでは、EMQXでDatalayersサーバーに接続するためのコネクターを作成する方法を説明します。

以下の手順はEMQXとDatalayersがローカルで稼働していることを前提としています。別環境やリモート環境にデプロイしている場合は接続設定を適宜更新してください。

1. EMQXダッシュボードで、**Integration** -> **Connectors**に移動します。

2. ページ右上の**Create**をクリックします。

3. **Create Connector**ページで**Datalayers**を選択し、**Next**をクリックします。

5. **Configuration**ページでコネクターの詳細を入力します：

   - **Connector Name**：英数字で始まり、英数字・ハイフン・アンダースコアのみ使用可能。例：`my_datalayers`
   - **Description**（任意）：後で識別しやすいよう説明を追加可能。

   Datalayersサーバー接続の設定：

   - **Driver Type**：

     - `InfluxDB Line Protocol`：InfluxDB互換のLine Protocolを使用してデータを取り込みます。テーブルは自動作成されます。

     - `Arrow Flight`：SQLテンプレートを用いた高性能な構造化データ書き込みを可能にします。スキーマ管理が厳密で高い書き込みスループットが必要な場合に適しています。

       ::: warning 注意

       Arrow FlightドライバーはRustで実装され、Erlang VMにNIFで統合されています。現在実験的機能のため、テスト環境での評価を推奨します。

       :::

   - **Server Host**：

     - デフォルト：`127.0.0.1:8361`
     - `Arrow Flight`ドライバー使用時はgRPC通信のためポート`8360`を使用します。

   - **Database Name**：Datalayersの対象データベース名（例：`mqtt`）

   - **Username / Password**：Datalayersアクセス用の認証情報（例：`admin` / `public`）

   - **Enable TLS**（任意）：暗号化接続を有効にします。有効化すると証明書パスや検証オプションを設定可能です。詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。

     ::: warning Arrow Flight TLS要件

     EMQX 6.3.3以降、`Arrow Flight`ドライバーでTLSを有効にする場合は以下の要件があります：

     - EMQXは常にDatalayersサーバー証明書を検証します。`TLS Verify`は有効のままにしてください。`verify_none`はArrow Flightで拒否されます。未設定の場合はEMQXがデフォルト値（`verify = verify_peer`または`cacertfile = "system_default"`）を適用します。
     - `system_default`は`node.trusted_certs`で選択されたCAバンドルを使用します。Datalayersサーバー証明書を発行したCAが含まれていない場合は、**CA Cert**にPEM形式のCA証明書を指定してください。詳細は[アウトバウンドTLSの信頼CA証明書](../../guides/network/tls-certificate.md#trusted-ca-certificates-for-outbound-tls)を参照。
     - **Certificate Source**は**手動入力**を選択し、**TLS Cert**と**TLS Key**は空欄のままにしてください。Arrow Flightはクライアント証明書設定（`certfile`、`keyfile`）や管理証明書バンドル（`managed_certs`）をサポートしません。
     - `server_name_indication`を`disable`に設定しないでください。サーバー証明書はコネクターのホスト名または明示的に設定したSNIサーバー名と一致する必要があります。

     これらは`Arrow Flight`ドライバーのみの要件であり、`InfluxDB Line Protocol`ドライバーのTLS動作には影響しません。

     :::

5. ドライバーに`Arrow Flight`を選択した場合、**Enable Prepared Statements**オプションが表示されます。これはSinkがSQLテンプレートを使ってデータ挿入できるかを制御し、デフォルトで有効です。

6. **Create**をクリックする前に、**Test Connectivity**でDatalayersサーバーへの接続テストが可能です。

7. ページ下部の**Create**をクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**か、続けて**Create Rule**でルールとSinkの作成に進めます。詳細は[Create a Datalayers Rule](#create-a-datalayers-rule)を参照してください。

## Datalayersルールの作成

このセクションでは、EMQXでトピック`t/#`からのMQTTメッセージを処理し、処理結果を設定済みのDatalayers Sinkに送信するルールの作成方法を説明します。

### SQLを定義したルールの作成

1. EMQXダッシュボードの左メニューから**Data Integration** -> **Rules**に移動します。

2. **Rules**ページ右上の**Create**ボタンをクリックします。

3. ルール作成フォームでルールIDを入力します（例：`my_rule`）。

4. **SQL Editor**でルールロジックを定義します。トピック`t/#`にパブリッシュされたMQTTメッセージをDatalayersに保存するには、以下のSQLを使用できます：

   ::: tip 注意

   カスタムSQLルールを書く場合、Sinkテンプレート内で参照する変数（例：`${clientid}`、`${payload.temp}`）はすべてルールの`SELECT`句に含める必要があります。

   :::

   ```
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   EMQXのSQLに不慣れな場合は、**SQL Examples**や**Enable Debug**をクリックしてサンプルクエリを参照し、出力をテストできます。

   :::

5. ルールにDatalayers Sinkを追加し、処理結果をDatalayersに書き込みます。

   - **InfluxDB Line Protocol**を使用する場合は、[Add an InfluxDB Line Protocol Sink](#add-an-influxdb-line-protocol-sink)を参照してください。
   - **Arrow Flight SQLドライバー**を使用する場合は、[Add an Arrow Flight SQL Sink](#add-an-arrow-flight-sql-sink)を参照してください。

6. **Create Rule**ページで設定内容を確認し、**Save**をクリックしてルールを作成します。

作成後、ルールは**Rules**一覧に表示されます。対象ルールの**Actions (Sink)**タブでDatalayers Sinkを確認できます。

また、**Integrations** -> **Flow Designer**でトポロジーグラフを表示すると、トピック`t/#`のメッセージが`my_rule`ルールで処理され、Datalayersに書き込まれる様子が可視化されます。

### InfluxDB Line Protocol Sinkの追加

このセクションでは、InfluxDB Line Protocolを使って処理済みデータをDatalayersに書き込むSinkをルールに追加する方法を説明します。

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルール条件にマッチした際にトリガーされるアクションを定義します。このアクションで処理済みメッセージをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のDatalayers Sinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sinkの名前を入力します（例：`dl_sink_influx`）。名前は英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンで、`InfluxDB Line Protocol`ドライバーで設定済みのコネクターを選択します。利用可能なコネクターがない場合は隣のボタンから作成できます。詳細は[Create a Datalayers Connector](#create-a-datalayers-connector)を参照してください。

5. **Time Precision**はデフォルトでミリ秒に設定します。

6. Datalayersへのデータ解析・書き込み用の**Data Format**と内容を定義します。`JSON`または`Line Protocol`から選択可能です：

   - **JSON**：

     **Measurement**、**Fields**、**Timestamp**、**Tags**を指定します。キーと値は定数または`${payload.temp}`のような変数プレースホルダーをサポートします。フォーマットルールは[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     **Fields**はCSVファイルを使った一括設定も可能です。[Use CSV to Batch Configure Fields](#use-csv-to-batch-configure-fields)を参照してください。

   - **Line Protocol**：

     テーブル、フィールド、タイムスタンプ、タグを含む単一のLine Protocol文字列を定義できます。キーと値は定数またはプレースホルダーをサポートします。構文は[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     ::: tip

     Datalayersに書き込むデータはInfluxDB v1のLine Protocolと完全互換なので、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)の設定例を参考にできます。

     例えば、符号付き整数値を入力する場合は、`${payload.int}i`のように値の後に`i`を付けます。詳細は[InfluxDB 1.8で整数値を書く方法](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。

     :::

     Line Protocolの例：

     ```sql
     devices,clientid=${clientid} temp=${payload.temp},hum=${payload.hum},precip=${payload.precip}i ${timestamp}
     ```

7. **Fallback Actions**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

8. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照してください。

9. **Create**をクリックする前に、**Test Connectivity**でSinkがDatalayersサーバーに接続できるかテスト可能です。

10. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

#### CSVを使ったフィールド一括設定

::: tip

この機能は**InfluxDB Line Protocol**のSinkで、データフォーマットが`JSON`の場合のみ利用可能です。フィールド設定を一括でインポートできます。

:::

Datalayersのデータエントリーは数百のフィールドを含むことが多く、データフォーマット設定が煩雑になりがちです。これを解決するため、EMQXはフィールド一括設定機能を提供しています。

JSONフォーマット設定時に、CSVファイルからフィールドのキー・値ペアを一括インポートできます。

1. **Fields**テーブルの**Batch Settings**ボタンをクリックし、**Import Batch Settings**ポップアップを開きます。

2. 指示に従い、一括設定テンプレートファイルをダウンロードし、フィールドのキー・値ペアを記入します。テンプレートのデフォルト内容は以下の通りです：

   | Field  | Value              | 備考（任意）                                              |
   | ------ | ------------------ | --------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                           |
   | hum    | ${payload.hum}     |                                                           |
   | precip | ${payload.precip}i | 値の後に`i`を付けるとDatalayersは整数型として保存します。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、Line Protocolに従い型識別子を付加可能。
   - **備考**：CSV内のコメント用で、EMQXへのインポート対象外。

   一括設定CSVファイルは2048行を超えられません。

3. 記入済みテンプレートを保存し、**Import Batch Settings**ポップアップにアップロードして**Import**をクリックし、一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・値ペアをさらに調整可能です。

### Arrow Flight SQL Sinkの追加

このセクションでは、**Arrow Flight SQL**ドライバーを使い、SQLのINSERT文でDatalayersにデータを書き込むSinkをルールに追加する方法を説明します。

::: warning 注意

Arrow Flight SQLドライバーは現在実験的です。運用環境での利用は慎重に行ってください。

:::

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルールマッチ時にトリガーされるアクションを定義します。このアクションで処理済みデータをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のDatalayers Sinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sinkの名前を入力します（例：`dl_sink_arrow`）。英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンで`Arrow Flight`ドライバーで設定済みのコネクターを選択します。存在しない場合は隣のボタンから作成可能です。詳細は[Create a Datalayers Connector](#create-a-datalayers-connector)を参照してください。

5. データを対象テーブルに挿入する方法を定義する**SQL**テンプレートを設定します。

   ::: tip

   これは[Preprocessing SQL](./data-bridges.md#prepared-statement)テンプレートです。フィールド名は引用符で囲まず、SQL文の末尾にセミコロン`;`を付けないでください。`${}`のプレースホルダーはルールSQLで選択したフィールドと一致させる必要があります。

   :::

   ::: tip

   コネクター設定のデータベース以外にデータを挿入する場合は、SQLテンプレート内で対象データベース名を明示的に指定してください。ただし、コネクターは対象データベースの存在をチェックします。

   :::

   例：

   ```sql
   insert into t_mqtt_msg(time, msgid, sender, topic, qos, payload, arrived) values (${timestamp}, ${id}, ${clientid}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```

6. **Fallback Actions**（任意）：信頼性向上のため、Sinkがメッセージ処理に失敗した場合にトリガーされるフォールバックアクションを1つ以上設定できます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

7. **Advanced Settings**を展開し、必要に応じて追加設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connection**ボタンでSinkがDatalayersサーバーに接続できるか検証可能です。

9. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

## ルールとSinkのテスト

ルールとSinkの設定後、テスト用MQTTメッセージをパブリッシュしてDatalayersへの書き込みが成功しているか確認できます。

1. [MQTTX](https://mqttx.app/)などを使い、トピック`t/1`にメッセージを送信します。これによりセッションイベント（クライアントのオンライン/オフラインなど）もトリガーされる場合があります。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "temp": "23.5", "hum": "62", "precip": 2 }'
   ```

   このメッセージはルールエンジンをトリガーし、設定済みのDatalayers Sinkに転送されます。ルールにクライアント接続・切断などのセッションイベントが含まれている場合も同様にトリガーされます。

2. Sinkの実行統計を確認します。EMQXダッシュボードの**Rules**ページで対象ルールを探し、**Actions (Sink)**タブに切り替えます。対象Sinkの**Matched**と**Success**のカウントが1増えていることを確認してください。

3. CLIからDatalayersのデータを確認します。

   Datalayersコンテナにアクセスし、CLIツールを起動します：

   ```bash
   docker exec -it datalayers bash
   dlsql -u admin -p public
   ```

   書き込み方法に応じてSQLクエリを実行します：

   - InfluxDB Line Protocolの場合、テーブル名はSink設定の`measurement`（例：`devices`）がデフォルトです：

     ```sql
     use mqtt
     select * from devices
     ```

   - Arrow Flight SQLの場合、事前に作成した対象テーブル（例：`t_mqtt_msg`）をクエリします：

     ```sql
     use mqtt
     select * from t_mqtt_msg
     ```

## 詳細設定

このセクションでは、DatalayersコネクターおよびSinkの詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**を展開して以下のパラメーターを必要に応じて調整できます。

| フィールド名              | 説明                                                                                                                                            | デフォルト値 |
| ------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- | ------------ |
| Buffer Pool Size          | バッファワーカープロセスの数を指定します。これらのプロセスはEMQXとDatalayersのEgressタイプSink間のデータフローを管理し、データを一時的に保存・処理してから送信します。特にパフォーマンス最適化やスムーズなデータ送信に重要です。Ingressのみを扱うブリッジでは`0`に設定可能です。 | `4`          |
| Request TTL               | リクエストTTL（Time to Live）は、リクエストがバッファに入ってから有効とみなされる最大時間（秒）を指定します。TTLを超えたリクエストや、送信後にDatalayersからの応答・アックがタイムリーに得られない場合は期限切れとみなされます。 | `45`         |
| Health Check Interval     | SinkがDatalayersとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。                                                                 | `15`         |
| Max Buffer Queue Size     | Datalayers Sinkの各バッファワーカープロセスがバッファリング可能な最大バイト数を指定します。ワーカープロセスはデータ送信前に一時的にデータを保持し、効率的なデータストリーム処理を行います。システム性能やデータ送信要件に応じて調整してください。 | `1`          |
| Batch Size                | EMQXからDatalayersに一度に転送するデータバッチの最大サイズを指定します。この値を調整することでデータ転送の効率やパフォーマンスを最適化できます。<br />`1`に設定すると、データはバッチ化されず個別に送信されます。 | `100``       |
| Query Mode                | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信を要件に応じて最適化します。非同期モードではDatalayersへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがDatalayersに到達する前にメッセージを受信する可能性があります。 | `Asynch`     |
| Inflight Window           | インフライトキューリクエストとは、送信済みで応答やアックをまだ受け取っていないリクエストのことです。SinkとDatalayers間の通信で同時に存在可能なインフライトキューリクエストの最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合、このパラメーターは特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`        |
