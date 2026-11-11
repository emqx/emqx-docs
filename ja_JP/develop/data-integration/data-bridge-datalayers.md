# DatalayersへのMQTTデータ取り込み

Datalayersは、産業用IoT、IoV、エネルギーなどの分野向けに設計されたマルチモーダルかつハイパーコンバージドなデータベースです。高いデータスループットと安定したパフォーマンスを備えており、IoTアプリケーションに最適です。EMQXは現在、Sinkを介してDatalayersへのメッセージおよびデータの保存をサポートしており、データ分析や可視化を容易にしています。

本ページでは、EMQXとDatalayersのデータ統合について詳しく解説し、ルールとSinkの作成方法を実践的に案内します。

## 動作概要

Datalayersデータ統合はEMQXの標準機能であり、デバイスからのMQTTメッセージをシームレスにDatalayersへ転送し、保存および分析を可能にします。ルールとSinkを設定することで、処理済みのMQTTデータを柔軟にDatalayersへルーティングできます。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとDatalayersの典型的なアーキテクチャを示しています。

![MQTT to Datalayers](./assets/mqtt-to-datalayers.jpg)

このアーキテクチャでは、EMQXがデバイスの接続管理、メッセージ転送、ルールベースの処理を担当し、Datalayersがデータの保存、分析、可視化を担います。両者が連携することで、エネルギー消費のリアルタイムデータを効率的に収集・分析するスケーラブルなIoTプラットフォームを構築します。

EMQX 6.0.0以降、DatalayersはApache Arrowに基づく高性能バイナリ通信プロトコルであるArrow Flight SQLをサポートしています。従来のInfluxDB Line Protocolと比較して、Arrow Flight SQLはより効率的なデータ転送と構造化データ書き込みの強力なサポートを提供します。

::: warning 注意

Arrow FlightドライバーはRustで実装され、Erlang VMにNative Implemented Function（NIF）を通じて統合されています。本機能は現在実験的であり、テスト環境での利用を推奨します。

:::

具体的なワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**  
   デバイスはMQTTでEMQXに接続し、電力、電流、電圧などのエネルギー関連メトリクスを定期的にパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジンに渡します。

2. **ルールエンジンによるメッセージ処理**  
   EMQXの組み込みルールエンジンはトピックパターンに基づいて受信メッセージをマッチングし、ペイロードの変換、フィールドのフィルタリング、コンテキスト情報の付加などの処理を行います。

3. **Datalayersへの書き込み**  
   ルールがトリガーされると、処理済みデータをDatalayersに書き込むSinkアクションが実行されます。SinkはフィールドをDatalayersのテーブル・カラムにマッピングするカスタマイズ可能なSQLテンプレートをサポートします。

   EMQXは以下の2つの書き込み方式をサポートしています。

   - InfluxDB Line Protocol
   - Arrow Flight SQLドライバー

   Sinkの設定は選択した方式により異なります。

エネルギー貯蔵データがDatalayersに書き込まれた後は、対応するツールを使って柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続し、エネルギー貯蔵データのチャート表示を行う。
- 業務システムに接続し、エネルギー貯蔵機器の状態監視やアラートを実施する。

## 特長とメリット

Datalayersデータ統合の主な特長と利点は以下の通りです。

- **効率的なデータ処理**  
  EMQXは多数のIoTデバイス接続とメッセージスループットを処理可能であり、Datalayersはデータ書き込み・保存・クエリに優れているため、システムに過負荷をかけずにIoTシナリオのデータ処理要件を満たします。

- **メッセージ変換**  
  EMQXのルール内でメッセージを高度に処理・変換してからDatalayersに書き込むことが可能です。

- **スケーラビリティ**  
  EMQXとDatalayersの両方がクラスター機能を備えており、ビジネスの成長に応じた柔軟な水平スケールアウトが可能です。

- **豊富なクエリ機能**  
  Datalayersはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから価値あるインサイトを抽出できます。

- **効率的なストレージ**  
  Datalayersは高圧縮エンコーディングを用いてストレージコストを大幅に削減し、不要なデータがストレージを占有しないようカスタマイズ可能なデータ保持期間も設定可能です。

## はじめる前に

このセクションでは、EMQXでDatalayers Sinkを作成する前に必要な準備として、Datalayersのインストール、データベース作成、テーブル構造定義について説明します。

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解
- 使用予定のドライバーに応じて、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)または[Arrow Flight SQL](https://arrow.apache.org/docs/format/FlightSql.html#arrow-flight-sql)の理解

### Datalayersのインストールとセットアップ

1. Dockerを使用してDatalayersをインストールし起動します。詳細は[Install Datalayers](https://docs.datalayers.cn/datalayers/latest/getting-started/docker.html)を参照してください。

   ```bash
   # Datalayersコンテナを起動
   docker run -d --name datalayers -p 8360:8360 -p 8361:8361 datalayers/datalayers:latest
   ```

   - ポート`8360`はArrow Flight SQL用のgRPCのデフォルトポートです。
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

   - データベース作成（例：`mqtt`）：

     ```sql
     create database mqtt
     ```

4. Arrow Flight SQLドライバーを使用する場合は、対象テーブルを事前に作成する必要があります。

   ::: tip 注意

   InfluxDB Line Protocolを使用する場合はテーブルの事前作成は不要です。Datalayersは受信したLine Protocolデータの`measurement`とフィールド定義に基づき自動的にテーブルを作成します。

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

このセクションでは、EMQXでSinkをDatalayersサーバーに接続するためのコネクター作成手順を示します。

以下の手順はEMQXとDatalayersがローカルで稼働していることを前提としています。別環境やリモート環境にデプロイしている場合は接続設定を適宜変更してください。

1. EMQXダッシュボードで、**Integration** -> **Connectors**をクリックします。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**画面で**Datalayers**を選択し、**Next**をクリックします。

5. **Configuration**画面でコネクターの詳細を入力します。

   - **Connector Name**：英数字で始まり、英数字、ハイフン、アンダースコアのみ使用可能。例：`my_datalayers`
   - **Description**（任意）：後で識別しやすいよう説明を追加可能。

   Datalayersサーバー接続設定：

   - **Driver Type**：

     - `InfluxDB Line Protocol`：InfluxDB互換のLine Protocolを用いたデータ取り込み。テーブルは自動作成されます。

     - `Arrow Flight`：SQLテンプレートを用いた高性能な構造化データ書き込みを有効化。厳密なスキーマ管理や高スループット書き込みが必要な場合に最適です。

       ::: warning 注意

       Arrow FlightドライバーはRustで実装され、Erlang VMにNIF経由で統合されています。本機能は現在実験的であり、テスト環境での評価を推奨します。

       :::

   - **Server Host**：

     - デフォルト：`127.0.0.1:8361`
     - `Arrow Flight`ドライバー使用時はgRPC通信でポート`8360`を使用します。

   - **Database Name**：Datalayersの対象データベース名（例：`mqtt`）

   - **Username / Password**：Datalayersアクセス用の認証情報（例：`admin` / `public`）

   - **Enable TLS**（任意）：暗号化接続を有効にする場合に切り替え。証明書パスや検証オプションの設定が可能です。詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。

     ::: warning Arrow Flight TLS要件

     EMQX 6.3.3以降、`Arrow Flight`ドライバーのTLS有効化時には以下の要件があります。

     - EMQXは常にDatalayersサーバー証明書を検証します。`TLS Verify`は有効のままにしてください。`verify_none`はArrow Flightで拒否されます。未設定の場合、EMQXはデフォルトの`verify = verify_peer`または`cacertfile = "system_default"`を適用します。
     - `system_default`は`node.trusted_certs`で選択されたCAバンドルを使用します。Datalayersサーバー証明書を発行したCAが含まれていない場合は、**CA Cert**にPEM形式のCA証明書を指定してください。詳細は[アウトバウンドTLSの信頼済みCA証明書](../../guides/network/tls-certificate.md#trusted-ca-certificates-for-outbound-tls)を参照してください。
     - **Certificate Source**は**Enter Manually**を選択し、**TLS Cert**と**TLS Key**は空欄のままにしてください。Arrow Flightはクライアント証明書設定（`certfile`と`keyfile`）や管理証明書バンドル（`managed_certs`）をサポートしません。
     - `server_name_indication`を`disable`に設定しないでください。サーバー証明書はコネクターのホスト名または明示的に設定されたSNIサーバー名と一致する必要があります。

     これらの要件は`Arrow Flight`ドライバーにのみ適用され、`InfluxDB Line Protocol`ドライバーのTLS動作は従来通りです。

     :::

5. `Arrow Flight`ドライバー選択時は、**Enable Prepared Statements**オプションが表示されます。これはSinkがSQLテンプレートを用いたデータ挿入を行うかどうかの設定で、デフォルトで有効です。

6. **Create**をクリックする前に、**Test Connectivity**でDatalayersサーバーへの接続テストが可能です。

7. 画面下部の**Create**をクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択できます。ルールとSinkの作成手順は[Create a Datalayers Rule](#create-a-datalayers-rule)を参照してください。

## Datalayersルールの作成

このセクションでは、EMQXでトピック`t/#`からのMQTTメッセージを処理し、処理結果を設定済みのDatalayers Sinkに送信するルールの作成方法を示します。

### SQLを定義したルールの作成

1. EMQXダッシュボードの左メニューから**Data Integration** -> **Rules**に移動します。

2. **Rules**ページ右上の**Create**ボタンをクリックします。

3. ルール作成フォームでルールIDを入力します（例：`my_rule`）。

4. **SQL Editor**でルールロジックを定義します。トピック`t/#`にパブリッシュされたMQTTメッセージをDatalayersに保存する例は以下の通りです。

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

   EMQXのSQLが初めての場合は、**SQL Examples**や**Enable Debug**をクリックしてサンプルクエリを確認し、出力をテストできます。

   :::

5. ルールにDatalayers Sinkを追加し、処理結果をDatalayersに書き込みます。

   - **InfluxDB Line Protocol**を使用する場合は：[Add an InfluxDB Line Protocol Sink](#add-an-influxdb-line-protocol-sink)を参照してください。
   - **Arrow Flight SQLドライバー**を使用する場合は：[Add an Arrow Flight SQL Sink](#add-an-arrow-flight-sql-sink)を参照してください。

6. **Create Rule**ページで設定内容を確認し、**Save**をクリックしてルールを作成します。

作成したルールは**Rules**一覧に表示されます。ルールの**Actions (Sink)**タブをクリックすると、このルールに紐づくDatalayers Sinkを確認できます。

また、**Integrations** -> **Flow Designer**でトポロジーグラフを表示すると、トピック`t/#`からのメッセージが`my_rule`ルールで処理され、Datalayersに書き込まれる様子が視覚的に確認できます。

### InfluxDB Line Protocol Sinkの追加

このセクションでは、InfluxDB Line Protocolを用いて処理済みデータをDatalayersに書き込むSinkをルールに追加する方法を説明します。

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルール条件にマッチした際にトリガーされるアクションを定義します。このアクションは処理済みメッセージをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のDatalayers Sinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sinkの名前を入力します（例：`dl_sink_influx`）。名前は英数字の組み合わせで指定してください。

4. **Connector**ドロップダウンから、`InfluxDB Line Protocol`ドライバーで設定済みのコネクターを選択します。コネクターがない場合は隣のボタンから作成可能です。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照してください。

5. **Time Precision**はデフォルトでミリ秒に設定します。

6. Datalayersにデータを書き込むための**Data Format**と内容を定義します。`JSON`か`Line Protocol`のいずれかを選択可能です。

   - **JSON**：

     **Measurement**、**Fields**、**Timestamp**、**Tags**を指定します。キーと値は定数または`${payload.temp}`のような変数プレースホルダーをサポートします。書式ルールは[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     **Fields**はCSVファイルによる一括設定も可能です。[Use CSV to Batch Configure Fields](#use-csv-to-batch-configure-fields)を参照してください。

   - **Line Protocol**：

     テーブル、フィールド、タイムスタンプ、タグを含む1行のLine Protocol文字列を定義できます。キーと値は定数またはプレースホルダーをサポートします。構文は[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     ::: tip

     Datalayersに書き込むデータはInfluxDB v1のLine Protocolと完全互換のため、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)も参考にできます。

     例えば、符号付き整数値を入力する場合は、プレースホルダーの後に`i`を付けます（例：`${payload.int}i`）。詳細は[InfluxDB 1.8で整数値を書き込む](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。

     :::

     Line Protocolの例：

     ```sql
     devices,clientid=${clientid} temp=${payload.temp},hum=${payload.hum},precip=${payload.precip}i ${timestamp}
     ```

7. **Fallback Actions**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

8. **Advanced Settings**を展開し、必要に応じて詳細設定を行います（任意）。詳細は[Advanced Settings](#advanced-settings)を参照してください。

9. **Create**をクリックする前に、**Test Connectivity**でSinkがDatalayersサーバーに接続可能かテストできます。

10. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

#### CSVによるフィールド一括設定の利用

::: tip

この機能は**InfluxDB Line Protocol**のSinkで、データフォーマットが`JSON`の場合のみ利用可能です。フィールド設定を一括インポートできます。

:::

Datalayersのデータエントリーは数百のフィールドを含むことが多く、データフォーマット設定が煩雑になるため、EMQXはフィールド一括設定機能を提供しています。

JSONフォーマット設定時に、CSVファイルからキー・バリューのペアを一括インポートできます。

1. **Fields**テーブルの**Batch Settings**ボタンをクリックし、**Import Batch Settings**ポップアップを開きます。

2. 指示に従いテンプレートファイルをダウンロードし、フィールドのキー・バリューを入力します。テンプレートのデフォルト内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | フィールド値の後に`i`を付けるとDatalayersは整数型として保存 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポート。Line Protocolに従い型指定子を付加可能。
   - **備考**：CSV内のコメント用で、EMQXにはインポートされません。

   バッチ設定CSVファイルは2048行を超えないようにしてください。

3. 入力済みテンプレートファイルを保存し、**Import Batch Settings**ポップアップにアップロードして**Import**をクリックし一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・バリューをさらに調整可能です。

### Arrow Flight SQL Sinkの追加

このセクションでは、**Arrow Flight SQL**ドライバーを使用して、SQL挿入文でDatalayersにデータを書き込むSinkをルールに追加する方法を説明します。

::: warning 注意

Arrow Flight SQLドライバーは現在実験的です。商用環境での利用は慎重に行ってください。

:::

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルールマッチ時にトリガーされるアクションを定義します。このアクションは処理済みデータをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のDatalayers Sinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sinkの名前を入力します（例：`dl_sink_arrow`）。英数字の組み合わせを推奨します。

4. **Connector**ドロップダウンから、`Arrow Flight`ドライバーで設定済みのコネクターを選択します。存在しない場合は隣のボタンから作成可能です。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照してください。

5. データを対象テーブルに挿入する方法を定義する**SQL**テンプレートを設定します。

   ::: tip

   これは[プリプロセスSQL](./data-bridges.md#prepared-statement)テンプレートです。フィールド名はクォートで囲まず、SQL文の末尾にセミコロン`;`を含めないでください。すべての`${}`プレースホルダーはルールSQLで選択したフィールドと一致させる必要があります。

   :::

   ::: tip

   コネクターで設定したデータベース以外にデータを挿入する場合は、SQLテンプレート内で明示的に対象データベース名を指定してください。なお、コネクターは対象データベースの存在をチェックします。

   :::

   例：

   ```sql
   insert into t_mqtt_msg(time, msgid, sender, topic, qos, payload, arrived) values (${timestamp}, ${id}, ${clientid}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```

6. **Fallback Actions**（任意）：信頼性向上のため、Sink処理失敗時にトリガーされるフォールバックアクションを1つ以上設定可能です。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

7. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connection**ボタンでSinkがDatalayersサーバーに接続可能か検証します。

9. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

## ルールとSinkのテスト

ルールとSinkの設定後、テスト用MQTTメッセージをパブリッシュしてDatalayersへの書き込みが成功しているか確認できます。

1. [MQTTX](https://mqttx.app/)を使い、トピック`t/1`にメッセージを送信します。セッションイベント（クライアントのオンライン/オフラインなど）がトリガーされる場合もあります。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "temp": "23.5", "hum": "62", "precip": 2 }'
   ```

   このメッセージはルールエンジンをトリガーし、設定済みのDatalayers Sinkに転送されます。ルールにクライアント接続・切断などのセッションイベントが含まれる場合も、この操作でトリガーされます。

2. Sinkの実行統計を確認します。EMQXダッシュボードの**Rules**ページで対象ルールを探し、**Actions (Sink)**タブに切り替えます。対象Sinkの**Matched**と**Success**のカウントが1増えていることを確認してください。

3. CLIでDatalayers内のデータを確認します。

   DatalayersコンテナにアクセスしCLIツールを起動します。

   ```bash
   docker exec -it datalayers bash
   dlsql -u admin -p public
   ```

   使用した書き込み方式に応じてSQLクエリを実行します。

   - InfluxDB Line Protocolの場合、Sink設定の`measurement`がテーブル名のデフォルトです（例：`devices`）。

     ```sql
     use mqtt
     select * from devices
     ```

   - Arrow Flight SQLの場合、事前作成した対象テーブルをクエリします（例：`t_mqtt_msg`）。

     ```sql
     use mqtt
     select * from t_mqtt_msg
     ```

## 詳細設定

このセクションでは、DatalayersコネクターおよびSinkの詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**を展開して以下のパラメーターを用途に応じて調整できます。

| フィールド名             | 説明                                                                                                                           | デフォルト値 |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------ | ------------ |
| Buffer Pool Size         | バッファワーカープロセスの数を指定します。これらのプロセスはEMQXとDatalayersのエグレス型Sink間のデータフローを管理し、データを一時的に保存・処理してから送信します。エグレスシナリオでのパフォーマンス最適化とスムーズなデータ送信に重要です。イングレスのみを扱うブリッジでは適用されないため`0`に設定可能です。 | `4`          |
| Request TTL              | リクエストの有効期限（秒）を指定します。リクエストがバッファに入った時点でカウント開始し、TTLを超えてバッファに滞留するか、Datalayersからの応答やアックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45`         |
| Health Check Interval    | SinkがDatalayersとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。                                                    | `15`         |
| Max Buffer Queue Size    | Datalayers Sinkの各バッファワーカープロセスがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、効率的にデータストリームを処理します。システム性能やデータ送信要件に応じて調整してください。 | `1`          |
| Batch Size               | EMQXからDatalayersへ1回の転送操作で送信するデータバッチの最大サイズを指定します。サイズ調整によりデータ転送の効率とパフォーマンスを最適化できます。`1`に設定するとバッチ化せず個別に送信します。 | `100``       |
| Query Mode               | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信を要件に応じて最適化します。非同期モードではDatalayersへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがメッセージを受信した時点でDatalayersにまだ到達していない可能性があります。 | `Asynch`     |
| Inflight Window          | 送信済みだが応答やアックをまだ受け取っていないリクエスト（インフライトキューリクエスト）の最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合に特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`        |
