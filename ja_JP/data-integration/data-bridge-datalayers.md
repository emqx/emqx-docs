# MQTTデータをDatalayersに取り込む

<<<<<<< HEAD
Datalayersは、産業用IoT、IoV、エネルギーなどの業界向けに設計されたマルチモーダルかつハイパーコンバージドなデータベースです。高いデータスループットと安定したパフォーマンスを備えており、IoTアプリケーションに最適です。EMQXは現在、Sinkを介してDatalayersにメッセージやデータを保存することをサポートしており、データの分析や可視化を容易にしています。

本ページでは、EMQXとDatalayersのデータ統合の詳細な概要を説明し、ルールとSinkの作成方法について実践的なガイドを提供します。

## 動作概要

Datalayersデータ統合はEMQXの標準機能であり、デバイスからのMQTTメッセージをシームレスにDatalayersに転送して保存・分析できます。ルールとSinkを設定することで、処理済みのMQTTデータを柔軟にDatalayersへルーティングできます。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとDatalayersの統合アーキテクチャの典型例を示しています。

![MQTT to Datalayers](./assets/mqtt-to-datalayers.jpg)

このアーキテクチャでは、EMQXがデバイスの接続管理、メッセージ転送、ルールベースの処理を担当し、Datalayersがデータの保存、分析、可視化を担います。両者が連携することで、エネルギー消費のリアルタイムデータを効率的に収集・分析できるスケーラブルなIoTプラットフォームを構築します。
=======
Datalayersは、産業用IoT、IoV、エネルギーなどの分野向けに設計されたマルチモーダル・ハイパーコンバージドデータベースです。高いデータスループットと安定したパフォーマンスを備えており、IoTアプリケーションに最適です。EMQXは現在、Sinkを通じてDatalayersにメッセージやデータを保存することをサポートしており、データ分析や可視化を容易にしています。

本ページでは、EMQXとDatalayersのデータ統合について詳しく解説し、ルールとSinkの作成方法を実践的に案内します。

## 動作概要

Datalayersデータ統合はEMQXの標準機能であり、デバイスからのMQTTメッセージをシームレスにDatalayersへ転送し、保存・分析を可能にします。ルールとSinkを設定することで、処理済みのMQTTデータを柔軟にDatalayersへルーティングできます。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとDatalayersの典型的な統合アーキテクチャを示しています。

![MQTT to Datalayers](./assets/mqtt-to-datalayers.jpg)

このアーキテクチャでは、EMQXがデバイスの接続管理、メッセージ転送、ルールベースの処理を担当し、Datalayersがデータの保存、分析、可視化を担います。両者が連携することで、エネルギー消費のリアルタイムデータを効率的に収集・分析するスケーラブルなIoTプラットフォームを構築できます。
>>>>>>> origin/release-6.1

EMQX 6.0.0以降、DatalayersはApache Arrowに基づく高性能バイナリ通信プロトコルであるArrow Flight SQLをサポートしています。従来のInfluxDB Line Protocolと比較して、Arrow Flight SQLはより効率的なデータ転送と構造化データの書き込みに強みがあります。

::: warning 注意

<<<<<<< HEAD
Arrow FlightドライバーはRustで実装され、Native Implemented Function（NIF）を介してErlang VMに統合されています。本機能は現在実験的であり、テスト環境での利用を推奨します。
=======
Arrow FlightドライバーはRustで実装され、Erlang VMにNative Implemented Function（NIF）を通じて統合されています。本機能は現在実験的であり、テスト環境での利用を推奨します。
>>>>>>> origin/release-6.1

:::

具体的なワークフローは以下の通りです。

<<<<<<< HEAD
1. **メッセージのパブリッシュと受信**  
   デバイスはMQTTでEMQXに接続し、電力、電流、電圧などのエネルギー関連メトリクスを定期的にパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジンに渡します。

2. **ルールエンジンによるメッセージ処理**  
   EMQXの組み込みルールエンジンはトピックパターンに基づいてメッセージをマッチングし、ペイロードの変換やフィールドのフィルタリング、コンテキスト情報の付加などの処理を行います。

3. **Datalayersへの書き込み**  
   ルールがトリガーされると、Sinkアクションが実行され、処理済みデータをDatalayersに書き込みます。SinkはフィールドをDatalayersのテーブルやカラムにマッピングするカスタマイズ可能なSQLテンプレートをサポートしています。
=======
1. **メッセージのパブリッシュと受信**：デバイスはMQTTでEMQXに接続し、電力、電流、電圧などのエネルギー関連メトリクスを定期的にパブリッシュします。EMQXはこれらのメッセージを受信し、ルールエンジンに渡します。

2. **ルールエンジンによるメッセージ処理**：EMQXの組み込みルールエンジンはトピックパターンに基づいて受信メッセージをマッチングし、ペイロードの変換、フィールドのフィルタリング、コンテキスト情報の付加などの処理を行います。

3. **Datalayersへの書き込み**：ルールがトリガーされると、Sinkアクションが実行され、処理済みデータをDatalayersに書き込みます。SinkはフィールドをDatalayersのテーブルやカラムにマッピングするカスタマイズ可能なSQLテンプレートをサポートします。
>>>>>>> origin/release-6.1

   EMQXは以下の2つの書き込み方式をサポートしています。

   - InfluxDB Line Protocol
   - Arrow Flight SQLドライバー

   Sinkの設定は選択した方式によって異なります。

<<<<<<< HEAD
エネルギー貯蔵データがDatalayersに書き込まれた後は、以下のような対応ツールを使って柔軟にデータ分析が可能です。

- Grafanaなどの可視化ツールに接続し、エネルギー貯蔵データのチャートを生成・表示する。
- 業務システムに接続し、エネルギー貯蔵デバイスの状態監視やアラートを実施する。

## 特長とメリット

Datalayersデータ統合は以下の特長と利点を提供します。

- **効率的なデータ処理**  
  EMQXは多数のIoTデバイス接続とメッセージスループットを処理可能であり、Datalayersはデータの書き込み、保存、クエリに優れているため、IoTシナリオのデータ処理要件をシステムに過負荷をかけずに満たせます。

- **メッセージ変換**  
  メッセージはEMQXのルール内で大規模な処理・変換が可能であり、Datalayersへの書き込み前に柔軟に加工できます。

- **スケーラビリティ**  
  EMQXとDatalayersは共にクラスター機能を備えており、ビジネスの成長に合わせて水平スケールが可能です。

- **豊富なクエリ機能**  
  Datalayersはタイムスタンプデータの効率的なクエリ・分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから価値ある洞察を抽出できます。

- **効率的なストレージ**  
  Datalayersは高圧縮エンコーディング方式を採用し、ストレージコストを大幅に削減します。また、カスタマイズ可能なデータ保持期間を設定でき、不要なデータがストレージを占有するのを防ぎます。

## はじめる前に

本節では、EMQXでDatalayers Sinkを作成する前に必要な準備として、Datalayersのインストール、データベース作成、テーブル構造の定義について説明します。
=======
エネルギー貯蔵データがDatalayersに書き込まれた後は、対応するツールを使って柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続し、エネルギー貯蔵データのチャートを作成・表示する。
- 業務システムに連携し、エネルギー貯蔵機器の状態監視やアラートを実装する。

## 特徴と利点

Datalayersデータ統合の主な特徴と利点は以下の通りです。

- **効率的なデータ処理**：EMQXは多数のIoTデバイス接続とメッセージスループットを処理可能であり、Datalayersはデータの書き込み、保存、クエリに優れているため、IoTシナリオのデータ処理要件をシステムに過負荷をかけずに満たせます。
- **メッセージ変換**：メッセージはEMQXのルールで大規模な処理・変換が可能であり、Datalayersに書き込む前に柔軟に加工できます。
- **スケーラビリティ**：EMQXとDatalayersは共にクラスタリング機能を持ち、ビジネスの成長に応じて水平スケールが可能です。
- **豊富なクエリ機能**：Datalayersはタイムスタンプデータの効率的なクエリと分析のために最適化された関数、演算子、インデックス技術を提供し、IoT時系列データから価値ある洞察を抽出できます。
- **効率的なストレージ**：Datalayersは高圧縮エンコーディングを用いてストレージコストを大幅に削減し、カスタマイズ可能なデータ保持期間を設定して不要なデータのストレージ占有を防止します。

## はじめる前に

本セクションでは、EMQXでDatalayers Sinkを作成する前に必要な準備事項を説明します。Datalayersのインストール、データベース作成、テーブル構造定義などが含まれます。
>>>>>>> origin/release-6.1

### 前提条件

- [ルール](./rules.md)の基本知識
- [データ統合](./data-bridges.md)の基本知識
<<<<<<< HEAD
- 使用するドライバータイプに応じて、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)または[Arrow Flight SQL](https://arrow.apache.org/docs/format/FlightSql.html#arrow-flight-sql)の理解
=======
- 使用するドライバーに応じて、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)または[Arrow Flight SQL](https://arrow.apache.org/docs/format/FlightSql.html#arrow-flight-sql)の理解
>>>>>>> origin/release-6.1

### Datalayersのインストールとセットアップ

1. Dockerを使ってDatalayersをインストールし起動します。詳細は[Install Datalayers](https://docs.datalayers.cn/datalayers/latest/getting-started/docker.html)を参照してください。

   ```bash
   # Datalayersコンテナを起動
   docker run -d --name datalayers -p 8360:8360 -p 8361:8361 datalayers/datalayers:latest
   ```

<<<<<<< HEAD
   - ポート`8360`はArrow Flight SQL用のデフォルトgRPCポートです。
   - ポート`8361`はHTTPポートで、主にLine Protocol書き込みや管理APIに使用されます。

2. Datalayersサービス起動後、デフォルトのユーザー名・パスワード`admin`/`public`でDatalayers CLIにログインします。以下の手順でデータベースを作成できます。
=======
   - ポート`8360`はArrow Flight SQLのgRPCデフォルトポートです。
   - ポート`8361`はHTTPポートで、主にLine Protocol書き込みや管理APIに使用されます。

2. Datalayersサービス起動後、デフォルトのユーザー名・パスワード`admin`/`public`でCLIにログインし、データベースを作成します。
>>>>>>> origin/release-6.1

   - Datalayersコンテナにアクセス：

     ```bash
     docker exec -it datalayers bash
     ```
<<<<<<< HEAD

=======
     
>>>>>>> origin/release-6.1
   - Datalayers CLIを起動：

     ```bash
     dlsql -u admin -p public
     ```
<<<<<<< HEAD

=======
     
>>>>>>> origin/release-6.1
   - データベースを作成（例：`mqtt`）：

     ```sql
     create database mqtt
     ```

<<<<<<< HEAD
4. Arrow Flight SQLドライバーを使用する場合は、対象テーブルを事前に作成する必要があります。

   ::: tip 注意

   InfluxDB Line Protocolを使用する場合は、テーブルの事前作成は不要です。Datalayersは受信したLine Protocolの`measurement`やフィールド定義に基づき自動でテーブルを作成します。

   :::

   例として、`t_mqtt_msg`というテーブルを以下のSQLで作成します。
=======
3. Arrow Flight SQLドライバーを使用する場合は、対象テーブルを事前に作成してください。

   ::: tip 注意

   InfluxDB Line Protocolを使用する場合は、テーブルの事前作成は不要です。Datalayersは受信したLine Protocolデータの`measurement`やフィールド定義に基づき自動でテーブルを作成します。

   :::

   例えば、以下のSQLで`t_mqtt_msg`テーブルを作成します。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
本節では、EMQXでSinkをDatalayersサーバーに接続するためのコネクター作成方法を説明します。

以下の手順はEMQXとDatalayersがローカルで稼働していることを前提としています。別環境やリモート環境にデプロイしている場合は接続設定を適宜更新してください。
=======
このセクションでは、EMQXでDatalayersサーバーに接続するコネクターを作成する方法を示します。

以下の手順はEMQXとDatalayersがローカルで稼働していることを前提としています。別環境やリモート環境にデプロイしている場合は、接続設定を適宜更新してください。
>>>>>>> origin/release-6.1

1. EMQXダッシュボードで、**Integration** -> **Connectors**をクリックします。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**ページで**Datalayers**を選択し、**Next**をクリックします。

<<<<<<< HEAD
5. **Configuration**ページでコネクターの詳細を入力します。

   - **Connector Name**：英数字で始まり、英数字、ハイフン、アンダースコアのみ使用可能。例：`my_datalayers`
   - **Description**（任意）：後で識別しやすいように説明を追加可能。
=======
4. **Configuration**ページでコネクターの詳細を入力します。

   - **Connector Name**：英数字で始まり、英数字、ハイフン、アンダースコアのみ使用可能。例：`my_datalayers`
   - **Description**（任意）：コネクター識別用の説明を入力
>>>>>>> origin/release-6.1

   Datalayersサーバー接続設定：

   - **Driver Type**：

<<<<<<< HEAD
     - `InfluxDB Line Protocol`：InfluxDB互換のLine Protocolを用いたデータ取り込み。テーブルは自動作成されます。

     - `Arrow Flight`：SQLテンプレートを用いた高性能な構造化データ書き込みを可能にします。スキーマ管理や高スループットが必要な場合に適しています。

       ::: warning 注意

       Arrow FlightドライバーはRustで実装され、NIFを介してErlang VMに統合されています。本機能は実験的であり、まずテスト環境で評価してください。
=======
     - `InfluxDB Line Protocol`：InfluxDB互換のLine Protocolでデータを取り込みます。テーブルは自動作成されます。

     - `Arrow Flight`：SQLテンプレートを使った高性能な構造化データ書き込みを有効化します。スキーマ管理が厳格で高スループットが必要な場合に適しています。

       ::: warning 注意

       Arrow FlightドライバーはRust実装でErlang VMにNIF経由で統合されています。現在実験的機能のため、テスト環境での評価を推奨します。
>>>>>>> origin/release-6.1

       :::

   - **Server Host**：

     - デフォルト：`127.0.0.1:8361`
<<<<<<< HEAD
     - `Arrow Flight`ドライバー使用時はgRPC通信のためポート`8360`を使用します。

   - **Database Name**：Datalayersの対象データベース名（例：`mqtt`）

   - **Username / Password**：Datalayersアクセス用の認証情報（例：`admin` / `public`）

   - **Enable TLS**（任意）：暗号化接続を有効化可能。有効時は証明書パスや検証オプションを設定できます。詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#tls-for-external-resource-access)を参照してください。

     ::: tip 注意

     Arrow Flight SQLプロトコル使用時は証明書検証をスキップできません（`verify_none`はライブラリ制約により非対応）。gRPCサーバーの証明書はサーバーホストと一致する有効なCNを持つ必要があります。

     :::

5. ドライバーに`Arrow Flight`を選択すると、**Enable Prepared Statements**オプションが追加表示されます。これはSinkがSQLテンプレートを用いたデータ挿入を行うかどうかを設定し、デフォルトで有効です。

6. **Create**をクリックする前に、**Test Connectivity**でDatalayersサーバーへの接続テストが可能です。

7. 画面下部の**Create**をクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択できます。ルールとSinkの作成手順は[Create a Datalayers Rule](#create-a-datalayers-rule)を参照してください。

## Datalayersルールの作成

本節では、EMQXでソーストピック`t/#`からMQTTメッセージを処理し、設定済みのSinkを使ってDatalayersに送信するルールの作成方法を説明します。

### SQLを定義したルールの作成

1. EMQXダッシュボードの左メニューから**Data Integration** -> **Rules**を選択します。
=======
     - `Arrow Flight`ドライバー使用時はgRPC通信でポート`8360`を使用します。

   - **Database Name**：Datalayers上の対象データベース名（例：`mqtt`）

   - **Username / Password**：Datalayersアクセス用認証情報（例：`admin` / `public`）

   - **Enable TLS**（任意）：暗号化接続を有効化する場合に切り替え。証明書パスや検証オプションを設定可能。詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#tls-for-external-resource-access)を参照。

     ::: tip 注意

     Arrow Flight SQLプロトコル使用時は証明書検証をスキップできません（`verify_none`はライブラリ制約により非対応）。gRPCサーバー証明書のCommon Name（CN）がサーバーホスト名と一致している必要があります。

     :::

5. `Arrow Flight`ドライバー選択時は、**Enable Prepared Statements**オプションが表示されます。これはSinkがSQLテンプレートを使ってデータ挿入を行うかどうかを制御し、デフォルトで有効です。

6. **Create**をクリックする前に、**Test Connectivity**を押してコネクターがDatalayersサーバーに接続できるか確認できます。

7. 画面下部の**Create**をクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択可能です。ルールとSinkの作成手順は[Create a Datalayers Rule](#create-a-datalayers-rule)を参照してください。

## Datalayersルールの作成

このセクションでは、EMQXでソーストピック`t/#`からMQTTメッセージを処理し、処理結果を設定済みのSinkを使ってDatalayersに送信するルールの作成方法を示します。

### SQLを定義したルールの作成

1. EMQXダッシュボードの左メニューから**Data Integration** -> **Rules**に移動します。
>>>>>>> origin/release-6.1

2. **Rules**ページで右上の**Create**ボタンをクリックします。

3. ルール作成フォームでルールIDを入力します（例：`my_rule`）。

4. **SQL Editor**にルールロジックを定義します。トピック`t/#`にパブリッシュされたMQTTメッセージをDatalayersに保存するには、以下のSQLを使用できます。

   ::: tip 注意

<<<<<<< HEAD
   カスタムSQLルールを書く場合、Sinkテンプレートで参照するすべての変数（例：`${clientid}`, `${payload.temp}`）はルールの`SELECT`句に含めてください。
=======
   カスタムSQLルールを書く場合、Sinkテンプレート内で参照するすべての変数（例：`${clientid}`, `${payload.temp}`）がルールの`SELECT`句に含まれていることを確認してください。
>>>>>>> origin/release-6.1

   :::

   ```
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

<<<<<<< HEAD
   EMQXのSQLに不慣れな場合は、**SQL Examples**や**Enable Debug**を使ってサンプルクエリを試し、出力を確認できます。

   :::

5. ルールにDatalayers Sinkを追加し、処理結果をDatalayersに書き込みます。

   - **InfluxDB Line Protocol**を使う場合は[Add an InfluxDB Line Protocol Sink](#add-an-influxdb-line-protocol-sink)を参照してください。
   - **Arrow Flight SQLドライバー**を使う場合は[Add an Arrow Flight SQL Sink](#add-an-arrow-flight-sql-sink)を参照してください。

6. **Create Rule**ページで設定内容を確認し、**Save**をクリックしてルールを作成します。

作成したルールは**Rules**一覧に表示されます。ルールの**Actions (Sink)**タブをクリックすると、このルールに紐づくDatalayers Sinkを確認できます。

また、**Integrations** -> **Flow Designer**でトポロジーグラフを表示すると、トピック`t/#`のメッセージが`my_rule`ルールで処理されDatalayersに書き込まれる様子が視覚的に確認できます。

### InfluxDB Line Protocol Sinkの追加

本節では、InfluxDB Line Protocolを使って処理済みデータをDatalayersに書き込むSinkの追加方法を説明します。

1. ルールエディタ右側の**Add Action**ボタンをクリックし、ルール条件に合致した際にトリガーされるアクションを定義します。このアクションで処理済みメッセージをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のDatalayers Sinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sinkの名前を入力します（例：`dl_sink_influx`）。英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンで、`InfluxDB Line Protocol`ドライバーを設定した既存のコネクターを選択します。コネクターがない場合は隣のボタンから新規作成してください。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照。

5. **Time Precision**はデフォルトでミリ秒に設定します。

6. Datalayersにデータを書き込むための**Data Format**と内容を定義します。`JSON`または`Line Protocol`を選択可能です。

   - **JSON**：

     **Measurement**、**Fields**、**Timestamp**、**Tags**を指定します。キーと値は定数または`${payload.temp}`のような変数プレースホルダーをサポートします。フォーマットルールは[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     **Fields**はCSVファイルを使った一括設定も可能です。[Use CSV to Batch Configure Fields](#use-csv-to-batch-configure-fields)を参照。

   - **Line Protocol**：

     テーブル、フィールド、タイムスタンプ、タグを含む単一のLine Protocol文字列を記述します。キーと値は定数またはプレースホルダーをサポートします。構文は[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     ::: tip

     Datalayersに書き込むデータはInfluxDB v1のLine Protocolと完全互換のため、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)も参考にできます。

     例として、符号付き整数値を入力する場合はプレースホルダーの後に`i`を付けます（例：`${payload.int}i`）。詳細は[InfluxDB 1.8で整数値を書き込む方法](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照。
=======
   EMQXのSQLに慣れていない場合は、**SQL Examples**や**Enable Debug**をクリックしてサンプルクエリを試し、出力を確認できます。

   :::

5. 処理結果をDatalayersに書き込むため、ルールにDatalayers Sinkを追加します。

   - **InfluxDB Line Protocol**を使用する場合は、[Add an InfluxDB Line Protocol Sink](#add-an-influxdb-line-protocol-sink)を参照してください。
   - **Arrow Flight SQLドライバー**を使用する場合は、[Add an Arrow Flight SQL Sink](#add-an-arrow-flight-sql-sink)を参照してください。

6. **Create Rule**ページで設定を確認し、**Save**をクリックしてルールを作成します。

作成したルールは**Rules**一覧に表示されます。ルールの**Actions (Sink)**タブをクリックすると、このルールに紐づくDatalayers Sinkを確認できます。

また、**Integrations** -> **Flow Designer**でトポロジーグラフを表示できます。ここではトピック`t/#`のメッセージが`my_rule`ルールで処理され、Datalayersに書き込まれる様子が可視化されます。

### InfluxDB Line Protocol Sinkの追加

このセクションでは、InfluxDB Line Protocolを使って処理済みデータをDatalayersに書き込むSinkをルールに追加する方法を示します。

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルール条件にマッチした際にトリガーされるアクションを定義します。このアクションは処理済みメッセージをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のDatalayers Sinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sinkの名前を入力します（例：`dl_sink_influx`）。名前は英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンから、`InfluxDB Line Protocol`ドライバーで設定済みのコネクターを選択します。コネクターがない場合は隣のボタンから作成可能です。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照してください。

5. **Time Precision**はデフォルトでミリ秒に設定します。

6. Datalayersへのデータ解析・書き込み用に**Data Format**と内容を定義します。`JSON`または`Line Protocol`から選択可能です。

   - **JSON**：

     **Measurement**、**Fields**、**Timestamp**、**Tags**を指定する必要があります。キーと値は定数またはプレースホルダー（例：`${payload.temp}`）をサポートします。書式は[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     **Fields**はCSVファイルを使った一括設定も可能です。[Use CSV to Batch Configure Fields](#use-csv-to-batch-configure-fields)を参照してください。

   - **Line Protocol**：

     テーブル、フィールド、タイムスタンプ、タグを含む単一のLine Protocol文字列を定義できます。キーと値は定数またはプレースホルダーをサポートします。書式は[InfluxDB Line Protocol](https://docs.datalayers.cn/datalayers/latest/development-guide/writing-with-influxdb-line-protocol.html)を参照してください。

     ::: tip

     Datalayersへの書き込みはInfluxDB v1のLine Protocolと完全互換のため、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/)を参照して設定できます。

     例えば、符号付き整数値を入力する場合は、プレースホルダーの後に`i`を付けます（例：`${payload.int}i`）。詳細は[InfluxDB 1.8で整数値を書き込む方法](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
>>>>>>> origin/release-6.1

     :::

     Line Protocolの例：

     ```sql
     devices,clientid=${clientid} temp=${payload.temp},hum=${payload.hum},precip=${payload.precip}i ${timestamp}
     ```

<<<<<<< HEAD
7. **Fallback Actions**（任意）：メッセージ配信失敗時の信頼性向上のため、プライマリSinkが失敗した場合にトリガーされるフォールバックアクションを1つ以上定義できます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

8. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照してください。

9. **Create**をクリックする前に、**Test Connectivity**でSinkがDatalayersサーバーに接続可能かテストできます。

10. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新規Sinkが表示されます。
=======
7. **Fallback Actions**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

8. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。詳細は[Advanced Settings](#advanced-settings)を参照してください。

9. **Create**をクリックする前に、**Test Connectivity**でSinkがDatalayersサーバーに接続できるかテスト可能です。

10. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。
>>>>>>> origin/release-6.1

#### CSVを使ったフィールドの一括設定

::: tip

<<<<<<< HEAD
この機能は**InfluxDB Line Protocol**ドライバーでデータフォーマットが`JSON`の場合のみ利用可能です。フィールド設定を一括でインポートできます。

:::

Datalayersのデータエントリは数百のフィールドを含むことが多く、データフォーマット設定が煩雑になりがちです。これを解決するため、EMQXはフィールドの一括設定機能を提供しています。

JSONフォーマット設定時に、CSVファイルからフィールドのキー・バリューを一括インポートできます。

1. **Fields**テーブルの**Batch Settings**ボタンをクリックし、**Import Batch Settings**ポップアップを開きます。

2. 指示に従い一括設定テンプレートファイルをダウンロードし、フィールドのキー・バリューを記入します。テンプレートのデフォルト内容は以下の通りです。
=======
この機能は**InfluxDB Line Protocol**のSinkで、データフォーマットが`JSON`の場合にのみ利用可能です。フィールド設定をCSVで一括インポートできます。

:::

Datalayersのデータエントリは数百のフィールドを含むことが多く、データフォーマット設定が複雑になる場合があります。これを解決するため、EMQXはフィールドの一括設定機能を提供しています。

JSONフォーマット設定時に、CSVファイルからフィールドのキー・値ペアを一括インポートできます。

1. **Fields**テーブルの**Batch Settings**ボタンをクリックし、**Import Batch Settings**ポップアップを開きます。

2. 指示に従いテンプレートファイルをダウンロードし、フィールドのキー・値ペアを入力します。テンプレートのデフォルト内容は以下の通りです。
>>>>>>> origin/release-6.1

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
<<<<<<< HEAD
   | precip | ${payload.precip}i | フィールド値の後に`i`を付けるとDatalayersは整数型として保存 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポート。Line Protocolに従い型識別子を付加可能。
   - **備考**：CSV内のコメント用で、EMQXへのインポート対象外。
=======
   | precip | ${payload.precip}i | 値の後に`i`を付けるとDatalayersは整数型として保存します。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、Line Protocolに準じた型識別子の付加も可能。
   - **備考**：CSV内のコメント用で、EMQXにはインポートされません。
>>>>>>> origin/release-6.1

   一括設定CSVファイルは2048行を超えないようにしてください。

<<<<<<< HEAD
3. 記入済みテンプレートファイルを保存し、**Import Batch Settings**ポップアップにアップロード後、**Import**をクリックして一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・バリューをさらに調整可能です。

### Arrow Flight SQL Sinkの追加

本節では、**Arrow Flight SQL**ドライバーを使い、SQL挿入文でDatalayersにデータを書き込むSinkの追加方法を説明します。

::: warning 注意

Arrow Flight SQLドライバーは現在実験的です。商用環境での利用は慎重に行ってください。

:::

1. ルールエディタ右側の**Add Action**ボタンをクリックし、ルールマッチ時にトリガーされるアクションを定義します。このアクションで処理済みデータをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存Sinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sinkの名前を入力します（例：`dl_sink_arrow`）。英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンで、`Arrow Flight`ドライバーを設定した既存コネクターを選択します。存在しない場合は隣のボタンから新規作成してください。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照。

5. データをターゲットテーブルに挿入する方法を定義する**SQL**テンプレートを設定します。

   ::: tip

   これは[プリプロセスSQL](./data-bridges.md#prepared-statement)テンプレートです。フィールド名をクォートで囲まず、SQL文の末尾にセミコロン`;`を含めないでください。  
   `${}`プレースホルダーはルールSQLで選択したフィールドと一致させる必要があります。
=======
3. 入力したテンプレートファイルを保存し、**Import Batch Settings**ポップアップでアップロード後、**Import**をクリックして一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・値ペアをさらに調整可能です。

### Arrow Flight SQL Sinkの追加

このセクションでは、**Arrow Flight SQL**ドライバーを使い、SQL挿入文でDatalayersにデータを書き込むSinkをルールに追加する方法を示します。

::: warning 注意

Arrow Flight SQLドライバーは現在実験的機能です。商用環境での利用は慎重に行ってください。

:::

1. ルールエディター右側の**Add Action**ボタンをクリックし、ルールマッチ時にトリガーされるアクションを定義します。このアクションは処理済みデータをDatalayersに転送します。

2. **Type of Action**ドロップダウンで`Datalayers`を選択し、**Action**はデフォルトの`Create Action`のままにします。既存のSinkを選択することも可能ですが、本例では新規作成を想定しています。

3. Sinkの名前を入力します（例：`dl_sink_arrow`）。英数字の組み合わせが推奨されます。

4. **Connector**ドロップダウンから、`Arrow Flight`ドライバーで設定済みのコネクターを選択します。存在しない場合は隣のボタンから作成可能です。[Create a Datalayers Connector](#create-a-datalayers-connector)を参照してください。

5. データを対象テーブルに挿入するSQLテンプレートを設定します。

   ::: tip

   これは[プリプロセッシングSQL](./data-bridges.md#prepared-statement)テンプレートです。フィールド名は引用符で囲まず、SQL文の末尾にセミコロン`;`を付けないでください。すべての`${}`プレースホルダーはルールSQLで選択したフィールドと一致させる必要があります。
>>>>>>> origin/release-6.1

   :::

   ::: tip

<<<<<<< HEAD
   コネクターで設定したデータベース以外にデータを挿入する場合は、SQLテンプレート内でターゲットデータベース名を明示的に指定してください。  
   ただし、コネクターはターゲットデータベースの存在をチェックします。
=======
   コネクターで設定したデータベース以外にデータを挿入する場合は、SQLテンプレート内で対象データベース名を明示的に指定してください。ただし、コネクターは対象データベースの存在をチェックします。
>>>>>>> origin/release-6.1

   :::

   例：

   ```sql
   insert into t_mqtt_msg(time, msgid, sender, topic, qos, payload, arrived) values (${timestamp}, ${id}, ${clientid}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```

<<<<<<< HEAD
6. **Fallback Actions**（任意）：信頼性向上のため、Sinkがメッセージ処理に失敗した場合にトリガーされるフォールバックアクションを1つ以上設定可能です。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

7. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connection**でSinkがDatalayersサーバーに接続可能か検証できます。

9. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新規Sinkが表示されます。

## ルールとSinkのテスト

ルールとSinkの設定後、テスト用MQTTメッセージをパブリッシュしてDatalayersへの書き込みが成功しているか確認できます。

1. [MQTTX](https://mqttx.app/)を使い、トピック`t/1`にメッセージを送信します。これによりセッションイベント（クライアントのオンライン/オフラインなど）もトリガーされる場合があります。
=======
6. **Fallback Actions**（任意）：信頼性向上のため、1つ以上のフォールバックアクションを設定可能です。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

7. **Advanced Settings**を展開し、必要に応じて詳細設定を行います。[Advanced Settings](#advanced-settings)を参照してください。

8. **Create**をクリックする前に、**Test Connection**でSinkがDatalayersサーバーに接続できるか確認可能です。

9. **Create**をクリックしてSink作成を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

## ルールとSinkのテスト

ルールとSinkを設定後、テスト用のMQTTメッセージをパブリッシュしてDatalayersへの書き込みが成功するか検証できます。

1. [MQTTX](https://mqttx.app/)を使い、トピック`t/1`にメッセージを送信します。セッションイベント（クライアントのオンライン/オフラインなど）もトリガーされる場合があります。
>>>>>>> origin/release-6.1

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "temp": "23.5", "hum": "62", "precip": 2 }'
   ```

<<<<<<< HEAD
   このメッセージはルールエンジンをトリガーし、設定済みのDatalayers Sinkに転送されます。ルールにクライアント接続・切断などのセッションイベントが含まれている場合も同時に処理されます。

2. Sinkの実行統計を確認します。EMQXダッシュボードの**Rules**ページで該当ルールを選択し、**Actions (Sink)**タブに切り替えます。対象Sinkの**Matched**および**Success**カウントが1増加していることを確認してください。

3. CLIでDatalayersのデータを検証します。
=======
   このメッセージによりルールエンジンが起動し、設定済みのDatalayers Sinkへ転送されます。ルールにクライアント接続や切断などのセッションイベントが含まれている場合、それらも同時にトリガーされる可能性があります。

2. Sinkの実行統計を確認します。EMQXダッシュボードの**Rules**ページで対象ルールを選択し、**Actions (Sink)**タブに切り替えます。対象Sinkの**Matched**および**Success**カウントが1増加していることを確認してください。

3. CLIでDatalayers内のデータを確認します。
>>>>>>> origin/release-6.1

   Datalayersコンテナにアクセスし、CLIツールを起動します。

   ```bash
   docker exec -it datalayers bash
   dlsql -u admin -p public
   ```

   使用した書き込み方式に応じてSQLクエリを実行します。

   - InfluxDB Line Protocolの場合、テーブル名はSink設定の`measurement`（例：`devices`）がデフォルトです。

     ```sql
     use mqtt
     select * from devices
     ```

<<<<<<< HEAD
   - Arrow Flight SQLの場合、事前に作成したターゲットテーブル（例：`t_mqtt_msg`）をクエリします。
=======
   - Arrow Flight SQLの場合は、事前に作成した対象テーブル（例：`t_mqtt_msg`）をクエリします。
>>>>>>> origin/release-6.1

     ```sql
     use mqtt
     select * from t_mqtt_msg
     ```

## 詳細設定

<<<<<<< HEAD
本節では、DatalayersコネクターおよびSinkの詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際に、**Advanced Settings**を展開して以下のパラメーターを用途に応じて調整できます。

| フィールド名             | 説明                                                                                                                             | デフォルト値 |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------- | ------------ |
| Buffer Pool Size        | バッファワーカープロセスの数を指定します。これらのプロセスはEMQXとDatalayersのエグレス型Sink間のデータフローを管理し、データを一時的に保存・処理してターゲットサービスに送信します。エグレスシナリオでのパフォーマンス最適化とスムーズなデータ転送に重要です。イングレスのみを扱うブリッジでは`0`に設定可能です。 | `4`          |
| Request TTL             | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがこのTTLを超えてバッファに滞留するか、Datalayersからの応答・アックを受け取れない場合、リクエストは期限切れとみなされます。 | `45`         |
| Health Check Interval   | SinkがDatalayersとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。                                                     | `15`         |
| Max Buffer Queue Size   | Datalayers Sinkの各バッファワーカープロセスがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、効率的にデータストリームを処理します。システム性能やデータ転送要件に応じて調整してください。 | `1`          |
| Batch Size              | EMQXからDatalayersへ一度に転送するデータバッチの最大サイズを指定します。この値を調整することでデータ転送の効率とパフォーマンスを最適化できます。`1`に設定するとバッチ化せず個別に送信します。 | `100`        |
| Query Mode              | `synchronous`（同期）または`asynchronous`（非同期）のリクエストモードを選択し、メッセージ送信を要件に応じて最適化します。非同期モードではDatalayersへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがDatalayersに到達する前にメッセージを受信する可能性があります。 | `Asynch`     |
| Inflight Window         | 送信済みだが応答・アックをまだ受け取っていないリクエストの最大数を制御します。`Request Mode`が`asynchronous`の場合に特に重要で、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`        |
=======
本セクションでは、DatalayersコネクターおよびSinkで利用可能な詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**を展開して以下のパラメータを用途に応じて調整できます。

| フィールド名               | 説明                                                                                                                             | デフォルト値 |
| -------------------------- | -------------------------------------------------------------------------------------------------------------------------------- | ------------ |
| Buffer Pool Size           | バッファワーカープロセスの数を指定します。これらのプロセスはEMQXとDatalayersのエグレス型Sink間のデータフローを管理し、データを一時的に保存・処理してからターゲットサービスに送信します。エグレスシナリオのパフォーマンス最適化とスムーズなデータ送信に重要です。イングレスのみを扱うブリッジでは`0`に設定可能です。 | `4`          |
| Request TTL                | バッファに入ったリクエストが有効とみなされる最大時間（秒）を指定します。リクエストがTTLを超えてバッファ内に滞留するか、Datalayersからの応答やアックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45`         |
| Health Check Interval      | SinkがDatalayersとの接続状態を自動的にヘルスチェックする間隔（秒）を指定します。                                              | `15`         |
| Max Buffer Queue Size      | Datalayers Sinkの各バッファワーカープロセスがバッファリング可能な最大バイト数を指定します。バッファワーカーはデータを一時保存し、効率的にデータストリームを処理します。システム性能やデータ送信要件に応じて調整してください。 | `1`          |
| Batch Size                 | EMQXからDatalayersへ一度の転送操作で送信するデータバッチの最大サイズを指定します。この値を調整することでデータ転送の効率とパフォーマンスを最適化できます。<br />`1`に設定すると、データはバッチ化せず個別に送信されます。 | `100`        |
| Query Mode                 | `synchronous`または`asynchronous`のリクエストモードを選択し、メッセージ送信を要件に応じて最適化します。非同期モードではDatalayersへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがメッセージ到着前に受信する可能性があります。 | `Asynch`     |
| Inflight Window            | 送信済みだが応答やアックをまだ受け取っていないリクエスト（インフライトキューリクエスト）の最大数を制御します。<br/>`Request Mode`が`asynchronous`の場合に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合はこの値を`1`に設定してください。 | `100`        |
>>>>>>> origin/release-6.1
