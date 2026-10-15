# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/) は時系列データの保存と分析に特化したデータベースです。高いデータスループット能力と安定したパフォーマンスにより、IoT（モノのインターネット）分野での適用に非常に適しています。EMQXは現在、主流のInfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの各バージョンとの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ統合について、実践的な手順を交えて包括的に解説します。

## 動作の仕組み

InfluxDBデータ統合はEMQXの標準機能であり、EMQXのリアルタイムデータキャプチャと送信機能をInfluxDBのデータ保存・分析機能と組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、EMQXからInfluxDBへのデータ取り込みが簡素化され、複雑なコーディングが不要になります。EMQXはルールエンジンとSinkを介してデバイスデータをInfluxDBへ転送し保存・分析を行います。InfluxDBは分析結果をレポートやチャートとして生成し、InfluxDBの可視化ツールを通じてユーザーに提供します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとInfluxDB間の典型的なデータ統合アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBは、エネルギー消費データをリアルタイムに効率的に収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがIoTプラットフォームとしてデバイスの接続管理、メッセージ送受信、データルーティングを担当し、InfluxDBがデータ保存・分析プラットフォームとして機能します。ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：エネルギー貯蔵機器や産業用IoT機器がMQTTプロトコルを用いてEMQXに正常に接続し、電力消費量や入出力電力などのデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：組み込みのルールエンジンを使い、特定の送信元からのメッセージをトピックマッチングに基づいて処理します。メッセージが到着するとルールエンジンを通過し、対応するルールにマッチングされ、データ形式の変換や特定情報のフィルタリング、文脈情報の付加などの処理が行われます。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、InfluxDBへの書き込み操作が実行されます。InfluxDB SinkはLine Protocolテンプレートを提供し、メッセージの特定フィールドをInfluxDBの対応するメジャメントやフィールドに柔軟にマッピングして書き込みが可能です。

エネルギー消費データがInfluxDBに書き込まれた後は、Line Protocolを活用して柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続し、エネルギー貯蔵データを基にチャートを生成する。
- 業務システムに接続し、エネルギー貯蔵機器の状態監視やアラートを行う。

## 特長と利点

InfluxDBデータ統合は以下の特長と利点を備えています。

- **効率的なデータ処理**：EMQXは膨大なIoTデバイス接続とメッセージスループットを処理可能であり、InfluxDBはデータ書き込み・保存・クエリに優れた性能を発揮します。これにより、IoTシナリオのデータ処理要件をシステムに過度な負荷をかけずに満たせます。
- **メッセージ変換**：EMQXのルールを通じて、InfluxDBに書き込む前にメッセージの高度な処理や変換が可能です。
- **スケーラビリティ**：EMQXとInfluxDBの両方がクラスター拡張に対応しており、ビジネスの成長に応じて柔軟に水平拡張できます。
- **豊富なクエリ機能**：InfluxDBは最適化された関数、演算子、インデックス技術を提供し、時系列データの効率的なクエリと分析を可能にし、IoTデータから価値ある洞察を抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコード方式を採用し、ストレージコストを大幅に削減します。また、データ種別ごとに保存期間をカスタマイズ可能で、不要なデータによるストレージ占有を防止します。

## はじめる前に

このセクションでは、InfluxDBデータ統合を作成する前に必要な準備、特にInfluxDBのインストールとセットアップについて説明します。

### 前提条件

- EMQXがInfluxDBにデータを書き込む際に従う[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### InfluxDBのインストールとセットアップ

1. Docker経由で[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDBのDockerイメージを起動
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、[http://localhost:8086](http://localhost:8086) にアクセスし、**ユーザー名**、**パスワード**、**組織名**、**バケット名**を設定します。
3. InfluxDBのUIで **Load Data** -> **API Token** をクリックし、指示に従って[全権限トークンを作成](https://docs.influxdata.com/influxdb/v2/install/#create-all-access-tokens)します。

## コネクターの作成

このセクションでは、SinkをInfluxDBサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとInfluxDBの両方をローカルマシンで実行していることを前提としています。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。

2. 画面右上の **Create** をクリックします。

3. **Create Connector** ページで **InfluxDB** を選択し、**Next** をクリックします。

4. **Configuration** ステップで以下の情報を設定します。

   以下の設定はすべてのInfluxDBバージョン共通です。

   - **Connector Name**：コネクターの一意な名前。英数字のみで構成し、例：`my_influxdb`
   - **Description**（任意）：コネクターの簡単な説明
   - **Server Host**：InfluxDBサーバーのアドレス。例：`127.0.0.1:8086`。InfluxDB Cloudの場合はポート`443`（例：`{url}:443`）を指定し、TLSを有効にします。
   - **Version of InfluxDB**：使用するInfluxDBのバージョンを選択。`v1`、`v2`（デフォルト）、`v3`がサポートされています。
   - **Enable TLS**：InfluxDBサーバーがTLS接続を要求する場合は有効にします。詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#enabling-tls-for-external-resource-access)を参照してください。

   選択したInfluxDBバージョンにより必要な設定項目が異なります。以下の表を参照し、[InfluxDBのインストールとセットアップ](#install-and-set-up-influxdb)での設定と一致させてください。

   | 設定項目              | InfluxDB v1           | InfluxDB v2 | InfluxDB v3 |
   | --------------------- | --------------------- | ----------- | ----------- |
   | 認証方式              | ユーザー名 / パスワード | トークン     | トークン     |
   | **トークン**           | -                     | 必須        | 必須        |
   | **ユーザー名**         | 任意                  | -           | -           |
   | **パスワード**         | 任意                  | -           | -           |
   | **組織名**             | -                     | 必須        | -           |
   | **バケット**           | -                     | 必須        | -           |
   | **データベース名**     | 必須                  | -           | 必須        |

   補足：

   - **InfluxDB v1**では、EMQXは指定したデータベースに直接書き込み、ユーザー名/パスワード認証は任意です。
   - **InfluxDB v2**では、組織名とバケットモデルを使用し、トークンは指定バケットへの書き込み権限を持つ必要があります。
   - **InfluxDB v3**では、v1に似たデータベースベースのモデルを採用しつつ、トークン認証を利用します。

5. **Create** をクリックする前に、**Test Connectivity** を押してEMQXがInfluxDBサーバーに正常に接続できるか確認できます。

6. **Create** をクリックしてコネクター作成を完了します。

コネクター作成後、**Back to Connector List** を選ぶか、続けて **Create Rule** をクリックし、MQTTデータをInfluxDBに転送するルールとSinkを定義できます。詳細は[InfluxDB Sinkを使ったルール作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sinkを使ったルール作成

このセクションでは、EMQXでソースMQTTトピック `t/#` からのメッセージを処理し、設定済みのSinkを通じてInfluxDBに送信するルールの作成方法を説明します。

1. EMQXダッシュボードで左メニューから **Integration** -> **Rules** をクリックします。

2. 画面右上の **Create** をクリックします。

3. ルール作成ページで、ルールIDに `my_rule` と入力します。

4. **SQL Editor** でルールを設定します。例えば、トピック `t/#` のMQTTメッセージをInfluxDBに保存したい場合、以下のSQL文を使用します。

   ::: tip

   独自のSQL文を指定する場合は、後で設定するSinkのデータ形式に含まれるすべての変数が `SELECT` 部分に含まれていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   補足：初心者の方は **SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストを行うことができます。

5. + **Add Action** ボタンをクリックし、ルールがトリガーするアクションを定義します。このアクションにより、EMQXはルールで処理したデータをInfluxDBに送信します。

6. **Type of Action** ドロップダウンから `InfluxDB` を選択します。**Action** はデフォルトの `Create Action` のままにします。既に作成済みのSinkがあれば選択可能ですが、この例では新規Sinkを作成します。

7. Sinkの名前を入力します。名前は英数字の大小文字を組み合わせてください。

8. **Connector** ドロップダウンから先ほど作成した `my_influxdb` を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

9. **Time Precision** を指定します。デフォルトは `millisecond` です。

10. **Data Format** を `JSON` または `Line Protocol` から選択し、InfluxDBへのデータ解析・書き込み方法を指定します。

    - JSON形式の場合、**Measurement**、**Timestamp**、**Fields**、**Tags**などのデータ解析方法を定義します。すべてのキー値は変数やプレースホルダーに対応し、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に準拠して設定可能です。**Fields** はCSVファイルによる一括設定もサポートしています。詳細は[一括設定](#batch-setting)を参照してください。
    - Line Protocol形式の場合、InfluxDB Line Protocolの構文に従い、メジャメント、タグセット、フィールドセット、タイムスタンプをテキスト形式で指定します。プレースホルダーも利用可能です。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型の値を書き込む場合は、プレースホルダーの後に `i` を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8 整数値の書き込み](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型の値を書き込む場合は、プレースホルダーの後に `u` を付けます。例：`${payload.int}u`。詳細は同上リンクを参照してください。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にこれらがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：詳細設定については[詳細設定](#advanced-configurations)を参照してください。

13. **Create** をクリックする前に、**Test Connectivity** を押してSinkがInfluxDBサーバーに接続できるかテスト可能です。

14. **Create** をクリックしてSink作成を完了します。ルール作成ページの **Action Outputs** タブに新しいSinkが表示されます。

15. ルール作成ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新規InfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックしてトポロジーを確認できます。トピック `t/#` のメッセージがルール `my_rule` によって解析され、InfluxDBに送信・保存されていることがわかります。

### 一括設定

InfluxDBのデータエントリーは通常数百のフィールドを含むため、データ形式の設定は複雑になりがちです。これを解決するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータ形式を設定する際、CSVファイルからフィールドのキー・バリューのペアを一括インポートできます。

1. **Fields** テーブルの **Batch Setting** ボタンをクリックし、**Import Batch Setting** ポップアップを開きます。

2. 指示に従い、一括設定テンプレートファイルをダウンロードし、テンプレート内にフィールドのキー・バリューを記入します。テンプレートのデフォルト内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | 値の後に `i` を付けてInfluxDBに整数として保存することを示す。 |

   - **Field**：フィールドキー。定数または `${var}` 形式のプレースホルダーが使用可能。
   - **Value**：フィールド値。定数またはプレースホルダー。Line Protocolに従い型識別子を付加可能。
   - **備考**：CSV内のメモ用で、EMQXにはインポートされません。

   CSVファイルの一括設定データは2048行を超えないようにしてください。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting** ポップアップにアップロード後、**Import** をクリックして一括設定を完了します。

4. インポート後、**Fields** 設定テーブルでキー・バリューの調整が可能です。

## ルールのテスト

MQTTXを使い、トピック `t/1` にメッセージを送信してオンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

InfluxDBのUIでは、**Data Explorer** ウィンドウからメッセージがInfluxDBに書き込まれていることを確認できます。

## 詳細設定

このセクションでは、InfluxDBコネクターとSinkの詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings** に移動し、以下のパラメーターをニーズに合わせて調整できます。

| **項目**               | **説明**                                                                                                               | **推奨値** |
| ---------------------- | ---------------------------------------------------------------------------------------------------------------------- | ---------- |
| Start Timeout          | コネクターが自動起動したリソースの正常状態を待機する最大秒数です。InfluxDBのデータベースインスタンスなどが完全に稼働し、データ処理可能になるまでの待機時間を設定します。 | `5`        |
| Buffer Pool Size       | EMQXとInfluxDB間の送信（egress）タイプのブリッジでデータフローを管理するバッファワーカープロセスの数を指定します。これらのワーカーはデータ送信前の一時保存と処理を担当します。Ingress（受信）専用のSinkでは無効で、`0`に設定可能です。 | `4`        |
| Request TTL            | バッファに入ったリクエストの有効期間（秒）を指定します。TTLを超えてバッファに滞留するか、InfluxDBからの応答やアックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45`       |
| Health Check Interval  | SinkがInfluxDBへの接続状態を自動的にヘルスチェックする間隔（秒）です。                                             | `15`       |
| Max Buffer Queue Size  | 各バッファワーカーがInfluxDB Sinkでバッファリング可能な最大バイト数を指定します。バッファワーカーはデータ送信前の一時保存を担い、システム性能やデータ転送要件に応じて調整してください。 | `1`        |
| Max Batch Size         | EMQXからInfluxDBへ一度に転送可能なデータバッチの最大サイズを指定します。サイズ調整によりデータ転送の効率と性能を最適化できます。`1`に設定すると、データはバッチ化せず個別に送信されます。 | `100`      |
| Query Mode             | メッセージ送信の最適化のため、`asynchronous`（非同期）または`synchronous`（同期）モードを選択します。非同期モードではInfluxDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがメッセージをInfluxDB到着前に受信する可能性があります。 | `Async`    |
| Inflight Window        | 「インフライトクエリ」とは開始済みで応答やアックをまだ受け取っていないクエリのことです。この設定はSinkがInfluxDBと通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode** が `async` の場合、このパラメーターは特に重要です。同一MQTTクライアントからのメッセージを厳密な順序で処理したい場合は `1` に設定してください。 | `100`      |

## さらに詳しく

以下のリンクから詳細情報を確認できます。

**ブログ**：

[1時間で構築するEMQX + InfluxDB + GrafanaによるIoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[時系列IoTアプリケーションのためのMQTTデータのInfluxDB統合](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
