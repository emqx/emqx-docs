# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/)は時系列データの保存と分析に特化したデータベースです。高いデータスループット性能と安定した動作により、IoT分野での活用に非常に適しています。EMQXは現在、InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの主要バージョンとの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ連携について、作成方法や検証手順を含めて包括的に解説します。

## 動作の仕組み

InfluxDBデータ連携はEMQXに標準搭載された機能であり、EMQXのリアルタイムデータ取得・転送機能とInfluxDBのデータ保存・分析機能を組み合わせています。内蔵の[ルールエンジン](./rules.md)コンポーネントにより、EMQXからInfluxDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。EMQXはルールエンジンとSinkを通じてデバイスデータをInfluxDBに転送し保存・分析を行います。InfluxDBは分析結果をレポートやグラフとして生成し、InfluxDBの可視化ツールでユーザーに提供します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとInfluxDBの典型的なデータ連携アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBは、エネルギー消費データをリアルタイムで効率的に収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがデバイス接続、メッセージ転送、データルーティングを担当するIoTプラットフォームとして機能し、InfluxDBがデータ保存・分析プラットフォームとして役割を担います。ワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：エネルギー貯蔵機器や産業用IoT機器はMQTTプロトコルを用いてEMQXに接続し、電力消費量、入出力電力などのデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：内蔵のルールエンジンを使い、特定のトピックに基づいてメッセージを処理します。メッセージが到着するとルールエンジンを通過し、対応するルールにマッチしてデータ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義されたルールにより、InfluxDBへの書き込み操作がトリガーされます。InfluxDB SinkはLine Protocolテンプレートを提供し、メッセージの特定フィールドをInfluxDBの対応するmeasurementやfieldに柔軟にマッピングできます。

エネルギー消費データがInfluxDBに書き込まれた後は、Line Protocolを活用して以下のような分析が可能です：

- Grafanaなどの可視化ツールに接続し、エネルギー貯蔵データを基にグラフを生成する。
- 業務システムと連携してエネルギー貯蔵機器の状態監視やアラートを実施する。

## 特長とメリット

InfluxDBデータ連携は以下の特長と利点を提供します：

- **効率的なデータ処理**：EMQXは大量のIoTデバイス接続とメッセージスループットを処理可能であり、InfluxDBはデータ書き込み・保存・クエリに優れた性能を発揮し、IoTシナリオのデータ処理要件をシステムに過負荷をかけずに満たします。
- **メッセージ変換**：EMQXのルールを通じてメッセージを多様に処理・変換してからInfluxDBに書き込めます。
- **スケーラビリティ**：EMQXとInfluxDBはどちらもクラスター拡張に対応し、ビジネスの成長に応じて柔軟に水平拡張可能です。
- **豊富なクエリ機能**：InfluxDBは最適化された関数、演算子、インデックス技術を備え、時系列データの効率的なクエリと分析を実現し、IoTデータから価値ある洞察を正確に抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコード方式を採用し、ストレージコストを大幅に削減します。また、データタイプごとに保存期間をカスタマイズでき、不要なデータによるストレージ占有を防止します。

## はじめる前に

このセクションでは、InfluxDBデータ連携を作成する前に必要な準備、特にInfluxDBのインストールとセットアップについて説明します。

### 前提条件

- EMQXがInfluxDBにデータを書き込む際に従う[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXデータ連携の[ルール](./rules.md)の知識
- [データ連携](./data-bridges.md)の知識

### InfluxDBのインストールとセットアップ

1. Dockerで[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDBのDockerイメージを起動
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、ブラウザで [http://localhost:8086](http://localhost:8086) にアクセスし、**Username**、**Password**、**Organization Name**、**Bucket Name**を設定します。
3. InfluxDBのUIで、**Load Data** -> **API Token** をクリックし、指示に従って[全権限トークンを作成](https://docs.influxdata.com/influxdb/v2.5/install/#create-all-access-tokens)します。

## コネクターの作成

このセクションでは、SinkをInfluxDBサーバーに接続するためのコネクター作成方法を説明します。

以下の手順はEMQXとInfluxDBをローカルマシンで実行している場合を想定しています。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**InfluxDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下を設定します：
   - コネクター名を入力します。英数字の組み合わせで、例：`my_influxdb`
   - **Version of InfluxDB**を必要に応じて選択（デフォルトは`V2`）
   - InfluxDBサーバー接続情報を入力：
     - **Server Host**に`127.0.0.1:8086`を入力。InfluxDB Cloudを使う場合はポート443を指定し、`{url}:443`と入力して**Enable TLS**を有効にします。
     - [InfluxDBのインストールとセットアップ](#install-and-set-up-influxdb)に従い、**Token**、**Organization**、**Bucket**を設定。InfluxDB v1を選択した場合は、**Database**、**Username**、**Password**を設定してください。
   - TLSを有効にするかどうかを決定します。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../../operate/network/overview.md#enabling-tls-for-external-resource-access)を参照してください。
5. **Create**をクリックする前に、**Test Connectivity**でInfluxDBサーバーへの接続テストが可能です。
6. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択可能です。続けてルールとSinkを作成し、InfluxDBに転送するデータを指定できます。詳細は[InfluxDB Sink付きルールの作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sink付きルールの作成

このセクションでは、EMQXでMQTTトピック `t/#` のメッセージを処理し、設定済みのSinkを通じてInfluxDBに送信するルールの作成方法を説明します。

1. EMQXダッシュボードにアクセスし、左メニューの**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルール作成ページで、ルールIDに`my_rule`を入力します。

4. **SQL Editor**にルールを設定します。例えば、トピック `t/#` のMQTTメッセージをInfluxDBに保存したい場合、以下のSQL文を使用します。

   ::: tip

   独自のSQL文を指定する場合は、後で設定するSinkのデータ形式に含まれるすべての変数が`SELECT`句に含まれていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注：初心者の方は**SQL Examples**と**Enable Test**を使ってSQLルールの学習とテストが可能です。

5. + **Add Action**ボタンをクリックし、ルールがトリガーするアクションを定義します。このアクションにより、EMQXはルールで処理したデータをInfluxDBに送信します。

6. **Type of Action**のドロップダウンから`InfluxDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択可能ですが、ここでは新規Sinkを作成します。

7. Sink名を入力します。英数字の組み合わせで指定してください。

8. **Connector**のドロップダウンから先に作成した`my_influxdb`を選択します。新規作成も可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

9. **Time Precision**を指定します。デフォルトは`millisecond`です。

10. **Data Format**を`JSON`または`Line Protocol`から選択し、InfluxDBへのデータ解析・書き込み方法を指定します。

    - JSON形式の場合、**Measurement**、**Timestamp**、**Fields**、**Tags**の解析方法を定義します。すべてのキーは変数やプレースホルダーを利用可能で、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に準拠して設定できます。**Fields**はCSVファイルによる一括設定も可能です。詳細は[一括設定](#batch-setting)を参照してください。
    - Line Protocol形式の場合、テキストベースでmeasurement、tag set、field set、timestampを指定し、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に従ったプレースホルダーを利用できます。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型の値を書き込む場合は、プレースホルダーの後に`i`を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8で整数値を書き込む](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型の値を書き込む場合は、プレースホルダーの後に`u`を付けます。例：`${payload.int}u`。詳細は同上リンクを参照してください。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。プライマリSinkがメッセージ処理に失敗した場合にこれらがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：[詳細設定](#advanced-configurations)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**でSinkがInfluxDBサーバーに接続可能かテストできます。

14. **Create**をクリックしてSink作成を完了します。ルール作成ページの**Action Outputs**タブに新規Sinkが表示されます。

15. ルール作成ページで設定内容を確認し、**Create**をクリックしてルールを生成します。

これでルールが作成され、**Rule**ページに新規ルールが表示されます。**Actions(Sink)**タブをクリックすると、新規InfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**でトポロジーを確認できます。トピック `t/#` のメッセージがルール`my_rule`で解析され、InfluxDBに送信・保存されていることがわかります。

### 一括設定

InfluxDBでは1つのデータエントリに数百のフィールドが含まれることが多く、データ形式の設定が複雑になりがちです。これに対応するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータ形式を設定する際、CSVファイルからフィールドのキー・バリューを一括インポートできます。

1. **Fields**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従い、一括設定テンプレートファイルをダウンロードし、テンプレートにフィールドのキー・バリューを記入します。デフォルトのテンプレート内容は以下の通りです：

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | フィールド値に`i`を付けてInfluxDBに整数として保存する指示 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、line protocolに従い型識別子を付加可能。
   - **備考**：CSV内のメモ用で、EMQXへのインポートには含まれません。

   CSVファイルの行数は2048行を超えないようにしてください。

3. 記入したテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックし、一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・バリューをさらに調整可能です。

## ルールのテスト

MQTTクライアントMQTTXを使い、トピック `t/1` にメッセージを送信してオンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

InfluxDBのUIの**Data Explorer**ウィンドウで、メッセージがInfluxDBに書き込まれていることを確認できます。

## 詳細設定

このセクションでは、InfluxDBコネクターおよびSinkの詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**に進み、以下のパラメータをニーズに合わせて調整できます。

| **項目**               | **説明**                                                                                                                         | **推奨値** |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------------------- | ---------- |
| Start Timeout          | コネクターが自動起動したリソース（例：InfluxDBのデータベースインスタンス）が正常状態になるまで待機する最大秒数です。リソースが準備完了するまで処理を進めないようにするための設定です。 | `5`        |
| Buffer Pool Size       | EMQXとInfluxDB間の送信型ブリッジでデータフローを管理するバッファワーカープロセス数を指定します。これらのプロセスはデータを一時的に保持し、送信前に処理します。IngressのみのSinkでは`0`に設定可能です。 | `4`        |
| Request TTL            | バッファに入ったリクエストが有効とみなされる最大時間（秒）です。TTLを超えたリクエストやInfluxDBからの応答・アックが遅延したリクエストは期限切れとみなされます。 | `45`       |
| Health Check Interval  | SinkがInfluxDB接続のヘルスチェックを自動的に行う間隔（秒）です。                                                                   | `15`       |
| Max Buffer Queue Size  | 各バッファワーカーがInfluxDB Sinkでバッファリング可能な最大バイト数です。データフローの効率化に関わるため、システム性能や転送要件に応じて調整してください。 | `1`        |
| Max Batch Size         | EMQXからInfluxDBに一度に送信するデータバッチの最大サイズです。サイズ調整によりデータ転送効率を最適化できます。`1`に設定すると個別に送信されます。 | `100`      |
| Query Mode             | メッセージ送信の最適化のため、`asynchronous`（非同期）または`synchronous`（同期）を選択可能です。非同期モードではInfluxDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがInfluxDB到達前にメッセージを受け取る可能性があります。 | `Async`    |
| Inflight Window        | 送信済みだが応答・アックをまだ受け取っていない「インフライトクエリ」の最大数を制御します。**Query Mode**が`async`の場合、同一MQTTクライアントのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`      |

## 参考情報

以下のリンクからさらに詳しく学べます：

**ブログ**：

[1時間で構築するEMQX + InfluxDB + Grafana IoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[時系列IoTアプリケーション向けMQTTデータのInfluxDB統合](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
