# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/)は時系列データの保存と分析に特化したデータベースです。その高いデータスループット性能と安定した動作により、IoT（モノのインターネット）分野での利用に非常に適しています。EMQXは現在、InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの主要なバージョンとの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ統合について、実践的な手順を交えて包括的に解説します。

## 動作概要

InfluxDBデータ統合はEMQXに標準搭載された機能であり、EMQXのリアルタイムデータキャプチャと転送機能とInfluxDBのデータ保存・分析機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからInfluxDBへのデータ取り込みが簡素化され、複雑なコーディングを不要にします。EMQXはルールエンジンとSinkを介してデバイスデータをInfluxDBに転送し保存・分析を行います。InfluxDBは分析結果をレポートやグラフなどの形で生成し、InfluxDBの可視化ツールを通じてユーザーに提供します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとInfluxDBの典型的なデータ統合アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBはリアルタイムでエネルギー消費データを効率的に収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがIoTプラットフォームとしてデバイス接続、メッセージ転送、データルーティングを担当し、InfluxDBがデータ保存および分析プラットフォームとして機能します。ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：エネルギー貯蔵機器や産業用IoT機器はMQTTプロトコルを介してEMQXに接続し、電力消費量や入出力電力などのエネルギーデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：組み込みのルールエンジンを用いて、特定の送信元からのメッセージをトピックマッチングに基づいて処理します。メッセージが到着するとルールエンジンを通過し、対応するルールにマッチした後、データフォーマットの変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、InfluxDBへの書き込み処理が実行されます。InfluxDB SinkはLine Protocolのテンプレートを提供し、メッセージの特定フィールドをInfluxDBの対応するmeasurementやfieldに柔軟にマッピングできます。

エネルギー消費データがInfluxDBに書き込まれた後は、Line Protocolを活用してデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールと連携し、エネルギーデータのグラフを生成・表示する
- 業務システムと連携してエネルギー貯蔵機器の状態監視やアラートを行う

## 特長とメリット

InfluxDBデータ統合は以下の特長と利点を備えています。

- **効率的なデータ処理**：EMQXは大量のIoTデバイス接続とメッセージスループットを処理可能であり、InfluxDBはデータ書き込み、保存、クエリに優れた性能を発揮します。これによりIoTシナリオのデータ処理要件をシステムに過度な負荷をかけずに満たします。
- **メッセージ変換**：EMQXのルールを活用し、InfluxDBに書き込む前にメッセージを多様に処理・変換できます。
- **スケーラビリティ**：EMQXとInfluxDBの両方がクラスター拡張に対応しており、ビジネスの成長に応じて柔軟に水平拡張が可能です。
- **豊富なクエリ機能**：InfluxDBは最適化された関数、演算子、インデックス技術を備え、タイムスタンプ付きデータの効率的なクエリと分析を実現し、IoT時系列データから価値ある洞察を正確に抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコード方式を採用し、ストレージコストを大幅に削減します。また、データ種別ごとに保存期間をカスタマイズ可能で、不必要なデータのストレージ占有を防ぎます。

## はじめる前に

このセクションでは、InfluxDBデータ統合の作成に先立ち必要な準備、特にInfluxDBのインストールとセットアップについて説明します。

### 前提条件

- EMQXがInfluxDBにデータを書き込む際に従う[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXデータ統合の[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識

### InfluxDBのインストールとセットアップ

1. Dockerを使って[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDBのDockerイメージを起動するコマンド
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、ブラウザで [http://localhost:8086](http://localhost:8086) にアクセスし、**Username**、**Password**、**Organization Name**、**Bucket Name**を設定します。
3. InfluxDBのUIで、**Load Data** -> **API Token**をクリックし、指示に従って[全権限トークンを作成](https://docs.influxdata.com/influxdb/v2/install/#create-all-access-tokens)します。

## コネクターの作成

このセクションでは、SinkをInfluxDBサーバーに接続するためのコネクター作成方法を説明します。

以下の手順はEMQXとInfluxDBをローカルマシンで実行している前提です。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. 画面右上の**Create**をクリックします。
3. **Create Connector**ページで**InfluxDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します。

   以下の設定はすべてのInfluxDBバージョン共通です。

   - **Connector Name**：コネクターの一意な名前。英数字のみで例：`my_influxdb`
   - **Description**（任意）：コネクターの簡単な説明
   - **Server Host**：InfluxDBサーバーのアドレス。例：`127.0.0.1:8086`。InfluxDB Cloudを使用する場合はポート`443`（例：`{url}:443`）を指定しTLSを有効にします。
   - **Version of InfluxDB**：使用しているInfluxDBのバージョンを選択。`v1`、`v2`（デフォルト）、`v3`をサポート。
   - **Enable TLS**：InfluxDBサーバーがTLS接続を要求する場合に有効化します。詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#enabling-tls-for-external-resource-access)を参照。

   選択したInfluxDBバージョンにより必要な設定項目は異なります。以下の表は[InfluxDBのインストールとセットアップ](#install-and-set-up-influxdb)での設定と一致させてください。

   | 設定項目             | InfluxDB v1          | InfluxDB v2 | InfluxDB v3 |
   | -------------------- | -------------------- | ----------- | ----------- |
   | 認証方式             | ユーザー名 / パスワード | トークン     | トークン     |
   | **Token**            | -                    | 必須        | 必須        |
   | **Username**         | 任意                 | -           | -           |
   | **Password**         | 任意                 | -           | -           |
   | **Organization**     | -                    | 必須        | -           |
   | **Bucket**           | -                    | 必須        | -           |
   | **Database Name**    | 必須                 | -           | 必須        |

   注意：

   - **InfluxDB v1**では、EMQXは指定したデータベースに直接書き込み、ユーザー名/パスワード認証は任意です。
   - **InfluxDB v2**では、組織とバケットのモデルを使用し、トークンは指定バケットへの書き込み権限を持つ必要があります。
   - **InfluxDB v3**では、v1に似たデータベースベースのモデルを採用しつつ、トークン認証を使用します。

5. **Create**をクリックする前に、**Test Connectivity**をクリックしてEMQXがInfluxDBサーバーに正常に接続できるか確認できます。
6. **Create**をクリックしてコネクター作成を完了します。

コネクター作成後は、**Back to Connector List**を選択するか、続けて**Create Rule**をクリックし、MQTTデータをInfluxDBに転送するルールとSinkを定義できます。詳細は[InfluxDB Sinkを用いたルールの作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sinkを用いたルールの作成

このセクションでは、EMQXでソースMQTTトピック `t/#` のメッセージを処理し、設定済みのSinkを通じてInfluxDBに送信するルールの作成方法を説明します。

1. EMQXダッシュボードにアクセスし、左メニューの**Integration** -> **Rules**をクリックします。
2. 画面右上の**Create**をクリックします。
3. ルール作成ページで、ルールIDに `my_rule` と入力します。
4. **SQL Editor**でルールを設定します。例えば、トピック `t/#` のMQTTメッセージをInfluxDBに保存したい場合、以下のSQL文を使用します。

   ::: tip

   独自のSQL文を指定する場合、後で設定するSinkのデータフォーマットに含まれるすべての変数が`SELECT`句に含まれていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注：初心者の場合は**SQL Examples**や**Enable Test**をクリックしてSQLルールを学習・テストできます。

5. + **Add Action**ボタンをクリックして、ルールがトリガーするアクションを定義します。このアクションにより、EMQXはルールで処理したデータをInfluxDBに送信します。
6. **Type of Action**ドロップダウンから`InfluxDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。すでにSinkを作成していれば選択も可能です。この例では新規Sinkを作成します。
7. Sinkの名前を入力します。英数字の大文字・小文字を組み合わせてください。
8. **Connector**ドロップダウンから先ほど作成した`my_influxdb`を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックします。設定パラメーターは[コネクターの作成](#create-a-connector)を参照してください。
9. **Time Precision**を指定します。デフォルトは`millisecond`です。
10. **Data Format**を`JSON`または`Line Protocol`から選択し、InfluxDBへのデータ解析・書き込み方法を指定します。

    - JSONフォーマットの場合、**Measurement**、**Timestamp**、**Fields**、**Tags**などのデータ解析方法を定義します。すべてのキー値は変数やプレースホルダーにでき、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に従って設定可能です。**Fields**はCSVファイルによる一括設定もサポートしています。詳細は[一括設定](#batch-setting)を参照してください。
    - Line Protocolフォーマットの場合、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に準拠したテキスト形式で、measurement、tagセット、fieldセット、タイムスタンプを指定します。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型の値を書き込む場合、プレースホルダーの後に`i`を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8で整数値を書き込む方法](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型の値を書き込む場合は`u`を付けます。例：`${payload.int}u`。詳細は同上リンクを参照してください。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にこれらがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
12. **詳細設定（任意）**：詳細は[高度な設定](#advanced-configurations)を参照してください。
13. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがInfluxDBサーバーに接続できるかテスト可能です。
14. **Create**をクリックしてSink作成を完了します。ルール作成ページの**Action Outputs**タブに新しいSinkが表示されます。
15. ルール作成ページで設定内容を確認し、**Create**をクリックしてルールを生成します。

これでルールが正常に作成され、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新規InfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール`my_rule`で解析されInfluxDBに送信・保存されていることがわかります。

### 一括設定

InfluxDBでは1つのデータエントリに数百のフィールドが含まれることが多く、データフォーマットの設定は煩雑になりがちです。これを解決するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータフォーマットを設定する際、CSVファイルからフィールドのキー・バリューを一括インポートできます。

1. **Fields**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。
2. 指示に従い、一括設定テンプレートファイルをダウンロードし、フィールドのキー・バリューを記入します。テンプレートのデフォルト内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                             |
   | ------ | ------------------ | -------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                          |
   | hum    | ${payload.hum}     |                                                          |
   | precip | ${payload.precip}i | フィールド値に`i`を付けてInfluxDBに整数として保存する。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、line protocolに従い型識別子を付加可能。
   - **備考**：CSV内のメモ用であり、EMQXにはインポートされません。

   CSVファイルの一括設定データは2048行を超えないようにしてください。

3. 記入したテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロード後、**Import**をクリックして一括設定を完了します。
4. インポート後は**Fields**設定テーブルでキー・バリューをさらに調整可能です。

## ルールのテスト

MQTTクライアントMQTTXを使い、トピック `t/1` にメッセージを送信してオンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

InfluxDBのUIでは、**Data Explorer**ウィンドウでメッセージがInfluxDBに書き込まれているか確認できます。

## 高度な設定

このセクションでは、InfluxDBコネクターおよびSinkの高度な設定オプションについて詳述します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**に進み、以下のパラメーターをニーズに合わせて調整できます。

| **項目**               | **説明**                                                                                                                      | **推奨値** |
| ---------------------- | ----------------------------------------------------------------------------------------------------------------------------- | ---------- |
| Start Timeout          | コネクターが自動起動したリソースの正常状態到達を待つ最大時間（秒）。InfluxDBのデータベースインスタンスなど接続先リソースが完全に稼働し準備完了するまで操作を進めないようにするための設定です。 | `5`        |
| Buffer Pool Size       | EMQXとInfluxDB間の出力型ブリッジでデータフローを管理するバッファワーカープロセス数。これらのワーカーはデータを一時的に保存・処理し、送信前のデータフローを最適化します。Ingress（入力）データのみ扱うSinkの場合は`0`に設定可能です。 | `4`        |
| Request TTL            | バッファに入ったリクエストが有効とみなされる最大期間（秒）。この期間を超えて応答やアックがInfluxDBから返されない場合、リクエストは期限切れと判断されます。 | `45`       |
| Health Check Interval  | SinkがInfluxDBとの接続状態を自動的にヘルスチェックする間隔（秒）です。                                                       | `15`       |
| Max Buffer Queue Size  | InfluxDB Sinkの各バッファワーカーが保持可能な最大バイト数。データ送信前の一時保存領域として機能し、システム性能や転送要件に応じて調整します。 | `1`        |
| Max Batch Size         | EMQXからInfluxDBに一度に転送可能なデータバッチの最大サイズ。サイズを調整することで転送効率や性能を最適化できます。`1`の場合は個別にデータを送信します。 | `100`      |
| Query Mode             | メッセージ送信を最適化するため、`asynchronous`（非同期）または`synchronous`（同期）モードを選択可能。非同期モードではInfluxDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがInfluxDBへの書き込み前にメッセージを受信する可能性があります。 | `Async`    |
| Inflight Window        | 「インフライトクエリ」とは開始済みで応答やアックをまだ受けていないクエリのことです。SinkがInfluxDBと通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode**が`async`の場合、このパラメーターは特に重要です。同一MQTTクライアントからのメッセージを厳密に順序通り処理したい場合は`1`に設定してください。 | `100`      |

## さらに詳しく

以下のリンクから詳細情報をご覧いただけます。

**ブログ**：

[1時間で構築するEMQX + InfluxDB + Grafana IoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[時系列IoTアプリケーションのためのMQTTデータをInfluxDBに統合](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
