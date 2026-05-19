# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/)は時系列データの保存と分析に特化したデータベースです。高いデータスループット性能と安定した動作により、IoT分野での利用に非常に適しています。EMQXは現在、InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの主流バージョンとの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ統合について、実践的な手順を交えて包括的に紹介します。

## 動作概要

InfluxDBデータ統合はEMQXの標準機能であり、EMQXのリアルタイムデータキャプチャと転送機能をInfluxDBのデータ保存・分析機能と組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからInfluxDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。EMQXはルールエンジンとSinkを介してデバイスデータをInfluxDBに転送し保存・分析します。InfluxDBは分析結果をレポートやグラフとして生成し、InfluxDBの可視化ツールを通じてユーザーに提供します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとInfluxDBの典型的なデータ統合アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBは、エネルギー消費データをリアルタイムに効率的に収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがIoTプラットフォームとしてデバイスの接続、メッセージ送受信、データルーティングを担当し、InfluxDBがデータ保存・分析プラットフォームとして機能します。ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：エネルギー貯蔵機器や産業用IoT機器はMQTTプロトコルでEMQXに接続し、消費電力、入出力電力などのエネルギーデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：組み込みのルールエンジンを使い、特定のトピックに基づいてメッセージを処理します。メッセージはルールエンジンを通過し、対応するルールとマッチングされ、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをInfluxDBに書き込む処理が実行されます。InfluxDB SinkはLine Protocolテンプレートを提供し、メッセージの特定フィールドをInfluxDBの測定値やフィールドに柔軟にマッピングできます。

エネルギー消費データがInfluxDBに書き込まれた後は、Line Protocolを活用してデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールに接続し、エネルギー貯蔵データのグラフを生成する。
- 業務システムに接続して、エネルギー貯蔵機器の状態監視やアラートを実施する。

## 特長とメリット

InfluxDBデータ統合は以下の特長と利点を提供します。

- **効率的なデータ処理**：EMQXは大量のIoTデバイス接続とメッセージスループットを処理可能であり、InfluxDBはデータ書き込み・保存・クエリに優れた性能を発揮し、IoTシナリオのデータ処理要件をシステムに負荷をかけずに満たします。
- **メッセージ変換**：EMQXのルールを通じてメッセージは多様な処理・変換が可能であり、InfluxDBへの書き込み前に柔軟に加工できます。
- **スケーラビリティ**：EMQXとInfluxDBは共にクラスター拡張に対応しており、ビジネスの成長に応じて水平スケールが可能です。
- **豊富なクエリ機能**：InfluxDBは最適化された関数、演算子、インデックス技術を備え、時系列データの効率的なクエリと分析を実現し、IoTデータから価値ある洞察を抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコーディング方式を採用し、ストレージコストを大幅に削減します。また、データ種別ごとに保存期間をカスタマイズ可能で、不要なデータの保存を防ぎます。

## はじめる前に

本節では、InfluxDBデータ統合を作成する前に必要な準備、特にInfluxDBのインストールと設定について説明します。

### 前提条件

- EMQXがInfluxDBにデータを書き込む際に従う[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### InfluxDBのインストールとセットアップ

1. Dockerを使って[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDB Dockerイメージの起動
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、[http://localhost:8086](http://localhost:8086)にアクセスし、**ユーザー名**、**パスワード**、**組織名**、**バケット名**を設定します。
3. InfluxDBのUIで、**Load Data** -> **API Token**をクリックし、指示に従って[全権限トークンを作成](https://docs.influxdata.com/influxdb/v2/install/#create-all-access-tokens)します。

## コネクターの作成

この節では、SinkをInfluxDBサーバーに接続するためのコネクター作成方法を示します。

以下の手順は、EMQXとInfluxDBをローカルマシンで実行していることを前提としています。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**ページで**InfluxDB**を選択し、**Next**をクリックします。

4. **Configuration**ステップで以下の情報を設定します。

   以下の設定はすべてのInfluxDBバージョン共通です。

   - **Connector Name**：コネクターの一意な名前。英数字のみで、例：`my_influxdb`
   - **Description**（任意）：コネクターの簡単な説明
   - **Server Host**：InfluxDBサーバーのアドレス。例：`127.0.0.1:8086`。InfluxDB Cloudの場合はポート`443`（例：`{url}:443`）を指定し、TLSを有効にします。
   - **Version of InfluxDB**：使用するInfluxDBのバージョンを選択。`v1`、`v2`（デフォルト）、`v3`がサポートされています。
   - **Enable TLS**：InfluxDBサーバーがTLS接続を要求する場合に有効化します。詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#enabling-tls-for-external-resource-access)を参照してください。

   選択したInfluxDBバージョンにより、必要な設定項目が異なります。以下の表の通りで、値は[InfluxDBのインストールとセットアップ](#install-and-set-up-influxdb)の内容と一致させてください。

   | 設定項目              | InfluxDB v1          | InfluxDB v2 | InfluxDB v3 |
   | --------------------- | -------------------- | ----------- | ----------- |
   | 認証方式              | ユーザー名 / パスワード | トークン     | トークン     |
   | **トークン**           | -                    | 必須        | 必須        |
   | **ユーザー名**         | 任意                 | -           | -           |
   | **パスワード**         | 任意                 | -           | -           |
   | **組織名**             | -                    | 必須        | -           |
   | **バケット**           | -                    | 必須        | -           |
   | **データベース名**     | 必須                 | -           | 必須        |

   補足：

   - **InfluxDB v1**では、EMQXは指定されたデータベースに直接データを書き込み、ユーザー名/パスワード認証は任意です。
   - **InfluxDB v2**では、EMQXは組織とバケットのモデルを使用し、トークンは指定バケットへの書き込み権限を持つ必要があります。
   - **InfluxDB v3**では、v1に似たデータベースベースのモデルを使用しますが、トークン認証を採用しています。

5. **Create**をクリックする前に、**Test Connectivity**をクリックしてEMQXがInfluxDBサーバーに正常に接続できるか確認できます。

6. **Create**をクリックしてコネクター作成を完了します。

コネクター作成後、**Back to Connector List**を選択するか、**Create Rule**をクリックしてMQTTデータをInfluxDBに転送するルールとSinkを定義できます。詳細は[InfluxDB Sinkを使ったルールの作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sinkを使ったルールの作成

この節では、EMQXでソースMQTTトピック`t/#`のメッセージを処理し、設定済みのSinkを通じてInfluxDBに送信するルールの作成方法を示します。

1. EMQXダッシュボードで、左メニューの**Integration** -> **Rules**をクリックします。

2. 画面右上の**Create**をクリックします。

3. ルール作成ページで、ルールIDに`my_rule`を入力します。

4. **SQL Editor**でルールを設定します。例えば、トピック`t/#`のMQTTメッセージをInfluxDBに保存したい場合、以下のSQL文を使用します。

   ::: tip

   独自のSQL文を指定する場合は、後で設定するSinkのデータフォーマットに含まれるすべての変数が`SELECT`句に含まれていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   補足：初心者の方は**SQL Examples**と**Enable Test**をクリックしてSQLルールの学習とテストを行うことを推奨します。

5. + **Add Action**ボタンをクリックして、ルールがトリガーするアクションを定義します。このアクションにより、EMQXはルールで処理したデータをInfluxDBに送信します。

6. **Type of Action**ドロップダウンリストから`InfluxDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択可能ですが、ここでは新規Sinkを作成します。

7. Sinkの名前を入力します。名前は英数字の組み合わせにしてください。

8. **Connector**ドロップダウンから、先ほど作成した`my_influxdb`を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメーターは[コネクターの作成](#create-a-connector)を参照してください。

9. **Time Precision**を指定します。デフォルトは`millisecond`です。

10. **Data Format**として`JSON`または`Line Protocol`を選択し、InfluxDBに書き込むデータの解析・書き込み方法を指定します。

    - JSON形式の場合、**Measurement**、**Timestamp**、**Fields**、**Tags**のデータ解析方法を定義します。すべてのキー値は変数やプレースホルダーにできます。また[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に従って設定可能です。**Fields**はCSVファイルによる一括設定もサポートしています。詳細は[一括設定](#batch-setting)を参照してください。
    - Line Protocol形式の場合、テキストベースのフォーマットで、測定値、タグセット、フィールドセット、タイムスタンプを指定し、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に準拠したプレースホルダーをサポートします。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型の値を書き込む場合、プレースホルダーの後に`i`を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8で整数値を書き込む](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型の値を書き込む場合は、`u`を付けます。例：`${payload.int}u`。詳細は同上リンクを参照してください。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：詳細は[詳細設定](#advanced-configurations)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがInfluxDBサーバーに接続できるかテスト可能です。

14. **Create**をクリックしてSink作成を完了します。ルール作成ページの**Action Outputs**タブに新しいSinkが表示されます。

15. ルール作成ページで設定内容を確認し、**Create**をクリックしてルールを生成します。

これでルール作成が完了し、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新規InfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されInfluxDBに送信・保存されていることが確認できます。

### 一括設定

InfluxDBでは1件のデータに数百のフィールドが含まれることが多く、データフォーマットの設定が煩雑になる場合があります。これに対応するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータフォーマットを設定する際、CSVファイルからフィールドのキー・値ペアを一括でインポートできます。

1. **Fields**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従い、まず一括設定用テンプレートファイルをダウンロードし、テンプレートにフィールドのキー・値ペアを記入します。デフォルトのテンプレート内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | フィールド値に`i`を付けてInfluxDBに整数として保存する指示 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、Line Protocolに従った型識別子の付加も可能。
   - **備考**：CSVファイル内の注釈用で、EMQXへのインポート対象外。

   CSVファイルの一括設定データは2048行以内にしてください。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロード後、**Import**をクリックして一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・値ペアをさらに調整可能です。

## ルールのテスト

MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージを送信してオンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、1件の新規受信メッセージと1件の新規送信メッセージがあるはずです。

InfluxDBのUIでは、**Data Explorer**ウィンドウを使ってメッセージがInfluxDBに書き込まれていることを確認できます。

## 詳細設定

本節では、InfluxDBコネクターおよびSinkの詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**に進み、以下のパラメーターをニーズに合わせて調整してください。

| **項目**              | **説明**                                                                                                                             | **推奨値** |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------ | ---------- |
| Start Timeout         | コネクターが自動起動したリソースの正常状態到達を待機する最大秒数です。この設定により、InfluxDBのデータベースインスタンスなど接続先リソースが完全に稼働し、データ処理準備が整うまで操作を進めないようにします。 | `5`        |
| Buffer Pool Size      | EMQXとInfluxDB間の送信タイプのブリッジでデータフロー管理に割り当てるバッファーワーカープロセス数です。これらのワーカーはデータ送信前の一時保存・処理を担います。IngressのみのSinkでは`0`に設定可能です。 | `4`        |
| Request TTL           | バッファに入ったリクエストが有効とみなされる最大秒数です。TTLを超えるか、InfluxDBからの応答やアックがタイムリーに得られない場合、リクエストは期限切れとみなされます。 | `45`       |
| Health Check Interval | SinkがInfluxDB接続の自動ヘルスチェックを実施する間隔（秒）です。                                                                              | `15`       |
| Max Buffer Queue Size | 各バッファーワーカーがInfluxDB Sink内でバッファリング可能な最大バイト数です。バッファーワーカーはデータ送信前の一時保存を担い、システム性能やデータ転送要件に応じて調整してください。 | `1`        |
| Max Batch Size        | EMQXからInfluxDBへ一度に転送可能なデータバッチの最大サイズです。サイズ調整によりデータ転送効率と性能を最適化できます。`1`に設定するとレコードを個別送信します。 | `100`      |
| Query Mode            | メッセージ送信要件に応じて`asynchronous`（非同期）または`synchronous`（同期）モードを選択します。非同期モードではInfluxDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがInfluxDB到着前にメッセージを受信する可能性があります。 | `Async`    |
| Inflight Window       | 未応答または未アックの「インフライトクエリ」の最大数を制御します。**Query Mode**が`async`の場合に重要で、同一MQTTクライアントからのメッセージを厳密な順序で処理したい場合は`1`に設定してください。 | `100`      |

## 参考情報

以下のリンクもご参照ください。

**ブログ**：

[1時間で構築するEMQX + InfluxDB + Grafana IoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[時系列IoTアプリケーションのためのMQTTデータのInfluxDB統合](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
