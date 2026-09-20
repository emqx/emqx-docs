# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/)は時系列データの保存と分析に特化したデータベースです。高いデータスループット性能と安定した動作により、IoT分野での利用に非常に適しています。EMQXは現在、InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの主要なバージョンへの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ統合について、実践的な手順を交えて包括的に解説します。

## 動作の仕組み

InfluxDBデータ統合は、EMQXに標準搭載された機能であり、EMQXのリアルタイムデータキャプチャと転送機能をInfluxDBのデータ保存・分析機能と組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからInfluxDBへのデータ取り込みが簡素化され、複雑なコーディングを不要にします。EMQXはルールエンジンとSinkを介してデバイスデータをInfluxDBに転送し保存・分析します。InfluxDBは分析結果をレポートやチャートとして生成し、InfluxDBの可視化ツールを通じてユーザーに提供します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとInfluxDBの典型的なデータ統合アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBは、エネルギー消費データをリアルタイムに効率的に収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがIoTプラットフォームとしてデバイス接続、メッセージ転送、データルーティングを担当し、InfluxDBがデータ保存・分析プラットフォームとして機能します。ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：エネルギー貯蔵デバイスや産業用IoTデバイスはMQTTプロトコルを用いてEMQXに接続し、電力消費量、入出力電力などのエネルギーデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：組み込みのルールエンジンを使い、特定のトピックにマッチするメッセージを処理します。メッセージはルールエンジンを通過し、対応するルールとマッチングされ、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをInfluxDBに書き込む操作が実行されます。InfluxDB SinkはLine Protocolのテンプレートを提供し、メッセージの特定フィールドをInfluxDBの計測値やフィールドに柔軟にマッピング可能です。

エネルギー消費データがInfluxDBに書き込まれた後は、Line Protocolを活用してデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールと連携し、エネルギーデータのチャートを生成・表示する。
- 業務システムと連携し、エネルギー貯蔵デバイスの状態監視やアラートを実施する。

## 特長と利点

InfluxDBデータ統合は以下の特長とメリットを提供します。

- **効率的なデータ処理**：EMQXは大量のIoTデバイス接続とメッセージスループットを処理可能であり、InfluxDBはデータ書き込み・保存・クエリに優れた性能を発揮します。これにより、IoTシナリオのデータ処理要件をシステムに過度な負荷をかけずに満たせます。
- **メッセージ変換**：EMQXのルールを介してメッセージを多様に処理・変換してからInfluxDBに書き込めます。
- **スケーラビリティ**：EMQXとInfluxDBの両方がクラスター拡張に対応し、ビジネス拡大に応じて柔軟に水平スケール可能です。
- **豊富なクエリ機能**：InfluxDBは最適化された関数、演算子、インデックス技術を備え、タイムスタンプ付きデータの効率的なクエリと分析を実現し、IoT時系列データから価値ある洞察を抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコード方式を採用し、ストレージコストを大幅に削減します。また、データ種別ごとに保存期間をカスタマイズ可能で、不必要なデータのストレージ占有を防止します。

## はじめる前に

本節では、InfluxDBデータ統合を作成する前に必要な準備、特にInfluxDBのインストールと設定について説明します。

### 前提条件

- EMQXがInfluxDBにデータを書き込む際に従う[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### InfluxDBのインストールと設定

1. Docker経由で[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDB Dockerイメージの起動
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、ブラウザで[http://localhost:8086](http://localhost:8086)にアクセスし、**ユーザー名**、**パスワード**、**組織名**、**バケット名**を設定します。
3. InfluxDB UIの**Load Data** -> **API Token**をクリックし、[全権限トークンの作成](https://docs.influxdata.com/influxdb/v2/install/#create-all-access-tokens)手順に従ってトークンを作成します。

## コネクターの作成

この節では、SinkをInfluxDBサーバーに接続するためのコネクター作成手順を示します。

以下の手順は、EMQXとInfluxDBをローカル環境で実行していることを前提としています。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**ページで**InfluxDB**を選択し、**Next**をクリックします。

4. **Configuration**ステップで以下の情報を設定します。

   以下の設定は全InfluxDBバージョン共通です。

   - **Connector Name**：コネクターの一意な名前。英数字のみで構成し、例：`my_influxdb`
   - **Description**（任意）：コネクターの簡単な説明
   - **Server Host**：InfluxDBサーバーのアドレス（例：`127.0.0.1:8086`）。InfluxDB Cloudの場合はポート`443`（例：`{url}:443`）を指定しTLSを有効にします。
   - **Version of InfluxDB**：使用するInfluxDBのバージョンを選択。`v1`、`v2`（デフォルト）、`v3`がサポートされています。
   - **Enable TLS**：InfluxDBサーバーがTLS接続を要求する場合は有効にします。詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#enabling-tls-for-external-resource-access)を参照してください。

   選択したInfluxDBバージョンにより必要な設定項目が異なります。以下の表を参照し、[InfluxDBのインストールと設定](#install-and-set-up-influxdb)での設定と一致させてください。

   | 設定項目              | InfluxDB v1         | InfluxDB v2 | InfluxDB v3 |
   | --------------------- | ------------------- | ----------- | ----------- |
   | 認証方式              | ユーザー名 / パスワード | トークン     | トークン     |
   | **Token**             | -                   | 必須        | 必須        |
   | **Username**          | 任意                | -           | -           |
   | **Password**          | 任意                | -           | -           |
   | **Organization**      | -                   | 必須        | -           |
   | **Bucket**            | -                   | 必須        | -           |
   | **Database Name**     | 必須                | -           | 必須        |

   補足：

   - **InfluxDB v1**では、EMQXは指定されたデータベースに直接データを書き込み、ユーザー名/パスワード認証は任意です。
   - **InfluxDB v2**では、組織とバケットモデルを使用し、指定バケットへの書き込み権限を持つトークンが必要です。
   - **InfluxDB v3**はv1に似たデータベースベースのモデルを採用しますが、トークン認証を使用します。

5. **Create**をクリックする前に、**Test Connectivity**をクリックしてEMQXがInfluxDBサーバーに正常に接続できるか確認できます。

6. **Create**をクリックしてコネクター作成を完了します。

作成後は**Back to Connector List**を選択するか、続けて**Create Rule**をクリックし、MQTTデータをInfluxDBに転送するルールとSinkを定義できます。詳細は[InfluxDB Sinkを用いたルール作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sinkを用いたルール作成

この節では、EMQXでMQTTトピック `t/#` のメッセージを処理し、設定済みのSinkを通じてInfluxDBに送信するルールの作成方法を示します。

1. EMQXダッシュボードで、左メニューから**Integration** -> **Rules**をクリックします。

2. 画面右上の**Create**をクリックします。

3. ルール作成ページで、ルールIDに`my_rule`を入力します。

4. **SQL Editor**でルールを設定します。例えば、トピック `t/#` のMQTTメッセージをInfluxDBに保存したい場合、以下のSQL文を使用します。

   ::: tip

   独自のSQL文を指定する場合は、後で設定するSinkのデータ形式に含まれるすべての変数が`SELECT`部分に含まれていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   補足：初心者の方は**SQL Examples**と**Enable Test**をクリックし、SQLルールの学習とテストを行うことを推奨します。

5. + **Add Action**ボタンをクリックし、ルールがトリガーするアクションを定義します。このアクションにより、EMQXはルールで処理したデータをInfluxDBに送信します。

6. **Type of Action**ドロップダウンから`InfluxDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択も可能です。この例では新規Sinkを作成します。

7. Sinkの名前を入力します。英大文字・小文字と数字の組み合わせが望ましいです。

8. **Connector**ドロップダウンから先ほど作成した`my_influxdb`を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

9. **Time Precision**を指定します。デフォルトは`millisecond`です。

10. **Data Format**を`JSON`または`Line Protocol`から選択し、InfluxDBへのデータ解析・書き込み方法を指定します。

    - JSON形式の場合、**Measurement**、**Timestamp**、**Fields**、**Tags**の解析方法を定義します。すべてのキー値は変数やプレースホルダーを使用可能で、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に従って設定できます。**Fields**はCSVファイルによる一括設定もサポートしています。詳細は[一括設定](#batch-setting)を参照してください。
    - Line Protocol形式の場合、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に準拠したテキスト形式で、計測値、タグセット、フィールドセット、タイムスタンプを指定し、プレースホルダーも利用可能です。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型の値を書き込む場合、プレースホルダーの後に`i`を付けます（例：`${payload.int}i`）。詳細は[InfluxDB 1.8 整数値の書き込み](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型の値を書き込む場合は、`u`を付けます（例：`${payload.int}u`）。詳細は同上を参照してください。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：詳細は[高度な設定](#advanced-configurations)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがInfluxDBサーバーに接続可能かテストできます。

14. **Create**をクリックしてSink作成を完了します。ルール作成ページの**Action Outputs**タブに新しいSinkが表示されます。

15. ルール作成ページで設定内容を確認し、**Create**をクリックしてルールを生成します。

これでルールが正常に作成され、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新規InfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール`my_rule`で解析されInfluxDBに送信・保存されている様子が確認できます。

### 一括設定

InfluxDBのデータエントリは通常数百のフィールドを含むため、データ形式の設定は複雑になりがちです。これを解決するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータ形式を設定する際、CSVファイルからフィールドのキー・バリューを一括インポートできます。

1. **Fields**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従い、一括設定テンプレートファイルをダウンロードし、フィールドのキー・バリューを記入します。テンプレートのデフォルト内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ------------------------------------------------------------ |
   | temp   | ${payload.temp}    |                                                              |
   | hum    | ${payload.hum}     |                                                              |
   | precip | ${payload.precip}i | フィールド値に`i`を付けてInfluxDBに整数として保存することを示す。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、line protocolに従い型識別子を付加可能。
   - **備考**：CSV内の注釈用で、EMQXへのインポート対象外。

   CSVファイルの一括設定データは2048行を超えないようにしてください。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックし、一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・バリューをさらに調整可能です。

## ルールのテスト

MQTTクライアントMQTTXを使用して、トピック `t/1` にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

InfluxDB UIの**Data Explorer**ウィンドウで、メッセージがInfluxDBに書き込まれていることを確認できます。

## 高度な設定

本節では、InfluxDBコネクターおよびSinkの高度な設定オプションについて詳述します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**を開き、以下のパラメータをニーズに合わせて調整してください。

| **項目**               | **説明**                                                                                          | **推奨値** |
| --------------------- | ------------------------------------------------------------------------------------------------ | ---------- |
| Start Timeout         | コネクターが自動起動したリソース（例：InfluxDBのデータベースインスタンス）が正常状態になるまで待機する最大秒数です。この設定により、リソースが完全に稼働しデータ処理可能になるまで操作を進めないようにします。 | `5`        |
| Buffer Pool Size      | EMQXとInfluxDB間のエグレス（送信）タイプのブリッジでデータフローを管理するバッファワーカープロセス数を指定します。これらのワーカーはデータ送信前に一時的にデータを保持・処理します。インバウンドのみのSinkでは`0`に設定可能です。 | `4`        |
| Request TTL           | バッファに入ったリクエストが有効とみなされる最大秒数です。TTLを超えてバッファに残るか、InfluxDBからの応答・アックが遅延するとリクエストは期限切れと判断されます。 | `45`       |
| Health Check Interval | SinkがInfluxDB接続のヘルスチェックを自動実行する間隔（秒）です。 | `15`       |
| Max Buffer Queue Size | 各バッファワーカーがInfluxDB Sinkでバッファリング可能な最大バイト数です。データ転送要件やシステム性能に応じて調整してください。 | `1`        |
| Max Batch Size        | EMQXからInfluxDBへ一度に送信可能なデータバッチの最大サイズです。サイズを調整することで転送効率を最適化できます。`1`に設定すると単一レコードずつ送信されます。 | `100`      |
| Query Mode            | メッセージ送信要件に応じて`asynchronous`（非同期）または`synchronous`（同期）モードを選択可能です。非同期モードではInfluxDBへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがInfluxDBへの書き込み前にメッセージを受信する可能性があります。 | `Async`    |
| Inflight Window       | 送信済みだが応答・アックをまだ受け取っていない「インフライトクエリ」の最大数を制御します。**Query Mode**が`async`の場合、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合はこの値を1に設定してください。 | `100`      |

## 参考情報

以下のリンクからさらに詳細を学べます。

**ブログ**：

[1時間で構築するEMQX + InfluxDB + GrafanaによるIoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[MQTTデータをInfluxDBに統合した時系列IoTアプリケーションの構築](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
