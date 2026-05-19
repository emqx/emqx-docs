# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/)は時系列データの保存と分析に特化したデータベースです。その高いデータスループット性能と安定した動作により、IoT（Internet of Things）分野での利用に非常に適しています。EMQXは現在、InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの主流バージョンとの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ統合について、作成方法や検証手順を含めて包括的に紹介します。

## 動作概要

InfluxDBデータ統合はEMQXの標準機能であり、EMQXのリアルタイムデータキャプチャと転送機能をInfluxDBのデータ保存・分析機能と組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからInfluxDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。EMQXはルールエンジンとSinkを介してデバイスデータをInfluxDBに転送し保存・分析を行います。InfluxDBは分析結果をレポートやグラフとして生成し、InfluxDBの可視化ツールを通じてユーザーに提供します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとInfluxDBの典型的なデータ統合アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBは、リアルタイムでエネルギー消費データを効率的に収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがIoTプラットフォームとしてデバイスの接続、メッセージ転送、データルーティングを担当し、InfluxDBがデータ保存・分析プラットフォームとして機能します。ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：エネルギー貯蔵装置や産業用IoTデバイスはMQTTプロトコルを使いEMQXに接続し、電力消費量、入出力電力などのエネルギーデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータ処理**：組み込みのルールエンジンを使い、特定のトピックにマッチしたメッセージを処理します。メッセージはルールエンジンを通過し、対応するルールにマッチすると、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義したルールがトリガーとなり、メッセージをInfluxDBに書き込む操作が実行されます。InfluxDB SinkはLine Protocolテンプレートを提供し、メッセージの特定フィールドをInfluxDBの測定値やフィールドに柔軟にマッピングできます。

エネルギーデータがInfluxDBに書き込まれた後は、Line Protocolを活用して以下のような分析が可能です。

- Grafanaなどの可視化ツールと連携し、エネルギー貯蔵データのグラフを生成・表示する。
- 業務システムと連携し、エネルギー貯蔵装置の状態監視やアラート発報を行う。

## 特長と利点

InfluxDBデータ統合は以下の特長とメリットを提供します。

- **効率的なデータ処理**：EMQXは大量のIoTデバイス接続とメッセージスループットを処理可能であり、InfluxDBはデータ書き込み、保存、クエリに優れた性能を持つため、IoTシナリオのデータ処理要件をシステムに過度な負荷をかけずに満たします。
- **メッセージ変換**：メッセージはEMQXのルールで多様な処理・変換を経てInfluxDBに書き込まれます。
- **スケーラビリティ**：EMQXとInfluxDBの両方がクラスター拡張に対応し、ビジネス成長に応じて柔軟に水平拡張が可能です。
- **豊富なクエリ機能**：InfluxDBは最適化された関数や演算子、インデックス技術を備え、時系列データの効率的なクエリと分析を実現し、IoTデータから価値ある洞察を抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコード方式を採用し、ストレージコストを大幅に削減します。また、データ種別ごとに保存期間をカスタマイズ可能で、不要なデータによるストレージ圧迫を防止します。

## はじめる前に

このセクションでは、InfluxDBデータ統合の作成前に必要な準備、特にInfluxDBのインストールとセットアップについて説明します。

### 前提条件

- EMQXがInfluxDBにデータを書き込む際に従う[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXのデータ統合[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識

### InfluxDBのインストールとセットアップ

1. Dockerを使って[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDBのDockerイメージを起動
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、ブラウザで [http://localhost:8086](http://localhost:8086) にアクセスし、**ユーザー名**、**パスワード**、**組織名**、**バケット名**を設定します。
3. InfluxDBのUIで、**Load Data** -> **API Token**をクリックし、[全権限トークンの作成](https://docs.influxdata.com/influxdb/v2/install/#create-all-access-tokens)手順に従います。

## コネクターの作成

このセクションでは、SinkをInfluxDBサーバーに接続するコネクターの作成方法を説明します。

以下の手順はEMQXとInfluxDBをローカルマシンで実行している場合を想定しています。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックします。

3. **Create Connector**ページで**InfluxDB**を選択し、**Next**をクリックします。

4. **Configuration**ステップで以下の情報を設定します。

   以下の設定はすべてのInfluxDBバージョン共通です。

   - **Connector Name**：コネクターの一意な名前。英数字のみで、例：`my_influxdb`
   - **Description**（任意）：コネクターの簡単な説明
   - **Server Host**：InfluxDBサーバーのアドレス。例：`127.0.0.1:8086`。InfluxDB Cloudの場合はポート`443`（例：`{url}:443`）を指定しTLSを有効にします。
   - **Version of InfluxDB**：使用しているInfluxDBのバージョンを選択。`v1`、`v2`（デフォルト）、`v3`がサポートされています。
   - **Enable TLS**：InfluxDBサーバーがTLS接続を要求する場合に有効にします。詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#enabling-tls-for-external-resource-access)を参照してください。

   選択したInfluxDBバージョンにより必要な設定項目が異なります。以下の表を参照し、[InfluxDBのインストールとセットアップ](#install-and-set-up-influxdb)での設定と一致させてください。

   | 設定項目               | InfluxDB v1         | InfluxDB v2 | InfluxDB v3 |
   | ---------------------- | ------------------- | ----------- | ----------- |
   | 認証方式               | ユーザー名／パスワード | トークン     | トークン     |
   | **トークン**            | -                   | 必須        | 必須        |
   | **ユーザー名**          | 任意                | -           | -           |
   | **パスワード**          | 任意                | -           | -           |
   | **組織**               | -                   | 必須        | -           |
   | **バケット**           | -                   | 必須        | -           |
   | **データベース名**      | 必須                | -           | 必須        |

   補足：

   - **InfluxDB v1**では、EMQXは指定されたデータベースに直接書き込み、ユーザー名／パスワード認証は任意です。
   - **InfluxDB v2**では、組織とバケットモデルを使用し、トークンは指定バケットへの書き込み権限が必要です。
   - **InfluxDB v3**では、v1に似たデータベースベースのモデルを使いますが、トークン認証を採用しています。

5. **Create**をクリックする前に、**Test Connectivity**をクリックしてEMQXがInfluxDBサーバーに正常に接続できるか確認できます。

6. **Create**をクリックしてコネクターの作成を完了します。

作成後は、**Back to Connector List**を選択するか、続けて**Create Rule**をクリックし、MQTTデータをInfluxDBに転送するルールとSinkを定義できます。詳細は[InfluxDB Sinkを使ったルール作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sinkを使ったルール作成

このセクションでは、EMQXでMQTTのソーストピック `t/#` からメッセージを処理し、設定済みのSinkを通じてInfluxDBに送信するルールの作成方法を説明します。

1. EMQXダッシュボードで左側ナビゲーションメニューから**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルール作成ページでルールIDに`my_rule`と入力します。

4. **SQL Editor**でルールを設定します。例えば、トピック `t/#` のMQTTメッセージをInfluxDBに保存したい場合、以下のSQLを使用します。

   ::: tip

   独自のSQL構文を指定する場合は、後で設定するSinkのデータフォーマットに含まれるすべての変数が`SELECT`句に含まれていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   補足：初心者の方は**SQL Examples**と**Enable Test**をクリックしてSQLルールを学習・テストできます。

5. + **Add Action**ボタンをクリックし、ルールがトリガーした際のアクションを定義します。このアクションにより、EMQXはルールで処理したデータをInfluxDBに送信します。

6. **Type of Action**ドロップダウンリストから`InfluxDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択も可能です。この例では新規Sinkを作成します。

7. Sinkの名前を入力します。名前は英数字の大文字・小文字を組み合わせてください。

8. **Connector**ドロップダウンから先ほど作成した`my_influxdb`を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

9. **Time Precision**を指定します。デフォルトは`millisecond`です。

10. **Data Format**を`JSON`または`Line Protocol`から選択し、InfluxDBへのデータ解析・書き込み方法を指定します。

    - JSON形式の場合、**Measurement**、**Timestamp**、**Fields**、**Tags**の解析方法を定義します。すべてのキー値は変数やプレースホルダーで指定可能で、[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に従うこともできます。**Fields**はCSVファイルを使った一括設定にも対応しています。詳細は[一括設定](#batch-setting)を参照してください。
    - Line Protocol形式の場合、InfluxDB Line Protocolの構文に従い、測定値、タグセット、フィールドセット、タイムスタンプをテキスト形式で指定します。プレースホルダーもサポートしています。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型の値を書き込む場合、プレースホルダーの後に`i`を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8で整数値を書き込む方法](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型の場合は`u`を付けます。例：`${payload.int}u`。詳細は同上リンクを参照してください。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：詳細は[高度な設定](#advanced-configurations)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがInfluxDBサーバーに接続できるかテストできます。

14. **Create**をクリックしてSinkの作成を完了します。ルール作成ページの**Action Outputs**タブに新しいSinkが表示されます。

15. ルール作成ページで設定内容を確認し、**Create**ボタンをクリックしてルールを生成します。

これでルールが正常に作成され、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると新しいInfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール`my_rule`で解析されInfluxDBに送信・保存されていることが確認できます。

### 一括設定

InfluxDBのデータエントリは通常数百のフィールドを含むため、データフォーマットの設定は複雑になりがちです。これを解決するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータフォーマットを設定する際、CSVファイルからフィールドのキー・バリューを一括インポートできます。

1. **Fields**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従い、一括設定テンプレートファイルをダウンロードし、テンプレートにフィールドのキー・バリューを記入します。テンプレートのデフォルト内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | フィールド値の後ろに`i`を付けることでInfluxDBに整数として保存 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、Line Protocolに従い型識別子を付加可能。
   - **備考**：CSV内のメモ用で、EMQXへのインポート対象外。

   CSVファイルの一括設定データは2048行を超えないようにしてください。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロード後、**Import**をクリックして一括設定を完了します。

4. インポート後は**Fields**設定テーブルでキー・バリューをさらに調整可能です。

## ルールのテスト

MQTTクライアントMQTTXを使い、トピック `t/1` にメッセージを送信してオンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、新しい受信メッセージと送信メッセージが1件ずつあるはずです。

InfluxDBのUIでは、**Data Explorer**ウィンドウでメッセージがInfluxDBに書き込まれていることを確認できます。

## 高度な設定

このセクションでは、InfluxDBコネクターおよびSinkの高度な設定オプションについて詳述します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**に進み、以下のパラメータをニーズに合わせて調整してください。

| **項目**               | **説明**                                                                                                                        | **推奨値** |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------- | ---------- |
| Start Timeout          | コネクターが自動起動したリソース（例：InfluxDBのデータベースインスタンス）が正常状態になるまで待機する最大秒数です。リソースの準備完了を確認してから処理を進めるための設定です。 | `5`        |
| Buffer Pool Size       | EMQXとInfluxDB間の送信型ブリッジでデータフローを管理するバッファワーカープロセス数を指定します。これらのワーカーは送信前のデータを一時的に保持します。IngressのみのSinkでは`0`に設定可能です。 | `4`        |
| Request TTL            | バッファに入ったリクエストが有効とみなされる最大秒数です。TTLを超えるか、InfluxDBからの応答やアックがタイムリーに得られない場合、リクエストは期限切れと判断されます。 | `45`       |
| Health Check Interval  | SinkがInfluxDBへの接続状態を自動的にヘルスチェックする間隔（秒）です。                                                        | `15`       |
| Max Buffer Queue Size  | 各バッファワーカーがInfluxDB Sinkでバッファリング可能な最大バイト数です。パフォーマンスやデータ転送要件に応じて調整してください。 | `1`        |
| Max Batch Size         | EMQXからInfluxDBへ一度に送信可能なデータバッチの最大サイズです。サイズを調整することで転送効率を最適化できます。`1`に設定するとレコード単位で送信されます。 | `100`      |
| Query Mode             | メッセージ送信要件に応じて`asynchronous`（非同期）または`synchronous`（同期）を選択します。非同期モードではInfluxDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがInfluxDB書き込み前にメッセージを受信する可能性があります。 | `Async`    |
| Inflight Window        | 送信済みだが応答やアックをまだ受け取っていない「インフライトクエリ」の最大数を制御します。**Query Mode**が`async`の場合、同一MQTTクライアントのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`      |

## さらに詳しく

以下のリンクから詳細情報をご覧いただけます。

**ブログ**：

[1時間で構築するEMQX + InfluxDB + GrafanaによるIoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[MQTTデータをInfluxDBに統合した時系列IoTアプリケーションの構築](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
