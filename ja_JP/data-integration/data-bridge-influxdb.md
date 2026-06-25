# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/)は時系列データの保存と分析に特化したデータベースです。高いデータスループット性能と安定した動作により、IoT（モノのインターネット）分野での利用に非常に適しています。EMQXは現在、InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの主流バージョンとの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ統合について、実践的な手順を交えて包括的に解説します。

## 動作概要

InfluxDBデータ統合はEMQXの標準機能であり、EMQXのリアルタイムデータキャプチャと転送機能をInfluxDBのデータ保存・分析機能と組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからInfluxDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。EMQXはルールエンジンとSinkを介してデバイスデータをInfluxDBに転送し保存・分析を行います。InfluxDBは分析結果をレポートやチャートなどの形で生成し、InfluxDBの可視化ツールを通じてユーザーに提供します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとInfluxDBの典型的なデータ統合アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBは、エネルギー消費データをリアルタイムに効率的に収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがIoTプラットフォームとしてデバイス接続、メッセージ送受信、データルーティングを担当し、InfluxDBがデータ保存と分析の役割を担います。ワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：エネルギー貯蔵機器や産業用IoT機器はMQTTプロトコルを用いてEMQXに接続し、消費電力、入出力電力などのエネルギーデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：組み込みのルールエンジンを用いて、特定のトピックに基づくメッセージを処理します。メッセージが到着するとルールエンジンで対応するルールにマッチし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、InfluxDBへの書き込み処理が実行されます。InfluxDB SinkはLine Protocolテンプレートを提供し、メッセージ内の特定フィールドをInfluxDBのメジャメントやフィールドに柔軟にマッピング可能です。

エネルギー消費データがInfluxDBに書き込まれた後は、Line Protocolを活用して柔軟にデータ分析が可能です。例えば：

- Grafanaなどの可視化ツールと連携し、エネルギー貯蔵データのチャートを生成・表示する。
- 業務システムと連携し、エネルギー貯蔵機器の状態監視やアラート通知を行う。

## 特長とメリット

InfluxDBデータ統合が提供する主な特長と利点は以下の通りです：

- **効率的なデータ処理**：EMQXは膨大な数のIoTデバイス接続とメッセージスループットを処理可能であり、InfluxDBは高速なデータ書き込み、保存、クエリ性能に優れています。これによりIoTシナリオのデータ処理要件をシステムに過度な負荷をかけずに満たせます。
- **メッセージ変換**：EMQXのルールを通じてメッセージを多様に処理・変換してからInfluxDBに書き込めます。
- **スケーラビリティ**：EMQXとInfluxDBの両方がクラスター拡張に対応し、ビジネスの成長に応じた柔軟な水平スケールが可能です。
- **豊富なクエリ機能**：InfluxDBは時系列データに最適化された関数、演算子、インデックス技術を備え、効率的なクエリと分析を実現し、IoT時系列データから価値ある洞察を正確に抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコーディング方式を採用し、ストレージコストを大幅に削減します。また、データタイプごとに保存期間をカスタマイズ可能で、不要なデータのストレージ占有を防止します。

## はじめる前に

このセクションでは、InfluxDBデータ統合作成前に必要な準備、特にInfluxDBのインストールとセットアップについて説明します。

### 前提条件

- EMQXがInfluxDBにデータを書き込む際に従う[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### InfluxDBのインストールとセットアップ

1. Dockerを用いて[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDB Dockerイメージの起動
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、ブラウザで [http://localhost:8086](http://localhost:8086) にアクセスし、**ユーザー名**、**パスワード**、**組織名**、**バケット名**を設定します。
3. InfluxDBのUIで **Load Data** -> **API Token** をクリックし、[全権限トークンの作成](https://docs.influxdata.com/influxdb/v2.5/install/#create-all-access-tokens)手順に従ってトークンを作成します。

## コネクターの作成

このセクションでは、SinkをInfluxDBサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとInfluxDBをローカルマシンで実行していることを前提としています。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **InfluxDB** を選択し、**Next** をクリックします。

4. **Configuration** ステップで以下の情報を設定します：
   - コネクター名を入力します。英数字の組み合わせで、例：`my_influxdb`。
   
   - **Server Host** に `127.0.0.1:8086` を入力します。InfluxDB Cloudを使用する場合はポート443を指定し、`{url}:443` と入力し、**Enable TLS** を有効にしてTLS接続を有効化します。

   - **InfluxDBのバージョン**を選択します。バージョンにより認証項目が異なります。以下の表を参照してください。InfluxDB v2では[インストールとセットアップ](#install-and-set-up-influxdb)で設定した組織名、バケット名、トークンを使用します。InfluxDB v1ではデータベース名と、設定している場合はユーザー名・パスワードを入力します。
   
     | 項目           | InfluxDB v1 | InfluxDB v2 |
     | -------------- | ----------- | ----------- |
     | **Token**      | —           | 必須        |
     | **Username**   | 任意        | —           |
     | **Password**   | 任意        | —           |
     | **Organization** | —         | 必須        |
     | **Bucket**     | —           | 必須        |
     | **Database**   | 必須        | —           |
   
     - InfluxDB v1では、EMQXは指定したデータベースに直接書き込み、任意のユーザー名・パスワードで認証します。
     - InfluxDB v2では、組織・バケットモデルを使用し、トークンは指定バケットへの書き込み権限を持つ必要があります。
   
   - **Ping With Auth** の切り替えで、EMQXが`/ping`ヘルスチェックリクエストに認証情報を含めるかを制御します。InfluxDBサービスが`/ping`に認証を要求する場合は有効にします。デフォルトは無効で、認証なしで`/ping`を送信します。
   
   - TLSを有効にするかどうかを設定します。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#enabling-tls-for-external-resource-access)を参照してください。
   
5. **Create**をクリックする前に、**Test Connectivity**を押してInfluxDBサーバーへの接続テストを行えます。

6. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてルールとSinkの作成に進めます。詳細は[InfluxDB Sink付きルールの作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sink付きルールの作成

このセクションでは、EMQXでMQTTトピック `t/#` のメッセージを処理し、設定済みのSinkを通じてInfluxDBに送信するルールの作成方法を説明します。

1. EMQXダッシュボードで左メニューの **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール作成ページでルールIDに `my_rule` と入力します。

4. **SQL Editor**でルールを設定します。例えば、トピック `t/#` のMQTTメッセージをInfluxDBに保存する場合、以下のSQL文を使用します。

   ::: tip

   独自のSQL文を指定する場合は、後で設定するSinkのデータ形式に含まれるすべての変数が`SELECT`句で選択されていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注：初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールを学習・テストできます。

5. + **Add Action** ボタンをクリックし、ルールがトリガーするアクションを定義します。このアクションでEMQXはルールで処理したデータをInfluxDBに送信します。

6. **Type of Action**ドロップダウンから `InfluxDB` を選択します。**Action**はデフォルトの `Create Action` のままにします。既に作成済みのSinkがあれば選択可能ですが、ここでは新規Sinkを作成します。

7. Sinkの名前を入力します。英数字の組み合わせで指定してください。

8. **Connector**ドロップダウンから先に作成した `my_influxdb` を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックします。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

9. **Time Precision**を指定します。デフォルトは `millisecond` です。

10. **Data Format**を `JSON` または `Line Protocol` から選択し、InfluxDBへのデータ解析・書き込み方法を指定します。

    - JSON形式の場合、**Measurement**、**Timestamp**、**Fields**、**Tags**のデータ解析方法を定義します。すべてのキー値は変数やプレースホルダーを指定可能で、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に準拠して設定できます。**Fields**はCSVファイルによる一括設定も可能です。詳細は[一括設定](#batch-setting)を参照してください。
    - Line Protocol形式の場合、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に従い、メジャメント、タグセット、フィールドセット、タイムスタンプをテキスト形式で指定します。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型の値を書き込む場合、プレースホルダーの後に`i`を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8の整数値書き込み](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型の場合は`u`を付けます。例：`${payload.int}u`。詳細は同上リンクを参照してください。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、プライマリSinkが処理できなかった場合にトリガーされるフォールバックアクションを1つ以上定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：詳細は[高度な設定](#advanced-configurations)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**でSinkがInfluxDBサーバーに接続可能かテストできます。

14. **Create**をクリックしてSink作成を完了します。ルール作成ページの**Action Outputs**タブに新規Sinkが表示されます。

15. ルール作成ページで設定内容を確認し、**Create**ボタンをクリックしてルールを生成します。

これでルールの作成が完了し、**Rule**ページに新規ルールが表示されます。**Actions(Sink)**タブをクリックすると、新規InfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**を開くとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析されInfluxDBに送信・保存されていることが確認できます。

### 一括設定

InfluxDBのデータエントリは通常数百のフィールドを含むため、データ形式設定が複雑になることがあります。これに対応するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータ形式を設定する際、CSVファイルからフィールドのキー・値ペアを一括インポートできます。

1. **Fields**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従い、一括設定テンプレートファイルをダウンロードし、テンプレート内にフィールドのキー・値ペアを記入します。デフォルトのテンプレート内容は以下の通りです：

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | フィールド値に `i` を付けて整数としてInfluxDBに保存する。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、line protocolに従い型識別子を付加可能。
   - **備考**：CSV内の注釈用で、EMQXへのインポート対象外。

   CSVファイルの行数は2048行を超えないようにしてください。

3. 記入済みテンプレートを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックし一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・値ペアをさらに調整可能です。

## ルールのテスト

MQTTクライアントMQTTXを使い、トピック `t/1` にメッセージを送信してオンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、1件の新規受信メッセージと1件の新規送信メッセージがあるはずです。

InfluxDBのUIの**Data Explorer**ウィンドウで、メッセージがInfluxDBに書き込まれていることを確認できます。

## 高度な設定

このセクションでは、InfluxDBコネクターとSinkの高度な設定オプションについて詳述します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**に進み、以下のパラメータをニーズに合わせて調整できます。

| **項目**               | **説明**                                                                                                                        | **推奨値** |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------- | ---------- |
| Start Timeout          | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数です。この設定により、InfluxDBのデータベースインスタンスなどのリソースが完全に稼働し、データ処理準備が整うまでコネクターが操作を進めないようにします。 | `5`        |
| Buffer Pool Size       | EMQXとInfluxDB間の送信（egress）タイプのブリッジでデータフローを管理するバッファーワーカープロセス数を指定します。これらのワーカーはデータを一時的に保持し、送信前の処理を行います。Ingress（受信）専用のSinkでは「0」に設定可能です。 | `4`        |
| Request TTL            | バッファに入ったリクエストが有効とみなされる最大秒数です。バッファリング開始時からカウントし、TTLを超えたリクエストやInfluxDBから応答・アックが得られないリクエストは期限切れと判断されます。 | `45`       |
| Health Check Interval  | SinkがInfluxDBへの接続状態を自動的にヘルスチェックする間隔（秒）です。                                                            | `15`       |
| Max Buffer Queue Size  | InfluxDB Sinkの各バッファーワーカーが保持可能な最大バイト数です。バッファーワーカーはデータ送信前の一時保管を担い、システム性能やデータ転送要件に応じて調整します。 | `1`        |
| Max Batch Size         | EMQXからInfluxDBへ一度に転送可能なデータバッチの最大サイズです。サイズ調整によりデータ転送の効率と性能を最適化できます。`1`に設定するとデータを個別に送信します。 | `100`      |
| Query Mode             | メッセージ送信を最適化するため、`asynchronous`（非同期）または`synchronous`（同期）モードを選択できます。非同期モードではInfluxDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしませんが、クライアントがInfluxDBへの書き込み前にメッセージを受信する可能性があります。 | `Async`    |
| Inflight Window        | 「インフライトクエリ」とは、開始されたがまだ応答やアックを受けていないクエリのことです。SinkがInfluxDBと通信する際に同時に存在可能なインフライトクエリの最大数を制御します。<br/>**Query Mode**が`async`の場合、この設定は特に重要です。同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`      |

## 参考情報

以下のリンクもご参照ください：

**ブログ**：

[1時間で構築するEMQX + InfluxDB + Grafana IoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[時系列IoTアプリケーション向けMQTTデータのInfluxDB統合](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
