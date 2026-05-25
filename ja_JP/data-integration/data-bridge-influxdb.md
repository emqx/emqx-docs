# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/)は時系列データの保存と分析に特化したデータベースです。高いデータスループット性能と安定した動作により、IoT分野での利用に非常に適しています。EMQXは現在、InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの主流バージョンとの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ統合について、実践的な手順を交えながら包括的に紹介します。

## 動作概要

InfluxDBデータ統合はEMQXに標準搭載された機能であり、EMQXのリアルタイムデータキャプチャおよび転送能力とInfluxDBのデータ保存・分析機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからInfluxDBへのデータ取り込みが簡素化され、複雑なコーディングを不要にします。EMQXはルールエンジンとSinkを介してデバイスデータをInfluxDBに転送し保存・分析を行います。InfluxDBは分析結果をレポートやチャートなどで生成し、InfluxDBの可視化ツールを通じてユーザーに提供します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとInfluxDBの典型的なデータ統合アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBは、エネルギー消費データをリアルタイムに効率的に収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがIoTプラットフォームとしてデバイス接続、メッセージ転送、データルーティングを担当し、InfluxDBがデータ保存・分析プラットフォームとして機能します。ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：エネルギー貯蔵装置や産業用IoTデバイスはMQTTプロトコルでEMQXに接続し、電力消費量、入出力電力などのエネルギーデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータ処理**：組み込みのルールエンジンを用いて、特定のトピックに基づくメッセージを処理します。メッセージが到着するとルールエンジンで該当ルールとマッチングされ、データフォーマットの変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、InfluxDBへの書き込み操作が実行されます。InfluxDB SinkはLine Protocolテンプレートを提供し、メッセージの特定フィールドをInfluxDBの対応するmeasurementやfieldに柔軟にマッピング可能です。

エネルギー消費データがInfluxDBに書き込まれた後、Line Protocolを活用して以下のような分析が可能です。

- Grafanaなどの可視化ツールと連携し、エネルギーデータのチャートを生成・表示
- 業務システムと連携し、エネルギー貯蔵装置の状態監視やアラート発報

## 特長と利点

InfluxDBデータ統合の主な特長と利点は以下の通りです。

- **効率的なデータ処理**：EMQXは大量のIoTデバイス接続およびメッセージスループットを処理可能であり、InfluxDBはデータ書き込み・保存・クエリに優れた性能を発揮します。これにより、IoTシナリオのデータ処理要件をシステムに過度な負荷をかけずに満たせます。
- **メッセージ変換**：EMQXのルールを通じてメッセージの高度な処理・変換が可能であり、InfluxDBへの書き込み前にデータを最適化できます。
- **スケーラビリティ**：EMQXとInfluxDBはともにクラスター対応であり、ビジネスの成長に応じて柔軟に水平スケールが可能です。
- **豊富なクエリ機能**：InfluxDBは最適化された関数、演算子、インデックス技術を備え、時系列データの効率的なクエリと分析を実現し、IoTデータから有益なインサイトを抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコード方式を採用し、ストレージコストを大幅に削減します。また、データ種別ごとに保存期間をカスタマイズ可能で、不要なデータのストレージ占有を防止します。

## はじめる前に

本節では、InfluxDBデータ統合を作成する前に必要な準備、特にInfluxDBのインストールと設定について説明します。

### 前提条件

- EMQXがInfluxDBにデータを書き込む際に従う[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### InfluxDBのインストールと設定

1. Dockerを用いて[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDB Dockerイメージの起動
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、ブラウザで [http://localhost:8086](http://localhost:8086) にアクセスし、**ユーザー名**、**パスワード**、**組織名**、**バケット名**を設定します。
3. InfluxDB UIで **Load Data** -> **API Token** をクリックし、[全アクセス権限トークンの作成](https://docs.influxdata.com/influxdb/v2.5/install/#create-all-access-tokens)手順に従います。

## コネクターの作成

本節では、SinkをInfluxDBサーバーに接続するためのコネクター作成手順を示します。

以下の手順は、EMQXとInfluxDBをローカルマシンで動作させていることを前提としています。リモート環境の場合は適宜設定を調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックします。

3. **Create Connector**ページで**InfluxDB**を選択し、**Next**をクリックします。

4. **Configuration**ステップで以下を設定します：
   - コネクター名を入力します。英数字の組み合わせで、例：`my_influxdb`
   
   - **Server Host**に`127.0.0.1:8086`を入力します。InfluxDB Cloudを利用する場合はポート443を指定し、例：`{url}:443`、**Enable TLS**をオンにしてTLS接続を有効化します。

   - **InfluxDBのバージョン**を選択します。バージョンによって認証項目が異なります。以下の表を参照してください。InfluxDB v2の場合は[InfluxDBのインストールと設定](#install-and-set-up-influxdb)で設定した組織名、バケット名、トークンを使用します。InfluxDB v1の場合はデータベース名と、設定されていればそのデータベースのユーザー名とパスワードを入力します。
   
     | 項目           | InfluxDB v1 | InfluxDB v2 |
     | -------------- | ----------- | ----------- |
     | **Token**      | —           | 必須        |
     | **Username**   | 任意        | —           |
     | **Password**   | 任意        | —           |
     | **Organization** | —         | 必須        |
     | **Bucket**     | —           | 必須        |
     | **Database**   | 必須        | —           |
   
     - InfluxDB v1では、EMQXは指定したデータベースに直接書き込み、任意のユーザー名・パスワードで認証します。
     - InfluxDB v2では、組織とバケットのモデルを使用し、トークンは指定バケットへの書き込み権限を持つ必要があります。
   
   - **Ping With Auth**の切り替えで、EMQXが`/ping`ヘルスチェックリクエストに認証情報を含めるか制御します。InfluxDBサービスが`/ping`に認証を要求する場合は有効にします。デフォルトは無効で、認証なしで`/ping`を送信します。
   
   - TLS接続の有効化を設定します。詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#enabling-tls-for-external-resource-access)を参照してください。
   
5. **Create**をクリックする前に、**Test Connectivity**でInfluxDBサーバーへの接続テストが可能です。

6. ページ下部の**Create**をクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択できます。ルールとSinkの作成手順は[InfluxDB Sinkを使ったルール作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sinkを使ったルール作成

本節では、EMQXでMQTTトピック`t/#`からのメッセージを処理し、設定済みのSinkを介してInfluxDBに送信するルールの作成方法を示します。

1. EMQXダッシュボードの左メニューから**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルール作成ページで、ルールIDに`my_rule`を入力します。

4. **SQL Editor**でルールを設定します。例えば、トピック`t/#`のMQTTメッセージをInfluxDBに保存したい場合、以下のSQLを使用します。

   ::: tip

   独自のSQLを指定する場合は、後で設定するSinkのデータフォーマットに含まれるすべての変数が`SELECT`句に含まれていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注：初心者の方は**SQL Examples**や**Enable Test**を使ってSQLルールの学習とテストが可能です。

5. + **Add Action**ボタンをクリックし、ルールがトリガーするアクションを定義します。このアクションでEMQXはルール処理済みデータをInfluxDBに送信します。

6. **Type of Action**ドロップダウンから`InfluxDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkを選択することも可能ですが、本例では新規Sinkを作成します。

7. Sink名を入力します。英数字の組み合わせで指定してください。

8. **Connector**ドロップダウンから先に作成した`my_influxdb`を選択します。新規コネクター作成は隣のボタンから可能です。設定パラメータは[コネクター作成](#create-a-connector)を参照してください。

9. **Time Precision**を指定します。デフォルトは`millisecond`です。

10. **Data Format**を`JSON`または`Line Protocol`から選択し、InfluxDBへのデータ解析・書き込み方法を指定します。

    - JSON形式の場合、**Measurement**、**Timestamp**、**Fields**、**Tags**の解析方法を定義します。すべてのキー値は変数やプレースホルダーで指定可能で、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に準拠して設定できます。**Fields**はCSVファイルによる一括設定も可能です（詳細は[一括設定](#batch-setting)参照）。
    - Line Protocol形式の場合、InfluxDB line protocolの構文に従い、measurement、タグセット、フィールドセット、タイムスタンプをテキスト形式で指定します。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型を送信する場合、プレースホルダーの後に`i`を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8 整数値書き込み](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型の場合は`u`を付けます。例：`${payload.int}u`。詳細は同上リンク参照。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：[詳細設定](#advanced-configurations)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**でSinkからInfluxDBサーバーへの接続テストが可能です。

14. **Create**をクリックしてSink作成を完了します。ルール作成ページの**Action Outputs**タブに新規Sinkが表示されます。

15. ルール作成ページで設定内容を確認し、**Create**をクリックしてルールを生成します。

これでルールが作成され、ルール一覧ページに新規ルールが表示されます。**Actions(Sink)**タブをクリックすると、新規InfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**を開くとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されInfluxDBに送信・保存されていることが確認できます。

### 一括設定

InfluxDBのデータエントリは通常数百のフィールドを含むため、データフォーマット設定が煩雑になりがちです。これを解決するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータフォーマットを設定する際、CSVファイルからフィールドのキー・値ペアを一括インポートできます。

1. **Fields**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従い、一括設定テンプレートファイルをダウンロードし、テンプレートにフィールドのキー・値ペアを記入します。テンプレートのデフォルト内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | フィールド値に`i`を付加し、InfluxDBに整数として保存する指定 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、line protocolに従い型識別子を付加可能。
   - **備考**：CSV内の注釈用で、EMQXへのインポート対象外。

   CSVファイルの行数は2048行を超えないようにしてください。

3. 記入済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロード後、**Import**をクリックして一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・値ペアをさらに調整可能です。

## ルールのテスト

MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージを送信してオンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

InfluxDB UIの**Data Explorer**ウィンドウで、メッセージがInfluxDBに書き込まれているか確認できます。

## 詳細設定

本節では、InfluxDBコネクターおよびSinkの詳細設定項目について説明します。ダッシュボードでコネクター・Sinkを設定する際、**Advanced Settings**を開き、以下のパラメータをニーズに応じて調整してください。

| **項目**               | **説明**                                                                                                                 | **推奨値** |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------ | ---------- |
| Start Timeout          | コネクターが自動起動したリソース（例：InfluxDBのデータベースインスタンス）が正常状態になるまで待機する最大秒数。リソース作成要求に応答する前に、接続先リソースが完全に稼働していることを確認するための設定。 | `5`        |
| Buffer Pool Size       | EMQXとInfluxDB間の送信タイプブリッジでデータフロー管理に割り当てるバッファワーカープロセス数。データ送信前の一時的な保管・処理を担う。受信のみのSinkでは不要なため`0`に設定可能。 | `4`        |
| Request TTL            | バッファに入ったリクエストの有効期限（秒）。この期間を超えて応答やアックがない場合、リクエストは期限切れとみなされる。 | `45`       |
| Health Check Interval  | SinkがInfluxDBとの接続状態を自動的にヘルスチェックする間隔（秒）。 | `15`       |
| Max Buffer Queue Size  | 各バッファワーカーがInfluxDB Sinkでバッファリング可能な最大バイト数。システム性能やデータ転送要件に応じて調整。 | `1`        |
| Max Batch Size         | EMQXからInfluxDBへ一度に転送可能なデータバッチの最大サイズ。`1`に設定すると単一レコードずつ送信される。 | `100`      |
| Query Mode             | メッセージ送信の最適化のため、`asynchronous`（非同期）または`synchronous`（同期）を選択可能。非同期モードではInfluxDBへの書き込みがMQTTメッセージのパブリッシュ処理をブロックしないが、クライアントがInfluxDB書き込み前にメッセージを受信する可能性がある。 | `Async`    |
| Inflight Window        | SinkがInfluxDBと通信中に同時に存在可能な未応答・未アックのクエリ数。**Query Mode**が`async`の場合に重要で、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定する。 | `100`      |

## 参考情報

以下のリンクからさらに詳細を学べます。

**ブログ**：

[1時間で構築するEMQX + InfluxDB + GrafanaによるIoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[MQTTデータをInfluxDBに統合した時系列IoTアプリケーション構築](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
