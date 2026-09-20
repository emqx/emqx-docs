# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/)は時系列データの保存と分析に特化したデータベースです。高いデータスループット性能と安定した動作により、IoT分野での利用に非常に適しています。EMQXは現在、InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの主要なバージョンとの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ統合について、実践的な手順を交えて包括的に紹介します。

## 動作概要

InfluxDBデータ統合はEMQXの標準機能であり、EMQXのリアルタイムデータ取得・転送機能とInfluxDBのデータ保存・分析機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからInfluxDBへのデータ取り込みを簡素化し、複雑なコーディングなしで実現可能です。EMQXはルールエンジンとSinkを通じてデバイスデータをInfluxDBに転送し保存・分析します。InfluxDBは解析結果をレポートやグラフとして生成し、可視化ツールを通じてユーザーに提供します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとInfluxDBの典型的なデータ統合アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBは、エネルギー消費データをリアルタイムに効率的に収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャにおいて、EMQXはデバイスアクセス、メッセージ転送、データルーティングを担当するIoTプラットフォームとして機能し、InfluxDBはデータ保存・分析プラットフォームとして役割を担います。ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：エネルギー貯蔵デバイスや産業用IoTデバイスはMQTTプロトコルを介してEMQXに接続し、電力消費量や入出力電力などのエネルギーデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータ処理**：組み込みのルールエンジンを用いて、特定のトピックに基づくメッセージを処理します。メッセージが到着するとルールエンジンが該当ルールとマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、InfluxDBへの書き込み操作が実行されます。InfluxDB SinkはLine Protocolのテンプレートを提供し、メッセージ内の特定フィールドをInfluxDBのメジャメントやフィールドに柔軟にマッピング可能です。

エネルギー消費データがInfluxDBに書き込まれた後は、Line Protocolを活用してデータ解析が行えます。例えば：

- Grafanaなどの可視化ツールと連携し、エネルギーデータのグラフを生成・表示する。
- 業務システムと連携してエネルギー貯蔵デバイスの状態監視やアラート通知を行う。

## 特長とメリット

InfluxDBデータ統合は以下の特長と利点を提供します。

- **効率的なデータ処理**：EMQXは大量のIoTデバイス接続とメッセージスループットを処理可能であり、InfluxDBはデータ書き込み・保存・クエリに優れた性能を発揮します。IoTシナリオのデータ処理要件をシステムに負荷をかけずに満たします。
- **メッセージ変換**：EMQXのルールを通じて、InfluxDBへの書き込み前にメッセージを多様に処理・変換可能です。
- **スケーラビリティ**：EMQXとInfluxDBは共にクラスター拡張に対応し、ビジネスの成長に応じて柔軟に水平拡張できます。
- **豊富なクエリ機能**：InfluxDBは最適化された関数、演算子、インデックス技術を備え、時系列データの効率的なクエリと分析を実現し、IoTデータから価値ある洞察を抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコーディングを用い、ストレージコストを大幅に削減します。また、データ種別ごとに保存期間をカスタマイズ可能で、不要なデータの保存を回避します。

## はじめる前に

本節では、InfluxDBデータ統合の作成に先立ち必要な準備、特にInfluxDBのインストールと設定について説明します。

### 前提条件

- EMQXがInfluxDBへデータを書き込む際に従う[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXのデータ統合[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識

### InfluxDBのインストールと設定

1. Dockerを用いて[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDB Dockerイメージの起動
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、ブラウザで [http://localhost:8086](http://localhost:8086) にアクセスし、**Username**、**Password**、**Organization Name**、**Bucket Name**を設定します。
3. InfluxDBのUIで、**Load Data** -> **API Token**をクリックし、[全権限トークンの作成](https://docs.influxdata.com/influxdb/v2/install/#create-all-access-tokens)手順に従います。

## コネクターの作成

本節では、SinkをInfluxDBサーバーに接続するためのコネクター作成手順を示します。

以下の手順はEMQXとInfluxDBをローカルマシンで実行している前提です。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**InfluxDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します。

   以下の設定はすべてのInfluxDBバージョン共通です。

   - **Connector Name**：コネクターの一意な名前。英数字のみで例：`my_influxdb`
   - **Description**（任意）：コネクターの簡単な説明
   - **Server Host**：InfluxDBサーバーのアドレス。例：`127.0.0.1:8086`。InfluxDB Cloud利用時はポート`443`（例：`{url}:443`）を指定しTLSを有効にします。
   - **Version of InfluxDB**：利用するInfluxDBのバージョンを選択。`v1`、`v2`（デフォルト）、`v3`が選択可能。
   - **Enable TLS**：InfluxDBサーバーがTLS接続を要求する場合に有効化します。詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#enabling-tls-for-external-resource-access)を参照。

   選択したInfluxDBバージョンにより必要な設定項目は異なります。以下の表は[InfluxDBのインストールと設定](#install-and-set-up-influxdb)の内容と合わせて設定してください。

   | 設定項目               | InfluxDB v1           | InfluxDB v2   | InfluxDB v3   |
   | ---------------------- | --------------------- | ------------- | ------------- |
   | 認証方式               | ユーザー名 / パスワード | トークン       | トークン       |
   | **Token**              | -                     | 必須          | 必須          |
   | **Username**           | 任意                  | -             | -             |
   | **Password**           | 任意                  | -             | -             |
   | **Organization**       | -                     | 必須          | -             |
   | **Bucket**             | -                     | 必須          | -             |
   | **Database Name**      | 必須                  | -             | 必須          |

   注意：

   - **InfluxDB v1**ではEMQXは指定したデータベースに直接書き込み、ユーザー名／パスワード認証は任意です。
   - **InfluxDB v2**は組織とバケットモデルを使用し、トークンは指定バケットへの書き込み権限が必要です。
   - **InfluxDB v3**はv1に似たデータベースベースのモデルですが、トークン認証を用います。

5. **Create**をクリックする前に、**Test Connectivity**を押してEMQXがInfluxDBサーバーに正常に接続できるか確認できます。
6. **Create**をクリックしてコネクターの作成を完了します。

コネクター作成後は、**Back to Connector List**を選択するか、続けて**Create Rule**をクリックしてMQTTデータをInfluxDBに転送するルールとSinkを定義できます。詳細は[InfluxDB Sinkを用いたルール作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sinkを用いたルール作成

本節では、EMQXでMQTTトピック `t/#` からのメッセージを処理し、設定済みのSink経由でInfluxDBに送信するルールの作成手順を示します。

1. EMQXダッシュボードで左メニューから**Integration** -> **Rules**をクリックします。
2. ページ右上の**Create**をクリックします。
3. ルール作成ページで、ルールIDに `my_rule` と入力します。
4. **SQL Editor**でルールを設定します。例として、トピック `t/#` のMQTTメッセージをInfluxDBに保存する場合、以下のSQLを使用します。

   ::: tip

   独自のSQLを指定する場合は、後で設定するSinkのデータ形式で必要な変数が`SELECT`句に含まれていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   補足：初心者の場合は**SQL Examples**や**Enable Test**を利用してSQLルールを学習・テストできます。

5. + **Add Action**ボタンをクリックし、ルールがトリガーした際のアクションを定義します。このアクションでEMQXは処理済みデータをInfluxDBに送信します。
6. **Type of Action**ドロップダウンから`InfluxDB`を選択し、**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択可能ですが、本例では新規Sinkを作成します。
7. Sinkの名前を入力します。英数字の組み合わせが推奨されます。
8. **Connector**ドロップダウンから先に作成した`my_influxdb`を選択します。新規作成も可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。
9. **Time Precision**を指定します。デフォルトは`millisecond`です。
10. **Data Format**を`JSON`または`Line Protocol`から選択し、InfluxDBへのデータ解析・書き込み方法を指定します。

    - JSON形式の場合、**Measurement**、**Timestamp**、**Fields**、**Tags**などのデータ解析方法を定義します。すべてのキーは変数やプレースホルダーに対応し、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に準拠した設定も可能です。**Fields**はCSVファイルによる一括設定もサポートします（詳細は[一括設定](#batch-setting)参照）。
    - Line Protocol形式はテキストベースで、メジャメント、タグセット、フィールドセット、タイムスタンプを指定し、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に準拠したプレースホルダーを利用可能です。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型の値を書き込む場合は、プレースホルダーの後に`i`を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8で整数値を書き込む](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型の値を書き込む場合は、プレースホルダーの後に`u`を付けます。例：`${payload.int}u`。詳細は同上リンクを参照。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。プライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
12. **詳細設定（任意）**：詳細は[高度な設定](#advanced-configurations)を参照してください。
13. **Create**をクリックする前に、**Test Connectivity**でSinkがInfluxDBサーバーに接続可能かテストできます。
14. **Create**をクリックしてSink作成を完了します。ルール作成ページの**Action Outputs**タブに新規Sinkが表示されます。
15. ルール作成ページで設定内容を確認し、**Create**をクリックしてルールを生成します。

これでルールの作成が完了し、ルール一覧ページに新規ルールが表示されます。**Actions(Sink)**タブをクリックすると新規InfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**を開くとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析されInfluxDBに送信・保存されていることが確認できます。

### 一括設定

InfluxDBのデータエントリーは数百のフィールドを含むことが多く、データ形式の設定が煩雑になる場合があります。これを解決するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータ形式を設定する際、CSVファイルからフィールドのキー・バリューを一括インポート可能です。

1. **Fields**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。
2. 指示に従い、一括設定テンプレートファイルをダウンロードし、フィールドのキー・バリューを入力します。テンプレートのデフォルト内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | 値の後にiを付けてInfluxDBに整数として保存することを示す。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダー対応。
   - **Value**：フィールド値。定数またはプレースホルダー対応。Line Protocolに従い型識別子を付加可能。
   - **備考**：CSVファイル内の注釈用で、EMQXへのインポート対象外。

   CSVファイルの一括設定データは2048行を超えないようにしてください。

3. 入力済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックし一括設定を完了します。
4. インポート後、**Fields**設定テーブルでキー・バリューの微調整が可能です。

## ルールのテスト

MQTTクライアントMQTTXを使い、トピック `t/1` にメッセージを送信してオンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

InfluxDBのUIで**Data Explorer**ウィンドウを使い、メッセージがInfluxDBに書き込まれているか確認できます。

## 高度な設定

本節ではInfluxDBコネクターおよびSinkの高度な設定オプションについて詳述します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**を開き、以下のパラメータをニーズに合わせて調整してください。

| **項目**               | **説明**                                                                                                                   | **推奨値** |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------------- | ---------- |
| Start Timeout          | コネクターが自動起動したリソースの正常状態到達を待機する最大秒数。InfluxDBのデータベースインスタンスなどが完全に稼働し、データ処理可能になるまでの待機時間を指定します。 | `5`        |
| Buffer Pool Size       | EMQXとInfluxDB間の送信（egress）タイプのブリッジでデータフローを管理するバッファワーカー数。データ送信前の一時保存・処理を担当します。IngressのみのSinkでは`0`に設定可能です。 | `4`        |
| Request TTL            | バッファに入ったリクエストが有効とみなされる最大秒数。TTLを超えたリクエストやInfluxDBからの応答・アックが得られないリクエストは期限切れとなります。 | `45`       |
| Health Check Interval  | SinkがInfluxDB接続の自動ヘルスチェックを行う間隔（秒）です。                                                                 | `15`       |
| Max Buffer Queue Size  | 各バッファワーカーがInfluxDB Sinkでバッファリング可能な最大バイト数。データ送信前の一時保存容量を調整し、システム性能に合わせて設定します。 | `1`        |
| Max Batch Size         | EMQXからInfluxDBへ一度に転送可能なデータバッチの最大サイズ。サイズ調整により転送効率を最適化可能。`1`の場合はバッチ化せず個別送信となります。 | `100`      |
| Query Mode             | メッセージ送信の最適化のため、`asynchronous`（非同期）または`synchronous`（同期）を選択可能。非同期モードではInfluxDBへの書き込みがMQTTパブリッシュ処理をブロックしませんが、クライアントがInfluxDB到着前にメッセージを受信する可能性があります。 | `Async`    |
| Inflight Window        | SinkがInfluxDBと通信中に同時に存在可能な未応答クエリ（インフライトクエリ）の最大数。**Query Mode**が`async`の場合に重要で、同一MQTTクライアントのメッセージを厳密に順序処理したい場合は`1`に設定します。 | `100`      |

## さらに詳しく

以下のリンクもご参照ください。

**ブログ**：

[1時間で構築するEMQX＋InfluxDB＋GrafanaによるIoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[MQTTデータをInfluxDBに統合した時系列IoTアプリケーションの構築](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
