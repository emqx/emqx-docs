# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/)は時系列データの保存と分析に特化したデータベースです。高いデータスループット性能と安定した動作により、IoT分野での利用に非常に適しています。EMQXは現在、InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの主流バージョンとの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ統合について、実践的な手順を交えて包括的に解説します。

## 動作の仕組み

InfluxDBデータ統合は、EMQXに標準搭載された機能であり、EMQXのリアルタイムデータキャプチャと送信能力をInfluxDBのデータ保存・分析機能と組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからInfluxDBへのデータ取り込みが簡素化され、複雑なコーディングを不要にします。EMQXはルールエンジンとSinkを通じてデバイスデータをInfluxDBに転送し保存・分析します。InfluxDBは分析結果をレポートやグラフとして生成し、可視化ツールを通じてユーザーに提供します。

以下の図は、エネルギー蓄電シナリオにおけるEMQXとInfluxDBの典型的なデータ統合アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBは、リアルタイムでエネルギー消費データを効率的に収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがIoTプラットフォームとしてデバイス接続、メッセージ送受信、データルーティングを担い、InfluxDBがデータ保存・分析プラットフォームとして機能します。ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：蓄電デバイスや産業用IoTデバイスはMQTTプロトコルでEMQXに接続し、電力消費量、入出力電力などのデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：組み込みのルールエンジンを使い、特定のトピックに基づいてメッセージを処理します。メッセージはルールエンジンを通過し、対応するルールとマッチングされ、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理が行われます。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをInfluxDBに書き込む操作が実行されます。InfluxDB SinkはLine Protocolのテンプレートを提供し、メッセージの特定フィールドをInfluxDBの計測値やフィールドに柔軟にマッピングできます。

蓄電データがInfluxDBに書き込まれた後は、Line Protocolを活用して以下のような分析が可能です。

- Grafanaなどの可視化ツールに接続し、蓄電データを基にグラフを生成する。
- 業務システムに接続し、蓄電デバイスの状態監視やアラート通知を行う。

## 特長と利点

InfluxDBデータ統合は以下の特長と利点を提供します。

- **効率的なデータ処理**：EMQXは大量のIoTデバイス接続とメッセージスループットを処理でき、InfluxDBはデータ書き込み・保存・クエリに優れた性能を発揮し、IoTシナリオのデータ処理ニーズをシステムに負荷をかけずに満たします。
- **メッセージ変換**：EMQXのルールを通じてメッセージを多様に処理・変換した上でInfluxDBに書き込めます。
- **スケーラビリティ**：EMQXとInfluxDBはどちらもクラスター拡張に対応しており、ビジネスの成長に合わせて柔軟に水平拡張が可能です。
- **豊富なクエリ機能**：InfluxDBは最適化された関数、演算子、インデックス技術を備え、時系列データの効率的なクエリと分析を実現し、IoTデータから価値ある洞察を正確に抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコード方式を採用し、ストレージコストを大幅に削減します。また、データ種別ごとに保存期間をカスタマイズでき、不必要なデータのストレージ占有を防ぎます。

## はじめる前に

本節では、InfluxDBデータ統合の作成を始める前に必要な準備、特にInfluxDBのインストールと設定について説明します。

### 前提条件

- EMQXがInfluxDBにデータを書き込む際に従う[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXのデータ統合[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識

### InfluxDBのインストールとセットアップ

1. Docker経由で[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDBのDockerイメージを起動する
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、[http://localhost:8086](http://localhost:8086)にアクセスし、**ユーザー名**、**パスワード**、**組織名**、**バケット名**を設定します。
3. InfluxDBのUIで、**Load Data** -> **API Token**をクリックし、[全権限トークンの作成](https://docs.influxdata.com/influxdb/v2.5/install/#create-all-access-tokens)手順に従います。

## コネクターの作成

本節では、SinkをInfluxDBサーバーに接続するためのコネクター作成手順を示します。

以下の手順はEMQXとInfluxDBをローカルマシンで実行している前提です。リモート環境の場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックします。

3. **Create Connector**ページで**InfluxDB**を選択し、**Next**をクリックします。

4. **Configuration**ステップで以下を設定します：
   - コネクター名を入力します。英数字の組み合わせで、例：`my_influxdb`
   
   - **Server Host**に`127.0.0.1:8086`を入力します。InfluxDB Cloudを使う場合はポート443を指定し、`{url}:443`と入力して**Enable TLS**をオンにしてTLS接続を有効化します。

   - **InfluxDBのバージョン**を選択します。バージョンにより認証項目が異なります。以下の表を参照してください。InfluxDB v2の場合は[InfluxDBのインストールとセットアップ](#install-and-set-up-influxdb)で設定した組織名、バケット名、トークンを使用します。InfluxDB v1の場合はデータベース名と、設定されていればユーザー名・パスワードを入力します。
   
     | 項目           | InfluxDB v1 | InfluxDB v2 |
     | -------------- | ----------- | ----------- |
     | **Token**      | —           | 必須        |
     | **Username**   | 任意        | —           |
     | **Password**   | 任意        | —           |
     | **Organization** | —         | 必須        |
     | **Bucket**     | —           | 必須        |
     | **Database**   | 必須        | —           |
   
     - InfluxDB v1ではEMQXは指定データベースに直接書き込み、任意のユーザー名・パスワードで認証します。
     - InfluxDB v2では組織・バケットモデルを使用し、トークンは指定バケットへの書き込み権限を持つ必要があります。
   
   - **Ping With Auth**を切り替え、EMQXが`/ping`ヘルスチェックリクエストにコネクター認証情報を含めるか制御します。InfluxDBサービスが`/ping`に認証を要求する場合は有効にしてください。デフォルトは無効で、認証なしで`/ping`を送信します。
   
   - TLSを有効にするか設定します。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#enabling-tls-for-external-resource-access)を参照してください。
   
5. **Create**をクリックする前に、**Test Connectivity**でInfluxDBサーバーへの接続テストが可能です。

6. ページ下部の**Create**をクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択できます。ルールとSinkの作成手順は[InfluxDB Sink付きルールの作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sink付きルールの作成

本節では、EMQXでMQTTトピック`t/#`のメッセージを処理し、設定したSinkを通じてInfluxDBに送信するルールの作成方法を示します。

1. EMQXダッシュボードで左メニューの**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルール作成ページで、ルールIDに`my_rule`を入力します。

4. **SQL Editor**でルールを設定します。例えば、トピック`t/#`のMQTTメッセージをInfluxDBに保存する場合、以下のSQLを使用します。

   ::: tip

   独自のSQLを指定する場合、後で設定するSinkのデータ形式に含まれる変数が`SELECT`句に含まれていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注：初心者の方は**SQL Examples**と**Enable Test**を使ってSQLルールを学習・テストできます。

5. + **Add Action**ボタンをクリックし、ルールがトリガーするアクションを定義します。このアクションでEMQXはルール処理済みデータをInfluxDBに送信します。

6. **Type of Action**ドロップダウンから`InfluxDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既存のSinkを選択することも可能ですが、本例では新規Sinkを作成します。

7. Sink名を入力します。英数字の組み合わせで指定してください。

8. **Connector**ドロップダウンから先に作成した`my_influxdb`を選択します。新規コネクターはドロップダウン横のボタンで作成可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

9. **Time Precision**を指定します。デフォルトは`millisecond`です。

10. **Data Format**を`JSON`または`Line Protocol`から選択し、InfluxDBへのデータ解析・書き込み方法を指定します。

    - JSON形式の場合、**Measurement**、**Timestamp**、**Fields**、**Tags**のデータ解析方法を定義します。すべてのキー値は変数やプレースホルダーが使え、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に準拠して設定可能です。**Fields**はCSVファイルによる一括設定もサポートします。詳細は[一括設定](#batch-setting)を参照してください。
    - Line Protocol形式の場合、テキストベースで計測値、タグセット、フィールドセット、タイムスタンプを指定し、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に従ったプレースホルダーを利用できます。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型値を書き込む場合、プレースホルダーの後に`i`を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8 整数値の書き込み](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型値の場合は`u`を付けます。例：`${payload.int}u`。詳細は同上リンクを参照してください。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：詳細は[高度な設定](#advanced-configurations)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**でSinkからInfluxDBサーバーへの接続テストが可能です。

14. **Create**をクリックしてSink作成を完了します。ルール作成ページの**Action Outputs**タブに新しいSinkが表示されます。

15. ルール作成ページで設定内容を確認し、**Create**をクリックしてルールを生成します。

これでルールが作成され、**Rule**ページに新規ルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいInfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**でトポロジーを確認できます。トピック`t/#`のメッセージがルール`my_rule`で解析され、InfluxDBに送信・保存されていることがわかります。

### 一括設定

InfluxDBのデータエントリーは数百のフィールドを含むことが多く、データ形式の設定が複雑です。これを解決するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータ形式を設定する際、CSVファイルからフィールドのキー・バリューを一括インポートできます。

1. **Fields**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従い、一括設定テンプレートファイルをダウンロードし、フィールドのキー・バリューを入力します。テンプレートのデフォルト内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | フィールド値の後に`i`を付けてInfluxDBに整数として保存指示。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、Line Protocolに従い型識別子を付加可能。
   - **備考**：CSV内の注釈用で、EMQXへのインポートには反映されません。

   CSVファイルの一括設定データは2048行を超えないようにしてください。

3. 入力済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックし一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・バリューをさらに調整可能です。

## ルールのテスト

MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージを送信してオンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

InfluxDBのUIの**Data Explorer**ウィンドウで、メッセージがInfluxDBに書き込まれていることを確認できます。

## 高度な設定

本節ではInfluxDBコネクターおよびSinkの高度な設定オプションについて詳述します。ダッシュボードでコネクター・Sinkを設定する際、**Advanced Settings**に進み、以下のパラメータをニーズに合わせて調整してください。

| **項目**               | **説明**                                                                                                                   | **推奨値** |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------------- | ---------- |
| Start Timeout          | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数です。InfluxDBのデータベースインスタンスなど、接続先リソースが完全に稼働し準備完了になるまで操作を進めないための設定です。 | `5`        |
| Buffer Pool Size       | EMQXとInfluxDB間の送信タイプブリッジでデータフローを管理するバッファワーカープロセス数です。これらのワーカーは送信前にデータを一時保存・処理します。IngressのみのSinkでは無効で、`0`に設定可能です。 | `4`        |
| Request TTL            | バッファに入ったリクエストが有効とみなされる最大秒数です。TTLを超えるか、InfluxDBからの応答やアックがタイムリーに得られない場合、リクエストは期限切れと判断されます。 | `45`       |
| Health Check Interval  | SinkがInfluxDB接続のヘルスチェックを自動実行する間隔（秒）です。                                                               | `15`       |
| Max Buffer Queue Size  | 各バッファワーカーがInfluxDB Sinkでバッファリング可能な最大バイト数です。データ送信前の一時保存容量を示し、システム性能や転送要件に応じて調整してください。 | `1`        |
| Max Batch Size         | EMQXからInfluxDBへ単一転送操作で送信可能な最大データバッチサイズです。サイズ調整により転送効率・性能を最適化できます。`1`の場合はバッチ化せず個別送信となります。 | `100`      |
| Query Mode             | メッセージ送信を最適化するため、`asynchronous`（非同期）または`synchronous`（同期）モードを選択します。非同期モードではInfluxDBへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがInfluxDB到達前にメッセージを受け取る可能性があります。 | `Async`    |
| Inflight Window        | 送信済みだが応答やアックをまだ受け取っていない「インフライトクエリ」の最大数を制御します。<br/>**Query Mode**が`async`の時に重要で、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`      |

## さらに詳しく

以下のリンクから詳細情報をご覧いただけます。

**ブログ**：

[1時間で構築するEMQX + InfluxDB + Grafana IoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[時系列IoTアプリケーションのためのMQTTデータのInfluxDB統合](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
