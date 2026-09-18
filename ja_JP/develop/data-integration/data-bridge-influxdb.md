# InfluxDBへのMQTTデータ取り込み

[InfluxDB](https://www.influxdata.com/)は時系列データの保存と分析に特化したデータベースです。高いデータスループット性能と安定した動作により、IoT分野での利用に非常に適しています。EMQXは現在、InfluxDB Cloud、InfluxDB OSS、InfluxDB Enterpriseの主要なバージョンとの接続をサポートしています。

本ページでは、EMQXとInfluxDB間のデータ統合について、実践的な手順を交えて包括的に解説します。

## 動作概要

InfluxDBデータ統合はEMQXの標準機能であり、EMQXのリアルタイムデータ取得・転送機能とInfluxDBのデータ保存・分析機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、EMQXからInfluxDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。EMQXはルールエンジンとSinkを介してデバイスデータをInfluxDBに転送し保存・分析します。InfluxDBは分析結果をレポートやグラフとして生成し、InfluxDBの可視化ツールを通じてユーザーに提供します。

以下の図は、エネルギー貯蔵シナリオにおけるEMQXとInfluxDBの典型的なデータ統合アーキテクチャを示しています。

![MQTT to InfluxDB](./assets/mqtt-to-influxdb.jpg)

EMQXとInfluxDBは、エネルギー消費データをリアルタイムに効率よく収集・分析するための拡張可能なIoTプラットフォームを提供します。このアーキテクチャでは、EMQXがIoTプラットフォームとしてデバイス接続、メッセージ転送、データルーティングを担い、InfluxDBがデータ保存・分析プラットフォームとして機能します。ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：エネルギー貯蔵装置や産業用IoTデバイスはMQTTプロトコルでEMQXに接続し、電力消費量や入出力電力などのデータを定期的にパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：組み込みのルールエンジンを用いて、特定のトピックに基づくメッセージを処理します。メッセージが到着するとルールエンジンが該当ルールとマッチングし、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
3. **InfluxDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、InfluxDBへの書き込み処理が実行されます。InfluxDB SinkはLine Protocolのテンプレートを提供し、メッセージの特定フィールドをInfluxDBの計測値やフィールドに柔軟にマッピング可能です。

エネルギー消費データがInfluxDBに書き込まれた後は、Line Protocolを活用して以下のような分析が可能です。

- Grafanaなどの可視化ツールに接続し、エネルギー貯蔵データのグラフを生成
- 業務システムに連携し、エネルギー貯蔵装置の状態監視やアラート発報を実現

## 特長と利点

InfluxDBデータ統合は以下の特長と利点を提供します。

- **高効率なデータ処理**：EMQXは大量のIoTデバイス接続とメッセージスループットを処理可能であり、InfluxDBは高速な書き込み・保存・クエリ性能を持つため、IoTシナリオのデータ処理要件をシステム負荷を抑えつつ満たします。
- **メッセージ変換**：EMQXのルールを通じてメッセージは多様な処理・変換が可能であり、InfluxDBへの書き込み前に柔軟にデータを整形できます。
- **スケーラビリティ**：EMQXとInfluxDBは共にクラスター対応であり、ビジネス成長に応じてクラスターの水平拡張が可能です。
- **豊富なクエリ機能**：InfluxDBは最適化された関数、演算子、インデックス技術を備え、時系列データの効率的なクエリと分析を実現し、IoTデータから有用な知見を抽出します。
- **効率的なストレージ**：InfluxDBは高圧縮率のエンコード方式を採用し、ストレージコストを大幅に削減します。また、データ種別ごとに保存期間をカスタマイズ可能で、不要なデータによる容量圧迫を防止します。

## はじめる前に

本節では、InfluxDBデータ統合の作成を開始する前に必要な準備、特にInfluxDBのインストールとセットアップについて説明します。

### 前提条件

- EMQXがInfluxDBにデータを書き込む際に従う[InfluxDB Line Protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)の知識
- EMQXのデータ統合に関する[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識

### InfluxDBのインストールとセットアップ

1. Docker経由で[InfluxDBをインストール](https://docs.influxdata.com/influxdb/v2.5/install/)し、Dockerイメージを起動します。

```bash
# InfluxDBのDockerイメージを起動
docker run --name influxdb -p 8086:8086 influxdb:2.5.1
```

2. InfluxDBが起動したら、ブラウザで[http://localhost:8086](http://localhost:8086)にアクセスし、**ユーザー名**、**パスワード**、**組織名**、**バケット名**を設定します。
3. InfluxDBのUIで、**Load Data** -> **API Token**をクリックし、[全アクセス権限トークンの作成](https://docs.influxdata.com/influxdb/v2.5/install/#create-all-access-tokens)手順に従ってトークンを作成します。

## コネクターの作成

本節では、SinkをInfluxDBサーバーに接続するためのコネクター作成方法を示します。

以下の手順は、EMQXとInfluxDBを同一マシンで実行していることを前提としています。リモート環境の場合は適宜設定を調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。

2. 画面右上の**Create**をクリックします。

3. **Create Connector**画面で**InfluxDB**を選択し、**Next**をクリックします。

4. **Configuration**ステップで以下を設定します。
   - コネクター名を入力します。英数字の大文字・小文字の組み合わせで、例：`my_influxdb`
   - **Server Host**に`127.0.0.1:8086`を入力します。InfluxDB Cloudを利用する場合はポート443を指定し、`{url}:443`と入力して**Enable TLS**を有効にします。
   - **InfluxDBのバージョン**を選択します。バージョンによって認証項目が異なります。以下の表を参照してください。InfluxDB v2の場合は[InfluxDBのセットアップ](#install-and-set-up-influxdb)で設定した組織名、バケット名、トークンを使用します。InfluxDB v1の場合はデータベース名と、設定されていればユーザー名・パスワードを入力します。

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
   
   - **Ping With Auth**を切り替え、EMQXが`/ping`ヘルスチェックリクエストに認証情報を含めるか制御します。InfluxDBサービスが`/ping`で認証を要求する場合は有効にしてください。デフォルトは無効で、認証なしで`/ping`を送信します。
   
   - TLSの有効化を設定します。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#enabling-tls-for-external-resource-access)を参照してください。
   
5. **Create**をクリックする前に、**Test Connectivity**を押してInfluxDBサーバーへの接続確認ができます。

6. 画面下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**か**Create Rule**を選択できます。ルールとSinkの作成方法は[InfluxDB Sink付きルール作成](#create-a-rule-with-influxdb-sink)を参照してください。

## InfluxDB Sink付きルールの作成

本節では、EMQXでソースMQTTトピック`t/#`からのメッセージを処理し、設定済みのSinkを介してInfluxDBに送信するルールの作成方法を示します。

1. EMQXダッシュボードの左メニューから**Integration** -> **Rules**をクリックします。

2. 画面右上の**Create**をクリックします。

3. ルール作成画面で、ルールIDに`my_rule`を入力します。

4. **SQL Editor**でルールを設定します。例えば、トピック`t/#`のMQTTメッセージをInfluxDBに保存したい場合、以下のSQL文を使用します。

   ::: tip

   独自のSQL文を指定する場合は、後で設定するSinkのデータ形式に含まれるすべての変数が`SELECT`句に含まれていることを確認してください。

   :::

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   初心者の方は、**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習とテストが可能です。

5. + **Add Action**ボタンをクリックし、ルールがトリガーするアクションを定義します。このアクションでEMQXはルール処理済みデータをInfluxDBに送信します。

6. **Type of Action**ドロップダウンから`InfluxDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既存のSinkがあれば選択可能ですが、本例では新規Sinkを作成します。

7. Sink名を入力します。英数字の大文字・小文字の組み合わせで指定してください。

8. **Connector**ドロップダウンから先に作成した`my_influxdb`を選択します。新規作成も可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

9. **Time Precision**を指定します。デフォルトは`millisecond`です。

10. **Data Format**を`JSON`または`Line Protocol`から選択し、InfluxDBへのデータ解析・書き込み方法を指定します。

    - JSON形式の場合、**Measurement**、**Timestamp**、**Fields**、**Tags**のデータ解析方法を定義します。すべてのキー値は変数やプレースホルダーを指定可能で、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.5/reference/syntax/line-protocol/)に準拠して設定できます。**Fields**はCSVファイルによる一括設定も可能です（詳細は[一括設定](#batch-setting)参照）。
    - Line Protocol形式の場合、テキストベースで計測値、タグセット、フィールドセット、タイムスタンプを指定し、[InfluxDB line protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/)の構文に準拠したプレースホルダーを利用します。

    ::: tip

    - InfluxDB 1.xまたは2.xに符号付き整数型を送る場合は、プレースホルダーの後に`i`を付けます。例：`${payload.int}i`。詳細は[InfluxDB 1.8で整数値を書き込む](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference/#write-the-field-value-1-as-an-integer-to-influxdb)を参照してください。
    - 符号なし整数型の場合は`u`を付けます。例：`${payload.int}u`。詳細は同上リンクを参照してください。

    :::

11. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。プライマリSinkが処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

12. **詳細設定（任意）**：詳細設定は[詳細設定](#advanced-configurations)を参照してください。

13. **Create**をクリックする前に、**Test Connectivity**でSinkがInfluxDBサーバーに接続可能かテストできます。

14. **Create**をクリックしてSink作成を完了します。ルール作成画面の**Action Outputs**タブに新規Sinkが表示されます。

15. ルール作成画面で設定内容を確認し、**Create**をクリックしてルールを生成します。

これでルールが正常に作成され、**Rule**ページに新規ルールが表示されます。**Actions(Sink)**タブをクリックすると、新規InfluxDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**でトポロジーを確認可能です。トピック`t/#`のメッセージがルール`my_rule`で解析されInfluxDBに送信・保存されていることがわかります。

### 一括設定

InfluxDBではデータエントリに数百のフィールドが含まれることが多く、データ形式の設定は複雑です。これに対応するため、EMQXはフィールドの一括設定機能を提供しています。

JSON形式でデータ形式を設定する際、CSVファイルからフィールドのキー・値ペアを一括インポートできます。

1. **Fields**テーブルの**Batch Setting**ボタンをクリックし、**Import Batch Setting**ポップアップを開きます。

2. 指示に従い、一括設定テンプレートファイルをダウンロードし、フィールドのキー・値ペアを入力します。テンプレートのデフォルト内容は以下の通りです。

   | Field  | Value              | 備考（任意）                                               |
   | ------ | ------------------ | ---------------------------------------------------------- |
   | temp   | ${payload.temp}    |                                                            |
   | hum    | ${payload.hum}     |                                                            |
   | precip | ${payload.precip}i | 値の後に`i`を付けてInfluxDBに整数として保存することを示す。 |

   - **Field**：フィールドキー。定数または`${var}`形式のプレースホルダーをサポート。
   - **Value**：フィールド値。定数またはプレースホルダーをサポートし、Line Protocolに従い型識別子を付加可能。
   - **備考**：CSV内の注釈用で、EMQXへのインポート対象外。

   CSVファイルのデータは2048行を超えないようにしてください。

3. 入力済みテンプレートファイルを保存し、**Import Batch Setting**ポップアップにアップロードして**Import**をクリックし、一括設定を完了します。

4. インポート後、**Fields**設定テーブルでキー・値ペアをさらに調整可能です。

## ルールのテスト

MQTTクライアントMQTTXを使い、トピック`t/1`にメッセージを送信してオンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello InfluxDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

InfluxDB UIの**Data Explorer**画面で、メッセージがInfluxDBに書き込まれていることを確認できます。

## 詳細設定

本節では、InfluxDBコネクターおよびSinkの詳細設定オプションについて説明します。ダッシュボードでコネクターやSinkを設定する際、**Advanced Settings**に進み、以下のパラメータを用途に応じて調整してください。

| **項目**               | **説明**                                                                                                         | **推奨値** |
| ---------------------- | ---------------------------------------------------------------------------------------------------------------- | ---------- |
| Start Timeout          | コネクターが自動起動したリソースの正常状態到達を待機する最大秒数です。InfluxDBのデータベースインスタンスなどが完全に稼働し、データ処理可能になるまでの待機時間を制御します。 | `5`        |
| Buffer Pool Size       | EMQXとInfluxDB間の送信型ブリッジでデータフロー管理に割り当てるバッファワーカープロセス数です。これらは送信前のデータ一時保存・処理を担当します。受信のみのSinkでは`0`に設定可能です。 | `4`        |
| Request TTL            | バッファに入ったリクエストの有効期限（秒）を指定します。TTLを超えてバッファ内にあるか、InfluxDBから応答・アックが得られない場合、リクエストは期限切れとみなされます。 | `45`       |
| Health Check Interval  | SinkがInfluxDB接続の自動ヘルスチェックを行う間隔（秒）です。                                                  | `15`       |
| Max Buffer Queue Size  | 各バッファワーカーがInfluxDB Sinkで一時的に保持可能な最大バイト数です。システム性能やデータ転送要件に応じて調整してください。 | `1`        |
| Max Batch Size         | EMQXからInfluxDBへ一度に転送するデータバッチの最大サイズです。サイズ調整により転送効率・性能を最適化できます。`1`の場合はバッチ化せず個別送信します。 | `100`      |
| Query Mode             | メッセージ送信の最適化のため、`asynchronous`（非同期）または`synchronous`（同期）モードを選択します。非同期モードではInfluxDB書き込みがMQTTパブリッシュ処理をブロックしませんが、クライアントがメッセージ受信をInfluxDB書き込みより先に受ける可能性があります。 | `Async`    |
| Inflight Window        | 送信済みだが応答・アック未受領の「インフライトクエリ」の最大数を制御します。**Query Mode**が`async`の場合、同一MQTTクライアントからのメッセージを厳密に順序処理したい場合は`1`に設定してください。 | `100`      |

## 参考情報

以下のリンクもご参照ください。

**ブログ**：

[1時間で構築するEMQX + InfluxDB + Grafana IoTデータ可視化ソリューション](https://www.emqx.com/en/blog/build-emqx-influxdb-grafana-iot-data-visualization-solution-in-one-hour)

[MQTTデータをInfluxDBに統合した時系列IoTアプリケーション構築](https://www.emqx.com/en/blog/building-an-iot-time-series-data-application-with-mqtt-and-influxdb)

[MQTTパフォーマンスベンチマークテスト：EMQX-InfluxDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-influxdb-integration)
