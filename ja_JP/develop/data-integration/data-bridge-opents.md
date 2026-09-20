# OpenTSDBへのMQTTデータ取り込み

[OpenTSDB](http://opentsdb.net/)はスケーラブルで分散型の時系列データベースです。EMQXはOpenTSDBとの連携をサポートしており、MQTTメッセージをOpenTSDBに保存して後続の分析や取得に利用できます。

本ページでは、EMQXとOpenTSDB間のデータ統合について包括的に解説し、データ統合の作成および検証の実践的な手順を紹介します。

## 動作概要

OpenTSDBデータ統合はEMQXの標準機能であり、EMQXのリアルタイムデータキャプチャおよび送信機能とOpenTSDBのデータ保存・分析機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからOpenTSDBへのデータ取り込みが簡素化され、複雑なコーディングを不要にします。

以下の図は、EMQXとOpenTSDB間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration OpenTSDB](./assets/emqx-integration-opentsdb.png)

EMQXはルールエンジンとSinkを介してデバイスデータをOpenTSDBに挿入します。OpenTSDBは豊富なクエリ機能を提供し、レポートやチャートなどのデータ分析結果の生成をサポートします。産業用エネルギー管理のシナリオを例にすると、ワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用デバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的にエネルギー消費データをパブリッシュします。このデータには生産ライン識別子やエネルギー消費値が含まれます。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンはトピックマッチングに基づき特定のソースからのメッセージを処理します。メッセージが到着するとルールエンジンを通過し、対応するルールとマッチングしてメッセージデータを処理します。これにはデータ形式の変換、特定情報のフィルタリング、コンテキスト情報によるメッセージの付加などが含まれます。
3. **OpenTSDBへのデータ取り込み**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをOpenTSDBに書き込む操作が実行されます。

データがOpenTSDBに書き込まれた後は、以下のように柔軟に活用できます。

- Grafanaなどの可視化ツールと連携し、エネルギー貯蔵データを表示するチャートを生成する。
- 監視やアラートのために業務システムと連携し、エネルギー貯蔵デバイスの状態を監視する。

## 特長と利点

OpenTSDBデータ統合は以下の特長と利点を提供します。

- **効率的なデータ処理**：EMQXは大量のIoTデバイス接続とメッセージスループットを処理可能であり、OpenTSDBはデータの書き込み、保存、クエリに優れた性能を発揮します。これにより、IoTシナリオのデータ処理要件をシステムに過度な負荷をかけずに満たせます。
- **メッセージ変換**：EMQXのルールを通じてメッセージの高度な処理や変換が可能であり、OpenTSDBへの書き込み前にデータを柔軟に整形できます。
- **大規模データ保存**：EMQXとOpenTSDBを統合することで、大量のデバイスデータを直接OpenTSDBに保存できます。OpenTSDBは大規模な時系列データの保存・クエリに特化したデータベースであり、IoTデバイスが生成する膨大な時系列データを効率的に処理可能です。
- **豊富なクエリ機能**：OpenTSDBの最適化されたストレージ構造とインデックスにより、数十億のデータポイントの高速な書き込みとクエリが可能です。これはIoTデバイスデータのリアルタイム監視、分析、可視化を必要とするアプリケーションに極めて有用です。
- **スケーラビリティ**：EMQXとOpenTSDBの両方がクラスター拡張に対応しており、ビジネスの成長に合わせて柔軟に水平スケールが可能です。

## はじめる前に

このセクションでは、OpenTSDBデータ統合の作成を開始する前に必要な準備、特にOpenTSDBサーバーのセットアップ方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### OpenTSDBのインストール

Dockerを使ってOpenTSDBをインストールし、Dockerイメージを起動します（現時点ではx86プラットフォームのみ対応）。

```bash
docker pull petergrace/opentsdb-docker

docker run -d --name opentsdb -p 4242:4242 petergrace/opentsdb-docker
```

## コネクターの作成

このセクションでは、SinkをOpenTSDBサーバーに接続するためのコネクターの作成方法を説明します。

以下の手順は、EMQXとOpenTSDBの両方をローカルマシンで実行していることを前提としています。リモートで実行している場合は、設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**OpenTSDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します。
   - コネクター名を入力します。英数字の組み合わせとしてください。例：`my_opentsdb`
   - **Server Host**に`http://127.0.0.1:4242`を入力します。OpenTSDBサーバーがリモートの場合は実際のURLを指定してください。
   - その他のオプションはデフォルトのままにします。
5. 詳細設定（任意）：詳細は[Sinkの特長](./data-bridges.md#features-of-sink)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがOpenTSDBサーバーに接続可能かテストできます。
7. ページ下部の**Create**ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてルールとSinkの作成を続行できます。詳細は[OpenTSDB Sink付きルールの作成](#create-a-rule-with-opentsdb-sink)を参照してください。

## OpenTSDB Sink付きルールの作成

このセクションでは、ダッシュボードでルールを作成し、ソースMQTTトピック `t/#` からのメッセージを処理し、処理済みデータを設定済みのSinkを介してOpenTSDBに保存する方法を説明します。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**に以下のステートメントを設定します。これはトピック`t/#`配下のMQTTメッセージをOpenTSDBに保存することを意味します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`部分に含めていることを確認してください。

   ```sql
   	SELECT
     		payload.metric as metric, payload.tags as tags, payload.value as value
   	FROM
     		"t/#"
   ```

   注意：初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールを学習・テストできます。

4. **+ Add Action**ボタンをクリックして、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをOpenTSDBに送信します。

5. **Type of Action**ドロップダウンリストから`OpenTSDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkを選択することも可能です。このデモでは新規Sinkを作成します。

6. Sinkの名前を入力します。英数字の組み合わせにしてください。

7. **Connector**ドロップダウンから先ほど作成した`my_opentsdb`を選択します。ドロップダウン横のボタンから新規コネクターを作成することも可能です。設定パラメーターの詳細は[コネクターの作成](#create-a-connector)を参照してください。

8. **Write Data**フィールドで、MQTTメッセージをOpenTSDBが要求する形式に正しく変換するための書き込み方法を指定します。例えば、クライアントが以下のデータを報告するとします。

   - トピック：`t/opents`
   - ペイロード：

   ```json
   {
     "metric": "cpu",
     "tags": {
       "host": "serverA"
     },
     "value": 12
   }
   ```

   提供されたペイロードデータ形式に基づき、以下の形式情報を設定します。

   - **Timestamp**：OpenTSDBはデータポイントの時刻を記録するためにタイムスタンプを必要とします。MQTTメッセージにタイムスタンプが含まれない場合は、EMQXのSink設定で現在時刻をタイムスタンプとして使用するか、クライアントの報告データ形式を修正してタイムスタンプフィールドを含める必要があります。
   - **Metric**：この例では`"metric": "cpu"`がメトリック名`cpu`を示します。
   - **Tags**：タグはメトリックに関する追加情報を表します。ここでは`"tags": {"host": "serverA"}`で、このメトリックデータがホスト`serverA`からのものであることを示しています。
   - **Value**：実際のメトリック値です。この例では`"value": 12`で、メトリック値が12であることを示します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの特長](./data-bridges.md#features-of-sink)の該当設定情報を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがOpenTSDBサーバーに接続可能かテストできます。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認します。**Create**ボタンをクリックしてルールを生成します。

これでOpenTSDB Sinkを介したデータ転送用のルールが正常に作成されました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると、新しいOpenTSDB Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックしてトポロジーを確認すると、トピック`t/#`配下のメッセージがルール`my_rule`で解析され、OpenTSDBに送信・保存されていることがわかります。

## ルールのテスト

MQTTXを使ってトピック`t/opents`にメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/opents -m '{"metric":"cpu","tags":{"host":"serverA"},"value":12}'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあるはずです。

OpenTSDBにデータが書き込まれているか確認します。

```bash
curl -X POST -H "Accept: Application/json" -H "Content-Type: application/json" http://localhost:4242/api/query -d '{
    "start": "1h-ago",
    "queries": [
        {
            "aggregator": "last",
            "metric": "cpu",
            "tags": {
                "host": "*"
            }
        }
    ],
    "showTSUIDs": "true",
    "showQuery": "true",
    "delete": "false"
}'
```

クエリ結果の整形済み出力例は以下の通りです。

```json
[
  {
    "metric": "cpu",
    "tags": {
      "host": "serverA"
    },
    "aggregateTags": [],
    "query": {
      "aggregator": "last",
      "metric": "cpu",
      "tsuids": null,
      "downsample": null,
      "rate": false,
      "filters": [
        {
          "tagk": "host",
          "filter": "*",
          "group_by": true,
          "type": "wildcard"
        }
      ],
      "percentiles": null,
      "index": 0,
      "rateOptions": null,
      "filterTagKs": [
        "AAAB"
      ],
      "explicitTags": false,
      "useFuzzyFilter": true,
      "preAggregate": false,
      "rollupUsage": null,
      "rollupTable": "raw",
      "showHistogramBuckets": false,
      "useMultiGets": true,
      "tags": {
        "host": "wildcard(*)"
      },
      "histogramQuery": false
    },
    "tsuids": [
      "000001000001000001"
    ],
    "dps": {
      "1683532519": 12
    }
  }
]
```
