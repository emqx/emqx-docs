# ElasticsearchへのMQTTデータ取り込み

[Elasticsearch](https://www.elastic.co/elasticsearch/)は、分散型の検索およびデータ分析エンジンであり、多様なデータタイプに対して全文検索、構造化検索、分析機能を提供します。EMQXはElasticsearchと連携することで、MQTTデータをシームレスにElasticsearchに取り込み、保存することが可能です。この連携により、Elasticsearchの強力なスケーラビリティと分析機能を活用し、IoTアプリケーション向けに効率的かつスケーラブルなデータ保存・分析ソリューションを提供します。

本ページでは、EMQXとElasticsearch間のデータ統合について詳述し、ルールおよびSinkの作成方法を実践的に解説します。

## 動作概要

Elasticsearchとのデータ統合はEMQXの標準機能であり、EMQXのデバイスアクセスおよびメッセージ転送機能とElasticsearchのデータ保存・分析機能を組み合わせています。簡単な設定でMQTTデータのシームレスな統合が実現可能です。

![MQTT to Elasticsearch](./assets/mqtt-to-Elasticsearch.jpg)

EMQXとElasticsearchは、リアルタイムのデバイスデータを効率的に収集・分析するスケーラブルなIoTプラットフォームを提供します。このアーキテクチャにおいて、EMQXはデバイスアクセス、メッセージ転送、データルーティングを担うIoTプラットフォームとして機能し、Elasticsearchはデータ保存および分析プラットフォームとしてデータの保存、検索、分析を担当します。

EMQXはルールエンジンとSinkを介してデバイスデータをElasticsearchに転送し、Elasticsearchは強力な検索・分析機能を用いてレポートやチャートなどのデータ分析結果を生成し、Kibanaの可視化ツールを通じてユーザーに表示します。ワークフローは以下の通りです。

1. **デバイスメッセージのパブリッシュと受信**：IoTデバイスはMQTTプロトコルで接続し、テレメトリやステータスデータを特定のトピックにパブリッシュします。EMQXはこれを受信し、ルールエンジンで比較処理を行います。
2. **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンを用いて、特定のトピックからのMQTTメッセージをトピックマッチングに基づき処理します。ルールエンジンは対応するルールにマッチしたメッセージを処理し、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などを行います。
3. **Elasticsearchへの書き込み**：ルールエンジンで定義されたルールにより、メッセージをElasticsearchに書き込む処理がトリガーされます。Elasticsearch Sinkは柔軟な操作方法とドキュメントテンプレートを提供し、メッセージの特定フィールドを対応するインデックスに所望の形式で書き込みます。

デバイスデータがElasticsearchに書き込まれた後は、Elasticsearchの検索・分析機能を柔軟に活用して以下のようなデータ処理が可能です。

1. **ログ監視**：IoTデバイスは大量のログデータを生成し、これをElasticsearchに送信して保存・分析できます。Kibanaなどの可視化ツールと連携することで、ログデータに基づくチャートを作成し、デバイスの状態、稼働履歴、エラーメッセージなどのリアルタイム情報を表示できます。これにより、開発者や運用者は潜在的な問題を迅速に特定し解決できます。
2. **地理情報（マップ）**：IoTデバイスは位置情報を生成することが多く、これをElasticsearchに保存可能です。KibanaのMaps機能を利用して、デバイスの位置情報を地図上に可視化し、追跡や分析を行えます。
3. **エンドポイントセキュリティ**：IoTデバイスのセキュリティログデータをElasticsearchに送信し、Elastic Securityと連携してセキュリティレポートを生成、デバイスのセキュリティ状況をリアルタイムで監視し、潜在的な脅威を検知・対応できます。

## 特長とメリット

Elasticsearchデータ統合は以下の特長とメリットをビジネスにもたらします。

- **効率的なデータインデックスと検索**：ElasticsearchはEMQXからの大規模リアルタイムメッセージデータを容易に処理可能です。強力な全文検索とインデックス機能により、IoTメッセージデータの迅速かつ効率的な検索・クエリが可能です。
- **データの可視化**：Elastic Stackの一部であるKibanaと連携することで、IoTデータの強力な可視化が実現し、データの理解と分析を支援します。
- **柔軟なデータ操作**：EMQXのElasticsearch統合はインデックス、ドキュメントID、ドキュメントテンプレートの動的設定をサポートし、ドキュメントの作成、更新、削除が可能で、より幅広いIoTデータ統合シナリオに対応します。
- **スケーラビリティ**：ElasticsearchとEMQXは共にクラスタリングをサポートし、ノードを追加することで処理能力を容易に拡張でき、ビジネスの継続的な拡大を支援します。

## はじめる前に

このセクションでは、EMQXでElasticsearchデータ統合を作成する前に必要な準備作業として、Elasticsearchのインストールおよびインデックス作成について紹介します。

### 前提条件

- [ルール](./rules.md)の理解
- [データ統合](./data-bridges.md)の理解

### Elasticsearchのインストールとインデックス作成

EMQXはプライベートに展開したElasticsearchまたはクラウド上のElasticと連携可能です。Elastic CloudやDockerを利用してElasticsearchインスタンスを展開できます。

1. Docker環境がない場合は、[Dockerをインストール](https://docs.docker.com/install/)してください。

2. X-Packセキュリティ認証を有効にしたElasticsearchコンテナを起動します。デフォルトのユーザー名は`elastic`、パスワードは`public`に設定します。

   ```bash
   docker run -d --name elasticsearch \
       -p 9200:9200 \
       -p 9300:9300 \
       -e "discovery.type=single-node" \
       -e "xpack.security.enabled=true" \
       -e "ELASTIC_PASSWORD=public" \
       docker.elastic.co/elasticsearch/elasticsearch:7.10.1
   ```

3. デバイスがパブリッシュするメッセージを保存するための`device_data`インデックスを作成します。Elasticsearchのユーザー名とパスワードは適宜置き換えてください。

   ```bash
   curl -u elastic:public -X PUT "localhost:9200/device_data?pretty" -H 'Content-Type: application/json' -d'
   {
     "mappings": {
       "properties": {
         "ts": { "type": "date" },
         "clientid": { "type": "keyword" },
         "payload": {
           "type": "object",
           "dynamic": true
         }
       }
     }
   }'
   ```

## コネクターの作成

Elasticsearch Sinkを追加する前に、Elasticsearchコネクターを作成する必要があります。

以下の手順は、EMQXとElasticsearchを同一ローカルマシンで実行していることを前提としています。リモート環境で実行している場合は設定を適宜調整してください。

1. ダッシュボードの **Integration** -> **Connectors** ページに移動します。
2. 画面右上の **Create** をクリックします。
3. コネクタータイプとして **Elasticsearch** を選択し、次へ進みます。
4. コネクター名を入力します。例として `my-elasticsearch` とします。名前は大文字・小文字・数字の組み合わせで指定してください。
5. デプロイ方法に応じてElasticsearch接続情報を入力します。
   - **URL**：ElasticsearchサービスのRESTインターフェースURLを `http://localhost:9200` と入力します。
   - **Username**：Elasticsearchサービスのユーザー名を `elastic` と指定します。
   - **Password**：Elasticsearchサービスのパスワードを `public` と入力します。
6. 画面下部の **Create** ボタンをクリックしてコネクター作成を完了します。

これでコネクターが作成されました。次に、Elasticsearchに書き込むデータを指定するルールを作成します。

## Elasticsearch Sinkを用いたルールの作成

このセクションでは、EMQXでソースMQTTトピック`t/#`からのメッセージを処理し、処理結果を設定したSinkを通じてElasticsearchの`device_data`インデックスに書き込むルールの作成方法を示します。

1. ダッシュボードの **Integration** -> **Rules** ページに移動します。

2. 画面右上の **Create** をクリックします。

3. ルールIDに `my_rule` を入力し、SQLエディターに`t/#`トピックからのMQTTメッセージをElasticsearchに保存するルールSQLを入力します。SQLは以下の通りです。

   ```sql
   SELECT
     clientid,
     timestamp as ts,
     payload
   FROM
       "t/#"
   ```

   ::: tip

   SQLに不慣れな場合は、**SQL Examples**や**Enable Debugging**をクリックしてルールSQLの学習やテストが可能です。

   :::

4. **Add Action** をクリックし、**Action Type**のドロップダウンリストから`Elasticsearch`を選択します。**Action**のドロップダウンはデフォルトの`Create Action`のままにするか、既存のElasticsearchアクションを選択できます。この例では新規Sinkを作成しルールに追加します。

5. Sinkの名前と説明を入力します。

6. コネクタードロップダウンから先ほど作成した`my-elasticsearch`を選択します。隣のボタンをクリックするとポップアップで新規コネクター作成も可能です。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

7. JSON形式のデータ挿入用ドキュメントテンプレートを以下のように設定します。

   - **Action**：`Create`、`Update`、`Delete`の任意のアクションを指定可能です。

   - **Index Name**：操作対象のインデックスまたはインデックスエイリアス名。`${var}`形式のプレースホルダーが利用可能です。

   - **Document ID**：`Create`アクションは任意、その他は必須。インデックス内のドキュメント一意識別子。`${var}`形式のプレースホルダーが利用可能です。指定しない場合はElasticsearchが自動生成します。

   - **Routing**：ドキュメントを格納するインデックスのシャード指定。空欄の場合はElasticsearchが自動判定します。

   - **Document Template**：カスタムドキュメントテンプレート。JSONオブジェクトに変換可能で`${var}`形式のプレースホルダーをサポートします。例：`{ "field": "${payload.field}"}`や`${payload}`。

   - **Max Retries**：書き込み失敗時の最大リトライ回数。デフォルトは3回。

   - **Overwrite Document**（`Create`アクション特有）：既存ドキュメントがある場合に上書きするか。`No`の場合は書き込み失敗となります。

   - **Enable Upsert**（`Update`アクション特有）：更新対象ドキュメントが存在しない場合、挿入処理として扱い、新規ドキュメントを挿入します。

   本例では、インデックス名を`device_data`に設定し、クライアントIDとタイムスタンプの組み合わせ`${clientid}_${ts}`をドキュメントIDとして使用します。ドキュメントにはクライアントID、現在のタイムスタンプ、およびメッセージ本文全体を格納します。ドキュメントテンプレートは以下の通りです。

   ```json
   {
     "clientid": "${clientid}",
     "ts": ${ts},
     "payload": ${payload}
   }
   ```

8. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。プライマリSinkが処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
9. その他のパラメータはデフォルトのままにします。
10. **Create** ボタンをクリックしてSinkの作成を完了します。新規Sinkは**Action Outputs**に追加されます。
11. ルール作成画面に戻り、**Create** ボタンをクリックしてルール作成を完了します。

これでルールの作成が完了しました。**Rules**ページで新規ルールを確認でき、**Actions (Sink)**タブで新規Elasticsearch Sinkを確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、`t/#`トピックからのメッセージがルール`my_rule`で解析されElasticsearchに書き込まれる様子を視覚的に確認できます。

## ルールのテスト

MQTTクライアントMQTTXを使って、`t/1`トピックにメッセージをパブリッシュします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{"temp":24,"humidity":30}'
```

Sinkの動作統計を確認すると、ヒット数および送信成功数がそれぞれ+1されていることがわかります。

`_search` APIを使ってインデックス内のドキュメント内容を確認し、`device_data`インデックスにデータが書き込まれているかをチェックします。

```bash
curl -X GET "localhost:9200/device_data/_search?pretty"
```

正しい応答結果は以下のようになります。

```json
  "took" : 1098,
  "timed_out" : false,
  "_shards" : {
    "total" : 1,
    "successful" : 1,
    "skipped" : 0,
    "failed" : 0
  },
  "hits" : {
    "total" : {
      "value" : 1,
      "relation" : "eq"
    },
    "max_score" : 1.0,
    "hits" : [
      {
        "_index" : "device_data",
        "_type" : "_doc",
        "_id" : "emqx_c_1705479455289",
        "_score" : 1.0,
        "_source" : {
          "clientid" : "emqx_c",
          "ts" : 1705479455289,
          "payload" : {
            "temperature": 24,
            "humidity": 30
          }
        }
      }
    ]
  }
}
```

<!-- ## Advanced Settings-->

<!-- TODO -->
