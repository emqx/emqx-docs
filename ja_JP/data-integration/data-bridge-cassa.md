# CassandraへのMQTTデータ取り込み

[Apache Cassandra](https://cassandra.apache.org/_/index.html) は、大規模データセットの処理と高スループットアプリケーションの構築を目的とした、人気のあるオープンソースの分散型NoSQLデータベース管理システムです。EMQXとApache Cassandraの統合により、メッセージやイベントをCassandraデータベースに保存できるようになり、時系列データの保存、デバイス登録および管理、リアルタイムデータ分析などの機能を実現します。

本ページでは、EMQXとCassandra間のデータ統合について包括的に紹介し、データ統合の作成および検証に関する実践的な手順を提供します。

:::tip
現在の実装はCassandra v3.xのみ対応しており、v4.xには未対応です。
:::

## 動作概要

Cassandraデータ統合はEMQXの標準機能であり、EMQXのデバイス接続およびメッセージ送信機能とCassandraの強力なデータ保存機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからCassandraへのデータ取り込みが簡素化され、複雑なコーディングを必要としません。

以下の図は、EMQXとCassandra間のデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration Cassandra](./assets/emqx-integration-cassandra.png)

CassandraへのMQTTデータ取り込みは以下のように動作します：

1. **メッセージのパブリッシュと受信**：接続された車両、IIoTシステム、エネルギー管理プラットフォームなどのIoTデバイスは、MQTTプロトコルを通じてEMQXに正常に接続し、特定のトピックにMQTTメッセージをパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールに従って処理されます。ルールは事前定義された条件に基づき、Cassandraにルーティングすべきメッセージを判別します。ペイロードの変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの処理が適用されます。
3. **Cassandraへのデータ取り込み**：ルールエンジンがCassandraへの保存対象メッセージを特定すると、Cassandraへの転送アクションがトリガーされます。処理済みデータはCassandraデータベースのコレクションにシームレスに書き込まれます。
4. **データの保存と活用**：データがCassandraに保存されることで、企業はそのクエリ機能を活用して様々なユースケースに対応できます。例えば、コネクテッドビークル分野では、車両の状態管理、リアルタイム指標に基づくルート最適化、資産追跡に利用できます。IIoT環境では、機械の状態監視、メンテナンス予測、生産スケジュールの最適化などに活用されます。

## 特長とメリット

Cassandraとのデータ統合は、効率的なデータ伝送、保存、活用を実現するための多彩な特長とメリットを提供します：

- **大規模時系列データの保存**：EMQXは大量のデバイス接続とメッセージ送信を処理可能です。Cassandraの高いスケーラビリティと分散ストレージ機能を活用し、大規模データセット（時系列データを含む）の保存と管理を実現し、時間範囲に基づくクエリや集計操作をサポートします。
- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからCassandraへの効率的かつ信頼性の高いデータ伝送を保証します。即時の洞察とアクションが必要なユースケースに理想的です。
- **高可用性の保証**：EMQXとCassandraは共にクラスター機能を提供します。組み合わせて使用することで、デバイス接続とデータを複数のサーバーに分散可能です。ノード障害時には自動的に他の利用可能なノードにフェイルオーバーし、高いスケーラビリティとフォールトトレランスを実現します。
- **柔軟なデータ変換**：EMQXの強力なSQLベースのルールエンジンにより、Cassandraに保存する前にデータを前処理できます。フィルタリング、ルーティング、集計、エンリッチメントなど多様な変換機能をサポートし、ニーズに応じたデータ整形が可能です。
- **柔軟なデータモデル**：Cassandraはカラムベースのデータモデルを採用し、柔軟なスキーマとカラムの動的追加をサポートします。構造化されたデバイスイベントやメッセージデータの保存・管理に適しており、多様なMQTTメッセージデータを容易に格納できます。

## はじめる前に

このセクションでは、TimescaleDBデータブリッジの作成前に必要な準備事項を説明します。Cassandraサーバーのインストール方法やキー スペースおよびテーブルの作成方法も含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Cassandraサーバーのインストール

以下のコマンドでDockerを使い、シンプルなCassandraサービスを起動します：

```bash
docker run --name cassa --rm -p 9042:9042 cassandra:3.11.14
```

### キースペースとテーブルの作成

Cassandra用のデータブリッジを作成する前に、キースペースとテーブルを作成する必要があります。

1. `mqtt`という名前のキースペースを作成します：

```bash
docker exec -it cassa cqlsh "-e CREATE KEYSPACE mqtt WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': 1}"
```

2. Cassandraに`mqtt_msg`テーブルを作成します：

```bash
docker exec -it cassa cqlsh "-e \
    CREATE TABLE mqtt.mqtt_msg( \
        msgid text, \
        topic text, \
        qos int,    \
        payload text, \
        arrived timestamp, \
        PRIMARY KEY(msgid, topic));"
```

## コネクターの作成

このセクションでは、SinkをCassandraサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとCassandraをローカルマシンで実行していることを前提としています。リモート環境で実行している場合は、設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**Cassandra**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下を設定します：
   - コネクター名を入力します。英数字の組み合わせで、例：`my_cassandra`
   - **Servers**に`127.0.0.1:9042`、**Keyspace**に`mqtt`を入力し、その他はデフォルトのままにします。
   - TLSを有効にするかどうかを選択します。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#enabling-tls-for-external-resource-access)を参照してください。
5. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがCassandraサーバーに接続できるかテストできます。
6. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてルールとSinkの作成を続行できます。詳細は[Create a Rule with Cassandra Sink](#create-a-rule-with-cassandra-sink)を参照してください。

## Cassandra Sinkを用いたルールの作成

このセクションでは、Dashboard上でソースMQTTトピック`t/#`のメッセージを処理し、処理結果をCassandraの`mqtt_msg`テーブルに保存するルールの作成方法を説明します。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**でルールを設定します。例えば、トピック`t/#`配下のMQTTメッセージをCassandraに転送したい場合、以下のSQL構文を使用します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドが`SELECT`部分に含まれていることを確認してください。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   注意：初心者の方は**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習とテストが可能です。

4. **+ Add Action**ボタンをクリックし、ルールにトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをCassandraに送信します。

5. **Type of Action**ドロップダウンから`Cassandra`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既に作成済みのSinkを選択することも可能ですが、ここでは新しいSinkを作成します。

6. Sinkの名前を入力します。英数字の組み合わせで指定してください。

7. **Connector**ドロップダウンから先ほど作成した`my_cassandra`を選択します。ドロップダウン横のボタンから新しいコネクターを作成することも可能です。設定パラメータの詳細は[Create a Connector](#create-a-connector)を参照してください。

8. Cassandraに`topic`、`id`、`clientid`、`qos`、`payload`、`timestamp`を保存するための**CQLテンプレート**を設定します。このテンプレートはCassandra Query Languageで実行され、サンプルコードは以下の通りです：

   ```sql
   insert into mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**同期（sync）**または**非同期（async）**クエリモードを選択します。詳細は[Features of Sink](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**ボタンをクリックしてSink設定を完了します。**Create Rule**ページに戻ると、**Action Outputs**タブに新しいSinkが表示されます。

12. **Create Rule**ページで設定内容を確認し、**Create**ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status**は`connected`となります。

これでルールの作成が完了し、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいCassandra Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認でき、トピック`t/#`配下のメッセージがルール`my_rule`で解析され、Cassandraに送信・保存されていることがわかります。

## ルールのテスト

MQTTXを使ってトピック`t/1`にメッセージを送信します：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Cassandra" }'
```

ルールとSinkの稼働状況を確認し、統計カウントが増加していることを確認してください。

以下のコマンドでメッセージがCassandraに保存されているか確認します：

```bash
docker exec -it cassa cqlsh "-e SELECT * FROM mqtt.mqtt_msg;"
```
