# CassandraへのMQTTデータ取り込み

[Apache Cassandra](https://cassandra.apache.org/_/index.html) は、大規模データセットの処理や高スループットアプリケーションの構築に適した、人気のあるオープンソースの分散型NoSQLデータベース管理システムです。EMQXとApache Cassandraの統合により、Cassandraデータベースへのメッセージやイベントの保存が可能となり、時系列データの保存、デバイス登録・管理、リアルタイムデータ分析などの機能を実現できます。

本ページでは、EMQXとCassandra間のデータ統合について、実践的な作成方法と検証手順を含めて包括的に紹介します。

:::tip
現在の実装はCassandra v3.xのみ対応しており、v4.xには未対応です。
:::

## 動作概要

Cassandraデータ統合は、EMQXに標準搭載された機能であり、EMQXのデバイス接続およびメッセージ送受信機能とCassandraの強力なデータ保存機能を組み合わせています。内蔵の[ルールエンジン](./rules.md)コンポーネントにより、EMQXからCassandraへのデータ取り込みが簡素化され、複雑なコーディングを必要としません。

以下の図は、EMQXとCassandra間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration Cassandra](./assets/emqx-integration-cassandra.png)

CassandraへのMQTTデータ取り込みは以下のように動作します：

1. **メッセージのパブリッシュと受信**：接続車両、IIoTシステム、エネルギー管理プラットフォームなどのIoTデバイスは、MQTTプロトコルを通じてEMQXに正常に接続し、特定のトピックにMQTTメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータ処理**：メッセージが到着するとルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前定義された条件に基づき、Cassandraにルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Cassandraへのデータ取り込み**：ルールエンジンがCassandra保存対象のメッセージを特定すると、メッセージをCassandraに転送するアクションをトリガーします。処理済みデータはCassandraデータベースのコレクションにシームレスに書き込まれます。
4. **データ保存と活用**：データがCassandraに保存されることで、企業はそのクエリ機能を活用して様々なユースケースに対応できます。例えば、接続車両分野では、車両の状態管理、リアルタイム指標に基づくルート最適化、資産追跡などに活用可能です。IIoT環境では、機械の状態監視、保守予測、生産スケジュールの最適化などに利用されます。

## 特長とメリット

Cassandraとのデータ統合は、効率的なデータ送信、保存、活用を実現するための多彩な特長とメリットを提供します：

- **大規模時系列データ保存**：EMQXは膨大なデバイス接続とメッセージ送信を処理可能です。Cassandraの高いスケーラビリティと分散ストレージ機能を活用し、大規模データセット（時系列データを含む）の保存・管理を実現し、時間範囲に基づくクエリや集約操作をサポートします。
- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからCassandraへの効率的かつ信頼性の高いデータ伝送を保証します。即時の洞察とアクションが求められるユースケースに最適です。
- **高可用性の保証**：EMQXとCassandraは共にクラスター機能を提供します。組み合わせて使用することで、デバイス接続とデータを複数サーバーに分散可能です。ノード障害時には自動的に他の利用可能なノードに切り替わり、高いスケーラビリティとフォールトトレランスを確保します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、Cassandraに保存する前にデータの前処理が可能です。フィルタリング、ルーティング、集約、強化など多様なデータ変換機構をサポートし、組織のニーズに合わせてデータを整形できます。
- **柔軟なデータモデル**：Cassandraはカラムベースのデータモデルを採用し、柔軟なスキーマ設計と動的なカラム追加をサポートします。構造化されたデバイスイベントやメッセージデータの保存・管理に適しており、多様なMQTTメッセージデータの格納が容易です。

## はじめる前に

このセクションでは、TimescaleDBデータブリッジ作成前の準備として、Cassandraサーバーのインストールやキー スペースおよびテーブルの作成方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Cassandraサーバーのインストール

Dockerを使ってシンプルなCassandraサービスを起動します：

```bash
docker run --name cassa --rm -p 9042:9042 cassandra:3.11.14
```

### キースペースとテーブルの作成

Cassandra用のデータブリッジを作成する前に、キースペースとテーブルを作成する必要があります。

1. `mqtt`という名前のキースペースを作成します：

```bash
docker exec -it cassa cqlsh "-e CREATE KEYSPACE mqtt WITH REPLICATION = {'class': 'SimpleStrategy', 'replication_factor': 1}"
```

2. Cassandraにテーブル `mqtt_msg` を作成します：

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

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Cassandra** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下を設定します：
   - コネクター名を入力します。英数字の組み合わせで、例：`my_cassandra`
   - **Servers** に `127.0.0.1:9042`、**Keyspace** に `mqtt` を入力し、その他はデフォルトのままにします。
   - TLSを有効にするかどうかを選択します。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../../guides/network/overview.md#enabling-tls-for-external-resource-access)を参照してください。
5. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターがCassandraサーバーに接続できるかテストできます。
6. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** をクリックし、ルールとSinkの作成を続けてCassandraへのデータ転送を指定できます。詳細は[Create a Rule with Cassandra Sink](#create-a-rule-with-cassandra-sink)を参照してください。

## Cassandra Sinkを使ったルールの作成

このセクションでは、ダッシュボード上でソースMQTTトピック `t/#` からのメッセージを処理し、処理結果をCassandraテーブル `mqtt_msg` に保存するルールの作成方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` を入力し、**SQL Editor** にルールを設定します。トピック `t/#` のMQTTメッセージをCassandraに転送したい場合、以下のSQL構文を使用します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注意：初心者の方は **SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストが可能です。

4. **+ Add Action** ボタンをクリックして、ルールによりトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをCassandraに送信します。

5. **Type of Action** ドロップダウンから `Cassandra` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みのSinkがあれば選択も可能です。本例では新規Sinkを作成します。

6. Sinkの名前を入力します。英数字の組み合わせで指定してください。

7. **Connector** ドロップダウンから先ほど作成した `my_cassandra` を選択します。隣のボタンをクリックして新規コネクターを作成することも可能です。設定パラメーターの詳細は[Create a Connector](#create-a-connector)を参照してください。

8. Cassandraに `topic`、`id`、`clientid`、`qos`、`payload`、`timestamp` を保存するための**CQLテンプレート**を設定します。このテンプレートはCassandra Query Languageで実行され、サンプルコードは以下の通りです：

   ```sql
   insert into mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて **sync** または **async** クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create** ボタンをクリックしてSink設定を完了します。**Create Rule** ページに戻ると、**Action Outputs** タブに新しいSinkが表示されます。

12. **Create Rule** ページで設定内容を確認し、**Create** ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status** は `connected` となります。

これでルールが正常に作成され、**Rule** ページに新しいルールが表示されます。**Actions(Sink)** タブをクリックすると、新しいCassandra Sinkが確認できます。

また、**Integration** -> **Flow Designer** をクリックしてトポロジーを確認できます。トピック `t/#` のメッセージがルール `my_rule` によって解析され、Cassandraに送信・保存されていることがわかります。

## ルールのテスト

MQTTXを使ってトピック `t/1` にメッセージを送信します：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Cassandra" }'
```

ルールとSinkの稼働状況を確認すると、統計カウントが多少増加しているはずです。

以下のコマンドでCassandraにメッセージが保存されているか確認します：

```bash
docker exec -it cassa cqlsh "-e SELECT * FROM mqtt.mqtt_msg;"
```
