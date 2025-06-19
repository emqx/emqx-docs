# CassandraへのMQTTデータ取り込み

[Apache Cassandra](https://cassandra.apache.org/_/index.html) は、大規模データセットの処理と高スループットアプリケーションの構築を目的とした、人気のあるオープンソースの分散型NoSQLデータベース管理システムです。EMQXとApache Cassandraの統合により、メッセージやイベントをCassandraデータベースに保存できるようになり、時系列データの保存、デバイス登録・管理、リアルタイムデータ分析などの機能を実現します。

本ページでは、EMQXとCassandra間のデータ統合について、実践的な手順を交えて包括的に紹介します。

:::tip
現在の実装はCassandra v3.xのみ対応しており、v4.xには未対応です。
:::

## 動作概要

Cassandraデータ統合はEMQXの標準機能であり、EMQXのデバイス接続およびメッセージ送受信機能とCassandraの強力なデータ保存機能を組み合わせています。内蔵の[ルールエンジン](./rules.md)コンポーネントにより、EMQXからCassandraへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとCassandra間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration Cassandra](./assets/emqx-integration-cassandra.png)

CassandraへのMQTTデータ取り込みの流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：接続車両、IIoTシステム、エネルギー管理プラットフォームなどのIoTデバイスは、MQTTプロトコルを通じてEMQXに接続し、特定のトピックにMQTTメッセージをパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージはルールエンジンを通過し、EMQXで定義されたルールに従って処理されます。ルールは事前に定義された条件に基づき、Cassandraにルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Cassandraへのデータ取り込み**：ルールエンジンがCassandra保存対象のメッセージを特定すると、メッセージをCassandraに転送するアクションをトリガーします。処理済みデータはCassandraデータベースのコレクションにシームレスに書き込まれます。
4. **データの保存と活用**：データがCassandraに保存されることで、企業はそのクエリ機能を活用して様々なユースケースに対応できます。例えば、接続車両の分野では、車両の健康状態の把握、リアルタイム指標に基づくルート最適化、資産追跡などに利用可能です。IIoT環境では、機械の健康監視、メンテナンス予測、生産スケジュールの最適化などに活用されます。

## 特長とメリット

Cassandraとのデータ統合は、効率的なデータ伝送、保存、活用を実現するために以下の特長とメリットを提供します：

- **大規模時系列データの保存**：EMQXは大量のデバイス接続とメッセージ送受信を処理可能です。Cassandraの高いスケーラビリティと分散ストレージ機能を活用し、大規模データセット（時系列データを含む）の保存・管理を実現し、時間範囲に基づくクエリや集約操作をサポートします。
- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからCassandraへの効率的かつ信頼性の高いデータ伝送を保証します。即時の洞察とアクションが求められるユースケースに適しています。
- **高可用性の保証**：EMQXとCassandraは共にクラスタリング機能を提供します。組み合わせて使用することで、デバイス接続とデータを複数のサーバーに分散可能です。ノード障害時には自動的に他の利用可能なノードに切り替わり、高いスケーラビリティとフォールトトレランスを確保します。
- **柔軟なデータ変換**：EMQXの強力なSQLベースのルールエンジンにより、Cassandraに保存する前にデータの前処理が可能です。フィルタリング、ルーティング、集約、強化など多様な変換機能をサポートし、ニーズに応じてデータを整形できます。
- **柔軟なデータモデル**：Cassandraはカラムベースのデータモデルを採用し、柔軟なスキーマと動的なカラム追加をサポートします。構造化されたデバイスイベントやメッセージデータの保存・管理に適しており、多様なMQTTメッセージデータを容易に格納できます。

## はじめる前に

このセクションでは、TimescaleDBデータブリッジの作成を始める前に必要な準備について説明します。Cassandraサーバーのインストール方法やキー スペースとテーブルの作成手順も含みます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Cassandraサーバーのインストール

Dockerを使って簡単にCassandraサービスを起動します：

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
   - コネクター名を入力します。英数字の組み合わせが推奨されます（例：`my_cassandra`）。
   - **Servers**に`127.0.0.1:9042`、**Keyspace**に`mqtt`を入力し、他はデフォルトのままにします。
   - TLSを有効にするかどうかを選択します。TLS接続オプションの詳細は[外部リソースアクセスのTLS有効化](../network/overview.md#enabling-tls-for-external-resource-access)を参照してください。
5. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがCassandraサーバーに接続できるか確認できます。
6. ページ下部の**Create**ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで**Back to Connector List**または**Create Rule**を選択して、ルールとSinkの作成を続けることができます。詳細は[Create a Rule with Cassandra Sink](#create-a-rule-with-cassandra-sink)を参照してください。

## Cassandra Sink付きルールの作成

このセクションでは、ダッシュボード上で、ソースMQTTトピック`t/#`からのメッセージを処理し、処理結果をCassandraの`mqtt_msg`テーブルに保存するルールの作成方法を説明します。

1. EMQXダッシュボードで、**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**でルールを設定します。例えば、トピック`t/#`配下のMQTTメッセージをCassandraに転送したい場合、以下のSQL文を使用します。

   注意：独自のSQL文を指定する場合は、Sinkが必要とする全フィールドを`SELECT`句に含めてください。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   注意：初心者の方は**SQL Examples**や**Enable Test**をクリックして、SQLルールの学習やテストが可能です。

4. **+ Add Action**ボタンをクリックし、ルールにトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをCassandraに送信します。

5. **Type of Action**ドロップダウンから`Cassandra`を選択します。**Action**はデフォルトの`Create Action`のままにします。既存のSinkがあれば選択可能ですが、この例では新規Sinkを作成します。

6. Sink名を入力します。英数字の組み合わせが推奨されます。

7. **Connector**ドロップダウンから先ほど作成した`my_cassandra`を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメータの詳細は[Create a Connector](#create-a-connector)を参照してください。

8. Cassandraに`topic`、`id`、`clientid`、`qos`、`payload`、`timestamp`を保存するための**CQLテンプレート**を設定します。このテンプレートはCassandra Query Languageで実行され、サンプルは以下の通りです：

   ```sql
   insert into mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, ${timestamp})
   ```

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**同期（sync）**または**非同期（async）**クエリモードを選択します。詳細は[Features of Sink](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**ボタンをクリックしてSinkの設定を完了します。**Create Rule**ページの**Action Outputs**タブに新しいSinkが表示されます。

12. **Create Rule**ページで設定内容を確認し、**Create**ボタンをクリックしてルールを生成します。作成したルールはルール一覧に表示され、**status**は`connected`となります。

これでルールの作成が完了し、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいCassandra Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認でき、トピック`t/#`配下のメッセージがルール`my_rule`で解析され、Cassandraに送信・保存されていることが分かります。

## ルールのテスト

MQTTXを使ってトピック`t/1`にメッセージを送信します：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Cassandra" }'
```

ルールとSinkの稼働状況を確認し、統計カウントが増加していることを確認してください。

以下のコマンドでCassandraにメッセージが保存されているか確認します：

```bash
docker exec -it cassa cqlsh "-e SELECT * FROM mqtt.mqtt_msg;"
```
