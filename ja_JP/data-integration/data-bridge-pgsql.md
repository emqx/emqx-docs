# PostgreSQLへのMQTTデータ取り込み

[PostgreSQL](https://www.postgresql.org/)は、世界で最も先進的なオープンソースのリレーショナルデータベースであり、シンプルなアプリケーションから複雑なデータ処理まで対応可能な強力なデータ処理能力を備えています。EMQXはPostgreSQLとの統合をサポートしており、IoTデバイスからのリアルタイムデータストリームを効率的に処理できます。この統合により、大規模なデータ保存、正確なクエリ、複雑なデータ関連分析が可能となり、データの整合性も確保されます。EMQXの効率的なメッセージルーティングとPostgreSQLの柔軟なデータモデルを活用することで、デバイスの状態監視、イベント追跡、操作監査が容易になり、ビジネスに深いデータインサイトと強力なビジネスインテリジェンス支援を提供します。

本ページでは、EMQXとPostgreSQL間のデータ統合について包括的に紹介し、ルールとシンクの作成方法を実践的に解説します。

::: tip
本ページの内容はMatrixDBにも適用可能です。
:::

## 動作概要

PostgreSQLデータ統合は、MQTTベースのIoTデータとPostgreSQLの強力なデータ保存機能を橋渡しするためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからPostgreSQLへのデータ取り込みが簡素化され、複雑なコーディングを不要にします。

以下の図は、EMQXとPostgreSQL間のデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration PostgreSQL](./assets/emqx-integration-postgesql.png)

PostgreSQLへのMQTTデータ取り込みの流れは以下の通りです。

- **IoTデバイスがEMQXに接続**：IoTデバイスがMQTTプロトコルを通じて正常に接続されると、オンラインイベントがトリガーされます。イベントにはデバイスID、送信元IPアドレスなどの情報が含まれます。
- **メッセージのパブリッシュと受信**：デバイスは特定のトピックにテレメトリやステータスデータをパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
- **ルールエンジンによるメッセージ処理**：組み込みのルールエンジンは、特定のソースからのメッセージやイベントをトピックマッチングに基づいて処理します。ルールエンジンは対応するルールをマッチさせ、データ形式の変換、特定情報のフィルタリング、コンテキスト情報の付加などの処理を行います。
- **PostgreSQLへの書き込み**：ルールがメッセージのPostgreSQLへの書き込みをトリガーします。SQLテンプレートを利用して、ルール処理結果からデータを抽出しSQLを構築、PostgreSQLに送信して実行することで、メッセージの特定フィールドを対応するテーブルやカラムに書き込んだり更新したりします。

イベントやメッセージデータがPostgreSQLに書き込まれた後は、PostgreSQLに接続してデータを読み出し、以下のような柔軟なアプリケーション開発が可能です。

- Grafanaなどの可視化ツールに接続し、データに基づくグラフを生成してデータ変化を表示。
- デバイス管理システムに接続し、デバイス一覧や状態を確認、異常動作を検知して潜在的な問題をタイムリーに解消。

## 特長とメリット

PostgreSQLは豊富な機能を持つ人気のオープンソースリレーショナルデータベースです。PostgreSQLとのデータ統合は、以下のような特長と利点をビジネスにもたらします。

- **柔軟なイベント処理**：EMQXルールエンジンを通じて、PostgreSQLはデバイスのライフサイクルイベントを処理でき、IoTアプリケーション実装に必要な各種管理・監視タスクの開発を大幅に容易にします。イベントデータを分析することで、デバイスの故障や異常動作、傾向変化を迅速に検出し、適切な対応が可能です。
- **メッセージ変換**：メッセージはEMQXルールで多様な処理・変換を経てからPostgreSQLに書き込まれるため、保存や利用がより便利になります。
- **柔軟なデータ操作**：PostgreSQLデータブリッジが提供するSQLテンプレートを使い、特定フィールドのデータを対応テーブル・カラムに簡単に書き込み・更新でき、柔軟なデータ保存・管理を実現します。
- **ビジネスプロセスの統合**：PostgreSQLデータブリッジにより、デバイスデータをPostgreSQLの豊富なエコシステムアプリケーションと統合可能で、ERP、CRM、その他カスタムビジネスシステムとの連携を促進し、高度なビジネスプロセスや自動化を実現します。
- **IoTとGIS技術の融合**：PostgreSQLはGISデータの保存・クエリ機能を備え、地理空間インデックス、ジオフェンスやアラート、リアルタイム位置追跡、地理情報処理などをサポートします。EMQXの信頼性の高いメッセージ伝送能力と組み合わせることで、車両など移動体の地理位置情報を効率的に処理・分析し、リアルタイム監視、インテリジェントな意思決定、業務最適化を可能にします。
- **ランタイムメトリクス**：各シンクのランタイムメトリクス（総メッセージ数、成功/失敗数、現在のレートなど）を閲覧可能です。

柔軟なイベント処理、多様なメッセージ変換、柔軟なデータ操作、リアルタイム監視・分析機能を通じて、効率的で信頼性が高くスケーラブルなIoTアプリケーションを構築でき、ビジネスの意思決定や最適化に役立ちます。

## はじめる前に

このセクションでは、PostgreSQLデータベースシンクを作成する前に必要な準備として、PostgreSQLサーバーのセットアップやデータテーブルの作成方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### PostgreSQLのインストール

Dockerを使ってPostgreSQLをインストールし、Dockerイメージを起動します。

```bash
# PostgreSQLのDockerイメージを起動し、パスワードをpublicに設定
docker run --name PostgreSQL -p 5432:5432 -e POSTGRES_PASSWORD=public -d postgres

# コンテナにアクセス
docker exec -it PostgreSQL bash

# コンテナ内でPostgreSQLサーバーに接続し、パスワードを入力
psql -U postgres -W

# データベースを作成し、選択
CREATE DATABASE emqx_data;

\c emqx_data;
```

### データテーブルの作成

以下のSQL文を使い、PostgreSQLデータベースにクライアントID、トピック、ペイロード、メッセージの作成時間を保存するデータテーブル`t_mqtt_msg`を作成します。

```sql
CREATE TABLE t_mqtt_msg (
  id SERIAL primary key,
  msgid character varying(64),
  sender character varying(64),
  topic character varying(255),
  qos integer,
  retain integer,
  payload text,
  arrived timestamp without time zone
);
```

クライアントID、イベントタイプ、作成時間を保存するデータテーブル`emqx_client_events`を作成するSQL文は以下の通りです。

```sql
CREATE TABLE emqx_client_events (
  id SERIAL primary key,
  clientid VARCHAR(255),
  event VARCHAR(255),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

## コネクターの作成

PostgreSQLシンクを追加する前に、PostgreSQLコネクターを作成する必要があります。ここではEMQXとPostgreSQLがローカルマシンで動作していることを前提としています。リモートで動作している場合は設定を適宜調整してください。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connector**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**PostgreSQL**を選択し、**Next**をクリックします。
4. コネクターの名前を入力します。名前は英数字の組み合わせとしてください（例：`my_psql`）。
5. 接続情報を入力します。

   - **Server Host**：`127.0.0.1:5432`（PostgreSQLサーバーがリモートの場合は実際のホスト名を入力）
   - **Database Name**：`emqx_data`
   - **Username**：`postgres`
   - **Password**：`public`
   - **Enable TLS**：暗号化接続を行う場合はトグルスイッチをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照してください。
6. 詳細設定（任意）：詳細は[シンクの機能](./data-bridges.md#features-of-sink)を参照してください。
7. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがPostgreSQLサーバーに接続できるかテスト可能です。
8. ページ下部の**Create**ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックしてコネクター一覧に戻るか、**Create Rule**をクリックしてシンクを使ったルールの作成を続行できます。ルール作成の詳細は[メッセージ保存用PostgreSQLシンク付きルールの作成](#create-a-rule-with-postgresql-sink-for-message-storage)および[イベント記録用PostgreSQLシンク付きルールの作成](#create-a-rule-with-postgresql-for-events-recording)を参照してください。

:::tip 注意事項

EMQX v5.7.1で**Disable Prepared Statements**オプションが導入されました。PGBouncerのトランザクションモードやSupabaseなど、プリペアドステートメントをサポートしないPostgreSQLサービスを利用する場合は、詳細設定でこのオプションを有効にしてください。

:::

## メッセージ保存用PostgreSQLシンク付きルールの作成

このセクションでは、ダッシュボードでソースMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みのシンク経由でPostgreSQLテーブル`t_mqtt_msg`に保存するルールの作成方法を示します。

1. ダッシュボードの**Integration** -> **Rules**ページにアクセスします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、SQLエディターにルールを入力します。ここではトピック`t/#`のMQTTメッセージをPostgreSQLに保存する例を示します。ルールのSELECT部分で選択するフィールドは、SQLテンプレートで使用する変数をすべて含むようにしてください。ルールSQLは以下の通りです。

   ```sql
   SELECT
   *
   FROM
   "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を使ってSQLルールの学習とテストが可能です。

   :::

4. + **Add Action**ボタンをクリックし、ルールによりトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをPostgreSQLに送信します。

5. **Type of Action**ドロップダウンからPostgreSQLを選択し、**Action**ドロップダウンはデフォルトの`Create Action`のままにするか、既存のPostgreSQLアクションを選択します。本例では新規シンクを作成し、ルールに追加します。

6. シンクの名前と説明をフォームに入力します。

7. **Connector**ドロップダウンから先に作成した`my_psql`を選択します。ドロップダウン横のボタンから新規コネクター作成も可能です。設定パラメーターは[コネクターの作成](#コネクターの作成)を参照してください。

8. **SQL Template**を設定します。以下のSQL文を使ってデータを挿入します。

   注意：これは[プリプロセス済みSQL](./data-bridges.md#prepared-statement)のため、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
   INSERT INTO t_mqtt_msg(msgid, sender, topic, qos, payload, arrived) VALUES(
     ${id},
     ${clientid},
     ${topic},
     ${qos},
     ${payload},
     TO_TIMESTAMP((${timestamp} :: bigint)/1000)
   )
   ```

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：詳細は[シンクの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**を押してシンクがPostgreSQLサーバーに接続できるかテスト可能です。

12. **Create**ボタンをクリックしてシンク設定を完了します。新しいシンクが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認後、**Create**ボタンをクリックしてルールを生成します。

これでルールが正常に作成されました。**Integration** -> **Rules**ページで新規ルールを確認でき、**Action (Sink)**タブで新規PostgreSQLシンクも確認できます。

また、**Integration** -> **Flow Designer**を開くとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されPostgreSQLに書き込まれている様子を可視化できます。

## イベント記録用PostgreSQLシンク付きルールの作成

このセクションでは、クライアントのオンライン/オフライン状態を記録し、イベントデータを設定済みシンク経由でPostgreSQLテーブル`emqx_client_events`に保存するルールの作成方法を示します。

手順は[メッセージ保存用PostgreSQLシンク付きルールの作成](#メッセージ保存用postgresqlシンク付きルールの作成)とほぼ同様ですが、SQLテンプレートとSQLルールが異なります。

オンライン/オフライン状態記録用のルールSQL文は以下の通りです。

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

イベント記録用のSQLテンプレートは以下の通りです。

注意：これは[プリプロセス済みSQL](./data-bridges.md#prepared-statement)のため、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  TO_TIMESTAMP((${timestamp} :: bigint)/1000)
)
```

## ルールのテスト

MQTTXを使ってトピック`t/1`にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello PostgreSQL" }'
```

2つのシンクの稼働状況を確認します。メッセージ保存用シンクには新規の受信・送信メッセージが1件ずつあるはずです。イベント記録用シンクには2件のイベントレコードがあります。

`t_mqtt_msg`データテーブルにデータが書き込まれているか確認します。

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello PostgreSQL" } | 2023-01-19 07:10:32
(1 row)

```

`emqx_client_events`テーブルにデータが書き込まれているか確認します。

```bash
emqx_data=# select * from emqx_client_events;
 id | clientid |        event        |     created_at
----+----------+---------------------+---------------------
  3 | emqx_c   | client.connected    | 2023-01-19 07:10:32
  4 | emqx_c   | client.disconnected | 2023-01-19 07:10:32
(2 rows)

```
