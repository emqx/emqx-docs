# Oracle DatabaseへのMQTTデータ取り込み

[Oracle Database](https://www.oracle.com/database/)は、企業や組織のさまざまな規模・種類で広く利用されている主要なリレーショナル商用データベースソリューションの一つです。EMQXはOracle Databaseとの統合をサポートしており、MQTTメッセージやクライアントイベントをOracle Databaseに保存することが可能です。これにより、複雑なデータパイプラインや分析プロセスの構築、データ管理・分析、またはデバイス接続の管理やERPやCRMなど他の企業システムとの連携が実現できます。

本ページでは、EMQXとOracle Database間のデータ統合について、実践的な手順を交えながら包括的に解説します。

## 動作の仕組み

Oracle Databaseとのデータ統合は、MQTTベースのIoTデータとOracle Databaseの強力なデータ保存機能をつなぐEMQXの標準機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからOracle Databaseへのデータ取り込みが簡素化され、複雑なコーディングを必要としません。

以下の図は、EMQXとOracle Database間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration Oracel](./assets/emqx-integration-oracle.png)

Oracle DatabaseへのMQTTデータ取り込みは以下のように動作します：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、機械、センサー、製品ラインの稼働状態や計測値、トリガーイベントに基づくリアルタイムMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理が開始されます。  
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールにより処理されます。ルールは事前定義された条件に基づき、Oracle Databaseへルーティングすべきメッセージを決定します。ペイロード変換を指定するルールがある場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Oracle Databaseへのデータ取り込み**：ルールによりメッセージのOracle Databaseへの書き込みがトリガーされます。SQLテンプレートを用いて、ルール処理結果からデータを抽出しSQLを構築、Oracle Databaseへ送信して実行することで、メッセージの特定フィールドを対応するテーブル・カラムに書き込みまたは更新します。
4. **データの保存と活用**：データがOracle Databaseに保存されることで、企業はそのクエリ機能を活用してさまざまなユースケースに対応できます。たとえば、Oracleの高度な分析・予測機能を利用し、IoTデータから価値ある情報や洞察を抽出できます。

## 特長とメリット

Oracle Databaseとのデータ統合は、効率的なデータ伝送、保存、活用を実現するための多様な特長とメリットを提供します：

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリーム処理に最適化されており、ソースシステムからOracle Databaseへの効率的かつ信頼性の高いデータ伝送を実現します。即時の洞察とアクションが必要なユースケースに適しています。
- **高性能とスケーラビリティ**：EMQXのクラスターおよび分散アーキテクチャは、増加し続けるデバイス接続数やメッセージ送信量に対応可能です。Oracleはデータパーティショニング、データレプリケーション・冗長化、クラスタリング、高可用性など多様な拡張・スケーリングソリューションを提供し、柔軟で信頼性の高い高性能データベース環境を実現します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、Oracle Databaseに保存する前にデータを前処理できます。フィルタリング、ルーティング、集約、強化など多様な変換機能をサポートし、ニーズに応じたデータ整形が可能です。
- **簡単なデプロイと管理**：EMQXはデータソース設定、データ前処理ルール、Oracle Database保存設定を直感的に操作できるインターフェースを提供し、データ統合のセットアップと継続的な管理を簡素化します。
- **高度な分析**：Oracle Databaseの強力なSQLクエリ言語と複雑な分析関数のサポートにより、IoTデータから価値ある洞察を得られ、予測分析や異常検知などに活用できます。

## はじめる前に

このセクションでは、Oracle Databaseデータ統合の作成を開始する前に必要な準備、Oracle Databaseサーバーのセットアップやデータテーブルの作成方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Oracle Databaseサーバーのインストール

Dockerを使ってOracle Databaseサーバーをインストールし、Dockerイメージを起動します。

```bash
# ローカルでOracle DatabaseのDockerイメージを起動
docker run --name oracledb -p 1521:1521 -d oracleinanutshell/oracle-xe-11g:1.0.0

# リモートでOracle DatabaseのDockerイメージを起動
docker run --name oracledb -p 1521:1521 -e ORACLE_ALLOW_REMOTE=true -d oracleinanutshell/oracle-xe-11g:1.0.0

# パフォーマンス向上のため、ディスク非同期IOを無効化する場合
docker run --name oracledb -p 1521:1521 -e ORACLE_DISABLE_ASYNCH_IO=true -d oracleinanutshell/oracle-xe-11g:1.0.0

# コンテナにアクセス
docker exec -it oracledb bash

# デフォルトデータベース "XE" に接続
# ユーザー名: "system"
# パスワード: "oracle"
sqlplus
```

### データテーブルの作成

以下のSQL文を使って、メッセージID、クライアントID、トピック、QoS、リテインフラグ、メッセージペイロード、タイムスタンプを保存するデータテーブル `t_mqtt_msgs` をOracle Databaseに作成します。

```sql
CREATE TABLE t_mqtt_msgs (
  msgid VARCHAR2(64),
  sender VARCHAR2(64),
  topic VARCHAR2(255),
  qos NUMBER(1),
  retain NUMBER(1),
  payload NCLOB,
  arrived TIMESTAMP
);
```

また、クライアントID、イベントタイプ、作成日時を保存するデータテーブル `t_emqx_client_events` を以下のSQL文で作成します。

```sql
CREATE TABLE t_emqx_client_events (
  clientid VARCHAR2(255),
  event VARCHAR2(255),
  created_at TIMESTAMP
);
```

## コネクターの作成

このセクションでは、SinkをOracle Databaseサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとOracle Databaseを同一マシンで実行している場合を想定しています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Oracle Database** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。英数字の大文字・小文字の組み合わせが推奨されます（例：`my_oracle`）。
   - **Server Host**：Oracle Databaseサーバーがローカルの場合は `127.0.0.1:1521`、リモートの場合は実際のホスト名を入力します。
   - **Database Name**：`XE` を入力します。
   - **Oracle Database SID**：`XE` を入力します。
   - **Username**：`system` を入力します。
   - **Password**：`oracle` を入力します。
   - **Role**：Oracleデータベース接続に使用するロールを選択します。
     - **normal**：特別なロールを使用しません。
     - **sysdba**：高度な権限を持つシステムデータベース管理者ロールを使用します。
5. 詳細設定（任意）：詳細は[Features of Sink](./data-bridges.md#features-of-sink)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity** をクリックしてコネクターがOracle Databaseサーバーに接続可能かテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** を選択できます。後者を選ぶと、Oracle Databaseへ転送するデータやクライアントイベントを記録するルールを作成できます。詳細は[Create a Rule with Oracle Database Sink for Message Storage](#create-a-rule-with-oracle-database-sink-for-message-storage)および[Create a Rule with Oracle Database Sink for Events Recording](#create-a-rule-with-oracle-database-sink-for-events-recording)を参照してください。

## Oracle Database Sinkを使ったメッセージ保存ルールの作成

このセクションでは、ダッシュボード上でMQTTトピック `t/#` からのメッセージを処理し、処理結果を設定済みのSink経由でOracleデータテーブル `t_mqtt_msgs` に保存するルールの作成方法を示します。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` を入力し、**SQL Editor** に以下のSQL文を入力します。これはトピック `t/#` 配下のMQTTメッセージをOracle Databaseに保存する意味です。

   注意：独自のSQL文を指定する場合は、Sinkが要求するすべてのフィールドを`SELECT`句に含めていることを確認してください。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   初心者の場合は、**SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストが可能です。

4. + **Add Action** ボタンをクリックして、ルール発動時にトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをOracle Databaseに送信します。

5. **Type of Action** ドロップダウンから `Oracle Database` を選択します。**Action** はデフォルトの `Create Action` のままにします。既存のOracle Database Sinkがあれば選択可能ですが、ここでは新規Sinkを作成します。

6. Sinkの名前を入力します。英数字の大文字・小文字の組み合わせが推奨されます。

7. **Connector** ドロップダウンから先ほど作成した `my_oracle` を選択します。新規コネクター作成は隣のボタンから可能です。設定パラメータは[Create a Connector](#create-a-connector)を参照してください。

8. 利用する機能に応じて**SQL Template**を設定します。

   注意：これは[プリプロセス済みSQL](./data-bridges.md#prepared-statement)なので、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

   ```sql
   INSERT INTO t_mqtt_msgs(msgid, sender, topic, qos, retain, payload, arrived) VALUES(
     ${id},
     ${clientid},
     ${topic},
     ${qos},
     ${flags.retain},
     ${payload},
     TO_TIMESTAMP('1970-01-01 00:00:00', 'YYYY-MM-DD HH24:MI:SS') + NUMTODSINTERVAL(${timestamp}/1000, 'SECOND')
   )
   ```

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[Fallback Actions](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Features of Sink](./data-bridges.md#features-of-sink)の関連設定情報を参照してください。

11. **Create**をクリックする前に、**Test Connectivity** をクリックしてSinkがOracle Databaseサーバーに接続可能かテストできます。

12. **Create** ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule** ページに戻り、設定内容を確認してから **Create** ボタンをクリックしルールを生成します。

これでOracle Database Sink経由でデータ転送を行うルールが作成されました。**Integration** -> **Rules** ページで新規ルールを確認できます。**Actions(Sink)** タブをクリックすると新しいOracle Database Sinkが表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` 配下のメッセージがルール `my_rule` により解析されOracle Databaseに送信・保存されている様子を確認できます。

## Oracle Database Sinkを使ったイベント記録ルールの作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みのSink経由でOracleデータテーブル `t_emqx_client_events` に保存するルールの作成方法を示します。

ルール作成手順は[Oracle Database Sinkを使ったメッセージ保存ルールの作成](#oracle-database-sinkを使ったメッセージ保存ルールの作成)とほぼ同様ですが、SQLルール文とSQLテンプレートが異なります。

オンライン／オフライン状態記録用のSQLルール文は以下の通りです：

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

Sink用のSQLテンプレートは以下の通りです：

注意：これは[プリプロセス済みSQL](./data-bridges.md#prepared-statement)なので、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

```sql
INSERT INTO t_emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  TO_TIMESTAMP('1970-01-01 00:00:00', 'YYYY-MM-DD HH24:MI:SS') + NUMTODSINTERVAL(${timestamp}/1000, 'SECOND')
)
```

## ルールのテスト

MQTTXを使ってトピック `t/1` にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello Oracle Database" }'
```

2つのSinkの稼働状況を確認すると、新規の受信メッセージ1件と送信メッセージ1件、イベントレコード2件があるはずです。

`t_mqtt_msgs` データテーブルにデータが書き込まれているか確認します。

```sql
SELECT * FROM t_mqtt_msgs;

MSGID                            SENDER TOPIC QOS RETAIN PAYLOAD                            ARRIVED
-------------------------------- ------ ----- --- ------ ---------------------------------- ----------------------------
0005FA6CE9EF9F24F442000048100002 emqx_c t/1   0   0      { "msg": "hello Oracle Database" } 28-APR-23 08.22.51.760000 AM
```

`t_emqx_client_events` テーブルにデータが書き込まれているか確認します。

```sql
SELECT * FROM t_emqx_client_events;

CLIENTID EVENT               CREATED_AT
-------- ------------------- ----------------------------
emqx_c   client.connected    28-APR-23 08.22.51.757000 AM
emqx_c   client.disconnected 28-APR-23 08.22.51.760000 AM
```
