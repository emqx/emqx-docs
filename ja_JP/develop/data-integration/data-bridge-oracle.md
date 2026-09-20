# Oracle DatabaseへのMQTTデータ取り込み

[Oracle Database](https://www.oracle.com/database/) は、企業や組織の規模や種類を問わず広く利用されている主要なリレーショナル商用データベースソリューションの一つです。EMQXはOracle Databaseとの統合をサポートしており、MQTTメッセージやクライアントイベントをOracle Databaseに保存できます。これにより、複雑なデータパイプラインや分析プロセスの構築、データ管理・解析、あるいはデバイス接続の管理やERPやCRMなどの他の企業システムとの連携が可能になります。

本ページでは、EMQXとOracle Database間のデータ統合について、実践的な作成および検証手順を含めて包括的に紹介します。

## 動作概要

Oracle Databaseとのデータ統合は、MQTTベースのIoTデータとOracle Databaseの強力なデータストレージ機能を橋渡しするためにEMQXに標準搭載された機能です。内蔵の[ルールエンジン](./rules.md)コンポーネントにより、EMQXからOracle Databaseへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとOracle Database間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration Oracel](./assets/emqx-integration-oracle.png)

Oracle DatabaseへのMQTTデータ取り込みは以下のように動作します：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、機械、センサー、製品ラインの稼働状態、計測値、またはトリガーされたイベントに基づくリアルタイムMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールにより処理されます。ルールは事前定義された条件に基づき、どのメッセージをOracle Databaseにルーティングするかを決定します。ペイロード変換を指定するルールがある場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **Oracle Databaseへのデータ取り込み**：ルールはメッセージをOracle Databaseに書き込むトリガーとなります。SQLテンプレートを用いて、ルール処理結果からデータを抽出しSQLを構築、Oracle Databaseに送信して実行します。これにより、メッセージの特定フィールドを対応するデータベースのテーブルやカラムに書き込んだり更新したりできます。
4. **データの保存と活用**：データがOracle Databaseに保存された後、企業はそのクエリ機能を活用して様々なユースケースに利用できます。例えば、Oracleの高度な分析や予測機能を利用して、IoTデータから価値ある情報や洞察を抽出できます。

## 特長とメリット

Oracle Databaseとのデータ統合は、効率的なデータ伝送、保存、活用を実現するための多彩な特長とメリットを提供します：

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリーム処理に最適化されており、ソースシステムからOracle Databaseへの効率的かつ信頼性の高いデータ伝送を保証します。即時の洞察やアクションが必要なユースケースに最適です。
- **高性能かつスケーラブル**：EMQXのクラスターおよび分散アーキテクチャは、増大するデバイス接続数やメッセージ送信量に対応可能です。Oracleはデータパーティショニング、データレプリケーションと冗長化、クラスター、高可用性など多様な拡張・スケーリングソリューションを提供し、柔軟かつ信頼性の高い高性能データベース環境を実現します。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを備えており、Oracle Databaseに保存する前にデータを前処理可能です。フィルタリング、ルーティング、集約、強化など多様なデータ変換機能をサポートし、ニーズに応じたデータ整形が可能です。
- **簡単なデプロイと管理**：EMQXはデータソースの設定、データ前処理ルール、Oracle Databaseの保存設定をユーザーフレンドリーなインターフェースで提供し、データ統合プロセスのセットアップと運用管理を簡素化します。
- **高度な分析機能**：Oracle Databaseの強力なSQLクエリ言語と複雑な分析関数のサポートにより、ユーザーはIoTデータから価値ある洞察を得られ、予測分析や異常検知などに活用できます。

## はじめる前に

このセクションでは、Oracle Databaseデータ統合の作成を開始する前に必要な準備として、Oracle Databaseサーバーのセットアップやデータテーブルの作成方法を説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Oracle Databaseサーバーのインストール

Dockerを用いてOracle Databaseサーバーをインストールし、Dockerイメージを起動します。

```bash
# ローカルでOracle Database Dockerイメージを起動する場合
docker run --name oracledb -p 1521:1521 -d oracleinanutshell/oracle-xe-11g:1.0.0

# リモートでOracle Database Dockerイメージを起動する場合
docker run --name oracledb -p 1521:1521 -e ORACLE_ALLOW_REMOTE=true -d oracleinanutshell/oracle-xe-11g:1.0.0

# パフォーマンス向上のため、ディスク非同期IOを無効化したい場合：
docker run --name oracledb -p 1521:1521 -e ORACLE_DISABLE_ASYNCH_IO=true -d oracleinanutshell/oracle-xe-11g:1.0.0

# コンテナにアクセス
docker exec -it oracledb bash

# デフォルトデータベース "XE" に接続
# ユーザー名: "system"
# パスワード: "oracle"
sqlplus
```

### データテーブルの作成

以下のSQL文を使用して、Oracle DatabaseにメッセージID、クライアントID、トピック、QoS、リテインフラグ、メッセージペイロード、タイムスタンプを格納するデータテーブル `t_mqtt_msgs` を作成します。

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

また、クライアントID、イベントタイプ、作成日時を格納するデータテーブル `t_emqx_client_events` を作成するには、以下のSQL文を使用します。

```sql
CREATE TABLE t_emqx_client_events (
  clientid VARCHAR2(255),
  event VARCHAR2(255),
  created_at TIMESTAMP
);
```

## コネクターの作成

このセクションでは、SinkをOracle Databaseサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとOracle Databaseをローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **Oracle Database** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。大文字・小文字の英数字の組み合わせで、例：`my_oracle`
   - **Server Host**：`127.0.0.1:1521` またはOracle Databaseサーバーがリモートの場合は実際のホスト名を入力
   - **Database Name**：`XE`
   - **Oracle Database SID**：`XE`
   - **Username**：`system`
   - **Password**：`oracle`
   - **Role**：Oracleデータベース接続に使用するロールを選択
     - **normal**：特別なロールを使用しない
     - **sysdba**：高度な権限を持つシステムデータベース管理者ロールを使用
5. 詳細設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity** をクリックしてコネクターがOracle Databaseサーバーに接続できるかテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** または **Create Rule** をクリックして、Oracle Databaseへの転送やクライアントイベント記録を指定するルール作成に進めます。詳細は[メッセージ保存用Oracle Database Sinkのルール作成](#create-a-rule-with-oracle-database-sink-for-message-storage)および[イベント記録用Oracle Database Sinkのルール作成](#create-a-rule-with-oracle-database-sink-for-events-recording)を参照してください。

## メッセージ保存用Oracle Database Sinkのルール作成

このセクションでは、ソースMQTTトピック `t/#` からのメッセージを処理し、処理済みデータを設定済みSink経由でOracleのデータテーブル `t_mqtt_msgs` に保存するルールをDashboardで作成する方法を示します。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` を入力し、**SQL Editor** に以下のSQL文を入力します。これはトピック `t/#` 配下のMQTTメッセージをOracle Databaseに保存することを意味します。

   注意：独自のSQL文を指定する場合は、Sinkが必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   注意：初心者の方は **SQL Examples** と **Enable Test** をクリックしてSQLルールを学習・テストできます。

4. + **Add Action** ボタンをクリックし、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをOracle Databaseに送信します。

5. **Type of Action** ドロップダウンリストから `Oracle Database` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みのOracle Database Sinkを選択することも可能ですが、ここでは新しいSinkを作成します。

6. Sinkの名前を入力します。名前は大文字・小文字の英数字の組み合わせにしてください。

7. **Connector** ドロップダウンボックスから先ほど作成した `my_oracle` を選択します。隣のボタンをクリックして新しいコネクターを作成することも可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

8. 利用する機能に基づいて**SQL Template**を設定します。

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

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)の関連設定情報を参照してください。

11. **Create**をクリックする前に、**Test Connectivity** をクリックしてSinkがOracle Databaseサーバーに接続できるかテストできます。

12. **Create** ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認してから**Create**をクリックしルールを生成します。

これでOracle Database Sinkを介したデータ転送ルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成したルールを確認できます。**Actions(Sink)** タブをクリックすると新しいOracle Database Sinkが表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` 配下のメッセージがルール `my_rule` によって解析されOracle Databaseに送信・保存されていることがわかります。

## イベント記録用Oracle Database Sinkのルール作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みSink経由でOracleのデータテーブル `t_emqx_client_events` に保存するルール作成方法を示します。

ルール作成手順は[メッセージ保存用Oracle Database Sinkのルール作成](#メッセージ保存用oracle-database-sinkのルール作成)とほぼ同様ですが、SQLルール文とSQLテンプレートが異なります。

オンライン／オフライン状態記録用のSQLルール文は以下の通りです：

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

Sink用SQLテンプレートは以下の通りです：

注意：これは[プリプロセス済みSQL](./data-bridges.md#prepared-statement)なので、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

```sql
INSERT INTO t_emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  TO_TIMESTAMP('1970-01-01 00:00:00', 'YYYY-MM-DD HH24:MI:SS') + NUMTODSINTERVAL(${timestamp}/1000, 'SECOND')
)
```

## ルールのテスト

MQTTXを使用してトピック `t/1` にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello Oracle Database" }'
```

2つのSinkの稼働状況を確認すると、新規の受信メッセージ1件、新規の送信メッセージ1件、イベントレコード2件があるはずです。

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
