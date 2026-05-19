# AlloyDBへのMQTTデータ取り込み

[AlloyDB for PostgreSQL](https://cloud.google.com/products/alloydb?hl=en) は、Google Cloudが提供するフルマネージドのPostgreSQL互換データベースサービスで、エンタープライズ向けの高負荷ワークロードに対応しています。EMQXはAlloyDBとのシームレスな統合をサポートしており、IoTデバイスからのMQTTデータをリアルタイムに取り込み、保存することが可能です。EMQXの効率的なメッセージルーティングと、AlloyDBの高スループットなトランザクション処理能力およびHybrid Transactional/Analytical Processing（HTAP）エンジンによるリアルタイム分析を活用することで、デバイスの状態取得、イベントログ記録、洞察に富んだ分析を行う強力なパイプラインを構築できます。

本ページでは、EMQXとAlloyDB間のデータ統合について包括的に解説し、データ統合の作成および検証手順を実践的に説明します。

## 動作概要

EMQXのAlloyDBデータ統合は組み込み機能であり、MQTTベースのIoTデータストリームを直接AlloyDBの高性能なPostgreSQL互換データベースに取り込みます。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからAlloyDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。AlloyDB Sinkを通じて、MQTTメッセージやクライアントイベントをAlloyDBに保存できます。さらにイベントによりAlloyDB内のデータ更新や削除操作をトリガーでき、デバイスのオンライン状態や接続履歴などの情報を記録可能です。

以下の図は、EMQXとAlloyDB間のデータ統合の典型的なアーキテクチャを示しています。



![EMQX Integration AlloyDB](./assets/alloydb_architecture.png)

AlloyDBへのMQTTデータ取り込みは以下のように動作します：

1. **IoTデバイスがEMQXに接続**：MQTTプロトコルを通じてIoTデバイスが正常に接続されると、オンラインイベントがトリガーされます。イベントにはデバイスID、送信元IPアドレスなどの属性情報が含まれます。
2. **メッセージのパブリッシュと受信**：デバイスは特定のトピックにテレメトリや状態データをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
3. **ルールエンジンによるメッセージ処理**：EMQXのルールエンジンは、トピックやメッセージ内容に基づき定義されたルールにマッチさせてイベントやメッセージを処理します。処理内容は、データ変換（例：JSONからSQL用フォーマットへの変換）、フィルタリング、コンテキスト情報によるデータ強化などが含まれ、データベース挿入前に行われます。
4. **AlloyDBへの書き込み**：マッチしたルールはAlloyDBに対するSQL実行をトリガーします。SQLテンプレートを使用して、処理済みデータのフィールドをAlloyDBのテーブルやカラムにマッピングします。AlloyDBは並列クエリ実行および組み込みのカラムナエンジンによる最適化ストレージをサポートしているため、高速なデータ挿入と即時の分析クエリが可能です。

イベントおよびメッセージデータがAlloyDBに書き込まれた後は、AlloyDBに接続してデータを読み取り、以下のような柔軟なアプリケーション開発が可能です：

- Grafanaなどの可視化ツールに接続し、データに基づくチャート作成やデータ変化の表示
- AlloyDBをデバイス管理システムや分析モデルと統合し、デバイスのヘルス監視、異常検知、アラート発動
- AlloyDBのHTAP機能を活用し、ライブIoTデータに対して複雑な分析（集計、結合、時系列クエリ）を実行しつつ、新しいデバイステレメトリをリアルタイムに処理

## 特長と利点

AlloyDBとのデータ統合により、以下の特長と利点をビジネスにもたらします：

- **柔軟なイベント処理**：EMQXルールエンジンを活用し、デバイスのライフサイクルイベント（接続、切断、状態変化）を低レイテンシでAlloyDBに保存・処理可能です。AlloyDBの並列クエリ実行と独立スケーリング機能と組み合わせることで、リアルタイムにイベントデータを分析し、デバイス障害や異常、利用傾向を検出できます。
- **メッセージ変換**：EMQXルールによる高度な処理・変換を経てからAlloyDBに書き込むため、保存や利用がより便利になります。
- **SQLテンプレートによる柔軟なデータ操作**：EMQXのSQLテンプレートマッピングを通じて、構造化されたIoTデータをAlloyDBのテーブルやカラムに挿入・更新可能です。AlloyDBのPostgreSQL互換性により標準SQLやJSONBストレージ、インデックスをサポートし、AI駆動のインデックス最適化によりクエリ性能を自動的に向上させます。
- **業務プロセス統合**：AlloyDBのPostgreSQLエコシステム互換性により、Google Cloud上またはオンプレミスのERP、CRM、GIS、カスタム業務システムと直接統合可能です。EMQXと組み合わせることで、複雑なデータパイプラインなしにイベント駆動の自動化や業務プロセスオーケストレーションを実現できます。
- **高度な地理空間機能**：PostgreSQL拡張のPostGISなどを通じて、AlloyDBは地理空間データの保存、インデックス作成、クエリをサポートし、ジオフェンシング、ルート追跡、位置情報分析を可能にします。EMQXの信頼性の高いMQTT取り込みと組み合わせることで、車両追跡、資産監視などのリアルタイムIoT-GISソリューション構築が可能です。
- **組み込みのメトリクスと監視**：EMQXは各AlloyDB Sinkのランタイムメトリクスを提供し、AlloyDBはCloud Monitoringと連携してクエリ性能、ストレージ利用率、レプリカの健全性を監視し、エンドツーエンドの可観測性を確保します。

## はじめる前に

本セクションでは、AlloyDB統合の作成を開始する前に必要な準備、すなわちAlloyDBインスタンスの作成およびデータベースとデータテーブルの作成方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### AlloyDBでのデータベースとテーブルの作成

EMQXでAlloyDBコネクターを作成する前に、AlloyDBインスタンスが利用可能であり、IoTデータを保存するための必要なデータベースとテーブルが作成されていることを確認してください。

[公式AlloyDBクイックスタートガイド](https://cloud.google.com/alloydb/docs/quickstart/create-and-connect)に従い、以下を実施します：

1. AlloyDBインスタンスを作成します。

   - このセットアップ時に、以下のユーザー認証情報を定義します：

     - **ユーザー名**：`emqx_user`（接続、挿入、更新、選択の権限を持つ必要があります）

     - **パスワード**：`your_password_here`

   - このユーザーはインスタンスプロビジョニング時、または後からSQL、Google Cloudコンソール、`gcloud` CLIで作成可能です。

2. インスタンス内にデータベースを作成します。ここでは例としてデータベース名を`emqx_data`とします。

3. PostgreSQL互換クライアント（例：`psql`）を使用して、上記の認証情報でデータベースに接続します。

4. MQTTメッセージとクライアントイベントデータを保存するための2つのテーブルを`emqx_data`データベースに作成します。

   - 以下のSQL文で、クライアントID、トピック、QoS、ペイロード、到着時間などのメタデータを含むMQTTメッセージ保存用の`t_mqtt_msg`テーブルを作成します：

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

   - 以下のSQL文で、クライアントのオンライン／オフラインイベントとタイムスタンプを保存する`emqx_client_events`テーブルを作成します：

     ```sql
     CREATE TABLE emqx_client_events (
       id SERIAL primary key,
       clientid VARCHAR(255),
       event VARCHAR(255),
       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
     );
     ```

## AlloyDBコネクターの作成

AlloyDB Sinkを追加する前に、EMQXでAlloyDBコネクターを作成します。コネクターはEMQXがGoogle CloudのAlloyDBインスタンスに接続する方法を定義します。

1. EMQXダッシュボードで、**Integration** -> **Connector** に移動します。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **AlloyDB** を選択し、**Next** をクリックします。

4. コネクター名を入力します。名前は英数字で始まり、英数字、ハイフン、アンダースコアを含めることができます。例：`my_alloydb`

5. 接続情報を入力します：

   - **Server Host**：Google Cloud上のAlloyDBインスタンスのホスト名またはIPアドレス
   - **Database Name**：EMQXがデータを書き込むAlloyDBの対象データベース名。例では`emqx_data`
   - **Username**：認証および識別に使うAlloyDBのデータベースユーザー名。例では`emqx_user`
   - **Password**：`emqx_user`のパスワード
   - **Enable TLS**：暗号化接続を確立する場合はトグルスイッチをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照してください。

6. 高度な設定（任意）：接続プールサイズ、アイドルタイムアウト、リクエストタイムアウトなどの追加設定を行えます。

7. **Test Connectivity** をクリックして、EMQXが指定した設定でAlloyDBインスタンスに正常に接続できるか確認します。

8. **Create** をクリックしてコネクターを保存します。

9. 作成後は以下のいずれかを選択できます：

   - **Back to Connector List** をクリックして全コネクター一覧を表示
   - **Create Rule** をクリックして、このコネクターを利用するルールを即座に作成

   詳細な例は以下を参照してください：

   - [メッセージ保存用のAlloyDB Sinkを使ったルール作成](#create-a-rule-with-alloydb-sink-for-message-storage)
   - [イベント記録用のAlloyDB Sinkを使ったルール作成](#create-a-rule-with-alloydb-sink-for-events-recording)

## メッセージ保存用のAlloyDB Sinkを使ったルール作成

本節では、ソースMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みSink経由でAlloyDBの`t_mqtt_msg`テーブルに保存するルールをダッシュボードで作成する方法を示します。

1. ダッシュボードの **Integration** -> **Rules** ページに移動します。

2. ページ右上の **Create** をクリックします。

3. ルールIDに`my_rule`を入力し、SQLエディターにルールを入力します。ここではトピック`t/#`のMQTTメッセージをAlloyDBに保存するため、ルールで選択するフィールド（SELECT句）がSQLテンプレートで使用する変数をすべて含むことを確認してください。ルールSQLは以下の通りです：

   ```sql
   SELECT
   *
   FROM
   "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQLルールの学習とテストを行うことができます。

   :::

4. + **Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをAlloyDBに送信します。

5. **Type of Action** ドロップダウンからAlloyDBを選択し、**Action** ドロップダウンはデフォルトの`Create Action`のままにするか、既存のAlloyDBアクションを選択します。本例では新規Sinkを作成し、ルールに追加します。

6. Sinkの名前と説明をフォームに入力します。

7. **Connector** ドロップダウンから、前に作成した`my_alloydb`を選択します。新規コネクターはドロップダウン横のボタンから作成可能です。設定パラメーターの詳細は[AlloyDBコネクターの作成](#create-an-alloydb-connector)を参照してください。

8. **SQLテンプレート**を設定します。以下のSQL文を使ってデータを挿入します。

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

10. **高度な設定（任意）**：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**でSinkがAlloyDBインスタンスに接続できるかテスト可能です。

12. **Create**ボタンをクリックしてSinkの設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページで設定内容を確認し、**Save**ボタンをクリックしてルールを生成します。

ルール作成が成功すると、**Integration** -> **Rules** ページで新規ルールを確認でき、**Action (Sink)** タブで新規AlloyDB Sinkも確認できます。

また、**Integration** -> **Flow Designer** でトポロジーを確認でき、トピック`t/#`のメッセージがルール`my_rule`で解析されてAlloyDBに書き込まれている様子を可視化できます。

## イベント記録用のAlloyDB Sinkを使ったルール作成

本節では、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みSink経由でAlloyDBの`emqx_client_events`テーブルに保存するルールの作成方法を示します。

手順は[メッセージ保存用のAlloyDB Sinkを使ったルール作成](#create-a-rule-with-alloydb-sink-for-message-storage)とほぼ同様ですが、SQLテンプレートとSQLルールが異なります。

オンライン／オフライン状態記録のSQLルール文は以下の通りです。

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

MQTTXを使ってトピック`t/1`にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello AlloyDB" }'
```

2つのSinkの稼働状況を確認します。メッセージ保存用Sinkでは新規受信メッセージ1件と送信メッセージ1件があるはずです。イベント記録用Sinkでは2件のイベントレコードが記録されます。

`t_mqtt_msg`データテーブルにデータが書き込まれているか確認します。

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello AlloyDB" } | 2023-01-19 07:10:32
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
