# CockroachDBへのMQTTデータ取り込み

[CockroachDB](https://www.cockroachlabs.com/product/overview/)は、分散型のPostgreSQL互換データベースで、フルマネージドのクラウドサービス（CockroachDB Cloud）またはセルフホスト型のデプロイメントとして利用可能です。高いレジリエンス、水平スケーラビリティ、完全なSQL互換性を必要とするグローバルアプリケーション向けに設計されています。EMQXはCockroachDBとスムーズに統合し、IoTデバイスからのMQTTデータをリアルタイムでキャプチャして保存します。これにより、グローバル展開における高速かつ信頼性の高いデータ取り込み、Raftベースのレプリケーションによる一貫性のあるデータ保証、そして運用および分析のための低レイテンシな読み取りを実現します。

本ページでは、EMQXとCockroachDB間のデータ統合について包括的に紹介し、データ統合の作成および検証に関する実践的な手順を提供します。

## 動作概要

EMQXにおけるCockroachDBデータ統合は、MQTTベースのIoTデータストリームをCockroachDBの分散型PostgreSQL互換データベースに直接取り込む組み込み機能です。EMQXの組み込み[ルールエンジン](./rules.md)を利用することで、複雑なカスタムコードを書かずに、CockroachDBへ直接データを取り込み、グローバルに一貫した保存とリアルタイムクエリを実現できます。

CockroachDBの共有なし（shared-nothing）分散アーキテクチャは、Raftベースのコンセンサスを用いて複数のノードやリージョン間でデータを自動的にレプリケートし、障害時でも強い一貫性を維持します。これにより、IoTデータは常に安全かつ同期され、利用可能な状態が保たれます。

以下の図は、EMQXとCockroachDB間のデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration CockroachDB](./assets/cockroachdb_architecture.png)

MQTTデータのCockroachDBへの取り込みは以下のように動作します。

1. **IoTデバイスがEMQXに接続**: IoTデバイスがMQTTプロトコルを介して正常に接続されると、オンラインイベントがトリガーされます。イベントにはデバイスID、送信元IPアドレス、その他属性情報が含まれます。
2. **メッセージのパブリッシュと受信**: デバイスは特定のトピックにテレメトリやステータスデータをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理が開始されます。
3. **ルールエンジンによるメッセージ処理**: EMQXのルールエンジンは、トピックやメッセージ内容に基づいて定義されたルールとイベント・メッセージをマッチングし処理します。処理内容には、データ変換（例：JSONからSQL用フォーマットへの変換）、フィルタリング、文脈情報によるデータ強化などが含まれ、データベースへの挿入準備が行われます。
4. **CockroachDBへの書き込み**: マッチしたルールはCockroachDBに対するSQL実行をトリガーします。SQLテンプレートを用いて、処理済みデータのフィールドをCockroachDBのテーブルやカラムにマッピングします。CockroachDBの分散SQL実行およびベクトル化クエリエンジンにより、高スループットな書き込みと低レイテンシな分析クエリが可能です。マルチリージョン展開ではジオパーティションも活用できます。

イベントおよびメッセージデータがCockroachDBに書き込まれた後は、

- CockroachDBをGrafanaなどのツールに接続し、ライブのIoTメトリクスを表示するダッシュボードやチャートを作成できます。
- デバイス管理プラットフォームやAI/MLモデルと連携して、ヘルスチェック、異常検知、アラート発報を行えます。
- CockroachDBの分散クエリエンジンを活用し、ライブIoTデータに対する集計、結合、時系列分析を実行しつつ、新しいテレメトリの処理を並行して継続できます。

## 特長とメリット

CockroachDBとのデータ統合により、以下の特長と利点が得られます。

- **柔軟なイベント処理**: EMQXルールエンジンを利用し、CockroachDBはデバイスのライフサイクルイベント（接続、切断、ステータス変化）を低レイテンシで保存・処理できます。CockroachDBの分散実行と自動リバランシングにより、イベントデータは高可用性を維持し、リアルタイムで障害検知や異常検知、トレンド分析が可能です。
- **メッセージ変換**: メッセージはEMQXルールを通じて高度に処理・変換されてからCockroachDBに書き込まれるため、保存データは最初から分析に適した形となります。これによりクエリの複雑さが軽減され、下流処理の効率化が図れます。
- **SQLテンプレートによる柔軟なデータ操作**: EMQXのSQLテンプレートマッピングを通じて、構造化されたIoTデータをCockroachDBのテーブル・カラムに挿入または更新できます。PostgreSQL互換により標準SQL、JSONBストレージ、インデックスをサポートし、ベクトル化実行エンジンによる高速分析や、フォロワーリードによる低レイテンシなリージョンローカルアクセスが可能です。
- **業務プロセスの統合**: CockroachDBのPostgreSQL互換性により、ERP、CRM、GISなどの業務システムと統合できます。EMQXと組み合わせることで、複雑なETLパイプラインを構築せずにイベント駆動の自動化やクロスシステムオーケストレーションを実現可能です。
- **高度な地理空間機能**: PostgreSQL拡張のPostGISなどを通じて、CockroachDBは地理空間データの保存、インデックス作成、クエリをサポートします。これにより、ジオフェンシング、位置情報ベースのアラート、ルート追跡、リアルタイム資産監視がEMQXの信頼性の高いIoTデータ取り込みと連携して実現できます。
- **組み込みのメトリクスと監視**: EMQXは各CockroachDBシンクのランタイムメトリクス（メッセージ数、成功/失敗率、スループット）を提供し、CockroachDBは組み込みの可観測性ツールを備え、PrometheusやGrafanaと連携して詳細なパフォーマンスおよびヘルス監視を可能にします。

## はじめる前に

このセクションでは、CockroachDB統合を作成する前に必要な準備事項を説明します。CockroachDBのデプロイメントやデータベース・テーブルの作成方法についても解説します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### CockroachDBでのデータベースおよびテーブル作成

EMQXでCockroachDBコネクターを作成する前に、CockroachDBクラスターが稼働していること、およびIoTデータを保存するためのデータベースとテーブルが準備されていることを確認してください。

1. CockroachDBクラスターを作成します。

   - CockroachDB Cloudの場合は、[CockroachDB Cloudドキュメント](https://www.cockroachlabs.com/docs/cockroachcloud)に従ってクラスターをプロビジョニングしてください。
   - セルフホスト型の場合は、[インストールガイド](https://www.cockroachlabs.com/docs/stable/install-cockroachdb-linux.html)に従ってください。

2. EMQX用の専用SQLユーザーを作成します。詳細は[CockroachDBユーザー管理ガイド](https://www.cockroachlabs.com/docs/cockroachcloud/managing-access#manage-sql-users-on-a-cluster)を参照してください。本例では`emqx_user`という名前のSQLユーザーを作成し、後でCockroachDBコネクターの設定に使用します。このユーザーには以下の権限が必要です。

   - 対象データベースへの接続権限
   - テーブル作成権限
   - EMQXデータテーブルへの読み書き権限

3. [データベース作成](https://www.cockroachlabs.com/docs/cockroachcloud/managing-access#manage-sql-users-on-a-cluster)の手順に従い、データベースを作成します。本例では`emqx_data`という名前のデータベースを使用します。

4. `emqx_data`データベースに接続し、MQTTメッセージとクライアントイベントデータを保存するための2つのテーブルを作成します。[テーブル作成](https://www.cockroachlabs.com/docs/v25.3/schema-design-table#create-a-table)の手順を参照してください。

   - 以下のSQL文で、クライアントID、トピック、QoS、ペイロード、到着時刻などのメタデータを含むMQTTメッセージ保存用の`t_mqtt_msg`テーブルを作成します。

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

   - 以下のSQL文で、クライアントのオンライン/オフラインイベントをタイムスタンプ付きで保存する`emqx_client_events`テーブルを作成します。

     ```sql
     CREATE TABLE emqx_client_events (
       id SERIAL primary key,
       clientid VARCHAR(255),
       event VARCHAR(255),
       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
     );
     ```

## CockroachDBコネクターの作成

CockroachDBシンクを追加する前に、EMQXでCockroachDBコネクターを作成する必要があります。コネクターは、EMQXがセルフホスト型またはCockroachDB Cloudのクラスターに接続する方法を定義します。

1. EMQXダッシュボードで、**Integration** -> **Connector** に移動します。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **CockroachDB** を選択し、**Next** をクリックします。
4. コネクター名を入力します。名前は英数字で始まり、英数字、ハイフン、アンダースコアを含めることができます。例：`my_cockroachdb`
5. 接続情報を入力します。

   - **Server Host**: CockroachDBクラスターのホスト名またはIPアドレス
     - **CockroachDB Cloud**: CockroachDB Cloudコンソールで提供される接続文字列のホスト値を使用（例：`free-tier.gcp-us-central1.cockroachlabs.cloud`）
     - **セルフホスト型**: CockroachDBが稼働しているアドレスを使用（例：ローカルなら`127.0.0.1`、サーバーのパブリック/プライベートIPなど）
   - **Database Name**: EMQXがデータを保存する対象データベース名。本例では`emqx_data`
   - **Username**: CockroachDBの認証・識別に使用するSQLユーザー名。本例では`emqx_user`
   - **Password**: `emqx_user`のパスワード
   - **Enable TLS**: 暗号化接続を確立する場合はトグルスイッチをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。
6. 詳細設定（任意）：接続プールサイズ、アイドルタイムアウト、リクエストタイムアウトなどの追加接続プロパティを設定できます。詳細は[シンクの機能](./data-bridges.md#features-of-sink)を参照してください。
7. **Test Connectivity** をクリックして、EMQXが指定した設定でCockroachDBクラスターに正常に接続できるか確認します。
8. **Create** をクリックしてコネクターを保存します。
9. 作成後は以下のいずれかを選択できます。

   - **Back to Connector List** をクリックして全コネクター一覧を表示
   - **Create Rule** をクリックして、このコネクターを使用するルールをすぐに作成

   詳細な例は以下を参照してください。

   - [メッセージ保存用CockroachDBシンクのルール作成](#create-a-rule-with-cockroachdb-sink-for-message-storage)
   - [イベント記録用CockroachDBシンクのルール作成](#create-a-rule-with-cockroachdb-sink-for-events-recording)

## メッセージ保存用CockroachDBシンクのルール作成

このセクションでは、ダッシュボードでソースMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みのCockroachDBシンク経由で`t_mqtt_msg`テーブルに保存するルールの作成方法を示します。

1. ダッシュボードの **Integration** -> **Rules** ページに移動します。
2. ページ右上の **Create** をクリックします。
3. ルールIDに`my_rule`を入力し、SQLエディターにルールを記述します。ここでは`t/#`トピックのMQTTメッセージをCockroachDBに保存するため、ルールのSELECT句でSQLテンプレートで使用するすべての変数を含むフィールドを選択してください。ルールSQLは以下の通りです。

   ```sql
   SELECT
   *
   FROM
   "t/#"
   ```

   ::: tip

   初心者の方は、**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールの学習とテストを行うことができます。

   :::

4. + **Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理されたデータをCockroachDBに送信します。
5. **Type of Action** ドロップダウンからCockroachDBを選択し、**Action** ドロップダウンはデフォルトの`Create Action`のままにするか、既存のCockroachDBアクションを選択できます。本例では新規シンクを作成してルールに追加します。
6. シンクの名前と説明をフォームに入力します。
7. **Connector** ドロップダウンから先ほど作成した`my_cockroachdb`を選択します。新しいコネクターを作成する場合は、ドロップダウン横のボタンをクリックしてください。設定パラメータの詳細は[コネクターの作成](#create-a-cockroachdb-connector)を参照してください。
8. **SQL Template** を設定します。以下のSQL文を使用してデータを挿入します。

   注意：これは[プリペアドステートメント](./data-bridges.md#prepared-statement)のため、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

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
11. **Create**をクリックする前に、**Test Connectivity**をクリックしてシンクがCockroachDBクラスターに接続できるかテストできます。
12. **Create** ボタンをクリックしてシンク設定を完了します。新しいシンクが**Action Outputs**に追加されます。
13. **Create Rule**ページで設定内容を確認し、**Save**をクリックしてルールを生成します。

これでルールが正常に作成されました。**Integration** -> **Rules** ページで新規作成したルールを確認でき、**Action (Sink)** タブで新規CockroachDBシンクも確認できます。

また、**Integration** -> **Flow Designer** を開くとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されてCockroachDBに書き込まれている様子を可視化できます。

## イベント記録用CockroachDBシンクのルール作成

このセクションでは、クライアントのオンライン/オフライン状態を記録し、イベントデータを設定済みのCockroachDBシンク経由で`emqx_client_events`テーブルに保存するルールの作成方法を示します。

手順は[メッセージ保存用CockroachDBシンクのルール作成](#create-a-rule-with-cockroachdb-sink-for-message-storage)とほぼ同じですが、SQLテンプレートとSQLルールが異なります。

オンライン/オフライン状態記録用のSQLルールは以下の通りです。

```sql
SELECT
  *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

イベント記録用のSQLテンプレートは以下の通りです。

注意：これは[プリペアドステートメント](./data-bridges.md#prepared-statement)のため、フィールドは引用符で囲まず、文末にセミコロンを付けないでください。

```sql
INSERT INTO emqx_client_events(clientid, event, created_at) VALUES (
  ${clientid},
  ${event},
  TO_TIMESTAMP((${timestamp} :: bigint)/1000)
)
```

## ルールのテスト

MQTTXを使用してトピック`t/1`にメッセージを送信し、オンライン/オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello CockroachDB" }'
```

2つのシンクの稼働状況を確認します。メッセージ保存用シンクでは新規の受信メッセージ1件と送信メッセージ1件があるはずです。イベント記録用シンクでは2件のイベントレコードがあります。

`t_mqtt_msg`データテーブルにデータが書き込まれているか確認します。

```bash
emqx_data=# select * from t_mqtt_msg;
 id |              msgid               | sender | topic | qos | retain |            payload
        |       arrived
----+----------------------------------+--------+-------+-----+--------+-------------------------------+---------------------
  1 | 0005F298A0F0AEE2F443000012DC0002 | emqx_c | t/1   |   0 |        | { "msg": "hello CockroachDB" } | 2023-01-19 07:10:32
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
