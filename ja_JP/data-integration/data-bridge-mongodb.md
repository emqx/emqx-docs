# MongoDBへのMQTTデータ取り込み

[MongoDB](https://www.mongodb.com/)は、スキーマ設計の柔軟性、スケーラビリティ、大量の構造化および半構造化データの保存能力で知られる主要なNoSQLデータベースです。EMQXとMongoDBを統合することで、ユーザーはMQTTメッセージやクライアントイベントをMongoDBに直接効率的に取り込むことができます。これにより、MongoDB内での長期的な時系列データの保存や高度なクエリが可能になります。この統合は一方向のデータフローを保証し、EMQXからのMQTTメッセージがMongoDBデータベースに書き込まれます。この強力な組み合わせは、IoTデータを効果的に管理したい企業にとって堅実な基盤となります。

本ページでは、EMQXとMongoDB間のデータ統合について包括的に紹介し、データ統合の作成と検証に関する実践的な手順を提供します。

## 動作概要

MongoDBデータ統合は、MQTTベースのIoTデータとMongoDBの強力なデータ保存機能をつなぐためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントを活用することで、EMQXからMongoDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとMongoDB間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/mongdb_bridge_architecture.png" alt="MongoDBブリッジアーキテクチャ" style="zoom:67%;" />

MongoDBへのMQTTデータ取り込みの流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：接続された車両、IIoTシステム、エネルギー管理プラットフォームなどのIoTデバイスは、MQTTプロトコルを通じてEMQXに正常に接続し、特定のトピックにMQTTメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前定義された条件に基づき、MongoDBにルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、ペイロードのコンテキスト付加などの変換が適用されます。
3. **MongoDBへのデータ取り込み**：ルールエンジンがMongoDB保存対象のメッセージを特定すると、メッセージをMongoDBに転送するアクションがトリガーされます。処理済みデータはMongoDBのコレクションにシームレスに書き込まれます。
4. **データの保存と活用**：データがMongoDBに保存されることで、企業はそのクエリ機能を活用し、様々なユースケースに対応できます。例えば、接続車両の分野では、保存されたデータを使って車両の健康状態を把握したり、リアルタイムの指標に基づいたルート最適化や資産追跡を行えます。IIoT環境では、機械の状態監視、メンテナンス予測、生産スケジュールの最適化に活用されます。

この統合システムを利用することで、電力・エネルギー分野の企業はグリッドの健康状態を継続的に監視し、需要予測や潜在的な障害の早期検知が可能になります。リアルタイムおよび履歴データから得られる価値は、運用効率の向上だけでなく、コスト削減や顧客体験の強化にもつながります。

## 特長とメリット

MongoDBとのデータ統合は、効果的なデータ処理と保存を実現するために以下の特長とメリットを提供します：

- **IoTデータ管理の効率化**

  IoTデータの取り込み、保存、処理、分析を一元的に行え、複雑な統合や面倒なデータ移行を排除します。データのサイロ化を防ぎ、IoTデータの統合ビューを実現します。
  
- **リアルタイムデータ処理**

  EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからMongoDBへの効率的かつ信頼性の高いデータ伝送を保証します。即時の洞察やアクションが必要なユースケースに理想的です。

- **柔軟なMongoDB接続オプション**

  単一のMongoDBインスタンスでも、レプリカセットの堅牢性を活用する場合でも、両方の構成にネイティブ対応し、インフラ要件に応じた柔軟な接続が可能です。

- **高性能かつスケーラブル**

  EMQXの分散アーキテクチャとMongoDBのカラム型ストレージフォーマットにより、データ量の増加に伴うスムーズなスケーラビリティを実現。大規模データセットでも一貫したパフォーマンスと応答性を維持します。IoT展開の拡大に合わせてデータ保存能力も容易に拡張可能です。

- **柔軟なデータ変換**

  EMQXの強力なSQLベースのルールエンジンにより、MongoDBに保存する前にデータを前処理できます。フィルタリング、ルーティング、集約、エンリッチメントなど多様な変換機構をサポートし、ニーズに合わせたデータ整形が可能です。

- **NoSQLの利点**

  MongoDBのスキーマレスアーキテクチャにより、多様なMQTTメッセージ構造を厳格なスキーマなしで容易に保存でき、IoTデータの動的な性質に対応します。

- **信頼性の高いデータ保存**

  EMQXルールエンジンがメッセージを処理・ルーティングした後、MongoDBに保存され、プラットフォームの実績ある信頼性によりデータの整合性と継続的な可用性が保証されます。

- **運用指標と高度な分析**

  総メッセージ数、送信トラフィック率などの指標を取得可能です。これらの指標とMongoDBの強力なクエリ機能を組み合わせて、データフローの監視、分析、最適化ができ、予測分析や異常検知などに活用できます。

- **最新MongoDBバージョン対応**

  最新のMongoDBバージョンに対応しており、ユーザーはデータベースプラットフォームの最新機能、最適化、セキュリティアップデートを享受できます。

- **コスト効率**

  EMQXとMongoDBは共にオープンソースソリューションであり、プロプライエタリ製品に比べてコスト効率が高いです。これにより、IoTプロジェクトの総所有コスト削減と投資収益率の向上が期待できます。

このMongoDBデータ統合は、デバイスが生成する膨大なデータを単に保存するだけでなく、将来のクエリや分析に備えて活用可能な形で保持し、セットアップの容易さと運用の優秀性により、IoTシステムの効率性と信頼性を大幅に向上させます。

## はじめる前に

このセクションでは、EMQXダッシュボードでMongoDBデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXのデータ統合[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- [MongoDB](https://www.mongodb.com/)に関する知識

### MongoDBサーバーのセットアップ

以下のコマンドを使用して、Docker経由でMongoDBをインストールし、コンテナを起動し、ユーザーを作成できます。

```bash
# MongoDBのDockerイメージを起動し、パスワードをpublicに設定
docker run -d --name mongodb -p 27017:27017 mongo

# コンテナにアクセス
docker exec -it mongodb bash

# コンテナ内でMongoDBサーバーを起動（4.x系では`mongo`を使用）
mongosh

# ユーザー作成
use admin
db.createUser({ user: "admin", pwd: "public", roles: [ { role: "root", db: "admin" } ] })
```

### データベースの作成

以下のコマンドでMongoDB内にデータベースとコレクションを作成できます。

```bash
# データベースemqx_dataを作成
use emqx_data

# コレクションemqx_messagesを作成
db.createCollection('emqx_messages')
```

## コネクターの作成

このセクションでは、MongoDB SinkをMongoDBサーバーに接続するコネクターの作成方法を説明します。

以下の手順は、EMQXとMongoDBを同一マシン上で実行していることを前提としています。MongoDBが別環境にある場合は、設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**MongoDB**を選択し、**Next**をクリックします。
4. コネクターの名前を入力します。名前は英数字の組み合わせで、例：`my_mongodb`。
5. MongoDBサーバーの接続情報を設定します。必須項目（*印）を入力してください。

   - **MongoDB Mode**：実際のMongoDBの展開形態に応じて接続タイプを選択します。この例では`single`を選択可能です。
     - `single`：単一のMongoDBインスタンス
     - `rs`：レプリカセット、同一データセットを維持する複数の`mongod`プロセスのグループ
     - `sharded`：MongoDBのシャーディングクラスター
   - **Server Host**：`127.0.0.1:27017`またはMongoDBサーバーがリモートの場合は実際のURLを入力
   - **Database Name**：`emqx_data`を入力
   - **Write Mode**：デフォルトの`unsafe`のまま
   - **Username**：`admin`を入力
   - **Password**：`public`を入力
   - **Auth Source**：ユーザー認証に使用するデータベース名を入力
   - **Use Legacy Protocol**：MongoDBのレガシープロトコルを使用するかどうかを設定（MongoDB 3.6で新しいワイヤープロトコルが導入され、レガシープロトコルは後方互換のために残されています）。`true`、`false`、`auto`のいずれかで、`auto`（デフォルト）ではMongoDBのバージョンに応じて自動判別します。
   - **Srv Record**：デフォルトで無効。有効にすると、DNSのSRVレコードを使ってMongoDBホストを自動検出し、レプリカセットやシャーディングクラスターへの接続が容易になります。
   - 暗号化接続を確立したい場合は、**Enable TLS**のトグルスイッチをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md/#tls-for-external-resource-access)を参照してください。
6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
7. **詳細設定（任意）**：詳細は[高度な設定](#advanced-configurations)を参照してください。
8. **Create**をクリックする前に、**Test Connectivity**を押してコネクターがMongoDBサーバーに接続できるかテストできます。
9. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップで**Back to Connector List**または**Create Rule**を選択可能です。ルールとSinkを作成してMongoDBへのデータ転送を指定するには[ルールとMongoDB Sinkの作成](#create-a-rule-with-mongodb-sink)を参照してください。

## MongoDB Sinkを使ったルールの作成

このセクションでは、ダッシュボードでルールを作成し、ソースMQTTトピック`t/#`からのメッセージを処理し、設定済みのSinkを介してMongoDBに保存する方法を説明します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**にルールを設定します。トピック`t/#`のMQTTメッセージをMongoDBに保存したい場合、以下のSQL文を使用できます。

   注：独自のSQL文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`句に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   例えば、`timestamp`を日付型として、`payload`をJSON文字列として保存するには以下のSQL文を使えます：

   ```sql
   SELECT
     *,
     mongo_date(timestamp) as timestamp,
     json_encode(payload) as payload
   FROM
     "t/#"
   ```

   注：初心者の方は**SQL Examples**をクリックし、**Enable Test**でSQLルールを学習・テストできます。

4. + **Add Action**ボタンをクリックし、ルールにトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをMongoDBに送信します。

5. **Type of Action**ドロップダウンリストから`MongoDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択可能ですが、この例では新規Sinkを作成します。

6. Sinkの名前を入力します。名前は英数字の組み合わせにしてください。

7. **Connector**ドロップダウンから`my_mongodb`を選択します。新規コネクターを作成する場合は隣のボタンをクリックします。設定パラメータの詳細は[コネクターの作成](#create-a-connector)を参照してください。

8. **Collection**欄にデータを保存するコレクション名を入力します。`${var_name}`のプレースホルダーを使った動的設定も可能です。この例では`emqx_messages`を入力します。

9. **Payload template**を設定し、`clientid`、`topic`、`qos`、`timestamp`、`payload`をMongoDBに保存します。このテンプレートはMongoDBのinsertコマンドで実行され、サンプルコードは以下の通りです：

   ```json
   {
     "clientid": "${clientid}",
     "topic": "${topic}",
     "qos": ${qos},
     "timestamp": ${timestamp},
     "payload": ${payload}
   }
   ```

   ペイロードテンプレート設定時の注意点：

   - すべての`key`はダブルクォーテーション`"`で囲む必要があります。
   - 値のデータ型の自動判別はサポートされていません：
     - 文字列は`"`で囲む必要があります。囲まないとエラーになります。
     - 数値は囲まないでください。囲むと文字列として認識されます。
     - timestamp、日付、時間型は特別な処理をしないと数値または文字列として認識されます。日付や時間として保存するには、ルールSQLの`mongo_date`関数を使ってフィールドを処理してください。詳細は[時間・日付関数](./rule-sql-builtin-functions.md#mongodb時間関数)を参照してください。（注：`mongo_`で始まる関数の戻り値はMongoDB Action専用で、他のActionでは使用できません。）
   - 値がJSONオブジェクトの場合はネスト可能です：
     - テンプレート内で値を`"`で囲んでネストすることはできません。実行エラーになります。
     - オブジェクトは自身の構造に従ってネスト保存されます。
   - オブジェクトをJSON文字列として保存したい場合は、ルールSQLの`json_encode`関数で変換し、テンプレート内の対応する値は`"`で囲まないでください。

10. 詳細設定（任意）：詳細は[高度な設定](#advanced-configurations)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**を押してSinkがMongoDBサーバーに接続できるかテストできます。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックし、ルールを生成します。

これでルールが正常に作成され、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいMongoDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されMongoDBに送信・保存されている様子が確認できます。

## ルールのテスト

ルールとSinkが期待通りに動作するかテストするために、[MQTTX](https://mqttx.app/)を使ってクライアントをシミュレートし、EMQXにMQTTメッセージをパブリッシュできます。

1. MQTTXでトピック`t/1`にメッセージを送信します：

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello MongoDB" }'
   ```

2. Sinkの稼働状況を確認し、**Matched**が1件、**Sent Successfully**が1件増えていることを確認します。

3. メッセージがコレクション`emqx_messages`に書き込まれているか確認します：

   ```
   > db.emqx_messages.find().pretty()
   {
       "_id" : ObjectId("63db7059df489d01ed000009"),
       "clientid" : "emqx_c",
       "payload" : {
         "msg" : "hello MongoDB"
       },
       "qos" : 0,
       "timestamp" : NumberLong("1675325529070"),
       "topic" : "t/1"
   }
   ```

   ルール設定で2番目のSQL文を使用した場合は、以下のように返されます：

   ```
   > db.emqx_messages.find().pretty()
   {
       "_id" : ObjectId("63db7535df489d01ed000013"),
       "clientid" : "emqx_c",
       "payload" : "{ \"msg\": \"hello MongoDB\" }",
       "qos" : 0,
       "timestamp" : ISODate("2023-02-02T08:33:36.715Z"),
       "topic" : "t/1"
   }
   ```

## 高度な設定

このセクションでは、EMQX MongoDBコネクターおよびSinkの高度な設定オプションを紹介します。コネクターやSinkの設定時に**Advanced Settings**を展開し、以下のパラメータをニーズに合わせて調整できます。

| **項目**                     | **説明**                                                       | **推奨値**    |
| ---------------------------- | -------------------------------------------------------------- | ------------ |
| **Connect Timeout**          | MongoDBへの接続確立を試みる際にEMQXが待機する最大時間。タイムアウトまでの時間。 | 30秒         |
| **Socket Timeout**           | MongoDBとのソケット接続でデータ送受信を試みる際の最大待機時間。タイムアウトまでの時間。 | 30秒         |
| **Max Overflow Workers**     | 既存のワーカーがすべて占有されている場合に追加で生成可能なワーカー数。負荷急増時の同時接続数増加に重要。 | 0            |
| **Wait Queue Timeout**       | MongoDB接続が利用可能になるまでワーカーがアイドル状態で待機できる最大時間。 | 10秒         |
| **Heartbeat Period**         | ドライバーがMongoDBの状態をチェックする間隔。連続したチェック間の時間を指定し、MongoDBの稼働状況を監視。 | 200秒        |
| **Minimum Heartbeat Period** | ハートビート間の最短間隔を設定し、MongoDB状態チェックの過剰な頻度を防止。EMQXとMongoDB間の効率的な通信を確保。 | 200秒        |

## 参考情報

以下のリンクからさらに詳しく学べます：

**ブログ**：

[MQTTとMongoDB：IoTデータ管理のシームレスな連携を実現](https://www.emqx.com/en/blog/mqtt-and-mongodb-crafting-seamless-synergy-for-iot-data-mangement)

**レポート**：

[MQTTパフォーマンスベンチマークテスト：EMQX-MongoDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-mongodb-integration)

**動画**：

https://www.youtube.com/watch?v=c2M-rlkkT5o
