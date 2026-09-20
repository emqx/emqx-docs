# MongoDBへのMQTTデータ取り込み

[MongoDB](https://www.mongodb.com/)は、スキーマ設計の柔軟性、スケーラビリティ、大量の構造化および半構造化データの保存能力で知られる主要なNoSQLデータベースです。EMQXとMongoDBを統合することで、ユーザーはMQTTメッセージやクライアントイベントを直接MongoDBに効率的に取り込むことができます。これにより、MongoDB内での長期的な時系列データの保存や高度なクエリ機能が可能になります。この統合は一方向のデータフローを保証し、EMQXからのMQTTメッセージがMongoDBデータベースに書き込まれます。この強力な組み合わせは、IoTデータを効果的に管理したい企業にとって堅実な基盤となります。

本ページでは、EMQXとMongoDB間のデータ統合について包括的に紹介し、データ統合の作成と検証に関する実践的な手順を提供します。

## 動作概要

MongoDBデータ統合は、MQTTベースのIoTデータとMongoDBの強力なデータ保存機能をつなぐためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからMongoDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとMongoDB間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/mongdb_bridge_architecture.png" alt="mongdb_bridge_architecture" style="zoom:67%;" />

MongoDBへのMQTTデータ取り込みの流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：接続された車両、IIoTシステム、エネルギー管理プラットフォームなどのIoTデバイスは、MQTTプロトコルを通じてEMQXに正常に接続し、特定のトピックにMQTTメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前定義された条件に基づき、MongoDBにルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、ペイロードへの追加コンテキストの付加などの変換が適用されます。
3. **MongoDBへのデータ取り込み**：ルールエンジンがMongoDBへの保存対象メッセージを特定すると、MongoDBへの転送アクションをトリガーします。処理済みデータはMongoDBデータベースのコレクションにシームレスに書き込まれます。
4. **データの保存と活用**：データがMongoDBに保存されることで、企業はそのクエリ機能を活用して様々なユースケースに対応できます。例えば、接続車両分野では、車両の状態管理、リアルタイム指標に基づくルート最適化、資産追跡などに利用可能です。IIoT環境では、機械の状態監視、メンテナンス予測、生産スケジュールの最適化に活用されます。

この統合システムを利用することで、電力・エネルギー分野の企業はグリッドの状態を継続的に監視し、需要予測や障害発生前の検知が可能になります。リアルタイムおよび履歴データから得られる価値は、運用効率の向上だけでなく、コスト削減や顧客体験の向上にもつながります。

## 特長とメリット

MongoDBとのデータ統合は、効果的なデータ処理と保存を実現するために以下の特長とメリットを提供します。

- **統合されたIoTデータ管理**

  IoTデータの取り込み、保存、処理、分析を一元的に行え、複雑な統合や面倒なデータ移行を不要にします。データサイロを解消し、IoTデータの統合ビューを実現します。

- **リアルタイムデータ処理**

  EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからMongoDBへの効率的かつ信頼性の高いデータ伝送を保証します。即時の洞察やアクションが必要なユースケースに理想的です。

- **柔軟なMongoDB接続オプション**

  単一のMongoDBインスタンスでもレプリカセットの堅牢性を活用する場合でも、両方の構成にネイティブ対応し、インフラ要件に応じて柔軟に適応可能です。

- **高性能かつスケーラブル**

  EMQXの分散アーキテクチャとMongoDBのカラムナストレージ形式により、データ量の増加に伴うスケーラビリティをシームレスに実現します。大規模データセットでも一貫したパフォーマンスと応答性を維持します。

- **柔軟なデータ変換**

  EMQXの強力なSQLベースのルールエンジンにより、MongoDBへの保存前にデータの前処理が可能です。フィルタリング、ルーティング、集約、エンリッチメントなど多様な変換機能をサポートし、ニーズに応じたデータ整形を実現します。

- **NoSQLの柔軟性**

  MongoDBのスキーマレスアーキテクチャにより、多様なMQTTメッセージ構造を厳格なスキーマなしで容易に保存でき、IoTデータの動的な性質に対応します。

- **信頼性の高いデータ保存**

  EMQXルールエンジンで処理・ルーティングされたメッセージは、MongoDBの実績ある信頼性によりデータ整合性と継続的な可用性が保証されます。

- **運用メトリクスと高度な分析**

  総メッセージ数、送信トラフィック率などのメトリクスから洞察を得られます。これらのメトリクスとMongoDBの強力なクエリ機能を組み合わせて、データフローの監視、分析、最適化が可能です。予測分析や異常検知などにも活用できます。

- **最新のMongoDBバージョン対応**

  最新のMongoDBバージョンに対応しており、最新機能、最適化、セキュリティアップデートの恩恵を受けられます。

- **コスト効率**

  EMQXとMongoDBは共にオープンソースソリューションであり、プロプライエタリ製品と比べてコスト効率に優れています。これにより、IoTプロジェクトの総所有コスト削減と投資収益率向上に寄与します。

このMongoDBデータ統合は、IoTインフラを強化し、デバイスから生成される膨大なデータを単に保存するだけでなく、将来のクエリや分析に備えて準備します。セットアップの容易さと運用の優秀性により、IoTシステムの効率性と信頼性を大幅に向上させます。

## はじめる前に

このセクションでは、EMQXダッシュボードでMongoDBデータ統合を作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
- [MongoDB](https://www.mongodb.com/)に関する知識

### MongoDBサーバーのセットアップ

以下のコマンドを使用してDocker経由でMongoDBをインストールし、コンテナを起動し、ユーザーを作成できます。

```bash
# MongoDBのDockerイメージを起動し、パスワードをpublicに設定
docker run -d --name mongodb -p 27017:27017 mongo

# コンテナにアクセス
docker exec -it mongodb bash

# コンテナ内でMongoDBサーバーを起動（4.xバージョンでは`mongo`を使用）
mongosh

# ユーザー作成
use admin
db.createUser({ user: "admin", pwd: "public", roles: [ { role: "root", db: "admin" } ] })
```

### データベースの作成

以下のコマンドでMongoDBにデータベースとコレクションを作成できます。

```bash
# データベースemqx_dataを作成
use emqx_data

# コレクションemqx_messagesを作成
db.createCollection('emqx_messages')
```

## コネクターの作成

このセクションでは、MongoDB SinkをMongoDBサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとMongoDBの両方をローカルマシンで実行していることを前提としています。MongoDBが別の環境にある場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**MongoDB**を選択し、**Next**をクリックします。
4. コネクター名を入力します。名前は大文字・小文字の英数字の組み合わせにしてください（例：`my_mongodb`）。
5. MongoDBサーバーの接続情報を設定します。必須項目（*印）を入力してください。

   - **MongoDB Mode**：実際のMongoDBのデプロイモードに応じて接続タイプを選択します。ここでは例として`single`を選択します。
     - `single`：単一のスタンドアロンMongoDBインスタンス
     - `rs`：同じデータセットを保持する`mongod`プロセスのグループ（レプリカセット）
     - `sharded`：MongoDBのシャーディングクラスター
   - **Server Host**：`127.0.0.1:27017`またはMongoDBサーバーがリモートの場合は実際のURLを入力
   - **Database Name**：`emqx_data`を入力
   - **Write Mode**：デフォルトの`unsafe`のまま
   - **Username**：`admin`を入力
   - **Password**：`public`を入力
   - **Auth Source**：ユーザー認証に使用するデータベース名を入力
   - **Use Legacy Protocol**：MongoDBのレガシー通信プロトコルを使用するかどうかを設定（MongoDB 3.6で新しいワイヤープロトコルが導入され、レガシープロトコルは後方互換のため残されています）。`true`、`false`、`auto`から選択可能で、`auto`（デフォルト）ではMongoDBのバージョンに応じて自動判別されます。
   - **Srv Record**：デフォルトで無効。これを有効にすると、EMQXがDNS SRVレコードを使って接続すべきMongoDBホストを検出でき、レプリカセットやシャーディングクラスターへの接続が容易になります。
   - 暗号化接続を確立したい場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。
6. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。
7. **詳細設定（任意）**：詳細は[詳細設定](#advanced-configurations)を参照してください。
8. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがMongoDBサーバーに接続できるかテストできます。
9. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてルールとSinkの作成を続行できます。詳細な手順は[ルールとMongoDB Sinkの作成](#create-a-rule-and-mongodb-sink)を参照してください。

## MongoDB Sinkを使ったルールの作成

このセクションでは、ダッシュボードでMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みSinkを通じてMongoDBに保存するルールの作成方法を説明します。

1. EMQXダッシュボードで、**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力し、**SQL Editor**でルールを設定します。トピック`t/#`のMQTTメッセージをMongoDBに保存したい場合、以下のSQL構文を使用できます。

   注意：独自のSQL構文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`句に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   例として、`timestamp`を日時型として保存し、`payload`をJSON文字列として保存するSQL構文は以下の通りです。

   ```sql
   SELECT
     *,
     mongo_date(timestamp) as timestamp,
     json_encode(payload) as payload
   FROM
     "t/#"
   ```

   注意：初心者の方は**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習とテストが可能です。

4. + **Add Action**ボタンをクリックし、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをMongoDBに送信します。

5. **Type of Action**ドロップダウンから`MongoDB`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既に作成済みのSinkを選択することも可能です。本デモでは新規Sinkを作成します。

6. Sinkの名前を入力します。名前は大文字・小文字の英数字の組み合わせにしてください。

7. **Connector**ドロップダウンから`my_mongodb`を選択します。ドロップダウン横のボタンで新規コネクターを作成することも可能です。設定パラメータの詳細は[コネクターの作成](#create-a-connector)を参照してください。

8. **Collection**欄にデータを保存するコレクション名を入力します。`${var_name}`のプレースホルダーを使った動的設定もサポートしています。ここでは`emqx_messages`を入力します。

9. **Payload template**を設定し、`clientid`、`topic`、`qos`、`timestamp`、`payload`をMongoDBに保存します。このテンプレートはMongoDBのinsertコマンドで実行され、サンプルコードは以下の通りです。

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
   - 値のデータ型の自動判別はサポートされていません。
     - 文字列は`"`で囲む必要があります。囲まないとエラーになります。
     - 数値などは囲まないでください。囲むと文字列として認識されます。
     - timestamp、date、time型は特別な処理がない場合、数値または文字列として扱われます。日付や時刻として保存したい場合は、ルールSQLの[MongoDBタイム関数](./rule-sql-builtin-functions.md#mongodb-time-functions)を使用してください。（注：これらの関数の戻り値はMongoDBアクション専用で、他のアクションでは使用できません。）
   
   - 値がJSONオブジェクトの場合はネストされたオブジェクトが許容されます。
     - テンプレート内で値を`"`で囲んでネストすることはできません。実行エラーになります。
     - オブジェクトは自身の構造に従ってネストされ保存されます。

   - オブジェクトをJSON文字列として保存したい場合は、ルールSQLの`json_encode`関数で変換し、テンプレート内の対応する値は`"`で囲まないでください。

10. 詳細設定（任意）：詳細は[詳細設定](#advanced-configurations)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがMongoDBサーバーに接続できるかテストできます。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでルールが正常に作成され、**Rule**ページに新しいルールが表示されます。**Actions(Sink)**タブをクリックすると、新しいMongoDB Sinkが確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`のメッセージがルール`my_rule`で解析されMongoDBに送信・保存されていることが確認できます。

## ルールのテスト

ルールとSinkが期待通りに動作するかテストするために、[MQTTX](https://mqttx.app/)を使ってクライアントをシミュレートし、EMQXにMQTTメッセージをパブリッシュできます。

1. MQTTXでトピック`t/1`にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello MongoDB" }'
   ```

2. Sinkの稼働状況を確認すると、新たに1件の**Matched**と1件の**Sent Successfully**が表示されているはずです。

3. メッセージがコレクション`emqx_messages`に書き込まれているか確認します。

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

   ルール設定で2番目のSQL構文を使用した場合、返される情報は以下のようになります。

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

## 詳細設定

このセクションでは、EMQX MongoDBコネクターおよびSinkの詳細設定オプションを紹介します。コネクターやSinkの設定時に**Advanced Settings**を展開し、以下のパラメータをニーズに合わせて調整できます。

| **項目**                     | **説明**                                                     | **推奨値**             |
| ---------------------------- | ------------------------------------------------------------ | --------------------- |
| **Connect Timeout**          | EMQXがMongoDBへの接続確立を試みる際のタイムアウト時間。       | 30秒                  |
| **Socket Timeout**           | MongoDBとのソケット接続でデータ送受信を試みる際のタイムアウト時間。 | 30秒                  |
| **Max Overflow Workers**     | 既存のワーカーがすべて使用中の場合に追加で作成可能なワーカー数。負荷増大時にMongoDBへの同時接続数を増やすために重要。 | 0                     |
| **Wait Queue Timeout**       | ワーカーがMongoDB接続の利用可能待ちでアイドル状態でいられる最大時間。 | 10秒                  |
| **Heartbeat Period**         | ドライバーがMongoDBデプロイメントの状態をチェックする間隔。MongoDBの稼働状況を確認するハートビート信号の頻度を制御。 | 200秒                 |
| **Minimum Heartbeat Period** | ハートビート間の最短間隔。ドライバーがMongoDB状態を過度に頻繁にチェックしないようにするための設定。 | 200秒                 |

## さらに詳しく

以下のリンクから詳細情報をご覧いただけます。

**ブログ**：

[MQTTとMongoDB：IoTデータ管理のためのシームレスなシナジーの構築](https://www.emqx.com/en/blog/mqtt-and-mongodb-crafting-seamless-synergy-for-iot-data-mangement)

**レポート**：

[MQTTパフォーマンスベンチマークテスト：EMQX-MongoDB統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-mongodb-integration)

**動画**：

https://www.youtube.com/watch?v=c2M-rlkkT5o
