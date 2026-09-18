# DynamoDBへのMQTTデータ取り込み

[DynamoDB](https://www.amazonaws.cn/en/dynamodb/)は、AWS上のフルマネージドで高性能なサーバーレスのキー・バリューストア型データベースサービスです。高速でスケーラブルかつ信頼性の高いデータストレージを必要とするアプリケーション向けに設計されています。EMQXはDynamoDBとの連携をサポートしており、MQTTメッセージやクライアントイベントをDynamoDBに保存することで、IoTデバイスの登録・管理やデバイスデータの長期保存およびリアルタイム分析を可能にします。DynamoDBのデータ統合を通じて、MQTTメッセージやクライアントイベントをDynamoDBに格納できるほか、イベントに応じてDynamoDB内のデータの更新や削除をトリガーし、デバイスのオンライン状態や接続履歴などの情報を記録できます。

本ページでは、EMQXとDynamoDB間のデータ統合について包括的に解説し、データ統合の作成および検証手順を実践的に紹介します。

## 動作概要

DynamoDBデータ統合は、EMQXに標準搭載された機能であり、EMQXのデバイス接続およびメッセージ送信機能とDynamoDBの強力なデータストレージ機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからDynamoDBへのデータ取り込みと管理を簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとDynamoDB間のデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration DynamoDB](./assets/emqx-integration-dynamodb.png)

MQTTデータをDynamoDBに取り込む流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：接続された車両、IIoTシステム、エネルギー管理プラットフォームなどのIoTデバイスは、MQTTプロトコルを通じてEMQXに正常に接続し、特定のトピックにMQTTメッセージをパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQXで定義されたルールに従って処理されます。ルールは事前に定義された条件に基づき、DynamoDBにルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、ペイロードの付加的なコンテキスト付与などの変換が適用されます。
3. **DynamoDBへのデータ取り込み**：ルールエンジンがDynamoDBへの保存対象メッセージを特定すると、DynamoDBへの転送アクションをトリガーします。処理済みデータはシームレスにDynamoDBデータベースのコレクションへ書き込まれます。
4. **データの保存と活用**：データがDynamoDBに保存された後、企業はそのクエリ機能を活用して様々なユースケースに利用できます。例えば、コネクテッドビークルの分野では、保存されたデータを用いて車両の状態管理、リアルタイムメトリクスに基づくルート最適化、資産追跡などが可能です。同様にIIoT環境では、機械の状態監視、メンテナンス予測、生産スケジュールの最適化などに活用されます。

## 特長とメリット

DynamoDBとのデータ統合は、効率的なデータ送信、保存、活用を実現するために以下の特長とメリットを提供します。

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリーム処理に最適化されており、ソースシステムからDynamoDBへの効率的かつ信頼性の高いデータ送信を保証します。即時のインサイトとアクションを必要とするユースケースに理想的です。
- **柔軟なデータ変換**：EMQXの強力なSQLベースのルールエンジンにより、DynamoDBに保存する前にデータの前処理が可能です。フィルタリング、ルーティング、集約、エンリッチメントなど多様な変換機構をサポートし、ニーズに応じてデータを整形できます。
- **柔軟なデータモデル**：DynamoDBはキー・バリューおよびドキュメント型データモデルを採用しており、構造化されたデバイスイベントやメッセージデータの保存・管理に適しています。異なるMQTTメッセージ構造の保存も容易です。
- **強力なスケーラビリティ**：EMQXはクラスターのスケーラビリティを提供し、デバイス接続数やメッセージ量に応じてシームレスな水平スケールが可能です。DynamoDBはサーバーやインフラ管理を不要とし、基盤リソースの管理とスケーリングを自動で行います。両者の組み合わせにより、高性能かつ高信頼のデータストレージとスケーラビリティを実現します。

## はじめる前に

本節では、DynamoDBデータ統合を作成する前に必要な準備について説明します。認証方法の選択、DynamoDBサーバーのインストール、データテーブルの作成などが含まれます。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### 認証方法の選択

EMQX 5.10.5以降、DynamoDBコネクターは以下の認証方法をサポートしています。EMQXのデプロイ環境に応じて適切な方法を選択してください。

- **アクセスキーの手動設定**：対象DynamoDBリソースへのアクセス権限を持つAWS Access Key IDとAWS Secret Access Keyを指定します。この方法はローカルデプロイ、非AWS環境、ECSタスクロールやEC2インスタンスロールを使用しないデプロイに適しています。
- **一時的認証情報の自動取得**：EMQXがAmazon ECSタスクまたはAmazon EC2インスタンスとして実行されている場合、対象DynamoDBリソースへのアクセス権限を持つECSタスクロールまたはEC2インスタンスロールを設定します。コネクターの**AWS Access Key ID**および**AWS Secret Access Key**は空欄にします。EMQXはECSタスクロールまたはEC2インスタンスメタデータから一時的認証情報を取得し、有効期限前に自動更新します。

::: warning 重要なお知らせ

**AWS Access Key ID**と**AWS Secret Access Key**は両方とも指定するか、両方とも空欄にしてください。片方のみの指定は無効なコネクター設定となります。

:::

### DynamoDBローカルサーバーのインストールとテーブル作成

1. 以下のコマンドでDynamoDBサーバーをローカルで起動します。

   - Access Key ID: `root`
   - Secret Access Key: `public`
   - Region: `us-west-2`

   ```bash
   docker run -d -p 8000:8000 --name dynamodb-local \
     -e AWS_ACCESS_KEY_ID=root \
     -e AWS_SECRET_ACCESS_KEY=public \
     -e AWS_DEFAULT_REGION=us-west-2 \
     amazon/dynamodb-local:2.4.0
   ```

2. テーブル定義ファイルを作成し、カレントディレクトリに`mqtt_msg.json`という名前で保存します。テーブル定義は以下の通りです。

   - `device_id`をハッシュキー（パーティションキー）として定義
   - `timestamp`をレンジキー（ソートキー）として定義
   - `device_id`属性は文字列型（S）
   - `timestamp`属性は数値型（N）

   ```json
   {
       "TableName": "mqtt_msg",
       "AttributeDefinitions": [
           {
               "AttributeName": "device_id",
               "AttributeType": "S"
           },
           {
               "AttributeName": "timestamp",
               "AttributeType": "N"
           }
       ],
       "KeySchema": [
           {
               "AttributeName": "device_id",
               "KeyType": "HASH"
           },
           {
               "AttributeName": "timestamp",
               "KeyType": "RANGE"
           }
       ],
       "ProvisionedThroughput": {
           "ReadCapacityUnits": 5,
           "WriteCapacityUnits": 5
       }
   }
   ```

3. Dockerを使って`aws-cli`コマンドを実行し、ファイルを用いて新しいテーブルを作成します。

   ```bash
   docker run --rm -v $PWD:/dynamo_data \
       -e AWS_ACCESS_KEY_ID=root \
       -e AWS_SECRET_ACCESS_KEY=public \
       -e AWS_DEFAULT_REGION=us-west-2 \
       amazon/aws-cli:2.15.57 dynamodb create-table \
       --cli-input-json file:///dynamo_data/mqtt_msg.json \
       --endpoint-url http://host.docker.internal:8000
   ```

4. Dockerを使って`aws-cli`コマンドを実行し、テーブル作成が成功したか確認します。

   ```bash
   docker run --rm \
       -e AWS_ACCESS_KEY_ID=root \
       -e AWS_SECRET_ACCESS_KEY=public \
       -e AWS_DEFAULT_REGION=us-west-2 \
       amazon/aws-cli:2.15.57 dynamodb list-tables \
       --endpoint-url http://host.docker.internal:8000
   ```

   テーブル作成が成功していれば、以下のJSONが出力されます。

   ```json
   {
       "TableNames": [
           "mqtt_msg"
       ]
   }
   ```

## コネクターの作成

本節では、SinkをDynamoDBサーバーに接続するためのコネクター作成手順を示します。

以下の手順は、EMQXとDynamoDBをローカルマシン上で実行していることを前提としています。リモート環境でDynamoDBやEMQXを実行している場合は、設定を適宜調整してください。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**DynamoDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します。
   - **Connector name**：コネクター名を入力します。英数字の大文字・小文字の組み合わせで、例：`my_dynamodb`
   - **DynamoDB Region**：`us-west-2`を入力
   - **DynamoDB Endpoint**：`http://127.0.0.1:8000`（DynamoDBサーバーがリモートの場合は実際のURLを入力）
   - **AWS Access Key ID**および**AWS Secret Access Key**：ローカルDynamoDBの例ではそれぞれ`root`と`public`を入力。ECSタスクロールやEC2インスタンスロールを使用する場合は両方空欄にします。詳細は[認証方法の選択](#認証方法の選択)を参照してください。
5. 詳細設定（任意）：詳細は[Sinkの特長](./data-bridges.md#features-of-sink)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがDynamoDBサーバーに接続できるかテストできます。
7. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを利用したルール作成を続行できます。詳細な手順は[メッセージ保存用DynamoDB Sinkのルール作成](#create-a-rule-with-dynamodb-sink-for-message-storage)および[イベント記録用DynamoDB Sinkのルール作成](#create-a-rule-with-dynamodb-sink-for-events-recording)を参照してください。

## メッセージ保存用DynamoDB Sinkのルール作成

本節では、ソースMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みSink経由でDynamoDBテーブル`mqtt_msg`に書き込むルールをダッシュボードで作成する方法を示します。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力します。メッセージ保存用ルールを作成するため、**SQL Editor**に以下のステートメントを入力します。これはトピック`t/#`配下のMQTTメッセージをDynamoDBに保存することを意味します。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`句に含めていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールを学習・テストできます。

   :::

4. + **Add Action**ボタンをクリックして、ルールによりトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをDynamoDBに送信します。

5. **Type of Action**ドロップダウンリストから`DynamoDB`を選択します。**Action**ドロップダウンはデフォルトの`Create Action`のままにします。既存のSinkを選択することもできますが、本例では新規Sinkを作成します。

6. Sinkの名前を入力します。名前は英数字の大文字・小文字の組み合わせで指定してください。

7. **Connector**ドロップダウンから先ほど作成した`my_dynamodb`を選択します。隣のボタンをクリックして新規コネクターを作成することも可能です。設定パラメーターの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. 以下の設定を行います。

   - **Table**：先に作成したテーブル名`mqtt_msg`を入力

   - **Hash Key**：`${clientid}`を入力し、クライアントIDをハッシュキーとして使用

   - **Range Key**（任意）：`${timestamp}`を入力し、メッセージのタイムスタンプをレンジキーとして使用

   - **Message Template**：デフォルトで空欄のままにします

     ::: tip

     空欄の場合はメッセージ全体がデータベースに保存されます。実際の値はJSONテンプレートデータです。

     :::

     SQLテンプレート内でプレースホルダー変数が未定義の場合、**Message template**上部の**Undefined Vars as Null**スイッチを切り替えてルールエンジンの動作を設定できます。

     - **無効**（デフォルト）：ルールエンジンは未定義変数に`undefined`文字列を挿入可能

     - **有効**：未定義変数に対し`NULL`を挿入可能

       ::: tip

       可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

       :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの特長](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがサーバーに接続できるかテストできます。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**ボタンをクリックしルールを生成します。

これでDynamoDB Sink経由でデータを転送するルールが正常に作成されました。**Integration** -> **Rules**ページで新規作成ルールを確認できます。**Actions(Sink)**タブをクリックすると新しいDynamoDB Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`配下のメッセージがルール`my_rule`で解析されDynamoDBに送信・保存されている様子を確認できます。

## イベント記録用DynamoDB Sinkのルール作成

本節では、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みSink経由でDynamoDBテーブル`mqtt_msg`に書き込むルールの作成方法を示します。

::: tip

利便性のため、オンライン／オフラインイベントの受け取りに`mqtt_msg`トピックを再利用します。

:::

ルールおよびアクション作成手順は[メッセージ保存用DynamoDB Sinkのルール作成](#メッセージ保存用dynamodb-sinkのルール作成)とほぼ同様ですが、SQLルール構文が異なります。

オンライン／オフライン状態記録用のSQLルール構文は以下の通りです。

```sql
SELECT
  str(event) + timestamp as id, *
FROM
  "$events/client_connected", "$events/client_disconnected"
```

### ルールのテスト

MQTT Xを使い、トピック`t/1`にメッセージを送信してオンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello DynamoDB" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージ1件と送信メッセージ1件、イベントレコード2件があるはずです。

データが`mqtt_msg`テーブルに書き込まれているか確認します。

```bash
docker run --rm -e AWS_ACCESS_KEY_ID=root -e AWS_SECRET_ACCESS_KEY=public -e AWS_DEFAULT_REGION=us-west-2 amazon/aws-cli dynamodb scan --table-name=mqtt_msg --endpoint-url http://host.docker.internal:8000
```

出力例は以下の通りです。

```json
{
    "Items": [
        {
            "metadata": {
                "S": "{\"rule_id\":\"90d98f59\"}"
            },
            "peerhost": {
                "S": "127.0.0.1"
            },
            "clientid": {
                "S": "emqx_c"
            },
            "flags": {
                "S": "{\"retain\":false,\"dup\":false}"
            },
            "node": {
                "S": "emqx@127.0.0.1"
            },
            "qos": {
                "N": "0"
            },
            "payload": {
                "S": "{ \"msg\": \"hello DynamoDB\" }"
            },
            "pub_props": {
                "S": "{\"User-Property\":{}}"
            },
            "publish_received_at": {
                "N": "1678263363503"
            },
            "topic": {
                "S": "t/1"
            },
            "id": {
                "S": "0005F65F239F03FEF44300000BB40002"
            },
            "event": {
                "S": "message.publish"
            },
            "username": {
                "S": "undefined"
            },
            "timestamp": {
                "N": "1678263363503"
            }
        },
        {
            "conn_props": {
                "S": "{\"User-Property\":{},\"Request-Problem-Information\":1}"
            },
            "peername": {
                "S": "127.0.0.1:59582"
            },
            "metadata": {
                "S": "{\"rule_id\":\"703890a5\"}"
            },
            "clientid": {
                "S": "emqx_c"
            },
            "is_bridge": {
                "S": "false"
            },
            "keepalive": {
                "N": "30"
            },
            "proto_ver": {
                "N": "5"
            },
            "proto_name": {
                "S": "MQTT"
            },
            "connected_at": {
                "N": "1678263363499"
            },
            "receive_maximum": {
                "N": "32"
            },
            "sockname": {
                "S": "127.0.0.1:1883"
            },
            "mountpoint": {
                "S": "undefined"
            },
            "node": {
                "S": "emqx@127.0.0.1"
            },
            "id": {
                "S": "client.connected1678263363499"
            },
            "expiry_interval": {
                "N": "0"
            },
            "event": {
                "S": "client.connected"
            },
            "username": {
                "S": "undefined"
            },
            "timestamp": {
                "N": "1678263363499"
            },
            "clean_start": {
                "S": "true"
            }
        },
        {
            "reason": {
                "S": "normal"
            },
            "peername": {
                "S": "127.0.0.1:59582"
            },
            "metadata": {
                "S": "{\"rule_id\":\"703890a5\"}"
            },
            "clientid": {
                "S": "emqx_c"
            },
            "proto_ver": {
                "N": "5"
            },
            "proto_name": {
                "S": "MQTT"
            },
            "sockname": {
                "S": "127.0.0.1:1883"
            },
            "disconn_props": {
                "S": "{\"User-Property\":{}}"
            },
            "node": {
                "S": "emqx@127.0.0.1"
            },
            "id": {
                "S": "client.disconnected1678263363503"
            },
            "event": {
                "S": "client.disconnected"
            },
            "disconnected_at": {
                "N": "1678263363503"
            },
            "username": {
                "S": "undefined"
            },
            "timestamp": {
                "N": "1678263363503"
            }
        }
    ],
    "Count": 3,
    "ScannedCount": 3,
    "ConsumedCapacity": null
}
```
