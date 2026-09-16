# DynamoDBへのMQTTデータ取り込み

[DynamoDB](https://www.amazonaws.cn/en/dynamodb/)は、AWS上で提供されるフルマネージドの高性能サーバーレスなキー・バリュー型ストアデータベースサービスです。高速でスケーラブルかつ信頼性の高いデータストレージを必要とするアプリケーションに適しています。EMQXはDynamoDBとの連携をサポートしており、MQTTメッセージやクライアントイベントをDynamoDBに保存することで、IoTデバイスの登録・管理やデバイスデータの長期保存およびリアルタイム分析を実現します。DynamoDBのデータ統合を通じて、MQTTメッセージやクライアントイベントをDynamoDBに格納できるほか、イベントに応じてDynamoDB内のデータの更新や削除をトリガーし、デバイスのオンライン状態や接続履歴などの情報を記録できます。

本ページでは、EMQXとDynamoDB間のデータ統合について包括的に解説し、データ統合の作成および検証方法を実践的に説明します。

## 動作概要

DynamoDBデータ統合は、EMQXに標準搭載された機能であり、EMQXのデバイス接続およびメッセージ送受信機能とDynamoDBの強力なデータストレージ機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからDynamoDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとDynamoDB間のデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration DynamoDB](./assets/emqx-integration-dynamodb.png)

MQTTデータのDynamoDBへの取り込みは、以下のように動作します。

1. **メッセージのパブリッシュと受信**：接続された車両、IIoTシステム、エネルギー管理プラットフォームなどのIoTデバイスは、MQTTプロトコルを介してEMQXに正常に接続し、特定のトピックにMQTTメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージが到着すると、ルールエンジンを通過し、EMQXで定義されたルールに従って処理されます。ルールは事前定義された条件に基づき、DynamoDBにルーティングすべきメッセージを判定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの拡充などが適用されます。
3. **DynamoDBへのデータ取り込み**：ルールエンジンがDynamoDBへの保存対象メッセージを特定すると、DynamoDBへの転送アクションをトリガーします。処理済みデータはDynamoDBのテーブルにシームレスに書き込まれます。
4. **データの保存と活用**：データがDynamoDBに保存された後、企業はそのクエリ機能を活用して様々なユースケースに利用できます。例えば、接続車両分野では、車両の状態管理、リアルタイム指標に基づくルート最適化、資産追跡などに活用可能です。IIoT環境では、機械の状態監視、メンテナンス予測、生産スケジュールの最適化などに利用されます。

## 特長とメリット

DynamoDBとのデータ統合は、効率的なデータ送信、保存、活用を実現するための多彩な特長とメリットを提供します。

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからDynamoDBへの効率的かつ信頼性の高いデータ送信を保証します。即時のインサイトやアクションが必要なユースケースに適しています。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを提供し、DynamoDBに保存する前にデータを前処理できます。フィルタリング、ルーティング、集約、拡充など多様な変換機構をサポートし、ニーズに応じたデータ整形が可能です。
- **柔軟なデータモデル**：DynamoDBはキー・バリュー型およびドキュメント型データモデルを採用しており、構造化されたデバイスイベントやメッセージデータの保存・管理に適しています。異なるMQTTメッセージ構造も容易に格納できます。
- **強力なスケーラビリティ**：EMQXはクラスターのスケーラビリティを提供し、デバイス接続数やメッセージ量に応じてシームレスな水平スケールが可能です。DynamoDBはサーバーやインフラ管理を不要とし、リソース管理とスケーリングを自動で行います。両者の組み合わせにより、高性能かつ高信頼性のデータ保存とスケーラビリティを実現します。

## はじめる前に

このセクションでは、DynamoDBデータ統合を作成する前に必要な準備について説明します。認証方法の選択、DynamoDBサーバーのインストール、データテーブルの作成が含まれます。

### 前提条件

- EMQXのデータ統合に関する[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識

### 認証方法の選択

EMQX 6.0.4以降、DynamoDBコネクターは以下の認証方法をサポートしています。EMQXのデプロイ環境に応じて適切な方法を選択してください。

- **アクセスキーの手動設定**：対象のDynamoDBリソースにアクセス権限を持つAWS Access Key IDとAWS Secret Access Keyを指定します。この方法はローカルデプロイ、AWS外環境、ECSタスクロールやEC2インスタンスロールを使用しないデプロイに適しています。
- **一時認証情報の自動取得**：EMQXがAmazon ECSタスクまたはAmazon EC2インスタンスとして実行されている場合、対象のDynamoDBリソースにアクセス権限を持つECSタスクロールまたはEC2インスタンスロールを設定します。コネクターの**AWS Access Key ID**と**AWS Secret Access Key**は空欄にします。EMQXはECSタスクロールまたはEC2インスタンスメタデータから一時認証情報を取得し、有効期限前に更新します。

::: warning 重要なお知らせ

**AWS Access Key ID**と**AWS Secret Access Key**は両方とも指定するか、両方とも空欄にする必要があります。一方のみを指定すると、コネクター設定が無効になります。

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

   - `device_id`をハッシュキー（パーティションキー）として定義。
   - `timestamp`をレンジキー（ソートキー）として定義。
   - `device_id`属性を文字列型（S）として定義。
   - `timestamp`属性を数値型（N）として定義。

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

   テーブル作成が成功していれば、以下のJSONが表示されます。

   ```json
   {
       "TableNames": [
           "mqtt_msg"
       ]
   }
   ```

## コネクターの作成

このセクションでは、SinkをDynamoDBサーバーに接続するコネクターの作成方法を説明します。

以下の手順は、EMQXとDynamoDBをローカルマシン上で実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**DynamoDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します。
   - **Connector name**：コネクター名を入力します。大文字・小文字の英数字の組み合わせが推奨されます。例：`my_dynamodb`
   - **DynamoDB Region**：`us-west-2`を入力します。
   - **DynamoDB Endpoint**：`http://127.0.0.1:8000`（ローカルの場合）またはDynamoDBサーバーの実際のURLを入力します。
   - **AWS Access Key ID**および**AWS Secret Access Key**：ローカルDynamoDBの例ではそれぞれ`root`と`public`を入力します。ECSタスクロールやEC2インスタンスロールを使用する場合は両方空欄にします。詳細は[認証方法の選択](#認証方法の選択)を参照してください。
5. 詳細設定（任意）：詳細は[Sinkの特長](./data-bridges.md#features-of-sink)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがDynamoDBサーバーに接続可能かテストできます。
7. ページ下部の**Create**ボタンをクリックしてコネクターの作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを使ったルール作成を続行できます。詳細は[メッセージ保存用DynamoDB Sinkルールの作成](#create-a-rule-with-dynamodb-sink-for-message-storage)および[イベント記録用DynamoDB Sinkルールの作成](#create-a-rule-with-dynamodb-sink-for-events-recording)を参照してください。

## メッセージ保存用DynamoDB Sinkルールの作成

このセクションでは、ソースMQTTトピック`t/#`からのメッセージを処理し、処理済みデータを設定済みSink経由でDynamoDBテーブル`mqtt_msg`に書き込むルールの作成方法をダッシュボード上で説明します。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`を入力します。メッセージ保存用ルールを作成するため、**SQL Editor**に以下のステートメントを入力します。これはトピック`t/#`配下のMQTTメッセージをDynamoDBに保存することを意味します。

   注意：独自のSQL構文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`部に含めていることを確認してください。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は、**SQL Examples**をクリックし、**Enable Test**を有効にしてSQLルールを学習・テストできます。

   :::

4. + **Add Action**ボタンをクリックして、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをDynamoDBに送信します。

5. **Type of Action**のドロップダウンリストから`DynamoDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkを選択することもできますが、ここでは新規Sinkを作成します。

6. Sinkの名前を入力します。名前は大文字・小文字の英数字の組み合わせにしてください。

7. **Connector**のドロップダウンから先ほど作成した`my_dynamodb`を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメータの詳細は[コネクターの作成](#コネクターの作成)を参照してください。

8. 以下の設定を行います。

   - **Table**：先に作成したテーブル名`mqtt_msg`を入力します。

   - **Hash Key**：`${clientid}`を入力し、クライアントIDをハッシュキーとして使用します。

   - **Range Key**（任意）：`${timestamp}`を入力し、メッセージのタイムスタンプをレンジキーとして使用します。

   - **Message Template**：デフォルトでは空欄のままにします。

     ::: tip

     この値が空欄の場合、メッセージ全体がデータベースに保存されます。実際の値はJSONテンプレートデータです。

     :::

     SQLテンプレート内でプレースホルダー変数が未定義の場合、**Message template**上部の**Undefined Vars as Null**スイッチでルールエンジンの動作を切り替えられます。

     - **Disabled**（デフォルト）：ルールエンジンは文字列`undefined`をデータベースに挿入します。

     - **Enabled**：未定義変数の場合、ルールエンジンは`NULL`をデータベースに挿入します。

       ::: tip

       可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

       :::

9. **フォールバックアクション（任意）**：メッセージ配送失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの特長](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがサーバーに接続可能かテストできます。

12. **Create**ボタンをクリックしてSink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認してから**Create**ボタンをクリックし、ルールを生成します。

これでDynamoDB Sinkを通じてデータを転送するルールの作成が完了しました。**Integration** -> **Rules**ページで新規作成したルールを確認できます。**Actions(Sink)**タブをクリックすると、新しいDynamoDB Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認でき、トピック`t/#`配下のメッセージがルール`my_rule`で解析されDynamoDBに送信・保存されている様子が確認できます。

## イベント記録用DynamoDB Sinkルールの作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みSink経由でDynamoDBテーブル`mqtt_msg`に書き込むルールの作成方法を説明します。

::: tip

便宜上、オンライン／オフラインイベントの受信には`mqtt_msg`トピックを再利用します。

:::

ルールおよびアクションの作成手順は、[メッセージ保存用DynamoDB Sinkルールの作成](#メッセージ保存用dynamodb-sinkルールの作成)とほぼ同様ですが、SQLルール構文が異なります。

オンライン／オフライン状態記録用のSQLルール構文は以下の通りです。

```sql
SELECT
  str(event) + timestamp as id, *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

### ルールのテスト

MQTT Xを使ってトピック`t/1`にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello DynamoDB" }'
```

Sinkの稼働状況を確認すると、1件の新規受信メッセージと1件の新規送信メッセージ、合計2件のイベントレコードがあるはずです。

`mqtt_msg`データテーブルにデータが書き込まれているか確認します。

```bash
docker run --rm -e AWS_ACCESS_KEY_ID=root -e AWS_SECRET_ACCESS_KEY=public -e AWS_DEFAULT_REGION=us-west-2 amazon/aws-cli dynamodb scan --table-name=mqtt_msg --endpoint-url http://host.docker.internal:8000
```

出力例：

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
