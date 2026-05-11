# DynamoDBへのMQTTデータ取り込み

[DynamoDB](https://www.amazonaws.cn/en/dynamodb/)は、AWS上で提供されるフルマネージドの高性能サーバーレスなキー・バリューストア型データベースサービスです。高速でスケーラブルかつ信頼性の高いデータストレージを必要とするアプリケーション向けに設計されています。EMQXはDynamoDBとの連携をサポートしており、MQTTメッセージやクライアントイベントをDynamoDBに保存することで、IoTデバイスの登録・管理やデバイスデータの長期保存およびリアルタイム分析を実現します。DynamoDBデータ連携を通じて、MQTTメッセージやクライアントイベントをDynamoDBに格納できるほか、イベントに応じてDynamoDB内のデータ更新や削除をトリガーし、デバイスのオンライン状態や接続履歴などの情報を記録可能です。

本ページでは、EMQXとDynamoDB間のデータ連携について包括的に解説し、データ連携の作成および検証方法を実践的に説明します。

## 動作概要

DynamoDBデータ連携はEMQXに標準搭載された機能であり、EMQXのデバイス接続およびメッセージ伝送機能とDynamoDBの強力なデータストレージ機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからDynamoDBへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとDynamoDB間の典型的なデータ連携アーキテクチャを示しています。

![EMQX Integration DynamoDB](./assets/emqx-integration-dynamodb.png)

MQTTデータをDynamoDBに取り込む流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：接続車両、IIoTシステム、エネルギー管理プラットフォームなどのIoTデバイスがMQTTプロトコルでEMQXに正常に接続し、特定のトピックにMQTTメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：メッセージ到着後、ルールエンジンを通過し、EMQXで定義されたルールに基づいて処理されます。ルールは事前定義された条件により、DynamoDBへルーティングすべきメッセージを判別します。ペイロード変換が指定されている場合は、データ形式変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **DynamoDBへのデータ取り込み**：ルールエンジンがDynamoDB保存対象のメッセージを特定すると、DynamoDBへの転送アクションをトリガーします。処理済みデータはDynamoDBのテーブルにシームレスに書き込まれます。
4. **データの保存と活用**：DynamoDBに保存されたデータは、様々なユースケースでクエリ機能を活用できます。例えば、接続車両分野では車両の状態監視、リアルタイムメトリクスに基づくルート最適化、資産追跡に利用可能です。IIoT環境では機械の健康監視、メンテナンス予測、生産スケジュール最適化などに活用されます。

## 特長とメリット

DynamoDBとのデータ連携は、効率的なデータ伝送、保存、活用を実現する多彩な特長とメリットを提供します：

- **リアルタイムデータストリーミング**：EMQXはリアルタイムデータストリームの処理に最適化されており、ソースシステムからDynamoDBへの効率的かつ信頼性の高いデータ伝送を実現します。即時のインサイトやアクションが必要なユースケースに適しています。
- **柔軟なデータ変換**：EMQXは強力なSQLベースのルールエンジンを備え、DynamoDBに保存する前にデータの前処理が可能です。フィルタリング、ルーティング、集約、強化など多様なデータ変換機能をサポートし、ニーズに応じたデータ整形を実現します。
- **柔軟なデータモデル**：DynamoDBはキー・バリューおよびドキュメントデータモデルを採用しており、構造化されたデバイスイベントやメッセージデータの保存・管理に適しています。異なるMQTTメッセージ構造の格納も容易です。
- **強力なスケーラビリティ**：EMQXはクラスターのスケーラビリティを提供し、デバイス接続数やメッセージ量に応じてシームレスな水平スケールが可能です。DynamoDBはサーバーやインフラ管理不要で、基盤リソースの管理とスケーリングを自動で行います。両者の組み合わせにより、高性能かつ高信頼性のデータ保存とスケーラビリティを実現します。

## はじめる前に

このセクションでは、DynamoDBデータ連携の作成前に必要な準備として、DynamoDBサーバーのインストールおよびデータテーブルの作成方法を説明します。

### 前提条件

- EMQXデータ連携の[ルール](./rules.md)に関する知識
- [データ連携](./data-bridges.md)に関する知識

### DynamoDBローカルサーバーのインストールとテーブル作成

1. 以下のコマンドでDynamoDBサーバーをローカルで起動します：

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

2. テーブル定義ファイルを作成し、カレントディレクトリに`mqtt_msg.json`という名前で保存します。テーブル定義は以下の通りです：

   - `device_id`をハッシュキー（パーティションキー）として定義
   - `timestamp`をレンジキー（ソートキー）として定義
   - 属性`device_id`は文字列型（S）
   - 属性`timestamp`は数値型（N）

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

3. Dockerを使って`aws-cli`コマンドを実行し、上記ファイルを用いてテーブルを作成します：

   ```bash
   docker run --rm -v $PWD:/dynamo_data \
       -e AWS_ACCESS_KEY_ID=root \
       -e AWS_SECRET_ACCESS_KEY=public \
       -e AWS_DEFAULT_REGION=us-west-2 \
       amazon/aws-cli:2.15.57 dynamodb create-table \
       --cli-input-json file:///dynamo_data/mqtt_msg.json \
       --endpoint-url http://host.docker.internal:8000
   ```

4. Dockerで`aws-cli`コマンドを実行し、テーブル作成が成功したか確認します：

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

このセクションでは、SinkをDynamoDBサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとDynamoDBをローカルマシンで実行していることを前提としています。リモート環境で実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**DynamoDB**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下を設定します：
   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_dynamodb`
   - **DynamoDB Region**：`us-west-2`を入力
   - **DynamoDB Endpoint**：`http://127.0.0.1:8000`（DynamoDBサーバーがリモートの場合は実際のURLを入力）
   - **AWS Access Key ID**：`root`
   - **AWS Secret Access Key**：`public`
5. 詳細設定（任意）：詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがDynamoDBサーバーに接続できるかテスト可能です。
7. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを指定したルール作成に進めます。詳細は[メッセージ保存用DynamoDB Sinkのルール作成](#create-a-rule-with-dynamodb-sink-for-message-storage)および[イベント記録用DynamoDB Sinkのルール作成](#create-a-rule-with-dynamodb-sink-for-events-recording)を参照してください。

## メッセージ保存用DynamoDB Sinkのルール作成

このセクションでは、ダッシュボード上でMQTTのソーストピック`t/#`からのメッセージを処理し、処理済みデータを設定済みのSink経由でDynamoDBテーブル`mqtt_msg`に書き込むルール作成方法を説明します。

1. EMQXダッシュボードで**Integration** -> **Rules**を開きます。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`と入力し、メッセージ保存用ルールとして以下のSQL文を**SQL Editor**に入力します。これはトピック`t/#`配下のMQTTメッセージをDynamoDBに保存することを意味します。

   注意：独自のSQL文を指定する場合は、Sinkが必要とする全フィールドを`SELECT`に含めてください。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習やテストを行うことを推奨します。

   :::

4. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをDynamoDBに送信します。

5. **Type of Action**ドロップダウンから`DynamoDB`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択可能ですが、本デモでは新規Sinkを作成します。

6. Sink名を入力します。英数字の組み合わせで指定してください。

7. **Connector**ドロップダウンから先ほど作成した`my_dynamodb`を選択します。新規コネクターを作成する場合はドロップダウン横のボタンをクリックしてください。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

8. 以下の設定を行います：

   - **Table**：先に作成したテーブル名`mqtt_msg`を入力

   - **Hash Key**：`${clientid}`を入力し、クライアントIDをハッシュキーとして使用

   - **Range Key**（任意）：`${timestamp}`を入力し、メッセージのタイムスタンプをレンジキーとして使用

   - **Message Template**：デフォルトで空欄のままにします

     ::: tip

     この値が空の場合、メッセージ全体がデータベースに保存されます。実際の値はJSONテンプレートデータです。

     :::

     SQLテンプレート内でプレースホルダー変数が未定義の場合、**Message template**上部の**Undefined Vars as Null**スイッチでルールエンジンの動作を切り替えられます：

     - **Disabled**（デフォルト）：未定義変数は文字列`undefined`としてデータベースに挿入されます。

     - **Enabled**：未定義変数は`NULL`として挿入されます。

       ::: tip

       可能な限りこのオプションは有効にしてください。無効化は後方互換性確保のためのみ推奨されます。

       :::

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：必要に応じて**sync**または**async**クエリモードを選択します。詳細は[Sinkの機能](./data-bridges.md#features-of-sink)を参照してください。

11. **Create**をクリックする前に、**Test Connectivity**でSinkがサーバーに接続できるかテスト可能です。

12. **Create**ボタンをクリックし、Sink設定を完了します。新しいSinkが**Action Outputs**に追加されます。

13. **Create Rule**ページに戻り、設定内容を確認して**Create**ボタンをクリックしルールを生成します。

これでDynamoDB Sinkを通じたデータ転送ルールの作成が完了しました。**Integration** -> **Rules**ページで新規作成ルールを確認できます。**Actions(Sink)**タブをクリックすると新しいDynamoDB Sinkが表示されます。

また、**Integration** -> **Flow Designer**を開くとトポロジーが表示され、トピック`t/#`配下のメッセージがルール`my_rule`で処理されDynamoDBに送信・保存されていることが確認できます。

## イベント記録用DynamoDB Sinkのルール作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みのSink経由でDynamoDBテーブル`mqtt_msg`に書き込むルール作成方法を説明します。

::: tip

利便性のため、オンライン／オフラインイベント受信用に`mqtt_msg`トピックを再利用します。

:::

ルールおよびアクション作成手順は[メッセージ保存用DynamoDB Sinkのルール作成](#メッセージ保存用dynamodb-sinkのルール作成)とほぼ同様ですが、SQLルール文が異なります。

オンライン／オフライン状態記録用のSQLルール文は以下の通りです：

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

Sinkの稼働状況を確認すると、1件の新規受信メッセージと1件の新規送信メッセージ、及び2件のイベントレコードがあるはずです。

データが`mqtt_msg`テーブルに書き込まれているか確認します。

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
