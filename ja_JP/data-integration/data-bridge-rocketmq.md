# Bridge MQTT Data into RocketMQ

EMQXは[RocketMQ](https://rocketmq.apache.org/)へのデータブリッジをサポートしており、MQTTメッセージやクライアントイベントをRocketMQに転送できます。例えば、RocketMQを利用してデバイスからのセンサーデータやログデータを収集することが可能です。

本ページでは、EMQXとRocketMQ間のデータ統合の詳細な概要と、データ統合の作成および検証に関する実践的な手順を提供します。

::: tip 注意

このデータ統合は、Alibaba CloudがホストするRocketMQサービスを使用する場合、バッチモードをサポートしていません。

:::

## 動作概要

RocketMQデータ統合は、EMQXに標準搭載された機能であり、EMQXのリアルタイムデータキャプチャと送信機能とRocketMQの強力なメッセージキュー処理機能を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからRocketMQへのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQXとRocketMQ間のデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration RocketMQ](./assets/emqx-integration-rocketmq.png)

MQTTデータをRocketMQに取り込む流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、リアルタイムMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQXで定義されたルールによって処理されます。ルールは事前定義された条件に基づき、RocketMQへルーティングすべきメッセージを判定します。ペイロードの変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **RocketMQへのデータ取り込み**：ルールによる処理が完了すると、メッセージをRocketMQに転送するアクションがトリガーされます。処理済みデータはシームレスにRocketMQへ書き込まれます。
4. **データの保存と活用**：データがRocketMQに保存された後、企業はそのクエリ機能を活用して様々なユースケースに対応できます。例えば金融業界では、RocketMQを信頼性の高い高性能メッセージキューとして利用し、決済端末や取引システムからのデータを管理します。これにより、リスク管理、不正検知・防止、規制遵守などの要件を満たすためのデータ分析や規制プラットフォームと連携が可能です。

## 特長と利点

RocketMQとのデータ統合は、以下の特長とメリットをビジネスにもたらします：

- **信頼性の高いIoTデータメッセージ配信**：EMQXはMQTTメッセージをバッチ処理で確実にRocketMQへ送信でき、IoTデバイスとRocketMQおよびアプリケーションシステムの統合を実現します。
- **MQTTメッセージの変換**：ルールエンジンを用いてEMQXはMQTTメッセージのフィルタリングや変換が可能です。データ抽出、フィルタリング、強化、変換を経てRocketMQに送信されます。
- **クラウドネイティブな弾力的スケーリング**：EMQXとRocketMQは共にクラウドネイティブアーキテクチャ上に構築されており、Kubernetes（K8s）に対応しクラウドネイティブエコシステムと統合可能です。ビジネスの急速な成長に対応するため無限かつ弾力的にスケールアウトできます。
- **柔軟なトピックマッピング**：RocketMQデータ統合はMQTTトピックからRocketMQトピックへの柔軟なマッピングをサポートし、RocketMQメッセージ内のキー（Key）および値（Value）の設定を簡単に行えます。
- **高スループットシナリオでの処理能力**：RocketMQデータ統合は同期・非同期の書き込みモードに対応し、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。

## はじめる前に

このセクションでは、RocketMQデータ統合の作成を始める前に必要な準備、特にRocketMQサーバーのセットアップ方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### RocketMQのインストール

1. RocketMQをセットアップするためのdocker-composeファイル`rocketmq.yaml`を準備します。

```yaml
version: '3.9'

services:
  mqnamesrv:
    image: apache/rocketmq:4.9.4
    container_name: rocketmq_namesrv
    ports:
      - 9876:9876
    volumes:
      - ./rocketmq/logs:/opt/logs
      - ./rocketmq/store:/opt/store
    command: ./mqnamesrv

  mqbroker:
    image: apache/rocketmq:4.9.4
    container_name: rocketmq_broker
    ports:
      - 10909:10909
      - 10911:10911
    volumes:
      - ./rocketmq/logs:/opt/logs
      - ./rocketmq/store:/opt/store
      - ./rocketmq/conf/broker.conf:/etc/rocketmq/broker.conf
    environment:
        NAMESRV_ADDR: "rocketmq_namesrv:9876"
        JAVA_OPTS: " -Duser.home=/opt"
        JAVA_OPT_EXT: "-server -Xms1024m -Xmx1024m -Xmn1024m"
    command: ./mqbroker -c /etc/rocketmq/broker.conf
    depends_on:
      - mqnamesrv
```

2. RocketMQの実行に必要なフォルダーと設定を準備します。

```bash
mkdir rocketmq
mkdir rocketmq/logs
mkdir rocketmq/store
mkdir rocketmq/conf
```

3. 以下の内容を`rocketmq/conf/broker.conf`に保存します。

```bash
brokerClusterName=DefaultCluster
brokerName=broker-a
brokerId=0

brokerIP1=change me to your real IP address

defaultTopicQueueNums=4
autoCreateTopicEnable=true
autoCreateSubscriptionGroup=true

listenPort=10911
deleteWhen=04

fileReservedTime=120
mapedFileSizeCommitLog=1073741824
mapedFileSizeConsumeQueue=300000
diskMaxUsedSpaceRatio=100
maxMessageSize=65536

brokerRole=ASYNC_MASTER

flushDiskType=ASYNC_FLUSH
```

4. サーバーを起動します。

```bash
docker-compose -f rocketmq.yaml up
```

5. コンシューマーを起動します。

```
docker run --rm -e NAMESRV_ADDR=host.docker.internal:9876 apache/rocketmq:4.9.4 ./tools.sh org.apache.rocketmq.example.quickstart.Consumer
```

::: tip

Linux環境では、`host.docker.internal`を実際のIPアドレスに変更してください。

:::

## コネクターの作成

このセクションでは、SinkをRocketMQサーバーに接続するためのコネクター作成方法を示します。

以下の手順は、EMQXとRocketMQをローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックします。
3. **Create Connector**ページで**RocketMQ**を選択し、**Next**をクリックします。
4. **Configuration**ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_rocketmq`。
   - **Servers**：`127.0.0.1:9876`と入力します。
   - **Namespace**：RocketMQサービスでネームスペースを設定していない場合は空欄のままにします。
   - **AccessKey**、**SecretKey**、**Secret Token**：必要に応じて設定、または空欄のままにします。
   - その他はデフォルトのままにします。
5. 高度な設定（任意）：詳細は[Sinkの特長](./data-bridges.md#features-of-sink)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity**をクリックしてコネクターがRocketMQサーバーに接続できるかテストできます。
7. ページ下部の**Create**ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで**Back to Connector List**をクリックするか、**Create Rule**をクリックしてSinkを使ったルール作成に進めます。詳細は[メッセージ保存用RocketMQ Sinkルールの作成](#create-a-rule-with-rocketmq-sink-for-message-storage)および[イベント記録用RocketMQ Sinkルールの作成](#create-a-rule-with-rocketmq-sink-for-events-recording)を参照してください。

## メッセージ保存用RocketMQ Sinkルールの作成

このセクションでは、DashboardでMQTTのソーストピック`t/#`からメッセージを処理し、処理済みデータを設定済みSink経由でRocketMQトピック`TopicTest`に転送するルールの作成方法を示します。

1. EMQXダッシュボードで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDに`my_rule`と入力します。メッセージ保存用のルールを作成するには、**SQL Editor**に以下の文を入力します。これはトピック`t/#`配下のMQTTメッセージをRocketMQに保存することを意味します。

   注意：独自のSQL構文を指定する場合は、Sinkが必要とするすべてのフィールドが`SELECT`句に含まれていることを確認してください。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は、**SQL Examples**をクリックし、**Enable Test**でSQLルールの学習とテストを行うことを推奨します。

   :::

4. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをRocketMQに送信します。

5. **Type of Action**のドロップダウンリストから`RocketMQ`を選択します。**Action**はデフォルトの`Create Action`のままにします。既に作成済みのSinkがあれば選択可能です。本例では新規Sinkを作成します。

6. Sink名を入力します。英数字の組み合わせで指定してください。

7. **Connector**のドロップダウンから先に作成した`my_rocketmq`を選択します。隣のボタンから新規コネクター作成も可能です。設定パラメーターは[コネクターの作成](#create-a-connector)を参照してください。

8. **RocketMQ Topic**欄に`TopicTest`と入力します。

9. **Template**はデフォルトで空欄のままにします。

   ::: tip

   ここを空欄にするとメッセージ全体がRocketMQに転送されます。実際にはJSONテンプレートデータです。

   :::

10. **Fallback Actions（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **高度な設定（任意）**：詳細は[Sinkの特長](./data-bridges.md#features-of-sink)を参照してください。

12. **Create**をクリックする前に、**Test Connectivity**をクリックしてSinkがRocketMQサーバーに接続できるか確認できます。

13. **Create**ボタンをクリックしてSink設定を完了します。新規Sinkが**Action Outputs**に追加されます。

14. **Create Rule**ページに戻り、設定内容を確認して**Create**をクリックしルールを生成します。

これでRocketMQ Sink用のルールが正常に作成されました。**Integration** -> **Rules**ページで新規ルールを確認できます。**Actions(Sink)**タブをクリックすると新規RocketMQ Sinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーが表示され、トピック`t/#`配下のメッセージがルール`my_rule`で解析されてRocketMQに送信・保存されていることが確認できます。

## イベント記録用RocketMQ Sinkルールの作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みSink経由でRocketMQトピック`TestTopic`に転送するルールの作成方法を示します。

ルール作成手順は[メッセージ保存用RocketMQ Sinkルールの作成](#create-a-rule-with-rocketmq-sink-for-message-storage)とほぼ同様ですが、SQLルール構文が異なります。

オンライン／オフライン状態記録用のSQLルール構文は以下の通りです：

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

::: tip

便宜上、オンライン／オフラインイベントの受け取りに`TopicTest`トピックを再利用します。

:::

## ルールのテスト

MQTTXを使ってトピック`t/1`にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello RocketMQ" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージが1件ずつあるはずです。

データが`TopicTest`トピックに転送されているか確認してください。

以下のようなデータがコンシューマーにより出力されます。

```bash
ConsumeMessageThread_please_rename_unique_group_name_4_1 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=581, queueOffset=0, sysFlag=0, bornTimestamp=1679037578889, bornHost=/172.26.83.106:43920, storeTimestamp=1679037578891, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000060E, commitLogOffset=1550, bodyCRC=7414108, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_2 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=511, queueOffset=1, sysFlag=0, bornTimestamp=1679037580174, bornHost=/172.26.83.106:43920, storeTimestamp=1679037580176, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F0000000000000E61, commitLogOffset=3681, bodyCRC=1604860416, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_3 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=458, queueOffset=2, sysFlag=0, bornTimestamp=1679037584933, bornHost=/172.26.83.106:43920, storeTimestamp=1679037584934, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000166E, commitLogOffset=5742, bodyCRC=383397630, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
```
