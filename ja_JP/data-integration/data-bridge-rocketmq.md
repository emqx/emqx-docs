# Bridge MQTT Data into RocketMQ

EMQXは[RocketMQ](https://rocketmq.apache.org/)へのデータブリッジをサポートしており、MQTTメッセージやクライアントイベントをRocketMQに転送できます。例えば、RocketMQを使ってデバイスからのセンサーデータやログデータを収集することが可能です。

本ページでは、EMQXとRocketMQ間のデータ統合の詳細な概要と、データ統合の作成および検証に関する実践的な手順を提供します。

::: tip 注意

このデータ統合は、Alibaba CloudがホストするRocketMQサービスを利用する場合、バッチモードをサポートしていません。

:::

## 動作の仕組み

RocketMQデータ統合は、EMQXに標準搭載された機能であり、EMQXのリアルタイムデータキャプチャと送信能力と、RocketMQの強力なメッセージキュー処理能力を組み合わせています。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、EMQXからRocketMQへのデータ取り込みが簡素化され、複雑なコーディングを必要としません。

以下の図は、EMQXとRocketMQ間の典型的なデータ統合アーキテクチャを示しています。

![EMQX Integration RocketMQ](./assets/emqx-integration-rocketmq.png)

MQTTデータをRocketMQに取り込む流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：産業用IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、リアルタイムMQTTデータをEMQXにパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQXで定義されたルールにより処理されます。ルールは事前定義された条件に基づき、RocketMQへルーティングすべきメッセージを判別します。ペイロード変換を指定するルールがある場合は、データ形式の変換、特定情報のフィルタリング、ペイロードの付加情報による拡充などが適用されます。
3. **RocketMQへのデータ取り込み**：ルールによる処理が完了すると、メッセージをRocketMQに転送するアクションがトリガーされます。処理済みデータはシームレスにRocketMQに書き込まれます。
4. **データの保存と活用**：データがRocketMQに保存されることで、企業はそのクエリ機能を活用して様々なユースケースに対応できます。例えば金融業界では、RocketMQを信頼性の高い高性能メッセージキューとして利用し、決済端末や取引システムからのデータを管理できます。メッセージをデータ分析や規制プラットフォームに連携させ、リスク管理、不正検知・防止、規制遵守などの要件を満たします。

## 特徴と利点

RocketMQとのデータ統合は、以下の特徴と利点をビジネスにもたらします。

- **信頼性の高いIoTデータメッセージ配信**：EMQXはMQTTメッセージを信頼性高くバッチ処理しRocketMQに送信でき、IoTデバイスとRocketMQおよびアプリケーションシステムの統合を可能にします。
- **MQTTメッセージの変換**：ルールエンジンを用いて、EMQXはMQTTメッセージのフィルタリングや変換が可能です。データ抽出、フィルタリング、拡充、変換を行った上でRocketMQに送信できます。
- **クラウドネイティブな弾力的スケーリング**：EMQXとRocketMQは共にクラウドネイティブアーキテクチャ上に構築されており、Kubernetes（K8s）に対応し、クラウドネイティブエコシステムと統合可能です。ビジネスの急速な成長に応じて無限かつ弾力的にスケールアウトできます。
- **柔軟なトピックマッピング**：RocketMQデータ統合はMQTTトピックからRocketMQトピックへの柔軟なマッピングをサポートし、RocketMQメッセージ内のキー（Key）や値（Value）の設定を容易に行えます。
- **高スループット環境での処理能力**：RocketMQデータ統合は同期・非同期の両書き込みモードをサポートし、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。

## はじめる前に

このセクションでは、RocketMQデータ統合を作成する前に必要な準備、特にRocketMQサーバーのセットアップ方法について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### RocketMQのインストール

1. RocketMQをセットアップするためのdocker-composeファイル `rocketmq.yaml` を用意します。

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

2. RocketMQの実行に必要なフォルダと設定を準備します。

```bash
mkdir rocketmq
mkdir rocketmq/logs
mkdir rocketmq/store
mkdir rocketmq/conf
```

3. 以下の内容を `rocketmq/conf/broker.conf` に保存します。

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

Linux環境の場合は、`host.docker.internal` を実際のIPアドレスに変更してください。

:::

## コネクターの作成

このセクションでは、SinkをRocketMQサーバーに接続するためのコネクター作成方法を説明します。

以下の手順は、EMQXとRocketMQの両方をローカルマシンで実行していることを前提としています。リモートで実行している場合は設定を適宜調整してください。

1. EMQXダッシュボードに入り、**Integration** -> **Connectors** をクリックします。
2. ページ右上の **Create** をクリックします。
3. **Create Connector** ページで **RocketMQ** を選択し、次へ進みます。
4. **Configuration** ステップで以下を設定します：
   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_rocketmq`
   - **Servers**：`127.0.0.1:9876` と入力します。
   - **Namespace**：RocketMQサービスにネームスペースが設定されていない限り空欄のままにします。
   - **AccessKey**、**SecretKey**、**Secret Token**：RocketMQサービスの設定に応じて空欄のままか入力します。
   - その他はデフォルトのままにします。
5. 詳細設定（任意）：詳細は[Sinkの特徴](./data-bridges.md#features-of-sink)を参照してください。
6. **Create**をクリックする前に、**Test Connectivity** をクリックしてコネクターがRocketMQサーバーに接続可能かテストできます。
7. ページ下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで、**Back to Connector List** をクリックするか、**Create Rule** をクリックしてルール作成に進み、RocketMQへのデータ転送やクライアントイベントの記録を指定できます。詳細は[メッセージ保存用RocketMQ Sinkのルール作成](#create-a-rule-with-rocketmq-sink-for-message-storage)および[イベント記録用RocketMQ Sinkのルール作成](#create-a-rule-with-rocketmq-sink-for-events-recording)を参照してください。

## メッセージ保存用RocketMQ Sinkのルール作成

このセクションでは、ダッシュボード上で、ソースMQTTトピック `t/#` からのメッセージを処理し、処理済みデータを設定済みSink経由でRocketMQトピック `TopicTest` に転送するルールの作成方法を説明します。

1. EMQXダッシュボードで、**Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDに `my_rule` と入力します。メッセージ保存用ルールを作成するために、**SQL Editor** に以下の文を入力します。これはトピック `t/#` 配下のMQTTメッセージをRocketMQに保存することを意味します。

   注意：独自のSQL構文を指定する場合は、Sinkが必要とするすべてのフィールドを `SELECT` 部分に含めていることを確認してください。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は、**SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストを行うことを推奨します。

   :::

4. + **Add Action** ボタンをクリックして、ルールでトリガーされるアクションを定義します。このアクションにより、EMQXはルールで処理したデータをRocketMQに送信します。

5. **Type of Action** ドロップダウンから `RocketMQ` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みのSinkがあれば選択可能です。この例では新規Sinkを作成します。

6. Sink名を入力します。英数字の組み合わせで指定してください。

7. **Connector** ドロップダウンから先に作成した `my_rocketmq` を選択します。ドロップダウン横のボタンから新規コネクター作成も可能です。設定パラメータは[コネクターの作成](#create-a-connector)を参照してください。

8. **RocketMQ Topic** フィールドに `TopicTest` と入力します。

9. **Template** はデフォルトで空欄のままにします。

   ::: tip

   この値が空欄の場合、メッセージ全体がRocketMQに転送されます。実際の値はJSONテンプレートデータです。

   :::

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **詳細設定（任意）**：詳細は[Sinkの特徴](./data-bridges.md#features-of-sink)を参照してください。

12. **Create** をクリックする前に、**Test Connectivity** をクリックしてSinkがRocketMQサーバーに接続可能かテストできます。

13. **Create** ボタンをクリックしてSink設定を完了します。新しいSinkが **Action Outputs** に追加されます。

14. **Create Rule** ページに戻り、設定内容を確認します。**Create** ボタンをクリックしてルールを生成します。

これでRocketMQ Sink用のルールが正常に作成されました。**Integration** -> **Rules** ページで新規ルールを確認できます。**Actions(Sink)** タブをクリックすると新しいRocketMQ Sinkが表示されます。

また、**Integration** -> **Flow Designer** をクリックしてトポロジーを確認すると、トピック `t/#` 配下のメッセージがルール `my_rule` によって解析され、RocketMQに送信・保存されていることがわかります。

## イベント記録用RocketMQ Sinkのルール作成

このセクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みSink経由でRocketMQトピック `TestTopic` に転送するルールの作成方法を説明します。

ルール作成手順は[メッセージ保存用RocketMQ Sinkのルール作成](#create-a-rule-with-rocketmq-sink-for-message-storage)とほぼ同じですが、SQLルールの構文が異なります。

オンライン／オフライン状態記録用のSQLルール構文は以下の通りです。

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

::: tip

便宜上、オンライン／オフラインイベントの受け取りには `TopicTest` トピックを再利用します。

:::

## ルールのテスト

MQTTXを使ってトピック `t/1` にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello RocketMQ" }'
```

Sinkの稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ1件ずつあるはずです。

データが `TopicTest` トピックに転送されているか確認してください。

以下のようなデータがコンシューマーにより出力されます。

```bash
ConsumeMessageThread_please_rename_unique_group_name_4_1 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=581, queueOffset=0, sysFlag=0, bornTimestamp=1679037578889, bornHost=/172.26.83.106:43920, storeTimestamp=1679037578891, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000060E, commitLogOffset=1550, bodyCRC=7414108, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_2 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=511, queueOffset=1, sysFlag=0, bornTimestamp=1679037580174, bornHost=/172.26.83.106:43920, storeTimestamp=1679037580176, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F0000000000000E61, commitLogOffset=3681, bodyCRC=1604860416, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_3 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=458, queueOffset=2, sysFlag=0, bornTimestamp=1679037584933, bornHost=/172.26.83.106:43920, storeTimestamp=1679037584934, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000166E, commitLogOffset=5742, bodyCRC=383397630, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
```
