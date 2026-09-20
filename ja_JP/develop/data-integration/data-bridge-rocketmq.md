# Bridge MQTT Data into RocketMQ

EMQX は [RocketMQ](https://rocketmq.apache.org/) へのデータブリッジをサポートしており、MQTT メッセージやクライアントイベントを RocketMQ に転送できます。例えば、RocketMQ を利用してデバイスからのセンサーデータやログデータを収集することが可能です。

本ページでは、EMQX と RocketMQ 間のデータ統合について詳しく解説し、データ統合の作成および検証に関する実践的な手順を提供します。

::: tip 注意

Alibaba Cloud が提供する RocketMQ サービスを利用する場合、本データ統合はバッチモードをサポートしていません。

:::

## 動作概要

RocketMQ データ統合は、EMQX に標準搭載された機能であり、EMQX のリアルタイムデータキャプチャと送信能力を RocketMQ の強力なメッセージキュー処理能力と組み合わせています。内蔵の [ルールエンジン](./rules.md) コンポーネントにより、EMQX から RocketMQ へのデータ取り込みを簡素化し、複雑なコーディングを不要にします。

以下の図は、EMQX と RocketMQ 間のデータ統合の典型的なアーキテクチャを示しています。

![EMQX Integration RocketMQ](./assets/emqx-integration-rocketmq.png)

MQTT データを RocketMQ に取り込む流れは以下の通りです：

1. **メッセージのパブリッシュと受信**：産業用 IoT デバイスは MQTT プロトコルを介して EMQX に正常に接続し、リアルタイムの MQTT データを EMQX にパブリッシュします。EMQX はこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。  
2. **メッセージデータの処理**：メッセージが到着するとルールエンジンを通過し、EMQX に定義されたルールにより処理されます。ルールは事前定義された条件に基づき、どのメッセージを RocketMQ にルーティングするかを決定します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの拡充などが適用されます。
3. **RocketMQ へのデータ取り込み**：ルールでメッセージが処理されると、RocketMQ への転送アクションがトリガーされます。処理済みデータはシームレスに RocketMQ に書き込まれます。
4. **データの保存と活用**：データが RocketMQ に保存されることで、企業はそのクエリ機能を活用してさまざまなユースケースに対応可能です。例えば金融業界では、RocketMQ を信頼性の高い高性能メッセージキューとして利用し、決済端末や取引システムからのデータを保存・管理します。これにより、リスク管理、不正検知・防止、規制遵守などの要件を満たすためのデータ分析や規制プラットフォームとの連携が可能となります。

## 特長とメリット

RocketMQ とのデータ統合は、以下の特長と利点をビジネスにもたらします：

- **信頼性の高い IoT データメッセージ配信**：EMQX は MQTT メッセージをバッチ処理で確実に RocketMQ に送信でき、IoT デバイスと RocketMQ およびアプリケーションシステムの統合を実現します。
- **MQTT メッセージ変換**：ルールエンジンを利用して、EMQX は MQTT メッセージのフィルタリングや変換を行えます。データ抽出、フィルタリング、拡充、変換を経てから RocketMQ に送信可能です。
- **クラウドネイティブな弾力的スケーリング**：EMQX と RocketMQ は共にクラウドネイティブアーキテクチャ上に構築されており、Kubernetes（K8s）をはじめとするクラウドネイティブエコシステムと親和性が高いです。ビジネスの急速な成長に対応し、無限かつ弾力的なスケールアウトが可能です。
- **柔軟なトピックマッピング**：RocketMQ データ統合は、MQTT トピックと RocketMQ トピックの柔軟なマッピングをサポートし、RocketMQ メッセージ内のキー（Key）や値（Value）の設定も簡単に行えます。
- **高スループットシナリオでの処理能力**：RocketMQ データ統合は同期・非同期の書き込みモードをサポートし、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。

## はじめる前に

本セクションでは、RocketMQ データ統合の作成を開始する前に必要な準備、特に RocketMQ サーバーのセットアップ方法について説明します。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### RocketMQ のインストール

1. RocketMQ をセットアップするための docker-compose ファイル `rocketmq.yaml` を準備します。

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

2. RocketMQ の実行に必要なフォルダと設定を準備します。

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

Linux 環境では、`host.docker.internal` を実際の IP アドレスに変更してください。

:::

## コネクターの作成

本セクションでは、Sink を RocketMQ サーバーに接続するためのコネクター作成手順を示します。

以下の手順は、EMQX と RocketMQ の両方をローカルマシンで実行していることを前提としています。リモート環境で実行している場合は設定を適宜調整してください。

1. EMQX ダッシュボードにアクセスし、**Integration** -> **Connectors** をクリックします。
2. 画面右上の **Create** をクリックします。
3. **Create Connector** ページで **RocketMQ** を選択し、**Next** をクリックします。
4. **Configuration** ステップで以下の情報を設定します：
   - **Connector name**：コネクター名を入力します。英数字の組み合わせで、例：`my_rocketmq`
   - **Servers**：`127.0.0.1:9876` と入力します。
   - **Namespace**：RocketMQ サービスにネームスペースが設定されていなければ空欄のままにします。
   - **AccessKey**、**SecretKey**、**Secret Token**：RocketMQ サービスの設定に応じて空欄のままか適宜入力します。
   - その他はデフォルトのままにします。
5. 詳細設定（任意）：詳細は [Features of Sink](./data-bridges.md#features-of-sink) を参照してください。
6. **Create** をクリックする前に、**Test Connectivity** をクリックしてコネクターが RocketMQ サーバーに接続可能かテストできます。
7. 画面下部の **Create** ボタンをクリックしてコネクター作成を完了します。ポップアップダイアログで **Back to Connector List** をクリックするか、**Create Rule** をクリックして Sink を用いたルール作成に進めます。詳細は [Create a Rule with RocketMQ Sink for Message Storage](#create-a-rule-with-rocketmq-sink-for-message-storage) および [Create a Rule with RocketMQ Sink for Events Recording](#create-a-rule-with-rocketmq-sink-for-events-recording) を参照してください。

## RocketMQ Sink を用いたメッセージ保存ルールの作成

本セクションでは、Dashboard でソース MQTT トピック `t/#` からのメッセージを処理し、処理済みデータを設定済みの Sink 経由で RocketMQ トピック `TopicTest` に転送するルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. 画面右上の **Create** をクリックします。

3. ルール ID に `my_rule` と入力します。メッセージ保存用のルールを作成するため、**SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` 配下の MQTT メッセージを RocketMQ に保存することを意味します。

   注意：独自の SQL 構文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストを行うことができます。

   :::

4. + **Add Action** ボタンをクリックし、ルールによってトリガーされるアクションを定義します。このアクションにより、EMQX はルールで処理したデータを RocketMQ に送信します。

5. **Type of Action** ドロップダウンリストから `RocketMQ` を選択します。**Action** ドロップダウンはデフォルトの `Create Action` のままにします。既に作成済みの Sink があれば選択可能ですが、本デモでは新規 Sink を作成します。

6. Sink の名前を入力します。英数字の組み合わせで指定してください。

7. **Connector** ドロップダウンから先ほど作成した `my_rocketmq` を選択します。隣のボタンから新規コネクターを作成することも可能です。設定パラメータは [Create a Connector](#create-a-connector) を参照してください。

8. 以下の RocketMQ 固有フィールドを入力します：

   - **RocketMQ Topic**：メッセージを転送するトピック名を入力します。例：`TopicTest`
   - **Tag**（任意）：RocketMQ タグを動的に割り当てるテンプレートフィールドです。値にはルール SQL の結果で実行時に置換されるプレースホルダーを含められます。例：`${msg_type}`、`${clientid}`。空欄の場合はタグなしとなります。
   - **Key**（任意）：各メッセージにキーを割り当てるテンプレートフィールドです。メッセージのトレースや重複排除に役立ちます。例：`${device_id}`、`${username}`。こちらもルール SQL 結果からのプレースホルダーをサポートします。

9. **Message Template** フィールドで、RocketMQ に送信するメッセージペイロードの構造をカスタマイズできます：

   > デフォルト値は空欄です。空欄の場合、メッセージ全体が RocketMQ に転送されます。
   >

   テンプレートは任意の有効な文字列で、プレースホルダーを含められます。例：

   - `${id}`, `${username}`, `${clientid}`, `${timestamp}`
   - `{"id": ${id}, "username": ${username}}`

   実際の値は文字列で、JSON 形式のテンプレートも可能です。プレースホルダーはルール SQL で選択したフィールドで実行時に置換されます。

10. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリ Sink が処理に失敗した場合にトリガーされます。詳細は [Fallback Actions](./data-bridges.md#fallback-actions) を参照してください。

11. **詳細設定（任意）**：詳細は [Features of Sink](./data-bridges.md#features-of-sink) を参照してください。

12. **Create** をクリックする前に、**Test Connectivity** をクリックして Sink が RocketMQ サーバーに接続可能かテストできます。

13. **Create** ボタンをクリックして Sink 設定を完了します。新しい Sink が **Action Outputs** に追加されます。

14. **Create Rule** ページで設定内容を確認し、**Save** ボタンをクリックしてルールを生成します。

これで RocketMQ Sink 用のルールが正常に作成されました。**Integration** -> **Rules** ページで新規ルールを確認できます。**Actions(Sink)** タブをクリックすると、新しい RocketMQ Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認でき、トピック `t/#` 配下のメッセージがルール `my_rule` によって解析され、RocketMQ に送信・保存されていることがわかります。

## RocketMQ Sink を用いたイベント記録ルールの作成

本セクションでは、クライアントのオンライン／オフライン状態を記録し、イベントデータを設定済みの Sink 経由で RocketMQ トピック `TestTopic` に転送するルールの作成方法を示します。

ルール作成手順は [RocketMQ Sink を用いたメッセージ保存ルールの作成](#rocketmq-sink-を用いたメッセージ保存ルールの作成) とほぼ同様ですが、SQL ルール構文が異なります。

オンライン／オフライン状態記録用の SQL ルール構文は以下の通りです：

```sql
SELECT
  *
FROM 
  "$events/client_connected", "$events/client_disconnected"
```

::: tip

利便性のため、オンライン／オフラインイベントの受け取りに `TopicTest` トピックを再利用します。

:::

## ルールのテスト

MQTTX を使ってトピック `t/1` にメッセージを送信し、オンライン／オフラインイベントをトリガーします。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello RocketMQ" }'
```

Sink の稼働状況を確認すると、新規の受信メッセージと送信メッセージがそれぞれ1件ずつ存在するはずです。

データが `TopicTest` トピックに転送されているか確認してください。

以下のデータがコンシューマーにより出力されます。

```bash
ConsumeMessageThread_please_rename_unique_group_name_4_1 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=581, queueOffset=0, sysFlag=0, bornTimestamp=1679037578889, bornHost=/172.26.83.106:43920, storeTimestamp=1679037578891, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000060E, commitLogOffset=1550, bodyCRC=7414108, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_2 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=511, queueOffset=1, sysFlag=0, bornTimestamp=1679037580174, bornHost=/172.26.83.106:43920, storeTimestamp=1679037580176, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F0000000000000E61, commitLogOffset=3681, bodyCRC=1604860416, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
ConsumeMessageThread_please_rename_unique_group_name_4_3 Receive New Messages: [MessageExt [brokerName=broker-a, queueId=3, storeSize=458, queueOffset=2, sysFlag=0, bornTimestamp=1679037584933, bornHost=/172.26.83.106:43920, storeTimestamp=1679037584934, storeHost=/172.26.83.106:10911, msgId=AC1A536A00002A9F000000000000166E, commitLogOffset=5742, bodyCRC=383397630, reconsumeTimes=0, preparedTransactionOffset=0, toString()=Message{topic='TopicTest', flag=0, properties={MIN_OFFSET=0, MAX_OFFSET=8, CONSUME_START_TIME=1679037605342, CLUSTER=DefaultCluster}, body=[...], transactionId='null'}]]
```
