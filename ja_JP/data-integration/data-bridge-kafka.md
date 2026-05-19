# Apache KafkaへのMQTTデータストリーミング

[Apache Kafka](https://kafka.apache.org/)は、高スループットかつリアルタイムのデータ処理を目的とした広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafkaクライアントは安定したネットワーク接続と高いシステムリソースを必要とするため、エッジIoT通信には適していません。IoTシナリオでは、デバイスは一般的に軽量なMQTTプロトコルを使用して、不安定なネットワーク上でも効率的にデータを送信します。

EMQXはMQTTとKafka/[Confluent](https://www.confluent.io/)を統合し、IoTデバイスとバックエンドシステム間のシームレスなデータストリーミングを実現します。MQTTメッセージはKafkaトピックに取り込まれ、リアルタイム処理、保存、分析に利用される一方で、KafkaトピックからのデータはMQTTクライアントに配信され、タイムリーなアクションをトリガーすることも可能です。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQXとKafkaのデータ統合について紹介し、その作成および検証手順を段階的に解説します。

## 動作概要

Apache Kafkaとのデータ統合はEMQXの組み込み機能であり、MQTTベースのIoTデータをKafkaにストリーミングして下流の処理や分析を可能にします。組み込みの[ルールエンジン](./rules.md)を活用することで、カスタムコードなしにデータのフィルタリング、変換、ルーティングが行えます。

以下の図は、自動車IoTシナリオにおける典型的なEMQX–Kafka統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->

Apache Kafkaへデータを流入または流出させるには、Kafka Sink（Kafkaへメッセージを送信）またはKafka Source（Kafkaからメッセージを受信）を作成します。以下はKafka Sinkのワークフローです。

1. **メッセージ取り込み**：車両に接続されたIoTデバイスがEMQXにMQTT接続を確立し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがメッセージを受信すると、ルールエンジンでルールマッチングを開始します。
2. **ルールベース処理**：マッチしたルールは、ペイロードのフィルタリング、変換、拡張などを定義通りに実行します。
3. **Kafkaへのデータ転送**：ルールエンジンで定義したルールがKafkaへの転送アクションをトリガーします。Kafka Sinkを使い、MQTTトピックを事前定義されたKafkaトピックにマッピングし、処理済みのメッセージとデータをKafkaトピックに書き込みます。

Kafkaにデータが取り込まれた後は、以下の方法で消費および処理が可能です。

- バックエンドサービスがKafkaトピックからリアルタイムデータストリームを直接消費。
- Kafka Streamsを用いたリアルタイム集約、相関分析、解析。
- Kafka Connectを使い、MySQLやElasticsearchなど外部システムへデータ転送し保存やさらなる処理。

## 特長とメリット

Apache Kafkaとのデータ統合は以下の特長とメリットを提供します。

- **信頼性の高い双方向IoTデータメッセージング**：EMQXは不安定なネットワーク環境でもMQTTメッセージをKafkaへ確実に転送し、バックエンドからのKafkaメッセージを接続中のIoTクライアントへ配信します。
- **ペイロード変換**：SQLルールを用いてメッセージをフィルタリング、拡張、変換してからKafkaへ転送可能です。
- **効果的なトピックマッピング**：MQTTトピックやユーザープロパティを柔軟にKafkaトピックやヘッダーにマッピングでき、1対1、1対多、ワイルドカードベースのマッピングをサポートします。
- **柔軟なパーティション選択戦略**：MQTTトピックやクライアントに基づき、同じKafkaパーティションへメッセージを転送します。
- **高スループット処理**：同期・非同期のKafka書き込みをサポートし、異なるワークロードシナリオでレイテンシとスループットをバランスさせます。
- **ランタイムメトリクス**：各SinkおよびSourceの総メッセージ数、成功/失敗数、現在のレートなどのメトリクスを閲覧可能です。
- **動的設定**：ダッシュボードまたは設定ファイルでSinkおよびSourceを動的に設定できます。

これらの機能により、効率的なデータ取り込みと管理を備えたスケーラブルでレジリエントなIoTデータプラットフォームを構築できます。

## はじめる前に

このセクションでは、EMQXダッシュボードでKafka SinkおよびSourceを作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Kafkaサーバーのセットアップ

ここではmacOSを例にインストール手順を示します。以下のコマンドでKafkaをインストールし起動できます。

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# KRaftモードでKafkaを起動
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

詳細な操作手順は[Kafkaドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/)をご参照ください。

### Kafkaトピックの作成

EMQXでデータ統合を作成する前に、Kafkaトピックを作成しておく必要があります。以下のコマンドでSink用の`testtopic-in`とSource用の`testtopic-out`の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafkaプロデューサーコネクターの作成

Kafka Sinkアクションを追加する前に、EMQXとKafka間の接続を確立するためKafkaプロデューサーコネクターを作成します。

1. EMQXダッシュボードで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例：`my-kafka`。名前はKafka Sinkとコネクターを紐付けるために使用し、クラスター内で一意である必要があります。

4. Kafka接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**：`127.0.0.1:9092`と入力します。デモではEMQXとKafkaをローカルで実行している前提です。リモート環境の場合は適宜調整してください。

   - **Authentication**：Kafkaクラスターで必要な認証方式を選択します。以下の方式をサポートしています。

     - `None`：認証不要。
     - `AWS IAM for MSK`：Amazon EC2上にデプロイされたEMQXからAmazon MSKクラスターへ接続する場合に使用。
     - `OAuth`：[OAuth 2.0](https://oauth.net/2/)ベースの認証で、OAuthまたはOIDC対応のKafkaクラスターに接続。
     - `Basic Auth`：ユーザー名とパスワードで認証。`plain`、`scram_sha_256`、`scram_sha_512`のいずれかのメカニズムを選択。
     - `Kerberos`：Kerberos（GSSAPI）認証。Kerberosプリンシパルとキータブファイルの指定が必要。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照してください。

   - **Advanced Settings**（任意）：[詳細設定](#advanced-configurations)を参照。

5. **Create**をクリックする前に、**Test Connection**を押してKafkaサーバーへの接続が成功するかテスト可能です。

6. **Create**をクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にKafkaへ接続します。次に、このコネクターを利用したルールを作成し、Kafkaクラスターへデータを転送します。

### 認証方式

EMQXでKafkaコネクターを作成する際、Kafkaクラスターのセキュリティ設定に応じて以下の認証方式を選択可能です。

- **None**：認証不要。

- **MSK IAM**：Amazon EC2上のEMQXからAmazon MSKクラスターへ接続する場合に使用。

  この方式は、EC2インスタンスのメタデータサービスを利用してIAMポリシーに基づく認証トークンを生成します。

  ::: tip 重要

  MSK IAM認証は、EMQXがEC2インスタンス上で稼働しMSKクラスターに接続する場合のみサポートされます。これはEC2インスタンスメタデータサービスに依存しているためです。

  `iptables`や`nftables`でホストレベルのアウトバウンドフィルタリングを行う場合は、`169.254.169.254`へのアクセスをブロックしないでください。EMQXはMSK IAM認証のためにインスタンスメタデータサービスへアクセスする必要があります。同様の例外はS3、S3 Tables、DynamoDB、KinesisなどのAWSベースコネクターにも適用されます。詳細は[ルールエンジンポリシーとファイアウォールルールによるSSRF緩和](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。

  :::

- **OAuth**：OAuth 2.0ベースの認証で、OAuthまたはOIDC対応のKafkaクラスター（Confluent CloudやOAuth有効化済みのセルフマネージドKafkaなど）に接続します。

  EMQXはOAuth 2.0クライアントとして動作し、OAuth認可サーバーから定期的にアクセストークンを取得し、SASL/OAUTHBEARERメカニズムでKafkaブローカーに認証します。

  必須項目：

  - **OAuth Grant Type**：アクセストークン取得に使うOAuth 2.0のグラントタイプ（現在は`client_credentials`のみ対応）。
  - **OAuth Token Endpoint URI**：OAuth/OIDCプロバイダーのトークンエンドポイントURI。EMQXはここにトークンリクエストを送信。
  - **OAuth Client ID**：OAuth認可サーバーに登録されたクライアントID。
  - **OAuth Client Secret**：トークン取得時にEMQXを認証するためのクライアントシークレット。
  - **OAuth Request Scope**：（任意）トークンリクエストに含めるスコープ。
  - **SASL Extensions**：（高度な設定、任意）認証時にSASL拡張として送信する追加のキー・バリュー。Confluent Cloudなど一部のKafkaプロバイダーで必要となる場合があります。例：`logicalCluster`、`identityPoolId`。

  Confluent CloudにおけるOAuth/OIDC認証の詳細は[公式ドキュメント](https://docs.confluent.io/cloud/current/security/authenticate/workload-identities/identity-providers/oauth/overview.html)を参照してください。

- **Basic Auth**：ユーザー名とパスワードによる認証。

  選択時は以下を指定する必要があります。

  - **Mechanism**：`plain`、`scram_sha_256`、`scram_sha_512`のいずれか。
  - **Username**と**Password**：Kafkaクラスター認証用の資格情報。

- **Kerberos**：Kerberos GSSAPI認証。

  必須項目：

  - **Kerberos Principal**：認証に使用するKerberosプリンシパル。
  - **Kerberos Keytab File**：非対話認証用のキータブファイルパス。

  ::: tip 重要

  キータブファイルはすべてのEMQXノードで同じパスに配置し、EMQXサービスユーザーに読み取り権限が必要です。

  :::

## Kafka Sinkを用いたルールの作成

このセクションでは、MQTTトピック`t/#`のメッセージを処理し、Kafkaの`testtopic-in`トピックへ送信するKafka Sinkを利用したルールの作成方法を示します。

1. EMQXダッシュボードで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`。

4. **SQL Editor**に以下の文を入力し、トピック`t/#`のMQTTメッセージをKafkaに転送する設定を行います。

   注意：独自のSQL文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`句に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**や**Try It Out**をクリックしてSQLルールを学習・テストできます。

   :::

   ::: tip

   EMQX v5.7.2以降、ルールSQLで環境変数を読み取る機能が追加されました。詳細は[ルールSQLで環境変数を使う](#use-environment-variables)を参照してください。

   :::

5. **Create Rule**ページで、+ **Add Action**をクリックし、ルールの出力アクションを定義します。

6. **Type of Action**ドロップダウンから`Kafka Producer`を選択します。

   **Action**ドロップダウンはデフォルトの`Create Action`のままにします。

   > 既存のSinkを選択することも可能ですが、本例では新規作成します。

7. **Name**と任意で**Description**を入力します。

8. **Connector**ドロップダウンから先ほど作成した`my-kafka`コネクターを選択します。必要に応じて新規作成も可能です。[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

9. Sinkのデータ送信方法を設定します。

      - **Kafka Topic**：メッセージをパブリッシュするKafkaトピック。`testtopic-in`と入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。[変数テンプレートの使用](#use-variable-templates)を参照してください。
      - **Kafka Headers**：Kafkaメッセージに付加する任意のキー・バリュー形式のメタデータ。ヘッダー値はオブジェクトとして解決される必要があります。エンコード方式は**Kafka Header Value Encode Type**で選択でき、複数のヘッダーを**Add**で追加可能です。
      - **Message Key**：Kafkaメッセージのキー。パーティション割り当てやメッセージ順序付けに使用されます。静的文字列または`${.clientid}`などのプレースホルダーを含めることが可能です。
      - **Message Value**：テンプレートから生成されるKafkaメッセージのペイロード。静的文字列または`${.}`などのプレースホルダーを使い、ルールコンテキストから動的に値を生成できます。テンプレートが`NULL`（例：参照フィールドが存在しない場合）に解決されると、空文字列ではなくKafkaの`NULL`値が生成されます。
      - **Message Timestamp**：Kafkaメッセージのタイムスタンプ。固定値または`${timestamp}`などのプレースホルダーでルール出力から動的に設定可能です。
      - **Partition Strategy**：Kafkaパーティションへのメッセージ振り分け方法を選択します。
      - **Partitions Limit**：プロデューサーがメッセージを送信するパーティションの最大数を制限します。有効にすると、全パーティションではなく指定数のパーティション間でのみメッセージを分散します。
      - **Compression**：Kafkaメッセージのレコード圧縮/解凍に使用する圧縮アルゴリズムを指定します。

10. **Fallback Actions**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **Advanced Settings**（任意）：[詳細設定](#advanced-configuration)を参照してください。

12. **Create**をクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新規Sinkがルールアクションに追加されます。

13. **Create**をクリックしてルール作成を完了します。

![kafka_producer_bridge](./assets/kafka_producer_bridge.png)

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブでKafkaプロデューサーSinkも確認できます。

また、**Integration** -> **Flow Designer**でトポロジーを表示可能です。トポロジーでは、トピック`t/#`のメッセージがルール`my_rule`で解析されKafkaへ送信・保存される様子が直感的に把握できます。

### Kafka動的トピックの設定

EMQX v5.7.2以降、Kafka Producer Sink設定で環境変数や変数テンプレートを使いKafkaトピックを動的に設定可能です。本節ではこれら2つのユースケースを紹介します。

#### 環境変数の使用

EMQX v5.7.2では、ルールSQL処理中に[環境変数](../configuration/configuration.md#environment-variables)から値を動的に取得し、メッセージフィールドに割り当てる機能が追加されました。これはルールエンジンの組み込みSQL関数[getenv](../data-integration/rule-sql-builtin-functions.md#system-function)を用いてEMQXの環境変数を取得し、その値をSQL処理結果に設定します。この機能を応用し、Kafka SinkルールアクションのKafkaトピック設定にルール出力結果のフィールドを参照してトピックを指定できます。以下はその例です。

::: tip 注意

ルールエンジンが他のシステム環境変数の漏洩を防ぐため、使用する環境変数名は必ず`EMQXVAR_`という固定プレフィックスを付ける必要があります。例えば、`getenv`関数で`KAFKA_TOPIC`を読み取る場合、環境変数名は`EMQXVAR_KAFKA_TOPIC`と設定してください。

:::

1. Kafkaを起動し、`testtopic-in`というKafkaトピックを事前作成します。[はじめる前に](#before-you-start)の手順を参照してください。

2. EMQXを起動し、環境変数を設定します。zipインストールの場合、起動時に直接環境変数を指定可能です。例として、Kafkaトピック`testtopic-in`を環境変数`EMQXVAR_KAFKA_TOPIC`に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

4. Kafka Sinkルールを設定し、**SQL Editor**に以下の文を入力します。

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

5. SQLテストを有効にし、環境変数`testtopic-in`が正常に取得されていることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka Producer Sinkアクションを追加します。ルールの右側**Action Outputs**で**Add Action**をクリックします。

   - **Connector**：先ほど作成したコネクター`test-kafka`を選択。
   - **Kafka Topic**：SQLルール出力の変数テンプレート`${kafka_topic}`形式で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. その他の設定は[Kafka Sinkを用いたルールの作成](#create-a-rule-with-kafka-sink)を参照し、最後に**Create**をクリックしてルール作成を完了します。

8. [Kafkaプロデューサールールのテスト](#test-kafka-producer-rule)の手順に従い、Kafkaへメッセージを送信します。

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -i pub -t t/Connection -q 1 -m 'payload string'
   ```

   メッセージはKafkaトピック`testtopic-in`で受信されるはずです。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 \
     --topic testtopic-in
   
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   ```

#### 変数テンプレートの使用

**Kafka Topic**フィールドに静的なトピック名を設定する代わりに、変数テンプレートを使って動的にトピック名を生成可能です。これにより、メッセージ内容に基づいてKafkaトピックを構築でき、柔軟なメッセージ処理と分配が可能になります。例えば、`device-${payload.device}`のように指定すると、特定デバイスからのメッセージを`device-1`などデバイスIDをサフィックスに持つトピックに簡単に送信できます。

この例では、Kafkaに送信するメッセージペイロードに`device`キーが含まれている必要があります。以下は例のペイロードです。

```json
{
    "topic": "t/devices/data",
    "payload": {
        "device": "1",
        "temperature": 25.6,
        "humidity": 60.2
    }
}
```

このキーが含まれないとトピックのレンダリングに失敗し、回復不能なメッセージドロップが発生します。

また、Kafka側で解決されるすべてのトピック（例：`device-1`、`device-2`など）を事前作成しておく必要があります。テンプレートが存在しないトピック名に解決された場合も、メッセージは回復不能なエラーで破棄されます。

## Kafkaプロデューサールールのテスト

Kafkaプロデューサールールが期待通りに動作するかテストするため、[MQTTX](https://mqttx.app/en)を使ってEMQXにMQTTメッセージをパブリッシュするクライアントをシミュレートできます。

1. MQTTXでトピック`t/1`にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)**ページでSink名をクリックし統計情報を確認します。新規受信メッセージ数と送信メッセージ数が1件ずつ増えているはずです。

3. 以下のコマンドで`testtopic-in`トピックにメッセージが書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafkaコンシューマーコネクターの作成

Kafka Sourceアクションを追加する前に、EMQXとKafka間の接続を確立するKafkaコンシューマーコネクターを作成します。

1. EMQXダッシュボードで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックします。

3. **Create Connector**ページで **Kafka Consumer** を選択し、**Next**をクリックします。

4. ソースの名前を入力します。大文字・小文字の英数字の組み合わせで、例：`my-kafka-source`。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**：`127.0.0.1:9092`と入力します。ローカル環境を想定しています。リモート環境の場合は適宜調整してください。

   - **Authentication**：Kafkaクラスターに必要な認証方式を選択します。以下の方式をサポートしています。

     - `None`：認証不要。
     - `authentication_msk_iam`：Amazon EC2上のEMQXからAWS MSKクラスターへ接続する場合に使用。
     - `OAuth`：[OAuth 2.0](https://oauth.net/2/)認証。
     - `Basic Auth`：**Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）と**Username**、**Password**を指定。
     - `Kerberos`：**Kerberos Principal**と**Kerberos Keytab File**を指定。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は**TLS for External Resource Access**を参照してください。

   - **Advanced Settings**（任意）：[詳細設定](#advanced-configuration)を参照。

6. **Create**をクリックする前に、**Test Connection**を押してKafkaサーバーへの接続テストが可能です。

11. **Create**をクリックします。関連するルールの作成オプションが表示されます。[KafkaコンシューマーSourceを用いたルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## KafkaコンシューマーSourceを用いたルールの作成

このセクションでは、KafkaコンシューマーSourceで転送されたメッセージをEMQXでさらに処理し、MQTTトピックへ再パブリッシュするルールの作成方法を示します。

### ルールSQLの作成

1. EMQXダッシュボードで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`。

4. Kafkaソース`$bridges/kafka_consumer:<sourceName>`から変換されたメッセージをEMQXに転送する場合、**SQL Editor**に以下の文を入力します。

   注意：独自のSQL文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドを`SELECT`句に含めてください。Kafka Sourceの`SELECT`文では`ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node`などのフィールドが利用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   注意：初心者の方は**SQL Examples**や**Enable Test**をクリックしてSQLルールを学習・テストできます。

### KafkaコンシューマーSourceをデータ入力として追加

1. ルール作成ページ右側の**Data Inputs**タブを選択し、**Add Input**をクリックします。

2. **Input Type**ドロップダウンから**Kafka Consumer**を選択します。**Source**ドロップダウンはデフォルトの`Create Source`のままにするか、既存のKafkaコンシューマーSourceを選択可能です。本例では新規作成してルールに追加します。

3. ソースの名前と説明を入力します。

4. **Connector**ドロップダウンから先ほど作成した`my-kafka-consumer`コネクターを選択します。必要に応じて隣のボタンから新規作成も可能です。[Kafkaコンシューマーコネクターの作成](#create-a-kafka-consumer-connector)を参照してください。

5. 以下のフィールドを設定します。

   - **Kafka Topic**：コンシューマーソースが購読するKafkaトピック。
   - **Group ID**：このソースのコンシューマーグループID。指定しない場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode**と**Value Encoding Mode**：Kafkaメッセージのキーと値のエンコード方式を選択。

7. **Offset Reset Policy**：Kafkaコンシューマーがオフセットを持たない場合や無効な場合に読み込み開始位置を決定します。

   - `latest`を選択すると、コンシューマーは最新のオフセットから読み取りを開始し、開始前に生成されたメッセージはスキップされます。
   - `earliest`を選択すると、パーティションの先頭から読み取りを開始し、開始前に生成されたメッセージも含めてすべての履歴データを読み取ります。

8. **Advanced Settings**（任意）：[詳細設定](#advanced-configuration)を参照してください。

9. **Create**をクリックする前に、**Test Connectivity**を押してKafkaサーバーへの接続テストが可能です。

10. **Create**をクリックしてソース作成を完了します。ルール作成ページの**Data Inputs**タブに新規ソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs**タブを選択し、+ **Add Action**をクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action**ドロップダウンから**Republish**を選択します。

3. **Topic**と**Payload**フィールドに再パブリッシュするメッセージのトピックとペイロードを入力します。例として、`t/1`と`${.}`を入力します。
   - **Topic**フィールドには`${}`を使い動的にMQTTトピックを指定可能です。例：`t/${key}`（`${}`内のパラメータはSQLの`SELECT`文に含める必要があります）。

4. **Add**をクリックしてアクションをルールに追加します。

5. ルール作成ページに戻り、**Save**をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Sourceルールのテスト

Kafka Sourceとルールが期待通りに動作するかテストするため、[MQTTX](https://mqttx.app/)を使ってEMQXのトピックをサブスクライブするクライアントをシミュレートし、KafkaプロデューサーでKafkaトピックにデータを生成します。その後、KafkaからのデータがEMQXで再パブリッシュされ、クライアントのサブスクライブトピックに届くか確認します。

1. MQTTXでトピック`t/1`をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドでKafkaプロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力待ちになります。

3. `{"msg": "Hello EMQX"}`を入力して`testtopic-out`トピックにメッセージを生成し、Enterを押します。

4. MQTTXのサブスクリプションで以下のKafkaからのメッセージがトピック`t/1`で受信されることを確認します。

   ```json
   {
       "value": "{\"msg\": \"Hello EMQX\"}",
       "ts_type": "create",
       "ts": 1679665968238,
       "topic": "testtopic-out",
       "offset": 2,
       "key": "key",
       "headers": {
           "header_key": "header_value"
       }
   }
   ```

## 詳細設定

本セクションでは、データ統合のパフォーマンス最適化やシナリオに応じたカスタマイズのための詳細設定オプションを説明します。コネクター、Sink、Source作成時に**Advanced Settings**を展開し、以下の設定を業務要件に応じて調整可能です。

| 項目                                   | 説明                                                         | 推奨値              |
| -------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation              | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータ取得リクエスト時に存在しないKafkaトピックを自動作成可能にします。 | `disabled`         |
| Min Metadata Refresh Interval          | クライアントがKafkaブローカーおよびトピックメタデータを更新する最小間隔。短すぎるとKafkaサーバーに不要な負荷がかかる可能性があります。 | `3`秒              |
| Metadata Request Timeout               | ブリッジがKafkaにメタデータ要求を送る際の最大待機時間。               | `5`秒              |
| Connect Timeout                        | TCP接続確立の最大待機時間。認証有効時は認証時間も含みます。             | `5`秒              |
| Max Wait Time (Source)                 | Kafkaブローカーからのフェッチ応答を待つ最大時間。                      | `1`秒              |
| Fetch Bytes (Source)                   | 1回のフェッチリクエストでKafkaから取得するバイト数。設定値がメッセージサイズ未満だとフェッチ性能に悪影響を及ぼす可能性があります。 | `896` KB           |
| Max Batch Bytes (Sink)                 | Kafkaバッチ内でメッセージを収集する最大バイト数。Kafkaブローカーのデフォルトは1MBですが、EMQXはメッセージエンコードのオーバーヘッドを考慮し若干低めに設定しています。単一メッセージがこの制限を超える場合は別バッチとして送信されます。 | `896` KB           |
| Offset Commit Interval (Source)        | コンシューマーグループごとに送信するオフセットコミット間隔。               | `5`秒              |
| Required Acks (Sink)                   | Kafkaパーティションリーダーがフォロワーから待つアックの種類：<br />`all_isr`：全インシンクレプリカからのアックを要求<br />`leader_only`：リーダーのみからのアックを要求<br />`none`：Kafkaからのアック不要 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加検知後、指定の`partition_strategy`に基づき新パーティションへメッセージを振り分けます。 | `60`秒             |
| Max Inflight (Sink)                    | Kafkaプロデューサーがアックを受信する前に送信可能な最大バッチ数（パーティションごと）。大きいほどスループット向上が期待できますが、1より大きいとメッセージの順序が入れ替わるリスクがあります。 | `10`               |
| Query Mode (Source)                    | メッセージ送信最適化のため非同期または同期モードを選択可能。非同期モードではKafka書き込みがMQTTパブリッシュ処理をブロックしませんが、クライアントがKafka到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)       | 同期モード時の最大待機時間。メッセージ送信完了をタイムリーに保証し、長時間待機を防ぎます。同期モード時のみ有効。 | `5`秒              |
| Buffer Mode (Sink)                    | メッセージ送信前のバッファリング方式。メモリバッファは高速ですがEMQXノード再起動時にメッセージが失われます。<br />`memory`：メモリバッファ<br />`disk`：ディスクバッファ（再起動後も保持）<br />`hybrid`：メモリバッファが一定量を超えるとディスクにオフロード。再起動時はメモリモード同様メッセージが失われます。 | `memory`           |
| Per-partition Buffer Limit (Sink)      | Kafkaパーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄してバッファ空間を確保します。メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)              | バッファモードが`disk`または`hybrid`の場合に適用。メッセージ保存用のセグメントファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)      | バッファモードが`memory`の場合に有効。メモリ圧迫時に古いメッセージを自動破棄し、システムの安定性を確保します。Linuxシステムでのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size      | ネットワーク送受信性能最適化のためのソケットバッファサイズ。                  | `1024` KB          |
| TCP Keepalive                        | Kafkaブリッジ接続のTCPキープアライブ設定。長時間のアイドルによる接続切断を防止します。`Idle, Interval, Probes`の3つの数値をカンマ区切りで指定。<br />Idle：アイドル状態が続いた秒数（Linuxデフォルト7200秒）<br />Interval：キープアライブプローブ間隔（Linuxデフォルト75秒）<br />Probes：応答なしと判断するまでの最大プローブ数（Linuxデフォルト9回）<br />例：`240,30,5`は240秒アイドル後に30秒間隔で最大5回プローブを送信し応答なしなら接続切断。 | `none`             |
| Max Linger Time                     | パーティションごとのプロデューサーがバッチ収集のためにメッセージを待つ最大時間。デフォルト`0`は待機なし。メモリ以外のバッファモードでは`5ms`に設定するとIOPSが大幅削減されますがレイテンシは増加します。 | `0`ミリ秒          |
| Max Linger Bytes                    | パーティションごとのプロデューサーがバッチ収集のためにメッセージを待つ最大バイト数。 | `10` MB            |
| Health Check Interval               | コネクターの稼働状態をチェックする間隔。                                 | `15`秒             |

## 参考情報

EMQXはApache Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [MQTTとKafkaでつなぐコネクテッドビークルのストリーミングデータパイプライン：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka：IoTデータ統合の高速化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [EMQX Cloudルールエンジンを使ったデバイスデータのKafkaブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloudルールエンジンに関する動画で、将来的により適切な動画に差し替え予定）
