# Apache KafkaへMQTTデータをストリームする

[Apache Kafka](https://kafka.apache.org/)は、高スループットかつリアルタイムのデータ処理を目的とした広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafkaクライアントは安定したネットワーク接続と高いシステムリソースを必要とするため、エッジIoT通信には適していません。IoTシナリオでは、デバイスは一般的に軽量なMQTTプロトコルを使用して、不安定なネットワーク上でも効率的にデータを送信します。

EMQXはMQTTとKafka/[Confluent](https://www.confluent.io/)を統合し、IoTデバイスとバックエンドシステム間のシームレスなデータストリーミングを可能にします。MQTTメッセージはKafkaトピックに取り込まれ、リアルタイム処理、保存、分析に利用される一方で、KafkaトピックのデータはMQTTクライアントに配信され、タイムリーなアクションをトリガーできます。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページではEMQXとKafkaのデータ統合について紹介し、統合の作成と検証手順を段階的に解説します。

## 動作概要

Apache Kafkaとのデータ統合はEMQXの組み込み機能であり、MQTTベースのIoTデータをKafkaにストリームして下流処理や分析を可能にします。組み込みの[ルールエンジン](./rules.md)を活用することで、カスタムコードなしにデータのフィルタリング、変換、ルーティングが可能です。

以下の図は、自動車IoTシナリオにおける典型的なEMQX–Kafka統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->

Apache Kafkaへデータを流入または流出させるには、Kafka Sink（Kafkaへメッセージを送信）またはKafka Source（Kafkaからメッセージを受信）を作成します。以下はKafka Sinkのワークフローです。

1. **メッセージ取り込み**: 車両に接続されたIoTデバイスはEMQXにMQTT接続を確立し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがメッセージを受信すると、ルールエンジンでルールマッチングが開始されます。
2. **ルールベース処理**: マッチしたルールにより、ペイロードのフィルタリング、変換、強化などが行われます。
3. **Kafkaへのデータ転送**: ルールエンジンで定義されたルールがアクションをトリガーし、メッセージをKafkaに転送します。Kafka Sinkを使用してMQTTトピックを事前定義されたKafkaトピックにマッピングし、処理済みメッセージとデータをKafkaトピックに書き込みます。

Kafkaにデータが取り込まれた後は、以下のように複数の方法で消費・処理できます。

- バックエンドサービスがKafkaトピックからリアルタイムデータストリームを直接消費。
- Kafka Streamsを利用したリアルタイム集計、相関分析、解析。
- Kafka Connectを使い、MySQLやElasticsearchなど外部システムへデータ転送し保存・追加処理。

## 特長と利点

Apache Kafkaとのデータ統合は以下の特長と利点を提供します。

- **信頼性の高い双方向IoTデータメッセージング**: EMQXは不安定なネットワーク環境でもMQTTメッセージをKafkaに確実に転送し、バックエンドからのKafkaメッセージを接続されたIoTクライアントに届けます。
- **ペイロード変換**: メッセージはKafkaに転送する前にSQLルールでフィルタリング、強化、変換が可能です。
- **柔軟なトピックマッピング**: MQTTトピックやユーザープロパティをKafkaトピックやヘッダーに柔軟にマッピングでき、1対1、1対多、ワイルドカードベースのマッピングをサポートします。
- **柔軟なパーティション選択戦略**: MQTTトピックやクライアントに基づき、同じKafkaパーティションへメッセージを転送します。
- **高スループット処理**: 同期・非同期のKafka書き込みをサポートし、レイテンシとスループットのバランスを異なるワークロードに応じて調整可能です。
- **ランタイムメトリクス**: 各SinkおよびSourceの総メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスを表示可能です。
- **動的設定**: ダッシュボードまたは設定ファイルでSinkおよびSourceを動的に設定できます。

これらの機能により、効率的なデータ取り込みと管理を備えたスケーラブルでレジリエントなIoTデータプラットフォームを構築できます。

## はじめる前に

このセクションでは、EMQXダッシュボードでKafka SinkおよびSourceを作成する前に必要な準備について説明します。

### 前提条件

- EMQXのデータ統合[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識

### Kafkaサーバーのセットアップ

ここではmacOSを例にインストールと起動方法を示します。以下のコマンドでKafkaをインストール・起動できます。

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# KRaftモードでKafkaを起動
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

詳細な操作手順は[Kafkaドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/)を参照してください。

### Kafkaトピックの作成

EMQXでデータ統合を作成する前に、関連するKafkaトピックを作成してください。以下のコマンドでSink用の`testtopic-in`とSource用の`testtopic-out`の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafkaプロデューサーコネクターの作成

Kafka Sinkアクションを追加する前に、EMQXとKafka間の接続を確立するためのKafkaプロデューサーコネクターを作成する必要があります。

1. EMQXダッシュボードで **Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例：`my-kafka`。名前はKafka Sinkとコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. Kafka接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**: `127.0.0.1:9092`を入力します。デモではEMQXとKafkaをローカルで実行している前提です。リモート環境の場合は適宜設定を調整してください。

   - **Authentication**: Kafkaクラスターの認証方式を選択します。以下の方式をサポートしています。

     - `None`: 認証なし。
     - `AWS IAM for MSK`: EMQXがAmazon EC2上で稼働し、Amazon MSKクラスターに接続する場合に使用。
     - `MSK IAM Roles Anywhere`: EC2外の環境からAmazon MSKに接続するためにAWS IAM Roles Anywhereクレデンシャルヘルパーを使用。
     - `OAuth`: OAuth 2.0ベースの認証を使用し、OAuthまたはOIDCをサポートするKafkaクラスターに接続。
     - `Basic Auth`: ユーザー名とパスワードによる認証。`plain`、`scram_sha_256`、`scram_sha_512`のいずれかのメカニズムを選択。
     - `Kerberos`: Kerberos (GSSAPI)認証。Kerberosプリンシパルとキータブファイルを指定。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS** トグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。
   - **Request Timeout**: Kafkaからの応答を待つ最大時間（秒）。デフォルトは30秒。タイムアウト超過時は接続を再確立します。値が小さすぎると、Kafkaはリクエストを受け入れても応答を遅延させ、EMQXが再送することで重複メッセージや過剰な下流データが発生する可能性があります。

   - **Advanced Settings**（任意）: [高度な設定](#advanced-configurations)を参照。

5. **Create**をクリックする前に、**Test Connection**でKafkaサーバーへの接続が成功するか確認できます。

6. **Create**をクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にKafkaに接続します。次に、このコネクターを基にルールを作成し、Kafkaクラスターへデータを転送します。

### 認証方式

EMQXでKafkaコネクターを作成する際、Kafkaクラスターのセキュリティ設定に応じて複数の認証方式から選択できます。

- **None**: 認証なし。

- **MSK IAM**: EMQXがAmazon EC2上で稼働し、Amazon MSKクラスターに接続する場合に使用。

  AWS EC2インスタンスメタデータサービスを利用し、インスタンスに付与されたIAMポリシーに基づく認証トークンを生成します。

  ::: tip 重要なお知らせ

  MSK IAM認証は、EMQXがEC2インスタンス上で稼働しMSKクラスターに接続する場合のみサポートされます。これはEC2インスタンスメタデータサービスに依存しているためです。

  `iptables`や`nftables`でホストレベルのアウトバウンドフィルタリングを行う場合、`169.254.169.254`へのアクセスをブロックしないでください。EMQXはMSK IAM認証のためにインスタンスメタデータサービスにアクセスする必要があります。同様の例外はS3、S3 Tables、DynamoDB、KinesisなどEC2メタデータからクレデンシャルを取得するAWSベースの他のコネクターにも適用されます。詳細は[ルールエンジンポリシーとファイアウォールルールによるSSRF緩和](../../guides/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。

  :::

- **MSK IAM Roles Anywhere**: EC2外の環境（オンプレミスなど）からAWS IAM Roles Anywhereクレデンシャルヘルパーを利用してAmazon MSKに接続する場合に使用。

  クレデンシャルヘルパープロセスは`serve`モードで起動し、EMQXにHTTP APIを公開します。EMQXはこのAPIから一時的なAWSクレデンシャルを取得し、SASL/OAUTHBEARERトークンを生成してMSK IAM認証に使用します。

  必要な設定:

  - **Roles Anywhere Endpoint**: クレデンシャルヘルパーのAPIエンドポイント。例: `http://127.0.0.1:9911`
  - **AWS Region**: MSKクラスターが稼働するAWSリージョン。

- **OAuth**: OAuth 2.0ベースの認証で、OAuthまたはOIDCをサポートするKafkaクラスター（Confluent CloudやOAuth有効なセルフマネージドKafkaなど）に接続。

  EMQXはOAuth 2.0クライアントとして動作し、OAuth認可サーバーから定期的にアクセストークンを取得し、SASL/OAUTHBEARER機構でKafkaブローカーに認証します。

  必要な設定:

  - **OAuth Grant Type**: アクセストークン取得に使用するOAuth 2.0のグラントタイプ（現在は`client_credentials`のみサポート）。
  - **OAuth Token Endpoint URI**: トークンエンドポイントURI。
  - **OAuth Client ID**: OAuth認可サーバーに登録されたクライアントID。
  - **OAuth Client Secret**: クライアントIDに対応するシークレット。
  - **OAuth Request Scope**: （任意）トークンリクエストに含めるスコープ。
  - **SASL Extensions**: （高度、任意）認証時にSASL拡張として送信する追加のキー・バリュー。Confluent Cloudなど一部のKafkaプロバイダーでメタデータ（`logicalCluster`や`identityPoolId`など）を渡すために必要。

  詳細はConfluent Cloudの[公式ドキュメント](https://docs.confluent.io/cloud/current/security/authenticate/workload-identities/identity-providers/oauth/overview.html)を参照してください。

- **Basic Auth**: ユーザー名とパスワードによる認証。

  必須項目:

  - **Mechanism**: `plain`、`scram_sha_256`、`scram_sha_512`から選択。
  - **Username**、**Password**: 認証情報。

- **Kerberos**: Kerberos GSSAPI認証。

  必須項目:

  - **Kerberos Principal**: 認証に使用するKerberosプリンシパル。
  - **Kerberos Keytab File**: 非対話認証用のキータブファイルパス。

  ::: tip 重要なお知らせ

  KerberosキータブファイルはすべてのEMQXノードで同じパスに配置し、EMQXサービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sinkを使ったルールの作成

このセクションでは、MQTTトピック`t/#`からのメッセージを処理し、Kafka Sinkを通じてKafkaの`testtopic-in`トピックに送信するルールの作成方法を示します。

1. EMQXダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. **SQL Editor**に以下のステートメントを入力します。これはトピック`t/#`のMQTTメッセージをKafkaに転送する例です。

   注意: 独自のSQLを指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`に含めてください。

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

5. **Create Rule**ページで + **Add Action** をクリックし、ルールの出力アクションを定義します。

6. **Type of Action**ドロップダウンから`Kafka Producer`を選択します。

   **Action**ドロップダウンはデフォルトの`Create Action`のままにします。

   > 既存のSinkを選択することも可能ですが、この例では新規作成します。

7. **Name**と任意で**Description**を入力します。

8. **Connector**ドロップダウンから先ほど作成した`my-kafka`コネクターを選択します。必要に応じて新規作成も可能です。[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

9. Sinkのデータ送信方法を設定します。

      - **Kafka Topic**: メッセージをパブリッシュするKafkaトピック。`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。[変数テンプレートの使用](#use-variable-templates)を参照してください。
      - **Kafka Headers**: Kafkaメッセージに付加する任意のキー・バリューメタデータ。ヘッダー値はオブジェクトとして解決される必要があります。**Kafka Header Value Encode Type**ドロップダウンでエンコード方法を選択し、**Add**で複数ヘッダーを追加可能です。
      - **Message Key**: Kafkaメッセージのキー。パーティション分散やメッセージ順序付けに使用。静的文字列または`${.clientid}`などのプレースホルダーを含めることができます。
      - **Message Value**: Kafkaメッセージのペイロード。テンプレートからレンダリングされます。静的文字列または`${.}`のようなプレースホルダーを使い、ルールコンテキストから動的に生成可能です。テンプレートが`NULL`（例：参照フィールドが存在しない場合）を返した場合、空文字列ではなくKafkaの`NULL`値が生成されます。
      - **Message Timestamp**: Kafkaメッセージのタイムスタンプ。固定値または`${timestamp}`のようなプレースホルダーで動的に設定可能です。
      - **Partition Strategy**: プロデューサーがKafkaパーティションにメッセージを分配する方法を選択します。
      - **Partitions Limit**: プロデューサーがメッセージを送信できる最大パーティション数を制限します。有効にすると、すべてのパーティションではなく指定数のパーティション間でのみメッセージを分配します。
      - **Compression**: Kafkaメッセージのレコード圧縮・解凍に使用する圧縮アルゴリズムを指定します。

10. **フォールバックアクション**（任意）: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **高度な設定**（任意）: [高度な設定](#advanced-configuration)を参照してください。

12. **Create**をクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新規Sinkがルールアクションに追加されます。

13. **Create**をクリックしてルール作成を完了します。

![kafka_producer_bridge](./assets/kafka_producer_bridge.png)

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブに新規KafkaプロデューサーSinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認でき、トピック`t/#`のメッセージがルール`my_rule`で解析されKafkaに送信・保存される様子を直感的に把握できます。

### Kafkaの動的トピック設定

EMQX v5.7.2以降、KafkaプロデューサーSink設定で環境変数や変数テンプレートを使いKafkaトピックを動的に設定できます。このセクションでは2つのユースケースを紹介します。

#### 環境変数の利用

EMQX v5.7.2は、ルールSQL処理中に[環境変数](../../guides/configuration/configuration.md#environment-variables)の値を動的に割り当てる機能を追加しました。これはルールエンジンの組み込みSQL関数[getenv](./rule-sql-builtin-functions.md#system-function)を使い、EMQXの環境変数を取得し、SQL処理結果に設定します。この機能を応用し、Kafka SinkルールアクションでKafkaトピック設定にルール出力結果のフィールドを参照できます。以下はその例です。

::: tip 注意

ルールエンジンが読み取る環境変数名は、他のシステム環境変数の漏洩を防ぐため、必ず`EMQXVAR_`という固定プレフィックスを付ける必要があります。例えば`getenv('KAFKA_TOPIC')`で読み取る変数名が`KAFKA_TOPIC`の場合、環境変数名は`EMQXVAR_KAFKA_TOPIC`に設定してください。

:::

1. Kafkaを起動し、`testtopic-in`トピックを事前作成します。[はじめる前に](#はじめる前に)を参照。

2. EMQXを起動し環境変数を設定します。zip版インストールの場合、起動時に直接環境変数を指定可能です。例としてKafkaトピック`testtopic-in`を環境変数`EMQXVAR_KAFKA_TOPIC`に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照。

4. Kafka Sinkルールを設定し、**SQL Editor**に以下を入力します。

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

5. SQLテストを有効化し、環境変数`testtopic-in`が正常に取得できることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. KafkaプロデューサーSinkにアクションを追加します。ルールの右側**Action Outputs**で**Add Action**をクリック。

   - **Connector**: 先ほど作成したコネクター`test-kafka`を選択。
   - **Kafka Topic**: SQLルール出力の変数テンプレート`${kafka_topic}`形式で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sinkを使ったルールの作成](#kafka-sinkを使ったルールの作成)を参照して追加設定を完了し、最後に**Create**をクリックしてルール作成を完了します。

8. [Kafkaプロデューサールールのテスト](#test-kafka-producer-rule)の手順に従い、Kafkaにメッセージを送信します。

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -i pub -t t/Connection -q 1 -m 'payload string'
   ```

   Kafkaトピック`testtopic-in`でメッセージを受信できるはずです。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 \
     --topic testtopic-in

   {"payload":"payload string","kafka_topic":"testtopic-in"}
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   ```

#### 変数テンプレートの利用

**Kafka Topic**フィールドに静的なトピック名を設定する代わりに、変数テンプレートを使って動的にトピックを生成できます。これによりメッセージ内容に基づきKafkaトピックを構築し、柔軟なメッセージ処理・振り分けが可能です。例えば`device-${payload.device}`のように指定すると、特定デバイスからのメッセージを`device-1`などのデバイスID付きトピックに簡単に送信できます。

この例では、Kafkaに送信するメッセージのペイロードに`device`キーが含まれている必要があります。例:

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

`device`キーがない場合、トピックのレンダリングに失敗し、メッセージが復旧不能な形でドロップされます。

また、Kafkaには`device-1`、`device-2`など、解決されるすべてのトピックを事前作成しておく必要があります。存在しないトピック名に解決された場合もメッセージはドロップされます。

## Kafkaプロデューサールールのテスト

Kafkaプロデューサールールが期待通りに動作するか、[MQTTX](https://mqttx.app/en)を使ってMQTTメッセージをEMQXにパブリッシュするクライアントをシミュレートしてテストできます。

1. MQTTXでトピック`t/1`にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)**ページでSink名をクリックし統計情報を確認します。Sinkの稼働状況に新規の受信メッセージ数と送信メッセージ数が1件ずつ増えているはずです。

3. 以下のコマンドで`testtopic-in`トピックにメッセージが書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafkaコンシューマーコネクターの作成

Kafka Sourceアクションを追加する前に、EMQXとKafka間の接続を確立するKafkaコンシューマーコネクターを作成する必要があります。

1. EMQXダッシュボードで **Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector**ページで **Kafka Consumer** を選択し、**Next**をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例：`my-kafka-source`。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**: `127.0.0.1:9092`を入力します。EMQXとKafkaをローカルで実行している前提です。リモート環境の場合は適宜調整してください。

   - **Authentication**: Kafkaクラスターの認証方式を選択します。以下をサポートしています。

     - `None`: 認証なし。
     - `AWS IAM for MSK`: EC2上のEMQXからAmazon MSKに接続する場合。
     - `MSK IAM Roles Anywhere`: EC2外の環境からAmazon MSKに接続する場合。
     - `OAuth`: [OAuth 2.0](https://oauth.net/2/)認証。
     - `Basic Auth`: **Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）と**Username**、**Password**を指定。
     - `Kerberos`: **Kerberos Principal**と**Kerberos Keytab File**を指定。

     詳細は[認証方式](#authentication-method)を参照。

   - 暗号化接続を確立する場合は**Enable TLS**をオンにします。詳細は**TLS for External Resource Access**を参照。

   - **Advanced Settings**（任意）: [高度な設定](#advanced-configuration)を参照。

6. **Create**をクリックする前に、**Test Connection**でKafkaサーバーへの接続を確認できます。

11. **Create**をクリックします。関連するルールの作成オプションが表示されます。[KafkaコンシューマーSourceを使ったルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## KafkaコンシューマーSourceを使ったルールの作成

このセクションでは、KafkaコンシューマーSourceで転送されたメッセージをEMQXでさらに処理し、MQTTトピックに再パブリッシュするルールの作成方法を示します。

### ルールSQLの作成

1. EMQXダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. Kafkaソース`$bridges/kafka_consumer:<sourceName>`から変換されたメッセージをEMQXに転送する場合、**SQL Editor**に以下を入力します。

   注意: 独自SQLを指定する場合、後続の再パブリッシュアクションで必要なすべてのフィールドを`SELECT`に含めてください。Kafka Sourceの`SELECT`文では`ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node`などのフィールドが利用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   注意: 初心者は**SQL Examples**や**Enable Test**をクリックしてSQLルールを学習・テストできます。

### KafkaコンシューマーSourceをデータ入力に追加

1. ルール作成ページ右側の**Data Inputs**タブを選択し、**Add Input**をクリックします。

2. **Input Type**ドロップダウンから**Kafka Consumer**を選択します。**Source**ドロップダウンはデフォルトの`Create Source`のままか、既存のKafka Consumerソースを選択可能です。この例では新規作成してルールに追加します。

3. ソースの名前と説明を入力します。

4. **Connector**ドロップダウンから先ほど作成した`my-kafka-consumer`コネクターを選択します。隣のボタンから新規コネクター作成も可能です。[Kafkaコンシューマーコネクターの作成](#create-a-kafka-consumer-connector)を参照してください。

5. 以下のフィールドを設定します。

   - **Kafka Topic**: コンシューマーソースが購読するKafkaトピック。
   - **Group ID**: このソースのコンシューマーグループ識別子。未指定の場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode**、**Value Encoding Mode**: Kafkaメッセージのキーと値のエンコードモードを選択。

7. **Offset Reset Policy**: コンシューマーがKafkaトピックパーティションのどこから読み始めるかのポリシー。

   - `latest`: コンシューマー開始時点の最新オフセットから読み、過去のメッセージはスキップ。
   - `earliest`: パーティションの先頭から読み、過去のメッセージもすべて読み取る。

8. **Advanced Settings**（任意）: [高度な設定](#advanced-configuration)を参照。

9. **Create**をクリックする前に、**Test Connectivity**でKafkaサーバーへの接続を確認できます。

10. **Create**をクリックしてソース作成を完了します。ルール作成ページの**Data Inputs**タブに新規ソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs**タブを選択し、+ **Add Action**をクリックしてルールがトリガーするアクションを定義します。

2. **Type of Action**ドロップダウンから**Republish**を選択します。

3. **Topic**および**Payload**フィールドに再パブリッシュするメッセージのトピックとペイロードを入力します。例として` t/1`と`${.}`を入力します。

   - **Topic**フィールドには`${}`を使い動的にMQTTトピックを指定可能です。例：`t/${key}`（`${}`内のパラメータはSQLの`SELECT`文に含める必要があります）。

4. **Add**をクリックしてアクションをルールに追加します。

5. ルール作成ページに戻り、**Save**をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Sourceルールのテスト

Kafkaソースとルールが期待通りに動作するか、[MQTTX](https://mqttx.app/)を使ってEMQXのトピックをサブスクライブするクライアントをシミュレートし、KafkaプロデューサーでKafkaトピックにデータを生成してテストできます。EMQXがKafkaのデータをクライアントがサブスクライブするトピックに再パブリッシュするか確認します。

1. MQTTXでトピック`t/1`をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインを開き、以下のコマンドでKafkaプロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力待ちになります。

3. `{"msg": "Hello EMQX"}`を入力し、`testtopic-out`トピックにメッセージを生成します。

4. MQTTXのサブスクリプションで、Kafkaからの以下のメッセージがトピック`t/1`で受信されることを確認します。

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

## 高度な設定

このセクションでは、データ統合のパフォーマンス最適化や特定シナリオに応じたカスタマイズに役立つ高度な設定オプションを説明します。コネクター、Sink、Source作成時に**Advanced Settings**を展開し、ビジネス要件に応じて以下の設定を行えます。

| フィールド名                             | 説明                                                         | 推奨値             |
| --------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation               | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータ取得要求時に存在しないKafkaトピックを自動作成します。 | `disabled`         |
| Min Metadata Refresh Interval           | Kafkaブローカーやトピックのメタデータ更新間隔の最小時間。小さすぎるとKafkaサーバーに不要な負荷がかかる可能性があります。 | `3`秒              |
| Metadata Request Timeout                | Kafkaからメタデータを要求する際の最大待機時間。                   | `5`秒              |
| Connect Timeout                         | TCP接続確立の最大待機時間。認証時間も含みます。                   | `5`秒              |
| Max Wait Time (Source)                  | Kafkaブローカーからのフェッチ応答を待つ最大時間。                   | `1`秒              |
| Fetch Bytes (Source)                    | 1回のフェッチ要求でKafkaから取得するバイト数。設定値がメッセージサイズ未満だとフェッチ性能に悪影響を与える可能性があります。 | `896` KB           |
| Max Batch Bytes (Sink)                  | Kafkaバッチ内で収集可能なメッセージの最大バイト数。Kafkaブローカーのデフォルトは1MBですが、EMQXはエンコードオーバーヘッドを考慮しやや小さめに設定。単一メッセージが上限を超える場合は別バッチで送信されます。 | `896` KB           |
| Offset Commit Interval (Source)         | コンシューマーグループごとにオフセットコミット要求を送る間隔。       | `5`秒              |
| Required Acks (Sink)                    | Kafkaパーティションリーダーがフォロワーから待つ必要があるアックの種類。<br />`all_isr`: 全インシンクレプリカからのアックを要求。<br />`leader_only`: パーティションリーダーのみからのアックを要求。<br />`none`: Kafkaからのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加検知後、指定の`partition_strategy`に基づき新パーティションにメッセージを分配。 | `60`秒             |
| Max Inflight (Sink)                     | Kafkaプロデューサーがアック受信前に送信可能な最大バッチ数（パーティション単位）。値が大きいほどスループット向上。ただし1より大きいとメッセージの順序入れ替わりリスクあり。 | `10`               |
| Query Mode (Source)                     | 非同期または同期クエリモードを選択し、メッセージ伝送を最適化。非同期モードではKafka書き込みがMQTTパブリッシュ処理をブロックしませんが、クライアントがKafka到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)        | 同期モード時の最大待機時間。メッセージ伝送完了を保証し長時間待機を防止。同期モード時のみ有効。 | `5`秒              |
| Buffer Mode (Sink)                      | メッセージ送信前のバッファリング方法。メモリバッファリングは送信速度向上に寄与。<br />`memory`: メモリにバッファ。EMQXノード再起動でメッセージは失われる。<br />`disk`: ディスクにバッファ。再起動後もメッセージ保持。<br />`hybrid`: 初期はメモリバッファ。一定容量超過時に順次ディスクにオフロード。メモリモード同様、再起動でメッセージは失われる。 | `memory`           |
| Per-partition Buffer Limit (Sink)       | Kafkaパーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ領域を確保。メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)               | バッファモードが`disk`または`hybrid`の場合に適用。メッセージ保存用分割ファイルのサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)       | バッファモードが`memory`の場合に適用。メモリ圧迫時に古いメッセージを自動破棄し、システム安定性を確保。Linuxのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size       | ソケットバッファサイズを管理しネットワーク伝送性能を最適化。           | `1024` KB          |
| TCP Keepalive                           | Kafkaブリッジ接続のTCPキープアライブ設定。長時間の非アクティブ状態による接続切断を防止。`Idle, Interval, Probes`の3つの数値をカンマ区切りで指定。例: `240,30,5`は240秒アイドル後にキープアライブ開始、30秒間隔で最大5回プローブ送信。 | `none`             |
| Max Batch Age (Sink)                    | プロデューサーバッファ内のメッセージが送信されずに保持可能な最大時間。超過するとメッセージは破棄され、`dropped.expired`メトリクスにカウント。デフォルトは`infinity`で期限切れなし。バッファオーバーフロー時は期限切れに関わらず破棄される可能性あり。 | `infinity`         |
| Max Retries (Sink)                      | Kafkaがリトライ可能なエラー（例：パーティションリーダー変更）を返した際の最大リトライ回数。初回試行とリトライがすべて失敗するとバッチは破棄され、`failed`メトリクスにカウント。接続喪失による再送はリトライ回数にカウントされず、`max_batch_age`で制限。デフォルトは無制限。 | `infinity`         |
| Reconnect Delay (Sink)                  | 接続喪失後、プロデューサーがKafkaに再接続を試みるまでの遅延時間。切断中もメッセージはバッファに蓄積されるがバッファ制限や`max_batch_age`の影響を受ける。デフォルトは2秒。 | `2`秒              |
| Max Linger Time                         | パーティションごとのプロデューサーがより大きなバッチを作るために待機する最大時間。すべてのバッファモードに適用。デフォルト0は待機なしでレイテンシ最適化。多少の遅延を許容できる場合は設定するとリクエスト数削減に寄与。ディスクバッファ時はバッチ書き込み前の待機時間。最低5ms推奨。 | `0`ミリ秒          |
| Max Linger Bytes                        | パーティションごとのプロデューサーがバッチ送信前に蓄積する最大バイト数。 | `10` MB            |
| Health Check Interval                   | コネクターの稼働状態をチェックする間隔。                           | `15`秒             |

## さらに詳しく

EMQXはApache Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [MQTTとKafkaでつなぐコネクテッドビークルのストリーミングデータパイプライン：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka：IoTデータ統合のパワーアップ](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [EMQX Cloudルールエンジンを使ったデバイスデータのKafkaブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloudルールエンジンに関する動画で、将来的により適切な動画に差し替え予定）
