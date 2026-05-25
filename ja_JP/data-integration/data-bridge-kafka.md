# Apache KafkaへのMQTTデータストリーミング

[Apache Kafka](https://kafka.apache.org/)は、アプリケーションやシステム間でのデータストリームのリアルタイム転送を処理できる、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、KafkaはエッジIoT通信向けに設計されておらず、Kafkaクライアントは安定したネットワーク接続とより多くのハードウェアリソースを必要とします。IoTの領域では、デバイスやアプリケーションから生成されるデータは軽量なMQTTプロトコルを用いて送信されます。EMQXのKafkaとの統合により、ユーザーはMQTTデータをKafkaにシームレスにストリーミングできます。MQTTデータストリームはKafkaのトピックに取り込まれ、リアルタイムの処理、保存、分析を可能にします。逆に、KafkaのトピックデータはMQTTデバイスによって消費され、タイムリーなアクションを実現します。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQXとKafka間のデータ統合について包括的に紹介し、データ統合の作成および検証方法について実践的な手順を提供します。

## 動作の仕組み

Apache Kafkaとのデータ統合は、MQTTベースのIoTデータとKafkaの強力なデータ処理能力のギャップを埋めるためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、両プラットフォーム間のデータストリーミングと処理のプロセスを簡素化し、複雑なコーディングを不要にします。

以下の図は、自動車IoTで使用されるEMQXとKafka間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

Apache Kafkaへのデータの流入および流出には、それぞれKafka Sink（Kafkaへメッセージを送信）とKafka Source（Kafkaからメッセージを受信）を作成する必要があります。ここではSinkを例に挙げ、その処理の流れを説明します。

1. **メッセージのパブリッシュと受信**：接続された車載IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的に状態データを含むメッセージをMQTTでパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：ブローカーと一体となって動作する組み込みのルールエンジンにより、これらのMQTTメッセージはトピックマッチングルールに基づいて処理されます。メッセージが到着するとルールエンジンを通過し、定義されたルールが評価されます。ペイロード変換を指定するルールがあれば、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **Kafkaへのブリッジング**：ルールエンジンで定義されたルールは、メッセージをKafkaに転送するアクションをトリガーします。Kafkaブリッジ機能を用いて、MQTTトピックは事前定義されたKafkaトピックにマッピングされ、処理済みのすべてのメッセージとデータがKafkaトピックに書き込まれます。

車両データがKafkaに取り込まれた後は、以下のように柔軟にデータを活用できます。

- サービスはKafkaクライアントと直接連携し、特定トピックからリアルタイムのデータストリームを消費してカスタマイズされたビジネス処理を行えます。
- Kafka Streamsを利用してストリーム処理を行い、車両状態をメモリ内で集約・相関させてリアルタイム監視を実現します。
- Kafka Connectコンポーネントを使用し、MySQLやElasticSearchなど外部システムへのデータ出力を行い、保存を行うことができます。

## 特長とメリット

Apache Kafkaとのデータ統合は、以下の特長とメリットをビジネスにもたらします。

- **信頼性の高い双方向IoTデータメッセージング**：不安定なモバイルネットワーク上で動作するリソース制限のあるIoTデバイスとKafka間のデータ通信は、不確実なネットワーク環境に強いMQTTプロトコルで処理されます。EMQXはMQTTメッセージをバッチでKafkaに転送するだけでなく、バックエンドシステムからのKafkaメッセージをサブスクライブし、接続されたIoTクライアントに配信します。
- **ペイロード変換**：メッセージペイロードは転送中に定義されたSQLルールで処理可能です。例えば、総メッセージ数、成功/失敗配信数、メッセージレートなどのリアルタイムメトリクスを含むペイロードは、Kafkaに取り込まれる前にデータ抽出、フィルタリング、強化、変換を経ることができます。
- **効果的なトピックマッピング**：多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティのKafkaヘッダーへのマッピングをサポートし、1対1、1対多、多対多の柔軟なトピックマッピング方法を採用し、MQTTトピックフィルター（ワイルドカード）もサポートします。
- **柔軟なパーティション選択戦略**：MQTTトピックやクライアントに基づいて同一Kafkaパーティションへメッセージを転送することをサポートします。
- **高スループット状況での処理能力**：EMQX Kafkaプロデューサーは同期・非同期の両書き込みモードをサポートし、リアルタイム優先と性能優先のデータ書き込み戦略を区別可能で、シナリオに応じてレイテンシとスループットの柔軟なバランス調整を可能にします。
- **ランタイムメトリクス**：各SinkおよびSourceの総メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスを閲覧可能です。
- **動的構成**：Dashboardまたは設定ファイルでSinkおよびSourceを動的に構成できます。

これらの特長は統合能力と柔軟性を高め、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続のもとで送信され、さらに効果的に保存・管理されます。

## はじめる前に

このセクションでは、EMQX DashboardでKafka SinkおよびSourceを作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Kafkaサーバーのセットアップ

ここではmacOSを例に、Kafkaのインストールと起動手順を示します。以下のコマンドでKafkaをインストール・起動できます。

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# KRaftモードでKafkaを起動
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

詳細な操作手順は、[Kafkaドキュメントのクイックスタート](https://kafka.apache.org/documentation/#quickstart)を参照してください。

### Kafkaトピックの作成

EMQXでデータ統合を作成する前に、関連するKafkaトピックを作成する必要があります。以下のコマンドでKafkaに2つのトピックを作成します：`testtopic-in`（Sink用）と`testtopic-out`（Source用）。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafkaプロデューサーコネクターの作成

Kafka Sinkアクションを追加する前に、EMQXとKafka間の接続を確立するためにKafkaプロデューサーコネクターを作成する必要があります。

1. EMQX Dashboardにアクセスし、**Integration** -> **Connector**をクリックします。

2. ページ右上の**Create**をクリックし、コネクター選択画面で**Kafka Producer**を選択して**Next**をクリックします。

3. 名前と説明を入力します。例：`my-kafka`。名前はKafka Sinkとコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. Kafka接続に必要なパラメータを設定します：
   - **Bootstrap Hosts**：`127.0.0.1:9092`を入力します。デモではEMQXとKafkaがローカルマシンで動作している前提です。リモート環境の場合は適宜調整してください。

   - **Authentication**：Kafkaクラスターで必要な認証方式を選択します。以下の方式をサポートしています：

     - `None`：認証なし。
     - `AWS IAM for MSK`：EMQXがEC2インスタンス上にデプロイされている場合のAWS MSKクラスター用。
     - `Basic Auth`：**mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**username**と**password**を入力。
     - `Kerberos`：**Kerberos Principal**と**Kerberos Keytabファイル**を指定。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立したい場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照してください。

   - **Advanced Settings**（任意）：[高度な設定](#advanced-configurations)を参照してください。

5. **Create**をクリックする前に、**Test Connection**を押してKafkaサーバーへの接続が成功するかテストできます。

6. **Create**をクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にKafkaに接続します。次に、このコネクターを基にルールを作成し、Kafkaクラスターへのデータ転送を行います。

### 認証方式

EMQXでKafkaコネクターを作成する際、Kafkaクラスターのセキュリティ設定に応じて以下の認証方式から選択可能です。

- **None**：認証不要。

- **MSK IAM**：EMQXがAmazon EC2インスタンス上にデプロイされている場合のAmazon MSKクラスター接続用。

  この方式はAWS EC2インスタンスメタデータサービスを利用し、インスタンスに付与されたIAMポリシーに基づく認証トークンを生成します。

  ::: tip 重要なお知らせ

  MSK IAM認証は、EMQXがEC2インスタンス上で稼働しMSKクラスターに接続する場合のみサポートされます。AWS Metadata APIに依存しているためです。

  :::

- **Basic Auth**：ユーザー名とパスワードによる認証。

  選択時は以下を指定する必要があります：
  - **Mechanism**：`plain`、`scram_sha_256`、`scram_sha_512`から選択。
  - **Username**と**Password**：Kafkaクラスター認証用の資格情報。

- **Kerberos**：Kerberos GSSAPIによる認証。

  必要な項目：
  - **Kerberos Principal**：認証に使用するKerberosのプリンシパル。
  - **Kerberos Keytabファイル**：非対話認証に使用するkeytabファイルのパス。

  ::: tip 重要なお知らせ

  Kerberos keytabファイルはすべてのEMQXノードで同一パスに配置し、EMQXサービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sinkを用いたルールの作成

このセクションでは、MQTTトピック`t/#`からのメッセージを処理し、Kafka Sinkを使ってKafkaの`testtopic-in`トピックに送信するルールの作成方法を示します。

1. EMQX Dashboardで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDを入力します。例：`my_rule`。

4. **SQL Editor**に以下のステートメントを入力します。これはトピック`t/#`のMQTTメッセージをKafkaに転送する例です。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`句に含めるようにしてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習とテストが可能です。

   :::

   ::: tip

   EMQX v5.7.2からルールSQLで環境変数を読み取る機能が追加されました。詳細は[ルールSQLで環境変数を使う](#use-environment-variables)を参照してください。

   :::

5. + **Add Action**ボタンをクリックし、トリガーされるアクションを定義します。**Type of Action**ドロップダウンリストから`Kafka Producer`を選択し、**Action**はデフォルトの`Create Action`のままにするか、既存のKafka Producerアクションを選択します。ここでは新規にProducerアクションを作成しルールに追加します。

6. Sinkの名前と説明を入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-kafka`コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。設定パラメータは[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

8. Sinkのデータ送信方法を設定します：

   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。詳細は[変数テンプレートの使用](#use-variable-templates)を参照してください。

   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**ドロップダウンから選択可能です。**Add**をクリックしてさらにキー・バリューを追加できます。

   - **Message Key**：Kafkaメッセージのキー。純粋な文字列または`${var}`形式のプレースホルダーを含む文字列を入力可能です。

   - **Message Value**：Kafkaメッセージの値。純粋な文字列または`${var}`形式のプレースホルダーを含む文字列を入力可能です。

   - **Partition Strategy**：プロデューサーがKafkaパーティションにメッセージを分配する方法を選択します。

   - **Compression**：Kafkaメッセージのレコードを圧縮/解凍するための圧縮アルゴリズムを指定します。

9. **フォールバックアクション**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **高度な設定**（任意）：[高度な設定](#advanced-configuration)を参照してください。

11. **Create**をクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新規Sinkがルールアクションに追加されます。

12. **Create**をクリックしてルール作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブで新規Kafka Producer Sinkも確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認できます。トポロジーを通じて、トピック`t/#`のメッセージがルール`my_rule`で解析されKafkaに送信・保存されていることを直感的に把握できます。

![Kafka_producer_bridge](./assets/Kafka_producer_bridge.png)

### Kafka動的トピックの設定

EMQX v5.7.2以降、Kafka Producer Sink設定で環境変数や変数テンプレートを用いてKafkaトピックを動的に設定可能です。本節ではこれら2つの動的トピック設定のユースケースを紹介します。

#### 環境変数の利用

EMQX v5.7.2では、ルールSQL処理段階で[環境変数](../configuration/configuration.md#environment-variables)から取得した値をメッセージ内のフィールドに動的に割り当てる機能が追加されました。この機能はルールエンジンの組み込みSQL関数[getenv](../data-integration/rule-sql-builtin-functions.md#system-function)を用いてEMQXの環境変数を取得し、その値をSQL処理結果に設定します。この機能の応用として、Kafka SinkルールアクションのKafkaトピック設定でルール出力結果のフィールドを参照してKafkaトピックを設定できます。以下はその例です。

::: tip 注意

システムの他の環境変数漏洩を防ぐため、ルールエンジンが使用する環境変数名は固定プレフィックス`EMQXVAR_`を付ける必要があります。例えば、`getenv`関数で読み取る変数名が`KAFKA_TOPIC`の場合、環境変数名は`EMQXVAR_KAFKA_TOPIC`に設定してください。

:::

1. Kafkaを起動し、`testtopic-in`トピックを事前作成します。[はじめる前に](#はじめる前に)の手順を参照してください。

2. EMQXを起動し、環境変数を設定します。zipインストールの場合は起動時に直接環境変数を指定可能です。例としてKafkaトピック`testtopic-in`を環境変数`EMQXVAR_KAFKA_TOPIC`に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

4. Kafka Sinkルールを設定します。**SQL Editor**に以下を入力します。

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

5. SQLテストを有効にし、環境変数値`testtopic-in`が正常に取得できることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka Producer Sinkにアクションを追加します。ルール右側の**Action Outputs**で**Add Action**をクリックします。

   - **Connector**：先に作成したコネクター`test-kafka`を選択。
   - **Kafka Topic**：SQLルール出力に基づき変数テンプレート形式`${kafka_topic}`で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sinkを用いたルールの作成](#create-a-rule-with-kafka-sink)を参照して追加設定を行い、最後に**Create**をクリックしてルール作成を完了します。

8. [Kafkaプロデューサールールのテスト](#test-kafka-producer-rule)の手順に従い、Kafkaにメッセージを送信します。

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

#### 変数テンプレートの利用

**Kafka Topic**フィールドに静的なトピック名を設定する代わりに、変数テンプレートを用いて動的にトピックを生成できます。これによりメッセージ内容に基づいてKafkaトピックを構築でき、柔軟なメッセージ処理と分配が可能になります。例えば、`device-${payload.device}`のような形式を指定すると、特定デバイスからのメッセージをデバイスIDをサフィックスとするトピック（例：`device-1`）に簡単に送信できます。

この例では、Kafkaに送信されるメッセージペイロードに`device`キーが含まれている必要があります。以下は例のペイロードです。

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

このキーが含まれない場合、トピックのレンダリングに失敗し、メッセージが回復不能な形で破棄されます。

また、Kafka側で`device-1`、`device-2`など、テンプレートで解決されるすべてのトピックを事前に作成しておく必要があります。存在しないトピック名に解決された場合も、メッセージは回復不能なエラーで破棄されます。

## Kafkaプロデューサールールのテスト

Kafkaプロデューサールールが期待通りに動作するかをテストするために、[MQTTX](https://mqttx.app/en)を使ってEMQXにMQTTメッセージをパブリッシュするクライアントをシミュレートできます。

1. MQTTXを使ってトピック`t/1`にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)**ページでSink名をクリックし統計情報を確認します。Sinkの稼働状況をチェックし、新規の受信メッセージと送信メッセージが1件ずつあることを確認します。

3. 以下のコマンドでメッセージが`testtopic-in`トピックに書き込まれているか確認します。

```bash
bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
```

## Kafkaコンシューマーコネクターの作成

Kafka Sourceアクションを追加する前に、EMQXとKafka間の接続を確立するKafkaコンシューマーコネクターを作成する必要があります。

1. EMQX Dashboardで**Integration** -> **Connector**をクリックします。

2. ページ右上の**Create**をクリックします。

3. **Create Connector**ページで**Kafka Consumer**を選択し、**Next**をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例：`my-kafka-source`。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**：`127.0.0.1:9092`を入力します。デモではローカルマシン上でEMQXとKafkaが動作している前提です。リモート環境の場合は適宜調整してください。
   
   - **Authentication**：Kafkaクラスターで必要な認証方式を選択します。以下の方式をサポートしています：
   
     - `None`：認証なし。
     - `authentication_msk_iam`：EMQXがEC2インスタンス上にデプロイされている場合のAWS MSKクラスター用。
     - `Basic Auth`：**Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**Username**と**Password**を入力。
     - `Kerberos`：**Kerberos Principal**と**Kerberos Keytab File**を指定。
   
     詳細は[認証方式](#authentication-method)を参照してください。
     
   - 暗号化接続を確立したい場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は**外部リソースアクセスのTLS**を参照してください。
   
   - **Advanced Settings**（任意）：[高度な設定](#advanced-configuration)を参照してください。
   
6. **Create**をクリックする前に、**Test Connection**を押してKafkaサーバーへの接続が成功するかテストできます。

7. **Create**をクリックします。関連するルール作成オプションが表示されます。[KafkaコンシューマーSourceを用いたルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## KafkaコンシューマーSourceを用いたルールの作成

このセクションでは、設定済みKafkaコンシューマーSourceから転送されたメッセージをさらに処理し、MQTTトピックに再パブリッシュするルールの作成方法を示します。

### ルールSQLの作成

1. EMQX Dashboardで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDを入力します。例：`my_rule`。

4. Kafka Source`$bridges/kafka_consumer:<sourceName>`から変換されたメッセージをEMQXに転送する場合、**SQL Editor**に以下を入力します。

   注意：独自のSQL構文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドを`SELECT`句に含めるようにしてください。Kafka Sourceの`SELECT`文では`ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node`などのフィールドが使用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   初心者の方は**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習とテストが可能です。

### KafkaコンシューマーSourceをデータ入力として追加

1. ルール作成ページ右側の**Data Inputs**タブを選択し、**Add Input**をクリックします。

2. **Input Type**ドロップダウンから**Kafka Consumer**を選択します。**Source**はデフォルトの`Create Source`のままにするか、既存のKafka Consumer Sourceを選択します。ここでは新規にConsumer Sourceを作成しルールに追加します。

3. Sourceの名前と説明を入力します。

4. **Connector**ドロップダウンから先ほど作成した`my-kafka-consumer`コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。設定パラメータは[Kafkaコンシューマーコネクターの作成](#create-a-kafka-consumer-connector)を参照してください。

5. 以下のフィールドを設定します：

   - **Kafka Topic**：コンシューマーSourceがサブスクライブするKafkaトピックを指定します。
   - **Group ID**：このSourceのコンシューマーグループ識別子を指定します。指定しない場合はSource名に基づき自動生成されます。
   - **Key Encoding Mode**および**Value Encoding Mode**：Kafkaメッセージのキーと値のエンコードモードを選択します。

6. **Offset Reset Policy**：Kafkaコンシューマーがオフセットを持たない場合や無効な場合に、Kafkaトピックパーティションのどこから読み始めるかを指定します。

   - `latest`を選択すると、コンシューマーは最新のオフセットから読み始め、開始前のメッセージはスキップします。
   - `earliest`を選択すると、コンシューマーはパーティションの先頭から読み始め、開始前のメッセージも含めてすべての履歴データを読みます。

7. **Advanced Settings**（任意）：[高度な設定](#advanced-configuration)を参照してください。

8. **Test Connectivity**をクリックしてSourceがKafkaサーバーに接続可能かテストします。

9. **Create**をクリックしてSource作成を完了します。**Create Rule**ページに戻ると、**Data Inputs**タブに新規Sourceが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs**タブを選択し、+ **Add Action**ボタンをクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action**ドロップダウンから**Republish**を選択します。

3. **Topic**および**Payload**フィールドに再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として、トピックに`t/1`、ペイロードに`${.}`を入力します。
   - **Topic**フィールドには`${}`を使って動的にMQTTトピックを指定可能です。例：`t/${key}`（`${}`内のパラメータはSQLの`SELECT`文に含まれている必要があります）。

4. **Add**をクリックしてアクションをルールに追加します。

5. **Create Rule**ページに戻り、**Save**をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Sourceルールのテスト

Kafka Sourceとルールが期待通りに動作するかテストするため、[MQTTX](https://mqttx.app/)を使ってEMQXのトピックをサブスクライブするクライアントをシミュレートし、KafkaプロデューサーでKafkaトピックにデータを生成します。その後、KafkaからのデータがEMQXによってクライアントがサブスクライブするトピックに再パブリッシュされるか確認します。

1. MQTTXでトピック`t/1`をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドでKafkaプロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力を促されます。

3. `{"msg": "Hello EMQX"}`を入力して`testtopic-out`トピックにメッセージを生成し、Enterキーを押します。

4. MQTTXのサブスクリプションを確認します。Kafkaからの以下のメッセージがトピック`t/1`で受信されるはずです。

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

このセクションでは、データ統合のパフォーマンス最適化や特定シナリオに応じたカスタマイズのための高度な設定オプションを説明します。コネクター、Sink、Source作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

| フィールド名                                | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                 | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータ取得リクエストを送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `disabled`         |
| Min Metadata Refresh Interval             | クライアントがKafkaブローカーおよびトピックのメタデータを更新する最小間隔。小さすぎるとKafkaサーバーの負荷が増加します。 | `3`秒              |
| Metadata Request Timeout                  | ブリッジがKafkaからメタデータを要求する際の最大待機時間。 | `5`秒              |
| Connect Timeout                           | TCP接続確立の最大待機時間。認証時間も含みます。 | `5`秒              |
| Max Wait Time (Source)                    | Kafkaブローカーからのフェッチ応答を待つ最大時間。 | `1`秒              |
| Fetch Bytes (Source)                      | Kafkaから1回のフェッチで取得するバイト数。設定値がKafka内のメッセージサイズより小さいとフェッチ性能に悪影響を与える可能性があります。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafkaバッチ内で収集するメッセージの最大サイズ（バイト）。Kafkaブローカーのデフォルトは1MBですが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮しやや小さめに設定しています。単一メッセージがこの制限を超える場合は別バッチとして送信されます。 | `896` KB           |
| Offset Commit Interval (Source)           | コンシューマーグループごとにオフセットコミットリクエストを送信する間隔。 | `5`秒              |
| Required Acks (Sink)                      | Kafkaパーティションリーダーがフォロワーから待つ必要のあるアックの種類：<br />`all_isr`：全てのインシンクレプリカからのアックを要求。<br />`leader_only`：リーダーのみからのアックを要求。<br />`none`：Kafkaからのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafkaプロデューサーがパーティション数の増加を検知する間隔。増加検知後、EMQXは`partition_strategy`に基づき新パーティションをメッセージ送信に利用します。 | `60`秒             |
| Max Inflight (Sink)                       | Kafkaプロデューサーがアック受信前に送信可能な最大バッチ数（パーティション毎）。値が大きいほどスループットは向上しますが、1より大きいとメッセージの順序入れ替わりリスクがあります。 | `10`               |
| Query Mode (Source)                       | 非同期または同期クエリモードを選択し、メッセージ伝送を最適化。非同期モードではKafka書き込みがMQTTパブリッシュをブロックしませんが、クライアントがKafka到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の最大待機時間。メッセージ伝送完了を確保し長時間待機を防止します。同期モード時のみ有効。 | `5`秒              |
| Buffer Mode (Sink)                        | メッセージ送信前のバッファリング方式。メモリバッファリングは伝送速度向上に寄与。<br />`memory`：メモリにバッファ。EMQXノード再起動時にメッセージは失われます。<br />`disk`：ディスクにバッファ。ノード再起動後もメッセージは保持されます。<br />`hybrid`：初期はメモリバッファ。一定容量超過時に段階的にディスクへオフロード。メモリモード同様、ノード再起動時にメッセージは失われます。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafkaパーティション毎の最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ空間を確保。メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが`disk`または`hybrid`の場合に適用。メッセージ保存用セグメントファイルのサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが`memory`の場合に適用。メモリ使用過多時に古いメッセージを自動破棄しシステム安定性を確保。Linuxシステムのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ソケットバッファサイズを管理しネットワーク伝送性能を最適化。 | `1024` KB          |
| TCP Keepalive                             | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間の非アクティブ状態による接続切断を防止。値は`Idle, Interval, Probes`の3つの数値をカンマ区切りで指定。<br />Idle：接続がアイドル状態になる秒数（Linuxデフォルト7200秒）。<br />Interval：キープアライブプローブ間隔秒数（Linuxデフォルト75秒）。<br />Probes：応答なしと判断するまでの最大プローブ回数（Linuxデフォルト9回）。<br />例：`240,30,5,`は240秒アイドル後にプローブ開始、30秒間隔で最大5回プローブ送信。 | `none`             |
| Max Linger Time                           | パーティション毎のプロデューサーがメッセージをバッチ収集のために待機する最大時間。デフォルト`0`は待機なし。メモリ以外のバッファモードでは`5ms`でIOPSを大幅削減可能だがレイテンシ増加のトレードオフあり。 | `0`ミリ秒          |
| Max Linger Bytes                          | パーティション毎のプロデューサーがメッセージをバッチ収集のために待機する最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状況チェック間隔。 | `15`秒             |

## さらに詳しく

EMQXはApache Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ：**

- [MQTTとKafkaによるコネクテッドビークルストリーミングデータパイプライン構築：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka：IoTデータ統合の強化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画：**

- [EMQX Cloudルールエンジンを使ったデバイスデータのKafkaブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloudルールエンジンに関する動画。将来的により適切な動画に差し替え予定）
