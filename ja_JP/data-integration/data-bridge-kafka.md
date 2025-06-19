# Apache KafkaへのMQTTデータストリーミング

[Apache Kafka](https://kafka.apache.org/)は、アプリケーションやシステム間でのリアルタイムなデータストリーム転送を処理できる、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、KafkaはエッジIoT通信向けに設計されておらず、Kafkaクライアントは安定したネットワーク接続とより多くのハードウェアリソースを必要とします。IoTの領域では、デバイスやアプリケーションから生成されるデータは軽量なMQTTプロトコルを用いて送信されます。EMQXのKafkaとの統合により、ユーザーはMQTTデータをKafkaにシームレスにストリーミングできます。MQTTのデータストリームはKafkaのトピックに取り込まれ、リアルタイムの処理、保存、分析が可能です。逆に、KafkaのトピックデータはMQTTデバイスによって消費され、タイムリーなアクションを実現します。

<img src="./assets/kafka_bridge.jpg" alt="Kafkaブリッジ" style="zoom:67%;" />

本ページでは、EMQXとKafka間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に解説します。

## 動作概要

Apache Kafkaとのデータ統合は、MQTTベースのIoTデータとKafkaの強力なデータ処理機能のギャップを埋めるためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、両プラットフォーム間のデータストリーミングと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は、自動車IoTで利用されるEMQXとKafka間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="Kafkaアーキテクチャ" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->
Apache Kafkaへのデータの流入および流出には、それぞれKafka Sink（Kafkaへメッセージを送信）とKafka Source（Kafkaからメッセージを受信）を作成する必要があります。ここではSinkを例に、その処理フローを説明します。

1. **メッセージのパブリッシュと受信**：接続された車載IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、定期的に状態データを含むメッセージをMQTTでパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：ブローカーと一体化した組み込みルールエンジンにより、これらのMQTTメッセージはトピックマッチングルールに基づいて処理されます。メッセージが到着するとルールエンジンを通過し、定義されたルールが評価されます。ペイロード変換を指定するルールがあれば、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **Kafkaへのブリッジ**：ルールエンジンで定義されたルールがトリガーされると、メッセージはKafkaに転送されます。Kafkaブリッジ機能を用いて、MQTTトピックは事前定義されたKafkaトピックにマッピングされ、処理済みのすべてのメッセージとデータがKafkaトピックに書き込まれます。

車両データがKafkaに取り込まれた後は、以下のように柔軟にデータへアクセスし活用できます。

- サービスはKafkaクライアントと直接連携し、特定トピックからリアルタイムのデータストリームを消費してカスタマイズされたビジネス処理を実行可能です。
- Kafka Streamsを利用してストリーム処理を行い、車両状態をメモリ上で集約・相関させてリアルタイム監視を実現します。
- Kafka Connectコンポーネントを活用し、MySQLやElasticSearchなど外部システムへのデータ出力用コネクターを選択して保存できます。

## 特長とメリット

Apache Kafkaとのデータ統合は、以下の特長とメリットをビジネスにもたらします。

- **信頼性の高い双方向IoTデータメッセージング**：不安定なモバイルネットワーク上で動作するリソース制約のあるIoTデバイスとKafka間のデータ通信は、不確実なネットワークでのメッセージングに優れたMQTTプロトコルで処理されます。EMQXはMQTTメッセージをバッチでKafkaに転送するだけでなく、バックエンドシステムからのKafkaメッセージをサブスクライブして接続されたIoTクライアントに配信します。
- **ペイロード変換**：メッセージのペイロードは転送中に定義されたSQLルールで処理可能です。例えば、総メッセージ数、成功/失敗配信数、メッセージレートなどのリアルタイムメトリクスを含むペイロードは、Kafkaに取り込む前にデータ抽出、フィルタリング、強化、変換を経ることができます。
- **効果的なトピックマッピング**：多数のIoTビジネストピックは設定されたKafka統合によりKafkaトピックにマッピングされます。EMQXはMQTTユーザープロパティのKafkaヘッダーへのマッピングをサポートし、1対1、1対多、多対多の柔軟なトピックマッピング方法を採用し、MQTTトピックフィルター（ワイルドカード）にも対応しています。
- **柔軟なパーティション選択戦略**：MQTTトピックやクライアントに基づいて同じKafkaパーティションにメッセージを転送することをサポートします。
- **高スループット環境での処理能力**：EMQX Kafkaプロデューサーは同期・非同期の両書き込みモードをサポートし、リアルタイム優先とパフォーマンス優先のデータ書き込み戦略を区別可能で、シナリオに応じてレイテンシとスループットの柔軟なバランス調整を実現します。
- **ランタイムメトリクス**：各SinkおよびSourceの総メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスの閲覧をサポートします。
- **動的設定**：Dashboardまたは設定ファイルでSinkおよびSourceを動的に設定可能です。

これらの機能は統合能力と柔軟性を高め、効果的で堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続のもとで送信され、さらに効果的に保存・管理されます。

## はじめる前に

このセクションでは、EMQX DashboardでKafka SinkおよびSourceを作成する前に必要な準備について説明します。

### 前提条件

- EMQXのデータ統合[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Kafkaサーバーのセットアップ

ここではmacOSを例にインストールと起動手順を示します。以下のコマンドでKafkaをインストールし起動できます。

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# KRaftモードでKafkaを起動
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

詳細な手順は[Kafka公式ドキュメントのクイックスタート](https://kafka.apache.org/documentation/#quickstart)を参照してください。

### Kafkaトピックの作成

EMQXでのデータ統合作成前に、Kafkaトピックを作成しておく必要があります。以下のコマンドでSink用の`testtopic-in`とSource用の`testtopic-out`の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafkaプロデューサーコネクターの作成

Kafka Sinkアクションを追加する前に、EMQXとKafka間の接続を確立するためのKafkaプロデューサーコネクターを作成します。

1. EMQX Dashboardで**Integration** -> **Connector**をクリックします。

2. ページ右上の**Create**をクリックし、コネクター選択画面で**Kafka Producer**を選択して**Next**をクリックします。

3. 名前と説明を入力します。例：`my-kafka`。名前はKafka Sinkとコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. Kafka接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**：`127.0.0.1:9092`を入力します。デモではEMQXとKafkaをローカルで実行している想定です。リモート環境の場合は適宜調整してください。

   - **Authentication**：Kafkaクラスターで必要な認証方式を選択します。以下の方式をサポートしています。

     - `None`：認証なし。
     - `AWS IAM for MSK`：EMQXがEC2インスタンス上で動作し、AWS MSKクラスターを利用する場合。
     - `Basic Auth`：**mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`のいずれか）を選択し、**username**と**password**を入力。
     - `Kerberos`：**Kerberos Principal**と**Kerberos Keytab file**を指定。

     詳細は[認証方式](#authentication-method)を参照してください。

   - その他のオプションはデフォルトのままか、ビジネス要件に応じて設定してください。

   - 暗号化接続を確立する場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照してください。

5. **Create**をクリックする前に、**Test Connection**をクリックしてKafkaサーバーへの接続が成功するか確認できます。

6. **Create**をクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にKafkaに接続します。次に、このコネクターを基にルールを作成し、Kafkaクラスターへのデータ転送を設定します。

### 認証方式

EMQXでKafkaコネクターを作成する際、Kafkaクラスターのセキュリティ設定に応じて以下の認証方式を選択できます。

- **None**：認証不要。

- **MSK IAM**：EMQXがAmazon EC2インスタンス上で動作し、Amazon MSKクラスターに接続する場合に使用。

  AWS EC2インスタンスのメタデータサービスを利用し、IAMポリシーに基づく認証トークンを生成します。

  ::: tip 重要

  MSK IAM認証は、EMQXがEC2インスタンス上で動作しMSKクラスターに接続する場合のみサポートされます。AWS Metadata APIに依存しているためです。

  :::

- **Basic Auth**：ユーザー名とパスワードによる認証。

  選択時は以下を指定する必要があります。
  - **Mechanism**：`plain`、`scram_sha_256`、`scram_sha_512`のいずれかを選択。
  - **Username**と**Password**：Kafkaクラスター認証用の資格情報。

- **Kerberos**：Kerberos GSSAPIによる認証。

  以下を指定する必要があります。
  - **Kerberos Principal**：認証に使用するKerberosのプリンシパル。
  - **Kerberos Keytab File**：非対話認証用のkeytabファイルのパス。

  ::: tip 重要

  KerberosのkeytabファイルはすべてのEMQXノードで同じパスに配置し、EMQXサービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sinkを用いたルールの作成

このセクションでは、MQTTトピック`t/#`からのメッセージを処理し、Kafka Sinkを使ってKafkaの`testtopic-in`トピックに送信するルールの作成方法を示します。

1. EMQX Dashboardで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDを入力します。例：`my_rule`。

4. **SQL Editor**に以下のステートメントを入力します。これはトピック`t/#`のMQTTメッセージをKafkaに転送する例です。

   注意：独自のSQL構文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`句に含めてください。

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

5. + **Add Action**ボタンをクリックしてルールでトリガーされるアクションを定義します。**Type of Action**ドロップダウンから`Kafka Producer`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のKafka Producerアクションを選択します。ここでは新規作成してルールに追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-kafka`コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクター作成も可能です。設定パラメータは[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

8. Sinkのデータ送信方法を設定します。

   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートしています。詳細は[変数テンプレートの使用](#use-variable-templates)を参照してください。

   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**ドロップダウンから選択可能です。**Add**をクリックしてキー・バリューのペアを追加できます。

   - **Message Key**：Kafkaメッセージのキー。純粋な文字列または`${var}`を含む文字列を入力可能です。

   - **Message Value**：Kafkaメッセージの値。純粋な文字列または`${var}`を含む文字列を入力可能です。

   - **Partition Strategy**：プロデューサーがKafkaパーティションにメッセージを分配する方法を選択します。

   - **Compression**：Kafkaメッセージのレコード圧縮/解凍に使用する圧縮アルゴリズムを指定します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#advanced-configurations)を参照してください。

11. **Create**をクリックしてSinkの作成を完了します。作成後は**Create Rule**画面に戻り、新しいSinkがルールアクションに追加されます。

12. **Create**をクリックしてルール作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブには新規KafkaプロデューサーSinkが表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認できます。トポロジーでは、トピック`t/#`のメッセージがルール`my_rule`で解析されKafkaに送信・保存される様子が直感的に把握できます。

![Kafka_producer_bridge](./assets/Kafka_producer_bridge.png)

### Kafkaの動的トピック設定

EMQX v5.7.2以降、Kafka Producer Sink設定で環境変数や変数テンプレートを用いてKafkaトピックを動的に設定可能です。本節ではこれら2つの動的トピック設定のユースケースを紹介します。

#### 環境変数の利用

EMQX v5.7.2では、ルールSQL処理中に[環境変数](../configuration/configuration.md#environment-variables)から取得した値をメッセージ内のフィールドに動的に割り当てる機能が追加されました。この機能はルールエンジンの組み込みSQL関数[getenv](../data-integration/rule-sql-builtin-functions.md#system-function)を利用し、EMQXの環境変数を取得してSQL処理結果にセットします。この機能を応用し、Kafka SinkルールアクションのKafkaトピック設定でルール出力結果のフィールドを参照してKafkaトピックを設定できます。以下はその例です。

::: tip 注意

ルールエンジンが読み取る環境変数名は、他のシステム環境変数の漏洩を防ぐために固定プレフィックス`EMQXVAR_`を付ける必要があります。例えば、`getenv`関数で読み取る変数名が`KAFKA_TOPIC`の場合、環境変数名は`EMQXVAR_KAFKA_TOPIC`と設定してください。

:::

1. Kafkaを起動し、`testtopic-in`トピックを事前作成します。[はじめる前に](#はじめる前に)の手順を参照してください。

2. EMQXを起動し環境変数を設定します。zip版インストールの場合、起動時に直接環境変数を指定可能です。例としてKafkaトピック`testtopic-in`を環境変数`EMQXVAR_KAFKA_TOPIC`に設定します。

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

5. SQLテストを有効にし、環境変数値`testtopic-in`が正しく取得できることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka Producer Sinkのアクションを追加します。ルール右側の**Action Outputs**で**Add Action**をクリックします。

   - **Connector**：先ほど作成した`test-kafka`を選択。
   - **Kafka Topic**：SQLルール出力に基づき変数テンプレート`${kafka_topic}`形式で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sinkを用いたルールの作成](#kafka-sinkを用いたルールの作成)を参照して残りの設定を完了し、最後に**Create**をクリックしてルール作成を完了します。

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

**Kafka Topic**フィールドに静的なトピック名を設定する代わりに、変数テンプレートを用いて動的にトピックを生成することも可能です。これによりメッセージ内容に基づいてKafkaトピックを構築でき、柔軟なメッセージ処理と振り分けが実現します。例えば、`device-${payload.device}`のように指定すると、特定デバイスからのメッセージを`device-1`などデバイスIDを付加したトピックに簡単に送信できます。

この例では、Kafkaに送信するメッセージのペイロードに`device`キーが含まれている必要があります。例：

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

このキーがないとトピックのレンダリングに失敗し、メッセージは回復不能なドロップとなります。

また、Kafka側で解決されるすべてのトピック（例：`device-1`、`device-2`など）を事前に作成しておく必要があります。存在しないトピック名に解決された場合もメッセージは回復不能なエラーでドロップされます。

## Kafkaプロデューサールールのテスト

Kafkaプロデューサールールが期待通りに動作するかをテストするには、[MQTTX](https://mqttx.app/en)を使ってEMQXにMQTTメッセージをパブリッシュするクライアントをシミュレートします。

1. MQTTXでトピック`t/1`にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)**ページでSink名をクリックし統計情報を確認します。Sinkの稼働状況をチェックし、新規受信メッセージ数と送信メッセージ数が1件ずつ増えていることを確認します。

3. 以下のコマンドでメッセージが`testtopic-in`トピックに書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafkaコンシューマーコネクターの作成

Kafka Sourceアクションを追加する前に、EMQXとKafka間の接続を確立するKafkaコンシューマーコネクターを作成します。

1. EMQX Dashboardで**Integration** -> **Connector**をクリックします。

2. ページ右上の**Create**をクリックします。

3. **Create Connector**ページで**Kafka Consumer**を選択し、**Next**をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで例：`my-kafka-source`。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**：`127.0.0.1:9092`を入力します。デモではEMQXとKafkaをローカルで実行している想定です。リモート環境の場合は適宜調整してください。
   - **Authentication**：Kafkaクラスターで必要な認証方式を選択します。以下をサポートしています。

     - `None`：認証なし。
     - `authentication_msk_iam`：EMQXがEC2インスタンス上で動作し、AWS MSKクラスターを利用する場合。
     - `Basic Auth`：**Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`のいずれか）を選択し、**Username**と**Password**を入力。
     - `Kerberos`：**Kerberos Principal**と**Kerberos Keytab File**を指定。

     詳細は[認証方式](#authentication-method)を参照してください。
   - その他のオプションはデフォルトのままか、ビジネス要件に応じて設定してください。
   - 暗号化接続を確立する場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照してください。

6. 詳細設定（任意）：[詳細設定](#advanced-configurations)を参照してください。

7. **Create**をクリックする前に、**Test Connection**をクリックしてKafkaサーバーへの接続が成功するか確認できます。

8. **Create**をクリックしてソースの作成を完了します。関連ルールの作成オプションが表示されます。[KafkaコンシューマーSourceを用いたルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## KafkaコンシューマーSourceを用いたルールの作成

このセクションでは、KafkaコンシューマーSourceで転送されたメッセージをさらに処理し、MQTTトピックに再パブリッシュするルールの作成方法を示します。

### ルールSQLの作成

1. EMQX Dashboardで**Integration** -> **Rules**をクリックします。

2. ページ右上の**Create**をクリックします。

3. ルールIDを入力します。例：`my_rule`。

4. Kafkaソース`$bridges/kafka_consumer:<sourceName>`から変換されたメッセージをEMQXに転送する場合、**SQL Editor**に以下のステートメントを入力します。

   注意：独自のSQL構文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドを`SELECT`句に含めてください。Kafka Sourceの`SELECT`文では、`ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node`などのフィールドが使用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   初心者の方は**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習とテストが可能です。

### KafkaコンシューマーSourceをデータ入力に追加

1. ルール作成画面右側の**Data Inputs**タブを選択し、**Add Input**をクリックします。

2. **Input Type**ドロップダウンから**Kafka Consumer**を選択します。**Source**はデフォルトの`Create Source`のままか、既存のKafka Consumerソースを選択します。ここでは新規作成してルールに追加します。

3. ソースの名前と説明を対応するテキストボックスに入力します。

4. **Connector**ドロップダウンから先ほど作成した`my-kafka-consumer`コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクター作成も可能です。設定パラメータは[Kafkaコンシューマーコネクターの作成](#kafkaコンシューマーコネクターの作成)を参照してください。

5. 以下のフィールドを設定します。

   - **Kafka Topic**：コンシューマーソースがサブスクライブするKafkaトピックを指定します。
   - **Group ID**：このソースのコンシューマーグループIDを指定します。未指定の場合はソース名に基づいて自動生成されます。
   - **Key Encoding Mode**および**Value Encoding Mode**：Kafkaメッセージのキーと値のエンコードモードを選択します。

6. **Offset Reset Policy**：Kafkaコンシューマーがオフセットを持たないか無効な場合に読み取り開始位置を決定するポリシーを選択します。

   - `latest`を選択すると、コンシューマーは最新のオフセットから読み始め、開始前のメッセージはスキップします。
   - `earliest`を選択すると、コンシューマーはパーティションの先頭から読み始め、開始前のメッセージも含めてすべての履歴データを読み取ります。

7. 詳細設定（任意）：[詳細設定](#advanced-configurations)を参照してください。

8. **Create**をクリックする前に、**Test Connectivity**をクリックしてKafkaサーバーへの接続が成功するか確認できます。

9. **Create**をクリックしてソースの作成を完了します。ルール作成画面の**Data Inputs**タブに新しいソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs**タブを選択し、+ **Add Action**ボタンをクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action**ドロップダウンから**Republish**を選択します。

3. **Topic**および**Payload**フィールドに再パブリッシュするメッセージのトピックとペイロードを入力します。例として`Topic`に`t/1`、`Payload`に`${.}`を入力します。
   - `${}`を用いてMQTTトピックを動的指定することも可能です。例：`t/${key}`（`${}`内のパラメータはSQLの`SELECT`文に含まれている必要があります）。

4. **Add**をクリックしてアクションをルールに追加します。

5. ルール作成画面に戻り、**Save**をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Sourceルールのテスト

Kafka Sourceとルールが期待通りに動作するかをテストするには、[MQTTX](https://mqttx.app/)を使ってEMQXのトピックをサブスクライブするクライアントをシミュレートし、KafkaプロデューサーでKafkaトピックにデータを生成します。その後、KafkaからのデータがEMQXによってクライアントがサブスクライブするトピックに再パブリッシュされているか確認します。

1. MQTTXでトピック`t/1`をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドでKafkaプロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力待ちになります。

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

## 詳細設定

このセクションでは、データ統合のパフォーマンス最適化やシナリオに応じたカスタマイズを可能にする詳細設定オプションを説明します。コネクター、Sink、Source作成時に**Advanced Settings**を展開し、ビジネス要件に応じて以下の設定を行えます。

| フィールド名                              | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Min Metadata Refresh Interval             | クライアントがKafkaブローカーおよびトピックのメタデータを更新する最小間隔。小さすぎるとKafkaサーバーの負荷が増加する可能性があります。 | `3`秒              |
| Metadata Request Timeout                  | Kafkaからメタデータを要求する際の最大待機時間。               | `5`秒              |
| Connect Timeout                           | TCP接続確立の最大待機時間。認証有効時は認証時間も含みます。   | `5`秒              |
| Max Wait Time (Source)                    | Kafkaブローカーからのフェッチ応答を待つ最大時間。              | `1`秒              |
| Fetch Bytes (Source)                      | Kafkaから1回のフェッチで取得するバイト数。設定値がメッセージサイズより小さいとフェッチ性能に悪影響があります。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafkaバッチ内で収集するメッセージの最大バイト数。Kafkaブローカーのデフォルトは1MBですが、EMQXはメッセージエンコードのオーバーヘッドを考慮しやや小さめに設定。単一メッセージが制限を超える場合は別バッチで送信。 | `896` KB           |
| Offset Commit Interval (Source)           | コンシューマーグループごとに送信するオフセットコミット要求の間隔。 | `5`秒              |
| Required Acks (Sink)                      | Kafkaパーティションリーダーがフォロワーから待つアックの種類：<br />`all_isr`：全てのインシンクレプリカからのアックを要求。<br />`leader_only`：リーダーのみからのアックを要求。<br />`none`：Kafkaからのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加検知後、指定の`partition_strategy`に基づき新パーティションをメッセージ送信に組み込みます。 | `60`秒             |
| Max Inflight (Sink)                       | Kafkaプロデューサーがアック受信前に送信可能な最大バッチ数（パーティションごと）。大きいほどスループット向上。ただし1より大きいとメッセージ順序が入れ替わるリスクあり。 | `10`               |
| Query Mode (Source)                       | 非同期または同期クエリモードを選択し、メッセージ送信を最適化。非同期モードではKafka書き込みがMQTTパブリッシュをブロックしませんが、クライアントがKafka到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の最大待機時間。メッセージ送信完了をタイムリーに保証し、長時間待機を防止。同期モード設定時のみ有効。 | `5`秒              |
| Buffer Mode (Sink)                        | メッセージ送信前のバッファリング方式。メモリバッファリングは送信速度向上に寄与。<br />`memory`：メモリにバッファ。EMQXノード再起動時にメッセージは失われる。<br />`disk`：ディスクにバッファ。再起動後もメッセージ保持。<br />`hybrid`：初めはメモリバッファ。一定サイズ超過時に徐々にディスクにオフロード。メモリモード同様、再起動時はメッセージ失われる。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafkaパーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ領域を確保。メモリ使用量と性能のバランス調整に有効。 | `2`GB              |
| Segment File Bytes (Sink)                 | バッファモードが`disk`または`hybrid`の場合に適用。メッセージ保存用のセグメントファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100`MB            |
| Memory Overload Protection (Sink)         | バッファモードが`memory`の場合に適用。メモリ圧迫時に古いバッファメッセージを自動破棄し、システムの安定性を確保。Linuxのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ネットワーク送受信のソケットバッファサイズを管理し、通信性能を最適化。 | `1024`KB           |
| TCP Keepalive                             | Kafkaブリッジ接続のTCPキープアライブを有効化し、長時間のアイドルによる接続切断を防止。値は`Idle, Interval, Probes`の3つの数値をカンマ区切りで指定。<br />Idle：サーバーがキープアライブプローブを送るまでのアイドル秒数（Linuxデフォルト7200秒）。<br />Interval：プローブ間隔秒数（Linuxデフォルト75秒）。<br />Probes：応答なしと判断するまでの最大プローブ回数（Linuxデフォルト9回）。<br />例：`240,30,5`は240秒のアイドル後にプローブ開始、30秒間隔で最大5回送信。 | `none`             |
| Max Linger Time                           | パーティションごとのプロデューサーがバッチ収集のためにメッセージを待つ最大時間。デフォルト`0`は待機なし。メモリバッファ以外では`5ms`がIOPS削減に効果的だがレイテンシ増加のトレードオフあり。 | `0`ミリ秒          |
| Max Linger Bytes                          | パーティションごとのプロデューサーがバッチ収集のために待つ最大バイト数。 | `10`MB             |
| Health Check Interval                     | コネクターの稼働状況チェック間隔。                             | `15`秒             |

## さらに詳しく

EMQXはApache Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ：**

- [MQTTとKafkaによるコネクテッドビークルのストリーミングデータパイプライン構築：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka：IoTデータ統合の強化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画：**

- [EMQX Cloudルールエンジンを使ったデバイスデータのKafkaブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（本動画はCloudルールエンジンに関するもので、将来的により適切な動画に差し替え予定）
