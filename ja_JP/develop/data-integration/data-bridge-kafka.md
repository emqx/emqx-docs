# Apache KafkaへMQTTデータをストリームする

[Apache Kafka](https://kafka.apache.org/)は、アプリケーションやシステム間のデータストリームをリアルタイムで転送できる、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、KafkaはエッジIoT通信向けに設計されておらず、Kafkaクライアントは安定したネットワーク接続とより多くのハードウェアリソースを必要とします。IoTの領域では、デバイスやアプリケーションから生成されるデータは軽量なMQTTプロトコルを用いて送信されます。EMQXのKafkaとの統合により、ユーザーはMQTTデータをKafkaへ、またはKafkaからシームレスにストリームできます。MQTTのデータストリームはKafkaのトピックに取り込まれ、リアルタイムの処理、保存、分析が可能です。逆に、KafkaのトピックデータはMQTTデバイスによって消費され、タイムリーなアクションを実現します。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQXとKafka間のデータ統合について包括的に紹介し、データ統合の作成と検証方法を実践的に解説します。

## 動作の仕組み

Apache Kafkaとのデータ統合は、MQTTベースのIoTデータとKafkaの強力なデータ処理機能の橋渡しを目的としたEMQXの標準機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、両プラットフォーム間のデータストリーミングと処理が簡素化され、複雑なコーディングを不要にします。

以下の図は、自動車IoTで用いられるEMQXとKafka間の典型的なデータ統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

Apache Kafkaへデータを流入・流出させるには、それぞれKafka Sink（Kafkaへメッセージを送信）とKafka Source（Kafkaからメッセージを受信）を作成します。Sinkを例にとると、処理の流れは以下の通りです。

1. **メッセージのパブリッシュと受信**：接続された車載IoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、定期的に状態データを含むメッセージをMQTTでパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：ブローカーと一体化した組み込みのルールエンジンにより、MQTTメッセージはトピックマッチングルールに基づいて処理されます。メッセージ到着時にルールエンジンが定義済みのルールを評価し、ペイロードの変換が指定されていれば、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの拡充などが適用されます。
3. **Kafkaへのブリッジ**：ルールエンジンで定義されたルールがメッセージをKafkaへ転送するアクションをトリガーします。Kafkaブリッジ機能を用いて、MQTTトピックは事前定義されたKafkaトピックにマッピングされ、処理済みのメッセージとデータがKafkaトピックに書き込まれます。

車両データがKafkaに取り込まれた後は、以下のように柔軟にデータを活用できます。

- サービスはKafkaクライアントと直接統合し、特定トピックからリアルタイムのデータストリームを消費してカスタマイズされたビジネス処理を実現可能です。
- Kafka Streamsを利用してストリーム処理を行い、車両状態をメモリ内で集約・相関付けてリアルタイム監視が可能です。
- Kafka Connectコンポーネントを使用し、MySQLやElasticSearchなどの外部システムへデータを出力して保存できます。

## 特徴と利点

Apache Kafkaとのデータ統合は、以下の特徴と利点をビジネスにもたらします。

- **信頼性の高い双方向IoTデータメッセージング**：Kafkaと不安定なモバイルネットワーク上で動作するリソース制約のあるIoTデバイス間のデータ通信は、不確実なネットワークでのメッセージングに優れたMQTTプロトコルで処理されます。EMQXはMQTTメッセージをバッチでKafkaへ転送するだけでなく、バックエンドシステムからのKafkaメッセージをサブスクライブし、接続されたIoTクライアントへ配信します。
- **ペイロード変換**：メッセージのペイロードは送信中に定義されたSQLルールで処理可能です。例えば、総メッセージ数、成功/失敗配信数、メッセージレートなどのリアルタイムメトリクスを含むペイロードは、Kafkaに取り込まれる前にデータ抽出、フィルタリング、拡充、変換を経ることができます。
- **効果的なトピックマッピング**：多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティのKafkaヘッダーへのマッピングをサポートし、1対1、1対多、多対多の柔軟なトピックマッピング方式を採用、MQTTトピックフィルター（ワイルドカード）にも対応しています。
- **柔軟なパーティション選択戦略**：MQTTトピックやクライアントに基づいて同一Kafkaパーティションへメッセージを転送することをサポートします。
- **高スループット状況での処理能力**：EMQX Kafkaプロデューサーは同期・非同期の両書き込みモードに対応し、リアルタイム優先とパフォーマンス優先のデータ書き込み戦略を区別可能で、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。
- **ランタイムメトリクス**：各SinkおよびSourceの合計メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスの閲覧をサポートします。
- **動的設定**：Dashboardまたは設定ファイルからSinkおよびSourceを動的に設定可能です。

これらの特徴により、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築が促進されます。増加するIoTデータは安定したネットワーク接続のもとで送信され、さらに効果的に保存・管理できます。

## はじめる前に

このセクションでは、EMQX DashboardでKafka SinkおよびSourceを作成する前に必要な準備について説明します。

### 前提条件

- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Kafkaサーバーのセットアップ

ここではmacOSを例にインストールと起動手順を示します。以下のコマンドでKafkaをインストール・起動できます。

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# KRaftモードでKafkaを起動
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

詳細な操作手順は、[Kafkaドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/)を参照してください。

### Kafkaトピックの作成

EMQXでデータ統合を作成する前に、関連するKafkaトピックを作成してください。以下のコマンドでKafkaに2つのトピック `testtopic-in`（Sink用）と `testtopic-out`（Source用）を作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafkaプロデューサーコネクターの作成

Kafka Sinkアクションを追加する前に、EMQXとKafka間の接続を確立するKafkaプロデューサーコネクターを作成する必要があります。

1. EMQX Dashboardで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例：`my-kafka`。名前はKafka Sinkとコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. Kafka接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**：`127.0.0.1:9092` と入力します。デモではEMQXとKafkaをローカルマシンで起動している前提です。リモート環境の場合は適宜調整してください。

   - **Authentication**：Kafkaクラスターの認証方式を選択します。以下の方式をサポートしています。

     - `None`：認証なし。
     - `AWS IAM for MSK`：EMQXがEC2インスタンス上にデプロイされている場合のAWS MSKクラスター用。
     - `Basic Auth`：**mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**username**と**password**を入力。
     - `Kerberos`：**Kerberos Principal**と**Kerberos Keytabファイル**を指定。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。

   - **Request Timeout**：Kafkaからの応答待ち時間を秒単位で指定します。デフォルトは30秒です。タイムアウト超過時は接続が古くなったとみなし再接続します。値が小さすぎると、Kafkaがリクエストを受け入れても応答を遅延させる場合があり、EMQXが再接続後にバッチを再送し、重複メッセージや過剰な下流データ量を引き起こす可能性があります。

   - **Advanced Settings**（任意）：[詳細設定](#advanced-configuration)を参照してください。

5. **Create**をクリックする前に、**Test Connection**でKafkaサーバーへの接続テストが可能です。

6. **Create**をクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にKafkaへ接続します。次に、このコネクターを基にルールを作成し、Kafkaクラスターへデータを転送します。

### 認証方式

EMQXでKafkaコネクターを作成する際、Kafkaクラスターのセキュリティ設定に応じて以下の認証方式から選択できます。

- **None**：認証不要。

- **MSK IAM**：EMQXがAmazon EC2インスタンス上にデプロイされている場合のAmazon MSKクラスター接続用。

  AWS EC2インスタンスのメタデータサービスを利用し、インスタンスに付与されたIAMポリシーに基づく認証トークンを生成します。

  ::: tip 重要

  MSK IAM認証は、EMQXがEC2インスタンス上で稼働しMSKクラスターに接続する場合のみサポートされます。EC2インスタンスメタデータサービスに依存しているためです。

  `iptables`や`nftables`によるホストレベルのアウトバウンドフィルタリングを行う場合、`169.254.169.254`へのアクセスをブロックしないでください。EMQXはMSK IAM認証のためにインスタンスメタデータサービスへアクセスする必要があります。同様の例外は、S3、S3 Tables、DynamoDB、KinesisなどEC2インスタンスメタデータから認証情報を取得する他のAWSベースのコネクターにも適用されます。[ルールエンジンポリシーとファイアウォールルールによるSSRF緩和](../../guides/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。

  :::

- **Basic Auth**：ユーザー名とパスワードによる認証。

  選択時は以下を指定します。
  - **Mechanism**：`plain`、`scram_sha_256`、`scram_sha_512`のいずれか。
  - **Username**と**Password**：Kafkaクラスター認証用資格情報。

- **Kerberos**：Kerberos GSSAPI認証。

  必須項目：
  - **Kerberos Principal**：認証に使用するKerberosプリンシパル。
  - **Kerberos Keytab File**：非対話認証に用いるキータブファイルのパス。

  ::: tip 重要

  KerberosキータブファイルはすべてのEMQXノードで同一パスに配置し、EMQXサービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sink付きルールの作成

このセクションでは、MQTTトピック `t/#` からのメッセージを処理し、Kafkaの `testtopic-in` トピックへKafka Sinkを使って送信するルール作成方法を示します。

1. EMQX Dashboardで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. **SQL Editor**に以下のステートメントを入力します。これはトピック `t/#` のMQTTメッセージをKafkaへ転送する例です。

   注意：独自のSQL文を指定する場合は、Sinkが必要とするすべてのフィールドを`SELECT`句に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は、**SQL Examples**や**Enable Test**をクリックしてSQLルールを学習・テストできます。

   :::

   ::: tip

   EMQX v5.7.2からルールSQLで環境変数を読み取る機能が追加されました。詳細は[ルールSQLで環境変数を使う](#use-environment-variables)を参照してください。

   :::

5. + **Add Action** ボタンをクリックし、トリガーされるアクションを定義します。**Type of Action**ドロップダウンから `Kafka Producer` を選択し、**Action**はデフォルトの `Create Action` のままか、既存のKafka Producerアクションを選択します。この例では新規プロデューサーアクションを作成しルールに追加します。

6. Sinkの名前と説明を入力します。

7. **Connector**ドロップダウンから先ほど作成した `my-kafka` コネクターを選択します。隣のボタンからポップアップで新規コネクター作成も可能です。設定パラメータは[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

8. Sinkのデータ送信方法を設定します。

   - **Kafka Topic**：`testtopic-in` と入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。[変数テンプレートの使用](#use-variable-templates)を参照してください。

   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは **Kafka Header Value Encod Type** ドロップダウンから選択可能です。**Add**をクリックしてキー・バリューのペアを追加できます。

   - **Message Key**：Kafkaメッセージのキー。純粋な文字列またはプレースホルダー（`${var}`）を含む文字列を入力可能です。

   - **Message Value**：Kafkaメッセージの値。純粋な文字列またはプレースホルダーを含む文字列を入力可能です。

   - **Partition Strategy**：プロデューサーがKafkaパーティションへメッセージを分配する方法を選択します。

   - **Compression**：Kafkaメッセージのレコード圧縮/解凍に使用する圧縮アルゴリズムを指定します。

9. **フォールバックアクション**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定**（任意）：[詳細設定](#advanced-configuration)を参照してください。

11. **Create**をクリックしてSinkの作成を完了します。作成後は**Create Rule**画面に戻り、新規Sinkがルールアクションに追加されます。

12. **Create**をクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules** ページで新規ルールを確認でき、**Actions(Sink)** タブで新規Kafka Producer Sinkも確認できます。

また、**Integration** -> **Flow Designer** でトポロジーを確認可能です。トポロジーから、トピック `t/#` のメッセージがルール `my_rule` によって解析されKafkaへ送信・保存されていることが直感的に把握できます。

![Kafka_producer_bridge](./assets/Kafka_producer_bridge.png)

### Kafka動的トピックの設定

EMQX v5.7.2以降、Kafka Producer Sinkの設定で環境変数や変数テンプレートを用いてKafkaトピックを動的に設定できます。本節ではこれら2つの動的トピック設定のユースケースを紹介します。

#### 環境変数の使用

EMQX v5.7.2は、ルールSQL処理中に[環境変数](../../guides/configuration/configuration.md#environment-variables)から取得した値をメッセージ内のフィールドに動的に割り当てる機能を追加しました。この機能はルールエンジンの組み込みSQL関数[getenv](./rule-sql-builtin-functions.md#system-function)を用いてEMQXの環境変数を取得し、その値をSQL処理結果に設定します。この機能の応用例として、Kafka SinkルールアクションのKafkaトピック設定でルール出力結果のフィールドを参照してKafkaトピックを設定できます。以下はその例です。

::: tip 注意

ルールエンジンが使用する環境変数名は、他のシステム環境変数の漏洩を防ぐために固定接頭辞 `EMQXVAR_` を付ける必要があります。例えば、`getenv`関数で読み取る変数名が `KAFKA_TOPIC` の場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` と設定してください。

:::

1. Kafkaを起動し、`testtopic-in` トピックを事前作成します。[はじめる前に](#はじめる前に)の手順を参照してください。

2. EMQXを起動し環境変数を設定します。zipインストールの場合は起動時に直接環境変数を指定可能です。例としてKafkaトピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

4. Kafka Sinkルールを設定します。**SQL Editor**に以下のステートメントを入力します。

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

5. SQLテストを有効にし、環境変数値 `testtopic-in` が正常に取得できることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka Producer Sinkにアクションを追加します。ルール右側の**Action Outputs**で**Add Action**をクリックします。

   - **Connector**：先ほど作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**：SQLルール出力に基づき変数テンプレート形式 `${kafka_topic}` で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sink付きルールの作成](#kafka-sink付きルールの作成)を参照して追加設定を行い、最後に**Create**をクリックしてルール作成を完了します。

8. [Kafkaプロデューサールールのテスト](#test-kafka-producer-rule)の手順に従い、Kafkaへメッセージを送信します。

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -i pub -t t/Connection -q 1 -m 'payload string'
   ```

   メッセージはKafkaトピック `testtopic-in` で受信されるはずです。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 \
     --topic testtopic-in

   {"payload":"payload string","kafka_topic":"testtopic-in"}
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   ```

#### 変数テンプレートの使用

**Kafka Topic**フィールドに静的なトピック名を設定する代わりに、変数テンプレートを用いて動的トピックを生成できます。これによりメッセージ内容に基づくKafkaトピックの構築が可能となり、柔軟なメッセージ処理と振り分けが実現します。例えば、`device-${payload.device}` のように指定することで、特定デバイスからのメッセージをデバイスIDをサフィックスに持つトピック（例：`device-1`）へ簡単に送信できます。

この例では、Kafkaへ送信されるメッセージペイロードに `device` キーが含まれている必要があります。以下は例のペイロードです。

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

このキーが存在しない場合、トピックのレンダリングに失敗し、メッセージが回復不能な形でドロップされます。

また、Kafkaに事前に解決済みのすべてのトピック（例：`device-1`、`device-2`など）を作成しておく必要があります。テンプレートがKafkaに存在しないトピック名に解決されると、メッセージは回復不能なエラーでドロップされます。

## Kafkaプロデューサールールのテスト

Kafkaプロデューサールールが期待通りに動作するかをテストするために、[MQTTX](https://mqttx.app/en)を使ってEMQXへMQTTメッセージをパブリッシュするクライアントをシミュレートできます。

1. MQTTXでトピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページでSink名をクリックし統計情報を確認します。Sinkの稼働状況を確認し、新しい受信メッセージと送信メッセージが1件ずつあることを確認してください。

3. 以下のコマンドでメッセージが `testtopic-in` トピックに書き込まれているかを確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

## Kafkaコンシューマーコネクターの作成

Kafka Sourceアクションを追加する前に、EMQXとKafka間の接続を確立するKafkaコンシューマーコネクターを作成する必要があります。

1. EMQX Dashboardで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例：`my-kafka-source`。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**：`127.0.0.1:9092` と入力します。デモはローカルマシンでEMQXとKafkaを起動している前提です。リモート環境の場合は適宜調整してください。

   - **Authentication**：Kafkaクラスターの認証方式を選択します。以下をサポートしています。

     - `None`：認証なし。
     - `authentication_msk_iam`：EMQXがEC2インスタンス上にデプロイされている場合のAWS MSKクラスター用。
     - `Basic Auth`：**Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**Username**と**Password**を入力。
     - `Kerberos`：**Kerberos Principal**と**Kerberos Keytab File**を指定。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。

   - **Advanced Settings**（任意）：[詳細設定](#advanced-configuration)を参照してください。

6. **Create**をクリックする前に、**Test Connection**でKafkaサーバーへの接続テストが可能です。

7. **Create**をクリックします。関連するルールの作成オプションが表示されます。[Kafka Consumer Source付きルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## Kafka Consumer Source付きルールの作成

このセクションでは、設定済みKafka Consumerソースから転送されたメッセージをさらに処理し、MQTTトピックへ再パブリッシュするルールの作成方法を示します。

### ルールSQLの作成

1. EMQX Dashboardで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. Kafkaソース `$bridges/kafka_consumer:<sourceName>` から変換されたメッセージをEMQXへ転送する場合、**SQL Editor**に以下のステートメントを入力します。

   注意：独自のSQL文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドを`SELECT`句に含めてください。Kafka Sourceの`SELECT`文では、`ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node`などのフィールドが利用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   初心者の方は、**SQL Examples**や**Enable Test**をクリックしてSQLルールを学習・テストできます。

### Kafka Consumer Sourceをデータ入力として追加

1. ルール作成画面右側の**Data Inputs**タブを選択し、**Add Input**をクリックします。

2. **Input Type**ドロップダウンから **Kafka Consumer** を選択します。**Source**ドロップダウンはデフォルトの `Create Source` のままか、既存のKafka Consumerソースを選択します。この例では新規ソースを作成しルールに追加します。

3. ソースの名前と説明を入力します。

4. **Connector**ドロップダウンから先ほど作成した `my-kafka-consumer` コネクターを選択します。隣のボタンからポップアップで新規コネクター作成も可能です。設定パラメータは[Kafkaコンシューマーコネクターの作成](#kafkaコンシューマーコネクターの作成)を参照してください。

5. 以下のフィールドを設定します。

   - **Kafka Topic**：コンシューマーソースがメッセージを受信するKafkaトピックを指定します。
   - **Group ID**：このソースのコンシューマーグループ識別子を指定します。未指定の場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode**および**Value Encoding Mode**：Kafkaメッセージのキーと値のエンコードモードを選択します。

6. **Offset Reset Policy**：Kafkaコンシューマーがオフセットを持たないか無効な場合に、どの位置から読み始めるかのポリシーを選択します。

   - `latest`：最新のオフセットから読み始め、開始前のメッセージはスキップします。
   - `earliest`：パーティションの先頭から読み始め、開始前のメッセージも含めてすべての履歴データを読みます。

7. **Advanced Settings**（任意）：[詳細設定](#advanced-configuration)を参照してください。

8. **Test Connectivity**でKafkaサーバーへの接続テストが可能です。

9. **Create**をクリックしてソース作成を完了します。ルール作成画面の**Data Inputs**タブに新規ソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs**タブを選択し、+ **Add Action** ボタンをクリックしてルールトリガーアクションを定義します。

2. **Type of Action**ドロップダウンから **Republish** を選択します。

3. **Topic**と**Payload**フィールドに再パブリッシュするメッセージのトピックとペイロードを入力します。例として、`t/1` と `${.}` を入力します。
   - **Topic**フィールドには`${}`を用いて動的にMQTTトピックを指定可能です。例：`t/${key}` （`${}`内のパラメータはSQLの`SELECT`句に含まれている必要があります）。

4. **Add**をクリックしてアクションをルールに追加します。

5. ルール作成画面に戻り、**Save**をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Sourceルールのテスト

Kafkaソースとルールが期待通りに動作するかをテストするために、[MQTTX](https://mqttx.app/)を使ってEMQXのトピックをサブスクライブするクライアントをシミュレートし、KafkaプロデューサーでKafkaトピックにデータを生成します。その後、KafkaからのデータがEMQXによってクライアントがサブスクライブするトピックへ再パブリッシュされるかを確認します。

1. MQTTXでトピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドでKafkaプロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力を促されます。

3. `{"msg": "Hello EMQX"}` と入力し、`testtopic-out` トピックへメッセージを生成してEnterを押します。

4. MQTTXのサブスクリプションで、以下のKafkaからのメッセージがトピック `t/1` で受信されることを確認します。

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

このセクションでは、データ統合のパフォーマンス最適化やシナリオに応じたカスタマイズに役立つ詳細設定オプションを説明します。コネクター、Sink、Source作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                                      | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                 | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータ取得要求を送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `disabled`         |
| Min Metadata Refresh Interval             | クライアントがKafkaブローカーやトピックのメタデータを更新する最小間隔。小さすぎるとKafkaサーバーの負荷が増加します。 | `3` 秒             |
| Metadata Request Timeout                  | ブリッジがKafkaからメタデータを要求する際の最大待機時間。 | `5` 秒             |
| Connect Timeout                           | TCP接続確立の最大待機時間。認証時間も含みます。 | `5` 秒             |
| Max Wait Time (Source)                    | Kafkaブローカーからのフェッチ応答を待つ最大時間。 | `1` 秒             |
| Fetch Bytes (Source)                      | Kafkaから1回のフェッチで取得するバイト数。設定値がKafka内のメッセージサイズより小さいとフェッチ性能に悪影響を与える可能性があります。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafkaバッチ内で収集するメッセージの最大サイズ（バイト）。Kafkaブローカーのデフォルトは1MBですが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮し1MB未満に設定しています。単一メッセージがこの制限を超える場合は別バッチとして送信されます。 | `896` KB           |
| Offset Commit Interval (Source)           | 各コンシューマーグループに対してオフセットコミット要求を送る間隔。 | `5` 秒             |
| Required Acks (Sink)                      | Kafkaパーティションリーダーがフォロワーから待つ必要があるアック数：<br />`all_isr`：全てのインシンクレプリカからのアックを要求。<br />`leader_only`：リーダーのみからのアックを要求。<br />`none`：Kafkaからのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加検知後、EMQXは`partition_strategy`に基づき新パーティションへメッセージを分配します。 | `60` 秒            |
| Max Inflight (Sink)                       | Kafkaプロデューサーがアックを受け取る前に送信可能な最大バッチ数（パーティション毎）。値が大きいほどスループットは向上しますが、1より大きい場合はメッセージの順序入れ替わりリスクがあります。 | `10`               |
| Query Mode (Source)                       | 非同期または同期クエリモードを選択し、メッセージ送信を最適化します。非同期モードではKafkaへの書き込みがMQTTメッセージパブリッシュをブロックしませんが、クライアントがKafka到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の最大待機時間。メッセージ送信の完了を保証し、長時間待機を防ぎます。同期モード時のみ有効。 | `5` 秒             |
| Buffer Mode (Sink)                        | メッセージ送信前にバッファリングするかどうかを定義。<br />`memory`：メモリにバッファ。EMQXノード再起動時にメッセージは失われます。<br />`disk`：ディスクにバッファ。ノード再起動後もメッセージは保持されます。<br />`hybrid`：最初はメモリにバッファし、一定量を超えるとディスクにオフロード。メモリモード同様ノード再起動時にメッセージは失われます。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafkaパーティション毎の最大バッファサイズ（バイト）。制限超過時は古いメッセージを破棄してバッファ領域を確保します。メモリ使用量とパフォーマンスのバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが`disk`または`hybrid`時に適用。メッセージ保存用セグメントファイルのサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが`memory`時に適用。メモリ圧迫時に古いバッファメッセージを自動破棄し、システムの安定性を維持。Linuxシステムのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ソケットバッファサイズを管理し、ネットワーク伝送性能を最適化。 | `1024` KB          |
| TCP Keepalive                             | Kafkaブリッジ接続のTCPキープアライブ設定。接続の有効性を維持し、長時間のアイドルによる切断を防止。値は `Idle, Interval, Probes` の3つの数値のカンマ区切りリスト。<br />Idle：接続がアイドル状態となってからキープアライブプローブを送信開始するまでの秒数（Linuxデフォルト7200秒）。<br />Interval：キープアライブプローブ間隔（Linuxデフォルト75秒）。<br />Probes：応答がない場合に送信する最大プローブ数（Linuxデフォルト9）。<br />例：`240,30,5,` は240秒のアイドル後にプローブ開始し、30秒間隔で最大5回送信。応答なければ接続切断と判断。 | `none`             |
| Max Batch Age (Sink)                      | プロデューサーバッファ内のメッセージが送信されずに保持可能な最大期間。バッチ内のすべてのメッセージがこの期間を超えるとバッチは破棄されます。切断時のバッファメッセージやアック待ちメッセージも含みます。破棄されたメッセージは`dropped.expired`メトリクスにカウントされます。デフォルトは無期限（`infinity`）でメッセージの期限切れを防止。バッファオーバーフロー時は破棄される場合があります。 | `infinity`         |
| Max Retries (Sink)                        | Kafkaからリトライ可能なエラー応答（例：パーティションリーダー変更）を受けた後の最大リトライ回数。初回試行と全リトライ失敗時はバッチ破棄され、メッセージは`failed`メトリクスにカウント。明示的なKafkaエラー応答のみリトライ回数を増加させ、接続喪失による再送は増加させず`max_batch_age`で制限。デフォルトは無制限（`infinity`）。 | `infinity`         |
| Reconnect Delay (Sink)                    | 接続喪失後にKafkaへ再接続を試みるまでの遅延時間。切断中もメッセージはバッファに蓄積され、バッファ制限と`max_batch_age`の影響を受けます。デフォルトは2秒。 | `2` 秒             |
| Max Linger Time                           | パーティション毎のプロデューサーがより大きなバッチを形成するために待機する最大時間。すべてのバッファモードに適用。デフォルト0は待機なしでレイテンシ最適化。多少の遅延を許容できる場合は設定することでKafkaへのリクエスト数を削減可能。ディスクバッファ時はバッチ書き込み前に待機するため、ディスクIOPS削減のため最低5ms以上推奨。 | `0` ミリ秒         |
| Max Linger Bytes                          | パーティション毎のプロデューサーがバッチ送信前に蓄積する最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状態をチェックする間隔。 | `15` 秒            |

## さらに詳しく

EMQXはApache Kafkaとのデータ統合に関する多くの学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ：**

- [MQTTとKafkaでつなぐコネクテッドビークルのストリーミングデータパイプライン：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka：IoTデータ統合の強化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画：**

- [EMQX Cloudルールエンジンを使ったデバイスデータのKafkaブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloudルールエンジンに関する動画であり、将来的により適切な動画に差し替え予定）
