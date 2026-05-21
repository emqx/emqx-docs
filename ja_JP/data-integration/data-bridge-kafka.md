# Apache Kafka に MQTT データをストリームする

<<<<<<< HEAD
[Apache Kafka](https://kafka.apache.org/)は、アプリケーションやシステム間でのデータストリームのリアルタイム転送を処理できる、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、KafkaはエッジIoT通信向けに設計されておらず、Kafkaクライアントは安定したネットワーク接続とより多くのハードウェアリソースを必要とします。IoTの領域では、デバイスやアプリケーションから生成されるデータは軽量なMQTTプロトコルを用いて送信されます。EMQXのKafkaとの統合により、ユーザーはMQTTデータをKafkaへシームレスにストリーミングできます。MQTTのデータストリームはKafkaのトピックに取り込まれ、リアルタイムの処理、保存、分析が可能になります。逆に、KafkaのトピックデータはMQTTデバイスに配信され、タイムリーなアクションを実現します。
=======
[Apache Kafka](https://kafka.apache.org/)は、アプリケーションやシステム間でのリアルタイムデータストリーム転送を処理できる、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、KafkaはエッジIoT通信向けに設計されておらず、Kafkaクライアントは安定したネットワーク接続とより多くのハードウェアリソースを必要とします。IoTの分野では、デバイスやアプリケーションから生成されるデータは軽量なMQTTプロトコルを用いて送信されます。EMQXのKafkaとの統合により、ユーザーはMQTTデータをKafkaへ、またはKafkaからシームレスにストリーミングできます。MQTTのデータストリームはKafkaのトピックに取り込まれ、リアルタイム処理、保存、分析を実現します。逆に、KafkaトピックのデータはMQTTデバイスによって消費され、タイムリーなアクションを可能にします。
>>>>>>> origin/release-5.10

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQXとKafka間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に解説します。

## 動作概要

Apache Kafkaとのデータ統合は、MQTTベースのIoTデータとKafkaの強力なデータ処理機能のギャップを埋めるためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、両プラットフォーム間のデータストリーミングと処理が簡素化され、複雑なコーディングを不要にします。

<<<<<<< HEAD
以下の図は、自動車IoTで使われるEMQXとKafka間の典型的なデータ統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->
Apache Kafkaへのデータの流入および流出には、それぞれKafka Sink（Kafkaへメッセージを送信）とKafka Source（Kafkaからメッセージを受信）を作成する必要があります。ここではSinkを例に、処理の流れを説明します。

1. **メッセージのパブリッシュと受信**：接続された車両のIoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的に状態データを含むメッセージをMQTTでパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：組み込みのルールエンジンはブローカーと一体となって動作し、トピックマッチングルールに基づいてMQTTメッセージを処理します。メッセージが到着するとルールエンジンを通過し、定義されたルールを評価します。ペイロード変換が指定されている場合は、データフォーマットの変換、特定情報のフィルタリング、追加コンテキストによるペイロードの拡充などの変換が適用されます。
3. **Kafkaへのブリッジ**：ルールエンジンで定義されたルールがメッセージをKafkaに転送するアクションをトリガーします。Kafkaブリッジ機能を使い、MQTTトピックを事前定義されたKafkaトピックにマッピングし、処理済みのすべてのメッセージとデータをKafkaトピックに書き込みます。

車両データがKafkaに取り込まれた後は、柔軟にデータへアクセスし活用できます。

- サービスはKafkaクライアントと直接統合し、特定トピックからリアルタイムのデータストリームを消費してカスタマイズされた業務処理を実行可能です。
- Kafka Streamsを利用してストリーム処理を行い、車両状態をメモリ内で集約・相関させてリアルタイム監視ができます。
- Kafka Connectコンポーネントを用い、MySQLやElasticSearchなど外部システムへのデータ出力コネクターを選択し、保存が可能です。
=======
以下の図は、自動車IoTで用いられるEMQXとKafka間の典型的なデータ統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

Apache Kafkaへデータを流入または流出させるには、それぞれKafka Sink（Kafkaへメッセージ送信）とKafka Source（Kafkaからメッセージ受信）を作成する必要があります。ここではSinkを例に、その動作フローを説明します。

1. **メッセージのパブリッシュと受信**：接続された車載IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的に状態データを含むメッセージをMQTTでパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：ブローカーと一体となった組み込みのルールエンジンにより、これらのMQTTメッセージはトピックマッチングルールに基づいて処理されます。メッセージが到着するとルールエンジンを通過し、定義されたルールが評価されます。ペイロード変換を指定するルールがあれば、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Kafkaへのブリッジ**：ルールエンジンで定義されたルールがメッセージをKafkaへ転送するアクションをトリガーします。Kafkaブリッジ機能を用いて、MQTTトピックは事前定義されたKafkaトピックにマッピングされ、処理済みのメッセージとデータはKafkaトピックに書き込まれます。

車両データが Kafka に取り込まれた後は、以下のように柔軟にデータを活用できます。

- サービスは Kafka クライアントと直接連携し、特定トピックからリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を実行可能です。
- Kafka Streams を利用してストリーム処理を行い、車両状態をメモリ上で集約・相関させてリアルタイム監視ができます。
- Kafka Connect コンポーネントを使い、MySQL や ElasticSearch など外部システムへデータを出力して保存するための各種コネクターを選択できます。
>>>>>>> origin/release-5.10

## 特長とメリット

Apache Kafkaとのデータ統合は、以下の特長とメリットをビジネスにもたらします。

<<<<<<< HEAD
- **信頼性の高い双方向IoTデータメッセージング**：Kafkaと不安定なモバイルネットワーク上で動作するリソース制約のあるIoTデバイス間のデータ通信は、不確実なネットワークでのメッセージングに優れたMQTTプロトコルで処理されます。EMQXはMQTTメッセージをバッチでKafkaに転送するだけでなく、バックエンドシステムからのKafkaメッセージをサブスクライブし、接続されたIoTクライアントに配信します。
- **ペイロード変換**：メッセージペイロードは送信中に定義されたSQLルールで処理可能です。例えば、総メッセージ数、成功／失敗配信数、メッセージレートなどのリアルタイム指標を含むペイロードは、Kafkaに取り込まれる前にデータ抽出、フィルタリング、拡充、変換を経ることができます。
- **効果的なトピックマッピング**：多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティをKafkaヘッダーにマッピングし、1対1、1対多、多対多の柔軟なトピックマッピング方式をサポートし、MQTTトピックフィルター（ワイルドカード）にも対応しています。
- **柔軟なパーティション選択戦略**：MQTTトピックやクライアントに基づき、同じKafkaパーティションへメッセージを転送することが可能です。
- **高スループット環境での処理能力**：EMQX Kafkaプロデューサーは同期・非同期の両書き込みモードをサポートし、リアルタイム優先やパフォーマンス優先のデータ書き込み戦略を区別可能です。シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。
- **ランタイムメトリクス**：各SinkおよびSourceの総メッセージ数、成功／失敗数、現在のレートなどのランタイムメトリクスを閲覧可能です。
- **動的設定**：Dashboardや設定ファイルからSinkおよびSourceを動的に設定できます。

これらの特長により、効果的で堅牢なIoTプラットフォームアーキテクチャの構築が可能となり、増大するIoTデータを安定したネットワーク接続下で送信し、さらに効率的に保存・管理できます。
=======
- **信頼性の高い双方向IoTデータメッセージング**：不安定なモバイルネットワーク上で動作するリソース制約のあるIoTデバイスとKafka間のデータ通信は、不確実なネットワークでのメッセージングに優れたMQTTプロトコルで処理されます。EMQXはMQTTメッセージをバッチでKafkaに転送するだけでなく、バックエンドシステムからのKafkaメッセージをサブスクライブし、接続中のIoTクライアントに配信します。
- **ペイロード変換**：メッセージペイロードは転送中に定義されたSQLルールで処理可能です。例えば、総メッセージ数、成功/失敗配信数、メッセージレートなどのリアルタイムメトリクスを含むペイロードは、Kafkaに取り込む前にデータ抽出、フィルタリング、強化、変換を経ることができます。
- **効果的なトピックマッピング**：多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティをKafkaヘッダーにマッピングし、1対1、1対多、多対多の柔軟なトピックマッピング方式をサポートし、MQTTトピックフィルター（ワイルドカード）にも対応します。
- **柔軟なパーティション選択戦略**：MQTTトピックやクライアントに基づき、同一Kafkaパーティションへのメッセージ転送をサポートします。
- **高スループット状況での処理能力**：EMQX Kafkaプロデューサーは同期・非同期の書き込みモードをサポートし、リアルタイム優先やパフォーマンス優先のデータ書き込み戦略を区別可能です。シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。
- **ランタイムメトリクス**：各SinkおよびSourceの総メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスを閲覧可能です。
- **動的設定**：Dashboardまたは設定ファイルからSinkおよびSourceを動的に設定できます。

これらの特長は統合機能と柔軟性を高め、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続下で送信され、さらに効果的に保存・管理されます。
>>>>>>> origin/release-5.10

## はじめる前に

このセクションでは、EMQX DashboardでKafka SinkおよびSourceを作成する前に必要な準備事項を説明します。

### 前提条件

<<<<<<< HEAD
- EMQXデータ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識
=======
- EMQX のデータ統合[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識
>>>>>>> origin/release-5.10

### Kafka サーバーのセットアップ

ここでは macOS を例にインストールと起動手順を示します。以下のコマンドで Kafka をインストールし起動できます。

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# KRaft モードで Kafka を起動
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

<<<<<<< HEAD
詳細な操作手順は、[Kafkaドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/)を参照してください。
=======
詳細な操作手順は、[Kafka ドキュメントのクイックスタート](https://kafka.apache.org/documentation/#quickstart)を参照してください。
>>>>>>> origin/release-5.10

### Kafka トピックの作成

<<<<<<< HEAD
EMQXでデータ統合を作成する前に、関連するKafkaトピックを作成しておく必要があります。以下のコマンドでKafkaに2つのトピックを作成します：`testtopic-in`（Sink用）と`testtopic-out`（Source用）。
=======
EMQXでデータ統合を作成する前に、関連するKafkaトピックを作成してください。以下のコマンドで、Sink用の`testtopic-in`とSource用の`testtopic-out`の2つのトピックを作成します。
>>>>>>> origin/release-5.10

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka プロデューサーコネクターの作成

<<<<<<< HEAD
Kafka Sinkアクションを追加する前に、EMQXとKafka間の接続を確立するためKafkaプロデューサーコネクターを作成する必要があります。

1. EMQX Dashboardで **Integration** -> **Connector** を開きます。

2. ページ右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。
=======
Kafka Sink アクションを追加する前に、EMQX と Kafka 間の接続を確立するため Kafka プロデューサーコネクターを作成する必要があります。

1. EMQX Dashboard にアクセスし、**Integration** -> **Connector** をクリックします。
>>>>>>> origin/release-5.10

1. EMQX Dashboardで **Integration** -> **Connector** を開きます。
2. ページ右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。
3. 名前と説明を入力します。例：`my-kafka`。名前はKafka Sinkとコネクターを関連付けるために使用され、クラスター内で一意である必要があります。
<<<<<<< HEAD

4. Kafka接続に必要なパラメータを設定します：
   - **Bootstrap Hosts**：`127.0.0.1:9092`と入力します。なお、ここではEMQXとKafkaをローカルで起動している前提です。リモート環境の場合は適宜調整してください。

   - **Authentication**：Kafkaクラスターで必要な認証方式を選択します。以下の方式をサポートしています：

     - `None`：認証なし
     - `AWS IAM for MSK`：EMQXがEC2インスタンス上にデプロイされている場合のAWS MSKクラスター用
     - `Basic Auth`：**mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`のいずれか）と**username**、**password**を指定
     - `Kerberos`：**Kerberos Principal**と**Kerberos Keytabファイル**を指定

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照してください。

   - **Advanced Settings**（任意）：[高度な設定](#advanced-configurations)を参照してください。

5. **Create**をクリックする前に、**Test Connection**をクリックしてKafkaサーバーへの接続が成功するかテストできます。

6. **Create**をクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にKafkaに接続します。次に、このコネクターを使ってKafkaクラスターにデータを転送するルールを作成します。

### 認証方式

EMQXでKafkaコネクターを作成する際、Kafkaクラスターのセキュリティ設定に応じて以下の認証方式から選択可能です。

- **None**：認証不要。

- **MSK IAM**：EMQXがAmazon EC2インスタンス上にデプロイされている場合のAmazon MSKクラスター接続用。

  この方式はEC2インスタンスのメタデータサービスを利用し、IAMポリシーに基づく認証トークンを生成します。

  ::: tip 重要

  MSK IAM認証は、EMQXがEC2インスタンス上で動作しMSKクラスターに接続する場合のみサポートされます。これはEC2インスタンスのメタデータサービスに依存しているためです。

  `iptables`や`nftables`でホストレベルのイグレスフィルタリングを行う場合、`169.254.169.254`への通信をブロックしないでください。EMQXはMSK IAM認証のためにインスタンスメタデータサービスにアクセスする必要があります。同様の例外は、S3、S3 Tables、DynamoDB、KinesisなどEC2インスタンスメタデータから認証情報を取得するAWSベースの他のコネクターにも適用されます。[ルールエンジンポリシーとファイアウォールルールによるSSRF緩和](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。

  :::

- **Basic Auth**：ユーザー名とパスワードによる認証。

  選択時は以下を指定する必要があります：
  - **Mechanism**：`plain`、`scram_sha_256`、`scram_sha_512`から選択
  - **Username**と**Password**：Kafkaクラスター認証用の資格情報

- **Kerberos**：Kerberos GSSAPI認証。

  以下を指定します：
  - **Kerberos Principal**：認証に使用するKerberosのプリンシパル
  - **Kerberos Keytabファイル**：非対話認証に用いるkeytabファイルのパス

  ::: tip 重要

  Kerberos keytabファイルはすべてのEMQXノードで同一パスに配置し、EMQXサービスユーザーに読み取り権限が必要です。

  :::
=======
4. Kafka接続に必要なパラメータを設定します：
   - **Bootstrap Hosts** に `127.0.0.1:9092` を入力します。※本デモはEMQXとKafkaをローカルで起動している想定です。リモート環境の場合は適宜調整してください。
   - 他のオプションはデフォルトのままか、ビジネス要件に応じて設定します。
   - 暗号化接続を確立する場合は、**Enable TLS** のトグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照してください。
5. **Create**をクリックする前に、**Test Connection**を押してKafkaサーバーへの接続が成功するか確認できます。
6. **Create**をクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にKafkaへ接続します。次に、このコネクターを基にルールを作成し、Kafkaクラスターへデータを転送します。
>>>>>>> origin/release-5.10

## Kafka Sinkを使ったルールの作成

このセクションでは、MQTTトピック`t/#`からのメッセージを処理し、Kafkaの`testtopic-in`トピックへ処理結果を送信するルールの作成方法を示します。

1. EMQX Dashboardで **Integration** -> **Rules** を開きます。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`。

<<<<<<< HEAD
4. **SQL Editor**に以下の文を入力します。これはMQTTトピック`t/#`のメッセージをKafkaに転送する例です。

   注意：独自のSQL文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`句に含めてください。
=======
4. **SQL Editor**に以下の文を入力し、トピック`t/#`のMQTTメッセージをKafkaに転送します。

   ※独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドを`SELECT`句に含めてください。
>>>>>>> origin/release-5.10

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

<<<<<<< HEAD
   初心者の方は、**SQL Examples**をクリックし、**Enable Test**を使ってSQLルールを学習・テストできます。
=======
   初心者の方は、**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習やテストが可能です。
>>>>>>> origin/release-5.10

   :::

   ::: tip

   EMQX v5.7.2からルールSQLで環境変数を読み取る機能が追加されました。詳細は[ルールSQLで環境変数を使う](#use-environment-variables)を参照してください。

   :::

<<<<<<< HEAD
5. + **Add Action**ボタンをクリックし、トリガーされるアクションを定義します。**Type of Action**ドロップダウンから`Kafka Producer`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のKafka Producerアクションを選択します。本例では新規作成します。

6. Sinkの名前と説明を入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-kafka`コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。設定パラメータは[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

8. Sinkのデータ送信方法を設定します：

   - **Kafka Topic**：`testtopic-in`と入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。[変数テンプレートの使用](#use-variable-templates)を参照してください。

   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**から選択可能です。**Add**をクリックしてキー・バリューを追加できます。

   - **Message Key**：Kafkaメッセージのキー。プレーン文字列または`${var}`形式のプレースホルダーが使用可能です。

   - **Message Value**：Kafkaメッセージの値。プレーン文字列または`${var}`形式のプレースホルダーが使用可能です。

   - **Partition Strategy**：プロデューサーがKafkaパーティションにメッセージを分配する方法を選択します。

   - **Compression**：Kafkaメッセージのレコード圧縮／解凍に使用する圧縮アルゴリズムを指定します。
=======
5. + **Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action**ドロップダウンから`Kafka Producer`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のKafka Producerアクションを選択します。本例では新規作成します。

6. Sinkの名前と説明を入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-kafka`コネクターを選択します。隣のボタンからポップアップで新規コネクターを素早く作成することも可能です。設定パラメータは[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

8. Sinkのデータ送信方法を設定します：

   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。[変数テンプレートの使用](#use-variable-templates)を参照してください。

   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**から選択可能です。**Add**でキー・バリューを追加できます。

   - **Message Key**：Kafkaメッセージのキー。純粋な文字列か`${var}`形式のプレースホルダーを含む文字列を入力します。

   - **Message Value**：Kafkaメッセージの値。純粋な文字列か`${var}`形式のプレースホルダーを含む文字列を入力します。

   - **Partition Strategy**：プロデューサーがKafkaパーティションにメッセージを分配する方法を選択します。

   - **Compression**：Kafkaメッセージのレコード圧縮・解凍に使用する圧縮アルゴリズムを指定します。
>>>>>>> origin/release-5.10

9. **Fallback Actions**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **Advanced Settings**（任意）：[高度な設定](#advanced-configuration)を参照してください。

<<<<<<< HEAD
11. **Create**をクリックしてSinkの作成を完了します。作成後、**Create Rule**画面に戻り、新規Sinkがルールアクションに追加されます。

12. **Create**をクリックしてルール作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブでKafka Producer Sinkも確認できます。

また、**Integration** -> **Flow Designer**でトポロジーを表示可能です。トポロジーでは、トピック`t/#`のメッセージがルール`my_rule`で解析されKafkaに送信・保存される様子を直感的に把握できます。
=======
11. **Create**をクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新しいSinkがルールアクションに追加されます。

12. **Create**をクリックしてルール作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページに新規ルールが表示され、**Actions(Sink)**タブにKafka Producer Sinkが追加されていることが確認できます。

また、**Integration** -> **Flow Designer**でトポロジーを確認できます。トポロジーでは、トピック`t/#`のメッセージがルール`my_rule`で解析されKafkaに送信・保存されている様子が直感的に把握できます。
>>>>>>> origin/release-5.10

![Kafka_producer_bridge](./assets/Kafka_producer_bridge.png)

### Kafka動的トピックの設定

<<<<<<< HEAD
EMQX v5.7.2以降、Kafka Producer Sink設定で環境変数や変数テンプレートを用いてKafkaトピックを動的に設定できます。本節ではこれら2つのユースケースを紹介します。
=======
EMQX v5.7.2以降、Kafka Producer Sink設定で環境変数や変数テンプレートを使いKafkaトピックを動的に設定可能です。本節ではこれら2つの動的トピック設定のユースケースを紹介します。
>>>>>>> origin/release-5.10

#### 環境変数の使用

<<<<<<< HEAD
EMQX v5.7.2では、ルールSQL処理段階で[環境変数](../configuration/configuration.md#environment-variables)から取得した値をメッセージ内のフィールドに動的に割り当てる機能が追加されました。この機能はルールエンジンの組み込みSQL関数[getenv](../data-integration/rule-sql-builtin-functions.md#system-function)を用いてEMQXの環境変数を取得し、その値をSQL処理結果に設定します。この機能の応用例として、Kafka SinkルールアクションのKafkaトピック設定にルール出力結果のフィールドを参照してトピックを設定できます。以下はその例です。
=======
EMQX v5.7.2は、ルールSQL処理段階で[環境変数](../configuration/configuration.md#environment-variables)から値を動的に取得しメッセージフィールドに割り当てる機能を追加しました。この機能はルールエンジンの組み込みSQL関数[getenv](../data-integration/rule-sql-builtin-functions.md#system-function)を使い、EMQXの環境変数を取得します。取得した値はSQL処理結果に設定されます。この機能の応用例として、Kafka SinkルールアクションのKafkaトピック設定でルール出力結果のフィールドを参照してトピックを設定できます。以下はその例です。
>>>>>>> origin/release-5.10

::: tip 注意

他のシステム環境変数の漏洩を防ぐため、ルールエンジンで使用する環境変数名は固定プレフィックス`EMQXVAR_`を付ける必要があります。例えば、`getenv`関数で読み取る変数名が`KAFKA_TOPIC`の場合、環境変数名は`EMQXVAR_KAFKA_TOPIC`と設定してください。

:::

<<<<<<< HEAD
1. Kafkaを起動し、`testtopic-in`トピックを事前作成します。[はじめる前に](#before-you-start)の手順を参照してください。

2. EMQXを起動し環境変数を設定します。zipインストールの場合、起動時に環境変数を直接指定可能です。例としてKafkaトピック`testtopic-in`を環境変数`EMQXVAR_KAFKA_TOPIC`に設定：
=======
1. Kafkaを起動し、`testtopic-in`トピックを事前作成します。[はじめる前に](#はじめる前に)を参照してください。

2. EMQXを起動し環境変数を設定します。zipインストールの場合、起動時に環境変数を直接指定可能です。例としてKafkaトピック`testtopic-in`を環境変数`EMQXVAR_KAFKA_TOPIC`の値として設定します。
>>>>>>> origin/release-5.10

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

<<<<<<< HEAD
4. Kafka Sinkルールを設定します。**SQL Editor**に以下を入力：
=======
4. Kafka Sinkルールを設定し、**SQL Editor**に以下の文を入力します。
>>>>>>> origin/release-5.10

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

<<<<<<< HEAD
5. SQLテストを有効にし、環境変数値`testtopic-in`が正常に取得されていることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka Producer Sinkにアクションを追加します。ルール右側の**Action Outputs**で**Add Action**をクリック。

   - **Connector**：先に作成した`test-kafka`を選択。
   - **Kafka Topic**：SQLルール出力に基づき変数テンプレート形式`${kafka_topic}`で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sinkルールの作成](#create-a-rule-with-kafka-sink)を参照して追加設定を行い、最後に**Create**をクリックしてルール作成を完了します。

8. [Kafkaプロデューサールールのテスト](#test-kafka-producer-rule)の手順に従い、Kafkaにメッセージを送信します：
=======
5. SQLテストを有効化し、環境変数値`testtopic-in`が正しく取得されていることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka Producer Sinkにアクションを追加します。ルールの右側**Action Outputs**で**Add Action**をクリックします。

   - **Connector**：先ほど作成したコネクター`test-kafka`を選択します。
   - **Kafka Topic**：SQLルール出力に基づき変数テンプレート`${kafka_topic}`形式で設定します。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sinkを用いたルールの作成](#create-a-rule-with-kafka-sink)を参照し追加設定を完了し、最後に**Create**をクリックしてルール作成を完了します。

8. [Kafkaプロデューサールールのテスト](#test-kafka-producer-rule)の手順に従い、Kafkaへメッセージを送信します。
>>>>>>> origin/release-5.10

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -i pub -t t/Connection -q 1 -m 'payload string'
   ```

   メッセージはKafkaトピック`testtopic-in`で受信されるはずです：

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 \
     --topic testtopic-in
   
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   ```

#### 変数テンプレートの使用

<<<<<<< HEAD
**Kafka Topic**フィールドに静的なトピック名を設定する以外に、変数テンプレートを用いて動的にトピックを生成可能です。これによりメッセージ内容に基づいてKafkaトピックを構築でき、柔軟なメッセージ処理・分配が可能になります。例えば、`device-${payload.device}`のように指定すると、特定デバイスからのメッセージをデバイスIDを付加したトピック（例：`device-1`）に送信できます。
=======
**Kafka Topic**フィールドに静的なトピック名を設定する代わりに、変数テンプレートを使って動的にトピックを生成できます。これによりメッセージ内容に基づきKafkaトピックを構築し、柔軟なメッセージ処理・分配が可能です。例えば`device-${payload.device}`のように指定すると、特定デバイスからのメッセージをデバイスIDをサフィックスに持つトピック（例：`device-1`）に簡単に送信できます。
>>>>>>> origin/release-5.10

この例では、Kafkaに送信するメッセージペイロードに`device`キーが含まれている必要があります。例：

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

このキーがないとトピックのレンダリングに失敗し、メッセージが回復不能な形で破棄されます。

<<<<<<< HEAD
また、Kafkaには`device-1`、`device-2`などの解決済みトピックを事前作成しておく必要があります。テンプレートが存在しないトピック名に解決された場合も、メッセージは破棄されます。
=======
また、Kafka側で`device-1`、`device-2`など、解決されるすべてのトピックを事前作成しておく必要があります。テンプレートが存在しないトピック名に解決された場合も、メッセージは回復不能なエラーで破棄されます。
>>>>>>> origin/release-5.10

## Kafka プロデューサールールのテスト

<<<<<<< HEAD
Kafkaプロデューサールールが期待通り動作するかをテストするため、[MQTTX](https://mqttx.app/en)を使ってEMQXにMQTTメッセージをパブリッシュするクライアントをシミュレートできます。
=======
Kafkaプロデューサールールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/en)を使ってMQTTメッセージをEMQXにパブリッシュするクライアントをシミュレートできます。
>>>>>>> origin/release-5.10

1. MQTTXでトピック`t/1`にメッセージを送信：

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

<<<<<<< HEAD
2. **Actions(Sink)**ページでSink名をクリックし統計情報を確認します。Sinkの稼働状況に新規の受信メッセージ数と送信メッセージ数が1件ずつ増えているはずです。

3. 以下のコマンドでメッセージが`testtopic-in`トピックに書き込まれているか確認します：
=======
2. **Actions(Sink)**ページでSink名をクリックし統計情報を確認します。Sinkの稼働状況に新規受信メッセージ1件、新規送信メッセージ1件があることを確認してください。

3. 以下のコマンドで`testtopic-in`トピックにメッセージが書き込まれているか確認します。
>>>>>>> origin/release-5.10

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafka コンシューマーコネクターの作成

<<<<<<< HEAD
Kafka Sourceアクションを追加する前に、EMQXとKafka間の接続を確立するためKafkaコンシューマーコネクターを作成する必要があります。

1. EMQX Dashboardで **Integration** -> **Connector** を開きます。

2. ページ右上の **Create** をクリックします。

3. **Create Connector**ページで **Kafka Consumer** を選択し、**Next**をクリックします。

4. ソースの名前を入力します。大文字・小文字の英数字の組み合わせで、例：`my-kafka-source`。
=======
Kafka Source アクションを追加する前に、EMQX と Kafka 間の接続を確立するため Kafka コンシューマーコネクターを作成する必要があります。

1. EMQX Dashboard で **Integration** -> **Connector** をクリックします。
>>>>>>> origin/release-5.10

1. EMQX Dashboardで **Integration** -> **Connector** を開きます。
2. ページ右上の **Create** をクリックします。
3. **Create Connector**ページで **Kafka Consumer** を選択し、**Next**をクリックします。
4. ソースの名前を入力します。英数字の組み合わせで、例：`my-kafka-source`。
5. ソースの接続情報を入力します。
<<<<<<< HEAD
   - **Bootstrap Hosts**：`127.0.0.1:9092`と入力します。ローカル起動前提のため、リモート環境の場合は適宜調整してください。
   
   - **Authentication**：Kafkaクラスターで必要な認証方式を選択します。以下の方式をサポートしています：
   
     - `None`：認証なし
     - `authentication_msk_iam`：EMQXがEC2インスタンス上にデプロイされている場合のAWS MSKクラスタ用
     - `Basic Auth`：**Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`のいずれか）と**Username**、**Password**を指定
     - `Kerberos`：**Kerberos Principal**と**Kerberos Keytab File**を指定
   
     詳細は[認証方式](#authentication-method)を参照してください。
     
   - 暗号化接続を確立する場合は、**Enable TLS**のトグルをオンにします。TLS接続の詳細は**TLS for External Resource Access**を参照してください。
   
   - **Advanced Settings**（任意）：[高度な設定](#advanced-configuration)を参照してください。
   
6. **Create**をクリックする前に、**Test Connection**をクリックしてKafkaサーバーへの接続が成功するかテストできます。

11. **Create**をクリックします。関連するルール作成のオプションが表示されます。[KafkaコンシューマーSourceを使ったルール作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## KafkaコンシューマーSourceを使ったルール作成

このセクションでは、設定済みのKafkaコンシューマーSourceから転送されたメッセージをさらに処理し、MQTTトピックに再パブリッシュするルールの作成方法を示します。
=======
   - **Bootstrap Hosts**に`127.0.0.1:9092`を入力します。※本デモはローカル起動想定です。リモート環境の場合は適宜調整してください。
   - 他のオプションはデフォルトのままかビジネス要件に応じて設定します。
   - 暗号化接続を確立する場合は**Enable TLS**をオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../network/overview.md#tls-for-external-resource-access)を参照してください。
6. 詳細設定（任意）：[詳細設定](#advanced-configurations)を参照してください。
7. **Create**をクリックする前に、**Test Connection**を押してKafkaサーバーへの接続確認が可能です。
8. **Create**をクリックします。関連するルール作成のオプションが表示されます。[KafkaコンシューマーSourceを用いたルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## KafkaコンシューマーSourceを用いたルールの作成

このセクションでは、KafkaコンシューマーSourceで転送されたメッセージをさらに処理し、MQTTトピックへ再パブリッシュするルールの作成方法を示します。
>>>>>>> origin/release-5.10

このセクションでは、設定済みの Kafka コンシューマーソースから転送されたメッセージをさらに処理し、MQTT トピックに再パブリッシュするルールの作成方法を説明します。

1. EMQX Dashboardで **Integration** -> **Rules** を開きます。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`。

4. Kafkaソース`$bridges/kafka_consumer:<sourceName>`から変換されたメッセージをEMQXに転送する場合、**SQL Editor**に以下の文を入力します。

<<<<<<< HEAD
   注意：独自のSQL文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドを`SELECT`句に含めてください。Kafka Sourceの`SELECT`文では`ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node`などのフィールドが使用可能です。
=======
   ※独自のSQL構文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドを`SELECT`句に含めてください。Kafka Sourceの`SELECT`文では`ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node`などのフィールドが使用可能です。
>>>>>>> origin/release-5.10

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

<<<<<<< HEAD
   注意：初心者の方は、**SQL Examples**をクリックし、**Enable Test**を使ってSQLルールを学習・テストできます。

### KafkaコンシューマーSourceをデータ入力に追加

1. ルール作成画面右側の**Data Inputs**タブを選択し、**Add Input**をクリックします。

2. **Input Type**ドロップダウンから**Kafka Consumer**を選択します。**Source**はデフォルトの`Create Source`のままか、既存のKafka Consumerソースを選択します。本例では新規作成します。

3. Sourceの名前と説明を入力します。

4. **Connector**ドロップダウンから先ほど作成した`my-kafka-consumer`コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。設定パラメータは[Kafkaコンシューマーコネクターの作成](#create-a-kafka-consumer-connector)を参照してください。

5. 以下のフィールドを設定します：

   - **Kafka Topic**：コンシューマーSourceがメッセージを受信するKafkaトピックを指定します。
   - **Group ID**：このSourceのコンシューマーグループ識別子を指定します。未指定の場合、ソース名に基づき自動生成されます。
   - **Key Encoding Mode**および**Value Encoding Mode**：Kafkaメッセージのキーと値のエンコードモードを選択します。
7. **Offset Reset Policy**：KafkaコンシューマーがKafkaトピックパーティションのどのオフセットから読み始めるかを指定します。コンシューマーのオフセットが存在しないか無効な場合に適用されます。

   - `latest`を選択すると、コンシューマーは最新のオフセットから読み始め、開始前に生成されたメッセージはスキップされます。
   - `earliest`を選択すると、コンシューマーはパーティションの先頭から読み始め、開始前に生成されたメッセージも含めてすべての履歴データを読み取ります。
8. **Advanced Settings**（任意）：[高度な設定](#advanced-configuration)を参照してください。
9. **Create**をクリックする前に、**Test Connectivity**をクリックしてKafkaサーバーへの接続をテスト可能です。
10. **Create**をクリックしてSourceの作成を完了します。**Create Rule**画面に戻ると、**Data Inputs**タブに新規Sourceが表示されます。
=======
   初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールの学習とテストが可能です。

   初心者の方は、**SQL Examples**や**Enable Test**をクリックしてSQLルールの学習やテストが可能です。

1. ルール作成画面右側の **Data Inputs** タブを選択し、**Add Input** をクリックします。

### KafkaコンシューマーSourceをデータ入力に追加

1. ルール作成ページの右側**Data Inputs**タブを選択し、**Add Input**をクリックします。
2. **Input Type**ドロップダウンから**Kafka Consumer**を選択します。**Source**はデフォルトの`Create Source`のままか、既存のKafka Consumerソースを選択します。本例では新規作成します。
3. Sourceの名前と説明を入力します。
4. **Connector**ドロップダウンから先ほど作成した`my-kafka-consumer`コネクターを選択します。隣のボタンからポップアップで新規コネクターを素早く作成することも可能です。設定パラメータは[Kafkaコンシューマーコネクターの作成](#create-a-kafka-consumer-connector)を参照してください。
5. 以下のフィールドを設定します：

   - **Kafka Topic**：コンシューマーソースがサブスクライブするKafkaトピックを指定します。
   - **Group ID**：このソースのコンシューマーグループ識別子を指定します。未指定の場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode**および**Value Encoding Mode**：Kafkaメッセージのキーと値のエンコードモードを選択します。
7. **Offset Reset Policy**：Kafkaコンシューマーがオフセットを持たない、または無効な場合に読み込み開始位置を決めるポリシーを選択します。

   - `latest`を選択すると、コンシューマーは最新のオフセットから読み始め、開始前のメッセージはスキップされます。
   - `earliest`を選択すると、コンシューマーはパーティションの先頭から読み始め、開始前のメッセージも含めてすべての履歴データを読みます。
8. 詳細設定（任意）：[詳細設定](#advanced-configurations)を参照してください。
9. **Create**をクリックする前に、**Test Connectivity**を押してKafkaサーバーへの接続確認が可能です。
10. **Create**をクリックしてSource作成を完了します。ルール作成ページの**Data Inputs**タブに新しいSourceが表示されます。
>>>>>>> origin/release-5.10

### 再パブリッシュアクションの追加

1. **Action Outputs**タブを選択し、+ **Add Action**ボタンをクリックしてルールでトリガーされるアクションを定義します。
2. **Type of Action**ドロップダウンから**Republish**を選択します。
<<<<<<< HEAD

3. **Topic**および**Payload**フィールドに再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として、`t/1`と`${.}`を入力します。
   - **Topic**フィールドには`${}`を使って動的にMQTTトピックを指定可能です。例：`t/${key}`（`${}`内のパラメータはSQLの`SELECT`文に含める必要があります）。
4. **Add**をクリックしてアクションをルールに追加します。

5. **Create Rule**画面に戻り、**Save**をクリックします。
=======
3. **Topic**と**Payload**フィールドに、再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として、トピックに`t/1`、ペイロードに`${.}`を入力します。
   - **Topic**フィールドには`${}`を使って動的にMQTTトピックを指定可能です。例：`t/${key}`（`${}`内のパラメータはSQLの`SELECT`文に含まれている必要があります）。
4. **Add**をクリックしてアクションをルールに追加します。
5. ルール作成ページに戻り、**Save**をクリックします。
>>>>>>> origin/release-5.10

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka ソースルールのテスト

<<<<<<< HEAD
Kafka Sourceとルールが期待通り動作するかテストするため、[MQTTX](https://mqttx.app/)を使ってEMQXのトピックをサブスクライブするクライアントをシミュレートし、KafkaプロデューサーでKafkaトピックにデータを生成します。その後、KafkaからのデータがEMQXによってクライアントがサブスクライブするトピックに再パブリッシュされるか確認します。
=======
Kafka Sourceとルールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/)を使ってEMQXのトピックをサブスクライブするクライアントをシミュレートし、KafkaプロデューサーでKafkaトピックにデータを生成します。その後、KafkaからのデータがEMQXによってクライアントがサブスクライブするトピックに再パブリッシュされているか確認します。
>>>>>>> origin/release-5.10

1. MQTTXでトピック`t/1`をサブスクライブ：

   ```bash
   mqttx sub -t t/1 -v
   ```

<<<<<<< HEAD
2. 新しいコマンドラインウィンドウを開き、以下のコマンドでKafkaプロデューサーを起動：
=======
2. 新しいコマンドラインウィンドウを開き、以下のコマンドで Kafka プロデューサーを起動します。
>>>>>>> origin/release-5.10

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力待ちになります。

<<<<<<< HEAD
3. `{"msg": "Hello EMQX"}`を入力し、`testtopic-out`トピックにメッセージを生成してEnterを押します。

4. MQTTXのサブスクリプションを確認すると、Kafkaからの以下のメッセージがトピック`t/1`で受信されるはずです：
=======
3. `{"msg": "Hello EMQX"}`を入力し、`testtopic-out`トピックにメッセージを生成します。

4. MQTTXのサブスクリプションで、Kafkaからの以下のメッセージがトピック`t/1`で受信されることを確認します。
>>>>>>> origin/release-5.10

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

<<<<<<< HEAD
このセクションでは、データ統合のパフォーマンス最適化や特定シナリオに応じたカスタマイズのための高度な設定オプションを説明します。コネクター、Sink、Source作成時に**Advanced Settings**を展開し、ビジネス要件に応じて以下の設定を行えます。

| 項目                                      | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                 | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータ取得要求を送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `disabled`         |
| Min Metadata Refresh Interval             | クライアントがKafkaブローカーやトピックのメタデータを更新する最小間隔。短すぎるとKafkaサーバーの負荷が増加します。 | `3`秒              |
| Metadata Request Timeout                  | Kafkaからメタデータを要求する際の最大待機時間。                   | `5`秒              |
| Connect Timeout                           | TCP接続確立の最大待機時間。認証時間も含みます。                   | `5`秒              |
| Max Wait Time (Source)                    | Kafkaブローカーからのフェッチ応答を待つ最大時間。                   | `1`秒              |
| Fetch Bytes (Source)                      | フェッチ要求ごとにKafkaから取得するバイト数。設定値がKafka内のメッセージサイズより小さいとフェッチ性能に悪影響を与える可能性があります。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafkaバッチ内でメッセージを収集する最大バイト数。Kafkaブローカーのデフォルトは1MBですが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮し若干小さめに設定しています。単一メッセージがこの制限を超える場合は別バッチで送信されます。 | `896` KB           |
| Offset Commit Interval (Source)           | 各コンシューマーグループのオフセットコミット要求送信間隔。           | `5`秒              |
| Required Acks (Sink)                      | Kafkaパーティションリーダーがフォロワーから待つ必要のあるアック数：<br />`all_isr`：全てのインシンクレプリカからのアックを要求。<br />`leader_only`：リーダーからのみアックを要求。<br />`none`：Kafkaからのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加検知後、`partition_strategy`に基づき新パーティションをメッセージ送信に組み込みます。 | `60`秒             |
| Max Inflight (Sink)                       | Kafkaプロデューサーがアック受信前に送信可能な最大バッチ数（パーティション毎）。値が大きいほどスループットは向上しますが、1より大きい場合はメッセージ順序の入れ替わりリスクがあります。<br />未アックメッセージ数を制御し、負荷バランスを取ります。 | `10`               |
| Query Mode (Source)                       | 非同期または同期クエリモードを選択し、要件に応じてメッセージ送信を最適化。非同期モードではKafka書き込みがMQTTパブリッシュをブロックしませんが、クライアントがKafka到達前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の確認待機最大時間。メッセージ送信完了をタイムリーに保証し、長時間待機を回避します。<br />`Sync`モード時のみ有効。 | `5`秒              |
| Buffer Mode (Sink)                        | メッセージ送信前のバッファリング方式。メモリバッファリングは送信速度を向上させます。<br />`memory`：メモリにバッファ。EMQXノード再起動でメッセージ消失。<br />`disk`：ディスクにバッファ。再起動後もメッセージ保持。<br />`hybrid`：初めはメモリバッファで、一定量超過時にディスクにオフロード。メモリモード同様、再起動で消失。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafkaパーティション毎の最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ空間を確保。<br />メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが`disk`または`hybrid`時に適用。メッセージ保存用の分割ファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが`memory`時に適用。高メモリ圧迫時に古いバッファメッセージを自動破棄し、システムの安定性を確保。<br />Linuxシステムのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ソケットバッファサイズを管理し、ネットワーク送信性能を最適化。         | `1024` KB          |
| TCP Keepalive                             | Kafkaブリッジ接続のTCPキープアライブを有効化し、長時間の非アクティブによる接続切断を防止。値は`Idle, Interval, Probes`の3つの数値をカンマ区切りで指定：<br />Idle：サーバーがキープアライブプローブを開始するまでのアイドル秒数（Linuxデフォルト7200秒）。<br />Interval：各キープアライブプローブ間の秒数（Linuxデフォルト75秒）。<br />Probes：応答なしと判断するまでの最大プローブ数（Linuxデフォルト9回）。<br />例：`240,30,5`は240秒アイドル後にプローブ開始、30秒間隔で最大5回送信し応答なしなら接続切断。 | `none`             |
| Max Linger Time                           | パーティション毎のプロデューサーがバッチ収集のためにメッセージを待つ最大時間。デフォルト`0`は待機なし。メモリバッファ以外では`5ms`に設定するとIOPSを大幅削減可能だがレイテンシ増加のトレードオフあり。 | `0` ミリ秒         |
| Max Linger Bytes                          | パーティション毎のプロデューサーがバッチ収集のためにメッセージを待つ最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状況チェック間隔。                               | `15`秒             |
=======
このセクションでは、データ統合のパフォーマンス最適化や特定シナリオに応じたカスタマイズに役立つ詳細設定オプションを説明します。コネクター、Sink、Source作成時に**Advanced Settings**を展開し、ビジネス要件に応じて以下の設定を行えます。

| 項目                                      | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Min Metadata Refresh Interval             | Kafkaブローカーおよびトピックのメタデータを更新する最短間隔。小さすぎるとKafkaサーバーへの負荷が増加する可能性があります。 | `3`秒              |
| Metadata Request Timeout                  | Kafkaからメタデータを要求する際の最大待機時間。               | `5`秒              |
| Connect Timeout                           | TCP接続確立の最大待機時間。認証有効時は認証時間も含みます。     | `5`秒              |
| Max Wait Time (Source)                    | Kafkaブローカーからのフェッチ応答を待つ最大時間。              | `1`秒              |
| Fetch Bytes (Source)                      | Kafkaから1回のフェッチで取得するバイト数。設定値がメッセージサイズより小さいとフェッチ性能に悪影響を与える可能性があります。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafkaバッチ内で収集するメッセージの最大サイズ（バイト）。Kafkaブローカーのデフォルトは1MBですが、EMQXはメッセージエンコードのオーバーヘッドを考慮しやや小さめに設定。単一メッセージが上限を超える場合は別バッチで送信されます。 | `896` KB           |
| Offset Commit Interval (Source)           | コンシューマーグループごとに送信するオフセットコミットの間隔。 | `5`秒              |
| Required Acks (Sink)                      | Kafkaパーティションリーダーがフォロワーから待つアックの種類：<br />`all_isr`：全てのインシンクレプリカからのアックを要求。<br />`leader_only`：リーダーのみからのアックを要求。<br />`none`：Kafkaからのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加検知後、設定された`partition_strategy`に基づき新パーティションをメッセージ送信に組み込みます。 | `60`秒             |
| Max Inflight (Sink)                       | Kafkaプロデューサー（パーティションごと）でアック受信前に送信可能な最大バッチ数。値が大きいほどスループットは向上しますが、1より大きいとメッセージの順序入れ替わりリスクがあります。未アックメッセージ数を制御し負荷バランスを取ります。 | `10`               |
| Query Mode (Source)                       | 非同期または同期クエリモードを選択し、メッセージ送信を最適化。非同期モードではKafka書き込みがMQTTパブリッシュをブロックしませんが、クライアントがKafka到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の最大待機時間。メッセージ送信完了をタイムリーに保証し長時間待機を防ぎます。同期モード時のみ有効。 | `5`秒              |
| Buffer Mode (Sink)                        | メッセージ送信前のバッファリング方式。メモリバッファは送信速度向上に寄与。<br />`memory`：メモリにバッファ。EMQXノード再起動時にメッセージは失われる。<br />`disk`：ディスクにバッファ。ノード再起動後もメッセージ保持。<br />`hybrid`：最初はメモリにバッファし、一定量超過時に段階的にディスクへオフロード。メモリモード同様ノード再起動でメッセージは失われる。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafkaパーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ空間を確保。メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが`disk`または`hybrid`時に適用。メッセージ保存用セグメントファイルのサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが`memory`時に適用。高メモリ圧迫時に古いバッファメッセージを自動破棄し、システムの安定性を確保。Linux環境のみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ソケットバッファサイズを管理しネットワーク送信性能を最適化。 | `1024` KB          |
| TCP Keepalive                             | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間の非通信による接続切断を防止。値は`Idle, Interval, Probes`の3つの数値をカンマ区切りで指定。<br />Idle：接続がアイドル状態となってからキープアライブプローブ送信までの秒数（Linuxデフォルト7200秒）。<br />Interval：プローブ間隔秒数（Linuxデフォルト75秒）。<br />Probes：応答なしで接続切断と判断するまでの最大プローブ数（Linuxデフォルト9）。<br />例：`240,30,5`は240秒アイドル後にプローブ開始、30秒間隔で5回プローブ送信し応答なければ切断。 | `none`             |
| Max Linger Time                           | パーティションごとのプロデューサーがバッチ収集のためにメッセージを待つ最大時間。デフォルト`0`は待機なし。メモリ以外のバッファモードでは`5ms`設定でIOPSを大幅削減可能だがレイテンシ増加のトレードオフあり。 | `0` ミリ秒         |
| Max Linger Bytes                          | パーティションごとのプロデューサーがバッチ収集のためにメッセージを待つ最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状態をチェックする間隔。                       | `15`秒             |
>>>>>>> origin/release-5.10

## 参考情報

EMQX は Apache Kafka とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ：**

<<<<<<< HEAD
- [MQTTとKafkaでつくるコネクテッドビークルのストリーミングデータパイプライン：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
=======
- [MQTTとKafkaでつなぐコネクテッドビークルのストリーミングデータパイプライン：3分でわかるガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
>>>>>>> origin/release-5.10
- [MQTTとKafka：IoTデータ統合の強化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterprise パフォーマンスベンチマークテスト：Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画：**

<<<<<<< HEAD
- [EMQX Cloudルールエンジンを使ったデバイスデータのKafkaブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloudルールエンジンに関する動画で、将来的により適切な動画に差し替え予定）
=======
- [EMQX Cloudルールエンジンを使ったデバイスデータのKafkaブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloudルールエンジンに関する動画で、将来的により適切な動画に置き換え予定）
>>>>>>> origin/release-5.10
