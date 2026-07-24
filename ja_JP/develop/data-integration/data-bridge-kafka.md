# Apache KafkaへのMQTTデータストリーミング

[Apache Kafka](https://kafka.apache.org/)は、アプリケーションやシステム間でのデータストリームのリアルタイム転送を処理できる、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、KafkaはエッジIoT通信向けに設計されておらず、Kafkaクライアントは安定したネットワーク接続とより多くのハードウェアリソースを必要とします。IoTの領域では、デバイスやアプリケーションから生成されるデータは軽量なMQTTプロトコルを使って送信されます。EMQXのKafkaとの統合により、ユーザーはMQTTデータをKafkaへ、またはKafkaからシームレスにストリーミングできます。MQTTのデータストリームはKafkaのトピックに取り込まれ、リアルタイムの処理、保存、分析が可能です。逆に、KafkaのトピックデータはMQTTデバイスで消費でき、タイムリーなアクションを実現します。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQXとKafka間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に解説します。

## 動作概要

Apache Kafkaとのデータ統合は、MQTTベースのIoTデータとKafkaの強力なデータ処理能力のギャップを埋めるためにEMQXに標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、両プラットフォーム間のデータストリーミングと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は、自動車IoTで利用されるEMQXとKafka間の典型的なデータ統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->
Apache Kafkaへのデータの流入および流出には、それぞれKafka Sink（Kafkaへメッセージを送信）とKafka Source（Kafkaからメッセージを受信）を作成する必要があります。ここではSinkを例に、そのフローを説明します。

1. **メッセージのパブリッシュと受信**: 接続された車載IoTデバイスはMQTTプロトコルを通じてEMQXに正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**: ブローカーと一体化した組み込みのルールエンジンにより、これらのMQTTメッセージはトピックマッチングルールに基づいて処理されます。メッセージが到着するとルールエンジンを通過し、定義されたルールを評価します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **Kafkaへのブリッジ**: ルールエンジンで定義されたルールがメッセージのKafka転送アクションをトリガーします。Kafkaブリッジ機能を用いて、MQTTトピックが事前定義されたKafkaトピックにマッピングされ、処理済みのすべてのメッセージとデータがKafkaトピックに書き込まれます。

車両データがKafkaに取り込まれた後は、柔軟にデータへアクセスし活用できます。

- サービスはKafkaクライアントと直接連携し、特定トピックからリアルタイムのデータストリームを消費してカスタマイズされたビジネス処理を実現可能です。
- Kafka Streamsを利用したストリーム処理により、車両状態の集約や相関付けをメモリ内で行いリアルタイム監視が可能です。
- Kafka Connectコンポーネントを使い、MySQLやElasticSearchなどの外部システムへデータを出力し保存するための各種コネクターを選択できます。

## 特徴と利点

Apache Kafkaとのデータ統合は、以下の特徴と利点をビジネスにもたらします。

- **信頼性の高い双方向IoTデータメッセージング**: 不安定なモバイルネットワーク上で動作するリソース制約のあるIoTデバイスとKafka間のデータ通信は、不確実なネットワークに強いMQTTプロトコルで処理されます。EMQXはMQTTメッセージをバッチでKafkaに転送するだけでなく、バックエンドシステムからのKafkaメッセージをサブスクライブし、接続されたIoTクライアントに配信します。
- **ペイロード変換**: メッセージのペイロードは転送中に定義されたSQLルールで処理可能です。例えば、総メッセージ数、成功/失敗配信数、メッセージレートなどのリアルタイムメトリクスを含むペイロードは、Kafkaに取り込まれる前にデータ抽出、フィルタリング、強化、変換を経ることができます。
- **効果的なトピックマッピング**: 多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティをKafkaヘッダーにマッピングし、1対1、1対多、多対多の柔軟なトピックマッピング方法をサポートし、MQTTトピックフィルター（ワイルドカード）にも対応しています。
- **柔軟なパーティション選択戦略**: MQTTトピックやクライアントに基づいて同一Kafkaパーティションへメッセージを転送することをサポートします。
- **高スループット状況での処理能力**: EMQX Kafkaプロデューサーは同期・非同期の両書き込みモードをサポートし、リアルタイム優先とパフォーマンス優先のデータ書き込み戦略を区別可能で、シナリオに応じてレイテンシとスループットの柔軟なバランスを実現します。
- **ランタイムメトリクス**: 各SinkおよびSourceのランタイムメトリクス（総メッセージ数、成功/失敗数、現在のレートなど）を閲覧可能です。
- **動的設定**: ダッシュボードまたは設定ファイルでSinkおよびSourceを動的に設定できます。

これらの特徴は統合能力と柔軟性を高め、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増加するIoTデータ量は安定したネットワーク接続下で送信され、さらに効果的に保存・管理できます。

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

詳細な操作手順は[Kafkaドキュメントのクイックスタート](https://kafka.apache.org/documentation/#quickstart)を参照してください。

### Kafkaトピックの作成

EMQXでデータ統合を作成する前に、関連するKafkaトピックを作成してください。以下のコマンドでKafkaに2つのトピックを作成します：`testtopic-in`（Sink用）と`testtopic-out`（Source用）。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafkaプロデューサーコネクターの作成

Kafka Sinkアクションを追加する前に、EMQXとKafka間の接続を確立するためKafkaプロデューサーコネクターを作成します。

1. EMQXダッシュボードで、**Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例：`my-kafka`。名前はKafka Sinkとコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. Kafka接続に必要なパラメータを設定します：
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。デモではEMQXとKafkaをローカルで動かしている前提です。リモート環境の場合は適宜調整してください。

   - **Authentication**: Kafkaクラスターの認証方式を選択します。以下の方式をサポートしています：

     - `None`: 認証なし。
     - `AWS IAM for MSK`: AWS MSKクラスター利用時、EMQXがEC2上にデプロイされている場合。
     - `Basic Auth`: **mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**username** と **password** を指定。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab file** を指定。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS** トグルをオンにします。TLS接続の詳細は[外部リソースアクセスのTLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。

   - **Advanced Settings**（任意）：[高度な設定](#advanced-configurations)を参照。

5. **Create** をクリックする前に、**Test Connection** をクリックしてKafkaサーバーへの接続が成功するかテストできます。

6. **Create** ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にKafkaに接続します。次に、このコネクターを基にルールを作成し、Kafkaクラスターへデータを転送します。

### 認証方式

EMQXでKafkaコネクターを作成する際、Kafkaクラスターのセキュリティ設定に応じて複数の認証方式から選択可能です。

- **None**: 認証不要。

- **MSK IAM**: EMQXがAmazon EC2上にデプロイされ、Amazon MSKクラスターに接続する場合に使用。

  AWS EC2インスタンスメタデータサービスを利用し、インスタンスに付与されたIAMポリシーに基づく認証トークンを生成します。

  ::: tip 重要

  MSK IAM認証は、EMQXがEC2インスタンス上で動作しMSKクラスターに接続する場合のみサポートされます。AWS Metadata APIに依存しているためです。

  :::

- **Basic Auth**: ユーザー名とパスワードによる認証。

  選択時は以下を指定する必要があります：
  - **Mechanism**: `plain`、`scram_sha_256`、`scram_sha_512`のいずれか。
  - **Username** と **Password**: Kafkaクラスター認証用の資格情報。

- **Kerberos**: Kerberos GSSAPI認証。

  必須項目：
  - **Kerberos Principal**: 認証に使用するKerberosのプリンシパル。
  - **Kerberos Keytab File**: 非対話認証に用いるkeytabファイルのパス。

  ::: tip 重要

  KerberosのkeytabファイルはすべてのEMQXノードで同一パスに配置し、EMQXサービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sinkを用いたルールの作成

このセクションでは、MQTTトピック `t/#` のメッセージを処理し、Kafka Sinkを使ってKafkaの `testtopic-in` トピックに送信するルールの作成方法を示します。

1. EMQXダッシュボードで、**Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`。

4. **SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` のMQTTメッセージをKafkaに転送する例です。

   注意：独自のSQL構文を指定する場合は、Sinkで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は、**SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストが可能です。

   :::

   ::: tip

   EMQX v5.7.2からルールSQL内で環境変数を読み取る機能が追加されました。詳細は[ルールSQLで環境変数を使う](#use-environment-variables)を参照してください。

   :::

5. + **Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Kafka Producer` を選択し、**Action** はデフォルトの `Create Action` のままにするか、既存のKafka Producerアクションを選択します。この例では新規プロデューサーアクションを作成しルールに追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-kafka` コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。設定パラメータは[Kafkaプロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

8. Sinkのデータ送信方法を設定します：

   - **Kafka Topic**: `testtopic-in` と入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。詳細は[変数テンプレートの使用](#use-variable-templates)を参照してください。

   - **Kafka Headers**: Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは **Kafka Header Value Encod Type** ドロップダウンから選択可能です。**Add** をクリックしてキー・バリューのペアを追加できます。

   - **Message Key**: Kafkaメッセージのキー。純粋な文字列またはプレースホルダー（${var}）を含む文字列を入力可能です。

   - **Message Value**: Kafkaメッセージの値。純粋な文字列またはプレースホルダー（${var}）を含む文字列を入力可能です。

   - **Partition Strategy**: プロデューサーがKafkaパーティションにメッセージを分配する方法を選択します。

   - **Compression**: Kafkaメッセージ内のレコードを圧縮/解凍するための圧縮アルゴリズムを指定します。

9. **Fallback Actions**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **Advanced Settings**（任意）：[高度な設定](#advanced-configuration)を参照してください。

11. **Create** ボタンをクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新しいSinkがルールアクションに追加されます。

12. **Create** ボタンをクリックしてルール作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules** ページで新規ルールを確認でき、**Actions(Sink)** タブには新規KafkaプロデューサーSinkが表示されます。

また、**Integration** -> **Flow Designer** でトポロジーを確認できます。トポロジーを通じて、トピック `t/#` のメッセージがルール `my_rule` によって解析されKafkaに送信・保存される様子を直感的に把握できます。

![Kafka_producer_bridge](./assets/Kafka_producer_bridge.png)

### Kafka動的トピックの設定

EMQX v5.7.2以降、Kafka Producer Sink設定で環境変数や変数テンプレートを使いKafkaトピックを動的に設定可能です。本節ではこれら2つの動的トピック設定のユースケースを紹介します。

#### 環境変数の使用

EMQX v5.7.2は、ルールSQL処理段階で[環境変数](../../guides/configuration/configuration.md#environment-variables)から取得した値をメッセージ内のフィールドに動的に割り当てる機能を導入しました。この機能はルールエンジンの組み込みSQL関数[getenv](../data-integration/rule-sql-builtin-functions.md#system-function)を使い、EMQXの環境変数を取得します。取得した変数値はSQL処理結果に設定されます。この機能の応用例として、Kafka SinkルールアクションでKafkaトピックをルール出力結果のフィールドを参照して設定できます。以下はその例です。

::: tip 注意

システムの他の環境変数の漏洩を防ぐため、ルールエンジンで使用する環境変数名は固定プレフィックス `EMQXVAR_` を付ける必要があります。例えば、`getenv`関数で読み取る変数名が `KAFKA_TOPIC` の場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` と設定してください。

:::

1. Kafkaを起動し、`testtopic-in` というKafkaトピックを事前作成します。[はじめる前に](#はじめる前に)の手順を参照してください。

2. EMQXを起動し環境変数を設定します。zipインストールの場合は起動時に直接環境変数を指定可能です。例としてKafkaトピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` の値に設定します。

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

5. SQLテストを有効にし、環境変数の値 `testtopic-in` が正しく取得されていることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka Producer Sinkにアクションを追加します。ルール右側の**Action Outputs**で **Add Action** をクリックします。

   - **Connector**: 先ほど作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**: SQLルール出力の変数テンプレート形式 `${kafka_topic}` で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. 追加設定は[Kafka Sinkを用いたルールの作成](#create-a-rule-with-kafka-sink)を参照し、最後に **Create** をクリックしてルール作成を完了します。

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

**Kafka Topic** フィールドに静的なトピック名を設定する代わりに、変数テンプレートを使って動的にトピックを生成できます。これによりメッセージ内容に基づいてKafkaトピックを構築でき、柔軟なメッセージ処理と分配が可能です。例えば、`device-${payload.device}` のような形式を指定すると、特定デバイスからのメッセージをデバイスIDをサフィックスとするトピック（例：`device-1`）へ簡単に送信できます。

この例では、Kafkaに送信するメッセージペイロードに `device` キーが含まれている必要があります。以下は例のペイロードです。

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

このキーが含まれていないとトピックのレンダリングに失敗し、メッセージが回復不能な形でドロップされます。

また、Kafka側で解決されるすべてのトピック（例：`device-1`、`device-2`など）を事前に作成しておく必要があります。テンプレートで解決されたトピック名がKafkaに存在しない場合も、メッセージは回復不能なエラーでドロップされます。

## Kafkaプロデューサールールのテスト

Kafkaプロデューサールールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/en)を使ってEMQXにMQTTメッセージをパブリッシュするクライアントをシミュレートできます。

1. MQTTXでトピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページでSink名をクリックし統計情報を確認します。Sinkの稼働状況を確認し、新規の受信メッセージ数と送信メッセージ数が1件ずつあることを確認してください。

3. 以下のコマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafkaコンシューマーコネクターの作成

Kafka Sourceアクションを追加する前に、EMQXとKafka間の接続を確立するためKafkaコンシューマーコネクターを作成します。

1. EMQXダッシュボードで、**Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例：`my-kafka-source`。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。デモではローカル環境を想定しています。リモート環境の場合は適宜調整してください。
   
   - **Authentication**: Kafkaクラスターの認証方式を選択します。以下の方式をサポートしています：
   
     - `None`: 認証なし。
     - `authentication_msk_iam`: AWS MSKクラスター利用時、EMQXがEC2上にデプロイされている場合。
     - `Basic Auth`: **Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**Username** と **Password** を指定。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab File** を指定。
   
     詳細は[認証方式](#authentication-method)を参照してください。
     
   - 暗号化接続を確立する場合は、**Enable TLS** トグルをオンにします。詳細は**外部リソースアクセスのTLS**を参照してください。
   
   - **Advanced Settings**（任意）：[高度な設定](#advanced-configuration)を参照してください。
   
6. **Create** をクリックする前に、**Test Connection** をクリックしてKafkaサーバーへの接続が成功するかテスト可能です。

11. **Create** をクリックします。関連するルール作成のオプションが表示されます。[KafkaコンシューマーSourceを用いたルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## KafkaコンシューマーSourceを用いたルールの作成

このセクションでは、設定済みのKafkaコンシューマーSourceから転送されたメッセージをさらに処理し、MQTTトピックへ再パブリッシュするルールの作成方法を示します。

### ルールSQLの作成

1. EMQXダッシュボードで、**Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します。例：`my_rule`。

4. **SQL Editor** に以下のステートメントを入力します。これはKafkaソース`$bridges/kafka_consumer:<sourceName>`から変換されたメッセージをEMQXに転送する例です。

   注意：独自のSQL構文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。Kafka Sourceの`SELECT`文では、`ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node`などのフィールドが使用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   注意：初心者の方は、**SQL Examples** と **Enable Test** をクリックしてSQLルールの学習とテストが可能です。

### KafkaコンシューマーSourceをデータ入力として追加

1. ルール作成ページ右側の**Data Inputs**タブを選択し、**Add Input** をクリックします。

2. **Input Type** ドロップダウンから **Kafka Consumer** を選択します。**Source** ドロップダウンはデフォルトの `Create Source` のままにするか、既存のKafkaコンシューマーSourceを選択します。この例では新規コンシューマーSourceを作成しルールに追加します。

3. ソースの名前と説明を対応するテキストボックスに入力します。

4. **Connector** ドロップダウンから先ほど作成した `my-kafka-consumer` コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。設定パラメータは[Kafkaコンシューマーコネクターの作成](#create-a-kafka-consumer-connector)を参照してください。

5. 以下のフィールドを設定します：

   - **Kafka Topic**: コンシューマーSourceがサブスクライブするKafkaトピックを指定します。
   - **Group ID**: このソースのコンシューマーグループIDを指定します。未指定の場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode** と **Value Encoding Mode**: Kafkaメッセージのキーおよび値のエンコードモードを選択します。

7. **Offset Reset Policy**: コンシューマーがKafkaトピックパーティションのどのオフセットから読み始めるかのポリシーを選択します。

   - `latest` を選択すると、コンシューマーは最新のオフセットから読み始め、開始前に生成されたメッセージはスキップされます。
   - `earliest` を選択すると、コンシューマーはパーティションの先頭から読み始め、開始前に生成されたメッセージも含めてすべての履歴データを読みます。

8. **Advanced Settings**（任意）：[高度な設定](#advanced-configuration)を参照してください。

9. **Create** をクリックする前に、**Test Connectivity** をクリックしてKafkaサーバーへの接続テストが可能です。

10. **Create** をクリックしてSourceの作成を完了します。**Create Rule** ページに戻ると、**Data Inputs** タブに新しいSourceが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs** タブを選択し、+ **Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action** ドロップダウンから **Republish** を選択します。

3. **Topic** と **Payload** フィールドに、再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として、`t/1` と `${.}` を入力します。
   - **Topic** フィールドには `${}` を使って動的にMQTTトピックを指定可能です。例：`t/${key}`（`${}`内のパラメータはSQLの`SELECT`文に含まれている必要があります）。

4. **Add** をクリックしてアクションをルールに追加します。

5. **Create Rule** ページに戻り、**Save** をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Sourceルールのテスト

Kafka Sourceとルールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/)を使ってEMQXのトピックをサブスクライブするクライアントをシミュレートし、KafkaプロデューサーでKafkaトピックにデータを生成します。その後、KafkaからのデータがEMQXによってクライアントがサブスクライブするトピックに再パブリッシュされているか確認します。

1. MQTTXでトピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドでKafkaプロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力を促されます。

3. `{"msg": "Hello EMQX"}` と入力し、`testtopic-out` トピックにメッセージを生成してEnterを押します。

4. MQTTXのサブスクリプションを確認します。Kafkaからの以下のメッセージがトピック `t/1` で受信されるはずです。

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

このセクションでは、データ統合のパフォーマンス最適化やシナリオに応じたカスタマイズのための高度な設定オプションを説明します。コネクター、Sink、Source作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                                      | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                 | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `disabled`         |
| Min Metadata Refresh Interval             | クライアントがKafkaブローカーやトピックのメタデータを更新する最小間隔。小さすぎるとKafkaサーバーの負荷が増大します。 | `3` 秒             |
| Metadata Request Timeout                  | Kafkaからメタデータを要求する際の最大待機時間。                   | `5` 秒             |
| Connect Timeout                           | TCP接続確立の最大待機時間。認証有効時は認証時間も含みます。         | `5` 秒             |
| Max Wait Time (Source)                    | Kafkaブローカーからのフェッチ応答を待つ最大時間。                   | `1` 秒             |
| Fetch Bytes (Source)                      | フェッチ要求ごとにKafkaから引き出すバイト数。設定値がKafka内のメッセージサイズより小さいとフェッチ性能に悪影響があります。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafkaバッチ内でメッセージを収集する最大サイズ（バイト）。Kafkaブローカーのデフォルトは1MBですが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮し1MBよりやや小さめに設定しています。単一メッセージがこのサイズを超える場合は別バッチとして送信されます。 | `896` KB           |
| Offset Commit Interval (Source)           | 各コンシューマーグループに送信するオフセットコミット要求の間隔。     | `5` 秒             |
| Required Acks (Sink)                      | Kafkaパーティションリーダーがフォロワーから待つ必要のあるアックの種類：<br />`all_isr`: 全てのインシンクレプリカからのアックを要求。<br />`leader_only`: パーティションリーダーのみからのアックを要求。<br />`none`: Kafkaからのアックは不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加検知後、`partition_strategy`に基づき新パーティションをメッセージ配送に組み込みます。 | `60` 秒            |
| Max Inflight (Sink)                       | Kafkaプロデューサーがアック受信前に送信可能なバッチ数（パーティション毎）。値が大きいほどスループット向上が期待できますが、1より大きい場合はメッセージの順序入れ替わりリスクがあります。<br />未アックのメッセージ数を制御し、システム過負荷を防ぎます。 | `10`               |
| Query Mode (Source)                       | メッセージ送信最適化のため非同期または同期クエリモードを選択可能。非同期モードではKafka書き込みがMQTTパブリッシュをブロックしませんが、クライアントがKafka到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の最大待機時間。メッセージ送信完了をタイムリーに保証し長時間待機を防ぎます。<br />`Sync`モード時のみ適用。 | `5` 秒             |
| Buffer Mode (Sink)                        | メッセージ送信前にバッファリングするかどうかを定義。メモリバッファは送信速度を向上させます。<br />`memory`: メモリにバッファ。EMQXノード再起動時にメッセージは失われます。<br />`disk`: ディスクにバッファ。EMQXノード再起動後もメッセージは保持されます。<br />`hybrid`: 初めはメモリにバッファし、一定サイズ（`segment_bytes`設定参照）に達すると徐々にディスクへオフロードします。メモリモード同様、ノード再起動時にメッセージは失われます。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafkaパーティション毎の最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ領域を確保します。<br />メモリ使用量とパフォーマンスのバランス調整に役立ちます。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが`disk`または`hybrid`の場合に適用。メッセージ格納用のセグメントファイルサイズを制御し、ディスクストレージの最適化に影響します。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが`memory`の場合に適用。高メモリ圧迫時に古いバッファメッセージを自動破棄し、過剰メモリ使用によるシステム不安定化を防止します。<br />Linux環境でのみ有効です。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ネットワーク送受信性能最適化のためソケットバッファサイズを管理します。 | `1024` KB          |
| TCP Keepalive                             | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間無通信による接続切断を防止します。値はカンマ区切りの3つの数値（Idle, Interval, Probes）で指定します：<br />Idle: サーバーがキープアライブプローブを送信開始するまでのアイドル秒数（Linuxデフォルト7200秒）。<br />Interval: 各キープアライブプローブ間の秒数（Linuxデフォルト75秒）。<br />Probes: 応答なしと判断するまでの最大プローブ数（Linuxデフォルト9回）。<br />例：`240,30,5` は240秒アイドル後にプローブ開始、30秒間隔で送信、5回応答なしで接続切断。 | `none`             |
| Max Linger Time                           | パーティション毎のプロデューサーがバッチ収集のためメッセージ待機する最大時間。デフォルトの `0` は待機なし。メモリバッファ以外では `5ms` に設定するとIOPSが大幅削減されますがレイテンシは増加します。 | `0` ミリ秒         |
| Max Linger Bytes                          | パーティション毎のプロデューサーがバッチ収集のためメッセージ待機する最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状況をチェックする間隔。                           | `15` 秒            |

## さらに詳しく

EMQXはApache Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [MQTTとKafkaでつなぐコネクテッドビークルのストリーミングデータパイプライン：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka：IoTデータ統合の強化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [EMQX Cloudルールエンジンを使ったデバイスデータのKafkaへのブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloudルールエンジンに関する動画で、将来的により適切な動画に差し替え予定）
