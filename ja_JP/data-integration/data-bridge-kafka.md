# Apache Kafka へ MQTT データをストリーミングする

[Apache Kafka](https://kafka.apache.org/) は、アプリケーションやシステム間でのデータストリームのリアルタイム転送を処理できる、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafka はエッジ IoT 通信向けに設計されておらず、Kafka クライアントは安定したネットワーク接続とより多くのハードウェアリソースを必要とします。IoT の領域では、デバイスやアプリケーションから生成されるデータは軽量な MQTT プロトコルを使って送信されます。EMQX と Kafka の統合により、ユーザーは MQTT データをシームレスに Kafka へ、または Kafka からストリーミングできます。MQTT のデータストリームは Kafka トピックに取り込まれ、リアルタイムの処理、保存、分析が可能です。逆に、Kafka トピックのデータは MQTT デバイスによって消費され、タイムリーなアクションを実現します。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQX と Kafka 間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に説明します。

## 動作概要

Apache Kafka とのデータ統合は、MQTT ベースの IoT データと Kafka の強力なデータ処理機能のギャップを埋めるために EMQX に標準搭載された機能です。組み込みの [ルールエンジン](./rules.md) コンポーネントにより、両プラットフォーム間のデータストリーミングと処理を簡素化し、複雑なコーディングを不要にします。

下図は、自動車 IoT でよく使われる EMQX と Kafka 間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

Apache Kafka へのデータの流入および流出には、それぞれ Kafka Sink（Kafka へメッセージを送信）と Kafka Source（Kafka からメッセージを受信）を作成する必要があります。ここでは Sink を例に、そのワークフローを説明します。

1. **メッセージのパブリッシュと受信**: 接続された車載 IoT デバイスは MQTT プロトコルを介して EMQX に正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQX がこれらのメッセージを受信すると、ルールエンジン内でマッチング処理が開始されます。
2. **メッセージデータの処理**: ブローカーと一体化した組み込みルールエンジンにより、これらの MQTT メッセージはトピックマッチングルールに基づいて処理されます。メッセージが到着するとルールエンジンを通過し、定義されたルールが評価されます。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Kafka へのブリッジ**: ルールエンジンで定義されたルールは、メッセージを Kafka に転送するアクションをトリガーします。Kafka ブリッジ機能を使い、MQTT トピックは事前定義された Kafka トピックにマッピングされ、処理済みのすべてのメッセージとデータが Kafka トピックに書き込まれます。

車両データが Kafka に取り込まれた後は、柔軟にデータにアクセスし活用できます。

- サービスは Kafka クライアントと直接統合し、特定トピックのリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を実現できます。
- Kafka Streams を利用してストリーム処理を行い、車両状態をメモリ内で集約・相関させてリアルタイム監視が可能です。
- Kafka Connect コンポーネントを使い、MySQL や ElasticSearch などの外部システムへデータを出力して保存できます。

## 特長とメリット

Apache Kafka とのデータ統合は、以下の特長とメリットをビジネスにもたらします。

- **信頼性の高い双方向 IoT データメッセージング**: 不安定なモバイルネットワーク上で動作するリソース制限のある IoT デバイスと Kafka 間のデータ通信は、不確実なネットワークでのメッセージングに優れた MQTT プロトコルで処理されます。EMQX は MQTT メッセージをバッチで Kafka に転送するだけでなく、バックエンドシステムからの Kafka メッセージをサブスクライブして接続された IoT クライアントに配信します。
- **ペイロード変換**: メッセージペイロードは送信時に定義された SQL ルールで処理可能です。例えば、総メッセージ数、成功/失敗配信数、メッセージレートなどのリアルタイムメトリクスを含むペイロードは、データ抽出、フィルタリング、強化、変換を経て Kafka に取り込まれます。
- **効果的なトピックマッピング**: 多数の IoT ビジネストピックを Kafka トピックにマッピング可能です。EMQX は MQTT ユーザープロパティを Kafka ヘッダーにマッピングし、1対1、1対多、多対多の柔軟なトピックマッピング方式をサポートし、MQTT トピックフィルター（ワイルドカード）にも対応します。
- **柔軟なパーティション選択戦略**: MQTT トピックやクライアントに基づいて同じ Kafka パーティションへメッセージを転送できます。
- **高スループット環境での処理能力**: EMQX Kafka プロデューサーは同期・非同期両方の書き込みモードをサポートし、リアルタイム優先や性能優先のデータ書き込み戦略を区別可能で、シナリオに応じてレイテンシとスループットの柔軟なバランス調整ができます。
- **ランタイムメトリクス**: 各 Sink や Source の総メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスを閲覧可能です。
- **動的設定**: ダッシュボードまたは設定ファイルで Sink と Source を動的に設定できます。

これらの機能は統合能力と柔軟性を高め、効果的で堅牢な IoT プラットフォームアーキテクチャの構築を支援します。増加する IoT データを安定したネットワーク接続のもとで送信し、さらに効果的に保存・管理できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Kafka Sink と Source を作成する前に必要な準備を説明します。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### Kafka サーバーのセットアップ

ここでは macOS を例にインストールと起動手順を示します。以下のコマンドで Kafka をインストール・起動できます。

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# KRaft モードで Kafka を起動
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

詳細な手順は [Kafka ドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/) を参照してください。

### Kafka トピックの作成

EMQX でデータ統合を作成する前に、関連する Kafka トピックを作成してください。以下のコマンドで Kafka に `testtopic-in`（Sink 用）と `testtopic-out`（Source 用）の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka プロデューサーコネクターの作成

Kafka Sink アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka プロデューサーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例：`my-kafka`。この名前は Kafka Sink とコネクターを紐付けるために使われ、クラスター内で一意である必要があります。

4. Kafka への接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。なお、本デモは EMQX と Kafka をローカルで動作させる前提です。リモート環境の場合は適宜設定を調整してください。

   - **Authentication**: Kafka クラスターが要求する認証方式を選択します。以下の方式がサポートされています。

     - `None`: 認証なし。
     - `AWS IAM for MSK`: EMQX が EC2 インスタンス上で動作し、AWS MSK クラスターに接続する場合に使用。
     - `Basic Auth`: **mechanism**（`plain`、`scram_sha_256`、`scram_sha_512` のいずれか）を選択し、**username** と **password** を入力。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab ファイル**を指定。

     詳細は [Authentication Method](#authentication-method) を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS** のトグルスイッチをオンにします。TLS 接続の詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。

   - **Advanced Settings**（任意）については [Advanced Configurations](#advanced-configurations) を参照してください。

5. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続が成功するか確認できます。

6. **Create** ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的に Kafka に接続します。次に、このコネクターを使ってデータを Kafka クラスターに転送するルールを作成します。

### 認証方式

EMQX で Kafka コネクターを作成する際、Kafka クラスターのセキュリティ設定に応じて以下の認証方式から選択できます。

- **None**: 認証不要。

- **MSK IAM**: EMQX が Amazon EC2 インスタンス上で動作し、Amazon MSK クラスターに接続する場合に使用。

  この方式は、EC2 インスタンスのメタデータサービスを利用して IAM ポリシーに基づく認証トークンを生成します。

  ::: tip 重要なお知らせ

  MSK IAM 認証は、EMQX が EC2 インスタンス上で MSK クラスターに接続する場合のみサポートされます。EC2 インスタンスのメタデータサービスに依存するためです。

  `iptables` や `nftables` でホストレベルのアウトバウンドフィルタリングを行う場合、`169.254.169.254` へのアクセスをブロックしないでください。EMQX は MSK IAM 認証のためにインスタンスメタデータサービスへアクセスする必要があります。同様の例外は、S3、S3 Tables、DynamoDB、Kinesis など EC2 メタデータから認証情報を取得する他の AWS ベースコネクターにも適用されます。詳細は [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。

  :::

- **Basic Auth**: ユーザー名とパスワードによる認証。

  選択時は以下を指定する必要があります。
  - **Mechanism**: `plain`、`scram_sha_256`、`scram_sha_512` のいずれかを選択。
  - **Username** と **Password**: Kafka クラスター認証用の資格情報。

- **Kerberos**: Kerberos GSSAPI による認証。

  必要な設定は以下の通りです。
  - **Kerberos Principal**: 認証に使用する Kerberos ID。
  - **Kerberos Keytab ファイル**: 非対話認証に用いる keytab ファイルのパス。

  ::: tip 重要なお知らせ

  Kerberos keytab ファイルはすべての EMQX ノードで同一パスに配置し、EMQX サービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sink を使ったルールの作成

このセクションでは、MQTT トピック `t/#` からメッセージを処理し、Kafka Sink を使って Kafka の `testtopic-in` トピックに処理結果を送信するルールの作成方法を説明します。

1. EMQX ダッシュボードで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルール ID（例：`my_rule`）を入力します。

4. **SQL Editor** に以下のステートメントを入力します。これは MQTT トピック `t/#` のメッセージを Kafka に転送する例です。

   注意：独自の SQL 文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** をクリックし、**Enable Test** を使って SQL ルールの学習とテストが可能です。

   :::

   ::: tip

   EMQX v5.7.2 からルール SQL 内で環境変数を読み取る機能が追加されました。詳細は [Use Environment Variables in Rule SQL](#use-environment-variables) を参照してください。

   :::

5. + **Add Action** ボタンをクリックし、トリガーされるアクションを定義します。**Type of Action** ドロップダウンから `Kafka Producer` を選択し、**Action** はデフォルトの `Create Action` のままにするか、既存の Kafka Producer アクションを選択します。本例では新規作成してルールに追加します。

6. Sink の名前と説明を入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-kafka` コネクターを選択します。隣のボタンをクリックしてポップアップで新規コネクターを素早く作成することも可能です。設定パラメータは [Create a Kafka Producer Connector](#create-a-kafka-producer-connector) を参照してください。

8. Sink のデータ送信方法を設定します。

   - **Kafka Topic**: `testtopic-in` と入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートします。詳細は [Use Variable Templates](#use-variable-templates) を参照してください。

   - **Kafka Headers**: Kafka メッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは **Kafka Header Value Encod Type** ドロップダウンから選択可能です。**Add** をクリックしてキー・バリューを追加できます。

   - **Message Key**: Kafka メッセージのキー。純粋な文字列または `${var}` 形式のプレースホルダーを含む文字列を入力可能です。

   - **Message Value**: Kafka メッセージの値。純粋な文字列または `${var}` 形式のプレースホルダーを含む文字列を入力可能です。

   - **Partition Strategy**: プロデューサーが Kafka パーティションにメッセージを分配する方法を選択します。

   - **Compression**: Kafka メッセージのレコードを圧縮/解凍する圧縮アルゴリズムを指定します。

9. **Fallback Actions**（任意）: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [Fallback Actions](./data-bridges.md#fallback-actions) を参照してください。

10. **Advanced Settings**（任意）については [Advanced Configuration](#advanced-configuration) を参照してください。

11. **Create** ボタンをクリックして Sink の作成を完了します。作成後、**Create Rule** ページに戻り、新しい Sink がルールアクションに追加されます。

12. **Create** ボタンをクリックしてルールの作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules** ページに新しいルールが表示され、**Actions(Sink)** タブに新規 Kafka プロデューサー Sink が確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され Kafka に送信・保存されている様子を直感的に確認できます。

![Kafka_producer_bridge](./assets/kafka_producer_bridge.png)

### Kafka 動的トピックの設定

EMQX v5.7.2 以降、Kafka Producer Sink の設定で環境変数や変数テンプレートを使って Kafka トピックを動的に設定できます。本節ではこれら2つのユースケースを紹介します。

#### 環境変数の利用

EMQX v5.7.2 では、ルール SQL の処理段階で [環境変数](../configuration/configuration.md#environment-variables) の値をメッセージのフィールドに動的に割り当てる機能が追加されました。これはルールエンジンの組み込み SQL 関数の [getenv](../data-integration/rule-sql-builtin-functions.md#system-function) を使い、EMQX の環境変数を取得して SQL 処理結果に設定します。この機能の応用例として、Kafka Sink ルールアクションの Kafka トピック設定でルール出力結果のフィールドを参照し、Kafka トピックを設定できます。以下はその例です。

::: tip 注意

ルールエンジンで使用する環境変数名は、他のシステム環境変数の漏洩を防ぐために固定プレフィックス `EMQXVAR_` を付ける必要があります。例えば、`getenv` 関数で読み取る変数名が `KAFKA_TOPIC` の場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` と設定してください。

:::

1. Kafka を起動し、`testtopic-in` という Kafka トピックを事前に作成します。[はじめる前に](#はじめる前に) の手順を参照してください。

2. EMQX を起動し、環境変数を設定します。zip インストールの場合、起動時に直接環境変数を指定可能です。例として Kafka トピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafka プロデューサーコネクターの作成](#kafka-プロデューサーコネクターの作成) を参照してください。

4. Kafka Sink ルールを設定し、**SQL Editor** に以下を入力します。

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

5. SQL テストを有効にし、環境変数の値 `testtopic-in` が正常に取得できることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka Producer Sink にアクションを追加します。ルールの右側の **Action Outputs** で **Add Action** をクリックします。

   - **Connector**: 先ほど作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**: SQL ルール出力に基づき、変数テンプレート形式 `${kafka_topic}` で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. その他の設定は [Kafka Sink を使ったルールの作成](#kafka-sink-を使ったルールの作成) を参照し、最後に **Create** をクリックしてルール作成を完了します。

8. [Kafka プロデューサールールのテスト](#test-kafka-producer-rule) の手順に従い、Kafka にメッセージを送信して動作を確認します。

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -i pub -t t/Connection -q 1 -m 'payload string'
   ```

   Kafka トピック `testtopic-in` でメッセージが受信されるはずです。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 \
     --topic testtopic-in
   
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   ```

#### 変数テンプレートの利用

**Kafka Topic** フィールドに静的なトピック名を設定する代わりに、変数テンプレートを使って動的にトピックを生成できます。これによりメッセージ内容に基づいて Kafka トピックを構築でき、柔軟なメッセージ処理と分配が可能です。例えば、`device-${payload.device}` のように指定すると、特定デバイスからのメッセージをデバイスIDをサフィックスに持つトピック（例：`device-1`）に簡単に送信できます。

この例では、Kafka に送信するメッセージペイロードに `device` キーが含まれている必要があります。以下は例のペイロードです。

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

また、Kafka 側に `device-1`、`device-2` などテンプレートで解決されるすべてのトピックを事前に作成しておく必要があります。存在しないトピック名に解決された場合も、メッセージは回復不能なエラーで破棄されます。

## Kafka プロデューサールールのテスト

Kafka プロデューサールールが期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/en) を使って EMQX に MQTT メッセージをパブリッシュするクライアントをシミュレートします。

1. MQTTX でトピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページで Sink 名をクリックし、統計情報を確認します。Sink の稼働状況に新規受信メッセージ数と新規送信メッセージ数が1件ずつあるはずです。

3. 以下のコマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

## Kafka コンシューマーコネクターの作成

Kafka Source アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka コンシューマーコネクターを作成します。

1. EMQX ダッシュボードで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例：`my-kafka-source`。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。ローカル環境を想定しています。リモート環境の場合は適宜調整してください。

   - **Authentication**: Kafka クラスターの認証方式を選択します。以下がサポートされています。

     - `None`: 認証なし。
     - `authentication_msk_iam`: EMQX が EC2 インスタンスで動作し AWS MSK クラスターに接続する場合。
     - `Basic Auth`: **Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**Username** と **Password** を入力。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab File** を指定。

     詳細は [Authentication Method](#authentication-method) を参照してください。

   - 暗号化接続を確立する場合は **Enable TLS** をオンにします。詳細は **TLS for External Resource Access** を参照してください。

   - **Advanced Settings**（任意）については [Advanced Configuration](#advanced-configuration) を参照してください。

6. **Create** をクリックする前に、**Test Connection** で Kafka への接続を確認できます。

7. **Create** をクリックします。関連ルールの作成オプションが表示されます。[Kafka コンシューマー Source を使ったルールの作成](#create-a-rule-with-kafka-consumer-source) を参照してください。

## Kafka コンシューマー Source を使ったルールの作成

このセクションでは、Kafka コンシューマー Source で転送されたメッセージをさらに処理し、MQTT トピックに再パブリッシュするルールの作成方法を説明します。

### ルール SQL の作成

1. EMQX ダッシュボードで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルール ID（例：`my_rule`）を入力します。

4. Kafka Source `$bridges/kafka_consumer:<sourceName>` から変換されたメッセージを EMQX に転送する場合、**SQL Editor** に以下を入力します。

   注意：独自の SQL 文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドを `SELECT` 部分に含めてください。Kafka Source の `SELECT` 文では `ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node` などのフィールドが利用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   初心者の方は **SQL Examples** をクリックし、**Enable Test** を使って SQL ルールの学習とテストが可能です。

### Kafka コンシューマー Source をデータ入力に追加

1. ルール作成画面の右側の **Data Inputs** タブを選択し、**Add Input** をクリックします。

2. **Input Type** ドロップダウンから **Kafka Consumer** を選択します。**Source** はデフォルトの `Create Source` のままにするか、既存の Kafka Consumer Source を選択します。本例では新規作成してルールに追加します。

3. Source の名前と説明を入力します。

4. **Connector** ドロップダウンから先ほど作成した `my-kafka-consumer` コネクターを選択します。隣のボタンをクリックしてポップアップで新規コネクターを素早く作成することも可能です。設定パラメータは [Kafka コンシューマーコネクターの作成](#kafka-コンシューマーコネクターの作成) を参照してください。

5. 以下のフィールドを設定します。

   - **Kafka Topic**: コンシューマー Source がサブスクライブする Kafka トピックを指定します。
   - **Group ID**: この Source のコンシューマーグループ識別子を指定します。未指定の場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode** と **Value Encoding Mode**: Kafka メッセージのキーと値のエンコードモードを選択します。

6. **Offset Reset Policy**: コンシューマーが Kafka トピックパーティションの読み取り開始位置をリセットするポリシーを選択します。

   - `latest`: コンシューマー開始時点の最新オフセットから読み取り、開始前のメッセージはスキップします。
   - `earliest`: パーティションの先頭から読み取り、開始前のメッセージも含めてすべての履歴データを読みます。

7. **Advanced Settings**（任意）については [Advanced Configuration](#advanced-configuration) を参照してください。

8. **Create** をクリックする前に **Test Connectivity** で Kafka サーバーへの接続確認が可能です。

9. **Create** をクリックして Source の作成を完了します。ルール作成画面の **Data Inputs** タブに新しい Source が表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs** タブを選択し、+ **Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action** ドロップダウンから **Republish** を選択します。

3. **Topic** と **Payload** フィールドに、再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として `t/1` と `${.}` を入力します。
   - **Topic** フィールドでは `${}` を使って動的に MQTT トピックを指定可能です。例：`t/${key}`（`${}` 内のパラメータは SQL の `SELECT` 文に含める必要があります）。

4. **Add** をクリックしてアクションをルールに追加します。

5. ルール作成画面に戻り、**Save** をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Source ルールのテスト

Kafka Source とルールが期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/) を使って EMQX のトピックをサブスクライブするクライアントをシミュレートし、Kafka プロデューサーで Kafka トピックにデータを生成します。次に、Kafka からのデータが EMQX によってクライアントがサブスクライブしたトピックに再パブリッシュされているか確認します。

1. MQTTX でトピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドで Kafka プロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力待ちになります。

3. `{"msg": "Hello EMQX"}` と入力して `testtopic-out` トピックにメッセージを生成し、Enter キーを押します。

4. MQTTX のサブスクリプションで、Kafka からの以下のメッセージがトピック `t/1` で受信されることを確認します。

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

このセクションでは、データ統合のパフォーマンス最適化や特定シナリオに応じたカスタマイズのための高度な設定オプションを説明します。コネクター、Sink、Source 作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                                     | 説明                                                         | 推奨値             |
| ---------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータ取得要求を送信した際に Kafka トピックが存在しなければ自動作成を許可します。 | `disabled`         |
| Min Metadata Refresh Interval            | クライアントが Kafka ブローカーやトピックのメタデータを更新する最小間隔。小さすぎると Kafka サーバーへの負荷が増大します。 | `3` 秒             |
| Metadata Request Timeout                 | Kafka にメタデータ要求を送信した際の最大待機時間。               | `5` 秒             |
| Connect Timeout                          | TCP 接続確立の最大待機時間。認証時間も含みます。                 | `5` 秒             |
| Max Wait Time (Source)                   | Kafka ブローカーからフェッチ応答を待つ最大時間。                 | `1` 秒             |
| Fetch Bytes (Source)                     | Kafka からフェッチするバイト数。設定値が Kafka メッセージサイズ未満だとフェッチ性能に悪影響があります。 | `896` KB           |
| Max Batch Bytes (Sink)                   | Kafka バッチ内でメッセージを収集する最大サイズ（バイト）。Kafka ブローカーのデフォルトは 1MB ですが、EMQX はエンコードオーバーヘッドを考慮しやや小さめに設定。単一メッセージが上限を超える場合は別バッチで送信されます。 | `896` KB           |
| Offset Commit Interval (Source)          | コンシューマーグループごとにオフセットコミット要求を送る間隔。     | `5` 秒             |
| Required Acks (Sink)                     | Kafka パーティションリーダーがフォロワーから受け取る必要があるアックの種類：<br />`all_isr`: 全てのインシンクレプリカからのアックが必要。<br />`leader_only`: パーティションリーダーのみからのアック。<br />`none`: Kafka からのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafka プロデューサーがパーティション数増加を検知する間隔。増加検知後、指定の `partition_strategy` に基づき新パーティションをメッセージ送信に組み込みます。 | `60` 秒            |
| Max Inflight (Sink)                      | Kafka プロデューサーがアック受信前に送信可能な最大バッチ数（パーティションごと）。大きいほどスループット向上。ただし 1 超はメッセージ順序入れ替わりリスクあり。 | `10`               |
| Query Mode (Source)                      | 非同期または同期クエリモードを選択し、メッセージ送信を最適化。非同期モードでは Kafka 書き込みが MQTT パブリッシュ処理をブロックしませんが、クライアントが Kafka 到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の最大待機時間。メッセージ送信完了をタイムリーに保証します。同期モード時のみ有効。 | `5` 秒             |
| Buffer Mode (Sink)                       | メッセージ送信前のバッファリング方式。メモリバッファは高速ですが、EMQX ノード再起動でメッセージ消失。<br />`memory`: メモリバッファ。<br />`disk`: ディスクバッファ。再起動後もメッセージ保持。<br />`hybrid`: メモリバッファから一定容量超過でディスクへオフロード。再起動時はメモリバッファ分消失。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafka パーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄してバッファ空間を確保。メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)                | バッファモードが `disk` または `hybrid` の場合に適用。メッセージ保存用の分割ファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)          | バッファモードが `memory` の場合に適用。メモリ圧迫時に古いバッファメッセージを自動破棄し、システム安定性を確保。Linux システムのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ソケットバッファサイズを管理し、ネットワーク送信性能を最適化。       | `1024` KB          |
| TCP Keepalive                            | Kafka ブリッジ接続の TCP キープアライブを有効化し、長時間の非アクティブ時の接続切断を防止。値は `Idle, Interval, Probes` の3つの数値のカンマ区切りで指定。<br />Idle: サーバーがキープアライブプローブを送るまでのアイドル秒数（Linux デフォルト 7200秒）。<br />Interval: キープアライブプローブ間隔（Linux デフォルト 75秒）。<br />Probes: 反応なしと判断するまでの最大プローブ数（Linux デフォルト 9回）。<br />例：`240,30,5` は 240秒アイドル後にプローブ開始、30秒間隔で最大5回送信し応答なしで切断。 | `none`             |
| Max Linger Time                          | パーティションごとのプロデューサーがバッチ収集のためにメッセージを待つ最大時間。デフォルト `0` は待機なし。メモリバッファ以外で `5ms` に設定すると IOPS が大幅減少するがレイテンシは増加。 | `0` ミリ秒         |
| Max Linger Bytes                         | パーティションごとのプロデューサーがバッチ収集のために待つ最大バイト数。 | `10` MB            |
| Health Check Interval                    | コネクターの稼働状況をチェックする間隔。                           | `15` 秒            |

## さらに詳しく

EMQX は Apache Kafka とのデータ統合に関する多くの学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [MQTT と Kafka を使ったコネクテッドビークルのストリーミングデータパイプライン構築：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTT と Kafka：IoT データ統合の強化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTT パフォーマンスベンチマークテスト：EMQX-Kafka 統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterprise パフォーマンスベンチマークテスト：Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [EMQX Cloud ルールエンジンを使ったデバイスデータの Kafka ブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine) （Cloud ルールエンジンに関する動画で、将来的により適切な動画に差し替え予定）
