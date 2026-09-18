# Stream MQTT Data into Apache Kafka

[Apache Kafka](https://kafka.apache.org/) は、アプリケーションやシステム間でのデータストリームのリアルタイム転送を処理できる広く使われているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafka はエッジIoT通信向けに設計されておらず、Kafka クライアントは安定したネットワーク接続とより多くのハードウェアリソースを必要とします。IoTの領域では、デバイスやアプリケーションから生成されるデータは軽量な MQTT プロトコルを用いて送信されます。EMQX の Kafka/[Kafka](https://www.Kafka.io/) との統合により、ユーザーは MQTT データをシームレスに Kafka へまたは Kafka からストリームできます。MQTT データストリームは Kafka トピックに取り込まれ、リアルタイムの処理、保存、分析が可能です。逆に、Kafka トピックのデータは MQTT デバイスによって消費され、タイムリーなアクションを実現します。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQX と Kafka 間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に解説します。

## 仕組み

Apache Kafka とのデータ統合は、MQTT ベースの IoT データと Kafka の強力なデータ処理機能のギャップを埋めるために EMQX に標準搭載された機能です。組み込みの [ルールエンジン](./rules.md) コンポーネントにより、両プラットフォーム間のデータストリーミングと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は、自動車向け IoT で使われる EMQX と Kafka 間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->
Apache Kafka へのデータの流入・流出には、それぞれ Kafka Sink（Kafka へメッセージを送信）と Kafka Source（Kafka からメッセージを受信）を作成する必要があります。ここでは Sink を例に、そのフローを説明します。

1. **メッセージのパブリッシュと受信**：接続された車両の IoT デバイスは MQTT プロトコルを通じて EMQX に正常に接続し、定期的に状態データを含むメッセージを MQTT でパブリッシュします。EMQX がこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：ブローカーと一体となった組み込みのルールエンジンにより、これらの MQTT メッセージはトピックマッチングルールに基づいて処理されます。メッセージが到着するとルールエンジンを通過し、定義されたルールを評価します。ペイロード変換を指定するルールがあれば、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **Kafka へのブリッジ**：ルールエンジンで定義されたルールがメッセージを Kafka へ転送するアクションをトリガーします。Kafka ブリッジ機能を用いて MQTT トピックは事前定義された Kafka トピックにマッピングされ、処理済みのメッセージとデータは Kafka トピックに書き込まれます。

車両データが Kafka に取り込まれた後は、以下のように柔軟にデータへアクセス・活用できます。

- サービスは Kafka クライアントと直接統合し、特定トピックからリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を実現可能です。
- Kafka Streams を利用してストリーム処理を行い、車両状態をメモリ内で集約・相関付けてリアルタイム監視を実現できます。
- Kafka Connect コンポーネントを使い、MySQL や ElasticSearch など外部システムへのデータ出力コネクターを選択して保存できます。

## 特長と利点

Apache Kafka とのデータ統合は、以下の特長と利点をビジネスにもたらします。

- **信頼性の高い双方向 IoT データメッセージング**：不安定なモバイルネットワーク上で動作するリソース制約のある IoT デバイスと Kafka 間のデータ通信は、不確実なネットワークでのメッセージングに優れた MQTT プロトコルで処理されます。EMQX は MQTT メッセージをバッチで Kafka に転送するだけでなく、バックエンドシステムからの Kafka メッセージをサブスクライブし、接続された IoT クライアントに配信します。
- **ペイロード変換**：メッセージペイロードは転送中に定義された SQL ルールで処理可能です。例えば、総メッセージ数、成功／失敗配信数、メッセージレートなどのリアルタイムメトリクスを含むペイロードは、Kafka 取り込み前にデータ抽出、フィルタリング、強化、変換を経ることができます。
- **効果的なトピックマッピング**：多くの IoT ビジネストピックを Kafka トピックにマッピング可能です。EMQX は MQTT ユーザープロパティを Kafka ヘッダーにマッピングし、1対1、1対多、多対多の柔軟なトピックマッピング方式をサポートし、MQTT トピックフィルター（ワイルドカード）も対応します。
- **柔軟なパーティション選択戦略**：MQTT トピックやクライアントに基づき、同じ Kafka パーティションへメッセージを転送することをサポートします。
- **高スループット環境での処理能力**：EMQX Kafka プロデューサーは同期・非同期の両書き込みモードをサポートし、リアルタイム優先とパフォーマンス優先のデータ書き込み戦略を区別可能で、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。
- **ランタイムメトリクス**：各 Sink および Source の総メッセージ数、成功／失敗数、現在のレートなどのランタイムメトリクスを閲覧可能です。
- **動的設定**：Dashboard または設定ファイルで Sink と Source を動的に設定できます。

これらの機能は統合能力と柔軟性を高め、効果的かつ堅牢な IoT プラットフォームアーキテクチャの構築を支援します。増大する IoT データ量を安定したネットワーク接続下で送信し、さらに効率的に保存・管理できます。

## はじめる前に

このセクションでは、EMQX Dashboard で Kafka Sink と Source を作成する前に必要な準備を説明します。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

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

詳細な操作手順は [Kafka ドキュメントのクイックスタート](https://kafka.apache.org/documentation/#quickstart) を参照してください。

### Kafka トピックの作成

EMQX でデータ統合を作成する前に、関連する Kafka トピックを作成してください。以下のコマンドで Kafka に 2 つのトピック `testtopic-in`（Sink 用）と `testtopic-out`（Source 用）を作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka プロデューサーコネクターの作成

Kafka Sink アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka プロデューサーコネクターを作成する必要があります。

1. EMQX Dashboard にアクセスし、**Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例：`my-kafka`。名前は Kafka Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. Kafka 接続に必要なパラメータを設定します：
   - **Bootstrap Hosts**：`127.0.0.1:9092` を入力します。この例では EMQX と Kafka が同一マシン上で動作している想定です。異なるマシンの場合は実際の Kafka ブローカーアドレスを入力してください。EMQX 5.10.5 以降は、IPv6 アドレスを角括弧で囲んで指定可能です（例：`[::1]:9092`）。

   - **IP Family**：EMQX 5.10.5 以降、Kafka ブローカーへの接続に使用する IP アドレスファミリーを選択できます：

     - **Auto** (`auto`)：デフォルト。IP アドレスの場合はそのファミリーを使用。ホスト名の場合は IPv4 を優先し、失敗時に IPv6 を試行。
     - **IPv4** (`ipv4`)：IPv4 のみで接続。
     - **IPv6** (`ipv6`)：IPv6 のみで接続。

   - **Authentication**：Kafka クラスターの認証方式を選択します。以下の方式をサポートしています：

     - `None`：認証なし。
     - `AWS IAM for MSK`：EMQX が EC2 インスタンス上にデプロイされている場合の AWS MSK クラスター用。
     - `Basic Auth`：**mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**username** と **password** を入力。
     - `Kerberos`：**Kerberos Principal** と **Kerberos Keytab ファイル**を指定。

     詳細は [認証方式](#authentication-method) を参照してください。

   - 暗号化接続を確立したい場合は、**Enable TLS** トグルスイッチをオンにします。TLS 接続の詳細は [外部リソースアクセスの TLS](../../guides/network/overview.md#tls-for-external-resource-access) を参照してください。

   - **詳細設定**（任意）：[詳細設定](#advanced-configuration) を参照してください。

5. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続が成功するかテストできます。

6. **Create** ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的に Kafka に接続します。次に、このコネクターを基にルールを作成し、Kafka クラスターへデータを転送します。

### 認証方式

EMQX で Kafka コネクターを作成する際、Kafka クラスターのセキュリティ設定に応じて複数の認証方式から選択可能です。

- **None**：認証不要。

- **MSK IAM**：EMQX が Amazon EC2 インスタンス上にデプロイされている場合に Amazon MSK クラスターへ接続するための方式です。

  この方式は AWS EC2 インスタンスメタデータサービスを利用し、インスタンスに付与された IAM ポリシーに基づいて認証トークンを生成します。

  ::: tip 重要なお知らせ

  MSK IAM 認証は、EMQX が EC2 インスタンス上で動作し MSK クラスターに接続する場合のみサポートされます。AWS メタデータ API に依存しているためです。

  :::

- **Basic Auth**：ユーザー名とパスワードによる認証です。

  この方式を選択した場合、以下を指定する必要があります：
  - **Mechanism**：`plain`、`scram_sha_256`、`scram_sha_512` のいずれかを選択。
  - **Username** と **Password**：Kafka クラスター認証用の資格情報。

- **Kerberos**：Kerberos GSSAPI による認証です。

  以下を指定する必要があります：
  - **Kerberos Principal**：認証に使用する Kerberos ID。
  - **Kerberos Keytab ファイル**：非対話認証に用いる keytab ファイルのパス。

  ::: tip 重要なお知らせ

  Kerberos keytab ファイルは全ての EMQX ノードで同一パスに配置し、EMQX サービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sink を使ったルールの作成

このセクションでは、MQTT トピック `t/#` からのメッセージを処理し、処理結果を Kafka の `testtopic-in` トピックに送信する Kafka Sink を使ったルールの作成方法を説明します。

1. EMQX Dashboard で **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID を入力します。例：`my_rule`

4. **SQL Editor** に以下の文を入力します。これは MQTT トピック `t/#` からのメッセージを Kafka に転送する例です。

   注意：独自の SQL 文を指定する場合は、Sink で必要な全てのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールを学習・テストできます。

   :::

   ::: tip

   EMQX v5.7.2 からルール SQL 内で環境変数を読み込む機能が追加されました。詳細は [ルール SQL で環境変数を使う](#use-environment-variables) を参照してください。

   :::

5. + **Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Kafka Producer` を選択し、**Action** ドロップダウンはデフォルトの `Create Action` のままか、既存の Kafka Producer アクションを選択します。この例では新規プロデューサーアクションを作成してルールに追加します。

6. Sink の名前と説明を入力します。

7. **Connector** ドロップダウンで先ほど作成した `my-kafka` コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。設定パラメータは [Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector) を参照してください。

8. Sink のデータ送信方法を設定します：

   - **Kafka Topic**：`testtopic-in` を入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートします。詳細は [変数テンプレートの使用](#use-variable-templates) を参照してください。

   - **Kafka Headers**：Kafka メッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。**Kafka Header Value Encod Type** ドロップダウンからヘッダー値のエンコードタイプを選択可能です。**Add** をクリックしてキー・バリューのペアを追加できます。

   - **Message Key**：Kafka メッセージのキーです。純粋な文字列または `${var}` を含む文字列を入力できます。

   - **Message Value**：Kafka メッセージの値です。純粋な文字列または `${var}` を含む文字列を入力できます。

   - **Partition Strategy**：プロデューサーがメッセージを Kafka パーティションに分配する方法を選択します。

   - **Compression**：Kafka メッセージ内のレコードを圧縮／解凍する圧縮アルゴリズムを指定します。

9. **フォールバックアクション**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

10. **詳細設定**（任意）：[詳細設定](#advanced-configuration) を参照してください。

11. **Create** ボタンをクリックして Sink の作成を完了します。作成後、ページは **Create Rule** に戻り、新しい Sink がルールアクションに追加されます。

12. **Create** ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules** ページで新規ルールを確認でき、**Actions(Sink)** タブで新規 Kafka プロデューサー Sink を確認できます。

また、**Integration** -> **Flow Designer** をクリックしてトポロジーを表示できます。トポロジーでは、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Kafka に送信・保存されている様子を直感的に確認できます。

![Kafka_producer_bridge](./assets/Kafka_producer_bridge.png)

### Kafka 動的トピックの設定

EMQX v5.7.2 以降、Kafka Producer Sink 設定で環境変数や変数テンプレートを使って Kafka トピックを動的に設定できます。本節ではこの2つのユースケースを紹介します。

#### 環境変数の使用

EMQX v5.7.2 では、ルール SQL の処理フェーズで [環境変数](../../guides/configuration/configuration.md#environment-variables) の値を動的に割り当てる機能が追加されました。この機能はルールエンジンの組み込み SQL 関数の [getenv](../data-integration/rule-sql-builtin-functions.md#system-function) を使い、EMQX の環境変数を取得して SQL 処理結果に設定します。この機能の応用例として、Kafka Sink ルールアクションの Kafka トピック設定でルール出力結果のフィールドを参照して Kafka トピックを設定できます。以下はその例です。

::: tip 注意

他のシステム環境変数の漏洩を防ぐため、ルールエンジンが使用する環境変数名は固定プレフィックス `EMQXVAR_` を付ける必要があります。例えば `getenv` 関数で読み込む変数名が `KAFKA_TOPIC` の場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` と設定してください。

:::

1. Kafka を起動し、`testtopic-in` という Kafka トピックを事前に作成します。[はじめる前に](#はじめる前に) の手順を参照してください。

2. EMQX を起動し、環境変数を設定します。zip インストールの場合、起動時に直接環境変数を指定可能です。例として Kafka トピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` に設定：

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector) を参照してください。

4. Kafka Sink ルールを設定し、**SQL Editor** に以下を入力します。

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

5. SQL テストを有効にし、環境変数の値 `testtopic-in` が正しく取得されていることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka Producer Sink にアクションを追加します。ルールの右側 **Action Outputs** で **Add Action** をクリックします。

   - **Connector**：先ほど作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**：SQL ルール出力に基づき変数テンプレート形式 `${kafka_topic}` で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sink を使ったルールの作成](#kafka-sink-を使ったルールの作成) を参照して追加設定を完了し、最後に **Create** をクリックしてルール作成を完了します。

8. [Kafka プロデューサールールのテスト](#test-kafka-producer-rule) の手順に従い、Kafka にメッセージを送信します。

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -i pub -t t/Connection -q 1 -m 'payload string'
   ```

   メッセージは Kafka トピック `testtopic-in` で受信されるはずです。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 \
     --topic testtopic-in
   
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   ```

#### 変数テンプレートの使用

**Kafka Topic** フィールドに静的なトピック名を設定する以外に、変数テンプレートを使って動的なトピックを生成できます。これによりメッセージ内容に基づいて Kafka トピックを構築し、柔軟なメッセージ処理・振り分けが可能です。例えば `device-${payload.device}` のような形式を指定すると、特定デバイスからのメッセージをデバイスID付きのトピック（例：`device-1`）に簡単に送信できます。

この例では、Kafka に送信するメッセージペイロードに `device` キーが含まれている必要があります。例：

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

また、Kafka では `device-1`、`device-2` など、テンプレートで解決される全トピックを事前に作成しておく必要があります。存在しないトピック名に解決された場合もメッセージは回復不能なドロップとなります。

## Kafka プロデューサールールのテスト

Kafka プロデューサールールが期待通り動作するかテストするには、[MQTTX](https://mqttx.app/en) を使って EMQX に MQTT メッセージをパブリッシュするクライアントをシミュレートできます。

1. MQTTX でトピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページで Sink 名をクリックし統計情報を確認します。Sink の稼働状況に新規受信メッセージ1件、新規送信メッセージ1件があるはずです。

3. 以下のコマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafka コンシューマーコネクターの作成

Kafka Source アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka コンシューマーコネクターを作成する必要があります。

1. EMQX Dashboard で **Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例：`my-kafka-source`。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**：`127.0.0.1:9092` を入力します。この例では EMQX と Kafka が同一マシン上で動作している想定です。異なるマシンの場合は実際の Kafka ブローカーアドレスを入力してください。EMQX 5.10.5 以降は IPv6 アドレスを角括弧で囲んで指定可能です（例：`[::1]:9092`）。

   - **IP Family**：EMQX 5.10.5 以降、Kafka ブローカーへの接続に使用する IP アドレスファミリーを選択できます：

     - **Auto** (`auto`)：デフォルト。IP アドレスの場合はそのファミリーを使用。ホスト名の場合は IPv4 を優先し、失敗時に IPv6 を試行。
     - **IPv4** (`ipv4`)：IPv4 のみで接続。
     - **IPv6** (`ipv6`)：IPv6 のみで接続。
   
   - **Authentication**：Kafka クラスターの認証方式を選択します。以下の方式をサポートしています：
   
     - `None`：認証なし。
     - `authentication_msk_iam`：EMQX が EC2 インスタンス上にデプロイされている場合の AWS MSK クラスター用。
     - `Basic Auth`：**Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**Username** と **Password** を入力。
     - `Kerberos`：**Kerberos Principal** と **Kerberos Keytab ファイル**を指定。
   
     詳細は [認証方式](#authentication-method) を参照してください。
     
   - 暗号化接続を確立したい場合は、**Enable TLS** トグルスイッチをオンにします。TLS 接続の詳細は **TLS for External Resource Access** を参照してください。
   
   - **詳細設定**（任意）：[詳細設定](#advanced-configuration) を参照してください。
   
6. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続が成功するかテストできます。

11. **Create** をクリックします。関連ルールの作成オプションが表示されます。[Kafka コンシューマー Source を使ったルールの作成](#create-a-rule-with-kafka-consumer-source) を参照してください。

## Kafka コンシューマー Source を使ったルールの作成

このセクションでは、設定済み Kafka コンシューマー Source から転送されたメッセージをさらに処理し、MQTT トピックに再パブリッシュするルールの作成方法を説明します。

### ルール SQL の作成

1. EMQX Dashboard で **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID を入力します。例：`my_rule`

4. Kafka Source `$bridges/kafka_consumer:<sourceName>` から変換されたメッセージを EMQX に転送する場合、**SQL Editor** に以下の文を入力します。

   注意：独自の SQL 文を指定する場合は、後続の再パブリッシュアクションで必要な全フィールドを `SELECT` 部分に含めてください。Kafka Source の `SELECT` 文では `ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node` などのフィールドが使えます。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   注意：初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールを学習・テストできます。

### Kafka コンシューマー Source をデータ入力として追加

1. ルール作成ページ右側の **Data Inputs** タブを選択し、**Add Input** をクリックします。

2. **Input Type** ドロップダウンリストから **Kafka Consumer** を選択します。**Source** ドロップダウンはデフォルトの `Create Source` のままか、既存の Kafka Consumer ソースを選択します。この例では新規コンシューマーソースを作成してルールに追加します。

3. ソースの名前と説明を入力します。

4. **Connector** ドロップダウンで先ほど作成した `my-kafka-consumer` コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。設定パラメータは [Kafka コンシューマーコネクターの作成](#kafka-コンシューマーコネクターの作成) を参照してください。

5. 以下のフィールドを設定します：

   - **Kafka Topic**：コンシューマーソースがサブスクライブする Kafka トピックを指定します。
   - **Group ID**：このソースのコンシューマーグループ識別子を指定します。未指定の場合、ソース名に基づいて自動生成されます。
   - **Key Encoding Mode** と **Value Encoding Mode**：Kafka メッセージのキーと値のエンコードモードを選択します。
7. **Offset Reset Policy**：Kafka コンシューマーがオフセットを持たないか無効な場合に、Kafka トピックパーティションのどこから読み始めるかのポリシーを選択します。

   - `latest`：最新のオフセットから読み始め、コンシューマー開始前のメッセージはスキップします。
   - `earliest`：パーティションの先頭から読み始め、過去の全データを読みます。
8. **詳細設定**（任意）：[詳細設定](#advanced-configuration) を参照してください。
9. **Create** をクリックする前に、**Test Connectivity** をクリックして Kafka サーバーへの接続をテストできます。
10. **Create** をクリックしてソース作成を完了します。ルール作成ページの **Data Inputs** タブに新規ソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs** タブを選択し、+ **Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action** ドロップダウンリストから **Republish** を選択します。

3. **Topic** と **Payload** フィールドに再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として `t/1` と `${.}` を入力します。
   - **Topic** フィールドには `${}` を使って動的に MQTT トピックを指定可能です。例：`t/${key}`（`${}` 内のパラメータは SQL の `SELECT` 文に含まれている必要があります）。
4. **Add** をクリックしてアクションをルールに追加します。

5. ルール作成ページに戻り、**Save** をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Source ルールのテスト

Kafka Source とルールが期待通り動作するかテストするには、[MQTTX](https://mqttx.app/) を使って EMQX のトピックをサブスクライブするクライアントをシミュレートし、Kafka プロデューサーで Kafka トピックにデータを生成します。その後、Kafka からのデータが EMQX によってクライアントがサブスクライブするトピックに再パブリッシュされているか確認します。

1. MQTTX でトピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドで Kafka プロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力を促されます。

3. `{"msg": "Hello EMQX"}` と入力し、`testtopic-out` トピックにメッセージを生成して Enter キーを押します。

4. MQTTX のサブスクリプションを確認します。Kafka からの以下のメッセージがトピック `t/1` で受信されるはずです。

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

このセクションでは、データ統合のパフォーマンス最適化や特定シナリオに応じたカスタマイズのための詳細設定オプションを説明します。コネクター、Sink、Source 作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| フィールド名                              | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                 | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータフェッチ要求を送った際に Kafka トピックが存在しなければ自動作成を許可します。 | `disabled`         |
| Min Metadata Refresh Interval             | クライアントが Kafka ブローカーやトピックのメタデータを更新する最小間隔。小さすぎると Kafka サーバーの負荷が増加する可能性があります。 | `3` 秒             |
| Metadata Request Timeout                  | Kafka からメタデータを要求する際の最大待機時間。                 | `5` 秒             |
| Connect Timeout                           | TCP 接続確立の最大待機時間（認証時間含む）。                     | `5` 秒             |
| Max Wait Time (Source)                    | Kafka ブローカーからのフェッチ応答を待つ最大時間。               | `1` 秒             |
| Fetch Bytes (Source)                      | Kafka からのフェッチリクエストで取得するバイト数。設定値がメッセージサイズより小さいとフェッチ性能に影響します。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafka バッチ内でメッセージを収集する最大バイト数。Kafka ブローカーのデフォルトは 1MB ですが、EMQX はメッセージエンコードのオーバーヘッドを考慮しやや小さめに設定。単一メッセージがこの制限を超える場合は別バッチで送信されます。 | `896` KB           |
| Offset Commit Interval (Source)           | 各コンシューマーグループのオフセットコミットリクエスト間隔。     | `5` 秒             |
| Required Acks (Sink)                      | Kafka パーティションリーダーがフォロワーから受け取る必要があるアックの種類：<br />`all_isr`：全てのインシンクレプリカからのアック。<br />`leader_only`：リーダーのみ。<br />`none`：アック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafka プロデューサーがパーティション数増加を検知する間隔。増加検知後、`partition_strategy` に基づき新パーティションをメッセージ配信に組み込みます。 | `60` 秒            |
| Max Inflight (Sink)                       | Kafka プロデューサーがアック受信前に送信可能な最大バッチ数（パーティション毎）。大きいほどスループット向上。ただし 1 超はメッセージ順序入れ替わりのリスクあり。 | `10`               |
| Query Mode (Source)                       | 非同期または同期クエリモードを選択し、メッセージ送信を最適化。非同期モードでは Kafka 書き込みが MQTT パブリッシュをブロックしませんが、クライアントが Kafka 到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の最大待機時間。メッセージ送信完了を保証し長時間待機を防止。同期モード時のみ有効。 | `5` 秒             |
| Buffer Mode (Sink)                        | メッセージ送信前のバッファリング方式。メモリバッファリングは送信速度向上。<br />`memory`：メモリにバッファ。EMQX ノード再起動でメッセージ消失。<br />`disk`：ディスクにバッファ。再起動後もメッセージ保持。<br />`hybrid`：初めはメモリバッファ、一定サイズ超えたら段階的にディスクへ。メモリモード同様再起動で消失。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafka パーティション毎の最大バッファサイズ（バイト）。上限超過時は古いメッセージを破棄してバッファ空間を確保。メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが `disk` または `hybrid` の場合に適用。メッセージ格納用セグメントファイルのサイズで、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが `memory` の場合に適用。メモリ圧迫時に古いメッセージを自動破棄し、システム安定性を確保。Linux システムのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ソケットバッファサイズを管理しネットワーク送信性能を最適化。       | `1024` KB          |
| TCP Keepalive                             | Kafka ブリッジ接続の TCP キープアライブ機能を有効化し、長時間無通信による接続切断を防止。`Idle, Interval, Probes` の3数値をカンマ区切りで指定。<br />Idle：サーバーがキープアライブプローブを開始するまでのアイドル秒数（Linux デフォルト 7200秒）。<br />Interval：プローブ間隔（Linux デフォルト 75秒）。<br />Probes：応答なしと判断するまでの最大プローブ回数（Linux デフォルト 9回）。<br />例：`240,30,5` は 240秒アイドル後にプローブ開始、30秒間隔で最大5回プローブを送信。 | `none`             |
| Max Linger Time                           | パーティション毎のプロデューサーがバッチ収集のためにメッセージを待つ最大時間。デフォルト `0` は待機なし。メモリバッファ以外では `5ms` に設定すると IOPS が大幅に減少するがレイテンシは増加。 | `0` ミリ秒         |
| Max Linger Bytes                          | パーティション毎のプロデューサーがバッチ収集のために待つ最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状況チェック間隔。                               | `15` 秒            |

## さらに詳しく

EMQX は Apache Kafka とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ：**

- [MQTT と Kafka を使ったコネクテッドビークルのストリーミングデータパイプライン構築：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTT と Kafka：IoT データ統合の強化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTT パフォーマンスベンチマークテスト：EMQX-Kafka 統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterprise パフォーマンスベンチマークテスト：Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画：**

- [EMQX Cloud ルールエンジンを使ったデバイスデータの Kafka へのブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloud ルールエンジンに関する動画で、今後より適切な動画に差し替え予定）
