# Apache Kafka への MQTT データストリーミング

[Apache Kafka](https://kafka.apache.org/) は、アプリケーションやシステム間のデータストリームをリアルタイムに転送できる、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafka はエッジ IoT 通信向けに設計されておらず、Kafka クライアントは安定したネットワーク接続とより多くのハードウェアリソースを必要とします。IoT の領域では、デバイスやアプリケーションから生成されるデータは軽量な MQTT プロトコルを使って送信されます。EMQX の Kafka との統合により、ユーザーは MQTT データをシームレスに Kafka へ、または Kafka からストリーミングできます。MQTT のデータストリームは Kafka のトピックに取り込まれ、リアルタイムの処理、保存、分析を実現します。逆に、Kafka のトピックデータは MQTT デバイスに配信され、タイムリーなアクションを可能にします。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQX と Kafka 間のデータ統合について包括的に紹介し、データ統合の作成と検証方法について実践的な手順を提供します。

## 動作概要

Apache Kafka とのデータ統合は、MQTT ベースの IoT データと Kafka の強力なデータ処理機能のギャップを埋めるために EMQX に標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントを活用することで、両プラットフォーム間のデータストリーミングと処理が簡素化され、複雑なコーディングを不要にします。

以下の図は、自動車 IoT における EMQX と Kafka 間の典型的なデータ統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->
Apache Kafka へのデータの流入および流出には、それぞれ Kafka Sink（Kafka へメッセージを送信）と Kafka Source（Kafka からメッセージを受信）を作成する必要があります。ここでは Sink を例に、処理の流れを説明します。

1. **メッセージのパブリッシュと受信**：接続された車両の IoT デバイスは MQTT プロトコルを通じて EMQX に正常に接続し、定期的に状態データを含むメッセージを MQTT でパブリッシュします。EMQX がこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：組み込みのルールエンジンはブローカーと一体のコンポーネントとして動作し、トピックマッチングルールに基づいて MQTT メッセージを処理します。メッセージが到着するとルールエンジンを通過し、定義されたルールを評価します。ペイロード変換が指定されている場合は、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Kafka へのブリッジ**：ルールエンジンで定義されたルールは、メッセージを Kafka に転送するアクションをトリガーします。Kafka ブリッジ機能を使い、MQTT トピックを事前定義された Kafka トピックにマッピングし、処理済みのメッセージとデータを Kafka トピックに書き込みます。

車両データが Kafka に取り込まれた後は、以下のように柔軟にデータへアクセスし活用できます。

- サービスは Kafka クライアントと直接連携し、特定トピックからリアルタイムのデータストリームを消費してカスタマイズされたビジネス処理を実行可能です。
- Kafka Streams を利用してストリーム処理を行い、車両状態をメモリ上で集約・相関させてリアルタイム監視を実施できます。
- Kafka Connect コンポーネントを利用し、MySQL や ElasticSearch などの外部システムへデータを出力して保存できます。

## 特長とメリット

Apache Kafka とのデータ統合は、以下の特長とメリットをビジネスにもたらします。

- **信頼性の高い双方向 IoT データメッセージング**：Kafka と不安定なモバイルネットワーク上で動作するリソース制限のある IoT デバイス間のデータ通信は、不確実なネットワークでのメッセージングに優れた MQTT プロトコルで処理されます。EMQX は MQTT メッセージをバッチで Kafka に転送するだけでなく、バックエンドシステムからの Kafka メッセージをサブスクライブして接続された IoT クライアントに配信します。
- **ペイロード変換**：メッセージペイロードは送信中に定義された SQL ルールで処理可能です。例えば、総メッセージ数、成功/失敗配信数、メッセージレートなどのリアルタイム指標を含むペイロードは、Kafka への取り込み前にデータ抽出、フィルタリング、強化、変換を経ることができます。
- **効果的なトピックマッピング**：多数の IoT ビジネストピックを Kafka トピックにマッピング可能です。EMQX は MQTT ユーザープロパティの Kafka ヘッダーへのマッピングをサポートし、1対1、1対多、多対多の柔軟なトピックマッピング方式や MQTT トピックフィルター（ワイルドカード）にも対応しています。
- **柔軟なパーティション選択戦略**：MQTT トピックやクライアントに基づき、同一 Kafka パーティションへのメッセージ転送をサポートします。
- **高スループット環境での処理能力**：EMQX Kafka プロデューサーは同期および非同期の書き込みモードをサポートし、リアルタイム優先とパフォーマンス優先のデータ書き込み戦略を区別可能です。シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。
- **ランタイムメトリクス**：各 Sink および Source の総メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスを閲覧可能です。
- **動的設定**：Dashboard または設定ファイルで Sink と Source を動的に設定できます。

これらの特長により、効果的かつ堅牢な IoT プラットフォームアーキテクチャの構築が可能となり、増大する IoT データを安定したネットワーク接続下で送信し、さらに効率的に保存・管理できます。

## はじめる前に

このセクションでは、EMQX Dashboard で Kafka Sink と Source を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

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

詳細な操作手順は、[Kafka ドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/)を参照してください。

### Kafka トピックの作成

EMQX でデータ統合を作成する前に、関連する Kafka トピックを作成してください。以下のコマンドで Kafka に `testtopic-in`（Sink 用）と `testtopic-out`（Source 用）の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka プロデューサーコネクターの作成

Kafka Sink アクションを追加する前に、EMQX と Kafka 間の接続を確立するための Kafka プロデューサーコネクターを作成する必要があります。

1. EMQX Dashboard にアクセスし、**Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例として `my-kafka` とします。名前は Kafka Sink とコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. Kafka への接続に必要なパラメータを設定します：
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。デモでは EMQX と Kafka をローカルマシンで実行している想定です。リモート環境の場合は設定を適宜調整してください。

   - **Authentication**: Kafka クラスターの認証方式を選択します。以下の方法をサポートしています：

     - `None`: 認証なし。
     - `AWS IAM for MSK`: EMQX を EC2 インスタンス上にデプロイし、AWS MSK クラスターと連携する場合。
     - `Basic Auth`: **mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**username** と **password** を提供。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab ファイル**を指定。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS** トグルをオンにします。TLS 接続の詳細は[外部リソースアクセスの TLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。

   - **Request Timeout**: Kafka からの応答を待つ最大時間（秒）を指定します。デフォルトは `30` 秒です。タイムアウトを超えると EMQX は接続を古いと判断して再接続します。値が小さすぎると、Kafka がリクエストを受け入れても応答を遅延させ、再接続後に同じバッチを再送する可能性があり、重複メッセージや下流の過剰なデータ量を招きます。

   - **Advanced Settings**（オプション）：[高度な設定](#advanced-configuration)を参照してください。

5. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続が成功するか確認できます。

6. **Create** ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的に Kafka に接続します。次に、このコネクターを基にルールを作成し、Kafka クラスターへデータを転送します。

### 認証方式

EMQX で Kafka コネクターを作成する際、Kafka クラスターのセキュリティ設定に応じて複数の認証方式から選択可能です。

- **None**：認証不要。

- **MSK IAM**：EMQX を Amazon EC2 インスタンス上にデプロイし、Amazon MSK クラスターに接続する場合に使用。

  この方式は、EC2 インスタンスのメタデータサービスを利用して IAM ポリシーに基づく認証トークンを生成します。

  ::: tip 重要なお知らせ

  MSK IAM 認証は、EMQX が EC2 インスタンス上で実行され、MSK クラスターに接続する場合のみサポートされます。これは EC2 インスタンスのメタデータサービスに依存するためです。

  `iptables` や `nftables` によるホストレベルのアウトバウンドフィルタリングを適用する場合、`169.254.169.254` への通信をブロックしないでください。EMQX は MSK IAM 認証に必要な資格情報を取得するためにメタデータサービスにアクセスする必要があります。同様の例外は、S3、S3 Tables、DynamoDB、Kinesis などの AWS ベースの他のコネクターにも適用されます。[ルールエンジンポリシーとファイアウォールルールによる SSRF 対策](../../guides/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。

  :::

- **Basic Auth**：ユーザー名とパスワードによる認証。

  選択時は以下を指定する必要があります：
  - **Mechanism**：`plain`、`scram_sha_256`、`scram_sha_512` のいずれかを選択。
  - **Username** と **Password**：Kafka クラスター認証用の資格情報。

- **Kerberos**：Kerberos GSSAPI による認証。

  必要な設定：
  - **Kerberos Principal**：認証に使用する Kerberos ID。
  - **Kerberos Keytab ファイル**：非対話認証に用いるキータブファイルのパス。

  ::: tip 重要なお知らせ

  Kerberos キータブファイルはすべての EMQX ノードで同一パスに配置し、EMQX サービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sink を使ったルールの作成

ここでは、MQTT トピック `t/#` からのメッセージを処理し、Kafka Sink を使って Kafka の `testtopic-in` トピックに送信するルールの作成方法を示します。

1. EMQX Dashboard で **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID を入力します。例：`my_rule`

4. **SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` の MQTT メッセージを Kafka に転送する例です。

   注意：独自の SQL 文を指定する場合は、Sink で必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** をクリックし、**Enable Test** を使って SQL ルールを学習・テストできます。

   :::

   ::: tip

   EMQX v5.7.2 からルール SQL 内で環境変数を読み取る機能が追加されました。詳細は[ルール SQL で環境変数を使う](#use-environment-variables)を参照してください。

   :::

5. + **Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンから `Kafka Producer` を選択し、**Action** はデフォルトの `Create Action` のままにするか、既存の Kafka Producer アクションを選択します。本例では新規作成します。

6. Sink の名前と説明を入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-kafka` コネクターを選択します。隣のボタンをクリックしてポップアップで新規コネクターを素早く作成することも可能です。設定パラメータは[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

8. Sink のデータ送信方法を設定します：

   - **Kafka Topic**：`testtopic-in` と入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートします。詳細は[変数テンプレートの使用](#use-variable-templates)を参照してください。

   - **Kafka Headers**：Kafka メッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは **Kafka Header Value Encod Type** ドロップダウンから選択可能です。**Add** をクリックしてキー・バリューのペアを追加できます。

   - **Message Key**：Kafka メッセージのキー。純粋な文字列または `${var}` を含む文字列を入力可能です。

   - **Message Value**：Kafka メッセージの値。こちらも純粋な文字列または `${var}` を含む文字列を入力可能です。

   - **Partition Strategy**：プロデューサーが Kafka パーティションにメッセージを分配する方法を選択します。

   - **Compression**：Kafka メッセージのレコードを圧縮/解凍するための圧縮アルゴリズムを指定します。

9. **フォールバックアクション**（任意）：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **高度な設定**（任意）：[高度な設定](#advanced-configuration)を参照してください。

11. **Create** ボタンをクリックして Sink の作成を完了します。作成後、ページは **Create Rule** に戻り、新規 Sink がルールアクションに追加されます。

12. **Create** ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules** ページで新規ルールを確認でき、**Actions(Sink)** タブで新規 Kafka プロデューサー Sink も確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを表示できます。トポロジー上で、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Kafka に送信・保存されている様子を直感的に確認できます。

![Kafka_producer_bridge](./assets/Kafka_producer_bridge.png)

### Kafka 動的トピックの設定

EMQX v5.7.2 以降、Kafka Producer Sink の設定で環境変数や変数テンプレートを使って Kafka トピックを動的に設定できます。本節ではこの2つのユースケースを紹介します。

#### 環境変数の使用

EMQX v5.7.2 では、[環境変数](../../guides/configuration/configuration.md#environment-variables)から取得した値を SQL 処理フェーズ内のフィールドに動的に割り当てる機能が追加されました。この機能はルールエンジンの組み込み SQL 関数の [getenv](./rule-sql-builtin-functions.md#system-function) を利用し、EMQX の環境変数を取得して SQL 処理結果に設定します。この機能の応用例として、Kafka Sink ルールアクションの Kafka トピック設定でルール出力結果のフィールドを参照してトピックを設定できます。以下はそのデモ例です。

::: tip 注意

他のシステム環境変数の漏洩を防ぐため、ルールエンジンで使用する環境変数名は固定プレフィックス `EMQXVAR_` を付ける必要があります。例えば、`getenv` 関数で読み取る変数名が `KAFKA_TOPIC` の場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` と設定してください。

:::

1. Kafka を起動し、`testtopic-in` トピックを事前作成します。[はじめる前に](#before-you-start)の手順を参照してください。

2. EMQX を起動し、環境変数を設定します。zip インストールの場合は起動時に直接環境変数を指定可能です。例として、Kafka トピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

4. Kafka Sink ルールを設定し、**SQL Editor** に以下を入力します。

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

5. SQL テストを有効にし、環境変数値 `testtopic-in` が正常に取得できることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka Producer Sink にアクションを追加します。ルールの右側の **Action Outputs** で **Add Action** をクリックします。

   - **Connector**：先ほど作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**：SQL ルール出力に基づき、変数テンプレート形式 `${kafka_topic}` で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sink を使ったルールの作成](#create-a-rule-with-kafka-sink)を参照して追加設定を行い、最後に **Create** をクリックしてルール作成を完了します。

8. [Kafka プロデューサールールのテスト](#test-kafka-producer-rule)の手順に従い、Kafka へメッセージを送信します。

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -i pub -t t/Connection -q 1 -m 'payload string'
   ```

   メッセージは Kafka の `testtopic-in` トピックで受信されるはずです。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 \
     --topic testtopic-in

   {"payload":"payload string","kafka_topic":"testtopic-in"}
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   ```

#### 変数テンプレートの使用

**Kafka Topic** フィールドに静的なトピック名を設定する以外に、変数テンプレートを使って動的にトピックを生成できます。これにより、メッセージ内容に基づいて Kafka トピックを構築し、柔軟なメッセージ処理と配信が可能です。例えば、`device-${payload.device}` のように指定すると、特定デバイスからのメッセージをデバイスIDをサフィックスに持つトピック（例：`device-1`）に簡単に送信できます。

この例では、Kafka に送信するメッセージのペイロードに `device` キーが含まれている必要があります。以下は例のペイロードです。

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

また、Kafka 側で解決されるすべてのトピック（例：`device-1`、`device-2` など）を事前に作成しておく必要があります。テンプレートで解決されたトピック名が Kafka に存在しない場合も、メッセージは回復不能なエラーで破棄されます。

## Kafka プロデューサールールのテスト

Kafka プロデューサールールが期待通りに動作するかをテストするには、[MQTTX](https://mqttx.app/en) を使ってクライアントが EMQX に MQTT メッセージをパブリッシュするシミュレーションを行います。

1. MQTTX でトピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページで Sink 名をクリックし、統計情報を確認します。Sink の稼働状況を確認し、新規の受信メッセージ数と送信メッセージ数がそれぞれ1件ずつ増えているはずです。

3. 以下のコマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafka コンシューマーコネクターの作成

Kafka Source アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka コンシューマーコネクターを作成する必要があります。

1. EMQX Dashboard にアクセスし、**Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例として `my-kafka-source` とします。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。デモでは EMQX と Kafka をローカルマシンで実行している想定です。リモート環境の場合は設定を適宜調整してください。

   - **Authentication**: Kafka クラスターの認証方式を選択します。以下の方法をサポートしています：

     - `None`: 認証なし。
     - `authentication_msk_iam`: EMQX を EC2 インスタンス上にデプロイし、AWS MSK クラスターと連携する場合。
     - `Basic Auth`: **Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**Username** と **Password** を提供。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab ファイル**を指定。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS** トグルをオンにします。TLS 接続の詳細は**外部リソースアクセスの TLS**を参照してください。

   - **Advanced Settings**（オプション）：[高度な設定](#advanced-configuration)を参照してください。

6. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続が成功するか確認できます。

11. **Create** をクリックします。関連するルール作成のオプションが表示されます。[Kafka コンシューマー Source を使ったルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## Kafka コンシューマー Source を使ったルールの作成

ここでは、設定済みの Kafka コンシューマー Source から転送されたメッセージをさらに処理し、MQTT トピックに再パブリッシュするルールの作成方法を示します。

### ルール SQL の作成

1. EMQX Dashboard で **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID を入力します。例：`my_rule`

4. Kafka Source `$bridges/kafka_consumer:<sourceName>` から変換されたメッセージを EMQX に転送する場合、**SQL Editor** に以下のステートメントを入力します。

   注意：独自の SQL 文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。Kafka Source の `SELECT` 文では、`ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node` などのフィールドが使用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   注意：初心者の方は **SQL Examples** をクリックし、**Enable Test** を使って SQL ルールを学習・テストできます。

### Kafka コンシューマー Source をデータ入力として追加

1. ルール作成ページ右側の **Data Inputs** タブを選択し、**Add Input** をクリックします。

2. **Input Type** ドロップダウンから **Kafka Consumer** を選択します。**Source** ドロップダウンはデフォルトの `Create Source` のままにするか、既存の Kafka コンシューマー Source を選択します。本例では新規作成します。

3. Source の名前と説明を入力します。

4. **Connector** ドロップダウンから先ほど作成した `my-kafka-consumer` コネクターを選択します。隣のボタンをクリックしてポップアップで新規コネクターを素早く作成することも可能です。設定パラメータは[Kafka コンシューマーコネクターの作成](#create-a-kafka-consumer-connector)を参照してください。

5. 以下のフィールドを設定します：

   - **Kafka Topic**：コンシューマー Source がサブスクライブする Kafka トピックを指定します。
   - **Group ID**：この Source のコンシューマーグループ識別子を指定します。未指定の場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode** と **Value Encoding Mode**：Kafka メッセージのキーおよび値のエンコードモードを選択します。

7. **Offset Reset Policy**：Kafka コンシューマーがオフセットを持たない、または無効な場合にどこから読み始めるかのポリシーを選択します。

   - `latest`：最新のオフセットから読み始め、開始前のメッセージはスキップします。
   - `earliest`：パーティションの先頭から読み始め、開始前のメッセージも含めてすべての履歴データを読みます。

8. **高度な設定**（オプション）：[高度な設定](#advanced-configuration)を参照してください。

9. **Create** をクリックする前に、**Test Connectivity** をクリックして Kafka サーバーへの接続が成功するか確認できます。

10. **Create** をクリックして Source の作成を完了します。ルール作成ページの **Data Inputs** タブに新規 Source が表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs** タブを選択し、+ **Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action** ドロップダウンから **Republish** を選択します。

3. **Topic** と **Payload** フィールドに、再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として、トピックに `t/1`、ペイロードに `${.}` を入力します。
   - **Topic** フィールドでは `${}` を使って MQTT トピックを動的に指定可能です。例：`t/${key}`（`${}` 内のパラメータは SQL の `SELECT` 文に含まれている必要があります）。

4. **Add** をクリックしてアクションをルールに追加します。

5. ルール作成ページに戻り、**Save** をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Source ルールのテスト

Kafka Source とルールが期待通りに動作するかをテストするには、[MQTTX](https://mqttx.app/) を使って EMQX でトピックをサブスクライブするクライアントをシミュレートし、Kafka プロデューサーで Kafka トピックにデータを生成します。その後、Kafka からのデータが EMQX によってクライアントがサブスクライブするトピックに再パブリッシュされているか確認します。

1. MQTTX でトピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドで Kafka プロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力待ちになります。

3. `{"msg": "Hello EMQX"}` を入力して `testtopic-out` トピックにメッセージを生成し、Enter キーを押します。

4. MQTTX のサブスクリプションで、Kafka からの以下のメッセージがトピック `t/1` に届いていることを確認します。

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

本セクションでは、データ統合のパフォーマンス最適化やシナリオに応じたカスタマイズのための高度な設定オプションについて説明します。コネクター、Sink、Source 作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| フィールド名                          | 説明                                                         | 推奨値             |
| ------------------------------------ | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation             | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際に Kafka トピックが存在しなければ自動作成を許可します。 | `disabled`         |
| Min Metadata Refresh Interval         | クライアントが Kafka ブローカーおよびトピックのメタデータを更新する最小間隔。小さすぎると Kafka サーバーの負荷が増加します。 | `3` 秒             |
| Metadata Request Timeout              | Kafka からメタデータを要求する際の最大待機時間。                 | `5` 秒             |
| Connect Timeout                       | TCP 接続確立の最大待機時間。認証時間も含みます。                 | `5` 秒             |
| Max Wait Time (Source)                | Kafka ブローカーからのフェッチ応答を待つ最大時間。                 | `1` 秒             |
| Fetch Bytes (Source)                  | Kafka からのフェッチリクエストで取得するバイト数。設定値がメッセージサイズより小さいとフェッチ性能に悪影響があります。 | `896` KB           |
| Max Batch Bytes (Sink)                | Kafka バッチ内で収集するメッセージの最大サイズ（バイト）。Kafka ブローカーのデフォルトは 1MB ですが、EMQX はエンコードオーバーヘッドを考慮しやや小さめに設定。単一メッセージが超過する場合は別バッチで送信。 | `896` KB           |
| Offset Commit Interval (Source)       | コンシューマーグループごとにオフセットコミット要求を送る間隔。     | `5` 秒             |
| Required Acks (Sink)                  | Kafka パーティションリーダーがフォロワーから待つアックの種類：<br />`all_isr`：全インシンクレプリカからのアックを要求。<br />`leader_only`：リーダーのみからのアックを要求。<br />`none`：Kafka からのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafka プロデューサーがパーティション数の増加を検知する間隔。増加検知後、指定の `partition_strategy` に基づき新パーティションをメッセージ送信に組み込みます。 | `60` 秒            |
| Max Inflight (Sink)                   | Kafka プロデューサーがアックを受け取る前に送信可能なバッチ数（パーティション単位）。値が大きいほどスループット向上。ただし 1 超過時はメッセージ順序が入れ替わるリスクあり。 | `10`               |
| Query Mode (Source)                   | 非同期または同期クエリモードを選択し、要件に応じてメッセージ送信を最適化。非同期モードでは Kafka 書き込みが MQTT パブリッシュ処理をブロックしませんが、クライアントが Kafka 到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)      | 同期クエリモード時の最大待機時間。メッセージ送信完了を保証し、長時間待機を防止。同期モード時のみ有効。 | `5` 秒             |
| Buffer Mode (Sink)                   | メッセージ送信前のバッファリング方式。メモリバッファリングは送信速度向上。<br />`memory`：メモリにバッファ。EMQX ノード再起動でメッセージ消失。<br />`disk`：ディスクにバッファ。再起動後もメッセージ保持。<br />`hybrid`：初期はメモリバッファ。一定サイズ超過時にディスクへオフロード。メモリモード同様、再起動で消失。 | `memory`           |
| Per-partition Buffer Limit (Sink)     | Kafka パーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄し新規メッセージ用に空間を確保。メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)             | バッファモードが `disk` または `hybrid` の場合に適用。メッセージ保存用セグメントファイルのサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)     | バッファモードが `memory` の場合に適用。メモリ圧迫時に古いメッセージを自動破棄し、システムの安定性を確保。Linux システムのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size     | ソケットバッファサイズを管理し、ネットワーク送信性能を最適化。       | `1024` KB          |
| TCP Keepalive                       | Kafka ブリッジ接続の TCP キープアライブ機能を有効化し、長時間の非アクティブ状態による接続切断を防止。値は `Idle, Interval, Probes` のカンマ区切り3数値で指定。<br />Idle：接続がアイドル状態となってからキープアライブプローブ送信開始までの秒数（Linux デフォルト 7200 秒）。<br />Interval：プローブ間隔（Linux デフォルト 75 秒）。<br />Probes：応答なしと判断するまでの最大プローブ回数（Linux デフォルト 9 回）。<br />例：`240,30,5,` は、240 秒アイドル後にプローブ開始、30 秒間隔で最大5回送信し応答なしなら接続切断と判断。 | `none`             |
| Max Batch Age (Sink)                | プロデューサーバッファ内のメッセージが送信されずに保持できる最大期間。期間超過したバッチは破棄され、破棄メッセージは `dropped.expired` メトリクスにカウント。デフォルトは `infinity` でメッセージの期限切れを防止。バッファオーバーフロー時は期限に関係なく破棄される。 | `infinity`         |
| Max Retries (Sink)                  | Kafka からリトライ可能なエラー（例：パーティションリーダー変更）応答時の最大リトライ回数。初回試行と全リトライ失敗時はバッチ破棄し、各メッセージは `failed` メトリクスにカウント。明示的な Kafka エラー応答のみリトライ回数を増加。接続喪失による再送はカウントせず、`max_batch_age` に制限される。デフォルトは無制限。 | `infinity`         |
| Reconnect Delay (Sink)              | 接続喪失後にプロデューサーが Kafka に再接続を試みるまでの遅延時間。切断中もメッセージはバッファに蓄積されるが、バッファ制限と `max_batch_age` の影響を受ける。デフォルトは `2` 秒。 | `2` 秒             |
| Max Linger Time                   | パーティションごとのプロデューサーがより大きなバッチを作るために待機する最大時間。全バッファモードに適用。デフォルト `0` は待機なしでレイテンシ最適化。小さな遅延を許容すればリクエスト数削減可能。ディスクバッファ時はバッチ書き込み前の待機時間。ディスク IOPS 削減には最低 `5ms` 推奨。 | `0` ミリ秒         |
| Max Linger Bytes                  | パーティションごとのプロデューサーが待機を終了しバッチ送信する最大バイト数。 | `10` MB            |
| Health Check Interval             | コネクターの稼働状況をチェックする間隔。                           | `15` 秒            |

## さらに詳しく

EMQX は Apache Kafka とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [MQTT と Kafka でつなぐコネクテッドビークルのストリーミングデータパイプライン：3分でわかるガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTT と Kafka：IoT データ統合のパワーアップ](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTT パフォーマンスベンチマークテスト：EMQX-Kafka 統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterprise パフォーマンスベンチマークテスト：Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [EMQX Cloud ルールエンジンを使ったデバイスデータの Kafka へのブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloud ルールエンジンに関する動画で、今後より適切な動画に差し替え予定）
