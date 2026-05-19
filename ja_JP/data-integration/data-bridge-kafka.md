# Apache Kafka に MQTT データをストリームする

[Apache Kafka](https://kafka.apache.org/) は、アプリケーションやシステム間でのリアルタイムなデータストリーム転送を処理できる、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafka はエッジ IoT 通信向けに設計されておらず、Kafka クライアントは安定したネットワーク接続とより多くのハードウェアリソースを必要とします。IoT の分野では、デバイスやアプリケーションから生成されるデータは軽量な MQTT プロトコルを使って送信されます。EMQX の Kafka との統合により、ユーザーは MQTT データを Kafka にシームレスにストリームインまたはストリームアウトできます。MQTT のデータストリームは Kafka トピックに取り込まれ、リアルタイム処理、保存、分析が可能になります。逆に、Kafka トピックのデータは MQTT デバイスによって消費され、タイムリーなアクションを実現します。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQX と Kafka 間のデータ統合について包括的に紹介し、データ統合の作成および検証方法を実践的に説明します。

## 動作概要

Apache Kafka とのデータ統合は、MQTT ベースの IoT データと Kafka の強力なデータ処理能力のギャップを埋めるために EMQX に標準搭載された機能です。組み込みの[ルールエンジン](./rules.md)コンポーネントにより、両プラットフォーム間のデータストリーミングと処理が簡素化され、複雑なコーディングは不要です。

下図は、自動車 IoT で利用される EMQX と Kafka 間のデータ統合の典型的なアーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

Apache Kafka へデータを流入または流出させるには、それぞれ Kafka Sink（Kafka へメッセージを送信）と Kafka Source（Kafka からメッセージを受信）を作成する必要があります。ここでは Sink を例に、その動作フローを説明します。

1. **メッセージのパブリッシュと受信**: 接続された車両上の IoT デバイスは MQTT プロトコルを通じて EMQX に正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQX がこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**: ブローカーと一体化した組み込みのルールエンジンにより、これらの MQTT メッセージはトピックマッチングルールに基づいて処理されます。メッセージが到着するとルールエンジンを通過し、定義されたルールを評価します。ペイロード変換が指定されていれば、データフォーマットの変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Kafka へのブリッジング**: ルールエンジンで定義されたルールがトリガーされると、メッセージは Kafka に転送されます。Kafka ブリッジ機能を使い、MQTT トピックは事前定義された Kafka トピックにマッピングされ、処理済みのメッセージとデータは Kafka トピックに書き込まれます。

車両データが Kafka に取り込まれた後は、以下のように柔軟にデータを活用できます。

- サービスは Kafka クライアントと直接連携し、特定トピックからリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を実行可能です。
- Kafka Streams を利用してストリーム処理を行い、車両状態をメモリ上で集約・相関させてリアルタイム監視ができます。
- Kafka Connect コンポーネントを使い、MySQL や ElasticSearch など外部システムへデータを出力して保存するための各種コネクターを選択できます。

## 特長とメリット

Apache Kafka とのデータ統合は、ビジネスに以下の特長とメリットをもたらします。

- **信頼性の高い双方向 IoT データメッセージング**: 不安定なモバイルネットワーク上で動作するリソース制約のある IoT デバイスと Kafka 間のデータ通信は、不確実なネットワークでのメッセージングに優れた MQTT プロトコルで処理されます。EMQX は MQTT メッセージをバッチで Kafka に転送するだけでなく、バックエンドシステムからの Kafka メッセージをサブスクライブして接続中の IoT クライアントに配信します。
- **ペイロード変換**: メッセージペイロードは送信中に定義された SQL ルールで処理可能です。例えば、総メッセージ数、成功／失敗配信数、メッセージレートなどのリアルタイムメトリクスを含むペイロードは、Kafka に取り込まれる前にデータ抽出、フィルタリング、強化、変換を経ることができます。
- **効果的なトピックマッピング**: 多数の IoT ビジネストピックを Kafka トピックにマッピング可能です。EMQX は MQTT ユーザープロパティを Kafka ヘッダーにマッピングし、1対1、1対多、多対多の柔軟なトピックマッピング方式をサポートし、MQTT トピックフィルター（ワイルドカード）にも対応しています。
- **柔軟なパーティション選択戦略**: MQTT トピックやクライアントに基づいて同一 Kafka パーティションへメッセージを転送することをサポートします。
- **高スループット環境での処理能力**: EMQX Kafka プロデューサーは同期・非同期書き込みモードの両方をサポートし、リアルタイム優先と性能優先のデータ書き込み戦略を区別可能で、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。
- **ランタイムメトリクス**: 各 Sink と Source の総メッセージ数、成功／失敗数、現在のレートなどのランタイムメトリクスを閲覧可能です。
- **動的設定**: Dashboard または設定ファイルで Sink と Source を動的に設定できます。

これらの特長は統合能力と柔軟性を高め、効果的かつ堅牢な IoT プラットフォームアーキテクチャの構築を支援します。増加する IoT データは安定したネットワーク接続下で送信され、効率的に保存・管理されます。

## はじめる前に

このセクションでは、EMQX Dashboard で Kafka Sink と Source を作成する前に必要な準備について説明します。

### 前提条件

- EMQX のデータ統合[ルール](./rules.md)の知識
- [データ統合](./data-bridges.md)の知識

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

詳細な操作手順は、[Kafka ドキュメントのクイックスタート](https://kafka.apache.org/documentation/#quickstart)を参照してください。

### Kafka トピックの作成

EMQX でデータ統合を作成する前に、関連する Kafka トピックを作成する必要があります。以下のコマンドで Kafka に `testtopic-in`（Sink 用）と `testtopic-out`（Source 用）の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka プロデューサーコネクターの作成

Kafka Sink アクションを追加する前に、EMQX と Kafka 間の接続を確立するため Kafka プロデューサーコネクターを作成する必要があります。

1. EMQX Dashboard にアクセスし、**Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例として `my-kafka` とします。名前は Kafka Sink とコネクターを関連付けるために使われ、クラスター内で一意である必要があります。

4. Kafka への接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。ここでは EMQX と Kafka をローカルマシンで動作させている前提です。リモート環境の場合は適宜調整してください。

   - **Authentication**: Kafka クラスターの認証方式を選択します。以下の方式をサポートしています。

     - `None`: 認証なし。
     - `AWS IAM for MSK`: EMQX が EC2 インスタンス上にデプロイされている場合の AWS MSK クラスター用。
     - `Basic Auth`: **mechanism**（`plain`、`scram_sha_256`、`scram_sha_512` のいずれか）を選択し、**username** と **password** を入力。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab ファイル**を指定。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS** トグルスイッチをオンにします。TLS 接続の詳細は[外部リソースアクセスの TLS](../network/overview.md#tls-for-external-resource-access)を参照してください。

   - **Advanced Settings**（任意）については[高度な設定](#advanced-configurations)を参照してください。

5. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続が成功するかテストできます。

6. **Create** ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的に Kafka に接続します。次に、このコネクターを基にルールを作成し、Kafka クラスターへデータを転送します。

### 認証方式

EMQX で Kafka コネクターを作成する際、Kafka クラスターのセキュリティ設定に応じて複数の認証方式から選択できます。

- **None**: 認証不要。

- **MSK IAM**: EMQX が Amazon EC2 インスタンス上にデプロイされている場合の Amazon MSK クラスター接続用。

  この方式は、EC2 インスタンスに付与された IAM ポリシーに基づき、AWS EC2 インスタンスメタデータサービスを利用して認証トークンを生成します。

  ::: tip 重要なお知らせ

  MSK IAM 認証は、EMQX が EC2 インスタンス上で動作し MSK クラスターに接続する場合にのみサポートされます。AWS Metadata API に依存しているためです。

  :::

- **Basic Auth**: ユーザー名とパスワードによる認証。

  選択時は以下を入力する必要があります。
  - **Mechanism**: `plain`、`scram_sha_256`、`scram_sha_512` のいずれかを選択。
  - **Username** と **Password**: Kafka クラスター認証用の資格情報。

- **Kerberos**: Kerberos GSSAPI による認証。

  必要な設定は以下の通りです。
  - **Kerberos Principal**: 認証に使用する Kerberos ID。
  - **Kerberos Keytab ファイル**: 非対話認証に使う keytab ファイルのパス。

  ::: tip 重要なお知らせ

  Kerberos keytab ファイルはすべての EMQX ノードで同一パスに配置し、EMQX サービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sink を使ったルールの作成

このセクションでは、MQTT トピック `t/#` からのメッセージを処理し、処理結果を Kafka の `testtopic-in` トピックに送信する Kafka Sink を使ったルールの作成方法を説明します。

1. EMQX Dashboard で **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID を入力します。例として `my_rule` とします。

4. **SQL Editor** に以下の文を入力します。これはトピック `t/#` からの MQTT メッセージを Kafka に転送する例です。

   注意: 独自の SQL 文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールの学習とテストが可能です。

   :::

   ::: tip

   EMQX v5.7.2 からルール SQL で環境変数を読み込む機能が追加されました。詳細は[ルール SQL で環境変数を使う](#use-environment-variables)を参照してください。

   :::

5. + **Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。**Type of Action** ドロップダウンリストから `Kafka Producer` を選択し、**Action** はデフォルトの `Create Action` のままにするか、既存の Kafka Producer アクションを選択します。この例では新規プロデューサーアクションを作成してルールに追加します。

6. Sink の名前と説明を対応するテキストボックスに入力します。

7. **Connector** ドロップダウンから先ほど作成した `my-kafka` コネクターを選択します。隣のボタンをクリックしてポップアップで新規コネクターを素早く作成することも可能です。設定パラメータは[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

8. Sink のデータ送信方法を設定します。

   - **Kafka Topic**: `testtopic-in` と入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートします。詳細は[変数テンプレートの利用](#use-variable-templates)を参照してください。

   - **Kafka Headers**: Kafka メッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは **Kafka Header Value Encod Type** ドロップダウンから選択可能です。**Add** をクリックしてキー・バリューを追加できます。

   - **Message Key**: Kafka メッセージのキー。プレーン文字列または `${var}` を含む文字列を入力可能です。

   - **Message Value**: Kafka メッセージの値。プレーン文字列または `${var}` を含む文字列を入力可能です。

   - **Partition Strategy**: プロデューサーが Kafka パーティションにメッセージを分配する方法を選択します。

   - **Compression**: Kafka メッセージのレコードを圧縮／解凍する圧縮アルゴリズムを指定します。

9. **Fallback Actions**（任意）: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **Advanced Settings**（任意）については[高度な設定](#advanced-configuration)を参照してください。

11. **Create** をクリックして Sink の作成を完了します。作成後、ページは **Create Rule** に戻り、新規 Sink がルールアクションに追加されます。

12. **Create** をクリックしてルールの作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules** ページで新規ルールを確認でき、**Actions(Sink)** タブで新規 Kafka プロデューサー Sink も確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを表示できます。トポロジーから、トピック `t/#` のメッセージがルール `my_rule` によって解析され Kafka に送信・保存されていることが直感的に把握できます。

![Kafka_producer_bridge](./assets/Kafka_producer_bridge.png)

### Kafka 動的トピックの設定

EMQX v5.7.2 以降、Kafka プロデューサー Sink の設定で環境変数や変数テンプレートを使い、Kafka トピックを動的に設定できます。本節ではこれら2つのユースケースを紹介します。

#### 環境変数の利用

EMQX v5.7.2 では、ルールエンジンの SQL 処理段階で [環境変数](../configuration/configuration.md#environment-variables)から取得した値をメッセージのフィールドに動的に割り当てる機能が追加されました。この機能はルールエンジンの組み込み SQL 関数の [getenv](../data-integration/rule-sql-builtin-functions.md#system-function) を使い、EMQX の環境変数を取得します。取得した変数の値は SQL 処理結果にセットされます。この機能の応用例として、Kafka Sink のルールアクションで Kafka トピックをルール出力結果のフィールドを参照して設定できます。以下はその例です。

::: tip 注意

システムの他の環境変数の漏洩を防ぐため、ルールエンジンで使用する環境変数名は必ず `EMQXVAR_` という固定プレフィックスを付ける必要があります。例えば、`getenv` 関数で読み込む変数名が `KAFKA_TOPIC` の場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` と設定してください。

:::

1. Kafka を起動し、`testtopic-in` という Kafka トピックを事前作成します。[はじめる前に](#はじめる前に)の手順を参照してください。

2. EMQX を起動し、環境変数を設定します。zip インストールの場合は起動時に直接環境変数を指定可能です。例として Kafka トピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

4. Kafka Sink ルールを設定します。**SQL Editor** に以下の文を入力します。

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

5. SQL テストを有効にし、環境変数の値 `testtopic-in` が正常に取得されていることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka プロデューサー Sink にアクションを追加します。ルールの右側の **Action Outputs** で **Add Action** をクリックします。

   - **Connector**: 先に作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**: SQL ルール出力に基づき変数テンプレート形式 `${kafka_topic}` で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sink を使ったルールの作成](#create-a-rule-with-kafka-sink)を参照して追加設定を行い、最後に **Create** をクリックしてルール作成を完了します。

8. [Kafka プロデューサールールのテスト](#test-kafka-producer-rule)の手順に従い、Kafka へメッセージを送信します。

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

#### 変数テンプレートの利用

**Kafka Topic** フィールドに静的なトピック名を設定する代わりに、変数テンプレートを使って動的にトピックを生成できます。これによりメッセージ内容に基づいて Kafka トピックを構築でき、柔軟なメッセージ処理と振り分けが可能です。例えば、`device-${payload.device}` のように指定すると、特定デバイスからのメッセージをデバイスIDをサフィックスとするトピック（例：`device-1`）に簡単に送信できます。

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

このキーが含まれないとトピックのレンダリングに失敗し、メッセージが復旧不能な形でドロップされます。

また、Kafka 側に `device-1`、`device-2` など解決されるすべてのトピックを事前作成しておく必要があります。テンプレートが Kafka に存在しないトピック名に解決された場合も、メッセージは復旧不能なエラーでドロップされます。

## Kafka プロデューサールールのテスト

Kafka プロデューサールールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/en) を使って EMQX に MQTT メッセージをパブリッシュするクライアントをシミュレートできます。

1. MQTTX でトピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページで Sink 名をクリックし統計情報を確認します。Sink の稼働状況に新規の受信メッセージ数および送信メッセージ数が1件ずつ増えているはずです。

3. 以下のコマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafka コンシューマーコネクターの作成

Kafka Source アクションを追加する前に、EMQX と Kafka 間の接続を確立するため Kafka コンシューマーコネクターを作成する必要があります。

1. EMQX Dashboard で **Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。大文字・小文字の英数字の組み合わせで、例として `my-kafka-source` とします。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。ここでは EMQX と Kafka をローカルマシンで動作させている前提です。リモート環境の場合は適宜調整してください。
   
   - **Authentication**: Kafka クラスターの認証方式を選択します。以下の方式をサポートしています。
   
     - `None`: 認証なし。
     - `authentication_msk_iam`: EMQX が EC2 インスタンス上にデプロイされている場合の AWS MSK クラスター用。
     - `Basic Auth`: **Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512` のいずれか）を選択し、**Username** と **Password** を入力。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab ファイル**を指定。
   
     詳細は[認証方式](#authentication-method)を参照してください。
     
   - 暗号化接続を確立する場合は、**Enable TLS** トグルスイッチをオンにします。TLS 接続の詳細は[外部リソースアクセスの TLS](../network/overview.md#tls-for-external-resource-access)を参照してください。
   
   - **Advanced Settings**（任意）については[高度な設定](#advanced-configuration)を参照してください。
   
6. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続が成功するかテストできます。

7. **Create** をクリックします。関連するルールの作成オプションが表示されます。[Kafka コンシューマーソースを使ったルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## Kafka コンシューマーソースを使ったルールの作成

このセクションでは、設定済みの Kafka コンシューマーソースから転送されたメッセージをさらに処理し、MQTT トピックに再パブリッシュするルールの作成方法を説明します。

### ルール SQL の作成

1. EMQX Dashboard で **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルール ID を入力します。例として `my_rule` とします。

4. Kafka ソース `$bridges/kafka_consumer:<sourceName>` から変換されたメッセージを EMQX に転送する場合、**SQL Editor** に以下の文を入力します。

   注意: 独自の SQL 文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドを `SELECT` 部分に含めてください。Kafka ソースの `SELECT` 文では `ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node` などのフィールドが使用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールの学習とテストが可能です。

### Kafka コンシューマーソースをデータ入力に追加

1. ルール作成画面右側の **Data Inputs** タブを選択し、**Add Input** をクリックします。

2. **Input Type** ドロップダウンリストから **Kafka Consumer** を選択します。**Source** はデフォルトの `Create Source` のままにするか、既存の Kafka コンシューマーソースを選択します。この例では新規ソースを作成してルールに追加します。

3. ソースの名前と説明を対応するテキストボックスに入力します。

4. **Connector** ドロップダウンから先ほど作成した `my-kafka-consumer` コネクターを選択します。隣のボタンをクリックしてポップアップで新規コネクターを素早く作成することも可能です。設定パラメータは[Kafka コンシューマーコネクターの作成](#create-a-kafka-consumer-connector)を参照してください。

5. 以下のフィールドを設定します。

   - **Kafka Topic**: コンシューマーソースがメッセージを受信する Kafka トピックを指定します。
   - **Group ID**: このソースのコンシューマーグループ識別子を指定します。指定しない場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode** と **Value Encoding Mode**: Kafka メッセージのキーと値のエンコードモードを選択します。

6. **Offset Reset Policy**: コンシューマーが Kafka トピックのパーティションを読み始めるオフセットのリセットポリシーを選択します。

   - `latest` を選択すると、コンシューマーは最新のオフセットから読み始め、開始前に生成されたメッセージはスキップされます。
   - `earliest` を選択すると、コンシューマーはパーティションの先頭から読み始め、開始前に生成されたメッセージも含めてすべての履歴データを読みます。

7. **Advanced Settings**（任意）については[高度な設定](#advanced-configuration)を参照してください。

8. **Test Connectivity** をクリックしてソースが Kafka サーバーに接続できるかテストします。

9. **Create** をクリックしてソースの作成を完了します。ルール作成画面の **Data Inputs** タブに新規ソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs** タブを選択し、+ **Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action** ドロップダウンリストから **Republish** を選択します。

3. **Topic** と **Payload** フィールドに再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として `t/1` と `${.}` を入力します。
   - **Topic** フィールドでは `${}` を使って MQTT トピックを動的に指定可能です。例：`t/${key}`（`${}` 内のパラメータは SQL の `SELECT` 文に含まれている必要があります）。

4. **Add** をクリックしてアクションをルールに追加します。

5. ルール作成画面に戻り、**Save** をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka ソースルールのテスト

Kafka ソースとルールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/) を使って EMQX にサブスクライブするクライアントをシミュレートし、Kafka プロデューサーで Kafka トピックにデータを生成します。その後、Kafka からのデータが EMQX によってクライアントがサブスクライブするトピックに再パブリッシュされているか確認します。

1. MQTTX でトピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドで Kafka プロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力を促されます。

3. `{"msg": "Hello EMQX"}` と入力して `testtopic-out` トピックにメッセージを生成し、Enter キーを押します。

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

## 高度な設定

本節では、データ統合のパフォーマンスを最適化し、特定のシナリオに応じて動作をカスタマイズするための高度な設定オプションについて説明します。コネクター、Sink、Source 作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| フィールド名                                | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                 | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータ取得リクエストを送信した際に Kafka トピックが存在しなければ自動作成を許可します。 | `disabled`         |
| Min Metadata Refresh Interval             | クライアントが Kafka ブローカーやトピックのメタデータを更新する最小間隔。小さすぎると Kafka サーバーへの負荷が増大します。 | `3` 秒             |
| Metadata Request Timeout                  | Kafka からメタデータ取得を要求する際の最大待機時間。           | `5` 秒             |
| Connect Timeout                           | TCP 接続確立の最大待機時間。認証有効時は認証時間も含みます。    | `5` 秒             |
| Max Wait Time (Source)                    | Kafka ブローカーからのフェッチ応答を待つ最大時間。              | `1` 秒             |
| Fetch Bytes (Source)                      | Kafka からのフェッチリクエストで取得するバイト数。設定値がメッセージサイズ未満だとフェッチ性能に悪影響があります。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafka バッチ内でメッセージを収集する最大バイト数。Kafka ブローカーのデフォルトは 1MB ですが、EMQX はメッセージエンコードのオーバーヘッドを考慮しやや小さめに設定しています。単一メッセージがこの制限を超える場合は別バッチで送信されます。 | `896` KB           |
| Offset Commit Interval (Source)           | コンシューマーグループごとに送信されるオフセットコミット要求の間隔。 | `5` 秒             |
| Required Acks (Sink)                      | Kafka パーティションリーダーがフォロワーから待つアックの種類：<br />`all_isr`: 全てのインシンクレプリカからのアックを要求。<br />`leader_only`: パーティションリーダーのみからのアックを要求。<br />`none`: Kafka からのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafka プロデューサーがパーティション数増加を検知する間隔。増加検知後、指定の `partition_strategy` に基づき新パーティションにメッセージを振り分けます。 | `60` 秒            |
| Max Inflight (Sink)                       | Kafka プロデューサーがアック受信前に送信可能な最大バッチ数（パーティションごと）。大きいほどスループット向上が期待できますが、1より大きいとメッセージ順序が入れ替わるリスクがあります。未アックのメッセージ数を制御し、負荷バランスを取ります。 | `10`               |
| Query Mode (Source)                       | 非同期または同期クエリモードを選択し、メッセージ送信を最適化。非同期モードでは Kafka 書き込みが MQTT メッセージパブリッシュをブロックしませんが、クライアントが Kafka 到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の最大待機時間。メッセージ送信完了を適時保証し、長時間待機を回避します。同期モード時のみ有効。 | `5` 秒             |
| Buffer Mode (Sink)                        | メッセージ送信前のバッファリング方法。メモリバッファリングは送信速度を向上させます。<br />`memory`: メモリにバッファ。EMQX ノード再起動時にメッセージは失われます。<br />`disk`: ディスクにバッファ。再起動後もメッセージは保持されます。<br />`hybrid`: 初期はメモリにバッファし、一定容量超過時に段階的にディスクにオフロード。メモリモード同様、再起動時にメッセージは失われます。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafka パーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ空間を確保。メモリ使用量と性能のバランスを調整します。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが `disk` または `hybrid` の場合に適用。メッセージ保存用のセグメントファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが `memory` の場合に適用。メモリ圧迫時に古いバッファメッセージを自動破棄し、システムの安定性を確保。Linux システムでのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ソケットバッファサイズを管理し、ネットワーク送信性能を最適化。 | `1024` KB          |
| TCP Keepalive                             | Kafka ブリッジ接続の TCP キープアライブ機能を有効化し、長時間のアイドルによる接続切断を防止。値は `Idle, Interval, Probes` の3つの数値をカンマ区切りで指定。<br />Idle: 接続がアイドル状態でキープアライブプローブ送信開始までの秒数（Linux デフォルト 7200秒）。<br />Interval: プローブ送信間隔（Linux デフォルト 75秒）。<br />Probes: 応答なしで接続切断と判断するまでの最大プローブ数（Linux デフォルト 9回）。<br />例: `240,30,5` は 240秒アイドル後にプローブ開始、30秒間隔で最大5回送信。応答なければ接続切断と判断。 | `none`             |
| Max Linger Time                           | パーティションごとのプロデューサーがメッセージをバッチ収集のために待機する最大時間。デフォルト `0` は待機なし。メモリバッファ以外のモードでは `5ms` に設定すると IOPS を大幅に削減可能だがレイテンシは増加。 | `0` ミリ秒         |
| Max Linger Bytes                          | パーティションごとのプロデューサーがメッセージをバッチ収集のために待機する最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状態をチェックする間隔。                         | `15` 秒            |

## さらに詳しく

EMQX は Apache Kafka とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [MQTT と Kafka を使ったコネクテッドビークルのストリーミングデータパイプライン構築：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTT と Kafka：IoT データ統合の強化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTT パフォーマンスベンチマークテスト：EMQX-Kafka 統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterprise パフォーマンスベンチマークテスト：Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [EMQX Cloud ルールエンジンを使ったデバイスデータの Kafka へのブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloud ルールエンジンに関する動画で、将来的により適切な動画に差し替え予定）
