# Stream MQTT Data into Apache Kafka

[Apache Kafka](https://kafka.apache.org/) は、高スループットかつリアルタイムのデータ処理を目的とした広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafka クライアントは安定したネットワーク接続と高いシステムリソースを必要とするため、エッジ IoT 通信には適していません。IoT シナリオでは、デバイスは一般的に軽量な MQTT プロトコルを使用して、不安定なネットワーク上でも効率的にデータを送信します。

EMQX は MQTT と Kafka/[Confluent](https://www.confluent.io/) を統合し、IoT デバイスとバックエンドシステム間のシームレスなデータストリーミングを実現します。MQTT メッセージは Kafka トピックに取り込まれ、リアルタイム処理、保存、分析に利用される一方、Kafka トピックからのデータは MQTT クライアントに配信され、タイムリーなアクションをトリガーできます。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは EMQX と Kafka のデータ統合について紹介し、統合の作成と検証の手順を解説します。

## 動作概要

Apache Kafka とのデータ統合は EMQX に組み込まれた機能であり、MQTT ベースの IoT データを Kafka にストリーミングして下流の処理や分析を可能にします。組み込みの [ルールエンジン](./rules.md) を活用することで、カスタムコードなしにデータのフィルタリング、変換、ルーティングが可能です。

以下の図は、自動車 IoT シナリオにおける典型的な EMQX–Kafka 統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->

Apache Kafka へのデータの流入または流出には、Kafka Sink（Kafka へメッセージを送信）または Kafka Source（Kafka からメッセージを受信）を作成します。以下は Kafka Sink のワークフローです。

1. **メッセージ取り込み**: 車両に接続された IoT デバイスは EMQX に MQTT 接続を確立し、状態データを含むメッセージを定期的にパブリッシュします。EMQX がメッセージを受信すると、ルールエンジンでルールマッチングが開始されます。
2. **ルールベース処理**: マッチしたルールは、ペイロードのフィルタリング、変換、強化などを行います。
3. **Kafka へのデータ転送**: ルールエンジンで定義されたルールは、Kafka へメッセージを転送するアクションをトリガーします。Kafka Sink を使用して MQTT トピックを事前定義された Kafka トピックにマッピングし、処理済みのメッセージとデータを Kafka トピックに書き込みます。

Kafka にデータが取り込まれた後は、以下のように複数の方法で消費および処理が可能です。

- バックエンドサービスが Kafka トピックからリアルタイムデータストリームを直接消費。
- Kafka Streams によるリアルタイム集約、相関、分析。
- Kafka Connect による MySQL や Elasticsearch など外部システムへのデータ転送とさらなる処理。

## 特長とメリット

Apache Kafka とのデータ統合は以下の特長とメリットを提供します。

- **信頼性の高い双方向 IoT データメッセージング**: EMQX は不安定なネットワーク環境下でも MQTT メッセージを確実に Kafka に転送し、バックエンドシステムからの Kafka メッセージを接続された IoT クライアントに配信します。
- **ペイロード変換**: メッセージは Kafka に転送される前に SQL ルールでフィルタリング、強化、変換が可能です。
- **柔軟なトピックマッピング**: MQTT トピックやユーザープロパティを Kafka トピックやヘッダーに柔軟にマッピング可能で、1対1、1対多、ワイルドカードベースのマッピングをサポートします。
- **柔軟なパーティション選択戦略**: MQTT トピックやクライアントに基づき、同一 Kafka パーティションへのメッセージ転送が可能です。
- **高スループット処理**: 同期および非同期の Kafka 書き込みをサポートし、レイテンシとスループットのバランスをワークロードに応じて調整可能です。
- **ランタイムメトリクス**: 各 Sink および Source の総メッセージ数、成功・失敗数、現在のレートなどのランタイムメトリクスを閲覧可能です。
- **動的設定**: ダッシュボードまたは設定ファイルから Sink と Source の動的な設定が可能です。

これらの機能により、効率的なデータ取り込みと管理を備えたスケーラブルかつレジリエントな IoT データプラットフォームを構築できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Kafka Sink と Source を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### Kafka サーバーのセットアップ

ここでは macOS を例にインストールと起動手順を示します。以下のコマンドで Kafka をインストールし、起動できます。

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# KRaft モードで Kafka を起動
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

詳細な操作手順は [Kafka ドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/) を参照してください。

### Kafka トピックの作成

EMQX でデータ統合を作成する前に、Kafka に関連するトピックを作成しておく必要があります。以下のコマンドで Kafka に `testtopic-in`（Sink 用）と `testtopic-out`（Source 用）の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka プロデューサーコネクターの作成

Kafka Sink アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka プロデューサーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例: `my-kafka`。名前は Kafka Sink とコネクターを紐付けるために使用され、クラスター内で一意である必要があります。

4. Kafka への接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。デモでは EMQX と Kafka をローカルで動作させている想定です。リモート環境の場合は適宜調整してください。

   - **Authentication**: Kafka クラスターの認証方式を選択します。以下の方式がサポートされています。

     - `None`: 認証なし。
     - `AWS IAM for MSK`: Amazon EC2 上にデプロイされた EMQX から Amazon MSK クラスターへ接続する場合に使用。
     - `OAuth`: OAuth 2.0 ベースの認証で、OAuth または OIDC 対応 Kafka クラスターに接続。
     - `Basic Auth`: ユーザー名とパスワードによる認証。メカニズムは `plain`、`scram_sha_256`、`scram_sha_512` から選択。
     - `Kerberos`: Kerberos (GSSAPI) 認証。Kerberos プリンシパルとキータブファイルの指定が必要。

     詳細は [Authentication Method](#authentication-method) を参照してください。

   - 暗号化接続を確立する場合は **Enable TLS** のトグルをオンにします。TLS 接続の詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
   - **Request Timeout**: Kafka からの応答待機時間（秒）を指定します。デフォルトは `30` 秒です。タイムアウト超過時は接続を再確立します。値が小さすぎると Kafka がリクエストを受理しても応答を遅延させる場合があり、EMQX が再接続後に同じバッチを再送することで重複メッセージや過剰な下流データが発生する可能性があります。

   - **Advanced Settings**（任意）: [Advanced Configurations](#advanced-configurations) を参照してください。

5. **Create** をクリックする前に、**Test Connection** を押して Kafka サーバーへの接続が成功するか確認できます。

6. **Create** をクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的に Kafka に接続します。次に、このコネクターを基にルールを作成し、Kafka クラスターへのデータ転送を設定します。

### 認証方式

EMQX で Kafka コネクターを作成する際、Kafka クラスターのセキュリティ設定に応じて以下の認証方式から選択できます。

- **None**: 認証なし。

- **MSK IAM**: Amazon EC2 上の EMQX から Amazon MSK クラスターへ接続する場合に使用。

  AWS EC2 インスタンスメタデータサービスを利用し、インスタンスに付与された IAM ポリシーに基づく認証トークンを生成します。

  ::: tip 重要

  MSK IAM 認証は、EMQX が EC2 インスタンス上で稼働し MSK クラスターに接続する場合のみサポートされます。これは EC2 インスタンスメタデータサービスに依存するためです。

  `iptables` や `nftables` によるホストレベルのアウトバウンドフィルタリングを行う場合、`169.254.169.254` へのアクセスをブロックしないでください。EMQX は MSK IAM 認証のためにインスタンスメタデータサービスへアクセスする必要があります。同様の例外は、S3、S3 Tables、DynamoDB、Kinesis など EC2 メタデータから認証情報を取得する AWS ベースの他のコネクターにも適用されます。詳細は [Mitigate SSRF with Rule Engine Policy and Firewall Rules](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。

  :::

- **OAuth**: OAuth 2.0 ベースの認証で、OAuth または OIDC 対応の Kafka クラスター（Confluent Cloud や OAuth 有効化済みのセルフマネージド Kafka など）に接続します。

  EMQX は OAuth 2.0 クライアントとして動作し、OAuth 認可サーバーからアクセストークンを定期的に取得し、SASL/OAUTHBEARER メカニズムで Kafka ブローカーに認証します。

  必要なパラメータは以下の通りです。

  - **OAuth Grant Type**: アクセストークン取得に使用する OAuth 2.0 グラントタイプ（現状は `client_credentials` のみ対応）。
  - **OAuth Token Endpoint URI**: OAuth/OIDC プロバイダーのトークンエンドポイント URI。
  - **OAuth Client ID**: OAuth 認可サーバーに登録されたクライアント ID。
  - **OAuth Client Secret**: トークン取得時に EMQX を認証するためのクライアントシークレット。
  - **OAuth Request Scope**: （任意）トークンリクエストに含めるスコープ。
  - **SASL Extensions**: （高度な設定、任意）認証時に SASL 拡張として送信する追加のキー・バリュー。Confluent Cloud など一部 Kafka プロバイダーで必要となるメタデータ（例: `logicalCluster`、`identityPoolId`）を渡すために使用します。

  Confluent Cloud における OAuth/OIDC 認証の詳細は [公式ドキュメント](https://docs.confluent.io/cloud/current/security/authenticate/workload-identities/identity-providers/oauth/overview.html) を参照してください。

- **Basic Auth**: ユーザー名とパスワードによる認証。

  選択時は以下を指定します。

  - **Mechanism**: `plain`、`scram_sha_256`、`scram_sha_512` のいずれか。
  - **Username** と **Password**: Kafka クラスター認証用の資格情報。

- **Kerberos**: Kerberos GSSAPI 認証。

  指定が必要な項目は以下の通りです。

  - **Kerberos Principal**: 認証に使用する Kerberos プリンシパル。
  - **Kerberos Keytab File**: 非対話認証に使用するキータブファイルのパス。

  ::: tip 重要

  Kerberos キータブファイルはすべての EMQX ノードで同一パスに配置し、EMQX サービスユーザーに読み取り権限が必要です。

  :::

## Kafka Sink を使ったルールの作成

このセクションでは、MQTT トピック `t/#` からメッセージを処理し、Kafka Sink を介して Kafka の `testtopic-in` トピックに送信するルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルール ID を入力します。例: `my_rule`

4. **SQL Editor** に以下のステートメントを入力します。これは MQTT トピック `t/#` のメッセージを Kafka に転送する例です。

   注意: 独自の SQL を指定する場合は、Sink で必要なすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Try It Out** をクリックして SQL ルールを学習・テストできます。

   :::

   ::: tip

   EMQX v5.7.2 からはルール SQL 内で環境変数を読み取る機能が追加されました。詳細は [Use Environment Variables in Rule SQL](#use-environment-variables) を参照してください。

   :::

5. **Create Rule** ページで + **Add Action** をクリックし、ルールの出力を定義します。

6. **Type of Action** ドロップダウンから `Kafka Producer` を選択します。

   **Action** ドロップダウンはデフォルトの `Create Action` のままにします。

   > 既存の Sink を選択することも可能ですが、本例では新規作成します。

7. **Name** と任意で **Description** を入力します。

8. **Connector** ドロップダウンから先ほど作成した `my-kafka` コネクターを選択します。必要に応じて新規作成も可能です。[Create a Kafka Producer Connector](#create-a-kafka-producer-connector) を参照してください。

9. Sink のデータ送信方法を設定します。

      - **Kafka Topic**: メッセージをパブリッシュする Kafka トピック。`testtopic-in` と入力します。EMQX v5.7.2 以降は動的トピック設定もサポートします。詳細は [Use Variable Templates](#use-variable-templates) を参照してください。
      - **Kafka Headers**: Kafka メッセージに追加する任意のキー・バリュー形式のメタデータ。ヘッダー値はオブジェクトとして解決される必要があります。エンコード方式は **Kafka Header Value Encode Type** で選択でき、複数ヘッダーは **Add** で追加可能です。
      - **Message Key**: Kafka メッセージのキー。パーティション割り当てやメッセージ順序に使用されます。静的文字列または `${.clientid}` などのプレースホルダーが利用可能です。
      - **Message Value**: Kafka メッセージのペイロード。テンプレートからレンダリングされます。静的文字列または `${.}` などのプレースホルダーでルールコンテキストから動的生成可能です。テンプレートが `NULL`（例: フィールド未存在）を返す場合は空文字列ではなく Kafka の `NULL` 値が生成されます。
      - **Message Timestamp**: Kafka メッセージのタイムスタンプ。固定値または `${timestamp}` などのプレースホルダーでルール出力から動的設定可能です。
      - **Partition Strategy**: メッセージを Kafka パーティションに振り分ける方法を選択します。
      - **Partitions Limit**: プロデューサーがメッセージを送信できる最大パーティション数を制限します。有効化時は全パーティションではなく指定数のパーティションにのみメッセージを分配します。
      - **Compression**: Kafka メッセージのレコード圧縮／解凍に使用する圧縮アルゴリズムを指定します。

10. **Fallback Actions**（任意）: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [Fallback Actions](./data-bridges.md#fallback-actions) を参照してください。

11. **Advanced Settings**（任意）: [Advanced Configuration](#advanced-configuration) を参照してください。

12. **Create** をクリックして Sink の作成を完了します。作成後は **Create Rule** ページに戻り、新規 Sink がルールアクションに追加されます。

13. **Create** をクリックしてルール全体の作成を完了します。

![kafka_producer_bridge](./assets/kafka_producer_bridge.png)

これでルールの作成が完了し、**Integration** -> **Rules** ページで新規ルールを確認でき、**Actions(Sink)** タブで新規 Kafka プロデューサー Sink を確認できます。

また、**Integration** -> **Flow Designer** でトポロジーを表示可能です。トポロジーからは、トピック `t/#` のメッセージがルール `my_rule` によって解析され Kafka に送信・保存されていることが直感的に把握できます。

### Kafka 動的トピックの設定

EMQX v5.7.2 以降、Kafka プロデューサー Sink 設定で環境変数や変数テンプレートを利用して Kafka トピックを動的に設定できます。本節ではこれら2つのユースケースを紹介します。

#### 環境変数の利用

EMQX v5.7.2 では、ルール SQL の処理フェーズで [環境変数](../configuration/configuration.md#environment-variables) の値を動的にメッセージのフィールドに割り当てる機能が追加されました。この機能はルールエンジンの組み込み SQL 関数の [getenv](../data-integration/rule-sql-builtin-functions.md#system-function) を使い、EMQX の環境変数を取得して SQL 処理結果に設定します。

この機能の応用例として、Kafka Sink ルールアクションの Kafka トピック設定にルール出力のフィールドを参照して動的にトピックを指定できます。以下はその例です。

::: tip 注意

ルールエンジンで使用する環境変数名は、他のシステム環境変数の漏洩を防ぐために必ず `EMQXVAR_` という固定プレフィックスを付ける必要があります。例えば `getenv` 関数で読み取る変数名が `KAFKA_TOPIC` の場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` としてください。

:::

1. Kafka を起動し、`testtopic-in` という Kafka トピックを事前作成します。手順は [はじめる前に](#はじめる前に) を参照してください。

2. EMQX を起動し、環境変数を設定します。zip インストールの場合は起動時に直接指定可能です。例として Kafka トピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。詳細は [Kafka プロデューサーコネクターの作成](#kafka-プロデューサーコネクターの作成) を参照してください。

4. Kafka Sink ルールを設定します。**SQL Editor** に以下のステートメントを入力します。

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

5. SQL テストを有効にし、環境変数 `testtopic-in` が正常に取得できていることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka プロデューサー Sink のアクションを追加します。ルールの右側の **Action Outputs** で **Add Action** をクリックします。

   - **Connector**: 先に作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**: SQL ルール出力の変数テンプレート形式 `${kafka_topic}` を指定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sink を使ったルールの作成](#kafka-sink-を使ったルールの作成) を参考に追加設定を行い、最後に **Create** をクリックしてルール作成を完了します。

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

#### 変数テンプレートの利用

**Kafka Topic** フィールドに静的なトピック名を設定する代わりに、変数テンプレートを使って動的にトピック名を生成できます。これによりメッセージ内容に基づいて Kafka トピックを構築でき、柔軟なメッセージ処理と振り分けが可能です。

例えば、`device-${payload.device}` のように指定すれば、特定デバイスからのメッセージを `device-1` のようにデバイスIDをサフィックスに持つトピックに簡単に送信できます。

この例では、Kafka に送信されるメッセージペイロードに `device` キーが含まれている必要があります。例:

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

このキーがないとトピックのレンダリングに失敗し、メッセージが回復不能な形でドロップされます。

また、Kafka には解決されるすべてのトピック（例: `device-1`、`device-2` など）を事前作成しておく必要があります。存在しないトピック名に解決された場合も、メッセージは回復不能エラーでドロップされます。

## Kafka プロデューサールールのテスト

Kafka プロデューサールールが期待通りに動作するか確認するため、[MQTTX](https://mqttx.app/en) を使ってクライアントが EMQX に MQTT メッセージをパブリッシュする動作をシミュレートできます。

1. MQTTX でトピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページで Sink 名をクリックし統計情報を確認します。Sink の稼働状況に新規の受信メッセージ数と送信メッセージ数が1件ずつ増えているはずです。

3. 以下のコマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafka コンシューマーコネクターの作成

Kafka Source アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka コンシューマーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例: `my-kafka-source`。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。デモでは EMQX と Kafka をローカルで動作させている想定です。リモート環境の場合は適宜調整してください。

   - **Authentication**: Kafka クラスターの認証方式を選択します。以下の方式がサポートされています。

     - `None`: 認証なし。
     - `authentication_msk_iam`: AWS MSK クラスターに EC2 上の EMQX から接続する場合に使用。
     - `OAuth`: [OAuth 2.0](https://oauth.net/2/) を使った認証。
     - `Basic Auth`: **Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**Username** と **Password** を指定。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab File** を指定。

     詳細は [Authentication Method](#authentication-method) を参照してください。

   - 暗号化接続を確立する場合は **Enable TLS** のトグルをオンにします。TLS 接続の詳細は **TLS for External Resource Access** を参照してください。

   - **Advanced Settings**（任意）: [Advanced Configuration](#advanced-configuration) を参照してください。

6. **Create** をクリックする前に、**Test Connection** を押して Kafka サーバーへの接続が成功するか確認できます。

11. **Create** をクリックします。関連するルールの作成オプションが表示されます。[Create a Rule with Kafka Consumer Source](#create-a-rule-with-kafka-consumer-source) を参照してください。

## Kafka コンシューマーソースを使ったルールの作成

このセクションでは、Kafka コンシューマーソースから転送されたメッセージをさらに処理し、MQTT トピックに再パブリッシュするルールの作成方法を示します。

### ルール SQL の作成

1. EMQX ダッシュボードで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルール ID を入力します。例: `my_rule`

4. Kafka ソース `$bridges/kafka_consumer:<sourceName>` から変換されたメッセージを EMQX に転送する場合、**SQL Editor** に以下のステートメントを入力します。

   注意: 独自の SQL を指定する場合は、後続の再パブリッシュアクションで必要なフィールドを `SELECT` 部分に含めてください。Kafka ソースの `SELECT` 文では `ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node` などのフィールドが利用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   注意: 初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールを学習・テストできます。

### Kafka コンシューマーソースをデータ入力として追加

1. ルール作成画面の右側にある **Data Inputs** タブを選択し、**Add Input** をクリックします。

2. **Input Type** ドロップダウンから **Kafka Consumer** を選択します。**Source** ドロップダウンはデフォルトの `Create Source` のままか、既存の Kafka コンシューマーソースを選択します。本デモでは新規作成します。

3. ソースの名前と説明を入力します。

4. **Connector** ドロップダウンから先ほど作成した `my-kafka-consumer` コネクターを選択します。必要に応じて隣のボタンから新規作成も可能です。[Create a Kafka Consumer Connector](#kafka-コンシューマーコネクターの作成) を参照してください。

5. 以下のフィールドを設定します。

   - **Kafka Topic**: コンシューマーソースがサブスクライブする Kafka トピックを指定。
   - **Group ID**: このソースのコンシューマーグループ識別子。未指定の場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode** と **Value Encoding Mode**: Kafka メッセージのキーと値のエンコード方式を選択。

7. **Offset Reset Policy**: コンシューマーが Kafka トピックパーティションのどこから読み始めるかのポリシーを選択。

   - `latest`: コンシューマー開始前に生成されたメッセージをスキップし、最新のオフセットから読み始めます。
   - `earliest`: コンシューマー開始前のメッセージも含め、パーティションの先頭から読み始めます。すなわちトピックの全履歴データを読みます。

8. **Advanced Settings**（任意）: [Advanced Configuration](#advanced-configuration) を参照してください。

9. **Create** をクリックする前に、**Test Connectivity** を押して Kafka サーバーへの接続を確認できます。

10. **Create** をクリックしてソース作成を完了します。ルール作成画面の **Data Inputs** タブに新規ソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs** タブを選択し、+ **Add Action** をクリックしてルールトリガー時のアクションを定義します。

2. **Type of Action** ドロップダウンから **Republish** を選択します。

3. **Topic** と **Payload** フィールドに再パブリッシュしたい MQTT トピックとペイロードを入力します。例として `t/1` と `${.}` を入力します。

   - **Topic** フィールドには `${}` を使って動的に MQTT トピックを指定することも可能です。例: `t/${key}`（`${}` 内のパラメータは SQL の `SELECT` 文に含める必要があります）。

4. **Add** をクリックしてアクションをルールに追加します。

5. ルール作成画面に戻り、**Save** をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka ソースルールのテスト

Kafka ソースとルールが期待通りに動作するか確認するため、[MQTTX](https://mqttx.app/) を使って EMQX のトピックをサブスクライブするクライアントをシミュレートし、Kafka プロデューサーで Kafka トピックにデータを生成します。その後、Kafka からのデータが EMQX によってクライアントがサブスクライブするトピックに再パブリッシュされるか確認します。

1. MQTTX でトピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドで Kafka プロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力待ちになります。

3. `{"msg": "Hello EMQX"}` と入力し、`testtopic-out` トピックにメッセージを生成します。

4. MQTTX のサブスクリプションで以下の Kafka からのメッセージを受信できるはずです。

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

このセクションでは、データ統合のパフォーマンス最適化やシナリオに応じた動作カスタマイズのための高度な設定オプションを説明します。コネクター、Sink、Source 作成時に **Advanced Settings** を展開し、ビジネス要件に応じて以下の設定を行えます。

| 項目                                      | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                 | （プロデューサーコネクターのみ）有効化すると、クライアントがメタデータ取得リクエスト時に存在しない Kafka トピックを自動作成可能。 | `disabled`         |
| Min Metadata Refresh Interval             | クライアントが Kafka ブローカーとトピックのメタデータを更新する最小間隔。小さすぎると Kafka サーバー負荷が増加する可能性あり。 | `3` 秒             |
| Metadata Request Timeout                  | Kafka からメタデータを要求する際の最大待機時間。               | `5` 秒             |
| Connect Timeout                           | TCP 接続確立の最大待機時間（認証時間含む）。                   | `5` 秒             |
| Max Wait Time (Source)                    | Kafka ブローカーからのフェッチ応答待機の最大時間。             | `1` 秒             |
| Fetch Bytes (Source)                      | Kafka からのフェッチリクエストで取得するバイト数。設定値がメッセージサイズ未満の場合、フェッチ性能に悪影響が出る可能性あり。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafka バッチ内で収集するメッセージの最大サイズ（バイト）。Kafka ブローカーのデフォルトは 1MB。EMQX はオーバーヘッドを考慮しやや小さめに設定。単一メッセージが上限超過の場合は別バッチで送信。 | `896` KB           |
| Offset Commit Interval (Source)           | コンシューマーグループごとにオフセットコミットリクエストを送る間隔。 | `5` 秒             |
| Required Acks (Sink)                      | Kafka パーティションリーダーがフォロワーから待つアックの種類。<br />`all_isr`: 全てのインシンクレプリカからのアック。<br />`leader_only`: パーティションリーダーのみ。<br />`none`: アック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafka プロデューサーがパーティション数増加を検知する間隔。増加時は指定の `partition_strategy` に基づき新パーティションにメッセージを分配。 | `60` 秒            |
| Max Inflight (Sink)                       | Kafka プロデューサーがアック受信前に送信可能な最大バッチ数（パーティションごと）。大きいほどスループット向上。ただし 1 超過時はメッセージ順序乱れのリスクあり。 | `10`               |
| Query Mode (Source)                       | 非同期または同期のクエリモードを選択し、メッセージ送信を最適化。非同期は MQTT パブリッシュをブロックしないが、クライアントが Kafka 到着前にメッセージを受信する可能性あり。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期モード時の最大待機時間。メッセージ送信完了を保証し長時間待機を防止。同期モード時のみ有効。 | `5` 秒             |
| Buffer Mode (Sink)                        | メッセージ送信前のバッファリング方式。<br />`memory`: メモリバッファ。EMQX 再起動で消失。<br />`disk`: ディスクバッファ。再起動後も保持。<br />`hybrid`: メモリバッファ上限到達後にディスクへオフロード。メモリモード同様再起動で消失。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafka パーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄して空き確保。メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが `disk` または `hybrid` の場合に適用。メッセージ保存用セグメントファイルのサイズ。ディスクストレージ最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが `memory` の場合に適用。メモリ圧迫時に古いバッファメッセージを自動破棄し、システム安定性を確保。Linux システムのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ソケットバッファサイズの管理。ネットワーク送受信性能の最適化に利用。 | `1024` KB          |
| TCP Keepalive                             | Kafka ブリッジ接続の TCP キープアライブ設定。長時間アイドル時の接続維持に利用。`Idle, Interval, Probes` の3つの数値をカンマ区切りで指定。Linux のデフォルトは `7200,75,9`。例: `240,30,5` は 240秒アイドル後にキープアライブ開始、30秒間隔で送信、5回応答なしで切断。 | `none`             |
| Max Batch Age (Sink)                      | プロデューサーバッファ内のメッセージが送信されずに保持可能な最大時間。すべてのメッセージがこの時間を超えるとバッチは破棄される。キューイング中や切断時のバッファメッセージに適用。破棄されたメッセージは `dropped.expired` メトリクスにカウント。デフォルトは `infinity`（期限なし）。バッファオーバーフロー時もメッセージは破棄される可能性あり。 | `infinity`         |
| Max Retries (Sink)                        | Kafka がリトライ可能なエラーを返した場合の最大リトライ回数。初回試行とリトライ全て失敗するとバッチ破棄、メッセージは `failed` メトリクスにカウント。接続喪失による再送はリトライ回数に含まれず、`max_batch_age` によって制限。デフォルトは `infinity`（無制限）。 | `infinity`         |
| Reconnect Delay (Sink)                    | 接続喪失後にプロデューサーが Kafka に再接続を試みるまでの待機時間。切断中もメッセージはバッファに蓄積される（バッファ制限と `max_batch_age` の範囲内）。デフォルトは `2` 秒。 | `2` 秒             |
| Max Linger Time                           | パーティションごとのプロデューサーがメッセージをバッチにまとめる最大待機時間。すべてのバッファモードに適用。デフォルト `0` は待機なしでレイテンシ最適化。小さな遅延を許容するとリクエスト数削減可能。ディスクバッファの場合はバッチ書き込み前に待機。ディスク IOPS 削減のため最低 `5ms` 推奨。 | `0` ミリ秒         |
| Max Linger Bytes                          | パーティションごとのプロデューサーがバッチ送信を開始する最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状況チェック間隔。                             | `15` 秒            |

## さらに詳しく

EMQX は Apache Kafka とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [Building Connected Vehicle Streaming Data Pipelines with MQTT and Kafka: A 3-Minute Guide](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTT with Kafka: Supercharging IoT Data Integration](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTT Performance Benchmark Testing: EMQX-Kafka Integration](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterprise Performance Benchmark Testing: Kafka Integration](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [Bridge device data to Kafka using the EMQX Cloud Rule Engine](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloud ルールエンジンに関する動画。今後より適切な動画に差し替え予定）
