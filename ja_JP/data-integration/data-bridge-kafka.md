# Apache Kafka へ MQTT データをストリームする

[Apache Kafka](https://kafka.apache.org/) は、高スループットかつリアルタイムのデータ処理を目的とした、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafka クライアントは安定したネットワーク接続と高いシステムリソースを必要とするため、エッジ IoT 通信には適していません。IoT シナリオでは、デバイスが軽量な MQTT プロトコルを用いて、不安定なネットワーク上でも効率的にデータを送信することが一般的です。

EMQX は MQTT と Kafka／[Confluent](https://www.confluent.io/) を統合し、IoT デバイスとバックエンドシステム間のシームレスなデータストリーミングを可能にします。MQTT メッセージは Kafka トピックに取り込まれ、リアルタイム処理、保存、分析に利用される一方で、Kafka トピックからのデータは MQTT クライアントに配信され、タイムリーなアクションをトリガーできます。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQX と Kafka のデータ統合について紹介し、統合の作成と検証手順をステップバイステップで解説します。

## 動作概要

Apache Kafka とのデータ統合は EMQX に組み込まれた機能であり、MQTT ベースの IoT データを Kafka にストリームして下流の処理や分析に活用します。組み込みの[ルールエンジン](./rules.md)を活用することで、カスタムコードなしにデータのフィルタリング、変換、ルーティングが可能です。

以下の図は、自動車 IoT シナリオにおける典型的な EMQX–Kafka 統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->

Apache Kafka へのデータの流入・流出には、それぞれ Kafka Sink（Kafka へメッセージを送信）と Kafka Source（Kafka からメッセージを受信）を作成します。以下は Kafka Sink のワークフローです。

1. **メッセージ取り込み**: 車両に接続された IoT デバイスが EMQX に MQTT 接続を確立し、定期的に状態データを含むメッセージをパブリッシュします。EMQX がメッセージを受信すると、ルールエンジンでルールマッチングを開始します。
2. **ルールベース処理**: マッチしたルールは、ペイロードのフィルタリング、変換、または拡張を行います。
3. **Kafka へのデータ転送**: ルールエンジンで定義されたルールが Kafka への転送アクションをトリガーします。Kafka Sink を使用して MQTT トピックを事前定義された Kafka トピックにマッピングし、処理済みのメッセージとデータを Kafka トピックに書き込みます。

Kafka にデータが取り込まれると、以下のように複数の方法で消費・処理できます。

- バックエンドサービスが Kafka トピックからリアルタイムデータストリームを直接消費。
- Kafka Streams を用いたリアルタイム集計、相関分析、アナリティクス。
- Kafka Connect を介して MySQL や Elasticsearch など外部システムへ転送し、保存やさらなる処理を実施。

## 特長とメリット

Apache Kafka とのデータ統合は以下の特長とメリットを提供します。

- **信頼性の高い双方向 IoT データメッセージング**: EMQX は不安定なネットワーク環境でも MQTT メッセージを確実に Kafka に転送し、Kafka からのメッセージを接続された IoT クライアントに配信します。
- **ペイロード変換**: メッセージは Kafka に転送される前に SQL ルールでフィルタリング、拡張、変換が可能です。
- **効果的なトピックマッピング**: MQTT トピックやユーザープロパティを柔軟に Kafka トピックやヘッダーにマッピングでき、1対1、1対多、ワイルドカードベースのマッピングをサポートします。
- **柔軟なパーティション選択戦略**: MQTT トピックやクライアントに基づき、同じ Kafka パーティションにメッセージを転送可能です。
- **高スループット処理**: 同期・非同期の Kafka 書き込みをサポートし、レイテンシとスループットのバランスをワークロードに応じて調整できます。
- **ランタイムメトリクス**: 各 Sink や Source の総メッセージ数、成功／失敗数、現在のレートなどの実行時メトリクスを閲覧可能です。
- **動的設定**: ダッシュボードまたは設定ファイルから Sink と Source を動的に設定できます。

これらの機能により、効率的なデータ取り込みと管理を備えたスケーラブルでレジリエントな IoT データプラットフォームを構築できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Kafka Sink と Source を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

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

詳細な操作手順は [Kafka ドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/) を参照してください。

### Kafka トピックの作成

EMQX でデータ統合を作成する前に、関連する Kafka トピックを作成しておく必要があります。以下のコマンドで Kafka に `testtopic-in`（Sink 用）と `testtopic-out`（Source 用）の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka プロデューサーコネクターの作成

Kafka Sink アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka プロデューサーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。

2. 画面右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例として `my-kafka` とします。名前は Kafka Sink とコネクターを紐づけるために使用され、クラスター内で一意である必要があります。

4. Kafka 接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。なお、この例では EMQX と Kafka をローカルマシンで起動している前提です。リモート環境の場合は適宜設定を変更してください。

   - **Authentication**: Kafka クラスターが要求する認証方式を選択します。以下の方式がサポートされています。

     - `None`: 認証なし。
     - `AWS IAM for MSK`: EMQX を Amazon EC2 インスタンス上にデプロイし、Amazon MSK クラスターに接続する場合に使用。
     - `MSK IAM Roles Anywhere`: EC2 外の環境（オンプレミスなど）から AWS IAM Roles Anywhere クレデンシャルヘルパーを使って MSK に接続する場合に使用。
     - `OAuth`: OAuth 2.0 ベースの認証を用いて OAuth または OIDC 対応の Kafka クラスターに接続。
     - `Basic Auth`: ユーザー名とパスワードによる認証。メカニズムとして `plain`、`scram_sha_256`、`scram_sha_512` のいずれかを選択。
     - `Kerberos`: Kerberos (GSSAPI) 認証。Kerberos プリンシパルとキータブファイルの指定が必要。

     詳細は [Authentication Method](#authentication-method) を参照してください。

   - 暗号化接続を確立する場合は **Enable TLS** トグルをオンにします。TLS 接続の詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。
   - **Request Timeout**: EMQX が Kafka からの応答を待つ最大時間（秒単位）を指定します。デフォルトは `30` 秒です。タイムアウトを超えると接続が古くなったとみなし再接続します。値が小さすぎると、Kafka はリクエストを受理しても応答を遅延させることがあり、EMQX は再接続後にバッチを再送して重複メッセージや過剰な下流データ量が発生する可能性があります。

   - **Advanced Settings**（任意）: [Advanced Configurations](#advanced-configurations) を参照してください。

5. **Create** をクリックする前に、**Test Connection** をクリックして Kafka への接続が成功するか確認できます。

6. **Create** ボタンをクリックしてコネクター作成を完了します。

作成後、コネクターは自動的に Kafka に接続します。次に、このコネクターを使って Kafka クラスターにデータを転送するルールを作成します。

### 認証方式

EMQX で Kafka コネクターを作成する際、Kafka クラスターのセキュリティ設定に応じて複数の認証方式から選択できます。

- **None**: 認証なし。

- **MSK IAM**: EMQX を Amazon EC2 インスタンス上にデプロイし、Amazon MSK クラスターに接続する場合に使用。

  この方式は、EC2 インスタンスに付与された IAM ポリシーに基づき、EC2 インスタンスメタデータサービスから認証トークンを生成します。

  ::: tip 重要

  MSK IAM 認証は、EMQX が EC2 インスタンス上で動作し MSK クラスターに接続する場合のみサポートされます。これは EC2 インスタンスメタデータサービスに依存するためです。

  `iptables` や `nftables` でホストレベルのアウトバウンドフィルタリングを行う場合、`169.254.169.254` へのアクセスをブロックしないでください。EMQX は MSK IAM 認証のためにインスタンスメタデータサービスへアクセスする必要があります。同様の例外は S3、S3 Tables、DynamoDB、Kinesis など EC2 メタデータからクレデンシャルを取得する AWS ベースのコネクターにも適用されます。詳細は [ルールエンジンポリシーとファイアウォールルールによる SSRF 緩和](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。

  :::

- **MSK IAM Roles Anywhere**: EC2 以外の環境（オンプレミスなど）から AWS IAM Roles Anywhere クレデンシャルヘルパーを使って MSK クラスターに接続する場合に使用。

  この方式では、クレデンシャルヘルパーが `serve` モードで動作し HTTP API を EMQX に公開します。EMQX はこの API から一時的な AWS クレデンシャルを取得し、SASL/OAUTHBEARER トークンを生成して MSK IAM 認証を行います。

  必須パラメータ:

  - **Roles Anywhere Endpoint**: クレデンシャルヘルパーの API エンドポイント。スキームとポートを含む完全な HTTP URL を指定します（例: `http://127.0.0.1:9911`）。
  - **AWS Region**: MSK クラスターが稼働する AWS リージョン。

- **OAuth**: OAuth 2.0 ベースの認証を用いて、OAuth または OIDC 対応の Kafka クラスター（Confluent Cloud や OAuth 有効化済みのセルフマネージド Kafka）に接続。

  EMQX は OAuth 2.0 クライアントとして動作し、OAuth 認可サーバーから定期的にアクセストークンを取得し、SASL/OAUTHBEARER メカニズムで Kafka ブローカーに認証します。

  必須パラメータ:

  - **OAuth Grant Type**: アクセストークン取得に使用する OAuth 2.0 グラントタイプ（現時点で `client_credentials` のみ対応）。
  - **OAuth Token Endpoint URI**: トークン取得用の OAuth/OIDC プロバイダーのエンドポイント。
  - **OAuth Client ID**: OAuth 認可サーバーに登録されたクライアント ID。
  - **OAuth Client Secret**: クライアント ID に対応するシークレット。トークン取得時の認証に使用。
  - **OAuth Request Scope**: （任意）トークンリクエストに含めるスコープ。
  - **SASL Extensions**: （高度な設定、任意）認証時に SASL 拡張として送信する追加のキー・バリュー。Confluent Cloud など一部の Kafka プロバイダーでメタデータを渡すために必要です（例: `logicalCluster`、`identityPoolId`）。詳細は Confluent Cloud の [公式ドキュメント](https://docs.confluent.io/cloud/current/security/authenticate/workload-identities/identity-providers/oauth/overview.html) を参照してください。

- **Basic Auth**: ユーザー名とパスワードによる認証。

  必須項目:

  - **Mechanism**: `plain`、`scram_sha_256`、`scram_sha_512` のいずれかを選択。
  - **Username** と **Password**: Kafka クラスター認証用の資格情報。

- **Kerberos**: Kerberos GSSAPI 認証。

  必須項目:

  - **Kerberos Principal**: 認証に使用する Kerberos プリンシパル。
  - **Kerberos Keytab File**: 非対話認証に使用するキータブファイルのパス。

  ::: tip 重要

  Kerberos キータブファイルはすべての EMQX ノードで同じパスに配置し、EMQX サービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sink を使ったルールの作成

このセクションでは、MQTT トピック `t/#` のメッセージを処理し、Kafka の `testtopic-in` トピックに送信する Kafka Sink を使ったルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. 画面右上の **Create** をクリックします。

3. ルール ID を入力します。例として `my_rule` とします。

4. **SQL Editor** に以下のステートメントを入力します。これは MQTT トピック `t/#` のメッセージを Kafka に転送する例です。

   注意: 独自の SQL 文を指定する場合は、Sink が必要とするすべてのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Try It Out** をクリックして、SQL ルールの学習とテストが可能です。

   :::

   ::: tip

   EMQX v5.7.2 からルール SQL で環境変数を読み取る機能が追加されました。詳細は [Use Environment Variables in Rule SQL](#use-environment-variables) を参照してください。

   :::

5. **Create Rule** ページで + **Add Action** をクリックし、ルールの出力アクションを定義します。

6. **Type of Action** ドロップダウンから `Kafka Producer` を選択します。

   **Action** ドロップダウンはデフォルトの `Create Action` のままにします。

   > 既存の Sink を選択することも可能ですが、本例では新規作成します。

7. **Name** と任意で **Description** を入力します。

8. **Connector** ドロップダウンから、先に作成した `my-kafka` コネクターを選択します。必要に応じて新規作成も可能です。[Create a Kafka Producer Connector](#create-a-kafka-producer-connector) を参照してください。

9. Sink のデータ送信方法を設定します。

      - **Kafka Topic**: メッセージをパブリッシュする対象の Kafka トピック。`testtopic-in` と入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートします。[Use Variable Templates](#use-variable-templates) を参照してください。
      - **Kafka Headers**: Kafka メッセージに付加する任意のキー・バリュー形式のメタデータ。ヘッダー値はオブジェクトに解決される必要があります。エンコード方法は **Kafka Header Value Encode Type** で選択可能で、複数ヘッダーは **Add** ボタンで追加できます。
      - **Message Key**: Kafka メッセージのキー。パーティション割り当てやメッセージ順序付けに使用されます。静的文字列やプレースホルダー（例: `${.clientid}`）を含めることが可能です。
      - **Message Value**: Kafka メッセージのペイロード。テンプレートからレンダリングされます。静的文字列やプレースホルダー（例: `${.}`）を使ってルールコンテキストから動的に生成可能です。テンプレートが `NULL`（参照フィールドが存在しない場合など）に解決されると、空文字列ではなく Kafka の `NULL` 値が生成されます。
      - **Message Timestamp**: Kafka メッセージのタイムスタンプ。固定値またはプレースホルダー（例: `${timestamp}`）を使ってルール出力から動的に設定できます。
      - **Partition Strategy**: プロデューサーが Kafka パーティションにメッセージを分配する方法を選択します。
      - **Partitions Limit**: プロデューサーがメッセージを送信する最大パーティション数を制限します。有効にすると、利用可能なすべてのパーティションではなく指定数のパーティション間でのみ分配されます。
      - **Compression**: Kafka メッセージのレコードを圧縮／解凍するための圧縮アルゴリズムを指定します。

10. **Fallback Actions**（任意）: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリ Sink がメッセージ処理に失敗した場合にトリガーされます。詳細は [Fallback Actions](./data-bridges.md#fallback-actions) を参照してください。

11. **Advanced Settings**（任意）: [Advanced Configuration](#advanced-configuration) を参照してください。

12. **Create** ボタンをクリックして Sink の作成を完了します。作成後、**Create Rule** ページに戻り、新しい Sink がルールアクションに追加されます。

13. **Create** ボタンをクリックしてルール全体の作成を完了します。

![kafka_producer_bridge](./assets/kafka_producer_bridge.png)

これでルールが正常に作成され、**Integration** -> **Rules** ページに新規ルールが表示されるほか、**Actions(Sink)** タブに Kafka プロデューサー Sink が表示されます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを確認できます。トポロジーからは、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Kafka に送信・保存されている様子が直感的に把握できます。

### Kafka 動的トピックの設定

EMQX v5.7.2 以降、Kafka プロデューサー Sink の設定で環境変数や変数テンプレートを使い、Kafka トピックを動的に設定できます。本節ではこれら2つのユースケースを紹介します。

#### 環境変数の利用

EMQX v5.7.2 では、ルール SQL の処理フェーズで [環境変数](../configuration/configuration.md#environment-variables) の値を動的にメッセージのフィールドに割り当てる機能が追加されました。この機能はルールエンジンの組み込み SQL 関数の [getenv](../data-integration/rule-sql-builtin-functions.md#system-function) を使い、EMQX の環境変数を取得します。取得した値は SQL 処理結果にセットされます。

この機能の応用例として、Kafka Sink ルールアクションの Kafka トピック設定にルール出力結果のフィールドを参照してトピックを設定できます。以下はその例です。

::: tip 注意

他のシステム環境変数の漏洩を防ぐため、ルールエンジンで使用する環境変数名は必ず `EMQXVAR_` プレフィックスを付ける必要があります。例えば、`getenv` 関数で読み取る変数名が `KAFKA_TOPIC` の場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` と設定してください。

:::

1. Kafka を起動し、`testtopic-in` トピックを事前作成します。[はじめる前に](#はじめる前に) を参照してください。

2. EMQX を起動し、環境変数を設定します。zip 版インストールの場合、起動時に環境変数を指定可能です。例として Kafka トピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` に設定します。

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

5. SQL テストを有効にし、環境変数の値 `testtopic-in` が正しく取得できることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka プロデューサー Sink のアクションを追加します。ルールの右側の **Action Outputs** で **Add Action** をクリックします。

   - **Connector**: 先に作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**: SQL ルール出力に基づき変数テンプレート形式 `${kafka_topic}` で設定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. その他の設定は [Kafka Sink を使ったルールの作成](#kafka-sink-を使ったルールの作成) を参照し、最後に **Create** をクリックしてルール作成を完了します。

8. [Kafka プロデューサールールのテスト](#test-kafka-producer-rule) の手順に従い、Kafka へメッセージを送信します。

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

**Kafka Topic** フィールドに静的なトピック名を設定する以外に、変数テンプレートを使って動的にトピックを生成できます。これによりメッセージ内容に基づいて Kafka トピックを構築でき、柔軟なメッセージ処理と分散が可能になります。例えば、`device-${payload.device}` のように指定すると、特定デバイスからのメッセージをデバイス ID をサフィックスに持つトピック（例: `device-1`）に簡単に送信できます。

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

このキーが含まれていないとトピックのレンダリングに失敗し、回復不能なメッセージドロップが発生します。

また、Kafka 側に解決されるすべてのトピック（例: `device-1`、`device-2` など）を事前作成しておく必要があります。存在しないトピック名に解決されると、同様に回復不能なエラーでメッセージがドロップされます。

## Kafka プロデューサールールのテスト

Kafka プロデューサールールが期待通りに動作するか確認するために、[MQTTX](https://mqttx.app/en) を使って EMQX に MQTT メッセージをパブリッシュするクライアントをシミュレートできます。

1. MQTTX でトピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページで Sink 名をクリックし統計情報を表示します。Sink の稼働状況を確認し、新規の受信メッセージと送信メッセージが1件ずつあることを確認します。

3. 以下のコマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafka コンシューマーコネクターの作成

Kafka Source アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka コンシューマーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。

2. 画面右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例として `my-kafka-source` とします。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。なお、この例では EMQX と Kafka をローカルマシンで起動している前提です。リモート環境の場合は適宜設定を変更してください。

   - **Authentication**: Kafka クラスターが要求する認証方式を選択します。以下の方式がサポートされています。

     - `None`: 認証なし。
     - `AWS IAM for MSK`: EMQX を EC2 インスタンス上にデプロイし Amazon MSK クラスターに接続する場合に使用。
     - `MSK IAM Roles Anywhere`: EC2 以外の環境から AWS IAM Roles Anywhere クレデンシャルヘルパーを使って MSK に接続する場合に使用。
     - `OAuth`: [OAuth 2.0](https://oauth.net/2/) を使った認証。
     - `Basic Auth`: **Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）を選択し、**Username** と **Password** を指定。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab File** を指定。

     詳細は [Authentication Method](#authentication-method) を参照してください。

   - 暗号化接続を確立する場合は **Enable TLS** トグルをオンにします。TLS 接続の詳細は **TLS for External Resource Access** を参照してください。

   - **Advanced Settings**（任意）: [Advanced Configuration](#advanced-configuration) を参照してください。

6. **Create** をクリックする前に、**Test Connection** をクリックして Kafka への接続が成功するか確認できます。

11. **Create** をクリックします。関連するルール作成のオプションが表示されます。[Kafka コンシューマー Source を使ったルールの作成](#create-a-rule-with-kafka-consumer-source) を参照してください。

## Kafka コンシューマー Source を使ったルールの作成

このセクションでは、Kafka コンシューマー Source で転送されたメッセージをさらに処理し、MQTT トピックに再パブリッシュするルールの作成方法を示します。

### ルール SQL の作成

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. 画面右上の **Create** をクリックします。

3. ルール ID を入力します。例として `my_rule` とします。

4. Kafka Source `$bridges/kafka_consumer:<sourceName>` から変換されたメッセージを EMQX に転送する場合、**SQL Editor** に以下のステートメントを入力します。

   注意: 独自の SQL 文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドを `SELECT` 部分に含めてください。Kafka Source の `SELECT` 文では `ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node` などのフィールドが使用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   注意: 初心者の方は **SQL Examples** と **Enable Test** をクリックして、SQL ルールの学習とテストが可能です。

### Kafka コンシューマー Source をデータ入力として追加

1. ルール作成ページの右側にある **Data Inputs** タブを選択し、**Add Input** をクリックします。

2. **Input Type** ドロップダウンから **Kafka Consumer** を選択します。**Source** ドロップダウンはデフォルトの `Create Source` のままにするか、既存の Kafka コンシューマーソースを選択します。本例では新規作成してルールに追加します。

3. ソースの名前と説明を入力します。

4. **Connector** ドロップダウンから、先ほど作成した `my-kafka-consumer` コネクターを選択します。必要に応じて隣のボタンから新規作成も可能です。[Kafka コンシューマーコネクターの作成](#kafka-コンシューマーコネクターの作成) を参照してください。

5. 以下の項目を設定します。

   - **Kafka Topic**: コンシューマーソースが購読する Kafka トピックを指定します。
   - **Group ID**: このソースのコンシューマーグループ識別子を指定します。未指定の場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode** と **Value Encoding Mode**: Kafka メッセージのキーと値のエンコード方式を選択します。

7. **Offset Reset Policy**: コンシューマーが Kafka トピックのパーティションを読み始めるオフセットリセットポリシーを選択します。

   - `latest`: コンシューマー開始時点の最新オフセットから読み始め、過去のメッセージはスキップ。
   - `earliest`: パーティションの先頭から読み始め、過去のすべてのメッセージを読み取る。

8. **Advanced Settings**（任意）: [Advanced Configuration](#advanced-configuration) を参照してください。

9. **Create** をクリックする前に、**Test Connectivity** をクリックして Kafka への接続が成功するか確認できます。

10. **Create** をクリックしてソース作成を完了します。ルール作成ページの **Data Inputs** タブに新しいソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs** タブを選択し、+ **Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action** ドロップダウンから **Republish** を選択します。

3. **Topic** と **Payload** フィールドに再パブリッシュするメッセージのトピックとペイロードを入力します。例として `t/1` と `${.}` を入力します。

   - **Topic** フィールドには `${}` を使って動的に MQTT トピックを指定することも可能です。例: `t/${key}` （`${}` 内のパラメータは SQL の `SELECT` 文に含まれている必要があります）。

4. **Add** をクリックしてアクションをルールに追加します。

5. ルール作成ページに戻り、**Save** をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Source ルールのテスト

Kafka Source とルールが期待通りに動作するか確認するために、[MQTTX](https://mqttx.app/) を使って EMQX でトピックをサブスクライブするクライアントをシミュレートし、Kafka プロデューサーで Kafka トピックにデータを投入します。その後、Kafka から EMQX を経由してクライアントがサブスクライブしたトピックにデータが再パブリッシュされるか確認します。

1. MQTTX でトピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドで Kafka プロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力待ちになります。

3. `{"msg": "Hello EMQX"}` と入力し、`testtopic-out` トピックにメッセージを送信します。

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

このセクションでは、データ統合のパフォーマンス最適化や特定シナリオに応じたカスタマイズのための高度な設定オプションについて説明します。コネクター、Sink、Source 作成時に **Advanced Settings** を展開し、以下の設定をビジネスニーズに応じて調整できます。

| 項目名                                    | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                 | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータフェッチ要求時に存在しない Kafka トピックを自動作成可能にします。 | `disabled`         |
| Min Metadata Refresh Interval             | クライアントが Kafka ブローカーおよびトピックのメタデータを更新する最短間隔。短すぎると Kafka サーバーの負荷が増大します。 | `3` 秒             |
| Metadata Request Timeout                  | Kafka からメタデータを要求する際の最大待機時間。                   | `5` 秒             |
| Connect Timeout                           | TCP 接続確立の最大待機時間（認証時間含む）。                      | `5` 秒             |
| Max Wait Time (Source)                    | Kafka ブローカーからのフェッチ応答を待つ最大時間。                 | `1` 秒             |
| Fetch Bytes (Source)                      | フェッチリクエストごとに Kafka から取得するバイト数。設定値がメッセージサイズ未満だとフェッチ性能に悪影響を及ぼす可能性があります。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafka バッチ内でメッセージを収集する最大バイト数。Kafka ブローカーのデフォルトは 1MB ですが、EMQX はエンコードオーバーヘッドを考慮し若干小さめに設定しています。単一メッセージがこのサイズを超える場合は別バッチで送信されます。 | `896` KB           |
| Offset Commit Interval (Source)           | コンシューマーグループごとにオフセットコミット要求を送る間隔。       | `5` 秒             |
| Required Acks (Sink)                      | Kafka パーティションリーダーがフォロワーから待つアックの種類：<br />`all_isr`: 全インシンクレプリカからのアック<br />`leader_only`: リーダーのみ<br />`none`: アック不要 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafka プロデューサーがパーティション数の増加を検知する間隔。増加検知後、指定した `partition_strategy` に基づき新パーティションへメッセージを分配します。 | `60` 秒            |
| Max Inflight (Sink)                       | Kafka プロデューサーがアック受信前に送信可能な最大バッチ数（パーティションごと）。値が大きいほどスループット向上が期待できますが、1より大きいとメッセージの順序入れ替わりリスクがあります。 | `10`               |
| Query Mode (Source)                       | 非同期または同期クエリモードを選択し、メッセージ送信を最適化。非同期モードでは Kafka 書き込みが MQTT パブリッシュ処理をブロックしませんが、クライアントが Kafka 到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期モード時の最大待機時間。メッセージ送信完了を確実に待つための設定。同期モード時のみ有効。 | `5` 秒             |
| Buffer Mode (Sink)                        | メッセージを送信前にバッファリングする方式。<br />`memory`: メモリ上にバッファ。EMQX ノード再起動でメッセージは失われる。<br />`disk`: ディスク上にバッファ。再起動後もメッセージ保持。<br />`hybrid`: 初めはメモリバッファで、一定サイズ超過後にディスクへオフロード。メモリモード同様再起動でメッセージは失われる。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafka パーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄して新規メッセージ用に空間を確保。メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが `disk` または `hybrid` の場合に適用。メッセージ保存用のセグメントファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが `memory` の場合に適用。メモリ圧迫時に古いメッセージを自動破棄し、システムの安定性を確保。Linux システムでのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ネットワーク送受信のソケットバッファサイズを管理し、通信性能を最適化。 | `1024` KB          |
| TCP Keepalive                             | Kafka ブリッジ接続の TCP キープアライブ設定。接続の長時間アイドルによる切断を防止。`Idle, Interval, Probes` の3つの数値をカンマ区切りで指定。Linux のデフォルトは `7200,75,9`。例: `240,30,5` は 240秒アイドル後にキープアライブを開始し、30秒間隔で最大5回プローブを送信。 | `none`             |
| Max Batch Age (Sink)                      | プロデューサーバッファ内でメッセージが保持される最大時間。超過したバッチは送信されず破棄される。キューイング中や接続断時のバッファ内メッセージに適用。破棄されたメッセージは `dropped.expired` メトリクスにカウント。デフォルトは無制限（`infinity`）。 | `infinity`         |
| Max Retries (Sink)                        | Kafka がリトライ可能なエラーを返した際の最大リトライ回数。初回試行とリトライがすべて失敗するとバッチは破棄され、メッセージは `failed` メトリクスにカウント。接続断による再送はリトライ回数に含まれず、`max_batch_age` によって制限。デフォルトは無制限（`infinity`）。 | `infinity`         |
| Reconnect Delay (Sink)                    | 接続断後にプロデューサーが Kafka に再接続を試みるまでの遅延時間。切断中もメッセージはバッファに蓄積されるが、バッファ制限と `max_batch_age` の影響を受ける。デフォルトは `2` 秒。 | `2` 秒             |
| Max Linger Time                           | パーティションごとのプロデューサーがより大きなバッチを作るために待機する最大時間。すべてのバッファモードに適用。デフォルト `0` は待機なしでレイテンシ最適化。小さな遅延を許容するとリクエスト数削減可能。ディスクバッファ時はバッチ書き込み前の待機時間。IOPS 削減には少なくとも 5ms 推奨。 | `0` ミリ秒         |
| Max Linger Bytes                          | パーティションごとのプロデューサーがバッチ送信を開始する最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状況をチェックする間隔。                         | `15` 秒            |

## さらに詳しく

EMQX は Apache Kafka とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [MQTT と Kafka を使ったコネクテッドビークルのストリーミングデータパイプライン構築：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTT と Kafka：IoT データ統合のパワーアップ](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTT パフォーマンスベンチマークテスト：EMQX-Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterprise パフォーマンスベンチマークテスト：Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [EMQX Cloud ルールエンジンを使ったデバイスデータの Kafka へのブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloud ルールエンジンに関する動画で、今後より適切な動画に差し替え予定）
