# Stream MQTT Data into Apache Kafka

[Apache Kafka](https://kafka.apache.org/) は、高スループットかつリアルタイムのデータ処理を目的とした広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafka クライアントは安定したネットワーク接続と高いシステムリソースを必要とするため、エッジ IoT 通信には適していません。IoT シナリオでは、デバイスが軽量な MQTT プロトコルを使用して、不安定なネットワーク上でも効率的にデータを送信することが一般的です。

EMQX は MQTT と Kafka/[Confluent](https://www.confluent.io/) を統合し、IoT デバイスとバックエンドシステム間のシームレスなデータストリーミングを実現します。MQTT メッセージは Kafka トピックに取り込まれ、リアルタイム処理、保存、分析に利用される一方で、Kafka トピックのデータは消費されて MQTT クライアントに配信され、タイムリーなアクションをトリガーすることも可能です。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは EMQX と Kafka のデータ統合について紹介し、統合の作成および検証手順を段階的に解説します。

## 仕組み

Apache Kafka とのデータ統合は EMQX に組み込まれた機能であり、MQTT ベースの IoT データを Kafka にストリーミングして下流の処理や分析に活用します。組み込みの[ルールエンジン](./rules.md)を活用することで、カスタムコードなしにデータのフィルタリング、変換、ルーティングが可能です。

以下の図は、自動車 IoT シナリオにおける典型的な EMQX–Kafka 統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->

Apache Kafka へのデータの流入または流出には、Kafka Sink（Kafka へメッセージを送信）または Kafka Source（Kafka からメッセージを受信）を作成します。以下は Kafka Sink のワークフローです。

1. **メッセージ取り込み**: 車両に接続された IoT デバイスは EMQX に MQTT 接続を確立し、定期的に状態データを含むメッセージをパブリッシュします。EMQX がメッセージを受信すると、ルールエンジンでルールマッチングが開始されます。
2. **ルールベース処理**: マッチしたルールは、定義された通りにペイロードのフィルタリング、変換、または付加処理を行います。
3. **Kafka へのデータ転送**: ルールエンジンで定義されたルールが Kafka への転送アクションをトリガーします。Kafka Sink を使い、MQTT トピックを事前定義された Kafka トピックにマッピングし、処理済みのメッセージとデータを Kafka トピックに書き込みます。

Kafka にデータが取り込まれた後は、以下のように複数の方法で消費・処理が可能です。

- バックエンドサービスが Kafka トピックからリアルタイムデータストリームを直接消費。
- Kafka Streams によるリアルタイム集約、相関分析、解析。
- Kafka Connect を使い、MySQL や Elasticsearch など外部システムへデータを転送し保存・追加処理。

## 特長と利点

Apache Kafka とのデータ統合は以下の特長と利点を提供します。

- **信頼性の高い双方向 IoT データメッセージング**: EMQX は不安定なネットワーク環境でも MQTT メッセージを確実に Kafka に転送し、バックエンドからの Kafka メッセージを接続された IoT クライアントに配信します。
- **ペイロード変換**: メッセージは Kafka に転送する前に SQL ルールでフィルタリング、付加、変換が可能です。
- **効果的なトピックマッピング**: MQTT トピックやユーザープロパティを柔軟に Kafka トピックやヘッダーにマッピングでき、一対一、一対多、ワイルドカードベースのマッピングに対応します。
- **柔軟なパーティション選択戦略**: MQTT トピックやクライアントに基づき、同じ Kafka パーティションにメッセージを転送可能です。
- **高スループット処理**: 同期・非同期の Kafka 書き込みをサポートし、異なるワークロードシナリオに応じてレイテンシとスループットのバランスを調整できます。
- **ランタイムメトリクス**: 各 Sink・Source の総メッセージ数、成功/失敗数、現在のレートなどのランタイムメトリクスを閲覧可能です。
- **動的設定**: ダッシュボードまたは設定ファイルから Sink と Source の動的設定が可能です。

これらの機能により、効率的なデータ取り込みと管理が可能なスケーラブルかつレジリエントな IoT データプラットフォームを構築できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Kafka Sink と Source を作成する前に必要な準備について説明します。

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

詳細な操作手順は [Kafka ドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/) を参照してください。

### Kafka トピックの作成

EMQX でデータ統合を作成する前に、関連する Kafka トピックを作成してください。以下のコマンドで Sink 用の `testtopic-in` と Source 用の `testtopic-out` の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka プロデューサーコネクターの作成

Kafka Sink アクションを追加する前に、EMQX と Kafka の接続を確立するための Kafka プロデューサーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例として `my-kafka` とします。この名前は Kafka Sink とコネクターを関連付けるために使われ、クラスター内で一意である必要があります。

4. Kafka への接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。なお、本デモは EMQX と Kafka をローカルマシンで実行していることを想定しています。リモート環境の場合は適宜調整してください。

   - **Authentication**: Kafka クラスターで必要な認証方式を選択します。以下の方式がサポートされています。

     - `None`: 認証なし。
     - `AWS IAM for MSK`: Amazon EC2 上の EMQX から Amazon MSK クラスターに接続する場合に使用。
     - `OAuth`: OAuth 2.0 ベースの認証で、OAuth または OIDC 対応の Kafka クラスターに接続。
     - `Basic Auth`: ユーザー名とパスワードによる認証。`plain`、`scram_sha_256`、`scram_sha_512` のいずれかのメカニズムを選択。
     - `Kerberos`: Kerberos (GSSAPI) 認証。Kerberos プリンシパルとキータブファイルの指定が必要。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は **Enable TLS** をオンにします。TLS 接続の詳細は[外部リソースアクセスの TLS](../../guides/network/overview.md#tls-for-external-resource-access)を参照してください。
   - **Request Timeout**: Kafka からの応答待ち時間（秒）を指定します。デフォルトは `30` 秒です。タイムアウト超過時は接続が古いと見なされ再接続されます。値が小さすぎると、Kafka はリクエストを受け入れても応答を遅延させる場合があり、EMQX は再接続後に同じバッチを再送し、重複メッセージや過剰な下流データ量を招く可能性があります。

   - **Advanced Settings**（任意）: [高度な設定](#advanced-configurations)を参照してください。

5. **Create** をクリックする前に、**Test Connection** で Kafka サーバーへの接続が成功するか確認できます。

6. **Create** をクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的に Kafka に接続します。次に、このコネクターを使ってデータを Kafka クラスターに転送するルールを作成します。

### 認証方式

EMQX で Kafka コネクターを作成する際、Kafka クラスターのセキュリティ設定に応じて以下の認証方式を選択できます。

- **None**: 認証なし。

- **MSK IAM**: Amazon EC2 上の EMQX から Amazon MSK クラスターに接続する場合に使用。

  この方式は、EC2 インスタンスメタデータサービスを利用して、インスタンスに付与された IAM ポリシーに基づく認証トークンを生成します。

  ::: tip 重要

  MSK IAM 認証は、EMQX が EC2 インスタンス上で実行され、MSK クラスターに接続する場合のみサポートされます。これは EC2 インスタンスメタデータサービスに依存しているためです。

  `iptables` や `nftables` などでホストレベルのアウトバウンドフィルタリングを行う場合は、`169.254.169.254` へのアクセスをブロックしないでください。EMQX は MSK IAM 認証のためにインスタンスメタデータサービスにアクセスする必要があります。同様の例外は、S3、S3 Tables、DynamoDB、Kinesis など EC2 インスタンスメタデータから認証情報を取得する他の AWS ベースのコネクターにも適用されます。詳細は[ルールエンジンポリシーとファイアウォールルールによる SSRF 緩和](../../guides/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules)を参照してください。

  :::

- **OAuth**: OAuth 2.0 ベースの認証で、OAuth または OIDC 対応の Kafka クラスター（例: Confluent Cloud、OAuth 有効化済みのセルフマネージド Kafka）に接続します。

  EMQX は OAuth 2.0 クライアントとして動作し、OAuth 認可サーバーから定期的にアクセストークンを取得し、SASL/OAUTHBEARER メカニズムで Kafka ブローカーに認証します。

  必要なパラメータは以下の通りです。

  - **OAuth Grant Type**: アクセストークン取得に使用する OAuth 2.0 グラントタイプ（現状は `client_credentials` のみ対応）。
  - **OAuth Token Endpoint URI**: OAuth/OIDC プロバイダーのトークンエンドポイント URI。
  - **OAuth Client ID**: OAuth 認可サーバーに登録されたクライアント ID。
  - **OAuth Client Secret**: トークン取得時に EMQX を認証するためのクライアントシークレット。
  - **OAuth Request Scope**: （任意）トークンリクエストに含めるスコープ。
  - **SASL Extensions**: （高度な設定、任意）認証時に送信する追加のキー・バリュー。Confluent Cloud など一部 Kafka プロバイダーで必要なメタデータ（例: `logicalCluster`、`identityPoolId`）を渡すために使います。

  Confluent Cloud における OAuth/OIDC 認証の詳細は[公式ドキュメント](https://docs.confluent.io/cloud/current/security/authenticate/workload-identities/identity-providers/oauth/overview.html)を参照してください。

- **Basic Auth**: ユーザー名とパスワードによる認証。

  選択時は以下を指定します。

  - **Mechanism**: `plain`、`scram_sha_256`、`scram_sha_512` のいずれか。
  - **Username** と **Password**: 認証に使用する資格情報。

- **Kerberos**: Kerberos GSSAPI 認証。

  以下を指定します。

  - **Kerberos Principal**: 認証に使用する Kerberos プリンシパル。
  - **Kerberos Keytab File**: 非対話認証に使用するキータブファイルのパス。

  ::: tip 重要

  Kerberos キータブファイルは全ての EMQX ノードで同一パスに配置し、EMQX サービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sink を使ったルールの作成

このセクションでは、MQTT トピック `t/#` のメッセージを処理し、Kafka Sink を使って Kafka の `testtopic-in` トピックに送信するルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルール ID を入力します。例として `my_rule` とします。

4. **SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` の MQTT メッセージを Kafka に転送する例です。

   注意: 独自の SQL を指定する場合は、Sink が必要とする全てのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の場合は **SQL Examples** と **Try It Out** をクリックして SQL ルールの学習とテストが可能です。

   :::

   ::: tip

   EMQX v5.7.2 からルール SQL 内で環境変数を読み取る機能が追加されました。詳細は[ルール SQL で環境変数を使う](#use-environment-variables)を参照してください。

   :::

5. **Create Rule** ページで + **Add Action** をクリックし、ルールの出力アクションを定義します。

6. **Type of Action** ドロップダウンから `Kafka Producer` を選択します。

   **Action** ドロップダウンはデフォルトの `Create Action` のままにします。

   > 既存の Sink を選択することも可能ですが、本例では新規作成します。

7. **Name** と任意で **Description** を入力します。

8. **Connector** ドロップダウンから先ほど作成した `my-kafka` コネクターを選択します。必要に応じて新規作成も可能です。[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

9. Sink のデータ送信方法を設定します。

      - **Kafka Topic**: メッセージをパブリッシュする Kafka トピック。`testtopic-in` と入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートします。[変数テンプレートの使用](#use-variable-templates)を参照してください。
      - **Kafka Headers**: Kafka メッセージに付加する任意のキー・バリュー形式のメタデータ。ヘッダー値はオブジェクトとして解決される必要があります。エンコード方式は **Kafka Header Value Encode Type** で選択でき、複数ヘッダーは **Add** で追加可能です。
      - **Message Key**: Kafka メッセージのキー。パーティション割り当てやメッセージ順序付けに使用されます。静的文字列や `${.clientid}` のようなプレースホルダーを含めることが可能です。
      - **Message Value**: Kafka メッセージのペイロード。テンプレートからレンダリングされ、静的文字列や `${.}` のようなプレースホルダーを含めて動的に生成可能です。テンプレートが `NULL`（例：参照フィールドが存在しない場合）を返すと、空文字列ではなく Kafka の `NULL` 値が生成されます。
      - **Message Timestamp**: Kafka メッセージのタイムスタンプ。固定値や `${timestamp}` のようなプレースホルダーでルール出力から動的に設定可能です。
      - **Partition Strategy**: プロデューサーが Kafka パーティションにメッセージを分配する方法を選択します。
      - **Partitions Limit**: プロデューサーがメッセージを送信できる最大パーティション数を制限します。有効化すると、全パーティションではなく指定数のパーティション間でのみメッセージを分配します。
      - **Compression**: Kafka メッセージのレコードを圧縮・解凍するための圧縮アルゴリズムを指定します。

10. **フォールバックアクション**（任意）: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

11. **高度な設定**（任意）: [高度な設定](#advanced-configuration)を参照してください。

12. **Create** をクリックして Sink の作成を完了します。作成後は **Create Rule** ページに戻り、新しい Sink がルールアクションに追加されます。

13. **Create** をクリックしてルール作成を完了します。

![kafka_producer_bridge](./assets/kafka_producer_bridge.png)

これでルールが正常に作成され、**Integration** -> **Rules** ページに新規ルールが表示され、**Actions(Sink)** タブに Kafka プロデューサー Sink が追加されていることが確認できます。

また、**Integration** -> **Flow Designer** でトポロジーを表示すると、トピック `t/#` のメッセージがルール `my_rule` によって解析され Kafka に送信・保存されている様子を直感的に確認できます。

### Kafka 動的トピックの設定

EMQX v5.7.2 以降、Kafka プロデューサー Sink の設定で環境変数や変数テンプレートを使い、Kafka トピックを動的に設定可能です。本節ではこれら2つのユースケースを紹介します。

#### 環境変数の利用

EMQX v5.7.2 から、ルール SQL の処理段階で [環境変数](../../guides/configuration/configuration.md#environment-variables)の値を動的にメッセージフィールドに割り当てる機能が追加されました。これはルールエンジンの組み込み SQL 関数 [getenv](./rule-sql-builtin-functions.md#system-function) を使い、EMQX の環境変数を取得して SQL 処理結果に設定します。この機能を応用し、Kafka Sink ルールアクションの Kafka トピック設定でルール出力結果のフィールドを参照してトピックを指定できます。以下はその例です。

::: tip 注意

他のシステム環境変数の漏洩を防ぐため、ルールエンジンで使用する環境変数名は必ず `EMQXVAR_` プレフィックスを付ける必要があります。例えば `getenv` で読み取る変数名が `KAFKA_TOPIC` の場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` と設定してください。

:::

1. Kafka を起動し、`testtopic-in` トピックを事前作成します。[はじめる前に](#before-you-start)の手順を参照してください。

2. EMQX を起動し、環境変数を設定します。zip インストールの場合は起動時に直接指定可能です。例として Kafka トピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

4. Kafka Sink ルールを設定します。**SQL Editor** に以下を入力します。

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

6. Kafka プロデューサー Sink のアクションを追加します。ルールの右側 **Action Outputs** で **Add Action** をクリックします。

   - **Connector**: 先に作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**: SQL ルール出力の変数テンプレート形式 `${kafka_topic}` を指定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sink を使ったルールの作成](#create-a-rule-with-kafka-sink)を参照して追加設定を完了し、最後に **Create** をクリックしてルール作成を完了します。

8. [Kafka プロデューサールールのテスト](#test-kafka-producer-rule)の手順に従い、Kafka にメッセージを送信します。

   ```bash
   mqttx pub -h 127.0.0.1 -p 1883 -i pub -t t/Connection -q 1 -m 'payload string'
   ```

   Kafka トピック `testtopic-in` でメッセージを受信できるはずです。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092 \
     --topic testtopic-in

   {"payload":"payload string","kafka_topic":"testtopic-in"}
   {"payload":"payload string","kafka_topic":"testtopic-in"}
   ```

#### 変数テンプレートの利用

**Kafka Topic** フィールドに静的なトピック名を設定する以外に、変数テンプレートを使って動的にトピックを生成できます。これにより、メッセージ内容に基づいて Kafka トピックを構築し、柔軟なメッセージ処理・振り分けが可能になります。例えば、`device-${payload.device}` のように指定すると、特定デバイスからのメッセージをデバイスID付きのトピック（例：`device-1`）に簡単に送信できます。

この例では、Kafka に送信するメッセージのペイロードに `device` キーが含まれている必要があります。例：

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

また、Kafka 側で解決される全てのトピック（例：`device-1`、`device-2` など）を事前作成しておく必要があります。存在しないトピック名に解決された場合も、メッセージは回復不能なエラーで破棄されます。

## Kafka プロデューサールールのテスト

Kafka プロデューサールールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/en) を使って EMQX に MQTT メッセージをパブリッシュするクライアントをシミュレートできます。

1. MQTTX でトピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページで Sink 名をクリックし統計情報を確認します。新規の受信メッセージ数と送信メッセージ数がそれぞれ1件ずつ増えているはずです。

3. 以下のコマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafka コンシューマーコネクターの作成

Kafka Source アクションを追加する前に、EMQX と Kafka の接続を確立するための Kafka コンシューマーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例として `my-kafka-source` とします。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。デモはローカル環境を想定しているため、リモート環境の場合は適宜調整してください。

   - **Authentication**: Kafka クラスターで必要な認証方式を選択します。以下の方式がサポートされています。

     - `None`: 認証なし。
     - `authentication_msk_iam`: Amazon EC2 上の EMQX から AWS MSK クラスターに接続する場合に使用。
     - `OAuth`: [OAuth 2.0](https://oauth.net/2/) を使った認証。
     - `Basic Auth`: **Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512` のいずれか）、**Username**、**Password** の指定が必要。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab File** の指定が必要。

     詳細は[認証方式](#authentication-method)を参照してください。

   - 暗号化接続を確立する場合は **Enable TLS** をオンにします。TLS 接続の詳細は **TLS for External Resource Access** を参照してください。

   - **Advanced Settings**（任意）: [高度な設定](#advanced-configuration)を参照してください。

6. **Create** をクリックする前に、**Test Connection** で Kafka サーバーへの接続が成功するか確認できます。

11. **Create** をクリックします。関連するルールの作成オプションが表示されます。[Kafka コンシューマー Source を使ったルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## Kafka コンシューマー Source を使ったルールの作成

このセクションでは、Kafka コンシューマー Source で転送されたメッセージを EMQX でさらに処理し、MQTT トピックに再パブリッシュするルールの作成方法を示します。

### ルール SQL の作成

1. EMQX ダッシュボードで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルール ID を入力します。例として `my_rule` とします。

4. Kafka ソース `$bridges/kafka_consumer:<sourceName>` から変換されたメッセージを EMQX に転送する場合、**SQL Editor** に以下のステートメントを入力します。

   注意: 独自の SQL を指定する場合は、後続の再パブリッシュアクションに必要な全フィールドを `SELECT` 部分に含めてください。Kafka Source の `SELECT` 文では、`ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node` などのフィールドが利用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   注意: 初心者の場合は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

### Kafka コンシューマー Source をデータ入力として追加

1. ルール作成画面の右側にある **Data Inputs** タブを選択し、**Add Input** をクリックします。

2. **Input Type** ドロップダウンから **Kafka Consumer** を選択します。**Source** ドロップダウンはデフォルトの `Create Source` のままにするか、既存の Kafka コンシューマーソースを選択します。本例では新規作成してルールに追加します。

3. ソースの名前と説明を入力します。

4. **Connector** ドロップダウンから先ほど作成した `my-kafka-consumer` コネクターを選択します。必要に応じて隣のボタンから新規コネクターを作成可能です。[Kafka コンシューマーコネクターの作成](#create-a-kafka-consumer-connector)を参照してください。

5. 以下のフィールドを設定します。

   - **Kafka Topic**: コンシューマーソースが購読する Kafka トピック。
   - **Group ID**: このソースのコンシューマーグループ識別子。未指定の場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode** と **Value Encoding Mode**: Kafka メッセージのキーと値のエンコード方式を選択。

7. **Offset Reset Policy**: コンシューマーが Kafka トピックパーティションを読み始めるオフセットリセットポリシーを選択します。

   - `latest`: コンシューマー開始時点以降のメッセージのみ読み込み、過去のメッセージはスキップ。
   - `earliest`: パーティションの先頭からすべての過去メッセージを含めて読み込み。

8. **高度な設定**（任意）: [高度な設定](#advanced-configuration)を参照してください。

9. **Test Connectivity** で Kafka サーバーへの接続確認が可能です。

10. **Create** をクリックしてソース作成を完了します。ルール作成画面の **Data Inputs** タブに新規ソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs** タブを選択し、+ **Add Action** をクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action** ドロップダウンから **Republish** を選択します。

3. **Topic** と **Payload** フィールドに再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として `t/1` と `${.}` を入力します。
   - `${}` を使い、動的に MQTT トピックを指定することも可能です。例：`t/${key}`（`${}` 内のパラメータは SQL `SELECT` 文に含まれている必要があります）。

4. **Add** をクリックしてアクションをルールに追加します。

5. ルール作成画面に戻り、**Save** をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Source ルールのテスト

Kafka ソースとルールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/) を使って EMQX のトピックをサブスクライブするクライアントをシミュレートし、Kafka プロデューサーで Kafka トピックにデータを送信します。EMQX が Kafka からのデータをクライアントがサブスクライブするトピックに再パブリッシュするか確認します。

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

4. MQTTX のサブスクリプションで、Kafka からのメッセージがトピック `t/1` に届いていることを確認します。

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

本セクションでは、データ統合のパフォーマンス最適化やシナリオに応じたカスタマイズのための高度な設定オプションを説明します。コネクター、Sink、Source 作成時に **Advanced Settings** を展開し、以下の設定をビジネスニーズに応じて構成できます。

| 項目                                      | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                 | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータ取得リクエスト時に存在しない Kafka トピックを自動作成可能。 | `disabled`         |
| Min Metadata Refresh Interval             | クライアントが Kafka ブローカーやトピックのメタデータを更新する最短間隔。小さすぎると Kafka サーバーの負荷が増大する可能性あり。 | `3` 秒             |
| Metadata Request Timeout                  | Kafka にメタデータ要求を送信する際の最大待機時間。                 | `5` 秒             |
| Connect Timeout                           | TCP 接続確立の最大待機時間。認証時間も含む。                     | `5` 秒             |
| Max Wait Time (Source)                    | Kafka ブローカーからのフェッチ応答の最大待機時間。                 | `1` 秒             |
| Fetch Bytes (Source)                      | Kafka からのフェッチリクエストで取得するバイト数。設定値がメッセージサイズ未満だとフェッチ性能が低下する可能性あり。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafka バッチ内で収集するメッセージの最大サイズ（バイト）。Kafka ブローカーのデフォルトは 1MB だが、EMQX はエンコードオーバーヘッドを考慮しやや小さめに設定。単一メッセージが超過する場合は別バッチで送信。 | `896` KB           |
| Offset Commit Interval (Source)           | コンシューマーグループごとにオフセットコミット要求を送る間隔。       | `5` 秒             |
| Required Acks (Sink)                      | Kafka パーティションリーダーがフォロワーから待つアックの種類：<br />`all_isr`: 全てのインシンクレプリカからのアックを待つ。<br />`leader_only`: リーダーのみ待つ。<br />`none`: アック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafka プロデューサーがパーティション数の増加を検知する間隔。増加検知後、指定の `partition_strategy` に従い新パーティションにメッセージを分配。 | `60` 秒            |
| Max Inflight (Sink)                       | Kafka プロデューサーがアックを受け取る前に送信可能な最大バッチ数（パーティションごと）。値が大きいほどスループットは向上するが、1より大きいとメッセージの順序入れ替わりリスクあり。 | `10`               |
| Query Mode (Source)                       | 非同期または同期のクエリモードを選択し、メッセージ送信を最適化。非同期モードでは Kafka 書き込みが MQTT パブリッシュ処理をブロックしないが、クライアントが Kafka 到着前にメッセージを受信する可能性あり。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期モード時の最大待機時間。メッセージ送信完了をタイムリーに保証し、長時間待機を防止。`Sync` モード時のみ適用。 | `5` 秒             |
| Buffer Mode (Sink)                        | メッセージを送信前にバッファリングするか設定。メモリバッファは送信速度向上に寄与。<br />`memory`: メモリにバッファ。EMQX 再起動時にメッセージ消失。<br />`disk`: ディスクにバッファ。再起動後もメッセージ保持。<br />`hybrid`: まずメモリにバッファし、一定容量超過時に徐々にディスクにオフロード。メモリモード同様、再起動で消失。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafka パーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄してバッファ空間を確保。メモリ使用量と性能のバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが `disk` または `hybrid` の場合に適用。メッセージ保存用のセグメントファイルサイズを制御し、ディスクストレージの最適化に寄与。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが `memory` の場合に適用。メモリ使用率が高い際に古いバッファメッセージを自動破棄し、システム安定性を確保。Linux システムでのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ソケットバッファサイズを管理し、ネットワーク送信性能を最適化。         | `1024` KB          |
| TCP Keepalive                             | Kafka ブリッジ接続の TCP キープアライブ設定。長時間の非アクティブ状態による接続切断を防止。`Idle, Interval, Probes` の3つの数値をカンマ区切りで指定。<br />Idle: 接続がアイドル状態になる秒数（Linux デフォルト 7200秒）<br />Interval: キープアライブプローブ間隔（Linux デフォルト 75秒）<br />Probes: 応答なしと判断するまでの最大プローブ数（Linux デフォルト 9回）<br />例: `240,30,5` は 240秒アイドル後にプローブ開始、30秒間隔で最大5回送信。 | `none`             |
| Max Batch Age (Sink)                      | プロデューサーバッファ内でメッセージが保持される最大期間。期限切れのバッチは送信せず破棄。破棄されたメッセージは `dropped.expired` メトリクスにカウント。デフォルトは無期限。バッファオーバーフロー時は期限切れに関係なく破棄される場合あり。 | `infinity`         |
| Max Retries (Sink)                        | Kafka がリトライ可能なエラーを返した場合の最大再試行回数。初回と全リトライが失敗するとバッチ破棄され、メッセージは `failed` メトリクスにカウント。接続切断による再送はリトライ回数に含まれず、`max_batch_age` によって制限。デフォルトは無制限。 | `infinity`         |
| Reconnect Delay (Sink)                    | 接続断後に再接続を試みるまでの待機時間。切断中もメッセージはバッファに蓄積され、バッファ制限と `max_batch_age` の影響を受ける。デフォルトは 2 秒。 | `2` 秒             |
| Max Linger Time                           | パーティションごとのプロデューサーがより大きなバッチを形成するために待機する最大時間。全バッファモードに適用。デフォルト 0 は待機なしでレイテンシ最適化。多少の遅延が許容される場合は設定するとリクエスト数削減に有効。ディスクバッファ時はバッファ書き込み前の待機時間。IOPS 削減のため最低 5ms 推奨。 | `0` ミリ秒         |
| Max Linger Bytes                          | パーティションごとのプロデューサーがバッチ送信を開始するまでに蓄積する最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状態をチェックする間隔。                           | `15` 秒            |

## さらに詳しく

EMQX は Apache Kafka とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [MQTT と Kafka を使ったコネクテッドビークルのストリーミングデータパイプライン構築：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTT と Kafka：IoT データ統合の強化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTT パフォーマンスベンチマークテスト：EMQX-Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterprise パフォーマンスベンチマークテスト：Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [EMQX Cloud ルールエンジンを使ったデバイスデータの Kafka へのブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloud ルールエンジンに関する動画で、将来的により適切な動画に差し替え予定）
