# Stream MQTT Data into Apache Kafka

[Apache Kafka](https://kafka.apache.org/) は、高スループットかつリアルタイムのデータ処理を目的とした、広く利用されているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafka クライアントは安定したネットワーク接続と高いシステムリソースを必要とするため、エッジIoT通信には適していません。IoTシナリオでは、デバイスが軽量な MQTT プロトコルを用いて、不安定なネットワーク上でも効率的にデータを送信することが一般的です。

EMQX は MQTT と Kafka／[Confluent](https://www.confluent.io/) を統合し、IoTデバイスとバックエンドシステム間のシームレスなデータストリーミングを実現します。MQTT メッセージは Kafka トピックに取り込まれ、リアルタイム処理、保存、分析に利用される一方で、Kafka トピックからのデータは MQTT クライアントに配信され、タイムリーなアクションをトリガーすることも可能です。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは EMQX と Kafka のデータ統合について紹介し、統合の作成および検証手順をステップバイステップで解説します。

## 仕組み

Apache Kafka データ統合は EMQX に組み込まれた機能であり、MQTT ベースの IoT データを Kafka にストリームし、下流の処理や分析に活用します。組み込みの [ルールエンジン](./rules.md) を活用することで、カスタムコード不要でデータのフィルタリング、変換、ルーティングを実現します。

以下の図は、自動車向け IoT シナリオにおける典型的な EMQX–Kafka 統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->

Apache Kafka へのデータの流入または流出には、それぞれ Kafka Sink（Kafka へメッセージを送信）と Kafka Source（Kafka からメッセージを受信）を作成します。以下は Kafka Sink のワークフローです。

1. **メッセージの取り込み**: 車両に接続された IoT デバイスが EMQX に MQTT 接続を確立し、定期的に状態データを含むメッセージをパブリッシュします。EMQX はメッセージ受信時にルールエンジンでルールマッチングを開始します。
2. **ルールベースの処理**: マッチしたルールが、ペイロードのフィルタリング、変換、拡張などを行います。
3. **Kafka へのデータ転送**: ルールエンジンで定義されたルールは、Kafka へのメッセージ転送アクションをトリガーします。Kafka Sink を使い、MQTT トピックを事前定義された Kafka トピックにマッピングし、処理済みのメッセージやデータを Kafka トピックに書き込みます。

Kafka にデータが取り込まれた後は、以下のように複数の方法で消費・処理が可能です。

- バックエンドサービスが Kafka トピックからリアルタイムデータストリームを直接消費。
- Kafka Streams によるリアルタイム集約、相関分析、解析。
- Kafka Connect を利用して MySQL や Elasticsearch など外部システムへデータを転送し、保存やさらなる処理を実施。

## 特長とメリット

Apache Kafka とのデータ統合は以下の特長とメリットを提供します。

- **信頼性の高い双方向 IoT データメッセージング**: EMQX は不安定なネットワーク環境でも MQTT メッセージを確実に Kafka に転送し、バックエンドからの Kafka メッセージを接続された IoT クライアントに配信します。
- **ペイロード変換**: メッセージは Kafka に転送する前に SQL ルールでフィルタリング、拡張、変換が可能です。
- **柔軟なトピックマッピング**: MQTT トピックやユーザープロパティを Kafka トピックやヘッダーに柔軟にマッピングでき、一対一、一対多、ワイルドカードベースのマッピングをサポートします。
- **柔軟なパーティション選択戦略**: MQTT トピックやクライアントに基づき、同じ Kafka パーティションへメッセージを転送可能です。
- **高スループット処理**: 同期・非同期の Kafka 書き込みをサポートし、レイテンシとスループットのバランスをワークロードに応じて調整可能です。
- **ランタイムメトリクス**: 各 Sink および Source の総メッセージ数、成功／失敗数、現在のレートなどのランタイムメトリクスを閲覧可能です。
- **動的設定**: ダッシュボードまたは設定ファイルから Sink と Source の動的設定が可能です。

これらの機能により、スケーラブルでレジリエントな IoT データプラットフォームを効率的に構築・管理できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Kafka Sink と Source を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の [ルール](./rules.md) に関する知識
- [データ統合](./data-bridges.md) に関する知識

### Kafka サーバーのセットアップ

ここでは macOS を例にインストールと起動手順を示します。以下のコマンドで Kafka をインストールし、起動します。

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# KRaft を使って Kafka を起動
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

詳細な操作手順は、[Kafka ドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/)をご参照ください。

### Kafka トピックの作成

EMQX でデータ統合を作成する前に、関連する Kafka トピックを作成してください。以下のコマンドで、Sink 用の `testtopic-in` と Source 用の `testtopic-out` の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka プロデューサーコネクターの作成

Kafka Sink アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka プロデューサーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例として `my-kafka` とします。名前は Kafka Sink とコネクターを紐付けるために使用され、クラスター内で一意である必要があります。

4. Kafka への接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。なお、本デモは EMQX と Kafka をローカルマシンで動作させている前提です。リモート環境の場合は適宜設定を変更してください。

   - **認証**: Kafka クラスターで要求される認証方式を選択します。以下の方式をサポートしています。

     - `None`: 認証不要。
     - `AWS IAM for MSK`: Amazon EC2 上にデプロイされた EMQX から Amazon MSK クラスターに接続する場合に使用。
     - `OAuth`: OAuth 2.0 ベースの認証で、OAuth または OIDC をサポートする Kafka クラスターに接続。
     - `Basic Auth`: ユーザー名とパスワードによる認証。`plain`、`scram_sha_256`、`scram_sha_512` のいずれかのメカニズムを選択。
     - `Kerberos`: Kerberos (GSSAPI) 認証。Kerberos プリンシパルとキータブファイルの指定が必要。

     各認証方式の詳細は [認証方式](#authentication-method) を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS** トグルをオンにします。TLS 接続の詳細は [外部リソースアクセスの TLS](../../guides/network/overview.md#tls-for-external-resource-access) を参照してください。
   - **Request Timeout**: EMQX が Kafka からの応答を待つ最大時間（秒）。デフォルトは `30` 秒です。タイムアウトを超えると接続が古くなったとみなし再接続します。値が小さすぎると、Kafka はリクエストを受理しても応答が遅れ、EMQX が再接続後に再送することで重複メッセージや過剰な下流データが発生する可能性があります。

   - **詳細設定**（任意）: [詳細設定](#advanced-configurations) を参照してください。

5. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続が成功するか確認できます。

6. **Create** ボタンをクリックしてコネクターを作成します。

作成後、コネクターは自動的に Kafka に接続します。次に、このコネクターを利用して Kafka クラスターへデータを転送するルールを作成します。

### 認証方式

EMQX で Kafka コネクターを作成する際、Kafka クラスターのセキュリティ設定に応じて以下の認証方式を選択できます。

- **None**: 認証不要。

- **MSK IAM**: Amazon EC2 上の EMQX から Amazon MSK クラスターに接続する場合に使用。

  AWS EC2 インスタンスのメタデータサービスを利用して、インスタンスに紐づく IAM ポリシーに基づく認証トークンを生成します。

  ::: tip 重要

  MSK IAM 認証は、EMQX が EC2 インスタンス上で動作し、MSK クラスターに接続する場合のみサポートされます。これは EC2 インスタンスメタデータサービスに依存するためです。

  `iptables` や `nftables` でホストレベルのアウトバウンドフィルタリングを行う場合は、`169.254.169.254` への通信をブロックしないでください。EMQX は MSK IAM 認証のためにインスタンスメタデータサービスにアクセスする必要があります。同様の例外は S3、S3 Tables、DynamoDB、Kinesis などの AWS ベースコネクターにも適用されます。詳細は [ルールエンジンポリシーとファイアウォールルールによる SSRF 対策](../../guides/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。

  :::

- **OAuth**: OAuth 2.0 ベースの認証で、Confluent Cloud や OAuth 有効化済みのセルフマネージド Kafka クラスターに接続します。

  EMQX は OAuth 2.0 クライアントとして動作し、OAuth 認可サーバーから定期的にアクセストークンを取得します。取得したトークンは SASL/OAUTHBEARER メカニズムを介して Kafka ブローカーの認証に使用されます。

  必須パラメータ:

  - **OAuth Grant Type**: アクセストークン取得に使用する OAuth 2.0 のグラントタイプ（現在は `client_credentials` のみ対応）。
  - **OAuth Token Endpoint URI**: トークン取得先の OAuth/OIDC プロバイダーのエンドポイント。
  - **OAuth Client ID**: OAuth 認可サーバーに登録されたクライアントID。
  - **OAuth Client Secret**: クライアントIDに対応するシークレット。
  - **OAuth Request Scope**: （任意）トークンリクエストに含めるスコープ。
  - **SASL Extensions**: （高度な設定、任意）Confluent Cloud など一部の Kafka プロバイダーで必要なメタデータ（例: `logicalCluster`, `identityPoolId`）を SASL 拡張として送信。

  Confluent Cloud における OAuth/OIDC 認証の詳細は [公式ドキュメント](https://docs.confluent.io/cloud/current/security/authenticate/workload-identities/identity-providers/oauth/overview.html) を参照してください。

- **Basic Auth**: ユーザー名とパスワードによる認証。

  選択時は以下を指定します。
  - **Mechanism**: `plain`、`scram_sha_256`、`scram_sha_512` のいずれか。
  - **Username** と **Password**: Kafka クラスター認証用の資格情報。

- **Kerberos**: Kerberos GSSAPI 認証。

  以下を指定します。
  - **Kerberos Principal**: 認証に使用する Kerberos プリンシパル。
  - **Kerberos Keytab File**: 非対話認証用のキータブファイルのパス。

  ::: tip 重要

  キータブファイルはすべての EMQX ノードで同じパスに配置し、EMQX サービスユーザーが読み取り権限を持つ必要があります。

  :::

## Kafka Sink を使ったルールの作成

このセクションでは、MQTT トピック `t/#` のメッセージを処理し、Kafka Sink を使って Kafka の `testtopic-in` トピックに送信するルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します。例: `my_rule`

4. **SQL Editor** に以下のステートメントを入力します。これはトピック `t/#` の MQTT メッセージを Kafka に転送する例です。

   注意: 独自の SQL 文を指定する場合は、Sink で必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は、**SQL Examples** と **Try It Out** をクリックして SQL ルールを学習・テストできます。

   :::

   ::: tip

   EMQX v5.7.2 からルール SQL 内で環境変数を読み込む機能が追加されました。詳細は [ルール SQL で環境変数を使用する](#use-environment-variables) を参照してください。

   :::

5. **Create Rule** ページで + **Add Action** をクリックし、ルールの出力を定義します。

6. **Type of Action** ドロップダウンから `Kafka Producer` を選択します。

   **Action** ドロップダウンはデフォルトの `Create Action` のままにします。

   > 既存の Sink を選択することも可能ですが、本例では新規作成します。

7. **Name** と任意で **Description** を入力します。

8. **Connector** ドロップダウンから、先ほど作成した `my-kafka` コネクターを選択します。必要に応じて新規作成も可能です。[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector) を参照してください。

9. Sink のデータ送信方法を設定します。

      - **Kafka Topic**: メッセージをパブリッシュする Kafka トピック。`testtopic-in` と入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートしています。[変数テンプレートの使用](#use-variable-templates) を参照してください。
      - **Kafka Headers**: Kafka メッセージに付加する任意のキー・バリュー形式のメタデータ。ヘッダー値はオブジェクトとして解決される必要があります。エンコード方式は **Kafka Header Value Encode Type** から選択でき、複数ヘッダーは **Add** で追加可能です。
      - **Message Key**: Kafka メッセージのキー。パーティション分散やメッセージ順序付けに使用されます。静的文字列または `${.clientid}` のようなプレースホルダーを含めることができます。
      - **Message Value**: Kafka メッセージのペイロード。テンプレートからレンダリングされます。静的文字列または `${.}` のようなプレースホルダーを使い、ルールコンテキストから動的に生成可能です。テンプレートが `NULL`（例: 参照フィールドが存在しない場合）になると、空文字列ではなく Kafka の `NULL` 値が生成されます。
      - **Message Timestamp**: Kafka メッセージのタイムスタンプ。固定値または `${timestamp}` のようなプレースホルダーで動的に設定可能です。
      - **Partition Strategy**: プロデューサーがメッセージを Kafka パーティションに分配する方法を選択します。
      - **Partitions Limit**: プロデューサーがメッセージを送信する最大パーティション数を制限します。有効にすると、すべてのパーティションではなく指定数のパーティション間でのみ分配されます。
      - **Compression**: Kafka メッセージのレコード圧縮／解凍に使用する圧縮アルゴリズムを指定します。

10. **フォールバックアクション**（任意）: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

11. **詳細設定**（任意）: [詳細設定](#advanced-configuration) を参照してください。

12. **Create** ボタンをクリックして Sink の作成を完了します。作成後は **Create Rule** ページに戻り、新しい Sink がルールアクションに追加されます。

13. **Create** ボタンをクリックしてルールの作成を完了します。

![kafka_producer_bridge](./assets/kafka_producer_bridge.png)

これでルールが正常に作成され、**Integration** -> **Rules** ページで新規ルールを確認でき、**Actions(Sink)** タブに Kafka プロデューサー Sink が表示されます。

また、**Integration** -> **Flow Designer** でトポロジーを確認できます。トポロジー上で、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Kafka に送信・保存されている様子を直感的に把握できます。

### Kafka 動的トピックの設定

EMQX v5.7.2 以降、Kafka プロデューサー Sink の設定で環境変数や変数テンプレートを使って Kafka トピックを動的に設定できます。本節ではこれら2つのユースケースを紹介します。

#### 環境変数の使用

EMQX v5.7.2 では、[環境変数](../../guides/configuration/configuration.md#environment-variables) の値をルール SQL の処理結果に動的に割り当てる機能が追加されました。これはルールエンジンの組み込み SQL 関数の [getenv](./rule-sql-builtin-functions.md#system-function) を使い、EMQX の環境変数を取得して SQL 処理結果に設定します。この機能を応用し、Kafka Sink のルールアクションでルール出力のフィールドを参照して Kafka トピックを設定できます。以下はその例です。

::: tip 注意

ルールエンジンが使用する環境変数名は、他のシステム環境変数の漏洩を防ぐために必ず `EMQXVAR_` プレフィックスを付ける必要があります。例えば、`getenv` 関数で読み込む変数名が `KAFKA_TOPIC` の場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` と設定してください。

:::

1. Kafka を起動し、`testtopic-in` トピックを事前作成します。[はじめる前に](#before-you-start) を参照してください。

2. EMQX を起動し、環境変数を設定します。zip インストールの場合は起動時に直接指定可能です。例として Kafka トピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector) を参照してください。

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

6. Kafka プロデューサー Sink のアクションを追加します。ルール右側の **Action Outputs** で **Add Action** をクリックします。

   - **Connector**: 先ほど作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**: SQL ルール出力の変数テンプレート `${kafka_topic}` を指定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sink を使ったルールの作成](#create-a-rule-with-kafka-sink) を参照し、残りの設定を完了して **Create** をクリックしルール作成を完了します。

8. [Kafka プロデューサールールのテスト](#test-kafka-producer-rule) の手順に従い、Kafka にメッセージを送信します。

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

#### 変数テンプレートの使用

**Kafka Topic** フィールドに静的なトピック名を設定する以外に、変数テンプレートを使って動的にトピックを生成することも可能です。これにより、メッセージ内容に基づいて Kafka トピックを構築でき、柔軟なメッセージ処理・振り分けが可能になります。例えば、`device-${payload.device}` のように指定すると、特定デバイスからのメッセージを `device-1` のようなデバイスID付きトピックに簡単に送信できます。

この例では、Kafka に送信するメッセージのペイロードに `device` キーが含まれている必要があります。以下は例です。

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

このキーが含まれないとトピックのレンダリングに失敗し、メッセージが回復不能な形で破棄されます。

また、Kafka には事前に `device-1`、`device-2` など、解決されるすべてのトピックを作成しておく必要があります。存在しないトピック名に解決された場合も、回復不能なエラーでメッセージが破棄されます。

## Kafka プロデューサールールのテスト

Kafka プロデューサールールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/en) を使って EMQX に MQTT メッセージをパブリッシュするクライアントをシミュレートできます。

1. MQTTX を使い、トピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページで Sink 名をクリックし、統計情報を確認します。新規の受信メッセージと送信メッセージがそれぞれ1件ずつあるはずです。

3. 以下のコマンドで `testtopic-in` トピックにメッセージが書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafka コンシューマーコネクターの作成

Kafka Source アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka コンシューマーコネクターを作成する必要があります。

1. EMQX ダッシュボードで **Integration** -> **Connector** をクリックします。

2. ページ右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例として `my-kafka-source` とします。

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。ローカル環境でのデモを想定しています。リモート環境の場合は適宜変更してください。

   - **認証**: Kafka クラスターで要求される認証方式を選択します。以下をサポートしています。

     - `None`: 認証不要。
     - `authentication_msk_iam`: AWS MSK クラスターに EC2 インスタンス上の EMQX から接続する場合。
     - `OAuth`: [OAuth 2.0](https://oauth.net/2/) を使った認証。
     - `Basic Auth`: **Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）と **Username**、**Password** の指定が必要。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab File** の指定が必要。

     詳細は [認証方式](#authentication-method) を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS** トグルをオンにします。詳細は **TLS for External Resource Access** を参照してください。

   - **詳細設定**（任意）: [詳細設定](#advanced-configuration) を参照してください。

6. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続を確認できます。

11. **Create** をクリックします。関連するルール作成オプションが表示されます。[Kafka コンシューマー Source を使ったルールの作成](#create-a-rule-with-kafka-consumer-source) を参照してください。

## Kafka コンシューマー Source を使ったルールの作成

このセクションでは、Kafka コンシューマー Source で転送されたメッセージを EMQX で処理し、MQTT トピックに再パブリッシュするルールの作成方法を示します。

### ルール SQL の作成

1. EMQX ダッシュボードで **Integration** -> **Rules** をクリックします。

2. ページ右上の **Create** をクリックします。

3. ルールIDを入力します。例: `my_rule`

4. Kafka Source `$bridges/kafka_consumer:<sourceName>` から変換されたメッセージを EMQX に転送する場合、**SQL Editor** に以下のステートメントを入力します。

   注意: 独自の SQL 文を指定する場合は、後続の再パブリッシュアクションで必要なすべてのフィールドが `SELECT` 部分に含まれていることを確認してください。Kafka Source の `SELECT` 文では `ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node` などのフィールドが使用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   注意: 初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールを学習・テストできます。

### Kafka コンシューマー Source をデータ入力に追加

1. ルール作成ページ右側の **Data Inputs** タブを選択し、**Add Input** をクリックします。

2. **Input Type** ドロップダウンから **Kafka Consumer** を選択します。**Source** はデフォルトの `Create Source` のままか、既存の Kafka コンシューマー Source を選択します。本デモでは新規作成してルールに追加します。

3. ソースの名前と説明を入力します。

4. **Connector** ドロップダウンから、先ほど作成した `my-kafka-consumer` コネクターを選択します。必要に応じて隣のボタンから新規作成も可能です。[Kafka コンシューマーコネクターの作成](#create-a-kafka-consumer-connector) を参照してください。

5. 以下のフィールドを設定します。

   - **Kafka Topic**: コンシューマーが購読する Kafka トピックを指定します。
   - **Group ID**: このソースのコンシューマーグループ識別子。未指定の場合はソース名に基づき自動生成されます。
   - **Key Encoding Mode** と **Value Encoding Mode**: Kafka メッセージのキーと値のエンコード方式を選択します。

7. **Offset Reset Policy**: コンシューマーが Kafka トピックパーティションのどのオフセットから読み始めるかのポリシーを選択します。

   - `latest`: コンシューマー開始時点の最新オフセットから読み始め、過去のメッセージはスキップ。
   - `earliest`: パーティションの先頭から読み始め、過去のメッセージもすべて読み取る。

8. **詳細設定**（任意）: [詳細設定](#advanced-configuration) を参照してください。

9. **Test Connectivity** をクリックして Kafka サーバーへの接続を確認できます。

10. **Create** をクリックしてソース作成を完了します。ルール作成ページの **Data Inputs** タブに新規ソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs** タブを選択し、+ **Add Action** をクリックしてルールトリガーアクションを定義します。

2. **Type of Action** ドロップダウンから **Republish** を選択します。

3. **Topic** と **Payload** フィールドに再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として `t/1` と `${.}` を入力します。
   - **Topic** フィールドには `${}` を使って動的に MQTT トピックを指定することも可能です。例: `t/${key}` （`${}` 内のパラメータは SQL の `SELECT` 文に含まれている必要があります）。

4. **Add** をクリックしてアクションをルールに追加します。

5. ルール作成ページに戻り、**Save** をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Source ルールのテスト

Kafka Source とルールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/) を使って EMQX にサブスクライブするクライアントをシミュレートし、Kafka プロデューサーで Kafka トピックにデータを生成します。その後、Kafka からのデータが EMQX によってクライアントがサブスクライブするトピックに再パブリッシュされるか確認します。

1. MQTTX でトピック `t/1` をサブスクライブします。

   ```bash
   mqttx sub -t t/1 -v
   ```

2. 新しいコマンドラインウィンドウを開き、以下のコマンドで Kafka プロデューサーを起動します。

   ```bash
   bin/kafka-console-producer --bootstrap-server 127.0.0.1:9092 --topic testtopic-out
   ```

   メッセージ入力待ちになります。

3. `{"msg": "Hello EMQX"}` を入力し、`testtopic-out` トピックにメッセージを送信します。

4. MQTTX のサブスクリプションを確認します。Kafka からの以下のメッセージがトピック `t/1` で受信できるはずです。

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

このセクションでは、データ統合のパフォーマンス最適化や特定シナリオに応じたカスタマイズに役立つ詳細設定オプションを説明します。コネクター、Sink、Source 作成時に **Advanced Settings** を展開し、ビジネスニーズに応じて以下の設定を行えます。

| 項目                                      | 説明                                                         | 推奨値             |
| ----------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                 | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際に Kafka トピックが存在しなければ自動作成を許可します。 | `disabled`         |
| Min Metadata Refresh Interval             | クライアントが Kafka ブローカーおよびトピックのメタデータを更新する最小間隔。短すぎると Kafka サーバーへの負荷が増加します。 | `3` 秒             |
| Metadata Request Timeout                  | Kafka からメタデータを要求する際の最大待機時間。                 | `5` 秒             |
| Connect Timeout                           | TCP 接続確立の最大待機時間（認証時間含む）。                     | `5` 秒             |
| Max Wait Time (Source)                    | Kafka ブローカーからのフェッチ応答を待つ最大時間。               | `1` 秒             |
| Fetch Bytes (Source)                      | Kafka からフェッチするバイト数。設定値がメッセージサイズ未満の場合、フェッチ性能に影響する可能性があります。 | `896` KB           |
| Max Batch Bytes (Sink)                    | Kafka バッチ内でメッセージを収集する最大サイズ（バイト）。Kafka ブローカーのデフォルトは 1 MB ですが、EMQX はメッセージエンコードのオーバーヘッドを考慮し少し小さめに設定。単一メッセージがこのサイズを超える場合は別バッチで送信されます。 | `896` KB           |
| Offset Commit Interval (Source)           | コンシューマーグループごとにオフセットコミット要求を送る間隔。   | `5` 秒             |
| Required Acks (Sink)                      | Kafka パーティションリーダーがフォロワーから受け取る必要のあるアックの種類。<br />`all_isr`: 全てのインシンクレプリカからのアックを要求。<br />`leader_only`: リーダーのみからアックを要求。<br />`none`: Kafka からのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafka プロデューサーがパーティション数の増加を検知する間隔。増加が検知されると、指定された `partition_strategy` に基づき新パーティションにメッセージを分配。 | `60` 秒            |
| Max Inflight (Sink)                       | Kafka プロデューサーがアック受信前に送信可能な最大バッチ数（パーティションごと）。値が大きいほどスループットは向上しますが、1より大きいとメッセージの順序が入れ替わるリスクがあります。 | `10`               |
| Query Mode (Source)                       | メッセージ送信の最適化のため、非同期または同期クエリモードを選択可能。非同期モードでは Kafka 書き込みが MQTT メッセージパブリッシュをブロックしませんが、クライアントが Kafka 到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の最大待機時間。メッセージ送信完了を適時保証します。同期モード時のみ適用。 | `5` 秒             |
| Buffer Mode (Sink)                        | メッセージ送信前のバッファリング方式。<br />`memory`: メモリ上にバッファ。EMQX ノード再起動時にメッセージは失われます。<br />`disk`: ディスク上にバッファ。再起動後もメッセージは保持されます。<br />`hybrid`: 初期はメモリバッファで、一定サイズ超過時に段階的にディスクにオフロード。メモリモード同様に再起動時は失われます。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafka パーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄してバッファ空間を確保。メモリ使用量と性能のバランス調整に有用。 | `2` GB             |
| Segment File Bytes (Sink)                 | バッファモードが `disk` または `hybrid` の場合に適用。メッセージ保存用セグメントファイルのサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが `memory` の場合に適用。メモリ圧迫時に古いバッファメッセージを自動破棄し、システムの安定性を確保。Linux システムのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size         | ソケットバッファサイズを管理し、ネットワーク送信性能を最適化。       | `1024` KB          |
| TCP Keepalive                             | Kafka ブリッジ接続の TCP キープアライブ設定。長時間アイドル状態での接続切断を防止します。カンマ区切りの3つの数値（Idle, Interval, Probes）で指定。Linux のデフォルトはそれぞれ 7200, 75, 9 秒。例: `240,30,5` は 240秒アイドル後にキープアライブを開始し、30秒間隔で最大5回プローブ送信。応答なければ接続切断。 | `none`             |
| Max Batch Age (Sink)                      | プロデューサーバッファ内のメッセージが送信されずに保持可能な最大時間。バッチ内の全メッセージがこの時間を超えるとバッチは破棄される。キューイング中や切断時のバッファメッセージに適用。破棄されたメッセージは `dropped.expired` メトリクスにカウント。デフォルトは無期限（`infinity`）。 | `infinity`         |
| Max Retries (Sink)                        | Kafka がリトライ可能なエラーを返した際の最大リトライ回数。初回試行とリトライがすべて失敗するとバッチは破棄され、メッセージは `failed` メトリクスにカウント。接続損失による再送はリトライ回数に含まれず、`max_batch_age` によって制限。デフォルトは無制限（`infinity`）。 | `infinity`         |
| Reconnect Delay (Sink)                    | 接続喪失後、プロデューサーが Kafka に再接続を試みるまでの遅延時間。切断中もメッセージはバッファに蓄積されるが、バッファ制限や `max_batch_age` の影響を受ける。デフォルトは `2` 秒。 | `2` 秒             |
| Max Linger Time                           | パーティションごとのプロデューサーがより大きなバッチを形成するために待機する最大時間。すべてのバッファモードに適用。デフォルトは `0`（待機なし）でレイテンシ最適化。小さな遅延を許容できる場合は設定すると Kafka へのリクエスト数を減らせる。ディスクバッファの場合はバッチ書き込み前の待機時間で、IOPS 削減のため少なくとも 5ms 推奨。 | `0` ミリ秒        |
| Max Linger Bytes                          | パーティションごとのプロデューサーがバッチ送信を開始するまでに蓄積する最大バイト数。 | `10` MB            |
| Health Check Interval                     | コネクターの稼働状況をチェックする間隔。                         | `15` 秒            |

## さらに詳しく

EMQX は Apache Kafka とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [MQTT と Kafka を使ったコネクテッドビークルのストリーミングデータパイプライン構築：3分ガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTT と Kafka：IoT データ統合の強化](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTT パフォーマンスベンチマークテスト：EMQX-Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterprise パフォーマンスベンチマークテスト：Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [EMQX Cloud ルールエンジンを使ったデバイスデータの Kafka へのブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloud ルールエンジンに関する動画で、今後より適切な動画に差し替え予定）
