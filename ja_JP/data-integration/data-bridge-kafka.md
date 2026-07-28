# Apache Kafka に MQTT データをストリームする

[Apache Kafka](https://kafka.apache.org/) は、高スループットかつリアルタイムデータ処理向けに設計された広く使われているオープンソースの分散イベントストリーミングプラットフォームです。しかし、Kafka クライアントは安定したネットワーク接続と高いシステムリソースを必要とするため、Kafka はエッジ IoT 通信には適していません。IoT シナリオでは、デバイスは一般的に軽量な MQTT プロトコルを使用して、不安定なネットワーク上でも効率的にデータを送信します。

EMQX は MQTT と Kafka/[Confluent](https://www.confluent.io/) を統合し、IoT デバイスとバックエンドシステム間のシームレスなデータストリーミングを実現します。MQTT メッセージは Kafka トピックに取り込まれ、リアルタイム処理、保存、分析に利用される一方で、Kafka トピックのデータは消費されて MQTT クライアントに配信され、適時のアクションをトリガーします。

<img src="./assets/kafka_bridge.jpg" alt="kafka_bridge" style="zoom:67%;" />

本ページでは、EMQX と Kafka のデータ統合について紹介し、統合の作成と検証の手順を解説します。

## 動作概要

Apache Kafka データ統合は EMQX に組み込まれた機能で、MQTT ベースの IoT データを Kafka にストリームし、下流の処理や分析に活用します。組み込みの[ルールエンジン](./rules.md)を活用することで、カスタムコード不要でデータのフィルタリング、変換、ルーティングが可能です。

以下の図は、自動車 IoT シナリオにおける典型的な EMQX–Kafka 統合アーキテクチャを示しています。

<img src="./assets/kafka_architecture.png" alt="kafka_architecture" style="zoom:67%;" />

<!-- 将数据流入或流出 Apache Kafka 需要分别创建 Kafka Sink（向 Kafka 发送消息）和 Kafka Source（从 Kafka 接收消息）。以 Sink 为例，其工作流程如下： -->

Apache Kafka へのデータの流入・流出には、それぞれ Kafka Sink（Kafka へメッセージを送信）と Kafka Source（Kafka からメッセージを受信）を作成します。以下は Kafka Sink のワークフローです。

1. **メッセージ取り込み**: 車両に接続された IoT デバイスは EMQX に MQTT 接続を確立し、定期的に状態データを含むメッセージをパブリッシュします。EMQX はメッセージを受信すると、ルールエンジンでルールマッチングを開始します。
2. **ルールベース処理**: マッチしたルールは、ペイロードのフィルタリング、変換、拡張などを行います。
3. **Kafka へのデータ転送**: ルールエンジンで定義されたルールは、Kafka へのメッセージ転送アクションをトリガーします。Kafka Sink を使い、MQTT トピックはあらかじめ定義した Kafka トピックにマッピングされ、処理済みのメッセージとデータが Kafka トピックに書き込まれます。

Kafka にデータが取り込まれた後は、以下のように消費・処理が行われます。

- バックエンドサービスが Kafka トピックからリアルタイムデータストリームを直接消費。
- Kafka Streams によるリアルタイム集約、相関分析、アナリティクス。
- Kafka Connect による MySQL や Elasticsearch など外部システムへの転送とさらなる処理。

## 特長とメリット

Apache Kafka とのデータ統合は以下の特長とメリットを提供します。

- **信頼性の高い双方向 IoT データメッセージング**: EMQX は不安定なネットワーク環境でも MQTT メッセージを確実に Kafka に転送し、バックエンドからの Kafka メッセージを接続中の IoT クライアントに配信します。
- **ペイロード変換**: メッセージは Kafka に転送する前に SQL ルールでフィルタリング、拡張、変換が可能です。
- **柔軟なトピックマッピング**: MQTT トピックやユーザープロパティを Kafka トピックやヘッダーに柔軟にマッピングでき、1対1、1対多、ワイルドカードベースのマッピングをサポートします。
- **柔軟なパーティション選択戦略**: MQTT トピックやクライアントに基づいて同じ Kafka パーティションにメッセージを転送可能です。
- **高スループット処理**: 同期・非同期の Kafka 書き込みをサポートし、異なるワークロードシナリオに応じてレイテンシとスループットを調整します。
- **ランタイムメトリクス**: 各 Sink や Source の総メッセージ数、成功／失敗数、現在のレートなどのメトリクスを表示可能です。
- **動的設定**: ダッシュボードまたは設定ファイルから Sink と Source を動的に設定できます。

これらの機能により、効率的なデータ取り込みと管理を備えたスケーラブルでレジリエントな IoT データプラットフォームを構築できます。

## はじめる前に

このセクションでは、EMQX ダッシュボードで Kafka Sink と Source を作成する前に必要な準備について説明します。

### 前提条件

- EMQX データ統合の[ルール](./rules.md)に関する知識
- [データ統合](./data-bridges.md)に関する知識

### Kafka サーバーのセットアップ

ここでは macOS を例にインストール手順を示します。以下のコマンドで Kafka をインストール・起動できます。

```bash
wget https://archive.apache.org/dist/kafka/3.3.1/kafka_2.13-3.3.1.tgz

tar -xzf  kafka_2.13-3.3.1.tgz

cd kafka_2.13-3.3.1

# KRaft モードで Kafka を起動
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

bin/kafka-server-start.sh config/kraft/server.properties
```

詳細な手順は [Kafka ドキュメントのクイックスタート](https://kafka.apache.org/41/getting-started/quickstart/)を参照してください。

### Kafka トピックの作成

EMQX でデータ統合を作成する前に、Kafka トピックを作成しておく必要があります。以下のコマンドで Sink 用の `testtopic-in` と Source 用の `testtopic-out` の2つのトピックを作成します。

```bash
bin/kafka-topics.sh --create --topic testtopic-in --bootstrap-server localhost:9092

bin/kafka-topics.sh --create --topic testtopic-out --bootstrap-server localhost:9092
```

## Kafka プロデューサーコネクターの作成

Kafka Sink アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka プロデューサーコネクターを作成します。

1. EMQX ダッシュボードで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックし、コネクター選択画面で **Kafka Producer** を選択して **Next** をクリックします。

3. 名前と説明を入力します。例えば `my-kafka`。名前は Kafka Sink とコネクターを紐付けるために使用され、クラスター内で一意である必要があります。

4. Kafka 接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。※本例では EMQX と Kafka をローカルで起動している想定です。リモート環境の場合は適宜設定を変更してください。

   - **認証**: Kafka クラスターに必要な認証方式を選択します。以下の方式をサポートしています。

     - `None`: 認証なし。
     - `AWS IAM for MSK`: EMQX を Amazon EC2 上にデプロイし、Amazon MSK クラスターに接続する場合に使用。
     - `OAuth`: OAuth 2.0 ベースの認証で、OAuth または OIDC 対応の Kafka クラスターに接続。
     - `Basic Auth`: ユーザー名とパスワードによる認証。`plain`、`scram_sha_256`、`scram_sha_512` のいずれかのメカニズムを選択。
     - `Kerberos`: Kerberos (GSSAPI) 認証。Kerberos プリンシパルとキータブファイルの指定が必要。

     詳細は [認証方式](#authentication-method) を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS** トグルをオンにします。TLS 接続の詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。

   - **詳細設定**（任意）：[詳細設定](#advanced-configurations) を参照してください。

5. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続をテストできます。

6. **Create** をクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的に Kafka に接続します。次に、このコネクターを使って Kafka クラスターにデータを転送するルールを作成します。

### 認証方式

EMQX で Kafka コネクターを作成する際、Kafka クラスターのセキュリティ設定に応じて複数の認証方式を選択可能です。

- **None**: 認証なし。

- **MSK IAM**: EMQX を Amazon EC2 インスタンス上にデプロイし、Amazon MSK クラスターに接続する場合に使用。

  この方式は EC2 インスタンスのメタデータサービスを利用し、IAM ポリシーに基づく認証トークンを生成します。

  ::: tip 重要

  MSK IAM 認証は、EMQX が EC2 インスタンス上で動作し MSK クラスターに接続する場合のみサポートされます。これは EC2 インスタンスメタデータサービスに依存するためです。

  `iptables` や `nftables` でホストレベルのアウトバウンドフィルタリングを行う場合は、`169.254.169.254` へのアクセスをブロックしないでください。EMQX は MSK IAM 認証のためにインスタンスメタデータサービスにアクセスする必要があります。同様の例外は、S3、S3 Tables、DynamoDB、Kinesis など EC2 メタデータから認証情報を取得する AWS ベースの他のコネクターにも適用されます。詳細は [ルールエンジンポリシーとファイアウォールルールによる SSRF 対策](../deploy/cluster/security.md#mitigate-ssrf-with-rule-engine-policy-and-firewall-rules) を参照してください。

  :::

- **OAuth**: OAuth 2.0 ベースの認証で、OAuth または OIDC 対応の Kafka クラスター（例：Confluent Cloud や OAuth 有効化済みの自己管理 Kafka）に接続します。

  EMQX は OAuth 2.0 クライアントとして動作し、OAuth 認可サーバーから定期的にアクセストークンを取得します。取得したトークンは SASL/OAUTHBEARER メカニズムを通じて Kafka ブローカーの認証に使用されます。

  必須項目は以下の通りです。

  - **OAuth Grant Type**: アクセストークン取得に使用する OAuth 2.0 グラントタイプ（現状は `client_credentials` のみ対応）。
  - **OAuth Token Endpoint URI**: OAuth/OIDC プロバイダーのトークンエンドポイント URI。EMQX はここにトークンリクエストを送信します。
  - **OAuth Client ID**: OAuth 認可サーバーに登録されたクライアント ID。
  - **OAuth Client Secret**: クライアント ID に対応するシークレット。トークン取得時の認証に使用。
  - **OAuth Request Scope**: （任意）トークンリクエストに含めるスコープ。
  - **SASL Extensions**: （高度な設定、任意）

    認証時に SASL 拡張として送信される追加のキー・バリュー。Confluent Cloud など一部の Kafka プロバイダーで、以下のようなメタデータを渡すために必要です。

    - `logicalCluster`
    - `identityPoolId`

    必要な拡張と値は Kafka クラスターや OAuth プロバイダーの設定に依存します。Confluent Cloud における OAuth/OIDC 認証の詳細は [公式ドキュメント](https://docs.confluent.io/cloud/current/security/authenticate/workload-identities/identity-providers/oauth/overview.html) を参照してください。

- **Basic Auth**: ユーザー名とパスワードによる認証。

  選択時は以下を指定します。

  - **Mechanism**: `plain`、`scram_sha_256`、`scram_sha_512` のいずれか。
  - **Username** と **Password**: Kafka クラスター認証用の資格情報。

- **Kerberos**: Kerberos GSSAPI 認証。

  以下を指定します。

  - **Kerberos Principal**: 認証に使用する Kerberos プリンシパル。
  - **Kerberos Keytab File**: 非対話認証に使用するキータブファイルのパス。

  ::: tip 重要

  キータブファイルは全ての EMQX ノードで同じパスに配置し、EMQX サービスユーザーが読み取り可能である必要があります。

  :::

## Kafka Sink を使ったルールの作成

このセクションでは、MQTT トピック `t/#` からのメッセージを処理し、処理結果を Kafka の `testtopic-in` トピックに送信する Kafka Sink を使ったルールの作成方法を示します。

1. EMQX ダッシュボードで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルール ID を入力します。例：`my_rule`

4. **SQL Editor** に以下の文を入力します。これは MQTT トピック `t/#` からのメッセージを Kafka に転送する例です。

   ※独自の SQL 文を指定する場合は、Sink が必要とする全てのフィールドを `SELECT` 部分に含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   ::: tip

   初心者の方は **SQL Examples** と **Try It Out** をクリックして、SQL ルールの学習とテストができます。

   :::

   ::: tip

   EMQX v5.7.2 からルール SQL 内で環境変数を読み取る機能が追加されました。詳細は [ルール SQL で環境変数を使う](#use-environment-variables) を参照してください。

   :::

5. **Create Rule** ページで + **Add Action** をクリックし、ルールの出力先を定義します。

6. **Type of Action** ドロップダウンから `Kafka Producer` を選択します。

   **Action** ドロップダウンはデフォルトの `Create Action` のままにします。

   > 既存の Sink を選択することも可能ですが、本例では新規作成します。

7. **Name** と任意で **Description** を入力します。

8. **Connector** ドロップダウンから先ほど作成した `my-kafka` コネクターを選択します。必要に応じて新規コネクターの作成も可能です。[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

9. Sink のデータ送信方法を設定します。

      - **Kafka Topic**: メッセージをパブリッシュする Kafka トピック。`testtopic-in` と入力します。EMQX v5.7.2 以降、このフィールドは動的トピック設定もサポートします。[変数テンプレートの使用](#use-variable-templates)を参照してください。
      - **Kafka Headers**: Kafka メッセージに付加する任意のキー・バリュー形式のメタデータ。ヘッダー値はオブジェクトとして解決される必要があります。エンコード方式は **Kafka Header Value Encode Type** で選択でき、複数のヘッダーを追加可能です。
      - **Message Key**: Kafka メッセージのキー。パーティション分散やメッセージ順序付けに使用します。静的文字列や `${.clientid}` のようなプレースホルダーを含めることができます。
      - **Message Value**: Kafka メッセージのペイロード。テンプレートからレンダリングされます。静的文字列や `${.}` のようなプレースホルダーでルールコンテキストから動的生成可能です。テンプレートが `NULL`（例えば参照フィールドが存在しない場合）に解決されると、空文字列ではなく Kafka の `NULL` 値が生成されます。
      - **Message Timestamp**: Kafka メッセージのタイムスタンプ。固定値または `${timestamp}` のようなプレースホルダーでルール出力から動的に設定可能です。
      - **Partition Strategy**: プロデューサーがメッセージを Kafka パーティションに分散する方法を選択します。
      - **Partitions Limit**: プロデューサーがメッセージを送信可能な最大パーティション数を制限します。有効にすると、全パーティションではなく指定したパーティション数内でメッセージを分散します。
      - **Compression**: Kafka メッセージのレコードを圧縮・解凍する圧縮アルゴリズムを指定します。

10. **フォールバックアクション**（任意）: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。詳細は [フォールバックアクション](./data-bridges.md#fallback-actions) を参照してください。

11. **詳細設定**（任意）: [詳細設定](#advanced-configuration) を参照してください。

12. **Create** をクリックして Sink の作成を完了します。作成後は **Create Rule** ページに戻り、新規 Sink がルールアクションに追加されます。

13. **Create** をクリックしてルール作成を完了します。

![kafka_producer_bridge](./assets/kafka_producer_bridge.png)

これでルールが正常に作成され、**Integration** -> **Rules** ページに新規ルールが表示され、**Actions(Sink)** タブに新規 Kafka プロデューサー Sink が確認できます。

また、**Integration** -> **Flow Designer** を開くとトポロジーが表示され、トピック `t/#` のメッセージがルール `my_rule` によって解析され Kafka に送信・保存されている様子を直感的に確認できます。

### Kafka 動的トピックの設定

EMQX v5.7.2 以降、Kafka プロデューサー Sink の設定で環境変数や変数テンプレートを使い Kafka トピックを動的に設定できます。本節ではこれら2つのユースケースを紹介します。

#### 環境変数の利用

EMQX v5.7.2 では、ルール SQL 処理段階で [環境変数](../configuration/configuration.md#environment-variables) の値を動的にメッセージ内のフィールドに割り当てる機能が追加されました。この機能はルールエンジンの組み込み SQL 関数の [getenv](../data-integration/rule-sql-builtin-functions.md#system-function) を使い、EMQX の環境変数を取得します。取得した値は SQL 処理結果にセットされます。この機能を活用し、Kafka Sink のルールアクションで Kafka トピックをルール出力のフィールドから設定できます。以下はその例です。

::: tip 注意

ルールエンジンが読み取る環境変数名は、他のシステム環境変数の漏洩を防ぐために必ず `EMQXVAR_` プレフィックスを付ける必要があります。例えば `getenv` 関数で `KAFKA_TOPIC` を読み取る場合、環境変数名は `EMQXVAR_KAFKA_TOPIC` と設定してください。

:::

1. Kafka を起動し、`testtopic-in` トピックを事前作成します。[はじめる前に](#before-you-start)を参照してください。

2. EMQX を起動し、環境変数を設定します。zip インストールの場合は起動時に直接指定可能です。例として Kafka トピック `testtopic-in` を環境変数 `EMQXVAR_KAFKA_TOPIC` に設定します。

   ```bash
   EMQXVAR_KAFKA_TOPIC=testtopic-in bin/emqx start
   ```

3. コネクターを作成します。[Kafka プロデューサーコネクターの作成](#create-a-kafka-producer-connector)を参照してください。

4. Kafka Sink ルールを設定し、**SQL Editor** に以下の文を入力します。

   ```sql
   SELECT
     getenv('KAFKA_TOPIC') as kafka_topic,
     payload
   FROM
     "t/#"
   ```

   ![kafka_dynamic_topic_sql](./assets/kafka_dynamic_topic_sql.png)

5. SQL テストを有効にし、環境変数値 `testtopic-in` が正しく取得できることを確認します。

   ![kafka_dynamic_topic_sql_test](./assets/kafka_dynamic_topic_sql_test.png)

6. Kafka プロデューサー Sink のアクションを追加します。ルールの右側の **Action Outputs** で **Add Action** をクリックします。

   - **Connector**: 先ほど作成したコネクター `test-kafka` を選択。
   - **Kafka Topic**: SQL ルール出力の変数テンプレート `${kafka_topic}` を指定。

   ![kafka_dynamic_topic](./assets/kafka_dynamic_topic.png)

7. [Kafka Sink を使ったルールの作成](#create-a-rule-with-kafka-sink)を参照し、追加設定を完了して **Create** をクリックしルール作成を完了します。

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

**Kafka Topic** フィールドに静的なトピック名を設定する代わりに、変数テンプレートを使って動的にトピックを生成できます。これによりメッセージ内容に基づいて Kafka トピックを構築でき、柔軟なメッセージ処理と配信が可能です。例えば、`device-${payload.device}` のように指定すると、特定デバイスのメッセージを `device-1` のようなデバイス ID 付きトピックに簡単に送信できます。

この例では、Kafka に送信するメッセージペイロードに `device` キーが含まれている必要があります。以下は例です。

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

また、Kafka 側で解決される全てのトピック（例：`device-1`、`device-2` など）を事前に作成しておく必要があります。テンプレートが存在しないトピック名に解決されると、同様にメッセージは回復不能なエラーで破棄されます。

## Kafka プロデューサールールのテスト

Kafka プロデューサールールが期待通り動作するかをテストするために、[MQTTX](https://mqttx.app/en) を使って EMQX に MQTT メッセージをパブリッシュするクライアントをシミュレートできます。

1. MQTTX でトピック `t/1` にメッセージを送信します。

```bash
mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Kafka" }'
```

2. **Actions(Sink)** ページで Sink 名をクリックし統計情報を確認します。新規の受信メッセージ数と送信メッセージ数がそれぞれ1件増えているはずです。

3. 以下のコマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   bin/kafka-console-consumer.sh --bootstrap-server 127.0.0.1:9092  --topic testtopic-in
   ```

<!--TODO 5.4 refactor-->

## Kafka コンシューマーコネクターの作成

Kafka Source アクションを追加する前に、EMQX と Kafka 間の接続を確立する Kafka コンシューマーコネクターを作成します。

1. EMQX ダッシュボードで **Integration** -> **Connector** を開きます。

2. 画面右上の **Create** をクリックします。

3. **Create Connector** ページで **Kafka Consumer** を選択し、**Next** をクリックします。

4. ソースの名前を入力します。英数字の組み合わせで、例：`my-kafka-source`

5. ソースの接続情報を入力します。
   - **Bootstrap Hosts**: `127.0.0.1:9092` と入力します。※本例では EMQX と Kafka をローカルで起動している想定です。リモート環境の場合は適宜設定を変更してください。

   - **認証**: Kafka クラスターに必要な認証方式を選択します。以下の方式をサポートしています。

     - `None`: 認証なし。
     - `authentication_msk_iam`: EMQX を EC2 インスタンス上にデプロイし AWS MSK クラスターに接続する場合に使用。
     - `OAuth`: [OAuth 2.0](https://oauth.net/2/) を使った認証。
     - `Basic Auth`: **Mechanism**（`plain`、`scram_sha_256`、`scram_sha_512`）の選択と **Username**、**Password** の指定が必要。
     - `Kerberos`: **Kerberos Principal** と **Kerberos Keytab File** の指定が必要。

     詳細は [認証方式](#authentication-method) を参照してください。

   - 暗号化接続を確立する場合は、**Enable TLS** トグルをオンにします。TLS 接続の詳細は **TLS for External Resource Access** を参照してください。

   - **詳細設定**（任意）：[詳細設定](#advanced-configuration) を参照してください。

6. **Create** をクリックする前に、**Test Connection** をクリックして Kafka サーバーへの接続をテストできます。

11. **Create** をクリックします。関連するルールを作成するオプションが表示されます。[Kafka コンシューマー Source を使ったルールの作成](#create-a-rule-with-kafka-consumer-source)を参照してください。

## Kafka コンシューマー Source を使ったルールの作成

このセクションでは、Kafka コンシューマー Source で転送されたメッセージをさらに処理し、MQTT トピックに再パブリッシュするルールの作成方法を示します。

### ルール SQL の作成

1. EMQX ダッシュボードで **Integration** -> **Rules** を開きます。

2. 画面右上の **Create** をクリックします。

3. ルール ID を入力します。例：`my_rule`

4. Kafka Source `$bridges/kafka_consumer:<sourceName>` から変換されたメッセージを EMQX に転送する場合、**SQL Editor** に以下の文を入力します。

   ※独自の SQL 文を指定する場合は、後で設定する再パブリッシュアクションに必要な全てのフィールドを `SELECT` 部分に含めてください。Kafka Source の `SELECT` 文では `ts_type`、`topic`、`ts`、`event`、`headers`、`key`、`metadata`、`value`、`timestamp`、`offset`、`node` などのフィールドが使用可能です。

   ```sql
   SELECT
     *
   FROM
     "$bridges/kafka_consumer:<sourceName>"
   ```

   ※初心者の方は **SQL Examples** と **Enable Test** をクリックして SQL ルールの学習とテストが可能です。

### Kafka コンシューマー Source をデータ入力に追加

1. ルール作成画面の右側の **Data Inputs** タブを選択し、**Add Input** をクリックします。

2. **Input Type** ドロップダウンから **Kafka Consumer** を選択します。**Source** ドロップダウンはデフォルトの `Create Source` のままにするか、既存の Kafka コンシューマー Source を選択します。本例では新規作成してルールに追加します。

3. ソースの名前と説明を入力します。

4. **Connector** ドロップダウンから先ほど作成した `my-kafka-consumer` コネクターを選択します。必要に応じて隣のボタンから新規コネクターを作成可能です。[Kafka コンシューマーコネクターの作成](#create-a-kafka-consumer-connector)を参照してください。

5. 以下のフィールドを設定します。

   - **Kafka Topic**: コンシューマーソースがサブスクライブする Kafka トピックを指定します。
   - **Group ID**: このソースのコンシューマーグループ識別子を指定します。未指定の場合はソース名に基づいて自動生成されます。
   - **Key Encoding Mode** と **Value Encoding Mode**: Kafka メッセージのキーと値のエンコード方式を選択します。

7. **Offset Reset Policy**: コンシューマーが Kafka トピックパーティションのどのオフセットから読み始めるかを指定します。

   - `latest` を選択すると、コンシューマーは最新のオフセットから読み始め、開始前のメッセージはスキップします。
   - `earliest` を選択すると、コンシューマーはパーティションの先頭から読み始め、開始前のメッセージも含めて全履歴データを読みます。

8. **詳細設定**（任意）：[詳細設定](#advanced-configuration) を参照してください。

9. **Create** をクリックする前に、**Test Connectivity** をクリックして Kafka サーバーへの接続をテストできます。

10. **Create** をクリックしてソース作成を完了します。ルール作成画面の **Data Inputs** タブに新規ソースが表示されます。

### 再パブリッシュアクションの追加

1. **Action Outputs** タブを選択し、+ **Add Action** ボタンをクリックしてルールでトリガーされるアクションを定義します。

2. **Type of Action** ドロップダウンから **Republish** を選択します。

3. **Topic** と **Payload** フィールドに、再パブリッシュしたいメッセージのトピックとペイロードを入力します。例として `t/1` と `${.}` を入力します。

   - **Topic** フィールドでは `${}` を使って動的に MQTT トピックを指定可能です。例：`t/${key}`（`${}` 内のパラメータは SQL の `SELECT` 文に含まれている必要があります）。

4. **Add** をクリックしてアクションをルールに追加します。

5. ルール作成画面に戻り、**Save** をクリックします。

![Kafka_consumer_rule](./assets/Kafka_consumer_rule.png)

## Kafka Source ルールのテスト

Kafka Source とルールが期待通り動作するかをテストするために、[MQTTX](https://mqttx.app/) を使って EMQX のトピックをサブスクライブするクライアントをシミュレートし、Kafka プロデューサーで Kafka トピックにデータを生成します。EMQX が Kafka からのデータをクライアントがサブスクライブするトピックに再パブリッシュできるか確認します。

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

4. MQTTX のサブスクリプションで以下の Kafka からのメッセージがトピック `t/1` に届くはずです。

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

このセクションでは、データ統合のパフォーマンス最適化や特定シナリオに応じたカスタマイズのための詳細設定オプションを説明します。コネクター、Sink、Source 作成時に **Advanced Settings** を展開し、ビジネス要件に応じて以下の設定を行えます。

| 項目                                     | 説明                                                         | 推奨値             |
| ---------------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation                | （プロデューサーコネクターのみ）有効にすると、クライアントがメタデータ取得リクエスト時に存在しない Kafka トピックを自動作成可能。 | `disabled`         |
| Min Metadata Refresh Interval            | クライアントが Kafka ブローカーやトピックのメタデータを更新する最小間隔。小さすぎると Kafka サーバーの負荷が増加する可能性あり。 | `3` 秒             |
| Metadata Request Timeout                 | ブリッジが Kafka にメタデータ要求を送信する際の最大待機時間。 | `5` 秒             |
| Connect Timeout                          | TCP 接続確立の最大待機時間。認証有効時は認証時間も含む。       | `5` 秒             |
| Max Wait Time (Source)                   | Kafka ブローカーからのフェッチ応答を待つ最大時間。             | `1` 秒             |
| Fetch Bytes (Source)                     | フェッチリクエストで Kafka から取得するバイトサイズ。設定値がメッセージサイズより小さいとフェッチ性能が低下する可能性あり。 | `896` KB           |
| Max Batch Bytes (Sink)                   | Kafka バッチ内でメッセージを収集する最大バイト数。Kafka ブローカーのデフォルトは 1MB だが、EMQX はメッセージエンコードのオーバーヘッドを考慮しやや小さめに設定。単一メッセージが上限を超える場合は別バッチとして送信。 | `896` KB           |
| Offset Commit Interval (Source)          | コンシューマーグループごとにオフセットコミット要求を送る間隔。 | `5` 秒             |
| Required Acks (Sink)                     | Kafka パーティションリーダーがフォロワーから待つアックの種類：<br />`all_isr`: 全てのインシンクレプリカからのアックを要求。<br />`leader_only`: パーティションリーダーのみからのアックを要求。<br />`none`: Kafka からのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval (Source) | Kafka プロデューサーがパーティション数増加を検知する間隔。増加検知後、指定の `partition_strategy` に基づき新パーティションにメッセージを振り分ける。 | `60` 秒            |
| Max Inflight (Sink)                      | Kafka プロデューサーがアック受信前に送信可能な最大バッチ数（パーティションごと）。値が大きいほどスループット向上。ただし 1 より大きいとメッセージの順序入れ替わりリスクあり。 | `10`               |
| Query Mode (Source)                      | 非同期または同期クエリモードを選択し、メッセージ送信を最適化。非同期モードでは Kafka 書き込みが MQTT パブリッシュ処理をブロックしないが、クライアントが Kafka 到着前にメッセージを受信する可能性あり。 | `Async`            |
| Synchronous Query Timeout (Sink)          | 同期クエリモード時の確認待機最大時間。メッセージ送信完了を適時保証し、長時間待機を防止。同期モード時のみ有効。 | `5` 秒             |
| Buffer Mode (Sink)                       | メッセージ送信前にバッファリングするかどうかを定義。メモリバッファリングは送信速度向上に寄与。<br />`memory`: メモリにバッファ。EMQX ノード再起動時にメッセージは失われる。<br />`disk`: ディスクにバッファ。EMQX ノード再起動後もメッセージは保持。<br />`hybrid`: 初めはメモリにバッファし、一定サイズ超過時に段階的にディスクにオフロード。メモリモード同様、ノード再起動時はメッセージが失われる。 | `memory`           |
| Per-partition Buffer Limit (Sink)         | Kafka パーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ領域を確保。メモリ使用量とパフォーマンスのバランス調整に有効。 | `2` GB             |
| Segment File Bytes (Sink)                | バッファモードが `disk` または `hybrid` の場合に適用。メッセージ保存用の分割ファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection (Sink)         | バッファモードが `memory` の場合に適用。メモリ圧迫時に古いバッファメッセージを自動破棄し、システムの安定性を確保。Linux システムのみ有効。 | `Enabled`          |
| Socket Send / Receive Buffer Size        | ネットワーク送受信性能最適化のためのソケットバッファサイズ管理。 | `1024` KB          |
| TCP Keepalive                            | Kafka ブリッジ接続に TCP キープアライブ機能を有効化し、長時間のアイドルによる接続切断を防止。値はカンマ区切りの3つの数値（Idle, Interval, Probes）で指定。<br />Idle: サーバーがキープアライブプローブを開始するまでのアイドル秒数（Linux デフォルト 7200 秒）。<br />Interval: 各キープアライブプローブ間の秒数（Linux デフォルト 75 秒）。<br />Probes: 応答なしと判断するまでの最大プローブ回数（Linux デフォルト 9 回）。<br />例：`240,30,5` は 240 秒のアイドル後にプローブ開始、30 秒間隔で最大 5 回送信し応答なしなら接続切断。 | `none`             |
| Max Linger Time                          | パーティションごとのプロデューサーがメッセージをバッファするために待機する最大時間。デフォルト `0` は待機なし。メモリ以外のバッファモードでは `5ms` に設定すると IOPS を大幅に削減できるがレイテンシは増加。 | `0` ミリ秒         |
| Max Linger Bytes                         | パーティションごとのプロデューサーがメッセージをバッファするために待機する最大バイト数。 | `10` MB            |
| Health Check Interval                    | コネクターの稼働状態をチェックする間隔。                         | `15` 秒            |

## さらに詳しく

EMQX は Apache Kafka とのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクから詳細を学べます。

**ブログ:**

- [MQTT と Kafka でつなぐコネクテッドビークルのストリーミングデータパイプライン：3分でわかるガイド](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTT と Kafka：IoT データ統合の強力な組み合わせ](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTT パフォーマンスベンチマークテスト：EMQX-Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterprise パフォーマンスベンチマークテスト：Kafka 統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**動画:**

- [EMQX Cloud ルールエンジンを使ったデバイスデータの Kafka ブリッジ](https://www.emqx.com/en/resources/bridge-device-data-to-kafka-using-the-emqx-cloud-rule-engine)（Cloud ルールエンジンに関する動画で、将来的により適切な動画に差し替え予定）
