# ConfluentへMQTTデータをストリームする

[Confluent Cloud](https://www.confluent.io/)はApache Kafkaをベースとした、レジリエンスが高くスケーラブルでフルマネージドのストリーミングデータサービスです。EMQXはルールエンジンとSinkを通じてConfluentとのデータ統合をサポートし、MQTTデータをConfluentに簡単にストリームしてリアルタイム処理、保存、分析を可能にします。

![EMQX Confluent Integration](./assets/confluent-integration.png)

本ページでは主にConfluent統合の機能と利点を紹介し、Confluent Cloudの設定およびEMQXでのConfluent Producer Sinkの作成方法を案内します。

## 動作の仕組み

Confluentデータ統合はEMQXのすぐに使える機能であり、MQTTベースのIoTデータとConfluentの強力なデータ処理機能を橋渡しします。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、両プラットフォーム間のデータフローと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は自動車IoTにおけるEMQXとConfluentのデータ統合の典型的なアーキテクチャを示しています。

![Confluent Architecture](./assets/confluent-architecture.png)

Confluentへのデータの入出力はConfluent Sink（Confluentへのメッセージ送信）とConfluent Source（Confluentからのメッセージ受信）を介して行われます。Confluent Sinkを作成すると、そのワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：車両に接続されたIoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：これらのMQTTメッセージは、組み込みのルールエンジンとメッセージングサーバーの協調動作により、トピックマッチングルールに従って処理されます。メッセージがルールエンジンに到着し、定義済みの処理ルールを評価します。ペイロード変換を指定するルールがあれば、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Confluentへのブリッジング**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをConfluentに転送するアクションが実行されます。Confluent Sink機能を使い、MQTTトピックをConfluentの事前定義されたKafkaトピックにマッピングし、処理済みのメッセージとデータをこれらのトピックに書き込みます。

車両データがConfluentに入力されると、以下のように柔軟にデータへアクセスし活用できます。

- サービスはConfluentと直接連携し、特定トピックのリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を行えます。
- Kafka Streamsを利用してストリーム処理を行い、車両状態をメモリ内で集約・相関させてリアルタイム監視が可能です。
- ConfluentのStream Designerコンポーネントを使い、MySQLやElasticSearchなど外部システムへのデータ出力用コネクターを選択して保存できます。

## 機能と利点

Confluentとのデータ統合は以下の機能と利点をビジネスにもたらします。

- **大規模メッセージ送信の信頼性**：EMQXとConfluent Cloudは共に高信頼のクラスター機構を用いて安定したメッセージ送信チャネルを構築し、大規模IoTデバイスからのメッセージの損失ゼロを保証します。どちらもノード追加による水平スケールが可能で、突発的な大規模メッセージにも動的にリソースを調整し、メッセージ送信の可用性を確保します。
- **強力なデータ処理能力**：EMQXのローカルルールエンジンとConfluent Cloudはそれぞれ異なる段階で信頼性の高いストリーミングデータ処理を提供します。IoTデバイスからアプリケーションまでのデータに対し、リアルタイムのデータフィルタリング、形式変換、集約解析などシナリオに応じた処理を行い、より複雑なIoTメッセージ処理ワークフローとデータ分析アプリケーションのニーズに応えます。
- **強力な統合機能**：Confluent Cloudが提供する多様なコネクターを通じて、EMQXは他のデータベース、データウェアハウス、データストリーム処理システムなどと容易に統合でき、アジャイルなデータ分析アプリケーションのための完全なIoTデータワークフローを構築します。
- **高スループット処理能力**：同期・非同期の両書き込みモードをサポートし、リアルタイム優先とパフォーマンス優先のデータ書き込み戦略を区別可能で、シナリオに応じてレイテンシとスループットを柔軟にバランスさせられます。
- **効果的なトピックマッピング**：ブリッジ設定を通じて多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティをKafkaヘッダーにマッピングすることをサポートし、1対1、1対多、多対多など多様なトピックマッピング方式を採用し、MQTTトピックフィルター（ワイルドカード）もサポートします。

これらの機能は統合能力と柔軟性を高め、効果的で堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続を介して送信され、さらに効果的に保存・管理されます。

## はじめる前に

本節ではEMQXダッシュボードでConfluentデータ統合を設定するための準備作業を説明します。

### 前提条件

- [ルールエンジン](./rules.md)の理解
- [Sink](./data-bridges.md)の理解

### Confluent Cloudの設定

Confluentデータ統合を作成する前に、Confluent Cloudコンソールでクラスターを作成し、Confluent Cloud CLIを使ってトピックとAPIキーを作成する必要があります。

#### クラスターの作成

1. Confluent Cloudコンソールにログインし、クラスターを作成します。例としてStandardクラスターを選択し、**Begin configuration**をクリックします。

![EMQX Confluent Create Cluster](./assets/confluent_create_cluster_1.2d537cc0.png)

2. リージョン/ゾーンを選択します。デプロイメントリージョンがConfluent Cloudのリージョンと一致していることを確認し、**Continue**をクリックします。

![EMQX Confluent Select Cluster Region](./assets/confluent_create_cluster_2.a8f517c4.png)

3. クラスター名を入力し、**Launch cluster**をクリックします。

![image-20231013105736218](./assets/confluent_create_cluster_3.d38c10a0.png)

#### Confluent Cloud CLIを使ったトピックとAPIキーの作成

クラスターがConfluent Cloudで起動したら、**Cluster Overview** -> **Cluster Settings**ページから**Bootstrap server**のURLを取得できます。

![image-20231013111959327](./assets/confluent_cluster_info.773da650.png)

Confluent Cloud CLIを使ってクラスターを管理できます。以下は基本的なCLIコマンドです。

##### Confluent Cloud CLIのインストール

```bash
curl -sL --http1.1 https://cnfl.io/cli | sh -s -- -b /usr/local/bin
```

既にインストール済みの場合は、以下のコマンドでアップデート可能です。

```bash
confluent update
```

##### アカウントにログイン

```bash
confluent login --save
```

##### 環境を選択

```bash
# 環境一覧表示
confluent environment list
# 環境選択
confluent environment use <environment_id>
```

##### クラスターを選択

```bash
# Kafkaクラスター一覧表示
confluent kafka cluster list
# Kafkaクラスター選択
confluent kafka cluster use <kafka_cluster_id>
```

##### APIキーとシークレットの使用

既存のAPIキーを使う場合は、以下のコマンドでCLIに登録します。

```bash
confluent api-key store --resource <kafka_cluster_id>
Key: <API_KEY>
Secret: <API_SECRET>
```

APIキーとシークレットを持っていない場合は、以下のコマンドで作成できます。

```bash
$ confluent api-key create --resource <kafka_cluster_id>

APIキーの準備には数分かかる場合があります。
APIキーとシークレットは保存してください。シークレットは後から取得できません。
+------------+------------------------------------------------------------------+
| API Key    | YZ6R7YO6Q2WK35X7                                                 |
| API Secret | ****************************************                         |
+------------+------------------------------------------------------------------+
```

CLIに追加後、以下のコマンドでAPIキーとシークレットを使用できます。

```bash
confluent api-key use <API_Key> --resource <kafka_cluster_id>
```

##### トピックの作成

`testtopic-in`という名前のトピックを作成するには以下のコマンドを使います。

```bash
confluent kafka topic create testtopic-in
```

トピック一覧は以下のコマンドで確認できます。

```bash
confluent kafka topic list
```

##### トピックへのメッセージ送信（プロデュース）

以下のコマンドでプロデューサーを起動できます。起動後、メッセージを入力してEnterを押すと、メッセージが該当トピックにパブリッシュされます。

```bash
confluent kafka topic produce testtopic-in
```

##### トピックからのメッセージ受信（コンシューム）

以下のコマンドでコンシューマーを起動し、該当トピックの全メッセージを出力します。

```bash
confluent kafka topic consume -b testtopic-in
```

## コネクターの作成

Confluent Sinkアクションを追加する前に、EMQXとConfluent Cloud間の接続を確立するためにConfluentプロデューサーコネクターを作成する必要があります。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックし、コネクター選択画面で**Confluent Producer**を選択して**Next**をクリックします。
3. 名前と説明を入力します。例：`my-confluent`。名前はConfluent Sinkとコネクターを関連付けるために使われ、クラスター内で一意である必要があります。
4. Confluent Cloudへの接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**：Confluentクラスター設定ページのEndpoints情報に対応します。
   - **Username** と **Password**：先にConfluent Cloud CLIで作成したAPIキーとシークレットを入力します。
   - その他のオプションはデフォルトのままか、ビジネスニーズに応じて設定してください。
5. **Create**ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にConfluent Cloudに接続します。次に、このコネクターを基にルールを作成し、コネクターで設定したConfluentクラスターへデータを転送します。

## Confluent Sinkを使ったルールの作成

本節では、MQTTトピック`t/#`からのメッセージを処理し、処理結果をConfluentの`testtopic-in`トピックに送信するルールの作成方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules**をクリックします。

2. 右上の**Create**をクリックします。

3. ルールIDを入力します。例：`my_rule`

4. MQTTメッセージをトピック`t/#`からConfluentに転送したい場合、**SQL Editor**に以下の文を入力します。

   注意：独自のSQL構文を指定する場合、`SELECT`部分にSinkが必要とする全フィールドを含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注意：初心者の場合は**SQL Example**と**Enable Test**をクリックしてSQLルールを学習・テストできます。

5. + **Add Action**ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action**ドロップダウンリストから`Confluent Producer`を選択し、**Action**ドロップダウンはデフォルトの`Create Action`のままか、既存のConfluent Producerアクションを選択します。本例では新規ルールを作成しアクションを追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-confluent`コネクターを選択します。ドロップダウン横のボタンをクリックするとポップアップで新規コネクターを素早く作成でき、必要な設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sinkのデータ送信方法を設定します。

   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**ドロップダウンから選択可能です。**Add**をクリックしてキー・バリューのペアを追加できます。
   - **Message Key**：Kafkaメッセージのキーです。プレースホルダー（${var}）を含む文字列か純粋な文字列を入力します。
   - **Message Value**：Kafkaメッセージの値です。プレースホルダー（${var}）を含む文字列か純粋な文字列を入力します。
   - **Partition Strategy**：プロデューサーがKafkaのパーティションにメッセージを分配する方法を選択します。
   - **Compression**：Kafkaメッセージ内のレコードを圧縮／解凍するための圧縮アルゴリズムの使用有無を指定します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にこれらがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#advanced-configuration)を参照してください。

11. **Create**ボタンをクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新規Sinkがルールアクションに追加されます。

12. **Create**ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブで新規作成されたConfluent Producer Sinkも確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認できます。トポロジーを通じて、トピック`t/#`配下のメッセージがルール`my_rule`で解析され、Confluentに送信・保存されていることを直感的に把握できます。

## Confluent Producerルールのテスト

Confluent Producerルールが期待通りに動作するかテストするため、[MQTTX](https://mqttx.app/en)を使ってクライアントのMQTTメッセージパブリッシュをシミュレートできます。

1. MQTTXでトピック`t/1`にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Confluent" }'
   ```

2. **Actions(Sink)**ページでSinkの名前をクリックし、統計情報を確認します。Sinkの稼働状況をチェックし、新規受信メッセージ1件、新規送信メッセージ1件があるはずです。

3. 以下のConfluentコマンドでメッセージが`testtopic-in`トピックに書き込まれているか確認します。

   ```bash
   confluent kafka topic consume -b testtopic-in
   ```

## 詳細設定

本節ではコネクターおよびSink/Sourceのパフォーマンスを最適化し、特定のシナリオに応じたカスタマイズ操作を可能にする高度な設定オプションを説明します。該当オブジェクト作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

### コネクター設定

| フィールド                      | 説明                                                                                     | 推奨値           |
| ------------------------------ | ---------------------------------------------------------------------------------------- | ---------------- |
| Allow Auto Topic Creation       | （Producerのみ）有効にすると、クライアントがメタデータ取得リクエストを送信した際に、Kafkaトピックが存在しなければEMQXが自動的にトピックを作成します。 | `Disabled`       |
| Connect Timeout                 | TCP接続確立の最大待機時間（認証有効時は認証時間も含む）                                   | `5`秒            |
| Start Timeout                   | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。SinkがConfluentクラスターなどのリソースが完全に稼働し準備完了するまで処理を進めないようにするための設定。 | `5`秒            |
| Health Check Interval           | コネクターの稼働状態をチェックする間隔                                                    | `15`秒           |
| Min Metadata Refresh Interval   | Kafkaブローカーとトピックのメタデータ更新を行う最小間隔。小さすぎるとKafkaサーバーの負荷が増加する恐れあり。 | `3`秒            |
| Metadata Request Timeout        | Kafkaからメタデータを取得する際の最大待機時間                                            | `5`秒            |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能を最適化するためのソケットバッファサイズ                              | `1`MB            |
| No Delay                       | システムカーネルがTCPソケットを即時送信するか遅延送信するかの設定。オンにすると即時送信される。デフォルトは40ミリ秒の遅延送信。 | `Enabled`        |
| TCP Keepalive                  | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間のアイドル状態による接続切断を防止。<br>値は`Idle, Interval, Probes`の3つの数値をカンマ区切りで指定。<br>Idle：接続がアイドル状態になる秒数（Linuxデフォルト7200秒）<br>Interval：キープアライブプローブ間隔（Linuxデフォルト75秒）<br>Probes：応答なしとみなすまでの最大プローブ回数（Linuxデフォルト9回）<br>例：`240,30,5`は240秒アイドル後にプローブ開始、30秒間隔で5回応答なしで切断判定。 | `none`           |

### Confluent Producer Sink設定

| フィールド                      | 説明                                                                                     | 推奨値           |
| ------------------------------ | ---------------------------------------------------------------------------------------- | ---------------- |
| Health Check Interval           | Sinkの稼働状態をチェックする間隔                                                        | `15`秒           |
| Max Batch Bytes                | Kafkaバッチ内でメッセージを収集する最大バイト数。Kafkaブローカーのデフォルトは1MBだが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮しやや小さめに設定。単一メッセージがこのサイズを超える場合は別バッチで送信される。 | `896`KB          |
| Required Acks                  | Kafkaパーティションリーダーがフォロワーから受け取る必要のあるアックの種類。<br>`all_isr`：全てのインシンクレプリカからのアックを要求<br>`leader_only`：リーダーのみからのアックを要求<br>`none`：Kafkaからのアック不要 | `all_isr`        |
| Partition Count Refresh Interval | Kafkaプロデューサーがパーティション数の増加を検知する間隔。増加検知後、EMQXは`partition_strategy`に基づき新パーティションをメッセージ送信に組み込む。 | `60`秒           |
| Max Inflight                  | Kafkaプロデューサーがアックを受け取る前に送信可能な最大バッチ数（パーティション毎）。値が大きいほどスループットは向上するが、1より大きい場合はメッセージの順序入れ替わりリスクあり。未アックメッセージ数を制御し負荷バランスを取る設定。 | `10`             |
| Query Mode (Producer)          | 非同期または同期のクエリモードを選択し、要件に応じてメッセージ送信を最適化。非同期モードではKafka書き込みがMQTTパブリッシュ処理をブロックしないが、クライアントがKafka到着前にメッセージを受け取る可能性あり。 | `Async`          |
| Synchronous Query Timeout      | 同期モード時の最大待機時間。メッセージ送信完了をタイムリーに保証し、長時間待機を防止。`Sync`モード時のみ有効。 | `5`秒            |
| Buffer Mode                  | メッセージ送信前のバッファリング方法を定義。メモリバッファリングは送信速度向上に寄与。<br>`memory`：メモリにバッファ。EMQXノード再起動時にメッセージは失われる。<br>`disk`：ディスクにバッファ。ノード再起動後もメッセージ保持。<br>`hybrid`：最初はメモリにバッファし、一定サイズ（`segment_bytes`設定参照）を超えるとディスクにオフロード。メモリモード同様、ノード再起動時にメッセージは失われる。 | `memory`         |
| Per-partition Buffer Limit    | Kafkaパーティション毎の最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ領域を確保。メモリ使用量とパフォーマンスのバランス調整に有効。 | `2`GB            |
| Segment File Bytes            | バッファモードが`disk`または`hybrid`の場合に適用。メッセージ保存用のセグメントファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100`MB          |
| Memory Overload Protection    | バッファモードが`memory`の場合に適用。メモリ使用過多時に古いバッファメッセージを自動破棄し、システムの安定性を確保。<br>**注意**：Linuxシステムのみ有効。 | Disabled         |

### <!-- Confluent Consumer Source設定 -->

## 追加情報

EMQXはConfluent/Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクもご参照ください。

**ブログ：**

- [MQTTとKafkaでつなぐコネクテッドビークルのストリーミングデータパイプライン構築](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka｜IoTメッセージングとストリームデータ統合の実践](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)
