# ConfluentへのMQTTデータストリーミング

[Confluent Cloud](https://www.confluent.io/)はApache Kafkaを基盤とした、レジリエントでスケーラブルかつフルマネージドのストリーミングデータサービスです。EMQXはルールエンジンとSinkを通じてConfluentとのデータ統合をサポートし、MQTTデータをConfluentに簡単にストリーミングしてリアルタイム処理、保存、分析を可能にします。

![EMQX Confluent統合](./assets/confluent-integration.png)

本ページでは主にConfluent統合の機能と利点を紹介し、Confluent Cloudの設定およびEMQXでのConfluent Producer Sinkの作成方法を案内します。

## 動作概要

Confluentデータ統合はEMQXのすぐに使える機能であり、MQTTベースのIoTデータとConfluentの強力なデータ処理機能を橋渡しします。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、両プラットフォーム間のデータフローと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は自動車IoTにおけるEMQXとConfluentのデータ統合の典型的なアーキテクチャを示しています。

![Confluentアーキテクチャ](./assets/confluent-architecture.png)

Confluentへのデータの入出力は、Confluent Sink（Confluentへのメッセージ送信）とConfluent Source（Confluentからのメッセージ受信）を介して行われます。Confluent Sinkを作成した場合のワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：車両に接続されたIoTデバイスはMQTTプロトコルでEMQXに正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：これらのMQTTメッセージは、組み込みルールエンジンとメッセージサーバーの連携により、トピックマッチングルールに基づいて処理されます。メッセージが到着してルールエンジンを通過すると、事前に定義された処理ルールが評価されます。ペイロード変換を指定するルールがあれば、データ形式変換、特定情報のフィルタリング、追加コンテキストによるペイロードの拡充などの変換が適用されます。
3. **Confluentへのブリッジ**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをConfluentに転送するアクションが実行されます。Confluent Sink機能を使い、MQTTトピックをConfluentのKafkaトピックにマッピングし、処理済みのメッセージとデータをこれらのトピックに書き込みます。

車両データがConfluentに入力されると、以下のように柔軟にアクセス・活用できます。

- サービスはConfluentと直接連携し、特定トピックのリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を行えます。
- Kafka Streamsを利用したストリーム処理や、車両状態のメモリ内集約・相関によるリアルタイム監視が可能です。
- ConfluentのFlowデザイナーコンポーネントを使い、MySQLやElasticSearchなど外部システムへのデータ出力用コネクターを選択して保存できます。

## 機能と利点

Confluentとのデータ統合は以下の機能と利点をビジネスにもたらします。

- **大規模メッセージ伝送の信頼性**：EMQXとConfluent Cloudは共に高信頼のクラスター機構を用い、安定かつ信頼性の高いメッセージ伝送チャネルを構築し、大規模IoTデバイスからのメッセージのロスゼロを保証します。両者ともノード追加による水平スケールが可能で、突発的な大規模メッセージにも動的にリソースを調整し、メッセージ伝送の可用性を確保します。
- **強力なデータ処理能力**：EMQXのローカルルールエンジンとConfluent Cloudはそれぞれ異なる段階でIoTデバイスからアプリケーションへのデータ処理を担い、リアルタイムのデータフィルタリング、形式変換、集約分析などをシナリオに応じて提供し、より複雑なIoTメッセージ処理ワークフローを実現し、データ分析アプリケーションの要件を満たします。
- **高い統合能力**：Confluent Cloudが提供する多様なコネクターを通じて、EMQXは他のデータベース、データウェアハウス、データストリーム処理システムなどと容易に統合でき、柔軟なデータ分析アプリケーションのための完全なIoTデータワークフローを構築します。
- **高スループット処理能力**：同期・非同期の両書き込みモードをサポートし、リアルタイム優先と性能優先のデータ書き込み戦略を使い分け、シナリオに応じてレイテンシとスループットを柔軟にバランスできます。
- **効果的なトピックマッピング**：ブリッジ設定を通じて多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティをKafkaヘッダーにマッピングでき、1対1、1対多、多対多など多様なトピックマッピング方式を採用し、MQTTトピックフィルター（ワイルドカード）もサポートします。

これらの機能は統合能力と柔軟性を高め、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続で伝送され、さらに効果的に保存・管理されます。

## はじめる前に

このセクションではEMQXダッシュボードでConfluentデータ統合を設定するための準備作業について説明します。

### 前提条件

- [ルールエンジン](./rules.md)の理解
- [Sink](./data-bridges.md)の理解

### Confluent Cloudの設定

Confluentデータ統合を作成する前に、Confluent CloudコンソールでConfluentクラスターを作成し、Confluent Cloud CLIを使ってトピックとAPIキーを作成する必要があります。

#### クラスターの作成

1. Confluent Cloudコンソールにログインし、クラスターを作成します。例としてStandardクラスターを選択し、**Begin configuration**をクリックします。

![EMQX Confluent クラスター作成](./assets/confluent_create_cluster_1.2d537cc0.png)

2. リージョン/ゾーンを選択します。デプロイリージョンがConfluent Cloudのリージョンと一致していることを確認し、**Continue**をクリックします。

![EMQX Confluent クラスターリージョン選択](./assets/confluent_create_cluster_2.a8f517c4.png)

3. クラスター名を入力し、**Launch cluster**をクリックします。

![image-20231013105736218](./assets/confluent_create_cluster_3.d38c10a0.png)

#### Confluent Cloud CLIでトピックとAPIキーを作成

クラスターがConfluent Cloudで稼働したら、**Cluster Overview** -> **Cluster Settings**ページから**Bootstrap server**のURLを取得できます。

![image-20231013111959327](./assets/confluent_cluster_info.773da650.png)

Confluent Cloud CLIでクラスターを管理できます。以下は基本的なCLIコマンドです。

##### Confluent Cloud CLIのインストール

```bash
curl -sL --http1.1 https://cnfl.io/cli | sh -s -- -b /usr/local/bin
```

すでにインストール済みの場合は、以下でアップデート可能です。

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

既存のAPIキーを使う場合は、以下でCLIに登録します。

```bash
confluent api-key store --resource <kafka_cluster_id>
Key: <API_KEY>
Secret: <API_SECRET>
```

APIキーとシークレットがない場合は、以下で作成可能です。

```bash
$ confluent api-key create --resource <kafka_cluster_id>

APIキーの準備には数分かかる場合があります。
APIキーとシークレットは保存してください。シークレットは後で取得できません。
+------------+------------------------------------------------------------------+
| API Key    | YZ6R7YO6Q2WK35X7                                                 |
| API Secret | ****************************************                         |
+------------+------------------------------------------------------------------+
```

CLIに登録後、以下でAPIキーとシークレットを使用します。

```bash
confluent api-key use <API_Key> --resource <kafka_cluster_id>
```

##### トピックの作成

`testtopic-in`という名前のトピックを作成するには以下を実行します。

```bash
confluent kafka topic create testtopic-in
```

トピック一覧は以下で確認できます。

```bash
confluent kafka topic list
```

##### トピックへのメッセージ送信（Producer）

以下でプロデューサーを起動し、メッセージを入力してEnterを押すとトピックに送信されます。

```bash
confluent kafka topic produce testtopic-in
```

##### トピックからのメッセージ受信（Consumer）

以下でコンシューマーを起動し、トピック内のすべてのメッセージを出力します。

```bash
confluent kafka topic consume -b testtopic-in
```

## コネクターの作成

Confluent Sinkアクションを追加する前に、EMQXとConfluent Cloud間の接続を確立するためConfluentプロデューサーコネクターを作成する必要があります。

1. EMQXダッシュボードで**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックし、コネクター選択画面で**Confluent Producer**を選択して**Next**をクリックします。

3. 名前と説明を入力します（例：`my-confluent`）。この名前はConfluent Sinkとコネクターの紐付けに使われ、クラスター内で一意である必要があります。

4. Confluent Cloudへの接続に必要なパラメーターを設定します。
   - **Bootstrap Hosts**：Confluent Cloudクラスター設定ページの**Endpoints**欄のエンドポイント情報を入力します。
   - **Authentication**：Confluent Cloudクラスターで必要な認証方式を選択します。
     - **Basic auth**：Confluent Cloudで作成したAPI KeyとAPI Secretに対応する**Username**と**Password**を入力します。
     - **OAuth**：Confluent CloudのOAuth/OIDC設定に従い、トークンエンドポイント、クライアントID、クライアントシークレットなどOAuthパラメーターを設定します。
       
       OAuth設定はKafkaコネクターと同様です。詳細は[認証方式](./data-bridge-kafka.md#authentication-method)を参照してください。
   - その他のオプションはデフォルトのままか、ビジネス要件に応じて設定してください。

5. **Create**ボタンをクリックしてコネクター作成を完了します。

作成後、コネクターは自動的にConfluent Cloudに接続します。次に、このコネクターを基にルールを作成し、コネクターで設定したConfluentクラスターにデータを転送します。

## Confluent Sinkを使ったルールの作成

このセクションでは、MQTTトピック`t/#`のメッセージを処理し、処理結果をConfluentの`testtopic-in`トピックに送信するルールの作成方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules**をクリックします。

2. 右上の**Create**をクリックします。

3. ルールIDを入力します（例：`my_rule`）。

4. MQTTメッセージをトピック`t/#`からConfluentに転送したい場合、**SQL Editor**に以下の文を入力します。

   注意：独自のSQL文を指定する場合は、`SELECT`部分にSinkが必要とするすべてのフィールドを含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   初心者の場合は、**SQL Example**や**Enable Test**をクリックしてSQLルールを学習・テストできます。

5. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action**のドロップダウンリストから`Confluent Producer`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のConfluent Producerアクションを選択します。この例では新規ルールを作成し、ルールに追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-confluent`コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを素早く作成できます。必要な設定パラメーターは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sinkのデータ送信方法を設定します。

   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**ドロップダウンから選択可能です。**Add**をクリックしてキー・バリューのペアを追加できます。
   - **Message Key**：Kafkaメッセージのキーを入力します。純粋な文字列か、プレースホルダー（${var}）を含む文字列が指定可能です。
   - **Message Value**：Kafkaメッセージの値を入力します。純粋な文字列か、プレースホルダー（${var}）を含む文字列が指定可能です。
   - **Partition Strategy**：プロデューサーがKafkaのパーティションにメッセージを分配する方法を選択します。
   - **Compression**：Kafkaメッセージ内のレコードを圧縮／解凍するための圧縮アルゴリズムを指定します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#advanced-configuration)を参照してください。

11. **Create**ボタンをクリックしてSinkの作成を完了します。作成後は**Create Rule**ページに戻り、新しいSinkがルールアクションに追加されます。

12. **Create**ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブで新規Confluent Producer Sinkも確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを表示できます。トポロジーから、トピック`t/#`のメッセージがルール`my_rule`で解析され、Confluentに送信・保存されている様子が直感的に把握できます。

## Confluent Producerルールのテスト

Confluent Producerルールが期待通り動作するかテストするため、[MQTTX](https://mqttx.app/en)を使ってクライアントがEMQXにMQTTメッセージをパブリッシュする動作をシミュレートできます。

1. MQTTXでトピック`t/1`にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Confluent" }'
   ```

2. **Actions(Sink)**ページでSink名をクリックし統計情報を確認します。Sinkの稼働状況をチェックし、新規受信メッセージ数と送信メッセージ数がそれぞれ1件ずつ増えていることを確認します。

3. 以下のConfluentコマンドで`testtopic-in`トピックにメッセージが書き込まれているか確認します。

   ```bash
   confluent kafka topic consume -b testtopic-in
   ```

## 詳細設定

このセクションでは、コネクターやSink/Sourceのパフォーマンス最適化やシナリオに応じたカスタマイズ操作のための詳細設定オプションを説明します。対応するオブジェクト作成時に**Advanced Settings**を展開し、ビジネス要件に応じて以下の設定を行えます。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producerのみ）有効にすると、クライアントがメタデータ取得要求を送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証有効時は認証時間も含む）       | `5`秒              |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。Sinkが接続先リソース（例：Confluentクラスター）の完全稼働を確認するまで処理を進めないための設定。 | `5`秒              |
| Health Check Interval             | コネクターの稼働状態チェック間隔                              | `15`秒             |
| Min Metadata Refresh Interval     | Kafkaブローカーやトピックのメタデータ更新の最短間隔。短すぎるとKafkaサーバーに過負荷をかける可能性あり。 | `3`秒              |
| Metadata Request Timeout          | Kafkaからメタデータ取得時の最大待機時間                       | `5`秒              |
| Socket Send / Receive Buffer Size | ソケットバッファサイズを管理しネットワーク伝送性能を最適化   | `1`MB              |
| No Delay                          | システムカーネルがTCPソケットを即時送信するか遅延送信するかの設定。オンで即時送信、オフで少量送信時に最大40ミリ秒の遅延あり。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続のTCPキープアライブ設定。接続の長時間非アクティブによる切断防止のため、`Idle, Interval, Probes`の3つの数値をカンマ区切りで指定。例：`240,30,5`は240秒アイドル後に30秒間隔で5回プローブ送信し応答なければ切断。 | `none`             |

### Confluent Producer Sink設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Health Check Interval            | Sinkの稼働状態チェック間隔                                   | `15`秒             |
| Max Batch Bytes                  | Kafkaバッチ内でメッセージを収集する最大バイト数。Kafkaブローカーのデフォルトは1MBだが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮しやや小さめに設定。単一メッセージがこの値を超える場合は別バッチで送信。 | `896`KB            |
| Required Acks                    | Kafkaパーティションリーダーがフォロワーから待つ必要があるアックの種類：<br />`all_isr`: 全てのインシンクレプリカからのアックを要求<br />`leader_only`: リーダーからのみアックを要求<br />`none`: Kafkaからのアック不要 | `all_isr`          |
| Partition Count Refresh Interval | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加検知後、EMQXは`partition_strategy`に基づき新パーティションをメッセージ送信に組み込む。 | `60`秒             |
| Max Inflight                     | Kafkaプロデューサーがアック受信前に送信可能な最大バッチ数（パーティション毎）。値が大きいほどスループット向上。ただし1より大きいとメッセージ順序入れ替わりのリスクあり。未アックメッセージ数を制御し負荷バランスを取る。 | `10`秒             |
| Query Mode (Producer)            | 非同期または同期クエリモードを選択し、要件に応じたメッセージ送信を最適化。非同期モードではKafka書き込みがMQTTパブリッシュ処理をブロックしないが、クライアントがKafka到着前にメッセージを受信する可能性あり。 | `Async`            |
| Synchronous Query Timeout        | 同期クエリモード時の最大待機時間。メッセージ送信完了を適時確認し長時間待機を防止。`Sync`モード時のみ有効。 | `5`秒              |
| Buffer Mode                      | メッセージ送信前のバッファリング方法。メモリバッファリングは送信速度向上に寄与。<br />`memory`: メモリ上にバッファ。EMQXノード再起動時にメッセージは失われる。<br />`disk`: ディスク上にバッファ。EMQXノード再起動後もメッセージ保持。<br />`hybrid`: 初期はメモリバッファで、一定容量（`segment_bytes`設定参照）超過時に順次ディスクへオフロード。メモリモード同様、ノード再起動時にメッセージは失われる。 | `memory`           |
| Per-partition Buffer Limit       | Kafkaパーティション毎の最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ空間を確保。メモリ使用量と性能のバランス調整に有効。 | `2`GB              |
| Segment File Bytes               | バッファモードが`disk`または`hybrid`の場合に適用。メッセージ保存用セグメントファイルのサイズを制御し、ディスクストレージの最適化に影響。 | `100`MB            |
| Memory Overload Protection       | バッファモードが`memory`の場合に適用。メモリ圧迫時に古いバッファメッセージを自動破棄し、システムの安定性を確保。<br />**注意**：Linux環境のみ有効。 | 無効               |

### <!-- Confluent Consumer Source設定 -->

## 追加情報

EMQXはConfluent/Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクもぜひご覧ください。

**ブログ：**

- [MQTTとKafkaで構築するコネクテッドビークルのストリーミングデータパイプライン](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka｜IoTメッセージングとストリームデータ統合の実践](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)
