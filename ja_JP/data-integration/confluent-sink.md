# ConfluentへのMQTTデータストリーミング

[Confluent Cloud](https://www.confluent.io/)はApache Kafkaをベースにした、レジリエントでスケーラブル、かつフルマネージドのストリーミングデータサービスです。EMQXはルールエンジンとSinkを通じてConfluentとのデータ連携をサポートし、MQTTデータをConfluentに簡単にストリーミングしてリアルタイム処理、保存、分析を可能にします。

![EMQX Confluent Integration](./assets/confluent-integration.png)

本ページでは主にConfluent連携の特徴と利点を紹介し、Confluent Cloudの設定およびEMQXでのConfluent Producer Sinkの作成方法を案内します。

## 動作の仕組み

Confluentデータ連携はEMQXのすぐに使える機能であり、MQTTベースのIoTデータとConfluentの強力なデータ処理機能を橋渡しします。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、両プラットフォーム間のデータフローと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は自動車IoTにおけるEMQXとConfluentのデータ連携の典型的なアーキテクチャを示しています。

![Confluent Architecture](./assets/confluent-architecture.png)

Confluentへのデータの入出力は、Confluent Sink（Confluentへメッセージを送信）とConfluent Source（Confluentからメッセージを受信）を介して行われます。Confluent Sinkを作成した場合のワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**: 車両に接続されたIoTデバイスはMQTTプロトコルでEMQXに正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**: これらのMQTTメッセージは、組み込みのルールエンジンとメッセージングサーバーの協調動作により、トピックマッチングルールに従って処理されます。メッセージが到着しルールエンジンを通過すると、事前定義された処理ルールが評価されます。ペイロード変換を指定するルールがあれば、データ形式変換、特定情報のフィルタリング、追加コンテキストによるペイロードの拡充などが適用されます。
3. **Confluentへの橋渡し**: ルールエンジンで定義されたルールがトリガーとなり、メッセージをConfluentに転送するアクションが実行されます。Confluent Sink機能を使い、MQTTトピックをConfluentの事前定義されたKafkaトピックにマッピングし、処理済みのメッセージとデータをこれらのトピックに書き込みます。

車両データがConfluentに入力されると、以下のように柔軟にデータを活用できます。

- サービスはConfluentと直接連携し、特定トピックのリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を行えます。
- Kafka Streamsを利用したストリーム処理や、メモリ上での車両状態の集約・相関によるリアルタイム監視が可能です。
- Confluent Stream Designerコンポーネントを使い、MySQLやElasticSearchなど外部システムへのデータ出力用の各種コネクターを選択して保存できます。

## 特徴と利点

Confluentとのデータ連携は以下の特徴と利点をビジネスにもたらします。

- **大規模メッセージ送信の信頼性**: EMQXとConfluent Cloudは共に高信頼のクラスター機構を用い、安定かつ信頼性の高いメッセージ送信チャネルを確立し、大規模IoTデバイスからのメッセージロスをゼロにします。ノード追加による水平スケールやリソースの動的調整で、突発的な大規模メッセージにも対応し、メッセージ送信の可用性を確保します。
- **強力なデータ処理能力**: EMQXのローカルルールエンジンとConfluent Cloudは、デバイスからアプリケーションまでのIoTデータの異なる段階で信頼性の高いストリーミングデータ処理を提供します。リアルタイムのデータフィルタリング、形式変換、集約分析などシナリオに応じた処理が可能で、より複雑なIoTメッセージ処理ワークフローを実現し、データ分析アプリケーションのニーズに応えます。
- **強力な連携機能**: Confluent Cloudが提供する多様なコネクターを通じて、EMQXは他のデータベース、データウェアハウス、データストリーム処理システムなどと容易に統合でき、アジャイルなデータ分析アプリケーションのための完全なIoTデータワークフローを構築します。
- **高スループット処理能力**: 同期・非同期の書き込みモードをサポートし、リアルタイム優先や性能優先などシナリオに応じて書き込み戦略を使い分け、レイテンシとスループットのバランスを柔軟に調整可能です。
- **効果的なトピックマッピング**: ブリッジ設定を通じて、多数のIoTビジネストピックをKafkaトピックにマッピングできます。EMQXはMQTTユーザープロパティをKafkaヘッダーにマッピング可能で、1対1、1対多、多対多など多様なトピックマッピング方式を採用し、MQTTトピックフィルター（ワイルドカード）もサポートします。

これらの特徴は連携能力と柔軟性を高め、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続で送信され、さらに効果的に保存・管理されます。

## はじめる前に

本節ではEMQXダッシュボードでConfluentデータ連携を設定するための準備作業を説明します。

### 前提条件

- [ルールエンジン](./rules.md)の理解
- [Sink](./data-bridges.md)の理解

### Confluent Cloudの設定

Confluentデータ連携を作成する前に、Confluent CloudコンソールでConfluentクラスターを作成し、Confluent Cloud CLIを使ってトピックとAPIキーを作成する必要があります。

#### クラスターの作成

1. Confluent Cloudコンソールにログインし、クラスターを作成します。例としてStandardクラスターを選択し、**Begin configuration**をクリックします。

![EMQX Confluent Create Cluster](./assets/confluent_create_cluster_1.2d537cc0.png)

2. リージョン/ゾーンを選択します。デプロイメントリージョンがConfluent Cloudのリージョンと一致していることを確認し、**Continue**をクリックします。

![EMQX Confluent Select Cluster Region](./assets/confluent_create_cluster_2.a8f517c4.png)

3. クラスター名を入力し、**Launch cluster**をクリックします。

![image-20231013105736218](./assets/confluent_create_cluster_3.d38c10a0.png)

#### Confluent Cloud CLIを使ったトピックとAPIキーの作成

Confluent Cloudでクラスターが稼働したら、**Cluster Overview** -> **Cluster Settings**ページから**Bootstrap server**のURLを取得できます。

![image-20231013111959327](./assets/confluent_cluster_info.773da650.png)

Confluent Cloud CLIでクラスターを管理できます。以下は基本的なCLIコマンドです。

##### Confluent Cloud CLIのインストール

```bash
curl -sL --http1.1 https://cnfl.io/cli | sh -s -- -b /usr/local/bin
```

すでにインストール済みの場合は、以下のコマンドで更新可能です。

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

`testtopic-in`という名前のトピックを以下のコマンドで作成できます。

```bash
confluent kafka topic create testtopic-in
```

トピック一覧は以下のコマンドで確認できます。

```bash
confluent kafka topic list
```

##### トピックへのメッセージ送信（Producer）

以下のコマンドでプロデューサーを作成できます。起動後、メッセージを入力しEnterを押すと該当トピックにメッセージが送信されます。

```bash
confluent kafka topic produce testtopic-in
```

##### トピックからのメッセージ受信（Consumer）

以下のコマンドでコンシューマーを作成できます。該当トピックの全メッセージを出力します。

```bash
confluent kafka topic consume -b testtopic-in
```

## コネクターの作成

Confluent Sinkアクションを追加する前に、EMQXとConfluent Cloud間の接続を確立するためにConfluentプロデューサーコネクターを作成する必要があります。

1. EMQXダッシュボードで **Integration** -> **Connectors** を開きます。
2. ページ右上の **Create** をクリックし、コネクター選択ページで **Confluent Producer** を選択して **Next** をクリックします。
3. 名前と説明を入力します（例：`my-confluent`）。名前はConfluent Sinkとコネクターを紐付けるために使用され、クラスター内で一意である必要があります。
4. Confluent Cloud接続に必要なパラメータを設定します。
   - **Bootstrap Hosts**: Confluentクラスター設定ページのEndpoints情報に対応します。
   - **Username** と **Password**: 先ほどConfluent Cloud CLIで作成したAPIキーとシークレットを入力します。
   - その他のオプションはデフォルトのままか、ビジネスニーズに応じて設定してください。
5. **Create** ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にConfluent Cloudに接続します。次に、このコネクターを基にルールを作成し、コネクターで設定したConfluentクラスターへデータを転送します。

## Confluent Sinkを使ったルールの作成

この節では、MQTTトピック `t/#` からのメッセージを処理し、処理結果をConfluentの `testtopic-in` トピックに送信するルールをEMQXで作成する方法を示します。

1. EMQXダッシュボードに入り、 **Integration** -> **Rules** をクリックします。

2. 右上の **Create** をクリックします。

3. ルールIDを入力します（例：`my_rule`）。

4. MQTTメッセージをトピック `t/#` からConfluentに転送したい場合、**SQL Editor**に以下の文を入力します。

   注意：独自のSQL構文を指定する場合は、Sinkが必要とする全フィールドが`SELECT`に含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注意：初心者の場合は、**SQL Example**と**Enable Test**をクリックしてSQLルールの学習とテストが可能です。

5. + **Add Action** ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action**ドロップダウンリストから `Confluent Producer` を選択し、**Action**ドロップダウンはデフォルトの `Create Action` のままか、既存のConfluent Producerアクションを選択します。この例では新規ルールを作成しアクションを追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector**ドロップダウンから先ほど作成した `my-confluent` コネクターを選択します。ドロップダウン横のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sinkのデータ送信方法を設定します。

   - **Kafka Topic**: `testtopic-in` と入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Kafka Headers**: Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは **Kafka Header Value Encod Type** ドロップダウンから選択可能です。**Add**をクリックしてキー・バリューのペアを追加できます。
   - **Message Key**: Kafkaメッセージのキー。純粋な文字列か、プレースホルダー（${var}）を含む文字列を入力します。
   - **Message Value**: Kafkaメッセージの値。純粋な文字列か、プレースホルダー（${var}）を含む文字列を入力します。
   - **Partition Strategy**: プロデューサーがKafkaパーティションにメッセージを分配する方法を選択します。
   - **Compression**: Kafkaメッセージ内のレコードを圧縮・解凍するための圧縮アルゴリズムを指定します。

9. **フォールバックアクション（任意）**: メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。これらはプライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**: [詳細設定](#advanced-configuration)を参照してください。

11. **Create** ボタンをクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新規Sinkがルールアクションに追加されます。

12. **Create** ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules** ページで新規ルールを確認でき、**Actions(Sink)** タブで新規Confluent Producer Sinkも確認できます。

また、**Integration** -> **Flow Designer** をクリックするとトポロジーを表示できます。トポロジーを通じて、トピック `t/#` のメッセージがルール `my_rule` によって解析され、Confluentに送信・保存される様子を直感的に確認できます。

## Confluent Producerルールのテスト

Confluent Producerルールが期待通りに動作するかテストするため、[MQTTX](https://mqttx.app/en)を使ってクライアントがEMQXにMQTTメッセージをパブリッシュする動作をシミュレートできます。

1. MQTTXを使ってトピック `t/1` にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Confluent" }'
   ```

2. **Actions(Sink)** ページでSinkの名前をクリックし統計情報を確認します。Sinkの稼働状況をチェックし、新規受信メッセージ数と送信メッセージ数がそれぞれ1件ずつあることを確認します。

3. 以下のConfluentコマンドでメッセージが `testtopic-in` トピックに書き込まれているか確認します。

   ```bash
   confluent kafka topic consume -b testtopic-in
   ```

## 詳細設定

本節では、コネクターやSink/Sourceのパフォーマンス最適化や特定シナリオに応じたカスタマイズ操作に役立つ詳細設定オプションを説明します。対応するオブジェクト作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証有効時は認証時間も含む）       | `5` 秒             |
| Start Timeout                    | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。Sinkが接続リソース（例：Confluentクラスター）の完全稼働を確認してから処理を進めるための設定。 | `5` 秒             |
| Health Check Interval            | コネクターの稼働状態チェック間隔                               | `15` 秒            |
| Min Metadata Refresh Interval    | Kafkaブローカー・トピックメタデータの最小更新間隔。小さすぎるとKafkaサーバーの負荷が増大する可能性あり。 | `3` 秒             |
| Metadata Request Timeout         | Kafkaからメタデータ取得時の最大待機時間                       | `5` 秒             |
| Socket Send / Receive Buffer Size| ソケットバッファサイズの管理。ネットワーク伝送性能の最適化に寄与。 | `1` MB             |
| No Delay                        | システムカーネルがTCPソケットを即時送信するか遅延送信するかの設定。オンで即時送信。オフの場合、送信内容が少ないと40ミリ秒程度の遅延が発生。 | `Enabled`          |
| TCP Keepalive                   | Kafkaブリッジ接続のTCPキープアライブ機構を有効化し、長時間の非通信による接続切断を防止。値は`Idle, Interval, Probes`の3つの数値のカンマ区切りで指定。<br>Idle: 接続がアイドル状態である秒数（Linuxデフォルト7200秒）。<br>Interval: キープアライブプローブ間隔（Linuxデフォルト75秒）。<br>Probes: 最大プローブ回数（Linuxデフォルト9回）。<br>例：`240,30,5`は240秒のアイドル後にプローブ開始、30秒ごとにプローブ、5回応答なしで接続切断判定。 | `none`             |

### Confluent Producer Sink設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Health Check Interval            | Sinkの稼働状態チェック間隔                                   | `15` 秒            |
| Max Batch Bytes                 | Kafkaバッチ内でメッセージを収集する最大サイズ（バイト）。Kafkaブローカーのデフォルトは1MBだが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮し1MB未満に設定。単一メッセージがこのサイズを超える場合は別バッチで送信。 | `896` KB           |
| Required Acks                   | Kafkaパーティションリーダーがフォロワーから待つ必要のあるアックの種類。<br>`all_isr`: 全てのインシンクレプリカからのアックを要求。<br>`leader_only`: パーティションリーダーからのみアックを要求。<br>`none`: Kafkaからのアック不要。 | `all_isr`          |
| Partition Count Refresh Interval| Kafkaプロデューサーがパーティション数増加を検知する間隔。増加検知後、EMQXは`partition_strategy`に基づき新パーティションをメッセージ送信に組み込む。 | `60` 秒            |
| Max Inflight                   | Kafkaプロデューサーがアック受信前に送信可能な最大バッチ数（パーティション毎）。値が大きいほどスループット向上。ただし1より大きいとメッセージの順序入れ替わりリスクあり。未アックメッセージ数の制御で負荷バランスを取る。 | `10`               |
| Query Mode (Producer)          | 非同期または同期クエリモードを選択し、メッセージ送信要件に応じて最適化。非同期モードではKafka書き込みがMQTTパブリッシュをブロックしないが、クライアントがKafka到着前にメッセージを受け取る可能性あり。 | `Async`            |
| Synchronous Query Timeout      | 同期クエリモード時の最大待機時間。メッセージ送信完了をタイムリーに保証し、長時間待機を防止。`Sync`モード時のみ有効。 | `5` 秒             |
| Buffer Mode                   | メッセージ送信前のバッファリング方式。メモリバッファリングは送信速度向上に寄与。<br>`memory`: メモリにバッファ。EMQXノード再起動でメッセージ消失。<br>`disk`: ディスクにバッファ。EMQXノード再起動後もメッセージ保持。<br>`hybrid`: 初めはメモリにバッファし、一定サイズ（`segment_bytes`参照）超過で順次ディスクにオフロード。メモリモード同様、ノード再起動でメッセージ消失。 | `memory`           |
| Per-partition Buffer Limit    | Kafkaパーティション毎の最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ空間を確保。メモリ使用量と性能のバランス調整に寄与。 | `2` GB             |
| Segment File Bytes           | バッファモードが`disk`または`hybrid`時に適用。メッセージ保存用の分割ファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection   | バッファモードが`memory`時に適用。EMQXが高メモリ圧迫時に古いバッファメッセージを自動破棄し、システムの安定性を確保。<br>**注**: Linux環境のみ有効。 | 無効               |

### <!-- Confluent Consumer Source Configuration -->

## 追加情報

EMQXはConfluent/Kafkaとのデータ連携に関する豊富な学習リソースを提供しています。以下のリンクもご参照ください。

**ブログ:**

- [MQTTとKafkaを使ったコネクテッドビークルのストリーミングデータパイプライン構築](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka｜IoTメッセージングとストリームデータ連携の実践](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka連携](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート:**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka連携](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)
