# ConfluentへのMQTTデータストリーミング

[Confluent Cloud](https://www.confluent.io/)はApache Kafkaをベースにした、レジリエントでスケーラブルかつフルマネージドのストリーミングデータサービスです。EMQXはルールエンジンとSinkを通じてConfluentとのデータ統合をサポートしており、MQTTデータをConfluentに簡単にストリーミングしてリアルタイム処理、保存、分析を行えます。

![EMQX Confluent Integration](./assets/confluent-integration.png)

本ページでは主にConfluent統合の機能と利点を紹介し、Confluent Cloudの設定およびEMQXでのConfluent Producer Sinkの作成方法を案内します。

## 動作概要

Confluentデータ統合はEMQXのすぐに使える機能であり、MQTTベースのIoTデータとConfluentの強力なデータ処理機能を橋渡しします。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、両プラットフォーム間のデータフローと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は自動車IoTにおけるEMQXとConfluentのデータ統合の典型的なアーキテクチャを示しています。

![Confluent Architecture](./assets/confluent-architecture.png)

Confluentへのデータの入出力はConfluent Sink（Confluentへのメッセージ送信）とConfluent Source（Confluentからのメッセージ受信）を介して行われます。Confluent Sinkを作成した場合、そのワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：車両に接続されたIoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：これらのMQTTメッセージは、組み込みのルールエンジンとメッセージサーバーの協調動作により、トピックマッチングルールに従って処理されます。メッセージが到着しルールエンジンを通過すると、事前に定義された処理ルールが評価されます。ペイロード変換を指定するルールがあれば、データフォーマット変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **Confluentへの橋渡し**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをConfluentに転送するアクションが実行されます。Confluent Sink機能を用いて、MQTTトピックがConfluentのKafkaトピックにマッピングされ、処理済みのメッセージとデータがこれらのトピックに書き込まれます。

車両データがConfluentに入力されると、以下のように柔軟にデータを活用できます：

- サービスはConfluentと直接連携し、特定トピックからリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を行えます。
- Kafka Streamsを利用してストリーム処理を行い、車両状態をメモリ内で集約・相関させてリアルタイム監視が可能です。
- Confluent Stream Designerコンポーネントを使い、MySQLやElasticSearchなど外部システムへのデータ出力用コネクターを選択して保存できます。

## 機能と利点

Confluentとのデータ統合は以下の機能と利点をビジネスにもたらします：

- **大規模メッセージ伝送の信頼性**：EMQXとConfluent Cloudは共に高信頼のクラスター機構を用い、安定かつ信頼性の高いメッセージ伝送チャネルを構築し、大規模IoTデバイスからのメッセージ損失をゼロにします。ノード追加による水平スケールや動的リソース調整で突発的な大規模メッセージにも対応し、メッセージ伝送の可用性を確保します。
- **強力なデータ処理能力**：EMQXのローカルルールエンジンとConfluent Cloudは、デバイスからアプリケーションまでの異なる段階で信頼性の高いストリーミングデータ処理を提供します。リアルタイムのデータフィルタリング、フォーマット変換、集約分析などシナリオに応じた処理が可能で、より複雑なIoTメッセージ処理ワークフローを実現し、データ分析アプリケーションのニーズに応えます。
- **強力な統合機能**：Confluent Cloudが提供する多様なコネクターを通じて、EMQXは他のデータベース、データウェアハウス、データストリーム処理システム等と容易に統合でき、柔軟なデータ分析アプリケーションのための包括的なIoTデータワークフローを構築します。
- **高スループット処理能力**：同期・非同期の両書き込みモードをサポートし、リアルタイム優先や性能優先などデータ書き込み戦略を使い分け、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。
- **効果的なトピックマッピング**：ブリッジ設定により多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティをKafkaヘッダーにマッピングでき、1対1、1対多、多対多の柔軟なトピックマッピング方式を採用し、MQTTトピックフィルター（ワイルドカード）もサポートします。

これらの機能は統合力と柔軟性を高め、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続で伝送され、さらに効果的に保存・管理されます。

## はじめる前に

このセクションではEMQXダッシュボードでConfluentデータ統合を設定するための準備作業を説明します。

### 前提条件

- [ルールエンジン](./rules.md)の理解
- [Sink](./data-bridges.md)の理解

### Confluent Cloudの設定

Confluentデータ統合を作成する前に、Confluent CloudコンソールでConfluentクラスターを作成し、Confluent Cloud CLIを使ってトピックとAPIキーを作成する必要があります。

#### クラスターの作成

1. Confluent Cloudコンソールにログインし、クラスターを作成します。例としてStandardクラスターを選択し、**Begin configuration**をクリックします。

![EMQX Confluent Create Cluster](./assets/confluent_create_cluster_1.2d537cc0.png)

2. リージョン/ゾーンを選択します。デプロイリージョンがConfluent Cloudのリージョンと一致していることを確認し、**Continue**をクリックします。

![EMQX Confluent Select Cluster Region](./assets/confluent_create_cluster_2.a8f517c4.png)

3. クラスター名を入力し、**Launch cluster**をクリックします。

![image-20231013105736218](./assets/confluent_create_cluster_3.d38c10a0.png)

#### Confluent Cloud CLIでトピックとAPIキーを作成

クラスターがConfluent Cloudで起動したら、**Cluster Overview** -> **Cluster Settings**ページから**Bootstrap server**のURLを取得できます。

![image-20231013111959327](./assets/confluent_cluster_info.773da650.png)

Confluent Cloud CLIを使ってクラスターを管理できます。以下は基本的なCLIコマンドです。

##### Confluent Cloud CLIのインストール

```bash
curl -sL --http1.1 https://cnfl.io/cli | sh -s -- -b /usr/local/bin
```

既にインストール済みの場合は、以下のコマンドで更新可能です。

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

既存のAPIキーを使う場合は、以下のコマンドでCLIに追加します。

```bash
confluent api-key store --resource <kafka_cluster_id>
Key: <API_KEY>
Secret: <API_SECRET>
```

APIキーとシークレットがない場合は、以下のコマンドで作成できます。

```bash
$ confluent api-key create --resource <kafka_cluster_id>

APIキーが準備されるまで数分かかることがあります。
APIキーとシークレットは保存してください。シークレットは後で取得できません。
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

`testtopic-in`という名前のトピックを作成するには、以下のコマンドを実行します。

```bash
confluent kafka topic create testtopic-in
```

トピック一覧は以下のコマンドで確認できます。

```bash
confluent kafka topic list
```

##### トピックへのメッセージパブリッシュ

以下のコマンドでプロデューサーを作成します。開始後、メッセージを入力してEnterを押すと、該当トピックにメッセージがパブリッシュされます。

```bash
confluent kafka topic produce testtopic-in
```

##### トピックからのメッセージ消費

以下のコマンドでコンシューマーを作成し、該当トピック内のすべてのメッセージを出力します。

```bash
confluent kafka topic consume -b testtopic-in
```

## コネクターの作成

Confluent Sinkアクションを追加する前に、EMQXとConfluent Cloud間の接続を確立するためにConfluentプロデューサーコネクターを作成する必要があります。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックし、コネクター選択ページで**Confluent Producer**を選択して**Next**をクリックします。

3. `my-confluent`などの名前と説明を入力します。この名前はConfluent Sinkとコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. Confluent Cloudへの接続に必要なパラメータを設定します：
   - **Bootstrap Hosts**：Confluent Cloudクラスター設定ページの**Endpoints**セクションからエンドポイント情報を入力します。
   
   - **Authentication**：Confluent Cloudクラスターで必要な認証方式を選択します：
     - **Basic auth**：Confluent Cloudで作成したAPI KeyとAPI Secretに対応する**Username**と**Password**を入力します。
     
     - **OAuth**：Confluent CloudのOAuth/OIDC設定に従い、トークンエンドポイント、クライアントID、クライアントシークレットなどOAuthパラメータを設定します。
     
       OAuth設定はKafkaコネクターと同様です。各パラメータの詳細は[認証方式](./data-bridge-kafka.md#authentication-method)を参照してください。
     
   - その他のオプションはデフォルトのままか、ビジネスニーズに応じて設定してください。
   
5. **Create**ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にConfluent Cloudに接続します。次に、このコネクターを基にルールを作成し、コネクターで設定したConfluentクラスターにデータを転送します。

## Confluent Sinkを使ったルールの作成

このセクションでは、MQTTトピック`t/#`のメッセージを処理し、処理結果を設定済みのConfluent Sinkを使ってConfluentの`testtopic-in`トピックに送信するルールの作成方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules**をクリックします。

2. 右上の**Create**をクリックします。

3. `my_rule`などのルールIDを入力します。

4. MQTTメッセージをトピック`t/#`からConfluentに転送したい場合、**SQL Editor**に以下の文を入力します。

   注意：独自のSQL構文を指定する場合、`SELECT`部分にSinkが必要とするすべてのフィールドが含まれていることを確認してください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注意：初心者の場合は**SQL Example**や**Enable Test**をクリックしてSQLルールの学習とテストが可能です。

5. + **Add Action**ボタンをクリックして、ルールでトリガーされるアクションを定義します。**Type of Action**ドロップダウンリストから`Confluent Producer`を選択し、**Action**ドロップダウンはデフォルトの`Create Action`のままにするか、既存のConfluent Producerアクションを選択します。この例では新規ルールを作成し、ルールに追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-confluent`コネクターを選択します。ドロップダウン横のボタンをクリックしてポップアップで新規コネクターを素早く作成することも可能です。必要な設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sinkのデータ送信方法を設定します：

   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートしています。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**ドロップダウンから選択できます。**Add**をクリックしてさらにキー・バリューを追加可能です。
   - **Message Key**：Kafkaメッセージのキーを入力します。純粋な文字列か、プレースホルダー（${var}）を含む文字列が指定可能です。
   - **Message Value**：Kafkaメッセージの値を入力します。純粋な文字列か、プレースホルダー（${var}）を含む文字列が指定可能です。
   - **Partition Strategy**：プロデューサーがKafkaのパーティションにメッセージを分配する方法を選択します。
   - **Compression**：Kafkaメッセージ内のレコードを圧縮／解凍するための圧縮アルゴリズムを指定します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にこれらのアクションがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#advanced-configuration)を参照してください。

11. **Create**ボタンをクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新しいSinkがルールアクションに追加されます。

12. **Create**ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブで新規Confluent Producer Sinkも確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを表示できます。トポロジーを通じて、トピック`t/#`のメッセージがルール`my_rule`で解析されConfluentに送信・保存される様子を直感的に把握できます。

## Confluent Producerルールのテスト

Confluent Producerルールが期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/en)を使ってクライアントがEMQXにMQTTメッセージをパブリッシュする動作をシミュレートします。

1. MQTTXを使い、トピック`t/1`にメッセージを送信します：

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Confluent" }'
   ```

2. **Actions(Sink)**ページでSink名をクリックし統計情報を確認します。Sinkの稼働状況をチェックし、新規の受信メッセージ数と送信メッセージ数がそれぞれ1件ずつ増えていることを確認します。

3. 以下のConfluentコマンドでメッセージが`testtopic-in`トピックに書き込まれているか確認します：

   ```bash
   confluent kafka topic consume -b testtopic-in
   ```

## 詳細設定

このセクションでは、コネクターおよびSink/Sourceのパフォーマンス最適化や特定シナリオに応じたカスタマイズ操作のための詳細設定オプションを説明します。該当オブジェクト作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producerのみ）有効にすると、クライアントがメタデータ取得要求を送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証有効時は認証時間も含む）       | `5`秒              |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。これによりSinkはConfluentクラスターなど接続先リソースが完全に稼働しデータ処理準備が整うまで操作を進めません。 | `5`秒              |
| Health Check Interval             | コネクターの稼働状態をチェックする間隔時間                   | `15`秒             |
| Min Metadata Refresh Interval     | Kafkaブローカーやトピックのメタデータを更新する最小間隔。小さすぎるとKafkaサーバーに不要な負荷をかける可能性があります。 | `3`秒              |
| Metadata Request Timeout          | Kafkaからメタデータを要求する際の最大待機時間                 | `5`秒              |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能最適化のためのソケットバッファサイズ管理 | `1`MB              |
| No Delay                          | システムカーネルがTCPソケットを即時送信するか遅延送信するか選択。トグルオンで即時送信。オフの場合、送信内容が少ないと40ミリ秒程度の遅延が発生する可能性あり。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間の非アクティブ状態による接続切断を防止。値は`Idle, Interval, Probes`の3つの数値のカンマ区切りで指定：<br />Idle：接続がアイドル状態である秒数（Linuxデフォルト7200秒）<br />Interval：キープアライブプローブ間隔秒数（Linuxデフォルト75秒）<br />Probes：応答なしと判断するまでの最大プローブ回数（Linuxデフォルト9回）<br />例：`240,30,5`は240秒アイドル後にプローブ開始、30秒間隔で5回応答なしなら切断と判断。 | `none`             |

### Confluent Producer Sink設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Health Check Interval            | Sinkの稼働状態をチェックする間隔時間                         | `15`秒             |
| Max Batch Bytes                  | Kafkaバッチ内で収集するメッセージの最大バイト数。Kafkaブローカーのデフォルトは1MBですが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮し、特に小さいメッセージが多い場合に備えて1MBよりやや小さい値を設定しています。単一メッセージがこの制限を超える場合は別バッチとして送信されます。 | `896`KB            |
| Required Acks                    | Kafkaパーティションリーダーがフォロワーから受け取る必要がある確認応答の種類：<br />`all_isr`：全てのインシンクレプリカからの応答が必要<br />`leader_only`：リーダーのみ応答が必要<br />`none`：Kafkaからの応答不要 | `all_isr`          |
| Partition Count Refresh Interval | Kafkaプロデューサーがパーティション数の増加を検知する間隔。Kafkaのパーティション数が増加すると、EMQXは`partition_strategy`に基づき新パーティションへのメッセージ送信を開始します。 | `60`秒             |
| Max Inflight                     | KafkaプロデューサーがKafkaからの確認応答を受け取る前に送信可能な最大バッチ数（パーティションごと）。値が大きいほどスループットは向上しますが、1より大きい場合はメッセージの順序入れ替わりリスクがあります。未確認メッセージ数を制御し、システム負荷のバランスを取ります。 | `10`秒             |
| Query Mode (Producer)            | 非同期または同期クエリモードを選択し、要件に応じてメッセージ送信を最適化。非同期モードではKafkaへの書き込みがMQTTメッセージパブリッシュ処理をブロックしませんが、クライアントがKafka到着前にメッセージを受け取る可能性があります。 | `Async`            |
| Synchronous Query Timeout        | 同期クエリモード時の最大待機時間。メッセージ送信完了をタイムリーに保証し、長時間待機を防止します。`Sync`モード時のみ有効。 | `5`秒              |
| Buffer Mode                      | メッセージ送信前のバッファリング方法を定義。メモリバッファリングは送信速度を向上させます。<br />`memory`：メモリにバッファリング。EMQXノード再起動時にメッセージは失われます。<br />`disk`：ディスクにバッファリング。EMQXノード再起動後もメッセージは保持されます。<br />`hybrid`：初めはメモリにバッファリングし、一定サイズ（`segment_bytes`設定参照）に達すると徐々にディスクにオフロード。メモリモード同様、ノード再起動時にメッセージは失われます。 | `memory`           |
| Per-partition Buffer Limit       | Kafkaパーティションごとの最大バッファサイズ（バイト）。上限に達すると古いメッセージを破棄しバッファ空間を確保します。メモリ使用量とパフォーマンスのバランス調整に役立ちます。 | `2`GB              |
| Segment File Bytes               | バッファモードが`disk`または`hybrid`の場合に適用。メッセージ保存用のセグメントファイルサイズを制御し、ディスクストレージの最適化に影響します。 | `100`MB            |
| Memory Overload Protection       | バッファモードが`memory`の場合に適用。EMQXはメモリ圧迫時に古いバッファメッセージを自動破棄し、システムの安定性を確保します。<br />**注意**：Linuxシステムのみ有効です。 | Disabled           |

### <!-- Confluent Consumer Source Configuration -->

## 追加情報

EMQXはConfluent/Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクもご参照ください。

**ブログ：**

- [MQTTとKafkaで構築するコネクテッドビークルのストリーミングデータパイプライン](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka | IoTメッセージングとストリームデータ統合の実践](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)
