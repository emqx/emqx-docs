# ConfluentへのMQTTデータストリーミング

[Confluent Cloud](https://www.confluent.io/)はApache Kafkaをベースにした、レジリエンスが高くスケーラブルで完全にマネージドされたストリーミングデータサービスです。EMQXはルールエンジンとSinkを通じてConfluentとのデータ統合をサポートしており、MQTTデータをConfluentに簡単にストリーミングしてリアルタイム処理、保存、分析が可能です。

![EMQX Confluent Integration](./assets/confluent-integration.png)

本ページでは主にConfluent統合の機能と利点を紹介し、Confluent Cloudの設定およびEMQXでのConfluent Producer Sinkの作成方法を案内します。

## 動作の仕組み

Confluentデータ統合はEMQXのすぐに使える機能で、MQTTベースのIoTデータとConfluentの強力なデータ処理機能を橋渡しします。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、両プラットフォーム間のデータフローと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は自動車IoTにおけるEMQXとConfluentのデータ統合の典型的なアーキテクチャを示しています。

![Confluent Architecture](./assets/confluent-architecture.png)

Confluentへのデータの入出力はConfluent Sink（Confluentへのメッセージ送信）とConfluent Source（Confluentからのメッセージ受信）を介して行われます。Confluent Sinkを作成した場合、そのワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：車両に接続されたIoTデバイスはMQTTプロトコルを介してEMQXに正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：これらのMQTTメッセージは、組み込みのルールエンジンとメッセージングサーバーの協調動作により、トピックマッチングルールに従って処理されます。メッセージが到着しルールエンジンを通過すると、事前定義された処理ルールが評価されます。ペイロード変換を指定するルールがあれば、データ形式変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **Confluentへのブリッジ**：ルールエンジンで定義されたルールはメッセージをConfluentに転送するアクションをトリガーします。Confluent Sink機能を使い、MQTTトピックがConfluentのKafkaトピックにマッピングされ、処理済みのすべてのメッセージとデータがこれらのトピックに書き込まれます。

車両データがConfluentに入力されると、以下のように柔軟にデータを活用できます：

- サービスはConfluentと直接統合し、特定トピックのリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を行えます。
- Kafka Streamsを利用してストリーム処理を行い、車両状態をメモリ内で集約・相関させてリアルタイム監視が可能です。
- ConfluentのStream Designerコンポーネントを使い、MySQLやElasticSearchなど外部システムへのデータ出力用コネクターを選択して保存できます。

## 機能と利点

Confluentとのデータ統合は以下の機能と利点をビジネスにもたらします：

- **大規模メッセージ送信の信頼性**：EMQXとConfluent Cloudは共に高信頼なクラスター機構を用い、安定かつ信頼性の高いメッセージ送信チャネルを構築し、大規模IoTデバイスからのメッセージ損失をゼロにします。どちらもノード追加による水平スケールが可能で、リソースを動的に調整して突発的な大規模メッセージに対応し、メッセージ送信の可用性を確保します。
- **強力なデータ処理能力**：EMQXのローカルルールエンジンとConfluent Cloudは、デバイスからアプリケーションまでの異なる段階で信頼性の高いストリーミングデータ処理を提供します。リアルタイムのデータフィルタリング、形式変換、集約分析などをシナリオに応じて実施し、より複雑なIoTメッセージ処理ワークフローを実現し、データ分析アプリケーションのニーズに応えます。
- **強力な統合能力**：Confluent Cloudが提供する多様なコネクターを通じて、EMQXは他のデータベース、データウェアハウス、データストリーム処理システムなどと容易に統合でき、迅速なデータ分析アプリケーションのための完全なIoTデータワークフローを構築します。
- **高スループット処理能力**：同期・非同期の両方の書き込みモードをサポートし、リアルタイム優先と性能優先のデータ書き込み戦略を区別でき、異なるシナリオでレイテンシとスループットのバランスを柔軟に調整可能です。
- **効果的なトピックマッピング**：ブリッジ設定を通じて、多数のIoTビジネストピックをKafkaトピックにマッピングできます。EMQXはMQTTのユーザープロパティをKafkaヘッダーにマッピングすることをサポートし、1対1、1対多、多対多など多様なトピックマッピング方式を採用し、MQTTトピックフィルター（ワイルドカード）にも対応します。

これらの機能は統合能力と柔軟性を高め、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続を通じて送信され、さらに効果的に保存・管理されます。

## はじめる前に

このセクションでは、EMQXダッシュボードでConfluentデータ統合を設定するための準備作業について説明します。

### 前提条件

- [ルールエンジン](./rules.md)の理解
- [Sink](./data-bridges.md)の理解

### Confluent Cloudの設定

Confluentデータ統合を作成する前に、Confluent CloudコンソールでConfluentクラスターを作成し、Confluent Cloud CLIを使ってトピックとAPIキーを作成する必要があります。

#### クラスターの作成

1. Confluent Cloudコンソールにログインし、クラスターを作成します。例としてStandardクラスターを選択し、**Begin configuration**をクリックします。

![EMQX Confluent Create Cluster](./assets/confluent_create_cluster_1.2d537cc0.png)

2. リージョン/ゾーンを選択します。デプロイメントリージョンがConfluent Cloudのリージョンと一致していることを確認し、**Continue**をクリックします。

![EMQX Confluent Select Cluster Region](./assets/confluent_create_cluster_2.a8f517c4.png)

3. クラスター名を入力し、**Launch cluster**をクリックします。

![image-20231013105736218](./assets/confluent_create_cluster_3.d38c10a0.png)

#### Confluent Cloud CLIでトピックとAPIキーを作成

クラスターがConfluent Cloudで稼働している状態で、**Cluster Overview** -> **Cluster Settings**ページから**Bootstrap server**のURLを取得できます。

![image-20231013111959327](./assets/confluent_cluster_info.773da650.png)

Confluent Cloud CLIを使ってクラスターを管理できます。以下は基本的なCLIコマンドです。

##### Confluent Cloud CLIのインストール

```bash
curl -sL --http1.1 https://cnfl.io/cli | sh -s -- -b /usr/local/bin
```

すでにインストール済みの場合は、以下のコマンドでアップデート可能です：

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

既存のAPIキーをCLIに追加する場合は以下のコマンドを実行します：

```bash
confluent api-key store --resource <kafka_cluster_id>
Key: <API_KEY>
Secret: <API_SECRET>
```

APIキーとシークレットを持っていない場合は、以下のコマンドで作成できます：

```bash
$ confluent api-key create --resource <kafka_cluster_id>

APIキーの準備には数分かかる場合があります。
APIキーとシークレットは保存してください。シークレットは後から取得できません。
+------------+------------------------------------------------------------------+
| API Key    | YZ6R7YO6Q2WK35X7                                                 |
| API Secret | ****************************************                         |
+------------+------------------------------------------------------------------+
```

CLIに追加した後、以下のコマンドでAPIキーとシークレットを使用します：

```bash
confluent api-key use <API_Key> --resource <kafka_cluster_id>
```

##### トピックの作成

`testtopic-in`という名前のトピックを作成するには以下のコマンドを実行します：

```bash
confluent kafka topic create testtopic-in
```

トピック一覧は以下のコマンドで確認できます：

```bash
confluent kafka topic list
```

##### トピックへのメッセージパブリッシュ

以下のコマンドでプロデューサーを作成できます。起動後、メッセージを入力してEnterキーを押すと該当トピックにメッセージがパブリッシュされます。

```bash
confluent kafka topic produce testtopic-in
```

##### トピックからのメッセージコンシューム

以下のコマンドでコンシューマーを作成できます。該当トピックのすべてのメッセージが出力されます。

```bash
confluent kafka topic consume -b testtopic-in
```

## コネクターの作成

Confluent Sinkアクションを追加する前に、EMQXとConfluent Cloud間の接続を確立するためにConfluent Producerコネクターを作成する必要があります。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックし、コネクター選択ページで**Confluent Producer**を選択して**Next**をクリックします。

3. `my-confluent`などの名前と説明を入力します。名前はConfluent Sinkとコネクターを紐付けるために使用され、クラスター内で一意である必要があります。

4. Confluent Cloudへの接続に必要なパラメータを設定します：
   - **Bootstrap Hosts**：Confluent Cloudクラスター設定ページの**Endpoints**セクションからエンドポイント情報を入力します。
   
   - **Authentication**：Confluent Cloudクラスターが要求する認証方法を選択します：
     - **Basic auth**：Confluent Cloudで作成したAPIキーとAPIシークレットに対応する**Username**と**Password**を入力します。
     
     - **OAuth**：Confluent CloudのOAuth/OIDC設定に従い、トークンエンドポイント、クライアントID、クライアントシークレットなどOAuthパラメータを設定します。  
       
       OAuth設定はKafkaコネクターと同様です。各パラメータの詳細は[認証方法](./data-bridge-kafka.md#authentication-method)を参照してください。
     
   - その他のオプションはデフォルトのままにするか、ビジネスニーズに応じて設定してください。
   
5. **Create**ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にConfluent Cloudに接続します。次に、このコネクターを基にルールを作成し、コネクターで設定したConfluentクラスターにデータを転送します。

## Confluent Sinkを使ったルールの作成

このセクションでは、MQTTトピック`t/#`のメッセージを処理し、処理結果を設定済みのConfluent Sinkを通じてConfluentの`testtopic-in`トピックに送信するルールの作成方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules**をクリックします。

2. 右上の**Create**をクリックします。

3. ルールIDを`my_rule`などで入力します。

4. MQTTメッセージをトピック`t/#`からConfluentに転送したい場合、**SQL Editor**に以下の文を入力します。

   注意：独自のSQL構文を指定する場合、`SELECT`部分にSinkが必要とするすべてのフィールドを含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注意：初心者の場合は**SQL Example**や**Enable Test**をクリックしてSQLルールの学習やテストが可能です。

5. + **Add Action**ボタンをクリックしてルールでトリガーされるアクションを定義します。**Type of Action**のドロップダウンリストから`Confluent Producer`を選択し、**Action**ドロップダウンはデフォルトの`Create Action`のままか、既存のConfluent Producerアクションを選択します。この例では新規ルールを作成し、ルールに追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-confluent`コネクターを選択します。ドロップダウン横のボタンをクリックするとポップアップで新しいコネクターを素早く作成でき、必要な設定パラメータは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sinkのデータ送信方法を設定します：

   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**ドロップダウンから選択可能です。**Add**をクリックしてさらにキー・バリューを追加できます。
   - **Message Key**：Kafkaメッセージのキーです。純粋な文字列またはプレースホルダー（${var}）を含む文字列を入力できます。
   - **Message Value**：Kafkaメッセージの値です。純粋な文字列またはプレースホルダー（${var}）を含む文字列を入力できます。
   - **Partition Strategy**：プロデューサーがKafkaのパーティションにメッセージを分配する方法を選択します。
   - **Compression**：Kafkaメッセージ内のレコードを圧縮・解凍するための圧縮アルゴリズムを指定します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にこれらのアクションがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#advanced-configuration)を参照してください。

11. **Create**ボタンをクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新しいSinkがルールアクションに追加されます。

12. **Create**ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールが確認でき、**Actions(Sink)**タブで新規Confluent Producer Sinkも確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認できます。トポロジーを通じて、トピック`t/#`のメッセージがルール`my_rule`で解析され、Confluentに送信・保存されていることが直感的に把握できます。

## Confluent Producerルールのテスト

Confluent Producerルールが期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/en)を使ってクライアントがEMQXにMQTTメッセージをパブリッシュするシミュレーションが可能です。

1. MQTTXを使い、トピック`t/1`にメッセージを送信します：

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Confluent" }'
   ```

2. **Actions(Sink)**ページでSinkの名前をクリックして統計情報を表示します。Sinkの稼働状況を確認し、新規の受信メッセージ数と送信メッセージ数がそれぞれ1件ずつあることを確認します。

3. 以下のConfluentコマンドでメッセージが`testtopic-in`トピックに書き込まれているか確認します：

   ```bash
   confluent kafka topic consume -b testtopic-in
   ```

## 詳細設定

このセクションでは、コネクターやSink/Sourceのパフォーマンス最適化や特定シナリオに応じたカスタマイズ操作が可能な詳細設定オプションを説明します。該当オブジェクト作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producerのみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証有効時は認証時間も含む）       | `5` 秒             |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。Sinkが接続リソース（例：Confluentクラスター）の完全稼働を確認してから処理を進めるための設定。 | `5` 秒             |
| Health Check Interval             | コネクターの稼働状態をチェックする間隔時間                   | `15` 秒            |
| Min Metadata Refresh Interval     | Kafkaブローカーとトピックのメタデータ更新を行う最短間隔。短すぎるとKafkaサーバーに負荷がかかる可能性あり。 | `3` 秒             |
| Metadata Request Timeout          | Kafkaからメタデータを要求する際の最大待機時間                 | `5` 秒             |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能最適化のためのソケットバッファサイズ管理 | `1` MB             |
| No Delay                          | システムカーネルがTCPソケットを即時送信するか遅延送信するかの選択。トグルオンで即時送信。デフォルトは40ミリ秒の遅延あり。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間の非アクティブによる接続切断を防止。値は`Idle, Interval, Probes`の3つの数値のカンマ区切りで指定。<br />Idle：接続がアイドル状態になる秒数（Linuxデフォルト7200秒）<br />Interval：キープアライブプローブ間隔秒数（Linuxデフォルト75秒）<br />Probes：応答なしで接続切断と判断するプローブ回数（Linuxデフォルト9回）<br />例：`240,30,5`はアイドル240秒後にプローブ開始、30秒間隔で最大5回プローブ実施し応答なければ切断。 | `none`             |

### Confluent Producer Sink設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Health Check Interval            | Sinkの稼働状態をチェックする間隔時間                         | `15` 秒            |
| Max Batch Bytes                  | Kafkaバッチ内でメッセージを収集する最大サイズ（バイト）。Kafkaブローカーのデフォルトは1MBだが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮し1MB未満に設定。単一メッセージがこのサイズを超える場合は別バッチで送信される。 | `896` KB           |
| Required Acks                    | Kafkaパーティションリーダーがフォロワーから待つ必要のあるアックの種類：<br />`all_isr`：全てのインシンクレプリカからのアックを要求<br />`leader_only`：リーダーのみからのアックを要求<br />`none`：Kafkaからのアック不要 | `all_isr`          |
| Partition Count Refresh Interval | Kafkaプロデューサーがパーティション数増加を検知する間隔時間。パーティション増加時、EMQXは`partition_strategy`に基づき新パーティションをメッセージ送信に組み込む。 | `60` 秒            |
| Max Inflight                     | Kafkaプロデューサーがアック受信前に送信可能な最大バッチ数（パーティション毎）。大きいほどスループット向上。ただし1より大きい場合、メッセージの順序入れ替わりリスクあり。未アックメッセージ数を制御し、負荷バランスを取る。 | `10` 秒            |
| Query Mode (Producer)            | 非同期または同期クエリモードを選択し、異なる要件に応じてメッセージ送信を最適化。非同期モードではKafka書き込みがMQTTパブリッシュをブロックしないが、クライアントがKafka到着前にメッセージを受信する可能性あり。 | `Async`            |
| Synchronous Query Timeout        | 同期クエリモード時の最大待機時間。メッセージ送信完了を適時保証し長時間待機を防止。`Sync`モード時のみ適用。 | `5` 秒             |
| Buffer Mode                      | メッセージ送信前のバッファリング方法を定義。メモリバッファリングは送信速度向上に寄与。<br />`memory`：メモリにバッファ。EMQXノード再起動でメッセージ消失。<br />`disk`：ディスクにバッファ。ノード再起動後もメッセージ保持。<br />`hybrid`：初めはメモリにバッファし、一定サイズ（`segment_bytes`設定参照）超過で徐々にディスクにオフロード。メモリモード同様、ノード再起動でメッセージ消失。 | `memory`           |
| Per-partition Buffer Limit       | Kafkaパーティション毎の最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ領域を確保。メモリ使用量と性能のバランス調整に利用。 | `2` GB             |
| Segment File Bytes               | バッファモードが`disk`または`hybrid`の場合に適用。メッセージ保存用のセグメントファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100` MB           |
| Memory Overload Protection       | バッファモードが`memory`の場合に適用。メモリ圧迫時に古いバッファメッセージを自動破棄し、過剰メモリ使用によるシステム不安定化を防止し信頼性を確保。<br />**注**：Linuxシステムのみ有効。 | Disabled           |

### <!-- Confluent Consumer Source Configuration -->

## 追加情報

EMQXはConfluent/Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクをご参照ください：

**ブログ：**

- [MQTTとKafkaを使ったコネクテッドビークルのストリーミングデータパイプライン構築](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka｜IoTメッセージングとストリームデータ統合の実践](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)
