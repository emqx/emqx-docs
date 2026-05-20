# ConfluentへのMQTTデータストリーミング

[Confluent Cloud](https://www.confluent.io/)はApache Kafkaをベースにした、レジリエントでスケーラブル、かつフルマネージドのストリーミングデータサービスです。EMQXはルールエンジンとSinkを通じてConfluentとのデータ統合をサポートしており、MQTTデータをConfluentに簡単にストリーミングしてリアルタイム処理、保存、分析を行うことができます。

![EMQX Confluent Integration](./assets/confluent-integration.png)

本ページでは主にConfluent統合の機能と利点を紹介し、Confluent Cloudの設定およびEMQXでのConfluent Producer Sinkの作成方法を案内します。

## 動作概要

Confluentデータ統合はEMQXの即利用可能な機能であり、MQTTベースのIoTデータとConfluentの強力なデータ処理機能を橋渡しします。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、両プラットフォーム間のデータフローと処理を簡素化し、複雑なコーディングを不要にします。

下図は自動車IoTにおけるEMQXとConfluentのデータ統合の典型的なアーキテクチャを示しています。

![Confluent Architecture](./assets/confluent-architecture.png)

Confluentへのデータの入出力は、Confluent Sink（Confluentへメッセージを送信）とConfluent Source（Confluentからメッセージを受信）を通じて行われます。Confluent Sinkを作成した場合、そのワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：車両に接続されたIoTデバイスはMQTTプロトコルでEMQXに正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータ処理**：これらのMQTTメッセージは、組み込みのルールエンジンとメッセージサーバーの連携により、トピックマッチングルールに従って処理されます。メッセージが到着しルールエンジンを通過すると、事前定義された処理ルールが評価されます。ペイロード変換を指定するルールがあれば、データ形式変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などの変換が適用されます。
3. **Confluentへのブリッジ**：ルールエンジンで定義されたルールは、メッセージをConfluentに転送するアクションをトリガーします。Confluent Sink機能を使い、MQTTトピックはConfluentの事前定義されたKafkaトピックにマッピングされ、処理済みのメッセージとデータはこれらのトピックに書き込まれます。

車両データがConfluentに入力されると、以下のように柔軟にデータを活用できます。

- サービスはConfluentと直接連携し、特定トピックのリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を行えます。
- Kafka Streamsを利用してストリーム処理を行い、車両状態をメモリ上で集約・相関させてリアルタイム監視が可能です。
- ConfluentのStream Designerコンポーネントを使い、MySQLやElasticSearchなど外部システムへのデータ出力用コネクターを選択して保存できます。

## 機能と利点

Confluentとのデータ統合は、以下の機能と利点をビジネスにもたらします。

- **大規模メッセージ伝送の信頼性**：EMQXとConfluent Cloudはどちらも高信頼のクラスター機構を用いて安定かつ信頼性の高いメッセージ伝送チャネルを構築し、大規模IoTデバイスからのメッセージのロスをゼロにします。両者はノード追加による水平スケールが可能で、突発的な大規模メッセージにも動的にリソースを調整し、メッセージ伝送の可用性を確保します。
- **強力なデータ処理能力**：EMQXのローカルルールエンジンとConfluent Cloudは、デバイスからアプリケーションまでの異なる段階で信頼性の高いストリーミングデータ処理機能を提供します。リアルタイムデータのフィルタリング、形式変換、集約分析などシナリオに応じて複雑なIoTメッセージ処理ワークフローを実現し、データ分析アプリケーションのニーズに応えます。
- **強力な統合能力**：Confluent Cloudが提供する多様なコネクターを通じて、EMQXは他のデータベース、データウェアハウス、データストリーム処理システムなどと容易に統合でき、柔軟なデータ分析アプリケーションのための完全なIoTデータワークフローを構築します。
- **高スループット処理能力**：同期・非同期書き込みモードの両方をサポートし、リアルタイム優先とパフォーマンス優先のデータ書き込み戦略を使い分け、シナリオに応じてレイテンシとスループットを柔軟にバランスさせます。
- **効果的なトピックマッピング**：ブリッジ設定を通じて、多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティをKafkaヘッダーにマッピングすることをサポートし、1対1、1対多、多対多の柔軟なトピックマッピング方式を採用、MQTTトピックフィルター（ワイルドカード）もサポートします。

これらの機能は統合能力と柔軟性を高め、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続で伝送され、さらに効果的に保存・管理されます。

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

2. リージョン/ゾーンを選択します。デプロイリージョンがConfluent Cloudのリージョンと一致していることを確認し、**Continue**をクリックします。

![EMQX Confluent Select Cluster Region](./assets/confluent_create_cluster_2.a8f517c4.png)

3. クラスター名を入力し、**Launch cluster**をクリックします。

![image-20231013105736218](./assets/confluent_create_cluster_3.d38c10a0.png)

#### Confluent Cloud CLIを使ったトピックとAPIキーの作成

クラスターがConfluent Cloudで稼働したら、**Cluster Overview** -> **Cluster Settings**ページから**Bootstrap server**のURLを取得できます。

![image-20231013111959327](./assets/confluent_cluster_info.773da650.png)

Confluent Cloud CLIを使ってクラスターを管理できます。以下は基本的なCLIコマンドです。

##### Confluent Cloud CLIのインストール

```bash
curl -sL --http1.1 https://cnfl.io/cli | sh -s -- -b /usr/local/bin
```

すでにインストール済みの場合は以下でアップデート可能です。

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

既存のAPIキーを使う場合は以下でCLIに登録します。

```bash
confluent api-key store --resource <kafka_cluster_id>
Key: <API_KEY>
Secret: <API_SECRET>
```

APIキーとシークレットがない場合は以下で作成可能です。

```bash
$ confluent api-key create --resource <kafka_cluster_id>

APIキーの準備に数分かかる場合があります。
APIキーとシークレットは保存してください。シークレットは後で取得できません。
+------------+------------------------------------------------------------------+
| API Key    | YZ6R7YO6Q2WK35X7                                                 |
| API Secret | ****************************************                         |
+------------+------------------------------------------------------------------+
```

CLIに追加後、以下でAPIキーとシークレットを使用します。

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

以下のコマンドでプロデューサーを起動します。起動後、メッセージを入力してEnterを押すと、そのメッセージがトピックにパブリッシュされます。

```bash
confluent kafka topic produce testtopic-in
```

##### トピックからのメッセージ受信（Consumer）

以下のコマンドでコンシューマーを起動し、トピック内のすべてのメッセージを出力します。

```bash
confluent kafka topic consume -b testtopic-in
```

## コネクターの作成

Confluent Sinkアクションを追加する前に、EMQXとConfluent Cloud間の接続を確立するためにConfluent Producerコネクターを作成する必要があります。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックし、コネクター選択ページで**Confluent Producer**を選択して**Next**をクリックします。

3. 名前と説明を入力します（例：`my-confluent`）。この名前はConfluent Sinkとコネクターを関連付けるために使われ、クラスター内で一意である必要があります。

4. Confluent Cloud接続に必要なパラメーターを設定します。
   - **Bootstrap Hosts**：Confluent Cloudクラスター設定ページの**Endpoints**セクションからエンドポイント情報を入力します。
   
   - **Authentication**：Confluent Cloudクラスターで必要な認証方式を選択します。
     - **Basic auth**：Confluent Cloudで作成したAPI KeyとAPI Secretに対応する**Username**と**Password**を入力します。
     
     - **OAuth**：Confluent CloudのOAuth/OIDC設定に従い、トークンエンドポイント、クライアントID、クライアントシークレットなどOAuthパラメーターを設定します。
     
       OAuth設定はKafkaコネクターと同様です。詳細は[認証方式](./data-bridge-kafka.md#authentication-method)を参照してください。
     
   - その他のオプションはデフォルトのままか、ビジネス要件に応じて設定してください。
   
5. **Create**ボタンをクリックしてコネクター作成を完了します。

作成後、コネクターは自動的にConfluent Cloudに接続します。次に、このコネクターを基にルールを作成し、コネクターで設定したConfluentクラスターへデータを転送します。

## Confluent Sinkを使ったルールの作成

このセクションでは、MQTTトピック`t/#`のメッセージを処理し、処理結果をConfluentの`testtopic-in`トピックに送信するルールをEMQXで作成する方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules**をクリックします。

2. 右上の**Create**をクリックします。

3. ルールIDを入力します（例：`my_rule`）。

4. MQTTメッセージをトピック`t/#`からConfluentに転送したい場合、**SQL Editor**に以下の文を入力します。

   注意：独自のSQL構文を指定する場合は、`SELECT`部分にSinkが必要とするすべてのフィールドを含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   初心者の場合は、**SQL Example**や**Enable Test**をクリックしてSQLルールの学習・テストが可能です。

5. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action**のドロップダウンリストから`Confluent Producer`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のConfluent Producerアクションを選択します。この例では新規ルールに追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector**ドロップダウンリストから先ほど作成した`my-confluent`コネクターを選択します。隣のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。必要な設定パラメーターは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sinkのデータ送信方法を設定します。
   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**ドロップダウンから選択可能です。**Add**をクリックしてキー・バリューを追加できます。
   - **Message Key**：Kafkaメッセージのキーを入力します。純粋な文字列か、プレースホルダー（${var}）を含む文字列が利用可能です。
   - **Message Value**：Kafkaメッセージの値を入力します。純粋な文字列か、プレースホルダー（${var}）を含む文字列が利用可能です。
   - **Partition Strategy**：プロデューサーがKafkaパーティションにメッセージを分配する方法を選択します。
   - **Compression**：Kafkaメッセージ内のレコードを圧縮・解凍するための圧縮アルゴリズムを指定します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。プライマリSinkがメッセージ処理に失敗した場合にこれらがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#advanced-configuration)を参照してください。

11. **Create**ボタンをクリックしてSink作成を完了します。作成後、ページは**Create Rule**に戻り、新しいSinkがルールアクションに追加されます。

12. **Create**ボタンをクリックしてルール作成全体を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールが表示され、**Actions(Sink)**タブで新規Confluent Producer Sinkを確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認可能です。トポロジーを通じて、トピック`t/#`のメッセージがルール`my_rule`で解析され、Confluentに送信・保存されている様子を直感的に把握できます。

## Confluent Producerルールのテスト

Confluent Producerルールが期待通りに動作するかをテストするため、[MQTTX](https://mqttx.app/en)を使ってクライアントがEMQXにMQTTメッセージをパブリッシュするシミュレーションが可能です。

1. MQTTXを使い、トピック`t/1`にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Confluent" }'
   ```

2. **Actions(Sink)**ページでSink名をクリックし統計情報を確認します。Sinkの稼働状況をチェックし、新規の受信メッセージと送信メッセージが1件ずつあることを確認します。

3. 以下のConfluentコマンドでメッセージが`testtopic-in`トピックに書き込まれているか確認します。

   ```bash
   confluent kafka topic consume -b testtopic-in
   ```

## 詳細設定

このセクションでは、コネクターおよびSink/Sourceのパフォーマンス最適化や特定シナリオに合わせたカスタマイズ操作が可能な詳細設定オプションを説明します。該当オブジェクト作成時に**Advanced Settings**を展開し、ビジネス要件に応じて以下の設定を行えます。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producerのみ）有効にすると、クライアントがメタデータ取得要求を送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証有効時は認証時間も含む）         | `5`秒              |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。Sinkが接続先リソース（例：Confluentクラスター）の完全稼働を確認してから処理を進めるための設定。 | `5`秒              |
| Health Check Interval             | コネクターの稼働状態チェック間隔                               | `15`秒             |
| Min Metadata Refresh Interval     | Kafkaブローカー・トピックのメタデータ更新最小間隔。短すぎるとKafkaサーバー負荷増加の恐れあり。 | `3`秒              |
| Metadata Request Timeout          | Kafkaからメタデータを要求する際の最大待機時間                   | `5`秒              |
| Socket Send / Receive Buffer Size | ソケットバッファサイズの管理。ネットワーク伝送性能最適化に寄与。 | `1`MB              |
| No Delay                          | システムカーネルがTCPソケットを即時送信するか遅延送信するかの設定。トグルONで即時送信。OFFの場合、送信内容が少ないと40ミリ秒程度の遅延が発生。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続のTCPキープアライブ機能を有効化し、長時間の非通信による接続切断を防止。<br />値はカンマ区切りの3つの数字（Idle, Interval, Probes）で指定。<br />Idle：接続がアイドル状態になる秒数（Linuxデフォルト7200秒）<br />Interval：キープアライブプローブ間隔（Linuxデフォルト75秒）<br />Probes：応答なしと判断するまでの最大プローブ回数（Linuxデフォルト9回）<br />例：`240,30,5`は240秒アイドル後に30秒間隔で5回プローブ送信し応答なければ切断判定。 | `none`             |

### Confluent Producer Sink設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Health Check Interval            | Sinkの稼働状態チェック間隔                                   | `15`秒             |
| Max Batch Bytes                  | Kafkaバッチ内で収集するメッセージの最大バイト数。Kafkaブローカーのデフォルトは1MBだが、EMQXはメッセージエンコードのオーバーヘッドを考慮しやや小さめに設定。単一メッセージが上限を超える場合は別バッチで送信。 | `896`KB            |
| Required Acks                    | Kafkaパーティションリーダーがフォロワーから待つアックの種類。<br />`all_isr`：全てのインシンクレプリカからのアックを要求<br />`leader_only`：リーダーのみからのアックを要求<br />`none`：Kafkaからのアック不要 | `all_isr`          |
| Partition Count Refresh Interval | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加検知後、EMQXは`partition_strategy`に基づき新パーティションにメッセージを配信。 | `60`秒             |
| Max Inflight                     | Kafkaプロデューサーがアック受信前に送信可能な最大バッチ数（パーティション毎）。値が大きいほどスループット向上だが、1より大きいとメッセージ順序が入れ替わる可能性あり。未アックメッセージ数を制御し負荷バランスを取る。 | `10`               |
| Query Mode (Producer)            | 非同期または同期クエリモードを選択し、要件に応じてメッセージ送信を最適化。非同期モードではKafka書き込みがMQTTパブリッシュをブロックしないが、クライアントがKafka到着前にメッセージを受信する可能性あり。 | `Async`            |
| Synchronous Query Timeout        | 同期モード時の最大待機時間。メッセージ送信完了を待つ時間制限。`Sync`モード時のみ適用。 | `5`秒              |
| Buffer Mode                      | メッセージ送信前のバッファリング方式。メモリバッファは高速だがノード再起動で消失。ディスクバッファは永続化可能。ハイブリッドはメモリから一定サイズでディスクにオフロード。 | `memory`           |
| Per-partition Buffer Limit       | Kafkaパーティション毎の最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ空間を確保。メモリ使用量と性能のバランス調整に寄与。 | `2`GB              |
| Segment File Bytes               | バッファモードが`disk`または`hybrid`時に適用。メッセージ保存用セグメントファイルのサイズを制御し、ディスクストレージの最適化に影響。 | `100`MB            |
| Memory Overload Protection       | バッファモードが`memory`時に適用。メモリ使用過多時に古いバッファメッセージを自動破棄し、システム安定性を確保。Linux環境のみ有効。 | Disabled           |

### <!-- Confluent Consumer Source Configuration -->

## 参考情報

EMQXはConfluent/Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクもご参照ください。

**ブログ：**

- [MQTTとKafkaで構築するコネクテッドカーのストリーミングデータパイプライン](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka | IoTメッセージングとストリームデータ統合の実践](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)
