# ConfluentへのMQTTデータストリーミング

[Confluent Cloud](https://www.confluent.io/)はApache Kafkaをベースにした、レジリエンスが高くスケーラブルでフルマネージドのストリーミングデータサービスです。EMQXはルールエンジンとSinkを通じてConfluentとのデータ統合をサポートしており、MQTTデータをConfluentに簡単にストリーミングしてリアルタイム処理、保存、分析が可能です。

![EMQX Confluent Integration](./assets/confluent-integration.png)

本ページでは主にConfluent統合の機能と利点を紹介し、Confluent Cloudの設定およびEMQXでのConfluent Producer Sinkの作成方法を案内します。

## 動作概要

Confluentデータ統合はEMQXのすぐに使える機能であり、MQTTベースのIoTデータとConfluentの強力なデータ処理機能を橋渡しします。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、両プラットフォーム間のデータフローと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は、自動車IoTにおけるEMQXとConfluentのデータ統合の典型的なアーキテクチャを示しています。

![Confluent Architecture](./assets/confluent-architecture.png)

Confluentへのデータの入出力は、Confluent Sink（Confluentへメッセージ送信）とConfluent Source（Confluentからメッセージ受信）を介して行われます。Confluent Sinkを作成した場合、そのワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：車両に接続されたIoTデバイスはMQTTプロトコルでEMQXに正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQXはこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：これらのMQTTメッセージは、組み込みのルールエンジンとメッセージサーバーの協調動作により、トピックマッチングルールに従って処理されます。メッセージが到着してルールエンジンを通過すると、事前定義された処理ルールが評価されます。ペイロード変換を指定するルールがあれば、データ形式変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Confluentへのブリッジング**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをConfluentに転送するアクションが実行されます。Confluent Sink機能を使い、MQTTトピックはConfluentの事前定義されたKafkaトピックにマッピングされ、処理済みのメッセージとデータがこれらのトピックに書き込まれます。

車両データがConfluentに入力されると、以下のように柔軟にデータを活用できます。

- サービスは直接Confluentと連携し、特定トピックのリアルタイムデータストリームを消費してカスタマイズされた業務処理を行えます。
- Kafka Streamsを利用してストリーム処理を行い、車両状態をメモリ内で集約・相関させてリアルタイム監視が可能です。
- ConfluentのStream Designerコンポーネントを活用し、MySQLやElasticSearchなど外部システムへのデータ出力用コネクターを選択して保存できます。

## 機能と利点

Confluentとのデータ統合は、以下の機能と利点をビジネスにもたらします。

- **大規模メッセージ伝送の信頼性**：EMQXとConfluent Cloudはどちらも高信頼のクラスター機構を用い、安定かつ信頼性の高いメッセージ伝送チャネルを構築し、大規模IoTデバイスからのメッセージのロスゼロを保証します。両者はノード追加による水平スケールとリソースの動的調整に対応し、突発的な大規模メッセージにも対応可能でメッセージ伝送の可用性を確保します。
- **強力なデータ処理能力**：EMQXのローカルルールエンジンとConfluent Cloudは、デバイスからアプリケーションまでの異なる段階で信頼性の高いストリーミングデータ処理機能を提供します。リアルタイムのデータフィルタリング、形式変換、集約分析などシナリオに応じた処理が可能で、より複雑なIoTメッセージ処理ワークフローを実現し、データ分析アプリケーションのニーズに応えます。
- **強力な統合能力**：Confluent Cloudが提供する多様なコネクターを通じて、EMQXは他のデータベース、データウェアハウス、データストリーム処理システムなどと容易に統合でき、柔軟なデータ分析アプリケーションのための完全なIoTデータワークフローを構築します。
- **高スループット処理能力**：同期・非同期の両書き込みモードをサポートし、リアルタイム優先や性能優先などシナリオに応じたデータ書き込み戦略を区別可能で、レイテンシとスループットのバランスを柔軟に調整できます。
- **効果的なトピックマッピング**：ブリッジ設定を通じて多数のIoT業務トピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティをKafkaヘッダーにマッピングすることをサポートし、1対1、1対多、多対多など多様なトピックマッピング方式を採用し、MQTTトピックフィルター（ワイルドカード）もサポートします。

これらの機能により統合能力と柔軟性が向上し、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続を介して伝送され、さらに効果的に保存・管理されます。

## はじめる前に

本セクションでは、EMQXダッシュボードでConfluentデータ統合を設定するための準備作業を説明します。

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

クラスターがConfluent Cloudで稼働したら、**Cluster Overview** -> **Cluster Settings**ページから**Bootstrap server**のURLを取得できます。

![image-20231013111959327](./assets/confluent_cluster_info.773da650.png)

Confluent Cloud CLIを使ってクラスターを管理できます。以下は基本的なCLIコマンドです。

##### Confluent Cloud CLIのインストール

```bash
curl -sL --http1.1 https://cnfl.io/cli | sh -s -- -b /usr/local/bin
```

すでにインストール済みの場合は、以下のコマンドでアップデート可能です。

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

##### APIキーとシークレットの利用

既存のAPIキーを使う場合は、以下のコマンドでCLIに追加します。

```bash
confluent api-key store --resource <kafka_cluster_id>
Key: <API_KEY>
Secret: <API_SECRET>
```

APIキーとシークレットを持っていない場合は、以下のコマンドで作成できます。

```bash
$ confluent api-key create --resource <kafka_cluster_id>

APIキーの準備に数分かかる場合があります。
APIキーとシークレットを保存してください。シークレットは後で取得できません。
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

`testtopic-in`という名前のトピックを作成するには以下のコマンドを実行します。

```bash
confluent kafka topic create testtopic-in
```

トピック一覧は以下のコマンドで確認できます。

```bash
confluent kafka topic list
```

##### トピックへのメッセージ送信（プロデュース）

以下のコマンドでプロデューサーを作成できます。起動後、メッセージを入力してEnterを押すと、該当トピックにメッセージが送信されます。

```bash
confluent kafka topic produce testtopic-in
```

##### トピックからのメッセージ受信（コンシューム）

以下のコマンドでコンシューマーを作成できます。該当トピックの全メッセージが出力されます。

```bash
confluent kafka topic consume -b testtopic-in
```

## コネクターの作成

Confluent Sinkアクションを追加する前に、EMQXとConfluent Cloud間の接続を確立するためにConfluentプロデューサーコネクターを作成する必要があります。

1. EMQXダッシュボードにアクセスし、**Integration** -> **Connectors**をクリックします。
2. ページ右上の**Create**をクリックし、コネクター選択画面で**Confluent Producer**を選択して**Next**をクリックします。
3. 名前と説明を入力します。例として`my-confluent`とします。この名前はConfluent Sinkとコネクターを紐付けるために使用され、クラスター内で一意である必要があります。
4. Confluent Cloudへの接続に必要なパラメーターを設定します：
   - **Bootstrap Hosts**：Confluentクラスター設定ページのEndpoints情報に対応します。
   - **Username**と**Password**：先ほどConfluent Cloud CLIで作成したAPIキーとシークレットを入力します。
   - 他のオプションはデフォルトのままか、業務要件に応じて設定してください。
5. **Create**ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にConfluent Cloudに接続します。次に、このコネクターを基にしたルールを作成し、コネクターで設定したConfluentクラスターへデータを転送します。

## Confluent Sinkを使ったルールの作成

このセクションでは、MQTTトピック`t/#`からのメッセージを処理し、処理結果をConfluentの`testtopic-in`トピックに送信するルールをEMQXで作成する方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules**をクリックします。

2. 右上の**Create**をクリックします。

3. ルールIDを入力します。例として`my_rule`とします。

4. MQTTメッセージをトピック`t/#`からConfluentに転送したい場合は、**SQL Editor**に以下の文を入力します。

   注意：独自のSQL構文を指定する場合は、`SELECT`部分にSinkが必要とするすべてのフィールドを含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注意：初心者の方は**SQL Example**や**Enable Test**をクリックしてSQLルールの学習とテストが可能です。

5. + **Add Action**ボタンをクリックして、ルールによってトリガーされるアクションを定義します。**Type of Action**のドロップダウンリストから`Confluent Producer`を選択し、**Action**ドロップダウンはデフォルトの`Create Action`のままか、既存のConfluent Producerアクションを選択します。この例では新規ルールを作成し、アクションを追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-confluent`コネクターを選択します。ドロップダウン横のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。必要な設定パラメーターは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sinkのデータ送信方法を設定します：

   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**ドロップダウンから選択可能です。**Add**をクリックしてキー・バリューのペアを追加できます。
   - **Message Key**：Kafkaメッセージのキーです。プレーンな文字列か、プレースホルダー（${var}）を含む文字列を入力できます。
   - **Message Value**：Kafkaメッセージの値です。プレーンな文字列か、プレースホルダー（${var}）を含む文字列を入力できます。
   - **Partition Strategy**：プロデューサーがKafkaパーティションにメッセージを配布する方法を選択します。
   - **Compression**：Kafkaメッセージ内のレコードを圧縮/解凍するための圧縮アルゴリズムの使用有無を指定します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#詳細設定)を参照してください。

11. **Create**ボタンをクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新規Sinkがルールアクションに追加されます。

12. **Create**ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブで新規Confluent Producer Sinkも確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認できます。トポロジーを通じて、トピック`t/#`のメッセージがルール`my_rule`で解析され、Confluentに送信・保存されていることを直感的に把握できます。

## Confluent Producerルールのテスト

Confluent Producerルールが期待通りに動作するかテストするには、[MQTTX](https://mqttx.app/en)を使ってクライアントをシミュレートし、EMQXにMQTTメッセージをパブリッシュします。

1. MQTTXでトピック`t/1`にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Confluent" }'
   ```

2. **Actions(Sink)**ページでSink名をクリックし、統計情報を表示します。Sinkの稼働状況を確認し、新規受信メッセージ数と送信メッセージ数がそれぞれ1件増えているはずです。

3. 以下のConfluentコマンドで`testtopic-in`トピックにメッセージが書き込まれているか確認します。

   ```bash
   confluent kafka topic consume -b testtopic-in
   ```

## 詳細設定

本セクションでは、コネクターおよびSink/Sourceのパフォーマンス最適化や特定シナリオに応じたカスタマイズ運用のための高度な設定オプションを説明します。該当オブジェクト作成時に**Advanced Settings**を展開し、業務要件に応じて以下の設定を行えます。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producerのみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証有効時は認証時間含む）           | `5`秒              |
| Start Timeout                     | 自動起動リソースが正常状態になるまでの最大待機時間（秒）。Sinkが接続先リソース（例：Confluentクラスター）の完全稼働を確認してから処理を進めるための設定です。 | `5`秒              |
| Health Check Interval             | コネクターの稼働状態チェック間隔                              | `15`秒             |
| Min Metadata Refresh Interval     | Kafkaブローカーやトピックのメタデータを更新する最小間隔。小さすぎるとKafkaサーバーの負荷が増大します。 | `3`秒              |
| Metadata Request Timeout          | Kafkaからメタデータを要求する際の最大待機時間                 | `5`秒              |
| Socket Send / Receive Buffer Size | ソケットバッファサイズを管理し、ネットワーク伝送性能を最適化   | `1`MB              |
| No Delay                          | システムカーネルがTCPソケットを即時送信するか遅延送信するかを選択。トグルONで即時送信。OFFの場合、送信内容が少ないと40ミリ秒程度の遅延が発生する可能性あり。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続のTCPキープアライブ機構を有効化し、長時間の非アクティブによる接続切断を防止。値は`Idle, Interval, Probes`の3つの数値のカンマ区切りリストで指定。<br />Idle：接続がアイドル状態となってからキープアライブプローブを送信開始するまでの秒数（Linuxのデフォルトは7200秒）。<br />Interval：各キープアライブプローブ間の秒数（Linuxのデフォルトは75秒）。<br />Probes：応答なしと判断するまでの最大プローブ回数（Linuxのデフォルトは9回）。<br />例：`240,30,5,`は240秒のアイドル後にプローブ開始し、30秒ごとにプローブを送り、5回応答がなければ接続切断と判断。 | `none`             |

### Confluent Producer Sink設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Health Check Interval            | Sinkの稼働状態チェック間隔                                   | `15`秒             |
| Max Batch Bytes                  | Kafkaバッチ内で収集するメッセージの最大サイズ（バイト）。Kafkaブローカーのデフォルトは1MBですが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮し、特に小さいメッセージが多い場合に備えて1MBよりやや小さく設定しています。単一メッセージがこの制限を超える場合は別バッチとして送信されます。 | `896`KB            |
| Required Acks                    | Kafkaパーティションリーダーがフォロワーから受け取る必要のある確認応答の種類：<br />`all_isr`：すべてのインシンクレプリカからの確認が必要。<br />`leader_only`：リーダーのみの確認が必要。<br />`none`：Kafkaからの確認不要。 | `all_isr`          |
| Partition Count Refresh Interval | Kafkaプロデューサーがパーティション数増加を検知する間隔。パーティション数増加時、EMQXは`partition_strategy`に基づき新パーティションにメッセージを送信します。 | `60`秒             |
| Max Inflight                     | KafkaプロデューサーがKafkaからのアックを受け取る前に送信可能な最大バッチ数（パーティションごと）。値が大きいほどスループット向上。ただし1より大きい場合、メッセージの順序入れ替わりのリスクあり。未確認メッセージ数を制御し、負荷バランスを調整。 | `10`               |
| Query Mode (Producer)            | 非同期または同期クエリモードを選択し、要件に応じてメッセージ送信を最適化。非同期モードではKafkaへの書き込みがMQTTメッセージのパブリッシュをブロックしませんが、クライアントがKafka到着前にメッセージを受信する可能性があります。 | `Async`            |
| Synchronous Query Timeout        | 同期クエリモード時の最大待機時間。メッセージ送信完了を適時確認し、長時間待機を防止。ブリッジのクエリモードが`Sync`に設定されている場合にのみ適用。 | `5`秒              |
| Buffer Mode                      | メッセージを送信前にバッファリングするかどうかを定義。メモリバッファリングは送信速度向上に寄与。<br />`memory`：メモリにバッファ。EMQXノード再起動時にメッセージは失われる。<br />`disk`：ディスクにバッファ。EMQXノード再起動後もメッセージは保持される。<br />`hybrid`：初めはメモリにバッファし、一定サイズ（`segment_bytes`設定参照）に達すると徐々にディスクにオフロード。メモリモード同様、EMQXノード再起動時はメッセージが失われる。 | `memory`           |
| Per-partition Buffer Limit       | Kafkaパーティションごとの最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄してバッファ空間を確保。メモリ使用量と性能のバランス調整に役立つ。 | `2`GB              |
| Segment File Bytes               | バッファモードが`disk`または`hybrid`の場合に適用。メッセージ保存用のセグメントファイルサイズを制御し、ディスクストレージの最適化に影響。 | `100`MB            |
| Memory Overload Protection       | バッファモードが`memory`の場合に適用。メモリ使用率が高い場合に古いバッファメッセージを自動破棄し、システムの安定性を確保。<br />**注意**：Linuxシステムのみ有効。 | Disabled           |

### <!-- Confluent Consumer Source Configuration -->

## 参考情報

EMQXはConfluent/Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクもご参照ください。

**ブログ：**

- [MQTTとKafkaでつくるコネクテッドカーのストリーミングデータパイプライン](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka | IoTメッセージングとストリームデータ統合の実践](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/blog/mqtt-performance-benchmark-testing-emqx-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)
