# ConfluentへMQTTデータをストリーム配信する

[Confluent Cloud](https://www.confluent.io/)はApache Kafkaをベースにした、レジリエントでスケーラブルかつフルマネージドのストリーミングデータサービスです。EMQXはルールエンジンとSinkを通じてConfluentとのデータ統合をサポートし、MQTTデータをConfluentへ簡単にストリーミングしてリアルタイム処理、保存、分析を可能にします。

![EMQX Confluent Integration](./assets/confluent-integration.png)

本ページでは主にConfluent統合の機能と利点を紹介し、Confluent Cloudの設定およびEMQXでのConfluent Producer Sinkの作成方法を案内します。

## 動作概要

Confluentデータ統合はEMQXのすぐに使える機能であり、MQTTベースのIoTデータとConfluentの強力なデータ処理機能を橋渡しします。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、両プラットフォーム間のデータフローと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は自動車IoTにおけるEMQXとConfluentのデータ統合の典型的なアーキテクチャを示しています。

![Confluent Architecture](./assets/confluent-architecture.png)

Confluentへのデータの入出力は、Confluent Sink（Confluentへメッセージ送信）とConfluent Source（Confluentからメッセージ受信）を介して行われます。Confluent Sinkを作成した場合のワークフローは以下の通りです。

1. **メッセージのパブリッシュと受信**：車両に接続されたIoTデバイスはMQTTプロトコルでEMQXに正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：これらのMQTTメッセージは、組み込みのルールエンジンとメッセージサーバーの連携により、トピックマッチングルールに従って処理されます。メッセージが到着しルールエンジンを通過すると、事前定義された処理ルールが評価されます。ペイロード変換を指定するルールがあれば、データフォーマット変換、特定情報のフィルタリング、追加コンテキストによるペイロードの拡充などの変換が適用されます。
3. **Confluentへのブリッジ**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをConfluentへ転送するアクションが実行されます。Confluent Sink機能を用いて、MQTTトピックはConfluent内の事前定義されたKafkaトピックにマッピングされ、処理済みの全メッセージとデータがこれらのトピックに書き込まれます。

車両データがConfluentに入力されると、以下のように柔軟にデータを活用できます。

- サービスはConfluentと直接統合し、特定トピックのリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を行えます。
- Kafka Streamsを利用してストリーム処理を実施し、車両状態をメモリ上で集約・相関させてリアルタイム監視が可能です。
- ConfluentのStream Designerコンポーネントを使い、MySQLやElasticSearchなど外部システムへのデータ出力用コネクターを選択して保存できます。

## 機能と利点

Confluentとのデータ統合は以下の機能と利点をビジネスにもたらします。

- **大規模メッセージ送信の信頼性**：EMQXとConfluent Cloudはどちらも高信頼なクラスター機構を用い、安定かつ信頼性の高いメッセージ送信チャネルを確立し、大規模IoTデバイスからのメッセージのロスゼロを保証します。ノード追加による水平スケールやリソースの動的調整により、突発的な大規模メッセージにも対応し、メッセージ送信の可用性を確保します。
- **強力なデータ処理能力**：EMQXのローカルルールエンジンとConfluent Cloudは、デバイスからアプリケーションまでの異なる段階で信頼性の高いストリーミングデータ処理を提供します。リアルタイムのデータフィルタリング、フォーマット変換、集計分析などシナリオに応じた処理が可能で、より複雑なIoTメッセージ処理ワークフローを実現し、データ分析アプリケーションのニーズに応えます。
- **強力な統合機能**：Confluent Cloudが提供する各種コネクターを通じて、EMQXは他のデータベース、データウェアハウス、データストリーム処理システムなどと容易に統合でき、アジャイルなデータ分析アプリケーションのための完全なIoTデータワークフローを構築します。
- **高スループット処理能力**：同期・非同期の両書き込みモードをサポートし、リアルタイム優先とパフォーマンス優先のデータ書き込み戦略を使い分け、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整できます。
- **効果的なトピックマッピング**：ブリッジ設定により、多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティのKafkaヘッダーへのマッピングをサポートし、1対1、1対多、多対多の柔軟なトピックマッピング方式を採用、MQTTトピックフィルター（ワイルドカード）もサポートします。

これらの機能は統合能力と柔軟性を高め、効果的で堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続で送信され、さらに効果的に保存・管理されます。

## はじめる前に

このセクションではEMQXダッシュボードでConfluentデータ統合を設定するための準備作業を説明します。

### 前提条件

- [ルールエンジン](./rules.md)の理解
- [Sink](./data-bridges.md)の理解

### Confluent Cloudの設定

Confluentデータ統合を作成する前に、Confluent Cloudコンソールでクラスターを作成し、Confluent Cloud CLIを使ってトピックとAPIキーを作成する必要があります。

#### クラスターの作成

1. Confluent Cloudコンソールにログインし、クラスターを作成します。例としてStandardクラスターを選択し、**Begin configuration**をクリックします。

![EMQX Confluent Create Cluster](./assets/confluent_create_cluster_1.2d537cc0.png)

2. リージョン／ゾーンを選択します。デプロイリージョンがConfluent Cloudのリージョンと一致していることを確認し、**Continue**をクリックします。

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

##### APIキーとシークレットの利用

既存のAPIキーを使う場合は、以下のコマンドでCLIに登録します。

```bash
confluent api-key store --resource <kafka_cluster_id>
Key: <API_KEY>
Secret: <API_SECRET>
```

APIキーとシークレットを持っていない場合は、以下のコマンドで作成できます。

```bash
$ confluent api-key create --resource <kafka_cluster_id>

APIキーが利用可能になるまで数分かかることがあります。
APIキーとシークレットは保存してください。シークレットは後から取得できません。
+------------+------------------------------------------------------------------+
| API Key    | YZ6R7YO6Q2WK35X7                                                 |
| API Secret | ****************************************                         |
+------------+------------------------------------------------------------------+
```

CLIに追加後、以下のコマンドでAPIキーとシークレットを使用します。

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

##### トピックへのメッセージ送信（Producer）

以下のコマンドでプロデューサーを作成できます。開始後、メッセージを入力してEnterを押すと、該当トピックにメッセージが送信されます。

```bash
confluent kafka topic produce testtopic-in
```

##### トピックからのメッセージ受信（Consumer）

以下のコマンドでコンシューマーを作成できます。該当トピックの全メッセージが出力されます。

```bash
confluent kafka topic consume -b testtopic-in
```

## コネクターの作成

Confluent Sinkアクションを追加する前に、EMQXとConfluent Cloud間の接続を確立するためにConfluent Producerコネクターを作成する必要があります。

1. EMQXダッシュボードで**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックし、コネクター選択ページで**Confluent Producer**を選択して**Next**をクリックします。

3. 名前と説明を入力します。例：`my-confluent`。この名前はConfluent Sinkとコネクターの紐付けに使われ、クラスター内で一意である必要があります。

4. Confluent Cloudへの接続に必要なパラメーターを設定します。
   - **Bootstrap Hosts**：Confluent Cloudクラスター設定ページの**Endpoints**セクションからエンドポイント情報を入力します。
   
   - **Authentication**：Confluent Cloudクラスターで必要な認証方式を選択します。
     - **Basic auth**：Confluent Cloudで作成したAPIキーとAPIシークレットに対応する**Username**と**Password**を入力します。
     
     - **OAuth**：Confluent CloudのOAuth/OIDC設定に従い、トークンエンドポイント、クライアントID、クライアントシークレットなどOAuthパラメーターを設定します。
     
       OAuth設定はKafkaコネクターと同様です。各パラメーターの詳細は[認証方式](./data-bridge-kafka.md#authentication-method)を参照してください。

   - **Request Timeout**：EMQXがConfluentからの応答を待つ最大時間（秒）を指定します。デフォルトは`30`秒です。タイムアウト超過時は接続が古くなったと判断し再接続します。値が小さすぎると、Confluentがリクエストを受け入れても応答を遅延させる場合があり、再接続後に同じバッチを再送して重複メッセージや下流の過剰データを招く恐れがあります。
   - その他のオプションはデフォルトのままか、ビジネスニーズに応じて設定してください。
   
5. **Create**ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にConfluent Cloudへ接続します。次に、このコネクターを基にしたルールを作成し、コネクターで設定したConfluentクラスターへデータを転送します。

## Confluent Sinkを使ったルールの作成

このセクションでは、MQTTトピック`t/#`のメッセージを処理し、処理結果をConfluentの`testtopic-in`トピックに送信するルールをEMQXで作成する方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules**をクリックします。

2. 右上の**Create**をクリックします。

3. ルールIDを入力します。例：`my_rule`。

4. MQTTメッセージをトピック`t/#`からConfluentへ転送する場合、**SQL Editor**に以下の文を入力します。

   注意：独自のSQL構文を指定する場合は、`SELECT`部分にSinkが必要とするすべてのフィールドを含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注意：初心者の方は**SQL Example**や**Enable Test**をクリックしてSQLルールの学習やテストが可能です。

5. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action**ドロップダウンから`Confluent Producer`を選択し、**Action**ドロップダウンはデフォルトの`Create Action`のままか、既存のConfluent Producerアクションを選択します。この例では新規ルールに新規アクションを追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-confluent`コネクターを選択します。ドロップダウン横のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。必要な設定パラメーターは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sinkのデータ送信方法を設定します。
   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトである必要があります。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**ドロップダウンから選択可能です。**Add**をクリックしてキー・バリューの追加もできます。
   - **Message Key**：Kafkaメッセージのキーを入力します。純粋な文字列か、プレースホルダー（${var}）を含む文字列が指定可能です。
   - **Message Value**：Kafkaメッセージの値を入力します。こちらも純粋な文字列かプレースホルダーを含む文字列が指定可能です。
   - **Partition Strategy**：プロデューサーがKafkaパーティションにメッセージを分配する方法を選択します。
   - **Compression**：Kafkaメッセージのレコードを圧縮／解凍するための圧縮アルゴリズムを指定します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義可能です。プライマリSinkがメッセージ処理に失敗した場合にこれらがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#advanced-configuration)を参照してください。

11. **Create**ボタンをクリックしてSinkの作成を完了します。作成後は**Create Rule**ページに戻り、新しいSinkがルールアクションに追加されます。

12. **Create**ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブで新規Confluent Producer Sinkも確認できます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを表示できます。トポロジーでは、トピック`t/#`のメッセージがルール`my_rule`で解析され、Confluentに送信・保存される様子を直感的に確認できます。

## Confluent Producerルールのテスト

Confluent Producerルールが期待通り動作するかテストするため、[MQTTX](https://mqttx.app/en)を使ってクライアントがEMQXにMQTTメッセージをパブリッシュする動作をシミュレートできます。

1. MQTTXでトピック`t/1`にメッセージを送信します。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Confluent" }'
   ```

2. **Actions(Sink)**ページでSink名をクリックし統計情報を表示します。Sinkの稼働状況を確認し、新規受信メッセージ数と新規送信メッセージ数がそれぞれ1件あることを確認します。

3. 以下のConfluentコマンドで`testtopic-in`トピックにメッセージが書き込まれているか確認します。

   ```bash
   confluent kafka topic consume -b testtopic-in
   ```

## 詳細設定

このセクションでは、コネクターやSink/Sourceのパフォーマンスを最適化し、特定シナリオに応じたカスタマイズ操作が可能な詳細設定オプションを説明します。該当オブジェクト作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

### コネクター設定

| 項目                              | 説明                                                         | 推奨値             |
| --------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producerのみ）有効にすると、クライアントがメタデータフェッチ要求を送信した際にKafkaトピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証時間含む）                     | `5`秒              |
| Start Timeout                     | 自動起動したリソースが正常状態になるまで待つ最大時間（秒）。Sinkが接続先リソース（例：Confluentクラスター）の稼働を確認してから処理を進めるための設定。 | `5`秒              |
| Health Check Interval             | コネクターの稼働状態をチェックする間隔                       | `15`秒             |
| Min Metadata Refresh Interval     | Kafkaブローカーやトピックのメタデータを更新する最小間隔。小さすぎるとKafkaサーバーに過負荷をかける可能性あり。 | `3`秒              |
| Metadata Request Timeout          | Kafkaにメタデータ要求を送る際の最大待機時間                   | `5`秒              |
| Socket Send / Receive Buffer Size | ネットワーク伝送性能最適化のためのソケットバッファサイズ管理 | `1`MB              |
| No Delay                          | TCPソケットを即時送信するか遅延送信するかの設定。オンで即時送信。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続のTCPキープアライブ設定。`Idle, Interval, Probes`形式のカンマ区切り3数値で指定。<br>Idle：接続がアイドル状態になる秒数（Linuxデフォルト7200秒）<br>Interval：キープアライブプローブ間隔（Linuxデフォルト75秒）<br>Probes：応答なしで接続切断と判断するプローブ回数（Linuxデフォルト9回）<br>例：`240,30,5`は240秒アイドル後にプローブ開始、30秒間隔で最大5回試行。 | `none`             |

### Confluent Producer Sink設定

| 項目                              | 説明                                                         | 推奨値             |
| --------------------------------- | ------------------------------------------------------------ | ------------------ |
| Health Check Interval            | Sinkの稼働状態をチェックする間隔                             | `15`秒             |
| Max Batch Bytes                  | Kafkaバッチ内で収集するメッセージの最大サイズ（バイト）。Kafkaブローカーのデフォルトは1MBだが、EMQXはエンコードオーバーヘッドを考慮し1MB未満に設定。単一メッセージが超過する場合は別バッチで送信。 | `896`KB            |
| Required Acks                    | Kafkaパーティションリーダーがフォロワーから待つアックの種類：<br>`all_isr`：全てのインシンクレプリカから<br>`leader_only`：リーダーのみ<br>`none`：不要 | `all_isr`          |
| Partition Count Refresh Interval | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加時は`partition_strategy`に従い新パーティションへメッセージを送信。 | `60`秒             |
| Max Inflight                     | Kafkaプロデューサーがアック受信前に送信可能な最大バッチ数（パーティション毎）。値が大きいほどスループット向上だが、1超過時はメッセージ順序乱れのリスクあり。 | `10`秒             |
| Query Mode (Producer)            | 非同期／同期クエリモードを選択し、メッセージ送信を最適化。非同期はMQTTパブリッシュをブロックしないが、クライアントがKafka到着前にメッセージを受け取る可能性あり。 | `Async`            |
| Synchronous Query Timeout        | 同期モード時の最大待機時間。メッセージ送信完了をタイムアウトで制御。`Sync`モード時のみ有効。 | `5`秒              |
| Buffer Mode                      | メッセージを送信前にバッファリングするか設定。<br>`memory`：メモリバッファ（EMQX再起動で消失）<br>`disk`：ディスクバッファ（再起動耐性あり）<br>`hybrid`：初めはメモリバッファ、一定量超過でディスクにオフロード（再起動で消失） | `memory`           |
| Per-partition Buffer Limit       | Kafkaパーティション毎の最大バッファサイズ（バイト）。超過時は古いメッセージを破棄して領域確保。メモリ使用量と性能のバランス調整に有効。 | `2`GB              |
| Segment File Bytes               | バッファモードが`disk`または`hybrid`時に適用。メッセージ保存用分割ファイルのサイズを制御し、ディスクストレージ最適化に影響。 | `100`MB            |
| Memory Overload Protection       | バッファモードが`memory`時に適用。メモリ圧迫時に古いメッセージを自動破棄し、システム安定性を確保。Linuxのみ有効。 | Disabled           |
| Max Batch Age                    | プロデューサーバッファ内でメッセージが送信されずに保持できる最大期間。バッチ内全メッセージが超過時にバッチ破棄。切断中のバッファリングやアック待ちメッセージも対象。破棄されたメッセージは`dropped.expired`メトリクスにカウント。デフォルトは`infinity`で期限切れなし。バッファオーバーフロー時は破棄あり。 | `infinity`         |
| Max Retries                      | Confluentからリトライ可能なエラー応答時の最大リトライ回数。初回と全リトライ失敗時はバッチ破棄し`failed`メトリクスにカウント。明示的なエラー応答のみリトライ回数加算。接続喪失による再送は加算せず`max_batch_age`で制限。デフォルトは`infinity`で無制限。 | `infinity`         |
| Reconnect Delay                  | 接続喪失後にプロデューサーが再接続を試みるまでの遅延時間。切断中もメッセージはバッファに蓄積されるが、バッファ制限と`max_batch_age`の影響を受ける。デフォルトは`2`秒。 | `2`秒              |
| Max Linger Time                  | パーティション毎のプロデューサーがより大きなバッチを作るために待機する最大時間。全バッファモードに適用。デフォルト`0`は待機なしで低レイテンシ最適化。小さな遅延を許容するとリクエスト数削減可能。ディスクバッファ時はバッファ書き込み前に待機。ディスクIO削減には最低`5ms`推奨。 | `0`ミリ秒          |
| Max Linger Bytes                 | パーティション毎のプロデューサーがバッチ送信前に蓄積する最大バイト数。 | `10`MB             |

### <!-- Confluent Consumer Source Configuration -->

## 追加情報

EMQXはConfluent/Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクもご参照ください。

**ブログ：**

- [MQTTとKafkaでつなぐコネクテッドビークルのストリーミングデータパイプライン構築](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka | IoTメッセージングとストリームデータ統合の実践](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)
