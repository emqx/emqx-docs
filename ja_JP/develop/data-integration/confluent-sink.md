# ConfluentへのMQTTデータストリーム

[Confluent Cloud](https://www.confluent.io/)はApache Kafkaをベースにした、レジリエントでスケーラブルかつフルマネージドのストリーミングデータサービスです。EMQXはルールエンジンとSinkを通じてConfluentとのデータ統合をサポートしており、MQTTデータをConfluentに簡単にストリーミングしてリアルタイム処理、保存、分析を可能にします。

![EMQX Confluent Integration](./assets/confluent-integration.png)

本ページでは主にConfluent統合の機能と利点を紹介し、Confluent Cloudの設定およびEMQXでのConfluent Producer Sinkの作成方法を案内します。

## 動作概要

Confluentデータ統合はEMQXの即利用可能な機能であり、MQTTベースのIoTデータとConfluentの強力なデータ処理機能を橋渡しします。組み込みの[ルールエンジン](./rules.md)コンポーネントを利用することで、両プラットフォーム間のデータフローと処理を簡素化し、複雑なコーディングを不要にします。

以下の図は自動車IoTにおけるEMQXとConfluentのデータ統合の典型的なアーキテクチャを示しています。

![Confluent Architecture](./assets/confluent-architecture.png)

Confluentへのデータの入出力はConfluent Sink（Confluentへのメッセージ送信）とConfluent Source（Confluentからのメッセージ受信）を介して行われます。Confluent Sinkを作成した場合、そのワークフローは以下の通りです：

1. **メッセージのパブリッシュと受信**：車両に接続されたIoTデバイスはMQTTプロトコルでEMQXに正常に接続し、定期的に状態データを含むメッセージをパブリッシュします。EMQXがこれらのメッセージを受信すると、ルールエンジン内でマッチング処理を開始します。
2. **メッセージデータの処理**：これらのMQTTメッセージは、組み込みのルールエンジンとメッセージサーバーの連携によりトピックマッチングルールに従って処理されます。メッセージがルールエンジンを通過すると、事前定義された処理ルールが評価されます。ペイロード変換を指定するルールがあれば、データ形式の変換、特定情報のフィルタリング、追加コンテキストによるペイロードの強化などが適用されます。
3. **Confluentへの橋渡し**：ルールエンジンで定義されたルールがトリガーとなり、メッセージをConfluentに転送するアクションが実行されます。Confluent Sink機能を利用し、MQTTトピックはConfluentの事前定義されたKafkaトピックにマッピングされ、処理済みのメッセージとデータはこれらのトピックに書き込まれます。

車両データがConfluentに入力されると、以下のように柔軟にデータを活用できます：

- サービスはConfluentと直接統合し、特定トピックのリアルタイムデータストリームを消費してカスタマイズされたビジネス処理を行えます。
- Kafka Streamsを利用したストリーム処理や、車両状態のメモリ内集約・相関によるリアルタイム監視が可能です。
- ConfluentのStream Designerコンポーネントを使用し、MySQLやElasticSearchなど外部システムへのデータ出力用コネクターを選択して保存できます。

## 機能と利点

Confluentとのデータ統合は以下の機能と利点をビジネスにもたらします：

- **大規模メッセージ送信の信頼性**：EMQXとConfluent Cloudは共に高信頼のクラスター機構を用い、安定かつ信頼性の高いメッセージ送信チャネルを確立し、大規模IoTデバイスからのメッセージロスをゼロにします。両者はノード追加による水平スケールが可能で、リソースを動的に調整して突発的な大規模メッセージにも対応し、メッセージ送信の可用性を確保します。
- **強力なデータ処理能力**：EMQXのローカルルールエンジンとConfluent Cloudは、デバイスからアプリケーションまで異なる段階で信頼性の高いストリーミングデータ処理を提供します。リアルタイムのデータフィルタリング、形式変換、集約分析などをシナリオに応じて実施し、より複雑なIoTメッセージ処理ワークフローを実現し、データ分析アプリケーションのニーズに応えます。
- **強力な統合機能**：Confluent Cloudが提供する多様なコネクターを通じて、EMQXは他のデータベース、データウェアハウス、データストリーム処理システムなどと容易に統合でき、柔軟なデータ分析アプリケーションのための完全なIoTデータワークフローを構築します。
- **高スループット処理能力**：同期・非同期の両書き込みモードをサポートし、リアルタイム優先とパフォーマンス優先のデータ書き込み戦略を使い分け、シナリオに応じてレイテンシとスループットのバランスを柔軟に調整可能です。
- **効果的なトピックマッピング**：ブリッジ設定を通じて多数のIoTビジネストピックをKafkaトピックにマッピング可能です。EMQXはMQTTユーザープロパティをKafkaヘッダーにマッピングでき、1対1、1対多、多対多の柔軟なトピックマッピング方式を採用し、MQTTトピックフィルター（ワイルドカード）もサポートします。

これらの機能は統合能力と柔軟性を高め、効果的かつ堅牢なIoTプラットフォームアーキテクチャの構築を支援します。増大するIoTデータは安定したネットワーク接続で送信され、さらに効果的に保存・管理されます。

## はじめる前に

本節ではEMQXダッシュボードでConfluentデータ統合を設定するための準備作業を説明します。

### 前提条件

- [ルールエンジン](./rules.md)の理解
- [Sink](./data-bridges.md)の理解

### Confluent Cloudの設定

Confluentデータ統合を作成する前に、Confluent CloudコンソールでConfluentクラスターを作成し、Confluent Cloud CLIを用いてトピックとAPIキーを作成する必要があります。

#### クラスターの作成

1. Confluent Cloudコンソールにログインし、クラスターを作成します。例としてStandardクラスターを選択し、**Begin configuration**をクリックします。

![EMQX Confluent Create Cluster](./assets/confluent_create_cluster_1.2d537cc0.png)

2. リージョン/ゾーンを選択します。デプロイメントリージョンがConfluent Cloudのリージョンと一致していることを確認し、**Continue**をクリックします。

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

既存のAPIキーを使う場合は、以下のコマンドでCLIに追加します。

```bash
confluent api-key store --resource <kafka_cluster_id>
Key: <API_KEY>
Secret: <API_SECRET>
```

APIキーとシークレットがない場合は、以下のコマンドで作成可能です。

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

トピック一覧は以下で確認可能です。

```bash
confluent kafka topic list
```

##### トピックへのメッセージ送信（Producer）

以下のコマンドでプロデューサーを起動します。起動後、メッセージを入力してEnterを押すとトピックに送信されます。

```bash
confluent kafka topic produce testtopic-in
```

##### トピックからのメッセージ受信（Consumer）

以下のコマンドでコンシューマーを起動し、トピック内の全メッセージを出力します。

```bash
confluent kafka topic consume -b testtopic-in
```

## コネクターの作成

Confluent Sinkアクションを追加する前に、EMQXとConfluent Cloud間の接続を確立するためにConfluentプロデューサーコネクターを作成する必要があります。

1. EMQXダッシュボードで**Integration** -> **Connectors**をクリックします。

2. ページ右上の**Create**をクリックし、コネクター選択ページで**Confluent Producer**を選択して**Next**をクリックします。

3. `my-confluent`のような名前と説明を入力します。この名前はConfluent Sinkとコネクターを関連付けるために使用され、クラスター内で一意である必要があります。

4. Confluent Cloudへの接続に必要なパラメーターを設定します：
   - **Bootstrap Hosts**：Confluent Cloudクラスター設定ページの**Endpoints**セクションからエンドポイント情報を入力します。
   
   - **Authentication**：Confluent Cloudクラスターで必要な認証方式を選択します：
     - **Basic auth**：Confluent Cloudで作成したAPI KeyとAPI Secretに対応する**Username**と**Password**を入力します。
     
     - **OAuth**：Confluent CloudのOAuth/OIDC設定に従い、トークンエンドポイント、クライアントID、クライアントシークレットなどのOAuthパラメーターを設定します。
     
       OAuth設定はKafkaコネクターと同様です。詳細は[認証方式](./data-bridge-kafka.md#authentication-method)を参照してください。

   - **Request Timeout**：EMQXがConfluentからの応答を待つ最大時間を秒単位で指定します。デフォルトは`30`秒です。タイムアウトを超えるとEMQXは接続を古いものとみなし再接続します。この値が小さすぎると、Confluentはパブリッシュ要求を受け入れても応答を遅延させ、EMQXが再接続後に同じバッチを再送し、重複メッセージや過剰な下流データ量を引き起こす可能性があります。
   - その他のオプションはデフォルトのままか、ビジネスニーズに応じて設定してください。
   
5. **Create**ボタンをクリックしてコネクターの作成を完了します。

作成後、コネクターは自動的にConfluent Cloudに接続します。次に、このコネクターを基にルールを作成し、コネクターで設定したConfluentクラスターにデータを転送します。

## Confluent Sinkを使ったルールの作成

本節では、MQTTトピック`t/#`からのメッセージを処理し、処理結果をConfluentの`testtopic-in`トピックに送信するルールをEMQXで作成する方法を示します。

1. EMQXダッシュボードに入り、**Integration** -> **Rules**をクリックします。

2. 右上の**Create**をクリックします。

3. ルールID（例：`my_rule`）を入力します。

4. MQTTメッセージをトピック`t/#`からConfluentに転送したい場合、**SQL Editor**に以下の文を入力します。

   注意：独自のSQL構文を指定する場合、`SELECT`部分にSinkが必要とする全フィールドを含めてください。

   ```sql
   SELECT
     *
   FROM
     "t/#"
   ```

   注意：初心者の場合は**SQL Example**と**Enable Test**をクリックしてSQLルールの学習とテストが可能です。

5. + **Add Action**ボタンをクリックし、ルールでトリガーされるアクションを定義します。**Type of Action**ドロップダウンから`Confluent Producer`を選択し、**Action**はデフォルトの`Create Action`のままか、既存のConfluent Producerアクションを選択します。この例では新規ルールを作成しアクションを追加します。

6. Sinkの名前と説明を対応するテキストボックスに入力します。

7. **Connector**ドロップダウンから先ほど作成した`my-confluent`コネクターを選択します。ドロップダウン横のボタンをクリックするとポップアップで新規コネクターを素早く作成可能です。設定パラメーターは[コネクターの作成](#コネクターの作成)を参照してください。

8. Sinkのデータ送信方法を設定します：

   - **Kafka Topic**：`testtopic-in`を入力します。EMQX v5.7.2以降、このフィールドは動的トピック設定もサポートします。詳細は[Kafka動的トピックの設定](./data-bridge-kafka.md#configure-kafka-dynamic-topics)を参照してください。
   - **Kafka Headers**：Kafkaメッセージに関連するメタデータやコンテキスト情報を入力します（任意）。プレースホルダーの値はオブジェクトでなければなりません。ヘッダー値のエンコードタイプは**Kafka Header Value Encod Type**ドロップダウンから選択可能です。**Add**をクリックしてキー・バリューのペアを追加できます。
   - **Message Key**：Kafkaメッセージのキーです。純粋な文字列か、プレースホルダー（${var}）を含む文字列を入力します。
   - **Message Value**：Kafkaメッセージの値です。純粋な文字列か、プレースホルダー（${var}）を含む文字列を入力します。
   - **Partition Strategy**：プロデューサーがKafkaのパーティションにメッセージを分配する方法を選択します。
   - **Compression**：Kafkaメッセージ内のレコードを圧縮/解凍するための圧縮アルゴリズムを指定します。

9. **フォールバックアクション（任意）**：メッセージ配信失敗時の信頼性向上のため、1つ以上のフォールバックアクションを定義できます。プライマリSinkがメッセージ処理に失敗した場合にこれらのアクションがトリガーされます。詳細は[フォールバックアクション](./data-bridges.md#fallback-actions)を参照してください。

10. **詳細設定（任意）**：[詳細設定](#advanced-configuration)を参照してください。

11. **Create**ボタンをクリックしてSinkの作成を完了します。作成後、ページは**Create Rule**に戻り、新しいSinkがルールアクションに追加されます。

12. **Create**ボタンをクリックしてルール全体の作成を完了します。

これでルールが正常に作成され、**Integration** -> **Rules**ページで新規ルールを確認でき、**Actions(Sink)**タブに新規のConfluent Producer Sinkも表示されます。

また、**Integration** -> **Flow Designer**をクリックするとトポロジーを確認できます。トポロジーでは、トピック`t/#`のメッセージがルール`my_rule`で解析され、Confluentに送信・保存される様子を直感的に把握できます。

## Confluent Producerルールのテスト

Confluent Producerルールが期待通り動作するかテストするため、[MQTTX](https://mqttx.app/en)を使ってクライアントがEMQXにMQTTメッセージをパブリッシュする動作をシミュレートできます。

1. MQTTXを使ってトピック`t/1`にメッセージを送信します：

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "Hello Confluent" }'
   ```

2. **Actions(Sink)**ページでSink名をクリックし統計情報を表示します。Sinkの稼働状況を確認し、新規受信メッセージ数と送信メッセージ数がそれぞれ1件増えていることを確認します。

3. 以下のConfluentコマンドで`testtopic-in`トピックにメッセージが書き込まれているか確認します：

   ```bash
   confluent kafka topic consume -b testtopic-in
   ```

## 詳細設定

本節では、コネクターやSink/Sourceのパフォーマンスを最適化し、特定シナリオに応じたカスタマイズ操作を可能にする高度な設定オプションを説明します。該当オブジェクト作成時に**Advanced Settings**を展開し、ビジネスニーズに応じて以下の設定を行えます。

### コネクター設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Allow Auto Topic Creation         | （Producerのみ）有効にすると、クライアントがメタデータ取得要求時にKafkaトピックが存在しなければ自動作成を許可します。 | `Disabled`         |
| Connect Timeout                   | TCP接続確立の最大待機時間（認証時間含む）                     | `5`秒              |
| Start Timeout                     | コネクターが自動起動したリソースの正常状態到達を待つ最大秒数。Sinkが接続先リソース（例：Confluentクラスター）の完全稼働を確認してから処理を進めるための設定。 | `5`秒              |
| Health Check Interval             | コネクターの稼働状態チェック間隔                              | `15`秒             |
| Min Metadata Refresh Interval     | Kafkaブローカー・トピックのメタデータ更新最小間隔。短すぎるとKafkaサーバー負荷増加の恐れあり。 | `3`秒              |
| Metadata Request Timeout          | Kafkaへのメタデータ要求の最大待機時間                         | `5`秒              |
| Socket Send / Receive Buffer Size | ソケットバッファサイズの管理。ネットワーク伝送性能の最適化に寄与。 | `1`MB              |
| No Delay                          | TCPソケット送信を即時に行うか遅延させるかの設定。オンで即時送信。オフの場合、送信内容が少ないと約40ミリ秒の遅延が発生。 | `Enabled`          |
| TCP Keepalive                     | Kafkaブリッジ接続のTCPキープアライブ設定。接続の長時間アイドルによる切断防止。`Idle, Interval, Probes`の3値カンマ区切りで指定。例：`240,30,5`は240秒アイドル後に30秒間隔で5回プローブ送信し応答なければ切断。 | `none`             |

### Confluent Producer Sink設定

| 項目                             | 説明                                                         | 推奨値             |
| -------------------------------- | ------------------------------------------------------------ | ------------------ |
| Health Check Interval            | Sinkの稼働状態チェック間隔                                   | `15`秒             |
| Max Batch Bytes                  | Kafkaバッチ内で収集するメッセージの最大サイズ（バイト）。Kafkaブローカーのデフォルトは1MBだが、EMQXはKafkaメッセージのエンコードオーバーヘッドを考慮しやや小さめに設定。単一メッセージが超過する場合は別バッチで送信。 | `896`KB            |
| Required Acks                    | Kafkaパーティションリーダーがフォロワーから待つ必要のあるアックの種類：<br />`all_isr`：全てのインシンクレプリカからのアックを要求<br />`leader_only`：リーダーのみからのアックを要求<br />`none`：Kafkaからのアック不要 | `all_isr`          |
| Partition Count Refresh Interval | Kafkaプロデューサーがパーティション数増加を検知する間隔。増加時は`partition_strategy`に基づき新パーティションにメッセージを分配。 | `60`秒             |
| Max Inflight                     | Kafkaプロデューサー（パーティション毎）がアック受信前に送信可能な最大バッチ数。大きいほどスループット向上。ただし1より大きいとメッセージ順序が乱れるリスクあり。未アックメッセージ数を制御し負荷バランスを調整。 | `10`秒             |
| Query Mode (Producer)            | 非同期または同期クエリモードを選択し、要件に応じてメッセージ送信を最適化。非同期はKafka書き込みがMQTTパブリッシュをブロックしないが、クライアントがKafka到着前にメッセージを受け取る可能性あり。 | `Async`            |
| Synchronous Query Timeout        | 同期モード時の最大待機時間。メッセージ送信完了をタイムリーに保証し長時間待機を防止。`Sync`モード時のみ適用。 | `5`秒              |
| Buffer Mode                      | メッセージ送信前のバッファリング方式。メモリバッファは高速だがノード再起動で消失。ディスクバッファは永続化。ハイブリッドは一定サイズまでメモリ、その後ディスクにオフロード。 | `memory`           |
| Per-partition Buffer Limit       | Kafkaパーティション毎の最大バッファサイズ（バイト）。上限到達時は古いメッセージを破棄しバッファ空間を確保。メモリ使用量と性能のバランス調整に有効。 | `2`GB              |
| Segment File Bytes               | バッファモードが`disk`または`hybrid`時に適用。メッセージ保存用の分割ファイルサイズを制御し、ディスクストレージの最適化に寄与。 | `100`MB            |
| Memory Overload Protection       | バッファモードが`memory`時に適用。メモリ圧迫時に古いメッセージを自動破棄し、システムの安定性を確保。Linuxのみ有効。 | Disabled           |
| Max Batch Age                    | プロデューサーバッファ内でメッセージが送信されずに保持可能な最大期間。期限切れのバッチは破棄され、破棄されたメッセージは`dropped.expired`メトリクスにカウント。デフォルトは`infinity`で期限切れなし。バッファオーバーフロー時は期限に関係なく破棄される可能性あり。 | `infinity`         |
| Max Retries                      | Confluentがリトライ可能なエラーを返した場合の最大再試行回数。初回試行と全リトライ失敗時はバッチ破棄され、破棄メッセージは`failed`メトリクスにカウント。明示的なConfluentエラー応答のみリトライ回数に加算。接続喪失による再送は加算されず、`max_batch_age`で制限。デフォルトは無制限。 | `infinity`         |
| Reconnect Delay                  | 接続断後にConfluentへの再接続を試みるまでの待機時間。切断中もメッセージはバッファに蓄積され、バッファ制限や`max_batch_age`の影響を受ける。デフォルトは`2`秒。 | `2`秒              |
| Max Linger Time                  | パーティション毎のプロデューサーがより大きなバッチを形成するために待機する最大時間。全バッファモードに適用。デフォルト`0`は待機なしで低レイテンシ最適化。多少の遅延許容でリクエスト数削減可能。ディスクバッファ時はバッファ書き込み前に待機し、5ms以上推奨。 | `0`ミリ秒          |
| Max Linger Bytes                 | パーティション毎のプロデューサーが待機を終了しバッチ送信する最大バイト数。 | `10`MB             |

### <!-- Confluent Consumer Source Configuration -->

## 追加情報

EMQXはConfluent/Kafkaとのデータ統合に関する豊富な学習リソースを提供しています。以下のリンクもご参照ください：

**ブログ：**

- [MQTTとKafkaによるコネクテッドビークルのストリーミングデータパイプライン構築](https://www.emqx.com/en/blog/building-connected-vehicle-streaming-data-pipelines-with-mqtt-and-kafka)
- [MQTTとKafka｜IoTメッセージングとストリームデータ統合の実践](https://www.emqx.com/en/blog/mqtt-and-kafka)
- [MQTTパフォーマンスベンチマークテスト：EMQX-Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)

**ベンチマークレポート：**

- [EMQX Enterpriseパフォーマンスベンチマークテスト：Kafka統合](https://www.emqx.com/en/resources/emqx-enterprise-performance-benchmark-testing-kafka-integration)
