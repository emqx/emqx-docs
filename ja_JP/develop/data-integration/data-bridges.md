# データ統合

EMQXは、MQTTプロトコルを通じてIoTデバイスを接続し、リアルタイムでメッセージを送信するMQTTメッセージングプラットフォームです。これを基盤として、EMQXのデータ統合は外部データシステムとの接続を導入し、デバイスと他の業務システムとのシームレスな統合を可能にします。

データ統合はSinkおよびSourceコンポーネントを用いて外部データシステムと接続します。SinkはMySQL、Kafka、HTTPサービスなどの外部データシステムへメッセージを送信するために使用され、SourceはMQTT、Kafka、GCP PubSubなどの外部データシステムからメッセージを受信するために使用されます。

この仕組みにより、EMQXは単なるIoTデバイス間のメッセージ送信を超え、デバイスが生成するデータを業務全体のエコシステムに有機的に統合できます。これにより、IoTアプリケーションの適用シナリオが広がり、デバイスと業務システム間の相互作用が豊かで多様になります。

::: tip 注意

- EMQX v5.4.0以降、従来のデータブリッジはデータフローの方向に応じて分割され、SinkおよびSourceに名称変更されました。

- 現時点でEMQXがSourceとしてサポートする外部データシステムは以下の通りです：

  - MQTTサービス
  - Kafka
  - GCP PubSub

:::

本ページでは、SinkおよびSourceの動作原理、対応する外部データシステム、主な機能、管理方法について包括的に解説します。

## 動作原理

EMQXのデータ統合は標準機能として提供されています。MQTTメッセージングプラットフォームとして、EMQXはMQTTプロトコル経由でIoTデバイスからデータを受信します。組み込みのルールエンジンの助けを借りて、受信したデータはルールエンジンに設定されたルールによって処理されます。ルールは処理済みデータを設定されたSink/Sourceを通じて外部データシステムへ転送するアクションをトリガーします。Dashboard上の[Rules](./rule-get-started.md)や[Flowデザイナー](../flow-designer/introduction.md)を使えば、コーディング不要で簡単にルール作成、アクションの紐付け、Sink/Sourceの作成が可能です。

### 組み込みルールエンジン

さまざまなIoTデバイスやシステムからのデータソースは多種多様なデータ型やフォーマットを持っています。EMQXはSQLルールに基づく強力な組み込みルールエンジンを備えており、これはデータ処理と配布の中核コンポーネントです。ルールエンジンは条件判定、文字列操作、データ型変換、圧縮／解凍機能など幅広い機能を持ち、複雑なデータを柔軟に処理できます。

クライアントが特定のイベントをトリガーしたり、メッセージがEMQXに到達した際、ルールエンジンは事前定義されたルールに従ってリアルタイムにデータを処理します。データ抽出、フィルタリング、付加情報の追加、フォーマット変換などを行い、処理済みデータを指定されたSinkへ転送します。

ルールエンジンの詳細な動作については[ルールエンジン](./rules.md)章をご参照ください。

### Sink

Sinkはルールの[action](./rules.md)に追加されるデータ出力コンポーネントです。デバイスがイベントをトリガーしたりメッセージがEMQXに到達すると、システムは該当ルールをマッチングして実行し、データをフィルタリング・処理します。ルールエンジンで処理されたデータは指定されたSinkへ転送されます。Sinkでは`${var}`や`${.var}`構文を用いてデータから変数を抽出し、動的にSQL文やデータテンプレートを生成するなどの処理を設定できます。その後、対応する[コネクター](./connector.md)を通じて外部データシステムへデータを送信し、メッセージの保存、データ更新、イベント通知などの操作を実現します。

```mermaid
graph LR
  A[クライアント] -->|メッセージパブリッシュ| B[ルール]
  A1[クライアント] --> |メッセージパブリッシュ| B

  subgraph ルールエンジン
    B -->  |アクション実行| C[Kafka Sink] --> D[Kafka コネクター]
  end

D -->|メッセージ保存| E[Kafka]
```

Sinkでサポートされる変数抽出構文は以下の通りです：

- `${var}`：ルールの出力結果から変数を抽出する構文です。例：`${topic}`。ネストした変数を抽出したい場合はピリオド`.`を使い、`${payload.temp}`のように指定します。抽出対象の変数が出力結果に含まれない場合は文字列`undefined`が返されます。
- `${.}`, `${.var}`：`${.}`はルールの全出力結果を含むJSON文字列を抽出し、`${.var}`は`${var}`と同じ意味です。

### Source

Sourceはデータ入力コンポーネントであり、ルールの[data source](./rule-sql-events-and-fields.md)として機能し、ルールSQLで選択されます。

SourceはMQTTやKafkaなどの外部データシステムからメッセージをサブスクライブまたはコンシュームします。コネクターを通じて新しいメッセージが到着すると、ルールエンジンが該当ルールをマッチングして実行し、データをフィルタリング・処理します。処理後のデータは指定されたEMQXトピックへパブリッシュされ、クラウドコマンド配信などの操作が可能になります。

```mermaid
graph LR
  A[Kafka] --> B[Kafka コネクター]
  subgraph ルールエンジン
    B --> C[Kafka Source]
    C -->  |トリガー| D[ルール]
    D -->  |アクション実行| D1[メッセージ再パブリッシュ]
  end

D1 -->|メッセージパブリッシュ| E[クライアント]
```

## 対応統合システム

EMQXは以下の種類のデータシステムとのデータ統合をサポートしています：

**デフォルト**

- [MQTT](./data-bridge-mqtt.md)
- [Webhook](./webhook.md)/[HTTPServer](./data-bridge-webhook.md)

**クラウド**

- [Amazon Kinesis](./data-bridge-kinesis.md)
- [Azure EventHub](./data-bridge-azure-event-hub.md)
- [Azure Event Grid](./azure-event-grid.md)
- [GCP PubSub](./data-bridge-gcp-pubsub.md)

**TSDB**

- [Apache IoTDB](./data-bridge-iotdb.md)
- [InfluxDB](./data-bridge-influxdb.md)
- [OpenTSDB](./data-bridge-opents.md)
- [TimescaleDB](./data-bridge-timescale.md)
- [Datalayers](./data-bridge-datalayers.md)
- [Timestream for InfluxDB](./timestream-for-influxdb.md)

**SQL**

- [Cassandra](./data-bridge-cassa.md)
- [Microsoft SQL Server](./data-bridge-sqlserver.md)
- [MySQL](./data-bridge-mysql.md)
- [Oracle](./data-bridge-oracle.md)
- [PostgreSQL](./data-bridge-pgsql.md)
- [Lindorm](./lindorm.md)
- [Doris](./apache-doris.md)
- [AlloyDB](./alloydb.md)
- [CockroachDB](./cockroachdb.md)
- [Redshift](./redshift.md)
- [QuasarDB](./quasardb.md)

**NoSQL**

- [ClickHouse](./data-bridge-clickhouse.md)
- [Couchbase](./data-bridge-couchbase.md)
- [DynamoDB](./data-bridge-dynamo.md)
- [Greptime](./data-bridge-greptimedb.md)
- [MongoDB](./data-bridge-mongodb.md)
- [Redis](./data-bridge-redis.md)
- [TDengine](./data-bridge-tdengine.md)
- [Elasticsearch](./elasticsearch.md)
- [EMQX Tables](./emqx-tables.md)

**メッセージキュー**

- [Apache Kafka/Confluent](./data-bridge-kafka.md)
- [Pulsar](./data-bridge-pulsar.md)
- [RabbitMQ](./data-bridge-rabbitmq.md)
- [RocketMQ](./data-bridge-rocketmq.md)

**その他**

- [SysKeeper](./syskeeper.md)
- [Amazon S3](./s3.md)
- [Amazon S3 Tables](./s3-tables.md)
- [Azure Blob Storage](./azure-blob-storage.md)
- [Snowflake](./snowflake.md)
- [Disk Log](./disk-log.md)
- [BigQuery](./bigquery.md)
- [Databricks](./databricks.md)

## Sinkの特徴

Sinkは以下の機能により使い勝手を向上させ、データ統合のパフォーマンスと信頼性をさらに強化します。すべてのSinkがこれらの機能を完全に実装しているわけではありません。具体的な対応状況は各Sinkのドキュメントをご参照ください。

### 非同期リクエストモード

非同期リクエストモードは、メッセージのパブリッシュ・サブスクライブ処理がSinkの実行速度に影響されないように設計されています。ただし、非同期リクエストモードを有効にすると、サブスクライバーがメッセージを受信しても、まだ外部データシステムに書き込まれていない場合があります。

データ処理効率を高めるため、EMQXはデフォルトで非同期リクエストモードを有効にしています。メッセージの配信タイミングに厳密な要件がある場合は、非同期リクエストモードを無効にしてください。

`max_inflight`パラメータも非同期リクエスト時のメッセージ順序に影響します。一部のSinkにはこのパラメータがあり、非同期モードで同一MQTTクライアントからのメッセージを順序通りに処理する必要がある場合は、この値を1に設定する必要があります。

### バッチモード

バッチモードは複数のデータエントリをまとめて外部データ統合システムに書き込むことを可能にします。バッチモードが有効な場合、EMQXは各リクエストのデータ（単一エントリ）を一時的に蓄積し、一定時間経過または一定数のデータが蓄積された後にまとめてターゲットデータシステムに書き込みます（いずれも設定可能）。

**利点：**

- 書き込み効率の向上：単一メッセージ書き込みと比較して、バッチモードはデータベースシステムがメッセージをキャッシュまたは事前処理できるため、書き込み効率が向上します。
- ネットワークレイテンシの削減：バッチ書き込みによりネットワーク送信回数が減り、レイテンシが低減します。

**課題：**

書き込み遅延：設定された時間やエントリ数に達するまでデータの書き込みが遅延します。これらの設定はパラメータで調整可能です。

### バッファキュー

バッファキューはSinkに一定のフォールトトレランスを提供し、データ安全性向上のため有効化が推奨されます。

各リソース接続（MQTT接続ではありません）にはバッファキュー長（容量サイズ）があり、これを超えたデータはFIFO原則に従い破棄されます。

#### バッファファイルの場所

Kafka Sinkの場合、ディスクキャッシュファイルは`data/kafka`にあります。その他のSinkは`data/bufs`にあります。

実際の運用では、`data`ディレクトリを高性能ディスクにマウントしてスループットを向上させることが可能です。

### プリペアドステートメント

MySQLやPostgreSQLなどのSQLデータベースでは、SQLテンプレートはフィールド変数を明示的に指定せずに事前処理実行されます。

SQLを直接実行する場合、topicとpayloadは文字列型、qosは整数型としてシングルクォートで明示的に指定する必要があります：

```sql
INSERT INTO msg(topic, qos, payload) VALUES('${topic}', ${qos}, '${payload}');
```

しかし、プリペアドステートメントをサポートするSinkでは、SQLテンプレートは必ずクォートなしで記述します：

```sql
INSERT INTO msg(topic, qos, payload) VALUES(${topic}, ${qos}, ${payload});
```

フィールド型の自動推論に加え、プリペアドステートメント技術はSQLインジェクションを防止し、セキュリティを強化します。

### フォールバックアクション

EMQX 5.9.0以降、任意のアクションに対してフォールバックアクションを定義できます。これは、プライマリアクションがメッセージ処理に失敗した際にトリガーされる一連の代替アクションです。この仕組みにより、メッセージを別のSinkや再パブリッシュアクションなどのセカンダリターゲットにリダイレクトでき、データの信頼性と可観測性を向上させます。

フォールバックアクションの用途例：

- 失敗したメッセージをバックアップデータシステム（例：別のSink）に転送
- 失敗したメッセージを監視用トピックに再パブリッシュし、トラブルシューティングやアラートに活用
- プライマリアクションの一時的な問題によるデータ損失を最小化

#### 主な特徴

- フォールバックアクションはプライマリアクションがメッセージ処理に失敗した場合のみトリガーされます。失敗には配信エラー、バッファオーバーフロー、リクエストTTL切れが含まれます。
- フォールバックアクションは自身の設定に関わらず常に非同期リクエストモードで動作します。
- 定義されたすべてのフォールバックアクションは同時にトリガーされます。EMQXは順番に試行したり、最初の成功で停止したりしません。
- フォールバックアクションは通常アクションと同じバッファリング機構を共有し、リクエストTTLまたはバッファオーバーフローまでメッセージをリトライします。
- フォールバックアクションは更なるフォールバックアクションをトリガーしません。フォールバックアクション自身が失敗しても、その設定されたフォールバックアクションはトリガーされません。
- フォールバックアクションによるメッセージ処理は、プライマリアクションやそれをトリガーした元のルールのメトリクスに影響しません。

#### フォールバックアクションの定義例

HTTPアクション`my_http`に対してフォールバックアクションを定義し、既存のMQTTアクション`fallback`を利用する例です。

```hcl
actions {
  http {
    my_http {
      fallback_actions = [
        {kind = reference, type = mqtt, name = fallback},
        {
          kind = republish,
          args = {
            topic = "fallback/republish/topic"
            qos = 1
            payload = "${payload}"
          }
        }
      ]
      # その他の設定は省略
    }
  }
  mqtt {
    fallback {
      fallback_actions = [
        {kind = reference, type = mqtt, name = another_fallback}
      ]
      # その他の設定は省略
    }
  }
}
```

この例では：

- HTTPアクション`my_http`が失敗した場合、メッセージは以下に処理されます：
  - MQTTアクション`fallback`に転送
  - トピック`fallback/republish/topic`に再パブリッシュ
- `fallback`が失敗しても、`fallback`内で定義されたフォールバックアクション`another_fallback`はトリガーされません。フォールバックアクションは再帰的な連鎖をサポートしません。
- ただし、`fallback`が別のルールでプライマリアクションとしてトリガーされ失敗した場合は、そのフォールバック`another_fallback`が適用されます。

## Sinkの状態と統計情報

Dashboard上でSinkの稼働状況や統計情報を確認し、正常に動作しているか把握できます。

### 稼働状態

Sinkは以下の状態を持ちます：

- `connecting`：ヘルスプローブが実施される前の初期状態で、ブリッジが外部データシステムへの接続を試みている段階。
- `connected`：Sinkが正常に接続され、正常稼働中。ヘルスプローブが失敗した場合は、失敗の程度に応じて`connecting`または`disconnected`に遷移することがあります。
- `disconnected`：ヘルスプローブに失敗し、非正常状態。設定により定期的に自動再接続を試みる場合があります。
- `stopped`：手動で無効化された状態。
- `inconsistent`：クラスター内のノード間でSinkの状態に不整合がある状態。

### 稼働統計

EMQXはデータ統合に関する以下のカテゴリの稼働統計を提供します：

- Matched（カウンター）
- Sent Successfully（カウンター）
- Sent Failed（カウンター）
- Dropped（カウンター）
- Late Reply（カウンター）
- Inflight（ゲージ）
- Queuing（ゲージ）

<img src="./assets/data-bridge-metrics.png" alt="データブリッジのメトリクス"  />

#### Matched

`matched`はSinkにルーティングされたリクエスト／メッセージの数をカウントします。状態に関係なくカウントされます。各メッセージは最終的に他のメトリクスで計上されるため、`matched = success + failed + inflight + queuing + late_reply + dropped`となります。

#### Sent Successfully

`success`は外部データシステムに正常に受信されたメッセージ数をカウントします。`retried.success`は`success`のサブカウントで、少なくとも1回配信リトライされたメッセージ数を追跡します。したがって、`retried.success <= success`です。

#### Sent Failed

`failed`は外部データシステムへの受信に失敗したメッセージ数をカウントします。`retried.failed`は`failed`のサブカウントで、少なくとも1回配信リトライされたメッセージ数を追跡します。したがって、`retried.failed <= failed`です。

#### Dropped

`dropped`は配信試行なしに破棄されたメッセージ数をカウントします。複数の具体的なカテゴリに分類され、それぞれ破棄理由を示します。計算式は`dropped = dropped.expired + dropped.queue_full + dropped.resource_stopped + dropped.resource_not_found`です。

- `expired`：メッセージのTTLがキューイング中に到達し、送信機会を失った。
- `queue_full`：キューの最大サイズに達し、メモリオーバーフロー防止のため破棄された。
- `resource_stopped`：Sinkが停止中に配信を試みたメッセージ。
- `resource_not_found`：Sinkが存在しない状態で配信を試みたメッセージ。稀に発生し、Sink削除時の競合状態が原因。

#### Late Reply

`late_reply`はメッセージ送信を試みたが、基盤ドライバーからの応答がメッセージTTL切れ後に受信された場合にインクリメントされます。

::: tip
`late_reply`はメッセージが成功したか失敗したかを示すものではありません。不明な状態であり、外部データシステムへの挿入に成功した可能性も失敗した可能性も、あるいは接続タイムアウトした可能性もあります。
:::

#### Inflight

`inflight`はバッファリング層に存在し、現在外部データシステムからの応答待ちのメッセージ数を示すゲージです。

#### Queuing

`queuing`はバッファリング層で受信済みだが、まだ外部データシステムへ送信されていないメッセージ数を示すゲージです。
