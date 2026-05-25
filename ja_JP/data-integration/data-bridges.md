# データ統合

EMQXは、MQTTプロトコルを通じてIoTデバイスを接続し、リアルタイムでメッセージを送受信するMQTTメッセージングプラットフォームです。これを基盤として、EMQXのデータ統合は外部データシステムとの接続を導入し、デバイスと他の業務システムとのシームレスな統合を可能にします。

データ統合では、SinkおよびSourceコンポーネントを使用して外部データシステムと接続します。SinkはMySQL、Kafka、HTTPサービスなどの外部データシステムへメッセージを送信するために使用され、SourceはMQTT、Kafka、GCP PubSubなどの外部データシステムからメッセージを受信するために使用されます。

この仕組みにより、EMQXは単なるIoTデバイス間のメッセージ送信を超えて、デバイスが生成するデータを業務全体のエコシステムに有機的に統合します。これにより、IoTアプリケーションの適用シナリオが拡大し、デバイスと業務システム間のやり取りがより豊かで多様になります。

::: tip 注意

- EMQX v5.4.0以降、従来のデータブリッジはデータフローの方向に応じて分離され、それぞれSinkとSourceに名称変更されました。

- 現時点でEMQXがSourceとしてサポートする外部データシステムは以下の通りです：

  - MQTTサービス
  - Kafka
  - GCP PubSub

:::

本ページでは、SinkとSourceの動作原理、対応する外部データシステム、主な機能、管理方法について包括的に解説します。

## 動作原理

EMQXのデータ統合は標準機能として提供されています。MQTTメッセージングプラットフォームとして、EMQXはMQTTプロトコル経由でIoTデバイスからデータを受信します。内蔵のルールエンジンの助けを借りて、受信したデータはルールエンジンに設定されたルールに基づいて処理されます。ルールは処理済みデータを設定されたSink/Source経由で外部データシステムに転送するアクションをトリガーします。Dashboard上の[ルール](./rule-get-started.md)や[Flowデザイナー](../flow-designer/introduction.md)を使って、コーディング不要で簡単にルールの作成、アクションの紐付け、Sink/Sourceの作成が可能です。

### 内蔵ルールエンジン

さまざまなIoTデバイスやシステムからのデータソースは多種多様なデータ型やフォーマットを持ちます。EMQXはSQLルールに基づく強力な内蔵ルールエンジンを備えており、これはデータ処理と配信の中核コンポーネントです。ルールエンジンは条件判定、文字列操作、データ型変換、圧縮／解凍など幅広い機能を持ち、複雑なデータを柔軟に処理できます。

クライアントが特定のイベントをトリガーしたり、メッセージがEMQXに到達した際に、ルールエンジンは事前定義されたルールに従ってリアルタイムにデータを処理します。データ抽出、フィルタリング、付加情報付与、フォーマット変換などを行い、処理済みデータを指定されたSinkに転送します。

ルールエンジンの詳細な動作は[ルールエンジン](./rules.md)章をご覧ください。

### Sink

Sinkはルールの[アクション](./rules.md)として追加されるデータ出力コンポーネントです。デバイスがイベントをトリガーしたりメッセージがEMQXに到達すると、システムは対応するルールをマッチングして実行し、データをフィルタリング・処理します。ルールエンジンで処理されたデータは指定されたSinkに転送されます。Sinkでは`${var}`や`${.var}`構文を用いてデータから変数を抽出し、動的にSQL文やデータテンプレートを生成するなどの処理を設定できます。その後、対応する[コネクター](./connector.md)を介して外部データシステムへデータを送信し、メッセージ保存、データ更新、イベント通知などの操作を実現します。

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

- `${var}`：ルールの出力結果から変数を抽出する構文です。例：`${topic}`。ネストした変数を抽出する場合はドット`.`を使います。例：`${payload.temp}`。抽出対象の変数が出力結果に含まれない場合は文字列`undefined`が返されます。
- `${.}`, `${.var}`：`${.}`はルールの全出力結果を含むJSON文字列を抽出し、`${.var}`は`${var}`と同じ意味です。

### Source

Sourceはデータ入力コンポーネントであり、ルールの[データソース](./rule-sql-events-and-fields.md)として機能し、ルールSQLで選択されます。

SourceはMQTTやKafkaなどの外部データシステムからメッセージをサブスクライブまたはコンシュームします。コネクター経由で新しいメッセージが到着すると、ルールエンジンは対応するルールをマッチングして実行し、データをフィルタリング・処理します。処理済みデータは指定されたEMQXトピックにパブリッシュされ、クラウドコマンド配信などの操作が可能になります。

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

## 対応インテグレーション

EMQXは以下の種類のデータシステムとのデータ統合をサポートしています：

**デフォルト**

- [MQTT](./data-bridge-mqtt.md)
- [Webhook](./webhook.md)/[HTTPServer](./data-bridge-webhook.md)

**クラウド**

- [Amazon Kinesis](./data-bridge-kinesis.md)
- [Azure EventHub](./data-bridge-azure-event-hub.md)
- [GCP PubSub](./data-bridge-gcp-pubsub.md)

**TSDB**

- [Apache IoTDB](./data-bridge-iotdb.md)
- [InfluxDB](./data-bridge-influxdb.md)
- [OpenTSDB](./data-bridge-opents.md)
- [TimescaleDB](./data-bridge-timescale.md)
- [Datalayers](./data-bridge-datalayers.md)

**SQL**

- [Cassandra](./data-bridge-cassa.md)
- [Microsoft SQL Server](./data-bridge-sqlserver.md)
- [MySQL](./data-bridge-mysql.md)
- [Oracle](./data-bridge-oracle.md)
- [PostgreSQL](./data-bridge-pgsql.md)
- [Lindorm](./lindorm)
- [Doris](./apache-doris)

**NoSQL**

- [ClickHouse](./data-bridge-clickhouse.md)
- [Couchbase](./data-bridge-couchbase.md)
- [DynamoDB](./data-bridge-dynamo.md)
- [Greptime](./data-bridge-greptimedb.md)
- [MongoDB](./data-bridge-mongodb.md)
- [Redis](./data-bridge-redis.md)
- [TDengine](./data-bridge-tdengine.md)
- [Elasticsearch](./elasticsearch.md)

**メッセージキュー**

- [Apache Kafka/Confluent](./data-bridge-kafka.md)
- [HStreamDB](./data-bridge-hstreamdb.md)
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

## Sinkの特徴

Sinkは以下の機能により利便性を高め、データ統合のパフォーマンスと信頼性を向上させます。すべてのSinkがこれらの機能を完全に実装しているわけではありません。詳細な対応状況は各Sinkのドキュメントをご参照ください。

### 非同期リクエストモード

非同期リクエストモードは、メッセージのパブリッシュ・サブスクライブ処理がSinkの実行速度に影響されないよう設計されています。ただし、非同期リクエストモードを有効にすると、サブスクライバーがメッセージを受信していても外部データシステムへの書き込みがまだ完了していない場合があります。

EMQXはデータ処理効率を高めるために非同期リクエストモードをデフォルトで有効にしています。メッセージの配信タイミングに厳密な要件がある場合は、非同期リクエストモードを無効にしてください。

`max_inflight`パラメータも非同期リクエストにおけるメッセージ順序に影響します。一部のSinkはこのパラメータを持ち、非同期モード時に同一MQTTクライアントからのメッセージを順序通りに処理する必要がある場合は、この値を1に設定する必要があります。

### バッチモード

バッチモードは複数のデータエントリをまとめて外部データ統合システムに書き込む機能です。バッチモードを有効にすると、EMQXは各リクエストのデータ（単一エントリ）を一時的に蓄積し、一定時間経過または一定数のデータが蓄積された後にまとめてターゲットデータシステムに書き込みます（いずれも設定可能）。

**利点：**

- 書き込み効率の向上：単一メッセージ書き込みに比べ、データベースシステムがメッセージをキャッシュや事前処理できるため、書き込み効率が向上します。
- ネットワークレイテンシの削減：バッチ書き込みによりネットワーク送信回数が減り、レイテンシが低減します。

**課題：**

データ書き込みの遅延：設定された時間またはエントリ数に達するまで書き込みが遅延します。これらの設定はパラメータで調整可能です。

### バッファキュー

バッファキューはSinkのフォールトトレランスを一定程度提供し、データ安全性向上のために有効化が推奨されます。

各リソース接続（MQTT接続ではありません）にはバッファキュー長（容量サイズ）があり、これを超えるデータはFIFO原則に従い破棄されます。

#### バッファファイルの場所

Kafka Sinkの場合、ディスクキャッシュファイルは`data/kafka`に保存されます。その他のSinkは`data/bufs`に保存されます。

実運用では`data`ディレクトリを高性能ディスクにマウントし、スループットを向上させることが可能です。

### プリペアドステートメント

MySQLやPostgreSQLなどのSQLデータベースでは、SQLテンプレートがフィールド変数を明示的に指定せずに事前処理されて実行されます。

SQLを直接実行する場合、トピックとペイロードは文字列型、QoSは整数型としてシングルクォートで明示的に指定する必要があります：

```sql
INSERT INTO msg(topic, qos, payload) VALUES('${topic}', ${qos}, '${payload}');
```

しかし、プリペアドステートメントに対応したSinkでは、SQLテンプレートは必ずクォートなしのプリペアドステートメント形式を使用します：

```sql
INSERT INTO msg(topic, qos, payload) VALUES(${topic}, ${qos}, ${payload});
```

プリペアドステートメント技術により、フィールド型の自動推論に加え、SQLインジェクション防止によるセキュリティ強化が実現されます。

### フォールバックアクション

EMQX 5.9.0以降、任意のアクションに対してフォールバックアクションのセットを定義可能です。プライマリアクションがメッセージ処理に失敗した場合にフォールバックアクションがトリガーされます。これにより、メッセージを別のSinkや再パブリッシュアクションなどの二次ターゲットにリダイレクトでき、データの信頼性と可観測性が向上します。

フォールバックアクションの用途例：

- 失敗したメッセージをバックアップデータシステム（別のSinkなど）に転送
- 失敗したメッセージを監視トピックに再パブリッシュし、トラブルシューティングやアラートに活用
- プライマリアクションの一時的な問題によるデータ損失を最小化

#### 主な特徴

- フォールバックアクションはプライマリアクションがメッセージ処理に失敗した場合のみトリガーされます。失敗には配信エラー、バッファオーバーフロー、リクエストTTL切れなどが含まれます。
- フォールバックアクションは自身の設定に関わらず常に非同期リクエストモードで動作します。
- 定義されたすべてのフォールバックアクションが同時にトリガーされます。EMQXは順次試行や最初の成功で停止しません。
- フォールバックアクションは通常のアクションと同じバッファリング機構を共有し、リクエストTTLまたはバッファオーバーフローまでメッセージをリトライします。
- フォールバックアクションはさらにフォールバックアクションをトリガーしません。フォールバックアクション自身が失敗しても、その設定されたフォールバックアクションはトリガーされません。
- フォールバックアクションによるメッセージ処理は、プライマリアクションや元のルールのメトリクスに影響を与えません。

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

- HTTPアクション`my_http`が失敗した場合、メッセージは
  - MQTTアクション`fallback`に転送され、
  - トピック`fallback/republish/topic`に再パブリッシュされます。
- `fallback`が失敗しても、`fallback`に定義されたフォールバックアクション`another_fallback`はトリガーされません。フォールバックアクションは再帰的な連鎖をサポートしません。
- ただし、`fallback`が別ルールのプライマリアクションとしてトリガーされ失敗した場合は、そのフォールバック`another_fallback`が適用されます。

## Sinkの状態と統計情報

Dashboard上でSinkの稼働状況や統計情報を確認し、正常に動作しているか把握できます。

### 稼働状態

Sinkは以下の状態を持ちます：

- `connecting`：ヘルスプローブが行われる前の初期状態で、外部データシステムへの接続を試みている状態。
- `connected`：Sinkが正常に接続され、稼働中の状態。ヘルスプローブが失敗すると、失敗の程度に応じて`connecting`または`disconnected`に遷移する可能性があります。
- `disconnected`：ヘルスプローブに失敗し、非正常状態。設定により自動的に再接続を試みる場合があります。
- `stopped`：手動で無効化された状態。
- `inconsistent`：クラスターのノード間でSinkの状態に不整合がある状態。

### 稼働統計

EMQXはデータ統合の稼働統計を以下のカテゴリで提供します：

- Matched（カウンター）
- Sent Successfully（カウンター）
- Sent Failed（カウンター）
- Dropped（カウンター）
- Late Reply（カウンター）
- Inflight（ゲージ）
- Queuing（ゲージ）

<img src="./assets/data-bridge-metrics.png" alt="データブリッジのメトリクス"  />

#### Matched

`matched`はSinkにルーティングされたリクエスト／メッセージの総数をカウントします。状態に関わらずカウントされます。各メッセージは他のメトリクスで最終的にカウントされるため、`matched = success + failed + inflight + queuing + late_reply + dropped`となります。

#### Sent Successfully

`success`は外部データシステムに正常に受信されたメッセージ数をカウントします。`retried.success`は`success`のサブカウントで、少なくとも一度配信リトライされたメッセージ数を追跡します。したがって、`retried.success <= success`です。

#### Sent Failed

`failed`は外部データシステムへの受信に失敗したメッセージ数をカウントします。`retried.failed`は`failed`のサブカウントで、少なくとも一度配信リトライされたメッセージ数を追跡します。したがって、`retried.failed <= failed`です。

#### Dropped

`dropped`は配信試行なしに破棄されたメッセージ数をカウントします。複数の具体的なカテゴリに分かれ、それぞれ破棄理由を示します。計算式は`dropped = dropped.expired + dropped.queue_full + dropped.resource_stopped + dropped.resource_not_found`です。

- `expired`：キューイング中にメッセージのTTLが切れた。
- `queue_full`：キューの最大サイズに達し、メモリオーバーフロー防止のため破棄された。
- `resource_stopped`：Sinkが停止中に配信を試みたメッセージ。
- `resource_not_found`：Sinkが見つからない状態で配信を試みたメッセージ。まれに発生し、Sink削除時の競合状態が原因。

#### Late Reply

`late_reply`はメッセージ送信を試みたが、基盤ドライバーからの応答がメッセージTTL切れ後に届いた場合にインクリメントされます。

::: tip
`late_reply`はメッセージが成功したか失敗したかを示すものではありません。不明な状態です。外部データシステムへの挿入に成功した可能性もあれば、失敗した可能性もありますし、接続確立時にタイムアウトした可能性もあります。
:::

#### Inflight

`inflight`はバッファリング層に存在し、現在外部データシステムからの応答待ちのメッセージ数を示すゲージです。

#### Queuing

`queuing`はバッファリング層で受信済みだが、まだ外部データシステムに送信されていないメッセージ数を示すゲージです。
