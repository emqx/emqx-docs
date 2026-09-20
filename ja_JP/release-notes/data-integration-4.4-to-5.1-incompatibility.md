# EMQX 5.1 と EMQX 4.4 間のデータ統合の非互換性

データ統合の概念は EMQX 5.1 で大幅にアップグレードされました。

- 以前の **ルール** -> **アクション** -> **リソース** のプロセスは **ルール** -> **データブリッジ** に変更されました。

  EMQX 4.4 ではアクション用の設定エンティティがありましたが、EMQX 5.1 では特定のルールにアクションを追加する際、まずデータブリッジを作成し、そのブリッジの SQL テンプレートを修正してルールの出力に適合させる必要があります。

  <img src="./assets/config-action-for-rule.png" alt="ルール用アクション設定" style="zoom:67%;" />

- **モジュール** -> **メッセージパブリッシュ** は **データブリッジ** に移動されました。

  EMQX 4.4 のメッセージパブリッシュモジュール:

  <img src="./assets/message-publish-modules.png" alt="メッセージパブリッシュモジュール" style="zoom:67%;" />

- EMQX 4.4 の [オフラインメッセージ保存](https://docs.emqx.com/en/enterprise/v4.4/rule/offline_msg_to_redis.html) 機能は削除されました。

- EMQX 4.4 の [サブスクリプション取得](https://docs.emqx.com/en/enterprise/v4.4/rule/get_subs_from_redis.html) 機能は削除されました。

- DolphinDB、Lindorm、SAP Event Mesh のデータブリッジはサポートされていませんが、SAP Event Mesh は製品ロードマップに含まれています。

- リソースタイプとしての `EMQX Bridge` はサポートされなくなりました。

  <img src="./assets/emqx-bridge-resource.png" alt="EMQXブリッジリソース" style="zoom:50%;" />

## 共通の非互換性変更点

- すべての SSL 関連設定オプション（`ssl`、`cafile`、`keyfile`、`certfile`、`verify`）は統一された構造と名称に変更されました。例：`ssl.cacertfile`、`ssl.certfile`、`ssl.keyfile`、`ssl.verify` など。
- クライアントがトピックをサブスクライブした際に外部データベースにオフラインメッセージを保存し取得する機能（`$events/session_subscribed` イベントとブリッジルールアクションによる）は EMQX 5.1 には存在しません。

## 機能および設定項目の非互換性

以下は各データブリッジごとの機能および設定項目の変更点です。

### Cassandra

- 設定名 `nodes` は `servers` に変更されました。

### Kafka Producer

- 変更された設定項目：
  - `servers` → `bootstrap_hosts`
  - `authentication_mechanism` → `authentication`
  - `sync_timeout` → `sync_query_timeout`
  - `send_buffer` → `socket_opts.sndbuf`
  - `tcp_keepalive` → `socket.tcp_keepalive`
  - `strategy` → `partition_strategy`
  - `cache_mode` → `kafka.buffer.mode`
  - バッファモードの列挙値 `memory+disk` → `hybrid`
  - `highmem_drop` → `kafka.buffer.memory_overload_protection`
- EMQX 5.1 での非対応項目：
  - `query_api_versions`
  - `kafka_ext_headers`
- `kafka` キー配下にネストされた `replayq` 関連オプション（例：`max_batch_bytes`）
- メッセージキーはテンプレート可能になり、以前のように限られた選択肢ではなくなりました。

### Kafka Consumer

- 変更された設定項目：
  - `servers` → `bootstrap_hosts`
  - `max_bytes` → `kafka.max_batch_bytes`
  - `offset_reset_policy` 列挙値：`{reset_to_latest, reset_by_subscriber}` → `{latest, earliest}`
- EMQX 5.1 には `pool_size` がなく、パーティション数に応じてライブラリがワーカー数を自動設定します。
- EMQX 4.4 では認証にプレーン SASL のみ対応していましたが、EMQX 5.1 では Kafka Producer と同じ認証機構をサポートしています。

### Pulsar Consumer

- EMQX 5.1.0 には Pulsar Consumer が存在しません。

### Pulsar Producer

- EMQX 5.1 ではブリッジはドライバーの非同期 API のみを使用し、同期 API のオプションはありません。
- メッセージキーのテンプレートが追加され、以前のような限定的な選択肢ではなくなりました。
- 変更された設定項目：
  - バッファモード列挙値 `memory+disk` → `hybrid`
  - `max_total_bytes` → `buffer.per_partition_limit`
  - `segment_bytes` → `buffer.segment_bytes`

### Redis

- 設定項目 `cmd` は `command_template` に変更されました（3つの Redis モード共通）。
- 「クラスタ」モードの変更点：
  - EMQX 5.1 には `database` フィールドがありません。
  - EMQX 4.4 のオフラインメッセージの `ttl` に相当するものは EMQX 5.1 にありません。

### Postgres

- コネクターに差異はありません。
- バッチ処理設定はアクション設定の `resource_opts.*` に移動しました。
  - `enable_batch = true`（EMQX 4.4） → `resource_opts.batch_size > 1`（EMQX 5.1）
  - `batch_time` は非表示で EMQX 5.1 ではデフォルト `0`
  - `sql` → `prepare_statement`

### MySQL

- `user` は `username` に変更されました。
- バッチ処理設定はアクション設定の `resource_opts.*` に移動しました。
  - `enable_batch = true`（EMQX 4.4） → `resource_opts.batch_size > 1`（EMQX 5.1）
  - `batch_time` は非表示で EMQX 5.1 ではデフォルト `0`
  - `sql` → `prepare_statement`

### MQTT

- 変更された設定項目：
  - `address` → `server`
  - `pool_size` → `{egress,ingress}.pool_size`
  - `reconnect_interval` → `resource_opts.health_check_interval`
- EMQX 5.1 には以下の項目はありません：
  - `append`
  - `mountpoint`
- EMQX 4.4 の `disk_cache = on` は、EMQX 5.1 の隠し設定オプションである `resource_opts.buffer_mode = volatile_offload` に多少近いですが、デフォルトは `memory_only` です。
- EMQX 5.1 には RPC MQTT ブリッジの相当機能はありません。
- アクションの設定変更：
  - `forward_topic` → `egress.remote.topic`
  - `payload_tmpl` → `payload`

### InfluxDB

API v1 と v2 の両方に共通する変更点：

- ブリッジ設定の変更：
  - `host` と `port` は `server` に変更
  - `https_enabled` や `tls_version` などの SSL オプションは `ssl` に統合
- アクション設定の変更：
  - EMQX 5.1 には `int_suffix` の相当はなく、型は直接 `write_syntax` で指定
  - `measurement`、`timestamp`、`fields`、`tags` はすべて `write_syntax` に変更

### DolphinDB、Lindorm、SAP Event Mesh

EMQX 5.1 にはこれらのデータブリッジの相当はありません。

### Clickhouse

- 変更された設定項目：
  - `server` → `url`
  - `user` → `username`
  - `key` → `password`

### Dynamo

- EMQX 5.1 には `region` の相当はありません。
- `payload_template` が追加されました。

### HStreamDB

- 設定項目 `server` は `url` に変更されました。
- EMQX 5.1 には以下の項目の相当はありません：
  - `grpc_timeout`
  - `partition_key`
  - `grpc_flush_timeout`

### IoTDB

- 変更された設定項目：
  - `host`、`rest_port` → `base_url`
  - `request_timeout` → `resource_opts.request_ttl`

### MongoDB

- 変更された設定項目：
  - `login` → `username`
  - `connectTimeoutMS` → `connect_timeout_ms`
  - `rs_set_name` → `replica_set_name`
  - `payload_tmpl` → `payload_template`

### OpenTSDB

- `sync` は `resource_opts.query_mode = sync` に変更されました。

### Oracle

- `user` は `username` に変更されました。

### TDengine

- 変更された設定項目：
  - `host`、`port` → `server`
  - `dbname` → `database`

### GCP PubSub Producer

- 非推奨となった設定項目：
  - `flush_mode`
  - `flush_period_ms`

### RabbitMQ Producer

- 変更された設定項目：
  - `server` → `host` と `port`
  - `payload_tmpl` → `payload_template`
  - `durable` → `delivery_mode`
- `exchange_type` は EMQX 5.1 に相当するものがありません。

### RocketMQ

- EMQX 5.1 に相当するものがない設定項目：
  - `namespace`
  - `strategy`
  - `key`
- 変更された設定項目：
  - `type` → `resource_opts.query_mode`
  - `payload_tmpl` → `payload_template`
