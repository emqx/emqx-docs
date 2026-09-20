# EMQX 5.1 と EMQX 4.4 間のデータ統合非互換性

EMQX 5.1 ではデータ統合の概念が全面的にアップグレードされました。

- 以前の **Rule** -> **Action** -> **Resources** のプロセスは、**Rules** -> **Data Bridge** に変更されました。

  EMQX 4.4 では Action の設定エンティティが存在しましたが、EMQX 5.1 では特定のルールに対してアクションを追加する際、まずデータブリッジを作成し、ブリッジの SQL テンプレートを修正してルール出力に適合させる必要があります。

  <img src="./assets/config-action-for-rule.png" alt="ルールに対するアクション設定" style="zoom:67%;" />

- **Modules** -> **Message Publish** は **Data Bridge** に移動しました。

  EMQX 4.4 のメッセージパブリッシュモジュール:

  <img src="./assets/message-publish-modules.png" alt="メッセージパブリッシュモジュール" style="zoom:67%;" />

- EMQX 4.4 の [オフラインメッセージ保存機能](https://docs.emqx.com/en/enterprise/v4.4/rule/offline_msg_to_redis.html) は削除されました。

- EMQX 4.4 の [サブスクリプション取得機能](https://docs.emqx.com/en/enterprise/v4.4/rule/get_subs_from_redis.html) は削除されました。

- DolphinDB、Lindorm、SAP Event Mesh のデータブリッジはサポートされていませんが、SAP Event Mesh は製品ロードマップに含まれています。

- リソースタイプとしての `EMQX Bridge` はサポートされなくなりました。

  <img src="./assets/emqx-bridge-resource.png" alt="EMQXブリッジリソース" style="zoom:50%;" />

## 共通の非互換変更点

- すべての SSL 関連設定オプション（`ssl`、`cafile`、`keyfile`、`certfile`、`verify`）は統一された構造と名称に変更されました。例：`ssl.cacertfile`、`ssl.certfile`、`ssl.keyfile`、`ssl.verify` など。
- クライアントがトピックをサブスクライブした際に外部データベースにオフラインメッセージを保存し、取得する機能（`$events/session_subscribed` イベントおよびブリッジルールアクションを通じて）は存在しません。

## 機能および設定項目の非互換性

以下に各データブリッジごとの機能および設定項目の変更点を示します。

### Cassandra

- 設定名 `nodes` が `servers` に変更されました。

### Kafka Producer

- 変更された設定項目:
  - `servers` → `bootstrap_hosts`
  - `authentication_mechanism` → `authentication`
  - `sync_timeout` → `sync_query_timeout`
  - `send_buffer` → `socket_opts.sndbuf`
  - `tcp_keepalive` → `socket.tcp_keepalive`
  - `strategy` → `partition_strategy`
  - `cache_mode` → `kafka.buffer.mode`
  - バッファモードの列挙値 `memory+disk` → `hybrid`
  - `highmem_drop` → `kafka.buffer.memory_overload_protection`
- EMQX 5.1 に相当機能なし:
  - `query_api_versions`
  - `kafka_ext_headers`
- `kafka` キー配下にネストされた `replayq` 関連オプション（例：`max_batch_bytes`）
- メッセージキーがテンプレート可能に。以前は限定的なオプションのみ。

### Kafka Consumer

- 変更された設定項目:
  - `servers` → `bootstrap_hosts`
  - `max_bytes` → `kafka.max_batch_bytes`
  - `offset_reset_policy` 列挙値: `{reset_to_latest, reset_by_subscriber}` → `{latest, earliest}`
- EMQX 5.1 には `pool_size` がなく、トピックのパーティション数に応じてワーカー数が自動設定されます。
- EMQX 4.4 では認証にプレーン SASL のみ対応。EMQX 5.1 では Kafka Producer と同様の認証機構をサポート。

### Pulsar Consumer

- EMQX 5.1.0 には Pulsar Consumer が存在しません。

### Pulsar Producer

- EMQX 5.1 ではドライバーの非同期 API のみを使用してメッセージを生成し、同期 API のオプションはありません。
- メッセージキーにテンプレートが使用可能。以前は限定的なオプションのみ。
- 変更された設定項目:
  - バッファモード列挙値 `memory+disk` → `hybrid`
  - `max_total_bytes` → `buffer.per_partition_limit`
  - `segment_bytes` → `buffer.segment_bytes`

### Redis

- 設定項目 `cmd` が `command_template` に変更（3つの Redis モード共通）。
- 「Cluster」モードの変更点:
  - EMQX 5.1 には `database` フィールドがありません。
  - EMQX 4.4 のオフラインメッセージ用 `ttl` に相当するものは EMQX 5.1 にありません。

### Postgres

- コネクターに差異なし。
- バッチ設定は Action 設定の `resource_opts.*` に移動。
  - `enable_batch = true`（EMQX 4.4）→ `resource_opts.batch_size > 1`（EMQX 5.1）
  - `batch_time` は非表示でデフォルト `0`（EMQX 5.1）
  - `sql` → `prepare_statement`

### MySQL

- `user` が `username` に変更。
- バッチ設定は Action 設定の `resource_opts.*` に移動。
  - `enable_batch = true`（EMQX 4.4）→ `resource_opts.batch_size > 1`（EMQX 5.1）
  - `batch_time` は非表示でデフォルト `0`（EMQX 5.1）
  - `sql` → `prepare_statement`

### MQTT

- 変更された設定項目:
  - `address` → `server`
  - `pool_size` → `{egress,ingress}.pool_size`
  - `reconnect_interval` → `resource_opts.health_check_interval`
- EMQX 5.1 に相当機能なし:
  - `append`
  - `mountpoint`
- EMQX 4.4 の `disk_cache = on` は、EMQX 5.1 の隠し設定オプション `resource_opts.buffer_mode = volatile_offload` に多少相当しますが、デフォルトは `memory_only` です。
- EMQX 5.1 には RPC MQTT ブリッジの相当機能なし。
- Action 設定の変更:
  - `forward_topic` → `egress.remote.topic`
  - `payload_tmpl` → `payload`

### InfluxDB

API v1 と v2 の両方に共通する変更点。

- ブリッジ設定の変更:
  - `host` と `port` が `server` に変更
  - `https_enabled` と `tls_version` などの SSL オプションが `ssl` に統合
- Action 設定の変更:
  - EMQX 5.1 には `int_suffix` の相当なし。型は直接 `write_syntax` で指定。
  - `measurement`、`timestamp`、`fields`、`tags` が `write_syntax` に統合

### DolphinDB、Lindorm、SAP Event Mesh

EMQX 5.1 には相当するデータブリッジがありません。

### Clickhouse

- 変更された設定項目:
  - `server` → `url`
  - `user` → `username`
  - `key` → `password`

### Dynamo

- EMQX 5.1 に `region` の相当なし。
- `payload_template` が追加。

### HStreamDB

- 設定項目 `server` が `url` に変更。
- EMQX 5.1 に相当しない項目:
  - `grpc_timeout`
  - `partition_key`
  - `grpc_flush_timeout`

### IoTDB

- 変更された設定項目:
  - `host`、`rest_port` → `base_url`
  - `request_timeout` → `resource_opts.request_ttl`

### MongoDB

- 変更された設定項目:
  - `login` → `username`
  - `connectTimeoutMS` → `connect_timeout_ms`
  - `rs_set_name` → `replica_set_name`
  - `payload_tmpl` → `payload_template`

### OpenTSDB

- `sync` が `resource_opts.query_mode = sync` に変更。

### Oracle

- `user` が `username` に変更。

### TDengine

- 変更された設定項目:
  - `host`、`port` → `server`
  - `dbname` → `database`

### GCP PubSub Producer

- 廃止された設定項目:
  - `flush_mode`
  - `flush_period_ms`

### RabbitMQ Producer

- 変更された設定項目:
  - `server` → `host` と `port`
  - `payload_tmpl` → `payload_template`
  - `durable` → `delivery_mode`
- `exchange_type` は EMQX 5.1 に相当なし。

### RocketMQ

- EMQX 5.1 に相当しない設定項目:
  - `namespace`
  - `strategy`
  - `key`
- 変更された設定項目:
  - `type` → `resource_opts.query_mode`
  - `payload_tmpl` → `payload_template`
