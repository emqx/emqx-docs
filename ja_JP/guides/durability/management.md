# Durable Sessionsの設定と管理

本ドキュメントでは、EMQXにおける[MQTT Durable Sessions](./durability_introduction.md)機能の設定、管理、および最適化に関するリファレンスと手順を提供します。セッションおよびストレージの設定も含みます。

## 設定パラメータ

MQTT Durable Sessionsの設定は大きく2つのカテゴリに分かれています。

- `durable_sessions`：MQTTクライアントのセッションに関する設定で、耐久ストレージからのデータ消費方法やデータ保持期間などを含みます。
- `durable_storage`：MQTTメッセージデータを保持する耐久ストレージシステムの設定を管理します。

### Durable Sessionsの設定

Durable Sessionsのパラメータはダッシュボードから設定可能です。ダッシュボードの左メニューで **Management** -> **MQTT Settings** をクリックし、**Durable Session** タブを選択してパラメータを設定します。

<img src="./assets/dashboard_session_config.png" alt="ダッシュボードのセッション設定" style="zoom:67%;" />

| パラメータ                                   | ダッシュボードUI           | 説明                                                         |
| ------------------------------------------- | --------------------------- | ------------------------------------------------------------ |
| `durable_sessions.enable`                   | Durable Sessionsを有効化    | セッションの耐久性を有効にします。この設定はダッシュボード、REST API、CLIからは変更できず、設定ファイルでのみ設定可能です。変更にはEMQXノードの再起動が必要です。 |
| `durable_sessions.message_retention_period` | メッセージ保持期間          | Durable Sessions内のMQTTメッセージの保持期間を定義します。注意：このパラメータはグローバル設定です。 |
| `durable_sessions.batch_size`               | メッセージクエリバッチサイズ | Durable Sessionsがストレージから消費するメッセージの最大バッチサイズを制御します。 |
| `durable_sessions.checkpoint_interval`      | セッションチェックポイント間隔 | セッションメタデータの保存間隔を指定します。                   |

以下のパラメータは[ゾーン](../configuration/configuration.md#zone-override)ごとに上書き可能です。

- `durable_sessions.enable`
- `durable_sessions.batch_size`
- `durable_sessions.checkpoint_interval`

### Durable Storageの設定

`<DS>` は「durable storage（耐久ストレージ）」を表すプレースホルダーです。現在、利用可能な `<DS>` のパラメータは `message` のみです。

#### コア耐久ストレージパラメータ

| パラメータ                                 | 説明                                                         |
| ----------------------------------------- | ------------------------------------------------------------ |
| `durable_storage.n_sites`                 | [サイト数](./managing-replication.md#number-of-sites)を指定します。 |
| `durable_storage.<DS>.data_dir`           | EMQXがデータを保存するファイルシステム上のディレクトリです。    |
| `durable_storage.<DS>.n_shards`           | [シャード数](./managing-replication.md#number-of-shards)を指定します。 |
| `durable_storage.<DS>.replication_factor` | [レプリケーション係数](./managing-replication.md#replication-factor)で、各シャードのレプリカ数を決定します。 |
| `durable_storage.<DS>.transaction`        | メッセージバッファリングに関するパラメータを含みます。詳細は[バッファリング](#buffering)を参照してください。 |
| `durable_storage.<DS>.layout`             | EMQXがディスク上にデータを配置する方法を制御するパラメータを含みます。詳細は[ストレージレイアウト設定](#storage-layout-configuration)を参照してください。 |

#### データベースグループの設定

EMQX 6.0.2以降、Durable Storageは[データベースグループ](../../develop/design/durable-storage.md#durable-storage-database-groups)を導入し、ノードレベルのリソースガバナンスをサポートしています。データベースグループにより、複数の耐久ストレージデータベースを論理データモデルを変更せずに共有リソース制限のもとで一括管理できます。

デフォルトでは、各耐久ストレージデータベースは自身の名前を冠したデータベースグループに属し、そのグループにはそのデータベースのみが含まれ、従来の動作を維持します。

データベースグループは `durable_storage.db_groups` ネームスペースで設定します。

| パラメータ                                                 | 説明                                                     |
| --------------------------------------------------------- | -------------------------------------------------------- |
| `durable_storage.db_groups.<group>.storage_quota`         | グループ内のSSTファイル合計ディスク使用量のソフトクォータです。 |
| `durable_storage.db_groups.<group>.write_buffer_size`     | グループのRocksDBメモリテーブルの最大合計サイズです。    |
| `durable_storage.db_groups.<group>.rocksdb_nthreads_high` | 高優先度のRocksDBバックグラウンドスレッド数です。         |
| `durable_storage.db_groups.<group>.rocksdb_nthreads_low`  | 低優先度のRocksDBバックグラウンドスレッド数です。         |

#### バッファリング

EMQXはクライアントからのMQTTメッセージを耐久ストレージにバッチ単位で書き込み、スループットを最大化します。バッチングは `durable_storage.<DS>.transaction` 配下の以下のパラメータで設定します。

| パラメータ             | 説明                                                         |
| --------------------- | ------------------------------------------------------------ |
| `max_pending`         | 指定したメッセージ数に達した時点でバッファをフラッシュします。 |
| `flush_interval`      | バッファに1件以上メッセージがある場合、この間隔でバッファをフラッシュします。 |
| `idle_flush_interval` | 新規メッセージがこの間隔内に到着しなかった場合、早期にバッファをフラッシュします。 |

#### ストレージレイアウト設定

ストレージレイアウトはEMQXがディスク上にデータをどのように配置するかを決定します。`durable_storage.<DS>.layout.type` パラメータを設定することで、新しい[世代](./durability_introduction.md#generation)で使用するレイアウトを変更できます。この変更は既存の世代には影響しません。各レイアウトタイプの設定は `durable_storage.<DS>.layout` 配下にあります。現在は `wildcard_optimized` レイアウトタイプが利用可能です。

##### `wildcard_optimized` レイアウトタイプの設定

`wildcard_optimized` レイアウトは、多数のMQTTトピックに対するワイルドカードサブスクライブのマッチングを最適化することを目的としています。トピック構造に関する知識を自律的に蓄積し、軽量な機械学習アルゴリズムを用いてクライアントがサブスクライブしそうなワイルドカードトピックフィルターを予測します。その後、これらのトピックを統合ストリームに整理し、一度のスイープで効率的に消費できるようにします。

| パラメータ               | 説明                                                     |
| ----------------------- | -------------------------------------------------------- |
| `bytes_per_topic_level` | トピックレベルのハッシュサイズを決定します。             |
| `topic_index_bytes`     | ストリーム識別子のバイト数を指定します。                 |

## CLIコマンド

以下のCLIコマンドで耐久ストレージの管理が可能です。

### `emqx ctl ds info`

耐久ストレージの状態概要を表示します。

例：

```bash
$ emqx ctl ds info

THIS SITE:
D8894F95DC86DFDB

SITES:
.------------------.-------------------.----------.
: Site             : Node              : Status   :
:------------------:-------------------:----------:
: 5C6028D6CE9459C7 : 'emqx@n2.local'   : up       :
: D8894F95DC86DFDB : 'emqx@n1.local'   : up       :
: F4E92DEA197C8EBC : 'emqx@n3.local'   : (x) down :
`------------------`-------------------`----------`

SHARDS:
.-------------.------------------.-------------.
: DB/Shard    : Replicas         : Transitions :
:-------------:------------------:-------------:
:-messages/0--:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/1--:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/10-:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/11-:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/12-:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/2--:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/3--:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/4--:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/5--:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/6--:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/7--:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/8--:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
:-messages/9--:------------------:-------------:
:             : 5C6028D6CE9459C7 :             :
`-------------`------------------`-------------`
```

このコマンド出力には以下が含まれます：

- `THIS SITE`：ローカルEMQXノードが担当するサイトのID。
- `SITES`：既知の全サイトの一覧。EMQXノード名とステータスを含みます。
- `SHARDS`：耐久ストレージのシャード一覧と、そのレプリカが配置されているサイトID。

### `emqx ctl ds set-replicas all <site1> <site2> ...`

クラスタ内の耐久ストレージレプリカを保持するサイトのリストを設定します。実行すると、シャードをサイト間で公平に割り当てる操作計画が作成され、バックグラウンドで実行されます。

::: warning 重要なお知らせ
耐久ストレージのレプリカリスト更新は、サイト間で大量のデータコピーを伴うためコストが高くなる可能性があります。
:::

例：

```bash
$ emqx ctl ds set-replicas all 5C6028D6CE9459C7 D8894F95DC86DFDB F4E92DEA197C8EBC
ok
```

このコマンド実行後、`ds info` の出力は以下のようになる場合があります。

```bash
$ emqx ctl ds info

THIS SITE:
D8894F95DC86DFDB

SITES:
.------------------.-------------------.----------.
: Site             : Node              : Status   :
:------------------:-------------------:----------:
: 5C6028D6CE9459C7 : 'emqx@n2.local'   : up       :
: D8894F95DC86DFDB : 'emqx@n1.local'   : up       :
: F4E92DEA197C8EBC : 'emqx@n3.local'   : up       :
`------------------`-------------------`----------`

SHARDS:
.-------------.------------------.--------------------.
: DB/Shard    : Replicas         : Transitions        :
:-------------:------------------:--------------------:
:-messages/0--:------------------:--------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             : D8894F95DC86DFDB :                    :
:-messages/1--:------------------:--------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             : D8894F95DC86DFDB :                    :
:-messages/10-:------------------:--------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             :                  : + D8894F95DC86DFDB :
:-messages/11-:------------------:-------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             : D8894F95DC86DFDB :                    :
:-messages/2--:------------------:--------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             : D8894F95DC86DFDB :                    :
:-messages/3--:------------------:--------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             :                  : + D8894F95DC86DFDB :
:-messages/4--:------------------:-------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             : D8894F95DC86DFDB :                    :
:-messages/5--:------------------:--------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             : D8894F95DC86DFDB :                    :
:-messages/6--:------------------:--------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             :                  : + D8894F95DC86DFDB :
:-messages/7--:------------------:-------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             : D8894F95DC86DFDB :                    :
:-messages/8--:------------------:--------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             : D8894F95DC86DFDB :                    :
:-messages/9--:------------------:--------------------:
:             : 5C6028D6CE9459C7 : + F4E92DEA197C8EBC :
:             :                  : + D8894F95DC86DFDB :
`-------------`------------------`--------------------`
```

新たに追加された `REPLICA TRANSITIONS` セクションには保留中の操作が表示され、すべて完了すると空になります。

### `emqx ctl ds join all <site>` / `emqx ctl ds leave all <site>`

これらのコマンドは耐久ストレージのレプリカサイトリストにサイトを追加または削除します。`set_replicas` コマンドと似ていますが、一度に1サイトずつ更新します。

例：

```bash
$ emqx ctl ds join all B2A7DBB2413CD6EE
ok
```

詳細は[サイトの追加](./managing-replication.md#add-sites)および[サイトの削除](./managing-replication.md#remove-sites)を参照してください。

## REST API

組み込みのDurable Sessionsの管理と監視に利用できるREST APIエンドポイントは以下の通りです。

- `/ds/sites`：既知のサイト一覧を取得します。
- `/ds/sites/:site`：サイトの情報（ステータス、管理中のEMQXノード名など）を取得します。
- `/ds/storages`：耐久ストレージ一覧を取得します。
- `/ds/storages/:ds`：耐久ストレージおよびそのシャードの情報を取得します。
- `/ds/storages/:ds/replicas`：耐久ストレージのレプリカを保持するサイトの一覧取得および更新を行います。
- `/ds/storages/:ds/replicas/:site`：特定サイトの耐久ストレージレプリカの追加・削除を行います。

詳細はEMQX OpenAPIスキーマを参照してください。

## メトリクス

Durable Sessionsに関連するPrometheusメトリクスは以下の通りです。

### `emqx_ds_egress_batches`

耐久ストレージへのメッセージバッチ書き込みが成功するたびにインクリメントされます。

### `emqx_ds_egress_messages`

耐久ストレージに正常に書き込まれたメッセージ数をカウントします。

### `emqx_ds_egress_bytes`

耐久ストレージに正常に書き込まれたペイロードデータの合計バイト数をカウントします。注意：このメトリクスはメッセージペイロードのみを対象としているため、実際の書き込みデータ量はこれより大きい場合があります。

### `emqx_ds_egress_batches_failed`

耐久ストレージへの書き込みが何らかの理由で失敗するたびにインクリメントされます。

### `emqx_ds_egress_flush_time`

耐久ストレージにバッチを書き込むのにかかる時間（μs単位）のローリング平均です。レプリケーション速度の重要な指標です。

### `emqx_ds_store_batch_time`

ローカルのRocksDBストレージにバッチを書き込むのにかかる時間（μs単位）のローリング平均です。`emqx_ds_egress_flush_time`とは異なり、ネットワークレプリケーションコストを除外しているため、ディスクI/O効率の重要な指標となります。

### `emqx_ds_builtin_next_time`

耐久ストレージからメッセージバッチを消費するのにかかる時間（μs単位）のローリング平均です。

### `emqx_ds_storage_bitfield_lts_counter_seek` および `emqx_ds_storage_bitfield_lts_counter_next`

これらは「wildcard optimized」ストレージレイアウト固有のカウンターで、ローカルストレージからのデータ消費効率を測定します。`seek` 操作は一般的に遅いため、`emqx_ds_storage_bitfield_lts_counter_next` の増加率が `seek` より速いことが望ましいです。

`durable_storage.messages.layout.epoch_bits` パラメータを増やすことでこの比率を改善できます。

### `emqx_ds_raft_db_shards_num`

DBが分割されているシャード数です。

### `emqx_ds_raft_db_sites_num`

DS DBがレプリケートされている現在および割り当てられたサイト数を追跡するゲージです。

通常、現在のサイト数は割り当てられたサイト数と同じです。長期間異なる場合はレプリカ転送に問題がある可能性があります。

### `emqx_ds_raft_shard_replication_factor`

DS DBシャードのレプリカセット内のレプリカ数を追跡します。

この数が設定されたレプリケーション係数を下回る場合、耐久性が危険にさらされているため、より多くのサイトにレプリカを再配置することを検討してください。

### `emqx_ds_raft_db_shards_online_num`

このノードでアクティブに管理されているDS DBシャード数を追跡します。

この数は現在このノードに割り当てられているシャード数と一致する必要があります。異なる場合は可用性に問題がある可能性があるため、ログを確認してください。

### `emqx_ds_raft_shard_transition_queue_len`

DS DBシャードの保留中のレプリカセット遷移数を追跡します。

この数が長期間ゼロでない場合、レプリカ転送に問題があります。

### `emqx_ds_raft_shard_transitions`

DBシャードのレプリカセット遷移の開始・完了・スキップ・クラッシュ数をカウントします。

クラッシュした遷移は常にゼロであるべきです。そうでない場合はログのエラーを確認してください。

### `emqx_ds_raft_shard_transition_errors`

DBシャードのレプリカセット遷移のオーケストレーション中に発生した一時的なエラー数をカウントします。

このカウンターが増加する場合、レプリカ転送に問題があるためログを確認してください。

### `emqx_ds_raft_snapshot_reads`

シャードがスナップショットレプリケーションのソースであった際のスナップショット読み取りの開始・完了数をカウントします。

### `emqx_ds_raft_snapshot_read_errors`

スナップショット読み取り中に発生し、スナップショットレプリケーションが中断されたエラー数をカウントします。

エラーは発生しないことが期待されるため、ログで原因を調査してください。

### `emqx_ds_raft_snapshot_read_chunks`

スナップショット転送のソースとなるDS DBシャードで読み取られ、受信側に転送されたチャンク数をカウントします。

### `emqx_ds_raft_snapshot_read_chunk_bytes`

ソースDS DBシャードでチャンクとして読み取られたバイト数をカウントします。

### `emqx_ds_raft_snapshot_writes`

シャードがスナップショットレプリケーションの受信側であった際のスナップショット書き込みの開始・完了数をカウントします。

### `emqx_ds_raft_snapshot_write_errors`

受信側DS DBシャードへのスナップショット書き込み中に発生し、レプリケーションが中断されたエラー数をカウントします。

これも増加しないことが期待されるため、詳細はログを確認してください。

### `emqx_ds_raft_snapshot_write_chunks`

ソースDS DBシャードから受信し、受信側に書き込まれたチャンク数をカウントします。

### `emqx_ds_raft_snapshot_write_chunk_bytes`

受信側DS DBシャードでチャンクとして書き込まれたバイト数をカウントします。

### `emqx_ds_raft_current_timestamp_us`

シャードサーバーが現在レプリケートしている最新の操作タイムスタンプ（マイクロ秒単位）を追跡します。

通常、各レプリカは同じタイムスタンプを持つべきです。異なる場合はレプリケーションに問題があります。

### `emqx_ds_raft_rasrv_state_changes`

Raftサーバーが候補者／フォロワー／リーダーに変わった回数をカウントします。

頻繁な状態変化は不安定の兆候です。ログを確認してください。

### データベースグループメトリクス

以下のPrometheusメトリクスは耐久ストレージのデータベースグループに対するノードレベルの可視性を提供します。

#### `emqx_ds_disk_usage`

グループ内の全データベースで使用されているSSTファイルの合計サイズ。

#### `emqx_ds_write_buffer_memory_usage`

グループで使用されているRocksDBメモリテーブルの合計メモリ使用量。

#### `emqx_ds_total_trash_size`

削除待ちの不要なSSTファイルのディスク使用量。

これらのメトリクスはノードおよびデータベースグループごとに報告されます。クラスタ環境では、運用者が外部で集約してクラスタ全体の容量を評価できます。
