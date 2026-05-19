# Durable Sessions の設定と管理

本ドキュメントでは、EMQX における [MQTT Durable Sessions](./durability_introduction.md) 機能の設定、管理、および最適化に関するリファレンスと手順を提供します。セッションおよびストレージの設定方法についても解説します。

## 設定パラメータ

MQTT Durable Sessions の設定は大きく2つのカテゴリに分かれています。

- `durable_sessions`：MQTT クライアントのセッションに関する設定で、耐久ストレージからのデータ消費方法やデータ保持期間などを含みます。
- `durable_storage`：MQTT メッセージデータを保持する耐久ストレージシステムの設定を管理します。

### Durable Sessions の設定

Dashboard で Durable Sessions のパラメータを設定できます。Dashboard の左メニューから **Management** -> **MQTT Settings** をクリックし、**Durable Session** タブを選択してパラメータを設定してください。

<img src="./assets/dashboard_session_config.png" alt="ダッシュボードのセッション設定" style="zoom:67%;" />

| パラメータ                                   | Dashboard UI 表示名           | 説明                                                         |
| ------------------------------------------- | ----------------------------- | ------------------------------------------------------------ |
| `durable_sessions.enable`                   | Durable Sessions を有効化      | セッション耐久性を有効にします。この設定は Dashboard、REST API、CLI から変更できず、設定ファイルでのみ変更可能です。変更を反映するには EMQX ノードの再起動が必要です。 |
| `durable_sessions.message_retention_period` | メッセージ保持期間             | Durable Sessions 内の MQTT メッセージの保持期間を定義します。このパラメータはグローバル設定です。 |
| `durable_sessions.batch_size`               | メッセージクエリバッチサイズ   | Durable Sessions がストレージから消費するメッセージの最大バッチサイズを制御します。 |
| `durable_sessions.checkpoint_interval`      | セッションチェックポイント間隔 | セッションメタデータを保存する間隔を指定します。              |

以下のパラメータは [ゾーン](../configuration/configuration.md#zone-override)ごとに上書き可能です。

- `durable_sessions.enable`
- `durable_sessions.batch_size`
- `durable_sessions.checkpoint_interval`

### Durable Storage の設定

`<DS>` は「durable storage（耐久ストレージ）」のプレースホルダーです。現在、利用可能な `<DS>` のパラメータは `message` のみです。

#### コア Durable Storage パラメータ

| パラメータ                                 | 説明                                                         |
| ----------------------------------------- | ------------------------------------------------------------ |
| `durable_storage.n_sites`                 | [サイト数](./managing-replication.md#number-of-sites)を指定します。 |
| `durable_storage.<DS>.data_dir`           | EMQX がデータを保存するファイルシステム上のディレクトリです。 |
| `durable_storage.<DS>.n_shards`           | [シャード数](./managing-replication.md#number-of-shards)を指定します。 |
| `durable_storage.<DS>.replication_factor` | [レプリケーションファクター](./managing-replication.md#replication-factor)で、各シャードのレプリカ数を決定します。 |
| `durable_storage.<DS>.transaction`        | メッセージバッファリングに関するパラメータを含みます。詳細は [バッファリング](#buffering) を参照してください。 |
| `durable_storage.<DS>.layout`             | EMQX がディスク上にデータを配置する方法を制御するパラメータを含みます。詳細は [ストレージレイアウトの設定](#storage-layout-configuration) を参照してください。 |

#### データベースグループの設定

EMQX 6.0.2 以降、Durable Storage は [データベースグループ](../design/durable-storage.md/#durable-storage-database-groups)を導入し、ノード単位のリソースガバナンスをサポートしています。データベースグループにより、複数の耐久ストレージデータベースを論理データモデルを変更することなく、共有リソース制限のもとで一括管理できます。

デフォルトでは、各耐久ストレージデータベースは自身の名前を持つデータベースグループに属し、そのグループにはその単一のデータベースのみが含まれ、従来の動作を維持します。

データベースグループは `durable_storage.db_groups` ネームスペースで設定します。

| パラメータ                                                 | 説明                                                       |
| --------------------------------------------------------- | ---------------------------------------------------------- |
| `durable_storage.db_groups.<group>.storage_quota`         | グループの SST ファイル合計ディスク使用量のソフトクォータです。 |
| `durable_storage.db_groups.<group>.write_buffer_size`     | グループの RocksDB メモリテーブルの最大合計サイズです。       |
| `durable_storage.db_groups.<group>.rocksdb_nthreads_high` | 高優先度 RocksDB バックグラウンドスレッド数です。             |
| `durable_storage.db_groups.<group>.rocksdb_nthreads_low`  | 低優先度 RocksDB バックグラウンドスレッド数です。             |

#### バッファリング

EMQX はクライアントからの MQTT メッセージを耐久ストレージにバッチで書き込み、スループットを最大化します。バッチングは `durable_storage.<DS>.transaction` 設定サブツリーの以下のパラメータで制御します。

| パラメータ             | 説明                                                         |
| --------------------- | ------------------------------------------------------------ |
| `max_pending`         | 指定されたメッセージ数に達したらバッファをフラッシュします。 |
| `flush_interval`      | 少なくとも1件のメッセージがある場合、この時間間隔でバッファをフラッシュします。 |
| `idle_flush_interval` | 新しいメッセージがこの間隔内に到着しなければ早期にバッファをフラッシュします。 |

#### ストレージレイアウトの設定

ストレージレイアウトは EMQX がディスク上にデータをどのように配置するかを決定します。`durable_storage.<DS>.layout.type` パラメータを設定することで、新しい [世代](./durability_introduction.md#generation)で使用されるレイアウトを変更できます。この変更は既存の世代には影響しません。各レイアウトタイプの設定は `durable_storage.<DS>.layout` サブツリーに含まれます。現在は `wildcard_optimized` レイアウトタイプが利用可能です。

##### `wildcard_optimized` レイアウトタイプの設定

`wildcard_optimized` レイアウトは、多数の MQTT トピックに対するワイルドカードサブスクライブのマッチングを最適化することを目的としています。時間経過でトピック構造に関する知識を自律的に蓄積し、軽量な機械学習アルゴリズムを活用してクライアントがサブスクライブしそうなワイルドカードトピックフィルターを予測します。その後、これらのトピックを統合されたストリームに整理し、一度の処理で効率的に消費できるようにします。

| パラメータ               | 説明                                                       |
| ----------------------- | ---------------------------------------------------------- |
| `bytes_per_topic_level` | トピックレベルのハッシュサイズを決定します。               |
| `topic_index_bytes`     | ストリーム識別子のバイトサイズを指定します。                 |

## CLI コマンド

耐久ストレージの管理に利用できる CLI コマンドは以下の通りです。

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

このコマンドの出力には以下が含まれます。

- `THIS SITE`：ローカル EMQX ノードが保持するサイトの ID。
- `SITES`：既知のすべてのサイトのリスト。EMQX ノード名とそのステータスを含みます。
- `SHARDS`：耐久ストレージのシャードと、そのレプリカが配置されているサイト ID のリスト。

### `emqx ctl ds set-replicas all <site1> <site2> ...`

このコマンドは、クラスター内で耐久ストレージのレプリカを保持するサイトのリストを設定します。実行すると、シャードをサイト間で公平に割り当てるための操作計画を作成し、バックグラウンドで実行を続けます。

::: warning 重要なお知らせ
耐久ストレージのレプリカリストの更新は、サイト間で大量のデータコピーが発生する可能性があるためコストがかかる場合があります。
:::

例：

```bash
$ emqx ctl ds set-replicas all 5C6028D6CE9459C7 D8894F95DC86DFDB F4E92DEA197C8EBC
ok
```

このコマンド実行後、`ds info` の出力例は以下のようになります。

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

新たに追加された `REPLICA TRANSITIONS` セクションには保留中の操作が表示されます。すべての操作が完了すると、このリストは空になります。

### `emqx ctl ds join all <site>` / `emqx ctl ds leave all <site>`

これらのコマンドは、耐久ストレージのレプリカサイトリストにサイトを1つずつ追加または削除します。`set_replicas` コマンドと似ていますが、1サイト単位で更新します。

例：

```bash
$ emqx ctl ds join all B2A7DBB2413CD6EE
ok
```

詳細は [Add Sites](./managing-replication.md#add-sites) および [Remove Sites](./managing-replication.md#remove-sites) を参照してください。

## REST API

組み込み Durable Sessions の管理および監視に利用できる REST API エンドポイントは以下の通りです。

- `/ds/sites`：既知のサイト一覧を取得します。
- `/ds/sites/:site`：サイトの情報（ステータス、現在そのサイトを管理する EMQX ノード名など）を取得します。
- `/ds/storages`：耐久ストレージ一覧を取得します。
- `/ds/storages/:ds`：耐久ストレージおよびそのシャードの情報を取得します。
- `/ds/storages/:ds/replicas`：耐久ストレージのレプリカを保持するサイトの一覧取得および更新を行います。
- `/ds/storages/:ds/replicas/:site`：特定サイトの耐久ストレージレプリカの追加または削除を行います。

詳細は EMQX OpenAPI スキーマを参照してください。

## メトリクス

Durable Sessions に関連する Prometheus メトリクスは以下の通りです。

### `emqx_ds_egress_batches`

耐久ストレージへのメッセージバッチ書き込みが成功するたびにインクリメントされます。

### `emqx_ds_egress_messages`

耐久ストレージに正常に書き込まれたメッセージ数をカウントします。

### `emqx_ds_egress_bytes`

耐久ストレージに正常に書き込まれたペイロードデータの総量をカウントします。注意：このメトリクスはメッセージのペイロードのみを対象としているため、実際に書き込まれるデータ量はこれより多い可能性があります。

### `emqx_ds_egress_batches_failed`

耐久ストレージへの書き込みが何らかの理由で失敗するたびにインクリメントされます。

### `emqx_ds_egress_flush_time`

耐久ストレージへのバッチ書き込みにかかった時間（μ秒）のローリング平均です。レプリケーション速度の重要な指標です。

### `emqx_ds_store_batch_time`

ローカル RocksDB ストレージへのバッチ書き込みにかかった時間（μ秒）のローリング平均です。`emqx_ds_egress_flush_time` と異なり、ネットワークレプリケーションのコストを除外しているため、ディスク I/O 効率の重要な指標となります。

### `emqx_ds_builtin_next_time`

耐久ストレージからメッセージバッチを消費するのにかかった時間（μ秒）のローリング平均です。

### `emqx_ds_storage_bitfield_lts_counter_seek` と `emqx_ds_storage_bitfield_lts_counter_next`

これらのカウンターは「wildcard optimized」ストレージレイアウト固有のもので、ローカルストレージからのデータ消費効率を測定します。`seek` 操作は一般的に遅いため、`emqx_ds_storage_bitfield_lts_counter_next` の増加率が `seek` より速いことが望ましいです。

`durable_storage.messages.layout.epoch_bits` パラメータを増やすことで、この比率の改善に寄与します。

### `emqx_ds_raft_db_shards_num`

DB が分割されているシャード数です。

### `emqx_ds_raft_db_sites_num`

DS DB がレプリケートされている現在および割り当てられているサイト数を追跡するゲージです。

通常、現在のサイト数は割り当てられたサイト数と等しいはずです。長期間異なる場合は、レプリカ転送に問題がある可能性があります。

### `emqx_ds_raft_shard_replication_factor`

DS DB シャードのレプリカセット内のレプリカ数を追跡します。

この数が設定されたレプリケーションファクターを下回る場合、耐久性が危険にさらされているため、より多くのサイトにレプリカを再配置することを検討してください。

### `emqx_ds_raft_db_shards_online_num`

このノードでアクティブに管理されている DS DB シャード数を追跡します。

この数は現在このノードに割り当てられているシャード数と一致する必要があります。異なる場合は可用性に問題がある可能性があるため、ログを確認してください。

### `emqx_ds_raft_shard_transition_queue_len`

DS DB シャードのレプリカセット遷移の保留数を追跡します。

この数が長期間ゼロでない場合、レプリカ転送に問題があります。

### `emqx_ds_raft_shard_transitions`

DB シャードのレプリカセット遷移の開始／完了／スキップ／クラッシュ数をカウントします。

クラッシュした遷移は常にゼロであるべきです。そうでない場合はログを確認してください。

### `emqx_ds_raft_shard_transition_errors`

DB シャードのレプリカセット遷移のオーケストレーション中に発生した一時的なエラー数をカウントします。

このカウンターが増加する場合、レプリカ転送に問題があるためログを確認してください。

### `emqx_ds_raft_snapshot_reads`

シャードがスナップショットレプリケーションのソースであった際のスナップショット読み込みの開始／完了数をカウントします。

### `emqx_ds_raft_snapshot_read_errors`

スナップショット読み込み中に発生し、スナップショットレプリケーションが中止されたエラー数をカウントします。

エラーは発生しないことが期待されるため、ログで原因を調査してください。

### `emqx_ds_raft_snapshot_read_chunks`

スナップショット転送のソース DS DB シャードで読み込まれ、受信側に転送されたチャンク数をカウントします。

### `emqx_ds_raft_snapshot_read_chunk_bytes`

ソース DS DB シャードでチャンクとして読み込まれたバイト数をカウントします。

### `emqx_ds_raft_snapshot_writes`

シャードがスナップショットレプリケーションの受信側であった際のスナップショット書き込みの開始／完了数をカウントします。

### `emqx_ds_raft_snapshot_write_errors`

スナップショット書き込み中に発生し、スナップショットレプリケーションが中止されたエラー数をカウントします。

こちらも増加は想定されないため、ログを確認してください。

### `emqx_ds_raft_snapshot_write_chunks`

ソース DS DB シャードから受信し、受信側に書き込まれたチャンク数をカウントします。

### `emqx_ds_raft_snapshot_write_chunk_bytes`

受信側 DS DB シャードでチャンクとして書き込まれたバイト数をカウントします。

### `emqx_ds_raft_current_timestamp_us`

シャードサーバーが現在レプリケートしている最新の操作タイムスタンプ（マイクロ秒単位）を追跡します。

通常、各レプリカは同じタイムスタンプを持つべきです。異なる場合はレプリケーションに問題があります。

### `emqx_ds_raft_rasrv_state_changes`

Raft サーバーが候補者／フォロワー／リーダーに状態変化した回数をカウントします。

頻繁な状態変化は不安定の兆候です。ログを確認してください。

### データベースグループのメトリクス

以下の Prometheus メトリクスはノード単位で耐久ストレージのデータベースグループの状態を可視化します。

#### `emqx_ds_disk_usage`

グループ内のすべてのデータベースが使用する SST ファイルの合計サイズ。

#### `emqx_ds_write_buffer_memory_usage`

グループが使用する RocksDB メモリテーブルの合計メモリ使用量。

#### `emqx_ds_total_trash_size`

削除予定の不要な SST ファイルのディスク使用量。

これらのメトリクスはノードごとおよびデータベースグループごとに報告されます。クラスター環境では、運用者が外部で集計し、クラスター全体の容量を評価できます。
