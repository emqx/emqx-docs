# Durable Sessionsの設定と管理

本ドキュメントでは、EMQXにおける[MQTT Durable Sessions](./durability_introduction.md)機能の設定、管理、および最適化に関するリファレンスと手順を提供します。セッションおよびストレージの設定も含みます。

## 設定パラメータ

MQTT Durable Sessions の設定は大きく2つのカテゴリに分かれています。

- `durable_sessions`：MQTT クライアントのセッションに関する設定を含み、耐久ストレージからのデータ消費方法やデータ保持パラメータを管理します。
- `durable_storage`：MQTT メッセージデータを保持する耐久ストレージシステムの設定を管理します。

### Durable Sessionsの設定

Dashboard の左メニューから **Management** -> **MQTT Settings** をクリックし、**Durable Session** タブを選択すると、Durable Sessions のパラメータを設定できます。

<img src="./assets/dashboard_session_config.png" alt="ダッシュボードのセッション設定" style="zoom:67%;" />

<<<<<<< HEAD
| パラメータ                                   | Dashboard UI 表示名           | 説明                                                         |
| ------------------------------------------- | ----------------------------- | ------------------------------------------------------------ |
| `durable_sessions.enable`                   | Durable Sessions を有効化     | セッションの耐久性を有効にします。この設定は Dashboard、REST API、CLI から変更できず、設定ファイルでのみ設定可能です。変更を反映するには EMQX ノードの再起動が必要です。 |
| `durable_sessions.message_retention_period` | メッセージ保持期間            | Durable Sessions 内の MQTT メッセージの保持期間を定義します。注意：このパラメータはグローバル設定です。 |
| `durable_sessions.batch_size`               | メッセージクエリのバッチサイズ | Durable Sessions がストレージから消費するメッセージの最大バッチサイズを制御します。 |
| `durable_sessions.checkpoint_interval`      | セッションチェックポイント間隔 | セッションメタデータを保存する間隔を指定します。               |
=======
| パラメータ                                   | Dashboard UI 表示名          | 説明                                                         |
| ------------------------------------------- | ---------------------------- | ------------------------------------------------------------ |
| `durable_sessions.enable`                   | Enable Durable Sessions      | セッションの耐久性を有効化します。この設定はホットコンフィグレーションで変更できず、設定ファイルで指定する必要があります。変更を反映するには EMQX ノードの再起動が必要です。 |
| `durable_sessions.message_retention_period` | Message Retention Period     | Durable Sessions 内の MQTT メッセージの保持期間を定義します。注意：このパラメータはグローバル設定です。 |
| `durable_sessions.batch_size`               | Message Query Batch Size     | Durable Sessions がストレージから消費するメッセージの最大バッチサイズを制御します。 |
| `durable_sessions.idle_poll_interval`       | Idle Poll Interval           | Durable Sessions が新しいメッセージをストレージに問い合わせる頻度を制御します。新しいメッセージが見つかれば、クライアントのインフライトキューに空きがある場合、次のバッチを即座に取得します。 |
| `durable_sessions.heartbeat_interval`       | Session Heartbeat Interval   | セッションメタデータを保存する間隔を指定します。               |
| `durable_sessions.renew_streams_interval`   | -                           | セッションが新しいストリームをストレージに問い合わせる頻度を定義します。 |
| `durable_sessions.session_gc_interval`      | Session GC Interval          | セッションをスイープし、有効期限切れのものを削除する間隔を指定します。 |
>>>>>>> origin/release-5.10

以下のパラメータは [ゾーン](../configuration/configuration.md#zone-override)ごとにオーバーライド可能です。

- `durable_sessions.enable`
- `durable_sessions.batch_size`
- `durable_sessions.checkpoint_interval`

### Durable Storageの設定

<<<<<<< HEAD
`<DS>` は「durable storage（耐久ストレージ）」を表すプレースホルダーです。現在、利用可能な `<DS>` のパラメータは `message` です。

#### コア Durable Storage パラメータ

| パラメータ                                 | 説明                                                         |
| ----------------------------------------- | ------------------------------------------------------------ |
| `durable_storage.n_sites`                 | [サイト数](./managing-replication.md#number-of-sites)を指定します。 |
| `durable_storage.<DS>.data_dir`           | EMQX がデータを保存するファイルシステム上のディレクトリです。    |
| `durable_storage.<DS>.n_shards`           | [シャード数](./managing-replication.md#number-of-shards)を指定します。 |
| `durable_storage.<DS>.replication_factor` | [レプリケーションファクター](./managing-replication.md#replication-factor)は各シャードのレプリカ数を決定します。 |
| `durable_storage.<DS>.transaction`        | メッセージバッファリングに関するパラメータを含みます。詳細は[バッファリング](#buffering)を参照してください。 |
| `durable_storage.<DS>.layout`             | EMQX がディスク上にデータを配置する方法を制御するパラメータを含みます。詳細は[ストレージレイアウト設定](#storage-layout-configuration)を参照してください。 |

#### データベースグループの設定

EMQX 6.0.2 以降、Durable Storage は [データベースグループ](../design/durable-storage.md/#durable-storage-database-groups)を導入し、ノードレベルのリソースガバナンスをサポートしています。データベースグループにより、複数の耐久ストレージデータベースを論理データモデルを変更せずにリソース制限を共有しながら一括管理できます。

デフォルトでは、各耐久ストレージデータベースは自身の名前を冠したデータベースグループに属し、そのグループには単一のデータベースのみが含まれ、以前のリリースの挙動を維持します。

データベースグループは `durable_storage.db_groups` ネームスペースで設定します。

| パラメータ                                                 | 説明                                                       |
| --------------------------------------------------------- | ---------------------------------------------------------- |
| `durable_storage.db_groups.<group>.storage_quota`         | グループの SST ファイル合計ディスク使用量のソフトクォータです。 |
| `durable_storage.db_groups.<group>.write_buffer_size`     | グループの RocksDB メモリテーブルの最大合計サイズです。    |
| `durable_storage.db_groups.<group>.rocksdb_nthreads_high` | 高優先度 RocksDB バックグラウンドスレッド数です。          |
| `durable_storage.db_groups.<group>.rocksdb_nthreads_low`  | 低優先度 RocksDB バックグラウンドスレッド数です。          |

#### バッファリング

EMQX はクライアントからの MQTT メッセージを耐久ストレージにバッチで書き込み、スループットを最大化します。バッチングは `durable_storage.<DS>.transaction` 設定サブツリーの以下のパラメータで制御します。

| パラメータ             | 説明                                                         |
| --------------------- | ------------------------------------------------------------ |
| `max_pending`         | 指定したメッセージ数に達したらバッファをフラッシュします。   |
| `flush_interval`      | 少なくとも1件のメッセージがあれば、指定時間間隔でバッファをフラッシュします。 |
| `idle_flush_interval` | 新しいメッセージがこの間隔内に到着しない場合、早期にバッファをフラッシュします。 |

#### ストレージレイアウト設定

ストレージレイアウトは EMQX がディスク上のデータをどのように整理するかを決定します。`durable_storage.<DS>.layout.type` パラメータを設定することで、新しい [世代](./durability_introduction.md#generation)で使用されるレイアウトを変更できます。この変更は既存の世代には影響しません。各レイアウトタイプの設定は `durable_storage.<DS>.layout` サブツリーに含まれます。現在利用可能なレイアウトタイプは `wildcard_optimized` のみです。

##### `wildcard_optimized` レイアウトタイプの設定

`wildcard_optimized` レイアウトは、多数の MQTT トピックにマッチするワイルドカードサブスクライブを最適化することを目的としています。トピック構造に関する知識を自律的に蓄積し、軽量な機械学習アルゴリズムを活用してクライアントがサブスクライブしそうなワイルドカードトピックフィルターを予測します。その後、これらのトピックを統合されたストリームに整理し、一度のスイープで効率的に消費できるようにします。

| パラメータ               | 説明                                                       |
| ----------------------- | ---------------------------------------------------------- |
| `bytes_per_topic_level` | トピックレベルのハッシュサイズを決定します。              |
| `topic_index_bytes`     | ストリーム識別子のバイトサイズを指定します。                |
=======
`<DS>` は「durable storage（耐久ストレージ）」を表すプレースホルダーです。現在、利用可能な `<DS>` のパラメータは `message` のみです。

| パラメータ                                 | 説明                                                         |
| ----------------------------------------- | ------------------------------------------------------------ |
| `durable_storage.<DS>.data_dir`           | EMQX がデータを保存するファイルシステム上のディレクトリ。    |
| `durable_storage.<DS>.n_shards`           | [シャード数](./managing-replication.md#number-of-shards)。    |
| `durable_storage.<DS>.n_sites`            | [サイト数](./managing-replication.md#number-of-sites)。       |
| `durable_storage.<DS>.replication_factor` | [レプリケーション係数](./managing-replication.md#replication-factor)。各シャードのレプリカ数を決定します。 |
| `durable_storage.<DS>.local_write_buffer` | メッセージのバッファリングに関するパラメータ。詳細は[ローカル書き込みバッファの設定](#local-write-buffer-configuration)を参照してください。 |
| `durable_storage.<DS>.layout`             | EMQX がディスク上にデータを配置する方法を制御するパラメータ。詳細は[ストレージレイアウトの設定](#storage-layout-configuration)を参照してください。 |

#### ローカル書き込みバッファ設定

EMQX はクライアントからの MQTT メッセージをバッチ単位で耐久ストレージに書き込み、スループットを最大化します。バッチ処理は `durable_storage.<DS>.layout` のサブツリーにある以下のパラメータで設定します。

| パラメータ        | 説明                                                         |
| ---------------- | ------------------------------------------------------------ |
| `max_items`      | バッファのサイズがこの値に達したときにフラッシュ（書き込み）されます。 |
| `flush_interval` | バッファに少なくとも1件のメッセージがある場合、この間隔でバッファがフラッシュされます。 |

#### ストレージレイアウト設定

ストレージレイアウトは EMQX がディスク上のデータをどのように整理するかを決定します。`durable_storage.<DS>.layout.type` パラメータを設定すると、新しい [世代](./durability_introduction.html#generation)で使用されるレイアウトを変更できます。この変更は既存の世代には影響しません。各レイアウトタイプの設定は `durable_storage.<DS>.layout` サブツリーに含まれます。現在は `wildcard_optimized` レイアウトタイプが利用可能です。

##### `wildcard_optimized` レイアウトタイプの設定

`wildcard_optimized` レイアウトは、多数の MQTT トピックに対するワイルドカードサブスクライブのマッチングを最適化することを目的としています。トピック構造に関する知識を時間とともに自律的に蓄積し、軽量な機械学習アルゴリズムを活用してクライアントがサブスクライブしそうなワイルドカードトピックフィルターを予測します。その後、これらのトピックを統合されたストリームに整理し、一度のスイープで効率的に消費できるようにします。

| パラメータ              | 説明                                                         |
| ---------------------- | ------------------------------------------------------------ |
| `bits_per_topic_level` | トピックレベルハッシュのサイズを決定します。                |
| `epoch_bits`           | メッセージタイムスタンプ（マイクロ秒単位）の下位ビットを使って計算されるエポック内のメッセージオフセットのビット数を定義します。 |
| `topic_index_bytes`    | ストリーム識別子のバイト数を指定します。                    |

**エポック設定**

ワイルドカード最適化ストリームはエポックと呼ばれる時間区間に分割されます。各エポック内のメッセージは一度のスイープで処理できるため、効率とスループットが向上します。ただし、エポックが大きいと、現在のエポックのメッセージが即座に消費できずレイテンシが増加します。

エポックの時間間隔は以下の式で計算できます：`epoch length (μs) = 2 ^ epoch_bits`

| Epoch Bits | エポック長さ     |
| ---------- | ---------------- |
| 1          | 2 μs             |
| 2          | 4 μs             |
| 10         | 約1 ms           |
| 17         | 約100 ms         |
| 20         | 約1 秒           |
| 21         | 約2 秒           |
| 24         | 約17 秒          |

デフォルトでは `epoch_bits` は 20（約1秒）に設定されており、レイテンシと効率のバランスを取っています。この値を調整することで、レイテンシとスループットのトレードオフを微調整できます。
>>>>>>> origin/release-5.10

## CLIコマンド

<<<<<<< HEAD
耐久ストレージの管理に利用可能な CLI コマンドは以下の通りです。
=======
耐久ストレージの管理に使用できるCLIコマンドは以下の通りです。
>>>>>>> origin/release-5.10

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

このコマンドの出力には以下が含まれます：

- `THIS SITE`：ローカル EMQX ノードが管理するサイトの ID。
<<<<<<< HEAD
- `SITES`：既知のすべてのサイトの一覧。EMQX ノード名とそのステータスを含みます。
- `SHARDS`：耐久ストレージのシャード一覧と、それらのレプリカが配置されているサイト ID。

### `emqx ctl ds set-replicas all <site1> <site2> ...`

クラスタ内の耐久ストレージのレプリカを保持するサイトのリストを設定します。実行すると、シャードをサイト間で公平に割り当てる操作計画を作成し、バックグラウンドで実行を継続します。

::: warning 重要なお知らせ
耐久ストレージのレプリカリストの更新は、サイト間で大量のデータコピーを伴うためコストが高くなる可能性があります。
=======
- `SITES`：既知の全サイトのリスト、EMQX ノード名とステータスを含む。
- `SHARDS`：耐久ストレージのシャードと、そのレプリカが存在するサイト ID のリスト。

### `emqx_ctl ds set_replicas <storage> <site1> <site2> ...`

クラスタ内の耐久ストレージのレプリカを保持するサイトのリストを設定します。実行すると、シャードをサイト間で公平に割り当てる操作計画を作成し、バックグラウンドで実行を続けます。

::: warning 重要なお知らせ
耐久ストレージのレプリカリストを更新すると、大量のデータをサイト間でコピーする可能性があるため、コストがかかる場合があります。
>>>>>>> origin/release-5.10
:::

例：

```bash
<<<<<<< HEAD
$ emqx ctl ds set-replicas all 5C6028D6CE9459C7 D8894F95DC86DFDB F4E92DEA197C8EBC
=======
$ emqx ctl ds set-replicas messages 5C6028D6CE9459C7 D8894F95DC86DFDB F4E92DEA197C8EBC
>>>>>>> origin/release-5.10
ok
```

このコマンド実行後、`ds info` の出力例：

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

新たに追加された `REPLICA TRANSITIONS` セクションは保留中の操作を示します。すべての操作が完了するとこのリストは空になります。

<<<<<<< HEAD
### `emqx ctl ds join all <site>` / `emqx ctl ds leave all <site>`
=======
### `emqx_ctl ds join <storage> <site>` / `emqx_ctl ds leave <storage> <site>`
>>>>>>> origin/release-5.10

これらのコマンドは、耐久ストレージのレプリカサイトリストにサイトを追加または削除します。`set_replicas` コマンドと似ていますが、1サイトずつ更新します。

例：

```bash
<<<<<<< HEAD
$ emqx ctl ds join all B2A7DBB2413CD6EE
=======
$ emqx ctl ds join messages B2A7DBB2413CD6EE
>>>>>>> origin/release-5.10
ok
```

詳細は[サイトの追加](./managing-replication.md#add-sites)および[サイトの削除](./managing-replication.md#remove-sites)を参照してください。

## REST API

<<<<<<< HEAD
組み込み Durable Sessions の管理および監視に利用可能な REST API エンドポイントは以下の通りです。

- `/ds/sites`：既知のサイト一覧を取得します。
- `/ds/sites/:site`：サイトの情報（ステータス、現在管理している EMQX ノード名など）を取得します。
- `/ds/storages`：耐久ストレージ一覧を取得します。
=======
組み込みの Durable Sessions の管理および監視に利用できる REST API エンドポイントは以下の通りです。

- `/ds/sites`：既知のサイト一覧を取得します。
- `/ds/sites/:site`：サイトの情報（ステータス、現在そのサイトを管理している EMQX ノード名など）を取得します。
- `/ds/storages`：耐久ストレージの一覧を取得します。
>>>>>>> origin/release-5.10
- `/ds/storages/:ds`：耐久ストレージおよびそのシャードの情報を取得します。
- `/ds/storages/:ds/replicas`：耐久ストレージのレプリカを保持するサイトの一覧取得および更新を行います。
- `/ds/storages/:ds/replicas/:site`：特定サイトの耐久ストレージレプリカの追加または削除を行います。

詳細はEMQX OpenAPIスキーマを参照してください。

## メトリクス

Durable Sessionsに関連するPrometheusメトリクスは以下の通りです。

### `emqx_ds_egress_batches`

耐久ストレージへのメッセージバッチ書き込みが成功するたびにインクリメントされます。

### `emqx_ds_egress_messages`

<<<<<<< HEAD
耐久ストレージに正常に書き込まれたメッセージ数をカウントします。

### `emqx_ds_egress_bytes`

耐久ストレージに正常に書き込まれたペイロードデータの総量をカウントします。注意：このメトリクスはメッセージのペイロードのみを対象としているため、実際の書き込みデータ量はこれより多くなる可能性があります。

### `emqx_ds_egress_batches_failed`

耐久ストレージへの書き込みが何らかの理由で失敗した場合にインクリメントされます。
=======
耐久ストレージへのメッセージ書き込み成功数をカウントします。

### `emqx_ds_egress_bytes`

耐久ストレージに正常に書き込まれたペイロードデータの総量をカウントします。注意：このメトリクスはメッセージのペイロードのみを対象としているため、実際の書き込みデータ量はこれより大きい場合があります。

### `emqx_ds_egress_batches_failed`

耐久ストレージへの書き込みが何らかの理由で失敗した際にインクリメントされます。
>>>>>>> origin/release-5.10

### `emqx_ds_egress_flush_time`

耐久ストレージへのバッチ書き込みに費やした時間（μs）のローリング平均です。レプリケーション速度の重要な指標です。

### `emqx_ds_store_batch_time`

<<<<<<< HEAD
ローカル RocksDB ストレージへのバッチ書き込みに費やした時間（μs）のローリング平均です。`emqx_ds_egress_flush_time` と異なり、ネットワークレプリケーションコストを除外しているため、ディスク I/O 効率の重要な指標となります。
=======
ローカルの RocksDB ストレージへのバッチ書き込みにかかる時間（μs）のローリング平均です。`emqx_ds_egress_flush_time` と異なり、ネットワークレプリケーションコストを除外しているため、ディスク I/O 効率の重要な指標となります。
>>>>>>> origin/release-5.10

### `emqx_ds_builtin_next_time`

耐久ストレージからメッセージバッチを消費するのに費やした時間（μs）のローリング平均です。

### `emqx_ds_storage_bitfield_lts_counter_seek` および `emqx_ds_storage_bitfield_lts_counter_next`

これらのカウンターは「wildcard optimized」ストレージレイアウト固有のもので、ローカルストレージからのデータ消費効率を測定します。`seek` 操作は一般的に遅いため、`emqx_ds_storage_bitfield_lts_counter_next` の増加率が `seek` より速いことが望ましいです。

`durable_storage.messages.layout.epoch_bits` パラメータを増やすことで、この比率を改善できます。
<<<<<<< HEAD

### `emqx_ds_raft_db_shards_num`

DB が分割されているシャード数です。

### `emqx_ds_raft_db_sites_num`

DS DB がレプリケートされている現在および割り当てられたサイト数を追跡するゲージです。

通常、現在のサイト数は割り当てられたサイト数と同じです。長期間異なる場合は、レプリカ転送に問題がある可能性があります。

### `emqx_ds_raft_shard_replication_factor`

DS DB シャードのレプリカセット内のレプリカ数を追跡します。

この数が設定されたレプリケーションファクターより少ない場合、耐久性が危険にさらされています。より多くのサイトにレプリカを再配置することを検討してください。

### `emqx_ds_raft_db_shards_online_num`

このノードでアクティブに管理されている DS DB シャード数を追跡します。

この数は現在このノードに割り当てられているシャード数と一致する必要があります。そうでない場合、可用性に問題がある可能性があります。ログを確認してください。

### `emqx_ds_raft_shard_transition_queue_len`

DS DB シャードの保留中のレプリカセット遷移数を追跡します。

この数が長期間ゼロでない場合、レプリカ転送に問題があります。

### `emqx_ds_raft_shard_transitions`

DB シャードのレプリカセット遷移の開始／完了／スキップ／クラッシュ数をカウントします。

クラッシュした遷移は常にゼロであるべきです。そうでない場合はログを確認してください。

### `emqx_ds_raft_shard_transition_errors`

DB シャードのレプリカセット遷移オーケストレーション中に発生した一時的エラー数をカウントします。

このカウンターが増加する場合、レプリカ転送に問題があります。ログを確認してください。

### `emqx_ds_raft_snapshot_reads`

シャードがスナップショットレプリケーションのソースであった際のスナップショット読み取りの開始／完了数をカウントします。

### `emqx_ds_raft_snapshot_read_errors`

ソース DS DB シャードでのスナップショット読み取り中に発生し、スナップショットレプリケーションが中断されたエラー数をカウントします。

エラーは想定されていません。原因はログを確認してください。

### `emqx_ds_raft_snapshot_read_chunks`

スナップショット転送のソース DS DB シャードで読み取られ、受信先に転送されたチャンク数をカウントします。

### `emqx_ds_raft_snapshot_read_chunk_bytes`

ソース DS DB シャードでチャンクとして読み取られたバイト数をカウントします。

### `emqx_ds_raft_snapshot_writes`

シャードがスナップショットレプリケーションの受信先であった際のスナップショット書き込みの開始／完了数をカウントします。

### `emqx_ds_raft_snapshot_write_errors`

受信先 DS DB シャードへのスナップショット書き込み中に発生し、スナップショットレプリケーションが中断されたエラー数をカウントします。

これも増加は想定されていません。詳細はログを確認してください。

### `emqx_ds_raft_snapshot_write_chunks`

ソース DS DB シャードから受信し受信先に書き込まれたチャンク数をカウントします。

### `emqx_ds_raft_snapshot_write_chunk_bytes`

受信先 DS DB シャードでチャンクとして書き込まれたバイト数をカウントします。

### `emqx_ds_raft_current_timestamp_us`

シャードサーバーが現在レプリケートしている最新の操作タイムスタンプ（マイクロ秒単位）を追跡します。

通常、各レプリカは同じタイムスタンプを持つべきです。異なる場合はレプリケーションに問題があります。

### `emqx_ds_raft_rasrv_state_changes`

Raft サーバーが候補者／フォロワー／リーダーに変わった回数をカウントします。

頻繁な状態変化は不安定の兆候です。ログを確認してください。

### データベースグループメトリクス

以下の Prometheus メトリクスは、ノードレベルでの耐久ストレージデータベースグループの可視化を提供します。

#### `emqx_ds_disk_usage`

グループ内のすべてのデータベースが使用する SST ファイルの合計サイズ。

#### `emqx_ds_write_buffer_memory_usage`

グループが使用する RocksDB メモリテーブルの合計メモリ使用量。

#### `emqx_ds_total_trash_size`

削除待ちの不要な SST ファイルのディスク使用量。

これらのメトリクスはノード単位およびデータベースグループ単位で報告されます。クラスタ環境では、運用者が外部で集約してクラスタ全体の容量を評価することが可能です。
=======
>>>>>>> origin/release-5.10
