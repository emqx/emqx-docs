# Durable Sessions の設定と管理

本ドキュメントでは、EMQX における [MQTT Durable Sessions](./durability_introduction.md) 機能の設定、管理、および最適化に関するリファレンスと手順を提供します。セッションおよびストレージの設定も含みます。

## 設定パラメータ

MQTT Durable Sessions の設定は大きく2つのカテゴリに分かれます。

- `durable_sessions`：MQTT クライアントのセッションに関する設定で、耐久ストレージからのデータ消費方法やデータ保持パラメータを含みます。
- `durable_storage`：MQTT メッセージデータを保持する耐久ストレージシステムの設定を管理します。

### Durable Sessions の設定

Dashboard で durable sessions のパラメータを設定できます。Dashboard 左メニューの **Management** -> **MQTT Settings** をクリックし、**Durable Session** タブを選択してパラメータを設定してください。

<img src="./assets/dashboard_session_config.png" alt="ダッシュボードのセッション設定" style="zoom:67%;" />

| パラメータ                                   | Dashboard UI 表示名        | 説明                                                         |
| ------------------------------------------- | -------------------------- | ------------------------------------------------------------ |
| `durable_sessions.enable`                   | Enable Durable Sessions    | セッションの耐久性を有効化します。この設定はホットコンフィグレーションでは変更できず、設定ファイルで設定する必要があります。変更を反映するには EMQX ノードの再起動が必要です。 |
| `durable_sessions.message_retention_period` | Message Retention Period   | Durable Sessions 内の MQTT メッセージの保持期間を定義します。注意：このパラメータはグローバル設定です。 |
| `durable_sessions.batch_size`               | Message Query Batch Size   | Durable Sessions がストレージから消費するメッセージの最大バッチサイズを制御します。 |
| `durable_sessions.idle_poll_interval`       | Idle Poll Interval         | Durable Sessions が新しいメッセージをストレージに問い合わせる頻度を制御します。新しいメッセージが見つかると、クライアントのインフライトキューに空きがあれば次のバッチを即座に取得します。 |
| `durable_sessions.heartbeat_interval`       | Session Heartbeat Interval | セッションメタデータを保存する間隔を指定します。              |
| `durable_sessions.renew_streams_interval`   | -                          | セッションがストレージに新しいストリームを問い合わせる頻度を定義します。 |
| `durable_sessions.session_gc_interval`      | Session GC Interval        | セッションをスキャンし、期限切れのものを削除する間隔を指定します。 |

以下のパラメータは [ゾーンごとにオーバーライド可能](../configuration/configuration.md#zone-override)です。

- `durable_sessions.enable`
- `durable_sessions.batch_size`
- `durable_sessions.idle_poll_interval`
- `durable_sessions.renew_streams_interval`

### Durable Storage の設定

`<DS>` は "durable storage" を表すプレースホルダーです。現在、利用可能な `<DS>` のパラメータは `message` です。

| パラメータ                                 | 説明                                                         |
| ----------------------------------------- | ------------------------------------------------------------ |
| `durable_storage.<DS>.data_dir`           | EMQX がデータを保存するファイルシステム上のディレクトリ。    |
| `durable_storage.<DS>.n_shards`           | [シャード数](./managing-replication.md#number-of-shards)。    |
| `durable_storage.<DS>.n_sites`            | [サイト数](./managing-replication.md#number-of-sites)。       |
| `durable_storage.<DS>.replication_factor` | [レプリケーション係数](./managing-replication.md#replication-factor)で、各シャードのレプリカ数を決定します。 |
| `durable_storage.<DS>.local_write_buffer` | メッセージバッファリングに関するパラメータを含みます。詳細は[ローカル書き込みバッファの設定](#local-write-buffer-configuration)を参照してください。 |
| `durable_storage.<DS>.layout`             | EMQX がディスク上にデータを配置する方法を制御するパラメータを含みます。詳細は[ストレージレイアウトの設定](#storage-layout-configuration)を参照してください。 |

#### ローカル書き込みバッファの設定

EMQX はクライアントからの MQTT メッセージをバッチで耐久ストレージに書き込み、スループットを最大化します。バッチ処理は `durable_storage.<DS>.layout` 設定サブツリーの以下のパラメータで制御されます。

| パラメータ        | 説明                                                         |
| ---------------- | ------------------------------------------------------------ |
| `max_items`      | バッファのサイズがこの値に達した時点でフラッシュ（書き込み）されます。 |
| `flush_interval` | バッファに少なくとも1件のメッセージがある場合、この間隔で定期的にフラッシュされます。 |

#### ストレージレイアウトの設定

ストレージレイアウトは EMQX がディスク上にデータをどのように配置するかを決定します。`durable_storage.<DS>.layout.type` パラメータを設定することで、新しい [世代](./durability_introduction.html#generation)で使用されるレイアウトを変更できます。この変更は既存の世代には影響しません。各レイアウトタイプの設定は `durable_storage.<DS>.layout` サブツリーに含まれます。現在利用可能なレイアウトタイプは `wildcard_optimized` です。

##### `wildcard_optimized` レイアウトタイプの設定

`wildcard_optimized` レイアウトは、多数の MQTT トピックに対するワイルドカードサブスクリプションのマッチングを最適化することを目的としています。時間経過でトピック構造に関する知識を自律的に蓄積し、軽量な機械学習アルゴリズムを活用してクライアントがサブスクライブしそうなワイルドカードトピックフィルターを予測します。その後、これらのトピックを統合されたストリームにまとめ、効率的に一括消費できるようにします。

| パラメータ              | 説明                                                         |
| ---------------------- | ------------------------------------------------------------ |
| `bits_per_topic_level` | トピックレベルのハッシュサイズを決定します。               |
| `epoch_bits`           | メッセージタイムスタンプ（マイクロ秒）の下位ビットを用いて計算されるエポック内のメッセージオフセットのビット数を定義します。 |
| `topic_index_bytes`    | ストリーム識別子のバイト数を指定します。                     |

**エポックの設定**

ワイルドカード最適化ストリームはエポックと呼ばれる時間区間に分割されます。各エポック内のメッセージは一括で処理可能であり、効率とスループットを向上させます。ただし、エポックが大きいと、現在のエポックのメッセージを即座に消費できずレイテンシが増加します。

各エポックの時間間隔は以下の式で計算できます：`epoch length (μs) = 2 ^ epoch_bits`

| Epoch Bits | エポック長さ    |
| ---------- | -------------- |
| 1          | 2 μs           |
| 2          | 4 μs           |
| 10         | 約1 ms         |
| 17         | 約100 ms       |
| 20         | 約1 秒         |
| 21         | 約2 秒         |
| 24         | 約17 秒        |

デフォルトでは `epoch_bits` は 20（約1秒）に設定されており、レイテンシと効率のバランスを取っています。この値を調整することで、レイテンシとスループットのトレードオフを細かく制御できます。

## CLI コマンド

耐久ストレージの管理に利用できる CLI コマンドは以下の通りです。

### `emqx_ctl ds info`

耐久ストレージの状態概要を表示します。

例：

```bash
$ emqx_ctl ds info

THIS SITE:
D8894F95DC86DFDB

SITES:
5C6028D6CE9459C7    'emqx@n2.local'        up
D8894F95DC86DFDB    'emqx@n1.local'        up
F4E92DEA197C8EBC    'emqx@n3.local'    (x) down

SHARDS:
Shard                             Replicas
messages/0                        5C6028D6CE9459C7
messages/1                        5C6028D6CE9459C7
messages/10                       5C6028D6CE9459C7
messages/11                       5C6028D6CE9459C7
messages/2                        5C6028D6CE9459C7
messages/3                        5C6028D6CE9459C7
messages/4                        5C6028D6CE9459C7
messages/5                        5C6028D6CE9459C7
messages/6                        5C6028D6CE9459C7
messages/7                        5C6028D6CE9459C7
messages/8                        5C6028D6CE9459C7
messages/9                        5C6028D6CE9459C7
```

このコマンド出力には以下が含まれます：

- `THIS SITE`：ローカル EMQX ノードが管理するサイトの ID。
- `SITES`：既知の全サイトのリスト。EMQX ノード名と状態を含みます。
- `SHARDS`：耐久ストレージのシャードと、そのレプリカが存在するサイト ID のリスト。

### `emqx_ctl ds set_replicas <DS> <Site1> <Site2> ...`

クラスタ内で耐久ストレージのレプリカを保持するサイトのリストを設定します。実行すると、シャードをサイト間で公平に割り当てる操作計画を作成し、バックグラウンドで実行を開始します。

::: warning 重要なお知らせ
耐久ストレージのレプリカリストの更新は、大量のデータをサイト間でコピーする可能性があるためコストがかかる場合があります。
:::

例：

```bash
$ emqx_ctl ds set_replicas messages 5C6028D6CE9459C7 D8894F95DC86DFDB F4E92DEA197C8EBC
ok
```

このコマンド実行後、`ds info` の出力は以下のようになる場合があります。

```bash
$ emqx_ctl ds info

THIS SITE:
D8894F95DC86DFDB

SITES:
5C6028D6CE9459C7    'emqx@n2.local'        up
D8894F95DC86DFDB    'emqx@n1.local'        up
F4E92DEA197C8EBC    'emqx@n3.local'        up

SHARDS:
Shard                             Replicas
messages/0                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/1                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/10                       5C6028D6CE9459C7
messages/11                       5C6028D6CE9459C7      D8894F95DC86DFDB
messages/2                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/3                        5C6028D6CE9459C7
messages/4                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/5                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/6                        5C6028D6CE9459C7
messages/7                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/8                        5C6028D6CE9459C7      D8894F95DC86DFDB
messages/9                        5C6028D6CE9459C7

REPLICA TRANSITIONS:
Shard                         Transitions
messages/0                    +F4E92DEA197C8EBC
messages/1                    +F4E92DEA197C8EBC
messages/10                   +F4E92DEA197C8EBC  +D8894F95DC86DFDB
messages/11                   +F4E92DEA197C8EBC
messages/2                    +F4E92DEA197C8EBC
messages/3                    +F4E92DEA197C8EBC  +D8894F95DC86DFDB
messages/4                    +F4E92DEA197C8EBC
messages/5                    +F4E92DEA197C8EBC
messages/6                    +F4E92DEA197C8EBC  +D8894F95DC86DFDB
messages/7                    +F4E92DEA197C8EBC
messages/8                    +F4E92DEA197C8EBC
messages/9                    +F4E92DEA197C8EBC  +D8894F95DC86DFDB
```

新たに追加された `REPLICA TRANSITIONS` セクションは保留中の操作を示します。すべての操作が完了すると、このリストは空になります。

### `emqx_ctl ds join <DS> <Site>` / `emqx_ctl ds leave <DS> <Site>`

これらのコマンドは、耐久ストレージのレプリカサイトリストにサイトを追加または削除します。`set_replicas` コマンドと似ていますが、一度に1サイトずつ更新します。

例：

```bash
$ bin/emqx_ctl ds join messages B2A7DBB2413CD6EE
ok
```

詳細は [Add Sites](./managing-replication.md#add-sites) および [Remove Sites](./managing-replication.md#remove-sites) を参照してください。

## REST API

組み込みの durable sessions の管理および監視に利用できる REST API エンドポイントは以下の通りです。

- `/ds/sites`：既知のサイト一覧を取得します。
- `/ds/sites/:site`：サイトの情報（状態、現在管理している EMQX ノード名など）を取得します。
- `/ds/storages`：耐久ストレージ一覧を取得します。
- `/ds/storages/:ds`：耐久ストレージおよびそのシャードの情報を取得します。
- `/ds/storages/:ds/replicas`：耐久ストレージのレプリカを保持するサイトの一覧取得および更新。
- `/ds/storages/:ds/replicas/:site`：特定サイトの耐久ストレージレプリカの追加または削除。

詳細は EMQX OpenAPI スキーマを参照してください。

## メトリクス

Durable Sessions に関連する Prometheus メトリクスは以下の通りです。

### `emqx_ds_egress_batches`

メッセージのバッチが耐久ストレージに正常に書き込まれるたびにインクリメントされます。

### `emqx_ds_egress_messages`

耐久ストレージに正常に書き込まれたメッセージ数をカウントします。

### `emqx_ds_egress_bytes`

耐久ストレージに正常に書き込まれたペイロードデータの総量をカウントします。注意：このメトリクスはメッセージのペイロードのみを対象としているため、実際の書き込みデータ量はこれより多い場合があります。

### `emqx_ds_egress_batches_failed`

耐久ストレージへの書き込みが何らかの理由で失敗するたびにインクリメントされます。

### `emqx_ds_egress_flush_time`

耐久ストレージへのバッチ書き込みにかかる時間（μs）のローリング平均です。レプリケーション速度の重要な指標です。

### `emqx_ds_store_batch_time`

ローカルの RocksDB ストレージへのバッチ書き込みにかかる時間（μs）のローリング平均です。`emqx_ds_egress_flush_time` と異なりネットワークレプリケーションコストを含まないため、ディスク I/O 効率の重要な指標となります。

### `emqx_ds_builtin_next_time`

耐久ストレージからメッセージのバッチを消費するのにかかる時間（μs）のローリング平均です。

### `emqx_ds_storage_bitfield_lts_counter_seek` と `emqx_ds_storage_bitfield_lts_counter_next`

これらのカウンターは "wildcard optimized" ストレージレイアウト固有のもので、ローカルストレージからのデータ消費効率を測定します。`seek` 操作は一般的に遅いため、`emqx_ds_storage_bitfield_lts_counter_next` の増加率が `seek` より速いことが理想的です。

`durable_storage.messages.layout.epoch_bits` パラメータを増やすことで、この比率の改善に寄与します。
