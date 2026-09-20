# メッセージキュー ユーザーガイド

このページでは、EMQXのメッセージキュー機能の実践的な使い方について、キューの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## ダッシュボードからキューを手動で作成する

メッセージキューは、メッセージを格納・配信する前に明示的に宣言／作成する必要があります。キューは手動または自動で作成できます。自動作成の詳細は[ダッシュボードからメッセージキューを自動作成する](#automatically-create-message-queues-via-dashboard)をご覧ください。

1. 左メニューの **Queues** に移動します。

2. ページ上の **Create** ボタンをクリックします。

3. **Create Queue** ダイアログで以下のオプションを設定します：

   - **Name**：キューの一意な名前を指定します。キュー名には以下の文字のみ使用可能です：

     - 英数字（`A–Z`、`a–z`、`0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でキューは識別・管理されます。

     クライアントは以下のサブスクリプション形式でメッセージを消費できます：

     - キューが既に存在する場合は `$queue/<name>` を使用します。
     - キューが既に存在する場合にオプションで `$queue/<name>/<topic_filter>` を使用できます。自動作成が有効な場合は、キューが存在しないときに `<topic_filter>` を使ってEMQXが自動的にキューを作成します。

   - **Topic Filter**：トピックまたはトピックフィルター（例：`t/1`）を入力します。これはパブリッシュされたメッセージのトピックとマッチするものをキューに格納するための条件です。キューはこのトピックフィルターにマッチするすべてのメッセージを収集します。

     > トピックフィルターはキューの設定の一部ですが、キューの識別子ではありません。

   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法を選択します。利用可能な戦略は以下の通りです：

     - `Least Inflight Subscriber`：未アックのメッセージが最も少ないサブスクライバーを優先します。
     - `Random`：（デフォルト）ランダムにサブスクライバーを選択します。
     - `Round Robin`：すべてのサブスクライバーに均等に順番に配信します。

   - **Data Retention Period**：メッセージをキュー内に保持する期間を指定します。時間単位（例：日）を設定可能です。

   - **Last Value Semantics**：デフォルトで有効です。有効時は同じキューキーを持つ新しいメッセージが、同じキュー内の未消費の旧メッセージを上書きします。これによりキーごとに最新のメッセージのみが保持されます。デフォルトのキーはメッセージパブリッシャーのクライアントIDです。キューキーの設定例は以下をご参照ください。

     - **[Queue Key Expression](#queue-key-expression)**：Last Value Semanticsが有効な場合、このフィールドで各メッセージからキーを抽出する式を定義します。デフォルトは `message.from`（メッセージパブリッシャーのクライアントID）です。このフィールドは[Variform式](../../guides/configuration/configuration.md#variform-expressions)で設定可能です。

   - **Max Shard Message Count**：（任意）キューの各シャードに許容される最大メッセージ数を設定します。この設定を有効にしてカスタム値を入力するか、無効にして無制限（`infinity`）にできます。この設定は永続ストレージに保存されます。

   - **Max Shard Message Bytes**：（任意）キューの各シャードに許容されるメッセージの合計サイズ（バイト単位）を設定します。この設定を有効にして値（例：`200MB`）を入力するか、無効にして無制限（`infinity`）にできます。この設定も永続ストレージに保存されます。

     ::: tip パフォーマンスに関する注意

     サイズ制限付きのキューは、特に高スループット時に書き込み性能が低下する可能性があります。

     :::

4. **Create** をクリックしてキューを保存します。

新しいキューはキュー一覧に表示され、名前、トピックフィルター、配信戦略、Last Value Semanticsの状態、データ保持期間が確認できます。キューの設定編集や削除は **Actions** 列のボタンから行えます。

## Queue Key Expression

Queue Key Expressionは、Last Value Semanticsモードでメッセージの重複排除に使うキーを抽出する方法を指定します。この式はメッセージのデータに対して評価され、[Variform式](../../guides/configuration/configuration.md#variform-expressions)の構文に従います。

式は、`from`、`topic`、`payload`、`headers.properties`などのフィールドを含むメッセージコンテキストに対して評価されます。例えば、ユーザープロパティをキーに使う場合は以下のように設定できます：

```
message.headers.properties.User-Property.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しない）、メッセージは破棄され、キューに格納されません。

### メッセージコンテキストの例

<!--@include: ../shared/key-expression-message-context.md-->

### Queue Key Expressionの例

#### 例1

以下の条件でキューを設定したとします：

- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`message.headers.properties.User-Property.mq-key`

以下のメッセージがEMQXにパブリッシュされ（クライアントは存在しないものとする）：
| No | 送信元 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 1  | `client1` | `t/1` | `keyA` |
| 2  | `client1` | `t/2` | `keyB` |
| 3  | `client2` | `t/3` | `keyA` |
| 4  | `client2` | `t/4` | `keyB` |

クライアントが接続してキューにサブスクライブすると、以下のメッセージが配信されます：
| No | 送信元 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 3  | `client2` | `t/3` | `keyA` |
| 4  | `client2` | `t/4` | `keyB` |

同じ `message.headers.properties.User-Property.mq-key` の値ごとに最新のメッセージのみがキューに保持されます。キー式はトピックを跨いでキュー全体に適用されるため、`keyA` のメッセージは `t/1` から後の `t/3` のメッセージで上書きされます。

#### 例2

以下の条件でキューを設定したとします：

- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`message.from`

例1と同じメッセージがパブリッシュされた場合、クライアントが接続してキューにサブスクライブすると、以下のメッセージが配信されます：
| No | 送信元 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 2  | `client1` | `t/2` | `keyB` |
| 4  | `client2` | `t/4` | `keyB` |

同じ `message.from` の値を持つメッセージは上書きされるため、送信元ごとに最新のメッセージのみが保持されます。

#### 例3

以下の条件でキューを設定したとします：

- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`concat(message.headers.properties.User-Property.mq-key, '-', message.topic)`

以下のメッセージがEMQXにパブリッシュされました：
| No | 送信元 | トピック | ユーザープロパティ `mq-key` | 計算されたキー |
|----|--------|----------|-----------------------------|----------------|
| 1  | `client1` | `t/1` | `keyA` | `keyA-t/1` |
| 2  | `client1` | `t/2` | `keyB` | `keyB-t/2` |
| 3  | `client1` | `t/1` | `keyB` | `keyB-t/1` |
| 4  | `client1` | `t/2` | `keyA` | `keyA-t/2` |

クライアントが接続してキューにサブスクライブすると、すべてのメッセージが配信されます。これは `message.headers.properties.User-Property.mq-key` と `message.topic` の組み合わせが各メッセージでユニークだからです。

## ダッシュボードからキューを自動作成する

クライアントが `$queue/` プレフィックス付きトピックにサブスクライブすると、メッセージキューを自動的に作成できます。これにより手動設定なしでキューを動的にプロビジョニング可能です。

自動作成が有効な場合：

- `$queue/<name>` へのサブスクライブはキューが既に存在する場合のみ機能します。
- `$queue/<name>/<topic_filter>` へのサブスクライブは、キューが存在しない場合に `<topic_filter>` を使ってEMQXが自動的にキューを作成します。

キューは通常のキューまたはLast Value Semanticsキューとして自動作成できます。

::: tip 注意

適切なキュー動作を保証するために、**Auto Create Regular Queue** と **Auto Create Last Value Semantics Queue** は同時に有効にしないでください。

:::

### Last Value Semanticsキューの自動作成

このオプションはデフォルトで **Management** -> **MQTT Settings** -> **Queues** タブの **Enable Auto Create Queue** -> **Last Value Semantics Queue** で有効になっています。これにより、キーごとに最新のメッセージのみを保持するLast Value SemanticsキューをEMQXが自動作成します。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. デフォルトで **Enable Auto Create Queue** -> **Last Value Semantics Queue** が有効です。

   以下を設定します：

   - **Queue Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。
   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージをキューに保持する期間。

3. **Save Changes** をクリックします。

クライアントが `$queue/my_queue/test` のようなトピックにサブスクライブすると、`my_queue` が存在しない場合はEMQXが自動的にLast Value Semanticsキューを作成し、`test` をトピックフィルターとして使用します。キューは **Queues** 一覧に表示されます。

### 通常キューの自動作成

メッセージを上書きせず独立して保存する通常キューを自動作成したい場合は、このオプションを手動で有効にできます。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. **Enable Auto Create Queue** -> **Regular Queue** をオンにします。

3. 以下を設定します：

   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージをキューに保持する期間。

4. **Save Changes** をクリックします。

## キュー設定の構成

このセクションでは、EMQX内のすべてのメッセージキューに適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージ保持、クリーンアップ間隔、内部キュー動作、自動作成動作を制御します。ダッシュボード、REST API、設定ファイルで設定可能です。

### ダッシュボード

EMQXダッシュボードからメッセージキューの設定を直接更新でき、ブローカーの再起動は不要です。システム全体の動作をランタイムで変更する際に便利です。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

   または、**Queues** ページ右上の **Settings** ボタンをクリックします。

2. **Queues** パネルで以下の設定が可能です：

   - **Enable Queues**：メッセージキュー機能を有効化します。

     > ダッシュボードからはキュー機能を無効化できません。無効化する場合は設定ファイルを直接編集してください。

   - **Max Queue Count**：作成可能なキューの最大数を設定します。

   - **GC Interval**：期限切れメッセージをキューからクリーンアップする間隔。デフォルトは `1` 時間です。

   - **Regular Queue Retention Period**：通常キューでメッセージを保持する最大期間。デフォルトは `7` 日です。

   - **Find Queue Retry Interval**：クライアントが `$queue/<name>` にサブスクライブしキューが見つからない場合、再試行する間隔。デフォルトは `10` 秒です。

   - **Enable Auto Create Queue**：クライアントがキュートピックにサブスクライブし、該当キューが存在しない場合に自動作成を有効化します。

   - **Auto Create Queue Type**：自動作成するキューのタイプを指定します：

     - **Last Value Semantics Queue**（デフォルト有効）：`$queue/<name>/<topic_filter>` にサブスクライブし該当キューがない場合、Last Value Semanticsキューを自動作成します。

       詳細は[Last Value Semanticsキューの自動作成](#auto-create-last-value-semantics-queues)をご覧ください。

     - **Regular Queue**：有効にすると、`$queue/<name>/<topic_filter>` のサブスクライブ時に通常キューを自動作成します。

       詳細は[通常キューの自動作成](#auto-create-regular-queues)をご覧ください。

3. 設定変更後、**Save Changes** をクリックして適用します。

### REST API

REST APIを使ってグローバルなメッセージキュー設定を構成できます。これらの設定はシステム全体に適用され、すべてのキューの内部管理に影響します。

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### 設定ファイル

永続的かつバージョン管理可能な設定には、EMQX設定ファイル（`emqx.conf`）にメッセージキュー設定を定義できます。主要な設定例は以下の通りです：

```hocon
mq {
    gc_interval = 1h
    regular_queue_retention_period = 1d
    find_queue_retry_interval = 10s
    max_queue_count = 100
    }
}
```

#### 設定項目の説明

- **`gc_interval`**：メッセージキューが期限切れメッセージをクリーンアップする間隔を定義します。
- **`regular_queue_retention_period`**：通常キューでメッセージを保持する最大期間を設定します。この期間を過ぎるとメッセージは削除されます。
- **`find_queue_retry_interval`**：クライアントが `$queue/<name>` にサブスクライブしキューが見つからない場合に再試行する頻度を決定します。
- **`max_queue_count`**：（任意）作成可能なキューの最大数を設定します。

## REST APIでキューを管理する

EMQXはメッセージキューのライフサイクル管理（作成、取得、更新、削除）を行うREST APIを提供しています。

::: tip 注意

すべてのREST API操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは[REST API](../../guides/api.md)の「Message Queue」セクションをご参照ください。

:::

以下の例はすべてAPIキーとシークレットを用いたベーシック認証を前提としています。

### キューを作成する

キュー名、トピックフィルター、Last Value Semanticsの有効化などのキュー属性を指定して新しいメッセージキューを作成します：

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"name": "my_queue", "topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
```

レスポンスには作成されたキューの名前や設定内容が含まれます。

### すべてのキューを一覧表示する

既存のメッセージキュー一覧を取得します：

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

### キューを更新する

既存キューの配信戦略などの属性を更新します：

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/my_queue \
-d '{"dispatch_strategy": "least_inflight", "limits": {"max_shard_message_count": 5000, "max_shard_message_bytes": "100MB"}}' | jq
```

### キューを削除する

メッセージキューとその中に保持されているすべてのメッセージを削除します：

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/my_queue
```

削除後、キューは新しいメッセージの受け入れを停止し、保存されていたデータは削除されます。

## FAQとトラブルシューティング

### なぜメッセージがキューに格納されないのですか？

- 宣言済みメッセージキューのトピックフィルターがパブリッシュされたメッセージのトピックと一致しているか確認してください。
- キューが存在し、正しく設定されているか確認してください。
- EMQXのログを確認し、`mq_` プレフィックスのエントリを中心にキュー関連のエラーや警告を探してください。

### キューの容量が超過した場合はどうなりますか？

EMQXのメッセージキューは複数の容量制限タイプをサポートしています。いずれかの制限に達すると、GC（ガベージコレクション）時に古いメッセージが削除され、キューサイズが設定範囲内に戻るまで処理されます。

- **時間ベースの制限**：すべてのキューは設定された保持期間の制限を受けます。保持期間を超えたメッセージは配信対象外となり、自動的にGCで削除されます。

- **サイズベースの制限**：オプションでシャードごとに以下の制限を設定可能です：

  - **最大メッセージ数**（`max_shard_message_count`）
  - **最大メッセージ合計サイズ（バイト）**（`max_shard_message_bytes`）

  これらの制限はソフト制限であり、リアルタイムではなくGC時に適用されます。GCサイクル間は一時的に閾値を超える場合があります。

  なお、これらの制限は永続ストレージの各シャードごとに適用されます。シャード数の設定方法は[シャード数](../../guides/durability/managing-replication.md#number-of-shards)をご参照ください。また、サイズ制限は[レプリケーションファクター](../../guides/durability/managing-replication.md#replication-factor)を考慮していません。実際の物理ストレージ使用量はレプリケーションファクターにより増加します。
