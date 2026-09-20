# メッセージキュー ユーザーガイド

このページでは、EMQXのメッセージキュー機能の実践的な使い方について、キューの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## ダッシュボードからのキュー手動作成

メッセージキューは、メッセージを格納・配信する前に明示的に宣言・作成する必要があります。キューは手動または自動で作成可能です。自動作成の詳細は[ダッシュボードからのメッセージキュー自動作成](#自動的にメッセージキューをダッシュボードから作成する)をご参照ください。

1. 左メニューの **Queues** に移動します。

2. ページ上の **Create** ボタンをクリックします。

3. **Create Queue** ダイアログで以下のオプションを設定します：

   - **Name**：キューの一意の名前を指定します。キュー名には以下の文字のみ使用可能です：

     - 英数字（`A–Z`、`a–z`、`0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でキューは識別・管理されます。

     クライアントは以下のサブスクリプション形式でメッセージを消費できます：

     - `$queue/<name>` はキューが既に存在する場合に使用します。
     - `$queue/<name>/<topic_filter>` は既存キューへのサブスクライブ時に任意で使用可能です。自動作成が有効な場合に使えます。キューがまだ存在しない場合、EMQXは指定された `<topic_filter>` を使ってキューを自動作成します。

   - **Topic Filter**：トピックまたはトピックフィルター（例：`t/1`）を入力します。これはパブリッシュされたメッセージのトピックと照合し、キューに格納するメッセージを決定します。キューはこのトピックフィルターにマッチするすべてのメッセージを収集します。

     > トピックフィルターはキューの設定の一部ですが、キューの識別子ではありません。

   - **Dispatch Strategy**：メッセージをサブスクライバー間でどのように配信するかを選択します。利用可能な戦略は以下の通りです：

     - `Least Inflight Subscriber`：未アックメッセージ数が最も少ないサブスクライバーを優先します。
     - `Random`：（デフォルト）ランダムにサブスクライバーを選択します。
     - `Round Robin`：全サブスクライバーに均等に順番に配信します。

   - **Data Retention Period**：キュー内にメッセージを保持する期間を指定します。時間単位（例：日）を設定可能です。

   - **Last Value Semantics**：デフォルトで有効です。有効にすると、同じキューキーを持つ新しいメッセージが、未消費の同キーの既存メッセージを上書きします。これにより、キーごとに最新のメッセージのみが保持されます。デフォルトのキーはパブリッシャーのクライアントIDです。キューキーの設定例は以下をご参照ください。

     - **[Queue Key Expression](#queue-key-expression)**：Last Value Semanticsが有効な場合、このフィールドでメッセージからキーを抽出する式を定義します。デフォルトは `message.from`（パブリッシャーのクライアントID）です。このフィールドは[Variform式](../../guides/configuration/configuration.md#variform-expressions)で設定可能です。

   - **Max Shard Message Count**：（任意）キューの各シャードに許容される最大メッセージ数を設定します。この設定をオンにして任意の値を入力するか、無制限（`infinity`）にすることも可能です。設定は永続ストレージに保存されます。

   - **Max Shard Message Bytes**：（任意）キューの各シャードに許容されるメッセージの合計サイズ（バイト単位）を設定します。この設定をオンにして値（例：`200MB`）を入力するか、無制限（`infinity`）に設定可能です。設定は永続ストレージに保存されます。

     ::: tip パフォーマンスに関する注意

     サイズ制限を設定したキューは、特に高スループット時に書き込み性能が低下する可能性があります。

     :::

4. **Create** をクリックしてキューを保存します。

新しいキューはキュー一覧に表示され、名前、トピックフィルター、配信戦略、Last Value Semanticsの状態、データ保持期間が確認できます。キューの設定変更や削除は **Actions** 列のボタンから行えます。

## Queue Key Expression

Queue Key Expressionは、Last Value Semanticsモードでメッセージの重複排除に使うキーをメッセージから抽出する方法を指定します。この式はメッセージのデータに対して評価され、[Variform式](../../guides/configuration/configuration.md#variform-expressions)の構文に従います。

式は、`from`、`topic`、`payload`、`headers.properties`などのフィールドを含むメッセージコンテキストに対して評価されます。例えば、ユーザープロパティをキーに使う場合、式は以下のように設定できます：

```
message.headers.properties.User-Property.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しない）、メッセージは破棄され、キューに格納されません。

### メッセージコンテキスト例

<!--@include: ../shared/key-expression-message-context.md-->

### Queue Key Expressionの例

#### 例1

以下の条件でキューを設定したとします：
- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`message.headers.properties.User-Property.mq-key`

以下のメッセージがEMQXにパブリッシュされ、クライアントは存在せず消費されていないとします：
| No | 送信元 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|------------------------------|
| 1  | `client1` | `t/1`   | `keyA`                       |
| 2  | `client1` | `t/2`   | `keyB`                       |
| 3  | `client2` | `t/3`   | `keyA`                       |
| 4  | `client2` | `t/4`   | `keyB`                       |

クライアントが接続してキューにサブスクライブすると、配信されるメッセージは以下の通りです：
| No | 送信元 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|------------------------------|
| 3  | `client2` | `t/3`   | `keyA`                       |
| 4  | `client2` | `t/4`   | `keyB`                       |

キューには、`message.headers.properties.User-Property.mq-key` の値ごとに最新のメッセージのみが保持されます。キー式はトピックを跨いでキュー全体に適用されるため、`t/1` にパブリッシュされた `keyA` のメッセージは、後に `t/3` にパブリッシュされた `keyA` のメッセージで上書きされます。

#### 例2

以下の条件でキューを設定したとします：
- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`message.from`

例1と同じメッセージがEMQXにパブリッシュされた場合、クライアントが接続してキューにサブスクライブすると、配信されるメッセージは以下の通りです：
| No | 送信元 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|------------------------------|
| 2  | `client1` | `t/2`   | `keyB`                       |
| 4  | `client2` | `t/4`   | `keyB`                       |

同じ `message.from` の値を持つメッセージは互いに上書きされるため、送信元ごとに最新のメッセージのみが保持されます。

#### 例3

以下の条件でキューを設定したとします：
- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`concat(message.headers.properties.User-Property.mq-key, '-', message.topic)`

以下のメッセージがEMQXにパブリッシュされたとします：
| No | 送信元 | トピック | ユーザープロパティ `mq-key` | 計算されたキー |
|----|--------|----------|------------------------------|----------------|
| 1  | `client1` | `t/1`   | `keyA`                       | `keyA-t/1`     |
| 2  | `client1` | `t/2`   | `keyB`                       | `keyB-t/2`     |
| 3  | `client1` | `t/1`   | `keyB`                       | `keyB-t/1`     |
| 4  | `client1` | `t/2`   | `keyA`                       | `keyA-t/2`     |

クライアントが接続してキューにサブスクライブすると、すべてのメッセージが配信されます。これは、`message.headers.properties.User-Property.mq-key` と `message.topic` の組み合わせが各メッセージでユニークだからです。

## ダッシュボードからのキュー自動作成

クライアントが `$queue/` プレフィックス付きのトピックにサブスクライブした際に、メッセージキューを自動作成できます。これにより手動設定なしでキューを動的にプロビジョニング可能です。

自動作成が有効な場合：

- `$queue/<name>` へのサブスクライブはキューが既に存在する場合のみ機能します。
- `$queue/<name>/<topic_filter>` へのサブスクライブは、指定された `<topic_filter>` を使ってキューが存在しない場合にEMQXが自動作成します。

キューは通常のキューまたはLast Value Semanticsキューとして自動作成可能です。

::: tip 注意

適切なキュー動作のため、**Auto Create Regular Queue** または **Auto Create Last Value Semantics Queue** のいずれか一方のみを有効にしてください。両方同時には有効にできません。

:::

### Last Value Semanticsキューの自動作成

このオプションはデフォルトで **MQTT Settings** の **Queues** タブにて有効です。キーごとに最新のメッセージのみを保持するLast Value Semantics対応キューを自動作成します。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. デフォルトで **Enable Auto Create Queue** -> **Last Value Semantics Queue** が有効になっています。

   以下を設定可能です：

   - **Queue Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。
   - **Dispatch Strategy**：メッセージのサブスクライバーへの配信方法を決定します（デフォルト：`Random`）。
   - **Data Retention Period**：キュー内メッセージの保持期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$queue/my_queue/test` のようなトピックにサブスクライブすると、`my_queue` が存在しなければEMQXは `test` をトピックフィルターとするLast Value Semanticsキュー `my_queue` を自動作成します。作成されたキューは **Queues** 一覧に表示されます。

### 通常キューの自動作成

メッセージを上書きせず独立して保存する通常キューを自動作成したい場合に手動で有効にできます。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。
2. **Enable Auto Create Queue** -> **Regular Queue** をオンにします。
3. 以下を設定します：
   - **Dispatch Strategy**：サブスクライバーへのメッセージ配信方法（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージの保持期間。
4. **Save Changes** をクリックします。

## キュー設定の構成

このセクションでは、EMQXのすべてのメッセージキューに適用されるグローバル設定の方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部キューの動作、キューの自動作成動作を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQXダッシュボードからメッセージキュー設定を直接更新可能で、ブローカーの再起動は不要です。システム全体の動作をランタイムで変更する際に便利です。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

   または、**Queues** ページ右上の **Settings** ボタンをクリックします。

2. **Queues** パネルで以下の設定が可能です：
   - **Enable Queues**：メッセージキュー機能を有効化します。

     > ダッシュボードからはキュー機能を無効化できません。無効化する場合は設定ファイルを直接編集してください。

   - **Max Queue Count**：作成可能なキューの最大数を設定します。

   - **GC Interval**：期限切れメッセージのクリーンアップ間隔。デフォルトは `1` 時間です。

   - **Regular Queue Retention Period**：通常キューでメッセージを保持する最大期間。デフォルトは `7` 日です。

   - **Find Queue Retry Interval**：クライアントが `$queue/<name>` にサブスクライブし、該当キューが見つからない場合に再試行する間隔。デフォルトは `10` 秒です。

   - **Enable Auto Create Queue**：キューが存在しない場合に自動作成を有効化します。

   - **Auto Create Queue Type**：自動作成するキューのタイプを指定します：

     - **Last Value Semantics Queue**（デフォルト有効）：クライアントが `$queue/<name>/<topic_filter>` にサブスクライブし、該当キューが存在しない場合、Last Value Semantics対応キューを自動作成します。

       詳細は[Last Value Semanticsキューの自動作成](#auto-create-last-value-semantics-queues)をご覧ください。

     - **Regular Queue**：有効にすると、`$queue/<name>/<topic_filter>` のサブスクライブ時に通常キューを自動作成します。

       詳細は[通常キューの自動作成](#auto-create-regular-queues)をご覧ください。

3. 変更後、**Save Changes** をクリックして設定を適用します。

### REST API

REST APIを使ってもグローバルなメッセージキュー設定を変更可能です。これらの設定はシステム全体に適用され、すべてのキューの内部管理に影響します。

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### 設定ファイル

永続的かつバージョン管理可能な設定には、EMQXの設定ファイル（`emqx.conf`）にメッセージキュー設定を記述します。以下は主要な設定例です：

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
- **`regular_queue_retention_period`**：通常キューでメッセージを保持する最大期間を設定します。この期間を過ぎたメッセージは削除されます。
- **`find_queue_retry_interval`**：クライアントが `$queue/<name>` にサブスクライブし、キューが見つからない場合に再試行する頻度を決定します。
- **`max_queue_count`**：（任意）作成可能なキューの最大数を設定します。

## REST APIによるキュー管理

EMQXはメッセージキューのライフサイクル管理（作成、取得、更新、削除）を行うREST APIを提供しています。

::: tip 注意

すべてのREST API操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは[REST API](../../guides/api.md)の「Message Queue」セクションをご参照ください。

:::

以下の例はすべてAPIキーとシークレットを用いたベーシック認証を想定しています。

### キューの作成

キュー名、トピックフィルター、Last Value Semanticsの有効化などのキュー属性を指定して新しいメッセージキューを作成します：

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"name": "my_queue", "topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
```

レスポンスには作成されたキューの詳細（`name` や設定内容）が含まれます。

### すべてのキュー一覧取得

既存のメッセージキュー一覧を取得します：

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

### キューの更新

既存キューの属性（例：配信戦略）を更新します：

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/my_queue \
-d '{"dispatch_strategy": "least_inflight", "limits": {"max_shard_message_count": 5000, "max_shard_message_bytes": "100MB"}}' | jq
```

### キューの削除

メッセージキューとその保持メッセージをすべて削除します：

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/my_queue
```

削除後、キューは新規メッセージの受け入れを停止し、保存データは削除されます。

## FAQとトラブルシューティング

### なぜメッセージがキューに格納されないのですか？

- 宣言済みメッセージキューのトピックフィルターがパブリッシュされたメッセージのトピックに一致しているか確認してください。
- キューが存在し、適切に設定されているか確認してください。
- EMQXのログを確認し、`mq_` プレフィックスのエントリを探してキュー関連のエラーや警告を診断してください。

### キューの容量が超過した場合はどうなりますか？

EMQXのメッセージキューは複数の容量制限タイプをサポートしています。いずれかの制限に達した場合、GC（ガベージコレクション）時に最も古いメッセージから順に削除し、キューサイズを設定範囲内に戻します。

- **時間ベースの制限**：すべてのキューは設定された保持期間の制限を受けます。保持期間を超えたメッセージは配信対象外となり、GCで自動的に削除されます。

- **サイズベースの制限**：シャードごとに以下の制限を任意で設定可能です：

  - **最大メッセージ数**（`max_shard_message_count`）
  - **最大メッセージ合計サイズ（バイト）**（`max_shard_message_bytes`）

  これらの制限はソフト制限であり、リアルタイムではなくGC時に適用されます。GCサイクル間は設定値を一時的に超過することがあります。

  なお、これらの制限は永続ストレージの各シャード単位で適用されます。シャード数の設定方法は[シャード数](../../guides/durability/managing-replication.md#number-of-shards)をご参照ください。また、サイズ制限は[レプリケーションファクター](../../guides/durability/managing-replication.md#replication-factor)を考慮していません。実際の物理ストレージ使用量はレプリケーションファクター分だけ増加します。
