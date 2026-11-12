# メッセージキュー ユーザーガイド

このページでは、EMQXのメッセージキュー機能の実践的な使い方について、キューの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## ダッシュボードからキューを手動で作成する

メッセージキューは、メッセージを格納または配信する前に明示的に宣言・作成する必要があります。キューは手動または自動で作成できます。自動作成の詳細については、[ダッシュボードからメッセージキューを自動作成する](#自動的にメッセージキューをダッシュボードから作成する)を参照してください。

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

     - キューが既に存在する場合は `$queue/<name>` を使用します。
     - キューが既に存在する場合にオプションで `$queue/<name>/<topic_filter>` を使用できます。自動作成が有効な場合は、キューが存在しない場合に `<topic_filter>` を使ってEMQXが自動的にキューを作成します。

   - **Topic Filter**：トピックまたはトピックフィルター（例：`t/1`）を入力します。これはパブリッシュされたメッセージのトピックマッチングに基づき、キューに格納されるメッセージを定義します。キューはこのトピックフィルターにマッチするすべてのメッセージを収集します。

     > トピックフィルターはキューの設定の一部ですが、キューの識別子ではありません。

   - **Dispatch Strategy**：メッセージをサブスクライバー間でどのように配信するかを選択します。利用可能な戦略は以下の通りです：

     - `Least Inflight Subscriber`：未アック済みメッセージが最も少ないサブスクライバーを優先します。
     - `Random`：（デフォルト）ランダムにサブスクライバーを選択します。
     - `Round Robin`：すべてのサブスクライバーに均等に順番に配信します。

   - **Data Retention Period**：メッセージをキュー内に保持する期間を指定します。時間単位（例：日）を設定可能です。

   - **Last Value Semantics**：デフォルトで有効です。有効にすると、同じキューキーを持つ新しいメッセージが、同じキュー内の未消費の以前のメッセージを上書きします。これにより、キーごとに最新のメッセージのみが保持されます。デフォルトのキーはメッセージパブリッシャーのクライアントIDです。キューキーの設定例は以下を参照してください。

     - **[Queue Key Expression](#queue-key-expression)**：Last Value Semanticsが有効な場合、このフィールドで各メッセージからキーを抽出するための式を定義します。デフォルト値は `message.from`（メッセージパブリッシャーのクライアントID）です。このフィールドは[Variform式](../../guides/configuration/configuration.md#variform-expressions)による設定をサポートします。

   - **Max Shard Message Count**：（オプション）キューの各シャードに許容される最大メッセージ数を設定します。この設定をオンにしてカスタム値を入力するか、無制限（`infinity`）にするために無効のままにできます。この設定は耐久ストレージに永続化されます。

   - **Max Shard Message Bytes**：（オプション）キューの各シャードに許容されるメッセージの合計サイズ（バイト単位）を設定します。この設定をオンにして値（例：`200MB`）を入力するか、無制限（`infinity`）にするために無効のままにできます。この設定は耐久ストレージに永続化されます。

     ::: tip パフォーマンスに関する注意

     サイズ制限付きのキューは、特に高スループット時に書き込み性能が低下する可能性があります。

     :::

4. **Create** をクリックしてキューを保存します。

新しいキューはキュー一覧に表示され、名前、トピックフィルター、配信戦略、Last Value Semanticsの状態、データ保持期間が示されます。キュー設定の編集や削除は **Actions** 列のボタンから行えます。

## Queue Key Expression

Queue Key Expressionは、Last Value Semanticsモードでメッセージ重複排除に使うキーをどのように抽出するかを指定します。この式はメッセージのデータに対して評価され、[Variform式](../../guides/configuration/configuration.md#variform-expressions)の構文に従います。

式は、`from`、`topic`、`payload`、`headers.properties`などのフィールドを含むメッセージコンテキストに対して評価されます。例えば、ユーザープロパティをキーに使う場合は以下のように設定できます：

```
message.headers.properties.User-Property.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しない場合）、メッセージは破棄され、キューに格納されません。

### メッセージコンテキストの例

<!--@include: ../shared/key-expression-message-context.md-->

### Queue Key Expressionの例

#### 例1

以下の設定でキューを作成したとします：
- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`message.headers.properties.User-Property.mq-key`

以下のメッセージがEMQXにパブリッシュされ、クライアントは存在しないものとします：
| No | 送信者 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 1  | `client1` | `t/1` | `keyA` |
| 2  | `client1` | `t/2` | `keyB` |
| 3  | `client2` | `t/3` | `keyA` |
| 4  | `client2` | `t/4` | `keyB` |

クライアントが接続してキューをサブスクライブすると、以下のメッセージが配信されます：
| No | 送信者 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 3  | `client2` | `t/3` | `keyA` |
| 4  | `client2` | `t/4` | `keyB` |

キューには各ユニークな `message.headers.properties.User-Property.mq-key` の最新メッセージのみが保持されます。キー式はトピックを跨いでキュー全体に適用されるため、`t/1` にパブリッシュされた `keyA` のメッセージは、後に `t/3` にパブリッシュされた `keyA` のメッセージで上書きされます。

#### 例2

以下の設定でキューを作成したとします：
- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`message.from`

例1と同じメッセージがEMQXにパブリッシュされた場合、クライアントが接続してキューをサブスクライブすると、以下のメッセージが配信されます：
| No | 送信者 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 2  | `client1` | `t/2` | `keyB` |
| 4  | `client2` | `t/4` | `keyB` |

同じ `message.from` 値を持つメッセージは互いに上書きされるため、各送信者の最新メッセージのみが保持されます。

#### 例3

以下の設定でキューを作成したとします：
- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`concat(message.headers.properties.User-Property.mq-key, '-', message.topic)`

以下のメッセージがEMQXにパブリッシュされた場合：
| No | 送信者 | トピック | ユーザープロパティ `mq-key` | 計算されたキー |
|----|--------|----------|-----------------------------|----------------|
| 1  | `client1` | `t/1` | `keyA` | `keyA-t/1` |
| 2  | `client1` | `t/2` | `keyB` | `keyB-t/2` |
| 3  | `client1` | `t/1` | `keyB` | `keyB-t/1` |
| 4  | `client1` | `t/2` | `keyA` | `keyA-t/2` |

クライアントが接続してキューをサブスクライブすると、すべてのメッセージが配信されます。これは、`message.headers.properties.User-Property.mq-key` と `message.topic` の組み合わせが各メッセージでユニークだからです。

## ダッシュボードからキューを自動作成する

クライアントが `$queue/` プレフィックス付きのトピックをサブスクライブすると、メッセージキューを自動的に作成できます。これにより、手動設定なしで動的にキューをプロビジョニング可能です。

自動作成が有効な場合：

- `$queue/<name>` へのサブスクライブはキューが既に存在する場合のみ有効です。
- `$queue/<name>/<topic_filter>` へのサブスクライブは、キューが存在しない場合に `<topic_filter>` を使ってEMQXが自動的にキューを作成します。

キューは通常のキューまたはLast Value Semanticsキューとして自動作成されます。

::: tip 注意

キューの適切な動作を保証するために、**Auto Create Regular Queue** または **Auto Create Last Value Semantics Queue** のいずれか一方のみを有効にしてください。両方同時に有効にしないでください。

:::

### Last Value Semanticsキューの自動作成

このオプションはデフォルトで **Management** -> **MQTT Settings** -> **Queues** タブの中で有効になっています。これにより、EMQXはLast Value Semanticsをサポートするキューを自動的に作成し、キーごとに最新のメッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. デフォルトで **Enable Auto Create Queue** -> **Last Value Semantics Queue** が有効です。

   以下を設定します：

   - **Queue Key Expression**：必須。各メッセージからユニークキーを抽出する方法を定義します（デフォルト：`message.from`）。
   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法を決定します（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージをキューに保持する期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$queue/my_queue/test` のようなトピックをサブスクライブすると、`my_queue` が存在しない場合、EMQXは `test` をトピックフィルターとしてLast Value Semanticsキュー `my_queue` を自動作成します。キューは **Queues** 一覧に表示されます。

### 通常キューの自動作成

メッセージを独立して保存し、上書きしない通常のキューを自動作成したい場合は、このオプションを手動で有効にできます。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. **Enable Auto Create Queue** -> **Regular Queue** をオンにします。

3. 以下を設定します：

   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法を決定します（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージをキューに保持する期間を指定します。

4. **Save Changes** をクリックします。

## キュー設定の構成

このセクションでは、EMQXのすべてのメッセージキューに適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部キュー動作、キューの自動作成動作を制御します。ダッシュボード、REST API、設定ファイルで設定可能です。

### ダッシュボード

EMQXダッシュボードからメッセージキュー設定を直接更新できます。ブローカーの再起動は不要で、システム全体の動作変更に便利です。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

   または、**Queues** ページ右上の **Settings** ボタンをクリックします。

2. **Queues** パネルで以下の設定が可能です：

   - **Enable Queues**：メッセージキュー機能を有効にします。

     > ダッシュボードからはキュー機能を無効にできません。無効にするには設定ファイルを直接編集してください。

   - **Max Queue Count**：作成可能なキューの最大数を設定します。

   - **GC Interval**：期限切れメッセージをキューからクリーンアップする間隔。デフォルトは1時間です。

   - **Regular Queue Retention Period**：通常キューでメッセージを保持する最大期間。デフォルトは7日です。

   - **Find Queue Retry Interval**：クライアントが `$queue/<name>` にサブスクライブした際にキューが見つからない場合、再試行する間隔。デフォルトは10秒です。

   - **Enable Auto Create Queue**：クライアントがキュートピックをサブスクライブし、該当キューが存在しない場合に自動作成を有効にします。

   - **Auto Create Queue Type**：自動作成するキューのタイプを指定します：

     - **Last Value Semantics Queue**（デフォルト有効）：クライアントが `$queue/<name>/<topic_filter>` にサブスクライブし、該当キューが存在しない場合、Last Value Semantics有効なキューを自動作成します。

       詳細は[Last Value Semanticsキューの自動作成](#auto-create-last-value-semantics-queues)を参照してください。

     - **Regular Queue**：有効にすると、`$queue/<name>/<topic_filter>` サブスクライブ時に通常キューを自動作成します。

       詳細は[通常キューの自動作成](#auto-create-regular-queues)を参照してください。

3. 設定変更後、**Save Changes** をクリックして適用します。

### REST API

グローバルなメッセージキュー設定はREST APIでも変更可能です。これらの設定はシステム全体に適用され、すべてのキューの内部管理に影響します。

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### 設定ファイル

永続的かつバージョン管理可能な設定のために、EMQX設定ファイル（`emqx.conf`）にメッセージキュー設定を定義できます。以下は主な設定例です：

```hocon
mq {
    gc_interval = 1h
    regular_queue_retention_period = 1d
    find_queue_retry_interval = 10s
    max_queue_count = 100
    }
}
```

#### 設定の説明

- **`gc_interval`**：メッセージキューが期限切れメッセージをクリーンアップする間隔を定義します。
- **`regular_queue_retention_period`**：通常キューでメッセージを保持する最大期間を設定します。この期間を過ぎたメッセージは削除されます。
- **`find_queue_retry_interval`**：クライアントが `$queue/<name>` にサブスクライブした際にキューが見つからない場合、再試行する間隔を決定します。
- **`max_queue_count`**：（オプション）作成可能なキューの最大数を設定します。

## REST APIによるキュー管理

EMQXはメッセージキューのライフサイクル管理（作成、取得、更新、削除）を行うREST APIを提供しています。

::: tip 注意

すべてのREST API操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細なスキーマは[REST API](../../guides/api.md)の「Message Queue」セクションを参照してください。

:::

以下の例はすべてAPIキーとシークレットによるベーシック認証を前提としています。

### キューを作成する

キュー名、トピックフィルター、Last Value Semanticsの有効化などのキュー属性を指定して新しいキューを作成します：

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"name": "my_queue", "topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
```

レスポンスには作成されたキューの名前や設定などの詳細が含まれます。

### すべてのキューを一覧表示する

既存のメッセージキュー一覧を取得します：

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

EMQX 6.3.3以降、削除中のキューもバックグラウンドクリーンアップが完了するまでレスポンスに含まれます。削除中のキューにはUnixタイムスタンプ（ミリ秒）の `deleted_at` フィールドが含まれます。削除中でないキューにはこのフィールドはありません。

### キューを更新する

既存キューの属性（例：配信戦略）を更新します：

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/my_queue \
-d '{"dispatch_strategy": "least_inflight", "limits": {"max_shard_message_count": 5000, "max_shard_message_bytes": "100MB"}}' | jq
```

### キューを削除する

キューとその中に保持されているすべてのメッセージを削除するリクエストを送信します：

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/my_queue
```

EMQX 6.3.3以降、この操作は非同期です。EMQXはキューを削除対象としてマークし、バックグラウンドでクリーンアップをスケジュールし、保存データの削除完了を待たずにHTTPステータスコード `204` を返します。

バックグラウンドクリーンアップが完了するまで：

- EMQXはキューへのパブリッシュメッセージの書き込みを停止します。キューのコンシューマーも停止し再起動しません。
- キューは一覧に `deleted_at` フィールド付きで残ります。キューの取得や更新リクエストはHTTPステータスコード `400` を返します。
- 同じ名前のキューを新規作成できません。
- キューは `max_queue_count` のカウントに含まれます。

削除に失敗した場合やノードがクリーンアップ前に停止した場合は、再度 `DELETE` リクエストを送信してクリーンアップを再スケジュールしてください。

::: warning ローリングアップグレードの制限

ローリングアップグレード中、削除マーカーを認識しないノードはバックグラウンドクリーンアップ完了までパブリッシュメッセージをキューにルーティングし続け、APIの一覧レスポンスにキューを含める可能性があります。

:::

## FAQとトラブルシューティング

### なぜメッセージがキューに格納されないのですか？

- 宣言済みメッセージキューのトピックフィルターが、パブリッシュされたメッセージのトピックにマッチしているか確認してください。
- キューが存在し、正しく設定されているか確認してください。
- EMQXのログに関連するエラーや警告がないか確認してください。特に `mq_` プレフィックスのログエントリをチェックしてキュー関連の問題を診断してください。

### キューの容量が超過した場合はどうなりますか？

EMQXのメッセージキューは複数の容量制限タイプをサポートしています。これらの制限のいずれかに達すると、EMQXはガベージコレクション（GC）時に最も古いメッセージを削除し、キューサイズを設定範囲内に戻します。

- **時間ベースの制限**：すべてのキューは設定された保持期間の制約を受けます。この期間を超えたメッセージは配信対象外となり、GC時に自動的に削除されます。

- **サイズベースの制限**：オプションでシャードごとに以下の制限を設定可能です：

  - **最大メッセージ数**（`max_shard_message_count`）
  - **最大メッセージサイズ合計（バイト単位）**（`max_shard_message_bytes`）

  これらの制限はソフト制限であり、リアルタイムではなくGC時に適用されます。GCサイクル間は一時的に制限を超える場合があります。

  これらの制限は耐久ストレージのシャード単位で適用されます。シャード数の設定方法は[シャード数](../../guides/durability/managing-replication.md#number-of-shards)を参照してください。また、サイズ制限は[レプリケーションファクター](../../guides/durability/managing-replication.md#replication-factor)を考慮していません。実際の物理ストレージ使用量はレプリケーションファクター分だけ増加します。
