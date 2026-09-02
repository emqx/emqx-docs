# メッセージキュー ユーザーガイド

このページでは、EMQXのメッセージキュー機能の実践的な使い方について、キューの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## ダッシュボードからキューを手動で作成する

メッセージキューは、メッセージを格納・配信する前に明示的に宣言・作成する必要があります。キューは手動または自動で作成できます。自動作成の詳細は[ダッシュボードからメッセージキューを自動作成する](#automatically-create-message-queues-via-dashboard)をご覧ください。

1. 左メニューの **Queues** に移動します。

2. ページの **Create** ボタンをクリックします。

3. **Create Queue** ダイアログで以下のオプションを設定します：

   - **Name**：キューの一意の名前を指定します。キュー名には以下の文字のみ使用可能です：

     - 英数字（`A–Z`、`a–z`、`0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でキューは識別・管理されます。

     クライアントは以下のサブスクリプション形式でメッセージを消費できます：

     - `$queue/<name>`：キューが既に存在する場合に使用します。
     - `$queue/<name>/<topic_filter>`：既存キューにサブスクライブする際に任意で使用可能です。自動作成が有効な場合に使います。キューがまだ存在しない場合、EMQXは指定された `<topic_filter>` を使って自動的にキューを作成します。

   - **Topic Filter**：トピックまたはトピックフィルター（例：`t/1`）を入力します。これはパブリッシュされたメッセージのトピックと照合し、キューに格納するメッセージを決定します。キューはこのトピックフィルターにマッチするすべてのメッセージを収集します。

     > トピックフィルターはキューの設定の一部ですが、キューの識別子ではありません。

   - **Dispatch Strategy**：メッセージをサブスクライバー間でどのように配信するかを選択します。利用可能な戦略は以下の通りです：

     - `Least Inflight Subscriber`：未アックのメッセージ数が最も少ないサブスクライバーを優先します。
     - `Random`：（デフォルト）ランダムにサブスクライバーを選択します。
     - `Round Robin`：すべてのサブスクライバーに均等に配信を回します。

   - **Data Retention Period**：キュー内のメッセージをどのくらいの期間保持するかを指定します。時間単位（例：日）も設定可能です。

   - **Last Value Semantics**：デフォルトで有効です。有効時、同じキューキーを持つ新しいメッセージは、同じキュー内の未消費の以前のメッセージを上書きします。これにより、キーごとに最新のメッセージのみが保持されます。デフォルトのキーはメッセージパブリッシャーのクライアントIDです。キューキーの設定例は以下をご参照ください。

     - **[Queue Key Expression](#queue-key-expression)**：Last Value Semanticsが有効な場合、このフィールドで各メッセージからキーを抽出する式を定義します。デフォルトは `message.from`（メッセージパブリッシャーのクライアントID）です。このフィールドは[Variform式](../configuration/configuration.md#variform-expressions)で設定可能です。

   - **Max Shard Message Count**：（任意）キューの各シャードに許容される最大メッセージ数を設定します。この設定を有効にしてカスタム値を入力するか、無効にして無制限（`infinity`）にできます。この設定は永続ストレージに保存されます。

   - **Max Shard Message Bytes**：（任意）キューの各シャードに許容されるメッセージの合計サイズ（バイト単位）を設定します。この設定を有効にして値（例：`200MB`）を入力するか、無効にして無制限（`infinity`）にできます。この設定も永続ストレージに保存されます。

     ::: tip パフォーマンスに関する注意

     サイズ制限付きのキューは、特に高スループット環境下で書き込み性能が低下する可能性があります。

     :::

4. **Create** をクリックしてキューを保存します。

新しいキューはキュー一覧に表示され、名前、トピックフィルター、配信戦略、Last Value Semanticsの状態、データ保持期間が確認できます。キューの設定変更や削除は **Actions** 列のボタンから行えます。

## キューキー式（Queue Key Expression）

キューキー式は、Last Value Semanticsモードでメッセージの重複排除に使うキーを抽出する方法を指定します。この式はメッセージのデータに対して評価され、[Variform式](../configuration/configuration.md#variform-expressions)の構文に従います。

式は、`from`、`topic`、`payload`、`headers.properties` などを含むメッセージコンテキストに対して評価されます。例えば、ユーザープロパティをキーにする場合は以下のように設定します：

```
message.headers.properties.User-Property.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しないなど）、メッセージは破棄され、キューに格納されません。

### メッセージコンテキスト例

<!--@include: ../shared/key-expression-message-context.md-->

### キューキー式の例

#### 例1

以下の条件でキューを設定したとします：

- Last Value Semantics 有効
- トピックフィルター：`t/#`
- キューキー式：`message.headers.properties.User-Property.mq-key`

以下のメッセージがEMQXにパブリッシュされ（クライアントは存在せず消費されない）：
| No | 送信者 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 1  | `client1` | `t/1`   | `keyA`                      |
| 2  | `client1` | `t/2`   | `keyB`                      |
| 3  | `client2` | `t/3`   | `keyA`                      |
| 4  | `client2` | `t/4`   | `keyB`                      |

クライアントが接続してキューにサブスクライブすると、以下のメッセージが配信されます：
| No | 送信者 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 3  | `client2` | `t/3`   | `keyA`                      |
| 4  | `client2` | `t/4`   | `keyB`                      |

同じ `message.headers.properties.User-Property.mq-key` の値ごとに最新のメッセージのみがキューに保持されます。キー式はキュー全体に対して有効であり、`keyA` のメッセージが `t/1` にパブリッシュされた後、`t/3` にパブリッシュされたメッセージで上書きされます。

#### 例2

以下の条件でキューを設定したとします：

- Last Value Semantics 有効
- トピックフィルター：`t/#`
- キューキー式：`message.from`

例1と同じメッセージがパブリッシュされた場合、クライアントが接続してキューにサブスクライブすると、以下のメッセージが配信されます：
| No | 送信者 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 2  | `client1` | `t/2`   | `keyB`                      |
| 4  | `client2` | `t/4`   | `keyB`                      |

同じ `message.from` の値を持つメッセージは上書きされるため、送信者ごとに最新のメッセージのみが保持されます。

#### 例3

以下の条件でキューを設定したとします：

- Last Value Semantics 有効
- トピックフィルター：`t/#`
- キューキー式：`concat(message.headers.properties.User-Property.mq-key, '-', message.topic)`

以下のメッセージがEMQXにパブリッシュされました：
| No | 送信者 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 1  | `client1` | `t/1`   | `keyA`                      |
| 2  | `client1` | `t/2`   | `keyB`                      |
| 3  | `client1` | `t/1`   | `keyB`                      |
| 4  | `client1` | `t/2`   | `keyA`                      |

クライアントが接続してキューにサブスクライブすると、すべてのメッセージが配信されます。なぜなら、`message.headers.properties.User-Property.mq-key` と `message.topic` の組み合わせが各メッセージでユニークだからです：
| No | 送信者 | トピック | ユーザープロパティ `mq-key` | 計算されたキー             |
|----|--------|----------|-----------------------------|----------------------------|
| 1  | `client1` | `t/1`   | `keyA`                      | `keyA-t/1`                 |
| 2  | `client1` | `t/2`   | `keyB`                      | `keyB-t/2`                 |
| 3  | `client1` | `t/1`   | `keyB`                      | `keyB-t/1`                 |
| 4  | `client1` | `t/2`   | `keyA`                      | `keyA-t/2`                 |

## ダッシュボードからキューを自動作成する

メッセージキューは、クライアントが `$queue/` プレフィックス付きトピックにサブスクライブした際に自動的に作成できます。これにより手動設定なしで動的にキューをプロビジョニングできます。

自動作成が有効な場合：

- `$queue/<name>` へのサブスクライブはキューが既に存在する場合のみ有効です。
- `$queue/<name>/<topic_filter>` へのサブスクライブは、キューが存在しない場合に指定された `<topic_filter>` を使ってEMQXが自動的にキューを作成します。

キューは通常のキューまたはLast Value Semanticsキューとして自動作成できます。

::: tip 注意

キューの動作を正しく保つため、**Auto Create Regular Queue** と **Auto Create Last Value Semantics Queue** の両方を同時に有効にすることはできません。

:::

### Last Value Semanticsキューの自動作成

このオプションはデフォルトで **MQTT Settings** の **Queues** タブ内で有効になっています。EMQXがLast Value Semantics対応のキューを自動作成し、指定されたキーごとに最新のメッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. デフォルトで **Enable Auto Create Queue** -> **Last Value Semantics Queue** が有効です。

   以下を設定します：

   - **Queue Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。
   - **Dispatch Strategy**：メッセージ配信方法を決定します（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージの保持期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$queue/my_queue/test` のようなトピックにサブスクライブすると、`my_queue` が存在しない場合、EMQXは `test` をトピックフィルターとしてLast Value Semanticsキュー `my_queue` を自動作成します。キューは **Queues** 一覧に表示されます。

### 通常キューの自動作成

メッセージを上書きせず独立して保存する通常キューを自動作成したい場合に手動で有効にできます。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。
2. **Enable Auto Create Queue** -> **Regular Queue** をオンにします。
3. 以下を設定します：
   - **Dispatch Strategy**：メッセージ配信方法（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージ保持期間。
4. **Save Changes** をクリックします。

## キュー設定の構成

このセクションでは、EMQX内のすべてのメッセージキューに適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部キューの動作、キューの自動作成動作を制御します。ダッシュボード、REST API、設定ファイルで設定可能です。

### ダッシュボード

EMQXダッシュボードからメッセージキューの設定をブローカー再起動なしで更新できます。システム全体の動作変更に便利です。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

   または、**Queues** ページ右上の **Settings** ボタンをクリックします。

2. **Queues** パネルで以下の設定が可能です：

   - **Enable Queues**：メッセージキュー機能を有効にします。

     > ダッシュボードからキュー機能を無効化することはできません。無効化するには設定ファイルを直接編集してください。

   - **Max Queue Count**：作成可能なキューの最大数を設定します。

   - **GC Interval**：期限切れメッセージをキューからクリーンアップする間隔。デフォルトは `1` 時間です。

   - **Regular Queue Retention Period**：通常キューでメッセージを保持する最大期間。デフォルトは `7` 日です。

   - **Find Queue Retry Interval**：クライアントが `$queue/<name>` にサブスクライブした際、該当キューが見つからない場合に再試行する間隔。デフォルトは `10` 秒です。

   - **Enable Auto Create Queue**：クライアントがキュートピックにサブスクライブし、該当キューが存在しない場合に自動作成を有効にします。

   - **Auto Create Queue Type**：自動作成するキューの種類を指定します：

     - **Last Value Semantics Queue**（デフォルトで有効）：クライアントが `$queue/<name>/<topic_filter>` にサブスクライブし、該当キューがなければLast Value Semantics対応キューを自動作成します。

       詳細は[Last Value Semanticsキューの自動作成](#auto-create-last-value-semantics-queues)を参照してください。

     - **Regular Queue**：有効にすると、EMQXは `$queue/<name>/<topic_filter>` のサブスクライブ時に通常キューを自動作成します。

       詳細は[通常キューの自動作成](#auto-create-regular-queues)を参照してください。

3. 設定変更後、**Save Changes** をクリックして適用します。

### REST API

REST APIを使ってグローバルなメッセージキュー設定を構成できます。これらの設定はシステム全体に適用され、すべてのキューの内部管理に影響します。

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### 設定ファイル

永続的かつバージョン管理可能な設定のために、EMQXの設定ファイル（`emqx.conf`）にメッセージキュー設定を定義できます。以下は主要設定の例です：

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
- **`find_queue_retry_interval`**：クライアントが `$queue/<name>` にサブスクライブし、キューが見つからない場合に再試行する頻度を決定します。
- **`max_queue_count`**：（任意）作成可能なキューの最大数を設定します。

## REST APIでキューを管理する

EMQXはメッセージキューのライフサイクル管理（作成、取得、更新、削除）を行うREST APIを提供しています。

::: tip 注意

すべてのREST API操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは[REST API](../admin/api.md)の「Message Queue」セクションを参照してください。

:::

以下の例はすべてAPIキーとシークレットによるベーシック認証を前提としています。

### キューを作成する

キュー名、トピックフィルター、Last Value Semanticsの有効化などのキュー属性を指定して新しいメッセージキューを作成します：

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"name": "my_queue", "topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
```

レスポンスには作成されたキューの詳細（`name` や設定内容）が含まれます。

### すべてのキューを一覧表示する

既存のメッセージキュー一覧を取得します：

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

### キューを更新する

既存キューの属性（例：配信戦略）を更新します：

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

削除後、キューは新しいメッセージの受け入れを停止し、保存データは削除されます。

## FAQとトラブルシューティング

### なぜメッセージがキューに格納されないのですか？

- 宣言済みメッセージキューのトピックフィルターが、パブリッシュされたメッセージのトピックと一致しているか確認してください。
- キューが存在し、正しく設定されているか確認してください。
- EMQXのログに関連するエラーや警告がないか確認してください。特に `mq_` プレフィックスのログをチェックするとキュー関連の問題を特定しやすいです。

### キューの容量が超過した場合はどうなりますか？

EMQXのメッセージキューは複数の容量制限タイプをサポートしています。いずれかの制限に達すると、GC（ガベージコレクション）時に古いメッセージが削除され、キューサイズが設定範囲内に戻されます。

- **時間ベースの制限**：すべてのキューは設定された保持期間の制限を受けます。保持期間を超えたメッセージは配信対象外となり、GC時に自動的に削除されます。

- **サイズベースの制限**：シャードごとに以下の制限をオプションで設定可能です：

  - **最大メッセージ数**（`max_shard_message_count`）
  - **最大メッセージ合計サイズ（バイト単位）**（`max_shard_message_bytes`）

  これらの制限はリアルタイムではなくGC時に適用されるソフト制限です。GCサイクル間では一時的に制限を超過することがあります。

  なお、これらの制限は永続ストレージの各シャード単位で適用されます。シャード数の設定方法は[シャード数](../durability/managing-replication.md#number-of-shards)を参照してください。また、サイズ制限は[レプリケーションファクター](../durability/managing-replication.md#replication-factor)を考慮していません。実際の物理ストレージ使用量はレプリケーションファクター分だけ増加します。
