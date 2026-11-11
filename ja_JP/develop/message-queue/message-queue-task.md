# メッセージキュー ユーザーガイド

このページでは、EMQXのメッセージキュー機能の実践的な使い方を解説します。キューの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを網羅しています。

## ダッシュボードからキューを手動で作成する

メッセージキューは、メッセージを格納・配信する前に明示的に宣言・作成する必要があります。キューは手動または自動で作成可能です。自動作成の詳細は[ダッシュボードからメッセージキューを自動作成する](#automatically-create-message-queues-via-dashboard)をご参照ください。

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

     - `$queue/<name>`：キューが既に存在する場合に使用。
     - `$queue/<name>/<topic_filter>`：既存キューへのサブスクライブ時に任意で使用可能。自動作成が有効な場合に利用されます。キューが未作成の場合、EMQXは指定された `<topic_filter>` を使って自動的にキューを作成します。

   - **Topic Filter**：トピックまたはトピックフィルター（例：`t/1`）を入力します。パブリッシュされたメッセージのトピックとマッチしたメッセージがこのキューに格納されます。

     > トピックフィルターはキューの設定の一部ですが、キューの識別子ではありません。

   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法を選択します。利用可能な戦略は以下の通りです：

     - `Least Inflight Subscriber`：未アックのメッセージが最も少ないサブスクライバーを優先。
     - `Random`：（デフォルト）ランダムにサブスクライバーを選択。
     - `Round Robin`：全サブスクライバーに均等に順番に配信。

   - **Data Retention Period**：メッセージをキュー内に保持する期間を指定します。時間単位（例：日）を設定可能です。

   - **Last Value Semantics**：デフォルトで有効です。有効時は同じキューキーを持つ新しいメッセージが、同じキュー内の未消費の古いメッセージを上書きします。これによりキーごとに最新のメッセージのみが保持されます。デフォルトのキーはメッセージパブリッシャーのクライアントIDです。キューキーの設定例は以下をご参照ください。

     - **[Queue Key Expression](#queue-key-expression)**：Last Value Semanticsが有効な場合、このフィールドでメッセージからキーを抽出する式を定義します。デフォルトは `message.from`（パブリッシャーのクライアントID）です。このフィールドは[Variform式](../../guides/configuration/configuration.md#variform-expressions)で設定可能です。

   - **Max Shard Message Count**：（任意）キューの各シャードに許容される最大メッセージ数を設定します。有効化して値を入力するか、無効化して無制限（`infinity`）にできます。この設定は永続ストレージに保存されます。

   - **Max Shard Message Bytes**：（任意）キューの各シャードに許容されるメッセージの合計最大サイズ（バイト単位）を設定します。有効化して値（例：`200MB`）を入力するか、無効化して無制限（`infinity`）にできます。この設定も永続ストレージに保存されます。

     ::: tip パフォーマンスに関する注意

     サイズ制限付きキューは、特に高スループット時に書き込み性能が低下する可能性があります。

     :::

4. **Create** をクリックしてキューを保存します。

新しいキューはキュー一覧に表示され、名前、トピックフィルター、配信戦略、Last Value Semanticsの状態、データ保持期間が確認できます。キューの設定編集や削除は **Actions** 列のボタンから行えます。

## Queue Key Expression

Queue Key Expressionは、Last Value Semanticsモードでメッセージの重複排除に使うキーを抽出する方法を指定します。この式はメッセージのデータに対して評価され、[Variform式](../../guides/configuration/configuration.md#variform-expressions)の構文に従います。

式は、`from`、`topic`、`payload`、`headers.properties`などのフィールドを含むメッセージコンテキストに対して評価されます。例えば、ユーザープロパティをキーに使う場合は以下のように設定します：

```
message.headers.properties.User-Property.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しない場合）、メッセージは破棄されキューに格納されません。

### メッセージコンテキスト例

<!--@include: ../shared/key-expression-message-context.md-->

### Queue Key Expressionの例

#### 例1

以下のキューを設定したとします：

- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`message.headers.properties.User-Property.mq-key`

以下のメッセージがEMQXにパブリッシュされ、クライアントは存在しないものとします：

| No | 送信元 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 1  | `client1` | `t/1` | `keyA`                      |
| 2  | `client1` | `t/2` | `keyB`                      |
| 3  | `client2` | `t/3` | `keyA`                      |
| 4  | `client2` | `t/4` | `keyB`                      |

クライアントが接続しキューにサブスクライブすると、以下のメッセージが配信されます：

| No | 送信元 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 3  | `client2` | `t/3` | `keyA`                      |
| 4  | `client2` | `t/4` | `keyB`                      |

キューには `message.headers.properties.User-Property.mq-key` の値ごとに最新のメッセージのみが保持されます。キー式はトピックを跨いでキュー全体に適用されるため、`t/1` にパブリッシュされた `keyA` のメッセージは、後に `t/3` にパブリッシュされた `keyA` のメッセージで上書きされます。

#### 例2

以下のキューを設定したとします：

- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`message.from`

例1と同じメッセージがパブリッシュされた場合、クライアントが接続してサブスクライブすると次のメッセージが配信されます：

| No | 送信元 | トピック | ユーザープロパティ `mq-key` |
|----|--------|----------|-----------------------------|
| 2  | `client1` | `t/2` | `keyB`                      |
| 4  | `client2` | `t/4` | `keyB`                      |

同じ `message.from` の値を持つメッセージは上書きされるため、送信元ごとに最新のメッセージのみが保持されます。

#### 例3

以下のキューを設定したとします：

- Last Value Semantics 有効
- トピックフィルター：`t/#`
- Queue Key Expression：`concat(message.headers.properties.User-Property.mq-key, '-', message.topic)`

以下のメッセージがEMQXにパブリッシュされます：

| No | 送信元 | トピック | ユーザープロパティ `mq-key` | 計算されたキー              |
|----|--------|----------|-----------------------------|-----------------------------|
| 1  | `client1` | `t/1` | `keyA`                      | `keyA-t/1`                  |
| 2  | `client1` | `t/2` | `keyB`                      | `keyB-t/2`                  |
| 3  | `client1` | `t/1` | `keyB`                      | `keyB-t/1`                  |
| 4  | `client1` | `t/2` | `keyA`                      | `keyA-t/2`                  |

クライアントが接続しサブスクライブすると、キーがユニークなため全てのメッセージが配信されます。

## ダッシュボードからキューを自動作成する

クライアントが `$queue/` プレフィックス付きのトピックにサブスクライブすると、メッセージキューを自動作成できます。これにより手動設定なしで動的にキューをプロビジョニング可能です。

自動作成が有効な場合：

- `$queue/<name>` へのサブスクライブはキューが既に存在する場合のみ機能します。
- `$queue/<name>/<topic_filter>` へのサブスクライブは、キューが存在しない場合に `<topic_filter>` を使ってEMQXがキューを自動作成します。

キューは通常のキューまたはLast Value Semanticsキューとして自動作成できます。

::: tip 注意

適切なキュー動作のため、**Auto Create Regular Queue** または **Auto Create Last Value Semantics Queue** のいずれか一方のみを有効にしてください。両方同時の有効化は推奨されません。

:::

### Last Value Semanticsキューの自動作成

このオプションはデフォルトで **Management** -> **MQTT Settings** -> **Queues** タブの中で有効になっています。これにより、指定されたキーの最新メッセージのみを保持するLast Value Semantics対応キューを自動作成できます。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. デフォルトで **Enable Auto Create Queue** -> **Last Value Semantics Queue** が有効です。

   以下を設定します：

   - **Queue Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルトは `message.from`）。
   - **Dispatch Strategy**：メッセージの配信方法を指定します（デフォルトは `Random`）。
   - **Data Retention Period**：メッセージをキューに保持する期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$queue/my_queue/test` のようなトピックにサブスクライブすると、`my_queue` が存在しない場合、EMQXは `test` をトピックフィルターとしてLast Value Semanticsキュー `my_queue` を自動作成します。作成されたキューは **Queues** 一覧に表示されます。

### 通常キューの自動作成

メッセージが上書きされない通常のキューを自動作成したい場合は、このオプションを手動で有効にできます。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. **Enable Auto Create Queue** -> **Regular Queue** を有効にします。

3. 以下を設定します：

   - **Dispatch Strategy**：メッセージの配信方法（デフォルトは `Random`）。
   - **Data Retention Period**：メッセージ保持期間。

4. **Save Changes** をクリックします。

## キュー設定の構成

このセクションでは、EMQX内のすべてのメッセージキューに適用されるグローバル設定の方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部キュー動作、キューの自動作成動作を制御します。ダッシュボード、REST API、設定ファイルで設定可能です。

### ダッシュボード

EMQXダッシュボードからメッセージキュー設定を直接更新でき、ブローカーの再起動は不要です。システム全体の動作をランタイムで変更する際に便利です。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

   または、**Queues** ページ右上の **Settings** ボタンをクリックします。

2. **Queues** パネルで以下の設定が可能です：

   - **Enable Queues**：メッセージキュー機能を有効化します。

     > ダッシュボードからは無効化できません。無効化するには設定ファイルを直接編集してください。

   - **Max Queue Count**：作成可能なキューの最大数を設定します。

   - **GC Interval**：期限切れメッセージのクリーンアップ間隔。デフォルトは `1` 時間です。

   - **Regular Queue Retention Period**：通常キューのメッセージ最大保持期間。デフォルトは `7` 日です。

   - **Find Queue Retry Interval**：クライアントが `$queue/<name>` にサブスクライブし、対応するキューが見つからない場合の再試行間隔。デフォルトは `10` 秒です。

   - **Enable Auto Create Queue**：キューが存在しない場合に自動作成を有効にします。

   - **Auto Create Queue Type**：自動作成するキューのタイプを指定します：

     - **Last Value Semantics Queue**（デフォルト有効）：`$queue/<name>/<topic_filter>` にサブスクライブ時に該当キューがなければLast Value Semanticsキューを自動作成します。

       詳細は[Last Value Semanticsキューの自動作成](#auto-create-last-value-semantics-queues)をご覧ください。

     - **Regular Queue**：有効にすると、`$queue/<name>/<topic_filter>` のサブスクライブ時に通常キューを自動作成します。

       詳細は[通常キューの自動作成](#auto-create-regular-queues)をご覧ください。

3. 設定変更後は **Save Changes** をクリックして反映します。

### REST API

グローバルなメッセージキュー設定はREST APIからも変更可能です。これらの設定はシステム全体に適用され、すべてのキューの内部管理に影響します。

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### 設定ファイル

永続的かつバージョン管理可能な設定として、EMQX設定ファイル（`emqx.conf`）にメッセージキュー設定を記述できます。以下は主要な設定例です：

```hocon
mq {
    gc_interval = 1h
    regular_queue_retention_period = 1d
    find_queue_retry_interval = 10s
    max_queue_count = 100
    }
}
```

#### 設定説明

- **`gc_interval`**：メッセージキューが期限切れメッセージをクリーンアップする間隔を定義します。
- **`regular_queue_retention_period`**：通常キューでメッセージが保持される最大期間。期間経過後はメッセージが削除されます。
- **`find_queue_retry_interval`**：`$queue/<name>` トピックにサブスクライブ時、キューが見つからない場合の再試行間隔。
- **`max_queue_count`**：（任意）作成可能なキューの最大数を設定します。

## REST APIでキューを管理する

EMQXはメッセージキューのライフサイクル管理（作成、取得、更新、削除）を行うREST APIを提供しています。

::: tip 注意

すべてのREST API操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは[REST API](../../guides/api.md)の「Message Queue」セクションをご参照ください。

:::

以下の例はすべてAPIキーとシークレットによるベーシック認証を前提としています。

### キューを作成する

キュー名、トピックフィルター、Last Value Semanticsの有効化などのキュー属性を指定して新しいキューを作成します。

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"name": "my_queue", "topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
```

レスポンスには作成されたキューの名前や設定詳細が含まれます。

### すべてのキューを一覧表示する

既存のメッセージキュー一覧を取得します。

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

EMQX 6.3.3以降、削除処理中のキューもバックグラウンドクリーンアップ完了までレスポンスに含まれます。これらのキューには削除要求時刻を示すミリ秒単位のUnixタイムスタンプを持つ `deleted_at` フィールドが含まれます。削除中でないキューにはこのフィールドはありません。

### キューを更新する

既存キューの配信戦略などの属性を更新します。

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/my_queue \
-d '{"dispatch_strategy": "least_inflight", "limits": {"max_shard_message_count": 5000, "max_shard_message_bytes": "100MB"}}' | jq
```

### キューを削除する

キューとその中に保持されているメッセージをすべて削除するリクエストを送信します。

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/my_queue
```

EMQX 6.3.3以降、この操作は非同期です。EMQXはキューを削除対象としてマークし、バックグラウンドでクリーンアップをスケジュールし、保存データの削除完了を待たずにHTTPステータスコード `204` を返します。

バックグラウンドクリーンアップ完了までの間：

- EMQXはキューへのパブリッシュを停止し、キューのコンシューマーも停止して再起動しません。
- キューは一覧に `deleted_at` フィールド付きで残り、取得や更新リクエストはHTTPステータスコード `400` を返します。
- 同名の新規キューは作成できません。
- キューは `max_queue_count` のカウントに含まれ続けます。

削除に失敗した場合やノード停止でクリーンアップが実行されなかった場合は、再度 `DELETE` リクエストを送信してクリーンアップを再スケジュールしてください。

::: warning ローリングアップグレード制限

ローリングアップグレード中、削除マーカーを認識しないノードはバックグラウンドクリーンアップ完了までキューへのパブリッシュを継続し、API一覧にもキューを含める可能性があります。

:::

## FAQとトラブルシューティング

### なぜメッセージがキューに格納されないのですか？

- 宣言済みメッセージキューのトピックフィルターが、パブリッシュされたメッセージのトピックにマッチしているか確認してください。
- キューが存在し、正しく設定されているか確認してください。
- EMQXのログを確認し、`mq_` プレフィックスのエントリを中心にキュー関連のエラーや警告を調査してください。

### キューが容量を超えた場合どうなりますか？

EMQXのメッセージキューは複数の容量制限タイプをサポートしています。いずれかの制限を超えた場合、ガベージコレクション（GC）時に古いメッセージから削除され、キューサイズが設定範囲内に戻るまで繰り返されます。

- **時間ベースの制限**：すべてのキューは設定された保持期間の制限を受けます。期間を超えたメッセージは配信対象外となり、GCで自動的に削除されます。

- **サイズベースの制限**：シャードごとに以下の制限を任意で設定可能です：

  - **最大メッセージ数**（`max_shard_message_count`）
  - **最大メッセージ合計サイズ（バイト）**（`max_shard_message_bytes`）

  これらの制限はソフト制限であり、リアルタイムではなくGC時に適用されます。GCサイクル間は一時的に制限を超える場合があります。

  なお、これらの制限は永続ストレージのシャード単位で適用されます。シャード数の設定方法は[シャード数](../../guides/durability/managing-replication.md#number-of-shards)を参照してください。また、サイズ制限は[レプリケーション係数](../../guides/durability/managing-replication.md#replication-factor)を考慮していません。実際の物理ストレージ使用量はレプリケーション係数分増加します。
