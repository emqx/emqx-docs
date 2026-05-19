# メッセージキュー ユーザーガイド

このページでは、EMQXのメッセージキュー機能の実践的な使い方について、キューの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## ダッシュボードからのキューの手動作成

メッセージキューは、メッセージを格納・配信する前に明示的に宣言・作成する必要があります。キューは手動または自動で作成可能です。自動作成の詳細は[ダッシュボードからのメッセージキューの自動作成](#自動的にメッセージキューをダッシュボードから作成する)をご覧ください。

1. 左メニューの **Queues** に移動します。

2. ページ上の **Create** ボタンをクリックします。

3. **Create Queue** ダイアログで以下のオプションを設定します：

   - **Name**：キューの一意な名前を指定します。キュー名には以下の文字のみ使用可能です：

     - 英数字（`A–Z`, `a–z`, `0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でキューは識別・管理されます。

     クライアントは以下のサブスクリプション形式でメッセージを消費できます：

     - キューが既に存在する場合は `$queue/<name>` を使用します。
     - キューが存在する場合にオプションで `$queue/<name>/<topic_filter>` を使用できます。自動作成が有効な場合は、キューが存在しないときに `<topic_filter>` を使ってキューを自動作成します。

   - **Topic Filter**：トピックまたはトピックフィルター（例：`t/1`）を入力します。これはパブリッシュされたメッセージのトピックと照合し、該当するメッセージをキューに格納するための条件を定義します。キューはこのトピックフィルターにマッチするすべてのメッセージを収集します。

     > トピックフィルターはキューの設定の一部ですが、キューの識別子ではありません。

   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法を選択します。利用可能な戦略は以下の通りです：

     - `Least Inflight Subscriber`：未アックのメッセージ数が最も少ないサブスクライバーを優先します。
     - `Random`：（デフォルト）ランダムにサブスクライバーを選択します。
     - `Round Robin`：全サブスクライバーに均等に順番に配信します。

   - **Data Retention Period**：メッセージをキューに保持する期間を指定します。時間単位（例：日）を設定可能です。

   - **Last Value Semantics**：デフォルトで有効です。有効時は、同じキューキーを持つ新しいメッセージが、同じキュー内の未消費の以前のメッセージを上書きします。これにより、キーごとに最新のメッセージのみが保持されます。デフォルトのキーはメッセージパブリッシャーのクライアントIDです。

     - **[Queue Key Expression](#queue-key-expression)**：Last Value Semanticsが有効な場合、このフィールドで各メッセージからキーを抽出する式を定義します。デフォルトは `message.from`（メッセージパブリッシャーのクライアントID）です。このフィールドは[Variform式](../configuration/configuration.md#variform-expressions)で設定可能です。

   - **Max Shard Message Count**：（任意）キューの各シャードに許容される最大メッセージ数を設定します。この設定をオンにして任意の値を入力するか、無制限（`infinity`）のままにできます。この設定は永続ストレージに保存されます。

   - **Max Shard Message Bytes**：（任意）キューの各シャードに許容されるメッセージの合計サイズ（バイト単位）を設定します。設定をオンにして値（例：`200MB`）を入力するか、無制限（`infinity`）のままにできます。この設定も永続ストレージに保存されます。

     ::: tip パフォーマンスに関する注意

     サイズ制限付きのキューは、特に高スループット時に書き込み性能が低下する可能性があります。

     :::

4. **Create** をクリックしてキューを保存します。

新しいキューはキュー一覧に表示され、名前、トピックフィルター、配信戦略、Last Value Semanticsの状態、データ保持期間が確認できます。キューの設定変更や削除は **Actions** 列のボタンから行えます。

### Queue Key Expression

Queue Key Expressionは、Last Value Semanticsモードでメッセージの重複排除に使うキーをどのように抽出するかを指定する式です。この式はメッセージのメタデータに対して評価され、[Variform式](../configuration/configuration.md#variform-expressions)の構文に従います。

式は、`from`、`topic`、`payload`、`headers.properties`などのフィールドを含むメッセージコンテキストに対して評価されます。例えば、ユーザープロパティをキーに使う場合は以下のように設定できます：

```
message.headers.properties.'User-Property'.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しない場合）、メッセージは破棄され、キューに格納されません。

#### メッセージコンテキストの例

Queue Key Expressionは以下のメッセージ構造に対して評価されます：

<details>
<summary><strong>JSON例</strong></summary>

```json
{
  "message": {
    "qos": 0,
    "topic": "some/topic",
    "payload": "some-payload",
    "headers": {
      "client_attrs": {},
      "proto_ver": 5,
      "properties": {
        "User-Property": {
          "user-prop": "some-value"
        }
      },
      "peerhost": "127.0.0.1",
      "username": "undefined",
      "protocol": "mqtt",
      "peername": "127.0.0.1:49352"
    },
    "from": "clientid",
    "timestamp": 1759238376252,
    "id": "..non utf8 bytes...",
    "flags": {
      "retain": false,
      "dup": false
    },
    "extra": {}
  }
}
```

</details>

<details> <summary><strong>Erlangターム例</strong></summary>

```erlang
#{message =>
      #{extra => #{},
        flags => #{dup => false, retain => false},
        id => <<0,6,64,4,154,125,229,77,244,69,0,0,28,21,0,2>>,
        timestamp => 1759238376252, from => <<"clientid">>,
        headers =>
            #{peername => <<"127.0.0.1:49352">>, protocol => mqtt,
              username => undefined, peerhost => <<"127.0.0.1">>,
              properties =>
                  #{'User-Property' => #{<<"user-prop">> => <<"some-value">>}},
              proto_ver => 5, client_attrs => #{}
            },
        payload => <<"some-payload">>, topic => <<"some/topic">>,
        qos => 0
      }
    }
```

</details>

## ダッシュボードからのメッセージキューの自動作成

クライアントが `$queue/` プレフィックス付きのトピックをサブスクライブするときに、メッセージキューを自動的に作成できます。これにより手動設定なしでキューを動的にプロビジョニング可能です。

自動作成が有効な場合：

- `$queue/<name>` へのサブスクライブはキューが既に存在する場合のみ成功します。
- `$queue/<name>/<topic_filter>` へのサブスクライブは、キューが存在しない場合に `<topic_filter>` を使ってEMQXがキューを自動作成します。

キューは通常のキューまたはLast Value Semanticsキューとして自動作成されます。

::: tip 注意

適切なキュー動作を確保するため、**Auto Create Regular Queue** と **Auto Create Last Value Semantics Queue** は同時に有効にしないでください。

:::

### Last Value Semanticsキューの自動作成

このオプションはデフォルトで **MQTT Settings** の **Queues** タブで有効になっています。Last Value Semanticsをサポートするキューを自動作成し、キーごとに最新のメッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. デフォルトで **Enable Auto Create Queue** -> **Last Value Semantics Queue** が有効です。

   以下を設定します：

   - **Queue Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。
   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージをキューに保持する期間。

3. **Save Changes** をクリックします。

クライアントが `$queue/my_queue/test` のようなトピックをサブスクライブし、`my_queue` が存在しない場合、EMQXは `test` をトピックフィルターとして使い、Last Value Semanticsキュー `my_queue` を自動作成します。キューは **Queues** リストに表示されます。

### 通常キューの自動作成

メッセージを上書きせず独立して保存する通常キューを自動作成したい場合に手動で有効化できます。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. **Enable Auto Create Queue** -> **Regular Queue** をオンにします。

3. 以下を設定します：

   - **Dispatch Strategy**：メッセージ配信方法（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージ保持期間。

4. **Save Changes** をクリックします。

## キュー設定の構成

このセクションでは、EMQX内のすべてのメッセージキューに適用されるグローバル設定の方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部キュー動作、自動作成の挙動を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQXダッシュボードからメッセージキュー設定を直接更新でき、ブローカーの再起動は不要です。システム全体の動作をランタイムで変更する際に便利です。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

   または、**Queues** ページ右上の **Settings** ボタンをクリックします。

2. **Queues** パネルで以下の設定が可能です：

   - **Enable Queues**：メッセージキュー機能を有効化します。

     > ダッシュボードからは無効化できません。無効化する場合は設定ファイルを直接編集してください。

   - **Max Queue Count**：作成可能なキューの最大数を設定します。

   - **GC Interval**：期限切れメッセージをキューからクリーンアップする間隔。デフォルトは `1` 時間です。

   - **Regular Queue Retention Period**：通常キューでメッセージを保持する最大期間。デフォルトは `7` 日です。

   - **Find Queue Retry Interval**：クライアントが `$queue/<name>` をサブスクライブし該当キューが見つからない場合、再試行する間隔。デフォルトは `10` 秒です。

   - **Enable Auto Create Queue**：キューが存在しない場合にクライアントのサブスクライブに応じてキューを自動作成します。

   - **Auto Create Queue Type**：自動作成するキューのタイプを指定します：

     - **Last Value Semantics Queue**（デフォルト有効）：`$queue/<name>/<topic_filter>` へのサブスクライブ時に該当キューがなければLast Value Semanticsキューを自動作成します。

       詳細は[Last Value Semanticsキューの自動作成](#auto-create-last-value-semantics-queues)をご覧ください。

     - **Regular Queue**：有効にすると、`$queue/<name>/<topic_filter>` へのサブスクライブ時に通常キューを自動作成します。

       詳細は[通常キューの自動作成](#auto-create-regular-queues)をご覧ください。

3. 変更後、**Save Changes** をクリックして設定を適用します。

### REST API

REST APIを使ってグローバルなメッセージキュー設定を構成することも可能です。これらの設定はシステム全体に適用され、すべてのキューの内部管理に影響します。

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### 設定ファイル

永続的かつバージョン管理可能な設定のために、EMQXの設定ファイル（`emqx.conf`）にメッセージキュー設定を記述できます。以下は主要な設定例です：

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
- **`find_queue_retry_interval`**：`$queue/<name>` トピックをサブスクライブした際にキューが見つからない場合、再試行する間隔を決定します。
- **`max_queue_count`**：（任意）作成可能なキューの最大数を設定します。

## REST APIによるキュー管理

EMQXはメッセージキューのライフサイクル管理（作成、取得、更新、削除）を行うREST APIを提供しています。

::: tip 注意

すべてのREST API操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは[REST API](../admin/api.md)の「Message Queue」セクションをご参照ください。

:::

以下の例はAPIキーとシークレットによるベーシック認証を前提としています。

### キューの作成

キュー名、トピックフィルター、Last Value Semanticsの有効/無効などのキュー属性を指定して新しいメッセージキューを作成します：

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

メッセージキューとその保持中のすべてのメッセージを削除します：

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/my_queue
```

削除後、キューは新規メッセージの受け入れを停止し、保存データは消去されます。

## FAQおよびトラブルシューティング

### メッセージがキューに格納されないのはなぜですか？

- 宣言済みのメッセージキューのトピックフィルターが、パブリッシュされたメッセージのトピックと一致しているか確認してください。
- キューが存在し、正しく設定されていることを確認してください。
- EMQXのログを確認し、`mq_` プレフィックスのエントリを中心にキュー関連のエラーや警告を探してください。

### キューが容量を超えた場合はどうなりますか？

EMQXのメッセージキューは複数の容量制限タイプをサポートしています。いずれかの制限に達した場合、GC（ガベージコレクション）時に最も古いメッセージから順に削除され、キューサイズが設定範囲内に戻るまで処理されます。

- **時間ベースの制限**：すべてのキューは設定された保持期間の対象です。保持期間を超えたメッセージは配信対象外となり、GCで自動的に削除されます。

- **サイズベースの制限**：オプションで各シャードごとに以下の制限を設定可能です：

  - **最大メッセージ数**（`max_shard_message_count`）
  - **最大メッセージ合計サイズ（バイト）**（`max_shard_message_bytes`）

  これらの制限はリアルタイムではなくGC時に適用されるソフトリミットです。GCサイクル間は一時的に制限を超える場合があります。

  なお、これらの制限は永続ストレージの各シャード単位で適用されます。シャード数の設定方法については[シャード数](../durability/managing-replication.md#number-of-shards)を参照してください。また、サイズ制限は[レプリケーション係数](../durability/managing-replication.md#replication-factor)を考慮していません。実際の物理ストレージ使用量はレプリケーション係数分だけ増加します。
