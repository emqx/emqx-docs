# メッセージキュー ユーザーガイド

このページでは、EMQXのメッセージキュー機能の実践的な使い方について、キューの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## ダッシュボードからメッセージキューを手動作成する

メッセージキューは、メッセージを格納・配信する前に明示的に宣言（作成）する必要があります。メッセージキューは手動または自動で作成できます。自動作成の詳細は[ダッシュボードからメッセージキューを自動作成する](#automatically-create-message-queue-via-dashboard)をご参照ください。

EMQXダッシュボードを使ってメッセージキューを手動で作成する手順は以下の通りです。

1. 左メニューの **Message Queue** に移動します。

2. ページ上の **Create** ボタンをクリックします。

3. **Create Message Queue** ダイアログで以下の項目を設定します。

   - **Topic Filter**: トピックまたはトピックフィルター（例：`t/1`）を入力します。これはパブリッシュされたメッセージのトピックに基づいてキューに格納するメッセージを決定します。キューはこのトピックフィルターにマッチするすべてのメッセージを収集します。

     キューからメッセージを消費するには、クライアントは`$q/{Topic Filter}`形式のトピックをサブスクライブする必要があります。

   - **Dispatch Strategy**: メッセージをサブスクライバー間でどのように配信するかを選択します。利用可能な戦略は以下の通りです。

     - `Least Inflight Subscriber`: 未アック（未確認）メッセージが最も少ないサブスクライバーを優先します。
     - `Random`: （デフォルト）ランダムにサブスクライバーを選択します。
     - `Round Robin`: 全サブスクライバーに均等に配信を回します。

   - **Data Retention Period**: メッセージをキューに保持する期間を指定します。時間単位（例：日）を設定可能です。

   - **Last Value Semantics**: デフォルトで有効です。有効にすると、同じキューキーを持つ新しいメッセージが、同じキュー内の未消費の以前のメッセージを上書きします。これにより、キーごとに最新のメッセージのみが保持されます。デフォルトのキーはメッセージパブリッシャーのクライアントIDです。

     - **[Queue Key Expression](#queue-key-expression)**: Last Value Semanticsが有効な場合、このフィールドで各メッセージからキーを抽出するための式を定義します。デフォルトは`message.from`（メッセージパブリッシャーのクライアントID）です。このフィールドは[Variform式](../configuration/configuration.md#variform-expressions)で設定可能です。

   - **Max Shard Message Count**: （任意）キューの各シャードに許容される最大メッセージ数を設定します。この設定をオンにして任意の値を入力するか、無制限（`infinity`）にするために無効のままにできます。この設定は永続ストレージに保存されます。

   - **Max Shard Message Bytes**: （任意）キューの各シャードに許容されるメッセージの合計サイズ（バイト単位）を設定します。この設定をオンにして値（例：`200MB`）を入力するか、無制限（`infinity`）にするために無効のままにできます。この設定も永続ストレージに保存されます。

     ::: tip パフォーマンスに関する注意

     サイズ制限があるキューは、特に高スループット時に書き込み性能が低下する可能性があります。

     :::

4. **Create** をクリックしてキューを保存します。

作成したキューはメッセージキュー一覧に表示され、トピックフィルター、ディスパッチ戦略、Last Value Semanticsの状態、データ保持期間が確認できます。キューの設定変更や削除は、**Actions** 列のボタンから行えます。

### Queue Key Expression

Queue Key Expressionは、Last Value Semanticsモードでメッセージの重複排除に使用するキーを抽出する方法を指定します。この式はメッセージのメタデータに対して評価され、[Variform式](../configuration/configuration.md#variform-expressions)の構文に従います。

式は、`from`、`topic`、`payload`、`headers.properties`などのフィールドを含むメッセージコンテキストに対して評価されます。例えば、ユーザープロパティをキーとして使いたい場合は、以下のように設定できます。

```
message.headers.properties.'User-Property'.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しない場合）、メッセージは破棄され、キューに格納されません。

#### メッセージコンテキストの例

Queue Key Expressionは以下のメッセージ構造に対して評価されます。

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

## ダッシュボードからメッセージキューを自動作成する

EMQX 6.0.1以降、クライアントが`$q/`プレフィックス付きトピックをサブスクライブすると、メッセージキューが自動的に作成されるようになりました。これにより手動設定なしで動的にキューをプロビジョニングできます。

自動作成されるキューは、通常のキューまたはLast Value Semanticsキューのいずれかになります。

::: tip 注意

適切なキュー動作を保証するために、**Auto Create Regular Message Queue** と **Auto Create Last Value Semantics Queue** の両方を同時に有効にしないでください。

:::

### Auto Create Last Value Semantics Queue

このオプションはデフォルトで有効になっており、**Management** -> **MQTT Settings** -> **Message Queue** タブで設定可能です。Last Value Semanticsをサポートするキューを自動作成し、キーごとに最新のメッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Message Queue** タブに移動します。

2. デフォルトで **Enable Auto Create Last Value Semantics Queue** が有効です。

   以下を設定できます。

   - **Queue Key Expression**: 必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。
   - **Dispatch Strategy**: メッセージをサブスクライバーに配信する方法を決定します（デフォルト：`Random`）。
   - **Data Retention Period**: キューにメッセージを保持する期間を指定します。

3. **Save Changes** をクリックします。

クライアントが`$q/test`のようなトピックをサブスクライブすると、EMQXは自動的にLast Value Semanticsキューを作成し、**Message Queue**一覧に表示されます。

### Auto Create Regular Message Queue

このオプションは、メッセージが上書きされず独立して格納される通常のキューを自動作成したい場合に手動で有効にできます。

1. **Management** -> **MQTT Settings** -> **Message Queue** タブに移動します。

2. **Enable Auto Create Regular Message Queue** をオンにします。

3. 以下を設定します。

   - **Dispatch Strategy**: メッセージをサブスクライバーに配信する方法（デフォルト：`Random`）。
   - **Data Retention Period**: メッセージの保持期間。

4. **Save Changes** をクリックします。

## メッセージキュー設定の構成

このセクションでは、EMQX内のすべてのメッセージキューに適用されるグローバル設定の方法を説明します。これらの設定はメッセージ保持、クリーンアップ間隔、内部キュー動作、キューの自動作成動作を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQXダッシュボードからメッセージキュー設定を直接更新でき、ブローカーの再起動は不要です。システム全体の動作変更に便利です。

メッセージキューのグローバル設定をダッシュボードで行う手順：

1. **Management** -> **MQTT Settings** -> **Message Queue** タブに移動します。

   または、**Message Queue** ページ右上の **Settings** ボタンをクリックします。

2. **Message Queue** パネルには以下の設定項目があります。

   - **Enable Message Queue**: メッセージキューシステムはデフォルトで有効で、ダッシュボードから無効化できません。

     > 無効化する場合は設定ファイルを直接編集してください。

   - **Max Queue Count**: 作成可能なキューの最大数を設定します。

   - **GC Interval**: 期限切れメッセージをキューからクリーンアップする間隔。デフォルトは1時間。

   - **Regular Queue Retention Period**: 通常キューでメッセージを保持する最大期間。デフォルトは7日間。

   - **Find Queue Retry Interval**: クライアントが`$q/`プレフィックス付きトピックをサブスクライブした際に、対応するキューが存在しない場合の再試行間隔。デフォルトは10秒。

   - **Auto-Creation Options**: EMQXは自動作成機能で動的にキューをプロビジョニング可能です。

     - **Auto Create Last Value Semantics Queue**（デフォルトで有効）: クライアントが`$q/`トピックをサブスクライブし、該当キューがない場合、Last Value Semanticsを有効にしたキューを自動作成します。

       詳細は[Auto Create Last Value Semantics Queue](#auto-create-last-value-semantics-queue)を参照してください。

     - **Auto Create Regular Message Queue**: 上記に代わり有効化可能。通常（上書きなし）キューを`$q/`サブスクライブ時に自動作成します。

       詳細は[Auto Create Regular Message Queue](#auto-create-regular-message-queue)を参照してください。

3. 変更後、**Save Changes** をクリックして設定を適用します。

### REST API

REST APIでもグローバルなメッセージキュー設定を変更可能です。これらの設定はシステム全体に適用され、すべてのキューの管理に影響します。

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### 設定ファイル

永続的かつバージョン管理可能な設定として、EMQX設定ファイル（`emqx.conf`）にメッセージキュー設定を記述できます。以下は主要な設定例です。

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

- **`gc_interval`**: メッセージキューが期限切れメッセージをクリーンアップする間隔を定義します。
- **`regular_queue_retention_period`**: 通常キューでメッセージを保持する最大期間。経過後、メッセージは削除されます。
- **`find_queue_retry_interval`**: クライアントが存在しない`$q/`トピックのキューを探す再試行間隔を決定します。
- **`max_queue_count`**: （任意）作成可能なキューの最大数を設定します。

## REST APIによるメッセージキュー管理

EMQXはメッセージキューの作成、取得、更新、削除などのライフサイクル管理を行うREST APIを提供しています。

### メッセージキューの作成

トピックフィルターやLast Value Semanticsの有効化などのキュー属性を指定して新しいメッセージキューを作成します。

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
```

### すべてのメッセージキュー一覧取得

既存のメッセージキュー一覧を取得します。

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

### メッセージキューの更新

既存キューの属性（例：ディスパッチ戦略）を更新します。

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/t1%2F%23 \
-d '{"dispatch_strategy": "least_inflight", "limits": {"max_shard_message_count": 5000, "max_shard_message_bytes": "100MB"}}' | jq
```

### メッセージキューの削除

メッセージキューとその中に保持されているすべてのメッセージを削除します。

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/t1%2F%23
```

> **注意:**
>
> - URL内のトピックフィルターはURLエンコードが必要です（例：`t1/#`は`t1%2F%23`）。
> - 認証が必要です（`key:secret`）。

## FAQとトラブルシューティング

### なぜメッセージがキューに格納されないのですか？

- 宣言済みメッセージキューのトピックフィルターが、パブリッシュされたメッセージのトピックと一致しているか確認してください。
- キューが存在し、正しく設定されているか確認してください。
- EMQXのログを確認し、`mq_`プレフィックスのエントリを中心にキュー関連のエラーや警告を調査してください。

### キューの容量が超過した場合はどうなりますか？

EMQXのメッセージキューは複数の容量制限タイプをサポートしています。いずれかの制限に達した場合、ガベージコレクション（GC）時に最も古いメッセージから削除され、キューサイズが設定範囲内に戻るまで続けられます。

- **時間ベースの制限**: すべてのキューは設定された保持期間の対象です。保持期間を超えたメッセージは配信対象外となり、GCで自動的に削除されます。

- **サイズベースの制限**: オプションでシャードごとに以下の制限を設定可能です。

  - **最大メッセージ数**（`max_shard_message_count`）
  - **最大メッセージ合計サイズ（バイト）**（`max_shard_message_bytes`）

  これらの制限はソフト制限であり、リアルタイムではなくGC時に適用されます。GCサイクル間は一時的に閾値を超える場合があります。

  なお、これらの制限は永続ストレージのシャード単位で適用されます。シャード数の設定方法は[シャード数](../durability/managing-replication.md#number-of-shards)をご参照ください。また、サイズ制限は[レプリケーションファクター](../durability/managing-replication.md#replication-factor)を考慮していません。実際の物理ストレージ使用量はレプリケーションファクター分だけ増加します。
