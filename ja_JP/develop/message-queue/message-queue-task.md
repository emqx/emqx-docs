# メッセージキュー ユーザーガイド

このページでは、EMQXのメッセージキュー機能の実践的な使い方を説明します。キューの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## ダッシュボードからメッセージキューを手動で作成する

メッセージキューは、メッセージを格納または配信する前に明示的に宣言／作成する必要があります。キューは手動または自動で作成できます。自動作成の詳細は[ダッシュボードからメッセージキューを自動作成する](#automatically-create-message-queue-via-dashboard)をご覧ください。

EMQXダッシュボードでメッセージキューを手動で作成する手順は以下の通りです。

1. 左メニューの **Message Queue** に移動します。

2. ページ上の **Create** ボタンをクリックします。

3. **Create Message Queue** ダイアログで以下のオプションを設定します。

   - **Topic Filter**：トピックまたはトピックフィルター（例：`t/1`）を入力します。これはパブリッシュされたメッセージのトピックに基づいてキューに格納されるメッセージを定義します。キューはこのトピックフィルターにマッチするすべてのメッセージを収集します。

     キューからメッセージを消費するには、クライアントは `$q/{Topic Filter}` 形式のトピックをサブスクライブする必要があります。

   - **Dispatch Strategy**：メッセージをサブスクライバー間でどのように配信するかを選択します。利用可能な戦略は以下の通りです。

     - `Least Inflight Subscriber`：未アックのメッセージ数が最も少ないサブスクライバーを優先。
     - `Random`：（デフォルト）ランダムにサブスクライバーを選択。
     - `Round Robin`：すべてのサブスクライバーに均等に配信を回す。

   - **Data Retention Period**：キュー内のメッセージを保持する期間を指定します。時間単位（例：日）を設定可能です。

   - **Last Value Semantics**：デフォルトで有効です。有効にすると、同じキューキーを持つ新しいメッセージが、未消費の以前のメッセージを上書きします。これにより、キーごとに最新のメッセージのみが保持されます。デフォルトのキーはメッセージパブリッシャーのクライアントIDです。

     - **[Queue Key Expression](#queue-key-expression)**：Last Value Semanticsが有効な場合、このフィールドで各メッセージからキーを抽出するための式を定義します。デフォルトは `message.from`（メッセージパブリッシャーのクライアントID）です。このフィールドは[Variform式](../../guides/configuration/configuration.md#variform-expressions)で設定可能です。

   - **Max Shard Message Count**：（任意）キューの各シャードに許容される最大メッセージ数を設定します。この設定をオンにしてカスタム値を入力するか、無制限（`infinity`）にするために無効のままにできます。この設定は永続ストレージに保存されます。

   - **Max Shard Message Bytes**：（任意）キューの各シャードに許容されるメッセージの合計サイズ（バイト単位）を設定します。この設定をオンにして値（例：`200MB`）を入力するか、無制限（`infinity`）にするために無効のままにできます。この設定も永続ストレージに保存されます。

     ::: tip パフォーマンスに関する注意

     サイズ制限付きのキューは、特に高スループット環境下で書き込み性能が低下する可能性があります。

     :::

4. **Create** をクリックしてキューを保存します。

新しいキューはメッセージキュー一覧に表示され、トピックフィルター、配信戦略、Last Value Semanticsの状態、データ保持期間が確認できます。キューの設定変更や削除は **Actions** 列のボタンから行えます。

### Queue Key Expression

Queue Key Expressionは、Last Value Semanticsモードでメッセージの重複排除に使うキーを抽出する方法を指定します。この式はメッセージのメタデータに対して評価され、[Variform式](../../guides/configuration/configuration.md#variform-expressions)の構文に従います。

式は以下のようなメッセージコンテキストに対して評価されます。例としてユーザープロパティをキーにする場合は、以下のように設定します。

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

EMQX 6.0.1以降、クライアントが `$q/` プレフィックス付きトピックをサブスクライブすると、メッセージキューが自動的に作成されるようになりました。これにより手動設定なしでキューを動的にプロビジョニングできます。

キューは通常のキューまたはLast Value Semanticsキューとして自動作成されます。

::: tip 注意

適切なキュー動作を確保するために、**Auto Create Regular Message Queue** と **Auto Create Last Value Semantics Queue** は同時に有効にしないでください。

:::

### Auto Create Last Value Semantics Queue

このオプションはデフォルトで **MQTT Settings** の **Message Queue** タブにて有効になっています。Last Value Semanticsをサポートするキューを自動作成し、キーごとに最新のメッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Message Queue** タブに移動します。

2. デフォルトで **Enable Auto Create Last Value Semantics Queue** が有効です。

   以下を設定します。

   - **Queue Key Expression**：必須。各メッセージからユニークキーを抽出する方法を定義します（デフォルト：`message.from`）。
   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法を決定します（デフォルト：`Random`）。
   - **Data Retention Period**：キュー内のメッセージ保持期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$q/test` のようなトピックをサブスクライブすると、EMQXは自動的にLast Value Semanticsキューを作成し、**Message Queue** 一覧に表示されます。

### Auto Create Regular Message Queue

このオプションは、メッセージが上書きされず独立して保存される通常のキューを自動作成したい場合に手動で有効化できます。

1. **Management** -> **MQTT Settings** -> **Message Queue** タブに移動します。

2. **Enable Auto Create Regular Message Queue** をオンにします。

3. 以下を設定します。

   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法（デフォルト：`Random`）。
   - **Data Retention Period**：キュー内のメッセージ保持期間。

4. **Save Changes** をクリックします。

## メッセージキューの設定を構成する

このセクションでは、EMQX内のすべてのメッセージキューに適用されるグローバル設定の方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部キュー動作、キューの自動作成動作を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQXダッシュボードからメッセージキューの設定を直接変更でき、ブローカーの再起動は不要です。システム全体の動作をランタイムで変更する際に便利です。

ダッシュボードでメッセージキューのグローバル設定を行う手順：

1. **Management** -> **MQTT Settings** -> **Message Queue** タブに移動します。

   または、**Message Queue** ページ右上の **Settings** ボタンをクリックします。

2. **Message Queue** パネルで以下の設定項目があります。

   - **Enable Message Queue**：メッセージキューシステムはデフォルトで有効で、ダッシュボードから無効化できません。

     > 無効化する場合は設定ファイルを直接変更してください。

   - **Max Queue Count**：作成可能な最大キュー数を設定します。

   - **GC Interval**：期限切れメッセージをキューからクリーンアップする間隔。デフォルトは1時間。

   - **Regular Queue Retention Period**：通常キューでメッセージを保持する最大期間。デフォルトは7日。

   - **Find Queue Retry Interval**：クライアントが `$q/` プレフィックストピックをサブスクライブし、対応するキューがまだ存在しない場合に、キューを再検索する間隔。デフォルトは10秒。

   - **Auto-Creation Options**：EMQXは自動作成機能による動的キュープロビジョニングをサポートします。

     - **Auto Create Last Value Semantics Queue**（デフォルトで有効）：クライアントが `$q/` トピックをサブスクライブし、対応するキューが存在しない場合、Last Value Semanticsを有効にしたキューを自動作成します。

       詳細は[Auto Create Last Value Semantics Queue](#auto-create-last-value-semantics-queue)を参照してください。

     - **Auto Create Regular Message Queue**：上記の代わりに有効化可能です。有効にすると、通常の（上書きされない）キューを自動作成します。

       詳細は[Auto Create Regular Message Queue](#auto-create-regular-message-queue)を参照してください。

3. 設定を変更したら **Save Changes** をクリックして適用します。

### REST API

REST APIを使ってもグローバルなメッセージキュー設定を構成できます。これらの設定はシステム全体に適用され、すべてのキューの内部管理に影響します。

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

- **`gc_interval`**：メッセージキューが期限切れメッセージをクリーンアップする間隔を定義します。
- **`regular_queue_retention_period`**：通常キューでメッセージを保持する最大期間を設定します。この期間を過ぎたメッセージは削除されます。
- **`find_queue_retry_interval`**：クライアントが `$q/` トピックをサブスクライブし、対応するキューが存在しない場合に再検索する頻度を決定します。
- **`max_queue_count`**：（任意）作成可能な最大キュー数を設定します。

## REST APIでメッセージキューを管理する

EMQXはメッセージキューのライフサイクル管理のためのREST APIを提供しています。作成、取得、更新、削除が可能です。

### メッセージキューを作成する

トピックフィルターやLast Value Semanticsの有効化などのキュー属性を指定して新しいキューを作成します。

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
-d '{"topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
```

### すべてのメッセージキューを一覧表示する

既存のメッセージキュー一覧を取得します。

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

### メッセージキューを更新する

既存キューの属性（例：配信戦略）を更新します。

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/t1%2F%23 \
-d '{"dispatch_strategy": "least_inflight", "limits": {"max_shard_message_count": 5000, "max_shard_message_bytes": "100MB"}}' | jq
```

### メッセージキューを削除する

キューとその中に保持されているすべてのメッセージを削除します。

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/t1%2F%23
```

> **注意：**
>
> - URL内のトピックフィルターはURLエンコードが必要です（例：`t1/#` は `t1%2F%23`）。
> - 認証が必要です（`key:secret`）。

## FAQとトラブルシューティング

### メッセージがキューに格納されないのはなぜですか？

- 宣言済みのメッセージキューのトピックフィルターが、パブリッシュされたメッセージのトピックに一致しているか確認してください。
- キューが存在し、正しく設定されていることを確認してください。
- EMQXのログを確認し、`mq_` プレフィックスのエントリを中心にキュー関連のエラーや警告を調査してください。

### キューが容量を超えた場合はどうなりますか？

EMQXのメッセージキューは複数の容量制限タイプをサポートしています。いずれかの制限に達すると、ガベージコレクション（GC）時に最も古いメッセージから順に削除され、キューサイズが設定範囲内に戻るまで処理されます。

- **時間ベースの制限**：すべてのキューは設定された保持期間の制限を受けます。保持期間を超えたメッセージは配信対象外となり、GCで自動的に削除されます。

- **サイズベースの制限**：オプションでシャードごとに以下の制限を設定可能です。

  - **最大メッセージ数**（`max_shard_message_count`）
  - **最大メッセージ合計サイズ（バイト）**（`max_shard_message_bytes`）

  これらの制限はソフト制限であり、リアルタイムではなくGC時に適用されます。GCサイクル間は一時的に制限を超える場合があります。

  なお、これらの制限は永続ストレージのシャード単位で適用されます。シャード数の設定方法は[シャード数](../../guides/durability/managing-replication.md#number-of-shards)を参照してください。また、サイズ制限は[レプリケーションファクター](../../guides/durability/managing-replication.md#replication-factor)を考慮していません。実際の物理ストレージ使用量はレプリケーションファクター分だけ増加します。
