# メッセージキュー ユーザーガイド

<<<<<<< HEAD
このページでは、EMQXのメッセージキュー機能の実践的な使い方について、キューの作成から動作設定、ダッシュボードやREST API、設定ファイルを使った管理方法までを解説します。

## ダッシュボードからのキュー手動作成

メッセージキューは、メッセージを格納・配信する前に明示的に宣言・作成する必要があります。キューは手動または自動で作成可能です。自動作成の詳細は[ダッシュボードからのメッセージキュー自動作成](#automatically-create-message-queues-via-dashboard)をご覧ください。

1. 左メニューの **Queues** に移動します。
=======
このページでは、EMQXのメッセージキュー機能の実践的な使い方を解説します。キューの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを順を追って説明します。

## ダッシュボードからメッセージキューを手動で作成する

メッセージキューは、メッセージを格納または配信する前に明示的に宣言／作成する必要があります。キューは手動または自動で作成可能です。自動作成の詳細は[ダッシュボードからメッセージキューを自動作成する](#automatically-create-message-queues-via-dashboard)をご覧ください。

EMQXダッシュボードを使ってメッセージキューを手動で作成する手順は以下の通りです。
>>>>>>> origin/release-6.1

2. ページ上の **Create** ボタンをクリックします。

3. **Create Queue** ダイアログで以下のオプションを設定します：

<<<<<<< HEAD
   - **Name**：キューの一意の名前を指定します。キュー名には以下の文字のみ使用可能です：
=======
   - **Name**：キューの一意の名前を指定します。キュー名に使用できる文字は以下の通りです：
>>>>>>> origin/release-6.1

     - 英数字（`A–Z`、`a–z`、`0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でキューは識別・管理されます。

     クライアントは以下のサブスクリプション形式でメッセージを消費できます：

<<<<<<< HEAD
     - `$queue/<name>`：キューが既に存在する場合に使用します。
     - `$queue/<name>/<topic_filter>`：既存のキューにサブスクライブする際は任意です。自動作成が有効な場合に使用可能です。キューがまだ存在しない場合、EMQXは指定された `<topic_filter>` を使って自動的にキューを作成します。

   - **Topic Filter**：トピックまたはトピックフィルター（例：`t/1`）を入力します。これは、パブリッシュされたメッセージのトピックと照合してキューに格納するメッセージを決定します。キューはこのトピックフィルターにマッチするすべてのメッセージを収集します。
=======
     - `$queue/<name>` はキューが既に存在する場合に使用します。
     - `$queue/<name>/<topic_filter>` は既存キューへのサブスクライブ時に任意で使用可能です。自動作成が有効な場合に利用できます。キューがまだ存在しない場合、EMQXは指定された `<topic_filter>` を使ってキューを自動作成します。

   - **Topic Filter**：トピックまたはトピックフィルター（例：`t/1`）を入力します。パブリッシュされたメッセージのトピックとマッチしたものがキューに格納されます。キューはこのトピックフィルターにマッチするすべてのメッセージを収集します。
>>>>>>> origin/release-6.1

     > トピックフィルターはキューの設定の一部ですが、キューの識別子ではありません。

   - **Dispatch Strategy**：メッセージをサブスクライバー間でどのように配信するかを選択します。利用可能な戦略は以下の通りです：

     - `Least Inflight Subscriber`：未アックのメッセージが最も少ないサブスクライバーを優先します。
     - `Random`：（デフォルト）ランダムにサブスクライバーを選択します。
<<<<<<< HEAD
     - `Round Robin`：すべてのサブスクライバーに均等に順番に配信します。

   - **Data Retention Period**：メッセージをキューに保持する期間を指定します。時間単位（例：日）で設定可能です。

   - **Last Value Semantics**：デフォルトで有効です。有効時は、同じキューキーを持つ新しいメッセージが、同じキュー内の未消費の以前のメッセージを上書きします。これにより、キーごとに最新のメッセージのみが保持されます。デフォルトのキーはメッセージパブリッシャーのクライアントIDです。

     - **[Queue Key Expression](#queue-key-expression)**：Last-Value Semanticsが有効な場合、このフィールドで各メッセージからキーを抽出するための式を定義します。デフォルトは `message.from`（メッセージパブリッシャーのクライアントID）です。このフィールドは[Variform式](../configuration/configuration.md#variform-expressions)で設定可能です。

   - **Max Shard Message Count**：（任意）キューの各シャードに許容される最大メッセージ数を設定します。この設定をオンにして任意の値を入力するか、無制限（`infinity`）のままにできます。この設定は永続ストレージに保存されます。

   - **Max Shard Message Bytes**：（任意）キューの各シャードに許容されるメッセージの合計最大サイズ（バイト単位）を設定します。この設定をオンにして値（例：`200MB`）を入力するか、無制限（`infinity`）のままにできます。この設定も永続ストレージに保存されます。

     ::: tip パフォーマンス注意点

     サイズ制限付きのキューは、特に高スループット環境下で書き込み性能が低下する可能性があります。
=======
     - `Round Robin`：すべてのサブスクライバーに均等に配信を回します。

   - **Data Retention Period**：キュー内のメッセージを保持する期間を指定します。時間単位（例：日）を設定可能です。

   - **Last Value Semantics**：デフォルトで有効です。有効時は同じキューキーを持つ新しいメッセージが、同じキュー内の未消費の以前のメッセージを上書きします。これにより、キーごとに最新のメッセージのみが保持されます。デフォルトのキーはメッセージパブリッシャーのクライアントIDです。キューキーの設定例は以下を参照してください。

     - **[Queue Key Expression](#queue-key-expression)**：Last-Value Semanticsが有効な場合、このフィールドで各メッセージからキーを抽出するための式を定義します。デフォルトは `message.from`（メッセージパブリッシャーのクライアントID）です。このフィールドは[Variform式](../configuration/configuration.md#variform-expressions)による設定をサポートします。

   - **Max Shard Message Count**：（任意）キューの各シャードに許容される最大メッセージ数を設定します。この設定を有効にしてカスタム値を入力するか、無効にして無制限（`infinity`）にできます。設定は永続ストレージに保存されます。

   - **Max Shard Message Bytes**：（任意）キューの各シャードに許容されるメッセージの合計サイズ（バイト単位）を設定します。この設定を有効にして値（例：`200MB`）を入力するか、無効にして無制限（`infinity`）にできます。設定は永続ストレージに保存されます。

     ::: tip パフォーマンスに関する注意

     サイズ制限付きのキューは、特に高スループット時に書き込み性能が低下する可能性があります。
>>>>>>> origin/release-6.1

     :::

4. **Create** をクリックしてキューを保存します。

<<<<<<< HEAD
新しいキューはキュー一覧に表示され、名前、トピックフィルター、配信戦略、Last-Value Semanticsの状態、データ保持期間が確認できます。**Actions** 列のボタンからキュー設定の編集や削除が可能です。

### Queue Key Expression

Queue Key Expressionは、Last-Value Semanticsモードでメッセージの重複排除に使うキーをどのように抽出するかを指定します。この式はメッセージのメタデータに対して評価され、[Variform式](../configuration/configuration.md#variform-expressions)の構文に従います。

式は以下のようなメッセージコンテキストに対して評価されます。例としてユーザープロパティをキーにする場合は以下のように設定します：

```
message.headers.properties.'User-Property'.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しない場合）、メッセージは破棄され、キューに格納されません。

#### メッセージコンテキスト例

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

## ダッシュボードからのキュー自動作成

クライアントが `$queue/` プレフィックス付きのトピックにサブスクライブすると、メッセージキューを自動的に作成できます。これにより手動設定なしで動的にキューをプロビジョニング可能です。

自動作成が有効な場合：

- `$queue/<name>` へのサブスクライブは、キューが既に存在する場合のみ有効です。
- `$queue/<name>/<topic_filter>` へのサブスクライブは、キューが存在しない場合に `<topic_filter>` を使ってEMQXが自動的にキューを作成します。

キューは通常のキューまたはLast-Value Semanticsキューとして自動作成されます。

::: tip 注意

適切なキュー動作を保証するため、**Auto Create Regular Queue** と **Auto Create Last Value Semantics Queue** の両方を同時に有効にすることはできません。

:::

### Last Value Semanticsキューの自動作成

このオプションはデフォルトで **MQTT Settings** の **Queues** タブにて有効になっています。Last-Value Semantics対応のキューを自動作成し、キーごとに最新のメッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

2. デフォルトで **Enable Auto Create Queue** -> **Last Value Semantics Queue** が有効です。

   以下を設定します：

   - **Queue Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。
   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法を決定します（デフォルト：`Random`）。
   - **Data Retention Period**：キューにメッセージを保持する期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$queue/my_queue/test` のようなトピックにサブスクライブすると、`my_queue` が存在しない場合はEMQXが自動的に `test` をトピックフィルターとするLast-Value Semanticsキュー `my_queue` を作成します。キューは **Queues** 一覧に表示されます。

### 通常キューの自動作成

メッセージを上書きせず独立して保存する通常のキューを自動作成したい場合は、このオプションを手動で有効にします。
=======
新しいキューはキュー一覧に表示され、名前、トピックフィルター、ディスパッチ戦略、Last-Value Semanticsの状態、データ保持期間が確認できます。キュー設定の編集や削除は **Actions** 列のボタンから行えます。

## Queue Key Expression

Queue Key Expressionは、Last-Value Semanticsモードでメッセージの重複排除に使うキーをどのように抽出するかを指定します。この式はメッセージのデータに対して評価され、[Variform式](../configuration/configuration.md#variform-expressions)の構文に従います。

式はメッセージコンテキスト（`from`、`topic`、`payload`、`headers.properties`などのフィールドを含む）に対して評価されます。例えば、ユーザープロパティをキーに使う場合は以下のように設定します：

```
message.headers.properties.User-Property.user-prop
```

式に基づいてキーを抽出できない場合（例：フィールドが存在しないなど）、メッセージは破棄され、キューに格納されません。

### メッセージコンテキスト例

<!--@include: ../shared/key-expression-message-context.md-->

### Queue Key Expressionの例

#### 例1

以下の条件でキューを設定したとします：

- Last-Value Semantics有効
- トピックフィルター：`t/#`
- Queue Key Expression：`message.headers.properties.User-Property.mq-key`

以下のメッセージがEMQXにパブリッシュされ、クライアントは存在せず消費されていないと仮定します：

| N | 送信元 | トピック | ユーザープロパティ `mq-key` |
|---|--------|----------|------------------------------|
| 1 | `client1` | `t/1` | `keyA` |
| 2 | `client1` | `t/2` | `keyB` |
| 3 | `client2` | `t/3` | `keyA` |
| 4 | `client2` | `t/4` | `keyB` |

クライアントが接続してキューにサブスクライブすると、以下のメッセージが配信されます：

| N | 送信元 | トピック | ユーザープロパティ `mq-key` |
|---|--------|----------|------------------------------|
| 3 | `client2` | `t/3` | `keyA` |
| 4 | `client2` | `t/4` | `keyB` |

同じ `message.headers.properties.User-Property.mq-key` の値ごとに最新のメッセージのみがキューに保持されます。キー式はトピックを跨いでキュー全体に適用されるため、`keyA` のメッセージが `t/1` にパブリッシュされた後、`t/3` にパブリッシュされたメッセージで上書きされます。

#### 例2

以下の条件でキューを設定したとします：

- Last-Value Semantics有効
- トピックフィルター：`t/#`
- Queue Key Expression：`message.from`

例1と同じメッセージがパブリッシュされた場合、クライアントが接続してキューにサブスクライブすると以下のメッセージが配信されます：

| N | 送信元 | トピック | ユーザープロパティ `mq-key` |
|---|--------|----------|------------------------------|
| 2 | `client1` | `t/2` | `keyB` |
| 4 | `client2` | `t/4` | `keyB` |

同じ `message.from` の値を持つメッセージは相互に上書きされるため、送信元ごとに最新のメッセージのみが保持されます。

#### 例3

以下の条件でキューを設定したとします：

- Last-Value Semantics有効
- トピックフィルター：`t/#`
- Queue Key Expression：`concat(message.headers.properties.User-Property.mq-key, '-', message.topic)`

以下のメッセージがEMQXにパブリッシュされたとします：

| N | 送信元 | トピック | ユーザープロパティ `mq-key` |
|---|--------|----------|------------------------------|
| 1 | `client1` | `t/1` | `keyA` |
| 2 | `client1` | `t/2` | `keyB` |
| 3 | `client1` | `t/1` | `keyB` |
| 4 | `client1` | `t/2` | `keyA` |

クライアントが接続してキューにサブスクライブすると、すべてのメッセージが配信されます。なぜなら、`message.headers.properties.User-Property.mq-key` と `message.topic` の組み合わせが各メッセージでユニークだからです：

| N | 送信元 | トピック | ユーザープロパティ `mq-key` | 計算されたキー |
|---|--------|----------|------------------------------|----------------|
| 1 | `client1` | `t/1` | `keyA` | `keyA-t/1` |
| 2 | `client1` | `t/2` | `keyB` | `keyB-t/2` |
| 3 | `client1` | `t/1` | `keyB` | `keyB-t/1` |
| 4 | `client1` | `t/2` | `keyA` | `keyA-t/2` |

## ダッシュボードからキューを自動作成する

クライアントが `$queue/` プレフィックス付きのトピックにサブスクライブするときに、メッセージキューを自動的に作成できます。これにより、手動設定なしで動的にキューをプロビジョニング可能です。

EMQX 6.0.1以降、クライアントが `$q/` プレフィックス付きのトピックをサブスクライブすると、メッセージキューが自動的に作成されるようになりました。これにより手動設定なしで動的にキューをプロビジョニングできます。

- `$queue/<name>` へのサブスクライブは、キューが既に存在する場合のみ機能します。
- `$queue/<name>/<topic_filter>` へのサブスクライブは、キューが存在しない場合に指定された `<topic_filter>` を使ってEMQXがキューを自動作成します。

キューは通常のキューまたはLast-Value Semanticsキューとして自動作成できます。

::: tip 注意

キューの動作を正しくするために、**Auto Create Regular Queue** または **Auto Create Last Value Semantics Queue** のいずれか一方のみを有効にしてください。両方同時の有効化は推奨されません。

:::

### Auto Create Last Value Semantics Queue

このオプションはデフォルトで **Management** -> **MQTT Settings** -> **Queues** タブの中で有効になっています。これにより、Last-Value Semanticsをサポートするキューが自動作成され、キーごとに最新のメッセージのみが保持されます。

1. **Management** -> **MQTT Settings** -> **Message Queue** タブに移動します。

2. デフォルトで **Enable Auto Create Last Value Semantics Queue** が有効です。

   以下を設定します。

   - **Queue Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。
   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法を決定します（デフォルト：`Random`）。
   - **Data Retention Period**：キュー内のメッセージを保持する期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$queue/my_queue/test` のようなトピックにサブスクライブすると、`my_queue` が存在しない場合はEMQXが `test` をトピックフィルターとしてLast-Value Semanticsキューを自動作成します。作成されたキューは **Queues** 一覧に表示されます。

### Auto Create Regular Message Queue

メッセージを上書きせず独立して保存する通常キューを好む場合、このオプションを手動で有効にできます。
>>>>>>> origin/release-6.1

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。
2. **Enable Auto Create Queue** -> **Regular Queue** をオンにします。
3. 以下を設定します：
<<<<<<< HEAD
   - **Dispatch Strategy**：メッセージ配信方法（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージ保持期間。
4. **Save Changes** をクリックします。

## キュー設定の構成

このセクションでは、EMQXのすべてのメッセージキューに適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部キューの動作、キュー自動作成の挙動を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQXダッシュボードからメッセージキュー設定を直接更新でき、ブローカーの再起動は不要です。システム全体の動作変更に便利です。

1. **Management** -> **MQTT Settings** -> **Queues** タブに移動します。

   または、**Queues** ページ右上の **Settings** ボタンをクリックします。

2. **Queues** パネルで以下の設定項目があります：
   - **Enable Queues**：メッセージキュー機能を有効化します。

     > ダッシュボードからはキュー機能を無効化できません。無効化する場合は設定ファイルを直接編集してください。

   - **Max Queue Count**：作成可能なキューの最大数を設定します。

   - **GC Interval**：期限切れメッセージをキューからクリーンアップする間隔。デフォルトは `1` 時間です。

   - **Regular Queue Retention Period**：通常キューでメッセージを保持する最大期間。デフォルトは `7` 日です。

   - **Find Queue Retry Interval**：クライアントが `$queue/<name>` にサブスクライブした際、対応するキューが見つからない場合に再試行する間隔。デフォルトは `10` 秒です。

   - **Enable Auto Create Queue**：クライアントがキュートピックにサブスクライブし、該当キューが存在しない場合に自動作成を有効にします。

   - **Auto Create Queue Type**：自動作成するキューのタイプを指定します：

     - **Last Value Semantics Queue**（デフォルト有効）：クライアントが `$queue/<name>/<topic_filter>` にサブスクライブし該当キューがない場合、Last-Value Semanticsを有効にしたキューを自動作成します。

       詳細は[Last Value Semanticsキューの自動作成](#auto-create-last-value-semantics-queues)を参照してください。

     - **Regular Queue**：有効にすると、`$queue/<name>/<topic_filter>` のサブスクライブ時に通常キューを自動作成します。

       詳細は[通常キューの自動作成](#auto-create-regular-queues)を参照してください。
   
3. 変更後は **Save Changes** をクリックして設定を反映します。

### REST API

REST APIでもグローバルなメッセージキュー設定を構成可能です。これらの設定はシステム全体に適用され、すべてのキューの内部管理に影響します。
=======
   - **Dispatch Strategy**：メッセージをサブスクライバーに配信する方法（デフォルト：`Random`）。
   - **Data Retention Period**：メッセージの保持期間。
4. **Save Changes** をクリックします。

## メッセージキュー設定の構成

このセクションでは、EMQX内のすべてのメッセージキューに適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部キューの動作、キューの自動作成動作を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQXダッシュボードからメッセージキューの設定を直接更新でき、ブローカーの再起動は不要です。システム全体の動作をランタイムで変更する際に便利です。

ダッシュボードでグローバル設定を構成する手順：

1. **Management** -> **MQTT Settings** -> **Message Queue** タブに移動します。

2. **Queues** パネルで以下の設定が可能です：
   - **Enable Queues**：メッセージキュー機能を有効化します。

     > ダッシュボードからはキュー機能を無効化できません。無効化するには設定ファイルを直接編集してください。

     > 無効化するには設定ファイルを直接編集してください。

   - **Max Queue Count**: 作成可能なキューの最大数を設定します。

   - **GC Interval**: 有効期限切れメッセージをキューからクリーンアップする間隔。デフォルトは1時間。

   - **Regular Queue Retention Period**: 通常キューでメッセージを保持する最大期間。デフォルトは7日。

   - **Enable Auto Create Queue**：キューが存在しない場合にクライアントのサブスクライブに応じてキューを自動作成します。

   - **Auto-Creation Options**: EMQXは自動作成機能で動的にキューをプロビジョニング可能です。

     - **Last Value Semantics Queue**（デフォルト有効）：クライアントが `$queue/<name>/<topic_filter>` にサブスクライブし、キューが存在しない場合にLast-Value Semanticsキューを自動作成します。

       詳細は[Auto Create Last Value Semantics Queue](#auto-create-last-value-semantics-queue)を参照してください。

     - **Regular Queue**：有効にすると、`$queue/<name>/<topic_filter>` のサブスクライブ時に通常のキューを自動作成します。

       詳細は[Auto Create Regular Message Queue](#auto-create-regular-message-queue)を参照してください。

3. 設定変更後、**Save Changes** をクリックして反映します。

### REST API

REST APIを使ってグローバルなメッセージキュー設定を構成することもできます。これらの設定はシステム全体に適用され、すべてのキューの内部管理に影響します。
>>>>>>> origin/release-6.1

```bash
curl -v -u key:secret -X PUT -H "Content-Type: application/json" http://localhost:18083/api/v5/message_queues/config -d '{"find_queue_retry_interval": "10s", "gc_interval": "1h", "regular_queue_retention_period": "7d"}'
```

### 設定ファイル

<<<<<<< HEAD
永続的かつバージョン管理可能な設定として、EMQX設定ファイル（`emqx.conf`）にメッセージキュー設定を記述できます。主要な設定例は以下の通りです：
=======
永続的かつバージョン管理可能な設定のために、EMQX設定ファイル（`emqx.conf`）にメッセージキュー設定を定義できます。以下は主要な設定例です：
>>>>>>> origin/release-6.1

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
<<<<<<< HEAD
- **`regular_queue_retention_period`**：通常キューでメッセージを保持する最大時間を設定します。この期間を過ぎるとメッセージは削除されます。
- **`find_queue_retry_interval`**：クライアントが `$queue/<name>` にサブスクライブし該当キューが見つからない場合に再試行する頻度を決定します。
=======
- **`regular_queue_retention_period`**：通常キューでメッセージを保持する最大期間を設定します。この期間を過ぎるとメッセージは削除されます。
- **`find_queue_retry_interval`**：クライアントが `$queue/<name>` にサブスクライブした際にキューが見つからない場合、再試行する頻度を決定します。
>>>>>>> origin/release-6.1
- **`max_queue_count`**：（任意）作成可能なキューの最大数を設定します。

## REST APIによるキュー管理

<<<<<<< HEAD
EMQXはメッセージキューのライフサイクル管理（作成、取得、更新、削除）を行うREST APIを提供しています。

::: tip 注意

すべてのREST API操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは[REST API](../admin/api.md)の「Message Queue」セクションを参照してください。

:::

以下の例はすべてAPIキーとシークレットによるベーシック認証を前提としています。

### キューの作成

キュー名、トピックフィルター、Last-Value Semanticsの有効/無効などのキュー属性を指定して新しいキューを作成します：
=======
EMQXはメッセージキューのライフサイクル管理のためのREST APIを提供しています。キューの作成、取得、更新、削除が可能です。

::: tip 注意

すべてのREST API操作には適切な認証と権限が必要です。リクエストおよびレスポンスのスキーマ詳細は[REST API](../admin/api.md)の「Message Queue」セクションを参照してください。

:::

以下の例はすべてAPIキーとシークレットによるベーシック認証を想定しています。

### キューの作成

キュー名、トピックフィルター、Last-Value Semanticsの有効化などのキュー属性を指定して新しいメッセージキューを作成します：
>>>>>>> origin/release-6.1

```bash
curl -s -u key:secret -X POST -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues \
<<<<<<< HEAD
-d '{"name": "my_queue", "topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
=======
-d '{"topic_filter": "t1/#", "is_lastvalue": false, "limits": {"max_shard_message_count": 10000, "max_shard_message_bytes": "200MB"}}' | jq
>>>>>>> origin/release-6.1
```

レスポンスには作成されたキューの詳細（`name` や設定内容）が含まれます。

<<<<<<< HEAD
### すべてのキュー一覧取得

既存のメッセージキュー一覧を取得します：
=======
### すべてのキューの一覧取得

既存のメッセージキュー一覧を取得します。
>>>>>>> origin/release-6.1

```bash
curl -s -u key:secret -X GET -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues | jq
```

### キューの更新

<<<<<<< HEAD
既存キューの配信戦略などの属性を更新します：

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/my_queue \
=======
既存キューのプロパティ（例：ディスパッチ戦略）を更新します：

```bash
curl -s -u key:secret -X PUT -H "Content-Type: application/json" \
http://localhost:18083/api/v5/message_queues/t1%2F%23 \
>>>>>>> origin/release-6.1
-d '{"dispatch_strategy": "least_inflight", "limits": {"max_shard_message_count": 5000, "max_shard_message_bytes": "100MB"}}' | jq
```

### キューの削除

<<<<<<< HEAD
キューとその中に保持されているすべてのメッセージを削除します：
=======
メッセージキューとその中に保持されているすべてのメッセージを削除します。
>>>>>>> origin/release-6.1

```bash
curl -s -u key:secret -X DELETE \
http://localhost:18083/api/v5/message_queues/my_queue
```

<<<<<<< HEAD
削除後、そのキューは新たなメッセージの受け入れを停止し、保存データは消去されます。
=======
削除後はキューが新しいメッセージの受け入れを停止し、保存データも消去されます。
>>>>>>> origin/release-6.1

## FAQとトラブルシューティング

### なぜメッセージがキューに格納されないのですか？

<<<<<<< HEAD
- 宣言済みメッセージキューのトピックフィルターが、パブリッシュされたメッセージのトピックと一致しているか確認してください。
- キューが存在し、正しく設定されているか確認してください。
- EMQXのログを確認し、`mq_` プレフィックスのエントリを中心にキュー関連のエラーや警告を調査してください。

### キューが容量を超えた場合はどうなりますか？

EMQXのメッセージキューは複数の容量制限タイプをサポートしています。いずれかの制限に達すると、ガベージコレクション（GC）時に最も古いメッセージから順に削除され、キューサイズが設定範囲内に戻るまで処理されます。

- **時間ベースの制限**：すべてのキューは設定された保持期間の制限を受けます。保持期間を超えたメッセージは配信対象外となり、GCで自動的に削除されます。

- **サイズベースの制限**：オプションでシャードごとに以下の制限を設定可能です：

  - **最大メッセージ数**（`max_shard_message_count`）
  - **最大メッセージ合計サイズ（バイト）**（`max_shard_message_bytes`）

  これらの制限はソフトリミットであり、リアルタイムではなくGC時に適用されます。GCサイクル間は一時的に制限を超える場合があります。

  なお、これらの制限は永続ストレージのシャード単位で適用されます。シャード数の設定方法は[シャード数](../durability/managing-replication.md#number-of-shards)を参照してください。また、サイズ制限は[レプリケーション係数](../durability/managing-replication.md#replication-factor)を考慮していません。実際の物理ストレージ使用量はレプリケーション係数分だけ増加します。
=======
- 宣言済みメッセージキューのトピックフィルターが、パブリッシュされたメッセージのトピックに一致しているか確認してください。
- キューが存在し、正しく設定されているか確認してください。
- EMQXのログを確認し、関連するエラーや警告を探してください。特に `mq_` プレフィックスのログをチェックするとキュー関連の問題を特定しやすいです。

### キューの容量が超過した場合はどうなりますか？

EMQXのメッセージキューは複数の容量制限をサポートしています。これらの制限のいずれかに達した場合、EMQXはガベージコレクション（GC）時に最も古いメッセージから順に削除し、キューサイズを設定範囲内に戻します。

- **時間ベースの制限**：すべてのキューは設定された保持期間の制限を受けます。保持期間を超えたメッセージは配信対象外となり、GCによって自動的に削除されます。

- **サイズベースの制限**：シャードごとに以下の制限を任意で設定可能です：

  - **最大メッセージ数**（`max_shard_message_count`）
  - **最大メッセージサイズ合計（バイト）**（`max_shard_message_bytes`）

  これらの制限はソフト制限であり、リアルタイムではなくGC時に適用されます。GCサイクル間では一時的に制限を超えることがあります。

  なお、これらの制限は永続ストレージのシャード単位で適用されます。シャード数の設定方法は[シャード数](../durability/managing-replication.md#number-of-shards)を参照してください。また、サイズ制限は[レプリケーションファクター](../durability/managing-replication.md#replication-factor)を考慮していません。実際の物理ストレージ使用量はレプリケーションファクターに応じて増加します。
>>>>>>> origin/release-6.1
