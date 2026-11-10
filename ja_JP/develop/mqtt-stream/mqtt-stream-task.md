# MQTT Streams ユーザーガイド

::: warning 重要なお知らせ
MQTT Streams は現在ベータ機能です。その機能やインターフェースは大幅に変更される可能性があります。
:::

このページでは、EMQX における MQTT Streams 機能の実践的な使い方を説明します。ストリームの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## MQTT Streams 機能の有効化

MQTT Streams 機能はデフォルトで無効になっています。ストリームを作成または使用する前に、ダッシュボードで機能を有効にする必要があります。

1. 左メニューの **Streams** に移動します。
2. 機能が有効でない場合は、無効である旨のメッセージが表示されます。
3. **Settings** をクリックして **Streams** 設定ページを開きます。
4. **Enable Streams** を **On** に切り替えます。
5. **Save Changes** をクリックします。

有効化すると、MQTT Streams 機能が即座に利用可能になり、ストリームの作成と管理を開始できます。

## ダッシュボードからの手動ストリーム作成

MQTT Streams は、メッセージを保存またはリプレイする前に明示的に作成する必要があります。ストリームは手動または自動で作成・管理できます。自動作成の詳細は [ダッシュボードからの自動ストリーム作成](#automatically-create-mqtt-streams-via-dashboard) を参照してください。

1. 左メニューの **Streams** に移動します。

2. **Create Stream** をクリックして **Create Stream** ダイアログを開きます。

3. 以下のオプションを設定します：

   - **Name**：必須。ストリームの一意の名前を指定します。名前には以下の文字のみ使用可能です：

     - 英数字（`A–Z`、`a–z`、`0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でストリームは識別・管理されます。

   - **Topic Filter**：ストリームにキャプチャするパブリッシュされたメッセージの対象となるトピックまたはトピックフィルターを入力します（例：`t/1` または `sensors/+/data`）。このフィルターにマッチするトピックにパブリッシュされたすべてのメッセージがストリームに保存されます。

     > クライアントは以下のサブスクリプション形式でメッセージを消費できます：
     >
     > - ストリームが既に存在する場合は `$stream/<name>` を使用します。
     > - ストリームが存在する場合にオプションで `$stream/<name>/<topic_filter>` を使用できます。自動作成が有効な場合に使用され、ストリームが存在しなければ指定された `<topic_filter>` を使って自動作成されます。
     >
     > `<topic_filter>` セグメントはストリームの設定されたトピックフィルターと一致する必要があります。
     >
     > 過去のメッセージをリプレイするには、MQTT 5 のサブスクリプションプロパティ `stream-offset` を指定します。値は以下のいずれかです：
     >
     > - マイクロ秒単位の Unix タイムスタンプ
     > - `earliest`
     > - `latest`

   - **Data Retention Period**：メッセージをストリームに保持する期間を指定します。設定された保持期間より古いメッセージは自動的に削除され、リプレイ可能な過去の範囲が制限されます。

   - **Last-Value Semantics**：このオプションを有効にすると、各キーごとに最新のメッセージのみを保持します。同じキーの新しいメッセージが古いメッセージを上書きします。デバイスの状態や設定など状態指向のデータに適しています。

   - **Stream Key Expression**：必須。各受信メッセージからキーを抽出するための式を定義します。デフォルト値は `message.from` で、メッセージパブリッシャーのクライアント ID を意味します。このフィールドは [Variform 式](../../guides/configuration/configuration.md#variform-expressions) による設定をサポートします。

      ::: tip

      Stream Key Expression は Message Queue の Queue Key Expression と似ています。キー抽出の例は [Queue Key Expression](../message-queue/message-queue-task.md#queue-key-expression) を参照してください。

      :::

      抽出されたキーはストリームの種類によって異なる役割を持ちます：
        - **Last-Value** ストリームではキーは主キーとして機能します。同じキーのメッセージは上書きされ、キーごとに最新のメッセージのみが保持されます。詳細と例は [Stream Key Expression](#stream-key-expression) を参照してください。

        - **通常** ストリームではキーはシャーディングキーとして使われ、メッセージがどのストレージシャードに書き込まれるかを決定します。

          ::: tip

          通常ストリームでは、定数や低カーディナリティの式を使うとメッセージが単一シャードに集中し、書き込み性能に影響を与える可能性があるため避けてください。

          :::

   - **Limiter**：ストリームの各シャードのストレージ使用量を制御する制限を設定します：

      - **Max Shard Message Count**：各シャードに保持する最大メッセージ数を設定します。有効にして値を指定するか、無効にして無制限（`infinity`）にできます。
     - **Max Shard Message Bytes**：各シャードに保持するメッセージの合計サイズの最大値を設定します。有効にしてサイズを指定（例：`200MB`）するか、無効にして無制限（`infinity`）にできます。

      これらの制限は永続ストレージに保存され、保持期間設定と連動して動作します。

4. **Create** をクリックしてストリームを保存します。

作成後、MQTT ストリームは即座に有効になります。設定されたトピックフィルターにマッチするトピックにパブリッシュされたメッセージは、保持期間とリミッター設定に従って保存され、ストリームをサブスクライブするクライアントによってリプレイ可能です。

## Stream Key Expression

Stream Key Expression は Last-Value Semantics モードでメッセージの重複排除に使うキーを抽出する方法を指定します。この式はメッセージのデータに対して評価され、[Variform 式](../../guides/configuration/configuration.md#variform-expressions) の構文に従います。

式は `from`、`topic`、`payload`、`headers.properties` などのフィールドを含むメッセージコンテキストに対して評価されます。例えばユーザープロパティをキーに使う場合は次のように設定します：

```
message.headers.properties.User-Property.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しないなど）、メッセージは破棄されストリームに保存されません。

### メッセージコンテキスト例

<!--@include: ../shared/key-expression-message-context.md-->

### Stream Key Expression の例

#### 例 1

以下の条件でストリームを設定したとします：
- Last-Value Semantics 有効
- Topic Filter は `t/#`
- Stream Key Expression は `message.headers.properties.User-Property.stream-key`

EMQX に以下のメッセージがパブリッシュされ、クライアントは存在しないものとします：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|---------|------------------------|
| 1 | `client1` | `t/1` | `keyA` |
| 2 | `client1` | `t/2` | `keyB` |
| 3 | `client2` | `t/3` | `keyA` |
| 4 | `client2` | `t/4` | `keyB` |

クライアントが接続してストリームをサブスクライブすると、以下のメッセージが配信されます：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|---------|------------------------|
| 3 | `client2` | `t/3` | `keyA` |
| 4 | `client2` | `t/4` | `keyB` |

各ユニークな `message.headers.properties.User-Property.stream-key` の最新メッセージのみがストリームに保持されます。キー式はトピックを跨いでストリーム全体に適用されるため、`t/1` にパブリッシュされた `keyA` のメッセージは後から `t/3` にパブリッシュされた同じキーのメッセージで上書きされます。

#### 例 2

以下の条件でストリームを設定したとします：
- Last-Value Semantics 有効
- Topic Filter は `t/#`
- Stream Key Expression は `message.from`

例 1 と同じメッセージが EMQX にパブリッシュされた場合、クライアントがストリームをサブスクライブすると以下のメッセージが配信されます：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|---------|------------------------|
| 2 | `client1` | `t/2` | `keyB` |
| 4 | `client2` | `t/4` | `keyB` |

同じ `message.from` の値を持つメッセージは上書きされるため、送信元ごとに最新のメッセージのみが保持されます。

#### 例 3

以下の条件でストリームを設定したとします：
- Last-Value Semantics 有効
- Topic Filter は `t/#`
- Stream Key Expression は `concat(message.headers.properties.User-Property.stream-key, '-', message.topic)`

EMQX に以下のメッセージがパブリッシュされます：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|---------|------------------------|
| 1 | `client1` | `t/1` | `keyA` |
| 2 | `client1` | `t/2` | `keyB` |
| 3 | `client1` | `t/1` | `keyB` |
| 4 | `client1` | `t/2` | `keyA` |

クライアントがストリームをサブスクライブすると、すべてのメッセージが配信されます。なぜなら `message.headers.properties.User-Property.stream-key` と `message.topic` の組み合わせが各メッセージでユニークだからです：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` | 計算されたキー |
|---|--------|---------|------------------------|--------------|
| 1 | `client1` | `t/1` | `keyA` | `keyA-t/1` |
| 2 | `client1` | `t/2` | `keyB` | `keyB-t/2` |
| 3 | `client1` | `t/1` | `keyB` | `keyB-t/1` |
| 4 | `client1` | `t/2` | `keyA` | `keyA-t/2` |

## ダッシュボードからの自動ストリーム作成

MQTT Streams は、クライアントが `$stream/<name>` プレフィックス付きのトピックをサブスクライブした際に自動的にストリームを作成できます。サブスクリプションの `<name>` がストリーム名になります。

::: tip 補足

自動ストリーム作成は MQTT Streams 機能がグローバルに有効な場合のみ利用可能です。

:::

ストリームは通常ストリームまたは Last-Value Semantics ストリームとして自動作成されます。

::: tip 補足

適切なストリーム動作を保証するため、自動作成は通常ストリームか Last-Value ストリームのいずれか一方のみを有効にできます。両方同時にはできません。

:::

### Last-Value ストリームの自動作成

このオプションはデフォルトで **Streams** タブの **MQTT Settings** 内で有効になっています。これにより、EMQX は Last-Value Semantics をサポートするストリームを自動作成します。ここではキーごとに最新のメッセージのみが保持されます。

1. **Management** -> **MQTT Settings** -> **Messages** タブに移動します。

2. デフォルトで **Enable Auto Create Stream** が有効で、**Last Value Stream** タイプが選択されています。

   以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルトは `message.from`）。Last-Value ストリームではこのキーが主キーとして機能し、同じキーのメッセージは上書きされ最新値のみが保持されます。
   - **Data Retention Period**：メッセージをストリームに保持する期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$stream/my_stream/test` のようなトピックをサブスクライブすると、EMQX は `my_stream` という Last-Value ストリームを自動作成し、**Streams** リストに表示されます。

### 通常ストリームの自動作成

メッセージを個別に保存し上書きしない通常ストリームを自動作成したい場合は、このオプションを手動で有効にできます。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. デフォルトで **Enable Auto Create Message Stream** が有効です。**Regular Message Stream** タイプを選択します。

3. 以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルトは `message.from`）。

     通常ストリームでは、このキーはシャーディングキーとして使われ、同じキーのメッセージは同一シャードにルーティングされます。これによりキーごとの順序が保持され、シャード間で負荷分散が可能になります。

   - **Data Retention Period**：メッセージをストリームに保持する期間を指定します。

4. **Save Changes** をクリックします。

## ストリーム設定の構成

このセクションでは、EMQX のすべての MQTT ストリームに適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージ保持、クリーンアップ間隔、内部ストリーム動作、自動作成の挙動を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQX ダッシュボードから MQTT Streams 設定を直接更新可能で、ブローカーの再起動は不要です。システム全体のストリーム動作をランタイムで調整するのに便利です。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. 以下のオプションを設定します：

   - **Enable Streams**：MQTT Streams 機能のグローバル有効化・無効化。無効にするとストリームの作成・使用はできません。

   - **Max Stream Count**：クラスター内に存在可能なストリームの最大数。過剰なストリーム作成によるリソース過剰使用を防ぎます。

   - **GC Interval**：期限切れストリームメッセージのクリーンアップ間隔。デフォルトは `1` 時間です。

   - **Regular Stream Retention Period**：通常（Last-Value でない）ストリームのデフォルト保持期間。これより古いメッセージは自動削除されます。デフォルトは `7` 日です。

   - **Enable Auto Create Message Stream**：クライアントがストリームトピックをサブスクライブし該当ストリームが存在しない場合に自動作成を有効化します。

   - **Auto Create Stream Type**：自動作成するストリームのタイプを指定します：

     - **Last Value Stream**（デフォルト）：Last-Value セマンティクスを有効にしたストリームを自動作成します。
     - **Regular Stream**：すべてのメッセージを保持し上書きしない通常ストリームを自動作成します。

   - **Stream Key Expression**：Last-Value セマンティクスが有効な自動作成ストリームで使われるキー抽出式。デフォルトは `message.from`。キーの抽出方法を決定し、キーごとの順序や上書き挙動に影響します。

   - **Data Retention Period**：自動作成ストリームの保持期間。これより古いメッセージは自動削除されます。

   - **Max Shard Message Bytes**：ストリームの各シャードに保存可能なデータ量の上限。制限を有効にするか無効にして無制限（`infinity`）にできます。

   - **Max Shard Message Count**：ストリームの各シャードに保持可能な最大メッセージ数。制限を有効にするか無効にして無制限（`infinity`）にできます。

     ::: tip

     [シャード](../design/durable-storage.md#shard) の数は Durable Storage 設定でグローバルに定義され、すべてのストリームに適用されます。この制限はシャード単位で適用され、データのレプリケーションは考慮されません。ストレージ容量計画時は、ストリームの総ディスク使用量がシャード数とレプリケーション係数に比例して増加することに注意してください。

     :::

3. 設定変更後、**Save Changes** をクリックして適用します。

変更は即座に反映され、既存および新規作成ストリームに適用されます。

### REST API

EMQX REST API を使ってグローバル MQTT Streams 設定をプログラムから構成できます。

MQTT Streams グローバル設定を更新するには、以下のエンドポイントに `PUT` リクエストを送信します：

```
PUT /api/v5/message_streams/config
```

**リクエスト例**：

```bash
curl -s -u key:secret \
  -X PUT \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/message_streams/config \
  -d '{
    "gc_interval": "1h",
    "regular_stream_retention_period": "1d",
    "check_stream_status_interval": "10s"
  }'
```

### 設定ファイル

EMQX の設定ファイルを編集してグローバル MQTT Streams 設定を構成できます。起動時のデフォルト動作定義や設定ファイル管理が主な環境で有効です。

**設定例**：

MQTT Streams 設定は EMQX 設定ファイル（`emqx.conf`）の `streams` セクションに定義します。

```hocon
streams {
    gc_interval = 1h
    regular_stream_retention_period = 1d
    check_stream_status_interval = 10s
}
```

#### 設定オプション

- **gc_interval**：期限切れメッセージを MQTT Streams から削除する頻度を制御します。ストリームストレージのガベージコレクションサイクルに影響します。
- **regular_stream_retention_period**：通常ストリームの最大保持期間を指定します。これより古いメッセージは自動削除されます。
- **check_stream_status_interval**：サブスクライバーが `$stream/<name>` トピックをサブスクライブし対応するストリームが存在しない場合に、ストリームの存在を再試行する間隔を指定します。

すべての期間値は `s`（秒）、`m`（分）、`h`（時間）、`d`（日）などの標準時間単位を使用します。

#### Durable Storage 設定

ストリームメッセージは EMQX Durable Storage に保存されます。MQTT Streams のストレージ関連設定は `durable_storage.streams_messages` セクションで構成します。

```hocon
durable_storage {
    ## ストリームメッセージを保存するデータベースの設定。
    ## 詳細は Durable Storage 設定を参照してください。
    streams_messages {
        transaction {
            flush_interval = 100
            idle_flush_interval = 20
            conflict_window = 5000
        }
    }
}
```

これらの設定は MQTT Streams データの永続化に関するトランザクションバッチ処理やフラッシュ動作を制御します。通常はデフォルト値で十分であり、ストレージ性能調整時以外は変更不要です。

## REST API を使ったストリーム管理

EMQX はストリーム管理用の REST API を提供しています。これらの API を使ってストリームの作成、更新、一覧取得、照会、削除やグローバル MQTT Streams 設定の構成が可能です。自動化や外部システム連携、大規模管理に便利です。

::: tip 補足

すべての REST API 操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは [REST API](../../guides/api.md) の「MQTT Stream」セクションを参照してください。

:::

以下の例は API キーとシークレットによるベーシック認証を想定しています。

### ストリームの作成

新しいストリームを作成するには、ストリームエンドポイントに `POST` リクエストを送り、リクエストボディにストリーム設定を指定します。

```bash
curl -s -u key:secret \
  -X POST \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/message_streams/streams \
  -d '{
    "name": "my_stream",
    "topic_filter": "t1/#",
    "is_lastvalue": false
  }' | jq
```

レスポンスには作成されたストリームの詳細（`topic_filter` など）が含まれます。

### ストリーム一覧の取得

既存のストリーム一覧を取得するには、ストリームエンドポイントに `GET` リクエストを送信します。

```bash
curl -s -u key:secret \
  -X GET \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/message_streams/streams | jq
```

レスポンスにはストリームのリストとページネーション情報が含まれます。

```bash
{
  "data": [
    {
      "name": "my_stream",
      "topic_filter": "t1/#"
    }
  ],
  "meta": {
    "hasnext": false
  }
}
```

### ストリームの更新

既存ストリームを更新するには、ストリーム名で識別されるリソースに対して `PUT` リクエストを送信します。トピックフィルターは URL エンコードしてください。

```bash
curl -s -u key:secret \
  -X PUT \
  -H "Content-Type: application/json" \
  http://localhost:18083/api/v5/message_streams/streams/my_stream \
  -d '{
    "key_expression": "message.from",
    "is_lastvalue": false
  }' | jq
```

レスポンスには更新後のストリーム設定が返されます。

### ストリームの削除

ストリームを削除するには、ストリーム名で識別されるリソースに対して `DELETE` リクエストを送信します。

```bash
curl -s -u key:secret \
  -X DELETE \
  http://localhost:18083/api/v5/message_streams/streams/my_stream
```

削除後、ストリームはメッセージの収集を停止し、保存されていたデータは内部クリーンアップルールに従って削除されます。

### ストリームグローバル設定の構成

[Configure Streams Settings -REST API](#rest-api) を参照してください。
