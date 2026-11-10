# MQTT Streams ユーザーガイド

::: warning 重要なお知らせ
MQTT Streams は現在ベータ機能です。機能やインターフェースは大幅に変更される可能性があります。
:::

このページでは、EMQX における MQTT Streams 機能の実践的な利用方法について、ストリームの作成から動作設定、ダッシュボードや REST API、設定ファイルによる管理方法までを解説します。

## MQTT Streams 機能の有効化

MQTT Streams 機能はデフォルトで無効になっています。ストリームを作成または使用する前に、ダッシュボードで機能を有効化する必要があります。

1. 左メニューの **Streams** に移動します。
2. 機能が有効化されていない場合、無効である旨のメッセージが表示されます。
3. **Settings** をクリックして **Streams** 設定ページを開きます。
4. **Enable Streams** を **オン** に切り替えます。
5. **Save Changes** をクリックします。

有効化すると、MQTT Streams 機能が即座に利用可能となり、ストリームの作成や管理を開始できます。

## ダッシュボードからのストリーム手動作成

MQTT Streams は、メッセージを保存またはリプレイするために明示的に作成する必要があります。ストリームは手動または自動で作成・管理できます。自動作成の詳細は [ダッシュボードからの自動ストリーム作成](#automatically-create-mqtt-streams-via-dashboard) を参照してください。

1. 左メニューの **Streams** に移動します。

2. **Create Stream** をクリックして **Create Stream** ダイアログを開きます。

3. 以下のオプションを設定します：

   - **Name**：必須。ストリームの一意の名前を指定します。ストリーム名には以下のみ使用可能です：

     - 英数字 (`A–Z`, `a–z`, `0–9`)
     - アンダースコア (`_`)
     - ハイフン (`-`)
     - ドット (`.`)

     この名前でストリームを識別・管理します。

   - **Topic Filter**：ストリームに取り込むパブリッシュされたメッセージを定義するトピックまたはトピックフィルター（例：`t/1` または `sensors/+/data`）を入力します。このフィルターにマッチするトピックにパブリッシュされたすべてのメッセージがストリームに保存されます。

     > クライアントは以下のサブスクリプション形式でメッセージを消費できます：
     >
     > - ストリームが既に存在する場合は `$stream/<name>` を使用します。
     > - `$stream/<name>/<topic_filter>` は既存ストリームのサブスクライブ時にオプションで使用可能です。自動作成が有効な場合に利用されます。ストリームがまだ存在しない場合、EMQX は指定された `<topic_filter>` を使って自動的にストリームを作成します。
     >
     > `<topic_filter>` セグメントはストリームの設定されたトピックフィルターと一致している必要があります。
     >
     > 過去のメッセージをリプレイするには、MQTT 5 のサブスクリプションプロパティ `stream-offset` を指定します。値は以下のいずれかです：
     >
     > - マイクロ秒単位の Unix タイムスタンプ
     > - `earliest`
     > - `latest`

   - **Data Retention Period**：メッセージをストリームに保持する期間を指定します。設定期間を超えた古いメッセージは自動的に削除され、リプレイ可能な過去メッセージの範囲を制限します。

   - **Last-Value Semantics**：このオプションを有効にすると、各キーごとに最新のメッセージのみを保持します。同じキーの新しいメッセージが古いメッセージを上書きします。デバイスの状態や設定など状態指向のデータに適しています。

   - **Stream Key Expression**：必須。各受信メッセージからキーを抽出するための式を定義します。デフォルト値は `message.from` で、メッセージパブリッシャーのクライアント ID を意味します。このフィールドは [Variform 式](../../guides/configuration/configuration.md#variform-expressions) による設定をサポートします。

      ::: tip

      Stream Key Expression は Message Queue の Queue Key Expression に類似しています。キー抽出の例は [Queue Key Expression](../message-queue/message-queue-task.md#queue-key-expression) を参照してください。

      :::

      抽出されたキーはストリームの種類により用途が異なります：
        - **Last-Value** ストリームではキーが主キーとして機能し、同じキーのメッセージが上書きされ、キーごとに最新のメッセージのみが保持されます。詳細と例は [Stream Key Expression](#stream-key-expression) を参照してください。

        - **通常** ストリームではキーはシャーディングキーとして使用され、メッセージが書き込まれるストレージシャードを決定します。

          ::: tip

          通常ストリームでは定数や低カーディナリティの式を避けてください。メッセージが単一シャードに集中し、書き込み性能に影響を与える可能性があります。

          :::

   - **Limiter**：ストリームの各シャードに対するストレージ使用制限を設定します：

      - **Max Shard Message Count**：各シャードに保持可能な最大メッセージ数を設定します。有効化して値を指定するか、無効化して無制限（`infinity`）にできます。
     - **Max Shard Message Bytes**：各シャードに保持可能なメッセージの合計サイズ上限を設定します（例：`200MB`）。有効化してサイズを指定するか、無効化して無制限（`infinity`）にできます。

      これらの制限は永続ストレージに保存され、保持期間設定と連携して動作します。

4. **Create** をクリックしてストリームを保存します。

作成後、MQTT ストリームは即座に有効化されます。設定されたトピックフィルターにマッチするトピックにパブリッシュされたメッセージは保持期間やリミッター設定に従って保存され、ストリームをサブスクライブするクライアントによりリプレイ可能です。

## Stream Key Expression

Stream Key Expression は Last-Value Semantics モードでメッセージの重複排除に使用するキーを抽出する方法を指定します。この式はメッセージのデータに対して評価され、[Variform 式](../../guides/configuration/configuration.md#variform-expressions) の構文に従います。

式は `from`, `topic`, `payload`, `headers.properties` などのフィールドを含むメッセージコンテキストに対して評価されます。例えばユーザープロパティをキーに使う場合、式は以下のようになります：

```
message.headers.properties.User-Property.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しない）、メッセージは破棄されストリームに保存されません。

### メッセージコンテキスト例

<!--@include: ../shared/key-expression-message-context.md-->

### Stream Key Expression の例

#### 例 1

以下の設定でストリームを作成したとします：
- Last-Value Semantics 有効
- トピックフィルター `t/#`
- Stream Key Expression：`message.headers.properties.User-Property.stream-key`

EMQX に以下のメッセージがパブリッシュされ、クライアントは存在しないと仮定します：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|----------|------------------------|
| 1 | `client1` | `t/1` | `keyA` |
| 2 | `client1` | `t/2` | `keyB` |
| 3 | `client2` | `t/3` | `keyA` |
| 4 | `client2` | `t/4` | `keyB` |

クライアントが接続してストリームをサブスクライブすると、以下のメッセージが配信されます：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|----------|------------------------|
| 3 | `client2` | `t/3` | `keyA` |
| 4 | `client2` | `t/4` | `keyB` |

ストリームには各ユニークな `message.headers.properties.User-Property.stream-key` の最新メッセージのみが保持されます。キー式はトピックを跨いでストリーム全体に適用されるため、`t/1` にパブリッシュされた `keyA` のメッセージは後に `t/3` にパブリッシュされた同じ `keyA` のメッセージで上書きされます。

#### 例 2

以下の設定でストリームを作成したとします：
- Last-Value Semantics 有効
- トピックフィルター `t/#`
- Stream Key Expression：`message.from`

例 1 と同じメッセージが EMQX にパブリッシュされた場合、クライアントが接続してサブスクライブすると以下のメッセージが配信されます：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|----------|------------------------|
| 2 | `client1` | `t/2` | `keyB` |
| 4 | `client2` | `t/4` | `keyB` |

同じ `message.from` の値を持つメッセージは上書きされるため、各送信元の最新メッセージのみが保持されます。

#### 例 3

以下の設定でストリームを作成したとします：
- Last-Value Semantics 有効
- トピックフィルター `t/#`
- Stream Key Expression：`concat(message.headers.properties.User-Property.stream-key, '-', message.topic)`

EMQX に以下のメッセージがパブリッシュされた場合：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|----------|------------------------|
| 1 | `client1` | `t/1` | `keyA` |
| 2 | `client1` | `t/2` | `keyB` |
| 3 | `client1` | `t/1` | `keyB` |
| 4 | `client1` | `t/2` | `keyA` |

クライアントが接続してサブスクライブすると、すべてのメッセージが配信されます。なぜなら `message.headers.properties.User-Property.stream-key` と `message.topic` の組み合わせが各メッセージでユニークだからです：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` | 計算されたキー |
|---|--------|----------|------------------------|--------------|
| 1 | `client1` | `t/1` | `keyA` | `keyA-t/1` |
| 2 | `client1` | `t/2` | `keyB` | `keyB-t/2` |
| 3 | `client1` | `t/1` | `keyB` | `keyB-t/1` |
| 4 | `client1` | `t/2` | `keyA` | `keyA-t/2` |

## ダッシュボードからの自動ストリーム作成

クライアントが `$stream/<name>` プレフィックス付きトピックをサブスクライブすると、MQTT ストリームが自動的に作成されます。サブスクリプションの `<name>` がストリーム名になります。

::: tip 注意

自動ストリーム作成は MQTT Streams 機能がグローバルに有効化されている場合のみ利用可能です。

:::

ストリームは通常ストリームまたは Last-Value Semantics ストリームとして自動作成されます。

::: tip 注意

適切なストリーム動作を確保するため、自動作成は通常ストリームか Last-Value ストリームのいずれか一方のみ有効にできます。両方同時にはできません。

:::

### Last-Value ストリームの自動作成

このオプションはデフォルトで **Streams** タブの **MQTT Settings** 内にて有効です。EMQX はキーごとに最新のメッセージのみを保持する Last-Value Semantics 対応のストリームを自動作成します。

1. **Management** -> **MQTT Settings** -> **Messages** タブに移動します。

2. デフォルトで **Enable Auto Create Stream** が有効で、**Last Value Stream** タイプが選択されています。

   以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。Last-Value ストリームではこのキーが主キーとして機能し、同じキーのメッセージは上書きされ最新値のみ保持されます。
   - **Data Retention Period**：ストリームにメッセージを保持する期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$stream/my_stream/test` のようなトピックをサブスクライブすると、EMQX は `my_stream` という名前の Last-Value ストリームを自動作成し、**Streams** リストに表示されます。

### 通常ストリームの自動作成

通常ストリーム（メッセージを上書きせずにすべて保持）を希望する場合、このオプションを手動で有効にできます。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. デフォルトで **Enable Auto Create Message Stream** が有効です。**Regular Message Stream** タイプを選択します。

3. 以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。

     通常ストリームでは、このキーはシャーディングキーとして使われ、同じキーのメッセージは同じシャードにルーティングされます。これによりキーごとの順序性が保たれ、シャード間で負荷分散が可能です。

   - **Data Retention Period**：ストリームにメッセージを保持する期間を指定します。

4. **Save Changes** をクリックします。

## ストリーム設定の構成

このセクションでは、EMQX のすべての MQTT ストリームに適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージ保持、クリーンアップ間隔、内部ストリーム動作、自動ストリーム作成の挙動を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQX ダッシュボードから直接 MQTT Streams 設定を更新できます。ブローカーの再起動は不要で、システム全体のストリーム動作をランタイムに調整可能です。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. 以下のオプションを設定します：

   - **Enable Streams**：MQTT Streams 機能のグローバル有効化・無効化。無効時はストリームの作成・利用ができません。

   - **Max Stream Count**：クラスター内に存在可能なストリームの最大数。無制御なストリーム作成によるリソース過剰使用を防止します。

   - **GC Interval**：期限切れストリームメッセージのクリーンアップ間隔。デフォルトは `1` 時間です。

   - **Regular Stream Retention Period**：通常（Last-Value でない）ストリームのデフォルト保持期間。これを超えたメッセージは自動削除されます。デフォルトは `7` 日です。

   - **Enable Auto Create Message Stream**：クライアントがストリームトピックをサブスクライブし、該当ストリームが存在しない場合に自動作成を有効化します。

   - **Auto Create Stream Type**：自動作成するストリームのタイプを指定します：

     - **Last Value Stream**（デフォルト）：Last-Value セマンティクス対応ストリームを自動作成します。
     - **Regular Stream**：すべてのメッセージを保持する通常ストリームを自動作成します。

   - **Stream Key Expression**：Last-Value セマンティクス有効時に自動作成されるストリームで使用するキー式。デフォルトは `message.from`。キー抽出によりキーごとの順序性や上書き動作が決まります。

   - **Data Retention Period**：自動作成ストリームの保持期間。期間を超えたメッセージは自動削除されます。

   - **Max Shard Message Bytes**：ストリームの各シャードに保存可能なデータ量の上限。有効化して制限を設定するか、無効化して無制限（`infinity`）にできます。

   - **Max Shard Message Count**：ストリームの各シャードに保持可能な最大メッセージ数。設定可能または無制限（`infinity`）にできます。

     ::: tip

     [シャード](../design/durable-storage.md#shard) の数は Durable Storage 設定でグローバルに定義され、すべてのストリームに適用されます。この制限はシャードごとに適用され、データのレプリケーションは考慮しません。ストレージ容量計画時は、シャード数とレプリケーション係数に応じてストリームの総ディスク使用量が増加する点に注意してください。

     :::

3. 変更後、**Save Changes** をクリックして設定を適用します。

更新された設定は即時反映され、既存および新規作成されるストリームに適用されます。

### REST API

EMQX REST API を使ってグローバル MQTT Streams 設定をプログラム的に構成できます。

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

EMQX の設定ファイルを編集してグローバル MQTT Streams 設定を構成できます。この方法は起動時のデフォルト動作定義や、設定ファイルが主な管理手段の環境で有効です。

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

- **gc_interval**：MQTT ストリームの期限切れメッセージを削除する頻度を制御します。ストリームストレージのガベージコレクションサイクルに影響します。
- **regular_stream_retention_period**：通常ストリームの最大保持期間を指定します。期間を超えたメッセージは自動削除されます。
- **check_stream_status_interval**：`$stream/<name>` トピックをサブスクライブした際に該当ストリームが存在しない場合、サブスクライバーがストリームを検出するために再試行する間隔を指定します。

すべての期間値は `s`（秒）、`m`（分）、`h`（時間）、`d`（日）などの標準的な時間単位を使用します。

#### Durable Storage 設定

ストリームメッセージは EMQX Durable Storage に保存されます。MQTT ストリームのストレージ関連設定は `durable_storage.streams_messages` セクションで構成します。

```hocon
durable_storage {
    ## ストリームメッセージを保存するデータベースの設定
    ## 詳細は Durable Storage 設定を参照してください
    streams_messages {
        transaction {
            flush_interval = 100
            idle_flush_interval = 20
            conflict_window = 5000
        }
    }
}
```

これらの設定は MQTT ストリームデータの永続化におけるトランザクションバッチやフラッシュ動作を制御します。通常はデフォルト値で十分であり、ストレージ性能のチューニングが必要な場合のみ調整してください。

## REST API でのストリーム管理

EMQX はストリーム管理用の REST API を提供しています。これらの API を使い、ストリームの作成、更新、一覧取得、照会、削除やグローバル設定の構成が可能です。自動化や外部システムとの連携、大規模なストリーム管理に役立ちます。

::: tip 注意

すべての REST API 操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは [REST API](../../guides/api.md) の「MQTT Stream」セクションを参照してください。

:::

以下の例はすべて API キーとシークレットによるベーシック認証を想定しています。

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

レスポンスには作成されたストリームの詳細（`topic_filter` 含む）が含まれます。

### ストリーム一覧の取得

既存ストリームの一覧を取得するには、ストリームエンドポイントに `GET` リクエストを送信します。

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

削除後、ストリームはメッセージの収集を停止し、内部クリーンアップルールに従って保存データが削除されます。

### ストリームのグローバル設定構成

[Configure Streams Settings -REST API](#rest-api) を参照してください。
