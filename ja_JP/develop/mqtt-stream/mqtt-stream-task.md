# MQTT Streams ユーザーガイド

このページでは、EMQX の MQTT Streams 機能の実践的な使い方を説明します。ストリームの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## MQTT Streams 機能の有効化

MQTT Streams 機能はデフォルトで無効になっています。ストリームを作成または使用する前に、ダッシュボードで機能を有効化する必要があります。

1. 左メニューの **Streams** に移動します。
2. 機能が無効の場合、無効である旨のメッセージが表示されます。
3. **Settings** をクリックして **Streams** 設定ページを開きます。
4. **Enable Streams** を **On** に切り替えます。
5. **Save Changes** をクリックします。

有効化すると、MQTT Streams 機能が即座に利用可能になり、ストリームの作成や管理を開始できます。

## ダッシュボードからの手動ストリーム作成

MQTT Streams は、メッセージを保存・再生するために明示的に作成する必要があります。ストリームは手動または自動で作成・管理できます。自動作成の詳細は [ダッシュボードからの自動ストリーム作成](#automatically-create-mqtt-streams-via-dashboard) を参照してください。

1. 左メニューの **Streams** に移動します。

2. **Create Stream** をクリックして **Create Stream** ダイアログを開きます。

3. 以下のオプションを設定します：

   - **Name**：必須。ストリームの一意の名前を指定します。名前には以下の文字のみ使用可能です：

     - 英数字（`A–Z`、`a–z`、`0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でストリームが識別・管理されます。

   - **Topic Filter**：ストリームに取り込むパブリッシュされたメッセージを定義するトピックまたはトピックフィルター（例：`t/1`、`sensors/+/data`）を入力します。このフィルターにマッチするトピックにパブリッシュされたすべてのメッセージがストリームに保存されます。

     > クライアントは以下のサブスクリプション形式でメッセージを消費できます：
     >
     > - `$stream/<name>` はストリームが既に存在する場合に使用します。
     > - `$stream/<name>/<topic_filter>` は既存ストリームにサブスクライブする際に任意で使用可能です。自動作成が有効な場合に使えます。ストリームがまだ存在しない場合、EMQX は指定された `<topic_filter>` を使って自動的にストリームを作成します。
     >
     > `<topic_filter>` セグメントはストリームの設定されたトピックフィルターと一致する必要があります。
     >
     > 過去メッセージを再生するには、MQTT 5 のサブスクリプションプロパティ `stream-offset` を指定します。値は以下のいずれかです：
     >
     > - マイクロ秒単位の Unix タイムスタンプ
     > - `earliest`
     > - `latest`

   - **Data Retention Period**：メッセージの保持期間を指定します。設定期間より古いメッセージは自動的に削除され、再生可能な過去メッセージの範囲が制限されます。

   - **Last-Value Semantics**：このオプションを有効にすると、各キーに対して最新のメッセージのみを保持します。同じキーの新しいメッセージが古いメッセージを上書きします。デバイスの状態や設定など状態指向のデータに適しています。

   - **Stream Key Expression**：必須。各メッセージからキーを抽出するための式を定義します。デフォルトは `message.from`（メッセージパブリッシャーのクライアントID）です。このフィールドは [Variform expressions](../../guides/configuration/configuration.md#variform-expressions) による設定をサポートします。

      ::: tip

      Stream Key Expression は Message Queue の Queue Key Expression に似ています。キー抽出の例は [Queue Key Expression](../message-queue/message-queue-task.md#queue-key-expression) を参照してください。

      :::

      抽出されたキーはストリームの種類によって異なる役割を持ちます：
        - **Last-Value** ストリームでは、キーはプライマリキーとして機能します。同じキーのメッセージは上書きされ、キーごとに最新のメッセージのみが保持されます。詳細と例は [Stream Key Expression](#stream-key-expression) を参照してください。

        - **通常** のストリームでは、キーはシャーディングキーとして使われ、メッセージが書き込まれるストレージのシャードを決定します。

          ::: tip

          通常ストリームでは、定数や低カーディナリティの式を避けてください。メッセージが単一シャードに集中し、書き込み性能に影響を与える可能性があります。

          :::

   - **Limiter**：ストリームの各シャードのストレージ使用量を制御する制限を設定します：

      - **Max Shard Message Count**：各シャードに保持される最大メッセージ数を設定します。有効にして値を指定するか、無効にして無制限（`infinity`）にできます。

      - **Max Shard Message Bytes**：各シャードに保持されるメッセージの合計サイズの最大値を設定します。有効にしてサイズ（例：`200MB`）を指定するか、無効にして無制限（`infinity`）にできます。

      これらの制限は永続ストレージに保存され、保持期間設定と連携して動作します。

4. **Create** をクリックしてストリームを保存します。

作成後、MQTT ストリームは即座に有効になります。設定したトピックフィルターにマッチするトピックにパブリッシュされたメッセージは、保持期間や制限設定に従って保存され、ストリームにサブスクライブするクライアントによって再生可能です。

## Stream Key Expression

Stream Key Expression は、Last-Value Semantics モードでメッセージの重複排除に使うキーを抽出する方法を指定します。この式はメッセージのデータに対して評価され、[Variform expressions](../../guides/configuration/configuration.md#variform-expressions) の構文に従います。

式は `from`、`topic`、`payload`、`headers.properties` などのフィールドを含むメッセージコンテキストに対して評価されます。例えば、ユーザープロパティをキーにする場合は以下のように設定します：

```
message.headers.properties.User-Property.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しない）、メッセージは破棄され、ストリームに保存されません。

### メッセージコンテキスト例

<!--@include: ../shared/key-expression-message-context.md-->

### Stream Key Expression の例

#### 例 1

以下の設定のストリームを作成したとします：
- Last-Value Semantics 有効
- Topic Filter は `t/#`
- Stream Key Expression は `message.headers.properties.User-Property.stream-key`

以下のメッセージが EMQX にパブリッシュされ、クライアントは存在しません：

| N | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|---------|----------------------------|
| 1 | `client1` | `t/1` | `keyA` |
| 2 | `client1` | `t/2` | `keyB` |
| 3 | `client2` | `t/3` | `keyA` |
| 4 | `client2` | `t/4` | `keyB` |

クライアントが接続してストリームにサブスクライブすると、以下のメッセージが配信されます：

| N | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|---------|----------------------------|
| 3 | `client2` | `t/3` | `keyA` |
| 4 | `client2` | `t/4` | `keyB` |

各ユニークな `message.headers.properties.User-Property.stream-key` の最新メッセージのみがストリームに保持されます。キー式はトピックを跨いでストリーム全体に適用されるため、`t/1` にパブリッシュされた `keyA` のメッセージは後に `t/3` にパブリッシュされた同じキーのメッセージで上書きされます。

#### 例 2

以下の設定のストリームを作成したとします：
- Last-Value Semantics 有効
- Topic Filter は `t/#`
- Stream Key Expression は `message.from`

例 1 と同じメッセージがパブリッシュされた場合、クライアントが接続してサブスクライブすると以下のメッセージが配信されます：

| N | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|---------|----------------------------|
| 2 | `client1` | `t/2` | `keyB` |
| 4 | `client2` | `t/4` | `keyB` |

同じ `message.from` のメッセージは上書きされるため、送信元ごとに最新のメッセージのみが保持されます。

#### 例 3

以下の設定のストリームを作成したとします：
- Last-Value Semantics 有効
- Topic Filter は `t/#`
- Stream Key Expression は `concat(message.headers.properties.User-Property.stream-key, '-', message.topic)`

以下のメッセージがパブリッシュされた場合：

| N | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|---------|----------------------------|
| 1 | `client1` | `t/1` | `keyA` |
| 2 | `client1` | `t/2` | `keyB` |
| 3 | `client1` | `t/1` | `keyB` |
| 4 | `client1` | `t/2` | `keyA` |

クライアントが接続してサブスクライブすると、すべてのメッセージが配信されます。なぜなら `message.headers.properties.User-Property.stream-key` と `message.topic` の組み合わせが各メッセージでユニークだからです：

| N | 送信元 | トピック | ユーザープロパティ `stream-key` | 計算されたキー |
|---|--------|---------|----------------------------|--------------|
| 1 | `client1` | `t/1` | `keyA` | `keyA-t/1` |
| 2 | `client1` | `t/2` | `keyB` | `keyB-t/2` |
| 3 | `client1` | `t/1` | `keyB` | `keyB-t/1` |
| 4 | `client1` | `t/2` | `keyA` | `keyA-t/2` |

## ダッシュボードからの自動ストリーム作成

クライアントが `$stream/<name>` プレフィックス付きのトピックにサブスクライブすると、MQTT Streams は自動的にストリームを作成できます。サブスクリプションの `<name>` がストリーム名になります。

::: tip 注意

自動ストリーム作成は MQTT Streams 機能がグローバルに有効な場合のみ利用可能です。

:::

ストリームは通常ストリームまたは Last-Value Semantics ストリームとして自動作成されます。

::: tip 注意

適切なストリーム動作を保証するため、自動作成は通常ストリームか Last-Value Semantics ストリームのいずれか一方のみを有効にできます。同時に両方は有効にできません。

:::

### Last-Value ストリームの自動作成

このオプションはデフォルトで **Streams** タブの **MQTT Settings** 内で有効になっています。EMQX は Last-Value Semantics をサポートするストリームを自動的に作成し、キーごとに最新のメッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Messages** タブに移動します。

2. デフォルトで **Enable Auto Create Stream** が有効で、**Last Value Stream** タイプが選択されています。

   以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルトは `message.from`）。Last-Value ストリームでは、このキーがプライマリキーとして機能し、同じキーのメッセージは上書きされ、最新の値のみ保持されます。
   - **Data Retention Period**：メッセージの保持期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$stream/my_stream/test` のようなトピックにサブスクライブすると、EMQX は `my_stream` という名前の Last-Value ストリームを自動的に作成し、**Streams** リストに表示されます。

### 通常ストリームの自動作成

メッセージを上書きせずに独立して保存する通常ストリームを好む場合、このオプションを手動で有効にできます。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. デフォルトで **Enable Auto Create Message Stream** が有効です。**Regular Message Stream** タイプを選択します。

3. 以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルトは `message.from`）。

     通常ストリームでは、このキーがシャーディングキーとして使われ、同じキーのメッセージは同じシャードにルーティングされます。これによりキーごとの順序が保たれ、シャード間で負荷分散が行われます。

   - **Data Retention Period**：メッセージの保持期間を指定します。

4. **Save Changes** をクリックします。

## ストリーム設定の構成

このセクションでは、EMQX のすべての MQTT Streams に適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部ストリーム動作、自動作成動作を制御します。ダッシュボード、REST API、設定ファイルで設定可能です。

### ダッシュボード

EMQX ダッシュボードから MQTT Streams 設定を直接更新できます。ブローカーの再起動は不要で、システム全体のストリーム動作をランタイムで調整可能です。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. 以下のオプションを設定します：

   - **Enable Streams**：MQTT Streams 機能のグローバル有効/無効を切り替えます。無効の場合、ストリームの作成や使用はできません。

   - **Max Stream Count**：クラスター内に存在可能なストリームの最大数を設定します。無制御なストリーム作成によるリソース過剰使用を防止します。

   - **GC Interval**：期限切れのストリームメッセージをクリーンアップする間隔を指定します。デフォルトは `1` 時間です。

   - **Regular Stream Retention Period**：通常（Last-Value でない）ストリームのデフォルト保持期間を定義します。これを超えたメッセージは自動削除されます。デフォルトは `7` 日です。

   - **Enable Auto Create Message Stream**：クライアントがストリームトピックにサブスクライブし、該当ストリームが存在しない場合に自動作成を有効にします。

   - **Auto Create Stream Type**：自動作成するストリームのタイプを指定します：

     - **Last Value Stream**（デフォルト）：Last-Value セマンティクスを有効にしたストリームを自動作成します。
     - **Regular Stream**：メッセージを上書きせずすべて保持する通常ストリームを自動作成します。

   - **Stream Key Expression**：Last-Value セマンティクスが有効な自動作成ストリームで使用するキー式を定義します。デフォルトは `message.from` です。この式はキー抽出と上書き動作に影響します。

   - **Data Retention Period**：自動作成ストリームの保持期間を指定します。これを超えたメッセージは自動削除されます。

   - **Max Shard Message Bytes**：ストリームの各シャードに保存可能なデータ量の上限を設定します。有効にして制限を設定するか、無効にして無制限（`infinity`）にできます。

   - **Max Shard Message Count**：ストリームの各シャードに保存可能なメッセージ数の上限を設定します。有効にして制限を設定するか、無効にして無制限（`infinity`）にできます。

     ::: tip

     [シャード](../design/durable-storage.md#shard) の数は Durable Storage 設定でグローバルに定義され、すべてのストリームに適用されます。この制限はシャード単位で適用され、データ複製は考慮しません。ストレージ容量計画時には、シャード数とレプリケーション係数によりストリームの総ディスク使用量がスケールすることに注意してください。

     :::

3. 変更後、**Save Changes** をクリックして設定を適用します。

更新された設定は即座に反映され、既存および新規作成ストリームに適用されます。

### REST API

EMQX REST API を使ってグローバル MQTT Streams 設定をプログラム的に構成できます。

MQTT Streams のグローバル設定を更新するには、以下のエンドポイントに `PUT` リクエストを送信します：

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

EMQX の設定ファイルを編集してグローバル MQTT Streams 設定を構成できます。この方法は起動時のデフォルト動作定義や設定ファイル管理が主な環境で有用です。

**設定例**：

MQTT Streams の設定は EMQX 設定ファイル（`emqx.conf`）の `streams` セクションで定義します。

```hocon
streams {
    gc_interval = 1h
    regular_stream_retention_period = 1d
    check_stream_status_interval = 10s
}
```

#### 設定項目

- **gc_interval**：期限切れメッセージを MQTT Streams から削除する頻度を制御します。ストリームストレージのガベージコレクションサイクルに影響します。
- **regular_stream_retention_period**：通常ストリームの最大保持期間を指定します。これを超えたメッセージは自動削除されます。
- **check_stream_status_interval**：クライアントが `$stream/<name>` トピックにサブスクライブし、該当ストリームが存在しない場合にストリームを検出するリトライ間隔を指定します。

すべての期間値は `s`（秒）、`m`（分）、`h`（時間）、`d`（日）などの標準時間単位を使用します。

#### Durable Storage 設定

ストリームメッセージは EMQX Durable Storage に保存されます。MQTT Streams のストレージ関連設定は `durable_storage.streams_messages` セクションで構成します。

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

これらの設定は MQTT Streams データの Durable Storage への書き込み方法（トランザクションのバッチングやフラッシュ動作など）を制御します。通常はデフォルト値で十分であり、ストレージ性能調整時のみ変更が必要です。

## REST API によるストリーム管理

EMQX はストリーム管理用の REST API を提供しています。これらの API を使ってストリームの作成、更新、一覧取得、照会、削除やグローバル設定の構成が可能です。自動化や外部システム連携、大規模管理に便利です。

::: tip 注意

すべての REST API 操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは [REST API](../../guides/api.md) の「MQTT Stream」セクションを参照してください。

:::

以下の例はすべて API キーとシークレットを使ったベーシック認証を想定しています。

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

既存ストリームを更新するには、ストリーム名で識別されるリソースに `PUT` リクエストを送信します。トピックフィルターは URL エンコードしてください。

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

レスポンスには更新されたストリーム設定が返されます。

### ストリームの削除

ストリームを削除するには、ストリーム名で識別されるリソースに `DELETE` リクエストを送信します。

```bash
curl -s -u key:secret \
  -X DELETE \
  http://localhost:18083/api/v5/message_streams/streams/my_stream
```

削除後、ストリームはメッセージの収集を停止し、保存されていたデータは内部クリーンアップルールに従って削除されます。

### ストリームのグローバル設定構成

[Configure Streams Settings -REST API](#rest-api) を参照してください。
