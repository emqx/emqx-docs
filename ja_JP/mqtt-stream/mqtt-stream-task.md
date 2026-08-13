# MQTT Streams ユーザーガイド

このページでは、EMQXのMQTT Streams機能の実用的な使い方について、ストリームの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## MQTT Streams機能の有効化

MQTT Streams機能はデフォルトで無効になっています。ストリームを作成または使用する前に、ダッシュボードで機能を有効化する必要があります。

1. 左メニューの **Streams** に移動します。
2. 機能が無効の場合、無効である旨のメッセージが表示されます。
3. **Settings** をクリックして **Streams** 設定ページを開きます。
4. **Enable Streams** を **On** に切り替えます。
5. **Save Changes** をクリックします。

有効化すると、MQTT Streams機能が即座に利用可能となり、ストリームの作成と管理を開始できます。

## ダッシュボードからのストリーム手動作成

MQTT Streamsは、メッセージの保存や再生を行うために明示的に作成する必要があります。ストリームは手動または自動で作成・管理できます。自動作成の詳細は[ダッシュボードからの自動作成](#automatically-create-mqtt-streams-via-dashboard)を参照してください。

1. 左メニューの **Streams** に移動します。

2. **Create Stream** をクリックして **Create Stream** ダイアログを開きます。

3. 以下のオプションを設定します：

   - **Name**：必須。ストリームの一意な名前を指定します。名前には以下の文字のみ使用可能です：

     - 英数字（`A–Z`、`a–z`、`0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でストリームが識別・管理されます。

   - **Topic Filter**：パブリッシュされたメッセージをストリームに取り込む対象のトピックまたはトピックフィルターを入力します（例：`t/1`、`sensors/+/data`）。このフィルターにマッチするトピックにパブリッシュされたすべてのメッセージがストリームに保存されます。

     > クライアントは以下のサブスクリプション形式でメッセージを消費できます：
     >
     > - `$stream/<name>` はストリームが既に存在する場合に使用します。
     > - `$stream/<name>/<topic_filter>` は既存ストリームへのサブスクライブ時に任意で使用可能です。自動作成が有効な場合に使用でき、ストリームが存在しない場合は指定した `<topic_filter>` を使って自動的にストリームを作成します。
     >
     > `<topic_filter>` セグメントはストリームの設定済みトピックフィルターと一致している必要があります。
     >
     > 過去メッセージを再生するには、MQTT 5のサブスクリプションプロパティ `stream-offset` を指定します。値は以下のいずれかです：
     >
     > - マイクロ秒単位のUnixタイムスタンプ
     > - `earliest`
     > - `latest`

   - **Data Retention Period**：メッセージの保持期間を指定します。設定期間を超えた古いメッセージは自動的に削除され、再生可能な期間が制限されます。

   - **Last-Value Semantics**：このオプションを有効にすると、各キーごとに最新のメッセージのみを保持します。同じキーの新しいメッセージが古いメッセージを上書きします。デバイス状態や設定などの状態指向データに適しています。

   - **Stream Key Expression**：必須。各メッセージからキーを抽出するための式を定義します。デフォルトは `message.from` で、メッセージパブリッシャーのクライアントIDを意味します。このフィールドは[Variform式](../configuration/configuration.md#variform-expressions)で設定可能です。

      ::: tip

      Stream Key ExpressionはMessage QueueのQueue Key Expressionに似ています。キー抽出の例は[Queue Key Expression](../message-queue/message-queue-task.md#queue-key-expression)を参照してください。

      :::

      抽出されたキーはストリームタイプによって異なる役割を持ちます：
        - **Last-Value**ストリームでは、キーが主キーとして機能します。同じキーのメッセージは上書きされ、キーごとに最新のメッセージのみが保持されます。詳細と例は[Stream Key Expression](#stream-key-expression)を参照してください。

        - **通常**ストリームでは、キーはシャーディングキーとして使われ、どのストレージシャードにメッセージを書き込むかを決定します。

          ::: tip

          通常ストリームでは、定数や低カーディナリティの式は避けてください。メッセージが単一シャードに集中し、書き込み性能に影響を与える可能性があります。

          :::

   - **Limiter**：ストリームの各シャードごとのストレージ使用制限を設定します：

      - **Max Shard Message Count**：各シャードに保持する最大メッセージ数を設定します。有効化して値を指定するか、無効化して無制限（`infinity`）にできます。
     - **Max Shard Message Bytes**：各シャードに保持するメッセージの合計最大サイズを設定します（例：`200MB`）。有効化してサイズを指定するか、無効化して無制限（`infinity`）にできます。

      これらの制限は永続ストレージに保存され、保持期間設定と連動して動作します。

4. **Create** をクリックしてストリームを保存します。

作成後、MQTTストリームは即座に有効になります。設定したトピックフィルターにマッチするメッセージは保持期間と制限に従って保存され、ストリームにサブスクライブしたクライアントから再生可能です。

## Stream Key Expression

Stream Key Expressionは、Last-Value Semanticsモードでメッセージの重複排除に使うキーの抽出方法を指定します。この式はメッセージのデータに対して評価され、[Variform式](../configuration/configuration.md#variform-expressions)の構文に従います。

式は、`from`、`topic`、`payload`、`headers.properties`などのフィールドを含むメッセージコンテキストに対して評価されます。例えば、ユーザープロパティをキーに使う場合は以下のように設定します：

```
message.headers.properties.User-Property.user-prop
```

式に基づいてキーが抽出できない場合（例：フィールドが存在しない）、メッセージは破棄されストリームに保存されません。

### メッセージコンテキスト例

<!--@include: ../shared/key-expression-message-context.md-->

### Stream Key Expressionの例

#### 例1

以下の条件でストリームを設定したとします：
- Last-Value Semantics 有効
- Topic Filter：`t/#`
- Stream Key Expression：`message.headers.properties.User-Property.stream-key`

以下のメッセージがEMQXにパブリッシュされ、クライアントは存在しないものとします：

| N | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|----------|------------------------------|
| 1 | `client1` | `t/1` | `keyA` |
| 2 | `client1` | `t/2` | `keyB` |
| 3 | `client2` | `t/3` | `keyA` |
| 4 | `client2` | `t/4` | `keyB` |

クライアントが接続してストリームにサブスクライブすると、以下のメッセージが配信されます：

| N | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|----------|------------------------------|
| 3 | `client2` | `t/3` | `keyA` |
| 4 | `client2` | `t/4` | `keyB` |

各ユニークな `message.headers.properties.User-Property.stream-key` の最新メッセージのみがストリームに保持されます。キー式はトピックをまたいでストリーム全体に適用されるため、`t/1` にパブリッシュされた `keyA` のメッセージは後に `t/3` にパブリッシュされた同じキーのメッセージで上書きされます。

#### 例2

以下の条件でストリームを設定したとします：
- Last-Value Semantics 有効
- Topic Filter：`t/#`
- Stream Key Expression：`message.from`

例1と同じメッセージがEMQXにパブリッシュされた場合、クライアントが接続してストリームにサブスクライブすると、以下のメッセージが配信されます：

| N | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|----------|------------------------------|
| 2 | `client1` | `t/2` | `keyB` |
| 4 | `client2` | `t/4` | `keyB` |

同じ `message.from` の値を持つメッセージは上書きされるため、送信元ごとに最新のメッセージのみが保持されます。

#### 例3

以下の条件でストリームを設定したとします：
- Last-Value Semantics 有効
- Topic Filter：`t/#`
- Stream Key Expression：`concat(message.headers.properties.User-Property.stream-key, '-', message.topic)`

以下のメッセージがEMQXにパブリッシュされました：

| N | 送信元 | トピック | ユーザープロパティ `stream-key` |
|---|--------|----------|------------------------------|
| 1 | `client1` | `t/1` | `keyA` |
| 2 | `client1` | `t/2` | `keyB` |
| 3 | `client1` | `t/1` | `keyB` |
| 4 | `client1` | `t/2` | `keyA` |

クライアントが接続してストリームにサブスクライブすると、すべてのメッセージが配信されます。なぜなら `message.headers.properties.User-Property.stream-key` と `message.topic` の組み合わせがメッセージごとにユニークだからです：

| N | 送信元 | トピック | ユーザープロパティ `stream-key` | 計算されたキー |
|---|--------|----------|------------------------------|----------------|
| 1 | `client1` | `t/1` | `keyA` | `keyA-t/1` |
| 2 | `client1` | `t/2` | `keyB` | `keyB-t/2` |
| 3 | `client1` | `t/1` | `keyB` | `keyB-t/1` |
| 4 | `client1` | `t/2` | `keyA` | `keyA-t/2` |

## ダッシュボードからのストリーム自動作成

クライアントが `$stream/<name>` プレフィックス付きトピックにサブスクライブすると、MQTT Streamsは自動的にストリームを作成できます。サブスクリプションの `<name>` がストリーム名になります。

::: tip 注意

自動ストリーム作成はMQTT Streams機能がグローバルに有効な場合のみ利用可能です。

:::

ストリームは通常ストリームまたはLast-Value Semanticsストリームとして自動作成されます。

::: tip 注意

適切なストリーム動作を確保するため、自動作成は通常ストリームかLast-Value Semanticsストリームのいずれか一方のみ有効にしてください。同時に両方を有効にすることはできません。

:::

### Last-Valueストリームの自動作成

このオプションはデフォルトで **Streams** タブの **MQTT Settings** 内で有効になっています。これにより、EMQXはLast-Value Semanticsをサポートするストリームを自動作成し、キーごとに最新のメッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Messages** タブに移動します。

2. デフォルトで **Enable Auto Create Stream** が有効で、**Last Value Stream** タイプが選択されています。

   以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。Last-Valueストリームではこのキーが主キーとして機能し、同じキーのメッセージは上書きされ、最新の値のみが保持されます。
   - **Data Retention Period**：メッセージの保持期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$stream/my_stream/test` のようなトピックにサブスクライブすると、EMQXは `my_stream` という名前のLast-Valueストリームを自動作成し、**Streams** リストに表示されます。

### 通常ストリームの自動作成

このオプションは手動で有効にできます。通常ストリームはメッセージを独立して保存し、上書きしません。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. デフォルトで **Enable Auto Create Message Stream** が有効です。**Regular Message Stream** タイプを選択します。

3. 以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。

     通常ストリームでは、このキーはシャーディングキーとして使われ、同じキーのメッセージは同じシャードにルーティングされます。これによりキーごとの順序が保たれ、負荷分散が可能になります。

   - **Data Retention Period**：メッセージの保持期間を指定します。

4. **Save Changes** をクリックします。

## ストリーム設定の構成

このセクションでは、EMQXのすべてのMQTT Streamsに適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部ストリーム動作、自動作成の挙動を制御します。ダッシュボード、REST API、設定ファイルで設定可能です。

### ダッシュボード

EMQXダッシュボードからMQTT Streams設定を直接更新できます。ブローカーの再起動は不要で、システム全体のストリーム動作をランタイムに調整可能です。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. 以下のオプションを設定します：

   - **Enable Streams**：MQTT Streams機能のグローバル有効/無効を切り替えます。無効時はストリームの作成・使用ができません。

   - **Max Stream Count**：クラスター内で存在可能なストリームの最大数を設定します。無制御なストリーム作成によるリソース過剰使用を防止します。

   - **GC Interval**：期限切れストリームメッセージのクリーンアップ間隔を指定します。デフォルトは `1` 時間です。

   - **Regular Stream Retention Period**：通常（Last-Valueでない）ストリームのデフォルト保持期間を定義します。期間を超えたメッセージは自動削除されます。デフォルトは `7` 日です。

   - **Enable Auto Create Message Stream**：クライアントがストリームトピックにサブスクライブし、該当ストリームが存在しない場合に自動作成を有効にします。

   - **Auto Create Stream Type**：自動作成するストリームのタイプを指定します：

     - **Last Value Stream**（デフォルト）：Last-Valueセマンティクスを有効にしたストリームを自動作成します。
     - **Regular Stream**：すべてのメッセージを保持し上書きしないストリームを自動作成します。

   - **Stream Key Expression**：Last-Valueセマンティクスが有効な自動作成ストリームで使うキー式を定義します。デフォルトは `message.from` です。キーの抽出方法を決定し、キーごとの順序や上書き動作に影響します。

   - **Data Retention Period**：自動作成ストリームの保持期間を指定します。期間を超えたメッセージは自動削除されます。

   - **Max Shard Message Bytes**：ストリームの各シャードに保存可能なデータ量の上限を設定します。有効化して制限を設定するか、無効化して無制限（`infinity`）にできます。

   - **Max Shard Message Count**：ストリームの各シャードに保持可能な最大メッセージ数を設定します。有効化して制限を設定するか、無効化して無制限（`infinity`）にできます。

     ::: tip

     [シャード](../design/durable-storage.md#shard)の数はDurable Storage設定でグローバルに定義され、すべてのストリームに適用されます。この制限はシャード単位で適用され、データのレプリケーションは考慮されません。ストレージ容量計画時は、シャード数とレプリケーション係数に応じて総ディスク使用量が増加することに注意してください。

     :::

3. 設定変更後、**Save Changes** をクリックして反映します。

更新された設定は即時に適用され、既存および新規作成ストリームに影響します。

### REST API

EMQXのREST APIを使ってグローバルMQTT Streams設定をプログラムから構成できます。

MQTT Streamsのグローバル設定を更新するには、以下のエンドポイントに `PUT` リクエストを送信します：

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

EMQXの設定ファイルを編集してグローバルMQTT Streams設定を構成できます。この方法は起動時のデフォルト動作を定義したり、設定ファイル管理が主な環境での設定に適しています。

**設定例**：

MQTT Streams設定はEMQX設定ファイル（`emqx.conf`）の `streams` セクションで定義します。

```hocon
streams {
    gc_interval = 1h
    regular_stream_retention_period = 1d
    check_stream_status_interval = 10s
}
```

#### 設定オプション

- **gc_interval**：MQTT Streamsから期限切れメッセージを削除する頻度を制御します。ストリームストレージのガベージコレクションサイクルに影響します。
- **regular_stream_retention_period**：通常ストリームの最大保持期間を指定します。期間を超えたメッセージは自動削除されます。
- **check_stream_status_interval**：`$stream/<name>` トピックにサブスクライブした際、対応するストリームが存在しない場合にサブスクライバーがストリームを再試行する頻度を指定します。

すべての期間値は `s`（秒）、`m`（分）、`h`（時間）、`d`（日）などの標準時間単位を使用します。

#### Durable Storage設定

ストリームメッセージはEMQX Durable Storageに保存されます。MQTT Streamsのストレージ関連設定は `durable_storage.streams_messages` セクションで構成します。

```hocon
durable_storage {
    ## ストリームメッセージを保存するデータベースの設定。
    ## 詳細はDurable Storage設定を参照してください。
    streams_messages {
        transaction {
            flush_interval = 100
            idle_flush_interval = 20
            conflict_window = 5000
        }
    }
}
```

これらの設定はMQTT Streamsデータの永続化に関するトランザクションバッチ処理やフラッシュ動作を制御します。通常はデフォルト値で十分であり、ストレージ性能調整時以外は変更不要です。

## REST APIによるストリーム管理

EMQXはストリーム管理のためのREST APIを提供しています。これらのAPIを使ってストリームの作成、更新、一覧取得、照会、削除やグローバルMQTT Streams設定の構成が可能です。自動化や外部システム連携、大規模管理に便利です。

::: tip 注意

すべてのREST API操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは[REST API](../admin/api.md)の「MQTT Stream」セクションを参照してください。

:::

以下の例はすべてAPIキーとシークレットによるベーシック認証を想定しています。

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

レスポンスには作成されたストリームの詳細（`topic_filter`など）が含まれます。

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

既存ストリームを更新するには、ストリーム名で識別されるリソースに対して `PUT` リクエストを送信します。トピックフィルターはURLエンコードしてください。

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

### ストリームのグローバル設定構成

[Configure Streams Settings -RESP API](#rest-api) を参照してください。
