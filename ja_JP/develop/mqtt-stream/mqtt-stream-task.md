# MQTT Streams ユーザーガイド

::: warning 重要なお知らせ
MQTT Streams は現在ベータ機能です。その機能やインターフェースは大幅に変更される可能性があります。
:::

本ページでは、EMQX の MQTT Streams 機能の実践的な使い方を解説します。ストリームの作成から動作設定、ダッシュボード、REST API、設定ファイルによる管理方法までを網羅しています。

## MQTT Streams 機能の有効化

MQTT Streams 機能はデフォルトで無効になっています。ストリームを作成または使用する前に、ダッシュボードで機能を有効にする必要があります。

1. 左メニューの **Streams** に移動します。
2. 機能が有効でない場合、無効である旨のメッセージが表示されます。
3. **Settings** をクリックして **Streams** 設定ページを開きます。
4. **Enable Streams** を **On** に切り替えます。
5. **Save Changes** をクリックします。

有効化すると、MQTT Streams 機能が即座に利用可能となり、ストリームの作成や管理を開始できます。

## ダッシュボードからのストリーム手動作成

MQTT Streams は、メッセージを保存または再生するために明示的に作成する必要があります。ストリームは手動または自動で作成・管理できます。自動作成の詳細は [ダッシュボードからの自動ストリーム作成](#automatically-create-mqtt-streams-via-dashboard) を参照してください。

1. 左メニューの **Streams** に移動します。

2. **Create Stream** をクリックして **Create Stream** ダイアログを開きます。

3. 以下のオプションを設定します：

   - **Name**: 必須。ストリームの一意な名前を指定します。名前には以下の文字のみ使用可能です：

     - 英数字（`A–Z`, `a–z`, `0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でストリームが識別・管理されます。

   - **Topic Filter**: ストリームに取り込むパブリッシュされたメッセージの対象となるトピックまたはトピックフィルターを入力します（例：`t/1` や `sensors/+/data`）。このフィルターにマッチするトピックにパブリッシュされたすべてのメッセージがストリームに保存されます。

     > クライアントは以下のサブスクリプション形式でメッセージを消費できます：
     >
     > - `$stream/<name>` は既存のストリームに対して使用します。
     > - `$stream/<name>/<topic_filter>` は既存ストリームへのサブスクライブ時にオプションで使用可能です。自動作成が有効な場合に利用でき、ストリームが存在しなければ `<topic_filter>` を使って自動作成されます。
     >
     > `<topic_filter>` セグメントはストリームの設定されたトピックフィルターと一致している必要があります。
     >
     > 過去メッセージを再生するには、MQTT 5 のサブスクリプションプロパティ `stream-offset` を指定します。値は以下のいずれかです：
     >
     > - マイクロ秒単位の Unix タイムスタンプ
     > - `earliest`
     > - `latest`

   - **Data Retention Period**: メッセージをストリームに保持する期間を指定します。設定期間を超えた古いメッセージは自動的に削除され、再生可能な過去メッセージの範囲が制限されます。

   - **Last-Value Semantics**: 有効にすると、各キーに対して最新のメッセージのみを保持します。同じキーの新しいメッセージが古いメッセージを上書きします。デバイスの状態や設定など状態指向のデータに適しています。

   - **Stream Key Expression**: 必須。各メッセージからキーを抽出するための式を定義します。デフォルトは `message.from` で、メッセージパブリッシャーのクライアントIDを指します。このフィールドは [Variform expressions](../../guides/configuration/configuration.md#variform-expressions) による設定をサポートします。

      ::: tip

      Stream Key Expression は Message Queue の Queue Key Expression と似ています。キー抽出の例は [Queue Key Expression](../message-queue/message-queue-task.md#queue-key-expression) を参照してください。

      :::

      抽出されたキーはストリームタイプにより用途が異なります：
        - **Last-Value** ストリームでは、キーは主キーとして機能します。同じキーのメッセージは上書きされ、キーごとに最新のメッセージのみが保持されます。詳細と例は [Stream Key Expression](#stream-key-expression) を参照してください。

        - **通常** ストリームでは、キーはシャーディングキーとして使われ、どのストレージシャードにメッセージを書き込むかを決定します。

          ::: tip

          通常ストリームでは、定数や低カーディナリティの式は避けてください。メッセージが単一シャードに集中し、書き込み性能に影響を与える可能性があります。

          :::

   - **Limiter**: ストリームの各シャードごとのストレージ使用制限を設定します：

      - **Max Shard Message Count**: 各シャードに保持する最大メッセージ数を設定します。有効化して値を指定するか、無効化して無制限（`infinity`）にできます。
     - **Max Shard Message Bytes**: 各シャードに保持するメッセージの最大合計サイズを設定します（例：`200MB`）。有効化してサイズを指定するか、無効化して無制限（`infinity`）にできます。

      これらの制限は永続ストレージに保存され、保持期間設定と連動して動作します。

4. **Create** をクリックしてストリームを保存します。

作成後、MQTT ストリームは即座に有効化されます。設定したトピックフィルターにマッチするパブリッシュメッセージは保持期間と制限に従って保存され、ストリームをサブスクライブするクライアントにより再生可能です。

## Stream Key Expression

Stream Key Expression は、Last-Value Semantics モードでメッセージの重複排除に使うキーの抽出方法を指定します。この式はメッセージデータに対して評価され、[Variform expressions](../../guides/configuration/configuration.md#variform-expressions) の構文に従います。

式は `from`、`topic`、`payload`、`headers.properties` などを含むメッセージコンテキストに対して評価されます。例えばユーザープロパティをキーに使う場合は以下のように設定します：

```
message.headers.properties.User-Property.user-prop
```

式によりキーが抽出できない場合（例：フィールドが存在しないなど）は、メッセージは破棄されストリームに保存されません。

### メッセージコンテキスト例

<!--@include: ../shared/key-expression-message-context.md-->

### Stream Key Expression の例

#### 例 1

以下の条件でストリームを設定したとします：
- Last-Value Semantics 有効
- トピックフィルター：`t/#`
- Stream Key Expression：`message.headers.properties.User-Property.stream-key`

EMQX に以下のメッセージがパブリッシュされ、クライアントは存在しないものとします：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|----|--------|----------|------------------------------|
| 1  | `client1` | `t/1`  | `keyA`                       |
| 2  | `client1` | `t/2`  | `keyB`                       |
| 3  | `client2` | `t/3`  | `keyA`                       |
| 4  | `client2` | `t/4`  | `keyB`                       |

クライアントが接続しストリームをサブスクライブすると、以下のメッセージが配信されます：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|----|--------|----------|------------------------------|
| 3  | `client2` | `t/3`  | `keyA`                       |
| 4  | `client2` | `t/4`  | `keyB`                       |

`message.headers.properties.User-Property.stream-key` の値ごとに最新のメッセージのみがストリームに保持されます。キー式はトピックを跨いで効果を持ち、`keyA` のメッセージは `t/1` から `t/3` の後発メッセージで上書きされています。

#### 例 2

以下の条件でストリームを設定したとします：
- Last-Value Semantics 有効
- トピックフィルター：`t/#`
- Stream Key Expression：`message.from`

例 1 と同じメッセージが EMQX にパブリッシュされた場合、クライアントが接続しサブスクライブすると以下のメッセージが配信されます：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|----|--------|----------|------------------------------|
| 2  | `client1` | `t/2`  | `keyB`                       |
| 4  | `client2` | `t/4`  | `keyB`                       |

`message.from` の値が同じメッセージは上書きされ、送信元ごとに最新のメッセージのみが保持されます。

#### 例 3

以下の条件でストリームを設定したとします：
- Last-Value Semantics 有効
- トピックフィルター：`t/#`
- Stream Key Expression：`concat(message.headers.properties.User-Property.stream-key, '-', message.topic)`

EMQX に以下のメッセージがパブリッシュされます：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` |
|----|--------|----------|------------------------------|
| 1  | `client1` | `t/1`  | `keyA`                       |
| 2  | `client1` | `t/2`  | `keyB`                       |
| 3  | `client1` | `t/1`  | `keyB`                       |
| 4  | `client1` | `t/2`  | `keyA`                       |

クライアントが接続しサブスクライブすると、各メッセージのキーがユニークなためすべて配信されます：
| No | 送信元 | トピック | ユーザープロパティ `stream-key` | 計算されたキー             |
|----|--------|----------|------------------------------|--------------------------|
| 1  | `client1` | `t/1`  | `keyA`                       | `keyA-t/1`               |
| 2  | `client1` | `t/2`  | `keyB`                       | `keyB-t/2`               |
| 3  | `client1` | `t/1`  | `keyB`                       | `keyB-t/1`               |
| 4  | `client1` | `t/2`  | `keyA`                       | `keyA-t/2`               |

## ダッシュボードからの自動ストリーム作成

クライアントが `$stream/<name>` プレフィックスのトピックをサブスクライブすると、MQTT Streams は自動的にストリームを作成できます。サブスクリプションの `<name>` がストリーム名になります。

::: tip 注意

自動ストリーム作成は MQTT Streams 機能がグローバルに有効な場合のみ利用可能です。

:::

ストリームは通常ストリームまたは Last-Value セマンティクスストリームとして自動作成されます。

::: tip 注意

適切なストリーム動作のため、自動作成は通常ストリームか Last-Value ストリームのいずれか一方のみ有効にしてください。両方同時は推奨されません。

:::

### Last-Value ストリームの自動作成

このオプションはデフォルトで **Streams** タブの **MQTT Settings** にて有効です。Last-Value セマンティクスをサポートするストリームを自動作成します。ここではキーごとに最新のメッセージのみが保持されます。

1. **Management** -> **MQTT Settings** -> **Messages** タブに移動します。

2. デフォルトで **Enable Auto Create Stream** が有効で、**Last Value Stream** タイプが選択されています。

   以下を設定します：

   - **Stream Key Expression**: 必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。Last-Value ストリームではこのキーが主キーとなり、同じキーのメッセージは上書きされ最新のみ保持されます。
   - **Data Retention Period**: メッセージの保持期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$stream/my_stream/test` のようなトピックをサブスクライブすると、EMQX は自動的に `my_stream` という Last-Value ストリームを作成し、**Streams** リストに表示されます。

### 通常ストリームの自動作成

メッセージを上書きせずに独立して保存する通常ストリームを自動作成したい場合は、手動でこのオプションを有効にできます。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. デフォルトで **Enable Auto Create Message Stream** が有効です。**Regular Message Stream** タイプを選択します。

3. 以下を設定します：

   - **Stream Key Expression**: 必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。

     通常ストリームでは、このキーがシャーディングキーとして使われ、同じキーのメッセージは同じシャードにルーティングされます。これによりキーごとの順序性が保たれ、負荷分散が可能になります。

   - **Data Retention Period**: メッセージの保持期間を指定します。

4. **Save Changes** をクリックします。

## ストリーム設定の構成

このセクションでは、EMQX のすべての MQTT Streams に適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部ストリーム動作、自動作成動作を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQX ダッシュボードから MQTT Streams の設定を直接更新できます。ブローカーの再起動は不要で、システム全体のストリーム動作をランタイムで調整するのに便利です。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. 以下のオプションを設定します：

   - **Enable Streams**: MQTT Streams 機能のグローバル有効/無効を切り替えます。無効時はストリームの作成・使用ができません。

   - **Max Stream Count**: クラスター内に存在可能なストリームの最大数を設定します。過剰なストリーム作成によるリソース過剰使用を防止します。

   - **GC Interval**: 期限切れストリームメッセージのクリーンアップ間隔を指定します。デフォルトは `1` 時間です。

   - **Regular Stream Retention Period**: 通常（Last-Value でない）ストリームのデフォルト保持期間を定義します。これを超えたメッセージは自動削除されます。デフォルトは `7` 日です。

   - **Enable Auto Create Message Stream**: クライアントがストリームトピックをサブスクライブし、該当ストリームが存在しない場合に自動作成を有効にします。

   - **Auto Create Stream Type**: 自動作成するストリームのタイプを指定します：

     - **Last Value Stream**（デフォルト）：Last-Value セマンティクスを有効にしたストリームを自動作成します。
     - **Regular Stream**：すべてのメッセージを保持し上書きしないストリームを自動作成します。

   - **Stream Key Expression**: Last-Value セマンティクスが有効な自動作成ストリームで使用されるキー式を定義します。デフォルトは `message.from` です。キーの抽出方法を決定し、キーごとの順序性や上書き動作に影響します。

   - **Data Retention Period**: 自動作成ストリームの保持期間を指定します。これを超えたメッセージは自動削除されます。

   - **Max Shard Message Bytes**: ストリームの各シャードに保存可能なデータ量の上限を設定します。有効化して制限を設定するか、無効化して無制限（`infinity`）にできます。

   - **Max Shard Message Count**: ストリームの各シャードに保持可能な最大メッセージ数を設定します。有効化して制限を設定するか、無効化して無制限（`infinity`）にできます。

     ::: tip

     [シャード](../design/durable-storage.md#shard) の数は Durable Storage 設定でグローバルに定義され、すべてのストリームに適用されます。この制限はシャード単位であり、データのレプリケーションは考慮しません。ストレージ容量計画時は、シャード数とレプリケーション係数によりストリームの総ディスク使用量がスケールする点に注意してください。

     :::

3. 変更後、**Save Changes** をクリックして設定を適用します。

更新された設定は即時反映され、既存および新規ストリームに適用されます。

### REST API

EMQX の REST API を使ってグローバル MQTT Streams 設定をプログラムから構成できます。

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

EMQX の設定ファイルを編集してグローバル MQTT Streams 設定を構成できます。この方法は起動時のデフォルト動作定義や、設定ファイル管理が主な環境で有効です。

**設定例**：

MQTT Streams 設定は EMQX 設定ファイル（`emqx.conf`）の `streams` セクションに記述します。

```hocon
streams {
    gc_interval = 1h
    regular_stream_retention_period = 1d
    check_stream_status_interval = 10s
}
```

#### 設定オプション

- **gc_interval**: MQTT Streams の期限切れメッセージを削除する頻度を制御します。ストリームストレージのガベージコレクションサイクルに影響します。
- **regular_stream_retention_period**: 通常ストリームの最大保持期間を指定します。これを超えたメッセージは自動削除されます。
- **check_stream_status_interval**: クライアントが `$stream/<name>` トピックをサブスクライブし、対応するストリームが存在しない場合に再試行する間隔を指定します。

すべての期間値は `s`（秒）、`m`（分）、`h`（時間）、`d`（日）などの標準時間単位を使用します。

#### Durable Storage 設定

ストリームメッセージは EMQX Durable Storage に保存されます。MQTT Streams のストレージ関連設定は `durable_storage.streams_messages` セクションで構成します。

```hocon
durable_storage {
    ## ストリームメッセージを格納するデータベースの設定。
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

これらの設定は MQTT Streams データの永続化に関するトランザクションのバッチ処理やフラッシュ動作を制御します。通常はデフォルト値で十分であり、ストレージ性能チューニング時以外は変更不要です。

## REST API によるストリーム管理

EMQX はストリーム管理用の REST API を提供しています。これらの API を使ってストリームの作成、更新、一覧取得、照会、削除やグローバル設定の構成が可能です。自動化や外部システム連携、大規模管理に便利です。

::: tip 注意

すべての REST API 操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは [REST API](../../guides/api.md) の「MQTT Stream」セクションを参照してください。

:::

以下の例は API キーとシークレットによるベーシック認証を想定しています。

### ストリーム作成

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

### ストリーム一覧取得

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

### ストリーム更新

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

### ストリーム削除

ストリームを削除するには、ストリーム名で識別されるリソースに対して `DELETE` リクエストを送信します。

```bash
curl -s -u key:secret \
  -X DELETE \
  http://localhost:18083/api/v5/message_streams/streams/my_stream
```

削除後、ストリームはメッセージの収集を停止し、内部のクリーンアップルールに従って保存データが削除されます。

### ストリームグローバル設定の構成

[Configure Streams Settings -RESP API](#rest-api) を参照してください。
