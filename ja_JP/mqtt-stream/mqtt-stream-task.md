# MQTT Streams ユーザーガイド

このページでは、EMQX の MQTT Streams 機能の実践的な使い方について、ストリームの作成から動作設定、ダッシュボード、REST API、設定ファイルによる管理方法までを解説します。

## MQTT Streams 機能の有効化

MQTT Streams 機能はデフォルトで無効化されています。ストリームを作成または使用する前に、ダッシュボードで機能を有効にする必要があります。

1. 左メニューの **Streams** に移動します。
2. 機能が無効の場合は、無効である旨のメッセージが表示されます。
3. **Settings** をクリックして **Streams** 設定ページを開きます。
4. **Enable Streams** を **On** に切り替えます。
5. **Save Changes** をクリックします。

有効化すると、MQTT Streams 機能が即座に利用可能となり、ストリームの作成および管理を開始できます。

## ダッシュボードからのストリーム手動作成

MQTT Streams は、メッセージの保存や再生を行うために明示的に作成する必要があります。ストリームは手動または自動で作成・管理できます。自動作成の詳細は [ダッシュボードからの MQTT Streams 自動作成](#automatically-create-mqtt-streams-via-dashboard) を参照してください。

1. 左メニューの **Streams** に移動します。

2. **Create Stream** をクリックして **Create Stream** ダイアログを開きます。

3. 以下のオプションを設定します：

   - **Name**：必須。ストリームの一意の名前を指定します。名前には以下の文字のみ使用可能です：

     - 英数字（`A–Z`, `a–z`, `0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でストリームが識別・管理されます。

   - **Topic Filter**：ストリームに取り込むパブリッシュされたメッセージを定義するトピックまたはトピックフィルターを入力します（例：`t/1` や `sensors/+/data`）。このフィルターにマッチするトピックにパブリッシュされたすべてのメッセージがストリームに保存されます。

     > クライアントは以下のサブスクリプション形式でメッセージを消費できます：
     >
     > - `$stream/<name>` は既存のストリームに対して使用します。
     > - `$stream/<name>/<topic_filter>` は既存ストリームのサブスクライブ時に任意で使用可能です。自動作成が有効な場合に利用できます。ストリームが存在しない場合、EMQX は指定された `<topic_filter>` を使って自動的にストリームを作成します。
     >
     > `<topic_filter>` セグメントはストリームの設定されたトピックフィルターと一致する必要があります。
     >
     > 過去メッセージを再生するには、MQTT 5 のサブスクリプションプロパティ `stream-offset` を指定します。値は以下のいずれかです：
     >
     > - マイクロ秒単位の Unix タイムスタンプ
     > - `earliest`
     > - `latest`

   - **Data Retention Period**：メッセージの保持期間を指定します。設定した期間より古いメッセージは自動的に削除され、再生可能な過去メッセージの範囲を制限します。

   - **Last-Value Semantics**：このオプションを有効にすると、各キーに対して最新のメッセージのみを保持します。同じキーの新しいメッセージが古いメッセージを上書きします。デバイスの状態や設定など状態指向のデータに適しています。

   - **Stream Key Expression**：必須。各受信メッセージからキーを抽出するための式を定義します。デフォルトは `message.from` で、メッセージのパブリッシャーのクライアントIDを意味します。このフィールドは [Variform 式](../configuration/configuration.md#variform-expressions) による設定をサポートします。

      ::: tip
      
      Stream Key Expression は Message Queue の Queue Key Expression に似ています。キー抽出の例は [Queue Key Expression](../message-queue/message-queue-task.md#queue-key-expression) を参照してください。
      
      :::

      抽出されたキーはストリームの種類によって異なる役割を持ちます：
      
      - **Last-Value** ストリームでは、キーが主キーとして機能します。同じキーのメッセージは上書きされ、キーごとに最新のメッセージのみが保持されます。
      
      - **通常** のストリームでは、キーはシャーディングキーとして使われ、メッセージが書き込まれるストレージシャードを決定します。同じキーのメッセージは同じシャードにルーティングされ、キー単位の順序を保ちつつシャード間で並列保存が可能です。
      
        ::: tip
      
        通常ストリームでは、定数や低カーディナリティの式は避けてください。メッセージが単一シャードに集中し、書き込み性能に影響を与える可能性があります。
      
        :::

   - **Limiter**：ストリームの各シャードのストレージ使用量を制限する設定です：

      - **Max Shard Message Count**：各シャードに保持する最大メッセージ数を設定します。有効にして値を指定するか、無効にして無制限（`infinity`）にできます。
      
      - **Max Shard Message Bytes**：各シャードに保持するメッセージの合計サイズの上限を設定します（例：`200MB`）。有効にしてサイズを指定するか、無効にして無制限（`infinity`）にできます。

      これらの制限は永続ストレージに保存され、保持期間設定と連動して動作します。

4. **Create** をクリックしてストリームを保存します。

作成後、MQTT ストリームは即座に有効となり、設定したトピックフィルターにマッチするメッセージが保持期間およびリミッター設定に従って保存され、サブスクライブしたクライアントによって再生可能になります。

## ダッシュボードからの MQTT Streams 自動作成

クライアントが `$stream/<name>` プレフィックス付きのトピックをサブスクライブすると、MQTT Streams を自動的に作成できます。サブスクリプションの `<name>` がストリーム名になります。

::: tip 注意

自動ストリーム作成は、MQTT Streams 機能がグローバルに有効化されている場合のみ利用可能です。

:::

ストリームは通常ストリームまたは Last-Value セマンティクスストリームとして自動作成されます。

::: tip 注意

適切なストリーム動作を確保するため、自動作成は通常ストリームか Last-Value ストリームのいずれか一方のみ有効にできます。同時に両方を有効にすることはできません。

:::

### Last-Value ストリームの自動作成

このオプションはデフォルトで **Streams** タブの **MQTT Settings** 内にて有効になっています。EMQX は Last-Value セマンティクスを持つストリームを自動的に作成し、キーごとに最新のメッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Messages** タブに移動します。

2. デフォルトで **Enable Auto Create Stream** が有効で、**Last Value Stream** タイプが選択されています。

   以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルトは `message.from`）。Last-Value ストリームではこのキーが主キーとして機能し、同じキーのメッセージは上書きされ最新値のみが保持されます。
   - **Data Retention Period**：メッセージの保持期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$stream/my_stream/test` のようなトピックをサブスクライブすると、EMQX は `my_stream` という名前の Last-Value ストリームを自動作成し、**Streams** リストに表示されます。

### 通常ストリームの自動作成

メッセージを上書きせず独立して保存する通常ストリームを自動作成したい場合は、このオプションを手動で有効にします。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. デフォルトで **Enable Auto Create Message Stream** が有効です。**Regular Message Stream** タイプを選択します。

3. 以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルトは `message.from`）。

     通常ストリームでは、このキーがシャーディングキーとして使われ、メッセージが書き込まれるストレージシャードを決定します。同じキーのメッセージは同じシャードにルーティングされ、キー単位の順序を保ちつつシャード間で負荷分散されます。

   - **Data Retention Period**：メッセージの保持期間を指定します。

4. **Save Changes** をクリックします。

## ストリーム設定の構成

このセクションでは、EMQX のすべての MQTT Streams に適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージの保持、クリーンアップ間隔、内部ストリーム動作、自動作成動作を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQX ダッシュボードから直接 MQTT Streams の設定を変更でき、ブローカーの再起動は不要です。システム全体のストリーム動作をランタイムで調整するのに便利です。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. 以下のオプションを設定します：

   - **Enable Streams**：MQTT Streams 機能のグローバル有効化・無効化。無効時はストリームの作成や使用ができません。

   - **Max Stream Count**：クラスター内に存在可能な最大ストリーム数を設定します。過剰なストリーム作成によるリソース過剰使用を防止します。

   - **GC Interval**：期限切れのストリームメッセージをクリーンアップする間隔を指定します。デフォルトは `1` 時間です。

   - **Regular Stream Retention Period**：通常ストリーム（Last-Value でない）のデフォルト保持期間を定義します。これより古いメッセージは自動削除されます。デフォルトは `7` 日です。

   - **Enable Auto Create Message Stream**：クライアントがストリームトピックをサブスクライブし、該当ストリームが存在しない場合に自動作成を有効にします。

   - **Auto Create Stream Type**：自動作成するストリームのタイプを指定します：

     - **Last Value Stream**（デフォルト）：Last-Value セマンティクスを持つストリームを自動作成します。
     - **Regular Stream**：メッセージを上書きせずすべて保持する通常ストリームを自動作成します。

   - **Stream Key Expression**：Last-Value セマンティクス有効時に自動作成されるストリームで使用するキー抽出式を定義します。デフォルトは `message.from`。キー抽出によりキー単位の順序付けや上書き動作が決まります。

   - **Data Retention Period**：自動作成されるストリームの保持期間を指定します。期間を過ぎたメッセージは自動削除されます。

   - **Max Shard Message Bytes**：ストリームの各シャードに保存可能なデータ量の上限を設定します。有効にして制限を設けるか、無効にして無制限（`infinity`）にできます。

   - **Max Shard Message Count**：ストリームの各シャードに保持可能な最大メッセージ数を設定します。有効にして制限を設けるか、無効にして無制限（`infinity`）にできます。

     ::: tip

     [シャード](../design/durable-storage.md#shard) の数は Durable Storage の設定でグローバルに定義され、すべてのストリームに適用されます。この制限はシャード単位で適用され、データのレプリケーションは考慮されません。ストレージ容量計画時は、ストリームの総ディスク使用量がシャード数とレプリケーション係数に比例して増加する点に注意してください。

     :::

3. 設定変更後、**Save Changes** をクリックして反映します。

変更は即時に適用され、既存および新規ストリームに対して有効になります。

### REST API

EMQX REST API を使ってグローバル MQTT Streams 設定をプログラムから変更できます。

MQTT Streams グローバル設定を更新するには、以下のエンドポイントに `PUT` リクエストを送信します。

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

EMQX の設定ファイルを編集してグローバル MQTT Streams 設定を行うことも可能です。この方法は起動時のデフォルト動作定義や、設定ファイル管理が主な環境での運用に適しています。

**設定例**：

MQTT Streams の設定は EMQX 設定ファイル（`emqx.conf`）の `streams` セクションに記述します。

```hocon
streams {
    gc_interval = 1h
    regular_stream_retention_period = 1d
    check_stream_status_interval = 10s
}
```

#### 設定項目

- **gc_interval**：期限切れメッセージを MQTT Streams から削除する頻度を制御します。ストリームストレージのガベージコレクション周期に影響します。
- **regular_stream_retention_period**：通常ストリームの最大保持期間を指定します。この期間を超えたメッセージは自動削除されます。
- **check_stream_status_interval**：クライアントが `$stream/<name>` トピックをサブスクライブし、該当ストリームが存在しない場合にストリームを探す再試行間隔を指定します。

期間は `s`（秒）、`m`（分）、`h`（時間）、`d`（日）などの標準的な時間単位を使用します。

#### Durable Storage 設定

ストリームメッセージは EMQX Durable Storage に保存されます。MQTT Streams のストレージ関連設定は `durable_storage.streams_messages` セクションで行います。

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

これらの設定は MQTT Streams データの永続化におけるトランザクションのバッチ処理やフラッシュ動作を制御します。通常はデフォルト値で十分であり、ストレージ性能のチューニング時のみ調整が必要です。

## REST API によるストリーム管理

EMQX はストリーム管理用の REST API を提供しています。これらの API を使ってストリームの作成、更新、一覧取得、クエリ、削除やグローバル設定の構成が可能です。自動化や外部システムとの連携、大規模管理に便利です。

::: tip 注意

すべての REST API 操作には適切な認証と権限が必要です。リクエストおよびレスポンスの詳細なスキーマは [REST API](../admin/api.md) の「MQTT Stream」セクションを参照してください。

:::

以下の例はすべて API キーとシークレットによるベーシック認証を前提としています。

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

レスポンスはストリームのリストとページネーション情報を含みます。

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

レスポンスには更新後のストリーム設定が返されます。

### ストリームの削除

ストリームを削除するには、ストリーム名で識別されるリソースに `DELETE` リクエストを送信します。

```bash
curl -s -u key:secret \
  -X DELETE \
  http://localhost:18083/api/v5/message_streams/streams/my_stream
```

削除後、ストリームはメッセージの収集を停止し、内部のクリーンアップルールに従って保存データが削除されます。

### ストリームのグローバル設定構成

詳細は [Configure Streams Settings -REST API](#rest-api) を参照してください。
