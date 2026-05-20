# MQTT Streams ユーザーガイド

このページでは、EMQXのMQTT Streams機能の実践的な使い方について、ストリームの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## MQTT Streams機能の有効化

MQTT Streams機能はデフォルトで無効になっています。ストリームを作成または使用する前に、ダッシュボードで機能を有効化する必要があります。

1. 左メニューの **Streams** に移動します。
2. 機能が無効の場合、無効である旨のプロンプトが表示されます。
3. **Settings** をクリックして **Streams** 設定ページを開きます。
4. **Enable Streams** を **On** に切り替えます。
5. **Save Changes** をクリックします。

有効化すると、MQTT Streams機能が即座に利用可能となり、ストリームの作成や管理を開始できます。

## ダッシュボードでのストリーム手動作成

MQTT Streamsは、メッセージの保存や再生を行うために明示的に作成する必要があります。ストリームは手動または自動で作成・管理できます。自動作成の詳細は[ダッシュボードでのMQTT Streams自動作成](#自動でmqtt-streamsをダッシュボードから作成する)を参照してください。

1. 左メニューの **Streams** に移動します。

2. **Create Stream** をクリックして **Create Stream** ダイアログを開きます。

3. 以下のオプションを設定します：

   - **Name**：必須。ストリームの一意な名前を指定します。ストリーム名には以下の文字のみ使用可能です：

     - 英数字 (`A–Z`, `a–z`, `0–9`)
     - アンダースコア (`_`)
     - ハイフン (`-`)
     - ドット (`.`)

     この名前でストリームは識別・管理されます。

   - **Topic Filter**：パブリッシュされたメッセージをキャプチャする対象のトピックまたはトピックフィルター（例：`t/1` や `sensors/+/data`）を入力します。このフィルターにマッチするトピックにパブリッシュされたすべてのメッセージがストリームに保存されます。

     > クライアントは以下のサブスクライブ形式でメッセージを消費できます：
     >
     > - ストリームが既に存在する場合は `$stream/<name>` を使用します。
     > - `$stream/<name>/<topic_filter>` は既存ストリームへのサブスクライブ時にオプションで使用可能です。自動作成が有効な場合に使え、ストリームが存在しない場合は指定した `<topic_filter>` を用いてEMQXが自動作成します。
     >
     > `<topic_filter>` セグメントはストリームの設定済みトピックフィルターと一致する必要があります。
     >
     > 過去メッセージを再生するには、MQTT 5のサブスクリプションプロパティ `stream-offset` を指定します。値は以下のいずれかです：
     >
     > - マイクロ秒単位のUnixタイムスタンプ
     > - `earliest`
     > - `latest`
     
   - **Data Retention Period**：メッセージの保持期間を指定します。保持期間を過ぎたメッセージは自動的に削除され、再生可能な過去メッセージの範囲を制限します。
     
   - **Last-Value Semantics**：このオプションを有効にすると、各キーごとに最新のメッセージのみを保持します。同じキーの新しいメッセージが古いメッセージを上書きします。デバイスの状態や設定など状態指向データに適しています。
     
   - **Stream Key Expression**：必須。各メッセージからキーを抽出するための式を定義します。デフォルトは `message.from` で、メッセージのパブリッシャーのクライアントIDを意味します。このフィールドは[Variform式](../configuration/configuration.md#variform-expressions)で設定可能です。
     
      ::: tip
      
      Stream Key ExpressionはMessage QueueのQueue Key Expressionに似ています。キー抽出の例は[Queue Key Expression](../message-queue/message-queue-task.md#queue-key-expression)を参照してください。
      
      :::
      
      抽出されたキーはストリームの種類により役割が異なります：
        - **Last-Value** ストリームではキーが主キーとして機能し、同じキーのメッセージは上書きされ、最新のメッセージのみが保持されます。
      
        - **通常** ストリームではキーがシャーディングキーとして使われ、同じキーのメッセージは同じシャードに書き込まれます。これによりキー単位の順序性を保ちつつ、シャード間で並列に保存できます。
      
          ::: tip
      
          通常ストリームでは、定数や低カーディナリティの式は避けてください。メッセージが単一シャードに集中し書き込み性能に影響する可能性があります。
      
          :::
      
   - **Limiter**：ストリームの各シャードに対するストレージ使用制限を設定します：
     
      - **Max Shard Message Count**：各シャードに保持する最大メッセージ数を設定します。有効化して値を指定するか、無効化して無制限（`infinity`）にできます。
     - **Max Shard Message Bytes**：各シャードに保持するメッセージの最大合計サイズを設定します（例：`200MB`）。有効化してサイズを指定するか、無効化して無制限（`infinity`）にできます。
     
      これらの制限は永続化ストレージに保存され、保持期間設定と連動して動作します。
   
4. **Create** をクリックしてストリームを保存します。

作成後、MQTTストリームは即時に有効化されます。設定したトピックフィルターにマッチするトピックにパブリッシュされたメッセージは保持期間やリミッター設定に従って保存され、ストリームをサブスクライブするクライアントにより再生可能です。

## ダッシュボードでのMQTT Streams自動作成

クライアントが `$stream/<name>` プレフィックス付きトピックにサブスクライブすると、MQTT Streamsが自動的に作成されます。サブスクライブの `<name>` がストリーム名となります。

::: tip 注意

自動ストリーム作成はMQTT Streams機能がグローバルに有効な場合のみ利用可能です。

:::

自動作成されるストリームは通常ストリームまたはLast-Valueセマンティクスストリームのいずれかです。

::: tip 注意

適切な動作を確保するため、自動作成は通常ストリームかLast-Valueストリームのいずれか一方のみを有効にできます。両方同時にはできません。

:::

### Last-Valueストリームの自動作成

このオプションはデフォルトで **Streams** タブの **MQTT Settings** にて有効になっています。EMQXはLast-Valueセマンティクスをサポートするストリームを自動作成し、同じキーの最新メッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Messages** タブに移動します。

2. デフォルトで **Enable Auto Create Stream** が有効で、**Last Value Stream** タイプが選択されています。

   以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。Last-Valueストリームではこのキーが主キーとして機能し、同じキーのメッセージは上書きされ最新値のみ保持されます。
   - **Data Retention Period**：メッセージの保持期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$stream/my_stream/test` のようなトピックにサブスクライブすると、EMQXは `my_stream` というLast-Valueストリームを自動作成し、**Streams** リストに表示されます。

### 通常ストリームの自動作成

メッセージを上書きせず独立して保存する通常ストリームを自動作成したい場合に手動で有効化できます。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. デフォルトで **Enable Auto Create Message Stream** が有効です。**Regular Message Stream** タイプを選択します。

3. 以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。

     通常ストリームでは、このキーがシャーディングキーとして使われ、同じキーのメッセージは同じシャードにルーティングされます。これによりキー単位の順序性を保ちつつ、シャード間で負荷分散が可能です。

   - **Data Retention Period**：メッセージの保持期間を指定します。

4. **Save Changes** をクリックします。

## ストリーム設定の構成

このセクションでは、EMQXのすべてのMQTT Streamsに適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージ保持、クリーンアップ間隔、内部ストリーム動作、自動作成動作を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQXダッシュボードからMQTT Streams設定を直接更新できます。ブローカーの再起動は不要で、システム全体のストリーム動作をランタイムに調整可能です。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. 以下のオプションを設定します：

   - **Enable Streams**：MQTT Streams機能のグローバル有効化・無効化。無効時はストリームの作成・使用ができません。

   - **Max Stream Count**：クラスター内に存在可能なストリームの最大数。無制限のストリーム作成によるリソース過剰使用を防止します。

   - **GC Interval**：期限切れストリームメッセージのクリーンアップ間隔。デフォルトは `1` 時間です。

   - **Regular Stream Retention Period**：通常ストリーム（Last-Valueでない）のデフォルト保持期間。これを超えたメッセージは自動削除されます。デフォルトは `7` 日です。

   - **Enable Auto Create Message Stream**：クライアントがストリームトピックにサブスクライブし、該当ストリームが存在しない場合に自動作成を有効化します。

   - **Auto Create Stream Type**：自動作成するストリームのタイプを指定します：

     - **Last Value Stream**（デフォルト）：Last-Valueセマンティクスを有効にしたストリームを自動作成します。
     - **Regular Stream**：メッセージを上書きしない通常ストリームを自動作成します。

   - **Stream Key Expression**：Last-Valueセマンティクスが有効な自動作成ストリームで使用するキー抽出式。デフォルトは `message.from`。キー単位の順序性や上書き動作を決定します。

   - **Data Retention Period**：自動作成ストリームの保持期間。期間を超えたメッセージは自動削除されます。

   - **Max Shard Message Bytes**：ストリームの各シャードに保存可能なデータ量の上限。制限を有効化して設定するか、無効化して無制限（`infinity`）にできます。

   - **Max Shard Message Count**：ストリームの各シャードに保存可能な最大メッセージ数。制限を有効化して設定するか、無効化して無制限（`infinity`）にできます。

     ::: tip

     [シャード](../design/durable-storage.md#shard)の数はDurable Storageの設定でグローバルに定義され、すべてのストリームに適用されます。この制限はシャード単位で適用され、データの複製は考慮しません。ストレージ容量計画時は、ストリームの総ディスク使用量がシャード数と複製係数に比例して増加する点に注意してください。

     :::

3. 設定変更後、**Save Changes** をクリックして適用します。

変更は即時反映され、既存および新規作成ストリームに適用されます。

### REST API

EMQX REST APIを使ってグローバルMQTT Streams設定をプログラム的に構成できます。

MQTT Streamsグローバル設定を更新するには、以下のエンドポイントに `PUT` リクエストを送信します：

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

EMQXの設定ファイルを編集してグローバルMQTT Streams設定を行うこともできます。起動時のデフォルト動作定義や、設定ファイル管理が主な環境で有効です。

**設定例**：

MQTT Streams設定はEMQX設定ファイル（`emqx.conf`）の `streams` セクションに記述します。

```hocon
streams {
    gc_interval = 1h
    regular_stream_retention_period = 1d
    check_stream_status_interval = 10s
}
```

#### 設定項目

- **gc_interval**：期限切れメッセージをストリームから削除する頻度を制御します。ストリームストレージのガベージコレクションサイクルに影響します。
- **regular_stream_retention_period**：通常ストリームの最大保持期間を指定します。期間を超えたメッセージは自動削除されます。
- **check_stream_status_interval**：`$stream/<name>` トピックにサブスクライブ時、対応するストリームが存在しない場合にサブスクライバーがストリーム検出を再試行する間隔です。

すべての期間値は `s`（秒）、`m`（分）、`h`（時間）、`d`（日）などの標準時間単位を使用します。

#### Durable Storage設定

ストリームメッセージはEMQX Durable Storageに保存されます。MQTT Streamsのストレージ関連設定は `durable_storage.streams_messages` セクションで行います。

```hocon
durable_storage {
    ## ストリームメッセージを保存するデータベースの設定
    ## 詳細はDurable Storage設定を参照してください
    streams_messages {
        transaction {
            flush_interval = 100
            idle_flush_interval = 20
            conflict_window = 5000
        }
    }
}
```

これらの設定はMQTT Streamsデータの永続化方法、トランザクションのバッチ処理やフラッシュ動作を制御します。ほとんどの場合、デフォルト値で十分であり、ストレージ性能調整時のみ変更が必要です。

## REST APIによるストリーム管理

EMQXはストリーム管理用のREST APIを提供しています。これらのAPIを使い、ストリームの作成、更新、一覧取得、照会、削除、グローバル設定構成が可能です。自動化や外部システム連携、大規模管理に便利です。

::: tip 注意

すべてのREST API操作は適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは[REST API](../admin/api.md)の「MQTT Stream」セクションを参照してください。

:::

以下の例はAPIキーとシークレットによるベーシック認証を想定しています。

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

既存ストリームを更新するには、ストリーム名で識別されるリソースに `PUT` リクエストを送信します。トピックフィルターはURLエンコードしてください。

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

レスポンスは更新後のストリーム設定を返します。

### ストリームの削除

ストリームを削除するには、ストリーム名で識別されるリソースに `DELETE` リクエストを送信します。

```bash
curl -s -u key:secret \
  -X DELETE \
  http://localhost:18083/api/v5/message_streams/streams/my_stream
```

削除後、ストリームはメッセージ収集を停止し、内部クリーンアップルールに従い保存データが削除されます。

### ストリームのグローバル設定構成

[Configure Streams Settings -RESP API](#rest-api) を参照してください。
