# MQTT Streams ユーザーガイド

このページでは、EMQXのMQTT Streams機能の実際の使い方について、ストリームの作成から動作設定、ダッシュボード、REST API、設定ファイルを使った管理方法までを解説します。

## MQTT Streams機能の有効化

MQTT Streams機能はデフォルトで無効になっています。ストリームを作成または使用する前に、ダッシュボードで機能を有効化する必要があります。

1. 左メニューの **Streams** に移動します。
2. 機能が無効の場合、無効である旨のメッセージが表示されます。
3. **Settings** をクリックして **Streams** 設定ページを開きます。
4. **Enable Streams** を **On** に切り替えます。
5. **Save Changes** をクリックします。

有効化すると、MQTT Streams機能が即座に利用可能となり、ストリームの作成や管理を開始できます。

## ダッシュボードからのストリーム手動作成

MQTT Streamsは、メッセージの保存や再生を行う前に明示的に作成する必要があります。ストリームは手動または自動で作成・管理できます。自動作成の詳細は[ダッシュボードからの自動作成](#automatically-create-mqtt-streams-via-dashboard)をご覧ください。

1. 左メニューの **Streams** に移動します。

2. **Create Stream** をクリックして **Create Stream** ダイアログを開きます。

3. 以下のオプションを設定します：

   - **Name**：必須。ストリームの一意な名前を指定します。名前は以下の文字のみ使用可能です：

     - 英数字（`A–Z`、`a–z`、`0–9`）
     - アンダースコア（`_`）
     - ハイフン（`-`）
     - ドット（`.`）

     この名前でストリームが識別・管理されます。

   - **Topic Filter**：ストリームに取り込むパブリッシュされたメッセージを定義するトピックまたはトピックフィルターを入力します（例：`t/1`、`sensors/+/data`）。このフィルターにマッチするトピックにパブリッシュされたすべてのメッセージがストリームに保存されます。
   
     > クライアントは以下のサブスクリプション形式でメッセージを消費できます：
     >
     > - ストリームが既に存在する場合は `$stream/<name>` を使用します。
     > - `$stream/<name>/<topic_filter>` は既存ストリームのサブスクライブ時に任意で使用可能です。自動作成が有効な場合に使えます。ストリームが存在しない場合、EMQXは指定された `<topic_filter>` を用いて自動的にストリームを作成します。
     > 
     > `<topic_filter>` セグメントはストリームの設定されたトピックフィルターと一致する必要があります。
     >
     > 過去メッセージを再生するには、MQTT 5のサブスクリプションプロパティ `stream-offset` を指定します。値は以下のいずれかです：
     >
     > - マイクロ秒単位のUnixタイムスタンプ
     > - `earliest`
     > - `latest`
     
   - **Data Retention Period**：メッセージの保持期間を指定します。設定期間より古いメッセージは自動的に削除され、再生可能な期間が制限されます。
     
   - **Last-Value Semantics**：このオプションを有効にすると、各キーに対して最新のメッセージのみを保持します。同じキーの新しいメッセージが古いメッセージを上書きします。デバイス状態や設定などの状態指向データに適しています。
     
   - **Stream Key Expression**：必須。受信メッセージからキーを抽出するための式を定義します。デフォルトは `message.from`（メッセージのパブリッシャーのクライアントID）です。このフィールドは[Variform式](../configuration/configuration.md#variform-expressions)で設定可能です。
     
      ::: tip
      
      Stream Key ExpressionはMessage QueueのQueue Key Expressionに類似しています。キー抽出の例は[Queue Key Expression](../message-queue/message-queue-task.md#queue-key-expression)をご参照ください。
      
      :::
      
      抽出されたキーはストリームタイプによって異なる役割を持ちます：
        - **Last-Value** ストリームでは、キーはプライマリキーとして機能します。同じキーのメッセージは上書きされ、キーごとに最新のメッセージのみ保持されます。
      
        - **通常** ストリームでは、キーはシャーディングキーとして使われ、メッセージがどのストレージシャードに書き込まれるかを決定します。同じキーのメッセージは同じシャードにルーティングされ、キー単位の順序を保ちつつシャード間で並列に保存されます。
      
          ::: tip
      
          通常ストリームでは、定数や低カーディナリティの式を使うとメッセージが単一シャードに集中し、書き込み性能に影響するため避けてください。
      
          :::
      
   - **Limiter**：ストリームの各シャードのストレージ使用量を制限する設定です：
     
      - **Max Shard Message Count**：各シャードに保持する最大メッセージ数を設定します。有効にして値を指定するか、無効にして無制限（`infinity`）にできます。
     - **Max Shard Message Bytes**：各シャードに保持するメッセージ合計サイズの上限を設定します（例：`200MB`）。有効にしてサイズを指定するか、無効にして無制限（`infinity`）にできます。
     
      これらの制限は永続ストレージに保存され、保持期間設定と連携して動作します。
   
4. **Create** をクリックしてストリームを保存します。

作成後、MQTTストリームは即座に有効となり、設定したトピックフィルターにマッチするメッセージが保持期間とリミッター設定に従って保存され、サブスクライブしたクライアントから再生可能になります。

## ダッシュボードからのストリーム自動作成

クライアントが `$stream/<name>` プレフィックスのトピックにサブスクライブすると、MQTTストリームが自動的に作成されます。サブスクリプションの `<name>` がストリーム名になります。

::: tip 注意

自動ストリーム作成はMQTT Streams機能がグローバルに有効な場合のみ利用可能です。

:::

自動作成されるストリームは通常ストリームまたはLast-Valueセマンティクスストリームのいずれかです。

::: tip 注意

ストリームの動作を適切に保つため、自動作成は通常ストリームかLast-Valueストリームのいずれか一方のみを有効にしてください。両方を同時に有効にすることはできません。

:::

### Last-Valueストリームの自動作成

このオプションはデフォルトで **Streams** タブの **MQTT Settings** 内にて有効になっています。Last-Valueセマンティクス対応のストリームを自動作成し、キーごとに最新のメッセージのみを保持します。

1. **Management** -> **MQTT Settings** -> **Messages** タブに移動します。

2. デフォルトで **Enable Auto Create Stream** が有効で、**Last Value Stream** タイプが選択されています。

   以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。Last-Valueストリームではこのキーがプライマリキーとなり、同じキーのメッセージは上書きされ最新の値のみ保持されます。
   - **Data Retention Period**：メッセージの保持期間を指定します。

3. **Save Changes** をクリックします。

クライアントが `$stream/my_stream/test` のようなトピックにサブスクライブすると、EMQXは自動的に `my_stream` という名前のLast-Valueストリームを作成し、**Streams** リストに表示されます。

### 通常ストリームの自動作成

メッセージを独立して保存し上書きしない通常ストリームを自動作成したい場合は、このオプションを手動で有効にします。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. デフォルトで **Enable Auto Create Message Stream** が有効です。**Regular Message Stream** タイプを選択します。

3. 以下を設定します：

   - **Stream Key Expression**：必須。各メッセージから一意のキーを抽出する方法を定義します（デフォルト：`message.from`）。

     通常ストリームでは、このキーがシャーディングキーとして使われ、メッセージがどのストレージシャードに書き込まれるかを決定します。同じキーのメッセージは同じシャードにルーティングされ、キー単位の順序を保ちつつシャード間で負荷分散されます。

   - **Data Retention Period**：メッセージの保持期間を指定します。

4. **Save Changes** をクリックします。

## ストリーム設定の構成

このセクションでは、EMQXのすべてのMQTTストリームに適用されるグローバル設定の構成方法を説明します。これらの設定はメッセージ保持、クリーンアップ間隔、内部ストリーム動作、自動作成の挙動を制御します。ダッシュボード、REST API、設定ファイルから設定可能です。

### ダッシュボード

EMQXダッシュボードからMQTT Streams設定を直接更新でき、ブローカーの再起動は不要です。システム全体のストリーム動作をランタイムで調整するのに便利です。

1. **Management** -> **MQTT Settings** -> **Streams** タブに移動します。

2. 以下のオプションを設定します：

   - **Enable Streams**：MQTT Streams機能をグローバルに有効または無効にします。無効時はストリームの作成・使用ができません。

   - **Max Stream Count**：クラスター内に存在可能なストリームの最大数を設定します。過剰なストリーム作成によるリソース過剰使用を防ぎます。

   - **GC Interval**：期限切れのストリームメッセージをクリーンアップする間隔を指定します。デフォルトは `1` 時間です。

   - **Regular Stream Retention Period**：通常（Last-Valueでない）ストリームのデフォルト保持期間を定義します。期間を超えたメッセージは自動削除されます。デフォルトは `7` 日です。

   - **Enable Auto Create Message Stream**：クライアントがストリームトピックにサブスクライブし、該当ストリームが存在しない場合に自動作成を有効にします。

   - **Auto Create Stream Type**：自動作成するストリームのタイプを指定します：

     - **Last Value Stream**（デフォルト）：Last-Valueセマンティクス対応のストリームを自動作成します。
     - **Regular Stream**：メッセージを上書きせずすべて保持する通常ストリームを自動作成します。

   - **Stream Key Expression**：Last-Valueセマンティクスが有効な自動作成ストリームで使用するキー抽出式を定義します。デフォルトは `message.from` です。キーごとの順序付けや上書き動作に影響します。

   - **Data Retention Period**：自動作成ストリームの保持期間を指定します。期間超過メッセージは自動削除されます。

   - **Max Shard Message Bytes**：ストリームの各シャードに保存可能なデータ量の上限を設定します。有効にして制限を設定するか、無効にして無制限（`infinity`）にできます。

   - **Max Shard Message Count**：ストリームの各シャードに保存可能な最大メッセージ数を設定します。有効にして制限を設定するか、無効にして無制限（`infinity`）にできます。

     ::: tip

     [シャード](../design/durable-storage.md#shard)の数はDurable Storage設定でグローバルに定義され、すべてのストリームに適用されます。この制限はシャード単位で適用され、データのレプリケーションは考慮しません。ストレージ容量計画時は、ストリームの総ディスク使用量がシャード数とレプリケーション係数に比例して増加する点に注意してください。

     :::

3. 設定変更後、**Save Changes** をクリックして反映します。

更新された設定は即時に適用され、既存および新規ストリームに影響します。

### REST API

EMQXのREST APIを使ってプログラムからグローバルMQTT Streams設定を構成できます。

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

EMQXの設定ファイルを編集してグローバルMQTT Streams設定を行うことも可能です。起動時のデフォルト動作定義や設定ファイル管理が主な環境に適しています。

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

- **gc_interval**：期限切れメッセージの削除頻度を制御します。ストリームストレージのガベージコレクションサイクルに影響します。
- **regular_stream_retention_period**：通常ストリームの最大保持期間を指定します。期間を超えたメッセージは自動削除されます。
- **check_stream_status_interval**：サブスクライバーが `$stream/<name>` トピックにサブスクライブし、対応するストリームが存在しない場合にストリーム検出を再試行する頻度を指定します。

すべての期間値は `s`（秒）、`m`（分）、`h`（時間）、`d`（日）などの標準時間単位を使用します。

#### Durable Storage設定

ストリームメッセージはEMQX Durable Storageに保存されます。MQTT Streamsのストレージ関連設定は `durable_storage.streams_messages` セクションで構成します。

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

これらの設定はMQTTストリームデータのDurable Storageへの書き込み方法（トランザクションのバッチ処理やフラッシュ動作）を制御します。通常はデフォルト値で十分であり、ストレージ性能調整時のみ変更が必要です。

## REST APIによるストリーム管理

EMQXはストリーム管理用のREST APIを提供しています。これらのAPIを使ってストリームの作成、更新、一覧取得、照会、削除やグローバルMQTT Streams設定の構成が可能です。自動化や外部システム連携、大規模管理に便利です。

::: tip 注意

すべてのREST API操作には適切な認証と権限が必要です。リクエスト・レスポンスの詳細スキーマは[REST API](../admin/api.md)の「MQTT Stream」セクションを参照してください。

:::

以下の例はすべてAPIキーとシークレットを用いたベーシック認証を前提としています。

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

[Configure Streams Settings -REST API](#rest-api) を参照してください。
