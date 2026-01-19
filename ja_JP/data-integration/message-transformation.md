# メッセージ変換

メッセージ変換は、ユーザー定義のルールに基づいてメッセージを処理またはサブスクライバーに配信する前に変更およびフォーマットする機能です。この機能は非常にカスタマイズ可能で、複数のエンコーディングや高度な変換をサポートしています。

## ワークフロー

メッセージがパブリッシュされると、以下のワークフローを経ます。

1. **スキーマ検証**：メッセージがパブリッシュされ認可を通過すると、まず[スキーマ検証](./schema-validation.md)が行われます。メッセージが検証に合格すると、次のステップに進みます。

2. **メッセージ変換パイプライン**：

   - **変換マッチング**：メッセージはトピックに基づいてユーザー定義の変換リストと照合されます。異なるトピックやトピックフィルターに対して複数の変換を設定可能です。
   - **変換実行**：マッチした変換は設定された順序で実行されます。パイプラインはJSON、Protobuf、Avroなどの各種エンコーダー・デコーダーをサポートし、[Variform式](../configuration/configuration.md#variform-expressions)を用いてメッセージの拡充や変更が可能です。
   - **変換後処理**：メッセージが変換パイプラインを正常に通過すると、ルールエンジンのトリガーやサブスクライバーへのメッセージ配信などの次の処理に進みます。

3. **失敗時の処理**：変換が失敗した場合、ユーザー設定のアクションが実行されます。

   - **メッセージ破棄**：パブリッシュを終了しメッセージを破棄、QoS 1およびQoS 2メッセージにはPUBACKで特定の理由コード（131 - Implementation Specific Error）を返します。
   - **切断してメッセージ破棄**：メッセージを破棄し、パブリッシュしたクライアントを切断します。
   - **無視**：追加のアクションは行いません。

   変換失敗時には設定に関わらずログエントリが生成される場合があります。ログの出力レベルはユーザーが設定可能で、デフォルトは`warning`です。さらに、変換失敗はルールエンジンのイベント（`$events/message_transformation/failed`）をトリガーでき、誤ったメッセージを別トピックに再パブリッシュしたり、Kafkaに送信して解析するなどのカスタム処理が可能です。

## ユーザーガイド

このセクションでは、メッセージ変換機能の設定方法とテスト方法を説明します。

### ダッシュボードでのメッセージ変換設定

ダッシュボードでメッセージ変換を作成・設定する手順は以下の通りです。

1. ダッシュボードの左ナビゲーションメニューから **Smart Data Hub** -> **Message Transform** をクリックします。

2. **Message Transform** ページ右上の **Create** をクリックします。

3. 「Create Message Transform」ページで以下の情報を設定します：

   - **Name**：変換の名前を入力します。

   - **Message Source Topic**：変換対象とするメッセージのトピックを設定します。複数のトピックやトピックフィルターを設定可能です。

   - **Note**（任意）：メモを入力します。

   - **Message Format Transformation**：
     - **Source Format**：変換パイプラインに入るメッセージのペイロードデコーダーを指定します。選択肢は以下の通りです：

       - `None`（デコードなし）
       - `JSON`
       - `Avro`
       - `Protobuf`
       - `Custom (External HTTP)`

       これらのデコーダーはバイナリの入力ペイロードを構造化されたマップに変換します。`Avro`、`Protobuf`、`Custom (External HTTP)`を選択する場合は、あらかじめ[スキーマレジストリ](./schema-registry.md)で作成しておく必要があります。

       複数の変換が連なるパイプラインでは、各ステップでデコードを行う必要はありません。例えば、変換`T1`でペイロードがすでにデコードされていれば、続く変換`T2`はデコードをスキップし、正しい形式のペイロードを利用できます。

     - **Target Format**：変換パイプラインの最後にメッセージペイロードをバイナリ値としてエンコードするためのエンコーダーを指定します。エンコーダーの選択肢は**Source Format**と同じです。

       パイプラインの最後の変換のみがバイナリエンコードを行えばよく、中間の変換はバイナリエンコードを処理する必要はありません。

   - **Message Properties Transformation**：

     - **Properties**：式の結果として得られた変換後の値を書き込む先を指定します。有効な書き込み先は`payload`、`topic`、`qos`、`retain`（対応するフラグを設定）、および`user_property`（MQTTのUser-Propertyプロパティ用）です。`user_property`を使用する場合は、このフィールドの下に正確に1つのキーを指定する必要があります（例：`user_property.my_custom_prop`）。`payload`はそのまま使用してメッセージペイロード全体を上書きするか、ネストされたJSONオブジェクトのように特定のキー階層を指定できます（例：`payload.x.y`）。
     - **Target Value**：設定したプロパティに書き込む値を定義します。この値は`qos`、`retain`、`topic`、`payload`、`payload.x.y`など他のフィールドからコピーするか、[variform式](../configuration/configuration.md#variform-expressions)を使って生成できます。

   - **Transformation Failure Operation**：
     - **Action After Failure**：変換失敗時に実行するアクションを選択します：
       - **Drop Message**：パブリッシュ処理を終了しメッセージを破棄、QoS 1およびQoS 2メッセージにはPUBACKで特定の理由コードを返します。
       - **Disconnect and Drop Message**：メッセージを破棄し、パブリッシュしたクライアントを切断します。
       - **Ignore**：追加のアクションは行いません。

   - **Output Logs**：変換失敗時にログを生成するかどうかを選択します。ログはデフォルトで有効です。

   - **Logs Level**：ログの出力レベルを設定します。デフォルトは`warning`です。

4. 設定が完了したら **Create** をクリックして作成を完了します。

作成前に変換をテストしたい場合は、**Preview** をクリックします。新しいペインが開き、QoS、ペイロード、リテインフラグの有無、パブリッシャーのユーザー名やクライアントIDなどのメッセージコンテキストを入力できます。必要な情報を入力後、**Execute Transformation** をクリックすると指定したコンテキストで変換を実行し、結果を確認できます。

作成した変換はメッセージ変換ページのリストに表示され、デフォルトで有効になります。必要に応じて無効化したり、**Actions**列の**Settings**から設定を更新できます。削除や順序変更は**More**から行えます。

### 設定ファイルでのメッセージ変換設定

Avro形式でエンコードされたメッセージをJSONにデコードし、さらにパブリッシュクライアントのクライアント属性から取得した`tenant`属性をトピックの先頭に付加してからルールエンジンで処理したい場合、以下のように設定できます。

```hocon
message_transformation {
  transformations = [
    {
      name = mytransformation
      topics = ["t"]
      failure_action = drop
      payload_decoder = {type = avro, schema = myschema}
      payload_encoder = {type = json}
      operations = [
        {key = "topic", value = "concat([client_attrs.tenant, '/', topic])"}
      ]
    }
  ]
}
```

この設定は、`mytransformation`という名前の変換を指定し、

- 指定したスキーマでAvro形式のペイロードをデコードし、
- ペイロードをJSON形式にエンコードし、
- クライアント属性の`tenant`と元のトピックを連結してトピックを変更します。

詳細な設定方法は[Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

### REST API

REST APIを使ったメッセージ変換の詳細な利用方法は[EMQX Enterprise API](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)をご覧ください。

### デコード／エンコードスキーマの作成

デコーダーおよびエンコーダースキーマの作成方法については、[スキーマレジストリ](./schema-registry.md)のセクションを参照してください。

## 統計と指標

メッセージ変換が有効化されている場合、ダッシュボードで統計情報や指標を確認できます。メッセージ変換ページで変換名をクリックすると、以下の情報が表示されます。

**統計情報**：

- **Total**：システム起動以来のトリガー総数
- **Success**：成功したデータ変換の数
- **Failed**：失敗したデータ変換の数

**レート指標**：

- 現在の変換スループット
- 過去5分間のスループット
- 過去の最大スループット

統計はリセット可能で、Prometheusの`/prometheus/message_transformation`からも取得できます。
