# メッセージ変換

メッセージ変換は、ユーザー定義のルールに基づいてメッセージを処理またはサブスクライバーに配信する前に変更・フォーマットする機能です。この機能は高度にカスタマイズ可能で、複数のエンコーディングや高度な変換をサポートしています。

## ワークフロー

メッセージがパブリッシュされると、以下のワークフローを経ます：

1. **スキーマ検証**：メッセージがパブリッシュされ認可を通過すると、まず[スキーマ検証](./schema-validation.md)が行われます。メッセージが検証に合格すると次のステップに進みます。

2. **メッセージ変換パイプライン**：

   - **変換マッチング**：メッセージはトピックに基づいてユーザー定義の変換リストと照合されます。異なるトピックやトピックフィルターに対して複数の変換を設定可能です。
   - **変換実行**：マッチした変換は設定された順序で実行されます。パイプラインはJSON、Protobuf、Avroなどの各種エンコーダー・デコーダーをサポートし、[Variform式](../../guides/configuration/configuration.md#variform-expressions)を用いてメッセージの拡張や変更が可能です。
   - **変換後処理**：メッセージが変換パイプラインを正常に通過すると、ルールエンジンの起動やサブスクライバーへの配信など次の処理に進みます。

3. **失敗時の処理**：変換が失敗した場合、ユーザー設定のアクションが実行されます：

   - **メッセージ破棄**：パブリッシュを終了しメッセージを破棄、QoS 1およびQoS 2メッセージにはPUBACKで特定の理由コード（131 - 実装固有エラー）を返します。
   - **切断してメッセージ破棄**：メッセージを破棄し、パブリッシュしたクライアントを切断します。
   - **無視**：追加の処理は行いません。

   変換失敗時には設定にかかわらずログが生成可能で、ログ出力レベルはデフォルトで`warning`です。また、変換失敗はルールエンジンイベント（`$events/message_transformation_failed`）をトリガーでき、誤ったメッセージを別トピックに再パブリッシュしたりKafkaに送信して解析するなどのカスタム処理が可能です。

## ユーザーガイド

このセクションでは、メッセージ変換機能の設定方法とテスト方法を説明します。

### ダッシュボードでのメッセージ変換設定

ダッシュボードでメッセージ変換を作成・設定する手順は以下の通りです。

1. ダッシュボードの左ナビゲーションメニューで **Smart Data Hub** -> **Message Transform** をクリックします。

2. **Message Transform** ページ右上の **Create** をクリックします。

3. 「Create Message Transform」ページで以下を設定します：
   - **Name**：変換の名前を入力します。

   - **Message Source Topic**：変換対象のメッセージのトピックを設定します。複数のトピックやトピックフィルターを設定可能です。

   - **Note**（任意）：メモを入力します。

   - **Message Format Transformation**：
     - **Source Format**：変換パイプラインに入るメッセージのペイロードに適用するデコーダーを指定します。選択肢は以下の通りです：

       - `None`（デコードなし）
       - `JSON`
       - `Avro`
       - `Protobuf`
       - `Custom (External HTTP)`

       これらのデコーダーはバイナリの入力ペイロードを構造化マップに変換します。`Avro`、`Protobuf`、`Custom (External HTTP)`を選択する場合は、[スキーマレジストリ](./schema-registry.md)にて事前に作成済みである必要があります。

       複数の変換が連なるパイプラインでは、各ステップでのデコードは必須ではありません。例えば、変換`T1`で既にペイロードをデコード済みなら、後続の変換`T2`はデコードをスキップし、正しい形式のペイロードを利用できます。

     - **Target Format**：変換パイプラインの最後にメッセージペイロードをバイナリ値としてエンコードするエンコーダーを指定します。エンコーダーの選択肢は**Source Format**と同じです。

       パイプラインの最後の変換のみがバイナリエンコードを行えばよく、中間の変換はバイナリエンコードを扱う必要はありません。

   - **Message Properties Transformation**：

     - **Properties**：式によって生成された変換後の値を書き込む宛先を指定します。有効な宛先は`payload`、`topic`、`qos`、`retain`（対応するフラグを設定）、`user_property`（MQTTのUser-Property）です。`user_property`を使う場合は、必ず1つのキーを指定します（例：`user_property.my_custom_prop`）。`payload`はそのまま使いメッセージペイロード全体を上書きするか、ネストされたJSONオブジェクトのように特定のキーのパスを指定できます（例：`payload.x.y`）。
     - **Target Value**：設定したプロパティに書き込む値を定義します。この値は`qos`、`retain`、`topic`、`payload`、`payload.x.y`など他のフィールドからコピーするか、[variform式](../../guides/configuration/configuration.md#variform-expressions)で生成可能です。

   - **Transformation Failure Operation**：
     - **Action After Failure**：変換失敗時に実行するアクションを選択します：
       - **Drop Message**：パブリッシュ処理を終了しメッセージを破棄、QoS 1およびQoS 2メッセージにはPUBACKで特定の理由コードを返します。
       - **Disconnect and Drop Message**：メッセージを破棄し、パブリッシュしたクライアントを切断します。
       - **Ignore**：追加の処理は行いません。

   - **Output Logs**：変換失敗時にログを生成するか選択します。ログはデフォルトで有効です。

   - **Logs Level**：ログの出力レベルを設定します。デフォルトは`warning`です。

4. **Create** をクリックして設定を完了します。

作成前に **Preview** をクリックすると、変換のテストが可能です。新しいペインが開き、QoS、ペイロード、リテインフラグの有無、パブリッシャーのユーザー名やクライアントIDなどの入力欄が表示されます。必要な情報を入力後、**Execute Transformation** をクリックすると指定したコンテキストで変換を実行し、結果を確認できます。

作成後は、メッセージ変換ページの一覧に変換が表示され、デフォルトで有効化されています。必要に応じて無効化したり、**Actions** 列の **Settings** から設定を更新可能です。削除や順序変更は **More** から行えます。

### 設定ファイルでのメッセージ変換設定

Avro形式でエンコードされたメッセージを受信し、JSONにデコードした後、パブリッシュしたクライアントのクライアント属性から取得した`tenant`属性をトピックの先頭に付加してからルールエンジンで処理したい場合、以下の設定で実現できます：

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

この設定は、`mytransformation`という名前の変換を指定し：

- 指定したスキーマでAvro形式のペイロードをデコード
- ペイロードをJSON形式にエンコード
- クライアント属性の`tenant`と元のトピックを連結し、トピックを変更

という処理を行います。

詳細な設定については[設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)を参照してください。

### REST API

REST APIを使ったメッセージ変換の詳細は[EMQX Enterprise API](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)をご覧ください。

### デコード／エンコード用スキーマの作成

デコーダーおよびエンコーダースキーマの作成方法については、[スキーマレジストリ](./schema-registry)セクションを参照してください。

## 統計と指標

有効化すると、メッセージ変換の統計情報と指標がダッシュボードに表示されます。メッセージ変換ページで変換名をクリックすると以下を確認できます：

**統計情報**：

- **Total**：システム起動以降のトリガー総数
- **Success**：成功したデータ変換数
- **Failed**：失敗したデータ変換数

**レート指標**：

- 現在の変換速度
- 過去5分間の速度
- 過去の最大速度

統計はリセット可能で、Prometheusの`/prometheus/message_transformation`からも取得できます。
