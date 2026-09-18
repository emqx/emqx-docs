# メッセージ変換

メッセージ変換は、ユーザー定義のルールに基づいてメッセージを変更およびフォーマットし、その後の処理やサブスクライバーへの配信前に適用する機能です。この機能は高度にカスタマイズ可能で、複数のエンコーディングや高度な変換をサポートしています。

## ワークフロー

メッセージがパブリッシュされると、以下のワークフローを経ます。

1. **スキーマ検証**：メッセージがパブリッシュされ認可を通過すると、まず[スキーマ検証](./schema-validation.md)が行われます。メッセージが検証を通過すると次のステップに進みます。

2. **メッセージ変換パイプライン**：

   - **変換マッチング**：メッセージはトピックに基づいてユーザー定義の変換リストと照合されます。異なるトピックやトピックフィルターに対して複数の変換を設定可能です。
   - **変換実行**：マッチした変換は設定された順序で実行されます。パイプラインはJSON、Protobuf、Avroなどの各種エンコーダー・デコーダーをサポートし、[Variform式](../../guides/configuration/configuration.md#variform-expressions)を用いてメッセージの拡張や変更が可能です。
   - **変換後処理**：メッセージが変換パイプラインを正常に通過すると、ルールエンジンのトリガーやサブスクライバーへのメッセージ配信など次の処理に進みます。

3. **失敗時の処理**：変換が失敗した場合、ユーザー設定に応じたアクションが実行されます。

   - **メッセージ破棄**：パブリッシュを終了しメッセージを破棄します。QoS 1およびQoS 2メッセージにはPUBACKで特定の理由コード（131 - 実装固有エラー）が返されます。
   - **切断してメッセージ破棄**：メッセージを破棄し、パブリッシュしたクライアントを切断します。
   - **無視**：追加の処理は行いません。

   変換失敗時には設定に関わらずログが生成されることがあります。ログの出力レベルはユーザーが設定可能で、デフォルトは`warning`です。さらに、変換失敗はルールエンジンのイベント（`$events/message_transformation/failed`）をトリガーでき、誤ったメッセージを別トピックに再パブリッシュしたり、Kafkaへ送信して詳細解析を行うなどのカスタム処理が可能です。

## ユーザーガイド

このセクションでは、メッセージ変換機能の設定方法とテスト方法を説明します。

### ダッシュボードでのメッセージ変換設定

ダッシュボードでメッセージ変換を作成・設定する手順を示します。

1. ダッシュボードにアクセスし、左側のナビゲーションメニューから **Smart Data Hub** -> **Message Transform** をクリックします。

2. **Message Transform** ページ右上の **Create** をクリックします。

3. 「Create Message Transform」ページで以下の情報を設定します。

   - **Name**：変換の名前を入力します。

   - **Message Source Topic**：変換対象とするメッセージのトピックを設定します。複数のトピックやトピックフィルターを設定可能です。

   - **Note**（任意）：メモを入力します。

   - **Message Format Transformation**：
     - **Source Format**：変換パイプラインに入るメッセージのペイロードデコーダーを指定します。選択肢は以下の通りです。

       - `None`（デコードなし）
       - `JSON`
       - `Avro`
       - `Protobuf`
       - `Custom (External HTTP)`

       これらのデコーダーはバイナリの入力ペイロードを構造化マップに変換します。`Avro`、`Protobuf`、`Custom (External HTTP)`を選択する場合は、あらかじめ[スキーマレジストリ](./schema-registry.md)に登録されている必要があります。

       複数の変換が連なるパイプラインでは、各ステップで必ずしもデコードが必要なわけではありません。例えば、変換`T1`で既にペイロードがデコードされていれば、続く変換`T2`はデコードをスキップしても構いません。

     - **Target Format**：変換パイプラインの最後にメッセージペイロードをバイナリ値としてエンコードするためのエンコーダーを指定します。選択肢は**Source Format**と同じです。

       パイプラインの最後の変換のみがバイナリエンコードを行えばよく、中間の変換はバイナリエンコードを行う必要はありません。

   - **Message Properties Transformation**：

     - **Properties**：式の結果として生成された変換後の値を書き込む先を指定します。指定可能な宛先は`payload`、`topic`、`qos`、`retain`（対応するフラグを設定）、および`user_property`（MQTTのUser-Property）です。`user_property`を使用する場合は、必ず1つのキーを指定してください（例：`user_property.my_custom_prop`）。`payload`はそのままメッセージペイロード全体を上書きするか、ネストされたJSONオブジェクトの特定のキー（例：`payload.x.y`）として扱うことが可能です。

     - **Target Value**：設定したプロパティに書き込む値を定義します。この値は`qos`、`retain`、`topic`、`payload`、`payload.x.y`などの他のフィールドからコピーするか、[variform式](../../guides/configuration/configuration.md#variform-expressions)で生成することができます。

   - **Transformation Failure Operation**：
     - **Action After Failure**：変換失敗時に実行するアクションを選択します。
       - **Drop Message**：パブリッシュ処理を終了しメッセージを破棄、QoS 1およびQoS 2メッセージにはPUBACKで特定の理由コードを返します。
       - **Disconnect and Drop Message**：メッセージを破棄し、パブリッシュしたクライアントを切断します。
       - **Ignore**：追加の処理は行いません。

   - **Output Logs**：変換失敗時にログを生成するかを選択します。デフォルトでログは有効です。

   - **Logs Level**：ログの出力レベルを設定します。デフォルトは`warning`です。

4. 設定が完了したら **Create** をクリックします。

作成前に変換をテストしたい場合は、**Preview** をクリックしてください。新しいペインが開き、QoS、ペイロード、retainフラグの有無、パブリッシャーのユーザー名やクライアントIDなど、受信メッセージのコンテキストを入力できます。必要な情報を入力後、**Execute Transformation** をクリックすると指定したコンテキストで変換が実行され、結果の出力を確認できます。

変換が作成されると、Message Transformationページの一覧にデフォルトで有効状態で表示されます。必要に応じて無効化したり、**Actions**列の**Settings**から設定を更新できます。削除や順序変更は**More**から行えます。

### 設定ファイルでのメッセージ変換設定

Avro形式でエンコードされたメッセージを受信し、JSONにデコードしたいとします。デコード後、パブリッシュしたクライアントのクライアント属性から取得した`tenant`属性をトピックの先頭に付加してからルールエンジンで処理したい場合、以下の設定で実現可能です。

```hocon
message_transformation {
  transformations = [
    {
      name = mytransformation
      topics = ["t"]
      failure_action = drop
      payload_decoder {
        type = avro
        schema = myschema
      }
      payload_encoder {
        type = json
      }
      operations = [
        {key = "topic", value = "concat([client_attrs.tenant, '/', topic])"}
      ]
    }
  ]
}
```

この設定は、`mytransformation`という名前の変換を指定し、

- 指定したスキーマを使ってAvro形式のペイロードをデコードし、
- ペイロードをJSON形式にエンコードし、
- クライアント属性の`tenant`と元のトピックを連結してトピックを変更しています。

詳細な設定方法は[Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご参照ください。

### REST API

REST APIを使ったメッセージ変換の詳細は[EMQX Enterprise API](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html)をご覧ください。

### デコード／エンコード用スキーマの作成

デコーダーおよびエンコーダースキーマの作成方法については、[スキーマレジストリ](./schema-registry)のセクションをご参照ください。

## 統計と指標

メッセージ変換を有効にすると、ダッシュボード上で統計情報や指標を確認できます。Message Transformationページで変換名をクリックすると以下が表示されます。

**統計情報**：

- **Total**：システム起動以降のトリガー総数
- **Success**：成功したデータ変換の件数
- **Failed**：失敗したデータ変換の件数

**レート指標**：

- 現在の変換速度
- 過去5分間の速度
- 過去の最大速度

統計情報はリセット可能で、Prometheusの`/prometheus/message_transformation`からも取得可能です。
