# スキーマレジストリの例 - JSON Schema

このページでは、スキーマレジストリに draft 2020-12 JSON Schema を登録し、ルール内で `schema_check` 関数を使用して MQTT メッセージのペイロードを検証する方法を示します。この例では、スキーマに準拠したペイロードのみを再パブリッシュします。

## 対応している JSON Schema ドラフト

EMQX 6.0.4 以降、スキーマレジストリは以下の JSON Schema ドラフトをサポートしています。

- draft-03
- draft-04
- draft-06
- draft 2019-09
- draft 2020-12

EMQX は `$schema` フィールドの値に基づいて JSON Schema のバージョンを選択します。`$schema` が省略された場合は draft-06 を使用します。

draft 2019-09 および draft 2020-12 のサポートには以下の制限があります。

- draft 2019-09 は `$recursiveRef` をサポートしていません。
- draft 2020-12 は `$dynamicRef` をサポートしていません。
- これら2つのドラフトではリモートスキーマへの参照はサポートされていません。

サポートされていないキーワードがスキーマに含まれている場合、検証はエラーを返し、キーワードを無視することはありません。

## JSON Schema の作成

正確に2つの整数を含む配列を受け入れるスキーマを作成します。

1. EMQX ダッシュボードの左側ナビゲーションメニューで **Smart Data Hub** -> **Schema Registry** をクリックします。
2. **Internal** タブで **Create** をクリックします。
3. 以下の項目を設定します。

   - **Name**: `json_array` と入力します。
   - **Type**: **JSON Schema** を選択します。
   - **Schema**: 以下の draft 2020-12 スキーマを入力します。

     ```json
     {
       "$schema": "https://json-schema.org/draft/2020-12/schema",
       "type": "array",
       "prefixItems": [
         { "type": "integer" },
         { "$ref": "#/prefixItems/0" }
       ],
       "minItems": 2,
       "maxItems": 2
     }
     ```

4. **Create** をクリックします。

`prefixItems` 配列は各位置のスキーマを定義します。ローカル `$ref` により、2番目の要素も1番目の要素と同じ整数スキーマを使用します。`minItems` と `maxItems` はペイロードが正確に2つの要素を含むことを要求します。

## ルールの作成

ペイロードが `json_array` に準拠する場合にのみメッセージを再パブリッシュするルールを作成します。

1. ダッシュボードの左側ナビゲーションメニューで **Integration** -> **Rules** をクリックします。
2. **Rules** ページで **Create** をクリックします。
3. **Name** フィールドに `validate_json_array` と入力します。
4. **SQL Editor** に以下のステートメントを入力します。

   ```sql
   SELECT *
   FROM "t/json"
   WHERE schema_check('json_array', payload)
   ```

   `schema_check` 関数はペイロードが `json_array` に準拠する場合に `true` を返します。そうでない場合は `false` を返し、ルールはアクションを実行しません。

5. **Add Action** をクリックし、**Republish** を選択します。
6. **Topic** フィールドに `validated/json`、**Payload** フィールドに `${payload}` と入力します。
7. **Create** をクリックします。

## ルールのテスト

MQTTX CLI を使用してルールを検証します。

1. 再パブリッシュ先のトピックをサブスクライブします。

   ```bash
   mqttx sub -t validated/json
   ```

2. 別のターミナルでスキーマに準拠したペイロードをパブリッシュします。

   ```bash
   mqttx pub -t t/json -m '[1, 2]'
   ```

   サブスクライバーは `validated/json` トピックから `[1, 2]` を受信します。

3. 2番目の要素が整数でないペイロードをパブリッシュします。

   ```bash
   mqttx pub -t t/json -m '[1, "two"]'
   ```

   ルールは再パブリッシュアクションを実行せず、サブスクライバーはメッセージを受信しません。

ルール内での `schema_check` の使用はルールの実行をフィルタリングしますが、元の MQTT メッセージを拒否するわけではありません。非準拠メッセージを拒否または破棄するには、[スキーマ検証](./schema-validation.md) を使用してください。
