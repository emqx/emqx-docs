# スキーマレジストリの例 - Avro

このページでは、スキーマレジストリとルールエンジンがAvro形式のメッセージのエンコードおよびデコードをどのようにサポートするかを示します。

## デコードシナリオ

<<<<<<< HEAD
デバイスがAvroでエンコードされたバイナリメッセージをパブリッシュし、ルールエンジンがそれをマッチさせて、`name`フィールドに対応するトピックに再パブリッシュする必要があります。トピックの形式は `avro_user/${name}` です。
=======
デバイスがAvroでエンコードされたバイナリメッセージをパブリッシュし、ルールエンジンでマッチングした後、`name`フィールドに対応するトピックに再パブリッシュする必要があります。トピックの形式は`avro_user/${name}`です。
>>>>>>> origin/release-6.1

例えば、`name`フィールドが`Shawn`のメッセージをトピック`avro_user/Shawn`に再パブリッシュする必要があります。

### スキーマの作成

ルールエンジンがAvroメッセージを正しくデコードまたはエンコードできるようにするために、まずスキーマレジストリを使ってAvroメッセージの構造を定義するスキーマを登録する必要があります。

1. ダッシュボードの左ナビゲーションメニューから **Smart Data Hub** -> **Schema Registry** を選択します。

2. **Internal Schema** タブの下で、**Create** をクリックします。

3. 次のパラメータでAvroスキーマを作成します：

   - **Name**: `avro_user`。この名前はエンコード・デコード関数で使用されます。

   - **Type**: `Avro`

   - **Schema**:

     ```json
     {
       "type":"record",
       "name": "myrecord1",
       "fields":[
           {"name":"name", "type":"string"},
           {"name":"favorite_number", "type":["int", "null"]},
           {"name":"favorite_color", "type":["string", "null"]}
       ]
     }
     ```

4. **Create** をクリックします。

![](./assets/schema_registry/avro_create1.png)

### ルールの作成
1. ダッシュボードのナビゲーションメニューから **Integration** -> **Rules** を選択します。

2. **Rules** ページで、右上の **Create** をクリックします。

<<<<<<< HEAD
3. 先ほど作成したスキーマを使用して、以下のルールSQL文を記述します：
=======
3. 先ほど作成したスキーマを使って、以下のルールSQL文を書きます：
>>>>>>> origin/release-6.1

   ```sql
   SELECT
     schema_decode('avro_user', payload) as avro_user, payload
   FROM
     "t/#"
   WHERE
     avro_user.name = 'Shawn'
   ```

   ここでのポイントは `schema_decode('avro_user', payload)` です：

<<<<<<< HEAD
   - `schema_decode` 関数は、スキーマ `avro_user` に従ってペイロードフィールドの内容をデコードします。
   - `as avro_user` はデコードされた値を変数 `avro_user` に格納します。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに、送信先トピックとして `avro_user/${avro_user.name}` と入力します。

6. **Payload** フィールドにメッセージ内容のテンプレートとして `${avro_user}` と入力します。
=======
   - `schema_decode` 関数は、`avro_user`スキーマに従ってペイロードの内容をデコードします。
   - `as avro_user` はデコードした値を変数 `avro_user` に格納します。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに、宛先トピックとして `avro_user/${avro_user.name}` を入力します。

6. **Payload** フィールドに、メッセージ内容のテンプレートとして `${avro_user}` を入力します。
>>>>>>> origin/release-6.1

このアクションにより、デコードされたメッセージがJSON形式でトピック `avro_user/${avro_user.name}` に送信されます。`${avro_user.name}` は変数プレースホルダーであり、実行時にデコードされたメッセージの `name` フィールドの値に置き換えられます。

### デバイス側コードの準備

ルールが作成されたら、テスト用にデータをシミュレートできます。

以下のコードはPython言語を使用してユーザーメッセージを作成し、バイナリデータとしてエンコードしてから、トピック `t/1` に送信します。詳細は[フルコード](https://gist.github.com/thalesmg/bbda65b400f35f8ab0f719b06cf875f6)をご覧ください。

```python
def publish_msg(client):
    datum_w = avro.io.DatumWriter(SCHEMA)
    buf = io.BytesIO()
    encoder = avro.io.BinaryEncoder(buf)
    datum_w.write({"name": "Shawn", "favorite_number": 666, "favorite_color": "red"}, encoder)
    message = buf.getvalue()
    topic = "t/1"
    print("publish to topic: t/1, payload:", message)
    client.publish(topic, payload=message, qos=0, retain=False)
```

### ルール実行結果の確認
1) ダッシュボードで **Diagnose** -> **WebSocket Client** を選択します。

2) 現在のEMQXインスタンスの接続情報を入力します。
<<<<<<< HEAD

   - EMQXをローカルで実行している場合は、デフォルト値を使用できます。

   - 認証設定などでEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードを入力する必要があります。

3. **Connect** をクリックし、MQTTクライアントとしてEMQXインスタンスに接続します。
=======
   - ローカルでEMQXを実行している場合は、デフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要です。

3. **Connect** をクリックして、EMQXインスタンスにMQTTクライアントとして接続します。
>>>>>>> origin/release-6.1

4. **Subscription** エリアの **Topic** フィールドに `avro_user/#` と入力し、**Subscribe** をクリックします。

5. Pythonの依存パッケージをインストールし、デバイス側コードを実行します：

   ```shell
   $ pip3 install avro paho-mqtt

   $ python3 avro_mqtt.py
   Connected with result code 0
   publish to topic: t/1, payload: b'\nShawn\x00\xb4\n\x00\x06red'
   ```

6. WebSocket側でトピック `avro_user/Shawn` のメッセージが受信されていることを確認します：

   ```json
   {"favorite_color":"red","favorite_number":666,"name":"Shawn"}
   ```

## エンコードシナリオ

<<<<<<< HEAD
デバイスがトピック `avro_out` をサブスクライブし、Avroでエンコードされたバイナリメッセージを受信することを想定しています。ルールエンジンはそのようなメッセージをエンコードし、対応するトピックにパブリッシュします。
=======
デバイスがトピック `avro_out` をサブスクライブし、Avroでエンコードされたバイナリメッセージを受信することを想定しています。ルールエンジンはこのようなメッセージをエンコードし、対応するトピックにパブリッシュします。
>>>>>>> origin/release-6.1

### スキーマの作成

[デコードシナリオ](#デコードシナリオ)で説明したのと同じスキーマを使用します。

### ルールの作成

1. ダッシュボードのナビゲーションメニューから **Integration** -> **Rules** を選択します。

2. **Rules** ページで、右上の **Create** をクリックします。

<<<<<<< HEAD
3. 先ほど作成したスキーマを使用して、以下のルールSQL文を記述します：
=======
3. 先ほど作成したスキーマを使って、以下のルールSQL文を書きます：
>>>>>>> origin/release-6.1

   ```sql
   SELECT
     schema_encode('avro_user', json_decode(payload)) as avro_user
   FROM
     "avro_in"
   ```

   ここでのポイントは `schema_encode('avro_user', json_decode(payload))` です：

<<<<<<< HEAD
   - `schema_encode` 関数は、スキーマ `avro_user` に従ってペイロードフィールドの内容をエンコードします。
   - `as avro_user` はエンコードされた値を変数 `avro_user` に格納します。
   - `json_decode(payload)` は、`payload` が一般的にJSONエンコードされたバイナリであるため必要であり、`schema_encode` はMap型の入力を要求します。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに送信先トピックとして `avro_out` と入力します。

6. **Payload** フィールドにメッセージ内容のテンプレートとして `${avro_user}` と入力します。

このアクションにより、Avroでエンコードされたメッセージがトピック `avro_out` に送信されます。`${avro_user}` は変数プレースホルダーであり、`schema_encode` の結果（バイナリ値）に実行時に置き換えられます。
=======
   - `schema_encode` 関数は、`avro_user`スキーマに従ってペイロードの内容をエンコードします。
   - `as avro_user` はエンコードした値を変数 `avro_user` に格納します。
   - `json_decode(payload)` は、`payload` が一般的にJSONエンコードされたバイナリであり、`schema_encode` の入力にはMap型が必要なため使用します。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに、宛先トピックとして `avro_out` を入力します。

6. **Payload** フィールドに、メッセージ内容のテンプレートとして `${avro_user}` を入力します。

このアクションにより、Avroでエンコードされたメッセージがトピック `avro_out` に送信されます。`${avro_user}` は、`schema_encode` の結果（バイナリ値）に実行時に置き換えられる変数プレースホルダーです。
>>>>>>> origin/release-6.1

### デバイス側コードの準備

ルールが作成されたら、テスト用にデータをシミュレートできます。

<<<<<<< HEAD
以下のコードはPython言語を使用してユーザーメッセージを受信し、バイナリデータをデコードして表示します。詳細は[フルコード](https://gist.github.com/thalesmg/02046f89e9ceb70b9806dc98e6ed8b55)をご覧ください。
=======
以下のコードはPython言語を使用し、Userメッセージを受信してデコードし、内容を表示します。詳細は[フルコード](https://gist.github.com/thalesmg/02046f89e9ceb70b9806dc98e6ed8b55)を参照してください。
>>>>>>> origin/release-6.1

```python
def on_message(client, userdata, msg):
    datum_r = avro.io.DatumReader(SCHEMA)
    buf = io.BytesIO(msg.payload)
    decoder = avro.io.BinaryDecoder(buf)
    decoded_payload = datum_r.read(decoder)
    print(msg.topic+" "+str(decoded_payload))
```

### ルール実行結果の確認

1) ダッシュボードで **Diagnose** -> **WebSocket Client** を選択します。

2) 現在のEMQXインスタンスの接続情報を入力します。
<<<<<<< HEAD

   - EMQXをローカルで実行している場合は、デフォルト値を使用できます。

   - 認証設定などでEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードを入力する必要があります。

3. **Connect** をクリックし、MQTTクライアントとしてEMQXインスタンスに接続します。
=======
   - ローカルでEMQXを実行している場合は、デフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要です。

3. **Connect** をクリックして、EMQXインスタンスにMQTTクライアントとして接続します。
>>>>>>> origin/release-6.1

4. **Publish** エリアの **Topic** フィールドに `avro_in` と入力し、**Payload** フィールドに以下のメッセージを入力します：

   ```json
   {"favorite_color":"red","favorite_number":666,"name":"Shawn"}
   ```

5. **Publish** をクリックします。

6. Pythonの依存パッケージをインストールし、デバイス側コードを実行します：

   ```shell
   $ pip3 install avro paho-mqtt
   
   $ python3 avro_mqtt_sub.py
   Connected with result code 0
   msg payload b'\nShawn\x00\xb4\n\x00\x06red'
   avro_out {'name': 'Shawn', 'favorite_number': 666, 'favorite_color': 'red'}
   ```
