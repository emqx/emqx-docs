# スキーマレジストリの例 - Protobuf

このページでは、スキーマレジストリとルールエンジンがProtobuf形式のメッセージのエンコードおよびデコードをどのようにサポートするかを示します。

## デコードシナリオ

デバイスがProtobufでエンコードされたバイナリメッセージをパブリッシュし、そのメッセージをルールエンジンでマッチさせて、`name`フィールドに対応するトピックに再パブリッシュする必要があります。トピックの形式は `person/${name}` です。

例えば、`name`フィールドが "Shawn" のメッセージをトピック `person/Shawn` に再パブリッシュします。

### スキーマの作成

ルールエンジンがProtobufメッセージを正しくデコードまたはエンコードできるように、まずスキーマレジストリを使ってProtobufメッセージの構造を定義するスキーマを登録する必要があります。

1. ダッシュボードの左ナビゲーションメニューから **Smart Data Hub** -> **Schema Registry** を選択します。

2. **Internal Schema** タブの下で、**Create** をクリックします。

3. スキーマの **Name** を入力します。例：`protobuf_person`。この名前はエンコード・デコード関数で使用されます。

4. スキーマの **Type** を選択します。`Protobuf` を選択してください。

5. **Creation Method** を選択します。選択肢は2つあります：

   ::: tip

   このページの例では **Input** メソッドを使用しています。

   :::

   - **Input**（シンプルなスキーマ向け）:

     - 作成方法に **Input** を選択します。

     - Protobuf定義を直接 **Schema** フィールドに貼り付けます。例：

       ```protobuf
       message Person {
         required string name = 1;
         required int32 id = 2;
         optional string email = 3;
       }
       ```

   - **Upload Protobuf Bundle**（複雑または複数ファイルのスキーマ向け）:

     - 作成方法に **Upload Protobuf Bundle** を選択します。

     - `.proto` ファイルを含む `.tar.gz` バンドルをアップロードするために **Select file** をクリックします。

     - **Root Proto File** にエントリポイントファイル名（例：`person.proto`）を指定します。このファイルはバンドルのルートに存在する必要があります。

6. **Create** をクリックしてスキーマを登録します。

### ルールの作成

1. ダッシュボードのナビゲーションメニューから **Integration** -> **Rules** を選択します。

2. **Rules** ページの右上の **Create** をクリックします。

3. 先ほど作成したスキーマを使って、以下のようなルールSQL文を記述します：

   ```sql
   SELECT
     schema_decode('protobuf_person', payload, 'Person') as person, payload
   FROM
     "t/#"
   WHERE
     person.name = 'Shawn'
   ```

   ポイントは `schema_decode('protobuf_person', payload, 'Person')` の部分です：

   - `schema_decode` 関数は、`protobuf_person` スキーマに従ってペイロードの内容をデコードします。

   - `as person` はデコードした値を変数 `person` に格納します。

   - 最後の引数 `Person` は、ペイロード内のメッセージタイプがProtobufスキーマで定義された `Person` 型であることを指定します。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに、送信先トピックとして `person/${person.name}` と入力します。

6. **Payload** フィールドに、メッセージ内容のテンプレートとして `${person}` と入力します。

このアクションにより、デコードされた "person" メッセージがJSON形式でトピック `person/${person.name}` に送信されます。`${person.name}` は変数プレースホルダーで、実行時にデコードされたメッセージの `name` フィールドの値に置き換えられます。

### デバイス側コードの準備

ルールが作成されたら、テスト用にデータをシミュレートできます。

以下のコードはPython言語を使い、ユーザーメッセージを作成してバイナリデータとしてエンコードし、`t/1` トピックに送信します。詳細は[フルコード](https://gist.github.com/thalesmg/3c5fdbae2843d63c2380886e69d6123c)を参照してください。

```python
def publish_msg(client):
    p = person_pb2.Person()
    p.id = 1
    p.name = "Shawn"
    p.email = "shawn@example.com"
    message = p.SerializeToString()
    topic = "t/1"
    print("publish to topic: t/1, payload:", message)
    client.publish(topic, payload=message, qos=0, retain=False)
```

### ルール実行結果の確認

1) ダッシュボードの **Diagnose** -> **WebSocket Client** を選択します。

2) 現在のEMQXインスタンスの接続情報を入力します。

   - EMQXをローカルで実行している場合は、デフォルト値を使用できます。

   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードを入力する必要があります。

3. **Connect** をクリックしてEMQXインスタンスにMQTTクライアントとして接続します。

4. **Subscription** エリアの **Topic** フィールドに `person/#` と入力し、**Subscribe** をクリックします。

5. Pythonの依存パッケージをインストールし、デバイス側コードを実行します：

   ```shell
   $ pip3 install protobuf paho-mqtt
   $ protoc --python_out=. person.proto

   $ python3 protobuf_mqtt.py
   Connected with result code 0
   publish to topic: t/1, payload: b'\n\x05Shawn\x10\x01\x1a\x11shawn@example.com'
   ```

6. WebSocket側でトピック `person/Shawn` のメッセージを受信していることを確認します：

   ```json
   {"name":"Shawn","id":1,"email":"shawn@example.com"}
   ```

## エンコードシナリオ

デバイスがトピック `protobuf_out` をサブスクライブし、Protobufでエンコードされたバイナリメッセージを受信することを期待しています。ルールエンジンを使ってそのようなメッセージをエンコードし、関連トピックにパブリッシュします。

### スキーマの作成

[デコードシナリオ](#デコードシナリオ)で説明したのと同じスキーマを使用します。

### ルールの作成

1. ダッシュボードのナビゲーションメニューから **Integration** -> **Rules** を選択します。

2. **Rules** ページの右上の **Create** をクリックします。

3. 先ほど作成したスキーマを使って、以下のルールSQL文を記述します：

   ```sql
   SELECT
     schema_encode('protobuf_person', json_decode(payload), 'Person') as protobuf_person
   FROM
     "protobuf_in"
   ```

   ポイントは `schema_encode('protobuf_person', json_decode(payload), 'Person')` の部分です：

   - `schema_encode` 関数は、`protobuf_person` スキーマに従ってペイロードの内容をエンコードします。

   - `as protobuf_person` はエンコードした値を変数 `protobuf_person` に格納します。

   - 最後の引数 `Person` は、ペイロード内のメッセージタイプがProtobufスキーマで定義された `Person` 型であることを指定します。

   - `json_decode(payload)` は、`payload` が一般的にJSONエンコードされたバイナリであるため、`schema_encode` の入力としてMap型が必要なため使用します。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに送信先トピックとして `protobuf_out` と入力します。

6. **Payload** フィールドにメッセージ内容のテンプレートとして `${protobuf_person}` と入力します。

このアクションにより、Protobufでエンコードされたユーザーメッセージがトピック `protobuf_out` に送信されます。`${protobuf_person}` は変数プレースホルダーで、実行時に `schema_encode` の結果（バイナリ値）に置き換えられます。

### デバイス側コードの準備

ルールが作成されたら、テスト用にデータをシミュレートできます。

以下のコードはPython言語を使い、ユーザーメッセージを作成してバイナリデータとしてエンコードし、`protobuf_in` トピックに送信します。詳細は[フルコード](https://gist.github.com/thalesmg/c5f03f99f982401d16ef6583e30144fa)を参照してください。

```python
def on_message(client, userdata, msg):
    print("msg payload", msg.payload)
    p = person_pb2.Person()
    p.ParseFromString(msg.payload)
    print(msg.topic+" "+str(p))
```

### ルール実行結果の確認

1) ダッシュボードの **Diagnose** -> **WebSocket Client** を選択します。

2) 現在のEMQXインスタンスの接続情報を入力します。

   - EMQXをローカルで実行している場合は、デフォルト値を使用できます。

   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードを入力する必要があります。

3. **Connect** をクリックしてEMQXインスタンスにMQTTクライアントとして接続します。

4. **Publish** エリアの **Topic** フィールドに `protobuf_in` と入力し、**Payload** フィールドに以下のメッセージを入力します：

   ```json
   {"name":"Shawn","id":1,"email":"shawn@example.com"}
   ```

5. **Publish** をクリックします。

6. Pythonの依存パッケージをインストールし、デバイス側コードを実行します：

   ```shell
   $ pip3 install protobuf paho-mqtt
   
   $ python3 protobuf_mqtt_sub.py
   Connected with result code 0
   msg payload b'\n\x05Shawn\x10\x01\x1a\x11shawn@example.com'
   protobuf_out name: "Shawn"
   id: 1
   email: "shawn@example.com"
   ```
