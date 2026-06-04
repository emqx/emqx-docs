# スキーマレジストリの例 - Protobuf

このページでは、スキーマレジストリとルールエンジンがProtobuf形式のメッセージのエンコードおよびデコードをどのようにサポートするかを示します。

## デコードシナリオ

<<<<<<< HEAD
デバイスがProtobufでエンコードされたバイナリメッセージをパブリッシュし、ルールエンジンがこれをマッチさせて、`name`フィールドに対応するトピックに再パブリッシュする必要があります。トピックの形式は `person/${name}` です。
=======
デバイスがProtobufでエンコードされたバイナリメッセージをパブリッシュし、そのメッセージをルールエンジンでマッチングして、`name`フィールドに対応するトピックに再パブリッシュする必要があります。トピックの形式は `person/${name}` です。
>>>>>>> origin/release-6.1

例えば、`name`フィールドが「Shawn」のメッセージをトピック `person/Shawn` に再パブリッシュします。

### スキーマの作成

ルールエンジンがProtobufメッセージを正しくデコードまたはエンコードできるように、まずスキーマレジストリを使ってProtobufメッセージの構造を定義するスキーマを登録する必要があります。

<<<<<<< HEAD
1. ダッシュボードで、左側のナビゲーションメニューから **Smart Data Hub** -> **Schema Registry** を選択します。
=======
1. ダッシュボードの左側ナビゲーションメニューから **Smart Data Hub** -> **Schema Registry** を選択します。
>>>>>>> origin/release-6.1

2. **Internal Schema** タブの下で、**Create** をクリックします。

3. スキーマの **Name** を入力します。例：`protobuf_person`。この名前はエンコードおよびデコード関数で使用されます。

4. スキーマの **Type** を選択します：`Protobuf` を選びます。

5. **Creation Method** を選択します。以下の2つのオプションがあります。

   ::: tip

   このページの例では **Input** メソッドを使用しています。

   :::

<<<<<<< HEAD
   - **Input**（単純なスキーマの場合）：
=======
   - **Input**（単純なスキーマの場合）:
>>>>>>> origin/release-6.1

     - 作成方法として **Input** を選択します。

     - Protobuf定義を直接 **Schema** フィールドに貼り付けます。例：

       ```protobuf
       message Person {
         required string name = 1;
         required int32 id = 2;
         optional string email = 3;
       }
       ```

<<<<<<< HEAD
   - **Upload Protobuf Bundle**（複雑または複数ファイルのスキーマの場合）：
=======
   - **Upload Protobuf Bundle**（複雑または複数ファイルのスキーマの場合）:
>>>>>>> origin/release-6.1

     - 作成方法として **Upload Protobuf Bundle** を選択します。

     - `.proto` ファイルを含む `.tar.gz` バンドルをアップロードするために **Select file** をクリックします。

     - **Root Proto File** にエントリポイントとなるファイル名（例：`person.proto`）を指定します。このファイルはバンドルのルートに存在する必要があります。

6. **Create** をクリックしてスキーマを登録します。

### ルールの作成

1. ダッシュボードで、ナビゲーションメニューから **Integration** -> **Rules** を選択します。

<<<<<<< HEAD
2. **Rules** ページで、右上の **Create** をクリックします。

3. 先ほど作成したスキーマを使って、以下のようにルールのSQL文を記述します：
=======
2. **Rules** ページで右上の **Create** をクリックします。

3. 先ほど作成したスキーマを使って、以下のようにルールのSQL文を記述します。
>>>>>>> origin/release-6.1

   ```sql
   SELECT
     schema_decode('protobuf_person', payload, 'Person') as person, payload
   FROM
     "t/#"
   WHERE
     person.name = 'Shawn'
   ```

   ここでのポイントは `schema_decode('protobuf_person', payload, 'Person')` です：

<<<<<<< HEAD
   - `schema_decode` 関数は、`protobuf_person` スキーマに従ってペイロードの内容をデコードします。
   - `as person` はデコードした値を変数 `person` に格納します。
   - 最後の引数 `Person` は、ペイロード内のメッセージタイプがProtobufスキーマで定義された `Person` 型であることを指定します。
=======
   - `schema_decode` 関数は `protobuf_person` スキーマに従ってペイロードの内容をデコードします。
   - `as person` はデコードした値を変数 `person` に格納します。
   - 最後の引数 `Person` は、ペイロード内のメッセージタイプがProtobufスキーマで定義された `Person` 型であることを示します。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンから `Republish` を選択します。
>>>>>>> origin/release-6.1

5. **Topic** フィールドに `person/${person.name}` と入力し、送信先トピックを指定します。

<<<<<<< HEAD
5. **Topic** フィールドに、送信先トピックとして `person/${person.name}` と入力します。

6. **Payload** フィールドに、メッセージコンテンツのテンプレートとして `${person}` と入力します。
=======
6. **Payload** フィールドにメッセージコンテンツテンプレートとして `${person}` と入力します。
>>>>>>> origin/release-6.1

このアクションにより、デコードされた "person" メッセージがJSON形式でトピック `person/${person.name}` に送信されます。`${person.name}` は変数プレースホルダーで、実行時にデコードされたメッセージの `name` フィールドの値に置き換えられます。

### デバイス側コードの準備

ルールが作成されたら、テスト用にデータをシミュレートできます。

<<<<<<< HEAD
以下のコードはPython言語を使用してユーザーメッセージを作成し、バイナリデータとしてエンコードしてからトピック `t/1` に送信します。詳細は[フルコード](https://gist.github.com/thalesmg/3c5fdbae2843d63c2380886e69d6123c)を参照してください。
=======
以下のコードはPython言語を使い、ユーザーメッセージを作成してバイナリデータにエンコードし、`t/1` トピックに送信します。詳細は[フルコード](https://gist.github.com/thalesmg/3c5fdbae2843d63c2380886e69d6123c)を参照してください。
>>>>>>> origin/release-6.1

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

<<<<<<< HEAD
1) ダッシュボードで、**Diagnose** -> **WebSocket Client** を選択します。

2) 現在のEMQXインスタンスへの接続情報を入力します。
   - EMQXをローカルで実行している場合は、デフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要です。

3. **Connect** をクリックして、EMQXインスタンスにMQTTクライアントとして接続します。

4. **Subscription** エリアの **Topic** フィールドに `person/#` と入力し、**Subscribe** をクリックします。

5. Pythonの依存関係をインストールし、デバイス側コードを実行します：
=======
1) ダッシュボードの **Diagnose** -> **WebSocket Client** を選択します。

2) 現在のEMQXインスタンスの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要になることがあります。

3. **Connect** をクリックしてEMQXインスタンスにMQTTクライアントとして接続します。

4. **Subscription** エリアの **Topic** フィールドに `person/#` と入力し、**Subscribe** をクリックします。

5. Pythonの依存関係をインストールし、デバイス側コードを実行します。
>>>>>>> origin/release-6.1

   ```shell
   $ pip3 install protobuf paho-mqtt
   $ protoc --python_out=. person.proto

   $ python3 protobuf_mqtt.py
   Connected with result code 0
   publish to topic: t/1, payload: b'\n\x05Shawn\x10\x01\x1a\x11shawn@example.com'
   ```

<<<<<<< HEAD
6. WebSocket側でトピック `person/Shawn` のメッセージが受信されていることを確認します：
=======
6. WebSocket側でトピック `person/Shawn` のメッセージが受信されていることを確認します。
>>>>>>> origin/release-6.1

   ```json
   {"name":"Shawn","id":1,"email":"shawn@example.com"}
   ```

## エンコードシナリオ

デバイスが `protobuf_out` トピックをサブスクライブし、Protobufでエンコードされたバイナリメッセージを受信することを期待しています。ルールエンジンを使ってそのようなメッセージをエンコードし、関連するトピックにパブリッシュします。

### スキーマの作成

[デコードシナリオ](#デコードシナリオ)で説明したのと同じスキーマを使用します。

### ルールの作成

1. ダッシュボードで、ナビゲーションメニューから **Integration** -> **Rules** を選択します。

<<<<<<< HEAD
2. **Rules** ページで、右上の **Create** をクリックします。

3. 先ほど作成したスキーマを使って、以下のようにルールのSQL文を記述します：
=======
2. **Rules** ページで右上の **Create** をクリックします。

3. 先ほど作成したスキーマを使って、以下のようにルールのSQL文を記述します。
>>>>>>> origin/release-6.1

   ```sql
   SELECT
     schema_encode('protobuf_person', json_decode(payload), 'Person') as protobuf_person
   FROM
     "protobuf_in"
   ```

   ここでのポイントは `schema_encode('protobuf_person', json_decode(payload), 'Person')` です：

<<<<<<< HEAD
   - `schema_encode` 関数は、`protobuf_person` スキーマに従ってペイロードの内容をエンコードします。
   - `as protobuf_person` はエンコードした値を変数 `protobuf_person` に格納します。
   - 最後の引数 `Person` は、ペイロード内のメッセージタイプがProtobufスキーマで定義された `Person` 型であることを指定します。
   - `json_decode(payload)` は、ペイロードが一般的にJSONエンコードされたバイナリであるため、`schema_encode` の入力にMap型を渡すために必要です。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに、送信先トピックとして `protobuf_out` と入力します。

6. **Payload** フィールドに、メッセージコンテンツのテンプレートとして `${protobuf_person}` と入力します。
=======
   - `schema_encode` 関数は `protobuf_person` スキーマに従ってペイロードの内容をエンコードします。
   - `as protobuf_person` はエンコードした値を変数 `protobuf_person` に格納します。
   - 最後の引数 `Person` は、ペイロード内のメッセージタイプがProtobufスキーマで定義された `Person` 型であることを示します。
   - `json_decode(payload)` は、ペイロードが一般的にJSONエンコードされたバイナリであるため、`schema_encode` の入力としてMap型が必要なため使用します。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンから `Republish` を選択します。

5. **Topic** フィールドに `protobuf_out` と入力し、送信先トピックを指定します。

6. **Payload** フィールドにメッセージコンテンツテンプレートとして `${protobuf_person}` と入力します。
>>>>>>> origin/release-6.1

このアクションにより、Protobufでエンコードされたユーザーメッセージがトピック `protobuf_out` に送信されます。`${protobuf_person}` は変数プレースホルダーで、実行時に `schema_encode` の結果（バイナリ値）に置き換えられます。

### デバイス側コードの準備

ルールが作成されたら、テスト用にデータをシミュレートできます。

<<<<<<< HEAD
以下のコードはPython言語を使用してユーザーメッセージを作成し、バイナリデータとしてエンコードしてからトピック `protobuf_in` に送信します。詳細は[フルコード](https://gist.github.com/thalesmg/c5f03f99f982401d16ef6583e30144fa)を参照してください。
=======
以下のコードはPython言語を使い、ユーザーメッセージを作成してバイナリデータをデコードし、受信したメッセージを表示します。詳細は[フルコード](https://gist.github.com/thalesmg/c5f03f99f982401d16ef6583e30144fa)を参照してください。
>>>>>>> origin/release-6.1

```python
def on_message(client, userdata, msg):
    print("msg payload", msg.payload)
    p = person_pb2.Person()
    p.ParseFromString(msg.payload)
    print(msg.topic+" "+str(p))
```

### ルール実行結果の確認

<<<<<<< HEAD
1) ダッシュボードで、**Diagnose** -> **WebSocket Client** を選択します。

2) 現在のEMQXインスタンスへの接続情報を入力します。
   - EMQXをローカルで実行している場合は、デフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要です。

3. **Connect** をクリックして、EMQXインスタンスにMQTTクライアントとして接続します。
=======
1) ダッシュボードの **Diagnose** -> **WebSocket Client** を選択します。

2) 現在のEMQXインスタンスの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要になることがあります。

3. **Connect** をクリックしてEMQXインスタンスにMQTTクライアントとして接続します。
>>>>>>> origin/release-6.1

4. **Publish** エリアの **Topic** フィールドに `protobuf_in` と入力し、**Payload** フィールドに以下のメッセージを入力します。

   ```json
   {"name":"Shawn","id":1,"email":"shawn@example.com"}
   ```

5. **Publish** をクリックします。

<<<<<<< HEAD
6. Pythonの依存関係をインストールし、デバイス側コードを実行します：
=======
6. Pythonの依存関係をインストールし、デバイス側コードを実行します。
>>>>>>> origin/release-6.1

   ```shell
   $ pip3 install protobuf paho-mqtt
   
   $ python3 protobuf_mqtt_sub.py
   Connected with result code 0
   msg payload b'\n\x05Shawn\x10\x01\x1a\x11shawn@example.com'
   protobuf_out name: "Shawn"
   id: 1
   email: "shawn@example.com"
   ```
