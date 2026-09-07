# スキーマレジストリの例 - 外部HTTPサーバー

このページでは、スキーマレジストリとルールエンジンが、カスタムロジックを持つ外部HTTPサーバーを用いたメッセージのエンコードおよびデコードをどのようにサポートするかを示します。

場合によっては、EMQXがネイティブにサポートしていないカスタムのエンコードやデコードロジックを適用する必要があります。EMQXは、ルール内の `schema_encode` および `schema_decode` 関数を通じて外部HTTPサービスを呼び出すことで、この処理を外部に委譲することが可能です。

## 外部HTTP API仕様

外部HTTPサーバーは、EMQXの `schema_encode` および `schema_decode` 関数からのエンコードおよびデコード要求を受け取る単一のエンドポイントを公開する必要があります。EMQXはこのエンドポイントを `POST`（デフォルト）または `GET` メソッドで呼び出すことができます。

### リクエスト形式

#### POSTリクエスト

リクエストボディは以下のフィールドを持つJSONオブジェクトです：

- `payload`：ルールエンジンの `schema_encode` または `schema_decode` 関数に渡されたBase64エンコード済みの文字列値。
- `type`：評価されている関数に応じて、`encode` または `decode` の文字列。
- `schema_name`：EMQXで設定されたこの外部HTTPスキーマの名前を識別する文字列。
- `opts`：EMQXで設定可能な任意の文字列で、追加オプションとしてHTTPサーバーにそのまま渡されます。

#### GETリクエスト

スキーマのメソッドが `GET` に設定されている場合、EMQXは同じフィールドをURLクエリパラメータとして送信します：

- `payload`：パディングなしのURLセーフBase64エンコード済み文字列。
- `type`：`encode` または `decode` の文字列。
- `schema_name`：EMQXで設定されたこの外部HTTPスキーマの名前を識別する文字列。
- `opts`：変更なしでそのまま渡される任意の文字列。

スキーマURLにすでにクエリパラメータが含まれている場合、EMQXはこれら4つのパラメータを既存のクエリ文字列に追加します。

### レスポンス形式

- サーバーはHTTPステータスコード `200` で応答する必要があります。
- レスポンスボディは結果を表すBase64エンコード済みの文字列を含む必要があります。このBase64値はEMQXに返す際にさらにJSONエンコードしてはいけません。

## スキーマ設定リファレンス

ダッシュボードで外部HTTPスキーマを作成する際、以下のフィールドが利用可能です：

| フィールド       | 必須     | 説明                                                                                                                                                           |
| -------------- | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Name**       | はい     | EMQX内でスキーマを一意に識別する名前。                                                                                                                        |
| **Type**       | はい     | `External HTTP` に設定します。                                                                                                                                |
| **URL**        | はい     | 外部HTTPサーバーのエンドポイントの完全なURL。例：`http://server:9500/serde`。                                                                                  |
| **Method**     | はい     | エンドポイントを呼び出すHTTPメソッド。デフォルトは `POST`。外部サービスがクエリ文字列でリクエストフィールドを受け取る場合のみ `GET` を使用してください。      |
| **Params**     | いいえ   | すべてのリクエストで `opts` フィールドとして渡される任意の文字列。サービスに追加オプションや設定値を送るために使用します。                                      |
| **Headers**    | いいえ   | すべてのリクエストに含めるHTTPヘッダー。`content-type: application/json` はデフォルトで追加されます。追加のヘッダー（認証トークンなど）を含める場合は **Add** をクリックしてください。 |
| **Enable TLS** | いいえ   | 外部HTTPサーバーがTLS接続を必要とする場合にオンにします。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。 |

## 利用例

デバイスがバイナリメッセージをパブリッシュし、そのペイロードをカスタムのXOR演算でエンコードまたはデコードしたいとします。このセクションでは、単純な外部HTTPサービスを構築し、カスタムのエンコード・デコードロジックをEMQXに統合する方法を示します。

### 外部HTTPサービスの構築

以下の例は、PythonとFlaskを使ってシンプルなHTTPサーバーを作成・実行する方法を示します。サーバーは `POST` または `GET` リクエストを受け付け、受信したペイロードをデコードしてXOR演算を適用します。

<details>
<summary><strong>外部HTTPサーバーのサンプルコード</strong></summary>

[Flask](https://flask.palletsprojects.com/en/stable/) がインストールされていることを確認してください：

```sh
pip install Flask==3.1.0
```

サンプルコード：

```python
from flask import Flask, request
import base64

app = Flask(__name__)


def decode_payload(payload64):
    if request.method == "GET":
        # EMQXはGETペイロードをパディングなしのURLセーフBase64で送信します。
        payload64 += "=" * (-len(payload64) % 4)
        return base64.urlsafe_b64decode(payload64)
    return base64.b64decode(payload64)


@app.route("/serde", methods=["POST", "GET"])
def serde():
    # POSTはJSONボディ、GETはクエリパラメータを使用します。
    body = request.args if request.method == "GET" else request.get_json(force=True)
    print("incoming request:", body)
    payload64 = body.get("payload")
    payload = decode_payload(payload64)
    secret = 122
    response = bytes(b ^ secret for b in payload)
    # レスポンスもBase64エンコードする必要があります
    response64 = base64.b64encode(response)
    return response64
```

サーバーの起動方法：

```sh
# サーバーが同じディレクトリの `myapp.py` というファイルにあると仮定しています
flask --app myapp --debug run -h 0.0.0.0 -p 9500
```

</details>

### EMQXで外部HTTPスキーマを作成する

1. ダッシュボードの左側ナビゲーションメニューから **Smart Data Hub** -> **Schema Registry** を選択します。

2. **Internal** タブページで **Create** をクリックします。

3. 以下のパラメータで外部HTTPサーバースキーマを作成します：
   - **Name**：`myhttp`
   - **Type**：`External HTTP`
   - **URL**：サーバーが稼働している完全なURI。例：`http://server:9500/serde`
   - **Method**：`POST` または `GET` を選択。デフォルトは `POST`。外部サービスがクエリ文字列でリクエストフィールドを受け取る場合のみ `GET` を使用してください。

4. **Create** をクリックします。

### スキーマを適用するルールを作成する

EMQXのルールエンジンを使って、メッセージのエンコードおよびデコードにスキーマを適用するルールを作成します。

1. ダッシュボードのナビゲーションメニューから **Integration** -> **Rules** を選択します。

2. **Rules** ページで右上の **Create** をクリックします。

3. 作成したスキーマを使って以下のSQL文を記述します：

   ```sql
   SELECT
     schema_encode('myhttp', payload) as encoded,
     schema_decode('myhttp', encoded) as decoded
   FROM
     "t/external_http"
   ```

   `schema_encode('myhttp', payload)` と `schema_decode('myhttp', encoded)` の両方が、設定した外部HTTPサーバーを呼び出してペイロードのエンコード／デコードを行います。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに送信先トピックとして `external_http/out` と入力します。

6. **Payload** フィールドにメッセージコンテンツテンプレートとして `${.}` と入力します。

7. **Add** をクリックしてアクションをルールに追加します。

   このアクションは、デコードされたメッセージをJSON形式でトピック `external_http/out` に送信します。`${.}` はルールの出力全体の値に実行時に置き換えられる変数プレースホルダーです。

8. **Save** をクリックしてルールの作成を完了します。

### ルール実行結果の確認

1. ダッシュボードのナビゲーションメニューから **Diagnose** -> **WebSocket Client** を選択します。

2. 現在のEMQXインスタンスの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要になることがあります。

3. **Connect** をクリックしてEMQXインスタンスにMQTTクライアントとして接続します。

4. **Subscription** エリアの **Topic** フィールドに `external_http/out` と入力し、**Subscribe** をクリックします。

5. **Publish** エリアの **Topic** フィールドに `t/external_http` と入力し、任意のペイロードを記入して **Publish** をクリックします。

6. WebSocket側でトピック `external_http/out` のメッセージが受信されることを確認します。例えば、ペイロードが `hello` だった場合：

   ```json
   {"encoded":"\u0012\u001F\u0016\u0016\u0015","decoded":"hello"}
   ```
