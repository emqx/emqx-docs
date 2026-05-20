# スキーマレジストリの例 - 外部HTTPサーバー

このページでは、スキーマレジストリとルールエンジンが、カスタムロジックを持つ外部HTTPサーバーを用いてメッセージのエンコードおよびデコードをサポートする方法を示します。

特定のシナリオでは、EMQXがネイティブにサポートしていないカスタムのエンコードやデコードロジックを適用する必要がある場合があります。EMQXでは、ルール内の `schema_encode` および `schema_decode` 関数を通じて外部HTTPサービスを呼び出し、この処理を委任することが可能です。

## 外部HTTP API仕様

外部HTTPサーバーは、EMQXの `schema_encode` および `schema_decode` 関数からのエンコード・デコード要求を受け取る単一のエンドポイントを公開する必要があります。EMQXはこのエンドポイントを `POST`（デフォルト）または `GET` メソッドで呼び出せます。

### リクエスト形式

#### POSTリクエスト

リクエストボディは以下のフィールドを持つJSONオブジェクトです：

- `payload`：ルールエンジンの `schema_encode` または `schema_decode` 関数に渡されたBase64エンコード済みの文字列値。
- `type`：評価される関数に応じて、`encode` または `decode` の文字列。
- `schema_name`：EMQXで設定されたこの外部HTTPスキーマの名前を識別する文字列。
- `opts`：EMQXで設定可能な任意の文字列で、追加オプションを提供し、HTTPサーバーにそのまま渡されます。

#### GETリクエスト

スキーマのメソッドが `GET` に設定されている場合、EMQXは同じフィールドをURLのクエリパラメータとして送信します：

- `payload`：パディングなしのURLセーフBase64エンコード済み文字列。
- `type`：`encode` または `decode` の文字列。
- `schema_name`：EMQXで設定されたこの外部HTTPスキーマの名前を識別する文字列。
- `opts`：変更されずに渡される任意の文字列。

スキーマURLにすでにクエリパラメータが含まれている場合、EMQXはこれら4つのパラメータを既存のクエリ文字列に追加します。

### レスポンス形式

- サーバーはHTTPステータスコード `200` で応答する必要があります。
- レスポンスボディには、結果を表すBase64エンコード済みの文字列を含める必要があります。なお、このBase64値はEMQXに返答する際にさらにJSONエンコードしてはいけません。

## スキーマ設定リファレンス

ダッシュボードで外部HTTPスキーマを作成する際、以下のフィールドが利用可能です：

| フィールド       | 必須      | 説明                                                                                                                                                         |
| --------------- | --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **Name**        | はい      | EMQX内でスキーマを一意に識別する名前。                                                                                                                      |
| **Type**        | はい      | `External HTTP` に設定します。                                                                                                                              |
| **URL**         | はい      | 外部HTTPサーバーのエンドポイントの完全なURL。例：`http://server:9500/serde`。                                                                               |
| **Method**      | はい      | エンドポイント呼び出しに使用するHTTPメソッド。デフォルトは `POST`。外部サービスがクエリ文字列でリクエストフィールドを受け取る場合のみ `GET` を使用します。 |
| **Params**      | いいえ    | すべてのリクエストの `opts` フィールドとして渡される任意の文字列。サービスへの追加オプションや設定値を送るのに使用します。                                  |
| **Headers**     | いいえ    | すべてのリクエストに含まれるHTTPヘッダー。`content-type: application/json` ヘッダーはデフォルトで追加されます。認証トークンなど追加ヘッダーを設定する場合は **Add** をクリックしてください。 |
| **Enable TLS**  | いいえ    | 外部HTTPサーバーがTLS接続を必要とする場合にオンにします。詳細は [TLS for External Resource Access](../network/overview.md#tls-for-external-resource-access) を参照してください。 |

## 利用例

デバイスがバイナリメッセージをパブリッシュし、そのペイロードをカスタムのXOR演算でエンコードまたはデコードしたい場合を想定します。このセクションでは、簡単な外部HTTPサービスを構築し、カスタムのエンコード・デコードロジックをEMQXに統合する方法を示します。

### 外部HTTPサービスの構築

以下の例は、PythonとFlaskを使ってシンプルなHTTPサーバーを作成・実行する方法を示します。このサーバーは `POST` または `GET` リクエストを受け付け、受信したペイロードをデコードしてXOR演算を適用します。

<details>
<summary><strong>サンプル外部HTTPサーバーのコード</strong></summary>

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

サーバーを起動するには：

```sh
# サーバーが同じディレクトリの `myapp.py` というファイル名で保存されていると仮定
flask --app myapp --debug run -h 0.0.0.0 -p 9500
```

</details>

### EMQXで外部HTTPスキーマを作成

1. ダッシュボードの左ナビゲーションメニューから **Smart Data Hub** -> **Schema Registry** を選択します。

2. **Internal** タブページで **Create** をクリックします。

3. 以下のパラメータで外部HTTPサーバースキーマを作成します：
   - **Name**：`myhttp`

   - **Type**：`External HTTP`

   - **URL**：サーバーが稼働している完全なURI。例：`http://server:9500/serde`

   - **Method**：`POST` または `GET` を選択。デフォルトは `POST`。外部サービスがクエリ文字列でリクエストフィールドを受け取る場合のみ `GET` を使用します。

4. **Create** をクリックします。

### スキーマを適用するルールの作成

EMQXのルールエンジンを使い、メッセージのエンコード・デコードにスキーマを適用するルールを作成します。

1. ダッシュボードのナビゲーションメニューから **Integration** -> **Rules** を選択します。

2. **Rules** ページの右上にある **Create** をクリックします。

3. 作成したスキーマを使い、以下のルールSQL文を記述します：

   ```sql
   SELECT
     schema_encode('myhttp', payload) as encoded,
     schema_decode('myhttp', encoded) as decoded
   FROM
     "t/external_http"
   ```

   `schema_encode('myhttp', payload)` と `schema_decode('myhttp', encoded)` の両方が設定した外部HTTPサーバーを呼び出し、指定されたペイロードのエンコード／デコードを行います。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに送信先トピックとして `external_http/out` を入力します。

6. **Payload** フィールドにメッセージコンテンツテンプレートとして `${.}` を入力します。

7. **Add** をクリックしてアクションをルールに追加します。

   このアクションはデコード済みメッセージをJSON形式でトピック `external_http/out` に送信します。`${.}` はルールの出力全体の値に実行時に置き換わる変数プレースホルダーです。

8. **Save** をクリックしてルールの作成を完了します。

### ルール実行結果の確認

1. ダッシュボードのナビゲーションメニューから **Diagnose** -> **WebSocket Client** を選択します。

2. 現在のEMQXインスタンスへの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要です。

3. **Connect** をクリックしてEMQXインスタンスにMQTTクライアントとして接続します。

4. **Subscription** エリアの **Topic** フィールドに `external_http/out` と入力し、**Subscribe** をクリックします。

5. **Publish** エリアの **Topic** フィールドに `t/external_http` と入力し、任意のペイロードを記入して **Publish** をクリックします。

6. WebSocket側でトピック `external_http/out` のメッセージを受信できることを確認します。例えば、ペイロードが `hello` の場合：

   ```json
   {"encoded":"\u0012\u001F\u0016\u0016\u0015","decoded":"hello"}
   ```
