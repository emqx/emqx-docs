# スキーマレジストリの例 - 外部HTTPサーバー

このページでは、スキーマレジストリとルールエンジンが、カスタムロジックを持つ外部HTTPサーバーを使ってメッセージのエンコードおよびデコードをサポートする方法を示します。

場合によっては、EMQXがネイティブにサポートしていないカスタムのエンコードやデコードロジックを適用する必要があります。EMQXでは、ルール内の `schema_encode` および `schema_decode` 関数を通じて外部HTTPサービスを呼び出し、この処理を委任することが可能です。

## 外部HTTP API仕様

EMQXの `schema_encode` および `schema_decode` 関数と連携するカスタム外部HTTP APIを実装するには、外部HTTPサーバーがEMQXからのエンコードまたはデコード要求を処理する単一の `POST` エンドポイントを提供する必要があります。

### リクエスト形式

リクエストボディは以下のフィールドを持つJSONオブジェクトです：

- `payload`：ルールエンジンの `schema_encode` または `schema_decode` 関数に渡されるBase64エンコードされた文字列値。
- `type`：評価される関数に応じて、`encode` または `decode` の文字列。
- `schema_name`：EMQXで設定されたこの外部HTTPスキーマの名前を識別する文字列。
- `opts`：EMQXで設定可能な任意の文字列で、追加オプションとしてHTTPサーバーにそのまま渡されます。

### レスポンス形式

- サーバーはHTTPステータスコード `200` で応答する必要があります。
- レスポンスボディは結果を表すBase64エンコードされた文字列を含む必要があります。このBase64値はEMQXに返す際にさらにJSONエンコードしてはいけません。

## 利用例

デバイスがバイナリメッセージをパブリッシュし、ペイロードをカスタムのXOR演算でエンコードまたはデコードしたい場合を想定します。このセクションでは、シンプルな外部HTTPサービスを構築し、カスタムのエンコード・デコードロジックをEMQXに統合する方法を示します。

### 外部HTTPサービスの構築

以下の例は、PythonとFlaskを使ってシンプルなHTTPサーバーを作成・実行する方法を示しています。このサーバーはBase64エンコードされたデータを受け取り、デコードしたペイロードにXOR演算を適用します。

<details>
<summary><strong>外部HTTPサーバーのサンプルコード</strong></summary>

[Flask](https://flask.palletsprojects.com/en/stable/)がインストールされていることを確認してください：

```sh
pip install Flask==3.1.0
```

サンプルコード：

```python
from flask import Flask, request
import base64

app = Flask(__name__)

@app.route("/serde", methods=['POST'])
def serde():
    # 入力ペイロードはBase64エンコードされています
    body = request.get_json(force=True)
    print("incoming request:", body)
    payload64 = body.get("payload")
    payload = base64.b64decode(payload64)
    secret = 122
    response = bytes(b ^ secret for b in payload)
    # レスポンスもBase64エンコードする必要があります
    response64 = base64.b64encode(response)
    return response64
```

サーバーを起動するには：

```sh
# サーバーが同じディレクトリの `myapp.py` というファイル名であると仮定しています
flask --app myapp --debug run -h 0.0.0.0 -p 9500
```

</details>

### EMQXで外部HTTPスキーマを作成する

1. ダッシュボードにアクセスし、左側のナビゲーションメニューから **Smart Data Hub** -> **Schema Registry** を選択します。

2. **Internal** タブページで **Create** をクリックします。

3. 以下のパラメータで外部HTTPサーバースキーマを作成します：
   - **Name**：`myhttp`

   - **Type**：`External HTTP`

   - **URL**：サーバーが稼働している完全なURI。例：`http://server:9500/serde`

4. **Create** をクリックします。

### スキーマを適用するルールを作成する

EMQXのルールエンジンを使って、メッセージのエンコードおよびデコードにスキーマを適用するルールを作成します。

1. ダッシュボードのナビゲーションメニューから **Integration** -> **Rules** を選択します。

2. **Rules** ページの右上にある **Create** をクリックします。

3. 先ほど作成したスキーマを使って、以下のルールSQL文を記述します：

   ```sql
   SELECT
     schema_encode('myhttp', payload) as encoded,
     schema_decode('myhttp', encoded) as decoded
   FROM
     "t/external_http"
   ```

   `schema_encode('myhttp', payload)` と `schema_decode('myhttp', encoded)` の両方が、設定された外部HTTPサーバーを呼び出してペイロードのエンコード／デコードを行います。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに送信先トピックとして `external_http/out` を入力します。

6. **Payload** フィールドにメッセージコンテンツテンプレートとして `${.}` を入力します。

7. **Add** をクリックしてアクションをルールに追加します。

   このアクションは、デコードされたメッセージをJSON形式でトピック `external_http/out` に送信します。`${.}` はルールの出力全体の値に実行時に置き換えられる変数プレースホルダーです。

8. **Save** をクリックしてルール作成を完了します。

### ルール実行結果の確認

1. ダッシュボードで **Diagnose** -> **WebSocket Client** を選択します。

2. 現在のEMQXインスタンスの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要になることがあります。

3. **Connect** をクリックしてEMQXインスタンスにMQTTクライアントとして接続します。

4. **Subscription** エリアの **Topic** フィールドに `external_http/out` を入力し、**Subscribe** をクリックします。

5. **Publish** エリアの **Topic** フィールドに `t/external_http` を入力し、任意のペイロードを記入して **Publish** をクリックします。

6. WebSocket側でトピック `external_http/out` のメッセージを受信できることを確認します。例えば、ペイロードが `hello` の場合：

   ```json
   {"encoded":"\u0012\u001F\u0016\u0016\u0015","decoded":"hello"}
   ```
