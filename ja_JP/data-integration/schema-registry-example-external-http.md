# Schema Registry Example - External HTTP Server

このページでは、Schema Registry とルールエンジンが、カスタムロジックを持つ外部HTTPサーバーを使ったメッセージのエンコードおよびデコードをどのようにサポートするかを示します。

場合によっては、EMQXがネイティブにサポートしていないカスタムのエンコードやデコードロジックを適用する必要があります。EMQXでは、ルール内の `schema_encode` および `schema_decode` 関数を通じて外部HTTPサービスを呼び出し、この処理を委任することが可能です。

## External HTTP API仕様

EMQXの `schema_encode` および `schema_decode` 関数と連携するカスタムのExternal HTTP APIを実装するには、外部HTTPサーバーはEMQXからのエンコードまたはデコード要求を処理する単一の `POST` エンドポイントを提供する必要があります。

### リクエスト形式

リクエストボディは以下のフィールドを持つJSONオブジェクトです：

- `payload`：Rule Engineの `schema_encode` または `schema_decode` 関数に渡されるBase64エンコード済みの文字列値
- `type`：評価される関数に応じて `encode` または `decode` の文字列
- `schema_name`：EMQXで設定されたこのExternal HTTPスキーマの名前を識別する文字列
- `opts`：EMQXで設定可能な任意の文字列で、追加オプションとしてHTTPサーバーにそのまま渡されます

### レスポンス形式

- サーバーはHTTPステータスコード `200` で応答する必要があります。
- レスポンスボディは結果を表すBase64エンコード済みの文字列を含む必要があります。このBase64値はEMQXに返す際にさらにJSONエンコードしてはいけません。

## 利用例

デバイスがバイナリメッセージをパブリッシュし、ペイロードにカスタムのXOR演算を使ってエンコードまたはデコードしたい場合を想定します。このセクションでは、シンプルな外部HTTPサービスを構築してカスタムのエンコード・デコードロジックをEMQXに統合する方法を示します。

### 外部HTTPサービスの構築

以下の例は、PythonとFlaskを使ってシンプルなHTTPサーバーを作成・実行する方法を示します。このサーバーはBase64エンコードされたデータを受け取り、デコードしたペイロードにXOR演算を適用します。

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

サーバーを実行するには：

```sh
# サーバーコードが同じディレクトリの `myapp.py` というファイルにあることを想定
flask --app myapp --debug run -h 0.0.0.0 -p 9500
```

</details>

### EMQXでExternal HTTPスキーマを作成

1. ダッシュボードの左側メニューから **Smart Data Hub** -> **Schema Registry** を選択します。

2. **Internal** タブページで **Create** をクリックします。

3. 以下のパラメータでExternal HTTPサーバースキーマを作成します：
   - **Name**: `myhttp`
   - **Type**: `External HTTP`
   - **URL**: サーバーが動作している完全なURI（例：`http://server:9500/serde`）

4. **Create** をクリックします。

### スキーマを適用するルールの作成

EMQXのルールエンジンを使って、メッセージのエンコード・デコードにスキーマを適用するルールを作成します。

1. ダッシュボードのナビゲーションメニューから **Integration** -> **Rules** を選択します。

2. **Rules** ページの右上にある **Create** をクリックします。

3. 作成したスキーマを使って以下のルールSQL文を記述します：

   ```sql
   SELECT
     schema_encode('myhttp', payload) as encoded,
     schema_decode('myhttp', encoded) as decoded
   FROM
     "t/external_http"
   ```

   `schema_encode('myhttp', payload)` と `schema_decode('myhttp', encoded)` はどちらも設定したExternal HTTPサーバーを呼び出し、指定されたペイロードのエンコード／デコードを行います。

4. **Add Action** をクリックし、**Action** フィールドのドロップダウンリストから `Republish` を選択します。

5. **Topic** フィールドに送信先トピックとして `external_http/out` と入力します。

6. **Payload** フィールドにメッセージコンテンツテンプレートとして `${.}` と入力します。

7. **Add** をクリックしてアクションをルールに追加します。

   このアクションは、デコードされたメッセージをJSON形式でトピック `external_http/out` に送信します。`${.}` はルールの出力全体の値に実行時に置き換わる変数プレースホルダーです。

8. **Save** をクリックしてルールの作成を完了します。

### ルール実行結果の確認

1. ダッシュボードのメニューから **Diagnose** -> **WebSocket Client** を選択します。

2. 現在のEMQXインスタンスの接続情報を入力します。
   - ローカルでEMQXを実行している場合はデフォルト値を使用できます。
   - 認証設定などEMQXのデフォルト設定を変更している場合は、ユーザー名やパスワードの入力が必要になることがあります。

3. **Connect** をクリックしてMQTTクライアントとしてEMQXインスタンスに接続します。

4. **Subscription** エリアの **Topic** フィールドに `external_http/out` と入力し、**Subscribe** をクリックします。

5. **Publish** エリアの **Topic** フィールドに `t/external_http` と入力し、任意のペイロードを記入して **Publish** をクリックします。

6. WebSocket側でトピック `external_http/out` のメッセージを受信できることを確認します。例えばペイロードが `hello` の場合：

   ```json
   {"encoded":"\u0012\u001F\u0016\u0016\u0015","decoded":"hello"}
   ```
