# REST API

EMQXはOpenAPI（Swagger）3.0仕様に準拠したHTTP管理APIを公開しています。

EMQXはREST APIを探索・操作するための複数の方法を提供しています。EMQX起動後、以下のAPI仕様エンドポイントが利用可能です：

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウン形式のAPIリファレンスページ。 |
| `/api-spec.md` | Markdown | Markdown形式のAPIリファレンス。AIエージェントや自動化ツール向け。 |
| `/api-spec.json` | JSON | JSON形式のOpenAPI 3.0仕様。スクリプトやプログラムツール向け。 |
| `/api-docs/index.html` | HTML | ブラウザ上でAPI呼び出しを直接テストできるインタラクティブなSwagger UI。**非推奨**：v7で削除予定。 |

上記の全てのエンドポイントは、ダッシュボード設定で`swagger_support`が`true`（デフォルト）に設定されている必要があります。`false`に設定するとAPIドキュメントの全エンドポイントが無効になります。詳細は[ダッシュボード設定](../configuration/dashboard.md)をご参照ください。

本セクションではEMQX REST APIの利用方法を紹介します。

## 基本パス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降の全APIパスは`/api/v5`で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは`Accept`ヘッダーに`application/json`を設定する必要があり、指定がなければレスポンスはJSON形式で返されます。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)の標準に従っています。主なステータスコードは以下の通りです：

| コード | 説明                                                  |
| ----- | ----------------------------------------------------- |
| 200   | リクエスト成功。返却されるJSONデータに詳細が含まれます。 |
| 201   | 作成成功。新規オブジェクトがBodyに返されます。         |
| 204   | リクエスト成功。通常は削除や更新操作で返却Bodyは空です。 |
| 400   | 不正なリクエスト。リクエストボディやパラメータのエラー。 |
| 401   | 認証失敗。APIキーの期限切れまたは存在しません。         |
| 403   | 禁止。オブジェクトが使用中、または依存関係制約があります。 |
| 404   | 見つかりません。Bodyの`message`フィールドで理由を確認可能。 |
| 409   | 競合。オブジェクトが既に存在するか、数の上限を超過。     |
| 500   | サーバ内部エラー。Bodyやログで原因を確認してください。   |

## 認証

EMQXのREST APIは主に2つの認証方法をサポートしています：APIキーを用いたBasic認証とベアラートークン認証です。

### APIキーを用いたBasic認証

この方法では、APIキーとシークレットキーをユーザー名とパスワードとして使用し、APIリクエストを認証します。EMQXのREST APIは[HTTP Basic認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを使用する前にAPIキーを作成する必要があります。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0以降はダッシュボードのユーザー認証情報をREST API認証に使用できません。代わりにAPIキーを作成して認証に使用してください。

:::

#### APIキーの作成

ダッシュボードの**システム** -> **APIキー**から手動でAPIキーを作成できます。詳細は[システム - APIキー](../dashboard/system.md#api-keys)をご参照ください。

また、ブートストラップファイル方式でAPIキーを作成することも可能です。以下の設定ファイルを追加し、ファイルの場所を指定します：

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイル内には複数のAPIキーを以下の形式で改行区切りで記述します：

`{API Key}:{Secret Key}:{?Role}`

- **API Key**：任意の文字列でキー識別子。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの[ロール](#roles-and-permissions)を指定。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
```

この方法で作成したAPIキーは無期限で有効です。

EMQX起動時にファイル内のデータがAPIキーリストに追加されます。既存のAPIキーがある場合は、シークレットキーとロールが更新されます。

#### ロールと権限

REST APIはロールベースのアクセス制御を実装しています。APIキー作成時に以下の3つのプリセットロールのいずれかを割り当てられます：

- **Administrator**：全リソースにアクセス可能。ロール識別子は`administrator`。指定がなければデフォルト。
- **Viewer**：リソースやデータの閲覧のみ可能。REST APIの全GETリクエストに対応。ロール識別子は`viewer`。
- **Publisher**：MQTTメッセージのパブリッシュ専用。メッセージパブリッシュ関連APIのみアクセス可能。ロール識別子は`publisher`。

#### APIキーを用いた認証方法

APIキーとシークレットキーを取得したら、それらを使ってリクエストを認証します。APIキーはユーザー名、シークレットキーはパスワードとしてBasic認証に使用します。

各言語での例：

:::: tabs type:card
:::tab cURL

```bash
curl -X GET http://localhost:18083/api/v5/nodes \
     -u 4f33d24d7b8e448d:gwtbmFJZrnzUu8mPK1BxUkBA66PygETiDEegkf1q8dD \
     -H "Content-Type: application/json"
```

:::
::: tab Java

```java
import okhttp3.*;

import java.io.IOException;

public class EMQXNodesAPIExample {
    public static void main(String[] args) {
        try {
            String username = "4f33d24d7b8e448d";
            String password = "gwtbmFJZrnzUu8mPK1BxUkBA66PygETiDEegkf1q8dD";

            OkHttpClient client = new OkHttpClient();

            Request request = new Request.Builder()
                    .url("http://localhost:18083/api/v5/nodes")
                    .header("Content-Type", "application/json")
                    .header("Authorization", Credentials.basic(username, password))
                    .build();

            Response response = client.newCall(request).execute();
            System.out.println(response.body().string());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

:::
::: tab Python

```python
import urllib.request
import json
import base64

username = '4f33d24d7b8e448d'
password = 'gwtbmFJZrnzUu8mPK1BxUkBA66PygETiDEegkf1q8dD'

url = 'http://localhost:18083/api/v5/nodes'

req = urllib.request.Request(url)
req.add_header('Content-Type', 'application/json')

auth_header = "Basic " + base64.b64encode((username + ":" + password).encode()).decode()
req.add_header('Authorization', auth_header)

with urllib.request.urlopen(req) as response:
    data = json.loads(response.read().decode())

print(data)

```

:::
::: tab Go

```go
package main

import (
    "fmt"
    "net/http"
    "bytes"
    "encoding/json"
)

func main() {
    username := "4f33d24d7b8e448d"
    password := "gwtbmFJZrnzUu8mPK1BxUkBA66PygETiDEegkf1q8dD"

    url := "http://localhost:18083/api/v5/nodes"

    req, err := http.NewRequest("GET", url, nil)
    if err != nil {
        panic(err)
    }
    req.SetBasicAuth(username, password)
    req.Header.Set("Content-Type", "application/json")

    client := &http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    buf := new(bytes.Buffer)
    _, err = buf.ReadFrom(resp.Body)
    if err != nil {
        panic(err)
    }

    var data interface{}
    json.Unmarshal(buf.Bytes(), &data)
    fmt.Println(data)
}

```

:::
::: tab JavaScript

```js
const axios = require('axios')

const username = '4f33d24d7b8e448d'
const password = 'gwtbmFJZrnzUu8mPK1BxUkBA66PygETiDEegkf1q8dD'

axios
  .get('http://localhost:18083/api/v5/nodes', {
    auth: {
      username: username,
      password: password,
    },
    headers: {
      'Content-Type': 'application/json',
    },
  })
  .then((response) => {
    console.log(response.data)
  })
  .catch((error) => {
    console.log(error)
  })
```

:::
::::

### ベアラートークン認証

APIキー認証の代替として、EMQX REST APIへの安全かつプログラム的なアクセスのためにベアラートークンを使用できます。ベアラートークンを取得するには、以下のログインAPIエンドポイントにリクエストを送信します。

#### ベアラートークンの取得

ベアラートークンを取得するには、以下のログインAPIエンドポイントにHTTP `POST`リクエストを送信します：

```bash
POST http://your-emqx-address:8483/api/v5/login
```

**ヘッダー：**

- `Content-Type: application/json`

**リクエストボディ：**

```json
{
  "username": "admin",
  "password": "yourpassword"
}
```

- `your-emqx-address`はEMQXノードのアドレスまたはIPに置き換えてください。
- `"admin"`と`"yourpassword"`はEMQXダッシュボードの認証情報に置き換えてください。

レスポンスにベアラートークンが含まれ、APIリクエストの認証に使用できます。

#### ベアラートークンを用いた認証

ベアラートークンを取得したら、APIリクエストの`Authorization`ヘッダーに以下のように含めてください：

```bash
--header "Authorization: Bearer <your-token>"
```

## ページネーション

大量データを扱う一部APIではページネーション機能が提供されています。データの特性に応じて2種類のページネーション方式があります。

### ページ番号によるページネーション

ページネーション対応APIの多くでは、`page`（ページ番号）と`limit`（ページサイズ）パラメータでページ制御が可能です。最大ページサイズは`10000`です。`limit`が指定されない場合はデフォルトで`100`となります。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総データ件数を予測できないため、`meta.hasnext`フィールドで次ページの有無を示します：

```json
{
  "data":[],
  "meta":{
    "count":0,
    "limit":20,
    "page":1,
    "hasnext":false
  }
}
```

### カーソルによるページネーション

データが急速に変化し、ページ番号方式が非効率な一部APIではカーソルページネーションを採用しています。

`position`または`cursor`（開始位置）パラメータでデータの開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置から取得する件数を指定します。最大ページサイズは`10000`です。`limit`が指定されない場合はデフォルトで`100`です。

例：

```bash
GET /clients/{clientid}/mqueue_messages?position=1716187698257189921_0&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれ、`meta.position`または`meta.cursor`が次ページの開始位置を示します：

```json
{
    "meta": {
        "start": "1716187698009179275_0",
        "position": "1716187698491337643_0"
    },
    "data": [
        {
            "inserted_at": "1716187698260190832",
            "publish_at": 1716187698260,
            "from_clientid": "mqttx_70e2eecf_10",
            "from_username": "undefined",
            "msgid": "000618DD161F682DF4450000F4160011",
            "mqueue_priority": 0,
            "qos": 0,
            "topic": "t/1",
            "payload": "SGVsbG8gRnJvbSBNUVRUWCBDTEk="
        }
    ]
}
```

この方式はデータ変動が激しいシナリオで効率的に連続したデータ取得を実現します。

## エラーコード

HTTPレスポンスステータスコードに加えて、EMQXは特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時は、BodyにJSON形式でエラーコードが返されます：

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| エラーコード                                    | 説明                                                  |
| ---------------------------------------------- | ----------------------------------------------------- |
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています            |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはキー＆シークレットが間違っています |
| BAD_REQUEST                                    | リクエストパラメータが不正です                         |
| NOT_MATCH                                      | 条件が一致しません                                     |
| ALREADY_EXISTS                                 | リソースが既に存在します                               |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です                                  |
| BAD_LISTENER_ID                                | 不正なリスナーIDです                                  |
| BAD_NODE_NAME                                  | 不正なノード名です                                    |
| BAD_RPC                                        | RPC失敗。クラスター状態および対象ノードの状態を確認してください |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります |
| EXCEED_LIMIT                                   | 作成しようとしたリソースが最大または最小制限を超えています |
| INVALID_PARAMETER                              | リクエストパラメータが不正で境界値を超えています       |
| CONFLICT                                       | リクエストリソースに競合があります                     |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使われていません   |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています                 |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージIDの解析エラーです                            |
| INVALID_ID                                     | 不正なIDスキーマです                                  |
| MESSAGE_ID_NOT_FOUND                           | メッセージIDが存在しません                             |
| NOT_FOUND                                      | リソースが見つかりません                               |
| CLIENTID_NOT_FOUND                             | クライアントIDが見つかりません                         |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常MQTTクライアントではありません） |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません                               |
| TOPIC_NOT_FOUND                                | トピックが見つかりません                               |
| USER_NOT_FOUND                                 | ユーザーが見つかりません                               |
| INTERNAL_ERROR                                 | サーバ内部エラーです                                   |
| SERVICE_UNAVAILABLE                            | サービスが利用できません                               |
| SOURCE_ERROR                                   | ソースエラーです                                       |
| UPDATE_FAILED                                  | 更新に失敗しました                                     |
| REST_FAILED                                    | リセットソースまたは設定に失敗しました                 |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません                             |
