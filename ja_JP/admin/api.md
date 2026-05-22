# REST API

EMQXはOpenAPI（Swagger）3.0仕様に準拠したHTTP管理APIを公開しています。

EMQXはREST APIを探索・操作するための複数の方法を提供しています。EMQX起動後、以下のAPI仕様エンドポイントが利用可能です。

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウン形式のAPIリファレンスページ。 |
| `/api-spec.md` | Markdown | AIエージェントや自動化ツールに適したMarkdown形式のAPIリファレンス。 |
| `/api-spec.json` | JSON | スクリプトやプログラム的ツールに適したOpenAPI 3.0仕様のJSON形式。 |
| `/api-docs/index.html` | HTML | ブラウザ上で直接APIコールをテスト可能なインタラクティブなSwagger UI。**非推奨**：v7で削除予定。 |

上記のすべてのエンドポイントは、ダッシュボード設定で`swagger_support`が`true`（デフォルト）に設定されている必要があります。`false`に設定すると、すべてのAPIドキュメントエンドポイントが無効になります。詳細は[ダッシュボード設定](../configuration/dashboard.md)をご参照ください。

本節ではEMQX REST APIの利用方法について説明します。

## 基本パス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降のすべてのAPIパスは`/api/v5`で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは、`Accept`ヘッダーに`application/json`を設定する必要があります。特に指定がない限り、レスポンスはJSON形式で返されます。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)標準に準拠しています。主なステータスコードは以下の通りです。

| コード | 説明 |
| ----- | ------------------------------------------------------------ |
| 200   | リクエスト成功。返却されるJSONデータに詳細が含まれます。 |
| 201   | 作成成功。新規オブジェクトがBodyに返されます。 |
| 204   | リクエスト成功。通常は削除や更新操作で、返却Bodyは空です。 |
| 400   | 不正なリクエスト。リクエストボディやパラメータのエラー。 |
| 401   | 認証失敗。APIキーが期限切れまたは存在しません。 |
| 403   | 禁止。オブジェクトが使用中、または依存制約があります。 |
| 404   | 見つかりません。Bodyの`message`フィールドで理由を確認可能。 |
| 409   | 競合。オブジェクトが既に存在するか、数の制限を超過。 |
| 500   | サーバ内部エラー。Bodyやログで原因を確認してください。 |

## 認証

EMQXのREST APIは主に2つの認証方法をサポートしています。APIキーを用いたベーシック認証とベアラートークン認証です。

### APIキーを用いたベーシック認証

この方法では、APIキーとシークレットキーをユーザー名とパスワードとして使用し、APIリクエストを認証します。EMQXのREST APIは[HTTPベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを使用する前に、APIキーを作成する必要があります。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0以降はダッシュボードのユーザー認証情報をREST API認証に使用できません。代わりにAPIキーを作成し、認証に使用してください。

:::

#### APIキーの作成

ダッシュボードの**システム** -> **APIキー**から手動でAPIキーを作成できます。詳しくは[システム - APIキー](../dashboard/system.md#api-keys)をご参照ください。

また、ブートストラップファイル方式でAPIキーを作成することも可能です。以下の設定ファイルを追加し、ファイルの場所を指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定したファイル内に複数のAPIキーを以下の形式で改行区切りで記述します。

```
{API Key}:{Secret Key}:{?Role}
```

- **API Key**: 任意の文字列でキー識別子。
- **Secret Key**: ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**: キーの[ロール](#roles-and-permissions)を指定。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
```

この方法で作成されたAPIキーは無期限で有効です。

EMQX起動時にファイル内のデータがAPIキーリストに追加されます。既存のAPIキーがある場合は、シークレットキーとロールが更新されます。

#### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つの事前定義ロールのいずれかを割り当てられます。

- **administrator**（管理者）: すべてのリソースにアクセス可能。ロール未指定時のデフォルト。
- **viewer**（閲覧者）: リソースやデータの閲覧のみ可能。REST APIのすべてのGETリクエストに対応。
- **publisher**（パブリッシャー）: MQTTメッセージのパブリッシュに特化し、メッセージパブリッシュ関連APIのみアクセス可能。

#### APIキーを用いた認証方法

APIキーとシークレットキーを取得したら、ベーシック認証のユーザー名にAPIキー、パスワードにシークレットキーを指定してリクエストを認証できます。

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

APIキー認証の代替として、ベアラートークンを使用してEMQX REST APIに安全かつプログラム的にアクセスできます。ベアラートークンを取得するには、以下のログインAPIエンドポイントにリクエストを送信します。

#### ベアラートークンの取得

ベアラートークンを取得するには、以下のログインAPIエンドポイントにHTTP `POST`リクエストを送信します。

```bash
POST http://your-emqx-address:8483/api/v5/login
```

**ヘッダー:**

- `Content-Type: application/json`

**リクエストボディ:**

```json
{
  "username": "admin",
  "password": "yourpassword"
}
```

- `your-emqx-address`はEMQXノードのアドレスまたはIPに置き換えてください。
- `"admin"`と`"yourpassword"`はEMQXダッシュボードの認証情報に置き換えてください。

レスポンスにはベアラートークンが含まれ、APIリクエストの認証に使用できます。

#### ベアラートークンを用いた認証

ベアラートークン取得後、APIリクエストの`Authorization`ヘッダーに以下のようにトークンを含めてください。

```bash
--header "Authorization: Bearer <your-token>"
```

## ページネーション

大量のデータを扱う一部APIではページネーション機能が提供されています。データの特性に応じて2種類のページネーション方式があります。

### ページ番号によるページネーション

ページネーション対応APIの多くは、`page`（ページ番号）と`limit`（ページサイズ）パラメータでページングを制御します。最大ページサイズは`10000`です。`limit`が指定されない場合、デフォルトは`100`です。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総データ数を予測できないため、`meta.hasnext`フィールドで次ページの有無を示します。

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

データが急速に変化し、ページ番号によるページネーションが非効率な一部APIではカーソルページネーションを採用しています。

`position`または`cursor`（開始位置）パラメータでデータの開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置から読み込む件数を指定します。最大ページサイズは`10000`で、`limit`未指定時は`100`です。

例：

```bash
GET /clients/{clientid}/mqueue_messages?position=1716187698257189921_0&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれ、`meta.position`または`meta.cursor`が次ページの開始位置を示します。

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

この方式はデータの変動が激しい場合に効率的かつ連続的なデータ取得を実現します。

## エラーコード

HTTPレスポンスステータスコードに加え、EMQXは特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時は、BodyにJSON形式でエラーコードが返されます。

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| エラーコード                                    | 説明                                                  |
| ---------------------------------------------- | ----------------------------------------------------- |
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています             |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはキー＆シークレットが間違っています |
| BAD_REQUEST                                    | リクエストパラメータが不正です                         |
| NOT_MATCH                                      | 条件が一致しません                                     |
| ALREADY_EXISTS                                 | リソースが既に存在します                               |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です                                  |
| BAD_LISTENER_ID                                | リスナーIDが不正です                                  |
| BAD_NODE_NAME                                  | ノード名が不正です                                    |
| BAD_RPC                                        | RPC失敗。クラスター状態と対象ノードの状態を確認してください |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります |
| EXCEED_LIMIT                                   | 作成しようとしたリソースが最大または最小制限を超えています |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています    |
| CONFLICT                                       | リクエストリソースが競合しています                     |
| NO_DEFAULT_VALUE                               | リクエストパラメータがデフォルト値を使用していません    |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています                 |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージIDの解析エラー                               |
| INVALID_ID                                     | IDスキーマが不正です                                  |
| MESSAGE_ID_NOT_FOUND                           | メッセージIDが存在しません                             |
| NOT_FOUND                                      | リソースが見つかりません                               |
| CLIENTID_NOT_FOUND                             | クライアントIDが見つかりません                         |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常はMQTTクライアントではありません） |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません                               |
| TOPIC_NOT_FOUND                                | トピックが見つかりません                               |
| USER_NOT_FOUND                                 | ユーザーが見つかりません                               |
| INTERNAL_ERROR                                 | サーバ内部エラー                                       |
| SERVICE_UNAVAILABLE                            | サービス利用不可                                       |
| SOURCE_ERROR                                   | ソースエラー                                           |
| UPDATE_FAILED                                  | 更新失敗                                              |
| REST_FAILED                                    | リセットソースまたは設定の失敗                         |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません                             |
