# REST API

EMQXは、OpenAPI（Swagger）3.0仕様に準拠したHTTP管理APIを公開しています。

EMQX起動後、[http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html) にアクセスするとAPIドキュメントを閲覧でき、Swagger UIから管理APIを実行できます。デフォルトでは、ダッシュボード設定の`swagger_support`が`true`に設定されており、Swagger UIサポートが有効であることを示します。これにより、インタラクティブなAPIドキュメント生成など、Swagger関連の機能がすべて有効になります。`false`に設定するとこの機能を無効化できます。詳細は[ダッシュボード設定](../configuration/dashboard.md)をご参照ください。

本セクションでは、EMQX REST APIの利用方法について説明します。

## 基本パス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降のすべてのAPIパスは`/api/v5`で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは、`Accept`ヘッダーに`application/json`を設定する必要があります。これにより、レスポンスはJSON形式で返されます（特に指定がない限り）。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)の標準に従っています。主なステータスコードは以下の通りです。

| コード | 説明                                                         |
| ------ | ------------------------------------------------------------ |
| 200    | リクエスト成功。返却されるJSONデータに詳細が含まれます。     |
| 201    | 作成成功。新規オブジェクトがBodyに返されます。               |
| 204    | リクエスト成功。通常は削除や更新操作で返却Bodyは空です。     |
| 400    | 不正なリクエスト。リクエストボディやパラメータのエラー。     |
| 401    | 認証失敗。APIキーが期限切れまたは存在しません。               |
| 403    | 禁止。オブジェクトが使用中、または依存関係の制約があります。 |
| 404    | 見つかりません。Bodyの`message`フィールドで理由を確認可能。  |
| 409    | コンフリクト。オブジェクトが既に存在するか、数の上限超過。    |
| 500    | サーバ内部エラー。Bodyやログで原因を確認してください。        |

## 認証

EMQXのREST APIは、主にAPIキーを用いたベーシック認証とベアラートークン認証の2つの認証方式をサポートしています。

### APIキーを用いたベーシック認証

この方式では、APIキーとシークレットキーをユーザー名とパスワードとして認証に使用します。EMQXのREST APIは[HTTPベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを利用する前にAPIキーを作成してください。詳細は[APIキー管理](#api-key-management)をご覧ください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0以降はダッシュボードのユーザー認証情報でREST APIリクエストを認証できません。代わりにAPIキーを作成し、それを用いて認証してください。

:::

#### APIキーで認証する方法

APIキーとシークレットキーを取得したら、APIキーをユーザー名、シークレットキーをパスワードとしてベーシック認証を行います。

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

APIキー認証の代替として、ベアラートークンを使った安全かつプログラム的なEMQX REST APIへのアクセスも可能です。ベアラートークンを取得するには、以下のログインAPIエンドポイントにリクエストを送信します。

#### ベアラートークンの取得

ベアラートークンを取得するには、以下のログインAPIエンドポイントへHTTP `POST`リクエストを送信します。

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

レスポンスにベアラートークンが含まれ、これをAPIリクエストの認証に使用します。

#### ベアラートークンを使った認証

ベアラートークンを取得したら、APIリクエストの`Authorization`ヘッダーに以下のように含めます。

```bash
--header "Authorization: Bearer <your-token>"
```

## APIキー管理

### APIキーの作成

#### ダッシュボード

ダッシュボードの **システム** -> **APIキー** から手動でAPIキーを作成できます。

1. 右上の **+ 作成** ボタンをクリックし、作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **名前**（必須）：APIキーの名前を入力します。
   - **有効期限**：空欄にすると期限なしになります。
   - **有効化**：デフォルトで有効です。
   - **ロール**：ロールを選択します（任意）。[ロールと権限](#roles-and-permissions)を参照してください。
   - **スコープ**：付与するスコープを選択します（任意）。デフォルトはすべてのスコープ権限です。[APIスコープ](#api-scopes)を参照してください。
   - **備考**：任意で説明を入力します。
3. **確認**をクリックすると、作成成功ダイアログにAPIキーとシークレットキーが表示されます。

   ::: warning 重要

   APIキーとシークレットキーは必ずすぐに保存してください。シークレットキーは再表示されません。

   :::

4. **閉じる**をクリックしてダイアログを閉じます。

キー名をクリックすると詳細を確認でき、**編集**ボタンで有効期限、状態、備考を変更、**削除**ボタンでキーを削除できます。

#### ブートストラップファイル

ブートストラップファイルを使ってAPIキーを作成することも可能です。設定ファイルに以下を追加し、ファイルの場所を指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定したファイルに複数のAPIキーを以下の形式で記述します。各行は改行で区切ります。

```
{API Key}:{Secret Key}:{?Role}:{?Scopes}
```

- **API Key**：任意の文字列でキー識別子。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの[ロール](#roles-and-permissions)を指定。
- **Scopes（任意）**：キーがアクセス可能な[APIスコープ](#api-scopes)をカンマ区切りで指定。省略時はすべてのユーザー可視スコープが付与されます（管理者向けの全許可設定で、旧バージョンとの互換性のため）。ログイン専用スコープ（`user_management`, `mfa_management`, `sso_management`, `api_key_management`）はAPIキーには無効です。これらがブートストラップファイルに含まれている場合、EMQX起動時に削除され警告ログが出力されます。キーは作成されますが、該当スコープは付与されません。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

この方法で作成されたAPIキーは無期限で有効です。

EMQX起動時にファイル内のデータがAPIキーリストに追加されます。既存のAPIキーがある場合は、シークレットキー、ロール、スコープが更新されます。

### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つのプリセットロールのいずれかを割り当てられます。

- **administrator（管理者）**：すべてのリソースにアクセス可能。ロール未指定時のデフォルト。
- **viewer（閲覧者）**：リソースやデータの閲覧のみ可能。REST APIのすべてのGETリクエストに対応。
- **publisher（パブリッシャー）**：MQTTメッセージのパブリッシュ専用。メッセージパブリッシュ関連APIのみアクセス可能。

::: tip 注意
`publisher`ロールのキーは`publish`スコープのみ許可されます。スコープ割り当て時に`publish`以外のスコープを指定するとHTTP 400エラーになります。キーのロールを`publisher`に変更する場合、同時に`"scopes": ["publish"]`または空リストをリクエストに含めてください。既存のスコープに`publish`以外が含まれているとリクエストは拒否されます。
:::

### APIスコープ

スコープはEMQX 5.10で導入された、APIキーごとの権限の粒度を示すものです。スコープは[ロールと権限](#roles-and-permissions)とは独立しており、両者を組み合わせてアクセス制御を行います。

| 次元 | 目的 | 粒度 |
| ---- | ---- | ---- |
| **ロール** | HTTPメソッドの制限（読み取り専用、書き込み、パブリッシュのみなど） | リクエストアクション |
| **スコープ** | APIのドメイン制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方を通過した場合のみ許可されます。

マイクロサービスや統合シナリオでは、外部システムがEMQX管理機能の一部だけにアクセスすることが多いです。例えば監視プラットフォームは`monitoring`スコープのみ、ルールパブリッシュサービスは`data_integration`のみ、クラスター運用ツールは`cluster_operations`のみ必要です。スコープにより最小権限の原則を適用し、キー漏洩時の被害範囲を最小化できます。

#### 組み込みスコープ

EMQX 5.10には以下の10個のスコープがあり、APIキー作成時に自由に組み合わせ可能です。

| スコープ | 名称 | 主なAPI領域 |
| -------- | ---- | ----------- |
| `connections` | 接続管理 | `/clients`, `/subscriptions`, `/topics`, `/banned`, `/retainer`, `/file_transfer`, `/mqtt/delayed`, `/mqtt/topic_rewrite`など |
| `publish` | メッセージパブリッシュ | `/publish`, `/publish/bulk` |
| `data_integration` | データ統合 | `/rules`, `/connectors`, `/actions`, `/schema_registry`, `/schema_validations`, `/message_transformations`, `/exhooks`, `/ai/*` |
| `access_control` | アクセス制御 | `/authentication`, `/authorization/*` |
| `gateways` | プロトコルゲートウェイ | `/gateways`, `/coap/*`, `/lwm2m/*`, `/gcp_devices`など |
| `monitoring` | 監視データ | `/metrics`, `/stats`, `/monitor*`, `/alarms`, `/trace`, `/slow_subscriptions`, `/telemetry`, `/prometheus/{auth,stats,data_integration,...}`など |
| `cluster_operations` | クラスター運用 | `/cluster*`, `/nodes`, `/load_rebalance`, `/node_eviction`, `/mt/*`など |
| `system` | システム設定 | `/configs*`, `/listeners*`, `/plugins*`, `/ds/*`, `/data/*`, `/status`, `/relup`, `/opentelemetry*`, `/prometheus`など |
| `audit` | 監査ログ | `/audit` |
| `license` | ライセンス | `/license*` |

これらのAPIキー用スコープに加え、ダッシュボードログインユーザーには4つのログイン専用スコープがあり、ブラウザセッション専用でAPIキーには割り当てられません。詳細は[ログインユーザースコープ](../dashboard/system.md#login-user-scopes)を参照してください。

| スコープ | 必要ロール | 目的 |
| -------- | ---------- | ---- |
| `user_management` | administrator | ダッシュボードユーザー管理 |
| `sso_management` | administrator | SSOバックエンドとSSOユーザーレコード管理 |
| `api_key_management` | administrator | APIキー管理 |
| `mfa_management` | 任意 | 自アカウントのMFA管理。管理者は他ユーザーのMFAも管理可能 |

::: tip
スコープ名はEMQXのアップグレード間で変更されない安定した識別子です。OpenAPIタグが変更されても、同じスコープを持つキーは引き続き動作します。
:::

ダッシュボードログイン、SSOコールバック、APIキー自己管理エンドポイント（例：`/api_key`）は、キーの`scopes`設定に関わらずAPIキー認証を受け付けません。これはダッシュボードのセキュリティ境界であり、スコープモデルとは別の制御です。

#### `scopes`のデフォルト動作

APIキーの`scopes`フィールドは以下のルールに従います。

| `scopes`の値 | 意味 |
| ------------ | ---- |
| **未設定**（フィールドなし） | すべての業務用エンドポイントが許可されます。スコープ機能導入前に作成されたキーの後方互換デフォルトです。 |
| **空リスト** `[]` | すべての業務用エンドポイントが拒否されます。キーを削除せずに一時無効化したい場合に有用です。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定したスコープのみ許可されます。 |

ブートストラップファイルのエントリでスコープ指定を省略すると、ユーザー可視のすべてのスコープ（管理者向け全許可）が明示的に割り当てられます。これによりアップグレード時に既存キーの権限が意図せず削除されることを防ぎます。

同様の3状態モデルはダッシュボードログインユーザーにも適用されます。ログインユーザーの`scopes`フィールドが未設定の場合、ロールに基づくデフォルトセットが割り当てられます。管理者は4つのログイン専用スコープを含むすべてのスコープを持ち、閲覧者は10個のAPIキー用スコープのみ持ちます。`mfa_management`などのログイン専用スコープは明示的に割り当てない限り持ちません。

#### 利用可能なスコープの一覧取得

EMQXは利用可能なスコープカタログを取得するために以下の2つのエンドポイントを公開しています。

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記10個の業務ドメインスコープ）を返します。APIキー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（ログイン専用4スコープを含む）を返します。ベアラートークン認証が必要です。

これらのエンドポイントはスコープ選択UIの表示や自動化スクリプトの検証に利用できます。

```bash
# APIキー用スコープ一覧取得
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ一覧取得（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの方法で設定可能です。

- **ダッシュボード**：**システム** -> **APIキー**でキー作成・編集時に付与するスコープを選択。
- **REST API**：作成・更新リクエストボディに`"scopes": ["monitoring", "cluster_operations"]`を含める。
- **ブートストラップファイル**：各行の4番目のセグメントにカンマ区切りスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ページネーション

大量データを扱う一部APIではページネーション機能が提供されています。データの特性に応じて2種類のページネーション方式があります。

### ページ番号方式

ページ番号方式のページネーションをサポートする多くのAPIでは、`page`（ページ番号）と`limit`（ページサイズ）パラメータで制御します。最大ページサイズは`10000`です。`limit`未指定時はデフォルトで`100`となります。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総件数を予測できないため、`meta.hasnext`フィールドで次ページの有無を示します。

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

### カーソル方式

データが急速に変化し、ページ番号方式が非効率な一部APIではカーソル方式を採用しています。

`position`または`cursor`（開始位置）パラメータで読み込み開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置からの件数を指定します。最大ページサイズは`10000`です。`limit`未指定時はデフォルトで`100`です。

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

この方式はデータ変動が激しい場合に効率的かつ連続的なデータ取得を実現します。

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

| エラーコード                                   | 説明                                                         |
| ---------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています                  |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワード、またはAPIキー＆シークレットが間違っています |
| BAD_REQUEST                                    | リクエストパラメータが不正です                               |
| NOT_MATCH                                      | 条件に一致しません                                           |
| ALREADY_EXISTS                                 | リソースが既に存在します                                     |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です                                         |
| BAD_LISTENER_ID                                | リスナーIDが不正です                                         |
| BAD_NODE_NAME                                  | ノード名が不正です                                           |
| BAD_RPC                                        | RPC失敗。クラスター状態や対象ノードの状態を確認してください     |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります |
| EXCEED_LIMIT                                   | 作成しようとしたリソースが最大または最小制限を超えています       |
| INVALID_PARAMETER                              | リクエストパラメータが不正で境界値を超えています               |
| CONFLICT                                       | リクエストリソースに競合があります                           |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使用されていません         |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています                       |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージIDの解析エラー                                     |
| INVALID_ID                                     | IDスキーマが不正です                                         |
| MESSAGE_ID_NOT_FOUND                           | メッセージIDが存在しません                                   |
| NOT_FOUND                                      | リソースが見つかりません                                     |
| CLIENTID_NOT_FOUND                             | クライアントIDが見つかりません                               |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常はMQTTクライアントではありません） |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません                                     |
| TOPIC_NOT_FOUND                                | トピックが見つかりません                                     |
| USER_NOT_FOUND                                 | ユーザーが見つかりません                                     |
| INTERNAL_ERROR                                 | サーバ内部エラー                                             |
| SERVICE_UNAVAILABLE                            | サービス利用不可                                             |
| SOURCE_ERROR                                   | ソースエラー                                                 |
| UPDATE_FAILED                                  | 更新に失敗しました                                           |
| REST_FAILED                                    | ソースまたは設定のリセットに失敗しました                     |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません                                   |
