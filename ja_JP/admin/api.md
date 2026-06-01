# REST API

EMQXはOpenAPI（Swagger）3.0仕様に準拠したHTTP管理APIを公開しています。

EMQX起動後、[http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html) にアクセスすると、APIドキュメントを閲覧でき、Swagger UIから管理APIを実行できます。デフォルトではダッシュボード設定の `swagger_support` が `true` に設定されており、Swagger UIのサポートが有効で、インタラクティブなAPIドキュメント生成などのSwagger関連機能がすべて有効になっています。これを `false` に設定して機能を無効化することも可能です。詳細は[ダッシュボード設定](../configuration/dashboard.md)をご参照ください。

本節ではEMQX REST APIの利用方法を紹介します。

## 基本パス

EMQXのREST APIはバージョン管理がされており、EMQX 5.0.0以降のすべてのAPIパスは `/api/v5` で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは、`Accept` ヘッダーを `application/json` に設定する必要があり、特に指定がない限りレスポンスはJSON形式で返されます。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)標準に準拠しています。主なステータスコードは以下の通りです。

| コード | 説明                                                         |
| ------ | ------------------------------------------------------------ |
| 200    | リクエスト成功。返却されるJSONデータに詳細が含まれます。     |
| 201    | 作成成功。新規オブジェクトがBodyに返されます。               |
| 204    | リクエスト成功。通常は削除や更新操作で返却Bodyは空です。     |
| 400    | 不正なリクエスト。リクエストボディやパラメータのエラー。     |
| 401    | 認証エラー。APIキーの有効期限切れまたは存在しません。         |
| 403    | 禁止。オブジェクトが使用中、または依存関係の制約があります。   |
| 404    | 見つかりません。Bodyの `message` フィールドで理由を確認可能。 |
| 409    | コンフリクト。オブジェクトが既に存在するか、数の上限超過。     |
| 500    | サーバ内部エラー。Bodyやログで原因を確認してください。         |

## 認証

EMQXのREST APIは主に2つの認証方式をサポートしています。APIキーを用いたベーシック認証とベアラートークン認証です。

### APIキーを用いたベーシック認証

この方式では、APIキーとシークレットキーをそれぞれユーザー名とパスワードとしてAPIリクエストの認証に使用します。EMQXのREST APIは[HTTPベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを利用する前にAPIキーを作成する必要があります。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0以降はダッシュボードのユーザー認証情報でREST API認証を行うことはできません。代わりにAPIキーを作成し、それを用いて認証してください。

:::

#### APIキーの作成

##### ダッシュボード

ダッシュボードの **システム** -> **APIキー** から手動でAPIキーを作成できます。

1. 右上の **+ 作成** ボタンをクリックし、作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **名前**（必須）：APIキーの名前を入力します。
   - **有効期限**：空欄の場合は期限なしとなります。
   - **有効化**：デフォルトで有効です。
   - **ロール**：ロールを選択します（任意）。詳細は[ロールと権限](#roles-and-permissions)を参照してください。
   - **スコープ**：付与するスコープを選択します（任意）。デフォルトはすべてのスコープ権限です。[APIスコープ](#api-scopes)を参照してください。
   - **備考**：キーの説明を任意で入力できます。
3. **確認** をクリックすると、APIキーとシークレットキーが **作成成功** ダイアログに表示されます。

   ::: warning 重要

   APIキーとシークレットキーはこの時点で必ず保存してください。シークレットキーは再表示されません。

   :::

4. **閉じる** をクリックしてダイアログを閉じます。

キー名をクリックすると詳細を確認でき、**編集** ボタンで有効期限や状態、備考を変更、**削除** ボタンでキーを削除できます。

##### ブートストラップファイル

ブートストラップファイル方式でもAPIキーを作成可能です。以下のように設定ファイルの場所を指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定したファイルに複数のAPIキーを `{API Key}:{Secret Key}:{?Role}:{?Scopes}` の形式で改行区切りで記述します。

- **API Key**：任意の文字列でキー識別子。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの[ロール](#roles-and-permissions)を指定。
- **Scopes（任意）**：キーがアクセス可能な[APIスコープ](#api-scopes)をカンマ区切りで指定。省略時はすべてのユーザー可視スコープが付与されます（管理者全許可、旧バージョンとの互換性のため）。ログイン専用スコープ（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）はAPIキーには無効です。ブートストラップファイルにこれらが含まれる場合、EMQX起動時に削除され警告ログが出力されます。キーは作成されますが該当スコープは付与されません。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

この方法で作成されたAPIキーは無期限で有効です。

EMQX起動時にファイル内のデータがAPIキーリストに追加され、既存のAPIキーがあればシークレットキー、ロール、スコープが更新されます。

#### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つのプリセットロールのいずれかを割り当てられます。

- **administrator**（管理者）：すべてのリソースにアクセス可能で、ロール未指定時のデフォルト値です。
- **viewer**（閲覧者）：リソースやデータの閲覧のみ可能で、REST APIのすべてのGETリクエストに対応します。
- **publisher**（パブリッシャー）：MQTTメッセージのパブリッシュ専用に設計されており、メッセージパブリッシュ関連APIのみアクセス可能です。

::: tip 注意
`publisher` ロールのキーは `publish` スコープのみ許容します。スコープ割り当て時に `publish` 以外のスコープが指定されるとHTTP 400エラーになります。キーのロールを `publisher` に変更する場合、同時リクエストで `"scopes": ["publish"]` または空リストを含める必要があります。既存のスコープに `publish` 以外が含まれている場合、指定がないとリクエストは拒否されます。
:::

#### APIスコープ

スコープはEMQX 5.10で導入されたAPIキー単位の権限管理で、キーがアクセス可能なREST APIの業務領域を宣言します。スコープと[ロールと権限](#roles-and-permissions)は独立しており、両方のチェックを通過した場合のみアクセスが許可される2層のアクセス制御を形成します。

| 次元 | 目的 | 粒度 |
| ---- | ---- | ---- |
| **ロール** | HTTP動詞の制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | APIドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方を満たす必要があります。

##### スコープが必要な理由

マイクロサービスや統合シナリオでは、外部システムがEMQX管理機能の一部のみアクセスすることが一般的です。

- 監視プラットフォームは `monitoring` スコープ（`/metrics`、`/stats`、`/prometheus` など）だけを必要とします。
- ルールパブリッシュサービスは `data_integration`（`/rules`、`/connectors`、`/actions` など）だけを必要とします。
- クラスター運用ツールは `cluster_operations`（`/cluster`、`/nodes`、`/load_rebalance` など）だけを必要とします。

`administrator` / `viewer` / `publisher` のみでは粒度が粗く、例えばルールの書き込み権限を与えるには管理者権限を渡す必要があり、システム全体の完全な制御権を与えることになります。

スコープを使うことで最小権限の原則に基づき、必要なスコープのみをキーに付与し、キーが漏洩した場合の影響範囲を最小化できます。

##### 組み込みスコープ

EMQX 5.10には以下の10個のスコープがあり、APIキー作成時に自由に組み合わせて指定できます。

| スコープ | 名称 | 代表的なAPI領域 |
| -------- | ---- | --------------- |
| `connections` | 接続管理 | `/clients`、`/subscriptions`、`/topics`、`/banned`、`/retainer`、`/file_transfer`、`/mqtt/delayed`、`/mqtt/topic_rewrite` など |
| `publish` | メッセージパブリッシュ | `/publish`、`/publish/bulk` |
| `data_integration` | データ統合 | `/rules`、`/connectors`、`/actions`、`/schema_registry`、`/schema_validations`、`/message_transformations`、`/exhooks`、`/ai/*` |
| `access_control` | アクセス制御 | `/authentication`、`/authorization/*` |
| `gateways` | プロトコルゲートウェイ | `/gateways`、`/coap/*`、`/lwm2m/*`、`/gcp_devices` など |
| `monitoring` | 監視データ | `/metrics`、`/stats`、`/monitor*`、`/alarms`、`/trace`、`/slow_subscriptions`、`/telemetry`、`/prometheus/{auth,stats,data_integration,...}` など |
| `cluster_operations` | クラスター運用 | `/cluster*`、`/nodes`、`/load_rebalance`、`/node_eviction`、`/mt/*` など |
| `system` | システム設定 | `/configs*`、`/listeners*`、`/plugins*`、`/ds/*`、`/data/*`、`/status`、`/relup`、`/opentelemetry*`、`/prometheus` など |
| `audit` | 監査ログ | `/audit` |
| `license` | ライセンス | `/license*` |

これらのAPIキー用スコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、APIキーには割り当てられません。ログインユーザーのスコープ割り当てと適用については[ログインユーザースコープ](../dashboard/system.md#login-user-scopes)を参照してください。

| スコープ | 必要ロール | 目的 |
| -------- | ---------- | ---- |
| `user_management` | Administrator | ダッシュボードユーザー管理 |
| `sso_management` | Administrator | SSOバックエンドとSSOユーザー管理 |
| `api_key_management` | Administrator | APIキー管理 |
| `mfa_management` | 任意 | 自アカウントのMFA管理。管理者は他ユーザーのMFAも管理可能 |

::: tip
スコープ名はEMQXのアップグレード間で変更されない安定した識別子です。OpenAPIタグ名が変更されても、同じスコープを持つキーは引き続き動作します。
:::

ダッシュボードログイン、SSOコールバック、APIキー自己管理エンドポイント（例：`/api_key`）は、キーの `scopes` 設定に関わらずAPIキー認証を受け付けません。これはスコープモデルとは無関係なダッシュボードのセキュリティ境界です。

##### `scopes` のデフォルト動作

APIキーの `scopes` フィールドは以下のルールに従います。

| `scopes` の値 | 意味 |
| ------------- | ---- |
| **未設定**（フィールドなし） | すべての業務エンドポイントが許可されます。スコープ機能導入前に作成されたキーの後方互換デフォルトです。 |
| **空リスト** `[]` | すべての業務エンドポイントが拒否されます。キーのソフト無効化に便利です。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定されたスコープのみ許可されます。 |

ブートストラップファイルのエントリでスコープ部分を省略すると、キーは明示的にすべてのユーザー可視スコープ（管理者全許可）で書き込まれ、アップグレード時に既存キーの権限が不意に削除されることを防ぎます。

同様の3状態モデルはダッシュボードログインユーザーにも適用されます。ログインユーザーの `scopes` フィールドがない場合、ロール由来のデフォルトセットが割り当てられます。管理者は4つのログイン専用スコープを含むすべてのスコープを得ます。ビューアは10個のAPIキー用スコープすべてを得ますが、4つのログイン専用スコープ（`mfa_management`含む）は明示的に割り当てない限り持ちません。

##### 利用可能なスコープ一覧の取得

EMQXは利用可能なスコープカタログを照会するために2つのエンドポイントを公開しています。

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記10個の業務ドメインスコープ）を返します。APIキー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（4つのログイン専用スコープ含む）を返します。ベアラートークン認証が必要です。

スコープ選択UIの構築や自動化スクリプトの検証に利用してください。

```bash
# APIキー用スコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

##### スコープの割り当て

スコープは以下のいずれかの方法で設定可能です。

- **ダッシュボード**：**システム** -> **APIキー** でキー作成・編集時に付与するスコープを選択。
- **REST API**：作成・更新リクエストボディに `"scopes": ["monitoring", "cluster_operations"]` を含める。
- **ブートストラップファイル**：各行の4番目のセグメントにカンマ区切りでスコープ一覧を指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

#### APIキーを用いた認証方法

APIキーとシークレットキーを取得したら、それらを使ってリクエストを認証できます。APIキーをユーザー名、シークレットキーをパスワードとしてベーシック認証に使用します。

各言語の例：

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

### ベアラートークンを用いた認証

APIキー認証の代替として、ベアラートークンを使った安全かつプログラム的なEMQX REST APIアクセスも可能です。ベアラートークンは以下のログインAPIエンドポイントにリクエストを送信して取得します。

#### ベアラートークンの取得

以下のログインAPIエンドポイントにHTTP `POST` リクエストを送信してください。

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

- `your-emqx-address` はEMQXノードのアドレスまたはIPに置き換えてください。
- `"admin"` と `"yourpassword"` はEMQXダッシュボードの認証情報に置き換えてください。

レスポンスにベアラートークンが含まれ、APIリクエストの認証に利用できます。

#### ベアラートークンを用いた認証

ベアラートークンを取得したら、APIリクエストの `Authorization` ヘッダーに以下のように含めてください。

```bash
--header "Authorization: Bearer <your-token>"
```

## ページネーション

大量データを扱うAPIではページネーション機能が提供されています。データの特性に応じて2種類のページネーション方式があります。

### ページ番号によるページネーション

ページネーション対応APIの多くは、`page`（ページ番号）と `limit`（ページサイズ）パラメータで制御可能です。最大ページサイズは `10000` です。`limit` パラメータ未指定時はデフォルトで `100` となります。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総件数を予測できないため、`meta.hasnext` フィールドで次ページの有無を示します。

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

データ変動が激しくページ番号方式が非効率な一部APIでは、カーソルページネーションを採用しています。

`position` または `cursor`（開始位置）パラメータで読み込み開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置からの件数を指定します。最大ページサイズは `10000` です。`limit` 未指定時はデフォルトで `100` です。

例：

```bash
GET /clients/{clientid}/mqueue_messages?position=1716187698257189921_0&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれ、`meta.position` または `meta.cursor` に次ページの開始位置が示されます。

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

この方式はデータ変動が激しい場合に効率的かつ連続的にデータ取得を行えます。

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
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています                   |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはキー＆シークレットが間違っています |
| BAD_REQUEST                                    | リクエストパラメータが不正です                               |
| NOT_MATCH                                      | 条件が一致しません                                           |
| ALREADY_EXISTS                                 | リソースが既に存在します                                     |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です                                         |
| BAD_LISTENER_ID                                | リスナーIDが不正です                                         |
| BAD_NODE_NAME                                  | ノード名が不正です                                           |
| BAD_RPC                                        | RPC失敗。クラスター状態や対象ノードの状態を確認してください。 |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル仕様に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成リソースが最大または最小制限を超えています               |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています         |
| CONFLICT                                       | リクエストリソースが競合しています                           |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使われていません         |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています                       |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージIDの解析エラー                                     |
| INVALID_ID                                     | 不正なIDスキーマ                                            |
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
| UPDATE_FAILED                                  | 更新失敗                                                   |
| REST_FAILED                                    | リセットソースまたは設定失敗                                 |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません                                   |
