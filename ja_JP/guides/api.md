# REST API

EMQXはOpenAPI（Swagger）3.0仕様に基づいて設計されたHTTP管理APIを公開しています。

EMQXを起動後、[http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html) にアクセスすると、APIドキュメントを閲覧でき、Swagger UIから管理APIを実行できます。デフォルトでは、ダッシュボード設定の下で `swagger_support` が `true` に設定されており、Swagger UIのサポートが有効であることを示しています。これにより、インタラクティブなAPIドキュメントの生成など、Swagger関連の機能がすべて有効になります。この機能を無効にするには `false` に設定してください。詳細は[ダッシュボード設定](./configuration/dashboard.md)を参照してください。

本節では、EMQX REST APIの利用方法について説明します。

## 基本パス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降のすべてのAPIパスは `/api/v5` で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは、`Accept` ヘッダーを `application/json` に設定する必要があります。これにより、レスポンスは特に指定がない限りJSON形式で返されます。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)の標準に従っています。主なステータスコードは以下の通りです：

| コード | 説明                                                         |
| ------ | ------------------------------------------------------------ |
| 200    | リクエスト成功。返却されるJSONデータに詳細が含まれます。     |
| 201    | 作成成功。新規オブジェクトがBodyに返されます。               |
| 204    | リクエスト成功。通常は削除や更新操作で返却Bodyは空です。     |
| 400    | 不正なリクエスト。リクエストボディやパラメータのエラー。     |
| 401    | 認証失敗。APIキーが期限切れか存在しません。                   |
| 403    | 禁止。オブジェクトが使用中か依存関係の制約があります。       |
| 404    | 見つかりません。Bodyの `message` フィールドで理由を確認可能。 |
| 409    | 競合。オブジェクトが既に存在するか数の上限を超過。           |
| 500    | サーバ内部エラー。Bodyやログで原因を確認してください。        |

## 認証

EMQXのREST APIは主に2つの認証方式をサポートしています：APIキーによるベーシック認証とベアラートークン認証です。

### APIキーによるベーシック認証

この方式では、APIキーとシークレットキーをユーザー名とパスワードとして使用し、APIリクエストを認証します。EMQXのREST APIは[HTTPベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを使用する前にAPIキーを作成する必要があります。詳細は[APIキー管理](#apiキー管理)を参照してください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0以降はダッシュボードのユーザー認証情報をREST APIの認証に使用できません。代わりにAPIキーを作成し、認証に使用してください。

:::

#### APIキーでの認証例

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

APIキー認証の代わりに、ベアラートークンを使用してEMQX REST APIに安全かつプログラム的にアクセスできます。ベアラートークンを取得するには、以下のログインAPIエンドポイントにリクエストを送信します。

#### ベアラートークンの取得

ベアラートークンを取得するには、以下のログインAPIエンドポイントにHTTP `POST` リクエストを送信します：

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

レスポンスにはベアラートークンが含まれ、APIリクエストの認証に使用できます。

#### ベアラートークンの利用方法

ベアラートークンを取得したら、APIリクエストの `Authorization` ヘッダーに以下のように含めてください：

```bash
--header "Authorization: Bearer <your-token>"
```

## APIキー管理

このセクションでは、APIキーの作成・管理方法およびロール、ネームスペース、スコープの設定方法を説明します。

### APIキーの作成

#### ダッシュボード

ダッシュボードの **System** -> **API Keys** から手動でAPIキーを作成できます：

1. 右上の **+ Create** ボタンをクリックし、作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **Name**（必須）：APIキーの名前を入力します。
   - **Expire At**：空欄の場合は期限なしとなります。
   - **Is Enable**：デフォルトで有効です。
   - **Role**：ロールを選択（任意）。詳細は[ロールと権限](#roles-and-permissions)を参照してください。
   - **Namespace**：デフォルトはオフ。グローバル管理者の場合はオフのままでグローバルAPIキーが作成されます。オンにしてネームスペースを選択すると、そのネームスペース内のキーが作成されます。ネームスペース管理者は自分のネームスペース内でのみキーを作成可能です。
   - **Permission Mode**：管理者またはビューアのキーの場合、スコープの割り当て方法を選択します。パブリッシャーキーには表示されません。スコープの動作と制限については[APIスコープ](#api-scopes)を参照してください。
     - **Role Default Scopes**：選択したロールのデフォルトを使用します。ロールのデフォルト変更は自動的に反映されます。
     - **System-level Permissions**：`system` スコープのみを付与します。
     - **Custom Restricted Permissions**：アクセス可能なAPI領域を制限するスコープを1つ以上選択します。**Scopes**を空欄にするとスコープ保護されたAPIにアクセスできません。
   - **Scopes**：**Custom Restricted Permissions** 選択時に表示され、付与するスコープを選択します。
   - **Note**：任意で説明を入力できます。
3. **Confirm** をクリックすると、APIキーとシークレットキーが「作成成功」ダイアログに表示されます。

   ::: warning 重要

   APIキーとシークレットキーは必ずすぐに保存してください。シークレットキーは再表示されません。

   :::

4. **Close** をクリックしてダイアログを閉じます。

**Permission Mode** はダッシュボードのみで利用可能です。REST APIを使う場合は、`scopes` フィールドを直接設定してください。詳細は[scopesのデフォルト動作](#default-behavior-of-scopes)を参照してください。

キー名をクリックすると詳細を確認でき、**Edit** ボタンで有効期限、状態、ロール、パーミッションモード、スコープ、説明を変更できます。**Delete** ボタンでキーを削除できます。

#### REST API

REST API経由でAPIキーを作成・更新するには、ダッシュボードユーザーのベアラートークンを使用します。APIキー管理のエンドポイントはAPIキー認証を受け付けません。

EMQX 6.0.4以降、`POST /api/v5/api_key` および `PUT /api/v5/api_key/:name` のリクエストボディにトップレベルの `namespace` フィールドが追加されました。例として、`team-a` ネームスペースに管理者APIキーを作成するリクエスト：

```bash
curl -X POST "http://localhost:18083/api/v5/api_key" \
  -H "Authorization: Bearer <your-token>" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "team-a-key",
    "role": "administrator",
    "namespace": "team-a",
    "scopes": "unset"
  }'
```

`scopes` に `"unset"` を指定するとロールデフォルトのスコープが明示的に適用されます。作成リクエストで `scopes` を省略しても同様の効果です。

ネームスペースは以下のいずれかの方法で指定可能です：

- `administrator` のようなロールと `namespace` フィールドを併用する。
- `ns:<namespace>::<role>` の形式でロールにネームスペースを含める（例：`ns:team-a::administrator`）。

両形式は引き続きサポートされます。両方が同時に含まれる場合はネームスペースが一致する必要があります。不一致や空の場合はHTTP 400が返されます。APIキー作成後はネームスペースの変更はできません。

#### ブートストラップファイル

ブートストラップファイルを使ってAPIキーを作成することも可能です。以下の設定でファイルの場所を指定します：

```bash
api_key {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイルには複数のAPIキーを `{API Key}:{Secret Key}:{?Role}:{?Scopes}` 形式で改行区切りで記述します：

- **API Key**：任意の文字列でキー識別子。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの[ロール](#roles-and-permissions)。ネームスペースキーの場合は `ns:<namespace>::<role>` 形式（例：`ns:team-a::administrator`）。
- **Scopes（任意）**：キーがアクセス可能な[APIスコープ](#api-scopes)をカンマ区切りで指定。省略時はロールのデフォルトが適用されます。検証動作は[ブートストラップスコープの検証](#validate-bootstrap-scopes)を参照。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
team-a-ops:8d4f2a7c1e6b9035:ns:team-a::administrator:connections,monitoring
```

##### ブートストラップスコープの検証

ブートストラップエントリが以下のスコープルールに違反した場合、EMQXは該当スコープを削除し、警告ログを出力した上でキーの作成・更新を続行します：

- **ログイン専用スコープ**：`user_management`、`mfa_management`、`sso_management`、`api_key_management` はAPIキーに対して無効です。EMQXはこれらを削除し、残りのスコープでキーを作成・更新します。
- **管理者相当スコープ**：APIキーに割り当て可能なスコープの中で、`system` のみが管理者相当権限を付与します。EMQX 6.0.4以降、管理者相当スコープと管理者相当でないスコープが混在する場合、管理者相当スコープをすべて削除し、残りのスコープを保持します。
- **ネームスペーススコープ**：EMQX 6.0.4以降、ネームスペースロールが保持できないスコープを明示的に指定した場合、EMQXは許可されないスコープを削除し、残りを保持します。残るスコープがない場合、スコープ保護されたビジネスAPIにアクセスできません。許可されるスコープは[ネームスペース呼び出し元の制限](#restrictions-for-namespaced-callers)を参照してください。

##### ブートストラップAPIキーのリロード

この方法で作成されたAPIキーは無期限に有効です。

EMQX起動時にファイルの内容がAPIキーリストに追加されます。既存のAPIキーがあれば、シークレットキー、ロール、スコープが更新されます。

### ネームスペース管理者によるAPIキー管理

EMQX 6.0.4以降、ネームスペースダッシュボード管理者は自分のネームスペース内のAPIキーを管理可能です。認証にはベアラートークンが必要です。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| APIキー作成 | 管理者のネームスペース内でのみ作成可能。ネームスペース省略、グローバル指定、他ネームスペース指定はHTTP 403を返す。 |
| APIキー一覧取得 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーはレスポンスから除外。 |
| APIキーの読み取り・更新・削除 | 管理者のネームスペース内のキーのみ操作可能。他ネームスペースのキーはHTTP 404を返し存在を隠蔽。 |
| APIキーのネームスペース変更 | 他ネームスペースへの移動不可。更新はHTTP 400を返す。 |

グローバルダッシュボード管理者は引き続き全ネームスペースのAPIキーを管理可能です。

## APIキーの権限

### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つのプリセットロールのいずれかを割り当てられます：

- **Administrator**：すべてのリソースにアクセス可能。ロール指定がない場合のデフォルト。ロール識別子は `administrator`。
- **Viewer**：リソースやデータの閲覧のみ可能。REST APIのすべてのGETリクエストに対応。ロール識別子は `viewer`。
- **Publisher**：MQTTメッセージのパブリッシュ専用に設計されたロール。メッセージパブリッシュ関連APIのみアクセス可能。ロール識別子は `publisher`。

::: tip 注意
`publisher` キーは `publish` スコープのみ受け入れます。スコープ割り当て時に `publish` 以外のスコープを指定するとHTTP 400が返されます。キーのロールを `publisher` に変更する場合は、同時リクエストに `"scopes": ["publish"]` または空リストを含めてください。既存スコープに `publish` 以外がある場合はリクエストが拒否されます。
:::

### APIスコープ

スコープはキーごとの権限次元であり、REST APIのどのビジネス領域にアクセス可能かを宣言します。スコープと[ロールと権限](#roles-and-permissions)は独立しており、両方が適用されることで2層のアクセス制御を形成します：

| 次元 | 目的 | 粒度 |
| ---- | ---- | ---- |
| **ロール** | HTTPメソッドの制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | APIドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方を通過した場合にのみ許可されます。

マイクロサービスや統合シナリオでは、外部システムがEMQX管理面の一部のみを必要とすることが多いです。例えば監視プラットフォームは `monitoring` スコープのみ、ルールパブリッシュサービスは `data_integration` のみ、クラスター運用ツールは `cluster_operations` のみ必要です。スコープにより最小権限の原則でキーを割り当て、キー漏洩時の影響範囲を最小化できます。

::: tip
スコープ名はEMQXのアップグレード間で変更されない安定した識別子です。OpenAPIタグが変更されても、同じスコープ設定のキーは引き続き動作します。
:::

#### 組み込みAPIキー用スコープ

EMQXはAPIキー用に以下10種類のスコープを提供しています：

| スコープ | 名称 | 代表的なAPI領域 |
| -------- | ---- | --------------- |
| `connections` | 接続管理 | `/clients`, `/subscriptions`, `/topics`, `/banned`, `/retainer`, `/file_transfer`, `/mqtt/delayed`, `/mqtt/topic_rewrite` など |
| `publish` | メッセージパブリッシュ | `/publish`, `/publish/bulk` |
| `data_integration` | データ統合 | `/rules`, `/connectors`, `/actions`, `/schema_registry`, `/schema_validations`, `/message_transformations`, `/exhooks`, `/ai/*` |
| `access_control` | アクセス制御 | `/authentication`, `/authorization/*` |
| `gateways` | プロトコルゲートウェイ | `/gateways`, `/coap/*`, `/lwm2m/*`, `/gcp_devices` など |
| `monitoring` | 監視データ | `/metrics`, `/stats`, `/monitor*`, `/alarms`, `/trace`, `/slow_subscriptions`, `/telemetry`, `/prometheus/{auth,stats,data_integration,...}` など |
| `cluster_operations` | クラスター操作 | `/cluster*`, `/nodes`, `/load_rebalance`, `/node_eviction`, `/mt/*` など |
| `system` | システム設定 | `/configs*`, `/listeners*`, `/plugins*`, `/ds/*`, `/data/*`, `/status`, `/relup`, `/opentelemetry*`, `/prometheus` など |
| `audit` | 監査ログ | `/audit` |
| `license` | ライセンス | `/license*` |

::: tip 注意

EMQX 6.0.4以降、`audit` スコープはネームスペース呼び出し元に監査ログアクセスを付与しません。`GET /api/v5/audit` はグローバル管理者およびグローバルビューアのみ呼び出せます。詳細は[監査ログアクセス](./dashboard/audit-log.md#audit-log-access)を参照してください。

:::

::: warning 管理者相当スコープと制限付きスコープの混在禁止

EMQXは `system`、`user_management`、`api_key_management`、`sso_management` を管理者相当スコープ（検証メッセージでは `privilege scopes`）として分類しています。これらを制限付きスコープと混在させるとアカウントの実効権限は減りません。4つのうちAPIキーに割り当て可能なのは `system` のみで、他3つは[ログイン専用スコープ](#login-only-scopes)に該当します。

そのためEMQX 6.0.4以降、APIキー作成・更新時の明示的なスコープリストは `system` のみ、または `system` を含まないスコープ群のどちらかでなければなりません。混在リストはHTTP 400を返し変更は適用されません。

既存の混在スコープリストは引き続き有効で `system` は有効なままです。次回の明示的なスコープ更新は `system` のみ、または `system` を含まないリストでなければなりません。ダッシュボードで編集時は保存前にパーミッションモードの選択を促されます。

:::

#### ログイン専用スコープ

APIキー用スコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、APIキーには割り当てできません。ログインユーザーへの割り当てと適用方法は[ログインユーザースコープ](./dashboard/system.md#login-user-scopes)を参照してください。

| スコープ | 必要ロール | 目的 |
| -------- | ---------- | ---- |
| `user_management` | 管理者 | ダッシュボードユーザー管理 |
| `sso_management` | 管理者 | SSOバックエンドおよびSSOユーザーレコード管理 |
| `api_key_management` | 管理者 | APIキー管理 |
| `mfa_management` | グローバル管理者またはグローバルビューア | 自身のMFA管理。管理者は他ユーザーのMFAも管理可能 |

#### `scopes` のデフォルト動作

EMQX 6.0.4以降、APIキーの `scopes` フィールドは以下のルールに従います：

| `scopes` の値 | 意味 |
| ------------- | ---- |
| **作成リクエストで未指定** | 選択したロールのデフォルトを使用 |
| **更新リクエストで未指定** | キーの現在のスコープ設定を保持 |
| **未設定のセントネル `"unset"`** | 明示的なスコープ設定を解除。後方互換のため、EMQXはスコープ許可リストを適用しません。ロール、ネームスペース、APIキー固有のパス制限は適用されます。 |
| **空リスト `[]`** | すべてのビジネスエンドポイントを拒否。キーを無効化するソフトな方法として有用。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定されたスコープのみ許可 |

ロールデフォルトと同じスコープセットの明示的リストは `"unset"` に正規化され、同様の動作となります。比較は順序に依存しません。

ブートストラップファイルのエントリでスコープセグメントを省略すると、EMQXは処理時に指定ロールのデフォルトを適用します。

スコープはキーがアクセス可能なAPI領域を決定します。ロールやネームスペース制限を上書きしません。リクエストはロール、スコープ、ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能なスコープ一覧の取得

EMQXは利用可能なスコープカタログを問い合わせるための2つのエンドポイントを公開しています：

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記10種のビジネスドメインスコープ）を返します。APIキー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（ログイン専用4スコープ含む）を返します。ベアラートークン認証が必要です。

スコープ選択UIの構築や自動化スクリプトの検証に利用してください：

```bash
# APIキー用スコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの入口から設定可能です：

- **ダッシュボード**：**System** -> **API Keys** でキー作成・編集時に **Permission Mode** を選択。**Custom Restricted Permissions** の場合のみ個別スコープを選択。
- **REST API**：作成・更新リクエストボディに `"scopes": ["monitoring", "cluster_operations"]` を含める。
- **ブートストラップファイル**：各行の4番目のセグメントにカンマ区切りスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ネームスペース呼び出し元の制限

ネームスペース呼び出し元（ロールが特定ネームスペースに制限されたユーザーまたはAPIキー）は、スコープチェックに加えて追加のエンドポイントレベル制限を受けます。スコープ付与はこれらの制限を上書きしません。

### ネームスペースAPIキーのスコープ制限

EMQX 6.0.4以降、作成リクエストで `scopes` を省略した場合、ネームスペースの管理者またはビューアロールのAPIキーは以下7つのスコープが付与されます：`connections`、`monitoring`、`data_integration`、`access_control`、`system`、`cluster_operations`、`license`。`publish`、`gateways`、`audit` は含まれません。

ネームスペースAPIキーの管理者またはビューアロール作成時や既存キーの明示的スコープリスト変更時は、リクエストにこれら7つのスコープのみ含めることができます。`publish`、`gateways`、`audit` などネームスペースロールが持てないスコープを指定するとHTTP 400が返され、禁止スコープが特定されて変更は適用されません。`system` と制限付きスコープの混在禁止も明示的スコープリストに適用されます。

### 禁止スコープを含む既存キー

禁止スコープを含む既存キーは自動的に変更されません。読み取り・修正・書き込みクライアントの互換性のため、更新時に同じスコープリストを再送信し、ロールとネームスペースが同じなら受け入れられます。ただし、`publish` のみを含むネームスペースキーはAPIアクセス不可のため、同じ内容の更新でもHTTP 400が返されます。この場合はキーを削除し、ネームスペースなしで再作成してください。実際のロールやスコープ変更は再検証され、許可リストに準拠する必要があります。

禁止スコープを含むネームスペースAPIキーは、キーのローテーションまで以前の権限が有効です。ただし、ネームスペースのエンドポイント制限は適用されます。ブートストラップエントリ再処理時は禁止スコープが削除され、警告ログが出力され、残りのスコープが保持されます。詳細は[ブートストラップスコープの検証](#validate-bootstrap-scopes)を参照してください。

### メッセージパブリッシュの制限

ネームスペースAPIキーはメッセージパブリッシュAPI（`POST /api/v5/publish` など）を呼び出せません。以前のスコープリストに `publish` が含まれていても、ネームスペースレベルの制限が優先されます。

### メッセージコンテンツの制限

ネームスペース呼び出し元が `connections` または `monitoring` スコープを持っていても、クラスター全体の生のMQTTメッセージコンテンツ（保持メッセージや遅延メッセージストアを含む）を読み書きするエンドポイントにはアクセスできません。以下のメッセージ関連エンドポイントは `403 Forbidden` を返します：

- `GET /clients/:clientid/mqueue_messages`
- `GET /clients/:clientid/inflight_messages`
- `GET /mqtt/retainer/messages`
- `GET /mqtt/retainer/message/:topic`
- `DELETE /mqtt/retainer/message/:topic`
- `DELETE /mqtt/retainer/messages`
- `GET /mqtt/delayed/messages`
- `GET /mqtt/delayed/messages/:node/:msgid`
- `DELETE /mqtt/delayed/messages/:node/:msgid`
- `DELETE /mqtt/delayed/messages/:topic`

### ファイル転送の制限

ファイル転送ストアはグローバルでネームスペース非対応です。ネームスペース呼び出し元は以下のファイル転送コンテンツエンドポイントにアクセスできず、スコープ付与もこの制限を上書きしません：

- `GET /file_transfer/files`
- `GET /file_transfer/files/:clientid/:fileid`
- `GET /file_transfer/file`

グローバル呼び出し元はロールとスコープに応じてこれらのエンドポイントにアクセス可能です。`/file_transfer` 設定エンドポイントは影響を受けません。

### トレースの制限

トレース操作では、`GET /trace` は呼び出し元のネームスペース内のトレースのみを一覧表示します。以下のトレース単位操作はトレースが他ネームスペースの場合 `404 Not Found` を返し、存在を隠蔽します：

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この動作により他ネームスペースのトレース情報の漏洩を防止します。トレース一括削除（`DELETE /trace`）はネームスペース呼び出し元に対して `403 Forbidden` を返し、全トレースのクリアはグローバル管理者のみ可能です。

ダッシュボードログイン、SSOコールバック、APIキーの自己管理エンドポイント（例：`/api_key`）は、キーの `scopes` 設定に関わらずAPIキー認証を受け付けません。これはスコープモデルとは無関係なダッシュボードのセキュリティ境界です。

## ページネーション

大量データを扱う一部APIではページネーション機能が提供されています。データ特性に応じて2種類のページネーション方式があります。

### ページ番号ページネーション

ページネーション対応APIの多くは、`page`（ページ番号）と `limit`（ページサイズ）パラメータで制御可能です。最大ページサイズは `10000` です。`limit` 未指定時はデフォルトで `100` となります。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総データ数を予測できないため、`meta.hasnext` フィールドで次ページの有無を示します：

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

### カーソルページネーション

データ変動が激しくページ番号ページネーションが非効率な一部APIではカーソルページネーションを採用しています。

`position` または `cursor`（開始位置）パラメータで読み込み開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置からの件数を指定します。最大ページサイズは `10000` で、未指定時は `100` です。

例：

```bash
GET /clients/{clientid}/mqueue_messages?position=1716187698257189921_0&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれ、`meta.position` または `meta.cursor` に次ページの開始位置が示されます：

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

この方式はデータ変動が激しいシナリオでの連続性と効率的なデータ取得を実現します。

## エラーコード

HTTPレスポンスステータスコードに加え、EMQXは特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時は、BodyにJSON形式でエラーコードが返されます：

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| エラーコード                                   | 説明                                                         |
| ---------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています <img width=200/>  |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはキー＆シークレットが間違っています |
| BAD_REQUEST                                    | リクエストパラメータが不正です                               |
| NOT_MATCH                                      | 条件が一致しません                                           |
| ALREADY_EXISTS                                 | リソースが既に存在します                                     |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です                                         |
| BAD_LISTENER_ID                                | リスナーIDが不正です                                         |
| BAD_NODE_NAME                                  | ノード名が不正です                                           |
| BAD_RPC                                        | RPC失敗。クラスター状態と対象ノードの状態を確認してください。   |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成リソースが最大または最小制限を超えています               |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています         |
| CONFLICT                                       | リクエストリソースが競合しています                           |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使用されていません       |
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
| UPDATE_FAILED                                  | 更新失敗                                                   |
| REST_FAILED                                    | リセットソースまたは設定失敗                                 |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません                                   |
