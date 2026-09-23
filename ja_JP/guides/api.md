# REST API

EMQXはOpenAPI（Swagger）3.0仕様に準拠したHTTP管理APIを公開しています。

EMQXを起動後、[http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html) にアクセスすると、Swagger UIからAPIドキュメントの閲覧および管理APIの実行が可能です。デフォルトでは、ダッシュボード設定の下で `swagger_support` が `true` に設定されており、Swagger UIのサポートが有効になっています。これにより、インタラクティブなAPIドキュメントの生成など、Swagger関連の機能がすべて有効になります。この機能を無効にするには `false` に設定してください。詳細は[ダッシュボード設定](./configuration/dashboard.md)を参照してください。

本節では、EMQX REST APIの利用方法を紹介します。

## 基本パス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降のすべてのAPIパスは `/api/v5` から始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは、`Accept` ヘッダーに `application/json` を設定する必要があり、これによりレスポンスはJSON形式で返されます（特に指定がない限り）。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)標準に準拠しています。主なステータスコードは以下の通りです：

| コード | 説明                                                         |
| ------ | ------------------------------------------------------------ |
| 200    | リクエスト成功。返却されるJSONデータに詳細が含まれます。     |
| 201    | 作成成功。新規オブジェクトがBodyに返されます。               |
| 204    | リクエスト成功。通常は削除や更新操作に使われ、Bodyは空です。 |
| 400    | 不正なリクエスト。リクエストボディやパラメータのエラー。     |
| 401    | 認証エラー。APIキーの期限切れまたは存在しません。             |
| 403    | 禁止。オブジェクトが使用中または依存関係がある可能性があります。 |
| 404    | 見つかりません。Bodyの `message` フィールドで理由を確認可能。 |
| 409    | コンフリクト。オブジェクトが既に存在するか、数の上限を超過。   |
| 500    | サーバ内部エラー。Bodyやログで原因を確認してください。         |

## 認証

EMQXのREST APIは主に2つの認証方式をサポートしています：APIキーを用いたベーシック認証とベアラートークン認証です。

### APIキーを用いたベーシック認証

この方式では、APIキーとシークレットキーをユーザー名とパスワードとして使用し、APIリクエストを認証します。EMQXのREST APIは[HTTPベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを利用する前に、APIキーを作成する必要があります。詳細は[APIキー管理](#api-key-management)を参照してください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0以降はダッシュボードのユーザー認証情報をREST API認証に使用できません。代わりにAPIキーを作成して認証に使用してください。

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

APIキー認証の代替として、ベアラートークンを使用してEMQX REST APIに安全かつプログラム的にアクセスできます。ベアラートークンを取得するには、以下のログインAPIエンドポイントにリクエストを送信します。

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

#### ベアラートークンを使った認証

ベアラートークンを取得後、APIリクエストの `Authorization` ヘッダーに以下のように含めてください：

```bash
--header "Authorization: Bearer <your-token>"
```

## APIキー管理

このセクションでは、APIキーの作成および管理方法、ロール、ネームスペース、スコープの設定方法を説明します。

### APIキーの作成

#### ダッシュボード

ダッシュボードの **System** -> **API Keys** から手動でAPIキーを作成できます：

1. 右上の **+ Create** ボタンをクリックして作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **Name**（必須）：APIキーの名前を入力します。
   - **Expire At**：空欄の場合、キーは期限切れになりません。
   - **Is Enable**：デフォルトで有効です。
   - **Role**：ロールを選択します（任意）。詳細は[ロールと権限](#roles-and-permissions)を参照してください。
   - **Namespace**：デフォルトはオフです。グローバル管理者の場合はオフのままでグローバルAPIキーが作成されます。オンにしてネームスペースを選択すると、そのネームスペース内のキーが作成されます。ネームスペース管理者は自分のネームスペース内でのみキーを作成可能です。
   - **Permission Mode**：管理者またはビューアのキーの場合、スコープの割り当て方法を選択します。パブリッシャーキーには表示されず、ロールデフォルトの `publish` スコープが使用されます。スコープの動作と制限については[APIスコープ](#api-scopes)を参照してください。
     - **Role Default Scopes**：選択したロールのデフォルトを使用します。ロールデフォルトの変更は自動的に反映されます。
     - **System-level Permissions**：`system` スコープのみを付与します。
     - **Custom Restricted Permissions**：アクセス可能なAPI領域を制限するために1つ以上のスコープを選択します。**Scopes** を空欄にするとスコープ保護されたAPIにアクセスできません。
   - **Scopes**：**Custom Restricted Permissions** 選択時に表示され、付与するスコープを選択します。
   - **Note**：任意で説明を入力します。
3. **Confirm** をクリックすると、APIキーとシークレットキーが **Created Successfully** ダイアログに表示されます。

   ::: warning 重要

   APIキーとシークレットキーはこの時点で必ず保存してください。シークレットキーは再表示されません。

   :::

4. **Close** をクリックしてダイアログを閉じます。

**Permission Mode** はダッシュボードのみで利用可能です。REST APIを使用する場合は `scopes` フィールドを直接設定してください。詳細は[scopesのデフォルト動作](#default-behavior-of-scopes)を参照してください。

キーの詳細は名前をクリックして確認できます。**Edit** ボタンで有効期限、状態、ロール、権限モード、スコープ、説明を変更可能です。**Delete** ボタンでキーを削除できます。

#### REST API

ダッシュボードユーザーのベアラートークンを使ってREST API経由でAPIキーを作成・更新できます。APIキー管理エンドポイントはAPIキー認証を受け付けません。

EMQX 6.0.4以降、`POST /api/v5/api_key` および `PUT /api/v5/api_key/:name` のリクエストボディにトップレベルの `namespace` フィールドが追加されました。例えば、`team-a` ネームスペース内に管理者APIキーを作成するリクエスト例：

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

`scopes` に `"unset"` を指定するとロールデフォルトのスコープが明示的に適用されます。`scopes` を省略しても同様の効果です。

ネームスペースは以下のいずれかの方法で指定可能です：

- `administrator` のようなロールと `namespace` フィールドを併用する。
- ロールに `ns:<namespace>::<role>` の形式でネームスペースを埋め込む（例：`ns:team-a::administrator`）。

両方の形式がサポートされており、両方を含む場合はネームスペースが一致している必要があります。異なる場合や空の場合はHTTP 400が返されます。APIキー作成後にネームスペースを変更することはできません。

#### ブートストラップファイル

ブートストラップファイル方式でもAPIキーを作成可能です。以下の設定でファイルの場所を指定します：

```bash
api_key {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイルに複数のAPIキーを以下の形式で改行区切りで記述します：

```
{API Key}:{Secret Key}:{?Role}:{?Scopes}
```

- **API Key**：キー識別子として任意の文字列。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの[ロール](#roles-and-permissions)。ネームスペース付きキーは `ns:<namespace>::<role>` 形式（例：`ns:team-a::administrator`）。
- **Scopes（任意）**：キーに許可する[APIスコープ](#api-scopes)をカンマ区切りで指定。省略時はロールのデフォルトが適用されます。検証動作は[ブートストラップスコープの検証](#validate-bootstrap-scopes)を参照。

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

以下のスコープルールに違反するエントリは、EMQXが該当スコープを削除し警告ログを出力した上でキーを作成・更新します：

- **ログイン専用スコープ**：`user_management`、`mfa_management`、`sso_management`、`api_key_management` はAPIキーに無効です。EMQXはこれらを削除し、残りのスコープでキーを作成・更新します。
- **管理者相当スコープ**：APIキーに割り当て可能なスコープの中で、`system` のみが管理者相当権限を付与します。EMQX 6.0.4以降、管理者相当スコープと管理者相当でないスコープが混在する場合、管理者相当スコープをすべて削除し、残りのスコープを保持します。
- **ネームスペース付きスコープ**：EMQX 6.0.4以降、ネームスペース付きエントリがネームスペースロールで許可されていないスコープを明示的に指定した場合、許可されないスコープを削除し、残りを保持します。残るスコープがない場合、スコープ保護されたビジネスAPIにアクセスできません。許可スコープは[ネームスペース付き呼び出し元の制限](#restrictions-for-namespaced-callers)を参照。

##### ブートストラップAPIキーのリロード

この方法で作成されたAPIキーは無期限に有効です。

EMQX起動時にファイル内のデータをAPIキーリストに追加します。既存のAPIキーがある場合は、シークレットキー、ロール、スコープが更新されます。

### ネームスペース管理者によるAPIキー管理

EMQX 6.0.4以降、ネームスペース付きダッシュボード管理者は自身のネームスペース内でAPIキーを管理できます。認証にはベアラートークンが必要です。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| APIキー作成 | 管理者のネームスペース内でのみ作成可能。ネームスペース省略、グローバル指定、他ネームスペース指定はHTTP 403。 |
| APIキー一覧取得 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーは除外。 |
| APIキーの読み取り・更新・削除 | 管理者のネームスペース内のキーのみ操作可能。別ネームスペースのキーはHTTP 404で存在を非公開。 |
| APIキーのネームスペース変更 | 他ネームスペースへの移動不可。更新はHTTP 400。 |

グローバルダッシュボード管理者は引き続き全ネームスペースのAPIキーを管理可能です。

## APIキーの権限

### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つのプリセットロールのいずれかを割り当てられます：

- **Administrator**：すべてのリソースにアクセス可能。指定がなければデフォルト。ロール識別子は `administrator`。
- **Viewer**：リソースやデータの閲覧のみ可能。REST APIのGETリクエスト全般に対応。ロール識別子は `viewer`。
- **Publisher**：MQTTメッセージのパブリッシュ専用。メッセージパブリッシュ関連APIのみアクセス可能。ロール識別子は `publisher`。

::: tip 注意
`publisher` キーは `publish` スコープのみ許容します。スコープ割り当て時に `publish` 以外が含まれるとHTTP 400が返されます。キーのロールを `publisher` に変更する場合は、同時に `"scopes": ["publish"]` または空リストを含めてください。既存スコープに `publish` 以外があるとリクエストは拒否されます。
:::

### APIスコープ

スコープはキーごとの権限の次元であり、REST APIのどのビジネス領域にアクセス可能かを宣言します。スコープと[ロールと権限](#roles-and-permissions)は独立しており、両方のチェックを通過した場合にのみアクセスが許可される2層のアクセス制御を形成します：

| 次元 | 目的 | 粒度 |
| ---- | ---- | ---- |
| **ロール** | HTTPメソッドの制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | APIドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方で検証されます。両方を満たす場合にのみリクエストが受け入れられます。

マイクロサービスや統合シナリオでは、外部システムは通常、EMQXの管理機能の一部のみを必要とします。例えば、監視プラットフォームは `monitoring` スコープのみ、ルールパブリッシュサービスは `data_integration` のみ、クラスター運用ツールは `cluster_operations` のみが必要です。スコープを使うことで最小権限の原則を適用し、キー漏洩時の影響範囲を最小化できます。

::: tip
スコープ名はEMQXのアップグレード間で安定した識別子です。OpenAPIタグ名が変更されても、同じスコープを持つキーは引き続き機能します。
:::

#### 組み込みAPIキー用スコープ

EMQXはAPIキー用に以下10のスコープを提供しています：

| スコープ | 名称 | 典型的なAPI領域 |
| --- | --- | --- |
| `connections` | 接続管理 | `/clients`, `/subscriptions`, `/topics`, `/banned`, `/retainer`, `/file_transfer`, `/mqtt/delayed`, `/mqtt/topic_rewrite`, ... |
| `publish` | メッセージパブリッシュ | `/publish`, `/publish/bulk` |
| `data_integration` | データ統合 | `/rules`, `/connectors`, `/actions`, `/schema_registry`, `/schema_validations`, `/message_transformations`, `/exhooks`, `/ai/*` |
| `access_control` | アクセス制御 | `/authentication`, `/authorization/*` |
| `gateways` | プロトコルゲートウェイ | `/gateways`, `/coap/*`, `/lwm2m/*`, `/gcp_devices`, ... |
| `monitoring` | 監視データ | `/metrics`, `/stats`, `/monitor*`, `/alarms`, `/trace`, `/slow_subscriptions`, `/telemetry`, `/prometheus/{auth,stats,data_integration,...}`, ... |
| `cluster_operations` | クラスター運用 | `/cluster*`, `/nodes`, `/load_rebalance`, `/node_eviction`, `/mt/*`, ... |
| `system` | システム設定 | `/configs*`, `/listeners*`, `/plugins*`, `/ds/*`, `/data/*`, `/status`, `/relup`, `/opentelemetry*`, `/prometheus`, ... |
| `audit` | 監査ログ | `/audit` |
| `license` | ライセンス | `/license*` |

::: tip 注意

EMQX 6.0.4以降、`audit` スコープはネームスペース付き呼び出し元に監査ログアクセスを付与しません。`GET /api/v5/audit` はグローバル管理者およびグローバルビューアのみが呼び出せます。詳細は[監査ログアクセス](./dashboard/audit-log.md#audit-log-access)を参照してください。

:::

::: warning 管理者相当スコープと制限付きスコープの混在禁止

EMQXは `system`、`user_management`、`api_key_management`、`sso_management` を管理者相当スコープ（検証メッセージでは `privilege scopes`）として分類しています。これらを制限付きスコープと混在させると、アカウントの実効権限は減少しません。4つのうちAPIキーに割り当て可能なのは `system` のみで、他3つは[ログイン専用スコープ](#login-only-scopes)に該当します。

そのため、EMQX 6.0.4以降、APIキー作成・更新時の明示的スコープリストは `system` のみか、`system` を含まないスコープのいずれかでなければなりません。混在リストはHTTP 400で拒否され、変更は適用されません。

既存の混在スコープリストは引き続き有効で `system` は有効なままです。次回の明示的スコープ更新は `system` のみか、`system` を含まないリストで行う必要があります。ダッシュボードで編集時は保存前に権限モードの選択を促されます。

:::

#### ログイン専用スコープ

APIキー用スコープに加え、ダッシュボードログインユーザーには4つのログイン専用スコープがあり、ブラウザセッションにのみ適用されAPIキーには割り当てられません。ログインユーザーのスコープ割り当てと適用については[ログインユーザースコープ](./dashboard/system.md#login-user-scopes)を参照してください。

| スコープ | 必要ロール | 目的 |
| --- | --- | --- |
| `user_management` | Administrator | ダッシュボードユーザー管理。 |
| `sso_management` | Administrator | SSOバックエンドおよびSSOユーザーレコード管理。 |
| `api_key_management` | Administrator | APIキー管理。 |
| `mfa_management` | 任意 | 自身のMFA管理。管理者は他ユーザーのMFAも管理可能。 |

#### `scopes` のデフォルト動作

EMQX 6.0.4以降、APIキーの `scopes` フィールドは以下のルールに従います：

| `scopes` の値 | 意味 |
| --- | --- |
| **作成リクエストで省略** | 選択したロールのデフォルトを使用。 |
| **更新リクエストで省略** | キーの現在のスコープ設定を維持。 |
| **ロールデフォルトのセントネル `"unset"`** | 明示的スコープ設定を削除し、ロールデフォルトを使用。ロールデフォルトの変更は自動反映。 |
| **空リスト `[]`** | すべてのビジネスエンドポイントを拒否。キーの一時無効化に有用。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定されたスコープのみ許可。 |

明示的リストがロールデフォルトと同じスコープセットの場合、`"unset"` と同じ効果であり、ロールデフォルトの変更を追従します。比較は順序に依存しません。

ブートストラップファイルのエントリでスコープセグメントを省略すると、指定ロールのデフォルトが適用されます。

スコープはキーがアクセス可能なAPI領域を決定し、ロールやネームスペース制限を上書きしません。リクエストはロール、スコープ、ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能なスコープの一覧取得

EMQXは利用可能なスコープカタログを取得するための2つのエンドポイントを公開しています：

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記10のビジネスドメインスコープ）を返します。APIキー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（ログイン専用4スコープ含む）を返します。ベアラートークン認証が必要です。

スコープ選択UIの構築や自動化スクリプトの検証に利用してください：

```bash
# APIキー用スコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの方法で設定可能です：

- **ダッシュボード**：**System** -> **API Keys** でキー作成・編集時に **Permission Mode** を選択。**Custom Restricted Permissions** の場合に個別スコープを選択。
- **REST API**：作成・更新リクエストボディに `"scopes": ["monitoring", "cluster_operations"]` を含める。
- **ブートストラップファイル**：各行の4番目のセグメントにカンマ区切りでスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ネームスペース付き呼び出し元の制限

ネームスペース付き呼び出し元（ロールが特定ネームスペースに制限されたユーザーやAPIキー）は、スコープチェックに加えてエンドポイントレベルの追加制限を受けます。スコープ付与はこれらの制限を上書きしません。

### ネームスペース付きAPIキーのスコープ制限

EMQX 6.0.4以降、ネームスペース付き管理者APIキーのロールデフォルトスコープは `connections`、`monitoring`、`data_integration`、`access_control`、`system`、`cluster_operations`、`license` です。`publish`、`gateways`、`audit` は含まれません。

ネームスペース付きAPIキー作成時や既存キーの明示的スコープリスト変更時は、ネームスペースロールで許可されたスコープのみ割り当て可能です。`publish`、`gateways`、`audit`、その他許可されていないスコープを指定するとHTTP 400が返され、変更は適用されません。`system` と制限付きスコープの混在禁止も明示的スコープリストに適用されます。

### 許可されていないスコープを含む既存キー

保存済みスコープリストに許可されていないスコープが含まれるキーは自動的に変更されません。読み取り-修正-書き込みクライアントとの互換性のため、更新時に同じスコープリストを再送信し、ロールとネームスペースが同じなら許容されます。ただし、スコープリストが `publish` のみのネームスペース付きキーはAPIアクセス不可のため、変更なし更新でもHTTP 400が返されます。この場合はキーを削除し、ネームスペースなしで再作成してください。実際のロールやスコープ変更は再検証され、許可リストに準拠する必要があります。

ネームスペース付きAPIキーの更新やローテーションは、以前の権限がキーのローテーションまで有効であるため、ネームスペースエンドポイント制限の対象となります。ブートストラップエントリ再処理時は許可されないスコープを削除し警告ログを出し、残りのスコープを保持します。詳細は[ブートストラップスコープの検証](#validate-bootstrap-scopes)を参照してください。

### メッセージパブリッシュの制限

ネームスペース付きAPIキーはメッセージパブリッシュAPI（`POST /api/v5/publish` など）を呼び出せません。以前のスコープリストに `publish` が含まれていても、スコープ割り当てはネームスペースレベルの制限を上書きしません。

### メッセージ内容の制限

ネームスペース付き呼び出し元が `connections` または `monitoring` スコープを持っていても、クラスター全体のMQTTメッセージ内容（保持メッセージや遅延メッセージストア）を読み書きするエンドポイントにはアクセスできません。以下のメッセージ関連エンドポイントは `403 Forbidden` を返します：

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

### トレースの制限

トレース操作では、`GET /trace` は呼び出し元のネームスペース内のトレースのみを一覧表示します。以下のトレース単位操作は、異なるネームスペースのトレースに対して `404 Not Found` を返します：

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この動作により他ネームスペースのトレースの存在が漏れません。バルク削除操作（`DELETE /trace`）はネームスペース付き呼び出し元に対して `403 Forbidden` を返し、全トレースのクリアはグローバル管理者のみ可能です。

ダッシュボードログイン、SSOコールバック、APIキーの自己管理エンドポイント（例：`/api_key`）は、キーの `scopes` 設定に関わらずAPIキー認証を受け付けません。これはスコープモデルとは無関係のダッシュボードのセキュリティ境界です。

## ページネーション

大量データを扱う一部APIではページネーション機能が提供されています。データの特性に応じて2種類のページネーション方式があります。

### ページ番号ページネーション

ページネーション対応APIの多くは、`page`（ページ番号）と `limit`（ページサイズ）パラメータで制御可能です。最大ページサイズは `10000` です。`limit` を指定しない場合はデフォルトで `100` となります。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総件数を予測できないため、`meta.hasnext` で次ページの有無を示します：

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

`position` または `cursor`（開始位置）パラメータで取得開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置からの件数を指定します。最大ページサイズは `10000` です。`limit` 未指定時はデフォルトで `100` です。

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

この方式はデータ変動が激しいシナリオで連続性と効率性を確保します。

## エラーコード

HTTPレスポンスステータスコードに加え、EMQXは特定エラーを識別するためのエラーコード一覧を定義しています。

エラー発生時はBodyにJSON形式でエラーコードが返されます：

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| エラーコード                                    | 説明                                                         |
| ---------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています。                 |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワード、またはAPIキー＆シークレットが間違っています。 |
| BAD_REQUEST                                    | リクエストパラメータが不正です。                             |
| NOT_MATCH                                      | 条件が一致しません。                                         |
| ALREADY_EXISTS                                 | リソースが既に存在します。                                   |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です。                                       |
| BAD_LISTENER_ID                                | リスナーIDが不正です。                                       |
| BAD_NODE_NAME                                  | ノード名が不正です。                                         |
| BAD_RPC                                        | RPC失敗。クラスター状態および対象ノード状態を確認してください。 |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成しようとしたリソースが最大または最小制限を超えています。   |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています。       |
| CONFLICT                                       | リクエストリソースに競合があります。                         |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使用されていません。     |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています。                     |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージIDの解析エラー。                                   |
| INVALID_ID                                     | IDスキーマが不正です。                                       |
| MESSAGE_ID_NOT_FOUND                           | メッセージIDが存在しません。                                 |
| NOT_FOUND                                      | リソースが見つかりません。                                   |
| CLIENTID_NOT_FOUND                             | クライアントIDが見つかりません。                             |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常はMQTTクライアントではありません）。 |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません。                                   |
| TOPIC_NOT_FOUND                                | トピックが見つかりません。                                   |
| USER_NOT_FOUND                                 | ユーザーが見つかりません。                                   |
| INTERNAL_ERROR                                 | サーバ内部エラーです。                                       |
| SERVICE_UNAVAILABLE                            | サービスが利用できません。                                   |
| SOURCE_ERROR                                   | ソースエラーです。                                           |
| UPDATE_FAILED                                  | 更新に失敗しました。                                         |
| REST_FAILED                                    | ソースまたは設定のリセットに失敗しました。                   |
| CLIENT_NOT_RESPONSE                            | クライアントが応答していません。                             |
