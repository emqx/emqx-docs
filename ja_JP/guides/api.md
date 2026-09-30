# REST API

EMQX は、OpenAPI (Swagger) 3.0 仕様に準拠した HTTP 管理 API を公開しています。

EMQX を起動後、[http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html) にアクセスすると、API ドキュメントを閲覧でき、Swagger UI から管理 API を実行できます。デフォルトでは、ダッシュボード設定の下で `swagger_support` が `true` に設定されており、Swagger UI サポートが有効になっています。これにより、インタラクティブな API ドキュメントの生成など、Swagger 関連の機能がすべて有効になります。`false` に設定すると、この機能を無効化できます。詳細は [ダッシュボード設定](./configuration/dashboard.md) を参照してください。

本節では、EMQX REST API の利用方法を紹介します。

## 基本パス

EMQX の REST API はバージョン管理されており、EMQX 5.0.0 以降のすべての API パスは `/api/v5` で始まります。

## HTTP ヘッダー

ほとんどの API リクエストでは、`Accept` ヘッダーに `application/json` を設定する必要があります。特に指定がない限り、レスポンスは JSON 形式で返されます。

## HTTP レスポンスステータスコード

EMQX は [HTTP レスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status) の標準に準拠しています。主なステータスコードは以下の通りです。

| コード | 説明                                                         |
| ------ | ------------------------------------------------------------ |
| 200    | リクエスト成功。返却される JSON データに詳細が含まれます。   |
| 201    | 作成成功。新規オブジェクトが Body に返されます。             |
| 204    | リクエスト成功。削除や更新操作でよく使われ、返却 Body は空です。 |
| 400    | 不正なリクエスト。リクエストボディやパラメータのエラー。      |
| 401    | 認証失敗。API キーの期限切れまたは存在しません。              |
| 403    | 禁止。オブジェクトが使用中または依存関係の制約があります。    |
| 404    | 見つかりません。Body の `message` フィールドで理由を確認可能。 |
| 409    | コンフリクト。オブジェクトが既に存在するか、数の上限を超過。   |
| 500    | サーバ内部エラー。Body やログで原因を確認してください。       |

## 認証

EMQX の REST API は、API キーを用いたベーシック認証とベアラートークン認証の2つの主要な認証方法をサポートしています。

### API キーを用いたベーシック認証

この方法では、API キーとシークレットキーをユーザー名とパスワードとして使用し、API リクエストを認証します。EMQX の REST API は [HTTP ベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework) に準拠しており、これらの認証情報が必要です。EMQX REST API を使用する前に、API キーを作成する必要があります。詳細は [API キー管理](#api-key-management) を参照してください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0 以降はダッシュボードのユーザー認証情報を使って REST API リクエストを認証できません。代わりに API キーを作成して認証に使用してください。

:::

#### API キー認証の例

API キーとシークレットキーを取得したら、API キーをユーザー名に、シークレットキーをパスワードにしてベーシック認証を行います。

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

API キー認証の代替として、ベアラートークンを使った安全かつプログラム的な EMQX REST API へのアクセスも可能です。ベアラートークンを取得するには、以下のログイン API エンドポイントにリクエストを送信します。

#### ベアラートークンの取得

ベアラートークンを取得するには、以下のログイン API エンドポイントに HTTP `POST` リクエストを送信します。

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

- `your-emqx-address` は EMQX ノードのアドレスまたは IP に置き換えてください。
- `"admin"` と `"yourpassword"` は EMQX ダッシュボードの認証情報に置き換えてください。

レスポンスにベアラートークンが含まれ、これを API リクエストの認証に使用します。

#### ベアラートークンを使った認証

ベアラートークンを取得したら、API リクエストの `Authorization` ヘッダーに以下のように含めます。

```bash
--header "Authorization: Bearer <your-token>"
```

## API キー管理

このセクションでは、API キーの作成と管理、ロール・ネームスペース・スコープの設定方法を説明します。

### API キーの作成

#### ダッシュボード

ダッシュボードの **System** -> **API Keys** から手動で API キーを作成できます。

1. 右上の **+ Create** ボタンをクリックして作成ダイアログを開きます。
2. API キーの詳細を設定します。
   - **Name**（必須）：API キーの名前を入力します。
   - **Expire At**：空欄のままにすると期限なしになります。
   - **Enabled**：デフォルトで有効です。
   - **Role**：ロールを選択します（任意）。詳細は [ロールと権限](#roles-and-permissions) を参照してください。
   - **Namespace**：デフォルトはオフです。グローバル管理者の場合はオフのままでグローバル API キーが作成されます。オンにしてネームスペースを選択すると、そのネームスペース内のキーが作成されます。ネームスペース管理者は自分のネームスペース内でのみキーを作成可能です。
   - **Permission Mode**：管理者または閲覧者キーの場合、スコープの割り当て方法を選択します。パブリッシャーキーでは表示されません（ロールデフォルトの `publish` スコープが適用されます）。スコープの動作と制限については [API スコープ](#api-scopes) を参照してください。
     - **Role Default Scopes**：選択したロールのデフォルトを使用します。ロールデフォルトの変更は自動的に反映されます。
     - **System-level Permissions**：`system` スコープのみを付与します。
     - **Custom Restricted Permissions**：アクセス可能な API 領域を制限するために1つ以上のスコープを選択します。**Scopes** が空の場合、スコープ保護された API にはアクセスできません。
   - **Scopes**：**Custom Restricted Permissions** を選択した場合に表示され、付与するスコープを選択します。
   - **Note**：任意で説明を入力できます。
3. **Confirm** をクリックすると、API キーとシークレットキーが「作成成功」ダイアログに表示されます。

   ::: warning 重要

   API キーとシークレットキーはこの時点で必ず保存してください。シークレットキーは再表示されません。

   :::

4. **Close** をクリックしてダイアログを閉じます。

**Permission Mode** はダッシュボードのみで利用可能です。REST API では `scopes` フィールドを直接設定します。詳細は [scopes のデフォルト動作](#default-behavior-of-scopes) を参照してください。

キーの詳細は名前をクリックして確認できます。**Edit** ボタンで有効期限、状態、ロール、パーミッションモード、スコープ、説明を変更可能です。**Delete** ボタンでキーを削除できます。

#### REST API

REST API では、ダッシュボードユーザーのベアラートークンを使って API キーを作成・更新します。API キー管理のエンドポイントは API キー認証を受け付けません。

EMQX 6.0.4 以降、`POST /api/v5/api_key` および `PUT /api/v5/api_key/:name` のリクエストボディにトップレベルの `namespace` フィールドを指定可能です。例えば、以下のリクエストは `team-a` ネームスペースに管理者 API キーを作成します。

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

`scopes` に `"unset"` を指定するとロールデフォルトのスコープが明示的に適用されます。作成リクエストで `scopes` を省略した場合も同様です。

ネームスペースは以下のいずれかの方法で指定できます。

- `administrator` のようなロールと `namespace` フィールドを併用する。
- ロールに `ns:<namespace>::<role>` の形式でネームスペースを埋め込む（例：`ns:team-a::administrator`）。

両方の形式がサポートされており、両方がリクエストに含まれる場合はネームスペースが一致している必要があります。異なる場合や `namespace` が空の場合は HTTP 400 が返されます。API キー作成後はネームスペースを変更できません。

#### ブートストラップファイル

ブートストラップファイルを使って API キーを作成することも可能です。以下の設定ファイルでファイルパスを指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定したファイルに複数の API キーを `{API Key}:{Secret Key}:{?Role}:{?Scopes}` の形式で改行区切りで記述します。

- **API Key**：任意の文字列でキー識別子。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの [ロール](#roles-and-permissions)。ネームスペース付きキーは `ns:<namespace>::<role>` 形式（例：`ns:team-a::administrator`）。
- **Scopes（任意）**：キーがアクセス可能な [API スコープ](#api-scopes) をカンマ区切りで指定。省略時はロールのデフォルトが適用されます。検証動作は [ブートストラップスコープの検証](#validate-bootstrap-scopes) を参照。

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

ブートストラップエントリが以下のスコープルールに違反すると、EMQX は該当スコープを削除し、警告ログを出力してキーの作成・更新を続行します。

- **ログイン専用スコープ**：`user_management`、`mfa_management`、`sso_management`、`api_key_management` は API キーに対して無効です。EMQX はこれらを削除し、残りのスコープでキーを作成・更新します。
- **管理者相当スコープ**：API キーに割り当て可能なスコープの中で、`system` のみが管理者相当の権限を付与します。EMQX 6.0.4 以降、管理者相当スコープと管理者相当でないスコープが混在する場合、管理者相当スコープをすべて削除し、残りのスコープを保持します。
- **ネームスペース付きスコープ**：EMQX 6.0.4 以降、ネームスペース付きエントリがそのロールで保持できないスコープを明示的に指定した場合、許可されていないスコープを削除し、残りのスコープを保持します。スコープが残らない場合、キーはスコープ保護されたビジネス API にアクセスできません。許可されるスコープは [ネームスペース付き呼び出し元の制限](#restrictions-for-namespaced-callers) を参照。

##### ブートストラップ API キーのリロード

この方法で作成された API キーは無期限に有効です。

EMQX 起動時にファイルの内容が API キーリストに追加されます。既存の API キーがあれば、シークレットキー、ロール、スコープが更新されます。

### ネームスペース管理者による API キー管理

EMQX 6.0.4 以降、ネームスペース付きダッシュボード管理者は自身のネームスペース内の API キーを管理できます。管理者はベアラートークンで認証する必要があります。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| API キーの作成 | 管理者のネームスペース内にのみ作成可能。ネームスペース省略、グローバル指定、他ネームスペース指定は HTTP 403。 |
| API キー一覧取得 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーはレスポンスから除外。 |
| API キーの読み取り・更新・削除 | 管理者のネームスペース内のキーのみ操作可能。他ネームスペースのキーは HTTP 404（存在を隠蔽）。 |
| API キーのネームスペース変更 | 不可。更新は HTTP 400。 |

グローバルダッシュボード管理者は引き続き全ネームスペースの API キーを管理可能です。

## API キーの権限

### ロールと権限

REST API はロールベースアクセス制御を実装しています。API キー作成時に以下の3つのプリセットロールのいずれかを割り当てられます。

- **Administrator**：すべてのリソースにアクセス可能。指定がなければデフォルト。ロール識別子は `administrator`。
- **Viewer**：リソースやデータの閲覧のみ可能。REST API のすべての GET リクエストに対応。ロール識別子は `viewer`。
- **Publisher**：MQTT メッセージのパブリッシュ専用。メッセージパブリッシュ関連 API のみアクセス可能。ロール識別子は `publisher`。

::: tip 注意
`publisher` キーは `publish` スコープのみ受け入れます。スコープ割り当て時に `publish` 以外のスコープがあると HTTP 400 が返されます。ロールを `publisher` に変更する場合は、同時に `"scopes": ["publish"]` または空リストをリクエストに含めてください。そうしないと既存スコープに `publish` 以外が含まれている場合、リクエストは拒否されます。
:::

### API スコープ

スコープはキーごとの権限の次元で、キーがアクセス可能な REST API のビジネス領域を宣言します。スコープと [ロールと権限](#roles-and-permissions) は独立しており、両方が適用されてアクセス制御の2層を形成します。

| 次元 | 目的 | 粒度 |
| ---- | ---- | ---- |
| **ロール** | HTTP 動詞の制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | API ドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方を通過した場合にのみ許可されます。

マイクロサービスや統合シナリオでは、外部システムが EMQX 管理面の一部のみアクセスすることが多いです。例えば監視プラットフォームは `monitoring` スコープのみ、ルールパブリッシュサービスは `data_integration`、クラスター運用ツールは `cluster_operations` のみ必要です。スコープを使うことで最小権限の原則に基づきキーを割り当て、キー漏洩時の被害範囲を最小化できます。

::: tip
スコープ名は EMQX アップグレード間で変更されない安定した識別子です。OpenAPI タグ名が変更されても、同じスコープを設定したキーは引き続き動作します。
:::

#### 組み込みの API キースコープ

EMQX は API キー用に以下の10個のスコープを提供しています。

| スコープ | 名称 | 代表的な API 領域 |
| -------- | ---- | ----------------- |
| `connections` | 接続管理 | `/clients`, `/subscriptions`, `/topics`, `/banned`, `/retainer`, `/file_transfer`, `/mqtt/delayed`, `/mqtt/topic_rewrite`, ... |
| `publish` | メッセージパブリッシュ | `/publish`, `/publish/bulk` |
| `data_integration` | データ統合 | `/rules`, `/connectors`, `/actions`, `/schema_registry`, `/schema_validations`, `/message_transformations`, `/exhooks`, `/ai/*` |
| `access_control` | アクセス制御 | `/authentication`, `/authorization/*` |
| `gateways` | プロトコルゲートウェイ | `/gateways`, `/coap/*`, `/lwm2m/*`, `/gcp_devices`, ... |
| `monitoring` | 監視データ | `/metrics`, `/stats`, `/monitor*`, `/alarms`, `/trace`, `/slow_subscriptions`, `/telemetry`, `/prometheus/{auth,stats,data_integration,...}`, ... |
| `cluster_operations` | クラスター操作 | `/cluster*`, `/nodes`, `/load_rebalance`, `/node_eviction`, `/mt/*`, ... |
| `system` | システム設定 | `/configs*`, `/listeners*`, `/plugins*`, `/ds/*`, `/data/*`, `/status`, `/relup`, `/opentelemetry*`, `/prometheus`, ... |
| `audit` | 監査ログ | `/audit` |
| `license` | ライセンス | `/license*` |

::: tip 注意

EMQX 6.0.4 以降、`audit` スコープはネームスペース付き呼び出し元に監査ログアクセスを付与しません。`GET /api/v5/audit` はグローバル管理者とグローバル閲覧者のみ呼び出せます。詳細は [監査ログアクセス](./dashboard/audit-log.md#audit-log-access) を参照してください。

:::

::: warning 管理者相当スコープと制限付きスコープを混在させないでください

EMQX は `system`、`user_management`、`api_key_management`、`sso_management` を管理者相当スコープ（検証メッセージでは `privilege scopes`）として分類しています。これらを制限付きスコープと組み合わせるとアカウントの実効権限は減りません。4つのうち API キーに割り当て可能なのは `system` のみで、残り3つは [ログイン専用スコープ](#login-only-scopes) に記載されています。

そのため、EMQX 6.0.4 以降、API キー作成・更新時に明示的なスコープリストは `system` のみ、または `system` を含まないスコープのいずれかでなければなりません。混在すると HTTP 400 が返され、変更は適用されません。

既存の混在スコープリストは引き続き有効で `system` は有効なままです。次回の明示的なスコープ更新時は `system` のみか `system` を含まないリストにする必要があります。ダッシュボードで編集する際は保存前にパーミッションモードの選択を促されます。

:::

#### ログイン専用スコープ

API キースコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、API キーには割り当てられません。ログインユーザーへの割り当てと適用方法は [ログインユーザースコープ](./dashboard/system.md#login-user-scopes) を参照してください。

| スコープ | 必要ロール | 目的 |
| -------- | ---------- | ---- |
| `user_management` | Administrator | ダッシュボードユーザー管理 |
| `sso_management` | Administrator | SSO バックエンドとユーザーレコード管理 |
| `api_key_management` | Administrator | API キー管理 |
| `mfa_management` | グローバル管理者またはグローバル閲覧者 | 自身の MFA 管理。管理者は他ユーザーの MFA も管理可能。 |

#### `scopes` のデフォルト動作

EMQX 6.0.4 以降、API キーの `scopes` フィールドは以下のルールに従います。

| `scopes` の値 | 意味 |
| ------------- | ---- |
| **作成リクエストで省略** | 選択したロールのデフォルトを使用 |
| **更新リクエストで省略** | キーの現在のスコープ設定を保持 |
| **未設定セントネル `"unset"`** | 明示的なスコープ設定を解除。後方互換のため、EMQX はスコープ許可リストを適用しません。ロール・ネームスペース・API キー固有のパス制限は適用されます。 |
| **空リスト `[]`** | すべてのビジネスエンドポイントを拒否。キーをソフトに無効化するのに有用。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定したスコープの API のみ許可 |

ロールデフォルトと同じスコープセットの明示的リストは `"unset"` に正規化され、同様の動作になります。順序は無関係です。

ブートストラップファイルのエントリでスコープ区切りが省略された場合は、処理時に指定ロールのデフォルトが適用されます。

スコープはキーがアクセスできる API 領域を決定します。ロールやネームスペースの制限を上書きしません。リクエストはロール・スコープ・ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能なスコープの一覧取得

EMQX は利用可能なスコープカタログを問い合わせるために以下の2つのエンドポイントを公開しています。

- `GET /api/v5/api_key_scopes`：API キーに割り当て可能なスコープ（上記10個のビジネスドメインスコープ）を返します。API キー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（ログイン専用スコープ4つを含む）を返します。ベアラートークン認証が必要です。

スコープ選択 UI の初期化や自動化スクリプトの検証に利用してください。

```bash
# API キースコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの方法で設定可能です。

- **ダッシュボード**：**System** -> **API Keys** でキー作成・編集時に **Permission Mode** を選択。**Custom Restricted Permissions** の場合のみ個別スコープを選択。
- **REST API**：作成・更新リクエストボディに `"scopes": ["monitoring", "cluster_operations"]` を含める。
- **ブートストラップファイル**：各行の4番目の区切りとしてカンマ区切りスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ネームスペース付き呼び出し元の制限

ネームスペース付き呼び出し元（ロールが特定ネームスペースに制限されたユーザーや API キー）は、スコープチェックに加え、エンドポイントレベルで追加の制限を受けます。スコープ付与はこれらの制限を上書きしません。

### ネームスペース付き API キーのスコープ制限

EMQX 6.0.4 以降、作成リクエストで `scopes` を省略すると、ネームスペース付きの管理者または閲覧者ロールの API キーは以下の7つのスコープで作成されます。

`connections`, `monitoring`, `data_integration`, `access_control`, `system`, `cluster_operations`, `license`

これらのデフォルトには `publish`, `gateways`, `audit` は含まれません。

ネームスペース付きの管理者または閲覧者ロールの API キーを作成する場合、または既存キーの明示的スコープリストを変更する場合、リクエストに含められるスコープは上記7つのいずれかのみです。`publish`, `gateways`, `audit` またはロールで許可されていないスコープを指定すると HTTP 400 が返され、禁止されたスコープが特定されて変更は適用されません。`system` と制限付きスコープの混在禁止も明示的スコープリストに適用されます。

### 禁止スコープを含む既存キー

禁止スコープを含む既存キーは自動的に変更されません。読み取り・修正・書き込みクライアントの互換性のため、更新時に同じスコープリストを再送信し、ロールとネームスペースが変わらなければ受け入れられます。

例外は、禁止スコープが `publish` のみのネームスペース付きキーで、API にアクセスできないため、変更がなくても更新は HTTP 400 になります。この場合、キーを削除してネームスペースなしで再作成してください。ロールやスコープの実際の変更は再検証され、許可リストに準拠する必要があります。

禁止スコープを含むネームスペース付き API キーは、キーがローテーションされるまで以前の権限が有効です。ただし、ネームスペースのエンドポイント制限は適用されます。ブートストラップエントリを再処理する際は、禁止スコープを削除し警告ログを出力し、残りのスコープを保持します。詳細は [ブートストラップスコープの検証](#validate-bootstrap-scopes) を参照してください。

### メッセージパブリッシュの制限

ネームスペース付き API キーは、`POST /api/v5/publish` を含むメッセージパブリッシュ API を呼び出せません。以前のスコープリストに `publish` が含まれていても、スコープ割り当てはネームスペースレベルの制限を上書きしません。

### メッセージコンテンツの制限

ネームスペース付き呼び出し元が `connections` または `monitoring` スコープを持っていても、クラスター全体の MQTT メッセージコンテンツ（保持メッセージや遅延メッセージストアを含む）を読み書きするエンドポイントにはアクセスできません。以下のメッセージ関連エンドポイントは `403 Forbidden` を返します。

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

ファイル転送ストアはグローバルでネームスペース非対応です。ネームスペース付き呼び出し元はロールに関係なく以下のファイル転送コンテンツエンドポイントにアクセスできず、スコープ付与もこれを上書きしません。

- `GET /file_transfer/files`
- `GET /file_transfer/files/:clientid/:fileid`
- `GET /file_transfer/file`

グローバル呼び出し元はロールとスコープに応じてこれらのエンドポイントにアクセス可能です。`/file_transfer` 設定エンドポイントは影響を受けません。

### トレースの制限

トレース操作では、`GET /trace` は呼び出し元のネームスペース内のトレースのみ一覧表示します。以下のトレース単体操作は、トレースが別ネームスペースの場合 `404 Not Found` を返します。

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この挙動により他ネームスペースのトレースの存在が漏れません。まとめて削除する操作（`DELETE /trace`）はネームスペース付き呼び出し元に対して `403 Forbidden` を返し、全トレースのクリアはグローバル管理者のみ可能です。

ダッシュボードログイン、SSO コールバック、API キーの自己管理エンドポイント（例：`/api_key`）は、キーの `scopes` 設定に関わらず API キー認証を受け付けません。これはスコープモデルとは無関係なダッシュボードのセキュリティ境界です。

## ページネーション

大量データを扱う一部の API ではページネーション機能を提供しています。データの特性に応じて2種類のページネーション方式があります。

### ページ番号によるページネーション

多くのページネーション対応 API では、`page`（ページ番号）と `limit`（ページサイズ）パラメータでページネーションを制御できます。最大ページサイズは `10000` です。`limit` が指定されない場合はデフォルトで `100` となります。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれます。EMQX は検索条件付きリクエストの総データ数を予測できないため、`meta.hasnext` フィールドで次ページの有無を示します。

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

データが急速に変化し、ページ番号方式が非効率な一部 API ではカーソルページネーションを採用しています。

`position` または `cursor`（開始位置）パラメータでデータの開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置から読み込む件数を指定します。最大ページサイズは `10000` です。`limit` が指定されない場合はデフォルトで `100` となります。

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

この方式はデータ変動が激しいシナリオで効率的かつ連続的なデータ取得を実現します。

## エラーコード

HTTP レスポンスステータスコードに加え、EMQX は特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時は、Body に JSON 形式でエラーコードが返されます。

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| エラーコード                                   | 説明                                                         |
| ---------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています。                |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはキー＆シークレットが間違っています。 |
| BAD_REQUEST                                    | リクエストパラメータが不正です。                             |
| NOT_MATCH                                      | 条件が一致しません。                                         |
| ALREADY_EXISTS                                 | リソースが既に存在します。                                   |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です。                                       |
| BAD_LISTENER_ID                                | リスナー ID が不正です。                                     |
| BAD_NODE_NAME                                  | ノード名が不正です。                                         |
| BAD_RPC                                        | RPC に失敗しました。クラスター状態と対象ノードの状態を確認してください。 |
| BAD_TOPIC                                      | トピックの構文エラー。MQTT プロトコル標準に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成しようとしたリソースが最大または最小制限を超えています。  |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています。       |
| CONFLICT                                       | リクエストリソースが競合しています。                         |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使用されていません。     |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています。                     |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージ ID の解析エラーです。                             |
| INVALID_ID                                     | ID のスキーマが不正です。                                   |
| MESSAGE_ID_NOT_FOUND                           | メッセージ ID が存在しません。                               |
| NOT_FOUND                                      | リソースが見つかりません。                                   |
| CLIENTID_NOT_FOUND                             | クライアント ID が見つかりません。                           |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常は MQTT クライアントではありません）。 |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません。                                   |
| TOPIC_NOT_FOUND                                | トピックが見つかりません。                                   |
| USER_NOT_FOUND                                 | ユーザーが見つかりません。                                   |
| INTERNAL_ERROR                                 | サーバ内部エラーです。                                       |
| SERVICE_UNAVAILABLE                            | サービスが利用できません。                                   |
| SOURCE_ERROR                                   | ソースエラーです。                                           |
| UPDATE_FAILED                                  | 更新に失敗しました。                                         |
| REST_FAILED                                    | リセットソースまたは設定に失敗しました。                     |
| CLIENT_NOT_RESPONSE                            | クライアントが応答していません。                             |
