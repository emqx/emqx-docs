# REST API

EMQX は OpenAPI 3.0 仕様に準拠した HTTP 管理 API を公開しています。

EMQX では REST API を探索および操作するための複数の方法を提供しています。EMQX 起動後、以下の API 仕様エンドポイントが利用可能です。

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウン形式の API リファレンスページ。 |
| `/api-spec.md` | Markdown | Markdown 形式の API リファレンス。AI エージェントや自動化ツール向け。 |
| `/api-spec.json` | JSON | JSON 形式の OpenAPI 3.0 仕様。スクリプトやプログラムによるツール向け。 |
| `/api-spec/:tag[/:name]` | JSON | API タグに絞った OpenAPI 3.0 仕様。リクエストまたはレスポンスのスキーマ名でさらに絞り込み可能。 |
| `/api-docs/swagger.json` | JSON | 外部 Swagger UI 展開や互換ツール向けの完全な OpenAPI 3.0 仕様。 |

上記すべてのエンドポイントは、ダッシュボード設定で `swagger_support` が `true`（デフォルト）に設定されている必要があります。`false` に設定するとすべての API ドキュメントエンドポイントが無効になります。詳細は [ダッシュボード設定](../configuration/dashboard.md) を参照してください。

EMQX 6.3.0 以降、EMQX は Swagger UI をバンドルしなくなりました。後方互換のため、`/api-docs` または `/api-docs/index.html` へのリクエストは HTTP 308 を返し、`/api-spec.html` にリダイレクトします。`/api-docs/index.html` と `/api-docs/swagger.json` を除くその他の `/api-docs/*` サブパスは以前 Swagger UI のアセットを提供していましたが、現在は HTTP 404 を返します。

本セクションでは EMQX REST API の利用方法を紹介します。

::: tip
EMQX 6.3.0 以降、[機能ゲート](../deploy/feature-gates.md) により起動時にオプション機能を無効化できます。無効化された機能が提供する REST API パスはアクセス可能なエンドポイントとして読み込まれません。`dashboard` 機能が有効な場合、`GET /api/v5/features` で解決済みの機能セットを確認できます。
:::

## API 仕様エンドポイントへのアクセス

EMQX 6.3.0 以降、上記の API 仕様エンドポイントからコンテンツを取得するには認証が必要です。

プログラムによるリクエストは、API キーとシークレットキーを用いた Basic 認証か、ベアラートークン認証のいずれかで認証してください。詳細は [認証](#authentication) を参照してください。

API 仕様へのアクセスは読み取り専用であり、API キーのロールやスコープには依存しません。

`/api-spec.md`、`/api-spec.json`、`/api-spec/:tag[/:name]`、`/api-docs/swagger.json` では、認証情報が欠落または無効なリクエストに対し HTTP `401` を返します。`WWW-Authenticate` レスポンスヘッダーは Basic と Bearer 認証を通知します。レスポンスボディは要求されたフォーマットに準じ、サポートされている認証方式を説明し、2つの公開エンドポイントを示します：ベアラートークン取得用の `POST /api/v5/login` とブローカー状態確認用の `GET /api/v5/status` です。最小限のレスポンスであり、要求された API 仕様内容は含まれません。

ブラウザからのアクセスでは、有効な `emqx_auth` セッションクッキーを受け入れます。認証されていない `/api-spec.html` へのリクエストは HTTP `401` を返し、完全な API Spec Explorer の代わりにサインインページを表示します。このレスポンスはベアラー認証のみを通知し、ブラウザのネイティブ Basic 認証ダイアログの表示を防止します。ダッシュボードのユーザー名とパスワードでサインインすると、EMQX は `emqx_auth` セッションクッキーを作成し、完全なエクスプローラーを読み込みます。サインアウトするとセッションクッキーはクリアされます。

`/api-docs` および `/api-docs/index.html` へのリクエストは認証不要です。これらのエンドポイントは `/api-spec.html` へリダイレクトするだけだからです。リダイレクト後の完全なエクスプローラーへのアクセスには認証が必要です。

## ベーシックパス

EMQX の REST API はバージョン管理されており、EMQX 5.0.0 以降のすべての API パスは `/api/v5` で始まります。

## HTTP ヘッダー

ほとんどの API リクエストでは、`Accept` ヘッダーを `application/json` に設定する必要があります。そうするとレスポンスは JSON 形式で返されます（特に指定がない限り）。

## HTTP レスポンスステータスコード

EMQX は [HTTP レスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status) 標準に準拠しています。主なステータスコードは以下の通りです。

| コード | 説明 |
| ----- | ------------------------------------------------------------ |
| 200   | リクエスト成功。返却される JSON データに詳細情報を含みます。 |
| 201   | 作成成功。新規オブジェクトがボディに返されます。 |
| 204   | リクエスト成功。通常は削除や更新操作で、返却ボディは空です。 |
| 400   | 不正なリクエスト。リクエストボディやパラメータのエラー。 |
| 401   | 認証エラー。認証情報が欠落、無効、または期限切れ。 |
| 403   | 禁止。オブジェクトが使用中、または依存関係制約がある場合。 |
| 404   | 見つからない。ボディの `message` フィールドで理由を確認可能。 |
| 409   | コンフリクト。オブジェクトが既に存在するか、数の制限超過。 |
| 500   | サーバ内部エラー。ボディやログで原因を確認してください。 |

## 認証

EMQX の REST API は主に API キーを用いた Basic 認証と、ベアラートークン認証の2つの方法をサポートしています。

### API キーを用いた Basic 認証

この方法では、API キーとシークレットキーをユーザー名とパスワードとして使用し、API リクエストを認証します。EMQX の REST API は [HTTP Basic 認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework) に準拠しています。EMQX REST API 利用前に API キーを作成する必要があります。詳細は [API キー管理](#api-key-management) を参照してください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0 以降はダッシュボードのユーザー認証情報を REST API 認証に使用できません。代わりに API キーを作成して認証に使用してください。

:::

#### API キーでの認証例

API キーとシークレットキーを取得したら、API キーをユーザー名、シークレットキーをパスワードとして Basic 認証に使用します。

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

API キー認証の代替として、ベアラートークンを用いた安全かつプログラム的な EMQX REST API へのアクセスが可能です。ベアラートークンを取得するには、以下のログイン API エンドポイントにリクエストを送信します。

#### ベアラートークンの取得

ベアラートークンを取得するには、以下のログイン API エンドポイントに HTTP `POST` リクエストを送信してください。

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

レスポンスにベアラートークンが含まれ、API リクエストの認証に使用できます。

#### ベアラートークンを用いた認証

ベアラートークンを取得したら、API リクエストの `Authorization` ヘッダーに以下のように含めてください。

```bash
--header "Authorization: Bearer <your-token>"
```

## API キー管理

本節では API キーの作成および管理方法、ロール、ネームスペース、スコープの設定方法を説明します。

### API キーの作成

#### ダッシュボード

ダッシュボードの **System** -> **API Keys** から手動で API キーを作成できます。

1. 右上の **+ Create** ボタンをクリックし、作成ダイアログを開きます。
2. API キーの詳細を設定します：
   - **Name**（必須）：API キーの名前を入力します。
   - **Expire At**：空欄のままにすると期限なしになります。
   - **Is Enable**：デフォルトで有効です。
   - **Role**：ロールを選択します（任意）。詳細は [ロールと権限](#roles-and-permissions) を参照してください。
   - **Namespace**：デフォルトはオフ。グローバル管理者の場合はオフのままでグローバルキーが作成されます。オンにしてネームスペースを選択すると、そのネームスペース内のキーになります。ネームスペース管理者は自分のネームスペース内でのみキーを作成可能です。
   - **Permission Mode**：管理者またはビューアのキーでスコープを割り当てる方法を選択します。パブリッシャーキーには表示されません。スコープの挙動と制限は [API スコープ](#api-scopes) を参照してください。
     - **Role Default Scopes**：選択したロールのデフォルトスコープを使用します。ロールデフォルトの変更は自動的に反映されます。
     - **System-level Permissions**：`system` スコープのみ付与します。
     - **Custom Restricted Permissions**：アクセス可能な API 領域を制限するためにスコープを1つ以上選択します。**Scopes** を空欄にするとスコープ保護された API にアクセスできません。
   - **Scopes**：**Custom Restricted Permissions** 選択時に表示され、付与するスコープを選択します。
   - **Note**：任意で説明を入力します。
3. **Confirm** をクリックすると、API キーとシークレットキーが **Created Successfully** ダイアログに表示されます。

   ::: warning 重要

   API キーとシークレットキーはすぐに保存してください。シークレットキーは再表示されません。

   :::

4. **Close** をクリックしてダイアログを閉じます。

**Permission Mode** はダッシュボードでのみ利用可能です。REST API では `scopes` フィールドを直接設定します。詳細は [スコープのデフォルト動作](#default-behavior-of-scopes) を参照してください。

キー名をクリックすると詳細を表示できます。**Edit** ボタンで有効期限、状態、ロール、パーミッションモード、スコープ、説明を変更可能です。**Delete** ボタンでキーを削除できます。

#### REST API

REST API 経由で API キーを作成・更新するには、ダッシュボードユーザーのベアラートークンで認証してください。API キー管理エンドポイントは API キー認証を受け付けません。

EMQX 6.0.4 以降、`POST /api/v5/api_key` と `PUT /api/v5/api_key/:name` のリクエストボディにトップレベルの `namespace` フィールドを指定可能です。例えば、`team-a` ネームスペースに管理者 API キーを作成するリクエスト例：

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

`scopes` に `"unset"` を指定するとロールデフォルトスコープが明示的に適用されます。作成リクエストで `scopes` を省略しても同様です。

ネームスペースは以下のいずれかの方法で指定できます。

- `administrator` のような素のロールと `namespace` フィールドを併用する。
- `ns:<namespace>::<role>` の形式でロールにネームスペースを埋め込む（例：`ns:team-a::administrator`）。

両形式は引き続きサポートされます。両方がリクエストに含まれる場合はネームスペースが一致している必要があります。異なる場合や空の場合は EMQX は HTTP 400 を返します。API キー作成後にネームスペースを変更することはできません。

EMQX 6.3.0 以降、`multi_tenancy.deny_namespaces` にリストされたネームスペースはどちらの形式でも使用できません。設定詳細は [拒否されたネームスペース名](../multi-tenancy/namespace-global-settings.md#denied-namespace-names) を参照してください。

グローバル API キーを作成するには、`namespace` を省略し、ネームスペースプレフィックスのないロールを使用してください。`namespace` に文字列 `"global"` を指定してもグローバルスコープにはなりません。

#### ブートストラップファイル

ブートストラップファイル方式でも API キーを作成可能です。以下の設定ファイルでファイルパスを指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイルに複数の API キーを `{API Key}:{Secret Key}:{?Role}:{?Scopes}` 形式で改行区切りで記述します。

- **API Key**：キー識別子として任意の文字列。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの [ロール](#roles-and-permissions)。
- **Scopes（任意）**：キーがアクセス可能な [API スコープ](#api-scopes) をカンマ区切りで指定。省略時はロールのデフォルトが適用されます。ログイン専用スコープ（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）は API キーに対して無効です。これらがブートストラップファイルに含まれると、EMQX は起動時に削除し警告ログを出力します。キーはスコープなしで作成されます。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

API キーに割り当て可能なスコープのうち、`system` のみが管理者相当の権限を付与します。EMQX 6.0.4 以降、管理者相当スコープと管理者相当でないスコープを混在させたブートストラップエントリは、管理者相当スコープをすべて削除し、残りのスコープを保持し、警告ログを出力してキーを作成または更新します。一方、REST API は混在スコープリストを HTTP 400 で拒否し、スコープ変更を適用しません。

この方法で作成された API キーは無期限で有効です。

EMQX 起動時にファイルのデータセットが API キーリストに追加されます。既存の API キーがあればシークレットキー、ロール、スコープが更新されます。

### ネームスペース管理者による API キー管理

EMQX 6.0.4 以降、ネームスペース管理者は自身のネームスペース内の API キーを管理できます。認証にはベアラートークンが必要です。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| API キー作成 | 管理者のネームスペース内でのみ作成可能。ネームスペース省略、グローバル指定、他ネームスペース指定は HTTP 403。 |
| API キー一覧取得 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーはレスポンスから除外。 |
| API キーの読み取り、更新、削除 | 管理者のネームスペース内のキーのみ操作可能。他ネームスペースのキーは HTTP 404（存在を秘匿）。 |
| API キーのネームスペース変更 | 他ネームスペースへの移動不可。更新は HTTP 400。 |

グローバル管理者は引き続き全ネームスペースの API キーを管理可能です。

### ロールと権限

REST API はロールベースアクセス制御を実装しています。API キー作成時に以下の3つのプリセットロールのいずれかを割り当てられます。

- **Administrator**：すべてのリソースにアクセス可能。指定がなければデフォルト。ロール識別子は `administrator`。
- **Viewer**：リソースやデータの閲覧のみ可能。REST API のすべての GET リクエストに対応。ロール識別子は `viewer`。
- **Publisher**：MQTT メッセージのパブリッシュ専用。メッセージパブリッシュ関連 API のみアクセス可能。ロール識別子は `publisher`。

::: tip 注意
`publisher` キーは `publish` スコープのみ許容します。スコープ割り当て時に `publish` 以外のスコープがあると HTTP 400 になります。キーのロールを `publisher` に変更する場合は、同時リクエストで `"scopes": ["publish"]` または空リストを含めてください。そうしないと既存スコープに `publish` 以外が含まれる場合、リクエストは拒否されます。
:::

### API スコープ

スコープはキーごとの権限次元であり、REST API のどの業務領域にアクセス可能かを宣言します。スコープと [ロールと権限](#roles-and-permissions) は独立しており、両方のチェックを通過した場合にのみアクセスが許可されます。

| 次元 | 目的 | 粒度 |
| --------- | ------- | ----------- |
| **ロール** | HTTP 動詞の制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | API ドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方で検証されます。両方を通過した場合のみ許可されます。

マイクロサービスや統合シナリオでは、外部システムが EMQX 管理面の一部のみアクセスすることが多いです。例えば、監視プラットフォームは `monitoring` スコープのみ、ルールパブリッシュサービスは `data_integration` のみ、クラスター運用ツールは `cluster_operations` のみ必要です。スコープにより最小権限の原則を適用し、キー漏洩時の影響範囲を最小化できます。

::: tip
スコープ名は安定した識別子であり、EMQX のアップグレードで変更されません。OpenAPI タグ名が変更されても、同じスコープを持つキーは引き続き動作します。
:::

#### 組み込み API キースコープ

EMQX は API キー用に以下の10のスコープを提供しています。

| スコープ | 名称 | 代表的な API 領域 |
| --- | --- | --- |
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

::: warning 管理者相当スコープと制限スコープを混在させないでください

EMQX は `system`、`user_management`、`api_key_management`、`sso_management` を管理者相当スコープ（検証メッセージでは `privilege scopes`）と分類しています。これらを制限スコープと混在させてもアカウントの実効権限は減りません。4つのうち API キーに割り当て可能なのは `system` のみです。その他は [ログイン専用スコープ](#login-only-scopes) に分類されます。

そのため EMQX 6.0.4 以降、API キー作成・更新時の明示的なスコープリストは `system` のみ、または `system` を含まないスコープのみのいずれかでなければなりません。混在リストは HTTP 400 で拒否され、変更は適用されません。

既存の混在スコープリストは引き続き有効で `system` は有効です。次回の明示的なスコープ更新は `system` のみか、`system` を含まないリストである必要があります。ダッシュボードで編集時は保存前にパーミッションモードの選択を促されます。

:::

#### ログイン専用スコープ

API キースコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、API キーには割り当てられません。割り当てと適用の詳細は [ログインユーザースコープ](../dashboard/system.md#login-user-scopes) を参照してください。

| スコープ | 必要ロール | 用途 |
| --- | --- | --- |
| `user_management` | Administrator | ダッシュボードユーザー管理。 |
| `sso_management` | Administrator | SSO バックエンドおよびユーザー管理。 |
| `api_key_management` | Administrator | API キー管理。 |
| `mfa_management` | 任意 | 自アカウントの MFA 管理。管理者は他ユーザーの MFA も管理可能。 |

#### ネームスペース制限付き呼び出し元の制限

ネームスペース制限付き呼び出し元（ロールが特定ネームスペースに限定されたユーザーまたは API キー）は、スコープチェックに加えてエンドポイントレベルの追加制限を受けます。スコープ付与はこれらの制限を上書きしません。

ネームスペース API キーはメッセージパブリッシュ API（`POST /api/v5/publish` を含む）を呼び出せません。スコープリストに `publish` があってもこの制限は解除されません。

ネームスペース呼び出し元が `connections` または `monitoring` スコープを持っていても、クラスター全体の MQTT メッセージコンテンツ（保持メッセージや遅延メッセージストアを含む）を読み書きするエンドポイントにはアクセスできません。以下のメッセージ関連エンドポイントは `403 Forbidden` を返します。

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

トレース操作では、`GET /trace` は呼び出し元のネームスペース内のトレースのみを一覧表示します。以下のトレース単位操作は異なるネームスペースのトレースに対して `404 Not Found` を返します。

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この動作により他ネームスペースのトレース情報の漏洩を防止します。まとめて削除する操作（`DELETE /trace`）はネームスペース呼び出し元に対して `403 Forbidden` を返し、グローバル管理者のみがすべてのトレースをクリア可能です。

ダッシュボードログイン、SSO コールバック、API キー自己管理エンドポイント（例：`/api_key`）は、キーの `scopes` 設定に関わらず API キー認証を受け付けません。これはスコープモデルとは無関係なダッシュボードのセキュリティ境界です。

#### `scopes` のデフォルト動作

EMQX 6.0.4 以降、API キーの `scopes` フィールドは以下のルールに従います。

| `scopes` の値 | 意味 |
| --- | --- |
| **作成リクエストで未指定** | 選択されたロールのデフォルトを使用。 |
| **更新リクエストで未指定** | キーの現在のスコープ設定を維持。 |
| **ロールデフォルトのセントネル `"unset"`** | 明示的なスコープ設定を削除し、ロールデフォルトを使用。ロールデフォルトの変更は自動反映。 |
| **空リスト `[]`** | すべての業務エンドポイントを拒否。キーを無効化せずにソフトに無効化可能。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定したスコープのみアクセス許可。 |

明示的リストがロールデフォルトと同じスコープセットの場合、`"unset"` と同じ効果でロールデフォルトの変更を追従します。順序は問われません。

ブートストラップファイルのエントリでスコープセグメントを省略すると、指定されたロールのデフォルトが適用されます。

スコープはキーがアクセス可能な API 領域を決定します。ロールやネームスペース制限を上書きしません。リクエストはロール、スコープ、ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能スコープの一覧取得

EMQX は利用可能なスコープカタログを照会するための2つのエンドポイントを公開しています。

- `GET /api/v5/api_key_scopes`：API キーに割り当て可能なスコープ（上記10の業務ドメインスコープ）を返します。API キー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（ログイン専用4スコープ含む）を返します。ベアラートークン認証が必要です。

スコープ選択 UI の生成や自動化スクリプトの検証に利用してください。

```bash
# API キースコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの入口から設定可能です。

- **ダッシュボード**：**System** -> **API Keys** でキー作成・編集時に **Permission Mode** を選択。**Custom Restricted Permissions** で個別スコープを選択。
- **REST API**：作成・更新リクエストボディに `"scopes": ["monitoring", "cluster_operations"]` を含める。
- **ブートストラップファイル**：各行の4番目のセグメントとしてカンマ区切りのスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ページネーション

大量データを扱う一部 API ではページネーション機能が提供されています。データ特性に応じて2種類のページネーション方式があります。

### ページ番号方式

ページネーション対応の多くの API では、`page`（ページ番号）と `limit`（ページサイズ）パラメータでページを制御します。最大ページサイズは `10000` です。`limit` 未指定時はデフォルトで `100` です。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれます。EMQX は検索条件付きリクエストの総件数を予測できないため、`meta.hasnext` フィールドで次ページの有無を示します。

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

データ変動が激しくページ番号方式が非効率な一部 API ではカーソル方式を採用しています。

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

この方式はデータ変動が激しいシナリオで連続性と効率性を確保します。

## エラーコード

HTTP レスポンスステータスコードに加え、EMQX は特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時、ボディは JSON 形式でエラーコードを返します。

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| エラーコード                                    | 説明                                                  |
| ---------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが誤っています。                  |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはキー＆シークレットが誤っています。 |
| BAD_REQUEST                                    | リクエストパラメータが不正です。                                 |
| NOT_MATCH                                      | 条件が一致しません。                                       |
| ALREADY_EXISTS                                 | リソースが既に存在します。                                      |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です。                                         |
| BAD_LISTENER_ID                                | リスナー ID が不正です。                                       |
| BAD_NODE_NAME                                  | ノード名が不正です。                                           |
| BAD_RPC                                        | RPC 失敗。クラスター状態と対象ノードの状態を確認してください。       |
| BAD_TOPIC                                      | トピック構文エラー。トピックは MQTT プロトコル標準に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成しようとするリソースが最大または最小制限を超えています。           |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています。                 |
| CONFLICT                                       | リクエストリソースに競合があります。                              |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使用されていません。               |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています。                           |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージ ID の解析エラー。                                     |
| INVALID_ID                                     | 不正な ID スキーマ。                                           |
| MESSAGE_ID_NOT_FOUND                           | メッセージ ID が存在しません。                                  |
| NOT_FOUND                                      | リソースが見つかりません。                                     |
| CLIENTID_NOT_FOUND                             | クライアント ID が見つかりません。                              |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常は MQTT クライアントではありません）。 |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません。                                     |
| TOPIC_NOT_FOUND                                | トピックが見つかりません。                                     |
| USER_NOT_FOUND                                 | ユーザーが見つかりません。                                     |
| INTERNAL_ERROR                                 | サーバ内部エラーです。                                         |
| SERVICE_UNAVAILABLE                            | サービス利用不可です。                                         |
| SOURCE_ERROR                                   | ソースエラーです。                                            |
| UPDATE_FAILED                                  | 更新に失敗しました。                                           |
| REST_FAILED                                    | リセットソースまたは設定に失敗しました。                           |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません。                                   |
