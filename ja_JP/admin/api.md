# REST API

EMQX は OpenAPI（Swagger）3.0 仕様に準拠した HTTP 管理 API を公開しています。

EMQX 起動後、[http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html) にアクセスすると、API ドキュメントの閲覧や Swagger UI から管理 API の実行が可能です。デフォルトでは、ダッシュボード設定の `swagger_support` が `true` に設定されており、Swagger UI が有効になっています。これにより、インタラクティブな API ドキュメント生成などの Swagger 関連機能がすべて有効になります。`false` に設定するとこれらの機能は無効化されます。詳細は [ダッシュボード設定](../configuration/dashboard.md) をご参照ください。

本節では EMQX REST API の利用方法について説明します。

## 基本パス

EMQX の REST API はバージョン管理されており、EMQX 5.0.0 以降のすべての API パスは `/api/v5` で始まります。

## HTTP ヘッダー

ほとんどの API リクエストでは、`Accept` ヘッダーを `application/json` に設定する必要があります。これにより、レスポンスは JSON 形式で返されます（特に指定がない限り）。

## HTTP レスポンスステータスコード

EMQX は [HTTP レスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status) の標準に準拠しています。主なステータスコードは以下の通りです。

| コード | 説明                                                         |
| ------ | ------------------------------------------------------------ |
| 200    | リクエスト成功。返却される JSON データに詳細が含まれます。  |
| 201    | 作成成功。新規オブジェクトがレスポンスボディに返されます。  |
| 204    | リクエスト成功。通常は削除や更新操作で返却ボディは空です。  |
| 400    | 不正なリクエスト。リクエストボディやパラメータのエラー。    |
| 401    | 認証失敗。API キーが期限切れか存在しません。                 |
| 403    | 禁止。オブジェクトが使用中、または依存関係の制約があります。 |
| 404    | 未検出。レスポンスボディの `message` フィールドで理由を確認可能。 |
| 409    | 競合。オブジェクトが既に存在するか、数の上限を超えています。 |
| 500    | サーバ内部エラー。レスポンスボディやログで原因を確認してください。 |

## 認証

EMQX の REST API は主に API キーを用いたベーシック認証とベアラートークン認証の2種類の認証方式をサポートしています。

### API キーを用いたベーシック認証

この方式では、API キーとシークレットキーをそれぞれユーザー名とパスワードとして使用し、API リクエストを認証します。EMQX REST API は [HTTP ベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework) に準拠しており、これらの認証情報が必要です。EMQX REST API を利用する前に API キーを作成する必要があります。詳細は [API キー管理](#api-key-management) をご参照ください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0 以降はダッシュボードのユーザー認証情報で REST API を認証できません。代わりに API キーを作成して認証に使用してください。

:::

#### API キーでの認証例

API キーとシークレットキーを取得したら、API キーをユーザー名、シークレットキーをパスワードとしてベーシック認証を行います。

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

API キー認証の代替として、ベアラートークンを使用して EMQX REST API に安全かつプログラム的にアクセスすることも可能です。ベアラートークンは以下のログイン API エンドポイントにリクエストを送信して取得します。

#### ベアラートークンの取得

以下のログイン API エンドポイントに HTTP `POST` リクエストを送信してベアラートークンを取得します。

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

取得したベアラートークンは、API リクエストの `Authorization` ヘッダーに以下のように含めてください。

```bash
--header "Authorization: Bearer <your-token>"
```

## API キー管理

本節では API キーの作成および管理方法、並びにロール、ネームスペース、スコープの設定方法について説明します。

### API キーの作成

#### ダッシュボード

ダッシュボードの **システム** -> **API キー** から手動で API キーを作成できます。

1. 右上の **+ 作成** ボタンをクリックし、作成ダイアログを開きます。
2. API キーの詳細を設定します：
   - **名前**（必須）：API キーの名前を入力します。
   - **有効期限**：空欄の場合は期限なしとなります。
   - **有効化**：デフォルトで有効です。
   - **ロール**：ロールを選択します（任意）。詳細は [ロールと権限](#roles-and-permissions) を参照してください。
   - **ネームスペース**：デフォルトはオフです。グローバル管理者の場合はオフのままでグローバル API キーが作成されます。オンにしてネームスペースを選択すると、そのネームスペース内のキーが作成されます。ネームスペース管理者は自分のネームスペース内でのみキーを作成可能です。
   - **権限モード**：管理者またはビューアのキーの場合、スコープの割り当て方法を選択します。パブリッシャーキーには表示されません（ロールデフォルトの `publish` スコープが適用されます）。スコープの挙動と制限は [API スコープ](#api-scopes) を参照してください。
     - **ロールデフォルトスコープ**：選択したロールのデフォルトを使用します。ロールデフォルトの変更は自動的に反映されます。
     - **システムレベルの権限**：`system` スコープのみを付与します。
     - **カスタム制限付き権限**：アクセス可能な API 領域を制限するためにスコープを1つ以上選択します。スコープを空欄にするとスコープ保護された API にはアクセスできません。
   - **スコープ**：**カスタム制限付き権限** 選択時に表示され、付与するスコープを選択します。
   - **備考**：任意で説明を入力できます。
3. **確認** をクリックすると、作成成功ダイアログに API キーとシークレットキーが表示されます。

   ::: warning 重要

   API キーとシークレットキーは必ずすぐに保存してください。シークレットキーは再表示されません。

   :::

4. **閉じる** をクリックしてダイアログを閉じます。

**権限モード** はダッシュボードのみで利用可能です。REST API では `scopes` フィールドを直接設定します。詳細は [scopes のデフォルト挙動](#default-behavior-of-scopes) を参照してください。

キー名をクリックすると詳細を確認でき、**編集** ボタンで有効期限、状態、ロール、権限モード、スコープ、備考を変更可能です。**削除** ボタンでキーを削除できます。

#### REST API

REST API を使って API キーを作成・更新する場合は、ダッシュボードユーザーのベアラートークンで認証してください。API キー管理のエンドポイントは API キー認証を受け付けません。

EMQX 6.0.4 以降、`POST /api/v5/api_key` および `PUT /api/v5/api_key/:name` のリクエストボディにトップレベルの `namespace` フィールドを指定可能です。例えば、`team-a` ネームスペースに管理者 API キーを作成するリクエストは以下の通りです。

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

`scopes` に `"unset"` を設定するとロールデフォルトのスコープが明示的に適用されます。作成リクエストで `scopes` を省略しても同様の効果です。

ネームスペースは以下のいずれかの方法で指定できます。

- `administrator` のようなロールと `namespace` フィールドを併用する。
- `ns:<namespace>::<role>` の形式でロールにネームスペースを埋め込む（例：`ns:team-a::administrator`）。

両形式は引き続きサポートされており、両方が含まれる場合はネームスペースが一致している必要があります。不一致や空の場合は HTTP 400 が返されます。API キー作成後はネームスペースの変更はできません。

#### ブートストラップファイル

ブートストラップファイルを使って API キーを作成することも可能です。以下の設定でファイルの場所を指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイル内に複数の API キーを `{API Key}:{Secret Key}:{?Role}:{?Scopes}` の形式で改行区切りで記述します。

- **API Key**：キー識別子として任意の文字列。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの [ロール](#roles-and-permissions) を指定。
- **Scopes（任意）**：キーがアクセス可能な [API スコープ](#api-scopes) をカンマ区切りで指定。省略時はロールのデフォルトが適用されます。ログイン専用スコープ（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）は API キーに割り当てられません。これらがブートストラップファイルに含まれる場合、EMQX 起動時に除外され警告ログが出力されます。キーはスコープなしで作成されます。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

API キーに割り当て可能なスコープのうち、`system` のみが管理者相当の権限を付与します。EMQX 6.0.4 以降、管理者相当スコープと管理者相当でないスコープが混在する場合、EMQX は管理者相当スコープをすべて除外し、残りのスコープを保持して警告ログを出力し、キーの作成・更新を続行します。一方、REST API は混在したスコープリストを HTTP 400 で拒否し、変更を適用しません。

この方法で作成された API キーは無期限で有効です。

EMQX 起動時にファイルの内容が API キーリストに追加されます。既存の API キーがある場合はシークレットキー、ロール、スコープが更新されます。

### ネームスペース管理者による API キー管理

EMQX 6.0.4 以降、ネームスペース管理者は自身のネームスペース内で API キーを管理可能です。管理者はベアラートークンで認証する必要があります。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| API キー作成 | 管理者のネームスペース内でのみ作成可能。ネームスペース省略、グローバル指定、他ネームスペース指定は HTTP 403。 |
| API キー一覧取得 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーは除外。 |
| API キーの読み取り・更新・削除 | 管理者のネームスペース内のキーのみ操作可能。他ネームスペースのキーは HTTP 404（存在を秘匿）。 |
| API キーのネームスペース変更 | 不可。更新は HTTP 400。 |

グローバルダッシュボード管理者は引き続き全ネームスペースの API キーを管理可能です。

### ロールと権限

REST API はロールベースアクセス制御を実装しています。API キー作成時に以下の3つのプリセットロールのいずれかを割り当てられます。

- **administrator**（管理者）：すべてのリソースにアクセス可能。指定がなければデフォルト。ロール識別子は `administrator`。
- **viewer**（ビューア）：リソースの閲覧のみ可能。REST API のすべての GET リクエストに対応。ロール識別子は `viewer`。
- **publisher**（パブリッシャー）：MQTT メッセージのパブリッシュ専用。メッセージパブリッシュ関連 API のみアクセス可能。ロール識別子は `publisher`。

::: tip 注意
`publisher` キーは `publish` スコープのみ許容します。スコープ割り当て時に `publish` 以外のスコープを指定すると HTTP 400 となります。キーのロールを `publisher` に変更する場合は、同時リクエスト内で `"scopes": ["publish"]` または空リストを含めてください。そうしないと既存スコープに `publish` 以外がある場合、リクエストは拒否されます。
:::

### API スコープ

スコープは API キーごとに許可された REST API の業務領域を示す権限の次元です。スコープと [ロールと権限](#roles-and-permissions) は独立しており、両方のチェックを通過した場合のみアクセスが許可されます。つまり、2層のアクセス制御を形成します。

| 次元 | 目的 | 粒度 |
| ---- | ---- | ---- |
| **ロール** | HTTP 動詞の制限（読み取り専用、書き込み、パブリッシュのみなど） | リクエストアクション |
| **スコープ** | API ドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方が行われ、両方合格した場合のみ許可されます。

マイクロサービスや統合シナリオでは、外部システムが EMQX 管理面の一部のみアクセスすることが多いです。例えば監視プラットフォームは `monitoring` スコープのみ、ルールパブリッシュサービスは `data_integration` のみ、クラスター運用ツールは `cluster_operations` のみ必要です。スコープを使うことで最小権限の原則を適用し、キー漏洩時の影響範囲を最小化できます。

::: tip
スコープ名は安定した識別子であり、EMQX のアップグレードによって変更されません。OpenAPI タグの名称が変わっても、同じスコープを持つキーは引き続き機能します。
:::

#### 組み込みの API キースコープ

EMQX は API キー向けに以下の10個のスコープを提供しています。

| スコープ | 名称 | 主な API 領域 |
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

::: warning 管理者相当スコープと制限付きスコープを混在させないでください

EMQX は `system`、`user_management`、`api_key_management`、`sso_management` を管理者相当スコープ（検証メッセージでは特権スコープ）として分類しています。これらを制限付きスコープと混在させてもアカウントの実効権限は減りません。うち API キーに割り当て可能なのは `system` のみで、他3つは [ログイン専用スコープ](#login-only-scopes) に分類されます。

そのため、EMQX 6.0.4 以降、API キー作成・更新時の明示的なスコープリストは `system` のみ、または `system` を含まないスコープ群のいずれかでなければなりません。混在したリストは HTTP 400 となり変更は適用されません。

既存の混在リストは引き続き有効で `system` は有効なままです。次回の明示的なスコープ更新は `system` のみ、または `system` を含まないリストである必要があります。ダッシュボードで編集する際は保存前に権限モードの選択が求められます。

:::

#### ログイン専用スコープ

API キースコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、API キーには割り当てられません。ログインユーザーへの割り当てと適用の詳細は [ログインユーザースコープ](../dashboard/system.md#login-user-scopes) を参照してください。

| スコープ | 必要ロール | 用途 |
| --- | --- | --- |
| `user_management` | 管理者 | ダッシュボードユーザー管理 |
| `sso_management` | 管理者 | SSO バックエンドおよびユーザーレコード管理 |
| `api_key_management` | 管理者 | API キー管理 |
| `mfa_management` | 任意 | 自身の MFA 管理。管理者は他ユーザーの MFA も管理可能。 |

#### ネームスペース制限付き呼び出し元の制限

ネームスペース制限付き呼び出し元（ロールが特定ネームスペースに制限されたユーザーや API キー）は、スコープチェックに加えエンドポイントレベルの追加制限を受けます。スコープ付与はこれらの制限を上書きしません。

ネームスペース制限付き API キーはメッセージパブリッシュ API（`POST /api/v5/publish` を含む）を呼び出せません。スコープリストに `publish` が含まれていても制限は解除されません。

ネームスペース制限付き呼び出し元が `connections` または `monitoring` スコープを持っていても、保持メッセージや遅延メッセージストアなどの MQTT メッセージコンテンツを読み書きするクラスター全体のエンドポイントにはアクセスできません。以下のメッセージ関連エンドポイントは `403 Forbidden` を返します。

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

トレース操作では、`GET /trace` は呼び出し元のネームスペース内のトレースのみを一覧表示します。以下のトレース単位操作は、異なるネームスペースのトレースに対しては `404 Not Found` を返します。

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この挙動により他ネームスペースのトレース情報の漏洩を防止します。バルク削除操作（`DELETE /trace`）はネームスペース制限付き呼び出し元に対して `403 Forbidden` を返し、全トレースのクリアはグローバル管理者のみ可能です。

ダッシュボードログイン、SSO コールバック、API キーの自己管理エンドポイント（例：`/api_key`）は、API キー認証を受け付けません。これはスコープモデルとは無関係なダッシュボードのセキュリティ境界です。

#### `scopes` のデフォルト挙動

EMQX 6.0.4 以降、API キーの `scopes` フィールドは以下のルールに従います。

| `scopes` の値 | 意味 |
| --- | --- |
| **作成リクエストで未指定** | 選択されたロールのデフォルトを使用 |
| **更新リクエストで未指定** | キーの現在のスコープ設定を保持 |
| **ロールデフォルトのセントネル `"unset"`** | 明示的なスコープ設定を解除しロールデフォルトを使用。ロールデフォルトの変更は自動反映。 |
| **空リスト `[]`** | すべての業務エンドポイントを拒否。キーをソフト無効化する用途に有効。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定されたスコープの API のみ許可 |

明示的リストがロールデフォルトと同じスコープセットの場合、`"unset"` と同等の効果を持ちます。キーはロールデフォルトの変更に追従します。順序は問われません。

ブートストラップファイルのエントリでスコープセグメントを省略した場合、EMQX は指定されたロールのデフォルトを適用します。

スコープはキーがアクセス可能な API 領域を決定しますが、ロールやネームスペースの制限を上書きしません。リクエストはロール・スコープ・ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能なスコープの一覧取得

EMQX は以下の2つのエンドポイントでスコープカタログを取得可能です。

- `GET /api/v5/api_key_scopes`：API キーに割り当て可能なスコープ（上記10個の業務ドメインスコープ）を返します。API キー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（ログイン専用スコープ4つを含む）を返します。ベアラートークン認証が必要です。

スコープ選択 UI の構築や自動化スクリプトの検証に利用してください。

```bash
# API キースコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの方法で設定可能です。

- **ダッシュボード**：**システム** -> **API キー** の作成・編集時に **権限モード** を選択し、**カスタム制限付き権限** 選択時に個別スコープを指定。
- **REST API**：作成・更新リクエストボディに `"scopes": ["monitoring", "cluster_operations"]` を含める。
- **ブートストラップファイル**：各行の4番目のセグメントにカンマ区切りのスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ページネーション

大量データを扱う一部の API ではページネーション機能が提供されています。データの特性に応じて2種類のページネーション方式があります。

### ページ番号によるページネーション

ページネーション対応の多くの API では、`page`（ページ番号）と `limit`（ページサイズ）パラメータで制御します。最大ページサイズは `10000` です。`limit` 未指定時はデフォルトで `100` となります。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれます。EMQX は検索条件付きリクエストの総件数を予測できないため、`meta.hasnext` が次ページの有無を示します。

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

データ変動が激しくページ番号方式が非効率な一部の API では、カーソルページネーションを採用しています。

`position` または `cursor`（開始位置）パラメータで読み込み開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置からの件数を指定します。最大ページサイズは `10000` です。`limit` 未指定時はデフォルトで `100` となります。

例：

```bash
GET /clients/{clientid}/mqueue_messages?position=1716187698257189921_0&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれ、`meta.position` または `meta.cursor` が次ページの開始位置を示します。

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

この方式はデータ変動が激しいシナリオで効率的かつ連続的なデータ取得を可能にします。

## エラーコード

HTTP レスポンスステータスコードに加え、EMQX は特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時はレスポンスボディに JSON 形式でエラーコードが返されます。

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| エラーコード                                    | 説明                                                         |
| ---------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています。               |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワード、または API キー＆シークレットが間違っています。 |
| BAD_REQUEST                                    | リクエストパラメータが不正です。                             |
| NOT_MATCH                                      | 条件が一致しません。                                         |
| ALREADY_EXISTS                                 | リソースが既に存在します。                                   |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です。                                       |
| BAD_LISTENER_ID                                | リスナー ID が不正です。                                     |
| BAD_NODE_NAME                                  | ノード名が不正です。                                         |
| BAD_RPC                                        | RPC 失敗。クラスター状態と対象ノードの状態を確認してください。 |
| BAD_TOPIC                                      | トピック構文エラー。トピックは MQTT プロトコル標準に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成しようとしたリソースが最大または最小制限を超えています。 |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています。       |
| CONFLICT                                       | リクエストリソースに競合があります。                         |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使用されていません。     |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています。                     |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージ ID の解析エラー。                                 |
| INVALID_ID                                     | ID スキーマが不正です。                                      |
| MESSAGE_ID_NOT_FOUND                           | メッセージ ID が存在しません。                               |
| NOT_FOUND                                      | リソースが見つかりません。                                   |
| CLIENTID_NOT_FOUND                             | クライアント ID が見つかりません。                           |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常 MQTT クライアントではありません）。 |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません。                                   |
| TOPIC_NOT_FOUND                                | トピックが見つかりません。                                   |
| USER_NOT_FOUND                                 | ユーザーが見つかりません。                                   |
| INTERNAL_ERROR                                 | サーバ内部エラーです。                                       |
| SERVICE_UNAVAILABLE                            | サービスが利用できません。                                   |
| SOURCE_ERROR                                   | ソースエラーです。                                           |
| UPDATE_FAILED                                  | 更新に失敗しました。                                         |
| REST_FAILED                                    | ソースまたは設定のリセットに失敗しました。                   |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません。                                 |
