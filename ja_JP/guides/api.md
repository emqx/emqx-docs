# REST API

EMQX は OpenAPI（Swagger）3.0 仕様に準拠した HTTP 管理 API を公開しています。

EMQX では REST API を探索・操作するための複数の方法を提供しています。EMQX 起動後、以下の API 仕様エンドポイントが利用可能です。

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウン形式の API リファレンスページ |
| `/api-spec.md` | Markdown | Markdown 形式の API リファレンス。AI エージェントや自動化ツール向け |
| `/api-spec.json` | JSON | OpenAPI 3.0 仕様の JSON 形式。スクリプトやプログラム的ツール向け |
| `/api-docs/index.html` | HTML | ブラウザ上で API 呼び出しを直接試せるインタラクティブな Swagger UI。**非推奨**：v7 で削除予定 |

上記すべてのエンドポイントは、ダッシュボード設定で `swagger_support` が `true`（デフォルト）に設定されている必要があります。`false` に設定すると、すべての API ドキュメントエンドポイントが無効になります。詳細は [Dashboard configuration](configuration/dashboard.md) を参照してください。

本節では EMQX REST API の利用方法を紹介します。

## 基本パス

EMQX の REST API はバージョン管理されており、EMQX 5.0.0 以降のすべての API パスは `/api/v5` で始まります。

## HTTP ヘッダー

ほとんどの API リクエストでは `Accept` ヘッダーに `application/json` を指定する必要があり、特に指定がなければレスポンスは JSON 形式で返されます。

## HTTP レスポンスステータスコード

EMQX は [HTTP レスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status) 標準に準拠しています。主なステータスコードは以下の通りです。

| コード | 説明 |
| ----- | ------------------------------------------------------------ |
| 200   | リクエスト成功。返却される JSON データに詳細情報が含まれます |
| 201   | 作成成功。新規オブジェクトがボディに返されます |
| 204   | リクエスト成功。通常は削除や更新操作で返却ボディは空です |
| 400   | 不正なリクエスト。リクエストボディやパラメータのエラー |
| 401   | 認証失敗。API キーが期限切れか存在しません |
| 403   | 禁止。オブジェクトが使用中か依存関係制約があります |
| 404   | 見つかりません。ボディの `message` フィールドで理由を確認可能 |
| 409   | 競合。オブジェクトが既に存在するか数の上限超過 |
| 500   | サーバ内部エラー。ボディやログで原因を確認してください |

## 認証

EMQX の REST API は主に API キーを使ったベーシック認証とベアラートークン認証の2つの方法をサポートしています。

### API キーを使ったベーシック認証

この方法では、API キーとシークレットキーをユーザー名とパスワードとして API リクエストの認証に使用します。EMQX の REST API は [HTTP ベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework) に準拠しており、これらの認証情報が必要です。EMQX REST API を利用する前に API キーを作成してください。詳細は [API Key Management](#api-key-management) を参照してください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0 以降はダッシュボードのユーザー認証情報を REST API 認証に使用できません。代わりに API キーを作成して認証に使用してください。

:::

#### API キーで認証する

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

API キー認証の代替として、ベアラートークンを使った安全かつプログラム的な EMQX REST API へのアクセスも可能です。ベアラートークンは以下のログイン API エンドポイントにリクエストを送信して取得します。

#### ベアラートークンを取得する

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

レスポンスにベアラートークンが含まれます。これを API リクエストの認証に使用します。

#### ベアラートークンを使った認証

ベアラートークンを取得したら、API リクエストの `Authorization` ヘッダーに以下のように含めてください。

```bash
--header "Authorization: Bearer <your-token>"
```

## API キー管理

この節では API キーの作成・管理方法、およびロール、ネームスペース、スコープの設定について説明します。

### API キーを作成する

#### ダッシュボード

ダッシュボードの **System** -> **API Keys** から手動で API キーを作成できます。

1. 右上の **+ Create** ボタンをクリックして作成ダイアログを開きます。
2. API キーの詳細を設定します：
   - **Name**（必須）：API キーの名前を入力します。
   - **Expire At**：空欄のままにすると期限なしになります。
   - **Is Enable**：デフォルトで有効です。
   - **Role**：ロールを選択します（任意）。詳細は [Roles and Permissions](#roles-and-permissions) を参照してください。
   - **Namespace**：デフォルトはオフです。グローバル管理者の場合はオフのままでグローバル API キーが作成されます。オンにしてネームスペースを選択すると、そのネームスペース内のキーが作成されます。ネームスペース管理者は自分のネームスペース内でのみキーを作成できます。
   - **Permission Mode**：管理者またはビューアのキーで、スコープの割り当て方法を選択します。パブリッシャーキーには表示されません。スコープの動作や制限は [API Scopes](#api-scopes) を参照してください。
     - **Role Default Scopes**：選択したロールのデフォルトを使用します。ロールのデフォルト変更は自動的に反映されます。
     - **System-level Permissions**：`system` スコープのみを付与します。
     - **Custom Restricted Permissions**：1つ以上のスコープを選択してアクセス可能な API 領域を制限します。**Scopes** を空欄にするとスコープ保護された API にアクセスできません。
   - **Scopes**：**Custom Restricted Permissions** 選択時に表示され、付与するスコープを選択します。
   - **Note**：任意で説明を入力します。
3. **Confirm** をクリックすると、作成成功ダイアログに API キーとシークレットキーが表示されます。

   ::: warning 重要

   API キーとシークレットキーは必ずすぐに保存してください。シークレットキーは再表示されません。

   :::

4. **Close** をクリックしてダイアログを閉じます。

**Permission Mode** はダッシュボードのみで利用可能です。REST API では `scopes` フィールドを直接設定します。詳細は [Default Behavior of `scopes`](#default-behavior-of-scopes) を参照してください。

キー名をクリックすると詳細を確認でき、**Edit** ボタンで有効期限、状態、ロール、パーミッションモード、スコープ、説明を変更可能です。**Delete** ボタンでキーを削除できます。

#### REST API

REST API ではダッシュボードユーザーのベアラートークンを使って API キーを作成・更新します。API キー管理エンドポイントは API キー認証を受け付けません。

EMQX 6.0.4 以降、`POST /api/v5/api_key` および `PUT /api/v5/api_key/:name` のリクエストボディにトップレベルの `namespace` フィールドを指定可能です。例として、`team-a` ネームスペースに管理者 API キーを作成するリクエストは以下の通りです。

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

`scopes` に `"unset"` を指定するとロールのデフォルトスコープが明示的に適用されます。作成リクエストで `scopes` を省略しても同様の効果です。

ネームスペースは以下のいずれかの方法で指定できます。

- `administrator` のようなロール名と `namespace` フィールドを併用する。
- `ns:<namespace>::<role>` の形式でロールにネームスペースを埋め込む（例：`ns:team-a::administrator`）。

両方の形式は引き続きサポートされます。両者が混在する場合、ネームスペースが一致しないと HTTP 400 が返されます。`namespace` が空の場合も同様です。API キーのネームスペースは作成後に変更できません。

#### ブートストラップファイル

ブートストラップファイル方式でも API キーを作成できます。以下の設定ファイルでファイルの場所を指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイルには複数の API キーを `{API Key}:{Secret Key}:{?Role}:{?Scopes}` 形式で改行区切りで記述します。

- **API Key**：キー識別子として任意の文字列
- **Secret Key**：ランダム文字列をシークレットキーとして使用
- **Role（任意）**：キーの [ロール](#roles-and-permissions)
- **Scopes（任意）**：キーがアクセス可能な [API スコープ](#api-scopes) をカンマ区切りで指定。省略時はロールのデフォルト。ログイン専用スコープ（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）は API キーには無効です。これらがブートストラップファイルに含まれると、起動時に削除され警告ログが出力されます。キーはスコープなしで作成されます。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

API キーに割り当て可能なスコープのうち、`system` のみが管理者相当の権限を付与します。EMQX 6.0.4 以降、管理者相当スコープと管理者相当でないスコープが混在する場合、EMQX は管理者相当スコープをすべて削除し、残りのスコープを保持して警告ログを出力し、キーの作成・更新を続行します。REST API ではこのような混在スコープは HTTP 400 で拒否され、変更は適用されません。

この方法で作成された API キーは無期限で有効です。

EMQX 起動時にファイルの内容が API キーリストに追加されます。既存のキーがあればシークレットキー、ロール、スコープが更新されます。

### ネームスペース管理者による API キー管理

EMQX 6.0.4 以降、ネームスペース管理者は自身のネームスペース内の API キーを管理可能です。管理者はベアラートークンで認証する必要があります。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| API キー作成 | 管理者のネームスペース内でのみ作成可能。ネームスペース省略、グローバル指定、他ネームスペース指定は HTTP 403 を返す。 |
| API キー一覧取得 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーはレスポンスから除外。 |
| API キーの読み取り・更新・削除 | 管理者のネームスペース内のキーのみ操作可能。他ネームスペースのキーは HTTP 404 を返し存在を隠蔽。 |
| API キーのネームスペース変更 | 他ネームスペースへの移動は不可。更新は HTTP 400 を返す。 |

グローバルダッシュボード管理者は引き続き全ネームスペースの API キーを管理可能です。

### ロールと権限

REST API はロールベースアクセス制御を実装しています。API キー作成時に以下の3つのプリセットロールのいずれかを割り当てます。

- **Administrator**：すべてのリソースにアクセス可能。指定がなければデフォルト。ロール識別子は `administrator`。
- **Viewer**：リソースやデータの閲覧のみ可能。REST API のすべての GET リクエストに対応。ロール識別子は `viewer`。
- **Publisher**：MQTT メッセージのパブリッシュ専用。メッセージパブリッシュ関連 API のみアクセス可能。ロール識別子は `publisher`。

::: tip 注意
`publisher` キーは `publish` スコープのみ許容します。スコープ割り当て時に `publish` 以外のスコープがあると HTTP 400 を返します。キーのロールを `publisher` に変更する場合は、同時に `"scopes": ["publish"]` または空リストを指定してください。そうしないと既存スコープに `publish` 以外が含まれている場合、リクエストは拒否されます。
:::

### API スコープ

スコープはキーごとの権限の次元で、REST API のどの業務領域にアクセスできるかを宣言します。スコープと [ロールと権限](#roles-and-permissions) は独立しており、両方のチェックを通過した場合にのみアクセスが許可されます。

| 次元 | 目的 | 粒度 |
| --------- | ------- | ----------- |
| **ロール** | HTTP 動詞を制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | API ドメインを制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方が行われ、両方を満たす場合のみ受け入れられます。

マイクロサービスや統合シナリオでは、外部システムが EMQX の管理領域の一部にのみアクセスすることが多いです。例えば監視プラットフォームは `monitoring` スコープのみ、ルールパブリッシュサービスは `data_integration` のみ、クラスター運用ツールは `cluster_operations` のみ必要です。スコープにより最小権限の原則でキーを割り当てられ、キー漏洩時の影響範囲を最小化できます。

::: tip
スコープ名は安定した識別子で、EMQX のアップグレード間で変更されません。OpenAPI タグ名が変わっても、同じスコープのキーは引き続き動作します。
:::

#### 組み込みの API キースコープ

EMQX は API キー向けに以下の10個のスコープを提供しています。

| スコープ | 名称 | 代表的な API 領域 |
| --- | --- | --- |
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

::: warning 管理者相当スコープと制限スコープを混在させないでください

EMQX は `system`、`user_management`、`api_key_management`、`sso_management` を管理者相当スコープ（検証メッセージでは `privilege scopes`）として分類しています。これらを制限スコープと組み合わせてもアカウントの実効権限は減りません。4つのうち API キーに割り当て可能なのは `system` のみです。その他は [ログイン専用スコープ](#login-only-scopes) に分類されます。

そのため EMQX 6.0.4 以降、API キー作成・更新時の明示的なスコープリストは `system` のみ、または `system` を含まないスコープ群のいずれかでなければなりません。混在リストは HTTP 400 を返し変更は適用されません。

既存の混在スコープリストは引き続き動作し、`system` は有効なままです。次回の明示的なスコープ更新時は `system` のみ、または `system` を含まないリストを指定してください。ダッシュボードで編集する際は保存前にパーミッションモードの選択を促されます。

:::

#### ログイン専用スコープ

API キースコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、API キーには割り当てられません。ログインユーザーへの割り当てと適用方法は [Login User Scopes](dashboard/system.md#login-user-scopes) を参照してください。

| スコープ | 必須ロール | 目的 |
| --- | --- | --- |
| `user_management` | Administrator | ダッシュボードユーザー管理 |
| `sso_management` | Administrator | SSO バックエンドおよび SSO ユーザーレコード管理 |
| `api_key_management` | Administrator | API キー管理 |
| `mfa_management` | 任意 | 自身の MFA 管理。管理者は他ユーザーの MFA も管理可能 |

#### ネームスペース制限付き呼び出し元の制限

ネームスペース制限付き呼び出し元（ロールが特定ネームスペースに制限されたユーザーまたは API キー）は、スコープチェックに加えてエンドポイントレベルの追加制限を受けます。スコープ付与はこれらの制限を上書きしません。

ネームスペース API キーはメッセージパブリッシュ API（`POST /api/v5/publish` を含む）を呼び出せません。スコープリストに `publish` が含まれていてもこの制限は解除されません。

`connections` や `monitoring` スコープを持っていても、ネームスペース呼び出し元はクラスター全体の MQTT メッセージの生データ（保持メッセージや遅延メッセージストアを含む）を読み書きするエンドポイントにアクセスできません。以下のメッセージ関連エンドポイントは `403 Forbidden` を返します。

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

トレース操作では、`GET /trace` は呼び出し元のネームスペース内のトレースのみを一覧表示します。以下のトレース単位操作は他ネームスペースのトレースに対して `404 Not Found` を返します。

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この挙動により他ネームスペースのトレースの存在が漏れません。まとめて削除する操作（`DELETE /trace`）はネームスペース呼び出し元に対して `403 Forbidden` を返し、グローバル管理者のみがすべてのトレースをクリア可能です。

ダッシュボードログイン、SSO コールバック、API キー自身の管理エンドポイント（例：`/api_key`）は、キーの `scopes` 設定に関わらず API キー認証を受け付けません。これはスコープモデルとは無関係のダッシュボードのセキュリティ境界です。

#### `scopes` のデフォルト動作

EMQX 6.0.4 以降、API キーの `scopes` フィールドは以下のルールに従います。

| `scopes` の値 | 意味 |
| --- | --- |
| **作成リクエストで未指定** | 選択されたロールのデフォルトスコープを使用 |
| **更新リクエストで未指定** | キーの現在のスコープ設定を維持 |
| **ロールデフォルトのセントネル `"unset"`** | 明示的なスコープ設定を解除し、ロールのデフォルトを使用。ロールデフォルトの変更は自動反映 |
| **空リスト `[]`** | すべての業務エンドポイントへのアクセスを拒否。キーを削除せずにソフト無効化可能 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定したスコープ内のリクエストのみ許可 |

明示的リストがロールデフォルトと同じスコープセットの場合、`"unset"` と同じ効果でロールデフォルトの変更を追従します。順序は問われません。

ブートストラップファイルのエントリでスコープ指定を省略した場合、指定されたロールのデフォルトが適用されます。

スコープはキーがアクセスできる API 領域を決定しますが、ロールやネームスペース制限を上書きしません。リクエストはロール、スコープ、ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能なスコープ一覧を取得する

EMQX は利用可能なスコープカタログを取得するための2つのエンドポイントを公開しています。

- `GET /api/v5/api_key_scopes`：API キーに割り当て可能なスコープ（上記10個の業務ドメインスコープ）を返します。API キー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーに利用可能なすべてのスコープ（ログイン専用スコープ4つを含む）を返します。ベアラートークン認証が必要です。

これらのエンドポイントはスコープ選択 UI の生成や自動化スクリプトの検証に利用できます。

```bash
# API キースコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの方法で設定可能です。

- **ダッシュボード**：**System** -> **API Keys** でキー作成・編集時に **Permission Mode** を選択。**Custom Restricted Permissions** で個別スコープを選択。
- **REST API**：作成・更新リクエストボディに `"scopes": ["monitoring", "cluster_operations"]` を含める。
- **ブートストラップファイル**：各行の4番目のセグメントとしてカンマ区切りのスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ページネーション

大量データを扱う一部 API ではページネーション機能を提供しています。データ特性に応じて2種類のページネーション方式があります。

### ページ番号ページネーション

ページネーション対応の多くの API では、`page`（ページ番号）と `limit`（ページサイズ）パラメータでページネーションを制御します。最大ページサイズは `10000` です。`limit` 未指定時はデフォルトで `100` です。

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

### カーソルページネーション

データが急速に変化し、ページ番号ページネーションが非効率な一部 API ではカーソルページネーションを使用します。

`position` または `cursor`（開始位置）パラメータで開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置から読み込む件数を指定します。最大ページサイズは `10000` です。`limit` 未指定時はデフォルトで `100` です。

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

この方式はデータ変動が激しいシナリオにおいて連続性と効率性を確保します。

## エラーコード

HTTP レスポンスステータスコードに加え、EMQX は特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時はボディに JSON 形式でエラーコードが返されます。

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| エラーコード                                    | 説明                                                  |
| ---------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています <img width=200/>                  |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはキー＆シークレットが間違っています                    |
| BAD_REQUEST                                    | リクエストパラメータが不正です                                 |
| NOT_MATCH                                      | 条件が一致しません                                       |
| ALREADY_EXISTS                                 | リソースが既に存在します                                      |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です                                 |
| BAD_LISTENER_ID                                | 不正なリスナー ID                                              |
| BAD_NODE_NAME                                  | 不正なノード名                                                |
| BAD_RPC                                        | RPC 失敗。クラスター状態および対象ノードの状態を確認してください |
| BAD_TOPIC                                      | トピック構文エラー。トピックは MQTT プロトコル標準に準拠する必要があります |
| EXCEED_LIMIT                                   | 作成しようとしたリソースが最大または最小制限を超えています |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています   |
| CONFLICT                                       | リクエストリソースが競合しています                                |
| NO_DEFAULT_VALUE                               | リクエストパラメータがデフォルト値を使用していません                 |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています                          |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージ ID の解析エラー                                     |
| INVALID_ID                                     | 不正な ID スキーマ                                                |
| MESSAGE_ID_NOT_FOUND                           | メッセージ ID が存在しません                                    |
| NOT_FOUND                                      | リソースが見つかりませんまたは存在しません                         |
| CLIENTID_NOT_FOUND                             | クライアント ID が見つかりませんまたは存在しません                        |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常は MQTT クライアントではありません） |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません                                           |
| TOPIC_NOT_FOUND                                | トピックが見つかりません                                              |
| USER_NOT_FOUND                                 | ユーザーが見つかりません                                               |
| INTERNAL_ERROR                                 | サーバ内部エラー                                           |
| SERVICE_UNAVAILABLE                            | サービス利用不可                                          |
| SOURCE_ERROR                                   | ソースエラー                                                 |
| UPDATE_FAILED                                  | 更新失敗                                                 |
| REST_FAILED                                    | リセットソースまたは設定失敗                          |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません                                        |
