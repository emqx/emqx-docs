# REST API

EMQXは、OpenAPI（Swagger）3.0仕様に準拠したHTTP管理APIを公開しています。

EMQXはREST APIを探索および操作するための複数の方法を提供しています。EMQX起動後、以下のAPI仕様エンドポイントが利用可能です：

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウンスタイルのAPIリファレンスページ。 |
| `/api-spec.md` | Markdown | Markdown形式のAPIリファレンス。AIエージェントや自動化ツール向け。 |
| `/api-spec.json` | JSON | JSON形式のOpenAPI 3.0仕様。スクリプトやプログラムツール向け。 |
| `/api-docs/index.html` | HTML | ブラウザ上でAPIコールを直接テストできるインタラクティブなSwagger UI。**非推奨**：v7で削除予定。 |

上記のすべてのエンドポイントは、ダッシュボード設定で `swagger_support` が `true`（デフォルト）に設定されている必要があります。APIドキュメントエンドポイントを無効化するには `false` に設定してください。詳細は[ダッシュボード設定](../configuration/dashboard.md)を参照してください。

本節ではEMQX REST APIの利用方法を紹介します。

## 基本パス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降のすべてのAPIパスは `/api/v5` で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは、`Accept` ヘッダーに `application/json` を設定する必要があります。これにより、レスポンスはJSON形式で返されます（特に指定がない限り）。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)の標準に従っています。主なステータスコードは以下の通りです：

| コード | 説明 |
| ----- | ------------------------------------------------------------ |
| 200   | リクエスト成功。返却されるJSONデータに詳細が含まれます。 |
| 201   | 作成成功。新規オブジェクトがBodyに返されます。 |
| 204   | リクエスト成功。通常は削除や更新操作で返却Bodyは空です。 |
| 400   | 不正なリクエスト。リクエストボディやパラメータのエラー。 |
| 401   | 認証失敗。APIキーが期限切れか存在しません。 |
| 403   | 禁止。オブジェクトが使用中または依存関係制約があります。 |
| 404   | 見つかりません。Bodyの `message` フィールドで理由を確認可能。 |
| 409   | コンフリクト。オブジェクトが既に存在するか数の上限超過。 |
| 500   | サーバ内部エラー。Bodyやログで原因を確認してください。 |

## 認証

EMQXのREST APIは主に2つの認証方法をサポートしています：APIキーを用いたベーシック認証とベアラートークン認証です。

### APIキーを用いたベーシック認証

この方法では、APIキーとシークレットキーをユーザー名とパスワードとして使用し、APIリクエストを認証します。EMQXのREST APIは[HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを使用する前にAPIキーを作成する必要があります。詳細は[APIキー管理](#api-key-management)を参照してください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0以降はダッシュボードのユーザー認証情報をREST API認証に使用できません。代わりにAPIキーを作成して認証に使用してください。

:::

#### APIキーで認証する

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

#### ベアラートークンを取得する

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

ベアラートークンを取得したら、APIリクエストの `Authorization` ヘッダーに以下のように含めます：

```bash
--header "Authorization: Bearer <your-token>"
```

## APIキー管理

このセクションでは、APIキーの作成・管理方法、およびロール、ネームスペース、スコープの設定方法を説明します。

### APIキーを作成する

#### ダッシュボード

ダッシュボードの **System** -> **API Keys** から手動でAPIキーを作成できます：

1. 右上の **+ Create** ボタンをクリックして作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **Name**（必須）：APIキーの名前を入力します。
   - **Expire At**：空欄の場合は期限なしとなります。
   - **Is Enable**：デフォルトで有効です。
   - **Role**：ロールを選択します（任意）。詳細は[ロールと権限](#roles-and-permissions)を参照してください。
   - **Namespace**：デフォルトはオフ。グローバル管理者の場合はオフのままでグローバルAPIキーを作成します。オンにしてネームスペースを選択すると、そのネームスペース内のキーを作成します。ネームスペース管理者は自分のネームスペース内でのみキーを作成可能です。
   - **Permission Mode**：管理者または閲覧者キーの場合、スコープの割り当て方法を選択します。パブリッシャーキーには表示されません（ロールデフォルトの `publish` スコープを使用）。スコープの動作と制限は[APIスコープ](#api-scopes)を参照してください。
     - **Role Default Scopes**：選択したロールのデフォルトを使用。ロールデフォルトの変更は自動的に反映されます。
     - **System-level Permissions**：`system` スコープのみ付与。
     - **Custom Restricted Permissions**：1つ以上のスコープを選択し、アクセス可能なAPI領域を制限します。**Scopes**を空欄にするとスコープ保護されたAPIにアクセスできません。
   - **Scopes**：**Custom Restricted Permissions** 選択時に表示。付与するスコープを選択します。
   - **Note**：任意で説明を入力します。
3. **Confirm** をクリックすると、APIキーとシークレットキーが **作成成功** ダイアログに表示されます。

   ::: warning 重要

   APIキーとシークレットキーはこの時点で必ず保存してください。シークレットキーは再表示されません。

   :::

4. **Close** をクリックしてダイアログを閉じます。

**Permission Mode** はダッシュボードのみで利用可能です。REST APIを利用する場合は `scopes` フィールドを直接設定してください。詳細は[scopesのデフォルト動作](#default-behavior-of-scopes)を参照してください。

キー名をクリックすると詳細を確認でき、**Edit** ボタンで有効期限、状態、ロール、パーミッションモード、スコープ、説明を変更できます。**Delete** ボタンでキーを削除できます。

#### REST API

REST API経由でAPIキーを作成・更新するには、ダッシュボードユーザーのベアラートークンを使用します。APIキー管理エンドポイントはAPIキー認証を受け付けません。

EMQX 6.0.4以降、`POST /api/v5/api_key` および `PUT /api/v5/api_key/:name` のリクエストボディにトップレベルの `namespace` フィールドを指定可能です。例として、`team-a` ネームスペースに管理者APIキーを作成するリクエスト：

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

ネームスペースは以下のいずれかの方法で指定できます：

- `administrator` のようなロールと `namespace` フィールドを併用する方法
- ロールに `ns:<namespace>::<role>` の形式でネームスペースを埋め込む方法（例：`ns:team-a::administrator`）

両方の形式がサポートされており、両方がリクエストに含まれる場合はネームスペースが一致している必要があります。異なる場合や空の場合はHTTP 400を返します。APIキー作成後にネームスペースを変更することはできません。

#### ブートストラップファイル

ブートストラップファイル方式でもAPIキーを作成できます。以下の設定でファイルの場所を指定します：

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイル内に複数のAPIキーを `{API Key}:{Secret Key}:{?Role}:{?Scopes}` 形式で改行区切りで記述します：

- **API Key**：キー識別子として任意の文字列
- **Secret Key**：ランダムな文字列を使用
- **Role（任意）**：キーの[ロール](#roles-and-permissions)
- **Scopes（任意）**：キーがアクセス可能な[APIスコープ](#api-scopes)をカンマ区切りで指定。省略時はロールのデフォルトが適用されます。ログイン専用スコープ（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）はAPIキーには無効です。これらが含まれる場合、EMQX起動時に削除され警告ログが出力されますが、キーはスコープなしで作成されます。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

APIキーに割り当て可能なスコープのうち、`system` のみが管理者相当の権限を付与します。EMQX 6.0.4以降、ブートストラップファイルで管理者相当スコープと制限付きスコープを混在させた場合、管理者相当スコープはすべて削除され、残りのスコープが適用されます。警告ログが出力され、キーは作成・更新されます。一方、REST APIでは混在したスコープリストはHTTP 400で拒否され、変更は適用されません。

この方法で作成されたAPIキーは無期限に有効です。

EMQX起動時にファイル内のデータがAPIキーリストに追加されます。既存キーがあればシークレットキー、ロール、スコープが更新されます。

### ネームスペース管理者によるAPIキー管理

EMQX 6.0.4以降、ネームスペース管理者は自分のネームスペース内のAPIキーを管理できます。認証にはベアラートークンが必要です。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| APIキー作成 | 管理者のネームスペース内のみ作成可能。ネームスペース省略、グローバル指定、他ネームスペース指定はHTTP 403。 |
| APIキー一覧取得 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーはレスポンスから除外。 |
| APIキーの読み取り・更新・削除 | 管理者のネームスペース内のキーのみ操作可能。他ネームスペースのキーはHTTP 404で存在を隠蔽。 |
| APIキーのネームスペース変更 | 不可。更新はHTTP 400。 |

グローバル管理者は引き続き全ネームスペースのAPIキーを管理できます。

### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つのプリセットロールのいずれかを割り当てられます：

- **Administrator**：すべてのリソースにアクセス可能。ロール識別子は `administrator`。指定がなければデフォルト。
- **Viewer**：リソースやデータの閲覧のみ可能。REST APIのGETリクエストに対応。ロール識別子は `viewer`。
- **Publisher**：MQTTメッセージのパブリッシュ専用。メッセージパブリッシュ関連APIのみアクセス可能。ロール識別子は `publisher`。

::: tip 注意
`publisher` キーは `publish` スコープのみ許容します。スコープ割り当て時に `publish` 以外が含まれるとHTTP 400を返します。キーのロールを `publisher` に変更する場合、同時に `"scopes": ["publish"]` または空リストを指定してください。そうしないと既存スコープに `publish` 以外がある場合、リクエストは拒否されます。
:::

### APIスコープ

スコープはAPIキーごとの権限の次元で、キーがアクセス可能なREST APIの業務領域を宣言します。スコープと[ロールと権限](#roles-and-permissions)は独立しており、両方のチェックを通過した場合にアクセスが許可されます。つまり、2層のアクセス制御を形成します：

| 次元 | 目的 | 粒度 |
| --------- | ------- | ----------- |
| **ロール** | HTTPメソッドの制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | APIドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方を通過する必要があります。

マイクロサービスや統合シナリオでは、外部システムがEMQXの管理面の一部だけにアクセスすることが多いです。例えば監視プラットフォームは `monitoring` スコープのみ、ルールパブリッシュサービスは `data_integration` のみ、クラスター運用ツールは `cluster_operations` のみ必要です。スコープを使うことで最小権限の原則に基づきキーを割り当てられ、キーが漏洩した際の影響範囲を最小化できます。

::: tip
スコープ名はEMQXのアップグレード間でも安定した識別子です。OpenAPIタグ名が変更されても、同じスコープを持つキーは引き続き動作します。
:::

#### 組み込みAPIキー用スコープ

EMQXはAPIキー用に以下10個のスコープを提供しています：

| スコープ | 名称 | 典型的なAPI領域 |
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

::: warning 管理者相当スコープと制限付きスコープを混在させないでください

EMQXは `system`、`user_management`、`api_key_management`、`sso_management` を管理者相当スコープ（検証メッセージでは `privilege scopes`）と分類しています。これらを制限付きスコープと混在させてもアカウントの実効権限は減りません。4つのうちAPIキーに割り当て可能なのは `system` のみで、他3つは[ログイン専用スコープ](#login-only-scopes)です。

そのためEMQX 6.0.4以降、APIキー作成・更新時の明示的なスコープリストは `system` のみ、または `system` を含まないスコープのいずれかでなければなりません。混在リストはHTTP 400で拒否され、変更は適用されません。

既存の混在スコープリストは引き続き有効で `system` は有効なままです。次回の明示的なスコープ更新は `system` のみ、または `system` を含まないリストで行う必要があります。ダッシュボードで編集する際は保存前にパーミッションモードの選択を促されます。

:::

#### ログイン専用スコープ

APIキー用スコープに加え、ダッシュボードログインユーザーには4つのログイン専用スコープがあり、ブラウザセッション専用でAPIキーには割り当てられません。詳細は[ログインユーザースコープ](../dashboard/system.md#login-user-scopes)を参照してください。

| スコープ | 必要ロール | 目的 |
| --- | --- | --- |
| `user_management` | Administrator | ダッシュボードユーザー管理。 |
| `sso_management` | Administrator | SSOバックエンドおよびSSOユーザーレコード管理。 |
| `api_key_management` | Administrator | APIキー管理。 |
| `mfa_management` | 任意 | 自身のMFA管理。管理者は他ユーザーのMFAも管理可能。 |

#### ネームスペース制限付き呼び出し元の制限

ネームスペース制限付き呼び出し元（ロールが特定ネームスペースに制限されたユーザーやAPIキー）は、スコープチェックに加えエンドポイントレベルの追加制限を受けます。スコープ付与はこれらの制限を上書きしません。

ネームスペース制限付きAPIキーはメッセージパブリッシュAPI（`POST /api/v5/publish` など）を呼び出せません。スコープに `publish` が含まれていても制限は解除されません。

また、`connections` や `monitoring` スコープを持っていても、ネームスペース制限付き呼び出し元はクラスター全体のMQTTメッセージの生データ（保持メッセージや遅延メッセージストア）を読み書きするエンドポイントにアクセスできません。以下のメッセージ関連エンドポイントは `403 Forbidden` を返します：

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

トレース操作では、`GET /trace` は呼び出し元のネームスペース内のトレースのみを一覧表示します。以下のトレース単位操作は、トレースが別ネームスペースに属する場合 `404 Not Found` を返します：

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この動作により他ネームスペースのトレースの存在が漏れません。トレースの一括削除操作（`DELETE /trace`）はネームスペース制限付き呼び出し元に対して `403 Forbidden` を返し、全トレースのクリアはグローバル管理者のみが可能です。

ダッシュボードログイン、SSOコールバック、APIキーの自己管理エンドポイント（例：`/api_key`）は、キーの `scopes` 設定に関わらずAPIキー認証を受け付けません。これはスコープモデルとは無関係のダッシュボードのセキュリティ境界です。

#### `scopes` のデフォルト動作

EMQX 6.0.4以降、APIキーの `scopes` フィールドは以下のルールに従います：

| `scopes` の値 | 意味 |
| --- | --- |
| **作成リクエストで未指定** | 選択ロールのデフォルトスコープを使用。 |
| **更新リクエストで未指定** | キーの現在のスコープ設定を保持。 |
| **ロールデフォルトのセントネル `"unset"`** | 明示的なスコープ設定を解除し、ロールのデフォルトを使用。ロールデフォルトの変更は自動反映。 |
| **空リスト `[]`** | すべての業務エンドポイントを拒否。キーを削除せずにソフト無効化可能。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定したスコープのみ許可。 |

明示的リストがロールデフォルトと同じスコープセットの場合、 `"unset"` と同じ効果でロールデフォルトの変更を追従します。順序は問われません。

ブートストラップファイルのエントリでスコープセグメントを省略すると、指定ロールのデフォルトが適用されます。

スコープはキーがアクセス可能なAPI領域を決定し、ロールやネームスペースの制限を上書きしません。リクエストはロール、スコープ、ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能なスコープの一覧取得

EMQXは利用可能なスコープカタログを問い合わせるために以下の2つのエンドポイントを公開しています：

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記10個の業務ドメインスコープ）を返します。APIキー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（ログイン専用スコープ4つを含む）を返します。ベアラートークン認証が必要です。

これらのエンドポイントはスコープ選択UIの構築や自動化スクリプトの検証に利用できます：

```bash
# APIキー用スコープ一覧取得
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ一覧取得（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの入口から設定可能です：

- **ダッシュボード**：**System** -> **API Keys** でキー作成・編集時に **Permission Mode** を選択し、**Custom Restricted Permissions** の場合に個別スコープを選択。
- **REST API**：作成・更新リクエストボディに `"scopes": ["monitoring", "cluster_operations"]` を含める。
- **ブートストラップファイル**：各行の4番目のセグメントにカンマ区切りでスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ページネーション

大量データを扱うAPIの一部ではページネーション機能が提供されています。データの特性に応じて2種類のページネーション方式があります。

### ページ番号ページネーション

ページネーション対応APIの多くは、`page`（ページ番号）と `limit`（ページサイズ）パラメータで制御します。最大ページサイズは `10000` です。`limit` 未指定時はデフォルトで `100` となります。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総件数を予測できないため、`meta.hasnext` フィールドで次ページの有無を示します：

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

データ変動が激しくページ番号ページネーションが非効率な一部APIでは、カーソルページネーションを採用しています。

`position` または `cursor`（開始位置）パラメータで読み込み開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置からの件数を指定します。最大ページサイズは `10000` です。`limit` 未指定時はデフォルトで `100` となります。

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

HTTPレスポンスステータスコードに加え、EMQXは特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時、BodyにJSON形式でエラーコードが返されます：

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
| BAD_LISTENER_ID                                | リスナーIDが不正です                                              |
| BAD_NODE_NAME                                  | ノード名が不正です                                                |
| BAD_RPC                                        | RPC失敗。クラスター状態および対象ノードの状態を確認してください。 |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成しようとしたリソースが最大または最小制限を超えています。 |
| INVALID_PARAMETER                              | リクエストパラメータが不正か境界値を超えています。   |
| CONFLICT                                       | リクエストリソースが競合しています。                                |
| NO_DEFAULT_VALUE                               | リクエストパラメータがデフォルト値を使用していません。                 |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています。                          |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージIDの解析エラー                                     |
| INVALID_ID                                     | IDスキーマが不正です                                                |
| MESSAGE_ID_NOT_FOUND                           | メッセージIDが存在しません                                    |
| NOT_FOUND                                      | リソースが見つかりません。                         |
| CLIENTID_NOT_FOUND                             | クライアントIDが見つかりません。                        |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常はMQTTクライアントではありません） |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません                                           |
| TOPIC_NOT_FOUND                                | トピックが見つかりません                                              |
| USER_NOT_FOUND                                 | ユーザーが見つかりません                                               |
| INTERNAL_ERROR                                 | サーバ内部エラー                                           |
| SERVICE_UNAVAILABLE                            | サービス利用不可                                          |
| SOURCE_ERROR                                   | ソースエラー                                                 |
| UPDATE_FAILED                                  | 更新に失敗しました                                                 |
| REST_FAILED                                    | ソースまたは設定のリセットに失敗しました                          |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません                                        |
