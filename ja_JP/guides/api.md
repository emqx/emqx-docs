# REST API

EMQXは、OpenAPI（Swagger）3.0仕様に準拠したHTTP管理APIを公開しています。

EMQXはREST APIを探索および操作するための複数の方法を提供しています。EMQX起動後、以下のAPI仕様エンドポイントが利用可能です。

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウン形式のAPIリファレンスページ。 |
| `/api-spec.md` | Markdown | Markdown形式のAPIリファレンス。AIエージェントや自動化ツール向け。 |
| `/api-spec.json` | JSON | JSON形式のOpenAPI 3.0仕様。スクリプトやプログラムツール向け。 |
| `/api-docs/index.html` | HTML | ブラウザ上でAPI呼び出しを直接テストできるインタラクティブなSwagger UI。**非推奨**：v7で削除予定。 |

上記のすべてのエンドポイントは、ダッシュボード設定で`swagger_support`が`true`（デフォルト）に設定されている必要があります。`false`に設定するとすべてのAPIドキュメントエンドポイントが無効になります。詳細は[ダッシュボード設定](configuration/dashboard.md)を参照してください。

本節ではEMQX REST APIの利用方法を紹介します。

## 基本パス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降のすべてのAPIパスは`/api/v5`で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは`Accept`ヘッダーに`application/json`を設定する必要があり、特に指定がない限りレスポンスはJSON形式で返されます。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)の標準に従っています。主なステータスコードは以下の通りです。

| コード | 説明 |
| ----- | ------------------------------------------------------------ |
| 200   | リクエスト成功。返却されるJSONデータに詳細が含まれます。 |
| 201   | 作成成功。新規オブジェクトがBodyに返されます。 |
| 204   | リクエスト成功。通常は削除や更新操作で返却Bodyは空です。 |
| 400   | 不正なリクエスト。通常はリクエストボディやパラメータのエラー。 |
| 401   | 認証失敗。APIキーが期限切れか存在しません。 |
| 403   | 禁止。オブジェクトが使用中、または依存関係制約があります。 |
| 404   | 見つかりません。Bodyの`message`フィールドで理由を確認可能。 |
| 409   | 競合。オブジェクトが既に存在するか上限を超えています。 |
| 500   | サーバ内部エラー。Bodyやログで原因を確認してください。 |

## 認証

EMQXのREST APIは主に2つの認証方法をサポートしています：APIキーを用いたベーシック認証とベアラートークン認証です。

### APIキーを用いたベーシック認証

この方法では、APIキーとシークレットキーをユーザー名とパスワードとして使用しAPIリクエストを認証します。EMQXのREST APIは[HTTPベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを利用する前にAPIキーを作成する必要があります。詳細は[APIキー管理](#api-key-management)を参照してください。

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

APIキー認証の代替として、ベアラートークンを用いた安全かつプログラム的なEMQX REST APIアクセスが可能です。ベアラートークンは以下のログインAPIエンドポイントにリクエストを送信して取得します。

#### ベアラートークンの取得

以下のログインAPIエンドポイントにHTTP `POST`リクエストを送信します。

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

レスポンスにベアラートークンが含まれ、APIリクエスト認証に使用できます。

#### ベアラートークンを用いた認証

取得したベアラートークンはAPIリクエストの`Authorization`ヘッダーに以下の形式で含めます。

```bash
--header "Authorization: Bearer <your-token>"
```

## APIキー管理

本節ではAPIキーの作成・管理方法、およびロール、ネームスペース、スコープの設定方法を説明します。

### APIキーの作成

#### ダッシュボード

ダッシュボードの **System** -> **API Keys** から手動でAPIキーを作成できます。

1. 右上の **+ Create** ボタンをクリックして作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **Name**（必須）：APIキーの名前を入力。
   - **Expire At**：空欄の場合は期限なし。
   - **Is Enable**：デフォルトで有効。
   - **Role**：ロールを選択（任意）。詳細は[ロールと権限](#roles-and-permissions)参照。
   - **Namespace**：デフォルトはオフ。グローバル管理者の場合はオフのままでグローバルAPIキー作成。オンにしてネームスペースを選択するとそのネームスペース内のキーを作成。ネームスペース管理者は自身のネームスペース内のみ作成可能。
   - **Permission Mode**：管理者またはビューアキーの場合、スコープの割り当て方法を選択。パブリッシャーキーには表示されず、ロールのデフォルト`publish`スコープが適用されます。スコープの動作と制限は[APIスコープ](#api-scopes)参照。
     - **Role Default Scopes**：選択したロールのデフォルトを使用。ロールデフォルトの変更は自動反映。
     - **System-level Permissions**：`system`スコープのみ付与。
     - **Custom Restricted Permissions**：アクセス可能なAPI領域を制限するスコープを1つ以上選択。空欄の場合はスコープ保護されたAPIにアクセス不可。
   - **Scopes**：**Custom Restricted Permissions**選択時に表示。付与するスコープを選択。
   - **Note**：任意で説明を入力。
3. **Confirm**をクリック。APIキーとシークレットキーが**Created Successfully**ダイアログに表示されます。

   ::: warning 重要

   APIキーとシークレットキーは即座に保存してください。シークレットキーは再表示されません。

   :::

4. **Close**でダイアログを閉じます。

**Permission Mode**はダッシュボードのみで利用可能です。REST API利用時は`scopes`フィールドを直接設定します。詳細は[scopesのデフォルト動作](#default-behavior-of-scopes)を参照してください。

キー名をクリックすると詳細を確認でき、**Edit**で有効期限、状態、ロール、権限モード、スコープ、メモを変更可能です。**Delete**でキーを削除できます。

#### REST API

REST API経由でAPIキーを作成・更新する場合は、ダッシュボードユーザーのベアラートークンを使用してください。APIキー管理エンドポイントはAPIキー認証を受け付けません。

EMQX 6.0.4以降、`POST /api/v5/api_key`および`PUT /api/v5/api_key/:name`のリクエストボディにトップレベルの`namespace`フィールドを指定可能です。例として、`team-a`ネームスペースに管理者APIキーを作成するリクエスト：

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

`scopes`に`"unset"`を指定すると、後方互換性のためスコープ許可リストが明示的に解除されます。ロール、ネームスペース、APIキー固有のパス制限は引き続き適用されます。`scopes`を省略するとロールデフォルトのスコープが適用されます。

ネームスペースは以下のいずれかの形式で指定可能です：

- ロールを`administrator`などの素の文字列で指定し、`namespace`フィールドでネームスペースを指定。
- ロールに`ns:<namespace>::<role>`形式でネームスペースを含める（例：`ns:team-a::administrator`）。

両形式はサポートされており、両方が含まれる場合はネームスペースが一致する必要があります。不一致や空文字の場合はHTTP 400を返します。APIキー作成後はネームスペースの変更はできません。

#### ブートストラップファイル

ブートストラップファイル方式でもAPIキーを作成可能です。以下の設定でファイルの場所を指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイルに複数のAPIキーを以下の形式で改行区切りで記述します。

```
{API Key}:{Secret Key}:{?Role}:{?Scopes}
```

- **API Key**：任意の文字列でキー識別子。
- **Secret Key**：ランダム文字列をシークレットキーとして使用。
- **Role（省略可）**：キーの[ロール](#roles-and-permissions)。ネームスペース付きキーは`ns:<namespace>::<role>`形式（例：`ns:team-a::administrator`）。
- **Scopes（省略可）**：キーがアクセス可能な[APIスコープ](#api-scopes)をカンマ区切りで指定。省略時はロールのデフォルトが適用されます。検証動作は[ブートストラップスコープの検証](#validate-bootstrap-scopes)参照。

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

ブートストラップエントリが以下のスコープルールに違反した場合、EMQXは該当スコープを削除し警告をログに記録してキーの作成・更新を続行します。

- **ログイン専用スコープ**：`user_management`、`mfa_management`、`sso_management`、`api_key_management`はAPIキーに対して無効。これらは削除され、残りのスコープでキーが作成・更新されます。
- **管理者相当スコープ**：APIキーに割り当て可能なスコープのうち、`system`のみが管理者相当の権限を付与します。EMQX 6.0.4以降、管理者相当スコープと管理者相当でないスコープが混在する場合は、管理者相当スコープをすべて削除し残りのスコープを保持します。
- **ネームスペース付きスコープ**：EMQX 6.0.4以降、ネームスペース付きエントリがネームスペースロールで保持できないスコープを明示的に指定した場合、許可されないスコープを削除し残りを保持します。残るスコープがない場合はスコープ保護されたビジネスAPIにアクセスできません。許可されるスコープは[ネームスペース付き呼び出し元の制限](#restrictions-for-namespaced-callers)を参照。

##### ブートストラップAPIキーの再読み込み

この方法で作成されたAPIキーは無期限に有効です。

EMQX起動時にファイルの内容をAPIキーリストに追加します。既存のAPIキーがある場合はシークレットキー、ロール、スコープが更新されます。

### ネームスペース管理者によるAPIキー管理

EMQX 6.0.4以降、ネームスペース管理者は自身のネームスペース内でAPIキーを管理可能です。認証にはベアラートークンが必要です。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| APIキー作成 | 管理者のネームスペース内のみ作成可能。ネームスペース省略、グローバル指定、他ネームスペース指定はHTTP 403。 |
| APIキー一覧取得 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーはレスポンスから除外。 |
| APIキーの読み取り・更新・削除 | 管理者のネームスペース内のキーのみ操作可能。他ネームスペースのキーはHTTP 404で存在を秘匿。 |
| APIキーのネームスペース変更 | 他ネームスペースへの移動不可。更新はHTTP 400。 |

グローバル管理者は引き続き全ネームスペースのAPIキーを管理可能です。

## APIキーの権限

### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つの事前定義ロールのいずれかを割り当て可能です。

- **Administrator**：すべてのリソースにアクセス可能。指定がなければデフォルト。ロール識別子は`administrator`。
- **Viewer**：リソースとデータの閲覧のみ可能。REST APIのすべてのGETリクエストに対応。ロール識別子は`viewer`。
- **Publisher**：MQTTメッセージのパブリッシュ専用。メッセージパブリッシュ関連APIのみアクセス可能。ロール識別子は`publisher`。

::: tip 注意
`publisher`キーは`publish`スコープのみ受け入れます。スコープ割り当て時に`publish`以外のスコープはHTTP 400を返します。キーのロールを`publisher`に変更する場合は、同時に`"scopes": ["publish"]`または空リストをリクエストに含めてください。そうしないと既存スコープに`publish`以外が含まれている場合リクエストは拒否されます。
:::

### APIスコープ

スコープはキーごとの権限次元であり、キーがアクセス可能なREST APIのビジネス領域を宣言します。スコープと[ロールと権限](#roles-and-permissions)は独立しており、両方が適用されることで2層のアクセス制御を形成します。

| 次元 | 目的 | 粒度 |
| --------- | ------- | ----------- |
| **ロール** | HTTPメソッドの制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | APIドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方を通過した場合にのみ許可されます。

マイクロサービスや統合シナリオでは、外部システムがEMQX管理機能の一部だけにアクセスすることが多いです。例えば監視プラットフォームは`monitoring`スコープのみ、ルールパブリッシュサービスは`data_integration`のみ、クラスター運用ツールは`cluster_operations`のみ必要です。スコープにより最小権限の原則に基づきキーを割り当てることで、キー漏洩時の影響範囲を最小化できます。

::: tip
スコープ名はEMQXのアップグレード間でも変更されない安定した識別子です。OpenAPIタグ名が変更されても、同じスコープを持つキーは引き続き動作します。
:::

#### 組み込みAPIキー用スコープ

EMQX 6.2.4以降、APIキー用に11個のスコープを提供しています。

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
| `plugin_api` | プラグイン拡張API | `/plugin_api/{plugin}/...` |

`plugin_api`スコープはプラグインAPIゲートウェイを通じて公開されたエンドポイントへのアクセスを許可します。プラグインのインストール、起動、停止、設定エンドポイント（`/plugins*`）は`system`スコープに含まれます。ゲートウェイは`system`スコープも受け入れるため、`system`スコープを持つAPIキーは引き続きアクセス可能です。

`plugin_api`スコープはゲートウェイへのアクセスを制御し、プラグインが実装する操作自体を制御するものではありません。割り当て前にプラグインが公開する各エンドポイントのセキュリティ影響を確認してください。

::: tip 注意

EMQX 6.0.4以降、`audit`スコープはネームスペース付き呼び出し元に監査ログアクセスを付与しません。`GET /api/v5/audit`はグローバル管理者およびグローバルビューアのみが呼び出せます。詳細は[監査ログアクセス](./dashboard/audit-log.md#audit-log-access)を参照してください。

:::

::: warning 管理者相当スコープと制限スコープの混在禁止

EMQXは`system`、`user_management`、`api_key_management`、`sso_management`を管理者相当スコープ（検証メッセージでは`privilege scopes`）と分類しています。これらと制限スコープを組み合わせてもアカウントの実効権限は減りません。4つのうちAPIキーに割り当て可能なのは`system`のみで、他3つは[ログイン専用スコープ](#login-only-scopes)に分類されます。

そのためEMQX 6.0.4以降、APIキー作成・更新時の明示的スコープリストは`system`のみか、`system`を含まないスコープ群のいずれかでなければなりません。混在するとHTTP 400を返し変更は適用されません。

既存の混在スコープリストは動作を継続し、`system`は有効なままです。次回の明示的スコープ更新は`system`のみか`system`を含まないリストで行う必要があります。ダッシュボードで編集時は保存前に権限モード選択を促されます。

:::

#### ログイン専用スコープ

上記11個のAPIキー用スコープに加え、ダッシュボードログインユーザーには4つのログイン専用スコープがあり、ブラウザセッションにのみ適用されAPIキーには割り当てられません。詳細は[ログインユーザースコープ](./dashboard/system.md#login-user-scopes)を参照してください。

| スコープ | 必要ロール | 用途 |
| --- | --- | --- |
| `user_management` | Administrator | ダッシュボードユーザー管理。 |
| `sso_management` | Administrator | SSOバックエンドおよびSSOユーザー管理。 |
| `api_key_management` | Administrator | APIキー管理。 |
| `mfa_management` | グローバル管理者またはグローバルビューア | 自身のMFA管理。管理者は他ユーザーのMFAも管理可能。 |

#### `scopes`のデフォルト動作

EMQX 6.0.4以降、APIキーの`scopes`フィールドは以下のルールに従います。

| `scopes`の値 | 意味 |
| --- | --- |
| **作成リクエストで省略** | 選択されたロールのデフォルトを使用。 |
| **更新リクエストで省略** | キーの現在のスコープ設定を保持。 |
| **未設定セントネル `"unset"`** | 明示的なスコープ設定を解除。後方互換のためスコープ許可リストは適用しない。ロール、ネームスペース、APIキー固有パス制限は適用。 |
| **空リスト `[]`** | すべてのビジネスエンドポイントを拒否。キーをソフトに無効化可能。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定されたスコープのAPIのみ許可。 |

ロールデフォルトと同じスコープセットを明示的リストで指定した場合は`"unset"`に正規化され同様に扱われます。順序は無関係です。

ブートストラップファイルのエントリでスコープ区切りが省略された場合は、処理時に指定ロールのデフォルトが適用されます。

スコープはキーがアクセス可能なAPI領域を決定し、ロールやネームスペース制限を上書きしません。リクエストはロール、スコープ、ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能スコープの一覧取得

EMQXは利用可能なスコープカタログを取得するための2つのエンドポイントを公開しています。

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記11個）を返します。APIキー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（ログイン専用4スコープ含む）を返します。ベアラートークン認証が必要です。

スコープ選択UIの構築や自動化スクリプトの検証に利用してください。

```bash
# APIキー用スコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの方法で設定可能です。

- **ダッシュボード**：**System** -> **API Keys**でキー作成・編集時に**Permission Mode**を選択。**Custom Restricted Permissions**選択時に個別スコープを選択。
- **REST API**：作成・更新リクエストボディに`"scopes": ["monitoring", "cluster_operations"]`を含める。
- **ブートストラップファイル**：各行の4番目の区切りとしてカンマ区切りのスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ネームスペース付き呼び出し元の制限

ネームスペース付き呼び出し元（ロールが特定ネームスペースに制限されたユーザーまたはAPIキー）は、スコープチェックに加えてエンドポイントレベルの追加制限を受けます。スコープ付与はこれらの制限を上書きしません。

### ネームスペース付きAPIキーのスコープ制限

EMQX 6.0.4以降、作成リクエストで`scopes`を省略した場合、ネームスペース付きの管理者またはビューアロールのAPIキーは以下の7つのスコープを付与されます。

- `connections`
- `monitoring`
- `data_integration`
- `access_control`
- `system`
- `cluster_operations`
- `license`

EMQX 6.2.4以降は`plugin_api`も追加され8つになります。これらのデフォルトには`publish`、`gateways`、`audit`は含まれません。

ネームスペース付き管理者またはビューアロールのAPIキー作成時や既存キーの明示的スコープ変更時は、EMQX 6.0.4以降、指定可能なスコープは上記7つ（6.2.4以降は8つ）に限定されます。`publish`、`gateways`、`audit`やネームスペースロールが持てないスコープを指定するとHTTP 400を返し、禁止スコープを通知して変更は適用されません。`system`と制限スコープの混在禁止も明示的スコープリストに適用されます。

### 禁止スコープを含む既存キー

禁止スコープを含む既存キーは自動的に変更されません。読み取り-変更-書き込みクライアントの互換性のため、更新時にスコープリストを変更せずロールとネームスペースも同じ場合は許容されます。

例外は、`publish`のみを含むネームスペース付きキーで、APIアクセスできるAPIがないため、変更なしの更新でもHTTP 400を返します。この場合はキーを削除してネームスペースなしで再作成してください。ロールやスコープの実際の変更は再検証され、許可リストに準拠する必要があります。

ブートストラップファイル処理時は禁止スコープを削除し警告ログを出力して残りのスコープを保持します。詳細は[ブートストラップスコープの検証](#validate-bootstrap-scopes)参照。

### メッセージパブリッシュの制限

ネームスペース付きAPIキーはメッセージパブリッシュAPI（`POST /api/v5/publish`など）を呼び出せません。これは以前のスコープリストに`publish`が含まれていても適用される制限であり、スコープ割り当てはネームスペースレベルの制限を上書きしません。

### メッセージコンテンツの制限

ネームスペース付き呼び出し元が`connections`または`monitoring`スコープを持っていても、クラスター全体の生のMQTTメッセージコンテンツ（保持メッセージや遅延メッセージストア）を読み書きするエンドポイントにはアクセスできません。以下のメッセージ関連エンドポイントは`403 Forbidden`を返します。

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

ファイル転送ストアはグローバルでネームスペース非対応です。ネームスペース付き呼び出し元は以下のファイル転送コンテンツエンドポイントにアクセスできません。スコープ付与はこの制限を上書きしません。

- `GET /file_transfer/files`
- `GET /file_transfer/files/:clientid/:fileid`
- `GET /file_transfer/file`

グローバル呼び出し元はロールとスコープに応じてこれらのエンドポイントにアクセス可能です。`/file_transfer`の設定エンドポイントは影響を受けません。

### トレースの制限

トレース操作では、`GET /trace`は呼び出し元のネームスペース内のトレースのみ列挙します。以下のトレース単位操作はトレースが別ネームスペースの場合`404 Not Found`を返します。

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この動作は他ネームスペースのトレース情報漏洩を防止します。まとめて削除する`DELETE /trace`はネームスペース付き呼び出し元に`403 Forbidden`を返し、グローバル管理者のみが全トレースをクリア可能です。

ダッシュボードログイン、SSOコールバック、APIキー自己管理エンドポイント（例：`/api_key`）は、キーの`scopes`設定に関わらずAPIキー認証を受け付けません。これはスコープモデルとは無関係のダッシュボードの組み込みセキュリティ境界です。

## ページネーション

大量データを扱うAPIの一部にはページネーション機能があります。データ特性に応じて2種類のページネーション方式があります。

### ページ番号ページネーション

ページネーション対応の多くのAPIでは、`page`（ページ番号）と`limit`（ページサイズ）パラメータで制御可能です。最大ページサイズは`10000`です。`limit`未指定時はデフォルト`100`です。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総データ件数を予測できないため、`meta.hasnext`で次ページの有無を示します。

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

データ変動が激しくページ番号ページネーションが非効率なAPIではカーソルページネーションを採用しています。

`position`または`cursor`（開始位置）パラメータで読み込み開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置から読み込む件数を指定します。最大ページサイズは`10000`、`limit`未指定時はデフォルト`100`です。

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

この方式はデータ変動が激しいシナリオで連続性と効率性を確保します。

## エラーコード

HTTPレスポンスステータスコードに加え、EMQXは特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時はBodyにJSON形式でエラーコードが返されます。

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| エラーコード                                    | 説明                                                  |
| ---------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています。                  |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはAPIキー＆シークレットが間違っています。 |
| BAD_REQUEST                                    | リクエストパラメータが不正です。                                 |
| NOT_MATCH                                      | 条件が一致しません。                                       |
| ALREADY_EXISTS                                 | リソースが既に存在します。                                      |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です。                                       |
| BAD_LISTENER_ID                                | 無効なリスナーIDです。                                       |
| BAD_NODE_NAME                                  | 無効なノード名です。                                         |
| BAD_RPC                                        | RPC失敗。クラスター状態と対象ノードの状態を確認してください。       |
| BAD_TOPIC                                      | トピックの構文エラー。MQTTプロトコル標準に準拠する必要があります。   |
| EXCEED_LIMIT                                   | 作成リソースが最大または最小制限を超えています。                     |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています。                 |
| CONFLICT                                       | リクエストリソースが競合しています。                              |
| NO_DEFAULT_VALUE                               | リクエストパラメータがデフォルト値を使用していません。                 |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています。                          |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージIDの解析エラー。                                     |
| INVALID_ID                                     | 無効なIDスキーマです。                                       |
| MESSAGE_ID_NOT_FOUND                           | メッセージIDが存在しません。                                  |
| NOT_FOUND                                      | リソースが見つかりません。                                    |
| CLIENTID_NOT_FOUND                             | クライアントIDが見つかりません。                               |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常はMQTTクライアントではありません）。 |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません。                                    |
| TOPIC_NOT_FOUND                                | トピックが見つかりません。                                    |
| USER_NOT_FOUND                                 | ユーザーが見つかりません。                                    |
| INTERNAL_ERROR                                 | サーバ内部エラーです。                                       |
| SERVICE_UNAVAILABLE                            | サービス利用不可です。                                       |
| SOURCE_ERROR                                   | ソースエラーです。                                          |
| UPDATE_FAILED                                  | 更新に失敗しました。                                        |
| REST_FAILED                                    | ソースまたは設定のリセットに失敗しました。                        |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません。                                  |
