# REST API

EMQXは、OpenAPI（Swagger）3.0仕様に準拠したHTTP管理APIを公開しています。

EMQXはREST APIを探索・操作するための複数の方法を提供しています。EMQX起動後、以下のAPI仕様エンドポイントが利用可能です：

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウン形式のAPIリファレンスページ。 |
| `/api-spec.md` | Markdown | Markdown形式のAPIリファレンス。AIエージェントや自動化ツール向け。 |
| `/api-spec.json` | JSON | JSON形式のOpenAPI 3.0仕様。スクリプトやプログラムツール向け。 |
| `/api-docs/index.html` | HTML | ブラウザ上でAPIコールを直接テストできるインタラクティブなSwagger UI。**非推奨**：v7で削除予定。 |

上記のすべてのエンドポイントは、ダッシュボード設定で`swagger_support`が`true`（デフォルト）に設定されている必要があります。`false`に設定すると、すべてのAPIドキュメントエンドポイントが無効になります。詳細は[ダッシュボード設定](configuration/dashboard.md)をご覧ください。

本セクションでは、EMQX REST APIの利用方法を紹介します。

## 基本パス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降のすべてのAPIパスは`/api/v5`で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは`Accept`ヘッダーに`application/json`を設定する必要があり、特に指定がなければレスポンスはJSON形式で返されます。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)標準に準拠しています。主なステータスコードは以下の通りです：

| コード | 説明 |
| ----- | ------------------------------------------------------------ |
| 200   | リクエスト成功。返却されるJSONデータに詳細が含まれます。 |
| 201   | 作成成功。新規オブジェクトがBodyに返されます。 |
| 204   | リクエスト成功。通常は削除や更新操作で返却Bodyは空です。 |
| 400   | 不正なリクエスト。通常はリクエストボディやパラメータのエラー。 |
| 401   | 認証エラー。APIキーが期限切れまたは存在しません。 |
| 403   | 禁止。オブジェクトが使用中、または依存関係の制約があります。 |
| 404   | 見つかりません。Bodyの`message`フィールドで理由を確認できます。 |
| 409   | コンフリクト。オブジェクトが既に存在するか、数の上限を超えています。 |
| 500   | サーバ内部エラー。Bodyやログで原因を確認してください。 |

## 認証

EMQXのREST APIは主に2つの認証方法をサポートしています：APIキーを使ったベーシック認証とベアラートークン認証です。

### APIキーを使ったベーシック認証

この方法では、APIキーとシークレットキーをユーザー名とパスワードとして使用し、APIリクエストを認証します。EMQXのREST APIは[HTTPベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを利用する前にAPIキーを作成する必要があります。詳細は[APIキー管理](#apiキー管理)をご覧ください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0以降ではダッシュボードのユーザー認証情報をREST API認証に使用できません。代わりにAPIキーを作成し、認証に使用してください。

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

#### ベアラートークンの取得

以下のログインAPIエンドポイントにHTTP `POST`リクエストを送信してください：

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

レスポンスにはベアラートークンが含まれ、APIリクエストの認証に使用できます。

#### ベアラートークンを使った認証

ベアラートークンを取得したら、APIリクエストの`Authorization`ヘッダーに以下のように含めてください：

```bash
--header "Authorization: Bearer <your-token>"
```

## APIキー管理

このセクションでは、APIキーの作成・管理方法と、ロール、ネームスペース、スコープの設定について説明します。

### APIキーの作成

#### ダッシュボード

ダッシュボードの **System** -> **API Keys** から手動でAPIキーを作成できます：

1. 右上の **+ Create** ボタンをクリックして作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **Name**（必須）：APIキーの名前を入力します。
   - **Expire At**：空欄の場合は期限なしとなります。
   - **Is Enable**：デフォルトで有効です。
   - **Role**：ロールを選択（任意）。詳細は[ロールと権限](#roles-and-permissions)を参照してください。
   - **Namespace**：デフォルトはオフ。グローバル管理者の場合はオフのままでグローバルAPIキーを作成します。オンにしてネームスペースを選択すると、そのネームスペース内のキーを作成します。ネームスペース管理者は自分のネームスペース内でのみキーを作成可能です。
   - **Permission Mode**：管理者またはビューアキーの場合、スコープの割り当て方法を選択します。パブリッシャーキーには表示されません（ロールのデフォルト`publish`スコープを使用）。スコープの動作と制限は[APIスコープ](#api-scopes)を参照してください。
     - **Role Default Scopes**：選択したロールのデフォルトを使用。ロールデフォルトの変更は自動的に反映されます。
     - **System-level Permissions**：`system`スコープのみ付与。
     - **Custom Restricted Permissions**：アクセス可能なAPI領域を制限するために1つ以上のスコープを選択。**Scopes**を空にするとスコープ保護APIにアクセスできません。
   - **Scopes**：**Custom Restricted Permissions**選択時に表示。付与するスコープを選択します。
   - **Note**：任意で説明を入力します。
3. **Confirm**をクリックすると、APIキーとシークレットキーが**Created Successfully**ダイアログに表示されます。

   ::: warning 重要

   APIキーとシークレットキーはすぐに保存してください。シークレットキーは再表示されません。

   :::

4. **Close**をクリックしてダイアログを閉じます。

**Permission Mode**はダッシュボードのみで利用可能です。REST API利用時は`scopes`フィールドを直接設定してください。詳細は[scopesのデフォルト動作](#default-behavior-of-scopes)を参照してください。

キー名をクリックすると詳細を表示できます。**Edit**ボタンで有効期限、状態、ロール、パーミッションモード、スコープ、メモを変更可能です。**Delete**ボタンでキーを削除できます。

#### REST API

REST API経由でAPIキーを作成・更新する場合は、ダッシュボードユーザーのベアラートークンを使用してください。APIキー管理エンドポイントはAPIキー認証を受け付けません。

EMQX 6.0.4以降、`POST /api/v5/api_key`および`PUT /api/v5/api_key/:name`のリクエストボディにトップレベルの`namespace`フィールドを指定可能です。例えば、`team-a`ネームスペースに管理者APIキーを作成するリクエストは以下の通りです：

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

`scopes`を`"unset"`に設定すると、後方互換性のためにスコープ許可リストを明示的に解除します。ロール、ネームスペース、APIキー固有のパス制限は引き続き適用されます。`scopes`を省略した場合はロールのデフォルトスコープが適用されます。

ネームスペースは以下のいずれかの方法で指定できます：

- `administrator`のようなロールと`namespace`フィールドを併用する方法
- `ns:<namespace>::<role>`形式でロール内にネームスペースをエンコードする方法（例：`ns:team-a::administrator`）

両方の形式がサポートされており、両方が含まれる場合はネームスペースが一致する必要があります。不一致や空のネームスペースはHTTP 400を返します。APIキー作成後はネームスペースの変更はできません。

#### ブートストラップファイル

ブートストラップファイル方式でもAPIキーを作成できます。設定ファイルに以下を追加し、ファイルの場所を指定します：

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイル内に複数のAPIキーを以下の形式で改行区切りで記述します：

```
{API Key}:{Secret Key}:{?Role}:{?Scopes}
```

- **API Key**：キー識別子として任意の文字列
- **Secret Key**：ランダムな文字列をシークレットキーとして使用
- **Role（任意）**：キーの[ロール](#roles-and-permissions)。ネームスペースキーの場合は`ns:<namespace>::<role>`形式（例：`ns:team-a::administrator`）
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

ブートストラップエントリが以下のスコープルールに違反すると、EMQXは該当スコープを削除し、警告ログを出力しつつキーの作成・更新を続行します：

- **ログイン専用スコープ**：`user_management`、`mfa_management`、`sso_management`、`api_key_management`はAPIキーに無効です。EMQXはこれらのスコープを削除し、残りのスコープでキーを作成・更新します。
- **管理者相当スコープ**：APIキーに割り当て可能なスコープのうち、`system`のみが管理者相当の権限を付与します。EMQX 6.0.4以降、管理者相当スコープと管理者相当でないスコープが混在する場合、管理者相当スコープをすべて削除し、残りのスコープを保持します。
- **ネームスペーススコープ**：EMQX 6.0.4以降、ネームスペース付きエントリがネームスペースロールが保持できないスコープを明示的に指定した場合、許可されないスコープを削除し、残りのスコープを保持します。スコープが残らない場合、スコープ保護された業務APIにアクセスできません。許可されるスコープは[ネームスペース呼び出し元の制限](#restrictions-for-namespaced-callers)を参照してください。

##### ブートストラップAPIキーのリロード

この方法で作成されたAPIキーは無期限で有効です。

EMQX起動時にファイルのデータをAPIキーリストに追加します。既存のAPIキーがあれば、シークレットキー、ロール、スコープが更新されます。

### ネームスペース管理者によるAPIキー管理

EMQX 6.0.4以降、ネームスペース付きダッシュボード管理者は自身のネームスペース内でAPIキーを管理できます。管理者はベアラートークンで認証する必要があります。

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

- **Administrator**：すべてのリソースにアクセス可能。指定がなければデフォルト。ロール識別子は`administrator`。
- **Viewer**：リソースやデータの閲覧のみ可能。REST APIのすべてのGETリクエストに対応。ロール識別子は`viewer`。
- **Publisher**：MQTTメッセージのパブリッシュ専用。メッセージパブリッシュ関連APIのみアクセス可能。ロール識別子は`publisher`。

::: tip 注意
`publisher`キーは`publish`スコープのみ許容します。スコープ割り当て時に`publish`以外のスコープはHTTP 400を返します。キーのロールを`publisher`に変更する場合は、同時に`"scopes": ["publish"]`または空リストをリクエストに含めてください。そうしないと既存スコープに`publish`以外がある場合リクエストは拒否されます。
:::

### APIスコープ

スコープはキーごとの権限次元であり、キーがアクセス可能なREST APIの業務領域を宣言します。スコープと[ロールと権限](#roles-and-permissions)は独立しており、両方が適用されることで2層のアクセス制御を形成します：

| 次元 | 目的 | 粒度 |
| --------- | ------- | ----------- |
| **ロール** | HTTP動詞の制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | APIドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方を通過した場合のみ許可されます。

マイクロサービスや統合シナリオでは、外部システムは通常EMQX管理領域の一部のみアクセスします。監視プラットフォームは`monitoring`スコープのみ、ルールパブリッシュサービスは`data_integration`のみ、クラスター運用ツールは`cluster_operations`のみ必要です。スコープにより最小権限の原則でキーを割り当て、キー漏洩時の影響範囲を最小化します。

::: tip
スコープ名はEMQXのアップグレードを通じて安定した識別子です。OpenAPIタグ名が変更されても、同じスコープを持つキーは引き続き動作します。
:::

#### 組み込みAPIキー用スコープ

EMQXはAPIキー用に以下10個のスコープを提供しています：

| スコープ | 名称 | 主なAPI領域 |
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

::: tip 注意

EMQX 6.0.4以降、`audit`スコープはネームスペース呼び出し元に監査ログアクセスを付与しません。`GET /api/v5/audit`はグローバル管理者およびグローバルビューアのみ呼び出せます。詳細は[監査ログアクセス](./dashboard/audit-log.md#audit-log-access)を参照してください。

:::

::: warning 管理者相当スコープと制限スコープを混在させないでください

EMQXは`system`、`user_management`、`api_key_management`、`sso_management`を管理者相当スコープ（検証メッセージでは`privilege scopes`）と分類しています。これらを制限スコープと混在させてもアカウントの実効権限は減りません。4つのうちAPIキーに割り当て可能なのは`system`のみで、他3つは[ログイン専用スコープ](#login-only-scopes)に分類されます。

そのため、EMQX 6.0.4以降、APIキー作成・更新時の明示的スコープリストは`system`のみ、または`system`を含まないスコープのいずれかでなければなりません。混在リストはHTTP 400を返し、変更は適用されません。

既存の混在スコープリストは引き続き有効で`system`は有効なままです。次回の明示的スコープ更新時は`system`のみか`system`を含まないリストを使用してください。ダッシュボードで編集時は保存前にパーミッションモードの選択を促されます。

:::

#### ログイン専用スコープ

APIキー用スコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、APIキーに割り当てられません。ログインユーザーへの割り当てと適用方法は[ログインユーザースコープ](dashboard/system.md#login-user-scopes)を参照してください。

| スコープ | 必要ロール | 用途 |
| --- | --- | --- |
| `user_management` | Administrator | ダッシュボードユーザー管理。 |
| `sso_management` | Administrator | SSOバックエンドとSSOユーザーレコード管理。 |
| `api_key_management` | Administrator | APIキー管理。 |
| `mfa_management` | グローバル管理者またはグローバルビューア | 自身のMFA管理。管理者は他ユーザーのMFAも管理可能。 |

#### `scopes`のデフォルト動作

EMQX 6.0.4以降、APIキーの`scopes`フィールドは以下のルールに従います：

| `scopes`の値 | 意味 |
| --- | --- |
| **作成リクエストで未指定** | 選択したロールのデフォルトを使用。 |
| **更新リクエストで未指定** | キーの現在のスコープ設定を保持。 |
| **解除用セントネル `"unset"`** | 明示的なスコープ設定を解除。後方互換のためスコープ許可リストは適用されません。ロール、ネームスペース、APIキー固有のパス制限は適用。 |
| **空リスト `[]`** | すべての業務エンドポイントを拒否。キーを無効化する柔軟な方法。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定したスコープのみ許可。 |

ロールデフォルトと同じスコープセットの明示的リストは`"unset"`に正規化され、同様の動作をします。比較は順不同です。

ブートストラップファイルのエントリでスコープセグメントを省略すると、ファイル処理時に指定ロールのデフォルトが適用されます。

スコープはキーがアクセス可能なAPI領域を決定します。ロールやネームスペース制限を上書きしません。リクエストはロール・スコープ・ネームスペースのすべてのチェックを通過した場合のみ許可されます。

#### 利用可能なスコープ一覧の取得

EMQXは以下2つのエンドポイントで利用可能なスコープカタログを取得できます：

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記10個の業務ドメインスコープ）を返します。APIキー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（4つのログイン専用スコープ含む）を返します。ベアラートークン認証が必要です。

スコープ選択UIの構築や自動化スクリプトの検証に利用してください：

```bash
# APIキー用スコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下いずれかの入口から設定可能です：

- **ダッシュボード**：**System** -> **API Keys**でキー作成・編集時に**Permission Mode**を選択。**Custom Restricted Permissions**選択時に個別スコープを選択。
- **REST API**：作成・更新リクエストボディに`"scopes": ["monitoring", "cluster_operations"]`を含める。
- **ブートストラップファイル**：各行の4番目のセグメントとしてカンマ区切りスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ネームスペース呼び出し元の制限

ネームスペース付き呼び出し元（ロールが特定ネームスペースに制限されたユーザーやAPIキー）は、スコープチェックに加えて追加のエンドポイントレベル制限を受けます。スコープ付与はこれらの制限を上書きしません。

### ネームスペースAPIキーのスコープ制限

EMQX 6.0.4以降、作成リクエストで`scopes`を省略した場合、ネームスペース付きの管理者またはビューアロールAPIキーは以下7つのスコープで作成されます：

`connections`、`monitoring`、`data_integration`、`access_control`、`system`、`cluster_operations`、`license`

これらのデフォルトには`publish`、`gateways`、`audit`は含まれません。

ネームスペース付き管理者またはビューアロールAPIキー作成時、または既存キーの明示的スコープリスト変更時、リクエストにこれら7つ以外のスコープ（`publish`、`gateways`、`audit`など）が含まれるとEMQXはHTTP 400を返し、許可されないスコープを特定して変更を適用しません。`system`と制限スコープの混在禁止も明示的スコープリストに適用されます。

### 許可されないスコープを持つ既存キー

許可されないスコープを保持する既存キーは自動的に変更されません。読み取り-変更-書き込みクライアントの互換性のため、更新時に変更がなければ保存を受け入れ、同じロールとネームスペースを保持します。

例外は、許可されないスコープが`publish`のみのネームスペースキーで、変更なし更新でもHTTP 400を返します。これはAPIアクセスが一切できないためです。キーを削除し、ネームスペースなしで再作成してください。実際のロールやスコープ変更は再検証され、許可リストに準拠する必要があります。

ネームスペースAPIキーの更新やローテーションは、以前の権限がキーのローテーションまで有効であるため、ネームスペースエンドポイント制限の範囲内で可能です。ブートストラップエントリ再処理時は許可されないスコープを削除し、警告ログを出力し、残りのスコープを保持します。詳細は[ブートストラップスコープの検証](#validate-bootstrap-scopes)を参照してください。

### メッセージパブリッシュの制限

ネームスペースAPIキーは`POST /api/v5/publish`を含むメッセージパブリッシュAPIを呼び出せません。この制限は以前のスコープリストに`publish`が含まれていても適用され、スコープ割り当てはネームスペースレベルの制限を上書きしません。

### メッセージコンテンツの制限

ネームスペース呼び出し元が`connections`または`monitoring`スコープを持っていても、クラスター全体のMQTTメッセージコンテンツ（保持メッセージや遅延メッセージストアを含む）を読み書きするエンドポイントにはアクセスできません。以下のメッセージ関連エンドポイントは`403 Forbidden`を返します：

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

グローバル呼び出し元はロールとスコープに応じてこれらのエンドポイントにアクセス可能です。`/file_transfer`設定エンドポイントは影響を受けません。

### トレースの制限

トレース操作において、`GET /trace`は呼び出し元のネームスペース内のトレースのみ一覧表示します。以下のトレース単体操作はトレースが異なるネームスペースに属する場合`404 Not Found`を返します：

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この挙動は他ネームスペースのトレース情報漏洩を防ぎます。まとめて削除する`DELETE /trace`はネームスペース呼び出し元に対して`403 Forbidden`を返し、全トレース削除はグローバル管理者のみ可能です。

ダッシュボードログイン、SSOコールバック、APIキー自己管理エンドポイント（例：`/api_key`）は、キーの`scopes`設定に関わらずAPIキー認証を受け付けません。これはスコープモデルとは無関係のダッシュボードのセキュリティ境界です。

## ページネーション

大量データを扱う一部APIにはページネーション機能があります。データ特性に応じて2種類のページネーション方式があります。

### ページ番号ページネーション

ページネーション対応の多くのAPIでは、`page`（ページ番号）と`limit`（ページサイズ）パラメータでページ制御します。最大ページサイズは`10000`です。`limit`未指定時はデフォルト`100`です。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの`meta`フィールドにページ情報が含まれます。EMQXは検索条件付きリクエストの総件数を予測できないため、`meta.hasnext`で次ページの有無を示します：

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

データ変動が激しくページ番号ページネーションが非効率な一部APIではカーソルページネーションを使用します。

`position`または`cursor`（開始位置）パラメータで読み込み開始位置を指定し、`limit`（ページサイズ）で開始位置からの件数を指定します。最大ページサイズは`10000`です。`limit`未指定時はデフォルト`100`です。

例：

```bash
GET /clients/{clientid}/mqueue_messages?position=1716187698257189921_0&limit=100
```

レスポンスの`meta`フィールドにページ情報が含まれ、`meta.position`または`meta.cursor`が次ページの開始位置を示します：

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

この方式はデータ変動が激しいシナリオで継続的かつ効率的なデータ取得を実現します。

## エラーコード

HTTPレスポンスステータスコードに加え、EMQXは特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時はBodyにJSON形式でエラーコードが返されます：

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
| BAD_RPC                                        | RPC失敗。クラスター状態と対象ノード状態を確認してください |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります |
| EXCEED_LIMIT                                   | 作成リソースが最大または最小制限を超えています |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています   |
| CONFLICT                                       | リクエストリソースが競合しています                                |
| NO_DEFAULT_VALUE                               | リクエストパラメータがデフォルト値を使用していません                 |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています                          |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージID解析エラー                                     |
| INVALID_ID                                     | 不正なIDスキーマ                                                |
| MESSAGE_ID_NOT_FOUND                           | メッセージIDが存在しません                                    |
| NOT_FOUND                                      | リソースが見つかりません、または存在しません                         |
| CLIENTID_NOT_FOUND                             | クライアントIDが見つかりません、または存在しません                        |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常はMQTTクライアントではありません） |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません                                           |
| TOPIC_NOT_FOUND                                | トピックが見つかりません                                              |
| USER_NOT_FOUND                                 | ユーザーが見つかりません                                               |
| INTERNAL_ERROR                                 | サーバ内部エラー                                           |
| SERVICE_UNAVAILABLE                            | サービス利用不可                                          |
| SOURCE_ERROR                                   | ソースエラー                                                 |
| UPDATE_FAILED                                  | 更新失敗                                                 |
| REST_FAILED                                    | ソースまたは設定のリセット失敗                          |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません                                        |
