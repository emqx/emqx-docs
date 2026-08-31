# REST API

EMQXはOpenAPI 3.0仕様に準拠したHTTP管理APIを公開しています。

EMQXはREST APIを探索・操作するための複数の方法を提供しています。EMQX起動後、以下のAPI仕様エンドポイントが利用可能です。

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウン形式のAPIリファレンスページ。 |
| `/api-spec.md` | Markdown | Markdown形式のAPIリファレンス。AIエージェントや自動化ツール向け。 |
| `/api-spec.json` | JSON | JSON形式のOpenAPI 3.0仕様。スクリプトやプログラム的ツール向け。 |
| `/api-spec/:tag[/:name]` | JSON | APIタグに絞ったOpenAPI 3.0仕様。リクエストまたはレスポンスのスキーマ名でさらに絞り込み可能。 |
| `/api-docs/swagger.json` | JSON | 外部Swagger UIや互換ツール用の完全なOpenAPI 3.0仕様。 |

上記のすべてのエンドポイントは、ダッシュボード設定で`swagger_support`が`true`（デフォルト）に設定されている必要があります。`false`にするとすべてのAPIドキュメントエンドポイントが無効になります。詳細は[ダッシュボード設定](../configuration/dashboard.md)を参照してください。

EMQX 6.3.0以降、EMQXはSwagger UIをバンドルしなくなりました。後方互換のため、`/api-docs`または`/api-docs/index.html`へのリクエストはHTTP 308で`/api-spec.html`へリダイレクトされます。`/api-docs/index.html`および`/api-docs/swagger.json`以外の`/api-docs/*`サブパス（以前はSwagger UIのアセットを提供していた）はHTTP 404を返します。

本セクションではEMQX REST APIの利用方法を紹介します。

::: tip
EMQX 6.3.0以降、[feature gates](../deploy/feature-gates.md)により起動時にオプション機能を無効化できます。無効化された機能が提供するREST APIパスはアクセス可能なAPIエンドポイントとして読み込まれません。`dashboard`機能が有効な場合、`GET /api/v5/features`で解決された機能セットを確認できます。
:::

## API仕様エンドポイントへのアクセス

EMQX 6.3.0以降、上記エンドポイントからAPI仕様を取得するには認証が必要です。

プログラム的なリクエストは、APIキーとシークレットキーを用いたBasic認証か、ベアラートークンのいずれかで認証してください。詳細は[認証](#authentication)を参照してください。

API仕様へのアクセスは読み取り専用で、APIキーのロールやスコープには依存しません。

`/api-spec.md`、`/api-spec.json`、`/api-spec/:tag[/:name]`、`/api-docs/swagger.json`では、認証情報が欠落または無効な場合HTTP `401`を返します。`WWW-Authenticate`レスポンスヘッダーでBasic認証とBearer認証を通知します。レスポンスボディは要求されたフォーマットで、サポートされる認証方式を説明し、`POST /api/v5/login`（ベアラートークン取得用）と`GET /api/v5/status`（ブローカー状態確認用）の2つの公開エンドポイントをリストします。最小限のレスポンスであり、要求されたAPI仕様内容は含みません。

ブラウザアクセスの場合、EMQXは有効な`emqx_auth`セッションクッキーを受け入れます。認証されていない`/api-spec.html`へのリクエストはHTTP `401`を返し、完全なAPI Spec Explorerの代わりにサインインページを表示します。このレスポンスはブラウザのネイティブBasic認証ダイアログを開かせないようBearer認証のみを通知します。ダッシュボードのユーザー名とパスワードでサインインすると、EMQXは`emqx_auth`セッションクッキーを作成し、完全なエクスプローラーを読み込みます。サインアウトするとセッションクッキーはクリアされます。

`/api-docs`および`/api-docs/index.html`へのリクエストは認証不要です。これらは`/api-spec.html`へリダイレクトするだけのためです。リダイレクト後の完全なエクスプローラーアクセスには認証が必要です。

## ベーシックパス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降のすべてのAPIパスは`/api/v5`で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストは`Accept`ヘッダーに`application/json`を設定する必要があります。そうするとレスポンスはJSON形式で返されます（特に指定がない限り）。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)標準に従います。主なステータスコードは以下の通りです。

| コード | 説明 |
| ----- | ------------------------------------------------------------ |
| 200   | リクエスト成功。返却されるJSONデータに詳細が含まれます。 |
| 201   | 作成成功。新規オブジェクトがボディに返されます。 |
| 204   | リクエスト成功。通常は削除や更新操作で返却ボディは空です。 |
| 400   | 不正なリクエスト。リクエストボディやパラメータのエラー。 |
| 401   | 認証エラー。認証情報が欠落、無効、または期限切れ。 |
| 403   | 禁止。オブジェクトが使用中、または依存関係制約あり。 |
| 404   | 未検出。ボディの`message`フィールドで理由を確認可能。 |
| 409   | 競合。オブジェクトが既に存在するか、数の上限超過。 |
| 500   | サーバ内部エラー。ボディやログで原因を確認。 |

## 認証

EMQXのREST APIは主に2つの認証方法をサポートしています：APIキーを用いたBasic認証とベアラートークン認証です。

### APIキーを用いたBasic認証

この方法ではAPIキーとシークレットキーをユーザー名とパスワードとして使用し、APIリクエストを認証します。EMQXのREST APIは[HTTP Basic認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しています。EMQX REST APIを使用する前にAPIキーを作成する必要があります。詳細は[APIキー管理](#api-key-management)を参照してください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0以降、ダッシュボードのユーザー資格情報でREST APIリクエストを認証することはできません。代わりにAPIキーを作成して認証に使用してください。

:::

#### APIキーでの認証例

APIキーとシークレットキーを取得したら、Basic認証のユーザー名にAPIキー、パスワードにシークレットキーを使用します。

言語別の例：

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

以下のログインAPIエンドポイントにHTTP `POST`リクエストを送信してください。

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

レスポンスにベアラートークンが含まれ、APIリクエストの認証に使用できます。

#### ベアラートークンを用いた認証

ベアラートークンを取得したら、APIリクエストの`Authorization`ヘッダーに以下のように含めてください。

```bash
--header "Authorization: Bearer <your-token>"
```

## APIキー管理

本節ではAPIキーの作成・管理方法と、ロール・ネームスペース・スコープの設定について説明します。

### APIキーの作成

#### ダッシュボード

ダッシュボードの**システム** -> **APIキー**から手動でAPIキーを作成できます。

1. 右上の**+ 作成**ボタンをクリックし、作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **名前**（必須）：APIキーの名前を入力します。
   - **有効期限**：空欄の場合は期限なしとなります。
   - **有効化**：デフォルトで有効です。
   - **ロール**：ロールを選択します（任意）。[ロールと権限](#roles-and-permissions)を参照してください。
   - **ネームスペース**：デフォルトはオフ。グローバル管理者の場合オフのままでグローバルAPIキーが作成されます。オンにしてネームスペースを選択すると、そのネームスペース内のキーが作成されます。ネームスペース管理者は自身のネームスペース内でのみキーを作成可能です。
   - **権限モード**：管理者またはビューアのキーの場合、スコープの割り当て方法を選択します。パブリッシャーキーでは表示されず、ロールデフォルトの`publish`スコープが使用されます。スコープの動作と制限は[APIスコープ](#api-scopes)を参照してください。
     - **ロールデフォルトスコープ**：選択したロールのデフォルトを使用。ロールデフォルトの変更は自動反映されます。
     - **システムレベル権限**：`system`スコープのみ付与。
     - **カスタム制限付き権限**：1つ以上のスコープを選択し、アクセス可能なAPI領域を制限します。**スコープ**を空欄にするとスコープ保護されたAPIにアクセスできません。
   - **スコープ**：**カスタム制限付き権限**選択時に表示。付与するスコープを選択します。
   - **備考**：キーの説明を任意で入力可能。
3. **確認**をクリックすると、作成成功ダイアログにAPIキーとシークレットキーが表示されます。

   ::: warning 重要

   APIキーとシークレットキーはこの時点で必ず保存してください。シークレットキーは再表示されません。

   :::

4. **閉じる**をクリックしてダイアログを閉じます。

**権限モード**はダッシュボードのみで利用可能です。REST API利用時は`scopes`フィールドを直接設定してください。詳細は[スコープのデフォルト動作](#default-behavior-of-scopes)を参照してください。

キー名をクリックすると詳細を確認できます。**編集**ボタンで有効期限、状態、ロール、権限モード、スコープ、備考を変更可能です。**削除**ボタンでキーを削除できます。

#### REST API

REST API経由でAPIキーを作成・更新するには、ダッシュボードユーザーのベアラートークンで認証してください。APIキー管理エンドポイントはAPIキー認証を受け付けません。

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

`scopes`に`"unset"`を指定するとロールデフォルトスコープが明示的に適用されます。作成リクエストで`scopes`を省略しても同様です。

ネームスペースは以下のいずれかの方法で指定可能です：

- `administrator`のようなロールと`namespace`フィールドを併用。
- `ns:<namespace>::<role>`形式でロールにネームスペースを含める（例：`ns:team-a::administrator`）。

両形式は引き続きサポートされます。両方がリクエストに含まれる場合、ネームスペースは一致している必要があります。異なる場合や空の場合はHTTP 400を返します。APIキー作成後はネームスペースの変更はできません。

#### ブートストラップファイル

ブートストラップファイル方式でもAPIキーを作成可能です。設定ファイルにファイルパスを指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイルに複数のAPIキーを以下の形式で改行区切りで記述します。

`{APIキー}:{シークレットキー}:{?ロール}:{?スコープ}`

- **APIキー**：任意の文字列をキー識別子として使用。
- **シークレットキー**：ランダムな文字列を使用。
- **ロール（任意）**：キーの[ロール](#roles-and-permissions)を指定。
- **スコープ（任意）**：キーがアクセス可能な[APIスコープ](#api-scopes)をカンマ区切りで指定。省略時はロールのデフォルトが適用されます。ログイン専用スコープ（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）はAPIキーに無効です。これらが含まれると起動時に削除され警告ログが出力されます。キーはスコープなしで作成されます。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

APIキーに割り当て可能なスコープのうち、`system`のみが管理者相当の権限を付与します。EMQX 6.0.4以降、管理者相当スコープと管理者相当でないスコープを混在させたブートストラップエントリは、管理者相当スコープが削除され、残りのスコープが保持されます。警告ログが出力され、キーは作成または更新されます。一方、REST APIは混在したスコープリストをHTTP 400で拒否し、変更は適用されません。

この方法で作成されたAPIキーは無期限で有効です。

EMQX起動時にファイルの内容がAPIキーリストに追加されます。既存のAPIキーがあればシークレットキー、ロール、スコープが更新されます。

### ネームスペース管理者によるAPIキー管理

EMQX 6.0.4以降、ネームスペース管理者は自身のネームスペース内のAPIキーを管理できます。認証にはベアラートークンが必要です。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| APIキー作成 | 管理者のネームスペース内でのみ作成可能。ネームスペース未指定、グローバル指定、他ネームスペース指定はHTTP 403。 |
| APIキー一覧取得 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーはレスポンスから除外。 |
| APIキーの読み取り・更新・削除 | 管理者のネームスペース内のキーのみ操作可能。他ネームスペースのキーはHTTP 404で存在を隠蔽。 |
| APIキーのネームスペース変更 | 他ネームスペースへの移動不可。更新はHTTP 400。 |

グローバル管理者は引き続きすべてのネームスペースのAPIキーを管理可能です。

### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つのプリセットロールのいずれかを割り当てられます。

- **Administrator**：すべてのリソースにアクセス可能。指定がなければデフォルト。ロール識別子は`administrator`。
- **Viewer**：リソースやデータの閲覧のみ可能。REST APIのすべてのGETリクエストに対応。ロール識別子は`viewer`。
- **Publisher**：MQTTメッセージのパブリッシュ専用。メッセージパブリッシュ関連APIのみアクセス可能。ロール識別子は`publisher`。

::: tip 注意
`publisher`キーは`publish`スコープのみ許容します。スコープ割り当て時に`publish`以外のスコープがあるとHTTP 400を返します。キーのロールを`publisher`に変更する場合は、同時リクエストで`"scopes": ["publish"]`または空リストを含めてください。そうしないと既存スコープに`publish`以外がある場合リクエストは拒否されます。
:::

### APIスコープ

スコープはキーごとの権限の次元で、APIのどのビジネス領域にアクセス可能かを宣言します。スコープと[ロールと権限](#roles-and-permissions)は独立しており、両方のチェックを通過した場合にのみアクセスが許可されます。

| 次元 | 目的 | 粒度 |
| --------- | ------- | ----------- |
| **ロール** | HTTP動詞の制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | APIドメインの制限（クライアント、ルール、監視など） | リソース領域 |

マイクロサービスや統合シナリオでは、外部システムがEMQX管理面の一部のみアクセスすることが多いです。例えば監視プラットフォームは`monitoring`スコープのみ、ルールパブリッシュサービスは`data_integration`のみ、クラスター管理ツールは`cluster_operations`のみ必要です。スコープにより最小権限の原則を適用し、キー漏洩時の影響範囲を最小化できます。

::: tip
スコープ名はEMQXのアップグレード間でも変更されない安定した識別子です。OpenAPIタグ名が変更されても、同じスコープを設定したキーは引き続き機能します。
:::

#### 組み込みAPIキー用スコープ

EMQXはAPIキー用に以下10個のスコープを提供しています。

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

EMQXは`system`、`user_management`、`api_key_management`、`sso_management`を管理者相当スコープ（検証メッセージでは`privilege scopes`）と分類しています。これらを制限付きスコープと混在させるとアカウントの実効権限は減りません。4つのうちAPIキーに割り当て可能なのは`system`のみで、他3つは[ログイン専用スコープ](#login-only-scopes)に分類されます。

そのためEMQX 6.0.4以降、APIキー作成・更新時の明示的なスコープリストは`system`単独か`system`を含まないスコープのいずれかでなければなりません。混在リストはHTTP 400を返し変更は適用されません。

既存の混在スコープリストは引き続き動作し、`system`は有効です。次回の明示的なスコープ更新は`system`単独か`system`を含まないリストである必要があります。ダッシュボードで編集時は保存前に権限モード選択を促されます。

:::

#### ログイン専用スコープ

APIキー用スコープに加え、ダッシュボードログインユーザーには4つのログイン専用スコープがあり、ブラウザセッションにのみ適用され、APIキーには割り当てられません。これらの割り当てと適用方法は[ログインユーザースコープ](../dashboard/system.md#login-user-scopes)を参照してください。

| スコープ | 必要ロール | 用途 |
| --- | --- | --- |
| `user_management` | Administrator | ダッシュボードユーザー管理。 |
| `sso_management` | Administrator | SSOバックエンドおよびSSOユーザーレコード管理。 |
| `api_key_management` | Administrator | APIキー管理。 |
| `mfa_management` | 任意 | 自身のMFA管理。管理者は他ユーザーのMFA管理も可能。 |

#### ネームスペース制限付き呼び出し元の制限

ネームスペース制限付き呼び出し元（ロールが特定ネームスペースに制限されたユーザーまたはAPIキー）は、スコープチェックに加えエンドポイントレベルで追加制限を受けます。スコープ付与はこれら制限を上書きしません。

ネームスペース制限付きAPIキーはメッセージパブリッシュAPI（`POST /api/v5/publish`を含む）を呼び出せません。スコープリストに`publish`が含まれていてもこの制限は解除されません。

`connections`または`monitoring`スコープを持つ場合でも、ネームスペース制限付き呼び出し元は保持メッセージや遅延メッセージストアを読み書きするクラスター全体のエンドポイントにアクセスできません。以下のメッセージ関連エンドポイントは`403 Forbidden`を返します。

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

トレース操作では、`GET /trace`は呼び出し元のネームスペース内のトレースのみをリストします。以下のトレース単位操作は、トレースが異なるネームスペースに属する場合`404 Not Found`を返します。

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この挙動は他ネームスペースのトレース情報漏洩を防止します。全件削除操作（`DELETE /trace`）はネームスペース制限付き呼び出し元に対し`403 Forbidden`を返し、全トレースのクリアはグローバル管理者のみ可能です。

ダッシュボードログイン、SSOコールバック、APIキー自己管理エンドポイント（例：`/api_key`）はAPIキー認証を受け付けません。これはスコープモデルとは無関係なダッシュボードのセキュリティ境界です。

#### `scopes`のデフォルト動作

EMQX 6.0.4以降、APIキーの`scopes`フィールドは以下のルールに従います。

| `scopes`の値 | 意味 |
| --- | --- |
| **作成リクエストで未指定** | 選択したロールのデフォルトスコープを使用。 |
| **更新リクエストで未指定** | キーの現在のスコープ設定を保持。 |
| **ロールデフォルトのセントネル `"unset"`** | 明示的なスコープ設定を解除し、ロールデフォルトを使用。ロールデフォルトの変更は自動反映。 |
| **空リスト `[]`** | すべてのビジネスエンドポイントを拒否。キーの一時無効化に有用。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定したスコープのAPIのみ許可。 |

明示的リストがロールデフォルトと同じスコープセットの場合、`"unset"`と同等の効果を持ちます。キーはロールデフォルトの変更に追従します。順序は無関係です。

ブートストラップファイルでスコープセグメントを省略した場合、指定されたロールのデフォルトが適用されます。

スコープはキーがアクセス可能なAPI領域を決定します。ロールやネームスペースの制限を上書きしません。リクエストはロール、スコープ、ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能スコープの一覧取得

EMQXは利用可能なスコープカタログを問い合わせるための2つのエンドポイントを公開しています。

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記10個のビジネスドメインスコープ）を返します。APIキー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（ログイン専用4スコープ含む）を返します。ベアラートークン認証が必要です。

スコープ選択UIの構築や自動化スクリプトの検証に利用してください。

```bash
# APIキー用スコープ一覧取得
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ一覧取得（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの入口から設定可能です。

- **ダッシュボード**：**システム** -> **APIキー**でキー作成・編集時に**権限モード**を選択。**カスタム制限付き権限**の場合に個別スコープを選択。
- **REST API**：作成・更新リクエストボディに`"scopes": ["monitoring", "cluster_operations"]`を含める。
- **ブートストラップファイル**：各行の4番目のセグメントにカンマ区切りのスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ページネーション

大量データを扱う一部APIではページネーション機能を提供しています。データ特性に応じて2種類のページネーション方式があります。

### ページ番号方式

ページネーション対応APIの多くは、`page`（ページ番号）と`limit`（ページサイズ）パラメータで制御可能です。最大ページサイズは`10000`です。`limit`未指定時はデフォルト`100`です。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総件数を予測できないため、`meta.hasnext`で次ページの有無を示します。

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

データ変動が激しくページ番号方式が非効率な一部APIではカーソル方式を採用しています。

`position`または`cursor`（開始位置）パラメータで開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置からの件数を指定します。最大ページサイズは`10000`です。`limit`未指定時はデフォルト`100`です。

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

エラー発生時はボディにJSON形式でエラーコードが返されます。

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
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはAPIキー＆シークレットが間違っています。                    |
| BAD_REQUEST                                    | リクエストパラメータが不正です。                                 |
| NOT_MATCH                                      | 条件が一致しません。                                       |
| ALREADY_EXISTS                                 | リソースがすでに存在します。                                      |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です。                                 |
| BAD_LISTENER_ID                                | リスナーIDが不正です。                                              |
| BAD_NODE_NAME                                  | ノード名が不正です。                                                |
| BAD_RPC                                        | RPC失敗。クラスター状態と対象ノードの状態を確認してください。 |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成しようとしたリソースが最大または最小制限を超えています。 |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています。   |
| CONFLICT                                       | リクエストリソースが競合しています。                                |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使用されていません。                 |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています。                          |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージID解析エラー。                                     |
| INVALID_ID                                     | IDスキーマが不正です。                                                |
| MESSAGE_ID_NOT_FOUND                           | メッセージIDが存在しません。                                    |
| NOT_FOUND                                      | リソースが見つかりません。                         |
| CLIENTID_NOT_FOUND                             | クライアントIDが見つかりません。                        |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常はMQTTクライアントではありません）。 |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません。                                           |
| TOPIC_NOT_FOUND                                | トピックが見つかりません。                                              |
| USER_NOT_FOUND                                 | ユーザーが見つかりません。                                               |
| INTERNAL_ERROR                                 | サーバ内部エラー。                                           |
| SERVICE_UNAVAILABLE                            | サービス利用不可。                                          |
| SOURCE_ERROR                                   | ソースエラー。                                                 |
| UPDATE_FAILED                                  | 更新失敗。                                                 |
| REST_FAILED                                    | リセットソースまたは設定失敗。                          |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません。                                        |
