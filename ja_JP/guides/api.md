# REST API

EMQX は OpenAPI 3.0 仕様に準拠した HTTP 管理 API を公開しています。

このページは、EMQX の REST API と連携または自動化を行う開発者および運用者を対象としています。

EMQX は REST API を探索・操作するための複数の方法を提供しています。EMQX 起動後、以下の API 仕様エンドポイントが利用可能です。

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウン形式の API リファレンスページ。 |
| `/api-spec.md` | Markdown | Markdown 形式の API リファレンス。AI エージェントや自動化ツール向け。 |
| `/api-spec.json` | JSON | JSON 形式の OpenAPI 3.0 仕様。スクリプトやプログラムツール向け。 |
| `/api-spec/:tag[/:name]` | JSON | API タグに絞った OpenAPI 3.0 仕様。リクエストまたはレスポンススキーマ名でさらに絞り込み可能。 |
| `/api-docs/swagger.json` | JSON | 外部 Swagger UI 展開や互換ツール向けの完全な OpenAPI 3.0 仕様。 |

上記のすべてのエンドポイントは、ダッシュボード設定で `swagger_support` を `true`（デフォルト）に設定している必要があります。`false` に設定すると、すべての API ドキュメントエンドポイントが無効になります。詳細は [Dashboard configuration](configuration/dashboard.md) を参照してください。

EMQX 6.3.0 以降、EMQX は Swagger UI を同梱しません。後方互換のため、`/api-docs` または `/api-docs/index.html` へのリクエストは HTTP 308 を返し、`/api-spec.html` にリダイレクトします。リダイレクト先のエンドポイントは認証不要ですが、`/api-spec.html` はリダイレクト後に認証が必要です。`/api-docs/index.html` と `/api-docs/swagger.json` を除き、以前 Swagger UI アセットを提供していた他の `/api-docs/*` サブパスは HTTP 404 を返します。

このセクションでは、EMQX REST API の利用方法を紹介します。

::: tip
EMQX 6.3.0 以降、[feature gates](../get-started/deploy/feature-gates.md) により起動時にオプション機能を無効化できます。無効化された機能が提供する REST API パスはアクセス可能な API エンドポイントとして読み込まれません。`dashboard` 機能が有効な場合、`GET /api/v5/features` を呼び出して解決済みの機能セットを確認できます。
:::

## API 仕様エンドポイントへのアクセス

EMQX 6.3.0 以降、上記のエンドポイントから API 仕様コンテンツを取得するには認証が必要です。

### プログラムによるアクセス

プログラムからのリクエストは、API キーとシークレットキーによる Basic 認証、またはベアラートークン認証のいずれかで認証してください。詳細は [認証](#authentication) を参照してください。

API 仕様へのアクセスは読み取り専用であり、API キーのロールやスコープには依存しません。

`/api-spec.md`、`/api-spec.json`、`/api-spec/:tag[/:name]`、および `/api-docs/swagger.json` へのリクエストで認証情報が欠落または無効な場合、HTTP `401` を返します。

レスポンスボディは要求されたフォーマットですが、要求された API 仕様コンテンツの代わりに最小限の API 仕様を含みます。この最小仕様はサポートされている認証方式を説明し、以下の公開認証およびステータスエンドポイントを列挙します。

- SCRAM ログイン用の `POST /api/v5/login/challenge` と `POST /api/v5/login/verify`
- レガシーパスワードログイン用の `POST /api/v5/login`（`dashboard.password_login` が `both` に設定されている場合のみパスワードログインを受け付けます）
- ブローカー稼働確認用の `GET /api/v5/status`

### ブラウザからのアクセス

ブラウザからは `/api-spec.html` を開いてください。EMQX は有効な `emqx_auth` セッション Cookie を受け入れます。認証されていないリクエストは HTTP `401` を返し、完全な API Spec Explorer ではなく EMQX のサインインページを表示します。ブラウザのネイティブ Basic 認証ダイアログは表示されません。

EMQX 6.3.1 以降、サインインページはデフォルトで SCRAM-SHA-256 を使用します。HTTPS または他の安全なブラウザコンテキストを通じてページを開いてください。TLS はリバースプロキシやロードバランサーで終端可能であり、EMQX ダッシュボードリスナー自体が HTTPS を使用する必要はありません。

ダッシュボードのユーザー名とパスワードでサインインすると、EMQX は `emqx_auth` セッション Cookie を作成し、完全なエクスプローラーを読み込みます。サインアウトするとセッション Cookie はクリアされます。

## ベーシックパス

EMQX は REST API にバージョン管理を導入しており、EMQX 5.0.0 以降のすべての API パスは `/api/v5` で始まります。

## HTTP ヘッダー

ほとんどの API リクエストでは `Accept` ヘッダーに `application/json` を設定する必要があります。そうするとレスポンスは JSON 形式で返されます（特に指定がない限り）。

## HTTP レスポンスステータスコード

EMQX は [HTTP Response Status Code](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status) 標準に準拠しています。主なステータスコードは以下の通りです。

| コード | 説明 |
| ----- | ------------------------------------------------------------ |
| 200   | リクエスト成功。返却される JSON データに詳細が含まれます。 |
| 201   | 作成成功。新規オブジェクトがボディに返されます。 |
| 204   | リクエスト成功。通常は削除や更新操作で返却ボディは空です。 |
| 400   | 不正なリクエスト。通常はリクエストボディやパラメータのエラー。 |
| 401   | 認証エラー。認証情報が欠落、無効、または期限切れです。 |
| 403   | 禁止。オブジェクトが使用中または依存関係の制約があります。 |
| 404   | 見つかりません。ボディの `message` フィールドで理由を確認できます。 |
| 409   | コンフリクト。オブジェクトが既に存在するか、数の上限を超えています。 |
| 500   | サーバ内部エラー。ボディやログで原因を確認してください。 |

## 認証

EMQX の REST API は主に API キーを用いた Basic 認証とベアラートークン認証の2つの方法をサポートしています。

### API キーを用いた Basic 認証

この方法では、API キーとシークレットキーをユーザー名とパスワードとして使用し、API リクエストを認証します。EMQX の REST API は [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework) に準拠しており、これらの認証情報が必要です。EMQX REST API を使用する前に API キーを作成してください。詳細は [API キー管理](#api-key-management) を参照してください。

::: tip 注意

EMQX 5.0.0 以降、ダッシュボードのユーザー名とパスワードは REST API リクエストの Basic 認証資格情報として直接使用できません。ローカルダッシュボードユーザー資格情報で認証する場合は、ダッシュボードのログインフローを使って短期間有効なベアラートークンを取得してください。長期間のプログラム的アクセスには API キーを使用してください。

:::

ダッシュボードのログイン、SSO コールバック、API キーの自己管理エンドポイント（例：`/api_key`）は、キーの `scopes` 設定に関わらず API キー認証を受け付けません。これはスコープモデルとは無関係のダッシュボードのセキュリティ境界です。

#### API キーで認証する

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

クライアントの EMQX へのアクセス方法に応じて認証方式を選択してください。

- 長時間稼働するサービスや無人の自動化には、ダッシュボードのログイントークンは期限切れになるため API キーを使用してください。
- EMQX 6.3.1 以降、ローカルダッシュボードユーザー資格情報で短期間有効なベアラートークンを取得するには SCRAM-SHA-256 チャレンジレスポンス認証を使用してください。

#### SCRAM-SHA-256 でベアラートークンを取得する

パスワードを HTTP リクエストボディに送信せずに SCRAM でベアラートークンを取得する手順：

1. 20～128 文字のパディングなし Base64URL 文字を含むランダムなクライアントノンスを生成します。
2. ユーザー名とクライアントノンスを `POST /api/v5/login/challenge` に送信します。
3. 返されたサーバノンスをクライアントノンスに連結し、結合ノンスを作成します。
4. RFC 7677 SCRAM-SHA-256 メッセージを、チャレンジリクエストで保持した `username` と `client_nonce`、チャレンジレスポンスで返された `server_nonce`、`salt`、`iterations` を使って構築します。

   ```text
   client-first-message-bare = n=<escaped_username>,r=<client_nonce>
   server-first-message = r=<combined_nonce>,s=<salt>,i=<iterations>
   client-final-message-without-proof = c=biws,r=<combined_nonce>
   auth-message = <client-first-message-bare>,<server-first-message>,<client-final-message-without-proof>
   ```

   ユーザー名は RFC 5802 に従い、`=` を `=3D` に、`,` を `=2C` に置換してエスケープします。`server-first-message` にはチャレンジエンドポイントで返された Base64 エンコード済みの `salt` を使用します。
5. クライアント証明と期待されるサーバ署名を以下のように計算します。`HMAC-SHA-256(key, message)` はキーとメッセージの順で引数を取ります。`UTF8(value)` は文字列を UTF-8 バイト列に変換し、`Base64Decode(value)` は Base64 文字列をデコードし、`XOR` はバイト単位の排他的論理和を表します。

   ```text
   salted-password = PBKDF2-HMAC-SHA-256(UTF8(password), Base64Decode(salt), iterations, 32 bytes)
   client-key = HMAC-SHA-256(salted-password, "Client Key")
   stored-key = SHA-256(client-key)
   client-signature = HMAC-SHA-256(stored-key, UTF8(auth-message))
   client-proof = client-key XOR client-signature
   server-key = HMAC-SHA-256(salted-password, "Server Key")
   expected-server-signature = HMAC-SHA-256(server-key, UTF8(auth-message))
   ```

   `client-proof` を Base64 エンコードし、チャレンジ ID と結合ノンスとともに `POST /api/v5/login/verify` の `client_proof` フィールドに送信します。多要素認証が有効な場合は `mfa_token` も含めます。
6. レスポンスの `server_signature` を Base64 デコードし、`expected-server-signature` と比較してから `token` フィールドのベアラートークンを使用します。

チャレンジには有効期限があります。`POST /api/v5/login/verify` への正しい形式のリクエストは、認証成功・失敗に関わらずチャレンジを消費します（`BAD_MFA_TOKEN` 返却時も含む）。Base64 エンコード不正や長さ不正の `client_proof`、結合ノンス形式不正などの事前検証で拒否されたリクエストはチャレンジを消費せず再試行可能です。消費済みチャレンジが失敗した場合は新しいチャレンジを取得し、クライアント証明を再計算してください。

リクエスト・レスポンススキーマは [API 仕様](#access-api-specification-endpoints) の `dashboard` セクションを参照してください。

ブラウザベースの SCRAM ログインには HTTPS または他の安全なブラウザコンテキストが必要です。

#### パスワードログインでベアラートークンを取得する

互換性のためのエンドポイント `POST /api/v5/login` は、`dashboard.password_login` が `both` に設定されている場合のみユーザー名とパスワードを受け付けます。`dashboard.password_login` が `scram_only` の場合は HTTP `403` とエラーコード `PASSWORD_LOGIN_DISABLED` を返します。上記の SCRAM フローか API キーを使用してください。

パスワードログインが有効な場合、ローカルアクセス用のエンドポイントは以下の通りです。

```bash
POST http://localhost:18083/api/v5/login
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

- `"admin"` と `"yourpassword"` は EMQX ダッシュボードの認証情報に置き換えてください。

この例はローカルホストの HTTP を使用しています。リモートアクセスの場合は HTTPS リスナーを設定し、HTTPS 経由でリクエストを送信してください。

レスポンスにはベアラートークンが含まれ、API リクエストの認証に使用できます。

#### ベアラートークンを使った認証

ベアラートークンを取得したら、API リクエストの `Authorization` ヘッダーに以下のように含めてください。

```bash
--header "Authorization: Bearer <your-token>"
```

## API キー管理

このセクションでは、API キーの作成・管理方法とロール、ネームスペース、スコープの設定について説明します。

### API キーの作成

#### ダッシュボード

ダッシュボードの **System** -> **API Keys** から手動で API キーを作成できます。

1. 右上の **+ Create** ボタンをクリックして作成ダイアログを開きます。
2. API キーの詳細を設定します。
   - **Name**（必須）：API キーの名前を入力します。
   - **Expire At**：空欄の場合は期限なしとなります。
   - **Is Enable**：デフォルトで有効です。
   - **Role**：ロールを選択します（任意）。[ロールと権限](#roles-and-permissions)を参照してください。
   - **Namespace**：デフォルトはオフです。グローバル管理者の場合はオフのままにするとグローバル API キーが作成されます。オンにしてネームスペースを選択すると、そのネームスペース内のキーが作成されます。ネームスペース管理者は自分のネームスペース内でのみキーを作成可能です。
   - **Permission Mode**：管理者またはビューアのキーの場合、スコープの割り当て方法を選択します。パブリッシャーキーには表示されず、ロールのデフォルト `publish` スコープが使用されます。スコープの挙動と制限は [API スコープ](#api-scopes) を参照してください。
     - **Role Default Scopes**：選択したロールのデフォルトを使用します。ロールデフォルトの変更は自動的に反映されます。
     - **System-level Permissions**：`system` スコープのみを付与します。
     - **Custom Restricted Permissions**：アクセス可能な API 領域を制限するために1つ以上のスコープを選択します。**Scopes** を空欄にするとスコープ保護された API にはアクセスできません。
   - **Scopes**：**Custom Restricted Permissions** 選択時に表示され、付与するスコープを選択します。
   - **Note**：任意で説明を入力します。
3. **Confirm** をクリックすると、API キーとシークレットキーが **Created Successfully** ダイアログに表示されます。

   ::: warning 重要

   API キーとシークレットキーはすぐに保存してください。シークレットキーは再表示されません。

   :::

4. **Close** をクリックしてダイアログを閉じます。

**Permission Mode** はダッシュボードでのみ利用可能です。REST API では `scopes` フィールドを直接設定してください。詳細は [scopes のデフォルト挙動](#default-behavior-of-scopes) を参照してください。

キーの詳細は名前をクリックして表示できます。**Edit** ボタンで有効期限、状態、ロール、パーミッションモード、スコープ、説明を変更可能です。**Delete** ボタンでキーを削除できます。

#### REST API

REST API ではダッシュボードユーザーのベアラートークンを使って API キーを作成・更新します。API キー管理エンドポイントは API キー認証を受け付けません。

EMQX 6.0.4 以降、`POST /api/v5/api_key` と `PUT /api/v5/api_key/:name` のリクエストボディにトップレベルの `namespace` フィールドを指定可能です。例として、`team-a` ネームスペースに管理者 API キーを作成するリクエスト：

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

`scopes` に `"unset"` を指定すると、後方互換のためスコープ許可リストが明示的に解除されます。ロール、ネームスペース、API キー固有のパス制限は引き続き適用されます。作成リクエストで `scopes` を省略するとロールのデフォルトスコープが適用されます。

ネームスペースは以下のいずれかの方法で指定可能です。

- `administrator` のような素のロールと `namespace` フィールドを組み合わせる。
- `ns:<namespace>::<role>` の形式でロールにネームスペースをエンコードする（例：`ns:team-a::administrator`）。

両形式は引き続きサポートされます。両方が含まれる場合はネームスペースが一致する必要があります。異なる場合や空の場合は HTTP 400 を返します。API キー作成後は REST API でネームスペースを変更できません。

EMQX 6.3.0 以降、`multi_tenancy.deny_namespaces` にリストされたネームスペースは両形式とも使用できません。設定詳細は [Denied Namespace Names](multi-tenancy/namespace-global-settings.md#denied-namespace-names) を参照してください。

グローバル API キーを作成するには、`namespace` を省略し、ネームスペースプレフィックスなしのロールを使用してください。`namespace` に文字列 `"global"` を設定してもグローバルスコープにはなりません。

#### ブートストラップファイル

ブートストラップファイルを使って API キーを作成することも可能です。設定ファイルに以下を追加してファイルの場所を指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定したファイルに複数の API キーを以下の形式で改行区切りで記述します。

```
{API Key}:{Secret Key}:{?Role}:{?Scopes}
```

- **API Key**：任意の文字列でキー識別子。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの [ロール](#roles-and-permissions)。ネームスペース付きの場合は `ns:<namespace>::<role>` 形式（例：`ns:team-a::administrator`）。
- **Scopes（任意）**：キーに許可する [API スコープ](#api-scopes) をカンマ区切りで指定。省略時はロールのデフォルトが適用されます。検証挙動は [Validate Bootstrap Scopes](#validate-bootstrap-scopes) を参照。

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

- **ログイン専用スコープ**：`user_management`、`mfa_management`、`sso_management`、`api_key_management` は API キーには無効です。EMQX はこれらを削除し、残りのスコープでキーを作成・更新します。
- **管理者相当スコープ**：API キーに割り当て可能なスコープのうち、`system` のみが管理者相当権限を付与します。EMQX 6.0.4 以降、管理者相当スコープと管理者相当でないスコープが混在する場合、管理者相当スコープをすべて削除し、残りのスコープを保持します。
- **ネームスペース付きスコープ**：ネームスペース付きエントリがロールで保持できないスコープを明示的に指定している場合、EMQX は許可されないスコープを削除し、残りを保持します。スコープが残らない場合、キーはスコープ保護されたビジネス API にアクセスできません。許可されるスコープは [ネームスペース付き呼び出し元の制限](#restrictions-for-namespaced-callers) を参照してください。

##### ブートストラップ API キーのリロード

ブートストラップファイルから作成された API キーは無期限に有効です。EMQX は起動時にファイルを処理します。既存の API キーがある場合はロール、ネームスペース、スコープを更新します。

ファイル内のシークレットキーが変更されていなければ、EMQX は保存済みのハッシュを保持します。変更されていれば新しいハッシュを生成し、以前のシークレットキーは無効になります。

::: warning 重要

EMQX 6.2 から 6.3.1 へのローリングアップグレード中は、すべてのノードが EMQX 6.3 を実行するまでブートストラップ API キーのシークレットキーを変更しないでください。変更されたシークレットキーは 6.3 のハッシュ形式で保存され、6.2 実行中のノードは検証できません。

:::

### ネームスペース管理者による API キー管理

EMQX 6.0.4 以降、ネームスペース付きダッシュボード管理者は自分のネームスペース内の API キーを管理できます。管理者はベアラートークンで認証する必要があります。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| API キー作成 | 管理者のネームスペース内でのみ作成可能。ネームスペース省略、グローバル指定、他ネームスペース指定は HTTP 403。 |
| API キー一覧 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーはレスポンスから除外。 |
| API キーの読み取り・更新・削除 | 管理者のネームスペース内のキーのみ操作可能。他ネームスペースのキーは HTTP 404 を返し存在を隠蔽。 |
| API キーのネームスペース変更 | 他ネームスペースへの移動不可。更新は HTTP 400。 |

グローバルダッシュボード管理者は引き続き全ネームスペースの API キーを管理可能です。

## API キーの権限

### ロールと権限

REST API はロールベースアクセス制御を実装しています。API キー作成時に以下の3つの事前定義ロールのいずれかを割り当てられます。

- **Administrator**：すべてのリソースにアクセス可能。ロール指定なしの場合のデフォルト。ロール識別子は `administrator`。
- **Viewer**：リソースやデータの閲覧のみ可能。REST API のすべての GET リクエストに対応。ロール識別子は `viewer`。
- **Publisher**：MQTT メッセージパブリッシュ専用。メッセージパブリッシュ関連 API のみアクセス可能。ロール識別子は `publisher`。

::: tip 注意
`publisher` キーは `publish` スコープのみ受け入れます。スコープ割り当て時に `publish` 以外のスコープがあると HTTP 400 を返します。キーのロールを `publisher` に変更する場合は、同時リクエストで `"scopes": ["publish"]` または空リストを含めてください。そうしないと既存スコープに `publish` 以外がある場合リクエストは拒否されます。
:::

### API スコープ

スコープはキーごとの権限の次元であり、キーがアクセス可能な REST API のビジネス領域を宣言します。スコープと [ロールと権限](#roles-and-permissions) は独立しており、両方のチェックを通過した場合のみアクセスが許可されます。2つのアクセス制御層を形成します。

| 次元 | 目的 | 粒度 |
| --------- | ------- | ----------- |
| **ロール** | HTTP 動詞の制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | API ドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方を通過する必要があります。

マイクロサービスや統合シナリオでは、外部システムが EMQX 管理面の一部のみアクセスすることが多いです。監視プラットフォームは `monitoring` スコープのみ、ルールパブリッシュサービスは `data_integration` のみ、クラスター運用ツールは `cluster_operations` のみ必要です。スコープにより最小権限の原則でキーを割り当て、キー漏洩時の影響範囲を最小化できます。

::: tip
スコープ名は安定した識別子であり、EMQX のアップグレードで変更されません。OpenAPI タグ名が変更されても、同じスコープを持つキーは引き続き動作します。
:::

#### 組み込み API キースコープ

EMQX は API キー向けに11のスコープを提供しています。

| スコープ | 名称 | 代表的な API 領域 |
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
| `plugin_api` | プラグイン拡張 API | `/plugin_api/{plugin}/...` |

`plugin_api` スコープはプラグイン API ゲートウェイ経由でプラグインが公開するエンドポイントへのアクセスを許可します。プラグインのインストール、起動、停止、設定エンドポイント（`/plugins*` 以下）は `system` スコープに含まれ、`plugin_api` スコープではアクセスできません。ゲートウェイは `system` も受け入れるため、すでに `system` を持つ API キーはアクセスを維持します。

`plugin_api` スコープはゲートウェイへのアクセスを制御し、プラグインが実装する操作を制御するものではありません。プラグインが公開する各エンドポイントのセキュリティ影響を確認してからこのスコープを割り当ててください。

::: tip 注意

`audit` スコープはネームスペース付き呼び出し元に監査ログアクセスを付与しません。`GET /api/v5/audit` を呼べるのはグローバル管理者とグローバルビューアのみです。詳細は [Audit Log Access](./dashboard/audit-log.md#audit-log-access) を参照してください。

:::

::: warning 管理者相当スコープと制限スコープを混在させないでください

EMQX は `system`、`user_management`、`api_key_management`、`sso_management` を管理者相当スコープ（検証メッセージでは `privilege scopes`）として分類しています。これらを制限スコープと組み合わせてもアカウントの実効権限は減りません。4つのうち API キーに割り当て可能なのは `system` のみで、他3つは [ログイン専用スコープ](#login-only-scopes) に記載されています。

そのため EMQX 6.0.4 以降、API キー作成・更新時の明示的スコープリストは `system` のみ、または `system` を含まないスコープ群のどちらかでなければなりません。混在するリストは HTTP 400 を返し、変更は適用されません。

既存の混在スコープリストは動作を継続し、`system` は有効なままです。次回の明示的スコープ更新は `system` のみか、`system` を含まないリストでなければなりません。ダッシュボードで編集する場合は保存前にパーミッションモードの選択を促されます。

:::

#### ログイン専用スコープ

11の API キースコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、API キーには割り当てられません。ログインユーザーへの割り当てと適用方法は [Login User Scopes](dashboard/system.md#login-user-scopes) を参照してください。

| スコープ | 必要ロール | 目的 |
| --- | --- | --- |
| `user_management` | Administrator | ダッシュボードユーザー管理。 |
| `sso_management` | Administrator | SSO バックエンドおよび SSO ユーザーレコード管理。 |
| `api_key_management` | Administrator | API キー管理。 |
| `mfa_management` | グローバル管理者またはグローバルビューア | 自身の MFA 管理。管理者は他ユーザーの MFA も管理可能。 |

#### `scopes` のデフォルト挙動

EMQX 6.0.4 以降、API キーの `scopes` フィールドは以下のルールに従います。

| `scopes` の値 | 意味 |
| --- | --- |
| **作成リクエストで省略** | 選択したロールのデフォルトを使用。 |
| **更新リクエストで省略** | キーの現在のスコープ設定を保持。 |
| **解除用セントネル `"unset"`** | 明示的なスコープ設定を削除。後方互換のためスコープ許可リストは適用されません。ロール、ネームスペース、API キー固有のパス制限は引き続き適用。 |
| **空リスト `[]`** | すべてのビジネスエンドポイントを拒否。キーをソフト無効化する用途に便利。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定したスコープの API のみ許可。 |

ロールデフォルトと同じスコープセットの明示的リストは `"unset"` に正規化され、同様に扱われます。順序は無視されます。

ブートストラップファイルのエントリでスコープ区切りが省略された場合、EMQX は処理時に指定ロールのデフォルトを適用します。

スコープはキーがアクセス可能な API 領域を決定し、ロールやネームスペース制限を上書きしません。リクエストはロール・スコープ・ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能なスコープの一覧

EMQX は利用可能なスコープカタログを問い合わせるために2つのエンドポイントを公開しています。

- `GET /api/v5/api_key_scopes`：API キーに割り当て可能なスコープ（上記11のビジネスドメインスコープ）を返します。API キーで認証してください。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（ログイン専用4スコープを含む）を返します。ベアラートークンで認証してください。

スコープ選択 UI の初期化や自動化スクリプトの検証に利用できます。

```bash
# API キースコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの入口から設定可能です。

- **ダッシュボード**：**System** -> **API Keys** でキー作成・編集時に **Permission Mode** を選択。**Custom Restricted Permissions** の場合に個別スコープを選択。
- **REST API**：作成・更新リクエストボディに `"scopes": ["monitoring", "cluster_operations"]` を含める。
- **ブートストラップファイル**：各行の4番目の区切りとしてカンマ区切りスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ネームスペース付き呼び出し元の制限

ネームスペース付き呼び出し元（ロールが特定ネームスペースに制限されたユーザーや API キー）は、スコープチェックに加えてエンドポイントレベルの追加制限を受けます。スコープの付与はこれらの制限を上書きしません。

### ネームスペース付き API キーのスコープ制限

ネームスペース付き API キーのスコープ許可リストは以下を含みます。

`connections`, `monitoring`, `data_integration`, `access_control`, `system`, `cluster_operations`, `license`, `plugin_api`

- **デフォルトスコープ**：作成リクエストで `scopes` を省略した場合、管理者またはビューアロールのネームスペース付き API キーは実行中の EMQX バージョンのネームスペース許可リスト内のすべてのスコープを受け取ります。許可リストには `publish`、`gateways`、`audit` は含まれません。
- **明示的スコープ**：ネームスペース付き API キーの作成・更新時に明示的スコープリストは実行中の EMQX バージョンのネームスペース許可リストに収まる必要があります。そうでない場合、EMQX は HTTP 400 を返し、許可されないスコープを特定して変更を適用しません。明示的リストは `system` と制限スコープの混在も禁止されます。

### 許可されないスコープを持つ既存キー

許可されないスコープを含むキーは自動的に変更されません。読み取り・修正・書き込みクライアントの互換性のため、更新時に同じスコープリストを再送信し、ロールとネームスペースが同じなら許可されます。ただし、唯一 `publish` のみを含むネームスペース付きキーは API にアクセスできないため、更新時に HTTP 400 を返します。キーを削除してネームスペースなしで再作成してください。実際のロール・スコープ変更は再検証され、許可リストに準拠する必要があります。

ブートストラップエントリの再処理時は、許可されないスコープを削除し警告ログを出力し、残りのスコープを保持します。詳細は [Validate Bootstrap Scopes](#validate-bootstrap-scopes) を参照してください。

### メッセージパブリッシュの制限

ネームスペース付き API キーはメッセージパブリッシュ API（`POST /api/v5/publish` など）を呼べません。以前のスコープリストに `publish` が含まれていても、スコープ割り当てはネームスペースレベルの制限を上書きしません。

### メッセージコンテンツの制限

ネームスペース付き呼び出し元が `connections` または `monitoring` スコープを持っていても、クラスター全体の MQTT メッセージコンテンツ（保持メッセージや遅延メッセージストア）を読み書きするエンドポイントにはアクセスできません。以下のメッセージ関連エンドポイントは `403 Forbidden` を返します。

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

ファイル転送ストアはグローバルでネームスペース非対応です。ネームスペース付き呼び出し元は以下のファイル転送コンテンツエンドポイントにアクセスできず、スコープ付与はこの制限を上書きしません。

- `GET /file_transfer/files`
- `GET /file_transfer/files/:clientid/:fileid`
- `GET /file_transfer/file`

グローバル呼び出し元はロール・スコープに応じてこれらのエンドポイントにアクセス可能です。`/file_transfer` 設定エンドポイントは影響を受けません。

### トレースの制限

トレース操作では、`GET /trace` は呼び出し元のネームスペース内のトレースのみを一覧表示します。以下のトレース単位操作は、トレースが異なるネームスペースに属する場合 `404 Not Found` を返します。

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この挙動は他ネームスペースのトレース情報の漏洩を防止します。トレースの一括削除（`DELETE /trace`）はネームスペース付き呼び出し元に対して `403 Forbidden` を返し、すべてのトレースをクリアできるのはグローバル管理者のみです。

ダッシュボードログイン、SSO コールバック、API キー自己管理エンドポイント（例：`/api_key`）は、キーの `scopes` 設定に関わらず API キー認証を受け付けません。これはスコープモデルとは無関係のダッシュボードのセキュリティ境界です。

## ページネーション

大量データを扱う一部 API ではページネーション機能を提供しています。データ特性に応じて2種類のページネーション方式があります。

### ページ番号ページネーション

ページネーション対応の多くの API では、`page`（ページ番号）と `limit`（ページサイズ）パラメータでページングを制御できます。最大ページサイズは `10000` です。`limit` を指定しない場合はデフォルト `100` です。

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

データの変動が激しくページ番号ページネーションが非効率な一部 API ではカーソルページネーションを使用します。

`position` または `cursor`（開始位置）パラメータでデータの開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置から読み込む件数を指定します。最大ページサイズは `10000` です。`limit` を指定しない場合はデフォルト `100` です。

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

HTTP レスポンスステータスコードに加え、EMQX は特定エラーを識別するためのエラーコード一覧を定義しています。

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
| WRONG_USERNAME_OR_PWD                          | ユーザー名またはパスワードが間違っています。                  |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはキー＆シークレットが間違っています。 |
| BAD_REQUEST                                    | リクエストパラメータが不正です。                                 |
| NOT_MATCH                                      | 条件が一致しません。                                       |
| ALREADY_EXISTS                                 | リソースがすでに存在します。                                      |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です。                                       |
| BAD_LISTENER_ID                                | リスナー ID が不正です。                                      |
| BAD_NODE_NAME                                  | ノード名が不正です。                                        |
| BAD_RPC                                        | RPC に失敗しました。クラスター状態と対象ノードの状態を確認してください。 |
| BAD_TOPIC                                      | トピックの構文エラー。トピックは MQTT プロトコル標準に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成しようとするリソースが最大または最小制限を超えています。           |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています。               |
| CONFLICT                                       | リクエストリソースに競合があります。                              |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使われていません。               |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています。                          |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージ ID の解析エラーです。                                  |
| INVALID_ID                                     | ID スキーマが不正です。                                       |
| MESSAGE_ID_NOT_FOUND                           | メッセージ ID が存在しません。                                  |
| NOT_FOUND                                      | リソースが見つかりません。                                    |
| CLIENTID_NOT_FOUND                             | クライアント ID が見つかりません。                              |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常は MQTT クライアントではありません）。 |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません。                                    |
| TOPIC_NOT_FOUND                                | トピックが見つかりません。                                    |
| USER_NOT_FOUND                                 | ユーザーが見つかりません。                                    |
| INTERNAL_ERROR                                 | サーバ内部エラーです。                                       |
| SERVICE_UNAVAILABLE                            | サービスが利用できません。                                    |
| SOURCE_ERROR                                   | ソースエラーです。                                          |
| UPDATE_FAILED                                  | 更新に失敗しました。                                        |
| REST_FAILED                                    | ソースまたは設定のリセットに失敗しました。                          |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません。                                  |
