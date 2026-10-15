# REST API

EMQXはOpenAPI 3.0仕様に準拠したHTTP管理APIを公開しています。

このページは、EMQXのREST APIを通じて統合や自動化を行う開発者および運用者向けです。

EMQXはREST APIを探索・操作するための複数の方法を提供しています。EMQX起動後、以下のAPI仕様エンドポイントが利用可能です：

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウン形式のAPIリファレンスページ。 |
| `/api-spec.md` | Markdown | Markdown形式のAPIリファレンス。AIエージェントや自動化ツール向け。 |
| `/api-spec.json` | JSON | JSON形式のOpenAPI 3.0仕様。スクリプトやプログラム的ツール向け。 |
| `/api-spec/:tag[/:name]` | JSON | APIタグにフォーカスしたOpenAPI 3.0仕様。リクエストまたはレスポンススキーマ名で絞り込み可能。 |
| `/api-docs/swagger.json` | JSON | 外部Swagger UIや互換ツール向けの完全なOpenAPI 3.0仕様。 |

上記のすべてのエンドポイントは、ダッシュボード設定で`swagger_support`が`true`（デフォルト）に設定されている必要があります。`false`に設定すると、すべてのAPIドキュメントエンドポイントが無効になります。詳細は[ダッシュボード設定](configuration/dashboard.md)を参照してください。

EMQX 6.3.0以降、EMQXはSwagger UIを同梱しません。後方互換のため、`/api-docs`または`/api-docs/index.html`へのリクエストはHTTP 308を返し、`/api-spec.html`へリダイレクトします。リダイレクト先のエンドポイントは認証不要ですが、リダイレクト後の`/api-spec.html`は認証が必要です。`/api-docs/index.html`と`/api-docs/swagger.json`を除き、以前Swagger UI資産を提供していた他の`/api-docs/*`サブパスはHTTP 404を返します。

本節ではEMQX REST APIの利用方法を紹介します。

::: tip
EMQX 6.3.0以降、[feature gates](../get-started/deploy/feature-gates.md)により起動時にオプション機能を無効化できます。無効化された機能が提供するREST APIパスはアクセス可能なAPIエンドポイントとして読み込まれません。`dashboard`機能が有効な場合、`GET /api/v5/features`を呼び出して解決済みの機能セットを確認できます。
:::

## API仕様エンドポイントへのアクセス

EMQX 6.3.0以降、上記のAPI仕様エンドポイントから仕様内容を取得するには認証が必要です。

### プログラム的アクセス

APIキーとシークレットキーを用いたBasic認証、またはベアラートークンによる認証でプログラム的リクエストを認証します。詳細は[認証](#authentication)を参照してください。

API仕様へのアクセスは読み取り専用であり、APIキーのロールやスコープに依存しません。

`/api-spec.md`、`/api-spec.json`、`/api-spec/:tag[/:name]`、`/api-docs/swagger.json`への認証情報が欠落または無効なリクエストはHTTP `401`を返します。

レスポンスボディは要求されたフォーマットを使用しますが、要求されたAPI仕様の内容ではなく最小限のAPI仕様を含みます。この最小仕様はサポートされる認証スキームを記述し、以下の公開認証およびステータスエンドポイントを列挙します：

- SCRAMログイン用の`POST /api/v5/login/challenge`および`POST /api/v5/login/verify`
- レガシーパスワードログイン用の`POST /api/v5/login`（`dashboard.password_login`が`both`に設定されている場合のみパスワードログインを受け付けます）
- ブローカー稼働確認用の`GET /api/v5/status`

### ブラウザアクセス

ブラウザからは`/api-spec.html`を開きます。EMQXは有効な`emqx_auth`セッションCookieを受け入れます。認証されていないリクエストはHTTP `401`を返し、完全なAPI Spec ExplorerやブラウザのBasic認証ダイアログではなくEMQXのサインインページを表示します。

EMQX 6.3.1以降、サインインページはデフォルトでSCRAM-SHA-256を使用します。HTTPSまたはその他の安全なブラウザコンテキスト経由でページを開いてください。TLSはリバースプロキシやロードバランサーで終端可能であり、EMQXダッシュボードリスナー自体がHTTPSを使用する必要はありません。

ダッシュボードのユーザー名とパスワードでサインインすると、EMQXは`emqx_auth`セッションCookieを作成し、完全なエクスプローラーを読み込みます。サインアウトするとセッションCookieはクリアされます。

## ベーシックパス

EMQXはREST APIにバージョン管理を導入しており、EMQX 5.0.0以降のすべてのAPIパスは`/api/v5`で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは`Accept`ヘッダーに`application/json`を設定する必要があり、指定がなければレスポンスはJSON形式で返されます。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)標準に準拠しています。主なステータスコードは以下の通りです：

| コード | 説明 |
| ----- | ------------------------------------------------------------ |
| 200   | リクエスト成功。返却されたJSONデータに詳細が含まれます。 |
| 201   | 作成成功。新規オブジェクトがボディに返されます。 |
| 204   | リクエスト成功。通常は削除や更新操作で、返却ボディは空です。 |
| 400   | 不正なリクエスト。リクエストボディやパラメータのエラー。 |
| 401   | 認証エラー。認証情報が欠落、無効、または期限切れです。 |
| 403   | 禁止。オブジェクトが使用中、または依存関係制約があります。 |
| 404   | 見つかりません。ボディの`message`フィールドで理由を確認可能。 |
| 409   | 競合。オブジェクトが既に存在するか、数の上限を超過。 |
| 500   | サーバ内部エラー。ボディやログで原因を確認してください。 |

## 認証

EMQXのREST APIは主に2つの認証方法をサポートしています：APIキーを用いたBasic認証とベアラートークン認証です。

### APIキーを用いたBasic認証

この方法では、APIキーとシークレットキーをユーザー名とパスワードとして使用し、APIリクエストを認証します。EMQXのREST APIは[HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを使用する前にAPIキーを作成してください。詳細は[APIキー管理](#api-key-management)を参照してください。

::: tip 注意

EMQX 5.0.0以降、ダッシュボードのユーザー名とパスワードはREST APIリクエストのBasic認証資格情報として直接使用できません。ローカルダッシュボードユーザー資格情報で認証する場合は、ダッシュボードのログインフローを使って短期間有効なベアラートークンを取得してください。長時間のプログラム的アクセスにはAPIキーを使用してください。

:::

ダッシュボードのログイン、SSOコールバック、APIキーの自己管理エンドポイント（例：`/api_key`）は、キーの`scopes`設定に関わらずAPIキー認証を受け付けません。これはスコープモデルとは無関係のダッシュボードのセキュリティ境界です。

#### APIキーでの認証

APIキーとシークレットキーを入手したら、APIキーをユーザー名、シークレットキーをパスワードとしてBasic認証を行います。

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

クライアントのEMQXアクセス方法に応じて認証方法を選択してください：

- 長時間稼働するサービスや無人自動化には、ダッシュボードのログイントークンは期限切れになるためAPIキーを使用してください。
- EMQX 6.3.1以降、ローカルダッシュボードユーザー資格情報でSCRAM-SHA-256チャレンジレスポンス認証を用いて短期間有効なベアラートークンを取得できます。

#### SCRAM-SHA-256でベアラートークンを取得する

パスワードをHTTPリクエストボディに送信せずにSCRAMでベアラートークンを取得する手順：

1. 20〜128文字のパディングなしBase64URL文字列を含むランダムなクライアントノンスを生成します。
2. ユーザー名とクライアントノンスを`POST /api/v5/login/challenge`に送信します。
3. 返されたサーバーノンスをクライアントノンスに連結して結合ノンスを作成します。
4. チャレンジリクエストで保持した`username`と`client_nonce`、チャレンジレスポンスで返された`server_nonce`、`salt`、`iterations`を用いてRFC 7677 SCRAM-SHA-256メッセージを構築します：

   ```text
   client-first-message-bare = n=<escaped_username>,r=<client_nonce>
   server-first-message = r=<combined_nonce>,s=<salt>,i=<iterations>
   client-final-message-without-proof = c=biws,r=<combined_nonce>
   auth-message = <client-first-message-bare>,<server-first-message>,<client-final-message-without-proof>
   ```

   ユーザー名はRFC 5802に従い、`=`を`=3D`に、`,`を`=2C`に置換してエスケープします。`server-first-message`の`salt`はチャレンジエンドポイントで返されたBase64エンコード値を使用します。
5. クライアント証明と期待されるサーバ署名を以下のように計算します。`HMAC-SHA-256(key, message)`は引数の順序を示します。`UTF8(value)`は文字列をUTF-8バイトにエンコード、`Base64Decode(value)`はBase64文字列をデコード、`XOR`はバイト単位の排他的論理和です。

   ```text
   salted-password = PBKDF2-HMAC-SHA-256(UTF8(password), Base64Decode(salt), iterations, 32 bytes)
   client-key = HMAC-SHA-256(salted-password, "Client Key")
   stored-key = SHA-256(client-key)
   client-signature = HMAC-SHA-256(stored-key, UTF8(auth-message))
   client-proof = client-key XOR client-signature
   server-key = HMAC-SHA-256(salted-password, "Server Key")
   expected-server-signature = HMAC-SHA-256(server-key, UTF8(auth-message))
   ```

   `client-proof`をBase64エンコードし、チャレンジIDと結合ノンスと共に`POST /api/v5/login/verify`の`client_proof`フィールドに送信します。多要素認証が有効な場合は`mfa_token`も含めます。
6. レスポンスの`server_signature`をBase64デコードし、`expected-server-signature`と比較してから`token`フィールドのベアラートークンを使用してください。

チャレンジは時間制限付きです。`POST /api/v5/login/verify`への正しいリクエストは認証の成否に関わらずチャレンジを消費します（`BAD_MFA_TOKEN`返却時も含む）。Base64エンコード不正や`client_proof`のデコード長不正、`combined_nonce`形式不正などの事前検証で拒否されたリクエストはチャレンジを消費せず再試行可能です。消費済みチャレンジが失敗した場合は新しいチャレンジを取得し、クライアント証明を再計算してください。

リクエスト・レスポンススキーマは[API仕様](#access-api-specification-endpoints)の`dashboard`セクションを参照してください。

ブラウザベースのSCRAMログインはHTTPSまたはその他の安全なブラウザコンテキストが必要です。

#### パスワードログインでベアラートークンを取得する

互換性のための`POST /api/v5/login`は`dashboard.password_login`が`both`に設定されている場合のみユーザー名とパスワードを受け付けます。`scram_only`の場合はHTTP `403`とエラーコード`PASSWORD_LOGIN_DISABLED`を返します。上記のSCRAMフローかAPIキーを使用してください。

パスワードログインが有効な場合、ローカルアクセスには以下のエンドポイントを使用します：

```bash
POST http://localhost:18083/api/v5/login
```

**ヘッダー：**

- `Content-Type: application/json`

**リクエストボディ：**

```json
{
  "username": "admin",
  "password": "yourpassword"
}
```

- `"admin"`と`"yourpassword"`はEMQXダッシュボードの資格情報に置き換えてください。

この例はlocalhostのHTTPを使用しています。リモートアクセスにはHTTPSリスナーを設定し、HTTPS経由で送信してください。

レスポンスにはベアラートークンが含まれ、APIリクエストの認証に使用できます。

#### ベアラートークンを使った認証

ベアラートークンを取得したら、APIリクエストの`Authorization`ヘッダーに以下のように含めます：

```bash
--header "Authorization: Bearer <your-token>"
```

## APIキー管理

本節ではAPIキーの作成・管理方法とロール、ネームスペース、スコープの設定について説明します。

### APIキーの作成

#### ダッシュボード

ダッシュボードの**システム** -> **APIキー**から手動でAPIキーを作成できます：

1. 右上の**+ 作成**ボタンをクリックして作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **名前**（必須）：APIキーの名前を入力します。
   - **有効期限**：空欄の場合は期限なしになります。
   - **有効化**：デフォルトで有効です。
   - **ロール**：ロールを選択（任意）。[ロールと権限](#roles-and-permissions)を参照してください。
   - **ネームスペース**：デフォルトはオフ。グローバル管理者の場合はオフのままでグローバルAPIキーが作成されます。オンにしてネームスペースを選択すると、そのネームスペース内のキーが作成されます。ネームスペース管理者は自分のネームスペース内のみキーを作成可能です。
   - **権限モード**：管理者または閲覧者キーの場合に表示されます。パブリッシャーキーはロールデフォルトの`publish`スコープを使用するため表示されません。スコープの動作や制限は[APIスコープ](#api-scopes)を参照してください。
     - **ロールデフォルトスコープ**：選択したロールのデフォルトを使用。ロールデフォルトの変更は自動的に反映されます。
     - **システムレベル権限**：`system`スコープのみ付与。
     - **カスタム制限付き権限**：1つ以上のスコープを選択し、アクセス可能なAPI領域を制限。**スコープ**を空欄にするとスコープ保護されたAPIにアクセスできません。
   - **スコープ**：**カスタム制限付き権限**を選択した場合に表示。付与するスコープを選択します。
   - **備考**：任意で説明を入力可能。
3. **確認**をクリックすると、APIキーとシークレットキーが**作成成功**ダイアログに表示されます。

   ::: warning 重要

   APIキーとシークレットキーはすぐに保存してください。シークレットキーは再表示されません。

   :::

4. **閉じる**をクリックしてダイアログを閉じます。

**権限モード**はダッシュボードでのみ利用可能です。REST API利用時は`scopes`フィールドを直接設定してください。詳細は[scopesのデフォルト動作](#default-behavior-of-scopes)を参照してください。

キーの詳細は名前をクリックして表示できます。**編集**ボタンで有効期限、状態、ロール、権限モード、スコープ、備考を変更可能です。**削除**ボタンでキーを削除できます。

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

`scopes`に`"unset"`を指定すると後方互換のためスコープ許可リストが明示的に解除されます。ロール、ネームスペース、APIキー固有のパス制限は引き続き適用されます。`scopes`を省略するとロールデフォルトのスコープが適用されます。

ネームスペースは以下のいずれかの方法で指定可能です：

- `administrator`のようなロールと`namespace`フィールドを組み合わせる
- `ns:<namespace>::<role>`形式でロールにネームスペースを含める（例：`ns:team-a::administrator`）

両形式はサポートされ続けます。両方がリクエストに含まれる場合はネームスペースが一致する必要があり、不一致や空の場合はHTTP 400を返します。APIキー作成後はREST API経由でネームスペースを変更できません。

EMQX 6.3.0以降、`multi_tenancy.deny_namespaces`にリストされたネームスペースは両形式とも使用できません。詳細は[拒否されたネームスペース名](multi-tenancy/namespace-global-settings.md#denied-namespace-names)を参照してください。

グローバルAPIキーを作成するには、`namespace`を省略し、ネームスペースプレフィックスのないロールを使用してください。`namespace`に文字列`"global"`を設定してもグローバルスコープにはなりません。

#### ブートストラップファイル

ブートストラップファイル方式でもAPIキーを作成できます。設定ファイルに以下を追加し、ファイルの場所を指定します：

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定ファイル内に複数のAPIキーを以下の形式で改行区切りで記述します：`{API Key}:{Secret Key}:{?Role}:{?Scopes}`

- **API Key**：任意の文字列でキー識別子
- **Secret Key**：ランダム文字列をシークレットキーとして使用
- **Role（任意）**：キーの[ロール](#roles-and-permissions)。ネームスペース付きキーは`ns:<namespace>::<role>`形式（例：`ns:team-a::administrator`）
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

ブートストラップエントリが以下のスコープルールに違反すると、EMQXは該当スコープを削除し、警告ログを出力した上でキーの作成・更新を続行します：

- **ログイン専用スコープ**：`user_management`、`mfa_management`、`sso_management`、`api_key_management`はAPIキーに対して無効です。EMQXはこれらを削除し、残りのスコープでキーを作成・更新します。
- **管理者相当スコープ**：APIキーに割り当て可能なスコープのうち、`system`のみが管理者相当権限を付与します。EMQX 6.0.4以降、管理者相当スコープと管理者相当でないスコープが混在する場合、管理者相当スコープをすべて削除し、残りのスコープを保持します。
- **ネームスペース付きスコープ**：EMQX 6.3.1以降、ネームスペース付きエントリがネームスペースロールが保持できないスコープを明示的に含む場合、EMQXは許可されないスコープを削除し、残りを保持します。残りのスコープがない場合、キーはスコープ保護されたビジネスAPIにアクセスできません。許可されるスコープは[ネームスペース付き呼び出し元の制限](#restrictions-for-namespaced-callers)を参照してください。

##### ブートストラップAPIキーのリロード

ブートストラップファイルから作成されたAPIキーは無期限に有効です。EMQXは起動時に毎回ファイルを処理します。既存のAPIキーがある場合はロール、ネームスペース、スコープを更新します。

EMQX 6.3.1以降、ファイル内のシークレットキーが変更されていなければ保存済みのシークレットハッシュを保持します。変更されていれば新しいハッシュを生成し、以前のシークレットキーは無効になります。

::: warning 重要

EMQX 6.2から6.3.1へのローリングアップグレード中は、すべてのノードがEMQX 6.3を実行するまでブートストラップAPIキーのシークレットキーを変更しないでください。変更されたシークレットキーは6.3のハッシュ形式で保存され、6.2実行中のノードは検証できません。

:::

### ネームスペース管理者によるAPIキー管理

EMQX 6.0.4以降、ネームスペース付きダッシュボード管理者は自身のネームスペース内のAPIキーを管理できます。管理者はベアラートークンで認証する必要があります。

| 操作 | ネームスペース管理者の挙動 |
| --- | --- |
| APIキーの作成 | 管理者のネームスペース内にのみ作成可能。ネームスペース省略、グローバル指定、他ネームスペース指定はHTTP 403。 |
| APIキー一覧取得 | 管理者のネームスペース内のキーのみ表示。グローバルキーや他ネームスペースのキーはレスポンスから除外。 |
| APIキーの読み取り・更新・削除 | 管理者のネームスペース内のキーのみ操作可能。他ネームスペースのキーはHTTP 404を返し存在を非公開。 |
| APIキーのネームスペース変更 | 他ネームスペースへの移動不可。更新はHTTP 400。 |

グローバルダッシュボード管理者は引き続き全ネームスペースのAPIキーを管理可能です。

## APIキーの権限

### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つの事前定義ロールのいずれかを割り当て可能です：

- **administrator**（管理者）：すべてのリソースにアクセス可能。指定がなければデフォルトでこのロールになります。
- **viewer**（閲覧者）：リソースやデータの閲覧のみ可能。REST APIのすべてのGETリクエストに対応。
- **publisher**（パブリッシャー）：MQTTメッセージのパブリッシュ専用。メッセージパブリッシュ関連APIへのアクセスに限定。

::: tip 注意
`publisher`キーは`publish`スコープのみ受け付けます。スコープ割り当て時に`publish`以外のスコープがあるとHTTP 400を返します。ロールを`publisher`に変更する場合は同時に`"scopes": ["publish"]`か空リストを含めてください。そうしないと既存スコープに`publish`以外がある場合リクエストは拒否されます。
:::

### APIスコープ

スコープはキーごとの権限次元であり、キーがアクセス可能なREST APIのビジネス領域を宣言します。スコープと[ロールと権限](#roles-and-permissions)は独立しており、両方が適用されることで2層のアクセス制御を形成します：

| 次元 | 目的 | 粒度 |
| --------- | ------- | ----------- |
| **ロール** | HTTP動詞の制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | APIドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストは両方のチェックを通過した場合にのみ許可されます。

マイクロサービスや統合シナリオでは、外部システムは通常EMQX管理面の一部のみアクセスします。監視プラットフォームは`monitoring`スコープのみ、ルールパブリッシュサービスは`data_integration`のみ、クラスター運用ツールは`cluster_operations`のみ必要です。スコープにより最小権限の原則でキーを割り当て、キー漏洩時の影響範囲を最小化できます。

::: tip
スコープ名は安定した識別子であり、EMQXのアップグレードで変更されません。OpenAPIタグ名が変更されても、同じスコープを持つキーは引き続き動作します。
:::

#### 組み込みのAPIキー用スコープ

EMQX 6.3.2以降、APIキー用に11のスコープを提供しています：

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
| `plugin_api` | プラグイン拡張API | `/plugin_api/{plugin}/...` |

`plugin_api`スコープはプラグインAPIゲートウェイを通じてプラグインが公開するエンドポイントへのアクセスを許可します。プラグインのインストール、起動、停止、設定エンドポイント（`/plugins*`以下）は`system`スコープに含まれ、`plugin_api`スコープではアクセスできません。ゲートウェイは`system`も受け入れるため、`system`を持つAPIキーは引き続きアクセス可能です。

`plugin_api`スコープはゲートウェイへのアクセス権を制御し、プラグインが実装する操作自体を制御するものではありません。割り当て前にプラグインが公開する各エンドポイントのセキュリティ影響を確認してください。

::: tip 注意

EMQX 6.3.2以降、`audit`スコープはネームスペース付き呼び出し元に監査ログアクセスを付与しません。`GET /api/v5/audit`を呼び出せるのはグローバル管理者およびグローバル閲覧者のみです。詳細は[監査ログアクセス](./dashboard/audit-log.md#audit-log-access)を参照してください。

:::

::: warning 管理者相当スコープと制限付きスコープを混在させないでください

EMQXは`system`、`user_management`、`api_key_management`、`sso_management`を管理者相当スコープ（検証メッセージでは`privilege scopes`）に分類しています。これらを制限付きスコープと組み合わせても実効権限は減りません。4つのうちAPIキーに割り当て可能なのは`system`のみで、他3つは[ログイン専用スコープ](#login-only-scopes)に該当します。

そのためEMQX 6.0.4以降、APIキーの作成・更新時に指定するスコープリストは`system`のみ、または`system`を含まないスコープ群のいずれかでなければなりません。混在リストはHTTP 400を返し変更は適用されません。

既存の混在スコープリストは引き続き動作し、`system`は有効です。次回の明示的なスコープ更新は`system`のみか`system`を含まないリストでなければなりません。ダッシュボードで編集時は保存前に権限モードの選択を促されます。

:::

#### ログイン専用スコープ

11のAPIキー用スコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、APIキーには割り当てられません。割り当て・適用方法は[ログインユーザースコープ](dashboard/system.md#login-user-scopes)を参照してください。

| スコープ | 必要ロール | 用途 |
| --- | --- | --- |
| `user_management` | 管理者 | ダッシュボードユーザー管理 |
| `sso_management` | 管理者 | SSOバックエンドおよびSSOユーザーレコード管理 |
| `api_key_management` | 管理者 | APIキー管理 |
| `mfa_management` | グローバル管理者またはグローバル閲覧者 | 自身の多要素認証管理。管理者は他ユーザーのMFAも管理可能。 |

#### `scopes`のデフォルト動作

EMQX 6.0.4以降、APIキーの`scopes`フィールドは以下のルールに従います：

| `scopes`の値 | 意味 |
| --- | --- |
| **作成リクエストで省略** | 選択されたロールのデフォルトを使用 |
| **更新リクエストで省略** | キーの現在のスコープ設定を保持 |
| **解除用セントネル `"unset"`** | 明示的なスコープ設定を削除。後方互換のためスコープ許可リストは適用されず、ロール・ネームスペース・APIキー固有のパス制限は適用される。 |
| **空リスト `[]`** | すべてのビジネスエンドポイントを拒否。キーの一時無効化に便利。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定したスコープのAPIのみ許可 |

ロールデフォルトと同じスコープセットを含む明示的リストは`"unset"`に正規化され、同様の動作をします。順序は無関係です。

ブートストラップファイルのエントリでスコープセグメントが省略されると、EMQXは処理時に指定されたロールのデフォルトを適用します。

スコープはキーがアクセスできるAPI領域を決定し、ロールやネームスペースの制限を上書きしません。リクエストはロール・スコープ・ネームスペースのすべてのチェックを通過した場合にのみ許可されます。

#### 利用可能なスコープ一覧の取得

EMQXは利用可能なスコープカタログを問い合わせるための2つのエンドポイントを公開しています：

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記11のビジネスドメインスコープ）を返します。APIキーで認証してください。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（4つのログイン専用スコープを含む）を返します。ベアラートークンで認証してください。

スコープ選択UIの構築や自動化スクリプトの検証に利用してください：

```bash
# APIキー用スコープ
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの入口で設定可能です：

- **ダッシュボード**：**システム** -> **APIキー**でキー作成・編集時に**権限モード**を選択。**カスタム制限付き権限**の場合に個別スコープを選択。
- **REST API**：作成・更新リクエストボディに`"scopes": ["monitoring", "cluster_operations"]`を含める。
- **ブートストラップファイル**：各行の4番目のセグメントにカンマ区切りのスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ネームスペース付き呼び出し元の制限

ネームスペース付き呼び出し元（ロールが特定のネームスペースに制限されたユーザーやAPIキー）は、スコープチェックに加えてエンドポイントレベルの追加制限を受けます。スコープ付与はこれらの制限を上書きしません。

### ネームスペース付きAPIキーのスコープ制限

EMQX 6.3.1以降、ネームスペース付きAPIキーのスコープ許可リストは`connections`、`monitoring`、`data_integration`、`access_control`、`system`、`cluster_operations`、`license`を含みます。EMQX 6.3.2以降、`plugin_api`も許可リストに含まれます。

- **デフォルトスコープ**：EMQX 6.3.2以降、作成リクエストで`scopes`を省略した場合、管理者または閲覧者ロールのネームスペース付きAPIキーは実行中のEMQXバージョンの許可リスト内のすべてのスコープを付与されます。許可リストには`publish`、`gateways`、`audit`は含まれません。
- **明示的スコープ**：ネームスペース付きAPIキーの作成・更新時に指定する明示的スコープリストは実行中のEMQXバージョンの許可リスト内に収める必要があります。そうでない場合、EMQXはHTTP 400を返し、許可されないスコープを特定して変更を適用しません。明示的リストは`system`と制限付きスコープの混在も不可です。

### 許可されないスコープを含む既存キー

保存済みスコープリストに許可されないスコープを含むキーは自動的に変更されません。読み取り-修正-書き込みクライアントとの互換性のため、更新時にスコープリストを変更せず、ロールとネームスペースも同じ場合は保存済みリストを受け入れます。例外は保存済みスコープが`publish`のみのネームスペース付きキーで、変更なしの更新でもHTTP 400を返します。これはキーがAPIにアクセスできないためです。この場合はキーを削除し、ネームスペースなしで再作成してください。ロールやスコープの実際の変更は再検証され、許可リストに準拠する必要があります。ブートストラップ処理時は許可されないスコープを削除し警告ログを出力し、残りのスコープを保持します。詳細は[ブートストラップスコープの検証](#validate-bootstrap-scopes)を参照してください。

### メッセージパブリッシュの制限

ネームスペース付きAPIキーは`POST /api/v5/publish`を含むメッセージパブリッシュAPIを呼び出せません。この制限は保存済みスコープリストに`publish`が含まれていても適用され、スコープ割り当てはネームスペースレベルの制限を上書きしません。

### メッセージコンテンツの制限

ネームスペース付き呼び出し元が`connections`または`monitoring`スコープを持っていても、保持・遅延メッセージストアを含むクラスタ全体の生のMQTTメッセージコンテンツを読み書きするエンドポイントにはアクセスできません。以下のメッセージ関連エンドポイントは`403 Forbidden`を返します：

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

ファイル転送ストアはグローバルでネームスペース非対応です。ネームスペース付き呼び出し元は以下のファイル転送コンテンツエンドポイントにアクセスできず、スコープ付与はこの制限を上書きしません：

- `GET /file_transfer/files`
- `GET /file_transfer/files/:clientid/:fileid`
- `GET /file_transfer/file`

グローバル呼び出し元はロールとスコープに応じてこれらのエンドポイントにアクセス可能です。`/file_transfer`設定エンドポイントは影響を受けません。

### トレースの制限

トレース操作について、`GET /trace`は呼び出し元のネームスペース内のトレースのみ一覧表示します。以下のトレース単位操作はトレースが別ネームスペースの場合`404 Not Found`を返します：

- `PUT /trace/:name/stop`
- `GET /trace/:name/download`
- `GET /trace/:name/log`
- `GET /trace/:name/log_detail`
- `DELETE /trace/:name`

この挙動は他ネームスペースのトレース情報漏洩を防ぎます。トレースの一括削除操作（`DELETE /trace`）はネームスペース付き呼び出し元に対して`403 Forbidden`を返し、全トレースのクリアはグローバル管理者のみ可能です。

ダッシュボードログイン、SSOコールバック、APIキー自己管理エンドポイント（例：`/api_key`）はキーの`scopes`設定に関わらずAPIキー認証を受け付けません。これはスコープモデルとは無関係のダッシュボードのセキュリティ境界です。

## ページネーション

大量データを扱う一部APIではページネーション機能を提供しています。データ特性に応じて2種類のページネーション方式があります。

### ページ番号ページネーション

ページネーション対応APIの多くは、`page`（ページ番号）と`limit`（ページサイズ）パラメータで制御可能です。最大ページサイズは`10000`です。`limit`が指定されない場合はデフォルト`100`です。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総件数を予測できないため、`meta.hasnext`で次ページの有無を示します：

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

`position`または`cursor`（開始位置）パラメータで読み込み開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置からの件数を指定します。最大ページサイズは`10000`です。`limit`未指定時はデフォルト`100`です。

例：

```bash
GET /clients/{clientid}/mqueue_messages?position=1716187698257189921_0&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれ、`meta.position`または`meta.cursor`に次ページの開始位置が示されます：

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

エラー発生時はボディにJSON形式でエラーコードが返されます：

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
| BAD_LISTENER_ID                                | 不正なリスナーID                                              |
| BAD_NODE_NAME                                  | 不正なノード名                                                |
| BAD_RPC                                        | RPC失敗。クラスター状態および要求ノード状態を確認してください。 |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成しようとしたリソースが最大または最小制限を超えています。 |
| INVALID_PARAMETER                              | リクエストパラメータが不正か境界値を超えています。   |
| CONFLICT                                       | リクエストリソースが競合しています。                                |
| NO_DEFAULT_VALUE                               | リクエストパラメータがデフォルト値を使用していません。                 |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています。                          |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージIDの解析エラー                                     |
| INVALID_ID                                     | 不正なIDスキーマ                                                |
| MESSAGE_ID_NOT_FOUND                           | メッセージIDが存在しません                                    |
| NOT_FOUND                                      | リソースが見つかりませんまたは存在しません                         |
| CLIENTID_NOT_FOUND                             | クライアントIDが見つかりませんまたは存在しません                        |
| CLIENT_NOT_FOUND                               | クライアントが見つかりません（通常はMQTTクライアントではありません） |
| RESOURCE_NOT_FOUND                             | リソースが見つかりません                                           |
| TOPIC_NOT_FOUND                                | トピックが見つかりません                                              |
| USER_NOT_FOUND                                 | ユーザーが見つかりません                                               |
| INTERNAL_ERROR                                 | サーバ内部エラー                                           |
| SERVICE_UNAVAILABLE                            | サービス利用不可                                          |
| SOURCE_ERROR                                   | ソースエラー                                                 |
| UPDATE_FAILED                                  | 更新失敗                                                 |
| REST_FAILED                                    | リセットソースまたは設定失敗                          |
| CLIENT_NOT_RESPONSE                            | クライアントが応答しません                                        |
