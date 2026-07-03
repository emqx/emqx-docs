# REST API

EMQXは、OpenAPI（Swagger）3.0仕様に準拠したHTTP管理APIを公開しています。

EMQXでは、REST APIを探索・操作するための複数の方法を提供しています。EMQX起動後、以下のAPI仕様エンドポイントが利用可能です。

| エンドポイント | フォーマット | 説明 |
| --- | --- | --- |
| `/api-spec.html` | HTML | 人間が読みやすいドリルダウン形式のAPIリファレンスページ。 |
| `/api-spec.md` | Markdown | Markdown形式のAPIリファレンス。AIエージェントや自動化ツール向け。 |
| `/api-spec.json` | JSON | JSON形式のOpenAPI 3.0仕様。スクリプトやプログラム的なツール向け。 |
| `/api-docs/index.html` | HTML | ブラウザ上でAPIコールを直接テスト可能なインタラクティブなSwagger UI。**非推奨**：v7で削除予定。 |

上記のすべてのエンドポイントを利用するには、ダッシュボード設定で `swagger_support` を `true`（デフォルト）に設定する必要があります。APIドキュメントエンドポイントを無効化するには `false` に設定してください。詳細は[ダッシュボード設定](../configuration/dashboard.md)をご参照ください。

本節では、EMQX REST APIの利用方法を紹介します。

## 基本パス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降のすべてのAPIパスは `/api/v5` で始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは、`Accept` ヘッダーを `application/json` に設定する必要があり、特に指定がない限りレスポンスはJSON形式で返されます。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)標準に準拠しています。主なステータスコードは以下の通りです。

| コード | 説明 |
| ----- | ------------------------------------------------------------ |
| 200   | リクエスト成功。返却されるJSONデータに詳細が含まれます。 |
| 201   | 作成成功。新規オブジェクトがボディに返されます。 |
| 204   | リクエスト成功。通常は削除や更新操作で使用され、返却ボディは空です。 |
| 400   | 不正なリクエスト。リクエストボディやパラメータのエラー。 |
| 401   | 認証失敗。APIキーが期限切れか存在しません。 |
| 403   | 禁止。オブジェクトが使用中か依存関係の制約があります。 |
| 404   | 未検出。ボディの `message` フィールドで理由を確認できます。 |
| 409   | コンフリクト。オブジェクトが既に存在するか、数の上限を超えています。 |
| 500   | サーバ内部エラー。ボディやログで原因を確認してください。 |

## 認証

EMQXのREST APIは主に2つの認証方法をサポートしています：APIキーを用いたベーシック認証とベアラートークン認証です。

### APIキーを用いたベーシック認証

この方法では、APIキーとシークレットキーをそれぞれユーザー名とパスワードとして使用し、APIリクエストを認証します。EMQXのREST APIは[HTTPベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。EMQX REST APIを利用する前にAPIキーを作成してください。詳細は[APIキー管理](#apiキー管理)をご覧ください。

::: tip 注意事項

セキュリティ上の理由から、EMQX 5.0.0以降はダッシュボードのユーザー認証情報でREST APIリクエストを認証できません。代わりにAPIキーを作成し、認証に使用してください。

:::

#### APIキーでの認証例

APIキーとシークレットキーを取得したら、APIキーをユーザー名、シークレットキーをパスワードとしてベーシック認証に使用します。

各言語の例：

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

ベアラートークンを取得するには、以下のログインAPIエンドポイントにHTTP `POST` リクエストを送信します。

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

レスポンスにベアラートークンが含まれます。これをAPIリクエストの認証に使用します。

#### ベアラートークンを使った認証

ベアラートークンを取得したら、APIリクエストの `Authorization` ヘッダーに以下のように含めてください。

```bash
--header "Authorization: Bearer <your-token>"
```

## APIキー管理

### APIキーの作成

#### ダッシュボード

ダッシュボードの **System** -> **API Key** から手動でAPIキーを作成できます。

1. 右上の **+ Create** ボタンをクリックして作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **Name**（必須）：APIキーの名前を入力します。
   - **Expire At**：空欄の場合は期限なしとなります。
   - **Is Enable**：デフォルトで有効です。
   - **Role**：ロールを選択（任意）。詳細は[ロールと権限](#ロールと権限)を参照してください。
   - **Scopes**：付与するスコープを選択（任意）。デフォルトはすべてのスコープ権限です。[APIスコープ](#apiスコープ)を参照してください。
   - **Note**：キーの説明を任意で入力できます。
3. **Confirm** をクリックすると、APIキーとシークレットキーが **作成成功** ダイアログに表示されます。

   ::: warning 重要

   APIキーとシークレットキーはこの時点で必ず保存してください。シークレットキーは再表示されません。

   :::

4. **Close** をクリックしてダイアログを閉じます。

キー名をクリックすると詳細を確認でき、**Edit** ボタンで有効期限、状態、説明を編集、**Delete** ボタンで削除できます。

#### ブートストラップファイル

ブートストラップファイルを用いてAPIキーを作成することも可能です。以下の設定ファイルでファイルの場所を指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定したファイル内に複数のAPIキーを以下の形式で改行区切りで記述します。

```
{API Key}:{Secret Key}:{?Role}:{?Scopes}
```

- **API Key**：キー識別子として任意の文字列。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの[ロール](#ロールと権限)を指定。
- **Scopes（任意）**：キーがアクセス可能な[APIスコープ](#apiスコープ)をカンマ区切りで指定。省略時はすべてのユーザー可視スコープが付与されます（管理者権限の全許可、旧バージョンとの互換性のため）。ログイン専用スコープ（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）はAPIキーには無効です。これらがブートストラップファイルに含まれる場合、EMQX起動時に削除され警告ログが出力されます。キーはスコープなしで作成されます。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

この方法で作成されたAPIキーは無期限で有効です。

EMQX起動時にファイル内のデータがAPIキーリストに追加されます。既存のAPIキーがあれば、シークレットキー、ロール、スコープが更新されます。

### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つのプリセットロールのいずれかを割り当てられます。

- **Administrator**：すべてのリソースにアクセス可能で、ロール未指定時のデフォルトです。ロール識別子は `administrator` です。
- **Viewer**：リソースとデータの閲覧のみ可能で、REST APIのすべてのGETリクエストに対応します。ロール識別子は `viewer` です。
- **Publisher**：MQTTメッセージのパブリッシュ専用に設計されており、メッセージパブリッシュ関連APIへのアクセスに限定されます。ロール識別子は `publisher` です。

::: tip 注意
`publisher` ロールのキーは `publish` スコープのみ受け入れます。スコープ割り当て時に `publish` 以外のスコープがあるとHTTP 400が返されます。キーのロールを `publisher` に変更する場合は、同時リクエストで `"scopes": ["publish"]` または空リストを含めてください。そうしないと既存スコープに `publish` 以外がある場合リクエストは拒否されます。
:::

### APIスコープ

スコープはEMQX 5.10で導入されたAPIキーごとの権限次元で、キーがアクセス可能なREST APIの業務領域を宣言します。スコープと[ロールと権限](#ロールと権限)は独立しており、両方が適用されることで2層のアクセス制御を形成します。

| 次元 | 目的 | 粒度 |
| --------- | ------- | ----------- |
| **ロール** | HTTP動詞の制限（読み取り専用、書き込み専用、パブリッシュ専用など） | リクエストアクション |
| **スコープ** | APIドメインの制限（クライアント、ルール、監視など） | リソース領域 |

すべてのリクエストはロールチェックとスコープチェックの両方を通過した場合にのみ許可されます。

マイクロサービスや統合シナリオでは、外部システムがEMQX管理面の一部のみアクセスすることが多いです。監視プラットフォームは `monitoring` スコープのみ、ルールパブリッシュサービスは `data_integration` のみ、クラスター運用ツールは `cluster_operations` のみ必要です。スコープにより最小権限の原則を適用し、キー漏洩時の影響範囲を最小化できます。

#### 組み込みスコープ

EMQX 5.10には以下の10個のスコープがあり、APIキー作成時に自由に組み合わせ可能です。

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

これらAPIキー用スコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、APIキーには割り当てられません。ログインユーザーのスコープ割り当てと適用については[ログインユーザースコープ](../dashboard/system.md#login-user-scopes)を参照してください。

| スコープ | 必要ロール | 目的 |
| --- | --- | --- |
| `user_management` | Administrator | ダッシュボードユーザー管理。 |
| `sso_management` | Administrator | SSOバックエンドおよびSSOユーザー管理。 |
| `api_key_management` | Administrator | APIキー管理。 |
| `mfa_management` | 任意 | 自身のMFA管理。管理者は他ユーザーのMFAも管理可能。 |

::: tip
スコープ名はEMQXのアップグレード間で変更されない安定した識別子です。OpenAPIタグ名が変更されても、同じスコープを持つキーは引き続き機能します。
:::

::: warning `system` は管理者相当とみなす

`system` は設定管理エンドポイント（`/configs*`, `/data/*`, `/listeners*` など）をカバーします。このスコープを持つキーは任意の設定サブツリーを更新したり、バックアップアーカイブからEMQXデータを復元したりできます。これらは通常 `audit`、`access_control`、`monitoring` などの細かいスコープで保護される設定を変更可能です。

同一キーに `system` と制限付きスコープリストを組み合わせても制限は確実に適用されません。`system` は管理者権限を持つキーにのみ付与し、最小権限の原則に従って必要なスコープのみを付与してください。

:::

**ネームスペース制限された呼び出し元**（特定のネームスペースにロールが制限されたユーザーまたはAPIキー）は、スコープチェックに加えエンドポイントレベルの追加制限を受けます。`connections` や `monitoring` スコープが付与されていても、クラスター全体の生のMQTTメッセージ内容（保持メッセージや遅延メッセージストアを含む）を読み書きするエンドポイントにはアクセスできません。以下のエンドポイントはネームスペース制限された呼び出し元に対し、割り当てられたスコープに関わらず `403 Forbidden` を返します。

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
- `DELETE /trace`（すべてのトレース一括削除）

トレース一覧取得（`GET /trace`）では、ネームスペース制限された呼び出し元は自身のネームスペース内のトレースのみ閲覧可能です。個別トレース操作（`PUT /trace/:name/stop`、`GET /trace/:name/download`、`GET /trace/:name/log`、`GET /trace/:name/log_detail`、`DELETE /trace/:name`）は、他ネームスペースのトレースの場合 `404 Not Found` を返し、クロスネームスペースのトレース存在を漏らしません。

ダッシュボードログイン、SSOコールバック、APIキー自己管理エンドポイント（例：`/api_key`）は、キーの `scopes` 設定に関わらずAPIキー認証を受け付けません。これはスコープモデルとは無関係のダッシュボードのセキュリティ境界です。

#### `scopes` のデフォルト動作

APIキーの `scopes` フィールドは以下のルールに従います。

| `scopes` の値 | 意味 |
| --- | --- |
| **未設定**（フィールドなし） | すべての業務エンドポイントが許可されます。スコープ機能導入前に作成されたキーの互換性のためのデフォルト。 |
| **空リスト** `[]` | すべての業務エンドポイントが拒否されます。キーを削除せずにソフト無効化したい場合に有用。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定されたスコープ内のリクエストのみ許可されます。 |

ブートストラップファイルのエントリでスコープ指定が省略された場合、キーは明示的にすべてのユーザー可視スコープ（管理者全許可）で書き込まれ、既存のブートストラップキーの権限がアップグレードで不意に削除されることを防ぎます。

同様の3状態モデルがダッシュボードログインユーザーにも適用されます。ログインユーザーの `scopes` フィールドが未設定の場合、ロールに基づくデフォルトセットが割り当てられます。管理者は4つのログイン専用スコープを含むすべてのスコープを取得し、ビューアは10個のAPIキー用スコープすべてを取得しますが、4つのログイン専用スコープ（`mfa_management`含む）は明示的に割り当てない限り取得しません。

#### 利用可能なスコープ一覧の取得

EMQXは利用可能なスコープカタログを問い合わせるために2つのエンドポイントを公開しています。

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記10個の業務ドメインスコープ）を返します。APIキー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーに割り当て可能なすべてのスコープ（4つのログイン専用スコープを含む）を返します。ベアラートークン認証が必要です。

スコープ選択UIの構築や自動化スクリプトの検証に利用してください。

```bash
# APIキー用スコープ取得
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ取得（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの方法で設定可能です。

- **ダッシュボード**：**System** -> **API Key** でキー作成・編集時に付与するスコープを選択。
- **REST API**：作成・更新リクエストボディに `"scopes": ["monitoring", "cluster_operations"]` を含める。
- **ブートストラップファイル**：各行の4番目のセグメントとしてカンマ区切りのスコープリストを指定（例：`my-app:my-secret:administrator:monitoring,cluster_operations`）。

## ページネーション

大量データを扱う一部APIではページネーション機能が提供されています。データ特性に応じて2種類のページネーション方式があります。

### ページ番号によるページネーション

ページネーション対応APIの多くでは、`page`（ページ番号）と `limit`（ページサイズ）パラメータでページングを制御します。最大ページサイズは `10000` です。`limit` 指定がない場合はデフォルトで `100` となります。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの `meta` フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総件数を予測できないため、`meta.hasnext` フィールドで次ページの有無を示します。

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

データが急速に変化し、ページ番号ページネーションが非効率な一部APIではカーソルページネーションを使用します。

`position` または `cursor`（開始位置）パラメータでデータの開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置からの件数を指定します。最大ページサイズは `10000` です。`limit` 指定がない場合はデフォルトで `100` です。

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

この方式はデータが急速に変化するシナリオで効率的かつ連続的なデータ取得を実現します。

## エラーコード

HTTPレスポンスステータスコードに加え、EMQXは特定のエラーを識別するためのエラーコード一覧を定義しています。

エラー発生時は、ボディにJSON形式でエラーコードが返されます。

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
| ALREADY_EXISTS                                 | リソースが既に存在します。                                      |
| BAD_CONFIG_SCHEMA                              | 設定データが不正です。                                       |
| BAD_LISTENER_ID                                | リスナーIDが不正です。                                       |
| BAD_NODE_NAME                                  | ノード名が不正です。                                         |
| BAD_RPC                                        | RPC失敗。クラスター状態と対象ノードの状態を確認してください。      |
| BAD_TOPIC                                      | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります。 |
| EXCEED_LIMIT                                   | 作成しようとするリソースが最大または最小制限を超えています。          |
| INVALID_PARAMETER                              | リクエストパラメータが不正または境界値を超えています。               |
| CONFLICT                                       | リクエストリソースに競合があります。                              |
| NO_DEFAULT_VALUE                               | リクエストパラメータにデフォルト値が使用されていません。               |
| DEPENDENCY_EXISTS                              | リソースが他のリソースに依存しています。                          |
| MESSAGE_ID_SCHEMA_ERROR                        | メッセージIDの解析エラー。                                     |
| INVALID_ID                                     | IDスキーマが不正です。                                       |
| MESSAGE_ID_NOT_FOUND                           | メッセージIDが存在しません。                                   |
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
| REST_FAILED                                    | ソースまたは設定のリセットに失敗しました。                       |
| CLIENT_NOT_RESPONSE                            | クライアントが応答していません。                                |
