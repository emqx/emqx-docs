# REST API

EMQXはOpenAPI（Swagger）3.0仕様に準拠したHTTP管理APIを公開しています。

EMQXを起動後、[http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html) にアクセスするとAPIドキュメントを閲覧でき、Swagger UIから管理APIを実行できます。デフォルトではダッシュボード設定の`swagger_support`が`true`に設定されており、Swagger UIが有効で、インタラクティブなAPIドキュメント生成などのSwagger関連機能がすべて有効になっています。`false`に設定するとこれらの機能を無効化できます。詳細は[ダッシュボード設定](../configuration/dashboard.md)をご参照ください。

本節ではEMQX REST APIの利用方法について説明します。

## 基本パス

EMQXのREST APIはバージョン管理されており、EMQX 5.0.0以降のすべてのAPIパスは`/api/v5`から始まります。

## HTTPヘッダー

ほとんどのAPIリクエストでは`Accept`ヘッダーに`application/json`を設定する必要があり、特に指定がない限りレスポンスはJSON形式で返されます。

## HTTPレスポンスステータスコード

EMQXは[HTTPレスポンスステータスコード](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)の標準に準拠しています。主なステータスコードは以下の通りです。

| コード | 説明                                                         |
| ------ | ------------------------------------------------------------ |
| 200    | リクエスト成功。返却されるJSONデータに詳細情報が含まれます。 |
| 201    | 作成成功。新規オブジェクトがBodyに返されます。               |
| 204    | リクエスト成功。通常は削除や更新操作で返却Bodyは空です。     |
| 400    | 不正なリクエスト。リクエストボディやパラメータのエラー。     |
| 401    | 認証エラー。APIキーの期限切れまたは存在しません。             |
| 403    | 禁止。オブジェクトが使用中または依存関係制約があります。     |
| 404    | 見つかりません。Bodyの`message`フィールドで理由を確認可能。  |
| 409    | コンフリクト。オブジェクトが既に存在するか数の上限超過。      |
| 500    | サーバ内部エラー。Bodyやログで原因を確認してください。        |

## 認証

EMQXのREST APIは主にAPIキーを用いたベーシック認証とベアラートークン認証の2種類をサポートしています。

### APIキーを用いたベーシック認証

この方法ではAPIキーとシークレットキーをそれぞれユーザー名とパスワードとして使用し、APIリクエストを認証します。EMQXのREST APIは[HTTPベーシック認証](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#the_general_http_authentication_framework)に準拠しており、これらの認証情報が必要です。利用前にAPIキーを作成してください。詳細は[APIキー管理](#apiキー管理)をご覧ください。

::: tip 注意

セキュリティ上の理由から、EMQX 5.0.0以降はダッシュボードのユーザー認証情報をREST API認証に使用できません。代わりにAPIキーを作成して認証に利用してください。

:::

#### APIキーで認証する

APIキーとシークレットキーを用意したら、APIキーをユーザー名、シークレットキーをパスワードとしてベーシック認証を行います。

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

APIキー認証の代替として、ベアラートークンを用いてEMQX REST APIに安全かつプログラム的にアクセスできます。ベアラートークンは以下のログインAPIエンドポイントにリクエストを送信して取得します。

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

レスポンスにベアラートークンが含まれ、これをAPIリクエストの認証に使用します。

#### ベアラートークンを用いた認証

取得したベアラートークンはAPIリクエストの`Authorization`ヘッダーに以下のように含めてください。

```bash
--header "Authorization: Bearer <your-token>"
```

## APIキー管理

### APIキーの作成

#### ダッシュボード

ダッシュボードの **System** -> **API Key** からAPIキーを手動で作成できます。

1. 右上の **+ Create** ボタンをクリックして作成ダイアログを開きます。
2. APIキーの詳細を設定します：
   - **Name**（必須）：APIキーの名前を入力します。
   - **Expire At**：空欄の場合は期限なしとなります。
   - **Is Enable**：デフォルトで有効です。
   - **Role**：ロールを選択（任意）。[ロールと権限](#roles-and-permissions)を参照してください。
   - **Scopes**：付与するスコープを選択（任意）。デフォルトはすべてのスコープ権限。詳細は[APIスコープ](#api-scopes)を参照してください。
   - **Note**：任意で説明を入力できます。
3. **Confirm**をクリックすると、作成成功ダイアログにAPIキーとシークレットキーが表示されます。

   ::: warning 重要

   APIキーとシークレットキーはこの時点で必ず保存してください。シークレットキーは再表示されません。

   :::

4. **Close**をクリックしてダイアログを閉じます。

キー名をクリックすると詳細を確認でき、**Edit**ボタンで有効期限、状態、説明を編集可能です。**Delete**ボタンで削除できます。

#### ブートストラップファイル

ブートストラップファイルを使ってAPIキーを作成することも可能です。以下の設定を追加してファイルの場所を指定します。

```bash
api_key = {
  bootstrap_file = "etc/default_api_key.conf"
}
```

指定したファイルに複数のAPIキーを以下の形式で改行区切りで記述します。

```
{API Key}:{Secret Key}:{?Role}:{?Scopes}
```

- **API Key**：任意の文字列でキー識別子。
- **Secret Key**：ランダムな文字列をシークレットキーとして使用。
- **Role（任意）**：キーの[ロール](#roles-and-permissions)を指定。
- **Scopes（任意）**：キーがアクセス可能な[APIスコープ](#api-scopes)をカンマ区切りで指定。省略時はすべてのユーザー可視スコープ（管理者全許可）が付与されます。ログイン専用スコープ（`user_management`、`mfa_management`、`sso_management`、`api_key_management`）はAPIキーには無効です。これらがブートストラップファイルに含まれている場合、EMQX起動時に削除され警告ログが出力されます。キーは作成されますが該当スコープは付与されません。

例：

```bash
my-app:AAA4A275-BEEC-4AF8-B70B-DAAC0341F8EB
ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL:viewer
foo:3CA92E5F-30AB-41F5-B3E6-8D7E213BE97E:publisher
integration-svc:6f1a9f2d09c84e6b:viewer:monitoring,cluster_operations
rules-mgr:2b8e4a1c9d7e4f3b:administrator:data_integration,access_control
```

この方法で作成されたAPIキーは無期限で有効です。

EMQX起動時にファイルの内容がAPIキーリストに追加され、既存キーがあればシークレットキー、ロール、スコープが更新されます。

### ロールと権限

REST APIはロールベースアクセス制御を実装しています。APIキー作成時に以下の3つのプリセットロールのいずれかを割り当てられます。

- **Administrator**：すべてのリソースにアクセス可能。指定がなければデフォルト。ロール識別子は`administrator`。
- **Viewer**：リソースやデータの閲覧のみ可能。REST APIの全GETリクエストに対応。ロール識別子は`viewer`。
- **Publisher**：MQTTメッセージのパブリッシュ専用。メッセージパブリッシュ関連APIのみアクセス可能。ロール識別子は`publisher`。

::: tip 注意
`publisher`ロールのキーは`publish`スコープのみ許容します。スコープ割り当て時に`publish`以外が含まれるとHTTP 400エラーとなります。ロールを`publisher`に変更する場合、同時リクエストで`"scopes": ["publish"]`または空リストを含めてください。既存スコープに`publish`以外があるとリクエストは拒否されます。
:::

### APIスコープ

スコープはEMQX 5.10で導入されたAPIキーごとの権限次元で、キーがアクセス可能なREST APIの業務領域を宣言します。スコープと[ロールと権限](#roles-and-permissions)は独立しており、両方のチェックを通過した場合のみアクセスが許可される2層のアクセス制御を形成します。

| 次元       | 目的                             | 粒度               |
| ---------- | -------------------------------- | ------------------ |
| **ロール** | HTTPメソッド制限（読み取り専用、書き込み、パブリッシュ専用など） | リクエストの操作種別 |
| **スコープ** | APIドメイン制限（クライアント、ルール、監視など）           | リソース領域       |

すべてのリクエストはロールチェックとスコープチェックの両方を通過する必要があります。

マイクロサービスや統合シナリオでは、外部システムがEMQX管理機能の一部のみアクセスすることが多いため、監視プラットフォームは`monitoring`スコープのみ、ルール公開サービスは`data_integration`のみ、クラスター運用ツールは`cluster_operations`のみ必要とします。スコープにより最小権限の原則でキーを割り当てられ、キー漏洩時の影響範囲を最小化できます。

#### 組み込みスコープ

EMQX 5.10には以下の10種類のスコープがあり、APIキー作成時に自由に組み合わせ可能です。

| スコープ               | 名称               | 主なAPI領域                                                                                  |
| ---------------------- | ------------------ | -------------------------------------------------------------------------------------------- |
| `connections`          | 接続管理           | `/clients`, `/subscriptions`, `/topics`, `/banned`, `/retainer`, `/file_transfer`, `/mqtt/delayed`, `/mqtt/topic_rewrite`, ... |
| `publish`              | メッセージパブリッシュ | `/publish`, `/publish/bulk`                                                                  |
| `data_integration`     | データ統合         | `/rules`, `/connectors`, `/actions`, `/schema_registry`, `/schema_validations`, `/message_transformations`, `/exhooks`, `/ai/*` |
| `access_control`       | アクセス制御       | `/authentication`, `/authorization/*`                                                        |
| `gateways`             | プロトコルゲートウェイ | `/gateways`, `/coap/*`, `/lwm2m/*`, `/gcp_devices`, ...                                       |
| `monitoring`           | 監視データ         | `/metrics`, `/stats`, `/monitor*`, `/alarms`, `/trace`, `/slow_subscriptions`, `/telemetry`, `/prometheus/{auth,stats,data_integration,...}`, ... |
| `cluster_operations`   | クラスター運用     | `/cluster*`, `/nodes`, `/load_rebalance`, `/node_eviction`, `/mt/*`, ...                      |
| `system`               | システム設定       | `/configs*`, `/listeners*`, `/plugins*`, `/ds/*`, `/data/*`, `/status`, `/relup`, `/opentelemetry*`, `/prometheus`, ... |
| `audit`                | 監査ログ           | `/audit`                                                                                    |
| `license`              | ライセンス         | `/license*`                                                                                  |

これらAPIキー用スコープに加え、ダッシュボードログインユーザーにはブラウザセッション専用の4つのログイン専用スコープがあり、APIキーには割り当てられません。詳細は[ログインユーザースコープ](../dashboard/system.md#login-user-scopes)をご覧ください。

| スコープ              | 必要ロール    | 目的                                |
| --------------------- | ------------ | ---------------------------------- |
| `user_management`     | Administrator | ダッシュボードユーザー管理          |
| `sso_management`      | Administrator | SSOバックエンドおよびSSOユーザー管理 |
| `api_key_management`  | Administrator | APIキー管理                        |
| `mfa_management`      | いずれか      | 自身のアカウントのMFA管理。管理者は他ユーザーのMFAも管理可能 |

::: tip
スコープ名はEMQXのアップグレード間で変更されない安定した識別子です。OpenAPIタグ名が変更されても、同じスコープを持つキーは引き続き動作します。
:::

::: warning `system`は管理者相当とみなす

`system`は設定管理系エンドポイント(`/configs*`, `/data/*`, `/listeners*`など)をカバーします。このスコープを持つキーは設定のサブツリーを更新したり、バックアップアーカイブからEMQXデータを復元可能です。これらの操作は通常`audit`、`access_control`、`monitoring`などの細かいスコープで保護される設定を変更するため、`system`は管理者相当とみなしてください。

`system`と制限付きスコープを同一キーに組み合わせても制限は確実に適用されません。`system`は管理者に信頼されたキーにのみ割り当て、最小権限の原則に従って必要なスコープのみ付与してください。

:::

**ネームスペース制限付き呼び出し元**（特定のネームスペースにロールが制限されたユーザーやAPIキー）は、スコープチェックに加えエンドポイントレベルの追加制限を受けます。`connections`や`monitoring`スコープが付与されていても、クラスター全体の生のMQTTメッセージコンテンツ（保持メッセージや遅延メッセージストアを含む）を読み書きするエンドポイントにはアクセスできません。以下のエンドポイントはネームスペース制限付き呼び出し元に対して常に`403 Forbidden`を返します。

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
- `DELETE /trace`（全トレース一括削除）

トレース一覧取得（`GET /trace`）では、ネームスペース制限付き呼び出し元は自身のネームスペース内のトレースのみ閲覧可能です。個別トレース操作（`PUT /trace/:name/stop`、`GET /trace/:name/download`、`GET /trace/:name/log`、`GET /trace/:name/log_detail`、`DELETE /trace/:name`）は別ネームスペースのトレースに対して`404 Not Found`を返し、クロスネームスペースのトレース存在を漏らしません。

ダッシュボードログイン、SSOコールバック、APIキー自身の管理エンドポイント（例：`/api_key`）は、キーの`scopes`設定に関わらずAPIキー認証を受け付けません。これはスコープモデルとは無関係なダッシュボードのセキュリティ境界です。

#### `scopes`のデフォルト動作

APIキーの`scopes`フィールドは以下のルールに従います。

| `scopes`の値          | 意味                                                         |
| --------------------- | ------------------------------------------------------------ |
| **未設定**（フィールドなし） | すべての業務エンドポイントが許可されます。スコープ機能導入前に作成されたキーの互換デフォルトです。 |
| **空リスト** `[]`      | すべての業務エンドポイントが拒否されます。キーの一時無効化に便利です。 |
| **明示的リスト**（例：`["monitoring", "cluster_operations"]`） | 指定されたスコープのリクエストのみ許可されます。 |

ブートストラップファイルでスコープ指定を省略すると、キーはすべてのユーザー可視スコープ（管理者全許可）で明示的に書き込まれ、アップグレード時に既存キーの権限が静かに剥奪されることを防ぎます。

同様の3状態モデルはダッシュボードログインユーザーにも適用されます。ログインユーザーの`scopes`フィールドがない場合、ロールに基づくデフォルトセットが割り当てられます。管理者は4つのログイン専用スコープを含むすべてのスコープを取得し、ビューアは10個のAPIキー用スコープすべてを取得しますが、4つのログイン専用スコープ（`mfa_management`含む）は明示的に割り当てない限り付与されません。

#### 利用可能なスコープ一覧の取得

EMQXは以下の2つのエンドポイントでスコープカタログを取得できます。

- `GET /api/v5/api_key_scopes`：APIキーに割り当て可能なスコープ（上記10個の業務ドメインスコープ）を返します。APIキー認証が必要です。
- `GET /api/v5/user_scopes`：ダッシュボードログインユーザーが利用可能なすべてのスコープ（4つのログイン専用スコープ含む）を返します。ベアラートークン認証が必要です。

スコープ選択UIの構築や自動化スクリプトの検証に利用してください。

```bash
# APIキー用スコープ取得
curl -u "$API_KEY:$API_SECRET" http://localhost:18083/api/v5/api_key_scopes

# ログインユーザースコープ取得（ベアラートークン必要）
curl -H "Authorization: Bearer $TOKEN" http://localhost:18083/api/v5/user_scopes
```

#### スコープの割り当て

スコープは以下のいずれかの方法で設定可能です。

- **ダッシュボード**：**System** -> **API Key**でキー作成・編集時に付与するスコープをチェック。
- **REST API**：作成・更新リクエストボディに`"scopes": ["monitoring", "cluster_operations"]`を含める。
- **ブートストラップファイル**：各行の4番目のセグメントにカンマ区切りでスコープを指定。例：`my-app:my-secret:administrator:monitoring,cluster_operations`

## ページネーション

大量データを扱うAPIではページネーション機能が提供されています。データ特性に応じて2種類のページネーション方式があります。

### ページ番号ページネーション

ページネーション対応APIの多くは、`page`（ページ番号）と`limit`（ページサイズ）パラメータで制御可能です。最大ページサイズは`10000`です。`limit`未指定時はデフォルト`100`となります。

例：

```bash
GET /clients?page=1&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれます。EMQXは検索条件付きリクエストの総データ数を予測できないため、`meta.hasnext`フィールドで次ページの有無を示します。

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

`position`または`cursor`（開始位置）パラメータでデータの開始位置を指定し、`limit`（ページサイズ）パラメータで開始位置からの取得件数を指定します。最大ページサイズは`10000`です。`limit`未指定時はデフォルト`100`です。

例：

```bash
GET /clients/{clientid}/mqueue_messages?position=1716187698257189921_0&limit=100
```

レスポンスの`meta`フィールドにページネーション情報が含まれ、`meta.position`または`meta.cursor`に次ページの開始位置が示されます。

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

この方式はデータ変動が激しい場合でも連続性と効率性を保ってデータ取得を可能にします。

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

| エラーコード                                   | 説明                                                         |
| --------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                         | ユーザー名またはパスワードが間違っています                   |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | ユーザー名＆パスワードまたはAPIキー＆シークレットが間違っています |
| BAD_REQUEST                                   | リクエストパラメータが不正です                               |
| NOT_MATCH                                     | 条件が一致しません                                           |
| ALREADY_EXISTS                                | リソースが既に存在します                                     |
| BAD_CONFIG_SCHEMA                             | 設定データが不正です                                         |
| BAD_LISTENER_ID                               | リスナーIDが不正です                                         |
| BAD_NODE_NAME                                 | ノード名が不正です                                           |
| BAD_RPC                                       | RPC失敗。クラスター状態および対象ノードの状態を確認してください |
| BAD_TOPIC                                     | トピック構文エラー。トピックはMQTTプロトコル標準に準拠する必要があります |
| EXCEED_LIMIT                                  | 作成リソースが最大または最小制限を超えています               |
| INVALID_PARAMETER                             | リクエストパラメータが不正または境界値を超えています         |
| CONFLICT                                      | リクエストリソースに競合があります                           |
| NO_DEFAULT_VALUE                              | リクエストパラメータにデフォルト値が使用されていません       |
| DEPENDENCY_EXISTS                             | リソースが他のリソースに依存しています                       |
| MESSAGE_ID_SCHEMA_ERROR                       | メッセージIDの解析エラー                                     |
| INVALID_ID                                    | IDスキーマが不正です                                         |
| MESSAGE_ID_NOT_FOUND                          | メッセージIDが存在しません                                   |
| NOT_FOUND                                     | リソースが見つかりません                                     |
| CLIENTID_NOT_FOUND                            | クライアントIDが見つかりません                               |
| CLIENT_NOT_FOUND                              | クライアントが見つかりません（通常はMQTTクライアントではありません） |
| RESOURCE_NOT_FOUND                            | リソースが見つかりません                                     |
| TOPIC_NOT_FOUND                               | トピックが見つかりません                                     |
| USER_NOT_FOUND                                | ユーザーが見つかりません                                     |
| INTERNAL_ERROR                                | サーバ内部エラー                                             |
| SERVICE_UNAVAILABLE                           | サービス利用不可                                             |
| SOURCE_ERROR                                  | ソースエラー                                                 |
| UPDATE_FAILED                                 | 更新失敗                                                   |
| REST_FAILED                                   | リセット元または設定の失敗                                   |
| CLIENT_NOT_RESPONSE                           | クライアントが応答しません                                   |
