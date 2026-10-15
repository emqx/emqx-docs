# ダッシュボードのセキュリティ

このページは、EMQXダッシュボードへのアクセスを設定および保護する管理者および運用担当者向けです。初回ログイン、ローカルユーザーの認証方法、トークンベースのログイン、パスワード管理、アカウントロック、HTTPS、およびロールベースのアクセス制御について説明します。

## 初回ログイン

新規にEMQXをインストールした場合、<http://localhost:18083/> でダッシュボードを開き、デフォルトの認証情報（ユーザー名 `admin`、パスワード `public`）でログインしてください。

初回ログイン後、システムはデフォルトの認証情報を使用していることを検出し、先に進む前にパスワードの変更を強制します。新しいパスワードは元のものと異なる必要があり、`public` を再度使用することは推奨されません。

## ローカルダッシュボードユーザーの認証方法の設定

EMQX 6.3.1以降、EMQXはローカルダッシュボードユーザー向けにSCRAM-SHA-256チャレンジレスポンスエンドポイントを提供しています。SCRAMとパスワードベースのログインは同じローカルユーザー認証情報を認証し、ダッシュボードのベアラートークンを発行します。SCRAMでは、クライアントはHTTPリクエストボディにパスワードを送信せずにパスワードを知っていることを証明します。

### 認証モードの選択

`dashboard.password_login` を設定して受け入れる認証方法を選択します：

- `both`：SCRAM-SHA-256とパスワードベースの `POST /api/v5/login` リクエストの両方を受け入れます。これはデフォルト値です。
- `scram_only`：SCRAM-SHA-256のみを受け入れます。パスワードベースのエンドポイントはHTTP `403` とエラーコード `PASSWORD_LOGIN_DISABLED` を返します。

### SCRAMのみモードの準備

ローリングアップグレード中は `both` のままにしてください。すべてのEMQXノードおよびローカルダッシュボードユーザー認証情報でサインインするクライアントがSCRAMをサポートしてから `scram_only` に設定してください。ローカルダッシュボードユーザー認証情報でベアラートークンを取得するスクリプトやサードパーティクライアントは、`POST /api/v5/login/challenge` と `POST /api/v5/login/verify` に移行する必要があります。EMQX管理REST APIのみを呼び出すプログラムはAPIキーを使用できます。

EMQXがサーバーログでローカルユーザーのパスワード移行が必要と報告した場合は、`scram_only` を有効にする前に[ユーザーのパスワードをリセット](#reset-password)してください。

組み込みのAPI Spec ExplorerログインページはデフォルトでSCRAMを使用します。セキュアコンテキストの要件については[ブラウザアクセス](./api.md#browser-access)を参照してください。SCRAMの完全なフローについては[SCRAM-SHA-256でベアラートークンを取得する](./api.md#obtain-a-bearer-token-with-scram-sha-256)をご覧ください。

設定の詳細は[ダッシュボードの設定](./configuration/dashboard.md)を参照してください。

## URL経由のトークンベースログイン

EMQX 5.6.0以降、ダッシュボードはURLに認証情報を埋め込むトークンベースログインをサポートしています。これは、ユーザーが手動で認証情報を入力せずに自動的にログインするシームレスなリダイレクトや統合シナリオに便利です。

トークンベースログインは既存のダッシュボードベアラートークンを使用し、別の認証情報タイプではありません。

EMQXダッシュボードは管理REST APIを使用してデータを取得し管理操作を実行します。トークンベースログイン後、ダッシュボードはこれらのAPIリクエストを認証するためにベアラートークンを使用します。

### ダッシュボードトークンの取得

`dashboard.password_login` が `both` に設定されている場合、パスワードベースの `/login` エンドポイントからトークンを取得できます。レスポンスにユーザー名が含まれないため、完全なJSONペイロードをエンコードする前に手動で追加してください。以下のコマンドはトークンをリクエストし、ユーザー名を追加し、改行なしのBase64でコンパクトJSONをエンコードし、URLで使用するためにパーセントエンコードします：

```bash
curl -s -X POST "http://127.0.0.1:18083/api/v5/login" \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"username": "admin","password": "public"}' \
  | jq -c '.username = "admin"' \
  | base64 \
  | tr -d '\n' \
  | jq -sRr @uri
```

`dashboard.password_login` が `scram_only` に設定されている場合は、[SCRAM-SHA-256でトークンを取得](./api.md#obtain-a-bearer-token-with-scram-sha-256)してください。SCRAMレスポンスにユーザー名を追加し、改行なしのBase64でエンコードし、パーセントエンコードしてからログインURLを構築します。

### ログインURLの構築

パーセントエンコードされたBase64値を `login_meta` クエリパラメータに埋め込みます。

EMQXバージョン **5.6.0未満** の場合：

```bash
http://localhost:18083?login_meta=URL_ENCODED_BASE64_STRING
```

これはデフォルトのクラスター概要ページにリダイレクトします。

EMQX **5.6.0以降** の場合：

```bash
http://localhost:18083/#/dashboard/overview?login_meta=URL_ENCODED_BASE64_STRING
```

ログイン後のターゲットページを指定できます。

トークンは安全に取り扱い、適切な有効期限とスコープ制限を設定してください。

## パスワード管理

### パスワードのリセット

`admins` CLIコマンドでダッシュボードユーザーのパスワードをリセットできます。詳細は[CLI - admins](./cli.md#admins)を参照してください。

```bash
./bin/emqx ctl admins passwd <Username> <Password>
```

### パスワードの有効期限

ダッシュボードログインパスワードが設定された `password_expired_time` より長く使用されている場合、次回ログイン時に新しいパスワードの設定を促されます。**Administrator** ロールのユーザーは[REST API](../guides/api.md)を通じてこの設定を更新できます。

**例**：パスワード有効期限を1日に設定する場合：

```bash
curl -X 'PUT' \
  'http://admin:ppp@localhost:18083/api/v5/configs/dashboard' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"password_expired_time": "1d"}'
```

`password_expired_time` の全オプションについては[ダッシュボードの設定](./configuration/dashboard.md)を参照してください。

## アカウントロックと解除

ダッシュボードは5分間に5回連続でログイン失敗があった場合、ユーザーアカウントをロックします。アカウントは10分間ロックされ、その後自動的に解除されます。

**Administrator** ロールのユーザーはCLIでユーザーのパスワードをリセットすることでいつでも手動でアカウントを解除できます：

```bash
./bin/emqx ctl admins passwd <Username> <NewPassword>
```

管理者はバックエンド設定でロック時間や失敗試行回数の閾値も調整可能です。関連設定（`unsuccessful_login_max_attempts`、`unsuccessful_login_lock_duration`、`unsuccessful_login_interval`）は[ダッシュボードの設定](./configuration/dashboard.md)を参照してください。

## ダッシュボードのHTTPS有効化

デフォルトではダッシュボードはHTTPポート `18083` で待ち受けています。HTTPSでダッシュボードを提供するには、TLS証明書とキーを用いてHTTPSリスナーを設定します：

```hocon
dashboard {
  listeners {
    https {
      bind = "0.0.0.0:18084"
      ssl_options {
        certfile = "${EMQX_ETC_DIR}/certs/cert.pem"
        keyfile  = "${EMQX_ETC_DIR}/certs/key.pem"
      }
    }
  }
}
```

HTTPリスナーを無効化しHTTPSのみのアクセスを強制するには、HTTPのバインドポートを `0` に設定します：

```hocon
dashboard {
  listeners {
    http {
      bind = 0
    }
  }
}
```

リスナーおよびTLSオプションの全設定については[ダッシュボードの設定](./configuration/dashboard.md)を参照してください。

## ロールベースアクセス制御

EMQX 5.3以降、ダッシュボードユーザーには2つの事前定義されたロールのいずれかが割り当てられ、操作可能な範囲を制御します。ユーザー作成時に**システム > ユーザー**ページでロールを選択できます。

| ロール | 権限 |
|---|---|
| **Administrator** | クライアント管理、システム設定、APIキー、ユーザー管理を含むEMQXの全機能とリソースへのフルアクセス。 |
| **Viewer** | REST APIのすべての `GET` リクエストに対応する、すべてのデータと設定への読み取り専用アクセス。データの作成、変更、削除はできません。 |

::: tip
ダッシュボードのユーザー名とパスワードはREST APIリクエストのBasic認証資格情報として直接使用できません。プログラムによるアクセスには[APIキー](./api-keys.md)を使用するか、ダッシュボードのログインフローで短期間有効なベアラートークンを取得してください。長時間稼働するサービスや無人の自動化にはAPIキーを推奨します。
:::

ユーザー管理の詳細は[システム > ユーザー](./dashboard/system.md#users)を参照してください。
