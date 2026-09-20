# ダッシュボードのセキュリティ

このページでは、EMQXダッシュボードのセキュリティ関連機能について説明します。内容はログイン認証、パスワード管理、アカウントロック、HTTPSアクセス、およびロールベースのアクセス制御を含みます。

## 初回ログイン

新規にEMQXをインストールした場合、<http://localhost:18083/> にアクセスして、デフォルトの認証情報（ユーザー名 `admin`、パスワード `public`）でログインしてください。

初回ログイン時に、システムはデフォルト認証情報の使用を検知し、先にパスワード変更を強制します。新しいパスワードは元のものと異なる必要があり、`public` の再利用は推奨されません。

## URLによるトークンベースログイン

EMQX 5.6.0以降、ダッシュボードはURLに認証情報を埋め込むトークンベースのログインをサポートしています。これは、ユーザーが手動で認証情報を入力せずに自動的にログインできるシームレスなリダイレクトや統合シナリオに便利です。

### 利用方法

1. `/login` エンドポイントを使って認証トークンを取得します。レスポンスにはユーザー名が含まれないため、JSONペイロードに手動でユーザー名を追加してからBase64エンコードしてください。以下のコマンドは、トークン取得、ユーザー名の注入、Base64エンコードを一括で行います。

   ```bash
   curl -s -X POST "http://127.0.0.1:18083/api/v5/login" \
     -H 'accept: application/json' \
     -H 'Content-Type: application/json' \
     -d '{"username": "admin","password": "public"}' | jq '.username = "admin"' | base64
   ```

2. エンコードした文字列を `login_meta` クエリパラメータに埋め込んでログインURLを作成します。

   EMQX **5.6.0未満** の場合：

   ```bash
   http://localhost:18083?login_meta=BASE64_ENCODED_STRING
   ```

   これにより、デフォルトのクラスター概要ページにリダイレクトされます。

   EMQX **5.6.0以降** の場合：

   ```bash
   http://localhost:18083/#/dashboard/overview?login_meta=BASE64_ENCODED_STRING
   ```

   ログイン後の遷移先ページを指定できます。

トークンは安全に取り扱い、有効期限やスコープ制限を適切に設定してください。

## パスワードリセット

ダッシュボードユーザーのパスワードは `admins` CLIコマンドでリセット可能です。詳細は [CLI - admins](./cli.md#admins) を参照してください。

```bash
./bin/emqx ctl admins passwd <Username> <Password>
```

## パスワード有効期限

ダッシュボードのログインパスワードが設定された `password_expired_time` を超えて使用された場合、次回ログイン時に新しいパスワードの設定を促されます。**Administrator** ロールのユーザーは、[REST API](../guides/api.md) を通じてこの設定を更新することも可能です。

**例**：パスワード有効期限を1日に設定する場合：

```bash
curl -X 'PUT' \
  'http://admin:ppp@localhost:18083/api/v5/configs/dashboard' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"password_expired_time": "1d"}'
```

`password_expired_time` の全オプションについては [ダッシュボード設定](./configuration/dashboard.md) をご覧ください。

## アカウントロックとロック解除

ダッシュボードは、5分間に5回連続でログイン失敗が発生するとユーザーアカウントをロックします。ロックは10分間継続し、その後自動的に解除されます。

**Administrator** ロールのユーザーは、CLIを使っていつでもパスワードをリセットし、アカウントのロックを手動で解除できます。

```bash
./bin/emqx ctl admins passwd <Username> <NewPassword>
```

ロック時間や失敗回数の閾値はバックエンド設定で調整可能です。該当設定（`unsuccessful_login_max_attempts`、`unsuccessful_login_lock_duration`、`unsuccessful_login_interval`）の詳細は [ダッシュボード設定](./configuration/dashboard.md) を参照してください。

## ダッシュボードのHTTPS有効化

デフォルトではダッシュボードはHTTPのポート `18083` で待ち受けています。HTTPSでダッシュボードを提供するには、TLS証明書とキーを設定したHTTPSリスナーを構成してください。

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

HTTPリスナーを無効化し、HTTPSのみのアクセスを強制するには、HTTPのバインドポートを `0` に設定します。

```hocon
dashboard {
  listeners {
    http {
      bind = 0
    }
  }
}
```

リスナーおよびTLSオプションの全設定については [ダッシュボード設定](./configuration/dashboard.md) をご覧ください。

## ロールベースのアクセス制御

EMQX 5.3以降、ダッシュボードユーザーには2つの事前定義されたロールのいずれかが割り当てられ、操作可能な範囲を制御します。ユーザー作成時に **システム > ユーザー** ページでロールを選択できます。

| ロール | 権限 |
|---|---|
| **Administrator** | クライアント管理、システム設定、APIキー、ユーザー管理など、EMQXのすべての機能とリソースへのフルアクセス。 |
| **Viewer** | すべてのデータおよび設定に対する読み取り専用アクセス。REST APIのすべての `GET` リクエストに相当し、データの作成、変更、削除はできません。 |

::: tip
セキュリティ上の理由から、ダッシュボードユーザーはREST API認証に使用できません（EMQX 5.0.0以降）。プログラムによるアクセスには [APIキー](./api-keys.md) をご利用ください。
:::

ユーザー管理の詳細は [システム > ユーザー](./dashboard/system.md#users) を参照してください。
