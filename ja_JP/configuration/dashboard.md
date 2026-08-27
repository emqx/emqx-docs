# ダッシュボード設定

EMQXダッシュボードは、EMQXおよび接続されたデバイスのリアルタイム管理と監視を可能にする、ウェブベースのグラフィカルインターフェースです。

EMQXダッシュボードの設定には多くの項目があります。例えば、`swagger_support` 設定を通じてSwagger UIを有効化したり、EMQXダッシュボードのリスナーを設定してすべての着信接続を受け入れることができます。さらに、以下の一般的な設定項目も利用可能です。

- `listeners`
- `token_expired_time`
- `password_expired_time`
- `hwmark_expire_time`
- `cors`
- `default_password`
- `unsuccessful_login_max_attempts`
- `unsuccessful_login_duration`
- `unsuccessful_login_interval`
- `sso`

以下はダッシュボード設定のサンプルです。

```json
dashboard {
  listeners {
    http {
      # 'bind = 0' に設定するとこのリスナーは無効になります
      bind = "0.0.0.0:18083"
      max_connections = 512
    }
    https {
      # 'bind = 0' に設定するとこのリスナーは無効になります
      bind = "0.0.0.0:18084"
      ssl_options {
        certfile = "${EMQX_ETC_DIR}/certs/cert.pem"
        keyfile = "${EMQX_ETC_DIR}/certs/key.pem"
      }
    }
  }
  token_expired_time = 60m
  password_expired_time = 0
  cors = false
  swagger_support = true
  default_password = jEdOgGS6vzQ
  unsuccessful_login_max_attempts = 5
  unsuccessful_login_lock_duration = 10m
  unsuccessful_login_interval = 5m
  sso = {
    # 通常、`ldap`、`oidc`、`saml` のいずれか一つのみが有効になります。以下はデモ用の設定例です。
    ldap = {
      enable = true
      backend = "ldap"
      query_timeout = "5s"
      server = "localhost:389"
      pool_size = 8
      username = "cn=admin,dc=example,dc=com"
      password = "secret"
      base_dn = "dc=example,dc=com"
      filter = "(& (objectClass=person) (uid=${username}))"
      request_timeout = "10s"
    }
    oidc = {
      enable = true
      backend = oidc
      issuer = "https://issuer.example.com"
      clientid = "your-client-id"
      secret = "your-client-secret"
      scopes = [
        "openid"
      ]
      name_var = "${sub}"
      dashboard_addr = "http://127.0.0.1:18083"
      session_expiry = "30s"
      require_pkce = false
      preferred_auth_methods = [
        "client_secret_post",
        "client_secret_basic",
        "none"
      ]
      provider = generic
      fallback_methods = [
        "RS256"
      ]
    }
    saml = {
      enable = true
      backend = "saml"
      dashboard_addr = "https://127.0.0.1:18083"
      idp_metadata_url = "https://idp.example.com"
      sp_sign_request = false
      sp_public_key = "Pub Key"
      sp_private_key = "SP Private Key"
    }
  }
}
```

各項目の説明は以下の通りです。

- `bind = "0.0.0.0:18083"`

  リスナーがバインドするIPアドレスとポート番号です。この例では、リスナーはすべての利用可能なネットワークインターフェース（`0.0.0.0`）のポート `18083` にバインドされます。ポート番号を `0` に設定すると、このリスナーは無効になります。

- `max_connections = 512`

  リスナーが同時に受け入れる最大接続数です。この例では最大接続数が `512` に設定されています。

- `ssl_options.certfile`

  PEM形式の証明書チェーンファイルのパスです。サーバー証明書が最初にあり、その後に発行者の証明書、さらにその発行者の証明書が続きます。ルートCA証明書は任意です。パスのプレフィックス（先頭部分）のみ環境変数を利用可能です。

- `ssl_options.keyfile`

  PEM形式の秘密鍵ファイルのパスです。

- `token_expired_time`

  JWTトークンの有効期限です。これは「ブラウザのセッション有効期限」と同等です。ユーザーがログインすると、EMQXはJWTトークンとリフレッシュトークンを生成します。セッションは有効期限前に自動的に更新されます。デフォルト値は `60m` です。

- `hwmark_expire_time`

  最高ウォーターマークが有効である時間のウィンドウです。デフォルト値は `7d` です。有効期限切れ後、ダッシュボードは有効期限以降から現在までの新しい最高ウォーターマークを検索します。

- `password_expired_time`

  ダッシュボードにログインするためのユーザーのパスワードの有効期限を設定します（例：`1h`）。この時間を過ぎると、ユーザーはダッシュボードにログインする際にパスワードを変更する必要があります。デフォルト値 `0` はパスワードが期限切れにならないことを意味します。

- `cors`

  クロスオリジンリソースシェアリング（CORS）をサポートします。ダッシュボードAPIを他のドメイン（例：カスタムフロントエンド）からアクセス可能にしたい場合は、これを `true` に設定します。

- `swagger_support = true`

  APIドキュメントエンドポイントを有効にします。API仕様ページ（`/api-spec.html`、`/api-spec.md`、`/api-spec.json`）およびSwagger UI（`/api-docs`、非推奨でv7で削除予定）が含まれます。すべてのAPIドキュメントエンドポイントを無効にするには `false` に設定します。

- `default_password`

  `admin` ユーザーのデータベースレコードを初期化する際に使用されるパスワードです。注意：EMQXが初回起動後にこの設定を変更しても効果はありません。初期化後は、インストール時に付属するデフォルトパスワード `public` をダッシュボードまたはCLIから変更する必要があります。

- `unsuccessful_login_max_attempts`

  一定期間内に許容される最大のログイン失敗回数を指定します。この制限を超えると、アカウントは一時的にロックされます。デフォルト値は `5` です。

- `unsuccessful_login_duration`

  ログイン失敗回数の最大値に達した後、アカウントがロックされる期間（分単位）を設定します。デフォルト値は `10` 分です。

- `unsuccessful_login_interval`

  ログイン失敗回数がカウントされる時間ウィンドウを定義します。例えば `5` に設定すると、5分間の間に失敗したログイン回数を追跡します。デフォルト値は `5` 分です。

- `sso`

  [シングルサインオン（SSO）](../dashboard/sso.md) の設定を行います。`ldap`、`oidc`、`saml` のいずれか一つのみが同時に有効になります。詳細な設定説明は、[設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/#V-dashboard-S-dashboard-sso)のSSOセクションをご参照ください。

::: tip

EMQXはよりカスタマイズされたニーズに対応するため、さらに多くの設定項目を提供しています。詳細は[EMQX Enterprise設定マニュアル（Enterprise版）](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご覧ください。

:::
