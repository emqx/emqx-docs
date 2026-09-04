# ダッシュボード設定

EMQXダッシュボードは、EMQXおよび接続されたデバイスのリアルタイム管理と監視を可能にする、ウェブベースのグラフィカルインターフェースです。

EMQXダッシュボードの設定には多くの項目があります。たとえば、`swagger_support`設定を通じてAPI仕様エンドポイントを有効化したり、EMQXダッシュボードのリスナーを設定してすべての着信接続を受け入れることが可能です。加えて、以下の一般的な設定項目も利用できます：

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

以下はダッシュボード設定のサンプルです：

```json
dashboard {
  listeners {
    http {
      # 'bind = 0' を設定するとこのリスナーは無効になります
      bind = "0.0.0.0:18083"
      max_connections = 512
    }
    https {
      # 'bind = 0' を設定するとこのリスナーは無効になります
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
    # 通常、`ldap`、`oidc`、または `saml` のいずれか一つのみが有効になります。以下はデモ用設定です。
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

各設定の説明は以下の通りです。

- `bind = "0.0.0.0:18083"`

  リスナーがバインドするIPアドレスとポート番号です。この例では、リスナーはすべての利用可能なネットワークインターフェース（`0.0.0.0`）のポート`18083`にバインドします。ポート番号を`0`に設定すると、このリスナーは無効になります。

  EMQX 6.3.0以降、`dashboard.listeners.http.bind`にポート番号のみ（例：`18083`）を指定した場合、EMQXは`node.default_listener_address`を使って各ノードのアドレスを選択します。この設定がない場合、HTTPリスナーは`legacy`セキュリティプロファイルでは全ネットワークインターフェースに、`hardened`ではループバックにバインドします。上記のように明示的なIPアドレスを`bind`に指定した場合はそれが優先されます。

  このデフォルトアドレス設定は、ダッシュボードのHTTPSリスナーには適用されません。対応する値、再起動要件、Dockerのデフォルトについては[Default Listener Address](../access-control/security-profile.md#default-listener-address)を参照してください。

- `max_connections = 512`

  リスナーが受け入れる最大同時接続数です。この例では最大接続数を`512`に設定しています。

- `ssl_options.certfile`

  PEM形式の証明書チェーンファイルへのパスです。サーバー証明書を最初に、その発行者証明書、さらにその上位発行者証明書と続きます。ルートCA証明書は任意です。パスのプレフィックス（先頭部分）のみ環境変数を使用可能です。

- `ssl_options.keyfile`

  PEM形式の秘密鍵ファイルへのパスです。

- `token_expired_time`

  JWTトークンの有効期限です。ブラウザのセッション有効期限に相当します。ユーザーがログインすると、EMQXはJWTトークンとリフレッシュトークンを生成し、有効期限前にセッションが自動更新されます。デフォルト値は`60m`です。

- `hwmark_expire_time`

  最高ウォーターマークの有効期限時間ウィンドウです。デフォルト値は`7d`です。有効期限切れ後、ダッシュボードは有効期限時刻から現在までの間で新しい最高ウォーターマークを検索します。

- `password_expired_time`

  ダッシュボードにログインする際のユーザーのパスワード有効期限を設定します（例：`1h`）。この時間を過ぎると、ユーザーはログイン時にパスワード変更が必要です。デフォルト値`0`はパスワードが期限切れにならないことを意味します。

- `cors`

  クロスオリジンリソースシェアリング（CORS）をサポートします。ダッシュボードAPIを他のドメイン（例：カスタムフロントエンド）からアクセス可能にしたい場合は、これを`true`に設定します。

- `swagger_support = true`

  `/api-spec.html`、`/api-spec.md`、`/api-spec.json`、および完全なOpenAPI仕様を含む`/api-docs/swagger.json`などのAPIドキュメントエンドポイントを有効にします。後方互換性のため、`/api-docs`および`/api-docs/index.html`は`/api-spec.html`にリダイレクトされます。このオプションを`false`に設定すると、すべてのAPIドキュメントエンドポイントが無効になります。

  EMQX 6.3.0以降、これらのエンドポイントを通じたAPI仕様コンテンツへのアクセスには認証が必要です。対応する認証方法や未認証時の応答動作については[Access API Specification Endpoints](../admin/api.md#access-api-specification-endpoints)を参照してください。

- `default_password`

  `admin`ユーザーのデータベースレコード初期化に使用されるパスワードです。注意：EMQXが初回起動後にこの設定を変更しても反映されません。初期化後は、インストール時に付属するデフォルトパスワード`public`をダッシュボードまたはCLIから変更する必要があります。

- `unsuccessful_login_max_attempts`

  一定期間内に許容される最大のログイン失敗回数を指定します。この回数を超えるとアカウントが一時的にロックされます。デフォルト値は`5`です。

- `unsuccessful_login_duration`

  ログイン失敗回数の上限に達した後、アカウントがロックされる期間（分単位）を設定します。デフォルト値は`10`分です。

- `unsuccessful_login_interval`

  ログイン失敗回数がカウントされる時間ウィンドウを定義します。例えば`5`に設定すると、5分間の間に発生したログイン失敗回数を追跡します。デフォルト値は`5`分です。

- `sso`

  [シングルサインオン（SSO）](../dashboard/sso.md)の設定を行います。`ldap`、`oidc`、`saml`のうち一つのみが同時に有効になります。詳細な設定説明は[設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/#V-dashboard-S-dashboard-sso)のSSOセクションを参照してください。

::: tip

EMQXはよりカスタマイズされたニーズに応えるため、さらに多くの設定項目を提供しています。詳細は[EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご覧ください。

:::
