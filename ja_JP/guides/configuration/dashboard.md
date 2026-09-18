# ダッシュボードの設定

EMQXダッシュボードは、EMQXおよび接続されたデバイスのリアルタイム管理と監視を可能にするWebベースのグラフィカルインターフェースです。

EMQXダッシュボードの設定には多数の設定項目があります。例えば、`swagger_support`設定を有効にしてAPI仕様エンドポイントを利用可能にしたり、EMQXダッシュボードのリスナーを設定してすべての着信接続を受け入れることができます。さらに、以下の一般的な設定項目も利用可能です：

- `listeners`
- `token_expired_time`
- `password_expired_time`
- `password_login`
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
  password_login = both
  cors = false
  swagger_support = true
  default_password = jEdOgGS6vzQ
  unsuccessful_login_max_attempts = 5
  unsuccessful_login_lock_duration = 10m
  unsuccessful_login_interval = 5m
  sso = {
    # 通常、`ldap`、`oidc`、`saml`のうち一つのみが有効になります。以下は説明用の例です。
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

  リスナーがバインドするIPアドレスとポート番号です。この例では、リスナーはすべての利用可能なネットワークインターフェース（`0.0.0.0`）のポート`18083`にバインドします。ポート番号を`0`に設定すると、このリスナーは無効になります。

  EMQX 6.3.0以降では、`dashboard.listeners.http.bind`にポート番号のみ（例：`18083`）を指定した場合、EMQXは`node.default_listener_address`を使用して各ノードのアドレスを選択します。この設定がない場合、HTTPリスナーは`legacy`セキュリティプロファイルではすべてのネットワークインターフェースにバインドし、`hardened`ではループバックにバインドします。上記の例のように`bind`に明示的なIPアドレスが指定されている場合はそれが優先されます。

  このデフォルトアドレス設定はダッシュボードのHTTPSリスナーには適用されません。対応する値、再起動要件、Dockerのデフォルトについては[Default Listener Address](../access-control/security-profile.md#default-listener-address)を参照してください。

- `max_connections = 512`

  リスナーが受け入れる最大同時接続数です。この例では最大接続数は`512`に設定されています。

- `ssl_options.certfile`

  PEM形式の証明書チェーンファイルのパスです。サーバー証明書を最初に、その発行者の証明書、さらにその上位発行者の証明書と続きます。ルートCA証明書は任意です。パスのプレフィックス（先頭部分）のみ環境変数を使用可能です。

- `ssl_options.keyfile`

  PEM形式の秘密鍵ファイルのパスです。

- `token_expired_time`

  JWTトークンの有効期限です。ブラウザセッションの有効期限に相当します。ユーザーがログインすると、EMQXはJWTトークンとリフレッシュトークンを生成します。セッションは有効期限前に自動更新されます。デフォルト値は`60m`です。

- `hwmark_expire_time`

  最高ウォーターマークの有効期限の時間窓です。デフォルト値は`7d`です。有効期限切れ後、ダッシュボードは有効期限時刻から現在までの間で新しい最高ウォーターマークを検索します。

- `password_expired_time`

  ダッシュボードログインに使用するユーザーのパスワードの有効期限を設定します（例：`1h`）。この時間を過ぎると、ユーザーはダッシュボードにログインする際にパスワードの変更が必要になります。デフォルト値`0`はパスワードが期限切れにならないことを意味します。

- `password_login`

  EMQX 6.3.1以降、このオプションはローカルダッシュボードユーザーの認証方法を制御します。サポートされる値は以下の通りです：

  - `both`: SCRAM-SHA-256チャレンジレスポンスログインとパスワードベースの`POST /api/v5/login`リクエストの両方を受け入れます。これはデフォルト値であり、リクエストボディにパスワードを送信するスクリプトやクライアントとの互換性を維持します。
  - `scram_only`: SCRAM-SHA-256チャレンジレスポンスログインのみを受け入れます。このモードでは、`POST /api/v5/login`はHTTP `403`を返し、エラーコード`PASSWORD_LOGIN_DISABLED`となります。ローカルダッシュボードユーザー資格情報でベアラートークンを取得するスクリプトやサードパーティクライアントはSCRAMエンドポイントを使用する必要があります。EMQX管理REST APIのみを呼び出すプログラムはAPIキーを使用できます。

  このオプションを`scram_only`に設定する前に、ローカルダッシュボードユーザー資格情報でサインインするすべてのEMQXノードとクライアントをSCRAMをサポートするバージョンにアップグレードしてください。パスワード移行、ブラウザアクセス、ログイン方法については[ローカルダッシュボードユーザーの認証方法の設定](../dashboard-security.md#configure-authentication-methods-for-local-dashboard-users)を参照してください。

- `cors`

  クロスオリジンリソースシェアリング（CORS）をサポートします。ダッシュボードAPIを他ドメイン（例：カスタムフロントエンド）からアクセス可能にしたい場合は、これを`true`に設定します。

- `swagger_support = true`

  `/api-spec.html`、`/api-spec.md`、`/api-spec.json`、および完全なOpenAPI仕様の`/api-docs/swagger.json`を含むAPIドキュメントエンドポイントを有効にします。後方互換性のため、`/api-docs`および`/api-docs/index.html`は`/api-spec.html`にリダイレクトされます。このオプションを`false`に設定するとすべてのAPIドキュメントエンドポイントが無効になります。

  EMQX 6.3.0以降、これらのエンドポイントを通じたAPI仕様コンテンツへのアクセスには認証が必要です。サポートされる認証方法および認証なしの応答動作については[API仕様エンドポイントへのアクセス](../api.md#access-api-specification-endpoints)を参照してください。

- `default_password`

  `admin`ユーザーのデータベースレコードを初期化する際に使用されるパスワードです。注意：EMQXが初回起動後にこの設定を変更しても効果はありません。初期化後は、インストール時に付属するデフォルトパスワード`public`をダッシュボードまたはCLIから変更する必要があります。

- `unsuccessful_login_max_attempts`

  一定期間内に許容される最大のログイン失敗回数を指定します。この制限を超えると、アカウントは一時的にロックされます。デフォルト値は`5`です。

- `unsuccessful_login_duration`

  最大ログイン失敗回数に達した後、アカウントがロックされる期間（分単位）を設定します。デフォルト値は`10`分です。

- `unsuccessful_login_interval`

  ログイン失敗回数をカウントする時間窓を定義します。例えば`5`に設定すると、5分間の間に失敗したログイン回数を追跡します。デフォルト値は`5`分です。

- `sso`

  [シングルサインオン（SSO）](../dashboard/sso.md)の設定を行います。`ldap`、`oidc`、`saml`のうち一つのみが同時に有効にできます。詳細な設定説明は[設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/#V-dashboard-S-dashboard-sso)のSSOセクションを参照してください。

::: tip

EMQXはよりカスタマイズされたニーズに対応するために多くの設定項目を提供しています。詳細は[EMQX Enterprise設定マニュアル](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/)をご覧ください。

:::
