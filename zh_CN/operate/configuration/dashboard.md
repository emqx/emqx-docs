# Dashboard 配置

在 EMQX 中， Dashboard 是一个基于 Web 的图形界面，用于实时管理和监控 EMQX 及连接的设备。您可以配置以下 Dashboard 配置项：

- `listeners`
- `token_expired_time`
- `cors`
- `swagger_support`
- `sso`

例如，为 EMQX Dashboard 配置 `swagger_support` 和一个监听器以接受所有传入连接。

以下为 Dashboard 配置示例：

```bash
dashboard {
  listeners {
    http {
      bind = "0.0.0.0:18083"
      max_connections = 512
    }
    https {
      # set 'bind = 0' will disable this listener
      bind = "0.0.0.0:18084"
      ssl_options {
        certfile = "${EMQX_ETC_DIR}/certs/cert.pem"
        keyfile = "${EMQX_ETC_DIR}/certs/key.pem"
      }
    }
  }
  token_expired_time = 60m
  cors = false
  swagger_support = true
  default_password = jEdOgGS6vzQ
  sso = {
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

其中，

- `bind = "0.0.0.0:18083"`

  用于设置监听器绑定的网络地址和端口号。在以上示例中，监听器将绑定到所有可用的网络接口（`0.0.0.0`）上的端口 `18083`。

- `max_connections = 512` 

  用于设置监听器将接受的最大并发连接数。在以上示例中，最大连接数设置为 `512`。

- `ssl_options.certfile`

  指向包含 PEM 格式证书的文件。文件中第一个证书必须是服务器证书，紧接着是签发这个证书的中间 CA 证书，依次罗列到根 CA 证书为止。（根 CA 证书可选）。

- `ssl_options.keyfile`

  指向包含 PEM 格式的私钥文件。

- `token_expired_time`

  JWT Token 的过期时间，等同于“浏览器会话过期时间”。用户登录后，EMQX 会生成一个 JWT Token 和一个刷新 Token。会话会在到期前自动续期。默认值为 `60m`。

- `cors`

  是否支持跨域资源共享（CORS）。如果您希望从其他域（如自定义前端）访问 Dashboard 的 API，可将此项设置为 `true`。

- `swagger_support = true`

  用于启用所有与 swagger 相关的功能，如生成 Swagger API 文档。默认情况下，其值始终为 `true`，您可以将值设置为 `false` 以禁用它。

- `default_password`

  用于为 `admin` 用户初始化数据库条目的默认密码。注意：一旦 EMQX 初次启动成功，修改这个密码将不再不起作用。初始化后，密码必须在控制台或者命令行进行修改。

- `sso`

  配置[单点登录（SSO）](../dashboard/sso.md) 选项。`ldap`、`oidc` 和 `saml` 三者中只能启用一个。如需详细的配置说明，请参阅[配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/#V-dashboard-S-dashboard-sso)中的 SSO 部分。

::: tip

EMQX 提供了更多配置项以更好地满足定制化需求。详情请参见 [EMQX 开源版配置手册](https://docs.emqx.com/zh/emqx/v@CE_VERSION@/hocon/)和 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

:::
