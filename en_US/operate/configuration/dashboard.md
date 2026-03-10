# Dashboard Configuration

The EMQX Dashboard is a web-based graphical interface that enables real-time management and monitoring of EMQX and its connected devices. You can configure the following settings for the Dashboard:

- `listeners`
- `token_expired_time`
- `cors`
- `swagger_support`
- `sso`

For example, you can enable the Swagger UI and configure a listener for the EMQX Dashboard to accept all incoming connections. 

Below is a sample Dashboard configuration:

```json
dashboard {
  listeners {
    http {
      # set 'bind = 0' will disable this listener
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

Where,

- `bind = "0.0.0.0:18083"`

  The IP address and port number that the listener binds to. In this example, the listener will bind to all available network interfaces (`0.0.0.0`) on port `18083`. set to port number `0` will disable this listener.

- `max_connections = 512`

  The maximum number of concurrent connections that the listener will accept. In this example, the maximum number of connections is set to `512`.

- `ssl_options.certfile`

  Path to the PEM format certificates chain file. Server certificate as the first one, followed by its immediate issuer certificate then the issuer's issuer certificate, and so on. Root CA certificate is optional. The path prefix (only prefix) can be an environment variable.

- `ssl_options.keyfile`

  Path to the PEM format private key file.

- `token_expired_time`

  JWT token expiration time. It is equivalent to "browser session expiration time". When a user logs in, EMQX generates a JWT token along with a refresh token. The session is automatically renewed before expiration. The default value is `60m`.

- `cors`

  Support Cross-Origin Resource Sharing (CORS). If you want to allow dashboard APIs to be accessed from other domains (e.g., a custom frontend), you can set this to `true`.

- `swagger_support = true`

  Enable Swagger (OpenAPI) UI available at the endpoint `/api-docs`. Set to `false` to disable.

- `default_password`

  The password used to initialize the database record for `admin` user. NOTE: Changing this config after EMQX has booted for the first time has no effect. Once initialized, the default password `public` (which comes with the installation) must be changed from the Dashboard or CLI.

- `sso`

  Configure the [Single Sign-On (SSO)](../dashboard/sso.md) options. Only one of `ldap`, `oidc`, or `smal` can be active at a time. For detailed configuration descriptions, see the SSO section in the [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/#V-dashboard-S-dashboard-sso).

::: tip

EMQX offers more configuration items to serve customized needs better. For details, see the [EMQX Open Source Configuration Manual](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) and [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::
