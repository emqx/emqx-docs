# Dashboard Security

This page is intended for administrators and operators who configure and secure access to EMQX Dashboard. It covers first login, local-user authentication methods, token-based login, password management, account lockout, HTTPS, and role-based access control.

## First Login

For a fresh EMQX installation, open the Dashboard at <http://localhost:18083/> and log in with the default credentials: username `admin`, password `public`.

After the first login, the system detects that you are using the default credentials and forces a password change before you can proceed. The new password must differ from the original, and using `public` again is not recommended.

## Configure Authentication Methods for Local Dashboard Users

Starting from EMQX 6.3.1, EMQX provides SCRAM-SHA-256 challenge-response endpoints for local Dashboard users. SCRAM and password-based login authenticate the same local user credentials and issue Dashboard bearer tokens. With SCRAM, a client proves that it knows the password without sending the password in an HTTP request body.

### Choose an Authentication Mode

Set `dashboard.password_login` to select the accepted authentication methods:

- `both`: Accept SCRAM-SHA-256 and the password-based `POST /api/v5/login` request. This is the default value.
- `scram_only`: Accept only SCRAM-SHA-256. The password-based endpoint returns HTTP `403` with the error code `PASSWORD_LOGIN_DISABLED`.

### Prepare for SCRAM-Only Mode

Keep `both` during a rolling upgrade. Set `scram_only` only after all EMQX nodes and clients that sign in with local Dashboard user credentials support SCRAM. Scripts and third-party clients that obtain bearer tokens with local Dashboard user credentials must migrate to `POST /api/v5/login/challenge` and `POST /api/v5/login/verify`. Programs that only call the EMQX management REST API can use API keys instead.

If EMQX reports in the server logs that a local user's password must be migrated, [reset the user's password](#reset-password) before enabling `scram_only`.

The embedded API Spec Explorer login page uses SCRAM by default. For its secure-context requirements, see [Browser Access](./api.md#browser-access). For the complete SCRAM flow, see [Obtain a Bearer Token with SCRAM-SHA-256](./api.md#obtain-a-bearer-token-with-scram-sha-256).

For configuration details, see [Dashboard Configuration](./configuration/dashboard.md).

## Token-Based Login via URL

Starting from EMQX 5.6.0, the Dashboard supports token-based login by embedding authentication information in the URL. This is useful for seamless redirection and integration scenarios where a user should be logged in automatically without manually entering credentials.

Token-based login uses an existing Dashboard bearer token; it is not a separate credential type.

EMQX Dashboard uses the management REST API to retrieve data and perform administrative operations. After token-based login, Dashboard uses the bearer token to authenticate these API requests.

### Obtain a Dashboard Token

When `dashboard.password_login` is set to `both`, you can obtain a token from the password-based `/login` endpoint. Because the response does not include the username, add it manually before encoding the full JSON payload. The following command requests the token, adds the username, and Base64-encodes the result:

```bash
curl -s -X POST "http://127.0.0.1:18083/api/v5/login" \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"username": "admin","password": "public"}' | jq '.username = "admin"' | base64
```

If `dashboard.password_login` is set to `scram_only`, [obtain the token with SCRAM-SHA-256](./api.md#obtain-a-bearer-token-with-scram-sha-256). Add the username to the SCRAM response and Base64-encode the resulting JSON object before constructing the login URL.

### Construct the Login URL

Embed the Base64-encoded login information in the `login_meta` query parameter.

For EMQX versions **before 5.6.0**:

```bash
http://localhost:18083?login_meta=BASE64_ENCODED_STRING
```

This redirects to the default cluster overview page.

For EMQX **5.6.0 and later**:

```bash
http://localhost:18083/#/dashboard/overview?login_meta=BASE64_ENCODED_STRING
```

This allows specifying the target page after login.

Handle the token securely and set appropriate expiration and scope limits.

## Manage Passwords

### Reset Password

You can reset a Dashboard user's password with the `admins` CLI command. For details, see [CLI - admins](./cli.md#admins).

```bash
./bin/emqx ctl admins passwd <Username> <Password>
```

### Password Expiration

When a Dashboard login password has been in use longer than the configured `password_expired_time`, the user is prompted to set a new password at the next login. Users with the **Administrator** role can also update this setting via the [REST API](../guides/api.md).

**Example**: set the password expiration time to 1 day:

```bash
curl -X 'PUT' \
  'http://admin:ppp@localhost:18083/api/v5/configs/dashboard' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"password_expired_time": "1d"}'
```

For the full list of `password_expired_time` options, see [Dashboard Configuration](./configuration/dashboard.md).

## Account Lockout and Unlock

The Dashboard locks a user account after 5 consecutive failed login attempts within a 5-minute window. The account remains locked for 10 minutes before it is automatically unlocked.

Users with the **Administrator** role can manually unlock an account at any time by resetting the user's password via the CLI:

```bash
./bin/emqx ctl admins passwd <Username> <NewPassword>
```

Administrators can also adjust the lockout duration and the failed-attempt threshold through the backend configuration. Refer to [Dashboard Configuration](./configuration/dashboard.md) for the relevant settings (`unsuccessful_login_max_attempts`, `unsuccessful_login_lock_duration`, `unsuccessful_login_interval`).

## Enable HTTPS for Dashboard

By default, the Dashboard listens on HTTP port `18083`. To serve the Dashboard over HTTPS, configure an HTTPS listener with a TLS certificate and key:

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

To disable the HTTP listener and enforce HTTPS-only access, set the HTTP bind port to `0`:

```hocon
dashboard {
  listeners {
    http {
      bind = 0
    }
  }
}
```

For the full set of listener and TLS options, see [Dashboard Configuration](./configuration/dashboard.md).

## Role-Based Access Control

Starting from EMQX 5.3, Dashboard users are assigned one of two predefined roles that control what they can do. You can select a role when creating a user on the **System > Users** page.

| Role | Permissions |
|---|---|
| **Administrator** | Full access to all EMQX features and resources, including client management, system configuration, API keys, and user management. |
| **Viewer** | Read-only access to all data and configurations, corresponding to all `GET` requests in the REST API. Cannot create, modify, or delete any data. |

::: tip
Dashboard usernames and passwords cannot be used directly as Basic authentication credentials for REST API requests. For programmatic access, use [API keys](./api-keys.md) or obtain a short-lived bearer token through a Dashboard login flow. Use API keys for long-running services and unattended automation.
:::

For details on managing users, see [System > Users](./dashboard/system.md#users).
