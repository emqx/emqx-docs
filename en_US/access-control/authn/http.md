# Use HTTP Service

EMQX supports using an external HTTP service for password authentication. After enabling, when a client initiates a connect request, EMQX will use the received information to construct an HTTP request and determine whether to accept the request based on the query result, achieving a complex authentication logic.

::: tip Prerequisite

Knowledge about [basic EMQX authentication concepts](../authn/authn.md)

:::

## HTTP Request and Response

The authentication process is similar to an HTTP API call where EMQX, as the requesting client, constructs and initiates a request to the HTTP service in the format required by the "API", and the HTTP service returns the result as required by the "client".

- The response encoding format `content-type` must be `application/json`.
- The authentication result is marked by `result` in the body, with option value: `allow`, `deny`, `ignore`.
- Superuser is marked by `is_superuser` in the body, option value: `true`, `false`.
- Starting from EMQX v5.7.0, you can set [client attributes](../../client-attributes/client-attributes.md) using the optional `client_attrs` field. Note that both keys and values must be strings.
- Starting from EMQX v5.8.0, you can set an optional `acl` field in the response body to specify the client's permissions. See [Access Control List (ACL)](./acl.md) for more information.
- Starting from EMQX v5.8.0, you can set an optional `expire_at` field in the response body to specify the expiration time of the client's authenticity, forcing the client to disconnect and get reauthenticated at reconnection. The value is a Unix timestamp in seconds.
- The HTTP response status code `Status Code` should be `200` or `204`, the `4xx/5xx` status code returned will ignore the body and determine the result to be `ignore` and continue with the authentication chain.

Example response:

```js
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "result": "allow", // "allow" | "deny" | "ignore"
    "is_superuser": false, // options: true | false, default value: false
    "client_attrs": { // optional (since v5.7.0)
        "role": "admin",
        "sn": "10c61f1a1f47"
    }
    "expire_at": 1654254601, // optional (since v5.8.0)
    "acl": // optional (since v5.8.0)
    [
        {
            "permission": "allow",
            "action": "subscribe",
            "topic": "eq t/1/#",
            "qos": [1]
        },
        {
            "permission": "deny",
            "action": "all",
            "topic": "t/3"
        }
    ]
}
```

::: tip EMQX 4.x Compatibility Notes

In EMQX 4.x, only HTTP status code is used, but body is discarded, for example, `200` for `allow` and `403` for `deny`.
Due to the lack of expressiveness, it has been redesigned to make use of HTTP body, and thus is not compatible with EMQX 5.0.

:::

## Configure Dynamic Hostname Resolution

By default, the HTTP authenticator resolves the hostname in `url` when the authenticator is created and uses a persistent connection pool. To resolve the hostname for every authentication request, set `hostname_resolution` to `dynamic`.

Dynamic hostname resolution also allows placeholders in the host part of `url`. For example, the following configuration routes authentication requests to different endpoints according to the client's `tenant` attribute:

```hocon
{
    mechanism = password_based
    backend = http
    method = post
    url = "https://${client_attrs.tenant}.auth.example.com/authn"
    hostname_resolution = dynamic
    allowed_hosts = ["*.auth.example.com"]
    pool_size = 8
    headers {
        "Content-Type" = "application/json"
    }
    body {
        username = "${username}"
        password = "${password}"
    }
    ssl {
        enable = true
    }
}
```

When configuring dynamic hostname resolution, note the following:

- `hostname_resolution` accepts `static` or `dynamic`. The default value is `static`. You can also use `dynamic` with a literal hostname to resolve that hostname for every request.
- If the URL host contains placeholders, `hostname_resolution` must be `dynamic` and `allowed_hosts` must contain at least one entry.
- Each `allowed_hosts` entry must be an exact hostname, such as `auth.example.com`, or a wildcard pattern, such as `*.auth.example.com`. The wildcard matches hostnames under the specified suffix, but not the suffix itself. `allowed_hosts` has no effect when the URL uses a literal hostname.
- Within the URL authority, only the host can contain placeholders. The scheme must be `http` or `https`, and the port, if specified, must be a literal integer. URL userinfo and fragments are not supported. Placeholders in the URL path and query remain supported.
- If EMQX cannot render a valid hostname or the rendered hostname does not match `allowed_hosts`, EMQX does not send the HTTP request and the authentication fails.
- In `dynamic` mode, requests to all rendered hosts share a connection pool. `pool_size` limits how many idle connections the pool can keep for reuse. Set it to `0` to disable connection reuse. `enable_pipelining` and `max_inactive` do not apply in this mode.
- For HTTPS requests in `dynamic` mode, EMQX applies the configured TLS options to the rendered host. Unless Server Name Indication (SNI) is explicitly configured, EMQX derives it from the rendered hostname.
- OAuth2 is not supported when `hostname_resolution` is `dynamic`.

## Configure with Dashboard

You can use EMQX Dashboard to finish the relevant configuration.

1. In the EMQX Dashboard, click **Access Control** -> **Authentication** from the left navigation menu.

2. On the **Authentication** page, click **Create** in the top right corner.

3. Select **Password-Based** as the **Mechanism** and **HTTP Server** as the **Backend** to proceed to the **Configuration** step.

   <img src="./assets/authn-http.png" alt="HTTP" style="zoom:67%;" />

4. Follow the instructions below to configure the authentication backend:

   - **Method**: Select HTTP request method, optional values: `get`, `post`

     :::tip

     The `POST` method is recommended. When using the `GET` method, some sensitive information (such as plain text passwords) may be exposed via HTTP server logs. Also, for untrusted environments, please use HTTPS.
      :::

   - **URL**: Enter the URL address of the HTTP service. The host part can include [authentication placeholders](./authn.md#authentication-placeholders) when **Hostname Resolution** is set to `Dynamic`.
   - **Hostname Resolution**: Select `Static` to resolve a fixed hostname when creating the authenticator, or `Dynamic` to resolve the hostname for every request. The default option is `Static`. For more information, see [Configure Dynamic Hostname Resolution](#configure-dynamic-hostname-resolution).
   - **Allowed Hosts**: When the URL host contains placeholders, enter the exact hostnames or wildcard patterns that the rendered hostname is allowed to match.
   - **Precondition**: A [Variform expression](../../configuration/configuration.md#variform-expressions) used to control whether this HTTP Server authenticator should be applied to a client connection. The expression is evaluated against attributes from the client (such as `username`, `clientid`, `listener`, etc.). The authenticator will only be invoked if the expression evaluates to the string `"true"`. Otherwise, it will be skipped. For more information about the precondition, see [Authenticator Preconditions](./authn.md#authenticator-preconditions).
   - **Headers** (optional): HTTP request header. You can add several headers. Keys and values support using [placeholders](./authn.md#authentication-placeholders).
   - **OAuth2 Client Credentials**: Turn on the toggle switch to let EMQX obtain an access token and add it to requests sent to the external HTTP authentication service. For details, see [Configure OAuth2 Client Credentials](#configure-oauth2-client-credentials).
   - **Enable TLS**: Turn on the toggle switch to enable TLS for connections to the external HTTP authentication service. This setting is independent of the TLS setting for the OAuth2 token endpoint. For more information on enabling TLS, see [Network and TLS](../../network/overview.md).
   - **Body**: Request template; for `POST` requests, it is sent as a JSON in the request body; for `GET` requests, it is encoded as a Query String in the URL. Mapping keys and values support using [placeholders](./authn.md#authentication-placeholders).
   - **Advanced Settings**:
     - **Pool size** (optional): In `Static` mode, specify the persistent connection pool size. The value must be at least `1`. In `Dynamic` mode, specify the number of connections that can be reused across requests, or set it to `0` to disable connection reuse. Default: `8`.

     - **Connect Timeout** (optional): Specify the waiting period before EMQX assumes the connection is timed out. Units supported include milliseconds, second, minute, and hour.

     - **HTTP Pipelining** (optional): Input a positive integer to specify the maximum number of HTTP requests that can be sent without waiting for a response; default value: `100`. This setting does not apply when **Hostname Resolution** is set to `Dynamic`.

     - **Request Timeout** (optional): Specify the waiting period before EMQX assumes the request is timed out. Units supported include milliseconds, second, minute, and hour.

5. After you finish the settings, click **Create**.

### Configure OAuth2 Client Credentials

Starting from EMQX 6.0.4, an HTTP authenticator supports the OAuth 2.0 Client Credentials Grant. When OAuth2 is enabled, EMQX obtains, caches, and automatically refreshes an access token from the configured token endpoint. When EMQX calls the external HTTP authentication service, it sends the token in the `Authorization: Bearer <access_token>` request header so that the external service can authenticate EMQX.

Turn on **OAuth2 Client Credentials**, and then configure the following settings:

| Dashboard Setting | Description |
| --- | --- |
| **Token Endpoint** | Required. OAuth2 authorization server endpoint used to request an access token. The URL must use HTTP or HTTPS and must not contain user information. |
| **Client ID** | Required. OAuth2 client ID used to request an access token. |
| **Client Secret** | Required. OAuth2 client secret used to request an access token. |
| **Scope** | Optional OAuth2 scope requested for the access token. |
| **Token Request Timeout** | Timeout for the HTTP request to the token endpoint. The default is `5` seconds. |
| **Enable TLS** | Turn on the toggle switch to enable TLS for the token endpoint. This setting is independent of the **Enable TLS** setting for the external HTTP authentication service. |

EMQX sends a `POST` request with the `application/x-www-form-urlencoded` content type to the token endpoint. The request body contains `grant_type`, `client_id`, `client_secret`, and the optional `scope`. The token endpoint must return a `200` response with a JSON body containing an `access_token`. It can also return `token_type` and `expires_in`. If present, `token_type` must be `Bearer`, and `expires_in` must be a positive integer. For example:

```json
{
  "access_token": "eyJhbGciOi...",
  "token_type": "Bearer",
  "expires_in": 3600
}
```

::: warning Important Notice

- Do not configure an `Authorization` header for the HTTP authenticator when OAuth2 is enabled. EMQX rejects the configuration because it conflicts with the automatically generated Bearer authorization header.
- The token endpoint must accept the client ID and client secret as form fields in the request body. Authenticating to the token endpoint with an HTTP Basic `Authorization` header is not supported.

:::

## Configure with Configuration Items

You can configure the EMQX HTTP authenticator with EMQX configuration items. <!--For details, see [authn-http:post](../../configuration/configuration-manual.html#authn-http:post) and [authn-http:get](../../configuration/configuration-manual.html#authn-http:get). -->

Below are the HTTP `POST` and `GET` request examples:

:::: tabs type:card

::: tab POST request

```hcl
{
    mechanism = password_based
    backend = http

    method = post
    url = "http://127.0.0.1:8080/auth?clientid=${clientid}"
    body {
        username = "${username}"
        password = "${password}"
    }
    headers {
        "Content-Type" = "application/json"
        "X-Request-Source" = "EMQX"
    }
}
```

:::

::: tab GET request

Note: The "body" will be converted to a query string.

```hcl
{
    mechanism = password_based
    backend = http

    method = get
    url = "http://127.0.0.1:32333/auth"
    body {
        username = "${username}"
        password = "${password}"
    }
    headers {
        "X-Request-Source" = "EMQX"
    }
}
```

:::

::::

### OAuth2 Client Credentials Configuration

Starting from EMQX 6.0.4, you can enable OAuth2 Client Credentials by adding an `oauth2` block to the HTTP authenticator configuration. Place the block at the same level as `method`, `url`, `body`, and `headers`:

```hocon
oauth2 {
    enable = true
    grant_type = client_credentials
    token_endpoint = "https://auth.example.com/oauth/token"
    client_id = "emqx-client"
    client_secret = "emqx-client-secret"
    scope = "device.read device.write"
    timeout = 5s
    ssl {
        enable = true
    }
}
```

Omit `scope` if the authorization server does not require it. For the request format and restrictions, see [Configure OAuth2 Client Credentials](#configure-oauth2-client-credentials).
