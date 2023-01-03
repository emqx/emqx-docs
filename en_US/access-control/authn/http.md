# Password Authentication Using HTTP

HTTP authenticator delegates authentication to a custom HTTP API.

## Authentication principle

- In authenticator settings, an HTTP request pattern is specified.
- When an MQTT client connects to EMQX, the configured request template is rendered and the resulting request is emitted.
- HTTP Status Code is used to determine whether the the authentication request is received by authentication server and successfully executed.
  - Authentication result should be returned with HTTP Status Code 200 or 204. The authentication result and whether it is a super user is indicated by the specific field value `result` and `is_superuser`.</br>
  - Other response codes will be considered as HTTP authentication request execution failure, Such as 4xx, 5xx ... </br>
    EMQX will switch to the next authenticator for the authentication process with default value `ignore`. If the current HTTP authenticator is the last authenticator on the chain, the authentication fails and the client will be refused to connect.
- The encoding format of the HTTP response must be `application/json`, and the HTTP authenticator will automatically select the decoding method according to the `Content-Type` in the response. </br>

<!--- NOTE: the code supports `application/x-www-form-urlencoded` too, but it is not very easy to extend in the future, hence hidden from doc -->

::: tip Migrating from EMQX 4.x
In EMQX 4.x, only HTTP status code is used, but body is discarded, for example, `200` for `allow` and `403` for `deny`.
Due to the lack of expressiveness, it has been redesigned to make use of HTTP body.
:::

### HTTP response example

```json
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "result": "allow" | "deny" | "ignore", // Default `"ignore"`
    "is_superuser": true | false // Default `false`
}
```

::: warning
`POST` method is recommended. When using the `GET` method, some sensitive information (like plain text passwords) can be exposed through HTTP server logging.

For untrusted environments, HTTPS should be used.
:::

## Configuration

HTTP authentication is identified with `mechanism = password_based` and `backend = http`.

HTTP `POST` and `GET` requests are supported. Each of them has some specific options.

Example of an HTTP authenticator configured with `POST` request:

```
{
    mechanism = password_based
    backend = http
    enable = true

    method = post
    url = "http://127.0.0.1:32333/auth/${peercert}?clientid=${clientid}"
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

Example of an HTTP authenticator configured with `GET` request:

```
{
    mechanism = password_based
    backend = http
    enable = true

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

### `method`

Required field with possible values `get` or `post`. Denoting the corresponding HTTP request method used.

### `url`

HTTP URL for external authentication requests, required. It may contain [placeholders](./authn.md#authentication-placeholders).

For URLs with scheme `https` the `ssl` configuration must be enabled:

```
{
    ...
    url = "https://127.0.0.1:32333/auth/${peercert}?clientid=${clientid}"
    ssl {
        enable = true
    }
}
```

### `body`

Optional arbitrary map for sending to the external API. For `post` requests it is sent as a JSON body.
For `get` requests it is encoded as query parameters.
The map keys and values can contain [placeholders](./authn.md#authentication-placeholders).

Assume an MQTT client connects with client ID `id123`, username `iamuser`, and password `secret`.

1. `GET` request:

   ```
   {
       method = get
       url = "http://127.0.0.1:32333/auth/${clientid}"
       body {
           username = "${username}"
           password = "${password}"
       }
   }
   ```

   The resulting request will be:

   ```
   GET /auth/id123?username=iamuser&password=secret HTTP/1.1
   ... Headers ...
   ```

2. `POST` JSON request:

   ```
   {
       method = post
       url = "http://127.0.0.1:32333/auth/${clientid}"
       body {
           username = "${username}"
           password = "${password}"
       }
       headers {
           "content-type": "application/json"
       }
   }
   ```

   The resulting request will be:

   ```
   POST /auth/id123 HTTP/1.1
   Content-Type: application/json
   ... Other headers ...

   {"username":"iamuser","password":"secret"}
   ```

### `headers`

Map with arbitrary HTTP headers for external requests, optional.

For `GET` requests the default value is

```
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
}
```

Headers cannot contain `content-type` header for `GET` requests.

For `POST` requests the default value is

```
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
    "content-type" = "application/json"
}
```

`content-type` header value defines `body` encoding method for `POST` requests, it must be `application/json`.

### `enable_pipelining`

A positive integer set maximum allowed async HTTP requests [HTTP pipelining](https://wikipedia.org/wiki/HTTP_pipelining).
Optional, default value is `100`, set `1` to disable.

### `connect_timeout`, `request_timeout`, `retry_interval` and `max_retries`

Optional values controlling the corresponding request thresholds. The default values are:

```
connect_timeout = 15s
max_retries = 5
request_timeout = 5s
retry_interval = 1s
```

### `pool_size`

Optional integer value defining the number of concurrent connections from an EMQX node to the external API.
The default value is 8.

### `ssl`

Standard [SSL options](../ssl.md) for connecting to the external API.
