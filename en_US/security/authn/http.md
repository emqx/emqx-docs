# HTTP

HTTP authenticator delegates authentication to a custom HTTP API.

## Authentication principle

* In authenticator settings, an HTTP request pattern is specified.
* When an MQTT client connects to the broker, the configured request template is rendered and the resulting request is emitted.
* Receiving a 200 or 204 HTTP status is interpreted as authentication success. Other statuses indicate authentication failure.
A successful HTTP response can also contain a JSON or www-form-urlencoded map with `is_superuser` boolean field
that indicates superuser privileges for the client.

::: danger
`POST` method is recommended. When using the `GET` method, some sensitive information (like plain text passwords) can be exposed through HTTP server logging.

For untrusted environments, HTTPS should be used.
:::

## Configuration

HTTP authentication is identified with `mechanism = http` and `backend = built_in_database`.

HTTP `POST` and `GET` requests are supported. Each of them has some specific options.

Example of an HTTP authenticator configured with `POST` request:

```hocon
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

```hocon
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

HTTP url for external authentication requests, required. It may cantain [placeholders](./authn.md#authentication-placeholders).

For `https://` urls `ssl` configuration must be enabled:

```hocon
{
    ...
    url = "https://127.0.0.1:32333/auth/${peercert}?clientid=${clientid}"
    ssl {
        enable = true
    }
}

```

### `body`

Optional arbitrary map for sending to the external API. For `post` requests it is sent as a JSON or www-form-urlencoded
body. For `get` requests it is encoded as query parameters. The map keys and values can contain [placeholders](./authn.md#authentication-placeholders).

For different configurations `body` map will be encoded differently.

Assume an MQTT client connects with clientid `id123`, username `iamuser`, and password `secret`.

* `GET` request:
    ```hocon
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
* `POST` JSON request:
    ```hocon
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
* `POST` www-form-urlencoded request:
    ```hocon
    {
        method = post
        url = "http://127.0.0.1:32333/auth/${clientid}"
        body {
            username = "${username}"
            password = "${password}"
        }
        headers {
            "content-type": "application/x-www-form-urlencoded"
        }
    }
    ```
    The resulting request will be:
    ```
    POST /auth/id123 HTTP/1.1
    Content-Type: application/x-www-form-urlencoded
    ... Other headers ...

    username=iamuser&password=secret
    ```

### `headers`

Map with arbitrary HTTP headers for external requests, optional.

For `get` requests the default value is
```hocon
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
}
```
Headers cannot contain `content-type` header for `get` requests.

For `post` requests the default value is
```hocon
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
    "content-type" = "application/json"
}
```

`content-type` header value defines `body` encoding method for `post` requests. Possible values are:
* `application/json` for JSON;
* `application/x-www-form-urlencoded` for x-www-form-urlencoded format.

### `enable_pipelining`

Boolean value indicating whether to enable [HTTP pipelining](https://wikipedia.org/wiki/HTTP_pipelining).
Optional, default value is `true`.

### `connect_timeout`, `request_timeout`, `retry_interval` and `max_retries`

Optional values controlling the corresponding request thresholds. The default values are:

```hocon
  connect_timeout = 15s
  max_retries = 5
  request_timeout = 5s
  retry_interval = 1s
```

### `pool_size`

Optional integer value defining the number of concurrent connections from an EMQX node to the external API.
The default value is 8.

### `ssl`

Standard [SSL options](./ssl.md) for connecting to the external API.

