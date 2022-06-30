# HTTP

HTTP authenticator delegates authentication to a custom HTTP API.

## Authentication principle

- In authenticator settings, an HTTP request pattern is specified.
- When an MQTT client connects to EMQX, the configured request template is rendered and the resulting request is emitted.
- Receiving a 200 or 204 HTTP status is interpreted as authentication success. A 4xx status code returned by the HTTP server means authentication failure and termination, and the client will be refused to connect. If other status codes are returned or other problems occur (such as request timeout, etc.), EMQX will switch to the next authenticator for the authentication process. If the current HTTP authenticator is the last authenticator on the chain, the authentication fails and the client will be refused to connect.

A successful HTTP response can also contain a boolean `is_superuser` field to indicate whether the client has superuser privileges.

The encoding format of the HTTP response can be `application/json` and `application/x-www-form-urlencoded`, and the HTTP authenticator will automatically select the decoding method according to the `Content-Type` in the response.

Example:

```
HTTP/1.1 200OK
Content-Type: application/json
...

{"is_superuser": true}
```

```
HTTP/1.1 200OK
Content-Type: application/x-www-form-urlencoded
...

is_superuser=true
```



::: danger
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

Optional arbitrary map for sending to the external API. For `post` requests it is sent as a JSON or www-form-urlencoded
body. For `get` requests it is encoded as query parameters. The map keys and values can contain [placeholders](./authn.md#authentication-placeholders).

For different configurations `body` map will be encoded differently.

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

3. `POST` www-form-urlencoded request:

   ```
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

`content-type` header value defines `body` encoding method for `POST` requests. Possible values are:
- `application/json` for JSON;
- `application/x-www-form-urlencoded` for x-www-form-urlencoded format.

### `enable_pipelining`

Boolean value indicating whether to enable [HTTP pipelining](https://wikipedia.org/wiki/HTTP_pipelining).
Optional, default value is `true`.

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
