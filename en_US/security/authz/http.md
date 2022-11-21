# HTTP

HTTP authorizer delegates authorization to a custom HTTP API.

## Authorization principle

* In authorizer settings, an HTTP request pattern is specified.
* When an MQTT client makes a publish/subscribe request to the broker, the configured request template is rendered and the resulting request is emitted.
* Receiving a 200 or 204 HTTP status is interpreted as authorization success. Other statuses indicate authorization failure.
* Whether Pub/Sub authorization is granted by the HTTP status code or message body returned by the server.

::: warning
`POST` method is recommended. When using the `GET` method, some sensitive information can be exposed through HTTP server logging.

For untrusted environments, HTTPS should be used.
:::

## Required HTTP Response Format

- Response `Content-Type` must be `application/json`.
- If the HTTP Status Code is `200`, the authorization result is granted by HTTP Body. It depends on the value of the `result` field:
    - `allow`: Allow Publish or Subscribe.
    - `deny`: Deny Publish or Subscribe.
    - `ignore`: Ignore this request, it will be handed over to the next authorizer.
- If the HTTP Status Code is `204`, it means that this Publish or Subscribe request is allowed.

<!--- NOTE: the code supports `application/x-www-form-urlencoded` too, but it is not very easy to extend in the future, hence hidden from doc -->

## Configuration

The HTTP authorizer is identified by type `http`.

HTTP `POST` and `GET` requests are supported. Each of them has some specific options.

Example of an HTTP authorizer configured with `POST` request:

```
{
    type = http
    enable = true

    method = post
    url = "http://127.0.0.1:32333/authz/${peercert}?clientid=${clientid}"
    body {
        username = "${username}"
        topic = "${topic}"
        action = "${action}"
    }
    headers {
        "Content-Type" = "application/json"
        "X-Request-Source" = "EMQX"
    }
}
```

Example of an HTTP authorizer configured with `GET` request:

```
{
    type = http
    enable = true

    method = get
    url = "http://127.0.0.1:32333/authz"
    body {
        username = "${username}"
        topic = "${topic}"
        action = "${action}"
    }
    headers {
        "X-Request-Source" = "EMQX"
    }
}
```

### `method`

Required field with possible values `get` or `post`. Denoting the corresponding HTTP request method used.

### `url`

HTTP url for external authorization requests, required. It may contain [placeholders](./authz.md#authorization-placeholders):
* `${clientid}` — clientid of the client.
* `${username}` — username of the client.
* `${peerhost}` — client IP address.
* `${proto_name}` — Name of the protocol used by the client, such as `MQTT` and `CoAP` etc.
* `${mountpoint}` — Gateway listener's mountpoint.
* `${action}` — action that is being authorized.
* `${topic}` — topic access to which is authorized.

For `https://` urls `ssl` configuration must be enabled:

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
For `get` requests it is encoded as query parameters. The map keys and values can contain [placeholders](./authz.md#authorization-placeholders).

For different configurations `body` map will be encoded differently.

Assume an MQTT client is connected with clientid `id123`, username `iamuser` and tries to publish to `foo/bar` topic.

* `GET` request:
    ```
    {
        method = get
        url = "http://127.0.0.1:32333/auth/${clientid}"
        body {
            username = "${username}"
            topic = "${topic}"
            action = "${action}"
        }
    }
    ```
    The resulting request will be:
    ```
    GET /auth/id123?username=iamuser&topic=foo%2Fbar&action=publish HTTP/1.1
    ... Headers ...
    ```
* `POST` JSON request:
    ```
    {
        method = post
        url = "http://127.0.0.1:32333/auth/${clientid}"
        body {
            username = "${username}"
            topic = "${topic}"
            action = "${action}"
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

    {"username":"iamuser","topic":"foo/bar", "action": "publish"}
    ```
### `headers`

Map with arbitrary HTTP headers for external requests, optional.

For `get` requests the default value is
```
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
}
```
Headers cannot contain `content-type` header for `get` requests.

For `post` requests the default value is
```
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
    "content-type" = "application/json"
}
```

`content-type` header value defines `body` encoding method for `post` requests, it must be `application/json`.

### `enable_pipelining`

A positive integer set maximum allowed async HTTP requests [HTTP pipelining](https://wikipedia.org/wiki/HTTP_pipelining).
Optional, default value is `100`, set `1` to disable.

### `pool_size`

Optional integer value defining the number of concurrent connections from an EMQX node to the external API.
The default value is 8.

### `ssl`

Standard [SSL options](../ssl.md) for connecting to the external API.

### More configs

Optional values controlling the corresponding request thresholds. The default values are:

```
  connect_timeout = 15s
  max_retries = 5
  request_timeout = 30s
  retry_interval = 1s
```
