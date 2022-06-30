# HTTP

HTTP Authorizer将授权的请求委托给外部HTTP服务器。

## 基本原理

* 在Authorizer的配置中，预先定义好HTTP服务器的URL以及请求的模版。
* 当一个客户端需要执行发布或者订阅操作时候，EMQX根据预先定义的模版来构造一个HTTP请求，并发送给配置的HTTP服务器。
* 通过判断服务器返回的HTTP状态码，例如200 或 204 表示授权成功（允许请求）而其他的状态码，例如403表示授权失败（拒绝请求）

::: danger
推荐使用HTTP的 `POST`方法。如果使用 `GET` 方法，一些HTTP服务器可能会把这些携带敏感信息的HTTP请求记录到日志里。
若HTTP 服务器不在内网中，推荐使用HTTPS。
:::

## 配置

HTTP授权必需使用 `type=http`的配置。

HTTP 的`POST` 和 `GET` 方法都是支持的，但是各自有不一样的配置字段。

一个使用`POST` 方法的例子如下：

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

使用 `GET` 方法的例子如下：

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

该配置为必填字段，用于指定http方法，可以是 `get` 或者 `post`。 

### `url`

发送HTTP请求的URL，可以使用如下[占位符](./authz.md#authorization-placeholders):

* `${clientid}` — 客户端的ID。
* `${username}` — 客户端登录是用的用户名。
* `${peerhost}` — 客户端的源IP地址。
* `${proto_name}` — 客户端使用的协议名称。例如 `MQTT`，`CoAP` 等。
* `${mountpoint}` — 网关监听器的挂载点（主题前缀）。
* `${action}` — 当前执行的动作请求，例如 `publish`，`subscribe`。
* `${topic}` — 当前请求想要发布或订阅的主题（或主题过滤器）

如果URL前缀是 `https://`，那么需要加上 `ssl` 相关的配置，例如：

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

该配置项可选。用于构造一个HTTP请求的body。
如果是 `post` 请求，这个配置项会被 encode 成一个 JSON 或者 `www-form-urlencoded` 的字符串。
如果是 `get` 请求，这个配置项会被翻译成 HTTP 的query-string。
这些字段的名字和值中都可以使用[占位符](./authz.md#Authorizer配置中的占位符).

根据配置项的不同 `body` 的序列化方式也可能不同。

例如，如果一个MQTT客户端使用的 clientid 是 `id123`，用户名（username）是`iamuser` 并且尝试发布消息到 `foo/bar` 主题，
那么在不同的配置下， 可能构造的HTTP请求如下：

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
    最终的HTTP请求会是下面这样：
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
    最终的HTTP请求会是下面这样：
    ```
    POST /auth/id123 HTTP/1.1
    Content-Type: application/json
    ... Other headers ...

    {"username":"iamuser","topic":"foo/bar", "action": "publish"}
    ```
* `POST` www-form-urlencoded request:
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
            "content-type": "application/x-www-form-urlencoded"
        }
    }
    ```
    最终的HTTP请求会是下面这样：
    ```
    POST /auth/id123 HTTP/1.1
    Content-Type: application/x-www-form-urlencoded
    ... Other headers ...

    username=iamuser&topic=foo%2Fbar&action=publish
    ```

### `headers`

根据配置来构造的HTTP头会是如下情况。

对于 `get` 方法，默认的HTTP报头如下
```
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
}
```
`get` 请求不得携带 `Content-Type` 的HTTP 头。

对于 `post` 请求，默认的HTTP报头如下
```
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
    "content-type" = "application/json"
}
```

`content-type` 可以为 `post` 请求指定 `body` 的序列化格式，可能的值有：
* `application/json` 序列化成JSON;
* `application/x-www-form-urlencoded` 序列化成 `x-www-form-urlencoded` 格式的字符串。

### `enable_pipelining`

一个整形数字（默认100）用于指定流水线请求的最大数量[HTTP pipelining](https://wikipedia.org/wiki/HTTP_pipelining).


### `pool_size`

Optional integer value defining the number of concurrent connections from an EMQX node to the external API.
The default value is 8.

### `ssl`

Standard [SSL options](../ssl.md) for connecting to the external API.

### 更多配置项

以下都是可选字段，

```
  connect_timeout = 15s # 连接超时
  max_retries = 5 # 最大重试次数
  request_timeout = 30s # 请求超时限制
  retry_interval = 1s # 重试中间间隔
```