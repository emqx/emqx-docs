# HTTP

HTTP Authorizer 将授权的请求委托给外部 HTTP 服务器。

## 基本原理

* 在 Authorizer 的配置中，预先定义好 HTTP 服务器的URL以及请求的模版。
* 当一个客户端需要执行发布或者订阅操作时候，EMQX 根据预先定义的模版来构造一个 HTTP 请求，并发送给配置的 HTTP 服务器。
* 通过判断服务器返回的 HTTP 状态码或者消息体来判定是否对发布/订阅请求授权。

::: warning
推荐使用 HTTP 的 `POST `方法。如果使用 `GET` 方法，一些 HTTP 服务器可能会把这些携带敏感信息的 HTTP 请求记录到日志里。
若 HTTP 服务器不在内网中，推荐使用 HTTPS。
:::

## 应答格式

- `Content-Type` 必需是 `application/json`
- 当前 HTTP 返回状态码为 `200` 时，认证结果取决于 HTTP Body 中的 `result` 字段：
    - `allow`：允许此次发布/订阅。
    - `deny`：拒绝此次发布/订阅。
    - `ignore`：忽略本次请求，把它移交给下一个 Authorizer 处理。
- HTTP 返回状态码 `204` 表示允许此次发布/订阅请求。


## 配置

HTTP 授权必需使用 `type=http`的配置。

HTTP 的 `POST` 和 `GET` 方法都是支持的，但是各自有不一样的配置字段。

一个使用 `POST` 方法的例子如下：

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

该配置为必填字段，用于指定 http 方法，可以是 `get` 或者 `post`。 

### `url`

发送 HTTP 请求的 URL，可以使用如下[占位符](./authz.md#authorizer-配置中的占位符):

* `${clientid}` — 客户端的 ID。
* `${username}` — 客户端登录是用的用户名。
* `${peerhost}` — 客户端的源 IP 地址。
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

该配置项可选。用于构造一个 HTTP 请求的 body。
如果是 `post` 请求，这个配置项会被编码成一个 JSON。
如果是 `get` 请求，这个配置项会被翻译成 HTTP 的查询字符串。
这些字段的名字和值中都可以使用[占位符](./authz.md#authorizer-配置中的占位符).

根据配置项的不同 `body` 的序列化方式也可能不同。

例如，如果一个 MQTT 客户端使用的 clientid 是 `id123`，用户名（username）是 `iamuser` 并且尝试发布消息到 `foo/bar` 主题，
那么在不同的配置下， 可能构造的 HTTP 请求如下：

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

    最终的 HTTP 请求会是下面这样：

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

    最终的 HTTP 请求会是下面这样：

    ```
    POST /auth/id123 HTTP/1.1
    Content-Type: application/json
    ... Other headers ...

    {"username":"iamuser","topic":"foo/bar", "action": "publish"}
    ```

### `headers`

根据配置来构造的 HTTP 头会是如下情况。

对于 `get` 方法，默认的 HTTP 报头如下

```
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
}
```

`get` 请求不得携带 `Content-Type` 的 HTTP 头。

对于 `post` 请求，默认的 HTTP 报头如下
```
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
    "content-type" = "application/json"
}
```

`content-type` 可以为 `post` 请求指定 `body` 的序列化格式，必需为 `application/json`。

### `enable_pipelining`

正整数，用于指定异步 HTTP 请求管线的最大数量[HTTP pipelining](https://wikipedia.org/wiki/HTTP_pipelining)。可选，默认值为 `100`。设置为 `1` 时关闭。

### `pool_size`

可选的整型配置，用于指定 EMQX 节点到 HTTP 服务器的并发连接数，默认值为 8。

### `ssl`

用于连接到外部 HTTP 服务器的标准 [SSL 选项](../ssl.md)。

### 更多配置项

以下都是可选字段，

```
  connect_timeout = 15s # 连接超时
  max_retries = 5 # 最大重试次数
  request_timeout = 30s # 请求超时限制
  retry_interval = 1s # 重试中间间隔
```
