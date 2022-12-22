# 使用 HTTP 的密码认证

::: tip
先决条件：

- 了解 [EMQX 认证基本概念](../authn/authn.md)
:::

HTTP 认证使用外部自建 HTTP 应用作为数据源，根据 HTTP API 返回的数据判定认证结果，能够实现复杂的认证鉴权逻辑。

## 请求格式与返回结果

当客户端连接到 EMQX 时，HTTP 认证器会根据配置的请求模板构造并发送请求。用户需要在认证服务中实现认证逻辑并按以下要求返回结果：

- 响应编码格式 `content-type` 必须是 `application/json`
- 认证结果通过 body 中的 `result` 标示，可选 `alow`、`deny`、`ignore`
- 超级用户通过 body 中的 `is_superuser` 标示，可选 `true`、`false`
- 响应状态码 `Status Code` 应当为 `200` 或 `204`，返回 4xx/5xx 状态码时将忽略 body 并判定结果为 `ignore`，继续执行认证链

请求示例：

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

::: tip EMQX 4.x 兼容性说明
在 4.x 中，EMQX 仅用到了 HTTP API 返回的状态码，而内容则被丢弃。例如 `200` 表示 `allow`，`403` 表示 `deny`。

因为缺乏丰富的表达能力，在 5.0 中对这一机制进行了不兼容的调整。
:::

::: danger
推荐使用 `POST` 方法。 使用 `GET` 方法时，一些敏感信息（如纯文本密码）可能通过 HTTP 服务器日志记录暴露。
对于不受信任的环境，应使用 HTTPS。
:::

## 配置项

支持 HTTP `POST` 和 `GET` 请求，它们各自都有一些特定的选项。详细配置请参考 [authn-http:post](../../admin/cfg.md#authn-http:post) 与 [authn-http:get](../../admin/cfg.md#authn-http:get)。

使用 POST 请求配置的 HTTP 认证器示例：

```hocon
{
    mechanism = password_based
    backend = http
    enable = true

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

使用 GET 请求配置的 HTTP 认证器示例：

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

### method

必填，可选 `get` 和 `post`，表示使用相应的 HTTP 请求方法。

### url

用于外部认证请求的 HTTP URL，它可能包含 [placeholders](./authn.md#认证占位符)。

如果 URL 为 `https`，必须同时启用 `ssl`：

```hocon
{
    ...
    url = "https://127.0.0.1:32333/auth/${peercert}?clientid=${clientid}"
    ssl {
        enable = true
    }
}
```

### body

请求模板，对于 `post` 请求，它以 JSON 形式在请求体中发送。
对于 `get` 请求，它被编码为 URL 中的查询参数（Query String）。映射键和值可以包含 [placeholders](./authn.md#认证占位符)。

假设一个 MQTT 客户端使用客户端标识符 `emqx_c`、用户名 `emqx_u` 和密码 `public` 连接。

1. `GET` 请求配置如下：

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

最终的请求将是：

```bash
GET /auth/emqx_c?username=emqx_u&password=public HTTP/1.1
... Headers ...
```

2. `POST` JSON 格式的请求配置如下：

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

最终的请求将是：

```bash
POST /auth/id123 HTTP/1.1
Content-Type: application/json
... Other headers ...

{"username":"emqx_u","password":"public"}
```

### headers

配置 HTTP 认证请求中的 Headers，可选。

对于 `GET` 请求有以下默认 Headers：

```hocon
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
}
```

`GET` 请求的 Headers 不能包含 `Content-Type` Header。

对于 `POST` 请求有以下默认 Headers：

```hocon
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
    "content-type" = "application/json"
}
```

`content-type` Header 的值定义了 `POST` 请求的 `body` 编码方式，目前仅支持 `application/json`。

### enable_pipelining

正整数，用于指定可以无需等待响应发出的 HTTP 请求的最大数量 [HTTP pipelining](https://wikipedia.org/wiki/HTTP_pipelining)。可选，默认值为 `100`，设置为 `1` 表示关闭 HTTP pipelining 功能，即恢复成常规的同步请求响应模式。

### 请求配置

控制相应请求阈值的可选配置。默认值为：

```hocon
connect_timeout = 15s
max_retries = 5
request_timeout = 5s
retry_interval = 1s
```

### pool_size

可选的整型配置项，用于指定从 EMQX 节点到外部 HTTP Server 的并发连接数。默认值为 8。

### ssl

用于连接到外部 HTTP Server 的标准 SSL 选项。
