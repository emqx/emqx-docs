# 使用 HTTP 的密码认证

HTTP 认证使用外部自建 HTTP 应用认证数据源，根据 HTTP API 返回的数据判定认证结果，能够实现复杂的认证鉴权逻辑。

## 认证原理

- 在认证器的设置中，指定 HTTP 请求模式。
- 当 MQTT 客户端连接到 EMQX 时，HTTP 认证器会根据配置的请求模板渲染并发送生成的请求。
- HTTP **响应状态码** (HTTP Status Code) 被用于判断认证请求是否被认证服务器接收执行。</br>
  - 认证结果应通过 Status Code 200 或 204 进行返回。</br>
    认证结果、是否为超级用户将分别由 Response Body 内的 `result` 和 `is_superuser` 字段值指示。</br>
  - 其他响应码将被认为 HTTP 认证请求执行失败。如 4xx、5xx 等。</br>
    此时认证结果使用缺省值 `"ignore"`，继续执行认证链。如果当前的 HTTP 认证器是链上的最后一个认证器，则认证失败，客户端将被拒绝连接。
- HTTP 响应的编码格式可以是 `application/json`。

::: tip 从EMQX 4.x 迁移过来
在 4.x 中，EMQX 仅用到了 HTTP API 返回的状态码，而内容则被丢弃。
例如 `200` 表示 `allow`，`403` 表示 `deny`。

因为缺乏丰富的表达能力，在 5.0 中对这块进行了不兼容的重构。
:::

### 示例

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
推荐使用 `POST` 方法。 使用 `GET` 方法时，一些敏感信息（如纯文本密码）可以通过 HTTP 服务器日志记录暴露。
:::

对于不受信任的环境，应使用 HTTPS。

## 配置

HTTP 认证由 `mechanism = password_based` 和 `backend = http` 标识。

支持 HTTP `POST` 和 `GET` 请求。它们各自都有一些特定的选项。

使用 POST 请求配置的 HTTP 认证器示例：

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

使用 GET 请求配置的 HTTP 认证器示例：

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

具有 `get` 和 `post` 可选值的必填字段，表示使用相应的 HTTP 请求方法。

### `url`

用于外部认证请求的 HTTP URL，它可能包含 [placeholders](./authn.md#认证占位符)。

对于 Scheme 为 `https` 的 URLs `ssl` 配置必须启用：

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

请求模板。对于 `post` 请求，它以 JSON 形式在请求体中发送。
对于 `get` 请求，它被编码为 URL 中的查询参数。映射键和值可以包含 [placeholders](./authn.md#认证占位符)。

假设一个 MQTT 客户端使用客户端标识符 `id123`、用户名 `iamuser` 和密码 `secret` 连接。

1. `GET` 请求配置如下：

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

   最终的请求将是：

   ```
   GET /auth/id123?username=iamuser&password=secret HTTP/1.1
   ... Headers ...
   ```

2. `POST` JSON 格式的请求配置如下：

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

   最终的请求将是：

   ```
   POST /auth/id123 HTTP/1.1
   Content-Type: application/json
   ... Other headers ...

   {"username":"iamuser","password":"secret"}
   ```

### `headers`

用于配置 HTTP 认证请求中的 Headers，可选。

对于 `GET` 请求有以下默认 Headers：

```
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
}
```

`GET` 请求的 Headers 不能包含 `Content-Type` Header。

对于 `POST` 请求有以下默认 Headers：

```
{
    "accept" = "application/json"
    "cache-control" = "no-cache"
    "connection" = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
    "content-type" = "application/json"
}
```

`content-type` Header 的值定义了 `POST` 请求的 `body` 编码方，必需为`application/json`。

### `enable_pipelining`

正整数，用于指定异步 HTTP 请求管线的最大数量[HTTP pipelining](https://wikipedia.org/wiki/HTTP_pipelining)。可选，默认值为 `100`。设置为 `1` 时关闭。

### `connect_timeout`, `request_timeout`, `retry_interval` and `max_retries`

控制相应请求阈值的可选配置。默认值为：

```
connect_timeout = 15s
max_retries = 5
request_timeout = 5s
retry_interval = 1s
```

### `pool_size`

可选的整型配置项，用于指定从 EMQX 节点到外部 HTTP Server 的并发连接数。默认值为 8。

### `ssl`

用于连接到外部 HTTP Server 的标准 [SSL 选项](../ssl.md)。
