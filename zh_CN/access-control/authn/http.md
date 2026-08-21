# 使用 HTTP 服务进行密码认证

EMQX 支持通过外部 HTTP 服务进行密码认证。客户端连接时，EMQX 将使用客户端信息构造 HTTP 请求，并根据请求返回的内容判断认证结果，从而实现复杂的认证鉴权逻辑。

::: tip 前置准备

熟悉 [EMQX 认证基本概念](../authn/authn.md)
:::

## 请求格式与返回结果

认证过程类似一个 HTTP API 调用，EMQX 作为请求客户端需要按照 "API" 要求的格式构造并向 HTTP 服务发起请求，而 HTTP 服务需要按照 "客户端" 的要求返回结果：

- 响应编码格式 `content-type` 必须是 `application/json`。
- 认证结果通过 body 中的 `result` 标示，可选 `allow`、`deny`、`ignore`。
- 超级用户通过 body 中的 `is_superuser` 标示，可选 `true`、`false`。
- 从 EMQX v5.7.0 版本开始，您可以使用可选的 `client_attrs` 字段设置[客户端属性](../../client-attributes/client-attributes.md)。请注意，键和值都必须是字符串类型。
- 从 EMQX v5.8.0 版本开始，您可以在响应体中设置一个可选的 `acl` 字段，用于指定客户端的权限。有关更多信息，请参阅[权限列表（ACL）](./acl.md)。
- 从 EMQX v5.8.0 版本开始，您可以在响应体中设置一个可选的 `expire_at` 字段，用于指定客户端的认证到期时间，并强制客户端断开连接以便重新认证。该值为 Unix 时间戳（秒）。
- HTTP 响应状态码 `Status Code` 应当为 `200` 或 `204`，返回 `4xx/5xx` 状态码时将忽略 body 并判定结果为 `ignore`，继续执行认证链。

响应示例：

```js
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "result": "allow", // "allow" | "deny" | "ignore"
    "is_superuser": false, // true | false，该项为空时默认为 false
    "client_attrs": { // 可选 (自 v5.7.0 起)
        "role": "admin",
        "sn": "10c61f1a1f47"
    }
    "expire_at": 1654254601, // 可选 (自 v5.8.0 起)
    "acl": // 可选 (自 v5.8.0 起)
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

::: tip EMQX 4.x 兼容性说明
在 4.x 中，EMQX 仅用到了 HTTP API 返回的状态码，而内容则被丢弃。例如 `200` 表示 `allow`，`403` 表示 `deny`。因为缺乏丰富的表达能力，在 5.0 中对这一机制进行了不兼容的调整。
:::

## 配置动态主机名解析

默认情况下，HTTP 认证器在创建时解析 `url` 中的主机名，并使用持久连接池。如果需要为每次认证请求解析主机名，请将 `hostname_resolution` 设置为 `dynamic`。

动态主机名解析还允许在 `url` 的主机部分使用占位符。例如，以下配置根据客户端的 `tenant` 属性，将认证请求发送到不同的端点：

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

配置动态主机名解析时，请注意：

- `hostname_resolution` 的可选值为 `static` 和 `dynamic`，默认值为 `static`。也可以为字面量主机名使用 `dynamic`，使 EMQX 为每次请求重新解析该主机名。
- 如果 URL 主机包含占位符，必须将 `hostname_resolution` 设置为 `dynamic`，并在 `allowed_hosts` 中至少配置一个条目。
- `allowed_hosts` 中的每个条目必须是精确主机名（例如 `auth.example.com`）或通配模式（例如 `*.auth.example.com`）。通配模式匹配指定后缀下的主机名，但不匹配该后缀本身。URL 使用字面量主机名时，`allowed_hosts` 不生效。
- 在 URL 的网络位置（authority）部分，只有主机可以包含占位符。协议（scheme）必须是 `http` 或 `https`，端口必须是字面量整数。不支持用户信息（userinfo）和片段（fragment）。URL 路径和查询参数仍然支持占位符。
- 如果 EMQX 无法渲染出有效主机名，或者渲染后的主机名与 `allowed_hosts` 不匹配，EMQX 不会发送 HTTP 请求，认证将失败。
- 在 `dynamic` 模式下，发往所有渲染后主机的请求共享一个连接池。`pool_size` 限制连接池可保留以供复用的空闲连接数。将其设置为 `0` 可禁用连接复用。`enable_pipelining` 和 `max_inactive` 在该模式下不生效。
- 在 `dynamic` 模式下发送 HTTPS 请求时，EMQX 会将配置的 TLS 选项应用于渲染后的主机。如果没有显式配置服务器名称指示（SNI），EMQX 将根据渲染后的主机名生成 SNI。
- `hostname_resolution` 为 `dynamic` 时不支持 OAuth2。

## 通过 Dashboard 配置

1. 在 [EMQX Dashboard](http://127.0.0.1:18083/#/authentication) 页面，点击左侧导航栏的**访问控制** -> **认证**。
2. 在**认证**页面，点击**创建**。
3. 依次选择**认证方式**为 `Password-Based`，**数据源**为 `HTTP Server`，进入**配置参数**页签：

![authn-http](./assets/authn-http.png)

4. 您可根据如下说明完成相关配置：

   - **请求方式**：选择 HTTP 请求方式，可选值： `get`、`post`。

     :::tip
     推荐使用 `POST` 方法。 使用 `GET` 方法时，一些敏感信息（如纯文本密码）可能通过 HTTP 服务器日志记录暴露。此外，对于不受信任的环境，请使用 HTTPS。
     :::

   - **URL**：输入 HTTP 服务的 URL 地址。当**主机名解析方式**设置为 `动态` 时，主机部分可以使用[认证占位符](./authn.md#认证占位符)。

   - **主机名解析方式**：设置为 `静态` 时，在创建认证器时解析固定主机名；设置为 `动态` 时，为每次请求解析主机名。默认选项为 `静态`。更多信息，参见[配置动态主机名解析](#配置动态主机名解析)。

   - **允许的主机**：对应配置项 `allowed_hosts`。URL 主机包含占位符时，配置渲染后的主机名可以匹配的精确主机名或通配模式。

   - **调用条件**：一个 Variform 表达式，用于控制是否将此 HTTP 服务认证证器应用于客户端连接。该表达式会根据客户端的属性（例如 `username`、`clientid`、`listener` 等）进行评估。如果表达式的结果为字符串 `"true"`，则会触发认证器。否则，认证器将被跳过。有关调用条件的更多信息，请参见[认证器调用条件](./authn.md#认证器调用条件)。

   - **请求头**（可选）：HTTP 请求头配置。可以添加多个请求头。键和值可以使用[占位符](./authn.md#认证占位符)。

   - **启用 TLS**：配置是否启用 TLS。

   - **请求体**：请求模板，对于 `POST` 请求，它以 JSON 形式在请求体中发送。对于 `GET` 请求，它被编码为 URL 中的查询参数（Query String）。映射键和值可以使用[占位符](./authn.md#认证占位符)。

   - **高级设置**：在此部分进行并发连接、连接超时等待时间、最大 HTTP 请求数以及请求超时时间。

     - **连接池大小**（可选）：在 `静态` 模式下，指定持久连接池大小，取值不能小于 `1`；在 `动态` 模式下，指定可在请求间复用的连接数，设置为 `0` 可禁用连接复用。默认值：`8`。
     - **连接超时**（可选）：填入连接超时等待时长。默认值：`15` 秒。
     - **最大空闲时间**：HTTP 驱动在无任何活动时，尝试重连前的最大等待时间。默认值：`10` 秒。在 `动态` 模式下，该设置不生效。
     - **HTTP 管道**（可选）：正整数，指定无需等待响应可发出的最大 HTTP 请求数。默认值：`100`。在 `动态` 模式下，该设置不生效。
     - **请求超时**（可选）：填入连接超时等待时长。默认值：`5` 秒。

5. 最后点击**创建**完成相关配置。

## 通过配置文件设置

此外，您可以通过配置项完成相关配置。<!-- 具体可参考：[authn-http:post](../../configuration/configuration-manual.html#authn-http:post) 与 [authn-http:get](../../configuration/configuration-manual.html#authn-http:get)。-->

以下为使用 `POST` 和 `GET` 请求配置的 HTTP 请求示例：

<!--这里的内容需要更新-->

:::: tabs type:card

::: tab POST 请求示例

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

::: tab GET 请求示例

注意： body 将被转换为查询字符串。

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
