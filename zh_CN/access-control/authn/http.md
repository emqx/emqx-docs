# 使用 HTTP 服务进行密码认证

EMQX 支持通过外部 HTTP 服务进行密码认证。客户端连接时，EMQX 将使用客户端信息构造 HTTP 请求，并根据请求返回的内容判断认证结果，从而实现复杂的认证鉴权逻辑。

::: tip 前置准备：

- 熟悉 [EMQX 认证基本概念](../authn/authn.md)
:::

## 请求格式与返回结果

认证过程类似一个 HTTP API 调用，EMQX 作为请求客户端需要按照 "API" 要求的格式构造并向 HTTP 服务发起请求，而 HTTP 服务需要按照 "客户端" 的要求返回结果：

- 响应编码格式 `content-type` 必须是 `application/json`。
- 认证结果通过 body 中的 `result` 标示，可选 `allow`、`deny`、`ignore`。
- 超级用户通过 body 中的 `is_superuser` 标示，可选 `true`、`false`。
- HTTP 响应状态码 `Status Code` 应当为 `200` 或 `204`，返回 `4xx/5xx` 状态码时将忽略 body 并判定结果为 `ignore`，继续执行认证链。

响应示例：

```json
HTTP/1.1 200 OK
Headers: Content-Type: application/json
...
Body:
{
    "result": "allow", // 可选 "allow" | "deny" | "ignore"
    "is_superuser": true // 可选 true | false，该项为空时默认为 false
}
```

::: tip EMQX 4.x 兼容性说明
在 4.x 中，EMQX 仅用到了 HTTP API 返回的状态码，而内容则被丢弃。例如 `200` 表示 `allow`，`403` 表示 `deny`。因为缺乏丰富的表达能力，在 5.0 中对这一机制进行了不兼容的调整。
:::

## 通过 Dashboard 配置

在 [EMQX Dashboard](http://127.0.0.1:18083/#/authentication) 页面，点击左侧导航栏的**访问控制** -> **认证**，在随即打开的**认证**页面，单击**创建**，依次选择**认证方式**为 `Password-Based`，**数据源**为 `HTTP Server`，进入**配置参数**页签：

您可根据如下说明完成相关配置：

**HTTP**：配置请求方式与 URL。

- **请求方式**：选择 HTTP 请求方式，可选值： `get` 、 `post`

  :::tip
  推荐使用 `POST` 方法。 使用 `GET` 方法时，一些敏感信息（如纯文本密码）可能通过 HTTP 服务器日志记录暴露。此外，对于不受信任的环境，请使用 HTTPS。
  :::

- **URL**：输入 HTTP 服务的 URL 地址。

- **Headers**（可选）：HTTP 请求头配置。

**连接配置**：在此部分进行并发连接、连接超时等待时间、最大 HTTP 请求数以及请求超时时间。

- **Pool size**（可选）：整数，指定从 EMQX 节点到外部 HTTP Server 的并发连接数；默认值：**8**。<!--有范围吗？-->
- **连接超时**（可选）：填入连接超时等待时长，可选单位：**小时**、**分钟**、**秒**、**毫秒**。
- **HTTP 管道**（可选）：正整数，指定无需等待响应可发出的最大 HTTP 请求数；默认值：**100**。
- **请求超时**（可选）：填入连接超时等待时长，可选单位：**小时**、**分钟**、**秒**、**毫秒**

**TLS 配置**：配置是否启用 TLS。

**认证配置**：

- **Body**：请求模板，对于 `POST` 请求，它以 JSON 形式在请求体中发送。对于 `GET` 请求，它被编码为 URL 中的查询参数（Query String）。映射键和值可以使用 [占位符](./authn.md#认证占位符)。

最后点击**创建**完成相关配置。

## 通过配置文件设置

此外，您可以通过配置项完成相关配置，具体可参考：[authn-http:post](../../configuration/configuration-manual.md#authn-http:post) 与 [authn-http:get](../../configuration/configuration-manual.md#authn-http:get)。

以下为使用 `POST` 和 `GET` 请求配置的 HTTP 请求示例：

<!--这里的内容需要更新-->

:::: tabs type:card

::: tab POST 请求示例

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

:::

::: tab GET 请求示例

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

:::

::::
