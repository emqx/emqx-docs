# 使用 HTTP 应用进行密码认证

EMQX 支持通过 HTTP 应用进行密码认证。启用 HTTP 认证后，EMQX 将携带客户端信息向 HTTP 服务发起请求，并根据返回的响应状态码确认是否接受连接，从而实现复杂的认证鉴权逻辑。

在 4.x 版本中，EMQX 仅会提示 HTTP API 返回的状态码，如  `200` 、`403`，内容则会被丢弃。为了向用户提供更多的信息，我们在 EMQX 5.0 版本中增加了对请求内容的返回。

::: tip
前置准备：

- 熟悉 [EMQX 认证基本概念](../authn/authn.md)
:::

## 通过 Dashboard 配置

您可以使用 Dashboard 来创建通过 HTTP 应用进行密码认证。

在 [EMQX Dashboard](http://127.0.0.1:18083/#/authentication) 页面，点击左侧导航栏的**访问控制** -> **认证**，在随即打开的**认证**页面，单击**创建**，依次选择**认证方式**为 `Password-Based`，**数据源**为 `HTTP Server`，进入**配置参数**页签：

![EMQX 内置数据库认证](./assets/authn-mnesia-1.png)

您可根据如下说明完成相关配置：

**HTTP**：<!--插入简要说明，这快要配置什么-->

- **请求方式**：选择 HTTP 请求方式，可选值： `get` 、 `post`

  ::: danger
  推荐使用 `POST` 方法。 使用 `GET` 方法时，一些敏感信息（如纯文本密码）可能通过 HTTP 服务器日志记录暴露。此外，对于不受信任的环境，请使用 HTTPS。
  :::

- **URL**：输入 HTTP 应用的 IP 地址。

- **Headers**（可选）：完成 HTTP 请求头的配置 <!--键、值和添加这块内容需要补充下-->

**连接配置**：<!--插入简要说明，这快要配置什么-->

- **Pool size**（可选）：填入一个整数用于指定从 EMQX 节点到外部 HTTP Server 的并发连接数；默认值：**8**。<!--有范围吗？-->
- **连接超时**（可选）：填入连接超时等待时长，可选单位：**小时**、**分钟**、**秒**、**毫秒**
- **HTTP 管道**（可选）：填入一个正整数用于指定无需等待响应可发出的最大 HTTP 请求数；默认值：**100**。
- **请求超时**（可选）：填入连接超时等待时长，可选单位：**小时**、**分钟**、**秒**、**毫秒**

**TLS 配置**：配置是否启用 TLS

**认证配置**：在此处完成 HTTP 请求体的配置。

<!--需要补上相关信息-->

最后点击**创建**完成相关配置。



## 通过配置项进行设置

此外，您可以通过配置项完成相关配置，具体可参考：[authn-http:post](../../admin/cfg.md#authn-http:post) 与 [authn-http:get](../../admin/cfg.md#authn-http:get)。

以下为使用 `POST` 和 `GET` 请求配置的 HTTP 请求示例：

<!--这里的内容需要更新-->

:::: tabs type:card

::: tab POST 请求示例

```
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

:::

::::

### 返回结果

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

其中：

- 响应编码格式 `content-type` 必须是 `application/json`
- 认证结果通过 body 中的 `result` 标示，可选 `alow`、`deny`、`ignore`
- 超级用户通过 body 中的 `is_superuser` 标示，可选 `true`、`false`
- 响应状态码 `Status Code` 应当为 `200` 或 `204`，返回 4xx/5xx 状态码时将忽略 body 并判定结果为 `ignore`，继续执行认证。
