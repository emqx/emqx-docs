# 内置数据库

EMQX 通过内置数据库为用户提供了一种低成本、开箱即用的授权规则存储方式。您可以通过 Dashboard 或配置文件设置使用内置数据库作为数据源，通过 Dashboard 或 HTTP API 添加相关授权检查规则。

::: tip 前置准备：

- 熟悉 [EMQX 授权基本概念](./authz.md)
  :::

## 通过 Dashboard 配置

在 [EMQX Dashboard](http://127.0.0.1:18083/#/authentication) 页面，点击左侧导航栏的 **访问控制** -> **授权**，在 **授权** 页面，添加 **Built-in Database** 作为 **数据源**， 点击**下一步 **进入 **配置参数 **页签。由于无需配置其他参数，可直接点击 **创建** 完成配置。

## 通过配置文件配置

您也可通过配置文件中的 `authorization` 字段配置通过 EMQX 内置数据库存储授权规则。

代码示例：

```hocon
{
    type = built_in_database
    enable = true
}
```

其中

- `type`：授权检查器的数据源类型，此处填入 `built_in_database`
- `enable`：是否激活该检查器，可选值：`true`、`false`

详细参数列表，请参考 [authz-mnesia](../../configuration/configuration-manual.md#authz-mnesia)。

## 配置授权检查规则

您可通过 Dashboard 或 API 传入授权规则。

### 通过 Dashboard 配置

在 Dashboard 的 **授权** 页面，点击 **Built-in Database** 数据源对应的 **操作 **栏下的 **权限管理**，即可进行授权检查规则的配置。

您可根据需要从客户端 ID**、**用户名或直接从主题角度设置授权检查。

- **客户端 ID**：见 **客户端 ID**  页签，指定适用此条规则的客户端
- **用户名**：见 **用户名** 页签，指定适用此条规则的用户名
- **权限**：是否允许当前客户端/用户的某类操作请求；可选值：**Allow**、**Deny**
- **操作**：配置该条规则对应的操作；可选值：**Publish**、**Subscribe**、**Publish & Subscribe**
- **主题**：配置该条规则对应的主题

EMQX 支持针对单个客户端或用户配置多条授权检查规则，您可通过页面的 **上移**、**下移** 调整不同规则的执行顺序和优先级。

如希望同时针对多个客户端或用户配置授权检查规则，可通过 HTTP API 传入相关配置。

### 通过 API 配置

您可通过 API 传入以及管理授权规则：

- 指定适用此条规则的客户端：
  - `/api/v5/authorization/sources/built_in_database/clientid`
- 指定适用此条规则的用户名：
  - `/api/v5/authorization/sources/built_in_database/username`
- 指定适用此条规则的主题：
  - `/api/v5/authorization/sources/built_in_database/all`


比如我们可通过如下代码针对 `client1` 客户端创建授权规则：

```bash
curl -X 'POST' \
  'http://localhost:18083/api/v5/authorization/sources/built_in_database/clientid' \
  -H 'accept: */*' \
  -H 'Content-Type: application/json' \
  -d '[
  {
    "clientid": "client1",
    "rules": [
      {
        "action": "publish",
        "permission": "allow",
        "topic": "test/toopic/1"
      },
      {
        "action": "subscribe",
        "permission": "allow",
        "topic": "test/toopic/2"
      },
      {
        "action": "all",
        "permission": "deny",
        "topic": "eq test/#"
      }
    ]
  }
]'
```

每条规则应包括如下信息：

- permission：是否允许当前客户端/用户的某类操作请求；可选值：`Allow`、`Deny`
- action：配置该条规则对应的操作；可选值: `publish`、`subscribe`、 `all`
- topic：配置该条规则对应的主题，支持[主题占位符](authz.md#主题占位符)
