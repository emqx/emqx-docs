# 内置数据库

EMQX 通过内置数据库为用户提供了一种低成本、开箱即用的授权规则存储方式。您可以通过 Dashboard 或配置文件设置使用内置数据库作为数据源，通过 Dashboard 或 HTTP API 添加相关授权检查规则。
::: tip 前置准备

熟悉 [EMQX 授权基本概念](./authz.md)
:::

## 通过 Dashboard 配置

1. 在 [EMQX Dashboard](http://127.0.0.1:18083/#/authentication) 中，点击左侧菜单的**访问控制** > **授权**，进入**授权**页面。

2. 点击右上角的**创建**按钮，在**数据源**中选择**内置数据库**，然后点击 **下一步**。

   <img src="./assets/authz-mnesia.png" alt="authz-mnesia" style="zoom:60%;" />

3. 在**配置参数**步骤中，设置**最大规则数**（默认值为 `100`），该参数用于限制每个客户端或用户允许配置的最大权限规则数量。

   ::: tip 提示

   设置过多的权限规则可能会影响系统性能。

   :::

4. 点击**创建**完成配置。

## 通过配置文件配置

您也可通过配置文件中的 `authorization` 字段配置通过 EMQX 内置数据库存储授权规则。

代码示例：

```hcl
{
    type = built_in_database
    enable = true
}
```

其中

- `type`：授权检查器的数据源类型，此处填入 `built_in_database`
- `enable`：是否激活该检查器，可选值：`true`、`false`

<!--详细参数列表，请参考 [authz-mnesia](../../configuration/configuration-manual.html#authz-mnesia)。-->

## 配置授权检查规则

您可通过 Dashboard 或 API 创建和管理授权规则。

### 通过 Dashboard 添加授权规则

在 Dashboard 的**授权**页面，点击**内置数据库**数据源对应的**操作**栏下的**权限管理**，即可进行授权检查规则的配置。

![authz-mnesia-rule](./assets/authz-mnesia-rule.png)

您可根据需要从客户端 ID、用户名或直接从主题角度设置授权检查。

- **客户端 ID**：见 **客户端 ID** 页签，指定适用此条规则的客户端
- **用户名**：见 **用户名** 页签，指定适用此条规则的用户名
- **权限**：是否允许当前客户端/用户的某类操作请求；可选值：**允许**、**拒绝**
- **操作**：配置该条规则对应的操作；可选值：**发布**、**订阅**、**发布与订阅**
- **主题**：配置该条规则对应的主题

EMQX 支持针对单个客户端或用户配置多条授权检查规则，您可通过页面的 **上移**、**下移** 调整不同规则的执行顺序和优先级。

如希望同时针对多个客户端或用户配置授权检查规则，可通过 HTTP API 传入相关配置。

### 通过 API 创建授权规则

您可通过 `/api/v5/authorization/sources/built_in_database` API 创建以及管理内置数据库后端的授权规则。以下为操作步骤：

#### 步骤 1：获取认证 Token

你需要先通过 EMQX Dashboard 登录认证，以获取 API 访问所需的 Token：

```bash
export EMQX_TOKEN=$(curl --silent -X 'POST' "http://localhost:18083/api/v5/login" \
  -H 'Accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{"username": "admin","password": "public"}' | jq -r ".token")
```

#### 步骤 2：创建内置数据库授权源

比如您可通过如下代码针对 `client1` 客户端创建授权规则：

```bash
curl -X 'POST' \
  'http://localhost:18083/api/v5/authorization/sources' \
  -H "Authorization: Bearer $EMQX_TOKEN" \
  -H 'Accept: */*' \
  -H 'Content-Type: application/json' \
  -d '{
        "enable": true,
        "max_rules": 100,
        "type": "built_in_database"
  }'
```

#### 步骤 3：创建授权规则

你可以为以下对象创建规则：

- **指定 client ID 的客户端**：

  ```
  curl -X 'POST' \
    'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/clients' \
    -H "Authorization: Bearer $EMQX_TOKEN" \
    -H 'Accept: */*' \
    -H 'Content-Type: application/json' \
    -d '[
    {
      "clientid": "client1",
      "rules": [
        {
          "action": "publish",
          "permission": "allow",
          "topic": "test/topic/1"
        },
        {
          "action": "subscribe",
          "permission": "allow",
          "topic": "test/topic/2"
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

- **指定用户名的客户端**：

  ```bash
  curl -X 'POST' \
    'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/users' \
    -H "Authorization: Bearer $EMQX_TOKEN" \
    -H 'Accept: */*' \
    -H 'Content-Type: application/json' \
    -d '[
    {
      "username": "user1",
      "rules": [
        {
          "action": "publish",
          "permission": "allow",
          "topic": "test/topic/1"
        },
        {
          "action": "subscribe",
          "permission": "allow",
          "topic": "test/topic/2"
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

- **所有客户端（全局规则）**：

  ```bash
  curl -X 'POST' \
    'http://localhost:18083/api/v5/authorization/sources/built_in_database/rules/all' \
    -H "Authorization: Bearer $EMQX_TOKEN" \
    -H 'Accept: */*' \
    -H 'Content-Type: application/json' \
    -d '{
    "rules": [
      {
        "action": "publish",
        "permission": "allow",
        "topic": "test/topic/1"
      },
      {
        "action": "subscribe",
        "permission": "allow",
        "topic": "test/topic/2"
      },
      {
        "action": "all",
        "permission": "deny",
        "topic": "eq test/#"
      }
    ]
  }'
  ```

每条规则包括以下字段：

- `permission`：是否允许该操作，可选值为 `allow` 或 `deny`。
- `action`：操作类型，可选值为 `publish`、`subscribe` 或 `all`。
- `topic`：该规则适用的主题过滤器，支持使用[主题占位符](./authz.md#topic-placeholders)。
- `qos`：*(可选)* 规则适用的 QoS 等级数组，例如 `[0, 1]`；默认为全部 QoS。
- `retain`：*(可选)* 是否适用于保留消息，可选值为 `true` 或 `false`。默认允许保留消息。
