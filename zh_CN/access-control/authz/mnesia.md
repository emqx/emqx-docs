# 内置数据库

您可以将客户端的授权规则存储在 EMQX 的内置数据库中。
该方案的好处是开销小，因为它不需要去访问外部数据库，或者 HTTP 服务器，而是直接从本地（内存中）快速搜索匹配的规则。

## 配置

该 Authorizer 的配置必需有 `type = built_in_database`。

```
{
    type = built_in_database
    enable = true
}
```

## 规则管理

授权规则可以在仪表盘的 “访问控制” -> “授权” -> “Built-in Database” -> 用户管理 中通过点击 “+添加”按钮来输入。

也可以通过这个 API 来进行管理 `/api/v5/authorization/sources/built_in_database`。

每条规则可以适用于：

* 使用用户名对客户端进行匹配：`/api/v5/authorization/sources/built_in_database/username`
* 使用客户端 ID 对客户端进行匹配： `/api/v5/authorization/sources/built_in_database/clientid`;
* 所有的客户端：`/api/v5/authorization/sources/built_in_database/all`.

API 文档中可以找到更多的信息和例子。
下面是使用其中一个 API 为一个客户端（`client1`）创建规则的例子。
```
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
每个规则需要包括如下信息：
* permission：`allow` 或者 `deny`;
* action：客户端执行的操作 `publish`，`subscribe`，或 `all`;
* topic：主题，主题过滤器（通配符主题），或者带[占位符](authz.md#主题占位符)的主题。
