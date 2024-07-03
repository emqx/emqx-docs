# 授权列表

::: tip
该文档专注于作为 *认证响应* 的一部分包含的针对每个客户端的访问控制列表（ACL）规则。这种方法既简洁又高效，应能满足大部分需求。对于更全面但通用的授权方法，请参阅[授权文档](../authz/authz.md) 。
:::

如果在例如 HTTP 和 JWT 的认证结果中返中携带了权限列表 (ACL)，那么客户端连接后的发布和订阅动作将会受到这些规则的限制。

::: tip
通过认证设置的权限列表，将优先于所有授权检查器被检查，参考 [授权检查优先级](../authz/authz.md#授权检查优先级)。
:::

:::: tabs type:board-card

::: tab 新版格式

新版格式从 v5.5.0 开始支持，使用了权限列表来指定多条权限，更接近 ACL 规则的语义且使用更加灵活。

不同于旧版格式，使用新版格式时，当客户端操作未匹配到任何规则时，EMQX 将继续执行授权检查器的检查。旧版格式仍然是兼容的，但建议使用新版格式。

权限列表包含以下字段：

| 字段 | 必选 | 含义 |
| --- |  --- |  --- |
| permission | 是 | 是否允许当前客户端的操作请求；可选值：`allow`、`deny` |
| action | 是 | 规则对应的操作；可选值: `publish`、`subscribe`、 `all` |
| topic | 是 | 规则对应的主题，支持[主题占位符](../authz/authz.md#主题占位符) |
| qos | 否 | 数组，指定规则适用的消息 QoS，如 `[0, 1]`、`[1, 2]`，默认为全部 QoS |
| retain | 否 | 布尔值，仅用于发布操作，指定当前规则是否支持发布保留消息，可选值有 `true`、`false`，默认允许保留消息。 |

示例：

```json
{
  "exp": 1706844358,
  "username": "emqx_u",
  "acl": [
    {
      // 允许客户端发布 t/${clientid} 主题的消息，例如 t/emqx_c
      "permission": "allow",
      "action": "publish",
      "topic": "t/${clientid}"
    },
    {
      "permission": "allow",
      "action": "subscribe",
      // `eq` 前缀意味着该规则仅适用于主题过滤器 t/1/#，但不适用于 t/1/x 或 t/1/y 等
      "topic": "eq t/1/#",
      // 该规则只匹配 QoS 1 但不匹配 QoS 0 或 2
      "qos": [1]
    },
    {
      // 禁止客户端发布 t/2 主题的保留消息，消息为非保留消息则是允许的
      "permission": "deny",
      "action": "publish",
      "topic": "t/2",
      "retain": true
    },
    {
      // 禁止客户端发布或订阅 t/3 主题，包括所有 QoS 级别和保留消息
      "permission": "deny",
      "action": "all",
      "topic": "t/3"
    }
  ]
}
```

:::

::: tab 旧版格式

JWT 权限列表定义了 `pub`、`sub` 和 `all` 3 个可选字段，分别用于指定发布、订阅以及发布订阅的主题白名单列表。主题中允许使用主题通配符和占位符（目前仅支持 `${clientid}` 与 `${username}`）。由于可能存在主题内容与占位符语法冲突的情况，我们也提供了 `eq` 语法来取消占位符插值。示例：

```json
{
  "exp": 1654254601,
  "username": "emqx_u",
  "acl": {
    "pub": [
      "testpub1/${username}",
      "eq testpub2/${username}"
    ],
    "sub": [
      "testsub1/${username}",
      "testsub2/${clientid}"
      "testsub2/#"
    ],
    "all": [
      "testall1/${username}",
      "testall2/${clientid}",
      "testall3/#"
    ]
  }
}
```

其中，`testpub1/${username}` 会在运行时被替换为 `testpub1/emqx_u`，而 `eq testpub2/${username}` 在运行时仍会按照 `testpub2/${username}` 处理。

:::

::::
