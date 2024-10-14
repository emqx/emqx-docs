# Client-Info 认证

::: tip 

Client-info 认证是 EMQX 企业版功能。

:::

Client-info 认证（`cinfo` 类型）是一种轻量级的认证机制，它通过检查客户端的属性和特性，依据用户定义的规则进行验证。这些规则使用 Variform 表达式来定义匹配条件，并在找到匹配时决定认证结果。例如，要快速阻止没有用户名的客户端，可以使用条件 `str_eq(username, '')`，并将结果设置为 `deny`。

认证器配置如下：

```bash
authentication = [
  {
    mechanism = cinfo
    checks = [
      # 允许用户名以 'super-' 开头的客户端
      {
        is_match = "regex_match(username, '^super-.+$')"
        result = allow
      },
      # 拒绝用户名为空且客户端 ID 以 'v1-' 开头的客户端
      {
        # 当 is_match 是数组时，如果所有检查都为 true，则返回 'true'
        is_match = ["str_eq(username, '')", "str_eq(nth(1,tokens(clientid,'-')), 'v1')"]
        result = deny
      }
      # 如果所有检查都没有返回 'allow' 或 'deny'，则继续到下一个认证器
    ]
  },
  # ... 更多认证器 ...
  # ...
  # 如果所有认证器都没有返回 'allow' 或 'deny'，则客户端不会被拒绝
]
```

更多匹配表达式示例：

- 匹配所有客户端：`true`
- 匹配 TLS 证书的通用名与用户名相同的客户端：`str_eq(cert_common_name, username)`
- 匹配密码为环境变量 `EMQXVAR_SECRET` 与客户端 ID 连接后的 `sha1` 哈希值的客户端：`str_eq(password, hash(sha1, concat([clientid, getenv('SECRET')])))`
- 匹配客户端属性 `client_attrs.group` 不为 `g0` 的客户端：`str_neq(client_attrs.group, 'g0')`
- 匹配客户端 ID 以 zone 名称开头的客户端：`regex_match(clientid, concat(['^', zone, '.+$']))`

## 通过 Dashboard 配置 Clinet-info 认证

<!-- TODO --->

