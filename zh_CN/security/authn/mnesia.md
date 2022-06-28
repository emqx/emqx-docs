# 使用内置数据库（Mnesia）的密码认证

使用内置数据库（Mnesia）作为客户端身份凭据的存储介质，无需额外部署其他数据库，在使用上足够简单轻量。

## 配置

使用内置数据库的密码认证由 `mechanism = password_based` 和 `backend = built_in_database` 标识。

```
{
    mechanism = password_based
    backend = built_in_database
    enable = true

    password_hash_algorithm {
        name = sha256
        salt_position = suffix
    }

    user_id_type = username
}
```

### `user_id_type`

可选值：

- `username`
- `clientid`

此选项用于指定内置数据库中存储的用户 ID 的类型，也用于表明认证器应该使用 MQTT `CONNECT` 报文中的 `Username` 还是 `Client Identifier` 来检索数据库并验证客户端的身份。

### `password_hash_algorithm`

`password_hash_algorithm` 指定标准的 [散列选项](./authn.md#密码散列).

## 用户管理

使用内置数据库的密码认证可以通过 [HTTP API](./user_management.md) 管理用户.
