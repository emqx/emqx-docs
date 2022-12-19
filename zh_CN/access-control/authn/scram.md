# SCRAM 认证

该认证器实现了 [SCRAM](https://en.wikipedia.org/wiki/Salted_Challenge_Response_Authentication_Mechanism) 认证，并使用 EMQX 内置的 Mnesia 数据库存储客户端凭据（_users_）。

SCRAM 认证是一种比密码认证更复杂的机制，它依赖与 MQTT 5.0 提供的增强认证机制，需要在连接期间交换额外的 MQTT 报文。

SCRAM 认证不依赖外部数据源，使用简单轻量。

::: tip
SCRAM 认证仅支持使用 MQTT v5.0 的连接。
:::

## 配置

SCRAM 认证由 `mechanism = scram` and `backend = built_in_database` 标识.

```
{
    mechanism = scram
    backend = built_in_database
    enable = true

    algorithm = sha512
    iteration_count = 4096
}
```

### `algorithm`

可选值：

- `sha256` 用于 `SCRAM-SHA-256` 方法；
- `sha512` 用法 `SCRAM-SHA-512` 方法。

### `iteration_count`

可选的整型配置，用于指定迭代次数，默认值为 4096。

## 用户管理

用户可以通过 [HTTP API](./user_management.md) 进行管理。
