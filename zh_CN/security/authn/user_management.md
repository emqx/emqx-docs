# 用户管理 API

一些 [认证器](./authn.md#认证器) (具有 `built_in_database` 后端) 将用户凭据存储在 EMQX 的内置数据库（Mnesia）中：

- `password_based:built_in_database`
- `scram:built_in_database`

对于这些认证器，EMQX 提供了相关的 HTTP API 来管理用户凭据，支持创建、更新、删除和列出用户凭据。

::: warning
认证器之间的用户相互独立。
:::

## API 端点

用于 MQTT 全局认证的用户管理的 API 端点为 `/api/v5/authentication/{id}/users`。

用于 MQTT 监听器认证的用户管理的 API 端点为 `/api/v5/listeners/{listener_id}/authentication/{id}/`。

用于管理其他接入协议的全局认证的用户管理的 API 端点为 `/api/v5/gateway/{protocol}/authentication`。

用于管理其他接入协议的监听器认证的用户管理的 API 端点为 `/api/v5/gateway/{protocol}/listeners/{listener_id}/authentication`。

有关标识符的约定，请参阅 [认证 API 文档](./authn.md#http-api)。

## 导入用户

使用内置数据库的密码认证器支持用户导入。

用于导入用户的 API 端点为:

- `/api/v5/authentication/{id}/import_users`
- `/api/v5/listeners/{listener_id}/authentication/{id}/import_users`
- `/api/v5/gateway/{protocol}/authentication/import_users`
- `/api/v5/gateway/{protocol}/listeners/{listener_id}/import_users`

用户导入请求是一个请求体为 `multipart/form-data` 类型的 POST 请求。

示例：

```shell
curl -v -u admin:public -X 'POST' \
    -H 'Content-Type: multipart/form-data' \
    -F 'filename=@/tmp/myusers.csv' \
    'http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/import_users'
```

目前支持以下文件类型：

- `.csv`

  ```csv
  user_id,password_hash,salt,is_superuser
  myuser3,b6c743545a7817ae8c8f624371d5f5f0373234bb0ff36b8ffbf19bce0e06ab75,de1024f462fb83910fd13151bd4bd235,true
  myuser4,ee68c985a69208b6eda8c6c9b4c7c2d2b15ee2352cdd64a903171710a99182e8,ad773b5be9dd0613fe6c2f4d8c403139,false
  ```

- `.json`

  ```json
  [
    {
        "user_id":"myuser1",
        "password_hash":"c5e46903df45e5dc096dc74657610dbee8deaacae656df88a1788f1847390242",
        "salt": "e378187547bf2d6f0545a3f441aa4d8a",
        "is_superuser": true
    },
    {
        "user_id":"myuser2",
        "password_hash":"f4d17f300b11e522fd33f497c11b126ef1ea5149c74d2220f9a16dc876d4567b",
        "salt": "6d3f9bd5b54d94b98adbcfe10b6d181f",
        "is_superuser": false
    }
  ]
  ```
