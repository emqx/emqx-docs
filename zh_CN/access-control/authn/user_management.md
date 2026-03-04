# 使用 HTTP API 管理用户数据

对于存储在内置数据库中的认证数据，您可以通过 EMQX Dashboard 或 HTTP API 创建、更新、删除、查询以及导入用户认证数据。

适用于以下认证方式：

- [使用内置数据库进行密码认证](./mnesia.md)
- [MQTT 5.0 增强认证](./scram.md)

## 用户管理 API 端点

用户管理接口根据认证链的作用范围不同而有所区别。

- **全局 MQTT 认证链**

  ```
  /api/v5/authentication/{id}/users
  ```

- **指定 MQTT 监听器的认证链**

  ```
  /api/v5/listeners/{listener_id}/authentication/{id}/users
  ```

- **网关协议的全局认证链**

  ```
  /api/v5/gateway/{protocol}/authentication/{id}/users
  ```

- **指定网关监听器的认证链**

  ```
  /api/v5/gateway/{protocol}/listeners/{listener_id}/authentication/{id}/users
  ```

关于标识符约定及参数说明，请参见 [REST API](../../admin/api.md)。

## 导入用户

仅 `password_based:built_in_database` 认证器支持用户导入功能。

该功能用于向正在运行的 EMQX 实例批量导入用户。

支持以下接口：

- `/api/v5/authentication/{id}/import_users`
- `/api/v5/gateway/{protocol}/authentication/import_users`
- `/api/v5/gateway/{protocol}/listeners/{listener_id}/import_users`

请求必须为 `multipart/form-data` 类型的 `POST` 请求。

示例：

```bash
curl -v -u admin:public -X 'POST' \
    -H 'Content-Type: multipart/form-data' \
    -F 'filename=@/tmp/myusers.csv' \
    'http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/import_users'
```

### 支持的文件格式

文件格式由其扩展名决定，支持以下格式：

#### `.csv`

带表头的 CSV 文件：

```txt
user_id,password_hash,salt,is_superuser
myuser3,b6c743545a7817ae8c8f624371d5f5f0373234bb0ff36b8ffbf19bce0e06ab75,de1024f462fb83910fd13151bd4bd235,true
myuser4,ee68c985a69208b6eda8c6c9b4c7c2d2b15ee2352cdd64a903171710a99182e8,ad773b5be9dd0613fe6c2f4d8c403139,false
```

必填字段：

- `user_id`
- `password_hash`

可选字段：

- `salt`（默认为空字符串）
- `is_superuser`（默认为 `false`）

#### `.json`

JSON 对象数组：

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

字段要求与 CSV 相同。
