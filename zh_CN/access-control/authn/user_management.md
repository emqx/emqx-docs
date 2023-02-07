# 通过 HTTP API 管理认证数据

<!--这篇我觉得最好也能按照用户使用的路径重写下，目前我觉得有些不知道怎么开始-->

EMQX 提供了 Dashboard 与 HTTP API 来创建、更新、删除和查看内置数据库中的认证数据，适用于以下认证器：

- [使用内置数据库进行密码认证](./mnesia.md)
- [MQTT 5.0 增强认证](./scram.md)

## REST API

MQTT 全局认证数据管理 API： `/api/v5/authentication/{id}/users`。

MQTT 监听器认证数据管理 API： `/api/v5/listeners/{listener_id}/authentication/{id}/`。

其他接入协议的全局认证数据管理 API： `/api/v5/gateway/{protocol}/authentication`。

其他接入协议的监听器认证数据管理 API： `/api/v5/gateway/{protocol}/listeners/{listener_id}/authentication`。

详细的请求方式与参数请参考 [HTTP API](../../admin/api.md)。

## 导入数据

使用内置数据库的密码认证器支持从文件导入数据。

用于导入数据的 API 为:

- `/api/v5/authentication/{id}/import_users`
- `/api/v5/listeners/{listener_id}/authentication/{id}/import_users`
- `/api/v5/gateway/{protocol}/authentication/import_users`
- `/api/v5/gateway/{protocol}/listeners/{listener_id}/import_users`

数据导入 API 是一个文件上传的请求（类型为 `multipart/form-data` 的 POST 请求）。

示例：

```bash
curl -v -u admin:public -X 'POST' \
    -H 'Content-Type: multipart/form-data' \
    -F 'filename=@/tmp/myusers.csv' \
    'http://localhost:18083/api/v5/authentication/password_based%3Abuilt_in_database/import_users'
```

支持导入 csv 与 JSON 格式文件：

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
