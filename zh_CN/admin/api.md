# REST API

EMQX 提供了管理监控 REST API，这些 API 遵循 OpenAPI (Swagger) 3.0 规范。

EMQX 服务启动后，您可以访问 [http://localhost:18083/api-docs/index.html](http://localhost:18083/api-docs/index.html) 来查看 API 的文档。还可以直接在 Swagger UI 上尝试执行一些 API。

本章节将指导您快速开始使用 EMQX REST API。

## 基本路径

EMQX 在 REST API 上做了版本控制，EMQX 5.0.0 以后的所有 API 调用均以 `/api/v5` 开头。

## 认证

EMQX 的 REST API 使用 [HTTP Basic 认证](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication#%E9%80%9A%E7%94%A8%E7%9A%84_http_%E8%AE%A4%E8%AF%81%E6%A1%86%E6%9E%B6) 携带认证凭据，您可以在 Dashboard **系统设置** -> **API 密钥** 界面中创建用于认证的 API 密钥，详细操作请参考 [Dashboard - API 密钥](../dashboard/system.md#api-密钥)

## HTTP 请求头

除非有特殊说明，绝大多数 API 要求请求头中 `Accept` 值设置为 `application/json`，响应内容将以 JSON 格式返回。

## HTTP 响应状态码

EMQX 遵循[HTTP 响应状态码](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)标准，可能的状态码如下：

| 状态码 | 描述                                                                          |
| ------ | ----------------------------------------------------------------------------- |
| 200    | 请求成功，返回的 JSON 数据将提供更多信息                                      |
| 201    | 创建成功，新建的对象将在 Body 中返回                                          |
| 204    | 请求成功，常用于删除与更新操作，Body 不会返回内容                             |
| 400    | 请求无效，例如请求体或参数错误                                                |
| 401    | 未通过服务端认证，API 密钥过期或不存在时可能会发生                            |
| 403    | 无权操作，检查操作对象是否正在使用或有依赖约束                                |
| 404    | 找不到请求路径或请求的对象不存在，可参照 Body 中的 `message` 字段判断具体原因 |
| 409    | 请求的资源已存在或数量超过限制                                                |
| 500    | 服务端处理请求时发生内部错误，可通过 Body 返回内容与日志判断具体原因          |

## 错误码

HTTP 响应状态码能够直观的判断可能存在的问题，在此基础上 EMQX 定义了一系列的错误码来标识具体的错误原因。当发生错误时，错误码将通过 Body 以 JSON 格式返回，您可以根据错误码 `code` 了解错误分类，根据原因 `reason` 了解具体的错误信息：

```bash
# GET /clients/foo

{
  "code": "RESOURCE_NOT_FOUND",
  "reason": "Client id not found"
}
```

| 错误码                                         | 描述                                                         |
| ---------------------------------------------- | ------------------------------------------------------------ |
| WRONG_USERNAME_OR_PWD                          | Wrong username or password <img width=200/>                  |
| WRONG_USERNAME_OR_PWD_OR_API_KEY_OR_API_SECRET | Wrong username & password or key & secret                    |
| BAD_REQUEST                                    | Request parameters not legal                                 |
| NOT_MATCH                                      | Conditions not matched                                       |
| ALREADY_EXISTS                                 | Resources already exist                                      |
| BAD_CONFIG_SCHEMA                              | Configuration data not legal                                 |
| BAD_LISTENER_ID                                | Bad listener ID                                              |
| BAD_NODE_NAME                                  | Bad Node Name                                                |
| BAD_RPC                                        | RPC Failed. Check the cluster status and the requested node status |
| BAD_TOPIC                                      | Topic syntax error, topic needs to comply with the MQTT protocol standard |
| EXCEED_LIMIT                                   | Resources to be created exceed the maximum limit or minimum limit |
| INVALID_PARAMETER                              | Request parameters not legal and exceed the boundary value   |
| CONFLICT                                       | Conflicting request resources                                |
| NO_DEFAULT_VALUE                               | Request parameters do not use default values                 |
| DEPENDENCY_EXISTS                              | Resource depends on other resources                          |
| MESSAGE_ID_SCHEMA_ERROR                        | Message ID parsing error                                     |
| INVALID_ID                                     | Bad ID schema                                                |
| MESSAGE_ID_NOT_FOUND                           | Message ID does not exist                                    |
| NOT_FOUND                                      | Resource not found or does not exist                         |
| CLIENTID_NOT_FOUND                             | Client ID not found or does not exist                        |
| CLIENT_NOT_FOUND                               | Client not found or does not exist(usually not an MQTT client) |
| RESOURCE_NOT_FOUND                             | Resource not found                                           |
| TOPIC_NOT_FOUND                                | Topic not found                                              |
| USER_NOT_FOUND                                 | User not found                                               |
| INTERNAL_ERROR                                 | Server inter error                                           |
| SERVICE_UNAVAILABLE                            | Service unavailable                                          |
| SOURCE_ERROR                                   | Source error                                                 |
| UPDATE_FAILED                                  | Update fails                                                 |
| REST_FAILED                                    | Reset source or configuration fails                          |
| CLIENT_NOT_RESPONSE                            | Client not responding                                        |
