| 字段名称     | 类型 | 描述                                                         |
| ------------ | ---- | ------------------------------------------------------------ |
| time         | 整数 | 时间戳，表示日志记录的时间，以微秒为单位。                   |
| level        | 字符 | 日志级别。                                                   |
| msg          | 字符 | 操作描述。                                                   |
| from         | 字符 | 请求来源，`dashboard`、`rest_api` 分别表示来自 Dashboard、REST API。当值为 `cli`, `erlang_console` 时表示来自 CLI 以及 Erlang Shell 的操作，不适用此日志结构。 |
| node         | 字符 | 节点名称，表示执行操作的节点或服务器。                       |
| source     | 字符 | 执行操作的 Dashboard 用户名或 API 密钥名称。           |
| method       | 字符 | HTTP 请求方法，`post`, `put`, `delete` 对应创建、更新、删除操作。 |
| operate_id   | 字符 | 请求的 REST API 路径，请参考 [REST API](../admin/api.md)。   |
| bindings     | 对象 | 具体的请求对象信息，对应 `operate_id` 中的占位符。           |
| auth_type    | 字符 | 认证类型，表示用于身份验证的方法或机制，固定为 `jwt_token`(Dashboard) 或 `api_key`(REST API)。 |
| query_string | 对象 | HTTP 请求中的 URL 查询参数。                                 |
| code         | 整数 | HTTP 响应码，表示操作的结果状态。                            |
| headers      | 对象 | HTTP 请求头信息，包括客户端标识、请求来源等。                |
| duration_ms  | 整数 | 操作执行时间，以毫秒为单位。                                 |
| body         | 对象 | HTTP 请求体，包含操作的详细信息。                            |
