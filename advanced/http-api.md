# HTTP API {#http-api}

EMQ X 提供了 HTTP API 以实现与外部系统的集成，例如查询客户端信息、发布消息和创建规则等。

EMQ X 的 HTTP API 服务默认监听 8081 端口，可通过 `etc/plugins/emqx_management.conf` 配置文件修改监听端口，或启用 HTTPS 监听。[EMQ X 4.0.0](https://github.com/emqx/emqx/releases/tag/v4.0.0) 以后的所有 API 调用均以 `api/v4` 开头。

## 接口安全 {#http-api-security}

EMQ X 的 HTTP API 使用 [Basic 认证](https://en.wikipedia.org/wiki/Basic_access_authentication) 方式，`id` 和 `password` 须分别填写 AppID 和 AppSecret。
默认的 AppID 和 AppSecret 是：`amdin/public`。你可以在 Dashboard 的左侧菜单栏里，选择 "管理" -> "应用" 来修改和添加 AppID/AppSecret。

## 响应码 {#codes}

### HTTP 状态码 (status codes)

EMQ X 接口在调用成功时总是返回 200 OK，响应内容则以 JSON 格式返回。

可能的状态码如下：

| Status Code | Description |
| ---- | ----------------------- |
| 200  | 成功，返回的 JSON 数据将提供更多信息 |
| 400  | 客户端请求无效，例如请求体或参数错误 |
| 401  | 客户端未通过服务端认证，使用无效的身份验证凭据可能会发生 |
| 404  | 找不到请求的路径或者请求的对象不存在 |
| 500  | 服务端处理请求时发生内部错误 |

### 返回码 (result codes)

EMQ X 接口的响应消息体为 JSON 格式，其中总是包含返回码 `code`。

可能的返回码如下：

| Return Code | Description |
| ---- | ----------------------- |
| 0    | 成功 |
| 101  | RPC 错误 |
| 102  | 未知错误 |
| 103  | 用户名或密码错误 |
| 104  | 空用户名或密码 |
| 105  | 用户不存在 |
| 106  | 管理员账户不可删除 |
| 107  | 关键请求参数缺失 |
| 108  | 请求参数错误 |
| 109  | 请求参数不是合法 JSON 格式 |
| 110  | 插件已开启 |
| 111  | 插件已关闭 |
| 112  | 客户端不在线 |
| 113  | 用户已存在 |
| 114  | 旧密码错误  |
| 115  | 不合法的主题 |

## API Endpoints {#http-endpoints}

### /api/v4 {#endpoint-api-v4}

#### GET /api/v4 {#endpoint-get-api-v4}

返回 EMQ X 支持的所有 Endpoints。

**Parameters:** 无

**Success Response Body (JSON):**

|       Name       | Type |  Description   |
| ---------------- | --------- | -------------- |
| code             | Integer   | 0              |
| data             | Array     | Endpoints 列表 |
| - data[0].path   | String    | Endpoint       |
| - data[0].name   | String    | Endpoint 名    |
| - data[0].method | String    | HTTP Method    |
| - data[0].descr  | String    | 描述           |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4"

{"data":[{"path":"/auth_clientid","name":"list_clientid","method":"GET","descr":"List available clientid in the cluster"}, ...],"code":0}
```

### Broker 基本信息 {#endpoint-brokers}

#### GET /api/v4/brokers/{node} {#endpoint-get-brokers}

返回集群下所有节点的基本信息。

**Path Parameters:**

| Name | Type | Required | Description |
| ---- | --------- | ------------| ----------- |
| node | String    | False       | 节点名字，如 "emqx@127.0.0.1。<br/>不指定时返回所有节点的信息 |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Object/Array of Objects | node 参数存在时返回指定节点信息，<br/>不存在时返回所有节点的信息|
| data.datetime    | String    | 当前时间，格式为 "YYYY-MM-DD HH:mm:ss"                       |
| data.node        | String    | 节点名称                                                     |
| data.node_status | String    | 节点状态                                                     |
| data.otp_release | String    | EMQ X 使用的 Erlang/OTP 版本                          |
| data.sysdescr    | String    | 软件描述                                                     |
| data.uptime      | String    | EMQ X 运行时间，格式为 "H hours, m minutes, s seconds" |
| data.version     | String    | EMQ X 版本                                           |

**Examples:**

获取所有节点的基本信息：

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/brokers"

{"data":[{"version":"develop","uptime":"4 hours, 21 minutes, 19 seconds","sysdescr":"EMQ X Broker","otp_release":"R21/10.3.5","node_status":"Running","node":"emqx@127.0.0.1","datetime":"2020-02-19 15:27:24"}],"code":0}
```

获取节点 emqx@127.0.0.1 的基本信息：

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/brokers/emqx@127.0.0.1"

{"data":{"version":"develop","uptime":"1 minutes, 51 seconds","sysdescr":"EMQ X Broker","otp_release":"R21/10.3.5","node_status":"Running","node":"emqx@127.0.0.1","datetime":"2020-02-20 14:11:31"},"code":0}
```

### 节点 {#endpoint-nodes}

#### GET /api/v4/nodes/{node} {#endpoint-get-nodes}

返回节点的状态。

**Path Parameters:**

| Name | Type | Required | Description |
| ---- | --------- | ------------| ----------- |
| node | String    | False       | 节点名字，如 "emqx@127.0.0.1。<br/>不指定时返回所有节点的信息 |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Object/Array of Objects | node 参数存在时返回指定节点信息，<br/>不存在时以 Array 形式返回所有节点的信息|
| data.connections       | Integer   | 当前接入此节点的客户端数量          |
| data.load1             | String    | 1 分钟内的 CPU 平均负载             |
| data.load5             | String    | 5 分钟内的 CPU 平均负载             |
| data.load15            | String    | 15 分钟内的 CPU 平均负载            |
| data.max_fds           | Integer   | 操作系统的最大文件描述符限制        |
| data.memory_total      | String    | VM 已分配的系统内存                 |
| data.memory_used       | String    | VM 已占用的内存大小                 |
| data.node              | String    | 节点名称                            |
| data.node_status       | String    | 节点状态                            |
| data.otp_release       | String    | EMQ X 使用的 Erlang/OTP 版本 |
| data.process_available | Integer   | 可用的进程数量                      |
| data.process_used      | Integer   | 已占用的进程数量                    |
| data.uptime            | String    | EMQ X 运行时间               |
| data.version           | String    | EMQ X 版本                   |

**Examples:**

获取所有节点的状态：

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes"

{"data":[{"version":"develop","uptime":"7 seconds","process_used":315,"process_available":2097152,"otp_release":"R21/10.3.5","node_status":"Running","node":"emqx@127.0.0.1","memory_used":"96.75M","memory_total":"118.27M","max_fds":10240,"load5":"2.60","load15":"2.65","load1":"2.31","connections":0}],"code":0}
```

获取指定节点的状态：

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1"

{"data":{"version":"develop","uptime":"2 minutes, 21 seconds","process_used":310,"process_available":2097152,"otp_release":"R21/10.3.5","node_status":"Running","node":"emqx@127.0.0.1","memory_used":101379168,"memory_total":123342848,"max_fds":10240,"load5":"2.50","load15":"2.61","load1":"1.99","connections":0},"code":0}
```

### 客户端 {#endpoint-clients}

#### GET /api/v4/clients {#endpoint-get-clients}

返回集群下所有客户端的信息，支持分页。

**Query String Parameters:**

| Name   | Type | Required | Default | Description |
| ------ | --------- | -------- | ------- |  ---- |
| _page  | Integer   | False | 1       | 页码 |
| _limit | Integer   | False | 10000   | 每页显示的数据条数，未指定时由 `emqx-management` 插件的配置项 `max_row_limit` 决定 |


在 4.1 后，支持多条件和模糊查询，其包含的查询参数有：

| Name              | Type   | Required |  Description |
| ----------------- | ------ | -------- | ------------ |
| clientid          | String | False    | 客户端标识符 |
| username          | String | False    | 客户端用户名 |
| zone              | String | False    | 客户端配置组名称 |
| ip_address        | String | False    | 客户端 IP 地址  |
| conn_state        | Enum   | False    | 客户端当前连接状态，<br />可取值有：`connected`,`idle`,`disconnected` |
| clean_start       | Bool   | False    | 客户端是否使用了全新的会话 |
| proto_name        | Enum   | False    | 客户端协议名称，<br />可取值有：`MQTT`,`CoAP`,`LwM2M`,`MQTT-SN` |
| proto_ver         | Integer| False    | 客户端协议版本 |
| _like_clientid    | String | False    | 客户端标识符，子串方式模糊查找 |
| _like_username    | String | False    | 客户端用户名，子串方式模糊查找 |
| _gte_created_at   | Integer| False    | 客户端会话创建时间，小于等于查找 |
| _lte_created_at   | Integer| False    | 客户端会话创建时间，大于等于查找 |
| _gte_connected_at | Integer| False    | 客户端连接创建时间，小于等于查找 |
| _lte_connected_at | Integer| False    | 客户端连接创建时间，大于等于查找 |


**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | 所有客户端的信息|
| data[0].node              | String    | 客户端所连接的节点名称 |
| data[0].clientid          | String    | 客户端标识符 |
| data[0].username          | String    | 客户端连接时使用的用户名 |
| data[0].proto_name        | String    | 客户端协议名称 |
| data[0].proto_ver         | Integer   | 客户端使用的协议版本 |
| data[0].ip_address        | String    | 客户端的 IP 地址 |
| data[0].port              | Integer   | 客户端的端口 |
| data[0].is_bridge         | Boolean   | 指示客户端是否通过桥接方式连接 |
| data[0].connected_at      | String    | 客户端连接时间，格式为 "YYYY-MM-DD HH:mm:ss" |
| data[0].disconnected_at   | String    | 客户端离线时间，格式为 "YYYY-MM-DD HH:mm:ss"，<br/>此字段仅在 `connected` 为 `false` 时有效并被返回 |
| data[0].connected         | Boolean   | 客户端是否处于连接状态 |
| data[0].zone              | String    | 指示客户端使用的配置组 |
| data[0].keepalive         | Integer   | 保持连接时间，单位：秒 |
| data[0].clean_start       | Boolean   | 指示客户端是否使用了全新的会话 |
| data[0].expiry_interval   | Integer   | 会话过期间隔，单位：秒 |
| data[0].created_at        | String    | 会话创建时间，格式为 "YYYY-MM-DD HH:mm:ss" |
| data[0].subscriptions_cnt | Integer   | 此客户端已建立的订阅数量 |
| data[0].max_subscriptions | Integer   | 此客户端允许建立的最大订阅数量 |
| data[0].inflight          | Integer   | 飞行队列当前长度 |
| data[0].max_inflight      | Integer   | 飞行队列最大长度 |
| data[0].mqueue_len        | Integer   | 消息队列当前长度 |
| data[0].max_mqueue        | Integer   | 消息队列最大长度 |
| data[0].mqueue_dropped    | Integer   | 消息队列因超出长度而丢弃的消息数量 |
| data[0].awaiting_rel      | Integer   | 未确认的 PUBREC 报文数量 |
| data[0].max_awaiting_rel  | Integer   | 允许存在未确认的 PUBREC 报文的最大数量 |
| data[0].recv_oct          | Integer   | EMQ X Broker（下同）接收的字节数量 |
| data[0].recv_cnt          | Integer   | 接收的 TCP 报文数量 |
| data[0].recv_pkt          | Integer   | 接收的 MQTT 报文数量 |
| data[0].recv_msg          | Integer   | 接收的 PUBLISH 报文数量 |
| data[0].send_oct          | Integer   | 发送的字节数量 |
| data[0].send_cnt          | Integer   | 发送的 TCP 报文数量 |
| data[0].send_pkt          | Integer   | 发送的 MQTT 报文数量 |
| data[0].send_msg          | Integer   | 发送的 PUBLISH 报文数量 |
| data[0].mailbox_len       | Integer   | 进程邮箱大小 |
| data[0].heap_size         | Integer   | 进程堆栈大小，单位：字节 |
| data[0].reductions        | Integer   | Erlang reduction |
| meta       | Object    | 分页信息 |
| meta.page  | Integer   | 页码                 |
| meta.limit | Integer   | 每页显示的数据条数 |
| meta.count | Integer   | 数据总条数         |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/clients?_page=1&_limit=10"

{"meta":{"page":1,"limit":10,"count":1},"data":[{"zone":"external","recv_cnt":2,"max_mqueue":1000,"node":"emqx@127.0.0.1","username":"test","mqueue_len":0,"max_inflight":32,"is_bridge":false,"mqueue_dropped":0,"inflight":0,"heap_size":2586,"max_subscriptions":0,"proto_name":"MQTT","created_at":"2020-02-19 17:01:26","proto_ver":4,"reductions":3997,"send_msg":0,"ip_address":"127.0.0.1","send_cnt":0,"mailbox_len":1,"awaiting_rel":0,"keepalive":60,"recv_msg":0,"send_pkt":0,"recv_oct":29,"clientid":"example","clean_start":true,"expiry_interval":0,"connected":true,"port":64491,"send_oct":0,"recv_pkt":1,"connected_at":"2020-02-19 17:01:26","max_awaiting_rel":100,"subscriptions_cnt":0}],"code":0}
```

注：在 4.1 后，返回的 `meta` 内容做了修改：

- `count`：仍表示总数，但在 多条件/模糊查询时，固定为 -1。
- `hasnext`：为新增字段，表示是否存在下一页。

#### GET /api/v4/clients/{clientid} {#endpoint-get-a-client}

返回指定客户端的信息

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| clientid  | String | True | ClientID |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | 客户端的信息，详细请参见<br/>[GET /api/v4/clients](#endpoint-get-clients)|

**Examples:**

查询指定客户端

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/clients/example"

{"data":[{"recv_cnt":2,"max_subscriptions":0,"node":"emqx@127.0.0.1","proto_ver":4,"recv_pkt":1,"inflight":0,"max_mqueue":1000,"heap_size":2586,"username":"test","proto_name":"MQTT","subscriptions_cnt":0,"send_pkt":0,"created_at":"2020-02-20 13:38:51","reductions":3978,"ip_address":"127.0.0.1","send_msg":0,"send_cnt":0,"expiry_interval":0,"keepalive":60,"mqueue_dropped":0,"is_bridge":false,"max_inflight":32,"recv_msg":0,"max_awaiting_rel":100,"awaiting_rel":0,"mailbox_len":1,"mqueue_len":0,"recv_oct":29,"connected_at":"2020-02-20 13:38:51","clean_start":true,"clientid":"example","connected":true,"port":54889,"send_oct":0,"zone":"external"}],"code":0}
```

#### DELETE /api/v4/clients/{clientid} {#endpoint-delete-a-client}

踢除指定客户端。注意踢除客户端操作会将连接与会话一并终结。

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| clientid  | String | True | ClientID |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |

**Examples:**

踢除指定客户端

```bash
$ curl -i --basic -u admin:public -X DELETE "http://localhost:8081/api/v4/clients/example"

{"code":0}
```

#### GET /api/v4/nodes/{node}/clients {#endpoint-nodes-clients}

类似 [GET /api/v4/clients](#endpoint-get-clients)，返回指定节点下所有客户端的信息，支持分页。

**Query String Parameters:**

| Name   | Type | Required | Default | Description |
| ------ | --------- | -------- | ------- |  ---- |
| _page  | Integer   | False | 1       | 页码 |
| _limit | Integer   | False | 10000   | 每页显示的数据条数，未指定时由 `emqx-management` 插件的配置项 `max_row_limit` 决定 |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | 所有客户端的信息，详情请参看 [GET /api/v4/clients](#endpoint-get-clients) |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/clients?_page=1&_limit=10"

{"meta":{"page":1,"limit":10,"count":1},"data":[{"recv_cnt":2,"max_subscriptions":0,"node":"emqx@127.0.0.1","proto_ver":4,"recv_pkt":1,"inflight":0,"max_mqueue":1000,"heap_size":2586,"username":"test","proto_name":"MQTT","subscriptions_cnt":0,"send_pkt":0,"created_at":"2020-02-19 18:25:18","reductions":4137,"ip_address":"127.0.0.1","send_msg":0,"send_cnt":0,"expiry_interval":0,"keepalive":60,"mqueue_dropped":0,"is_bridge":false,"max_inflight":32,"recv_msg":0,"max_awaiting_rel":100,"awaiting_rel":0,"mailbox_len":1,"mqueue_len":0,"recv_oct":29,"connected_at":"2020-02-19 18:25:18","clean_start":true,"clientid":"example","connected":true,"port":49509,"send_oct":0,"zone":"external"}],"code":0}
```

#### GET /api/v4/nodes/{node}/clients/{clientid} {#endpoint-nodes-get-a-client}

类似 [GET /api/v4/clients/{clientid}](#endpoint-get-a-client)，返回指定节点下指定客户端的信息。

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| clientid  | String | True | ClientID |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Object | 客户端的信息，详细请参见<br/>[GET /api/v4/clients](#endpoint-get-clients)|

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/clients/example"

{"data":[{"recv_cnt":4,"max_subscriptions":0,"node":"emqx@127.0.0.1","proto_ver":4,"recv_pkt":1,"inflight":0,"max_mqueue":1000,"heap_size":2586,"username":"test","proto_name":"MQTT","subscriptions_cnt":0,"send_pkt":3,"created_at":"2020-02-20 13:38:51","reductions":5994,"ip_address":"127.0.0.1","send_msg":0,"send_cnt":3,"expiry_interval":0,"keepalive":60,"mqueue_dropped":0,"is_bridge":false,"max_inflight":32,"recv_msg":0,"max_awaiting_rel":100,"awaiting_rel":0,"mailbox_len":0,"mqueue_len":0,"recv_oct":33,"connected_at":"2020-02-20 13:38:51","clean_start":true,"clientid":"example","connected":true,"port":54889,"send_oct":8,"zone":"external"}],"code":0}
```

#### DELETE /api/v4/nodes/{node}/clients/{clientid} {#endpoint-nodes-delete-a-client}

类似 [DELETE /api/v4/clients/{clientid}](#endpoint-delete-a-client)，踢除指定节点下的指定客户端。

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| clientid  | String | True | ClientID |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X DELETE "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/clients/example"

{"code":0}
```

#### GET /api/v4/clients/username/{username} {#endpoint-get-clients-by-username}

通过 Username 查询客户端的信息。由于可能存在多个客户端使用相同的用户名的情况，所以可能同时返回多个客户端信息。

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| username  | String | True | Username |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | 客户端的信息，详细请参见<br/>[GET /api/v4/clients](#endpoint-get-clients)|

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/clients/username/steve"

{"data":[{"clean_start":true,"awaiting_rel":0,"recv_msg":0,"proto_name":"MQTT","recv_cnt":2,"mailbox_len":0,"node":"emqx@127.0.0.1","mqueue_len":0,"max_subscriptions":0,"created_at":"2020-02-20 13:50:11","is_bridge":false,"heap_size":2586,"proto_ver":4,"subscriptions_cnt":0,"clientid":"example","expiry_interval":0,"send_msg":0,"inflight":0,"reductions":4673,"send_pkt":1,"zone":"external","send_cnt":1,"ip_address":"127.0.0.1","keepalive":60,"max_inflight":32,"recv_oct":29,"recv_pkt":1,"max_awaiting_rel":100,"username":"steve","connected_at":"2020-02-20 13:50:11","connected":true,"port":56429,"send_oct":4,"mqueue_dropped":0,"max_mqueue":1000}],"code":0}
```

#### GET /api/v4/nodes/{node}/clients/username/{username} {#endpoint-nodes-get-clients-by-username}

类似 [GET /api/v4/clients/username/{username}](#endpoint-get-clients-by-username)，在指定节点下，通过 Username 查询指定客户端的信息。

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| username  | String | True | Username |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | 客户端的信息，详细请参见<br/>[GET /api/v4/clients](#endpoint-get-clients)|

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/clients/username/test"

{"data":[{"clean_start":true,"awaiting_rel":0,"recv_msg":0,"proto_name":"MQTT","recv_cnt":6,"mailbox_len":0,"node":"emqx@127.0.0.1","mqueue_len":0,"max_subscriptions":0,"created_at":"2020-02-20 13:50:11","is_bridge":false,"heap_size":1598,"proto_ver":4,"subscriptions_cnt":0,"clientid":"example","expiry_interval":0,"send_msg":0,"inflight":0,"reductions":7615,"send_pkt":5,"zone":"external","send_cnt":5,"ip_address":"127.0.0.1","keepalive":60,"max_inflight":32,"recv_oct":37,"recv_pkt":1,"max_awaiting_rel":100,"username":"test","connected_at":"2020-02-20 13:50:11","connected":true,"port":56429,"send_oct":12,"mqueue_dropped":0,"max_mqueue":1000}],"code":0}
```

#### GET /api/v4/clients/{clientid}/acl_cache {#endpoint-get-acl-cache}

查询指定客户端的 ACL 缓存。

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| clientid  | String | True | ClientID |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | ACL 详情|
| data[0].access       | String    | 发布/订阅        |
| data[0].topic        | String    | MQTT 主题        |
| data[0].result       | String    | 允许/拒绝        |
| data[0].updated_time | Integer   | ACL 缓存建立时间 |

**Examples:**

查询 ACL 缓存

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/clients/example/acl_cache"

{"data":[{"updated_time":1582180824571,"topic":"test","result":"allow","access":"publish"}],"code":0}
```

#### DELETE /api/v4/clients/{clientid}/acl_cache {#endpoint-delete-acl-cache}

清除指定客户端的 ACL 缓存。

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| clientid  | String | True | ClientID |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |

**Examples:**

清除 ACL 缓存

```bash
$ curl -i --basic -u admin:public -X DELETE "http://localhost:8081/api/v4/clients/example/acl_cache"

{"code":0}
```

### 订阅信息 {#endpoint-subscriptions}

#### GET /api/v4/subscriptions {#endpoint-get-subscriptions}

返回集群下所有订阅信息，支持分页机制。

**Query String Parameters:**

| Name   | Type | Required | Default | Description |
| ------ | --------- | -------- | ------- |  ---- |
| _page  | Integer   | False | 1       | 页码 |
| _limit | Integer   | False | 10000   | 每页显示的数据条数，未指定时由 `emqx-management` 插件的配置项 `max_row_limit` 决定 |

在 4.1 版本后，支持多条件和模糊查询：

| Name         | Type    | Description |
| ------------ | ------- | ----------- |
| clientid     | String  | 客户端标识符   |
| topic        | String  | 主题，全等查询 |
| qos          | Enum    | 可取值为：`0`,`1`,`2` |
| share        | String  | 共享订阅的组名称 |
| _match_topic | String  | 主题，匹配查询 |


**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | 所有订阅信息|
| data[0].node     | String    | 节点名称     |
| data[0].clientid | String    | 客户端标识符 |
| data[0].topic    | String    | 订阅主题     |
| data[0].qos      | Integer   | QoS 等级     |
| meta | Object    | 同 `/api/v4/clients` |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/subscriptions?_page=1&_limit=10"

{"meta":{"page":1,"limit":10000,"count":2},"data":[{"topic":"a/+/c","qos":0,"node":"emqx@127.0.0.1","clientid":"78082755-e8eb-4a87-bab7-8277541513f0"},{"topic":"a/b/c","qos":1,"node":"emqx@127.0.0.1","clientid":"7a1dfceb-89c0-4f7e-992b-dfeb09329f01"}],"code":0}
```

注：在 4.1 后，返回的 `meta` 内容做了修改：

- `count`：仍表示总数，但在 多条件/模糊查询 时，固定为 -1。
- `hasnext`：为新增字段，表示是否存在下一页。


#### GET /api/v4/subscriptions/{clientid} {#endpoint-get-subscriptions-by-clientid}

返回集群下指定客户端的订阅信息。

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| clientid  | String | True | ClientID |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Object | 所有订阅信息|
| data.node     | String    | 节点名称     |
| data.clientid | String    | 客户端标识符 |
| data.topic    | String    | 订阅主题     |
| data.qos      | Integer   | QoS 等级     |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/subscriptions/123"

{"data":[{"topic":"a/b/c","qos":1,"node":"emqx@127.0.0.1","clientid":"123"}],"code":0}
```

#### GET /api/v4/nodes/{node}/subscriptions {#endpoint-nodes-get-subscriptions}

类似 [GET /api/v4/subscriptions](#endpoint-get-subscriptions)，返回指定节点下的所有订阅信息，支持分页机制。

**Query String Parameters:**

| Name   | Type | Required | Default | Description |
| ------ | --------- | -------- | ------- |  ---- |
| _page  | Integer   | False | 1       | 页码 |
| _limit | Integer   | False | 10000   | 每页显示的数据条数，未指定时由 `emqx-management` 插件的配置项 `max_row_limit` 决定 |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | 所有订阅信息|
| data[0].node     | String    | 节点名称     |
| data[0].clientid | String    | 客户端标识符 |
| data[0].topic    | String    | 订阅主题     |
| data[0].qos      | Integer   | QoS 等级     |
| meta | Object    | 同 `/api/v4/clients` |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/subscriptions?_page=1&limit=10"

{"meta":{"page":1,"limit":10000,"count":2},"data":[{"topic":"a/+/c","qos":0,"node":"emqx@127.0.0.1","clientid":"78082755-e8eb-4a87-bab7-8277541513f0"},{"topic":"a/b/c","qos":1,"node":"emqx@127.0.0.1","clientid":"7a1dfceb-89c0-4f7e-992b-dfeb09329f01"}],"code":0}
```

#### GET /api/v4/nodes/{node}/subscriptions/{clientid} {#endpoint-nodes-get-subscriptions-by-clientid}

类似 [GET /api/v4/subscriptions/{clientid}](#endpoint-get-subscriptions-by-clientid)，在指定节点下，查询某 clientid 的所有订阅信息，支持分页机制。

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| clientid  | String | True | ClientID |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Object | 所有订阅信息|
| data.node     | String    | 节点名称     |
| data.clientid | String    | 客户端标识符 |
| data.topic    | String    | 订阅主题     |
| data.qos      | Integer   | QoS 等级     |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/subscriptions/sample"

{"data":[{"topic":"a/+/c","qos":0,"node":"emqx@127.0.0.1","clientid":"sample"}],"code":0}
```

### 路由 {#endpoint-routes}

#### GET /api/v4/routes {#endpoint-get-routes}

返回集群下的所有路由信息，支持分页机制。

**Query String Parameters:**

| Name   | Type | Required | Default | Description |
| ------ | --------- | -------- | ------- |  ---- |
| _page  | Integer   | False | 1       | 页码 |
| _limit | Integer   | False | 10000   | 每页显示的数据条数，未指定时由 `emqx-management` 插件的配置项 `max_row_limit` 决定 |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | 所有路由信息|
| data[0].topic | String    | MQTT 主题   |
| data[0].node  | String    | 节点名称    |
| meta | Object    | 同 `/api/v4/clients` |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/routes"

{"meta":{"page":1,"limit":10000,"count":2},"data":[{"topic":"a/+/c","node":"emqx@127.0.0.1"},{"topic":"a/b/c","node":"emqx@127.0.0.1"}],"code":0}
```

#### GET /api/v4/routes/{topic} {#endpoint-get-routes-by-topic}

返回集群下指定主题的路由信息。

**Path Parameters:**

| Name   | Type | Required | Description |
| ------ | --------- | -------- |  ---- |
| topic  | Integer   | True | 主题 |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Object | 所有路由信息|
| data.topic | String    | MQTT 主题   |
| data.node  | String    | 节点名称    |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/routes/a%2fb%2fc"

{"data":[{"topic":"a/b/c","node":"emqx@127.0.0.1"}],"code":0}
```

### 消息发布 {#endpoint-publish}

#### POST /api/v4/mqtt/publish {#endpoint-do-publish}

发布 MQTT 消息。

**Parameters (json):**

| Name     | Type | Required | Default | Description |
| -------- | --------- | -------- | ------- | --------- |
| topic    | String    | Optional |         | 主题，与 `topics` 至少指定其中之一 |
| topics   | String    | Optional |         | 以 `,` 分割的多个主题，使用此字段能够同时发布消息到多个主题 |
| clientid | String    | Required |         | 客户端标识符 |
| payload  | String    | Required |         | 消息正文 |
| encoding | String    | Optional | plain   | 消息正文使用的编码方式，目前仅支持 `plain` 与 `base64` 两种 |
| qos      | Integer   | Optional | 0       | QoS 等级 |
| retain   | Boolean   | Optional | false   | 是否为保留消息 |

**Success Response Body (JSON):**

| Name               | Type | Description   |
| ------------------ | --------- | ------------- |
| code               | Integer   | 0  |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/mqtt/publish" -d '{"topic":"a/b/c","payload":"Hello World","qos":1,"retain":false,"clientid":"example"}'

{"code":0}
```

### 主题订阅 {#endpoint-subscribe}

#### POST /api/v4/mqtt/subscribe {#endpoint-do-subscribe}

订阅 MQTT 主题。

**Parameters (json):**

| Name     | Type | Required | Default | Description |
| -------- | --------- | -------- | ------- | ------------ |
| topic    | String    | Optional |         | 主题，与 `topics` 至少指定其中之一 |
| topics   | String    | Optional |         | 以 `,` 分割的多个主题，使用此字段能够同时订阅多个主题 |
| clientid | String    | Required |         | 客户端标识符 |
| qos      | Integer   | Optional | 0       | QoS 等级 |

**Success Response Body (JSON):**

| Name               | Type | Description   |
| ------------------ | --------- | ------------- |
| code               | Integer   | 0  |

**Examples:**

同时订阅 `a`, `b`, `c` 三个主题

```bash
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/mqtt/subscribe" -d '{"topics":"a,b,c","qos":1,"clientid":"example"}'

{"code":0}
```

#### POST /api/v4/mqtt/unsubscribe {#endpoint-do-unsubscribe}

取消订阅。

**Parameters (json):**

| Name     | Type | Required | Default | Description  |
| -------- | --------- | -------- | ------- | ------------ |
| topic    | String    | Required |         | 主题         |
| clientid | String    | Required |         | 客户端标识符 |

**Success Response Body (JSON):**

| Name               | Type | Description   |
| ------------------ | --------- | ------------- |
| code               | Integer   | 0  |

**Examples:**

取消订阅 `a` 主题

```bash
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/mqtt/unsubscribe" -d '{"topic":"a","qos":1,"clientid":"example"}'

{"code":0}
```

### 消息批量发布 {#endpoint-publish-batch}

#### POST /api/v4/mqtt/publish_batch {#endpoint-do-publish-batch}

批量发布 MQTT 消息。

**Parameters (json):**

| Name         | Type | Required | Default | Description |
| ------------ | --------- | -------- | ------- | --------- |
| [0].topic    | String    | Optional |         | 主题，与 `topics` 至少指定其中之一 |
| [0].topics   | String    | Optional |         | 以 `,` 分割的多个主题，使用此字段能够同时发布消息到多个主题 |
| [0].clientid | String    | Required |         | 客户端标识符 |
| [0].payload  | String    | Required |         | 消息正文 |
| [0].encoding | String    | Optional | plain   | 消息正文使用的编码方式，目前仅支持 `plain` 与 `base64` 两种 |
| [0].qos      | Integer   | Optional | 0       | QoS 等级 |
| [0].retain   | Boolean   | Optional | false   | 是否为保留消息 |

**Success Response Body (JSON):**

| Name               | Type | Description   |
| ------------------ | --------- | ------------- |
| code               | Integer   | 0  |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/mqtt/publish_batch" -d '[{"topic":"a/b/c","payload":"Hello World","qos":1,"retain":false,"clientid":"example"},{"topic":"a/b/c","payload":"Hello World Again","qos":0,"retain":false,"clientid":"example"}]'

{"code":0}
```

### 主题批量订阅 {#endpoint-subscribe-batch}

#### POST /api/v4/mqtt/subscribe_batch {#endpoint-do-subscribe-batch}

批量订阅 MQTT 主题。

**Parameters (json):**

| Name         | Type | Required | Default | Description |
| ------------ | --------- | -------- | ------- | ------------ |
| [0].topic    | String    | Optional |         | 主题，与 `topics` 至少指定其中之一 |
| [0].topics   | String    | Optional |         | 以 `,` 分割的多个主题，使用此字段能够同时订阅多个主题 |
| [0].clientid | String    | Required |         | 客户端标识符 |
| [0].qos      | Integer   | Optional | 0       | QoS 等级 |

**Success Response Body (JSON):**

| Name               | Type | Description   |
| ------------------ | --------- | ------------- |
| code               | Integer   | 0  |

**Examples:**

一次性订阅 `a`, `b`, `c` 三个主题

```bash
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/mqtt/subscribe_batch" -d '[{"topic":"a","qos":1,"clientid":"example"},{"topic":"b","qos":1,"clientid":"example"},{"topic":"c","qos":1,"clientid":"example"}]'

{"code":0}
```

#### POST /api/v4/mqtt/unsubscribe_batch {#endpoint-do-unsubscribe-batch}

批量取消订阅。

**Parameters (json):**

| Name         | Type      | Required | Default | Description  |
| ------------ | --------- | -------- | ------- | ------------ |
| [0].topic    | String    | Required |         | 主题         |
| [0].clientid | String    | Required |         | 客户端标识符 |

**Success Response Body (JSON):**

| Name               | Type | Description   |
| ------------------ | --------- | ------------- |
| code               | Integer   | 0  |

**Examples:**

一次性取消订阅 `a`, `b` 主题

```bash
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/mqtt/unsubscribe_batch" -d '[{"topic":"a","qos":1,"clientid":"example"},{"topic":"b","qos":1,"clientid":"example"}]'

{"code":0}
```

### 插件 {#endpoint-plugins}

#### GET /api/v4/plugins {#endpoint-get-plugins}

返回集群下的所有插件信息。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | 所有路由信息|
| data[0].node    | String    | 节点名称 |
| data[0].plugins | Array     | 插件信息，由对象组成的数组，见下文 |
| data[0].plugins.name        | String    | 插件名称 |
| data[0].plugins.version     | String    | 插件版本 |
| data[0].plugins.description | String    | 插件描述 |
| data[0].plugins.active      | Boolean   | 插件是否启动 |
| data[0].plugins.type        | String    | 插件类型，目前有<br/>`auth`、`bridge`、`feature`、`protocol` 四种类型 |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/plugins"

{"data":[{"plugins":[{"version":"develop","type":"auth","name":"emqx_auth_clientid","description":"EMQ X Authentication with ClientId/Password","active":false}, ...],"node":"emqx@127.0.0.1"}],"code":0}
```

#### GET /api/v4/nodes/{node}/plugins {#endpoint-nodes-get-plugins}

类似 [GET /api/v4/plugins](#endpoint-get-plugins)，返回指定节点下的插件信息。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Array of Objects | 所有路由信息|
| data[0].name        | String    | 插件名称 |
| data[0].version     | String    | 插件版本 |
| data[0].description | String    | 插件描述 |
| data[0].active      | Boolean   | 插件是否启动 |
| data[0].type        | String    | 插件类型，目前有<br/>`auth`、`bridge`、`feature`、`protocol` 四种类型 |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/plugins"

{"data":[{"version":"develop","type":"auth","name":"emqx_auth_clientid","description":"EMQ X Authentication with ClientId/Password","active":false}, ...],"code":0}
```

#### PUT /api/v4/nodes/{node}/plugins/{plugin}/load {#endpoint-nodes-load-plugin}

加载指定节点下的指定插件。

**Parameters:** 无

**Success Response Body (JSON):**

| Name               | Type | Description   |
| ------------------ | --------- | ------------- |
| code               | Integer   | 0  |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X PUT "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/plugins/emqx_delayed_publish/load"

{"code":0}
```

#### PUT /api/v4/nodes/{node}/plugins/{plugin}/unload {#endpoint-nodes-unload-plugin}

卸载指定节点下的指定插件。

**Parameters:** 无

**Success Response Body (JSON):**

| Name               | Type | Description   |
| ------------------ | --------- | ------------- |
| code               | Integer   | 0  |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X PUT "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/plugins/emqx_delayed_publish/unload"

{"code":0}
```

#### PUT /api/v4/nodes/{node}/plugins/{plugin}/reload {#endpoint-nodes-reload-plugin}

重新加载指定节点下的指定插件。

**Parameters:** 无

**Success Response Body (JSON):**

| Name               | Type | Description   |
| ------------------ | --------- | ------------- |
| code               | Integer   | 0  |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X PUT "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/plugins/emqx_delayed_publish/reload"

{"code":0}
```

### 监听器 {#endpoint-listeners}

#### GET /api/v4/listeners {#endpoint-get-listeners}

返回集群下的所有监听器信息。

**Path Parameters:** 无

**Success **Response Body (JSON):**

| Name | Type | Description                                |
| ---- | --------- | ------------------------------------------ |
| code | Integer   | 0 |
| data | Array of Objects | 各节点的监听器列表 |
| data[0].node      | String    | 节点名称 |
| data[0].listeners | Array of Objects   | 监听器列表 |
| data[0].listeners[0].acceptors      | Integer   | Acceptor 进程数量 |
| data[0].listeners[0].listen_on      | String    | 监听端口 |
| data[0].listeners[0].protocol       | String    | 插件描述 |
| data[0].listeners[0].current_conns  | Integer   | 插件是否启动 |
| data[0].listeners[0].max_conns      | Integer   | 允许建立的最大连接数量 |
| data[0].listeners[0].shutdown_count | Array of Objects | 连接关闭原因及计数 |

*常见 shutdown_count*

| Name       | Type | Description                                                  |
| ---------- | --------- | ------------------------------------------------------------ |
| normal     | Integer   | 正常关闭的连接数量，仅在计数大于 0 时返回                    |
| kicked     | Integer   | 被手动踢除的连接数量，仅在计数大于 0 时返回                  |
| discarded  | Integer   | 由于 `Clean Session` 或 `Clean Start` 为 `true` 而被丢弃的连接数量 |
| takeovered | Integer   | 由于 `Clean Session` 或 `Clean Start` 为 `false` 而被接管的连接数量 |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/listeners"

{"data":[{"node":"emqx@127.0.0.1","listeners":[{"shutdown_count":[],"protocol":"mqtt:ssl","max_conns":102400,"listen_on":"8883","current_conns":0,"acceptors":16},{"shutdown_count":[],"protocol":"mqtt:tcp","max_conns":1024000,"listen_on":"0.0.0.0:1883","current_conns":13,"acceptors":8},{"shutdown_count":[],"protocol":"mqtt:tcp","max_conns":1024000,"listen_on":"127.0.0.1:11883","current_conns":0,"acceptors":4},{"shutdown_count":[],"protocol":"http:dashboard","max_conns":512,"listen_on":"18083","current_conns":0,"acceptors":4},{"shutdown_count":[],"protocol":"http:management","max_conns":512,"listen_on":"8081","current_conns":1,"acceptors":2},{"shutdown_count":[],"protocol":"https:dashboard","max_conns":512,"listen_on":"18084","current_conns":0,"acceptors":2},{"shutdown_count":[],"protocol":"mqtt:ws:8083","max_conns":102400,"listen_on":"8083","current_conns":1,"acceptors":4},{"shutdown_count":[],"protocol":"mqtt:wss:8084","max_conns":16,"listen_on":"8084","current_conns":0,"acceptors":4}]}],"code":0}
```

#### GET /api/v4/nodes/{node}/listeners {#endpoint-nodes-get-listeners}

类似 [GET /api/v4/listeners](#endpoint-get-listeners)，返回指定节点的监听器信息。

**Path Parameters:** 无

**Success **Response Body (JSON):**

| Name | Type | Description                                |
| ---- | --------- | ------------------------------------------ |
| code | Integer   | 0 |
| data | Array of Objects | 各节点的监听器列表 |
| data[0].acceptors      | Integer   | Acceptor 进程数量 |
| data[0].listen_on      | String    | 监听端口 |
| data[0].protocol       | String    | 插件描述 |
| data[0].current_conns  | Integer   | 插件是否启动 |
| data[0].max_conns      | Integer   | 允许建立的最大连接数量 |
| data[0].shutdown_count | Array of Objects | 连接关闭原因及计数 |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/listeners"

{"data":[{"shutdown_count":[],"protocol":"mqtt:ssl","max_conns":102400,"listen_on":"8883","current_conns":0,"acceptors":16},{"shutdown_count":[],"protocol":"mqtt:tcp","max_conns":1024000,"listen_on":"0.0.0.0:1883","current_conns":13,"acceptors":8},{"shutdown_count":[],"protocol":"mqtt:tcp","max_conns":1024000,"listen_on":"127.0.0.1:11883","current_conns":0,"acceptors":4},{"shutdown_count":[],"protocol":"http:dashboard","max_conns":512,"listen_on":"18083","current_conns":0,"acceptors":4},{"shutdown_count":[],"protocol":"http:management","max_conns":512,"listen_on":"8081","current_conns":1,"acceptors":2},{"shutdown_count":[],"protocol":"https:dashboard","max_conns":512,"listen_on":"18084","current_conns":0,"acceptors":2},{"shutdown_count":[],"protocol":"mqtt:ws:8083","max_conns":102400,"listen_on":"8083","current_conns":1,"acceptors":4},{"shutdown_count":[],"protocol":"mqtt:wss:8084","max_conns":16,"listen_on":"8084","current_conns":0,"acceptors":4}],"code":0}
```

### 内置模块 {#endpoint-modules}

#### GET /api/v4/modules {#endpoint-get-modules}

返回集群下所有内置模块信息。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description   |
| ---- | --------- | ------------- |
| code | Integer   | 0 |
| data | Array of Objects | 各节点上的内置模块列表 |
| data[0].node    | String    | 节点名称 |
| data[0].modules | Object     | 内置模块信息列表，详见下面的 modules: |

**modules:**

| Name            | Type     | Description |
| --------------- | -------- | ----------- |
| name            | String   | 模块名 |
| description     | String   | 模块功能描述 |
| active          | Boolean  | 是否处于活跃状态（是否正在运行） |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/modules"

{"data":[{"node":"emqx@127.0.0.1","modules":[{"name":"emqx_mod_delayed","description":"EMQ X Delayed Publish Module","active":true},{"name":"emqx_mod_topic_metrics","description":"EMQ X Topic Metrics Module","active":false},{"name":"emqx_mod_subscription","description":"EMQ X Subscription Module","active":false},{"name":"emqx_mod_acl_internal","description":"EMQ X Internal ACL Module","active":true},{"name":"emqx_mod_rewrite","description":"EMQ X Topic Rewrite Module","active":false},{"name":"emqx_mod_presence","description":"EMQ X Presence Module","active":true}]}],"code":0}
```

#### GET /api/v4/nodes/{node}/modules {#endpoint-nodes-get-modules}

类似 [GET /api/v4/modules](#endpoint-get-modules)，返回指定节点下所有内置模块信息。

#### PUT /api/v4/modules/{module}/load {#endpoint-load-module}

加载集群下所有节点的指定内置模块。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name    | Type      | Description                                  |
| ------- | --------- | -------------------------------------------- |
| code    | Integer   | 0 |
| message | String    | 仅在发生错误时返回，用于提供更详细的错误信息 |

```bash
$ curl -i --basic -u admin:public -X PUT "http://localhost:8081/api/v4/modules/emqx_mod_topic_metrics/load"

{"code":0}
```

#### PUT /api/v4/nodes/{node}/modules/{module}/load {#endpoint-nodes-load-module}

类似 [PUT /api/v4/modules/{module}/load](#endpoint-load-module)，加载指定节点下的指定内置模块。

#### PUT /api/v4/modules/{module}/unload {#endpoint-unload-module}

卸载集群下所有节点的指定内置模块。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name    | Type      | Description                                  |
| ------- | --------- | -------------------------------------------- |
| code    | Integer   | 0 |
| message | String    | 仅在发生错误时返回，用于提供更详细的错误信息 |

```bash
$ curl -i --basic -u admin:public -X PUT "http://localhost:8081/api/v4/modules/emqx_mod_topic_metrics/unload"

{"code":0}
```

#### PUT /api/v4/nodes/{node}/modules/{module}/unload {#endpoint-nodes-unload-module}

类似 [PUT /api/v4/modules/{module}/unload](#endpoint-unload-module)，卸载指定节点下的指定内置模块。

#### PUT /api/v4/modules/{module}/reload {#endpoint-reload-module}

重新加载集群下所有节点的指定内置模块，仅为 `emqx_mod_acl_internal` 提供此功能。

| Name    | Type      | Description                                  |
| ------- | --------- | -------------------------------------------- |
| code    | Integer   | 0 |
| message | String    | 仅在发生错误时返回，用于提供更详细的错误信息 |

```bash
$ curl -i --basic -u admin:public -X PUT "http://localhost:8081/api/v4/modules/emqx_mod_acl_internal/reload"

{"code":0}
```

#### PUT /api/v4/nodes/{node}/modules/{module}/reload {#endpoint-nodes-reload-module}

类似 [PUT /api/v4/modules/{module}/reload](#endpoint-reload-module)，重新加载指定节点下的指定内置模块，仅为 `emqx_mod_acl_internal` 提供此功能。

### 统计指标 {#endpoint-metrics}

#### GET /api/v4/metrics {#endpoint-get-metrics}

返回集群下所有统计指标数据。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description   |
| ---- | --------- | ------------- |
| code | Integer   | 0 |
| data | Array of Objects | 各节点上的统计指标列表 |
| data[0].node    | String    | 节点名称 |
| data[0].metrics | Object     | 监控指标数据，详见下面的 metrics：|

**metrics：**

| Name | Type | Description |
| ----------------| --------- | -------------------- |
| actions.failure                 | Integer   | 规则引擎 action 成功失败次数 |
| actions.success                 | Integer   | 规则引擎 action 执行失败次数 |
| bytes.received                  | Integer   | EMQ X 接收的字节数 |
| bytes.sent                      | Integer   | EMQ X 在此连接上发送的字节数 |
| client.authenticate             | Integer   | 客户端认证次数 |
| client.auth.anonymous           | Integer   | 匿名登录的客户端数量 |
| client.connect                  | Integer   | 客户端连接次数 |
| client.connack                  | Integer   | 发送 CONNACK 报文的次数 |
| client.connected                | Integer   | 客户端成功连接次数 |
| client.disconnected             | Integer   | 客户端断开连接次数 |
| client.check_acl                | Integer   | ACL 规则检查次数 |
| client.subscribe                | Integer   | 客户端订阅次数 |
| client.unsubscribe              | Integer   | 客户端取消订阅次数 |
| delivery.dropped.too_large      | Integer   | 发送时由于长度超过限制而被丢弃的消息数量 |
| delivery.dropped.queue_full     | Integer   | 发送时由于消息队列满而被丢弃的 QoS 不为 0 的消息数量 |
| delivery.dropped.qos0_msg       | Integer   | 发送时由于消息队列满而被丢弃的 QoS 为 0 的消息数量 |
| delivery.dropped.expired        | Integer   | 发送时由于消息过期而被丢弃的消息数量 |
| delivery.dropped.no_local       | Integer   | 发送时由于 `No Local` 订阅选项而被丢弃的消息数量 |
| delivery.dropped                | Integer   | 发送时丢弃的消息总数 |
| messages.delayed                | Integer   | EMQ X 存储的延迟发布的消息数量 |
| messages.delivered              | Integer   | EMQ X 内部转发到订阅进程的消息数量 |
| messages.dropped                | Integer   | EMQ X 内部转发到订阅进程前丢弃的消息总数 |
| messages.dropped.expired        | Integer   | 接收时由于消息过期而被丢弃的消息数量 |
| messages.dropped.no_subscribers | Integer   | 由于没有订阅者而被丢弃的消息数量 |
| messages.forward                | Integer   | 向其他节点转发的消息数量 |
| messages.publish                | Integer   | 除系统消息外发布的消息数量 |
| messages.qos0.received          | Integer   | 接收来自客户端的 QoS 0 消息数量 |
| messages.qos2.received          | Integer   | 接收来自客户端的 QoS 1 消息数量 |
| messages.qos1.received          | Integer   | 接收来自客户端的 QoS 2 消息数量 |
| messages.qos0.sent              | Integer   | 发送给客户端的 QoS 0 消息数量 |
| messages.qos1.sent              | Integer   | 发送给客户端的 QoS 1 消息数量 |
| messages.qos2.sent              | Integer   | 发送给客户端的 QoS 2 消息数量 |
| messages.received               | Integer   | 接收来自客户端的消息数量，等于 `messages.qos0.received`，`messages.qos1.received` 与 `messages.qos2.received` 之和 |
| messages.sent                   | Integer   | 发送给客户端的消息数量，等于 `messages.qos0.sent`，`messages.qos1.sent` 与 `messages.qos2.sent` 之和 |
| messages.retained               | Integer   | EMQ X 存储的保留消息数量 |
| messages.acked                  | Integer   | 接收的 PUBACK 和 PUBREC 报文数量 |
| packets.received                | Integer   | 接收的报文数量 |
| packets.sent                    | Integer   | 发送的报文数量 |
| packets.connect.received        | Integer   | 接收的 CONNECT 报文数量 |
| packets.connack.auth_error      | Integer   | 接收的认证失败的 CONNECT 报文数量 |
| packets.connack.error           | Integer   | 接收的未成功连接的 CONNECT 报文数量 |
| packets.connack.sent            | Integer   | 发送的 CONNACK 报文数量 |
| packets.publish.received        | Integer   | 接收的 PUBLISH 报文数量 |
| packets.publish.sent            | Integer   | 发送的 PUBLISH 报文数量 |
| packets.publish.inuse           | Integer   | 接收的报文标识符已被占用的 PUBLISH 报文数量 |
| packets.publish.auth_error      | Integer   | 接收的未通过 ACL 检查的 PUBLISH 报文数量 |
| packets.publish.error           | Integer   | 接收的无法被发布的 PUBLISH 报文数量 |
| packets.publish.dropped         | Integer   | 超出接收限制而被丢弃的消息数量 |
| packets.puback.received         | Integer   | 接收的 PUBACK 报文数量 |
| packets.puback.sent             | Integer   | 发送的 PUBACK 报文数量 |
| packets.puback.inuse            | Integer   | 接收的报文标识符已被占用的 PUBACK 报文数量 |
| packets.puback.missed           | Integer   | 接收的未知报文标识符 PUBACK 报文数量 |
| packets.pubrec.received         | Integer   | 接收的 PUBREC 报文数量 |
| packets.pubrec.sent             | Integer   | 发送的 PUBREC 报文数量 |
| packets.pubrec.inuse            | Integer   | 接收的报文标识符已被占用的 PUBREC 报文数量 |
| packets.pubrec.missed           | Integer   | 接收的未知报文标识符 PUBREC 报文数量 |
| packets.pubrel.received         | Integer   | 接收的 PUBREL 报文数量 |
| packets.pubrel.sent             | Integer   | 发送的 PUBREL 报文数量 |
| packets.pubrel.missed           | Integer   | 接收的未知报文标识符 PUBREL 报文数量 |
| packets.pubcomp.received        | Integer   | 接收的 PUBCOMP 报文数量 |
| packets.pubcomp.sent            | Integer   | 发送的 PUBCOMP 报文数量 |
| packets.pubcomp.inuse           | Integer   | 接收的报文标识符已被占用的 PUBCOMP 报文数量 |
| packets.pubcomp.missed          | Integer   | 发送的 PUBCOMP 报文数量 |
| packets.subscribe.received      | Integer   | 接收的 SUBSCRIBE 报文数量 |
| packets.subscribe.error         | Integer   | 接收的订阅失败的 SUBSCRIBE 报文数量 |
| packets.subscribe.auth_error    | Integer   | 接收的未通过 ACL 检查的 SUBACK 报文数量 |
| packets.suback.sent             | Integer   | 发送的 SUBACK 报文数量 |
| packets.unsubscribe.received    | Integer   | 接收的 UNSUBSCRIBE 报文数量 |
| packets.unsubscribe.error       | Integer   | 接收的取消订阅失败的 UNSUBSCRIBE 报文数量 |
| packets.unsuback.sent           | Integer   | 发送的 UNSUBACK 报文数量 |
| packets.pingreq.received        | Integer   | 接收的 PINGREQ 报文数量 |
| packets.pingresp.sent           | Integer   | 发送的 PUBRESP 报文数量 |
| packets.disconnect.received     | Integer   | 接收的 DISCONNECT 报文数量 |
| packets.disconnect.sent         | Integer   | 发送的 DISCONNECT 报文数量 |
| packets.auth.received           | Integer   | 接收的 AUTH 报文数量 |
| packets.auth.sent               | Integer   | 发送的 AUTH 报文数量 |
| rules.matched                   | Integer   | 规则的匹配次数 |
| session.created                 | Integer   | 创建的会话数量 |
| session.discarded               | Integer   | 由于 `Clean Session` 或 `Clean Start` 为 `true` 而被丢弃的会话数量 |
| session.resumed                 | Integer   | 由于 `Clean Session` 或 `Clean Start` 为 `false` 而恢复的会话数量 |
| session.takeovered              | Integer   | 由于 `Clean Session` 或 `Clean Start` 为 `false` 而被接管的会话数量 |
| session.terminated              | Integer   | 终结的会话数量 |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/metrics"

{"data":[{"node":"emqx@127.0.0.1","metrics":{"messages.dropped.no_subscribers":0,"packets.connack.sent":13,"bytes.received":805,"messages.received":0,"packets.unsuback.sent":0,"messages.delivered":0,"client.disconnected":0,"packets.puback.sent":0,"packets.subscribe.auth_error":0,"delivery.dropped.queue_full":0,"messages.forward":0,"delivery.dropped.qos0_msg":0,"delivery.dropped.expired":0,"bytes.sent":52,"messages.sent":0,"delivery.dropped.no_local":0,"packets.pubrec.received":0,"packets.pubcomp.received":0,"client.check_acl":0,"packets.puback.received":0,"session.takeovered":0,"messages.dropped.expired":0,"actions.success":0,"messages.qos1.sent":0,"messages.retained":0,"packets.pubcomp.inuse":0,"packets.pubrec.sent":0,"packets.received":13,"messages.acked":0,"session.terminated":0,"packets.sent":13,"packets.unsubscribe.error":0,"client.connect":13,"packets.pubrec.missed":0,"packets.auth.sent":0,"packets.disconnect.received":0,"messages.qos2.sent":0,"client.auth.anonymous":13,"packets.auth.received":0,"packets.unsubscribe.received":0,"packets.publish.auth_error":0,"client.connected":13,"rules.matched":0,"packets.disconnect.sent":0,"session.created":13,"packets.pingreq.received":0,"messages.dropped":0,"actions.failure":0,"packets.publish.sent":0,"session.resumed":0,"packets.connack.auth_error":0,"packets.pubrel.sent":0,"delivery.dropped":0,"packets.pubcomp.sent":0,"messages.qos2.received":0,"messages.qos0.received":0,"packets.publish.inuse":0,"client.unsubscribe":0,"packets.pubrel.received":0,"client.connack":13,"packets.connack.error":0,"packets.publish.dropped":0,"packets.publish.received":0,"client.subscribe":0,"packets.subscribe.error":0,"packets.suback.sent":0,"packets.pubcomp.missed":0,"messages.qos1.received":0,"delivery.dropped.too_large":0,"packets.pingresp.sent":0,"packets.pubrel.missed":0,"messages.qos0.sent":0,"packets.connect.received":13,"packets.puback.missed":0,"packets.subscribe.received":0,"packets.puback.inuse":0,"client.authenticate":13,"messages.publish":0,"packets.pubrec.inuse":0,"packets.publish.error":0,"messages.delayed":0,"session.discarded":0}}],"code":0}
```

#### GET /api/v4/nodes/{node}/metrics {#endpoint-nodes-get-metrics}

类似 [GET /api/v4/metrics](#endpoint-get-metrics)，返回指定节点下所有监控指标数据。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description   |
| ---- | --------- | ------------- |
| code | Integer   | 0 |
| data | Object | 各节点上的统计指标列表，详见 [GET /api/v4/metrics](#endpoint-get-metrics) |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/metrics"

{"data":{"bytes.received":0,"client.connected":0,"packets.pingreq.received":0,"messages.delayed":0,"rules.matched":0,"actions.failure":0,"packets.puback.sent":0,"packets.pingresp.sent":0,"packets.publish.auth_error":0,"client.check_acl":0,"delivery.dropped.queue_full":0,"actions.success":0,"packets.publish.error":0,"packets.pubcomp.received":0,"bytes.sent":0,"packets.pubrec.inuse":0,"packets.pubrec.missed":0,"packets.pubrel.sent":0,"delivery.dropped.too_large":0,"packets.pubcomp.missed":0,"packets.subscribe.error":0,"packets.suback.sent":0,"messages.qos2.sent":0,"messages.qos1.sent":0,"packets.pubrel.missed":0,"messages.publish":0,"messages.forward":0,"packets.auth.received":0,"delivery.dropped":0,"packets.sent":0,"packets.puback.inuse":0,"delivery.dropped.qos0_msg":0,"packets.publish.dropped":0,"packets.disconnect.sent":0,"packets.auth.sent":0,"packets.unsubscribe.received":0,"session.takeovered":0,"messages.delivered":0,"client.auth.anonymous":0,"packets.connack.error":0,"packets.connack.sent":0,"packets.subscribe.auth_error":0,"packets.unsuback.sent":0,"packets.pubcomp.sent":0,"packets.publish.sent":0,"client.connack":0,"packets.publish.received":0,"client.subscribe":0,"session.created":0,"delivery.dropped.expired":0,"client.unsubscribe":0,"packets.received":0,"packets.pubrel.received":0,"packets.unsubscribe.error":0,"messages.qos0.sent":0,"packets.connack.auth_error":0,"session.resumed":0,"delivery.dropped.no_local":0,"packets.puback.missed":0,"packets.pubcomp.inuse":0,"packets.pubrec.sent":0,"messages.dropped.expired":0,"messages.dropped.no_subscribers":0,"session.discarded":0,"messages.sent":0,"messages.received":0,"packets.puback.received":0,"messages.qos0.received":0,"messages.acked":0,"client.connect":0,"packets.disconnect.received":0,"client.disconnected":0,"messages.retained":3,"session.terminated":0,"packets.publish.inuse":0,"packets.pubrec.received":0,"messages.qos2.received":0,"messages.dropped":0,"packets.connect.received":0,"client.authenticate":0,"packets.subscribe.received":0,"messages.qos1.received":0},"code":0}
```

### 主题统计指标 {#endpoint-topic-metrics}

#### GET /api/v4/topic-metrics {#endpoint-list-all-topic-metrics}

返回所有主题统计指标数据。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name            | Type             | Description   |
| --------------- | ---------------- | ------------- |
| code            | Integer          | 0             |
| data            | Array of Objects | 各节点上的统计指标列表 |
| data[0].topic   | String           | 主题名 |
| data[0].metrics | Object           | 主题统计指标数据，详见下面的 metrics: |

**metrics: **

| Name                    | Type      | Description |
| ----------------------- | --------- | -------------------- |
| messages.qos2.out.rate  | Integer   | QoS 2 消息 5 秒内平均发送速率 |
| messages.qos2.out.count | Integer   | QoS 2 消息发送数量统计 |
| messages.qos2.in.rate   | Integer   | QoS 2 消息 5 秒内平均接收速率 |
| messages.qos2.in.count  | Integer   | QoS 2 消息接收数量统计 |
| messages.qos1.out.rate  | Integer   | QoS 1 消息 5 秒内平均发送速率 |
| messages.qos1.out.count | Integer   | QoS 1 消息发送数量统计 |
| messages.qos1.in.rate   | Integer   | QoS 1 消息 5 秒内平均接收速率 |
| messages.qos1.in.count  | Integer   | QoS 1 消息接收数量统计 |
| messages.qos0.out.rate  | Integer   | QoS 0 消息 5 秒内平均发送速率 |
| messages.qos0.out.count | Integer   | QoS 0 消息发送数量统计 |
| messages.qos0.in.rate   | Integer   | QoS 0 消息 5 秒内平均接收速率 |
| messages.qos0.in.count  | Integer   | QoS 0 消息接收数量统计 |
| messages.out.rate       | Integer   | MQTT 消息 5 秒内平均发送速率 |
| messages.out.count      | Integer   | MQTT 消息发送数量统计 |
| messages.in.rate        | Integer   | MQTT 消息 5 秒内平均接收速率 |
| messages.in.count       | Integer   | MQTT 消息接收数量统计 |
| messages.dropped.rate   | Integer   | MQTT 消息 5 秒内平均丢弃速率 |
| messages.dropped.count  | Integer   | MQTT 消息丢弃数量统计 |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/topic-metrics"

{"data":[],"code":0}
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/topic-metrics" -d '{"topic":"a/b/c"}'

{"code":0}
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/topic-metrics"

{"data":[{"topic":"a/b/c","metrics":{"messages.qos2.out.rate":0.0,"messages.qos2.out.count":0,"messages.qos2.in.rate":0.0,"messages.qos2.in.count":0,"messages.qos1.out.rate":0.0,"messages.qos1.out.count":0,"messages.qos1.in.rate":0.0,"messages.qos1.in.count":0,"messages.qos0.out.rate":0.0,"messages.qos0.out.count":0,"messages.qos0.in.rate":0.0,"messages.qos0.in.count":0,"messages.out.rate":0.0,"messages.out.count":0,"messages.in.rate":0.0,"messages.in.count":0,"messages.dropped.rate":0.0,"messages.dropped.count":0}}],"code":0}
```

#### GET /api/v4/topic-metrics/{topic} {#endpoint-get-topic-metrics}

返回指定主题的统计指标数据。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type             | Description   |
| ---- | ---------------- | ------------- |
| code | Integer          | 0             |
| data | Object           | 主题统计指标数据，详见下面的 data: |

**data: **

| Name                    | Type      | Description |
| ----------------------- | --------- | -------------------- |
| messages.qos2.out.rate  | Integer   | QoS 2 消息 5 秒内平均发送速率 |
| messages.qos2.out.count | Integer   | QoS 2 消息发送数量统计 |
| messages.qos2.in.rate   | Integer   | QoS 2 消息 5 秒内平均接收速率 |
| messages.qos2.in.count  | Integer   | QoS 2 消息接收数量统计 |
| messages.qos1.out.rate  | Integer   | QoS 1 消息 5 秒内平均发送速率 |
| messages.qos1.out.count | Integer   | QoS 1 消息发送数量统计 |
| messages.qos1.in.rate   | Integer   | QoS 1 消息 5 秒内平均接收速率 |
| messages.qos1.in.count  | Integer   | QoS 1 消息接收数量统计 |
| messages.qos0.out.rate  | Integer   | QoS 0 消息 5 秒内平均发送速率 |
| messages.qos0.out.count | Integer   | QoS 0 消息发送数量统计 |
| messages.qos0.in.rate   | Integer   | QoS 0 消息 5 秒内平均接收速率 |
| messages.qos0.in.count  | Integer   | QoS 0 消息接收数量统计 |
| messages.out.rate       | Integer   | MQTT 消息 5 秒内平均发送速率 |
| messages.out.count      | Integer   | MQTT 消息发送数量统计 |
| messages.in.rate        | Integer   | MQTT 消息 5 秒内平均接收速率 |
| messages.in.count       | Integer   | MQTT 消息接收数量统计 |
| messages.dropped.rate   | Integer   | MQTT 消息 5 秒内平均丢弃速率 |
| messages.dropped.count  | Integer   | MQTT 消息丢弃数量统计 |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/topic-metrics/a%2Fb%2Fc"

{"data":{"messages.qos2.out.rate":0.0,"messages.qos2.out.count":0,"messages.qos2.in.rate":0.0,"messages.qos2.in.count":0,"messages.qos1.out.rate":0.0,"messages.qos1.out.count":0,"messages.qos1.in.rate":0.0,"messages.qos1.in.count":0,"messages.qos0.out.rate":0.0,"messages.qos0.out.count":0,"messages.qos0.in.rate":0.0,"messages.qos0.in.count":0,"messages.out.rate":0.0,"messages.out.count":0,"messages.in.rate":0.0,"messages.in.count":0,"messages.dropped.rate":0.0,"messages.dropped.count":0},"code":0}
```

#### POST /api/v4/topic-metrics {#endpoint-add-topic-metrics}

开启对指定主题的指标统计。

**Parameters (json):**

| Name  | Type      | Required | Default | Description |
| ----- | --------- | -------- | ------- | ----------- |
| topic | String    | Required |         | MQTT 主题名  |

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |

**Examples:**

开启对 `a/b/c` 主题的指标统计 

```bash
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/topic-metrics" -d '{"topic":"a/b/c"}'

{"code":0}
```

#### DELETE /api/v4/topic-metrics/{topic} {#endpoint-delete-topic-metrics}

关闭对指定主题的指标统计。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |

**Examples:**

关闭对 `a/b/c` 主题的指标统计 

```bash
$ curl -i --basic -u admin:public -X DELETE "http://localhost:8081/api/v4/topic-metrics/a%2Fb%2Fc"

{"code":0}
```

#### DELETE /api/v4/topic-metrics {#endpoint-delete-all-topic-metrics}

关闭所有主题的指标统计。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type    | Description |
| ---- | ------- | ----------- |
| code | Integer | 0           |

**Examples:**

关闭所有主题的指标统计 

```bash
$ curl -i --basic -u admin:public -X DELETE "http://localhost:8081/api/v4/topic-metrics"

{"code":0}
```

### 状态 {#endpoint-stats}

#### GET /api/v4/stats {#endpoint-get-stats}

返回集群下所有状态数据。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description   |
| ---- | --------- | ------------- |
| code | Integer   | 0 |
| data | Array of Objects | 各节点上的状态数据列表 |
| data[0].node  | String    | 节点名称 |
| data[0].stats | Array     | 状态数据，详见下面的 *stats* |

**stats：**

| Name                       | Type | Description                |
| -------------------------- | --------- | -------------------------- |
| connections.count          | Integer   | 当前连接数量               |
| connections.max            | Integer   | 连接数量的历史最大值       |
| channels.count             | Integer   | 即 `sessions.count`        |
| channels.max               | Integer   | 即 `session.max`           |
| sessions.count             | Integer   | 当前会话数量               |
| sessions.max               | Integer   | 会话数量的历史最大值       |
| topics.count               | Integer   | 当前主题数量               |
| topics.max                 | Integer   | 主题数量的历史最大值       |
| suboptions.count           | Integer   | 即 `subscriptions.count`   |
| suboptions.max             | Integer   | 即 `subscriptions.max`     |
| subscribers.count          | Integer   | 当前订阅者数量             |
| subscribers.max            | Integer   | 订阅者数量的历史最大值     |
| subscriptions.count        | Integer   | 当前订阅数量，包含共享订阅 |
| subscriptions.max          | Integer   | 订阅数量的历史最大值       |
| subscriptions.shared.count | Integer   | 当前共享订阅数量           |
| subscriptions.shared.max   | Integer   | 共享订阅数量的历史最大值   |
| routes.count               | Integer   | 当前路由数量               |
| routes.max                 | Integer   | 路由数量的历史最大值       |
| retained.count             | Integer   | 当前保留消息数量           |
| retained.max               | Integer   | 保留消息的历史最大值       |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/stats"

{"data":[{"stats":{"topics.max":0,"topics.count":0,"subscriptions.shared.max":0,"subscriptions.shared.count":0,"subscriptions.max":0,"subscriptions.count":0,"subscribers.max":0,"subscribers.count":0,"suboptions.max":0,"suboptions.count":0,"sessions.max":0,"sessions.count":0,"rules.max":0,"rules.count":0,"routes.max":0,"routes.count":0,"retained.max":3,"retained.count":3,"resources.max":0,"resources.count":0,"connections.max":0,"connections.count":0,"channels.max":0,"channels.count":0,"actions.max":5,"actions.count":5},"node":"emqx@127.0.0.1"}],"code":0}
```

#### GET /api/v4/nodes/{node}/stats {#endpoint-nodes-get-stats}

类似 [GET /api/v4/stats](#endpoint-get-stats)，返回指定节点上的状态数据。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description   |
| ---- | --------- | ------------- |
| code | Integer   | 0 |
| data | Array of Objects | 各节点上的状态数据列表，详见 [GET /api/v4/stats](#endpoint-get-stats) |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/nodes/emqx@127.0.0.1/stats"

{"data":{"topics.max":0,"topics.count":0,"subscriptions.shared.max":0,"subscriptions.shared.count":0,"subscriptions.max":0,"subscriptions.count":0,"subscribers.max":0,"subscribers.count":0,"suboptions.max":0,"suboptions.count":0,"sessions.max":0,"sessions.count":0,"rules.max":0,"rules.count":0,"routes.max":0,"routes.count":0,"retained.max":3,"retained.count":3,"resources.max":0,"resources.count":0,"connections.max":0,"connections.count":0,"channels.max":0,"channels.count":0,"actions.max":5,"actions.count":5},"code":0}
```

### 告警 {#endpoint-alarms}

#### GET /api/v4/alarms/present {#endpoint-get-alarms-present}

返回集群下当前告警信息。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description   |
| ---- | --------- | ------------- |
| code | Integer   | 0 |
| data | Array of Objects | 各节点上的告警列表 |
| data[0].node   | String    | 节点名称                           |
| data[0].alarms | Array of Objects | 当前告警列表 |
| data[0].alarms[0].id   | String    | 告警标识符     |
| data[0].alarms[0].desc | String    | 告警的详细描述 |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/alarms/present"

{"data":[{"node":"emqx@127.0.0.1","alarms":[{"id":"cpu_high_watermark","desc":"88.30833333333334"}]}],"code":0}
```

#### GET /api/v4/alarms/present/{node} {#endpoint-nodes-get-alarms-present}

返回指定节点下当前告警信息。接口参数和返回请参看 [GET /api/v4/stats](#endpoint-get-alarms-present)。

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/alarms/present/emqx@127.0.0.1"

{"data":[{"id":"cpu_high_watermark","desc":"91.68333333333332"}],"code":0}
```

#### GET /api/v4/alarms/history {#endpoint-get-alarms-history}

返回集群下历史告警信息。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description   |
| ---- | --------- | ------------- |
| code | Integer   | 0 |
| data | Array of Objects | 各节点上的告警列表 |
| data[0].node   | String    | 节点名称 |
| data[0].alarms | Array of Objects | 当前告警列表 |
| data[0].alarms[0].id       | String    | 告警标识符 |
| data[0].alarms[0].desc     | String    | 告警的详细描述 |
| data[0].alarms[0].clear_at | String    | 告警清除时间，格式为 "YYYY-MM-DD HH:mm:ss" |

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/alarms/history"

{"data":[{"node":"emqx@127.0.0.1","alarms":[{"id":"cpu_high_watermark","desc":"93.27055293970582","clear_at":"2020-02-21 13:50:10"}]}],"code":0}
```

#### GET /api/v4/alarms/history/{node} {#endpoint-nodes-get-alarms-history}

返回指定节点下历史告警信息。接口参数和返回请参看 [GET /api/v4/alarms/history](#endpoint-get-alarms-history)。

**Examples:**

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/alarms/history/emqx@127.0.0.1"

{"data":[{"id":"cpu_high_watermark","desc":"93.27055293970582","clear_at":"2020-02-21 13:50:10"}],"code":0}
```

### 黑名单 {#endpoint-banned}

#### GET /api/v4/banned {#endpoint-get-banned}

获取黑名单

**Query String Parameters:**

  同 `/api/v4/clients`。

**Success Response Body (JSON):**

| Name | Type | Description                                                  |
| ---- | --------- | ------------------------------------------------------------ |
| code | Integer   | 0                   |
| data | Array     | 由对象构成的数组，对象中的字段与 *POST* 方法中的 Request Body 相同 |
| meta | Object    | 同 `/api/v4/clients`                                         |

**Examples:**

获取黑名单列表:

```bash
$ curl -i --basic -u admin:public -vX GET "http://localhost:8081/api/v4/banned"

{"meta":{"page":1,"limit":10000,"count":1},"data":[{"who":"example","until":1582265833,"reason":"undefined","by":"user","at":1582265533,"as":"clientid"}],"code":0}
```

#### POST /api/v4/banned {#endpoint-add-banned}

将对象添加至黑名单

**Parameters (json):**

| Name  | Type | Required | Default | Description                                                  |
| ----- | --------- | -------- | ----------| -------------------------------- |
| who   | String    | Required |    | 添加至黑名单的对象，可以是客户端标识符、用户名和 IP 地址 |
| as    | String    | Required |      | 用于区分黑名单对象类型，可以是 `clientid`，`username`，`peerhost` |
| by    | String    | Optional | user | 指示该对象被谁添加至黑名单 |
| at    | Integer   | Optional | 当前系统时间          | 添加至黑名单的时间，单位：秒 |
| until | Integer   | Optional | 当前系统时间 + 5 分钟 | 何时从黑名单中解除，单位：秒 |

**Success Response Body (JSON):**

| Name | Type | Description                                |
| ---- | --------- | ------------------------------------------ |
| code | Integer   | 0 |
| data | Object    | 与传入的 Request Body 相同                 |

**Examples:**

将 client 添加到黑名单:

```bash
$ curl -i --basic -u admin:public -vX POST "http://localhost:8081/api/v4/banned" -d '{"who":"example","as":"clientid"}'

{"data":{"who":"example","as":"clientid"},"code":0}
```

#### DELETE /api/v4/banned/{as}/{who} {#endpoint-delete-banned}

将对象从黑名单中删除

**Parameters:** 无

**Success Response Body (JSON):**

| Name    | Type | Description                                  |
| ------- | --------- | -------------------------------------------- |
| code    | Integer   | 0   |
| message | String    | 仅在发生错误时返回，用于提供更详细的错误信息 |

**Examples:**

将 client 从黑名单中移除:

```bash
$ curl -i --basic -u admin:public -X DELETE "http://localhost:8081/api/v4/banned/clientid/example"

{"code":0}
```

### 数据导入导出 {#endpoint-data-import-and-export}

数据导入导出。

#### GET /api/v4/data/export {#endpoint-get-export}

获取当前的导出文件信息列表，包括文件名、大小和创建时间。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name               | Type             | Description |
| ------------------ | ---------------- | ----------- |
| code               | Integer          | 0 |
| data               | Array of Objects | 所有路由信息 |
| data[0].filename   | String           | 文件名 |
| data[0].created_at | String           | "YYYY-MM-DD HH-mm-SS" 格式的文件创建时间 |
| data[0].size       | String           | 文件大小，单位：字节 |

**Examples:**

列出当前的导出文件信息列表:

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/data/export"

{"data":[{"size":350,"filename":"emqx-export-2020-5-15-18-6-29.json","created_at":"2020-5-15 18:6:29"},{"size":388,"filename":"emqx-export-2020-5-15-17-39-0.json","created_at":"2020-5-15 17:39:0"}],"code":0}
```

#### POST /api/v4/data/export {#endpoint-export-data-to-file}

导出当前数据到文件。

**Path Parameters:** 无

**Success Response Body (JSON):**

| Name            | Type    | Description |
| --------------- | ------- | ----------- |
| code            | Integer | 0 |
| data            | Object  | 文件信息 |
| data.filename   | String  | 文件名 |
| data.created_at | String  | "YYYY-MM-DD HH-mm-SS" 格式的文件创建时间 |
| data.size       | String  | 文件大小，单位：字节 |

**Examples:**

导出文件：

```bash
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/data/export"

{"data":{"size":350,"filename":"emqx-export-2020-5-18-17-17-44.json","created_at":"2020-5-18 17:17:44"},"code":0}
```

#### POST /api/v4/data/import {#endpoint-import-data-from-file}

从指定文件导入数据。

**Path Parameters:** 无

**Parameters (json):**

| Name     | Type  | Required | Default | Description |
| -------- | ----- | -------- | --------| ----------- |
| filename | String| Required |         | 导入的文件名  |

**Success Response Body (JSON):**

| Name    | Type      | Description                                  |
| ------- | --------- | -------------------------------------------- |
| code    | Integer   | 0 |
| message | String    | 仅在发生错误时返回，用于提供更详细的错误信息 |

**Examples:**

从指定文件导入数据：

```bash
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/data/import" -d '{"filename":"emqx-export-2020-5-18-17-17-44.json"}'

{"code":0}
```

#### GET /api/v4/data/file/{filename} {#endpoint-download-data-file}

下载数据文件。

**Path Parameters:** 无

**Parameters (json):**

| Name     | Type   | Required | Default | Description |
| -------- | ------ | -------- | --------| ----------- |
| filename | String | Required |         | 导入的文件名  |

**Success Response Body (JSON):**

| Name     | Type   | Description |
| -------- | ------ | ----------- |
| filename | String | 文件名 |
| file     | String | 文件内容 |

**Examples:**

下载指定的数据文件：

```bash
$ curl -i --basic -u admin:public -X GET "http://localhost:8081/api/v4/data/file/emqx-export-2020-5-18-17-17-44.json"

{"filename":"/Users/zhouzibo/emqx-rel/_build/emqx/rel/emqx/data/emqx-export-2020-5-18-17-17-44.json","file":"{\"version\":\"dev\",\"users\":[{\"username\":\"admin\",\"tags\":\"administrator\",\"password\":\"oKQPB1hbigv6+2ntALELNOb1fF0=\"}],\"schemas\":[],\"rules\":[],\"resources\":[],\"date\":\"2020-05-18 17:17:44\",\"blacklist\":[],\"auth_mnesia\":[],\"apps\":[{\"status\":true,\"secret\":\"public\",\"name\":\"Default\",\"id\":\"admin\",\"expired\":\"undefined\",\"desc\":\"Application user\"}],\"acl_mnesia\":[]}"}
```

#### POST /api/v4/data/file {#endpoint-upload-data-file}

上传数据文件。

**Path Parameters:** 无

**Parameters (json):**

| Name     | Type   | Required | Default | Description |
| -------- | ------ | -------- | --------| ----------- |
| filename | String | Required |         | 文件名  |
| file     | String | Required |         | 文件内容 |

**Success Response Body (JSON):**

| Name    | Type      | Description                                  |
| ------- | --------- | -------------------------------------------- |
| code    | Integer   | 0 |
| message | String    | 仅在发生错误时返回，用于提供更详细的错误信息 |

**Examples:**

上传指定的数据文件：

```bash
$ curl -i --basic -u admin:public -X POST "http://localhost:8081/api/v4/data/file" -d '{"filename":"emqx-export-2020-5-18-17-17-44.json","file":"{\"version\":\"dev\",\"users\":[{\"username\":\"admin\",\"tags\":\"administrator\",\"password\":\"oKQPB1hbigv6+2ntALELNOb1fF0=\"}],\"schemas\":[],\"rules\":[],\"resources\":[],\"date\":\"2020-05-18 17:17:44\",\"blacklist\":[],\"auth_mnesia\":[],\"apps\":[{\"status\":true,\"secret\":\"public\",\"name\":\"Default\",\"id\":\"admin\",\"expired\":\"undefined\",\"desc\":\"Application user\"}],\"acl_mnesia\":[]}"}'

{"code":0}
```

#### DELETE /api/v4/data/file/{filename} {#endpoint-delete-data-file}

远程删除数据文件。

**Path Parameters:** 无

**Parameters (json):**

| Name     | Type   | Required | Default | Description |
| -------- | ------ | -------- | --------| ----------- |
| filename | String | Required |         | 文件名  |

**Success Response Body (JSON):**

| Name    | Type      | Description                                  |
| ------- | --------- | -------------------------------------------- |
| code    | Integer   | 0 |
| message | String    | 仅在发生错误时返回，用于提供更详细的错误信息 |

**Examples:**

删除指定的数据文件：

```bash
$ curl -i --basic -u admin:public -X DELETE "http://localhost:8081/api/v4/data/file/emqx-export-2020-5-18-17-17-44.json"

{"code":0}
```

### 规则 {#endpoint-rules}

查询规则引擎的动作

#### GET /api/v4/rules/{rule_id} {#endpoint-get-rules}

获取某个规则的详情，包括规则的 SQL、Topics 列表、动作列表等。还会返回当前规则和动作的统计指标的值。

**Path Parameters:**

| Name    | Type | Required | Description                                                  |
| ------- | --------- | ----------- | ------------------------------------------------------------ |
| rule_id | String    | False       | 可选，Rule ID。如不指定 rule_id 则<br />以数组形式返回所有已创建的规则 |

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0         |
| data | Object | 规则对象    |
| - data.id              | String      | Rule ID                                          |
| - data.rawsql          | String      | SQL 语句，与请求中的 rawsql 一致                 |
| - data.for             | String      | Topic 列表，表示哪些 topic 可以匹配到此规则      |
| - data.metrics         | Array       | 统计指标，具体可参看 Dashboard 上的 Rule Metrics |
| - data.description     | String      | 规则的描述信息，与请求中的 description 一致      |
| - data.actions         | Array       | 动作列表                                         |
| - data.actions[0].id | String      | Action ID                                        |
| - data.actions[0].params | Object | 动作参数，与请求中的 actions.params 一致         |
| - data.actions[0].name | String      | 动作名字，与请求中的 actions.name 一致           |
| - data.actions[0].metrics | Array       | 统计指标，具体可参看 Dashboard 上的 Rule Metrics |

#### POST /api/v4/rules {#endpoint-add-rules}

创建规则，返回规则 ID。

**Parameters (json):**

| Name                 | Type | Required | Description |
| -------------------- | --------- | ----------- | ---------------- |
| rawsql               | String    | True        | 规则的 SQL 语句 |
| actions              | Array     | True        | 动作列表 |
| - actions[0].name   | String    | True        | 动作名称 |
| - actions[0].params | Object    | True        | 动作参数。参数以 key-value 形式表示。<br />详情可参看添加规则的示例 |
| description          | String    | False       | 可选，规则描述 |

**Success Response Body (JSON):**

| Name                       | Type | Description                                      |
| -------------------------- | --------- | ------------------------------------------------ |
| code                       | Integer   | 0                                                |
| data                       | Object    | 创建成功的规则对象，包含 Rule ID                 |
| - data.id                  | String    | Rule ID                                          |
| - data.rawsql              | String    | SQL 语句，与请求中的 rawsql 一致                 |
| - data.for                 | String    | Topic 列表，表示哪些 topic 可以匹配到此规则      |
| - data.metrics             | Array     | 统计指标，具体可参看 Dashboard 上的 Rule Metrics |
| - data.description         | String    | 规则的描述信息，与请求中的 description 一致      |
| - data.actions             | Array     | 动作列表，每个动作是一个 Object                  |
| - data.actions[0].id      | String    | Action ID                                        |
| - data.actions[0].params  | Object    | 动作参数，与请求中的 actions.params 一致         |
| - data.actions[0].name    | String    | 动作名字，与请求中的 actions.name 一致           |
| - data.actions[0].metrics | Array     | 统计指标，具体可参看 Dashboard 上的 Rule Metrics |

#### PUT /api/v4/rules/{rule_id} {#endpoint-update-rules}

更新规则，返回规则 ID。

**Parameters (json):**

| Name                 | Type | Required | Description |
| -------------------- | --------- | ----------- | ---------------- |
| rawsql               | String    | True        | 可选，规则的 SQL 语句 |
| actions              | Array     | True        | 可选，动作列表 |
| - actions[0].name    | String    | True        | 可选，动作名称 |
| - actions[0].params  | Object    | True        | 可选，动作参数。参数以 key-value 形式表示。<br />详情可参看添加规则的示例 |
| description          | String    | False       | 可选，规则描述 |

**Success Response Body (JSON):**

| Name                       | Type | Description                                      |
| -------------------------- | --------- | ------------------------------------------------ |
| code                       | Integer   | 0                                                |
| data                       | Object    | 创建成功的规则对象，包含 Rule ID                 |
| - data.id                  | String    | Rule ID                                          |
| - data.rawsql              | String    | SQL 语句，与请求中的 rawsql 一致                 |
| - data.for                 | String    | Topic 列表，表示哪些 topic 可以匹配到此规则      |
| - data.metrics             | Array     | 统计指标，具体可参看 Dashboard 上的 Rule Metrics |
| - data.description         | String    | 规则的描述信息，与请求中的 description 一致      |
| - data.actions             | Array     | 动作列表，每个动作是一个 Object                  |
| - data.actions[0].id      | String    | Action ID                                        |
| - data.actions[0].params  | Object    | 动作参数，与请求中的 actions.params 一致         |
| - data.actions[0].name    | String    | 动作名字，与请求中的 actions.name 一致           |
| - data.actions[0].metrics | Array     | 统计指标，具体可参看 Dashboard 上的 Rule Metrics |

#### DELETE /api/v4/rules/{rule_id} {#endpoint-delete-rules}

删除规则。

**Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0           |

**Examples:**

添加一个规则，对于所有匹配到主题 "t/a" 的消息，打印其规则运行参数。

```bash
$ curl -XPOST -d '{
  "rawsql": "select * from \"t/a\"",
  "actions": [{
      "name": "inspect",
      "params": {
          "a": 1
      }
  }],
  "description": "test-rule"
}' --basic -u admin:public 'http://localhost:8081/api/v4/rules'

{"data":{"rawsql":"select * from \"t/a\"","metrics":[{"speed_max":0,"speed_last5m":0.0,"speed":0.0,"node":"emqx@127.0.0.1","matched":0}],"id":"rule:7fdb2c9e","for":["t/a"],"enabled":true,"description":"test-rule","actions":[{"params":{"a":1},"name":"inspect","metrics":[{"success":0,"node":"emqx@127.0.0.1","failed":0}],"id":"inspect_1582434715354188116"}]},"code":0}
```

使用规则 ID 获取刚才创建的规则详情:

```bash
$ curl --basic -u admin:public 'http://localhost:8081/api/v4/rules/rule:7fdb2c9e'

{"data":{"rawsql":"select * from \"t/a\"","metrics":[{"speed_max":0,"speed_last5m":0.0,"speed":0.0,"node":"emqx@127.0.0.1","matched":0}],"id":"rule:7fdb2c9e","for":["t/a"],"enabled":true,"description":"test-rule","actions":[{"params":{"a":1},"name":"inspect","metrics":[{"success":0,"node":"emqx@127.0.0.1","failed":0}],"id":"inspect_1582434715354188116"}]},"code":0}
```

获取所有的规则，注意返回值里的 data 是个规则对象的数组:

```bash
$ curl --basic -u admin:public 'http://localhost:8081/api/v4/rules'

{"data":[{"rawsql":"select * from \"t/a\"","metrics":[{"speed_max":0,"speed_last5m":0.0,"speed":0.0,"node":"emqx@127.0.0.1","matched":0}],"id":"rule:7fdb2c9e","for":["t/a"],"enabled":true,"description":"test-rule","actions":[{"params":{"a":1},"name":"inspect","metrics":[{"success":0,"node":"emqx@127.0.0.1","failed":0}],"id":"inspect_1582434715354188116"}]}],"code":0}
```

更新一下规则的 SQL 语句，改为 `select * from "t/b"`:

```bash
$ curl -XPUT --basic -u admin:public 'http://localhost:8081/api/v4/rules/rule:7fdb2c9e' -d '{"rawsql":"select * from \"t/b\""}'

{"data":{"rawsql":"select * from \"t/b\"","metrics":[{"speed_max":0,"speed_last5m":0.0,"speed":0.0,"node":"emqx@127.0.0.1","matched":0}],"id":"rule:7fdb2c9e","for":["t/a"],"enabled":true,"description":"test-rule","actions":[{"params":{"a":1},"name":"inspect","metrics":[{"success":0,"node":"emqx@127.0.0.1","failed":0}],"id":"inspect_1582434715354188116"}]},"code":0}
```

停用规则 (disable):

```bash
$ curl -XPUT --basic -u admin:public 'http://localhost:8081/api/v4/rules/rule:7fdb2c9e' -d '{"enabled": false}'

{"data":{"rawsql":"select * from \"t/b\"","metrics":[{"speed_max":0,"speed_last5m":0.0,"speed":0.0,"node":"emqx@127.0.0.1","matched":0}],"id":"rule:7fdb2c9e","for":["t/a"],"enabled":false,"description":"test-rule","actions":[{"params":{"a":1},"name":"inspect","metrics":[{"success":0,"node":"emqx@127.0.0.1","failed":0}],"id":"inspect_1582434715354188116"}]},"code":0}
```

删除规则:

```bash
$ curl -XDELETE --basic -u admin:public 'http://localhost:8081/api/v4/rules/rule:7fdb2c9e'

{"code":0}
```

### 动作 {#endpoint-actions}

查询规则引擎的动作。注意动作只能由 emqx 提供，不能添加。

#### GET api/v4/actions/{action_name} {#endpoint-get-actions}

获取某个动作的详情，包括动作名字、参数列表等。

**Path Parameters:**

| Name        | Type | Required | Description |
| ----------- | --------- | ----------- | ----------------------------- |
| action_name | String    | False       | 可选，动作名。如不指定 action_name 则<br />以数组形式返回当前支持的所有动作。 |

**Success Response Body (JSON):**

| Name               | Type | Description                                                  |
| ------------------ | --------- | ------------------------------------------------------------ |
| code               | Integer   | 0                                                            |
| data               | Object    | 规则对象                                                     |
| - data.types       | String    | 指示当前动作从属于那些资源类型                               |
| - data.title       | Object    | 动作的简述，中英文。                                         |
| - data.params      | Object    | 动作的参数列表。参数以 key-value 形式表示。<br />详情可参看后面的示例 |
| - data.description | Object    | 动作的描述信息，中英文。                                     |
| - data.app         | String    | 动作的提供者                                                 |

**Examples:**

查询 inspect 动作的详情：

```bash
$ curl --basic -u admin:public 'http://localhost:8081/api/v4/actions/inspect'

{"data":{"types":[],"title":{"zh":"检查 (调试)","en":"Inspect (debug)"},"params":{},"name":"inspect","for":"$any","description":{"zh":"检查动作参数 (用以调试)","en":"Inspect the details of action params for debug purpose"},"app":"emqx_rule_engine"},"code":0}
```

查询当前所有的动作：

```bash
$ curl --basic -u admin:public 'http://localhost:8081/api/v4/actions'

{"data":[{"types":[],"title":{"zh":"空动作 (调试)","en":"Do Nothing (debug)"},"params":{},"name":"do_nothing","for":"$any","description":{"zh":"此动作什么都不做，并且不会失败 (用以调试)","en":"This action does nothing and never fails. It's for debug purpose"},"app":"emqx_rule_engine"}, ...],"code":0}
```

### 资源类型 {#endpoint-resource-types}

查询规则引擎的资源类型。注意资源类型只能由 emqx 提供，不能添加。

#### GET api/v4/resource_types/{resource_type_name} {#endpoint-get-resource-types}

获取某个资源的详情，包括资源描述、参数列表等。

**Path Parameters:**

| Name               | Type | Required | Description                                                  |
| ------------------ | --------- | ----------- | -------------------------- |
| resource_type_name | String    | False       | 可选，资源类型名。如不指定 resource_type_name 则<br />以数组形式返回当前支持的所有资源类型。 |

**Success Response Body (JSON):**

| Name               | Type | Description                                                  |
| ------------------ | --------- | ------------------------------------------------------------ |
| code               | Integer   | 0                                                            |
| data               | Object    | 规则对象                                                     |
| - data.title       | Object    | 资源类型的简述，中英文。                                     |
| - data.params      | Object    | 资源类型的参数列表。参数以 key-value 形式表示。<br />详情可参看后面的示例 |
| - data.description | Object    | 资源类型的描述信息，中英文。                                 |
| - data.provider    | String    | 资源类型的提供者                                             |

**Examples:**

查询 web_hook 资源类型的详细信息：

```bash
$ curl --basic -u admin:public 'http://localhost:8081/api/v4/resource_types/web_hook'

{"data":{"title":{"zh":"WebHook","en":"WebHook"},"provider":"emqx_web_hook","params":{"url":{"type":"string","title":{"zh":"请求 URL","en":"Request URL"},"required":true,"format":"url","description":{"zh":"请求 URL","en":"Request URL"}},"method":{"type":"string","title":{"zh":"请求方法","en":"Request Method"},"enum":["PUT","POST"],"description":{"zh":"请求方法","en":"Request Method"},"default":"POST"},"headers":{"type":"object","title":{"zh":"请求头","en":"Request Header"},"schema":{},"description":{"zh":"请求头","en":"Request Header"},"default":{}}},"name":"web_hook","description":{"zh":"WebHook","en":"WebHook"}},"code":0}
```

查询当前所有的资源类型：

```bash
$ curl --basic -u admin:public 'http://localhost:8081/api/v4/resource_types'

{"data":[{"title":{"zh":"WebHook","en":"WebHook"},"provider":"emqx_web_hook","params":{"url":{"type":"string","title":{"zh":"请求 URL","en":"Request URL"},"required":true,"format":"url","description":{"zh":"请求 URL","en":"Request URL"}},"method":{"type":"string","title":{"zh":"请求方法","en":"Request Method"},"enum":["PUT","POST"],"description":{"zh":"请求方法","en":"Request Method"},"default":"POST"},"headers":{"type":"object","title":{"zh":"请求头","en":"Request Header"},"schema":{},"description":{"zh":"请求头","en":"Request Header"},"default":{}}},"name":"web_hook","description":{"zh":"WebHook","en":"WebHook"}}, ...],"code":0}
```

### 资源 {#endpoint-resources}

管理规则引擎的资源。资源是资源类型的实例，用于维护数据库连接等相关资源。

#### GET api/v4/resources/{resource_id} {#endpoint-get-resources}

获取指定的资源的详细信息。

**Path Parameters:**

| Name        | Type | Required | Description                                                  |
| ----------- | --------- | ----------- | ------------------------------------------------------------ |
| resource_id | String    | False       | 可选，资源类型 ID。如不指定 resource_id 则<br />以数组形式返回当前所有的资源。 |

**Success Response Body (JSON):**

| Name               | Type | Description                                                  |
| ------------------ | --------- | ------------------------------------------------------------ |
| code               | Integer   | 0                                                            |
| data               | Object    | 规则对象                                                     |
| - data.id          | String    | 资源 ID                                                      |
| - data.type        | String    | 资源所从属的资源类型的名字。                                 |
| - data.config      | Object    | 资源的配置。参数以 key-value 形式表示。<br />详情可参看后面的示例 |
| - data.status      | Array     | 资源的状态信息。详情请参看 Dashboard 上资源的状态。          |
| - data.description | Object    | 资源的描述信息，中英文。                                     |

#### POST /api/v4/resources {#endpoint-add-resources}

创建规则，返回资源 ID。

**Parameters (json):**

| Name        | Type | Required | Description |
| ----------- | --------- | ----------- | --- |
| type        | String    | True        | 资源类型名。指定要使用哪个资源类型创建资源。               |
| config      | Object    | True        | 资源参数。要跟对应的资源类型的 params 里指定的格式相一致。 |
| description | String    | False       | 可选，资源描述                                             |

**Success Response Body (JSON):**

| Name               | Type | Description                                                  |
| ------------------ | --------- | ------------------------------------------------------------ |
| code               | Integer   | 0                                                            |
| data               | Object    | 规则对象                                                     |
| - data.id          | String    | 资源 ID                                                      |
| - data.type        | String    | 资源所从属的资源类型的名字。                                 |
| - data.config      | Object    | 资源的配置。参数以 key-value 形式表示。<br />详情可参看后面的示例 |
| - data.description | Object    | 资源的描述信息，中英文。                                     |

#### DELETE /api/v4/resources/{resource_id} {#endpoint-delete-resources}

删除资源。

**Parameters:** 无

**Success Response Body (JSON):**

| Name | Type | Description |
| ---- | --------- | ----------- |
| code | Integer   | 0           |

**Examples:**

创建一个 webhook 资源，webserver 的 URL 为 http://127.0.0.1:9910 ：

```bash
$ curl -XPOST -d '{
  "type": "web_hook",
  "config": {
      "url": "http://127.0.0.1:9910",
      "headers": {"token":"axfw34y235wrq234t4ersgw4t"},
      "method": "POST"
  },
  "description": "web hook resource-1"
}' --basic -u admin:public 'http://localhost:8081/api/v4/resources'

{"data":{"type":"web_hook","id":"resource:b12d3e44","description":"web hook resource-1","config":{"url":"http://127.0.0.1:9910","method":"POST","headers":{"token":"axfw34y235wrq234t4ersgw4t"}}},"code":0}
```

使用资源 ID 查询刚创建的资源：

```bash
$ curl --basic -u admin:public 'http://localhost:8081/api/v4/resources/resource:b12d3e44'

{"data":{"type":"web_hook","status":[{"node":"emqx@127.0.0.1","is_alive":false}],"id":"resource:b12d3e44","description":"web hook resource-1","config":{"url":"http://127.0.0.1:9910","method":"POST","headers":{"token":"axfw34y235wrq234t4ersgw4t"}}},"code":0}
```

查询当前已创建的所有的资源：

```bash
$ curl --basic -u admin:public 'http://localhost:8081/api/v4/resources'

{"data":[{"type":"web_hook","id":"resource:b12d3e44","description":"web hook resource-1","config":{"url":"http://127.0.0.1:9910","method":"POST","headers":{"token":"axfw34y235wrq234t4ersgw4t"}}}],"code":0}
```

删除资源:

```bash
$ curl -XDELETE --basic -u admin:public 'http://localhost:8081/api/v4/resources/resource:b12d3e44'

{"code":0}
```
