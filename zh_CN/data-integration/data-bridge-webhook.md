# 将 MQTT 数据发送到 HTTP 服务

HTTP 服务数据集成提供了将 EMQX 与外部服务快速集成的方法。它支持灵活的配置请求方法和请求数据格式，提供了 HTTPS 安全的通信机制以及身份验证机制，能够实时传输客户端的消息和事件数据，高效、灵活地实现物联网设备状态推送、告警通知以及数据集成等场景。

本页详细介绍了 HTTP 服务数据集成的功能特性，并提供了实用的规则和 HTTP 服务 Sink 创建指导。

::: tip
对于那些需要集成 HTTP 服务但无需使用规则进行数据处理的用户，我们推荐使用 [Webhook](./webhook.md)，因为它更加简单易用。
:::

## 工作原理

HTTP 服务数据集成是 EMQX 中开箱即用的功能，通过简单的配置即可实现 EMQX 与外部服务的集成。借助 HTTP 服务，用户可以使用自己熟悉的编程语言和框架编写代码，实现自定义的灵活和复杂的数据处理逻辑。

![EMQX Webhook 集成](./assets/emqx-integration-http.jpg)

EMQX 通过规则引擎与 Sink 将设备事件和数据转发至 HTTP 服务，其工作流程如下：

1. **设备连接到 EMQX**：物联网设备连接成功后将触发上线事件，事件包含设备 ID、来源 IP 地址以及其他属性等信息。
2. **设备发布消息**：设备通过特定的主题发布遥测和状态数据，消息将触发规则引擎。
3. **规则引擎处理消息**：通过内置的规则引擎，可以根据主题匹配处理特定来源的消息和事件。规则引擎会匹配对应的规则，并对消息和事件进行处理，例如转换数据格式、过滤掉特定信息或使用上下文信息丰富消息。
4. **发送到 HTTP 服务**：规则触发将消息发送到 HTTP 服务事件的动作。用户可以从规则处理结果中提取数据，动态构造请求头、请求体甚至 URL，实现灵活的将数据与外部服务集成。

事件和消息数据发送到 HTTP 服务后，您可以进行灵活的处理，例如：

- 实现设备状态更新、事件记录，基于数据开发设备管理系统。
- 将消息数据写入到数据库中，实现轻量级数据存储功能。
- 对于规则 SQL 过滤的异常数据，可以直接通过 HTTP 服务调用告警通知系统，进行设备异常监控。

## 特性与优势

使用 EMQX 的 HTTP 服务集成可以为业务带来以下优势：

- **将数据传递到更多的下游系统**：HTTP 服务可以将 MQTT 数据轻松集成到更多的外部系统中，比如分析平台、云服务等，实现多系统的数据分发。

- **实时响应并触发业务流程**：通过 HTTP 服务，外部系统可以实时接收到 MQTT 数据并触发业务流程，实现快速响应。例如接收报警数据并触发业务工作流。

- **自定义处理数据**：外部系统可以根据需要对接收到的数据进行二次处理，实现更复杂的业务逻辑，不受 EMQX 功能限制。

- **松耦合的集成方式**：HTTP 服务使用简单的 HTTP 接口，提供了一种松耦合的系统集成方式。

总之，HTTP 服务为业务提供了实时、灵活、自定义的数据集成能力，可以满足灵活，丰富的应用开发需求。

## 准备工作

本节介绍了在 EMQX 中创建 HTTP 服务 Sink 之前需要做的准备工作，即如何使用 Dashboard 创建一个简单的 HTTP 服务。

### 前置准备

- 了解[规则](./rules.md)。
- 了解[数据集成](./data-bridges.md)。

### 搭建简易 HTTP 服务

首先我们使用 Python 搭建一个简单的 HTTP 服务，用来接收 `POST /` 请求，该服务打印请求内容后返回 200 OK：

```python
from flask import Flask, json, request

api = Flask(__name__)

@api.route('/', methods=['POST'])
def print_messages():
  reply= {"result": "ok", "message": "success"}
  print("got post request: ", request.get_data())
  return json.dumps(reply), 200

if __name__ == '__main__':
  api.run()
```

将上面的代码保存为 `http_server.py` 文件，文件所在目录运行如下命令：

```shell
# 安装 flask 依赖
pip install flask

# 启动服务
python3 http_server.py
```

## 创建连接器

在创建 Sink 之前，我们需要先创建一个 HTTP 服务连接器，用来指定 HTTP 服务的地址、请求方法和请求头等信息。

1. 转到 Dashboard **集成** -> **连接器**页面。
2. 点击页面右上角的**创建**。
3. 在连接器类型中选择 **HTTP 服务**，点击**下一步**。
4. 为连接器输入名称，名称应由大小写字母或数字组成，例如：`httpserver`。
5. 将 **URL** 设置为 HTTP 服务器的地址。例如：`http://localhost:5000`。
6. [可选] 在**请求头**中添加 HTTP 请求头。
7. [可选] 开启 **OAuth2 客户端凭证**。EMQX 将获取 Access Token，并将其以 Bearer Token 的形式添加到发往目标 HTTP 服务的请求中。有关配置详情，参见[配置 OAuth2 客户端凭证认证](#配置-oauth2-客户端凭证认证)。
8. [可选] 开启**启用 TLS**，对目标 HTTP 服务的连接启用 TLS。此开关独立于 OAuth2 Token 端点的**启用 TLS**开关。
9. [可选] 在**高级设置**中配置连接相关选项。详细信息参见 [Sink 的特性](./data-bridges.md#sink-的特性)。
10. 在点击**创建**之前，可以点击**测试连接性**，验证连接器是否能成功连接到 HTTP 服务器。
11. 点击**创建**完成连接器配置。

### 配置 OAuth2 客户端凭证认证

从 EMQX 6.0.4 开始，HTTP 服务连接器支持 OAuth 2.0 客户端凭证模式（Client Credentials Grant）。启用 OAuth2 后，EMQX 从配置的 Token 端点（Token Endpoint）获取、缓存并自动刷新 Access Token。EMQX 调用目标 HTTP 服务时，会通过 `Authorization: Bearer <access_token>` 请求头携带该 Token，由目标服务验证 EMQX 的身份。

创建或编辑连接器时，开启 **OAuth2 客户端凭证**，然后配置以下设置：

| Dashboard 配置项 | 说明 |
| --- | --- |
| **Token 端点** | 必填。用于请求 Access Token 的 OAuth2 授权服务器端点。URL 必须使用 HTTP 或 HTTPS，且不能包含用户信息。 |
| **客户端 ID** | 必填。请求 Access Token 时使用的 OAuth2 客户端 ID。 |
| **客户端密钥** | 必填。请求 Access Token 时使用的 OAuth2 客户端密钥。 |
| **授权范围** | 可选。请求 Access Token 时使用的 OAuth2 授权范围。 |
| **Token 请求超时** | 向 Token 端点发送 HTTP 请求的超时时间。默认值为 `5` 秒。 |
| **启用 TLS** | 开启后，对 Token 端点启用 TLS。此开关独立于 OAuth2 配置面板外用于目标 HTTP 服务的**启用 TLS**开关。 |

使用 HOCON 配置时，将 `oauth2` 配置块添加到 HTTP 服务连接器配置中，与 `url`、`headers` 和 `ssl` 同级：

```hocon
oauth2 {
    enable = true
    grant_type = client_credentials
    token_endpoint = "https://auth.example.com/oauth/token"
    client_id = "emqx-client"
    client_secret = "emqx-client-secret"
    scope = "messages.write"
    timeout = 5s
    ssl {
        enable = true
    }
}
```

EMQX 使用 `POST` 方法向 Token Endpoint 发送 `application/x-www-form-urlencoded` 请求。请求体包含 `grant_type`、`client_id`、`client_secret` 和可选的 `scope`。Token Endpoint 必须返回状态码 `200`，JSON 响应体中必须包含 `access_token`，还可以包含 `token_type` 和 `expires_in`。如果返回 `token_type`，其值必须为 `Bearer`；如果返回 `expires_in`，其值必须为正整数。

::: warning 重要提示

- 启用 OAuth2 后，不要在 HTTP 服务连接器或其 Sink 中配置 `Authorization` 请求头。此请求头与 EMQX 自动生成的 Bearer 认证请求头冲突，EMQX 会拒绝该配置。
- Token Endpoint 必须从请求体的表单字段中接收 Client ID 和 Client Secret。不支持通过 HTTP Basic `Authorization` 请求头向 Token Endpoint 发送客户端凭证。

:::

如果 EMQX 无法获取 Access Token，连接器健康检查会将连接器状态报告为 `disconnected`。

连接器创建成功后，弹出一个对话框，询问是否使用此连接器创建规则。

- 点击**创建规则**，直接进入规则创建页面并继续配置集成。
- 或者，点击**返回连接器列表**，返回到连接器列表页面，稍后从**集成** -> **规则**中创建规则。

在此示例中，点击**创建规则**继续。

## 创建 HTTP 服务 Sink 规则

本节演示如何创建规则并配置 HTTP 服务器 Sink，将 MQTT 消息发送到 HTTP 服务器。

点击**创建规则**后，您将自动进入**创建规则**页面，页面右侧的动作添加面板（用于配置 HTTP 服务器 Sink）会自动弹出，并且连接器已经准备好使用。

1. **动作类型**和**动作**会自动填充为 `HTTP 服务` 和`创建动作`以创建一个全新的 Sink 并添加到规则中。

2. 输入 Sink 的名称与描述。**连接器**会自动填充为您之前创建的 `httpserver` 连接器。

3. 配置 HTTP 请求：

   - **URL 路径**：`/`
   - **请求方法**： `POST`

   最终的请求 URL 会由连接器的 URL 和此路径组合而成。

4. 配置**请求体**，以将 MQTT 消息数据发送到 HTTP 服务器：

   ```json
   {
     "topic": "${topic}",
     "payload": ${payload},
     "clientid": "${clientid}",
     "qos": ${qos},
     "timestamp": ${timestamp}
   }
   ```

5. **备选动作（可选）**：如果您希望在消息投递失败时提升系统的可靠性，可以为 Sink 配置一个或多个备选动作。当 Sink 无法成功处理消息时，这些备选动作将被触发。更多信息请参见：[备选动作](./data-bridges.md#备选动作)。

6. 在点击**创建**之前，您可以点击**测试连接**，验证 Sink 是否可以连接到 HTTP 服务器。

7. 点击**创建**完成 Sink 配置。新创建的 Sink 将出现在**创建规则**页面中规则的**动作输出**部分。

8. 输入规则 ID，该 ID 可以由系统随机生成，也可以由您自定义（可选），例如：`my_rule`。

9. 在 **SQL 编辑器**中，输入以下 SQL 语句：

   ```sql
   SELECT 
     *
   FROM
     "t/#"
   ```

   此规则匹配所有发布到 `t/#` 下的 MQTT 消息。

   ::: tip

   如果你想指定自定义 SQL 语法，请确保在 `SELECT` 部分包含 Sink 所需的所有字段。

   :::

10. 点击**保存**完成规则创建。

规则创建后，发布到 `t/#` 下的消息将由规则处理，并转发到配置的 HTTP 服务器。

您还可以进入**集成** -> **Flow 设计器**来查看规则和 HTTP 服务器 Sink 的数据流拓扑。

## 测试规则

1. 使用 MQTTX 向主题 `t/1` 发送一条消息，以触发上下线事件。

   ```bash
   mqttx pub -i emqx_c -t t/1 -m '{ "msg": "hello HTTP Server" }'
   ```

2. 转到 Dashboard 中的**规则**页面，点击规则名称查看其统计信息。指标应显示一条新的传入消息和一条新的传出消息，表示消息已成功被 HTTP 服务器 Sink 处理并转发。

3. 验证 HTTP 服务器是否已接收到请求。

   如果 Python HTTP 服务器正在运行，终端应显示类似以下内容：

   ```text
   python3 http_server.py
    * Serving Flask app 'http_server'
    * Environment: production
      WARNING: This is a development server. Do not use it in a production deployment.
      Use a production WSGI server instead.
    * Debug mode: off
    * Running on http://127.0.0.1:5000 (Press CTRL+C to quit)
   
   got post request:  b'{"topic":"t/1","payload":{"msg":"hello HTTP Server"},"clientid":"emqx_c","qos":0,"timestamp":1700000000000}'
   ```

   打印的内容显示，EMQX 已将 MQTT 消息以 JSON 格式转发到 HTTP 服务器。请求体中的字段对应 Sink 请求体模板中配置的变量。
