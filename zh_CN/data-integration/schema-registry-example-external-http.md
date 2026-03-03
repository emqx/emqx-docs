# 编解码举例 - 外部 HTTP

本文介绍了如何通过具有自定义逻辑的外部 HTTP 服务，实现 EMQX 的 Schema Registry 与规则引擎对消息的编码与解码处理。

在某些场景下，您可能需要对消息应用自定义的编码或解码逻辑，而这些逻辑并非 EMQX 原生支持。EMQX 支持通过规则中的 `schema_encode` 和 `schema_decode` 函数，将编码/解码处理委托给外部 HTTP 服务来完成。

## 外部 HTTP API 规范

要实现与 EMQX 的 `schema_encode` 和 `schema_decode` 函数配套的外部 HTTP API，服务端需提供一个 `POST` 接口用于接收 EMQX 发起的编码或解码请求。

### 请求格式

请求体是一个 JSON 对象，包含以下字段：

- `payload`：规则引擎中传入 `schema_encode` 或 `schema_decode` 函数的值，已进行 Base64 编码，类型为字符串。
- `type`：字符串，值为 `encode` 或 `decode`，用于区分当前执行的是 `schema_encode` 还是 `schema_decode`。
- `schema_name`：当前在 EMQX 中配置的外部 HTTP Schema 名称。
- `opts`：可选字符串，由 EMQX 配置传入，可用于携带额外参数，原样传递给 HTTP 服务。

### 响应格式

- 服务端必须返回 HTTP 状态码 `200`。
- 响应体应为一个 Base64 编码后的字符串，表示最终结果。
- 注意：此 Base64 字符串应为纯文本，不应嵌套在 JSON 对象中返回。

## 使用示例

假设某设备发布了一条二进制消息，您希望使用自定义的 XOR 操作来对该消息进行编码或解码。本节通过构建一个带有 XOR 编码逻辑的简单 HTTP 服务，展示如何将用户自定义的编解码逻辑通过外部服务接入 EMQX 规则引擎中。

### 构建外部 HTTP 服务

以下示例展示了如何使用 Python + Flask 编写并运行一个简单的 HTTP 服务，用于将接收到的 Base64 消息进行 XOR 编码处理。

<details> <summary><strong>示例：外部 HTTP 服务</strong></summary>

确保已安装 [Flask](https://flask.palletsprojects.com/en/stable/)：

```sh
pip install Flask==3.1.0
```

示例代码：

```python
from flask import Flask, request
import base64

app = Flask(__name__)

@app.route("/serde", methods=['POST'])
def serde():
    # 接收并解码 base64 编码的输入
    body = request.get_json(force=True)
    print("incoming request:", body)
    payload64 = body.get("payload")
    payload = base64.b64decode(payload64)
    secret = 122
    response = bytes(b ^ secret for b in payload)
    # 返回的结果也需进行 base64 编码
    response64 = base64.b64encode(response)
    return response64
```

运行服务：

```sh
# 假设服务保存在当前目录的 `myapp.py` 文件中
flask --app myapp --debug run -h 0.0.0.0 -p 9500
```

</details>

### 在 EMQX 中创建 External HTTP Schema

1. 进入 Dashboard，依次点击左侧导航栏的**数据智能中心** -> **Schema Registry**。
2. 在到 **内部 Schema** 标签页中，点击 **创建**。
3. 使用以下参数创建外部 HTTP Schema：
   - **名称**：`myhttp`
   - **类型**：`External HTTP`
   - **URL**：您的 HTTP 服务运行地址，例如 `http://server:9500/serde`。
4. 点击**创建**完成创建。

### 创建规则应用 Schema

通过规则引擎创建一条规则，使用该 Schema 对消息进行编码和解码。

1. 在 Dashboard 中，选择**集成** -> **规则**。

2. 点击右上角的**创建**进入规则创建页面。

3. 编写如下 SQL 语句：

   ```sql
   SELECT
     schema_encode('myhttp', payload) as encoded,
     schema_decode('myhttp', encoded) as decoded
   FROM
     "t/external_http"
   ```

   语句中的 `schema_encode` 和 `schema_decode` 会调用配置的外部 HTTP 服务，对 payload 进行处理。

4. 点击**添加动作**，在**动作**下拉列表中选择 `Republish`。

5. 在**主题**字段中填写目标主题：`external_http/out`。

6. 在 **Payload** 字段中填写消息模板：`${.}`。

7. 点击**添加**将动作添加到规则。

   该动作会将解码后的消息以 JSON 格式发布到主题 `external_http/out`。`${.}` 是变量模板，运行时会替换为规则输出的完整内容。

8. 点击**保存**保存规则。

### 验证规则执行结果

1. 在 Dashboard 中选择**诊断工具** -> **WebSocket 客户端**。

2. 输入当前 EMQX 实例的连接信息：

   - 如果你在本地运行 EMQX，可使用默认连接配置。
   - 若启用了认证机制，可能需要输入用户名和密码。

3. 点击**连接**以 MQTT 客户端身份连接到 EMQX。

4. 在**订阅**区域的**主题**字段中输入 `external_http/out`，点击**订阅**。

5. 在**发布**区域输入主题 `t/external_http`，填入任意 payload，点击**发布**发布消息。

6. 在 WebSocket 客户端接收区域检查是否收到响应。例如，发送内容为 `hello` 时，可能会收到如下消息：

   ```json
   {"encoded":"\u0012\u001F\u0016\u0016\u0015","decoded":"hello"}
   ```
