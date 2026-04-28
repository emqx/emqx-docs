# 将 MQTT 数据接入 AVEVA PI System

AVEVA PI System 是一个广泛应用于工业领域的数据基础设施平台，用于采集、存储和可视化工业资产的时序数据。EMQX 可以通过 HTTP 数据集成和 PI Web API 的 OMF（OSIsoft Message Format，OSIsoft 消息格式）端点，将来自现场设备的 MQTT 消息直接转发至 PI System，从而无需额外中间件即可构建实时运营数据管道。

本页介绍了集成架构概览，并提供了配置 EMQX 通过 PI Web API 向 AVEVA PI System 发送数据的分步指南。

## 集成方式选择

EMQX 支持两种与 AVEVA PI System 集成的方式：

- **HTTP 集成（本页）**：EMQX 主动将数据以 OMF 消息的形式通过 POST 请求推送至 PI Web API 端点。该方式适用于已安装 PI Web API 的任意 PI Server，可通过规则引擎对消息载荷进行精细化转换，且无需安装 PI Connector for MQTT。
- **[PI Connector for MQTT](../../connect-emqx/aveva-pi-system.md)**：PI Connector for MQTT 作为 MQTT 客户端连接至 EMQX，通过订阅主题拉取数据。该方式需要安装 AVEVA 的 PI Connector 软件，但对 EMQX 的配置要求极少，与标准代理配置基本一致。

当需要对消息载荷进行精细化转换、PI Web API 已是现有基础设施的一部分，或希望避免安装额外的 AVEVA 连接器软件时，建议选择 HTTP 集成方式。当设备已能发布符合 OMF 格式的消息载荷，且希望由 PI System 直接管理数据接入时，建议选择 PI Connector 方式。

## 工作原理

EMQX 接收来自工业设备的 MQTT 消息，并通过 EMQX 规则引擎和 HTTP Server Sink 将数据转发至 PI System。工作流程如下：

1. **设备发布遥测数据**：现场设备或边缘网关通过 MQTT 向 EMQX 发布传感器读数。消息载荷通常为 JSON 格式，包含一个或多个测量值。
2. **规则引擎处理消息**：规则引擎按主题匹配传入消息，并通过 SQL 提取相关字段，可对数据进行过滤、转换和丰富处理，再行转发。
3. **HTTP Server Sink 发送 OMF 数据**：处理后的数据通过 HTTPS POST 请求转发至 PI Web API OMF 端点。PI Web API 解析 OMF 消息后，将数值写入 PI Data Archive。

```
现场设备 → MQTT → EMQX → 规则引擎 → HTTP Sink → PI Web API（OMF）→ PI Data Archive
```

AVEVA 的 OSIsoft Message Format（OMF）是该路径的标准数据格式。OMF 消息为 JSON 载荷，包含以下三种消息类型之一：

- **Type（类型）**：定义数据模式（即数据容器的结构）
- **Container（容器）**：创建类型的命名实例，对应 PI 标签
- **Data（数据）**：向已有容器发送实际的时序数值

PI 标签注册（Type 和 Container 消息）为一次性设置步骤，后续 EMQX 数据集成仅需处理 Data 消息。

## 前提条件

- EMQX Enterprise v6.0 及以上版本
- AVEVA PI Server 2018 及以上版本，并已安装并启用 PI Web API 2019 及以上版本
- PI Web API 已配置为接受 OMF 请求（须在 PI Web API Admin 中启用 OMF 端点）
- 具有目标 PI Data Archive 写入权限的 PI Web API 服务账户
- EMQX 部署可通过网络访问 PI Web API 主机

## 通过 OMF 注册 PI 标签

在开始数据流传输之前，需在 PI Web API 中注册 Type 和 Container，以便 PI Data Archive 识别传入数据的模式。此为一次性步骤，可通过 `curl` 或 Postman 等任意 HTTP 客户端完成。

PI Web API OMF 端点 URL 格式如下：

```
https://<pi-web-api-host>/piwebapi/omf
```

### 步骤 1：创建 Type

Type 用于定义数据模式。以下示例定义了一个包含单个浮点值和时间戳的简单类型：

```bash
curl -k -X POST "https://<pi-web-api-host>/piwebapi/omf" \
  -H "Content-Type: application/json" \
  -H "X-Requested-With: XMLHttpRequest" \
  -H "omfversion: 1.1" \
  -H "action: create" \
  -H "messageformat: json" \
  -H "messagetype: type" \
  -u "<username>:<password>" \
  -d '[{
    "id": "emqx-sensor-type",
    "type": "object",
    "classification": "dynamic",
    "properties": {
      "Timestamp": { "type": "string", "format": "date-time", "isindex": true },
      "Value": { "type": "number", "format": "float64" }
    }
  }]'
```

### 步骤 2：创建 Container

Container 是 Type 的命名实例，每个 Container 对应 PI Data Archive 中的一个 PI 标签。

```bash
curl -k -X POST "https://<pi-web-api-host>/piwebapi/omf" \
  -H "Content-Type: application/json" \
  -H "X-Requested-With: XMLHttpRequest" \
  -H "omfversion: 1.1" \
  -H "action: create" \
  -H "messageformat: json" \
  -H "messagetype: container" \
  -u "<username>:<password>" \
  -d '[{
    "id": "sensor-001-temperature",
    "typeid": "emqx-sensor-type"
  }]'
```

将 `sensor-001-temperature` 替换为每个传感器对应的 PI 标签名称，并针对每个数据流重复此步骤。

## 创建连接器

本节介绍如何创建 HTTP Server 连接器，用于连接 EMQX Sink 与 PI Web API OMF 端点。

1. 在 EMQX Dashboard 中，点击**集成** -> **连接器**。
2. 点击页面右上角的**创建**，选择 **HTTP Server**，然后点击**下一步**。
3. 输入连接器名称，例如 `aveva_pi_connector`。
4. 将 **URL** 设置为 `https://<pi-web-api-host>/piwebapi/omf`。
5. 在**请求头**中添加 `Authorization` 请求头，用于传递 Basic Auth 凭据。将 PI Web API 服务账户的用户名和密码按 `username:password` 格式进行 Base64 编码，并将请求头值设置为 `Basic <base64编码后的凭据>`。
   <!-- TODO: 部分 PI 部署使用 Kerberos（Windows 身份验证）而非 Basic Auth。如果目标用户中此场景较为常见，请在此处补充说明：EMQX 的 HTTP 连接器不直接支持 Kerberos 认证的 PI Web API 端点，可能需要通过反向代理或中间件层来处理 Kerberos 协商。 -->
6. 若 PI Web API 使用自签名证书，请点击**启用 TLS** 并配置 CA 证书；仅在测试环境中可禁用证书验证。
7. 点击**创建**之前，您可以先点击**测试连接**，验证 EMQX 能否访问 PI Web API 端点。
8. 点击**创建**按钮完成连接器创建。页面将弹出**创建成功**对话框，询问是否立即创建规则。点击**创建规则**可直接进入规则创建页面并预选该连接器，或点击**返回连接器列表**稍后再创建规则。

## 创建带 HTTP Server Sink 的规则

本节介绍如何创建规则，从 MQTT 消息中提取数据，并通过 HTTP Server Sink 以 OMF 格式将其转发至 PI Web API。

1. 如果您在上一步中点击了**创建规则**，**添加动作**面板将自动打开，且**动作类型**已设置为 `HTTP Server` 并预选了连接器，可直接跳至第 5 步。否则，请前往 Dashboard **集成** -> **规则**页面，点击右上角**创建**。
2. 输入规则 ID，例如 `aveva_pi_rule`。
3. 在 **SQL 编辑器**中，输入匹配设备主题并提取所需字段的 SQL 语句。以下示例读取 `sensors/#` 下的消息，并提取传感器 ID、数值和时间戳：

   ```sql
   SELECT
     payload.sensor_id AS container_id,
     payload.value AS value,
     timestamp AS ts
   FROM
     "sensors/#"
   ```

4. 点击**+ 添加动作**，从**动作类型**下拉列表中选择 `HTTP Server`，保持**动作**下拉框为默认的**创建动作**选项。
5. 输入 Sink 名称，例如 `pi_omf_sink`。
6. 在**连接器**下拉列表中选择已创建的 `aveva_pi_connector`。
7. 将**请求方法**设置为 `POST`。
8. 将 **URL** 设置为 `https://<pi-web-api-host>/piwebapi/omf`。
9. 在**请求头**中添加以下键值对：

   | 键 | 值 |
   |---|---|
   | `Content-Type` | `application/json` |
   | `X-Requested-With` | `XMLHttpRequest` |
   | `omfversion` | `1.1` |
   | `action` | `create` |
   | `messageformat` | `json` |
   | `messagetype` | `data` |

10. 在**请求体**字段中输入 OMF 数据消息模板，使用 `${字段名}` 语法引用规则引擎提取的字段：

    ```json
    [{
      "containerid": "${container_id}",
      "values": [{
        "Timestamp": "${ts}",
        "Value": ${value}
      }]
    }]
    ```

11. **备选动作（可选）**：如果您希望在消息投递失败时提升系统的可靠性，可以为 Sink 配置一个或多个备选动作。更多信息请参见：[备选动作](./data-bridges.md#备选动作)。
12. 如需配置缓存队列、请求超期、请求模式等高级选项，可展开**高级设置**，默认值适用于大多数部署场景。详情请参阅 [Sink 的特性](./data-bridges.md#sink-的特性)。
13. 点击**创建**按钮完成 Sink 的创建，创建成功后页面将回到规则创建，新的 Sink 将添加到规则动作中。
14. 回到规则创建页面，点击**保存**按钮完成整个规则创建。

## 测试集成

使用 [MQTTX](https://mqttx.app/) 或任意 MQTT 客户端发布测试消息：

```bash
mqttx pub -t "sensors/building-a/room-1" -m '{"sensor_id":"sensor-001-temperature","value":23.5}'
```

发布后，在 PI System 中验证结果：

- 使用 PI Vision、PI DataLink 或 PI System Explorer 查询 `sensor-001-temperature` 标签。
- 确认时序数值 `23.5` 已以正确的时间戳写入。

也可在 EMQX Dashboard 的规则统计页面进行验证：点击**规则**页面中的规则名称，确认传入和传出消息计数已增加。

## 高级配置

### 将多个传感器映射到不同 PI 标签

如果设备在单条消息中发布多个传感器读数，可使用多个规则，或在规则 SQL 中对每个字段分别处理。OMF 请求体模板中的 `container_id` 字段决定数值写入哪个 PI 标签，不同的消息或使用不同请求体模板的独立 Sink 可将数据写入不同标签。

### 处理时间戳

PI Data Archive 要求时间戳为 ISO 8601 格式（如 `2024-01-15T10:30:00Z`）。EMQX 的 `timestamp` 字段为 Unix 毫秒时间戳整数。如果设备载荷中已包含格式正确的 ISO 8601 时间戳，可直接引用该字段；否则，请在设备端或边缘侧完成时间戳转换后再发布至 EMQX，也可使用规则引擎的时间相关函数进行处理。

### 安全建议

- 在 EMQX 与 PI Web API 之间的所有通信中使用 HTTPS。
- 使用专用 PI Web API 服务账户，并仅授予必要的写入权限。
- 为现场设备配置启用 TLS（端口 8883）的 EMQX MQTT 监听器，并要求客户端证书认证。
- 生产环境中不得禁用证书验证。
