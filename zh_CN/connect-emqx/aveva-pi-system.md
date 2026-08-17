# 将 EMQX 与 AVEVA PI System 配合使用

AVEVA PI System 提供了 PI Connector for MQTT，该连接器可订阅 MQTT 主题并通过 PI Asset Framework 将数据直接接入 PI Data Archive。本页介绍如何将 EMQX 配置为 PI Connector 的 MQTT 代理服务器，使现场设备能够通过 MQTT 发布传感器数据，并将其写入 PI System，无需任何额外中间件。

## 集成方式选择

EMQX 支持两种与 AVEVA PI System 集成的方式：

- **PI Connector for MQTT（本页）**：PI Connector for MQTT 作为 MQTT 客户端连接至 EMQX，通过订阅主题拉取数据。该方式需要安装 AVEVA 的 PI Connector 软件，但对 EMQX 的配置要求极少，与标准代理配置基本一致。
- **[HTTP 集成](../../data-integration/aveva-pi-system.md)**：EMQX 主动将数据以 OMF 消息的形式通过 POST 请求推送至 PI Web API 端点。该方式适用于已安装 PI Web API 的任意 PI Server，可通过规则引擎对消息载荷进行精细化转换，且无需安装 PI Connector for MQTT。

当设备已能发布符合 OMF 格式的消息载荷，且希望由 PI System 直接管理数据接入时，建议选择 PI Connector 方式。当需要对消息载荷进行精细化转换、PI Web API 已是现有基础设施的一部分，或希望避免安装额外的 AVEVA 连接器软件时，建议选择 HTTP 集成方式。

## 工作原理

PI Connector for MQTT 作为 MQTT 客户端运行：连接至 EMQX，订阅一个或多个主题，接收来自现场设备的消息，并使用 OSIsoft Message Format（OMF）将数据转发至 PI Server。

```
现场设备 → MQTT 发布 → EMQX → PI Connector for MQTT → PI Server
```

EMQX 负责所有 MQTT 代理功能：连接管理、认证、访问控制和消息路由。PI Connector 负责将 MQTT 消息转换为 PI Data Archive 写入操作。如有需要，EMQX 规则引擎可在消息到达 PI Connector 订阅的主题之前对其进行过滤或转换。

## 前提条件

- EMQX Enterprise v6.0 及以上版本
- AVEVA PI Server 2018 及以上版本
- 已安装 PI Connector for MQTT 的 PI Connector Relay（可从 AVEVA 软件分发渠道获取）
- PI Connector 主机与 EMQX 代理之间的网络连通性

## 为 PI Connector 配置 EMQX

PI Connector 像普通 MQTT 客户端一样连接至 EMQX。您需要为连接器的客户端身份配置监听器、认证凭据和访问控制。

### 步骤 1：确认 MQTT 监听器

EMQX 默认在 TCP 端口 `1883` 上监听。生产环境建议改用 TLS 监听器，端口为 `8883`。如需查看或调整监听器设置，请前往 **EMQX Dashboard** -> **管理** -> **监听器**。

对于 TLS，需确保已配置服务器证书和私钥，且 PI Connector 主机信任用于签署服务器证书的 CA 证书。

### 步骤 2：创建认证凭据

PI Connector 使用用户名和密码向 EMQX 进行认证。请为其创建专用凭据：

1. 在 EMQX Dashboard 中，点击**访问控制** -> **认证**。
2. 选择您的认证后端（例如内置数据库）。
3. 添加新用户，例如：
   - **用户名**：`pi-connector`
   - **密码**：强随机密码

请记录该用户名和密码，后续需在 PI Connector 配置中填写。

### 步骤 3：配置主题访问控制

将 PI Connector 的访问权限限制为其需要订阅的主题：

1. 在 EMQX Dashboard 中，点击**访问控制** -> **授权**。
2. 添加规则，允许 `pi-connector` 用户**订阅**相关主题模式，例如 `sensors/#`。
3. 如您的授权后端支持默认拒绝，请对该用户拒绝所有其他操作。

## 配置 PI Connector for MQTT

PI Connector for MQTT 通过 PI Connector Relay 的配置文件或管理界面进行配置，具体方式取决于所安装的版本。需设置的关键参数如下：

| 参数 | 值 |
|---|---|
| **代理地址** | EMQX 节点的主机名或 IP 地址 |
| **端口** | `1883`（TCP）或 `8883`（TLS） |
| **客户端 ID** | 唯一字符串，例如 `pi-connector-01` |
| **用户名** | `pi-connector`（在 EMQX 中创建的用户名） |
| **密码** | 在 EMQX 中设置的密码 |
| **主题订阅** | 需要订阅的 MQTT 主题，例如 `sensors/#` |
| **QoS** | 建议使用 `1`（至少一次），以确保可靠投递 |

TLS 连接还需配置：

| 参数 | 值 |
|---|---|
| **CA 证书** | 用于签署 EMQX 服务器证书的 CA 证书 |
| **TLS 版本** | TLS 1.2 或 1.3 |

配置文件路径和字段名称因安装版本而异，请参阅您所安装版本的 AVEVA PI Connector for MQTT 文档。

## 消息格式要求

<!-- TODO: 请核实 PI Connector for MQTT 区分 Type/Container/Data 消息类型的具体机制（主题后缀还是消息头字段），该机制因连接器版本而异。请查阅所安装版本对应的 AVEVA PI Connector for MQTT 发行说明或管理员指南进行确认。 -->
PI Connector for MQTT 要求消息符合 OMF（OSIsoft Message Format）格式。设备须向订阅主题发布符合 OMF 规范的 JSON 载荷。OMF 消息通过 MQTT 主题后缀或消息头字段（取决于连接器版本）标识消息类型：

- **Type 消息**：定义数据模式（仅在初始配置时发送一次）
- **Container 消息**：创建 PI 标签（仅在初始配置时发送一次）
- **Data 消息**：持续发送时序数值

最简 OMF 数据消息示例如下：

```json
[{
  "containerid": "sensor-001-temperature",
  "values": [{
    "Timestamp": "2024-01-15T10:30:00Z",
    "Value": 23.5
  }]
}]
```

如果设备发布的是非 OMF 格式的载荷（例如任意字段的 JSON），可使用 EMQX 规则引擎在独立主题上对消息进行重新格式化，再由 PI Connector 订阅该主题。详情请参阅[数据集成规则](../data-integration/rules.md)。

## 验证连接

完成两端配置后：

1. 在连接器主机上启动 PI Connector for MQTT。
2. 在 EMQX Dashboard 中，点击**监控** -> **客户端**，确认 Client ID 为 `pi-connector-01`（或您设置的 Client ID）的客户端显示为已连接。
3. 让设备向订阅主题发布测试消息，例如：

   ```bash
   mqttx pub -t "sensors/building-a/room-1" \
     -m '[{"containerid":"sensor-001-temperature","values":[{"Timestamp":"2024-01-15T10:30:00Z","Value":23.5}]}]'
   ```

4. 使用 PI System Explorer、PI Vision 或 PI DataLink 确认该数值已成功写入目标 PI 标签。

## 使用规则引擎进行载荷转换

如果设备无法发布符合 OMF 格式的载荷，EMQX 规则引擎可在 PI Connector 接收消息之前对任意 JSON 进行重新格式化。推荐模式如下：

1. 设备将原始载荷发布至输入主题，例如 `factory/line-1/+`。
2. 规则匹配该主题，提取字段后将重新格式化的 OMF 载荷重新发布至输出主题，例如 `omf/factory/#`。
3. PI Connector 订阅 `omf/factory/#`。

### 示例

某温湿度传感器向 `factory/line-1/sensor-001` 发布如下原始 JSON 载荷：

```json
{
  "device": "sensor-001",
  "temp_c": 72.4,
  "humidity": 58.2,
  "ts": "2024-01-15T10:30:00Z"
}
```

PI Connector 无法直接消费该格式。在 EMQX Dashboard 中[创建规则](../data-integration/rule-get-started.md#define-rule-sql)，使用以下 SQL 提取设备 ID、传感器数值和时间戳：

```sql
SELECT
  concat('pi-', payload.device, '-temp') AS container_id_temp,
  concat('pi-', payload.device, '-humidity') AS container_id_hum,
  payload.temp_c AS temp_value,
  payload.humidity AS hum_value,
  payload.ts AS timestamp
FROM
  "factory/line-1/+"
```

为该规则添加[**消息重新发布**](../data-integration/rule-get-started.md#add-republish-action)动作，将主题设置为 `omf/factory/line-1/${payload.device}`，载荷模板如下：

```json
[
  {
    "containerid": "${container_id_temp}",
    "values": [{ "Timestamp": "${timestamp}", "Value": ${temp_value} }]
  },
  {
    "containerid": "${container_id_hum}",
    "values": [{ "Timestamp": "${timestamp}", "Value": ${hum_value} }]
  }
]
```

EMQX 将此 OMF 格式的消息重新发布至 `omf/factory/line-1/sensor-001`。已订阅 `omf/factory/#` 的 PI Connector 接收该消息后，将向 PI Data Archive 写入两个数值：

- `72.4` 写入 PI 标签 `pi-sensor-001-temp`
- `58.2` 写入 PI 标签 `pi-sensor-001-humidity`

两个数值的时间戳均为 `2024-01-15T10:30:00Z`。

### 关于消息转换功能

当设备发布的是普通 JSON 载荷时，规则引擎 SQL 已足够处理。如果设备发布的是 Protobuf 或 Avro 等二进制编码载荷，则需要先配置 EMQX [消息转换](../data-integration/message-transformation.md)，将二进制载荷解码为 JSON，再由规则引擎对字段进行提取和处理。对于普通 JSON 载荷，无需使用消息转换功能。

## 安全建议

- 生产环境中，PI Connector 与 EMQX 之间的连接应使用 TLS（端口 8883）。
- 为 PI Connector 使用专用 EMQX 客户端凭据，并仅授予必要的主题访问权限。
- 定期轮换 PI Connector 的 EMQX 密码。
- 如安全策略要求客户端证书认证，请启用双向 TLS（mTLS）。
