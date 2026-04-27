# 使用 curl 连接 EMQX

curl 是一款广泛使用的命令行工具，用于数据传输与自动化。自 2020 年起，curl 支持 MQTT 协议；从 curl 8.19.0（预计 2026 年 3 月）开始，它还支持 MQTTS（基于 TLS 的 MQTT）。

借助 curl，开发者无需安装任何特定编程语言的 MQTT 客户端 SDK，即可直接在命令行中连接 EMQX、发布消息并订阅主题。这使得 curl 成为快速测试、脚本编排与物联网原型开发的便捷选择。

本页面介绍如何使用 curl 与 EMQX 进行 MQTT 与 MQTTS 通信，包括连接、发布/订阅、身份验证、TLS 配置，以及常见故障排查场景。

## curl 版本要求

| 功能               | 最低版本 | 发布时间       |
| ------------------ | -------- | -------------- |
| MQTT (`mqtt://`)   | 7.70.0   | 2020 年 4 月   |
| MQTTS (`mqtts://`) | 8.19.0   | 2026 年 3 月初 |

检查您已安装的 curl 版本：

```bash
curl --version
```

请确保 **Protocols** 列表中包含 `mqtt`（且在 curl ≥ 8.19.0 时也包含 `mqtts`）。

> 如果您的 curl 版本过旧，请通过包管理器升级，或从 https://curl.se/download 下载。
>
> 在 macOS 上，可使用 `brew install curl` 安装最新版本。

## MQTT Broker 设置

您需要一个 MQTT Broker 才能进行连接。本指南使用 EMQX，它同时支持 MQTT 与 MQTTS。

### EMQX 公共 Broker（测试用途）

若您希望快速测试且不部署自己的 Broker，可以使用 EMQX 公共 Broker。

| 参数       | 值               |
| ---------- | ---------------- |
| Broker     | `broker.emqx.io` |
| MQTT 端口  | `1883`           |
| MQTTS 端口 | `8883`           |

该公共 Broker 仅用于测试与演示目的。

### EMQX Enterprise 部署

在生产场景中，请使用您环境中定义的 Broker 地址、端口、认证凭据与 TLS 配置，将 curl 连接到您自己的 EMQX Enterprise 部署。

典型配置包括：

- 自定义 Broker 主机名或 IP 地址
- 在 EMQX Enterprise 中启用 MQTT 和/或 MQTTS 监听器
- 用户名/密码认证、基于令牌（token）的认证或双向 TLS（mutual TLS）
- 对主题应用访问控制规则（ACL）

在构造 curl 命令时，请参考您 EMQX Enterprise 的监听器、认证与 TLS 配置。

**说明**

- 当您针对自己的部署进行测试时，请将所有示例中的 `broker.emqx.io` 替换为您 EMQX Enterprise 的 Broker 地址。
- 连接前请确保在 EMQX Enterprise 中已启用相应的 MQTT 或 MQTTS 监听器。

> 除了自托管的 EMQX Enterprise 部署外，您也可以使用 curl 连接全托管的 MQTT 服务：[EMQX Cloud](https://docs.emqx.com/zh/cloud/latest/)（Serverless 或 Dedicated）。
>
> curl 的 MQTT/MQTTS 用法保持一致，仅需使用 EMQX Cloud 提供的 Broker 地址、端口及认证信息。

### 使用 curl 连接 EMQX Enterprise

在 MQTT 中，客户端会在执行某个操作（例如订阅主题或发布消息）时建立与 Broker 的连接。MQTT 并没有独立的“connect”命令。

当您使用 curl 连接 EMQX Enterprise 时，只要您使用 Enterprise 的 Broker 地址运行订阅或发布命令（并在需要时配置认证或 TLS），连接就会自动建立。

例如，下面的命令会在一步中连接到 EMQX Enterprise 并订阅某个主题：

```bash
curl -N mqtts://your-enterprise-broker.example.com/curl/test
```

> **注意：** MQTTS (`mqtts://`) 需要 curl 8.19.0 或更高版本。对于 curl 7.70.0 到 8.18.x 版本，请使用 `mqtt://`。

## 了解 curl 的 MQTT URL Scheme

curl 使用基于 URL 的语法来执行 MQTT 操作：

```
mqtt[s]://[user:password@]broker[:port]/topic
```

其中：
- `mqtt[s]` 表示协议（`mqtt` 或 `mqtts`）。
- `[user:password@]` 为可选的认证信息。
- `broker` 为 Broker 主机名或 IP 地址。
- `[:port]` 为可选的端口号。如果未指定端口，curl 将使用默认端口：
  - `mqtt://` 使用 `1883`
  - `mqtts://` 使用 `8883`
- `/topic` 为 MQTT 主题路径，例如：`/sensor/temperature`。

## curl 的 MQTT 输出格式

当订阅主题时，curl 会按如下格式输出原始 MQTT 消息数据：

```
[2 bytes: topic length (big-endian)] [topic string] [payload]
```

例如，在主题 `curl/test` 上收到的消息 `"hello"` 会显示为：

```
curl/testhello
```

输出是二进制格式，主题名称和 payload 连接在一起，不进行解析难以阅读。

该输出是二进制格式，默认不可读。下面的[解析 MQTT 消息](#解析-mqtt-消息)提供了转换为可读格式的示例。

## 订阅主题

订阅操作会保持连接打开，并将收到的消息输出到 `stdout`。

### 基础订阅（未加密）

```bash
curl -N mqtt://broker.emqx.io/curl/test --output messages.bin
```

`-N` 选项会禁用输出缓冲，使消息能够立即显示。

### 使用 MQTTS 的安全订阅（curl ≥ 8.19.0）

```bash
curl -N mqtts://broker.emqx.io/curl/test --output messages.bin
```

### 使用认证的订阅

```bash
curl -N -u "username:password" \
  mqtts://your-broker.emqxsl.com/curl/test --output messages.bin
```

## 解析 MQTT 消息

当订阅主题时，curl 输出 MQTT 消息时使用的是二进制格式，而非格式化的文本表示。

每条入站消息的结构为：

- 2 字节：主题长度（大端序 big-endian）
- 主题字符串
- Payload（原始字节）

因此，输出会表现为主题与 payload 的拼接，默认不可读。

### Bash 一行命令

要让订阅输出可读，可以将 curl 输出通过一个 shell 解析器进行管道处理：

```bash
curl -sN mqtt://broker.emqx.io/curl/test | \
  while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curl's MQTT subscribe output is: 2-byte topic length (MSB,LSB), topic, payload.
    # This loop uses NUL (0x00) as a delimiter, so we are seeing the LSB first and
    # implicitly using MSB=0 (works for topic lengths 0..255).
    lsb=$(printf "%d" "'${d:0:1}")
    topic_len=$((lsb))
    echo "[${d:1:$topic_len}] ${d:$((1 + topic_len))}"
  done
```

输出格式如下：

```
[curl/test] hello
```

该解析器采用了一种适用于演示场景的简化处理方式。

其工作流程如下：

1. 将数据流按空字节（null bytes）进行切分。
2. 从主题长度字段的两个字节中，取其低字节作为主题长度。该方式仅在主题长度小于 256 字节、且高字节为 0 的情况下有效。
3. 使用长度前缀提取主题字符串与 payload。
4. 将每条消息按 `[topic] payload` 格式输出。

### 保存原始输出以便检查

为了更好地理解 MQTT 消息的二进制结构，您可以将原始订阅输出保存到文件中并手动查看。

保存输出：

```bash
curl -sN mqtt://broker.emqx.io/curl/test > messages.bin
```

使用 `hexdump` 检查文件：

```bash
hexdump -C messages.bin
```

这样您可以清晰地看到主题长度前缀、主题字节与 payload 布局。

### 可复用的 Shell 函数

若需重复使用，可以将解析器封装成可复用的 shell 函数：

```bash
mqtt_subscribe() {
  curl -sN "$1" | while IFS= read -r -d $'\0' d; do
    [ -z "$d" ] && continue

    # curl's MQTT subscribe output is: 2-byte topic length (MSB,LSB), topic, payload.
    # This loop uses NUL (0x00) as a delimiter, so we are seeing the LSB first and
    # implicitly using MSB=0 (works for topic lengths 0..255).
    lsb=$(printf "%d" "'${d:0:1}")
    topic_len=$((lsb))
    echo "[${d:1:$topic_len}] ${d:$((1 + topic_len))}"
  done
}
```

用法示例：

```bash
mqtt_subscribe "mqtt://broker.emqx.io/curl/test"
```

> 在生产使用或存在复杂解析需求时，建议使用 [MQTTX CLI](https://mqttx.app/zh/cli)。它提供正确格式化的输出、完整的 MQTT 5.0 支持、QoS 处理，以及通配符订阅能力。

## 发布消息

发布消息时，请使用 curl 的 `-d`（data）选项指定消息 payload。

### 基础发布（未加密）

```bash
curl -d "Hello from curl" \
  mqtt://broker.emqx.io/curl/test
```

### 使用 MQTTS 的安全发布（curl ≥ 8.19.0）

```bash
curl -d "Secure message from curl" \
  mqtts://broker.emqx.io/curl/test
```

### 发布 JSON Payloads

```bash
curl -d '{"sensor_id":"temp-001","value":23.5}' \
  mqtt://broker.emqx.io/sensors/temperature
```

### 使用认证发布

```bash
curl -u "username:password" \
  -d '{"status":"online"}' \
  mqtts://your-broker.example.com/devices/status
```

## curl 相关选项

下表汇总了本页中使用到的 curl 命令行选项。

| 选项           | 说明                           | 常见用途 |
| -------------- | ------------------------------ | -------- |
| `-N`           | 禁用输出缓冲（订阅时必需）     | 订阅     |
| `-d`           | 要发布的消息 payload           | 发布     |
| `-u user:pass` | 用户名与密码认证               | 认证     |
| `-v`           | 详细输出（展示 MQTT 握手过程） | 故障排查 |
| `-s`           | 静默模式（抑制进度输出）       | 脚本     |
| `--cacert`     | 用于 TLS 校验的 CA 证书        | MQTTS    |
| `--cert`       | 用于双向 TLS 的客户端证书      | MQTTS    |
| `--key`        | 用于双向 TLS 的客户端私钥      | MQTTS    |
| `-k`           | 跳过 TLS 校验（仅用于测试）    | 故障排查 |

> 如需完整的 curl 选项列表，请参阅官方 curl 文档。

## TLS 配置（MQTTS）

### CA 证书校验

```bash
curl --cacert /path/to/ca.crt \
  -d "TLS verified message" \
  mqtts://your-broker.example.com/secure/topic
```

### 双向 TLS（mTLS）

```bash
curl --cacert /path/to/ca.crt \
  --cert /path/to/client.crt \
  --key /path/to/client.key \
  -d "mTLS message" \
  mqtts://your-broker.example.com/secure/topic
```

> `-k` 仅用于测试时跳过证书校验。

## 常见使用场景

### Broker 连通性测试

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

该命令可验证 DNS 解析、TCP 连通性以及 MQTT 握手。

**成功连接的指示信息**

当连接成功时，详细输出通常会显示：

- Broker 主机名解析为一个或多个 IP 地址。
- 与 `1883`（MQTT）或 `8883`（MQTTS）端口成功建立 TCP 连接。
- MQTT 握手顺利完成且无错误。

如果没有输出错误且命令正常结束，则表示已建立到 EMQX Broker 的连接。

### Shell 脚本与 IoT 原型开发

示例：模拟温度传感器每 5 秒发布一次数据。

```bash
#!/bin/bash
BROKER="mqtt://broker.emqx.io"
TOPIC="sensors/room1/temperature"

while true; do
  TEMP=$(awk -v min=20 -v max=30 'BEGIN{srand(); print min+rand()*(max-min)}')
  PAYLOAD="{\"temperature\": $TEMP, \"timestamp\": $(date +%s)}"
  curl -s -d "$PAYLOAD" "$BROKER/$TOPIC"
  sleep 5
done
```

## curl 在 MQTT 场景下的限制

虽然 curl 对测试与脚本化非常有用，但也存在以下限制：

| 限制项         | 说明                     |
| -------------- | ------------------------ |
| 仅支持 QoS 0   | 不支持 QoS 1 或 2        |
| 二进制输出     | 订阅输出未格式化         |
| 不支持通配符   | 无法使用 `+` 或 `#` 订阅 |
| 单主题         | 每个命令仅支持一个主题   |
| 不支持持久会话 | 连接无状态               |

如需高级 MQTT 特性，请使用 [MQTTX CLI](https://mqttx.app/zh/cli) 或 EMQX 客户端 SDK。

## 验证 curl 是否支持 MQTT

```bash
curl --version | grep -i mqtt
```

如果命令输出中包含 `mqtt`（或 curl ≥ 8.19.0 时的 `mqtts`），则您的 curl 构建包含 MQTT 支持。如果缺少 MQTT，您可能需要：

- 升级 curl 到 7.70.0 或更高版本
- 安装启用了 MQTT 的 curl 构建版本
- 从源码编译 curl 时使用 `--enable-mqtt`

## 故障排查

本节列出使用 curl 连接 EMQX 时的常见问题以及解决方法。

### 连接被拒绝或超时

**问题描述**

- `Connection refused`
- `Failed to connect to broker`
- 连接挂起后超时

**可能原因**

- Broker 地址或端口不正确
- 网络防火墙阻止 MQTT/MQTTS 端口
- Broker 未运行或未在指定端口监听

**解决方法**

- 核对 Broker 地址与端口：
  - MQTT：`1883`
  - MQTTS：`8883`
- 使用 verbose 模式检查网络连通性：

```bash
curl -v mqtt://broker.emqx.io/curl/test
```

### curl 不支持 MQTT 或 MQTTS

**问题描述**

- `Protocol "mqtt" not supported`
- `Unknown protocol`

**可能原因**

- curl 构建未包含 MQTT 支持
- curl 版本过旧

**解决方法**

- 验证协议支持：

```bash
curl --version
```

确保 **Protocols** 列表中包含 `mqtt`（以及 TLS 场景下的 `mqtts`）。

- 升级 curl 或安装启用了 MQTT 的构建版本。

### TLS 握手或证书错误（MQTTS）

**问题描述**

- `SSL certificate problem`
- `TLS handshake failed`
- `Unable to get local issuer certificate`

**可能原因**

- 缺少或使用了不正确的 CA 证书
- Broker 使用私有或自签名证书

**解决方法**

- 显式指定 CA 证书：

```bash
curl --cacert /path/to/ca.crt \
  mqtts://your-broker.example.com/topic
```

- 仅测试时可跳过校验（生产不推荐）：

```bash
curl -k mqtts://your-broker.example.com/topic
```

### 订阅时收不到消息

**问题描述**

- 订阅命令在运行，但没有输出

**可能原因**

- 输出缓冲未禁用
- 没有向该主题发布消息
- 主题不匹配

**解决方法**

- 订阅时始终使用 `-N`：

```bash
curl -N mqtt://broker.emqx.io/curl/test
```

- 确认消息发布到相同主题。

### 认证失败

**问题描述**

- 连接立即关闭
- Broker 日志中出现认证或授权错误

**可能原因**

- 用户名或密码错误
- 主题存在 ACL 限制

**解决方法**

- 核对认证信息：

```bash
curl -u "username:password" \
  mqtts://your-broker.example.com/topic
```

- 检查 EMQX 的认证与 ACL 配置。

## 更多信息

如需更详细的、循序渐进的 curl MQTT/MQTTS 使用指南（包括背景解释、更多示例与使用注意事项），请参阅博客文章：[Using curl for MQTT: Connect, Publish, and Subscribe with Secure IoT Communication](https://www.emqx.com/en/blog/using-curl-for-mqtt)。

该博客可作为本指南的补充，提供更深入的解释与扩展示例。

