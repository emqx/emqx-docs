# 从 Azure IoT Hub 迁移到 EMQX

本指南提供从 Azure IoT Hub 迁移物联网设备到 EMQX 的实用步骤演示。文中涵盖两种迁移路径：

1. **X.509 证书认证**：适用于使用客户端证书的设备
2. **SAS 令牌认证**：适用于使用共享访问签名（SAS）令牌并结合基于 HTTP 的认证的设备

## 迁移概览

对于使用 X.509 证书的设备，迁移主要是配置层面的变更。设备证书和私钥保持不变；仅需更新 Broker 端点地址和服务器 Certificate Authority (CA) 证书。必须将 EMQX 配置为信任 Azure 所信任的同一 CA，并复现 Azure 的身份映射模式，即证书的 Common Name（CN）等于 deviceId。

迁移过程由三个主要阶段组成：

1. **定位您的 CA 证书**：找到用于签发设备证书的 CA 证书。
2. **为 mTLS 配置 EMQX**：在 EMQX Broker 上设置 SSL/TLS 监听器，启用强制的对等端验证，并配置监听器信任您的 CA，并将证书 CN 映射为 deviceId。
3. **更新设备客户端**：更新设备代码以连接到新的 EMQX 端点，并信任 EMQX 服务器 CA 证书。设备可以继续使用 Azure IoT SDK 或使用标准 MQTT 客户端。

下表总结了参数变更：

| **参数**                         | **Azure IoT Hub（示例）**                   | **EMQX（示例）**                       | **说明**                     |
| -------------------------------- | ------------------------------------------- | -------------------------------------- | ---------------------------- |
| **端点主机名**                   | `my-hub.azure-devices.net`                  | `mqtt.example.com`                     | 更新设备客户端代码           |
| **设备证书**                     | `device-001.cert.pem`                       | `device-001.cert.pem`                  | 无变更，继续使用现有证书     |
| **设备私钥**                     | `device-001.key.pem`                        | `device-001.key.pem`                   | 无变更，继续使用现有私钥     |
| **服务器验证（设备验证服务器）** | 设备信任 Azure 的公共 CA                    | 设备必须信任 `emqx-server-ca.pem`      | 向设备部署 EMQX 服务器 CA    |
| **客户端验证（服务器验证设备）** | Azure 信任您的 CA（通过 CA 上传或指纹注册） | EMQX `cacertfile` 必须设置为您的 CA    | 与 Azure 使用相同的 CA       |
| **身份映射**                     | Azure 提取 `CN=deviceId`                    | 启用 `mqtt.peer_cert_as_clientid = cn` | 保持基于 deviceId 的授权模型 |

## 阶段 1：定位您的 CA 证书

在迁移到 EMQX 之前，您需要获取用于签发设备证书的 CA 证书（PEM 格式，例如 `device-ca.pem`）。这是 EMQX 验证设备身份的关键材料。

Azure IoT Hub 支持两种 X.509 注册方式：

- **CA 注册**：您将 CA 上传到 Azure IoT Hub。您需要找到当时上传的那份 CA 文件。
- **指纹注册（Thumbprint）**：您通过设备证书的指纹逐个注册每台设备。虽然您没有上传 CA，但这些设备证书仍然来自某个 CA（例如内部 CA、自签 CA 或企业 PKI）。 您必须找到签发设备证书的那份 CA。

无论采用哪种方式，证书层级结构相同：设备证书始终由您自己的 CA 签发。因此，迁移到 EMQX 时，您需要获取此 CA 证书，以便 EMQX 验证设备证书。

### 确定设备证书的签发 CA

可以使用 OpenSSL 查看设备证书的 Issuer 字段：

```bash
openssl x509 -in device-001.cert.pem -noout -issuer
```

预期输出类似：

```
issuer=CN = Azure-Device-CA
```

`Azure-Device-CA.pem` 就是您需要提供给 EMQX 的 CA 证书。这是定位 CA 的最可靠方法。

### 验证证书要求

Azure 要求证书的主题 Common Name（CN）与 deviceId 匹配（模块设备则为 `deviceId/moduleId`）。可使用以下命令验证：

```bash
openssl x509 -in device-001.cert.pem -noout -subject
```

输出示例：

```
subject=CN = device-001
```

EMQX 会在 mTLS 认证期间提取此 CN，并将其作为设备标识。

### 确保设备能访问其自身的凭据

每个设备必须安全地访问以下凭据：

- 设备的叶子证书（`device-001.cert.pem`）
- 设备的私钥（`device-001.key.pem`）

由于 Azure IoT Hub 和 EMQX 均使用标准 X.509 认证，因此此迁移路径不需要重新签发设备证书。

## 阶段 2：配置 EMQX 以支持与 Azure 兼容的 mTLS

将 EMQX 配置为使用与 Azure IoT Hub 相同的 CA 和身份映射规则来对设备进行 X.509 认证。

### 启用并配置 mTLS Listener

将 EMQX 配置为在 SSL Listener 上启用双向 SSL/TLS（mTLS）认证。有关 SSL/TLS 配置的详细信息，请参阅：[开启 SSL/TLS 连接](../network/emqx-mqtt-tls.md)。

打开 EMQX 配置文件（`emqx.conf`），或使用 Dashboard（**Management** -> **Listeners**）设置 SSL/TLS Listener：

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  ssl_options {
    # EMQX 服务器证书
    certfile = "etc/certs/server-cert.pem"

    # EMQX 服务器私钥
    keyfile = "etc/certs/server-key.pem"

    # --- 用于设备认证的 mTLS 配置 ---

    # 签发设备证书的 CA
    cacertfile = "etc/certs/azure-device-ca.pem"

    # 启用客户端证书验证
    verify = verify_peer

    # 拒绝无证书连接
    fail_if_no_peer_cert = true
  }
}
```

::: tip
Azure IoT Hub 与 EMQX 均使用 `8883` 作为 MQTT over TLS/SSL 默认端口，因此设备无需修改端口设置。
:::

**关键配置项说明：**

- `cacertfile`：用于验证设备证书的 CA（或自签设备证书的证书包）。
- `verify`：必须设置为 `verify_peer` 以启用 mTLS。
- `fail_if_no_peer_cert`：必须设置为 `true` 以强制要求客户端提供证书。

### 复现 Azure 的 `CN=deviceId` 身份映射

Azure 从证书的 Common Name 提取 deviceId 并用于授权。在 EMQX 中可通过以下配置复现：

```
mqtt.peer_cert_as_clientid = cn
mqtt.peer_cert_as_username = cn
```

该配置确保：

- MQTT ClientID 自动设置为证书 CN（deviceId）
- 用户名也设置为证书 CN
- 您可以使用 `${clientid}` 或 `${username}` 配置 ACL，从而保持 Azure 的授权模型

对于模块设备（`deviceId/moduleId` 格式），CN 包含两个标识符，可直接在 EMQX ACL 中使用。

### 应用配置更改

更新配置文件后，重新加载配置：

```bash
emqx ctl conf reload
```

如果通过 Dashboard 修改，请点击**更新**应用更改。监听器将自动重启。

验证监听器是否强制执行 mTLS：

```bash
openssl s_client -connect mqtt.example.com:8883 -showcerts
```

无客户端证书时连接应失败。

## 阶段 3：更新设备客户端并验证迁移

最后阶段是将设备客户端代码更新为连接到 EMQX，而不是 Azure IoT Hub。

### 准备 EMQX 服务器 CA 证书

更新设备代码前，需要获取 EMQX 服务器的 CA 证书，即用于签发 EMQX TLS 服务器证书的 CA。

**如果 EMQX 使用自签服务器证书**，则必须将其 CA 添加到设备的系统信任证书库中：

**Linux：**

```bash
sudo cp emqx-server-ca.pem /usr/local/share/ca-certificates/emqx-ca.crt
sudo update-ca-certificates
```

**macOS：**

```bash
sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain emqx-server-ca.pem
```

**Windows：**

```bash
Import-Certificate -FilePath emqx-server-ca.pem -CertStoreLocation Cert:\LocalMachine\Root
```

::: tip
如果 EMQX 使用公网 CA（如 Let's Encrypt）签发的证书，则无需执行此步骤，因为系统已信任该 CA。
:::

### 更新设备客户端代码

Azure IoT Python SDK（以及其他语言 SDK）支持通过 `server_verification_cert` 和自定义 `hostname` 参数连接至自定义 MQTT Broker，从而实现最小化修改。

**Python 示例：**

```python
from azure.iot.device import IoTHubDeviceClient, X509

x509 = X509(
    cert_file="certs/device-001.cert.pem",
    key_file="certs/device-001.key.pem"
)

with open("certs/emqx-server-ca.pem", "r") as f:
    emqx_server_ca = f.read()

client = IoTHubDeviceClient.create_from_x509_certificate(
    x509=x509,
    hostname="mqtt.example.com",
    device_id="device-001",
    server_verification_cert=emqx_server_ca
)

client.connect()
client.send_message("Hello from migrated device")
```

::: tip

- `server_verification_cert` 参数需要证书内容字符串，而非文件路径。
- 如果您已将 EMQX 服务器 CA 添加到系统信任库，可省略此参数。
- 继续使用 Azure IoT SDK 能保持现有应用逻辑，只需少量配置修改，是已使用 X.509 认证设备的最简迁移路径。
   :::

### 设备端参数总结

迁移时需要调整的参数如下：

1. **端点/主机名**：
   - Azure：`my-hub.azure-devices.net`
   - EMQX：`mqtt.example.com`
2. **服务器 CA 证书**：
   - Azure：系统信任库或 Azure CA
   - EMQX：必须显式提供 `emqx-server-ca.pem`
3. **设备凭据（无需变更）**：
   - 证书：继续使用现有设备证书
   - 私钥：继续使用现有设备私钥
4. **ClientId**：设置为 deviceId（与证书 CN 匹配）

### 验证检查清单

1. 设备在 EMQX Dashboard 中显示 `clientid = deviceId`。
2. TLS 握手成功，设备证书验证通过。
3. 设备可以发布被授权的主题。
4. 设备可以订阅被授权的主题。
5. EMQX 日志中无认证错误。

## 标准迁移路径的常见变式

除了上述核心迁移流程外，一些设备规模可能遵循稍有不同但仍属于同一 X.509 迁移路径的简单变式。本节介绍两个常见的变式场景，并说明 EMQX 如何无需修改设备证书或固件即可支持它们。

### 由同一 CA 签发证书的设备群

- 将 CA 证书上传至 EMQX。
- 所有由此 CA 签发的设备证书将自动被信任。
- 证书生命周期管理保持集中和简化。
- 可随时新增设备，无需对 EMQX 做任何配置修改。

此方案与 Azure IoT Hub 的 CA 注册模式一致，是大规模设备群的最可扩展迁移方式。

### 使用模块的设备（`deviceId/moduleId`）

- 证书 CN 格式为 `deviceId/moduleId` 的设备完全受支持。
- EMQX 可直接使用完整 CN 进行身份映射和认证。
- 授权规则（ACL）可引用完整 CN，从而保持 Azure 的模块级访问控制行为。

这使 Azure 模块体系结构中的设备无需变更证书或身份逻辑即可无缝迁移。

## 备选方案：使用 HTTP 服务认证器的 SAS 令牌认证

如果您的设备使用 Azure SAS 令牌，可以通过实现一个简单的 HTTP 认证服务继续在 EMQX 中使用此认证方式。有关 HTTP 认证的详细信息，请参阅：[使用 HTTP 服务进行密码认证](../access-control/authn/http.md)。

### SAS 令牌认证的工作方式

Azure IoT Hub 通过 MQTT 的用户名和密码字段传递 SAS 凭据：

- **用户名**：`{iothubhostname}/{deviceId}/?api-version=2021-04-12`
- **密码**：`SharedAccessSignature sr={resource}&sig={signature}&se={expiry}`

EMQX 会将这些字段转发给您的 HTTP 服务，由其执行实际的 SAS 令牌验证。

### 为 SAS 令牌实现 HTTP 认证

1. 创建一个 HTTP 认证服务。该服务应执行以下任务：
   - 接收来自 EMQX 的用户名和密码。
   - 从用户名中提取 `deviceId`。
   - 从密码字段解析 SAS 令牌。
   - 使用设备对称密钥验证令牌签名。
   - 检查令牌过期时间（`se` 字段）。
   - 根据验证结果返回 `{"result": "allow"}` 或 `{"result": "deny"}`。
2. 配置 EMQX 使用该 HTTP 服务，可通过 Dashboard 或配置文件添加 HTTP 服务认证器：

```hocon
authentication = [
  {
    mechanism = password_based
    backend = http
    method = post
    url = "http://your-auth-service:8080/auth"
    body {
      username = "${username}"
      password = "${password}"
      clientid = "${clientid}"
    }
    headers {
      "Content-Type" = "application/json"
    }
  }
]
```

3. 配置设备凭据：从 Azure IoT Hub 身份注册表导出设备标识和对称密钥，并将其存储在 HTTP 认证服务使用的数据库中，以便验证 SAS 签名。

### HTTP 认证服务响应示例

服务应返回类似以下内容的 JSON：

```json
{
  "result": "allow",
  "is_superuser": false,
  "client_attrs": {
    "device_id": "device-001"
  }
}
```

::: tip
此方法允许基于 SAS 令牌的设备在无需修改固件的情况下迁移至 EMQX。但从长期可移植性和安全性来看，建议迁移到 X.509 证书认证。
:::

## 总结

从 Azure IoT Hub 迁移设备到 EMQX 时，可根据设备当前使用的认证方式选择不同路径。

### X.509 证书认证设备

这是最简单、最直接的迁移方式。设备现有证书和私钥保持不变，只需：

- 配置 EMQX 信任 Azure 所使用的 CA
- 启用 mTLS 和基于证书的身份映射
- 更新设备的端点和服务器 CA 证书

完成这些配置后，设备即可在保持相同安全模型和证书流程的前提下迁移到 EMQX。

### SAS 令牌认证设备

基于 Azure SAS 令牌的设备可通过实现 HTTP 认证服务继续在 EMQX 中使用 SAS 令牌认证，从而实现无需固件修改的迁移。

然而，为了更好的可移植性与安全性，长期建议迁移到 X.509 证书认证。

::: tip
如果部署中同时包含 X.509 和 SAS 令牌设备，建议先迁移 X.509 设备以降低工作量、加速验证。然后评估 SAS 设备是否使用 HTTP 认证以实现快速兼容，或改用 X.509 证书以获得更好的长期维护性。
:::
