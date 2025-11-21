# 从 AWS IoT Core 迁移到 EMQX

本页提供了从 AWS IoT Core 迁移 IoT 设备到 EMQX 的完整操作指南。文中说明了如何重新配置设备和 EMQX Broker，以确保整个设备群能够顺利迁移。

本指南关注两个平台都支持的最常用且最可靠的身份认证方式：X.509 客户端证书（mTLS）认证。本指南假设您使用的是在 AWS IoT Core 中注册的自定义证书颁发机构（CA）来签发设备证书。如果您的设备使用的是 AWS 签发的“一键式（one-click）”证书，则这些证书无法重复使用。AWS 不向用户公开用于签发这些证书的中间 CA，因此这些证书无法被 EMQX 这样的 MQTT Broker 所信任。在这种情况下，您必须创建自己的 CA 并重新为设备签发证书。

## 迁移概览：标准流程

对于使用 AWS IoT Core 中由您自定义 CA 签发的 X.509 客户端证书（mTLS）的设备，迁移到 EMQX 十分简单。包括官方 AWS IoT Device SDK 在内的标准兼容客户端，只需对客户端代码进行最少的更改：仅需更新端点和服务器 CA 证书。设备现有的证书和私钥仍然有效。

迁移过程包括三个主要阶段：

1. **准备您的 CA 证书**：找到您在 AWS IoT Core 中注册并用于签发设备证书的自定义 CA 证书。
2. **为 mTLS 配置 EMQX**：在 EMQX Broker 上设置 SSL/TLS 监听器，启用强制的对等方验证，并配置监听器信任您的 CA。
3. **更新设备客户端**：将设备客户端代码更新为新的 EMQX 端点地址以及 EMQX 服务器的 CA 证书，用于服务器身份验证。

以下表格总结了所需变更的概览。

| **参数**                         | **AWS IoT Core（示例）**                        | **EMQX（示例）**                                     | **说明**                                               |
| -------------------------------- | ----------------------------------------------- | ---------------------------------------------------- | ------------------------------------------------------ |
| **Endpoint 主机名**              | `agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com` | `mqtt.example.com`                                   | 更新设备端客户端代码/固件                              |
| **设备证书**                     | `device-001.cert.pem`                           | `device-001.cert.pem`                                | 无需更改。设备继续使用由您的 CA 签发的现有证书。       |
| **设备私钥**                     | `device-001.key.pem`                            | `device-001.key.pem`                                 | 无需更改。设备继续使用现有私钥。                       |
| **服务器验证**（设备验证服务器） | 客户端使用 `AmazonRootCA1.pem`                  | 客户端需更新为使用 `emqx-server-ca.pem`              | 客户端必须信任签发 EMQX 服务器证书的 CA。              |
| **客户端验证**（服务器验证设备） | AWS IoT Core 信任您注册的 CA                    | EMQX Listener 的 `cacertfile` 需设置为 `your-ca.pem` | EMQX 必须配置为信任您在 AWS IoT Core 中注册的相同 CA。 |

## 阶段 1：准备您的 CA 证书

本指南假设您使用的是 AWS IoT Core 的 “Bring Your Own CA”（BYOCA）功能注册的自定义 CA。您的设备证书由该 CA 签发，而不是由 AWS 的专有中间 CA 签发。

**操作**：找到您的 CA 证书文件（例如 `my-company-ca.pem`）。这是您在 AWS IoT Core 中注册并用于签发设备证书的同一个 CA。

您可以通过检查设备证书来验证由哪个 CA 签发：

```
openssl x509 -in device-001.cert.pem -text -noout | grep "Issuer"
```

Issuer 应当与您的组织 CA 匹配，而不是 AWS 的中间 CA。

::: tip
**如果您使用 AWS 签发的证书**：AWS IoT Core 的“一键式”证书生成依赖 AWS 的专有中间 CA，这些 CA 对用户不可访问。如果设备使用 AWS 签发的证书，在迁移到 EMQX 之前，您需要创建自己的 CA 并重新为设备签发证书。这超出了本指南的范围，但您可以使用 OpenSSL 或 PKI 解决方案创建 CA 并签发设备证书。
:::

## 阶段 2：为 EMQX 配置 mTLS 认证

定位到 CA 证书后，下一步是配置 EMQX Broker，以接受并验证由您的 CA 签发的设备证书。

### 启用并配置 mTLS 监听器

迁移的核心是为 EMQX 监听器启用双向 SSL/TLS（mTLS）认证。此配置指示 EMQX 要求客户端提供证书，并根据您的 CA 验证其真实性。

有关 SSL/TLS 配置项和证书管理的详细信息，请参见：

- [开启 SSL/TLS 连接](../network/emqx-mqtt-tls.md)
- [获取 SSL/TLS 证书](../network/tls-certificate.md)

**操作**：打开 EMQX 配置文件（如 `emqx.conf`），配置 SSL/TLS 监听器，或在 Dashboard 中通过**管理 **-> **监听器**配置：

```hocon
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  ssl_options {
    # EMQX 服务器证书
    certfile = "etc/certs/server-cert.pem"

    # EMQX 服务器私钥
    keyfile = "etc/certs/server-key.pem"

    # --- 用于设备认证的 mTLS 配置 ---

    # 这是您的 CA 证书（来自阶段 1）
    # 与您在 AWS IoT Core 注册的 CA 相同
    cacertfile = "etc/certs/my-company-ca.pem"

    # 启用客户端证书验证
    verify = verify_peer

    # 拒绝未提供证书的客户端
    fail_if_no_peer_cert = true
  }
}
```

::: tip
AWS IoT Core 和 EMQX 都使用端口 `8883` 作为 MQTT over TLS/SSL 的默认端口，因此设备无需更改端口号。
:::

**关键配置项**：

- `cacertfile`：您在 AWS IoT Core 中注册的 CA 证书路径，用于 EMQX 验证设备证书。
- `verify`：必须设置为 `verify_peer` 才能启用 mTLS。
- `fail_if_no_peer_cert`：必须设置为 `true`，以拒绝未提供证书的客户端，从而强制执行 mTLS。
- `certfile` 和 `keyfile`：EMQX 服务器自身的证书和私钥，客户端将验证该证书以确保正在连接正确的 Broker。

更新配置文件后，重新加载配置：

```
emqx ctl conf reload
```

如果通过 Dashboard 更新，点击**更新**以应用更改。Listener 会自动重启以生效配置。

### （可选）将证书 CN 映射为 ClientID 或 Username

在许多 AWS IoT Core 实现中，授权策略依赖由证书字段填充的变量，例如使用证书的 Common Name（CN）作为 `iot:ClientId`。EMQX 可以无缝复现这种行为，便于迁移授权规则。

**操作**：要自动使用设备证书填充 ClientID 或 Username，请在 `emqx.conf` 中添加：

```
# 使用证书 CN 作为 ClientID
mqtt.peer_cert_as_clientid = cn

# 使用证书 CN 作为 Username
mqtt.peer_cert_as_username = cn
```

此配置指示 EMQX 在 TLS 握手过程中从客户端证书中提取 Common Name（或 `dn`，即 Distinguished Name），并将其作为 MQTT 会话的 ClientID 或 Username。这样可确保现有的授权逻辑（如基于 `${clientid}` 或 `${username}` 的 ACL）在迁移后仍能正常工作。

例如，如果设备证书的 CN=device-001，则启用 `mqtt.peer_cert_as_clientid = cn` 后，MQTT ClientID 将自动设为 “device-001”。

## 阶段 3：更新设备客户端并验证迁移

最后一步是将设备客户端代码更新为连接新的 EMQX Broker。这里以官方的 [AWS IoT Device SDK for Python v2](https://github.com/aws/aws-iot-device-sdk-python-v2) 为例进行说明。

AWS IoT SDK 不依赖 AWS 平台本身，实际上是标准兼容的 MQTT-over-TLS 客户端。这意味着现有基于该 SDK 的应用代码可以保留，仅需更改连接端点和服务器 CA 证书参数。

### 客户端代码修改（Python 示例）

基于 `aws-iot-device-sdk-python-v2` 的 `mqtt5_client_builder` 模块，从 AWS IoT Core 迁移到 EMQX 时需要更新以下连接参数：

1. **更新 Endpoint**
   - AWS：`endpoint="agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com"`
   - EMQX：`endpoint="mqtt.example.com"`（您的 EMQX Broker 的主机名/FQDN）
2. **更新服务器 CA 证书** (`ca_filepath`)
   - 用于设备验证 EMQX 服务器身份
   - AWS：通常省略（使用系统信任库）或使用 `AmazonRootCA1.pem`
   - EMQX：`ca_filepath="emqx-server-ca.pem"`
3. **设备证书保持不变**
   - 设备证书 (`cert_filepath`)：无需更改
   - 设备私钥 (`pri_key_filepath`)：无需更改

### 完整示例：使用 AWS SDK 连接 EMQX

以下示例展示如何使用 AWS IoT Device SDK for Python v2 连接到 EMQX。示例脚本基于 `samples/mqtt/mqtt5_x509.py`，仅需少量修改。

**AWS IoT Core 版本（迁移前）**：

```bash
python3 mqtt5_x509.py \
  --endpoint agwba84cbf2pn-ats.iot.eu-west-1.amazonaws.com \
  --cert device-001.cert.pem \
  --key device-001.key.pem \
  --client_id basicPubSub \
  --topic sdk/test/python \
  --count 10
```

**EMQX 版本（迁移后）**：

```bash
python3 mqtt5_x509.py \
  --endpoint mqtt.example.com \
  --cert device-001.cert.pem \
  --key device-001.key.pem \
  --client_id basicPubSub \
  --topic sdk/test/python \
  --count 10
```

注意证书和私钥参数保持不变，仅端点更新。

如果需要显式指定服务器 CA 证书（未使用系统信任库时），可在示例中添加 `ca_filepath`：

```python
# 在 mqtt5_x509.py 中找到 mqtt5_client_builder.mtls_from_path() 调用
# 并添加 ca_filepath 参数：

client = mqtt5_client_builder.mtls_from_path(
    endpoint=args.input_endpoint,
    cert_filepath=args.input_cert,
    pri_key_filepath=args.input_key,
    ca_filepath="emqx-server-ca.pem",  # 添加此行
    on_publish_received=on_publish_received,
    on_lifecycle_stopped=on_lifecycle_stopped,
    on_lifecycle_attempting_connect=on_lifecycle_attempting_connect,
    on_lifecycle_connection_success=on_lifecycle_connection_success,
    on_lifecycle_connection_failure=on_lifecycle_connection_failure,
    on_lifecycle_disconnection=on_lifecycle_disconnection,
    client_id=args.input_clientId
)
```

**关键变更总结**：

- **Endpoint**：更新为 EMQX Broker 主机名
- **服务器 CA**：可选，指定签发 EMQX 服务器证书的 CA
- **设备证书**：无需更改
- **应用逻辑**：无需更改，发布/订阅/消息处理逻辑完全一致

运行更新后的命令可验证成功连接、订阅和发布，确认设备迁移完成。

## 进阶迁移场景

以下进阶场景同样适用于上述迁移方式。

### 迁移使用 PKCS11（HSM） 的设备

对于使用硬件安全模块（HSM）存储私钥的设备，迁移过程也十分简单。只要设备证书由您的 CA 签发，私钥仍存储在 HSM 内，无需额外处理。

**客户端代码修改示例**：

EMQX 服务端配置（阶段 2）保持不变。客户端更新如下：

```python
client = mqtt5_client_builder.mtls_with_pkcs11(
    # 变更：您的 EMQX Broker 主机名
    endpoint="mqtt.example.com",

    # 变更：更新为您的 EMQX 服务器 CA
    ca_filepath="emqx-server-ca.pem",

    # 设备证书（无更改）
    cert_filepath="device-001.cert.pem",

    # HSM 配置（无更改）
    pkcs11_lib="/path/to/pkcs11.so",
    user_pin="YOUR-HSM-PIN",
    slot_id=pkcs11_slot_id,
    token_label="YOUR-TOKEN-LABEL",
    private_key_label="device-001-key",

    on_publish_received=on_publish_received,
    # ...其他回调...
    client_id="device-001"
)
```

### 迁移通过 HTTP Proxy 连接的设备

对于必须通过 HTTP Proxy 连接的设备，迁移过程与标准路径相同。mTLS 连接将通过 HTTP CONNECT 隧道传输。

EMQX 服务端配置（阶段 2）**完全不变**。Proxy 对 EMQX 监听器透明，EMQX 只会看到一条正常的 mTLS 连接。

**客户端代码修改示例（Python）**：

```python
# 1. 配置 HTTP Proxy
http_proxy_options = http.HttpProxyOptions(
    host_name="my-proxy.my-network.com",
    port=8888
)

# 2. 创建带 Proxy 配置的客户端
client = mqtt5_client_builder.mtls_from_path(
    # 变更：EMQX Broker 主机名
    endpoint="mqtt.example.com",

    # 变更：更新为 EMQX 服务器 CA
    ca_filepath="emqx-server-ca.pem",

    # 设备证书（无更改）
    cert_filepath="device-001.cert.pem",
    pri_key_filepath="device-001.key.pem",

    # 添加 Proxy 配置
    http_proxy_options=http_proxy_options,

    on_publish_received=on_publish_received,
    # ...其他回调...
    client_id="device-001"
)
```

## 总结

当设备使用自定义 CA 进行 mTLS 时，从 AWS IoT Core 迁移到 EMQX 的过程十分简单，主要是配置更改而非重新配置设备。

通过本指南的三个阶段：

1. 找到自定义 CA 证书
2. 配置 EMQX Broker 使用您的 CA 进行 mTLS 身份验证
3. 更新设备客户端端点

您的设备群即可顺利迁移到 EMQX，同时保持相同的强健 mTLS 身份认证机制。迁移过程中保留现有设备证书、私钥和应用逻辑，仅需对连接相关参数进行少量更新。组织因此能够在保持安全最佳实践的前提下，以最小干扰迁移至 EMQX。

::: tip
如果您的设备使用 AWS 签发的一键式证书，您需要先建立自己的 CA 基础设施并重新签发设备证书，才能完成迁移。这是摆脱 AWS 专有证书链的前置要求。
:::
