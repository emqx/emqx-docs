# 开启 SSL/TLS 连接

SSL/TLS 加密功能会在传输层对网络连接进行加密，它能在提升通信数据安全性的同时，保证数据的完整性。

本章节将向您详细介绍 SSL/TLS 加密连接的功能和优势以及在 EMQX 上开启 SSL/TLS 的步骤。

## 安全优势

启用 SSL/TLS 连接提供了以下安全优势：

1. **强认证**：开启 TLS 连接后，通讯双方将互相检查对方的身份，比如通过检查对方持有的 X.509 数字证书；这类数字证书通常是由受信机构 CA（Certificate Authority）颁发，不可伪造。
2. **机密性**：开启 TLS 连接后，每次会话都会根据双方协商得到的会话密钥进行加密。任何第三方都无法知晓通讯内容，因此即使一次会话的密钥泄露，也不影响其他会话的安全性。
3. **完整性**：加密通讯中的数据被篡改的可能性极低。

## 使用方式对比

您可以为包括 MQTT 在内的所有连接启用 SSL/TLS 加密连接，保证接入与消息传输安全。对于客户端的 SSL/TLS 连接，您可以根据使用场景选择以下两种使用方式之一：

| 使用方式                                  | 优势                                       | 缺点                                                                                                                |
| ----------------------------------------- | ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------- |
| 在 EMQX 上开启 SSL/TLS | 简单易用，不需要额外的组件。               | 会增加 EMQX 的资源消耗，如果连接数量巨大，可能会导致较高的 CPU 和内存消耗。                                         |
| 通过代理或负载均衡终结 TLS 连接           | 不影响 EMQX 性能，同时提供了负载均衡能力。 | 需要额外部署 [HAProxy](../deploy/cluster/lb-haproxy.md)、[NGINX](../deploy/cluster/lb-nginx.md) 或使用[云厂商负载均衡服务](../deploy/cluster/lb.md#公有云厂商-lb-产品)，其中只有部分云厂商支持 TCP SSL/TLS 终结。 |

有关如何通过代理或负载均衡终结 TLS 连接，请参考[集群负载均衡](../deploy/cluster/lb.md)。

## 单/双向认证对比

EMQX 提供了非常完整的 SSL/TLS 能力支持，支持通过 X.509 证书实现单向和双向客户端/服务器互信认证：

| 认证方式 | 说明 | 验证方式 | 优缺点 |
| --- |  --- |  --- |  --- |
| 单向认证 | 客户端验证服务器身份，但服务器不验证客户端的身份 | 客户端通常不需要提供证书，仅需验证服务器的证书是否由受信任的证书颁发机构（CA）签发 | 只能实现通信数据的机密性和完整性，但无法保证通信双方的身份 |
| 双向认证 | 服务器和客户端彼此验证对方的身份 | 需要为每个设备签发证书，服务器验证客户端的证书以确认其身份的合法性 | 可以确保服务器和客户端之间的互信关系，并防止中间人攻击 |

## SSL/TLS 证书

在启用 SSL/TLS 连接之前，您需要准备 SSL/TLS 证书，用于对连接进行身份验证并保障通信安全。

EMQX 同时支持传统的基于文件路径的证书管理方式，以及托管证书（EMQX 6.1+）。托管证书提供集中化管理能力，可在多个监听器和[连接器](../data-integration/connector.md)之间复用<!--，并支持通过自动证书管理环境（Automated Certificate Management Environment，ACME）实现可选的自动证书签发-->。

有关在 EMQX 中获取、管理和使用 SSL/TLS 证书的完整指南，包括托管证书的使用方式，请参阅 [SSL/TLS 证书](./tls-certificate.md)。

## 启用单向认证的 SSl/TLS 连接

默认情况下，EMQX 在端口 `8883` 上启用 SSL/TLS 监听器，并配置为单向认证模式，即：

- 客户端验证服务器证书
- 服务器不验证客户端证书

你可以通过 Dashboard 或配置文件来配置 SSL/TLS 监听器。在这两种方式下，EMQX 都支持以下两种证书提供方式：

- **基于路径的证书**（传统 PEM 文件）：在配置中通过文件路径直接引用证书文件。
- **托管证书**（EMQX 6.1 引入）：将证书作为可复用的资源由 EMQX 统一管理，并通过名称进行引用。

请根据你的部署方式和运维需求选择合适的方式。

### 通过 Dashboard 启用

1. 进入**管理** -> **监听器**。

2. 点击名为 **default** 的 **SSL** 类型监听器，打开**编辑监听器**页面。

3. 配置以下 SSL/TLS 相关选项：

   - **双向认证**：默认关闭（单向认证）。关闭时，EMQX 不会校验客户端证书。
   
   - **强制验证对端证书**：仅在开启**双向认证**时生效。对于单向认证场景，该选项应保持关闭。
   
   - **会话票据**：用于启用 TLS 1.3 的会话恢复功能。启用后，客户端可在重连时使用服务器签发的加密会话票据恢复之前的 TLS 会话，从而避免完整握手，降低连接延迟和 CPU 开销。
   
     - `disabled`：禁用（默认值）。每次客户端连接或重连时都会执行完整的 TLS 握手。
     - `stateless`：启用无状态会话票据。客户端可使用会话票据恢复 TLS 会话，服务器无需保存会话状态，可提升重连性能。但会话恢复后无法获取 TLS 客户端证书信息，适用于不依赖证书进行认证或授权的场景。
     - `stateless_with_cert`：启用携带证书信息的无状态会话票据。会话恢复后仍可获取证书信息。适用于基于证书进行认证或访问控制（如 mTLS）的场景，但会略微增加网络带宽开销。
   
     ::: tip 注意
   
     会话票据仅适用于 TLS 1.3，且仅支持无状态（stateless）机制，以保证在集群环境中的可扩展性。EMQX 不支持 TLS 1.2 的有状态会话恢复。
   
     此外，EMQX 不支持客户端早期数据（0-RTT），客户端必须在 TLS 握手完成后才能发送 MQTT 数据。
   
     :::
   
   - **证书来源**：选择服务器证书的提供方式：
     - **手动输入**：使用传统的基于路径的证书文件。需要配置以下字段：
     
       - **TLS Cert**：服务器证书文件路径。
       - **TLS Key**：服务器私钥文件路径。
     
     - **从托管证书中选择**：使用 EMQX 托管的证书包（EMQX 6.1+）。需要配置以下字段：
     
       - **命名空间**：托管证书所在的命名空间（默认：`全局`）。
       - **托管证书包名称**：选择一个已有的托管证书包。若需创建新的证书包，可点击**创建托管证书**。详情请参考：[通过 Dashboard 创建托管证书包](./tls-certificate.md#通过-dashboard-创建托管证书包)。
       - **SNI**（可选）：用于在同一监听器上配置多个证书时，根据客户端请求的 SNI 值匹配对应证书。
     
       你可以点击 “**+**” 按钮添加多个托管证书条目。当配置了多个证书时：
     
       - EMQX 会根据客户端发送的 SNI 动态选择证书。
       - 若未匹配到 SNI，则使用列表中的第一个证书作为默认证书。
   
   - **SSL 版本**：支持所有 TLS/DTLS 版本。默认设置为 `tlsv1.3` 和 `tlsv1.2`。如果 PSK 验证中使用了 PSK 密码套件，确保在此处设置 `tlsv1.2` ， `tlsv1.1` 和 `tlsv1`。更多关于 PSK 的内容请参阅 [PSK 认证](./psk-authentication.md)。
   
   - **加密套件**：用于指定允许使用的加密套件。
   
   - **CA 证书深度**：证书链允许的最大深度，默认值为 `10`。
   
   - **密钥文件密码**：如果密钥文件由密码保护，则需要输入密码。
   
   - **启用 OCSP Stapling**: 默认为不启用；如需获取 X.509 数字证书的撤销状态，可以点击切换开关。具体可参阅 [OCSP Stapling](./ocsp.md)。
   
   - **启用 CRL 检查**：默认为不启用；如需设置证书吊销列表（CRL）检查功能，可以点击切换开关。具体可参阅 [CRL 检查](./crl.md)。
   
4. 配置完成后，点击**更新**以应用更改。

### 通过配置文件启用

你也可以通过修改配置文件中的 `listeners.ssl.default` 配置组来启用 SSL/TLS 连接。

1. 将你的 SSL/TLS 证书文件放置到 EMQX 的 `etc/certs` 目录中。

2. 打开配置文件 `base.hocon`（根据安装方式不同，该文件位于 `./etc` 或 `/etc/emqx/etc` 目录中）。

3. 修改 `listeners.ssl.default` 配置组。

   - 如果您选择使用磁盘上的证书文件（基于路径的证书）：将示例中的证书文件替换为您自己的证书。如需启用单向认证，请设置 `verify = verify_none`：

     ```hocon
     listeners.ssl.default {
       bind = "0.0.0.0:8883"
       
       ssl_options {
         cacertfile = "etc/certs/rootCAs.pem"
         certfile = "etc/certs/server-cert.pem"
         keyfile = "etc/certs/server-key.pem"
         
         verify = verify_none
         fail_if_no_peer_cert = true
       }
     }
     ```

     **配置项说明：**

     - **`cacertfile`**：用于验证客户端证书的受信任 CA 证书（PEM 格式）文件路径。对于单向认证，该文件可以为空或不包含任何 CA 证书。
     - **`certfile`**：服务器 SSL/TLS 证书链（PEM 格式）文件路径。如果监听器使用的服务器证书不是由根 CA 直接签发，需要将中间 CA 证书按顺序追加在服务器证书之后，以形成完整的证书链。
     - **`keyfile`**：与服务器证书对应的私钥文件（PEM 格式）路径。
     - **`verify`**：控制 EMQX 是否验证客户端证书：
       - `verify_none`：不验证客户端证书（单向认证）。
       - `verify_peer`：验证客户端证书（双向认证 / mTLS 必需）。
     - **`fail_if_no_peer_cert`**：当客户端未提供证书时的握手行为：
       - `false`：允许客户端不提供证书，仅在客户端提供了无效证书时拒绝连接（单向认证）。
       - `true`：客户端未提供证书时直接拒绝连接（mTLS 必需）。

   - 如果您使用 EMQX 统一管理并通过名称引用的托管证书，配置示例如下：

     > 托管证书包需要事先通过 Dashboard 或 HTTP API 创建。监听器配置中只需引用已存在的托管证书。

     **示例：引用全局（global）命名空间中的证书**
     
     ```hocon
     listeners.ssl.default {
       bind = "0.0.0.0:8883"
     
       ssl_options {
         managed_certs = [
           {
             bundle_name = "example-cert-1"
             sni  = "example.com"
           },
           {
             bundle_name = "example-cert-2"
             sni  = "api.example.com"
           }
         ]
     
         # 单向认证
         verify = verify_none
         fail_if_no_peer_cert = false
       }
     }
     ```

     > 当引用全局（global）命名空间中的托管证书时，`namespace` 字段需要省略。如果省略 `namespace`，EMQX 会默认使用全局命名空间。

     **示例：引用非全局（租户）命名空间中的证书**
     
     ```hocon
     listeners.ssl.default {
       bind = "0.0.0.0:8883"
     
       ssl_options {
         managed_certs = [
           {
             namespace = "tenant-a"
             bundle_name = "mqtt-cert"
             sni       = "mqtt.tenant-a.example.com"
           }
         ]
     
         verify = verify_none
         fail_if_no_peer_cert = false
       }
     }
     ```
     
     > 当使用创建在非全局（租户）命名空间中的托管证书时，必须显式指定 `namespace` 字段。
     
     当配置了多个托管证书时：
     
     - EMQX 会根据客户端发送的 SNI 动态选择匹配的证书。
     - 如果没有匹配到 SNI，则使用列表中的第一个证书作为默认证书。

4. 重启 EMQX 以使配置生效。

完成 SSL/TLS 连接的配置后，您可以通过 MQTT 客户端连接到 EMQX。

## 单向认证客户端测试

您可以使用 [MQTTX CLI](https://mqttx.app/zh/cli) 进行测试，单向认证通常需要客户端提供 CA 证书，以便客户端验证服务器的身份：

```bash
mqttx sub -t 't/1' -h localhost -p 8883 \
  --protocol mqtts \
  --ca certs/rootCA.crt
```

如果服务器证书 CN 与客户端连接时指定的服务器地址不匹配，将会出现以下错误：

```bash
Error [ERR_TLS_CERT_ALTNAME_INVALID]: Hostname/IP does not match certificate's altnames: Host: localhost. is not cert's CN: Server
```

此时可以设置客户端证书 CN 与服务器地址匹配，或者通过 `--insecure` 选项忽略证书 CN 验证：

```bash
mqttx sub -t 't/1' -h localhost -p 8883 \
  --protocol mqtts \
  --ca certs/rootCA.crt \
  --insecure
```

## 启用双向认证

双向认证是在单向认证的基础上，进一步配置 EMQX 对客户端证书进行验证，以确保客户端的身份合法性。

除此之外，您还需要为客户端生成证书，具体操作请参考[签发客户端证书](./tls-certificate.md#签发客户端证书)。

对于 Dashboard 方式，您可以在**验证客户端证书**处选择**启用**，并配置**没有证书则 SSL 失败**选项为 `true` 强制开启双向认证。

您也可以在配置文件添加以下 2 项配置：

```bash
listeners.ssl.default {
  ...
  ssl_options {
    ...
    # 双向认证，验证客户端证书
    verify = verify_peer
    # 如果客户端没有证书，SSL/TLS 连接将被拒绝
    fail_if_no_peer_cert = true
  }
}
```

## 双向认证客户端测试

您可以使用 [MQTTX CLI](https://mqttx.app/zh/cli) 进行测试，双向认证除了需要客户端提供 CA 证书外，还应当提供客户端证书：

```bash
mqttx sub -t 't/1' -h localhost -p 8883 \
  --protocol mqtts \
  --ca certs/rootCA.crt \
  --cert certs/client-0001.crt \
  --key certs/client-0001.key
```

如果服务器证书 CN 与客户端连接时指定的服务器地址不匹配，将会出现以下错误：

```bash
Error [ERR_TLS_CERT_ALTNAME_INVALID]: Hostname/IP does not match certificate's altnames: Host: localhost. is not cert's CN: Server
```

此时可以设置客户端证书 CN 与服务器地址匹配，或者通过 `--insecure` 选项忽略证书 CN 验证：

```bash
mqttx sub -t 't/1' -h localhost -p 8883 \
  --protocol mqtts \
  --ca certs/rootCA.crt \
  --cert certs/client-0001.crt \
  --key certs/client-0001.key \
  --insecure
```
