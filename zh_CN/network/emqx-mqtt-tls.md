# 开启 SSL/TLS 连接

SSL/TLS 加密功能会在传输层对网络连接进行加密，它能在提升通信数据安全性的同时，保证数据的完整性。

本章节将向您详细介绍 SSL/TLS 加密连接的优势和在 EMQX 上开启 SSL/TLS 的步骤。

## 安全优势

启用 SSL/TLS 连接提供了以下安全优势：

1. **强认证**：开启 TLS 连接后，通讯双方将互相检查对方的身份，比如通过检查对方持有的 X.509 数字证书；这类数字证书通常是由受信机构 CA（Certificate Authority）颁发，不可伪造。
2. **机密性**：开启 TLS 连接后，每次会话都会根据双方协商得到的会话密钥进行加密。任何第三方都无法知晓通讯内容，因此即使一次会话的密钥泄露，也不影响其他会话的安全性。
3. **完整性**：加密通讯中的数据被篡改的可能性极低。

## 启用方式对比

您可以为包括 MQTT 在内的所有连接启用 SSL/TLS 加密连接，保证接入与消息传输安全。对于客户端的 SSL/TLS 连接，您可以根据使用场景选择以下两种使用方式之一：

| 使用方式                                  | 优势                                       | 缺点                                                                                                                |
| ----------------------------------------- | ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------- |
| 在 EMQX 上开启 SSL/TLS | 简单易用，不需要额外的组件。               | 会增加 EMQX 的资源消耗，如果连接数量巨大，可能会导致较高的 CPU 和内存消耗。                                         |
| 通过代理或负载均衡终结 TLS 连接           | 不影响 EMQX 性能，同时提供了负载均衡能力。 | 需要额外部署 [HAProxy](../deploy/cluster/lb-haproxy.md)、[Nginx](../deploy/cluster/lb-nginx.md) 或使用[云厂商负载均衡服务](../deploy/cluster/lb.md#公有云厂商-lb-产品)，其中只有部分云厂商支持 TCP SSL/TLS 终结。 |

有关如何通过代理或负载均衡终结 TLS 连接，请参考[集群负载均衡](../deploy/cluster/lb.md)。

## 单/双向认证对比

EMQX 提供了非常完整的 SSL/TLS 能力支持，支持通过 X.509 证书实现单向和双向客户端/服务器互信认证：

| 认证方式 | 说明 | 验证方式 | 优缺点 |
| --- |  --- |  --- |  --- |
| 单向认证 | 客户端验证服务器身份，但服务器不验证客户端的身份 | 客户端通常不需要提供证书，仅需验证服务器的证书是否由受信任的证书颁发机构（CA）签发 | 只能实现通信数据的机密性和完整性，但无法保证通信双方的身份 |
| 双向认证 | 服务器和客户端彼此验证对方的身份 | 需要为每个设备签发证书，服务器验证客户端的证书以确认其身份的合法性 | 可以确保服务器和客户端之间的互信关系，并防止中间人攻击 |

## 获取证书

开始之前，确保您已经具备 SSL/TLS 证书。EMQX 初始安装包中提供了一组仅用于测试目的的 SSL/TLS 证书，这些证书位于 etc/certs 目录。然而，在生产环境中，您需要切换为由可信任 CA 签发的证书。有关如何申请证书，请参考[获取 SSL/TLS 证书](./tls-certificate.md)。

## 启用单向认证

EMQX 默认在 `8883` 端口启用了 SSL/TLS 监听器并设置其为单向认证，您可以通过 Dashboard 与配置文件进行配置，实现证书替换和其他配置项的修改。

### 通过 Dashboard 配置

1. 打开 Dashboard，点击**管理** -> **监听器**进入监听器管理页面。

2. 选择名称为 **default**，类型为 **ssl** 的监听器，点击名称进行编辑。TLS/SSL 连接相关的参数如下：

   - **验证客户端证书**：默认为不启用，使用单向认证。
   - **TLS Cert**, **TLS Key** 和 **CA Cert**：点击**重新设置**上传您的私人证书文件以替换当前的证书文件。
   - **SSL 版本**:  支持所有 TLS/DTLS 版本。默认设置为 `tlsv1.3` 和 `tlsv1.2`。如果 PSK 验证中使用了 PSK 密码套件，确保在此处设置 `tlsv1.2` ， `tlsv1.1` 和 `tlsv1`。更多关于 PSK 的内容请参阅 [PSK 认证](./psk-authentication.md)。
   - **没有证书则 SSL 失败**：在**验证客户端证书**启用时有效，默认设置为 `false`。
     - 如果设置为 `true`，如果客户端发送空证书，则客户端身份验证失败，SSL/TLS 连接将被拒绝。
     - 如果设置为 `false`，只有当客户端发送无效证书时，客户端身份验证失败（空证书被认为是有效证书），SSL/TLS 连接将被拒绝。
   - **CA 证书深度**：允许的证书证书链长度， 默认值为 `10`。
   - **密钥文件密码**：如果密钥文件由密码保护，则需要输入密码。
   - **启用 OCSP Stapling**: 默认为不启用；如需获取 X.509 数字证书的撤销状态，可以点击切换开关。具体可参阅 [OCSP Stapling](./ocsp.md)。
   - **启用 CRL 检查**：默认为不启用；如需设置证书吊销列表（CRL）检查功能，可以点击切换开关。具体可参阅 [CRL 检查](./crl.md)。

3. 完成编辑后，点击**更新**。

   <img src="./assets/edit-listener.png" alt="edit-listener" style="zoom:40%;" />

### 通过配置文件配置

1. 将 SSL/TLS 证书文件复制到 EMQX `etc/cert` 目录。

2. 打开配置文件 `emqx.conf`，根据您的安装方式，可能位于 `./etc` 或 `/etc/emqx/etc` 目录。
3. `emqx.conf` 中默认没有监听器配置，添加以下配置内容：

   ```bash
    listeners.ssl.default {
      bind = "0.0.0.0:8883"
      ssl_options {
        cacertfile = "etc/certs/rootCA.crt"

        certfile = "etc/certs/server.crt"
        keyfile = "etc/certs/server.key"
        # 私钥文件受密码保护时需要输入密码
        # password = "123456"

        # 单向认证，不验证客户端证书
        verify = verify_none
      }
    }
   ```

4. 重启 EMQX，应用以上配置。

支持，您已将完成 SSL/TLS 连接的配置，接下来您可以通过 MQTT 客户端连接到 EMQX。

## 单向认证客户端测试

您可以使用 [MQTTX CLI](https://mqttx.app/) 进行测试，单向认证通常需要客户端提供 CA 证书，以便客户端验证服务器的身份：

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

您可以使用 [MQTTX CLI](https://mqttx.app/) 进行测试，双向认证除了需要客户端提供 CA 证书外，还应当提供客户端证书：

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
