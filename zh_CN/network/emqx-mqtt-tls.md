# 开启 SSL/TLS 连接

SSL/TLS 加密功能会在传输层对网络连接进行加密，它能在提升通信数据安全性的同时，保证数据的完整性。EMQX 提供了非常完整的 SSL/TLS 能力支持，包括支持单/双向认证、X.509 证书认证，您可以为包括 MQTT 在内的所有连接启用 SSL/TLS 加密连接，保证接入与消息传输安全。

本章节将向您详细介绍 SSL/TLS 加密连接以及如何在 EMQX 中开启 SSL/TLS 连接。

## 安全优势

1. 强认证：开启 TLS 连接后，通讯双方将互相检查对方的身份，比如通过检查对方持有的 X.509 数字证书；这类数字证书通常是由受信机构 CA（Certificate Authority）颁发，不可伪造。

2. 机密性：开启 TLS 连接后，每次会话都会根据双方协商得到的会话密钥进行加密。任何第三方都无法知晓通讯内容，因此即使一次会话的密钥泄露，也不影响其他会话的安全性。

3. 完整性：加密通讯中的数据被篡改的可能性极低。

对于客户端的 SSL/TLS 连接，您可以根据使用场景选择以下两种使用方式之一：

| 使用方式                                  | 优势                                       | 缺点                                                                                                                |
| ----------------------------------------- | ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------- |
| 直接在客户端与 EMQX 之间开启 SSL/TLS 连接 | 简单易用，不需要额外的组件。               | 会增加 EMQX 的资源消耗，如果连接数量巨大，可能会导致较高的 CPU 和内存消耗。                                         |
| 通过代理或负载均衡终结 TLS 连接           | 不影响 EMQX 性能，同时提供了负载均衡能力。 | 只有少数云厂商的负载均衡器支持 TCP SSL/TLS 终结，此外，用户还需自己部署 [HAProxy](http://www.haproxy.org/) 等软件。 |

本章节将重点介绍如何直接在客户端与 EMQX 之间开启 SSL/TLS 连接，有关如何通过代理或负载均衡终结 TLS 连接，请参考 [集群负载均衡](../deploy/cluster/lb.md)。

## 通过配置文件开启 SSL/TLS 连接

:::tip 前置准备：

已具备 SSL/TLS 证书。

EMQX 随安装包提供了一组仅用于测试的 SSL/TLS 证书（位于 `etc/certs` 目录），并在 `8883` 端口启用了 SSL/TLS 连接。当应用于生产环境时，应切换至由可靠 CA 签发的证书。有关如何申请相关证书，请参考 [获取 SSL/TLS 证书](./tls-certificate.md)部分。

:::

1. 将 SSL/TLS 证书文件移动到 EMQX `etc/cert` 目录。
2. 打开配置文件 `emqx.conf` （根据您的安装方式，可能位于 `./etc` 或 `/etc/emqx/etc` 目录），修改 `listeners.ssl.default` 配置组，将证书替换为您的证书，并添加 `verify = verify_none`：

```bash
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  max_connections = 512000
  ssl_options {
    # keyfile = "etc/certs/key.pem"
    keyfile = "etc/certs/server.key"
    # certfile = "etc/certs/cert.pem"
    certfile = "etc/certs/server.crt"
    # cacertfile = "etc/certs/cacert.pem"
    cacertfile = "etc/certs/rootCA.crt"

    # 不开启对端验证
    verify = verify_none
  }
}
```

至此您已经完成 EMQX 上的 SSL/TLS 单向认证配置，单向认证仅保证通信已经被加密，无法验证客户端身份。

如需启用双向认证，请在 `listeners.ssl.default` 配置组中添加如下配置：

```bash
listeners.ssl.default {
  ...
  ssl_options {
    ...
    # 开启对端验证
    verify = verify_peer
    # 强制开启双向认证，如果客户端无法提供证书，则 SSL/TLS 连接将被拒绝
    fail_if_no_peer_cert = true
  }
}
```

3. 重启 EMQX，应用以上配置。

## 启用 TLS 加密访问外部资源

除了允许接收启用了双向 SSL 认证的MQTT 客户端外，EMQX 还提供了通过 SSL 加密访问外部资源的功能。比如，在使用 HTTP 服务进行密码认证时，通过 HTTPS 访问 web 服务器。下面的示例将介绍如何在 Dashboard 和配置文件中开启 TLS 加密。

在 Dashboard 的左侧导航栏点击**访问控制**->**认证**，在**认证**页面，创建 **Password-Based** 认证。

<img src="./assets/TLS-external-resource.png" alt="TLS-external-resource" style="zoom:50%;" />

如上所示，连接到外部资源时，您可以启用 TLS 并配置其 SSL 证书。

- **SNI** 意为服务器名称指示，指示服务器域名和证书是否验证为相同；空值表示没有验证。
- 当服务器需要验证客户端证书时，则必须填写 **TLS 证书** 和 **TLS 密钥**。
- 当启用 **TLS 验证** 时，必须填写 **CA 证书** 字段以验证服务器证书的合法性。

除了在 Dashboard 中开启 SSL 加密连接外部资源，您还可以通过 `emqx.conf` 配置，例如在 `authentication` 配置组中添加配置：

```
authentication {
  url = "https://127.0.0.1:8080"
  backend = "http"

  ...

  ssl {
    enable = true
    cacertfile = "etc/certs/cacert.pem"
    certfile = "etc/certs/cert.pem"
    keyfile = "etc/certs/key.pem"
    ## `verify_peer` means turn on verification for server certificate
    verify = verify_peer
  }
}
```

