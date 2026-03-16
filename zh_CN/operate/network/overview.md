# 网络与 TLS

信息安全对于物联网场景下的端对端加密通信至关重要。SSL 和 TLS 常被用于网络通信，以确保数据传输保持机密性，并且无法被攻击者截获或修改。SSL/TLS 加密功能在传输层加密网络连接，并涉及使用数字证书来验证涉及各方的身份并建立安全通信通道。

EMQX 在以下几种应用中使用 SSL 和 TLS 协议来保障网络通信安全：

- MQTT 客户端与 EMQX 建立连接
- 连接到外部资源，比如外部数据库
- 集群中不同节点之间的通信

EMQX 提供了全面的 SSL/TLS 功能支持，包括单向/双向身份验证和 X.509 证书身份验证。当接受 MQTT 客户端或连接到外部资源（如数据库）时，EMQX 可以通过 SSL/TLS 建立安全连接。

## 启用 TLS 进行客户端加密连接

[开启 SSL/TLS 连接](./emqx-mqtt-tls.md)页面详细介绍了如何在 MQTT 客户端与 EMQX 的连接中启用 SSL/TLS。在[获取 SSL/TLS 证书](./tls-certificate.md)中介绍了创建自签名证书的方法。在 SSL/TLS 启用的情况下，您可以同时开启 [CRL 检查](./crl.md)或 [OCSP stapling](./ocsp.md) 来验证证书的状态。

[客户端 TLS](./mqtt-client-tls.md) 页面列举了 MQTT 客户端库接入示例和工程项目代码，在这些示例中包括了 TLS 使用指南。

### 国密 SSL

[国密 SSL](./gmssl.md) 即国家密码局认定的国产密码算法。我国在金融银行、教育、交通、通信、国防工业等各类重要领域的信息系统均已开始进行国产密码算法的升级改造。本节将介绍 EMQX 国密算法整体解决方案。

## 启用 TLS 加密访问外部资源

除了允许接收启用了双向 SSL 认证的 MQTT 客户端外，EMQX 还提供了通过 SSL 加密访问外部资源的功能。比如，在使用 HTTP 服务进行密码认证时，通过 HTTPS 访问 web 服务器。下面的示例将介绍如何在 Dashboard 和配置文件中开启 TLS 加密。

在 Dashboard 的左侧导航栏点击**访问控制**->**认证**，在**认证**页面，创建 **Password-Based** 认证。

<img src="./assets/TLS-external-resource.png" alt="TLS-external-resource" style="zoom:50%;" />

如上所示，连接到外部资源时，您可以启用 TLS 并配置其 SSL 证书。

- **SNI** 意为服务器名称指示，指示服务器域名和证书是否验证为相同；空值表示没有验证。
- 当服务器需要验证客户端证书时，则必须填写 **TLS 证书** 和 **TLS 密钥**。
- 当启用 **TLS 验证** 时，必须填写 **CA 证书** 字段以验证服务器证书的合法性。

除了在 Dashboard 中开启 SSL 加密连接外部资源，您还可以通过 `emqx.conf` 配置，例如在 `authentication` 配置组中添加配置：

```json
authentication {
  url = "https://127.0.0.1:8080"
  backend = "http"

  ...

  ssl {
    enable = true
    # PEM 格式的文件，包含一个或多个用于验证 HTTP 服务器证书的根 CA 证书
    cacertfile = "etc/certs/cacert.pem"
    # PEM 格式的客户端证书，如果证书不是直接由根 CA 签发，那么中间 CA 的证书必须加在服务器证书的后面组成一个证书链
    certfile = "etc/certs/cert.pem"
    # PEM 格式的密钥文件
    keyfile = "etc/certs/key.pem"
    # 设置成 'verify_peer' 来验证 HTTP 服务器端证书是否为 cacertfile 中某个根证书签发
    verify = verify_none
  }
}
```

## 启用 TLS 加密节点通信

关于如何在集群节点通信中启用 TLS的具体介绍，您可以参阅[集群安全](../deploy/cluster/security.md)。
