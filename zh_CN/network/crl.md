# CRL 检查

从 EMQX 开源版 v5.0.22 和 EMQX 企业版 v5.0.3 开始，EMQX 支持对 MQTT SSL 监听器进行证书吊销列表（Certificate Revocation List，CRL）检查。该功能仅适用于类型为 `ssl` 的监听器，不支持安全 WebSocket（`wss`）和 QUIC 监听器。

启用 CRL 检查后，EMQX 会根据客户端证书中指定的 CRL 分发点（CRL Distribution Point）来验证客户端证书是否已被吊销。如果证书被标记为已吊销，EMQX 将在 SSL/TLS 握手阶段拒绝该连接。

> **注意**
>
> 要使 CRL 吊销检查生效，CRL 本身必须包含 *Issuing Distribution Point* 扩展，详见 [RFC 3280 第 5.2.5 节](https://www.rfc-editor.org/rfc/rfc3280#section-5.2.5)。

## 启用 CRL 检查

要启用 CRL 检查，需要同时满足以下条件：

1. 在 SSL 监听器中启用 CRL 检查选项。
2. 将监听器的 `verify` 选项设置为 `verify_peer`，以确保对客户端证书进行校验。

以下示例展示了如何配置启用了 CRL 检查的 SSL 监听器：


```hcl
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  ssl_options {
    # 包含受信任 CA（证书颁发机构）证书的 PEM 格式文件，用于验证客户端证书的合法性
    cacertfile = "/etc/emqx/certs/ca.pem"

    # 包含监听器 SSL/TLS 证书链的 PEM 格式文件。
    # 如果证书不是由根 CA 直接签发，需要在监听器证书后附加中间 CA 证书以形成完整证书链
    certfile = "/etc/emqx/certs/server.pem"

    # 与 SSL/TLS 证书对应的私钥 PEM 文件
    keyfile = "/etc/emqx/certs/server.key"

    # 必须校验客户端证书
    verify = verify_peer

    # 强制客户端发送非空证书，否则 TLS 握手失败
    fail_if_no_peer_cert = true

    # 启用客户端证书吊销状态（CRL）检查
    enable_crl_check = true
  }
}
```

其中：

- `verify = verify_peer` 表示将启用对端验证。
- `enable_crl_check = true` 表示启用CRL 检查。

## CRL 缓存

为了避免频繁向 CRL 分发点发起 HTTP 请求，EMQX 会在本地缓存已获取的 CRL。

当客户端连接时，如果 EMQX 首次检测到新的 CRL URL，会从客户端证书中指定的分发点获取对应的 CRL。默认情况下，EMQX 每 15 分钟刷新一次缓存的 CRL，以确保证书吊销信息保持最新。
