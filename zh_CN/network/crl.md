# CRL 检查

从 EMQX 开源版 5.0.22 和 EMQX 企业版 5.0.3 开始，EMQX 支持针对 MQTT SSL 监听器设置证书吊销列表（CRL）检查功能。

注意：QUIC 类型监听器暂不支持此功能。

启用 CRL 检查后，EMQX 将根据客户端证书中的 CRD 信息对申请建立连接的客户端进行验证，如证书已被撤销，EMQX 将拒绝连接请求。

注意：CRL 中应包含 ["Issuing Distribution Point " 扩展](https://www.rfc-editor.org/rfc/rfc3280#section-5.2.5)，以便执行 CRL 检查。

## 通过配置文件配置

EMQX 支持通过配置文件 `emqx.conf` 启用 CRL 检查功能。

您只需将相关的配置项附加到 `emqx.conf` 文件的末尾，相应设置将在 EMQX 重启后生效。

**示例代码**：


```hcl
listeners.ssl.default {
  bind = "0.0.0.0:8883"
  max_connections = 512000
  ssl_options {
    # PEM format file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the clients.
    cacertfile = "/etc/emqx/certs/ca.pem"
    # PEM format file containing the SSL/TLS certificate chain for the listener. If the certificate is not directly issued by a root CA, the intermediate CA certificates should be appended after the listener certificate to form a chain.
    certfile = "/etc/emqx/certs/server.pem"
    # PEM format file containing the private key corresponding to the SSL/TLS certificate
    keyfile = "/etc/emqx/certs/server.key"
    # Must verify peer certificats
    verify = verify_peer
    # Force the client to send a non-empty certificate, otherwise fail the TLS handshake.
    fail_if_no_peer_cert = true
    # Also verify client certificate's revocation status
    enable_crl_check = true
  }
}
```

其中：

- `verify = verify_peer` 表示将启用对端验证。
- `enable_crl_check = true` 表示启用CRL 检查。
