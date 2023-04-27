# CRL 检查

{% emqxce %}

从 5.0.22 版本开始，EMQX 支持针对 MQTT SSL 监听器设置证书吊销列表（CRL）检查功能。

{% endemqxce %}

{% emqxee %}

从 5.0.3 版本开始，EMQX 支持针对 MQTT SSL 监听器设置证书撤销列表（CRL）检查功能。

{% endemqxee %}

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
    keyfile = "/etc/emqx/certs/server.key"
    certfile = "/etc/emqx/certs/server.pem"
    cacertfile = "/etc/emqx/certs/ca.pem"
    verify = verify_peer
    enable_crl_check = true
  }
}
```

其中：

- `verify = verify_peer` 表示将启用对段验证。
- `enable_crl_check = true` 表示启用CRL 检查。
