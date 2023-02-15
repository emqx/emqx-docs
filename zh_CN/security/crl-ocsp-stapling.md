# CRL/OCSP Stapling

EMQX 内置了 SSL/TLS 功能，您可以使用 X.509 证书实现客户端接入认证与通信安全加密。

X.509 证书尚未到期前，如果出现私钥泄漏或证书信息有误的情况，您需要将其吊销以确保您的设备不被非法利用，此时您可以使用 CRL 或 OCSP Stapling 功能，以此实现更高级别的安全保障。

## CRL

CRL（Certificate Revocation List，证书吊销列表）是由 CA 机构维护的一个列表，列表中包含已经被吊销的证书序列号和吊销时间。

EMQX 允许配置 CA 的请求端点并定时刷新获取 CRL，并存储在 EMQX 节点上，与浏览器使用 CRL 的方式不同，我们将 MQTT 客户端设计为无需维护 CRL 文件，仅在连接握手时通过 EMQX 对比验证即可完成证书有效性验证。

包括刷新获取 CRL 在内的所有操作都是在 EMQX 内部完成，无需使用额外的定时脚本或对客户端做任何改造。

## 配置 CRL

```bash
# 是否启用 CRL
listener.ssl.external.enable_crl_check = true

# 逗号分隔的 CRL 文件列表
listener.ssl.external.crl_cache_urls = http://my.crl.server/intermediate.crl.pem, http://my.other.crl.server/another.crl.pem

# CRL 请求超时
crl_cache_http_timeout = 15s


# CRL 刷新间隔，全局配置
crl_cache_refresh_interval = 15m
```

CRL 文件列表可以咨询 CA 或通过以下命令查询：

```bash
$ openssl x509 -in broker.emqx.io.crt -noout -text | grep crl

URI:http://crl3.digicert.com/RapidSSLGlobalTLSRSA4096SHA2562022CA1.crl
URI:http://crl4.digicert.com/RapidSSLGlobalTLSRSA4096SHA2562022CA1.crl
```

详细配置参数请参照 [启用 CRL](../configuration/configuration.md#listener-ssl-external-enable-crl-check)。

## OCSP Stapling

OCSP（Online Certificate Status Protocol，在线证书状态协议）是另外一个证书吊销方案，OCSP Stapling 是对 OCSP 技术的最新改进。

OCSP Stapling 允许通过 EMQX 确定证书状态，而不需要每个客户端向 OCSP Responder 发起请求。启用 OCSP Stapling 后，EMQX 将自行从 OCSP 服务器查询证书并缓存响应结果，当客户端向 EMQX 发起 SSL 握手请求时，EMQX 将证书的 OCSP 信息随证书链一同发送给客户端（Stapling），由客户端对证书有效性进行验证。

OCSP Stapling 提高了客户端证书查询的速度，并显著降低了 OCSP Responder 的负载。

### 配置 OCSP Stapling

```bash
# 启用 OCSP Stapling
listener.ssl.external.enable_ocsp_stapling = true

# OCSP Responder URL
## 根据证书提供商填写，或通过此命令获取
## openssl x509 -in broker.emqx.io.crt -noout -ocsp_uri
listener.ssl.external.ocsp_responder_url = http://ocsp.digicert.com

# OCSP Responder 证书
listener.ssl.external.ocsp_issuer_pem = etc/certs/ocsp-issuer.pem

# OCSP Stapling 刷新间隔与请求超时时间
listener.ssl.external.ocsp_refresh_interval = 5m
listener.ssl.external.ocsp_refresh_http_timeout = 15s
```

成功启用 OCSP Stapling 之后，可通过以下命令验证：

```bash
$ openssl s_client -connect broker.emqx.io:8883  -status -tlsextdebug < /dev/null 2>&1 | grep -i "OCSP response"

# 未启用显示结果
OCSP response: no response sent

# 已启用显示结果
OCSP response:
OCSP Response Data:
    OCSP Response Status: successful (0x0)
    Response Type: Basic OCSP Response
```

详细配置参数请参照 [启用 OCSP Stapling](../configuration/configuration.md#listener-ssl-external-enable-ocsp-stapling)。
