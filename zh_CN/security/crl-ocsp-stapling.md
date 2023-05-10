# CRL/OCSP Stapling

EMQX 内置了 SSL/TLS 功能，您可以使用 X.509 证书实现客户端接入认证与通信安全加密。

X.509 证书尚未到期前，如果出现私钥泄漏或证书信息有误的情况，您需要将其吊销以确保您的设备不被非法利用，此时您可以使用 CRL 或 OCSP Stapling 功能，以此实现更高级别的安全保障。

## CRL

CRL（Certificate Revocation List，证书吊销列表）是由 CA 机构维护的一个列表，列表中包含已经被吊销的证书序列号和吊销时间。

EMQX 允许配置 CA 的请求端点并定时刷新获取 CRL，并存储在 EMQX 节点上，与浏览器使用 CRL 的方式不同，我们将 MQTT 客户端设计为无需维护 CRL 文件，仅在连接握手时通过 EMQX 对比验证即可完成证书有效性验证。

包括刷新获取 CRL 在内的所有操作都是在 EMQX 内部完成，无需使用额外的定时脚本或对客户端做任何改造。

### 配置 CRL

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

CRL 文件列表可以咨询 CA 或通过以下命令查询，以 broker.emqx.io 域名证书为例：

```bash
$ openssl x509 -in broker.emqx.io.crt -noout -text | grep crl

URI:http://crl3.digicert.com/RapidSSLGlobalTLSRSA4096SHA2562022CA1.crl
URI:http://crl4.digicert.com/RapidSSLGlobalTLSRSA4096SHA2562022CA1.crl
```

详细配置参数请参照 [启用 CRL](../configuration/configuration.md#listener-ssl-external-enable-crl-check)。

### CRL 操作指南

下面将以自签名客户端证书为例，演示客户端证书的吊销与在 EMQX 部署操作流程。

::: tip 先决条件
您已启用 SSL/TLS 安全连接，使用自签名 CA 生成客户端证书，并且持有 CA 私钥。

有关 EMQX 启用 SSL/TLS 的详细操作请参照 [EMQX MQTT 服务器启用 SSL/TLS 安全连接](https://www.emqx.com/zh/blog/emqx-server-ssl-tls-secure-connection-configuration-guide)。
:::

1. 吊销客户端证书

   1. 生成 openssl.conf 配置文件与 index.txt 数据库文件

    ```bash
    cat > openssl.cnf <<EOF
    [ ca ]
    default_ca = myca

    [ myca ]
    dir = .
    database = \$dir/index.txt
    certificate = \$dir/ca.crt
    private_key = \$dir/ca.key
    default_days = 365
    default_md = sha256

    default_crl_days = 365
    EOF

    touch index.txt
    ```

    配置项含义如下：

    | 配置项 | 含义 |
    | --- | --- |
    | dir | 证书存放目录 |
    | database | 证书数据库文集爱你 |
    | certificate | CA 证书 |
    | private_key | CA 私钥 |
    | default_days | 证书有效期 |
    | default_md | 消息摘要算法 |
    | default_crl_days | CRL 有效期 |

   2. 通过 CA 证书和私钥以及配置文件生成 CRL 文件

    ```bash
    openssl ca -gencrl -keyfile ca.key -cert ca.crt -out ca.crl -config openssl.cnf
    ```

   3. 吊销客户端证书

    ```bash
    $ openssl ca -revoke client.crt -config openssl.cnf

    Adding Entry with serial number 36071A4116484CEA69F906B3CDE74507CC2939F4 to DB for /C=CN/ST=YN/L=KM/O=EMQ/OU=EMQX/CN=emqx-c
    Revoking Certificate 36071A4116484CEA69F906B3CDE74507CC2939F4.
    Data Base Updated
    ```

   4. 重新生成 CRL 文件

   ```bash
   openssl ca -gencrl -keyfile ca.key -cert ca.crt -out ca.crl -config openssl.cnf
   ```

2. 配置 EMQX

   1. 将 CRL 文件放置到 Web 服务器上，假设 CRL 文件的 URL 为 http://localhost:8080/ca.crl

   2. 配置 CRL 请求端点，确保 EMQX 能够访问到 CRL 文件

    ```bash
    listener.ssl.external.crl_cache_urls = http://localhost:8080/ca.crl
    ```

   3. 重启 EMQX 以应用配置

3. 使用 MQTTX CLI 验证

```bash
mqttx conn -h localhost -p 8883 --ca ca.crt --cert client.crt --key client.key --insecure
```

MQTTX CLI 无法建立连接，EMQX 将输出验证失败日志，日志内容如下：

```bash
2023-05-05T16:19:59.117098+08:00 [error] supervisor: 'esockd_connection_sup - <0.2576.0>', errorContext: connection_shutdown, reason: {ssl_error,{tls_alert,{certificate_revoked,"TLS server: In state wait_cert at ssl_handshake.erl:2098 generated SERVER ALERT: Fatal - Certificate Revoked\n"}}}...
```

至此，您已经成功吊销了客户端证书，并且 EMQX 也能够正确识别吊销的客户端证书。

## OCSP Stapling

OCSP（Online Certificate Status Protocol，在线证书状态协议）是另外一个证书吊销方案，OCSP Stapling 是对 OCSP 技术的最新改进。

OCSP Stapling 允许通过 EMQX 确定证书状态，而不需要每个客户端向 OCSP Responder 发起请求。启用 OCSP Stapling 后，EMQX 将自行从 OCSP 服务器查询证书并缓存响应结果，当客户端向 EMQX 发起 SSL 握手请求时，EMQX 将证书的 OCSP 信息随证书链一同发送给客户端（Stapling），由客户端对证书有效性进行验证。

OCSP Stapling 提高了客户端证书查询的速度，并显著降低了 OCSP Responder 的负载。

### 配置 OCSP Stapling

```bash
# 启用 OCSP Stapling
listener.ssl.external.enable_ocsp_stapling = true

# OCSP Responder URL
listener.ssl.external.ocsp_responder_url = http://ocsp.digicert.com

# OCSP Responder 证书
listener.ssl.external.ocsp_issuer_pem = etc/certs/ocsp-issuer.pem

# OCSP Stapling 刷新间隔与请求超时时间
listener.ssl.external.ocsp_refresh_interval = 5m
listener.ssl.external.ocsp_refresh_http_timeout = 15s
```

OCSP Responder URL 可以咨询 CA 或通过以下命令查询，以 broker.emqx.io 域名证书为例：

```bash
$ openssl x509 -in broker.emqx.io.crt -noout -ocsp_uri
http://ocsp.dcocsp.cn
```

### OCSP Stapling 验证

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
