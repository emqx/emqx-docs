# 启用 SSL/TLS 连接

EMQX 提供了非常完整的 SSL/TLS 能力支持，包括支持单/双向认证、X.509 证书认证，您可以为 MQTT 在内的所有连接启用 SSL/TLS，保证接入与消息传输安全。

本章节将带您详细了解并启用 SSL/TLS 连接。
## SSL/TLS 带来的安全优势

1. 强认证。 用 TLS 建立连接的时候，通讯双方可以互相检查对方的身份。在实践中，很常见的一种身份检查方式是检查对方持有的 X.509 数字证书。这样的数字证书通常是由一个受信机构颁发的，不可伪造。

2. 保证机密性。TLS 通讯的每次会话都会由会话密钥加密，会话密钥由通讯双方协商产生。任何第三方都无法知晓通讯内容。即使一次会话的密钥泄露，并不影响其他会话的安全性。

3. 完整性。 加密通讯中的数据很难被篡改而不被发现。


## 使用 SSL/TLS 连接的两种方式

对于客户端的 SSL/TLS 连接，您可以根据使用场景选择以下两种使用方式之一：

| 使用方式                            | 优势                                       | 缺点                                                                                                                                                           |
| ----------------------------------- | ------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 客户端直接与 EMQX 建立 SSL/TLS 连接 | 简单易用，不需要额外的组件。               | 会增加 EMQX 的资源消耗，如果连接数量非常大，可能会导致较高的 CPU 和内存消耗。                                                                                  |
| 使用代理或负载均衡终结 TLS 连接     | 不影响 EMQX 性能，同时提供了负载均衡能力。 | 只有少数云厂商负载均衡器支持 TCP SSL/TLS 终结，需要自己部署 [HAProxy](http://www.haproxy.org/) 等软件。 |

本章节将重点介绍第一种方式，第二种方式请参考 [集群负载均衡](../../zh_CN/deploy/cluster/lb.md)。

## 1. 获取证书

EMQX 随安装包提供了一组仅用于测试的 SSL/TLS 证书(位于 `etc/certs` 目录)，并在 `8883` 端口启用了 SSL/TLS 连接，您应当在生产环境中换用可靠 CA 签发的证书。

通常获取证书有以下两种方式：

1. 自签名证书，即自己签发根 CA。但自签名证书存在较多的安全隐患，通常我们建议仅用于测试验证。
2. 申请或购买证书，免费证书可以向 [Let's Encrypt](https://letsencrypt.org/zh-cn/) 等证书颁发机构申请，收费证书则可以向 [DigiCert](https://www.digicert.com/) 等证书颁发机构购买。目前国内像华为云、腾讯云等云厂商基本也都联合知名 CA 推出了 SSL/TLS 证书服务，也可以申请和签发免费证书。对于企业级用户，一般建议申请收费的 OV 及以上类型的证书，以获取更高等级的安全保护。

<!-- TODO 补充从 CA 申请证书的方式 -->

我们假设您的系统已经安装了 OpenSSL，自签名证书步骤如下：

1. **生成 CA 密钥对**：

   后续使用该密钥签发 CA 证书。使用此命令需要设置密钥保护密码，该密码在后续生成、签发、验证证书时均需用到。请妥善保存密钥与密码。

  ```bash
  openssl genrsa -des3 -out rootCA.key 2048
  ```

2. **使用密钥对中的私有密钥生成 CA 证书**：

  要求设置证书唯一标识名称（Distinguished Name，DN）以及证书的有效天数：

  ```bash
  openssl req -x509 -new -nodes -key rootCA.key -sha256 -days 3650 -out rootCA.crt
  ```

3. **使用自签名的 CA 证书来签发服务端证书**：

  服务端证书用于验证 Broker 所有者身份，服务端证书通常颁发给主机名，它可能是服务器名称或域名（如 www.emqx.com）

  **3.1 生成服务端证书密钥对：**

  ```bash
  openssl genrsa -out server.key 2048
  ```

  **3.2 使用 Server 密钥对制作 CSR：**

  CSR（Certificate Signing Request），即证书签名请求文件。CSR 使用 CA 根证书私钥签名后能够生成证书公钥文件，也就是颁发给用户的证书。生成 CSR 时要求设置证书唯一标识名称（Distinguished Name，DN）。

  ```bash
  openssl req -new -key server.key -out server.csr
  ```

  系统将提示以下信息，对应的含义如下：

  ```bash
  You are about to be asked to enter information that will be incorporated
  into your certificate request.
  What you are about to enter is what is called a Distinguished Name or a DN.
  There are quite a few fields but you can leave some blank
  For some fields there will be a default value,
  If you enter '.', the field will be left blank.
  -----
  Country Name (2 letter code) [AU]: # 国家/地区
  State or Province Name (full name) [Some-State]: # 省/市
  Locality Name (eg, city) []: # 城市
  Organization Name (eg, company) [Internet Widgits Pty Ltd]: # 组织机构（或公司名），如 EMQ
  Organizational Unit Name (eg, section) []: # 机构部门，如 EMQX
  Common Name (e.g. server FQDN or YOUR name) []: # 通用名称，此处应当设置为服务器域名如 mqtt.emqx.com
  ...
  ```

  **3.3 使用 CA 密钥 + CA 证书 + 服务端 CSR 生成服务端证书：**

  需要 3 个文件最终生成服务端证书，可以指定证书的有效天数，此处为 365 天：

  ```bash
  openssl x509 -req -in server.csr -CA rootCA.crt -CAkey rootCA.key -CAcreateserial -out server.crt -days 365
  ```

至此您就得到了一组证书。

```bash
.
├── rootCA.crt
├── rootCA.key
├── rootCA.srl
├── server.crt
├── server.csr
└── server.key
```

## 2. 在 EMQX 中启用 SSL/TLS

1. 将 1 中的证书文件移动到 EMQX `etc/cert` 目录下。

2. 打开配置文件 `emqx.conf`，修改 `listeners.ssl.default` 配置组将证书替换为您的证书，并添加 `verify = verify_none`：

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
