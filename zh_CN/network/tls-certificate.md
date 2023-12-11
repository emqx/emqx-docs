# 获取 SSL/TLS 证书

您可通过以下两种方式获取相关 SSL/TLS 证书：

1. 自签名证书：即使用自己签发的证书，由于自签名证书存在较多的安全隐患，因此只建议用于测试验证环境。
2. 申请或购买证书：您可以向 [Let's Encrypt](https://letsencrypt.org/zh-cn/) 或华为云、腾讯云等云厂商申请免费证书，也可以向 [DigiCert](https://www.digicert.com/) 等机构购买收费证书。对于企业级用户，一般建议申请收费的 OV 及以上类型的证书，以获取更高等级的安全保护。

## 创建自签名证书

:::tip 前置准备

已安装 [OpenSSL](https://www.openssl.org/)。

:::

1. 运行以下命令生成密钥对，该命令随即会提示您输入密钥保护密码，后续在生成、签发、验证证书时均需要此密码。请妥善相关密钥及密码。

```bash
openssl genrsa -des3 -out rootCA.key 2048
```

2. 运行以下命令通过密钥对中的私有密钥生成 CA 证书，该命令随即会提示您设置证书的唯一标识名称 DN（Distinguished Name）。

```bash
openssl req -x509 -new -nodes -key rootCA.key -sha256 -days 3650 -out rootCA.crt
```

3.  使用步骤 2 中的 CA 证书来签发服务端证书，用于验证服务器所有者的身份，服务端证书通常颁发给主机名、服务器名称或域名（如 www.emqx.com）。我们需要 CA 密钥（rootCA.key）、CA 证书（ rootCA.crt）和服务端 CSR （server.csr）生成服务端证书。

  3.1 运行以下命令生成服务端证书密钥对：

  ```bash
openssl genrsa -out server.key 2048
  ```

  3.2 运行以下命令使用 Server 密钥对制作 CSR。经 CA 根证书私钥签名后，CSR 可生成颁发给用户的证书公钥文件。该命令随即也会要求设置证书的唯一标识名称。

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

  3.3 生成服务端证书，此时也可指定证书的有效天数，此处为 365 天：

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

<!--申请或购买证书 -->

## 下一步

一旦您获得了 SSL/TLS 证书，您便可以启用客户端的 SSL/TLS 连接。您还可以在证书过期时更新证书。

- [启用 SSL/TLS 连接](./emqx-mqtt-tls.md)
- [更新证书](./emqx-mqtt-tls.md#更新-ssl-tls-证书)