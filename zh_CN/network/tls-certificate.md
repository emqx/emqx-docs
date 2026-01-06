# SSL/TLS 证书

SSL/TLS 证书是 EMQX 安全体系的重要组成部分，用于在通信过程中提供身份认证、数据加密和完整性保护。在 EMQX 中，以下场景需要使用 SSL/TLS 证书：

- 基于 TLS 的 MQTT 连接（MQTTS）
- 基于安全 WebSocket 的 MQTT 连接（WSS）
- HTTPS 服务与 Dashboard 访问
- 启用 TLS 的外部连接（例如数据集成）

从 EMQX 6.1 开始，证书不再仅仅被视为配置中的文件路径，而是作为可复用的资源进行管理。EMQX 支持以下两种证书管理方式：

1. **基于路径的证书**（传统方式）：在配置中通过文件路径直接引用证书。
2. **托管证书**（EMQX 6.1+）：将证书作为可复用资源进行集中管理，并通过名称进行引用。

这两种方式都使用标准的 PEM 编码文件，并与 EMQX 的 SSL/TLS 实现完全兼容。

本页面将介绍：

- 如何获取 SSL/TLS 证书  
- EMQX 中证书的管理
- EMQX 如何支持多证书配置以及自动化证书管理环境（Automated Certificate Management Environment， ACME）
- 更新 SSL/TLS 证书

## 获取 SSL/TLS 证书

在 EMQX 使用证书之前，必须先从受信任的来源获取 SSL/TLS 证书。选择哪种获取方式取决于您的部署环境和安全需求。

### 获取证书的方式

您可以通过以下方式获取 TLS 证书：

- **自签名证书**

  由您自己创建的证书颁发机构（CA）签发的证书。此类证书存在较多的安全隐患，仅建议用于测试环境或受控环境。

- **申请或购买由受信任 CA 签发的证书**

  从公共 CA 或企业级 CA 获取的证书，例如：

  - [Let's Encrypt](https://letsencrypt.org/zh-cn/)
  - 云服务提供商（例如华为云、腾讯云）
  - 商业 CA（例如 [DigiCert](https://www.digicert.com/)）

  在生产环境和企业级部署中，通常建议使用 OV（组织验证）或更高级别的证书，以获得更高的安全保障。

- **使用 ACME 自动签发证书**

  EMQX 支持通过 ACME 协议（例如 Let’s Encrypt）自动获取并续期服务器证书。

### 创建自签名 CA 证书

:::tip 前置准备

已安装 [OpenSSL](https://www.openssl.org/)。

:::

1. 运行以下命令生成密钥对，该命令随即会提示您输入密钥保护密码，后续在生成、签发、验证证书时均需要此密码。请妥善保管相关密钥及密码。

```bash
openssl genrsa -des3 -out rootCA.key 2048
```

2. 运行以下命令通过密钥对中的私有密钥生成 CA 证书，该命令随即会提示您设置证书的唯一标识名称 DN（Distinguished Name）。

```bash
openssl req -x509 -new -nodes -key rootCA.key -sha256 -days 3650 -out rootCA.crt
```

### 签发服务器证书

使用 CA 证书来签发服务器证书，用于验证服务器所有者的身份，服务器证书通常颁发给主机名、服务器名称或域名（如 [www.emqx.com](https://www.emqx.com/zh), localhost）。我们需要 CA 密钥（rootCA.key）、CA 证书（ rootCA.crt）和服务端 CSR （server.csr）生成服务器证书。

1. 运行以下命令生成服务器证书密钥对：

```bash
openssl genrsa -out server.key 2048
```

2. 运行以下命令使用 Server 密钥对制作 CSR。经 CA 根证书私钥签名后，CSR 可生成颁发给用户的证书公钥文件。该命令随即也会要求设置证书的唯一标识名称。

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
  Organization Name (eg, company) [Internet Widgets Pty Ltd]: # 组织机构（或公司名），如 EMQ
  Organizational Unit Name (eg, section) []: # 机构部门，如 EMQX
  Common Name (e.g. server FQDN or YOUR name) []: # 通用名称，此处应当设置为服务器域名如 mqtt.emqx.com
  ...
  ```

3. 使用 CSR 生成服务器证书，此时也可指定证书的有效天数，此处为 365 天：

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

### 签发客户端证书

签发客户端证书的步骤与签发服务器证书类似，只是在生成 CSR 时，需要将 Common Name 设置为客户端的唯一标识，如用户名、客户端 ID 等。

客户端证书与服务端证书使用相同的 CA 证书签名，因此客户端证书也可以使用上述 CA 证书签名。

## EMQX 中的证书管理

在获取证书之后，EMQX 支持通过两种方式对证书进行管理和引用。

### 基于路径的证书

基于路径的证书通过在监听器或其他组件的 TLS 配置项中显式指定证书文件路径来进行配置，例如：

- `certfile`
- `keyfile`
- `cacertfile`

在使用基于路径的证书时：

- 证书文件完全由用户或外部工具进行管理。
- 监听器直接引用证书文件路径。
- 证书更新通常通过替换磁盘上的证书文件来完成。

EMQX 在 `etc/certs` 目录下提供了一组示例证书，仅用于测试目的。

基于路径的证书在所有 EMQX 版本中均得到完整支持，并保持兼容。

### 托管证书

从 EMQX 6.1 开始，EMQX 引入了托管证书，这是一种集中式管理 TLS 证书文件的机制，可在多个组件之间复用。

托管证书可以在多个资源中复用，包括：

- MQTT SSL 监听器
- WSS 监听器
- HTTPS / Dashboard 监听器
- 启用 TLS 的连接器（用于数据集成）

托管证书可以通过 Dashboard 或 REST API 创建和管理，并存储在 EMQX 数据目录下的 `data/certs2/` 中。

在内部实现上，EMQX 在与 Erlang/OTP 的 SSL 库交互时，仍然使用基于路径的 PEM 文件方式，从而确保：

- 与现有 TLS 行为完全向后兼容
- 证书能够自动重新加载
- 更新证书后无需重启监听器或连接器

#### 托管证书包

托管证书包表示一组逻辑上的 TLS 相关文件，包括：

- 服务器证书
- 私钥
- 可选的 CA 证书

每个证书包由一个名称以及一个可选的命名空间进行标识，并且可以被多个监听器或连接器引用。

托管证书是一种可复用资源。更新某个托管证书包后，所有引用该证书包的监听器或连接器都会自动生效。

#### 证书格式与文件结构

EMQX 中的托管证书继续使用 PEM 编码文件，这与 EMQX 现有的 TLS 证书部署方式保持一致。

使用 PEM 文件具有以下优势：

- **透明性**：证书文件可以使用 `openssl` 等标准工具轻松查看和验证。
- **兼容性**：PEM 格式被 Erlang/OTP 的 SSL 库原生支持，并提供：
  - 基于文件的证书缓存
  - 证书文件更新后的自动重新加载

##### 证书文件结构

每个托管证书包在磁盘上以一个目录的形式存储，该目录由证书名称和命名空间标识。

示例：

```
mqtt.example.com
tenant1/certs1
```

一个托管证书包可能包含以下文件：

- `key.pem`（必需）：私钥
- `chain.pem`（必需）：证书链（不包含根 CA）
- `ca.pem`（可选）：用于验证对端证书的根 CA 证书包
- `acc-key.pem`（可选）：ACME 账户私钥（仅用于 TLS 服务器证书）

并非所有场景都需要上述所有文件。例如，`acc-key.pem` 仅在证书通过 ACME 签发或管理时才会存在。

#### 使用 SNI 的多证书支持

当在同一个监听器上配置了多个托管证书包时，EMQX 会基于服务器名称指示（Server Name Indication，SNI）进行动态证书选择。

SNI 是 TLS 的一种扩展，允许客户端在 TLS 握手过程中、在安全连接完全建立之前，指定其要连接的主机名。

##### SNI 的工作方式

- 每个托管证书引用都可以选择性地指定一个 `sni` 值。
- 在 TLS 握手过程中：
  1. 客户端发送 SNI 主机名。
  2. EMQX 将该 SNI 与已配置的证书条目进行匹配。
  3. 如果找到匹配项，则使用对应的证书包。
  4. 如果未找到匹配项，则使用证书列表中的第一个证书作为默认证书。

该机制使得单个监听器可以在同一个 IP 地址和端口上，使用不同的 TLS 证书安全地为多个域名或租户提供服务。

##### 示例

```
listeners.ssl.default {
  bind = "0.0.0.0:8883"

  ssl_options {
    managed_certs = [
      {
        certs_bundle_name = "default-cert"
        certs_namespace = "global"
      },
      {
        certs_bundle_name = "mqtt-example-com"
        certs_namespace = "global"
        sni = "mqtt.example.com"
      }
    ]
  }
}
```

## 创建和管理托管证书

本节介绍了如何通过 Dashboard 和 REST API 创建和管理托管证书。

### 通过 Dashboard 创建托管证书

你可以直接通过 Dashboard 创建托管证书包。

1. 进入**管理** -> **监听器**（或任何支持选择托管证书的页面）。
2. 在配置监听器时，将**证书来源**设置为**从托管证书中选择**。
3. 点击**托管证书包名称**下拉框旁的**创建托管证书**。
4. 在**创建托管证书**面板中，填写以下信息：
   - **名称**（必填）：托管证书包的唯一名称。
   - **命名空间**：托管证书包所在的命名空间。
     - 全局用户只能在 `global` 命名空间下创建证书。
     - 命名空间用户只能在其所属命名空间中创建证书。
   - **TLS Cert**（必填）：PEM 格式的服务器证书，可直接粘贴或上传文件。如客户端有要求，应包含完整证书链。
   - **TLS Key**（必填）：与服务器证书对应的私钥（PEM 格式）。
   - **私钥密码**：可选。如果私钥被加密，需要填写对应密码。
   - **CA Cert**：可选。PEM 格式的 CA 证书，通常在双向 TLS 认证或被需要 CA 证书的连接器复用时使用。
5. 点击**创建**保存托管证书包。

证书包创建完成后即可被选择，并可在多个监听器或连接器之间复用。

托管证书包存储在磁盘上，并由 EMQX 自动重新加载。更新托管证书无需重启 EMQX 或其监听器。

### 通过 REST API 管理证书

除了 Dashboard，EMQX 还提供 REST API 用于管理 TLS 证书文件。通过 API 创建的托管证书与通过 Dashboard 创建的证书完全一致，并可通过相同方式被监听器和连接器引用。

#### 上传证书文件

上传证书文件以创建或更新托管证书包。支持的文件类型包括：

- `key`：私钥
- `chain`：证书链（不包含根 CA）
- `ca`：CA 证书包
- `acc-key`：ACME 账户私钥（仅用于服务器证书）

上传到指定命名空间：

```
POST /certs/ns/:NAMESPACE/name/:NAME?file=key|chain|ca|acc-key
```

上传到全局命名空间：

```
POST /certs/global/name/:NAME?file=key|chain|ca|acc-key
```

#### 列出托管证书

- 列出指定命名空间下的证书：

  ```
  GET /certs/ns/:NAMESPACE
  ```

- 列出全局命名空间下的证书：

  ```
  GET /certs/global/list
  ```

#### 删除托管证书

- 从指定命名空间删除：

  ```
  DELETE /certs/ns/:NAMESPACE/name/:NAME
  ```

- 从全局命名空间删除：

  ```
  DELETE /certs/global/name/:NAME
  ```

## 使用 ACME 自动签发托管证书

EMQX 支持通过 ACME 协议（例如 Let’s Encrypt）自动签发和续期服务器端 TLS 证书。该功能是可选的，适用于客户端通过 TLS 直接连接到 EMQX、并且使用公网可访问域名的部署场景。当 TLS 由负载均衡器终止，或仅使用私有/内部主机名时，则不适合使用该功能。

ACME 需要由 EMQX 管理员通过配置文件进行启用和配置，无法通过 Dashboard 启用，也不对命名空间级用户开放。

在 EMQX 中，ACME 仅适用于服务器证书，包括用于 MQTTS、HTTPS/Dashboard 和 WSS 监听器的证书，不适用于客户端证书。

### ACME 与托管证书的集成方式

当启用 ACME 后：

1. EMQX 作为 ACME 客户端，向配置的 ACME CA 请求证书。
2. 证书会在过期前自动续期。
3. 生成的文件会写入托管证书目录中，证书包包含：
   - `key.pem`
   - `chain.pem`
   - `acc-key.pem`（如适用）
4. EMQX 会自动创建对应的托管证书包。

通过 ACME 签发的证书在行为上与手动上传的托管证书完全一致。用户在配置监听器时只需选择或引用生成的托管证书包，无需手动上传证书。

### 通过配置文件启用 ACME

该操作需要：

- 访问 EMQX 主机
- 具备修改 EMQX 配置文件的权限
- 对部署环境拥有管理员级控制权限

示例配置：

```hocon
## ACME CA 目录 URL（例如 Let’s Encrypt）
acme.directory_url = "https://acme-v02.api.letsencrypt.org/directory"

## 用于 ACME 账户注册的联系邮箱
acme.contact_email = "admin@example.com"

## 证书中包含的域名
## 域名所有权通过 HTTP-01 校验方式进行验证
acme.domains = ["mqtt1.example.com", "mqtt2.example.com"]

## 用于存储已签发证书的目录
## 该目录与托管证书机制集成
acme.cert_dir = "${EMQX_MANAGED_CERTS_DIR}"
```

## 更新 SSL/TLS 证书

为确保连接的安全性，SSL/TLS 证书必须在过期之前进行更新。在 EMQX 中，证书的更新方式取决于所使用的证书管理方式。

### 更新基于路径的证书

基于路径的证书是在配置中通过文件路径直接引用的证书（例如 `certfile`、`keyfile` 和 `cacertfile`）。

更新基于路径的证书时，请按以下步骤操作：

1. 使用新的证书文件、私钥文件或 CA 证书文件，替换 `./etc` 或 `/etc/emqx/etc` 目录中现有的证书文件。
2. 确保配置中引用的证书文件路径保持不变。

EMQX 会自动重新加载更新后的证书文件：

- EMQX 会定期检查并重新加载证书文件。
- 默认情况下，证书每 120 秒重新加载一次。
- 在大多数情况下，无需重启监听器。

### 更新托管证书

托管证书通过更新证书包的方式进行更新。

要更新托管证书，可以通过 Dashboard 或托管证书 API，向已有的托管证书包中上传新的证书、私钥或 CA 证书。

证书包更新完成后：

- 所有引用该托管证书的监听器和启用 TLS 的组件都会自动使用更新后的证书。
- 无需重启监听器或 EMQX。

托管证书被设计为可复用资源。更新某个证书包会影响所有引用该证书包的资源。

### 使用 ACME 自动续期（仅适用于托管证书）

对于通过 ACME 签发的托管证书：

- EMQX 会在证书过期前自动完成续期。
- 续期后的证书文件会写入托管证书目录。
- 相应的托管证书包会自动更新。

在启用并配置 ACME 后：

- 无需手动上传证书。
- 监听器继续引用同一个托管证书包。
- 证书轮换过程不会中断服务连接。

## 下一步

一旦您获得了 SSL/TLS 证书，您便可以启用客户端的 SSL/TLS 连接。

- [启用 SSL/TLS 连接](./emqx-mqtt-tls.md)
