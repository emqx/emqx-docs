# 集群安全

当涉及到 EMQX 集群的安全时，主要有两个方面需要考虑。

* 确保每个节点在集群中监听的端口安全。
* 对 Erlang 的 cookie 进行保密，见 `node.cookie` 配置。

::: tip
建议通过配置防火墙规则来保持集群端口的内部防火墙规则，如 AWS 安全组，或 iptables。
:::

## 集群内通信端口

为了形成一个集群，EMQX 节点需要通过一些常规端口进行互联

如果集群节点之间有防火墙，常规监听端口应该允许集群中的其他节点连通。

EMQX 节点之间有**两种不同的通道**进行通信。

### Erlang 分布式传输端口

::: tip
EMQX 采用了传统的端口映射机制，**而非** [Erlang Port Mapper Daemon, EPMD](https://www.erlang.org/docs/28/apps/erts/epmd_cmd)。
:::

Erlang 分布端口。`ListeningPort = BasePort + Offset`。
其中 `BasePort` 是 4370（默认不可以配置），`Offset` 是节点名称的后缀。
如果节点名称没有数字后缀，`Offsset` 就是 0。

例如，在 `emqx.conf` 中的 `node.name = emqx@192.168.0.12` 应该使
节点的监听端口为 `4370`，而 `emqx1`（或 `emqx-1`）的端口为 `4371`，以此类推。

### 集群 RPC 端口

默认情况下，每个 EMQX 节点有一个 RPC 监听端口。
防火墙应该配置成允许访问。

端口映射规则类似于 Erlang 分布式端口映射规则。
只有 "BasePort" 是 "5370"。

也就是说，在 `emqx.conf` 中的 `node.name = emqx@192.168.0.12` 应该使节点的
监听端口 `5370`，端口 `5371` 用于 `emqx1`（或 `emqx-1`），以此类推。

::: tip
Docker 容器中的 EMQX 使用静态端口 `5369` 进行集群 RPC。
:::

## 使用 TLS 为集群 RPC 传输层

::: tip
TLS 是以增加 CPU 负载和 RAM 使用为代价的。
:::

要为集群 RPC 配置 TLS，应在 `emqx.conf` 中进行以下配置：

```
rpc {
  driver = ssl
  # PEM format file containing the trusted CA (certificate authority) certificates that the listener uses to verify the authenticity of the cluster peers.
  cacertfile = "/path/to/cert/ca.pem"
  # PEM format file containing the SSL/TLS certificate chain for the listener. If the certificate is not directly issued by a root CA, the intermediate CA certificates should be appended after the listener certificate to form a chain.
  certfile = "/path/to/cert/domain.pem"
  # PEM format file containing the private key corresponding to the SSL/TLS certificate
  keyfile = "/path/to/cert/domain.key"
  # Set to 'verify_peer' to verify the authenticity of the clients' certificates, otherwise 'verify_none'.
  verify = verify_peer
  # If set to true, the handshake fails if the peer does not have a certificate to send, that is, sends an empty certificate. If set to false, it fails only if the peer sends an invalid certificate (an empty certificate is considered valid).
  fail_if_no_peer_cert = true

}
```

以下是创建证书和自签名 CA 的步骤。

1. 使用 `openssl` 工具创建一个根 CA。

   ```
   # Create self-signed root CA:
   openssl req -nodes -x509 -sha256 -days 1825 -newkey rsa:2048 -keyout rootCA.key -out rootCA.pem -subj "/O=LocalOrg/CN=LocalOrg-Root-CA"
   ```

2. 使用在步骤 1 创建的 `rootCA.pem` 为节点生成 CA 签名的证书。

   ```
   # Create a private key:
   openssl genrsa -out domain.key 2048
   # Create openssl extfile:
   cat <<EOF > domain.ext
   authorityKeyIdentifier=keyid,issuer
   basicConstraints=CA:FALSE
   subjectAltName = @alt_names
   [alt_names]
   DNS.1 = backplane
   EOF
   # Create a CSR:
   openssl req -key domain.key -new -out domain.csr -subj "/O=LocalOrg"
   # Sign the CSR with the Root CA:
   openssl x509 -req -CA rootCA.pem -CAkey rootCA.key -in domain.csr -out domain.pem -days 365 -CAcreateserial -extfile domain.ext
   ```

   集群中的所有节点必须使用由同一 CA 签署的证书。

3. 把生成的 `domain.pem`、`domain.key` 和 `rootCA.pem` 文件放到集群的每个节点上的 `/var/lib/emqx/ssl`。
   确保 EMQX 用户可以读取这些文件，并且权限设置为 `600`。


### 为 Erlang 分布式协议使用 TLS

::: tip
TLS 是以增加 CPU 负载和 RAM 使用为代价的。
:::

EMQX 核心节点用 Erlang 分布机制来同步数据库更新并管理集群中的节点，例如启停某个组件，收集运行指标等。
* 确保 `ssl_dist.conf` 文件有正确的密钥和证书的路径。
* 确保配置 `cluster.proto_dist` 被设置为 `inet_tls`。

## 结合规则引擎策略与防火墙规则防御 SSRF

EMQX 连接器、数据桥接和动作会向外部服务建立出站网络连接。如果缺乏访问控制，错误配置或恶意的目标地址可能导致 EMQX 向内部或敏感网络发起非预期请求，即服务端请求伪造（SSRF）漏洞。EMQX 提供两种互补的防御手段：覆盖范围有限的内置规则引擎 SSRF 策略，以及在运行时对所有连接器类型强制执行网络边界的主机级出站访问控制。

### 为 HTTP 和 MQTT 连接器启用 `rule_engine.ssrf`

从 EMQX `6.0.3` 开始，EMQX 为规则引擎相关的出站目标提供集群级 SSRF 策略。从 EMQX `6.0.4` 开始，该策略仅适用于 HTTP 连接器的 `url` 字段和 MQTT 连接器的 `server` 字段：

```hocon
rule_engine {
  ssrf {
    enable = true
    allow_cidrs = []
    deny_cidrs = [
      "127.0.0.0/8",
      "::1/128",
      "169.254.0.0/16",
      "fe80::/10",
      "10.0.0.0/8",
      "172.16.0.0/12",
      "192.168.0.0/16",
      "fc00::/7",
      "0.0.0.0/32",
      "224.0.0.0/4",
      "ff00::/8",
      "100.100.100.200/32",
      "169.254.169.253/32"
    ]
    deny_hosts = [
      "metadata.tencentyun.com",
      "metadata.google.internal",
      "metadata.azure.internal"
    ]
  }
}
```

如需在 Dashboard 中配置该策略，参见[规则引擎安全](../dashboard/cluster_settings.md#规则引擎安全)。

启用后，EMQX 会在测试、创建或更新 HTTP、MQTT 连接器配置时校验对应的目标地址。`deny_hosts` 中的主机会被直接拒绝；解析得到的 IP 会先匹配 `allow_cidrs`，如果没有命中，再继续匹配 `deny_cidrs`。

该策略不校验其他连接器类型、连接器启停操作、连接器删除操作或运行时出站连接。如果 HTTP 或 MQTT 连接器的目标地址在连接器创建后被策略禁止，仍可重新启用该连接器。删除连接器也不会被该策略阻止。

为保持兼容性，该策略默认关闭。如果需要防止使用被禁止的目标地址测试、创建或更新 HTTP、MQTT 连接器配置，请启用该策略。如果这些连接器必须访问内部服务，请先审查并调整 `allow_cidrs` 与 `deny_cidrs` 配置，再启用该策略。

### 何时仅启用 `rule_engine.ssrf` 通常就足够

如果同时满足以下条件，通常仅启用 `rule_engine.ssrf` 就已经足够：

- 所有需要 SSRF 防护的规则引擎目标均使用 HTTP 或 MQTT 连接器。
- 在创建连接器前已启用该策略，并且只有受信任的管理员可以修改连接器配置。
- 出站目标是稳定且可预期的，例如固定的 SaaS 端点或已批准的公共服务。
- 您的主要目标是在测试、创建或更新 HTTP、MQTT 连接器配置时阻止明显的 SSRF 目标或误配置。
- 您不依赖后续可能被重新绑定到其他地址的 DNS 名称。

### 何时还应同时实施防火墙规则

如果满足以下任一条件，建议再增加 `iptables`、`nftables`、云安全组或 Kubernetes NetworkPolicy 等主机级出站访问控制：

- 使用 HTTP 和 MQTT 以外的连接器类型。
- 策略变更需要应用于已保存的连接器配置、连接器启用操作或运行时连接。
- 委派管理员可以配置命名空间范围内的资源。
- 即使目标通过了配置时校验，EMQX 运行主机在运行时仍绝不能访问内部服务、元数据端点或管理网络。
- 您需要防御 DNS rebinding 或其他“校验通过后目标地址发生变化”的场景。
- 您的部署属于多租户、高风险场景，或需要在运行时强制执行严格的出站网络边界。

在规划出站访问限制时，建议：

- 仅放行部署实际需要访问的目标地址，例如身份提供商、Webhook 接收端和连接器后端服务。
- 默认拒绝 SSRF 攻击中常见的敏感地址，例如回环地址、链路本地地址以及实例元数据地址，除非您的部署明确需要访问它们。尤其建议显式阻止以下元数据端点：
  - `100.100.100.200`，阿里云元数据服务
  - `169.254.169.253`，AWS 外部元数据服务
  - `169.254.169.254`，AWS 和 Azure 元数据服务
  - `fd00:ec2::254`，AWS IPv6 元数据服务
- 如果您在 EC2 上使用 AWS 相关连接器或动作，并且通过省略 Access Key ID 与 Secret Access Key 的方式让 EMQX 从实例元数据服务获取凭据，请不要阻止 `169.254.169.254`。这不仅适用于 Amazon MSK IAM，也适用于 S3、S3 Tables、DynamoDB、Kinesis 等集成。该例外也应体现在您的 `iptables` 或 `nftables` 规则中。
- 在生产环境启用前，先在预发或测试环境中仔细验证规则。
- 如果 EMQX 运行在容器或 Kubernetes 中，应通过宿主机防火墙、云安全组或 Kubernetes NetworkPolicy 实现等效的出站访问控制。

`rule_engine.ssrf` 不能替代这些网络层控制。该策略只在测试、创建或更新 HTTP、MQTT 连接器配置时校验目标。其他连接器类型、策略变更后启用的已保存配置、DNS rebinding，以及目标在校验后解析到不同地址的其他场景，都需要运行时网络访问控制。

下面的 `iptables` 示例展示了基本思路。请根据您的环境调整网卡名称、端口和目标地址。如果宿主机未提供 `iptables`，请使用 `nftables` 配置等效规则：

```bash
# 允许已建立的出站连接
iptables -A OUTPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

# 如宿主机需要，可放行 DNS 和 NTP
iptables -A OUTPUT -p udp --dport 53 -j ACCEPT
iptables -A OUTPUT -p udp --dport 123 -j ACCEPT

# 仅允许访问经批准的外部服务
iptables -A OUTPUT -p tcp -d 198.51.100.10 --dport 443 -j ACCEPT
iptables -A OUTPUT -p tcp -d 203.0.113.20 --dport 443 -j ACCEPT

# 阻止常见的元数据地址和本地地址
iptables -A OUTPUT -d 127.0.0.0/8 -j REJECT
# 如果 EC2 上的 AWS 相关连接器或动作需要通过实例元数据服务获取凭据，
# 请不要直接套用这条对 169.254.169.254 生效的整段拒绝规则，而应改为更具体的放行规则。
iptables -A OUTPUT -d 169.254.0.0/16 -j REJECT
iptables -A OUTPUT -d 100.100.100.200 -j REJECT
ip6tables -A OUTPUT -d fd00:ec2::254 -j REJECT

# 默认拒绝所有新的其他出站连接
iptables -A OUTPUT -m conntrack --ctstate NEW -j REJECT
```

如果您的宿主机使用 `nftables` 而非 `iptables`，也应实现同样的策略，并显式阻止这些已知元数据端点：`100.100.100.200`、`169.254.169.253`、`169.254.169.254` 和 `fd00:ec2::254`。如果 EC2 上的 AWS 相关连接器或动作需要通过实例元数据服务获取凭据，请确保规则保留对 `169.254.169.254` 的访问。
