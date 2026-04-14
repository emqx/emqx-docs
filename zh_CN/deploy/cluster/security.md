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

## 使用防火墙规则缓解 SSRF

管理功能和各类集成可能需要 EMQX 主动建立出站网络连接。如果委派管理员可以配置命名空间范围内的资源，建议通过主机级出站访问控制，确保 EMQX 节点只能访问经批准的目标地址。

这不能替代 EMQX 的基于角色访问控制，但可以作为额外的纵深防御手段，降低 SSRF 风险，防止 Broker 所在主机访问任意端点。

在规划出站访问限制时，建议：

- 仅放行部署实际需要访问的目标地址，例如身份提供商、Webhook 接收端和连接器后端服务。
- 默认拒绝 SSRF 攻击中常见的敏感地址，例如回环地址、链路本地地址以及实例元数据地址，除非您的部署明确需要访问它们。尤其建议显式阻止以下元数据端点：
  - `100.100.100.200`，阿里云元数据服务
  - `169.254.169.253`，AWS 外部元数据服务
  - `169.254.169.254`，AWS 和 Azure 元数据服务
  - `fd00:ec2::254`，AWS IPv6 元数据服务
- 如果您在 EC2 上为 Amazon MSK 使用 AWS IAM 认证，则不要阻止 `169.254.169.254`，因为 EMQX 需要访问实例元数据服务来获取凭据或生成认证令牌。该例外也应体现在您的 `iptables` 或 `nftables` 规则中。
- 在生产环境启用前，先在预发或测试环境中仔细验证规则。
- 如果 EMQX 运行在容器或 Kubernetes 中，应通过宿主机防火墙、云安全组或 Kubernetes NetworkPolicy 实现等效的出站访问控制。

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
# 如果您在 EC2 上为 Amazon MSK 使用 AWS IAM 认证，请不要直接套用
# 这条对 169.254.169.254 生效的整段拒绝规则，而应改为更具体的放行规则。
iptables -A OUTPUT -d 169.254.0.0/16 -j REJECT
iptables -A OUTPUT -d 100.100.100.200 -j REJECT
ip6tables -A OUTPUT -d fd00:ec2::254 -j REJECT

# 默认拒绝所有新的其他出站连接
iptables -A OUTPUT -m conntrack --ctstate NEW -j REJECT
```

如果您的宿主机使用 `nftables` 而非 `iptables`，也应实现同样的策略，并显式阻止这些已知元数据端点：`100.100.100.200`、`169.254.169.253`、`169.254.169.254` 和 `fd00:ec2::254`。如果您在 EC2 上为 Amazon MSK 使用 AWS IAM 认证，请确保规则保留对 `169.254.169.254` 的访问。
