# 集群配置

在 EMQX 中，集群是多个 EMQX 节点的组合，这些节点协同工作，提供一个具有高扩展性和高容错能力的 MQTT 消息传递系统。通过建立集群，可以把负载分散到多个节点上，确保即便部分节点出现故障，系统依然能够正常运行。

## 配置节点名称

在开始创建集群的步骤之前需要先熟悉 EMQX 中的节点名称概念。EMQX 节点通过它们的名称来识别。一个节点名称由两部分组成，节点名称和主机名，用`@`分隔，例如，`emqx@s1.emqx.io`。主机部分必须是 IP 地址或者完全限定域名 (FQDN)，如 `myhost.example.tld`，例如：

- 对于部署在服务器 `s1.emqx.io` 上的 EMQX 节点，节点名称应该是 `emqx@s1.emqx.io`；
- 如果这个服务器有一个静态IP (`192.168.0.10`)，则节点名称应该是 `emqx@192.168.0.10`。

在使用 `emqx.conf` 配置节点时，可以使用以下代码：

```bash
node {
  name = "emqx@s1.emqx.io"
  role = core
}
```

其中，

- `name ` 指的是所需的节点名称，例如，`emqx@localhost`。
- `role` 指的是 EMQX 节点在 EMQX 集群中执行的功能。有两种类型的角色：核心节点和复制节点。关于核心节点和复制节点的详细解释，请参见 [EMQX 集群 - 核心节点和复制节点](../deploy/cluster/mria-introduction.md)。
  - 默认值：`core`
  - 可选值：`core` 或 `replicant`

## 配置集群

本节介绍如何配置 EMQX 集群。您可以在核心节点或复制节点上添加集群配置项，如果您正在配置一个复制节点，有几个配置项，例如 `core_nodes`，只在某些前提条件下生效：

- 节点的 `node.db_backend`设置为 `rlog`，表示节点使用 `rlog `作为数据库后端。
- `node.role `设置为 `replicant`，表示此节点作为一个复制节点功能。
- `node.discovery_strategy `设置为 `manual` 或 `static`，如果使用自动集群发现机制，则不需要设置此配置项。关于节点发现策略及相应配置项的详细解释，请参见[创建集群](../deploy/cluster/create-cluster.md)。

```bash
cluster {
  name = emqxcl
  discovery_strategy = manual
  core_nodes = []
  driver = tcp
  ssl_options {
    certfile = ""
    keyfile = ""
    cacertfile = ""
  }
}
```

其中，

| 配置项                             | 描述                                                         | 默认值   | 可选值                                   |
| ---------------------------------- | ------------------------------------------------------------ | -------- | ---------------------------------------- |
| `name`                             | 设置集群的名称。                                             | `emqxcl` |                                          |
| `discovery_strategy`               | 设置集群的节点发现策略。                                     | `manual` | `manual`, `static`, `dns`, `etcd`, `k8s`, `singleton` |
| `core_nodes`                       | 设置这个复制节点将连接的核心节点。<br />可以在这里添加多个节点，用`,`分隔。 | --       | --                                       |
| `driver`                           | 设置 EMQX 节点间通信的传输协议。                             | `tcp`    | `tcp`, `SSL`                             |
| `ssl_options`                      | 设置监听器的 SSL/TLS 配置选项，它有三个属性。                | --       | --                                       |
| `ssl_options.cacertfile`           | 包含监听器用来验证客户端证书真实性的受信任 CA（证书颁发机构）证书的 PEM 文件。 | --       | --                                       |
| `ssl_options.certfile`             | 包含监听器的 SSL/TLS 证书链的 PEM 文件。如果证书不是直接由根 CA 颁发的，中间 CA 证书应该在监听器证书之后附加，以形成一个证书链。 | --       | --                                       |
| `ssl_options.keyfile`              | 包含与 SSL/TLS 证书对应的私钥的 PEM 文件。                   | --       | --                                       |
| `ssl_options.fail_if_no_peer_cert` | 如果设置为 `true`，服务器在客户端没有发送证书（即发送空证书）的情况下失败。如果设置为 `false`，则只在客户端发送无效证书时失败（一个空证书被认为是有效的）。 | --       | --                                       |

::: tip

EMQX 提供了更多配置项以更好地满足定制化需求。详情请参见 [EMQX 开源版配置手册](https://docs.emqx.com/zh/emqx/v@CE_VERSION@/hocon/)和 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

:::
