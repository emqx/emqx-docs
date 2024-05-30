# 创建集群

EMQX 集群可为您带来高容错及高可用性等优势，即使集群中的一个或多个节点出现故障，集群仍可继续运行。您可以手动或自动创建EMQX集群。创建集群前，我们需要先熟悉两个关键概念，节点名称和节点发现。

## 概念

### 节点名称

EMQX 节点通过其名称进行标识。节点名称由两部分组成，节点名称和主机，用@分隔，例如，[emqx@s1.emqx.io](mailto:emqx@s1.emqx.io)。主机部分必须是IP地址或完全限定域名（FQDN），例如 myhost.example.tld，例如：

- 对于部署在服务器s1.emqx.io上的EMQX节点，节点名称应为[emqx@s1.emqx.io](mailto:emqx@s1.emqx.io)；
-  如果该服务器具有静态IP（192.168.0.10），则节点名称应为emqx@192.168.0.10。

::: tip

EMQX节点名称是不可变的，因此建议使用静态 FQDN 作为 EMQX 节点名称。

:::

### 节点发现 

EMQX 的集群功能的关键步骤之一是节点发现，节点发现在不同 IP 或位置的单个 EMQX 节点之间实现了相互发现和通信。EMQX支持多种节点发现策略：

| 策略   | 说明                    |
| ------ | ----------------------- |
| manual | 手动命令创建集群        |
| static | 静态节点列表自动集群    |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| k8s    | Kubernetes 服务自动集群 |

注意：mcast发现策略已被废弃，在未来的版本中会被删除。

EMQX 默认配置为手动创建集群，节点须通过 `./bin/emqx_ctl join <Node>` 命令加入:

```bash
cluster.discovery = manual
```

## 前置准备

在创建 EMQX 集群之前，必须先满足以下先决条件：

- 所有节点都以 `name@host` 的格式设置了唯一的节点名称，其中主机必须是 FQDN 或者 IP 地址。

- 如果在节点之间存在防火墙或安全组，请确保已打开集群通信端口。有关详细信息，参见[集群安全](./cluster-security.md)。

- 出于安全考虑，您应该在所有节点的默认 cookie 设置为 Secret cookie 。注意：所有加入集群的节点都应使用相同的 Secret cookie。具体信息，参见[集群安全](./cluster-security.md)。

  ```bash
  node.cookie = emqxsecretcookie
  ```

## 配置节点名称

假设要为分别部署在服务器 s1.emqx.io 和 s2.emqx.io 上的两个 EMQX 节点创建集群，您可按照如下步骤配置节点名称：

### 配置 emqx@s1.emqx.io 节点

在配置文件 emqx/etc/emqx.conf 添加如下配置：

```bash
node.name = emqx@s1.emqx.io
# 或
node.name = emqx@192.168.0.10
```

也可通过环境变量:

```bash
export EMQX_NODE_NAME=emqx@s1.emqx.io && ./bin/emqx start
```

**注意:** 节点启动加入集群后，节点名称不能变更。

### 配置 emqx@s2.emqx.io 节点

emqx/etc/emqx.conf:

```bash
node.name = emqx@s2.emqx.io
# 或
node.name = emqx@192.168.0.20
```

## 手动集群

EMQX 支持通过手动或自动方式创建 EMQX 集群。本节将介绍如何通过手动的方式创建集群。在配置文件 `cluster.conf` 中设置通过手动方式配置集群。

```bash
cluster.discovery = manual
```

## 节点加入集群

启动两台节点后，在 s2.emqx.io 上执行:

```bash
$ ./bin/emqx_ctl cluster join emqx@s1.emqx.io

Join the cluster successfully.
Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

或者在 s1.emqx.io 上执行:

```bash
$ ./bin/emqx_ctl cluster join emqx@s2.emqx.io

Join the cluster successfully.
Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

在任意节点上查询集群状态:

```bash
$ ./bin/emqx_ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

## 退出集群

节点退出集群，两种方式:

1. leave: 让本节点退出集群
2. force-leave: 从集群删除其他节点

让 emqx@s2.emqx.io 主动退出集群:

```bash
$ ./bin/emqx_ctl cluster leave
```

或在 s1.emqx.io 上，从集群删除 emqx@s2.emqx.io 节点:

```bash
$ ./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io
```



## 自动集群

EMQX 支持基于以下节点发现策略自动集群：

| 策略   | 说明                    |
| ------ | ----------------------- |
| static | 静态节点列表自动集群    |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| k8s    | Kubernetes 服务自动集群 |

注意：mcast发现策略已被废弃，在未来的版本中会被删除。

在配置文件 `cluster.conf` 中设置节点发现方式。

```bash
cluster.discovery = static
# static, dns, etcd, k8s
```

### 基于 static 节点列表自动集群

配置固定的节点列表，自动发现并创建集群:

```bash
cluster.discovery = static
cluster.static.seeds = emqx1@127.0.0.1,emqx2@127.0.0.1
```

### 基于 mcast 组播自动集群

基于 UDP 组播自动发现并创建集群:

```bash
cluster.discovery = mcast
cluster.mcast.addr = 239.192.0.1
cluster.mcast.ports = 4369,4370
cluster.mcast.iface = 0.0.0.0
cluster.mcast.ttl = 255
cluster.mcast.loop = on
```

### 基于 DNS A 记录自动集群

基于 DNS A 记录自动发现并创建集群:

```bash
cluster.discovery = dns
cluster.dns.name = localhost
cluster.dns.app  = ekka
```

### 基于 etcd 自动集群

基于 [etcd](https://coreos.com/etcd/) 自动发现并创建集群:

```bash
cluster.discovery = etcd
cluster.etcd.server = http://127.0.0.1:2379
cluster.etcd.prefix = emqcl
cluster.etcd.node_ttl = 1m
```

### 基于 kubernetes 自动集群

[Kubernetes](https://kubernetes.io/) 下自动发现并创建集群:

```bash
cluster.discovery = k8s
cluster.k8s.apiserver = http://10.110.111.204:8080
cluster.k8s.service_name = ekka
cluster.k8s.address_type = ip
cluster.k8s.app_name = ekka
```

> Kubernetes 不建议使用 Fannel 网络插件，推荐使用 Calico 网络插件。

## 单机伪分布式

对于只有个人电脑或者一台服务器的用户来说，可以使用伪分布式集群。请注意，我们若要在单机上启动两个或多个 emqx 实例，为避免端口冲突，我们需要对其它节点的监听端口做出调整。

基本思路是复制一份 emqx 文件夹然后命名为 emqx2 ，将原先所有 emqx 节点监听的端口 port 加上一个偏移 offset 作为新的 emqx2 节点的监听端口。例如，将原先 emqx 的MQTT/TCP 监听端口由默认的 1883 改为了 2883 作为 emqx2 的 MQTT/TCP 监听端口。完成以上操作的自动化脚本可以参照 [集群脚本](https://github.com/terry-xiaoyu/one_more_emqx)，具体配置请参见 [配置说明](../getting-started/config.md) 与 [配置项](../configuration/configuration.md)。
