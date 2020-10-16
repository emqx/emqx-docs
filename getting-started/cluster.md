# 创建集群

## 节点发现与自动集群 {#emqx-service-discovery}

EMQ X 支持基于 Ekka 库的集群自动发现 (Autocluster)。Ekka 是为 Erlang/OTP 应用开发的集群管理库，支持
Erlang 节点自动发现 (Service Discovery)、自动集群 (Autocluster)、脑裂自动愈合 (Network Partition
Autoheal)、自动删除宕机节点 (Autoclean)。

EMQ X 支持多种节点发现策略:

| 策略     | 说明                |
| ------ | ----------------- |
| manual | 手动命令创建集群         |
| static | 静态节点列表自动集群     |
| mcast  | UDP 组播方式自动集群     |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| k8s    | Kubernetes 服务自动集群 |

### 手动(manual) 方式管理集群介绍 {#emqx-service-discovery-manual-tutorial}

假设要在两台服务器 s1.emqx.io, s2.emqx.io 上部署 EMQ X 集群:

|                节点名                 | 主机名 (FQDN)  |   IP 地址    |
| ------------------------------------ | ------------- | ------------ |
| emqx@s1.emqx.io 或 emqx@192.168.0.10 | s1.emqx.io    | 192.168.0.10 |
| emqx@s2.emqx.io 或 emqx@192.168.0.20 | s2.emqx.io    | 192.168.0.20 |

**注意：** 节点名格式为 <Name@Host>, Host 必须是 IP 地址或 FQDN (主机名。域名)

#### 配置 emqx@s1.emqx.io 节点

emqx/etc/emqx.conf:

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

#### 配置 emqx@s2.emqx.io 节点

emqx/etc/emqx.conf:

```bash
node.name = emqx@s2.emqx.io
# 或
node.name = emqx@192.168.0.20
```

#### 节点加入集群

启动两台节点后，在 s2.emqx.io 上执行:

```bash
$ ./bin/emqx_ctl cluster join emqx@s1.emqx.io

Join the cluster successfully.
Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```
**注意:** s2.emqx.io加入集群后会清除本身全部的数据，同步s1.emqx.io节点的数据。如果还有s3.emqx.io节点，那么需要在s3.emqx.io节点去执行命令加入emqx@s1.emqx.io或者emqx@s2.emqx.io， 已经在集群的节点不能在join到其他节点，否则会退出当前集群和join的节点组成一个新的集群


在任意节点上查询集群状态:

```bash
$ ./bin/emqx_ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

#### 退出集群

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


## 防火墙设置 {#emqx-cluster-behind-firewall}

若预先设置了环境变量 WITH_EPMD=1, 启动 emqx 时会使用启动 epmd (监听端口 4369) 做节点发现。称为 `epmd 模式`。
若环境变量 WITH_EPMD 没有设置，则启动 emqx 时不启用 epmd，而使用 emqx ekka 的节点发现，这也是 4.0 之后的默认节点发现方式。称为 `ekka 模式`。

**epmd 模式：**
如果集群节点间存在防火墙，防火墙需要开启 TCP 4369 端口和一个 TCP 端口段。4369 由 epmd 端口映射服务使用，TCP
端口段用于节点间建立连接与通信。

防火墙设置后，需要在 `emqx/etc/emqx.conf` 中配置相同的端口段:

```bash
## Distributed node port range
node.dist_listen_min = 6369
node.dist_listen_max = 7369
```

**ekka 模式（4.0 版本之后的默认模式）：**

如果集群节点间存在防火墙，默认情况下，只需要开启 TCP 4370 端口。

但如果 node.name 配置制定的节点名字里，带有数字后缀(Offset)，则需要开启 4370 + Offset 端口。

比如：

```
node.name = emqx-1@192.168.0.12
```

则需要开启 4371 端口。
