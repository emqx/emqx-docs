# 创建与管理集群

EMQX 支持手动创建集群，也支持通过多种方式自动集群，本章节将指导您使用不同的方式创建并管理 EMQX 集群。

:::tip 前置条件：

- 了解 [分布式集群](./introduction.md)。
- 了解 [部署架构与集群要求](./mria-introduction.md)。
:::

## 创建前的准备

1. 所有节点设置唯一的节点名，节点名格式为 `name@host`，host 必须是 IP 地址或 FQDN(主机名或域名)。
2. 如果节点之间存在防火墙或安全组，确保已经开放集群通信端口，参考 [集群内通信端口](./security.md)。
3. 所有节点使用相同的 cookie。

:::tip
集群一经创建，节点名是不可变的，因为数据数据库和数据文件都与之相关。即使网络环境提供静态 IP 的情况下，也强烈建议使用静态 FQDN 作为 EMQX 节点名。
:::

## manual 手动创建集群

在手动集群中，您必须手动配置集群中的每个节点，包括设置节点之间的网络连接。

与自动集群相比，手动集群能够精细的配置自定义的网络拓扑结构，在自动集群机制不可用或不适合的情况下，手动集群非常适用。

假设有 `emqx@s1.emqx.io` 和 `emqx@s2.emqx.io` 两个节点，我们可以通过如下步骤为其手动创建集群：

1. 将集群发现策略设置为 `manual`:

    ```bash
    cluster {
        ## 可选 manual | static | mcast | dns | etcd | K8s
        discovery_strategy  =  manual
    }
    ```

2. 启动两台节点后，在其中一台节点执行集群加入命令：

    ```bash
    $ ./bin/emqx_ctl cluster join emqx@s1.emqx.io

    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
    ```

    :::tip
    1. 必须在待加入的节点执行该命令，以**请求**而不是**邀请**加入到集群。
    2. `emqx@s2.emqx.io` 加入 `emqx@s1.emqx.io` 组成集群后，它将清除本地数据并将 `emqx@s1.emqx.io` 中的数据同步过来。
    3. 已加入集群的节点加入另一个集群时，该节点将离开当前集群。
    :::

3. 在任意节点上查询集群的状态：

    ```bash
    $ ./bin/emqx_ctl cluster status

    Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
    ```

4. 退出集群，有以下两种方式：
   1. leave：让本节点退出集群
   2. force-leave：在集群内移除节点

   在 `emqx@s2.emqx.io` 上执行以下命令，让其退出集群：

    ```bash
    ./bin/emqx_ctl cluster leave
    ```

    或在 `emqx@s1.emqx.io` 上从集群移除 `emqx@s2.emqx.io` 节点：

    ```bash
    ./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io
    ```

### 单机伪分布式

对于只有单台服务器的用户来说，如果您想在一台服务器上测试 EMQX 集群，可以使用伪分布式集群。

启动第一个节点后，通过以下命令启动第二个节点并手动加入集群，为避免端口冲突，我们需要对一些监听端口做出调整：

```bash
EMQX_NODE__NAME='emqx2@127.0.0.1' \
    EMQX_STATSD__SERVER='127.0.0.1:8124' \
    EMQX_LISTENERS__TCP__DEFAULT__BIND='0.0.0.0:1882' \
    EMQX_LISTENERS__SSL__DEFAULT__BIND='0.0.0.0:8882' \
    EMQX_LISTENERS__WS__DEFAULT__BIND='0.0.0.0:8082' \
    EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8085' \
    EMQX_DASHBOARD__LISTENERS__HTTP__BIND='0.0.0.0:18082' \
    EMQX_NODE__DATA_DIR="./data2" \
./bin/emqx start

./bin/emqx_ctl cluster join emqx1@127.0.0.1
```

## 节点发现与自动集群

节点发现是创建集群的必要过程，它允许单个 EMQX 节点发现对方并互相通信，无论其位置或 IP 地址如何。

EMQX 支持基于 [Ekka](https://github.com/emqx/ekka) 库的集群自动发现 (Autocluster)。Ekka 是为 Erlang/OTP 应用开发的集群管理库，支持 Erlang 节点自动发现 (Service Discovery)、自动集群 (Autocluster)、脑裂自动愈合 (Network Partition Autoheal)、自动删除宕机节点 (Autoclean)。

EMQX 支持多种节点发现策略：

| 策略     | 说明                |
| ------ | ----------------- |
| manual | 手动命令创建集群        |
| static | 静态节点列表自动集群        |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| K8s    | Kubernetes 服务自动集群 |

EMQX 默认配置为手动创建集群，您可以通过 `emqx.conf` 配置文件配置节点发现策略：

```bash
cluster {
    ## 可选 manual | static | mcast | dns | etcd | K8s
    discovery_strategy  =  manual
}
```

注意：5.0 之前版本中的 mcast 发现策略已被废弃，在未来的版本中会被删除。

## 基于 static 节点列表自动集群

静态集群的原理是在所有需要加入集群的节点中配置一个相同的节点列表，这个列表包含所有节点的节点名，在各节点启动后，会根据列表自动建立一个集群。

静态集群是自动集群中最简单的一种，只需要各节点间可以通过 TCP 协议互相访问，不需要任何其他网络组件或服务。

在所有节点 `emqx.conf` 文件中配置相同的集群方式和节点列表：

```bash
cluster {
    discovery_strategy = static
    static {
        seeds = ["emqx@s1.emqx.io", "emqx@s2.emqx.io"] 
    }
}
```

逐一启动所有节点，集群即可建立。

## 基于 DNS 自动集群

### 工作原理

[DNS](https://tools.ietf.org/html/rfc1034) 是 Domain Name System 的缩写，即域名解析系统。一台 DNS 服务器在收到域名查询请求后，会返回这个域名对应的 IP 地址，也就是所谓的 A（Address）记录。DNS 允许一个域名有多项 A 记录，也就是多个IP地址，这样就形成了一个名字对应多个 IP 地址的映射。

EMQX 的 DNS 自动集群就是利用这样的一对多的映射来找到集群中所有的节点，使各个独立的节点都能加入到集群中。

**配置 DNS**

大部分的公有云服务中都有 DNS 服务，分配域名后，在管理界面把 EMQX 各个节点的 IP 地址添加到该域名的 A 记录中即可。如果 EMQX 部署在私有云或者内网中，域名也仅在本地网络有效，那么您可能需要使用如 [BIND](https://www.isc.org/bind/) 等软件建立自己的域名服务器。

准备完成后，在所有节点 `emqx.conf` 文件中配置相同的集群方式，即 `DNS`。DNS 自动集群支持 A 记录与 SRV 记录：

**DNS A 记录自动集群：**

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        record_type = a
    }
}
```

**DNS SRV 记录自动集群：**

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        record_type = srv
    }
}
```

配置完成后逐一启动所有节点即可完成集群。

## 基于 etcd 自动集群

[etcd](https://etcd.io/) 是 CoreOS 发起的开源项目，etcd 的应用场景多间于服务发现，解决分布式系统中同一个集群的进程之间如何相互发现并建立连接的问题，这个功能正是 EMQX 自动集群所需要的。

当网络中存在 etcd 服务器（集群）的时候，EMQ X 集群可以使用 ectd 的方式自动建立集群。安装和配置 etcd 服务集群请参考 [etcd install](https://etcd.io/docs/latest/install/)。

**配置 ectd**

您需要指定 etcd 服务器的地址，如果存在多个 etcd 服务器的时候，您可以使用 `,` 分隔多个服务器。同时还需要用于指定 EMQX 节点的目录前缀以及 TTL。

示例代码：

```bash
cluster {
    discovery_strategy = etcd
    etcd {
        server = "http://127.0.0.1:2379"
        prefix = emqxcl
        node_ttl = 1m
    }
}
```

在完成配置以后，我们可以逐一启动 EMQX 节点，并用 etcdctl 工具观察 etcd 服务器上的变化：

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

以上结果表明所有节点都正常启动并自动加入集群。

## 基于 kubernetes 自动集群

[Kubernetes（K8s）](https://kubernetes.io) 是 Google 的开源容器集群管理系统，是一个完备的分布式系统支撑平台，EMQ X 可以使用 kubernetes 的服务发现功能组建集群。

您需要为所有节点指定 Kubernetes API 服务器，EMQX 在 K8s 上的服务名，地址类型:

<!-- TODO 补充几个参数的作用介绍 -->

```bash
cluster {
    discovery_strategy = K8s
    K8s {
        apiserver = "http://10.110.111.204:8080"
        service_name = emqx
        address_type = ip
        app_name = emqx
        suffix = "pod.local"
        namespace = default
    }
}
```

配置完成后逐一启动所有节点即可完成集群。

::: tip
Kubernetes 不建议使用 Fannel 网络插件，推荐使用 Calico 网络插件。
:::
