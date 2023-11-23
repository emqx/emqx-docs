# 创建与管理集群

EMQX 支持手动创建集群，也支持通过多种方式自动集群。本页将为您介绍手动和自动集群方式并指导您使用这两种不同的方式创建并管理 EMQX 集群。

## 基本概念

您可以在[分布式集群介绍](./introduction.md)和[部署架构](./mria-introduction.md)中了解 EMQX 集群的基础知识和工作方式。想要创建集群，您还需要熟悉以下概念：

### 节点名称

EMQX 节点通过它们的名称来识别。所有节点都设置了唯一的节点名称，格式为 `name@host`，其中 host 必须是 IP 地址或完全限定域名（FQDN）。例如：

- 对于部署在服务器 `s1.emqx.io` 上的 EMQX 节点，节点名称应为 `emqx@s1.emqx.io`；
- 如果此服务器有一个静态 IP（`192.168.0.10`），节点名称应为 `emqx@192.168.0.10`。

::: tip 

EMQX 节点名称是不可变的，因为它们被固定在数据库架构和数据文件中。因此，建议为 EMQX 节点名称使用静态 FQDN。 

:::

### 节点发现

节点发现是创建集群的必要过程，它允许单个 EMQX 节点发现对方并互相通信，无论其位置或 IP 地址如何。

### 手动与自动集群

根据节点发现的方式，集群的创建方式可以分为手动集群与自动集群。

手动集群是通过使用命令来指定哪些节点应该成为集群的一部分来创建 EMQX 集群。自动集群是另一种方法，它允许多个 EMQX 节点在无需手动配置的情况下自动形成集群。自动集群简化了设置 EMQX 集群的过程，使动态地添加或从集群中移除节点变得更加容易。EMQX 支持基于静态节点列表、DNS 记录、etcd 和 Kubernetes 的自动集群。以下表格展示了 EMQX 支持的不同节点发现策略和集群创建方法：

| 方式   | 说明                    |
| ------ | ----------------------- |
| manual | 手动命令创建集群        |
| static | 静态节点列表自动集群    |
| DNS    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| K8s    | Kubernetes 服务自动集群 |

EMQX 也支持基于 [Ekka](https://github.com/emqx/ekka) 库自动创建集群。Ekka 是为 Erlang/OTP 应用开发的集群管理库，实现了 Erlang 节点自动发现 (Service Discovery)、自动集群 (Autocluster)、 网络分区自动愈合 (Network Partition Autoheal)、自动删除宕机节点 (Autoclean) 等功能。

EMQX 默认配置为手动创建集群，您可以通过 `emqx.conf` 配置文件配置节点发现策略：

```bash
cluster {
    ## 可选 manual | static | dns | etcd | K8s
    discovery_strategy  =  manual
}
```

## 前置准备

本节为您提供了在创建集群之前如何配置节点和网络环境的指导。

### 配置节点名称

在创建集群之前，您需要了解如何命名要加入集群的节点。假设您想为分别部署在 `s1.emqx.io` 和 `s2.emqx.io` 的两个节点创建一个集群，您可以按照以下步骤创建集群。

在第一个节点的 `emqx.conf` 配置文件中配置节点名称，例如：

```bash
node.name = emqx@s1.emqx.io
```

您还可以使用环境变量覆盖节点名称：

```bash
env EMQX_NODE__NAME='emqx@s1.emqx.io' ./bin/emqx start
```

为其他节点重复上述步骤以加入集群。

现在您已经命名了两个要加入集群的节点，`emqx@s1.emqx.io` 和 `emqx@s2.emqx.io`。您可以手动或自动创建一个集群。

### 设置节点 Cookie

出于安全考虑，您应该在所有要加入集群的节点上的 `emqx.conf` 中将默认 cookie 设置更改为秘密 cookie。所有要加入集群的节点都应使用相同的秘密 cookie。有关使用的秘密 cookie 的详细信息，请参见 [分布式 Erlang - 安全性](https://www.erlang.org/doc/reference_manual/distributed.html#security)。

### 配置网络环境

确保节点之间的网络连接正常。如果节点之间存在防火墙或安全组，需要开放[集群内通信端口](./security.md)。

## 快速开始

本节为您演示了如何在 Docker 网络中使用两种不同的方式快速创建集群：

:::: tabs type:card

::: tab 手动集群示例

1. 创建一个 Docker 网络，用于节点间通信。处于同一网络下的容器可以通过容器名或网络别名相互访问：

   ```bash
   docker network create emqx-net
   ```

2. 启动第一个节点，通过环境变量设置节点名。EMQX 默认的集群方式是手动集群，因此不需要进行额外设置。将节点添加到 Docker 网络中，并设置与节点 host 相同的网络别名。

   {% emqxce %}

   ```bash
   docker run -d \
       --name emqx1 \
       -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
       --network emqx-bridge \
       --network-alias node1.emqx.com \
       -p 1883:1883 \
       -p 8083:8083 \
       -p 8084:8084 \
       -p 8883:8883 \
       -p 18083:18083 \
       emqx/emqx:@CE_VERSION@
   ```

   {% endemqxce %}

   {% emqxee %}

   ```bash
   docker run -d \
       --name emqx1 \
       -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
       --network emqx-bridge \
       --network-alias node1.emqx.com \
       -p 1883:1883 \
       -p 8083:8083 \
       -p 8084:8084 \
       -p 8883:8883 \
       -p 18083:18083 \
       emqx/emqx-enterprise:@CE_VERSION@
   ```

   {% endemqxee %}

3. 当第一个节点启动完成后，启动第二个节点。新节点需要加入与第一个节点相同的网络，由于第一个节点已经占用了 1883 等端口，此处不再映射端口。

   {% emqxce %}

   ```bash
   docker run -d \
       --name emqx2 \
       -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
       --network emqx-bridge \
       --network-alias node2.emqx.com \
       emqx/emqx:@CE_VERSION@
   ```

   {% endemqxce %}

   {% emqxee %}

   ```bash
   docker run -d \
       --name emqx2 \
       -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
       --network emqx-bridge \
       --network-alias node2.emqx.com \
       emqx/emqx-enterprise:@CE_VERSION@
   ```

   {% endemqxee %}

4. 在任意一个节点上执行[手动创建集群](#手动创建集群)的命令，将当前节点与另一节点连接创建集群：

   ```bash
   docker exec -it emqx2 \
       emqx ctl cluster join emqx@node1.emqx.com
   ```

:::

::: tab 自动集群示例（static 方式）

1. 创建一个 Docker 网络，用于节点间通信。处于同一网络下的容器可以通过容器名或网络别名相互访问：

   ```bash
   docker network create emqx-net
   ```

2. 启动第一个节点，通过环境变量设置节点名和集群方式：

   - `EMQX_NODE_NAME` 环境变量用于设置节点名。
   - `EMQX_CLUSTER__DISCOVERY_STRATEGY` 环境变量用于设置集群发现策略，此处使用[静态集群](#基于-static-节点列表自动集群)。
   - `EMQX_CLUSTER__STATIC__SEEDS` 环境变量用于设置静态节点列表，需要包含所有节点的节点名。

   还需要将节点加入到 Docker 网络中，并设置与节点 host 相同的网络别名。

   {% emqxce %}

   ```bash
   docker run -d \
       --name emqx1 \
       -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
       -e "EMQX_CLUSTER__DISCOVERY_STRATEGY=static" \
       -e "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]" \
       --network emqx-bridge \
       --network-alias node1.emqx.com \
       -p 1883:1883 \
       -p 8083:8083 \
       -p 8084:8084 \
       -p 8883:8883 \
       -p 18083:18083 \
       emqx/emqx:@CE_VERSION@
   ```

   {% endemqxce %}

   {% emqxee %}

   ```bash
   docker run -d \
       --name emqx1 \
       -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
       -e "EMQX_CLUSTER__DISCOVERY_STRATEGY=static" \
       -e "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]" \
       --network emqx-bridge \
       --network-alias node1.emqx.com \
       -p 1883:1883 \
       -p 8083:8083 \
       -p 8084:8084 \
       -p 8883:8883 \
       -p 18083:18083 \
       emqx/emqx-enterprise:@CE_VERSION@
   ```

   {% endemqxee %}

3. 当第一个节点启动完成后，启动第二个节点。集群方式以及新节点需要加入与第一个节点相同的网络，由于第一个节点已经占用了 1883 等端口，此处不再映射端口。

   {% emqxce %}

   ```bash
   docker run -d \
       --name emqx2 \
       -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
       -e "EMQX_CLUSTER__DISCOVERY_STRATEGY=static" \
       -e "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]" \
       --network emqx-bridge \
       --network-alias node2.emqx.com \
       emqx/emqx:@CE_VERSION@
   ```

   {% endemqxce %}

   {% emqxee %}

   ```bash
   docker run -d \
      --name emqx2 \
      -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
      -e "EMQX_CLUSTER__DISCOVERY_STRATEGY=static" \
      -e "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]" \
      --network emqx-bridge \
      --network-alias node2.emqx.com \
      emqx/emqx-enterprise:@CE_VERSION@
   ```

   {% endemqxee %}

:::

::::

在任意一个节点上执行 `emqx ctl cluster status` 命令查看集群状态，如果集群状态正常，将会输出如下信息：

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

至此，您已经完成了一个简单的集群创建过程，接下来可以按照以下章节的内容选择您需要的集群创建方式进行修改部署。

## 手动创建集群

本节详细介绍手动创建集群的方式。在手动创建集群过程中，您必须手动配置集群中的每个节点，包括设置节点之间的网络连接。与自动集群相比，手动集群能够精细的配置自定义的网络拓扑结构，在自动集群机制不可用或不适合的情况下，手动集群非常适用。

:::tip
手动集群仅能用于核心节点，如果使用了核心-复制节点部署架构，请使用自动集群方式管理集群。
:::

假设有 `emqx@node1.emqx.com` 和 `emqx@node2.emqx.com` 两个节点，您可以通过如下步骤为其手动创建集群：

1. 将集群发现策略设置为 `manual`:

   ```bash
   cluster {
       ## 可选 manual | static | dns | etcd | K8s
       discovery_strategy  =  manual
   }
   ```

2. 启动两台节点后，在其中一台节点执行集群加入命令：

   ```bash
   $ ./bin/emqx ctl cluster join emqx@node1.emqx.com

   Join the cluster successfully.
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

   :::tip

   1. 必须在待加入的节点执行该命令，以**请求**而不是**邀请**加入到集群。
   2. `emqx@node2.emqx.com` 加入 `emqx@node1.emqx.com` 组成集群后，它将清除本地数据并将 `emqx@node1.emqx.com` 中的数据同步过来。
   3. 已加入集群的节点加入另一个集群时，该节点将离开当前集群。如需退出集群请参考[退出集群](#退出集群)。

   :::

3. 在任意节点上查询集群的状态：

   ```bash
   $ ./bin/emqx ctl cluster status
   
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

## 自动集群

本节详细介绍多种自动创建集群的方式。

### 基于 static 节点列表自动集群

静态集群的原理是在所有需要加入集群的节点中配置一个相同的节点列表，这个列表包含所有节点的节点名，在各节点启动后，会根据列表自动建立一个集群。

静态集群是自动集群中最简单的一种，只需要各节点间可以通过 TCP 协议相互访问，不需要任何其他网络组件或服务。

在所有节点 `emqx.conf` 文件中配置相同的集群方式和节点列表：

```bash
cluster {
    discovery_strategy = static
    static {
        seeds = ["emqx@node1.emqx.com", "emqx@node2.emqx.com"]
    }
}
```

其中，

- `discovery_strategy` 是节点发现策略，将其设置为 `static`。
- `seeds` 是一个数组，您可以在其中添加加入集群的节点，多个节点可以用 `,` 分隔。

逐一启动所有节点，集群即可建立。

### 基于 DNS 自动集群

[DNS (Domain Name System)](https://tools.ietf.org/html/rfc1034) 即域名解析系统。一台 DNS 服务器在收到域名查询请求后，会返回这个域名对应的 IP 地址，也就是所谓的 A（Address）记录。DNS 允许一个域名有多项 A 记录，也就是多个 IP 地址，这样就形成了一个名字对应多个 IP 地址的映射。EMQX 的 DNS 自动集群就是利用这样的一对多的映射来找到集群中所有的节点，使各个独立的节点都能加入到集群中。

#### 配置 DNS

大部分的公有云服务中都有 DNS 服务，分配域名后，在管理界面把 EMQX 各个节点的 IP 地址添加到该域名的 A 记录中即可。如果 EMQX 部署在私有云或者内网中，域名也仅在本地网络有效，那么您可能需要使用如 [BIND](https://www.isc.org/bind/) 等软件建立自己的域名服务器。

#### 通过 DNS 记录配置自动集群

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

其中，

- `discovery_strategy` 是节点发现策略，将其设置为 `dns`。
- `cluster.dns.name` 是一个字符串，输入 localhost。
- `cluster.dns.record_type` 是一个枚举值，可选值：`a` 或 `srv`。

配置完成后逐一启动所有节点即可完成集群。

### 基于 etcd 自动集群

[etcd](https://etcd.io/) 是 CoreOS 发起的开源项目，etcd 的应用场景多间于服务发现，解决分布式系统中同一个集群的进程之间如何相互发现并建立连接的问题，这个功能正是 EMQX 自动集群所需要的。

当网络中存在 etcd 服务器（集群）的时候，EMQX 集群可以使用 ectd 的方式自动建立集群。安装和配置 etcd 服务集群请参考 [etcd install](https://etcd.io/docs/latest/install/)。

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

其中：

- `discovery_strategy` 是节点发现策略，设置为 `etcd`。
- `cluster.etcd.server` 是服务器地址，多个节点可以用 `,` 分隔。
- `cluster.etcd.prefix` 是用于 EMQX 服务发现的 etcd 键前缀 。<!--不确定解释是否足够-->
- `cluster.etcd.node_ttl` 是持续时间，表示与节点相关联的 etcd 键的过期时间，默认值：`1m`。

在完成配置以后，您可以逐一启动 EMQX 节点，并用 etcdctl 工具观察 etcd 服务器上的变化：

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@node1.emqx.com
/emqxcl/emqxcl/nodes/emqx@node2.emqx.com
```

以上结果表明所有节点都正常启动并自动加入集群。

### 基于 kubernetes 自动集群

[EMQX Kubernetes Operator](https://docs.emqx.com/zh/emqx-operator/latest/) 可以帮您快速在 Kubernetes 的环境中创建和管理 EMQX 集群，极大简化了 EMQX 集群的部署和管理流程，将部署和管理变为一种低成本、标注化、可复用的工作。

如果你希望自行部署和管理 EMQX，依然可以通过 Kubernetes API 进行节点发现和自动集群。如希望使用此功能，需要先为 EMQX Pod 配置 RBAC，允许 EMQX 通过 endpoints 资源从 Kubernetes APIServer 获取集群节点信息，具体配置步骤，请参考 [使用 RBAC 鉴权](https://kubernetes.io/zh-cn/docs/reference/access-authn-authz/rbac/)。

您需要为所有节点指定 Kubernetes API 服务器，EMQX 在 K8s 上的服务名，地址类型:

<!-- TODO 补充几个参数的作用介绍 -->

```bash
cluster {
    discovery_strategy = K8s
    K8s {
        apiserver = "http://10.110.111.204:8080"
        service_name = emqx
        address_type = ip
        suffix = "pod.local" # 此为可选项
        namespace = default
    }
}
```

其中：

- `discovery_strategy` 是节点发现策略，设置为 `K8s`。
- `cluster.K8s.apiserver` 是 Kubernetes API 端点 URL，默认值：`http://10.110.111.204:8080`。
- `cluster.K8s.service_name` 是 EMQX 服务名称，默认值：`emqx`。
- `cluster.K8s.address_type` 是连接发现节点的地址类型，默认值：`ip`，可选值：`ip`、`dns`、`hostname`。
- [可选] `cluster.K8s.suffix` 是节点名称后缀，仅当 `cluster.K8s.address_type` 设置为 `dns` 时需要，默认值：`pod.local`。
- `cluster.K8s.namespace` 是 Kubernetes 命名空间，它是一个字符串对象，默认值：`default`。

配置完成后逐一启动所有节点即可完成集群。

::: tip
Kubernetes 不建议使用 Fannel 网络插件，推荐使用 Calico 网络插件。
:::

## 管理集群

创建集群之后，您可以管理集群和集群中的节点。

### 查询集群状态

在任何一个集群节点上运行以下命令来查询集群状态：

```bash
$ ./bin/emqx ctl cluster status

集群状态：[{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### 退出集群

退出集群有以下两种方式：

1. 运行 `cluster leave` 命令：让本节点退出集群。它会通知集群中的其他节点，并停止参与集群操作，在离开之前会完成任何正在进行的任务。
2. 运行 `cluster force-leave <node@host>` 命令：在集群内移除节点。目标节点将被强制从集群中移除。当节点出现故障或无响应时，通常使用此命令。

例如，在之前构建的集群中，如果 `emqx@s2.emqx.io` 想要离开集群，您可以在 `emqx@s2.emqx.io` 上运行以下命令：

```bash
./bin/emqx ctl cluster leave
```

或者在 `emqx@s1.emqx.io` 上运行以下命令来从集群中移除 `emqx@s2.emqx.io`：

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### 配置网络协议

在创建集群之后，您可以继续为节点设置网络协议。EMQX 支持通过 TCP 或 TLS 连接节点。连接方法可以在 `emqx.conf` 中配置：

要使用 TCP IPv4 和 TCP IPv6，您可以在 `emqx.conf` 中设置 `cluster.proto_dist`。

- TCP IPv4：`inet_tcp`（默认）
- TCP IPv6：`inet6_tcp`

要启用 SSL，您首先需要将 `cluster.proto_dist` 设置为 `inet_tls`，然后在 `etc` 文件夹中配置 `ssl_dist.conf` 文件并指定 TLS 证书。详情参见 [使用 TLS 进行 Erlang 分布式处理](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html)。
