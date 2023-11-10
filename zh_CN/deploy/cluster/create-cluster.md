# 创建与管理集群

EMQX 支持手动创建集群，也支持通过多种方式自动集群。本页将为您介绍手动和自动集群方式并指导您使用这两种不同的方式创建并管理 EMQX 集群。

## 手动与自动集群简介

节点发现是创建集群的必要过程，它允许单个 EMQX 节点发现对方并互相通信，无论其位置或 IP 地址如何。根据节点发现的方式，我们可以将创建集群的方式分为手动与自动集群。

EMQX 支持基于 [Ekka](https://github.com/emqx/ekka) 库自动创建集群。Ekka 是为 Erlang/OTP 应用开发的集群管理库，实现了 Erlang 节点自动发现 (Service Discovery)、自动集群 (Autocluster)、 网络分区自动愈合 (Network Partition Autoheal)、自动删除宕机节点 (Autoclean) 等功能。

| 方式      | 说明                                                         |
| --------- | ------------------------------------------------------------ |
| manual    | 手动命令创建集群                                             |
| static    | 静态节点列表自动集群                                         |
| multicast | 采用 UDP 组播模式的自动群集<br />注意：5.0 之前版本中的组播模式发现策略已被废弃，在未来的版本中会被删除。 |
| DNS       | DNS A 记录自动集群                                           |
| etcd      | 通过 etcd 自动集群                                           |
| K8s       | Kubernetes 服务自动集群                                      |

EMQX 默认配置为手动创建集群，您可以通过 `emqx.conf` 配置文件配置节点发现策略：

```bash
cluster {
    ## 可选 manual | static | dns | etcd | K8s
    discovery_strategy  =  manual
}
```

## 前置准备

在开始创建集群之前，首先需要了解以下基本概念：

- [分布式集群](./introduction.md)
- [部署架构与集群要求](./mria-introduction.md)

您还需要检查网络环境与节点配置：

- **网络环境配置**：确保节点之间的网络连接正常。如果节点之间存在防火墙或安全组，需要开放[集群内通信端口](./security.md)。
- **节点名设置**：为每个节点设置唯一的节点名，节点名的格式应为 `name@host`，其中 host 必须是其他节点可以访问的 IP 地址或 FQDN（主机名或域名）。
- **节点 Cookie 设置**：所有节点必须使用相同的 Cookie 值，请修改默认的 Cookie 值以确保安全。

::: tip
集群一经创建，节点名是不可变的，因为数据数据库和数据文件都与之相关。即使网络环境提供静态 IP 的情况下，也强烈建议使用静态 FQDN 作为 EMQX 节点名。
:::

## 快速开始

以下是通过 Docker 使用两种不同方式创建集群的示例：

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

::: tab 静态集群示例

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

如需退出集群请参考[退出集群](#退出集群)。

至此，您已经完成了一个简单的集群创建过程，接下来可以按照以下章节的内容选择您需要的集群创建方式进行修改部署。

## 手动创建集群

在手动集群中，您必须手动配置集群中的每个节点，包括设置节点之间的网络连接。

与自动集群相比，手动集群能够精细的配置自定义的网络拓扑结构，在自动集群机制不可用或不适合的情况下，手动集群非常适用。

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
   3. 已加入集群的节点加入另一个集群时，该节点将离开当前集群。

   :::

3. 在任意节点上查询集群的状态：

   ```bash
   $ ./bin/emqx ctl cluster status
   
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

## 基于 static 节点列表自动集群

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

逐一启动所有节点，集群即可建立。

## 基于 DNS 自动集群

### 工作原理

[DNS](https://tools.ietf.org/html/rfc1034) 是 Domain Name System 的缩写，即域名解析系统。一台 DNS 服务器在收到域名查询请求后，会返回这个域名对应的 IP 地址，也就是所谓的 A（Address）记录。DNS 允许一个域名有多项 A 记录，也就是多个 IP 地址，这样就形成了一个名字对应多个 IP 地址的映射。

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

在完成配置以后，您可以逐一启动 EMQX 节点，并用 etcdctl 工具观察 etcd 服务器上的变化：

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@node1.emqx.com
/emqxcl/emqxcl/nodes/emqx@node2.emqx.com
```

以上结果表明所有节点都正常启动并自动加入集群。

## 基于 kubernetes 自动集群

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

配置完成后逐一启动所有节点即可完成集群。

::: tip
Kubernetes 不建议使用 Fannel 网络插件，推荐使用 Calico 网络插件。
:::

## 退出集群

退出集群，有以下两种方式：

1. leave：让本节点退出集群
2. force-leave：在集群内移除节点

在 `emqx@node2.emqx.com` 上执行以下命令，让其退出集群：

```bash
./bin/emqx ctl cluster leave
```

或在 `emqx@node1.emqx.com` 上从集群移除 `emqx@node2.emqx.com` 节点：

```bash
./bin/emqx ctl cluster force-leave emqx@node2.emqx.com
```
