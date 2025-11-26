# 分布式集群

## Erlang/OTP 分布式编程

Erlang/OTP 最初是爱立信为开发电信设备系统设计的编程语言平台，电信设备(路由器、接入网关等)典型设计是通过背板连接主控板卡与多块业务板卡的分布式系统。

Erlang/OTP 语言平台的分布式程序，由分布互联的 Erlang 运行系统组成，每个 Erlang 运行系统被称为节点(Node)，节点(Node) 间通过 TCP 互联，消息传递的方式通信:

![](../assets/cluster_1.png)

### 节点(Node)

Erlang 节点由唯一的节点名称标识，节点间通过名称进行通信寻址。 例如在本机启动四个 Erlang 节点，节点名称分别为:

```bash
erl -name node1@127.0.0.1
erl -name node2@127.0.0.1
erl -name node3@127.0.0.1
erl -name node4@127.0.0.1
```
node1@127.0.0.1 控制台下建立与其他节点的连接:

```bash
(node1@127.0.0.1)1> net_kernel:connect_node('node2@127.0.0.1').
true
(node1@127.0.0.1)2> net_kernel:connect_node('node3@127.0.0.1').
true
(node1@127.0.0.1)3> net_kernel:connect_node('node4@127.0.0.1').
true
(node1@127.0.0.1)4> nodes().
['node2@127.0.0.1','node3@127.0.0.1','node4@127.0.0.1']
```

## EMQX 分布集群设计

EMQX 消息服务器集群基于 Erlang/OTP 分布式设计，集群原理可简述为下述两条规则:

MQTT 客户端订阅主题时，所在节点订阅成功后广播通知其他节点：某个主题(Topic)被本节点订阅。

MQTT 客户端发布消息时，所在节点会根据消息主题(Topic)，检索订阅并路由消息到相关节点。

EMQX 消息服务器同一集群的所有节点，都会复制一份主题(Topic) -> 节点(Node)映射的路由表，例如:

```bash
topic1 -> node1, node2
topic2 -> node3
topic3 -> node2, node4
```

### 主题树(Topic Trie)与路由表(Route Table)

EMQX 消息服务器每个集群节点，都保存一份主题树(Topic Trie)和路由表。

例如下述主题订阅关系:

| 客户端  | 节点  | 订阅主题     |
| ------- | ----- | ------------ |
| client1 | node1 | t/+/x, t/+/y |
| client2 | node2 | t/#          |
| client3 | node3 | t/+/x, t/a   |



最终会生成如下主题树(Topic Trie)和路由表(Route Table):

![](../assets/cluster_2.png)



### 订阅(Subscription)与消息派发

客户端的主题订阅(Subscription)关系，只保存在客户端所在节点，用于本节点内派发消息到客户端。

例如client1向主题’t/a’发布消息，消息在节点间的路由与派发流程:

```bash
title: Message Route and Deliver

client1 -> node1: Publish[t/a]
    node1 --> node2: Route[t/#]
        node2 --> client2: Deliver[t/#]
    node1 --> node3: Route[t/a]
        node3 --> client3: Deliver[t/a]
```

![_images/design_9.png](../assets/design_9.png)



## 节点发现与自动集群
EMQX 支持基于 Ekka 库的集群自动发现 (Autocluster)。Ekka 是为 Erlang/OTP 应用开发的集群管理库，支持
Erlang 节点自动发现 (Service Discovery)、自动集群 (Autocluster)、网络分区自动愈合 (Network Partition
Autoheal)、自动删除宕机节点 (Autoclean)。

EMQX 支持多种节点发现策略:

| 策略     | 说明                |
| ------ | ----------------- |
| manual | 手动命令创建集群         |
| static | 静态节点列表自动集群     |
| mcast  | UDP 组播方式自动集群     |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| k8s    | Kubernetes 服务自动集群 |

### 手动(manual) 方式管理集群介绍
假设要在两台服务器 s1.emqx.io, s2.emqx.io 上部署 EMQX 集群:

|                节点名                 | 主机名 (FQDN)  |   IP 地址    |
| ------------------------------------ | ------------- | ------------ |
| emqx@s1.emqx.io 或 emqx@192.168.0.10 | s1.emqx.io    | 192.168.0.10 |
| emqx@s2.emqx.io 或 emqx@192.168.0.20 | s2.emqx.io    | 192.168.0.20 |

**注意：** 节点名格式为 <Name@Host>, Host 必须是 IP 地址或 FQDN。
其中 FQDN 格式为：`主机名.域名`，不能是单层的 `主机名`。例如，`abc` 是不合法的，而 `abc.local` 是合法的。

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

#### 单机伪分布式

对于只有个人电脑或者一台服务器的用户来说，可以使用伪分布式集群。请注意，我们若要在单机上启动两个或多个 emqx 实例，为避免端口冲突，我们需要对其它节点的监听端口做出调整。

基本思路是复制一份 emqx 文件夹然后命名为 emqx2 ，将原先所有 emqx 节点监听的端口 port 加上一个偏移 offset 作为新的 emqx2 节点的监听端口。例如，将原先 emqx 的MQTT/TCP 监听端口由默认的 1883 改为了 2883 作为 emqx2 的 MQTT/TCP 监听端口。完成以上操作的自动化脚本可以参照 [集群脚本](https://github.com/terry-xiaoyu/one_more_emqx)，具体配置请参见 [配置说明](../getting-started/config.md) 与 [配置项](../configuration/configuration.md)。

## 防火墙设置

### 集群节点发现端口

若预先设置了环境变量 WITH_EPMD=1, 启动 emqx 时会使用启动 epmd (监听端口 4369) 做节点发现。称为 `epmd 模式`。

若环境变量 WITH_EPMD 没有设置，则启动 emqx 时不启用 epmd，而使用 emqx ekka 的节点发现，这也是 4.0 之后的默认节点发现方式。称为 `ekka 模式`。

**epmd 模式：**

如果集群节点间存在防火墙，防火墙需要为每个节点开通 TCP 4369 端口，用来让各节点能互相访问。

防火墙还需要开通一个 TCP 从 `node.dist_listen_min`(包含) 到 `node.dist_listen_max`(包含) 的端口段，
这两个配置的默认值都是 `6369`。

**ekka 模式（4.0 版本之后的默认模式）：**

跟`empd 模式`不同，在`ekka 模式`下，集群发现端口的映射关系是约定好的，而不是动态的。
`node.dist_listen_min` and `node.dist_listen_max` 两个配置在`ekka 模式`下不起作用。

如果集群节点间存在防火墙，防火墙需要放开这个约定的端口。约定端口的规则如下：

```
ListeningPort = BasePort + Offset
```

其中 `BasePort` 为 4370 (不可配置), `Offset` 为节点名的数字后缀. 如果节点名没有数字后缀的话，
`Offsset` 为 0。

举例来说, 如果 `emqx.conf` 里配置了节点名：`node.name = emqx@192.168.0.12`，那么监听端口为 `4370`，
但对于 `emqx1` (或者 `emqx-1`) 端口就是 `4371`，以此类推。

### The Cluster RPC Port

每个节点还需要监听一个 RPC 端口，也需要被防火墙也放开。跟上面说的`ekka 模式`下的集群发现端口一样，这个 RPC 端口也是约定式的。

RPC 端口的规则跟`ekka 模式`下的集群发现端口类似，只不过 `BasePort = 5370`。

就是说，如果 `emqx.conf` 里配置了节点名：`node.name = emqx@192.168.0.12`，那么监听端口为 `5370`，
但对于 `emqx1` (或者 `emqx-1`) 端口就是 `5371`，以此类推。
