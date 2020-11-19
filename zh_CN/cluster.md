# 分布集群(Cluster)

## Erlang/OTP 分布式编程

Erlang/OTP 最初是爱立信为开发电信设备系统设计的编程语言平台，电信设备(路由器、接入网关、...)典型设计是通过背板连接主控板卡与多块业务板卡的分布式系统。

Erlang/OTP 语言平台的分布式程序，由分布互联的 Erlang 运行系统组成，每个 Erlang 运行系统被称为节点(Node)，节点(Node)间通过 TCP 互联，消息传递的方式通信:

    ---------         ---------
    | Node1 | --------| Node2 |
    ---------         ---------
        |     \     /    |
        |       \ /      |
        |       / \      |
        |     /     \    |
    ---------         ---------
    | Node3 | --------| Node4 |
    ---------         ---------

### 节点(Node)

Erlang 节点由唯一的节点名称标识，节点间通过名称进行通信寻址。 例如在本机启动四个 Erlang 节点，节点名称分别为:

    erl -name node1@127.0.0.1
    erl -name node2@127.0.0.1
    erl -name node3@127.0.0.1
    erl -name node4@127.0.0.1

node1@127.0.0.1控制台下建立与其他节点的连接 :

    (node1@127.0.0.1)1> net_kernel:connect_node('node2@127.0.0.1').
    true
    (node1@127.0.0.1)2> net_kernel:connect_node('node3@127.0.0.1').
    true
    (node1@127.0.0.1)3> net_kernel:connect_node('node4@127.0.0.1').
    true
    (node1@127.0.0.1)4> nodes().
    ['node2@127.0.0.1','node3@127.0.0.1','node4@127.0.0.1']

### epmd

epmd(Erlang Port Mapper Daemon) -
Erlang 端口映射服务程序，在 Erlang 节点运行主机上自启动，负责映射节点名称到通信 TCP 端口号:

    (node1@127.0.0.1)6> net_adm:names().
    {ok,[{"node1",62740},
         {"node2",62746},
         {"node3",62877},
         {"node4",62895}]}

### 安全

Erlang 节点间通过一个相同的 cookie 进行互连认证。

Erlang 节点 Cookie 设置:

    1. $HOME/.erlang.cookie文件

    2. erl -setcookie <Cookie>

本节内容来自: [ http://erlang.org/doc/reference_manual/distributed.html
](http://erlang.org/doc/reference_manual/distributed.html)

## emqttd 分布集群设计

emqttd 消息服务器集群基于 Erlang/OTP 分布式设计，集群原理可简述为下述两条规则:

1. MQTT 客户端订阅主题时，所在节点订阅成功后广播通知其他节点：某个主题(Topic)被本节点订阅。
2. MQTT 客户端发布消息时，所在节点会根据消息主题(Topic)，检索订阅并路由消息到相关节点。

emqttd 消息服务器同一集群的所有节点，都会复制一份主题(Topic) -> 节点(Node)映射的路由表，例如:

    topic1 -> node1, node2
    topic2 -> node3
    topic3 -> node2, node4

### 主题树(Topic Trie)与路由表(Route Table)

emqttd 消息服务器每个集群节点，都保存一份主题树(Topic Trie)和路由表。

例如下述主题订阅关系:

| 客户端  | 节点  | 订阅主题     |
| ------- | ----- | ------------ |
| client1 | node1 | t/+/x, t/+/y |
| client2 | node2 | t/#          |
| client3 | node3 | t/+/x, t/a   |

最终会生成如下主题树(Topic Trie)和路由表(Route Table):

    --------------------------
    |             t          |
    |            / \         |
    |           +   #        |
    |         /  \           |
    |       x      y         |
    --------------------------
    | t/+/x -> node1, node3  |
    | t/+/y -> node1         |
    | t/#   -> node2         |
    | t/a   -> node3         |
    --------------------------

### 订阅(Subscription)与消息派发

客户端的主题订阅(Subscription)关系，只保存在客户端所在节点，用于本节点内派发消息到客户端。

例如 client1 向主题't/a'发布消息，消息在节点间的路由与派发流程:

    title: Message Route and Deliver

    client1->node1: Publish[t/a]
    node1-->node2: Route[t/#]
    node1-->node3: Route[t/a]
    node2-->client2: Deliver[t/#]
    node3-->client3: Deliver[t/a]

![image](./_static/images/route.png)

## emqttd 集群设置管理

假设部署两台服务器 s1.emqtt.io, s2.emqtt.io 上部署集群:

| 节点名                                    | 主机名(FQDN) | IP 地址      |
| ----------------------------------------- | ------------ | ------------ |
| emqttd@s1.emqtt.io 或 emqttd@192.168.0.10 | s1.emqtt.io  | 192.168.0.10 |
| emqttd@s2.emqtt.io 或 emqttd@192.168.0.20 | s2.emqtt.io  | 192.168.0.20 |

::: warning
节点名格式: Name@Host , Host 必须是 IP 地址或 FQDN(主机名.域名)
:::

### emqttd@s1.emqtt.io节点设置

emqttd/etc/vm.args:

    -name emqttd@s1.emqtt.io

    或

    -name emqttd@192.168.0.10

::: warning
节点启动加入集群后，节点名称不能变更。
:::

### emqttd@s2.emqtt.io节点设置

emqttd/etc/vm.args:

    -name emqttd@s2.emqtt.io

    或

    -name emqttd@192.168.0.20

### 节点加入集群

启动两台节点后，emqttd@s2.emqtt.io上执行:

    $ ./bin/emqttd_ctl cluster join emqttd@s1.emqtt.io

    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqttd@s1.emqtt.io','emqttd@s2.emqtt.io']}]

或，emqttd@s1.emqtt.io上执行:

    $ ./bin/emqttd_ctl cluster join emqttd@s2.emqtt.io

    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqttd@s1.emqtt.io','emqttd@s2.emqtt.io']}]

任意节点上查询集群状态:

    $ ./bin/emqttd_ctl cluster status

    Cluster status: [{running_nodes,['emqttd@s1.emqtt.io','emqttd@s2.emqtt.io']}]

### 节点退出集群

节点退出集群，两种方式:

1. leave: 本节点退出集群
2. remove: 从集群删除其他节点

emqttd@s2.emqtt.io主动退出集群 :

    $ ./bin/emqttd_ctl cluster leave

或emqttd@s1.emqtt.io节点上 ，从集群删除emqttd@s2.emqtt.io节点:

    $ ./bin/emqttd_ctl cluster remove emqttd@s2.emqtt.io

## 跨节点会话(Session)

emqttd 消息服务器集群模式下，MQTT 连接的持久会话(Session)跨节点。

例如负载均衡的两台集群节点:node1 与 node2，同一 MQTT 客户端先连接 node1，node1 节点会创建持久会话；客户端断线重连到 node2 时，MQTT 的连接在 node2 节点，持久会话仍在 node1 节点:

                                      node1
                                   -----------
                               |-->| session |
                               |   -----------
                 node2         |
              --------------   |
     client-->| connection |<--|
              --------------

## 防火墙设置

如果集群节点间存在防火墙，防火墙需要开启 4369 端口和一个 TCP 端口段。4369 由 epmd 端口映射服务使用，TCP 端口段用于节点间建立连接与通信。

防火墙设置后，emqttd 需要配置相同的端口段，etc/emqttd.config 文件:

    [{kernel, [
        ...
        {inet_dist_listen_min, 20000},
        {inet_dist_listen_max, 21000}
     ]},
     ...

## 注意事项: NetSplit

emqttd 消息服务器集群需要稳定网络连接以避免发生 NetSplit 故障。集群设计上默认不自动处理 NetSplit，如集群节点间发生 NetSplit，需手工重启某个分片上的相关节点。

::: tip
NetSplit 是指节点运行正常但因网络断开互相认为对方宕机。
:::
## 一致性 Hash 与 DHT

NoSQL 数据库领域分布式设计，大多会采用一致性 Hash 或 DHT。emqttd 消息服务器集群架构可支持千万级的路由，更大级别的集群可采用一致性 Hash、DHT 或 Shard 方式切分路由表。
