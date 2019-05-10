
.. _clustering:

=====================
分布集群 (Clustering)
=====================

---------------------
Erlang/OTP 分布式编程
---------------------

Erlang/OTP 最初是爱立信为开发电信设备系统设计的编程语言平台，电信设备(路由器、接入网关、...)典型设计是通过背板连接主控板卡与多块业务板卡的分布式系统。

Erlang/OTP 语言平台的分布式程序，由分布互联的 Erlang 运行系统组成，每个 Erlang 运行系统被称为节点(Node)，节点(Node) 间通过 TCP 互联，消息传递的方式通信::

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


节点(Node)
----------

Erlang 节点由唯一的节点名称标识，节点间通过名称进行通信寻址。 例如在本机启动四个 Erlang 节点，节点名称分别为:

.. code:: shell

    erl -name node1@127.0.0.1
    erl -name node2@127.0.0.1
    erl -name node3@127.0.0.1
    erl -name node4@127.0.0.1

node1@127.0.0.1 控制台下建立与其他节点的连接::

    (node1@127.0.0.1)1> net_kernel:connect_node('node2@127.0.0.1').
    true
    (node1@127.0.0.1)2> net_kernel:connect_node('node3@127.0.0.1').
    true
    (node1@127.0.0.1)3> net_kernel:connect_node('node4@127.0.0.1').
    true
    (node1@127.0.0.1)4> nodes().
    ['node2@127.0.0.1','node3@127.0.0.1','node4@127.0.0.1']

epmd
----

epmd(Erlang Port Mapper Daemon) - Erlang 端口映射服务程序，在 Erlang 节点运行主机上自启动，负责映射节点名称到通信 TCP 端口号::

    (node1@127.0.0.1)6> net_adm:names().
    {ok,[{"node1",62740},
         {"node2",62746},
         {"node3",62877},
         {"node4",62895}]}

安全
----

Erlang 节点间通过一个相同的 cookie 进行互连认证。

Erlang 节点 Cookie 设置::

    1. $HOME/.erlang.cookie 文件

    2. erl -setcookie <Cookie>

本节内容来自: http://erlang.org/doc/reference_manual/distributed.html

连接
----

Erlang 集群节点可通过 TCPv4, TCPv6 或 TLS 方式连接，EMQ X 支持在 ``etc/emqx.conf`` 中配置连接方式:

.. code-block:: properties

    ## Specify the erlang distributed protocol.
    ##
    ## Value: Enum
    ##  - inet_tcp: the default; handles TCP streams with IPv4 addressing.
    ##  - inet6_tcp: handles TCP with IPv6 addressing.
    ##  - inet_tls: using TLS for Erlang Distribution.
    ##
    ## vm.args: -proto_dist inet_tcp
    node.proto_dist = inet_tcp

    ## Specify SSL Options in the file if using SSL for Erlang Distribution.
    ##
    ## Value: File
    ##
    ## vm.args: -ssl_dist_optfile <File>
    ## node.ssl_dist_optfile = {{ platform_etc_dir }}/ssl_dist.conf

.. _cluster_emqx:

-------------------
EMQ X 分布集群设计
-------------------

*EMQ X* 消息服务器集群基于 Erlang/OTP 分布式设计，集群原理可简述为下述两条规则:

1. MQTT 客户端订阅主题时，所在节点订阅成功后广播通知其他节点：某个主题(Topic)被本节点订阅。

2. MQTT 客户端发布消息时，所在节点会根据消息主题(Topic)，检索订阅并路由消息到相关节点。

EMQ X 消息服务器同一集群的所有节点，都会复制一份主题(Topic) -> 节点(Node)映射的路由表，例如::

    topic1 -> node1, node2
    topic2 -> node3
    topic3 -> node2, node4

主题树(Topic Trie)与路由表(Route Table)
---------------------------------------

EMQ X 消息服务器每个集群节点，都保存一份主题树(Topic Trie)和路由表。

例如下述主题订阅关系:

+----------------+-------------+----------------------------+
| 客户端         | 节点        |  订阅主题                  |
+----------------+-------------+----------------------------+
| client1        | node1       | t/+/x, t/+/y               |
+----------------+-------------+----------------------------+
| client2        | node2       | t/#                        |
+----------------+-------------+----------------------------+
| client3        | node3       | t/+/x, t/a                 |
+----------------+-------------+----------------------------+

最终会生成如下主题树(Topic Trie)和路由表(Route Table)::

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

订阅(Subscription)与消息派发
----------------------------

客户端的主题订阅(Subscription)关系，只保存在客户端所在节点，用于本节点内派发消息到客户端。

例如client1向主题't/a'发布消息，消息在节点间的路由与派发流程::

    title: Message Route and Deliver

    client1 -> node1: Publish[t/a]
        node1 --> node2: Route[t/#]
            node2 --> client2: Deliver[t/#]
        node1 --> node3: Route[t/a]
            node3 --> client3: Deliver[t/a]

.. image:: ./_static/images/route.png

-----------------
手动配置管理集群
-----------------

假设部署两台服务器 s1.emqx.io, s2.emqx.io 上部署集群:

+----------------------+-----------------+---------------------+
| 节点名               | 主机名(FQDN)    |    IP 地址          |
+----------------------+-----------------+---------------------+
| emqx@s1.emqx.io 或   | s1.emqx.io      | 192.168.0.10        |
| emqx@192.168.0.10    |                 |                     |
+----------------------+-----------------+---------------------+
| emqx@s2.emqx.io 或   | s2.emqx.io      | 192.168.0.20        |
| emqx@192.168.0.20    |                 |                     |
+----------------------+-----------------+---------------------+

.. WARNING:: 节点名格式: Name@Host, Host 必须是 IP 地址或 FQDN(主机名.域名)

emqx@s1.emqx.io 节点设置
-------------------------

emqx/etc/emqx.conf::

    node.name = emqx@s1.emqx.io

    或

    node.name = emqx@192.168.0.10

也可通过环境变量::

    export EMQX_NODE_NAME=emqx@s1.emqx.io && ./bin/emqx start

.. WARNING:: 节点启动加入集群后，节点名称不能变更。

emqx@s2.emqx.io 节点设置
------------------------

emqx/etc/emqx.conf::

    node.name = emqx@s2.emqx.io

    或

    node.name = emqx@192.168.0.20

节点加入集群
------------

启动两台节点后，emqx@s2.emqx.io 上执行::

    $ ./bin/emqx_ctl cluster join emqx@s1.emqx.io

    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]

或，emq@s1.emqx.io 上执行::

    $ ./bin/emqx_ctl cluster join emqx@s2.emqx.io

    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]

任意节点上查询集群状态::

    $ ./bin/emqx_ctl cluster status

    Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]

节点退出集群
------------

节点退出集群，两种方式:

1. leave: 本节点退出集群

2. force-leave: 从集群删除其他节点

emqx@s2.emqx.io 主动退出集群::

    $ ./bin/emqx_ctl cluster leave

或 emqx@s1.emqx.io 节点上，从集群删除 emqx@s2.emqx.io 节点::

    $ ./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io

.. _autodiscovery:

------------------
节点发现与自动集群
------------------

EMQ X 支持基于 Ekka 库的集群自动发现(Autocluster)。Ekka 是为 Erlang/OTP 应用开发的集群管理库，支持 Erlang 节点自动发现(Discovery)、自动集群(Autocluster)、脑裂自动愈合(Network Partition Autoheal)、自动删除宕机节点(Autoclean)。

EMQ X 支持多种策略创建集群:

+-----------------+---------------------------+
| 策略            | 说明                      |
+=================+===========================+
| manual          | 手动命令创建集群          |
+-----------------+---------------------------+
| static          | 静态节点列表自动集群      |
+-----------------+---------------------------+
| mcast           | UDP 组播方式自动集群      |
+-----------------+---------------------------+
| dns             | DNS A 记录自动集群        |
+-----------------+---------------------------+
| etcd            | 通过 etcd 自动集群        |
+-----------------+---------------------------+
| k8s             | Kubernetes 服务自动集群   |
+-----------------+---------------------------+

manual 手动创建集群
-------------------

默认配置为手动创建集群，节点通过 `./bin/emqx_ctl join <Node>` 命令加入:

.. code-block:: properties

    cluster.discovery = manual

基于 static 节点列表自动集群
----------------------------

配置固定的节点列表，自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = static

    cluster.static.seeds = emq1@127.0.0.1,ekka2@127.0.0.1

基于 mcast 组播自动集群
-----------------------

基于 UDP 组播自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = mcast

    cluster.mcast.addr = 239.192.0.1

    cluster.mcast.ports = 4369,4370

    cluster.mcast.iface = 0.0.0.0

    cluster.mcast.ttl = 255

    cluster.mcast.loop = on

基于 DNS A 记录自动集群
-----------------------

基于 DNS A 记录自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = dns

    cluster.dns.name = localhost

    cluster.dns.app  = ekka

基于 etcd 自动集群
------------------

基于 `etcd`_ 自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = etcd

    cluster.etcd.server = http://127.0.0.1:2379

    cluster.etcd.prefix = emqcl

    cluster.etcd.node_ttl = 1m

基于 Kubernetes 自动集群
------------------------

`Kubernetes`_ 下自动发现并创建集群:

.. code-block:: properties

    cluster.discovery = k8s

    cluster.k8s.apiserver = http://10.110.111.204:8080

    cluster.k8s.service_name = ekka

    ## Address Type: ip | dns
    cluster.k8s.address_type = ip

    ## The Erlang application name
    cluster.k8s.app_name = ekka

.. _cluster_netsplit:

------------------
集群脑裂与自动愈合
------------------

*EMQ X* 支持集群脑裂自动恢复(Network Partition Autoheal):

.. code-block:: properties

    cluster.autoheal = on

集群脑裂自动恢复流程:

1. 节点收到 Mnesia 的 ``inconsistent_database`` 事件3秒后进行集群脑裂确认；

2. 节点确认集群脑裂发生后，向 Leader 节点(集群中最早启动节点)上报脑裂消息；

3. Leader 节点延迟一段时间后，在全部节点在线状态下创建脑裂视图(SplitView)；

4. Leader 节点在多数派(majority)分区选择集群自愈的 Coordinator 节点；

5. Coordinator 节点重启少数派(minority)分区节点恢复集群。

----------------
集群节点自动清除
----------------

*EMQ X* 支持从集群自动删除宕机节点(Autoclean):

.. code-block:: properties

    cluster.autoclean = 5m

.. _cluster_session:

-------------------
跨节点会话(Session)
-------------------

*EMQ X* 集群模式下，MQTT 连接的持久会话(Session)跨节点。

例如负载均衡的两台集群节点: node1 与 node2，同一 MQTT 客户端先连接 node1，node1 节点会创建持久会话；客户端断线重连到 node2 时，MQTT 的连接在 node2 节点，持久会话仍在 node1 节点::

                                      node1
                                   -----------
                               |-->| session |
                               |   -----------
                 node2         |
              --------------   |
     client-->| connection |<--|
              --------------

.. _cluster_firewall:

----------
防火墙设置
----------

如果集群节点间存在防火墙，防火墙需要开启 4369 端口和一个 TCP 端口段。4369 由 epmd 端口映射服务使用，TCP 端口段用于节点间建立连接与通信。

防火墙设置后，EMQ X 需要配置相同的端口段，emqx/etc/emqx.conf 文件::

    ## Distributed node port range
    node.dist_listen_min = 6369
    node.dist_listen_max = 7369

.. _cluster_hash:

..
 ------------------
 一致性 Hash 与 DHT
 ------------------
..
 NoSQL 数据库领域分布式设计，大多会采用一致性 Hash 或 DHT。EMQ X 消息服务器集群架构可支持千万级的路由，更大级别的集群可采用一致性 Hash、DHT 或 Shard 方式切分路由表。

.. _etcd:        https://coreos.com/etcd/
.. _Kubernetes:  https://kubernetes.io/

