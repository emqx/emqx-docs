
====================
分布式集群(Cluster)
====================

----------------------------
Erlang/OPT语言平台分布式编程
----------------------------

Erlang/OTP最初是爱立信为开发电信设备系统设计的编程语言平台，电信设备(路由器、接入网关、...)典型设计是通过背板连接主控板卡与多块业务板卡的分布式系统。

Erlang/OPT语言平台的分布式程序，由分布互联的Erlang运行系统组成，每个Erlang运行系统被称为节点(Node)，节点(Node)间通过TCP互联，消息传递的方式通信::

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

Erlang节点由唯一的节点名称标识，节点间通过该名称进行通信寻址。 例如在本机启动四个Erlang节点，节点名称分别为:

.. code:: shell

    erl -name node1@127.0.0.1
    erl -name node2@127.0.0.1
    erl -name node3@127.0.0.1
    erl -name node4@127.0.0.1

node1@127.0.0.1控制台下建立与其他节点的连接::

    (node1@127.0.0.1)1> net_kernel:connect_node('node2@127.0.0.1').
    true
    (node1@127.0.0.1)2> net_kernel:connect_node('node3@127.0.0.1').
    true
    (node1@127.0.0.1)3> net_kernel:connect_node('node4@127.0.0.1').
    true
    (node1@127.0.0.1)4> nodes().
    ['node2@127.0.0.1','node3@127.0.0.1','node4@127.0.0.1']


epmd
----------

epmd(Erlang Port Mapper Daemon) - Erlang端口映射服务程序，在Erlang节点运行主机上自启动，负责映射节点名称到通信TCP端口号::

    (node1@127.0.0.1)6> net_adm:names().
    {ok,[{"node1",62740},
         {"node2",62746},
         {"node3",62877},
         {"node4",62895}]}

安全
----------

Erlang节点间通过一个相同的cookie进行相互连接认证。

Erlang节点Cookie设置::

    1. $HOME/.erlang.cookie文件

    2. erl -setcookie <Cookie>

本节内容来自: http://erlang.org/doc/reference_manual/distributed.html


----------------------------
emqttd消息服务器分布集群设计
----------------------------

emqttd消息服务器集群是基于Erlang/OTP分布式设计: 同一集群不同节点的MQTT连接客户端，可以相互发布订阅消息路由。

集群原理可简述为下述两条规则:

1. MQTT客户端订阅主题时，所在节点处理成功会广播通知其他节点：某个主题(Topic)被本节点订阅。

2. MQTT客户端发布消息时，所在节点会根据消息主题(Topic)，检索订阅并路由消息到相关节点。


emqttd消息服务器同一集群的所有节点，都会复制一份主题(Topic) -> 节点(Node)映射的路由表，例如::

    topic1 -> node1, node2
    topic2 -> node3
    topic3 -> node2, node4

主题树(Topic Trie)与路由表
--------------------------

emqttd消息服务器每个集群节点，都保存一份主题树(Topic Trie)和路由表，例如下述订阅关系:

+----------------+----------------+------------------------------
|    客户端       |  节点          |  订阅主题                  |
+----------------+----------------+------------------------------
|   |  |  |
+----------------+----------------+------------------------------
|   |  |  |
+----------------+----------------+------------------------------
|   |  |  |
+----------------+----------------+------------------------------
|   |  |  |
+----------------+----------------+------------------------------

最终会生成如下主题树(Topic Trie)和路由表，并复制到全部节点::




2. Topic trie tree will be copied to every clusterd node.


订阅(Subscription)与消息派发
----------------------------

## Cluster Design

   3. Subscribers to topic will be stored in each node and will not be copied.

   ## Cluster Strategy

   TODO:...

   1. A message only gets forwarded to other cluster nodes if a cluster node is interested in it. this reduces the network traffic tremendously, because it prevents nodes from forwarding unnecessary messages.

   2. As soon as a client on a node subscribes to a topic it becomes known within the cluster. If one of the clients somewhere in the cluster is publishing to this topic, the message will be delivered to its subscriber no matter to which cluster node it is connected.

   ....
   TODO: 时序图


----------------------------
## Cluster Architecture

![Cluster Design](http://emqtt.io/static/img/Cluster.png)
## Cluster Command

```sh
./bin/emqttd_ctl cluster DiscNode
```

## Mnesia Example

```
(emqttd3@127.0.0.1)3> mnesia:info().
---> Processes holding locks <---
---> Processes waiting for locks <---
---> Participant transactions <---
---> Coordinator transactions <---
---> Uncertain transactions <---
---> Active tables <---
mqtt_retained : with 6 records occupying 221 words of mem
topic_subscriber: with 0 records occupying 305 words of mem
topic_trie_node: with 129 records occupying 3195 words of mem
topic_trie : with 128 records occupying 3986 words of mem
topic : with 93 records occupying 1797 words of mem
schema : with 6 records occupying 1081 words of mem
===> System info in version "4.12.4", debug level = none <===
opt_disc. Directory "/Users/erylee/Projects/emqttd/rel/emqttd3/data/mnesia" is NOT used.
use fallback at restart = false
running db nodes = ['emqttd2@127.0.0.1','emqttd@127.0.0.1','emqttd3@127.0.0.1']
stopped db nodes = []
master node tables = []
remote = []
ram_copies = [mqtt_retained,schema,topic,topic_subscriber,topic_trie,
topic_trie_node]
disc_copies = []
disc_only_copies = []
[{'emqttd2@127.0.0.1',ram_copies},
{'emqttd3@127.0.0.1',ram_copies},
{'emqttd@127.0.0.1',disc_copies}] = [schema]
[{'emqttd2@127.0.0.1',ram_copies},
{'emqttd3@127.0.0.1',ram_copies},
{'emqttd@127.0.0.1',ram_copies}] = [topic,topic_trie,topic_trie_node,
mqtt_retained]
[{'emqttd3@127.0.0.1',ram_copies}] = [topic_subscriber]
44 transactions committed, 5 aborted, 0 restarted, 0 logged to disc
   0 held locks, 0 in queue; 0 local transactions, 0 remote
   0 transactions waits for other nodes: []
   ```

   ## Cluster vs Bridge

   Cluster will copy topic trie tree between nodes, Bridge will not.


--------------
集群配置与管理
--------------

## Overview

Suppose we cluster two nodes on hosts:

Node | Host   | IpAddress  
-----|--------|-------------
node1(disc_copy)| host1 | 192.168.0.10
node2(ram_copy) | host2  | 192.168.0.20

## Configure and start 'node1'

configure 'etc/vm.args':

```
-name emqttd@192.168.0.10
```

or
```
-name emqttd@host1
```

If host1, host2 added to /etc/hosts of OS.

Then start node1:

```sh
./bin/emqttd start
```

**Notice that 'data/mnesia/*' should be removed before you start the broker with different node name.**

## Configure and start 'node2'

Configure 'etc/vm.args':

```
-name emqttd@192.168.0.20
```

or
```
-name emqttd@host2
```

Then start node2:

```sh
./bin/emqttd start
```

## Cluster two nodes

Run './bin/emqttd_ctl cluster' on host2:

```sh
./bin/emqttd_ctl cluster emqttd@192.168.0.10
```

## Check cluster status

And then check clustered status on any host:

```sh
./bin/emqttd_ctl cluster
```


---------
集群实践
---------

平行模式

前后端模式


------------------------
注意事项: NetSplit
------------------------


------------------------
一致性Hash与DHT
------------------------


