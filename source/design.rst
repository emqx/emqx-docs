
======================
架构设计(Design Guide)
======================

----
前言
----

emqttd消息服务器经过两年开发，开发方式像摇滚乐专辑的制作。最初由一些即兴创作的部分(单曲)组成，但最终整体上体现了某种程度的正交(Orthogonality)和一致性(Consistency)。

emqttd消息服务器1.0版本在某些细节上仍是粗糙的，但在架构上做出了正确的选择和设计，如果现在重新设计一遍整个系统，它最终很可能还是目前的样子。

emqttd1.0版本的设计选择带来的一个好的结果是：emqttd可能是开源领域唯一一个，几乎不需要用户做太多努力，就可以支持到100万连接的MQTT服务器。坏的结果是：我们无法向用户提供百万连接优化的商业服务。

100万连接
---------

多核心服务器和现代操作系统内核层面，可以很轻松支持到100万TCP连接，核心问题是应用层面如何处理业务瓶颈。

emqttd消息服务器在业务和应用层面，解决了承载100万连接的各类瓶颈问题。

连接测试需设置操作系统内核、TCP协议栈、Erlang虚拟机参数: http://docs.emqtt.cn/zh_CN/latest/tune.html

全异步架构
----------

emqttd消息服务器是基于Erlang/OTP平台的全异步的架构：异步TCP连接处理、异步主题(Topic)订阅、异步消息发布，只有在资源负载限制部分采用同步设计，比如TCP连接创建和Mnesia数据库事务执行。

一条MQTT消息从发布者(Publisher)到订阅者(Subscriber)，在emqttd消息服务器内部异步流过一系列Erlang进程的Mailbox::

                      --------------          ---------------          --------------
    Publisher --Msg-->| Pub Client | --Msg--> | Sub Session | --Msg--> | Sub Client | --Msg--> Subscriber
                      --------------          ---------------          --------------

消息持久化
----------

emqttd1.0版本没有实现服务器内部的消息持久化，这是一个架构设计选择。首先，emqttd解决的核心问题是连接与路由；其次，我们认为内置持久化会是个错误设计。

传统内置消息持久化的MQ服务器，比如广泛使用的JMS服务器ActiveMQ，几乎每个大版本都在重新设计持久化的部分。内置消息持久化设计上有两个问题:

1. 如何平衡内存与磁盘使用？消息路由基于内存，消息存储又是基于磁盘的。

2. 多节点分布式架构下，如何放置Queue？如何复制Queue？

Kafka在上述问题上，做出了正确的设计。它本身是一个完全基于磁盘的分布式commit log，但可以作为消息服务器使用:)

emqttd2.0版本会将会通过对接外部存储，例如Redis、Kafka、Cassandra、PostgreSQL，实现多种方式的消息持久化。设计上分离消息路由与消息存储职责后，数据复制或容灾备份甚至应用集成，都可以在数据层面实现。

NetSplit问题
------------

emqttd消息服务器集群，基于Mnesia数据库设计。NetSplit发生时，节点间状态是：Erlang节点间可以连通，互相询问自己是否宕机，对方回答你已经宕机:(

NetSplit故障发生时，emqttd消息服务器的log/emqttd_error.log日志，会打印critical级别日志::

    Mnesia inconsistent_database event: running_partitioned_network, emqttd@host

emqttd集群部署在同一IDC网络下，NetSplit发生的几率很低，一旦发生又很难自动化处理。所以emqttd1.0版本设计选择是，集群不会自动化处理NetSplit，需要人工重启部分节点。

应用场景测试
------------

MQTT是一个设计得非常出色的传输层协议，在移动消息、物联网、车联网、智能硬件甚至能源勘探等领域有着广泛的应用。 例如1个字节报头、2个字节心跳、消息QoS支持等设计，非常适合在低带宽、不可靠网络、嵌入式设备上应用。但不同的应用场景有不同的系统要求，用户使用emqttd消息服务器前，可以按自己的应用场景进行测试。

消息推送: 广播问题

即时消息: 收发确认

智能硬件: latency、

物联网数据采集: 吞吐


----------------------
系统架构(Architecture)
----------------------

概念模型
--------

emqttd消息服务器概念上更像一台网络路由器(Router)或交换机(Switch)，而不是传统的企业级消息服务器((MQ)。相比网络路由器按IP地址或MPLS标签路由报文，emqttd按MQTT的主题树(Topic Trie)发布订阅模式路由消息:

.. image:: _static/images/concept.png

设计原则
--------

1. emqttd消息服务器核心解决的问题：处理海量的并发MQTT连接与消息路由。
2. 充分利用Erlang/OTP平台软实时、低延时、高并发、分布容错的优势。
3. 连接(Connection)、会话(Session)、路由(Router)、集群(Cluster)分层。
4. 分离消息路由平面(Flow Plane)与控制管理平面(Control Plane)。
5. 通过后端数据库或NoSQL实现数据持久化、分布与容灾备份。

系统分层
--------

1. 连接层(Connection Layer)
   
   (Socket, Client, Protocol)

2. 会话层(Session Layer)
   
3. 路由层(Route Layer)
   
   Router, PubSub)

4. 分布层(Distributed Layer)
   
   (Topic Trie树, Route Table表)

5. 认证与访问控制(ACL)

6. 钩子(Hooks)与插件(Plugins)


发布订阅时序图
--------------

------------------------------------
连接层设计(Socket, Client, Protocol)
------------------------------------

Socket、Client、Protocol处理

eSockd - General Non-blocking TCP/SSL Socket Server

Acceptor Pool and Asynchronous TCP Accept

Max Connection Management

Leaky Bucket Rate Limiting

KeepAlive Timer

Parser and Serializer

Protocol Packets Processor

TCP/SSL Connection Support

MQTT Over WebSocket(SSL) Support

HTTP Publish API Support

Stomp, SockJS Support

Private TCP Protocol

全异步TCP收发::

    图片 

Parser Fun

Serializer Fun

Listeners列表


-------------------------
会话层设计(Session Layer)
-------------------------

会话层处理MQTT协议PUBLISH/SUBSCRIBE消息交互流程

Qos0/1/2消息接收与下发，消息超时重传，离线消息保存

飞行窗口(Inflight Window)，下发消息的顺序保证

缓存MQTT客户端的全部订阅(Subscription)，并终结QoS

服务器发送到客户端的，已发送未确认的Qos1/2消息

客户端发送到服务端，未接收到PUBREL的QoS2消息

客户端离线时，持久会话保存离线的Qos1/2消息

消息队列
--------------------------------------------

消息队列(Message Queue)和飞行窗口(Inflight Window)

飞行窗口(Inflight Window)保存当前正在发送未确认的Qos1/2消息。窗口值越大，吞吐越高；窗口值越小，消息顺序越严格

当客户端离线或者飞行窗口(Inflight Window)满时，消息缓存到队列

如果消息队列满，先丢弃Qos0消息，或者丢弃最早进入队列的消息

Qos
--------------------------------------------

Qos0, 1, 2

PacketId 与 MessageId
--------------------------------------------

PacketId 客户端到服务端的Packet收发与确认

MessageId 全局唯一的、时间序列的消息ID，分配给每一条Qos1/2消息，用于端到端的消息处理

Guid
--------------------------------------------

全局唯一时间序列消息ID结构：

64bits时间戳: erlang:system_time if Erlang >= R18, otherwise os:timestamp

Erlang节点ID: 编码为2字节

Erlang进程PID: 编码为4字节

进程内部序列号: 2字节的进程内部序列号


----------------------------------
路由层设计(Server, PubSub, Router)
----------------------------------

字典树(Trie)匹配路由

Topic表读取分布节点

Router进行消息路由分发

Session消息送达与重传

TODO: PubSub 图片

-------------------------------
分布集群设计(Distributed Layer)
-------------------------------

Topic Trie, Topic Table分布图

水平扩展??? 10台以上集群

集群(Cluster)
Mnesia数据库复制实现集群：一个disco_copies节点，多个ram_copies节点
订阅关系(Subscriptions)、本地路由表分别保存在各自节点
Topic Trie树、Topic->Node映射表多节点复制
桥接(Bridge)
Pub --> Broker1 --- Bridge Forward--> Broker2 -- Bridge Forward --> Broker3 --> Sub
桥接节点间只消息转发，不复制Mnesia数据库


-----------------------
认证与访问控制(ACL)设计
-----------------------

emqttd_access_control
----------------------

认证方式
------------------

用户名、密码认证
ClientID认证

匿名认证(anonymous)

浏览器Cookie认证

插件认证
LDAP
MySQL
PostgreSQL

ACL访问控制设计(https://github.com/emqtt/emqttd/wiki/ACL)
{allow | deny, Who, Access, TopicFilters}.
Who :: all | ClientId | {client, ClientId} | {ipaddr, IpAddr} | {user, Username}

ACL访问控制插件：
------------------

Internel: etc/acl.config 
MySQL
PostgreSQL
Redis(TODO)


----------------------------
钩子(Hook)与插件(Plugin)设计
----------------------------

钩子(Hooks) API
---------------

.. code:: erlang

    -export([hook/3, unhook/2, foreach_hooks/2, foldl_hooks/3]).

Hook::

    -spec hook(Hook :: atom(), Name :: any(), MFA :: mfa()) -> ok | {error, any()}.
    hook(Hook, Name, MFA) ->

Unhook::

    -spec unhook(Hook :: atom(), Name :: any()) -> ok | {error, any()}.
    unhook(Hook, Name) ->

Foreach Hooks::

    -spec foreach_hooks(Hook :: atom(), Args :: list()) -> any().
    foreach_hooks(Hook, Args) ->

Foldl Hooks::

    -spec foldl_hooks(Hook :: atom(), Args :: list(), Acc0 :: any()) -> any().
    foldl_hooks(Hook, Args, Acc0) ->
        ...

Hooks设计(https://github.com/emqtt/emqttd/wiki/Hooks%20Design)

比如端到端的消息处理...


插件(Plugins) API
------------------

插件通过钩子、模块注册等方式，扩展定制eMQTT消息服务器。

emqttd_plugin_template - Plugin template and demo
emqttd_dashboard - Web Dashboard
emqttd_plugin_mysql - Authentication with MySQL
emqttd_plugin_pgsql - Authentication with PostgreSQL
emqttd_plugin_redis - Redis Plugin
emqttd_stomp - Stomp Protocol Plugin
emqttd_sockjs - SockJS(Stomp) Plugin
emqttd_recon - Recon Plugin


.. code:: erlang

    %% Load all active plugins after broker started
    emqttd_plugins:load() 

    %% Load new plugin
    emqttd_plugins:load(Name)

    %% Unload all active plugins before broker stopped
    emqttd_plugins:unload()

    %% Unload a plugin
    emqttd_plugins:unload(Name)


端到端消息发布(Pub)与确认(Ack)
------------------------------


Could use 'message.publish', 'message.acked' hooks to implement end-to-end message pub/ack::

 PktId <-- --> MsgId <-- --> MsgId <-- --> PktId
      |<--- Qos --->|<---PubSub--->|<-- Qos -->|



--------------
Erlang设计相关
--------------

1. 使用Pool, Pool, Pool... 推荐GProc库(github.com/uwiger/gproc)

2. 异步，异步，异步消息...连接层到路由层异步消息，同步请求用于负载保护

3. 避免进程Mailbox累积消息，负载高的进程可以使用gen_server2

4. 服务器Socket连接、会话进程必须Hibernate

5. 多使用Binary数据，避免进程间内存复制

6. 使用ETS, ETS, ETS...Message Passing Vs ETS

7. 避免ETS表非键值字段select, match

8. 避免大量数据ETS读写, 每次ETS读写会复制内存，可使用lookup_element, update_counter

9. 适当开启ETS表{write_concurrency, true}

10. 保护Mnesia数据库事务，尽量减少事务数量，避免事务过载(overload)

11. 避免Mnesia数据表索引，和非键值字段match, select


