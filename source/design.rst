
======================
架构设计(Design Guide)
======================

----------------------
系统架构(Architecture)
----------------------

概念模型
----------------------

Nodes, Topic Trie, PubSub

.. image:: _static/images/concept.png

设计原则
----------------------
软实时、低延时、高并发Erlang/OTP平台！
连接与路由分层
分离Flow平面与控制平面
避免过度设计，充分利用Erlang/OTP平台


系统分层
----------------------

连接层(Socket, Client, Protocol)

会话层(Global Session)

路由层(Router, PubSub)

分布层(Trie树, Topic表)

认证与访问控制(ACL)

钩子(Hooks)与插件(Plugins)

Erlang相关的设计建议?

发布订阅时序图
----------------------

--------------------------------------------
连接层设计(Socket, Client, Protocol)
--------------------------------------------

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


--------------------------------------------
会话层设计(Session)
--------------------------------------------

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


--------------------------------------------
路由层设计(Router, PubSub, Trie)
--------------------------------------------

字典树(Trie)匹配路由

Topic表读取分布节点

Router进行消息路由分发

Session消息送达与重传

TODO: PubSub 图片

--------------------------------------------
分布集群设计(Distributed)
--------------------------------------------

Topic Trie, Topic Table分布图

水平扩展??? 10台以上集群

集群(Cluster)
Mnesia数据库复制实现集群：一个disco_copies节点，多个ram_copies节点
订阅关系(Subscriptions)、本地路由表分别保存在各自节点
Topic Trie树、Topic->Node映射表多节点复制
桥接(Bridge)
Pub --> Broker1 --- Bridge Forward--> Broker2 -- Bridge Forward --> Broker3 --> Sub
桥接节点间只消息转发，不复制Mnesia数据库


--------------------------------------------
认证与ACL设计
--------------------------------------------

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


--------------------------------------------
钩子(Hook)与插件(Plugin)设计
--------------------------------------------

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


--------------------------------------------
Event 与 broker pubsub设计
--------------------------------------------

事件，broker:subscribe, broker:pubsub


--------------------------------------------
Pool进程池设计
--------------------------------------------


--------------------------------------------
Erlang设计相关建议
--------------------------------------------

1. 使用Pool, Pool, Pool… and GProc(github.com/uwiger/gproc)

2. 异步，异步，异步消息...同步用于负载保护

3. 避免进程Mailbox累积消息，负载高的进程可以使用gen_server2

4. 服务器Socket连接、会话进程必须Hibernate

5. 多使用Binary数据，避免进程间内存复制

6. 使用ETS, ETS, ETS…Message Passing Vs ETS

避免ETS select, match without key

避免大量数据读写ETS, 使用lookup_element, update_counter…

适当开启ETS表{write_concurrency, true}

保护Mnesia Transaction，避免overload

避免Mnesia index_read, match, select

监控::

    erlang:system_monitor监控long_schedule, long_gc, busy_port, busy_dis_port
    etop查看msg_q, memory, reductions, runtime…


