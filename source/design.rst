
======================
架构设计(Design Guide)
======================

----
前言
----

emqttd消息服务器经过两年时间开发，开发方式有点像摇滚乐专辑的制作。最初由一些即兴创作的部分组成，但最终整体上体现了某种程度的正交(Orthogonality)和一致性(Consistency)。

emqttd消息服务器1.0版本在部分细节上仍是粗糙的，但在架构上做出了正确的选择和设计。目前设计带来的一个好的结果是：emqttd可能是开源领域唯一一个，几乎不需要用户做太多努力，就可以支持到100万连接的MQTT服务器。坏的结果是：我们无法向用户提供百万连接优化的商业服务。

100万连接
---------

多核心服务器和现代操作系统内核层面，可以很轻松支持到100万TCP连接，核心问题是应用层面如何处理业务瓶颈。

emqttd消息服务器在业务和应用层面，解决了承载100万连接的各类瓶颈问题。100万连接测试的操作系统内核、TCP协议栈、Erlang虚拟机参数: http://docs.emqtt.cn/zh_CN/latest/tune.html

全异步架构
----------

emqttd消息服务器是基于Erlang/OTP平台的全异步的架构：异步TCP连接处理、异步主题(Topic)订阅、异步消息发布。只有在资源负载限制部分采用同步设计，比如TCP连接创建和Mnesia数据库事务执行。

一条MQTT消息从发布者(Publisher)到订阅者(Subscriber)，在emqttd消息服务器内部异步流过一系列Erlang进程的Mailbox::

                     --------------          ---------------          --------------
    Publisher--Msg-->| Pub Client | --Msg--> | Sub Session | --Msg--> | Sub Client | --Msg-->Subscriber
                     --------------          ---------------          --------------

消息持久化
----------

emqttd1.0版本没有实现服务器内部的消息持久化，这是一个架构设计选择。首先，emqttd解决的核心问题是连接与路由；其次，我们认为内置持久化是个错误设计。

传统内置消息持久化的MQ服务器，比如广泛使用的JMS服务器ActiveMQ，几乎每个大版本都在重新设计持久化的部分。内置消息持久化设计上有两个问题:

1. 如何平衡内存与磁盘使用？消息路由基于内存，消息存储又是基于磁盘。

2. 多节点分布式架构下，如何放置Queue？如何复制Queue？

Kafka在上述问题上，做出了正确的设计：一个完全基于磁盘分布式commit log的消息服务器。

emqttd2.0版本通过对接外部存储，例如Redis、Kafka、Cassandra、PostgreSQL，实现多种方式的消息持久化。

设计上分离消息路由与消息存储职责后，数据复制或容灾备份甚至应用集成，可以在数据层面实现。

NetSplit问题
------------

emqttd消息服务器集群，基于Mnesia数据库设计。NetSplit发生时，节点间状态是：Erlang节点间可以连通，互相询问自己是否宕机，对方回答你已经宕机:(

NetSplit故障发生时，emqttd消息服务器的log/emqttd_error.log日志，会打印critical级别日志::

    Mnesia inconsistent_database event: running_partitioned_network, emqttd@host

emqttd集群部署在同一IDC网络下，NetSplit发生的几率很低，一旦发生又很难自动化处理。所以emqttd1.0版本设计选择是，集群不自动化处理NetSplit，需要人工重启部分节点。

----------------------
系统架构(Architecture)
----------------------

概念模型
--------

emqttd消息服务器概念上像一台网络路由器(Router)或交换机(Switch)，而不是传统的企业级消息服务器((MQ)。相比网络路由器按IP地址或MPLS标签路由报文，emqttd按MQTT的主题树(Topic Trie)发布订阅模式在集群节点间路由消息:

.. image:: _static/images/concept.png

设计原则
--------

1. emqttd消息服务器核心解决的问题：处理海量的并发MQTT连接与消息路由。
2. 充分利用Erlang/OTP平台软实时、低延时、高并发、分布容错的优势。
3. 连接(Connection)、会话(Session)、路由(Router)、集群(Cluster)分层。
4. 消息路由平面(Flow Plane)与控制管理平面(Control Plane)分离。
5. 支持后端数据库或NoSQL实现数据持久化、容灾备份与应用集成。

系统分层
--------

1. 连接层(Connection Layer)： 负责TCP连接处理、MQTT协议编解码。

2. 会话层(Session Layer)：处理MQTT协议发布订阅消息交互流程。
   
3. 路由层(Route Layer)：节点内路由MQTT消息。
   
4. 分布层(Distributed Layer)：分布节点间路由MQTT消息。
   
5. 认证与访问控制(ACL)：连接层支持可定制的认证与访问控制模块。

6. 钩子(Hooks)与插件(Plugins)：系统每层提供可扩展的钩子，支持用户插件方式扩展服务器。

------------------------------------
连接层设计(Socket, Client, Protocol)
------------------------------------

连接层主要负责服务端Socket处理与MQTT协议编解码：

1. 基于eSockd框架的异步TCP服务端
2. TCP Acceptor池与异步TCP Accept
3. TCP/SSL, WebSocket/SSL连接支持
4. 最大连接数限制
5. 基于IP地址访问控制
6. 基于Leaky Bucket的流控
7. MQTT协议编解码
8. MQTT协议心跳检测
9. MQTT协议报文处理

-------------------------
会话层设计(Session Layer)
-------------------------

会话层负责处理MQTT协议发布订阅(Publish/Subscribe)业务交互流程：

1. 缓存MQTT客户端的全部订阅(Subscription)，并终结QoS

2. 处理Qos0/1/2消息接收与下发，消息超时重传，离线消息保存

3. 飞行窗口(Inflight Window)，下发消息的顺序保证

4. 保存服务器发送到客户端的，已发送未确认的Qos1/2消息

5. 保存客户端发送到服务端，未接收到PUBREL的QoS2消息

6. 客户端离线时，保存持久会话的离线Qos1/2消息

消息队列(Queue)和飞行窗口(Inflight Window)
------------------------------------------

会话层通过一个内存消息队列和飞行窗口处理下发消息::

       |<----------------- Max Len ----------------->|
       -----------------------------------------------
 IN -> |      Messages Queue   |  Inflight Window    | -> Out
       -----------------------------------------------
                               |<---   Win Size  --->|

飞行窗口(Inflight Window)保存当前正在发送未确认的Qos1/2消息。窗口值越大，吞吐越高；窗口值越小，消息顺序越严格。

当客户端离线或者飞行窗口(Inflight Window)满时，消息缓存到队列。如果消息队列满，先丢弃Qos0消息，或丢弃最早进入队列的消息。

报文Id(PacketId)与消息ID(MessageId)
-----------------------------------

MQTT协议定义了一个16bit的报文ID(PacketId)，用于客户端到服务器的报文收发与确认。MQTT发布报文(PUBLISH)进入消息服务器后，转换为一个MQTT消息并分配全局唯一的128bits消息ID(MessageId)。

全局唯一时间序列消息ID结构：

1. 64bits时间戳: erlang:system_time if Erlang >= R18, otherwise os:timestamp
2. Erlang节点ID: 编码为2字节
3. Erlang进程PID: 编码为4字节
4. 进程内部序列号: 2字节的进程内部序列号

端到端消息发布订阅(Pub/Ack)过程中，发布报文、报文ID与报文QoS终结在会话层，由唯一ID标识的MQTT消息在节点间路由::

    PktId <-- Session --> MsgId <-- Router --> MsgId <-- Session --> PktId

----------------------------------
路由层设计(Server, PubSub, Router)
----------------------------------

路由层维护订阅者(subscriber)与订阅关系表(subscription)，并在本节点发布订阅模式派发(Dispatch)消息::

.. image:: _static/images/dispatch.png

消息派发到会话(Session)后，由会话负责发起消息送达的QoS流程。

-------------------------------
分布集群设计(Distributed Layer)
-------------------------------

分布层维护全局主题树(Topic Trie)与路由表(Route Table)。主题树由通配主题构成，路由表映射主题到节点::

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

路由层通过匹配主题树(Topic Trie)和查找路由表(Route Table)，在集群的节点间转发路由MQTT消息。

-----------------------
认证与访问控制(ACL)设计
-----------------------

emqttd消息服务器通过注册扩展模块方式，支持用户多种认证方式与访问控制。

认证(Authentication)
--------------------

emqttd_auth_mod定义认证模块Behavihour::

    -module(emqttd_auth_mod).

    -ifdef(use_specs).

    -callback init(AuthOpts :: list()) -> {ok, State :: any()}.

    -callback check(Client, Password, State) -> ok | ignore | {error, string()} when
        Client    :: mqtt_client(),
        Password  :: binary(),
        State     :: any().

    -callback description() -> string().

    -else.

    -export([behaviour_info/1]).

    behaviour_info(callbacks) ->
        [{init, 1}, {check, 3}, {description, 0}];
    behaviour_info(_Other) ->
        undefined.

    -endif.

认证模块实现emqttd_auth_mod回调函数，系统默认实现模块包括:

+----------------------+--------------------------------+
| 模块                 | 认证方式                       |
+----------------------+--------------------------------+
| emqttd_auth_username | 用户名密码认证                 |
+----------------------+--------------------------------+
| emqttd_auth_clientid | ClientID认证                   |
+----------------------+--------------------------------+
| emqttd_auth_ldap     | LDAP认证                       |
+----------------------+--------------------------------+
| emqttd_auth_anonymous | 匿名认证 |    
+----------------------+--------------------------------+

访问控制(ACL)
-------------

emqttd_acl_mod模块定义访问控制Behavihour::

    -module(emqttd_acl_mod).

    -include("emqttd.hrl").

    -ifdef(use_specs).

    -callback init(AclOpts :: list()) -> {ok, State :: any()}.

    -callback check_acl({Client, PubSub, Topic}, State :: any()) -> allow | deny | ignore when
        Client   :: mqtt_client(),
        PubSub   :: pubsub(),
        Topic    :: binary().

    -callback reload_acl(State :: any()) -> ok | {error, any()}.

    -callback description() -> string().

    -else.

    -export([behaviour_info/1]).

    behaviour_info(callbacks) ->
        [{init, 1}, {check_acl, 2}, {reload_acl, 1}, {description, 0}];
    behaviour_info(_Other) ->
        undefined.

    -endif.



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


--------------
钩子(Hook)设计
--------------

钩子(Hook)定义
--------------

通过钩子(Hook)处理客户端上下线、主题订阅、消息收发:

+------------------------+----------------------------------+
| 名称                   | 说明                             |
+------------------------+----------------------------------+
| client.connected       | 客户端上线                       |
+------------------------+----------------------------------+
| client.subscribe       | 客户端订阅主题前                 |
+------------------------+----------------------------------+
| client.subscribe.after | 客户端订阅主题后                 |
+------------------------+----------------------------------+
| client.unsubscribe     | 客户端取消订阅主题               |
+------------------------+----------------------------------+
| message.publish        | MQTT消息发布                     |
+------------------------+----------------------------------+
| message.delivered      | MQTT消息送达                     |
+------------------------+----------------------------------+
| message.acked          | MQTT消息回执                     |
+------------------------+----------------------------------+
| client.disconnected    | 客户端连接断开                   |
+----------------------- +----------------------------------+

职责链设计模式(Chain-of-responsibility): https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern

执行钩子时，会传入参数列表，和一个Accumulator?::

    run(HookPoint, Args, Acc)

                     --------  ok | {ok, NewAcc}   --------  ok | {ok, NewAcc}    --------
     (Args, Acc) --> | Fun1 | -------------------> | Fun2 | --------------------> | Fun3 | --> {ok, Acc} | {stop, Acc}
                     --------                      --------                       --------
                        |
                    {stop, NewAcc}


当事件发生时，Broker通过钩子，执行一系列回调函数(Callback)。每个回调函数可以返回:

+-----------------+--------------------+
| 返回            | 说明               |
+-----------------+--------------------+
| ok              | 继续执行           |
+-----------------+--------------------+
| {ok, NewAcc}    | 继续执行并返回结果 |
+-----------------+--------------------+
| stop            | 停止执行           |
+-----------------+--------------------+
| {stop, NewAcc}  | 停止执行并返回结果 |
+-----------------+--------------------+


钩子(Hook) API
---------------

emqttd_hook模块:

HOOK API:

.. code:: erlang

    -module(emqttd_hook).

    %% Hooks API
    -export([add/3, add/4, delete/2, run/3, lookup/1]).

    -spec(add(atom(), function(), list(any())) -> ok).

    -spec(add(atom(), function(), list(any()), integer()) -> ok).

    -spec(delete(atom(), function()) -> ok).

    -spec(run(atom(), list(any()), any()) -> any()).

    -spec(lookup(atom()) -> [#callback{}]).

emqttd模块wrap API:

.. code:: erlang

    -module(emqttd).

    %% Hooks API
    -export([hook/4, hook/3, unhook/2, run_hooks/3]).

    -spec(hook(atom(), function(), list(any())) -> ok | {error, any()}).

    -spec(hook(atom(), function(), list(any()), integer()) -> ok | {error, any()}).

    -spec(unhook(atom(), function()) -> ok | {error, any()}).

    -spec(run_hooks(atom(), list(any()), any()) -> {ok | stop, any()}).



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

端到端的消息处理
----------------

.. code::

    -module(emqttd_plugin_template).

    -export([load/1, unload/0]).
    
    -export([on_message_publish/2, on_message_delivered/3, on_message_acked/3]).

    load(Env) ->
        emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
        emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/3, [Env]),
        emqttd:hook('message.acked', fun ?MODULE:on_message_acked/3, [Env]).

    on_message_publish(Message, _Env) ->
        io:format("publish ~s~n", [emqttd_message:format(Message)]),
        {ok, Message}.

    on_message_delivered(ClientId, Message, _Env) ->
        io:format("delivered to client ~s: ~s~n", [ClientId, emqttd_message:format(Message)]),
        {ok, Message}.

    on_message_acked(ClientId, Message, _Env) ->
        io:format("client ~s acked: ~s~n", [ClientId, emqttd_message:format(Message)]),
        {ok, Message}.

    unload() ->
        emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2),
        emqttd:unhook('message.acked', fun ?MODULE:on_message_acked/3),
        emqttd:unhook('message.delivered', fun ?MODULE:on_message_delivered/3).

-----------------
插件(Plugin)设计
-----------------

插件是一个普通的Erlang应用，放置在emqttd/plugins目录可以被动态加载。插件可以通过注册扩展模块方式集成不同的认证访问控制，或通过钩子(Hook)机制扩展服务器功能。

插件机制由emqttd_plugins模块实现，提供加载卸载插件API::

    -module(emqttd_plugins).

    -export([load/1, unload/1]).

    %% @doc Load One Plugin
    load(atom()) -> ok | {error, any()}.

    %% @doc UnLoad One Plugin
    unload(atom()) -> ok | {error, any()}.

用户通过'./bin/emqttd_ctl'命令行加载卸载插件::

    ./bin/emqttd_ctl plugins load emqttd_plugin_redis

    ./bin/emqttd_ctl plugins unload emqttd_plugin_redis

用户可以参考模版插件: http://github.com/emqtt/emqttd_plugin_template

--------------
Erlang设计相关
--------------

1. 使用Pool, Pool, Pool... 推荐GProc库: https://github.com/uwiger/gproc

2. 异步，异步，异步消息...连接层到路由层异步消息，同步请求用于负载保护

3. 避免进程Mailbox累积消息，负载高的进程可以使用gen_server2

4. 消息流经的Socket连接、会话进程必须Hibernate，主动回收binary句柄

5. 多使用Binary数据，避免进程间内存复制

6. 使用ETS, ETS, ETS...Message Passing Vs ETS

7. 避免ETS表非键值字段select, match

8. 避免大量数据ETS读写, 每次ETS读写会复制内存，可使用lookup_element, update_counter

9. 适当开启ETS表{write_concurrency, true}

10. 保护Mnesia数据库事务，尽量减少事务数量，避免事务过载(overload)

11. 避免Mnesia数据表索引，和非键值字段match, select

