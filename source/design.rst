
.. _design:

=================
架构设计 (Design)
=================

----
前言
----

*EMQ X* 消息服务器在设计上，首先分离了前端协议(FrontEnd)与后端集成(Backend)，其次分离了消息路由平面(Flow Plane)与监控管理平面(Monitor/Control Plane):

.. image:: _static/images/design_1.png

100万连接
---------

多核服务器和现代操作系统内核层面，可以很轻松支持100万 TCP 连接，核心问题是应用层面如何处理业务瓶颈。

EMQ X 消息服务器在业务和应用层面，解决了单节点承载100万连接的各类瓶颈问题。连接测试的操作系统内核、TCP 协议栈、Erlang 虚拟机参数参见: http://docs.emqtt.cn/zh_CN/latest/tune.html

全异步架构
----------

EMQ X 消息服务器是基于 Erlang/OTP 平台的全异步的架构：异步 TCP 连接处理、异步主题(Topic)订阅、异步消息发布。只有在资源负载限制部分采用同步设计，比如 TCP 连接创建和 Mnesia 数据库事务执行。

EMQ X 3.0 版本中，一条 MQTT 消息从发布者(Publisher)到订阅者(Subscriber)，在 EMQ X 消息服务器内部异步流过一系列 Erlang 进程 Mailbox:

.. image:: _static/images/design_2.png

消息持久化
----------

EMQ X 开源产品不支持服务器内部消息持久化，这是一个架构设计选择。首先，EMQ X 解决的核心问题是连接与路由；其次，我们认为内置持久化是个错误设计。

传统内置消息持久化的 MQ 服务器，比如广泛使用的 JMS 服务器 ActiveMQ，几乎每个大版本都在重新设计持久化部分。内置消息持久化在设计上有两个问题:

1. 如何权衡内存与磁盘的使用？消息路由是基于内存的，而消息存储是基于磁盘的。

2. 多服务器分布集群架构下，如何放置 Queue 如何复制 Queue 的消息？

Kafka 在上述问题上，做出了正确的设计：一个完全基于磁盘分布式 Commit Log 的消息服务器。

EMQ X 在设计上分离消息路由与消息存储职责后，数据复制容灾备份甚至应用集成，可以在数据层面灵活实现。

EMQ X 企业版产品中，可以通过规则引擎或插件的方式，持久化消息到 Redis、MongoDB、Cassandra、MySQL、PostgreSQL 等数据库，以及 RabbitMQ、Kafka 等消息队列。

.. _architecture:

--------
系统架构
--------

概念模型
--------

EMQ X 消息服务器概念上更像一台网络路由器(Router)或交换机(Switch)，而不是传统的企业级消息队列(MQ)。相比网络路由器按 IP 地址或 MPLS 标签路由报文，EMQ X 按主题树(Topic Trie)发布订阅模式在集群节点间路由 MQTT 消息:

.. image:: ./_static/images/design_3.png

设计原则
--------

1. EMQ X 消息服务器核心解决的问题：处理海量的并发 MQTT 连接与路由消息。

2. 充分利用 Erlang/OTP 平台软实时、低延时、高并发、分布容错的优势。

3. 连接(Connection)、会话(Session)、路由(Router)、集群(Cluster)分层。

4. 消息路由平面(Flow Plane)与控制管理平面(Control Plane)分离。

5. 支持后端数据库或 NoSQL 实现数据持久化、容灾备份与应用集成。

系统分层
--------

1. 连接层(Connection Layer)：负责 TCP 连接处理、 MQTT 协议编解码。

2. 会话层(Session Layer)：处理 MQTT 协议发布订阅消息交互流程。

3. 路由层(Route Layer)：节点内路由派发 MQTT 消息。

4. 分布层(Distributed Layer)：分布节点间路由 MQTT 消息。

5. 认证与访问控制(ACL)：连接层支持可扩展的认证与访问控制模块。

6. 钩子(Hooks)与插件(Plugins)：系统每层提供可扩展的钩子，支持插件方式扩展服务器。

.. _connection_layer:

----------
连接层设计
----------

连接层处理服务端 Socket 连接与 MQTT 协议编解码：

1. 基于 `eSockd`_ 框架的异步 TCP 服务端

2. TCP Acceptor 池与异步 TCP Accept

3. TCP/SSL, WebSocket/SSL 连接支持

4. 最大并发连接数限制

5. 基于 IP 地址(CIDR)访问控制

6. 基于 Leaky Bucket 的流控

7. MQTT 协议编解码

8. MQTT 协议心跳检测

9. MQTT 协议报文处理

.. _session_layer:

----------
会话层设计
----------

会话层处理 MQTT 协议发布订阅(Publish/Subscribe)业务交互流程：

1. 缓存 MQTT 客户端的全部订阅(Subscription)，并终结订阅 QoS

2. 处理 Qos0/1/2 消息接收与下发，消息超时重传与离线消息保存

3. 飞行窗口(Inflight Window)，下发消息吞吐控制与顺序保证

4. 保存服务器发送到客户端的，已发送未确认的 Qos1/2 消息

5. 缓存客户端发送到服务端，未接收到 PUBREL 的 QoS2 消息

6. 客户端离线时，保存持久会话的离线 Qos1/2 消息

消息队列与飞行窗口
------------------

会话层通过一个内存消息队列和飞行窗口处理下发消息:

.. image:: _static/images/design_4.png

飞行窗口(Inflight Window)保存当前正在发送未确认的 Qos1/2 消息。窗口值越大，吞吐越高；窗口值越小，消息顺序越严格。

当客户端离线或者飞行窗口(Inflight Window)满时，消息缓存到队列。如果消息队列满，先丢弃 Qos0 消息或最早进入队列的消息。

报文 ID 与消息 ID
------------------

MQTT 协议定义了一个 16bits 的报文 ID(PacketId)，用于客户端到服务器的报文收发与确认。MQTT 发布报文(PUBLISH)进入消息服务器后，转换为一个消息对象并分配 128bits 消息 ID(MessageId)。

全局唯一时间序列消息 ID 结构:

.. image:: _static/images/design_5.png

1. 64bits 时间戳: erlang:system_time if Erlang >= R18, otherwise os:timestamp

2. Erlang 节点 ID: 编码为2字节

3. Erlang 进程 PID: 编码为4字节

4. 进程内部序列号: 2字节的进程内部序列号

端到端消息发布订阅(Pub/Sub)过程中，发布报文 ID 与报文 QoS 终结在会话层，由唯一 ID 标识的 MQTT 消息对象在节点间路由:

.. image:: _static/images/design_6.png

.. _route_layer:

----------
路由层设计
----------

路由层维护订阅者(subscriber)与订阅关系表(subscription)，并在本节点发布订阅模式派发(Dispatch)消息:

.. image:: ./_static/images/design_7.png

消息派发到会话(Session)后，由会话负责按不同 QoS 送达消息。

.. _distributed_layer:

----------
分布层设计
----------

分布层维护全局主题树(Topic Trie)与路由表(Route Table)。主题树由通配主题构成，路由表映射主题到节点:

.. image:: ./_static/images/design_8.png

分布层通过匹配主题树(Topic Trie)和查找路由表(Route Table)，在集群的节点间转发路由 MQTT 消息:

.. image:: ./_static/images/design_9.png

.. _hook:

--------------
钩子(Hook)设计
--------------

钩子(Hook)定义
--------------

*EMQ X* 在客户端生命周期和会话生命周期，对连接、订阅、消息收发位置设计了扩展钩子(Hook):

+------------------------+----------------------------------+
| 钩子                   | 说明                             |
+========================+==================================+
| client.connect         | 收到客户端连接报文               |
+------------------------+----------------------------------+
| client.connack         | 下发连接应答                     |
+------------------------+----------------------------------+
| client.connected       | 客户端上线                       |
+------------------------+----------------------------------+
| client.disconnected    | 客户端连接断开                   |
+------------------------+----------------------------------+
| client.authenticate    | 连接认证                         |
+------------------------+----------------------------------+
| client.check_acl       | ACL 校验                         |
+------------------------+----------------------------------+
| client.subscribe       | 客户端订阅主题                   |
+------------------------+----------------------------------+
| client.unsubscribe     | 客户端取消订阅主题               |
+------------------------+----------------------------------+
| session.created        | 会话创建                         |
+------------------------+----------------------------------+
| session.subscribed     | 会话订阅主题后                   |
+------------------------+----------------------------------+
| session.unsubscribed   | 会话取消订阅主题后               |
+------------------------+----------------------------------+
| session.resumed        | 会话恢复                         |
+------------------------+----------------------------------+
| session.discarded      | 会话被删除                       |
+------------------------+----------------------------------+
| session.takeovered     | 会话被其它节点接管               |
+------------------------+----------------------------------+
| session.terminated     | 会话终止                         |
+------------------------+----------------------------------+
| message.publish        | MQTT 消息发布                    |
+------------------------+----------------------------------+
| message.delivered      | MQTT 消息进行投递                |
+------------------------+----------------------------------+
| message.acked          | MQTT 消息回执                    |
+------------------------+----------------------------------+
| message.dropped        | MQTT 消息丢弃                    |
+------------------------+----------------------------------+


钩子(Hook) 采用职责链设计模式(`Chain-of-responsibility_pattern`_)，扩展模块或插件向钩子注册回调函数，系统在客户端上下线、主题订阅或消息发布确认时，触发钩子顺序执行回调函数:

.. image:: ./_static/images/design_10.png

不同钩子的回调函数输入参数不同，用户可参考插件模版的 `emqx_plugin_template`_ 模块，每个回调函数应该返回:

+----------------+----------------------+
|      返回      |         说明         |
+================+======================+
| ok             | 继续执行             |
+----------------+----------------------+
| {ok, NewAcc}   | 返回累积参数继续执行 |
+----------------+----------------------+
| stop           | 停止执行             |
+----------------+----------------------+
| {stop, NewAcc} | 返回累积参数停止执行 |
+----------------+----------------------+

钩子(Hook)实现
--------------

emqx 模块封装了 Hook 接口:

.. code-block:: erlang

    -spec(hook(emqx_hooks:hookpoint(), emqx_hooks:action()) -> ok | {error, already_exists}).
    hook(HookPoint, Action) ->
        emqx_hooks:add(HookPoint, Action).

    -spec(hook(emqx_hooks:hookpoint(), emqx_hooks:action(), emqx_hooks:filter() | integer())
        -> ok | {error, already_exists}).
    hook(HookPoint, Action, Priority) when is_integer(Priority) ->
        emqx_hooks:add(HookPoint, Action, Priority);
    hook(HookPoint, Action, Filter) when is_function(Filter); is_tuple(Filter) ->
        emqx_hooks:add(HookPoint, Action, Filter);
    hook(HookPoint, Action, InitArgs) when is_list(InitArgs) ->
        emqx_hooks:add(HookPoint, Action, InitArgs).

    -spec(hook(emqx_hooks:hookpoint(), emqx_hooks:action(), emqx_hooks:filter(), integer())
        -> ok | {error, already_exists}).
    hook(HookPoint, Action, Filter, Priority) ->
        emqx_hooks:add(HookPoint, Action, Filter, Priority).

    -spec(unhook(emqx_hooks:hookpoint(), emqx_hooks:action()) -> ok).
    unhook(HookPoint, Action) ->
        emqx_hooks:del(HookPoint, Action).

    -spec(run_hook(emqx_hooks:hookpoint(), list(any())) -> ok | stop).
    run_hook(HookPoint, Args) ->
        emqx_hooks:run(HookPoint, Args).

    -spec(run_fold_hook(emqx_hooks:hookpoint(), list(any()), any()) -> any()).
    run_fold_hook(HookPoint, Args, Acc) ->
        emqx_hooks:run_fold(HookPoint, Args, Acc).

钩子(Hook)使用
--------------

`emqx_plugin_template`_ 提供了全部钩子的使用示例，例如端到端的消息处理回调:

.. code-block:: erlang

    -module(emqx_plugin_template).

    -export([load/1, unload/0]).

    -export([on_message_publish/2, on_message_delivered/3, on_message_acked/3]).

    load(Env) ->
        emqx:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
        emqx:hook('message.delivered', fun ?MODULE:on_message_delivered/3, [Env]),
        emqx:hook('message.acked', fun ?MODULE:on_message_acked/3, [Env]).

    on_message_publish(Message, _Env) ->
        io:format("publish ~s~n", [emqx_message:format(Message)]),
        {ok, Message}.

    on_message_delivered(ClientInfo, Message, _Env) ->
        io:format("deliver to client ~s: ~s~n", [ClientInfo, emqx_message:format(Message)]),
        {ok, Message}.

    on_message_acked(ClientInfo, Message, _Env) ->
        io:format("client ~s acked: ~s~n", [ClientInfo, emqx_message:format(Message)]),
        {ok, Message}.

    unload() ->
        emqx:unhook('message.publish', fun ?MODULE:on_message_publish/2),
        emqx:unhook('message.acked', fun ?MODULE:on_message_acked/3),
        emqx:unhook('message.delivered', fun ?MODULE:on_message_delivered/3).

.. _auth_acl:

------------------
认证与访问控制设计
------------------

*EMQ X* 消息服务器支持可扩展的认证与访问控制，通过挂载 ``client.authenticate`` and ``client.check_acl`` 两个钩子实现。

编写鉴权钩子回调函数
--------------------

挂载回调函数到 ``client.authenticate`` 钩子:

.. code-block:: erlang

    Env = some_input_params,
    emqx:hook('client.authenticate', fun ?MODULE:on_client_authenticate/3, [Env]).

钩子回调函数必须接受三个 *EMQ X* 回调的参数 ``ClientInfo``  ``AuthResult`` 和 ``Env`` 并且返回一个新的 ``AuthResult``.

.. code-block:: erlang

    on_client_authenticate(ClientInfo = #{password := Password}, AuthResult, Env) ->
        {ok, AuthResult#{result => success}}.

``ClientInfo`` 结构体是一个包含该客户端信息的 map，包括 clientid, username, password 等；具体可查看 ``emqx_types.erl`` 模块的定义。
``AuthResult`` 结构体是一个用于返回认证结果的 map:

.. code-block:: erlang

    #{is_superuser => IsSuperUser,  %% 布尔值; 是否为超级用户
      result => Result,             %% 枚举值; success 表示认证成功
      anonymous => Anonymous        %% 布尔值; 是否为匿名用户
     }

编写 ACL 钩子回调函数
----------------------

挂载回调函数到 ``client.check_acl`` 钩子:

.. code-block:: erlang

    Env = some_input_params,
    emqx:hook('client.check_acl', fun ?MODULE:on_client_check_acl/5, [Env]).

回调函数须接受 ``ClientInfo``, ``AccessType``, ``Topic``, ``ACLResult``, ``Env`` 这几个参数， 然后返回一个新的 ACLResult:

.. code-block:: erlang

    on_client_check_acl(#{client_id := ClientId}, AccessType, Topic, ACLResult, Env) ->
        {ok, allow}.

AccessType: 枚举值; ``publish`` 或  ``subscribe``
Topic: binary 类型; PUBLISH/SUBSCRIBE 报文中的主题字段
ACLResult: 枚举值； ``allow`` 或 ``deny``

``emqx_mod_acl_internal`` 模块实现了基于 etc/acl.conf 文件的 ACL 机制，etc/acl.conf 文件的默认内容：

.. code-block:: erlang

    %%%-----------------------------------------------------------------------------
    %%%
    %%% -type who() :: all | binary() |
    %%%                {ipaddr, esockd_access:cidr()} |
    %%%                {client, binary()} |
    %%%                {user, binary()}.
    %%%
    %%% -type access() :: subscribe | publish | pubsub.
    %%%
    %%% -type topic() :: binary().
    %%%
    %%% -type rule() :: {allow, all} |
    %%%                 {allow, who(), access(), list(topic())} |
    %%%                 {deny, all} |
    %%%                 {deny, who(), access(), list(topic())}.
    %%%
    %%%-----------------------------------------------------------------------------

    {allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

    {allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

    {deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

    {allow, all}.

由 emqx 提供的 Auth/ACL 插件:

+-----------------------+--------------------------------+
| Plugin                | Authentication                 |
+-----------------------+--------------------------------+
| emqx_auth_username    | Username and Password          |
+-----------------------+--------------------------------+
| emqx_auth_clientid    | ClientID and Password          |
+-----------------------+--------------------------------+
| emqx_auth_ldap        | LDAP                           |
+-----------------------+--------------------------------+
| emqx_auth_http        | HTTP API                       |
+-----------------------+--------------------------------+
| emqx_auth_mysql       | MySQL                          |
+-----------------------+--------------------------------+
| emqx_auth_pgsql       | PostgreSQL                     |
+-----------------------+--------------------------------+
| emqx_auth_redis       | Redis                          |
+-----------------------+--------------------------------+
| emqx_auth_mongo       | MongoDB                        |
+-----------------------+--------------------------------+
| emqx_auth_jwt         | JWT                            |
+-----------------------+--------------------------------+

.. _plugin:

----------------
插件(Plugin)设计
----------------

插件是一个可以被动态加载的普通 Erlang 应用(Application)。插件主要通过钩子(Hook)机制扩展服务器功能，或通过注册扩展模块方式集成认证访问控制。

emqx_plugins 模块实现插件机制，提供加载卸载插件 API ::

    -module(emqx_plugins).

    -export([load/1, unload/1]).

    %% @doc Load a Plugin
    load(PluginName :: atom()) -> ok | {error, any()}.

    %% @doc UnLoad a Plugin
    unload(PluginName :: atom()) -> ok | {error, any()}.

用户可通过 `./bin/emqx_ctl` 命令行加载卸载插件::

    ./bin/emqx_ctl plugins load <plugin name>

    ./bin/emqx_ctl plugins unload <plugin name>

开发者请参考模版插件: http://github.com/emqx/emqx_plugin_template

-----------------
Mnesia/ETS 表设计
-----------------

+--------------------------+--------+------------------+
|          Table           |  Type  |   Description    |
+==========================+========+==================+
| emqx_conn                | ets    | 连接表           |
+--------------------------+--------+------------------+
| emqx_metrics             | ets    | 统计表           |
+--------------------------+--------+------------------+
| emqx_session             | ets    | 会话表           |
+--------------------------+--------+------------------+
| emqx_hooks               | ets    | 钩子表           |
+--------------------------+--------+------------------+
| emqx_subscriber          | ets    | 订阅者表         |
+--------------------------+--------+------------------+
| emqx_subscription        | ets    | 订阅表           |
+--------------------------+--------+------------------+
| emqx_admin               | mnesia | Dashboard 用户表 |
+--------------------------+--------+------------------+
| emqx_retainer            | mnesia | Retained 消息表  |
+--------------------------+--------+------------------+
| emqx_shared_subscription | mnesia | 共享订阅表       |
+--------------------------+--------+------------------+
| emqx_session_registry    | mnesia | 全局会话注册表   |
+--------------------------+--------+------------------+
| emqx_alarm_history       | mnesia | 告警历史表       |
+--------------------------+--------+------------------+
| emqx_alarm               | mnesia | 告警表           |
+--------------------------+--------+------------------+
| emqx_banned              | mnesia | 禁止登陆表       |
+--------------------------+--------+------------------+
| emqx_route               | mnesia | 路由表           |
+--------------------------+--------+------------------+
| emqx_trie                | mnesia | Trie 表          |
+--------------------------+--------+------------------+
| emqx_trie_node           | mnesia | Trie Node 表     |
+--------------------------+--------+------------------+
| mqtt_app                 | mnesia | App 表           |
+--------------------------+--------+------------------+

.. _erlang:

---------------
Erlang 设计相关
---------------

1. 使用 Pool, Pool, Pool... 推荐 GProc 库: https://github.com/uwiger/gproc

2. 异步，异步，异步消息...连接层到路由层异步消息，同步请求用于负载保护

3. 避免进程 Mailbox 累积消息

4. 消息流经的 Socket 连接、会话进程必须 Hibernate，主动回收 binary 句柄

5. 多使用 Binary 数据，避免进程间内存复制

6. 使用 ETS, ETS, ETS... Message Passing vs. ETS

7. 避免 ETS 表非键值字段 select, match

8. 避免大量数据 ETS 读写, 每次 ETS 读写会复制内存，可使用 lookup_element, update_counter

9. 适当开启 ETS 表 {write_concurrency, true}

10. 保护 Mnesia 数据库事务，尽量减少事务数量，避免事务过载(overload)

11. 避免对 Mnesia 数据表非索引、或非键值字段 match, select

.. _eSockd: https://github.com/emqx/esockd
.. _Chain-of-responsibility_pattern: https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern
.. _emqx_plugin_template: https://github.com/emqx/emqx_plugin_template/blob/master/src/emqx_plugin_template.erl

