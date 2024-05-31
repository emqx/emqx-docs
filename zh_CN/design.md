# 架构设计 (Design)

## 前言

EMQ 2.0 消息服务器设计，在 1.x 版本的基础上，首先分离了前端协议(FrontEnd)与后端集成(Backend)，其次分离了消息路由平面(Flow Plane)与监控管理平面(Monitor/Control Plane)。EMQ 2.0 消息服务器将在 1.x 版本支持 100 万 MQTT 连接的基础上，向可管理可监控坚如磐石的稳定性方向迭代演进:

                Control Plane
            --------------------
                |            |
    FrontEnd -> | Flow Plane | -> BackEnd
                |            |
                Session      Router
            ---------------------
                Monitor Plane

### 100 万连接

多核服务器和现代操作系统内核层面，可以很轻松支持 100 万 TCP 连接，核心问题是应用层面如何处理业务瓶颈。

EMQ 消息服务器在业务和应用层面，解决了承载 100 万连接的各类瓶颈问题。连接测试的操作系统内核、TCP 协议栈、Erlang 虚拟机参数: [ http://docs.emqtt.cn/zh_CN/latest/tune.html ](http://docs.emqtt.cn/zh_CN/latest/tune.html)

### 全异步架构

EMQ 消息服务器是基于 Erlang/OTP 平台的全异步的架构：异步 TCP 连接处理、异步主题(Topic)订阅、异步消息发布。只有在资源负载限制部分采用同步设计，比如 TCP 连接创建和 Mnesia 数据库事务执行。

一条 MQTT 消息从发布者(Publisher)到订阅者(Subscriber)，在 EMQ 消息服务器内部异步流过一系列 Erlang 进程 Mailbox:

                      ----------          -----------          ----------
    Publisher --Msg-->| Client | --Msg--> | Session | --Msg--> | Client | --Msg--> Subscriber
                      ----------          -----------          ----------

### 消息持久化

EMQ 1.0 版本不支持服务器内部消息持久化，这是一个架构设计选择。首先，EMQ 解决的核心问题是连接与路由；其次，我们认为内置持久化是个错误设计。

传统内置消息持久化的 MQ 服务器，比如广泛使用的 JMS 服务器 ActiveMQ，几乎每个大版本都在重新设计持久化部分。内置消息持久化在设计上有两个问题:

1. 如何平衡内存与磁盘使用？消息路由基于内存，消息存储是基于磁盘。
2. 多服务器分布集群架构下，如何放置 Queue 如何复制 Queue 的消息？

Kafka 在上述问题上，做出了正确的设计：一个完全基于磁盘分布式 Commit Log 的消息服务器。

EMQ 2.0 版本将发布 EMQ X 平台产品，支持消息持久化到 Redis、Kafka、Cassandra、PostgreSQL 等数据库。

设计上分离消息路由与消息存储职责后，数据复制容灾备份甚至应用集成，可以在数据层面灵活实现。

### NetSplit 问题

EMQ 1.0 消息服务器集群，基于 Mnesia 数据库设计。NetSplit 发生时，节点间状态是：Erlang 节点间可以连通，互相询问自己是否宕机，对方回答你已经宕机:(

NetSplit 故障发生时，EMQ 消息服务器的 log/emqttd_error.log 日志，会打印 critical 级别日志:

    Mnesia inconsistent_database event: running_partitioned_network, emqttd@host

EMQ 集群部署在同一 IDC 网络下，NetSplit 发生的几率很低，一旦发生又很难自动处理。所以 EMQ 1.0 版本设计选择是，集群不自动化处理 NetSplit，需要人工重启部分节点。

## 系统架构

### 概念模型

EMQ 消息服务器概念上更像一台网络路由器(Router)或交换机(Switch)，而不是传统的企业级消息服务器(MQ)。相比网络路由器按 IP 地址或 MPLS 标签路由报文，EMQ 按主题树(Topic Trie)发布订阅模式在集群节点间路由 MQTT 消息:

![image](./_static/images/concept.png)

### 设计原则

1. EMQ 消息服务器核心解决的问题：处理海量的并发 MQTT 连接与路由消息。
2. 充分利用 Erlang/OTP 平台软实时、低延时、高并发、分布容错的优势。
3. 连接(Connection)、会话(Session)、路由(Router)、集群(Cluster)分层。
4. 消息路由平面(Flow Plane)与控制管理平面(Control Plane)分离。
5. 支持后端数据库或 NoSQL 实现数据持久化、容灾备份与应用集成。

### 系统分层

1. 连接层(Connection Layer)：负责 TCP 连接处理、 MQTT 协议编解码。
2. 会话层(Session Layer)：处理 MQTT 协议发布订阅消息交互流程。
3. 路由层(Route Layer)：节点内路由派发 MQTT 消息。
4. 分布层(Distributed Layer)：分布节点间路由 MQTT 消息。
5. 认证与访问控制(ACL)：连接层支持可扩展的认证与访问控制模块。
6. 钩子(Hooks)与插件(Plugins)：系统每层提供可扩展的钩子，支持插件方式扩展服务器。

## 连接层设计

连接层处理服务端 Socket 连接与 MQTT 协议编解码：

1. 基于 [ eSockd ](https://github.com/emqtt/esockd) 框架的异步 TCP 服务端
2. TCP Acceptor 池与异步 TCP Accept
3. TCP/SSL, WebSocket/SSL 连接支持
4. 最大并发连接数限制
5. 基于 IP 地址(CIDR)访问控制
6. 基于 Leaky Bucket 的流控
7. MQTT 协议编解码
8. MQTT 协议心跳检测
9. MQTT 协议报文处理

## 会话层设计

会话层处理 MQTT 协议发布订阅(Publish/Subscribe)业务交互流程：

1. 缓存 MQTT 客户端的全部订阅(Subscription)，并终结订阅 QoS
2. 处理 Qos0/1/2 消息接收与下发，消息超时重传与离线消息保存
3. 飞行窗口(Inflight Window)，下发消息吞吐控制与顺序保证
4. 保存服务器发送到客户端的，已发送未确认的 Qos1/2 消息
5. 缓存客户端发送到服务端，未接收到 PUBREL 的 QoS2 消息
6. 客户端离线时，保存持久会话的离线 Qos1/2 消息

### 消息队列与飞行窗口

会话层通过一个内存消息队列和飞行窗口处理下发消息:

          |<----------------- Max Len ----------------->|
          -----------------------------------------------
    IN -> |      Messages Queue   |  Inflight Window    | -> Out
          -----------------------------------------------
                                  |<---   Win Size  --->|

飞行窗口(Inflight Window)保存当前正在发送未确认的 Qos1/2 消息。窗口值越大，吞吐越高；窗口值越小，消息顺序越严格。

当客户端离线或者飞行窗口(Inflight Window)满时，消息缓存到队列。如果消息队列满，先丢弃 Qos0 消息或最早进入队列的消息。

### 报文 ID 与消息 ID

MQTT 协议定义了一个 16bits 的报文 ID(PacketId)，用于客户端到服务器的报文收发与确认。MQTT 发布报文(PUBLISH)进入消息服务器后，转换为一个消息对象并分配 128bits 消息 ID(MessageId)。

全局唯一时间序列消息 ID 结构：

1. 64bits 时间戳: erlang:system_time if Erlang >= R18, otherwise os:timestamp
2. Erlang 节点 ID: 编码为 2 字节
3. Erlang 进程 PID: 编码为 4 字节
4. 进程内部序列号: 2 字节的进程内部序列号

端到端消息发布订阅(Pub/Sub)过程中，发布报文 ID 与报文 QoS 终结在会话层，由唯一 ID 标识的 MQTT 消息对象在节点间路由:

    PktId <-- Session --> MsgId <-- Router --> MsgId <-- Session --> PktId

## 路由层设计

路由层维护订阅者(subscriber)与订阅关系表(subscription)，并在本节点发布订阅模式派发(Dispatch)消息:

![image](./_static/images/dispatch.png)

消息派发到会话(Session)后，由会话负责按不同 QoS 送达消息。

## 分布层设计

分布层维护全局主题树(Topic Trie)与路由表(Route Table)。主题树由通配主题构成，路由表映射主题到节点:

    -------------------------
    |            t          |
    |           / \         |
    |          +   #        |
    |        /  \           |
    |      x      y         |
    -------------------------
    | t/+/x -> node1, node3 |
    | t/+/y -> node1        |
    | t/#   -> node2        |
    | t/a   -> node3        |
    -------------------------

分布层通过匹配主题树(Topic Trie)和查找路由表(Route Table)，在集群的节点间转发路由 MQTT 消息:

![image](./_static/images/route.png)

## 认证与访问控制设计

EMQ 消息服务器支持可扩展的认证与访问控制，由 emqttd_access_control、emqttd_auth_mod 和 emqttd_acl_mod 模块实现。

emqttd_access_control 模块提供了注册认证扩展接口:

    register_mod(auth | acl, atom(), list()) -> ok | {error, any()}.

    register_mod(auth | acl, atom(), list(), non_neg_integer()) -> ok | {error, any()}.

### 认证扩展模块

emqttd_auth_mod 定义认证扩展模块 Behavihour:

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

EMQ 消息服务器自身实现的认证模块/插件包括:

| 模块/插件         | 认证方式               |
| ----------------- | ---------------------- |
| emq_auth_username | 用户名、密码认证插件   |
| emq_auth_clientid | ClientID、密码认证插件 |

### 访问控制(ACL)

emqttd_acl_mod 模块定义访问控制 Behavihour:

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

emqttd_acl_internal 模块实现缺省的基于 etc/acl.conf 文件的访问控制:

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

## 钩子(Hook)设计

### 钩子(Hook)定义

EMQ 消息服务器在客户端上下线、主题订阅、消息收发位置设计了扩展钩子(Hook):

| 钩子                 | 说明                 |
| -------------------- | -------------------- |
| client.connected     | 客户端上线           |
| client.subscribe     | 客户端订阅主题前     |
| client.unsubscribe   | 客户端取消订阅主题   |
| session.subscribed   | 客户端订阅主题后     |
| session.unsubscribed | 客户端取消订阅主题后 |
| message.publish      | MQTT 消息发布        |
| message.delivered    | MQTT 消息送达        |
| message.acked        | MQTT 消息回执        |
| client.disconnected  | 客户端连接断开       |

钩子(Hook) 采用职责链设计模式( [ Chain-of-responsibility_pattern ](https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern) )，扩展模块或插件向钩子注册回调函数，系统在客户端上下线、主题订阅或消息发布确认时，触发钩子顺序执行回调函数:

                     --------  ok | {ok, NewAcc}   --------  ok | {ok, NewAcc}   --------
     (Args, Acc) --> | Fun1 | -------------------> | Fun2 | -------------------> | Fun3 | --> {ok, Acc} | {stop, Acc}
                     --------                      --------                      --------
                        |                             |                             |
                   stop | {stop, NewAcc}         stop | {stop, NewAcc}         stop | {stop, NewAcc}

不同钩子的回调函数输入参数不同，用户可参考插件模版的 emqttd_plugin_template 模块，每个回调函数应该返回:

| 返回           | 说明                 |
| -------------- | -------------------- |
| ok             | 继续执行             |
| {ok, NewAcc}   | 返回累积参数继续执行 |
| stop           | 停止执行             |
| {stop, NewAcc} | 返回累积参数停止执行 |

### 钩子(Hook)实现

emqttd 模块封装了 Hook 接口:

    -module(emqttd).

    %% Hooks API
    -export([hook/4, hook/3, unhook/2, run_hooks/3]).
    hook(Hook :: atom(), Callback :: function(), InitArgs :: list(any())) -> ok | {error, any()}.

    hook(Hook :: atom(), Callback :: function(), InitArgs :: list(any()), Priority :: integer()) -> ok | {error, any()}.

    unhook(Hook :: atom(), Callback :: function()) -> ok | {error, any()}.

    run_hooks(Hook :: atom(), Args :: list(any()), Acc :: any()) -> {ok | stop, any()}.

emqttd_hook 模块实现 Hook 机制:

    -module(emqttd_hook).

    %% Hooks API
    -export([add/3, add/4, delete/2, run/3, lookup/1]).

    add(HookPoint :: atom(), Callback :: function(), InitArgs :: list(any())) -> ok.

    add(HookPoint :: atom(), Callback :: function(), InitArgs :: list(any()), Priority :: integer()) -> ok.

    delete(HookPoint :: atom(), Callback :: function()) -> ok.

    run(HookPoint :: atom(), Args :: list(any()), Acc :: any()) -> any().

    lookup(HookPoint :: atom()) -> [#callback{}].

### 钩子(Hook)使用

[ emq_plugin_template ](https://github.com/emqtt/emqttd_plugin_template/blob/master/src/emqttd_plugin_template.erl) 提供了全部钩子的使用示例，例如端到端的消息处理回调:

    -module(emq_plugin_template).

    -export([load/1, unload/0]).

    -export([on_message_publish/2, on_message_delivered/4, on_message_acked/4]).

    load(Env) ->
        emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
        emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/4, [Env]),
        emqttd:hook('message.acked', fun ?MODULE:on_message_acked/4, [Env]).

    on_message_publish(Message, _Env) ->
        io:format("publish ~s~n", [emqttd_message:format(Message)]),
        {ok, Message}.

    on_message_delivered(ClientId, _Username, Message, _Env) ->
        io:format("delivered to client ~s: ~s~n", [ClientId, emqttd_message:format(Message)]),
        {ok, Message}.

    on_message_acked(ClientId, _Username, Message, _Env) ->
        io:format("client ~s acked: ~s~n", [ClientId, emqttd_message:format(Message)]),
        {ok, Message}.

    unload() ->
        emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2),
        emqttd:unhook('message.acked', fun ?MODULE:on_message_acked/4),
        emqttd:unhook('message.delivered', fun ?MODULE:on_message_delivered/4).

## 插件(Plugin)设计

插件是一个可以被动态加载的普通 Erlang 应用(Application)。插件主要通过钩子(Hook)机制扩展服务器功能，或通过注册扩展模块方式集成认证访问控制。

emqttd_plugins 模块实现插件机制，提供加载卸载插件 API :

    -module(emqttd_plugins).

    -export([load/1, unload/1]).

    %% @doc Load a Plugin
    load(PluginName :: atom()) -> ok | {error, any()}.

    %% @doc UnLoad a Plugin
    unload(PluginName :: atom()) -> ok | {error, any()}.

用户可通过 ./bin/emqttd_ctl 命令行加载卸载插件:

    ./bin/emqttd_ctl plugins load emq_auth_redis

    ./bin/emqttd_ctl plugins unload emq_auth_redis

开发者请参考模版插件: [ http://github.com/emqtt/emqttd_plugin_template ](http://github.com/emqtt/emqttd_plugin_template)

## Mnesia/ETS 表设计

| 表                 | 类型   | 描述                   |
| ------------------ | ------ | ---------------------- |
| mqtt_trie          | mnesia | Trie Table             |
| mqtt_trie_node     | mnesia | Trie Node Table        |
| mqtt_route         | mnesia | Global Route Table     |
| mqtt_local_route   | mnesia | Local Route Table      |
| mqtt_pubsub        | ets    | PubSub Tab             |
| mqtt_subscriber    | ets    | Subscriber Tab         |
| mqtt_subscription  | ets    | Subscription Tab       |
| mqtt_session       | mnesia | Global Session Table   |
| mqtt_local_session | ets    | Local Session Table    |
| mqtt_client        | ets    | Client Table           |
| mqtt_retained      | mnesia | Retained Message Table |

## Erlang 设计相关

1. 使用 Pool, Pool, Pool... 推荐 GProc 库: [ https://github.com/uwiger/gproc ](https://github.com/uwiger/gproc)
2. 异步，异步，异步消息...连接层到路由层异步消息，同步请求用于负载保护
3. 避免进程 Mailbox 累积消息，负载高的进程可以使用 gen_server2
4. 消息流经的 Socket 连接、会话进程必须 Hibernate，主动回收 binary 句柄
5. 多使用 Binary 数据，避免进程间内存复制
6. 使用 ETS, ETS, ETS... Message Passing vs ETS
7. 避免 ETS 表非键值字段 select, match
8. 避免大量数据 ETS 读写, 每次 ETS 读写会复制内存，可使用 lookup_element, update_counter
9. 适当开启 ETS 表 {write_concurrency, true}
10. 保护 Mnesia 数据库事务，尽量减少事务数量，避免事务过载(overload)
11. 避免 Mnesia 数据表索引，和非键值字段 match, select
