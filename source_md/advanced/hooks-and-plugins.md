---
# 标题
title: 钩子和插件
# 编写日期
date: 2020-02-18 17:15:26
# 作者 Github 名称
author: hjianbo
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref: undefined
---

# 钩子和插件

## 钩子

### 定义

**钩子(Hooks)** 是 **EMQ X** 消息服务器提供的一种机制，它通过拦截模块间的函数调用、消息传递、事件传递来修改或扩展系统功能。

在 **EMQ X** 中，钩子的执行采用职责链的设计模式 [Chain-of-responsibility-pattern](https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern)。职责链链的执行逻辑如下图所示：

![Hook Design](../../source/_static/images/design_10.png)

1. 该链的入参为只读的 `Args`与用于修改的参数 `Acc`
2. 该链无论以何种方式终止执行，其返回值均为新的 `Acc` 
3. 该链上一共注册了三个回调函数；分别为 `Fun1` `Fun2` `Fun3` 并按**顺序**执行
4. 回调函数通过返回 `ok` 或 `{ok, NewAcc}` 表示以 只读的`Args`和新的 `NewAcc` 为参数传递到下一个函数继续执行，直到所有回调函数执行完成
5. 回调函数也可以通过返回`stop` 或 `{stop, NewAcc}` 表示终止链的传递立即返回


**EMQ X** 的预置了大量的钩子，每个钩子都按上图的职责链的方式执行。

### 使用指南

**EMQ X** 消息服务器以一个客户端在其生命周期内的关键活动为基础，预置了大量的钩子。每个钩子都按照职责链的模式执行。目前系统中预置的钩子有：


| 名称                 | 说明         | 执行时机                                              |
| -------------------- | ------------ | ----------------------------------------------------- |
| client.connect       | 处理连接报文 | 服务端收到客户端的连接报文时                          |
| client.connack       | 下发连接应答 | 服务端准备下发连接应答报文时                          |
| client.connected     | 成功接入     | 客户端认证完成并成功接入系统后                        |
| client.disconnected  | 连接断开     | 客户端连接层在准备关闭时                              |
| client.authenticate  | 连接认证     | 执行完 `client.connect` 后                            |
| client.check_acl     | ACL 鉴权     | 执行实际 `PUB/SUB` 操作前                             |
| client.subscribe     | 订阅主题     | 收到订阅报文后，执行 `client.check_acl` 鉴权前        |
| client.unsubscribe   | 取消订阅     | 收到取消订阅报文后                                    |
| session.created      | 会话创建     | `client.connected` 执行完成，且创建新的会话后         |
| session.subscribed   | 会话订阅主题 | 完成订阅操作后                                        |
| session.unsubscribed | 会话取消订阅 | 完成取消订阅操作后                                    |
| session.resumed      | 会话恢复     | `client.connected` 执行完成，且成功恢复旧的会话信息后 |
| session.discarded    | 会话被移除   | 会话由于被**移除**而终止后                            |
| session.takeovered   | 会话被接管   | 会话由于被**接管**而终止后                            |
| session.terminated   | 会话终止     | 会话由于其他原因被终止后                              |
| message.publish      | 消息发布     | 服务端在发布（路由）消息前                            |
| message.delivered    | 消息投递     | 消息准备投递到客户端前                                |
| message.acked        | 消息回执     | 服务端在收到客户端发回的消息 ACK 后                   |
| message.dropped      | 消息丢弃     | 发布出的消息被丢弃后                                  |

许多的官方插件的功能实现，也依赖于以上的钩子，例如 [emqx_auth_http](https://github.com/emqx/emqx-auth-http) 依赖于 `client.authenticate` 和 `client.check_acl` 钩子，将 **连接认证** 和 **发布/订阅鉴权** 的请求发送到后端的 HTTP 服务，实现了 **EMQ X** 消息服务器与已有认证中心的对接。

如果你是在开发扩展插件中使用钩子，你应该**完全弄清楚每个钩子的作用，以及调用的时机，且不要在钩子内部执行阻塞的函数。**


#### 挂载与取消挂载

**EMQ X** 提供了 API 进行钩子的挂载：

``` erlang
%% Name: 钩子的名称（挂载点）如：'client.authenticate'
%% {Module, Function, Args}: 回调函数的模块、方法、和附加参数
%% Priority：优先级，整数; 不提供则默认为 0
emqx:hook(Name, {Module, Function, Args}, Priority).
```

挂载完成后，钩子会按优先级从大到小执行，同一优先级按挂载的先后顺序执行。所有官方插件所挂载的钩子的优先级都为默认值 0

取消挂载：

``` erlang
%% Name: 钩子的名称（挂载点）如：'client.authenticate'
%% {Module, Function}: 回调函数的模块、方法
emqx:unhook(Name, {Module, Function}).
```

#### 回调函数

回调函数入参及返回值要求，参考本节的附录一

## 插件

**EMQ X** 消息服务器发行包中，包含了大量的官方插件，提供了一些基础的、或各类扩展的功能。

**EMQ X** 官方提供的插件包括：

| 插件                                                                 | 配置文件                              | 说明                      |
| -------------------------------------------------------------------- | ------------------------------------- | ------------------------- |
| [emqx_dashboard](https://github.com/emqx/emqx-dashboard)             | etc/plugins/emqx_dashbord.conf        | Web 控制台插件 (默认加载) |
| [emqx_management](https://github.com/emqx/emqx-management)           | etc/plugins/emqx_management.conf      | HTTP API 与 CLI 管理插件  |
| [emqx_auth_clientid](https://github.com/emqx/emqx-auth-clientid)     | etc/plugins/emqx_auth_clientid.conf   | ClientId 认证插件         |
| [emqx_auth_username](https://github.com/emqx/emqx-auth-username)     | etc/plugins/emqx_auth_username.conf   | 用户名、密码认证插件      |
| [emqx_auth_jwt](https://github.com/emqx/emqx-auth-jwt)               | etc/plugins/emqx_auth_jwt.conf        | JWT 认证 / 访问控制       |
| [emqx_auth_ldap](https://github.com/emqx/emqx-auth-ldap)             | etc/plugins/emqx_auth_ldap.conf       | LDAP 认证 / 访问控制      |
| [emqx_auth_http](https://github.com/emqx/emqx-auth-http)             | etc/plugins/emqx_auth_http.conf       | HTTP 认证 / 访问控制      |
| [emqx_auth_mongo](https://github.com/emqx/emqx-auth-mongo)           | etc/plugins/emqx_auth_mongo.conf      | MongoDB 认证 / 访问控制   |
| [emqx_auth_mysql](https://github.com/emqx/emqx-auth-mysql)           | etc/plugins/emqx_auth_mysql.conf      | MySQL 认证 / 访问控制     |
| [emqx_auth_pgsql](https://github.com/emqx/emqx-auth-pgsql)           | etc/plugins/emqx_auth_pgsql.conf      | PostgreSQL 认证 / 访问控制|
| [emqx_auth_redis](https://github.com/emqx/emqx-auth-redis)           | etc/plugins/emqx_auth_redis.conf      | Redis 认证 / 访问控制     |
| [emqx_psk_file](https://github.com/emqx/emqx-psk-file)               | etc/plugins/emqx_psk_file.conf        | PSK 支持                  |
| [emqx_web_hook](https://github.com/emqx/emqx-web-hook)               | etc/plugins/emqx_web_hook.conf        | Web Hook 插件             |
| [emqx_lua_hook](https://github.com/emqx/emqx-lua-hook)               | etc/plugins/emqx_lua_hook.conf        | Lua Hook 插件             |
| [emqx_retainer](https://github.com/emqx/emqx-retainer)               | etc/plugins/emqx_retainer.conf        | Retain 消息存储模块       |
| [emqx_rule_engine](https://github.com/emqx/emqx-rule-engine)         | etc/plugins/emqx_rule_engine.conf     | 规则引擎                  |
| [emqx_bridge_mqtt](https://github.com/emqx/emqx-bridge-mqtt)         | etc/plugins/emqx_bridge_mqtt.conf     | MQTT 消息桥接插件         |
| [emqx_delayed_publish](https://github.com/emqx/emqx-delayed-publish) | etc/plugins/emqx_delayed_publish.conf | 客户端延时发布消息支持    |
| [emqx_coap](https://github.com/emqx/emqx-coap)                       | etc/plugins/emqx_coap.conf            | CoAP 协议支持             |
| [emqx_lwm2m](https://github.com/emqx/emqx-lwm2m)                     | etc/plugins/emqx_lwm2m.conf           | LwM2M 协议支持            |
| [emqx_sn](https://github.com/emqx/emqx-sn)                           | etc/plugins/emqx_sn.conf              | MQTT-SN 协议支持          |
| [emqx_stomp](https://github.com/emqx/emqx-stomp)                     | etc/plugins/emqx_stomp.conf           | Stomp 协议支持            |
| [emqx_recon](https://github.com/emqx/emqx-recon)                     | etc/plugins/emqx_recon.conf           | Recon 性能调试            |
| [emqx_reloader](https://github.com/emqx/emqx-reloader)               | etc/plugins/emqx_reloader.conf        | Reloader 代码热加载插件   |
| [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template) | etc/plugins/emqx_plugin_template.conf | 插件开发模版              |

### 启停插件

其中插件的加载有四种方式：

1.  启动时默认加载
2.  命令行启停插件
3.  使用 Dashboard 启停插件
4.  调用管理 API 启停插件


**开启默认加载**

如需在系统启动时就默认启动某插件，则直接在 `data/loaded_plugins` 添加需要启动的插件名称，例如自动加载的插件有：

``` erlang
emqx_management.
emqx_rule_engine.
emqx_recon.
emqx_retainer.
emqx_dashboard.
```


**命令行启停插件**

在运行过程中，我们可以通过 CLI 命令的方式查看可用的插件列表、和启停某插件：

``` bash
## 显示所有可用的插件列表
./bin/emqx_ctl plugins list

## 加载某插件
./bin/emqx_ctl plugins load emqx_auth_username

## 卸载某插件
./bin/emqx_ctl plugins unload emqx_auth_username

## 重新加载某插件
./bin/emqx_ctl plugins reload emqx_auth_username
```


**使用 Dashboard 启停插件**

若开启了 Dashbord 的插件，可以直接通过访问 `http://localhost:18083/plugins` 中的插件管理页面启停插件。


### 插件开发

#### 创建插件项目

参考 [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template) 插件模版创建新的插件项目。

> 备注： 在 `<plugin name>_app.erl` 文件中必须加上标签 `-emqx_plugin (?MODULE).` 以表明这是一个 EMQ X 的插件。


#### 创建认证 / 访问控制模块


接入认证示例代码 `- emqx_auth_demo.erl`

``` erlang
-module (emqx_auth_demo).

-export ([ init/1
         , check/2
         , description/0
         ]).

init (Opts) -> {ok, Opts}.

check (_ClientInfo = #{clientid := ClientId, username := Username, password := Password}, _State) ->
    io:format ("Auth Demo: clientId=~p, username=~p, password=~p~n", [ClientId, Username, Password]),
    ok.

description () -> "Auth Demo Module".
```


访问控制示例代码 `- emqx_acl_demo.erl`

``` erlang
-module (emqx_acl_demo).

-include_lib ("emqx/include/emqx.hrl").

%% ACL callbacks
-export ([ init/1
        , check_acl/5
        , reload_acl/1
        , description/0
        ]).

init (Opts) ->
    {ok, Opts}.

check_acl ({ClientInfo, PubSub, _NoMatchAction, Topic}, _State) ->
    io:format ("ACL Demo: ~p ~p ~p~n", [ClientInfo, PubSub, Topic]),
    allow.

reload_acl (_State) ->
    ok.

description () -> "ACL Demo Module".
```


挂载认证、访问控制钩子示例代码 `- emqx_plugin_template_app.erl`

``` erlang
ok = emqx:hook ('client.authenticate', fun emqx_auth_demo:check/2, []),
ok = emqx:hook ('client.check_acl', fun emqx_acl_demo:check_acl/5, []).
```


#### 挂载钩子

在扩展插件中，可通过挂载钩子来处理客户端上下线、主题订阅、消息收发等事件。


钩子挂载示例代码 `- emqx_plugin_template.erl`:

``` erlang
load (Env) ->
    emqx:hook ('client.connect',      {?MODULE, on_client_connect, [Env]}),
    emqx:hook ('client.connack',      {?MODULE, on_client_connack, [Env]}),
    emqx:hook ('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook ('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    emqx:hook ('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
    emqx:hook ('client.check_acl',    {?MODULE, on_client_check_acl, [Env]}),
    emqx:hook ('client.subscribe',    {?MODULE, on_client_subscribe, [Env]}),
    emqx:hook ('client.unsubscribe',  {?MODULE, on_client_unsubscribe, [Env]}),
    emqx:hook ('session.created',     {?MODULE, on_session_created, [Env]}),
    emqx:hook ('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    emqx:hook ('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
    emqx:hook ('session.resumed',     {?MODULE, on_session_resumed, [Env]}),
    emqx:hook ('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
    emqx:hook ('session.takeovered',  {?MODULE, on_session_takeovered, [Env]}),
    emqx:hook ('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
    emqx:hook ('message.publish',     {?MODULE, on_message_publish, [Env]}),
    emqx:hook ('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
    emqx:hook ('message.acked',       {?MODULE, on_message_acked, [Env]}),
    emqx:hook ('message.dropped',     {?MODULE, on_message_dropped, [Env]}).
```


#### 注册 CLI 命令


处理命令行命令示例代码 `- emqx_cli_demo.erl`：

``` erlang
-module (emqx_cli_demo).

-export ([cmd/1]).

cmd (["arg1", "arg2"]) ->
    emqx_cli:print ("ok");

cmd (_) ->
    emqx_cli:usage ([{"cmd arg1 arg2", "cmd demo"}]).
```


注册命令行示例代码 `- emqx_plugin_template_app.erl`：

``` erlang
ok = emqx_ctl:register_command (cmd, {emqx_cli_demo, cmd}, []),
```

插件加载后，使用`./bin/emqx_ctl` 验证新增的命令行：

``` bash
./bin/emqx_ctl cmd arg1 arg2
```

#### 插件配置文件

插件自带配置文件放置在 `etc/${plugin_name}.conf|config`。 **EMQ X** 支持两种插件配置格式：

1. Erlang 原生配置文件格式 - `${plugin_name}.config`：
   
    ```erlang
    [
      {plugin_name, [
        {key, value}
      ]}
    ].
   ```
   
2. sysctl 的 `k = v` 通用格式 - `${plugin_name}.conf`：
   
    ```erlang
    plugin_name.key = value
    ```

> 注：`k = v` 格式配置需要插件开发者创建 `priv/plugin_name.schema` 映射文件。


#### 编译和发布插件

1. clone emqx-rel 项目：

``` bash
git clone https://github.com/emqx/emqx-rel.git
```

2. rebar.config 添加依赖：

``` erlang
{deps,
   [ {plugin_name, {git, "url_of_plugin", {tag, "tag_of_plugin"}}}
   , ....
   ....
   ]
}
```

3. rebar.config 中 relx 段落添加：

``` erlang
{relx,
    [...
    , ...
    , {release, {emqx, git_describe},
       [
         {plugin_name, load},
       ]
      }
    ]
}
```


###  附录一:  Hooks 入参及返回值速查表


| 名称                 | 入参                                                         | 返回                |
| -------------------- | ------------------------------------------------------------ | ------------------- |
| client.connect       | `ConnInfo`：客户端连接层参数<br>`Props`：MQTTv5 连接报文的 Properties 属性 | 新的 `Props`        |
| client.connack       | `ConnInfo`：客户端连接层参数 <br>`Rc`：返回码<br>`Props`: MQTTv5 连接应答报文的 Properties 属性 | 新的 `Props`        |
| client.connected     | `ClientInfo`:  客户端信息参数<br>`ConnInfo`： 客户端连接层参数 | -                   |
| client.disconnected  | `ClientInfo`：客户端信息参数<br>`ConnInfo`：客户端连接层参数<br>`ReasonCode`：错误码 | -                   |
| client.authenticate  | `ClientInfo`：客户端信息参数<br>`AuthResult`：认证结果       | 新的 `AuthResult`   |
| client.check_acl     | `ClientInfo`：客户端信息参数<br>`Topic`：发布/订阅的主题<br>`PubSub`:  发布或订阅<br>`ACLResult`：鉴权结果 | 新的 `ACLResult`    |
| client.subscribe     | `ClientInfo`：客户端信息参数<br/>`Props`：MQTTv5订阅报文的 Properties 参数<br>`TopicFilters`：需订阅的主题列表 | 新的 `TopicFilters` |
| client.unsubscribe   | `ClientInfo`：客户端信息参数<br/>`Props`：MQTTv5取消订阅报文的 Properties 参数<br/>`TopicFilters`：需取消订阅的主题列表 | 新的 `TopicFilters` |
| session.created      | `ClientInfo`：客户端信息参数<br/>`SessInfo`：会话信息        | -                   |
| session.subscribed   | `ClientInfo`：客户端信息参数<br/>`Topic`：订阅的主题<br>`SubOpts`：订阅操作的配置选项 | -                   |
| session.unsubscribed | `ClientInfo`：客户端信息参数<br/>`Topic`：取消订阅的主题<br/>`SubOpts`：取消订阅操作的配置选项 | -                   |
| session.resumed      | `ClientInfo`：客户端信息参数<br/>`SessInfo`：会话信息        | -                   |
| session.discarded    | `ClientInfo`：客户端信息参数<br/>`SessInfo`：会话信息        | -                   |
| session.takeovered   | `ClientInfo`：客户端信息参数<br/>`SessInfo`：会话信息        |                     |
| session.terminated   | `ClientInfo`：客户端信息参数<br/>`Reason`：终止原因 <br>`SessInfo`：会话信息 | -                   |
| message.publish      | `Message`：消息对象                                          | 新的 `Message`      |
| message.delivered    | `ClientInfo`：客户端信息参数<br/>`Message`：消息对象         | 新的 `Message`      |
| message.acked        | `ClientInfo`：客户端信息参数<br/>`Message`：消息对象         | -                   |
| message.dropped      | `Message`：消息对象<br>`By`：被谁丢弃<br>`Reason`：丢弃原因  | -                   |

