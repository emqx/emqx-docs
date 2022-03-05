# Plugins

The EMQX Broker distribution contains a large number of official plug-ins, which provide some basic or various extended functions.

They rely on the code API of  [emqx](https://github.com/emqx/emqx) or [hooks](hooks.md) for their special functions.

You can compile it with the [emqx](https://github.com/emqx/emqx) core project and package it into a working package.

::: tip
EMQX Enterprise edition customers are not provided with the source code.
EMQ provides no supports for customer developed plugins in their development, test or integration work.
:::

## List of plugins

 The official plug-ins provided by EMQX include:

| Plugin                                                       | Configuration file                    | Description                        |
| ------------------------------------------------------------ | ------------------------------------- | ---------------------------------- |
| [emqx_dashboard](https://github.com/emqx/emqx/tree/main-v4.3/lib-ce/emqx_dashboard)   | etc/plugins/emqx_dashboard.conf       | Web dashboard Plugin (Default)     |
| [emqx_management](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_management)   | etc/plugins/emqx_management.conf      | HTTP API and CLI Management Plugin |
| [emqx_auth_mnesia](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_mnesia) | etc/plugins/emqx_auth_mnesia.conf     | Mnesia Auth/access control         |
| [emqx_auth_jwt](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_jwt)       | etc/plugins/emqx_auth_jwt.conf        | JWT Auth/access control            |
| [emqx_auth_ldap](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_ldap)     | etc/plugins/emqx_auth_ldap.conf       | LDAP Auth/access control           |
| [emqx_auth_http](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_http)     | etc/plugins/emqx_auth_http.conf       | HTTP Auth/access control           |
| [emqx_auth_mongo](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_mongo)   | etc/plugins/emqx_auth_mongo.conf      | MongoDB Auth/access control        |
| [emqx_auth_mysql](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_mysql)   | etc/plugins/emqx_auth_mysql.conf      | MySQL Auth/access control          |
| [emqx_auth_pgsql](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_pgsql)   | etc/plugins/emqx_auth_pgsql.conf      | PostgreSQL Auth/access control     |
| [emqx_auth_redis](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_auth_redis)   | etc/plugins/emqx_auth_redis.conf      | Redis Auth/access control          |
| [emqx_psk_file](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_psk_file)       | etc/plugins/emqx_psk_file.conf        | PSK support                        |
| [emqx_web_hook](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_web_hook)       | etc/plugins/emqx_web_hook.conf        | Web Hook Plugin                    |
| [emqx_lua_hook](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_lua_hook)       | etc/plugins/emqx_lua_hook.conf        | Lua Hook Plugin                    |
| [emqx_retainer](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_retainer)       | etc/plugins/emqx_retainer.conf        | Retain Message storage module      |
| [emqx_rule_engine](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_rule_engine) | etc/plugins/emqx_rule_engine.conf     | Rule engine                        |
| [emqx_bridge_mqtt](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_bridge_mqtt) | etc/plugins/emqx_bridge_mqtt.conf     | MQTT Message Bridge Plugin         |
| [emqx_coap](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_coap)               | etc/plugins/emqx_coap.conf            | CoAP protocol support              |
| [emqx_lwm2m](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_lwm2m)             | etc/plugins/emqx_lwm2m.conf           | LwM2M protocol support             |
| [emqx_sn](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_sn)                   | etc/plugins/emqx_sn.conf              | MQTT-SN protocol support           |
| [emqx_stomp](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_stomp)             | etc/plugins/emqx_stomp.conf           | Stomp protocol support             |
| [emqx_recon](https://github.com/emqx/emqx/tree/main-v4.3/apps/emqx_recon)             | etc/plugins/emqx_recon.conf           | Recon performance debugging        |
| [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template)               | etc/plugins/emqx_plugin_template.conf | plugin develop template            |

## Start and stop plugin

 There are four ways to load plugins: 

1.  Default loading
2.  Start and stop plugin on command line
3.  Start and stop plugin on Dashboard
4.  Start and stop plugin by calling management API


 **Default loading** 

 If a plugin needs to start with the broker, add this plugin in `data/loaded_plugins`.  

 For example, the plugins that are loaded by default are: 

```erlang
{emqx_management, true}.
{emqx_recon, true}.
{emqx_retainer, true}.
{emqx_dashboard, true}.
{emqx_rule_engine, true}.
{emqx_bridge_mqtt, false}.
```


 **Start and stop plugin on command line**

When the EMQX is running, plugins can be checked, loaded/unloaded by [CLI - Load/Unload Plugin](cli.md#load_plugin):

 **Start and stop plugin on Dashboard**

If Dashboard plugin is started (by default), the plugins can be start or stopped by visiting the managing page that can be found under `http://localhost:18083/plugins`.

**Start and stop plugins using management API**

When EMQX Broker is running, you can view, start and stop a plugin through [Managing and Monitoring API - Load Plugin](http-api.md#load_plugin).

## Plugin development

### Create plugin project

Refer to the [emqx_plugin_template](https://github.com/emqx/emqx-plugin-template) plugin template to create a new plugin project.

::: tip Tip
The tag of` -emqx_plugin (? MODULE)`should be added to `<plugin name>_app.erl` file to indicate that this is an EMQX Broker plugin.
:::


### Create Authentication / Access Control Module


Authentication/Access sample code - `emqx_auth_demo.erl`：

```erlang
-module(emqx_auth_demo).

-export([ init/1
        , check/2
        , description/0
        ]).

init(Opts) -> {ok, Opts}.

check(_ClientInfo = #{clientid := ClientId, username := Username, password := Password}, _State) ->
    io:format("Auth Demo: clientId=~p, username=~p, password=~p~n", [ClientId, Username, Password]),
    ok.

description() -> "Auth Demo Module".
```


Access control sample code - `emqx_acl_demo.erl`：

```erlang
-module(emqx_acl_demo).

-include_lib("emqx/include/emqx.hrl").

%% ACL callbacks
-export([ init/1
        , check_acl/5
        , reload_acl/1
        , description/0
        ]).

init(Opts) ->
    {ok, Opts}.

check_acl({ClientInfo, PubSub, _NoMatchAction, Topic}, _State) ->
    io:format("ACL Demo: ~p ~p ~p~n", [ClientInfo, PubSub, Topic]),
    allow.

reload_acl(_State) ->
    ok.

description() -> "ACL Demo Module".
```


Example code for mounting authentication and access control hooks - `emqx_plugin_template_app.erl`：

```erlang
ok = emqx:hook('client.authenticate', fun emqx_auth_demo:check/2, []),
ok = emqx:hook('client.check_acl', fun emqx_acl_demo:check_acl/5, []).
```


### Load hook

During the plugin extension, you can load [hooks](hooks.md) to handle events such as client online and offline, topic subscription, and message sending and receiving.

Hook load sample code - `emqx_plugin_template.erl`：

```erlang
load(Env) ->
    emqx:hook('client.connect',      {?MODULE, on_client_connect, [Env]}),
    emqx:hook('client.connack',      {?MODULE, on_client_connack, [Env]}),
    emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    emqx:hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
    emqx:hook('client.check_acl',    {?MODULE, on_client_check_acl, [Env]}),
    emqx:hook('client.subscribe',    {?MODULE, on_client_subscribe, [Env]}),
    emqx:hook('client.unsubscribe',  {?MODULE, on_client_unsubscribe, [Env]}),
    emqx:hook('session.created',     {?MODULE, on_session_created, [Env]}),
    emqx:hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    emqx:hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
    emqx:hook('session.resumed',     {?MODULE, on_session_resumed, [Env]}),
    emqx:hook('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
    emqx:hook('session.takeovered',  {?MODULE, on_session_takeovered, [Env]}),
    emqx:hook('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
    emqx:hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
    emqx:hook('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
    emqx:hook('message.acked',       {?MODULE, on_message_acked, [Env]}),
    emqx:hook('message.dropped',     {?MODULE, on_message_dropped, [Env]}).
```


### Register CLI commands

Processing command line sample code - `emqx_cli_demo.erl`：

```erlang
-module(emqx_cli_demo).

-export([cmd/1]).

cmd(["arg1", "arg2"]) ->
    emqx_cli:print ("ok");

cmd(_) ->
    emqx_cli:usage ([{"cmd arg1 arg2", "cmd demo"}]).
```

Register command line sample code - `emqx_plugin_template_app.erl`：

```erlang
ok = emqx_ctl:register_command(cmd, {emqx_cli_demo, cmd}, []),
```

After the plugin is loaded, use `./bin/emqx_ctl`  to verify the new command line:

```bash
./bin/emqx_ctl cmd arg1 arg2
```

### Plugin configuration file

Plug-in configuration files are placed in  `etc/${plugin_name}.conf|config`. EMQX Broker supports two plugin configuration formats:

1. Erlang native configuration file format-`${plugin_name}.config`:
```erlang
[
    {plugin_name, [
    {key, value}
    ]}
].
```

2. Common format of `k = v`   for sysctl-`${plugin_name}.conf`:

```erlang
plugin_name.key = value
```

::: tip Tip
`k = v` format configuration requires the plugin developer to create `priv/plugin_name.schema` mapping file.
:::


### Compile and publish the plugin

#### clone emqx project

```bash
git clone https://github.com/emqx/emqx.git
```

Add `plugin_name` as a dependency by describing it in `lib-extra/plugins`:

```erlang
{erlang_plugins,
   [ {plugin_name, {git, "url_of_plugin", {tag, "tag_of_plugin"}}}
   , ....
   ....
   ]
}
```

#### Build a release

```
$ export EMQX_EXTRA_PLUGINS=plugin_name
$ make
```

#### Run your code

Start the node (interactive mode)
```
./_build/emqx/rel/emqx/bin/emqx console
```

Load the plugin with the command:
```
./_build/emqx/rel/emqx/bin/emqx_ctl plugins load plugin_name
```

::: tip Tip
To have the plugin enabled/loaded by default, you can include it in `data/loaded_plugins.tmpl`.
:::
