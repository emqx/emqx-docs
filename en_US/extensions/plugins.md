# Plugins

EMQX allow users to customize the business logic or implement other protocols using plugins. In this chapter, you will learn how to develop plugins.

The basic process of plugin development and operation is as follows:

- Generate the corresponding plugin tar.gz package through the plugin template provided by EMQX.
- Install the plugin packages via Dashboard or CLI.
- Start/stop/uninstall plugins via Dashboard or CLI.

:::tip
Prerequisites

- Knowledge of EMQX [hooks](./hooks.md)
  :::

## Develop EMQX plugins

EMQX offers an [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template), which allows you to further customize your plugins. In the section below, we will use the access control plugin as an example to give you a step-by-step guide to plugin development. 

### 1. Download the plugin template

To download the  [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template), run: 

```
git clone https://github.com/emqx/emqx-plugin-template
```

As we can see from the directory structure, this is a standard Erlang application. 

```sh
$ git clone https://github.com/emqx/emqx-plugin-template
$ ls
LICENSE                  _build                   get-rebar3
Makefile                 check-vsn.sh             priv
README.md                rebar.config             src
```

### 2. Test the compile environment

Run `make rel` to test whether the plugin can be successfully compiled and packed. No coding is needed. 

The plugins under development need to use the functions of the main EMQX, so the program needs first to download the dependency and then compile the main project. Therefore, the first compilation usually takes a long time to complete.

For the compiling environment, see [Install from Source code](../deploy/install-source.md).

### 3. Mount the hook functions

Below is the directory structure:

```sh
> tree src/
src/
├── emqx_cli_demo.erl
├── emqx_plugin_template.app.src
├─ emqx_plugin_template.erl
├── emqx_plugin_template_app.erl
└── emqx_plugin_template_sup.erl
```

`emqx_plugins_template_app.erl` is the initiation portal of the application, where you can create your own supervision tree or load EMQX's hook functions. This template contains all hook functions, please remove the unnecessary hooks before using it.
In this example, we only need 2 hooks for authentication and access control, so we can modify `emqx_plugins_template:load/1` as follows:

```erlang
load(Env) ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, [Env]}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, [Env]}, ?HP_HIGHEST),
  ok.
```

We will use `on_client_authenticate/3` for client authentication and `on_client_authorize/5`  for access control. 

As one hook function may be mounted both by EMQX and customized plugins, we also need to specify the execution order when mounting it to the plugin.  `HP_HIGHEST` specifies that the current hook function has the highest priority and is executed first.

```erlang
%% Only allow connections with clientID name matching any of the following characters: A-Z, a-z, 0-9, and underscore.
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result, _Env) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.
%% Can only subscribe topic /room/{clientid}, but can send messages to any topics.
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result, _Env) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result, _Env) -> {ok, Result}.
```

In the above code example, we only allow clients with clientID matching the specification to log in. These clients can only subscribe to topic `/room/{clientid}`, thus building a simple chat room, that is, clients can send messages to any other clients, but each client can only subscribe to topics related to itself.

:::tip

1. Be sure to set `authorization.no_match` to `deny` in the configuration first, that is, EMQX will reject any unauthorized connection requests. 
2. In this example, we illustrate how to customize an access control plugin, you can also [set similar authorization rules based on File](../accesscontrol/../access-control/authz/file.md). 

​	:::

### 5. Pack the customized plugin

Modify the version information of the plugin via `rebar.config`:

```erlang
{relx, [ {release, {emqx_plugin_template, "5.0.0-rc.3"}, [emqx_plugin_template, map_sets]}
         , {dev_mode, false}
         , {include_erts, false}
         ]}.
  %% Additional info about the plugin
  {emqx_plugrel,
      [ {authors, ["EMQX Team"]}
      , {builder,
          [ {name, "EMQX Team"}
          , {contact, "emqx-support@emqx.io"}
          , {website, "www.emqx.com"}
          ]}
      , {repo, "https://github.com/emqx/emqx-plugin-template"}
      , {functionality, ["Demo"]}
      , {compatibility,
          [ {emqx, "~> 5.0"}
          ]}
      , {description, "This is a demo plugin"}
      ]
  }.
```
Rerun the packing command:
```sh
make rel
...
===> Release successfully assembled: _build/default/rel/emqx_plugin_template
===> [emqx_plugrel] creating _build/default/emqx_plugrel/emqx_plugin_template-5.0.0-rc.3.tar.gz  
```

The command will prompt to generate the plugin  `pluginname-version.tar.gz`.

### 6. Install/launch the plugin

Use CLI to install the compiled package: 

```bash
./bin/emqx_ctl plugins install {pluginName}
```

### 7. Uninstall the plugin

When you don't need the plugin, you can easily uninstall it with CLI:

```bash
./bin/emqx_ctl plugins uninstall {pluginName}
```

{%emqxee%}
Note: The plugins need to be reinstalled after hot upgrades. 
{%endemqxee%}
