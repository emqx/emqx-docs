# Plugins

EMQX allows users to customize the business logic or implement other protocols using plugins written in Erlang. In this chapter, you will learn how to develop such a custom plugin.

The basic process of plugin development and operation is as follows:

- Download and install our [rebar3 emqx-plugin template](https://github.com/emqx/emqx-plugin-template).
- Generate the corresponding plugin tarball using the plugin template provided by EMQX.
- Install the plugin package via Dashboard or CLI.
- Start/stop/uninstall your plugin via Dashboard or CLI.

:::tip
Prerequisites

- Knowledge of EMQX [hooks](./hooks.md)
  :::

## Develop EMQX Plugins

EMQX offers an [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template) to create a custom emqx-plugin project. In the section below, we will create a custom access control plugin as an example to give you a step-by-step guide to plugin development.

### Download, install and use the rebar3 plugin template

1. Download the  [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template), run:

```shell
$ mkdir -p ~/.config/rebar3/templates
$ pushd ~/.config/rebar3/templates
$ git clone https://github.com/emqx/emqx-plugin-template
$ popd
```

2. Now create your custom plugin from the template

```shell
$ rebar3 new emqx-plugin my_emqx_plugin
```

This will create a standard Erlang application with `emqx` as a dependency. Have a look at `rebar.config` and tune to your needs.

```shell
$ tree my_emqx_plugin
my_emqx_plugin
├── License
├── Makefile
├── README.md
├── check-svn.sh
├── erlang_ls.config
├── get-rebar3
├── priv
│   └── config.hocon
├── rebar.config
└── src
    ├── emqx_cli_demo.erl
    ├── my_emqx_plugin.app.src
    ├── my_emqx_plugin.erl
    ├── my_emqx_plugin_app.erl
    └── my_emqx_plugin_sup.erl

3 directories, 13 files
```

It also ships an example module on how to add your own custom `emqx ctl` commands (`emqx_cli_demo`).

**Note** Since the example depends on `emqx` this plugin needs a customized version of `rebar3`, which will be installed using the provided `get-rebar3` script.

### Test your development environment

{% emqxce %}
:::tip
For a working development environment, see [Install from Source code](../deploy/install-source.md).
:::
{% endemqxce %}

Run `make rel` to test whether the plugin can be successfully compiled and packaged. No coding is needed at this point.

As the example plugin relies on the EMQX main application, it needs to be downloaded along with its dependencies and subsequently compiled as part of the main project. Please note that the compilation process may take a significant amount of time to finish.

### Customize the example project

Now that things are working, you can start customizing the project to suit your needs. We've provided a core module that registers all currently [known hooks](https://www.emqx.io/docs/en/v5.0/extensions/hooks.html). This code can be found in `src/my_emqx_plugin.erl`. You will want to remove all unused hooks and then fill in the callbacks for the remaining ones with your own custom code.

In the example below, we only need 2 hooks for authentication and access control, so we modify `my_emqx_plugin:load/1` as follows:

```erlang
load(Env) ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, [Env]}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, [Env]}, ?HP_HIGHEST),
  ok.
```

We will use `on_client_authenticate/3` for client authentication and `on_client_authorize/5`  for access control.

As one hook function may be mounted both by EMQX and customized plugins, we also need to specify the execution order when mounting it to the plugin.  `HP_HIGHEST` specifies that the current hook function has the highest priority and is executed first.

#### Customize access control code

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
2. In this example, we illustrate how to customize an access control plugin, you can also [set similar authorization rules based on File](../access-control/authz/file.md).

:::

**Pack the customized plugin**

Modify the version information of the plugin via `rebar.config`:

```erlang
{relx, [ {release, {my_emqx_plugin, "1.0.0"}, %% this is the release version, different from app vsn in .app file
            [ my_emqx_plugin
            , map_sets
            ]}
       , {dev_mode, false}
       , {include_erts, false}
       ]}.

  %% Additional info about the plugin
  {emqx_plugrel,
    [ {authors, ["Your Name"]}
    , {builder,
        [ {name, "Your Name"}
        , {contact, "your_email@example.cpm"}
        , {website, "http://example.com"}
        ]}
    , {repo, "https://github.com/emqx/emqx-plugin-template"}
    , {functionality, ["Demo"]}
    , {compatibility,
        [ {emqx, "~> 5.0"}
        ]}
    , {description, "Another amazing EMQX plugin"}
    ]
}..
```

Now rerun the release command:

```shell
make rel
...
===> Release successfully assembled: _build/default/rel/my_emqx_plugin
===> [emqx_plugrel] creating _build/default/emqx_plugrel/my_emqx_plugin-1.0.0.tar.gz
```

This created a new EMQX plugin tarball  `my_emqx_plugin-1.0.0.tar.gz` that you can now upload and install to your running EMQX cluster.

## Install/launch the plugin

Use CLI to install the compiled package:

```bash
./bin/emqx ctl plugins install {pluginName}
```

## Uninstall the plugin

When you don't need the plugin, you can easily uninstall it with CLI:

```bash
./bin/emqx ctl plugins uninstall {pluginName}
```

{% emqxee %}
**Note**: Plugins need to be reinstalled after hot upgrades.
{% endemqxee %}
