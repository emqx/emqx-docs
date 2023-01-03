# Plugins

Plugins are  Erlang applications written according to the EMQX Plugin specification. By customizing them, developers can extend arbitrary functionality within EMQX.

As long as the developer has basic Erlang development skills, it will be easy to develop plugins according to the following steps.

- Generate the plugin `tar.gz`  from the plugin template provided by EMQX.
- Install the plugin package through Dashboard.
- Start/stop/uninstall the plugin through Dashboard.

## Plugin development

  [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)  is a basic EMQX plugin template that you can directly download and modify to generate a standard plugin. Next we will implement a new authentication/access control feature from scratch with the plugin.

### Download template

```sh
$ git clone https://github.com/emqx/emqx-plugin-template
$ ls
LICENSE                  _build                   get-rebar3
Makefile                 check-vsn.sh             priv
README.md                rebar.config             src
```

 This is a standard Erlang Application application, It is recommended to have a quick look at the readme documentation.

### Compile Template

Run `make rel` directly to make release without changing any code. The first compilation process is long because it depends on the main emqx project (to facilitate the plugin to use various functions of the main project directly), you need to download the dependencies and compile the main project.

### Custom the hook functions

```sh
> tree src/
src/
├── emqx_cli_demo.erl
├── emqx_plugin_template.app.src
├─ emqx_plugin_template.erl
├── emqx_plugin_template_app.erl
└── emqx_plugin_template_sup.erl
```

`emqx_plugins_template_app.erl` is the application initiation portal. Here you can create your own supervision tree, load the EMQX's hook functions. All hook functions are registered in this template plugin, so please remove the unnecessary hooks before using it.
We only need to use 2 hooks for authentication and access control, so we can modify emqx_plugins_template:load/1 as follows:

```erlang
load(Env) ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, [Env]}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, [Env]}, ?HP_HIGHEST),
  ok.
```

Note: Our custom `on_client_authenticate/3` function is executed when the client authenticates, and `on_client_authorize/5` function is executed when the client does authorize(ACL access control) checks. we also specify the order of its execution, since EMQX also mounts hook functions internally, or developers may develop multiple plugins that mount the same hook functions,  `HP_HIGHEST` specifies that the current hook function has the highest priority and is executed first.

```erlang
%% Only allow connections with clientid of [A-Za-z0-9_] characters.
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result, _Env) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.
%% Only allow topics subscribed to /room/{clientid}, but can send messages to any topic.
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result, _Env) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result, _Env) -> {ok, Result}.
```

With the above 2 hook functions, we only let the clients whose clientid format matches the specification log in. And only subscribe to `/room/{clientid}`, so that a simple chat room feature is implemented.
  Clients can send messages to any client, but each client can only subscribe to topics related to itself.

  *TIPS*: Be sure to set `authorization.no_match` to `deny` in the configuration first, it defaults to `allow`.

```
authorization {
  no_match: deny
}
```

*PS*: The same rule can more commonly be implemented through [Authorization](../security/authz/authz.md).

### Packaged plugin
  Modify the version information of the plugin via `rebar.config` with:

```erlang
{relx, [ {release, {emqx_plugin_template, "5.0.0-rc.3"}, [emqx_plugin_template, map_sets]}
         , {dev_mode, false}
         , {include_erts, false}
         ]}.
  %% Additional info of the plugin
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

```sh
make rel
...
===> Release successfully assembled: _build/default/rel/emqx_plugin_template
===> [emqx_plugrel] creating _build/default/emqx_plugrel/emqx_plugin_template-5.0.0-rc.3.tar.gz  
```

The command will prompt to generate the plugin `name-version.tar.gz` version of the package.

### Install/launch plugin

<img src="./assets/plugins_upload.png" alt="test" style="zoom:80%;" />

<img src="./assets/plugins_start.png" alt="plugins_start" style="zoom:80%;" />

After confirming that all information about the plugin is correct, click the start button directly.

<img src="./assets/plugins_start_ok.png" alt="plugins_start_ok" style="zoom:80%;" />

Use `MQTTX` client to verify if the function is working: connection is rejected when clientId is `1$`.

<img src="./assets/connect_failed.png" alt="connect_failed" style="zoom:80%;" />

Next, you can try to send messages to each other after connecting with multiple `MQTTX` clients.

### Uninstall plugin

When you don't need the plugin, you can easily uninstall it from the Dashboard.
If you are upgrading a plugin, hot upgrade plugins are not  supported currently and need to be reinstalled on the Dashboard.

<img src="./assets/plugins_uninstall.png" alt="plugins_uninstall" style="zoom:80%;" />

