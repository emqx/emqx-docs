# 插件

EMQX 支持通过插件扩展自定义业务逻辑，或通过插件的协议扩展接口实现其他协议适配，本章节将指导您进行插件的开发。

插件开发和运行基本流程如下：

- 通过 EMQX 提供的插件模板生成对应的插件 tar.gz 包。
- 通过 Dashboard 或 CLI 安装插件包，
- 通过 Dashboard 或 CLI 启动/停止/卸载插件。

:::tip 前置条件：

- 了解 EMQX [钩子](./hooks.md)。
  :::

## 插件开发

EMQX 提供了一个基础的插件模板 [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)，你可以基于它开始开发您的插件。此处以访问控制插件为例，指导您一步步开发自己的插件：

### 1. 下载插件模板 emqx-plugin-template

```sh
git clone https://github.com/emqx/emqx-plugin-template
```

查看目录结构，这是一个标准的 Erlang Application 应用。

```sh
$ ls
LICENSE                  _build                   get-rebar3
Makefile                 check-vsn.sh             priv
README.md                rebar.config             src
```

### 2. 测试编译环境

不改任何代码，直接运行 `make rel` 测试是否可以正常编译打包插件。

第一次编译过程较长，因为它依赖于 EMQX 主项目（方便插件直接使用主项目的各种函数），需要下载依赖，并编译主项目。

编译环境请参照 [源码编译安装 EMQX](../deploy/install-source.md) 配置。

### 3. 挂载钩子函数

查看目录结构：

```sh
> tree src/
src/
├── emqx_cli_demo.erl
├── emqx_plugin_template.app.src
├── emqx_plugin_template.erl
├── emqx_plugin_template_app.erl
└── emqx_plugin_template_sup.erl
```

`emqx_plugins_template_app.erl` 是应用启动入口。你可以在这里定义需要启动的监控树，并挂载 EMQX 的钩子函数。此模板插件中注册了所有的钩子函数，**使用前请删除不需要的钩子**。

此示例中只需要使用到认证和授权 2 个钩子，可以修改`emqx_plugins_template:load/1`为：

```erlang
load(Env) ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, [Env]}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize',    {?MODULE, on_client_authorize, [Env]}, ?HP_HIGHEST),
  ok.
```

即：当客户端认证时执行我们自定义的 `on_client_authenticate/3` 函数，当客户端做授权控制检查时执行`on_client_authorize/5`函数。

由于 EMQX 内部功能也会挂载钩子函数，并且其他插件也会挂载相同的钩子函数，所以我们必须在挂载钩子时指定它的执行的顺序。`?HP_HIGHEST` 即指定当前的钩子函数最高优先级，优先级越高越先被执行。

### 4. 编写自定义访问控制代码

```erlang
%%  只允许 clientid 为 [A-Za-z0-9_] 的客户端连接。
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result, _Env) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.
%% 只允许订阅 /room/{clientid} 的主题，但是可以发送消息给任意主题。
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result, _Env) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result, _Env) -> {ok, Result}.
```

通过以上 2 个钩子函数，我们只让 clientid 格式符合规范的客户端登录。并且只能订阅 `/room/{clientid}` 主题，这样就实现了一个简单的聊天室功能：客户端可以发消息给任意客户端，但每个客户端只能订阅与自己相关的主题。

:::tip

1. 需要先把配置中的 `authorization.no_match` 设置为 `deny`，默认拒绝未经授权的操作。
2. 要设置同样的订阅规则，无需开发插件，可以通过 EMQX 内置的 [基于文件进行授权](../access-control/authz/file.md) 来实现。
:::

### 5. 打包插件

通过 rebar.config 中修改插件的版本信息：

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

再次运行打包命令：

```sh
make rel
...
===> Release successfully assembled: _build/default/rel/emqx_plugin_template
===> [emqx_plugrel] creating _build/default/emqx_plugrel/emqx_plugin_template-5.0.0-rc.3.tar.gz
```

命令会提示生成名称为 `插件名-版本.tar.gz` 的插件包。

### 6. 安装并启动插件

编译完成的包可以通过 CLI 进行安装：

```bash
./bin/emqx_ctl plugins install {pluginName}
```

### 7. 卸载插件

当你不需要插件，可以通过 CLI 卸载插件：

```bash
./bin/emqx_ctl plugins uninstall {pluginName}
```

{%emqxee%}
热升级后需要重新安装插件。
{%endemqxee%}
