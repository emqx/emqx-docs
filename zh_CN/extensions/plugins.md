# 插件

EMQX 支持通过插件扩展自定义业务逻辑，或通过插件的协议扩展接口实现其他协议适配，本章节将指导您进行插件的开发。

插件开发和运行基本流程如下：

- 下载并安装我们的 [rebar3 emqx-plugin 模板](https://github.com/emqx/emqx-plugin-template)。
- 使用 EMQX 提供的插件模板生成相应的插件 tarball。  
- 通过 Dashboard 或 CLI 安装插件包。
- 通过 Dashboard 或 CLI 启动/停止/卸载您的插件。

:::tip 前置准备：

- 了解 EMQX [钩子](./hooks.md)。
  :::

## 开发 EMQX 插件

EMQX 提供了 [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template) 来创建自定义的 emqx-plugin 项目。在下面的部分中，我们将创建一个自定义的访问控制插件作为示例，为您逐步介绍插件开发。

### 下载、安装和使用 rebar3 插件模板

1. 下载 [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)，运行:

```shell
mkdir -p ~/.config/rebar3/templates  
pushd ~/.config/rebar3/templates
git clone https://github.com/emqx/emqx-plugin-template
popd
```

2. 现在从模板创建您的自定义插件

```shell  
rebar3 new emqx-plugin my_emqx_plugin
```

这将创建一个标准的 Erlang 应用，依赖 `emqx`。查看 `rebar.config` 并根据需要进行调整。

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
│   └── config.hocon
├── rebar.config
└── src
    ├── emqx_cli_demo.erl
    ├── my_emqx_plugin.app.src
    ├── my_emqx_plugin.erl
    ├── my_emqx_plugin_app.erl
    └── my_emqx_plugin_sup.erl

3 directories， 13 files
```

它还包含一个示例模块，演示如何添加自定义 `emqx ctl` 命令 `emqx_cli_demo`。

**注意**: 由于示例依赖 `emqx`，所以这个插件需要一个定制版本的 `rebar3`，它将通过提供的 `get-rebar3` 脚本安装。

### 测试您的开发环境

{% emqxce %}  
:::提示
要使用可工作的开发环境，请参阅[从源代码安装](../deploy/install-source.md)。
:::
{% endemqxce %}

运行 `make rel` 以测试插件是否可以成功编译和打包，此时无需编写代码。 

由于示例插件依赖 EMQX 主应用程序，它需要与依赖项一起下载然后作为主项目的一部分进行编译。请注意，编译过程可能需要较长时间才能完成。

### 定制示例项目

现在一切都工作正常了，您可以开始定制项目以满足您的需要。我们提供了一个核心模块，它注册了当前所有已知的[钩子](./hooks.md)。此代码位于 `src/my_emqx_plugin.erl` 中。您需要删除所有不需要的钩子，然后用自己的自定义代码实现剩余钩子的回调。

在下面的示例中，我们只需要两个钩子用于认证和访问控制，因此我们修改 `my_emqx_plugin:load/1` 如下:

```erlang
load(Env) ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, [Env]}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, [Env]}, ?HP_HIGHEST),
  ok.
```

我们使用 `on_client_authenticate/3` 进行客户端认证，使用 `on_client_authorize/5` 进行访问控制。

由于一个钩子函数可能同时被 EMQX 和定制插件挂载，因此在挂载到插件时，我们还需要指定执行顺序。`HP_HIGHEST` 指定当前钩子函数具有最高优先级，并首先执行。


#### 定制访问控制代码

```erlang
%% 只允许客户端ID名称匹配以下任一字符的连接: A-Z、a-z、0-9 和下划线。  
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result, _Env) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.
%% 只能订阅主题 /room/{clientid}，但可以向任何主题发送消息。
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result, _Env) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result, _Env) -> {ok, Result}.
```

在上面的代码示例中，我们只允许匹配规范的客户端登录。这些客户端只能订阅主题 `/room/{clientid}`，从而建立一个简单的聊天室，即客户端可以向任何其他客户端发送消息，但每个客户端只能订阅与自己相关的主题。

:::tip

1. 确保先在配置中将 `authorization.no_match` 设置为 `deny`，即 EMQX 将拒绝任何未经授权的连接请求。
2. 在此示例中，我们演示了如何自定义一个访问控制插件，您也可以[基于文件设置类似的授权规则](../access-control/authz/file.md)。 

:::

**打包定制的插件**

通过 `rebar.config` 修改插件的版本信息:

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
}.
```

现在重新运行 release 命令:

```shell 
make rel
...
===> Release successfully assembled: _build/default/rel/my_emqx_plugin
===> [emqx_plugrel] creating _build/default/emqx_plugrel/my_emqx_plugin-1.0.0.tar.gz
```

这将创建一个新的 EMQX 插件 tarball `my_emqx_plugin-1.0.0.tar.gz`，您现在可以上传并安装到运行中的 EMQX 集群中。

## 安装/启动插件  

使用 CLI 安装编译后的包:

```bash
./bin/emqx ctl plugins install {pluginName}
```

## 卸载插件

当不再需要该插件时，可以使用 CLI 轻松卸载它:

```bash  
./bin/emqx ctl plugins uninstall {pluginName}
```

{% emqxee %}
**注意**:插件需要在热升级后重新安装。

{% endemqxee %}

