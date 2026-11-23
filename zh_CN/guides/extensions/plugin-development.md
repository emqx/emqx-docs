# 开发 EMQX 插件

本页将指导你使用 EMQX 插件模板开发自定义插件的全过程。

## 前提条件

在开始之前，请确保你具备以下环境和知识：

- 了解 EMQX 的[钩子机制](./hooks.md)。
- 已配置构建环境（例如已安装 `build-essential` 和 `make`）。
- 安装了 [rebar3](https://www.rebar3.org/)。
- 安装了与目标 EMQX 版本相同主版本号的 Erlang/OTP。你可以查看 Docker 镜像中的 `org.opencontainers.image.otp.version` 标签，或参考 [.tool-versions](https://github.com/emqx/emqx/blob/e5.9.0-beta.4/.tool-versions) 文件以获取使用的版本号。建议使用 [ASDF](https://asdf-vm.com/) 管理 Erlang 版本，或运行[这个脚本](https://github.com/emqx/emqx-builder/blob/main/show-latest-images.sh) 拉取 emqx-builder 镜像。

## 安装插件模板

EMQX 提供了一个官方插件模板 [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template)，可以帮助你快速构建插件项目。

在 Linux 系统中，使用以下命令下载模板：

```shell
$ mkdir -p ~/.config/rebar3/templates
$ pushd ~/.config/rebar3/templates
$ git clone https://github.com/emqx/emqx-plugin-template
$ popd
```

::: tip

如果设置了 `REBAR_CACHE_DIR` 环境变量，模板的目录应为 `$REBAR_CACHE_DIR/.config/rebar3/templates`。相关问题可参考[此链接](https://github.com/erlang/rebar3/issues/2762)。

:::

## 生成插件项目结构

通过如下命令使用模板创建插件项目：

```shell
$ rebar3 new emqx-plugin my_emqx_plugin
```

执行后会在 `my_emqx_plugin` 目录下生成一个完整的插件骨架。

### 目录结构说明

生成的插件目录结构如下：

```shell
my_emqx_plugin
├── LICENSE
├── Makefile
├── README.md
├── erlang_ls.config
├── priv
│   ├── config.hocon.example
│   ├── config_i18n.json.example
│   ├── config_schema.avsc.enterprise.example
│   └── config_schema.avsc.example
├── rebar.config
├── scripts
│   ├── ensure-rebar3.sh
│   └── get-otp-vsn.sh
└── src
    ├── my_emqx_plugin_app.erl
    ├── my_emqx_plugin.app.src
    ├── my_emqx_plugin_cli.erl
    ├── my_emqx_plugin.erl
    └── my_emqx_plugin_sup.erl
```

- `src`：插件的 Erlang 应用程序源代码目录。
- `priv`：插件的配置文件和配置 Schema 定义文件（包含示例）。
- `rebar.config`：用于构建插件和打包发布的 `rebar3` 构建配置文件。
- `Makefile`：构建插件的入口文件。
- `scripts`：Makefile 使用的辅助脚本。**注意：**由于模板依赖 EMQX，建议使用模板附带的 `./scripts/ensure-rebar3.sh` 脚本安装定制版本的 rebar3。
- `README.md`：项目说明文件。
-  `LICENSE`：插件使用的许可证样本。

#### `rebar.config` 配置文件说明

`rebar.config` 文件用于构建插件并打包为可发布的 Release 文件。根据实际需求修改该文件是插件开发的重要步骤。

最关键的几个部分如下：

- `deps`：定义依赖的 Erlang OTP 应用；
- `relx`：定义发布信息；
- `emqx_plugrel`：定义插件的元数据信息。

在 `deps` 部分，您可以添加插件所依赖的其他 OTP 应用。例如：

```
{deps, [
    {map_sets, "1.1.0"}
]}.
```

模板默认引入了 `map_sets` 作为演示依赖，如果不需要可以删除。更多依赖配置详见 [`rebar3` 官方文档](https://www.rebar3.org/docs/configuration/dependencies/)。

在 `relx` 部分中，您需要指定发布版本的名称和版本号，以及需要包含在发布包中的应用列表：

```
{relx, [ {release, {my_emqx_plugin, "1.0.0"},
            [ my_emqx_plugin
            , map_sets
            ]}
       ]}.
```

通常情况下，应将运行时依赖的应用也添加到发布版本中。

发布名称和版本号对于插件安装到 EMQX 时的标识非常重要，它们组合成插件在 API 或 CLI 中使用的唯一标识符（例如 `my_emqx_plugin-1.0.0`）。

在插件描述部分，需要指定插件的额外信息：

```
{emqx_plugrel,
  [ {authors, ["Your Name"]}
  , {builder,
      [ {name, "Your Name"}
      , {contact, "your_email@example.com"}
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

#### `src` 目录说明

`src` 目录包含插件 OTP 应用的代码。

##### `my_emqx_plugin.app.src`

这是一个标准的 Erlang 应用描述文件，最终会被编译为发布版本中的 `my_emqx_plugin.app` 文件。

- 应用的版本号可以与发布版本号不同，并采用独立的版本策略。
- 请特别注意 `applications` 字段。由于插件是以 OTP 应用的形式构建的，启动、停止或重启插件与管理 OTP 应用的方式相同。如果你的插件依赖其他应用，请确保它们也被列在 `applications` 字段中。

##### `my_emqx_plugin_app.erl`

这是插件的主模块，实现了 [`application` 行为](https://www.erlang.org/doc/man/application.html)，包括 `start/2` 和 `stop/1` 函数，用于启动和停止插件应用及其监督树。

在 `start/2` 函数中通常会执行以下操作：

- 挂载 EMQX hookpoint。
- 注册 CLI 命令。
- 启动监督树。

此外，该模块还可以实现两个可选回调函数：`on_config_changed/2` 和 `on_health_check/1`。

- `on_config_changed/2`：当插件配置通过 Dashboard、API 或 CLI 被修改时调用。
- `on_health_check/1`：当请求插件状态时调用，用于汇报插件的运行状态。

#### 其他文件说明

- `my_emqx_plugin_cli.erl`：实现插件的 CLI 命令。注册后可通过 `emqx ctl` 调用。
- `my_emqx_plugin_sup.erl`：实现插件的监督树。
- `my_emqx_plugin.erl`：插件的主逻辑模块。模板中包含了一些示例钩子的实现，用于日志输出。你也可以添加任意其他模块作为扩展。

::: tip 提示

应用模块和文件的命名可以自定义，但需要满足以下要求：

- 应用名称必须与插件名称一致；
- 应用模块（`_app`）必须命名为 `{plugin_name}_app`。

:::

#### `priv` 目录说明

`priv` 目录用于存放插件的配置文件和 schema 文件。

##### `config.hocon`

该文件包含插件的初始配置，采用 [HOCON 格式](https://github.com/lightbend/config/blob/master/HOCON.md)。你可以参考 `config.hocon.example` 文件快速了解写法。

##### `config_schema.avsc`

该文件采用 [Avro 格式](https://avro.apache.org/docs/1.11.1/specification/)，用于定义插件配置的 schema。当该文件存在时，EMQX 会在每次配置更新时验证其合法性。如果配置文件 `config.hocon` 不符合 schema，发布构建将失败。

此外，该文件还可以包含 UI 提示信息，支持在 EMQX Dashboard 中以交互方式配置插件。示例请参考 `config_schema.avsc.enterprise.example`。

##### `config_i18n.json`

该文件以 JSON 格式提供插件配置界面的翻译内容，例如：

```json
{
  "$key": {
    "zh": "中文翻译",
    "en": "English translation"
  }
}
```

这些翻译信息会被引用到 `config_schema.avsc` 的 UI 提示中。更多内容请参考 `config_i18n.json.example` 和 `config_schema.avsc.enterprise.example`。

## 实现插件功能

在插件框架搭建完成后，接下来就可以开始实现插件的业务逻辑。通常需要实现以下功能：

- 实现钩子函数和 CLI 命令；
- 处理配置更新；
- 实现健康检查。

### 实现钩子函数和 CLI 命令

EMQX 为各种事件定义了 hookpoint（钩子点）。任何应用（包括插件）都可以为这些 hookpoint 注册回调函数，以响应事件或修改默认行为。

常用的 hookpoint 已在插件模板中提供。完整的 hookpoint 列表（包括参数和返回值）可参考 [EMQX 源码](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl)。

要为某个 hookpoint 注册回调函数，请使用 `emqx_hooks:add/3` 函数。需要提供以下参数：

- hookpoint 名称
- 回调模块和函数（可选地包含附加参数）
- 回调的优先级（通常为 `?HP_HIGHEST`，表示优先执行）

取消注册回调函数使用 `emqx_hooks:del/2`，传入 hookpoint 名称和模块/函数即可。

以下是为 `client.authenticate` 和 `client.authorize` 注册/取消注册回调的示例：

```
-module(my_emqx_plugin).
...
hook() ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, []}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, []}, ?HP_HIGHEST).

unhook() ->
  emqx_hooks:del('client.authenticate', {?MODULE, on_client_authenticate}),
  emqx_hooks:del('client.authorize', {?MODULE, on_client_authorize}).
```

通常，应当在插件启动和停止时自动注册/注销 hook，可将 `hook/0` 和 `unhook/0` 调用添加到插件应用的 `start/2` 和 `stop/1` 函数中：

```
start(_StartType, _StartArgs) ->
    {ok, Sup} = my_emqx_plugin_sup:start_link(),
    my_emqx_plugin:hook(),

    {ok, Sup}.

stop(_State) ->
    my_emqx_plugin:unhook().
```

回调函数的签名定义见 [hookpoint 规范](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl)。例如：

```
-callback 'client.authorize'(
    emqx_types:clientinfo(), emqx_types:pubsub(), emqx_types:topic(), allow | deny
) ->
    fold_callback_result(#{result := allow | deny, from => term()}).

-callback 'client.authenticate'(emqx_types:clientinfo(), ignore) ->
    fold_callback_result(
        ignore
        | ok
        | {ok, map()}
        | {ok, map(), binary()}
        | {continue, map()}
        | {continue, binary(), map()}
        | {error, term()}
    ).
```

以下是一些回调函数的实现示例：

```
%% 仅允许客户端 ID 为 A-Z、a-z、0-9、下划线的连接。
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.

%% 仅允许客户端订阅 /room/{clientid} 主题，但可以向任意主题发布消息。
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result) -> {ok, Result}.
```

在插件模板中，hook 注册由 `my_emqx_plugin:load/1` 完成，注销由 `my_emqx_plugin:unload/0` 完成。

### 处理配置更新

当用户更新插件的配置时，会调用插件应用的 `on_config_changed/2` 回调函数。

在该回调中，通常需要执行以下操作：

- 验证新配置的合法性；
- 如果插件正在运行，则应用新配置。

需要注意的是，在验证配置时插件应用可能尚未启动，因此应使用无状态的检查逻辑，避免依赖运行时环境的判断逻辑，以防节点间因配置处理结果不一致而引发错误。

如果插件已在运行，可以应用新配置。常见的实现方式如下：

- 插件启动时，启动一个 `gen_server` 用于配置管理；
- 该服务（如 `my_emqx_plugin_config_server`）读取当前配置并初始化其状态；
- `on_config_changed/2` 验证配置并将新配置发送给该服务；
- 如果该服务正在运行，则更新其状态；否则忽略配置变更。

### 处理健康检查

当 EMQX 请求插件状态时，会调用 `on_health_check/1` 回调函数。插件可通过返回以下值报告其健康状况：

- `ok`：插件运行正常；
- `{error, Reason}`：插件存在问题，并附带字符串原因。

该回调对依赖外部资源的插件尤为重要，可用于上报外部资源不可用的状态。

更多示例详见插件模板中的 `my_emqx_plugin_app:on_health_check/1`。

::: tip

虽然该函数通常在插件运行时调用，但由于并发原因，也可能在插件启动或关闭过程中被调用。

:::

更多实现示例请参考[实现自定义插件逻辑](./plugin-example.md)。

## 构建插件发布包

执行以下命令以构建插件发布版本：

```shell
$ cd my_emqx_plugin
$ make rel
```

该命令会生成插件发布包：`_build/default/emqx_plugin/my_emqx_plugin-1.0.0.tar.gz`。该包可用于部署或安装插件。

### 发布包结构

构建完成后，发布包的结构如下：

```
└── my_emqx_plugin-1.1.0.tar.gz
    ├── map_sets-1.1.0
    ├── my_emqx_plugin-0.1.0
    ├── README.md
    └── release.json
```

tar 包中包含：

- 所有已编译的应用（来自 `rebar.config` 中的 `relx` 配置）；
- `README.md` 文档；
- `release.json` 文件，包含插件的元数据信息。

`release.json` 包含插件的元数据：

```json
{
  "name": "my_emqx_plugin",
  "description": "Another amazing EMQX plugin.",
  "authors": "Anonymous",
  "builder": {
    "name": "Anonymous",
    "contact": "anonymous@example.org",
    "website": "http://example.com"
  },
  "repo": "https://github.com/emqx/emqx-plugin-template",
  "functionality": "Demo",
  "compatibility": {
    "emqx": "~> 5.7"
  },
  "rel_vsn": "1.1.0",
  "rel_apps": [
    "my_emqx_plugin-0.1.0",
    "map_sets-1.1.0"
  ],
  "with_config_schema": true
}
```