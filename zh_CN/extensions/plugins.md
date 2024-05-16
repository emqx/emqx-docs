# 插件

EMQX 支持通过插件扩展自定义业务逻辑，或通过插件的协议扩展接口实现其他协议适配，本章节将指导您进行插件的开发。

插件开发和运行基本流程如下：

- 下载并安装我们的 [rebar3 emqx-plugin 模板](https://github.com/emqx/emqx-plugin-template)。
- 使用 EMQX 提供的插件模板生成相应的插件 tarball。
- 通过 Dashboard 或 CLI 安装插件包。
- 通过 Dashboard 或 CLI 启动/停止/卸载您的插件。

::: tip 前置准备

了解 EMQX [钩子](./hooks.md)。

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

2. 现在从模板创建您的自定义插件：

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
│   ├── config.hocon
│   ├── config_i18n.json.example
│   └── config_schema.avsc.example
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
::: tip 提示
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

::: tip

1. 确保先在配置中将 `authorization.no_match` 设置为 `deny`，即 EMQX 将拒绝任何未经授权的连接请求。
2. 在此示例中，我们演示了如何自定义一个访问控制插件，您也可以[基于文件设置类似的授权规则](../access-control/authz/file.md)。

:::

#### 打包定制的插件

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

#### 为插件编写 Config Schema （可选）

{% emqxce %}
:::tip
EMQX 企业版功能。EMQX 企业版可以为您带来更全面的关键业务场景覆盖、更丰富的数据集成支持，更高的生产级可靠性保证以及 24/7 的全球技术支持，欢迎[免费试用](https://www.emqx.com/zh/try?product=enterprise)。
:::
{% endemqxce %}

我们在 EMQX 企业版 5.7.0 中为插件引入了 Avro Schema 以提供在运行时更新插件配置的能力，并且可以在该 Schema 中提供 UI 声明，从而可以在 EMQX Dashboard 中渲染配置表单以便使用。

::: tip **提示**

您可以在项目目录中找到两个示例文件：`priv/config_schmea.avsc.example`, `priv/config_i18n.json.example`。

:::

这需要您的插件包提供一个 Avro Schema 配置文件，它应位于 `priv/config_schmea.avsc`。该文件应当遵守 Apache Avro 规范（详情请参阅 [Apache Avro Specification (1.11.1)](https://avro.apache.org/docs/1.11.1/specification/)）。此外它也同时也包含了关于 UI 的描述声明。即可以使用 Avro Schema 的 metadata 配置一个 `$ui` 字段，EMQX Dashborad 将根据 `$ui` 字段中提供的信息来生成一份配置表单页。

此外还有一个**可选的**国际化配置文件以提供多语言支持， i18n 文件应位于 `priv/config_i18n.json`。它是一个键值对文件，如：`{ "$msgid": { "zh": "消息", "en": "Message" } }`。如果 `$ui` 配置中的字段名称、描述、验证规则的消息等需要支持多语言，需要在对应的配置里使用以 `$` 开头的 `$msgid`。

#### 声明式 UI 使用参考 （可选）

UI 声明被用于动态渲染表单，支持各种字段类型和自定义组件。以下是可用组件及其配置说明。

**配置项说明**

- `component`<br />
  必填。为该字段配置一个组件，用于显示和配置不同值和类型的数据，以下为支持的组件列表：
  
  | 组件名             | 描述                                               |
  |:-------------------|:---------------------------------------------------|
  | `input`            | 用于简短文本或字符串的文本输入框                   |
  | `input-password`   | 隐藏输入内容的密码输入框                           |
  | `input-number`     | 只允许数字输入的数字输入框                         |
  | `input-textarea`   | 适用于较长的文本输入的文本域                       |
  | `input-array`      | 输入值以逗号分隔，支持字符串和数字数组的数组输入框 |
  | `switch`           | 用于布尔值输入的开关                               |
  | `select`           | 用于枚举类型的下拉选择框                           |
  | `code-editor`      | 支持特定格式的代码（如 SQL、JSON 等）的代码编辑器  |
  | `key-value-editor` | 用于编辑 Avro 中的 map 类型的键值对编辑器          |
  | `maps-editor`      | 用于编辑 Avro 中的对象数组类型的对象数组编辑器     |
- `label`<br />
  必填。字段的标签或名称，可使用 $msgid。若不配置 i18n，将直接显示原文。
- `description`<br />
  可选。字段的详细描述，可使用 $msgid。若不配置 i18n，将直接显示原文。
- `flex`<br />
  必填。定义字段在网格布局中占据的比例，满格（24）表示占据一整行，半格（12）表示占据半行。
- `required`<br />
  可选。指示字段是否为必填项。
- `format` (仅 code-editor 组件适用)<br />
  可选。代码编辑器支持的格式，目前支持的数据格式为 `sql` 或 `json`。
- `options` (仅 select 组件适用)<br />
  可选。定义枚举类型的可选项，应与 Avro Schema 中的 symbols 保持一致。示例：
  ```json
  [
    {
      "label": "$mysql",
      "value": "MySQL"
    },
    {
      "label": "$pgsql",
      "value": "postgreSQL"
    }
  ]
  ```
- `items` (仅 maps-editor 组件适用)<br />
  可选。当使用 maps-editor 组件时，指定表单内项目的字段名和描述。例如：
  ```json
  {
    "items": {
      "optionName": {
        "label": "$optionNameLabel",
        "description": "$optionDesc",
        "type": "string"
      },
      "optionValue": {
        "label": "$optionValueLabel",
        "description": "$optionValueDesc",
        "type": "string"
      }
    }
  }
  ```
- `rules`<br />
  可选。用于定义字段的校验规则，一个规则可以配置多个。目前支持下述类型：
  - `pattern`，正则表达式验证，需要配置一个正则表达式进行验证。正则表达式写在 pattern 字段里。
  - `range`，用来验证输入数字的大小范围，最小值 min，最大值 max，可以同时配置，也可以单独配置一个。
  - `length`，用来验证输入的字符长度大小的限制，最短长度 minLength，最大长度 maxLength，可以同时配置，也可以单独配置。
  - `message`，验证不通过时的错误消息：支持配置 i18n 的 `$msgid`。

**示例片段**

以下为几个示例片段，更详细的示例请参考 `priv/config_schema.avsc.example`：

```json
{
    "rules": [
    {
      "type": "pattern",
      "pattern": "^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\\\\\\\-]{0,61}[a-zA-Z0-9])(\\\\\\\\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\\\\\\\-]{0,61}[a-zA-Z0-9]))*$",
      "message": "$hostname_validate"
    }
  ]
}
```

```json
{
    "rules": [
    {
      "type": "range",
      "min": 1,
      "max": 65535,
      "message": "$port_range_validate"
    }
  ]
}
```

```json
{
    "rules": [
    {
      "type": "length",
      "minLength": 8,
      "maxLength": 128,
      "message": "$password_length_validate"
    },
    {
      "type": "pattern",
      "pattern": "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\\\\\\\d)[a-zA-Z\\\\\\\\d]*$",
      "message": "$password_validate"
    }
  ]
}
```

在插件编译打包时，如果您提供了 Avro Schema 文件及 i18n 文件，它们将被一同添加至 tarball 中。在您的插件代码中，可以使用函数 `emqx_plugins:get_config/1,2,3,4` 来获取插件配置文件。

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

<!-- {% emqxee %} -->
<!-- **注意**:插件需要在热升级后重新安装。 -->

<!-- {% endemqxee %} -->

