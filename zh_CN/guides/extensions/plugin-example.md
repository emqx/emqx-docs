# 自定义插件逻辑

本页面介绍如何通过修改 `src/my_emqx_plugin.erl` 文件中的默认模板逻辑，来自定义你的 EMQX 插件。该模板默认注册了所有[可用的 EMQX hook](./hooks.md)。你应当移除不需要的 hook，并在保留的回调中实现自定义逻辑。

## 注册 Hook 函数

例如，如果你希望添加认证和权限控制逻辑，可以按如下方式定义 `my_emqx_plugin:hook/0` 函数：

```erlang
hook() ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, []}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, []}, ?HP_HIGHEST).
```

其中，`on_client_authenticate/2` 用于处理客户端认证，`on_client_authorize/4` 用于授权。

由于同一个 hook 函数可能被 EMQX 和其他自定义插件同时挂载，因此在注册时需要指定执行顺序。`?HP_HIGHEST` 表示当前 hook 函数的优先级最高，将被最先执行。

## 示例：添加访问控制逻辑

以下是一个实现基础访问控制的示例：

```erlang
%% 仅允许客户端 ID 由字母、数字或下划线组成的连接。
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.

%% 客户端只能订阅 /room/{clientid} 格式的主题，但可以向任意主题发送消息。
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result) -> {ok, Result}.
```

上述逻辑保证：

- 只有合法 ID 的客户端可以连接。
- 客户端可以发布到任意主题。
- 客户端只能订阅自己对应的 `/room/{clientid}` 主题，从而实现简易的聊天室机制。

::: tip

- 请在 EMQX 配置中设置 `authorization.no_match = deny` 以阻止未命中的访问请求。
- 如需基于文件的授权规则，请参考 [ACL 文件](../access-control/authz/file.md)。

:::

## 添加配置模式（可选）

从 EMQX 5.7.0 起，插件配置可以通过 REST API 动态管理。为启用该功能并确保配置校验，你的插件应包含：

- 一个用于校验配置结构的 Avro 模式文件，路径为 `priv/config_schema.avsc`。该文件必须符合 [Apache Avro 规范](https://avro.apache.org/docs/1.11.1/specification/)。
- 一个默认配置文件 `priv/config.hocon`，其内容需满足 Avro 模式定义的规则。

运行时，配置更新将写入 `data/plugins/<PLUGIN_NAME>/config.hocon`，旧版本配置文件会自动备份。

::: tip

你可以参考插件项目目录中的示例文件：

- `priv/config.hocon.example`
- `priv/config_schema.avsc.example`
- `priv/config_schema.avsc.enterprise.example`（包含 UI 定义）
- `priv/config_i18n.json.example`（用于国际化）

这些文件可作为构建插件配置模式和 UI 的模板。

:::

### 定义声明式 UI（可选）

Avro 模式文件可以包含 `$ui` 字段，用于定义配置项在 EMQX Dashboard 中的展示形式。插件用户可以通过自动生成的动态表单进行配置。

还可以通过一个可选的国际化（i18n）配置文件进行语言支持，路径为 `priv/config_i18n.json`，格式为键值对，例如：

```json
{
  "$msgid": {
    "zh": "消息",
    "en": "Message"
  }
}
```

在 `$ui` 配置中使用以 `$` 开头的 `$msgid` 可实现字段名称、描述、校验提示等元素的多语言支持。

**配置项字段说明**

声明式 UI 支持多种字段类型和自定义组件，可动态生成配置表单。以下是各组件说明及配置方式：

- `component`
   必填。指定用于展示和配置不同类型值的组件类型。支持的组件包括：

  | 组件名称           | 描述                                           |
  | ------------------ | ---------------------------------------------- |
  | `input`            | 文本输入框（短文本或字符串）                   |
  | `input-password`   | 密码输入框，内容不可见                         |
  | `input-number`     | 数字输入框，仅允许输入数字                     |
  | `input-textarea`   | 多行文本输入区域                               |
  | `input-array`      | 数组输入框，逗号分隔的值，支持字符串或数字数组 |
  | `switch`           | 布尔值开关组件                                 |
  | `select`           | 下拉选择框，用于枚举类型                       |
  | `code-editor`      | 代码编辑器，用于特定格式（如 SQL、JSON）       |
  | `key-value-editor` | 用于编辑 Avro map 的键值对编辑器               |
  | `maps-editor`      | 用于编辑 Avro 对象数组的编辑器                 |

- `label`
   必填。字段的显示名称，支持 `$msgid` 国际化；如未配置 i18n，显示原始文本。

- `description`
   可选。字段描述，支持 `$msgid` 国际化；如未配置 i18n，显示原始文本。

- `flex`
   必填。字段在栅格布局中所占比例：24 表示整行，12 表示半行。

- `required`
   可选。是否为必填字段。

- `format`（仅适用于 `code-editor`）
   可选。指定支持的代码格式，如 `sql` 或 `json`。

- `options`（仅适用于 `select`）
   可选。列出可选项，对应 Avro Schema 中的符号。例如：

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

- `items`（仅适用于 `maps-editor`）
   可选。用于指定编辑对象数组时的字段名和描述，例如：

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

- `rules`
   可选。定义字段的校验规则，可配置多个规则。支持的类型包括：

  - `pattern`：使用正则表达式校验字段值；
  - `range`：对数值字段进行范围校验，可设置 `min` 和 `max`；
  - `length`：限制输入字符长度，支持配置 `minLength` 和 `maxLength`；
  - `message`：校验失败时显示的错误提示，支持 `$msgid` 国际化。

**校验规则示例**

以下是一些校验规则配置示例。更多例子请参考 `priv/config_schema.avsc.example` 文件：

```json
{
  "rules": [
    {
      "type": "pattern",
      "pattern": "^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])(\\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9]))*$",
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
      "pattern": "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)[a-zA-Z\\d]*$",
      "message": "$password_validate"
    }
  ]
}
```

将 Avro 模式文件和 i18n 配置文件一同打包进插件，可以确保它们在插件构建和发布过程中被正确使用。在插件代码中，可通过 `emqx_plugins:get_config/1,2,3,4` 函数读取配置项。