# Plugins

EMQX allows users to customize the business logic or implement other protocols using plugins written in Erlang. This page introduces how to develop a customized plugin.

The basic process of plugin development and operation is as follows:

- Download and install our [rebar3 emqx-plugin template](https://github.com/emqx/emqx-plugin-template).
- Generate the corresponding plugin tarball using the plugin template provided by EMQX.
- Install the plugin package via Dashboard or CLI.
- Start/stop/uninstall your plugin via Dashboard or CLI.

:::tip Prerequisite

Knowledge of EMQX [hooks](./hooks.md)
:::

## Develop EMQX Plugins

This section provides a step-by-step guide on developing custom EMQX plugins, using the creation of an access control plugin as an example.

### Download and Install the rebar3 Plugin Template

EMQX offers an [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template) to facilitate the creation of custom emqx-plugin projects.

1. Use the following commands to download the `emqx-plugin-template`:

```shell
$ mkdir -p ~/.config/rebar3/templates
$ pushd ~/.config/rebar3/templates
$ git clone https://github.com/emqx/emqx-plugin-template
$ popd
```

2. Generate your customized plugin using the template with this command:

```shell
$ rebar3 new emqx-plugin my_emqx_plugin
```

This command creates a standard Erlang application structured as follows, with `emqx` included as a dependency:

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

3 directories, 13 files
```

Review the `rebar.config` file and adjust it as necessary for your plugin's requirements.

The project also includes an example module demonstrating how to add custom `emqx ctl` commands (`emqx_cli_demo.erl`).

**Note:** As the template depends on `emqx`, it requires a custom version of `rebar3`, which you can install using the included `get-rebar3` script.

### Test Your Development Environment

{% emqxce %}

Ensure your development environment is correctly set up by referring to the [Install from Source Code](../deploy/install-source.md) guide.

{% endemqxce %}

Execute the following command to verify if your plugin compiles and packages successfully:

```shell
$ make rel
```

At this stage, no additional coding is required.

Since the example plugin depends on the EMQX core application, you must download and compile it along with its dependencies. This step integrates the plugin into the main EMQX project. Be aware that this compilation process can be time-intensive.

### Customize the Example Project

After verifying that your setup works correctly, you can start tailoring the example project to meet your specific requirements. The initial template provides a core module at `src/my_emqx_plugin.erl`, which includes registration for all [currently known hooks](https://www.emqx.io/docs/en/v5.0/extensions/hooks.html). Begin by removing any hooks that are not needed and implement your own logic in the callbacks for the ones you retain.

#### Example Customization for Authentication and Access Control

For instance, if you need hooks for authentication and access control, modify the `my_emqx_plugin:load/1` function as shown:

```erlang
load(Env) ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, [Env]}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, [Env]}, ?HP_HIGHEST),
  ok.
```

Here, `on_client_authenticate/3` handles client authentication, while `on_client_authorize/5` manages access control.

As one hook function may be mounted by both EMQX and customized plugins, you need to specify the execution order when mounting it to the plugin.  `HP_HIGHEST` specifies that the current hook function has the highest priority and is executed first.

#### Customize Access Control Code

Consider the following access control implementations:

```erlang
%% Only allow connections with client IDs that match any of the characters: A-Z, a-z, 0-9, and underscore.
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result, _Env) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.
%% Clients can only subscribe to topics formatted as /room/{clientid}, but can send messages to any topics.
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result, _Env) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result, _Env) -> {ok, Result}.
```

In the provided code example, only clients with a client ID matching the specified pattern can log in. These clients are restricted to subscribing only to the topic `/room/{clientid}`, effectively creating a simple chat room setup. While clients can send messages to any topic, they are limited to subscribing to topics that directly pertain to their own client ID.

::: tip

1. Ensure `authorization.no_match` is set to `deny` in your configuration to prevent unauthorized connections.
2. This example details customizing an access control plugin. Similar authorization rules can be based on files as detailed in the [File-Based Authorization documentation](../access-control/authz/file.md).

:::

#### Pack the Customized Plugin

Modify the version information of the plugin via `rebar.config`:

```erlang
{relx, [ {release, {my_emqx_plugin, "1.0.0"}, %% This is the release version, different from app vsn in .app file
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

After updating the configuration, re-run the release command:

```shell
make rel
...
===> Release successfully assembled: _build/default/rel/my_emqx_plugin
===> [emqx_plugrel] creating _build/default/emqx_plugrel/my_emqx_plugin-1.0.0.tar.gz
```

This generates a new tarball, `my_emqx_plugin-1.0.0.tar.gz`, which you can upload and deploy to your EMQX cluster.

#### Write Configuration Schema for the Plugin (Optional)

{% emqxce %}
::: tip

EMQX Enterprise Edition features. EMQX Enterprise Edition provides comprehensive coverage of key business scenarios, rich data integration, product-level reliability, and 24/7 global technical support. Experience the benefits of this [enterprise-ready MQTT messaging platform](https://www.emqx.com/en/try?product=enterprise) today.

:::
{% endemqxce %}

EMQX version 5.7.0 introduced REST API for plugin configuration management and Avro Schema for configuration validation, enhancing the ability to update plugin configurations dynamically during runtime.
For Enterprise edition, it also allows the declaration of a user interface in the schema, enabling the EMQX Dashboard to dynamically generate configuration forms for easier management.

::: tip **Tip**

Check out the example files located in your project directory:  `priv/config_schmea.avsc.example` and `priv/config_i18n.json.example`.

:::

Your plugin package needs to include an Avro Schema configuration file, located at `priv/config_schema.avsc`. This file must adhere to the [Apache Avro specification](https://avro.apache.org/docs/1.11.1/specification/). Additionally, the schema supports UI declarations, allowing you to configure a `$ui` field. The EMQX Dashboard uses this field to create a configuration form page.

There is also an **optional** internationalization (i18n) config file, located at `priv/config_i18n.json`. This file is structured as key-value pairs, for example: `{ "$msgid": { "zh": "消息", "en": "Message" } }`. To support multiple languages in field names, descriptions, validation rule messages, and other UI elements in the `$ui` configuration, use `$msgid` prefixed with `$` in the relevant UI configurations.

#### Declarative UI Usage Reference (Optional)

Declarative UI components enable dynamic form rendering within the Dashboard, accommodating a variety of field types and custom components. Below is a description of the available components and their configurations:

**Configuration Item Descriptions**

- `component`<br />
  Required. Specifies the component type for displaying and configuring data of different values and types. Supported components include:

  | Component Name     | Description                                                  |
  | :----------------- | :----------------------------------------------------------- |
  | `input`            | Text input box for short texts or strings                    |
  | `input-password`   | Password input box that conceals input                       |
  | `input-number`     | Numeric input box allowing only numeric input                |
  | `input-textarea`   | Text area for longer text entries                            |
  | `input-array`      | Array input box for comma-separated values, supporting string and numeric arrays |
  | `switch`           | Toggle switch for boolean values                             |
  | `select`           | Dropdown selection box for enumerated types                  |
  | `code-editor`      | Code editor for specific formats (e.g., SQL, JSON)           |
  | `key-value-editor` | Editor for editing key-value pairs in Avro maps              |
  | `maps-editor`      | Editor for editing object arrays in Avro objects             |
- `label`<br />
  Required. Defines the field's label or name, supports `$msgid` for internationalization. If i18n is not configured, the original text will be displayed directly.
- `description`<br />
  Optional. Provides a detailed description of the field, supports `$msgid` for internationalization. If i18n is not configured, the original text will be displayed directly.
- `flex`<br />
  Required. Defines the proportion of the field in the grid layout; a full grid (24) spans an entire row, while a half grid (12) covers half a row.
- `required`<br />
  Optional. Indicates whether the field is mandatory.
- `format` (Applicable only for `code-editor` component)<br />
  Optional. Specifies the supported data formats, such as `sql` or `json`.
- `options` (Applicable only for `select` component)<br />
  Optional. Lists the selectable options, aligned with the symbols in the Avro Schema. Example:

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
- `items` (Applicable only for maps-editor component)<br />
  Optional. When using the maps-editor component, specify the field name and description of the items in the form. For example:

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
  Optional. Defines validation rules for the field, where multiple rules can be configured. Supported types include:

  - `pattern`: Requires a regular expression for validation.
  - `range`: Validates numeric input within a specified range. This validation can be configured with both a minimum value (`min`) and a maximum value (`max`), which can be set either together or independently.
  - `length`: Validates the character count of input, ensuring it falls within a specified range. This validation rule allows for the configuration of both a minimum length (`minLength`) and a maximum length (`maxLength`), which can be set either together or individually.
  - `message`: Specifies an error message to display when validation fails. This supports internationalization using `$msgid` to accommodate multiple languages.

**Example Validation Rules**:

The following are several example snippets. For more detailed examples, refer to `priv/config_schema.avsc.example`:

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
      "pattern": "^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d]*$",
      "message": "$password_validate"
    }
  ]
}
```

Including Avro Schema and i18n files in your plugin package ensures they are incorporated during plugin compilation and packaging. You can use the `emqx_plugins:get_config/1,2,3,4` function in your plugin code to retrieve configuration settings.

## Install and Launch the Plugin

To install the compiled plugin package, use the command line interface (CLI) as follows:

```bash
./bin/emqx ctl plugins install {pluginName}
```

## Uninstall the Plugin

When the plugin is no longer needed, you can uninstall it using the CLI with this command:

```bash
./bin/emqx ctl plugins uninstall {pluginName}
```

<!-- {% emqxee %} -->
<!-- **Note**: Plugins need to be reinstalled after hot upgrades. -->

<!-- {% endemqxee %} -->
