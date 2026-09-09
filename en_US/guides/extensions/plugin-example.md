# Customize Plugin Logic

This page explains how to customize your EMQX plugin by modifying the default template logic provided in `src/my_emqx_plugin.erl`. The template registers all [available EMQX hooks](./hooks.md) by default. You should remove unused hooks and implement your own logic in the relevant callbacks.

## Register Hook Functions

For example, to add authentication and authorization logic, register the `my_emqx_plugin:hook/0` function as shown:

```erlang
hook() ->
  emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, []}, ?HP_HIGHEST),
  emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, []}, ?HP_HIGHEST).
```

Here, `on_client_authenticate/2` handles client authentication, while `on_client_authorize/4` manages authorization.

As one hook function may be mounted by both EMQX and customized plugins, you need to specify the execution order when mounting it to the plugin. `?HP_HIGHEST` specifies that the current hook function has the highest priority and is executed first.

## Example: Add Access Control Logic

Here is an example implementation for basic access control:

```erlang
%% Only allow connections with client IDs that match any of the characters: A-Z, a-z, 0-9, and underscore.
on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result) ->
  case re:run(ClientId, "^[A-Za-z0-9_]+$", [{capture, none}]) of
    match -> {ok, Result};
    nomatch -> {stop, {error, banned}}
  end.

%% Clients can only subscribe to topics formatted as /room/{clientid}, but can send messages to any topics.
on_client_authorize(_ClientInfo = #{clientid := ClientId}, subscribe, Topic, Result) ->
  case emqx_topic:match(Topic, <<"/room/", ClientId/binary>>) of
    true -> {ok, Result};
    false -> stop
  end;
on_client_authorize(_ClientInfo, _Pub, _Topic, Result) -> {ok, Result}.
```

This logic ensures:

- Only clients with valid IDs can connect.
- Clients can publish to any topic.
- Clients can only subscribe to their own `/room/{clientid}` topic, enabling simple chat-room behavior.

::: tip

- Set `authorization.no_match = deny` in your EMQX configuration to block unmatched access attempts.

- For file-based authorization rules, refer to the [File-Based Authorization documentation](../access-control/authz/file.md).

  :::

## Add Configuration Schema (Optional)

Starting with EMQX 5.7.0, plugin configurations can be managed dynamically via REST APIs. To enable this functionality and ensure configuration validation, your plugin should include:

- An Avro Schema configuration file, located at the relative path `priv/config_schema.avsc`, for validating the configuration structure. This file must adhere to the [Apache Avro specification](https://avro.apache.org/docs/1.11.1/specification/).
- A default configuration file at `priv/config.hocon` that adheres to the Avro schema rules.

At runtime, the updated configuration is saved to `data/plugins/<PLUGIN_NAME>/config.hocon`, and the old configuration file is backed up automatically.

::: tip

Check out the example files in your project directory: 

- `priv/config.hocon.example`
- `priv/config_schema.avsc.example`
- `priv/config_schema.avsc.enterprise.example` (includes UI declarations)
- `priv/config_i18n.json.example` (for internationalization)

These can be used as templates to build your plugin's configuration schema and UI.

:::

### Define Declarative UI (Optional)

The Avro schema can include a `$ui` field to define how configuration items should be rendered in the EMQX Dashboard. Plugin users can edit the configuration through a dynamic, auto-generated form.

There is also an optional internationalization (i18n) config file, located at `priv/config_i18n.json`. This file is structured as key-value pairs, for example:

```json
{
  "$msgid": {
    "zh": "消息",
    "en": "Message"
  }
}
```

To support multiple languages in field names, descriptions, validation rule messages, and other UI elements in the `$ui` configuration, use `$msgid` prefixed with `$` in the relevant UI configurations.

**Configuration Item Descriptions**

Declarative UI components enable dynamic form rendering within the Dashboard, accommodating a variety of field types and custom components. Below is a description of the available components and their configurations.

- `component`
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

- `label`
  Required. Defines the field's label or name, supports `$msgid` for internationalization. If i18n is not configured, the original text will be displayed directly.

- `description`
  Optional. Provides a detailed description of the field, supports `$msgid` for internationalization. If i18n is not configured, the original text will be displayed directly.

- `flex`
  Required. Defines the proportion of the field in the grid layout; a full grid (24) spans an entire row, while a half grid (12) covers half a row.

- `required`
  Optional. Indicates whether the field is mandatory.

- `format` (Applicable only for `code-editor` component)
  Optional. Specifies the supported data formats, such as `sql` or `json`.

- `options` (Applicable only for `select` component)
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

- `items` (Applicable only for maps-editor component)
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

- `rules`
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
