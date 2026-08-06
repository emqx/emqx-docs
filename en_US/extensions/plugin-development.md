# Develop EMQX Plugins

This page walks you through the process of developing custom EMQX plugins using the EMQX plugin template.

EMQX supports two project styles for plugin development:

- **Monorepo plugin** — developed inside the EMQX monorepo under the `plugins/` directory, using Mix (Elixir build tooling) for compile, test, and packaging workflows. Recommended when the plugin is tightly coupled with a specific EMQX version.
- **Standalone project** — developed and packaged outside the EMQX monorepo using `rebar3` only. Recommended for independent plugin development.

The following sections cover both styles. The [Plugin Development Inside the EMQX Monorepo](#plugin-development-inside-the-emqx-monorepo) section describes the monorepo workflow and requirements. The [Standalone Plugin Development](#standalone-plugin-development) section provides a complete walkthrough for standalone projects.

## Prerequisites

Before you begin, make sure you have the following:

- Knowledge of EMQX [hooks](./hooks.md).
- A working build environment (e.g., `build_essential`), including `make`.
- [rebar3](https://www.rebar3.org/).
- Erlang/OTP of the same major version as the EMQX release you wish to target. For more information, see the `org.opencontainers.image.otp.version` attribute in the Docker or refer to the `.tool-versions` file for the used version (e.g., https://github.com/emqx/emqx/blob/e5.9.0-beta.4/.tool-versions). It's recommended to use [ASDF](https://asdf-vm.com/) to manage Erlang/OTP versions. Alternatively, you can pull the emqx-builder images by running [this command](https://github.com/emqx/emqx-builder/blob/main/show-latest-images.sh).

## Plugin Development Inside the EMQX Monorepo

This style is intended for plugins tightly coupled with a specific EMQX version. Plugins live under `plugins/` in the EMQX monorepo and participate in its Mix-based build and test workflows.

### Prerequisites

In addition to the [general prerequisites](#prerequisites), monorepo plugins require Mix (Elixir build tooling), which is included in the monorepo. Ensure `rebar3` is available by running:

```bash
make ensure-rebar3
```

### Bootstrap

1. Choose a plugin name. It must be globally unique and match the Erlang application name.

2. Check out the appropriate release branch matching the target EMQX version, for example, `release-60` for EMQX 6.0.

3. Generate the plugin application:

   ```bash
   cd plugins/
   rebar3 new emqx-plugin {plugin_name}
   ```

   Alternatively, keep the plugin in a separate repository and symlink it:

   ```bash
   ln -s /path/to/{plugin_name} plugins/{plugin_name}
   ```

4. Add `mix.exs` and `VERSION`:

   - Create `plugins/{plugin_name}/VERSION`, a single-line file containing the plugin version. This is the single source of truth.
   - Create `plugins/{plugin_name}/mix.exs`. Use `plugins/emqx_username_quota/mix.exs` as a reference.

   The `mix.exs` file must define:

   - `project/0`: OTP app name, version (read from `VERSION`), monorepo build paths, and `emqx_plugin` metadata. The required monorepo paths are:
     - `build_path: "../../_build"`
     - `deps_path: "../../deps"`
     - `lockfile: "../../mix.lock"`
   - `application/0`: OTP metadata (for example `mod`, `extra_applications`).
   - `deps/0`: include `{:emqx_mix, path: "../..", runtime: false}` for plugin build tooling. Set the `:emqx_mix` env to `:"emqx-enterprise-test"` in test profile and `:"emqx-enterprise"` otherwise.

   For Common Test support, also define:

   - `erlc_paths/0`: include `test` only in `*-test` Mix env.
   - `erlc_options/0`: enable `{:d, :TEST}` and `{:parse_transform, :cth_readable_transform}` in `*-test` Mix env.
   - A test-only dependency: `{:cth_readable, "1.5.1"}`.

   The `emqx_plugin/0` function must return a keyword list including:

   - `rel_vsn` (required, typically `version()`).
   - `name` (optional, defaults to `app`).
   - `rel_apps` (optional, defaults to `[app]`).
   - `metadata` keyword list (for example, `description`, `authors`, `builder`, `repo`, `functionality`, `compatibility`). Note that the package task flattens `metadata` into top-level fields in `release.json` — it is not emitted as a nested object.

### Development and Testing

- Implement plugin code under `plugins/{plugin_name}/src`.
- Add Common Test suites under `plugins/{plugin_name}/test`.
- Run Common Tests for the plugin:

  ```bash
  make plugins/{plugin_name}-ct
  ```

For quick local integration testing without adding the plugin to EMQX boot applications:

```bash
scripts/run-plugin-dev.sh {plugin_name} [--attach]
```

### Build the Plugin Package

Build the plugin package from the repository root using Mix-driven packaging:

```bash
make plugin-{plugin_name}
```

This produces a `.tar.gz` artifact under `_build/plugins/`, suitable for installation via `emqx ctl plugins`.

::: tip

Building a plugin package does not automatically load or start the plugin in EMQX. Manage the plugin lifecycle explicitly:

```bash
emqx ctl plugins install|enable|start
```

:::

## Standalone Plugin Development

### Install the Plugin Template

EMQX provides an [emqx-plugin-template](https://github.com/emqx/emqx-plugin-template) to simplify the creation of custom EMQX plugins. To create a new plugin, you should install `emqx-plugin-template` as a `rebar3` template.

For a Linux system, use the following commands to download the `emqx-plugin-template`:

```shell
$ mkdir -p ~/.config/rebar3/templates
$ pushd ~/.config/rebar3/templates
$ git clone https://github.com/emqx/emqx-plugin-template
$ popd
```

::: tip

If the `REBAR_CACHE_DIR` environment variable is set, the directory for templates should be `$REBAR_CACHE_DIR/.config/rebar3/templates`. [Here](https://github.com/erlang/rebar3/issues/2762) is a related issue.

:::

Verify the installation by running:

```shell
$ rebar3 new help
```

The output should list `emqx-plugin (custom)` as an available template.

### Generate the Plugin Skeleton

Generate a new plugin project using the installed template:

```shell
$ rebar3 new emqx-plugin my_emqx_plugin
```

This command creates a working skeleton for your plugin in the `my_emqx_plugin` directory.

### Directory Structure

The `rebar3 new emqx-plugin` command creates a standard Erlang application with `emqx` included as a dependency, structured as follows:

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

- `src`: Contains the code for the plugin’s OTP application.
- `priv`: Holds the plugin’s configuration files and schema (with example files).
- `rebar.config`: The `rebar3` configuration file used to build the application and package it into a release.
- `Makefile`: The entry point for building the plugin.
- `scripts`: Helper scripts for the `Makefile`. **Note:** As the template depends on `emqx`, it requires a custom version of `rebar3`, which you can install using the included `./scripts/ensure-rebar3.sh` script.
- `README.md`: Documentation placeholder.
- `LICENSE`: Sample license file for the plugin.

#### Understand the Configuration File: `rebar.config`

The `rebar.config` file is used to build the plugin and pack it into a release. Review the `rebar.config` file and adjust it as necessary for your plugin's requirements.

The most important sections are:

- Dependencies (`deps`) section;
- Release section (`relx`);
- Plugin description (`emqx_plugin`) section.

In the `deps` section, you can add dependencies to other OTP applications that your plugin depends on.

```
{deps,
    [
        ...
        %% this is my plugin's dependency
        {map_sets, "1.1.0"}
    ]}.
```

The template adds a single dependency to the plugin: `map_sets`. You can remove this if it's not required. For more details on dependencies, refer to the [`rebar3` dependency documentation](https://www.rebar3.org/docs/configuration/dependencies/).

In the `relx` section, you specify the release name and version, and the list of applications to be included in the release.

```
{relx, [ {release, {my_emqx_plugin, "1.0.0"},
            [ my_emqx_plugin
            , map_sets
            ]}
       ...
       ]}.
```

Normally, you would like to add the applications of the runtime dependencies from the `deps` section to the release.

The release name and version are important because they are used to identify the plugin when it is installed into EMQX. They form a single identifier for the plugin (`my_emqx_plugin-1.0.0`) by which it is addressed in the API or CLI.

In the plugin description section, you specify additional information about the plugin.

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
}
```

#### Overview of the `src` Directory

The `src` directory contains the code of the plugin's OTP application.

##### `my_emqx_plugin.app.src`

This is a standard Erlang application description file, which is compiled into `my_emqx_plugin.app` in the release.

- The version of the application does not have to match the release version and can follow a different versioning scheme.
- Pay special attention to the `applications` section. Since the plugin is built as an OTP application, starting, stopping, or restarting the plugin is the same as performing that action on the plugin's OTP application. If your plugin depends on other applications, make sure to list them in the `applications` section of the plugin's configuration file.

##### `my_emqx_plugin_app.erl`

This is the main module implementing the [`application` behaviour](https://www.erlang.org/doc/man/application.html) ( `start/2` and `stop/1` functions) to start and stop the plugin's application and its supervision tree.

Common activities in the `start/2` function include:

- Hook into EMQX hookpoints.
- Register CLI commands.
- Start the supervision tree.

Optionally, the `_app.erl` module can implement the `on_config_changed/2` and `on_health_check/1` callback functions.

- `on_config_changed/2` is called when the plugin's configuration is changed via the Dashboard, API or CLI.
- `on_health_check/1` is called when the plugin's status is requested. A plugin can report its status from this function.

#### Other Files

The `my_emqx_plugin_cli.erl` module implements the CLI commands of the plugin. When registered, CLI commands are called via `emqx ctl` command.

`my_emqx_plugin_sup.erl` implements a typical supervisor for the plugin.

`my_emqx_plugin.erl` is the main module of the plugin, implementing the plugin's logic. In the skeleton, it implements several demonstrational hooks with simple logging. Any other modules may be added to the plugin.

::: tip Note

The application modules and files may be arbitrarily named with the only requirements:

- The application name must be the same as the plugin name.
- The application module (`_app`) must be named as `{plugin_name}_app`.
  :::

#### Overview of the `priv` Directory

The `priv` directory holds the plugin's configuration files and schema.

##### `config.hocon`

This file contains the plugin's initial configuration in [HOCON format](https://github.com/lightbend/config/blob/master/HOCON.md). You can use `config.hocon.example` for quick reference.

##### `config_schema.avsc`

This file defines the schema for the plugin's configuration in [Avro format](https://avro.apache.org/docs/1.11.1/specification/). When present, EMQX will validate the plugin's configuration against this schema whenever it's updated. The release build will fail if `config.hocon` does not conform to the schema.

Additionally, this file can include UI hints, enabling interactive configuration through the EMQX Dashboard. For reference, see `config_schema.avsc.enterprise.example`.

##### `config_i18n.json`

This file contains translations for the plugin's configuration UI in JSON format. For example:

```
{
  "$key": {
    "zh": "中文翻译",
    "en": "English translation"
  },
  ...
}
```

The translations are referenced in the `config_schema.avsc` in UI hints. See `config_i18n.json.example` and `config_schema.avsc.enterprise.example` for more information.

### Implement the Plugin

Once the skeleton is ready, begin implementing your plugin's logic. To implement a plugin, the following logic is typically required:

- Implementing hooks and CLI commands.
- Handling configuration updates.
- Handling health checks.

### Implement Hooks and CLI Commands

EMQX defines hookpoints for various events. Any application (including a plugin) can register callbacks for these hookpoints to react to events or modify default behavior.

The most commonly used hookpoints are available in the skeleton file. A complete list of hookpoints, their arguments, and expected return values is also provided in the [EMQX code](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl).

To register a callback for a hookpoint, use the `emqx_hooks:add/3` function. You need to provide the following parameters:

- The hookpoint name
- The callback module and function, potentially with additional arguments that EMQX will pass
- The callback’s priority (usually `?HP_HIGHEST` to ensure it is called first)

To unregister a callback, use the `emqx_hooks:del/2` function with the hookpoint name and callback module/function.

For example, to register/unregister callbacks for `client.authenticate` and `client.authorize` hookpoints:

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

Typically, hooks should be enabled and disabled together with the plugin, so you can call `hook/unhook` in the `start/2` and `stop/1` functions of the plugin's application:

```
start(_StartType, _StartArgs) ->
    {ok, Sup} = my_emqx_plugin_sup:start_link(),
    my_emqx_plugin:hook(),

    {ok, Sup}.

stop(_State) ->
    my_emqx_plugin:unhook().
```

The signature of the callback functions is available in the [hookpoint specification](https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl). For example:

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

Here is an example of callback function implementations:

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

In the skeleton app, hooks are registered via `my_emqx_plugin:load/1` and unregistered via `my_emqx_plugin:unload/0`.

### Handle Configuration Updates

When a user updates the plugin's configuration, the `on_config_changed/2` callback function of the plugin's application is invoked.

In this callback, you typically need to:

- Validate the new configuration.
- React to the changes if the plugin is running.

While validating the configuration, keep in mind that the application may not yet be started. Therefore, use stateless checks and avoid environment-dependent checks that could cause inconsistencies across nodes.

If the plugin is running, you can apply the configuration changes. The usual pattern is as follows:

- On application start, a `gen_server` is initiated to handle the configuration.
- This server, e.g., `my_emqx_plugin_config_server`, reads the current configuration and initializes its state.
- The `on_config_changed/2` callback validates the configuration and sends the new configuration to the `my_emqx_plugin_config_server`.
- If the server is running, it updates its state with the new configuration; if not, no action is taken.

### Handle Health Checks

The `on_health_check/1` callback is called when EMQX requests the plugin's status. The plugin can report its health as follows:

- Return `ok` if the plugin is healthy.
- Return `{error, Reason}` with a binary reason to indicate an issue with the plugin.

This callback is essential for plugins that rely on external resources that may become unavailable.

For more details, see `my_emqx_plugin_app:on_health_check/1` in the skeleton app.

::: tip

Although this function is invoked for running plugins, it may also be called during plugin startup or shutdown due to concurrency.

:::

You can find more implementation examples in [Implement Customized Plugin Logic](./plugin-example.md).

### Build the Plugin Package

Execute the following command to make a release of the plugin:

```shell
$ cd my_emqx_plugin
$ make rel
```

This will create the plugin release: `_build/default/emqx_plugin/my_emqx_plugin-1.0.0.tar.gz`. This package can be used to provision/install the plugin.

### Package Structure

When a plugin is built into a release, the package structure is as follows:

```
└── my_emqx_plugin-1.1.0.tar.gz
    ├── map_sets-1.1.0
    ├── my_emqx_plugin-0.1.0
    ├── README.md
    └── release.json
```

The tarball includes the compiled applications (as specified in the `relx` section of `rebar.config`), the `README.md` file, and `release.json`, which contains metadata about the plugin.

```json
{
    "hidden": false,
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
    "git_ref": "unknown",
    "built_on_otp_release": "27",
    "emqx_plugrel_vsn": "0.5.1",
    "git_commit_or_build_date": "2025-04-29",
    "metadata_vsn": "0.2.0",
    "rel_apps": [
        "my_emqx_plugin-0.1.0",
        "map_sets-1.1.0"
    ],
    "rel_vsn": "1.1.0",
    "with_config_schema": true
}
```

## Plugin Extended API and UI

Plugins can expose custom HTTP endpoints through the EMQX plugin API gateway and optionally embed a native UI in the Dashboard.

### Plugin HTTP API

The plugin API gateway routes requests to your plugin under the path:

```
/api/v5/plugin_api/{plugin_name}/...
```

To handle these requests, implement `on_handle_api_call/4` in the plugin app module and dispatch by method and path. See `plugins/emqx_username_quota/src/emqx_username_quota_app.erl` and `emqx_username_quota_api.erl` for reference implementations.

#### Callback Contract

```erlang
on_handle_api_call(Method, PathRemainder, Request, Context) -> Result
```

| Parameter       | Description                                                                 |
| --------------- | --------------------------------------------------------------------------- |
| `Method`        | `get \| post \| put \| patch \| delete`                                     |
| `PathRemainder` | List of binary path segments after `{plugin_name}` (percent-decoded)        |
| `Request`       | Map with `query_string`, `headers`, and `body` (JSON, for non-GET/DELETE)   |
| `Context`       | Map with auth metadata and namespace info                                   |

Accepted return values:

- `{ok, StatusCode, Headers, Body}`
- `{error, StatusCode, Headers, Body}`
- `{error, not_found}`

### Plugin Native UI in Dashboard

If the `emqx_plugin` metadata contains an `index` field, the EMQX Dashboard presents the plugin's native UI in an iframe. The Dashboard prepends the plugin API base path:

```
/api/v5/plugin_api/{plugin_name}{index}
```

For example, `index: "/ui"` resolves to `/api/v5/plugin_api/{plugin_name}/ui`.

To disable the native UI, omit the `index` field or set it to an empty string.
