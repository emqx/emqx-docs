# Configuration Files

{% emqxce %}

Users can configure EMQX with configuration files or environment variables. This chapter will introduce the EMQX configuration files. For configuration items and detailed introduction, see [Configuration Manual](https://www.emqx.io/docs/en/v@CE_VERSION@/hocon/).

{% endemqxce %}

{% emqxee %}

Users can configure EMQX with configuration files or environment variables. This chapter will introduce the EMQX configuration files. For configuration items and detailed introduction, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

{% endemqxee %}

## Introduction

### **Main Configuration File**

EMQX will create a group of directories after installation, among which, `etc` is the folder that keeps all the configuration files. This section will focus on the main configuration file: `emqx.conf`.

Depending on your installation mode, `emqx.conf` is stored in:

| Installation                               | Path                      |
| ------------------------------------------ | ------------------------- |
| Installed with RPM or DEB package          | `/etc/emqx/emqx.conf`     |
| Running in docker container                | `/opt/emqx/etc/emqx.conf` |
| Extracted from portable compressed package | `./etc/emqx.conf`         |


As the main configuration file, `emqx.conf` contains most of the commonly used configuration items.
You can follow the examples provided in the `examples` directory (located within the same directory) to customize the settings.
EMQX uses the default settings if a config item is not found in the config files.

### Configuration Rewrite File

`emqx.conf` defines settings at a global level, for cases where you need to customize the settings for a cluster or a node, EMQX also provides a configuration rewrite file that extends but does not override `emqx.conf`:

**`cluster.hocon`**

Contains configuration items for the entire cluster, configuration changes made from Dashboard, REST API, and CLI will be persisted to this file.

If a certain cluster node is restarted or some new nodes are added, the node will automatically copy and apply the configuration file from other nodes within the cluster, therefore there is no need nor recommended to configure it manually.

The configuration rewrite files are located in the `$data/configs/` directory, and the path of the `data` directory varies according to the installation method:

| Installation                               | Path                 |
| ------------------------------------------ | -------------------- |
| Installed with RPM or DEB package          | `/var/lib/emqx`      |
| Running in docker container                | `/opt/emqx/data`     |
| Extracted from portable compressed package | `./data`             |

::: tip
It is possible to change data directory from config `node.data_dir` or environment variable `EMQX_NODE__DATA_DIR`, however, when running a cluster, all nodes should have the same path.
:::

By default, most global settings are defined in the `emqx.conf` file, if you perform certain operations on the cluster level from Dashboard, REST API or CLI, the changes will be stored in `cluster.hocon`. And this whole process is called hot reload.

For override rules, see [Configure override rules](#configure-override-rules).

::: tip
Some configuration items cannot be overridden, for example, `node.name`.
:::

Since version 5.1, when cluster configuration changes, EMQX backups the `cluster.hocon` file before overwriting it.
The backup files are suffixed with a timestamp of the node's local time.
At most 10 backup files can be kept.

## HOCON Configuration Format

Since EMQX 5.0, we have begun to use [Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon) as the configuration file format.

HOCON is a format for human-readable data and a superset of JSON. With features like inheritance, combined, and quotes, HOCON further simplifies the configuration work.

**HOCON syntax：**

HOCON values can be represented as JSON-like objects, for example:

```bash
node {
  name = "emqx@127.0.0.1"
  cookie = "mysecret"
  cluster_call {
    retry_interval  =  1m
  }
}
```

or in flattening:

```bash
node.name = "127.0.0.1"
node.cookie = "mysecret"
node.cluster_call.retry_interval = "1m"
```

This cuttlefish-like flattening format is backward compatible with the previous EMQX versions, but it is used differently:

HOCON recommends adding quotes at both ends of the string. Strings without special characters can also be unquoted, for example `foo`, `foo_bar`, while cuttlefish regards all characters to the right of `=` as values.

For more information about HOCON syntax, please refer to [HOCON Documentation](https://github.com/lightbend/config/blob/main/HOCON.md).

## Environment Variables

Besides configuration files, you can also use environment variables to configure EMQX.

For example, environment variable `EMQX_NODE__NAME=emqx2@127.0.0.1` will override the following configuration:

```bash
# emqx.conf
node {
  name = "emqx@127.0.0.1"
}
```

Configuration items and environment variables can be converted by the following rules:

1. Since the `.` separator in the configuration file cannot be used in environment variables, EMQX uses double underscores `__` as the configuration separator;
2. To distinguish the converted configuration items from other environment variables, EMQX also adds a prefix `EMQX_` to the environment variable;
3. The value of the environment variable is parsed according to the HOCON value, making it possible to use the environment variable to pass the value of complex data types, but please note that special characters such as `：` and `=` need to be wrapped in double quotes `"`.

Conversion example:

```bash
# Environment variables

## localhost:1883 will be parsed into a struct `{"localhost": 1883}`, so it needs to be wrapped in double quotes
export EMQX_LISTENERS__SSL__DEFAULT__BIND='"127.0.0.1:8883"'

## Pass the HOCON array directly by character
export EMQX_LISTENERS__SSL__DEFAULT__SSL_OPTIONS__CIPHERS='["TLS_AES_256_GCM_SHA384"]'


# Configuration file
listeners.ssl.default {
    ...
    bind = "127.0.0.1:8883"
    ssl_options {
      ciphers = ["TLS_AES_256_GCM_SHA384"]
    }
  }
}
```

::: tip

EMQX will ignore undefined root paths, for example, `EMQX_UNKNOWN_ROOT__FOOBAR` , because `UNKNOWN_ROOT` is not a pre-defined root path.

When a known root path is set with an unknown field name, EMQX will output a `warning` log at startup, for example, when `enable` is incorrectly configured as `enabled`, it will output:

```bash
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

:::

## Configure Override Rules

The value of HOCON will be overridden hierarchically, the rules are as follows:

- In the same file, the value defined in the later section will override any previous key value.
- A higher-level value will replace that of a lower-level.

The EMQX configuration is prioritized (overlayed) in the following order: `cluster.hocon < emqx.conf < environment variables`.

Settings in environment variables that begin with 'EMQX_' have the highest priority and will override any settings in the `etc/emqx.conf` file.

Changes made through EMQX Dashboard UI, HTTP API, or CLI are persisted in `data/configs/cluster.hocon` at runtime and will take effect immediately. However, if the same configuration items are set differently in the `etc/emqx.conf` file, the runtime updates will be overridden by the settings in `etc/emqx.conf` after the node restarts.

To avoid confusion, it is highly recommend NOT to have the same config keys in both `cluster.hocon` and `emqx.conf`.

::: tip
1. If you're using an older version of EMQX, specifically version e5.0.2/v5.0.22 or earlier(i.e. the `cluster-override.conf` file still exists in EMQX's data directory), then the order of priority for configuring your settings is as follows: `emqx.conf < ENV < HTTP API(cluster-override.conf)`.
2. If you're upgrading from e5.0.2/v5.0.22 or earlier to the latest version of EMQX,
   the configuration overriding order will remain unchanged, `cluster.hocon` will not be created to keep compatibility.
3. The `cluster-override.conf` mechanism is removed in version 5.1.
:::

### Override

In the following configuration, the `debug` value of `level` defined in the last line will overwrite the previously defined `error`, but the `enable` field remains unchanged:

```bash
log {
  console_handler{
    enable = true
    level = error
  }
}

## Set the console log printing level to debug, and keep the other configurations
log.console_handler.level = debug
```

The packet size limit was first set to 1MB, then overridden to 10MB:

```bash
zones {
  zone1 {
    mqtt.max_packet_size = 1M
  }
}
zones.zone1.mqtt.max_packet_size = 10M
```

### List Element Override

EMQX array has two expression ways:

- List, for example, `[1, 2, 3]`
- Map (subscribing), for example: `{"1"=1, "2"=2, "3"=3}`

The following 3 formats are equivalent:

```bash
authentication.1 = {...}
authentication = {"1": {...}}
authentication = [{...}]
```

Based on this feature, we can easily override the value of an element in an array, for example:

```bash
authentication  = [
  {
    enable = true,
    backend = "built_in_database",
    mechanism = "password_based"
  }
]

# The `enable` field of the first element can be overridden in the following way:
authentication.1.enable = false
```

::: tip

Arrays (in list format) will be fully overwritten and original value cannot be kept, for example:

```bash
authentication = [
  {
    enable = true
    backend = "built_in_database"
    mechanism="password_based"
  }
]

## With the following method, all fields except `enable` of the first element will be lost.
authentication = [{ enable = true }]
```

:::

## Zone Override

Zone in EMQX is a concept of configuration grouping.
A zone can be linked to a listener by configuring `the `zone` field with a zone name.
MQTT clients connected to a listener which linked to a zone will inherit the group of settings from
the zone configurations which override the global settings.

::: tip
Listeners are by default linked to a zone named `default`.
The `default` zone is just a logical group which does not exist in configuration files.
:::

Below config items can be overridden in the `zone` level:

- `mqtt`: MQTT connection and session settings. e.g. grant a greater maximum packet size for MQTT messages only in a specific zone.
- `force_shutdown`: Force shutdown policies.
- `force_gc`: Fine-tune the Erlang process garbage collection.
- `flapping_detect`: To detect the flapping of the clients.
- `session_persistence`: Session persistence settings. e.g. when one wants support durable storage for MQTT sessions only in a specific zone.

In EMQX version 5, the default configuration file does not come with any zones.
This is different from version 4, in which there are two default zones: 'internal' and 'external'.

To create a zone, you need to define it in `emqx.conf`, for example:

```
zones {
  # there can be multiple zones
  my_zone1 {
    # zones share the same config schema/layout as the global configs
    mqtt {
      # allow larger packet size for connections in this zone
      max_packet_size = 10M
    }
    force_shutdown {
      # allow larger heap size for connections in this zone
      ...
    }
    session_persistence {
      # only apply durable storage for sessions in this zone
      ...
    }
  }
  my_zone2 {
    ...
  }
}
```

## Schema

To make the HOCON objects type-safe, EMQX introduced a schema for it. The schema defines data types, and data fields' names and metadata for config value validation and more.

{% emqxee %}

The [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) is generated from schema.

{% endemqxee %}

{% emqxce %}

The [Configuration Manual](https://www.emqx.io/docs/en/v${CE_VERSION}/hocon/) is generated from schema.

{% endemqxce %}

::: tip
The Zone config schema is not generated in the in the configuration manual docs because the schemas are identical to each group respectively.
For example, `zones.my_zone1.mqtt {...}` has ideltical schema as `mqtt {...}`.
:::

### Primitive Data Types

Primitive data types in the configuration manual are largely self-explanatory, requiring minimal documentation. Below is a comprehensive list of all primitive types you will encounter.

#### Integer

Represents a whole number. Examples include `42`, `-3`, `0`.

#### Integer(Min..Max)

An integer that falls within a specified range. For example, `1..+inf` means from `1` to positive infinity (`+inf`), indicating that only positive integers are acceptable.

#### Enum(symbol1, symbol2, ...)

Defines an enumerated type that can only take one of the predefined symbols. For instance, `Enum(debug,info,warning,error)` defines acceptable logging levels.

#### String

The **String** data type represents a sequence of characters and supports several formats for diverse use cases:

- **Unquoted**: Ideal for simple identifiers or names that avoid special characters (see below for details).

- **Quoted Strings**: For strings that include special characters or whitespace,
  use double quotes (`"`), utilizing the backslash (`\`) to escape characters as needed. Example: `"line1\nline2"`.

- **Triple-quoted String**: Enclosed with triple quotes (`"""`), these strings do not require escapes (except for `\`),
  simplifying the inclusion of complex content. Note that quotes adjacent to the triple-quotes must be escaped to be considered part of the string.

- **Triple-quoted String with Indentation**: Surrounded by `"""~` and `~"""`,
  introduced since EMQX 5.6, this format allows for indentation within the string for better layout in the configuration file, ideal for multi-line or formatted text.

**Special Considerations for Unquoted Strings:**
- Avoid "forbidden characters": `$`, `"`, `{`, `}`, `[`, `]`, `:`, `=`, `,`, `+`, `#`, `` ` ``, `^`, `?`, `!`, `*`, `&`, `\`, or whitespace.
- Do not start with `//` (which introduces a comment).
- Do not begin with `true`, `false`, or `null` in a way that could be misinterpreted as boolean or null values.

**Guidelines for Triple-quoted Strings:**
- To include a quote character adjacent to the triple-quotes, escape it or use the `~` delimiter for clarity.
- Multiline strings support indentation with spaces (not tabs) for readability.
  The indentation level is determined by the smallest number of leading spaces on any line.

For example:

```
rule_xlu4 {
  sql = """~
    SELECT
      *
    FROM
      "t/#"
  ~"""
}
```

For additional details on string quoting conventions in HOCON, consult [the HOCON specification](https://github.com/lightbend/config/blob/main/HOCON.md#unquoted-strings).

For insights into EMQ's specialized adaptations of the triple-quoted string with indentation, refer to the [emqx/hocon.git README](https://github.com/emqx/hocon?tab=readme-ov-file#divergence-from-spec-and-caveats).

#### String("constant")

A constant string value, effectively acting as a single-value enumeration (`Enum`). This could be used to define a static value that doesn't change, such as a specific setting or mode.

#### Boolean

Either `true` or `false`, which is case sensitive.

#### Float

A floating-point number, supporting decimals. Examples include `3.14`, `-0.001`.

#### Duration

Represents a span of time in a human-readable format. Examples and explanation of format.

#### Duration(s)

Specifies a `Duration` type with a precision level of seconds. Further details and examples.

#### Secret

A type intended for sensitive information, such as passwords and tokens. Explanation of its usage and importance.


### Complex Data Types

Complex data types in EMQX's HOCON configuration are designed to encapsulate data structures that can include both other complex types and primitive values.
These data types enable flexible and hierarchical data representation.

#### Struct `Struct(name)`

Represents a structure with fields enclosed between curly braces `{}`.
The `name` parameter is a reference to a schema that specifies the structure's fields and their respective types.

#### Map `Map($name-\>Type)`

Similar to `Struct`, a `Map` holds key-value pairs without predefined names for the fields.

The `$name` variable indicates that the keys can be any string (except for a string with dot `.` in it),
representing the name of an entity or attribute.
The `Type` specifies that all values in the map must be of the same data type, allowing for uniform collections of data items.

#### OneOf `OneOf(Type1, Type2, ...)`

Defines a union type that can include two or more types to indicate that one struct field can be any one of the member types.
For example, this allows a configuration entry to be either `String(infinity)` or a `Duration`.

#### Array `Array(Type)`

Defines an array consisting of elements that adhere to the specified `Type`.


::: tip

If a Map field name is a positive integer number, it is interpreted as an alternative representation of an `Array`. For example:

```bash
myarray.1 = 74
myarray.2 = 75
```

will be interpreted as `myarray = [74, 75]`, which is handy when trying to override array elements.

:::

## Configuration Paths

If we consider the whole EMQX config as a tree, to reference a primitive value, we can use dot-separated names from string for the path from the tree root (always a Struct) down to the primitive values at tree-leaves.

Each segment of the dotted string is a Struct filed name or Map key. For Array elements, 1-based index is used.

Below are some examples

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```
