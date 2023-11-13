# Configuration Files

Users can configure EMQX with configuration files or environment variables. This chapter will introduce the EMQX configuration files. For configuration items and detailed introduction, see [Configuration Manual](./configuration-manual.html).

## Configuration Files

### **Main Configuration File**

EMQX will create a group of directories after installation, among which, `etc` is the folder that keeps all the configuration files. This section will focus on the main configuration file: `emqx.conf`.

Depends on your installation mode, `emqx.conf` is stored in:

| Installation                               | Path                      |
| ------------------------------------------ | ------------------------- |
| Installed with RPM or DEB package          | `/etc/emqx/emqx.conf`     |
| Running in docker container                | `/opt/emqx/etc/emqx.conf` |
| Extracted from portable compressed package | `./etc/emqx.conf`         |


As the main configuration file, `emqx.conf` contains most of the commonly used configuration items.
You can follow the example provided in the `emqx.conf.example` file (located within the same directory) to customize the settings.
EMQX uses the default settings if a config item is not found in the config files.

### Configuration Rewrite File

`emqx.conf` defines settings at a global level, for cases where you need to customize the settings for a cluster or a node, EMQX also provides a configuration rewrite file which extends but does not override `emqx.conf`:

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

For override rules, see [Configure override rules](#Configure+override+rules).

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
    retry_interval = "1m"
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
1. If you're using an older version of EMQX, specifically version e5.0.2/v5.0.22 or earlier(i.e. the `cluster-override.conf` file still exists in EMQX's data directory),
   then the order of priority for configuring your settings is as follows: `emqx.conf < ENV < HTTP API(cluster-override.conf)`.
2. If you're upgrading from e5.0.2/v5.0.22 or earlier to the latest version of EMQX, 
   the configuration overriding order will remain unchanged, `cluster.hocon` will not be created to keep compatibility.  
3. The `cluster-override.conf` mechanism is scheduled to be removed in version 5.1.   
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
zone {
  zone1 {
    mqtt.max_packet_size = "1M"
  }
}
zone.zone1.mqtt.max_packet_size = "10M"
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

## Schema

To make the HOCON objects type-safe, EMQX introduced a schema for it. The schema defines data types, and data fields' names and metadata for config value validation and more.

::: tip Tip 

The configuration document you are reading now is generated from schema metadata. 

:::

### Primitive Data Types

There are quite some different primitive types, to name a few:

- `atom()`.
- `boolean()`.
- `string()`.
- `integer()`.
- `float()`.
- `number()`.
- `binary()`, another format of string().
- ...

::: tip Tip 

The primitive types are mostly self-describing, so there is usually not a lot to document. For types that are not so clear by their names, the field description is to be used to find the details. 

:::

### Specialized Data Types

Specialized types are essentially primitive types with additional meaning that is reflected in their names, validation rules, and descriptions.

* `duration` (`duration_s` / `duration_ms`)

    A string that represents a time duration, for example: `10s`, `2.5m`, `1h30m`, `2345ms`, `1W2D`. When precision is specified, finer portions of the duration may be ignored: writing `1200ms` for `duration_s` is equivalent to writing `1s`. It doesn't matter if units are in upper or lower case.

* `bytesize`

    A string that represents a number of bytes, for example: `10B`, `640kb`, `4MB`, `1GB`. Units are interpreted as powers of 1024, and the unit part is case-insensitive.

* `secret`

    A string holding some sensitive information, such as a password. When secret starts with `file://`, the rest of the string is interpreted as a path to a file containing the secret itself: whole content of the file except any trailing whitespace characters is considered a secret value.

### Complex Data Types

Complex types define data 'boxes' which may contain other complex data, primitive or specialized values. There are 4 complex data types in EMQX's HOCON config:

1. Struct: Named using an unquoted string, followed by a predefined list of fields. Only lowercase letters and digits are allowed in struct and field names. Also, only underscore can be used as a word separator.
2. Map: Map is like Struct, however, the fields are not predefined.
3. Union: `MemberType1 | MemberType2 | ...`
4. Array: `[ElementType]`

::: tip Tip 

If map filed name is a positive integer number, it is interpreted as an alternative representation of an `Array`. For example:

```
myarray.1 = 74
myarray.2 = 75
```

will be interpreted as `myarray = [74, 75]`, which is handy when trying to override array elements. 

:::

### Configuration Paths

If we consider the whole EMQX config as a tree, to reference a primitive value, we can use dot-separated names from string for the path from the tree root (always a Struct) down to the primitive values at tree-leaves.

Each segment of the dotted string is a Struct filed name or Map key. For Array elements, 1-based index is used.

Below are some examples

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

