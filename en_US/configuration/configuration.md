# Configuration Files

Users can configure EMQX with configuration files or environment variables. This section introduces the EMQX configuration files. It also provides the basic configuration instructions for the most commonly used functions in EMQX. For comprehensive configuration items with detailed explanations, see [EMQX Open Source Configuration Manual](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) and [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

## Config Directories

EMQX creates a group of directories after installation, among which, `etc` is the folder that keeps the static configuration files.

Depending on your installation mode, `etc` directory is:

| Installation                               | Path            |
| ------------------------------------------ | --------------- |
| Installed with RPM or DEB package          | `/etc/emqx`     |
| Running in docker container                | `/opt/emqx/etc` |
| Extracted from portable compressed package | `./etc`         |

At runtime, EMQX allows you to reconfigure the system by making changes from the dashbaord, REST API or CLI.
The changes will be persisted to the `data/configus` directory.

Depending on your installation mode, `data/configs` directory is:

| Installation                               | Path                     |
| ------------------------------------------ | ------------------------ |
| Installed with RPM or DEB package          | `/var/lib/emqx/configs`  |
| Running in docker container                | `/opt/emqx/data/configs` |
| Extracted from portable compressed package | `./data/configs`         |

::: tip
It is possible to change data directory from config `node.data_dir` or environment variable `EMQX_NODE__DATA_DIR`, however, when running a cluster, all nodes should have the same path.
:::

The reason why EMQX separates the configuration in two directories is to emphasize the difference between immutable/static configurations and mutable/dynamic configurations. This allows the `etc` directory to be a read-only directory, while the `data` directory is writable.

Although not encouraged, the content of the configuration files can overlap.
In case of overlapping, the conflict is resolved by a predefined override rule, see [Config Override Rules](#config-override-rules).

## Config Examples

Althogh there is a schema (see [Schema](#schema) section), examples often comes handy when configuring EMQX.
You can find examples provided in the `etc/examples` directory for references.

## Base Configuration File

Starting from EMQX 5.8.4, there is a base configuration file named `base.hocon` in the `etc` directory.

This file can be used to configure the basic settings which are to be overridden by the hihger levels of configuration files at runtime.

For example, you may want to start the deployment with a basic authentication configuration,
and then override it with a more complex configuration at runtime from the dashboard UI.

For immutable configurations such as `node` and `cluster` configs, it is **NOT** recommended to set them in the `base.hocon` file.
See [Immutable Configurations File](#immutable-configuration-file) for more details.

::: tip
The `base.hocon` file is not synchronized across the cluster, it is only applied to the node where it is located.
:::

## Configuration Rewrite File

In `data/configs` directory, the `cluster.hocon` file contains configuration items for the entire cluster.
Configuration changes made from Dashboard, REST API, and CLI will be persisted to this file.

If a certain cluster node is restarted or some new nodes are added, the node will automatically copy and apply
the configuration file from other nodes within the cluster, therefore it is not recommended to configure it manually.

The configs in thie file are overlayed on the configs from `base.hocon`.
For override rules, see [Config override rules](#config-override-rules).

Since version 5.1, when cluster configuration changes, EMQX backups the `cluster.hocon` file before overwriting it.
The backup files are suffixed with a timestamp of the node's local time. At most 10 backup files can be kept.

## Immutable Configuration File

For historical reasons, `emqx.conf` remains as the main configuration file for immediate parts of the system, such as `node`, and `cluster`.

It has higher priority than `base.hocon` and `cluster.hocon`, and lower priority than environment variables.
See [Config override rules](#config-override-rules) for more details.

## Configuration Paths

If we consider the whole EMQX config as a tree, to reference a primitive value, we can use dot-separated names from string for the path from the tree root (always a Struct) down to the primitive values at tree-leaves.

Each segment of the dotted string is a Struct filed name or Map key. For Array elements, 1-based index is used.

Below are some examples:

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

## HOCON Configuration Format

From EMQX v5.0, EMQX uses [Human-Optimized Config Object Notation (HOCON)](https://github.com/emqx/hocon) as the configuration file format.

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

## Config Override Rules

The value of HOCON will be overridden hierarchically, the rules are as follows:

- In the same file, the value defined in the later section will override any previous key value.
- A higher-level value will replace that of a lower-level.

The EMQX configuration is prioritized (overlayed) in the following order:  
`base.hocon < cluster.hocon < emqx.conf < environment variables`.  
That is, the configuration in `base.hocon` has the lowest priority, and can be overridden by higher-level configurations,
and Settings in environment variables that begin with `EMQX_` have the highest priority.

::: tip
Prior to version 5.8.4, there was no `base.hocon` file, otherwise the order of priority remains the same.
:::

Changes made through EMQX Dashboard UI, HTTP API, or CLI are persisted in `cluster.hocon` at runtime and will take effect immediately.
However, changes may get reverted after a node restart if the same configuration items are set differently in `emqx.conf` or environment variables.  
To avoid confusion, it is highly recommend NOT to have config overlap between `emqx.conf` and `cluster.hocon`.

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
  console {
    enable = true
    level = error
  }
}

## Set the console log printing level to debug, and keep the other configurations
log.console.level = debug
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

Arrays (in list format) will be fully overwritten and the original value cannot be kept, for example:

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

### Zone Override

A zone in EMQX is a concept for grouping configurations. Zones can be associated with listeners by setting the `zone` field to the name of the desired zone. MQTT clients connected to listeners associated with a zone will inherit the configurations from that zone, which may override global settings.

::: tip
By default, listeners are linked to a zone named `default`. The `default` zone is a logical grouping and does not exist in the configuration files.
:::

The following configuration items can be overridden at the zone level:

- `mqtt`: MQTT connection and session settings, such as allowing a greater maximum packet size for MQTT messages in a specific zone.
- `force_shutdown`: Policies for forced shutdowns.
- `force_gc`: Fine-tuning for Erlang process garbage collection.
- `flapping_detect`: Detection of client flapping.
- `durable_sessions`: Session persistence settings, such as enabling durable storage for MQTT sessions in a specific zone.

In EMQX version 5, the default configuration file does not include any zones, which differs from version 4, where there are two default zones: `internal` and `external`.

To create a zone, you need to define it in `emqx.conf`, for example:

```bash
zones {
  # Multiple zones can be defined
  my_zone1 {
    # Zones share the same configuration schema as the global configurations
    mqtt {
      # Allow a larger packet size for connections in this zone
      max_packet_size = 10M
    }
    force_shutdown {
      # Configuration specific to this zone
      ...
    }
    durable_sessions {
      # Enable durable storage for sessions in this zone
      enable = true
      ...
    }
  }
  my_zone2 {
    ...
  }
}
```

In a listener, set the `zone` field to associate it with a zone that has been created.

```bash
listeners.tcp.default {
    bind = 1883
    zone = my_zone1
    ...
}
```

## Schema

To make the HOCON objects type-safe, EMQX introduced a schema for it. This schema defines data types, field names, and metadata, allowing for configuration value validation and more.

The [EMQX Open Source Configuration Manual](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) and [EMQX Enterprise Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/) are generated from the schema.

::: tip
The zone configuration schema is not included in the configuration manual because it is identical for each group. For example, `zones.my_zone1.mqtt {...}` has the same schema as `mqtt {...}`.
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

### Variform Expressions

Variform is a lightweight, expressive language designed for string manipulation and runtime evaluation.
It is not a full-fledged programming language but a specialized tool that can be embedded within
configurations for EMQX to perform string operations dynamically.

::: tip
Variform expressions are only applicable to certain configuration items. Do not use them unless specifically stated.
:::

::: tip NULL value:
In Variform expressions, a value-binding reference or sub-expression evaluation may result in an undefined value, which is represented as an empty string (`''`).

It is important to note that if a JSON-decoded field is `null`, it is treated as undefined value (hence `''`), but not string value `"null"`.
:::

#### Syntax

To illustrate:

```js
function_call(clientid, another_function_call(username))
```

This expression combines or manipulates clientid and username to generate a new string value.

Variform supports below literals:

- Boolean: `true` or `false`.
- Integer: For example, `42`.
- Float: For example, `3.14`.
- String: ASCII characters between single quotes `'` or double quotes `"`.
- Array: Elements between `[` and `]`, separated by a comma `,`.
- Variable: Referencing to predefined values, for example `clientid`.
- Function: Predefined functions, for example, `concat([...])`.

Variform does not support the following:

- Arithmetic operations
- Loops
- User-defined variables
- User-defined functions
- Exception handling and error recovery
- Escape sequence in string literals. Call the `unescape` function to unescape special characters.

Below is a configuration example with a Variform expression embedded.

```js
mqtt {
    client_attrs_init = [
        {
            # Extract the prefix of client ID before the first -
            expression = "nth(1, tokens(clientid, '-'))"
            # And set as client_attrs.group
            set_as_attr = group
        }
    ]
}
```

::: tip
When an unescape function is required in the expression, it's a good idea to use triple quote (`"""`) strings in HOCON config so there is no need to perform double escaping.

For example

```
#### For multi-line client ID, take the first line.
expression = """nth(1, tokens(clientid, unescape('\n')))"""
```
:::

#### Pre-defined Functions

EMQX includes a rich set of string, array, random, and hashing functions similar to those available in rule engine string functions.
These functions can be used to manipulate and format the extracted data. For instance, `lower()`, `upper()`,
and `concat()` help in adjusting the format of extracted strings, while `hash()` and `hash_to_range()` allow for creating hashed or ranged outputs based on the data.

Below are the functions that can be used in the expressions:

- **String functions**:
  - [String Operation Functions](../data-integration/rule-sql-builtin-functions.md#string-operation-functions)
  - A new function `any_to_string/1` is also added to convert any intermediate non-string value to a string.
- **Array functions**: [nth/2](../data-integration/rule-sql-builtin-functions.md#nth-n-integer-array-array-any)
- **Random functions**: rand_str, rand_int
- **Schema-less encode/decode functions**:
  - [bin2hexstr(Data)](../data-integration/rule-sql-builtin-functions.md#bin2hexstr-data-binary-string)
  - [hexstr2bin(Data)](../data-integration/rule-sql-builtin-functions.md#hexstr2bin-data-string-binary)
  - [base64_decode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-decode-data-string-bytes-string)
  - [base64_encode(Data)](../data-integration/rule-sql-builtin-functions.md#base64-encode-data-string-bytes-string)
  - `int2hexstr(Integer)`: Encode an integer to hex string. e.g. 15 as 'F' (uppercase).
- **Hash functions**:
  - `hash(Algorihtm, Data)`: Algorithm can be one of: md4 | md5, sha (or sha1) | sha224 | sha256 | sha384 | sha512 | sha3_224 | sha3_256 | sha3_384 | sha3_512 | shake128 | shake256 | blake2b | blake2s
  - `hash_to_range(Input, Min, Max)`: Use sha256 to hash the Input data and map the hash to an integer between Min and Max inclusive ( Min =< X =< Max)
  - `map_to_rage(Input, Min, Max)`: Map the input to an integer between Min and Max inclusive (Min =< X =< Max)
- **Compare functions**:
  - `num_eq(A, B)`: Return 'true' if two numbers are the same, otherwise 'false'.
  - `num_neq(A, B)`: Return 'true' if two numbers are NOT the same, otherwise 'false'.
  - `num_gt(A, B)`: Return 'true' if A is greater than B, otherwise 'false'.
  - `num_gte(A, B)`: Return 'true' if A is not less than B, otherwise 'false'.
  - `num_lt(A, B)`: Return 'true' if A is less than B, otherwise 'false'.
  - `num_lte(A, B)`: Return 'true' if A is not greater than B, otherwise 'false'.
  - `str_eq(A, B)`: Return 'true' if two strings are the same, otherwise 'false'.
  - `str_neq(A, B)`: Return 'true' if two strings are NOT the same, otherwise 'false'
  - `str_gt(A, B)`: Return 'true' if A is behind B in lexicographic order, otherwise 'false'.
  - `str_gte(A, B)`: Return 'true' if A is not before B in lexicographic order, otherwise 'false'.
  - `str_lt(A, B)`: Return 'true' if A is before B in lexicographic order, otherwise 'false'.
  - `str_lte(A, B)`: Return 'true' if A is not after B in lexicographic order, otherwise 'false'.
  - `is_empty_var(V)`: Check if a variable is empty. Empty in Variform means the value is not present (`undefined`), JSON's `null` (but not string `"null"`), or an empty string `""`.
  - `not(Bool)`: Return `true` if `Bool` is `false`, and return `false` if the condition is `true`. It also accepts string parameters. If the input is a string, the output is also a string.

- **System functions**:
  - `getenv(Name)`: Return the value of the environment variable `Name` with the following constraints:
    - Prefix `EMQXVAR_` is added before reading from OS environment variables. For example, `getenv('FOO_BAR')` is to read `EMQXVAR_FOO_BAR`.
    - Values are immutable once loaded from the OS environment.

#### Conditions

The variform expression so far has no comprehensive control flows.
Below functions can help to preform basic control of which value to return from the expression.

- `iif(Condition, ThenExpression, ElseExpression)`: returns `ThenExpression` if `Condition` yields `true` or a non-empty string value, otherwise returns `ElseExpression`.
- `coalesce(Arg1, Arg2, ...)`: returns the first non-empty argument.
- `coalesce([Element1, Element2, ...])`: returns the first non-empty element.

#### Error Handling

As the default behavior of scripting environments like Bash, Variform expression is designed to yield an empty string ("") in scenarios where errors occur, such as unbound variables or exceptions during runtime.

- Unbound Variables: If an expression references a variable that has not been defined or is out of scope (unbound), the expression will be evaluated as an empty string.
- Runtime Exceptions: Any exceptions that occur during the execution of an expression, whether due to incorrect function usage, invalid data types, or other unforeseen issues, will result in the expression yielding an empty string. For example, the array index is out of range.

#### Example Expressions

- `nth(1, tokens(clientid, '.'))`:  Extract the prefix of a dot-separated client ID.
- `strlen(username, 0, 5)`: Extract a partial username.
- `coalesce(regex_extract(clientid,'[0-9]+'),'vin-1000')`: Extract digits from client ID using a regular expression. If the regular expression yields empty string, then return `'000'`.
- `iif(true, "Value if true", "Value if false")`: Returns `Value if true`
- `iif("", "Value if true", "Value if false")`: Returns `Value if false`
- `iif("hello", "Value if true", "Value if false")`: Returns `Value if true`
- `iif(regex_match(clientid,'^foo\.+*'),'foo','bar')`: Returns `foo` if `clientid` starts with `foo.`, otherwise `bar`.
