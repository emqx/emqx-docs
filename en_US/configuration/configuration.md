# Configuration Files

<!-- 中文部分翻译 -->

<!-- Schema 沿用英文的描述 -->
## Schema

To make the HOCON objects type-safe, EMQX introduced a schema for it.
The schema defines data types, and data fields' names and metadata for config value validation
and more.

::: tip Tip
The configuration document you are reading now is generated from schema metadata.
:::

### Primitive Data Types

Complex types define data 'boxes' which may contain other complex data
or primitive values.
There are quite some different primitive types, to name a few:

- `atom()`.
- `boolean()`.
- `string()`.
- `integer()`.
- `float()`.
- `number()`.
- `binary()`, another format of string().
- `emqx_schema:duration()`, time duration, another format of integer()
- ...

::: tip Tip
The primitive types are mostly self-describing, so there is usually not a lot to document.
For types that are not so clear by their names, the field description is to be used to find the details.
:::

### Complex Data Types

There are 4 complex data types in EMQX's HOCON config:

1. Struct: Named using an unquoted string, followed by a predefined list of fields.
   Only lowercase letters and digits are allowed in struct and field names.
   Alos, only underscore can be used as word separator.
1. Map: Map is like Struct, however the fields are not predefined.
1. Union: `MemberType1 | MemberType2 | ...`
1. Array: `[ElementType]`

::: tip Tip
If map filed name is a positive integer number, it is interpreted as an alternative representation of an `Array`.
For example:

```bash
myarray.1 = 74
myarray.2 = 75
```

will be interpreated as `myarray = [74, 75]`, which is handy when trying to override array elements.
:::

### Config Paths

If we consider the whole EMQX config as a tree,
to reference a primitive value, we can use a dot-separated names form string for
the path from the tree-root (always a Struct) down to the primitive values at tree-leaves.

Each segment of the dotted string is a Struct filed name or Map key.
For Array elements, 1-based index is used.

below are some examples

```bash
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

### TLS/SSL ciphers

Starting from v5.0.6, EMQX no longer pre-populates the ciphers list with a default
set of cipher suite names.
Instead, the default ciphers are applied at runtime when starting the listener
for servers, or when establishing a TLS connection as a client.

Below are the default ciphers selected by EMQX.

For tlsv1.3:

```bash
ciphers =
  [ "TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256",
    "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256",
    "TLS_AES_128_CCM_8_SHA256"
  ]
```

For tlsv1.2 or earlier

```bash
ciphers =
  [ "ECDHE-ECDSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-ECDSA-AES256-SHA384",
    "ECDHE-RSA-AES256-SHA384",
    "ECDH-ECDSA-AES256-GCM-SHA384",
    "ECDH-RSA-AES256-GCM-SHA384",
    "ECDH-ECDSA-AES256-SHA384",
    "ECDH-RSA-AES256-SHA384",
    "DHE-DSS-AES256-GCM-SHA384",
    "DHE-DSS-AES256-SHA256",
    "AES256-GCM-SHA384",
    "AES256-SHA256",
    "ECDHE-ECDSA-AES128-GCM-SHA256",
    "ECDHE-RSA-AES128-GCM-SHA256",
    "ECDHE-ECDSA-AES128-SHA256",
    "ECDHE-RSA-AES128-SHA256",
    "ECDH-ECDSA-AES128-GCM-SHA256",
    "ECDH-RSA-AES128-GCM-SHA256",
    "ECDH-ECDSA-AES128-SHA256",
    "ECDH-RSA-AES128-SHA256",
    "DHE-DSS-AES128-GCM-SHA256",
    "DHE-DSS-AES128-SHA256",
    "AES128-GCM-SHA256",
    "AES128-SHA256",
    "ECDHE-ECDSA-AES256-SHA",
    "ECDHE-RSA-AES256-SHA",
    "DHE-DSS-AES256-SHA",
    "ECDH-ECDSA-AES256-SHA",
    "ECDH-RSA-AES256-SHA",
    "ECDHE-ECDSA-AES128-SHA",
    "ECDHE-RSA-AES128-SHA",
    "DHE-DSS-AES128-SHA",
    "ECDH-ECDSA-AES128-SHA",
    "ECDH-RSA-AES128-SHA"
  ]
```

For PSK enabled listeners

```bash
ciphers =
  [ "RSA-PSK-AES256-GCM-SHA384",
    "RSA-PSK-AES256-CBC-SHA384",
    "RSA-PSK-AES128-GCM-SHA256",
    "RSA-PSK-AES128-CBC-SHA256",
    "RSA-PSK-AES256-CBC-SHA",
    "RSA-PSK-AES128-CBC-SHA"
  ]
```
