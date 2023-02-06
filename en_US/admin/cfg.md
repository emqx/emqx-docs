# Configuration Files

<!--5.0.12-->
EMQX configuration files are in [HOCON](https://github.com/emqx/hocon) format.
HOCON, or Human-Optimized Config Object Notation is a format for human-readable data,
and a superset of JSON.

## Layered

EMQX configuration consists of 3 layers.
From bottom up:

1. Immutable base: `emqx.conf` + `EMQX_` prefixed environment variables.<br/>
   Changes in this layer require a full node restart to take effect.
1. Cluster overrides: `$EMQX_NODE__DATA_DIR/configs/cluster-override.conf`
1. Local node overrides: `$EMQX_NODE__DATA_DIR/configs/local-override.conf`

When environment variable `$EMQX_NODE__DATA_DIR` is not set, config `node.data_dir`
is used.

The `cluster-override.conf` file is overwritten at runtime when changes
are made from dashboard UI, management HTTP API, or CLI. When clustered,
after EMQX restarts, it copies the file from the node which has the greatest `uptime`.

:::tip Tip
Some of the configs (such as `node.name`) are boot-only configs and not overridable.
Config values from `*-override.conf` are **not** mapped to boot configs for
the config fields attributed with `mapping: path.to.boot.config.key`
:::

For detailed override rules, see [Config Overlay Rules](#config-overlay-rules).

## Syntax

In config file the values can be notated as JSON like objects, such as
```
node {
    name = "emqx@127.0.0.1"
    cookie = "mysecret"
}
```

Another equivalent representation is flat, such as

```
node.name = "127.0.0.1"
node.cookie = "mysecret"
```

This flat format is almost backward compatible with EMQX's config file format
in 4.x series (the so called 'cuttlefish' format).

It is not fully compatible because the often HOCON requires strings to be quoted,
while cuttlefish treats all characters to the right of the `=` mark as the value.

e.g. cuttlefish: `node.name = emqx@127.0.0.1`, HOCON: `node.name = "emqx@127.0.0.1"`.

Strings without special characters in them can be unquoted in HOCON too,
e.g. `foo`, `foo_bar` and `foo_bar_1`.

For more HOCON syntax, please refer to the [specification](https://github.com/lightbend/config/blob/main/HOCON.md)

## Schema

To make the HOCON objects type-safe, EMQX introduced a schema for it.
The schema defines data types, and data fields' names and metadata for config value validation
and more.

::: tip Tip
The configuration document you are reading now is generated from schema metadata.
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
```
myarray.1 = 74
myarray.2 = 75
```
will be interpreated as `myarray = [74, 75]`, which is handy when trying to override array elements.
:::

### Primitive Data Types

Complex types define data 'boxes' which may contain other complex data
or primitive values.
There are quite some different primitive types, to name a few:

* `atom()`.
* `boolean()`.
* `string()`.
* `integer()`.
* `float()`.
* `number()`.
* `binary()`, another format of string().
* `emqx_schema:duration()`, time duration, another format of integer()
* ...

::: tip Tip
The primitive types are mostly self-describing, so there is usually not a lot to document.
For types that are not so clear by their names, the field description is to be used to find the details.
:::

### Config Paths

If we consider the whole EMQX config as a tree,
to reference a primitive value, we can use a dot-separated names form string for
the path from the tree-root (always a Struct) down to the primitive values at tree-leaves.

Each segment of the dotted string is a Struct filed name or Map key.
For Array elements, 1-based index is used.

below are some examples

```
node.name = "emqx.127.0.0.1"
zone.zone1.max_packet_size = "10M"
authentication.1.enable = true
```

### Environment variables

Environment variables can be used to define or override config values.

Due to the fact that dots (`.`) are not allowed in environment variables, dots are
replaced with double-underscores (`__`).

And the `EMQX_` prefix is used as the namespace.

For example `node.name` can be represented as `EMQX_NODE__NAME`

Environment variable values are parsed as HOCON values, this allows users
to even set complex values from environment variables.

For example, this environment variable sets an array value.

```
export EMQX_LISTENERS__SSL__L1__AUTHENTICATION__SSL__CIPHERS='["TLS_AES_256_GCM_SHA384"]'
```

However this also means a string value should be quoted if it happens to contain special
characters such as `=` and `:`.

For example, a string value `"localhost:1883"` would be 
parsed into object (struct): `{"localhost": 1883}`.

To keep it as a string, one should quote the value like below:

```
EMQX_BRIDGES__MQTT__MYBRIDGE__CONNECTOR_SERVER='"localhost:1883"'
```

::: tip Tip
Unknown root paths are silently discarded by EMQX, for example `EMQX_UNKNOWN_ROOT__FOOBAR` is
silently discarded because `unknown_root` is not a predefined root path.

Unknown field names in environment variables are logged as a `warning` level log, for example:

```
[warning] unknown_env_vars: ["EMQX_AUTHENTICATION__ENABLED"]
```

because the field name is `enable`, not `enabled`.
:::


### Config Overlay Rules

HOCON objects are overlaid, in general:

- Within one file, objects defined 'later' recursively override objects defined 'earlier'
- When layered, 'later' (higher layer) objects override objects defined 'earlier' (lower layer)

Below are more detailed rules.

#### Struct Fields

Later config values overwrites earlier values.
For example, in below config, the last line `debug` overwrites `error` for
console log handler's `level` config, but leaving `enable` unchanged.
```
log {
    console_handler{
        enable=true,
        level=error
    }
}

## ... more configs ...

log.console_handler.level=debug
```

#### Map Values

Maps are like structs, only the files are user-defined rather than
the config schema. For instance, `zone1` in the example below.

```
zone {
    zone1 {
        mqtt.max_packet_size = 1M
    }
}

## The maximum packet size can be defined as above,
## then overridden as below

zone.zone1.mqtt.max_packet_size = 10M
```

#### Array Elements

Arrays in EMQX config have two different representations

* list, such as: `[1, 2, 3]`
* indexed-map, such as: `{"1"=1, "2"=2, "3"=3}`

Dot-separated paths with number in it are parsed to indexed-maps
e.g. `authentication.1={...}` is parsed as `authentication={"1": {...}}`

This feature makes it easy to override array elment values. For example:

```
authentication=[{enable=true, backend="built_in_database", mechanism="password_based"}]
# we can disable this authentication provider with:
authentication.1.enable=false
```

::: warning Warning
List arrays is a full-array override, but not a recursive merge, into indexed-map arrays.
e.g.

```
authentication=[{enable=true, backend="built_in_database", mechanism="password_based"}]
## below value will replace the whole array, but not to override just one field.
authentication=[{enable=true}]
```
:::

#### TLS/SSL ciphers

Starting from v5.0.6, EMQX no longer pre-populates the ciphers list with a default
set of cipher suite names.
Instead, the default ciphers are applied at runtime when starting the listener
for servers, or when establishing a TLS connection as a client.

Below are the default ciphers selected by EMQX.

For tlsv1.3:
```
ciphers =
  [ "TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256",
    "TLS_CHACHA20_POLY1305_SHA256", "TLS_AES_128_CCM_SHA256",
    "TLS_AES_128_CCM_8_SHA256"
  ]
```

For tlsv1.2 or earlier

```
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

```
ciphers =
  [ "RSA-PSK-AES256-GCM-SHA384",
    "RSA-PSK-AES256-CBC-SHA384",
    "RSA-PSK-AES128-GCM-SHA256",
    "RSA-PSK-AES128-CBC-SHA256",
    "RSA-PSK-AES256-CBC-SHA",
    "RSA-PSK-AES128-CBC-SHA"
  ]
```


## Root Config Keys




**Fields**

- listeners: <code>[broker:listeners](#broker-listeners)</code>



- zones: <code>{$name -> [broker:zone](#broker-zone)}</code>

  A zone is a set of configs grouped by the zone <code>name</code>.
  For flexible configuration mapping, the <code>name</code> can be set to a listener's <code>zone</code> config.
  NOTE: A built-in zone named <code>default</code> is auto created and can not be deleted.


- mqtt: <code>[broker:mqtt](#broker-mqtt)</code>

  Global MQTT configuration.
  The configs here work as default values which can be overridden in <code>zone</code> configs


- authentication: <code>[[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)]</code>

  Default authentication configs for all MQTT listeners.

  For per-listener overrides see <code>authentication</code> in listener configs

  This option can be configured with:
  <ul>
    <li><code>[]</code>: The default value, it allows *ALL* logins</li>
    <li>one: For example <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
    <li>chain: An array of structs.</li>
  </ul>

  When a chain is configured, the login credentials are checked against the backends per the configured order, until an 'allow' or 'deny' decision can be made.

  If there is no decision after a full chain exhaustion, the login is rejected.


- authorization: <code>[authorization](#authorization)</code>


  Authorization a.k.a. ACL.<br/>
  In EMQX, MQTT client access control is extremely flexible.<br/>
  An out-of-the-box set of authorization data sources are supported.
  For example,<br/>
  'file' source is to support concise and yet generic ACL rules in a file;<br/>
  'built_in_database' source can be used to store per-client customizable rule sets,
  natively in the EMQX node;<br/>
  'http' source to make EMQX call an external HTTP API to make the decision;<br/>
  'PostgreSQL' etc. to look up clients or rules from external databases;<br/>


- node: <code>[node](#node)</code>



- cluster: <code>[cluster](#cluster)</code>



- log: <code>[log](#log)</code>



- rpc: <code>[rpc](#rpc)</code>



- broker: <code>[broker](#broker)</code>

  Message broker options.

- sys_topics: <code>[broker:sys_topics](#broker-sys_topics)</code>

  System topics configuration.

- force_shutdown: <code>[broker:force_shutdown](#broker-force_shutdown)</code>



- overload_protection: <code>[broker:overload_protection](#broker-overload_protection)</code>



- force_gc: <code>[broker:force_gc](#broker-force_gc)</code>



- conn_congestion: <code>[broker:conn_congestion](#broker-conn_congestion)</code>



- stats: <code>[broker:stats](#broker-stats)</code>



- sysmon: <code>[broker:sysmon](#broker-sysmon)</code>



- alarm: <code>[broker:alarm](#broker-alarm)</code>



- flapping_detect: <code>[broker:flapping_detect](#broker-flapping_detect)</code>



- persistent_session_store: <code>[broker:persistent_session_store](#broker-persistent_session_store)</code>



- trace: <code>[broker:trace](#broker-trace)</code>



- bridges: <code>[bridge:bridges](#bridge-bridges)</code>



- retainer: <code>[retainer](#retainer)</code>



- statsd: <code>[statsd](#statsd)</code>



- auto_subscribe: <code>[auto_subscribe](#auto_subscribe)</code>



- delayed: <code>[modules:delayed](#modules-delayed)</code>



- telemetry: <code>[modules:telemetry](#modules-telemetry)</code>



- rewrite: <code>[[modules:rewrite](#modules-rewrite)]</code>

  List of topic rewrite rules.

- topic_metrics: <code>[[modules:topic_metrics](#modules-topic_metrics)]</code>

  List of topics whose metrics are reported.

- plugins: <code>[plugin:plugins](#plugin-plugins)</code>



- dashboard: <code>[dashboard](#dashboard)</code>



- gateway: <code>[gateway](#gateway)</code>



- prometheus: <code>[prometheus](#prometheus)</code>



- rule_engine: <code>[rule_engine](#rule_engine)</code>



- exhook: <code>[exhook](#exhook)</code>



- psk_authentication: <code>[authn-psk:psk_authentication](#authn-psk-psk_authentication)</code>



- limiter: <code>[limiter](#limiter)</code>



- slow_subs: <code>[slow_subs](#slow_subs)</code>




## authz:file
Authorization using a static file.


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>file</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- path: <code>string()</code>


  Path to the file which contains the ACL rules.
  If the file provisioned before starting EMQX node,
  it can be placed anywhere as long as EMQX has read access to it.
  That is, EMQX will treat it as read only.

  In case the rule-set is created or updated from EMQX Dashboard or HTTP API,
  a new file will be created and placed in `authz` subdirectory inside EMQX's `data_dir`,
  and the old file will not be used anymore.



## authz:http_get
Authorization using an external HTTP server (via GET requests).


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>http</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- url: <code>binary()</code>

  URL of the auth server.

- request_timeout: <code>string()</code>
  * default: 
  `"30s"`

  HTTP request timeout.

- body: <code>map()</code>

  HTTP request body.

- connect_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  The timeout when connecting to the HTTP server.

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  The pool size.

- request: <code>[connector-http:request](#connector-http-request)</code>


  If the request is provided, the caller can send HTTP requests via
  <code>emqx_resource:query(ResourceId, {send_message, BridgeId, Message})</code>


- retry_interval: <code>emqx_schema:duration()</code>

  Deprecated since 5.0.4.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- method: <code>get</code>
  * default: 
  `get`

  HTTP method.

- headers: <code>[{binary(), binary()}]</code>
  * default: 

  ```
  {
    accept = "application/json"
    "cache-control" = "no-cache"
    connection = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
  }
  ```

  List of HTTP headers (without <code>content-type</code>).


## authz:http_post
Authorization using an external HTTP server (via POST requests).


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>http</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- url: <code>binary()</code>

  URL of the auth server.

- request_timeout: <code>string()</code>
  * default: 
  `"30s"`

  HTTP request timeout.

- body: <code>map()</code>

  HTTP request body.

- connect_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  The timeout when connecting to the HTTP server.

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  The pool size.

- request: <code>[connector-http:request](#connector-http-request)</code>


  If the request is provided, the caller can send HTTP requests via
  <code>emqx_resource:query(ResourceId, {send_message, BridgeId, Message})</code>


- retry_interval: <code>emqx_schema:duration()</code>

  Deprecated since 5.0.4.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- method: <code>post</code>
  * default: 
  `post`

  HTTP method.

- headers: <code>[{binary(), binary()}]</code>
  * default: 

  ```
  {
    accept = "application/json"
    "cache-control" = "no-cache"
    connection = "keep-alive"
    "content-type" = "application/json"
    "keep-alive" = "timeout=30, max=1000"
  }
  ```

  List of HTTP Headers.


## authz:mnesia
Authorization using a built-in database (mnesia).


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>built_in_database</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider


## authz:mongo_rs
Authorization using a MongoDB replica set.


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>mongodb</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- collection: <code>atom()</code>

  `MongoDB` collection containing the authorization data.

- filter: <code>map()</code>
  * default: 
  `{}`


  Conditional expression that defines the filter condition in the query.
  Filter supports the following placeholders:
   - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
   - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


- mongo_type: <code>rs</code>
  * default: 
  `rs`

  Replica set.

- servers: <code>[term()]</code>


  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The MongoDB default port 27017 is used if `[:Port]` is not specified.


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  Write mode.

- r_mode: <code>master | slave_ok</code>
  * default: 
  `master`

  Read mode.

- replica_set_name: <code>binary()</code>

  Name of the replica set.

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  Use DNS SRV record.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- username: <code>binary()</code>

  EMQX's username in the external database.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authz:mongo_sharded
Authorization using a sharded MongoDB cluster.


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>mongodb</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- collection: <code>atom()</code>

  `MongoDB` collection containing the authorization data.

- filter: <code>map()</code>
  * default: 
  `{}`


  Conditional expression that defines the filter condition in the query.
  Filter supports the following placeholders:
   - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
   - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


- mongo_type: <code>sharded</code>
  * default: 
  `sharded`

  Sharded cluster.

- servers: <code>[term()]</code>


  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The MongoDB default port 27017 is used if `[:Port]` is not specified.


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  Write mode.

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  Use DNS SRV record.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- username: <code>binary()</code>

  EMQX's username in the external database.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authz:mongo_single
Authorization using a single MongoDB instance.


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>mongodb</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- collection: <code>atom()</code>

  `MongoDB` collection containing the authorization data.

- filter: <code>map()</code>
  * default: 
  `{}`


  Conditional expression that defines the filter condition in the query.
  Filter supports the following placeholders:
   - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
   - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


- mongo_type: <code>single</code>
  * default: 
  `single`

  Standalone instance.

- server: <code>emqx_schema:host_port()</code>


  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The MongoDB default port 27017 is used if `[:Port]` is not specified.


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  Write mode.

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  Use DNS SRV record.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- username: <code>binary()</code>

  EMQX's username in the external database.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authz:mysql
Authorization using a MySQL database.


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>mysql</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- server: <code>emqx_schema:host_port()</code>


  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The MySQL default port 3306 is used if `[:Port]` is not specified.


- database: <code>binary()</code>

  Database name.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- username: <code>binary()</code>

  EMQX's username in the external database.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  Enable automatic reconnect to the database.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- prepare_statement: <code>map()</code>

  Key-value list of SQL prepared statements.

- query: <code>binary()</code>

  Database query used to retrieve authorization data.


## authz:postgresql
Authorization using a PostgreSQL database.


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>postgresql</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- server: <code>emqx_schema:host_port()</code>


  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The PostgreSQL default port 5432 is used if `[:Port]` is not specified.


- database: <code>binary()</code>

  Database name.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- username: <code>binary()</code>

  EMQX's username in the external database.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  Enable automatic reconnect to the database.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- prepare_statement: <code>map()</code>

  Key-value list of SQL prepared statements.

- query: <code>binary()</code>

  Database query used to retrieve authorization data.


## authz:redis_cluster
Authorization using a Redis cluster.


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>redis</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- servers: <code>[term()]</code>


  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The MongoDB default port 27017 is used if `[:Port]` is not specified.


- redis_type: <code>cluster</code>
  * default: 
  `cluster`

  Cluster mode

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  Enable automatic reconnect to the database.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- cmd: <code>binary()</code>

  Database query used to retrieve authorization data.


## authz:redis_sentinel
Authorization using a Redis Sentinel.


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>redis</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- servers: <code>[term()]</code>


  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The MongoDB default port 27017 is used if `[:Port]` is not specified.


- redis_type: <code>sentinel</code>
  * default: 
  `sentinel`

  Sentinel mode

- sentinel: <code>string()</code>

  The cluster name in Redis sentinel mode.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- password: <code>binary()</code>

  EMQX's password in the external database.

- database: <code>integer()</code>
  * default: 
  `0`

  Redis database ID.

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  Enable automatic reconnect to the database.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- cmd: <code>binary()</code>

  Database query used to retrieve authorization data.


## authz:redis_single
Authorization using a single Redis instance.


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>redis</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- server: <code>emqx_schema:host_port()</code>


  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The Redis default port 6379 is used if `[:Port]` is not specified.


- redis_type: <code>single</code>
  * default: 
  `single`

  Single mode

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- password: <code>binary()</code>

  EMQX's password in the external database.

- database: <code>integer()</code>
  * default: 
  `0`

  Redis database ID.

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  Enable automatic reconnect to the database.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- cmd: <code>binary()</code>

  Database query used to retrieve authorization data.


## broker:alarm
Settings for the alarms.


**Config paths**

 - <code>alarm</code>


**Env overrides**

 - <code>EMQX_ALARM</code>



**Fields**

- actions: <code>[atom()]</code>
  * default: 
  `[log, publish]`

  The actions triggered when the alarm is activated.<br/>Currently, the following actions are supported: <code>log</code> and <code>publish</code>.
  <code>log</code> is to write the alarm to log (console or file).
  <code>publish</code> is to publish the alarm as an MQTT message to the system topics:
  <code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/activate</code> and
  <code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/deactivate</code>

- size_limit: <code>1..3000</code>
  * default: 
  `1000`

  The maximum total number of deactivated alarms to keep as history.<br/>When this limit is exceeded, the oldest deactivated alarms are deleted to cap the total number.


- validity_period: <code>emqx_schema:duration()</code>
  * default: 
  `"24h"`

  Retention time of deactivated alarms. Alarms are not deleted immediately
  when deactivated, but after the retention time.



## broker
Message broker options.


**Config paths**

 - <code>broker</code>


**Env overrides**

 - <code>EMQX_BROKER</code>



**Fields**

- enable_session_registry: <code>boolean()</code>
  * default: 
  `true`

  Enable session registry

- session_locking_strategy: <code>local | leader | quorum | all</code>
  * default: 
  `quorum`

  Session locking strategy in a cluster.
    - `local`: only lock the session on the current node
    - `one`: select only one remote node to lock the session
    - `quorum`: select some nodes to lock the session
    - `all`: lock the session on all the nodes in the cluster


- shared_subscription_strategy: <code>random | round_robin | round_robin_per_group | sticky | local | hash_topic | hash_clientid</code>
  * default: 
  `round_robin`

  Dispatch strategy for shared subscription.
    - `random`: dispatch the message to a random selected subscriber
    - `round_robin`: select the subscribers in a round-robin manner
    - `sticky`: always use the last selected subscriber to dispatch, until the subscriber disconnects.
    - `hash`: select the subscribers by the hash of `clientIds`


- shared_dispatch_ack_enabled: <code>boolean()</code>
  * default: 
  `false`

  Enable/disable shared dispatch acknowledgement for QoS 1 and QoS 2 messages.
  This should allow messages to be dispatched to a different subscriber in the group in case the picked (based on `shared_subscription_strategy`) subscriber is offline.


- route_batch_clean: <code>boolean()</code>
  * default: 
  `true`

  Enable batch clean for deleted routes.

- perf: <code>[broker:broker_perf](#broker-broker_perf)</code>



- shared_subscription_group: <code>{$name -> [broker:shared_subscription_group](#broker-shared_subscription_group)}</code>

  Per group dispatch strategy for shared subscription.
  This config is a map from shared subscription group name to the strategy
  name. The group name should be of format `[A-Za-z0-9]`. i.e. no
  special characters are allowed.



## broker:broker_perf
Broker performance tuning parameters.


**Config paths**

 - <code>broker.perf</code>


**Env overrides**

 - <code>EMQX_BROKER__PERF</code>



**Fields**

- route_lock_type: <code>key | tab | global</code>
  * default: 
  `key`

  Performance tuning for subscribing/unsubscribing a wildcard topic.
  Change this parameter only when there are many wildcard topics.

  NOTE: when changing from/to `global` lock, it requires all nodes in the cluster to be stopped before the change.
    - `key`: mnesia transactional updates with per-key locks. Recommended for a single-node setup.
    - `tab`: mnesia transactional updates with table lock. Recommended for a cluster setup.
    - `global`: updates are protected with a global lock. Recommended for large clusters.


- trie_compaction: <code>boolean()</code>
  * default: 
  `true`

  Enable trie path compaction.
  Enabling it significantly improves wildcard topic subscribe rate, if wildcard topics have unique prefixes like: 'sensor/{{id}}/+/', where ID is unique per subscriber.
  Topic match performance (when publishing) may degrade if messages are mostly published to topics with large number of levels.

  NOTE: This is a cluster-wide configuration. It requires all nodes to be stopped before changing it.



## broker:cache
Settings for the authorization cache.


**Config paths**

 - <code>authorization.cache</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__CACHE</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable or disable the authorization cache.

- max_size: <code>1..1048576</code>
  * default: 
  `32`

  Maximum number of cached items.

- ttl: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  Time to live for the cached data.  


## broker:conn_congestion
Settings for `conn_congestion` alarm.

Sometimes the MQTT connection (usually an MQTT subscriber) may
get "congested", because there are too many packets to be sent.
The socket tries to buffer the packets until the buffer is
full. If more packets arrive after that, the packets will be
"pending" in the queue, and we consider the connection
congested.

Note: `sndbuf` can be set to larger value if the
alarm is triggered too often.
The name of the alarm is of format `conn_congestion/<ClientID>/<Username>`,
where the `<ClientID>` is the client ID of the congested MQTT connection,
and `<Username>` is the username or `unknown_user`.


**Config paths**

 - <code>conn_congestion</code>


**Env overrides**

 - <code>EMQX_CONN_CONGESTION</code>



**Fields**

- enable_alarm: <code>boolean()</code>
  * default: 
  `true`

  Enable or disable connection congestion alarm.

- min_alarm_sustain_duration: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  Minimal time before clearing the alarm.<br/>The alarm is cleared only when there's no pending data in<br/>the queue, and at least <code>min_alarm_sustain_duration</code>milliseconds passed since the last time we considered the connection 'congested'.<br/>This is to avoid clearing and raising the alarm again too often.


## broker:deflate_opts
Compression options.


**Config paths**

 - <code>listeners.ws.$name.websocket.deflate_opts</code>
 - <code>listeners.wss.$name.websocket.deflate_opts</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WS__$NAME__WEBSOCKET__DEFLATE_OPTS</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__WEBSOCKET__DEFLATE_OPTS</code>



**Fields**

- level: <code>none | default | best_compression | best_speed</code>

  Compression level. 

- mem_level: <code>1..9</code>
  * default: 
  `8`


  Specifies the size of the compression state.<br/>
  Lower values decrease memory usage per connection.


- strategy: <code>default | filtered | huffman_only | rle</code>
  * default: 
  `default`

  Specifies the compression strategy.

- server_context_takeover: <code>takeover | no_takeover</code>
  * default: 
  `takeover`

  Takeover means the compression state is retained between server messages. 

- client_context_takeover: <code>takeover | no_takeover</code>
  * default: 
  `takeover`

  Takeover means the compression state is retained between client messages. 

- server_max_window_bits: <code>8..15</code>
  * default: 
  `15`

  Specifies the size of the compression context for the server.

- client_max_window_bits: <code>8..15</code>
  * default: 
  `15`

  Specifies the size of the compression context for the client.


## broker:event_names
Enable or disable client lifecycle event publishing.

The following options affect MQTT clients as well as
gateway clients. The types of the clients
are distinguished by the topic prefix:

- For the MQTT clients, the format is:
`$SYS/broker/<node>/clients/<clientid>/<event>`
- For the Gateway clients, it is
`$SYS/broker/<node>/gateway/<gateway-name>/clients/<clientid>/<event>`



**Config paths**

 - <code>sys_topics.sys_event_messages</code>


**Env overrides**

 - <code>EMQX_SYS_TOPICS__SYS_EVENT_MESSAGES</code>



**Fields**

- client_connected: <code>boolean()</code>
  * default: 
  `true`

  Enable to publish client connected event messages

- client_disconnected: <code>boolean()</code>
  * default: 
  `true`

  Enable to publish client disconnected event messages.

- client_subscribed: <code>boolean()</code>
  * default: 
  `false`

  Enable to publish event message that client subscribed a topic successfully.

- client_unsubscribed: <code>boolean()</code>
  * default: 
  `false`

  Enable to publish event message that client unsubscribed a topic successfully.


## broker:flapping_detect
This config controls the allowed maximum number of `CONNECT` packets received
from the same clientid in a time frame defined by `window_time`.
After the limit is reached, successive `CONNECT` requests are forbidden
(banned) until the end of the time period defined by `ban_time`.


**Config paths**

 - <code>flapping_detect</code>


**Env overrides**

 - <code>EMQX_FLAPPING_DETECT</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable flapping connection detection feature.

- max_count: <code>integer()</code>
  * default: 
  `15`

  The maximum number of disconnects allowed for a MQTT Client in `window_time`

- window_time: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  The time window for flapping detection.

- ban_time: <code>emqx_schema:duration()</code>
  * default: 
  `"5m"`

  How long the flapping clientid will be banned.


## broker:force_gc
Force garbage collection in MQTT connection process after
 they process certain number of messages or bytes of data.


**Config paths**

 - <code>force_gc</code>


**Env overrides**

 - <code>EMQX_FORCE_GC</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable forced garbage collection.

- count: <code>0..inf</code>
  * default: 
  `16000`

  GC the process after this many received messages.

- bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `"16MB"`

  GC the process after specified number of bytes have passed through.


## broker:force_shutdown
When the process message queue length, or the memory bytes
reaches a certain value, the process is forced to close.

Note: "message queue" here refers to the "message mailbox"
of the Erlang process, not the `mqueue` of QoS 1 and QoS 2.


**Config paths**

 - <code>force_shutdown</code>


**Env overrides**

 - <code>EMQX_FORCE_SHUTDOWN</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable `force_shutdown` feature.

- max_message_queue_len: <code>0..inf</code>
  * default: 
  `1000`

  Maximum message queue length.

- max_heap_size: <code>emqx_schema:wordsize()</code>
  * default: 
  `"32MB"`

  Total heap size


## broker:listener_ssl_opts
Socket options for SSL connections.


**Config paths**

 - <code>gateway.exproto.listeners.ssl.$name.ssl_options</code>
 - <code>gateway.stomp.listeners.ssl.$name.ssl_options</code>
 - <code>listeners.ssl.$name.ssl_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__SSL_OPTIONS</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__SSL_OPTIONS</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>


  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.


- certfile: <code>binary()</code>


  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.


- keyfile: <code>binary()</code>

  PEM format private key file. 

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification. 

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse. 

- depth: <code>integer()</code>
  * default: 
  `10`


  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.<br/>


- password: <code>string()</code>


  String containing the user's password.
  Only used if the private key file is password-protected.


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  All TLS/DTLS versions to be supported.<br/>
  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
  In case PSK cipher suites are intended, make sure to configure
  <code>['tlsv1.2', 'tlsv1.1']</code> here.


- ciphers: <code>[string()]</code>
  * default: 
  `[]`


  This config holds TLS cipher suite names separated by comma,
  or as an array of strings. e.g.
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
  <br/>
  Ciphers (and their ordering) define the way in which the
  client and server encrypts information over the network connection.
  Selecting a good cipher suite is critical for the
  application's data security, confidentiality and performance.

  The names should be in OpenSSL string format (not RFC format).
  All default values and examples provided by EMQX config
  documentation are all in OpenSSL format.<br/>

  NOTE: Certain cipher suites are only compatible with
  specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
  incompatible cipher suites will be silently dropped.
  For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
  configuring cipher suites for other versions will have no effect.
  <br/>

  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
  If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
  PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`

  EMQX-internal callback that is used to lookup pre-shared key (PSK) identity. 

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.


- dhfile: <code>string()</code>


  Path to a file containing PEM-encoded Diffie-Hellman parameters
  to be used by the server if a cipher suite using Diffie-Hellman
  key exchange is negotiated. If not specified, default parameters
  are used.<br/>
  NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


- fail_if_no_peer_cert: <code>boolean()</code>
  * default: 
  `false`


  Used together with {verify, verify_peer} by an TLS/DTLS server.
  If set to true, the server fails if the client does not have a
  certificate to send, that is, sends an empty certificate.
  If set to false, it fails only if the client sends an invalid
  certificate (an empty certificate is considered valid).


- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`


  An important security setting, it forces the cipher to be set based
   on the server-specified order instead of the client-specified order,
   hence enforcing the (usually more properly configured) security
   ordering of the server administrator.


- client_renegotiation: <code>boolean()</code>
  * default: 
  `true`


  In protocols that support client-initiated renegotiation,
  the cost of resources of such an operation is higher for the server than the client.
  This can act as a vector for denial of service attacks.
  The SSL application already takes measures to counter-act such attempts,
  but client-initiated renegotiation can be strictly disabled by setting this option to false.
  The default value is true. Note that disabling renegotiation can result in
  long-lived connections becoming unusable due to limits on
  the number of messages the underlying cipher suite can encipher.


- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`


  Maximum time duration allowed for the handshake to complete


- gc_after_handshake: <code>boolean()</code>
  * default: 
  `false`


  Memory usage tuning. If enabled, will immediately perform a garbage collection after
  the TLS/SSL handshake.



## broker:listener_wss_opts
Socket options for WebSocket/SSL connections.


**Config paths**

 - <code>listeners.wss.$name.ssl_options</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WSS__$NAME__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>


  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.


- certfile: <code>binary()</code>


  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.


- keyfile: <code>binary()</code>

  PEM format private key file. 

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification. 

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse. 

- depth: <code>integer()</code>
  * default: 
  `10`


  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.<br/>


- password: <code>string()</code>


  String containing the user's password.
  Only used if the private key file is password-protected.


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  All TLS/DTLS versions to be supported.<br/>
  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
  In case PSK cipher suites are intended, make sure to configure
  <code>['tlsv1.2', 'tlsv1.1']</code> here.


- ciphers: <code>[string()]</code>
  * default: 
  `[]`


  This config holds TLS cipher suite names separated by comma,
  or as an array of strings. e.g.
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
  <br/>
  Ciphers (and their ordering) define the way in which the
  client and server encrypts information over the network connection.
  Selecting a good cipher suite is critical for the
  application's data security, confidentiality and performance.

  The names should be in OpenSSL string format (not RFC format).
  All default values and examples provided by EMQX config
  documentation are all in OpenSSL format.<br/>

  NOTE: Certain cipher suites are only compatible with
  specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
  incompatible cipher suites will be silently dropped.
  For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
  configuring cipher suites for other versions will have no effect.
  <br/>

  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
  If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
  PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`

  EMQX-internal callback that is used to lookup pre-shared key (PSK) identity. 

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.


- dhfile: <code>string()</code>


  Path to a file containing PEM-encoded Diffie-Hellman parameters
  to be used by the server if a cipher suite using Diffie-Hellman
  key exchange is negotiated. If not specified, default parameters
  are used.<br/>
  NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


- fail_if_no_peer_cert: <code>boolean()</code>
  * default: 
  `false`


  Used together with {verify, verify_peer} by an TLS/DTLS server.
  If set to true, the server fails if the client does not have a
  certificate to send, that is, sends an empty certificate.
  If set to false, it fails only if the client sends an invalid
  certificate (an empty certificate is considered valid).


- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`


  An important security setting, it forces the cipher to be set based
   on the server-specified order instead of the client-specified order,
   hence enforcing the (usually more properly configured) security
   ordering of the server administrator.


- client_renegotiation: <code>boolean()</code>
  * default: 
  `true`


  In protocols that support client-initiated renegotiation,
  the cost of resources of such an operation is higher for the server than the client.
  This can act as a vector for denial of service attacks.
  The SSL application already takes measures to counter-act such attempts,
  but client-initiated renegotiation can be strictly disabled by setting this option to false.
  The default value is true. Note that disabling renegotiation can result in
  long-lived connections becoming unusable due to limits on
  the number of messages the underlying cipher suite can encipher.


- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`


  Maximum time duration allowed for the handshake to complete



## broker:listeners
MQTT listeners identified by their protocol type and assigned names


**Config paths**

 - <code>listeners</code>


**Env overrides**

 - <code>EMQX_LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [broker:mqtt_tcp_listener](#broker-mqtt_tcp_listener)}</code>

  TCP listeners.

- ssl: <code>{$name -> [broker:mqtt_ssl_listener](#broker-mqtt_ssl_listener)}</code>

  SSL listeners.

- ws: <code>{$name -> [broker:mqtt_ws_listener](#broker-mqtt_ws_listener)}</code>

  HTTP websocket listeners.

- wss: <code>{$name -> [broker:mqtt_wss_listener](#broker-mqtt_wss_listener)}</code>

  HTTPS websocket listeners.

- quic: <code>{$name -> [broker:mqtt_quic_listener](#broker-mqtt_quic_listener)}</code>

  QUIC listeners.


## broker:mqtt
Global MQTT configuration.<br/>The configs here work as default values which can be overridden
in <code>zone</code> configs


**Config paths**

 - <code>mqtt</code>


**Env overrides**

 - <code>EMQX_MQTT</code>



**Fields**

- idle_timeout: <code>infinity | emqx_schema:duration()</code>
  * default: 
  `"15s"`

  After the TCP connection is established, if the MQTT CONNECT packet from the client is not received within the time specified by <code>idle_timeout</code>, the connection will be disconnected.

- max_packet_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`

  Maximum MQTT packet size allowed.

- max_clientid_len: <code>23..65535</code>
  * default: 
  `65535`

  Maximum allowed length of MQTT Client ID.

- max_topic_levels: <code>1..65535</code>
  * default: 
  `128`

  Maximum topic levels allowed.

- max_qos_allowed: <code>qos()</code>
  * default: 
  `2`

  Maximum QoS allowed.

- max_topic_alias: <code>0..65535</code>
  * default: 
  `65535`

  Maximum topic alias, 0 means no topic alias supported.

- retain_available: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable support for MQTT retained message.

- wildcard_subscription: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable support for MQTT wildcard subscription.

- shared_subscription: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable support for MQTT shared subscription.

- exclusive_subscription: <code>boolean()</code>
  * default: 
  `false`

  Whether to enable support for MQTT exclusive subscription.

- ignore_loop_deliver: <code>boolean()</code>
  * default: 
  `false`

  Ignore loop delivery of messages for MQTT v3.1.1/v3.1.0, similar to <code>No Local</code> subscription option in MQTT 5.0.

- strict_mode: <code>boolean()</code>
  * default: 
  `false`

  Parse MQTT messages in strict mode.
  When set to true, invalid utf8 strings in for example client ID, topic name, etc. will cause the client to be disconnected

- response_information: <code>string()</code>
  * default: 
  `""`

  Specify the response information returned to the client. This feature is disabled if is set to "". Applies only to clients using MQTT 5.0.

- server_keepalive: <code>integer() | disabled</code>
  * default: 
  `disabled`

  The keep alive that EMQX requires the client to use. If configured as <code>disabled</code>, it means that the keep alive specified by the client will be used. Requires <code>Server Keep Alive</code> in MQTT 5.0, so it is only applicable to clients using MQTT 5.0 protocol.

- keepalive_backoff: <code>number()</code>
  * default: 
  `0.75`

  The backoff multiplier used by the broker to determine the client keep alive timeout. If EMQX doesn't receive any packet in <code>Keep Alive * Backoff * 2</code> seconds, EMQX will close the current connection.

- max_subscriptions: <code>1..inf | infinity</code>
  * default: 
  `infinity`

  Maximum number of subscriptions allowed per client.

- upgrade_qos: <code>boolean()</code>
  * default: 
  `false`

  Force upgrade of QoS level according to subscription.

- max_inflight: <code>1..65535</code>
  * default: 
  `32`

  Maximum number of QoS 1 and QoS 2 messages that are allowed to be delivered simultaneously before completing the acknowledgment.

- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"30s"`

  Retry interval for QoS 1/2 message delivering.

- max_awaiting_rel: <code>integer() | infinity</code>
  * default: 
  `100`

  For each publisher session, the maximum number of outstanding QoS 2 messages pending on the client to send PUBREL. After reaching this limit, new QoS 2 PUBLISH requests will be rejected with `147(0x93)` until either PUBREL is received or timed out.

- await_rel_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"300s"`

  For client to broker QoS 2 message, the time limit for the broker to wait before the `PUBREL` message is received. The wait is aborted after timed out, meaning the packet ID is freed for new `PUBLISH` requests. Receiving a stale `PUBREL` causes a warning level log. Note, the message is delivered to subscribers before entering the wait for PUBREL.

- session_expiry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"2h"`

  Specifies how long the session will expire after the connection is disconnected, only for non-MQTT 5.0 connections.

- max_mqueue_len: <code>non_neg_integer() | infinity</code>
  * default: 
  `1000`

  Maximum queue length. Enqueued messages when persistent client disconnected, or inflight window is full.

- mqueue_priorities: <code>map() | disabled</code>
  * default: 
  `disabled`

  Topic priorities. Priority number [1-255]
  There's no priority table by default, hence all messages are treated equal.

  **NOTE**: Comma and equal signs are not allowed for priority topic names.
  **NOTE**: Messages for topics not in the priority table are treated as either highest or lowest priority depending on the configured value for <code>mqtt.mqueue_default_priority</code>.

  **Examples**:
  To configure <code>"topic/1" > "topic/2"</code>:
  <code>mqueue_priorities: {"topic/1": 10, "topic/2": 8}</code>


- mqueue_default_priority: <code>highest | lowest</code>
  * default: 
  `lowest`

  Default topic priority, which will be used by topics not in <code>Topic Priorities</code> (<code>mqueue_priorities</code>).

- mqueue_store_qos0: <code>boolean()</code>
  * default: 
  `true`

  Specifies whether to store QoS 0 messages in the message queue while the connection is down but the session remains.

- use_username_as_clientid: <code>boolean()</code>
  * default: 
  `false`

  Whether to user Client ID as Username.
  This setting takes effect later than <code>Use Peer Certificate as Username</code> (<code>peer_cert_as_username</code>) and <code>Use peer certificate as Client ID</code> (<code>peer_cert_as_clientid</code>).


- peer_cert_as_username: <code>disabled | cn | dn | crt | pem | md5</code>
  * default: 
  `disabled`

  Use the CN, DN field in the peer certificate or the entire certificate content as Username. Only works for the TLS connection.
  Supported configurations are the following:
  - <code>cn</code>: Take the CN field of the certificate as Username
  - <code>dn</code>: Take the DN field of the certificate as Username
  - <code>crt</code>: Take the content of the <code>DER</code> or <code>PEM</code> certificate as Username
  - <code>pem</code>: Convert <code>DER</code> certificate content to <code>PEM</code> format as Username
  - <code>md5</code>: Take the MD5 value of the content of the <code>DER</code> or <code>PEM</code> certificate as Username


- peer_cert_as_clientid: <code>disabled | cn | dn | crt | pem | md5</code>
  * default: 
  `disabled`

  Use the CN, DN field in the peer certificate or the entire certificate content as Client ID. Only works for the TLS connection.
  Supported configurations are the following:
  - <code>cn</code>: Take the CN field of the certificate as Client ID
  - <code>dn</code>: Take the DN field of the certificate as Client ID
  - <code>crt</code>: Take the content of the <code>DER</code> or <code>PEM</code> certificate as Client ID
  - <code>pem</code>: Convert <code>DER</code> certificate content to <code>PEM</code> format as Client ID
  - <code>md5</code>: Take the MD5 value of the content of the <code>DER</code> or <code>PEM</code> certificate as Client ID



## broker:mqtt_quic_listener
Settings for the MQTT over QUIC listener.


**Config paths**

 - <code>listeners.quic.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__QUIC__$NAME</code>



**Fields**

- certfile: <code>string()</code>

  Path to the certificate file.

- keyfile: <code>string()</code>

  Path to the secret key file. 

- ciphers: <code>[string()]</code>
  * default: 
  `["TLS_AES_256_GCM_SHA384", "TLS_AES_128_GCM_SHA256", "TLS_CHACHA20_POLY1305_SHA256"]`


  This config holds TLS cipher suite names separated by comma,
  or as an array of strings. e.g.
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
  <br/>
  Ciphers (and their ordering) define the way in which the
  client and server encrypts information over the network connection.
  Selecting a good cipher suite is critical for the
  application's data security, confidentiality and performance.

  The names should be in OpenSSL string format (not RFC format).
  All default values and examples provided by EMQX config
  documentation are all in OpenSSL format.<br/>

  NOTE: Certain cipher suites are only compatible with
  specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
  incompatible cipher suites will be silently dropped.
  For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
  configuring cipher suites for other versions will have no effect.
  <br/>

  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
  If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
  PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>

  NOTE: QUIC listener supports only 'tlsv1.3' ciphers<br/>


- idle_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `0`

  How long a connection can go idle before it is gracefully shut down. 0 to disable

- handshake_idle_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"10s"`

  How long a handshake can idle before it is discarded. 

- keep_alive_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `0`


  How often to send PING frames to keep a connection alive. 0 means disabled.


- enabled: <code>boolean()</code>
  * default: 
  `true`

  Enable listener. 

- bind: <code>emqx_schema:ip_port() | integer()</code>
  * default: 
  `14567`


  IP address and port for the listening socket.


- acceptors: <code>pos_integer()</code>
  * default: 
  `16`

  The size of the listener's receiving pool.

- max_connections: <code>infinity | pos_integer()</code>
  * default: 
  `infinity`

  The maximum number of concurrent connections allowed by the listener. 

- mountpoint: <code>binary()</code>
  * default: 
  `""`


  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message
  is delivered to the subscriber. The mountpoint is a way that users can use
  to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint`
  set to `some_tenant`, then the client actually subscribes to the topic
  `some_tenant/t`. Similarly, if another client B (connected to the same listener
  as the client A) sends a message to topic `t`, the message is routed
  to all the clients subscribed `some_tenant/t`, so client A will receive the
  message, with topic name `t`.<br/>
  Set to `""` to disable the feature.<br/>

  Variables in mountpoint string:
    - <code>${clientid}</code>: clientid
    - <code>${username}</code>: username


- zone: <code>atom()</code>
  * default: 
  `default`


  The configuration zone to which the listener belongs.


- limiter: <code>[limiter:listener_fields](#limiter-listener_fields)</code>
  * default: 

  ```
  {
    connection {capacity = 1000, rate = "1000/s"}
  }
  ```


  Type of the rate limit.


- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`


  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
  process goes through the configured authentication chain.
  When set to <code>false</code> to allow any clients with or without authentication information such as username or password to log in.
  When set to <code>quick_deny_anonymous<code>, it behaves like when set to <code>true</code> but clients will be
  denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
  anonymous clients early.



## broker:mqtt_ssl_listener
Settings for the MQTT over SSL listener.


**Config paths**

 - <code>listeners.ssl.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__SSL__$NAME</code>



**Fields**

- enabled: <code>boolean()</code>
  * default: 
  `true`

  Enable listener. 

- bind: <code>emqx_schema:ip_port() | integer()</code>
  * default: 
  `8883`


  IP address and port for the listening socket.


- acceptors: <code>pos_integer()</code>
  * default: 
  `16`

  The size of the listener's receiving pool.

- max_connections: <code>infinity | pos_integer()</code>
  * default: 
  `infinity`

  The maximum number of concurrent connections allowed by the listener. 

- mountpoint: <code>binary()</code>
  * default: 
  `""`


  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message
  is delivered to the subscriber. The mountpoint is a way that users can use
  to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint`
  set to `some_tenant`, then the client actually subscribes to the topic
  `some_tenant/t`. Similarly, if another client B (connected to the same listener
  as the client A) sends a message to topic `t`, the message is routed
  to all the clients subscribed `some_tenant/t`, so client A will receive the
  message, with topic name `t`.<br/>
  Set to `""` to disable the feature.<br/>

  Variables in mountpoint string:
    - <code>${clientid}</code>: clientid
    - <code>${username}</code>: username


- zone: <code>atom()</code>
  * default: 
  `default`


  The configuration zone to which the listener belongs.


- limiter: <code>[limiter:listener_fields](#limiter-listener_fields)</code>
  * default: 

  ```
  {
    connection {capacity = 1000, rate = "1000/s"}
  }
  ```


  Type of the rate limit.


- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`


  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
  process goes through the configured authentication chain.
  When set to <code>false</code> to allow any clients with or without authentication information such as username or password to log in.
  When set to <code>quick_deny_anonymous<code>, it behaves like when set to <code>true</code> but clients will be
  denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
  anonymous clients early.


- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`


  The access control rules for this listener.<br/>See: https://github.com/emqtt/esockd#allowdeny


- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`


  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br/>
  See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"3s"`


  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


- authentication: <code>[[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)]</code>


  Per-listener authentication override.
  Authentication can be one single authenticator instance or a chain of authenticators as an array.
  When authenticating a login (username, client ID, etc.) the authenticators are checked in the configured order.<br/>



- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- ssl_options: <code>[broker:listener_ssl_opts](#broker-listener_ssl_opts)</code>




## broker:mqtt_tcp_listener
Settings for the MQTT over TCP listener.


**Config paths**

 - <code>listeners.tcp.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__TCP__$NAME</code>



**Fields**

- enabled: <code>boolean()</code>
  * default: 
  `true`

  Enable listener. 

- bind: <code>emqx_schema:ip_port() | integer()</code>
  * default: 
  `1883`


  IP address and port for the listening socket.


- acceptors: <code>pos_integer()</code>
  * default: 
  `16`

  The size of the listener's receiving pool.

- max_connections: <code>infinity | pos_integer()</code>
  * default: 
  `infinity`

  The maximum number of concurrent connections allowed by the listener. 

- mountpoint: <code>binary()</code>
  * default: 
  `""`


  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message
  is delivered to the subscriber. The mountpoint is a way that users can use
  to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint`
  set to `some_tenant`, then the client actually subscribes to the topic
  `some_tenant/t`. Similarly, if another client B (connected to the same listener
  as the client A) sends a message to topic `t`, the message is routed
  to all the clients subscribed `some_tenant/t`, so client A will receive the
  message, with topic name `t`.<br/>
  Set to `""` to disable the feature.<br/>

  Variables in mountpoint string:
    - <code>${clientid}</code>: clientid
    - <code>${username}</code>: username


- zone: <code>atom()</code>
  * default: 
  `default`


  The configuration zone to which the listener belongs.


- limiter: <code>[limiter:listener_fields](#limiter-listener_fields)</code>
  * default: 

  ```
  {
    connection {capacity = 1000, rate = "1000/s"}
  }
  ```


  Type of the rate limit.


- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`


  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
  process goes through the configured authentication chain.
  When set to <code>false</code> to allow any clients with or without authentication information such as username or password to log in.
  When set to <code>quick_deny_anonymous<code>, it behaves like when set to <code>true</code> but clients will be
  denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
  anonymous clients early.


- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`


  The access control rules for this listener.<br/>See: https://github.com/emqtt/esockd#allowdeny


- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`


  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br/>
  See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"3s"`


  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


- authentication: <code>[[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)]</code>


  Per-listener authentication override.
  Authentication can be one single authenticator instance or a chain of authenticators as an array.
  When authenticating a login (username, client ID, etc.) the authenticators are checked in the configured order.<br/>



- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>




## broker:mqtt_ws_listener
Settings for the MQTT over WebSocket listener.


**Config paths**

 - <code>listeners.ws.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WS__$NAME</code>



**Fields**

- enabled: <code>boolean()</code>
  * default: 
  `true`

  Enable listener. 

- bind: <code>emqx_schema:ip_port() | integer()</code>
  * default: 
  `8083`


  IP address and port for the listening socket.


- acceptors: <code>pos_integer()</code>
  * default: 
  `16`

  The size of the listener's receiving pool.

- max_connections: <code>infinity | pos_integer()</code>
  * default: 
  `infinity`

  The maximum number of concurrent connections allowed by the listener. 

- mountpoint: <code>binary()</code>
  * default: 
  `""`


  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message
  is delivered to the subscriber. The mountpoint is a way that users can use
  to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint`
  set to `some_tenant`, then the client actually subscribes to the topic
  `some_tenant/t`. Similarly, if another client B (connected to the same listener
  as the client A) sends a message to topic `t`, the message is routed
  to all the clients subscribed `some_tenant/t`, so client A will receive the
  message, with topic name `t`.<br/>
  Set to `""` to disable the feature.<br/>

  Variables in mountpoint string:
    - <code>${clientid}</code>: clientid
    - <code>${username}</code>: username


- zone: <code>atom()</code>
  * default: 
  `default`


  The configuration zone to which the listener belongs.


- limiter: <code>[limiter:listener_fields](#limiter-listener_fields)</code>
  * default: 

  ```
  {
    connection {capacity = 1000, rate = "1000/s"}
  }
  ```


  Type of the rate limit.


- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`


  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
  process goes through the configured authentication chain.
  When set to <code>false</code> to allow any clients with or without authentication information such as username or password to log in.
  When set to <code>quick_deny_anonymous<code>, it behaves like when set to <code>true</code> but clients will be
  denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
  anonymous clients early.


- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`


  The access control rules for this listener.<br/>See: https://github.com/emqtt/esockd#allowdeny


- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`


  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br/>
  See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"3s"`


  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


- authentication: <code>[[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)]</code>


  Per-listener authentication override.
  Authentication can be one single authenticator instance or a chain of authenticators as an array.
  When authenticating a login (username, client ID, etc.) the authenticators are checked in the configured order.<br/>



- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- websocket: <code>[broker:ws_opts](#broker-ws_opts)</code>




## broker:mqtt_wss_listener
Settings for the MQTT over WebSocket/SSL listener.


**Config paths**

 - <code>listeners.wss.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WSS__$NAME</code>



**Fields**

- enabled: <code>boolean()</code>
  * default: 
  `true`

  Enable listener. 

- bind: <code>emqx_schema:ip_port() | integer()</code>
  * default: 
  `8084`


  IP address and port for the listening socket.


- acceptors: <code>pos_integer()</code>
  * default: 
  `16`

  The size of the listener's receiving pool.

- max_connections: <code>infinity | pos_integer()</code>
  * default: 
  `infinity`

  The maximum number of concurrent connections allowed by the listener. 

- mountpoint: <code>binary()</code>
  * default: 
  `""`


  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message
  is delivered to the subscriber. The mountpoint is a way that users can use
  to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint`
  set to `some_tenant`, then the client actually subscribes to the topic
  `some_tenant/t`. Similarly, if another client B (connected to the same listener
  as the client A) sends a message to topic `t`, the message is routed
  to all the clients subscribed `some_tenant/t`, so client A will receive the
  message, with topic name `t`.<br/>
  Set to `""` to disable the feature.<br/>

  Variables in mountpoint string:
    - <code>${clientid}</code>: clientid
    - <code>${username}</code>: username


- zone: <code>atom()</code>
  * default: 
  `default`


  The configuration zone to which the listener belongs.


- limiter: <code>[limiter:listener_fields](#limiter-listener_fields)</code>
  * default: 

  ```
  {
    connection {capacity = 1000, rate = "1000/s"}
  }
  ```


  Type of the rate limit.


- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`


  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
  process goes through the configured authentication chain.
  When set to <code>false</code> to allow any clients with or without authentication information such as username or password to log in.
  When set to <code>quick_deny_anonymous<code>, it behaves like when set to <code>true</code> but clients will be
  denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
  anonymous clients early.


- access_rules: <code>[string()]</code>
  * default: 
  `["allow all"]`


  The access control rules for this listener.<br/>See: https://github.com/emqtt/esockd#allowdeny


- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`


  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.<br/>
  See: https://www.haproxy.com/blog/haproxy/proxy-protocol/


- proxy_protocol_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"3s"`


  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.


- authentication: <code>[[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)]</code>


  Per-listener authentication override.
  Authentication can be one single authenticator instance or a chain of authenticators as an array.
  When authenticating a login (username, client ID, etc.) the authenticators are checked in the configured order.<br/>



- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- ssl_options: <code>[broker:listener_wss_opts](#broker-listener_wss_opts)</code>



- websocket: <code>[broker:ws_opts](#broker-ws_opts)</code>




## broker:overload_protection
Overload protection mechanism monitors the load of the system and temporarily
disables some features (such as accepting new connections) when the load is high.


**Config paths**

 - <code>overload_protection</code>


**Env overrides**

 - <code>EMQX_OVERLOAD_PROTECTION</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  React on system overload or not.

- backoff_delay: <code>0..inf</code>
  * default: 
  `1`

  When at high load, some unimportant tasks could be delayed for execution, here set the duration in milliseconds precision.

- backoff_gc: <code>boolean()</code>
  * default: 
  `false`

  When at high load, skip forceful GC.

- backoff_hibernation: <code>boolean()</code>
  * default: 
  `true`

  When at high load, skip process hibernation.

- backoff_new_conn: <code>boolean()</code>
  * default: 
  `true`

  When at high load, close new incoming connections.


## broker:persistent_session_builtin
Settings for the built-in storage engine of persistent messages.


**Config paths**

 - <code>persistent_session_store.backend</code>


**Env overrides**

 - <code>EMQX_PERSISTENT_SESSION_STORE__BACKEND</code>



**Fields**

- type: <code>builtin</code>
  * default: 
  `builtin`



- session: <code>[broker:persistent_table_mria_opts](#broker-persistent_table_mria_opts)</code>

  Performance tuning options for built-in session table.

- session_messages: <code>[broker:persistent_table_mria_opts](#broker-persistent_table_mria_opts)</code>

  Performance tuning options for built-in session messages table.

- messages: <code>[broker:persistent_table_mria_opts](#broker-persistent_table_mria_opts)</code>

  Performance tuning options for built-in messages table.


## broker:persistent_session_store
Settings for message persistence.


**Config paths**

 - <code>persistent_session_store</code>


**Env overrides**

 - <code>EMQX_PERSISTENT_SESSION_STORE</code>



**Fields**

- enabled: <code>boolean()</code>
  * default: 
  `false`

  Use the database to store information about persistent sessions.
  This makes it possible to migrate a client connection to another
  cluster node if a node is stopped.


- on_disc: <code>boolean()</code>
  * default: 
  `true`

  Save information about the persistent sessions on disc.
  If this option is enabled, persistent sessions will survive full restart of the cluster.
  Otherwise, all the data will be stored in RAM, and it will be lost when all the nodes in the cluster are stopped.

- ram_cache: <code>boolean()</code>
  * default: 
  `false`

  Maintain a copy of the data in RAM for faster access.

- backend: <code>[broker:persistent_session_builtin](#broker-persistent_session_builtin)</code>
  * default: 

  ```
  {
    messages {ram_cache = "false"}
    session {ram_cache = "true"}
    session_messages {ram_cache = "true"}
    type = "builtin"
  }
  ```

  Database management system used to store information about persistent sessions and messages.
  - `builtin`: Use the embedded database (mria)

- max_retain_undelivered: <code>emqx_schema:duration()</code>
  * default: 
  `"1h"`

  The time messages that was not delivered to a persistent session
  is stored before being garbage collected if the node the previous
  session was handled on restarts of is stopped.


- message_gc_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1h"`

  The starting interval for garbage collection of undelivered messages to
  a persistent session. This affects how often the "max_retain_undelivered"
  is checked for removal.


- session_message_gc_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  The starting interval for garbage collection of transient data for
  persistent session messages. This does not affect the lifetime length
  of persistent session messages.



## broker:persistent_table_mria_opts
Tuning options for the mria table.


**Config paths**

 - <code>persistent_session_store.backend.messages</code>
 - <code>persistent_session_store.backend.session</code>
 - <code>persistent_session_store.backend.session_messages</code>


**Env overrides**

 - <code>EMQX_PERSISTENT_SESSION_STORE__BACKEND__MESSAGES</code>
 - <code>EMQX_PERSISTENT_SESSION_STORE__BACKEND__SESSION</code>
 - <code>EMQX_PERSISTENT_SESSION_STORE__BACKEND__SESSION_MESSAGES</code>



**Fields**

- ram_cache: <code>boolean()</code>
  * default: 
  `true`

  Maintain a copy of the data in RAM for faster access.


## broker:shared_subscription_group
Per group dispatch strategy for shared subscription


**Config paths**

 - <code>broker.shared_subscription_group.$name</code>


**Env overrides**

 - <code>EMQX_BROKER__SHARED_SUBSCRIPTION_GROUP__$NAME</code>



**Fields**

- strategy: <code>random | round_robin | round_robin_per_group | sticky | local | hash_topic | hash_clientid</code>
  * default: 
  `random`

  Dispatch strategy for shared subscription.
  - `random`: dispatch the message to a random selected subscriber
  - `round_robin`: select the subscribers in a round-robin manner
  - `round_robin_per_group`: select the subscribers in round-robin fashion within each shared subscriber group
  - `sticky`: always use the last selected subscriber to dispatch,
  until the subscriber disconnects.
  - `hash`: select the subscribers by the hash of `clientIds`
  - `local`: send to a random local subscriber. If local
  subscriber was not found, send to a random subscriber cluster-wide



## broker:ssl_client_opts
Socket options for SSL clients.


**Config paths**

 - <code>authentication.$INDEX.ssl</code>
 - <code>authorization.sources.$INDEX.ssl</code>
 - <code>bridges.mqtt.$name.ssl</code>
 - <code>bridges.webhook.$name.ssl</code>
 - <code>cluster.etcd.ssl</code>
 - <code>gateway.coap.authentication.ssl</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.exproto.authentication.ssl</code>
 - <code>gateway.exproto.handler.ssl_options</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.ssl</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.lwm2m.authentication.ssl</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.mqttsn.authentication.ssl</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.ssl</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.ssl</code>
 - <code>gateway.stomp.authentication.ssl</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.ssl</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.ssl</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.ws.$name.authentication.$INDEX.ssl</code>
 - <code>listeners.wss.$name.authentication.$INDEX.ssl</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__SSL</code>
 - <code>EMQX_BRIDGES__MQTT__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__WEBHOOK__$NAME__SSL</code>
 - <code>EMQX_CLUSTER__ETCD__SSL</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__HANDLER__SSL_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__SSL</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__SSL</code>



**Fields**

- cacertfile: <code>binary()</code>


  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.


- certfile: <code>binary()</code>


  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.


- keyfile: <code>binary()</code>

  PEM format private key file. 

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification. 

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse. 

- depth: <code>integer()</code>
  * default: 
  `10`


  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.<br/>


- password: <code>string()</code>


  String containing the user's password.
  Only used if the private key file is password-protected.


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  All TLS/DTLS versions to be supported.<br/>
  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
  In case PSK cipher suites are intended, make sure to configure
  <code>['tlsv1.2', 'tlsv1.1']</code> here.


- ciphers: <code>[string()]</code>
  * default: 
  `[]`


  This config holds TLS cipher suite names separated by comma,
  or as an array of strings. e.g.
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
  <br/>
  Ciphers (and their ordering) define the way in which the
  client and server encrypts information over the network connection.
  Selecting a good cipher suite is critical for the
  application's data security, confidentiality and performance.

  The names should be in OpenSSL string format (not RFC format).
  All default values and examples provided by EMQX config
  documentation are all in OpenSSL format.<br/>

  NOTE: Certain cipher suites are only compatible with
  specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
  incompatible cipher suites will be silently dropped.
  For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
  configuring cipher suites for other versions will have no effect.
  <br/>

  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
  If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
  PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`

  EMQX-internal callback that is used to lookup pre-shared key (PSK) identity. 

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.


- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable TLS. 

- server_name_indication: <code>disable | string()</code>


  Specify the host name to be used in TLS Server Name Indication extension.<br/>
  For instance, when connecting to "server.example.net", the genuine server
  which accepts the connection and performs TLS handshake may differ from the
  host the TLS client initially connects to, e.g. when connecting to an IP address
  or when the host has multiple resolvable DNS records <br/>
  If not specified, it will default to the host name string which is used
  to establish the connection, unless it is IP addressed used.<br/>
  The host name is then also used in the host name verification of the peer
  certificate.<br/> The special value 'disable' prevents the Server Name
  Indication extension from being sent and disables the hostname
  verification check.



## broker:stats
Enable/disable statistic data collection.
Statistic data such as message receive/send count/rate etc. It provides insights of system performance and helps to diagnose issues. You can find statistic data from the dashboard, or from the '/stats' API.


**Config paths**

 - <code>stats</code>


**Env overrides**

 - <code>EMQX_STATS</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable/disable statistic data collection.


## broker:sys_topics
The EMQX Broker periodically publishes its own status, message statistics,
client online and offline events to the system topic starting with `$SYS/`.

The following options control the behavior of `$SYS` topics.


**Config paths**

 - <code>sys_topics</code>


**Env overrides**

 - <code>EMQX_SYS_TOPICS</code>



**Fields**

- sys_msg_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `"1m"`

  Time interval of publishing `$SYS` messages.

- sys_heartbeat_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `"30s"`

  Time interval for publishing following heartbeat messages:
    - `$SYS/brokers/<node>/uptime`
    - `$SYS/brokers/<node>/datetime`


- sys_event_messages: <code>[broker:event_names](#broker-event_names)</code>

  Client events messages.


## broker:sysmon
Features related to system monitoring and introspection.


**Config paths**

 - <code>sysmon</code>


**Env overrides**

 - <code>EMQX_SYSMON</code>



**Fields**

- vm: <code>[broker:sysmon_vm](#broker-sysmon_vm)</code>



- os: <code>[broker:sysmon_os](#broker-sysmon_os)</code>



- top: <code>[broker:sysmon_top](#broker-sysmon_top)</code>




## broker:sysmon_os
This part of the configuration is responsible for monitoring
 the host OS health, such as free memory, disk space, CPU load, etc.


**Config paths**

 - <code>sysmon.os</code>


**Env overrides**

 - <code>EMQX_SYSMON__OS</code>



**Fields**

- cpu_check_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"60s"`

  The time interval for the periodic CPU check.

- cpu_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"80%"`

  The threshold, as percentage of system CPU load,
   for how much system cpu can be used before the corresponding alarm is raised.

- cpu_low_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"60%"`

  The threshold, as percentage of system CPU load,
   for how much system cpu can be used before the corresponding alarm is cleared.

- mem_check_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `"60s"`

  The time interval for the periodic memory check.

- sysmem_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"70%"`

  The threshold, as percentage of system memory,
   for how much system memory can be allocated before the corresponding alarm is raised.

- procmem_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"5%"`

  The threshold, as percentage of system memory,
   for how much system memory can be allocated by one Erlang process before
   the corresponding alarm is raised.


## broker:sysmon_top
This part of the configuration is responsible for monitoring
 the Erlang processes in the VM. This information can be sent to an external
 PostgreSQL database. This feature is inactive unless the PostgreSQL sink is configured.


**Config paths**

 - <code>sysmon.top</code>


**Env overrides**

 - <code>EMQX_SYSMON__TOP</code>



**Fields**

- num_items: <code>non_neg_integer()</code>
  * default: 
  `10`
  * mapping: 
  `system_monitor.top_num_items`

  The number of top processes per monitoring group

- sample_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"2s"`
  * mapping: 
  `system_monitor.top_sample_interval`

  Specifies how often process top should be collected

- max_procs: <code>non_neg_integer()</code>
  * default: 
  `1000000`
  * mapping: 
  `system_monitor.top_max_procs`

  Stop collecting data when the number of processes
  in the VM exceeds this value

- db_hostname: <code>string()</code>
  * default: 
  `[]`
  * mapping: 
  `system_monitor.db_hostname`

  Hostname of the PostgreSQL database that collects the data points

- db_port: <code>integer()</code>
  * default: 
  `5432`
  * mapping: 
  `system_monitor.db_port`

  Port of the PostgreSQL database that collects the data points.

- db_username: <code>string()</code>
  * default: 
  `"system_monitor"`
  * mapping: 
  `system_monitor.db_username`

  Username of the PostgreSQL database

- db_password: <code>binary()</code>
  * default: 
  `"system_monitor_password"`
  * mapping: 
  `system_monitor.db_password`

  EMQX user password in the PostgreSQL database

- db_name: <code>string()</code>
  * default: 
  `"postgres"`
  * mapping: 
  `system_monitor.db_name`

  PostgreSQL database name


## broker:sysmon_vm
This part of the configuration is responsible for collecting
 BEAM VM events, such as long garbage collection, traffic congestion in the inter-broker
 communication, etc.


**Config paths**

 - <code>sysmon.vm</code>


**Env overrides**

 - <code>EMQX_SYSMON__VM</code>



**Fields**

- process_check_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"30s"`

  The time interval for the periodic process limit check.

- process_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"80%"`

  The threshold, as percentage of processes, for how many
   processes can simultaneously exist at the local node before the corresponding
   alarm is raised.

- process_low_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `"60%"`

  The threshold, as percentage of processes, for how many
   processes can simultaneously exist at the local node before the corresponding
   alarm is cleared.

- long_gc: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `disabled`

  Enable Long GC monitoring.

- long_schedule: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `"240ms"`

  Enable Long Schedule monitoring.

- large_heap: <code>disabled | emqx_schema:bytesize()</code>
  * default: 
  `"32MB"`

  Enable Large Heap monitoring.

- busy_dist_port: <code>boolean()</code>
  * default: 
  `true`

  Enable Busy Distribution Port monitoring.

- busy_port: <code>boolean()</code>
  * default: 
  `true`

  Enable Busy Port monitoring.


## broker:tcp_opts
TCP listener options.


**Config paths**

 - <code>gateway.exproto.listeners.ssl.$name.tcp_options</code>
 - <code>gateway.exproto.listeners.tcp.$name.tcp_options</code>
 - <code>gateway.stomp.listeners.ssl.$name.tcp_options</code>
 - <code>gateway.stomp.listeners.tcp.$name.tcp_options</code>
 - <code>listeners.ssl.$name.tcp_options</code>
 - <code>listeners.tcp.$name.tcp_options</code>
 - <code>listeners.ws.$name.tcp_options</code>
 - <code>listeners.wss.$name.tcp_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_LISTENERS__WS__$NAME__TCP_OPTIONS</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__TCP_OPTIONS</code>



**Fields**

- active_n: <code>integer()</code>
  * default: 
  `100`


  Specify the {active, N} option for this Socket.<br/>
  See: https://erlang.org/doc/man/inet.html#setopts-2


- backlog: <code>pos_integer()</code>
  * default: 
  `1024`


  TCP backlog defines the maximum length that the queue of
   pending connections can grow to.


- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`

  The TCP send timeout for the connections. 

- send_timeout_close: <code>boolean()</code>
  * default: 
  `true`


  Close the connection if send timeout.


- recbuf: <code>emqx_schema:bytesize()</code>


  The TCP receive buffer (OS kernel) for the connections.


- sndbuf: <code>emqx_schema:bytesize()</code>


  The TCP send buffer (OS kernel) for the connections.


- buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `"4KB"`


  The size of the user-space buffer used by the driver.


- high_watermark: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`


  The socket is set to a busy state when the amount of data queued internally
    by the VM socket implementation reaches this limit.


- nodelay: <code>boolean()</code>
  * default: 
  `true`


  The TCP_NODELAY flag for the connections.


- reuseaddr: <code>boolean()</code>
  * default: 
  `true`


  The SO_REUSEADDR flag for the connections.



## broker:trace
Real-time filtering logs for the ClientID or Topic or IP for debugging.


**Config paths**

 - <code>trace</code>


**Env overrides**

 - <code>EMQX_TRACE</code>



**Fields**

- payload_encode: <code>hex | text | hidden</code>
  * default: 
  `text`


  Determine the format of the payload format in the trace file.<br/>
  `text`: Text-based protocol or plain text protocol.
   It is recommended when payload is JSON encoded.<br/>
  `hex`: Binary hexadecimal encode. It is recommended when payload is a custom binary protocol.<br/>
  `hidden`: payload is obfuscated as `******`




## broker:ws_opts
WebSocket listener options.


**Config paths**

 - <code>listeners.ws.$name.websocket</code>
 - <code>listeners.wss.$name.websocket</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WS__$NAME__WEBSOCKET</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__WEBSOCKET</code>



**Fields**

- mqtt_path: <code>string()</code>
  * default: 
  `"/mqtt"`


  WebSocket's MQTT protocol path. So the address of EMQX Broker's WebSocket is:
  <code>ws://{ip}:{port}/mqtt</code>


- mqtt_piggyback: <code>single | multiple</code>
  * default: 
  `multiple`


  Whether a WebSocket message is allowed to contain multiple MQTT packets.


- compress: <code>boolean()</code>
  * default: 
  `false`


  If <code>true</code>, compress WebSocket messages using <code>zlib</code>.<br/>
  The configuration items under <code>deflate_opts</code> belong to the compression-related parameter configuration.


- idle_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"7200s"`


  Close transport-layer connections from the clients that have not sent MQTT CONNECT
  message within this interval.


- max_frame_size: <code>infinity | integer()</code>
  * default: 
  `infinity`


  The maximum length of a single MQTT packet.


- fail_if_no_subprotocol: <code>boolean()</code>
  * default: 
  `true`


  If <code>true</code>, the server will return an error when
   the client does not carry the <code>Sec-WebSocket-Protocol</code> field.
   <br/>Note: WeChat applet needs to disable this verification.


- supported_subprotocols: <code>emqx_schema:comma_separated_list()</code>
  * default: 
  `"mqtt, mqtt-v3, mqtt-v3.1.1, mqtt-v5"`


  Comma-separated list of supported subprotocols.


- check_origin_enable: <code>boolean()</code>
  * default: 
  `false`


  If <code>true</code>, <code>origin</code> HTTP header will be
   validated against the list of allowed origins configured in <code>check_origins</code>
   parameter.


- allow_origin_absence: <code>boolean()</code>
  * default: 
  `true`


  If <code>false</code> and <code>check_origin_enable</code> is
   <code>true</code>, the server will reject requests that don't have <code>origin</code>
   HTTP header.


- check_origins: <code>emqx_schema:comma_separated_binary()</code>
  * default: 
  `"http://localhost:18083, http://127.0.0.1:18083"`


  List of allowed origins.<br/>See <code>check_origin_enable</code>.


- proxy_address_header: <code>string()</code>
  * default: 
  `"x-forwarded-for"`


  HTTP header used to pass information about the client IP address.
  Relevant when the EMQX cluster is deployed behind a load-balancer.


- proxy_port_header: <code>string()</code>
  * default: 
  `"x-forwarded-port"`


  HTTP header used to pass information about the client port.
  Relevant when the EMQX cluster is deployed behind a load-balancer.


- deflate_opts: <code>[broker:deflate_opts](#broker-deflate_opts)</code>




## broker:zone
A `Zone` defines a set of configuration items (such as the maximum number of connections) that can be shared between multiple listeners.

`Listener` can refer to a `Zone` through the configuration item <code>listener.\<Protocol>.\<Listener Name>.zone</code>.

The configs defined in the zones will override the global configs with the same key.

For example, given the following config:
```
a {
    b: 1, c: 1
}
zone.my_zone {
  a {
    b:2
  }
}
```

The global config `a` is overridden by the configs `a` inside the zone `my_zone`.

If there is a listener using the zone `my_zone`, the value of config `a` will be: `{b:2, c: 1}`.
Note that although the default value of `a.c` is `0`, the global value is used, i.e. configs in the zone have no default values. To override `a.c` one must configure it explicitly in the zone.

All the global configs that can be overridden in zones are:
 - `stats.*`
 - `mqtt.*`
 - `authorization.*`
 - `flapping_detect.*`
 - `force_shutdown.*`
 - `conn_congestion.*`
 - `force_gc.*`




**Config paths**

 - <code>zones.$name</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME</code>



**Fields**

- mqtt: <code>[zone:mqtt](#zone-mqtt)</code>



- stats: <code>[zone:stats](#zone-stats)</code>



- flapping_detect: <code>[zone:flapping_detect](#zone-flapping_detect)</code>



- force_shutdown: <code>[zone:force_shutdown](#zone-force_shutdown)</code>



- conn_congestion: <code>[zone:conn_congestion](#zone-conn_congestion)</code>



- force_gc: <code>[zone:force_gc](#zone-force_gc)</code>



- overload_protection: <code>[zone:overload_protection](#zone-overload_protection)</code>




## dashboard
Configuration for EMQX dashboard.


**Config paths**

 - <code>dashboard</code>


**Env overrides**

 - <code>EMQX_DASHBOARD</code>



**Fields**

- listeners: <code>[dashboard:listeners](#dashboard-listeners)</code>

  HTTP(s) listeners are identified by their protocol type and are
  used to serve dashboard UI and restful HTTP API.
  Listeners must have a unique combination of port number and IP address.
  For example, an HTTP listener can listen on all configured IP addresses
  on a given port for a machine by specifying the IP address 0.0.0.0.
  Alternatively, the HTTP listener can specify a unique IP address for each listener,
  but use the same port.

- default_username: <code>binary()</code>
  * default: 
  `"admin"`

  The default username of the automatically created dashboard user.

- default_password: <code>binary()</code>
  * default: 
  `"public"`

  The initial default password for dashboard 'admin' user.
  For safety, it should be changed as soon as possible.

- sample_interval: <code>emqx_schema:duration_s()</code>
  * default: 
  `"10s"`

  How often to update metrics displayed in the dashboard.
  Note: `sample_interval` should be a divisor of 60.

- token_expired_time: <code>emqx_schema:duration()</code>
  * default: 
  `"60m"`

  JWT token expiration time.

- cors: <code>boolean()</code>
  * default: 
  `false`

  Support Cross-Origin Resource Sharing (CORS).
  Allows a server to indicate any origins (domain, scheme, or port) other than
  its own from which a browser should permit loading resources.

- i18n_lang: <code>en | zh</code>
  * default: 
  `en`

  Internationalization language support.

- bootstrap_users_file: <code>binary()</code>

  Initialize users file.


## dashboard:http
Configuration for the dashboard listener (plaintext).


**Config paths**

 - <code>dashboard.listeners.http</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS__HTTP</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Ignore or enable this listener

- bind: <code>non_neg_integer() | emqx_schema:ip_port()</code>
  * default: 
  `18083`

  Port without IP(18083) or port with specified IP(127.0.0.1:18083).

- num_acceptors: <code>integer()</code>
  * default: 
  `4`

  Socket acceptor pool size for TCP protocols.

- max_connections: <code>integer()</code>
  * default: 
  `512`

  Maximum number of simultaneous connections.

- backlog: <code>integer()</code>
  * default: 
  `1024`

  Defines the maximum length that the queue of pending connections can grow to.

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`

  Send timeout for the socket.

- inet6: <code>boolean()</code>
  * default: 
  `false`

  Enable IPv6 support, default is false, which means IPv4 only.

- ipv6_v6only: <code>boolean()</code>
  * default: 
  `false`

  Disable IPv4-to-IPv6 mapping for the listener.


## dashboard:https
Configuration for the dashboard listener (TLS).


**Config paths**

 - <code>dashboard.listeners.https</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS__HTTPS</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  Ignore or enable this listener

- bind: <code>non_neg_integer() | emqx_schema:ip_port()</code>
  * default: 
  `18084`

  Port without IP(18083) or port with specified IP(127.0.0.1:18083).

- num_acceptors: <code>integer()</code>
  * default: 
  `4`

  Socket acceptor pool size for TCP protocols.

- max_connections: <code>integer()</code>
  * default: 
  `512`

  Maximum number of simultaneous connections.

- backlog: <code>integer()</code>
  * default: 
  `1024`

  Defines the maximum length that the queue of pending connections can grow to.

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`

  Send timeout for the socket.

- inet6: <code>boolean()</code>
  * default: 
  `false`

  Enable IPv6 support, default is false, which means IPv4 only.

- ipv6_v6only: <code>boolean()</code>
  * default: 
  `false`

  Disable IPv4-to-IPv6 mapping for the listener.

- cacertfile: <code>binary()</code>


  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.


- certfile: <code>binary()</code>


  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.


- keyfile: <code>binary()</code>

  PEM format private key file. 

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification. 

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse. 

- depth: <code>integer()</code>
  * default: 
  `10`


  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.<br/>


- password: <code>string()</code>


  String containing the user's password.
  Only used if the private key file is password-protected.


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  All TLS/DTLS versions to be supported.<br/>
  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
  In case PSK cipher suites are intended, make sure to configure
  <code>['tlsv1.2', 'tlsv1.1']</code> here.


- ciphers: <code>[string()]</code>
  * default: 
  `[]`


  This config holds TLS cipher suite names separated by comma,
  or as an array of strings. e.g.
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
  <br/>
  Ciphers (and their ordering) define the way in which the
  client and server encrypts information over the network connection.
  Selecting a good cipher suite is critical for the
  application's data security, confidentiality and performance.

  The names should be in OpenSSL string format (not RFC format).
  All default values and examples provided by EMQX config
  documentation are all in OpenSSL format.<br/>

  NOTE: Certain cipher suites are only compatible with
  specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
  incompatible cipher suites will be silently dropped.
  For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
  configuring cipher suites for other versions will have no effect.
  <br/>

  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
  If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
  PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`

  EMQX-internal callback that is used to lookup pre-shared key (PSK) identity. 

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.


- dhfile: <code>string()</code>


  Path to a file containing PEM-encoded Diffie-Hellman parameters
  to be used by the server if a cipher suite using Diffie-Hellman
  key exchange is negotiated. If not specified, default parameters
  are used.<br/>
  NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`


  An important security setting, it forces the cipher to be set based
   on the server-specified order instead of the client-specified order,
   hence enforcing the (usually more properly configured) security
   ordering of the server administrator.


- client_renegotiation: <code>boolean()</code>
  * default: 
  `true`


  In protocols that support client-initiated renegotiation,
  the cost of resources of such an operation is higher for the server than the client.
  This can act as a vector for denial of service attacks.
  The SSL application already takes measures to counter-act such attempts,
  but client-initiated renegotiation can be strictly disabled by setting this option to false.
  The default value is true. Note that disabling renegotiation can result in
  long-lived connections becoming unusable due to limits on
  the number of messages the underlying cipher suite can encipher.


- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`


  Maximum time duration allowed for the handshake to complete



## dashboard:listeners
Configuration for the dashboard listener.


**Config paths**

 - <code>dashboard.listeners</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS</code>



**Fields**

- http: <code>[dashboard:http](#dashboard-http)</code>

  TCP listeners

- https: <code>[dashboard:https](#dashboard-https)</code>

  SSL listeners


## exhook
External hook (exhook) configuration.


**Config paths**

 - <code>exhook</code>


**Env overrides**

 - <code>EMQX_EXHOOK</code>



**Fields**

- servers: <code>[[exhook:server](#exhook-server)]</code>
  * default: 
  `[]`

  List of exhook servers


## exhook:server
gRPC server configuration.


**Config paths**

 - <code>exhook.servers.$INDEX</code>


**Env overrides**

 - <code>EMQX_EXHOOK__SERVERS__$INDEX</code>



**Fields**

- name: <code>binary()</code>

  Name of the exhook server

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable this Exhook server

- url: <code>binary()</code>

  URL of the gRPC server

- request_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`

  The timeout of request gRPC server

- failed_action: <code>deny | ignore</code>
  * default: 
  `deny`

  The value that is returned when the request to the gRPC server fails for any reason

- ssl: <code>[exhook:ssl_conf](#exhook-ssl_conf)</code>



- socket_options: <code>[exhook:socket_options](#exhook-socket_options)</code>
  * default: 
  `{keepalive = true, nodelay = true}`



- auto_reconnect: <code>false | emqx_schema:duration()</code>
  * default: 
  `"60s"`

  Whether to automatically reconnect (initialize) the gRPC server.
  When gRPC is not available, Exhook tries to request the gRPC service at that interval and reinitialize the list of mounted hooks.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  The process pool size for gRPC client


## exhook:socket_options
Connection socket options


**Config paths**

 - <code>exhook.servers.$INDEX.socket_options</code>


**Env overrides**

 - <code>EMQX_EXHOOK__SERVERS__$INDEX__SOCKET_OPTIONS</code>



**Fields**

- keepalive: <code>boolean()</code>
  * default: 
  `true`

  Enables/disables periodic transmission on a connected socket when no other data is exchanged.
  If the other end does not respond, the connection is considered broken and an error message is sent to the controlling process.

- nodelay: <code>boolean()</code>
  * default: 
  `true`

  If true, option TCP_NODELAY is turned on for the socket,
  which means that also small amounts of data are sent immediately

- recbuf: <code>emqx_schema:bytesize()</code>

  The minimum size of receive buffer to use for the socket

- sndbuf: <code>emqx_schema:bytesize()</code>

  The minimum size of send buffer to use for the socket


## exhook:ssl_conf
SSL client configuration.


**Config paths**

 - <code>exhook.servers.$INDEX.ssl</code>


**Env overrides**

 - <code>EMQX_EXHOOK__SERVERS__$INDEX__SSL</code>



**Fields**

- cacertfile: <code>binary()</code>


  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.


- certfile: <code>binary()</code>


  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.


- keyfile: <code>binary()</code>

  PEM format private key file. 

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification. 

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse. 

- depth: <code>integer()</code>
  * default: 
  `10`


  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.<br/>


- password: <code>string()</code>


  String containing the user's password.
  Only used if the private key file is password-protected.


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  All TLS/DTLS versions to be supported.<br/>
  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
  In case PSK cipher suites are intended, make sure to configure
  <code>['tlsv1.2', 'tlsv1.1']</code> here.


- ciphers: <code>[string()]</code>
  * default: 
  `[]`


  This config holds TLS cipher suite names separated by comma,
  or as an array of strings. e.g.
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
  <br/>
  Ciphers (and their ordering) define the way in which the
  client and server encrypts information over the network connection.
  Selecting a good cipher suite is critical for the
  application's data security, confidentiality and performance.

  The names should be in OpenSSL string format (not RFC format).
  All default values and examples provided by EMQX config
  documentation are all in OpenSSL format.<br/>

  NOTE: Certain cipher suites are only compatible with
  specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
  incompatible cipher suites will be silently dropped.
  For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
  configuring cipher suites for other versions will have no effect.
  <br/>

  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
  If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
  PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>


- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.


- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable TLS. 

- server_name_indication: <code>disable | string()</code>


  Specify the host name to be used in TLS Server Name Indication extension.<br/>
  For instance, when connecting to "server.example.net", the genuine server
  which accepts the connection and performs TLS handshake may differ from the
  host the TLS client initially connects to, e.g. when connecting to an IP address
  or when the host has multiple resolvable DNS records <br/>
  If not specified, it will default to the host name string which is used
  to establish the connection, unless it is IP addressed used.<br/>
  The host name is then also used in the host name verification of the peer
  certificate.<br/> The special value 'disable' prevents the Server Name
  Indication extension from being sent and disables the hostname
  verification check.



## gateway:clientinfo_override
ClientInfo override.


**Config paths**

 - <code>gateway.coap.clientinfo_override</code>
 - <code>gateway.exproto.clientinfo_override</code>
 - <code>gateway.lwm2m.clientinfo_override</code>
 - <code>gateway.mqttsn.clientinfo_override</code>
 - <code>gateway.stomp.clientinfo_override</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__EXPROTO__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__LWM2M__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__MQTTSN__CLIENTINFO_OVERRIDE</code>
 - <code>EMQX_GATEWAY__STOMP__CLIENTINFO_OVERRIDE</code>



**Fields**

- username: <code>binary()</code>

  Template for overriding username.

- password: <code>binary()</code>

  Template for overriding password.

- clientid: <code>binary()</code>

  Template for overriding clientid.


## gateway:coap
The CoAP protocol gateway provides EMQX with the access capability of the CoAP protocol.
It allows publishing, subscribing, and receiving messages to EMQX in accordance
with a certain defined CoAP message format.


**Config paths**

 - <code>gateway.coap</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP</code>



**Fields**

- heartbeat: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  The gateway server required minimum heartbeat interval.
  When connection mode is enabled, this parameter is used to set the minimum heartbeat interval for the connection to be alive

- connection_required: <code>boolean()</code>
  * default: 
  `false`

  Enable or disable connection mode.
  Connection mode is a feature of non-standard protocols. When connection mode is enabled, it is necessary to maintain the creation, authentication and alive of connection resources

- notify_type: <code>non | con | qos</code>
  * default: 
  `qos`

  The Notification Message will be delivered to the CoAP client if a new message received on an observed topic.
  The type of delivered coap message can be set to:
    - non: Non-confirmable;
    - con: Confirmable;
    - qos: Mapping from QoS type of received message, QoS0 -> non, QoS1,2 -> con


- subscribe_qos: <code>qos0 | qos1 | qos2 | coap</code>
  * default: 
  `coap`

  The Default QoS Level indicator for subscribe request.
  This option specifies the QoS level for the CoAP Client when establishing a subscription membership, if the subscribe request is not carried `qos` option. The indicator can be set to:
    - qos0, qos1, qos2: Fixed default QoS level
    - coap: Dynamic QoS level by the message type of subscribe request
      * qos0: If the subscribe request is non-confirmable
      * qos1: If the subscribe request is confirmable


- publish_qos: <code>qos0 | qos1 | qos2 | coap</code>
  * default: 
  `coap`

  The Default QoS Level indicator for publish request.
  This option specifies the QoS level for the CoAP Client when publishing a message to EMQX PUB/SUB system, if the publish request is not carried `qos` option. The indicator can be set to:
    - qos0, qos1, qos2: Fixed default QoS level
    - coap: Dynamic QoS level by the message type of publish request
      * qos0: If the publish request is non-confirmable
      * qos1: If the publish request is confirmable

- mountpoint: <code>binary()</code>
  * default: 
  `""`

   

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>

  Settings for the UDP listeners.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable this gateway

- enable_stats: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable client process statistic

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  The idle time of the client connection process. It has two purposes:
    1. A newly created client process that does not receive any client requests after that time will be closed directly.
    2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo override.

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  Default authentication configs for all the gateway listeners. For per-listener overrides see <code>authentication</code>
   in listener configs


## gateway:dtls_listener
Settings for the DTLS listener.


**Config paths**

 - <code>gateway.coap.listeners.dtls.$name</code>
 - <code>gateway.exproto.listeners.dtls.$name</code>
 - <code>gateway.lwm2m.listeners.dtls.$name</code>
 - <code>gateway.mqttsn.listeners.dtls.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME</code>



**Fields**

- acceptors: <code>integer()</code>
  * default: 
  `16`

  Size of the acceptor pool.

- udp_options: <code>[gateway:udp_opts](#gateway-udp_opts)</code>



- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable the listener.

- bind: <code>emqx_gateway_schema:ip_port() | integer()</code>

  The IP address and port that the listener will bind.

- max_connections: <code>integer()</code>
  * default: 
  `1024`

  Maximum number of concurrent connections.

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  Maximum connections per second.

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  Default authentication configs for all the gateway listeners. For per-listener overrides see <code>authentication</code>
   in listener configs

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener. 
  When set to <code>false</code> clients will be allowed to connect without authentication.

- mountpoint: <code>binary()</code>

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber. The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`, then the client actually subscribes to the topic `some_tenant/t`. Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`, the message is routed to all the clients subscribed `some_tenant/t`, so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Variables in mountpoint string:
    - <code>${clientid}</code>: clientid
    - <code>${username}</code>: username


- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  The access control rules for this listener.
  See: https://github.com/emqtt/esockd#allowdeny

- dtls_options: <code>[gateway:dtls_opts](#gateway-dtls_opts)</code>

  DTLS socket options


## gateway:dtls_opts
Settings for the DTLS protocol.


**Config paths**

 - <code>gateway.coap.listeners.dtls.$name.dtls_options</code>
 - <code>gateway.exproto.listeners.dtls.$name.dtls_options</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.dtls_options</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.dtls_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__DTLS_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__DTLS_OPTIONS</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__DTLS_OPTIONS</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__DTLS_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>


  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.


- certfile: <code>binary()</code>


  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.


- keyfile: <code>binary()</code>

  PEM format private key file. 

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification. 

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse. 

- depth: <code>integer()</code>
  * default: 
  `10`


  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.<br/>


- password: <code>string()</code>


  String containing the user's password.
  Only used if the private key file is password-protected.


- versions: <code>[atom()]</code>
  * default: 
  `[dtlsv1.2, dtlsv1]`


  All TLS/DTLS versions to be supported.<br/>
  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
  In case PSK cipher suites are intended, make sure to configure
  <code>['tlsv1.2', 'tlsv1.1']</code> here.


- ciphers: <code>[string()]</code>
  * default: 
  `[]`


  This config holds TLS cipher suite names separated by comma,
  or as an array of strings. e.g.
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
  <br/>
  Ciphers (and their ordering) define the way in which the
  client and server encrypts information over the network connection.
  Selecting a good cipher suite is critical for the
  application's data security, confidentiality and performance.

  The names should be in OpenSSL string format (not RFC format).
  All default values and examples provided by EMQX config
  documentation are all in OpenSSL format.<br/>

  NOTE: Certain cipher suites are only compatible with
  specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
  incompatible cipher suites will be silently dropped.
  For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
  configuring cipher suites for other versions will have no effect.
  <br/>

  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
  If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
  PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`

  EMQX-internal callback that is used to lookup pre-shared key (PSK) identity. 

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.


- dhfile: <code>string()</code>


  Path to a file containing PEM-encoded Diffie-Hellman parameters
  to be used by the server if a cipher suite using Diffie-Hellman
  key exchange is negotiated. If not specified, default parameters
  are used.<br/>
  NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


- fail_if_no_peer_cert: <code>boolean()</code>
  * default: 
  `false`


  Used together with {verify, verify_peer} by an TLS/DTLS server.
  If set to true, the server fails if the client does not have a
  certificate to send, that is, sends an empty certificate.
  If set to false, it fails only if the client sends an invalid
  certificate (an empty certificate is considered valid).


- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`


  An important security setting, it forces the cipher to be set based
   on the server-specified order instead of the client-specified order,
   hence enforcing the (usually more properly configured) security
   ordering of the server administrator.


- client_renegotiation: <code>boolean()</code>
  * default: 
  `true`


  In protocols that support client-initiated renegotiation,
  the cost of resources of such an operation is higher for the server than the client.
  This can act as a vector for denial of service attacks.
  The SSL application already takes measures to counter-act such attempts,
  but client-initiated renegotiation can be strictly disabled by setting this option to false.
  The default value is true. Note that disabling renegotiation can result in
  long-lived connections becoming unusable due to limits on
  the number of messages the underlying cipher suite can encipher.


- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`


  Maximum time duration allowed for the handshake to complete


- gc_after_handshake: <code>boolean()</code>
  * default: 
  `false`


  Memory usage tuning. If enabled, will immediately perform a garbage collection after
  the TLS/SSL handshake.



## gateway:exproto
Settings for EMQX extension protocol (exproto).


**Config paths**

 - <code>gateway.exproto</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO</code>



**Fields**

- server: <code>[gateway:exproto_grpc_server](#gateway-exproto_grpc_server)</code>

  Configurations for starting the <code>ConnectionAdapter</code> service

- handler: <code>[gateway:exproto_grpc_handler](#gateway-exproto_grpc_handler)</code>

  Configurations for request to <code>ConnectionHandler</code> service

- mountpoint: <code>binary()</code>
  * default: 
  `""`

   

- listeners: <code>[gateway:tcp_udp_listeners](#gateway-tcp_udp_listeners)</code>

  Settings for the listeners.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable this gateway

- enable_stats: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable client process statistic

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  The idle time of the client connection process. It has two purposes:
    1. A newly created client process that does not receive any client requests after that time will be closed directly.
    2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo override.

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  Default authentication configs for all the gateway listeners. For per-listener overrides see <code>authentication</code>
   in listener configs


## gateway:exproto_grpc_handler
Settings for the exproto gRPC connection handler.


**Config paths**

 - <code>gateway.exproto.handler</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__HANDLER</code>



**Fields**

- address: <code>binary()</code>

  gRPC server address.

- ssl_options: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>

  SSL configuration for the gRPC client.


## gateway:exproto_grpc_server
Settings for the exproto gRPC server.


**Config paths**

 - <code>gateway.exproto.server</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__SERVER</code>



**Fields**

- bind: <code>emqx_gateway_schema:ip_port() | integer()</code>

  Listening address and port for the gRPC server.

- ssl_options: <code>[gateway:ssl_server_opts](#gateway-ssl_server_opts)</code>

  SSL configuration for the gRPC server.


## gateway
EMQX Gateway configuration root.


**Config paths**

 - <code>gateway</code>


**Env overrides**

 - <code>EMQX_GATEWAY</code>



**Fields**

- stomp: <code>[gateway:stomp](#gateway-stomp)</code>

  The Stomp Gateway configuration.
  This gateway supports v1.2/1.1/1.0

- mqttsn: <code>[gateway:mqttsn](#gateway-mqttsn)</code>

  The MQTT-SN Gateway configuration.
  This gateway only supports the v1.2 protocol

- coap: <code>[gateway:coap](#gateway-coap)</code>

  The CoAP Gateway configuration.
  This gateway is implemented based on RFC-7252 and https://core-wg.github.io/coap-pubsub/draft-ietf-core-pubsub.html

- lwm2m: <code>[gateway:lwm2m](#gateway-lwm2m)</code>

  The LwM2M Gateway configuration. This gateway only supports the v1.0.1 protocol.

- exproto: <code>[gateway:exproto](#gateway-exproto)</code>

  The Extension Protocol configuration


## gateway:lwm2m
The LwM2M protocol gateway.


**Config paths**

 - <code>gateway.lwm2m</code>


**Env overrides**

 - <code>EMQX_GATEWAY__LWM2M</code>



**Fields**

- xml_dir: <code>binary()</code>

  The Directory for LwM2M Resource definition.

- lifetime_min: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"15s"`

  Minimum value of lifetime allowed to be set by the LwM2M client.

- lifetime_max: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"86400s"`

  Maximum value of lifetime allowed to be set by the LwM2M client.

- qmode_time_window: <code>emqx_gateway_schema:duration_s()</code>
  * default: 
  `"22s"`

  The value of the time window during which the network link is considered valid by the LwM2M Gateway in QMode mode.
  For example, after receiving an update message from a client, any messages within this time window are sent directly to the LwM2M client, and all messages beyond this time window are temporarily stored in memory.

- auto_observe: <code>boolean()</code>
  * default: 
  `false`

  Automatically observe the object list of REGISTER packet.

- update_msg_publish_condition: <code>always | contains_object_list</code>
  * default: 
  `contains_object_list`

  Policy for publishing UPDATE event message.
    - always: send update events as long as the UPDATE request is received.
    - contains_object_list: send update events only if the UPDATE request carries any Object List


- translators: <code>[gateway:lwm2m_translators](#gateway-lwm2m_translators)</code>

  Topic configuration for LwM2M's gateway publishing and subscription.

- mountpoint: <code>binary()</code>
  * default: 
  `"lwm2m/${endpoint_name}/"`

   

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>

  Settings for the UDP listeners.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable this gateway

- enable_stats: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable client process statistic

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  The idle time of the client connection process. It has two purposes:
    1. A newly created client process that does not receive any client requests after that time will be closed directly.
    2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo override.

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  Default authentication configs for all the gateway listeners. For per-listener overrides see <code>authentication</code>
   in listener configs


## gateway:lwm2m_translators
MQTT topics that correspond to LwM2M events.


**Config paths**

 - <code>gateway.lwm2m.translators</code>


**Env overrides**

 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS</code>



**Fields**

- command: <code>[gateway:translator](#gateway-translator)</code>

  The topic for receiving downstream commands.
  For each new LwM2M client that succeeds in going online, the gateway creates a subscription relationship to receive downstream commands and send it to the LwM2M client

- response: <code>[gateway:translator](#gateway-translator)</code>

  The topic for gateway to publish the acknowledge events from LwM2M client

- notify: <code>[gateway:translator](#gateway-translator)</code>

  The topic for gateway to publish the notify events from LwM2M client.
  After succeed observe a resource of LwM2M client, Gateway will send the notify events via this topic, if the client reports any resource changes

- register: <code>[gateway:translator](#gateway-translator)</code>

  The topic for gateway to publish the register events from LwM2M client.

- update: <code>[gateway:translator](#gateway-translator)</code>

  The topic for gateway to publish the update events from LwM2M client


## gateway:mqttsn
The MQTT-SN (MQTT for Sensor Networks) protocol gateway.


**Config paths**

 - <code>gateway.mqttsn</code>


**Env overrides**

 - <code>EMQX_GATEWAY__MQTTSN</code>



**Fields**

- gateway_id: <code>integer()</code>
  * default: 
  `1`

  MQTT-SN Gateway ID.
  When the <code>broadcast</code> option is enabled, the gateway will broadcast ADVERTISE message with this value

- broadcast: <code>boolean()</code>
  * default: 
  `false`

  Whether to periodically broadcast ADVERTISE messages

- enable_qos3: <code>boolean()</code>
  * default: 
  `true`

  Allows connectionless clients to publish messages with a Qos of -1.
  This feature is defined for very simple client implementations which do not support any other features except this one. There is no connection setup nor tear down, no registration nor subscription. The client just sends its 'PUBLISH' messages to a GW

- subs_resume: <code>boolean()</code>
  * default: 
  `false`

  Whether to initiate all subscribed topic name registration messages to the client after the Session has been taken over by a new channel

- predefined: <code>[[gateway:mqttsn_predefined](#gateway-mqttsn_predefined)]</code>
  * default: 
  `[]`

  The pre-defined topic IDs and topic names.
  A 'pre-defined' topic ID is a topic ID whose mapping to a topic name is known in advance by both the client's application and the gateway

- mountpoint: <code>binary()</code>
  * default: 
  `""`

   

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>

  Settings for the UDP listeners.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable this gateway

- enable_stats: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable client process statistic

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  The idle time of the client connection process. It has two purposes:
    1. A newly created client process that does not receive any client requests after that time will be closed directly.
    2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo override.

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  Default authentication configs for all the gateway listeners. For per-listener overrides see <code>authentication</code>
   in listener configs


## gateway:mqttsn_predefined
The pre-defined topic name corresponding to the pre-defined topic
ID of N.

Note: the pre-defined topic ID of 0 is reserved.


**Config paths**

 - <code>gateway.mqttsn.predefined.$INDEX</code>


**Env overrides**

 - <code>EMQX_GATEWAY__MQTTSN__PREDEFINED__$INDEX</code>



**Fields**

- id: <code>integer()</code>

  Topic ID. Range: 1-65535

- topic: <code>binary()</code>

  Topic Name


## gateway:ssl_listener
Settings for the SSL listener.


**Config paths**

 - <code>gateway.exproto.listeners.ssl.$name</code>
 - <code>gateway.stomp.listeners.ssl.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME</code>



**Fields**

- acceptors: <code>integer()</code>
  * default: 
  `16`

  Size of the acceptor pool.

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>

  Setting the TCP socket options.

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.
  See: https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"15s"`

  Timeout for proxy protocol.
  EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable the listener.

- bind: <code>emqx_gateway_schema:ip_port() | integer()</code>

  The IP address and port that the listener will bind.

- max_connections: <code>integer()</code>
  * default: 
  `1024`

  Maximum number of concurrent connections.

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  Maximum connections per second.

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  Default authentication configs for all the gateway listeners. For per-listener overrides see <code>authentication</code>
   in listener configs

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener. 
  When set to <code>false</code> clients will be allowed to connect without authentication.

- mountpoint: <code>binary()</code>

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber. The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`, then the client actually subscribes to the topic `some_tenant/t`. Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`, the message is routed to all the clients subscribed `some_tenant/t`, so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Variables in mountpoint string:
    - <code>${clientid}</code>: clientid
    - <code>${username}</code>: username


- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  The access control rules for this listener.
  See: https://github.com/emqtt/esockd#allowdeny

- ssl_options: <code>[broker:listener_ssl_opts](#broker-listener_ssl_opts)</code>

  SSL Socket options.


## gateway:ssl_server_opts
SSL configuration for the server.


**Config paths**

 - <code>gateway.exproto.server.ssl_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__SERVER__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>


  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.


- certfile: <code>binary()</code>


  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.


- keyfile: <code>binary()</code>

  PEM format private key file. 

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification. 

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse. 

- depth: <code>integer()</code>
  * default: 
  `10`


  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.<br/>


- password: <code>string()</code>


  String containing the user's password.
  Only used if the private key file is password-protected.


- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2, tlsv1.1, tlsv1]`


  All TLS/DTLS versions to be supported.<br/>
  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
  In case PSK cipher suites are intended, make sure to configure
  <code>['tlsv1.2', 'tlsv1.1']</code> here.


- ciphers: <code>[string()]</code>
  * default: 
  `[]`


  This config holds TLS cipher suite names separated by comma,
  or as an array of strings. e.g.
  <code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code> or
  <code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>.
  <br/>
  Ciphers (and their ordering) define the way in which the
  client and server encrypts information over the network connection.
  Selecting a good cipher suite is critical for the
  application's data security, confidentiality and performance.

  The names should be in OpenSSL string format (not RFC format).
  All default values and examples provided by EMQX config
  documentation are all in OpenSSL format.<br/>

  NOTE: Certain cipher suites are only compatible with
  specific TLS <code>versions</code> ('tlsv1.1', 'tlsv1.2' or 'tlsv1.3')
  incompatible cipher suites will be silently dropped.
  For instance, if only 'tlsv1.3' is given in the <code>versions</code>,
  configuring cipher suites for other versions will have no effect.
  <br/>

  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config<br/>
  If PSK cipher suites are intended, 'tlsv1.3' should be disabled from <code>versions</code>.<br/>
  PSK cipher suites: <code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
  RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
  RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>


- user_lookup_fun: <code>string()</code>
  * default: 
  `"emqx_tls_psk:lookup"`

  EMQX-internal callback that is used to lookup pre-shared key (PSK) identity. 

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`


  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.


- dhfile: <code>string()</code>


  Path to a file containing PEM-encoded Diffie-Hellman parameters
  to be used by the server if a cipher suite using Diffie-Hellman
  key exchange is negotiated. If not specified, default parameters
  are used.<br/>
  NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.


- fail_if_no_peer_cert: <code>boolean()</code>
  * default: 
  `false`


  Used together with {verify, verify_peer} by an TLS/DTLS server.
  If set to true, the server fails if the client does not have a
  certificate to send, that is, sends an empty certificate.
  If set to false, it fails only if the client sends an invalid
  certificate (an empty certificate is considered valid).


- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`


  An important security setting, it forces the cipher to be set based
   on the server-specified order instead of the client-specified order,
   hence enforcing the (usually more properly configured) security
   ordering of the server administrator.


- client_renegotiation: <code>boolean()</code>
  * default: 
  `true`


  In protocols that support client-initiated renegotiation,
  the cost of resources of such an operation is higher for the server than the client.
  This can act as a vector for denial of service attacks.
  The SSL application already takes measures to counter-act such attempts,
  but client-initiated renegotiation can be strictly disabled by setting this option to false.
  The default value is true. Note that disabling renegotiation can result in
  long-lived connections becoming unusable due to limits on
  the number of messages the underlying cipher suite can encipher.


- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`


  Maximum time duration allowed for the handshake to complete



## gateway:stomp
The STOMP protocol gateway provides EMQX with the ability to access STOMP
(Simple (or Streaming) Text Orientated Messaging Protocol) protocol.


**Config paths**

 - <code>gateway.stomp</code>


**Env overrides**

 - <code>EMQX_GATEWAY__STOMP</code>



**Fields**

- frame: <code>[gateway:stomp_frame](#gateway-stomp_frame)</code>



- mountpoint: <code>binary()</code>
  * default: 
  `""`

   

- listeners: <code>[gateway:tcp_listeners](#gateway-tcp_listeners)</code>

  Settings for the TCP listeners.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable this gateway

- enable_stats: <code>boolean()</code>
  * default: 
  `true`

  Whether to enable client process statistic

- idle_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"30s"`

  The idle time of the client connection process. It has two purposes:
    1. A newly created client process that does not receive any client requests after that time will be closed directly.
    2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo override.

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  Default authentication configs for all the gateway listeners. For per-listener overrides see <code>authentication</code>
   in listener configs


## gateway:stomp_frame
Size limits for the STOMP frames.


**Config paths**

 - <code>gateway.stomp.frame</code>


**Env overrides**

 - <code>EMQX_GATEWAY__STOMP__FRAME</code>



**Fields**

- max_headers: <code>non_neg_integer()</code>
  * default: 
  `10`

  The maximum number of Header

- max_headers_length: <code>non_neg_integer()</code>
  * default: 
  `1024`

  The maximum string length of the Header Value

- max_body_length: <code>integer()</code>
  * default: 
  `65536`

  Maximum number of bytes of Body allowed per Stomp packet


## gateway:tcp_listener
Settings for the TCP listener.


**Config paths**

 - <code>gateway.exproto.listeners.tcp.$name</code>
 - <code>gateway.stomp.listeners.tcp.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME</code>



**Fields**

- acceptors: <code>integer()</code>
  * default: 
  `16`

  Size of the acceptor pool.

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>

  Setting the TCP socket options.

- proxy_protocol: <code>boolean()</code>
  * default: 
  `false`

  Enable the Proxy Protocol V1/2 if the EMQX cluster is deployed behind HAProxy or Nginx.
  See: https://www.haproxy.com/blog/haproxy/proxy-protocol/

- proxy_protocol_timeout: <code>emqx_gateway_schema:duration()</code>
  * default: 
  `"15s"`

  Timeout for proxy protocol.
  EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable the listener.

- bind: <code>emqx_gateway_schema:ip_port() | integer()</code>

  The IP address and port that the listener will bind.

- max_connections: <code>integer()</code>
  * default: 
  `1024`

  Maximum number of concurrent connections.

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  Maximum connections per second.

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  Default authentication configs for all the gateway listeners. For per-listener overrides see <code>authentication</code>
   in listener configs

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener. 
  When set to <code>false</code> clients will be allowed to connect without authentication.

- mountpoint: <code>binary()</code>

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber. The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`, then the client actually subscribes to the topic `some_tenant/t`. Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`, the message is routed to all the clients subscribed `some_tenant/t`, so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Variables in mountpoint string:
    - <code>${clientid}</code>: clientid
    - <code>${username}</code>: username


- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  The access control rules for this listener.
  See: https://github.com/emqtt/esockd#allowdeny


## gateway:tcp_listeners
Settings for the TCP listeners.


**Config paths**

 - <code>gateway.stomp.listeners</code>


**Env overrides**

 - <code>EMQX_GATEWAY__STOMP__LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [gateway:tcp_listener](#gateway-tcp_listener)}</code>

   

- ssl: <code>{$name -> [gateway:ssl_listener](#gateway-ssl_listener)}</code>

   


## gateway:tcp_udp_listeners
Settings for the listeners.


**Config paths**

 - <code>gateway.exproto.listeners</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [gateway:tcp_listener](#gateway-tcp_listener)}</code>

   

- ssl: <code>{$name -> [gateway:ssl_listener](#gateway-ssl_listener)}</code>

   

- udp: <code>{$name -> [gateway:udp_listener](#gateway-udp_listener)}</code>

   

- dtls: <code>{$name -> [gateway:dtls_listener](#gateway-dtls_listener)}</code>

   


## gateway:translator
MQTT topic that corresponds to a particular type of event.


**Config paths**

 - <code>gateway.lwm2m.translators.command</code>
 - <code>gateway.lwm2m.translators.notify</code>
 - <code>gateway.lwm2m.translators.register</code>
 - <code>gateway.lwm2m.translators.response</code>
 - <code>gateway.lwm2m.translators.update</code>


**Env overrides**

 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS__COMMAND</code>
 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS__NOTIFY</code>
 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS__REGISTER</code>
 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS__RESPONSE</code>
 - <code>EMQX_GATEWAY__LWM2M__TRANSLATORS__UPDATE</code>



**Fields**

- topic: <code>binary()</code>

  Topic Name

- qos: <code>qos()</code>
  * default: 
  `0`

  QoS Level


## gateway:udp_listener
Settings for the UDP listener.


**Config paths**

 - <code>gateway.coap.listeners.udp.$name</code>
 - <code>gateway.exproto.listeners.udp.$name</code>
 - <code>gateway.lwm2m.listeners.udp.$name</code>
 - <code>gateway.mqttsn.listeners.udp.$name</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME</code>



**Fields**

- udp_options: <code>[gateway:udp_opts](#gateway-udp_opts)</code>



- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable the listener.

- bind: <code>emqx_gateway_schema:ip_port() | integer()</code>

  The IP address and port that the listener will bind.

- max_connections: <code>integer()</code>
  * default: 
  `1024`

  Maximum number of concurrent connections.

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  Maximum connections per second.

- authentication: <code>[authn-builtin_db:authentication](#authn-builtin_db-authentication) | [authn-mysql:authentication](#authn-mysql-authentication) | [authn-postgresql:authentication](#authn-postgresql-authentication) | [authn-mongodb:standalone](#authn-mongodb-standalone) | [authn-mongodb:replica-set](#authn-mongodb-replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb-sharded-cluster) | [authn-redis:standalone](#authn-redis-standalone) | [authn-redis:cluster](#authn-redis-cluster) | [authn-redis:sentinel](#authn-redis-sentinel) | [authn-http:get](#authn-http-get) | [authn-http:post](#authn-http-post) | [authn-jwt:hmac-based](#authn-jwt-hmac-based) | [authn-jwt:public-key](#authn-jwt-public-key) | [authn-jwt:jwks](#authn-jwt-jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db-authentication)</code>

  Default authentication configs for all the gateway listeners. For per-listener overrides see <code>authentication</code>
   in listener configs

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener. 
  When set to <code>false</code> clients will be allowed to connect without authentication.

- mountpoint: <code>binary()</code>

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber. The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`, then the client actually subscribes to the topic `some_tenant/t`. Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`, the message is routed to all the clients subscribed `some_tenant/t`, so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Variables in mountpoint string:
    - <code>${clientid}</code>: clientid
    - <code>${username}</code>: username


- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  The access control rules for this listener.
  See: https://github.com/emqtt/esockd#allowdeny


## gateway:udp_listeners
Settings for the UDP listeners.


**Config paths**

 - <code>gateway.coap.listeners</code>
 - <code>gateway.lwm2m.listeners</code>
 - <code>gateway.mqttsn.listeners</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS</code>



**Fields**

- udp: <code>{$name -> [gateway:udp_listener](#gateway-udp_listener)}</code>

   

- dtls: <code>{$name -> [gateway:dtls_listener](#gateway-dtls_listener)}</code>

   


## gateway:udp_opts
Settings for the UDP sockets.


**Config paths**

 - <code>gateway.coap.listeners.dtls.$name.udp_options</code>
 - <code>gateway.coap.listeners.udp.$name.udp_options</code>
 - <code>gateway.exproto.listeners.dtls.$name.udp_options</code>
 - <code>gateway.exproto.listeners.udp.$name.udp_options</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.udp_options</code>
 - <code>gateway.lwm2m.listeners.udp.$name.udp_options</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.udp_options</code>
 - <code>gateway.mqttsn.listeners.udp.$name.udp_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__UDP_OPTIONS</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__UDP_OPTIONS</code>



**Fields**

- active_n: <code>integer()</code>
  * default: 
  `100`

  Specify the {active, N} option for the socket.
  See: https://erlang.org/doc/man/inet.html#setopts-2

- recbuf: <code>emqx_gateway_schema:bytesize()</code>

  Size of the kernel-space receive buffer for the socket.

- sndbuf: <code>emqx_gateway_schema:bytesize()</code>

  Size of the kernel-space send buffer for the socket.

- buffer: <code>emqx_gateway_schema:bytesize()</code>

  Size of the user-space buffer for the socket.

- reuseaddr: <code>boolean()</code>
  * default: 
  `true`

  Allow local reuse of port numbers.


## limiter:bucket_opts
Settings for the bucket.


**Config paths**

 - <code>listeners.quic.$name.limiter.bytes_in</code>
 - <code>listeners.quic.$name.limiter.connection</code>
 - <code>listeners.quic.$name.limiter.message_in</code>
 - <code>listeners.quic.$name.limiter.message_routing</code>
 - <code>listeners.ssl.$name.limiter.bytes_in</code>
 - <code>listeners.ssl.$name.limiter.connection</code>
 - <code>listeners.ssl.$name.limiter.message_in</code>
 - <code>listeners.ssl.$name.limiter.message_routing</code>
 - <code>listeners.tcp.$name.limiter.bytes_in</code>
 - <code>listeners.tcp.$name.limiter.connection</code>
 - <code>listeners.tcp.$name.limiter.message_in</code>
 - <code>listeners.tcp.$name.limiter.message_routing</code>
 - <code>listeners.ws.$name.limiter.bytes_in</code>
 - <code>listeners.ws.$name.limiter.connection</code>
 - <code>listeners.ws.$name.limiter.message_in</code>
 - <code>listeners.ws.$name.limiter.message_routing</code>
 - <code>listeners.wss.$name.limiter.bytes_in</code>
 - <code>listeners.wss.$name.limiter.connection</code>
 - <code>listeners.wss.$name.limiter.message_in</code>
 - <code>listeners.wss.$name.limiter.message_routing</code>


**Env overrides**

 - <code>EMQX_LISTENERS__QUIC__$NAME__LIMITER__BYTES_IN</code>
 - <code>EMQX_LISTENERS__QUIC__$NAME__LIMITER__CONNECTION</code>
 - <code>EMQX_LISTENERS__QUIC__$NAME__LIMITER__MESSAGE_IN</code>
 - <code>EMQX_LISTENERS__QUIC__$NAME__LIMITER__MESSAGE_ROUTING</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__LIMITER__BYTES_IN</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__LIMITER__CONNECTION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__LIMITER__MESSAGE_IN</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__LIMITER__MESSAGE_ROUTING</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__LIMITER__BYTES_IN</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__LIMITER__CONNECTION</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__LIMITER__MESSAGE_IN</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__LIMITER__MESSAGE_ROUTING</code>
 - <code>EMQX_LISTENERS__WS__$NAME__LIMITER__BYTES_IN</code>
 - <code>EMQX_LISTENERS__WS__$NAME__LIMITER__CONNECTION</code>
 - <code>EMQX_LISTENERS__WS__$NAME__LIMITER__MESSAGE_IN</code>
 - <code>EMQX_LISTENERS__WS__$NAME__LIMITER__MESSAGE_ROUTING</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__LIMITER__BYTES_IN</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__LIMITER__CONNECTION</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__LIMITER__MESSAGE_IN</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__LIMITER__MESSAGE_ROUTING</code>



**Fields**

- rate: <code>emqx_limiter_schema:rate()</code>
  * default: 
  `"infinity"`

  Rate for this bucket.

- capacity: <code>emqx_limiter_schema:capacity()</code>
  * default: 
  `"infinity"`

  The capacity of this token bucket.

- initial: <code>emqx_limiter_schema:initial()</code>
  * default: 
  `"0"`

  The initial number of tokens for this bucket.


## limiter:client_fields
Fields of the client level.


**Config paths**

 - <code>limiter.client</code>


**Env overrides**

 - <code>EMQX_LIMITER__CLIENT</code>



**Fields**

- bytes_in: <code>[limiter:client_opts](#limiter-client_opts)</code>
  * default: 
  `{}`

  The bytes_in limiter.
  This is used to limit the inbound bytes rate for this EMQX node.
  Once the limit is reached, the restricted client will be slow down even be hung for a while.

- message_in: <code>[limiter:client_opts](#limiter-client_opts)</code>
  * default: 
  `{}`

  The message in limiter.
  This is used to limit the inbound message numbers for this EMQX node
  Once the limit is reached, the restricted client will be slow down even be hung for a while.

- connection: <code>[limiter:client_opts](#limiter-client_opts)</code>
  * default: 
  `{}`

  The connection limiter.
  This is used to limit the connection rate for this EMQX node.
  Once the limit is reached, new connections will be refused

- message_routing: <code>[limiter:client_opts](#limiter-client_opts)</code>
  * default: 
  `{}`

  The message routing limiter.
  This is used to limit the forwarding rate for this EMQX node.
  Once the limit is reached, new publish will be refused

- internal: <code>[limiter:client_opts](#limiter-client_opts)</code>
  * default: 
  `{}`

  Limiter for EMQX internal app.


## limiter:client_opts
Settings for the client in bucket level.


**Config paths**

 - <code>limiter.client.bytes_in</code>
 - <code>limiter.client.connection</code>
 - <code>limiter.client.internal</code>
 - <code>limiter.client.message_in</code>
 - <code>limiter.client.message_routing</code>
 - <code>listeners.quic.$name.limiter.client.bytes_in</code>
 - <code>listeners.quic.$name.limiter.client.connection</code>
 - <code>listeners.quic.$name.limiter.client.message_in</code>
 - <code>listeners.quic.$name.limiter.client.message_routing</code>
 - <code>listeners.ssl.$name.limiter.client.bytes_in</code>
 - <code>listeners.ssl.$name.limiter.client.connection</code>
 - <code>listeners.ssl.$name.limiter.client.message_in</code>
 - <code>listeners.ssl.$name.limiter.client.message_routing</code>
 - <code>listeners.tcp.$name.limiter.client.bytes_in</code>
 - <code>listeners.tcp.$name.limiter.client.connection</code>
 - <code>listeners.tcp.$name.limiter.client.message_in</code>
 - <code>listeners.tcp.$name.limiter.client.message_routing</code>
 - <code>listeners.ws.$name.limiter.client.bytes_in</code>
 - <code>listeners.ws.$name.limiter.client.connection</code>
 - <code>listeners.ws.$name.limiter.client.message_in</code>
 - <code>listeners.ws.$name.limiter.client.message_routing</code>
 - <code>listeners.wss.$name.limiter.client.bytes_in</code>
 - <code>listeners.wss.$name.limiter.client.connection</code>
 - <code>listeners.wss.$name.limiter.client.message_in</code>
 - <code>listeners.wss.$name.limiter.client.message_routing</code>
 - <code>retainer.flow_control.batch_deliver_limiter.client</code>


**Env overrides**

 - <code>EMQX_LIMITER__CLIENT__BYTES_IN</code>
 - <code>EMQX_LIMITER__CLIENT__CONNECTION</code>
 - <code>EMQX_LIMITER__CLIENT__INTERNAL</code>
 - <code>EMQX_LIMITER__CLIENT__MESSAGE_IN</code>
 - <code>EMQX_LIMITER__CLIENT__MESSAGE_ROUTING</code>
 - <code>EMQX_LISTENERS__QUIC__$NAME__LIMITER__CLIENT__BYTES_IN</code>
 - <code>EMQX_LISTENERS__QUIC__$NAME__LIMITER__CLIENT__CONNECTION</code>
 - <code>EMQX_LISTENERS__QUIC__$NAME__LIMITER__CLIENT__MESSAGE_IN</code>
 - <code>EMQX_LISTENERS__QUIC__$NAME__LIMITER__CLIENT__MESSAGE_ROUTING</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__LIMITER__CLIENT__BYTES_IN</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__LIMITER__CLIENT__CONNECTION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__LIMITER__CLIENT__MESSAGE_IN</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__LIMITER__CLIENT__MESSAGE_ROUTING</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__LIMITER__CLIENT__BYTES_IN</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__LIMITER__CLIENT__CONNECTION</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__LIMITER__CLIENT__MESSAGE_IN</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__LIMITER__CLIENT__MESSAGE_ROUTING</code>
 - <code>EMQX_LISTENERS__WS__$NAME__LIMITER__CLIENT__BYTES_IN</code>
 - <code>EMQX_LISTENERS__WS__$NAME__LIMITER__CLIENT__CONNECTION</code>
 - <code>EMQX_LISTENERS__WS__$NAME__LIMITER__CLIENT__MESSAGE_IN</code>
 - <code>EMQX_LISTENERS__WS__$NAME__LIMITER__CLIENT__MESSAGE_ROUTING</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__LIMITER__CLIENT__BYTES_IN</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__LIMITER__CLIENT__CONNECTION</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__LIMITER__CLIENT__MESSAGE_IN</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__LIMITER__CLIENT__MESSAGE_ROUTING</code>
 - <code>EMQX_RETAINER__FLOW_CONTROL__BATCH_DELIVER_LIMITER__CLIENT</code>



**Fields**

- rate: <code>emqx_limiter_schema:rate()</code>
  * default: 
  `"infinity"`

  Rate for this bucket.

- initial: <code>emqx_limiter_schema:initial()</code>
  * default: 
  `"0"`

  The initial number of tokens for this bucket.

- low_watermark: <code>emqx_limiter_schema:initial()</code>
  * default: 
  `"0"`

  If the remaining tokens are lower than this value,
  the check/consume will succeed, but it will be forced to wait for a short period of time.

- capacity: <code>emqx_limiter_schema:capacity()</code>
  * default: 
  `"infinity"`

  The capacity of per user.

- divisible: <code>boolean()</code>
  * default: 
  `false`

  Is it possible to split the number of requested tokens?

- max_retry_time: <code>emqx_schema:duration()</code>
  * default: 
  `"10s"`

  The maximum retry time when acquire failed.

- failure_strategy: <code>emqx_limiter_schema:failure_strategy()</code>
  * default: 
  `force`

  The strategy when all the retries failed.


## limiter:internal
Internal limiter.


**Config paths**

 - <code>retainer.flow_control.batch_deliver_limiter</code>


**Env overrides**

 - <code>EMQX_RETAINER__FLOW_CONTROL__BATCH_DELIVER_LIMITER</code>



**Fields**

- rate: <code>emqx_limiter_schema:rate()</code>
  * default: 
  `"infinity"`

  Rate for this bucket.

- capacity: <code>emqx_limiter_schema:capacity()</code>
  * default: 
  `"infinity"`

  The capacity of this token bucket.

- initial: <code>emqx_limiter_schema:initial()</code>
  * default: 
  `"0"`

  The initial number of tokens for this bucket.

- client: <code>[limiter:client_opts](#limiter-client_opts)</code>

  The rate limit for each user of the bucket


## limiter
Settings for the rate limiter.


**Config paths**

 - <code>limiter</code>


**Env overrides**

 - <code>EMQX_LIMITER</code>



**Fields**

- bytes_in: <code>[limiter:node_opts](#limiter-node_opts)</code>
  * default: 
  `{}`

  The bytes_in limiter.
  This is used to limit the inbound bytes rate for this EMQX node.
  Once the limit is reached, the restricted client will be slow down even be hung for a while.

- message_in: <code>[limiter:node_opts](#limiter-node_opts)</code>
  * default: 
  `{}`

  The message in limiter.
  This is used to limit the inbound message numbers for this EMQX node
  Once the limit is reached, the restricted client will be slow down even be hung for a while.

- connection: <code>[limiter:node_opts](#limiter-node_opts)</code>
  * default: 
  `{}`

  The connection limiter.
  This is used to limit the connection rate for this EMQX node.
  Once the limit is reached, new connections will be refused

- message_routing: <code>[limiter:node_opts](#limiter-node_opts)</code>
  * default: 
  `{}`

  The message routing limiter.
  This is used to limit the forwarding rate for this EMQX node.
  Once the limit is reached, new publish will be refused

- internal: <code>[limiter:node_opts](#limiter-node_opts)</code>
  * default: 
  `{}`

  Limiter for EMQX internal app.

- client: <code>[limiter:client_fields](#limiter-client_fields)</code>
  * default: 

  ```
  {
    bytes_in {}
    connection {}
    internal {}
    message_in {}
    message_routing {}
  }
  ```

  The rate limit for each user of the bucket


## limiter:listener_client_fields
Fields of the client level of the listener.


**Config paths**

 - <code>listeners.quic.$name.limiter.client</code>
 - <code>listeners.ssl.$name.limiter.client</code>
 - <code>listeners.tcp.$name.limiter.client</code>
 - <code>listeners.ws.$name.limiter.client</code>
 - <code>listeners.wss.$name.limiter.client</code>


**Env overrides**

 - <code>EMQX_LISTENERS__QUIC__$NAME__LIMITER__CLIENT</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__LIMITER__CLIENT</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__LIMITER__CLIENT</code>
 - <code>EMQX_LISTENERS__WS__$NAME__LIMITER__CLIENT</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__LIMITER__CLIENT</code>



**Fields**

- bytes_in: <code>[limiter:client_opts](#limiter-client_opts)</code>

  The bytes_in limiter.
  This is used to limit the inbound bytes rate for this EMQX node.
  Once the limit is reached, the restricted client will be slow down even be hung for a while.

- message_in: <code>[limiter:client_opts](#limiter-client_opts)</code>

  The message in limiter.
  This is used to limit the inbound message numbers for this EMQX node
  Once the limit is reached, the restricted client will be slow down even be hung for a while.

- connection: <code>[limiter:client_opts](#limiter-client_opts)</code>

  The connection limiter.
  This is used to limit the connection rate for this EMQX node.
  Once the limit is reached, new connections will be refused

- message_routing: <code>[limiter:client_opts](#limiter-client_opts)</code>

  The message routing limiter.
  This is used to limit the forwarding rate for this EMQX node.
  Once the limit is reached, new publish will be refused


## limiter:listener_fields
Fields of the listener.


**Config paths**

 - <code>listeners.quic.$name.limiter</code>
 - <code>listeners.ssl.$name.limiter</code>
 - <code>listeners.tcp.$name.limiter</code>
 - <code>listeners.ws.$name.limiter</code>
 - <code>listeners.wss.$name.limiter</code>


**Env overrides**

 - <code>EMQX_LISTENERS__QUIC__$NAME__LIMITER</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__LIMITER</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__LIMITER</code>
 - <code>EMQX_LISTENERS__WS__$NAME__LIMITER</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__LIMITER</code>



**Fields**

- bytes_in: <code>[limiter:bucket_opts](#limiter-bucket_opts)</code>

  The bytes_in limiter.
  This is used to limit the inbound bytes rate for this EMQX node.
  Once the limit is reached, the restricted client will be slow down even be hung for a while.

- message_in: <code>[limiter:bucket_opts](#limiter-bucket_opts)</code>

  The message in limiter.
  This is used to limit the inbound message numbers for this EMQX node
  Once the limit is reached, the restricted client will be slow down even be hung for a while.

- connection: <code>[limiter:bucket_opts](#limiter-bucket_opts)</code>

  The connection limiter.
  This is used to limit the connection rate for this EMQX node.
  Once the limit is reached, new connections will be refused

- message_routing: <code>[limiter:bucket_opts](#limiter-bucket_opts)</code>

  The message routing limiter.
  This is used to limit the forwarding rate for this EMQX node.
  Once the limit is reached, new publish will be refused

- client: <code>[limiter:listener_client_fields](#limiter-listener_client_fields)</code>

  The rate limit for each user of the bucket


## limiter:node_opts
Settings for the limiter of the node level.


**Config paths**

 - <code>limiter.bytes_in</code>
 - <code>limiter.connection</code>
 - <code>limiter.internal</code>
 - <code>limiter.message_in</code>
 - <code>limiter.message_routing</code>


**Env overrides**

 - <code>EMQX_LIMITER__BYTES_IN</code>
 - <code>EMQX_LIMITER__CONNECTION</code>
 - <code>EMQX_LIMITER__INTERNAL</code>
 - <code>EMQX_LIMITER__MESSAGE_IN</code>
 - <code>EMQX_LIMITER__MESSAGE_ROUTING</code>



**Fields**

- rate: <code>emqx_limiter_schema:rate()</code>
  * default: 
  `"infinity"`

  Rate for this bucket.

- burst: <code>emqx_limiter_schema:burst_rate()</code>
  * default: 
  `0`

  The burst, This value is based on rate.<br/>
   This value + rate = the maximum limit that can be achieved when limiter burst.


## modules:delayed
Settings for the delayed module.


**Config paths**

 - <code>delayed</code>


**Env overrides**

 - <code>EMQX_DELAYED</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable this feature

- max_delayed_messages: <code>integer()</code>
  * default: 
  `0`

  Maximum number of delayed messages (0 is no limit).


## modules:rewrite
The topic rewriting function of EMQX supports rewriting topic A to topic B when the client subscribes to topics, publishes messages, and cancels subscriptions according to user-configured rules.
Each rewrite rule consists of three parts: subject filter, regular expression, and target expression.
Under the premise that the subject rewriting function is enabled, when EMQX receives a subject-based MQTT message such as a `PUBLISH` message,
it will use the subject of the message to sequentially match the subject filter part of the rule in the configuration file. If the match is successful,
the regular expression is used to extract the information in the subject, and then replaced with the target expression to form a new subject.
Variables in the format of `$N` can be used in the target expression to match the elements extracted from the regular expression.
The value of `$N` is the Nth element extracted from the regular expression. For example, `$1` is the regular expression. The first element extracted by the expression.
It should be noted that EMQX uses reverse order to read the rewrite rules in the configuration file.
When a topic can match the topic filter of multiple topic rewrite rules at the same time, EMQX will only use the first rule it matches. Rewrite.
If the regular expression in this rule does not match the subject of the MQTT message, the rewriting will fail, and no other rules will be attempted for rewriting.
Therefore, users need to carefully design MQTT message topics and topic rewriting rules when using them.


**Config paths**

 - <code>rewrite.$INDEX</code>


**Env overrides**

 - <code>EMQX_REWRITE__$INDEX</code>



**Fields**

- action: <code>subscribe | publish | all</code>

  Topic rewriting takes effect on the type of operation:
    - `subscribe`: Rewrite topic when client do subscribe.
    - `publish`: Rewrite topic when client do publish.
    - `all`: Both

- source_topic: <code>binary()</code>

  Source topic, specified by the client.

- dest_topic: <code>binary()</code>

  Destination topic.

- re: <code>binary()</code>

  Regular expressions


## modules:telemetry
Settings for the telemetry module.


**Config paths**

 - <code>telemetry</code>


**Env overrides**

 - <code>EMQX_TELEMETRY</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable telemetry.


## modules:topic_metrics



**Config paths**

 - <code>topic_metrics.$INDEX</code>


**Env overrides**

 - <code>EMQX_TOPIC_METRICS__$INDEX</code>



**Fields**

- topic: <code>binary()</code>

  Collect metrics for the topic.


## rule_engine:builtin_action_console
Configuration for a built-in action.


**Config paths**

 - <code>rule_engine.rules.$id.actions.$INDEX</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE__RULES__$ID__ACTIONS__$INDEX</code>



**Fields**

- function: <code>console</code>

  Print the actions to the console


## rule_engine:builtin_action_republish
Configuration for a built-in action.


**Config paths**

 - <code>rule_engine.rules.$id.actions.$INDEX</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE__RULES__$ID__ACTIONS__$INDEX</code>



**Fields**

- function: <code>republish</code>

  Republish the message as a new MQTT message

- args: <code>[rule_engine:republish_args](#rule_engine-republish_args)</code>
  * default: 
  `{}`




## rule_engine:republish_args
The arguments of the built-in 'republish' action.One can use variables in the args.
The variables are selected by the rule. For example, if the rule SQL is defined as following:
<code>
    SELECT clientid, qos, payload FROM "t/1"
</code>
Then there are 3 variables available: <code>clientid</code>, <code>qos</code> and
<code>payload</code>. And if we've set the args to:
<code>
    {
        topic = "t/${clientid}"
        qos = "${qos}"
        payload = "msg: ${payload}"
    }
</code>
When the rule is triggered by an MQTT message with payload = `hello`, qos = 1,
clientid = `Steve`, the rule will republish a new MQTT message to topic `t/Steve`,
payload = `msg: hello`, and `qos = 1`.


**Config paths**

 - <code>rule_engine.rules.$id.actions.$INDEX.args</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE__RULES__$ID__ACTIONS__$INDEX__ARGS</code>



**Fields**

- topic: <code>binary()</code>


  The target topic of message to be re-published.
  Template with variables is allowed, see description of the 'republish_args'.


- qos: <code>qos() | binary()</code>
  * default: 
  `"${qos}"`


  The qos of the message to be re-published.
  Template with variables is allowed, see description of the 'republish_args'.
  Defaults to ${qos}. If variable ${qos} is not found from the selected result of the rule,
  0 is used.


- retain: <code>boolean() | binary()</code>
  * default: 
  `"${retain}"`


  The 'retain' flag of the message to be re-published.
  Template with variables is allowed, see description of the 'republish_args'.
  Defaults to ${retain}. If variable ${retain} is not found from the selected result
  of the rule, false is used.


- payload: <code>binary()</code>
  * default: 
  `"${payload}"`


  The payload of the message to be re-published.
  Template with variables is allowed, see description of the 'republish_args'.
  Defaults to ${payload}. If variable ${payload} is not found from the selected result
  of the rule, then the string "undefined" is used.


- user_properties: <code>binary()</code>
  * default: 
  `"${user_properties}"`


  From which variable should the MQTT message's User-Property pairs be taken from.
  The value must be a map.
  You may configure it to <code>${pub_props.'User-Property'}</code> or
  use <code>SELECT *,pub_props.'User-Property' as user_properties</code>
  to forward the original user properties to the republished message.
  You may also call <code>map_put</code> function like
  <code>map_put('my-prop-name', 'my-prop-value', user_properties) as user_properties</code>
  to inject user properties.
  NOTE: MQTT spec allows duplicated user property names, but EMQX Rule-Engine does not.



## rule_engine
Configuration for the EMQX Rule Engine.


**Config paths**

 - <code>rule_engine</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE</code>



**Fields**

- ignore_sys_message: <code>boolean()</code>
  * default: 
  `true`

  When set to 'true' (default), rule-engine will ignore messages published to $SYS topics.

- rules: <code>{$id -> [rule_engine:rules](#rule_engine-rules)}</code>
  * default: 
  `{}`

  The rules

- jq_function_default_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"10s"`

  Default timeout for the `jq` rule engine function

- jq_implementation_module: <code>jq_nif | jq_port</code>
  * default: 
  `jq_nif`
  * mapping: 
  `jq.jq_implementation_module`

  The implementation module for the jq rule engine function. The two options are jq_nif and jq_port. With the jq_nif option an Erlang NIF library is used while with the jq_port option an implementation based on Erlang port programs is used. The jq_nif option (the default option) is the fastest implementation of the two but jq_port is safer as the jq programs will not execute in the same process as the Erlang VM.


## rule_engine:rules
Configuration for a rule.


**Config paths**

 - <code>rule_engine.rules.$id</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE__RULES__$ID</code>



**Fields**

- name: <code>binary()</code>
  * default: 
  `""`

  The name of the rule

- sql: <code>binary()</code>


  SQL query to transform the messages.
  Example: <code>SELECT * FROM "test/topic" WHERE payload.x = 1</code>


- actions: <code>[binary() | [rule_engine:builtin_action_republish](#rule_engine-builtin_action_republish) | [rule_engine:builtin_action_console](#rule_engine-builtin_action_console) | [rule_engine:user_provided_function](#rule_engine-user_provided_function)]</code>
  * default: 
  `[]`


  A list of actions of the rule.
  An action can be a string that refers to the channel ID of an EMQX bridge, or an object
  that refers to a function.
  There a some built-in functions like "republish" and "console", and we also support user
  provided functions in the format: "{module}:{function}".
  The actions in the list are executed sequentially.
  This means that if one of the action is executing slowly, all the following actions will not
  be executed until it returns.
  If one of the action crashed, all other actions come after it will still be executed, in the
  original order.
  If there's any error when running an action, there will be an error message, and the 'failure'
  counter of the function action or the bridge channel will increase.


- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable or disable the rule

- description: <code>binary()</code>
  * default: 
  `""`

  The description of the rule

- metadata: <code>map()</code>

  Rule metadata, do not change manually


## rule_engine:user_provided_function
Configuration for a built-in action.


**Config paths**

 - <code>rule_engine.rules.$id.actions.$INDEX</code>


**Env overrides**

 - <code>EMQX_RULE_ENGINE__RULES__$ID__ACTIONS__$INDEX</code>



**Fields**

- function: <code>binary()</code>


  The user provided function. Should be in the format: '{module}:{function}'.
  Where {module} is the Erlang callback module and {function} is the Erlang function.

  To write your own function, checkout the function <code>console</code> and
  <code>republish</code> in the source file:
  <code>apps/emqx_rule_engine/src/emqx_rule_actions.erl</code> as an example.


- args: <code>map()</code>
  * default: 
  `{}`


  The args will be passed as the 3rd argument to module:function/3,
  checkout the function <code>console</code> and <code>republish</code> in the source file:
  <code>apps/emqx_rule_engine/src/emqx_rule_actions.erl</code> as an example.



## config
The config for MQTT Bridges.


**Config paths**

 - <code>bridges.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable or disable this bridge

- resource_opts: <code>[bridge_mqtt:creation_opts](#bridge_mqtt-creation_opts)</code>
  * default: 
  `{}`

  Resource options.

- mode: <code>cluster_shareload</code>
  * default: 
  `cluster_shareload`


  The mode of the MQTT Bridge.<br/>

  - cluster_shareload: create an MQTT connection on each node in the emqx cluster.<br/>
  In 'cluster_shareload' mode, the incoming load from the remote broker is shared by
  using shared subscription.<br/>
  Note that the 'clientid' is suffixed by the node name, this is to avoid
  clientid conflicts between different nodes. And we can only use shared subscription
  topic filters for <code>remote.topic</code> of ingress connections.


- server: <code>emqx_schema:host_port()</code>

  The host and port of the remote MQTT broker

- reconnect_interval: <code>string()</code>
  * default: 
  `"15s"`

  Reconnect interval. Delay for the MQTT bridge to retry establishing the connection in case of transportation failure. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  <br/>or combination of whereof: `1h5m0s`

- proto_ver: <code>v3 | v4 | v5</code>
  * default: 
  `v4`

  The MQTT protocol version

- bridge_mode: <code>boolean()</code>
  * default: 
  `false`


  If enable bridge mode.
  NOTE: This setting is only for MQTT protocol version older than 5.0, and the remote MQTT
  broker MUST support this feature.
      

- username: <code>binary()</code>

  The username of the MQTT protocol

- password: <code>binary()</code>

  The password of the MQTT protocol

- clean_start: <code>boolean()</code>
  * default: 
  `true`

  The clean-start or the clean-session of the MQTT protocol

- keepalive: <code>string()</code>
  * default: 
  `"300s"`

  MQTT Keepalive. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  <br/>or combination of whereof: `1h5m0s`

- retry_interval: <code>string()</code>
  * default: 
  `"15s"`

  Message retry interval. Delay for the MQTT bridge to retry sending the QoS1/QoS2 messages in case of ACK not received. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  <br/>or combination of whereof: `1h5m0s`

- max_inflight: <code>non_neg_integer()</code>
  * default: 
  `32`

  Max inflight (sent, but un-acked) messages of the MQTT protocol

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- ingress: <code>[connector-mqtt:ingress](#connector-mqtt-ingress)</code>

  The ingress config defines how this bridge receive messages from the remote MQTT broker, and then
          send them to the local broker.<br/>
          Template with variables is allowed in 'remote.qos', 'local.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
          NOTE: if this bridge is used as the input of a rule, and also 'local.topic' is
          configured, then messages got from the remote broker will be sent to both the 'local.topic' and
          the rule.

- egress: <code>[connector-mqtt:egress](#connector-mqtt-egress)</code>

  The egress config defines how this bridge forwards messages from the local broker to the remote broker.<br/>
  Template with variables is allowed in 'remote.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
  NOTE: if this bridge is used as the action of a rule, and also 'local.topic'
  is configured, then both the data got from the rule and the MQTT messages that matches
  'local.topic' will be forwarded.


## config
Configuration for an HTTP bridge.


**Config paths**

 - <code>bridges.webhook.$name</code>


**Env overrides**

 - <code>EMQX_BRIDGES__WEBHOOK__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable or disable this bridge

- resource_opts: <code>[bridge_webhook:creation_opts](#bridge_webhook-creation_opts)</code>
  * default: 
  `{}`

  Resource options.

- connect_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  The timeout when connecting to the HTTP server.

- retry_interval: <code>emqx_schema:duration()</code>

  Deprecated since 5.0.4.

- pool_type: <code>emqx_connector_http:pool_type()</code>
  * default: 
  `random`

  The type of the pool. Can be one of `random`, `hash`.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  The pool size.

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.

- request: <code>[connector-http:request](#connector-http-request)</code>


  If the request is provided, the caller can send HTTP requests via
  <code>emqx_resource:query(ResourceId, {send_message, BridgeId, Message})</code>


- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- url: <code>binary()</code>


  The URL of the HTTP Bridge.<br/>
  Template with variables is allowed in the path, but variables cannot be used in the scheme, host,
  or port part.<br/>
  For example, <code> http://localhost:9901/${topic} </code> is allowed, but
  <code> http://${host}:9901/message </code> or <code> http://localhost:${port}/message </code>
  is not allowed.


- local_topic: <code>binary()</code>


  The MQTT topic filter to be forwarded to the HTTP server. All MQTT 'PUBLISH' messages with the topic
  matching the local_topic will be forwarded.<br/>
  NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
  configured, then both the data got from the rule and the MQTT messages that match local_topic
  will be forwarded.


- method: <code>post | put | get | delete</code>
  * default: 
  `post`


  The method of the HTTP request. All the available methods are: post, put, get, delete.<br/>
  Template with variables is allowed.<br/>


- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    "cache-control" = "no-cache"
    connection = "keep-alive"
    "content-type" = "application/json"
    "keep-alive" = "timeout=5"
  }
  ```


  The headers of the HTTP request.<br/>
  Template with variables is allowed.


- body: <code>binary()</code>
  * default: 
  `"${payload}"`


  The body of the HTTP request.<br/>
  Template with variables is allowed.


- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  HTTP request max retry times if failed.

- request_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  HTTP request timeout.


## cluster_dns
Service discovery via DNS SRV records.


**Config paths**

 - <code>cluster.dns</code>


**Env overrides**

 - <code>EMQX_CLUSTER__DNS</code>



**Fields**

- name: <code>string()</code>
  * default: 
  `"localhost"`

  The domain name from which to discover peer EMQX nodes' IP addresses.
  Applicable when <code>cluster.discovery_strategy = dns</code>


- record_type: <code>a | srv</code>
  * default: 
  `a`

  DNS record type. 


## cluster_etcd
Service discovery using 'etcd' service.


**Config paths**

 - <code>cluster.etcd</code>


**Env overrides**

 - <code>EMQX_CLUSTER__ETCD</code>



**Fields**

- server: <code>emqx_schema:comma_separated_list()</code>

  List of endpoint URLs of the etcd cluster

- prefix: <code>string()</code>
  * default: 
  `"emqxcl"`

  Key prefix used for EMQX service discovery.

- node_ttl: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  Expiration time of the etcd key associated with the node.
  It is refreshed automatically, as long as the node is alive.
            

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>

  Options for the TLS connection to the etcd cluster.


## cluster_k8s
Service discovery via Kubernetes API server.


**Config paths**

 - <code>cluster.k8s</code>


**Env overrides**

 - <code>EMQX_CLUSTER__K8S</code>



**Fields**

- apiserver: <code>string()</code>
  * default: 
  `"http://10.110.111.204:8080"`

  Kubernetes API endpoint URL.

- service_name: <code>string()</code>
  * default: 
  `"emqx"`

  EMQX broker service name.

- address_type: <code>ip | dns | hostname</code>
  * default: 
  `ip`

  Address type used for connecting to the discovered nodes.
  Setting <code>cluster.k8s.address_type</code> to <code>ip</code> will
  make EMQX to discover IP addresses of peer nodes from Kubernetes API.


- namespace: <code>string()</code>
  * default: 
  `"default"`

  Kubernetes namespace.

- suffix: <code>string()</code>
  * default: 
  `"pod.local"`

  Node name suffix.<br/>
  Note: this parameter is only relevant when <code>address_type</code> is <code>dns</code>
  or <code>hostname</code>.


## cluster_mcast
Service discovery via UDP multicast.


**Config paths**

 - <code>cluster.mcast</code>


**Env overrides**

 - <code>EMQX_CLUSTER__MCAST</code>



**Fields**

- addr: <code>string()</code>
  * default: 
  `"239.192.0.1"`

  Multicast IPv4 address.

- ports: <code>[integer()]</code>
  * default: 
  `[4369,4370]`

  List of UDP ports used for service discovery.<br/>
  Note: probe messages are broadcast to all the specified ports.
            

- iface: <code>string()</code>
  * default: 
  `"0.0.0.0"`

  Local IP address the node discovery service needs to bind to.

- ttl: <code>0..255</code>
  * default: 
  `255`

  Time-to-live (TTL) for the outgoing UDP datagrams.

- loop: <code>boolean()</code>
  * default: 
  `true`

  If <code>true</code>, loop UDP datagrams back to the local socket.

- sndbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `"16KB"`

  Size of the kernel-level buffer for outgoing datagrams.

- recbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `"16KB"`

  Size of the kernel-level buffer for incoming datagrams.

- buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `"32KB"`

  Size of the user-level buffer.


## cluster_static
Service discovery via static nodes.
The new node joins the cluster by connecting to one of the bootstrap nodes.


**Config paths**

 - <code>cluster.static</code>


**Env overrides**

 - <code>EMQX_CLUSTER__STATIC</code>



**Fields**

- seeds: <code>[atom()]</code>
  * default: 
  `[]`

  List EMQX node names in the static cluster. See <code>node.name</code>.


## authorization
Settings that control client authorization.


**Config paths**

 - <code>authorization</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION</code>



**Fields**

- no_match: <code>allow | deny</code>
  * default: 
  `allow`


  Default access control action if the user or client matches no ACL rules,
  or if no such user or client is found by the configurable authorization
  sources such as built_in_database, an HTTP API, or a query against PostgreSQL.
  Find more details in 'authorization.sources' config.


- deny_action: <code>ignore | disconnect</code>
  * default: 
  `ignore`

  The action when the authorization check rejects an operation.

- cache: <code>[broker:cache](#broker-cache)</code>



- sources: <code>[[authz:file](#authz-file) | [authz:http_get](#authz-http_get) | [authz:http_post](#authz-http_post) | [authz:mnesia](#authz-mnesia) | [authz:mongo_single](#authz-mongo_single) | [authz:mongo_rs](#authz-mongo_rs) | [authz:mongo_sharded](#authz-mongo_sharded) | [authz:mysql](#authz-mysql) | [authz:postgresql](#authz-postgresql) | [authz:redis_single](#authz-redis_single) | [authz:redis_sentinel](#authz-redis_sentinel) | [authz:redis_cluster](#authz-redis_cluster)]</code>
  * default: 
  `[]`


  Authorization data sources.<br/>
  An array of authorization (ACL) data providers.
  It is designed as an array, not a hash-map, so the sources can be
  ordered to form a chain of access controls.<br/>

  When authorizing a 'publish' or 'subscribe' action, the configured
  sources are checked in order. When checking an ACL source,
  in case the client (identified by username or client ID) is not found,
  it moves on to the next source. And it stops immediately
  once an 'allow' or 'deny' decision is returned.<br/>

  If the client is not found in any of the sources,
  the default action configured in 'authorization.no_match' is applied.<br/>

  NOTE:
  The source elements are identified by their 'type'.
  It is NOT allowed to configure two or more sources of the same type.



## cluster
EMQX nodes can form a cluster to scale up the total capacity.<br/>
      Here holds the configs to instruct how individual nodes can discover each other.


**Config paths**

 - <code>cluster</code>


**Env overrides**

 - <code>EMQX_CLUSTER</code>



**Fields**

- name: <code>atom()</code>
  * default: 
  `emqxcl`
  * mapping: 
  `ekka.cluster_name`

  Human-friendly name of the EMQX cluster.

- discovery_strategy: <code>manual | static | mcast | dns | etcd | k8s</code>
  * default: 
  `manual`

  Service discovery method for the cluster nodes.

- core_nodes: <code>emqx_schema:comma_separated_atoms()</code>
  * default: 
  `[]`
  * mapping: 
  `mria.core_nodes`


  List of core nodes that the replicant will connect to.<br/>
  Note: this parameter only takes effect when the <code>backend</code> is set
  to <code>rlog</code> and the <code>role</code> is set to <code>replicant</code>.<br/>
  This value needs to be defined for manual or static cluster discovery mechanisms.<br/>
  If an automatic cluster discovery mechanism is being used (such as <code>etcd</code>),
  there is no need to set this value.


- autoclean: <code>emqx_schema:duration()</code>
  * default: 
  `"5m"`
  * mapping: 
  `ekka.cluster_autoclean`

  Remove disconnected nodes from the cluster after this interval.

- autoheal: <code>boolean()</code>
  * default: 
  `true`
  * mapping: 
  `ekka.cluster_autoheal`

  If <code>true</code>, the node will try to heal network partitions automatically.

- proto_dist: <code>inet_tcp | inet6_tcp | inet_tls</code>
  * default: 
  `inet_tcp`
  * mapping: 
  `ekka.proto_dist`

  The Erlang distribution protocol for the cluster.

- static: <code>[cluster_static](#cluster_static)</code>



- mcast: <code>[cluster_mcast](#cluster_mcast)</code>



- dns: <code>[cluster_dns](#cluster_dns)</code>



- etcd: <code>[cluster_etcd](#cluster_etcd)</code>



- k8s: <code>[cluster_k8s](#cluster_k8s)</code>




## cluster_call
Options for the 'cluster call' feature that allows to execute a callback on all nodes in the cluster.


**Config paths**

 - <code>node.cluster_call</code>


**Env overrides**

 - <code>EMQX_NODE__CLUSTER_CALL</code>



**Fields**

- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"1m"`

  Time interval to retry after a failed call.

- max_history: <code>1..500</code>
  * default: 
  `100`

  Retain the maximum number of completed transactions (for queries).

- cleanup_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"5m"`

  Time interval to clear completed but stale transactions.
  Ensure that the number of completed transactions is less than the <code>max_history</code>.


## console_handler
Log handler that prints log events to the EMQX console.


**Config paths**

 - <code>log.console_handler</code>


**Env overrides**

 - <code>EMQX_LOG__CONSOLE_HANDLER</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable this log handler.

- level: <code>emqx_conf_schema:log_level()</code>
  * default: 
  `warning`


  The log level for the current log handler.
  Defaults to warning.


- time_offset: <code>string()</code>
  * default: 
  `"system"`


  The time offset to be used when formatting the timestamp.
  Can be one of:
    - <code>system</code>: the time offset used by the local system
    - <code>utc</code>: the UTC time offset
    - <code>+-[hh]:[mm]</code>: user specified time offset, such as "-02:00" or "+00:00"
  Defaults to: <code>system</code>.


- chars_limit: <code>unlimited | 100..inf</code>
  * default: 
  `unlimited`


  Set the maximum length of a single log message. If this length is exceeded, the log message will be truncated.
  NOTE: Restrict char limiter if formatter is JSON , it will get a truncated incomplete JSON data, which is not recommended.


- formatter: <code>text | json</code>
  * default: 
  `text`

  Choose log formatter. <code>text</code> for free text, and <code>json</code> for structured logging.

- single_line: <code>boolean()</code>
  * default: 
  `true`

  Print logs in a single line if set to true. Otherwise, log messages may span multiple lines.

- sync_mode_qlen: <code>non_neg_integer()</code>
  * default: 
  `100`

  As long as the number of buffered log events is lower than this value,
  all log events are handled asynchronously. This means that the client process sending the log event,
  by calling a log function in the Logger API, does not wait for a response from the handler
  but continues executing immediately after the event is sent.
  It is not affected by the time it takes the handler to print the event to the log device.
  If the message queue grows larger than this value,
  the handler starts handling log events synchronously instead,
  meaning that the client process sending the event must wait for a response.
  When the handler reduces the message queue to a level below the sync_mode_qlen threshold,
  asynchronous operation is resumed.


- drop_mode_qlen: <code>pos_integer()</code>
  * default: 
  `3000`

  When the number of buffered log events is larger than this value, the new log events are dropped.
  When drop mode is activated or deactivated, a message is printed in the logs.

- flush_qlen: <code>pos_integer()</code>
  * default: 
  `8000`

  If the number of buffered log events grows larger than this threshold, a flush (delete) operation takes place.
  To flush events, the handler discards the buffered log messages without logging.

- overload_kill: <code>[log_overload_kill](#log_overload_kill)</code>



- burst_limit: <code>[log_burst_limit](#log_burst_limit)</code>



- supervisor_reports: <code>error | progress</code>
  * default: 
  `error`


  Type of supervisor reports that are logged. Defaults to <code>error</code>
    - <code>error</code>: only log errors in the Erlang processes.
    - <code>progress</code>: log process startup.


- max_depth: <code>unlimited | non_neg_integer()</code>
  * default: 
  `100`

  Maximum depth for Erlang term log formatting and Erlang process message queue inspection.


## log
EMQX logging supports multiple sinks for the log events.
Each sink is represented by a _log handler_, which can be configured independently.


**Config paths**

 - <code>log</code>


**Env overrides**

 - <code>EMQX_LOG</code>



**Fields**

- console_handler: <code>[console_handler](#console_handler)</code>



- file_handlers: <code>{$name -> [log_file_handler](#log_file_handler)}</code>

  File-based log handlers.


## log_burst_limit
Large bursts of log events produced in a short time can potentially cause problems, such as:
 - Log files grow very large
 - Log files are rotated too quickly, and useful information gets overwritten
 - Overall performance impact on the system

Log burst limit feature can temporarily disable logging to avoid these issues.


**Config paths**

 - <code>log.console_handler.burst_limit</code>
 - <code>log.file_handlers.$name.burst_limit</code>


**Env overrides**

 - <code>EMQX_LOG__CONSOLE_HANDLER__BURST_LIMIT</code>
 - <code>EMQX_LOG__FILE_HANDLERS__$NAME__BURST_LIMIT</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable log burst control feature.

- max_count: <code>pos_integer()</code>
  * default: 
  `10000`

  Maximum number of log events to handle within a `window_time` interval. After the limit is reached, successive events are dropped until the end of the `window_time`.

- window_time: <code>emqx_schema:duration()</code>
  * default: 
  `"1s"`

  See <code>max_count</code>.


## log_file_handler
Log handler that prints log events to files.


**Config paths**

 - <code>log.file_handlers.$name</code>


**Env overrides**

 - <code>EMQX_LOG__FILE_HANDLERS__$NAME</code>



**Fields**

- file: <code>emqx_conf_schema:file()</code>

  Name the log file.

- rotation: <code>[log_rotation](#log_rotation)</code>



- max_size: <code>infinity | emqx_schema:bytesize()</code>
  * default: 
  `"50MB"`

  This parameter controls log file rotation. The value `infinity` means the log file will grow indefinitely, otherwise the log file will be rotated once it reaches `max_size` in bytes.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable this log handler.

- level: <code>emqx_conf_schema:log_level()</code>
  * default: 
  `warning`


  The log level for the current log handler.
  Defaults to warning.


- time_offset: <code>string()</code>
  * default: 
  `"system"`


  The time offset to be used when formatting the timestamp.
  Can be one of:
    - <code>system</code>: the time offset used by the local system
    - <code>utc</code>: the UTC time offset
    - <code>+-[hh]:[mm]</code>: user specified time offset, such as "-02:00" or "+00:00"
  Defaults to: <code>system</code>.


- chars_limit: <code>unlimited | 100..inf</code>
  * default: 
  `unlimited`


  Set the maximum length of a single log message. If this length is exceeded, the log message will be truncated.
  NOTE: Restrict char limiter if formatter is JSON , it will get a truncated incomplete JSON data, which is not recommended.


- formatter: <code>text | json</code>
  * default: 
  `text`

  Choose log formatter. <code>text</code> for free text, and <code>json</code> for structured logging.

- single_line: <code>boolean()</code>
  * default: 
  `true`

  Print logs in a single line if set to true. Otherwise, log messages may span multiple lines.

- sync_mode_qlen: <code>non_neg_integer()</code>
  * default: 
  `100`

  As long as the number of buffered log events is lower than this value,
  all log events are handled asynchronously. This means that the client process sending the log event,
  by calling a log function in the Logger API, does not wait for a response from the handler
  but continues executing immediately after the event is sent.
  It is not affected by the time it takes the handler to print the event to the log device.
  If the message queue grows larger than this value,
  the handler starts handling log events synchronously instead,
  meaning that the client process sending the event must wait for a response.
  When the handler reduces the message queue to a level below the sync_mode_qlen threshold,
  asynchronous operation is resumed.


- drop_mode_qlen: <code>pos_integer()</code>
  * default: 
  `3000`

  When the number of buffered log events is larger than this value, the new log events are dropped.
  When drop mode is activated or deactivated, a message is printed in the logs.

- flush_qlen: <code>pos_integer()</code>
  * default: 
  `8000`

  If the number of buffered log events grows larger than this threshold, a flush (delete) operation takes place.
  To flush events, the handler discards the buffered log messages without logging.

- overload_kill: <code>[log_overload_kill](#log_overload_kill)</code>



- burst_limit: <code>[log_burst_limit](#log_burst_limit)</code>



- supervisor_reports: <code>error | progress</code>
  * default: 
  `error`


  Type of supervisor reports that are logged. Defaults to <code>error</code>
    - <code>error</code>: only log errors in the Erlang processes.
    - <code>progress</code>: log process startup.


- max_depth: <code>unlimited | non_neg_integer()</code>
  * default: 
  `100`

  Maximum depth for Erlang term log formatting and Erlang process message queue inspection.


## log_overload_kill

Log overload kill features an overload protection that activates when the log handlers use too much memory or have too many buffered log messages.<br/>
When the overload is detected, the log handler is terminated and restarted after a cooldown period.



**Config paths**

 - <code>log.console_handler.overload_kill</code>
 - <code>log.file_handlers.$name.overload_kill</code>


**Env overrides**

 - <code>EMQX_LOG__CONSOLE_HANDLER__OVERLOAD_KILL</code>
 - <code>EMQX_LOG__FILE_HANDLERS__$NAME__OVERLOAD_KILL</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable log handler overload kill feature.

- mem_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `"30MB"`

  Maximum memory size that the log handler process is allowed to use.

- qlen: <code>pos_integer()</code>
  * default: 
  `20000`

  Maximum allowed queue length.

- restart_after: <code>emqx_schema:duration_ms() | infinity</code>
  * default: 
  `"5s"`

  If the handler is terminated, it restarts automatically after a delay specified in milliseconds. The value `infinity` prevents restarts.


## log_rotation

By default, the logs are stored in `./log` directory (for installation from zip file) or in `/var/log/emqx` (for binary installation).<br/>
This section of the configuration controls the number of files kept for each log handler.



**Config paths**

 - <code>log.file_handlers.$name.rotation</code>


**Env overrides**

 - <code>EMQX_LOG__FILE_HANDLERS__$NAME__ROTATION</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable log rotation feature.

- count: <code>1..2048</code>
  * default: 
  `10`

  Maximum number of log files.


## node
Node name, cookie, config & data directories and the Erlang virtual machine (BEAM) boot parameters.


**Config paths**

 - <code>node</code>


**Env overrides**

 - <code>EMQX_NODE</code>



**Fields**

- name: <code>string()</code>
  * default: 
  `"emqx@127.0.0.1"`

  Unique name of the EMQX node. It must follow <code>%name%@FQDN</code> or
  <code>%name%@IPv4</code> format.
            

- cookie: <code>string()</code>
  * mapping: 
  `vm_args.-setcookie`

  Secret cookie is a random string that should be the same on all nodes in
  the given EMQX cluster, but unique per EMQX cluster. It is used to prevent EMQX nodes that
  belong to different clusters from accidentally connecting to each other.

- process_limit: <code>1024..134217727</code>
  * default: 
  `2097152`
  * mapping: 
  `vm_args.+P`

  Maximum number of simultaneously existing processes for this Erlang system.
  The actual maximum chosen may be much larger than the Number passed.
  For more information, see: https://www.erlang.org/doc/man/erl.html
            

- max_ports: <code>1024..134217727</code>
  * default: 
  `1048576`
  * mapping: 
  `vm_args.+Q`

  Maximum number of simultaneously existing ports for this Erlang system.
  The actual maximum chosen may be much larger than the Number passed.
  For more information, see: https://www.erlang.org/doc/man/erl.html
            

- dist_buffer_size: <code>1..2097151</code>
  * default: 
  `8192`
  * mapping: 
  `vm_args.+zdbbl`

  Erlang's distribution buffer busy limit in kilobytes.

- max_ets_tables: <code>pos_integer()</code>
  * default: 
  `262144`
  * mapping: 
  `vm_args.+e`

  Max number of ETS tables

- data_dir: <code>string()</code>
  * mapping: 
  `emqx.data_dir`


  Path to the persistent data directory.<br/>
  Possible auto-created subdirectories are:<br/>
  - `mnesia/<node_name>`: EMQX's built-in database directory.<br/>
  For example, `mnesia/emqx@127.0.0.1`.<br/>
  There should be only one such subdirectory.<br/>
  Meaning, in case the node is to be renamed (to e.g. `emqx@10.0.1.1`),<br/>
  the old dir should be deleted first.<br/>
  - `configs`: Generated configs at boot time, and cluster/local override configs.<br/>
  - `patches`: Hot-patch beam files are to be placed here.<br/>
  - `trace`: Trace log files.<br/>

  **NOTE**: One data dir cannot be shared by two or more EMQX nodes.


- config_files: <code>[string()]</code>
  * mapping: 
  `emqx.config_files`

  List of configuration files that are read during startup. The order is
  significant: later configuration files override the previous ones.
            

- global_gc_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `"15m"`
  * mapping: 
  `emqx_machine.global_gc_interval`

  Periodic garbage collection interval. Set to <code>disabled</code> to have it disabled.

- crash_dump_file: <code>emqx_conf_schema:file()</code>
  * default: 
  `"log/erl_crash.dump"`
  * mapping: 
  `vm_args.-env ERL_CRASH_DUMP`

  Location of the crash dump file.

- crash_dump_seconds: <code>emqx_schema:duration_s()</code>
  * default: 
  `"30s"`
  * mapping: 
  `vm_args.-env ERL_CRASH_DUMP_SECONDS`

  The number of seconds that the broker is allowed to spend writing a crash dump.

- crash_dump_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `"100MB"`
  * mapping: 
  `vm_args.-env ERL_CRASH_DUMP_BYTES`

  The maximum size of a crash dump file in bytes.

- dist_net_ticktime: <code>emqx_schema:duration_s()</code>
  * default: 
  `"2m"`
  * mapping: 
  `vm_args.-kernel net_ticktime`

  This is the approximate time an EMQX node may be unresponsive until it is considered down and thereby disconnected.

- backtrace_depth: <code>integer()</code>
  * default: 
  `23`
  * mapping: 
  `emqx_machine.backtrace_depth`

  Maximum depth of the call stack printed in error messages and
  <code>process_info</code>.
            

- applications: <code>emqx_schema:comma_separated_atoms()</code>
  * default: 
  `[]`
  * mapping: 
  `emqx_machine.applications`

  List of Erlang applications that shall be rebooted when the EMQX broker joins the cluster.
            

- etc_dir: <code>string()</code>

  Deprecated since 5.0.8.

- cluster_call: <code>[cluster_call](#cluster_call)</code>



- db_backend: <code>mnesia | rlog</code>
  * default: 
  `rlog`
  * mapping: 
  `mria.db_backend`


  Select the backend for the embedded database.<br/>
  <code>rlog</code> is the default backend,
  that is suitable for very large clusters.<br/>
  <code>mnesia</code> is a backend that offers decent performance in small clusters.


- db_role: <code>core | replicant</code>
  * default: 
  `core`
  * mapping: 
  `mria.node_role`


  Select a node role.<br/>
  <code>core</code> nodes provide durability of the data, and take care of writes.
  It is recommended to place core nodes in different racks or different availability zones.<br/>
  <code>replicant</code> nodes are ephemeral worker nodes. Removing them from the cluster
  doesn't affect database redundancy<br/>
  It is recommended to have more replicant nodes than core nodes.<br/>
  Note: this parameter only takes effect when the <code>backend</code> is set
  to <code>rlog</code>.


- rpc_module: <code>gen_rpc | rpc</code>
  * default: 
  `gen_rpc`
  * mapping: 
  `mria.rlog_rpc_module`

  Protocol used for pushing transaction logs to the replicant nodes.

- tlog_push_mode: <code>sync | async</code>
  * default: 
  `async`
  * mapping: 
  `mria.tlog_push_mode`


  In sync mode the core node waits for an ack from the replicant nodes before sending the next
  transaction log entry.



## rpc
EMQX uses a library called <code>gen_rpc</code> for inter-broker communication.<br/>
Most of the time the default config should work,
but in case you need to do performance fine-tuning or experiment a bit,
this is where to look.


**Config paths**

 - <code>rpc</code>


**Env overrides**

 - <code>EMQX_RPC</code>



**Fields**

- mode: <code>sync | async</code>
  * default: 
  `async`

  In <code>sync</code> mode the sending side waits for the ack from the receiving side.

- driver: <code>tcp | ssl</code>
  * default: 
  `tcp`
  * mapping: 
  `gen_rpc.driver`

  Transport protocol used for inter-broker communication

- async_batch_size: <code>integer()</code>
  * default: 
  `256`
  * mapping: 
  `gen_rpc.max_batch_size`

  The maximum number of batch messages sent in asynchronous mode.
        Note that this configuration does not work in synchronous mode.
        

- port_discovery: <code>manual | stateless</code>
  * default: 
  `stateless`
  * mapping: 
  `gen_rpc.port_discovery`

  <code>manual</code>: discover ports by <code>tcp_server_port</code>.<br/>
  <code>stateless</code>: discover ports in a stateless manner, using the following algorithm.
  If node name is <code>emqxN@127.0.0.1</code>, where the N is an integer,
  then the listening port will be 5370 + N.

- tcp_server_port: <code>integer()</code>
  * default: 
  `5369`
  * mapping: 
  `gen_rpc.tcp_server_port`

  Listening port used by RPC local service.<br/>
  Note that this config only takes effect when rpc.port_discovery is set to manual.

- ssl_server_port: <code>integer()</code>
  * default: 
  `5369`
  * mapping: 
  `gen_rpc.ssl_server_port`

  Listening port used by RPC local service.<br/>
  Note that this config only takes effect when rpc.port_discovery is set to manual
  and <code>driver</code> is set to <code>ssl</code>.

- tcp_client_num: <code>1..256</code>
  * default: 
  `10`

  Set the maximum number of RPC communication channels initiated by this node to each remote node.

- connect_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`
  * mapping: 
  `gen_rpc.connect_timeout`

  Timeout for establishing an RPC connection.

- certfile: <code>emqx_conf_schema:file()</code>
  * mapping: 
  `gen_rpc.certfile`

  Path to TLS certificate file used to validate identity of the cluster nodes.
  Note that this config only takes effect when <code>rpc.driver</code> is set to <code>ssl</code>.
        

- keyfile: <code>emqx_conf_schema:file()</code>
  * mapping: 
  `gen_rpc.keyfile`

  Path to the private key file for the <code>rpc.certfile</code>.<br/>
  Note: contents of this file are secret, so it's necessary to set permissions to 600.

- cacertfile: <code>emqx_conf_schema:file()</code>
  * mapping: 
  `gen_rpc.cacertfile`

  Path to certification authority TLS certificate file used to validate <code>rpc.certfile</code>.<br/>
  Note: certificates of all nodes in the cluster must be signed by the same CA.

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`
  * mapping: 
  `gen_rpc.send_timeout`

  Timeout for sending the RPC request.

- authentication_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`
  * mapping: 
  `gen_rpc.authentication_timeout`

  Timeout for the remote node authentication.

- call_receive_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `"15s"`
  * mapping: 
  `gen_rpc.call_receive_timeout`

  Timeout for the reply to a synchronous RPC.

- socket_keepalive_idle: <code>emqx_schema:duration_s()</code>
  * default: 
  `"15m"`
  * mapping: 
  `gen_rpc.socket_keepalive_idle`

  How long the connections between the brokers should remain open after the last message is sent.

- socket_keepalive_interval: <code>emqx_schema:duration_s()</code>
  * default: 
  `"75s"`
  * mapping: 
  `gen_rpc.socket_keepalive_interval`

  The interval between keepalive messages.

- socket_keepalive_count: <code>integer()</code>
  * default: 
  `9`
  * mapping: 
  `gen_rpc.socket_keepalive_count`

  How many times the keepalive probe message can fail to receive a reply
  until the RPC connection is considered lost.

- socket_sndbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`
  * mapping: 
  `gen_rpc.socket_sndbuf`

  TCP tuning parameters. TCP sending buffer size.

- socket_recbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`
  * mapping: 
  `gen_rpc.socket_recbuf`

  TCP tuning parameters. TCP receiving buffer size.

- socket_buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`
  * mapping: 
  `gen_rpc.socket_buffer`

  TCP tuning parameters. Socket buffer size in user mode.

- insecure_fallback: <code>boolean()</code>
  * default: 
  `true`
  * mapping: 
  `gen_rpc.insecure_auth_fallback_allowed`

  Enable compatibility with old RPC authentication.


## topology
Topology of MongoDB.


**Config paths**

 - <code>authentication.$INDEX.topology</code>
 - <code>authorization.sources.$INDEX.topology</code>
 - <code>gateway.coap.authentication.topology</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.topology</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.topology</code>
 - <code>gateway.exproto.authentication.topology</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.topology</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.topology</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.topology</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.topology</code>
 - <code>gateway.lwm2m.authentication.topology</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.topology</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.topology</code>
 - <code>gateway.mqttsn.authentication.topology</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.topology</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.topology</code>
 - <code>gateway.stomp.authentication.topology</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.topology</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.topology</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.topology</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.topology</code>
 - <code>listeners.ws.$name.authentication.$INDEX.topology</code>
 - <code>listeners.wss.$name.authentication.$INDEX.topology</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__TOPOLOGY</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__TOPOLOGY</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__TOPOLOGY</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__TOPOLOGY</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__TOPOLOGY</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__TOPOLOGY</code>



**Fields**

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- max_overflow: <code>non_neg_integer()</code>
  * default: 
  `0`

  Max Overflow.

- overflow_ttl: <code>emqx_schema:duration_ms()</code>

  Time interval, such as timeout or TTL.

- overflow_check_period: <code>emqx_schema:duration_ms()</code>

  Time interval, such as timeout or TTL.

- local_threshold_ms: <code>emqx_schema:duration_ms()</code>

  Time interval, such as timeout or TTL.

- connect_timeout_ms: <code>emqx_schema:duration_ms()</code>

  Time interval, such as timeout or TTL.

- socket_timeout_ms: <code>emqx_schema:duration_ms()</code>

  Time interval, such as timeout or TTL.

- server_selection_timeout_ms: <code>emqx_schema:duration_ms()</code>

  Time interval, such as timeout or TTL.

- wait_queue_timeout_ms: <code>emqx_schema:duration_ms()</code>

  Time interval, such as timeout or TTL.

- heartbeat_frequency_ms: <code>emqx_schema:duration_ms()</code>

  Time interval, such as timeout or TTL.

- min_heartbeat_frequency_ms: <code>emqx_schema:duration_ms()</code>

  Time interval, such as timeout or TTL.


## zone:conn_congestion
Settings for `conn_congestion` alarm.

Sometimes the MQTT connection (usually an MQTT subscriber) may
get "congested", because there are too many packets to be sent.
The socket tries to buffer the packets until the buffer is
full. If more packets arrive after that, the packets will be
"pending" in the queue, and we consider the connection
congested.

Note: `sndbuf` can be set to larger value if the
alarm is triggered too often.
The name of the alarm is of format `conn_congestion/<ClientID>/<Username>`,
where the `<ClientID>` is the client ID of the congested MQTT connection,
and `<Username>` is the username or `unknown_user`.


**Config paths**

 - <code>zones.$name.conn_congestion</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__CONN_CONGESTION</code>



**Fields**

- enable_alarm: <code>boolean()</code>

  Enable or disable connection congestion alarm.

- min_alarm_sustain_duration: <code>emqx_schema:duration()</code>

  Minimal time before clearing the alarm.<br/>The alarm is cleared only when there's no pending data in<br/>the queue, and at least <code>min_alarm_sustain_duration</code>milliseconds passed since the last time we considered the connection 'congested'.<br/>This is to avoid clearing and raising the alarm again too often.


## zone:flapping_detect
This config controls the allowed maximum number of `CONNECT` packets received
from the same clientid in a time frame defined by `window_time`.
After the limit is reached, successive `CONNECT` requests are forbidden
(banned) until the end of the time period defined by `ban_time`.


**Config paths**

 - <code>zones.$name.flapping_detect</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__FLAPPING_DETECT</code>



**Fields**

- enable: <code>boolean()</code>

  Enable flapping connection detection feature.

- max_count: <code>integer()</code>

  The maximum number of disconnects allowed for a MQTT Client in `window_time`

- window_time: <code>emqx_schema:duration()</code>

  The time window for flapping detection.

- ban_time: <code>emqx_schema:duration()</code>

  How long the flapping clientid will be banned.


## zone:force_gc
Force garbage collection in MQTT connection process after
 they process certain number of messages or bytes of data.


**Config paths**

 - <code>zones.$name.force_gc</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__FORCE_GC</code>



**Fields**

- enable: <code>boolean()</code>

  Enable forced garbage collection.

- count: <code>0..inf</code>

  GC the process after this many received messages.

- bytes: <code>emqx_schema:bytesize()</code>

  GC the process after specified number of bytes have passed through.


## zone:force_shutdown
When the process message queue length, or the memory bytes
reaches a certain value, the process is forced to close.

Note: "message queue" here refers to the "message mailbox"
of the Erlang process, not the `mqueue` of QoS 1 and QoS 2.


**Config paths**

 - <code>zones.$name.force_shutdown</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__FORCE_SHUTDOWN</code>



**Fields**

- enable: <code>boolean()</code>

  Enable `force_shutdown` feature.

- max_message_queue_len: <code>0..inf</code>

  Maximum message queue length.

- max_heap_size: <code>emqx_schema:wordsize()</code>

  Total heap size


## zone:mqtt
Global MQTT configuration.<br/>The configs here work as default values which can be overridden
in <code>zone</code> configs


**Config paths**

 - <code>zones.$name.mqtt</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__MQTT</code>



**Fields**

- idle_timeout: <code>infinity | emqx_schema:duration()</code>

  After the TCP connection is established, if the MQTT CONNECT packet from the client is not received within the time specified by <code>idle_timeout</code>, the connection will be disconnected.

- max_packet_size: <code>emqx_schema:bytesize()</code>

  Maximum MQTT packet size allowed.

- max_clientid_len: <code>23..65535</code>

  Maximum allowed length of MQTT Client ID.

- max_topic_levels: <code>1..65535</code>

  Maximum topic levels allowed.

- max_qos_allowed: <code>qos()</code>

  Maximum QoS allowed.

- max_topic_alias: <code>0..65535</code>

  Maximum topic alias, 0 means no topic alias supported.

- retain_available: <code>boolean()</code>

  Whether to enable support for MQTT retained message.

- wildcard_subscription: <code>boolean()</code>

  Whether to enable support for MQTT wildcard subscription.

- shared_subscription: <code>boolean()</code>

  Whether to enable support for MQTT shared subscription.

- exclusive_subscription: <code>boolean()</code>

  Whether to enable support for MQTT exclusive subscription.

- ignore_loop_deliver: <code>boolean()</code>

  Ignore loop delivery of messages for MQTT v3.1.1/v3.1.0, similar to <code>No Local</code> subscription option in MQTT 5.0.

- strict_mode: <code>boolean()</code>

  Parse MQTT messages in strict mode.
  When set to true, invalid utf8 strings in for example client ID, topic name, etc. will cause the client to be disconnected

- response_information: <code>string()</code>

  Specify the response information returned to the client. This feature is disabled if is set to "". Applies only to clients using MQTT 5.0.

- server_keepalive: <code>integer() | disabled</code>

  The keep alive that EMQX requires the client to use. If configured as <code>disabled</code>, it means that the keep alive specified by the client will be used. Requires <code>Server Keep Alive</code> in MQTT 5.0, so it is only applicable to clients using MQTT 5.0 protocol.

- keepalive_backoff: <code>number()</code>

  The backoff multiplier used by the broker to determine the client keep alive timeout. If EMQX doesn't receive any packet in <code>Keep Alive * Backoff * 2</code> seconds, EMQX will close the current connection.

- max_subscriptions: <code>1..inf | infinity</code>

  Maximum number of subscriptions allowed per client.

- upgrade_qos: <code>boolean()</code>

  Force upgrade of QoS level according to subscription.

- max_inflight: <code>1..65535</code>

  Maximum number of QoS 1 and QoS 2 messages that are allowed to be delivered simultaneously before completing the acknowledgment.

- retry_interval: <code>emqx_schema:duration()</code>

  Retry interval for QoS 1/2 message delivering.

- max_awaiting_rel: <code>integer() | infinity</code>

  For each publisher session, the maximum number of outstanding QoS 2 messages pending on the client to send PUBREL. After reaching this limit, new QoS 2 PUBLISH requests will be rejected with `147(0x93)` until either PUBREL is received or timed out.

- await_rel_timeout: <code>emqx_schema:duration()</code>

  For client to broker QoS 2 message, the time limit for the broker to wait before the `PUBREL` message is received. The wait is aborted after timed out, meaning the packet ID is freed for new `PUBLISH` requests. Receiving a stale `PUBREL` causes a warning level log. Note, the message is delivered to subscribers before entering the wait for PUBREL.

- session_expiry_interval: <code>emqx_schema:duration()</code>

  Specifies how long the session will expire after the connection is disconnected, only for non-MQTT 5.0 connections.

- max_mqueue_len: <code>non_neg_integer() | infinity</code>

  Maximum queue length. Enqueued messages when persistent client disconnected, or inflight window is full.

- mqueue_priorities: <code>map() | disabled</code>

  Topic priorities. Priority number [1-255]
  There's no priority table by default, hence all messages are treated equal.

  **NOTE**: Comma and equal signs are not allowed for priority topic names.
  **NOTE**: Messages for topics not in the priority table are treated as either highest or lowest priority depending on the configured value for <code>mqtt.mqueue_default_priority</code>.

  **Examples**:
  To configure <code>"topic/1" > "topic/2"</code>:
  <code>mqueue_priorities: {"topic/1": 10, "topic/2": 8}</code>


- mqueue_default_priority: <code>highest | lowest</code>

  Default topic priority, which will be used by topics not in <code>Topic Priorities</code> (<code>mqueue_priorities</code>).

- mqueue_store_qos0: <code>boolean()</code>

  Specifies whether to store QoS 0 messages in the message queue while the connection is down but the session remains.

- use_username_as_clientid: <code>boolean()</code>

  Whether to user Client ID as Username.
  This setting takes effect later than <code>Use Peer Certificate as Username</code> (<code>peer_cert_as_username</code>) and <code>Use peer certificate as Client ID</code> (<code>peer_cert_as_clientid</code>).


- peer_cert_as_username: <code>disabled | cn | dn | crt | pem | md5</code>

  Use the CN, DN field in the peer certificate or the entire certificate content as Username. Only works for the TLS connection.
  Supported configurations are the following:
  - <code>cn</code>: Take the CN field of the certificate as Username
  - <code>dn</code>: Take the DN field of the certificate as Username
  - <code>crt</code>: Take the content of the <code>DER</code> or <code>PEM</code> certificate as Username
  - <code>pem</code>: Convert <code>DER</code> certificate content to <code>PEM</code> format as Username
  - <code>md5</code>: Take the MD5 value of the content of the <code>DER</code> or <code>PEM</code> certificate as Username


- peer_cert_as_clientid: <code>disabled | cn | dn | crt | pem | md5</code>

  Use the CN, DN field in the peer certificate or the entire certificate content as Client ID. Only works for the TLS connection.
  Supported configurations are the following:
  - <code>cn</code>: Take the CN field of the certificate as Client ID
  - <code>dn</code>: Take the DN field of the certificate as Client ID
  - <code>crt</code>: Take the content of the <code>DER</code> or <code>PEM</code> certificate as Client ID
  - <code>pem</code>: Convert <code>DER</code> certificate content to <code>PEM</code> format as Client ID
  - <code>md5</code>: Take the MD5 value of the content of the <code>DER</code> or <code>PEM</code> certificate as Client ID



## zone:overload_protection
Overload protection mechanism monitors the load of the system and temporarily
disables some features (such as accepting new connections) when the load is high.


**Config paths**

 - <code>zones.$name.overload_protection</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__OVERLOAD_PROTECTION</code>



**Fields**

- enable: <code>boolean()</code>

  React on system overload or not.

- backoff_delay: <code>0..inf</code>

  When at high load, some unimportant tasks could be delayed for execution, here set the duration in milliseconds precision.

- backoff_gc: <code>boolean()</code>

  When at high load, skip forceful GC.

- backoff_hibernation: <code>boolean()</code>

  When at high load, skip process hibernation.

- backoff_new_conn: <code>boolean()</code>

  When at high load, close new incoming connections.


## zone:stats
Enable/disable statistic data collection.
Statistic data such as message receive/send count/rate etc. It provides insights of system performance and helps to diagnose issues. You can find statistic data from the dashboard, or from the '/stats' API.


**Config paths**

 - <code>zones.$name.stats</code>


**Env overrides**

 - <code>EMQX_ZONES__$NAME__STATS</code>



**Fields**

- enable: <code>boolean()</code>

  Enable/disable statistic data collection.


## authn-builtin_db:authentication
Configuration of authenticator using built-in database as data source.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>built_in_database</code>

  Backend type.

- user_id_type: <code>clientid | username</code>
  * default: 
  `"username"`

  Specify whether to use `clientid` or `username` for authentication.

- password_hash_algorithm: <code>[authn-hash:bcrypt_rw](#authn-hash-bcrypt_rw) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash creation and verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


## authn-hash:bcrypt
Settings for bcrypt password hashing algorithm.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>
 - <code>gateway.coap.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.ws.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.wss.$name.authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>bcrypt</code>

  BCRYPT password hashing.


## authn-hash:bcrypt_rw
Settings for bcrypt password hashing algorithm (for DB backends with write capability).


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>
 - <code>gateway.coap.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.ws.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.wss.$name.authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>bcrypt</code>

  BCRYPT password hashing.

- salt_rounds: <code>integer()</code>
  * default: 
  `10`

  Salt rounds for BCRYPT password generation.


## authn-hash:other_algorithms
Settings for other password hashing algorithms.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>
 - <code>gateway.coap.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.ws.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.wss.$name.authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>plain | md5 | sha | sha256 | sha512</code>

  Simple password hashing algorithm.

- salt_position: <code>disable | prefix | suffix</code>
  * default: 
  `prefix`

  Salt position for PLAIN, MD5, SHA, SHA256 and SHA512 algorithms.


## authn-hash:pbkdf2
Settings for PBKDF2 password hashing algorithm.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>
 - <code>gateway.coap.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.password_hash_algorithm</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.password_hash_algorithm</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.ws.$name.authentication.$INDEX.password_hash_algorithm</code>
 - <code>listeners.wss.$name.authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>pbkdf2</code>

  PBKDF2 password hashing.

- mac_fun: <code>md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512</code>

  Specifies mac_fun for PBKDF2 hashing algorithm.

- iterations: <code>integer()</code>

  Iteration count for PBKDF2 hashing algorithm.

- dk_length: <code>integer()</code>

  Derived length for PBKDF2 hashing algorithm. If not specified, calculated automatically based on `mac_fun`.


## authn-http:get
Configuration of authenticator using HTTP Server as authentication service (Using GET request).


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- method: <code>get</code>
  * default: 
  `get`

  HTTP request method.

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    "cache-control" = "no-cache"
    connection = "keep-alive"
    "keep-alive" = "timeout=30, max=1000"
  }
  ```

  List of HTTP headers (without <code>content-type</code>).

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>http</code>

  Backend type.

- url: <code>binary()</code>

  URL of the HTTP server.

- body: <code>#{term() => binary()}</code>

  HTTP request body.

- request_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"5s"`

  HTTP request timeout.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- connect_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  The timeout when connecting to the HTTP server.

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  The pool size.

- request: <code>[connector-http:request](#connector-http-request)</code>


  If the request is provided, the caller can send HTTP requests via
  <code>emqx_resource:query(ResourceId, {send_message, BridgeId, Message})</code>


- retry_interval: <code>emqx_schema:duration()</code>

  Deprecated since 5.0.4.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn-http:post
Configuration of authenticator using HTTP Server as authentication service (Using POST request).


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- method: <code>post</code>
  * default: 
  `post`

  HTTP request method.

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    "cache-control" = "no-cache"
    connection = "keep-alive"
    "content-type" = "application/json"
    "keep-alive" = "timeout=30, max=1000"
  }
  ```

  List of HTTP Headers.

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>http</code>

  Backend type.

- url: <code>binary()</code>

  URL of the HTTP server.

- body: <code>#{term() => binary()}</code>

  HTTP request body.

- request_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"5s"`

  HTTP request timeout.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- connect_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  The timeout when connecting to the HTTP server.

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  The pool size.

- request: <code>[connector-http:request](#connector-http-request)</code>


  If the request is provided, the caller can send HTTP requests via
  <code>emqx_resource:query(ResourceId, {send_message, BridgeId, Message})</code>


- retry_interval: <code>emqx_schema:duration()</code>

  Deprecated since 5.0.4.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn-jwt:hmac-based
Configuration when the JWT for authentication is issued using the HMAC algorithm.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- use_jwks: <code>false</code>

  Whether to use JWKS.

- algorithm: <code>hmac-based</code>

  JWT signing algorithm, Supports HMAC (configured as <code>hmac-based</code>) and RSA, ECDSA (configured as <code>public-key</code>).

- secret: <code>binary()</code>

  The key to verify the JWT using HMAC algorithm.

- secret_base64_encoded: <code>boolean()</code>
  * default: 
  `false`

  Whether secret is base64 encoded.

- mechanism: <code>jwt</code>

  Authentication mechanism.

- acl_claim_name: <code>binary()</code>
  * default: 
  `"acl"`

  JWT claim name to use for getting ACL rules.

- verify_claims: <code>[term()]</code>
  * default: 
  `{}`


  A list of custom claims to validate, which is a list of name/value pairs.
  Values can use the following placeholders:
  - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
  - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting
  Authentication will verify that the value of claims in the JWT (taken from the Password field) matches what is required in <code>verify_claims</code>.


- from: <code>username | password</code>
  * default: 
  `password`

  Field to take JWT from.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


## authn-jwt:jwks
Configuration when JWTs used for authentication need to be fetched from the JWKS endpoint.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- use_jwks: <code>true</code>

  Whether to use JWKS.

- endpoint: <code>string()</code>

  JWKS endpoint, it's a read-only endpoint that returns the server's public key set in the JWKS format.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- refresh_interval: <code>integer()</code>
  * default: 
  `300`

  JWKS refresh interval.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL options.

- mechanism: <code>jwt</code>

  Authentication mechanism.

- acl_claim_name: <code>binary()</code>
  * default: 
  `"acl"`

  JWT claim name to use for getting ACL rules.

- verify_claims: <code>[term()]</code>
  * default: 
  `{}`


  A list of custom claims to validate, which is a list of name/value pairs.
  Values can use the following placeholders:
  - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
  - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting
  Authentication will verify that the value of claims in the JWT (taken from the Password field) matches what is required in <code>verify_claims</code>.


- from: <code>username | password</code>
  * default: 
  `password`

  Field to take JWT from.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


## authn-jwt:public-key
Configuration when the JWT for authentication is issued using RSA or ECDSA algorithm.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- use_jwks: <code>false</code>

  Whether to use JWKS.

- algorithm: <code>public-key</code>

  JWT signing algorithm, Supports HMAC (configured as <code>hmac-based</code>) and RSA, ECDSA (configured as <code>public-key</code>).

- public_key: <code>string()</code>

  The public key used to verify the JWT.

- mechanism: <code>jwt</code>

  Authentication mechanism.

- acl_claim_name: <code>binary()</code>
  * default: 
  `"acl"`

  JWT claim name to use for getting ACL rules.

- verify_claims: <code>[term()]</code>
  * default: 
  `{}`


  A list of custom claims to validate, which is a list of name/value pairs.
  Values can use the following placeholders:
  - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
  - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting
  Authentication will verify that the value of claims in the JWT (taken from the Password field) matches what is required in <code>verify_claims</code>.


- from: <code>username | password</code>
  * default: 
  `password`

  Field to take JWT from.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


## authn-mongodb:replica-set
Configuration of authenticator using MongoDB (Replica Set) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>mongodb</code>

  Backend type.

- collection: <code>binary()</code>

  Collection used to store authentication data.

- filter: <code>map()</code>
  * default: 
  `{}`


  Conditional expression that defines the filter condition in the query.
  Filter supports the following placeholders:
  - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
  - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


- password_hash_field: <code>binary()</code>
  * default: 
  `"password_hash"`

  Document field that contains password hash.

- salt_field: <code>binary()</code>
  * default: 
  `"salt"`

  Document field that contains the password salt.

- is_superuser_field: <code>binary()</code>
  * default: 
  `"is_superuser"`

  Document field that defines if the user has superuser privileges.

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- mongo_type: <code>rs</code>
  * default: 
  `rs`

  Replica set.

- servers: <code>[term()]</code>


  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The MongoDB default port 27017 is used if `[:Port]` is not specified.


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  Write mode.

- r_mode: <code>master | slave_ok</code>
  * default: 
  `master`

  Read mode.

- replica_set_name: <code>binary()</code>

  Name of the replica set.

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  Use DNS SRV record.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- username: <code>binary()</code>

  EMQX's username in the external database.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn-mongodb:sharded-cluster
Configuration of authenticator using MongoDB (Sharded Cluster) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>mongodb</code>

  Backend type.

- collection: <code>binary()</code>

  Collection used to store authentication data.

- filter: <code>map()</code>
  * default: 
  `{}`


  Conditional expression that defines the filter condition in the query.
  Filter supports the following placeholders:
  - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
  - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


- password_hash_field: <code>binary()</code>
  * default: 
  `"password_hash"`

  Document field that contains password hash.

- salt_field: <code>binary()</code>
  * default: 
  `"salt"`

  Document field that contains the password salt.

- is_superuser_field: <code>binary()</code>
  * default: 
  `"is_superuser"`

  Document field that defines if the user has superuser privileges.

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- mongo_type: <code>sharded</code>
  * default: 
  `sharded`

  Sharded cluster.

- servers: <code>[term()]</code>


  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The MongoDB default port 27017 is used if `[:Port]` is not specified.


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  Write mode.

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  Use DNS SRV record.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- username: <code>binary()</code>

  EMQX's username in the external database.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn-mongodb:standalone
Configuration of authenticator using MongoDB (Standalone) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>mongodb</code>

  Backend type.

- collection: <code>binary()</code>

  Collection used to store authentication data.

- filter: <code>map()</code>
  * default: 
  `{}`


  Conditional expression that defines the filter condition in the query.
  Filter supports the following placeholders:
  - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting
  - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting


- password_hash_field: <code>binary()</code>
  * default: 
  `"password_hash"`

  Document field that contains password hash.

- salt_field: <code>binary()</code>
  * default: 
  `"salt"`

  Document field that contains the password salt.

- is_superuser_field: <code>binary()</code>
  * default: 
  `"is_superuser"`

  Document field that defines if the user has superuser privileges.

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- mongo_type: <code>single</code>
  * default: 
  `single`

  Standalone instance.

- server: <code>emqx_schema:host_port()</code>


  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The MongoDB default port 27017 is used if `[:Port]` is not specified.


- w_mode: <code>unsafe | safe</code>
  * default: 
  `unsafe`

  Write mode.

- srv_record: <code>boolean()</code>
  * default: 
  `false`

  Use DNS SRV record.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- username: <code>binary()</code>

  EMQX's username in the external database.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[topology](#topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn-mysql:authentication
Configuration of authenticator using MySQL as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>mysql</code>

  Backend type.

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- query: <code>string()</code>

  SQL used to query data for authentication, such as password hash.

- query_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"5s"`

  Timeout for the SQL query.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- server: <code>emqx_schema:host_port()</code>


  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The MySQL default port 3306 is used if `[:Port]` is not specified.


- database: <code>binary()</code>

  Database name.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- username: <code>binary()</code>

  EMQX's username in the external database.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  Enable automatic reconnect to the database.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn-postgresql:authentication
Configuration of authenticator using PostgreSQL as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>postgresql</code>

  Backend type.

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- query: <code>string()</code>

  SQL used to query data for authentication, such as password hash.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- server: <code>emqx_schema:host_port()</code>


  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The PostgreSQL default port 5432 is used if `[:Port]` is not specified.


- database: <code>binary()</code>

  Database name.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- username: <code>binary()</code>

  EMQX's username in the external database.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  Enable automatic reconnect to the database.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn-psk:psk_authentication
PSK stands for 'Pre-Shared Keys'.
This config to enable TLS-PSK authentication.

Important! Make sure the SSL listener with only <code>tlsv1.2</code> enabled, and also PSK cipher suites
configured, such as <code>RSA-PSK-AES256-GCM-SHA384</code>.

See listener SSL options config for more details.

The IDs and secrets can be provided from a file which is configurable by the <code>init_file</code> field.



**Config paths**

 - <code>psk_authentication</code>


**Env overrides**

 - <code>EMQX_PSK_AUTHENTICATION</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  Whether to enable TLS PSK support

- init_file: <code>binary()</code>

  If init_file is specified, EMQX will import PSKs from the file into the built-in database at startup for use by the runtime.
  The file has to be structured line-by-line, each line must be in the format of <code>PSKIdentity:SharedSecret</code>.
  For example: <code>mydevice1:c2VjcmV0</code>

- separator: <code>binary()</code>
  * default: 
  `":"`

  The separator between <code>PSKIdentity</code> and <code>SharedSecret</code> in the PSK file

- chunk_size: <code>integer()</code>
  * default: 
  `50`

  The size of each chunk used to import to the built-in database from PSK file


## authn-redis:cluster
Configuration of authenticator using Redis (Cluster) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>redis</code>

  Backend type.

- cmd: <code>string()</code>

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- servers: <code>[term()]</code>


  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The MongoDB default port 27017 is used if `[:Port]` is not specified.


- redis_type: <code>cluster</code>
  * default: 
  `cluster`

  Cluster mode

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- password: <code>binary()</code>

  EMQX's password in the external database.

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  Enable automatic reconnect to the database.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn-redis:sentinel
Configuration of authenticator using Redis (Sentinel) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>redis</code>

  Backend type.

- cmd: <code>string()</code>

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- servers: <code>[term()]</code>


  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The MongoDB default port 27017 is used if `[:Port]` is not specified.


- redis_type: <code>sentinel</code>
  * default: 
  `sentinel`

  Sentinel mode

- sentinel: <code>string()</code>

  The cluster name in Redis sentinel mode.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- password: <code>binary()</code>

  EMQX's password in the external database.

- database: <code>integer()</code>
  * default: 
  `0`

  Redis database ID.

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  Enable automatic reconnect to the database.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn-redis:standalone
Configuration of authenticator using Redis (Standalone) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>redis</code>

  Backend type.

- cmd: <code>string()</code>

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.

- password_hash_algorithm: <code>[authn-hash:bcrypt](#authn-hash-bcrypt) | [authn-hash:pbkdf2](#authn-hash-pbkdf2) | [authn-hash:other_algorithms](#authn-hash-other_algorithms)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- server: <code>emqx_schema:host_port()</code>


  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The Redis default port 6379 is used if `[:Port]` is not specified.


- redis_type: <code>single</code>
  * default: 
  `single`

  Single mode

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool.

- password: <code>binary()</code>

  EMQX's password in the external database.

- database: <code>integer()</code>
  * default: 
  `0`

  Redis database ID.

- auto_reconnect: <code>boolean()</code>
  * default: 
  `true`

  Enable automatic reconnect to the database.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn-scram-builtin_db:authentication
Settings for Salted Challenge Response Authentication Mechanism
(SCRAM) authentication.


**Config paths**

 - <code>authentication.$INDEX</code>
 - <code>gateway.coap.authentication</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication</code>
 - <code>gateway.coap.listeners.udp.$name.authentication</code>
 - <code>gateway.exproto.authentication</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication</code>
 - <code>gateway.lwm2m.authentication</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication</code>
 - <code>gateway.mqttsn.authentication</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication</code>
 - <code>gateway.stomp.authentication</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication</code>
 - <code>listeners.ssl.$name.authentication.$INDEX</code>
 - <code>listeners.tcp.$name.authentication.$INDEX</code>
 - <code>listeners.ws.$name.authentication.$INDEX</code>
 - <code>listeners.wss.$name.authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>scram</code>

  Authentication mechanism.

- backend: <code>built_in_database</code>

  Backend type.

- algorithm: <code>sha256 | sha512</code>
  * default: 
  `sha256`

  Hashing algorithm.

- iteration_count: <code>non_neg_integer()</code>
  * default: 
  `4096`

  Iteration count.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


## auto_subscribe
After the device logs in successfully, the subscription is automatically completed for the device through the pre-defined subscription representation. Supports the use of placeholders.


**Config paths**

 - <code>auto_subscribe</code>


**Env overrides**

 - <code>EMQX_AUTO_SUBSCRIBE</code>



**Fields**

- topics: <code>[[auto_subscribe:topic](#auto_subscribe-topic)]</code>
  * default: 
  `[]`

  After the device logs in successfully, the subscription is automatically completed for the device through the pre-defined subscription representation. Supports the use of placeholders.


## auto_subscribe:topic
Topic name, placeholders are supported. For example: client/${clientid}/username/${username}/host/${host}/port/${port}
Required field, and cannot be empty string


**Config paths**

 - <code>auto_subscribe.topics.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTO_SUBSCRIBE__TOPICS__$INDEX</code>



**Fields**

- topic: <code>binary()</code>

  Topic name, placeholders are supported. For example: client/${clientid}/username/${username}/host/${host}/port/${port}
  Required field, and cannot be empty string

- qos: <code>qos()</code>
  * default: 
  `0`

  Default value 0. Quality of service.
  At most once (0)
  At least once (1)
  Exactly once (2)

- rh: <code>0..2</code>
  * default: 
  `0`

  Default value 0. This option is used to specify whether the server forwards the retained message to the client when establishing a subscription.
  Retain Handling is equal to 0, as long as the client successfully subscribes, the server will send the retained message.
  Retain Handling is equal to 1, if the client successfully subscribes and this subscription does not exist previously, the server sends the retained message. After all, sometimes the client re-initiate the subscription just to change the QoS, but it does not mean that it wants to receive the reserved messages again.
  Retain Handling is equal to 2, even if the client successfully subscribes, the server does not send the retained message.

- rap: <code>0..1</code>
  * default: 
  `0`

  Default value 0. This option is used to specify whether the server retains the RETAIN mark when forwarding messages to the client, and this option does not affect the RETAIN mark in the retained message. Therefore, when the option Retain As Publish is set to 0, the client will directly distinguish whether this is a normal forwarded message or a retained message according to the RETAIN mark in the message, instead of judging whether this message is the first received after subscribing(the forwarded message may be sent before the retained message, which depends on the specific implementation of different brokers).

- nl: <code>0..1</code>
  * default: 
  `0`

  Default value 0.
  MQTT v3.1.1： if you subscribe to the topic published by yourself, you will receive all messages that you published.
  MQTT v5: if you set this option as 1 when subscribing, the server will not forward the message you published to you.


## bridge:bridges
Configuration for MQTT bridges.


**Config paths**

 - <code>bridges</code>


**Env overrides**

 - <code>EMQX_BRIDGES</code>



**Fields**

- webhook: <code>{$name -> [bridge_webhook:config](#bridge_webhook-config)}</code>

  WebHook to an HTTP server.

- mqtt: <code>{$name -> [bridge_mqtt:config](#bridge_mqtt-config)}</code>

  MQTT bridges to/from another MQTT broker


## bridge_mqtt:creation_opts
Creation options.


**Config paths**

 - <code>bridges.mqtt.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>pos_integer()</code>
  * default: 
  `16`

  Resource worker pool size.

- health_check_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  Health check interval, in milliseconds.

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>
  * default: 
  `"60s"`

  The auto restart interval after the resource is disconnected, in milliseconds.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  Query mode. Optional 'sync/async', default 'sync'.

- async_inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  Async query inflight window.

- enable_queue: <code>boolean()</code>
  * default: 
  `false`

  Queue mode enabled.

- max_queue_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `"100MB"`

  Maximum queue storage.


## bridge_webhook:creation_opts
Creation options.


**Config paths**

 - <code>bridges.webhook.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__WEBHOOK__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>pos_integer()</code>
  * default: 
  `16`

  Resource worker pool size.

- health_check_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  Health check interval, in milliseconds.

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>
  * default: 
  `"60s"`

  The auto restart interval after the resource is disconnected, in milliseconds.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  Query mode. Optional 'sync/async', default 'sync'.

- async_inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  Async query inflight window.

- enable_queue: <code>boolean()</code>
  * default: 
  `false`

  Queue mode enabled.

- max_queue_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `"100MB"`

  Maximum queue storage.


## connector-http:request



**Config paths**

 - <code>authentication.$INDEX.request</code>
 - <code>authorization.sources.$INDEX.request</code>
 - <code>bridges.webhook.$name.request</code>
 - <code>gateway.coap.authentication.request</code>
 - <code>gateway.coap.listeners.dtls.$name.authentication.request</code>
 - <code>gateway.coap.listeners.udp.$name.authentication.request</code>
 - <code>gateway.exproto.authentication.request</code>
 - <code>gateway.exproto.listeners.dtls.$name.authentication.request</code>
 - <code>gateway.exproto.listeners.ssl.$name.authentication.request</code>
 - <code>gateway.exproto.listeners.tcp.$name.authentication.request</code>
 - <code>gateway.exproto.listeners.udp.$name.authentication.request</code>
 - <code>gateway.lwm2m.authentication.request</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.authentication.request</code>
 - <code>gateway.lwm2m.listeners.udp.$name.authentication.request</code>
 - <code>gateway.mqttsn.authentication.request</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.authentication.request</code>
 - <code>gateway.mqttsn.listeners.udp.$name.authentication.request</code>
 - <code>gateway.stomp.authentication.request</code>
 - <code>gateway.stomp.listeners.ssl.$name.authentication.request</code>
 - <code>gateway.stomp.listeners.tcp.$name.authentication.request</code>
 - <code>listeners.ssl.$name.authentication.$INDEX.request</code>
 - <code>listeners.tcp.$name.authentication.$INDEX.request</code>
 - <code>listeners.ws.$name.authentication.$INDEX.request</code>
 - <code>listeners.wss.$name.authentication.$INDEX.request</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__REQUEST</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__REQUEST</code>
 - <code>EMQX_BRIDGES__WEBHOOK__$NAME__REQUEST</code>
 - <code>EMQX_GATEWAY__COAP__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__COAP__LISTENERS__UDP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__EXPROTO__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__TCP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__UDP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__LWM2M__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__UDP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__MQTTSN__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__UDP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__STOMP__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__TCP__$NAME__AUTHENTICATION__REQUEST</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__AUTHENTICATION__$INDEX__REQUEST</code>
 - <code>EMQX_LISTENERS__TCP__$NAME__AUTHENTICATION__$INDEX__REQUEST</code>
 - <code>EMQX_LISTENERS__WS__$NAME__AUTHENTICATION__$INDEX__REQUEST</code>
 - <code>EMQX_LISTENERS__WSS__$NAME__AUTHENTICATION__$INDEX__REQUEST</code>



**Fields**

- method: <code>binary()</code>

  HTTP method.

- path: <code>binary()</code>

  URL path.

- body: <code>binary()</code>

  HTTP request body.

- headers: <code>map()</code>

  List of HTTP headers.

- max_retries: <code>non_neg_integer()</code>

  Max retry times if error on sending request.

- request_timeout: <code>emqx_schema:duration_ms()</code>

  HTTP request timeout.


## connector-mqtt:egress
The egress config defines how this bridge forwards messages from the local broker to the remote broker.<br/>
Template with variables is allowed in 'remote.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
NOTE: if this bridge is used as the action of a rule, and also 'local.topic'
is configured, then both the data got from the rule and the MQTT messages that matches
'local.topic' will be forwarded.


**Config paths**

 - <code>bridges.mqtt.$name.egress</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__EGRESS</code>



**Fields**

- local: <code>[connector-mqtt:egress_local](#connector-mqtt-egress_local)</code>

  The configs about receiving messages from local broker.

- remote: <code>[connector-mqtt:egress_remote](#connector-mqtt-egress_remote)</code>

  The configs about sending message to the remote broker.


## connector-mqtt:egress_local
The configs about receiving messages from local broker.


**Config paths**

 - <code>bridges.mqtt.$name.egress.local</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__EGRESS__LOCAL</code>



**Fields**

- topic: <code>binary()</code>

  The local topic to be forwarded to the remote broker


## connector-mqtt:egress_remote
The configs about sending message to the remote broker.


**Config paths**

 - <code>bridges.mqtt.$name.egress.remote</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__EGRESS__REMOTE</code>



**Fields**

- topic: <code>binary()</code>


  Forward to which topic of the remote broker.<br/>
  Template with variables is allowed.


- qos: <code>qos() | binary()</code>


  The QoS of the MQTT message to be sent.<br/>
  Template with variables is allowed.


- retain: <code>boolean() | binary()</code>


  The 'retain' flag of the MQTT message to be sent.<br/>
  Template with variables is allowed.


- payload: <code>binary()</code>


  The payload of the MQTT message to be sent.<br/>
  Template with variables is allowed.



## connector-mqtt:ingress
The ingress config defines how this bridge receive messages from the remote MQTT broker, and then
        send them to the local broker.<br/>
        Template with variables is allowed in 'remote.qos', 'local.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
        NOTE: if this bridge is used as the input of a rule, and also 'local.topic' is
        configured, then messages got from the remote broker will be sent to both the 'local.topic' and
        the rule.


**Config paths**

 - <code>bridges.mqtt.$name.ingress</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__INGRESS</code>



**Fields**

- remote: <code>[connector-mqtt:ingress_remote](#connector-mqtt-ingress_remote)</code>

  The configs about subscribing to the remote broker.

- local: <code>[connector-mqtt:ingress_local](#connector-mqtt-ingress_local)</code>

  The configs about sending message to the local broker.


## connector-mqtt:ingress_local
The configs about sending message to the local broker.


**Config paths**

 - <code>bridges.mqtt.$name.ingress.local</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__INGRESS__LOCAL</code>



**Fields**

- topic: <code>binary()</code>


  Send messages to which topic of the local broker.<br/>
  Template with variables is allowed.


- qos: <code>qos() | binary()</code>
  * default: 
  `"${qos}"`


  The QoS of the MQTT message to be sent.<br/>
  Template with variables is allowed.


- retain: <code>boolean() | binary()</code>
  * default: 
  `"${retain}"`


  The 'retain' flag of the MQTT message to be sent.<br/>
  Template with variables is allowed.


- payload: <code>binary()</code>


  The payload of the MQTT message to be sent.<br/>
  Template with variables is allowed.



## connector-mqtt:ingress_remote
The configs about subscribing to the remote broker.


**Config paths**

 - <code>bridges.mqtt.$name.ingress.remote</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__INGRESS__REMOTE</code>



**Fields**

- topic: <code>binary()</code>

  Receive messages from which topic of the remote broker

- qos: <code>qos() | binary()</code>
  * default: 
  `1`

  The QoS level to be used when subscribing to the remote broker


## plugin:plugins

Manage EMQX plugins.<br/>
Plugins can be pre-built as a part of EMQX package,
or installed as a standalone package in a location specified by
<code>install_dir</code> config key<br/>
The standalone-installed plugins are referred to as 'external' plugins.



**Config paths**

 - <code>plugins</code>


**Env overrides**

 - <code>EMQX_PLUGINS</code>



**Fields**

- states: <code>[[plugin:state](#plugin-state)]</code>
  * default: 
  `[]`

  An array of plugins in the desired states.<br/>
  The plugins are started in the defined order

- install_dir: <code>string()</code>
  * default: 
  `"plugins"`


  The installation directory for the external plugins.
  The plugin beam files and configuration files should reside in
  the subdirectory named as <code>emqx_foo_bar-0.1.0</code>.
  <br/>
  NOTE: For security reasons, this directory should **NOT** be writable
  by anyone except <code>emqx</code> (or any user which runs EMQX).


- check_interval: <code>emqx_schema:duration()</code>
  * default: 
  `"5s"`

  Check interval: check if the status of the plugins in the cluster is consistent, <br/>
  if the results of 3 consecutive checks are not consistent, then alarm.



## plugin:state
A per-plugin config to describe the desired state of the plugin.


**Config paths**

 - <code>plugins.states.$INDEX</code>


**Env overrides**

 - <code>EMQX_PLUGINS__STATES__$INDEX</code>



**Fields**

- name_vsn: <code>string()</code>

  The {name}-{version} of the plugin.<br/>
  It should match the plugin application name-version as the for the plugin release package name<br/>
  For example: my_plugin-0.1.0.


- enable: <code>boolean()</code>

  Set to 'true' to enable this plugin


## prometheus
Settings for reporting metrics to Prometheus


**Config paths**

 - <code>prometheus</code>


**Env overrides**

 - <code>EMQX_PROMETHEUS</code>



**Fields**

- push_gateway_server: <code>string()</code>
  * default: 
  `"http://127.0.0.1:9091"`

  URL of Prometheus server

- interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"15s"`

  Data reporting interval

- enable: <code>boolean()</code>
  * default: 
  `false`

  Turn Prometheus data pushing on or off


## retainer:flow_control
Retainer batching and rate limiting.


**Config paths**

 - <code>retainer.flow_control</code>


**Env overrides**

 - <code>EMQX_RETAINER__FLOW_CONTROL</code>



**Fields**

- batch_read_number: <code>non_neg_integer()</code>
  * default: 
  `0`

  Size of the batch when reading messages from storage. 0 means no limit.

- batch_deliver_number: <code>0..1000</code>
  * default: 
  `0`

  The number of retained messages can be delivered per batch.

- batch_deliver_limiter: <code>[limiter:internal](#limiter-internal)</code>

  The rate limiter name for retained messages' delivery.
  Limiter helps to avoid delivering too many messages to the client at once, which may cause the client to block or crash, or drop messages due to exceeding the size of the message queue.
  The names of the available rate limiters are taken from the existing rate limiters under `limiter.batch`.
  If this field is empty, limiter is not used.


## retainer:mnesia_config
Configuration of the internal database storing retained messages.


**Config paths**

 - <code>retainer.backend</code>


**Env overrides**

 - <code>EMQX_RETAINER__BACKEND</code>



**Fields**

- type: <code>built_in_database</code>
  * default: 
  `built_in_database`

  Backend type.

- storage_type: <code>ram | disc</code>
  * default: 
  `ram`

  Specifies whether the messages are stored in RAM or persisted on disc.

- max_retained_messages: <code>non_neg_integer()</code>
  * default: 
  `0`

  Maximum number of retained messages. 0 means no limit.

- index_specs: <code>[[integer()]]</code>
  * default: 

  ```
  [
    [1, 2, 3],
    [1, 3],
    [2, 3],
    [3]
  ]
  ```

  Retainer index specifications: list of arrays of positive ascending integers. Each array specifies an index. Numbers in an index specification are 1-based word positions in topics. Words from specified positions will be used for indexing.<br/>For example, it is good to have <code>[2, 4]</code> index to optimize <code>+/X/+/Y/...</code> topic wildcard subscriptions.


## retainer
Configuration related to handling `PUBLISH` packets with a `retain` flag set to 1.


**Config paths**

 - <code>retainer</code>


**Env overrides**

 - <code>EMQX_RETAINER</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable retainer feature

- msg_expiry_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"0s"`

  Message retention time. 0 means message will never be expired.

- msg_clear_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"0s"`

  Periodic interval for cleaning up expired messages.
  Never clear if the value is 0.
        

- flow_control: <code>[retainer:flow_control](#retainer-flow_control)</code>
  * default: 
  `{}`

  Flow control.

- max_payload_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `"1MB"`

  Maximum retained message size.

- stop_publish_clear_msg: <code>boolean()</code>
  * default: 
  `false`

  When the retained flag of the `PUBLISH` message is set and Payload is empty,
  whether to continue to publish the message.
  See:
  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718038

- backend: <code>[retainer:mnesia_config](#retainer-mnesia_config)</code>

  Settings for the database storing the retained messages.


## slow_subs
Configuration for `slow_subs` feature.


**Config paths**

 - <code>slow_subs</code>


**Env overrides**

 - <code>EMQX_SLOW_SUBS</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable this feature

- threshold: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"500ms"`

  The latency threshold for statistics

- expire_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"300s"`

  The eviction time of the record, which in the statistics record table

- top_k_num: <code>pos_integer()</code>
  * default: 
  `10`

  The maximum number of records in the slow subscription statistics record table

- stats_type: <code>whole | internal | response</code>
  * default: 
  `whole`

  The method to calculate the latency


## statsd
StatsD metrics collection and push configuration.


**Config paths**

 - <code>statsd</code>


**Env overrides**

 - <code>EMQX_STATSD</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable or disable StatsD metrics collection and push service.

- server: <code>emqx_schema:host_port()</code>
  * default: 
  `"127.0.0.1:8125"`

  StatsD server address.

- sample_time_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"30s"`

  The sampling interval for metrics.

- flush_time_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `"30s"`

  The push interval for metrics.

- tags: <code>map()</code>
  * default: 
  `{}`

  The tags for metrics.


