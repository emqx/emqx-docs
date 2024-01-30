# EMQX Configuration

<!--5.5.0-g4688b36c-->
EMQX configuration files are in [HOCON](https://github.com/emqx/hocon) format.
HOCON, or Human-Optimized Config Object Notation is a format for human-readable data,
and a superset of JSON.

## Layered

EMQX configuration consists of two layers.
From bottom up:

1. Cluster-synced configs: `$EMQX_NODE__DATA_DIR/configs/cluster.hocon`.
2. Local node configs: `emqx.conf` + `EMQX_` prefixed environment variables.

:::tip Tip
Prior to v5.0.23 and e5.0.3, the cluster-synced configs are stored in
`cluster-override.conf` which is applied on top of the local configs.

If upgraded from an earlier version, as long as `cluster-override.conf` exists,
`cluster.hocon` will not be created, and `cluster-override.conf` will stay on
top of the overriding layers.
:::

When environment variable `$EMQX_NODE__DATA_DIR` is not set, config `node.data_dir`
is used.

The `cluster.hocon` file is overwritten at runtime when changes
are made from Dashboard, management HTTP API, or CLI. When clustered,
after EMQX restarts, it copies the file from the node which has the greatest `uptime`.

:::tip Tip
To avoid confusion, don't add the same keys in both `cluster.hocon` and `emqx.conf`.
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
If map field name is a positive integer number, it is interpreted as an alternative representation of an `Array`.
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

Each segment of the dotted string is a Struct field name or Map key.
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
However, this also means a string value should be quoted if it happens to contain special
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

This feature makes it easy to override array element values. For example:

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
    "RSA-PSK-AES128-CBC-SHA",
    "PSK-AES256-GCM-SHA384",
    "PSK-AES128-GCM-SHA256",
    "PSK-AES256-CBC-SHA384",
    "PSK-AES256-CBC-SHA",
    "PSK-AES128-CBC-SHA256",
    "PSK-AES128-CBC-SHA"
  ]
```

## emqx:Root Config Keys




**Fields**

- listeners: <code>[broker:listeners](#broker-listeners)</code>



- mqtt: <code>[broker:mqtt](#broker-mqtt)</code>

  Global MQTT configuration.
  The configs here work as default values which can be overridden in <code>zone</code> configs

- authentication: <code>[[authn:builtin_db](#authn-builtin_db) | [authn:mysql](#authn-mysql) | [authn:postgresql](#authn-postgresql) | [authn:mongo_single](#authn-mongo_single) | [authn:mongo_rs](#authn-mongo_rs) | [authn:mongo_sharded](#authn-mongo_sharded) | [authn:redis_single](#authn-redis_single) | [authn:redis_cluster](#authn-redis_cluster) | [authn:redis_sentinel](#authn-redis_sentinel) | [authn:http_get](#authn-http_get) | [authn:http_post](#authn-http_post) | [authn:jwt_hmac](#authn-jwt_hmac) | [authn:jwt_public_key](#authn-jwt_public_key) | [authn:jwt_jwks](#authn-jwt_jwks) | [authn:scram](#authn-scram) | [authn:ldap](#authn-ldap) | [authn:ldap_deprecated](#authn-ldap_deprecated)]</code>
  * default: 
  `[]`

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

- authorization: <code>[emqx:authorization](#emqx-authorization)</code>

  Authorization a.k.a. ACL.<br/>
  In EMQX, MQTT client access control is extremely flexible.<br/>
  An out-of-the-box set of authorization data sources are supported.
  For example,<br/>
  'file' source is to support concise and yet generic ACL rules in a file;<br/>
  'built_in_database' source can be used to store per-client customizable rule sets,
  natively in the EMQX node;<br/>
  'http' source to make EMQX call an external HTTP API to make the decision;<br/>
  'PostgreSQL' etc. to look up clients or rules from external databases

- node: <code>[emqx:node](#emqx-node)</code>



- cluster: <code>[emqx:cluster](#emqx-cluster)</code>



- log: <code>[emqx:log](#emqx-log)</code>



- rpc: <code>[emqx:rpc](#emqx-rpc)</code>



- sys_topics: <code>[broker:sys_topics](#broker-sys_topics)</code>

  System topics configuration.

- force_shutdown: <code>[broker:force_shutdown](#broker-force_shutdown)</code>



- force_gc: <code>[broker:force_gc](#broker-force_gc)</code>



- sysmon: <code>[broker:sysmon](#broker-sysmon)</code>



- alarm: <code>[broker:alarm](#broker-alarm)</code>



- flapping_detect: <code>[broker:flapping_detect](#broker-flapping_detect)</code>



- bridges: <code>[bridge:bridges](#bridge-bridges)</code>



- connectors: <code>[connector:connectors](#connector-connectors)</code>



- actions: <code>[actions_and_sources:actions](#actions_and_sources-actions)</code>



- sources: <code>[actions_and_sources:sources](#actions_and_sources-sources)</code>



- retainer: <code>[retainer](#retainer)</code>



- telemetry: <code>[emqxtel:telemetry](#emqxtel-telemetry)</code>



- delayed: <code>[modules:delayed](#modules-delayed)</code>



- plugins: <code>[plugin:plugins](#plugin-plugins)</code>



- dashboard: <code>[dashboard](#dashboard)</code>



- gateway: <code>[gateway](#gateway)</code>



- prometheus: <code>[prometheus:recommend_setting](#prometheus-recommend_setting) | [prometheus:legacy_deprecated_setting](#prometheus-legacy_deprecated_setting)</code>
  * default: 
  `{}`



- exhook: <code>[exhook](#exhook)</code>



- psk_authentication: <code>[psk:psk_authentication](#psk-psk_authentication)</code>



- slow_subs: <code>[slow_subs](#slow_subs)</code>



- opentelemetry: <code>[opentelemetry](#opentelemetry)</code>



- api_key: <code>[api_key](#api_key)</code>




## api_key
API Key, can be used to request API other than the management API key and the Dashboard user management API


**Config paths**

 - <code>api_key</code>


**Env overrides**

 - <code>EMQX_API_KEY</code>



**Fields**

- bootstrap_file: <code>binary()</code>
  * default: 
  `""`

  The bootstrap file provides API keys for EMQX.
  EMQX will load these keys on startup to authorize API requests.
  It contains colon-separated values in the format: `api_key:api_secret:role`.
  Each line specifies an API key and its associated secret, and the role of this key.
  The 'role' part should be the pre-defined access scope group name,
  for example, `administrator` or `viewer`.
  The 'role' is introduced in 5.4, to be backward compatible, if it is missing, the key is implicitly granted `administrator` role.


## broker:authz_cache
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
  `1m`

  Time to live for the cached data.

- excludes: <code>[binary()]</code>
  * default: 
  `[]`

  Exclude caching ACL check results for topics matching the given patterns.


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
  `24h`

  Retention time of deactivated alarms. Alarms are not deleted immediately
  when deactivated, but after the retention time.


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

- window_time: <code>emqx_schema:duration()</code>
  * default: 
  `1m`

  The time window for flapping detection.

- max_count: <code>non_neg_integer()</code>
  * default: 
  `15`

  The maximum number of disconnects allowed for a MQTT Client in `window_time`

- ban_time: <code>emqx_schema:duration()</code>
  * default: 
  `5m`

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
  `16MB`

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

- max_mailbox_size: <code>0..inf</code>
  * default: 
  `1000`

  In EMQX, each online client corresponds to an individual Erlang process. The configuration value establishes a mailbox size limit for these processes. If the mailbox size surpasses this limit, the client will be automatically terminated.

- max_heap_size: <code>emqx_schema:wordsize()</code>
  * default: 
  `32MB`

  Total heap size


## broker:listener_quic_ssl_opts
TLS options for QUIC transport.


**Config paths**

 - <code>listeners.quic.$name.ssl_options</code>


**Env overrides**

 - <code>EMQX_LISTENERS__QUIC__$NAME__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM format private key file.

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification.

- password: <code>string()</code>

  String containing the user's password. Only used if the private key file is password-protected.


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
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM format private key file.

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification.

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.

- password: <code>string()</code>

  String containing the user's password. Only used if the private key file is password-protected.

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

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
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.

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

  An important security setting. It forces the cipher to be set based
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
  the number of messages the underlying cipher suite can encipher.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  Maximum time duration allowed for the handshake to complete

- gc_after_handshake: <code>boolean()</code>
  * default: 
  `false`

  Memory usage tuning. If enabled, will immediately perform a garbage collection after the TLS/SSL handshake.

- ocsp: <code>[broker:ocsp](#broker-ocsp)</code>



- enable_crl_check: <code>boolean()</code>
  * default: 
  `false`

  Whether to enable CRL verification for this listener.


## broker:listener_wss_opts
Socket options for WebSocket/SSL connections.


**Config paths**

 - <code>listeners.wss.$name.ssl_options</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WSS__$NAME__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM format private key file.

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification.

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.

- password: <code>string()</code>

  String containing the user's password. Only used if the private key file is password-protected.

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

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
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.

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

  An important security setting. It forces the cipher to be set based
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
  the number of messages the underlying cipher suite can encipher.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  Maximum time duration allowed for the handshake to complete


## broker:listeners
MQTT listeners identified by their protocol type and assigned names


**Config paths**

 - <code>listeners</code>


**Env overrides**

 - <code>EMQX_LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [broker:mqtt_tcp_listener](#broker-mqtt_tcp_listener) | marked_for_deletion}</code>

  TCP listeners.

- ssl: <code>{$name -> [broker:mqtt_ssl_listener](#broker-mqtt_ssl_listener) | marked_for_deletion}</code>

  SSL listeners.

- ws: <code>{$name -> [broker:mqtt_ws_listener](#broker-mqtt_ws_listener) | marked_for_deletion}</code>

  HTTP websocket listeners.

- wss: <code>{$name -> [broker:mqtt_wss_listener](#broker-mqtt_wss_listener) | marked_for_deletion}</code>

  HTTPS websocket listeners.

- quic: <code>{$name -> [broker:mqtt_quic_listener](#broker-mqtt_quic_listener) | marked_for_deletion}</code>

  QUIC listeners.


## broker:mqtt
Global MQTT configuration.


**Config paths**

 - <code>mqtt</code>


**Env overrides**

 - <code>EMQX_MQTT</code>



**Fields**

- idle_timeout: <code>infinity | emqx_schema:duration()</code>
  * default: 
  `15s`

  Configure the duration of time that a connection can remain idle (i.e., without any data transfer) before being:
    - Automatically disconnected  if no CONNECT package is received from the client yet.
    - Put into hibernation mode to save resources if some CONNECT packages are already received.
  Note: Please set the parameter with caution as long idle time will lead to resource waste.

- max_packet_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  Maximum MQTT packet size allowed. Default: 1 MB, Maximum: 256 MB

- max_clientid_len: <code>23..65535</code>
  * default: 
  `65535`

  Maximum allowed length of MQTT Client ID.

- max_topic_levels: <code>1..65535</code>
  * default: 
  `128`

  Maximum topic levels allowed.

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

- shared_subscription_strategy: <code>random | round_robin | round_robin_per_group | sticky | local | hash_topic | hash_clientid</code>
  * default: 
  `round_robin`

  Dispatch strategy for shared subscription.
   - `random`: Randomly select a subscriber for dispatch;
   - `round_robin`: Messages from a single publisher are dispatched to subscribers in turn;
   - `round_robin_per_group`: All messages are dispatched to subscribers in turn;
   - `local`: Randomly select a subscriber on the current node, if there are no subscribers on the current node, then randomly select within the cluster;
   - `sticky`: Continuously dispatch messages to the initially selected subscriber until their session ends;
   - `hash_clientid`: Hash the publisher's client ID to select a subscriber;
   - `hash_topic`: Hash the publishing topic to select a subscriber.

- exclusive_subscription: <code>boolean()</code>
  * default: 
  `false`

  Whether to enable support for MQTT exclusive subscription.

- ignore_loop_deliver: <code>boolean()</code>
  * default: 
  `false`

  Whether the messages sent by the MQTT v3.1.1/v3.1.0 client will be looped back to the publisher itself, similar to <code>No Local</code> in MQTT 5.0.

- strict_mode: <code>boolean()</code>
  * default: 
  `false`

  Whether to parse MQTT messages in strict mode.
  In strict mode, invalid utf8 strings in for example client ID, topic name, etc. will cause the client to be disconnected.

- response_information: <code>string()</code>
  * default: 
  `""`

  UTF-8 string, for creating the response topic, for example, if set to <code>reqrsp/</code>, the publisher/subscriber will communicate using the topic prefix <code>reqrsp/</code>.
  To disable this feature, input <code>""</code> in the text box below. Only applicable to MQTT 5.0 clients.

- server_keepalive: <code>pos_integer() | disabled</code>
  * default: 
  `disabled`

  The keep alive duration required by EMQX. To use the setting from the client side, choose disabled from the drop-down list. Only applicable to MQTT 5.0 clients.

- keepalive_multiplier: <code>number()</code>
  * default: 
  `1.5`

  Keep-Alive Timeout = Keep-Alive interval × Keep-Alive Multiplier.
  The default value 1.5 is following the MQTT 5.0 specification. This multiplier is adjustable, providing system administrators flexibility for tailoring to their specific needs. For instance, if a client's 10-second Keep-Alive interval PINGREQ gets delayed by an extra 10 seconds, changing the multiplier to 2 lets EMQX tolerate this delay.

- retry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `30s`

  Retry interval for QoS 1/2 message delivering.

- use_username_as_clientid: <code>boolean()</code>
  * default: 
  `false`

  Whether to use Username as Client ID.
  This setting takes effect later than <code>Use Peer Certificate as Username</code> and <code>Use peer certificate as Client ID</code>.

- peer_cert_as_username: <code>disabled | cn | dn | crt | pem | md5</code>
  * default: 
  `disabled`

  Use the CN, DN field in the peer certificate or the entire certificate content as Username. Only works for the TLS connection.
  Supported configurations are the following:
  - <code>cn</code>: CN field of the certificate
  - <code>dn</code>: DN field of the certificate
  - <code>crt</code>: Content of the <code>DER</code> or <code>PEM</code> certificate
  - <code>pem</code>: Convert <code>DER</code> certificate content to <code>PEM</code> format and use as Username
  - <code>md5</code>: MD5 value of the <code>DER</code> or <code>PEM</code> certificate

- peer_cert_as_clientid: <code>disabled | cn | dn | crt | pem | md5</code>
  * default: 
  `disabled`

  Use the CN, DN field in the peer certificate or the entire certificate content as Client ID. Only works for the TLS connection.
  Supported configurations are the following:
  - <code>cn</code>: CN field of the certificate
  - <code>dn</code>: DN field of the certificate
  - <code>crt</code>: <code>DER</code> or <code>PEM</code> certificate
  - <code>pem</code>: Convert <code>DER</code> certificate content to <code>PEM</code> format and use as Client ID
  - <code>md5</code>: MD5 value of the <code>DER</code> or <code>PEM</code> certificate

- session_expiry_interval: <code>emqx_schema:duration()</code>
  * default: 
  `2h`

  Specifies how long the session will expire after the connection is disconnected, only for non-MQTT 5.0 connections.

- max_awaiting_rel: <code>non_neg_integer() | infinity</code>
  * default: 
  `100`

  For each publisher session, the maximum number of outstanding QoS 2 messages pending on the client to send PUBREL. After reaching this limit, new QoS 2 PUBLISH requests will be rejected with `147(0x93)` until either PUBREL is received or timed out.

- max_qos_allowed: <code>qos()</code>
  * default: 
  `2`

  Maximum QoS allowed.

- mqueue_priorities: <code>disabled | map()</code>
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

- max_mqueue_len: <code>non_neg_integer() | infinity</code>
  * default: 
  `1000`

  Maximum queue length. Enqueued messages when persistent client disconnected, or inflight window is full.

- max_inflight: <code>1..65535</code>
  * default: 
  `32`

  Maximum number of QoS 1 and QoS 2 messages that are allowed to be delivered simultaneously before completing the acknowledgment.

- max_subscriptions: <code>1..inf | infinity</code>
  * default: 
  `infinity`

  Maximum number of subscriptions allowed per client.

- upgrade_qos: <code>boolean()</code>
  * default: 
  `false`

  Force upgrade of QoS level according to subscription.

- await_rel_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `300s`

  For client to broker QoS 2 message, the time limit for the broker to wait before the `PUBREL` message is received. The wait is aborted after timed out, meaning the packet ID is freed for new `PUBLISH` requests. Receiving a stale `PUBREL` causes a warning level log. Note, the message is delivered to subscribers before entering the wait for PUBREL.


## broker:mqtt_quic_listener
Settings for the MQTT over QUIC listener.


**Config paths**

 - <code>listeners.quic.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__QUIC__$NAME</code>



**Fields**

- ciphers: <code>[string()]</code>
  * default: 
  `[TLS_AES_256_GCM_SHA384, TLS_AES_128_GCM_SHA256, TLS_CHACHA20_POLY1305_SHA256]`

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

  NOTE: QUIC listener supports only 'tlsv1.3' ciphers

- ssl_options: <code>[broker:listener_quic_ssl_opts](#broker-listener_quic_ssl_opts)</code>

  TLS options for QUIC transport

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable listener.

- bind: <code>emqx_schema:ip_port()</code>
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

- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
  process goes through the configured authentication chain.
  When set to <code>false</code>, any client (with or without username/password) is allowed to connect.
  When set to <code>quick_deny_anonymous</code>, it behaves like when set to <code>true</code>, but clients will be
  denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
  anonymous clients early.

- max_conn_rate: <code>string()</code>

  Maximum connection rate.<br/>
  This is used to limit the connection rate for this node.
  Once the limit is reached, new connections will be deferred or refused.<br/>
  For example:<br/>
  - <code>1000/s</code> :: Only accepts 1000 connections per second<br/>
  - <code>1000/10s</code> :: Only accepts 1000 connections every 10 seconds.

- messages_rate: <code>string()</code>

  Messages publish rate.<br/>
  This is used to limit the inbound message numbers for this node.
  Once the limit is reached, the restricted client will slow down and even be hung for a while.<br/>
  For example:<br/>
  - <code>500/s</code> :: Only the first 500 messages are sent per second and other messages are buffered.<br/>
  - <code>500/10s</code> :: Only the first 500 messages are sent even 10 second and other messages are buffered.

- bytes_rate: <code>string()</code>

  Data publish rate.<br/>
  This is used to limit the inbound bytes rate for this node.
  Once the limit is reached, the restricted client will slow down and even be hung for a while.<br/>
  The unit of the bytes could be:KB MB GB.<br/>
  For example:<br/>
  - <code>500KB/s</code> :: Only the first 500 kilobytes are sent per second and other messages are buffered.<br/>
  - <code>500MB/10s</code> :: Only the first 500 megabytes are sent even 10 second and other messages are buffered.


## broker:mqtt_ssl_listener
Settings for the MQTT over SSL listener.


**Config paths**

 - <code>listeners.ssl.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__SSL__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable listener.

- bind: <code>emqx_schema:ip_port()</code>
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

- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
  process goes through the configured authentication chain.
  When set to <code>false</code>, any client (with or without username/password) is allowed to connect.
  When set to <code>quick_deny_anonymous</code>, it behaves like when set to <code>true</code>, but clients will be
  denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
  anonymous clients early.

- max_conn_rate: <code>string()</code>

  Maximum connection rate.<br/>
  This is used to limit the connection rate for this node.
  Once the limit is reached, new connections will be deferred or refused.<br/>
  For example:<br/>
  - <code>1000/s</code> :: Only accepts 1000 connections per second<br/>
  - <code>1000/10s</code> :: Only accepts 1000 connections every 10 seconds.

- messages_rate: <code>string()</code>

  Messages publish rate.<br/>
  This is used to limit the inbound message numbers for this node.
  Once the limit is reached, the restricted client will slow down and even be hung for a while.<br/>
  For example:<br/>
  - <code>500/s</code> :: Only the first 500 messages are sent per second and other messages are buffered.<br/>
  - <code>500/10s</code> :: Only the first 500 messages are sent even 10 second and other messages are buffered.

- bytes_rate: <code>string()</code>

  Data publish rate.<br/>
  This is used to limit the inbound bytes rate for this node.
  Once the limit is reached, the restricted client will slow down and even be hung for a while.<br/>
  The unit of the bytes could be:KB MB GB.<br/>
  For example:<br/>
  - <code>500KB/s</code> :: Only the first 500 kilobytes are sent per second and other messages are buffered.<br/>
  - <code>500MB/10s</code> :: Only the first 500 megabytes are sent even 10 second and other messages are buffered.

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
  `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- ssl_options: <code>[broker:listener_ssl_opts](#broker-listener_ssl_opts)</code>




## broker:mqtt_tcp_listener
Settings for the MQTT over TCP listener.


**Config paths**

 - <code>listeners.tcp.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__TCP__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable listener.

- bind: <code>emqx_schema:ip_port()</code>
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

- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
  process goes through the configured authentication chain.
  When set to <code>false</code>, any client (with or without username/password) is allowed to connect.
  When set to <code>quick_deny_anonymous</code>, it behaves like when set to <code>true</code>, but clients will be
  denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
  anonymous clients early.

- max_conn_rate: <code>string()</code>

  Maximum connection rate.<br/>
  This is used to limit the connection rate for this node.
  Once the limit is reached, new connections will be deferred or refused.<br/>
  For example:<br/>
  - <code>1000/s</code> :: Only accepts 1000 connections per second<br/>
  - <code>1000/10s</code> :: Only accepts 1000 connections every 10 seconds.

- messages_rate: <code>string()</code>

  Messages publish rate.<br/>
  This is used to limit the inbound message numbers for this node.
  Once the limit is reached, the restricted client will slow down and even be hung for a while.<br/>
  For example:<br/>
  - <code>500/s</code> :: Only the first 500 messages are sent per second and other messages are buffered.<br/>
  - <code>500/10s</code> :: Only the first 500 messages are sent even 10 second and other messages are buffered.

- bytes_rate: <code>string()</code>

  Data publish rate.<br/>
  This is used to limit the inbound bytes rate for this node.
  Once the limit is reached, the restricted client will slow down and even be hung for a while.<br/>
  The unit of the bytes could be:KB MB GB.<br/>
  For example:<br/>
  - <code>500KB/s</code> :: Only the first 500 kilobytes are sent per second and other messages are buffered.<br/>
  - <code>500MB/10s</code> :: Only the first 500 megabytes are sent even 10 second and other messages are buffered.

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
  `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>




## broker:mqtt_ws_listener
Settings for the MQTT over WebSocket listener.


**Config paths**

 - <code>listeners.ws.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WS__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable listener.

- bind: <code>emqx_schema:ip_port()</code>
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

- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
  process goes through the configured authentication chain.
  When set to <code>false</code>, any client (with or without username/password) is allowed to connect.
  When set to <code>quick_deny_anonymous</code>, it behaves like when set to <code>true</code>, but clients will be
  denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
  anonymous clients early.

- max_conn_rate: <code>string()</code>

  Maximum connection rate.<br/>
  This is used to limit the connection rate for this node.
  Once the limit is reached, new connections will be deferred or refused.<br/>
  For example:<br/>
  - <code>1000/s</code> :: Only accepts 1000 connections per second<br/>
  - <code>1000/10s</code> :: Only accepts 1000 connections every 10 seconds.

- messages_rate: <code>string()</code>

  Messages publish rate.<br/>
  This is used to limit the inbound message numbers for this node.
  Once the limit is reached, the restricted client will slow down and even be hung for a while.<br/>
  For example:<br/>
  - <code>500/s</code> :: Only the first 500 messages are sent per second and other messages are buffered.<br/>
  - <code>500/10s</code> :: Only the first 500 messages are sent even 10 second and other messages are buffered.

- bytes_rate: <code>string()</code>

  Data publish rate.<br/>
  This is used to limit the inbound bytes rate for this node.
  Once the limit is reached, the restricted client will slow down and even be hung for a while.<br/>
  The unit of the bytes could be:KB MB GB.<br/>
  For example:<br/>
  - <code>500KB/s</code> :: Only the first 500 kilobytes are sent per second and other messages are buffered.<br/>
  - <code>500MB/10s</code> :: Only the first 500 megabytes are sent even 10 second and other messages are buffered.

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
  `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- websocket: <code>[broker:ws_opts](#broker-ws_opts)</code>




## broker:mqtt_wss_listener
Settings for the MQTT over WebSocket/SSL listener.


**Config paths**

 - <code>listeners.wss.$name</code>


**Env overrides**

 - <code>EMQX_LISTENERS__WSS__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable listener.

- bind: <code>emqx_schema:ip_port()</code>
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

- enable_authn: <code>true | false | quick_deny_anonymous</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener, the authentication
  process goes through the configured authentication chain.
  When set to <code>false</code>, any client (with or without username/password) is allowed to connect.
  When set to <code>quick_deny_anonymous</code>, it behaves like when set to <code>true</code>, but clients will be
  denied immediately without going through any authenticators if <code>username</code> is not provided. This is useful to fence off
  anonymous clients early.

- max_conn_rate: <code>string()</code>

  Maximum connection rate.<br/>
  This is used to limit the connection rate for this node.
  Once the limit is reached, new connections will be deferred or refused.<br/>
  For example:<br/>
  - <code>1000/s</code> :: Only accepts 1000 connections per second<br/>
  - <code>1000/10s</code> :: Only accepts 1000 connections every 10 seconds.

- messages_rate: <code>string()</code>

  Messages publish rate.<br/>
  This is used to limit the inbound message numbers for this node.
  Once the limit is reached, the restricted client will slow down and even be hung for a while.<br/>
  For example:<br/>
  - <code>500/s</code> :: Only the first 500 messages are sent per second and other messages are buffered.<br/>
  - <code>500/10s</code> :: Only the first 500 messages are sent even 10 second and other messages are buffered.

- bytes_rate: <code>string()</code>

  Data publish rate.<br/>
  This is used to limit the inbound bytes rate for this node.
  Once the limit is reached, the restricted client will slow down and even be hung for a while.<br/>
  The unit of the bytes could be:KB MB GB.<br/>
  For example:<br/>
  - <code>500KB/s</code> :: Only the first 500 kilobytes are sent per second and other messages are buffered.<br/>
  - <code>500MB/10s</code> :: Only the first 500 megabytes are sent even 10 second and other messages are buffered.

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
  `3s`

  Timeout for proxy protocol. EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.

- tcp_options: <code>[broker:tcp_opts](#broker-tcp_opts)</code>



- ssl_options: <code>[broker:listener_wss_opts](#broker-listener_wss_opts)</code>



- websocket: <code>[broker:ws_opts](#broker-ws_opts)</code>




## broker:ocsp
Per listener OCSP Stapling configuration.


**Config paths**

 - <code>gateway.coap.listeners.dtls.$name.dtls_options.ocsp</code>
 - <code>gateway.exproto.listeners.dtls.$name.dtls_options.ocsp</code>
 - <code>gateway.exproto.listeners.ssl.$name.ssl_options.ocsp</code>
 - <code>gateway.lwm2m.listeners.dtls.$name.dtls_options.ocsp</code>
 - <code>gateway.mqttsn.listeners.dtls.$name.dtls_options.ocsp</code>
 - <code>gateway.stomp.listeners.ssl.$name.ssl_options.ocsp</code>
 - <code>listeners.ssl.$name.ssl_options.ocsp</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP__LISTENERS__DTLS__$NAME__DTLS_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__DTLS__$NAME__DTLS_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS__SSL__$NAME__SSL_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__LWM2M__LISTENERS__DTLS__$NAME__DTLS_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__MQTTSN__LISTENERS__DTLS__$NAME__DTLS_OPTIONS__OCSP</code>
 - <code>EMQX_GATEWAY__STOMP__LISTENERS__SSL__$NAME__SSL_OPTIONS__OCSP</code>
 - <code>EMQX_LISTENERS__SSL__$NAME__SSL_OPTIONS__OCSP</code>



**Fields**

- enable_ocsp_stapling: <code>boolean()</code>
  * default: 
  `false`

  Whether to enable Online Certificate Status Protocol (OCSP) stapling for the listener.  If set to true, requires defining the OCSP responder URL and issuer PEM path.

- responder_url: <code>emqx_schema:url()</code>

  URL for the OCSP responder to check the server certificate against.

- issuer_pem: <code>binary()</code>

  PEM-encoded certificate of the OCSP issuer for the server certificate.

- refresh_interval: <code>emqx_schema:duration()</code>
  * default: 
  `5m`

  The period to refresh the OCSP response for the server.

- refresh_http_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  The timeout for the HTTP request when checking OCSP responses.


## broker:ssl_client_opts
Socket options for SSL clients.


**Config paths**

 - <code>authentication.$INDEX.ssl</code>
 - <code>authorization.sources.$INDEX.ssl</code>
 - <code>bridges.mqtt.$name.ssl</code>
 - <code>bridges.webhook.$name.ssl</code>
 - <code>cluster.etcd.ssl_options</code>
 - <code>connectors.http.$name.ssl</code>
 - <code>connectors.mqtt.$name.ssl</code>
 - <code>gateway.exproto.handler.ssl_options</code>
 - <code>opentelemetry.exporter.ssl_options</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__SSL</code>
 - <code>EMQX_BRIDGES__MQTT__$NAME__SSL</code>
 - <code>EMQX_BRIDGES__WEBHOOK__$NAME__SSL</code>
 - <code>EMQX_CLUSTER__ETCD__SSL_OPTIONS</code>
 - <code>EMQX_CONNECTORS__HTTP__$NAME__SSL</code>
 - <code>EMQX_CONNECTORS__MQTT__$NAME__SSL</code>
 - <code>EMQX_GATEWAY__EXPROTO__HANDLER__SSL_OPTIONS</code>
 - <code>EMQX_OPENTELEMETRY__EXPORTER__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>

  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

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

  Enable TLS session reuse.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.

- password: <code>string()</code>

  String containing the user's password. Only used if the private key file is password-protected.

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

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
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.

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
  to establish the connection, unless it is IP address used.<br/>
  The host name is then also used in the host name verification of the peer
  certificate.<br/> The special value 'disable' prevents the Server Name
  Indication extension from being sent and disables the hostname
  verification check.


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
  `1m`

  Time interval for publishing following system messages:
    - `$SYS/brokers`
    - `$SYS/brokers/<node>/version`
    - `$SYS/brokers/<node>/sysdescr`
    - `$SYS/brokers/<node>/stats/<name>`
    - `$SYS/brokers/<node>/metrics/<name>`

- sys_heartbeat_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `30s`

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
  `60s`

  The time interval for the periodic CPU check. Disabled on Windows platform.

- cpu_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `80%`

  The threshold, as percentage of system CPU load,
   for how much system cpu can be used before the corresponding alarm is raised. Disabled on Windows platform

- cpu_low_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `60%`

  The threshold, as percentage of system CPU load,
   for how much system cpu can be used before the corresponding alarm is cleared. Disabled on Windows platform

- mem_check_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `60s`

  The time interval for the periodic memory check. Disabled on Windows platform.

- sysmem_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `70%`

  The threshold, as percentage of system memory,
   for how much system memory can be allocated before the corresponding alarm is raised. Disabled on Windows platform

- procmem_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `5%`

  The threshold, as percentage of system memory,
   for how much system memory can be allocated by one Erlang process before
   the corresponding alarm is raised. Disabled on Windows platform.


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
  `30s`

  The time interval for the periodic process limit check.

- process_high_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `80%`

  The threshold, as percentage of processes, for how many
   processes can simultaneously exist at the local node before the corresponding
   alarm is raised.

- process_low_watermark: <code>emqx_schema:percent()</code>
  * default: 
  `60%`

  The threshold, as percentage of processes, for how many
   processes can simultaneously exist at the local node before the corresponding
   alarm is cleared.

- long_gc: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `disabled`

  When an Erlang process spends long time to perform garbage collection, a warning level <code>long_gc</code> log is emitted,
  and an MQTT message is published to the system topic <code>$SYS/sysmon/long_gc</code>.

- long_schedule: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `240ms`

  When the Erlang VM detect a task scheduled for too long, a warning level 'long_schedule' log is emitted,
  and an MQTT message is published to the system topic <code>$SYS/sysmon/long_schedule</code>.

- large_heap: <code>disabled | emqx_schema:bytesize()</code>
  * default: 
  `32MB`

  When an Erlang process consumed a large amount of memory for its heap space,
  the system will write a warning level <code>large_heap</code> log, and an MQTT message is published to
  the system topic <code>$SYS/sysmon/large_heap</code>.

- busy_dist_port: <code>boolean()</code>
  * default: 
  `true`

  When the RPC connection used to communicate with other nodes in the cluster is overloaded,
  there will be a <code>busy_dist_port</code> warning log,
  and an MQTT message is published to system topic <code>$SYS/sysmon/busy_dist_port</code>.

- busy_port: <code>boolean()</code>
  * default: 
  `true`

  When a port (e.g. TCP socket) is overloaded, there will be a <code>busy_port</code> warning log,
  and an MQTT message is published to the system topic <code>$SYS/sysmon/busy_port</code>.


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
  `15s`

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
  `4KB`

  The size of the user-space buffer used by the driver.

- high_watermark: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

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

- keepalive: <code>string()</code>
  * default: 
  `none`

  Enable TCP keepalive for MQTT connections over TCP or SSL.
  The value is three comma separated numbers in the format of 'Idle,Interval,Probes'
   - Idle: The number of seconds a connection needs to be idle before the server begins to send out keep-alive probes (Linux default 7200).
   - Interval: The number of seconds between TCP keep-alive probes (Linux default 75).
   - Probes: The maximum number of TCP keep-alive probes to send before giving up and killing the connection if no response is obtained from the other end (Linux default 9).
  For example "240,30,5" means: EMQX should start sending TCP keepalive probes after the connection is in idle for 240 seconds, and the probes are sent every 30 seconds until a response is received from the MQTT client, if it misses 5 consecutive responses, EMQX should close the connection.
  Default: 'none'


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
  `7200s`

  Close transport-layer connections from the clients that have not sent MQTT CONNECT message within this interval.

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
  `x-forwarded-for`

  HTTP header used to pass information about the client IP address.
  Relevant when the EMQX cluster is deployed behind a load-balancer.

- proxy_port_header: <code>string()</code>
  * default: 
  `x-forwarded-port`

  HTTP header used to pass information about the client port. Relevant when the EMQX cluster is deployed behind a load-balancer.

- deflate_opts: <code>[broker:deflate_opts](#broker-deflate_opts)</code>




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

- token_expired_time: <code>emqx_schema:duration()</code>
  * default: 
  `60m`

  JWT token expiration time. Default is 60 minutes

- cors: <code>boolean()</code>
  * default: 
  `false`

  Support Cross-Origin Resource Sharing (CORS).
  Allows a server to indicate any origins (domain, scheme, or port) other than
  its own from which a browser should permit loading resources.


## dashboard:http
Configuration for the dashboard listener (plaintext).


**Config paths**

 - <code>dashboard.listeners.http</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS__HTTP</code>



**Fields**

- bind: <code>emqx_schema:ip_port()</code>
  * default: 
  `0`

  Port without IP(18083) or port with specified IP(127.0.0.1:18083).
  Disabled when setting bind to `0`.

- num_acceptors: <code>integer()</code>
  * default: 
  `16`

  Socket acceptor pool size for TCP protocols. Default is the number of schedulers online

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
  `10s`

  Send timeout for the socket.

- inet6: <code>boolean()</code>
  * default: 
  `false`

  Enable IPv6 support, default is false, which means IPv4 only.

- ipv6_v6only: <code>boolean()</code>
  * default: 
  `false`

  Disable IPv4-to-IPv6 mapping for the listener.
  The configuration is only valid when the inet6 is true.

- proxy_header: <code>boolean()</code>
  * default: 
  `false`

  Enable support for `HAProxy` header. Be aware once enabled regular HTTP requests can't be handled anymore.


## dashboard:https
Configuration for the dashboard listener (TLS).


**Config paths**

 - <code>dashboard.listeners.https</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS__HTTPS</code>



**Fields**

- bind: <code>emqx_schema:ip_port()</code>
  * default: 
  `0`

  Port without IP(18083) or port with specified IP(127.0.0.1:18083).
  Disabled when setting bind to `0`.

- ssl_options: <code>[dashboard:ssl_options](#dashboard-ssl_options)</code>

  SSL/TLS options for the dashboard listener.

- num_acceptors: <code>integer()</code>
  * default: 
  `16`

  Socket acceptor pool size for TCP protocols. Default is the number of schedulers online

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
  `10s`

  Send timeout for the socket.

- inet6: <code>boolean()</code>
  * default: 
  `false`

  Enable IPv6 support, default is false, which means IPv4 only.

- ipv6_v6only: <code>boolean()</code>
  * default: 
  `false`

  Disable IPv4-to-IPv6 mapping for the listener.
  The configuration is only valid when the inet6 is true.

- proxy_header: <code>boolean()</code>
  * default: 
  `false`

  Enable support for `HAProxy` header. Be aware once enabled regular HTTP requests can't be handled anymore.


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


## dashboard:ssl_options
SSL/TLS options for the dashboard listener.


**Config paths**

 - <code>dashboard.listeners.https.ssl_options</code>


**Env overrides**

 - <code>EMQX_DASHBOARD__LISTENERS__HTTPS__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM format private key file.

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification.

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.

- password: <code>string()</code>

  String containing the user's password. Only used if the private key file is password-protected.

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

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
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.

- dhfile: <code>string()</code>

  Path to a file containing PEM-encoded Diffie-Hellman parameters
  to be used by the server if a cipher suite using Diffie-Hellman
  key exchange is negotiated. If not specified, default parameters
  are used.<br/>
  NOTE: The <code>dhfile</code> option is not supported by TLS 1.3.

- honor_cipher_order: <code>boolean()</code>
  * default: 
  `true`

  An important security setting. It forces the cipher to be set based
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
  the number of messages the underlying cipher suite can encipher.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  Maximum time duration allowed for the handshake to complete


## emqx:cluster_dns
Service discovery via DNS SRV records.


**Config paths**

 - <code>cluster.dns</code>


**Env overrides**

 - <code>EMQX_CLUSTER__DNS</code>



**Fields**

- name: <code>string()</code>
  * default: 
  `localhost`

  The domain name from which to discover peer EMQX nodes' IP addresses.
  Applicable when <code>cluster.discovery_strategy = dns</code>

- record_type: <code>a | srv</code>
  * default: 
  `a`

  DNS record type.


## emqx:cluster_etcd
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
  `emqxcl`

  Key prefix used for EMQX service discovery.

- node_ttl: <code>emqx_schema:duration()</code>
  * default: 
  `1m`

  Expiration time of the etcd key associated with the node.
  It is refreshed automatically, as long as the node is alive.

- ssl_options: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>

  Options for the TLS connection to the etcd cluster.


## emqx:cluster_k8s
Service discovery via Kubernetes API server.


**Config paths**

 - <code>cluster.k8s</code>


**Env overrides**

 - <code>EMQX_CLUSTER__K8S</code>



**Fields**

- apiserver: <code>string()</code>
  * default: 
  `"https://kubernetes.default.svc:443"`

  Kubernetes API endpoint URL.

- service_name: <code>string()</code>
  * default: 
  `emqx`

  EMQX broker service name.

- address_type: <code>ip | dns | hostname</code>
  * default: 
  `ip`

  Address type used for connecting to the discovered nodes.
  Setting <code>cluster.k8s.address_type</code> to <code>ip</code> will
  make EMQX to discover IP addresses of peer nodes from Kubernetes API.

- namespace: <code>string()</code>
  * default: 
  `default`

  Kubernetes namespace.

- suffix: <code>string()</code>
  * default: 
  `pod.local`

  Node name suffix.<br/>
  Note: this parameter is only relevant when <code>address_type</code> is <code>dns</code>
  or <code>hostname</code>.


## emqx:cluster_static
Service discovery via static nodes.
The new node joins the cluster by connecting to one of the bootstrap nodes.


**Config paths**

 - <code>cluster.static</code>


**Env overrides**

 - <code>EMQX_CLUSTER__STATIC</code>



**Fields**

- seeds: <code>emqx_schema:comma_separated_atoms() | [atom()]</code>
  * default: 
  `[]`

  List EMQX node names in the static cluster. See <code>node.name</code>.


## emqx:authorization
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

- cache: <code>[broker:authz_cache](#broker-authz_cache)</code>



- sources: <code>[[authz:file](#authz-file) | [authz:builtin_db](#authz-builtin_db) | [authz:http_get](#authz-http_get) | [authz:http_post](#authz-http_post) | [authz:redis_single](#authz-redis_single) | [authz:redis_sentinel](#authz-redis_sentinel) | [authz:redis_cluster](#authz-redis_cluster) | [authz:mysql](#authz-mysql) | [authz:postgresql](#authz-postgresql) | [authz:mongo_single](#authz-mongo_single) | [authz:mongo_rs](#authz-mongo_rs) | [authz:mongo_sharded](#authz-mongo_sharded) | [authz:ldap](#authz-ldap)]</code>
  * default: 

  ```
  [
    {
      enable = true
      path = "${EMQX_ETC_DIR}/acl.conf"
      type = file
    }
  ]
  ```

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


## emqx:cluster
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

  Human-friendly name of the EMQX cluster.

- discovery_strategy: <code>manual | static | dns | etcd | k8s</code>
  * default: 
  `manual`

  Service discovery method for the cluster nodes. Possible values are:
  - manual: Use <code>emqx ctl cluster</code> command to manage cluster.<br/>
  - static: Configure static nodes list by setting <code>seeds</code> in config file.<br/>
  - dns: Use DNS A record to discover peer nodes.<br/>
  - etcd: Use etcd to discover peer nodes.<br/>
  - k8s: Use Kubernetes API to discover peer pods.

- autoclean: <code>emqx_schema:duration()</code>
  * default: 
  `24h`

  Remove disconnected nodes from the cluster after this interval.

- autoheal: <code>boolean()</code>
  * default: 
  `true`

  If <code>true</code>, the node will try to heal network partitions automatically.

- proto_dist: <code>inet_tcp | inet6_tcp | inet_tls | inet6_tls</code>
  * default: 
  `inet_tcp`

  The Erlang distribution protocol for the cluster.<br/>
  - inet_tcp: IPv4 TCP <br/>
  - inet_tls: IPv4 TLS, works together with <code>etc/ssl_dist.conf</code> <br/>
  - inet6_tcp: IPv6 TCP <br/>
  - inet6_tls: IPv6 TLS, works together with <code>etc/ssl_dist.conf</code>

- static: <code>[emqx:cluster_static](#emqx-cluster_static)</code>



- dns: <code>[emqx:cluster_dns](#emqx-cluster_dns)</code>



- etcd: <code>[emqx:cluster_etcd](#emqx-cluster_etcd)</code>



- k8s: <code>[emqx:cluster_k8s](#emqx-cluster_k8s)</code>




## emqx:console_handler
Log handler that prints log events to the EMQX console.


**Config paths**

 - <code>log.console</code>


**Env overrides**

 - <code>EMQX_LOG__CONSOLE</code>



**Fields**

- level: <code>debug | info | notice | warning | error | critical | alert | emergency | all</code>
  * default: 
  `warning`

  The log level for the current log handler.
  Defaults to warning.

- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable this log handler.

- formatter: <code>text | json</code>
  * default: 
  `text`

  Choose log formatter. <code>text</code> for free text, and <code>json</code> for structured logging.

- time_offset: <code>string()</code>
  * default: 
  `system`

  The time offset to be used when formatting the timestamp.
  Can be one of:
    - <code>system</code>: the time offset used by the local system
    - <code>utc</code>: the UTC time offset
    - <code>+-[hh]:[mm]</code>: user specified time offset, such as "-02:00" or "+00:00"
  Defaults to: <code>system</code>.
  This config has no effect for when formatter is <code>json</code> as the timestamp in JSON is milliseconds since epoch.


## emqx:log
EMQX supports multiple log handlers, one console handler and multiple file handlers.
EMQX by default logs to console when running in docker or in console/foreground mode,
otherwise it logs to file $EMQX_LOG_DIR/emqx.log.
For advanced configuration, you can find more parameters in this section.


**Config paths**

 - <code>log</code>


**Env overrides**

 - <code>EMQX_LOG</code>



**Fields**

- console: <code>[emqx:console_handler](#emqx-console_handler)</code>



- file: <code>[emqx:log_file_handler](#emqx-log_file_handler) | {$handler_name -> [emqx:log_file_handler](#emqx-log_file_handler)}</code>
  * default: 
  `{level = warning}`

  File-based log handlers.


## emqx:log_file_handler
Log handler that prints log events to files.


**Config paths**

 - <code>log.file</code>
 - <code>log.file.$handler_name</code>


**Env overrides**

 - <code>EMQX_LOG__FILE</code>
 - <code>EMQX_LOG__FILE__$HANDLER_NAME</code>



**Fields**

- path: <code>string()</code>
  * default: 
  `"${EMQX_LOG_DIR}/emqx.log"`

  Name the log file.

- rotation_count: <code>1..128</code>
  * default: 
  `10`

  Maximum number of log files.

- rotation_size: <code>infinity | emqx_schema:bytesize()</code>
  * default: 
  `50MB`

  This parameter controls log file rotation. The value `infinity` means the log file will grow indefinitely, otherwise the log file will be rotated once it reaches `rotation_size` in bytes.

- level: <code>debug | info | notice | warning | error | critical | alert | emergency | all</code>
  * default: 
  `warning`

  The log level for the current log handler.
  Defaults to warning.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable this log handler.

- formatter: <code>text | json</code>
  * default: 
  `text`

  Choose log formatter. <code>text</code> for free text, and <code>json</code> for structured logging.

- time_offset: <code>string()</code>
  * default: 
  `system`

  The time offset to be used when formatting the timestamp.
  Can be one of:
    - <code>system</code>: the time offset used by the local system
    - <code>utc</code>: the UTC time offset
    - <code>+-[hh]:[mm]</code>: user specified time offset, such as "-02:00" or "+00:00"
  Defaults to: <code>system</code>.
  This config has no effect for when formatter is <code>json</code> as the timestamp in JSON is milliseconds since epoch.


## emqx:node
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

  Secret cookie is a random string that should be the same on all nodes in
  the given EMQX cluster, but unique per EMQX cluster. It is used to prevent EMQX nodes that
  belong to different clusters from accidentally connecting to each other.

- max_ports: <code>1024..134217727</code>
  * default: 
  `1048576`

  Maximum number of simultaneously open files and sockets for this Erlang system.
  For more information, see: https://www.erlang.org/doc/man/erl.html

- dist_buffer_size: <code>1..2097151</code>
  * default: 
  `8192`

  Erlang's distribution buffer busy limit in kilobytes.

- data_dir: <code>string()</code>

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

- global_gc_interval: <code>disabled | emqx_schema:duration()</code>
  * default: 
  `15m`

  Periodic garbage collection interval. Set to <code>disabled</code> to have it disabled.

- role: <code>core | replicant</code>
  * default: 
  `core`

  Select a node role.<br/>
  <code>core</code> nodes provide durability of the data, and take care of writes.
  It is recommended to place core nodes in different racks or different availability zones.<br/>
  <code>replicant</code> nodes are ephemeral worker nodes. Removing them from the cluster
  doesn't affect database redundancy<br/>
  It is recommended to have more replicant nodes than core nodes.<br/>
  Note: this parameter only takes effect when the <code>backend</code> is set
  to <code>rlog</code>.


## emqx:rpc
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

- protocol: <code>tcp | ssl</code>
  * default: 
  `tcp`

  Transport protocol used for inter-broker communication

- async_batch_size: <code>integer()</code>
  * default: 
  `256`

  The maximum number of batch messages sent in asynchronous mode.
        Note that this configuration does not work in synchronous mode.

- port_discovery: <code>manual | stateless</code>
  * default: 
  `stateless`

  <code>manual</code>: discover ports by <code>tcp_server_port</code>.<br/>
  <code>stateless</code>: discover ports in a stateless manner, using the following algorithm.
  If node name is <code>emqxN@127.0.0.1</code>, where the N is an integer,
  then the listening port will be 5370 + N.

- tcp_server_port: <code>integer()</code>
  * default: 
  `5369`

  Listening port used by RPC local service.<br/>
  Note that this config only takes effect when rpc.port_discovery is set to manual.

- ssl_server_port: <code>integer()</code>
  * default: 
  `5369`

  Listening port used by RPC local service.<br/>
  Note that this config only takes effect when rpc.port_discovery is set to manual
  and <code>driver</code> is set to <code>ssl</code>.

- tcp_client_num: <code>1..256</code>
  * default: 
  `10`

  Set the maximum number of RPC communication channels initiated by this node to each remote node.

- connect_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Timeout for establishing an RPC connection.

- certfile: <code>string()</code>

  Path to TLS certificate file used to validate identity of the cluster nodes.
  Note that this config only takes effect when <code>rpc.driver</code> is set to <code>ssl</code>.

- keyfile: <code>string()</code>

  Path to the private key file for the <code>rpc.certfile</code>.<br/>
  Note: contents of this file are secret, so it's necessary to set permissions to 600.

- cacertfile: <code>string()</code>

  Path to certification authority TLS certificate file used to validate <code>rpc.certfile</code>.<br/>
  Note: certificates of all nodes in the cluster must be signed by the same CA.

- send_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Timeout for sending the RPC request.

- authentication_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Timeout for the remote node authentication.

- call_receive_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  Timeout for the reply to a synchronous RPC.

- socket_keepalive_idle: <code>emqx_schema:timeout_duration_s()</code>
  * default: 
  `15m`

  How long the connections between the brokers should remain open after the last message is sent.

- socket_keepalive_interval: <code>emqx_schema:timeout_duration_s()</code>
  * default: 
  `75s`

  The interval between keepalive messages.

- socket_keepalive_count: <code>integer()</code>
  * default: 
  `9`

  How many times the keepalive probe message can fail to receive a reply
  until the RPC connection is considered lost.

- socket_sndbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  TCP tuning parameters. TCP sending buffer size.

- socket_recbuf: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  TCP tuning parameters. TCP receiving buffer size.

- socket_buffer: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  TCP tuning parameters. Socket buffer size in user mode.

- insecure_fallback: <code>boolean()</code>
  * default: 
  `true`

  Enable compatibility with old RPC authentication.

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
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- tls_versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

  All TLS/DTLS versions to be supported.<br/>
  NOTE: PSK ciphers are suppressed by 'tlsv1.3' version config.<br/>
  In case PSK cipher suites are intended, make sure to configure
  <code>['tlsv1.2', 'tlsv1.1']</code> here.

- listen_address: <code>string()</code>
  * default: 
  `"0.0.0.0"`

  Indicates the IP address for the RPC server to listen on. For example, use <code>"0.0.0.0"</code> for IPv4 or <code>"::"</code> for IPv6.

- ipv6_only: <code>boolean()</code>
  * default: 
  `false`

  This setting is effective only when <code>rpc.listen_address</code> is assigned an IPv6 address.
  If set to <code>true</code>, the RPC client will exclusively use IPv6 for connections.
  Otherwise, the client might opt for IPv4, even if the server is on IPv6.


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

- request_timeout: <code>emqx_schema:timeout_duration()</code>
  * default: 
  `5s`

  The timeout of request gRPC server

- failed_action: <code>deny | ignore</code>
  * default: 
  `deny`

  The value that is returned when the request to the gRPC server fails for any reason

- ssl: <code>[exhook:ssl_conf](#exhook-ssl_conf)</code>



- socket_options: <code>[exhook:socket_options](#exhook-socket_options)</code>
  * default: 
  `{keepalive = true, nodelay = true}`



- auto_reconnect: <code>false | emqx_schema:timeout_duration()</code>
  * default: 
  `60s`

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

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

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

  Enable TLS session reuse.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.

- password: <code>string()</code>

  String containing the user's password. Only used if the private key file is password-protected.

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

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
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.

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
  to establish the connection, unless it is IP address used.<br/>
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


## gateway:dtls_listener
Settings for DTLS listener.


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

- bind: <code>emqx_gateway_schema:ip_port()</code>

  The IP address and port that the listener will bind.

- max_connections: <code>pos_integer() | infinity</code>
  * default: 
  `1024`

  Maximum number of concurrent connections.

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  Maximum connections per second.

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener.
  When set to <code>false</code> clients will be allowed to connect without authentication.

- mountpoint: <code>binary()</code>

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
  The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
  then the client actually subscribes to the topic `some_tenant/t`.
  Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
  the message is routed to all the clients subscribed `some_tenant/t`,
  so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Supported placeholders in mountpoint string:<br/>
    - <code>${clientid}</code>: clientid<br/>
    - <code>${username}</code>: username<br/>
    - <code>${endpoint_name}</code>: endpoint name

- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  The access control rules for this listener.
  See: https://github.com/emqtt/esockd#allowdeny

- dtls_options: <code>[gateway:dtls_opts](#gateway-dtls_opts)</code>

  DTLS socket options


## gateway:dtls_opts
Settings for DTLS protocol.


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
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM format private key file.

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification.

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.

- password: <code>string()</code>

  String containing the user's password. Only used if the private key file is password-protected.

- versions: <code>[atom()]</code>
  * default: 
  `[dtlsv1.2]`

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
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.

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

  An important security setting. It forces the cipher to be set based
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
  the number of messages the underlying cipher suite can encipher.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  Maximum time duration allowed for the handshake to complete

- gc_after_handshake: <code>boolean()</code>
  * default: 
  `false`

  Memory usage tuning. If enabled, will immediately perform a garbage collection after the TLS/SSL handshake.

- ocsp: <code>[broker:ocsp](#broker-ocsp)</code>



- enable_crl_check: <code>boolean()</code>
  * default: 
  `false`

  Whether to enable CRL verification for this listener.


## gateway
EMQX Gateway configuration root.


**Config paths**

 - <code>gateway</code>


**Env overrides**

 - <code>EMQX_GATEWAY</code>



**Fields**

- lwm2m: <code>[gateway:lwm2m](#gateway-lwm2m)</code>



- coap: <code>[gateway:coap](#gateway-coap)</code>



- mqttsn: <code>[gateway:mqttsn](#gateway-mqttsn)</code>



- exproto: <code>[gateway:exproto](#gateway-exproto)</code>



- stomp: <code>[gateway:stomp](#gateway-stomp)</code>




## gateway:ssl_listener
Settings for SSL listener.


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
  `3s`

  Timeout for proxy protocol.
  EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable the listener.

- bind: <code>emqx_gateway_schema:ip_port()</code>

  The IP address and port that the listener will bind.

- max_connections: <code>pos_integer() | infinity</code>
  * default: 
  `1024`

  Maximum number of concurrent connections.

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  Maximum connections per second.

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener.
  When set to <code>false</code> clients will be allowed to connect without authentication.

- mountpoint: <code>binary()</code>

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
  The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
  then the client actually subscribes to the topic `some_tenant/t`.
  Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
  the message is routed to all the clients subscribed `some_tenant/t`,
  so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Supported placeholders in mountpoint string:<br/>
    - <code>${clientid}</code>: clientid<br/>
    - <code>${username}</code>: username<br/>
    - <code>${endpoint_name}</code>: endpoint name

- access_rules: <code>[string()]</code>
  * default: 
  `[]`

  The access control rules for this listener.
  See: https://github.com/emqtt/esockd#allowdeny

- ssl_options: <code>[broker:listener_ssl_opts](#broker-listener_ssl_opts)</code>

  SSL Socket options.


## gateway:tcp_listener
Settings for TCP listener.


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
  `3s`

  Timeout for proxy protocol.
  EMQX will close the TCP connection if proxy protocol packet is not received within the timeout.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable the listener.

- bind: <code>emqx_gateway_schema:ip_port()</code>

  The IP address and port that the listener will bind.

- max_connections: <code>pos_integer() | infinity</code>
  * default: 
  `1024`

  Maximum number of concurrent connections.

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  Maximum connections per second.

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener.
  When set to <code>false</code> clients will be allowed to connect without authentication.

- mountpoint: <code>binary()</code>

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
  The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
  then the client actually subscribes to the topic `some_tenant/t`.
  Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
  the message is routed to all the clients subscribed `some_tenant/t`,
  so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Supported placeholders in mountpoint string:<br/>
    - <code>${clientid}</code>: clientid<br/>
    - <code>${username}</code>: username<br/>
    - <code>${endpoint_name}</code>: endpoint name

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

  A map from listener names to listener settings.

- ssl: <code>{$name -> [gateway:ssl_listener](#gateway-ssl_listener)}</code>

  A map from listener names to listener settings.


## gateway:tcp_udp_listeners
Settings for TCP and UDP listeners.


**Config paths**

 - <code>gateway.exproto.listeners</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__LISTENERS</code>



**Fields**

- tcp: <code>{$name -> [gateway:tcp_listener](#gateway-tcp_listener)}</code>

  A map from listener names to listener settings.

- ssl: <code>{$name -> [gateway:ssl_listener](#gateway-ssl_listener)}</code>

  A map from listener names to listener settings.

- udp: <code>{$name -> [gateway:udp_listener](#gateway-udp_listener)}</code>

  A map from listener names to listener settings.

- dtls: <code>{$name -> [gateway:dtls_listener](#gateway-dtls_listener)}</code>

  A map from listener names to listener settings.


## gateway:udp_listener
Settings for UDP listener.


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

- bind: <code>emqx_gateway_schema:ip_port()</code>

  The IP address and port that the listener will bind.

- max_connections: <code>pos_integer() | infinity</code>
  * default: 
  `1024`

  Maximum number of concurrent connections.

- max_conn_rate: <code>integer()</code>
  * default: 
  `1000`

  Maximum connections per second.

- enable_authn: <code>boolean()</code>
  * default: 
  `true`

  Set <code>true</code> (default) to enable client authentication on this listener.
  When set to <code>false</code> clients will be allowed to connect without authentication.

- mountpoint: <code>binary()</code>

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
  The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
  then the client actually subscribes to the topic `some_tenant/t`.
  Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
  the message is routed to all the clients subscribed `some_tenant/t`,
  so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Supported placeholders in mountpoint string:<br/>
    - <code>${clientid}</code>: clientid<br/>
    - <code>${username}</code>: username<br/>
    - <code>${endpoint_name}</code>: endpoint name

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

  A map from listener names to listener settings.

- dtls: <code>{$name -> [gateway:dtls_listener](#gateway-dtls_listener)}</code>

  A map from listener names to listener settings.


## gateway:udp_opts
Settings for UDP sockets.


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


## gateway:lwm2m
The LwM2M protocol gateway.


**Config paths**

 - <code>gateway.lwm2m</code>


**Env overrides**

 - <code>EMQX_GATEWAY__LWM2M</code>



**Fields**

- xml_dir: <code>binary()</code>

  The Directory for LwM2M Resource definition.

- lifetime_min: <code>emqx_lwm2m_schema:duration()</code>
  * default: 
  `15s`

  Minimum value of lifetime allowed to be set by the LwM2M client.

- lifetime_max: <code>emqx_lwm2m_schema:duration()</code>
  * default: 
  `86400s`

  Maximum value of lifetime allowed to be set by the LwM2M client.

- qmode_time_window: <code>emqx_lwm2m_schema:duration_s()</code>
  * default: 
  `22s`

  The value of the time window during which the network link is considered valid by the LwM2M Gateway in QMode mode.
  For example, after receiving an update message from a client, any messages within this time window are sent directly to the LwM2M client, and all messages beyond this time window are temporarily stored in memory.

- auto_observe: <code>boolean()</code>
  * default: 
  `false`

  Automatically observe the object list of REGISTER packet.

- update_msg_publish_condition: <code>always | contains_object_list</code>
  * default: 
  `contains_object_list`

  Policy for publishing UPDATE event message.<br/>
    - always: send update events as long as the UPDATE request is received.<br/>
    - contains_object_list: send update events only if the UPDATE request carries any Object List

- translators: <code>[gateway:lwm2m_translators](#gateway-lwm2m_translators)</code>

  Topic configuration for LwM2M's gateway publishing and subscription.

- mountpoint: <code>binary()</code>
  * default: 
  `"lwm2m/${endpoint_name}/"`

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
  The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
  then the client actually subscribes to the topic `some_tenant/t`.
  Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
  the message is routed to all the clients subscribed `some_tenant/t`,
  so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Supported placeholders in mountpoint string:<br/>
    - <code>${clientid}</code>: clientid<br/>
    - <code>${username}</code>: username<br/>
    - <code>${endpoint_name}</code>: endpoint name

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>



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
  `30s`

  The idle time of the client connection process. It has two purposes:
    1. A newly created client process that does not receive any client requests after that time will be closed directly.
    2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo override.


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


## opentelemetry
Open Telemetry Toolkit configuration


**Config paths**

 - <code>opentelemetry</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY</code>



**Fields**

- metrics: <code>[opentelemetry:otel_metrics](#opentelemetry-otel_metrics)</code>

  Open Telemetry Metrics configuration.

- logs: <code>[opentelemetry:otel_logs](#opentelemetry-otel_logs)</code>

  Open Telemetry Logs configuration. If enabled, EMQX installs a log handler that formats events according to Open Telemetry log data model and
  exports them to the configured Open Telemetry collector or backend.

- traces: <code>[opentelemetry:otel_traces](#opentelemetry-otel_traces)</code>

  Open Telemetry Traces configuration.

- exporter: <code>[opentelemetry:otel_exporter](#opentelemetry-otel_exporter)</code>

  Open Telemetry Exporter


## opentelemetry:otel_exporter
Open Telemetry Exporter


**Config paths**

 - <code>opentelemetry.exporter</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY__EXPORTER</code>



**Fields**

- endpoint: <code>emqx_schema:url()</code>
  * default: 
  `"http://localhost:4317"`

  The target URL to which the exporter is going to send Open Telemetry signal data.

- ssl_options: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL configuration for the Open Telemetry exporter


## opentelemetry:otel_logs
Open Telemetry Logs configuration. If enabled, EMQX installs a log handler that formats events according to Open Telemetry log data model and
exports them to the configured Open Telemetry collector or backend.


**Config paths**

 - <code>opentelemetry.logs</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY__LOGS</code>



**Fields**

- level: <code>debug | info | notice | warning | error | critical | alert | emergency | all</code>
  * default: 
  `warning`

  The log level of the Open Telemetry log handler.

- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable or disable Open Telemetry signal.

- scheduled_delay: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `1s`

  The delay interval between two consecutive exports of Open Telemetry signals.


## opentelemetry:otel_metrics
Open Telemetry Metrics configuration.


**Config paths**

 - <code>opentelemetry.metrics</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY__METRICS</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable or disable Open Telemetry signal.

- interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  The delay interval between two consecutive exports of Open Telemetry signals.


## opentelemetry:otel_traces
Open Telemetry Traces configuration.


**Config paths**

 - <code>opentelemetry.traces</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY__TRACES</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable or disable Open Telemetry signal.

- scheduled_delay: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  The delay interval between two consecutive exports of Open Telemetry signals.

- filter: <code>[opentelemetry:trace_filter](#opentelemetry-trace_filter)</code>

  Open Telemetry Trace Filter configuration


## opentelemetry:trace_filter
Open Telemetry Trace Filter configuration


**Config paths**

 - <code>opentelemetry.traces.filter</code>


**Env overrides**

 - <code>EMQX_OPENTELEMETRY__TRACES__FILTER</code>



**Fields**

- trace_all: <code>boolean()</code>
  * default: 
  `false`

  If enabled, all published messages are traced, a new trace ID is generated if it can't be extracted from the message.
  Otherwise, only messages published with trace context are traced. Disabled by default.


## prometheus:collectors
The internal advanced metrics of the virtual machine are initially disabled
and are usually only enabled during performance testing.
Enabling them will increase the CPU load.


**Config paths**

 - <code>prometheus.collectors</code>


**Env overrides**

 - <code>EMQX_PROMETHEUS__COLLECTORS</code>



**Fields**

- vm_dist: <code>disabled | enabled</code>
  * default: 
  `disabled`

  Enable or disable VM distribution collector,
  collects information about the sockets and processes involved in the Erlang distribution mechanism.

- mnesia: <code>enabled | disabled</code>
  * default: 
  `disabled`

  Collects Mnesia metrics mainly using <code> mnesia:system_info/1 </code>

- vm_statistics: <code>enabled | disabled</code>
  * default: 
  `disabled`

  Enable or disable VM statistics collector.

- vm_system_info: <code>enabled | disabled</code>
  * default: 
  `disabled`

  Enable or disable VM system info collector.

- vm_memory: <code>enabled | disabled</code>
  * default: 
  `disabled`

  Collects information about memory dynamically allocated by the Erlang emulator using
  <code> erlang:memory/0 </code>.

- vm_msacc: <code>enabled | disabled</code>
  * default: 
  `disabled`

  Enable or disable VM microstate accounting metrics collector.


## prometheus:legacy_deprecated_setting
Deprecated since 5.4.0


**Config paths**

 - <code>prometheus</code>


**Env overrides**

 - <code>EMQX_PROMETHEUS</code>



**Fields**

- push_gateway_server: <code>string()</code>
  * default: 
  `"http://127.0.0.1:9091"`

  Deprecated since 5.4.0, use `prometheus.push_gateway.url` instead

- interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  Deprecated since 5.4.0, use `prometheus.push_gateway.interval` instead

- headers: <code>map(string(), string())</code>
  * default: 
  `{}`

  Deprecated since 5.4.0, use `prometheus.push_gateway.headers` instead

- job_name: <code>binary()</code>
  * default: 
  `"${name}/instance/${name}~${host}"`

  Deprecated since 5.4.0, use `prometheus.push_gateway.job_name` instead

- enable: <code>boolean()</code>
  * default: 
  `false`

  Deprecated since 5.4.0, use `prometheus.push_gateway.url` instead

- vm_dist_collector: <code>disabled | enabled</code>
  * default: 
  `disabled`

  Deprecated since 5.4.0, use `prometheus.collectors.vm_dist` instead

- mnesia_collector: <code>enabled | disabled</code>
  * default: 
  `disabled`

  Deprecated since 5.4.0, use `prometheus.collectors.mnesia` instead

- vm_statistics_collector: <code>enabled | disabled</code>
  * default: 
  `disabled`

  Deprecated since 5.4.0, use `prometheus.collectors.vm_statistics` instead

- vm_system_info_collector: <code>enabled | disabled</code>
  * default: 
  `disabled`

  Deprecated, use `prometheus.collectors.vm_system_info` instead

- vm_memory_collector: <code>enabled | disabled</code>
  * default: 
  `disabled`

  Deprecated since 5.4.0, use `prometheus.collectors.vm_memory` instead

- vm_msacc_collector: <code>enabled | disabled</code>
  * default: 
  `disabled`

  Deprecated since 5.4.0, use `prometheus.collectors.vm_msacc` instead


## prometheus:push_gateway
Push Gateway is optional, should not be configured if prometheus is to scrape EMQX.


**Config paths**

 - <code>prometheus.push_gateway</code>


**Env overrides**

 - <code>EMQX_PROMETHEUS__PUSH_GATEWAY</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `false`

  Enable or disable Pushgateway

- url: <code>string()</code>
  * default: 
  `"http://127.0.0.1:9091"`

  URL of Pushgateway server. Pushgateway is optional, should not be configured if prometheus is to scrape EMQX.

- interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  Data reporting interval

- headers: <code>map(string(), string())</code>
  * default: 
  `{}`

  An HTTP Headers when pushing to Push Gateway.<br/>
  For example, <code> { Authorization = "some-authz-tokens"}</code>

- job_name: <code>binary()</code>
  * default: 
  `"${name}/instance/${name}~${host}"`

  Job Name that is pushed to the Push Gateway. Available variables:<br/>
  - ${name}: Name of EMQX node.<br/>
  - ${host}: Host name of EMQX node.<br/>
  For example, when the EMQX node name is <code>emqx@127.0.0.1</code> then the <code>name</code>
  variable takes value <code>emqx</code> and the <code>host</code> variable takes value <code>127.0.0.1</code>.
  Default value is: <code>${name}/instance/${name}~${host}</code>


## prometheus:recommend_setting
Recommended setting


**Config paths**

 - <code>prometheus</code>


**Env overrides**

 - <code>EMQX_PROMETHEUS</code>



**Fields**

- enable_basic_auth: <code>boolean()</code>
  * default: 
  `false`

  Enable or disable basic authentication for prometheus scrape api, not for Push Gateway

- push_gateway: <code>[prometheus:push_gateway](#prometheus-push_gateway)</code>

  Push Gateway is optional, should not be configured if prometheus is to scrape EMQX.

- collectors: <code>[prometheus:collectors](#prometheus-collectors)</code>

  The internal advanced metrics of the virtual machine are initially disabled
  and are usually only enabled during performance testing.
  Enabling them will increase the CPU load.


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
  `0s`

  Message retention time. This config is only applicable for messages without the Message Expiry Interval message property.
  0 means message will never expire.

- msg_clear_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `0s`

  Interval for EMQX to scan expired messages and delete them. Never scan if the value is 0.

- max_payload_size: <code>emqx_schema:bytesize()</code>
  * default: 
  `1MB`

  Maximum retained message size.

- stop_publish_clear_msg: <code>boolean()</code>
  * default: 
  `false`

  When the retained flag of the `PUBLISH` message is set and Payload is empty,
  whether to continue to publish the message.
  See:
  http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718038

- delivery_rate: <code>string()</code>
  * default: 
  `"1000/s"`

  The maximum rate of delivering retained messages

- backend: <code>[retainer:mnesia_config](#retainer-mnesia_config)</code>

  Settings for the database storing the retained messages.


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

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  Descriptive text.

- resource_opts: <code>[bridge_mqtt:creation_opts](#bridge_mqtt-creation_opts)</code>
  * default: 
  `{}`

  Resource options.

- mode: <code>cluster_shareload</code>

  Deprecated since v5.1.0 & e5.1.0.

- server: <code>string()</code>

  The host and port of the remote MQTT broker

- clientid_prefix: <code>binary()</code>

  Optional prefix to prepend to the clientid used by egress bridges.

- reconnect_interval: <code>string()</code>

  Deprecated since v5.0.16.

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
  If bridge_mode is set to true, the bridge will indicate to the remote broker that it is a bridge not an ordinary client.
  This means that loop detection will be more effective and that retained messages will be propagated correctly.

- username: <code>binary()</code>

  The username of the MQTT protocol

- password: <code>emqx_schema_secret:secret()</code>

  The password of the MQTT protocol

- clean_start: <code>boolean()</code>
  * default: 
  `true`

  Whether to start a clean session when reconnecting a remote broker for ingress bridge

- keepalive: <code>string()</code>
  * default: 
  `300s`

  MQTT Keepalive. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  <br/>or combination of whereof: `1h5m0s`

- retry_interval: <code>string()</code>
  * default: 
  `15s`

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

- ingress: <code>[connector_mqtt:ingress](#connector_mqtt-ingress)</code>

  The ingress config defines how this bridge receive messages from the remote MQTT broker, and then
          send them to the local broker.<br/>
          Template with variables is allowed in 'remote.qos', 'local.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
          NOTE: if this bridge is used as the input of a rule, and also 'local.topic' is
          configured, then messages got from the remote broker will be sent to both the 'local.topic' and
          the rule.

- egress: <code>[connector_mqtt:egress](#connector_mqtt-egress)</code>

  The egress config defines how this bridge forwards messages from the local broker to the remote broker.<br/>
  Template with variables is allowed in 'remote.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
  NOTE: if this bridge is used as the action of a rule, and also 'local.topic'
  is configured, then both the data got from the rule and the MQTT messages that matches
  'local.topic' will be forwarded.


## actions_and_sources:actions
Configuration for actions.


**Config paths**

 - <code>actions</code>


**Env overrides**

 - <code>EMQX_ACTIONS</code>



**Fields**

- http: <code>{$name -> [bridge_http:http_action](#bridge_http-http_action)}</code>

  HTTP Action Config

- mqtt: <code>{$name -> [bridge_mqtt_publisher:mqtt_publisher_action](#bridge_mqtt_publisher-mqtt_publisher_action)}</code>

  MQTT Publisher Action Config


## actions_and_sources:sources
Configuration for sources.


**Config paths**

 - <code>sources</code>


**Env overrides**

 - <code>EMQX_SOURCES</code>



**Fields**

- mqtt: <code>{$name -> [bridge_mqtt_publisher:mqtt_subscriber_source](#bridge_mqtt_publisher-mqtt_subscriber_source)}</code>

  MQTT Subscriber Source Config


## authn:http_get
Configuration of authenticator using HTTP Server as authentication service (Using GET request).


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- method: <code>get</code>

  HTTP request method.

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    keep-alive = "timeout=30, max=1000"
  }
  ```

  List of HTTP headers (without <code>content-type</code>).

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>http</code>

  Backend type.

- url: <code>binary()</code>

  URL of the HTTP server.

- body: <code>map()</code>

  HTTP request body.

- request_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `5s`

  HTTP request timeout.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- request: <code>[connector_http:request](#connector_http-request)</code>

  Configure HTTP request parameters.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  The pool size.

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  The timeout when connecting to the HTTP server.

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.


## authn:http_post
Configuration of authenticator using HTTP Server as authentication service (Using POST request).


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- method: <code>post</code>

  HTTP request method.

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    content-type = "application/json"
    keep-alive = "timeout=30, max=1000"
  }
  ```

  List of HTTP Headers.

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>http</code>

  Backend type.

- url: <code>binary()</code>

  URL of the HTTP server.

- body: <code>map()</code>

  HTTP request body.

- request_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `5s`

  HTTP request timeout.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- request: <code>[connector_http:request](#connector_http-request)</code>

  Configure HTTP request parameters.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  The pool size.

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  The timeout when connecting to the HTTP server.

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.


## authn:jwt_hmac
Configuration when the JWT for authentication is issued using the HMAC algorithm.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

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
  `acl`

  The JWT claim designated for accessing ACL (Access Control List) rules can be specified,
  such as using the `acl` claim. A typical decoded JWT with this claim might appear as:
  `{"username": "user1", "acl": ...}`.

  Supported ACL Rule Formats:

  - Object Format:
    Utilizes action types pub (publish), sub (subscribe), or all (both publish and subscribe).
    The value is a list of topic filters.
    Example: `{"pub": ["topic1"], "sub": [], "all": ["${username}/#"]}`.
    This example signifies that the token owner can publish to topic1 and perform both publish and subscribe
    actions on topics starting with their username.
    Note: In this format, if no topic matches, the action is denied, and the authorization process terminates.

  - Array Format (resembles File-Based ACL Rules):
    Example: `[{"permission": "allow", "action": "all", "topic": "${username}/#"}]`.
    Additionally, the `pub` or `publish` action rules can be extended with `qos` and `retain` field,
    and `sub` or `subscribe` action rules can be extended with a `qos` field.
    Note: Here, if no rule matches, the action is not immediately denied.
    The process continues to other configured authorization sources,
    and ultimately falls back to the default permission in config `authorization.no_match`.

  The ACL claim utilizes MQTT topic wildcard matching rules for publishing or subscribing.
  A special syntax for the 'subscribe' action allows the use of `eq` for an exact match.
  For instance, `eq t/#` permits or denies subscription to `t/#`, but not to `t/1`.

- verify_claims: <code>map()</code>
  * default: 
  `[]`

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


## authn:jwt_jwks
Configuration when JWTs used for authentication need to be fetched from the JWKS endpoint.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- use_jwks: <code>true</code>

  Whether to use JWKS.

- endpoint: <code>string()</code>

  JWKS endpoint, it's a read-only endpoint that returns the server's public key set in the JWKS format.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

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
  `acl`

  The JWT claim designated for accessing ACL (Access Control List) rules can be specified,
  such as using the `acl` claim. A typical decoded JWT with this claim might appear as:
  `{"username": "user1", "acl": ...}`.

  Supported ACL Rule Formats:

  - Object Format:
    Utilizes action types pub (publish), sub (subscribe), or all (both publish and subscribe).
    The value is a list of topic filters.
    Example: `{"pub": ["topic1"], "sub": [], "all": ["${username}/#"]}`.
    This example signifies that the token owner can publish to topic1 and perform both publish and subscribe
    actions on topics starting with their username.
    Note: In this format, if no topic matches, the action is denied, and the authorization process terminates.

  - Array Format (resembles File-Based ACL Rules):
    Example: `[{"permission": "allow", "action": "all", "topic": "${username}/#"}]`.
    Additionally, the `pub` or `publish` action rules can be extended with `qos` and `retain` field,
    and `sub` or `subscribe` action rules can be extended with a `qos` field.
    Note: Here, if no rule matches, the action is not immediately denied.
    The process continues to other configured authorization sources,
    and ultimately falls back to the default permission in config `authorization.no_match`.

  The ACL claim utilizes MQTT topic wildcard matching rules for publishing or subscribing.
  A special syntax for the 'subscribe' action allows the use of `eq` for an exact match.
  For instance, `eq t/#` permits or denies subscription to `t/#`, but not to `t/1`.

- verify_claims: <code>map()</code>
  * default: 
  `[]`

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


## authn:jwt_public_key
Configuration when the JWT for authentication is issued using RSA or ECDSA algorithm.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- algorithm: <code>public-key</code>

  JWT signing algorithm, Supports HMAC (configured as <code>hmac-based</code>) and RSA, ECDSA (configured as <code>public-key</code>).

- public_key: <code>string()</code>

  The public key used to verify the JWT.

- mechanism: <code>jwt</code>

  Authentication mechanism.

- acl_claim_name: <code>binary()</code>
  * default: 
  `acl`

  The JWT claim designated for accessing ACL (Access Control List) rules can be specified,
  such as using the `acl` claim. A typical decoded JWT with this claim might appear as:
  `{"username": "user1", "acl": ...}`.

  Supported ACL Rule Formats:

  - Object Format:
    Utilizes action types pub (publish), sub (subscribe), or all (both publish and subscribe).
    The value is a list of topic filters.
    Example: `{"pub": ["topic1"], "sub": [], "all": ["${username}/#"]}`.
    This example signifies that the token owner can publish to topic1 and perform both publish and subscribe
    actions on topics starting with their username.
    Note: In this format, if no topic matches, the action is denied, and the authorization process terminates.

  - Array Format (resembles File-Based ACL Rules):
    Example: `[{"permission": "allow", "action": "all", "topic": "${username}/#"}]`.
    Additionally, the `pub` or `publish` action rules can be extended with `qos` and `retain` field,
    and `sub` or `subscribe` action rules can be extended with a `qos` field.
    Note: Here, if no rule matches, the action is not immediately denied.
    The process continues to other configured authorization sources,
    and ultimately falls back to the default permission in config `authorization.no_match`.

  The ACL claim utilizes MQTT topic wildcard matching rules for publishing or subscribing.
  A special syntax for the 'subscribe' action allows the use of `eq` for an exact match.
  For instance, `eq t/#` permits or denies subscription to `t/#`, but not to `t/1`.

- verify_claims: <code>map()</code>
  * default: 
  `[]`

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


## authn:bind_method
Authenticate by the LDAP bind operation.


**Config paths**

 - <code>authentication.$INDEX.method</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__METHOD</code>



**Fields**

- type: <code>bind</code>
  * default: 
  `bind`

  Authentication method type.

- bind_password: <code>binary()</code>
  * default: 
  `"${password}"`

  The template for password to bind.


## authn:hash_method
Authenticate by comparing the hashed password which was provided by the `password attribute`.


**Config paths**

 - <code>authentication.$INDEX.method</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__METHOD</code>



**Fields**

- type: <code>hash</code>
  * default: 
  `hash`

  Authentication method type.

- password_attribute: <code>string()</code>
  * default: 
  `userPassword`

  Indicates which attribute is used to represent the user's password.

- is_superuser_attribute: <code>string()</code>
  * default: 
  `isSuperuser`

  Indicates which attribute is used to represent whether the user is a superuser.


## authn:ldap
Configuration of authenticator using LDAP as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>ldap</code>

  Backend type.

- query_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  Timeout for the LDAP query.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- server: <code>string()</code>

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The LDAP default port 389 is used if `[:Port]` is not specified.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- base_dn: <code>binary()</code>

  The name of the base object entry (or possibly the root) relative to
  which the Search is to be performed.

- filter: <code>binary()</code>
  * default: 
  `"(objectClass=mqttUser)"`

  The filter that defines the conditions that must be fulfilled in order
  for the Search to match a given entry.<br>
  The syntax of the filter follows RFC 4515 and also supports placeholders.

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  Sets the maximum time in milliseconds that is used for each individual request.

- ssl: <code>[ldap:ssl](#ldap-ssl)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- method: <code>[authn:hash_method](#authn-hash_method) | [authn:bind_method](#authn-bind_method)</code>

  Authentication method.


## authn:ldap_deprecated
This is a deprecated form, you should avoid using it.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>ldap</code>

  Backend type.

- query_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  Timeout for the LDAP query.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- server: <code>string()</code>

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The LDAP default port 389 is used if `[:Port]` is not specified.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- base_dn: <code>binary()</code>

  The name of the base object entry (or possibly the root) relative to
  which the Search is to be performed.

- filter: <code>binary()</code>
  * default: 
  `"(objectClass=mqttUser)"`

  The filter that defines the conditions that must be fulfilled in order
  for the Search to match a given entry.<br>
  The syntax of the filter follows RFC 4515 and also supports placeholders.

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  Sets the maximum time in milliseconds that is used for each individual request.

- ssl: <code>[ldap:ssl](#ldap-ssl)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- password_attribute: <code>string()</code>
  * default: 
  `userPassword`

  Indicates which attribute is used to represent the user's password.

- is_superuser_attribute: <code>string()</code>
  * default: 
  `isSuperuser`

  Indicates which attribute is used to represent whether the user is a superuser.


## authn:builtin_db
Configuration of authenticator using built-in database as data source.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- password_hash_algorithm: <code>[authn_hash:bcrypt_rw](#authn_hash-bcrypt_rw) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash creation and verification.

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>built_in_database</code>

  Backend type.

- user_id_type: <code>clientid | username</code>
  * default: 
  `username`

  Specify whether to use `clientid` or `username` for authentication.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.


## authn:mongo_rs
Configuration of authenticator using MongoDB (Replica Set) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



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
  `password_hash`

  Document field that contains password hash.

- salt_field: <code>binary()</code>
  * default: 
  `salt`

  Document field that contains the password salt.

- is_superuser_field: <code>binary()</code>
  * default: 
  `is_superuser`

  Document field that defines if the user has superuser privileges.

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
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

  Replica set. Must be set to 'rs' when MongoDB server is running in 'replica set' mode.

- servers: <code>string()</code>

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

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn:mongo_sharded
Configuration of authenticator using MongoDB (Sharded Cluster) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



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
  `password_hash`

  Document field that contains password hash.

- salt_field: <code>binary()</code>
  * default: 
  `salt`

  Document field that contains the password salt.

- is_superuser_field: <code>binary()</code>
  * default: 
  `is_superuser`

  Document field that defines if the user has superuser privileges.

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
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

  Sharded cluster. Must be set to 'sharded' when MongoDB server is running in 'sharded' mode.

- servers: <code>string()</code>

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

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn:mongo_single
Configuration of authenticator using MongoDB (Standalone) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



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
  `password_hash`

  Document field that contains password hash.

- salt_field: <code>binary()</code>
  * default: 
  `salt`

  Document field that contains the password salt.

- is_superuser_field: <code>binary()</code>
  * default: 
  `is_superuser`

  Document field that defines if the user has superuser privileges.

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
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

  Standalone instance. Must be set to 'single' when MongoDB server is running in standalone mode.

- server: <code>string()</code>

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

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[mongo:topology](#mongo-topology)</code>



- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn:mysql
Configuration of authenticator using MySQL as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>mysql</code>

  Backend type.

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- query: <code>string()</code>

  SQL used to query data for authentication, such as password hash.

- query_timeout: <code>emqx_schema:duration_ms()</code>
  * default: 
  `5s`

  Timeout for the SQL query.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- server: <code>string()</code>

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The MySQL default port 3306 is used if `[:Port]` is not specified.

- database: <code>binary()</code>

  Database name.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>
  * default: 
  `root`

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn:postgresql
Configuration of authenticator using PostgreSQL as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>postgresql</code>

  Backend type.

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- query: <code>string()</code>

  SQL used to query data for authentication, such as password hash.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- server: <code>string()</code>

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The PostgreSQL default port 5432 is used if `[:Port]` is not specified.

- database: <code>binary()</code>

  Database name.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn:redis_cluster
Configuration of authenticator using Redis (Cluster) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>redis</code>

  Backend type.

- cmd: <code>binary()</code>

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- servers: <code>string()</code>

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The Redis default port 6379 is used if `[:Port]` is not specified.

- redis_type: <code>cluster</code>
  * default: 
  `cluster`

  Cluster mode. Must be set to 'cluster' when Redis server is running in clustered mode.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn:redis_sentinel
Configuration of authenticator using Redis (Sentinel) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>redis</code>

  Backend type.

- cmd: <code>binary()</code>

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- servers: <code>string()</code>

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The Redis default port 6379 is used if `[:Port]` is not specified.

- redis_type: <code>sentinel</code>
  * default: 
  `sentinel`

  Sentinel mode. Must be set to 'sentinel' when Redis server is running in sentinel mode.

- sentinel: <code>string()</code>

  The cluster name in Redis sentinel mode.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis database ID.

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn:redis_single
Configuration of authenticator using Redis (Standalone) as authentication data source.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



**Fields**

- mechanism: <code>password_based</code>

  Authentication mechanism.

- backend: <code>redis</code>

  Backend type.

- cmd: <code>binary()</code>

  The Redis Command used to query data for authentication such as password hash, currently only supports <code>HGET</code> and <code>HMGET</code>.

- password_hash_algorithm: <code>[authn_hash:bcrypt](#authn_hash-bcrypt) | [authn_hash:pbkdf2](#authn_hash-pbkdf2) | [authn_hash:simple](#authn_hash-simple)</code>
  * default: 
  `{name = sha256, salt_position = prefix}`

  Options for password hash verification.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this auth provider.

- server: <code>string()</code>

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The Redis default port 6379 is used if `[:Port]` is not specified.

- redis_type: <code>single</code>
  * default: 
  `single`

  Single mode. Must be set to 'single' when Redis server is running in single mode.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis database ID.

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authn:scram
Settings for Salted Challenge Response Authentication Mechanism
(SCRAM) authentication.


**Config paths**

 - <code>authentication.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX</code>



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


## authn_hash:bcrypt
Settings for bcrypt password hashing algorithm.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>bcrypt</code>

  BCRYPT password hashing.


## authn_hash:bcrypt_rw
Settings for bcrypt password hashing algorithm (for DB backends with write capability).


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>bcrypt</code>

  BCRYPT password hashing.

- salt_rounds: <code>5..10</code>
  * default: 
  `10`

  Work factor for BCRYPT password generation.


## authn_hash:pbkdf2
Settings for PBKDF2 password hashing algorithm.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>pbkdf2</code>

  PBKDF2 password hashing.

- mac_fun: <code>md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512</code>

  Specifies mac_fun for PBKDF2 hashing algorithm.

- iterations: <code>pos_integer()</code>

  Iteration count for PBKDF2 hashing algorithm.

- dk_length: <code>integer()</code>

  Derived length for PBKDF2 hashing algorithm. If not specified, calculated automatically based on `mac_fun`.


## authn_hash:simple
Settings for simple algorithms.


**Config paths**

 - <code>authentication.$INDEX.password_hash_algorithm</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__PASSWORD_HASH_ALGORITHM</code>



**Fields**

- name: <code>plain | md5 | sha | sha256 | sha512</code>

  Simple password hashing algorithm.

- salt_position: <code>disable | prefix | suffix</code>
  * default: 
  `prefix`

  Salt position for PLAIN, MD5, SHA, SHA256 and SHA512 algorithms.


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
  `30s`

  HTTP request timeout.

- body: <code>{$name -> binary()}</code>

  HTTP request body.

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  The timeout when connecting to the HTTP server.

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  The pool size.

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.

- request: <code>[connector_http:request](#connector_http-request)</code>

  Configure HTTP request parameters.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- method: <code>get</code>

  HTTP method.

- headers: <code>map(binary(), binary())</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    keep-alive = "timeout=30, max=1000"
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
  `30s`

  HTTP request timeout.

- body: <code>{$name -> binary()}</code>

  HTTP request body.

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  The timeout when connecting to the HTTP server.

- max_retries: <code>non_neg_integer()</code>

  Deprecated since 5.0.4.

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  The pool size.

- enable_pipelining: <code>pos_integer()</code>
  * default: 
  `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.

- request: <code>[connector_http:request](#connector_http-request)</code>

  Configure HTTP request parameters.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- method: <code>post</code>

  HTTP method.

- headers: <code>map(binary(), binary())</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    content-type = "application/json"
    keep-alive = "timeout=30, max=1000"
  }
  ```

  List of HTTP Headers.


## authz:ldap
AuthZ with LDAP


**Config paths**

 - <code>authorization.sources.$INDEX</code>


**Env overrides**

 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX</code>



**Fields**

- type: <code>ldap</code>

  Backend type.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Set to <code>true</code> or <code>false</code> to disable this ACL provider

- publish_attribute: <code>string()</code>
  * default: 
  `mqttPublishTopic`

  Indicates which attribute is used to represent the allowed topics list of the `publish`.

- subscribe_attribute: <code>string()</code>
  * default: 
  `mqttSubscriptionTopic`

  Indicates which attribute is used to represent the allowed topics list of the `subscribe`.

- all_attribute: <code>string()</code>
  * default: 
  `mqttPubSubTopic`

  Indicates which attribute is used to represent the both allowed topics list of  `publish` and `subscribe`.

- query_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  Timeout for the LDAP query.

- server: <code>string()</code>

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The LDAP default port 389 is used if `[:Port]` is not specified.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- base_dn: <code>binary()</code>

  The name of the base object entry (or possibly the root) relative to
  which the Search is to be performed.

- filter: <code>binary()</code>
  * default: 
  `"(objectClass=mqttUser)"`

  The filter that defines the conditions that must be fulfilled in order
  for the Search to match a given entry.<br>
  The syntax of the filter follows RFC 4515 and also supports placeholders.

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `10s`

  Sets the maximum time in milliseconds that is used for each individual request.

- ssl: <code>[ldap:ssl](#ldap-ssl)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.


## authz:builtin_db
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

- collection: <code>binary()</code>

  `MongoDB` collection containing the authorization data.

- filter: <code>map()</code>
  * default: 
  `{}`

  Conditional expression that defines the filter condition in the query.
  Filter supports the following placeholders<br/>
   - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting<br/>
   - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting

- mongo_type: <code>rs</code>
  * default: 
  `rs`

  Replica set. Must be set to 'rs' when MongoDB server is running in 'replica set' mode.

- servers: <code>string()</code>

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

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[mongo:topology](#mongo-topology)</code>



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

- collection: <code>binary()</code>

  `MongoDB` collection containing the authorization data.

- filter: <code>map()</code>
  * default: 
  `{}`

  Conditional expression that defines the filter condition in the query.
  Filter supports the following placeholders<br/>
   - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting<br/>
   - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting

- mongo_type: <code>sharded</code>
  * default: 
  `sharded`

  Sharded cluster. Must be set to 'sharded' when MongoDB server is running in 'sharded' mode.

- servers: <code>string()</code>

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

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[mongo:topology](#mongo-topology)</code>



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

- collection: <code>binary()</code>

  `MongoDB` collection containing the authorization data.

- filter: <code>map()</code>
  * default: 
  `{}`

  Conditional expression that defines the filter condition in the query.
  Filter supports the following placeholders<br/>
   - <code>${username}</code>: Will be replaced at runtime with <code>Username</code> used by the client when connecting<br/>
   - <code>${clientid}</code>: Will be replaced at runtime with <code>Client ID</code> used by the client when connecting

- mongo_type: <code>single</code>
  * default: 
  `single`

  Standalone instance. Must be set to 'single' when MongoDB server is running in standalone mode.

- server: <code>string()</code>

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

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- use_legacy_protocol: <code>auto | true | false</code>
  * default: 
  `auto`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.

- auth_source: <code>binary()</code>

  Database name associated with the user's credentials.

- database: <code>binary()</code>

  Database name.

- topology: <code>[mongo:topology](#mongo-topology)</code>



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

- server: <code>string()</code>

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The MySQL default port 3306 is used if `[:Port]` is not specified.

- database: <code>binary()</code>

  Database name.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>
  * default: 
  `root`

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

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

- server: <code>string()</code>

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The PostgreSQL default port 5432 is used if `[:Port]` is not specified.

- database: <code>binary()</code>

  Database name.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

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

- servers: <code>string()</code>

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The Redis default port 6379 is used if `[:Port]` is not specified.

- redis_type: <code>cluster</code>
  * default: 
  `cluster`

  Cluster mode. Must be set to 'cluster' when Redis server is running in clustered mode.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

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

- servers: <code>string()</code>

  A Node list for Cluster to connect to. The nodes should be separated with commas, such as: `Node[,Node].`
  For each Node should be: The IPv4 or IPv6 address or the hostname to connect to.
  A host entry has the following form: `Host[:Port]`.
  The Redis default port 6379 is used if `[:Port]` is not specified.

- redis_type: <code>sentinel</code>
  * default: 
  `sentinel`

  Sentinel mode. Must be set to 'sentinel' when Redis server is running in sentinel mode.

- sentinel: <code>string()</code>

  The cluster name in Redis sentinel mode.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis database ID.

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

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

- server: <code>string()</code>

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
  A host entry has the following form: `Host[:Port]`.<br/>
  The Redis default port 6379 is used if `[:Port]` is not specified.

- redis_type: <code>single</code>
  * default: 
  `single`

  Single mode. Must be set to 'single' when Redis server is running in single mode.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the connection pool towards the bridge target service.

- username: <code>binary()</code>

  The username associated with the bridge in the external database used for authentication or identification purposes.

- password: <code>emqx_schema_secret:secret()</code>

  The password associated with the bridge, used for authentication with the external database.

- database: <code>non_neg_integer()</code>
  * default: 
  `0`

  Redis database ID.

- auto_reconnect: <code>boolean()</code>

  Deprecated since v5.0.15.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- cmd: <code>binary()</code>

  Database query used to retrieve authorization data.


## bridge:bridges
Configuration for MQTT bridges.


**Config paths**

 - <code>bridges</code>


**Env overrides**

 - <code>EMQX_BRIDGES</code>



**Fields**

- webhook: <code>{$name -> [bridge_http:config](#bridge_http-config)}</code>

  WebHook to an HTTP server.

- mqtt: <code>{$name -> [bridge_mqtt:config](#bridge_mqtt-config)}</code>

  MQTT bridges to/from another MQTT broker


## bridge_http:action_resource_opts
Resource options.


**Config paths**

 - <code>actions.http.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__HTTP__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  The number of buffer workers. Only applicable for egress type bridges.
  For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  Health check interval.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  Query mode. Optional 'sync/async', default 'async'.

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  Maximum number of bytes to buffer for each buffer worker.


## bridge_http:connector_resource_opts
Resource options.


**Config paths**

 - <code>connectors.http.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__HTTP__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  Health check interval.

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  Whether start the resource right after created.

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


## bridge_http:config
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

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  Descriptive text.

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  The timeout when connecting to the HTTP server.

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.

- pool_type: <code>random | hash</code>
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

- request: <code>map()</code>

  Deprecated since 5.3.2.

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

- direction: <code>egress</code>

  Deprecated since 5.0.12.

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
  Template with variables is allowed.

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    content-type = "application/json"
    keep-alive = "timeout=5"
  }
  ```

  The headers of the HTTP request.<br/>
  Template with variables is allowed.

- body: <code>binary()</code>

  The body of the HTTP request.<br/>
  If not provided, the body will be a JSON object of all the available fields.<br/>
  There, 'all the available fields' means the context of a MQTT message when
  this webhook is triggered by receiving a MQTT message (the `local_topic` is set),
  or the context of the event when this webhook is triggered by a rule (i.e. this
  webhook is used as an action of a rule).<br/>
  Template with variables is allowed.

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  HTTP request max retry times if failed.

- request_timeout: <code>emqx_schema:duration_ms()</code>

  Deprecated since v5.0.26.

- resource_opts: <code>[bridge_http:v1_resource_opts](#bridge_http-v1_resource_opts)</code>
  * default: 
  `{}`

  Resource options.


## bridge_http:config_connector
Configuration for an HTTP bridge.


**Config paths**

 - <code>connectors.http.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__HTTP__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable (true) or disable (false) this connector.

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  Descriptive text.

- url: <code>binary()</code>

  The URL of the HTTP Bridge.<br/>
  Template with variables is allowed in the path, but variables cannot be used in the scheme, host,
  or port part.<br/>
  For example, <code> http://localhost:9901/${topic} </code> is allowed, but
  <code> http://${host}:9901/message </code> or <code> http://localhost:${port}/message </code>
  is not allowed.

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    content-type = "application/json"
    keep-alive = "timeout=5"
  }
  ```

  The headers of the HTTP request.<br/>
  Template with variables is allowed.

- connect_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  The timeout when connecting to the HTTP server.

- retry_interval: <code>emqx_schema:timeout_duration()</code>

  Deprecated since 5.0.4.

- pool_type: <code>random | hash</code>
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

- request: <code>map()</code>

  Deprecated since 5.3.2.

- ssl: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>
  * default: 
  `{enable = false}`

  SSL connection settings.

- resource_opts: <code>[bridge_http:connector_resource_opts](#bridge_http-connector_resource_opts)</code>
  * default: 
  `{}`

  Resource options.


## bridge_http:http_action
Configuration for an HTTP bridge.


**Config paths**

 - <code>actions.http.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__HTTP__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable or disable this bridge

- connector: <code>binary()</code>

  Name of the connector specified by the action, used for external resource selection.

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  Descriptive text.

- parameters: <code>[bridge_http:parameters_opts](#bridge_http-parameters_opts)</code>

  The parameters for HTTP action.

- resource_opts: <code>[bridge_http:action_resource_opts](#bridge_http-action_resource_opts)</code>
  * default: 
  `{}`

  Resource options.


## bridge_http:parameters_opts
The parameters for HTTP action.


**Config paths**

 - <code>actions.http.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__HTTP__$NAME__PARAMETERS</code>



**Fields**

- path: <code>binary()</code>

  The URL path for this Action.<br/>
  This path will be appended to the Connector's <code>url</code> configuration to form the full
  URL address.
  Template with variables is allowed in this option. For example, <code>/room/{$room_no}</code>

- method: <code>post | put | get | delete</code>
  * default: 
  `post`

  The method of the HTTP request. All the available methods are: post, put, get, delete.<br/>
  Template with variables is allowed.

- headers: <code>map()</code>
  * default: 

  ```
  {
    accept = "application/json"
    cache-control = no-cache
    connection = keep-alive
    content-type = "application/json"
    keep-alive = "timeout=5"
  }
  ```

  The headers of the HTTP request.<br/>
  Template with variables is allowed.

- body: <code>binary()</code>

  The body of the HTTP request.<br/>
  If not provided, the body will be a JSON object of all the available fields.<br/>
  There, 'all the available fields' means the context of a MQTT message when
  this webhook is triggered by receiving a MQTT message (the `local_topic` is set),
  or the context of the event when this webhook is triggered by a rule (i.e. this
  webhook is used as an action of a rule).<br/>
  Template with variables is allowed.

- max_retries: <code>non_neg_integer()</code>
  * default: 
  `2`

  HTTP request max retry times if failed.

- request_timeout: <code>emqx_schema:duration_ms()</code>

  Deprecated since v5.0.26.


## bridge_http:v1_resource_opts
Creation options.


**Config paths**

 - <code>bridges.webhook.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__WEBHOOK__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  The number of buffer workers. Only applicable for egress type bridges.
  For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  Health check interval.

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  Whether start the resource right after created.

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  Query mode. Optional 'sync/async', default 'async'.

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  Maximum number of bytes to buffer for each buffer worker.


## bridge_mqtt:creation_opts
Creation options.


**Config paths**

 - <code>bridges.mqtt.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  The number of buffer workers. Only applicable for egress type bridges.
  For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  Health check interval.

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  Whether start the resource right after created.

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.

- auto_restart_interval: <code>infinity | emqx_schema:duration_ms()</code>

  Deprecated since 5.1.0.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  Query mode. Optional 'sync/async', default 'async'.

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.

- enable_queue: <code>boolean()</code>

  Deprecated since v5.0.14.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  Maximum number of bytes to buffer for each buffer worker.


## bridge_mqtt_publisher:action_parameters
Action specific configs.


**Config paths**

 - <code>actions.mqtt.$name.parameters</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MQTT__$NAME__PARAMETERS</code>



**Fields**

- topic: <code>binary()</code>

  Forward to which topic of the remote broker.<br/>
  Template with variables is allowed.

- qos: <code>qos() | binary()</code>
  * default: 
  `1`

  The QoS of the MQTT message to be sent.<br/>
  Template with variables is allowed.

- retain: <code>boolean() | binary()</code>
  * default: 
  `false`

  The 'retain' flag of the MQTT message to be sent.<br/>
  Template with variables is allowed.

- payload: <code>binary()</code>

  The payload of the MQTT message to be sent.<br/>
  Template with variables is allowed.


## bridge_mqtt_publisher:action_resource_opts
Creation options.


**Config paths**

 - <code>actions.mqtt.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MQTT__$NAME__RESOURCE_OPTS</code>



**Fields**

- worker_pool_size: <code>1..1024</code>
  * default: 
  `16`

  The number of buffer workers. Only applicable for egress type bridges.
  For bridges only have ingress direction data flow, it can be set to 0 otherwise must be greater than 0.

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  Health check interval.

- query_mode: <code>sync | async</code>
  * default: 
  `async`

  Query mode. Optional 'sync/async', default 'async'.

- request_ttl: <code>emqx_schema:timeout_duration_ms() | infinity</code>
  * default: 
  `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.

- inflight_window: <code>pos_integer()</code>
  * default: 
  `100`

  Query inflight window. When query_mode is set to async, this config has to be set to 1 if messages from the same MQTT client have to be strictly ordered.

- max_buffer_bytes: <code>emqx_schema:bytesize()</code>
  * default: 
  `256MB`

  Maximum number of bytes to buffer for each buffer worker.


## bridge_mqtt_publisher:ingress_parameters
Source specific configs.


**Config paths**

 - <code>sources.mqtt.$name.parameters</code>


**Env overrides**

 - <code>EMQX_SOURCES__MQTT__$NAME__PARAMETERS</code>



**Fields**

- topic: <code>binary()</code>

  Receive messages from which topic of the remote broker

- qos: <code>qos()</code>
  * default: 
  `1`

  The QoS level to be used when subscribing to the remote broker


## bridge_mqtt_publisher:source_resource_opts
Creation options.


**Config paths**

 - <code>sources.mqtt.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_SOURCES__MQTT__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  Health check interval.


## bridge_mqtt_publisher:mqtt_publisher_action
Action configs.


**Config paths**

 - <code>actions.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_ACTIONS__MQTT__$NAME</code>



**Fields**

- local_topic: <code>binary()</code>

  MQTT topic or topic filter as data source (action input).  If rule action is used as data source, this config should be left empty, otherwise messages will be duplicated in the remote system.

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable (true) or disable (false) this action.

- connector: <code>binary()</code>

  Name of the connector specified by the action, used for external resource selection.

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  Descriptive text.

- parameters: <code>[bridge_mqtt_publisher:action_parameters](#bridge_mqtt_publisher-action_parameters)</code>

  Action specific configs.

- resource_opts: <code>[bridge_mqtt_publisher:action_resource_opts](#bridge_mqtt_publisher-action_resource_opts)</code>
  * default: 
  `{}`

  Resource options.


## bridge_mqtt_publisher:mqtt_subscriber_source
Source configs.


**Config paths**

 - <code>sources.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_SOURCES__MQTT__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable (true) or disable (false) this action.

- connector: <code>binary()</code>

  Name of the connector specified by the action, used for external resource selection.

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  Descriptive text.

- parameters: <code>[bridge_mqtt_publisher:ingress_parameters](#bridge_mqtt_publisher-ingress_parameters)</code>



- resource_opts: <code>[bridge_mqtt_publisher:source_resource_opts](#bridge_mqtt_publisher-source_resource_opts)</code>
  * default: 
  `{}`

  Resource options.


## connector:connectors
Connectors that are used to connect to external systems


**Config paths**

 - <code>connectors</code>


**Env overrides**

 - <code>EMQX_CONNECTORS</code>



**Fields**

- http: <code>{$name -> [bridge_http:config_connector](#bridge_http-config_connector)}</code>

  HTTP Connector Config

- mqtt: <code>{$name -> [connector_mqtt:config_connector](#connector_mqtt-config_connector)}</code>

  MQTT Publisher Connector Config


## connector_http:request



**Config paths**

 - <code>authentication.$INDEX.request</code>
 - <code>authorization.sources.$INDEX.request</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__REQUEST</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__REQUEST</code>



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

- request_timeout: <code>emqx_schema:timeout_duration_ms()</code>

  HTTP request timeout.


## connector_mqtt:resource_opts
Resource options.


**Config paths**

 - <code>connectors.mqtt.$name.resource_opts</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MQTT__$NAME__RESOURCE_OPTS</code>



**Fields**

- health_check_interval: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `15s`

  Health check interval.

- start_after_created: <code>boolean()</code>
  * default: 
  `true`

  Whether start the resource right after created.

- start_timeout: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `5s`

  Time interval to wait for an auto-started resource to become healthy before responding resource creation requests.


## connector_mqtt:config_connector
Configurations for an MQTT connector.


**Config paths**

 - <code>connectors.mqtt.$name</code>


**Env overrides**

 - <code>EMQX_CONNECTORS__MQTT__$NAME</code>



**Fields**

- enable: <code>boolean()</code>
  * default: 
  `true`

  Enable (true) or disable (false) this connector.

- tags: <code>[binary()]</code>

  Tags to annotate this config entry.

- description: <code>string()</code>
  * default: 
  `""`

  Descriptive text.

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the pool of MQTT clients that will publish messages to the remote broker.<br/>
  Each MQTT client will be assigned 'clientid' of the form '${clientid_prefix}:${bridge_name}:egress:${node}:${n}'
  where 'n' is the number of a client inside the pool.

- resource_opts: <code>[connector_mqtt:resource_opts](#connector_mqtt-resource_opts)</code>
  * default: 
  `{}`

  Resource options.

- mode: <code>cluster_shareload</code>

  Deprecated since v5.1.0 & e5.1.0.

- server: <code>string()</code>

  The host and port of the remote MQTT broker

- clientid_prefix: <code>binary()</code>

  Optional prefix to prepend to the clientid used by egress bridges.

- reconnect_interval: <code>string()</code>

  Deprecated since v5.0.16.

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
  If bridge_mode is set to true, the bridge will indicate to the remote broker that it is a bridge not an ordinary client.
  This means that loop detection will be more effective and that retained messages will be propagated correctly.

- username: <code>binary()</code>

  The username of the MQTT protocol

- password: <code>emqx_schema_secret:secret()</code>

  The password of the MQTT protocol

- clean_start: <code>boolean()</code>
  * default: 
  `true`

  Whether to start a clean session when reconnecting a remote broker for ingress bridge

- keepalive: <code>string()</code>
  * default: 
  `300s`

  MQTT Keepalive. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
  - `s` for seconds,
  - `m` for minutes,
  - `h` for hours;
  <br/>or combination of whereof: `1h5m0s`

- retry_interval: <code>string()</code>
  * default: 
  `15s`

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


## connector_mqtt:egress
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

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the pool of MQTT clients that will publish messages to the remote broker.<br/>
  Each MQTT client will be assigned 'clientid' of the form '${clientid_prefix}:${bridge_name}:egress:${node}:${n}'
  where 'n' is the number of a client inside the pool.

- local: <code>[connector_mqtt:egress_local](#connector_mqtt-egress_local)</code>

  The configs about receiving messages from local broker.

- remote: <code>[connector_mqtt:egress_remote](#connector_mqtt-egress_remote)</code>

  The configs about sending message to the remote broker.


## connector_mqtt:egress_local
The configs about receiving messages from local broker.


**Config paths**

 - <code>bridges.mqtt.$name.egress.local</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__EGRESS__LOCAL</code>



**Fields**

- topic: <code>binary()</code>

  The local topic to be forwarded to the remote broker


## connector_mqtt:egress_remote
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
  * default: 
  `1`

  The QoS of the MQTT message to be sent.<br/>
  Template with variables is allowed.

- retain: <code>boolean() | binary()</code>
  * default: 
  `false`

  The 'retain' flag of the MQTT message to be sent.<br/>
  Template with variables is allowed.

- payload: <code>binary()</code>

  The payload of the MQTT message to be sent.<br/>
  Template with variables is allowed.


## connector_mqtt:ingress
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

- pool_size: <code>pos_integer()</code>
  * default: 
  `8`

  Size of the pool of MQTT clients that will ingest messages from the remote broker.<br/>
  This value will be respected only if 'remote.topic' is a shared subscription topic or topic-filter
  (for example `$share/name1/topic1` or `$share/name2/topic2/#`), otherwise only a single MQTT client will be used.
  Each MQTT client will be assigned 'clientid' of the form '${clientid_prefix}:${bridge_name}:ingress:${node}:${n}'
  where 'n' is the number of a client inside the pool.
  NOTE: Non-shared subscription will not work well when EMQX is clustered.

- remote: <code>[connector_mqtt:ingress_remote](#connector_mqtt-ingress_remote)</code>

  The configs about subscribing to the remote broker.

- local: <code>[connector_mqtt:ingress_local](#connector_mqtt-ingress_local)</code>

  The configs about sending message to the local broker.


## connector_mqtt:ingress_local
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


## connector_mqtt:ingress_remote
The configs about subscribing to the remote broker.


**Config paths**

 - <code>bridges.mqtt.$name.ingress.remote</code>


**Env overrides**

 - <code>EMQX_BRIDGES__MQTT__$NAME__INGRESS__REMOTE</code>



**Fields**

- topic: <code>binary()</code>

  Receive messages from which topic of the remote broker

- qos: <code>qos()</code>
  * default: 
  `1`

  The QoS level to be used when subscribing to the remote broker


## emqxtel:telemetry
Configure telemetry data report from this EMQX node to EMQ's telemetry data collection server.
See https://www.emqx.io/docs/en/v5.0/telemetry/telemetry.html for more details.


**Config paths**

 - <code>telemetry</code>


**Env overrides**

 - <code>EMQX_TELEMETRY</code>



**Fields**

- enable: <code>boolean()</code>

  Set to `false` disable telemetry data report


## gateway:coap
The CoAP protocol gateway provides EMQX with the access capability of the CoAP protocol.
It allows publishing, subscribing, and receiving messages to EMQX in accordance
with a certain defined CoAP message format.


**Config paths**

 - <code>gateway.coap</code>


**Env overrides**

 - <code>EMQX_GATEWAY__COAP</code>



**Fields**

- heartbeat: <code>emqx_coap_schema:duration()</code>
  * default: 
  `30s`

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
  The type of delivered coap message can be set to:<br/>
    - non: Non-confirmable;<br/>
    - con: Confirmable;<br/>
    - qos: Mapping from QoS type of received message, QoS0 -> non, QoS1,2 -> con

- subscribe_qos: <code>qos0 | qos1 | qos2 | coap</code>
  * default: 
  `coap`

  The Default QoS Level indicator for subscribe request.
  This option specifies the QoS level for the CoAP Client when establishing a subscription membership, if the subscribe request is not carried `qos` option. The indicator can be set to:<br/>
    - qos0, qos1, qos2: Fixed default QoS level<br/>
    - coap: Dynamic QoS level by the message type of subscribe request<br/>
      * qos0: If the subscribe request is non-confirmable<br/>
      * qos1: If the subscribe request is confirmable

- publish_qos: <code>qos0 | qos1 | qos2 | coap</code>
  * default: 
  `coap`

  The Default QoS Level indicator for publish request.
  This option specifies the QoS level for the CoAP Client when publishing a message to EMQX PUB/SUB system, if the publish request is not carried `qos` option. The indicator can be set to:<br/>
    - qos0, qos1, qos2: Fixed default QoS level<br/>
    - coap: Dynamic QoS level by the message type of publish request<br/>
      * qos0: If the publish request is non-confirmable<br/>
      * qos1: If the publish request is confirmable

- mountpoint: <code>binary()</code>
  * default: 
  `""`

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
  The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
  then the client actually subscribes to the topic `some_tenant/t`.
  Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
  the message is routed to all the clients subscribed `some_tenant/t`,
  so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Supported placeholders in mountpoint string:<br/>
    - <code>${clientid}</code>: clientid<br/>
    - <code>${username}</code>: username<br/>
    - <code>${endpoint_name}</code>: endpoint name

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>



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
  `30s`

  The idle time of the client connection process. It has two purposes:
    1. A newly created client process that does not receive any client requests after that time will be closed directly.
    2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo override.


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

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
  The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
  then the client actually subscribes to the topic `some_tenant/t`.
  Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
  the message is routed to all the clients subscribed `some_tenant/t`,
  so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Supported placeholders in mountpoint string:<br/>
    - <code>${clientid}</code>: clientid<br/>
    - <code>${username}</code>: username<br/>
    - <code>${endpoint_name}</code>: endpoint name

- listeners: <code>[gateway:tcp_udp_listeners](#gateway-tcp_udp_listeners)</code>



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
  `30s`

  The idle time of the client connection process. It has two purposes:
    1. A newly created client process that does not receive any client requests after that time will be closed directly.
    2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo override.


## gateway:exproto_grpc_handler
Settings for the exproto gRPC connection handler.


**Config paths**

 - <code>gateway.exproto.handler</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__HANDLER</code>



**Fields**

- address: <code>binary()</code>

  gRPC server address.

- service_name: <code>ConnectionHandler | ConnectionUnaryHandler</code>
  * default: 
  `ConnectionUnaryHandler`

  The service name to handle the connection events.
  In the initial version, we expected to use streams to improve the efficiency
  of requests in `ConnectionHandler`. But unfortunately, events between different
  streams are out of order. It causes the `OnSocketCreated` event to may arrive
  later than `OnReceivedBytes`.
  So we added the `ConnectionUnaryHandler` service since v5.0.25 and forced
  the use of Unary in it to avoid ordering problems.

- ssl_options: <code>[broker:ssl_client_opts](#broker-ssl_client_opts)</code>

  SSL configuration for the gRPC client.


## gateway:exproto_grpc_server
Settings for the exproto gRPC server.


**Config paths**

 - <code>gateway.exproto.server</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__SERVER</code>



**Fields**

- bind: <code>emqx_exproto_schema:ip_port()</code>

  Listening address and port for the gRPC server.

- ssl_options: <code>[gateway:ssl_server_opts](#gateway-ssl_server_opts)</code>

  SSL configuration for the gRPC server.


## gateway:ssl_server_opts
SSL configuration for the server.


**Config paths**

 - <code>gateway.exproto.server.ssl_options</code>


**Env overrides**

 - <code>EMQX_GATEWAY__EXPROTO__SERVER__SSL_OPTIONS</code>



**Fields**

- cacertfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cacert.pem"`

  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

- certfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/cert.pem"`

  PEM format certificates chain file.<br/>
  The certificates in this file should be in reversed order of the certificate
  issue chain. That is, the host's certificate should be placed in the beginning
  of the file, followed by the immediate issuer certificate and so on.
  Although the root CA certificate is optional, it should be placed at the end of
  the file if it is to be added.

- keyfile: <code>binary()</code>
  * default: 
  `"${EMQX_ETC_DIR}/certs/key.pem"`

  PEM format private key file.

- verify: <code>verify_peer | verify_none</code>
  * default: 
  `verify_none`

  Enable or disable peer verification.

- reuse_sessions: <code>boolean()</code>
  * default: 
  `true`

  Enable TLS session reuse.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.

- password: <code>string()</code>

  String containing the user's password. Only used if the private key file is password-protected.

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

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
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.

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

  An important security setting. It forces the cipher to be set based
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
  the number of messages the underlying cipher suite can encipher.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- handshake_timeout: <code>emqx_schema:duration()</code>
  * default: 
  `15s`

  Maximum time duration allowed for the handshake to complete


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

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
  The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
  then the client actually subscribes to the topic `some_tenant/t`.
  Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
  the message is routed to all the clients subscribed `some_tenant/t`,
  so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Supported placeholders in mountpoint string:<br/>
    - <code>${clientid}</code>: clientid<br/>
    - <code>${username}</code>: username<br/>
    - <code>${endpoint_name}</code>: endpoint name

- listeners: <code>[gateway:udp_listeners](#gateway-udp_listeners)</code>



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
  `30s`

  The idle time of the client connection process. It has two purposes:
    1. A newly created client process that does not receive any client requests after that time will be closed directly.
    2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo override.


## gateway:mqttsn_predefined
The pre-defined topic name corresponding to the pre-defined topic
ID of N.

Note: the pre-defined topic ID of 0 is reserved.


**Config paths**

 - <code>gateway.mqttsn.predefined.$INDEX</code>


**Env overrides**

 - <code>EMQX_GATEWAY__MQTTSN__PREDEFINED__$INDEX</code>



**Fields**

- id: <code>1..1024</code>

  Topic ID. Range: 1-65535

- topic: <code>binary()</code>

  Topic Name


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

  When publishing or subscribing, prefix all topics with a mountpoint string.
  The prefixed string will be removed from the topic name when the message is delivered to the subscriber.
  The mountpoint is a way that users can use to implement isolation of message routing between different listeners.
  For example if a client A subscribes to `t` with `listeners.tcp.\<name>.mountpoint` set to `some_tenant`,
  then the client actually subscribes to the topic `some_tenant/t`.
  Similarly, if another client B (connected to the same listener as the client A) sends a message to topic `t`,
  the message is routed to all the clients subscribed `some_tenant/t`,
  so client A will receive the message, with topic name `t`. Set to `""` to disable the feature.
  Supported placeholders in mountpoint string:<br/>
    - <code>${clientid}</code>: clientid<br/>
    - <code>${username}</code>: username<br/>
    - <code>${endpoint_name}</code>: endpoint name

- listeners: <code>[gateway:tcp_listeners](#gateway-tcp_listeners)</code>



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
  `30s`

  The idle time of the client connection process. It has two purposes:
    1. A newly created client process that does not receive any client requests after that time will be closed directly.
    2. A running client process that does not receive any client requests after this time will go into hibernation to save resources.

- clientinfo_override: <code>[gateway:clientinfo_override](#gateway-clientinfo_override)</code>

  ClientInfo override.


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


## ldap:ssl
SSL connection settings.


**Config paths**

 - <code>authentication.$INDEX.ssl</code>
 - <code>authorization.sources.$INDEX.ssl</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__SSL</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__SSL</code>



**Fields**

- cacertfile: <code>binary()</code>

  Trusted PEM format CA certificates bundle file.<br/>
  The certificates in this file are used to verify the TLS peer's certificates.
  Append new certificates to the file if new CAs are to be trusted.
  There is no need to restart EMQX to have the updated file loaded, because
  the system regularly checks if file has been updated (and reload).<br/>
  NOTE: invalidating (deleting) a certificate from the file will not affect
  already established connections.

- cacerts: <code>boolean()</code>

  Deprecated since 5.1.4.

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

  Enable TLS session reuse.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- depth: <code>non_neg_integer()</code>
  * default: 
  `10`

  Maximum number of non-self-issued intermediate certificates that can follow the peer certificate in a valid certification path.
  So, if depth is 0 the PEER must be signed by the trusted ROOT-CA directly;<br/>
  if 1 the path can be PEER, Intermediate-CA, ROOT-CA;<br/>
  if 2 the path can be PEER, Intermediate-CA1, Intermediate-CA2, ROOT-CA.

- password: <code>string()</code>

  String containing the user's password. Only used if the private key file is password-protected.

- versions: <code>[atom()]</code>
  * default: 
  `[tlsv1.3, tlsv1.2]`

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
  RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>

- secure_renegotiate: <code>boolean()</code>
  * default: 
  `true`

  SSL parameter renegotiation is a feature that allows a client and a server
  to renegotiate the parameters of the SSL connection on the fly.
  RFC 5746 defines a more secure way of doing this. By enabling secure renegotiation,
  you drop support for the insecure renegotiation, prone to MitM attacks.<br/>
  Has no effect when TLS version is configured (or negotiated) to 1.3

- log_level: <code>emergency | alert | critical | error | warning | notice | info | debug | none | all</code>
  * default: 
  `notice`

  Log level for SSL communication. Default is 'notice'. Set to 'debug' to inspect TLS handshake messages.

- hibernate_after: <code>emqx_schema:duration()</code>
  * default: 
  `5s`

  Hibernate the SSL process after idling for amount of time reducing its memory footprint.

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
  to establish the connection, unless it is IP address used.<br/>
  The host name is then also used in the host name verification of the peer
  certificate.<br/> The special value 'disable' prevents the Server Name
  Indication extension from being sent and disables the hostname
  verification check.


## mongo:topology
Topology of MongoDB.


**Config paths**

 - <code>authentication.$INDEX.topology</code>
 - <code>authorization.sources.$INDEX.topology</code>


**Env overrides**

 - <code>EMQX_AUTHENTICATION__$INDEX__TOPOLOGY</code>
 - <code>EMQX_AUTHORIZATION__SOURCES__$INDEX__TOPOLOGY</code>



**Fields**

- max_overflow: <code>non_neg_integer()</code>
  * default: 
  `0`

  The maximum number of additional workers that can be created when all workers in the pool are busy. This helps to manage temporary spikes in workload by allowing more concurrent connections to the MongoDB server.

- overflow_ttl: <code>emqx_schema:timeout_duration_ms()</code>

  Period of time before workers that exceed the configured pool size ("overflow") to be terminated.

- overflow_check_period: <code>emqx_schema:timeout_duration_ms()</code>

  Period for checking if there are more workers than configured ("overflow").

- local_threshold_ms: <code>emqx_schema:timeout_duration_ms()</code>

  The size of the latency window for selecting among multiple suitable MongoDB instances.

- connect_timeout_ms: <code>emqx_schema:timeout_duration_ms()</code>

  The duration to attempt a connection before timing out.

- socket_timeout_ms: <code>emqx_schema:timeout_duration_ms()</code>

  The duration to attempt to send or to receive on a socket before the attempt times out.

- server_selection_timeout_ms: <code>emqx_schema:timeout_duration_ms()</code>

  Specifies how long to block for server selection before throwing an exception.

- wait_queue_timeout_ms: <code>emqx_schema:timeout_duration_ms()</code>

  The maximum duration that a worker can wait for a connection to become available.

- heartbeat_frequency_ms: <code>emqx_schema:timeout_duration_ms()</code>
  * default: 
  `200s`

  Controls when the driver checks the state of the MongoDB deployment. Specify the interval between checks, counted from the end of the previous check until the beginning of the next one. If the number of connections is increased (which will happen, for example, if you increase the pool size), you may need to increase this period as well to avoid creating too many log entries in the MongoDB log file.

- min_heartbeat_frequency_ms: <code>emqx_schema:timeout_duration_ms()</code>

  Controls the minimum amount of time to wait between heartbeats.


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
  `plugins`

  The installation directory for the external plugins.
  The plugin beam files and configuration files should reside in
  the subdirectory named as <code>emqx_foo_bar-0.1.0</code>.
  <br/>
  NOTE: For security reasons, this directory should **NOT** be writable
  by anyone except <code>emqx</code> (or any user which runs EMQX).

- check_interval: <code>emqx_schema:duration()</code>

  Deprecated since 5.0.24.


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


## psk:psk_authentication
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
  `500ms`

  The latency threshold for statistics

- expire_interval: <code>emqx_schema:duration_ms()</code>
  * default: 
  `300s`

  The eviction time of the record, which in the statistics record table

- top_k_num: <code>pos_integer()</code>
  * default: 
  `10`

  The maximum number of records in the slow subscription statistics record table

- stats_type: <code>whole | internal | response</code>
  * default: 
  `whole`

  The method to calculate the latency


