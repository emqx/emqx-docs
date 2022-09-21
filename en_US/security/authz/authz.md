# Introduction

MQTT authorization refers to _permission control_ for publish/subscribe operations, i.e., authorization
determines which clients can publish/subscribe to which topics.

EMQX supports different kinds of authorization:

* Authorization based on access lists(_ACLs_) fetched from a database (MongoDB, MySQL, PostgreSQL, Redis, and built-in database).
* Based on the global static access-list configured in a file.
* Based on the result of an HTTP request to an external HTTP API.
* Based on access lists extracted from authentication data (access lists from JWT claims).

## Authorization sources

_Authorization source_ (or simply _authorizer_) is an EMQX module that implements MQTT authorization.
Authorizers are identified by their _type_, a unique identifier.

The following authorizers are available by default:

| type               | description                                                                   |
| ------------------ | ----------------------------------------------------------------------------- |
| built_in_database  | [Authorization with Mnesia database as rules storage](./mnesia.md)            |
| mysql              | [Authorization with MySQL database as rules storage](./mysql.md)              |
| postgresql         | [Authorization with PostgreSQL database as rules storage](./postgresql.md)    |
| mongodb            | [Authorization with MongoDB database as rules storage](./mongodb.md)          |
| redis              | [Authorization with Redis database as rules storage](./redis.md)              |
| http               | [Authorization using external HTTP API to determine permissions](./http.md)   |
| file               | [Authorization using static rules configured in a file](./file.md)            |

Each authorizer has its own configuration options.

Example:

```
{
    enable => true

    type = mysql
    database = "mqtt"
    username = "root"
    password = "public"

    query = "SELECT permission, action, topic FROM acl WHERE username = ${username}"
    server = "10.12.43.12:3306"
}
```

## Authorization chain

Configured authorizers form a global chain. When a client makes a publish/subscribe request, authorizers are
sequentially used to find access lists for the client. If an authorizer finds authorization rules, the request is checked
against them and gets allowed/denied. If authorization rules are not found, then the next authorizer from the chain is used.

If no authorizer finds any authorization rules, then the default permission is applied.

Unlike [authentication](../authn/authn.md#authentication-chains), authorization has only one global chain.

## Implicit authorization

Each _authentication_ backend can additionally provide rules as a result of the authentication. These rules, if present, are applied before any other authorizers.

See, for example, [JWT authorization](../authn/jwt.md#jwt-authorization).

## Authorization Cache

If a client sends requests intensively, it may be resource-consuming to fetch authorization rules for each request and match it
against the rules. Therefore, authorization cache may be enabled to cache authorization results for a particular time.

::: tip Tip
Caching improves performance significantly, so adjusting the default values to the relevant ones is essential.
You can find more information about cache configuration below.
:::

## Authorization placeholders

Many authorizers allow using _placeholders_ in their configuration.
Placeholders work as template variables filled with the corresponding runtime information.

### Placeholders in configurations

Configuration templates can be filled with client information such as client ID and user name at runtime.

The following placeholders are available:

* `${clientid}` — clientid of the client.
* `${username}` — username of the client.
* `${peerhost}` — client IP address.
* `${cert_subject}` — subject of client's TLS certificate, valid only for TLS connections.
* `${cert_common_name}` common name of client's TLS certificate, valid only for TLS connections.

Example of use in Redis authorizer:

```
{
  enable = true
  type = redis

  ... other parameters ...

  cmd = "HGETALL mqtt_user:${username}"
}
```

::: warning
If a value for a placeholder is absent then the placeholder is rendered as an ampty string. For example, a template
`HGETALL mqtt_user:${username}` will be rendered as `HGETALL mqtt_user:` if a username is not provided by an MQTT client.
:::

### Topic Placeholders

When authentication rules are fetched from external databases, the topics are represented as string values. These values are interpreted as templates.
The following placeholders are available:

* `${clientid}` — Client ID of the connecting client, when used in authorization rules, the MQTT client should assign a client ID before connect, but not let EMQX generate and assign a random one.
* `${username}` — `username` value used by the client for authentication.

Placeholders can be used as topic segments, like `a/b/${username}/c/d`, but not `a/b${username}c/d`.

To avoid placeholder interpolation, one may use special `eq` syntax: `eq a/b/${username}/c/d`. This topic will be treated as `a/b/${username}/c/d` literally, without interpolation.

## Config structure

The general config structure is the following:

```
authorization {
  sources = [
    { ...   },
    { ...   }
  ]
  no_match = allow
  deny_action = ignore
  cache {
    enable = true
    max_size = 1024
    duration = 1m
  }
}
```

### `sources`

Optional list value that configures the chain of authorizers. Each authorizer can be enabled or disabled.
Disabled authorizers are not taken into account. The absence of the value is treated as an empty chain.

For individual authorizer config formats, see the documentation for the corresponding authorizers.

### `no_match`

Optional value, `allow` or `deny`. The default value is `allow`. Determines the default action for a publish/subscribe
request if none of the configured authorizers found any authorization rules.

### `deny_action`

Optional value, `ignore` or `disconnect`. The default value is `ignore`. Determines what to do if a publish/subscribe operation was rejected according to the authorization checks. If set to `ignore`, the operation is silently ignored.
If set to `disconnect`, the client connection is dropped.

### `cache`

Optional value with caching settings.

* `cache.enable` — optional boolean value, default is `true`. Specifies whether to enable caching. When authentication JWT is the only data source of authentication data, then it is recommended to configure this field `false`.
* `cache.max_size` — optional integer value, default is 32. Specifies the maximum number of elements in the cache. Older records are evicted from the cache when the specified number is exceeded.
* `cache.ttl` — optional duration value, default is `1m`. Specifies how long cached values are kept in the cache.

## REST API

There are several API endpoints for managing authorization:

* `/api/v5/authorization/settings` — for general params, `no_match`, `deny_action`, and `cache`;
* `/api/v5/authorization/sources` — for managing and arranging authorizers;
* `/api/v5/authorization/cache` — for cleaning authorization cache;
* `/api/v5/authorization/sources/built_in_database` — for managing authorization rules of `built_in_database` authorizer.
