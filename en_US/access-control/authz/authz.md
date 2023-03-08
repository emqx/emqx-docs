# Authorization

In EMQX, authorization refers to the permission control over the publish/subscribe operation of the MQTT clients.

## Authorization principle

Here is how EMQX authentication works, when a client performs publish/subscribe operations, EMQX will query or follows the user-specified query statement to query the client's permission list from the configured data source, and then allow or reject the current operation based on the query result.

::: tip

The permission list of the client needs to be stored in a specific data source (database, file) in advance. You can update the list during runtime by updating the corresponding data record. 

:::

## Integrate with data storage objects

The EMQX authorization mechanism supports integration with various data storage objects, including built-in databases, files, MySQL, PostgreSQL, MongoDB, and Redis. You can manage permission data through REST API or EMQX Dashboard, or use a CSV or JSON file to import the data in a batch.

In addition, EMQX can also connect to HTTP services developed by our users to meet different authorization requirements.

According to the backend data storage used, there are currently 7 different types of EMQX authorizers:

| Database          | Description                                                  |
| ----------------- | ------------------------------------------------------------ |
| Built-in database | [Authorization with built-in database as rules storage](./mnesia.md) |
| MySQL             | [Authorization with MySQL as rules storage](./mysql.md)      |
| PostgreSQL        | [Authorization with PostgreSQL as rules storage](./postgresql.md) |
| MongoDB           | [Authorization with MongoDB as rules storage](./mongodb.md)  |
| Redis             | [Authorization with Redis as rules storage](./redis.md)      |
| HTTP              | [Authorization with external HTTP service](./http.md)        |
| File              | [Authorization with static rules configured in a file](./file.md) |

Each authorizer has its own configuration options. You can click the corresponding links in the above table for more details. 

Below is an example of how to configure an EMQX MySQL authorizer.

Example:

```
{
    enable => true

    type = mysql
    database = "mqtt"
    username = "root"
    password = "public"

    query = "SELECT permission, action, topic FROM mqtt_acl WHERE username = ${username}"
    server = "10.12.43.12:3306"
}
```



## Configure authorization mechanisms

EMQX provides 3 ways to use authorization, namely: Dashboard, Configuration file and HTTP API.

### Configure with Dashboard

EMQX Dashboard is an intuitive way to configure EMQX authorizer, where you can configure relevant parameters, check their working status, adjust their position in the authorization chain。 

### Configure with configuration file

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
    max_size = 32
    ttl = 1m
  }
}
```

Where, 

- `sources` (optional): An ordered array, with each array element defining the data source of the corresponding authorization checker. For detailed configurations, see the corresponding configuration file.

- `no_match`: Determines the default action for a publish/subscribe request if none of the configured authorizers found any authorization rules; optional value: `allow` or `deny`; default:  `allow`. The setting will also trigger the enabling of black/white list. 

- `deny_action`:  Determines the next step if a publish/subscribe operation was rejected; optional value: `ignore` or `disconnect`; default:  `ignore`. If set to `ignore`, the operation is silently ignored; if set to `disconnect`, the client connection is dropped.

- `cache`: Defines the caching settings, and include:

  * `cache.enable`: Specifies whether to enable caching, default: `true`. If the authorization is solely based on the JWT packets, it is recommended to configure this field `false`.

  * `cache.max_size`: Specifies the maximum number of elements in the cache; default: 32. Older records will be removed from the cache if the specified number is exceeded.

  * `cache.ttl`: Specifies the effective time of cached values, default: `1m` (one minute). 

## HTTP API

There are several API endpoints for managing authorization:

* `/api/v5/authorization/settings`: for general parameters, `no_match`, `deny_action`, and `cache`;
* `/api/v5/authorization/sources`: for managing and arranging authorizers;
* `/api/v5/authorization/cache`: for cleaning authorization cache;
* `/api/v5/authorization/sources/built_in_database`:  for managing authorization rules of `built_in_database` authorizer.

For detailed operation steps, see [HTTP API](../../admin/api.md).

## Basic concepts

### Authorization chain

EMQX allows the creation of authorization chain using multiple authorizers and follows the authorizers' position in the chain to perform the authorization.

#### Authorize flow

With authorization chain configured, EMQX will first try to retrieve the matching authentication information from the first authorizer, if fails, it will switch to the next authenticator to continue the process:

1. If EMQX successfully retrieves the client's permission information and then it will match the client's operation matches the retrieved permission list:
   - if matches, EMQX will allow or deny the operation based on permission setting.
   - if not match, EMQX will switch to the next authenticator to continue the process.

2. If EMQX fails to retrieve the client's permission information and then it will check if there are other authorizers configured:
   - if yes, EMQX will switch to the next authenticator to continue the process.
   - if  this is already the last authorizer,  EMQX will follow the setting of `no_match` to determine whether to allow or reject the client operation.

Unlike [Authentication chain](../authn/authn.md#authentication-chains), authorization has only one global chain.

### Implicit authorization

Each authentication backend can additionally provide rules as a result of the authentication. These rules, if present, are applied before any other authorizers.

See, for example, [JWT authorization](../authn/jwt.md#jwt-authorization).

### Authorization cache

To better handle the access pressure brought by a large number of publish/subscribe requests, EMQX introduces the authorization cache mechanism. 

::: tip Tip
If set properly, caching can greatly improve performance, so it is recommended to timely adjust the setting based on your system performance.
:::

### Authorization check priority

Besides the cache and authorization checker, the authorization result may also be affected by the [Super User Role and Permission](../authn/authn.md) set during the authentication phase.

For super users, all their operations will be skipped from authorization check; if the permission list is set, EMQX will first follow the client's permission data to run the  authorization checker. The priority is as follows:

```
Super user > permission data > authorization check
```

### Authorization placeholders

EMQX authorizers allow using placeholders in their configuration. During the authorization step, these placeholders will be replaced with actual client information to construct a query or HTTP request that matches the current client.

For example, in one EMQX MySQL authorizer, the default query SQL uses the placeholder `${username}`:

```sql
SELECT action, permission, topic FROM mqtt_acl where username = ${username}
```

So, when a client (name: `emqx_u`) initiates a connect request, the constructed query statement is like:

```sql
SELECT action, permission, topic FROM mqtt_acl where username = 'emqx_u'
```

#### Placeholders in data queries

The following placeholders are supported in query statements:

* `${clientid}`:  It will be replaced by the client ID at runtime. The client ID is normally explicitly specified by the client in the `CONNECT` packet. If `use_username_as_clientid` or `peer_cert_as_clientid` is enabled, this field will be overridden by the username, fields in the certificate, or the content of the certificate.
* `${username}`:  It will be replaced with the username at runtime. The username comes from the `Username` field in the `CONNECT` packet. If `peer_cert_as_username` is enabled, it will be overridden by the fields or the content of the certificate.
* `${password}`: It will be replaced with the password at runtime. The password comes from the `Password` field in the `CONNECT` packet.
* `${peerhost}`: It will be replaced with the client's IP address at runtime. EMQX supports [Proxy Protocol](http://www.haproxy.org/download/1.8/doc/proxy-protocol.txt), that is, even if EMQX is deployed behind some TCP proxy or load balancer, users can still use this placeholder to get the real IP address.
* `${cert_subject}`:  It will be replaced by the subject of the client's TLS certificate at runtime, only applicable to TLS connections.
* `${cert_common_name}`: It will be replaced by the Common Name of the client's TLS certificate at runtime, only applicable to TLS connections.

#### Topic placeholders

EMQX also allows placeholders to be used in topics to support dynamic themes. The supported placeholders are as follows:

* `${clientid}`: Client ID of the connecting client, when used in authorization rules, the MQTT client should assign a client ID before connecting, but not let EMQX generate and assign a random one.
* `${username}`: user name value used by the client for authentication.

Placeholders can be used as topic segments, like `a/b/${username}/c/d`, but not `a/b${username}c/d`.

To avoid placeholder interpolation, one may use special `eq` syntax: `eq a/b/${username}/c/d`. This topic will be treated as `a/b/${username}/c/d` literally, without interpolation.
