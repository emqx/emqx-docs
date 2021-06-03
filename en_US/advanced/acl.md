# Introduction

**Publish/Subscribe ACL** refers to **permission control**  for  **PUBLISH/SUBSCRIBE** operations. For example, the user name with the name `Anna` is prohibited to publish messages to ` open / elsa / door`.

EMQ X supports the permission management of client through client publish/subscribe ACLs. This chapter describes the publish/subscribe ACLs supported by EMQ X and the configuration methods of corresponding plugins.


## ACL Plugins

EMQ X supports the use of configuration files, external mainstream databases, and custom HTTP APIs as ACL data sources.

The data source connection and access control functions are implemented through plugins, and the corresponding plugins need to be enabled before use.

When a client subscribes to a topic or publishes a message, the plugin implements the management of publishing and subscription permissions for the client by checking whether the target topic is in the specified data source list.



**Configuration file**

{% emqxee %}

* [Built-in ACL](../modules/mnesia_authentication.md)

{% endemqxee %}

{% emqxce %}

* [Built-in ACL](./acl-file.md)
* [Mnesia ACL](./acl-mnesia.md)

{% endemqxce %}

The configuration file is used to provide an authentication data source, which is suitable for ACL management with less changes.



**External Database**


{% emqxee %}

* [MySQL ACL](../modules/mysql_authentication.md)
* [PostgreSQL ACL](../modules/pgsql_authentication.md)
* [Redis ACL](../modules/redis_authentication.md)
* [MongoDB ACL](../modules/mongo_authentication.md)
* [LDAP ACL](../modules/ldap_authentication.md)

{% endemqxee %}

{% emqxce %}

* [MySQL ACL](./acl-mysql.md)
* [PostgreSQL ACL](./acl-postgres.md)
* [Redis ACL](./acl-redis.md)
* [MongoDB ACL](./acl-mongodb.md)

{% endemqxce %}

The external database can store a large amount of data and dynamically manage ACLs to facilitate integration with external device management systems.



**Else**

{% emqxee %}

* [HTTP ACL](../modules/http_authentication.md)

{% endemqxee %}

{% emqxce %}

* [HTTP ACL](./acl-http.md)

{% endemqxce %}


HTTP ACL enables complex ACL management.



::: tip 

The ACL function is included in the authentication plugin. After changing the plugin configuration, you need to restart the plugin to take effect.

:::




## Detailed Rules

ACL is a collection of allowing and denying conditions. The following elements are used in EMQ X to describe ACL rules:

```bash
## Allow-Deny Who Pub-Sub Topic

"Allow/Deny"  "Who"  "Subscribe/Publish" "Topics"
```

{% emqxee %}

When there are multiple ACL rules at the same time, EMQ X will merge them in order according to the rules. Taking the default ACL in ACL file as an example, it loads the rule from bottom to top:

{% endemqxee %}

{% emqxce %}

When there are multiple ACL rules at the same time, EMQ X will merge them in order according to the rules. Taking the default ACL in [ACL file](./acl-file.md) as an example, it loads the rule from bottom to top:

{% endemqxce %}

1. The first rule allows clients to publish and subscribe to all topics
2. The second rule prohibits all clients from subscribing to the topics `$SYS/#` and `#`
3. The third rule allows clients with IP address `127.0.0.1` to publish/subscribe to the topics ` $SYS/# `and `# `, which makes a special case for the second rule
4. The fourth rule allows clients with the username `dashboard` to subscribe to the topic ` $SYS/# `, which makes a special case for the second rule

```erlang
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

{allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

{deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

{allow, all}.
```



## Authentication results

Any  ACL authentication eventually returns a result:

- Allow: Client operation is allowed after checking
- Deny: Client operations are denied after inspection
- Ignore: No ACL permission information was found, and the result could not be explicitly determined as allowed or denied. It will be determined by the next ACL plugin or the default ACL rule.



## Global Configuration

In the default configuration, ACL is open for authentication, which means when the authentication result is **ignore**, the client is allowed to pass the authentication.

This property can be changed through the ACL configuration in `etc / emqx.conf`:

```bash
# etc/emqx.conf

## Default authentication when ACLs do not match
## Value: allow | deny
acl_nomatch = allow
```

{% emqxce %}

Configure the default  [ACL file](./acl-file.md) and use the file to define the default ACL rule:

{% endemqxce %}

{% emqxee %}

Configure the default, use the file to define the default ACL rule:

{% endemqxee %}


```bash
# etc/emqx.conf

acl_file = etc/acl.conf
```

Configure the response action when ACL authentication is  **deny**, the device will be disconnected if it is `ignore`:

```bash
# etc/emqx.conf

## Value: ignore | disconnect
acl_deny_action = ignore
```

::: tip

In MQTT v3.1 and v3.1.1 protocols, the server returns without any packet error after the publishing operation is rejected, which is a flaw in the protocol design. However, a corresponding error message has been supported on the MQTT v5.0 protocol.

:::


## Superuser

Clients can have a "Superuser" identity, which has the highest permissions without being restricted by ACLs.

1. After the superuser function is enabled in the authentication plugin, EMQ X will check whether the client  has superuser identity first when publishing the subscription

2. When the client is a super user, the authentication is passed and subsequent ACL checks are skipped


## ACL Cache

ACL cache allows the client to cache an ACL rule into memory after hitting it, so that it can be used directly next time. Enabling ACL cache can improve the performance of ACL check when the client publishes and subscribes frequently.

You can configure the ACL cache size and cache time in `etc / emqx.conf`:

```bash
# etc/emqx.conf

## Whether to enable
enable_acl_cache = on

## Maximum number of cache rules per client
acl_cache_max_size = 32

## Cache expiry time, cache will be cleared after timeout
acl_cache_ttl = 1m
```


### Clear cache

After updating the ACL rule, some clients cannot take effect immediately because the cache already exists. You need to manually clear all ACL caches to make them taking effect immediately :

Refer to [HTTP API - CLear ACL cache](http-api.md#endpoint-get-acl-cache)


## ACL Authentication Chain

When multiple ACL plugins are enabled at the same time, EMQ X will perform chain authentication in the order in which the plugins are opened:
- Once authentication passed, terminate the chain and allow clients to pass authentication
- Once authorization fails, terminate the chain and deny clients from passing authentication
- if keep failing until the last ACL plugin, judge according to the **default authentication** configuration
  - Allow client to pass authentication when default authentication is *allow*
  - Deny clients from passing authentication When default authentication is *deny*


![_images/guide_3.png](./assets/guide_3.png)

<!-- replace -->

::: tip 

Enabling only one ACL plugin at the time can improve client ACL checking performance.

:::


