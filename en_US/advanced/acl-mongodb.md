---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref: undefined
---

# MongoDB ACL

For MongoDB ACL, an external MongoDB database is used to store ACL rules, which can store a large amount of data and dynamically manage ACLs for easy integration with external device management systems

Plugin:

```bash
emqx_auth_mongo
```

::: tip
The emqx_auth_mongo plugin also includes authentication, which can be disabled via comments
:::


## MongoDB connection information

MongoDB basic connection information needs to ensure that all nodes in the cluster can be accessed.

```bash
# etc/plugins/emqx_auth_mongo.conf

## MongoDB Architecture type
##
## Value: single | unknown | sharded | rs
auth.mongo.type = single

## rs mode needs to set rs name
## auth.mongo.rs_set_name =

## Server list, separated by comma in cluster mode
## Examples: 127.0.0.1:27017,127.0.0.2:27017...
auth.mongo.server = 127.0.0.1:27017

auth.mongo.pool = 8

auth.mongo.login =

auth.mongo.password =

## auth.mongo.auth_source = admin

auth.mongo.database = mqtt

auth.mongo.query_timeout = 5s

## SSL option
# auth.mongo.ssl = false

## auth.mongo.ssl_opts.keyfile =

## auth.mongo.ssl_opts.certfile =

## auth.mongo.ssl_opts.cacertfile =

## MongoDB write mode.
##
## Value: unsafe | safe
## auth.mongo.w_mode =

## Mongo read mode.
##
## Value: master | slave_ok
## auth.mongo.r_mode =

## MongoDB topology configuration, generally not used, see MongoDB website documentation for details
auth.mongo.topology.pool_size = 1
auth.mongo.topology.max_overflow = 0
## auth.mongo.topology.overflow_ttl = 1000
## auth.mongo.topology.overflow_check_period = 1000
## auth.mongo.topology.local_threshold_ms = 1000
## auth.mongo.topology.connect_timeout_ms = 20000
## auth.mongo.topology.socket_timeout_ms = 100
## auth.mongo.topology.server_selection_timeout_ms = 30000
## auth.mongo.topology.wait_queue_timeout_ms = 1000
## auth.mongo.topology.heartbeat_frequency_ms = 10000
## auth.mongo.topology.min_heartbeat_frequency_ms = 1000
```


## Default data structure

In the default configuration of MongoDB authentication, you need to ensure that the following collections are included in the database:

### Authentication / Super Collection

```sql
{
  username: "user",
  password: "password hash",
  salt: "password salt",
  is_superuser: false,
  created: "2020-02-20 12:12:14"
}
```

Sample data:

```bash
use mqtt

db.mqtt_user.insert({
  "username": "emqx",
  "password": "efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7",
  "is_superuser": false,
  "salt": ""
})
```

### ACL rule collection

```json
{
    username: "username",
    clientid: "clientid",
    publish: ["topic1", "topic2", ...],
    subscribe: ["subtop1", "subtop2", ...],
    pubsub: ["topic/#", "topic1", ...]
}
```

MongoDB ACL rule defines the publish, subscribe, and publish/subscribe information, and  all **allow** lists are included in the rule.

Rule field description:

- username: the user name of the connected client
- clientid: the Client ID of the connected client
- publish: the number of topics allowed to be published, supports wildcards
- subscribe: the number of topics allowed to be subscribed to, supports wildcards
- pubsub: the number of topics allowed to be published  and subscribed to, supports wildcards

::: tip
Wildcards can be used for topic, and placeholders can be added to the topic to match client information. For example,  the topic will be replaced with the client ID of the current client when matching `t/%c`.

  - %u：user name
  - %c：Client ID
:::


Sample data in the default configuration:

```bash
use mqtt

## All users cannot subscribe and publish system topics
## Clients are allowed to subscribe to the topic of /smarthome/${clientid}/temperature with their own Client ID

db.mqtt_acl.insert({
  username: "$all",
  clientid: "$all",
  publish: ["#"],
  subscribe: ["/smarthome/%c/temperature"]
})
```

After enabling MongoDB ACL and successfully connecting with the username emqx, the client should have the appropriate topic permissions for the data.


## super_query

When performing ACL authentication, EMQX Broker will use the current client information to execute a user-configured superuser query to query whether the client is a superuser. ACL queries are skipped when the client is a superuser.

```bash
# etc/plugins/emqx_auth_mongo.conf

## Enable superuser
auth.mongo.super_query = on

##collections for super queries
auth.mongo.super_query.collection = mqtt_user

##Field for super user
auth.mongo.super_query.super_field = is_superuser

## Superuser query selector, commas can be used to seperate multiple conditions
#auth.mongo.super_query.selector = username=%u, clientid=$all
auth.mongo.super_query.selector = username=%u
```

MongoDB `and` query is used in the actual query under multiple conditions of the same **selector**:

```bash
db.mqtt_user.find({
  "username": "wivwiv"
  "clientid": "$all"
})
```

You can use the following placeholders in your query conditions, and EMQX Broker will automatically populate with client information when executed:

- %u：user name
- %c：Client ID

You can adjust the super user query according to business to achieve more business-related functions, such as adding multiple query conditions and using database preprocessing functions. However, in any case, the superuser query needs to meet the following conditions:

1. The query result must include the is_superuser field, which should be explicitly true

::: tip
If superuser functionality is not needed, it can be more efficient when commenting and disabling this option
:::


## acl_query

When performing ACL authentication, EMQX Broker will use the current client information to execute the user-configured superuser query. If superuser query is not enabled or the client is not a superuser, ACL query will be used to find out the ACL rules of client in the database.

```bash
# etc/plugins/emqx_auth_mongo.conf

auth.mongo.acl_query = on

auth.mongo.acl_query.collection = mqtt_acl

## Query selector, commas can be used to seperate multiple conditions
## auth.mongo.acl_query.selector = username=%u,clientid=%c
auth.mongo.acl_query.selector = username=%u

## Using multiple query selectors
## auth.mongo.acl_query.selector.1 = username=$all
## auth.mongo.acl_query.selector.2 = username=%u
```

MongoDB `and` query is used in the actual query under multiple **conditions**  of the same selector:

```bash
db.mqtt_acl.find({
  "username": "emqx"
  "clientid": "$all"
})
```

MongoDB `or` query is used in actual query under multiple **selectors**:

```bash
db.mqtt_acl.find({
  "$or": [
    {
      "username": "$all"
    },
    {
      "username": "emqx"
    }
  ]
})
```


You can use the following placeholders in ACL queries, and EMQX Broker will automatically populate with client information when executed:

- %u：username
- %c：Client ID

::: tip
MongoDB ACL rules need to use the above data structures strictly.

All rules added in MongoDB ACL are **allow** rules. i.e. a whitelist.

When a client's rules list is empty, EMQX continues to check the next auth/ACL plugin.
Otherwise the check returns immediately without proceeding to the next auth/ACL plugins.

When the rule is non-empty and does not match the corresponding pub/sub permission,
an ath/ACL failure will be returned (the corresponding pub/sub behavior will be denied) and the auth/ACL chain will be terminated.

When more than one auth/ACL plugins are in use, it is recommended to position MongoDB ACL after other auth/ACL plugins in the enabled plugins list.
:::
