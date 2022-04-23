# MongoDB

MongoDB authentication uses an external MongoDB database as the authentication data source, which can store a large amount of data and facilitate integration with external device management systems.

Plugin:

```bash
emqx_auth_mongo
```

::: tip 
The emqx_auth_mongo plugin also includes ACL feature, which can be disabled via comments
:::



To enable MongoDB authentication, you need to configure the following in `etc/plugins/emqx_auth_mongo.conf` :

## MongoDB Connection information

For MongoDB basic connection information, it needs to ensure that all nodes in the cluster can access.

```bash
# etc/plugins/emqx_auth_mongo.conf

## MongoDB Architecture type
##
## Value: single | unknown | sharded | rs
auth.mongo.type = single

##rs mode needs to set rs name
## auth.mongo.rs_set_name =

## Server list, which is separated by comma in cluster mode
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

## MongoDB topology configuration, which is not used generally. See MongoDB official ##website documentation
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

In the default configuration of MongoDB authentication, you need to ensure that the database has the following collections:

```json
{
  username: "user",
  password: "password hash",
  salt: "password salt",
  is_superuser: false,
  created: "2020-02-20 12:12:14"
}
```

The sample data in the default configuration is as follows:

```bash
use mqtt

db.mqtt_user.insert({
  "username": "emqx",
  "password": "efa1f375d76194fa51a3556a97e641e61685f914d446979da50a551a4333ffd7",
  "is_superuser": false,
  "salt": ""
})
```

After MongoDB authentication is enabled, you can connect with username: emqx, password: public.

::: tip 
This is the collection structure used by default configuration. After being familiar with the use of the plugin, you can use any collection that meets the conditions for authentication.
:::



## Salting rules and hash methods

MongoDB authentication support to configure [Salting rules and hash methods](./auth.md#password-salting-rules-and-hash-methods)：

```bash
# etc/plugins/emqx_auth_mongo.conf

auth.mongo.password_hash = sha256
```


## auth_selector

During authentication, EMQX Broker will use the current client information to populate and execute the user-configured authentication SQL to query the client's authentication data in the database.

MongoDB supported configuration collection name, password field, and selector command

```bash
# etc/plugins/emqx_auth_mongo.conf

auth.mongo.auth_query.collection = mqtt_user

## If salting is enabled, it needs to be configured as password,salt
## Value:  password | password,salt
auth.mongo.auth_query.password_field = password

auth.mongo.auth_query.selector = username=%u
```

You can use the following placeholders in the selector, and EMQX Broker will be automatically populated with client information when executed:

- %u：Username
- %c：Client ID
- %C：TLS certificate common name (the domain name or subdomain name of the certificate), valid only for TLS connections
- %d：TLS certificate subject, valid only for TLS connections

You can adjust the authentication query according to business to achieve more business-related functions, such as adding multiple query conditions and using database preprocessing functions. However, in any case, the authentication query must meet the following conditions:

1. The query result must include the password field, which is used by EMQX Broker to compare with the client password
2. If the salting configuration is enabled, the query result must include the salt field, which is used by EMQX Broker as the salt value
3. MongoDB uses the findOne query command to ensure that the query results you expect are shown in the first data
