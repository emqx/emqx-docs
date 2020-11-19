# Plugins 

Through module registration and hooks (Hooks) mechanism user can develop plugin to customize authentication and service functions for EMQ X. 

The official plug-ins provided by EMQ X include: 

Plugin                                                                    |  Configuration file                    |  Description                        
--------------------------------------------------------------------------|----------------------------------------|-------------------------------------
[ emqx_dashboard ](https://github.com/emqx/emqx-dashboard) \+             |  etc/plugins/emqx_dashbord.conf        |  Web dashboard Plugin (Default)     
[ emqx_management ](https://github.com/emqx/emqx-management) \+           |  etc/plugins/emqx_management.conf      |  HTTP API and CLI Management Plugin 
[ emqx_auth_clientid ](https://github.com/emqx/emqx-auth-clientid) \+     |  etc/plugins/emqx_auth_clientid.conf   |  ClientId Auth Plugin               
[ emqx_auth_username ](https://github.com/emqx/emqx-auth-username) \+     |  etc/plugins/emqx_auth_username.conf   |  Username/Password Auth Plugin      
[ emqx_auth_jwt ](https://github.com/emqx/emqx-auth-jwt) \+               |  etc/plugins/emqx_auth_jwt.conf        |  JWT Auth/access control            
[ emqx_auth_ldap ](https://github.com/emqx/emqx-auth-ldap) \+             |  etc/plugins/emqx_auth_ldap.conf       |  LDAP Auth/access control           
[ emqx_auth_http ](https://github.com/emqx/emqx-auth-http) \+             |  etc/plugins/emqx_auth_http.conf       |  HTTP Auth/access control           
[ emqx_auth_mongo ](https://github.com/emqx/emqx-auth-mongo) \+           |  etc/plugins/emqx_auth_mongo.conf      |  MongoDB Auth/access control        
[ emqx_auth_mysql ](https://github.com/emqx/emqx-auth-mysql) \+           |  etc/plugins/emqx_auth_mysql.conf      |  MySQL Auth/access control          
[ emqx_auth_pgsql ](https://github.com/emqx/emqx-auth-pgsql) \+           |  etc/plugins/emqx_auth_pgsql.conf      |  PostgreSQL Auth/access control     
[ emqx_auth_redis ](https://github.com/emqx/emqx-auth-redis) \+           |  etc/plugins/emqx_auth_redis.conf      |  Redis Auth/access control          
[ emqx_psk_file ](https://github.com/emqx/emqx-psk-file) \+               |  etc/plugins/emqx_psk_file.conf        |  PSK support                        
[ emqx_web_hook ](https://github.com/emqx/emqx-web-hook) \+               |  etc/plugins/emqx_web_hook.conf        |  Web Hook Plugin                    
[ emqx_lua_hook ](https://github.com/emqx/emqx-lua-hook) \+               |  etc/plugins/emqx_lua_hook.conf        |  Lua Hook Plugin                    
[ emqx_retainer ](https://github.com/emqx/emqx-retainer) \+               |  etc/plugins/emqx_retainer.conf        |  Retain Message storage module      
[ emqx_rule_engine ](https://github.com/emqx/emqx-rule-engine) \+         |  etc/plugins/emqx_rule_engine.conf     |  Rule engine                        
[ emqx_bridge_mqtt ](https://github.com/emqx/emqx-bridge-mqtt) \+         |  etc/plugins/emqx_bridge_mqtt.conf     |  MQTT Message Bridge Plugin         
[ emqx_delayed_publish ](https://github.com/emqx/emqx-delayed-publish) \+ |  etc/plugins/emqx_delayed_publish.conf |  Delayed publish support            
[ emqx_coap ](https://github.com/emqx/emqx-coap) \+                       |  etc/plugins/emqx_coap.conf            |  CoAP protocol support              
[ emqx_lwm2m ](https://github.com/emqx/emqx-lwm2m) \+                     |  etc/plugins/emqx_lwm2m.conf           |  LwM2M protocol support             
[ emqx_sn ](https://github.com/emqx/emqx-sn) \+                           |  etc/plugins/emqx_sn.conf              |  MQTT-SN protocol support           
[ emqx_stomp ](https://github.com/emqx/emqx-stomp) \+                     |  etc/plugins/emqx_stomp.conf           |  Stomp protocol support             
[ emqx_recon ](https://github.com/emqx/emqx-recon) \+                     |  etc/plugins/emqx_recon.conf           |  Recon performance debugging        
[ emqx_reloader ](https://github.com/emqx/emqx-reloader) \+               |  etc/plugins/emqx_reloader.conf        |  Hot load plugin                    
[ emqx_plugin_template ](https://github.com/emqx/emqx-plugin-template) \+ |  etc/plugins/emqx_plugin_template.conf |  plugin develop template            



There are four ways to load plugins: 

  1. Default loading 
  2. Start and stop plugin on command line 
  3. Start and stop plugin on Dashboard 
  4. Start and stop plugin by calling management API 



**Default loading**

If a plugin needs to start with the broker, add this plugin in ` data/loaded_plugins ` . For example, the plugins that are loaded by default are: 
    
    
    emqx_management.
    emqx_rule_engine.
    emqx_recon.
    emqx_retainer.
    emqx_dashboard.

**Start and stop plugin on command line**

When the EMQ X is running, plugin list can be displayed and plugins can be loaded/unloaded using CLI command: 
    
    
    ## Display a list of all available plugins
    ./bin/emqx_ctl plugins list
    
    ## Load a plugin
    ./bin/emqx_ctl plugins load emqx_auth_username
    
    ## Unload a plugin
    ./bin/emqx_ctl plugins unload emqx_auth_username
    
    ## Reload a plugin
    ./bin/emqx_ctl plugins reload emqx_auth_username

**Start and stop plugin on Dashboard**

If Dashboard plugin is started (by default), the plugins can be also managed on the dashboard. the managing page can be found under ` http://localhost:18083/plugins ` . 

## Dashboard Plugin 

[ emqx_dashboard ](https://github.com/emqx/emqx-dashboard) is the web management console for the EMQ X broker, which is enabled by default. When EMQ X starts successfully, user can access it by visiting ` http://localhost:18083 ` with the default username/password: admin/public. 

> The basic information, statistics, and load status of the EMQ X broker, as well as the current client list (Connections), Sessions, Routing Table (Topics), and Subscriptions can be queried through dashboard. 

![image](./_static/images/dashboard.png)

In addition, dashboard provides a set of REST APIs for front-end calls. See ` Dashboard -> HTTP API ` for details. 

### Dashboard plugin settings 

etc/plugins/emqx_dashboard.conf: 
    
    
    ## Dashboard default username/password
    dashboard.default_user.login = admin
    dashboard.default_user.password = public
    
    ## Dashboard HTTP service Port Configuration
    dashboard.listener.http = 18083
    dashboard.listener.http.acceptors = 2
    dashboard.listener.http.max_clients = 512
    
    ## Dashboard HTTPS service Port Configuration
    ## dashboard.listener.https = 18084
    ## dashboard.listener.https.acceptors = 2
    ## dashboard.listener.https.max_clients = 512
    ## dashboard.listener.https.handshake_timeout = 15s
    ## dashboard.listener.https.certfile = etc/certs/cert.pem
    ## dashboard.listener.https.keyfile = etc/certs/key.pem
    ## dashboard.listener.https.cacertfile = etc/certs/cacert.pem
    ## dashboard.listener.https.verify = verify_peer
    ## dashboard.listener.https.fail_if_no_peer_cert = true

## HTTP API and CLI Management Plugin 

[ emqx_management ](https://github.com/emqx/emqx-management) is the HTTP API and CLI management plugin of the *EMQ X* broker，which is enabled by default. When *EMQ X* starts successfully, users can query the current client list and other operations via the HTTP API and CLI provided by this plugin. For details see ` rest_api ` and ` commands ` . 

### HTTP API and CLI Management Configuration 

etc/plugins/emqx_management.conf: 
    
    
    ## Max Row Limit
    management.max_row_limit = 10000
    
    ## Default Application Secret
    # management.application.default_secret = public
    
    ## Management HTTP Service Port Configuration
    management.listener.http = 8080
    management.listener.http.acceptors = 2
    management.listener.http.max_clients = 512
    management.listener.http.backlog = 512
    management.listener.http.send_timeout = 15s
    management.listener.http.send_timeout_close = on
    
    ## Management HTTPS Service Port Configuration
    ## management.listener.https = 8081
    ## management.listener.https.acceptors = 2
    ## management.listener.https.max_clients = 512
    ## management.listener.https.backlog = 512
    ## management.listener.https.send_timeout = 15s
    ## management.listener.https.send_timeout_close = on
    ## management.listener.https.certfile = etc/certs/cert.pem
    ## management.listener.https.keyfile = etc/certs/key.pem
    ## management.listener.https.cacertfile = etc/certs/cacert.pem
    ## management.listener.https.verify = verify_peer
    ## management.listener.https.fail_if_no_peer_cert = true

## ClientID authentication plugin 

[ emqx_auth_clientid ](https://github.com/emqx/emqx-auth-clientid) currently only supports connection authentication, it authenticates the client through ` clientid ` and ` password ` . When the password is stored, it can be encrypted according the configuration. 

### ClientID Authentication Configuration 

etc/plugins/emqx_auth_clientid.conf: 
    
    
    ## Default usernames Examples
    ##auth.client.1.clientid = id
    ##auth.client.1.password = passwd
    ##auth.client.2.clientid = dev:devid
    ##auth.client.2.password = passwd2
    ##auth.client.3.clientid = app:appid
    ##auth.client.3.password = passwd3
    ##auth.client.4.clientid = client~!@#$%^&*()_+
    ##auth.client.4.password = passwd~!@#$%^&*()_+
    
    ## Password encryption method
    ## Enumeration value: plain | md5 | sha | sha256
    auth.client.password_hash = sha256

## Username Authentication Plugin 

[ emqx_auth_username ](https://github.com/emqx/emqx-auth-username) currently only supports connection authentication, it authenticates the client through ` username ` and ` password ` . When the password is stored, it can be encrypted according the configuration. 

### Username Authentication Configuration 

etc/plugins/emqx_auth_username.conf: 
    
    
    ## Default usernames Examples:
    ##auth.user.1.username = admin
    ##auth.user.1.password = public
    ##auth.user.2.username = feng@emqtt.io
    ##auth.user.2.password = public
    ##auth.user.3.username = name~!@#$%^&*()_+
    ##auth.user.3.password = pwsswd~!@#$%^&*()_+
    
    ## Password encryption method
    ## Enumeration value: plain | md5 | sha | sha256
    auth.user.password_hash = sha256

## JWT Authentication Plugin 

[ emqx_auth_jwt ](https://github.com/emqx/emqx-auth-jwt) supports a [ JWT ](https://jwt.io) -based way to authenticate connected clients and only supports connection authentication. It parses and verifies the legitimacy and timeliness of the Token, and allows connection when satisfied. 

### JWT Authentication Configuration 

etc/plugins/emqx_auth_jwt.conf: 
    
    
    ## HMAC Hash Algorithm Key
    auth.jwt.secret = emqxsecret
    
    ## RSA or ECDSA algorithm's public key
    ## auth.jwt.pubkey = etc/certs/jwt_public_key.pem
    
    ## JWT Source of the string
    ## Enumeration value: username | password
    auth.jwt.from = password

## LDAP Authentication/Access Control Plugin 

[ emqx_auth_ldap ](https://github.com/emqx/emqx-auth-ldap) Emqx_auth_ldap supports access to [ LDAP ](https://ldap.com) for connection authentication and access control. 

### LDAP Authentication Plugin Configuration 

etc/plugins/emqx_auth_ldap.conf: 
    
    
    auth.ldap.servers = 127.0.0.1
    
    auth.ldap.port = 389
    
    auth.ldap.pool = 8
    
    auth.ldap.bind_dn = cn=root,dc=emqx,dc=io
    
    auth.ldap.bind_password = public
    
    auth.ldap.timeout = 30s
    
    auth.ldap.device_dn = ou=device,dc=emqx,dc=io
    
    auth.ldap.match_objectclass = mqttUser
    
    auth.ldap.username.attributetype = uid
    
    auth.ldap.password.attributetype = userPassword
    
    auth.ldap.ssl = false
    
    ## auth.ldap.ssl.certfile = etc/certs/cert.pem
    
    ## auth.ldap.ssl.keyfile = etc/certs/key.pem
    
    ## auth.ldap.ssl.cacertfile = etc/certs/cacert.pem
    
    ## auth.ldap.ssl.verify = verify_peer
    
    ## auth.ldap.ssl.fail_if_no_peer_cert = true

## HTTP Authentication/Access Control Plugin 

[ emqx_auth_http ](https://github.com/emqx/emqx-auth-http) implements connection authentication and access control via HTTP. It sends request to a specified HTTP service and determines whether it has access rights by the return value. 

This plugin supports three requests: 

  1. **auth.http.auth_req** : connection authentication 
  2. **auth.http.super_req** : determine if it is a superuser 
  3. **auth.http.acl_req** : Access Control Rights Query 



The request's parameter can be customized using the client's Username, IP address, and so on. 

::: tip Tip
%C %d support is added in the 3.1 version. 
:::

### HTTP Authentication Plugin Configuration 

etc/plugins/emqx_auth_http.conf: 
    
    
    ## Time-out time for the http request, 0 is never timeout.
    ## auth.http.request.timeout = 0
    
    ## Connection time-out time, used during the initial request when the client is connecting to the server
    ## auth.http.request.connect_timout = 0
    
    ## Re-send http reuqest times
    auth.http.request.retry_times = 3
    
    ## The interval for re-sending the http request
    auth.http.request.retry_interval = 1s
    
    ## The 'Exponential Backoff' mechanism for re-sending request. The actually re-send time interval is `interval * backoff ^ times`
    auth.http.request.retry_backoff = 2.0
    
    ## https certification configuration
    ## auth.http.ssl.cacertfile = {{ platform_etc_dir }}/certs/ca.pem
    ## auth.http.ssl.certfile = {{ platform_etc_dir }}/certs/client-cert.pem
    ## auth.http.ssl.keyfile = {{ platform_etc_dir }}/certs/client-key.pem
    
    ## Placeholder:
    ##  - %u: username
    ##  - %c: clientid
    ##  - %a: ipaddress
    ##  - %P: password
    ##  - %C: common name of client TLS cert
    ##  - %d: subject of client TLS cert
    auth.http.auth_req = http://127.0.0.1:8080/mqtt/auth
    
    ## HTTP method and parameter configuration for AUTH requests
    auth.http.auth_req.method = post
    auth.http.auth_req.params = clientid=%c,username=%u,password=%P
    
    auth.http.super_req = http://127.0.0.1:8080/mqtt/superuser
    auth.http.super_req.method = post
    auth.http.super_req.params = clientid=%c,username=%u
    
    ## Placeholder:
    ##  - %A: 1 | 2, 1 = sub, 2 = pub
    ##  - %u: username
    ##  - %c: clientid
    ##  - %a: ipaddress
    ##  - %t: topic
    auth.http.acl_req = http://127.0.0.1:8080/mqtt/acl
    auth.http.acl_req.method = get
    auth.http.acl_req.params = access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t

### HTTP API Return Value Processing 

**Connection authentication** ： 
    
    
    ## Authentication succeeded
    HTTP Status Code: 200
    
    ## Ignore this certification
    HTTP Status Code: 200
    Body: ignore
    
    ## Authentication failed
    HTTP Status Code: other than 200

**Super user** ： 
    
    
    ## Confirm as super user
    HTTP Status Code: 200
    
    ## Non-super user
    HTTP Status Code: other than 200

**Access control** ： 
    
    
    ##  Allow  Publish/Subscribe：
    HTTP Status Code: 200
    
    ## Ignore this authentication:
    HTTP Status Code: 200
    Body: ignore
    
    ## Deny this Publish/Subscribe:
    HTTP Status Code: other than 200

## MySQL Authentication/Access Control Plugin 

[ emqx_auth_mysql ](https://github.com/emqx/emqx-auth-mysql) supports accessing MySQL for connection authentication and access control. To use these features, it is necessary to create two tables in MySQL as following: 

### MQTT user table 
    
    
    CREATE TABLE `mqtt_user` (
      `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
      `username` varchar(100) DEFAULT NULL,
      `password` varchar(100) DEFAULT NULL,
      `salt` varchar(35) DEFAULT NULL,
      `is_superuser` tinyint(1) DEFAULT 0,
      `created` datetime DEFAULT NULL,
      PRIMARY KEY (`id`),
      UNIQUE KEY `mqtt_username` (`username`)
    ) ENGINE=MyISAM DEFAULT CHARSET=utf8;

::: tip Tip
The plugin also supports tables with custom structures, which can be realized by the query statement configuration via ` auth_query ` . 
:::

### MQTT Access Control Table 
    
    
    CREATE TABLE `mqtt_acl` (
      `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
      `allow` int(1) DEFAULT NULL COMMENT '0: deny, 1: allow',
      `ipaddr` varchar(60) DEFAULT NULL COMMENT 'IpAddress',
      `username` varchar(100) DEFAULT NULL COMMENT 'Username',
      `clientid` varchar(100) DEFAULT NULL COMMENT 'ClientId',
      `access` int(2) NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',
      `topic` varchar(100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',
      PRIMARY KEY (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8;
    
    INSERT INTO `mqtt_acl` (`id`, `allow`, `ipaddr`, `username`, `clientid`, `access`, `topic`)
    VALUES
        (1,1,NULL,'$all',NULL,2,'#'),
        (2,0,NULL,'$all',NULL,1,'$SYS/#'),
        (3,0,NULL,'$all',NULL,1,'eq #'),
        (5,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),
        (6,1,'127.0.0.1',NULL,NULL,2,'#'),
        (7,1,NULL,'dashboard',NULL,1,'$SYS/#');

### MySQL Authentication Plugin Configuration 

etc/plugins/emqx_auth_mysql.conf: 
    
    
    ## Mysql server address
    auth.mysql.server = 127.0.0.1:3306
    
    ## Mysql connection pool size
    auth.mysql.pool = 8
    
    ## Mysql connection username
    ## auth.mysql.username =
    
    ## Mysql connection password
    ## auth.mysql.password =
    
    ## Mysql authentication user table name
    auth.mysql.database = mqtt
    
    ## Mysql query timeout
    auth.mysql.query_timeout = 5s
    
    ## Available placeholders:
    ##  - %u: username
    ##  - %c: clientid
    ##  - %C: common name of client TLS cert
    ##  - %d: subject of client TLS cert
    ## Note: This SQL queries `password` field only
    auth.mysql.auth_query = select password from mqtt_user where username = '%u' limit 1
    
    ## Password encryption method: plain, md5, sha, sha256, pbkdf2
    auth.mysql.password_hash = sha256
    
    ##  Query statement for super user
    auth.mysql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1
    
    ## ACL query statement
    ## Note: You can add the 'ORDER BY' statement to control the rules match order
    auth.mysql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'

To prevent the security issue caused by password being too simple, the plugin also supports password salting: 
    
    
    ## Salted ciphertext format
    ## auth.mysql.password_hash = salt,sha256
    ## auth.mysql.password_hash = salt,bcrypt
    ## auth.mysql.password_hash = sha256,salt
    
    ## pbkdf2 with macfun format
    ## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
    ## auth.mysql.password_hash = pbkdf2,sha256,1000,20

::: tip Tip
%C %d support is added in version 3.1. 
:::

## PostgreSQL Authentication Plugin 

[ emqx_auth_pgsql ](https://github.com/emqx/emqx-auth-pgsql) implements connection authentication and access control by PostgreSQL. Two tables are required to be created as follows: 

### Postgres MQTT User Table 
    
    
    CREATE TABLE mqtt_user (
      id SERIAL primary key,
      is_superuser boolean,
      username character varying(100),
      password character varying(100),
      salt character varying(40)
    );

### PostgreSQL MQTT Access Control Table 
    
    
    CREATE TABLE mqtt_acl (
      id SERIAL primary key,
      allow integer,
      ipaddr character varying(60),
      username character varying(100),
      clientid character varying(100),
      access  integer,
      topic character varying(100)
    );
    
    INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)
    VALUES
        (1,1,NULL,'$all',NULL,2,'#'),
        (2,0,NULL,'$all',NULL,1,'$SYS/#'),
        (3,0,NULL,'$all',NULL,1,'eq #'),
        (5,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),
        (6,1,'127.0.0.1',NULL,NULL,2,'#'),
        (7,1,NULL,'dashboard',NULL,1,'$SYS/#');

### Postgres Authentication Plugin Configuration 

etc/plugins/emqx_auth_pgsql.conf: 
    
    
    ## PostgreSQL Service Address
    auth.pgsql.server = 127.0.0.1:5432
    
    ## PostgreSQL connection pool size
    auth.pgsql.pool = 8
    
    auth.pgsql.username = root
    
    ## auth.pgsql.password =
    
    auth.pgsql.database = mqtt
    
    auth.pgsql.encoding = utf8
    
    ## Connection authentication query SQL
    ## Placeholder:
    ##  - %u: username
    ##  - %c: clientid
    ##  - %C: common name of client TLS cert
    ##  - %d: subject of client TLS cert
    auth.pgsql.auth_query = select password from mqtt_user where username = '%u' limit 1
    
    ## Encryption method: plain | md5 | sha | sha256 | bcrypt
    auth.pgsql.password_hash = sha256
    
    ## Query Statement for super user (Usage of placeholders is consistent with which of auth_query)
    auth.pgsql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1
    
    ## ACL query statement
    ##
    ## Placeholder:
    ##  - %a: ipaddress
    ##  - %u: username
    ##  - %c: clientid
    ## Note: You can add the 'ORDER BY' statement to control the rules match order
    auth.pgsql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'

The password_hash can be configured for higher security: 
    
    
    ## Salted Encryption Format
    ## auth.pgsql.password_hash = salt,sha256
    ## auth.pgsql.password_hash = sha256,salt
    ## auth.pgsql.password_hash = salt,bcrypt
    
    ## pbkdf2 macfun format
    ## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
    ## auth.pgsql.password_hash = pbkdf2,sha256,1000,20

Enable the following configuration to support TLS connections to PostgreSQL: 
    
    
    ## Whether to enable SSL
    auth.pgsql.ssl = false
    
    ## Certificate Configuration
    ## auth.pgsql.ssl_opts.keyfile =
    ## auth.pgsql.ssl_opts.certfile =
    ## auth.pgsql.ssl_opts.cacertfile =

::: tip Tip
%C %d support is added in version 3.1. 
:::

## Redis Authentication/Access Control Plugin 

[ emqx_auth_redis ](https://github.com/emqx/emqx-auth-redis) implements connection authentication and access control functions by Redis. 

### Redis Authentication Plugin Configuration 

etc/plugins/emqx_auth_redis.conf: 
    
    
    ## Redis Service Cluster Type
    ## enumeration value: single | sentinel | cluster
    auth.redis.type = single
    
    ## Redis Server Address
    ##
    ## Single Redis Server: 127.0.0.1:6379, localhost:6379
    ## Redis Sentinel: 127.0.0.1:26379,127.0.0.2:26379,127.0.0.3:26379
    ## Redis Cluster: 127.0.0.1:6379,127.0.0.2:6379,127.0.0.3:6379
    auth.redis.server = 127.0.0.1:6379
    
    ## Redis sentinel name
    ## auth.redis.sentinel = mymaster
    
    ## Redis connection pool size
    auth.redis.pool = 8
    
    ## Redis database number
    auth.redis.database = 0
    
    ## Redis password
    ## auth.redis.password =
    
    ## Redis query timeout
    auth.redis.query_timeout = 5s
    
    ## Authentication Query Command
    ## Placeholder:
    ##  - %u: username
    ##  - %c: clientid
    ##  - %C: common name of client TLS cert
    ##  - %d: subject of client TLS cert
    auth.redis.auth_cmd = HMGET mqtt_user:%u password
    
    ## Password encryption method.
    ## enumeration value: plain | md5 | sha | sha256 | bcrypt
    auth.redis.password_hash = plain
    
    ## Super User Query Command (Usage of placeholders is consistent with which authentication)
    auth.redis.super_cmd = HGET mqtt_user:%u is_superuser
    
    ## ACL query command
    ##  Placeholder:
    ##  - %u: username
    ##  - %c: clientid
    auth.redis.acl_cmd = HGETALL mqtt_acl:%u

The password can be hashed for higher security: 
    
    
    ## Salted password hash
    ## auth.redis.password_hash = salt,sha256
    ## auth.redis.password_hash = sha256,salt
    ## auth.redis.password_hash = salt,bcrypt
    
    ## pbkdf2 macfun format
    ## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
    ## auth.redis.password_hash = pbkdf2,sha256,1000,20

::: tip Tip
%C %d support is added in version 3.1. 
:::

### Redis User Hash 

The default authentication is User Hash: 
    
    
    HSET mqtt_user:\<username> is_superuser 1
    HSET mqtt_user:\<username> password "passwd"
    HSET mqtt_user:\<username> salt "salt"

### Redis ACL Rule Hash 

> ACL rules is stored in Hash by default. 
    
    
    HSET mqtt_acl:\<username> topic1 1
    HSET mqtt_acl:\<username> topic2 2
    HSET mqtt_acl:\<username> topic3 3

::: tip Tip
1: subscribe, 2: publish, 3: pubsub 
:::

## MongoDB Authentication/Access Control Plugin 

[ emqx_auth_mongo ](https://github.com/emqx/emqx-auth-mongo) implements connection authentication and access control by MongoDB. 

### MongoDB authentication plugin configuration 

etc/plugins/emqx_auth_mongo.conf: 
    
    
    ## MongoDB topology type
    ## enumeration:  single | unknown | sharded | rs
    auth.mongo.type = single
    
    ## `set name` under rs mode
    ## auth.mongo.rs_set_name =
    
    ## MongoDB Service Address
    auth.mongo.server = 127.0.0.1:27017
    
    ## MongoDB connection pool size
    auth.mongo.pool = 8
    
    ## Connection authentication information
    ## auth.mongo.login =
    ## auth.mongo.password =
    ## auth.mongo.auth_source = admin
    
    ## Authentication data table name
    auth.mongo.database = mqtt
    
    ## MongoDB query timeout
    auth.mongo.query_timeout = 5s
    
    ## Authentication Query Configuration
    auth.mongo.auth_query.collection = mqtt_user
    auth.mongo.auth_query.password_field = password
    auth.mongo.auth_query.password_hash = sha256
    
    ## Connection Authentication Query Field List
    ## Placeholder:
    ##  - %u: username
    ##  - %c: clientid
    ##  - %C: common name of client TLS cert
    ##  - %d: subject of client TLS cert
    auth.mongo.auth_query.selector = username=%u
    
    ## Super User Query
    auth.mongo.super_query = on
    auth.mongo.super_query.collection = mqtt_user
    auth.mongo.super_query.super_field = is_superuser
    auth.mongo.super_query.selector = username=%u
    
    ## ACL  Query Configuration
    auth.mongo.acl_query = on
    auth.mongo.acl_query.collection = mqtt_acl
    
    auth.mongo.acl_query.selector = username=%u

::: tip Tip
%C %d support is added in version 3.1. 
:::

### MongoDB database 
    
    
    use mqtt
    db.createCollection("mqtt_user")
    db.createCollection("mqtt_acl")
    db.mqtt_user.ensureIndex({"username":1})

::: tip Tip
The name of database and collection can be customized. 
:::

### MongoDB user collection 
    
    
    {
        username: "user",
        password: "password hash",
        is_superuser: boolean (true, false),
        created: "datetime"
    }

Example: 
    
    
    db.mqtt_user.insert({username: "test", password: "password hash", is_superuser: false})
    db.mqtt_user:insert({username: "root", is_superuser: true})

### MongoDB ACL collection 
    
    
    {
        username: "username",
        clientid: "clientid",
        publish: ["topic1", "topic2", ...],
        subscribe: ["subtop1", "subtop2", ...],
        pubsub: ["topic/#", "topic1", ...]
    }

Example: 
    
    
    db.mqtt_acl.insert({username: "test", publish: ["t/1", "t/2"], subscribe: ["user/%u", "client/%c"]})
    db.mqtt_acl.insert({username: "admin", pubsub: ["#"]})

## PSK Authentication Plugin 

[ emqx_psk_file ](https://github.com/emqx/emqx-psk-file) mainly provides PSK support that aimes to implement connection authentication through PSK when the client establishes a TLS/DTLS connection. 

### PSK Authentication Plugin Configuration 

etc/plugins/emqx_psk_file.conf: 
    
    
    psk.file.path = etc/psk.txt

## WebHook Plugin 

[ emqx_web_hook ](https://github.com/emqx/emqx-web-hook) can send all EMQ X events and messages to the specified HTTP server. 

### WebHook plugin configuration 

etc/plugins/emqx_web_hook.conf: 
    
    
    ## Callback Web Server Address
    web.hook.api.url = http://127.0.0.1:8080
    
    ## Encode message payload field
    ## Value: undefined | base64 | base62
    ## Default: undefined (Do not encode)
    ## web.hook.encode_payload = base64
    
    ## Message and event configuration
    web.hook.rule.client.connected.1     = {"action": "on_client_connected"}
    web.hook.rule.client.disconnected.1  = {"action": "on_client_disconnected"}
    web.hook.rule.client.subscribe.1     = {"action": "on_client_subscribe"}
    web.hook.rule.client.unsubscribe.1   = {"action": "on_client_unsubscribe"}
    web.hook.rule.session.created.1      = {"action": "on_session_created"}
    web.hook.rule.session.subscribed.1   = {"action": "on_session_subscribed"}
    web.hook.rule.session.unsubscribed.1 = {"action": "on_session_unsubscribed"}
    web.hook.rule.session.terminated.1   = {"action": "on_session_terminated"}
    web.hook.rule.message.publish.1      = {"action": "on_message_publish"}
    web.hook.rule.message.deliver.1      = {"action": "on_message_deliver"}
    web.hook.rule.message.acked.1        = {"action": "on_message_acked"}

## Lua Plugin 

[ emqx_lua_hook ](https://github.com/emqx/emqx-lua-hook) sends all events and messages to the specified Lua function. See its README for specific use. 

## Retainer Plugin 

[ emqx_retainer ](https://github.com/emqx/emqx-retainer) is set to start by default and provides Retained type message support for EMQ X. It stores the Retained messages for all topics in the cluster's database and posts the message when the client subscribes to the topic 

### Retainer Plugin Configuration 

etc/plugins/emqx_retainer.conf: 
    
    
    ## retained Message storage method
    ##  - ram: memory only
    ##  - disc: memory and disk
    ##  - disc_only: disk only
    retainer.storage_type = ram
    
    ## Maximum number of storage (0 means unrestricted)
    retainer.max_retained_messages = 0
    
    ## Maximum storage size for single message
    retainer.max_payload_size = 1MB
    
    ## Expiration time, 0 means never expired
    ## Unit:  h hour; m minute; s second.For example, 60m means 60 minutes.
    retainer.expiry_interval = 0

## MQTT Message Bridge Plugin 

The concept of **Bridge** is that EMQ X forwards messages of some of its topics to another MQTT Broker in some way. 

Difference between **Bridge** and **cluster** is that bridge does not replicate topic trees and routing tables, a bridge only forwards MQTT messages based on Bridge rules. 

Currently the Bridge methods supported by EMQ X are as follows: 

  * RPC bridge: RPC Bridge only supports message forwarding and does not support subscribing to the topic of remote nodes to synchronize data. 
  * MQTT Bridge: MQTT Bridge supports both forwarding and data synchronization through subscription topic 



In EMQ X, bridge is configured by modifying ` etc/plugins/emqx_bridge_mqtt.conf ` . EMQ X distinguishes between different bridges based on different names. E.g: 
    
    
    ## Bridge address: node name for local bridge, host:port for remote.
    bridge.mqtt.aws.address = 127.0.0.1:1883

This configuration declares a bridge named ` aws ` and specifies that it is bridged to the MQTT server of ` 127.0.0.1:1883 ` by MQTT mode. 

In case of creating multiple bridges, it is convenient to replicate all configuration items of the first bridge, and modify the bridge name and other configuration items if necessary (such as bridge.mqtt.$name.address, where $name refers to the name of bridge) 

### MQTT Bridge Plugin Configuration 

etc/plugins/emqx_bridge_mqtt.conf 
    
    
    ## Bridge Address: Use node name (nodename@host) for rpc Bridge, and host:port for mqtt connection
    bridge.mqtt.aws.address = emqx2@192.168.1.2
    
    ## Forwarding topics of the message
    bridge.mqtt.aws.forwards = sensor1/#,sensor2/#
    
    ## bridged mountpoint
    bridge.mqtt.aws.mountpoint = bridge/emqx2/${node}/
    
    ## Bridge Address: Use node name for rpc Bridge, use host:port for mqtt connection
    bridge.mqtt.aws.address = 192.168.1.2:1883
    
    ## Bridged Protocol Version
    ## Enumeration value: mqttv3 | mqttv4 | mqttv5
    bridge.mqtt.aws.proto_ver = mqttv4
    
    ## mqtt client's client_id
    bridge.mqtt.aws.client_id = bridge_emq
    
    ## mqtt client's clean_start field
    ## Note: Some MQTT Brokers need to set the clean_start value as `true`
    bridge.mqtt.aws.clean_start = true
    
    ##  mqtt client's username field
    bridge.mqtt.aws.username = user
    
    ## mqtt client's password field
    bridge.mqtt.aws.password = passwd
    
    ## Whether the mqtt client uses ssl to connect to a remote serve or not
    bridge.mqtt.aws.ssl = off
    
    ## CA Certificate of Client SSL Connection (PEM format)
    bridge.mqtt.aws.cacertfile = etc/certs/cacert.pem
    
    ## SSL certificate of Client SSL connection
    bridge.mqtt.aws.certfile = etc/certs/client-cert.pem
    
    ## Key file of Client SSL connection
    bridge.mqtt.aws.keyfile = etc/certs/client-key.pem
    
    ## SSL encryption
    bridge.mqtt.aws.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384
    
    ## TTLS PSK password
    ## Note 'listener.ssl.external.ciphers' and 'listener.ssl.external.psk_ciphers' cannot be configured at the same time
    ##
    ## See 'https://tools.ietf.org/html/rfc4279#section-2'.
    ## bridge.mqtt.aws.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA
    
    ## Client's heartbeat interval
    bridge.mqtt.aws.keepalive = 60s
    
    ## Supported TLS version
    bridge.mqtt.aws.tls_versions = tlsv1.2,tlsv1.1,tlsv1
    
    ## Forwarding topics of the message
    bridge.mqtt.aws.forwards = sensor1/#,sensor2/#
    
    ## Bridged mountpoint
    bridge.mqtt.aws.mountpoint = bridge/emqx2/${node}/
    
    ## Subscription topic for Bridge
    bridge.mqtt.aws.subscription.1.topic = cmd/topic1
    
    ## Subscription qos for Bridge
    bridge.mqtt.aws.subscription.1.qos = 1
    
    ## Subscription topic for Bridge
    bridge.mqtt.aws.subscription.2.topic = cmd/topic2
    
    ## Subscription qos for Bridge
    bridge.mqtt.aws.subscription.2.qos = 1
    
    ## Bridge reconnection interval
    ## Default: 30s
    bridge.mqtt.aws.reconnect_interval = 30s
    
    ## QoS1 message retransmission interval
    bridge.mqtt.aws.retry_interval = 20s
    
    ## Inflight Size.
    bridge.mqtt.aws.max_inflight_batches = 32
    
    ## emqx_bridge internal number of messages used for batch
    bridge.mqtt.aws.queue.batch_count_limit = 32
    
    ##  emqx_bridge internal number of message bytes used for batch
    bridge.mqtt.aws.queue.batch_bytes_limit = 1000MB
    
    ## The path for placing replayq queue. If the item is not specified in the configuration, then replayq will run in `mem-only` mode and messages will not be cached on disk.
    bridge.mqtt.aws.queue.replayq_dir = data/emqx_emqx2_bridge/
    
    ## Replayq data segment size
    bridge.mqtt.aws.queue.replayq_seg_bytes = 10MB

## Delayed Publish Plugin 

[ emqx_delayed_publish ](https://github.com/emqx/emqx-delayed-publish) provides the function to delay publishing messages. When the client posts a message to EMQ X using the special topic prefix ` $delayed/\<seconds>/ ` , EMQ X will publish this message after \<seconds> seconds. 

## CoAP Protocol Plugin 

[ emqx_coap ](https://github.com/emqx/emqx-coap) provides support for the CoAP protocol (RFC 7252)。 

### CoAP protocol Plugin Configuration 

etc/plugins/emqx_coap.conf: 
    
    
    coap.port = 5683
    
    coap.keepalive = 120s
    
    coap.enable_stats = off

DTLS can be enabled if the following two configuration items are set: 
    
    
    ## Listen port for DTLS
    coap.dtls.port = 5684
    
    coap.dtls.keyfile = {{ platform_etc_dir }}/certs/key.pem
    coap.dtls.certfile = {{ platform_etc_dir }}/certs/cert.pem
    
    ## DTLS options
    ## coap.dtls.verify = verify_peer
    ## coap.dtls.cacertfile = {{ platform_etc_dir }}/certs/cacert.pem
    ## coap.dtls.fail_if_no_peer_cert = false

### Test the CoAP Plugin 

A CoAP client is necessary to test CoAP plugin. In following example the [ libcoap ](https://github.com/obgm/libcoap) is used. 
    
    
    yum install libcoap
    
    % coap client publish message
    coap-client -m put -e "qos=0&retain=0&message=payload&topic=hello" coap://localhost/mqtt

## LwM2M Protocol Plugin 

[ emqx_lwm2m ](https://github.com/emqx/emqx-lwm2m) provides support for the LwM2M protocol. 

### LwM2M plugin configuration 

etc/plugins/emqx_lwm2m.conf: 
    
    
    ## LwM2M listening port
    lwm2m.port = 5683
    
    ## Lifetime Limit
    lwm2m.lifetime_min = 1s
    lwm2m.lifetime_max = 86400s
    
    ## `time window` length under Q Mode Mode, in seconds.
    ## Messages that exceed the window will be cached
    #lwm2m.qmode_time_window = 22
    
    ## Whether LwM2M is deployed after coaproxy
    #lwm2m.lb = coaproxy
    
    ## Actively observe all objects after the device goes online
    #lwm2m.auto_observe = off
    
    # The topic mountpoint
    # Placeholders supported:
    #    '%e': Endpoint Name
    #    '%a': IP Address
    lwm2m.mountpoint = lwm2m/%e/
    
    ## the subscribed topic from EMQ X after client register succeeded
    ## Placeholder:
    ##    '%e': Endpoint Name
    ##    '%a': IP Address
    lwm2m.topics.command = dn/#
    
    ## client response message to EMQ X topic
    lwm2m.topics.response = up/resp
    
    ## client notify message to EMQ X topic
    lwm2m.topics.notify = up/notify
    
    ## client register message to EMQ X topic
    lwm2m.topics.register = up/resp
    
    # client update message to EMQ X topic
    lwm2m.topics.update = up/resp
    
    # xml file location defined by object
    lwm2m.xml_dir =  etc/lwm2m_xml

DTLS support can be enabled with the following configuration: 
    
    
    # DTLS Certificate Configuration
    lwm2m.certfile = etc/certs/cert.pem
    lwm2m.keyfile = etc/certs/key.pem

## MQTT-SN Protocol Plugin 

[ emqx_sn ](https://github.com/emqx/emqx-sn) provides support for the MQTT-SN protocol 

### MQTT-SN protocol plugin configuration 

etc/plugins/emqx_sn.conf: 
    
    
    mqtt.sn.port = 1884

## Stomp Protocol Plugin 

[ emqx_stomp ](https://github.com/emqx/emqx-stomp) provides support for the Stomp protocol. Clients connect to EMQ X through Stomp 1.0/1.1/1.2 protocol, publish and subscribe to MQTT message. 

### Stomp plugin configuration 

::: tip Tip
Stomp protocol port: 61613 
:::

etc/plugins/emqx_stomp.conf: 
    
    
    stomp.default_user.login = guest
    
    stomp.default_user.passcode = guest
    
    stomp.allow_anonymous = true
    
    stomp.frame.max_headers = 10
    
    stomp.frame.max_header_length = 1024
    
    stomp.frame.max_body_length = 8192
    
    stomp.listener = 61613
    
    stomp.listener.acceptors = 4
    
    stomp.listener.max_clients = 512

## Recon Performance Debugging Plugin 

[ emqx_recon ](https://github.com/emqx/emqx-recon) integrates the recon performance tuning library to view status information about the current system, for example: 
    
    
    ./bin/emqx_ctl recon
    
    recon memory                 #recon_alloc:memory/2
    recon allocated              #recon_alloc:memory(allocated_types, current|max)
    recon bin_leak               #recon:bin_leak(100)
    recon node_stats             #recon:node_stats(10, 1000)
    recon remote_load Mod        #recon:remote_load(Mod)

### Recon Plugin Configuration 

etc/plugins/emqx_recon.conf: 
    
    
    %% Garbage Collection: 10 minutes
    recon.gc_interval = 600

## Reloader Hot Reload Plugin 

[ emqx_reloader ](https://github.com/emqx/emqx-reloader) is used for code hot-upgrade during impelementation and debugging. After loading this plug-in, EMQ X updates the codes automatically according to the configuration interval. 

A CLI command is also provided to force a module to reload: 
    
    
    ./bin/emqx_ctl reload \<Module>

::: tip Tip
This plugin is not recommended for production environments. 
:::

### Reloader Plugin Configuration 

etc/plugins/emqx_reloader.conf: 
    
    
    reloader.interval = 60
    
    reloader.logfile = log/reloader.log

## Plugin Development Template 

[ emqx_plugin_template ](https://github.com/emqx/emqx-plugin-template) is an EMQ X plugin template and provides no functionality by itself. 

When developers need to customize a plugin, they can view this plugin's code and structure to deliver a standard EMQ X plugin faster. The plugin is actually a normal ` Erlang Application ` with the configuration file: ` etc/${PluginName}.config ` . 

## EMQ X R3.2 Plugin Development 

### Create a Plugin Project 

For creating a new plugin project please refer to the [ emqx_plugin_template ](https://github.com/emqx/emqx-plugin-template) . .. 

::: tip Tip
The tag ` -emqx_plugin(?MODULE). ` must be added to the ` \<plugin name>_app.erl ` file to indicate that this is a plugin for EMQ X. 
:::

### Create an Authentication/Access Control Module 

A demo of authentication module - emqx_auth_demo.erl 
    
    
    -module(emqx_auth_demo).
    
    -export([ init/1
            , check/2
            , description/0
            ]).
    
    init(Opts) -> {ok, Opts}.
    
    check(_Credentials = #{client_id := ClientId, username := Username, password := Password}, _State) ->
        io:format("Auth Demo: clientId=~p, username=~p, password=~p~n", [ClientId, Username, Password]),
        ok.
    
    description() -> "Auth Demo Module".

A demo of access control module - emqx_acl_demo.erl 
    
    
    -module(emqx_acl_demo).
    
    -include_lib("emqx/include/emqx.hrl").
    
    %% ACL callbacks
    -export([ init/1
            , check_acl/5
            , reload_acl/1
            , description/0
            ]).
    
    init(Opts) ->
        {ok, Opts}.
    
    check_acl({Credentials, PubSub, _NoMatchAction, Topic}, _State) ->
        io:format("ACL Demo: ~p ~p ~p~n", [Credentials, PubSub, Topic]),
        allow.
    
    reload_acl(_State) ->
        ok.
    
    description() -> "ACL Demo Module".

Registration of authentication, access control module - emqx_plugin_template_app.erl 
    
    
    ok = emqx:hook('client.authenticate', fun emqx_auth_demo:check/2, []),
    ok = emqx:hook('client.check_acl', fun emqx_acl_demo:check_acl/5, []).

### Hooks 

Events of client's online and offline, topic subscription, message sending and receiving can be handled through hooks. 

emqx_plugin_template.erl: 
    
    
    %% Called when the plugin application start
    load(Env) ->
        emqx:hook('client.authenticate', fun ?MODULE:on_client_authenticate/2, [Env]),
        emqx:hook('client.check_acl', fun ?MODULE:on_client_check_acl/5, [Env]),
        emqx:hook('client.connected', fun ?MODULE:on_client_connected/4, [Env]),
        emqx:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
        emqx:hook('client.subscribe', fun ?MODULE:on_client_subscribe/3, [Env]),
        emqx:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/3, [Env]),
        emqx:hook('session.created', fun ?MODULE:on_session_created/3, [Env]),
        emqx:hook('session.resumed', fun ?MODULE:on_session_resumed/3, [Env]),
        emqx:hook('session.subscribed', fun ?MODULE:on_session_subscribed/4, [Env]),
        emqx:hook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4, [Env]),
        emqx:hook('session.terminated', fun ?MODULE:on_session_terminated/3, [Env]),
        emqx:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
        emqx:hook('message.deliver', fun ?MODULE:on_message_deliver/3, [Env]),
        emqx:hook('message.acked', fun ?MODULE:on_message_acked/3, [Env]),
        emqx:hook('message.dropped', fun ?MODULE:on_message_dropped/3, [Env]).

Available hooks description: 

Hooks                |  Description                      
---------------------|-----------------------------------
client.authenticate  |  connection authentication        
client.check_acl     |  ACL validation                   
client.connected     |  client online                    
client.disconnected  |  client disconnected              
client.subscribe     |  subscribe topic by client        
client.unsubscribe   |  unsubscribe topic by client      
session.created      |  session created                  
session.resumed      |  session resumed                  
session.subscribed   |  session after topic subscribed   
session.unsubscribed |  session after topic unsubscribed 
session.terminated   |  session terminated               
message.publish      |  MQTT message publish             
message.deliver      |  MQTT message deliver             
message.acked        |  MQTT message acknowledged        
message.dropped      |  MQTT message dropped             



### Register CLI Command 

Demo module for extending command line - emqx_cli_demo.erl 
    
    
    -module(emqx_cli_demo).
    
    -export([cmd/1]).
    
    cmd(["arg1", "arg2"]) ->
        emqx_cli:print("ok");
    
    cmd(_) ->
        emqx_cli:usage([{"cmd arg1 arg2", "cmd demo"}]).

Register command line module - emqx_plugin_template_app.erl 
    
    
    ok = emqx_ctl:register_command(cmd, {emqx_cli_demo, cmd}, []),

After the plugin is loaded，a new CLI command is added to ` ./bin/emqx_ctl ` ： 
    
    
    ./bin/emqx_ctl cmd arg1 arg2

### Plugin Configuration File 

The plugin comes with a configuration file placed in ` etc/${plugin_name}.conf|config ` . EMQ X supports two plugin configuration formats: 

  1. Erlang native configuration file format - ` ${plugin_name}.config ` : 
    
        [
      {plugin_name, [
        {key, value}
      ]}
    ].

  2. sysctl's ` k = v ` universal forma - ` ${plugin_name}.conf ` : 
    
        plugin_name.key = value




::: tip Tip
` k = v ` format configuration requires the plugin developer to create a ` priv/plugin_name.schema ` mapping file. 
:::

### Compile and Release Plugin 

  1. clone emqx-rel project: 


    
    
    git clone https://github.com/emqx/emqx-rel.git

  2. Add dependency in rebar.config: 


    
    
    {deps,
       [ {plugin_name, {git, "url_of_plugin", {tag, "tag_of_plugin"}}}
       , ....
       ....
       ]
    }

  3. The relx paragraph in rebar.config is added: 


    
    
    {relx,
        [...
        , ...
        , {release, {emqx, git_describe},
           [
             {plugin_name, load},
           ]
          }
        ]
    }
