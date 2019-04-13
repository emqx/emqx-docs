
.. _plugins:


扩展插件 (Plugins)
^^^^^^^^^^^^^^^^^^^


*EMQ X* 消息服务器通过模块注册和钩子(Hooks)机制，支持用户开发扩展插件定制服务器认证鉴权与业务功能。

*EMQ X* 3.0 版本官方提供的插件包括:

+---------------------------+---------------------------+
| 插件                      | 说明                      |
+===========================+===========================+
| `emqx_dashboard`_         | Web 控制台插件(默认加载)  |
+---------------------------+---------------------------+
| `emqx_auth_clientid`_     | ClientId 认证插件         |
+---------------------------+---------------------------+
| `emqx_auth_username`_     | 用户名、密码认证插件      |
+---------------------------+---------------------------+
| `emqx_auth_jwt`_          | JWT 认证/访问控制         |
+---------------------------+---------------------------+
| `emqx_psk_file`_          | PSK 支持                  |
+---------------------------+---------------------------+
| `emqx_auth_ldap`_         | LDAP 认证/访问控制        |
+---------------------------+---------------------------+
| `emqx_auth_http`_         | HTTP 认证/访问控制        |
+---------------------------+---------------------------+
| `emqx_auth_mongo`_        | MongoDB 认证/访问控制     |
+---------------------------+---------------------------+
| `emqx_auth_mysql`_        | MySQL 认证/访问控制       |
+---------------------------+---------------------------+
| `emqx_auth_pgsql`_        | PostgreSQ L认证/访问控制  |
+---------------------------+---------------------------+
| `emqx_auth_redis`_        | Redis 认证/访问控制       |
+---------------------------+---------------------------+
| `emqx_web_hook`_          | Web Hook 插件             |
+---------------------------+---------------------------+
| `emqx_lua_hook`_          | Lua Hook 插件             |
+---------------------------+---------------------------+
| `emqx_retainer`_          | Retain 消息存储模块       |
+---------------------------+---------------------------+
| `emqx_delayed_publish`_   | 客户端延时发布消息支持    |
+---------------------------+---------------------------+
| `emqx_coap`_              | CoAP 协议支持             |
+---------------------------+---------------------------+
| `emqx_lwm2m`_             | LwM2M 协议支持            |
+---------------------------+---------------------------+
| `emqx_sn`_                | MQTT-SN 协议支持          |
+---------------------------+---------------------------+
| `emqx_stomp`_             | Stomp 协议支持            |
+---------------------------+---------------------------+
| `emqx_recon`_             | Recon 性能调试            |
+---------------------------+---------------------------+
| `emqx_reloader`_          | Reloader 代码热加载插件   |
+---------------------------+---------------------------+
| `emqx_plugin_template`_   | 插件开发模版              |
+---------------------------+---------------------------+

其中插件的加载有三种方式：

1. 默认加载
2. CLI 启停插件
3. Dashboard 启停插件

**开启默认加载**

如需在系统启动时就默认启动某插件，则直接在 ``data/loaded_plugins`` 配置入需要启动的插件，例如默认开启的加载的插件有：

.. code:: erlang

    emqx_management.
    emqx_rule_engine.
    emqx_recon.
    emqx_retainer.
    emqx_dashboard.


**CLI 启停插件**

所谓的 CLI 在 *EMQ X* 中特指 ``./bin/emqx_ctl`` 命令行工具。在运行过程中，我们可以通过 CLI 命令的方式查看可用的插件列表、和启停某插件

.. code:: bash

    ## 显示所有可用的插件列表
    ./bin/emqx_ctl plugins list

    ## 加载某插件
    ./bin/emqx_ctl plugins load emqx_auth_username

    ## 卸载某插件
    ./bin/emqx_ctl plugins unload emqx_auth_username

    ## 重新加载某插件
    ./bin/emqx_ctl plugins reload emqx_auth_username


**Dashboard 启停插件**

除上述俩种方式以外，如果 *EMQ X* 开启了 Dashbord 的插件(默认开启) 还可以直接通过 Dashboard 启停、或者配置插件


emqx_dashboard: Dashboard 插件
------------------------------

`emqx_dashboard`_ 是 *EMQ X* 消息服务器的 Web 管理控制台, 该插件默认开启。当 *EMQ X* 启动成功后，可访问 http://localhost:18083 进行查看，默认用户名/密码: admin/public。

Dashboard 插件可查询 *EMQ X* 消息服务器基本信息、统计数据、负载情况，查询当前客户端列表(Connections)、会话(Sessions)、路由表(Topics)、订阅关系(Subscriptions) 等详细信息。

.. image:: ./_static/images/dashboard.png

除此之外，Dashboard 默认提供了一系列的 REST API 供前端调用。其详情可以参考 ``Dashboard -> HTTP API`` 部分


Dashboard 插件设置
::::::::::::::::::

etc/plugins/emqx_dashboard.conf:

.. code:: properties

    ## Dashboard API Providers
    dashboard.api.providers = emqx_management,emqx_dashboard

    ## Default user's login username/password.
    dashboard.default_user.login = admin
    dashboard.default_user.password = public

    ## HTTP Listener
    dashboard.listener.http = 18083
    dashboard.listener.http.acceptors = 2
    dashboard.listener.http.max_clients = 512

    ## HTTPS Listener
    ## dashboard.listener.https = 18084
    ## dashboard.listener.https.acceptors = 2
    ## dashboard.listener.https.max_clients = 512
    ## dashboard.listener.https.handshake_timeout = 15s
    ## dashboard.listener.https.certfile = etc/certs/cert.pem
    ## dashboard.listener.https.keyfile = etc/certs/key.pem
    ## dashboard.listener.https.cacertfile = etc/certs/cacert.pem
    ## dashboard.listener.https.verify = verify_peer
    ## dashboard.listener.https.fail_if_no_peer_cert = true


emqx_auth_clientid - ClientID 认证插件
--------------------------------------

在 *EMQ X* 中所有有 ``_auth_`` 关键字的插件其主要职责有：

1. **连接认证**: 控制某客户端是否具有连接 EMQ X 的权限
2. **访问控制**: 控制某客户端是否具有 PUBLISH/SUBSCIRBE 操作的权限

`emqx_auth_clientid`_ 目前只包含 **连接认证** 功能不包括 **访问控制** 。他会允许满足其配置中 ``clientid`` 成功登录。其中值得注意的是 ``password`` 以明文的方式进行添加记录，在存储入系统时会按照配置的 hash 算法加密后存入。客户端在连接时应该携带对应的密文进行连接。

此外, 该插件还支持 REST API 和 CLI 用于在运行时管理。

.. NOTE:: 3.1 开始支持 REST API 管理 clientid，并移除配置文件中添加默认 clientid 的功能

ClientID 认证配置
:::::::::::::::::

etc/plugins/emqx_auth_clientid.conf:

.. code:: properties

    ## Password hash
    ## Value: plain | md5 | sha | sha256
    auth.client.password_hash = sha256


emqx_auth_username: 用户名密码认证插件
---------------------------------------

`emqx_auth_username`_ 目前只包含 **连接认证** 功能。其逻辑与 ``emqx_auth_clientid`` 相似，只不过其关心的是 ``username``

同样的，username 也支持 CLI 和 REST API 在运行时动态的管理。

.. NOTE:: 3.1 开始支持 REST API 管理 username，并移除配置文件中添加默认 username 的功能

用户名认证配置
::::::::::::::

etc/plugins/emqx_auth_username.conf:

.. code:: properties

    ## Password hash.
    ##
    ## Value: plain | md5 | sha | sha256
    auth.user.password_hash = sha256

emqx_auth_jwt: JWT认证插件
---------------------------

`emqx_auth_jwt`_ 支持基于 `JWT`_ 的方式，对连接的客户端进行认证，仅包括 **连接认证** 功能。它会解析并校验 Token 的合理性和时效、满足则允许连接

JWT 认证配置
::::::::::::

etc/plugins/emqx_auth_jwt.conf

.. code:: properties

    ## HMAC Hash Secret.
    ##
    ## Value: String
    auth.jwt.secret = emqxsecret

    ## From where the JWT string can be got
    ##
    ## Value: username | password
    ## Default: password
    auth.jwt.from = password

    ## RSA or ECDSA public key file.
    ##
    ## Value: File
    ## auth.jwt.pubkey = etc/certs/jwt_public_key.pem


emqx_psk_file: PSK 认证插件
---------------------------

`emqx_psk_file`_ 插件主要提供了 PSK 支持。其目的是用于在客户端建立 TLS/DTLS 连接时，使用 PSK 方式达到 **连接认证** 的功能


配置 PSK 认证插件
:::::::::::::::::

etc/plugins/emqx_psk_file.conf:

.. code:: properties

    psk.file.path = {{ platform_etc_dir }}/psk.txt
    psk.file.delimiter = :


emqx_auth_ldap: LDAP 认证插件
-----------------------------

`emqx_auth_ldap`_ 支持通过访问 `LDAP`_ 服务的方式，来实现控制客户端的接入。目前仅支持 **连接认证**


LDAP 认证插件配置
:::::::::::::::::

etc/plugins/emqx_auth_ldap.conf:

.. code:: properties

    auth.ldap.servers = 127.0.0.1

    auth.ldap.port = 389

    auth.ldap.timeout = 30

    auth.ldap.user_dn = uid=%u,ou=People,dc=example,dc=com

    auth.ldap.ssl = false


emqx_auth_http: HTTP 认证/访问控制插件
--------------------------------------

`emqx_auth_http`_ 插件实现 **连接认证** 与 **访问控制** 的功能。它会将每个请求发送到指定的 HTTP 服务，通过其返回值来判断是否具有具体操作的权限。

该插件总共支持三个请求分别为：

1. **auth.http.auth_req**: 连接认证
2. **auth.http.super_req**: 判断是否为超级用户
3. **auth.http.acl_req**: 访问控制权限查询

特别的是每个请求的参数，都支持使用真实的客户端的 username, IP 地址等进行自定义。

.. NOTE:: 其中在 3.1 版本中新增的 %cn %dn 的支持


HTTP 认证插件配置
:::::::::::::::::

etc/plugins/emqx_auth_http.conf:

.. code:: properties

    ## Variables:
    ##  - %u: username
    ##  - %c: clientid
    ##  - %a: ipaddress
    ##  - %P: password
    ##  - %cn: common name of client TLS cert
    ##  - %dn: subject of client TLS cert
    auth.http.auth_req = http://127.0.0.1:8080/mqtt/auth

    ## Value: post | get | put
    auth.http.auth_req.method = post
    auth.http.auth_req.params = clientid=%c,username=%u,password=%P

    auth.http.super_req = http://127.0.0.1:8080/mqtt/superuser
    auth.http.super_req.method = post
    auth.http.super_req.params = clientid=%c,username=%u

    ## Variables:
    ##  - %A: 1 | 2, 1 = sub, 2 = pub
    ##  - %u: username
    ##  - %c: clientid
    ##  - %a: ipaddress
    ##  - %t: topic
    auth.http.acl_req = http://127.0.0.1:8080/mqtt/acl
    auth.http.acl_req.method = get
    auth.http.acl_req.params = access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t


HTTP API 返回值处理
:::::::::::::::::::

**连接认证**:

.. code:: bash

    ## 认证成功
    HTTP Status Code: 200

    ## 忽略此次认证
    HTTP Status Code: 200
    Body: ignore

    ## 认证失败
    HTTP Status Code: Except 200

*超级用户*:

.. code:: bash

    ## 确认为超级用户
    HTTP Status Code: 200

    ## 非超级用户
    HTTP Status Code: Except 200

**访问控制**:

.. code:: bash

    ## 允许PUBLISH/SUBSCRIBE：
    HTTP Status Code: 200

    ## 忽略此次鉴权:
    HTTP Status Code: 200
    Body: ignore

    ## 拒绝该次PUBLISH/SUBSCRIBE:
    HTTP Status Code: Except 200


emqx_auth_mysql: MySQL 认证/访问控制插件
----------------------------------------

`emqx_auth_mysql`_ 支持访问 MySQL 来完成 **连接认证** **访问控制** 等功能。要完成这些功能，我们需要对 MySQL 创建俩张表其格式如下：

.. note:: 3.1 版本新增 %cn %dn 支持


MQTT 用户表
:::::::::::

.. code:: sql

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

.. NOTE:: 插件同样支持使用已有系统的表，通过 ``authquery`` 配置查询语句即可。


MQTT 访问控制表
:::::::::::::::

.. code:: sql

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


配置 MySQL 认证鉴权插件
::::::::::::::::::::::::

etc/plugins/emqx_auth_mysql.conf:

.. code:: properties

    ## Mysql Server
    auth.mysql.server = 127.0.0.1:3306

    ## Mysql Pool Size
    auth.mysql.pool = 8

    ## Mysql Username
    ## auth.mysql.username =

    ## Mysql Password
    ## auth.mysql.password =

    ## Mysql Database
    auth.mysql.database = mqtt

    ## Variables:
    ##  - %u: username
    ##  - %c: clientid
    ##  - %cn: common name of client TLS cert
    ##  - %dn: subject of client TLS cert
    ## Authentication Query: select password only
    auth.mysql.auth_query = select password from mqtt_user where username = '%u' limit 1

    ## Password hash: plain, md5, sha, sha256, pbkdf2
    auth.mysql.password_hash = sha256

    ## %% Superuser Query
    auth.mysql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1

    ## ACL Query Command
    auth.mysql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'

此外，为防止密码域过于简单而带来安全的隐患问题，该插件还支持密码加盐操作：

.. code:: properties

    ## sha256 with salt prefix
    ## auth.mysql.password_hash = salt,sha256

    ## bcrypt with salt only prefix
    ## auth.mysql.password_hash = salt,bcrypt

    ## sha256 with salt suffix
    ## auth.mysql.password_hash = sha256,salt

    ## pbkdf2 with macfun iterations dklen
    ## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
    ## auth.mysql.password_hash = pbkdf2,sha256,1000,20


emqx_auth_pgsql: Postgre 认证插件
---------------------------------

`emqx_auth_pgsql`_ 支持访问 Postgre 来完成 **连接认证** **访问控制** 等功能。同样需要定义俩张表如下:

.. note:: 3.1 版本新增 %cn %dn 支持


Postgre MQTT 用户表
:::::::::::::::::::

.. code:: sql

    CREATE TABLE mqtt_user (
      id SERIAL primary key,
      is_superuser boolean,
      username character varying(100),
      password character varying(100),
      salt character varying(40)
    );


Postgre MQTT 访问控制表
:::::::::::::::::::::::

.. code:: sql

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


配置 Postgre 认证鉴权插件
:::::::::::::::::::::::::

etc/plugins/emqx_auth_pgsql.conf:

.. code:: properties

    ## PostgreSQL server configurations.
    auth.pgsql.server = 127.0.0.1:5432

    auth.pgsql.pool = 8

    auth.pgsql.username = root

    ## auth.pgsql.password =

    auth.pgsql.database = mqtt

    auth.pgsql.encoding = utf8

    ## Authentication query.
    ##
    ## Value: SQL
    ##
    ## Variables:
    ##  - %u: username
    ##  - %c: clientid
    ##  - %cn: common name of client TLS cert
    ##  - %dn: subject of client TLS cert
    ##
    auth.pgsql.auth_query = select password from mqtt_user where username = '%u' limit 1

    ## Value: plain | md5 | sha | sha256 | bcrypt
    auth.pgsql.password_hash = sha256

    ## Superuser query. The Variables is same with Authentication query
    auth.pgsql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1

    ## ACL query. Comment this query, the ACL will be disabled.
    ##
    ## Variables:
    ##  - %a: ipaddress
    ##  - %u: username
    ##  - %c: clientid
    auth.pgsql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'

同样的 password_hash 可以配置为更为安全的模式:

.. code:: properties

    ## sha256 with salt prefix
    ## auth.pgsql.password_hash = salt,sha256

    ## sha256 with salt suffix
    ## auth.pgsql.password_hash = sha256,salt

    ## bcrypt with salt prefix
    ## auth.pgsql.password_hash = salt,bcrypt

    ## pbkdf2 with macfun iterations dklen
    ## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
    ## auth.pgsql.password_hash = pbkdf2,sha256,1000,20

开启以下配置，则可支持 TLS 连接到 Postgre:

.. code:: properties

    ## Whether to enable SSL connection.
    ##
    ## Value: true | false
    auth.pgsql.ssl = false

    ## SSL keyfile.
    ##
    ## Value: File
    ## auth.pgsql.ssl_opts.keyfile =

    ## SSL certfile.
    ##
    ## Value: File
    ## auth.pgsql.ssl_opts.certfile =

    ## SSL cacertfile.
    ##
    ## Value: File
    ## auth.pgsql.ssl_opts.cacertfile =


emqx_auth_redis: Redis 认证插件
-------------------------------

`emqx_auth_redis`_ 通过访问 Redis 数据以实现 **连接认证** 和 **访问控制** 的功能。

.. note:: 3.1 版本新增 %cn %dn 支持


配置 Redis 认证插件
:::::::::::::::::::

etc/plugins/emqx_auth_redis.conf:

.. code:: properties

    ## Redis server configurations

    ## Redis Server cluster type
    ## Value: single | sentinel | cluster
    auth.redis.type = single

    ## Redis server address.
    ##
    ## Single Redis Server: 127.0.0.1:6379, localhost:6379
    ## Redis Sentinel: 127.0.0.1:26379,127.0.0.2:26379,127.0.0.3:26379
    ## Redis Cluster: 127.0.0.1:6379,127.0.0.2:6379,127.0.0.3:6379
    auth.redis.server = 127.0.0.1:6379

    ## Redis sentinel cluster name.
    ## auth.redis.sentinel = mymaster

    ## Redis pool size.
    auth.redis.pool = 8

    ## Redis database no.
    auth.redis.database = 0

    ## Redis password.
    ## auth.redis.password =

    ## Query command configurations

    ## Authentication query command.
    ## Variables:
    ##  - %u: username
    ##  - %c: clientid
    ##  - %cn: common name of client TLS cert
    ##  - %dn: subject of client TLS cert
    auth.redis.auth_cmd = HMGET mqtt_user:%u password

    ## Password hash.
    ## Value: plain | md5 | sha | sha256 | bcrypt
    auth.redis.password_hash = plain

    ## Superuser query command. The variables is same with Authentication query.
    auth.redis.super_cmd = HGET mqtt_user:%u is_superuser

    ## ACL query command.
    ## Variables:
    ##  - %u: username
    ##  - %c: clientid
    auth.redis.acl_cmd = HGETALL mqtt_acl:%u

同样，该插件支持更安全的密码格式：

.. code:: properties

    ## sha256 with salt prefix
    ## auth.redis.password_hash = salt,sha256

    ## sha256 with salt suffix
    ## auth.redis.password_hash = sha256,salt

    ## bcrypt with salt prefix
    ## auth.redis.password_hash = salt,bcrypt

    ## pbkdf2 with macfun iterations dklen
    ## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
    ## auth.redis.password_hash = pbkdf2,sha256,1000,20


Redis 用户 Hash
::::::::::::::::

默认基于用户 Hash 认证::

    HSET mqtt_user:<username> is_superuser 1
    HSET mqtt_user:<username> password "passwd"
    HSET mqtt_user:<username> salt "salt"


Redis ACL 规则 Hash
::::::::::::::::::::

默认采用 Hash 存储 ACL 规则::

    HSET mqtt_acl:<username> topic1 1
    HSET mqtt_acl:<username> topic2 2
    HSET mqtt_acl:<username> topic3 3

.. NOTE:: 1: subscribe, 2: publish, 3: pubsub


emqx_auth_mongo: MongoDB 认证插件
---------------------------------

`emqx_auth_mongo`_ 基于 MongoDB 实现 **连接认证** 和 **访问控制** 的功能

.. note:: 3.1 版本新增 %cn %dn 支持


配置 MongoDB 认证插件
:::::::::::::::::::::

etc/plugins/emqx_auth_mongo.conf:

.. code:: properties

    ## MongonDB server configurations

    ## MongoDB Topology Type.
    ## Value: single | unknown | sharded | rs
    auth.mongo.type = single

    ## The set name if type is rs.
    ## auth.mongo.rs_set_name =

    ## MongoDB server list.
    auth.mongo.server = 127.0.0.1:27017

    auth.mongo.pool = 8
    ## auth.mongo.login =
    ## auth.mongo.password =
    ## auth.mongo.auth_source = admin
    auth.mongo.database = mqtt

    ## Query commands

    ## Authentication query.
    auth.mongo.auth_query.collection = mqtt_user
    auth.mongo.auth_query.password_field = password
    auth.mongo.auth_query.password_hash = sha256

    ## Authentication Selector.
    ## Variables:
    ##  - %u: username
    ##  - %c: clientid
    ##  - %cn: common name of client TLS cert
    ##  - %dn: subject of client TLS cert
    auth.mongo.auth_query.selector = username=%u

    ## Enable superuser query.
    auth.mongo.super_query = on
    auth.mongo.super_query.collection = mqtt_user
    auth.mongo.super_query.super_field = is_superuser

    ## The authentication variables can be used here
    auth.mongo.super_query.selector = username=%u

    ## Enable ACL query.
    auth.mongo.acl_query = on
    auth.mongo.acl_query.collection = mqtt_acl

    auth.mongo.acl_query.selector = username=%u


MongoDB 数据库
::::::::::::::

.. code:: javascript

    use mqtt
    db.createCollection("mqtt_user")
    db.createCollection("mqtt_acl")
    db.mqtt_user.ensureIndex({"username":1})

.. NOTE:: 数据库、集合名称可自定义

MongoDB 用户集合
::::::::::::::::

.. code:: javascript

    {
        username: "user",
        password: "password hash",
        is_superuser: boolean (true, false),
        created: "datetime"
    }

示例::

    db.mqtt_user.insert({username: "test", password: "password hash", is_superuser: false})
    db.mqtt_user:insert({username: "root", is_superuser: true})

MongoDB ACL 集合
::::::::::::::::

.. code:: javascript

    {
        username: "username",
        clientid: "clientid",
        publish: ["topic1", "topic2", ...],
        subscribe: ["subtop1", "subtop2", ...],
        pubsub: ["topic/#", "topic1", ...]
    }

示例::

    db.mqtt_acl.insert({username: "test", publish: ["t/1", "t/2"], subscribe: ["user/%u", "client/%c"]})
    db.mqtt_acl.insert({username: "admin", pubsub: ["#"]})


emqx_web_hook: WebHook 插件
---------------------------

`emqx_web_hook`_ 插件与上述的插件不同，它可以将所有 *EMQ X* 的事件，及消息都发送到指定的 HTTP 服务器。该插件也并不关心 HTTP 服务器的返回。


配置 WebHook 插件
:::::::::::::::::

etc/plugins/emqx_web_hook.conf

.. code:: properties

    ## The events/message callback URL
    web.hook.api.url = http://127.0.0.1:8080


emqx_lua_hook: Lua 插件
-----------------------

`emqx_lua_hook`_ 插件与 `emqx_web_hook`_ 插件类似，它将所有的事件和消息都发送到指定文件的 Lua 函数上。其具体使用参见其 README


emqx_retainer: Retainer 插件
----------------------------

`emqx_retainer`_ 该插件设置为默认启动，为 *EMQ X* 提供 PUBLISH 的 Retained 类型的消息支持。它会将所有主题的 Retained 消息存储在集群的数据库中，并待有客户端订阅该主题的时候将该消息投递出去。


配置 Retainer 插件
::::::::::::::::::

etc/plugins/emqx_retainer.conf:

.. code:: properties

    ## Where to store the retained messages.
    ##  - ram: memory only
    ##  - disc: both memory and disc
    ##  - disc_only: disc only
    retainer.storage_type = ram

    retainer.max_retained_messages = 0

    ## Maximum retained message size.
    retainer.max_payload_size = 1MB

    ## Expiry interval of the retained messages. Never expire if the value is 0.
    ## Value: Duration
    ##  - h: hour
    ##  - m: minute
    ##  - s: second
    retainer.expiry_interval = 0


emqx_delayed_publish: Delayed Publish 插件
------------------------------------------

`emqx_delayed_publish`_ 提供了 *EMQ X* 支持延迟发送某条消息的功能。客户端使用特殊主题 ``$delayed/<seconds>/t`` 发布消息到 *EMQ X* 。那么 *EMQ X* 将在 ``<seconds>`` 后向主题 ``t`` 发布该消息。


emqx_coap: CoAP 协议插件
------------------------

`emqx_coap`_ 提供 CoAP 协议的支持，支持 RFC 7252 规范。

配置 CoAP 协议插件
::::::::::::::::::

etc/plugins/emqx_coap.conf:

.. code:: properties

    coap.port = 5683

    coap.keepalive = 120s

    coap.enable_stats = off

若开启以下俩个配置，则可以支持 DTLS:

.. code:: properties

    coap.keyfile = {{ platform_etc_dir }}/certs/key.pem

    coap.certfile = {{ platform_etc_dir }}/certs/cert.pem


测试 CoAP 插件
::::::::::::::

我们可以通过安装 `libcoap`_ 来测试 *EMQ X* 对CoAP 协议的支持情况

.. code:: bash

    yum install libcoap

    % coap client publish message
    coap-client -m post -e "qos=0&retain=0&message=payload&topic=hello" coap://localhost/mqtt


emqx_lwm2m: LwM2M 协议插件
--------------------------

`emqx_lwm2m`_ 提供了对 LwM2M 协议的支持。


配置 LwM2M 插件
:::::::::::::::

etc/plugins/emqx_lwm2m.conf:

.. code:: properties

    lwm2m.port = 5683

    lwm2m.lifetime_min = 1s
    lwm2m.lifetime_max = 86400s

    # The time window for Q Mode, indicating that after how long time
    #   the downlink commands sent to the client will be cached.
    #lwm2m.qmode_time_window = 22

    # Is this LwM2M Gateway behind a coaproxy?
    #lwm2m.lb = coaproxy

    #lwm2m.auto_observe = off

    # The topic subscribed by the lwm2m client after it is connected
    # Placeholders supported:
    #    '%e': Endpoint Name
    #    '%a': IP Address
    lwm2m.topics.command = lwm2m/%e/dn/#

    # The topic to which the lwm2m client's response is published
    lwm2m.topics.response = lwm2m/%e/up/resp

    # The topic to which the lwm2m client's notify message is published
    lwm2m.topics.notify = lwm2m/%e/up/notify

    # The topic to which the lwm2m client's register message is published
    lwm2m.topics.register = lwm2m/%e/up/resp

    # The topic to which the lwm2m client's update message is published
    lwm2m.topics.update = lwm2m/%e/up/resp

    # Dir where the object definition files can be found
    lwm2m.xml_dir =  {{ platform_etc_dir }}/lwm2m_xml

同样可以通过以下配置打开 DTLS 支持：

.. code:: properties

    # Cert and Key file for DTLS
    lwm2m.certfile = {{ platform_etc_dir }}/certs/cert.pem
    lwm2m.keyfile = {{ platform_etc_dir }}/certs/key.pem


emqx_sn:  MQTT-SN 协议插件
--------------------------

`emqx_sn`_ 插件提供了 `MQTT-SN`_ 协议的支持。


配置 MQTT-SN 协议插件
:::::::::::::::::::::

etc/plugins/emqx_sn.conf:

.. code:: properties

    mqtt.sn.port = 1884


emqx_stomp: Stomp 协议插件
--------------------------

`emqx_stomp`_ 提供了 Stomp 协议的支持。支持 STOMP 1.0/1.1/1.2 协议客户端连接 EMQ，发布订阅 MQTT 消息。


配置 Stomp 插件
:::::::::::::::

.. NOTE:: Stomp 协议端口: 61613

etc/plugins/emqx_stomp.conf:

.. code:: properties

    stomp.default_user.login = guest

    stomp.default_user.passcode = guest

    stomp.allow_anonymous = true

    stomp.frame.max_headers = 10

    stomp.frame.max_header_length = 1024

    stomp.frame.max_body_length = 8192

    stomp.listener = 61613

    stomp.listener.acceptors = 4

    stomp.listener.max_clients = 512


emqx_recon: Recon 性能调试插件
------------------------------

`emqx_recon`_ 插件集成了 recon 性能调测库，可用于查看当前系统的一些状态信息，例如：

.. code:: bash

    ./bin/emqx_ctl recon

    recon memory                 #recon_alloc:memory/2
    recon allocated              #recon_alloc:memory(allocated_types, current|max)
    recon bin_leak               #recon:bin_leak(100)
    recon node_stats             #recon:node_stats(10, 1000)
    recon remote_load Mod        #recon:remote_load(Mod)


配置 Recon 插件
:::::::::::::::

etc/plugins/emqx_recon.conf:

.. code:: properties

    %% Garbage Collection: 10 minutes
    recon.gc_interval = 600


emqx_reloader: 代码热加载插件
-----------------------------

`emqx_reloader`_ 用于开发调试的代码热升级插件。加载该插件后 *EMQ X* 会根据配置的时间间隔自动热升级更新代码。

同时，也提供了 CLI 命令来指定 reload 某一个模块:

.. code:: bash

    ./bin/emqx_ctl reload <Module>

.. NOTE:: 产品部署环境不建议使用该插件


配置 Reloader 插件
::::::::::::::::::

etc/plugins/emqx_reloader.conf:

.. code:: properties

    reloader.interval = 60

    reloader.logfile = log/reloader.log


emqx_plugin_template: 插件开发模版
----------------------------------

`emqx_plugin_template`_ 是一个 *EMQ X* 插件模板，在功能上并无任何意义。

在想要定制一个新的插件时，可以查看该插件的代码和结构，以更快的开发一个标准的 *EMQ X* 插件。插件实际是一个普通的 ``Erlang Application``，其配置文件应置于: ``etc/${PluginName}.config`` 下



EMQ X R3.0 插件开发
-------------------

创建插件项目
::::::::::::

参考 `emqx_plugin_template`_ 插件模版创建新的插件项目。

.. NOTE:: 在 ``<plugin name>_app.erl`` 文件中必须加上标签 ``-emqx_plugin(?MODULE).`` 以表明这是一个 EMQ X 的插件


创建认证/访问控制模块
::::::::::::::::::::::

认证演示模块 - emqx_auth_demo.erl

.. code:: erlang

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

访问控制演示模块 - emqx_acl_demo.erl

.. code:: erlang

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

注册认证、访问控制模块 - emqx_plugin_template_app.erl

.. code:: erlang

    ok = emqx:hook('client.authenticate', fun emqx_auth_demo:check/2, []),
    ok = emqx:hook('client.check_acl', fun emqx_acl_demo:check_acl/5, []).


注册钩子(Hooks)
::::::::::::::::

通过钩子(Hook)处理客户端上下线、主题订阅、消息收发。

emqx_plugin_template.erl:

.. code:: erlang

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


所有可用钩子(Hook)说明:

+------------------------+----------------------------------+
| 钩子                   | 说明                             |
+========================+==================================+
| client.authenticate    | 连接认证                         |
+------------------------+----------------------------------+
| client.check_acl       | ACL 校验                         |
+------------------------+----------------------------------+
| client.connected       | 客户端上线                       |
+------------------------+----------------------------------+
| client.disconnected    | 客户端连接断开                   |
+------------------------+----------------------------------+
| client.subscribe       | 客户端订阅主题                   |
+------------------------+----------------------------------+
| client.unsubscribe     | 客户端取消订阅主题               |
+------------------------+----------------------------------+
| session.created        | 会话创建                         |
+------------------------+----------------------------------+
| session.resumed        | 会话恢复                         |
+------------------------+----------------------------------+
| session.subscribed     | 会话订阅主题后                   |
+------------------------+----------------------------------+
| session.unsubscribed   | 会话取消订阅主题后               |
+------------------------+----------------------------------+
| session.terminated     | 会话终止                         |
+------------------------+----------------------------------+
| message.publish        | MQTT 消息发布                    |
+------------------------+----------------------------------+
| message.deliver        | MQTT 消息进行投递                |
+------------------------+----------------------------------+
| message.acked          | MQTT 消息回执                    |
+------------------------+----------------------------------+
| message.dropped        | MQTT 消息丢弃                    |
+------------------------+----------------------------------+


注册CLI命令
:::::::::::

扩展命令行演示模块 - emqx_cli_demo.erl

.. code:: erlang

    -module(emqx_cli_demo).

    -export([cmd/1]).

    cmd(["arg1", "arg2"]) ->
        emqx_cli:print("ok");

    cmd(_) ->
        emqx_cli:usage([{"cmd arg1 arg2", "cmd demo"}]).

注册命令行模块 - emqx_plugin_template_app.erl

.. code:: erlang

    ok = emqx_ctl:register_command(cmd, {emqx_cli_demo, cmd}, []),

插件加载后，'./bin/emqx_ctl'新增命令行::

    ./bin/emqx_ctl cmd arg1 arg2


插件配置文件
::::::::::::

插件自带配置文件放置在 ``etc/${plugin_name}.conf|config`` *EMQ X* 支持两种插件配置格式:

1. ``${plugin_name}.config`` Erlang 原生配置文件格式:

.. code:: erlang

    [
      {plugin_name, [
        {key, value}
      ]}
    ].

2. ``${plugin_name}.conf`` sysctl 的 ``k = v`` 通用格式:

.. code:: properties

    plugin_name.key = value

.. NOTE:: ``k = v`` 格式配置需要插件开发者创建 ``priv/plugin_name.schema`` 映射文件。


编译发布插件
::::::::::::

1. clone emqx-rel 项目:

.. code:: bash

    git clone https://github.com/emqx/emqx-rel.git

2. Makefile 增加 `DEPS`:

.. code:: makefile

    DEPS += plugin_name
    dep_plugin_name = git url_of_plugin

3. relx.config 中 release 段落添加:

.. code:: erlang

    {plugin_name, load},

.. _emqx_dashboard:        https://github.com/emqx/emqx-dashboard
.. _emqx_retainer:         https://github.com/emqx/emqx-retainer
.. _emqx_delayed_publish:  https://github.com/emqx/emqx-delayed-publish
.. _emqx_auth_clientid:    https://github.com/emqx/emqx-auth-clientid
.. _emqx_auth_username:    https://github.com/emqx/emqx-auth-username
.. _emqx_auth_ldap:        https://github.com/emqx/emqx-auth-ldap
.. _emqx_auth_http:        https://github.com/emqx/emqx-auth-http
.. _emqx_auth_mysql:       https://github.com/emqx/emqx-auth-mysql
.. _emqx_auth_pgsql:       https://github.com/emqx/emqx-auth-pgsql
.. _emqx_auth_redis:       https://github.com/emqx/emqx-auth-redis
.. _emqx_auth_mongo:       https://github.com/emqx/emqx-auth-mongo
.. _emqx_auth_jwt:         https://github.com/emqx/emqx-auth-jwt
.. _emqx_web_hook:         https://github.com/emqx/emqx-web-hook
.. _emqx_lua_hook:         https://github.com/emqx/emqx-lua-hook
.. _emqx_sn:               https://github.com/emqx/emqx-sn
.. _emqx_coap:             https://github.com/emqx/emqx-coap
.. _emqx_lwm2m:            https://github.com/emqx/emqx-lwm2m
.. _emqx_stomp:            https://github.com/emqx/emqx-stomp
.. _emqx_recon:            https://github.com/emqx/emqx-recon
.. _emqx_reloader:         https://github.com/emqx/emqx-reloader
.. _emqx_psk_file:         https://github.com/emqx/emqx-psk-file
.. _emqx_plugin_template:  https://github.com/emqx/emqx-plugin-template
.. _recon:                 http://ferd.github.io/recon/
.. _LDAP:                  https://ldap.com
.. _JWT:                   https://jwt.io
.. _libcoap:               https://github.com/obgm/libcoap
.. _MQTT-SN:               https://github.com/emqx/emqx-sn

