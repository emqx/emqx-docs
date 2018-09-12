
.. _plugins:

==================
扩展插件 (Plugins)
==================

*EMQX* 消息服务器通过模块注册和钩子(Hooks)机制，支持用户开发扩展插件定制服务器认证鉴权与业务功能。

*EMQX* 3.0 版本官方提供的插件包括:

+---------------------------+---------------------------+
| 插件                      | 说明                      |
+===========================+===========================+
| `emqx_dashboard`_         | Web 控制台插件(默认加载)  |
+---------------------------+---------------------------+
| `emqx_auth_clientid`_     | ClientId 认证插件         |
+---------------------------+---------------------------+
| `emqx_auth_username`_     | 用户名、密码认证插件      |
+---------------------------+---------------------------+
| `emqx_auth_ldap`_         | LDAP 认证/访问控制        |
+---------------------------+---------------------------+
| `emqx_auth_http`_         | HTTP 认证/访问控制        |
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
| `emqx_auth_mongo`_        | MongoDB 认证/访问控制     |
+---------------------------+---------------------------+
| `emqx_retainer`_          | Retain 消息存储模块       |
+---------------------------+---------------------------+
| `emqx_coap`_              | CoAP 协议支持             |
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

------------------------------
emqx_retainer Retainer 模块插件
------------------------------

2.1-beta 版本将 emq_mod_retainer 模块更名为 emq_retainer 模块，Retainer 模块负责持久化 MQTT Retained 消息。

配置 Retainer 模块
------------------

etc/plugins/emqx_retainer.conf:

.. code-block:: properties

    ## disc: disc_copies, ram: ram_copies
    ## Notice: retainer's storage_type on each node in a cluster must be the same!
    retainer.storage_type = disc

    ## Max number of retained messages
    retainer.max_message_num = 1000000

    ## Max Payload Size of retained message
    retainer.max_payload_size = 64KB

    ## Expiry interval. Never expired if 0
    ## h - hour
    ## m - minute
    ## s - second
    retainer.expiry_interval = 0

加载 Retainer 模块
------------------

Retainer 模块默认加载。

-------------------------------------
emqx_auth_clientid - ClientID 认证插件
-------------------------------------

EMQ 2.0-rc.2 版本将 ClientId 认证模块改为独立插件: https://github.com/emqx/emqx-auth-clientid

ClientID 认证配置
-----------------

etc/plugins/emqx_auth_clientid.conf:

.. code-block:: properties

    ##auth.client.$N.clientid = clientid
    ##auth.client.$N.password = passwd

    ## Examples
    ##auth.client.1.clientid = id
    ##auth.client.1.password = passwd
    ##auth.client.2.clientid = dev:devid
    ##auth.client.2.password = passwd2
    ##auth.client.3.clientid = app:appid
    ##auth.client.3.password = passwd3

加载 ClientId 认证插件
----------------------

.. code-block:: bash

    ./bin/emqx_ctl plugins load emqx_auth_clientid

-------------------------------------
emqx_auth_username - 用户名密码认证插件
-------------------------------------

EMQ 2.0-rc.2 版本将用户名认证模块改为独立插件: https://github.com/emqx/emqx-auth-username

用户名认证配置
--------------

etc/plugins/emqx_auth_username.conf:

.. code-block:: properties

    ##auth.user.$N.username = admin
    ##auth.user.$N.password = public

    ## Examples:
    ##auth.user.1.username = admin
    ##auth.user.1.password = public
    ##auth.user.2.username = feng@emqtt.io
    ##auth.user.2.password = public

两种方式添加用户:

1. 直接在 etc/plugins/emqx_auth_username.conf 中明文配置默认用户例如::

    auth.username.test = public

2. 通过 './bin/emqx_ctl' 管理命令行添加用户::

   $ ./bin/emqx_ctl users add <Username> <Password>

加载用户名认证插件
------------------

.. code-block:: bash

    ./bin/emqx_ctl plugins load emqx_auth_username

---------------------------------
emqx_plugin_template: 插件开发模版
---------------------------------

EMQ 插件实际是一个普通的 Erlang 应用，插件配置文件: 'etc/${PluginName}.conf|config"。

emqx_plugin_template 是模版插件，编译发布在 lib/emq_plugin_template-2.0 目录，配置文件: etc/plugins/emqx_plugin_templat.config

加载、卸载插件
--------------

管理命令行 './bin/emqx_ctl' 加载卸载插件。

加载插件::

    ./bin/emqx_ctl plugins load <PluginName>

卸载插件::

    ./bin/emqx_ctl plugins unload <PluginName>

查询插件::

    ./bin/emqx_ctl plugins list

-----------------------------
emqx_dashboard: Dashboard 插件
-----------------------------

*EMQ* 消息服务器的 Web 管理控制台。插件项目地址: https://github.com/emqx/emqx-dashboard

*EMQ* 消息服务器默认加载 Dashboard 插件。URL 地址: http://localhost:18083 ，缺省用户名/密码: admin/public。

Dashboard 插件可查询 EMQ 消息服务器基本信息、统计数据、度量数据，查询系统客户端(Client)、会话(Session)、主题(Topic)、订阅(Subscription)。

.. image:: ./_static/images/dashboard.png

Dashboard 插件设置
------------------

etc/plugins/emqx_dashboard.conf:

.. code-block:: properties

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

----------------------------
emqx_auth_ldap: LDAP 认证插件
----------------------------

LDAP 认证插件: https://github.com/emqx/emqx-auth-ldap

.. NOTE:: 2.0-beta1 版本支持

LDAP 认证插件配置
-----------------

etc/plugins/emqx_auth_ldap.conf:

.. code-block:: properties

    auth.ldap.servers = 127.0.0.1

    auth.ldap.port = 389

    auth.ldap.timeout = 30

    auth.ldap.user_dn = uid=%u,ou=People,dc=example,dc=com

    auth.ldap.ssl = false

LDAP 认证插件加载
-----------------

./bin/emqx_ctl plugins load emqx_auth_ldap

-------------------------------------
emqx_auth_http: HTTP 认证/访问控制插件
-------------------------------------

HTTP 认证/访问控制插件: https://github.com/emqx/emqx-auth-http

.. NOTE:: 1.1版本支持

HTTP 认证插件配置
-----------------

etc/plugins/emqx_auth_http.conf:

.. code-block:: properties

    ## Variables: %u = username, %c = clientid, %a = ipaddress, %P = password, %t = topic

    auth.http.auth_req = http://127.0.0.1:8080/mqtt/auth
    auth.http.auth_req.method = post
    auth.http.auth_req.params = clientid=%c,username=%u,password=%P

    auth.http.super_req = http://127.0.0.1:8080/mqtt/superuser
    auth.http.super_req.method = post
    auth.http.super_req.params = clientid=%c,username=%u

    ## 'access' parameter: sub = 1, pub = 2
    auth.http.acl_req = http://127.0.0.1:8080/mqtt/acl
    auth.http.acl_req.method = get
    auth.http.acl_req.params = access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t

HTTP 认证/鉴权 API
------------------

认证/ACL 成功，API 返回200

认证/ACL 失败，API 返回4xx

加载 HTTP 认证插件
------------------

./bin/emqx_ctl plugins load emqx_auth_http

---------------------------------------
emqx_auth_mysql: MySQL 认证/访问控制插件
---------------------------------------

MySQL 认证/访问控制插件，基于 MySQL 库表认证鉴权: https://github.com/emqx/emqx-auth-mysql

MQTT 用户表
-----------

.. code-block:: sql

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

.. NOTE:: MySQL 插件可使用系统自有的用户表，通过 'authquery' 配置查询语句。

MQTT 访问控制表
---------------

.. code-block:: sql

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
-----------------------

etc/plugins/emqx_auth_mysql.conf:

.. code-block:: properties

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

    ## Variables: %u = username, %c = clientid

    ## Authentication Query: select password only
    auth.mysql.auth_query = select password from mqtt_user where username = '%u' limit 1

    ## Password hash: plain, md5, sha, sha256, pbkdf2
    auth.mysql.password_hash = sha256

    ## %% Superuser Query
    auth.mysql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1

    ## ACL Query Command
    auth.mysql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'

加载 MySQL 认证鉴权插件
-----------------------

./bin/emqx_ctl plugins load emqx_auth_mysql

-----------------------------------------
emqx_auth_pgsql: Postgre 认证/访问控制插件
-----------------------------------------

Postgre 认证/访问控制插件，基于 PostgreSQL 库表认证鉴权: https://github.com/emqx/emqx-auth-pgsql

Postgre MQTT 用户表
-------------------

.. code-block:: sql

    CREATE TABLE mqtt_user (
      id SERIAL primary key,
      is_superuser boolean,
      username character varying(100),
      password character varying(100),
      salt character varying(40)
    );

Postgre MQTT 访问控制表
-----------------------

.. code-block:: sql

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
-------------------------

etc/plugins/emqx_auth_pgsql.conf:

.. code-block:: properties

    ## Postgre Server
    auth.pgsql.server = 127.0.0.1:5432

    auth.pgsql.pool = 8

    auth.pgsql.username = root

    #auth.pgsql.password =

    auth.pgsql.database = mqtt

    auth.pgsql.encoding = utf8

    auth.pgsql.ssl = false

    ## Variables: %u = username, %c = clientid, %a = ipaddress

    ## Authentication Query: select password only
    auth.pgsql.auth_query = select password from mqtt_user where username = '%u' limit 1

    ## Password hash: plain, md5, sha, sha256, pbkdf2
    auth.pgsql.password_hash = sha256

    ## sha256 with salt prefix
    ## auth.pgsql.password_hash = salt sha256

    ## sha256 with salt suffix
    ## auth.pgsql.password_hash = sha256 salt

    ## Superuser Query
    auth.pgsql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1

    ## ACL Query. Comment this query, the acl will be disabled.
    auth.pgsql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'

加载 Postgre 认证鉴权插件
-------------------------

.. code-block:: bash

    ./bin/emqx_ctl plugins load emqx_auth_pgsql

---------------------------------------
emqx_auth_redis: Redis 认证/访问控制插件
---------------------------------------

基于 Redis 认证/访问控制: https://github.com/emqx/emqx-auth-redis

配置 Redis 认证鉴权插件
-----------------------

etc/plugins/emqx_auth_redis.conf:

.. code-block:: properties

    ## Redis Server
    auth.redis.server = 127.0.0.1:6379

    ## Redis Pool Size
    auth.redis.pool = 8

    ## Redis Database
    auth.redis.database = 0

    ## Redis Password
    ## auth.redis.password =

    ## Variables: %u = username, %c = clientid

    ## Authentication Query Command
    auth.redis.auth_cmd = HGET mqtt_user:%u password

    ## Password hash: plain, md5, sha, sha256, pbkdf2
    auth.redis.password_hash = sha256

    ## Superuser Query Command
    auth.redis.super_cmd = HGET mqtt_user:%u is_superuser

    ## ACL Query Command
    auth.redis.acl_cmd = HGETALL mqtt_acl:%u

Redis 用户 Hash
---------------

默认基于用户 Hash 认证::

    HSET mqtt_user:<username> is_superuser 1
    HSET mqtt_user:<username> password "passwd"

Redis ACL 规则 Hash
-------------------

默认采用 Hash 存储 ACL 规则::

    HSET mqtt_acl:<username> topic1 1
    HSET mqtt_acl:<username> topic2 2
    HSET mqtt_acl:<username> topic3 3

.. NOTE:: 1: subscribe, 2: publish, 3: pubsub

Redis 订阅 Hash
----------------

插件还支持 Redis 中创建 MQTT 订阅。当 MQTT 客户端连接成功，会自动从 Redis 加载订阅::

    HSET mqtt_sub:<username> topic1 0
    HSET mqtt_sub:<username> topic2 1
    HSET mqtt_sub:<username> topic3 2

.. WARNING:: 2.0-rc.2 版本已将订阅加载迁移至 EMQX 产品的emqx_backend_redis插件。

加载 Redis 认证鉴权插件
-----------------------

.. code-block:: bash

    ./bin/emqx_ctl plugins load emqx_auth_redis

-----------------------------------------
emqx_auth_mongo: MongoDB 认证/访问控制插件
-----------------------------------------

基于 MongoDB 认证/访问控制: https://github.com/emqx/emqx-auth-mongo

配置 MongoDB 认证鉴权插件
-------------------------

etc/plugins/emqx_auth_mongo.conf:

.. code-block:: properties

    ## Mongo Server
    auth.mongo.server = 127.0.0.1:27017

    ## Mongo Pool Size
    auth.mongo.pool = 8

    ## Mongo User
    ## auth.mongo.user =

    ## Mongo Password
    ## auth.mongo.password =

    ## Mongo Database
    auth.mongo.database = mqtt

    ## auth_query
    auth.mongo.auth_query.collection = mqtt_user

    auth.mongo.auth_query.password_field = password

    auth.mongo.auth_query.password_hash = sha256

    auth.mongo.auth_query.selector = username=%u

    ## super_query
    auth.mongo.super_query.collection = mqtt_user

    auth.mongo.super_query.super_field = is_superuser

    auth.mongo.super_query.selector = username=%u

    ## acl_query
    auth.mongo.acl_query.collection = mqtt_user

    auth.mongo.acl_query.selector = username=%u

MongoDB 数据库
--------------

.. code-block:: mongodb

    use mqtt
    db.createCollection("mqtt_user")
    db.createCollection("mqtt_acl")
    db.mqtt_user.ensureIndex({"username":1})

.. NOTE:: 数据库、集合名称可自定义

MongoDB 用户集合(User Collection)
---------------------------------

.. code-block:: javascript

    {
        username: "user",
        password: "password hash",
        is_superuser: boolean (true, false),
        created: "datetime"
    }

示例::

    db.mqtt_user.insert({username: "test", password: "password hash", is_superuser: false})
    db.mqtt_user:insert({username: "root", is_superuser: true})

MongoDB ACL 集合(ACL Collection)
--------------------------------

.. code-block:: javascript

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

加载 Mognodb 认证插件
---------------------

.. code-block:: bash

    ./bin/emqx_ctl plugins load emqx_auth_mongo

-----------------------
emqx_coap: CoAP 协议插件
-----------------------

CoAP 协议插件，支持 RFC 7252 规范。

配置 CoAP 协议插件
------------------

.. code-block:: properties

    coap.server = 5683

    coap.prefix.mqtt = mqtt

    coap.handler.mqtt = emq_coap_gateway

加载 CoAP 协议插件
------------------

.. code:: bash

    ./bin/emqx_ctl plugins load emqx_coap

libcoap 客户端
--------------

.. code:: bash

    yum install libcoap

    % coap client publish message
    coap-client -m post -e "qos=0&retain=0&message=payload&topic=hello" coap://localhost/mqtt

-------------------------
emqx_sn: MQTT-SN 协议插件
-------------------------

MQTT-SN 协议插件，支持 MQTT-SN 网关模式。

配置 MQTT-SN 协议插件
---------------------

.. NOTE:: 默认 MQTT-SN 协议 UDP 端口: 1884

etc/plugins/emqx_sn.conf:

.. code-block:: properties

    mqtt.sn.port = 1884

加载 MQTT-SN 协议插件
---------------------

.. code::

    ./bin/emqx_ctl plugins load emqx_sn

--------------------------
emqx_stomp: Stomp 协议插件
--------------------------

Stomp 协议插件。支持 STOMP 1.0/1.1/1.2 协议客户端连接 EMQ，发布订阅 MQTT 消息。

配置插件
--------

.. NOTE:: Stomp 协议端口: 61613

etc/plugins/emqx_stomp.conf:

.. code-block:: properties

    stomp.default_user.login = guest

    stomp.default_user.passcode = guest

    stomp.allow_anonymous = true

    stomp.frame.max_headers = 10

    stomp.frame.max_header_length = 1024

    stomp.frame.max_body_length = 8192

    stomp.listener = 61613

    stomp.listener.acceptors = 4

    stomp.listener.max_clients = 512

加载 Stomp 插件
---------------

.. code:: bash

    ./bin/emqx_ctl plugins load emqx_stomp


-----------------------------
emqx_recon: Recon 性能调试插件
-----------------------------

emqx_recon 插件集成 recon 性能调测库，'./bin/emqx_ctl' 命令行注册 recon 命令。

配置 Recon 插件
---------------

etc/plugins/emqx_recon.conf:

.. code-block:: properties

    %% Garbage Collection: 10 minutes
    recon.gc_interval = 600

加载 Recon 插件
---------------

.. code-block:: bash

    ./bin/emqx_ctl plugins load emqx_recon

recon 插件命令
---------------

.. code-block:: bash

    ./bin/emqx_ctl recon

    recon memory                 #recon_alloc:memory/2
    recon allocated              #recon_alloc:memory(allocated_types, current|max)
    recon bin_leak               #recon:bin_leak(100)
    recon node_stats             #recon:node_stats(10, 1000)
    recon remote_load Mod        #recon:remote_load(Mod)

----------------------------
emqx_reloader: 代码热加载插件
----------------------------

用于开发调试的代码热升级插件。加载该插件后，EMQ 会自动热升级更新代码。

.. NOTE:: 产品部署环境不建议使用该插件

配置 Reloader 插件
------------------

etc/plugins/emqx_reloader.conf:

.. code-block:: properties

    reloader.interval = 60

    reloader.logfile = log/reloader.log

加载 Reloader 插件
------------------

.. code-block:: bash

    ./bin/emqx_ctl plugins load emqx_reloader

Reloader 插件命令
-----------------

.. code-block:: bash

    ./bin/emqx_ctl reload

    reload <Module>             # Reload a Module

----------------
EMQ 3.0 插件开发
----------------

创建插件项目
------------

参考 `emqx_plugin_template`_ 插件模版创建新的插件项目。

注册认证/访问控制模块
---------------------

认证演示模块 - emq_auth_demo.erl

.. code-block:: erlang

    -module(emq_auth_demo).

    -behaviour(emqttd_auth_mod).

    -include_lib("emqttd/include/emqttd.hrl").

    -export([init/1, check/3, description/0]).

    init(Opts) -> {ok, Opts}.

    check(#mqtt_client{client_id = ClientId, username = Username}, Password, _Opts) ->
        io:format("Auth Demo: clientId=~p, username=~p, password=~p~n",
                  [ClientId, Username, Password]),
        ok.

    description() -> "Demo Auth Module".

访问控制演示模块 - emqttd_acl_demo.erl

.. code-block:: erlang

    -module(emq_acl_demo).

    -include_lib("emqttd/include/emqttd.hrl").

    %% ACL callbacks
    -export([init/1, check_acl/2, reload_acl/1, description/0]).

    init(Opts) ->
        {ok, Opts}.

    check_acl({Client, PubSub, Topic}, Opts) ->
        io:format("ACL Demo: ~p ~p ~p~n", [Client, PubSub, Topic]),
        allow.

    reload_acl(_Opts) ->
        ok.

    description() -> "ACL Module Demo".

注册认证、访问控制模块 - emq_plugin_template_app.erl

.. code-block:: erlang

    ok = emqttd_access_control:register_mod(auth, emq_auth_demo, []),
    ok = emqttd_access_control:register_mod(acl, emq_acl_demo, []),

注册扩展钩子(Hooks)
--------------------

通过钩子(Hook)处理客户端上下线、主题订阅、消息收发。

emq_plugin_template.erl::

    %% Called when the plugin application start
    load(Env) ->
        emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
        emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
        emqttd:hook('client.subscribe', fun ?MODULE:on_client_subscribe/4, [Env]),
        emqttd:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4, [Env]),
        emqttd:hook('session.subscribed', fun ?MODULE:on_session_subscribed/4, [Env]),
        emqttd:hook('session.unsubscribed', fun ?MODULE:on_session_unsubscribe/4, [Env]),
        emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
        emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/4, [Env]),
        emqttd:hook('message.acked', fun ?MODULE:on_message_acked/4, [Env]).

扩展钩子(Hook):

+------------------------+----------------------------------+
| 钩子                   | 说明                             |
+========================+==================================+
| client.connected       | 客户端上线                       |
+------------------------+----------------------------------+
| client.subscribe       | 客户端订阅主题前                 |
+------------------------+----------------------------------+
| session.subscribed     | 客户端订阅主题后                 |
+------------------------+----------------------------------+
| client.unsubscribe     | 客户端取消订阅主题               |
+------------------------+----------------------------------+
| session.unsubscribed   | 客户端取消订阅主题后             |
+------------------------+----------------------------------+
| message.publish        | MQTT 消息发布                    |
+------------------------+----------------------------------+
| message.delivered      | MQTT 消息送达                    |
+------------------------+----------------------------------+
| message.acked          | MQTT 消息回执                    |
+------------------------+----------------------------------+
| client.disconnected    | 客户端连接断开                   |
+------------------------+----------------------------------+

注册扩展命令行
--------------

扩展命令行演示模块 - emq_cli_demo.erl

.. code-block:: erlang

    -module(emq_cli_demo).

    -include_lib("emqttd/include/emqttd_cli.hrl").

    -export([cmd/1]).

    cmd(["arg1", "arg2"]) ->
        ?PRINT_MSG("ok");

    cmd(_) ->
        ?USAGE([{"cmd arg1 arg2",  "cmd demo"}]).

注册命令行模块 - emq_plugin_template_app.erl

.. code-block:: erlang

    emqx_ctl:register_cmd(cmd, {emq_cli_demo, cmd}, []).

插件加载后，'./bin/emqx_ctl'新增命令行::

    ./bin/emqx_ctl cmd arg1 arg2

插件配置文件
------------

插件自带配置文件放置在 etc/${plugin_name}.conf|config，EMQ 支持两种插件配置格式:

1. ${plugin_name}.config，Erlang 原生配置文件格式:

.. code-block:: erlang

    [
      {plugin_name, [
        {key, value}
      ]}
    ].

2. ${plugin_name}.conf, sysctl 的 `k = v` 通用格式:

.. code-block:: properties

    plugin_name.key = value

.. NOTE:: `k = v` 格式配置需要插件开发者创建 priv/plugin_name.schema 映射文件。

编译发布插件
------------

1. clone emqx-relx 项目:

.. code-block:: bash

    git clone https://github.com/emqx/emqx-rel.git

2. Makefile 增加 `DEPS`:

.. code-block:: makefile

    DEPS += plugin_name
    dep_plugin_name = git url_of_plugin

3. relx.config 中 release 段落添加:

.. code-block:: erlang

    {plugin_name, load},

.. _emqx_dashboard:        https://github.com/emqx/emqx-dashboard
.. _emqx_retainer:         https://github.com/emqx/emqx-retainer
.. _emqx_auth_clientid:    https://github.com/emqx/emqx-auth-clientid
.. _emqx_auth_username:    https://github.com/emqx/emqx-auth-username
.. _emqx_auth_ldap:        https://github.com/emqx/emqx-auth-ldap
.. _emqx_auth_http:        https://github.com/emqx/emqx-auth-http
.. _emqx_auth_mysql:       https://github.com/emqx/emqx-auth-mysql
.. _emqx_auth_pgsql:       https://github.com/emqx/emqx-auth-pgsql
.. _emqx_auth_redis:       https://github.com/emqx/emqx-auth-redis
.. _emqx_auth_mongo:       https://github.com/emqx/emqx-auth-mongo
.. _emqx_web_hook:         https://github.com/emqx/emqx-web-hook
.. _emqx_lua_hook:         https://github.com/emqx/emqx-lua-hook
.. _emqx_sn:               https://github.com/emqx/emqx-sn
.. _emqx_coap:             https://github.com/emqx/emqx-coap
.. _emqx_stomp:            https://github.com/emqx/emqx-stomp
.. _emqx_recon:            https://github.com/emqx/emqx-recon
.. _emqx_reloader:         https://github.com/emqx/emqx-reloader
.. _emqx_plugin_template:  https://github.com/emqx/emqx-plugin-template
.. _recon:                http://ferd.github.io/recon/
