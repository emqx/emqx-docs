
.. _plugins:

=================
扩展插件(Plugins)
=================

*EMQ* 消息服务器通过模块注册和钩子(Hooks)机制，支持用户开发扩展插件定制服务器认证鉴权与业务功能。

*EMQ* 2.0版本官方提供的插件包括:

+---------------------------+---------------------------+
| 插件                      | 说明                      |
+===========================+===========================+
| `emq_dashboard`_          | Web控制台插件(默认加载)   |
+---------------------------+---------------------------+
| `emq_auth_clientid`_      | ClientId认证插件          |
+---------------------------+---------------------------+
| `emq_auth_username`_      | 用户名、密码认证插件      |
+---------------------------+---------------------------+
| `emq_auth_ldap`_          | LDAP认证/访问控制         |
+---------------------------+---------------------------+
| `emq_auth_http`_          | HTTP认证/访问控制         |
+---------------------------+---------------------------+
| `emq_auth_mysql`_         | MySQL认证/访问控制        |
+---------------------------+---------------------------+
| `emq_auth_pgsql`_         | PostgreSQL认证/访问控制   |
+---------------------------+---------------------------+
| `emq_auth_redis`_         | Redis认证/访问控制        |
+---------------------------+---------------------------+
| `emq_auth_mongo`_         | MongoDB认证/访问控制      |
+---------------------------+---------------------------+
| `emq_mod_rewrite`_        | 重写主题(Topic)插件       |
+---------------------------+---------------------------+
| `emq_mod_retainer`_       | Retain消息存储模块        |
+---------------------------+---------------------------+
| `emq_mod_presence`_       | 客户端上下线状态消息发布  |
+---------------------------+---------------------------+
| `emq_mod_subscription`_   | 客户端上线自动主题订阅    |
+---------------------------+---------------------------+
| `emq_coap`_               | CoAP协议支持              |
+---------------------------+---------------------------+
| `emq_sn`_                 | MQTT-SN协议支持           |
+---------------------------+---------------------------+
| `emq_stomp`_              | Stomp协议支持             |
+---------------------------+---------------------------+
| `emq_sockjs`_             | Stomp over SockJS协议支持 |
+---------------------------+---------------------------+
| `emq_recon`_              | Recon性能调试             |
+---------------------------+---------------------------+
| `emq_reloader`_           | Reloader代码热加载插件    |
+---------------------------+---------------------------+
| `emq_plugin_template`_    | 插件开发模版              |
+---------------------------+---------------------------+

------------------------------------
emq_auth_clientid - ClientID认证插件
------------------------------------

EMQ 2.0-rc.2版本将ClientId认证模块改为独立插件: https://github.com/emqtt/emq_auth_clientid

ClientID认证配置
----------------

etc/plugins/emq_auth_clientid.conf:

.. code-block:: properties

    ## auth.client.$clientid = $password
    ## Examples
    ## auth.client.id = passwd
    ## auth.client.dev:devid = passwd2
    ## auth.client.app:appid = passwd2

加载ClientId认证插件
--------------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emq_auth_clientid

-------------------------------------
emq_auth_username - 用户名密码认证插件
-------------------------------------

EMQ 2.0-rc.2版本将用户名认证模块改为独立插件: https://github.com/emqtt/emq_auth_username

用户名认证配置
--------------

etc/plugins/emq_auth_username.conf:

.. code-block:: properties

    ##auth.username.$name=$password

    ## Examples:
    ##auth.username.admin=public
    ##auth.username.feng@emqtt.io=public

两种方式添加用户:

1. 直接在etc/plugins/emq_auth_username.conf中明文配置默认用户例如::

    auth.username.test = public

2. 通过'./bin/emqttd_ctl'管理命令行添加用户::

   $ ./bin/emqttd_ctl users add <Username> <Password>

加载用户名认证插件
------------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emq_auth_username

---------------------------------
emq_plugin_template: 插件开发模版
---------------------------------

EMQ插件实际是一个普通的Erlang应用，插件配置文件: 'etc/${PluginName}.conf|config"。

emq_plugin_template是模版插件，编译发布在lib/emq_plugin_template-2.0目录，配置文件: etc/plugins/emq_plugin_templat.config

加载、卸载插件
--------------

管理命令行'./bin/emqttd_ctl'加载卸载插件。

加载插件::

    ./bin/emqttd_ctl plugins load <PluginName>

卸载插件::

    ./bin/emqttd_ctl plugins unload <PluginName>

查询插件::

    ./bin/emqttd_ctl plugins list

----------------------------
emq_dashboard: Dashboard插件
----------------------------

*EMQ* 消息服务器的Web管理控制台。插件项目地址: https://github.com/emqtt/emqttd_dashboard

*EMQ* 消息服务器默认加载Dashboard插件。URL地址: http://localhost:18083 ，缺省用户名/密码: admin/public。

Dashboard插件可查询EMQ消息服务器基本信息、统计数据、度量数据，查询系统客户端(Client)、会话(Session)、主题(Topic)、订阅(Subscription)。

.. image:: ./_static/images/dashboard.png

Dashboard插件设置
-----------------

etc/plugins/emq_dashboard.conf:

.. code-block:: properties

    ## HTTP Listener
    dashboard.listener.http = 18083
    dashboard.listener.http.acceptors = 2
    dashboard.listener.http.max_clients = 512

    ## HTTPS Listener
    ## dashboard.listener.https = 18084
    ## dashboard.listener.https.acceptors = 2
    ## dashboard.listener.https.max_clients = 512
    ## dashboard.listener.https.handshake_timeout = 15
    ## dashboard.listener.https.certfile = etc/certs/cert.pem
    ## dashboard.listener.https.keyfile = etc/certs/key.pem
    ## dashboard.listener.https.cacertfile = etc/certs/cacert.pem
    ## dashboard.listener.https.verify = verify_peer
    ## dashboard.listener.https.failed_if_no_peer_cert = true

---------------------------
emq_auth_ldap: LDAP认证插件
---------------------------

LDAP认证插件: https://github.com/emqtt/emq_auth_ldap

.. NOTE:: 2.0-beta1版本支持

LDAP认证插件配置
----------------

etc/plugins/emq_auth_ldap.conf:

.. code-block:: properties

    auth.ldap.servers = 127.0.0.1

    auth.ldap.port = 389

    auth.ldap.timeout = 30

    auth.ldap.user_dn = uid=%u,ou=People,dc=example,dc=com

    auth.ldap.ssl = false

LDAP认证插件加载
----------------

./bin/emqttd_ctl plugins load emq_auth_ldap

------------------------------------
emq_auth_http: HTTP认证/访问控制插件
------------------------------------

HTTP认证/访问控制插件: https://github.com/emqtt/emq_auth_http

.. NOTE:: 1.1版本支持

HTTP认证插件配置
----------------

etc/plugins/emq_auth_http.conf:

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

    auth.http.acl_nomatch = deny

HTTP认证/鉴权API
----------------

认证/ACL成功，API返回200

认证/ACL失败，API返回4xx

加载HTTP认证插件
----------------

./bin/emqttd_ctl plugins load emq_auth_http

--------------------------------------
emq_auth_mysql: MySQL认证/访问控制插件
--------------------------------------

MySQL认证/访问控制插件，基于MySQL库表认证鉴权: https://github.com/emqtt/emq_plugin_mysql

MQTT用户表
----------

.. code-block:: sql

    CREATE TABLE `mqtt_user` (
      `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
      `username` varchar(100) DEFAULT NULL,
      `password` varchar(100) DEFAULT NULL,
      `salt` varchar(20) DEFAULT NULL,
      `is_superuser` tinyint(1) DEFAULT 0,
      `created` datetime DEFAULT NULL,
      PRIMARY KEY (`id`),
      UNIQUE KEY `mqtt_username` (`username`)
    ) ENGINE=MyISAM DEFAULT CHARSET=utf8;

.. NOTE:: MySQL插件可使用系统自有的用户表，通过'authquery'配置查询语句。

MQTT访问控制表
--------------

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

配置MySQL认证鉴权插件
---------------------

etc/plugins/emq_plugin_mysql.conf:

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

    ## ACL nomatch
    auth.mysql.acl_nomatch = deny

加载MySQL认证鉴权插件
---------------------

./bin/emqttd_ctl plugins load emq_auth_mysql

----------------------------------------
emq_auth_pgsql: Postgre认证/访问控制插件
----------------------------------------

Postgre认证/访问控制插件，基于PostgreSQL库表认证鉴权: https://github.com/emqtt/emqttd_plugin_pgsql

Postgre MQTT用户表
------------------

.. code-block:: sql

    CREATE TABLE mqtt_user (
      id SERIAL primary key,
      is_superuser boolean,
      username character varying(100),
      password character varying(100),
      salt character varying(40)
    );

Postgre MQTT访问控制表
----------------------

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

配置Postgre认证鉴权插件
-----------------------

etc/plugins/emq_plugin_pgsql.conf:

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

    ## If no rules matched, return...
    auth.pgsql.acl_nomatch = deny

加载Postgre认证鉴权插件
-----------------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emq_auth_pgsql

--------------------------------------
emq_auth_redis: Redis认证/访问控制插件
--------------------------------------

基于Redis认证/访问控制: https://github.com/emqtt/emqttd_plugin_redis

配置Redis认证鉴权插件
---------------------

etc/plugins/emq_auth_redis.conf:

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

    ## ACL nomatch
    auth.redis.acl_nomatch = deny

Redis 用户Hash
--------------

默认基于用户Hash认证::

    HSET mqtt_user:<username> is_superuser 1
    HSET mqtt_user:<username> password "passwd"

Redis ACL规则Hash
-----------------

默认采用Hash存储ACL规则::

    HSET mqtt_acl:<username> topic1 1
    HSET mqtt_acl:<username> topic2 2
    HSET mqtt_acl:<username> topic3 3

.. NOTE:: 1: subscribe, 2: publish, 3: pubsub

Redis 订阅Hash
---------------

插件还支持Redis中创建MQTT订阅。当MQTT客户端连接成功，会自动从Redis加载订阅::

    HSET mqtt_sub:<username> topic1 0
    HSET mqtt_sub:<username> topic2 1
    HSET mqtt_sub:<username> topic3 2

.. WARNING:: 2.0-rc.2版本已将订阅加载迁移至EMQPlus产品的emqplus_backend_redis插件。

加载Redis认证鉴权插件
---------------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emq_auth_redis

----------------------------------------
emq_auth_mongo: MongoDB认证/访问控制插件
----------------------------------------

基于MongoDB认证/访问控制: https://github.com/emqtt/emqttd_plugin_mongo

配置MongoDB认证鉴权插件
-----------------------

etc/plugins/emq_plugin_mongo.conf:

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

    ## acl_nomatch
    auth.mongo.acl_nomatch = deny

MongoDB数据库
-------------

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

MongoDB ACL集合(ACL Collection)
-------------------------------

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

加载Mognodb认证插件
-------------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emq_auth_mongo

---------------------------------
emq_mod_presence Presence模块插件
---------------------------------

2.0-rc.3版本将Presence模块改为独立插件，Presence模块会向$SYS主题(Topic)发布客户端上下线消息。

配置Presence模块
----------------

etc/plugins/emq_mod_presence.conf:

.. code-block:: properties

    ## Enable presence module
    ## Values: on | off
    module.presence = on

    module.presence.qos = 0

加载Presence模块
----------------

Presence模块默认加载。

---------------------------------
emq_mod_retainer Retainer模块插件
---------------------------------

2.0-rc.3版本将Retainer模块改为独立插件， Retainer模块负责持久化MQTT Retained消息。

配置Retainer模块
----------------

etc/plugins/emq_mod_retainer.conf:

.. code-block:: properties

    ## disc: disc_copies, ram: ram_copies
    module.retainer.storage_type = ram

    ## Max number of retained messages
    module.retainer.max_message_num = 100000

    ## Max Payload Size of retained message
    module.retainer.max_payload_size = 64KB

    ## Expired after seconds, never expired if 0
    module.retainer.expired_after = 0

加载Retainer模块
----------------

Retainer模块默认加载。

-------------------------------------
emq_mod_subscription 自动订阅模块插件
-------------------------------------

2.0-rc.3版本将Subscription模块改为独立插件，Subscription扩展模块支持客户端上线时，自动订阅或恢复订阅某些主题(Topic)。

配置Subscription模块
--------------------

etc/plugins/emq_mod_subscription.conf:

.. code-block:: properties

    ## Subscribe the Topics automatically when client connected
    module.subscription.1.topic = $client/%c
    ## Qos of the subscription: 0 | 1 | 2
    module.subscription.1.qos = 1

    ##module.subscription.2.topic = $user/%u
    ##module.subscription.2.qos = 1

    ## Load static subscriptions from backend storage
    ## Values: on | off
    module.subscription.backend = on

加载Subscription模块
--------------------

Subscription模块默认加载。

--------------------------
emq_mod_rewrite主题重写插件
--------------------------

2.0-rc.2版本将rewrite模块改为独立插件，rewrite插件支持重写发布订阅的主题(Topic)。

配置Rewrite插件
---------------

etc/plugins/emq_mod_rewrite.conf:

.. code-block:: erlang

  [
    {emq_mod_rewrite, [
      {rules, [
        %% {rewrite, Topic, Re, Dest}
        
        %% Example: x/y/ -> z/y/
        %% {rewrite, "x/#", "^x/y/(.+)$", "z/y/$1"},

        %% {rewrite, "y/+/z/#", "^y/(.+)/z/(.+)$", "y/z/$2"}
      ]}
    ]}
  ].

加载Rewrite插件
---------------

.. code:: bash

    ./bin/emqttd_ctl plugins load emq_mod_rewrite

----------------------
emq_coap: CoAP协议插件
----------------------

CoAP协议插件，支持RFC 7252规范。

配置CoAP协议插件
----------------

.. code-block:: properties

  coap.server = 5683

  coap.prefix.mqtt = mqtt

  coap.handler.mqtt = emq_coap_gateway

加载CoAP协议插件
----------------

.. code:: bash

    ./bin/emqttd_ctl plugins load emq_coap

libcoap客户端
-------------

.. code:: bash

  yum install libcoap

  % coap client publish message
  coap-client -m post -e "qos=0&retain=0&message=payload&topic=hello" coap://localhost/mqtt

-----------------------
emq_sn: MQTT-SN协议插件
-----------------------

MQTT-SN协议插件，支持MQTT-SN网关模式。

配置MQTT-SN协议插件
-------------------

.. NOTE:: 默认MQTT-SN协议UDP端口: 1884

etc/plugins/emq_sn.conf:

.. code-block:: properties

    mqtt.sn.port = 1884

加载MQTT-SN协议插件
------------------

.. code::

    ./bin/emqttd_ctl plugins load emq_sn

------------------------
emq_stomp: Stomp协议插件
------------------------

Stomp协议插件。支持STOMP 1.0/1.1/1.2协议客户端连接emqttd，发布订阅MQTT消息。

配置插件
--------

.. NOTE:: Stomp协议端口: 61613

etc/plugins/emq_stomp.conf:

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

加载Stomp插件
-------------

.. code:: bash

    ./bin/emqttd_ctl plugins load emq_stomp

----------------------------
emq_sockjs: Stomp/Sockjs插件
----------------------------

.. WARNING:: 2.0版本不再维护SockJS插件

配置SockJS插件
--------------

etc/plugins/emq_sockjs.config:

.. NOTE:: 缺省端口: 61616

.. code-block:: erlang

  [
    {emq_sockjs, [

      {sockjs, []},

      {cowboy_listener, {stomp_sockjs, 61616, 4}},

      %% TODO: unused...
      {stomp, [
        {frame, [
          {max_headers,       10},
          {max_header_length, 1024},
          {max_body_length,   8192}
        ]}
      ]}
    ]}
  ].

加载SockJS插件
--------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emq_sockjs

插件演示页面
------------

    http://localhost:61616/index.html

----------------------------
emq_recon: Recon性能调试插件
----------------------------

emq_recon插件集成recon性能调测库，'./bin/emqttd_ctl'命令行注册recon命令。

配置Recon插件
------------

etc/plugins/emq_recon.conf:

.. code-block:: properties

    %% Garbage Collection: 10 minutes
    recon.gc_interval = 600

加载Recon插件
-------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emq_recon

recon插件命令
-------------

.. code-block:: bash

    ./bin/emqttd_ctl recon

    recon memory                 #recon_alloc:memory/2
    recon allocated              #recon_alloc:memory(allocated_types, current|max)
    recon bin_leak               #recon:bin_leak(100)
    recon node_stats             #recon:node_stats(10, 1000)
    recon remote_load Mod        #recon:remote_load(Mod)

----------------------------
emq_reloader: 代码热加载插件
----------------------------

用于开发调试的代码热升级插件。加载该插件后，emqttd会自动热升级更新代码。

.. NOTE:: 产品部署环境不建议使用该插件

配置Reloader插件
----------------

etc/plugins/emq_reloader.conf:

.. code-block:: properties

  reloader.interval = 60

  reloader.logfile = log/reloader.log

加载Reloader插件
----------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emq_reloader

Reloader插件命令
----------------

.. code-block:: bash

    ./bin/emqttd_ctl reload

    reload <Module>             # Reload a Module

---------------
EMQ 2.0插件开发
---------------

创建插件项目
------------

参考`emq_plugin_template`_ 插件模版创建新的插件项目。

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
| message.publish        | MQTT消息发布                     |
+------------------------+----------------------------------+
| message.delivered      | MQTT消息送达                     |
+------------------------+----------------------------------+
| message.acked          | MQTT消息回执                     |
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

    emqttd_ctl:register_cmd(cmd, {emq_cli_demo, cmd}, []).

插件加载后，'./bin/emqttd_ctl'新增命令行::

    ./bin/emqttd_ctl cmd arg1 arg2

插件配置文件
------------

插件自带配置文件放置在etc/${plugin_name}.conf|config，EMQ支持两种插件配置格式:

1. ${plugin_name}.config，Erlang原生配置文件格式:

.. code-block:: erlang

    [
      {plugin_name, [
        {key, value}
      ]}
    ].

2. ${plugin_name}.conf, sysctl的`k = v`通用格式:

.. code-block:: properties

    plugin_name.key = value

.. NOTE:: `k = v`格式配置需要插件开发者创建priv/plugin_name.schema映射文件。
 
编译发布插件
------------

1. clone emqttd-relx项目:

.. code-block:: bash

    git clone https://github.com/emqtt/emqttd-relx.git

2. Makefile增加`DEPS`:

.. code-block:: makefile

    DEPS += plugin_name
    dep_plugin_name = git url_of_plugin

3. relx.config中release段落添加:

.. code-block:: erlang

    {plugin_name, load},

.. _emq_dashboard:       https://github.com/emqtt/emqttd_dashboard
.. _emq_mod_retainer:     https://github.com/emqtt/emq_mod_retainer
.. _emq_mod_presence:     https://github.com/emqtt/emq_mod_presence
.. _emq_mod_subscription: https://github.com/emqtt/emq_mod_subscription
.. _emq_auth_clientid:   https://github.com/emqtt/emq_auth_clientid
.. _emq_auth_username:   https://github.com/emqtt/emq_auth_username
.. _emq_auth_ldap:       https://github.com/emqtt/emq_auth_ldap
.. _emq_auth_http:       https://github.com/emqtt/emq_auth_http
.. _emq_auth_mysql:      https://github.com/emqtt/emq_auth_mysql
.. _emq_auth_pgsql:      https://github.com/emqtt/emq_auth_pgsql
.. _emq_auth_redis:      https://github.com/emqtt/emq_auth_redis
.. _emq_auth_mongo:      https://github.com/emqtt/emq_auth_mongo
.. _emq_mod_rewrite:     https://github.com/emqtt/emq_mod_rewrite
.. _emq_sn:              https://github.com/emqtt/emq_sn
.. _emq_coap:            https://github.com/emqtt/emq_coap
.. _emq_stomp:           https://github.com/emqtt/emq_stomp
.. _emq_sockjs:          https://github.com/emqtt/emq_sockjs
.. _emq_recon:           https://github.com/emqtt/emq_recon
.. _emq_reloader:        https://github.com/emqtt/emq_reloader
.. _emq_plugin_template: https://github.com/emqtt/emq_plugin_template
.. _recon:               http://ferd.github.io/recon/

