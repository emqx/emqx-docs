
.. _plugins:

=================
扩展插件(Plugins)
=================

EMQ消息服务器通过模块注册和钩子(Hooks)机制，支持用户开发扩展插件定制服务器认证鉴权与业务功能。

emqtt项目组开发维护的插件包括:

+---------------------------+---------------------------+
| 插件                      | 说明                      |
+===========================+===========================+
| `emqttd_dashboard`_       | Web控制台插件(默认加载)   |
+---------------------------+---------------------------+
| `emqttd_auth_ldap`_       | LDAP认证/访问控制         |
+---------------------------+---------------------------+
| `emqttd_auth_http`_       | HTTP认证/访问控制         |
+---------------------------+---------------------------+
| `emqttd_auth_mysql`_      | MySQL认证/访问控制        |
+---------------------------+---------------------------+
| `emqttd_auth_pgsql`_      | PostgreSQL认证/访问控制   |
+---------------------------+---------------------------+
| `emqttd_auth_redis`_      | Redis认证/访问控制        |
+---------------------------+---------------------------+
| `emqttd_auth_mongo`_      | MongoDB认证/访问控制      |
+---------------------------+---------------------------+
| `emqttd_sn`_              | MQTT-SN协议支持           |
+---------------------------+---------------------------+
| `emqttd_stomp`_           | Stomp协议支持             |
+---------------------------+---------------------------+
| `emqttd_sockjs`_          | Stomp over SockJS协议支持 |
+---------------------------+---------------------------+
| `emqttd_recon`_           | Recon性能调试             |
+---------------------------+---------------------------+
| `emqttd_reloader`_        | Reloader代码热加载插件    |
+---------------------------+---------------------------+
| `emqttd_plugin_template`_ | 插件开发模版              |
+---------------------------+---------------------------+

------------------------------------
emqttd_plugin_template: 插件开发模版
------------------------------------

EMQ插件实际是一个普通的Erlang应用，带自身的配置文件'etc/${PluginName}.conf"。

emqttd_plugin_template是一个模版插件，插件应用在lib/emqttd_plugin_template-2.0目录下，配置文件: etc/plugins/emqttd_plugin_templat.conf

加载、卸载插件
--------------

管理命令行'./bin/emqttd_ctl'加载卸载插件。

加载插件::

    ./bin/emqttd_ctl plugins load <PluginName>

卸载插件::

    ./bin/emqttd_ctl plugins unload <PluginName>

查询插件::

    ./bin/emqttd_ctl plugins list

-------------------------------
emqttd_dashboard: Dashboard插件
-------------------------------

EMQ消息服务器的Web管理控制台。插件项目地址: https://github.com/emqtt/emqttd_dashboard

EMQ消息服务器默认加载Dashboard插件。URL地址: http://localhost:18083 ，缺省用户名/密码: admin/public。

Dashboard插件可查询emqttd基本信息、统计数据、度量数据，查询系统客户端(Client)、会话(Session)、主题(Topic)、订阅(Subscription)。

.. image:: ./_static/images/dashboard.png

Dashboard插件设置
-----------------

etc/plugins/emqttd_dashboard.conf::

    {listener,
      {dashboard, 18083, [
        {acceptors, 4},
        {max_clients, 512}
      ]}
    }.

------------------------------
emqttd_auth_ldap: LDAP认证插件
------------------------------

LDAP认证插件: https://github.com/emqtt/emqttd_auth_ldap

.. NOTE:: 2.0-beta1版本支持

LDAP认证插件配置
----------------

etc/plugins/emqttd_auth_ldap.conf:

.. code:: erlang

    {ldap, [
        {servers, ["localhost"]},
        {port, 389},
        {timeout, 30},
        {user_dn, "uid=$u,ou=People,dc=example,dc=com"},
        {ssl, fasle},
        {sslopts, [
            {certfile, "ssl.crt"},
            {keyfile, "ssl.key"}
        ]}
    ]}.

LDAP认证插件加载
----------------

./bin/emqttd_ctl plugins load emqttd_auth_ldap

---------------------------------------
emqttd_auth_http: HTTP认证/访问控制插件
---------------------------------------

HTTP认证/访问控制插件: https://github.com/emqtt/emqttd_auth_http

.. NOTE:: 1.1版本支持

HTTP认证插件配置
---------------

etc/plugins/emqttd_auth_http.conf:

.. code:: erlang

    %% Variables: %u = username, %c = clientid, %a = ipaddress, %t = topic

    {super_req, [
      {method, post},
      {url, "http://localhost:8080/mqtt/superuser"},
      {params, [
        {username, "%u"},
        {clientid, "%c"}
      ]}
    ]}.

    {auth_req, [
      {method, post},
      {url, "http://localhost:8080/mqtt/auth"},
      {params, [
        {clientid, "%c"},
        {username, "%u"},
        {password, "%P"}
      ]}
    ]}.

    %% 'access' parameter: sub = 1, pub = 2

    {acl_req, [
      {method, post},
      {url, "http://localhost:8080/mqtt/acl"},
      {params, [
        {access,   "%A"},
        {username, "%u"},
        {clientid, "%c"},
        {ipaddr,   "%a"},
        {topic,    "%t"}
      ]}
    ]}.

HTTP认证/鉴权API
----------------

认证/ACL成功，API返回200

认证/ACL失败，API返回4xx

加载HTTP认证插件
----------------

./bin/emqttd_ctl plugins load emqttd_auth_http

-----------------------------------------
emqttd_auth_mysql: MySQL认证/访问控制插件
-----------------------------------------

MySQL认证/访问控制插件，基于MySQL库表认证鉴权: https://github.com/emqtt/emqttd_plugin_mysql

MQTT用户表
----------

.. code:: sql

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

配置MySQL认证鉴权插件
---------------------

etc/plugins/emqttd_plugin_mysql.conf:

.. code:: erlang

    {mysql_pool, [
      %% pool options
      {pool_size, 8},
      {auto_reconnect, 1},

      %% mysql options
      {host,     "localhost"},
      {port,     3306},
      {user,     ""},
      {password, ""},
      {database, "mqtt"},
      {encoding, utf8},
      {keep_alive, true}
    ]}.

    %% Variables: %u = username, %c = clientid, %a = ipaddress

    %% Superuser Query
    {superquery, "select is_superuser from mqtt_user where username = '%u' limit 1"}.

    %% Authentication Query: select password only
    {authquery, "select password from mqtt_user where username = '%u' limit 1"}.

    %% hash algorithm: plain, md5, sha, sha256, pbkdf2?
    {password_hash, sha256}.

    %% select password with salt
    %% {authquery, "select password, salt from mqtt_user where username = '%u'"}.

    %% sha256 with salt prefix
    %% {password_hash, {salt, sha256}}.

    %% sha256 with salt suffix
    %% {password_hash, {sha256, salt}}.

    %% '%a' = ipaddress, '%u' = username, '%c' = clientid
    %% Comment this query, the acl will be disabled
    {aclquery, "select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'"}.

    %% If no ACL rules matched, return...
    {acl_nomatch, allow}.

加载MySQL认证鉴权插件
---------------------

./bin/emqttd_ctl plugins load emqttd_auth_mysql

----------------------------------------------
emqttd_auth_pgsql: Postgre认证/访问控制插件
----------------------------------------------

Postgre认证/访问控制插件，基于PostgreSQL库表认证鉴权: https://github.com/emqtt/emqttd_plugin_pgsql

Postgre MQTT用户表
------------------

.. code:: sql

    CREATE TABLE mqtt_user (
      id SERIAL primary key,
      is_superuser boolean,
      username character varying(100),
      password character varying(100),
      salt character varying(40)
    );

Postgre MQTT访问控制表
----------------------

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

配置Postgre认证鉴权插件
-----------------------

etc/plugins/emqttd_plugin_pgsql.conf:

.. code:: erlang

    {pgsql_pool, [
      %% pool options
      {pool_size, 8},
      {auto_reconnect, 3},

      %% pgsql options
      {host, "localhost"},
      {port, 5432},
      {ssl, false},
      {username, "feng"},
      {password, ""},
      {database, "mqtt"},
      {encoding,  utf8}
    ]}.

    %% Variables: %u = username, %c = clientid, %a = ipaddress

    %% Superuser Query
    {superquery, "select is_superuser from mqtt_user where username = '%u' limit 1"}.

    %% Authentication Query: select password only
    {authquery, "select password from mqtt_user where username = '%u' limit 1"}.

    %% hash algorithm: plain, md5, sha, sha256, pbkdf2?
    {password_hash, sha256}.

    %% select password with salt
    %% {authquery, "select password, salt from mqtt_user where username = '%u'"}.

    %% sha256 with salt prefix
    %% {password_hash, {salt, sha256}}.

    %% sha256 with salt suffix
    %% {password_hash, {sha256, salt}}.

    %% Comment this query, the acl will be disabled. Notice: don't edit this query!
    {aclquery, "select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'"}.

    %% If no rules matched, return...
    {acl_nomatch, allow}.


加载Postgre认证鉴权插件
-----------------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emqttd_auth_pgsql

-----------------------------------------
emqttd_auth_redis: Redis认证/访问控制插件
-----------------------------------------

基于Redis认证/访问控制: https://github.com/emqtt/emqttd_plugin_redis

配置Redis认证鉴权插件
---------------------

etc/plugins/emqttd_plugin_redis.conf::

    {redis_pool, [
      %% pool options
      {pool_size, 8},
      {auto_reconnect, 2},

      %% redis options
      {host, "127.0.0.1"},
      {port, 6379},
      {database, 0},
      {password, ""}
    ]}.

    %% Variables: %u = username, %c = clientid

    %% HMGET mqtt_user:%u is_superuser
    {supercmd, ["HGET", "mqtt_user:%u", "is_superuser"]}.

    %% HMGET mqtt_user:%u password
    {authcmd, ["HGET", "mqtt_user:%u", "password"]}.

    %% Password hash algorithm: plain, md5, sha, sha256, pbkdf2?
    {password_hash, sha256}.

    %% SMEMBERS mqtt_acl:%u
    {aclcmd, ["SMEMBERS", "mqtt_acl:%u"]}.

    %% If no rules matched, return...
    {acl_nomatch, deny}.

    %% Load Subscriptions form Redis when client connected.
    {subcmd, ["HGETALL", "mqtt_subs:%u"]}.

Redis 用户Hash
--------------

默认基于用户Hash认证::

    HSET mqtt_user:<username> is_superuser 1
    HSET mqtt_user:<username> password "passwd"

Redis ACL规则SET
-----------------

默认采用SET存储ACL规则::

    SADD mqtt_acl:<username> "publish topic1"
    SADD mqtt_acl:<username> "subscribe topic2"
    SADD mqtt_acl:<username> "pubsub topic3"

Redis 订阅Hash
---------------

插件还支持Redis中创建MQTT订阅。当MQTT客户端连接成功，会自动从Redis加载订阅::

    HSET mqtt_subs:<username> topic1 0
    HSET mqtt_subs:<username> topic2 1
    HSET mqtt_subs:<username> topic3 2

加载Redis认证鉴权插件
---------------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emqttd_auth_redis

-------------------------------------------
emqttd_auth_mongo: MongoDB认证/访问控制插件
-------------------------------------------

基于MongoDB认证/访问控制: https://github.com/emqtt/emqttd_plugin_mongo

配置MongoDB认证鉴权插件
-----------------------

etc/plugins/emqttd_plugin_mongo.conf::

    {mongo_pool, [
      {pool_size, 8},
      {auto_reconnect, 3},

      %% Mongodb Opts
      {host, "localhost"},
      {port, 27017},
      %% {login, ""},
      %% {password, ""},
      {database, "mqtt"}
    ]}.

    %% Variables: %u = username, %c = clientid

    %% Superuser Query
    {superquery, pool, [
      {collection, "mqtt_user"},
      {super_field, "is_superuser"},
      {selector, {"username", "%u"}}
    ]}.

    %% Authentication Query
    {authquery, pool, [
      {collection, "mqtt_user"},
      {password_field, "password"},
      %% Hash Algorithm: plain, md5, sha, sha256, pbkdf2?
      {password_hash, sha256},
      {selector, {"username", "%u"}}
    ]}.

    %% ACL Query: "%u" = username, "%c" = clientid
    {aclquery, pool, [
      {collection, "mqtt_acl"},
      {selector, {"username", "%u"}}
    ]}.

    %% If no ACL rules matched, return...
    {acl_nomatch, deny}.

MongoDB数据库
-------------

.. code-block::

    use mqtt
    db.createCollection("mqtt_user")
    db.createCollection("mqtt_acl")
    db.mqtt_user.ensureIndex({"username":1})

.. NOTE:: 数据库、集合名称可自定义

MongoDB 用户集合(User Collection)
---------------------------------

.. code-block:: json

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
------------------------------

.. code-block:: json

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

    ./bin/emqttd_ctl plugins load emqttd_auth_mongo

--------------------------
emqttd_sn: MQTT-SN协议插件
--------------------------

MQTT-SN协议插件，支持MQTT-SN网关模式。

配置MQTT-SN协议插件
-------------------

.. NOTE:: 默认MQTT-SN协议UDP端口: 1884

etc/plugins/emqttd_sn.conf::

    {listener, {1884, []}}.

加载MQTT-SN协议插件
------------------

.. code::

    ./bin/emqttd_ctl plugins load emqttd_sn

---------------------------
emqttd_stomp: Stomp协议插件
---------------------------

Stomp协议插件。支持STOMP 1.0/1.1/1.2协议客户端连接emqttd，发布订阅MQTT消息。

配置插件
--------

.. NOTE:: Stomp协议端口: 61613

etc/plugins/emqttd_stomp.conf:

.. code:: erlang

    {default_user, [
        {login,    "guest"},
        {passcode, "guest"}
    ]}.

    {allow_anonymous, true}.

    {frame, [
      {max_headers,       10},
      {max_header_length, 1024},
      {max_body_length,   8192}
    ]}.

    {listener, emqttd_stomp, 61613, [
        {acceptors,   4},
        {max_clients, 512}
    ]}.

加载Stomp插件
-------------

.. code::

    ./bin/emqttd_ctl plugins load emqttd_stomp

-------------------------------
emqttd_sockjs: Stomp/Sockjs插件
-------------------------------

配置SockJS插件
--------------

etc/plugins/emqttd_sockjs.conf:

.. NOTE:: 缺省端口: 61616

.. code-block:: erlang

    {sockjs, []}.

    {cowboy_listener, {stomp_sockjs, 61616, 4}}.

    %% TODO: unused...
    {stomp, [
      {frame, [
        {max_headers,       10},
        {max_header_length, 1024},
        {max_body_length,   8192}
      ]}
    ]}.

加载SockJS插件
--------------

.. NOTE:: 需先加载emqttd_stomp插件

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emqttd_stomp

    ./bin/emqttd_ctl plugins load emqttd_sockjs

插件演示页面
------------

    http://localhost:61616/index.html

-------------------------------
emqttd_recon: Recon性能调试插件
-------------------------------

emqttd_recon插件集成recon性能调测库，'./bin/emqttd_ctl'命令行注册recon命令。

配置Recon插件
------------

etc/plugins/emqttd_recon.conf:

.. code-block:: erlang

    %% Garbage Collection: 10 minutes
    {gc_interval, 600}.

加载Recon插件
------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emqttd_recon

recon插件命令
-------------

.. code-block:: bash

    ./bin/emqttd_ctl recon

    recon memory                 #recon_alloc:memory/2
    recon allocated              #recon_alloc:memory(allocated_types, current|max)
    recon bin_leak               #recon:bin_leak(100)
    recon node_stats             #recon:node_stats(10, 1000)
    recon remote_load Mod        #recon:remote_load(Mod)

-------------------------------
emqttd_reloader: 代码热加载插件
-------------------------------

用于开发调试的代码热升级插件。加载该插件后，emqttd会自动热升级更新代码。

.. NOTE:: 产品部署环境不建议使用该插件

配置Reloader插件
----------------

etc/plugins/emqttd_reloader.conf:

.. code-block:: erlang

    {interval, 60}.

    {logfile, "log/emqttd_reloader.log"}.

加载Reloader插件
---------------

.. code-block:: bash

    ./bin/emqttd_ctl plugins load emqttd_reloader

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

github clone emqttd_plugin_template插件模版库，参考插件模版创建新的插件应用项目。

注册认证/访问控制模块
---------------------

认证演示模块 - emqttd_auth_demo.erl

.. code-block:: erlang

    -module(emqttd_auth_demo).

    -behaviour(emqttd_auth_mod).

    -include("../../../include/emqttd.hrl").

    -export([init/1, check/3, description/0]).

    init(Opts) -> {ok, Opts}.

    check(#mqtt_client{client_id = ClientId, username = Username}, Password, _Opts) ->
        io:format("Auth Demo: clientId=~p, username=~p, password=~p~n",
                  [ClientId, Username, Password]),
        ok.

    description() -> "Demo Auth Module".

访问控制演示模块 - emqttd_acl_demo.erl

.. code-block:: erlang

    -module(emqttd_acl_demo).

    -include("../../../include/emqttd.hrl").

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

注册认证、访问控制模块 - emqttd_plugin_template_app.erl

.. code-block:: erlang

    ok = emqttd_access_control:register_mod(auth, emqttd_auth_demo, []),
    ok = emqttd_access_control:register_mod(acl, emqttd_acl_demo, []),


注册扩展钩子(Hooks)
--------------------

通过钩子(Hook)处理客户端上下线、主题订阅、消息收发。

emqttd_plugin_template.erl::

    %% Called when the plugin application start
    load(Env) ->
        emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
        emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
        emqttd:hook('client.subscribe', fun ?MODULE:on_client_subscribe/3, [Env]),
        emqttd:hook('client.subscribe.after', fun ?MODULE:on_client_subscribe_after/3, [Env]),
        emqttd:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/3, [Env]),
        emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
        emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/3, [Env]),
        emqttd:hook('message.acked', fun ?MODULE:on_message_acked/3, [Env]).

扩展钩子(Hook):

+------------------------+----------------------------------+
| 钩子                   | 说明                             |
+========================+==================================+
| client.connected       | 客户端上线                       |
+------------------------+----------------------------------+
| client.subscribe       | 客户端订阅主题前                 |
+------------------------+----------------------------------+
| client.subscribe.after | 客户端订阅主题后                 |
+------------------------+----------------------------------+
| client.unsubscribe     | 客户端取消订阅主题               |
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

扩展命令行演示模块 - emqttd_cli_demo.erl

.. code-block:: erlang

    -module(emqttd_cli_demo).

    -include("../../../include/emqttd_cli.hrl").

    -export([cmd/1]).

    cmd(["arg1", "arg2"]) ->
        ?PRINT_MSG("ok");

    cmd(_) ->
        ?USAGE([{"cmd arg1 arg2",  "cmd demo"}]).

注册命令行模块 - emqttd_plugin_template_app.erl

.. code-block:: erlang

    emqttd_ctl:register_cmd(cmd, {emqttd_cli_demo, cmd}, []).

插件加载后，'./bin/emqttd_ctl'新增命令行::

    ./bin/emqttd_ctl cmd arg1 arg2

.. _emqttd_dashboard:       https://github.com/emqtt/emqttd_dashboard
.. _emqttd_auth_ldap:       https://github.com/emqtt/emqttd_auth_ldap
.. _emqttd_auth_http:       https://github.com/emqtt/emqttd_auth_http
.. _emqttd_auth_mysql:      https://github.com/emqtt/emqttd_plugin_mysql
.. _emqttd_auth_pgsql:      https://github.com/emqtt/emqttd_plugin_pgsql
.. _emqttd_auth_redis:      https://github.com/emqtt/emqttd_plugin_redis
.. _emqttd_auth_mongo:      https://github.com/emqtt/emqttd_plugin_mongo
.. _emqttd_sn:              https://github.com/emqtt/emqttd_sn
.. _emqttd_stomp:           https://github.com/emqtt/emqttd_stomp
.. _emqttd_sockjs:          https://github.com/emqtt/emqttd_sockjs
.. _emqttd_recon:           https://github.com/emqtt/emqttd_recon
.. _emqttd_reloader:        https://github.com/emqtt/emqttd_reloader
.. _emqttd_plugin_template: https://github.com/emqtt/emqttd_plugin_template
.. _recon:                  http://ferd.github.io/recon/

