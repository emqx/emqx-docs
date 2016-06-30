
.. _guide:

====================
用户指南(User Guide)
====================

.. _authentication:

--------------------
认证(Authentication)
--------------------

emqttd消息服务器支持按ClientID、用户名/密码、IP地址、HTTP Cookie认证MQTT客户端。认证机制由一系列扩展模块与认证插件实现，认证插件包括MySQL、PostgreSQL、Redis与Mongodb。

etc/emqttd.config配置启用一个认证模块::

    %% Authentication and Authorization
    {access, [
        %% Authetication. Anonymous Default
        {auth, [
            %% Authentication with username, password
            %{username, []},

            %% Authentication with clientid
            %{clientid, [{password, no}, {file, "etc/clients.config"}]},

            %% Authentication with LDAP
            % {ldap, [
            %    {servers, ["localhost"]},
            %    {port, 389},
            %    {timeout, 30},
            %    {user_dn, "uid=$u,ou=People,dc=example,dc=com"},
            %    {ssl, fasle},
            %    {sslopts, [
            %        {"certfile", "ssl.crt"},
            %        {"keyfile", "ssl.key"}]}
            % ]},

            %% Allow all
            {anonymous, []}
        ]},

.. NOTE:: "%" 注释一行

如果同时启用多个认证模块，认证流程如下::

               ----------------           ----------------           -------------
    Client --> |   Username   | -ignore-> |   ClientID   | -ignore-> | Anonymous |
               ----------------           ----------------           -------------
                      |                         |                         |
                     \|/                       \|/                       \|/
                allow | deny              allow | deny              allow | deny

emqtt项目维护开发的认证插件:

+---------------------------+---------------------------+
| 插件                      | 说明                      |
+===========================+===========================+
| `emqttd_auth_http`_       | HTTP认证/鉴权插件         |
+---------------------------+---------------------------+
| `emqttd_plugin_mysql`_    | MySQL认证/鉴权插件        |
+---------------------------+---------------------------+
| `emqttd_plugin_pgsql`_    | PostgreSQL认证/鉴权插件   |
+---------------------------+---------------------------+
| `emqttd_plugin_redis`_    | Redis认证/鉴权插件        |
+---------------------------+---------------------------+
| `emqttd_plugin_mongo`_    | MongoDB认证/鉴权插件      |
+---------------------------+---------------------------+

.. NOTE:: 如果加载认证插件，etc/emqttd.config中配置的认证模块失效

用户名认证
----------

基于MQTT登录用户名(username)、密码(password)认证::

    {username, [{client1, "passwd1"}, {client1, "passwd2"}]},

两种方式添加认证用户:

1. 直接配置用户名和明文密码::

    {username, [{client1, "passwd1"}, {client1, "passwd2"}]},

2. 使用'./bin/emqttd_ctl users'命令添加用户::

   $ ./bin/emqttd_ctl users add <Username> <Password>

ClientId认证
------------

基于MQTT客户端ID(ClientId)认证::

    {clientid, [{password, no}, {file, "etc/clients.config"}]},

etc/clients.config添加客户端ID::

    testclientid0
    testclientid1 127.0.0.1
    testclientid2 192.168.0.1/24

LDAP认证
--------

.. code-block:: erlang

    {ldap, [
       {servers, ["localhost"]},
       {port, 389},
       {timeout, 30},
       {user_dn, "uid=$u,ou=People,dc=example,dc=com"},
       {ssl, fasle},
       {sslopts, [
           {"certfile", "ssl.crt"},
           {"keyfile", "ssl.key"}]}
    ]},

匿名认证(Anonymous)
-------------------

emqttd消息服务器默认采用匿名认证，允许任何客户端登录::

    {anonymous, []}

HTTP插件认证
------------

emqttd_auth_http/etc/plugin.config配置'super_req', 'auth_req'::

    [

      {emqttd_auth_http, [

        %% Variables: %u = username, %c = clientid, %a = ipaddress, %t = topic

        {super_req, [
          {method, post},
          {url, "http://localhost:8080/mqtt/superuser"},
          {params, [
            {username, "%u"},
            {clientid, "%c"}
          ]}
        ]},

        {auth_req, [
          {method, post},
          {url, "http://localhost:8080/mqtt/auth"},
          {params, [
            {clientid, "%c"},
            {username, "%u"},
            {password, "%P"}
          ]}
        ]},

        ...

启用插件::

    ./bin/emqttd_ctl plugins load emqttd_auth_http


MySQL插件认证
-------------

通过MySQL数据库表认证，可创建如下的'mqtt_user'表::

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

emqttd_plugin_mysql/etc/plugin.config配置'superquery', 'authquery', 'password_hash'::

    [

    {emqttd_plugin_mysql, [

        ...

        %% Variables: %u = username, %c = clientid, %a = ipaddress

        %% Superuser Query
        {superquery, "select is_superuser from mqtt_user where username = '%u' limit 1"},

        %% select password only
        {authquery, "select password from mqtt_user where username = '%u' limit 1"},

        %% hash algorithm: md5, sha, sha256, pbkdf2?
        {password_hash, sha256},

        %% select password with salt
        %% {authquery, "select password, salt from mqtt_user where username = '%u'"},

        %% sha256 with salt prefix
        %% {password_hash, {salt, sha256}},

        %% sha256 with salt suffix
        %% {password_hash, {sha256, salt}},

        ...

    ]}
    ].

.. NOTE:: 如果系统已有MQTT认证表，可通过配置'authquery'查询语句集成。

启用插件::

    ./bin/emqttd_ctl plugins load emqttd_plugin_mysql

PostgreSQL插件认证
------------------

通过PostgreSQL数据库表认证，可创建如下的'mqtt_user'表::

    CREATE TABLE mqtt_user (
      id SERIAL primary key,
      is_superuser boolean,
      username character varying(100),
      password character varying(100),
      salt character varying(40)
    );

emqttd_plugin_pgsql/etc/plugin.config配置'authquery'、'password_hash'::

    [

      {emqttd_plugin_pgsql, [

        ...

        %% Variables: %u = username, %c = clientid, %a = ipaddress

        %% Superuser Query
        {superquery, "select is_superuser from mqtt_user where username = '%u' limit 1"},

        %% Authentication Query: select password only
        {authquery, "select password from mqtt_user where username = '%u' limit 1"},

        %% hash algorithm: plain, md5, sha, sha256, pbkdf2?
        {password_hash, sha256},

        %% select password with salt
        %% {authquery, "select password, salt from mqtt_user where username = '%u'"},

        %% sha256 with salt prefix
        %% {password_hash, {salt, sha256}},

        %% sha256 with salt suffix
        %% {password_hash, {sha256, salt}},

        ...

      ]}
    ].

启用插件::

    ./bin/emqttd_ctl plugins load emqttd_plugin_pgsql

Redis插件认证
-------------

Redis认证。MQTT用户记录存储在Redis Hash, 键值: "mqtt_user:<Username>"

emqttd_plugin_redis/etc/plugin.config设置'supercmd'、'authcmd'、'password_hash'::

    [
      {emqttd_plugin_redis, [

        ...

        %% Variables: %u = username, %c = clientid

        %% HMGET mqtt_user:%u is_superuser
        {supercmd, ["HGET", "mqtt_user:%u", "is_superuser"]},
        
        %% HMGET mqtt_user:%u password
        {authcmd, ["HGET", "mqtt_user:%u", "password"]},

        %% Password hash algorithm: plain, md5, sha, sha256, pbkdf2?
        {password_hash, sha256},
        ...

      ]}
    ].

启用插件::

    ./bin/emqttd_ctl plugins load emqttd_plugin_redis


MongoDB插件认证
---------------

按MongoDB用户集合认证，例如创建'mqtt_user'集合::

    {
        username: "user",
        password: "password hash",
        is_superuser: boolean (true, false),
        created: "datetime"
    }

emqttd_plugin_mongo/etc/plugin.config设置'superquery'、'authquery'::

    [
      {emqttd_plugin_mongo, [

        ...

        %% Variables: %u = username, %c = clientid

        %% Superuser Query
        {superquery, [
          {collection, "mqtt_user"},
          {super_field, "is_superuser"},
          {selector, {"username", "%u"}}
        ]},

        %% Authentication Query
        {authquery, [
          {collection, "mqtt_user"},
          {password_field, "password"},
          %% Hash Algorithm: plain, md5, sha, sha256, pbkdf2?
          {password_hash, sha256},
          {selector, {"username", "%u"}}
        ]},

        ...

      ]}
    ].


启用插件::

    ./bin/emqttd_ctl plugins load emqttd_plugin_mongodb

.. _acl:

-------------
访问控制(ACL)
-------------

emqttd消息服务器通过ACL(Access Control List)实现MQTT客户端访问控制。

ACL访问控制规则定义::

    允许(Allow)|拒绝(Deny) 谁(Who) 订阅(Subscribe)|发布(Publish) 主题列表(Topics)

MQTT客户端发起订阅/发布请求时，emqttd消息服务器的访问控制模块，会逐条匹配ACL规则，直到匹配成功为止::

              ---------              ---------              ---------
    Client -> | Rule1 | --nomatch--> | Rule2 | --nomatch--> | Rule3 | --> Default
              ---------              ---------              ---------
                  |                      |                      |
                match                  match                  match
                 \|/                    \|/                    \|/
            allow | deny           allow | deny           allow | deny

Internal访问控制
----------------

emqttd消息服务器默认的访问控制，由一个'internal'模块实现，etc/emqttd.config中配置::

    {acl, [
        %% Internal ACL module
        {internal,  [{file, "etc/acl.config"}, {nomatch, allow}]}
    ]}

ACL规则通过etc/acl.config配置，emqttd启动时加载到ETS内存表::

    %% Allow 'dashboard' to subscribe '$SYS/#'
    {allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.

    %% Allow clients from localhost to subscribe any topics
    {allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.

    %% Deny clients to subscribe '$SYS#' and '#'
    {deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.

    %% Allow all by default
    {allow, all}.


HTTP插件访问控制
------------------

HTTP API实现访问控制: https://github.com/emqtt/emqttd_auth_http

启用HTTP认证插件后，配置emqttd_auth_http/etc/plugin.config::

    ...

    %% Variables: %u = username, %c = clientid, %a = ipaddress, %t = topic
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
    ]}

MySQL插件访问控制
------------------

MySQL插件访问控制，通过mqtt_acl表定义ACL规则::

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

    INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)
    VALUES
        (1,1,NULL,'$all',NULL,2,'#'),
        (2,0,NULL,'$all',NULL,1,'$SYS/#'),
        (3,0,NULL,'$all',NULL,1,'eq #'),
        (5,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),
        (6,1,'127.0.0.1',NULL,NULL,2,'#'),
        (7,1,NULL,'dashboard',NULL,1,'$SYS/#');

emqttd_plugin_mysql/etc/plugin.config配置'aclquery'与'acl_nomatch'::

    [

      {emqttd_plugin_mysql, [

        ...

        %% comment this query, the acl will be disabled
        {aclquery, "select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'"},

        %% If no rules matched, return...
        {acl_nomatch, allow}

      ]}
    ].

PostgreSQL插件访问控制
-----------------------

PostgreSQL插件访问控制，通过mqtt_acl表定义ACL规则::

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

emqttd_plugin_pgsql/etc/plugin.config设置'aclquery'与'acl_nomatch'::

    [

      {emqttd_plugin_pgsql, [

        ...

        %% Comment this query, the acl will be disabled. Notice: don't edit this query!
        {aclquery, "select allow, ipaddr, username, clientid, access, topic from mqtt_acl
                     where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'"},

        %% If no rules matched, return...
        {acl_nomatch, allow}

        ...

      ]}
    ].

Redis插件访问控制
-----------------

Redis List存储一个MQTT客户端的访问控制规则，键值: "mqtt_acl:<Username>"，List存储: 存储"publish <Topic>", "subscribe <Topic>" 或 "pubsub <Topic>".

emqttd_plugin_redis/etc/plugin.config配置'aclcmd'与'acl_nomatch'::

    [
      {emqttd_plugin_redis, [

        ...

        %% SMEMBERS mqtt_acl:%u
        {aclcmd, ["SMEMBERS", "mqtt_acl:%u"]},

        %% If no rules matched, return...
        {acl_nomatch, deny},

        ...

      ]}
    ].

MongoDB插件访问控制
-------------------

MongoDB数据库创建'mqtt_acl'集合::

    {
        username: "username",
        clientid: "clientid",
        publish: ["topic1", "topic2", ...],
        subscribe: ["subtop1", "subtop2", ...],
        pubsub: ["topic/#", "topic1", ...]
    }

'mqtt_acl'集合插入数据，例如::

    db.mqtt_acl.insert({username: "test", publish: ["t/1", "t/2"], subscribe: ["user/%u", "client/%c"]})
    db.mqtt_acl.insert({username: "admin", pubsub: ["#"]})

emqttd_plugin_mongodb/etc/plugin.config配置'aclquery'与'acl_nomatch'::

    %% ACL Query: "%u" = username, "%c" = clientid
    {aclquery, [
      {collection, "mqtt_acl"},
      {selector, {"username", "%u"}}
    ]},

    %% If no ACL rules matched, return...
    {acl_nomatch, deny}

-------------
MQTT发布订阅
-------------

MQTT是为移动互联网、物联网设计的轻量发布订阅模式的消息服务器:

.. image:: ./_static/images/pubsub_concept.png

emqttd消息服务器安装启动后，任何设备或终端的MQTT客户端，可通过MQTT协议连接到emqttd，发布订阅消息方式互通。

MQTT协议客户端库: https://github.com/mqtt/mqtt.github.io/wiki/libraries

例如，mosquitto_sub/pub命令行发布订阅消息::

    mosquitto_sub -t topic -q 2
    mosquitto_pub -t topic -q 1 -m "Hello, MQTT!"

MQTT V3.1.1版本协议规范: http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/mqtt-v3.1.1.html

emqttd消息服务器的MQTT协议TCP监听器，可在etc/emqttd.config文件中设置::

        {mqtt, 1883, [
            %% Size of acceptor pool
            {acceptors, 16},

            %% Maximum number of concurrent clients
            {max_clients, 512},

            %% Socket Access Control
            {access, [{allow, all}]},

            %% Connection Options
            {connopts, [
                %% Rate Limit. Format is 'burst, rate', Unit is KB/Sec
                %% {rate_limit, "100,10"} %% 100K burst, 10K rate
            ]},

            %% Socket Options
            {sockopts, [
                %Set buffer if hight thoughtput
                %{recbuf, 4096},
                %{sndbuf, 4096},
                %{buffer, 4096},
                %{nodelay, true},
                {backlog, 512}
            ]}
        ]},

MQTT(SSL) TCP监听器，缺省端口8883::

        {mqtts, 8883, [
            %% Size of acceptor pool
            {acceptors, 4},

            %% Maximum number of concurrent clients
            {max_clients, 512},

            %% Socket Access Control
            {access, [{allow, all}]},

            %% SSL certificate and key files
            {ssl, [{certfile, "etc/ssl/ssl.crt"},
                   {keyfile,  "etc/ssl/ssl.key"}]},

            %% Socket Options
            {sockopts, [
                {backlog, 1024}
                %{buffer, 4096},
            ]}
        ]},

.. _http_publish:

------------
HTTP发布接口
------------

emqttd消息服务器提供了一个HTTP发布接口，应用服务器或Web服务器可通过该接口发布MQTT消息::

    HTTP POST http://host:8083/mqtt/publish

Web服务器例如PHP/Java/Python/NodeJS或Ruby on Rails，可通过HTTP POST请求发布MQTT消息::

    curl -v --basic -u user:passwd -d "qos=1&retain=0&topic=/a/b/c&message=hello from http..." -k http://localhost:8083/mqtt/publish

HTTP接口参数:

+---------+----------------+
| 参数    | 说明           |
+=========+================+
| client  | MQTT客户端ID   |
+---------+----------------+
| qos     | QoS: 0 | 1 | 2 |
+---------+----------------+
| retain  | Retain: 0 | 1  |
+---------+----------------+
| topic   | 主题(Topic)    |
+---------+----------------+
| message | 消息           |
+---------+----------------+

.. NOTE:: HTTP接口采用Basic认证

------------------
MQTT WebSocket连接
------------------

emqttd消息服务器支持MQTT WebSocket连接，Web浏览器可直接通过MQTT协议连接到emqttd:

+-------------------------+----------------------------+
| WebSocket URI:          | ws(s)://host:8083/mqtt     |
+-------------------------+----------------------------+
| Sec-WebSocket-Protocol: | 'mqttv3.1' or 'mqttv3.1.1' |
+-------------------------+----------------------------+

Dashboard插件提供了一个MQTT WebSocket连接的测试页面::

    http://127.0.0.1:18083/websocket.html

emqttd通过内嵌的HTTP服务器，实现MQTT WebSocket与HTTP发布接口，etc/emqttd.config设置::

    %% HTTP and WebSocket Listener
    {http, 8083, [
        %% Size of acceptor pool
        {acceptors, 4},
        %% Maximum number of concurrent clients
        {max_clients, 64},
        %% Socket Access Control
        {access, [{allow, all}]},
        %% Socket Options
        {sockopts, [
            {backlog, 1024}
            %{buffer, 4096},
        ]}
    ]}

.. _sys_topic:

-------------
$SYS-系统主题
-------------

emqttd消息服务器周期性发布自身运行状态、MQTT协议统计、客户端上下线状态到'$SYS/'开头系统主题。

$SYS主题路径以"$SYS/brokers/{node}/"开头，'${node}'是Erlang节点名称::

    $SYS/brokers/emqttd@127.0.0.1/version

    $SYS/brokers/emqttd@host2/uptime

.. NOTE:: 默认只允许localhost的MQTT客户端订阅$SYS主题，可通过etc/acl.config修改访问控制规则。

$SYS系统消息发布周期，通过etc/emqttd.config配置::

    {broker, [
        %% System interval of publishing broker $SYS messages
        {sys_interval, 60},

.. _sys_brokers:

服务器版本、启动时间与描述消息
------------------------------

+--------------------------------+-----------------------+
| 主题                           | 说明                  |
+================================+=======================+
| $SYS/brokers                   | 集群节点列表          |
+--------------------------------+-----------------------+
| $SYS/brokers/${node}/version   | emqttd版本            |
+--------------------------------+-----------------------+
| $SYS/brokers/${node}/uptime    | emqttd启动时间        |
+--------------------------------+-----------------------+
| $SYS/brokers/${node}/datetime  | emqttd服务器时间      |
+--------------------------------+-----------------------+
| $SYS/brokers/${node}/sysdescr  | emqttd描述            |
+--------------------------------+-----------------------+

.. _sys_clients:

MQTT客户端上下线状态消息
------------------------

$SYS主题前缀: $SYS/brokers/${node}/clients/

+--------------------------+--------------------------------------------+------------------------------------+
| 主题(Topic)              | 数据(JSON)                                 | 说明                               |
+==========================+============================================+====================================+
| ${clientid}/connected    | {ipaddress: "127.0.0.1", username: "test", | Publish when a client connected    |
|                          |  session: false, version: 3, connack: 0,   |                                    |
|                          |  ts: 1432648482}                           |                                    |
+--------------------------+--------------------------------------------+------------------------------------+
| ${clientid}/disconnected | {reason: "keepalive_timeout",              | Publish when a client disconnected |
|                          |  ts: 1432749431}                           |                                    |
+--------------------------+--------------------------------------------+------------------------------------+

'connected'消息JSON数据::

    {
        ipaddress: "127.0.0.1",
        username:  "test",
        session:   false,
        protocol:  3,
        connack:   0,
        ts:        1432648482
    }

'disconnected'消息JSON数据::

    {
        reason: normal,
        ts:     1432648486
    }

.. _sys_stats:

Statistics - 系统统计消息
--------------------------

系统主题前缀: $SYS/brokers/${node}/stats/

Clients - 客户端统计
....................

+---------------------+---------------------------------------------+
| 主题(Topic)         | 说明                                        |
+---------------------+---------------------------------------------+
| clients/count       | 当前客户端总数                              |
+---------------------+---------------------------------------------+
| clients/max         | 最大客户端数量                              |
+---------------------+---------------------------------------------+

Sessions - 会话统计
...................

+---------------------+---------------------------------------------+
| 主题(Topic)         | 说明                                        |
+---------------------+---------------------------------------------+
| sessions/count      | 当前会话总数                                |
+---------------------+---------------------------------------------+
| sessions/max        | 最大会话数量                                |
+---------------------+---------------------------------------------+

Subscriptions - 订阅统计
........................

+---------------------+---------------------------------------------+
| 主题(Topic)         | 说明                                        |
+---------------------+---------------------------------------------+
| subscriptions/count | 当前订阅总数                                |
+---------------------+---------------------------------------------+
| subscriptions/max   | 最大订阅数量                                |
+---------------------+---------------------------------------------+

Topics - 主题统计
................

+---------------------+---------------------------------------------+
| 主题(Topic)         | 说明                                        |
+---------------------+---------------------------------------------+
| topics/count        | 当前Topic总数(跨节点)                       |
+---------------------+---------------------------------------------+
| topics/max          | Max number of topics                        |
+---------------------+---------------------------------------------+

Metrics-收发流量/报文/消息统计
------------------------------

系统主题(Topic)前缀: $SYS/brokers/${node}/metrics/

收发流量统计
............

+---------------------+---------------------------------------------+
| 主题(Topic)         | 说明                                        |
+---------------------+---------------------------------------------+
| bytes/received      | 累计接收流量                                |
+---------------------+---------------------------------------------+
| bytes/sent          | 累计发送流量                                |
+---------------------+---------------------------------------------+

MQTT报文收发统计
................

+--------------------------+---------------------------------------------+
| 主题(Topic)              | 说明                                        |
+--------------------------+---------------------------------------------+
| packets/received         | 累计接收MQTT报文                            |
+--------------------------+---------------------------------------------+
| packets/sent             | 累计发送MQTT报文                            |
+--------------------------+---------------------------------------------+
| packets/connect          | 累计接收MQTT CONNECT报文                    |
+--------------------------+---------------------------------------------+
| packets/connack          | 累计发送MQTT CONNACK报文                    |
+--------------------------+---------------------------------------------+
| packets/publish/received | 累计接收MQTT PUBLISH报文                    |
+--------------------------+---------------------------------------------+
| packets/publish/sent     | 累计发送MQTT PUBLISH报文                    |
+--------------------------+---------------------------------------------+
| packets/subscribe        | 累计接收MQTT SUBSCRIBE报文                  |
+--------------------------+---------------------------------------------+
| packets/suback           | 累计发送MQTT SUBACK报文                     |
+--------------------------+---------------------------------------------+
| packets/unsubscribe      | 累计接收MQTT UNSUBSCRIBE报文                |
+--------------------------+---------------------------------------------+
| packets/unsuback         | 累计发送MQTT UNSUBACK报文                   |
+--------------------------+---------------------------------------------+
| packets/pingreq          | 累计接收MQTT PINGREQ报文                    |
+--------------------------+---------------------------------------------+
| packets/pingresp         | 累计发送MQTT PINGRESP报文数量               |
+--------------------------+---------------------------------------------+
| packets/disconnect       | 累计接收MQTT DISCONNECT数量                 |
+--------------------------+---------------------------------------------+

MQTT消息收发统计
................

+--------------------------+---------------------------------------------+
| 主题(Topic)              | 说明                                        |
+--------------------------+---------------------------------------------+
| messages/received        | 累计接收消息                                |
+--------------------------+---------------------------------------------+
| messages/sent            | 累计发送消息                                |
+--------------------------+---------------------------------------------+
| messages/retained        | Retained消息总数                            |
+--------------------------+---------------------------------------------+
| messages/dropped         | 丢弃消息总数                                |
+--------------------------+---------------------------------------------+

.. _sys_alarms:

Alarms-系统告警
---------------

系统主题(Topic)前缀: $SYS/brokers/${node}/alarms/

+------------------+------------------+
| 主题(Topic)      | 说明             |
+------------------+------------------+
| ${alarmId}/alert | 新产生告警       |
+------------------+------------------+
| ${alarmId}/clear | 清除告警         |
+------------------+------------------+

.. _sys_sysmon:

Sysmon-系统监控
---------------

系统主题(Topic)前缀: $SYS/brokers/${node}/sysmon/

+------------------+--------------------+
| 主题(Topic)      | 说明               |
+------------------+--------------------+
| long_gc          | GC时间过长警告     |
+------------------+--------------------+
| long_schedule    | 调度时间过长警告   |
+------------------+--------------------+
| large_heap       | Heap内存占用警告   |
+------------------+--------------------+
| busy_port        | Port忙警告         |
+------------------+--------------------+
| busy_dist_port   | Dist Port忙警告    |
+------------------+--------------------+

.. _trace:

----
追踪
----

emqttd消息服务器支持追踪来自某个客户端(Client)的全部报文，或者发布到某个主题(Topic)的全部消息。

追踪客户端(Client)::

    ./bin/emqttd_ctl trace client "clientid" "trace_clientid.log"

追踪主题(Topic)::

    ./bin/emqttd_ctl trace topic "topic" "trace_topic.log"

查询追踪::

    ./bin/emqttd_ctl trace list

停止追踪::

    ./bin/emqttd_ctl trace client "clientid" off

    ./bin/emqttd_ctl trace topic "topic" off


.. _emqttd_auth_http:     https://github.com/emqtt/emqttd_auth_http
.. _emqttd_plugin_mysql:  https://github.com/emqtt/emqttd_plugin_mysql
.. _emqttd_plugin_pgsql:  https://github.com/emqtt/emqttd_plugin_pgsql
.. _emqttd_plugin_redis:  https://github.com/emqtt/emqttd_plugin_redis
.. _emqttd_plugin_mongo:  https://github.com/emqtt/emqttd_plugin_mongo

