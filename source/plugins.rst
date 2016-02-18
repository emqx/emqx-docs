=================
扩展插件(Plugins)
=================

eMQTT消息服务器通过钩子(Hooks)与发布订阅(PubSub)机制，支持快速方便的扩展插件开发。

eMQTT项目开发维护的插件包括:

+------------------------+---------------------------+
| 插件                   | 说明                      |
+========================+===========================+
| emqttd_dashboard       | Dashboard插件             |
+------------------------+---------------------------+
| emqttd_plugin_mysql    | MySQL认证/访问控制        |
+------------------------+---------------------------+
| emqttd_plugin_pgsql    | PostgreSQL认证/访问控制   |
+------------------------+---------------------------+
| emqttd_plugin_redis    | Redis认证/访问控制        |
+------------------------+---------------------------+
| emqttd_stomp           | Stomp协议支持             |
+------------------------+---------------------------+
| emqttd_sockjs          | Stomp over SockJS协议支持 |
+------------------------+---------------------------+
| emqttd_recon           | Recon性能调试             |
+------------------------+---------------------------+
| emqttd_plugin_template | 插件开发模版              |
+------------------------+---------------------------+

-------------------------------
emqttd_dashboard: Dashboard插件
-------------------------------

eMQTT消息服务器的Web管理控制台。插件项目地址: https://github.com/emqtt/emqttd_dashboard

eMQTT消息服务器默认加载Dashboard插件。URL地址: http://localhost:18083 ，缺省用户名/密码: admin/public 。

Dashboard插件可查询eMQTT消息服务基本信息、统计数据、度量数据，查询系统全部客户端连接(Client)、会话(Session)、主题(Topic)、订阅(Subscription)。

.. image:: _static/images/dashboard.png

Dashboard插件设置
-----------------

plugins/emqttd_dashboard/etc/plugin.config::

    [
      {emqttd_dashboard, [
        {default_admin, [
          {login, "admin"},
          {password, "public"}
        ]},
        {listener,
          {emqttd_dashboard, 18083, [
            {acceptors, 4},
            {max_clients, 512}]}
        }
      ]}
    ].

-------------------------------------------
emqttd_plugin_mysql: MySQL认证/访问控制插件
-------------------------------------------

MySQL用户表/ACL表认证鉴权MQTT客户端。

MySQL库表
--------

MQTT用户表::

    CREATE TABLE `mqtt_user` (
      `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
      `username` varchar(100) DEFAULT NULL,
      `password` varchar(100) DEFAULT NULL,
      `salt` varchar(20) DEFAULT NULL,
      `created` datetime DEFAULT NULL,
      PRIMARY KEY (`id`),
      UNIQUE KEY `mqtt_username` (`username`)
    ) ENGINE=MyISAM DEFAULT CHARSET=utf8;

.. NOTE:: This is a demo table. You could authenticate with any user table.

MQTT访问控制表::

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


配置插件
--------

plugins/emqttd_plugin_mysql/etc/plugin.config::

    [

    {emqttd_plugin_mysql, [

        {mysql_pool, [
            %% ecpool options
            {pool_size, 4},
            {pool_type, random},
            {auto_reconnect, 3},

            %% mysql options
            {host,     "localhost"},
            {port,     3306},
            {user,     ""},
            {password, ""},
            {database, "mqtt"},
            {encoding, utf8}
        ]},

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

        %% comment this query, the acl will be disabled
        {aclquery, "select * from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'"},

        %% If no rules matched, return...
        {acl_nomatch, allow}
    ]}
    ].

加载插件
--------

./bin/emqttd_ctl plugins load emqttd_plugin_mysql



------------------------------------------------
emqttd_plugin_pgsql: PostgreSQL认证/访问控制插件
------------------------------------------------

PgSQL库表
---------


Notice: This is a demo table. You could authenticate with any user table.

MQTT用户表::

    CREATE TABLE mqtt_user (
      id SERIAL primary key,
      username character varying(100),
      password character varying(100),
      salt character varying(40)
    ) 

MQTT访问控制表::

    CREATE TABLE mqtt_acl (
      id SERIAL primary key,
      allow integer,
      ipaddr character varying(60),
      username character varying(100),
      clientid character varying(100),
      access  integer,
      topic character varying(100)
    ) 

    INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)
    VALUES
        (1,1,NULL,'$all',NULL,2,'#'),
        (2,0,NULL,'$all',NULL,1,'$SYS/#'),
        (3,0,NULL,'$all',NULL,1,'eq #'),
        (5,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),
        (6,1,'127.0.0.1',NULL,NULL,2,'#'),
        (7,1,NULL,'dashboard',NULL,1,'$SYS/#');


配置插件
--------

plugins/emqttd_plugin_pgsql/etc/plugin.config::

    [

      {emqttd_plugin_pgsql, [

        {pgsql_pool, [
          %% ecpool options
          {pool_size, 4},
          {auto_reconnect, 3},

          %% pgsql options
          {host, "localhost"},
          {port, 5432},
          {username, "feng"},
          {password, ""},
          {database, "mqtt"},
          {encoding,  utf8}
        ]},

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

        %% Comment this query, the acl will be disabled. Notice: don't edit this query!
        {aclquery, "select allow, ipaddr, username, clientid, access, topic from mqtt_acl
                     where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'"},

        %% If no rules matched, return...
        {acl_nomatch, allow}
      ]}
    ].


加载插件
--------

.. code:: shell

    ./bin/emqttd_ctl plugins load emqttd_plugin_pgsql

--------------------------------------------
emqttd_plugin_redis: Redis认证/访问控制插件
--------------------------------------------



--------------------------------
emqttd_plugin_template: 插件模版
--------------------------------

-----------------------------
emqttd_stomp: Stomp协议插件
-----------------------------

The plugin adds STOMP 1.0/1.1/1.2 protocol support to the emqttd broker.

The STOMP clients could pub/sub with the MQTT topics, and talk with the MQTT clients!

Build
-----

    Git submodule the plugin to 'emqttd/plugins' folder.

    ```
    git submodule add https://github.com/emqtt/emqttd_stomp.git plugins/emqttd_stomp

    make && make dist

    ```

Configure
----------

**The default port is 61613.**

Plugin config file: etc/plugin.config.

    ```erlang
    [
      {emqttd_stomp, [

        {default_user, [
            {login,    "guest"},
            {passcode, "guest"}
        ]},

        {allow_anonymous, true},

        %%TODO: unused...
        {frame, [
          {max_headers,       10},
          {max_header_length, 1024},
          {max_body_length,   8192}
        ]},

        {listeners, [
          {emqttd_stomp, 61613, [
            {acceptors,   4},
            {max_clients, 512}
          ]}
        ]}

      ]}
    ].
    ```

Load
----

    ./bin/emqttd_ctl plugins load emqttd_stomp


--------------------------------
emqttd_sockjs: Stomp/Sockjs插件
--------------------------------

Build
------

Notice that the emqttd_stomp plugin is required.

Git submodule to emqttd/plugins folder after the emqttd_stomp plugin installed.

git submodule add https://github.com/emqtt/emqttd_sockjs.git plugins/emqttd_sockjs

make && make dist

Configure
----------

Default port is 61616::

    [
      {emqttd_sockjs, [

        {sockjs, []},

        {cowboy_listener, {stomp_sockjs, 61616, 4}},

      ]}
    ].

Load

    ./bin/emqttd_ctl plugins load emqttd_stomp

    ./bin/emqttd_ctl plugins load emqttd_sockjs

Demo

    http://localhost:61616/index.html


--------------------------------
emqttd_recon: Recon性能调试插件  
--------------------------------


Recon debug/optimize plugin

## Load

```
./bin/emqttd_ctl plugins load emqttd_recon
```

## Commands

```
./bin/emqttd_ctl recon

recon memory                            #recon_alloc:memory/2
recon allocated                         #recon_alloc:memory(allocated_types, current|max)
recon bin_leak                          #recon:bin_leak(100)
recon node_stats                        #recon:node_stats(10, 1000)
recon remote_load Mod                   #recon:remote_load(Mod)
```

------------------------------------
emqttd_plugin_template: 插件开发模版
------------------------------------

注册认证/访问控制模块
-----------------------------

注册扩展钩子(Hooks)
-----------------------------


