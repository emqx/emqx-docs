
.. _upgrade:

==================
版本升级 (Upgrade)
==================

.. _upgrade_3.1:

-------------
升级到3.1版本
-------------

.. NOTE:: 3.1版本全新设计了项目架构、配置方式与插件管理方式。2.x与1.x版本升级需要重新配置部署。

升级流程:

1. 下载解压3.1版本到新安装目录，例如 /opt/emqx-3.1/；

2. 参考旧版本 etc/vm.args、etc/emqttd.config 或 etc/emq.conf，配置3.1版本的 etc/emqx.conf；

3. 重新配置插件 etc/plugins/${your-plugin}.conf；

4. 编辑插件加载文件 data/loaded_plugins；

5. 停止旧版本，启动新版。

.. _upgrade_2.0.3:

------------------
2.0升级到2.0.3版本
------------------

升级流程:

1. 下载解压2.0.3版本到新安装目录，例如 /opt/emqttd-2.0.3/；

2. 旧版本的 'etc/' 配置文件、'data/' 数据文件覆盖到新版目录；

3. 停止旧版本，启动新版。

.. _upgrade_2.0:

-------------
升级到2.0版本
-------------

.. NOTE:: 2.0版本全新设计了项目架构、配置方式与插件管理方式。1.x版本升级需要重新配置部署。

升级流程:

1. 下载解压2.0版本到新安装目录，例如 /opt/emqttd-2.0/

2. 参考旧版本 etc/vm.args、etc/emqttd.config，配置2.0版本的 etc/emq.conf

3. 重新配置插件 etc/plugins/${your-plugin}.conf

4. 编辑插件加载文件 data/loaded_plugins

5. 停止旧版本，启动新版。

.. _upgrade_1.1.2:

---------------
升级到1.1.2版本
---------------

.. NOTE:: 1.0以后版本可平滑升级到1.1.2

升级流程:

1. 下载解压1.1.2版本到新安装目录，例如 /opt/emqttd_112；

2. 旧版本的 'etc/' 配置文件、'data/' 数据文件覆盖到新版目录；

3. 如果有加载插件，将旧版插件配置文件覆盖到新版；

4. 停止旧版本，启动新版。

========
迁移指南
========

下文提供了一套从 **EMQ X 3.x** 版本迁移到最新 **EMQ X 4.0** 版本的准则。尽管我们试图减少一些重大更改，但为了兼顾性能、简化使用方式，我们在几个地方进行了修改。

**EMQ X 3.x 版本迁移 EMQ X 4.0 要花多长时间？**

EMQ X 始终保证接入协议的规范性和持续更新，版本迁移时客户端部分 **无需做任何调整**，这意味着您无需停止设备功能、重新烧录设备程序固件。您仅需关注插件、配置项、命令行以及 REST API 的变更。

所需时长取决于您的项目规模和变更涉及范围，中小型的项目基本一天内就可以搞定。

----
核心
----

client_id 改为 clientid
>>>>>>>>>>>>>>>>>>>>>>>>

在此处变量命名上我们做了较大的变动，EMQ X 内部所有 client_id 字符都更改为 clientid，包括：

- REST API 的 URL、请求/相应数据中的字段名称
- 源代码中的命名规范
- 命令行 CLI

--------
REST API
--------

v3 改为 v4
>>>>>>>>>>

REST API 由 ``http(s)://host:8081/api/v3/`` 变更为 ``http(s)://host:8081/api/v4/``。

连接 (connection) 改为客户端 (clients)
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

将 `connections` 概念改为 `clients`，涉及节点与集群相关的 API：

- 获取集群连接列表：``GET /connections`` -> ``/clients``
- 获取集群指定连接信息：``GET /connections/:clientid`` -> ``GET /connections/:clientid``
- 获取节点连接列表：``GET /nodes/:node/connections`` -> ``GET /nodes/:node/clients``
- 获取节点指定连接信息：``GET /nodes/:node/connections/:clientid`` -> ``GET /nodes/:node/clients/:clientid``
- 请求/相应数据中的 client_id 字段名称均变为 clientid

同时 API 返回内容有较大变动，变动部分详见 4.0 文档。

移除会话 (session) 相关的 API
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

4.0 中引入 Channel 概念，将会话 (session) 和客户端 (client) 合二为一，4.0 版本中以下 API 已被 **移除**：

- 获取集群会话列表：``GET /sessions``
- 获取集群指定客户端会话信息：``GET /sessions/:clientid``
- 获取节点会话列表：``GET /nodes/:node/sessions``
- 获取节点指定客户端会话信息：``GET /nodes/:node/sessions/:clientid``

4.0 以后如需获取会话相关信息，请使用客户端相关 API。

移除插件配置获取与更改 API
>>>>>>>>>>>>>>>>>>>>>>>>>>

插件配置中可能包含敏感信息，同时插件配置不支持持久化为用户使用带来了很大疑惑。考虑到安全问题与实用性问题，我们 **移除** 了插件获取与更改 API。

- 获取插件配置信息：``GET /nodes/:node/plugins/:plugin_name``
- 更新插件配置：``PUT /nodes/:node/plugins/:plugin_name``

我们计划在 **企业版** 中通过安全规范及配置项本地存储提供解决以上问题，重新提供插件热配置相关的 API 以，**目前企业版本已经支持关键配置的热配置操作**。

---------
Dashboard
---------

连接 (connection) 改为客户端 (clients)
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Dashboard 中 **连接 (connections)** 概念改为 **客户端 (clients)**，原连接信息可在现 **客户端 (clients)** 页面查看。

移除 **会话 (sessions)** 管理页面
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Dashboard 中移除 **会话 (sessions)** 管理页面，相关信息整合到 **客户端 (clients)** 页面中。

规则引擎
>>>>>>>>

规则引擎 SQL 语法有所变动，规则创建时 Dashboard 中不再提供 **事件** 下拉选择框，SQL 语法详细变更参照本文 **规则引擎** 部分。

--------
规则引擎
--------

SQL 语法变更
>>>>>>>>>>>>

4.0 版本中规则引擎 SQL 语法更加易用，3.x 版本中所有事件 **FROM** 子句后面均需要指定事件名称，4.0 以后我们引入 **事件主题** 概念，默认情况下 **消息发布** 事件不再需要指定事件名称：

.. code-block::

    ## 3.x 版本
    ## 需要指定事件名称进行处理
    SELECT * FROM "message.publish" WHERE topic =~ 't/#'

    ## 4.0 及以后版本
    ## 默认处理 message.publish 事件, FROM 后面直接筛选 MQTT 主题
    ## 上述 SQL 语句等价于:
    SELECT * FROM 't/#'

    ## 其他事件通过 事件主题 进行筛选
    SELECT * FROM "$events/message_acked" where topic =~ 't/#'
    SELECT * FROM "$events/client_connected"

Dashboard 中提供了旧版 SQL 语法转换功能可以完成 SQL 升级迁移。

事件名称变更
>>>>>>>>>>>>

4.0 版本中 **订阅/取消订阅** 主体变为 **会话 (session)**，**事件** 在转换为 **事件主题** 时，需要注意以下变更：

- **终端订阅** 变更为 **会话订阅**：``client.subscribe`` -> ``$events/session_subscribe``
- **终端取消订阅** 变更为 **会话取消订阅**：``client.unsubscribe`` -> ``$events/session_unsubscribe``
