
.. _rest_api:

======================
管理监控API (REST API)
======================

用户可以通过 REST API 查询 MQTT 客户端连接(Clients)、会话(Sessions)、订阅(Subscriptions)和路由(Routes)信息，还可以检索和监控服务器的性能指标和统计数据。

----------
URL 地址
---------

REST API 访问URL地址::

    http(s)://host:8080/api/v3/

-----------
Basic 认证
-----------

REST API 采用 HTTP Basic 认证(Authentication), 使用 Dashboard 中创建的 AppID 和 AppSecret 进行认证:

.. code-block:: bash

    curl -v --basic -u <appid>:<appsecret> -k http://localhost:8080/api/v3/brokers

----------
API 信息
----------

获取当前 REST API 列表
----------------------



API 定义::

    GET api/v3/


请求示例::

    GET api/v3/


返回数据:

.. code-block:: json

    [
      {
            "name": "list_brokers",
            "method": "GET",
            "path": "/brokers/",
            "descr": "A list of brokers in the cluster"
      },
      {
            "name": "list_connections",
            "method": "GET",
            "path": "/connections/",
            "descr": "A list of connections in the cluster"
      },
      {
            "name": "list_node_connections",
            "method": "GET",
            "path": "nodes/:node/connections/",
            "descr": "A list of connections on a node"
      },
      {
            "name": "list_listeners",
            "method": "GET",
            "path": "/listeners/",
            "descr": "A list of listeners in the cluster"
      },
      {
            "name": "list_node_listeners",
            "method": "GET",
            "path": "/nodes/:node/listeners",
            "descr": "A list of listeners on the node"
      },
      {
            "name": "list_node_metrics",
            "method": "GET",
            "path": "/nodes/:node/metrics/",
            "descr": "A list of metrics of a node"
      },
      {
            "name": "list_all_metrics",
            "method": "GET",
            "path": "/metrics/",
            "descr": "A list of metrics of all nodes in the cluster"
      },
      {
            "name": "list_nodes",
            "method": "GET",
            "path": "/nodes/",
            "descr": "A list of nodes in the cluster"
      },
      {
            "name": "list_sessions",
            "method": "GET",
            "path": "/sessions/",
            "descr": "A list of sessions in the cluster"
      },
      {
            "name": "list_node_sessions",
            "method": "GET",
            "path": "nodes/:node/sessions/",
            "descr": "A list of sessions on a node"
      },
      {
            "name": "lookup_node_stats",
            "method": "GET",
            "path": "/nodes/:node/stats/",
            "descr": "A list of stats of a node"
      },
      {
            "name": "list_stats",
            "method": "GET",
            "path": "/stats/",
            "descr": "A list of stats of all nodes in the cluster"
      },
      {
            "name": "list_subscriptions",
            "method": "GET",
            "path": "/subscriptions/",
            "descr": "A list of subscriptions in the cluster"
      },
      {
            "name": "lookup_client_subscriptions",
            "method": "GET",
            "path": "/subscriptions/:clientid",
            "descr": "A list of subscriptions of a client"
      },
      {
            "name": "lookup_client_subscriptions_with_node",
            "method": "GET",
            "path": "/nodes/:node/subscriptions/:clientid",
            "descr": "A list of subscriptions of a client on the node"
      },
      {
            "name": "list_node_subscriptions",
            "method": "GET",
            "path": "/nodes/:node/subscriptions/",
            "descr": "A list of subscriptions on a node"
      },
      {
            "name": "add_app",
            "method": "POST",
            "path": "/apps/",
            "descr": "Add Application"
      },
      {
            "name": "auth_user",
            "method": "POST",
            "path": "/auth",
            "descr": "Authenticate an user"
      },
      {
            "name": "change_pwd",
            "method": "PUT",
            "path": "/change_pwd/:username",
            "descr": "Change password for an user"
      },
      {
            "name": "clean_acl_cache",
            "method": "DELETE",
            "path": "/connections/:clientid/acl/:topic",
            "descr": "Clean ACL cache of a connection"
      },
      {
            "name": "create_user",
            "method": "POST",
            "path": "/users/",
            "descr": "Create an user"
      },
      {
            "name": "create_banned",
            "method": "POST",
            "path": "/banned/",
            "descr": "Create banned"
      },
      {
            "name": "del_app",
            "method": "DELETE",
            "path": "/apps/:appid",
            "descr": "Delete Application"
      },
      {
            "name": "delete_user",
            "method": "DELETE",
            "path": "/users/:name",
            "descr": "Delete an user"
      },
      {
            "name": "delete_banned",
            "method": "DELETE",
            "path": "/banned/:who",
            "descr": "Delete banned"
      },
      {
            "name": "get_all_configs",
            "method": "GET",
            "path": "/configs/",
            "descr": "Get all configs"
      },
      {
            "name": "get_all_configs",
            "method": "GET",
            "path": "/nodes/:node/configs/",
            "descr": "Get all configs of a node"
      },
      {
            "name": "get_broker",
            "method": "GET",
            "path": "/brokers/:node",
            "descr": "Get broker info of a node"
      },
      {
            "name": "get_plugin_configs",
            "method": "GET",
            "path": "/nodes/:node/plugin_configs/:plugin",
            "descr": "Get configurations of a plugin on the node"
      },
      {
            "name": "kickout_connection",
            "method": "DELETE",
            "path": "/connections/:clientid",
            "descr": "Kick out a connection"
      },
      {
            "name": "list_apps",
            "method": "GET",
            "path": "/apps/",
            "descr": "List Applications"
      },
      {
            "name": "list_node_alarms",
            "method": "GET",
            "path": "/alarms/:node",
            "descr": "List alarms of a node"
      },
      {
            "name": "list_all_alarms",
            "method": "GET",
            "path": "/alarms/",
            "descr": "List all alarms"
      },
      {
            "name": "list_all_plugins",
            "method": "GET",
            "path": "/plugins/",
            "descr": "List all plugins in the cluster"
      },
      {
            "name": "list_node_plugins",
            "method": "GET",
            "path": "/nodes/:node/plugins/",
            "descr": "List all plugins on a node"
      },
      {
            "name": "list_banned",
            "method": "GET",
            "path": "/banned/",
            "descr": "List banned"
      },
      {
            "name": "list_routes",
            "method": "GET",
            "path": "/routes/",
            "descr": "List routes"
      },
      {
            "name": "list_users",
            "method": "GET",
            "path": "/users/",
            "descr": "List users"
      },
      {
            "name": "load_plugin",
            "method": "PUT",
            "path": "/nodes/:node/plugins/:plugin/load",
            "descr": "Load a plugin"
      },
      {
            "name": "lookup_app",
            "method": "GET",
            "path": "/apps/:appid",
            "descr": "Lookup Application"
      },
      {
            "name": "lookup_connections",
            "method": "GET",
            "path": "/connections/:clientid",
            "descr": "Lookup a connection in the cluster"
      },
      {
            "name": "lookup_node_connections",
            "method": "GET",
            "path": "nodes/:node/connections/:clientid",
            "descr": "Lookup a connection on node"
      },
      {
            "name": "get_node",
            "method": "GET",
            "path": "/nodes/:node",
            "descr": "Lookup a node in the cluster"
      },
      {
            "name": "lookup_session",
            "method": "GET",
            "path": "/sessions/:clientid",
            "descr": "Lookup a session in the cluster"
      },
      {
            "name": "lookup_node_session",
            "method": "GET",
            "path": "nodes/:node/sessions/:clientid",
            "descr": "Lookup a session on the node"
      },
      {
            "name": "lookup_routes",
            "method": "GET",
            "path": "/routes/:topic",
            "descr": "Lookup routes to a topic"
      },
      {
            "name": "mqtt_publish",
            "method": "POST",
            "path": "/mqtt/publish",
            "descr": "Publish a MQTT message"
      },
      {
            "name": "mqtt_subscribe",
            "method": "POST",
            "path": "/mqtt/subscribe",
            "descr": "Subscribe a topic"
      },
      {
            "name": "unload_plugin",
            "method": "PUT",
            "path": "/nodes/:node/plugins/:plugin/unload",
            "descr": "Unload a plugin"
      },
      {
            "name": "mqtt_unsubscribe",
            "method": "POST",
            "path": "/mqtt/unsubscribe",
            "descr": "Unsubscribe a topic"
      },
      {
            "name": "update_app",
            "method": "PUT",
            "path": "/apps/:appid",
            "descr": "Update Application"
      },
      {
            "name": "update_user",
            "method": "PUT",
            "path": "/users/:name",
            "descr": "Update an user"
      },
      {
            "name": "update_config",
            "method": "PUT",
            "path": "/configs/:app",
            "descr": "Update config of an application in the cluster"
      },
      {
            "name": "update_node_config",
            "method": "PUT",
            "path": "/nodes/:node/configs/:app",
            "descr": "Update config of an application on a node"
      },
      {
            "name": "update_plugin_configs",
            "method": "PUT",
            "path": "/nodes/:node/plugin_configs/:plugin",
            "descr": "Update configurations of a plugin on the node"
      }
    ]





----------
集群与节点
----------

获取集群基本信息
----------------------



API 定义::

    GET api/v3/brokers/


请求示例::

    GET api/v3/brokers/


返回数据:

.. code-block:: json

    [
      {
            "datetime": "2018-09-12 10:42:57",
            "node": "emqx@127.0.0.1",
            "node_status": "Running",
            "otp_release": "R21/10.0.5",
            "sysdescr": "EMQ X Broker",
            "uptime": "1 days,18 hours, 45 minutes, 1 seconds",
            "version": "3.0"
      }
    ]





获取节点基本信息
----------------------



API 定义::

    GET api/v3/brokers/${node}


请求示例::

    GET api/v3/brokers/emqx@127.0.0.1


返回数据:

.. code-block:: json

    {
      "datetime": "2018-09-12 10:42:57",
      "node_status": "Running",
      "otp_release": "R21/10.0.5",
      "sysdescr": "EMQ X Broker",
      "uptime": "1 days,18 hours, 45 minutes, 1 seconds",
      "version": "3.0"
    }




获取集群监控数据
----------------------



API 定义::

    GET api/v3/nodes/


请求示例::

    GET api/v3/nodes/


返回数据:

.. code-block:: json

    [
      {
            "connections": 2,
            "load1": "2.75",
            "load15": "2.87",
            "load5": "2.57",
            "max_fds": 7168,
            "memory_total": "76.45M",
            "memory_used": "59.48M",
            "name": "emqx@127.0.0.1",
            "node": "emqx@127.0.0.1",
            "node_status": "Running",
            "otp_release": "R21/10.0.5",
            "process_available": 262144,
            "process_used": 331,
            "uptime": "1 days,18 hours, 45 minutes, 1 seconds",
            "version": "3.0"
      }
    ]




获取节点监控数据
----------------------



API 定义::

    GET api/v3/nodes/${node}


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1


返回数据:

.. code-block:: json

    {
      "connections": 2,
      "load1": "2.75",
      "load15": "2.87",
      "load5": "2.57",
      "max_fds": 7168,
      "memory_total": 80162816,
      "memory_used": 62254160,
      "name": "emqx@127.0.0.1",
      "node_status": "Running",
      "otp_release": "R21/10.0.5",
      "process_available": 262144,
      "process_used": 331,
      "uptime": "1 days,18 hours, 45 minutes, 1 seconds",
      "version": "3.0"
    }




-------------------
连接信息(Connections)
-------------------

获取集群连接信息
----------------------------



API 定义::

    GET api/v3/connections/


请求示例::

    GET api/v3/connections/?_page=1&_limit=10000


返回数据:

.. code-block:: json

    {
      "items": [
            {
                  "clean_start": true,
                  "client_id": "emqx-api-test:v1",
                  "connected_at": "2018-09-12 10:42:57",
                  "ipaddress": "127.0.0.1",
                  "is_bridge": false,
                  "is_super": false,
                  "keepalive": 60,
                  "mountpoint": "undefined",
                  "node": "emqx@127.0.0.1",
                  "peercert": "nossl",
                  "port": 64594,
                  "proto_name": "MQTT",
                  "proto_ver": 4,
                  "username": "emqx-api-test:v1",
                  "will_topic": "undefined",
                  "zone": "external"
            },
            {
                  "clean_start": true,
                  "client_id": "mqttjs_406e3f9a",
                  "connected_at": "2018-09-12 10:42:57",
                  "ipaddress": "127.0.0.1",
                  "is_bridge": false,
                  "is_super": false,
                  "keepalive": 60,
                  "mountpoint": "undefined",
                  "node": "emqx@127.0.0.1",
                  "peercert": "nossl",
                  "port": 64593,
                  "proto_name": "MQTT",
                  "proto_ver": 4,
                  "username": "undefined",
                  "will_topic": "undefined",
                  "zone": "external"
            }
      ],
      "meta": {
            "count": 2,
            "limit": 10000,
            "page": 1
      }
    }





获取节点连接信息
----------------------------



API 定义::

    GET api/v3/nodes/${node}/connections/


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1/connections/?_page=1&_limit=10000


返回数据:

.. code-block:: json

    {
      "items": [
            {
                  "clean_start": true,
                  "client_id": "emqx-api-test:v1",
                  "connected_at": "2018-09-12 10:42:57",
                  "ipaddress": "127.0.0.1",
                  "is_bridge": false,
                  "is_super": false,
                  "keepalive": 60,
                  "mountpoint": "undefined",
                  "node": "emqx@127.0.0.1",
                  "peercert": "nossl",
                  "port": 64594,
                  "proto_name": "MQTT",
                  "proto_ver": 4,
                  "username": "emqx-api-test:v1",
                  "will_topic": "undefined",
                  "zone": "external"
            },
            {
                  "clean_start": true,
                  "client_id": "mqttjs_406e3f9a",
                  "connected_at": "2018-09-12 10:42:57",
                  "ipaddress": "127.0.0.1",
                  "is_bridge": false,
                  "is_super": false,
                  "keepalive": 60,
                  "mountpoint": "undefined",
                  "node": "emqx@127.0.0.1",
                  "peercert": "nossl",
                  "port": 64593,
                  "proto_name": "MQTT",
                  "proto_ver": 4,
                  "username": "undefined",
                  "will_topic": "undefined",
                  "zone": "external"
            }
      ],
      "meta": {
            "count": 2,
            "limit": 10000,
            "page": 1
      }
    }






获取集群指定连接信息
--------------------------



API 定义::

    GET api/v3/connections/${clientid}


请求示例::

    GET api/v3/connections/emqx-api-test:v1


返回数据:

.. code-block:: json

    [
      {
            "clean_start": true,
            "client_id": "emqx-api-test:v1",
            "connected_at": "2018-09-12 10:42:57",
            "ipaddress": "127.0.0.1",
            "is_bridge": false,
            "is_super": false,
            "keepalive": 60,
            "mountpoint": "undefined",
            "node": "emqx@127.0.0.1",
            "peercert": "nossl",
            "port": 64594,
            "proto_name": "MQTT",
            "proto_ver": 4,
            "username": "emqx-api-test:v1",
            "will_topic": "undefined",
            "zone": "external"
      }
    ]





获取节点指定连接信息
----------------------------



API 定义::

    GET api/v3/nodes/${node}/connections/${clientid}


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1/connections/emqx-api-test:v1


返回数据:

.. code-block:: json

    [
      {
            "clean_start": true,
            "client_id": "emqx-api-test:v1",
            "connected_at": "2018-09-12 10:42:57",
            "ipaddress": "127.0.0.1",
            "is_bridge": false,
            "is_super": false,
            "keepalive": 60,
            "mountpoint": "undefined",
            "node": "emqx@127.0.0.1",
            "peercert": "nossl",
            "port": 64594,
            "proto_name": "MQTT",
            "proto_ver": 4,
            "username": "emqx-api-test:v1",
            "will_topic": "undefined",
            "zone": "external"
      }
    ]






断开指定连接
------------------



API 定义::

    DELETE api/v3/connections/${clientid}


请求示例::

    DELETE api/v3/connections/emqx-api-test:v1


返回数据:

.. code-block:: json

"ok"






--------------
会话(Sessions)
--------------

获取集群会话信息
------------------------------



API 定义::

    GET api/v3/sessions/


请求示例::

    GET api/v3/sessions/?_page=1&_limit=10000


返回数据:

.. code-block:: json

    {
      "items": [
            {
                  "awaiting_rel_len": 0,
                  "binding": "local",
                  "clean_start": true,
                  "client_id": "emqx-api-test:v1",
                  "created_at": "2018-09-12 10:42:57",
                  "deliver_msg": 0,
                  "enqueue_msg": 0,
                  "expiry_interval": 7200,
                  "heap_size": 376,
                  "inflight_len": 0,
                  "mailbox_len": 0,
                  "max_awaiting_rel": 100,
                  "max_inflight": 32,
                  "max_mqueue": 1000,
                  "max_subscriptions": 0,
                  "mqueue_dropped": 0,
                  "mqueue_len": 0,
                  "node": "emqx@127.0.0.1",
                  "reductions": 203,
                  "subscriptions_count": 0,
                  "username": "emqx-api-test:v1"
            },
            {
                  "awaiting_rel_len": 0,
                  "binding": "local",
                  "clean_start": true,
                  "client_id": "mqttjs_406e3f9a",
                  "created_at": "2018-09-12 10:42:57",
                  "deliver_msg": 0,
                  "enqueue_msg": 0,
                  "expiry_interval": 7200,
                  "heap_size": 233,
                  "inflight_len": 0,
                  "mailbox_len": 0,
                  "max_awaiting_rel": 100,
                  "max_inflight": 32,
                  "max_mqueue": 1000,
                  "max_subscriptions": 0,
                  "mqueue_dropped": 0,
                  "mqueue_len": 0,
                  "node": "emqx@127.0.0.1",
                  "reductions": 188,
                  "subscriptions_count": 0,
                  "username": "undefined"
            }
      ],
      "meta": {
            "count": 2,
            "limit": 10000,
            "page": 1
      }
    }





获取集群指定会话信息
------------------------------



API 定义::

    GET api/v3/sessions/${clientid}


请求示例::

    GET api/v3/sessions/emqx-api-test:v1


返回数据:

.. code-block:: json

    [
      {
            "awaiting_rel_len": 0,
            "binding": "local",
            "clean_start": true,
            "client_id": "emqx-api-test:v1",
            "created_at": "2018-09-12 10:42:57",
            "deliver_msg": 0,
            "enqueue_msg": 0,
            "expiry_interval": 7200,
            "heap_size": 376,
            "inflight_len": 0,
            "mailbox_len": 0,
            "max_awaiting_rel": 100,
            "max_inflight": 32,
            "max_mqueue": 1000,
            "max_subscriptions": 0,
            "mqueue_dropped": 0,
            "mqueue_len": 0,
            "node": "emqx@127.0.0.1",
            "reductions": 203,
            "subscriptions_count": 0,
            "username": "emqx-api-test:v1"
      }
    ]





获取节点会话信息
----------------------



API 定义::

    GET api/v3/nodes/${node}/sessions/


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1/sessions/?_page=1&_limit=10000


返回数据:

.. code-block:: json

    {
      "items": [
            {
                  "awaiting_rel_len": 0,
                  "binding": "local",
                  "clean_start": true,
                  "client_id": "emqx-api-test:v1",
                  "created_at": "2018-09-12 10:42:57",
                  "deliver_msg": 0,
                  "enqueue_msg": 0,
                  "expiry_interval": 7200,
                  "heap_size": 376,
                  "inflight_len": 0,
                  "mailbox_len": 0,
                  "max_awaiting_rel": 100,
                  "max_inflight": 32,
                  "max_mqueue": 1000,
                  "max_subscriptions": 0,
                  "mqueue_dropped": 0,
                  "mqueue_len": 0,
                  "node": "emqx@127.0.0.1",
                  "reductions": 203,
                  "subscriptions_count": 0,
                  "username": "emqx-api-test:v1"
            },
            {
                  "awaiting_rel_len": 0,
                  "binding": "local",
                  "clean_start": true,
                  "client_id": "mqttjs_406e3f9a",
                  "created_at": "2018-09-12 10:42:57",
                  "deliver_msg": 0,
                  "enqueue_msg": 0,
                  "expiry_interval": 7200,
                  "heap_size": 233,
                  "inflight_len": 0,
                  "mailbox_len": 0,
                  "max_awaiting_rel": 100,
                  "max_inflight": 32,
                  "max_mqueue": 1000,
                  "max_subscriptions": 0,
                  "mqueue_dropped": 0,
                  "mqueue_len": 0,
                  "node": "emqx@127.0.0.1",
                  "reductions": 188,
                  "subscriptions_count": 0,
                  "username": "undefined"
            }
      ],
      "meta": {
            "count": 2,
            "limit": 10000,
            "page": 1
      }
    }






获取节点指定会话信息
------------------------------



API 定义::

    GET api/v3/nodes/${node}/sessions/${clientid}


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1/sessions/emqx-api-test:v1


    请求参数 json:
    {
	      "topic" : "/World",
        "payload": "hello",
	      "qos": 0,
	      "retain" : false,
    	  "client_id": "mqttjs_722b4d845f"
    }

返回数据:

.. code-block:: json

    [
      {
            "awaiting_rel_len": 0,
            "binding": "local",
            "clean_start": true,
            "client_id": "emqx-api-test:v1",
            "created_at": "2018-09-12 10:42:57",
            "deliver_msg": 0,
            "enqueue_msg": 0,
            "expiry_interval": 7200,
            "heap_size": 376,
            "inflight_len": 0,
            "mailbox_len": 0,
            "max_awaiting_rel": 100,
            "max_inflight": 32,
            "max_mqueue": 1000,
            "max_subscriptions": 0,
            "mqueue_dropped": 0,
            "mqueue_len": 0,
            "node": "emqx@127.0.0.1",
            "reductions": 203,
            "subscriptions_count": 0,
            "username": "emqx-api-test:v1"
      }
    ]







-------------------
订阅(Subscriptions)
-------------------


获取集群订阅信息
------------------------



API 定义::

    GET api/v3/subscriptions/


请求示例::

    GET api/v3/subscriptions/?_page=1&_limit=10000


返回数据:

.. code-block:: json

    {
      "items": [
            {
                  "client_id": "emqx-api-test:v1",
                  "node": "emqx@127.0.0.1",
                  "qos": 0,
                  "topic": "/test"
            },
            {
                  "client_id": "mqttjs_406e3f9a",
                  "node": "emqx@127.0.0.1",
                  "qos": 0,
                  "topic": "/test"
            }
      ],
      "meta": {
            "count": 2,
            "limit": 10000,
            "page": 1
      }
    }





获取集群指定连接订阅信息
------------------------



API 定义::

    GET api/v3/subscriptions/${clientid}


请求示例::

    GET api/v3/subscriptions/emqx-api-test:v1


返回数据:

.. code-block:: json

    [
      {
            "client_id": "emqx-api-test:v1",
            "node": "emqx@127.0.0.1",
            "qos": 0,
            "topic": "/test"
      }
    ]





获取节点订阅信息
------------------------------



API 定义::

    GET api/v3/nodes/${node}/subscriptions/


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1/subscriptions/?_page=1&_limit=10000


返回数据:

.. code-block:: json

    {
      "items": [
            {
                  "client_id": "emqx-api-test:v1",
                  "node": "emqx@127.0.0.1",
                  "qos": 0,
                  "topic": "/test"
            },
            {
                  "client_id": "mqttjs_406e3f9a",
                  "node": "emqx@127.0.0.1",
                  "qos": 0,
                  "topic": "/test"
            }
      ],
      "meta": {
            "count": 2,
            "limit": 10000,
            "page": 1
      }
    }




获取节点指定连接订阅信息
------------------------------


API 定义::

    GET api/v3/nodes/${node}/subscriptions/${clientid}


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1/subscriptions/emqx-api-test:v1


返回数据:

.. code-block:: json

    [
      {
            "client_id": "emqx-api-test:v1",
            "node": "emqx@127.0.0.1",
            "qos": 0,
            "topic": "/test"
      }
    ]




------------
路由(Routes)
------------

获取集群路由表
--------------



API 定义::

    GET api/v3/nodes/


请求示例::

    GET api/v3/nodes/


返回数据:

.. code-block:: json

    [
      {
            "connections": 2,
            "load1": "2.75",
            "load15": "2.87",
            "load5": "2.57",
            "max_fds": 7168,
            "memory_total": "76.45M",
            "memory_used": "59.48M",
            "name": "emqx@127.0.0.1",
            "node": "emqx@127.0.0.1",
            "node_status": "Running",
            "otp_release": "R21/10.0.5",
            "process_available": 262144,
            "process_used": 331,
            "uptime": "1 days,18 hours, 45 minutes, 1 seconds",
            "version": "3.0"
      }
    ]





获取集群指定主题的路由信息
----------------------------



API 定义::

    GET api/v3/routes/${topic}


请求示例::

    GET api/v3/routes//test


返回数据:

.. code-block:: json

    []






------------------
发布/订阅/取消订阅
------------------

发布消息
--------



API 定义::

    POST api/v3/mqtt/publish

请求参数:

.. code-block:: json

    {
      "topic": "test_topic",
      "payload": "hello",
      "qos": 1,
      "retain": false,
      "client_id": "mqttjs_ab9069449e"
    }

      

请求示例::

    POST api/v3/mqtt/publish


返回数据:

.. code-block:: json

    {
      "code": 0
    }





创建订阅
--------



API 定义::

    POST api/v3/mqtt/subscribe

请求参数:

.. code-block:: json

    {
      "topic": "test_topic",
      "qos": 1,
      "client_id": "mqttjs_ab9069449e"
    }

      

请求示例::

    POST api/v3/mqtt/subscribe


返回数据:

.. code-block:: json

    {
      "code": 112
    }





取消订阅
--------



API 定义::

    POST api/v3/mqtt/unsubscribe

请求参数:

.. code-block:: json

    {
      "topic": "test_topic",
      "payload": "hello",
      "qos": 1,
      "retain": false,
      "client_id": "mqttjs_ab9069449e"
    }

      

请求示例::

    POST api/v3/mqtt/unsubscribe


返回数据:

.. code-block:: json

    {
      "code": 112
    }




-------------
插件(Plugins)
-------------

获取集群插件列表
------------------



API 定义::

    GET api/v3/plugins/


请求示例::

    GET api/v3/plugins/


返回数据:

.. code-block:: json

    [
      {
            "node": "emqx@127.0.0.1",
            "plugins": [
                  {
                        "name": "emqx_auth_clientid",
                        "version": "3.0",
                        "description": "EMQ X Authentication with ClientId/Password",
                        "active": false
                  },
                  {
                        "name": "emqx_auth_http",
                        "version": "3.0",
                        "description": "EMQ X Authentication/ACL with HTTP API",
                        "active": false
                  },
                  {
                        "name": "emqx_auth_jwt",
                        "version": "3.0",
                        "description": "EMQ X Authentication with JWT",
                        "active": false
                  },
                  {
                        "name": "emqx_auth_ldap",
                        "version": "3.0",
                        "description": "EMQ X Authentication/ACL with LDAP",
                        "active": false
                  },
                  {
                        "name": "emqx_auth_mongo",
                        "version": "3.0",
                        "description": "EMQ X Authentication/ACL with MongoDB",
                        "active": false
                  },
                  {
                        "name": "emqx_auth_mysql",
                        "version": "3.0",
                        "description": "EMQ X Authentication/ACL with MySQL",
                        "active": false
                  },
                  {
                        "name": "emqx_auth_pgsql",
                        "version": "3.0",
                        "description": "EMQ X Authentication/ACL with PostgreSQL",
                        "active": false
                  },
                  {
                        "name": "emqx_auth_redis",
                        "version": "3.0",
                        "description": "EMQ X Authentication/ACL with Redis",
                        "active": false
                  },
                  {
                        "name": "emqx_auth_username",
                        "version": "3.0",
                        "description": "EMQ X Authentication with Username/Password",
                        "active": false
                  },
                  {
                        "name": "emqx_coap",
                        "version": "3.0",
                        "description": "EMQ X CoAP Gateway",
                        "active": false
                  },
                  {
                        "name": "emqx_dashboard",
                        "version": "3.0",
                        "description": "EMQ X Web Dashboard",
                        "active": true
                  },
                  {
                        "name": "emqx_delayed_publish",
                        "version": "3.0",
                        "description": "EMQ X Delayed Publish",
                        "active": true
                  },
                  {
                        "name": "emqx_lwm2m",
                        "version": "3.0",
                        "description": "EMQ X LwM2M Gateway",
                        "active": false
                  },
                  {
                        "name": "emqx_management",
                        "version": "3.0",
                        "description": "EMQ X Management API and CLI",
                        "active": true
                  },
                  {
                        "name": "emqx_plugin_template",
                        "version": "3.0",
                        "description": "EMQ X Plugin Template",
                        "active": false
                  },
                  {
                        "name": "emqx_recon",
                        "version": "3.0",
                        "description": "EMQ X Recon Plugin",
                        "active": true
                  },
                  {
                        "name": "emqx_reloader",
                        "version": "3.0",
                        "description": "EMQ X Reloader Plugin",
                        "active": false
                  },
                  {
                        "name": "emqx_retainer",
                        "version": "3.0",
                        "description": "EMQ X Retainer",
                        "active": true
                  },
                  {
                        "name": "emqx_sn",
                        "version": "3.0",
                        "description": "EMQ X MQTT-SN Gateway",
                        "active": false
                  },
                  {
                        "name": "emqx_statsd",
                        "version": "3.0",
                        "description": "Statsd for EMQ X",
                        "active": false
                  },
                  {
                        "name": "emqx_stomp",
                        "version": "3.0",
                        "description": "EMQ X Stomp Protocol Plugin",
                        "active": false
                  },
                  {
                        "name": "emqx_web_hook",
                        "version": "3.0",
                        "description": "EMQ X Webhook Plugin",
                        "active": false
                  }
            ]
      }
    ]





获取节点插件列表
------------------



API 定义::

    GET api/v3/nodes/${node}/plugins/


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1/plugins/


返回数据:

.. code-block:: json

    [
      {
            "name": "emqx_auth_clientid",
            "version": "3.0",
            "description": "EMQ X Authentication with ClientId/Password",
            "active": false
      },
      {
            "name": "emqx_auth_http",
            "version": "3.0",
            "description": "EMQ X Authentication/ACL with HTTP API",
            "active": false
      },
      {
            "name": "emqx_auth_jwt",
            "version": "3.0",
            "description": "EMQ X Authentication with JWT",
            "active": false
      },
      {
            "name": "emqx_auth_ldap",
            "version": "3.0",
            "description": "EMQ X Authentication/ACL with LDAP",
            "active": false
      },
      {
            "name": "emqx_auth_mongo",
            "version": "3.0",
            "description": "EMQ X Authentication/ACL with MongoDB",
            "active": false
      },
      {
            "name": "emqx_auth_mysql",
            "version": "3.0",
            "description": "EMQ X Authentication/ACL with MySQL",
            "active": false
      },
      {
            "name": "emqx_auth_pgsql",
            "version": "3.0",
            "description": "EMQ X Authentication/ACL with PostgreSQL",
            "active": false
      },
      {
            "name": "emqx_auth_redis",
            "version": "3.0",
            "description": "EMQ X Authentication/ACL with Redis",
            "active": false
      },
      {
            "name": "emqx_auth_username",
            "version": "3.0",
            "description": "EMQ X Authentication with Username/Password",
            "active": false
      },
      {
            "name": "emqx_coap",
            "version": "3.0",
            "description": "EMQ X CoAP Gateway",
            "active": false
      },
      {
            "name": "emqx_dashboard",
            "version": "3.0",
            "description": "EMQ X Web Dashboard",
            "active": true
      },
      {
            "name": "emqx_delayed_publish",
            "version": "3.0",
            "description": "EMQ X Delayed Publish",
            "active": true
      },
      {
            "name": "emqx_lwm2m",
            "version": "3.0",
            "description": "EMQ X LwM2M Gateway",
            "active": false
      },
      {
            "name": "emqx_management",
            "version": "3.0",
            "description": "EMQ X Management API and CLI",
            "active": true
      },
      {
            "name": "emqx_plugin_template",
            "version": "3.0",
            "description": "EMQ X Plugin Template",
            "active": false
      },
      {
            "name": "emqx_recon",
            "version": "3.0",
            "description": "EMQ X Recon Plugin",
            "active": true
      },
      {
            "name": "emqx_reloader",
            "version": "3.0",
            "description": "EMQ X Reloader Plugin",
            "active": false
      },
      {
            "name": "emqx_retainer",
            "version": "3.0",
            "description": "EMQ X Retainer",
            "active": true
      },
      {
            "name": "emqx_sn",
            "version": "3.0",
            "description": "EMQ X MQTT-SN Gateway",
            "active": false
      },
      {
            "name": "emqx_statsd",
            "version": "3.0",
            "description": "Statsd for EMQ X",
            "active": false
      },
      {
            "name": "emqx_stomp",
            "version": "3.0",
            "description": "EMQ X Stomp Protocol Plugin",
            "active": false
      },
      {
            "name": "emqx_web_hook",
            "version": "3.0",
            "description": "EMQ X Webhook Plugin",
            "active": false
      }
    ]





启用节点指定插件
-----------------------



API 定义::

    PUT api/v3/nodes/${node}/plugins/${plugin}/load


请求示例::

    PUT api/v3/nodes/emqx@127.0.0.1/plugins/emqx_auth_clientid/load


返回数据:

.. code-block:: json

"ok"





关闭节点指定插件
-----------------------



API 定义::

    PUT api/v3/nodes/${node}/plugins/${plugin}/unload


请求示例::

    PUT api/v3/nodes/emqx@127.0.0.1/plugins/emqx_auth_clientid/unload


返回数据:

.. code-block:: json

"ok"





------------------
监听器(Listeners)
------------------

获取集群监听器列表
------------------------



API 定义::

    GET api/v3/listeners/


请求示例::

    GET api/v3/listeners/


返回数据:

.. code-block:: json

    [
      {
            "listeners": [
                  {
                        "acceptors": 16,
                        "current_conns": 0,
                        "listen_on": "8883",
                        "max_conns": 102400,
                        "protocol": "mqtt:ssl",
                        "shutdown_count": []
                  },
                  {
                        "acceptors": 8,
                        "current_conns": 2,
                        "listen_on": "0.0.0.0:1883",
                        "max_conns": 1024000,
                        "protocol": "mqtt:tcp",
                        "shutdown_count": {
                              "closed": 2,
                              "kicked": 1
                        }
                  },
                  {
                        "acceptors": 4,
                        "current_conns": 0,
                        "listen_on": "127.0.0.1:11883",
                        "max_conns": 10240000,
                        "protocol": "mqtt:tcp",
                        "shutdown_count": []
                  },
                  {
                        "acceptors": 4,
                        "current_conns": 1,
                        "listen_on": "18083",
                        "max_conns": 512,
                        "protocol": "http:dashboard",
                        "shutdown_count": []
                  },
                  {
                        "acceptors": 2,
                        "current_conns": 0,
                        "listen_on": "8080",
                        "max_conns": 512,
                        "protocol": "http:management",
                        "shutdown_count": []
                  },
                  {
                        "acceptors": 4,
                        "current_conns": 0,
                        "listen_on": "8083",
                        "max_conns": 102400,
                        "protocol": "mqtt:ws",
                        "shutdown_count": []
                  },
                  {
                        "acceptors": 4,
                        "current_conns": 0,
                        "listen_on": "8084",
                        "max_conns": 16,
                        "protocol": "mqtt:wss",
                        "shutdown_count": []
                  }
            ],
            "node": "emqx@127.0.0.1"
      }
    ]





获取节点监听器列表
------------------------



API 定义::

    GET api/v3/nodes/${node}/listeners


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1/listeners


返回数据:

.. code-block:: json

    [
      {
            "acceptors": 16,
            "current_conns": 0,
            "listen_on": "8883",
            "max_conns": 102400,
            "protocol": "mqtt:ssl",
            "shutdown_count": []
      },
      {
            "acceptors": 8,
            "current_conns": 2,
            "listen_on": "0.0.0.0:1883",
            "max_conns": 1024000,
            "protocol": "mqtt:tcp",
            "shutdown_count": {
                  "closed": 2,
                  "kicked": 1
            }
      },
      {
            "acceptors": 4,
            "current_conns": 0,
            "listen_on": "127.0.0.1:11883",
            "max_conns": 10240000,
            "protocol": "mqtt:tcp",
            "shutdown_count": []
      },
      {
            "acceptors": 4,
            "current_conns": 1,
            "listen_on": "18083",
            "max_conns": 512,
            "protocol": "http:dashboard",
            "shutdown_count": []
      },
      {
            "acceptors": 2,
            "current_conns": 0,
            "listen_on": "8080",
            "max_conns": 512,
            "protocol": "http:management",
            "shutdown_count": []
      },
      {
            "acceptors": 4,
            "current_conns": 0,
            "listen_on": "8083",
            "max_conns": 102400,
            "protocol": "mqtt:ws",
            "shutdown_count": []
      },
      {
            "acceptors": 4,
            "current_conns": 0,
            "listen_on": "8084",
            "max_conns": 16,
            "protocol": "mqtt:wss",
            "shutdown_count": []
      }
    ]




------------
收发报文统计
------------

获取集群收发报文统计
--------------------------



API 定义::

    GET api/v3/metrics/


请求示例::

    GET api/v3/metrics/


返回数据:

.. code-block:: json

    [
      {
            "node": "emqx@127.0.0.1",
            "metrics": {
                  "bytes/received": 342,
                  "packets/pubrel/sent": 0,
                  "packets/pubcomp/missed": 0,
                  "packets/sent": 13,
                  "packets/pubrel/received": 0,
                  "messages/qos1/received": 0,
                  "packets/publish/received": 2,
                  "packets/auth": 0,
                  "messages/qos0/received": 2,
                  "packets/pubcomp/received": 0,
                  "packets/unsuback": 0,
                  "packets/pubrec/missed": 0,
                  "messages/qos1/sent": 0,
                  "messages/qos2/sent": 0,
                  "bytes/sent": 116,
                  "messages/received": 2,
                  "messages/dropped": 1,
                  "messages/qos2/received": 0,
                  "packets/connect": 5,
                  "messages/qos0/sent": 4,
                  "packets/disconnect/received": 0,
                  "packets/pubrec/sent": 0,
                  "packets/publish/sent": 4,
                  "packets/pubrec/received": 0,
                  "packets/received": 11,
                  "packets/unsubscribe": 0,
                  "packets/subscribe": 4,
                  "packets/disconnect/sent": 0,
                  "packets/pingresp": 0,
                  "messages/qos2/dropped": 0,
                  "packets/puback/missed": 0,
                  "packets/pingreq": 0,
                  "packets/connack": 5,
                  "packets/pubrel/missed": 0,
                  "messages/sent": 4,
                  "packets/suback": 4,
                  "messages/retained": 3,
                  "packets/puback/sent": 0,
                  "packets/puback/received": 0,
                  "messages/qos2/expired": 0,
                  "messages/forward": 0,
                  "messages/expired": 0,
                  "packets/pubcomp/sent": 0
            }
      }
    ]




获取节点收发报文统计
--------------------------



API 定义::

    GET api/v3/nodes/${node}/metrics/


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1/metrics/


返回数据:

.. code-block:: json

    {
      "bytes/received": 342,
      "packets/pubrel/sent": 0,
      "packets/pubcomp/missed": 0,
      "packets/sent": 13,
      "packets/pubrel/received": 0,
      "messages/qos1/received": 0,
      "packets/publish/received": 2,
      "packets/auth": 0,
      "messages/qos0/received": 2,
      "packets/pubcomp/received": 0,
      "packets/unsuback": 0,
      "packets/pubrec/missed": 0,
      "messages/qos1/sent": 0,
      "messages/qos2/sent": 0,
      "bytes/sent": 116,
      "messages/received": 2,
      "messages/dropped": 1,
      "messages/qos2/received": 0,
      "packets/connect": 5,
      "messages/qos0/sent": 4,
      "packets/disconnect/received": 0,
      "packets/pubrec/sent": 0,
      "packets/publish/sent": 4,
      "packets/pubrec/received": 0,
      "packets/received": 11,
      "packets/unsubscribe": 0,
      "packets/subscribe": 4,
      "packets/disconnect/sent": 0,
      "packets/pingresp": 0,
      "messages/qos2/dropped": 0,
      "packets/puback/missed": 0,
      "packets/pingreq": 0,
      "packets/connack": 5,
      "packets/pubrel/missed": 0,
      "messages/sent": 4,
      "packets/suback": 4,
      "messages/retained": 3,
      "packets/puback/sent": 0,
      "packets/puback/received": 0,
      "messages/qos2/expired": 0,
      "messages/forward": 0,
      "messages/expired": 0,
      "packets/pubcomp/sent": 0
    }





-------------
连接会话统计
-------------

获取集群连接会话统计
---------------------------



API 定义::

    GET api/v3/stats/


请求示例::

    GET api/v3/stats/


返回数据:

.. code-block:: json

    [
      {
            "node": "emqx@127.0.0.1",
            "subscriptions/shared/max": 0,
            "subscriptions/max": 2,
            "subscribers/max": 2,
            "topics/count": 0,
            "subscriptions/count": 0,
            "topics/max": 1,
            "sessions/persistent/max": 2,
            "connections/max": 2,
            "subscriptions/shared/count": 0,
            "sessions/persistent/count": 0,
            "retained/count": 3,
            "routes/count": 0,
            "sessions/count": 0,
            "retained/max": 3,
            "sessions/max": 2,
            "routes/max": 1,
            "subscribers/count": 0,
            "connections/count": 0
      }
    ]




获取节点连接会话统计
--------------------------



API 定义::

    GET api/v3/nodes/${node}/stats/


请求示例::

    GET api/v3/nodes/emqx@127.0.0.1/stats/


返回数据:

.. code-block:: json

    {
      "subscriptions/shared/max": 0,
      "subscriptions/max": 2,
      "subscribers/max": 2,
      "topics/count": 0,
      "subscriptions/count": 0,
      "topics/max": 1,
      "sessions/persistent/max": 2,
      "connections/max": 2,
      "subscriptions/shared/count": 0,
      "sessions/persistent/count": 0,
      "retained/count": 3,
      "routes/count": 0,
      "sessions/count": 0,
      "retained/max": 3,
      "sessions/max": 2,
      "routes/max": 1,
      "subscribers/count": 0,
      "connections/count": 0
    }






----------
告警信息
----------

获取集群告警信息
---------------------------



API 定义::

    GET api/v3/alarms/${node}


请求示例::

    GET api/v3/alarms/emqx@127.0.0.1


返回数据:

.. code-block:: json

    []




获取节点告警信息
--------------------------



API 定义::

    GET api/v3/alarms/


请求示例::

    GET api/v3/alarms/


返回数据:

.. code-block:: json

    [
      {
            "alarms": [],
            "node": "emqx@127.0.0.1"
      }
    ]






----------
黑名单
----------

获取黑名单列表
---------------------------



API 定义::

    GET api/v3/banned/


请求示例::

    GET api/v3/banned/?_page=1&_limit=10000


返回数据:

.. code-block:: json

    {
      "items": [],
      "meta": {
            "count": 0,
            "limit": 10000,
            "page": 1
      }
    }




创建黑名单
--------------------------



API 定义::

    POST api/v3/banned/

请求参数:

.. code-block:: json

    {
      "who": "clientId/username/ipAddress",
      "as": "client_id",
      "reason": "banned the clientId",
      "desc": "normal banned",
      "until": 1536146187
    }

      

请求示例::

    POST api/v3/banned/


返回数据:

.. code-block:: json

    {
      "who": "clientId/username/ipAddress",
      "as": "client_id",
      "reason": "banned the clientId",
      "desc": "normal banned",
      "until": 1536146187
    }




删除指定黑名单
--------------------------



API 定义::

    DELETE api/v3/banned/${who}?as=${as}


请求示例::

    DELETE api/v3/banned/${who}?as=${as}


返回数据:

.. code-block:: json

"ok"






----------------
错误信息与数据分页
----------------

HTTP状态码大于500时响应携带错误信息返回
---------------------------------

错误示例::

    PUT api/v3/nodes/emqx@127.0.0.1/plugins/emqx_recon/load

返回数据:

.. code-block:: json

    {
        "message": "already_started"
    }


分页参数与分页信息
---------------

请求示例中使用了 ?_page=1&_limit=10000 参数的接口均支持分页::

    _page: 当前页码
    _limit: 分页大小


返回数据:

.. code-block:: json    

    {
      "items": [],
      "meta": {
          "page": 1,
          "limit": 10000,
          "count": 2
      }
    }

