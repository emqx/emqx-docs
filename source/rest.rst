
.. _rest_api:

======================
管理监控API (REST API)
======================

用户可以通过 REST API 查询 MQTT 客户端连接(Connections)、会话(Sessions)、订阅(Subscriptions)和路由(Routes)信息，还可以检索和监控服务器的性能指标和统计数据。

---------
URL 地址
---------

REST API 访问URL地址::

    http(s)://host:8081/api/v4/

-----------
Basic 认证
-----------

使用 REST API 必须携带 HTTP Basic 格式的认证(Authentication)信息。因此，需要使用 Dashboard 中来创建的 AppID 和 AppSecret 进行认证:

.. code-block:: bash

    # 例如：获取当前集群状态
    curl -v --basic -u <appid>:<appsecret> -k http://localhost:8081/api/v4/brokers

----------
API 信息
----------

获取当前 REST API 列表
----------------------

API 定义::

    GET api/v4/

请求示例::

    GET api/v4/

返回数据:

.. code:: json

  {
    "code": 0,
    "data": [
      {
        "name": "list_clientid",
        "method": "GET",
        "path": "/auth_clientid",
        "descr": "List available clientid in the cluster"
      },
      {
        "name": "lookup_clientid",
        "method": "GET",
        "path": "/auth_clientid/:clientid",
        "descr": "Lookup clientid in the cluster"
      },
      {
        "name": "add_clientid",
        "method": "POST",
        "path": "/auth_clientid",
        "descr": "Add clientid in the cluster"
      },
      {
        "name": "update_clientid",
        "method": "PUT",
        "path": "/auth_clientid/:clientid",
        "descr": "Update clientid in the cluster"
      },
      {
        "name": "delete_clientid",
        "method": "DELETE",
        "path": "/auth_clientid/:clientid",
        "descr": "Delete clientid in the cluster"
      },
      {
        "name": "list_username",
        "method": "GET",
        "path": "/auth_username",
        "descr": "List available username in the cluster"
      },
      {
        "name": "lookup_username",
        "method": "GET",
        "path": "/auth_username/:username",
        "descr": "Lookup username in the cluster"
      },
      {
        "name": "add_username",
        "method": "POST",
        "path": "/auth_username",
        "descr": "Add username in the cluster"
      },
      {
        "name": "update_username",
        "method": "PUT",
        "path": "/auth_username/:username",
        "descr": "Update username in the cluster"
      },
      {
        "name": "delete_username",
        "method": "DELETE",
        "path": "/auth_username/:username",
        "descr": "Delete username in the cluster"
      },
      {
        "name": "auth_user",
        "method": "POST",
        "path": "/auth",
        "descr": "Authenticate an user"
      },
      {
        "name": "create_user",
        "method": "POST",
        "path": "/users/",
        "descr": "Create an user"
      },
      {
        "name": "list_users",
        "method": "GET",
        "path": "/users/",
        "descr": "List users"
      },
      {
        "name": "update_user",
        "method": "PUT",
        "path": "/users/:name",
        "descr": "Update an user"
      },
      {
        "name": "delete_user",
        "method": "DELETE",
        "path": "/users/:name",
        "descr": "Delete an user"
      },
      {
        "name": "change_pwd",
        "method": "PUT",
        "path": "/change_pwd/:username",
        "descr": "Change password for an user"
      },
      {
        "name": "list_all_alarms",
        "method": "GET",
        "path": "/alarms/present",
        "descr": "List all alarms"
      },
      {
        "name": "list_node_alarms",
        "method": "GET",
        "path": "/alarms/present/:node",
        "descr": "List alarms of a node"
      },
      {
        "name": "list_all_alarm_history",
        "method": "GET",
        "path": "/alarms/history",
        "descr": "List all alarm history"
      },
      {
        "name": "list_node_alarm_history",
        "method": "GET",
        "path": "/alarms/history/:node",
        "descr": "List alarm history of a node"
      },
      {
        "name": "add_app",
        "method": "POST",
        "path": "/apps/",
        "descr": "Add Application"
      },
      {
        "name": "del_app",
        "method": "DELETE",
        "path": "/apps/:appid",
        "descr": "Delete Application"
      },
      {
        "name": "list_apps",
        "method": "GET",
        "path": "/apps/",
        "descr": "List Applications"
      },
      {
        "name": "lookup_app",
        "method": "GET",
        "path": "/apps/:appid",
        "descr": "Lookup Application"
      },
      {
        "name": "update_app",
        "method": "PUT",
        "path": "/apps/:appid",
        "descr": "Update Application"
      },
      {
        "name": "list_banned",
        "method": "GET",
        "path": "/banned/",
        "descr": "List banned"
      },
      {
        "name": "create_banned",
        "method": "POST",
        "path": "/banned/",
        "descr": "Create banned"
      },
      {
        "name": "delete_banned",
        "method": "DELETE",
        "path": "/banned/:who",
        "descr": "Delete banned"
      },
      {
        "name": "list_brokers",
        "method": "GET",
        "path": "/brokers/",
        "descr": "A list of brokers in the cluster"
      },
      {
        "name": "get_broker",
        "method": "GET",
        "path": "/brokers/:node",
        "descr": "Get broker info of a node"
      },
      {
        "name": "list_clients",
        "method": "GET",
        "path": "/clients/",
        "descr": "A list of clients on current node"
      },
      {
        "name": "list_node_clients",
        "method": "GET",
        "path": "nodes/:node/clients/",
        "descr": "A list of clients on specified node"
      },
      {
        "name": "lookup_client",
        "method": "GET",
        "path": "/clients/:clientid",
        "descr": "Lookup a client in the cluster"
      },
      {
        "name": "lookup_node_client",
        "method": "GET",
        "path": "nodes/:node/clients/:clientid",
        "descr": "Lookup a client on the node"
      },
      {
        "name": "lookup_client_via_username",
        "method": "GET",
        "path": "/clients/username/:username",
        "descr": "Lookup a client via username in the cluster"
      },
      {
        "name": "lookup_node_client_via_username",
        "method": "GET",
        "path": "/nodes/:node/clients/username/:username",
        "descr": "Lookup a client via username on the node "
      },
      {
        "name": "kickout_client",
        "method": "DELETE",
        "path": "/clients/:clientid",
        "descr": "Kick out the client in the cluster"
      },
      {
        "name": "clean_acl_cache",
        "method": "DELETE",
        "path": "/clients/:clientid/acl_cache",
        "descr": "Clear the ACL cache of a specified client in the cluster"
      },
      {
        "name": "list_acl_cache",
        "method": "GET",
        "path": "/clients/:clientid/acl_cache",
        "descr": "List the ACL cache of a specified client in the cluster"
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
        "name": "list_all_metrics",
        "method": "GET",
        "path": "/metrics/",
        "descr": "A list of metrics of all nodes in the cluster"
      },
      {
        "name": "list_node_metrics",
        "method": "GET",
        "path": "/nodes/:node/metrics/",
        "descr": "A list of metrics of a node"
      },
      {
        "name": "list_nodes",
        "method": "GET",
        "path": "/nodes/",
        "descr": "A list of nodes in the cluster"
      },
      {
        "name": "get_node",
        "method": "GET",
        "path": "/nodes/:node",
        "descr": "Lookup a node in the cluster"
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
        "name": "load_node_plugin",
        "method": "PUT",
        "path": "/nodes/:node/plugins/:plugin/load",
        "descr": "Load a plugin"
      },
      {
        "name": "unload_node_plugin",
        "method": "PUT",
        "path": "/nodes/:node/plugins/:plugin/unload",
        "descr": "Unload a plugin"
      },
      {
        "name": "reload_node_plugin",
        "method": "PUT",
        "path": "/nodes/:node/plugins/:plugin/reload",
        "descr": "Reload a plugin"
      },
      {
        "name": "unload_plugin",
        "method": "PUT",
        "path": "/plugins/:plugin/unload",
        "descr": "Unload a plugin in the cluster"
      },
      {
        "name": "reload_plugin",
        "method": "PUT",
        "path": "/plugins/:plugin/reload",
        "descr": "Reload a plugin in the cluster"
      },
      {
        "name": "mqtt_subscribe",
        "method": "POST",
        "path": "/mqtt/subscribe",
        "descr": "Subscribe a topic"
      },
      {
        "name": "mqtt_publish",
        "method": "POST",
        "path": "/mqtt/publish",
        "descr": "Publish a MQTT message"
      },
      {
        "name": "mqtt_unsubscribe",
        "method": "POST",
        "path": "/mqtt/unsubscribe",
        "descr": "Unsubscribe a topic"
      },
      {
        "name": "mqtt_subscribe_batch",
        "method": "POST",
        "path": "/mqtt/subscribe_batch",
        "descr": "Batch subscribes topics"
      },
      {
        "name": "mqtt_publish_batch",
        "method": "POST",
        "path": "/mqtt/publish_batch",
        "descr": "Batch publish MQTT messages"
      },
      {
        "name": "mqtt_unsubscribe_batch",
        "method": "POST",
        "path": "/mqtt/unsubscribe_batch",
        "descr": "Batch unsubscribes topics"
      },
      {
        "name": "list_routes",
        "method": "GET",
        "path": "/routes/",
        "descr": "List routes"
      },
      {
        "name": "lookup_routes",
        "method": "GET",
        "path": "/routes/:topic",
        "descr": "Lookup routes to a topic"
      },
      {
        "name": "list_stats",
        "method": "GET",
        "path": "/stats/",
        "descr": "A list of stats of all nodes in the cluster"
      },
      {
        "name": "lookup_node_stats",
        "method": "GET",
        "path": "/nodes/:node/stats/",
        "descr": "A list of stats of a node"
      },
      {
        "name": "list_subscriptions",
        "method": "GET",
        "path": "/subscriptions/",
        "descr": "A list of subscriptions in the cluster"
      },
      {
        "name": "list_node_subscriptions",
        "method": "GET",
        "path": "/nodes/:node/subscriptions/",
        "descr": "A list of subscriptions on a node"
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
        "name": "create_rule",
        "method": "POST",
        "path": "/rules/",
        "descr": "Create a rule"
      },
      {
        "name": "list_rules",
        "method": "GET",
        "path": "/rules/",
        "descr": "A list of all rules"
      },
      {
        "name": "show_rule",
        "method": "GET",
        "path": "/rules/:id",
        "descr": "Show a rule"
      },
      {
        "name": "delete_rule",
        "method": "DELETE",
        "path": "/rules/:id",
        "descr": "Delete a rule"
      },
      {
        "name": "list_actions",
        "method": "GET",
        "path": "/actions/",
        "descr": "A list of all actions"
      },
      {
        "name": "show_action",
        "method": "GET",
        "path": "/actions/:name",
        "descr": "Show an action"
      },
      {
        "name": "list_resources",
        "method": "GET",
        "path": "/resources/",
        "descr": "A list of all resources"
      },
      {
        "name": "create_resource",
        "method": "POST",
        "path": "/resources/",
        "descr": "Create a resource"
      },
      {
        "name": "show_resource",
        "method": "GET",
        "path": "/resources/:id",
        "descr": "Show a resource"
      },
      {
        "name": "get_resource_status",
        "method": "GET",
        "path": "/resource_status/:id",
        "descr": "Get status of a resource"
      },
      {
        "name": "start_resource",
        "method": "POST",
        "path": "/resources/:id",
        "descr": "Start a resource"
      },
      {
        "name": "delete_resource",
        "method": "DELETE",
        "path": "/resources/:id",
        "descr": "Delete a resource"
      },
      {
        "name": "list_resource_types",
        "method": "GET",
        "path": "/resource_types/",
        "descr": "List all resource types"
      },
      {
        "name": "show_resource_type",
        "method": "GET",
        "path": "/resource_types/:name",
        "descr": "Show a resource type"
      },
      {
        "name": "list_resources_by_type",
        "method": "GET",
        "path": "/resource_types/:type/resources",
        "descr": "List all resources of a resource type"
      },
      {
        "name": "list_events",
        "method": "GET",
        "path": "/rule_events/",
        "descr": "List all events with detailed info"
      }
    ]
  }

----------
集群与节点
----------

获取集群基本信息
----------------

API 定义::

    GET api/v4/brokers/

请求示例::

    GET api/v4/brokers/

返回数据:

.. code:: json

    {
      "code": 0,
      "data": [
        {
          "datetime": "2019-12-18 10:56:41",
          "node": "emqx@127.0.0.1",
          "node_status": "Running",
          "otp_release": "R21/10.3.2",
          "sysdescr": "EMQ X Broker",
          "uptime": "3 minutes, 59 seconds",
          "version": "v4.0.0"
        }
      ]
    }

获取节点基本信息
----------------------

API 定义::

    GET api/v4/brokers/${node}

请求示例::

    GET api/v4/brokers/emqx@127.0.0.1

返回数据:

.. code:: json

  {
    "code": 0,
    "data": {
      "datetime": "2019-12-18 10:57:40",
      "node_status": "Running",
      "otp_release": "R21/10.3.2",
      "sysdescr": "EMQ X Broker",
      "uptime": "7 minutes, 16 seconds",
      "version": "v4.0.0"
    }
  }

获取集群监控数据
----------------------

API 定义::

    GET api/v4/nodes/

请求示例::

    GET api/v4/nodes/

返回数据:

.. code:: json

  {
    "code": 0,
    "data": [
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
        "otp_release": "R21/10.3.2",
        "process_available": 262144,
        "process_used": 331,
        "uptime": "1 days,18 hours, 45 minutes, 1 seconds",
        "version": "v4.0.0"
      }
    ]
  }

获取节点监控数据
----------------------

API 定义::

    GET api/v4/nodes/${node}

请求示例::

    GET api/v4/nodes/emqx@127.0.0.1

返回数据:

.. code:: json

  {
    "code": 0,
    "data": {
      "connections": 1,
      "load1": "2.75",
      "load15": "2.87",
      "load5": "2.57",
      "max_fds": 7168,
      "memory_total": 80162816,
      "memory_used": 62254160,
      "name": "emqx@127.0.0.1",
      "node_status": "Running",
      "otp_release": "R21/10.3.2",
      "process_available": 262144,
      "process_used": 331,
      "uptime": "1 days,18 hours, 45 minutes, 1 seconds",
      "version": "v4.0.0"
    }
  }

-------------------
客户端信息(Clients)
-------------------

获取集群客户端信息
------------------

API 定义::

    GET api/v4/clients

请求示例::

    GET api/v4/clients?_page=1&_limit=10000

返回数据:

.. code:: json

  {
    "code": 0,
    "data": [
      {
        "username": "test",
        "recv_cnt": 2,
        "node": "emqx@127.0.0.1",
        "proto_name": "MQTT",
        "mqueue_len": 0,
        "mailbox_len": 1,
        "ip_address": "127.0.0.1",
        "awaiting_rel": 0,
        "max_mqueue": 1000,
        "send_msg": 0,
        "heap_size": 2586,
        "clientid": "mosquitto_mqtt",
        "created_at": "2019-12-18 10:27:24",
        "is_bridge": false,
        "proto_ver": 4,
        "expiry_interval": 0,
        "reductions": 4751,
        "max_subscriptions": 0,
        "recv_pkt": 1,
        "subscriptions_cnt": 0,
        "send_cnt": 0,
        "connected_at": "2019-12-18 10:27:24",
        "recv_msg": 0,
        "max_inflight": 32,
        "keepalive": 60,
        "max_awaiting_rel": 100,
        "mqueue_dropped": 0,
        "recv_oct": 21,
        "zone": "external",
        "inflight": 0,
        "connected": true,
        "port": 65273,
        "send_oct": 0,
        "send_pkt": 0,
        "clean_start": true
      }
    ],
    "meta": {
      "page": 1,
      "limit": 10000,
      "count": 1
    }
  }

获取节点客户端信息
------------------

API 定义::

    GET api/v4/nodes/${node}/clients

请求示例::

    GET api/v4/nodes/emqx@127.0.0.1/clients?_page=1&_limit=10000

返回数据:

.. code:: json

  {
    "code": 0,
    "data": [
      {
        "username": "test",
        "recv_cnt": 10,
        "node": "emqx@127.0.0.1",
        "proto_name": "MQTT",
        "mqueue_len": 0,
        "mailbox_len": 0,
        "ip_address": "127.0.0.1",
        "awaiting_rel": 0,
        "max_mqueue": 1000,
        "send_msg": 0,
        "heap_size": 610,
        "clientid": "mosquitto_mqtt",
        "created_at": "2019-12-18 10:27:24",
        "is_bridge": false,
        "proto_ver": 4,
        "expiry_interval": 0,
        "reductions": 11292,
        "max_subscriptions": 0,
        "recv_pkt": 1,
        "subscriptions_cnt": 0,
        "send_cnt": 9,
        "connected_at": "2019-12-18 10:27:24",
        "recv_msg": 0,
        "max_inflight": 32,
        "keepalive": 60,
        "max_awaiting_rel": 100,
        "mqueue_dropped": 0,
        "recv_oct": 37,
        "zone": "external",
        "inflight": 0,
        "connected": true,
        "port": 65273,
        "send_oct": 20,
        "send_pkt": 9,
        "clean_start": true
      }
    ],
    "meta": {
      "page": 1,
      "limit": 10000,
      "count": 1
    }
  }

获取集群指定客户端信息
--------------------------

API 定义::

    GET api/v4/clients/${clientid}

请求示例::

    GET api/v4/clients/mosquitto_mqtt

返回数据:

.. code:: json

  {
    "code": 0,
    "data": [
      {
        "username": "test",
        "recv_cnt": 38,
        "node": "emqx@127.0.0.1",
        "proto_name": "MQTT",
        "mqueue_len": 0,
        "mailbox_len": 0,
        "ip_address": "127.0.0.1",
        "awaiting_rel": 0,
        "max_mqueue": 1000,
        "send_msg": 0,
        "heap_size": 2586,
        "clientid": "mosquitto_mqtt",
        "created_at": "2019-12-18 10:27:24",
        "is_bridge": false,
        "proto_ver": 4,
        "expiry_interval": 0,
        "reductions": 32369,
        "max_subscriptions": 0,
        "recv_pkt": 1,
        "subscriptions_cnt": 0,
        "send_cnt": 37,
        "connected_at": "2019-12-18 10:27:24",
        "recv_msg": 0,
        "max_inflight": 32,
        "keepalive": 60,
        "max_awaiting_rel": 100,
        "mqueue_dropped": 0,
        "recv_oct": 93,
        "zone": "external",
        "inflight": 0,
        "connected": true,
        "port": 65273,
        "send_oct": 76,
        "send_pkt": 37,
        "clean_start": true
      }
    ]
  }

获取节点指定客户端信息
----------------------

API 定义::

    GET api/v4/nodes/${node}/clients/${clientid}

请求示例::

    GET api/v4/nodes/emqx@127.0.0.1/clients/mosquitto_mqtt

返回数据:

.. code:: json

  {
    "code": 0,
    "data": [
      {
        "username": "test",
        "recv_cnt": 46,
        "node": "emqx@127.0.0.1",
        "proto_name": "MQTT",
        "mqueue_len": 0,
        "mailbox_len": 0,
        "ip_address": "127.0.0.1",
        "awaiting_rel": 0,
        "max_mqueue": 1000,
        "send_msg": 0,
        "heap_size": 1598,
        "clientid": "mosquitto_mqtt",
        "created_at": "2019-12-18 10:27:24",
        "is_bridge": false,
        "proto_ver": 4,
        "expiry_interval": 0,
        "reductions": 38422,
        "max_subscriptions": 0,
        "recv_pkt": 1,
        "subscriptions_cnt": 0,
        "send_cnt": 45,
        "connected_at": "2019-12-18 10:27:24",
        "recv_msg": 0,
        "max_inflight": 32,
        "keepalive": 60,
        "max_awaiting_rel": 100,
        "mqueue_dropped": 0,
        "recv_oct": 109,
        "zone": "external",
        "inflight": 0,
        "connected": true,
        "port": 65273,
        "send_oct": 92,
        "send_pkt": 45,
        "clean_start": true
      }
    ]
  }

通过用户名获取集群指定客户端信息
--------------------------------

API 定义::

    GET api/v4/clients/username/${username}

请求示例::

    GET api/v4/clients/username/test

返回数据:

.. code:: json

  {
    "code": 0,
    "data": [
      {
        "username": "test",
        "recv_cnt": 2,
        "node": "emqx@127.0.0.1",
        "proto_name": "MQTT",
        "mqueue_len": 0,
        "mailbox_len": 0,
        "ip_address": "127.0.0.1",
        "awaiting_rel": 0,
        "max_mqueue": 1000,
        "send_msg": 0,
        "heap_size": 1598,
        "clientid": "mosquitto_mqtt",
        "created_at": "2019-12-18 11:21:08",
        "is_bridge": false,
        "proto_ver": 4,
        "expiry_interval": 0,
        "reductions": 5175,
        "max_subscriptions": 0,
        "recv_pkt": 1,
        "subscriptions_cnt": 0,
        "send_cnt": 1,
        "connected_at": "2019-12-18 11:21:08",
        "recv_msg": 0,
        "max_inflight": 32,
        "keepalive": 60,
        "max_awaiting_rel": 100,
        "mqueue_dropped": 0,
        "recv_oct": 36,
        "zone": "external",
        "inflight": 0,
        "connected": true,
        "port": 49816,
        "send_oct": 4,
        "send_pkt": 1,
        "clean_start": true
      }
    ]
  }

通过用户名获取节点指定客户端信息
--------------------------------

API 定义::

    GET api/v4/nodes/${nodes}/clients/username/${username}

请求示例::

    GET api/v4/nodes/emqx@127.0.0.1/clients/username/test

返回数据:

.. code:: json

  {
    "code": 0,
    "data": [
      {
        "username": "test",
        "recv_cnt": 4,
        "node": "emqx@127.0.0.1",
        "proto_name": "MQTT",
        "mqueue_len": 0,
        "mailbox_len": 0,
        "ip_address": "127.0.0.1",
        "awaiting_rel": 0,
        "max_mqueue": 1000,
        "send_msg": 0,
        "heap_size": 1598,
        "clientid": "mosquitto_mqtt",
        "created_at": "2019-12-18 11:21:08",
        "is_bridge": false,
        "proto_ver": 4,
        "expiry_interval": 0,
        "reductions": 6741,
        "max_subscriptions": 0,
        "recv_pkt": 1,
        "subscriptions_cnt": 0,
        "send_cnt": 3,
        "connected_at": "2019-12-18 11:21:08",
        "recv_msg": 0,
        "max_inflight": 32,
        "keepalive": 60,
        "max_awaiting_rel": 100,
        "mqueue_dropped": 0,
        "recv_oct": 40,
        "zone": "external",
        "inflight": 0,
        "connected": true,
        "port": 49816,
        "send_oct": 8,
        "send_pkt": 3,
        "clean_start": true
      }
    ]
  }

踢掉指定客户端
--------------

API 定义::

    DELETE api/v4/clients/${clientid}

请求示例::

    DELETE api/v4/clients/mosquitto_mqtt

返回数据:

.. code:: json

  {
    "code": 0
  }

获取指定客户端 ACL 缓存
-----------------------

API 定义::

    GET api/v4/clients/${clientid}/acl_cache

请求示例::

    GET api/v4/clients/mosquitto_mqtt/acl_cache

返回数据:

.. code:: json

  {
    "code": 0,
    "data": [
      {
        "access": "publish",
        "result": "allow",
        "topic": "mosquitto_mqtt",
        "updated_time": 1576659345830
      }
    ]
  }

清除指定客户端 ACL 缓存
-----------------------

API 定义::

    DELETE api/v4/clients/${clientid}/acl_cache

请求示例::

    DELETE api/v4/clients/mosquitto_mqtt/acl_cache

返回数据:

.. code:: json

  {
    "code": 0
  }

--------------------
订阅(Subscriptions)
--------------------

获取集群订阅信息
------------------------

API 定义::

    GET api/v4/subscriptions

请求示例::

    GET api/v4/subscriptions?_page=1&_limit=10000

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "clientid": "mqttjs_f79fbc5a4b",
        "node": "emqx@127.0.0.1",
        "qos": 0,
        "topic": "testtopic/#"
      },
      {
        "clientid": "mosquitto_mqtt",
        "node": "emqx@127.0.0.1",
        "qos": 0,
        "topic": "t"
      }
    ],
    "meta": {
      "page": 1,
      "limit": 10000,
      "count": 2
    }
  }

获取集群指定连接订阅信息
------------------------

API 定义::

    GET api/v4/subscriptions/${clientid}

请求示例::

    GET api/v4/subscriptions/mosquitto_mqtt

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "clientid": "mosquitto_mqtt",
        "node": "emqx@127.0.0.1",
        "qos": 0,
        "topic": "t"
      }
    ]
  }

获取节点订阅信息
------------------------------

API 定义::

    GET api/v4/nodes/${node}/subscriptions

请求示例::

    GET api/v4/nodes/emqx@127.0.0.1/subscriptions?_page=1&_limit=10000

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "clientid": "mqttjs_f79fbc5a4b",
        "node": "emqx@127.0.0.1",
        "qos": 0,
        "topic": "testtopic/#"
      },
      {
        "clientid": "mosquitto_mqtt",
        "node": "emqx@127.0.0.1",
        "qos": 0,
        "topic": "t"
      }
    ],
    "meta": {
      "page": 1,
      "limit": 10000,
      "count": 2
    }
  }

获取节点指定连接订阅信息
------------------------------

API 定义::

    GET api/v4/nodes/${node}/subscriptions/${clientid}

请求示例::

    GET api/v4/nodes/emqx@127.0.0.1/subscriptions/mosquitto_mqtt

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "clientid": "mosquitto_mqtt",
        "node": "emqx@127.0.0.1",
        "qos": 0,
        "topic": "t"
      }
    ]
  }

------------
路由(Routes)
------------

获取集群路由表
--------------

API 定义::

    GET api/v4/routes

请求示例::

    GET api/v4/routes

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "node": "emqx@127.0.0.1",
        "topic": "testtopic/#"
      },
      {
        "node": "emqx@127.0.0.1",
        "topic": "t"
      }
    ],
    "meta": {
      "page": 1,
      "limit": 10000,
      "count": 2
    }
  }

获取集群指定主题的路由信息
----------------------------

API 定义::

    GET api/v4/routes/${topic}

请求示例::

    GET api/v4/routes/t

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "node": "emqx@127.0.0.1",
        "topic": "t"
      }
    ]
  }

------------------
发布/订阅/取消订阅
------------------

发布消息
--------

API 定义::

    POST api/v4/mqtt/publish

请求参数:

.. code-block:: json

  {
    "topic": "test_topic",
    "payload": "hello",
    "qos": 1,
    "retain": false,
    "clientid": "mqttjs_ab9069449e"
  }

请求示例::

    POST api/v4/mqtt/publish

返回数据:

.. code-block:: json

  {
    "code": 0
  }

创建订阅
--------

API 定义::

    POST api/v4/mqtt/subscribe

请求参数:

.. code-block:: json

  {
    "topic": "test_topic",
    "qos": 1,
    "clientid": "mqttjs_ab9069449e"
  }

请求示例::

    POST api/v4/mqtt/subscribe

返回数据:

.. code-block:: json

  {
    "code": 0
  }

取消订阅
--------

API 定义::

    POST api/v4/mqtt/unsubscribe

请求参数:

.. code-block:: json

  {
    "topic": "test_topic",
    "clientid": "mqttjs_ab9069449e"
  }

请求示例::

    POST api/v4/mqtt/unsubscribe

返回数据:

.. code-block:: json

  {
    "code": 0
  }

-------------
插件(Plugins)
-------------

获取所有节点插件列表
------------------------

API 定义::

    GET api/v4/plugins

请求示例::

    GET api/v4/plugins

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "node": "emqx@127.0.0.1",
        "plugins": [
          {
            "name": "emqx_auth_clientid",
            "version": "v4.0.0",
            "description": "EMQ X Authentication with ClientId/Password",
            "active": false,
            "type": "auth"
          },
          {
            "name": "emqx_auth_http",
            "version": "v4.0.0",
            "description": "EMQ X Authentication/ACL with HTTP API",
            "active": false,
            "type": "auth"
          },
          {
            "name": "emqx_auth_jwt",
            "version": "v4.0.0",
            "description": "EMQ X Authentication with JWT",
            "active": false,
            "type": "auth"
          },
          {
            "name": "emqx_auth_ldap",
            "version": "v4.0.0",
            "description": "EMQ X Authentication/ACL with LDAP",
            "active": false,
            "type": "auth"
          },
          {
            "name": "emqx_auth_mongo",
            "version": "v4.0.0",
            "description": "EMQ X Authentication/ACL with MongoDB",
            "active": false,
            "type": "auth"
          },
          {
            "name": "emqx_auth_mysql",
            "version": "v4.0.0",
            "description": "EMQ X Authentication/ACL with MySQL",
            "active": false,
            "type": "auth"
          },
          {
            "name": "emqx_auth_pgsql",
            "version": "v4.0.0",
            "description": "EMQ X Authentication/ACL with PostgreSQL",
            "active": false,
            "type": "auth"
          },
          {
            "name": "emqx_auth_redis",
            "version": "v4.0.0",
            "description": "EMQ X Authentication/ACL with Redis",
            "active": false,
            "type": "auth"
          },
          {
            "name": "emqx_auth_username",
            "version": "v4.0.0",
            "description": "EMQ X Authentication with Username and Password",
            "active": false,
            "type": "auth"
          },
          {
            "name": "emqx_bridge_mqtt",
            "version": "v4.0.0",
            "description": "EMQ X Bridge to MQTT Broker",
            "active": false,
            "type": "bridge"
          },
          {
            "name": "emqx_coap",
            "version": "v4.0.0",
            "description": "EMQ X CoAP Gateway",
            "active": false,
            "type": "protocol"
          },
          {
            "name": "emqx_dashboard",
            "version": "v4.0.0",
            "description": "EMQ X Web Dashboard",
            "active": true,
            "type": "feature"
          },
          {
            "name": "emqx_delayed_publish",
            "version": "v4.0.0",
            "description": "EMQ X Delayed Publish",
            "active": false,
            "type": "feature"
          },
          {
            "name": "emqx_lua_hook",
            "version": "v4.0.0",
            "description": "EMQ X Lua Hooks",
            "active": false,
            "type": "feature"
          },
          {
            "name": "emqx_lwm2m",
            "version": "v4.0.0",
            "description": "EMQ X LwM2M Gateway",
            "active": false,
            "type": "protocol"
          },
          {
            "name": "emqx_management",
            "version": "v4.0.0",
            "description": "EMQ X Management API and CLI",
            "active": true,
            "type": "feature"
          },
          {
            "name": "emqx_psk_file",
            "version": "v4.0.0",
            "description": "EMQX PSK Plugin from File",
            "active": false,
            "type": "feature"
          },
          {
            "name": "emqx_recon",
            "version": "v4.0.0",
            "description": "EMQ X Recon Plugin",
            "active": true,
            "type": "feature"
          },
          {
            "name": "emqx_reloader",
            "version": "v4.0.0",
            "description": "EMQ X Reloader Plugin",
            "active": false,
            "type": "feature"
          },
          {
            "name": "emqx_retainer",
            "version": "v4.0.0",
            "description": "EMQ X Retainer",
            "active": true,
            "type": "feature"
          },
          {
            "name": "emqx_rule_engine",
            "version": "v4.0.0",
            "description": "EMQ X Rule Engine",
            "active": true,
            "type": "feature"
          },
          {
            "name": "emqx_sn",
            "version": "v4.0.0",
            "description": "EMQ X MQTT-SN Plugin",
            "active": false,
            "type": "protocol"
          },
          {
            "name": "emqx_statsd",
            "version": "v4.0.0",
            "description": "Statsd for EMQ X",
            "active": false,
            "type": "feature"
          },
          {
            "name": "emqx_stomp",
            "version": "v4.0.0",
            "description": "EMQ X Stomp Protocol Plugin",
            "active": false,
            "type": "protocol"
          },
          {
            "name": "emqx_web_hook",
            "version": "v4.0.0",
            "description": "EMQ X Webhook Plugin",
            "active": false,
            "type": "feature"
          }
        ]
      }
    ]
  }

获取节点插件列表
------------------

API 定义::

    GET api/v4/nodes/${node}/plugins/

请求示例::

    GET api/v4/nodes/emqx@127.0.0.1/plugins/

返回数据:

.. code:: json

  {
    "code": 0,
    "data": [
      {
        "name": "emqx_auth_clientid",
        "version": "develop",
        "description": "EMQ X Authentication with ClientId/Password",
        "active": false,
        "type": "auth"
      },
      {
        "name": "emqx_auth_http",
        "version": "develop",
        "description": "EMQ X Authentication/ACL with HTTP API",
        "active": false,
        "type": "auth"
      },
      {
        "name": "emqx_auth_jwt",
        "version": "develop",
        "description": "EMQ X Authentication with JWT",
        "active": false,
        "type": "auth"
      },
      {
        "name": "emqx_auth_ldap",
        "version": "develop",
        "description": "EMQ X Authentication/ACL with LDAP",
        "active": false,
        "type": "auth"
      },
      {
        "name": "emqx_auth_mongo",
        "version": "develop",
        "description": "EMQ X Authentication/ACL with MongoDB",
        "active": false,
        "type": "auth"
      },
      {
        "name": "emqx_auth_mysql",
        "version": "develop",
        "description": "EMQ X Authentication/ACL with MySQL",
        "active": false,
        "type": "auth"
      },
      {
        "name": "emqx_auth_pgsql",
        "version": "develop",
        "description": "EMQ X Authentication/ACL with PostgreSQL",
        "active": false,
        "type": "auth"
      },
      {
        "name": "emqx_auth_redis",
        "version": "develop",
        "description": "EMQ X Authentication/ACL with Redis",
        "active": false,
        "type": "auth"
      },
      {
        "name": "emqx_auth_username",
        "version": "develop",
        "description": "EMQ X Authentication with Username and Password",
        "active": false,
        "type": "auth"
      },
      {
        "name": "emqx_bridge_mqtt",
        "version": "develop",
        "description": "EMQ X Bridge to MQTT Broker",
        "active": false,
        "type": "bridge"
      },
      {
        "name": "emqx_coap",
        "version": "develop",
        "description": "EMQ X CoAP Gateway",
        "active": false,
        "type": "protocol"
      },
      {
        "name": "emqx_dashboard",
        "version": "develop",
        "description": "EMQ X Web Dashboard",
        "active": true,
        "type": "feature"
      },
      {
        "name": "emqx_delayed_publish",
        "version": "develop",
        "description": "EMQ X Delayed Publish",
        "active": false,
        "type": "feature"
      },
      {
        "name": "emqx_lua_hook",
        "version": "develop",
        "description": "EMQ X Lua Hooks",
        "active": false,
        "type": "feature"
      },
      {
        "name": "emqx_lwm2m",
        "version": "develop",
        "description": "EMQ X LwM2M Gateway",
        "active": false,
        "type": "protocol"
      },
      {
        "name": "emqx_management",
        "version": "develop",
        "description": "EMQ X Management API and CLI",
        "active": true,
        "type": "feature"
      },
      {
        "name": "emqx_psk_file",
        "version": "develop",
        "description": "EMQX PSK Plugin from File",
        "active": false,
        "type": "feature"
      },
      {
        "name": "emqx_recon",
        "version": "develop",
        "description": "EMQ X Recon Plugin",
        "active": true,
        "type": "feature"
      },
      {
        "name": "emqx_reloader",
        "version": "develop",
        "description": "EMQ X Reloader Plugin",
        "active": false,
        "type": "feature"
      },
      {
        "name": "emqx_retainer",
        "version": "develop",
        "description": "EMQ X Retainer",
        "active": true,
        "type": "feature"
      },
      {
        "name": "emqx_rule_engine",
        "version": "develop",
        "description": "EMQ X Rule Engine",
        "active": true,
        "type": "feature"
      },
      {
        "name": "emqx_sn",
        "version": "develop",
        "description": "EMQ X MQTT-SN Plugin",
        "active": false,
        "type": "protocol"
      },
      {
        "name": "emqx_statsd",
        "version": "develop",
        "description": "Statsd for EMQ X",
        "active": false,
        "type": "feature"
      },
      {
        "name": "emqx_stomp",
        "version": "develop",
        "description": "EMQ X Stomp Protocol Plugin",
        "active": false,
        "type": "protocol"
      },
      {
        "name": "emqx_web_hook",
        "version": "develop",
        "description": "EMQ X Webhook Plugin",
        "active": false,
        "type": "feature"
      }
    ]
  }

启用节点指定插件
-----------------------

API 定义::

    PUT api/v4/nodes/${node}/plugins/${plugin}/load

请求示例::

    PUT api/v4/nodes/emqx@127.0.0.1/plugins/emqx_auth_clientid/load

返回数据:

.. code-block:: json

  {
    "code": 0
  }

关闭节点指定插件
-----------------------

API 定义::

    PUT api/v4/nodes/${node}/plugins/${plugin}/unload

请求示例::

    PUT api/v4/nodes/emqx@127.0.0.1/plugins/emqx_auth_clientid/unload

返回数据:

.. code-block:: json

  {
    "code": 0
  }

重启节点指定插件
-----------------------

API 定义::

    PUT api/v4/nodes/${node}/plugins/${plugin}/reload

请求示例::

    PUT api/v4/nodes/emqx@127.0.0.1/plugins/emqx_auth_clientid/reload

返回数据:

.. code-block:: json

  {
    "code": 0
  }

------------------
监听器(Listeners)
------------------

获取集群监听器列表
------------------------

API 定义::

    GET api/v4/listeners/

请求示例::

    GET api/v4/listeners/

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "listeners": [
          {
            "acceptors": 16,
            "current_conns": 0,
            "listen_on": "8883",
            "max_conns": 102400,
            "protocol": "mqtt:ssl",
            "shutdown_count": [ ]
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
            "shutdown_count": [ ]
          },
          {
            "acceptors": 4,
            "current_conns": 1,
            "listen_on": "18083",
            "max_conns": 512,
            "protocol": "http:dashboard",
            "shutdown_count": [ ]
          },
          {
            "acceptors": 2,
            "current_conns": 0,
            "listen_on": "8081",
            "max_conns": 512,
            "protocol": "http:management",
            "shutdown_count": [ ]
          },
          {
            "acceptors": 4,
            "current_conns": 0,
            "listen_on": "8083",
            "max_conns": 102400,
            "protocol": "mqtt:ws",
            "shutdown_count": [ ]
          },
          {
            "acceptors": 4,
            "current_conns": 0,
            "listen_on": "8084",
            "max_conns": 16,
            "protocol": "mqtt:wss",
            "shutdown_count": [ ]
          }
        ],
        "node": "emqx@127.0.0.1"
      }
    ]
  }

获取节点监听器列表
------------------------

API 定义::

    GET api/v4/nodes/${node}/listeners

请求示例::

    GET api/v4/nodes/emqx@127.0.0.1/listeners

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "acceptors": 16,
        "current_conns": 0,
        "listen_on": "8883",
        "max_conns": 102400,
        "protocol": "mqtt:ssl",
        "shutdown_count": [ ]
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
        "shutdown_count": [ ]
      },
      {
        "acceptors": 4,
        "current_conns": 1,
        "listen_on": "18083",
        "max_conns": 512,
        "protocol": "http:dashboard",
        "shutdown_count": [ ]
      },
      {
        "acceptors": 2,
        "current_conns": 0,
        "listen_on": "8081",
        "max_conns": 512,
        "protocol": "http:management",
        "shutdown_count": [ ]
      },
      {
        "acceptors": 4,
        "current_conns": 0,
        "listen_on": "8083",
        "max_conns": 102400,
        "protocol": "mqtt:ws",
        "shutdown_count": [ ]
      },
      {
        "acceptors": 4,
        "current_conns": 0,
        "listen_on": "8084",
        "max_conns": 16,
        "protocol": "mqtt:wss",
        "shutdown_count": [ ]
      }
    ]
  }

------------
收发报文统计
------------

获取集群收发报文统计
--------------------------

API 定义::

    GET api/v4/metrics/

请求示例::

    GET api/v4/metrics/

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "node": "emqx@127.0.0.1",
        "metrics": {
          "auth.clientid.failure": 0,
          "rules.matched": 0,
          "messages.sent": 0,
          "packets.disconnect.sent": 0,
          "bytes.sent": 8,
          "packets.disconnect.received": 0,
          "packets.pingresp.sent": 0,
          "packets.pingreq.received": 0,
          "packets.unsubscribe.received": 0,
          "packets.pubcomp.missed": 0,
          "packets.puback.missed": 0,
          "packets.pubcomp.sent": 0,
          "packets.pubcomp.received": 0,
          "packets.pubrec.missed": 0,
          "auth.mqtt.anonymous": 2,
          "packets.connack.auth_error": 0,
          "packets.pubcomp.inuse": 0,
          "actions.failure": 0,
          "packets.pubrec.inuse": 0,
          "packets.suback.sent": 0,
          "packets.puback.sent": 0,
          "messages.retained": 0,
          "messages.received": 0,
          "packets.connect.received": 2,
          "messages.forward": 0,
          "packets.pubrel.missed": 0,
          "packets.publish.received": 0,
          "packets.connack.sent": 2,
          "auth.clientid.ignore": 2,
          "packets.subscribe.received": 0,
          "packets.pubrel.received": 0,
          "packets.pubrec.received": 0,
          "packets.puback.received": 0,
          "packets.sent": 2,
          "packets.received": 2,
          "bytes.received": 34,
          "messages.expired": 0,
          "messages.dropped": 0,
          "messages.qos2.dropped": 0,
          "messages.qos2.expired": 0,
          "packets.pubrel.sent": 0,
          "packets.pubrec.sent": 0,
          "packets.publish.sent": 0,
          "actions.success": 0,
          "channel.gc": 0,
          "packets.publish.error": 0,
          "packets.unsubscribe.error": 0,
          "messages.qos2.received": 0,
          "messages.qos1.received": 0,
          "messages.qos0.received": 0,
          "packets.auth.sent": 0,
          "messages.qos2.sent": 0,
          "messages.qos1.sent": 0,
          "messages.qos0.sent": 0,
          "packets.auth.received": 0,
          "packets.unsuback.sent": 0,
          "auth.clientid.success": 0,
          "packets.connack.error": 0,
          "packets.publish.auth_error": 0,
          "packets.subscribe.error": 0,
          "packets.subscribe.auth_error": 0
        }
      }
    ]
  }

获取节点收发报文统计
--------------------------

API 定义::

    GET api/v4/nodes/${node}/metrics/

请求示例::

    GET api/v4/nodes/emqx@127.0.0.1/metrics/

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": {
      "auth.clientid.failure": 0,
      "rules.matched": 0,
      "messages.sent": 0,
      "packets.disconnect.sent": 0,
      "bytes.sent": 52,
      "packets.disconnect.received": 0,
      "packets.pingresp.sent": 22,
      "packets.pingreq.received": 0,
      "packets.unsubscribe.received": 0,
      "packets.pubcomp.missed": 0,
      "packets.puback.missed": 0,
      "packets.pubcomp.sent": 0,
      "packets.pubcomp.received": 0,
      "packets.pubrec.missed": 0,
      "auth.mqtt.anonymous": 2,
      "packets.connack.auth_error": 0,
      "packets.pubcomp.inuse": 0,
      "actions.failure": 0,
      "packets.pubrec.inuse": 0,
      "packets.suback.sent": 0,
      "packets.puback.sent": 0,
      "messages.retained": 2,
      "messages.received": 0,
      "packets.connect.received": 2,
      "messages.forward": 0,
      "packets.pubrel.missed": 0,
      "packets.publish.received": 0,
      "packets.connack.sent": 2,
      "auth.clientid.ignore": 2,
      "packets.subscribe.received": 0,
      "packets.pubrel.received": 0,
      "packets.pubrec.received": 0,
      "packets.puback.received": 0,
      "packets.sent": 24,
      "packets.received": 2,
      "bytes.received": 78,
      "messages.expired": 0,
      "messages.dropped": 0,
      "messages.qos2.dropped": 0,
      "messages.qos2.expired": 0,
      "packets.pubrel.sent": 0,
      "packets.pubrec.sent": 0,
      "packets.publish.sent": 0,
      "actions.success": 0,
      "channel.gc": 0,
      "packets.publish.error": 0,
      "packets.unsubscribe.error": 0,
      "messages.qos2.received": 0,
      "messages.qos1.received": 0,
      "messages.qos0.received": 0,
      "packets.auth.sent": 0,
      "messages.qos2.sent": 0,
      "messages.qos1.sent": 0,
      "messages.qos0.sent": 0,
      "packets.auth.received": 0,
      "packets.unsuback.sent": 0,
      "auth.clientid.success": 0,
      "packets.connack.error": 0,
      "packets.publish.auth_error": 0,
      "packets.subscribe.error": 0,
      "packets.subscribe.auth_error": 0
    }
  }

-------------
连接会话统计
-------------

获取集群连接会话统计
---------------------------

API 定义::

    GET api/v4/stats

请求示例::

    GET api/v4/stats

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "node": "emqx@127.0.0.1",
        "stats": {
          "subscriptions.shared.max": 0,
          "subscriptions.max": 0,
          "subscribers.max": 0,
          "resources.max": 0,
          "topics.count": 0,
          "channels.count": 2,
          "subscriptions.count": 0,
          "suboptions.max": 0,
          "topics.max": 0,
          "connections.max": 2,
          "actions.count": 5,
          "retained.count": 0,
          "rules.count": 0,
          "routes.count": 0,
          "subscriptions.shared.count": 0,
          "suboptions.count": 0,
          "sessions.count": 2,
          "channels.max": 2,
          "actions.max": 5,
          "retained.max": 0,
          "sessions.max": 2,
          "rules.max": 0,
          "routes.max": 0,
          "resources.count": 0,
          "subscribers.count": 0,
          "connections.count": 2
        }
      }
    ]
  }

获取节点连接会话统计
--------------------------

API 定义::

    GET api/v4/nodes/${node}/stats

请求示例::

    GET api/v4/nodes/emqx@127.0.0.1/stats

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": {
      "subscriptions.shared.max": 0,
      "subscriptions.max": 0,
      "subscribers.max": 0,
      "resources.max": 0,
      "topics.count": 0,
      "channels.count": 2,
      "subscriptions.count": 0,
      "suboptions.max": 0,
      "topics.max": 0,
      "connections.max": 2,
      "actions.count": 5,
      "retained.count": 0,
      "rules.count": 0,
      "routes.count": 0,
      "subscriptions.shared.count": 0,
      "suboptions.count": 0,
      "sessions.count": 2,
      "channels.max": 2,
      "actions.max": 5,
      "retained.max": 0,
      "sessions.max": 2,
      "rules.max": 0,
      "routes.max": 0,
      "resources.count": 0,
      "subscribers.count": 0,
      "connections.count": 2
    }
  }

----------
告警信息
----------

获取集群当前告警信息
--------------------

API 定义::

    GET api/v4/alarms/present

请求示例::

    GET api/v4/alarms/present

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "alarms": [],
        "node": "emqx@127.0.0.1"
      }
    ]
  }

获取节点当前告警信息
--------------------

API 定义::

    GET api/v4/alarms/present/${node}

请求示例::

    GET api/v4/alarms/present/emqx@127.0.0.1

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": []
  }

获取集群历史告警信息
--------------------

API 定义::

    GET api/v4/alarms/history

请求示例::

    GET api/v4/alarms/history

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "alarms": [
          {
            "clear_at": "2019-07-10 16:54:35",
            "desc": "82.60344181007542",
            "id": "cpu_high_watermark"
          }
        ],
        "node": "emqx@127.0.0.1"
      }
    ]
  }

获取节点历史告警信息
--------------------

API 定义::

    GET api/v4/alarms/present/${node}

请求示例::

    GET api/v4/alarms/present/emqx@127.0.0.1

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
      {
        "clear_at": "2019-07-10 16:54:35",
        "desc": "82.60344181007542",
        "id": "cpu_high_watermark"
      }
    ]
  }

----------
黑名单
----------

获取黑名单列表
---------------------------

API 定义::

    GET api/v4/banned

请求示例::

    GET api/v4/banned?_page=1&_limit=10000

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [
        {
            "as": "clientid",
            "at": 1576734915,
            "by": "user",
            "reason": "banned the clientId",
            "until": 1576735035,
            "who": "mqttjs_ab9069449e"
        }
    ],
    "meta": {
        "page": 1,
        "limit": 10000,
        "count": 1
    }
  }

创建黑名单
--------------------------

API 定义::

    POST api/v4/banned

请求参数:

.. code-block:: json

  {
    "who": "mqttjs_ab9069449e",
    "as": "clientid",
    "reason": "banned the clientId",
    "until": 1576735035
  }
 
请求示例::

    POST api/v4/banned/

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": {
      "who": "mqttjs_ab9069449e",
      "as": "clientid",
      "reason": "banned the clientId",
      "until": 1576735035
    }
  }

删除指定黑名单
--------------------------

API 定义::

    DELETE api/v4/banned/${as}/${who}

请求示例::

    DELETE api/v4/banned/clientid/mqttjs_ab9069449e

返回数据:

.. code-block:: json

  {
    "code": 0
  }

------------------
错误信息与数据分页
------------------

HTTP 状态码大于 500 时响应携带错误信息返回
-------------------------------------------

错误示例::

    PUT api/v4/nodes/emqx@127.0.0.1/plugins/emqx_recon/load

返回数据:

.. code-block:: json

  {
    "message": "already_started"
  }

分页参数与分页信息
------------------

请求示例中使用了 ?_page=1&_limit=10000 参数的接口均支持分页::

    _page: 当前页码
    _limit: 分页大小

返回数据:

.. code-block:: json

  {
    "code": 0,
    "data": [],
    "meta": {
      "page": 1,
      "limit": 10000,
      "count": 0
    }
  }

----------------------
规则引擎(rule engine)
----------------------

创建规则
----------

API 定义::

  POST api/v4/rules

参数定义:

+-------------+---------------------------------------------------------------+-----------------------+
| name        | String，规则名字                                                                      |
+-------------+---------------------------------------------------------------+-----------------------+
| for         | String，Hook 的名字，可以为:                                                          |
|             | "message.publish"，"client.connected" ... 详见 :ref:`plugins`                         |
+-------------+---------------------------------------------------------------+-----------------------+
| rawsql      | String，用于筛选和转换原始数据的 SQL 语句                                             |
+-------------+---------------------------------------------------------------+-----------------------+
| actions     | JSON Array，动作列表                                                                  |
+-------------+---------------------------------------------------------------+-----------------------+
| -           | name                                                          | String, 动作名字      |
+-------------+---------------------------------------------------------------+-----------------------+
| -           | params                                                        | JSON Object, 动作参数 |
+-------------+---------------------------------------------------------------+-----------------------+
| description | String，可选，规则描述                                                                |
+-------------+---------------------------------------------------------------+-----------------------+

请求参数示例:

.. code-block:: json

  {
    "name": "test-rule",
    "for": "message.publish",
    "rawsql": "select * from \"t/a\"",
    "actions": [{
        "name": "built_in:inspect_action",
        "params": {
            "a": 1
        }
    }],
    "description": "test-rule"
  }

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": {
        "actions": [{
            "name": "built_in:inspect_action",
            "params": {
                "$resource": "built_in:test-resource",
                "a": 1
            }
        }],
        "description": "test-rule",
        "enabled": true,
        "for": "message.publish",
        "id": "test-rule:1556263150688255821",
        "name": "test-rule",
        "rawsql": "select * from \"t/a\""
    }
  }

查询规则
----------

API 定义::

  GET api/v4/rules/${rule_id}

请求参数示例::

  GET api/v4/rules/test-rule:1556263150688255821

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": {
        "actions": [{
            "name": "built_in:inspect_action",
            "params": {
                "$resource": "built_in:test-resource",
                "a": 1
            }
        }],
        "description": "test-rule",
        "enabled": true,
        "for": "message.publish",
        "id": "test-rule:1556263150688255821",
        "name": "test-rule",
        "rawsql": "select * from \"t/a\""
    }
  }

获取当前规则列表
----------------

API 定义::

  GET api/v4/rules

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": [{
        "actions": [{
            "name": "built_in:inspect_action",
            "params": {
                "$resource": "built_in:test-resource",
                "a": 1
            }
        }],
        "description": "test-rule",
        "enabled": true,
        "for": "message.publish",
        "id": "test-rule:1556263150688255821",
        "name": "test-rule",
        "rawsql": "select * from \"t/a\""
    }]
  }

删除规则
----------

API 定义::

  DELETE api/v4/rules/${rule_id}

请求参数示例::

  DELETE api/v4/rules/test-rule:1556263150688255821

返回数据示例:

.. code-block:: json

  {
    "code": 0
  }

获取当前动作列表
----------------

API 定义::

  GET api/v4/actions?for=${hook_type}

请求参示例::

  GET api/v4/actions

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": [{
        "app": "emqx_rule_engine",
        "description": "Republish a MQTT message to a another topic",
        "for": "message.publish",
        "name": "built_in:republish_action",
        "params": {
            "target_topic": {
                "description": "Repubilsh the message to which topic",
                "format": "topic",
                "required": true,
                "title": "To Which Topic",
                "type": "string"
            }
        },
        "type": "built_in"
    }, {
        "app": "emqx_web_hook",
        "description": "Forward Events to Web Server",
        "for": "$events",
        "name": "web_hook:event_action",
        "params": {
            "$resource": {
                "description": "Bind a resource to this action",
                "required": true,
                "title": "Resource ID",
                "type": "string"
            },
            "template": {
                "description": "The payload template to be filled with variables before sending messages",
                "required": false,
                "schema": {},
                "title": "Payload Template",
                "type": "object"
            }
        },
        "type": "web_hook"
    }, {
        "app": "emqx_web_hook",
        "description": "Forward Messages to Web Server",
        "for": "message.publish",
        "name": "web_hook:publish_action",
        "params": {
            "$resource": {
                "description": "Bind a resource to this action",
                "required": true,
                "title": "Resource ID",
                "type": "string"
            }
        },
        "type": "web_hook"
    }, {
        "app": "emqx_rule_engine",
        "description": "Inspect the details of action params for debug purpose",
        "for": "$any",
        "name": "built_in:inspect_action",
        "params": {},
        "type": "built_in"
    }]
  }

请求参数示例::

  GET api/v4/actions?for=client.connected

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": [{
        "app": "emqx_rule_engine",
        "description": "Inspect the details of action params for debug purpose",
        "for": "$any",
        "name": "built_in:inspect_action",
        "params": {},
        "type": "built_in"
    }]
  }

查询动作
---------

API 定义::

  GET api/v4/actions/:action_name

请求参数示例::

  GET api/v4/actions/built_in:inspect_action

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": {
        "app": "emqx_rule_engine",
        "description": "Inspect the details of action params for debug purpose",
        "for": "$any",
        "name": "built_in:inspect_action",
        "params": {},
        "type": "built_in"
    }
  }

获取当前资源类型列表
--------------------

API 定义::

  GET api/v4/resource_types

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": [{
        "attrs": "undefined",
        "config": {
            "url": "http://host-name/chats"
        },
        "description": "forward msgs to host-name/chats",
        "id": "web_hook:webhook1",
        "name": "webhook1",
        "type": "web_hook"
    }, {
        "attrs": "undefined",
        "config": {
            "a": 1
        },
        "description": "test-resource",
        "id": "built_in:test-resource",
        "name": "test-resource",
        "type": "built_in"
    }]
  }

查询资源类型
-------------

API 定义::

  GET api/v4/resource_types/${type}

请求参数示例::

  GET api/v4/resource_types/built_in

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": {
        "description": "The built in resource type for debug purpose",
        "name": "built_in",
        "params": {},
        "provider": "emqx_rule_engine"
    }
  }

获取某种类型的资源
--------------------

API 定义::

  GET api/v4/resource_types/${type}/resources

请求参数示例::

  GET api/v4/resource_types/built_in/resources

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": [{
        "attrs": "undefined",
        "config": {
            "a": 1
        },
        "description": "test-resource",
        "id": "built_in:test-resource",
        "name": "test-resource",
        "type": "built_in"
    }]
  }

获取某种类型的动作
--------------------

API 定义::

  GET api/v4/resource_types/${type}/actions

请求参数示例::

  GET api/v4/resource_types/built_in/actions

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": [{
        "app": "emqx_rule_engine",
        "description": "Inspect the details of action params for debug purpose",
        "for": "$any",
        "name": "built_in:inspect_action",
        "params": {},
        "type": "built_in"
    }, {
        "app": "emqx_rule_engine",
        "description": "Republish a MQTT message to a another topic",
        "for": "message.publish",
        "name": "built_in:republish_action",
        "params": {
            "target_topic": {
                "description": "Repubilsh the message to which topic",
                "format": "topic",
                "required": true,
                "title": "To Which Topic",
                "type": "string"
            }
        },
        "type": "built_in"
    }]
  }

创建资源
----------

API 定义::

  POST api/v4/resources

参数定义:

+-------------+------------------------+
| name        | String, 资源名字       |
+-------------+------------------------+
| type        | String, 资源类型       |
+-------------+------------------------+
| config      | JSON Object, 资源配置  |
+-------------+------------------------+
| description | String，可选，规则描述 |
+-------------+------------------------+

参数示例::

  {
    "name": "test-resource",
    "type": "built_in",
    "config": {
        "a": 1
    },
    "description": "test-resource"
  }

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": {
        "attrs": "undefined",
        "config": {
            "a": 1
        },
        "description": "test-resource",
        "id": "built_in:test-resource",
        "name": "test-resource",
        "type": "built_in"
    }
  }

获取资源列表
------------

API 定义::

  GET api/v4/resources

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": [{
        "attrs": "undefined",
        "config": {
            "url": "http://host-name/chats"
        },
        "description": "forward msgs to host-name/chats",
        "id": "web_hook:webhook1",
        "name": "webhook1",
        "type": "web_hook"
    }, {
        "attrs": "undefined",
        "config": {
            "a": 1
        },
        "description": "test-resource",
        "id": "built_in:test-resource",
        "name": "test-resource",
        "type": "built_in"
    }]
  }

查询资源
----------

API 定义::

  GET api/v4/resources/:resource_id

请求参数示例::

  GET api/v4/resources/built_in:test-resource

返回数据示例:

.. code-block:: json

  {
    "code": 0,
    "data": {
        "attrs": "undefined",
        "config": {
            "a": 1
        },
        "description": "test-resource",
        "id": "built_in:test-resource",
        "name": "test-resource",
        "type": "built_in"
    }
  }

删除资源
----------

API 定义::

  DELETE api/v4/resources/:resource_id

请求参数示例::

  DELETE api/v4/resources/built_in:test-resource

返回数据示例:

.. code-block:: json

  {
    "code": 0
  }
