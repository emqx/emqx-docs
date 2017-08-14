
.. _rest_api:

======================
管理监控API (REST API)
======================

用户可以通过REST API查询MQTT客户端连接(Clients)、会话(Sessions)、订阅(Subscriptions)和路由(Routes)信息，还可以检索和监控服务器的性能指标和统计数据。

-------
URL地址
-------

REST APIs访问URL地址::

    http(s)://host:8080/api/v2/

---------
Basic认证
---------

REST API采用HTTP Basic认证(Authentication):

.. code-block:: bash

    curl -v --basic -u <user>:<passwd> -k http://localhost:8080/api/v2/nodes/emq@127.0.0.1/clients

----------
集群与节点
----------

获取全部节点的基本信息
----------------------

API定义::

    GET api/v2/management/nodes

请求示例::

    GET api/v2/management/nodes

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        [
            {
                "name": "emq@127.0.0.1",
                "version": "2.3",
                "sysdescr": "EMQ",
                "uptime": "1 hours, 17 minutes, 1 seconds",
                "datetime": "2017-04-14 14:11:38",
                "otp_release": "R19/8.3",
                "node_status": "Running"
            }
        ]
    }

获取指定节点的基本信息
----------------------

API定义::

    GET api/v2/management/nodes/{node_name}

请求示例::

    GET api/v2/management/nodes/emq@127.0.0.1

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "version": "2.3",
            "sysdescr": "EMQ X",
            "uptime": "1 hours, 17 minutes, 18 seconds",
            "datetime": "2017-04-14 14:11:55",
            "otp_release": "R19/8.3",
            "node_status": "Running"
        }
    }

获取全部节点的监控数据
----------------------

API定义::

    GET api/v2/monitoring/nodes

请求示例::

    GET api/v2/monitoring/nodes

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        [
            {
                "name": "emq@127.0.0.1",
                "otp_release": "R19/8.3",
                "memory_total": "69.19M",
                "memory_used": "49.28M",
                "process_available": 262144,
                "process_used": 303,
                "max_fds": 256,
                "clients": 1,
                "node_status": "Running",
                "load1": "1.93",
                "load5": "1.93",
                "load15": "1.89"
            }
        ]
    }

获取指定节点的监控数据
----------------------

API定义::

    GET api/v2/monitoring/nodes/{node_name}

请求示例::

    GET api/v2/monitoring/nodes/emq@127.0.0.1

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "name": "emq@127.0.0.1",
            "otp_release": "R19/8.3",
            "memory_total": "69.19M",
            "memory_used": "49.24M",
            "process_available": 262144,
            "process_used": 303,
            "max_fds": 256,
            "clients": 1,
            "node_status": "Running",
            "load1": "2.21",
            "load5": "2.00",
            "load15": "1.92"
        }
    }

-------------------
客户端连接(Clients)
-------------------

获取指定节点的客户端连接列表
----------------------------

API定义::

    GET api/v2/nodes/{node_name}/clients
 
请求参数:: 

    curr_page={page_no}&page_size={page_size}

请求示例::

    api/v2/nodes/emq@127.0.0.1/clients?curr_page=1&page_size=20

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "current_page": 1,
            "page_size": 20,
            "total_num": 1,
            "total_page": 1,
            "objects":
            [
                {
                    "client_id": "C_1492145414740",
                    "username": "undefined",
                    "ipaddress": "127.0.0.1",
                    "port": 49639,
                    "clean_sess": true,
                    "proto_ver": 4,
                    "keepalive": 60,
                    "connected_at": "2017-04-14 12:50:15"
                }
            ]
        }   
    }

获取节点指定客户端连接的信息
----------------------------

API定义::

    GET api/v2/nodes/{node_name}/clients/{clientid}
 
请求示例::

    GET api/v2/nodes/emq@127.0.0.1/clients/C_1492145414740

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": {
            "objects": [
                {
                    "client_id": "C_1492145414740",
                    "username": "undefined",
                    "ipaddress": "127.0.0.1",
                    "port": 50953,
                    "clean_sess": true,
                    "proto_ver": 4,
                    "keepalive": 60,
                    "connected_at": "2017-04-14 13:35:15"
                }
            ]
        }
    }

获取集群内指定客户端的信息
--------------------------

API定义::

    GET api/v2/clients/{clientid}
 
请求示例::

    GET api/v2/clients/C_1492145414740

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": {
            "objects": [
                {
                    "client_id": "C_1492145414740",
                    "username": "undefined",
                    "ipaddress": "127.0.0.1",
                    "port": 50953,
                    "clean_sess": true,
                    "proto_ver": 4,
                    "keepalive": 60,
                    "connected_at": "2017-04-14 13:35:15"
                }
            ]
        }
    }


--------------
会话(Sessions)
--------------

获取指定节点的会话列表
----------------------

API定义::

    GET api/v2/nodes/{node_name}/sessions
 
请求参数::

    curr_page={page_no}&page_size={page_size}

请求示例::

    GET api/v2/nodes/emq@127.0.0.1/sessions?curr_page=1&page_size=20

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "current_page": 1,
            "page_size": 20,
            "total_num": 1,
            "total_page": 1,
            "objects":
            [
                {
                    "client_id": "C_1492145414740",
                    "clean_sess": true,
                    "max_inflight": "undefined",
                    "inflight_queue": "undefined",
                    "message_queue": "undefined",
                    "message_dropped": "undefined",
                    "awaiting_rel": "undefined",
                    "awaiting_ack": "undefined",
                    "awaiting_comp": "undefined",
                    "created_at": "2017-04-14 13:35:15"
                }
            ]
        }
    }

获取节点上指定客户端的会话信息
------------------------------

API定义::

    GET api/v2/nodes/{node_name}/sessions/{clientid}
 
请求示例::

    GET api/v2/nodes/emq@127.0.0.1/sessions/C_1492145414740

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "objects":
            [
                {
                    "client_id": "C_1492145414740",
                    "clean_sess": true,
                    "max_inflight": "undefined",
                    "inflight_queue": "undefined",
                    "message_queue": "undefined",
                    "message_dropped": "undefined",
                    "awaiting_rel": "undefined",
                    "awaiting_ack": "undefined",
                    "awaiting_comp": "undefined",
                    "created_at": "2017-04-14 13:35:15"
                }
            ]
        }
    }

获取集群内指定客户端的会话信息
------------------------------

API定义::

    GET api/v2/sessions/{clientid}
 
请求示例::

    GET api/v2/sessions/C_1492145414740

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "objects":
            [
                {
                    "client_id": "C_1492145414740",
                    "clean_sess": true,
                    "max_inflight": "undefined",
                    "inflight_queue": "undefined",
                    "message_queue": "undefined",
                    "message_dropped": "undefined",
                    "awaiting_rel": "undefined",
                    "awaiting_ack": "undefined",
                    "awaiting_comp": "undefined",
                    "created_at": "2017-04-14 13:35:15"
                }
            ]
        }
    }

-------------------
订阅(Subscriptions)
-------------------

获取某个节点上的订阅列表
------------------------

API定义::

    GET api/v2/nodes/{node_name}/subscriptions

请求参数::

    curr_page={page_no}&page_size={page_size}
 
请求示例::

    GET api/v2/nodes/emq@127.0.0.1/subscriptions?curr_page=1&page_size=20

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "current_page": 1,
            "page_size": 20,
            "total_num": 1,
            "total_page": 1,
            "objects":
            [
                {
                    "client_id": "C_1492145414740",
                    "topic": "$client/C_1492145414740",
                    "qos": 1
                }
            ]
        }
    }

获取节点上指定客户端的订阅信息
------------------------------

API定义::

    GET api/v2/nodes/{node_name}/subscriptions/{clientid}

请求示例::

    GET api/v2/nodes/emq@127.0.0.1/subscriptions/C_1492145414740

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "objects":
            [
                {
                    "client_id": "C_1492145414740",
                    "topic": "$client/C_1492145414740",
                    "qos": 1
                }
            ]
        }
    }

获取集群内指定客户端的订阅信息
------------------------------

API定义::

    GET api/v2/subscriptions/{clientid}

请求示例::

    GET api/v2/subscriptions/C_1492145414740

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "objects":
            [
                {
                    "client_id": "C_1492145414740",
                    "topic": "$client/C_1492145414740",
                    "qos": 1
                }
            ]
        }
    }


------------
路由(Routes)
------------

获取集群路由表
--------------

API定义::

    GET api/v2/routes

请求参数::

    curr_page={page_no}&page_size={page_size}

请求示例::

    GET api/v2/routes

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "current_page": 1,
            "page_size": 20,
            "total_num": 1,
            "total_page": 1,
            "objects":
            [
                {
                    "topic": "$client/C_1492145414740",
                    "node": "emq@127.0.0.1"
                }
            ]
        }
    }

获取集群内指定主题的路由信息
----------------------------

API定义::

    GET api/v2/routes/{topic}

请求示例::

    GET api/v2/routes

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result":
        {
            "objects":
            [
                {
                    "topic": "test_topic",
                    "node": "emq@127.0.0.1"
                }
            ]
        }
    }


----------
发布/订阅
----------

发布消息
--------

API定义::

    POST api/v2/mqtt/publish

请求参数:

.. code-block:: json

    {
        "topic"    : "test",
        "payload"  : "hello",
        "qos"      : 1,
        "retain"   : false,
        "client_id": "C_1492145414740"
    }

.. NOTE:: topic参数必填，其他参数可选。payload默认值空字符串，qos为0，retain为false，client_id为'http'。

请求示例::

    POST api/v2/mqtt/publish

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

创建订阅
--------

API定义::

    POST api/v2/mqtt/subscribe

请求参数:

.. code-block:: json

    {
        "topic"    : "test",
        "qos"      : 1,
        "client_id": "C_1492145414740"
    }

请求示例::

    POST api/v2/mqtt/subscribe

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

取消订阅
--------

API定义::

    POST api/v2/mqtt/unsubscribe

请求参数:

.. code-block:: json

    {
        "topic"    : "test",
        "client_id": "C_1492145414740"
    }

请求示例::

    POST api/v2/mqtt/unsubscribe

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

-------------
插件(Plugins)
-------------

获取节点的插件列表
------------------

API定义::

    GET api/v2/nodes/{node_name}/plugins

请求示例::

    GET api/v2/nodes/emq@127.0.0.1/plugins

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": [
            {
                "name": "emq_auth_clientid",
                "version": "2.3",
                "description": "Authentication with ClientId/Password",
                "active": false
            },
            {
                "name": "emq_auth_http",
                "version": "2.3",
                "description": "Authentication/ACL with HTTP API",
                "active": false
            },
            {
                "name": "emq_auth_jwt",
                "version": "2.3",
                "description": "Authentication with jwt",
                "active": false
            }, 
            {
                "name": "emq_auth_ldap",
                "version": "2.3",
                "description": "Authentication/ACL with LDAP",
                "active": false
            },
            {
                "name": "emq_auth_mongo",
                "version": "2.3",
                "description": "Authentication/ACL with MongoDB",
                "active": false
            },
            {
                "name": "emq_auth_mysql",
                "version": "2.3",
                "description": "Authentication/ACL with MySQL",
                "active": false
            },
            {
                "name": "emq_auth_pgsql",
                "version": "2.3",
                "description": "Authentication/ACL with PostgreSQL",
                "active": false
            },
            {
                "name": "emq_auth_redis",
                "version": "2.3",
                "description": "Authentication/ACL with Redis",
                "active": false
            },
            {
                "name": "emq_auth_username",
                "version": "2.3",
                "description": "Authentication with Username/Password",
                "active": false
            },
            {
                "name": "emq_coap",
                "version": "2.3",
                "description": "CoAP Gateway",
                "active": false
            },
            {
                "name": "emq_dashboard",
                "version": "2.3",
                "description": "EMQ Web Dashboard",
                "active": true
            },
            {
                "name": "emq_lua_hook",
                "version": "2.3",
                "description": "EMQ hooks in lua",
                "active": false
            },
            {
                "name": "emq_lwm2m",
                "version": "0.1",
                "description": "LWM2M Gateway",
                "active": false
            },
            {
                "name": "emq_modules",
                "version": "2.3",
                "description": "EMQ Modules",
                "active": true
            },
            {
                "name": "emq_plugin_template",
                "version": "2.3",
                "description": "EMQ Plugin Template",
                "active": false
            },
            {
                "name": "emq_recon",
                "version": "2.3",
                "description": "Recon Plugin",
                "active": true
            },
            {
                "name": "emq_reloader",
                "version": "2.3",
                "description": "Reloader Plugin",
                "active": false
            },
            {
                "name": "emq_retainer",
                "version": "2.3",
                "description": "EMQ Retainer",
                "active": true
            },
            {
                "name": "emq_sn",
                "version": "2.3",
                "description": "MQTT-SN Gateway",
                "active": false
            },
            {
                "name": "emq_stomp",
                "version": "2.3",
                "description": "Stomp Protocol Plugin",
                "active": false
            },
            {
                "name": "emq_web_hook",
                "version": "2.3",
                "description": "EMQ Webhook Plugin",
                "active": false
            }
        ]
    }


开启/关闭节点的指定插件
-----------------------

API定义::

    PUT /api/v2/nodes/{node_name}/plugins/{name}

请求参数::

    {"active": true | false}

请求示例::

    PUT api/v2/nodes/emq@127.0.0.1/plugins/emqx_recon

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

------------------
监听器(Listeners)
------------------

获取集群节点的监听器列表
------------------------

API定义::

    GET api/v2/monitoring/listeners

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": {
            "emq@127.0.0.1": [
                {
                    "protocol": "dashboard:http",
                    "listen": "18083",
                    "acceptors": 2,
                    "max_clients": 512,
                    "current_clients": 0,
                    "shutdown_count": []
                },
                {
                    "protocol": "mqtt:tcp",
                    "listen": "127.0.0.1:11883",
                    "acceptors": 16,
                    "max_clients": 102400,
                    "current_clients": 0,
                    "shutdown_count": []
                },
                {
                    "protocol": "mqtt:tcp",
                    "listen": "0.0.0.0:1883",
                    "acceptors": 16,
                    "max_clients": 102400,
                    "current_clients": 0,
                    "shutdown_count": []
                },
                {
                    "protocol": "mqtt:ws",
                    "listen": "8083",
                    "acceptors": 4,
                    "max_clients": 64,
                    "current_clients": 0,
                    "shutdown_count": []
                },
                {
                    "protocol": "mqtt:ssl",
                    "listen": "8883",
                    "acceptors": 16,
                    "max_clients": 1024,
                    "current_clients": 0,
                    "shutdown_count": []
                },
                {
                    "protocol": "mqtt:wss",
                    "listen": "8084",
                    "acceptors": 4,
                    "max_clients": 64,
                    "current_clients": 0,
                    "shutdown_count": []
                },
                {
                    "protocol": "mqtt:api",
                    "listen": "127.0.0.1:8080",
                    "acceptors": 4,
                    "max_clients": 64,
                    "current_clients": 1,
                    "shutdown_count": []
                }
            ]
        }
    }

获取指定节点的监听器列表
------------------------

API定义::

    GET api/v2/monitoring/listeners/{node_name}

请求示例::

    GET api/v2/monitoring/listeners/emq@127.0.0.1

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": [
            {
                "protocol": "mqtt:api",
                "listen": "127.0.0.1:8080",
                "acceptors": 4,
                "max_clients": 64,
                "current_clients": 1,
                "shutdown_count": []
            },
            {
                "protocol": "mqtt:wss",
                "listen": "8084",
                "acceptors": 4,
                "max_clients": 64,
                "current_clients": 0,
                "shutdown_count": []
            },
            {
                "protocol": "mqtt:ssl",
                "listen": "8883",
                "acceptors": 16,
                "max_clients": 1024,
                "current_clients": 0,
                "shutdown_count": []
            },
            {
                "protocol": "mqtt:ws",
                "listen": "8083",
                "acceptors": 4,
                "max_clients": 64,
                "current_clients": 0,
                "shutdown_count": []
            },
            {
                "protocol": "mqtt:tcp",
                "listen": "0.0.0.0:1883",
                "acceptors": 16,
                "max_clients": 102400,
                "current_clients": 0,
                "shutdown_count": []
            },
            {
                "protocol": "mqtt:tcp",
                "listen": "127.0.0.1:11883",
                "acceptors": 16,
                "max_clients": 102400,
                "current_clients": 0,
                "shutdown_count": []
            },
            {
                "protocol": "dashboard:http",
                "listen": "18083",
                "acceptors": 2,
                "max_clients": 512,
                "current_clients": 0,
                "shutdown_count": []
            }
        ]
    }

------------
收发报文统计
------------

获取全部节点的收发报文统计
--------------------------

API定义::

    GET api/v2/monitoring/metrics/

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": {
            "emq@127.0.0.1":
            {
                "packets/disconnect":0,
                "messages/dropped":0,
                "messages/qos2/received":0,
                "packets/suback":0,
                "packets/pubcomp/received":0,
                "packets/unsuback":0,
                "packets/pingresp":0,
                "packets/puback/missed":0,
                "packets/pingreq":0,
                "messages/retained":3,
                "packets/sent":0,
                "messages/qos2/dropped":0,
                "packets/unsubscribe":0,
                "packets/pubrec/missed":0,
                "packets/connack":0,
                "messages/received":0,
                "packets/pubrec/sent":0,
                "packets/publish/received":0,
                "packets/pubcomp/sent":0,
                "bytes/received":0,
                "packets/connect":0,
                "packets/puback/received":0,
                "messages/sent":0,
                "packets/publish/sent":0,
                "bytes/sent":0,
                "packets/pubrel/missed":0,
                "packets/puback/sent":0,
                "messages/qos0/received":0,
                "packets/subscribe":0,
                "packets/pubrel/sent":0,
                "messages/qos2/sent":0,
                "packets/received":0,
                "packets/pubrel/received":0,
                "messages/qos1/received":0,
                "messages/qos1/sent":0,
                "packets/pubrec/received":0,
                "packets/pubcomp/missed":0,
                "messages/qos0/sent":0
            }
        }
    }

获取指定节点的收发报文统计
--------------------------

API定义::

    GET api/v2/monitoring/metrics/{node_name}

请求示例::

    GET api/v2/monitoring/metrics/emq@127.0.0.1

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": {
            "packets/disconnect":0,
            "messages/dropped":0,
            "messages/qos2/received":0,
            "packets/suback":0,
            "packets/pubcomp/received":0,
            "packets/unsuback":0,
            "packets/pingresp":0,
            "packets/puback/missed":0,
            "packets/pingreq":0,
            "messages/retained":3,
            "packets/sent":0,
            "messages/qos2/dropped":0,
            "packets/unsubscribe":0,
            "packets/pubrec/missed":0,
            "packets/connack":0,
            "messages/received":0,
            "packets/pubrec/sent":0,
            "packets/publish/received":0,
            "packets/pubcomp/sent":0,
            "bytes/received":0,
            "packets/connect":0,
            "packets/puback/received":0,
            "messages/sent":0,
            "packets/publish/sent":0,
            "bytes/sent":0,
            "packets/pubrel/missed":0,
            "packets/puback/sent":0,
            "messages/qos0/received":0,
            "packets/subscribe":0,
            "packets/pubrel/sent":0,
            "messages/qos2/sent":0,
            "packets/received":0,
            "packets/pubrel/received":0,
            "messages/qos1/received":0,
            "messages/qos1/sent":0,
            "packets/pubrec/received":0,
            "packets/pubcomp/missed":0,
            "messages/qos0/sent":0
        }
    }

-------------
连接会话统计
-------------

获取全部节点的连接会话统计
---------------------------

API定义::

    GET api/v2/monitoring/stats

请求示例::

    GET api/v2/monitoring/stats

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": {
            "emq@127.0.0.1":
            {
                "clients/count":0,
                "clients/max":0,
                "retained/count":0,
                "retained/max":0,
                "routes/count":0,
                "routes/max":0,
                "sessions/count":0,
                "sessions/max":0,
                "subscribers/count":0,
                "subscribers/max":0,
                "subscriptions/count":0,
                "subscriptions/max":0,
                "topics/count":0,
                "topics/max":0
            }
        }
    }

获取指定节点的连接会话统计
--------------------------

API定义::

    GET api/v2/monitoring/stats/{node_name}

请求示例::

    GET api/v2/monitoring/stats/emq@127.0.0.1

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": {
            "clients/count":0,
            "clients/max":0,
            "retained/count":0,
            "retained/max":0,
            "routes/count":0,
            "routes/max":0,
            "sessions/count":0,
            "sessions/max":0,
            "subscribers/count":0,
            "subscribers/max":0,
            "subscriptions/count":0,
            "subscriptions/max":0,
            "topics/count":0,
            "topics/max":0
        }
    }

----------
返回错误码
----------

+-------+-----------------------------------------+
| 错误码| 备注                                    |
+=======+=========================================+
| 0     | 成功                                    |
+-------+-----------------------------------------+
| 101   | badrpc                                  |
+-------+-----------------------------------------+
| 102   | 未知错误                                |
+-------+-----------------------------------------+
| 103   | 用户名密码错误                          |
+-------+-----------------------------------------+
| 104   | 用户名密码不能为空                      |
+-------+-----------------------------------------+
| 105   | 删除的用户不存在                        |
+-------+-----------------------------------------+
| 106   | admin用户不能删除                       |
+-------+-----------------------------------------+
| 107   | 请求参数缺失                            |
+-------+-----------------------------------------+
| 108   | 请求参数类型错误                        |
+-------+-----------------------------------------+
| 109   | 请求参数不是json类型                    |
+-------+-----------------------------------------+
| 110   | 插件已经加载，不能重复加载              |
+-------+-----------------------------------------+
| 111   | 插件已经卸载，不能重复卸载              |
+-------+-----------------------------------------+
| 112   | 用户不在线                              |
+-------+-----------------------------------------+
| 113   | 新密码和确认密码不一致                  |
+-------+-----------------------------------------+
