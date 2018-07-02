
.. _rest_api:

======================
管理监控API (REST API)
======================

用户可以通过 REST API 查询 MQTT 客户端连接(Clients)、会话(Sessions)、订阅(Subscriptions)和路由(Routes)信息，还可以检索和监控服务器的性能指标和统计数据。

--------
URL 地址
--------

REST API 访问 URL 地址::

    http(s)://host:8080/api/v2/

----------
Basic 认证
----------

REST API 采用 HTTP Basic 认证(Authentication):

.. code-block:: bash

    curl -v --basic -u <user>:<passwd> -k http://localhost:8080/api/v2/nodes/emq@127.0.0.1/clients

----------
集群与节点
----------

获取全部节点的基本信息
----------------------

API 定义::

    GET api/v2/management/nodes

请求示例::

    GET api/v2/management/nodes

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": [
    		{
    			"name": "emq@127.0.0.1",
    			"version": "2.3.10",
    			"sysdescr": "Erlang MQTT Broker",
    			"uptime": "3 minutes, 32 seconds",
    			"datetime": "2018-06-29 09:03:52",
    			"otp_release": "R20/9.3.3",
    			"node_status": "Running"
    		}
    	]
    }

获取指定节点的基本信息
----------------------

API 定义::

    GET api/v2/management/nodes/{node_name}

请求示例::

    GET api/v2/management/nodes/emq@127.0.0.1

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"version": "2.3.10",
    		"sysdescr": "Erlang MQTT Broker",
    		"uptime": "5 minutes, 12 seconds",
    		"datetime": "2018-06-29 09:05:32",
    		"otp_release": "R20/9.3.3",
    		"node_status": "Running"
    	}
    }

获取全部节点的监控数据
----------------------

API 定义::

    GET api/v2/monitoring/nodes

请求示例::

    GET api/v2/monitoring/nodes

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": [
    		{
    			"name": "emq@127.0.0.1",
    			"otp_release": "R20/9.3.3",
    			"memory_total": "72.94M",
    			"memory_used": "50.55M",
    			"process_available": 262144,
    			"process_used": 324,
    			"max_fds": 7168,
    			"clients": 0,
    			"node_status": "Running",
    			"load1": "1.65",
    			"load5": "1.93",
    			"load15": "2.01"
    		}
    	]
    }

获取指定节点的监控数据
----------------------

API 定义::

    GET api/v2/monitoring/nodes/{node_name}

请求示例::

    GET api/v2/monitoring/nodes/emq@127.0.0.1

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"name": "emq@127.0.0.1",
    		"otp_release": "R20/9.3.3",
    		"memory_total": "73.69M",
    		"memory_used": "50.12M",
    		"process_available": 262144,
    		"process_used": 324,
    		"max_fds": 7168,
    		"clients": 0,
    		"node_status": "Running",
    		"load1": "1.88",
    		"load5": "1.99",
    		"load15": "2.02"
    	}
    }

-------------------
客户端连接(Clients)
-------------------

获取指定节点的客户端连接列表
----------------------------

API 定义::

    GET api/v2/nodes/{node_name}/clients
 
请求参数:: 

    curr_page={page_no}&page_size={page_size}

请求示例::

    api/v2/nodes/emq@127.0.0.1/clients?curr_page=1&page_size=20

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"current_page": 1,
    		"page_size": 20,
    		"total_num": 1,
    		"total_page": 1,
    		"objects": [
    			{
    				"client_id": "mqttjs_722b4d845f",
    				"username": "undefined",
    				"ipaddress": "127.0.0.1",
    				"port": 58459,
    				"clean_sess": true,
    				"proto_ver": 4,
    				"keepalive": 60,
    				"connected_at": "2018-06-29 09:15:25"
    			}
    		]
    	}
    }

获取节点指定客户端连接的信息
----------------------------

API 定义::

    GET api/v2/nodes/{node_name}/clients/{clientid}
 
请求示例::

    GET api/v2/nodes/emq@127.0.0.1/clients/mqttjs_722b4d845f

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"objects": [
    			{
    				"client_id": "mqttjs_722b4d845f",
    				"username": "undefined",
    				"ipaddress": "127.0.0.1",
    				"port": 58459,
    				"clean_sess": true,
    				"proto_ver": 4,
    				"keepalive": 60,
    				"connected_at": "2018-06-29 09:15:25"
    			}
    		]
    	}
    }

获取集群内指定客户端的信息
--------------------------

API 定义::

    GET api/v2/clients/{clientid}
 
请求示例::

    GET api/v2/clients/mqttjs_722b4d845f

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"objects": [
    			{
    				"client_id": "mqttjs_722b4d845f",
    				"username": "undefined",
    				"ipaddress": "127.0.0.1",
    				"port": 58459,
    				"clean_sess": true,
    				"proto_ver": 4,
    				"keepalive": 60,
    				"connected_at": "2018-06-29 09:15:25"
    			}
    		]
    	}
    }


断开集群内指定客户端连接
------------------

API定义::

    DELETE api/v2/clients/{clientid}

请求示例::

    DELETE api/v2/clients/mqttjs_722b4d845f

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

清除集群内指定客户端的ACL缓存
--------------------------

API定义::

    PUT api/v2/clients/{clientid}/clean_acl_cache

请求参数:

.. code-block:: json

    {
        "topic": "test"
    }

请求示例::

    PUT api/v2/clients/mqttjs_722b4d845f/clean_acl_cache
    
    请求的 json 参数:
    {
        "topic": "test"
    }

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

--------------
会话(Sessions)
--------------

获取指定节点的会话列表
----------------------

API 定义::

    GET api/v2/nodes/{node_name}/sessions
 
请求参数::

    curr_page={page_no}&page_size={page_size}

请求示例::

    GET api/v2/nodes/emq@127.0.0.1/sessions?curr_page=1&page_size=20

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"current_page": 1,
    		"page_size": 20,
    		"total_num": 1,
    		"total_page": 1,
    		"objects": [
    			{
    				"client_id": "mqttjs_722b4d845f",
    				"clean_sess": true,
    				"subscriptions": 0,
    				"max_inflight": 32,
    				"inflight_len": 0,
    				"mqueue_len": 0,
    				"mqueue_dropped": 0,
    				"awaiting_rel_len": 0,
    				"deliver_msg": 0,
    				"enqueue_msg": 0,
    				"created_at": "2018-06-29 10:05:13"
    			}
    		]
    	}
    }

获取节点上指定客户端的会话信息
------------------------------

API 定义::

    GET api/v2/nodes/{node_name}/sessions/{clientid}
 
请求示例::

    GET api/v2/nodes/emq@127.0.0.1/sessions/mqttjs_722b4d845f

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"objects": [
    			{
    				"client_id": "mqttjs_722b4d845f",
    				"clean_sess": true,
    				"subscriptions": 0,
    				"max_inflight": 32,
    				"inflight_len": 0,
    				"mqueue_len": 0,
    				"mqueue_dropped": 0,
    				"awaiting_rel_len": 0,
    				"deliver_msg": 0,
    				"enqueue_msg": 0,
    				"created_at": "2018-06-29 10:05:13"
    			}
    		]
    	}
    }

获取集群内指定客户端的会话信息
------------------------------

API 定义::

    GET api/v2/sessions/{clientid}
 
请求示例::

    GET api/v2/sessions/mqttjs_722b4d845f

返回数据:

.. code-block:: json
    {
    	"code": 0,
    	"result": {
    		"objects": [
    			{
    				"client_id": "mqttjs_722b4d845f",
    				"clean_sess": true,
    				"subscriptions": 0,
    				"max_inflight": 32,
    				"inflight_len": 0,
    				"mqueue_len": 0,
    				"mqueue_dropped": 0,
    				"awaiting_rel_len": 0,
    				"deliver_msg": 0,
    				"enqueue_msg": 0,
    				"created_at": "2018-06-29 10:05:13"
    			}
    		]
    	}
    }

-------------------
订阅(Subscriptions)
-------------------

获取某个节点上的订阅列表
------------------------

API 定义::

    GET api/v2/nodes/{node_name}/subscriptions

请求参数::

    curr_page={page_no}&page_size={page_size}
 
请求示例::

    GET api/v2/nodes/emq@127.0.0.1/subscriptions?curr_page=1&page_size=20

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"current_page": 1,
    		"page_size": 20,
    		"total_num": 1,
    		"total_page": 1,
    		"objects": [
    			{
    				"client_id": "mqttjs_722b4d845f",
    				"topic": "/World",
    				"qos": 0
    			}
    		]
    	}
    }

获取节点上指定客户端的订阅信息
------------------------------

API 定义::

    GET api/v2/nodes/{node_name}/subscriptions/{clientid}

请求示例::

    GET api/v2/nodes/emq@127.0.0.1/subscriptions/mqttjs_722b4d845f

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"objects": [
    			{
    				"client_id": "mqttjs_722b4d845f",
    				"topic": "/World",
    				"qos": 0
    			}
    		]
    	}
    }

获取集群内指定客户端的订阅信息
------------------------------

API 定义::

    GET api/v2/subscriptions/{clientid}

请求示例::

    GET api/v2/subscriptions/mqttjs_722b4d845f

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"objects": [
    			{
    				"client_id": "mqttjs_722b4d845f",
    				"topic": "/World",
    				"qos": 0
    			}
    		]
    	}
    }

------------
路由(Routes)
------------

获取集群路由表
--------------

API 定义::

    GET api/v2/routes

请求参数::

    curr_page={page_no}&page_size={page_size}

请求示例::

    GET api/v2/routes?curr_page=1&page_size=20

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"current_page": 1,
    		"page_size": 20,
    		"total_num": 1,
    		"total_page": 1,
    		"objects": [
    			{
    				"topic": "/World",
    				"node": "emq@127.0.0.1"
    			}
    		]
    	}
    }

获取集群内指定主题的路由信息
----------------------------

API 定义::

    GET api/v2/routes/{topic}

请求示例::

    GET api/v2/routes//World

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": {
    		"objects": [
    			{
    				"topic": "/World",
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

API 定义::

    POST api/v2/mqtt/publish

请求参数:

.. code-block:: json

    {
    	"topic" : "/World",
    	"payload": "hello",
    	"qos": 0,
    	"retain" : false,
    	"client_id": "mqttjs_722b4d845f"
    }

.. NOTE:: topic 参数必填，其他参数可选。payload 默认值空字符串，qos 默认为 0，retain 默认为 false，client_id 默认为 'http'。

请求示例::

    POST api/v2/mqtt/publish

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

    {
        "code": 0,
        "result": []
    }

创建订阅
--------

API 定义::

    POST api/v2/mqtt/subscribe

请求参数:

.. code-block:: json

    {
        "topic"    : "/World",
        "qos"      : 0,
        "client_id": "mqttjs_722b4d845f"
    }

请求示例::

    POST api/v2/mqtt/subscribe
    请求参数 json:
    {
	      "topic" : "/World",
	      "qos": 0,
    	  "client_id": "mqttjs_722b4d845f"
    }

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

取消订阅
--------

API 定义::

    POST api/v2/mqtt/unsubscribe

请求参数:

.. code-block:: json

    {
	      "topic" : "/World",
    	  "client_id": "mqttjs_722b4d845f"
    }

请求示例::

    POST api/v2/mqtt/unsubscribe
    请求参数 json:
    {
	      "topic" : "/World",
    	  "client_id": "mqttjs_722b4d845f"
    }


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

API 定义::

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
    			"version": "2.3.10",
    			"description": "Authentication with ClientId/Password",
    			"active": false
    		},
    		{
    			"name": "emq_auth_http",
    			"version": "2.3.10",
    			"description": "Authentication/ACL with HTTP API",
    			"active": false
    		},
    		{
    			"name": "emq_auth_jwt",
    			"version": "2.3.10",
    			"description": "Authentication with JWT",
    			"active": false
    		},
    		{
    			"name": "emq_auth_ldap",
    			"version": "2.3.10",
    			"description": "Authentication/ACL with LDAP",
    			"active": false
    		},
    		{
    			"name": "emq_auth_mongo",
    			"version": "2.3.10",
    			"description": "Authentication/ACL with MongoDB",
    			"active": false
    		},
    		{
    			"name": "emq_auth_mysql",
    			"version": "2.3.10",
    			"description": "Authentication/ACL with MySQL",
    			"active": false
    		},
    		{
    			"name": "emq_auth_pgsql",
    			"version": "2.3.10",
    			"description": "Authentication/ACL with PostgreSQL",
    			"active": false
    		},
    		{
    			"name": "emq_auth_redis",
    			"version": "2.3.10",
    			"description": "Authentication/ACL with Redis",
    			"active": false
    		},
    		{
    			"name": "emq_auth_username",
    			"version": "2.3.10",
    			"description": "Authentication with Username/Password",
    			"active": false
    		},
    		{
    			"name": "emq_coap",
    			"version": "2.3.10",
    			"description": "CoAP Gateway",
    			"active": false
    		},
    		{
    			"name": "emq_dashboard",
    			"version": "2.3.10",
    			"description": "EMQ Web Dashboard",
    			"active": true
    		},
    		{
    			"name": "emq_lua_hook",
    			"version": "2.3.10",
    			"description": "EMQ Hooks in lua",
    			"active": false
    		},
    		{
    			"name": "emq_modules",
    			"version": "2.3.10",
    			"description": "EMQ Modules",
    			"active": true
    		},
    		{
    			"name": "emq_plugin_template",
    			"version": "2.3.10",
    			"description": "EMQ Plugin Template",
    			"active": false
    		},
    		{
    			"name": "emq_recon",
    			"version": "2.3.10",
    			"description": "Recon Plugin",
    			"active": true
    		},
    		{
    			"name": "emq_reloader",
    			"version": "2.3.10",
    			"description": "Reloader Plugin",
    			"active": false
    		},
    		{
    			"name": "emq_retainer",
    			"version": "2.3.10",
    			"description": "EMQ Retainer",
    			"active": true
    		},
    		{
    			"name": "emq_sn",
    			"version": "2.3.10",
    			"description": "MQTT-SN Gateway",
    			"active": false
    		},
    		{
    			"name": "emq_stomp",
    			"version": "2.3.10",
    			"description": "Stomp Protocol Plugin",
    			"active": false
    		},
    		{
    			"name": "emq_web_hook",
    			"version": "2.3.10",
    			"description": "EMQ Webhook Plugin",
    			"active": false
    		}
    	]
    }

开启/关闭节点的指定插件
-----------------------

API 定义::

    PUT /api/v2/nodes/{node_name}/plugins/{name}

请求参数::

    {"active": true | false}

请求示例::

    PUT api/v2/nodes/emq@127.0.0.1/plugins/emq_recon
    json请求参数:
    {
    	"active": true
    }

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

API 定义::

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

API 定义::

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

API 定义::

    GET api/v2/monitoring/metrics/

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

获取指定节点的收发报文统计
--------------------------

API 定义::

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

API 定义::

    GET api/v2/monitoring/stats

请求示例::

    GET api/v2/monitoring/stats

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": [
    		{
    			"emq@127.0.0.1": {
    				"clients/count": 0,
    				"clients/max": 0,
    				"retained/count": 3,
    				"retained/max": 3,
    				"routes/count": 0,
    				"routes/max": 0,
    				"sessions/count": 0,
    				"sessions/max": 0,
    				"subscribers/count": 0,
    				"subscribers/max": 0,
    				"subscriptions/count": 0,
    				"subscriptions/max": 0,
    				"topics/count": 0,
    				"topics/max": 0
    			}
    		}
    	]
    }

获取指定节点的连接会话统计
--------------------------

API 定义::

    GET api/v2/monitoring/stats/{node_name}

请求示例::

    GET api/v2/monitoring/stats/emq@127.0.0.1

返回数据:

.. code-block:: json

   {
   	 "code": 0,
   	 "result": {
       "clients/count": 0,
       "clients/max": 0,
       "retained/count": 3,
       "retained/max": 3,
       "routes/count": 0,
       "routes/max": 0,
       "sessions/count": 0,
       "sessions/max": 0,
       "subscribers/count": 0,
       "subscribers/max": 0,
       "subscriptions/count": 0,
       "subscriptions/max": 0,
       "topics/count": 0,
       "topics/max": 0
   	 }
   }

------
热配置
------

获取全部节点的可修改配置项
--------------------------

API定义::

    GET api/v2/configs

请求示例::

    GET api/v2/configs

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": {
            "emq@127.0.0.1": [
                {
                    "key": "log.console.level",
                    "value": "error",
                    "datatpye": "enum",
                    "app": "emqttd"
                },
                {
                    "key": "mqtt.acl_file",
                    "value": "etc/acl.conf",
                    "datatpye": "string",
                    "app": "emqttd"
                },
                {
                    "key": "mqtt.acl_nomatch",
                    "value": "allow",
                    "datatpye": "enum",
                    "app": "emqttd"
                },
                {
                    "key": "mqtt.allow_anonymous",
                    "value": "true",
                    "datatpye": "enum",
                    "app": "emqttd"
                },
                {
                    "key": "mqtt.broker.sys_interval",
                    "value": "60",
                    "datatpye": "integer",
                    "app": "emqttd"
                },
                {
                    "key": "mqtt.cache_acl",
                    "value": "true",
                    "datatpye": "enum",
                    "app": "emqttd"
                }
            ]
        }
    }

获取指定节点的可修改配置项
--------------------------

API定义::

    GET api/v2/nodes/{node_name}/configs

请求示例::

    GET api/v2/nodes/emq@127.0.0.1/configs

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": [
            {
                "key": "log.console.level",
                "value": "error",
                "datatpye": "enum",
                "app": "emqttd"
            },
            {
                "key": "mqtt.acl_file",
                "value": "etc/acl.conf",
                "datatpye": "string",
                "app": "emqttd"
            },
            {
                "key": "mqtt.acl_nomatch",
                "value": "allow",
                "datatpye": "enum",
                "app": "emqttd"
            },
            {
                "key": "mqtt.allow_anonymous",
                "value": "true",
                "datatpye": "enum",
                "app": "emqttd"
            },
            {
                "key": "mqtt.broker.sys_interval",
                "value": "60",
                "datatpye": "integer",
                "app": "emqttd"
            },
            {
                "key": "mqtt.cache_acl",
                "value": "true",
                "datatpye": "enum",
                "app": "emqttd"
            }
        ]
    }

修改全部节点的配置项
--------------------

API定义::

    PUT /api/v2/configs/{app_name}

请求参数::

    {
        "key"   : "mqtt.allow_anonymous",
        "value" : "false"
    }

请求示例::

    PUT /api/v2/configs/emqttd

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

修改指定节点的配置项
--------------------

API定义::

    PUT /api/v2/nodes/{node_name}/configs/{app_name}

请求参数::

    {
        "key"   : "mqtt.allow_anonymous",
        "value" : "false"
     }

请求示例::

    PUT /api/v2/nodes/emq@127.0.0.1/configs/emqttd

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

获取指定节点的指定插件的配置项
--------------------------

API定义::

    GET api/v2/nodes/{node_name}/plugin_configs/{plugin_name}

请求示例::

    GET api/v2/nodes/emq@127.0.0.1/plugin_configs/emq_auth_http

返回数据:

.. code-block:: json

    {
    	"code": 0,
    	"result": [
    		{
    			"key": "auth.http.auth_req",
    			"value": "http://127.0.0.1:8080/mqtt/auth",
    			"desc": "",
    			"required": true
    		},
    		{
    			"key": "auth.http.auth_req.method",
    			"value": "post",
    			"desc": "",
    			"required": true
    		},
    		{
    			"key": "auth.http.auth_req.params",
    			"value": "clientid=%c,username=%u,password=%P",
    			"desc": "",
    			"required": true
    		},
    		{
    			"key": "auth.http.super_req",
    			"value": "http://127.0.0.1:8080/mqtt/superuser",
    			"desc": "",
    			"required": true
    		},
    		{
    			"key": "auth.http.super_req.method",
    			"value": "post",
    			"desc": "",
    			"required": true
    		},
    		{
    			"key": "auth.http.super_req.params",
    			"value": "clientid=%c,username=%u",
    			"desc": "",
    			"required": true
    		},
    		{
    			"key": "auth.http.acl_req",
    			"value": "http://127.0.0.1:8080/mqtt/acl",
    			"desc": "",
    			"required": true
    		},
    		{
    			"key": "auth.http.acl_req.method",
    			"value": "get",
    			"desc": "",
    			"required": true
    		},
    		{
    			"key": "auth.http.acl_req.params",
    			"value": "access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t",
    			"desc": "",
    			"required": true
    		}
    	]
    }

修改指定节点的指定插件的配置项
--------------------------

API定义::

    PUT api/v2/nodes/{node_name}/plugin_configs/{plugin_name}

请求参数::

    {
        "auth.http.auth_req.method": "get",
        "auth.http.auth_req": "http://127.0.0.1:8080/mqtt/auth",
        "auth.http.auth_req.params": "clientid=%c,username=%u,password=%P",
        "auth.http.acl_req.method": "get",
        "auth.http.acl_req": "http://127.0.0.1:8080/mqtt/acl",
        "auth.http.acl_req.params": "access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t",
        "auth.http.super_req.method": "post",
        "auth.http.super_req.params": "clientid=%c,username=%u",
        "auth.http.super_req": "http://127.0.0.1:8080/mqtt/superuser"
    }

请求示例::

    PUT api/v2/nodes/emq@127.0.0.1/plugin_configs/emq_auth_http

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

--------
用户管理
--------

获取管理用户列表
--------------

API定义::

    GET api/v2/users

请求示例::

    GET api/v2/users

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": [
            {
                "username": "admin",
                "tags": "administrator"
            }
        ]
    }

添加管理用户
----------

API定义::

    POST api/v2/users

请求参数::

    {
        "username": "test_user",
        "password": "password",
        "tags": "user"
    }

请求示例::

    POST api/v2/users

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

修改管理用户信息
--------------

API定义::

    PUT api/v2/users/{username}

请求参数::

    {
        "tags": "admin"
    }

请求示例::

    PUT api/v2/users/test_user

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

删除管理用户
-----------

API定义::

    DELETE api/v2/users/{username}

请求参数::


请求示例::

    DELETE api/v2/users/test_user

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

认证管理用户
-----------

API定义::

    POST api/v2/auth

请求参数::

    {
        "username": "test_user",
        "password": "password"
    }

请求示例::

    POST api/v2/auth

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
    }

修改管理用户密码
--------------

API定义::

    PUT api/v2/change_pwd/{username}

请求参数::

    {
        "new_pwd": "newpassword",
        "old_pwd": "password"
    }

请求示例::

    PUT api/v2/change_pwd/test_user

返回数据:

.. code-block:: json

    {
        "code": 0,
        "result": []
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
| 113   | 用户已经存在                            |
+-------+-----------------------------------------+
| 114   | 旧密码错误                              |
+-------+-----------------------------------------+
