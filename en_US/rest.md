# REST API

The REST API allows you to query MQTT clients, sessions, subscriptions, and routes. You can also query and monitor the metrics and statistics of the broker.

## Base URL

All REST APIs in the documentation have the following base URL:

    http(s)://host:8080/api/v2/

## Basic Authentication

The HTTP requests to the REST API are protected with HTTP Basic authentication, For example:

    curl -v --basic -u <user>:<passwd> -k http://localhost:8080/api/v2/nodes/emq@127.0.0.1/clients

## Nodes

### List all Nodes in the Cluster

Definition:

    GET api/v2/management/nodes

Example Request:

    GET api/v2/management/nodes

Response:

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

### Retrieve a Node's Info

Definition:

    GET api/v2/management/nodes/{node_name}

Example Request:

    GET api/v2/management/nodes/emq@127.0.0.1

Response:

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

### List all Nodes'statistics in the Cluster

Definition:

    GET api/v2/monitoring/nodes

Example Request:

    GET api/v2/monitoring/nodes

Response:

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

Retrieve a node's statistics ---------------------------

Definition:

    GET api/v2/monitoring/nodes/{node_name}

Example Request:

    GET api/v2/monitoring/nodes/emq@127.0.0.1

Response:

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

## Clients

### List all Clients on a Node

Definition:

    GET api/v2/nodes/{node_name}/clients

Request Parameter:

    curr_page={page_no}&page_size={page_size}

Example Request:

    api/v2/nodes/emq@127.0.0.1/clients?curr_page=1&page_size=20

Response:

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

Retrieve a Client on a Node --------------------------

Definition:

    GET api/v2/nodes/{node_name}/clients/{client_id}

Example Request:

    GET api/v2/nodes/emq@127.0.0.1/clients/mqttjs_722b4d845f

Response:

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

Retrieve a Client in the Cluster -------------------------------

Definition:

    GET api/v2/clients/{client_id}

Example Request:

    GET api/v2/clients/mqttjs_722b4d845f

Response:

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

### Disconnect a Specified Client in the Cluster

Definition:

    DELETE api/v2/clients/{clientid}

Example Request:

    DELETE api/v2/clients/mqttjs_722b4d845f

Response:

    {
        "code": 0,
        "result": []
    }

### Clear the ACL of a Specified Client in the Cluster

Definition:

    PUT api/v2/clients/{clientid}/clean_acl_cache

Request Parameter:

    {
        "topic": "test"
    }

Request Example:

    PUT api/v2/clients/C_1492145414740/clean_acl_cache

    Request Json Parameter:
    {
        "topic": "test"
    }

Response:

    {
        "code": 0,
        "result": []
    }

## Sessions

### List all Sessions on a Node

Definition:

    GET api/v2/node/{node_name}/sessions

Request Parameter:

    curr_page={page_no}&page_size={page_size}

Example Request:

    GET api/v2/nodes/emq@127.0.0.1/sessions?curr_page=1&page_size=20

Response:

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

### Retrieve a Session on a Node

Definition:

    GET api/v2/nodes/{node_name}/sessions/{client_id}

Example Request:

    GET api/v2/nodes/emq@127.0.0.1/sessions/mqttjs_722b4d845f

Response:

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

Retrieve a Session in the Cluster --------------------------------

Definition:

    GET api/v2/sessions/{client_id}

Example Request:

    GET api/v2/sessions/mqttjs_722b4d845f

Response:

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

## Subscriptions

### List all Subscriptions of a Node

Definition:

    GET api/v2/nodes/{node_name}/subscriptions

Request parameters:

    curr_page={page_no}&page_size={page_size}

Example Request:

    GET api/v2/nodes/emq@127.0.0.1/subscriptions?curr_page=1&page_size=20

Response:

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

List Subscriptions of a Client on a node ------------------------------

Definition:

    GET api/v2/nodes/{node_name}/subscriptions/{clientid}

Example Request:

    GET api/v2/nodes/emq@127.0.0.1/subscriptions/mqttjs_722b4d845f

Response:

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

### List Subscriptions of a Client in cluster

Definition:

    GET api/v2/subscriptions/{clientid}

Example Request:

    GET api/v2/subscriptions/mqttjs_722b4d845f

Response:

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

## Routes

### List all Routes in the Cluster

Definition:

    GET api/v2/routes

Request parameters:

    curr_page={page_no}&page_size={page_size}

Example Request:

    GET api/v2/routes?curr_page=1&page_size=20

Response:

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

Retrieve a Route of Topic in the Cluster -------------------------------

Definition:

    GET api/v2/routes/{topic}

Example Request:

    GET api/v2/routes//World

Response:

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

## Publish/Subscribe

### Publish Message

Definition:

    POST api/v2/mqtt/publish

Request parameters:

    {
        "topic" : "/World",
        "payload": "hello",
        "qos": 0,
        "retain" : false,
        "client_id": "mqttjs_722b4d845f"
    }

::: tip Tip
The topic parameter is required, other parameters are optional. Payload defaults to empty string, qos defaults to 0, retain defaults to false, client_id defaults to 'http'.
:::

Example Request:

    POST api/v2/mqtt/publish

    Request Json Parameter:
    {
          "topic" : "/World",
        "payload": "hello",
          "qos": 0,
          "retain" : false,
          "client_id": "mqttjs_722b4d845f"
    }

Response:

    {
        "code": 0,
        "result": []
    }

### Create a Subscription

Definition:

    POST api/v2/mqtt/subscribe

Request parameters:

    {
        "topic": "/World",
        "qos": 0,
        "client_id": "mqttjs_722b4d845f"
    }

Example Request:

    POST api/v2/mqtt/subscribe
    Request Json Parameter:
    {
          "topic" : "/World",
          "qos": 0,
          "client_id": "mqttjs_722b4d845f"
    }

Response:

    {
        "code": 0,
        "result": []
    }

Unsubscribe Topic ------------

Definition:

    POST api/v2/mqtt/unsubscribe

Request Parameter:

    {
          "topic" : "/World",
          "client_id": "mqttjs_722b4d845f"
    }

Example Request:

    POST api/v2/mqtt/unsubscribe
    Request Json Parameter:
    {
          "topic" : "/World",
          "client_id": "mqttjs_722b4d845f"
    }

Response:

    {
        "code": 0,
        "result": []
    }

## Plugins

### List all Plugins of a Node

Definition:

    GET /api/v2/nodes/{node_name}/plugins/

Example Request:

    GET api/v2/nodes/emq@127.0.0.1/plugins

Response:

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

### Start/Stop a Plugin

Definition:

    PUT /api/v2/nodes/{node_name}/plugins/{name}

Request parameters:

    {
        "active": true/false,
    }

Example Request:

    PUT api/v2/nodes/emq@127.0.0.1/plugins/emq_recon
    Request Json Parameter:
    {
        "active": true
    }

Response:

    {
        "code": 0,
        "result": []
    }

### List all Listeners

Definition:

    GET api/v2/monitoring/listeners

Response:

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

### List listeners of a Node

Definition:

    GET api/v2/monitoring/listeners/{node_name}

Example Request:

    GET api/v2/monitoring/listeners/emq@127.0.0.1

Response:

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

\-------------------------------------Statistics of packet sent and received -------------------------------------

### Get Statistics of all Nodes

Definition:

    GET api/v2/monitoring/metrics/

Response:

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

Get Statistics of specified Node ------------------------

Definition:

    GET api/v2/monitoring/metrics/{node_name}

Example Request:

    GET api/v2/monitoring/metrics/emq@127.0.0.1

Response:

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

## Statistics of connected session

### Get Statistics of connected session in all nodes

Definition:

    GET api/v2/monitoring/stats

Example Request:

    GET api/v2/monitoring/stats

Response:

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

### Get Statistics of connected session on specified node

Definition:

    GET api/v2/monitoring/stats/{node_name}

Example Request:

    GET api/v2/monitoring/stats/emq@127.0.0.1

Response:

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

## Hot configuration

### Get Modifiable configuration items of all nodes

Definition:

    GET api/v2/configs

Example Request:

    GET api/v2/configs

Response:

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

### Get Modifiable configuration items of specified node

Definition:

    GET api/v2/nodes/{node_name}/configs

Example Request:

    GET api/v2/nodes/emq@127.0.0.1/configs

Response:

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

### Modify configuration items of all nodes

Definition:

    PUT /api/v2/configs/{app_name}

Request Parameter:

    {
        "key"   : "mqtt.allow_anonymous",
        "value" : "false"
    }

Example Request:

    PUT /api/v2/configs/emqttd

Response:

    {
        "code"  : 0,
        "result": []
    }


### Modify configuration items of specified node

Definition:

    PUT /api/v2/nodes/{node_name}/configs/{app_name}

Request Parameter:

    {
        "key"   : "mqtt.allow_anonymous",
        "value" : "false"
    }

Response:

    {
        "code": 0,
        "result": []
    }

### Get configuration items of specified plugin in specified node

Definition:

    GET api/v2/nodes/{node_name}/plugin_configs/{plugin_name}

Example Request:

    GET api/v2/nodes/emq@127.0.0.1/plugin_configs/emq_auth_http

Response:

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

### Modify configuration item of specified plugin in specified node

Definition:

    PUT api/v2/nodes/{node_name}/plugin_configs/{plugin_name}

Request Parameter:

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

Example Request:

    PUT api/v2/nodes/emq@127.0.0.1/plugin_configs/emq_auth_http

Response:

    {
        "code": 0,
        "result": []
    }

## User Management

### Retrieve Admin User List

Definition:

    GET api/v2/users

Request Example:

    GET api/v2/users

Response:

    {
        "code": 0,
        "result": [
            {
                "username": "admin",
                "tags": "administrator"
            }
        ]
    }

### Add Admin User

Definition:

    POST api/v2/users

Request Parameter:

    {
        "username": "test_user",
        "password": "password",
        "tags": "user"
    }

Request Example:

    POST api/v2/users

Response:

    {
        "code": 0,
        "result": []
    }

### Modify Admin User Information

Definition:

    PUT api/v2/users/{username}

Request Parameter:

    {
        "tags": "admin"
    }

Request Example:

    PUT api/v2/users/test_user

Response:

    {
        "code": 0,
        "result": []
    }

### Delete Admin User

Definition:

    DELETE api/v2/users/{username}

Request Parameter:

Request Example:

    DELETE api/v2/users/test_user

Response:

    {
        "code": 0,
        "result": []
    }

### Authenticate Admin User

Definition:

    POST api/v2/auth

Request Parameter:

    {
        "username": "test_user",
        "password": "password"
    }

Request Example:

    POST api/v2/auth

Response:

    {
        "code": 0,
        "result": []
    }

### Modify Admin User Password

Definition:

    PUT api/v2/change_pwd/{username}

Request Parameter:

    {
        "new_pwd": "newpassword",
        "old_pwd": "password"
    }

Request Example:

    PUT api/v2/change_pwd/test_user

Response:

    {
        "code": 0,
        "result": []
    }

## Error Code

| Code | Comment                         |
| ---- | ------------------------------- |
| 0    | Success                         |
| 101  | badrpc                          |
| 102  | Unknown error                   |
| 103  | Username or password error      |
| 104  | empty username or password      |
| 105  | user does not exist             |
| 106  | admin can not be deleted        |
| 107  | missing request parameter       |
| 108  | request parameter type error    |
| 109  | request parameter is not a json |
| 110  | plugin has been loaded          |
| 111  | plugin has been unloaded        |
| 112  | User offline                    |
| 113  | User exists already             |
| 114  | Wrong old password              |
