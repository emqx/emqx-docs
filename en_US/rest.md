# REST API 

The REST API allows you to query MQTT clients, sessions, subscriptions, and routes. You can also query and monitor the metrics and statistics of the broker. 

## Base URL 

All REST APIs in the documentation have the following base URL: 
    
    
    http(s)://host:8080/api/v3/

## Basic Authentication 

The HTTP requests to the REST API are protected with HTTP Basic authentication. You can create an application in Dashboard, using appid and appsecret to authenticate. For example: 
    
    
    curl -v --basic -u \<appid>:\<appsecret> -k http://localhost:8080/api/v3/brokers

## API's Info 

### List all API describe 

Definition: 
    
    
    GET api/v3/

Example Request: 
    
    
    GET api/v3/

Response: 
    
    
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
          "name": "get_plugin_configs",
          "method": "GET",
          "path": "/nodes/:node/plugin_configs/:plugin",
          "descr": "Get configurations of a plugin on the node"
        },
        {
          "name": "update_plugin_configs",
          "method": "PUT",
          "path": "/nodes/:node/plugin_configs/:plugin",
          "descr": "Update configurations of a plugin on the node"
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
          "name": "lookup_node_connections",
          "method": "GET",
          "path": "nodes/:node/connections/:clientid",
          "descr": "Lookup a connection on node"
        },
        {
          "name": "lookup_connections",
          "method": "GET",
          "path": "/connections/:clientid",
          "descr": "Lookup a connection in the cluster"
        },
        {
          "name": "lookup_node_connection_via_username",
          "method": "GET",
          "path": "/nodes/:node/connection/username/:username",
          "descr": "Lookup a connection via username in the cluster "
        },
        {
          "name": "lookup_connection_via_username",
          "method": "GET",
          "path": "/connection/username/:username",
          "descr": "Lookup a connection via username on a node "
        },
        {
          "name": "kickout_connection",
          "method": "DELETE",
          "path": "/connections/:clientid",
          "descr": "Kick out a connection"
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
          "name": "load_plugin",
          "method": "PUT",
          "path": "/nodes/:node/plugins/:plugin/load",
          "descr": "Load a plugin"
        },
        {
          "name": "unload_plugin",
          "method": "PUT",
          "path": "/nodes/:node/plugins/:plugin/unload",
          "descr": "Unload a plugin"
        },
        {
          "name": "reload_plugin",
          "method": "PUT",
          "path": "/nodes/:node/plugins/:plugin/reload",
          "descr": "Reload a plugin"
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
          "name": "clean_presisent_session",
          "method": "DELETE",
          "path": "/sessions/persistent/:clientid",
          "descr": "Clean a persistent session in the cluster"
        },
        {
          "name": "clean_node_presisent_session",
          "method": "DELETE",
          "path": "nodes/:node/sessions/persistent/:clientid",
          "descr": "Clean a persistent session on the node"
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

## Cluster and Node 

### List all Cluster 

Definition: 
    
    
    GET api/v3/brokers/

Example Request: 
    
    
    GET api/v3/brokers/

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "datetime": "2019-04-29 10:56:41",
          "node": "emqx@127.0.0.1",
          "node_status": "Running",
          "otp_release": "R21/10.3.2",
          "sysdescr": "EMQ X Broker",
          "uptime": "3 minutes, 59 seconds",
          "version": "v3.1.0"
        }
      ]
    }

### Retrieve Info of a Node 

Definition: 
    
    
    GET api/v3/brokers/${node}

Example Request: 
    
    
    GET api/v3/brokers/emqx@127.0.0.1

Response: 
    
    
    {
      "code": 0,
      "data": {
        "datetime": "2019-04-29 10:59:59",
        "node_status": "Running",
        "otp_release": "R21/10.3.2",
        "sysdescr": "EMQ X Broker",
        "uptime": "7 minutes, 16 seconds",
        "version": "v3.1.0"
      }
    }

### List Statistics of All Nodes in the Cluster 

Definition: 
    
    
    GET api/v3/nodes/

Example Request: 
    
    
    GET api/v3/nodes/

Response: 
    
    
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
          "version": "v3.1.0"
        }
      ]
    }

### Retrieve Statistics of a Specific Node 

Definition: 
    
    
    GET api/v3/nodes/${node}

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1

Response: 
    
    
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
        "version": "v3.1.0"
      }
    }

## Connections 

### List all Connections in the Cluster 

Definition: 
    
    
    GET api/v3/connections/

Example Request: 
    
    
    GET api/v3/connections/?_page=1&_limit=10000

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "clean_start": true,
          "client_id": "mosquitto_mqtt",
          "conn_mod": "emqx_connection",
          "connected_at": "2019-04-29 11:05:01",
          "heap_size": 2586,
          "ipaddress": "127.0.0.1",
          "is_bridge": false,
          "keepalive": 60,
          "mailbox_len": 0,
          "node": "emqx@127.0.0.1",
          "peercert": "nossl",
          "port": 64899,
          "proto_name": "MQIsdp",
          "proto_ver": 3,
          "recv_cnt": 2,
          "recv_msg": 0,
          "recv_oct": 47,
          "recv_pkt": 2,
          "reductions": 3588,
          "send_cnt": 2,
          "send_msg": 0,
          "send_oct": 9,
          "send_pend": 0,
          "send_pkt": 2,
          "username": "undefined",
          "zone": "external"
        }
      ],
      "meta": {
        "page": 1,
        "limit": 10000,
        "count": 1
      }
    }

### List all Connections on a Node 

Definition: 
    
    
    GET api/v3/nodes/${node}/connections/

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1/connections/?_page=1&_limit=10000

Response: 
    
    
    {
      "code": 0,
      "data": [
       {
         "clean_start": true,
         "client_id": "mosquitto_mqtt",
         "conn_mod": "emqx_connection",
         "connected_at": "2019-04-29 11:05:01",
         "heap_size": 610,
         "ipaddress": "127.0.0.1",
         "is_bridge": false,
         "keepalive": 60,
         "mailbox_len": 0,
         "node": "emqx@127.0.0.1",
         "peercert": "nossl",
         "port": 64899,
         "proto_name": "MQIsdp",
         "proto_ver": 3,
         "recv_cnt": 5,
         "recv_msg": 0,
         "recv_oct": 53,
         "recv_pkt": 5,
         "reductions": 6081,
         "send_cnt": 5,
         "send_msg": 0,
         "send_oct": 15,
         "send_pend": 0,
         "send_pkt": 5,
         "username": "undefined",
         "zone": "external"
       }
      ],
      "meta": {
        "page": 1,
        "limit": 10000,
        "count": 1
      }
    }

### Retrieve a Connection in the Cluster 

Definition: 
    
    
    GET api/v3/connections/${clientid}

Example Request: 
    
    
    GET api/v3/connections/mosquitto_mqtt

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "clean_start": true,
          "client_id": "mosquitto_mqtt",
          "conn_mod": "emqx_connection",
          "connected_at": "2019-04-29 11:05:01",
          "heap_size": 610,
          "ipaddress": "127.0.0.1",
          "is_bridge": false,
          "keepalive": 60,
          "mailbox_len": 0,
          "node": "emqx@127.0.0.1",
          "peercert": "nossl",
          "port": 64899,
          "proto_name": "MQIsdp",
          "proto_ver": 3,
          "recv_cnt": 8,
          "recv_msg": 0,
          "recv_oct": 59,
          "recv_pkt": 8,
          "reductions": 8560,
          "send_cnt": 8,
          "send_msg": 0,
          "send_oct": 21,
          "send_pend": 0,
          "send_pkt": 8,
          "username": "undefined",
          "zone": "external"
        }
      ]
    }

### Retrieve a Connection on a Node 

Definition: 
    
    
    GET api/v3/nodes/${node}/connections/${clientid}

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1/connections/mosquitto_mqtt

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "clean_start": true,
          "client_id": "mosquitto_mqtt",
          "conn_mod": "emqx_connection",
          "connected_at": "2019-04-29 11:05:01",
          "heap_size": 610,
          "ipaddress": "127.0.0.1",
          "is_bridge": false,
          "keepalive": 60,
          "mailbox_len": 0,
          "node": "emqx@127.0.0.1",
          "peercert": "nossl",
          "port": 64899,
          "proto_name": "MQIsdp",
          "proto_ver": 3,
          "recv_cnt": 14,
          "recv_msg": 0,
          "recv_oct": 71,
          "recv_pkt": 14,
          "reductions": 13534,
          "send_cnt": 14,
          "send_msg": 0,
          "send_oct": 33,
          "send_pend": 0,
          "send_pkt": 14,
          "username": "undefined",
          "zone": "external"
        }
      ]
    }

### Kick-out a Specified Connection in Cluster 

Definition: 
    
    
    DELETE api/v3/connections/${clientid}

Example Request: 
    
    
    DELETE api/v3/connections/mosquitto_mqtt

Response: 
    
    
    {
      "code": 0
    }

## Sessions 

### List all Sessions in the Cluster 

Definition: 
    
    
    GET api/v3/sessions/

Example Request: 
    
    
    GET api/v3/sessions/?_page=1&_limit=10000

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "awaiting_rel_len": 0,
          "binding": "local",
          "clean_start": true,
          "client_id": "mqttjs_f79fbc5a4b",
          "created_at": "2019-04-29 11:28:04",
          "deliver_msg": 0,
          "enqueue_msg": 0,
          "expiry_interval": 0,
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
          "reductions": 211,
          "subscriptions_count": 0,
          "username": ""
        },
        {
          "awaiting_rel_len": 0,
          "binding": "local",
          "clean_start": true,
          "client_id": "mosquitto_mqtt",
          "created_at": "2019-04-29 11:28:28",
          "deliver_msg": 0,
          "enqueue_msg": 0,
          "expiry_interval": 0,
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
          "reductions": 202,
          "subscriptions_count": 0,
          "username": "undefined"
        }
      ],
      "meta": {
        "page": 1,
        "limit": 10000,
        "count": 2
      }
    }

### Retrieve a Session in the Cluster 

Definition: 
    
    
    GET api/v3/sessions/${clientid}

Example Request: 
    
    
    GET api/v3/sessions/mosquitto_mqtt

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "awaiting_rel_len": 0,
          "binding": "local",
          "clean_start": true,
          "client_id": "mosquitto_mqtt",
          "created_at": "2019-04-29 11:28:28",
          "deliver_msg": 0,
          "enqueue_msg": 0,
          "expiry_interval": 0,
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
          "reductions": 453,
          "subscriptions_count": 1,
          "username": "undefined"
        }
      ]
    }

### List all Sessions on a Node 

Definition: 
    
    
    GET api/v3/nodes/${node}/sessions/

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1/sessions/?_page=1&_limit=10000

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "awaiting_rel_len": 0,
          "binding": "local",
          "clean_start": true,
          "client_id": "mqttjs_f79fbc5a4b",
          "created_at": "2019-04-29 11:28:04",
          "deliver_msg": 0,
          "enqueue_msg": 0,
          "expiry_interval": 0,
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
          "reductions": 211,
          "subscriptions_count": 0,
          "username": ""
        },
        {
          "awaiting_rel_len": 0,
          "binding": "local",
          "clean_start": true,
          "client_id": "mosquitto_mqtt",
          "created_at": "2019-04-29 11:28:28",
          "deliver_msg": 0,
          "enqueue_msg": 0,
          "expiry_interval": 0,
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
          "reductions": 453,
          "subscriptions_count": 1,
          "username": "undefined"
        }
      ],
      "meta": {
        "page": 1,
        "limit": 10000,
        "count": 2
      }
    }

### Retrieve a Session on a Node 

Definition: 
    
    
    GET api/v3/nodes/${node}/sessions/${clientid}

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1/sessions/mosquitto_mqtt

Request JSON Parameter: 
    
    
    {
      "topic": "test_topic",
      "payload": "hello",
      "qos": 1,
      "retain": false,
      "client_id": "mqttjs_ab9069449e"
    }

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "awaiting_rel_len": 0,
          "binding": "local",
          "clean_start": true,
          "client_id": "mosquitto_mqtt",
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
          "username": "mosquitto_mqtt"
        }
      ]
    }

## Subscriptions 

### List all Subscriptions in the Cluster 

Definition: 
    
    
    GET api/v3/subscriptions/

Example Request: 
    
    
    GET api/v3/subscriptions/?_page=1&_limit=10000

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "client_id": "mqttjs_f79fbc5a4b",
          "node": "emqx@127.0.0.1",
          "qos": 0,
          "topic": "testtopic/#"
        },
        {
          "client_id": "mosquitto_mqtt",
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

### List Subscriptions of a Connection in the Cluster 

Definition: 
    
    
    GET api/v3/subscriptions/${clientid}

Example Request: 
    
    
    GET api/v3/subscriptions/mosquitto_mqtt

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "client_id": "mosquitto_mqtt",
          "node": "emqx@127.0.0.1",
          "qos": 0,
          "topic": "t"
        }
      ]
    }

### List all Subscriptions of a Node 

Definition: 
    
    
    GET api/v3/nodes/${node}/subscriptions/

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1/subscriptions/?_page=1&_limit=10000

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "client_id": "mqttjs_f79fbc5a4b",
          "node": "emqx@127.0.0.1",
          "qos": 0,
          "topic": "testtopic/#"
        },
        {
          "client_id": "mosquitto_mqtt",
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

### List Subscriptions of a Client on a node 

Definition: 
    
    
    GET api/v3/nodes/${node}/subscriptions/${clientid}

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1/subscriptions/mosquitto_mqtt

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "client_id": "mosquitto_mqtt",
          "node": "emqx@127.0.0.1",
          "qos": 0,
          "topic": "t"
        }
      ]
    }

## Routes 

### List all Routes in the Cluster 

Definition: 
    
    
    GET api/v3/routes/

Example Request: 
    
    
    GET api/v3/routes/

Response: 
    
    
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

### Retrieve a Route of Topic in the Cluster 

Definition: 
    
    
    GET api/v3/routes/${topic}

Example Request: 
    
    
    GET api/v3/routes/t

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "node": "emqx@127.0.0.1",
          "topic": "t"
        }
      ]
    }

## Publish/Subscribe 

### Publish Message 

Definition: 
    
    
    POST api/v3/mqtt/publish

Request JSON Parameter: 
    
    
    {
      "topic": "test_topic",
      "payload": "hello",
      "qos": 1,
      "retain": false,
      "client_id": "mqttjs_ab9069449e"
    }

Example Request: 
    
    
    POST api/v3/mqtt/publish

Response: 
    
    
    {
      "code": 0
    }

### Create a Subscription 

Definition: 
    
    
    POST api/v3/mqtt/subscribe

Request JSON Parameter: 
    
    
    {
      "topic": "test_topic",
      "qos": 1,
      "client_id": "mqttjs_ab9069449e"
    }

Example Request: 
    
    
    POST api/v3/mqtt/subscribe

Response: 
    
    
    {
      "code": 0
    }

### Unsubscribe Topic 

Definition: 
    
    
    POST api/v3/mqtt/unsubscribe

Request JSON Parameter: 
    
    
    {
      "topic": "test_topic",
      "client_id": "mqttjs_ab9069449e"
    }

Example Request: 
    
    
    POST api/v3/mqtt/unsubscribe

Response: 
    
    
    {
      "code": 0
    }

## Plugins 

### List all Plugins of Cluster 

Definition: 
    
    
    GET api/v3/plugins/

Example Request: 
    
    
    GET api/v3/plugins/

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "node": "emqx@127.0.0.1",
          "plugins": [
            {
              "name": "emqx_auth_clientid",
              "version": "v3.1.0",
              "description": "EMQ X Authentication with ClientId/Password",
              "active": false
            },
            {
              "name": "emqx_auth_http",
              "version": "v3.1.0",
              "description": "EMQ X Authentication/ACL with HTTP API",
              "active": false
            },
            {
              "name": "emqx_auth_jwt",
              "version": "v3.1.0",
              "description": "EMQ X Authentication with JWT",
              "active": false
            },
            {
              "name": "emqx_auth_ldap",
              "version": "v3.1.0",
              "description": "EMQ X Authentication/ACL with LDAP",
              "active": false
            },
            {
              "name": "emqx_auth_mongo",
              "version": "v3.1.0",
              "description": "EMQ X Authentication/ACL with MongoDB",
              "active": false
            },
            {
              "name": "emqx_auth_mysql",
              "version": "v3.1.0",
              "description": "EMQ X Authentication/ACL with MySQL",
              "active": false
            },
            {
              "name": "emqx_auth_pgsql",
              "version": "v3.1.0",
              "description": "EMQ X Authentication/ACL with PostgreSQL",
              "active": false
            },
            {
              "name": "emqx_auth_redis",
              "version": "v3.1.0",
              "description": "EMQ X Authentication/ACL with Redis",
              "active": false
            },
            {
              "name": "emqx_auth_username",
              "version": "v3.1.0",
              "description": "EMQ X Authentication with Username and Password",
              "active": false
            },
            {
              "name": "emqx_coap",
              "version": "v3.1.0",
              "description": "EMQ X CoAP Gateway",
              "active": false
            },
            {
              "name": "emqx_dashboard",
              "version": "v3.1.0",
              "description": "EMQ X Web Dashboard",
              "active": true
            },
            {
              "name": "emqx_delayed_publish",
              "version": "v3.1.0",
              "description": "EMQ X Delayed Publish",
              "active": false
            },
            {
              "name": "emqx_lua_hook",
              "version": "v3.1.0",
              "description": "EMQ X Lua Hooks",
              "active": false
            },
            {
              "name": "emqx_lwm2m",
              "version": "v3.1.0",
              "description": "EMQ X LwM2M Gateway",
              "active": false
            },
            {
              "name": "emqx_management",
              "version": "v3.1.0",
              "description": "EMQ X Management API and CLI",
              "active": true
            },
            {
              "name": "emqx_plugin_template",
              "version": "v3.1.0",
              "description": "EMQ X Plugin Template",
              "active": false
            },
            {
              "name": "emqx_psk_file",
              "version": "v3.1.0",
              "description": "EMQX PSK Plugin from File",
              "active": false
            },
            {
              "name": "emqx_recon",
              "version": "v3.1.0",
              "description": "EMQ X Recon Plugin",
              "active": true
            },
            {
              "name": "emqx_reloader",
              "version": "v3.1.0",
              "description": "EMQ X Reloader Plugin",
              "active": false
            },
            {
              "name": "emqx_retainer",
              "version": "v3.1.0",
              "description": "EMQ X Retainer",
              "active": true
            },
            {
              "name": "emqx_rule_engine",
              "version": "v3.1.0",
              "description": "EMQ X Rule Engine",
              "active": true
            },
            {
              "name": "emqx_sn",
              "version": "v3.1.0",
              "description": "EMQ X MQTT SN Plugin",
              "active": false
            },
            {
              "name": "emqx_statsd",
              "version": "v3.1.0",
              "description": "Statsd for EMQ X",
              "active": false
            },
            {
              "name": "emqx_stomp",
              "version": "v3.1.0",
              "description": "EMQ X Stomp Protocol Plugin",
              "active": false
            },
            {
              "name": "emqx_web_hook",
              "version": "v3.1.0",
              "description": "EMQ X Webhook Plugin",
              "active": false
            }
          ]
        }
      ]
    }

### List all Plugins of a Node 

Definition: 
    
    
    GET api/v3/nodes/${node}/plugins/

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1/plugins/

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "name": "emqx_auth_clientid",
          "version": "v3.1.0",
          "description": "EMQ X Authentication with ClientId/Password",
          "active": false
        },
        {
          "name": "emqx_auth_http",
          "version": "v3.1.0",
          "description": "EMQ X Authentication/ACL with HTTP API",
          "active": false
        },
        {
          "name": "emqx_auth_jwt",
          "version": "v3.1.0",
          "description": "EMQ X Authentication with JWT",
          "active": false
        },
        {
          "name": "emqx_auth_ldap",
          "version": "v3.1.0",
          "description": "EMQ X Authentication/ACL with LDAP",
          "active": false
        },
        {
          "name": "emqx_auth_mongo",
          "version": "v3.1.0",
          "description": "EMQ X Authentication/ACL with MongoDB",
          "active": false
        },
        {
          "name": "emqx_auth_mysql",
          "version": "v3.1.0",
          "description": "EMQ X Authentication/ACL with MySQL",
          "active": false
        },
        {
          "name": "emqx_auth_pgsql",
          "version": "v3.1.0",
          "description": "EMQ X Authentication/ACL with PostgreSQL",
          "active": false
        },
        {
          "name": "emqx_auth_redis",
          "version": "v3.1.0",
          "description": "EMQ X Authentication/ACL with Redis",
          "active": false
        },
        {
          "name": "emqx_auth_username",
          "version": "v3.1.0",
          "description": "EMQ X Authentication with Username and Password",
          "active": false
        },
        {
          "name": "emqx_coap",
          "version": "v3.1.0",
          "description": "EMQ X CoAP Gateway",
          "active": false
        },
        {
          "name": "emqx_dashboard",
          "version": "v3.1.0",
          "description": "EMQ X Web Dashboard",
          "active": true
        },
        {
          "name": "emqx_delayed_publish",
          "version": "v3.1.0",
          "description": "EMQ X Delayed Publish",
          "active": false
        },
        {
          "name": "emqx_lua_hook",
          "version": "v3.1.0",
          "description": "EMQ X Lua Hooks",
          "active": false
        },
        {
          "name": "emqx_lwm2m",
          "version": "v3.1.0",
          "description": "EMQ X LwM2M Gateway",
          "active": false
        },
        {
          "name": "emqx_management",
          "version": "v3.1.0",
          "description": "EMQ X Management API and CLI",
          "active": true
        },
        {
          "name": "emqx_plugin_template",
          "version": "v3.1.0",
          "description": "EMQ X Plugin Template",
          "active": false
        },
        {
          "name": "emqx_psk_file",
          "version": "v3.1.0",
          "description": "EMQX PSK Plugin from File",
          "active": false
        },
        {
          "name": "emqx_recon",
          "version": "v3.1.0",
          "description": "EMQ X Recon Plugin",
          "active": true
        },
        {
          "name": "emqx_reloader",
          "version": "v3.1.0",
          "description": "EMQ X Reloader Plugin",
          "active": false
        },
        {
          "name": "emqx_retainer",
          "version": "v3.1.0",
          "description": "EMQ X Retainer",
          "active": true
        },
        {
          "name": "emqx_rule_engine",
          "version": "v3.1.0",
          "description": "EMQ X Rule Engine",
          "active": true
        },
        {
          "name": "emqx_sn",
          "version": "v3.1.0",
          "description": "EMQ X MQTT SN Plugin",
          "active": false
        },
        {
          "name": "emqx_statsd",
          "version": "v3.1.0",
          "description": "Statsd for EMQ X",
          "active": false
        },
        {
          "name": "emqx_stomp",
          "version": "v3.1.0",
          "description": "EMQ X Stomp Protocol Plugin",
          "active": false
        },
        {
          "name": "emqx_web_hook",
          "version": "v3.1.0",
          "description": "EMQ X Webhook Plugin",
          "active": false
        }
      ]
    }

### Start a Plugin 

Definition: 
    
    
    PUT api/v3/nodes/${node}/plugins/${plugin}/load

Example Request: 
    
    
    PUT api/v3/nodes/emqx@127.0.0.1/plugins/emqx_auth_clientid/load

Response: 
    
    
    {
      "code": 0
    }

### Start a Plugin 

Definition: 
    
    
    PUT api/v3/nodes/${node}/plugins/${plugin}/unload

Example Request: 
    
    
    PUT api/v3/nodes/emqx@127.0.0.1/plugins/emqx_auth_clientid/unload

Response: 
    
    
    {
      "code": 0
    }

## Listeners 

### List all Listeners of Cluster 

Definition: 
    
    
    GET api/v3/listeners/

Example Request: 
    
    
    GET api/v3/listeners/

Response: 
    
    
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
              "listen_on": "8080",
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

### list all Listeners of a Node 

Definition: 
    
    
    GET api/v3/nodes/${node}/listeners

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1/listeners

Response: 
    
    
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
          "listen_on": "8080",
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

## Statistics of packet sent and received 

### Get Statistics in the Cluster 

Definition: 
    
    
    GET api/v3/metrics/

Example Request: 
    
    
    GET api/v3/metrics/

Response: 
    
    
    {
      "code": 0,
      "data": [
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
    }

### Get Statistics of a specified Node 

Definition: 
    
    
    GET api/v3/nodes/${node}/metrics/

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1/metrics/

Response: 
    
    
    {
      "code": 0,
      "data": {
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

## Statistics of connected session 

### Get Statistics of connected session of Cluster 

Definition: 
    
    
    GET api/v3/stats/

Example Request: 
    
    
    GET api/v3/stats/

Response: 
    
    
    {
      "code": 0,
      "data": [
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
    }

### Get Statistics of connected session on specified node 

Definition: 
    
    
    GET api/v3/nodes/${node}/stats/

Example Request: 
    
    
    GET api/v3/nodes/emqx@127.0.0.1/stats/

Response: 
    
    
    {
      "code": 0,
      "data": {
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
    }

## Alarms 

### Get Current Alarms of Cluster 

Definition: 
    
    
    GET api/v3/alarms/present

Example Request: 
    
    
    GET api/v3/alarms/present

Response: 
    
    
    {
      "code": 0,
      "data": [
        {
          "alarms": [],
          "node": "emqx@127.0.0.1"
        }
      ]
    }

### Get Current Alarms of Specified Node 

Definition: 
    
    
    GET api/v3/alarms/present/${node}

Example Request: 
    
    
    GET api/v3/alarms/present/emqx@127.0.0.1

Response: 
    
    
    {
      "code": 0,
      "data": []
    }

### Get Alarms History of Cluster 

Definition: 
    
    
    GET api/v3/alarms/history

Example Request: 
    
    
    GET api/v3/alarms/history

Response: 
    
    
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

### Get Alarms History of Specified Node 

Definition: 
    
    
    GET api/v3/alarms/present/${node}

Example Request: 
    
    
    GET api/v3/alarms/present/emqx@127.0.0.1

Response: 
    
    
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

## Banned 

### List all Ban Records in the Cluster 

Definition: 
    
    
    GET api/v3/banned/

Example Request: 
    
    
    GET api/v3/banned/?_page=1&_limit=10000

Response: 
    
    
    {
      "code": 0,
      "data": [],
      "meta": {
          "count": 0,
          "limit": 10000,
          "page": 1
      }
    }

### Create a Ban Record 

Definition: 
    
    
    POST api/v3/banned/

Request JSON Parameter: 
    
    
    {
      "who": "mqttjs_ab9069449e",
      "as": "client_id",
      "reason": "banned the clientId",
      "desc": "normal banned",
      "until": 1536146187
    }

Example Request: 
    
    
    POST api/v3/banned/

Response: 
    
    
    {
      "code": 0,
      "data": {
        "who": "mqttjs_ab9069449e",
        "as": "client_id",
        "reason": "banned the clientId",
        "desc": "normal banned",
        "until": 1536146187
      }
    }

### Delete a Ban Record 

Definition: 
    
    
    DELETE api/v3/banned/${who}?as=${as}

Example Request: 
    
    
    DELETE api/v3/banned/mqttjs_ab9069449e?as=client_id

Response: 
    
    
    {
      "code": 0
    }

## Error Message/Pagination 

### When the HTTP status code is 5xx, the response returns the error message 

Example Request: 
    
    
    PUT api/v3/nodes/emqx@127.0.0.1/plugins/emqx_recon/load

Response: 
    
    
    {
      "code": 102,
      "message": "already_started"
    }

### Pagination parameters and meta-data 

The API that uses the _page=1&_limit=10000 parameter in the request example supports pagination: 
    
    
    _page: Current Page
    _limit: Page Size

Response: 
    
    
    {
      "code": 0,
      "data": [],
      "meta": {
        "page": 1,
        "limit": 10000,
        "count": 0
      }
    }

## Rule Engine 

### Create Rule 

Definition: 
    
    
    POST api/v3/rules

Parameters: 

name        |  String, rule name                                                       |                                        
------------|--------------------------------------------------------------------------|----------------------------------------
for         |  String, for which hook. Can be: "message.pu See ` plugins ` for details |  blish", "client.connected" ...        
rawsql      |  String, the SQL                                                         |                                        
actions     |  JSON Array, the action list                                             |                                        
            
  *         
            |  name                                                                    |  String, name of the action            
            
  *         
            |  params                                                                  |  JSON Object, parameters of the action 
description |  String, optional, description of the rule                               |                                        



Parameter Example: 
    
    
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

Example Response: 
    
    
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

### Query Rule 

Definition: 
    
    
    GET api/v3/rules/:id

Request Example: 
    
    
    GET api/v3/rules/test-rule:1556263150688255821

Response Example: 
    
    
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

### List Rules 

Definition: 
    
    
    GET api/v3/rules

Response Example: 
    
    
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

### Delete a Rule 

Definition: 
    
    
    DELETE api/v3/rules/:id

Request Example: 
    
    
    DELETE api/v3/rules/test-rule:1556263150688255821

Response Example: 
    
    
    {
      "code": 0
    }

### List Actions 

Definition: 
    
    
    GET api/v3/actions?for=${hook_type}

Request Example: 
    
    
    GET api/v3/actions

Response Example: 
    
    
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

Request Example:: 
    

GET 'api/v3/actions?for=client.connected' 

Response Example: 
    
    
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

### Query Actions 

Definition: 
    
    
    GET api/v3/actions/:action_name

Request Example: 
    
    
    GET 'api/v3/actions/built_in:inspect_action'

Response Example: 
    
    
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

### List Resource Types 

Definition: 
    
    
    GET api/v3/resource_types

Response Example: 
    
    
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

### Query Resource Types 

Definition: 
    
    
    GET api/v3/resource_types/:type

Request Example: 
    
    
    GET api/v3/resource_types/built_in

Response Example: 
    
    
    {
      "code": 0,
      "data": {
          "description": "The built in resource type for debug purpose",
          "name": "built_in",
          "params": {},
          "provider": "emqx_rule_engine"
      }
    }

### Query Resources by Resource Type 

Definition: 
    
    
    GET api/v3/resource_types/:type/resources

Request Example: 
    
    
    GET api/v3/resource_types/built_in/resources

Response Example: 
    
    
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

### Query Actions by Resource Type 

Definition: 
    
    
    GET api/v3/resource_types/:type/actions

Request Example:: 
    

GET api/v3/resource_types/built_in/actions 

Response Example: 
    
    
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

### Create Resource 

Definition: 
    
    
    POST api/v3/resources

Parameters: 

name        |  String, name of the resource                  
------------|------------------------------------------------
type        |  String, resource type                         
config      |  JSON Object, resource configuration           
description |  String, optional, description of the resource 



Parameter Example: 
    
    
    {
      "name": "test-resource",
      "type": "built_in",
      "config": {
          "a": 1
      },
      "description": "test-resource"
    }

Response Example: 
    
    
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

### List Resources 

Definition: 
    
    
    GET api/v3/resources

Response Example: 
    
    
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

### Query Resource 

Definition: 
    
    
    GET api/v3/resources/:resource_id

Request Example: 
    
    
    GET 'api/v3/resources/built_in:test-resource'

Response Example: 
    
    
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

### Delete Resources 

Definition: 
    
    
    DELETE api/v3/resources/:resource_id

Request Example: 
    
    
    DELETE 'api/v3/resources/built_in:test-resource'

Response Example: 
    
    
    {
      "code": 0
    }
