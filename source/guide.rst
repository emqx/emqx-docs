
====================
用户手册(User Guide)
====================

-----------------------------------
发布订阅(PubSub)
-----------------------------------

emqttd连接手机、IoT终端设备、Web、MQ中间件、数据库

eMQTT是基于Erlang/OTP平台开发的MQTT协议发布订阅模式(Publish/Subscribe)的消息服务器:

.. image:: ./_static/images/pubsub_concept.png

eMQTT支持C1000K+的MQTT客户端连接与低延时的消息路由，适合移动推送、移动即时消息、物联网、智能硬件服务端等应用。

.. NOTE:: Wikipedia PubSub模式: https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern

发布订阅模式，

MQTT客户端库:

.....

Mosquitto_pub
mosquitto_sub

-----------------------------------
Authentication
-----------------------------------

-----------------------------------
ACL
-----------------------------------

-----------------------------------
HTTP API
-----------------------------------

-----------------------------------
WebSocket
-----------------------------------


-----------------------------------
$SYS Topics
-----------------------------------

-----------------------------------
Trace
-----------------------------------

-----------------------------------
Bridge
-----------------------------------

-----------------------------------
Rewrite
-----------------------------------


-----------------------------------
完整的MQTT V3.1/V3.1.1协议支持
-----------------------------------

Full MQTT V3.1/V3.1.1 protocol specification support

Retained Messages Support

Last Will Message Support


--------------------------------
MQTT客户端库与连接规划
--------------------------------

规划eMQTT的连接规模。

客户端库列表

TCP/SSL Connection Support

--------------------------------
MQTT应用主题(Topic)设计
--------------------------------

MQTT应用主要是设计MQTT Topic

例如PUSH应用

CHAT应用

物联网传感器应用...

--------------------------------
MQTT应用消息QoS设计
--------------------------------

QoS0, QoS1, QoS2 Publish and Subscribe

-------------------------------------------
MQTT持久会话(Session)与临时会话(Transient)
-------------------------------------------

持久会话保存离线消息

是否采用

Session Management and Offline Messages

--------------------------------
MQTT客户端认证与访问控制
--------------------------------

Client Authentication with clientId, ipaddress

Client Authentication with username, password.

Client ACL control with ipaddress, clientid, username.


--------------------------------
HTTP Publish API与WebSocket
--------------------------------

HTTP PUBLISH

WebSocket

----------------------------------
Cluster brokers on several servers.
----------------------------------


----------------------------------
桥接多台Broker
----------------------------------

FontEnd Brokers -> BackEnd Brokers


-----------------------------------------------------
插件扩展或定制eMQTT服务器
-----------------------------------------------------

Extensible architecture with Hooks, Modules and Plugins


----------------------
$SYS主题($SYS Topics)
----------------------

eMQTT消息服务器提供系统主题($SYS Topics)，用于监控系统状态、统计指标与客户端在线状态。

eMQTT系统主题($SYS Topic)以'$SYS/brokers/${node}'开头，'${node}'是集群的Erlang节点名称，例如::

    $SYS/brokers/emqttd@host1/version

    $SYS/brokers/emqttd@host2/version

Broker $SYS Topics
------------------

+------------------------------+--------------+
| $SYS Topic  | 说明 |
+==============+===============================+
-------------------------------|------------
$SYS/brokers                   | Broker nodes
$SYS/brokers/${node}/version   | Broker Version
$SYS/brokers/${node}/uptime    | Broker Uptime
$SYS/brokers/${node}/datetime  | Broker DateTime
$SYS/brokers/${node}/sysdescr  | Broker Description
 

## Client $SYS Topics

Start with: $SYS/brokers/${node}/clients/

Topic                 |   Payload(json)     | Description
----------------------|---------------------|--------------- 
${clientid}/connected | {ipaddress: "127.0.0.1", username: "test", session: false, version: 3, connack: 0, ts: 1432648482} | Publish when client connected 
${clientid}/disconnected | {reason: "normal" | "keepalive_timeout" | "conn_closed"}

Parameters of 'connected' Payload:

```
ipaddress: "127.0.0.1", 
username: "test", 
session: false, 
protocol: 3, 
connack: 0, 
ts: 1432648482
```

Parameters of 'disconnected' Payload:

```
reason: normal,
ts: 1432648486
```

## Statistics $SYS Topics

Start with '$SYS/brokers/${node}/stats/'

### Client Stats

Topic                                | Description
-------------------------------------|------------
clients/count   | count of current connected clients
clients/max     | max connected clients in the same time


### Session Stats

Topic            | Description
-----------------|------------
sessions/count   | count of current sessions
sessions/max     | max number of sessions

### Subscriber Stats

Topic             | Description
------------------|------------
subscriptions/count | count of current subscriptions
subscriptions/max   | max number of subscriptions


### Topic Stats

Topic             | Description
------------------|------------
topics/count      | count of current topics
topics/max        | max number of topics

### Queue Stats

Topic             | Description
------------------|------------
queues/count      | count of current queues
queues/max        | max number of queues

### Route Stats

Topic             | Description
------------------|------------
'routes/count'    | routes统计
'routes/reverse'  | reverse routes统计

### Retained消息统计

%% $SYS Topic for retained
-define(SYSTOP_RETAINED, [
    'retained/count',
    'retained/max'
]).

## Metrics $SYS Topics

Start with '$SYS/brokers/${node}/metrics/'

### Bytes sent and received

Topic                               | Description
------------------------------------|------------
bytes/received | MQTT Bytes Received since broker started
bytes/sent     | MQTT Bytes Sent since the broker started

### Packets sent and received
 
Topic                    | Description
-------------------------|------------
packets/received         | MQTT Packets received
packets/sent             | MQTT Packets sent
packets/connect          | MQTT CONNECT Packet received
packets/connack          | MQTT CONNACK Packet sent
packets/publish/received | MQTT PUBLISH packets received
packets/publish/sent     | MQTT PUBLISH packets sent
packets/subscribe        | MQTT SUBSCRIBE Packets received
packets/suback           | MQTT SUBACK packets sent
packets/unsubscribe      | MQTT UNSUBSCRIBE Packets received
packets/unsuback         | MQTT UNSUBACK Packets sent
packets/pingreq          | MQTT PINGREQ packets received
packets/pingresp         | MQTT PINGRESP Packets sent
packets/disconnect       | MQTT DISCONNECT Packets received

### Messages sent and received

Topic                                  | Description
---------------------------------------|-------------------
messages/received | Messages Received
messages/sent     | Messages Sent
messages/retained | Messages Retained
messages/stored   | TODO: Messages Stored
messages/dropped  | Messages Dropped

## Alarm Topics

Start with '$SYS/brokers/${node}/alarms/'

Topic            | Description
-----------------|-------------------
${alarmId}/alert | New Alarm
${alarmId}/clear | Clear Alarm


## Sysmon

Start with '$SYS/brokers/${node}/sysmon/'

Topic            | Description
-----------------|-------------------
long_gc          | Long GC Warning
long_schedule    | Long Schedule
large_heap       | Large Heap Warning
busy_port        | Busy Port Warning
busy_dist_port   | Busy Dist Port

## Sys Interval

sys_interval: 1 minute default
