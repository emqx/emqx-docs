===================
分布式桥接(Bridge)
===================

------------
emqttd间桥接
------------

## Overview

One or more emqttd brokers could be bridged together. Bridge forward PUBLISH message from one broker node to another.

Clustered nodes will share/copy the same topics tree, but bridge nodes will not.

## Architecture

Pub -----> Broker1 --- Bridge Forward--> Broker2 -- Bridge Forward --> Broker3 -> Sub

## User Guide

For example, two brokers:

Name    | Node              | MQTT Port 
--------|------------------ |-----------
emqttd1 | emqttd1@127.0.0.1 | 1883
emqttd2 | emqttd2@127.0.0.1 | 2883


Create a bridge from emqttd1 to emqttd2, follow steps:

### 1. Start brokers

```
cd emqttd1/ && ./bin/emqttd start
cd emqttd2/ && ./bin/emqttd start
```

### 2. Create bridge on emqttd1

```
cd emqttd1/ && ./bin/emqttd_ctl bridges start emqttd2@127.0.0.1 topic/# qos=2,prefix=abc/,suffix=/xyz
```

Print if successfully:

```
bridge is started."
```

Show bridges:

```
./bin/emqttd_ctl bridges list
```

### 3. Subscribe on emqttd2

```
mosquitto_sub -p 2883 -t topic/# -q 2 -d
```

### 4. Publish on emqttd1

```
mosquitto_pub -p 1883 -t topic/1 -m hello -q 1
```

## Admin Commands

```sh
#query bridges
./bin/emqttd_ctl bridges list

#start bridge                       
./bin/emqttd_ctl bridges start <Node> <Topic>

#start bridge with options
./bin/emqttd_ctl bridges start <Node> <Topic> <Options>

#stop bridge  
./bin/emqttd_ctl bridges stop <Node> <Topic>
```

<Options>: 

```
Options:
qos     = 0 | 1 | 2
prefix  = string
suffix  = string
queue   = integer
Example:
qos=2,prefix=abc/,suffix=/yxz,queue=1000
```

## API

```erlang
emqttd_bridge_sup:start_bridge(Node, SubTopic)

emqttd_bridge_sup:start_bridge(Node, SubTopic, Options)
```

## Options

```
-type option()  :: {max_queue_len, pos_integer()} |
{qos, mqtt_qos()} |
{topic_suffix, binary()} |
{topic_prefix, binary()} |
{ping_down_interval, pos_integer()}.
```

Option        |     Type        |  Description
--------------|-----------------|---------------
max_queue_len | pos_integer()   | max cache queue length(TODO: still not work)
qos           | mqtt_qos()      | reset qos
topic_suffix  | binary()        | topic suffix when forwarding message
topic_prefix  | binary()        | topic prefix when forwarding message 
ping_down_interval | pos_integer() | ping interval when bridge node down

1. cache messages when bridged node down?
2. shutdown bridge when remote down?
3. auto reconnect with remote node?
4. RemoteTopicPrefix??? 


## TODO: Round Robbin Bridge??

--->Bridge Group --> Bridge ->
--> Bridge ->
--> Bridge ->
--> Bridge ->
.......



-------------
mosquitto桥接
-------------




-------------
rsmb桥接
-------------





