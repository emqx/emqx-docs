# User Guide 

## Start EMQ X Broker 

The EMQ X Broker can be downloaded at: [ https://www.emqx.io/downloads/broker?osType=Linux ](https://www.emqx.io/downloads/broker?osType=Linux)

After downloading the package, it can be installed or unzipped directly to start running. Taking the zip package for MacOS platform as an example: 
    
    
    unzip emqx-macosx-v3.2.0.zip && cd emqx
    
    # Start emqx
    ./bin/emqx start
    
    # Check running status
    ./bin/emqx_ctl status

The default TCP ports used by the EMQ X message server include: 

1883  |  MQTT protocol port                
------|------------------------------------
8883  |  MQTT/SSL port                     
8083  |  MQTT/WebSocket port               
8080  |  HTTP API port                     
18083 |  Dashboard Management Console Port 



## MQTT publish and subscription 

EMQ X Broker is a lightweight publish-subscribe message broker designed for the mobile Internet and the IoT, it currently supports MQTT [ v3.1.1 ](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/mqtt-v3.1.1.html) and [ v5.0 ](http://docs.oasis-open.org/mqtt/mqtt/v5.0/mqtt-v5.0.html) : 

![image](./_static/images/guide_1.png)

After EMQ X is started, devices and clients can connect to the broker through the MQTT protocol and exchange messages via Publish/Subscribe. 

Some popular MQTT client libraries can be found here: [ https://github.com/mqtt/mqtt.github.io/wiki/libraries ](https://github.com/mqtt/mqtt.github.io/wiki/libraries)

For example, using the mosquitto_sub/pub client on command line to publish and to subscribe to messages: 
    
    
    mosquitto_sub -h 127.0.0.1 -p 1883 -t topic -q 2
    mosquitto_pub -h 127.0.0.1 -p 1883 -t topic -q 1 -m "Hello, MQTT!"

## Authentication and Access Control 

EMQ X Broker provides **Connection Authentication** and **Access Control** using a series of authentication plugins, whose name conforms to the pattern of [ emqx_auth ]() \<name>. 

In EMQ X, these two functions are: 

  1. Connection authentication: EMQ X verifies whether the client on each connection has access to the system. If not, it disconnects the connection 
  2. Access Control: EMQ X verifies the permissions of each Publish/Subscribe action of a client, and allows/denies the corresponding action 



### Authentication 

EMQ X Message Broker's Authentication is provided by a series of authentication plugins. It supports authentication by username, password, ClientID or anonymous. 

By default, anonymous authentication is enabled. Multiple authentication modules can be started by loading the corresponding authentication plug-ins and forming an authentication chain: 

![image](./_static/images/guide_2.png)

**Start anonymous authentication** 

Modify the ` etc/emqx.conf ` file to enable anonymous authentication: 
    
    
    ## Allow anonymous access
    ## Value: true | false
    allow_anonymous = true

### Access Control List 

EMQ X Broker implements MQTT client access control through an ACL (Access Control List). 

ACL access control rule definition: 
    
    
    Allow|Deny Identity Subscribe|Publish Topics

When an MQTT client initiates a subscribe/publish request, the access control module of EMQ X Broker will match the ACL rules one by one until the match is successful: 

![image](./_static/images/guide_3.png)

**Default access control settings**

EMQ X Message Broker's default access control can be set in file ` etc/emqx.conf ` : 
    
    
    ## Set whether to allow access when all ACL rules cannot match
    ## Value: allow | deny
    acl_nomatch = allow
    
    ## Set the default file for storing ACL rules
    ## Value: File Name
    acl_file = etc/acl.conf

The ACL rules are defined in file ` etc/acl.conf ` , which is loaded into memory when EMQ X starts: 
    
    
    %% Aallows 'dashboard' users to subscribe to '$SYS/#'
    {allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.
    
    %% Allows local user to publish and subscribe to all topics
    {allow, {ipaddr, "127.0.0.1"}, pubsub, ["$SYS/#", "#"]}.
    
    %% Deny all the users to subscribe to '$SYS/#' and '#' topics except local users
    {deny, all, subscribe, ["$SYS/#", {eq, "#"}]}.
    
    %% Allows any situation other than the above rules
    {allow, all}.

The authentication plugins provided by EMQ X include: 

plugins                                                            |  description                                 
-------------------------------------------------------------------|----------------------------------------------
[ emqx_auth_clientid ](https://github.com/emqx/emqx-auth-clientid) |  ClientId authentication plugin              
[ emqx_auth_username ](https://github.com/emqx/emqx-auth-username) |  username and password authentication plugin 
[ emqx_auth_jwt ](https://github.com/emqx/emqx-auth-jwt)           |  JWT authentication plugin                   
[ emqx_auth_ldap ](https://github.com/emqx/emqx-auth-ldap)         |  LDAP authentication plugin                  
[ emqx_auth_http ](https://github.com/emqx/emqx-auth-http)         |  HTTP authentication plugin                  
[ emqx_auth_mysql ](https://github.com/emqx/emqx-auth-mysql)       |  MySQ Lauthentication plugin                 
[ emqx_auth_pgsql ](https://github.com/emqx/emqx-auth-pgsql)       |  Postgre authentication plugin               
[ emqx_auth_redis ](https://github.com/emqx/emqx-auth-redis)       |  Redis authentication plugin                 
[ emqx_auth_mongo ](https://github.com/emqx/emqx-auth-mongo)       |  MongoDB authentication plugin               



For the configuration and usage of each authentication plug-in, please refer to authentication section of the [ Plugins ](https://docs.emqx.io/broker/v3/en/plugins.html) . 

::: tip Tip
Multiple auth plug-ins can be started at the same time. The plug-in that starts first checks first. 
:::

In addition, EMQ X also supports the use of PSK (Pre-shared Key) for authentication. However, the authentication chain mentioned above is not used in this case. The verification is done during the SSL handshake. For details please refer to [ Pre-shared Key ](https://en.wikipedia.org/wiki/Pre-shared_key) and [ emqx_psk_file ](https://github.com/emqx/emqx-psk-file)

## Shared Subscription 

The EMQ X R3.0 supports cluster-level shared subscriptions that supports multiple message delivery strategies: 

![image](./_static/images/guide_4.png)

Shared subscriptions support two usage methods: 


Subscription prefix   |  Example                               
----------------------|----------------------------------------
$queue/               |  mosquitto_sub -t '$queue/topic'       
$share/\<group>/       |  mosquitto_sub -t '$share/group/topic' 



Example: 
    
    
    mosquitto_sub -t '$share/group/topic'
    
    mosquitto_pub -t 'topic' -m msg -q 2

The dispatch strategy for shared messages can be configured by the ` broker.shared_subscription_strategy ` field in the ` etc/emqx.conf `

The following strategies are supported by EMQ X to distribute messages: 

Strategy    |  Description                              
------------|-------------------------------------------
random      |  Random among all shared subscribers      
round_robin |  According to subscription order          
sticky      |  The last dispatched subscriber is picked 
hash        |  Hash value of the ClientId of publisher  



::: tip Tip
When all subscribers are offline, a subscriber will still be picked and stored in the message queue of its Session. 
:::

## Bridge 

### Bridging two EMQ X Nodes 

The concept of bridging is that EMQ X forwards messages of some of its topics to another MQTT Broker in some way. 

Difference between Bridge and cluster is that bridge does not replicate topic trees and routing tables, a bridge only forwards MQTT messages based on bridging rules. 

Currently the bridging methods supported by EMQ X are as follows: 

  * RPC bridge: RPC Bridge only supports message forwarding and does not support subscribing to the topic of remote nodes to synchronize data. 
  * MQTT Bridge: MQTT Bridge supports both forwarding and data synchronization through subscription topic 



The concept is shown below: 

![image](./_static/images/bridge.png)

In addition, the EMQ X supports multi-node bridge mode interconnection: 

![image](./_static/images/guide_5.png)

In EMQ X, bridge is configured by modifying ` etc/plugins/emqx_bridge_mqtt.conf ` . EMQ X distinguishes between different bridges based on different names. E.g: 
    
    
    ## Bridge address: node name for local bridge, host:port for remote.
    bridge.mqtt.aws.address = 127.0.0.1:1883

This configuration declares a bridge named ` aws ` and specifies that it is bridged to the MQTT server of 127.0.0.1:1883 by MQTT mode. 

In case of creating multiple bridges, it is convenient to replicate all configuration items of the first bridge, and modify the bridge name and other configuration items if necessary (such as bridge.mqtt.$name.address, where $name refers to the name of bridge) 

The next two sections describe how to create a bridge in RPC and MQTT mode respectively and create a forwarding rule that forwards the messages from sensors. Assuming that two EMQ X nodes are running on two hosts: 

Name  |  Node              |  MQTT port 
------|--------------------|------------
emqx1 |  emqx1@192.168.1.1 |  1883      
emqx2 |  emqx2@192.168.1.2 |  1883      



## EMQ X Node RPC Bridge Configuration 

The following is the basic configuration of RPC bridging. The simplest RPC bridging only needs to configure the following three items: 
    
    
    ## Bridge Address: Use node name (nodename@host) for rpc bridging, and host:port for mqtt connection
    bridge.mqtt.aws.address = emqx2@192.168.1.2
    
    ## Forwarding topics of the message
    bridge.mqtt.aws.forwards = sensor1/#,sensor2/#
    
    ## bridged mountpoint
    bridge.mqtt.aws.mountpoint = bridge/emqx2/${node}/

If the message received by the local emqx1 node matches the topic ` sersor1/# ` or ` sensor2/# ` , these messages will be forwarded to the ` sensor1/# ` or ` sensor2/# ` topic of the remote emqx2 node. 

` forwards ` is used to specify topics. Messages of the in ` forwards ` specified topics on local node are forwarded to the remote node. 

` mountpoint ` is used to add a topic prefix when forwarding a message. To use ` mountpoint ` , the ` forwards ` directive must be set. In the above example, a message with the topic sensor1/hello received by the local node will be forwarded to the remote node with the topic ` bridge/emqx2/emqx1@192.168.1.1/sensor1/hello ` . 

Limitations of RPC bridging: 

  1. The RPC bridge of emqx can only forward local messages to the remote bridge node, and cannot synchronize the messages of the remote bridge node to the local node; 
  2. RPC bridge can only bridge two EMQ X together and cannot bridge EMQ X to other mqtt brokers. 



### EMQ X Node MQTT Bridge Configuration 

EMQ X 3.0 officially introduced MQTT bridge, so that EMQ X can bridge any MQTT broker. Because of the characteristics of the MQTT protocol, EMQ X can subscribe to the remote mqtt broker's topic through MQTT bridge, and then synchronize the remote MQTT broker's message to the local. 

EMQ X MQTT bridging principle: Create an MQTT client on the EMQ X broker, and connect this MQTT client to the remote MQTT broker. Therefore, in the MQTT bridge configuration, following fields may be set for the EMQ X to connect to the remote broker as an mqtt client: 
    
    
    ## Bridge Address: Use node name for rpc bridging, use host:port for mqtt connection
    bridge.mqtt.aws.address = 192.168.1.2:1883
    
    ## Bridged Protocol Version
    ## Enumeration value: mqttv3 | mqttv4 | mqttv5
    bridge.mqtt.aws.proto_ver = mqttv4
    
    ## mqtt client's client_id
    bridge.mqtt.aws.client_id = bridge_emq
    
    ## mqtt client's clean_start field
    ## Note: Some MQTT Brokers need to set the clean_start value as `true`
    bridge.mqtt.aws.clean_start = true
    
    ##  mqtt client's username field
    bridge.mqtt.aws.username = user
    
    ## mqtt client's password field
    bridge.mqtt.aws.password = passwd
    
    ## Whether the mqtt client uses ssl to connect to a remote serve or not
    bridge.mqtt.aws.ssl = off
    
    ## CA Certificate of Client SSL Connection (PEM format)
    bridge.mqtt.aws.cacertfile = etc/certs/cacert.pem
    
    ## SSL certificate of Client SSL connection
    bridge.mqtt.aws.certfile = etc/certs/client-cert.pem
    
    ## Key file of Client SSL connection
    bridge.mqtt.aws.keyfile = etc/certs/client-key.pem
    
    ## SSL encryption
    bridge.mqtt.aws.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384
    
    ## TTLS PSK password
    ## Note 'listener.ssl.external.ciphers' and 'listener.ssl.external.psk_ciphers' cannot be configured at the same time
    ##
    ## See 'https://tools.ietf.org/html/rfc4279#section-2'.
    ## bridge.mqtt.aws.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA
    
    ## Client's heartbeat interval
    bridge.mqtt.aws.keepalive = 60s
    
    ## Supported TLS version
    bridge.mqtt.aws.tls_versions = tlsv1.2,tlsv1.1,tlsv1
    
    ## Forwarding topics of the message
    bridge.mqtt.aws.forwards = sensor1/#,sensor2/#
    
    ## Bridged mountpoint
    bridge.mqtt.aws.mountpoint = bridge/emqx2/${node}/
    
    ## Subscription topic for bridging
    bridge.mqtt.aws.subscription.1.topic = cmd/topic1
    
    ## Subscription qos for bridging
    bridge.mqtt.aws.subscription.1.qos = 1
    
    ## Subscription topic for bridging
    bridge.mqtt.aws.subscription.2.topic = cmd/topic2
    
    ## Subscription qos for bridging
    bridge.mqtt.aws.subscription.2.qos = 1
    
    ## Bridging reconnection interval
    ## Default: 30s
    bridge.mqtt.aws.reconnect_interval = 30s
    
    ## QoS1 message retransmission interval
    bridge.mqtt.aws.retry_interval = 20s
    
    ## Inflight Size.
    bridge.mqtt.aws.max_inflight_batches = 32

## EMQ X Bridge Cache Configuration 

The bridge of EMQ X has a message caching mechanism. The caching mechanism is applicable to both RPC bridging and MQTT bridging. When the bridge is disconnected (such as when the network connection is unstable), the messages with a topic specified in ` forwards ` can be cached to the local message queue. Until the bridge is restored, these messages are re-forwarded to the remote node. The configuration of the cache queue is as follows: 
    
    
    ## emqx_bridge internal number of messages used for batch
    bridge.mqtt.aws.queue.batch_count_limit = 32
    
    ##  emqx_bridge internal number of message bytes used for batch
    bridge.mqtt.aws.queue.batch_bytes_limit = 1000MB
    
    ## The path for placing replayq queue. If the item is not specified in the configuration, then replayq will run in `mem-only` mode and messages will not be cached on disk.
    bridge.mqtt.aws.queue.replayq_dir = data/emqx_emqx2_bridge/
    
    ## Replayq data segment size
    bridge.mqtt.aws.queue.replayq_seg_bytes = 10MB

` bridge.emqx2.queue.replayq_dir ` is a configuration parameter for specifying the path of the bridge storage queue. 

` bridge.mqtt.aws.queue.replayq_seg_bytes ` is used to specify the size of the largest single file of the message queue that is cached on disk. If the message queue size exceeds the specified value, a new file is created to store the message queue. 

## CLI for EMQ X Bridge 

CLI for EMQ X Bridge: 
    
    
    $ cd emqx1/ && ./bin/emqx_ctl bridges
    bridges list                                    # List bridges
    bridges start \<Name>                            # Start a bridge
    bridges stop \<Name>                             # Stop a bridge
    bridges forwards \<Name>                         # Show a bridge forward topic
    bridges add-forward \<Name> \<Topic>              # Add bridge forward topic
    bridges del-forward \<Name> \<Topic>              # Delete bridge forward topic
    bridges subscriptions \<Name>                    # Show a bridge subscriptions topic
    bridges add-subscription \<Name> \<Topic> \<Qos>   # Add bridge subscriptions topic

List all bridge states 
    
    
    $ ./bin/emqx_ctl bridges list
    name: emqx     status: Stopped

Start the specified bridge 
    
    
    $ ./bin/emqx_ctl bridges start emqx
    Start bridge successfully.

Stop the specified bridge 
    
    
    $ ./bin/emqx_ctl bridges stop emqx
    Stop bridge successfully.

List the forwarding topics for the specified bridge 
    
    
    $ ./bin/emqx_ctl bridges forwards emqx
    topic:   topic1/#
    topic:   topic2/#

Add a forwarding topic for the specified bridge 
    
    
    $ ./bin/emqx_ctl bridges add-forwards emqx topic3/#
    Add-forward topic successfully.

Delete the forwarding topic for the specified bridge 
    
    
    $ ./bin/emqx_ctl bridges del-forwards emqx topic3/#
    Del-forward topic successfully.

List subscriptions for the specified bridge 
    
    
    $ ./bin/emqx_ctl bridges subscriptions emqx
    topic: cmd/topic1, qos: 1
    topic: cmd/topic2, qos: 1

Add a subscription topic for the specified bridge 
    
    
    $ ./bin/emqx_ctl bridges add-subscription emqx cmd/topic3 1
    Add-subscription topic successfully.

Delete the subscription topic for the specified bridge 
    
    
    $ ./bin/emqx_ctl bridges del-subscription emqx cmd/topic3
    Del-subscription topic successfully.

::: tip Tip
In case of creating multiple bridges, it is convenient to replicate all configuration items of the first bridge, and modify the bridge name and other configuration items if necessary. 
:::

## HTTP Publish API 

The EMQ X message server provides an HTTP publish interface through which an application server or web server can publish MQTT messages: 
    
    
    HTTP POST http://host:8080/api/v3/mqtt/publish

Web servers such as PHP/Java/Python/NodeJS or Ruby on Rails can publish MQTT messages via HTTP POST requests: 
    
    
    curl -v --basic -u user:passwd -H "Content-Type: application/json" -d \
    '{"qos":1, "retain": false, "topic":"world", "payload":"test" , "client_id": "C_1492145414740"}' \-k http://localhost:8080/api/v3/mqtt/publish

HTTP interface parameters: 

parameter |  description     
----------|------------------
client_id |  MQTT client ID  
qos       |  QoS: 0          | 1     | 2 
retain    |  Retain: true    | false 
topic     |  Topic           
payload   |  message payload 



::: tip Tip
HTTP publishing interface uses authentication of [ Basic ](https://en.wikipedia.org/wiki/basic_access_authentication) . The user and password in the above example are from the AppId and password in the Applications of Dashboard. 
:::

## MQTT WebSocket Connection 

EMQ X also supports WebSocket connections, web browsers or applications can connect directly to the broker via WebSocket: 

WebSocket URI:          |  ws(s)\:\//host:8083/mqtt     
------------------------|-----------------------------
Sec-WebSocket-Protocol: |  'mqttv3.1' or 'mqttv3.1.1' 



The Dashboard plugin provides a test tool for an MQTT WebSocket connection: 
    
    
    http://127.0.0.1:18083/#/websocket

## $SYS - System topic 

The EMQ X Broker periodically publishes its running status, message statistics, client online and offline events to the system topic starting with ` $SYS/ ` . 

The ` $SYS ` topic path begins with ` $SYS/brokers/{node}/ ` . ` {node} ` is the name of the node where the event/message is generated, for example: 
    
    
    $SYS/brokers/emqx@127.0.0.1/version
    
    $SYS/brokers/emqx@127.0.0.1/uptime

::: tip Tip
By default, only the MQTT client on localhost is allowed to subscribe to the $SYS topic, this can be changed by modifying the access control rules in file ` etc/acl.config ` . 
:::

$SYS system message publish interval is configured in ` etc/emqx.conf ` : 
    
    
    ## System interval of publishing $SYS messages.
    ##
    ## Value: Duration
    ## Default: 1m, 1 minute
    broker.sys_interval = 1m

### Cluster status information 

Topic                         |  Description               
------------------------------|----------------------------
$SYS/brokers                  |  cluster node list         
$SYS/brokers/${node}/version  |  EMQ X broker version      
$SYS/brokers/${node}/uptime   |  EMQ X broker startup time 
$SYS/brokers/${node}/datetime |  EMQ X broker time         
$SYS/brokers/${node}/sysdescr |  EMQ X broker Description  



### Client Online and Offline Events 

$SYS topic prefix: $SYS/brokers/${node}/clients/ 

Topic                    |  Description                                                        
-------------------------|---------------------------------------------------------------------
${clientid}/connected    |  Online event. This message is published when a client goes online. 
${clientid}/disconnected |  Offline event. This message is published when a client is offline  



The Payload of the 'connected' event message can be parsed into JSON format: 
    
    
    {
        "clientid":"id1",
        "username":"u",
        "ipaddress":"127.0.0.1",
        "connack":0,
        "ts":1554047291,
        "proto_ver":3,
        "proto_name":"MQIsdp",
        "clean_start":true,
        "keepalive":60
    }

The Payload of the 'disconnected' event message can be parsed into JSON format: 
    
    
    {
        "clientid":"id1",
        "username":"u",
        "reason":"normal",
        "ts":1554047291
    }

### Statistics 

System topic prefix: $SYS/brokers/${node}/stats/ 

#### Client statistics 

Topic             |  Description                     
------------------|----------------------------------
connections/count |  Total number of current clients 
connections/max   |  Maximum number of clients       



#### Session statistics 

Topic                     |  Description                           
--------------------------|----------------------------------------
sessions/count            |  Total number of current sessions      
sessions/max              |  maximum number of sessions            
sessions/persistent/count |  Total number of persistent sessions   
sessions/persistent/max   |  maximum number of persistent sessions 



#### Subscription statistics 

Topic                      |  Description                                  
---------------------------|-----------------------------------------------
suboptions/count           |  number of current subscription options       
suboptions/max             |  total number of maximum subscription options 
subscribers/max            |  total number of maximum subscribers          
subscribers/count          |  number of current subscribers                
subscriptions/max          |  maximum number of subscriptions              
subscriptions/count        |  total number of current subscription         
subscriptions/shared/count |  total number of current shared subscriptions 
subscriptions/shared/max   |  maximum number of shared subscriptions       



#### Topic statistics 

Topic        |  Description                    
-------------|---------------------------------
topics/count |  total number of current topics 
topics/max   |  maximum number of topics       



#### Routes statistics 

Topic        |  Description                    
-------------|---------------------------------
routes/count |  total number of current Routes 
routes/max   |  maximum number of Routes       



::: tip Tip
The topics/count and topics/max are numerically equal to routes/count and routes/max. 
:::

### Throughput (bytes/packets/message) statistics 

System Topic Prefix: $SYS/brokers/${node}/metrics/ 

#### sent and received bytes statistics 

Topic          |  Description                
---------------|-----------------------------
bytes/received |  Accumulated received bytes 
bytes/sent     |  Accumulated sent bytes     



#### sent and received MQTT packets statistics 

Topic                       |  Description                                       
----------------------------|----------------------------------------------------
packets/received            |  Accumulative received MQTT packets                
packets/sent                |  Accumulative sent MQTT packets                    
packets/connect             |  Accumulative received packets of MQTT CONNECT     
packets/connack             |  Accumulative sent packets of MQTT CONNACK         
packets/publish/received    |  Accumulative received packets of MQTT PUBLISH     
packets/publish/sent        |  Accumulative sent packets of MQTT PUBLISH         
packets/puback/received     |  Accumulative received packets of MQTT PUBACK      
packets/puback/sent         |  Accumulative sent packets of MQTT PUBACK          
packets/puback/missed       |  Accumulative missed packets of MQTT PUBACK        
packets/pubrec/received     |  Accumulative received packets of MQTT PUBREC      
packets/pubrec/sent         |  Accumulative sent packets of MQTT PUBREC          
packets/pubrec/missed       |  Accumulative missed packets of MQTT PUBREC        
packets/pubrel/received     |  Accumulative received packets of MQTT PUBREL      
packets/pubrel/sent         |  Accumulative sent packets of MQTT PUBREL          
packets/pubrel/missed       |  Accumulative missed packets of MQTT PUBREL        
packets/pubcomp/received    |  Accumulative received packets of MQTT PUBCOMP     
packets/pubcomp/sent        |  Accumulative sent packets of MQTT PUBCOMP         
packets/pubcomp/missed      |  Accumulative missed packets of MQTT PUBCOMP       
packets/subscribe           |  Accumulative received packets of MQTT SUBSCRIBE   
packets/suback              |  Accumulative sent packets of MQTT SUBACK          
packets/unsubscribe         |  Accumulative received packets of MQTT UNSUBSCRIBE 
packets/unsuback            |  Accumulative sent packets of MQTT UNSUBACK        
packets/pingreq             |  Accumulative received packets of MQTT PINGREQ     
packets/pingresp            |  Accumulative sent packets of MQTT PINGRESP        
packets/disconnect/received |  Accumulative received packets of MQTT DISCONNECT  
packets/disconnect/sent     |  Accumulative sent packets of MQTT MQTT DISCONNECT 
packets/auth                |  Accumulative received packets of MQTT Auth        



#### MQTT sent and received messages statistics 

Topic                                    |  Description                                    
-----------------------------------------|-------------------------------------------------
messages/received                        |  Accumulative received messages                 
messages/sent                            |  Accumulative sent messages                     
messages/expired                         |  Accumulative expired messages                  
messages/retained                        |  Accumulative retained messages                 
messages/dropped                         |  Total number of dropped messages               
messages/forward                         |  Total number of messages forwarded by the node 
messages/qos0/received                   |  Accumulative received messages of QoS0         
messages/qos0/sent                       |  Accumulative sent messages of QoS0             
messages/qos1/received                   |  Accumulative received messages QoS1            
messages/qos1/sent                       |  Accumulative sent messages QoS1                
messages/qos2/received                   |                                                 
                                         
> Accumulative received messages of QoS2 
                                         
messages/qos2/sent                       |  Accumulative sent messages of QoS2             
messages/qos2/expired                    |  Total number of expired messages of QoS2       
messages/qos2/dropped                    |  Total number of dropped messages of QoS2       



### Alarms - system alarms 

System Topic Prefix: $SYS/brokers/${node}/alarms/ 

Topic |  Description           
------|------------------------
alert |  newly generated alarm 
clear |  cleared alarm         



### Sysmon - system monitoring 

System Topic Prefix: $SYS/brokers/${node}/sysmon/ 

Topic          |  Description                         
---------------|--------------------------------------
long_gc        |  GC Overtime alarm                   
long_schedule  |  Alarm for Excessive Scheduling Time 
large_heap     |  ALarm for Heap Memory Occupancy     
busy_port      |  Alarm for Port busy                 
busy_dist_port |  Alarm for Dist Port busy            



## Trace 

EMQ X message server supports tracing all messages from a client or published to a topic. 

Trace messages from the client: 
    
    
    $ ./bin/emqx_ctl log primary-level debug
    
    $ ./bin/emqx_ctl trace start client "clientid" "trace_clientid.log" debug

Trace messages published to a topic: 
    
    
    $ ./bin/emqx_ctl log primary-level debug
    
    $ ./bin/emqx_ctl trace start topic "t/#" "trace_topic.log" debug

Query trace: 
    
    
    $ ./bin/emqx_ctl trace list

Stop trace: 
    
    
    $ ./bin/emqx_ctl trace stop client "clientid"
    
    $ ./bin/emqx_ctl trace stop topic "topic"
