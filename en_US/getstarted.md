# Get Started 

## EMQ X R3.2 Message Broker Introduction 

EMQ X (Erlang/Enterprise/Elastic MQTT Broker) is an open source IoT MQTT message broker based on the Erlang/OTP platform. Erlang/OTP is an excellent Soft-Realtime, Low-Latency and Distributed development platform. MQTT is a lightweight message exchange protocol using publish-subscribe pattern. 

*EMQ X* is designed for massive clients access and realizes fast and low-latency message routing between massive physical network devices: 

  1. Stable to host large-scale MQTT client connections, and a single-server node supports millions of connections. 
  2. Distributed cluster, fast and low-latency message routing, and single-cluster supports tens of thousands of routes. 
  3. Extensible, support customized plugins, such as authentication and other functions. 
  4. Comprehensive IoT protocol support, including MQTT, MQTT-SN, CoAP, LwM2M, and other TCP/UDP based proprietary protocol. 



## MQTT and Publish-Subscribe Messaging Pattern 

MQTT message exchanging is based on the Publish-Subscribe pattern which is essentially different from the HTTP Request/Response pattern. 

The **Subscriber** subscribes to a **Topic** from the **Broker** . The broker forwards the messages under a topic to all subscribers of this particular topic. 

**'/'** is used as a separator to distinguish between different levels in a topic. Topics can contain wildcards, there are two wildcards allowed in MQTT topic: 1)the **'#'** for multiple topic levels, and 2) the **'+'** for single topic level. Topics that contain the wildcard '+' or '#' are also known as **Topic Filters** ; those that do not contain wildcards are called Topic Names sometimes. For example: 
    
    
    sensor/1/temperature
    
    chat/room/subject
    
    presence/user/feng
    
    sensor/1/#
    
    sensor/+/temperature
    
    uber/drivers/joe/inbox

::: tip Tip
'+'matches single level and'#' matches multiple levels (must be at the end). 
Publishers can only post messages to 'topic names', and Subscribers can subscribe to multiple topic names by subscribing to 'topic filters'. 
:::

## EMQ X R3.1 Message Broker Features List 

  * Complete MQTT V3.1/V3.1.1 and V5.0 protocol specification support 
  * Three QoS level: QoS0, QoS1 and QoS2 
  * Persistent session and offline message 
  * Retained message 
  * Last Will message 
  * TCP/SSL 
  * MQTT/WebSocket/SSL 
  * HTTP message publishing interface 
  * $SYS/# (system topics) 
  * client status via query and subscription 
  * Authentication based on Client ID, Username or IP address 
  * integration with Redis, MySQL, PostgreSQL, MongoDB, HTTP and LDAP (authentication and authorization) 
  * Browser cookie authentication 
  * Access Control (ACL) based on client ID, IP address, and username 
  * Cluster 
  * Diverse cluster node discovery methods: manual, mcast, dns, etcd, k8s and etc 
  * Auto healing of network split 
  * Message rate limit 
  * Connection rate limit 
  * Configuring nodes by zone 
  * Bridging of multiple brokers via RPC 
  * Bridging of multiple brokers via MQTT 
  * Stomp protocol support 
  * MQTT-SN protocol support 
  * CoAP protocol support 
  * SockJS support 
  * Delayed publish ($delay/topic) 
  * Flapping detection 
  * Blacklist support 
  * Shared subscription ($share/\<group>/topic) 
  * TLS/PSK support 
  * Rule engine 



## Download and Start EMQ X in Five Minutes 

For each version EMQ X will be released as installation packages or zip packages for diverse OSes and platforms, including CentOS, Ubuntu, Debian, FreeBSD, macOS, Windows and etc. It is also available as Docker image. 

Download address: [ https://www.emqx.io/downloads/broker?osType=Linux ](https://www.emqx.io/downloads/broker?osType=Linux)

Once the package is downloaded and installed (or unzipped), the EQM X is ready to start. Taking the zip package for Mac as an example: 
    
    
    unzip emqx-macosx-v3.2.0.zip && cd emqx
    
    # start emqx
    ./bin/emqx start
    
    # Check the running status
    ./bin/emqx_ctl status
    
    # stop emqx
    ./bin/emqx stop

After EMQ X is started, the MQTT client can connect to it through port 1883. By default, the running log is in the directory of ` log/ ` . 

EMQ X loads the Dashboard plugin and launches the web management console by default. Users can check the broker running status, statistics, connections, sessions, topics, subscriptions, and plugins through the web console. 

Console address: [ http://127.0.0.1:18083，default ](http://127.0.0.1:18083，default) username: admin，password:public 

![image](./_static/images/dashboard.png)

## Open Source MQTT Client Project 

GitHub: [ https://github.com/emqtt ](https://github.com/emqtt)

[ emqttc ](https://github.com/emqtt/emqttc)                   |  Erlang MQTT client library         
--------------------------------------------------------------|-------------------------------------
[ CocoaMQTT ](https://github.com/emqtt/CocoaMQTT)             |  Swift Language MQTT Client Library 
[ QMQTT ](https://github.com/emqtt/qmqtt)                     |  QT framework MQTT client library   
[ emqtt_benchmark ](https://github.com/emqtt/emqtt_benchmark) |  MQTT benchmark tool                



Eclipse Paho: [ https://www.eclipse.org/paho/ ](https://www.eclipse.org/paho/)

MQTT.org: [ https://github.com/mqtt/mqtt.github.io/wiki/libraries ](https://github.com/mqtt/mqtt.github.io/wiki/libraries)
