# Commands 

The './bin/emqx_ctl' command line could be used to query and administrate the *EMQ X* broker. 

## status 

Show running status of the broker: 
    
    
    $ ./bin/emqx_ctl status
    
    Node 'emqx@127.0.0.1' is started
    emqx v3.1.0 is running

## mgmt 

Manages the ` apps ` of the broker. 

mgmt list                    |  List the Apps                 
-----------------------------|--------------------------------
mgmt insert \<AppId> \<Name>   |  Add App for REST API          
mgmt update \<AppId> \<status> |  Update App for REST API       
mgmt lookup \<AppId>          |  Query App details of an AppId 
mgmt delete \<AppId>          |  Delete Apps of an AppId       



### mgmt list 

List the ` apps ` : 
    
    
    $ ./bin/emqx_ctl mgmt list
    app_id: 901abdba8eb8c, secret: MjgzMzQ5MjM1MzUzMTc4MjgyMjE3NzU4ODcwMDg0NjQ4OTG, name: hello, desc: , status: true, expired: undefined

### mgmt insert \<AppId> \<Name>

Add an app for REST API: 
    
    
    $ ./bin/emqx_ctl mgmt insert dbcb6e023370b world
    AppSecret: MjgzMzQ5MjYyMTY3ODk4MjA5NzMwODExODMxMDM1NDk0NDA

### mgmt update \<AppId> \<status>

mgmt update \<AppId> \<status>: 
    
    
    $ ./bin/emqx_ctl mgmt update dbcb6e023370b stop
    update successfully.

### mgmt lookup \<AppId>

Query details of an app: 
    
    
    $ ./bin/emqx_ctl mgmt lookup dbcb6e023370b
    app_id: dbcb6e023370b
    secret: MjgzMzQ5MjYyMTY3ODk4MjA5NzMwODExODMxMDM1NDk0NDA
    name: world
    desc: Application user
    status: stop
    expired: undefined

### mgmt delete \<AppId>

Delete an app: 
    
    
    $ ./bin/emqx_ctl mgmt delete dbcb6e023370b
    ok

## broker 

Query basic information, statistics and metrics of the broker. 

broker         |  Show version, description, uptime of the broker                                 
---------------|----------------------------------------------------------------------------------
broker stats   |  Show statistics of client, session, topic, subscription and route of the broker 
broker metrics |  Show metrics of MQTT bytes, packets, messages sent/received.                    



Query version, description and uptime of the broker: 
    
    
    $ ./bin/emqx_ctl broker
    
    sysdescr  : EMQ X Broker
    version   : v3.1.0
    uptime    : 25 seconds
    datetime  : 2019-04-29 10:42:10

### broker stats 

Query statistics of MQTT Connections, Sessions, Topics, Subscriptions and Routes: 
    
    
    $ ./bin/emqx_ctl broker stats
    
    actions/max                   : 2
    connections/count             : 1
    connections/max               : 1
    resources/max                 : 0
    retained/count                : 2
    retained/max                  : 2
    routes/count                  : 0
    routes/max                    : 0
    rules/max                     : 0
    sessions/count                : 0
    sessions/max                  : 0
    sessions/persistent/count     : 0
    sessions/persistent/max       : 0
    suboptions/max                : 0
    subscribers/count             : 0
    subscribers/max               : 1
    subscriptions/count           : 1
    subscriptions/max             : 0
    subscriptions/shared/count    : 0
    subscriptions/shared/max      : 0
    topics/count                  : 0
    topics/max                    : 0

### broker metrics 

Query metrics of Bytes, MQTT Packets and Messages(sent/received): 
    
    
    $ ./bin/emqx_ctl broker metrics
    
    bytes/received          : 0
    bytes/sent              : 0
    messages/dropped        : 0
    messages/expired        : 0
    messages/forward        : 0
    messages/qos0/received  : 0
    messages/qos0/sent      : 0
    messages/qos1/received  : 0
    messages/qos1/sent      : 0
    messages/qos2/dropped   : 0
    messages/qos2/expired   : 0
    messages/qos2/received  : 0
    messages/qos2/sent      : 0
    messages/received       : 0
    messages/retained       : 3
    messages/sent           : 0
    packets/auth            : 0
    packets/connack         : 0
    packets/connect         : 0
    packets/disconnect/recei: 0
    packets/disconnect/sent : 0
    packets/pingreq         : 0
    packets/pingresp        : 0
    packets/puback/missed   : 0
    packets/puback/received : 0
    packets/puback/sent     : 0
    packets/pubcomp/missed  : 0
    packets/pubcomp/received: 0
    packets/pubcomp/sent    : 0
    packets/publish/received: 0
    packets/publish/sent    : 0
    packets/pubrec/missed   : 0
    packets/pubrec/received : 0
    packets/pubrec/sent     : 0
    packets/pubrel/missed   : 0
    packets/pubrel/received : 0
    packets/pubrel/sent     : 0
    packets/received        : 0
    packets/sent            : 0
    packets/suback          : 0
    packets/subscribe       : 0
    packets/unsuback        : 0
    packets/unsubscribe     : 0

## cluster 

Cluster two or more *EMQ X* brokers: 

cluster join \<Node>        |  Join the cluster               
---------------------------|---------------------------------
cluster leave              |  Leave the cluster              
cluster force-leave \<Node> |  Remove a node from the cluster 
cluster status             |  Query cluster status and nodes 



Suppose we have two *EMQ X* nodes on localhost and we want to cluster them together: 

Folder |  Node            |  MQTT Port 
-------|------------------|------------
emqx1  |  emqx1@127.0.0.1 |  1883      
emqx2  |  emqx2@127.0.0.1 |  2883      



Start emqx1 and emqx2: 
    
    
    $ cd emqx1 && ./bin/emqx start
    
    $ cd emqx2 && ./bin/emqx start

Under emqx2 folder: 
    
    
    $ ./bin/emqx_ctl cluster join emqx1@127.0.0.1
    
    Join the cluster successfully.
    Cluster status: [{running_nodes,['emqx1@127.0.0.1','emqx2@127.0.0.1']}]

Query cluster status: 
    
    
    $ ./bin/emqx_ctl cluster status
    
    Cluster status: [{running_nodes,['emqx2@127.0.0.1','emqx1@127.0.0.1']}]

Message route between nodes: 
    
    
    # Subscribe topic 'x' on emqx1 node
    $ mosquitto_sub -t x -q 1 -p 1883
    
    # Publish to topic 'x' on emqx2 node
    $ mosquitto_pub -t x -q 1 -p 2883 -m hello

emqx2 leaves the cluster: 
    
    
    $ cd emqx2 && ./bin/emqx_ctl cluster leave

Or remove emqx2 from the cluster on emqx1 node: 
    
    
    $ cd emqx1 && ./bin/emqx_ctl cluster force-leave emqx2@127.0.0.1

## acl 

reload acl.conf: 
    
    
    $ ./bin/emqx_ctl acl reload

## clients 

Query MQTT clients connected to the broker: 

clients list            |  List all MQTT clients   
------------------------|--------------------------
clients show \<ClientId> |  Show an MQTT Client     
clients kick \<ClientId> |  Kick out an MQTT client 



### clients list 

Query all MQTT clients connected to the broker: 
    
    
    $ ./bin/emqx_ctl clients list
    
    Connection(mosqsub/43832-airlee.lo, clean_start=true, username=test, peername=127.0.0.1:64896, connected_at=1452929113)
    Connection(mosqsub/44011-airlee.lo, clean_start=true, username=test, peername=127.0.0.1:64961, connected_at=1452929275)
    ...

Properties of the Client: 

clean_sess   |  Clean Session Flag                                
-------------|----------------------------------------------------
username     |  Username of the client                            
peername     |  Peername of the TCP connection                    
connected_at |  The timestamp when client connected to the broker 



### clients show \<ClientId>

Show a specific MQTT Client: 
    
    
    $ ./bin/emqx_ctl clients show "mosqsub/43832-airlee.lo"
    
    Connection(mosqsub/43832-airlee.lo, clean_sess=true, username=test, peername=127.0.0.1:64896, connected_at=1452929113)

### clients kick \<ClientId>

Kick out a MQTT Client: 
    
    
    $ ./bin/emqx_ctl clients kick "clientid"

## sessions 

Query all MQTT sessions. The broker will create a session for each MQTT client. 

sessions list            |  List all Sessions 
-------------------------|--------------------
sessions show \<ClientId> |  Show a session    



### sessions list 

Query all sessions: 
    
    
    $ ./bin/emqx_ctl sessions list
    
    Session(clientid, clean_start=true, expiry_interval=0, subscriptions_count=0, max_inflight=32, inflight=0, mqueue_len=0, mqueue_dropped=0, awaiting_rel=0, deliver_msg=0, enqueue_msg=0, created_at=1553760799)
    Session(mosqsub/44101-airlee.lo, clean_start=true, expiry_interval=0, subscriptions_count=0, max_inflight=32, inflight=0, mqueue_len=0, mqueue_dropped=0, awaiting_rel=0, deliver_msg=0, enqueue_msg=0, created_at=1553760314)

Properties of the Session: 

clean_start         |  Clean sessions                         
--------------------|-----------------------------------------
expiry_interval     |  Session expiration interval            
subscriptions_count |  Current subscription                   
max_inflight        |  Max inflight window                    
inflight            |  Inflight window                        
mqueue_len          |  Current cached messages.               
mqueue_dropped      |  Session dropped messages               
awaiting_rel        |  QoS2 msg waiting client to send PUBREL 
deliver_msg         |  Deliver messages count                 
enqueue_msg         |  Number of cached messages              
created_at          |  Session create timestamp               



### sessions show \<ClientId>

Show a session: 
    
    
    $ ./bin/emqx_ctl sessions show clientid
    
    Session(clientid, clean_start=true, expiry_interval=0, subscriptions_count=0, max_inflight=32, inflight=0, mqueue_len=0, mqueue_dropped=0, awaiting_rel=0, deliver_msg=0, enqueue_msg=0, created_at=1553760799)

## routes 

Show routing table of the broker. 

routes list         |  List all Routes 
--------------------|------------------
routes show \<Topic> |  Show a Route    



### routes list 

List all routes: 
    
    
    $ ./bin/emqx_ctl routes list
    
    t2/# -> emqx2@127.0.0.1
    t/+/x -> emqx2@127.0.0.1,emqx@127.0.0.1

### routes show \<Topic>

Show a route: 
    
    
    $ ./bin/emqx_ctl routes show t/+/x
    
    t/+/x -> emqx2@127.0.0.1,emqx@127.0.0.1

## subscriptions 

Query the subscription table of the broker: 

subscriptions list                         |  Query all subscriptions         
-------------------------------------------|----------------------------------
subscriptions show \<ClientId>              |  Show the a client subscriptions 
subscriptions add \<ClientId> \<Topic> \<QoS> |  Manually add a subscription     
subscriptions del \<ClientId> \<Topic>       |  Manually delete a subscription  



### subscriptions list 

Query all subscriptions: 
    
    
    $ ./bin/emqx_ctl subscriptions list
    
    mosqsub/91042-airlee.lo -> t/y:1
    mosqsub/90475-airlee.lo -> t/+/x:2

### subscriptions show \<ClientId>

Show the subscriptions of an MQTT client: 
    
    
    $ ./bin/emqx_ctl subscriptions show 'mosqsub/90475-airlee.lo'
    
    mosqsub/90475-airlee.lo -> t/+/x:2

### subscriptions add \<ClientId> \<Topic> \<QoS>

Manually add a subscription: 
    
    
    $ ./bin/emqx_ctl subscriptions add 'mosqsub/90475-airlee.lo' '/world' 1
    
    ok

### subscriptions del \<ClientId> \<Topic>

Manually delete a subscription: 
    
    
    $ ./bin/emqx_ctl subscriptions del 'mosqsub/90475-airlee.lo' '/world'
    
    ok

## plugins 

List, load、unload、reload plugins of *EMQ X* broker. 

plugins list            |  List all plugins 
------------------------|-------------------
plugins load \<Plugin>   |  Load Plugin      
plugins unload \<Plugin> |  Unload (Plugin)  
plugins reload \<Plugin> |  Reload (Plugin)  



::: tip Tip
When modifying the configuration file of a plugin, you need to execute the ` reload ` command if it needs to take effect immediately. Because the ` unload/load ` command does not compile new configuration files 
:::

### plugins list 

List all plugins: 
    
    
    $ ./bin/emqx_ctl plugins list
    
    Plugin(emqx_auth_clientid, version=v3.1.0, description=EMQ X Authentication with ClientId/Password, active=false)
    Plugin(emqx_auth_http, version=v3.1.0, description=EMQ X Authentication/ACL with HTTP API, active=false)
    Plugin(emqx_auth_jwt, version=v3.1.0, description=EMQ X Authentication with JWT, active=false)
    Plugin(emqx_auth_ldap, version=v3.1.0, description=EMQ X Authentication/ACL with LDAP, active=false)
    Plugin(emqx_auth_mongo, version=v3.1.0, description=EMQ X Authentication/ACL with MongoDB, active=false)
    Plugin(emqx_auth_mysql, version=v3.1.0, description=EMQ X Authentication/ACL with MySQL, active=false)
    Plugin(emqx_auth_pgsql, version=v3.1.0, description=EMQ X Authentication/ACL with PostgreSQL, active=false)
    Plugin(emqx_auth_redis, version=v3.1.0, description=EMQ X Authentication/ACL with Redis, active=false)
    Plugin(emqx_auth_username, version=v3.1.0, description=EMQ X Authentication with Username and Password, active=false)
    Plugin(emqx_coap, version=v3.1.0, description=EMQ X CoAP Gateway, active=false)
    Plugin(emqx_dashboard, version=v3.1.0, description=EMQ X Web Dashboard, active=true)
    Plugin(emqx_delayed_publish, version=v3.1.0, description=EMQ X Delayed Publish, active=false)
    Plugin(emqx_lua_hook, version=v3.1.0, description=EMQ X Lua Hooks, active=false)
    Plugin(emqx_lwm2m, version=v3.1.0, description=EMQ X LwM2M Gateway, active=false)
    Plugin(emqx_management, version=v3.1.0, description=EMQ X Management API and CLI, active=true)
    Plugin(emqx_plugin_template, version=v3.1.0, description=EMQ X Plugin Template, active=false)
    Plugin(emqx_psk_file, version=v3.1.0, description=EMQX PSK Plugin from File, active=false)
    Plugin(emqx_recon, version=v3.1.0, description=EMQ X Recon Plugin, active=true)
    Plugin(emqx_reloader, version=v3.1.0, description=EMQ X Reloader Plugin, active=false)
    Plugin(emqx_retainer, version=v3.1.0, description=EMQ X Retainer, active=true)
    Plugin(emqx_rule_engine, version=v3.1.0, description=EMQ X Rule Engine, active=true)
    Plugin(emqx_sn, version=v3.1.0, description=EMQ X MQTT SN Plugin, active=false)
    Plugin(emqx_statsd, version=v3.1.0, description=Statsd for EMQ X, active=false)
    Plugin(emqx_stomp, version=v3.1.0, description=EMQ X Stomp Protocol Plugin, active=false)
    Plugin(emqx_web_hook, version=v3.1.0, description=EMQ X Webhook Plugin, active=false)

Properties of a plugin: 

version     |  Plugin Version          
------------|--------------------------
description |  Plugin Description      
active      |  If the plugin is Loaded 



### plugins load \<Plugin>

Load a plugin: 
    
    
    $ ./bin/emqx_ctl plugins load emqx_lua_hook
    
    Start apps: [emqx_lua_hook]
    Plugin emqx_lua_hook loaded successfully.

### plugins unload \<Plugin>

Unload a plugin: 
    
    
    $ ./bin/emqx_ctl plugins unload emqx_lua_hook
    
    Plugin emqx_lua_hook unloaded successfully.

### plugins reload \<Plugin>

Reload a plugin: 
    
    
    $ ./bin/emqx_ctl plugins reload emqx_lua_hook
    
    Plugin emqx_lua_hook reloaded successfully.

## bridges 

Bridges command is used to create bridges between multiple *EMQ X* nodes: 
    
    
    ---------                     ---------
    Publisher --> | node1 | --Bridge Forward--> | node2 | --> Subscriber
    ---------                     ---------

bridges list                                  |  List bridges                      
----------------------------------------------|------------------------------------
bridges start \<Name>                          |  Start a bridge                    
bridges stop \<Name>                           |  Stop a bridge                     
bridges forwards \<Name>                       |  Show a bridge forward topic       
bridges add-forward \<Name> \<Topic>            |  Add bridge forward topic          
bridges del-forward \<Name> \<Topic>            |  Delete bridge forward topic       
bridges subscriptions \<Name>                  |  Show a bridge subscriptions topic 
bridges add-subscription \<Name> \<Topic> \<QoS> |  Add bridge subscriptions topic    
bridges del-subscription \<Name> \<Topic>       |  Delete bridge subscriptions topic 



The configuration items for bridges are in the emqx/emqx.config file. 

### bridges list 

List all bridge status: 
    
    
    $ ./bin/emqx_ctl bridges list
    
    name: emqx     status: Stopped

### bridges start \<Name>

Start a bridge : 
    
    
    $ ./bin/emqx_ctl bridges start emqx
    
    Start bridge successfully.

### bridges stop \<Name>

Stop a bridge: 
    
    
    $ ./bin/emqx_ctl bridges stop emqx
    
    Stop bridge successfully.

### bridges forwards \<Name>

Show a bridge forward topic: 
    
    
    $ ./bin/emqx_ctl bridges forwards emqx
    
    topic:   sensor/#

### bridges add-forward \<Name> \<Topic>

Add bridge forward topic: 
    
    
    $ ./bin/emqx_ctl bridges add-forward emqx device_status/#
    
    Add-forward topic successfully.

### bridges del-forward \<Name> \<Topic>

Delete bridge forward topic: 
    
    
    $ ./bin/emqx_ctl bridges del-forward emqx device_status/#
    
    Del-forward topic successfully.

### bridges subscriptions \<Name>

Show a bridge subscriptions topic: 
    
    
    $ ./bin/emqx_ctl bridges subscriptions emqx
    
    topic: cmd/topic, qos: 1

### bridges add-subscription \<Name> \<Topic> \<QoS>

Add bridge subscriptions topic: 
    
    
    $ ./bin/emqx_ctl bridges add-subscription emqx cmd/topic 1
    
    Add-subscription topic successfully.

### bridges del-subscription \<Name> \<Topic>

Delete bridge subscriptions topic: 
    
    
    $ ./bin/emqx_ctl bridges del-subscription emqx cmd/topic
    
    Del-subscription topic successfully.

## vm 

Query the load, cpu, memory, processes and IO information of the Erlang VM. 

vm         |  Query all                        
-----------|-----------------------------------
vm all     |  Query all                        
vm load    |  Query VM Load                    
vm memory  |  Query Memory Usage               
vm process |  Query Number of Erlang Processes 
vm io      |  Query Max Fds of VM              
vm ports   |  Query VM ports                   



### vm all 

Query all VM information, including load, memory, number of Erlang processes: 
    
    
    cpu/load1               : 4.22
    cpu/load5               : 3.29
    cpu/load15              : 3.16
    memory/total            : 99995208
    memory/processes        : 38998248
    memory/processes_used   : 38938520
    memory/system           : 60996960
    memory/atom             : 1189073
    memory/atom_used        : 1173808
    memory/binary           : 100336
    memory/code             : 25439961
    memory/ets              : 7161128
    process/limit           : 2097152
    process/count           : 315
    io/max_fds              : 10240
    io/active_fds           : 0
    ports/count             : 18
    ports/limit             : 1048576

### vm load 

Query load: 
    
    
    $ ./bin/emqx_ctl vm load
    
    cpu/load1               : 2.21
    cpu/load5               : 2.60
    cpu/load15              : 2.36

### vm memory 

Query memory: 
    
    
    $ ./bin/emqx_ctl vm memory
    
    memory/total            : 23967736
    memory/processes        : 3594216
    memory/processes_used   : 3593112
    memory/system           : 20373520
    memory/atom             : 512601
    memory/atom_used        : 491955
    memory/binary           : 51432
    memory/code             : 13401565
    memory/ets              : 1082848

### vm process 

Query number of erlang processes: 
    
    
    $ ./bin/emqx_ctl vm process
    
    process/limit           : 2097152
    process/count           : 314

### vm io 

Query max, active file descriptors of IO: 
    
    
    $ ./bin/emqx_ctl vm io
    
    io/max_fds              : 10240
    io/active_fds           : 0

### vm ports 

Query VM ports: 
    
    
    $ ./bin/emqx_ctl vm ports
    
    ports/count           : 18
    ports/limit           : 1048576

## mnesia 

Query the mnesia database system status. 

## log 

The log command is used to set the log level. Visit the [ Documentation of logger ](http://erlang.org/doc/apps/kernel/logger_chapter.html) for more information 

log set-level \<Level>                      |  Set the primary log level and all Handlers log levels 
-------------------------------------------|--------------------------------------------------------
log primary-level                          |  Show the main log level                               
log primary-lelvel \<Level>                 |  Set the primary log level                             
log handlers list                          |  Show all currently useing Hanlders                    
log handlers set-level \<HandlerId> \<Level> |  Set the log level for the specified Hanlder           



### log set-level \<Level>

Set the primary log level and all Handlers log levels: 
    
    
    $ ./bin/emqx_ctl log set-level debug
    
    debug

### log primary-level 

Show the main log level: 
    
    
    $ ./bin/emqx_ctl log primary-level
    
    debug

### log primary-level \<Level>

Set the primary log level: 
    
    
    $ ./bin/emqx_ctl log primary-level info
    
    info

### log handlers list 

Show all logger handlers: 
    
    
    $ ./bin/emqx_ctl log handlers list
    
    LogHandler(id=emqx_logger_handler, level=debug, destination=unknown)
    LogHandler(id=file, level=debug, destination=log/emqx.log)
    LogHandler(id=default, level=debug, destination=console)

### log handlers set-level \<HandlerId> \<Level>

Set the log level for a specified handler: 
    
    
    $ ./bin/emqx_ctl log handlers set-level emqx_logger_handler error
    
    error

## trace 

The trace command is used to trace a client or a topic and redirect related log messages to a file. 

trace list                                     |  Query all open traces 
-----------------------------------------------|------------------------
trace start client \<ClientId> \<File> [\<Level>] |  Start Client trace    
trace stop client \<ClientId>                   |  Stop Client trace     
trace start topic \<Topic> \<File> [\<Level>]     |  Start Topic trace     
trace stop topic \<Topic>                       |  Stop Topic trace      



::: tip Tip
Before using trace, you need to set the primary logger level to a value low enough. To improve system performance, the default primary log level is error. 
:::

### trace start client \<ClientId> \<File> [\<Level>] 

Start Client trace: 
    
    
    $ ./bin/emqx_ctl log primary-level debug
    
    debug
    
    $ ./bin/emqx_ctl trace start client clientid log/clientid_trace.log
    
    trace client clientid successfully
    
    $ ./bin/emqx_ctl trace start client clientid2 log/clientid2_trace.log error
    
    trace client_id clientid2 successfully

### trace stop client \<ClientId>

Stop Client trace: 
    
    
    $ ./bin/emqx_ctl trace stop client clientid
    
    stop tracing client_id clientid successfully

### trace start topic \<Topic> \<File> [\<Level>] 

Start Topic trace: 
    
    
    $ ./bin/emqx_ctl log primary-level debug
    
    debug
    
    $ ./bin/emqx_ctl trace start topic topic log/topic_trace.log
    
    trace topic topic successfully
    
    $ ./bin/emqx_ctl trace start topic topic2 log/topic2_trace.log error
    
    trace topic topic2 successfully

### trace stop topic \<Topic>

Stop Topic trace: 
    
    
    $ ./bin/emqx_ctl trace topic topic off
    
    stop tracing topic topic successfully

### trace list 

Query all open traces: 
    
    
    $ ./bin/emqx_ctl trace list
    
    Trace(client_id=clientid2, level=error, destination="log/clientid2_trace.log")
    Trace(topic=topic2, level=error, destination="log/topic2_trace.log")

## listeners 

The listeners command is used to query open TCP service listeners. 

listeners                     |  Show all the TCP listeners 
------------------------------|-----------------------------
listeners stop \<Proto> \<Port> |  Stop listener port         



### listeners list 

Show all the TCP listeners: 
    
    
    $ ./bin/emqx_ctl listeners
    
    listener on mqtt:ssl:8883
      acceptors       : 16
      max_conns       : 102400
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:tcp:0.0.0.0:1883
      acceptors       : 8
      max_conns       : 1024000
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:tcp:127.0.0.1:11883
      acceptors       : 4
      max_conns       : 1024000
      current_conn    : 2
      shutdown_count  : []
    listener on http:dashboard:18083
      acceptors       : 2
      max_conns       : 512
      current_conn    : 0
      shutdown_count  : []
    listener on http:management:8080
      acceptors       : 2
      max_conns       : 512
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:ws:8083
      acceptors       : 2
      max_conns       : 102400
      current_conn    : 0
      shutdown_count  : []
    listener on mqtt:wss:8084
      acceptors       : 2
      max_conns       : 16
      current_conn    : 0
      shutdown_count  : []

listener parameters: 

acceptors       |  TCP Acceptor Pool                    
----------------|---------------------------------------
max_clients     |  Max number of clients                
current_clients |  Count of current clients             
shutdown_count  |  Statistics of client shutdown reason 



### listeners stop \<Proto> \<Port>

Stop listener port: 
    
    
    $ ./bin/emqx_ctl listeners stop mqtt:tcp 0.0.0.0:1883
    
    Stop mqtt:tcp listener on 0.0.0.0:1883 successfully.

## Rule Engine 

## rules 

rules list                                                |  List all rules 
----------------------------------------------------------|-----------------
rules show \<RuleId>                                       |  Show a rule    
rules create \<name> \<hook> \<sql> \<actions> [-d [\<descr>]] |  Create a rule  
rules delete \<RuleId>                                     |  Delete a rule  



### rules create 

Create a new rule: 
    
    
    ## create a simple rule for testing, printing all messages sent to topic 't/a'
    $ ./bin/emqx_ctl rules create \
      'test1' \
      'message.publish' \
      'select * from "t/a"' \
      '[{"name":"built_in:inspect_action", "params": {"a": 1}}]' \
      -d 'Rule for debug'
    
    Rule test1:1556242324634254201 created

::: tip Tip
A rule is identified by a server-side-generated ID. So run 'rules create' multiple times using the same name will create multiple rules with the same name but different IDs. 
:::

### rules list 

List all rules: 
    
    
    $ ./bin/emqx_ctl rules list
    
    rule(id='test1:1556242324634254201', name='test1', for='message.publish', rawsql='select * from "t/a"', actions=[{"name":"built_in:inspect_action","params":{"a":1}}], enabled='true', description='Rule for debug')

### rules show 

Query a rule: 
    
    
    ## Query a rule by whose ID is 'test1:1556242324634254201'
    $ ./bin/emqx_ctl rules show 'test1:1556242324634254201'
    
    rule(id='test1:1556242324634254201', name='test1', for='message.publish', rawsql='select * from "t/a"', actions=[{"name":"built_in:inspect_action","params":{"a":1}}], enabled='true', description='Rule for debug')

### rules delete 

Delete a rule: 
    
    
    ## Delete a rule whose ID is 'test1:1556242324634254201'
    $ ./bin/emqx_ctl rules delete 'test1:1556242324634254201'
    
    ok

## rule-actions 

rule-actions list [-t [\<type>]] [-k [\<hook>]] |  List all actions   
----------------------------------------------|---------------------
rule-actions show \<ActionId>                  |  Show a rule action 



Important 

Actions could be built-in actions, or provided by emqx plugins, but cannot be added/deleted dynamically via CLI/API. 

### rule-actions show 

Query actions: 
    
    
    ## Query the action named 'built_in:inspect_action'
    $ ./bin/emqx_ctl rule-actions show 'built_in:inspect_action'
    
    action(name='built_in:inspect_action', app='emqx_rule_engine', for='$any', type='built_in', params=#{}, description='Inspect the details of action params for debug purpose')

### rule-actions list 

List actions by hook or resource-type: 
    
    
    ## List all the actions
    $ ./bin/emqx_ctl rule-actions list
    
    action(name='built_in:republish_action', app='emqx_rule_engine', for='message.publish', type='built_in', params=#{target_topic => #{description => <<"Repubilsh the message to which topic">>,format => topic,required => true,title => <<"To Which Topic">>,type => string}}, description='Republish a MQTT message to a another topic')
    action(name='web_hook:event_action', app='emqx_web_hook', for='$events', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string},template => #{description => <<"The payload template to be filled with variables before sending messages">>,required => false,schema => #{},title => <<"Payload Template">>,type => object}}, description='Forward Events to Web Server')
    action(name='web_hook:publish_action', app='emqx_web_hook', for='message.publish', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string}}, description='Forward Messages to Web Server')
    action(name='built_in:inspect_action', app='emqx_rule_engine', for='$any', type='built_in', params=#{}, description='Inspect the details of action params for debug purpose')
    
    ## List all the hooks whose resource type is web_hook
    $ ./bin/emqx_ctl rule-actions list -t web_hook
    
    action(name='web_hook:event_action', app='emqx_web_hook', for='$events', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string},template => #{description => <<"The payload template to be filled with variables before sending messages">>,required => false,schema => #{},title => <<"Payload Template">>,type => object}}, description='Forward Events to Web Server')
    action(name='web_hook:publish_action', app='emqx_web_hook', for='message.publish', type='web_hook', params=#{'$resource' => #{description => <<"Bind a resource to this action">>,required => true,title => <<"Resource ID">>,type => string}}, description='Forward Messages to Web Server')
    
    ## List all the hooks whose hook type matched to 'client.connected'
    $ ./bin/emqx_ctl rule-actions list -k 'client.connected'
    
    action(name='built_in:inspect_action', app='emqx_rule_engine', for='$any', type='built_in', params=#{}, description='Inspect the details of action params for debug purpose')

## resources 

emqx_ctl resources create \<name> \<type> [-c [\<config>]] [-d [\<descr>]] |  Create a resource  
-----------------------------------------------------------------------|---------------------
resources list [-t \<ResourceType>]                                     |  List all resources 
resources show \<ResourceId>                                            |  Show a resource    
resources delete \<ResourceId>                                          |  Delete a resource  



### resources create 

Create a new resource: 
    
    
    $ ./bin/emqx_ctl resources create 'webhook1' 'web_hook' -c '{"url": "http://host-name/chats"}' -d 'forward msgs to host-name/chats'
    
    Resource web_hook:webhook1 created

### resources list 

List all the resources: 
    
    
    $ ./bin/emqx_ctl resources list
    
    resource(id='web_hook:webhook1', name='webhook1', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, attrs=undefined, description='forward msgs to host-name/chats')

### resources list by type 

List resources by resource-type: 
    
    
    $ ./bin/emqx_ctl resources list --type 'debug_resource_type'
    
    resource(id='web_hook:webhook1', name='webhook1', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, attrs=undefined, description='forward msgs to host-name/chats')

### resources show 

Query resources: 
    
    
    $ ./bin/emqx_ctl resources show 'web_hook:webhook1'
    
    resource(id='web_hook:webhook1', name='webhook1', type='web_hook', config=#{<<"url">> => <<"http://host-name/chats">>}, attrs=undefined, description='forward msgs to host-name/chats')

### resources delete 

Delete resources: 
    
    
    $ ./bin/emqx_ctl resources delete 'web_hook:webhook1'
    
    ok

## resource-types 

resource-types list        |  List all resource-types 
---------------------------|--------------------------
resource-types show \<Type> |  Show a resource-type    



Important 

Resource types could be built-in resource types, or provided by emqx plugins, but cannot be added/deleted dynamically via CLI/API. 

### resource-types list 

List all the resource types: 
    
    
    ./bin/emqx_ctl resource-types list
    
    resource_type(name='built_in', provider='emqx_rule_engine', params=#{}, on_create={emqx_rule_actions,on_resource_create}, description='The built in resource type for debug purpose')
    resource_type(name='web_hook', provider='emqx_web_hook', params=#{headers => #{default => #{},description => <<"Request Header">>,schema => #{},title => <<"Request Header">>,type => object},method => #{default => <<"POST">>,description => <<"Request Method">>,enum => [<<"PUT">>,<<"POST">>],title => <<"Request Method">>,type => string},url => #{description => <<"Request URL">>,format => url,required => true,title => <<"Request URL">>,type => string}}, on_create={emqx_web_hook_actions,on_resource_create}, description='WebHook Resource')

### resource-types show 

Query a resource type by name: 
    
    
    $ ./bin/emqx_ctl resource-types show built_in
    
    resource_type(name='built_in', provider='emqx_rule_engine', params=#{}, on_create={emqx_rule_actions,on_resource_create}, description='The built in resource type for debug purpose')

## recon 

recon memory          |  recon_alloc:memory/2                             
----------------------|---------------------------------------------------
recon allocated       |  recon_alloc:memory(allocated_types, current/max) 
recon bin_leak        |  recon:bin_leak(100)                              
recon node_stats      |  recon:node_stats(10, 1000)                       
recon remote_load Mod |  recon:remote_load(Mod)                           



See [ Documentation for recon ](http://ferd.github.io/recon/) for more information. 

### recon memory 

recon_alloc:memory/2: 
    
    
    $ ./bin/emqx_ctl recon memory
    
    usage/current       : 0.810331960305788
    usage/max           : 0.7992495929358717
    used/current        : 84922296
    used/max            : 122519208
    allocated/current   : 104345600
    allocated/max       : 153292800
    unused/current      : 19631520
    unused/max          : 30773592

### recon allocated 

recon_alloc:memory(allocated_types, current/max): 
    
    
    $ ./bin/emqx_ctl recon allocated
    
    binary_alloc/current: 425984
    driver_alloc/current: 425984
    eheap_alloc/current : 4063232
    ets_alloc/current   : 3833856
    fix_alloc/current   : 1474560
    ll_alloc/current    : 90439680
    sl_alloc/current    : 163840
    std_alloc/current   : 2260992
    temp_alloc/current  : 655360
    binary_alloc/max    : 4907008
    driver_alloc/max    : 425984
    eheap_alloc/max     : 25538560
    ets_alloc/max       : 5931008
    fix_alloc/max       : 1736704
    ll_alloc/max        : 90439680
    sl_alloc/max        : 20348928
    std_alloc/max       : 2260992
    temp_alloc/max      : 1703936

### recon bin_leak 

recon:bin_leak(100): 
    
    
    $ ./bin/emqx_ctl recon bin_leak
    
    {\<10623.1352.0>,-3,
     [cowboy_clock,
      {current_function,{gen_server,loop,7}},
      {initial_call,{proc_lib,init_p,5}}]}
    {\<10623.3865.0>,0,
     [{current_function,{recon_lib,proc_attrs,2}},
      {initial_call,{erlang,apply,2}}]}
    {\<10623.3863.0>,0,
     [{current_function,{dist_util,con_loop,2}},
      {initial_call,{inet_tcp_dist,do_accept,7}}]}
      ...

### recon node_stats 

recon:node_stats(10, 1000): 
    
    
    $ ./bin/emqx_ctl recon node_stats
    
    {[{process_count,302},
      {run_queue,0},
      {memory_total,88925536},
      {memory_procs,27999296},
      {memory_atoms,1182843},
      {memory_bin,24536},
      {memory_ets,7163216}],
     [{bytes_in,62},
      {bytes_out,458},
      {gc_count,4},
      {gc_words_reclaimed,3803},
      {reductions,3036},
      {scheduler_usage,[{1,9.473889959272245e-4},
                        {2,5.085983030767205e-5},
                        {3,5.3851477624711046e-5},
                        {4,7.579021269127057e-5},
                        {5,0.0},
                        {6,0.0},
                        {7,0.0},
                        {8,0.0}]}]}
    ...

### recon remote_load Mod 

recon:remote_load(Mod): 
    
    
    $ ./bin/emqx_ctl recon remote_load

## retainer 

retainer info   |  show retainer messages count 
----------------|-------------------------------
retainer topics |  show all retainer topic      
retainer clean  |  Clear all retainer messages  



### retainer info 

Show count of retained messages: 
    
    
    $ ./bin/emqx_ctl retainer info
    
    retained/total: 3

### retainer topics 

Show retained topics: 
    
    
    $ ./bin/emqx_ctl retainer topics
    
    $SYS/brokers/emqx@127.0.0.1/version
    $SYS/brokers/emqx@127.0.0.1/sysdescr
    $SYS/brokers

### retainer clean 

Clean all retained messages: 
    
    
    $ ./bin/emqx_ctl retainer clean
    
    Cleaned 3 retained messages

## admins 

The 'admins' CLI is used to add/del admin account, which is registered on the emqx dashboard. 

admins add \<Username> \<Password> \<Tags> |  Create admin account 
----------------------------------------|-----------------------
admins passwd \<Username> \<Password>     |  Reset admin password 
admins del \<Username>                   |  Delete admin account 



### admins add \<Username> \<Password> \<Tags>

Create admin account: 
    
    
    $ ./bin/emqx_ctl admins add root public test
    
    ok

### admins passwd \<Username> \<Password>

Reset admin account: 
    
    
    $ ./bin/emqx_ctl admins passwd root private
    
    ok

### admins del \<Username>

Delete admin account: 
    
    
    $ ./bin/emqx_ctl admins del root
    
    ok
