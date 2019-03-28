
.. _bridge:

=================
节点桥接 (Bridge)
=================

.. _bridge_emqx:

----------------
EMQ X 节点间桥接
----------------

EMQ X 支持 RPC 桥接与 MQTT 桥接两种方式。RPC 桥接只支持消息转发，不支持订阅桥接
远程节点的主题去同步数据，MQTT 桥接同时支持转发和通过订阅数据同步两种方式。

节点间桥接与集群不同，不复制主题树与路由表，只按桥接规则转发 MQTT 消息。

*EMQ X* 消息服务器支持多节点桥接模式互联::

                  ---------                     ---------                     ---------
    Publisher --> | Node1 | --Bridge Forward--> | Node2 | --Bridge Forward--> | Node3 | --> Subscriber
                  ---------                     ---------                     ---------

EMQ X 节点 RPC 桥接配置
-----------------------

假设在本机创建两个 EMQ 节点，并创建一条桥接转发全部传感器(sensor)主题消息:

+---------+---------------------+-----------+
| 目录    | 节点                | MQTT 端口 |
+---------+---------------------+-----------+
| emqx1   | emqx1@127.0.0.1     | 1883      |
+---------+---------------------+-----------+
| emqx2   | emqx2@127.0.0.1     | 2883      |
+---------+---------------------+-----------+

如果是将匹配 topic1/# 和 topic2/# 主题的消息从 emqx1 节点转发到 emqx2 节点，只需在 emqx1
节点做出如下配置::

    ## Bridge address: node name for local bridge, host:port for remote.
    ##
    ## Value: String
    ## Example: emqx@127.0.0.1,  127.0.0.1:1883
    bridge.emqx.address = 127.0.0.1:2883

    ## Mountpoint of the bridge.
    ##
    ## Value: String
    bridge.emqx.mountpoint = bridge/aws/${node}/

    ## Forward message topics
    ##
    ## Value: String
    ## Example: topic1/#,topic2/#
    bridge.emqx.forwards = topic1/#,topic2/#

如果是 RPC 桥接，只需要将配置项的 bridge.emqx.address = 127.0.0.1:2883 改为
bridge.emqx.address = emqx2@127.0.0.1

EMQ X 节点 MQTT 桥接配置
-----------------------

有些配置项只对 mqtt 桥接起作用。比如::

    ## Protocol version of the bridge.
    ##
    ## Value: Enum
    ## - mqttv5
    ## - mqttv4
    ## - mqttv3
    bridge.emqx.proto_ver = mqttv4

    ## The ClientId of a remote bridge.
    ##
    ## Value: String
    bridge.emqx.client_id = bridge_aws

    ## The Clean start flag of a remote bridge.
    ##
    ## Value: boolean
    ## Default: true
    ##
    ## NOTE: Some IoT platforms require clean_start
    ##       must be set to 'true'
    bridge.emqx.clean_start = true

    ## The username for a remote bridge.
    ##
    ## Value: String
    bridge.emqx.username = user

    ## The password for a remote bridge.
    ##
    ## Value: String
    bridge.emqx.password = passwd

    ## Mountpoint of the bridge.
    ##
    ## Value: String
    bridge.emqx.mountpoint = bridge/aws/${node}/

    ## Forward message topics
    ##
    ## Value: String
    ## Example: topic1/#,topic2/#
    bridge.emqx.forwards = topic1/#,topic2/#

    ## Bribge to remote server via SSL.
    ##
    ## Value: on | off
    bridge.emqx.ssl = off

    ## PEM-encoded CA certificates of the bridge.
    ##
    ## Value: File
    bridge.emqx.cacertfile = {{ platform_etc_dir }}/certs/cacert.pem

    ## Client SSL Certfile of the bridge.
    ##
    ## Value: File
    bridge.emqx.certfile = {{ platform_etc_dir }}/certs/client-cert.pem

    ## Client SSL Keyfile of the bridge.
    ##
    ## Value: File
    bridge.emqx.keyfile = {{ platform_etc_dir }}/certs/client-key.pem

    ## SSL Ciphers used by the bridge.
    ##
    ## Value: String
    bridge.emqx.ciphers = ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384

    ## Ciphers for TLS PSK.
    ## Note that 'listener.ssl.external.ciphers' and 'listener.ssl.external.psk_ciphers' cannot
    ## be configured at the same time.
    ## See 'https://tools.ietf.org/html/rfc4279#section-2'.
    bridge.emqx.psk_ciphers = PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA

    ## Ping interval of a down bridge.
    ##
    ## Value: Duration
    ## Default: 10 seconds
    bridge.emqx.keepalive = 60s

    ## TLS versions used by the bridge.
    ##
    ## Value: String
    bridge.emqx.tls_versions = tlsv1.2,tlsv1.1,tlsv1

    ## Subscriptions of the bridge topic.
    ##
    ## Value: String
    bridge.emqx.subscription.1.topic = cmd/topic1

    ## Subscriptions of the bridge qos.
    ##
    ## Value: Number
    bridge.emqx.subscription.1.qos = 1

    ## Subscriptions of the bridge topic.
    ##
    ## Value: String
    bridge.emqx.subscription.2.topic = cmd/topic2

    ## Subscriptions of the bridge qos.
    ##
    ## Value: Number
    bridge.emqx.subscription.2.qos = 1

    ## Start type of the bridge.
    ##
    ## Value: enum
    ## manual
    ## auto
    bridge.emqx.start_type = manual

    ## Bridge reconnect time.
    ##
    ## Value: Duration
    ## Default: 30 seconds
    bridge.emqx.reconnect_interval = 30s

    ## Retry interval for bridge QoS1 message delivering.
    ##
    ## Value: Duration
    bridge.emqx.retry_interval = 20s

    ## Inflight size.
    ##
    ## Value: Integer
    bridge.emqx.max_inflight_batches = 32

    ## Max number of messages to collect in a batch for
    ## each send call towards emqx_bridge_connect
    ##
    ## Value: Integer
    ## default: 32
    bridge.emqx.queue.batch_count_limit = 32

    ## Max number of bytes to collect in a batch for each
    ## send call towards emqx_bridge_connect
    ##
    ## Value: Bytesize
    ## default: 1000M
    bridge.emqx.queue.batch_bytes_limit = 1000MB

    ## Base directory for replayq to store messages on disk
    ## If this config entry is missing or set to undefined,
    ## replayq works in a mem-only manner.
    ##
    ## Value: String
    bridge.emqx.queue.replayq_dir = {{ platform_data_dir }}/emqx_aws_bridge/

    ## Replayq segment size
    ##
    ## Value: Bytesize
    bridge.emqx.queue.replayq_seg_bytes = 10MB

MQTT 桥接相比 RPC 桥接要更灵活，以上配置很多都是 MQTT 连接所需要用到字段，除此之
外，与 RPC 桥接只能将本地消息转发到远程不同，MQTT 桥接不仅可以将远程的消息同步到
本地主题上，还可以将断开桥接时从将要转发的消息缓存到本地上去，当连接恢复时再把消
息发布到远程节点上去。与缓存消息有关的配置项都是以 bridge.$(Bridgename).queue 开
头。而与同步远程节点主题有关的配置项则都以 bridge.$(Bridgename).subscription 开
头。

除了配置文件的方式，还可以通过 CLI 的方式去操作 bridge.

.. code-block:: bash

    $ cd emqx1/ && ./bin/emqx_ctl bridges
    bridges list                                    # List bridges
    bridges start <Name>                            # Start a bridge
    bridges stop <Name>                             # Stop a bridge
    bridges forwards <Name>                         # Show a bridge forward topic
    bridges add-forward <Name> <Topic>              # Add bridge forward topic
    bridges del-forward <Name> <Topic>              # Delete bridge forward topic
    bridges subscriptions <Name>                    # Show a bridge subscriptions topic
    bridges add-subscription <Name> <Topic> <Qos>   # Add bridge subscriptions topic

    $ ./bin/emqx_ctl bridges list
    name: emqx     status: Stopped

    $ ./bin/emqx_ctl bridges start emqx
    Start bridge successfully.

    $ ./bin/emqx_ctl bridges stop emqx
    Stop bridge successfully.

    $ ./bin/emqx_ctl bridges forwards emqx
    topic:   topic1/#
    topic:   topic2/#
    
    $ ./bin/emqx_ctl bridges add-forwards emqx topic3/#
    Add-forward topic successfully.

    $ ./bin/emqx_ctl bridges del-forwards emqx topic3/#
    Del-forward topic successfully.

    $ ./bin/emqx_ctl bridges subscriptions emqx
    topic: cmd/topic1, qos: 1
    topic: cmd/topic2, qos: 1

    $ ./_rel/emqx/bin/emqx_ctl bridges add-subscription aws cmd/topic3 1
    Add-subscription topic successfully.

    $ ./_rel/emqx/bin/emqx_ctl bridges del-subscription aws cmd/topic3
    Del-subscription topic successfully.

.. _bridge_mosquitto:



--------------
mosquitto 桥接
--------------

mosquitto 可以普通 MQTT 连接方式，桥接到 emqx 消息服务器::

                 -------------             -----------------
    Sensor ----> | mosquitto | --Bridge--> |               |
                 -------------             |      EMQ X    |
                 -------------             |    Cluster    |
    Sensor ----> | mosquitto | --Bridge--> |               |
                 -------------             -----------------

mosquitto.conf
--------------

本机 2883 端口启动 emqx 消息服务器，1883 端口启动 mosquitto 并创建桥接。

mosquitto.conf 配置::

    connection emqx
    address 127.0.0.1:2883
    topic sensor/# out 2

    # Set the version of the MQTT protocol to use with for this bridge. Can be one
    # of mqttv31 or mqttv311. Defaults to mqttv31.
    bridge_protocol_version mqttv311

.. _bridge_rsmb:

---------
rsmb 桥接
---------

本机 2883 端口启动 emqx 消息服务器，1883 端口启动 rsmb 并创建桥接。

broker.cfg 桥接配置::

    connection emqx
    addresses 127.0.0.1:2883
    topic sensor/#
