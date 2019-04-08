
.. _bridge:

=================
节点桥接 (Bridge)
=================

.. _bridge_emqx:

----------------
EMQ X 节点间桥接
----------------

EMQ X 支持 RPC 桥接与 MQTT 桥接两种方式。RPC 桥接只支持消息转发，不支持订阅
远程节点的主题去同步数据，MQTT 桥接同时支持转发和通过订阅数据同步两种方式。

节点间桥接与集群不同，不复制主题树与路由表，只按桥接规则转发 MQTT 消息。

*EMQ X* 消息服务器支持多节点桥接模式互联::

                  ---------                     ---------                     ---------
    Publisher --> | Node1 | --Bridge Forward--> | Node2 | --Bridge Forward--> | Node3 | --> Subscriber
                  ---------                     ---------                     ---------

EMQ X 节点桥接配置
---------------------------

假设在本机创建两个 EMQ 节点，并创建一条桥接转发全部传感器(sensor)主题消息:

+---------+---------------------+-----------+
| 目录    | 节点                | MQTT 端口 |
+---------+---------------------+-----------+
| emqx1   | emqx1@192.168.1.1   | 1883      |
+---------+---------------------+-----------+
| emqx2   | emqx2@192.168.1.2   | 1883      |
+---------+---------------------+-----------+

如果是将匹配 sensor/# 主题的消息从 emqx1 节点转发到 emqx2 节点，只需在 emqx1
节点做出如下配置::

    ## Bridge address: node name for local bridge, host:port for remote.
    ##
    ## Value: String
    ## Example: emqx@127.0.0.1,  127.0.0.1:1883
    bridge.emqx2.address = emqx2@192.168.1.2
    
在上述配置中，emqx2 是 bridge 名字, 不同的 bridge 以 bridge 名字来区分，比如说桥接到
 AWS 上的 bridge 的配置可以写成::

    bridge.aws.address = 192.168.1.2:1883

如果桥接方式是 RPC，`bridge.emqx2.address` 需要配置为 `emqx2@192.168.1.2`。
如果桥接方式是 MQTT，`bridge.emqx2.address` 需要配置为 `192.168.1.2:1883`。

bridge 配置中的 forwards 指定的是主题，转发到本地节点指定 forwards 上的消息都会被转发
到远程节点上，比如下面的配置::

    ## Forward message topics
    ##
    ## Value: String
    ## Example: topic1/#,topic2/#
    bridge.aws.forwards = sensor1/#,sensor2/#
    
本地 emqx 接收到匹配主题 `sersor1/#`，`sensor2/#` 的消息都会转发到远程 aws 上的 `sersor1/#`，
`sensor2/#` 主题上。

bridge 配置中的 mountpoint 用来配置主题前缀，该配置选项是配合 forwards 使用的，mountpoint
的基本配置如下::
  
    ## Mountpoint of the bridge.
    ##
    ## Value: String
    bridge.emqx2.mountpoint = bridge/emqx2/${node}/

如果转发到本地的消息的主题是 `sensor1/hello`, 那么转发到远程的主题将会挂载到
`bridge/emqx2/emqx@192.168.1.1/sensor1/hello` 主题上。

emqx 的 bridge 有消息缓存机制，当 bridge 断开（如网络连接不稳定的情况）时可以将
forwards 主题的消息缓存到本地的磁盘队列上，当桥接恢复时，再把消息重新转发到
远程的 aws 或 emqx 节点上。关于缓存队列的配置如下::

    ## Max number of messages to collect in a batch for
    ## each send call towards emqx_bridge_connect
    ##
    ## Value: Integer
    ## default: 32
    bridge.aws.queue.batch_count_limit = 32
    
    ## Max number of bytes to collect in a batch for each
    ## send call towards emqx_bridge_connect
    ##
    ## Value: Bytesize
    ## default: 1000M
    bridge.aws.queue.batch_bytes_limit = 1000MB
    
    ## Base directory for replayq to store messages on disk
    ## If this config entry is missing or set to undefined,
    ## replayq works in a mem-only manner.
    ##
    ## Value: String
    bridge.aws.queue.replayq_dir = data/emqx_aws_bridge/
    
    ## Replayq segment size
    ##
    ## Value: Bytesize
    bridge.aws.queue.replayq_seg_bytes = 10MB

`bridge.aws.queue.batch_count_limit` 和 `bridge.aws.queue.batch_bytes_limit` 都
是负责 bridge 内部队列消息的批量发送的配置选项，这里用户可以不必关心这两个参数，
通常情况下，使用默认参数配置就能满足需求。

`bridge.aws.queue.replayq_dir` 是用来指定 bridge 存储队列的路径的配置参数。

`bridge.aws.queue.replayq_seg_bytes` 是用来指定缓存在磁盘上的消息队列的最大单个文
件的大小，如果消息队列大小超出指定值的话，会创建新的文件来存储消息队列。

EMQ X 节点 MQTT 桥接配置
-----------------------

前面提到过 emqx bridge 的桥接 有两种：RPC 和 MQTT。相比 MQTT 桥接，RPC 桥接的局限性要更多
，比如：emqx 的 RPC 桥接只能将本地的消息转发到远程桥接节点上，无法将远程桥接节点的消息同步
到本地节点上；而且 RPC 桥接只能将两个 emqx 桥接在一起，无法桥接 emqx 到其他的 mqtt broker
上，emqx 3.0 正式引入了 mqtt bridge，使 emqx 具备了桥接任意 mqtt broker 的能力，同时由于
 mqtt 协议本身的特性，mqtt bridge 还可以订阅远程 mqtt broker 的主题，将远程 mqtt broker 的
消息同步到本地。

其原理是在 emqx broker 中开启一个 emqx_client 的 erlang 进程，去连接远程的 mqtt broker，因
此在 mqtt bridge 的配置中，需要去填一些 mqtt 连接所必须用到的字段::

    ## Bridge address: node name for local bridge, host:port for remote.
    ##
    ## Value: String
    ## Example: emqx@127.0.0.1,  127.0.0.1:1883
    bridge.emqx.address = 192.168.1.2:1883

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
    bridge.emqx.client_id = bridge_emq

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

emqx bridge 的启动方式有两种，一种是自动启动 bridge，一种是手动启动 brid    

如果要创建多个 bridge，只要复制默认的 bridge 配置再拷贝到现有 bridge 配置中，
修改 bridge 名字，再在原有配置基础上做一些修改就可以了。

在配置完成后，可以通过 CLI 的方式去操作 bridge。如果用户在配置文件中指定 bridge
的启动方式是 `manual` ::
  
    ## Start type of the bridge.
    ##
    ## Value: enum
    ## manual
    ## auto
    bridge.emqx.start_type = auto

需要手动 `emqx_ctl bridges start emqx` 来启动桥接。如果 `start_type` 是 auto，
不需要 CLI 就可以自动启动桥接。

下面是桥接的基本 CLI 命令:

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

列出 bridge

.. code-block:: bash

    $ ./bin/emqx_ctl bridges list
    name: emqx     status: Stopped

启动指定 bridge

.. code-block:: bash

    $ ./bin/emqx_ctl bridges start emqx
    Start bridge successfully.

停止指定 bridge

.. code-block:: bash

    $ ./bin/emqx_ctl bridges stop emqx
    Stop bridge successfully.

列出指定 bridge 的转发主题

.. code-block:: bash

    $ ./bin/emqx_ctl bridges forwards emqx
    topic:   topic1/#
    topic:   topic2/#

给指定 bridge 添加转发主题

.. code-block:: bash

    $ ./bin/emqx_ctl bridges add-forwards emqx topic3/#
    Add-forward topic successfully.

给指定 bridge 删除转发主题

.. code-block:: bash

    $ ./bin/emqx_ctl bridges del-forwards emqx topic3/#
    Del-forward topic successfully.

列出指定 bridge 的订阅

.. code-block:: bash

    $ ./bin/emqx_ctl bridges subscriptions emqx
    topic: cmd/topic1, qos: 1
    topic: cmd/topic2, qos: 1

给指定 bridge 添加订阅主题

.. code-block:: bash

    $ ./_rel/emqx/bin/emqx_ctl bridges add-subscription emqx cmd/topic3 1
    Add-subscription topic successfully.

给指定 bridge 删除订阅主题

.. code-block:: bash

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
