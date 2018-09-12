
.. _bridge:

=================
节点桥接 (Bridge)
=================

.. _bridge_emqx:

----------------
EMQ X 节点间桥接
----------------

*EMQ X* 消息服务器支持多节点桥接模式互联::

                  ---------                     ---------                     ---------
    Publisher --> | Node1 | --Bridge Forward--> | Node2 | --Bridge Forward--> | Node3 | --> Subscriber
                  ---------                     ---------                     ---------

节点间桥接与集群不同，不复制主题树与路由表，只按桥接规则转发 MQTT 消息。

EMQ X 节点桥接配置
-------------------

假设在本机创建两个 EMQ 节点，并创建一条桥接转发全部传感器(sensor)主题消息:

+---------+---------------------+-----------+
| 目录    | 节点                | MQTT 端口 |
+---------+---------------------+-----------+
| emqx1   | emqx1@127.0.0.1     | 1883      |
+---------+---------------------+-----------+
| emqx2   | emqx2@127.0.0.1     | 2883      |
+---------+---------------------+-----------+

启动 emqx1, emqx2 节点:

.. code-block:: bash

    cd emqx1/ && ./bin/emqx start
    cd emqx2/ && ./bin/emqx start

emqx1 节点上创建到 emqx2 桥接:

.. code-block:: bash

    $ ./bin/emqx_ctl bridges start emqx2@127.0.0.1 sensor/#

    bridge is started.

    $ ./bin/emqx_ctl bridges list

    bridge: emqx1@127.0.0.1--sensor/#-->emqx2@127.0.0.1

测试 emqx1--sensor/#-->emqx2 的桥接:

.. code-block:: bash

    #emqx2节点上

    mosquitto_sub -t sensor/# -p 2883 -d

    #emqx1节点上

    mosquitto_pub -t sensor/1/temperature -m "37.5" -d

删除桥接:

.. code-block:: bash

    ./bin/emqx_ctl bridges stop emqx2@127.0.0.1 sensor/#

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
