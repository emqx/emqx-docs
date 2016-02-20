
.. _bridge:

==================
分布式桥接(Bridge)
==================

----------------
emqttd节点间桥接
----------------

emqttd消息服务器支持多节点桥接互联模式::

                  ---------                     ---------                     ---------
    Publisher --> | node1 | --Bridge Forward--> | node2 | --Bridge Forward--> | node3 | --> Subscriber
                  ---------                     ---------                     ---------

节点间桥接与集群不同，不复制主题树与路由表，只按桥接规则转发MQTT消息。

emqttd节点桥接配置
------------------

假设在本机创建两个emqttd节点，并创建一条桥接转发全部传感器(sensor)主题消息:

+---------+---------------------+----------+
| 目录    | 节点                | MQTT端口 |
+---------+---------------------+----------+
| emqttd1 | emqttd1@127.0.0.1   | 1883     |
+---------+---------------------+----------+
| emqttd2 | emqttd2@127.0.0.1   | 2883     |
+---------+---------------------+----------+

启动emqttd1, emqttd2节点::

    cd emqttd1/ && ./bin/emqttd start
    cd emqttd2/ && ./bin/emqttd start

emqttd1节点上创建到emqttd2桥接::

    $ ./bin/emqttd_ctl bridges start emqttd2@127.0.0.1 sensor/#

    bridge is started.
    
    $ ./bin/emqttd_ctl bridges list

    bridge: emqttd1@127.0.0.1--sensor/#-->emqttd2@127.0.0.1

测试emqttd1--sensor/#-->emqttd2的桥接::

    #emqttd2节点上

    mosquitto_sub -t sensor/# -p 2883 -d

    #emqttd1节点上

    mosquitto_pub -t sensor/1/temperature -m "37.5" -d 

删除桥接::

    ./bin/emqttd_ctl bridges stop emqttd2@127.0.0.1 sensor/#


-------------
mosquitto桥接
-------------




-------------
rsmb桥接
-------------





