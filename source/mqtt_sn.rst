
.. _mqtt_sn:

============
MQTT-SN 协议
============

MQTT-SN 协议是 MQTT 的直系亲属，它使用 UDP 进行通信，标准的端口是1884。MQTT-SN 的主要目的是为了适应受限的设备和网络，比如一些传感器，只有很小的内存和 CPU，TCP 对于这些设备来说非常奢侈。还有一些网络，比如 ZIGBEE，报文的长度在300字节以下，无法承载太大的数据包。所以 MQTT-SN 的数据包更小巧。

MQTT-SN 的官方标准下载地址: http://mqtt.org/new/wp-content/uploads/2009/06/MQTT-SN_spec_v1.2.pdf

-----------------------
MQTT-SN 和 MQTT 的区别
-----------------------

MQTT-SN 的信令和 MQTT 大部分都相同，比如都有 Will, 都有 Connect/Subscribe/Publish 命令.

MQTT-SN 最大的不同是，Topic 使用 TopicId 来代替，而 TopicId 是一个16比特的数字。每一个数字对应一个 Topic, 设备和云端需要使用 REGISTER 命令映射 TopicId 和 Topic 的对应关系。

MQTT-SN 可以随时更改 Will 的内容, 甚至可以取消. 而 MQTT 只允许在 CONNECT 时设定 Will 的内容, 而且不允许更改.

MQTT-SN 的网络中有网关这种设备，它负责把 MQTT-SN 转换成 MQTT，和云端的 MQTT Broker 通信. MQTT-SN 的协议支持自动发现网关的功能。

MQTT-SN 还支持设备的睡眠功能，如果设备进入睡眠状态，无法接收 UDP 数据，网关将把下行的 PUBLISH 消息缓存起来，直到设备苏醒后再传送。

---------------
EMQX-SN 网关插件
---------------

EMQX-SN 是 EMQ X 的一个网关插件，实现了 MQTT-SN 的大部分功能，它相当于一个在云端的 MQTT-SN 网关，直接和 EMQ X Broker 相连。

配置参数
--------

File: etc/plugins/emqx_sn.conf::

    mqtt.sn.port = 1884
    
    mqtt.sn.advertise_duration = 900
    
    mqtt.sn.gateway_id = 1
    
    mqtt.sn.username = mqtt_sn_user
    
    mqtt.sn.password = abc

+-----------------------------+-------------------------------------------------------------------------+
| mqtt.sn.port                | 指定 MQTT-SN 监听的端口号                                                 |
+-----------------------------+-------------------------------------------------------------------------+
| mqtt.sn.advertise_duration  | ADVERTISE 消息的发送间隔(秒)                                             |
+-----------------------------+-------------------------------------------------------------------------+
| mqtt.sn.gateway_id          | 网关 ID                                                                  |
+-----------------------------+-------------------------------------------------------------------------+
| mqtt.sn.username            | 这是可选的参数，指定所有 MQTT-SN 连接的用户名, 用于鉴权模块               |
+-----------------------------+-------------------------------------------------------------------------+
| mqtt.sn.password            | 这也是可选的参数, 和 username 一起使用                                    |
+-----------------------------+-------------------------------------------------------------------------+

启动emqx-sn
----------

.. code-block::

    ./bin/emqx_ctl plugins load emqx_sn

-----------------
MQTT-SN 客户端库
-----------------

1. https://github.com/eclipse/paho.mqtt-sn.embedded-c/
2. https://github.com/ty4tw/MQTT-SN
3. https://github.com/njh/mqtt-sn-tools
4. https://github.com/arobenko/mqtt-sn

