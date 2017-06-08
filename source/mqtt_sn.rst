
.. _mqtt_sn:

===============
MQTT-SN协议介绍
===============

MQTT-SN协议是MQTT的直系亲属，它使用UDP进行通信，标准的端口是1884。MQTT-SN的主要目的是为了适应受限的设备和网络，比如一些传感器，只有很小的内存和CPU，TCP对于这些设备来说非常奢侈。还有一些网络，比如ZIGBEE，报文的长度在300字节以下，无法承载太大的数据包。所以MQTT-SN的数据包更小巧。

MQTT-SN的官方标准下载地址是http://mqtt.org/new/wp-content/uploads/2009/06/MQTT-SN_spec_v1.2.pdf

===============
MQTT-SN和MQTT的区别
===============

MQTT-SN的信令和MQTT大部分都相同，比如都有Will, 都有Connect/Subscribe/Publish命令.

MQTT-SN最大的不同是，topic使用TopicId来代替，而TopicId是一个16比特的数字。每一个数字对应一个topic, 设备和云端需要使用REGISTER命令映射TopicId和topic的对应关系。

MQTT-SN可以随时更改Will的内容, 甚至可以取消. 而MQTT只允许在CONNECT时设定Will的内容, 而且不允许更改.

MQTT-SN的网络中有网关这种设备，它负责把MQTT-SN转换成MQTT，和云端的MQTT Broker通信. MQTT-SN的协议支持自动发现网关的功能。

MQTT-SN还支持设备的睡眠功能，如果设备进入睡眠状态，无法接收UDP数据，网关将把下行的PUBLISH消息缓存起来，直到设备苏醒后再传送。


===============
EMQ-SN的介绍
===============

EMQ-SN是EMQ的一个插件，实现了MQTT-SN的大部分功能，它相当于一个在云端的MQTT-SN网关，直接和EMQ Broker相连。



配置参数
----------------

File: etc/emq_sn.conf


    mqtt.sn.port = 1884
    mqtt.sn.advertise_duration = 900
    mqtt.sn.gateway_id = 1
    mqtt.sn.username = mqtt_sn_user
    mqtt.sn.password = abc

+-----------------------------+-------------------------------------------------------------------------+
| mqtt.sn.port                | 指定mqtt-sn监听的端口号                                                 |
+-----------------------------+-------------------------------------------------------------------------+
| mqtt.sn.advertise_duration  | ADVERTISE消息的发送间隔(秒)                                             |
+-----------------------------+-------------------------------------------------------------------------+
| mqtt.sn.gateway_id          | 网关ID                                                                  |
+-----------------------------+-------------------------------------------------------------------------+
| mqtt.sn.username            | 这是可选的参数，指定所有mqtt-sn连接的用户名, 用于鉴权模块               |
+-----------------------------+-------------------------------------------------------------------------+
| mqtt.sn.password            | 这也是可选的参数, 和username一起使用                                    |
+-----------------------------+-------------------------------------------------------------------------+


启动emq-sn
----------------

    ./bin/emqx_ctl plugins load emqx_sn
    

===============
MQTT-SN的客户端
===============

1. https://github.com/eclipse/paho.mqtt-sn.embedded-c/
2. https://github.com/ty4tw/MQTT-SN
3. https://github.com/njh/mqtt-sn-tools
4. https://github.com/arobenko/mqtt-sn


