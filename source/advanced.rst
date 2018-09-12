
.. _advanced:

============================
高级特性 (Advanced Features)
============================

------------------------------
共享订阅 (Shared Subscription)
------------------------------

*EMQ X* R3.0 版本支持集群级别的共享订阅功能。 共享订阅(Shared Subscription)支持在多订阅者间采用分组负载平衡方式派发消息::

                                ---------
                                |       | --Msg1--> Subscriber1
    Publisher--Msg1,Msg2,Msg3-->| EMQ X | --Msg2--> Subscriber2
                                |       | --Msg3--> Subscriber3
                                ---------

共享订阅支持两种使用方式:

+-----------------+-------------------------------------------+
|  订阅前缀       | 使用示例                                  |
+-----------------+-------------------------------------------+
| $queue/         | mosquitto_sub -t '$queue/topic'           |
+-----------------+-------------------------------------------+
| $share/<group>/ | mosquitto_sub -t '$share/group/topic'     |
+-----------------+-------------------------------------------+

示例::

    mosquitto_sub -t '$share/group/topic'

    mosquitto_pub -t 'topic' -m msg -q 2

