
===================
高级特性 (Advanced)
===================

*EMQ* 2.0版本新增了本地订阅与共享订阅功能。


-----------------------------
本地订阅(Local Subscription)
-----------------------------

本地订阅(Local Subscription)不只在本节点创建本地路由表，不会在集群节点间广播全局路由。

.. code-block:: shell

    mosquitto_sub -t '$local/topic'

-----------------------------
共享订阅(Shared Subscription)
-----------------------------

.. code-block:: shell

    mosquitto_sub -t '$queue/topic'
    mosquitto_sub -t '$share/group/topic'

