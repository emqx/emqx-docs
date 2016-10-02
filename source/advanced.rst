
===================
高级特性 (Advanced)
===================

-----------------------------
共享订阅(Shared Subscription)
-----------------------------

.. code-block:: shell

    mosquitto_sub -t '$queue/topic'
    mosquitto_sub -t '$share/group/topic'

-----------------------------
本地订阅(Shared Subscription)
-----------------------------

.. code-block:: shell

    mosquitto_sub -t '$local/topic'

