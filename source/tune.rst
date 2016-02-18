
====================
测试调优(Tune Guide)
====================

-----------------
eMQTT-Benchmark
-----------------

3G内存, 50%CPU/核心 (8核, 32G内存CentOS节点)
250+K Connections,
50K Topics,
250K Subscribers,
4K Qos1 Messages/Sec In,
20K Qos1 Messages/Sec Out,
12M+(bps) In, 56M+(bps) Out Traffic
产品环境：500K+手机连接
压力测试：900K+测试连接

原则:


Tune Guide
----------

OS操作系统
----------

TCP内核参数
----------

Erlang虚拟机
-------------

eMQTT参数
----------

Benchmark
----------

emqtt_benchmark
--------------

tsung测试工具
--------------


mqtt-bench
-----------

---
