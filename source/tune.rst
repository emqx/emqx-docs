
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

OS操作系统内核参数
------------------

TCP协议栈参数
-------------

# backlog - socket监听队列长度
net.core.somaxconn = 65536

sysctl -w net.ipv4.tcp_rmem='1024 4096 16384'
sysctl -w net.ipv4.tcp_wmem='1024 4096 16384'
sysctl -w net.core.rmem_max=16384
sysctl -w net.core.wmem_max=16384


Erlang虚拟机
-------------

emqttd/etc/vm.args设置::

    ## max process numbers
    +P 2097152

    ## Sets the maximum number of simultaneously existing ports for this system
    +Q 1048576

    ## Increase number of concurrent ports/sockets
    -env ERL_MAX_PORTS 1048576

    -env ERTS_MAX_PORTS 1048576

emqttd消息服务器参数
-------------------

emqttd/etc/emqttd.config设置::

        {mqtt, 1883, [
            %% Size of acceptor pool
            {acceptors, 64},

            %% Maximum number of concurrent clients
            {max_clients, 1000000},

            %% Socket Access Control
            {access, [{allow, all}]},

            %% Connection Options
            {connopts, [
                %% Rate Limit. Format is 'burst, rate', Unit is KB/Sec
                %% {rate_limit, "100,10"} %% 100K burst, 10K rate
            ]},
            ...




Benchmark
----------

emqtt_benchmark
--------------

tsung测试工具
--------------


mqtt-bench
-----------

---
