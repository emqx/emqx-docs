
==================
Erlang/OTP语言平台
==================

eMQTT消息服务器完全基于Erlang/OTP语言平台开发，Erlang/OTP是非常出色的软实时(Soft-Realtime)低延时(Low-Latency)的语言平台，非常适合消息服务器的开发:

1. 细粒度垃圾回收(Garbage Collection)

2. 基于Reduction计数的抢占式公平进程调度

3. 容错恢复与分布支持


--------------
Erlang/OTP历史
--------------

爱立信、Joe Armstrong，1988年起近30年历史

Erlang语言最初设计目的是开发电信设备与系统

1998年开源，2006多核CPU支持，互联网、即时消息、云计算应用

C++、Java、C#面向对象系列截然不同的设计思路

以消息为主的移动互联网、物联网最佳服务端平台


-------------------
Erlang/OTP平台特点
-------------------

高并发(Concurrency, Muti Core, Threads, Massive Processes)

低延时(Low-Latency)

软实时(Soft real-time)

容错(Fault-tolerant, monitor, link, supervisor tree)

分布(Distributed nodes, mnesia)

水平伸缩(Scalable)

热升级(Hot Upgrade)

------------
Erlang虚拟机
------------

类似操作系统: CPU Cores, Schedulers, Threads, Massive Processes

跨平台(Linux, FreeBSD, AIX, HP-UX, 树莓派…Windows)

出色的内存管理：process heap, binary, ets…

细粒度垃圾回收(Fine-grained Garbage Collected)

轻量进程、公平调度


------------------
Erlang编程语言(1)
-------------------

函数编程(Functional Programming)

模式匹配(Pattern Match)

轻量进程(Lightweight Processes)

消息传递(Message Passing)

递归(Recursion)、尾递归(Tail Recursion)

Actor-Oriented, Object-Oriented?


Atom, Pid, Tuple, List, Binary, Port…

List, Binary Comprehension

    List, Binary Comprehension
    << <<(serialise_utf(Topic))/binary, ?RESERVED:6, Qos:2>> || {Topic, Qos} <- Topics >>;

    routes(Topics, Pid) ->
        lists:unzip([{{Topic, Pid}, {Pid, Topic}} || Topic <- Topics]).

Binary Match解析网络协议

闭包(Closure)与高阶函数(HigherOrder Functions)

参数化模块(Parameterized Module)

    new(Sock, SockFun, Opts) ->
       {?MODULE, [Sock, SockFun, parse_opt(Opts)]}.

----------------
Erlang/OTP平台
----------------

OTP(Open Telecom Platform)
行为(Behaviours)

    gen_server(客户端服务器)
    gen_fsm(有限状态自动机)
    gen_event(事件通知)

监控(Supervisor)
     Supervisor Restart Strategies
     one_for_all
     one_for_one
     rest_for_one
     simple_one_for_one

应用(Applications)

    TREE IMAGE

发布(Releases)

    ERTS + Boot脚本 + Applications  => Binary Package


