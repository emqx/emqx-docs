---
# 标题
title: 安装部署
# 编写日期
date: 2020-02-20 12:44:32
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref:
---

# 安装部署
### EMQX 推荐部署的操作系统是什么？


EMQX 支持跨平台部署在 Linux、Windows、MacOS、ARM 嵌入系统，生产系推荐在 CentOS、Ubuntu、Debian 等 Linux 发行版上部署。




### EMQX 支持 Windows 操作系统吗？


支持。部署参考[文章](https://www.jianshu.com/p/e5cf0c1fd55c).




### EMQX 如何预估资源的使用？

**标签:** [*资源估算*](tags.md#资源估算)


EMQX 对资源的使用主要有以下的影响因素，每个因素都会对计算和存储资源的使用产生影响：

- 连接数：对于每一个 MQTT 长连接，EMQX 会创建两个 Erlang 进程，每个进程都会耗费一定的资源。连接数越高，所需的资源越多；

- 平均吞吐量：指的是每秒 Pub 和 Sub 的消息数量。吞吐量越高，EMQX 的路由处理和消息转发处理就需要更多的资源；

- 消息体大小：消息体越大，在 EMQX 中处理消息转发的时候在内存中进行数据存储和处理，所需的资源就越多；

- 主题数目：如果主题数越多，在 EMQX 中的路由表会相应增长，因此所需的资源就越多；

- QoS：消息的 QoS 越高，EMQX 服务器端所处理的逻辑会更多，因此会耗费更多的资源；

另外，如果设备通过 TLS（加密的连接）连接 EMQX，EMQX 会需要额外的资源（主要是 CPU 资源）。推荐方案是在 EMQX 前面部署负载均衡，由负载均衡节点卸载 TLS，实现职责分离。

可参考 [TODO](https://www.emqx.io) 来预估计算资源的使用；公有云快速部署 EMQX 实例，请参考[TODO](https://www.emqx.io)。




### EMQX 的百万连接压力测试的场景是什么？

**标签:** [*性能测试*](tags.md#性能测试)


在EMQ 2.0版本发布的时候，由第三方软件测试工具服务提供商 [XMeter](https://www.xmeter.net) 执行了一次百万级别连接的性能测试。测试基于开源社区中最流行的性能测试工具 [Apache JMeter](https://jmeter.apache.org/)，以及开源[性能测试插件](https://github.com/emqx/mqtt-jmeter)。该性能测试场景为测试客户端到服务器端的MQTT协议连接，该测试场景下除了发送MQTT协议的控制包和PING包（每5分钟发送一次）外，不发送用户数据，每秒新增连接数为1000，共计运行30分钟。

在该测试中，还执行了一些别的性能测试，主要为在为10万MQTT背景连接的情况下，执行了不同条件下的消息发送和接收的场景。具体请参见[性能测试报告](https://media.readthedocs.org/pdf/emq-xmeter-benchmark-cn/latest/emq-xmeter-benchmark-cn.pdf).




### 我的连接数目并不大，EMQX 生产环境部署需要多节点吗？

**标签:** [*集群*](tags.md#集群)


即使在连接数量，消息率不高的情况下（服务器低负载），在生产环境下部署多节点的集群依然是很有意义的。集群能提高系统的可用性，降低单点故障的可能性。当一个节点宕机时，其他在线节点可以保证整个系统的服务不中断。