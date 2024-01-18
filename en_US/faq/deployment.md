---
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

# Installation and deployment
## What's EMQX suggested OS?


EMQX supports deployment on at Linux, Windows, MacOS, ARM system, and suggest to deploy product environment at issued Linux version, such as CentOS, Ubuntu and Debian.




## How to estimate resource usage of EMQX?

**Tags:** [*Resource estimation*](tags.md#resource-estimation)


Following factors will have an impact on EMQX resource consumption, mainly on CPU and memory usage.

- Connection number: EMQX creates 2 Erlang process for every MQTT connection, and every Erlang process consumes some resource. The more connections, the more resource is required.

- Average throughput: Throughput means (pub message number + sub message number) processed by EMQX per second. With higher throughput value, more resource will be used for handling route and message delivery in EMQX.

- Payload size: With bigger size of payload, more memory and CPU are required for message cache and processing.

- Topic number: With more topics, the route table in EMQX will increase, and more resource is required.

- QoS：With higher message QoS level, more resource will be used for message handling.

If client devices connect to EMQX through TLS, more CPU resource is required for encryption and decryption. Our suggested solution is to add a load balancer before EMQX nodes, the TLS is offloaded at load balance node, connections between load balancer and backend EMQX nodes use plain TCP connections.

You can use our online calculation tool [TODO](https://www.emqx.io) to estimate the resource consumption.




## When I was executing stress test, the connection number and throughput are lower than expected. How can I tune the system to make full use of it?

**Tags:** [*Debug*](tags.md#debug)  [*Performance*](tags.md#performance)


When executing a stress test, besides ensuring the necessary hardware resource, it is also necessary to tune the OS and the Erlang VM to make the maximum use of the resource. The most common tuning is to modify the global limitation of file handles, the user limitation of file handles, the TCP backlog and buffer, the limitation fo process number of Erlang VM and so on. You will also need to tune the client machine to ensure it has the ability and resource to handle all the subs and pubs.

Different use scenario requires diferent tuning。 In the EMQX document there is a chapter about tuning the system for general purpose. [TODO](https://www.emqx.io)




## My connections number is small, do I still need to deploy multiple nodes in production?

**Tags:** [*Performance*](tags.md#performance)


Even when the connction number is small, or message rate is low, it is still very meaningfulto deploy a cluster with multiple nodes in production. A cluster improves the availability of system, when a single node is down, the rest nodes in cluster ensures the service is not interrupted.