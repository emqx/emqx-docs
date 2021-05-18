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
## What's EMQ X suggested OS?


EMQ X supports deployment on Linux, Windows, MacOS, ARM system, however it is recommended to deploy the product on one of the supported Linux distributions, such as CentOS, Ubuntu and Debian.




## How to estimate resource usage of EMQ X?

**Tags:** [*Resource estimation*](tags.md#resource-estimation)


The following factors will have an impact on EMQ X resource consumption, mainly on CPU and memory usage.

- Number of connections: EMQ X creates 2 Erlang process for each MQTT connection, and every Erlang process consumes some resource. The more connections, the more resources are required.

- Average throughput: Throughput means (pub message number + sub message number) processed by EMQ X per second. With higher throughput value, more resource will be used for handling route and message delivery in EMQ X.

- Payload size: With bigger size of payload, more memory and CPU are required for message cache and processing.

- Number of topics: With more topics, the route table in EMQ X will increase, and more resource is required.

- QoS: With higher message QoS level, more resource will be used for message handling.

If client devices connect to EMQ X through TLS, more CPU resource is required for encryption and decryption. Our suggested solution is to add a load balancer in front of EMQ X nodes, the TLS is offloaded at load balance node, connections between load balancer and backend EMQ X nodes use plain TCP connections.

You can use our online calculation tool [TODO](https://www.emqx.io) to estimate the resource consumption.




## When I was executing stress test, the connection number and throughput are lower than expected. How can I tune the system to make full use of it?

**Tags:** [*Debug*](tags.md#debug)  [*Performance*](tags.md#performance)


When executing a stress test, besides ensuring the necessary hardware resource, it is also necessary to tune the OS and the Erlang VM to make the maximum use of the resource. The most common tuning is to modify the global limitation of file handles, the user limitation of file handles, the TCP backlog and buffer, the limitation of process number of Erlang VM and so on. You will also need to tune the client machine to ensure it has the ability and resource to handle all the subs and pubs.

Different use cases require different tuning. In the EMQ X document there is a chapter about tuning the system for general purpose. [TODO](https://www.emqx.io)




## My connections number is small, do I still need to deploy multiple nodes in production?

**Tags:** [*Performance*](tags.md#performance)


Even when the connection number is low, or message rate is low, it still makes sense to deploy a cluster with multiple nodes in production. Clustering improves the availability of system: when a single node goes down, the rest of the nodes in the cluster ensure that the service is not interrupted.
