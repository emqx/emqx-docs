---
# 编写日期
date: 2020-02-07 17:15:26
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

# 入门概念

## EMQ X 是什么？


EMQ X 是开源百万级分布式 MQTT 消息服务器（MQTT Messaging Broker），用于支持各种接入标准 MQTT 协议的设备，实现从设备端到服务器端的消息传递，以及从服务器端到设备端的设备控制消息转发。从而实现物联网设备的数据采集，和对设备的操作和控制。


## 为什么选择EMQ X？


EMQ X 与别的 MQTT 服务器相比，有以下的优点：

- 经过100+版本的迭代，EMQ X 目前为开源社区中最流行的 MQTT 消息中间件，在各种客户严格的生产环境上经受了严苛的考验；
- EMQ X 支持丰富的物联网协议，包括 MQTT、MQTT-SN、CoAP、 LwM2M、LoRaWAN 和 WebSocket 等；
- 优化的架构设计，支持超大规模的设备连接。企业版单机能支持百万的 MQTT 连接；集群能支持千万级别的 MQTT 连接；
- 易于安装和使用；
- 灵活的扩展性，支持企业的一些定制场景；
- 中国本地的技术支持服务，通过微信、QQ等线上渠道快速响应客户需求；


## EMQ X 的主题数量有限制吗？

主题使用没有数量限制，主题数量增长对性能影响不大，可以放心使用。


## EMQ X 开源版怎么存储数据？

开源版不支持数据存储功能，可以使用企业版，或者使用外部程序订阅主题/Webhook 的方式获取数据，然后写入到数据库。


## EMQ X 与物联网平台的关系是什么？


典型的物联网平台包括设备硬件、数据采集、数据存储、分析、Web / 移动应用等。EMQ X 位于数据采集这一层，分别与硬件和数据存储、分析进行交互，是物联网平台的核心：前端的硬件通过 MQTT 协议与位于数据采集层的 EMQ X 交互，通过 EMQ X 将数据采集后，通过 EMQ X 提供的数据接口，将数据保存到后台的持久化平台中（各种关系型数据库和 NOSQL 数据库），或者流式数据处理框架等，上层应用通过这些数据分析后得到的结果呈现给最终用户。




## EMQ X 有哪些产品？

**标签:** [*企业版*](tags.md#企业版)


EMQ X 公司主要提供[三个产品](https://www.emqx.io/products)，主要体现在支持的连接数量、产品功能和商业服务等方面的区别：

- EMQ X Broker：EMQ X 开源版，提供 MQTT 协议、CoAP 和 LwM2M 等常见物联网协议的支持；支持 10 万级的并发连接；

- EMQ X Enterprise：EMQ X 企业版，在开源版基础上，增加了数据持久化 Redis、MySQL、MongoDB 或 PostgreSQL，数据桥接转发 Kafka，LoRaWAN 支持，监控管理，Kubernates 部署等方面的支持；支持百万级并发连接；

- EMQ X Platform：EMQ X 平台版，在企业版基础上，支持千万级的连接和跨数据中心的解决方案，提供物联网平台全生命周期中需要的各种服务（咨询、培训、架构设计、定制开发、平台建设、功能测试与运维服务）。



## EMQ X 与 NB-IoT、LoRAWAN 的关系是什么？

**标签:** [*NB-IoT*](tags.md#nb-iot)  [*LoRAWAN*](tags.md#lorawan)  [*多协议*](tags.md#多协议)


EMQ X 是一个开源的 MQTT 消息服务器，并且 MQTT 是一个 TCP 协议栈上位于应用层的协议；而 NB-IoT 和 LoRAWAN 在 TCP 协议层处于物理层，负责物理信号的传输。因此两者在 TCP 协议栈的不同层次上，实现不同的功能。




## MQTT 协议与 HTTP 协议相比，有何优点和弱点?

**标签:** [*多协议*](tags.md#多协议)


HTTP 协议是一个无状态的协议，每个 HTTP 请求为 TCP 短连接，每次请求都需要重新创建一个 TCP 连接（可以通过 keep-alive 属性来优化 TCP 连接的使用，多个 HTTP 请求可以共享该 TCP 连接）；而 MQTT 协议为长连接协议，每个客户端都会保持一个长连接。与 HTTP 协议相比优势在于
：

- MQTT 的长连接可以用于实现从设备端到服务器端的消息传送之外，还可以实现从服务器端到设备端的实时控制消息发送，而 HTTP 协议要实现此功能只能通过轮询的方式，效率相对来说比较低；

- MQTT 协议在维护连接的时候会发送心跳包，因此协议以最小代价内置支持设备 “探活” 的功能，而 HTTP 协议要实现此功能的话需要单独发出 HTTP 请求，实现的代价会更高；

- 低带宽、低功耗。MQTT 在传输报文的大小上与 HTTP 相比有巨大的优势，因为 MQTT 协议在连接建立之后，由于避免了建立连接所需要的额外的资源消耗，发送实际数据的时候报文传输所需带宽与 HTTP 相比有很大的优势，参考网上[有人做的测评](https://medium.com/@flespi/http-vs-mqtt-performance-tests-f9adde693b5f )，发送一样大小的数据，MQTT 比 HTTP 少近 50 倍的网络传输数据，而且速度快了将近 20 倍。在网上有人做的[另外一个评测显示](http://stephendnicholas.com/posts/power-profiling-mqtt-vs-https )，接收消息的场景，MQTT 协议的耗电量为 HTTP 协议的百分之一，而发送数据的时候 MQTT 协议的耗电量为 HTTP 协议的十分之一；

- MQTT 提供消息质量控制（QoS），消息质量等级越高，消息交付的质量就越有保障，在物联网的应用场景下，用户可以根据不同的使用场景来设定不同的消息质量等级；




## 什么是认证鉴权？使用场景是什么？

**标签:** [*认证鉴权*](tags.md#认证鉴权)


认证鉴权指的是当一个客户端连接到 MQTT 服务器的时候，通过服务器端的配置来控制客户端连接服务器的权限。EMQ 的认证机制包含了有三种，

- 用户名密码：针对每个 MQTT 客户端的连接，可以在服务器端进行配置，用于设定用户名和密码，只有在用户名和密码匹配的情况下才可以让客户端进行连接

- ClientID：每个 MQTT 客户端在连接到服务器的时候都会有个唯一的 ClientID，可以在服务器中配置可以连接该服务器的 ClientID 列表，这些 ClientID 的列表里的客户端可以连接该服务器

- 匿名：允许匿名访问

通过用户名密码、ClientID 认证的方式除了通过配置文件之外，还可以通过各类数据库和外部应用来配置，比如 MySQL、PostgreSQL、Redis、MongoDB、HTTP 和 LDAP 等。




## 什么是 Hook？使用场景是什么？

**标签:** [*WebHook*](tags.md#webhook)


钩子（hook）指的是由 EMQ X 在连接、对话和消息触发某些事件的时候提供给对外部的接口，主要提供了如下的钩子，EMQ X 提供了将这些 hook 产生的事件持久化至数据库的功能，从而很方便地查询得知客户端的连接、断开等各种信息。

- client.connected：客户端上线
- client.disconnected：客户端连接断开
- client.subscribe：客户端订阅主题
- client.unsubscribe：客户端取消订阅主题
- session.created：会话创建
- session.resumed：会话恢复
- session.subscribed：会话订阅主题后
- session.unsubscribed：会话取消订阅主题后
- session.terminated：会话终止
- message.publish：MQTT 消息发布
- message.delivered：MQTT 消息送达
- message.acked：MQTT 消息回执
- message.dropped：MQTT 消息丢弃




## 什么是 mqueue？如何配置 mqueue？

**标签:** [*消息队列*](tags.md#消息队列)


mqueue 是 EMQ X 在消息发布流程中保存在会话中的一个消息队列，当 MQTT 连接报文中的 clean session 设置为 false 的时候，即使是客户端断开连接的情况下，EMQ X 也会为断连客户端保存一个会话，这个会话会缓存订阅关系，并代替断开连接的客户端去接收订阅主题上的消息，而这些消息会存在 EMQ X 的 mqueue 中，等到客户端重新上线再将消息重新发给客户端。由于 qos0 消息在 MQTT 协议中的优先级比较低，所以 EMQ X 默认不缓存 qos 0 的消息，mqueue 在 EMQ X 中是可以配置的，通过配置 `zone.$name.mqueue_store_qos0 = true` 可以将 qos0 消息也存在 mqueue 中，mqueue 的大小也是有限制的，通过配置项 `zone.external.max_mqueue_len` ，可以确定每个会话缓存的消息数量。注意，这些消息是存储在内存中的，所以尽量不要将 mqueue 长度限制修改为 0（设置为 0 代表 mqueue 长度没有限制），否则在实际的业务场景中，有内存耗光的风险。




## 什么是 WebSocket？什么情况下需要通过 WebSocket 去连接 EMQ X 服务器？

**标签:** [*WebSocket*](tags.md#websocket)  [*多协议*](tags.md#多协议)


WebSocket 是一种在基于 HTTP 协议上支持全双工通讯的协议，通过该协议，用户可以实现浏览器和服务器之间的双向通信，比如可以通过服务器往浏览器端推送消息。EMQ X 提供了 WebSocket 连接支持，用户可以在浏览器端直接实现对主题的订阅和消息发送等操作。




## 什么是共享订阅？有何使用场景？

**标签:** [*共享订阅*](tags.md#共享订阅)


共享订阅是 MQTT 5.0 标准的新特性，在标准发布前，EMQ X 就已经把共享订阅作为标准外特性进行了支持。在普通订阅中，所有订阅者都会收到订阅主题的所有消息，而在共享订阅中，订阅同一个主题的客户端会轮流的收到这个主题下的消息，也就是说同一个消息不会发送到多个订阅者，从而实现订阅端的多个节点之间的负载均衡。

共享订阅对于数据采集 / 集中处理类应用非常有用。在这样的场景下，数据的生产者远多余数据的消费者，且同一条数据只需要被任意消费者处理一次。


更多使用方式请参考 [共享订阅](https://docs.emqx.io/tutorial/v3/cn/advanced/share_subscribe.html)。




## 什么是离线消息？

**标签:** [*离线消息*](tags.md#离线消息)


一般情况下 MQTT 客户端仅在连接到消息服务器的时候，如果客户端离线将收不到消息。但是在客户端有固定的 ClientID，clean_session 为 false，且 QoS 设置满足服务器端的配置要求时，在客户端离线时，服务器可以为客户端保持一定量的离线消息，并在客户端再次连接是发送给客户端。

离线消息在网络连接不是很稳定时，或者对 QoS 有一定要求时非常有用。




## 什么是代理订阅？使用场景是什么？

**标签:** [*代理订阅*](tags.md#代理订阅)


通常情况客户端需要在连接到 EMQ X 之后主动订阅主题。代理订阅是指服务器为客户端订阅主题，这一过程不需要客户端参与，客户端和需要代理订阅的主题的对应关系保存在服务器中。

使用代理订阅可以集中管理大量的客户端的订阅，同时为客户端省略掉订阅这个步骤，可以节省客户端侧的计算资源和网络带宽。

以 Redis 数据库为例，代理订阅在 EMQ X 上使用方式请参考 [Redis 实现客户端代理订阅](https://docs.emqx.io/tutorial/v3/cn/backend/redis.html#%E5%AE%A2%E6%88%B7%E7%AB%AF%E4%BB%A3%E7%90%86%E8%AE%A2%E9%98%85)

::: tip
注：目前 EMQ X 企业版支持代理订阅。
:::




## 系统主题有何用处？都有哪些系统主题？

**标签:** [*系统主题*](tags.md#系统主题)


系统主题以 `$SYS/` 开头。EMQ X 会以系统主题的方式周期性的发布关于自身运行状态、MQTT 协议统计、客户端上下线状态到系统主题。订阅系统主题可以获得这些信息。

这里列举一些系统主题，完整的系统主题请参考 EMQ X 文档的相关章节：

- \$SYS/brokers:  集群节点列表
- \$SYS/brokers/\${node}/clients/${clientid}/connected: 当客户端连接时发送的客户端信息
- \$SYS/broker/${node}/stats/connections/count: 当前客户端总数
- \$SYS/broker/${node}/stats/sessions/count: 当前会话总数