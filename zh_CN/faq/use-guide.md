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

# 使用教程
## 怎么样才能使用 EMQX？


EMQX 开源版可免费下载使用，下载地址：[https://www.emqx.com/zh/try?product=broker](https://www.emqx.com/zh/try?product=broker)

EMQX 企业版支持下载试用，用户可以在 [https://www.emqx.com/zh/try?product=enterprise](https://www.emqx.com/zh/try?product=enterprise) 下载，[申请试用 license](https://www.emqx.com/zh/apply-licenses/emqx)之后即可试用。

另外，还可以在公有云直接创建 EMQX 企业版：

- [阿里云](https://market.aliyun.com/products/56014009/cmjj029979.html?spm=5176.730005.productlist.d_cmjj029979.69013524xism4L&innerSource=search_EMQ)

- [青云](https://appcenter.qingcloud.com/search/category/iot)




## 怎样更新 EMQX license?

**标签:** [*License*](tags.md#license)

[申请试用 license](https://www.emqx.com/zh/apply-licenses/emqx) 后，License 文件将通过邮件发送，找到附件里的 zip 文件并解压，复制压缩包里的 `emqx.lic` 文件到 EMQX 的 license 目录.

点击 "Download License" 按钮下载 license, 然后找到您下载的 "license.zip" 文件并解压.

从压缩文件中提取许可证文件（emqx.lic）到 EMQX 用户可读的目录中。

在提取完成后，需要从命令行重新加载许可证以完成更新。

拷贝完成后需要通过命令行重新加载 license 以完成更新：

基础命令：

```
emqx_ctl license reload [license 文件所在路径]
```

不同安装方式更新命令如下：

```
## 适用于 zip 包
./bin/emqx_ctl license reload path/to/emqx.lic

## DEB/RPM 包安装
emqx_ctl license reload path/to/emqx.lic

## Docker 镜像安装
docker exec -it emqx-ee emqx_ctl license reload path/to/emqx.lic
```

::: tip
在一个多节点集群中，`emqx_ctl license reload`命令只需要在其中一个节点上执行，因为许可证将被复制并应用到所有成员。 每一个节点都会在配置好的 EMQX 的数据目录下包含一份新的许可证，以及一份旧的许可证的备份（如果有的话）。

注意，对于e4.3.10之前的EMQX版本，此命令只在执行命令的本地节点上生效，所以对于那些旧版本，此命令需要在集群的每个节点上执行。
:::



## EMQX 支持私有协议进行扩展吗？如支持应该如何实现？

**标签:** [*多协议*](tags.md#多协议)  [*扩展*](tags.md#扩展)


对于新开发的私有协议，EMQX 提供一套 TCP 协议接入规范，私有协议可以按照该规范进行开发接入。如果您所使用的协议已经定型或协议底层非 TCP，可以通过网关进行转换处理，之后通过 MQTT 协议接入 EMQX，或直接联系 EMQ 官方支持私有协议适配。




## 我可以捕获设备上下线的事件吗？该如何使用？

**标签:** [*WebHook*](tags.md#webhook)  [*系统主题*](tags.md#系统主题)


EMQX 企业版可以通过以下的三种方式捕获设备的上下线的事件，

- Web Hook
- 订阅相关的 $SYS 主题
  - $SYS/brokers/${node}/clients/${clientid}/connected
  - $SYS/brokers/${node}/clients/${clientid}/disconnected
- 直接保存到数据库

最后一种方法只有在企业版里才支持，支持的数据库包括 Redis、MySQL、PostgreSQL、MongoDB 和 Cassandra。用户可以通过配置文件指定所要保存的数据库，以及监听 client.connected 和 client.disconnected 事件，这样在设备上、下线的时候把数据保存到数据库中。




## 我想限定某些主题只为特定的客户端所使用，EMQX 该如何进行配置？

**标签:** [*ACL*](tags.md#acl)  [*发布订阅*](tags.md#发布订阅)


EMQX 支持限定客户端可以使用的主题，从而实现设备权限的管理。如果要做这样的限定，需要在 EMQX 启用 ACL（Access Control List），并禁用匿名访问和关闭无 ACL 命中的访问许可（为了测试调试方便，在默认配置中，后两项是开启的，请注意关闭）。

```bash
## etc/emqx.conf

## ACL nomatch
mqtt.acl_nomatch = allow
```

ACL 可以配置在文件 `etc/acl.conf` 中，或者配置在后台数据库中。下面例子是 ACL 控制文件的一个配置行，含义是用户 “dashboard” 可以订阅 “$SYS/#” 主题。ACL 在后台数据库中的配置思想与此类似，详细配置方法请参阅 EMQX 文档的 [ACL 访问控制](https://docs.emqx.io/tutorial/v3/cn/security/acl.html) 章节。
```
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.
```




## EMQX 能做流量控制吗？

**标签:** [*流量控制*](tags.md#流量控制)


能。目前 EMQX 支持连接速率和消息率控制。配置如下：

```
## Value: Number
listener.tcp.external.max_conn_rate = 1000

## Value: rate,burst
listener.tcp.external.rate_limit = 1024,4096
```




## EMQX 是如何实现支持大规模并发和高可用的？

**标签:** [*性能*](tags.md#性能)  [*高并发*](tags.md#高并发)


高并发和高可用是 EMQX 的设计目标，为了实现这些目标 EMQX 中应用了多种技术，比如：

- 利用 Erlang/OTP 平台的软实时、高并发和容错；
- 全异步架构；
- 连接、会话、路由、集群的分层设计；
- 消息平面和控制平面的分离等。

在精心设计和实现之后，单个 EMQX Enterprise 节点就可以处理百万级的连接。

EMQX 支持多节点集群，集群下整个系统的性能会成倍高于单节点，并能在单节点故障时保证系统服务不中断。




## EMQX 能把接入的 MQTT 消息保存到数据库吗？

**标签:** [*持久化*](tags.md#持久化)


EMQX 企业版支持消息持久化，可以将消息保存到数据库，开源版还暂时不支持。目前 EMQX 企业版消息持久化支持的数据库有：

- Redis
- MongoDB
- MySQL
- PostgreSQL
- Cassandra
- AWS DynamoDB
- TimescaleDB
- OpenTSDB
- InfluxDB

有关数据持久化的支持请参见 [EMQX 数据持久化概览](https://docs.emqx.io/tutorial/v3/cn/backend/whats_backend.html)。




## 在服务器端能够直接断开一个 MQTT 连接吗？

**标签:** [*HTTP API*](tags.md#http-api)  [*Dashboard*](tags.md#dashboard)


可以的。EMQX 提供的 HTTP API 中包含断开 MQTT 连接，该操作在 EMQX 2.x 和 3.0 的实现方式有所不同：

- 在 2.x 版本中是由 EMQX 自定义扩展协议实现的
- 在 3.0 版本之后按照 MQTT 5.0 协议对从服务器端断开连接的规范要求实现的

调用的 API 如下所示：

```html
HTTP 方法：DELETE
URL：api/[v2|v3]/clients/{clientid}
<!--请注意区分 URL 中第二部分的版本号，请根据使用的版本号来决定 -->

返回内容：
{
    "code": 0,
    "result": []
}
```

HTTP API 使用方式参考 [管理监控API (HTTP API)](https://docs.emqx.io/broker/v3/cn/rest.html)




## EMQX 能把接入的消息转发到 Kafka 吗？

**标签:** [*Kafka*](tags.md#kafka)  [*桥接*](tags.md#桥接)  [*持久化*](tags.md#持久化)


能。目前 EMQX 企业版提供了内置的 Kafka 桥接方式，支持把消息桥接至 Kafka 进行流式处理。

EMQX 使用 Kafka 参照 [EMQX 到 Kafka 的桥接](https://docs.emqx.io/tutorial/v3/cn/bridge/bridge_to_kafka.html)




## EMQX 企业版中桥接 Kafka，一条 MQTT 消息到达 EMQX 集群之后就回 MQTT Ack 报文还是写入 Kafka 之后才回 MQTT Ack 报文?

**标签:** [*Kafka*](tags.md#kafka)  [*配置*](tags.md#配置)


取决于 Kafka 桥接的配置，配置文件位于`/etc/emqx/plugins/emqx_bridge_kafka.conf`

```bash
## Pick a partition producer and sync/async.
bridge.kafka.produce = sync
```

- 同步：EMQX 在收到 Kafka 返回的 Ack 之后才会给前端返回 MQTT Ack 报文
- 异步：MQTT 消息到达 EMQX 集群之后就回 MQTT Ack 报文，而不会等待 Kafka 返回给 EMQX 的 Ack

如果运行期间，后端的 Kafka 服务不可用，则消息会被累积在 EMQX 服务器中，

- EMQX 2.4.3 之前的版本会将未发送至 Kafka 的消息在内存中进行缓存，直至内存使用完毕，并且会导致 EMQX 服务不可用。
- EMQX 2.4.3 版本开始会将未发送至 Kafka 的消息在磁盘中进行缓存，如果磁盘用完可能会导致数据丢失。

因此建议做好 Kafka 服务的监控，在发现 Kafka 服务有异常情况的时候尽快恢复 Kafka 服务。




## EMQX 支持集群自动发现吗？有哪些实现方式？

**标签:** [*集群*](tags.md#集群)


EMQX 支持集群自动发现。集群可以通过手动配置或自动配置的方式实现。

目前支持的自动发现方式有：

- 手动集群
- 静态集群
- IP Multi-cast 自动集群
- DNS 自动集群
- ETCD 自动集群
- K8S 自动集群

有关集群概念和组建集群方式请参照 [EMQX 的集群概念](https://docs.emqx.io/tutorial/v3/cn/cluster/whats_cluster.html)




## 我可以把 MQTT 消息从 EMQX 转发其他消息中间件吗？例如 RabbitMQ？

**标签:** [*RabbitMQ*](tags.md#rabbitmq)  [*桥接*](tags.md#桥接)  [*持久化*](tags.md#持久化)


EMQX 支持转发消息到其他消息中间件，通过 EMQX 提供的桥接方式就可以做基于主题级别的配置，从而实现主题级别的消息转发。

EMQX 桥接相关的使用方式请参照 [EMQX 桥接](https://docs.emqx.io/tutorial/v3/cn/bridge/bridge.html)




## 我可以把消息从 EMQX 转到公有云 MQTT 服务上吗？比如 AWS 或者 Azure 的 IoT Hub？

**标签:** [*桥接*](tags.md#桥接)


EMQX 可以转发消息到标准 MQTT Broker，包括其他 MQTT 实现、公有云的 IoT Hub，通过 EMQX 提供的桥接就可以实现。




## MQTT Broker（比如 Mosquitto）可以转发消息到 EMQX 吗？

**标签:** [*Mosquitto*](tags.md#mosquitto)  [*桥接*](tags.md#桥接)


Mosquitto 可以配置转发消息到 EMQX，请参考[数据桥接](../rule/bridge_emqx.md)。

::: tip
> EMQX 桥接相关的使用方式请参照 [EMQX 桥接](../rule/bridge_emqx.md)
:::


## 我想跟踪特定消息的发布和订阅过程，应该如何做？

**标签:** [*Trace*](tags.md#trace)  [*调试*](tags.md#调试)


EMQX 支持追踪来自某个客户端的报文或者发布到某个主题的报文。追踪消息的发布和订阅需要使用命令行工具（emqx_ctl）的 trace 命令，下面给出一个追踪‘topic’主题的消息并保存在 `trace_topic.log` 中的例子。更详细的说明请参阅 EMQX 文档的相关章节。

```
./bin/emqx_ctl trace topic "topic" "trace_topic.log"
```




## 为什么我做压力测试的时候，连接数目和吞吐量老是上不去，有系统调优指南吗？

**标签:** [*调试*](tags.md#调试)  [*性能测试*](tags.md#性能测试)


在做压力测试的时候，除了要选用有足够计算能力的硬件，也需要对软件运行环境做一定的调优。比如修改修改操作系统的全局最大文件句柄数，允许用户打开的文件句柄数，TCP 的 backlog 和 buffer，Erlang 虚拟机的进程数限制等等。甚至包括需要在客户端上做一定的调优以保证客户端可以有足够的连接资源。

系统的调优在不同的需求下有不同的方式，在 EMQX 的[文档-测试调优](https://developer.emqx.io/docs/broker/v3/cn/tune.html) 中对用于普通场景的调优有较详细的说明




## EMQX 支持加密连接吗？推荐的部署方案是什么？

**标签:** [*TLS*](tags.md#tls)  [*加密连接*](tags.md#加密连接)


EMQX 支持加密连接。在生产环境部署时，推荐的方案是使用负载均衡终结 TLS。通过该方式，设备端和服务器端（负载均衡）的采用加密的连接，而负载均衡和后端的 EMQX 节点采用一般的 TCP 连接。




## EMQX 安装之后无法启动怎么排查？

**标签:** [*调试*](tags.md#调试)


执行 `$ emqx console` ，查看输出内容

+	`logger` 命令缺失

  ```
  $ emqx console
  Exec: /usr/lib/emqx/erts-10.3.5.1/bin/erlexec -boot /usr/lib/emqx/releases/v3.2.1/emqx -mode embedded -boot_var ERTS_LIB_DIR /usr/lib/emqx/erts-10.3.5.1/../lib -mnesia dir "/var/lib/emqx/mnesia/emqx@127.0.0.1" -config /var/lib/emqx/configs/app.2019.07.23.03.07.32.config -args_file /var/lib/emqx/configs/vm.2019.07.23.03.07.32.args -vm_args /var/lib/emqx/configs/vm.2019.07.23.03.07.32.args -- console
  Root: /usr/lib/emqx
  /usr/lib/emqx
  /usr/bin/emqx: line 510: logger: command not found
  ```

  **解决办法：**

  + `Centos/Redhat`

    ```
    $ yum install rsyslog
    ```

  + `Ubuntu/Debian`

    ```
    $ apt-get install bsdutils
    ```

+	`openssl` 缺失

```
    $ emqx console
    Exec: /emqx/erts-10.3/bin/erlexec -boot /emqx/releases/v3.2.1/emqx -mode embedded -boot_var ERTS_LIB_DIR /emqx/erts-10.3/../lib -mnesia dir "/emqx/data/mnesia/emqx@127.0.0.1" -config /emqx/data/configs/app.2019.07.23.03.34.43.config -args_file /emqx/data/configs/vm.2019.07.23.03.34.43.args -vm_args /emqx/data/configs/vm.2019.07.23.03.34.43.args -- console
    Root: /emqx
    /emqx
    Erlang/OTP 21 [erts-10.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:32] [hipe]

    {"Kernel pid terminated",application_controller,"{application_start_failure,kernel,{{shutdown,{failed_to_start_child,kernel_safe_sup,{on_load_function_failed,crypto}}},{kernel,start,[normal,[]]}}}"}
    Kernel pid terminated (application_controller) ({application_start_failure,kernel,{{shutdown,{failed_to_start_child,kernel_safe_sup,{on_load_function_failed,crypto}}},{kernel,start,[normal,[]]}}})

    Crash dump is being written to: log/crash.dump...done
```

**解决办法：**
安装1.1.1以上版本的 `openssl`

+ `License` 文件缺失

```
  $ emqx console
  Exec: /usr/lib/emqx/erts-10.3.5.1/bin/erlexec -boot /usr/lib/emqx/releases/v3.2.1/emqx -mode embedded -boot_var ERTS_LIB_DIR /usr/lib/emqx/erts-10.3.5.1/../lib -mnesia dir "/var/lib/emqx/mnesia/emqx@127.0.0.1" -config /var/lib/emqx/configs/app.2019.07.23.05.52.46.config -args_file /var/lib/emqx/configs/vm.2019.07.23.05.52.46.args -vm_args /var/lib/emqx/configs/vm.2019.07.23.05.52.46.args -- console
  Root: /usr/lib/emqx
  /usr/lib/emqx
  Erlang/OTP 21 [erts-10.3.5.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:32] [hipe]

  Starting emqx on node emqx@127.0.0.1
  Start http:management listener on 8080 successfully.
  Start http:dashboard listener on 18083 successfully.
  Start mqtt:tcp listener on 127.0.0.1:11883 successfully.
  Start mqtt:tcp listener on 0.0.0.0:1883 successfully.
  Start mqtt:ws listener on 0.0.0.0:8083 successfully.
  Start mqtt:ssl listener on 0.0.0.0:8883 successfully.
  Start mqtt:wss listener on 0.0.0.0:8084 successfully.
  EMQX 3.2.1 is running now!
  "The license certificate is expired!"
  2019-07-23 05:52:51.355 [critical] The license certificate is expired!
  2019-07-23 05:52:51.355 [critical] The license certificate is expired! System shutdown!
  Stop mqtt:tcp listener on 127.0.0.1:11883 successfully.
  Stop mqtt:tcp listener on 0.0.0.0:1883 successfully.
  Stop mqtt:ws listener on 0.0.0.0:8083 successfully.
  Stop mqtt:ssl listener on 0.0.0.0:8883 successfully.
  Stop mqtt:wss listener on 0.0.0.0:8084 successfully.
  [os_mon] memory supervisor port (memsup): Erlang has closed
  [os_mon] cpu supervisor port (cpu_sup): Erlang has closed
```

  **解决办法：**
  登陆[emqx.io](https://emqx.io)申请license或安装开源版的 EMQX Broker




## EMQX中ssl resumption session的使用

**标签:** [*TLS*](tags.md#tls)


修改emqx.conf配置中的 reuse_sessions = on 并生效后。如果客户端与服务端通过 SSL 已经连接成功，当第二次遇到客户端连接时，会跳过 SSL 握手阶段，直接建立连接，节省连接时间，增加客户端连接速度。




## MQTT 客户端断开连接统计

**标签:** [*指标*](tags.md#指标)


执行 `emqx_ctl listeners`，查看对应端口下的 `shutdown_count` 统计。

客户端断开链接错误码列表：

+ `keepalive_timeout`：MQTT keepalive 超时
+ `closed`：TCP客户端断开连接（客户端发来的FIN，但没收到 MQTT DISCONNECT）
+ `normal`：MQTT客户端正常断开
+ `einval`：EMQX 想向客户端发送一条消息，但是Socket 已经断开
+ `function_clause`：MQTT 报文格式错误
+ `etimedout`：TCP 发送超时（没有收到TCP ACK 回应）
+ `proto_unexpected_c`：在已经有一条MQTT连接的情况下重复收到了MQTT连接请求
+ `idle_timeout`： TCP 连接建立 15s 之后，还没收到 connect 报文

## EMQX 向订阅者转发消息的时候能否保证原始顺序？

**标签:** [*发布订阅*](tags.md#发布订阅)

EMQX 会保证来自同一客户端的相同主题的消息按照到达顺序被转发，这与消息的 QoS 等级无关，QoS 等级不会影响转发顺序。不管消息丢失还是重复，也都不会导致消息失序。这也是 MQTT 协议所要求的。

但对于不同主题的消息，EMQX 不会提供转发顺序保证，我们可以将他们视为进入了不同的通道，比如主题 A 的消息先于主题 B 的消息到达 EMQX，但最终可能主题 B 的消息会更早被转发。

## 当我遇到客户端连接、发布、订阅相关的问题时应该怎么办？

**标签:** [*调试*](tags.md#调试)

EMQX 的 Debug 日志基本记录了所有的行为和现象，通过阅读 Debug 日志我们能够知道客户端何时发起了连接，连接时指定了哪些参数，连接是否通过，被拒绝连接的原因是什么等等。但是由于 Debug 日志记录的信息过多，会带来额外的资源消耗，并且不利于我们针对单个客户端或主题进行分析。

所以 EMQX 提供了[日志追踪](../getting-started/log.md)功能，我们可以指定想要追踪的客户端或主题，EMQX 会将所有与该客户端或主题相关的 Debug 日志都输出到指定日志文件中。这样不管是自己分析调试，还是寻求社区帮助，都会方便许多。

需要注意的是，如果客户端是因为网络原因而无法连接到 EMQX 的话，日志追踪功能也是无法提供帮助的，因为此时 EMQX 尚未收到任何报文。这种情况很多时候是因为防火墙、安全组等网络配置原因导致服务器端口没有开放，这在使用云主机部署 EMQX 时尤为常见。所以除了日志追踪，我们可以通过检查端口占用、监听情况，检查网络配置等手段来排除网络方面的原因。

## 为什么会出现 Client ID 为 CENSYS 的或者是其他我不认识的客户端？

**标签:** [*安全*](tags.md#安全)

CENSYS 是一款互联网探测扫描工具，它会周期性扫描 IPv4 地址空间，探测 HTTP、SSH、MQTT 等协议的默认端口。所以如果你发现有 Client ID 为 CENSYS 的或者其他未知的客户端接入了你的 MQTT Broker，这意味你目前处于相对较低的安全性保障下。以下措施可以有效帮助你避免这个问题：

1. 不要使用默认配置，例如 EMQX 用于验证 HTTP API 访问权限的 AppID 与 AppSecret 等。
2. 启用认证，可以是用户名密码认证，也可以是 JWT 认证，避免只需要知道 IP 地址就可以登录的尴尬情况。
3. 启用 TLS 双向认证，只有持有有效证书的客户端才能接入系统。
4. 启用授权，避免非法设备登录后可以获取敏感数据。
5. 配置你的防火墙，尽量关闭一些不需要的端口。

## 能否以共享订阅的方式订阅系统主题？

**标签:** [*共享订阅*](tags.md#共享订阅)

可以，某些系统消息的发布频率可能较高，例如客户端上下线事件，所以采用共享订阅对于客户端来说是非常必要的。订阅示例： `$share/group1/$SYS/brokers/+/clients/+/connected`。

## 为什么共享订阅时收不到保留消息？

**标签:** [*共享订阅*](tags.md#共享订阅)

MQTT 协议规定了服务端不能发送保留消息给共享订阅。

## 为什么使用共享订阅时消息会偶尔丢失？

**标签:** [*共享订阅*](tags.md#共享订阅)

在共享订阅者的连接断开，但会话仍然存在的情况下，服务端会仍然向该共享订阅者投递消息，只是消息将被暂存至对应的会话中，这会导致其余在线的共享订阅者看起来没能完整地消费到所有消息。另外，如果该共享订阅者在重连时选择创建全新会话，那么缓存在旧会话中的消息就会永久丢失。

如果我们确认了不存在以上情况，而消息丢失的问题仍然存在，那么可以借助 EMQX 的客户端追踪功能来进行进一步的排查。

## EMQX 启动时提示端口被占用（eaddrinuse）应该怎么办？

**标签:** [*启动失败*](tags.md#启动失败)

默认情况下，EMQX 启动时会占用 7 个端口，它们分别是：

1. 1883，用于 MQTT over TCP 监听器，可通过配置修改
2. 8883，用于 MQTT over SSL/TLS 监听器，可通过配置修改
3. 8083，用于 MQTT over WebSocket 监听器，可通过配置修改
4. 8084，用于 MQTT over WSS (WebSocket over SSL) 监听器，可通过配置修改
5. 18083，HTTP API 服务的默认监听端口，Dashboard 功能也依赖于这个端口，可通过配置修改
6. 4370，用于 EMQX 分布式集群远程函数调用、Mnesia 数据同步等。即便没有组成集群，这个端口也会被默认占用。这个监听端口实际上应该是 `BasePort (4370) + Offset`，4370 固定无法修改，Offset 则由节点名称（`Name@Host`）中 Name 部分的数字后缀决定，没有数字后缀则默认为 0。例如 `emqx@127.0.0.1` 的 Offset 为 0，`emqx1@127.0.0.1` 的 Offset 为 1。
7. 5370，用于分担上一端口压力的集群 RPC 端口，主要用于节点间转发 MQTT 消息。与 4370 端口类似，即便没有组成集群，这个端口也会被默认占用，并且它实际上应该是 `BasePort (5370) + Offset`，5370 固定无法修改，Offset 则由节点名称（`Name@Host`）中 Name 部分的数字后缀决定，没有数字后缀则默认为 0。

## 为什么 EMQX 启动时会输出日志 “ WARNING: Default (insecure) Erlang cookie is in use.”

**标签:** [*日志*](tags.md#日志)

完整的 WARNING 日志如下：

```
WARNING: Default (insecure) Erlang cookie is in use.
WARNING: Configure node.cookie in /usr/lib/emqx/etc/emqx.conf or override from environment variable EMQX_NODE__COOKIE
WARNING: NOTE: Use the same cookie for all nodes in the cluster.
```

只有使用相同 Cookie 的 EMQX 节点才能组成一个集群。虽然 Cookie 并不能保证集群的通信安全，但它可以避免节点连接到它不打算与之通信的集群。EMQX 节点默认统一将 `emqxsecretcookie` 作为 Cookie，所以我们会推荐用户在搭建集群时更改 Cookie 的值。

第二条 WARNING 日志则提示了修改 Cookie 的两种方式，分别为 `emqx.conf` 配置文件中的 `node.cookie`，和环境变量 `EMQX_NODE__COOKIE`。

## 使用 Docker 部署 EMQX 时，为什么容器重启会导致配置的规则、资源等数据丢失？

**标签:** [*持久化*](tags.md#持久化)

EMQX 的运行时数据，譬如规则和资源的配置、保留消息等等，它们都存储在 `/opt/emqx/data` 目录下，所以为了保证这部分数据不会因容器重启而丢失，我们需要将 `/opt/emqx/data` 目录挂载到本地主机目录或者数据卷中去。

但我们可能会发现，即便已经挂载了 `/opt/emqx/data` 目录，数据仍然可能会在容器重启后丢失。这是因为 EMQX 的运行时数据实际上存储在 `/opt/emqx/data/mnesia/${Node Name}` 目录。所以数据丢失，实际上是容器重启后 EMQX 的节点名发生了变化，进而导致 EMQX 创建了一个全新的存储目录。

EMQX 节点名由 Name 和 Host 两部分组成，其中 Host 默认来自容器的 IP 地址。在默认的网络配置下容器一旦重启它的 IP 就可能发生变化，所以我们要做的就是让容器的 IP 保持固定。

EMQX 提供了一个环境变量 `EMQX_HOST`，允许我们自行设置节点名的 Host 部分。当然前提是这个 Host 必须是其他节点可以连接的，所以我们还需要配合网络别名使用：

```
docker run -d --name emqx -p 18083:18083 -p 1883:1883 -e EMQX_HOST=alias-for-emqx --network example --network-alias alias-for-emqx --mount type=bind,source=/tmp/emqx,target=/opt/emqx/data emqx:5.0.24
```

## 常见日志原因

**标签:** [*日志*](tags.md#日志)

### 关键字：“reason: {shutdown, takeovered}”

在 EMQX 中，每个 MQTT 客户端连接都由独立的一个进程来维护。每当有 MQTT 客户端连接，EMQX 就会创建一个新的进程。如果客户端希望从已存在的会话状态中恢复通信，那么 EMQX 就需要将旧进程中的会话状态数据迁移至新进程。这个过程，我们称之为 Take over，而旧进程将在接管工作完成后关闭，关闭的原因为 `{shutdown, takeovered}`。如果原先的客户端仍处于连接状态，那么 EMQX 还将发送一个 Reason Code 为 0x8E (Session taken over) 的 DISCONNECT 报文，然后关闭旧连接。

### 关键字：“reason: {shutdown, discarded}”

与会话接管相反，如果客户端在连接时表示希望开始一个全新的会话，那么 EMQX 就需要丢弃旧进程中的会话状态数据，EMQX 将以 `{shutdown, discarded}` 原因关闭旧进程。如果原先的客户端仍处于连接状态，那么 EMQX 同样将发送一个 Reason Code 为 0x8E (Session taken over) 的 DISCONNECT 报文，然后关闭旧连接。

### 关键字：“reason: {shutdown, kicked}”

表示客户端连接被手动踢除（在 Dashboard 上点击 Kick Out 按钮或者调用 `DELETE clients/{clientid}` API）。如果原先的客户端仍处于连接状态，那么 EMQX 同样将发送一个 Reason Code 为 0x98 (Administrative action) 的 DISCONNECT 报文，然后关闭旧连接。

### 关键字：“reason: {shutdown, tcp_closed}”

这一日志表示客户端在没有发送 DISCONNECT 报文的情况下直接关闭了网络连接，与之对应的是 `reason: {shutdown, normal}`，这表示客户端先发送了一个 Reason Code 为 0 的 DISCONNECT 报文，然后再关闭了网络连接。

所以，当发现 EMQX 日志中出现 `tcp_closed`，并且这一断开连接的行为并不是你期望的时，建议排查客户端的实现是否存在问题。

### 关键字：“maximum heap size reached“

出现这一日志说明当前客户端进程占用的堆栈已经超过了预设的最大值，客户端进程将被强制杀死以保证 EMQX 的可用性，避免客户端进程的内存占用无限制增长最终导致 EMQX OOM。

通常消息堆积是导致客户端进程堆栈占用上升的主要原因，而消息堆积通常是因为客户端消费能力与对端消息的生产能力不匹配，推荐的解决方案是优化客户端处理代码或者使用共享订阅分散负载。

与此相关的配置项是 `force_shutdown_policy`，它的配置格式为 `<Maximum Message Queue Length>|<Maximum Heap Size>`，例如 `10000|64MB`。其中 `<Maximum Heap Size>` 就是限制每个客户端进程能够占用的最大堆栈内存。

### 关键字：“Parse failed for function_clause“

这一日志表示报文解析失败。可能因为这不是一个 MQTT 报文，我们遇到过很多向 MQTT 端口发送 HTTP 请求的情况，也可能因为报文中包含了非 UTF-8 字符等等。我们可以在这条 “Parse failed...” 日志中检索 `Frame data` 关键字以查看完整的报文，帮助我们分析解析失败的可能原因。

### 关键字：“Dropped msg due to mqueue is full“

EMQX 可以同时发送多个未确认的 QoS 1 和 QoS 2 消息，但受限于客户端的性能，通常我们会限制允许同时发送的消息的最大数量。在达到这一限制后，后续到达的消息都会被 EMQX 缓存在每个客户端进程的消息队列中。为了防止消息缓存过多，EMQX 同样为消息队列设置了最大长度限制。如果消息到达时消息队列已满，那么这个最新的消息仍然会入队，但队列中最老的消息将会出队并被丢弃，同时产生 “Dropped msg due to mqueue is full” 这一日志。

如果此日志只是在流量高峰期出现，那么可以修改配置项 `max_mqueue_len` 仅增加消息队列的最大长度。但如果持续出现，那么说明当前客户端消费能力较差，尽量选择优化客户端代码或者使用共享订阅分散负载。
