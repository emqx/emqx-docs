# 使用教程
### 怎么样才能使用 EMQ X？


EMQ X 开源版可免费下载使用，下载地址：[https://www.emqx.io/downloads#broker](https://www.emqx.io/downloads#broker)

EMQ X 企业版支持下载试用，用户可以在 [https://www.emqx.io/downloads#enterprise](https://www.emqx.io/downloads#enterprise) 下载，[申请试用 license](https://www.emqx.io/licenses#trial)之后即可试用。

另外，还可以在公有云直接创建 EMQ X 企业版：

- [阿里云](https://market.aliyun.com/products/56014009/cmjj029979.html?spm=5176.730005.productlist.d_cmjj029979.69013524xism4L&innerSource=search_EMQ)

- [青云](https://appcenter.qingcloud.com/search/category/iot)




### 怎样更新 EMQ X license?

**标签:** [*License*](tags.md#license)


点击 "Download License" 按钮下载 license, 然后找到您下载的 "license.zip" 文件并解压.

复制压缩包里的两个文件 (emqx.lic, emqx.key) 到 EMQ X 的 license 目录.

如果您的 EMQX 是使用 zip 包安装的, 那么压缩包里的两个文件需要拷贝到 "emqx/etc/" 目录;
如果是用 DEB/RPM 包安装的, 两个文件需要拷贝到 "/etc/emqx/" 目录;
如果是用 Docker 镜像安装的, 两个文件需要拷贝到 "/opt/emqx/etc/" 目录.

拷贝完成后需要通过命令行重新加载 license 以完成更新：

基础命令：

```
emqx_ctl license reload [license 文件所在路径]
```

不同安装方式更新命令如下：

```
## 适用于 zip 包
./bin/emqx_ctl license reload etc/emqx.lic

## DEB/RPM 包安装
emqx_ctl license reload /etc/emqx/emqx.lic

## Docker 镜像安装
docker exec -it emqx-ee emqx_ctl license reload /opt/emqx/etc/emqx.lic
```




### EMQ X 支持私有协议进行扩展吗？如支持应该如何实现？

**标签:** [*多协议*](tags.md#多协议)  [*扩展*](tags.md#扩展)


对于新开发的私有协议，EMQ X 提供一套 TCP 协议接入规范，私有协议可以按照该规范进行开发接入。如果您所使用的协议已经定型或协议底层非 TCP，可以通过网关进行转换处理，之后通过 MQTT 协议接入 EMQ X，或直接联系 EMQ 官方支持私有协议适配。




### 我可以捕获设备上下线的事件吗？该如何使用？

**标签:** [*WebHook*](tags.md#webhook)  [*系统主题*](tags.md#系统主题)


EMQ X 企业版可以通过以下的三种方式捕获设备的上下线的事件，

- Web Hook
- 订阅相关的 $SYS 主题
  - $SYS/brokers/${node}/clients/${clientid}/connected
  - $SYS/brokers/${node}/clients/${clientid}/disconnected
- 直接保存到数据库

最后一种方法只有在企业版里才支持，支持的数据库包括 Redis、MySQL、PostgreSQL、MongoDB 和 Cassandra。用户可以通过配置文件指定所要保存的数据库，以及监听 client.connected 和 client.disconnected 事件，这样在设备上、下线的时候把数据保存到数据库中。




### 我想限定某些主题只为特定的客户端所使用，EMQ X 该如何进行配置？

**标签:** [*ACL*](tags.md#acl)  [*发布订阅*](tags.md#发布订阅)


EMQ X 支持限定客户端可以使用的主题，从而实现设备权限的管理。如果要做这样的限定，需要在 EMQ X 启用 ACL（Access Control List），并禁用匿名访问和关闭无 ACL 命中的访问许可（为了测试调试方便，在默认配置中，后两项是开启的，请注意关闭）。

```bash
## etc/emqx.conf

## ACL nomatch
mqtt.acl_nomatch = allow
```

ACL 可以配置在文件 `etc/acl.conf` 中，或者配置在后台数据库中。下面例子是 ACL 控制文件的一个配置行，含义是用户 “dashboard” 可以订阅 “$SYS/#” 主题。ACL 在后台数据库中的配置思想与此类似，详细配置方法请参阅 EMQ X 文档的 [ACL 访问控制](https://docs.emqx.io/tutorial/v3/cn/security/acl.html) 章节。
```
{allow, {user, "dashboard"}, subscribe, ["$SYS/#"]}.
```




### EMQ X 能做流量控制吗？

**标签:** [*流量控制*](tags.md#流量控制)


能。目前 EMQ X 支持连接速率和消息率控制。配置如下：

```
## Value: Number
listener.tcp.external.max_conn_rate = 1000

## Value: rate,burst
listener.tcp.external.rate_limit = 1024,4096
```




### EMQ X 是如何实现支持大规模并发和高可用的？

**标签:** [*性能*](tags.md#性能)  [*高并发*](tags.md#高并发)


高并发和高可用是 EMQ X 的设计目标，为了实现这些目标 EMQ X 中应用了多种技术，比如：

- 利用 Erlang/OTP 平台的软实时、高并发和容错；
- 全异步架构；
- 连接、会话、路由、集群的分层设计；
- 消息平面和控制平面的分离等。

在精心设计和实现之后，单个 EMQ X Enterprise 节点就可以处理百万级的连接。

EMQ X 支持多节点集群，集群下整个系统的性能会成倍高于单节点，并能在单节点故障时保证系统服务不中断。




### EMQ X 能把接入的 MQTT 消息保存到数据库吗？

**标签:** [*持久化*](tags.md#持久化)


EMQ X 企业版支持消息持久化，可以将消息保存到数据库，开源版还暂时不支持。目前 EMQ X 企业版消息持久化支持的数据库有：

- Redis
- MongoDB
- MySQL
- PostgreSQL
- Cassandra
- AWS DynamoDB
- TimescaleDB
- OpenTSDB
- InfluxDB

有关数据持久化的支持请参见 [EMQ X 数据持久化概览](https://docs.emqx.io/tutorial/v3/cn/backend/whats_backend.html)。




### 在服务器端能够直接断开一个 MQTT 连接吗？

**标签:** [*RESt API*](tags.md#rest-api)  [*Dashboard*](tags.md#dashboard)


可以的。EMQ X 提供的 REST API 中包含断开 MQTT 连接，该操作在 EMQ X 2.x 和 3.0 的实现方式有所不同：

- 在 2.x 版本中是由 EMQ X 自定义扩展协议实现的
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

REST API 使用方式参考 [管理监控API (REST API)](https://docs.emqx.io/broker/v3/cn/rest.html)




### EMQ X 能把接入的消息转发到 Kafka 吗？

**标签:** [*Kafka*](tags.md#kafka)  [*桥接*](tags.md#桥接)  [*持久化*](tags.md#持久化)


能。目前 EMQ X 企业版提供了内置的 Kafka 桥接方式，支持把消息桥接至 Kafka 进行流式处理。

EMQ X 使用 Kafka 参照 [EMQ X 到 Kafka 的桥接](https://docs.emqx.io/tutorial/v3/cn/bridge/bridge_to_kafka.html)




### EMQ X 企业版中桥接 Kafka，一条 MQTT 消息到达 EMQ X 集群之后就回 MQTT Ack 报文还是写入 Kafka 之后才回 MQTT Ack 报文?

**标签:** [*Kafka*](tags.md#kafka)  [*配置*](tags.md#配置)


取决于 Kafka 桥接的配置，配置文件位于`/etc/emqx/plugins/emqx_bridge_kafka.conf`

```bash
## Pick a partition producer and sync/async.
bridge.kafka.produce = sync
```

- 同步：EMQ X 在收到 Kafka 返回的 Ack 之后才会给前端返回 MQTT Ack 报文
- 异步：MQTT 消息到达 EMQ X 集群之后就回 MQTT Ack 报文，而不会等待 Kafka 返回给 EMQ X 的 Ack

如果运行期间，后端的 Kafka 服务不可用，则消息会被累积在 EMQ X 服务器中，

- EMQ X 2.4.3 之前的版本会将未发送至 Kafka 的消息在内存中进行缓存，直至内存使用完毕，并且会导致 EMQ X 服务不可用。
- EMQ X 2.4.3 版本开始会将未发送至 Kafka 的消息在磁盘中进行缓存，如果磁盘用完可能会导致数据丢失。

因此建议做好 Kafka 服务的监控，在发现 Kafka 服务有异常情况的时候尽快恢复 Kafka 服务。




### EMQ X 支持集群自动发现吗？有哪些实现方式？

**标签:** [*集群*](tags.md#集群)


EMQ X 支持集群自动发现。集群可以通过手动配置或自动配置的方式实现。

目前支持的自动发现方式有：

- 手动集群
- 静态集群
- IP Multi-cast 自动集群
- DNS 自动集群
- ETCD 自动集群
- K8S 自动集群

有关集群概念和组建集群方式请参照 [EMQ X 的集群概念](https://docs.emqx.io/tutorial/v3/cn/cluster/whats_cluster.html)




### 我可以把 MQTT 消息从 EMQ X 转发其他消息中间件吗？例如 RabbitMQ？

**标签:** [*RabbitMQ*](tags.md#rabbitmq)  [*桥接*](tags.md#桥接)  [*持久化*](tags.md#持久化)


EMQ X 支持转发消息到其他消息中间件，通过 EMQ X 提供的桥接方式就可以做基于主题级别的配置，从而实现主题级别的消息转发。

EMQ X 桥接相关的使用方式请参照 [EMQ X 桥接](https://docs.emqx.io/tutorial/v3/cn/bridge/bridge.html)




### 我可以把消息从 EMQ X 转到公有云 MQTT 服务上吗？比如 AWS 或者 Azure 的 IoT Hub？

**标签:** [*桥接*](tags.md#桥接)


EMQ X 可以转发消息到标准 MQTT Broker，包括其他 MQTT 实现、公有云的 IoT Hub，通过 EMQ X 提供的桥接就可以实现。




### MQTT Broker（比如 Mosquitto）可以转发消息到 EMQ X 吗？

**标签:** [*Mosquitto*](tags.md#mosquitto)  [*桥接*](tags.md#桥接)


Mosquitto 可以配置转发消息到 EMQ X，请参考[数据桥接](https://developer.emqx.io/docs/tutorial/zh/bridge/bridge.html)。

> EMQ X 桥接相关的使用方式请参照 [EMQ X 桥接](https://docs.emqx.io/tutorial/v3/cn/bridge/bridge.html)




### 我想跟踪特定消息的发布和订阅过程，应该如何做？

**标签:** [*Trace*](tags.md#trace)  [*调试*](tags.md#调试)


EMQ X 支持追踪来自某个客户端的报文或者发布到某个主题的报文。追踪消息的发布和订阅需要使用命令行工具（emqx_ctl）的 trace 命令，下面给出一个追踪‘topic’主题的消息并保存在 `trace_topic.log` 中的例子。更详细的说明请参阅 EMQ X 文档的相关章节。

```
./bin/emqx_ctl trace topic "topic" "trace_topic.log"
```




### 为什么我做压力测试的时候，连接数目和吞吐量老是上不去，有系统调优指南吗？

**标签:** [*调试*](tags.md#调试)  [*性能测试*](tags.md#性能测试)


在做压力测试的时候，除了要选用有足够计算能力的硬件，也需要对软件运行环境做一定的调优。比如修改修改操作系统的全局最大文件句柄数，允许用户打开的文件句柄数，TCP 的 backlog 和 buffer，Erlang 虚拟机的进程数限制等等。甚至包括需要在客户端上做一定的调优以保证客户端可以有足够的连接资源。

系统的调优在不同的需求下有不同的方式，在 EMQ X 的[文档-测试调优](https://developer.emqx.io/docs/broker/v3/cn/tune.html) 中对用于普通场景的调优有较详细的说明




### EMQ X 支持加密连接吗？推荐的部署方案是什么？

**标签:** [*TLS*](tags.md#tls)  [*加密连接*](tags.md#加密连接)


EMQ X 支持加密连接。在生产环境部署时，推荐的方案是使用负载均衡终结 TLS。通过该方式，设备端和服务器端（负载均衡）的采用加密的连接，而负载均衡和后端的 EMQ X 节点采用一般的 TCP 连接。




### EMQ X 安装之后无法启动怎么排查？

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

**解决办法：**安装1.1.1以上版本的 `openssl`

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
  EMQ X Broker 3.2.1 is running now!
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

  **解决办法：**登陆[emqx.io](https://emqx.io)申请license或安装开源版的 EMQ X Broker




### EMQ X中ssl resumption session的使用

**标签:** [*TLS*](tags.md#tls)


修改emqx.conf配置中的 reuse_sessions = on 并生效后。如果客户端与服务端通过 SSL 已经连接成功，当第二次遇到客户端连接时，会跳过 SSL 握手阶段，直接建立连接，节省连接时间，增加客户端连接速度。




### MQTT 客户端断开连接统计

**标签:** [*指标*](tags.md#指标)


执行 `emqx_ctl listeners`，查看对应端口下的 `shutdown_count` 统计。

客户端断开链接错误码列表：

+ `keepalive_timeout`：MQTT keepalive 超时
+ `closed`：TCP客户端断开连接（客户端发来的FIN，但没收到 MQTT DISCONNECT）
+ `normal`：MQTT客户端正常断开
+ `einval`：EMQ X 想向客户端发送一条消息，但是Socket 已经断开
+ `function_clause`：MQTT 报文格式错误
+ `etimedout`：TCP 发送超时（没有收到TCP ACK 回应）
+ `proto_unexpected_c`：在已经有一条MQTT连接的情况下重复收到了MQTT连接请求
+ `idle_timeout`： TCP 连接建立 15s 之后，还没收到 connect 报文