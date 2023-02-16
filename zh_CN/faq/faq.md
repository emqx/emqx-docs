# 常见问题解答

## EMQX 的主题数量有限制吗？

主题使用没有数量限制，主题数量增长对性能影响不大，可以放心使用。

{% emqxce %}

## EMQX 开源版怎么存储数据？

开源版不支持数据存储功能，可以使用企业版，或者使用外部程序订阅主题/Webhook 的方式获取数据，然后写入到数据库。

{% endemqxce %}

## EMQX 与物联网应用的关系是什么？

典型的物联网应用包括设备硬件、数据采集、数据存储、分析、Web / 移动应用等。EMQX 位于数据采集这一层，分别与硬件和数据存储、分析进行交互，是物联网应用的核心：前端的硬件通过 MQTT 协议与位于数据采集层的 EMQX 交互，通过 EMQX 将数据采集后，通过 EMQX 提供的数据接口，将数据保存到后台的持久化平台中（各种关系型数据库和 NOSQL 数据库），或者流式数据处理框架等，上层应用通过这些数据分析后得到的结果呈现给最终用户。

## EMQX 有哪些产品？

EMQX 公司主要提供[三个产品](https://www.emqx.com/zh/products/emqx)，主要体现在支持的连接数量、产品功能和商业服务等方面的区别：

- EMQX Broker：EMQX 开源版，提供 MQTT 协议、CoAP 和 LwM2M 等常见物联网协议的支持；

- EMQX Enterprise：EMQX 企业版，在开源版基础上，增加了数据持久化 Redis、MySQL、MongoDB 或 PostgreSQL，数据桥接转发 Kafka，LoRaWAN 支持，监控管理，Kubernetes 部署等方面的支持；支持百万级并发连接；

- EMQX Cloud：[EMQX Cloud](https://www.emqx.com/zh/cloud) 是 EMQ 公司推出的一款面向物联网领域的 MQTT 消息中间件产品。作为全球首个全托管的 MQTT 5.0 公有云服务，EMQX Cloud 提供了一站式运维代管、独有隔离环境的 MQTT 消息服务。在万物互联的时代，EMQX Cloud 可以帮助您快速构建面向物联网领域的行业应用，轻松实现物联网数据的采集、传输、计算和持久化。

## EMQX 与 NB-IoT、LoRAWAN 的关系是什么？

EMQX 是一个开源的 MQTT 消息服务器，并且 MQTT 是一个 TCP 协议栈上位于应用层的协议；而 NB-IoT 和 LoRAWAN 在 TCP 协议层处于物理层，负责物理信号的传输。因此两者在 TCP 协议栈的不同层次上，实现不同的功能。

## MQTT 协议与 HTTP 协议相比，有何优点和弱点?

HTTP 协议是一个无状态的协议，每个 HTTP 请求为 TCP 短连接，每次请求都需要重新创建一个 TCP 连接（可以通过 keep-alive 属性来优化 TCP 连接的使用，多个 HTTP 请求可以共享该 TCP 连接）；而 MQTT 协议为长连接协议，每个客户端都会保持一个长连接。与 HTTP 协议相比优势在于：

- MQTT 的长连接可以用于实现从设备端到服务器端的消息传送之外，还可以实现从服务器端到设备端的实时控制消息发送，而 HTTP 协议要实现此功能只能通过轮询的方式，效率相对来说比较低；

- MQTT 协议在维护连接的时候会发送心跳包，因此协议以最小代价内置支持设备 “探活” 的功能，而 HTTP 协议要实现此功能的话需要单独发出 HTTP 请求，实现的代价会更高；

- 低带宽、低功耗。MQTT 在传输报文的大小上与 HTTP 相比有巨大的优势，因为 MQTT 协议在连接建立之后，由于避免了建立连接所需要的额外的资源消耗，发送实际数据的时候报文传输所需带宽与 HTTP 相比有很大的优势，参考网上[有人做的测评](https://medium.com/@flespi/http-vs-mqtt-performance-tests-f9adde693b5f)，发送一样大小的数据，MQTT 比 HTTP 少近 50 倍的网络传输数据，而且速度快了将近 20 倍。在网上有人做的[另外一个评测显示](http://stephendnicholas.com/posts/power-profiling-mqtt-vs-https)，接收消息的场景，MQTT 协议的耗电量为 HTTP 协议的百分之一，而发送数据的时候 MQTT 协议的耗电量为 HTTP 协议的十分之一；

- MQTT 提供消息质量控制（QoS），消息质量等级越高，消息交付的质量就越有保障，在物联网的应用场景下，用户可以根据不同的使用场景来设定不同的消息质量等级；

## 什么是 mqueue？如何配置 mqueue？

mqueue 是 EMQX 在消息发布流程中保存在会话中的一个消息队列，当 MQTT 连接报文中的 clean session 设置为 false 的时候，即使是客户端断开连接的情况下，EMQX 也会为断连客户端保存一个会话，这个会话会缓存订阅关系，并代替断开连接的客户端去接收订阅主题上的消息，而这些消息会存在 EMQX 的 mqueue 中，等到客户端重新上线再将消息重新发给客户端。由于 qos0 消息在 MQTT 协议中的优先级比较低，所以 EMQX 默认不缓存 qos 0 的消息，mqueue 在 EMQX 中是可以配置的，通过配置 `zone.$name.mqueue_store_qos0 = true` 可以将 qos0 消息也存在 mqueue 中，mqueue 的大小也是有限制的，通过配置项 `zone.external.max_mqueue_len` ，可以确定每个会话缓存的消息数量。注意，这些消息是存储在内存中的，所以尽量不要将 mqueue 长度限制修改为 0（设置为 0 代表 mqueue 长度没有限制），否则在实际的业务场景中，有内存耗光的风险。

## 什么是 WebSocket？什么情况下需要通过 WebSocket 去连接 EMQX 服务器？

WebSocket 是一种在基于 HTTP 协议上支持全双工通讯的协议，通过该协议，用户可以实现浏览器和服务器之间的双向通信，比如可以通过服务器往浏览器端推送消息。EMQX 提供了 WebSocket 连接支持，用户可以在浏览器端直接实现对主题的订阅和消息发送等操作。

## 什么是共享订阅？有何使用场景？

共享订阅是 MQTT 5.0 标准的新特性，在标准发布前，EMQX 就已经把共享订阅作为标准外特性进行了支持。在普通订阅中，所有订阅者都会收到订阅主题的所有消息，而在共享订阅中，订阅同一个主题的客户端会轮流的收到这个主题下的消息，也就是说同一个消息不会发送到多个订阅者，从而实现订阅端的多个节点之间的负载均衡。

共享订阅对于数据采集 / 集中处理类应用非常有用。在这样的场景下，数据的生产者远多余数据的消费者，且同一条数据只需要被任意消费者处理一次。

更多使用方式请参考 [共享订阅](../mqtt/mqtt-shared-subscription.md)。

## 什么是离线消息？

一般情况下 MQTT 客户端仅在连接到消息服务器的时候，如果客户端离线将收不到消息。但是在客户端有固定的 ClientID，clean_session 为 false，且 QoS 设置满足服务器端的配置要求时，在客户端离线时，服务器可以为客户端保持一定量的离线消息，并在客户端再次连接是发送给客户端。

离线消息在网络连接不是很稳定时，或者对 QoS 有一定要求时非常有用。

## 什么是代理订阅？使用场景是什么？

通常情况客户端需要在连接到 EMQX 之后主动订阅主题。代理订阅是指服务器为客户端订阅主题，这一过程不需要客户端参与，客户端和需要代理订阅的主题的对应关系保存在服务器中。

使用代理订阅可以集中管理大量的客户端的订阅，同时为客户端省略掉订阅这个步骤，可以节省客户端侧的计算资源和网络带宽。

## 系统主题有何用处？都有哪些系统主题？

系统主题以 `$SYS/` 开头。EMQX 会以系统主题的方式周期性的发布关于自身运行状态、MQTT 协议统计、客户端上下线状态到系统主题。订阅系统主题可以获得这些信息。

这里列举一些系统主题，完整的系统主题请参考 EMQX 文档的相关章节：

- \$SYS/brokers: 集群节点列表
- \$SYS/brokers/\${node}/clients/${clientid}/connected: 当客户端连接时发送的客户端信息
- \$SYS/broker/${node}/stats/connections/count: 当前客户端总数
- \$SYS/broker/${node}/stats/sessions/count: 当前会话总数

## OPENSSL 版本不正确

### 现象

执行 `./bin/emqx console` 输出的错误内容包含：

```bash
{application_start_failure,kernel,{{shutdown,{failed_to_start_child,kernel_safe_sup,{on_load_function_failed,crypto}}}, ..}
```

它表示，EMQX 依赖的 Erlang/OTP 中的 `crypto` 应用启动失败。

### 解决方法

#### Linux

进入到 EMQX 的安装目录（如果使用包管理工具安装 EMQX，则应该进入与 EMQX 的 `lib` 目录同级的位置）

```bash
## 安装包安装
$ cd emqx

## 包管理器安装，例如 yum。则它的 lib 目录应该在 /lib/emqx
$ cd /lib/emqx
```

查询 `crypto`依赖的 `.so` 动态库列表及其在内存中的地址：

```bash
$ ldd lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.6/priv/lib/crypto.so: /lib64/libcrypto.so.10: version `OPENSSL_1.1.1' not found (required by lib/crypto-4.6/priv/lib/crypto.so)
        linux-vdso.so.1 =>  (0x00007fff67bfc000)
        libcrypto.so.10 => /lib64/libcrypto.so.10 (0x00007fee749ca000)
        libc.so.6 => /lib64/libc.so.6 (0x00007fee74609000)
        libdl.so.2 => /lib64/libdl.so.2 (0x00007fee74404000)
        libz.so.1 => /lib64/libz.so.1 (0x00007fee741ee000)
        /lib64/ld-linux-x86-64.so.2 (0x00007fee74fe5000)

```

其中 `OPENSSL_1.1.1' not found`表明指定的 OPENSSL 版本的 `.so` 库未正确安装。

源码编译安装 OPENSSL 1.1.1，并将其 so 文件放置到可以被系统识别的路径：

```bash
## 下在最新版本 1.1.1
$ wget https://www.openssl.org/source/openssl-1.1.1c.tar.gz

## 上传至 ct-test-ha
$ scp openssl-1.1.1c.tar.gz ct-test-ha:~/

## 解压并编译安装
$ tar zxf   openssl-1.1.1c.tar.gz
$ cd openssl-1.1.1c
$ ./config
$ make test  # 执行测试；如果输出 PASS 则继续
$ make install

## 确保库的引用
$ ln -s /usr/local/lib64/libssl.so.1.1 /usr/lib64/libssl.so.1.1
$ ln -s /usr/local/lib64/libcrypto.so.1.1 /usr/lib64/libcrypto.so.1.1
```

完成后，执行在 EMQX 的 lib 同级目录下执行 `ldd lib/crypto-*/priv/lib/crypto.so` ，检查是否已能正确识别。如果不在有 `not found` 的 `.so` 库，即可正常启动 EMQX。

#### macOS

进入到 EMQX 的安装目录：

```bash
## 安装包安装
$ cd emqx

## brew 安装
$ cd /usr/local/Cellar/emqx/<version>/
```

查询 `crypto`依赖的 `.so` 动态库列表：

```bash
$ otool -L lib/crypto-*/priv/lib/crypto.so

lib/crypto-4.4.2.1/priv/lib/crypto.so:
	/usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib (compatibility version 1.1.0, current version 1.1.0)
	/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1252.200.5)
```

检查其显示 OPENSSL 已成功安装至指定的目录：

```bash
$ ls /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib
ls: /usr/local/opt/openssl@1.1/lib/libcrypto.1.1.dylib: No such file or directory
```

若不存在该文件，则需安装与 `otool` 打印出来的对应的 OPENSSL 版本，例如此处显示的为 `openssl@1.1`：

```bash
brew install openssl@1.1
```

安装完成后，即可正常启动 EMQX。

{% emqxce %}

## Windows 缺失 MSVCR120.dll

### 现象

Windows 执行 `./bin/emqx console` 弹出错误窗口：

```bash
无法启动次程序，因为计算机中丢失 MSVCR120.dll。请尝试重新安装该程序以解决此问题。
```

![error](./static/WechatIMG18396.png)

### 解决方法

安装 [Microsoft Visual C++ RedistributablePackage](https://www.microsoft.com/en-us/download/search.aspx?q=redistributable+package.)

{% endemqxce %}

## SSL 连接失败

### 现象

客户端无法与 EMQX 建立 SSL 连接。

### 解决方法

可以借助 EMQX 日志中的关键字来进行简单的问题排查，EMQX 日志相关内容请参考：[日志与追踪](../observability/tracer.md)。

1. certificate_expired

   日志中出现 `certificate_expired` 关键字，说明证书已经过期，请及时续签。

2. no_suitable_cipher

   日志中出现 `no_suitable_cipher` 关键字，说明握手过程中没有找到合适的密码套件，可能原因有证书类型与密码套件不匹配、没有找到服务端和客户端同时支持的密码套件等等。

3. handshake_failure

   日志中出现 `handshake_failure` 关键字，原因有很多，可能要结合客户端的报错来分析，例如，可能是客户端发现连接的服务器地址与服务器证书中的域名不匹配。

4. unknown_ca

   日志中出现 `unknown_ca` 关键字，意味着证书校验失败，常见原因有遗漏了中间 CA 证书、未指定 Root CA 证书或者指定了错误的 Root CA 证书。在双向认证中我们可以根据日志中的其他信息来判断是服务端还是客户端的证书配置出错。如果是服务端证书存在问题，那么报错日志通常为：

   ```bash
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify received CLIENT ALERT: Fatal - Unknown CA\n"}}}
   ```

   看到 `CLIENT ALERT` 就可以得知，这是来自客户端的警告信息，服务端证书未能通过客户端的检查。

   如果是客户端证书存在问题，那么报错日志通常为：

   ```bash
   {ssl_error,{tls_alert,{unknown_ca,"TLS server: In state certify at ssl_handshake.erl:1887 generated SERVER ALERT: Fatal - Unknown CA\n"}}}
   ```

   看到 `SERVER ALERT` 就能够得知，表示服务端在检查客户端证书时发现该证书无法通过认证，而客户端将收到来自服务端的警告信息。

5. protocol_version

   日志中出现 `protocol_version` 关键字，说明客户端与服务器支持的 TLS 协议版本不匹配。

## EMQX 支持私有协议进行扩展吗？如支持应该如何实现？

对于新开发的私有协议，EMQX 提供一套 TCP 协议接入规范，私有协议可以按照该规范进行开发接入。如果您所使用的协议已经定型或协议底层非 TCP，可以通过网关进行转换处理，之后通过 MQTT 协议接入 EMQX，或直接联系 EMQ 官方支持私有协议适配。

## 我可以捕获设备上下线的事件吗？该如何使用？

EMQX 企业版可以通过以下的三种方式捕获设备的上下线的事件，

- Web Hook
- 订阅相关的 $SYS 主题
  - $SYS/brokers/${node}/clients/${clientid}/connected
  - $SYS/brokers/${node}/clients/${clientid}/disconnected
- 直接保存到数据库

最后一种方法只有在企业版里才支持，支持的数据库包括 Redis、MySQL、PostgreSQL、MongoDB 和 Cassandra。用户可以通过配置文件指定所要保存的数据库，以及监听 client.connected 和 client.disconnected 事件，这样在设备上、下线的时候把数据保存到数据库中。

## 我想限定某些主题只为特定的客户端所使用，EMQX 该如何进行配置？

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

能。目前 EMQX 支持连接速率和消息率控制。配置如下：

```bash
## Value: Number
listener.tcp.external.max_conn_rate = 1000

## Value: rate,burst
listener.tcp.external.rate_limit = 1024,4096
```

## EMQX 是如何实现支持大规模并发和高可用的？

高并发和高可用是 EMQX 的设计目标，为了实现这些目标 EMQX 中应用了多种技术，比如：

- 利用 Erlang/OTP 平台的软实时、高并发和容错；
- 全异步架构；
- 连接、会话、路由、集群的分层设计；
- 消息平面和控制平面的分离等。

在精心设计和实现之后，单个 EMQX Enterprise 节点就可以处理百万级的连接。

EMQX 支持多节点集群，集群下整个系统的性能会成倍高于单节点，并能在单节点故障时保证系统服务不中断。

## EMQX 能把接入的 MQTT 消息保存到数据库吗？

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

可以的。EMQX 提供的 HTTP API 中包含断开 MQTT 连接，该操作在 EMQX 2.x 和 3.0 的实现方式有所不同：

- 在 2.x 版本中是由 EMQX 自定义扩展协议实现的
- 在 3.0 版本之后按照 MQTT 5.0 协议对从服务器端断开连接的规范要求实现的

调用的 API 如下所示：

```html
HTTP 方法：DELETE URL：api/[v2|v3]/clients/{clientid}
<!--请注意区分 URL 中第二部分的版本号，请根据使用的版本号来决定 -->

返回内容： { "code": 0, "result": [] }
```

HTTP API 使用方式参考 [管理监控 API (HTTP API)](https://docs.emqx.io/broker/v3/cn/rest.html)

## EMQX 能把接入的消息转发到 Kafka 吗？

能。目前 EMQX 企业版提供了内置的 Kafka 桥接方式，支持把消息桥接至 Kafka 进行流式处理。

EMQX 使用 Kafka 参照 [EMQX 到 Kafka 的桥接](https://docs.emqx.io/tutorial/v3/cn/bridge/bridge_to_kafka.html)

## EMQX 企业版中桥接 Kafka，一条 MQTT 消息到达 EMQX 集群之后就回 MQTT Ack 报文还是写入 Kafka 之后才回 MQTT Ack 报文?

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

EMQX 支持转发消息到其他消息中间件，通过 EMQX 提供的桥接方式就可以做基于主题级别的配置，从而实现主题级别的消息转发。

EMQX 桥接相关的使用方式请参照 [EMQX 桥接](https://docs.emqx.io/tutorial/v3/cn/bridge/bridge.html)

## 我可以把消息从 EMQX 转到公有云 MQTT 服务上吗？比如 AWS 或者 Azure 的 IoT Hub？

EMQX 可以转发消息到标准 MQTT Broker，包括其他 MQTT 实现、公有云的 IoT Hub，通过 EMQX 提供的桥接就可以实现。

## MQTT Broker（比如 Mosquitto）可以转发消息到 EMQX 吗？

Mosquitto 可以配置转发消息到 EMQX，请参考[数据桥接 - MQTT](../data-integration/data-bridge-mqtt.md)。

## 我想跟踪特定消息的发布和订阅过程，应该如何做？

EMQX 支持追踪来自某个客户端的报文或者发布到某个主题的报文。追踪消息的发布和订阅需要使用命令行工具（emqx_ctl）的 trace 命令，下面给出一个追踪‘topic’主题的消息并保存在 `trace_topic.log` 中的例子。更详细的说明请参阅 EMQX 文档的相关章节。

```bash
./bin/emqx_ctl trace topic "topic" "trace_topic.log"
```

## 为什么我做压力测试的时候，连接数目和吞吐量老是上不去，有系统调优指南吗？

在做压力测试的时候，除了要选用有足够计算能力的硬件，也需要对软件运行环境做一定的调优。比如修改修改操作系统的全局最大文件句柄数，允许用户打开的文件句柄数，TCP 的 backlog 和 buffer，Erlang 虚拟机的进程数限制等等。甚至包括需要在客户端上做一定的调优以保证客户端可以有足够的连接资源。

系统的调优在不同的需求下有不同的方式，在 EMQX 的[文档-测试调优](https://developer.emqx.io/docs/broker/v3/cn/tune.html) 中对用于普通场景的调优有较详细的说明

## EMQX 支持加密连接吗？推荐的部署方案是什么？

EMQX 支持加密连接。在生产环境部署时，推荐的方案是使用负载均衡终结 TLS。通过该方式，设备端和服务器端（负载均衡）的采用加密的连接，而负载均衡和后端的 EMQX 节点采用一般的 TCP 连接。

## EMQX 安装之后无法启动怎么排查？

执行 `$ emqx console` ，查看输出内容

- `logger` 命令缺失

```
$ emqx console
Exec: /usr/lib/emqx/erts-10.3.5.1/bin/erlexec -boot /usr/lib/emqx/releases/v3.2.1/emqx -mode embedded -boot_var ERTS_LIB_DIR /usr/lib/emqx/erts-10.3.5.1/../lib -mnesia dir "/var/lib/emqx/mnesia/emqx@127.0.0.1" -config /var/lib/emqx/configs/app.2019.07.23.03.07.32.config -args_file /var/lib/emqx/configs/vm.2019.07.23.03.07.32.args -vm_args /var/lib/emqx/configs/vm.2019.07.23.03.07.32.args -- console
Root: /usr/lib/emqx
/usr/lib/emqx
/usr/bin/emqx: line 510: logger: command not found
```

**解决办法：**

- `Centos/Redhat`

  ```
  $ yum install rsyslog
  ```

- `Ubuntu/Debian`

  ```
  $ apt-get install bsdutils
  ```

- `openssl` 缺失

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
安装 1.1.1 以上版本的 `openssl`

- `License` 文件缺失

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
登陆[emqx.io](https://emqx.io)申请 license 或安装开源版的 EMQX Broker

## EMQX 中 ssl resumption session 的使用

修改 emqx.conf 配置中的 reuse_sessions = on 并生效后。如果客户端与服务端通过 SSL 已经连接成功，当第二次遇到客户端连接时，会跳过 SSL 握手阶段，直接建立连接，节省连接时间，增加客户端连接速度。

## 如何进行 MQTT 客户端断开连接统计

执行 `emqx_ctl listeners`，查看对应端口下的 `shutdown_count` 统计。

客户端断开链接错误码列表：

- `keepalive_timeout`：MQTT keepalive 超时
- `closed`：TCP 客户端断开连接（客户端发来的 FIN，但没收到 MQTT DISCONNECT）
- `normal`：MQTT 客户端正常断开
- `einval`：EMQX 想向客户端发送一条消息，但是 Socket 已经断开
- `function_clause`：MQTT 报文格式错误
- `etimedout`：TCP 发送超时（没有收到 TCP ACK 回应）
- `proto_unexpected_c`：在已经有一条 MQTT 连接的情况下重复收到了 MQTT 连接请求
- `idle_timeout`： TCP 连接建立 15s 之后，还没收到 connect 报文

## EMQX 推荐部署的操作系统是什么？

EMQX 支持跨平台部署在 Linux、Windows、MacOS、ARM 嵌入系统，生产系推荐在 CentOS、Ubuntu、Debian 等 Linux 发行版上部署。

## EMQX 支持 Windows 操作系统吗？

仅在开源版支持，推荐用于测试而非生产部署，参考 [Windows 安装 EMQX](../deploy/install-windows.md)。

## EMQX 如何预估资源的使用？

EMQX 对资源的使用主要有以下的影响因素，每个因素都会对计算和存储资源的使用产生影响：

- 连接数：对于每一个 MQTT 长连接，EMQX 会创建两个 Erlang 进程，每个进程都会耗费一定的资源。连接数越高，所需的资源越多；

- 平均吞吐量：指的是每秒 Pub 和 Sub 的消息数量。吞吐量越高，EMQX 的路由处理和消息转发处理就需要更多的资源；

- 消息体大小：消息体越大，在 EMQX 中处理消息转发的时候在内存中进行数据存储和处理，所需的资源就越多；

- 主题数目：如果主题数越多，在 EMQX 中的路由表会相应增长，因此所需的资源就越多；

- QoS：消息的 QoS 越高，EMQX 服务器端所处理的逻辑会更多，因此会耗费更多的资源；

另外，如果设备通过 TLS（加密的连接）连接 EMQX，EMQX 会需要额外的资源（主要是 CPU 资源）。推荐方案是在 EMQX 前面部署负载均衡，由负载均衡节点卸载 TLS，实现职责分离。

可参考 [https://www.emqx.com/zh/server-estimate](https://www.emqx.com/zh/server-estimate) 来预估计算资源的使用。

## EMQX 的百万连接压力测试的场景是什么？

在 EMQ 2.0 版本发布的时候，由第三方软件测试工具服务提供商 [XMeter](https://www.xmeter.net) 执行了一次百万级别连接的性能测试。测试基于开源社区中最流行的性能测试工具 [Apache JMeter](https://jmeter.apache.org/)，以及开源[性能测试插件](https://github.com/emqx/mqtt-jmeter)。该性能测试场景为测试客户端到服务器端的 MQTT 协议连接，该测试场景下除了发送 MQTT 协议的控制包和 PING 包（每 5 分钟发送一次）外，不发送用户数据，每秒新增连接数为 1000，共计运行 30 分钟。

在该测试中，还执行了一些别的性能测试，主要为在为 10 万 MQTT 背景连接的情况下，执行了不同条件下的消息发送和接收的场景。具体请参见[性能测试报告](https://media.readthedocs.org/pdf/emq-xmeter-benchmark-cn/latest/emq-xmeter-benchmark-cn.pdf).

## 我的连接数目并不大，EMQX 生产环境部署需要多节点吗？

即使在连接数量，消息率不高的情况下（服务器低负载），在生产环境下部署多节点的集群依然是很有意义的。集群能提高系统的可用性，降低单点故障的可能性。当一个节点宕机时，其他在线节点可以保证整个系统的服务不中断。
