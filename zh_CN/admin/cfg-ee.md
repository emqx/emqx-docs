# 配置手册

EMQX 配置文件手册。

## 节点设置

设置节点名称以及 Cookie。每个 Erlang 节点(进程)需指配一个节点名，用于节点间通信互访。 所有互相通信的 Erlang 节点(进程)间通过一个共用的 Cookie 进行安全认证。

**node.name**

  *类型*: `string`

  *默认值*: `emqx@127.0.0.1`

  节点名。格式为 \<name>@\<host>。其中 <host> 可以是 IP 地址，也可以是 FQDN。
详见 http://erlang.org/doc/reference_manual/distributed.html。


**node.cookie**

  *类型*: `string`

  分布式 Erlang 集群使用的 cookie 值。集群间保持一致


**node.process_limit**

  *类型*: `integer`

  *默认值*: `2097152`

  *可选值*: `1024-134217727`

  Erlang系统同时存在的最大进程数。
实际选择的最大值可能比设置的数字大得多。
参考: https://www.erlang.org/doc/man/erl.html


**node.max_ports**

  *类型*: `integer`

  *默认值*: `1048576`

  *可选值*: `1024-134217727`

  Erlang系统同时存在的最大端口数。
实际选择的最大值可能比设置的数字大得多。
参考: https://www.erlang.org/doc/man/erl.html


**node.dist_buffer_size**

  *类型*: `integer`

  *默认值*: `8192`

  *可选值*: `1-2097151`

  Erlang分布式缓冲区的繁忙阈值，单位是KB。


**node.max_ets_tables**

  *类型*: `pos_integer`

  *默认值*: `262144`

  Erlang ETS 表的最大数量


**node.data_dir**

  *类型*: `string`

  节点数据存放目录，可能会自动创建的子目录如下：<br/>
- `mnesia/<node_name>`。EMQX的内置数据库目录。例如，`mnesia/emqx@127.0.0.1`。<br/>
如果节点要被重新命名（例如，`emqx@10.0.1.1`）。旧目录应该首先被删除。<br/>
- `configs`。在启动时生成的配置，以及集群/本地覆盖的配置。<br/>
- `patches`: 热补丁文件将被放在这里。<br/>
- `trace`: 日志跟踪文件。<br/>

**注意**: 一个数据dir不能被两个或更多的EMQX节点同时使用。


**node.global_gc_interval**

  *类型*: `disabled | duration`

  *默认值*: `15m`

  系统调优参数，设置节点运行多久强制进行一次全局垃圾回收。禁用设置为 <code>disabled</code>。


**node.crash_dump_file**

  *类型*: `file`

  *默认值*: `log/erl_crash.dump`

  设置 Erlang crash_dump 文件的存储路径和文件名。


**node.crash_dump_seconds**

  *类型*: `duration_s`

  *默认值*: `30s`

  该配置给出了运行时系统允许花费的写入崩溃转储的秒数。当给定的秒数已经过去，运行时系统将被终止。<br/>
- 如果设置为0秒，运行时会立即终止，不会尝试写入崩溃转储文件。<br/>
- 如果设置为一个正数 S，节点会等待 S 秒来完成崩溃转储文件，然后用SIGALRM信号终止运行时系统。<br/>
- 如果设置为一个负值导致运行时系统的终止等待无限期地直到崩溃转储文件已经完全写入。


**node.crash_dump_bytes**

  *类型*: `bytesize`

  *默认值*: `100MB`

  限制崩溃文件的大小，当崩溃时节点内存太大，
如果为了保存现场，需要全部存到崩溃文件中，此处限制最多能保存多大的文件。
如果超过此限制，崩溃转储将被截断。如果设置为0，系统不会尝试写入崩溃转储文件。


**node.dist_net_ticktime**

  *类型*: `duration_s`

  *默认值*: `2m`

  系统调优参数，此配置将覆盖 vm.args 文件里的 -kernel net_ticktime 参数。当一个节点持续无响应多久之后，认为其已经宕机并断开连接。


**node.backtrace_depth**

  *类型*: `integer`

  *默认值*: `23`

  错误信息中打印的最大堆栈层数


**node.applications**

  *类型*: `comma_separated_atoms`

  *默认值*: `[]`

  当新EMQX 加入集群时，应重启的Erlang应用程序的列表。


**node.etc_dir**

  *类型*: `string`

  Deprecated since 5.0.8.


**node.cluster_call**

  *类型*: `cluster_call`


**node.db_backend**

  *类型*: `enum`

  *默认值*: `rlog`

  *可选值*: `mnesia | rlog`

  配置后端数据库驱动，默认值为 <code>rlog</code> 它适用于大规模的集群。
<code>mnesia</code> 是备选数据库，适合中小集群。


**node.db_role**

  *类型*: `enum`

  *默认值*: `core`

  *可选值*: `core | replicant`

  选择节点的角色。<br/>
<code>core</code> 节点提供数据的持久性，并负责写入。建议将核心节点放置在不同的机架或不同的可用区。<br/>
<code>repliant</code> 节点是临时工作节点。 从集群中删除它们，不影响数据库冗余<br/>
建议复制节点多于核心节点。<br/>
注意：该参数仅在设置<code>backend</code>时生效到 <code>rlog</code>。


**node.rpc_module**

  *类型*: `enum`

  *默认值*: `gen_rpc`

  *可选值*: `gen_rpc | rpc`

  集群间推送事务日志到复制节点使用的协议。


**node.tlog_push_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  同步模式下，核心节点等待复制节点的确认信息，然后再发送下一条事务日志。



## RPC 设置


EMQX 使用 <code>gen_rpc</code> 库来实现跨节点通信。<br/>
大多数情况下，默认的配置应该可以工作，但如果你需要做一些性能优化或者实验，可以尝试调整这些参数。

**rpc.mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  在 <code>sync</code> 模式下，发送端等待接收端的 ack信号。


**rpc.driver**

  *类型*: `enum`

  *默认值*: `tcp`

  *可选值*: `tcp | ssl`

  集群间通信使用的传输协议。


**rpc.async_batch_size**

  *类型*: `integer`

  *默认值*: `256`

  异步模式下，发送的批量消息的最大数量。


**rpc.port_discovery**

  *类型*: `enum`

  *默认值*: `stateless`

  *可选值*: `manual | stateless`

  <code>manual</code>: 通过 <code>tcp_server_port</code> 来发现端口。
<br/><code>stateless</code>: 使用无状态的方式来发现端口，使用如下算法。如果节点名称是 <code>
emqxN@127.0.0.1</code>, N 是一个数字，那么监听端口就是 5370 + N。


**rpc.tcp_server_port**

  *类型*: `integer`

  *默认值*: `5369`

  RPC 本地服务使用的 TCP 端口。<br/>
只有当 rpc.port_discovery 设置为 manual 时，此配置才会生效。


**rpc.ssl_server_port**

  *类型*: `integer`

  *默认值*: `5369`

  RPC 本地服务使用的监听SSL端口。<br/>
只有当 rpc.port_discovery 设置为 manual 且 <code> dirver </code> 设置为 <code>ssl</code>，
此配置才会生效。


**rpc.tcp_client_num**

  *类型*: `integer`

  *默认值*: `10`

  *可选值*: `1-256`

  设置本节点与远程节点之间的 RPC 通信通道的最大数量。


**rpc.connect_timeout**

  *类型*: `duration`

  *默认值*: `5s`

  建立 RPC 连接的超时时间。


**rpc.certfile**

  *类型*: `file`

  TLS 证书文件的路径，用于验证集群节点的身份。
只有当 <code>rpc.driver</code> 设置为 <code>ssl</code> 时，此配置才会生效。


**rpc.keyfile**

  *类型*: `file`

  <code>rpc.certfile</code> 的私钥文件的路径。<br/>
注意：此文件内容是私钥，所以需要设置权限为 600。


**rpc.cacertfile**

  *类型*: `file`

  验证 <code>rpc.certfile</code> 的 CA 证书文件的路径。<br/>
注意：集群中所有节点的证书必须使用同一个 CA 签发。


**rpc.send_timeout**

  *类型*: `duration`

  *默认值*: `5s`

  发送 RPC 请求的超时时间。


**rpc.authentication_timeout**

  *类型*: `duration`

  *默认值*: `5s`

  远程节点认证的超时时间。


**rpc.call_receive_timeout**

  *类型*: `duration`

  *默认值*: `15s`

  同步 RPC 的回复超时时间。


**rpc.socket_keepalive_idle**

  *类型*: `duration_s`

  *默认值*: `15m`

  broker 之间的连接在最后一条消息发送后保持打开的时间。


**rpc.socket_keepalive_interval**

  *类型*: `duration_s`

  *默认值*: `75s`

  keepalive 消息的间隔。


**rpc.socket_keepalive_count**

  *类型*: `integer`

  *默认值*: `9`

  keepalive 探测消息发送失败的次数，直到 RPC 连接被认为已经断开。


**rpc.socket_sndbuf**

  *类型*: `bytesize`

  *默认值*: `1MB`

  TCP 调节参数。TCP 发送缓冲区大小。


**rpc.socket_recbuf**

  *类型*: `bytesize`

  *默认值*: `1MB`

  TCP 调节参数。TCP 接收缓冲区大小。


**rpc.socket_buffer**

  *类型*: `bytesize`

  *默认值*: `1MB`

  TCP 调节参数。用户模式套接字缓冲区大小。


**rpc.insecure_fallback**

  *类型*: `boolean`

  *默认值*: `true`

  兼容旧的无鉴权模式



## 集群设置


EMQX 节点可以组成一个集群，以提高总容量。<br/> 这里指定了节点之间如何连接。

**cluster.name**

  *类型*: `atom`

  *默认值*: `emqxcl`

  EMQX集群名称。每个集群都有一个唯一的名称。服务发现时会用于做路径的一部分。


**cluster.discovery_strategy**

  *类型*: `enum`

  *默认值*: `manual`

  *可选值*: `manual | static | mcast | dns | etcd | k8s`

  集群节点发现方式。可选值为:
- manual: 使用 <code>emqx ctl cluster</code> 命令管理集群。<br/>
- static: 配置静态节点。配置几个固定的节点，新节点通过连接固定节点中的某一个来加入集群。<br/>
- dns: 使用 DNS A 记录的方式发现节点。<br/>
- etcd: 使用 etcd 发现节点。<br/>
- k8s: 使用 Kubernetes API 发现节点。


**cluster.core_nodes**

  *类型*: `comma_separated_atoms`

  *默认值*: `[]`

  当前节点连接的核心节点列表。<br/>
注意：该参数仅在设置<code>backend</code>时生效到 <code>rlog</code>
并且设置<code>role</code>为<code>replicant</code>时生效。<br/>
该值需要在手动或静态集群发现机制下设置。<br/>
如果使用了自动集群发现机制（如<code>etcd</code>），则不需要设置该值。


**cluster.autoclean**

  *类型*: `duration`

  *默认值*: `5m`

  指定多久之后从集群中删除离线节点。


**cluster.autoheal**

  *类型*: `boolean`

  *默认值*: `true`

  集群脑裂自动恢复机制开关。


**cluster.proto_dist**

  *类型*: `enum`

  *默认值*: `inet_tcp`

  *可选值*: `inet_tcp | inet6_tcp | inet_tls`

  分布式 Erlang 集群协议类型。可选值为:<br/>
- inet_tcp: 使用 IPv4 <br/>
- inet_tls: 使用 TLS，需要配合 <code>etc/ssl_dist.conf</code> 一起使用。


**cluster.static**

  *类型*: `cluster_static`


**cluster.mcast**

  *类型*: `cluster_mcast`


**cluster.dns**

  *类型*: `cluster_dns`


**cluster.etcd**

  *类型*: `cluster_etcd`


**cluster.k8s**

  *类型*: `cluster_k8s`



## 集群自动发现

EMQX 支持多种策略的节点自动发现与集群，详见 [创建集群](../deploy/cluster/create-cluster.md)。

| 策略   | 说明                    |
| ------ | ----------------------- |
| manual | 手工命令创建集群        |
| static | 静态节点列表自动集群    |
| mcast  | UDP 组播方式自动集群    |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| k8s    | Kubernetes 服务自动集群 |

### manual 手动创建集群

默认配置为手动创建集群，节点通过 `./bin/emqx_ctl join <Node>` 命令加入:

```bash
cluster.discovery = manual
```

### 基于 static 节点列表自动集群


静态节点服务发现。新节点通过连接一个节点来加入集群。

**cluster.static.seeds**

  *类型*: `array`

  *默认值*: `[]`

  集群中的EMQX节点名称列表，
指定固定的节点列表，多个节点间使用逗号 , 分隔。
当 cluster.discovery_strategy 为 static 时，此配置项才有效。
适合于节点数量较少且固定的集群。



### 基于 mcast 组播自动集群


UDP 组播服务发现。

**cluster.mcast.addr**

  *类型*: `string`

  *默认值*: `239.192.0.1`

  指定多播 IPv4 地址。
当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。


**cluster.mcast.ports**

  *类型*: `array`

  *默认值*: `ᄑᄒ`

  指定多播端口。如有多个端口使用逗号 , 分隔。
当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。


**cluster.mcast.iface**

  *类型*: `string`

  *默认值*: `0.0.0.0`

  指定节点发现服务需要绑定到本地 IP 地址。
当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。


**cluster.mcast.ttl**

  *类型*: `integer`

  *默认值*: `255`

  *可选值*: `0-255`

  指定多播的 Time-To-Live 值。
当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。


**cluster.mcast.loop**

  *类型*: `boolean`

  *默认值*: `true`

  设置多播的报文是否投递到本地回环地址。
当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。


**cluster.mcast.sndbuf**

  *类型*: `bytesize`

  *默认值*: `16KB`

  外发数据报的内核级缓冲区的大小。
当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。


**cluster.mcast.recbuf**

  *类型*: `bytesize`

  *默认值*: `16KB`

  接收数据报的内核级缓冲区的大小。
当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。


**cluster.mcast.buffer**

  *类型*: `bytesize`

  *默认值*: `32KB`

  用户级缓冲区的大小。
当 cluster.discovery_strategy 为 mcast 时，此配置项才有效。



### 基于 DNS 记录自动集群


DNS SRV 记录服务发现。

**cluster.dns.name**

  *类型*: `string`

  *默认值*: `localhost`

  指定 DNS A 记录的名字。emqx 会通过访问这个 DNS A 记录来获取 IP 地址列表。
当<code>cluster.discovery_strategy</code> 为 <code>dns</code> 时有效。


**cluster.dns.record_type**

  *类型*: `enum`

  *默认值*: `a`

  *可选值*: `a | srv`

  DNS 记录类型。



### 基于 etcd 自动集群


使用 'etcd' 服务的服务发现。

**cluster.etcd.server**

  *类型*: `comma_separated_list`

  指定 etcd 服务的地址。如有多个服务使用逗号 , 分隔。
当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。


**cluster.etcd.prefix**

  *类型*: `string`

  *默认值*: `emqxcl`

  指定 etcd 路径的前缀。每个节点在 etcd 中都会创建一个路径:
v2/keys/<prefix>/<cluster.name>/<node.name> <br/>
当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。


**cluster.etcd.node_ttl**

  *类型*: `duration`

  *默认值*: `1m`

  指定 etcd 中节点信息的过期时间。
当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。


**cluster.etcd.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  当使用 TLS 连接 etcd 时的配置选项。
当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。



### 基于 Kubernetes 自动集群


Kubernetes 服务发现。

**cluster.k8s.apiserver**

  *类型*: `string`

  *默认值*: `http://10.110.111.204:8080`

  指定 Kubernetes API Server。如有多个 Server 使用逗号 , 分隔。
当 cluster.discovery_strategy 为 k8s 时，此配置项才有效。


**cluster.k8s.service_name**

  *类型*: `string`

  *默认值*: `emqx`

  指定 Kubernetes 中 EMQX 的服务名。
当 cluster.discovery_strategy 为 k8s 时，此配置项才有效。


**cluster.k8s.address_type**

  *类型*: `enum`

  *默认值*: `ip`

  *可选值*: `ip | dns | hostname`

  当使用 k8s 方式集群时，address_type 用来从 Kubernetes 接口的应答里获取什么形式的 Host 列表。
指定 <code>cluster.k8s.address_type</code> 为 <code>ip</code>，则将从 Kubernetes 接口中获取集群中其他节点
的IP地址。


**cluster.k8s.namespace**

  *类型*: `string`

  *默认值*: `default`

  当使用 k8s 方式并且 cluster.k8s.address_type 指定为 dns 类型时，
可设置 emqx 节点名的命名空间。与 cluster.k8s.suffix 一起使用用以拼接得到节点名列表。


**cluster.k8s.suffix**

  *类型*: `string`

  *默认值*: `pod.local`

  当使用 k8s 方式并且 cluster.k8s.address_type 指定为 dns 类型时，可设置 emqx 节点名的后缀。
与 cluster.k8s.namespace 一起使用用以拼接得到节点名列表。



### 集群调用设置


集群调用功能的选项。

**node.cluster_call.retry_interval**

  *类型*: `duration`

  *默认值*: `1m`

  当集群间调用出错时，多长时间重试一次。


**node.cluster_call.max_history**

  *类型*: `integer`

  *默认值*: `100`

  *可选值*: `1-500`

  集群间调用最多保留的历史记录数。只用于排错时查看。


**node.cluster_call.cleanup_interval**

  *类型*: `duration`

  *默认值*: `5m`

  清理过期事务的时间间隔



## 日志参数

配置日志输出位置、日志级别、日志文件存储路径以及日志轮换、过载保护等参数。

### 文件输出日志


日志处理进程将日志事件打印到文件。

**log.file_handlers.$name.file**

  *类型*: `file`

  日志文件路径及名字。


**log.file_handlers.$name.rotation**

  *类型*: `log_rotation`


**log.file_handlers.$name.max_size**

  *类型*: `infinity | bytesize`

  *默认值*: `50MB`

  此参数控制日志文件轮换。 `infinity` 意味着日志文件将无限增长，否则日志文件将在达到 `max_size`（以字节为单位）时进行轮换。
与 rotation count配合使用。如果 counter 为 10，则是10个文件轮换。


**log.file_handlers.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用此日志处理进程。


**log.file_handlers.$name.level**

  *类型*: `log_level`

  *默认值*: `warning`

  当前日志处理进程的日志级别。
默认为 warning 级别。


**log.file_handlers.$name.time_offset**

  *类型*: `string`

  *默认值*: `system`

  日志中的时间戳使用的时间偏移量。
可选值为：
  - <code>system</code>: 本地系统使用的时区偏移量
  - <code>utc</code>: 0 时区的偏移量
  - <code>+-[hh]:[mm]</code>: 自定义偏移量，比如 "-02:00" 或者 "+00:00"
默认值为本地系统的时区偏移量：<code>system</code>。


**log.file_handlers.$name.chars_limit**

  *类型*: `unlimited | 100..inf`

  *默认值*: `unlimited`

  设置单个日志消息的最大长度。 如果超过此长度，则日志消息将被截断。最小可设置的长度为100。
注意：如果日志格式为 JSON，限制字符长度可能会导致截断不完整的 JSON 数据。


**log.file_handlers.$name.formatter**

  *类型*: `enum`

  *默认值*: `text`

  *可选值*: `text | json`

  选择日志格式类型。 <code>text</code> 用于纯文本，<code>json</code> 用于结构化日志记录。


**log.file_handlers.$name.single_line**

  *类型*: `boolean`

  *默认值*: `true`

  如果设置为 true，则单行打印日志。 否则，日志消息可能跨越多行。


**log.file_handlers.$name.sync_mode_qlen**

  *类型*: `non_neg_integer`

  *默认值*: `100`

  只要缓冲的日志事件的数量低于这个值，所有的日志事件都会被异步处理。
这意味着，日志落地速度不会影响正常的业务进程，因为它们不需要等待日志处理进程的响应。
如果消息队列的增长超过了这个值，处理程序开始同步处理日志事件。也就是说，发送事件的客户进程必须等待响应。
当处理程序将消息队列减少到低于sync_mode_qlen阈值的水平时，异步操作就会恢复。
默认为100条信息，当等待的日志事件大于100条时，就开始同步处理日志。


**log.file_handlers.$name.drop_mode_qlen**

  *类型*: `pos_integer`

  *默认值*: `3000`

  当缓冲的日志事件数大于此值时，新的日志事件将被丢弃。起到过载保护的功能。
为了使过载保护算法正常工作必须要：<code> sync_mode_qlen =< drop_mode_qlen =< flush_qlen </code> 且 drop_mode_qlen > 1
要禁用某些模式，请执行以下操作。
- 如果sync_mode_qlen被设置为0，所有的日志事件都被同步处理。也就是说，异步日志被禁用。
- 如果sync_mode_qlen被设置为与drop_mode_qlen相同的值，同步模式被禁用。也就是说，处理程序总是以异步模式运行，除非调用drop或flushing。
- 如果drop_mode_qlen被设置为与flush_qlen相同的值，则drop模式被禁用，永远不会发生。


**log.file_handlers.$name.flush_qlen**

  *类型*: `pos_integer`

  *默认值*: `8000`

  如果缓冲日志事件的数量增长大于此阈值，则会发生冲刷（删除）操作。 日志处理进程会丢弃缓冲的日志消息。
来缓解自身不会由于内存瀑涨而影响其它业务进程。日志内容会提醒有多少事件被删除。


**log.file_handlers.$name.overload_kill**

  *类型*: `log_overload_kill`


**log.file_handlers.$name.burst_limit**

  *类型*: `log_burst_limit`


**log.file_handlers.$name.supervisor_reports**

  *类型*: `enum`

  *默认值*: `error`

  *可选值*: `error | progress`

  Supervisor 报告的类型。默认为 error 类型。<br/>
  - <code>error</code>：仅记录 Erlang 进程中的错误。
  - <code>progress</code>：除了 error 信息外，还需要记录进程启动的详细信息。


**log.file_handlers.$name.max_depth**

  *类型*: `unlimited | non_neg_integer`

  *默认值*: `100`

  Erlang 内部格式日志格式化和 Erlang 进程消息队列检查的最大深度。



### Console 输出日志


日志处理进程将日志事件打印到 EMQX 控制台。

**log.console_handler.enable**

  *类型*: `boolean`

  *默认值*: `false`

  启用此日志处理进程。


**log.console_handler.level**

  *类型*: `log_level`

  *默认值*: `warning`

  当前日志处理进程的日志级别。
默认为 warning 级别。


**log.console_handler.time_offset**

  *类型*: `string`

  *默认值*: `system`

  日志中的时间戳使用的时间偏移量。
可选值为：
  - <code>system</code>: 本地系统使用的时区偏移量
  - <code>utc</code>: 0 时区的偏移量
  - <code>+-[hh]:[mm]</code>: 自定义偏移量，比如 "-02:00" 或者 "+00:00"
默认值为本地系统的时区偏移量：<code>system</code>。


**log.console_handler.chars_limit**

  *类型*: `unlimited | 100..inf`

  *默认值*: `unlimited`

  设置单个日志消息的最大长度。 如果超过此长度，则日志消息将被截断。最小可设置的长度为100。
注意：如果日志格式为 JSON，限制字符长度可能会导致截断不完整的 JSON 数据。


**log.console_handler.formatter**

  *类型*: `enum`

  *默认值*: `text`

  *可选值*: `text | json`

  选择日志格式类型。 <code>text</code> 用于纯文本，<code>json</code> 用于结构化日志记录。


**log.console_handler.single_line**

  *类型*: `boolean`

  *默认值*: `true`

  如果设置为 true，则单行打印日志。 否则，日志消息可能跨越多行。


**log.console_handler.sync_mode_qlen**

  *类型*: `non_neg_integer`

  *默认值*: `100`

  只要缓冲的日志事件的数量低于这个值，所有的日志事件都会被异步处理。
这意味着，日志落地速度不会影响正常的业务进程，因为它们不需要等待日志处理进程的响应。
如果消息队列的增长超过了这个值，处理程序开始同步处理日志事件。也就是说，发送事件的客户进程必须等待响应。
当处理程序将消息队列减少到低于sync_mode_qlen阈值的水平时，异步操作就会恢复。
默认为100条信息，当等待的日志事件大于100条时，就开始同步处理日志。


**log.console_handler.drop_mode_qlen**

  *类型*: `pos_integer`

  *默认值*: `3000`

  当缓冲的日志事件数大于此值时，新的日志事件将被丢弃。起到过载保护的功能。
为了使过载保护算法正常工作必须要：<code> sync_mode_qlen =< drop_mode_qlen =< flush_qlen </code> 且 drop_mode_qlen > 1
要禁用某些模式，请执行以下操作。
- 如果sync_mode_qlen被设置为0，所有的日志事件都被同步处理。也就是说，异步日志被禁用。
- 如果sync_mode_qlen被设置为与drop_mode_qlen相同的值，同步模式被禁用。也就是说，处理程序总是以异步模式运行，除非调用drop或flushing。
- 如果drop_mode_qlen被设置为与flush_qlen相同的值，则drop模式被禁用，永远不会发生。


**log.console_handler.flush_qlen**

  *类型*: `pos_integer`

  *默认值*: `8000`

  如果缓冲日志事件的数量增长大于此阈值，则会发生冲刷（删除）操作。 日志处理进程会丢弃缓冲的日志消息。
来缓解自身不会由于内存瀑涨而影响其它业务进程。日志内容会提醒有多少事件被删除。


**log.console_handler.overload_kill**

  *类型*: `log_overload_kill`


**log.console_handler.burst_limit**

  *类型*: `log_burst_limit`


**log.console_handler.supervisor_reports**

  *类型*: `enum`

  *默认值*: `error`

  *可选值*: `error | progress`

  Supervisor 报告的类型。默认为 error 类型。<br/>
  - <code>error</code>：仅记录 Erlang 进程中的错误。
  - <code>progress</code>：除了 error 信息外，还需要记录进程启动的详细信息。


**log.console_handler.max_depth**

  *类型*: `unlimited | non_neg_integer`

  *默认值*: `100`

  Erlang 内部格式日志格式化和 Erlang 进程消息队列检查的最大深度。



### 日志轮换


默认情况下，日志存储在 `./log` 目录（用于从 zip 文件安装）或 `/var/log/emqx`（用于二进制安装）。<br/>
这部分配置，控制每个日志处理进程保留的文件数量。

**log.file_handlers.$name.rotation.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用日志轮换功能。启动后生成日志文件后缀会加上对应的索引数字，比如：log/emqx.log.1。
系统会默认生成<code>*.siz/*.idx</code>用于记录日志位置，请不要手动修改这两个文件。


**log.file_handlers.$name.rotation.count**

  *类型*: `integer`

  *默认值*: `10`

  *可选值*: `1-2048`

  轮换的最大日志文件数。



### 日志突发限制


短时间内产生的大量日志事件可能会导致问题，例如：
  - 日志文件变得非常大
  - 日志文件轮换过快，有用信息被覆盖
  - 对系统的整体性能影响

日志突发限制功能可以暂时禁用日志记录以避免这些问题。

**log_burst_limit.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用日志限流保护机制。


**log_burst_limit.max_count**

  *类型*: `pos_integer`

  *默认值*: `10000`

  在 `window_time` 间隔内处理的最大日志事件数。 达到限制后，将丢弃连续事件，直到 `window_time` 结束。


**log_burst_limit.window_time**

  *类型*: `duration`

  *默认值*: `1s`

  参考 <code>max_count</code>。



### 日志过载终止


日志过载终止，具有过载保护功能。当日志处理进程使用过多内存，或者缓存的日志消息过多时该功能被激活。<br/>
检测到过载时，日志处理进程将终止，并在冷却期后重新启动。

**log_overload_kill.enable**

  *类型*: `boolean`

  *默认值*: `true`

  日志处理进程过载时为保护自己节点其它的业务能正常，强制杀死日志处理进程。


**log_overload_kill.mem_size**

  *类型*: `bytesize`

  *默认值*: `30MB`

  日志处理进程允许使用的最大内存。


**log_overload_kill.qlen**

  *类型*: `pos_integer`

  *默认值*: `20000`

  允许的最大队列长度。


**log_overload_kill.restart_after**

  *类型*: `duration_ms | infinity`

  *默认值*: `5s`

  如果处理进程终止，它会在以指定的时间后后自动重新启动。 `infinity` 不自动重启。



## MQTT/TCP 监听器 - 1883

EMQX 支持配置多个监听器，默认 MQTT/TCP 监听器端口为 `1883`。

**listeners.tcp.$name.enabled**

  *类型*: `boolean`

  *默认值*: `true`

  启停监听器。


**listeners.tcp.$name.bind**

  *类型*: `ip_port | integer`

  *默认值*: `1883`

  监听套接字的 IP 地址和端口。


**listeners.tcp.$name.acceptors**

  *类型*: `pos_integer`

  *默认值*: `16`

  监听器接收池的大小。


**listeners.tcp.$name.max_connections**

  *类型*: `infinity | pos_integer`

  *默认值*: `infinity`

  监听器允许的最大并发连接数。


**listeners.tcp.$name.mountpoint**

  *类型*: `string`

  *默认值*: `""`

  发布或订阅时，请在所有主题前面加上 mountpoint 字符串。

将消息传递给订阅者时，将从主题名称中删除带前缀的字符串。挂载点是一种用户可以用来实现不同侦听器之间消息路由隔离的方法。

例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。<br/>
类似地，如果另一个客户端B（与客户端A连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息<br/>

设置为<code>""</code> 以禁用该功能<br/>

mountpoint 字符串中的变量：
- <code>${clientid}</code>: clientid
- <code>${username}</code>: username


**listeners.tcp.$name.zone**

  *类型*: `atom`

  *默认值*: `default`

  监听器所属的配置组。


**listeners.tcp.$name.limiter**

  *类型*: `limiter:listener_fields`

  速率限制类型


**listeners.tcp.$name.enable_authn**

  *类型*: `enum`

  *默认值*: `true`

  *可选值*: `true | false | quick_deny_anonymous`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
客户直接拒绝，不做使用任何认证器对客户端进行身份检查。


**listeners.tcp.$name.access_rules**

  *类型*: `array`

  *默认值*: `["allow all"]`

  此监听器的访问控制规则。


**listeners.tcp.$name.proxy_protocol**

  *类型*: `boolean`

  *默认值*: `false`

  如果EMQX集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 <br/>
详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.tcp.$name.proxy_protocol_timeout**

  *类型*: `duration`

  *默认值*: `3s`

  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX将关闭TCP连接。


**listeners.tcp.$name.authentication**

  *类型*: `array`

  监听器认证重载。
认证配置可以是单个认证器实例，也可以是一个认证器数组组成的认证链。
执行登录验证时（用户名、客户端 ID 等），将按配置的顺序执行。


**listeners.tcp.$name.tcp_options**

  *类型*: [broker:tcp_opts](#tcp_opts)



## MQTT/SSL 监听器 - 8883


Settings for the MQTT over SSL listener.

**listeners.ssl.$name.enabled**

  *类型*: `boolean`

  *默认值*: `true`

  启停监听器。


**listeners.ssl.$name.bind**

  *类型*: `ip_port | integer`

  *默认值*: `8883`

  监听套接字的 IP 地址和端口。


**listeners.ssl.$name.acceptors**

  *类型*: `pos_integer`

  *默认值*: `16`

  监听器接收池的大小。


**listeners.ssl.$name.max_connections**

  *类型*: `infinity | pos_integer`

  *默认值*: `infinity`

  监听器允许的最大并发连接数。


**listeners.ssl.$name.mountpoint**

  *类型*: `string`

  *默认值*: `""`

  发布或订阅时，请在所有主题前面加上 mountpoint 字符串。

将消息传递给订阅者时，将从主题名称中删除带前缀的字符串。挂载点是一种用户可以用来实现不同侦听器之间消息路由隔离的方法。

例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。<br/>
类似地，如果另一个客户端B（与客户端A连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息<br/>

设置为<code>""</code> 以禁用该功能<br/>

mountpoint 字符串中的变量：
- <code>${clientid}</code>: clientid
- <code>${username}</code>: username


**listeners.ssl.$name.zone**

  *类型*: `atom`

  *默认值*: `default`

  监听器所属的配置组。


**listeners.ssl.$name.limiter**

  *类型*: `limiter:listener_fields`

  速率限制类型


**listeners.ssl.$name.enable_authn**

  *类型*: `enum`

  *默认值*: `true`

  *可选值*: `true | false | quick_deny_anonymous`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
客户直接拒绝，不做使用任何认证器对客户端进行身份检查。


**listeners.ssl.$name.access_rules**

  *类型*: `array`

  *默认值*: `["allow all"]`

  此监听器的访问控制规则。


**listeners.ssl.$name.proxy_protocol**

  *类型*: `boolean`

  *默认值*: `false`

  如果EMQX集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 <br/>
详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.ssl.$name.proxy_protocol_timeout**

  *类型*: `duration`

  *默认值*: `3s`

  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX将关闭TCP连接。


**listeners.ssl.$name.authentication**

  *类型*: `array`

  监听器认证重载。
认证配置可以是单个认证器实例，也可以是一个认证器数组组成的认证链。
执行登录验证时（用户名、客户端 ID 等），将按配置的顺序执行。


**listeners.ssl.$name.tcp_options**

  *类型*: [broker:tcp_opts](#tcp_opts)


**listeners.ssl.$name.ssl_options**

  *类型*: [listener_ssl_opts](#监听器-ssl-tls-配置)



## MQTT Over QUIC/UDP 监听器 - 14567

设置 MQTT over QUIC UDP 监听器，该监听器默认不启用且在某些操作系统中不可用，详情请参考 [MQTT over QUIC 快速开始](../mqtt-over-quic/getting-started.md)


Settings for the MQTT over QUIC listener.

**listeners.quic.$name.certfile**

  *类型*: `string`

  证书文件。在 5.1 中会被废弃，使用 .ssl_options.certfile 代替。


**listeners.quic.$name.keyfile**

  *类型*: `string`

  私钥文件。在 5.1 中会被废弃，使用 .ssl_options.keyfile 代替。


**listeners.quic.$name.ciphers**

  *类型*: `array`

  *默认值*: `["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256","TLS_CHACHA20_POLY1305_SHA256"]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
<br/>
密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
不兼容的密码套件将被自动删除。

例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

<br/>
注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
如果打算使用PSK密码套件，<code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>

注：QUIC 监听器不支持 tlsv1.3 的 ciphers


**listeners.quic.$name.idle_timeout**

  *类型*: `duration_ms`

  *默认值*: `0`

  一个连接在被关闭之前可以空闲多长时间。0表示禁用。


**listeners.quic.$name.handshake_idle_timeout**

  *类型*: `duration_ms`

  *默认值*: `10s`

  一个握手在被丢弃之前可以空闲多长时间。


**listeners.quic.$name.keep_alive_interval**

  *类型*: `duration_ms`

  *默认值*: `0`

  发送 PING 帧的频率，以保活连接. 设为 0 表示禁用。


**listeners.quic.$name.ssl_options**

  *类型*: `broker:listener_quic_ssl_opts`

  QUIC 传输层的 TLS 选项


**listeners.quic.$name.enabled**

  *类型*: `boolean`

  *默认值*: `true`

  启停监听器。


**listeners.quic.$name.bind**

  *类型*: `ip_port | integer`

  *默认值*: `14567`

  监听套接字的 IP 地址和端口。


**listeners.quic.$name.acceptors**

  *类型*: `pos_integer`

  *默认值*: `16`

  监听器接收池的大小。


**listeners.quic.$name.max_connections**

  *类型*: `infinity | pos_integer`

  *默认值*: `infinity`

  监听器允许的最大并发连接数。


**listeners.quic.$name.mountpoint**

  *类型*: `string`

  *默认值*: `""`

  发布或订阅时，请在所有主题前面加上 mountpoint 字符串。

将消息传递给订阅者时，将从主题名称中删除带前缀的字符串。挂载点是一种用户可以用来实现不同侦听器之间消息路由隔离的方法。

例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。<br/>
类似地，如果另一个客户端B（与客户端A连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息<br/>

设置为<code>""</code> 以禁用该功能<br/>

mountpoint 字符串中的变量：
- <code>${clientid}</code>: clientid
- <code>${username}</code>: username


**listeners.quic.$name.zone**

  *类型*: `atom`

  *默认值*: `default`

  监听器所属的配置组。


**listeners.quic.$name.limiter**

  *类型*: `limiter:listener_fields`

  速率限制类型


**listeners.quic.$name.enable_authn**

  *类型*: `enum`

  *默认值*: `true`

  *可选值*: `true | false | quick_deny_anonymous`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
客户直接拒绝，不做使用任何认证器对客户端进行身份检查。



## MQTT/WebSocket 监听器 - 8083


Settings for the MQTT over WebSocket listener.

**listeners.ws.$name.enabled**

  *类型*: `boolean`

  *默认值*: `true`

  启停监听器。


**listeners.ws.$name.bind**

  *类型*: `ip_port | integer`

  *默认值*: `8083`

  监听套接字的 IP 地址和端口。


**listeners.ws.$name.acceptors**

  *类型*: `pos_integer`

  *默认值*: `16`

  监听器接收池的大小。


**listeners.ws.$name.max_connections**

  *类型*: `infinity | pos_integer`

  *默认值*: `infinity`

  监听器允许的最大并发连接数。


**listeners.ws.$name.mountpoint**

  *类型*: `string`

  *默认值*: `""`

  发布或订阅时，请在所有主题前面加上 mountpoint 字符串。

将消息传递给订阅者时，将从主题名称中删除带前缀的字符串。挂载点是一种用户可以用来实现不同侦听器之间消息路由隔离的方法。

例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。<br/>
类似地，如果另一个客户端B（与客户端A连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息<br/>

设置为<code>""</code> 以禁用该功能<br/>

mountpoint 字符串中的变量：
- <code>${clientid}</code>: clientid
- <code>${username}</code>: username


**listeners.ws.$name.zone**

  *类型*: `atom`

  *默认值*: `default`

  监听器所属的配置组。


**listeners.ws.$name.limiter**

  *类型*: `limiter:listener_fields`

  速率限制类型


**listeners.ws.$name.enable_authn**

  *类型*: `enum`

  *默认值*: `true`

  *可选值*: `true | false | quick_deny_anonymous`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
客户直接拒绝，不做使用任何认证器对客户端进行身份检查。


**listeners.ws.$name.access_rules**

  *类型*: `array`

  *默认值*: `["allow all"]`

  此监听器的访问控制规则。


**listeners.ws.$name.proxy_protocol**

  *类型*: `boolean`

  *默认值*: `false`

  如果EMQX集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 <br/>
详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.ws.$name.proxy_protocol_timeout**

  *类型*: `duration`

  *默认值*: `3s`

  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX将关闭TCP连接。


**listeners.ws.$name.authentication**

  *类型*: `array`

  监听器认证重载。
认证配置可以是单个认证器实例，也可以是一个认证器数组组成的认证链。
执行登录验证时（用户名、客户端 ID 等），将按配置的顺序执行。


**listeners.ws.$name.tcp_options**

  *类型*: [broker:tcp_opts](#tcp_opts)


**listeners.ws.$name.websocket**

  *类型*: [broker:ws_opts](#ws_opts)



## MQTT/WebSocket with SSL 监听器 - 8084


Settings for the MQTT over WebSocket/SSL listener.

**listeners.wss.$name.enabled**

  *类型*: `boolean`

  *默认值*: `true`

  启停监听器。


**listeners.wss.$name.bind**

  *类型*: `ip_port | integer`

  *默认值*: `8084`

  监听套接字的 IP 地址和端口。


**listeners.wss.$name.acceptors**

  *类型*: `pos_integer`

  *默认值*: `16`

  监听器接收池的大小。


**listeners.wss.$name.max_connections**

  *类型*: `infinity | pos_integer`

  *默认值*: `infinity`

  监听器允许的最大并发连接数。


**listeners.wss.$name.mountpoint**

  *类型*: `string`

  *默认值*: `""`

  发布或订阅时，请在所有主题前面加上 mountpoint 字符串。

将消息传递给订阅者时，将从主题名称中删除带前缀的字符串。挂载点是一种用户可以用来实现不同侦听器之间消息路由隔离的方法。

例如，如果客户机 A 使用 <code>listeners.tcp.\<name>.mountpoint</code> 设置为'some_tenant'，那么客户端实际上订阅了主题'some_tenant/t'。<br/>
类似地，如果另一个客户端B（与客户端A连接到同一个侦听器）向主题 't' 发送消息，该消息将路由到所有订阅了'some_租户/t'的客户端，因此客户端 A 将接收主题名为't'的消息<br/>

设置为<code>""</code> 以禁用该功能<br/>

mountpoint 字符串中的变量：
- <code>${clientid}</code>: clientid
- <code>${username}</code>: username


**listeners.wss.$name.zone**

  *类型*: `atom`

  *默认值*: `default`

  监听器所属的配置组。


**listeners.wss.$name.limiter**

  *类型*: `limiter:listener_fields`

  速率限制类型


**listeners.wss.$name.enable_authn**

  *类型*: `enum`

  *默认值*: `true`

  *可选值*: `true | false | quick_deny_anonymous`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
客户直接拒绝，不做使用任何认证器对客户端进行身份检查。


**listeners.wss.$name.access_rules**

  *类型*: `array`

  *默认值*: `["allow all"]`

  此监听器的访问控制规则。


**listeners.wss.$name.proxy_protocol**

  *类型*: `boolean`

  *默认值*: `false`

  如果EMQX集群部署在 HAProxy 或 Nginx 之后，请启用代理协议 V1/2 <br/>
详情见: https://www.haproxy.com/blog/haproxy/proxy-protocol/


**listeners.wss.$name.proxy_protocol_timeout**

  *类型*: `duration`

  *默认值*: `3s`

  代理协议超时。如果在超时时间内未收到代理协议数据包，EMQX将关闭TCP连接。


**listeners.wss.$name.authentication**

  *类型*: `array`

  监听器认证重载。
认证配置可以是单个认证器实例，也可以是一个认证器数组组成的认证链。
执行登录验证时（用户名、客户端 ID 等），将按配置的顺序执行。


**listeners.wss.$name.tcp_options**

  *类型*: [broker:tcp_opts](#tcp_opts)


**listeners.wss.$name.ssl_options**

  *类型*: [broker:listener_wss_opts](#listener_wss_opts)


**listeners.wss.$name.websocket**

  *类型*: [broker:ws_opts](#ws_opts)



## MQTT 基本参数

全局的 MQTT 配置参数。


Global MQTT configuration.<br/>The configs here work as default values which can be overridden
in <code>zone</code> configs

**mqtt.idle_timeout**

  *类型*: `infinity | duration`

  *默认值*: `15s`

  TCP 连接建立后，如果在 <code>idle_timeout</code> 指定的时间内未收到客户端的 MQTT CONNECT 报文，则连接将被断开。
如果连接在 CONNECT 报文被 EMQX 接受之后空闲超过该时长，那么服务这个连接的 Erlang 进程会进入休眠以节省系统资源。
注意，该配置值如果设置过大的情况下，如果大量恶意客户端只连接，但不发任何数据，可能会导致系统资源被恶意消耗。


**mqtt.max_packet_size**

  *类型*: `bytesize`

  *默认值*: `1MB`

  允许的最大 MQTT 报文大小。


**mqtt.max_clientid_len**

  *类型*: `integer`

  *默认值*: `65535`

  *可选值*: `23-65535`

  允许的最大 MQTT Client ID 长度。


**mqtt.max_topic_levels**

  *类型*: `integer`

  *默认值*: `128`

  *可选值*: `1-65535`

  允许的最大主题层级。


**mqtt.max_qos_allowed**

  *类型*: `qos`

  *默认值*: `2`

  允许的最大 QoS 等级。


**mqtt.max_topic_alias**

  *类型*: `integer`

  *默认值*: `65535`

  *可选值*: `0-65535`

  允许的最大主题别名数，0 表示不支持主题别名。


**mqtt.retain_available**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用对 MQTT 保留消息的支持。


**mqtt.wildcard_subscription**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用对 MQTT 通配符订阅的支持。


**mqtt.shared_subscription**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用对 MQTT 共享订阅的支持。


**mqtt.exclusive_subscription**

  *类型*: `boolean`

  *默认值*: `false`

  是否启用对 MQTT 排它订阅的支持。


**mqtt.ignore_loop_deliver**

  *类型*: `boolean`

  *默认值*: `false`

  是否为 MQTT v3.1.1/v3.1.0 客户端忽略投递自己发布的消息，类似于 MQTT 5.0 中的 <code>No Local</code> 订阅选项。


**mqtt.strict_mode**

  *类型*: `boolean`

  *默认值*: `false`

  是否以严格模式解析 MQTT 消息。
当设置为 true 时，例如客户端 ID、主题名称等中的无效 utf8 字符串将导致客户端断开连接。


**mqtt.response_information**

  *类型*: `string`

  *默认值*: `""`

  指定返回给客户端的响应信息。如果设置为 ""，则禁用此功能。仅适用于使用 MQTT 5.0 协议的客户端。


**mqtt.server_keepalive**

  *类型*: `integer | disabled`

  *默认值*: `disabled`

  EMQX 要求客户端使用的保活时间，配置为 <code>disabled</code> 表示将使用客户端指定的保活时间。需要用到 MQTT 5.0 中的 <code>Server Keep Alive</code>，因此仅适用于使用 MQTT 5.0 协议的客户端。


**mqtt.keepalive_backoff**

  *类型*: `number`

  *默认值*: `0.75`

  Broker 判定客户端保活超时使用的退避乘数。如果 EMQX 在 <code>Keep Alive * Backoff * 2</code> 秒内未收到任何报文，EMQX 将关闭当前连接。


**mqtt.max_subscriptions**

  *类型*: `1..inf | infinity`

  *默认值*: `infinity`

  允许每个客户端建立的最大订阅数量。


**mqtt.upgrade_qos**

  *类型*: `boolean`

  *默认值*: `false`

  投递消息时，是否根据订阅主题时的 QoS 等级来强制提升派发的消息的 QoS 等级。


**mqtt.max_inflight**

  *类型*: `integer`

  *默认值*: `32`

  *可选值*: `1-65535`

  允许在完成应答前同时投递的 QoS 1 和 QoS 2 消息的最大数量。


**mqtt.retry_interval**

  *类型*: `duration`

  *默认值*: `30s`

  QoS 1/2 消息的重新投递间隔。


**mqtt.max_awaiting_rel**

  *类型*: `integer | infinity`

  *默认值*: `100`

  每个发布者的会话中，都存在一个队列来处理客户端发送的 QoS 2 消息。该队列会存储 QoS 2 消息的报文 ID 直到收到客户端的 PUBREL 或超时，达到队列长度的限制后，新的 QoS 2 消息发布会被拒绝，并返回 `147(0x93)` 错误。


**mqtt.await_rel_timeout**

  *类型*: `duration`

  *默认值*: `300s`

  客户端发布 QoS 2 消息时，服务器等待 `PUBREL` 的最长时延。超过该时长后服务器会放弃等待，该PACKET ID 会被释放，从而允许后续新的 PUBLISH 消息使用。如果超时后收到 PUBREL，服务器将会产生一条告警日志。注意，向订阅客户端转发消息的动作发生在进入等待之前。


**mqtt.session_expiry_interval**

  *类型*: `duration`

  *默认值*: `2h`

  指定会话将在连接断开后多久过期，仅适用于非 MQTT 5.0 的连接。


**mqtt.max_mqueue_len**

  *类型*: `non_neg_integer | infinity`

  *默认值*: `1000`

  消息队列最大长度。持久客户端断开连接或飞行窗口已满时排队的消息长度。


**mqtt.mqueue_priorities**

  *类型*: `map | disabled`

  *默认值*: `disabled`

  主题优先级。取值范围 [1-255]
默认优先级表为空，即所有的主题优先级相同。

注：优先主题名称中不支持使用逗号和等号。
注：不在此列表中的主题，被视为最高/最低优先级，这取决于<code>mqtt.mqueue_default_priority</code> 的配置

示例：
配置 <code>"topic/1" > "topic/2"</code>:
<code>mqueue_priorities: {"topic/1": 10, "topic/2": 8}</code>


**mqtt.mqueue_default_priority**

  *类型*: `enum`

  *默认值*: `lowest`

  *可选值*: `highest | lowest`

  默认的主题优先级，不在 <code>主题优先级</code>（<code>mqueue_priorities</code>） 中的主题将会使用该优先级。


**mqtt.mqueue_store_qos0**

  *类型*: `boolean`

  *默认值*: `true`

  指定在连接断开但会话保持期间，是否需要在消息队列中存储 QoS 0 消息。


**mqtt.use_username_as_clientid**

  *类型*: `boolean`

  *默认值*: `false`

  是否使用用户名作为客户端 ID。
此设置的作用时间晚于 <code>使用对端证书作为用户名</code>（<code>peer_cert_as_username</code>） 和 <code>使用对端证书作为客户端 ID</code>（<code>peer_cert_as_clientid</code>）。


**mqtt.peer_cert_as_username**

  *类型*: `enum`

  *默认值*: `disabled`

  *可选值*: `disabled | cn | dn | crt | pem | md5`

  使用对端证书中的 CN、DN 字段或整个证书内容来作为用户名。仅适用于 TLS 连接。
目前支持配置为以下内容：
- <code>cn</code>: 取证书的 CN 字段作为 Username
- <code>dn</code>: 取证书的 DN 字段作为 Username
- <code>crt</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容作为 Username
- <code>pem</code>: 将 <code>DER</code> 证书内容转换为 <code>PEM</code> 格式后作为 Username
- <code>md5</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容的 MD5 值作为 Username


**mqtt.peer_cert_as_clientid**

  *类型*: `enum`

  *默认值*: `disabled`

  *可选值*: `disabled | cn | dn | crt | pem | md5`

  使用对端证书中的 CN、DN 字段或整个证书内容来作为客户端 ID。仅适用于 TLS 连接。
目前支持配置为以下内容：
- <code>cn</code>: 取证书的 CN 字段作为 Client ID
- <code>dn</code>: 取证书的 DN 字段作为 Client ID
- <code>crt</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容作为 Client ID
- <code>pem</code>: 将 <code>DER</code> 证书内容转换为 <code>PEM</code> 格式后作为 Client ID
- <code>md5</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容的 MD5 值作为 Client ID



<!-- TODO zone 的处理 -->

<!-- #topology# -->

### 保留消息


Configuration related to handling `PUBLISH` packets with a `retain` flag set to 1.

**retainer.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否开启消息保留功能


**retainer.msg_expiry_interval**

  *类型*: `duration_ms`

  *默认值*: `0s`

  消息保留时间。0 代表永久保留


**retainer.msg_clear_interval**

  *类型*: `duration_ms`

  *默认值*: `0s`

  消息清理间隔。0 代表不进行清理


**retainer.flow_control**

  *类型*: `retainer:flow_control`

  *默认值*: `{}`

  流控设置


**retainer.max_payload_size**

  *类型*: `bytesize`

  *默认值*: `1MB`

  消息大小最大值


**retainer.stop_publish_clear_msg**

  *类型*: `boolean`

  *默认值*: `false`

  是否不发送保留消息的清理消息，在 MQTT 5.0 中如果一条保留消息的消息体为空，则会清除掉之前存储
的对应的保留消息，通过这个值控制是否停止发送清理消息


**retainer.backend**

  *类型*: `retainer:mnesia_config`

  保留消息的存储后端




Retainer batching and rate limiting.

**retainer.flow_control.batch_read_number**

  *类型*: `non_neg_integer`

  *默认值*: `0`

  从存储后端批量加载时的每批数量上限，0 代表一次性读取


**retainer.flow_control.batch_deliver_number**

  *类型*: `integer`

  *默认值*: `0`

  *可选值*: `0-1000`

  批量派发时每批的数量。0 代表一次性全部派发


**retainer.flow_control.batch_deliver_limiter**

  *类型*: `limiter:internal`

  批量发送的限流器的名称。
限流器可以用来防止短时间内向客户端发送太多的消息，从而避免过多的消息导致客户端队列堵塞甚至崩溃。
这个名称需要是指向 `limiter.batch` 下的一个真实存在的限流器。
如果这个字段为空，则不使用限流器。




Configuration of the internal database storing retained messages.

**retainer.backend.type**

  *类型*: `built_in_database`

  *默认值*: `built_in_database`

  后端类型


**retainer.backend.storage_type**

  *类型*: `enum`

  *默认值*: `ram`

  *可选值*: `ram | disc`

  选择消息是存放在磁盘还是内存中


**retainer.backend.max_retained_messages**

  *类型*: `non_neg_integer`

  *默认值*: `0`

  消息保留的数量上限。0 表示无限


**retainer.backend.index_specs**

  *类型*: `[[integer]]`

  *默认值*: `[[1,2,3],[1,3],[2,3],[3]]`

  Retainer index specifications: list of arrays of positive ascending integers. Each array specifies an index. Numbers in an index specification are 1-based word positions in topics. Words from specified positions will be used for indexing.<br/>For example, it is good to have <code>[2, 4]</code> index to optimize <code>+/X/+/Y/...</code> topic wildcard subscriptions.



### 共享订阅

是否启用共享订阅可通过 `mqtt.shared_subscription` 或 `zone.$name.shared_subscription` 配置项配置。


Per group dispatch strategy for shared subscription

**broker.shared_subscription_group.$name.strategy**

  *类型*: `enum`

  *默认值*: `random`

  *可选值*: `random | round_robin | round_robin_per_group | sticky | local | hash_topic | hash_clientid`

  共享订阅的分发策略名称。
- `random`：随机选择一个组内成员；
- `round_robin`：循环选择下一个成员；
- `round_robin_per_group`：在共享组内循环选择下一个成员；
- `sticky`：使用上一次选中的成员；
- `hash`：根据 ClientID 哈希映射到一个成员；
- `local`：随机分发到节点本地成成员，如果本地成员不存在，则随机分发到任意一个成员。



### 系统主题


The EMQX Broker periodically publishes its own status, message statistics,
client online and offline events to the system topic starting with `$SYS/`.

The following options control the behavior of `$SYS` topics.

**sys_topics.sys_msg_interval**

  *类型*: `disabled | duration`

  *默认值*: `1m`

  发送 `$SYS` 主题的间隔时间。


**sys_topics.sys_heartbeat_interval**

  *类型*: `disabled | duration`

  *默认值*: `30s`

  发送心跳系统消息的间隔时间，它包括：
  - `$SYS/brokers/<node>/uptime`
  - `$SYS/brokers/<node>/datetime`


**sys_topics.sys_event_messages**

  *类型*: `broker:event_names`

  客户端事件消息。



## MQTT 扩展功能

### 延迟发布


Settings for the delayed module.

**delayed.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否开启该功能


**delayed.max_delayed_messages**

  *类型*: `integer`

  *默认值*: `0`

  延迟消息的数量上限(0 代表无限)



### 主题重写


EMQX 的主题重写功能支持根据用户配置的规则在客户端订阅主题、发布消息、取消订阅的时候将 A 主题重写为 B 主题。
重写规则分为 Pub 规则和 Sub 规则，Pub 规则匹配 PUSHLISH 报文携带的主题，Sub 规则匹配 SUBSCRIBE、UNSUBSCRIBE 报文携带的主题。
每条重写规则都由主题过滤器、正则表达式、目标表达式三部分组成。
在主题重写功能开启的前提下，EMQX 在收到诸如 PUBLISH 报文等带有主题的 MQTT 报文时，将使用报文中的主题去依次匹配配置文件中规则的主题过滤器部分，一旦成功匹配，则使用正则表达式提取主题中的信息，然后替换至目标表达式以构成新的主题。
目标表达式中可以使用 `$N` 这种格式的变量匹配正则表达中提取出来的元素，`$N` 的值为正则表达式中提取出来的第 N 个元素，比如 `$1` 即为正则表达式提取的第一个元素。
需要注意的是，EMQX 使用倒序读取配置文件中的重写规则，当一条主题可以同时匹配多条主题重写规则的主题过滤器时，EMQX 仅会使用它匹配到的第一条规则进行重写，如果该条规则中的正则表达式与 MQTT 报文主题不匹配，则重写失败，不会再尝试使用其他的规则进行重写。
因此用户在使用时需要谨慎的设计 MQTT 报文主题以及主题重写规则。

**rewrite.$INDEX.action**

  *类型*: `enum`

  *可选值*: `subscribe | publish | all`

  主题重写在哪种操作上生效：
  - `subscribe`：订阅时重写主题；
  - `publish`：发布时重写主题；
  -`all`：全部重写主题


**rewrite.$INDEX.source_topic**

  *类型*: `string`

  源主题，客户端业务指定的主题


**rewrite.$INDEX.dest_topic**

  *类型*: `string`

  目标主题。


**rewrite.$INDEX.re**

  *类型*: `string`

  正则表达式



### 代理订阅


设备登录成功之后，通过预设的订阅表示符，为设备自动完成订阅。支持使用占位符。

**auto_subscribe.topics**

  *类型*: `array`

  *默认值*: `[]`

  设备登录成功之后，通过预设的订阅表示符，为设备自动完成订阅。支持使用占位符。




订阅标识符，支持使用占位符，例如 client/${clientid}/username/${username}/host/${host}/port/${port}
必填，且不可为空字符串

**auto_subscribe.topics.$INDEX.topic**

  *类型*: `string`

  订阅标识符，支持使用占位符，例如 client/${clientid}/username/${username}/host/${host}/port/${port}
必填，且不可为空字符串


**auto_subscribe.topics.$INDEX.qos**

  *类型*: `qos`

  *默认值*: `0`

  缺省值为 0，服务质量，
QoS 0：消息最多传递一次，如果当时客户端不可用，则会丢失该消息。
QoS 1：消息传递至少 1 次。
QoS 2：消息仅传送一次。


**auto_subscribe.topics.$INDEX.rh**

  *类型*: `integer`

  *默认值*: `0`

  *可选值*: `0-2`

  指定订阅建立时服务端是否向客户端发送保留消息，
可选值 0：只要客户端订阅成功，服务端就发送保留消息。
可选值 1：客户端订阅成功且该订阅此前不存在，服务端才发送保留消息。毕竟有些时候客户端重新发起订阅可能只是为了改变一下 QoS，并不意味着它想再次接收保留消息。
可选值 2：即便客户订阅成功，服务端也不会发送保留消息。


**auto_subscribe.topics.$INDEX.rap**

  *类型*: `integer`

  *默认值*: `0`

  *可选值*: `0-1`

  缺省值为 0，这一选项用来指定服务端向客户端转发消息时是否要保留其中的 RETAIN 标识，注意这一选项不会影响保留消息中的 RETAIN 标识。因此当 Retain As Publish 选项被设置为 0 时，客户端直接依靠消息中的 RETAIN 标识来区分这是一个正常的转发消息还是一个保留消息，而不是去判断消息是否是自己订阅后收到的第一个消息（转发消息甚至可能会先于保留消息被发送，视不同 Broker 的具体实现而定）。


**auto_subscribe.topics.$INDEX.nl**

  *类型*: `integer`

  *默认值*: `0`

  *可选值*: `0-1`

  缺省值为0，
MQTT v3.1.1：如果设备订阅了自己发布消息的主题，那么将收到自己发布的所有消息。
MQTT v5：如果设备在订阅时将此选项设置为 1，那么服务端将不会向设备转发自己发布的消息



## 日志追踪


Real-time filtering logs for the ClientID or Topic or IP for debugging.

**trace.payload_encode**

  *类型*: `enum`

  *默认值*: `text`

  *可选值*: `hex | text | hidden`

  确定跟踪文件中有效负载格式的格式。<br/>
`text`：基于文本的协议或纯文本协议。
建议在有效负载为JSON编码时使用<br/>
`hex`：二进制十六进制编码。当有效负载是自定义二进制协议时，建议使用此选项<br/>
`hidden`：有效负载被模糊化为 `******`



## 集成 Prometheus


Prometheus 监控数据推送

**prometheus.push_gateway_server**

  *类型*: `string`

  *默认值*: `http://127.0.0.1:9091`

  Prometheus 服务器地址


**prometheus.interval**

  *类型*: `duration_ms`

  *默认值*: `15s`

  数据推送间隔


**prometheus.headers**

  *类型*: `[{string, string()}]`

  *默认值*: `{}`

  推送到 Push Gateway 的 HTTP Headers 列表。<br/>
例如，<code> { Authorization = "some-authz-tokens"}</code>


**prometheus.job_name**

  *类型*: `string`

  *默认值*: `${name}/instance/${name}~${host}`

  推送到 Push Gateway 的 Job 名称。可用变量为：<br/>
- ${name}: EMQX 节点的名称。
- ${host}: EMQX 节点主机名。
例如，当 EMQX 节点名为 <code>emqx@127.0.0.1</code> 则 name 变量的值为 <code>emqx</code>，host 变量的值为 <code>127.0.0.1</code>。<br/>
默认值为: <code>${name}/instance/${name}~${host}</code>


**prometheus.enable**

  *类型*: `boolean`

  *默认值*: `false`

  开启或关闭 Prometheus 数据推送



## 集成 StatsD


StatsD 指标采集与推送配置。

**statsd.enable**

  *类型*: `boolean`

  *默认值*: `false`

  启用或禁用 StatsD 指标采集和推送服务。


**statsd.server**

  *类型*: `string`

  *默认值*: `127.0.0.1:8125`

  StatsD 服务器地址。


**statsd.sample_time_interval**

  *类型*: `duration_ms`

  *默认值*: `30s`

  指标的采样间隔。


**statsd.flush_time_interval**

  *类型*: `duration_ms`

  *默认值*: `30s`

  指标的推送间隔。


**statsd.tags**

  *类型*: `map`

  *默认值*: `{}`

  指标的标签。



## 慢订阅

慢订阅消息延迟阈值与统计策略配置。

**slow_subs.enable**

  *类型*: `boolean`

  *默认值*: `false`

  开启慢订阅


**slow_subs.threshold**

  *类型*: `duration_ms`

  *默认值*: `500ms`

  慢订阅统计的阈值


**slow_subs.expire_interval**

  *类型*: `duration_ms`

  *默认值*: `300s`

  慢订阅记录的有效时间


**slow_subs.top_k_num**

  *类型*: `pos_integer`

  *默认值*: `10`

  慢订阅统计表的记录数量上限


**slow_subs.stats_type**

  *类型*: `enum`

  *默认值*: `whole`

  *可选值*: `whole | internal | response`

  慢订阅的统计类型



## 主题统计

配置需要统计详细消息流转数据的主题。




**topic_metrics.$INDEX.topic**

  *类型*: `string`

  Collect metrics for the topic.



## 告警与监控


Settings for the alarms.

**alarm.actions**

  *类型*: `array`

  *默认值*: `["log","publish"]`

  警报激活时触发的动作。<br/>目前，支持以下操作：<code>log</code> 和 <code>publish</code>.
<code>log</code> 将告警写入日志 (控制台或者文件).
<code>publish</code> 将告警作为 MQTT 消息发布到系统主题:
<code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/activate</code> and
<code>$SYS/brokers/emqx@xx.xx.xx.x/alarms/deactivate</code>


**alarm.size_limit**

  *类型*: `integer`

  *默认值*: `1000`

  *可选值*: `1-3000`

  要保留为历史记录的已停用报警的最大总数。当超过此限制时，将删除最旧的停用报警，以限制总数。


**alarm.validity_period**

  *类型*: `duration`

  *默认值*: `24h`

  停用报警的保留时间。报警在停用时不会立即删除，而是在保留时间之后删除。



### 告警阈值

<!-- #broker:sysmon# -->


This part of the configuration is responsible for monitoring
 the host OS health, such as free memory, disk space, CPU load, etc.

**sysmon.os.cpu_check_interval**

  *类型*: `duration`

  *默认值*: `60s`

  定期 CPU 检查的时间间隔。


**sysmon.os.cpu_high_watermark**

  *类型*: `percent`

  *默认值*: `80%`

  在发出相应警报之前可以使用多少系统 CPU 的阈值，以系统CPU负载的百分比表示。


**sysmon.os.cpu_low_watermark**

  *类型*: `percent`

  *默认值*: `60%`

  在解除相应警报之前可以使用多少系统 CPU 的阈值，以系统CPU负载的百分比表示。


**sysmon.os.mem_check_interval**

  *类型*: `disabled | duration`

  *默认值*: `60s`

  定期内存检查的时间间隔。


**sysmon.os.sysmem_high_watermark**

  *类型*: `percent`

  *默认值*: `70%`

  在发出相应报警之前可以分配多少系统内存的阈值，以系统内存的百分比表示。


**sysmon.os.procmem_high_watermark**

  *类型*: `percent`

  *默认值*: `5%`

  在发出相应警报之前，一个Erlang进程可以分配多少系统内存的阈值，以系统内存的百分比表示。




This part of the configuration is responsible for monitoring
 the Erlang processes in the VM. This information can be sent to an external
 PostgreSQL database. This feature is inactive unless the PostgreSQL sink is configured.

**sysmon.top.num_items**

  *类型*: `non_neg_integer`

  *默认值*: `10`

  每个监视组的顶级进程数。


**sysmon.top.sample_interval**

  *类型*: `duration`

  *默认值*: `2s`

  指定应收集进程顶部的频率。


**sysmon.top.max_procs**

  *类型*: `non_neg_integer`

  *默认值*: `1000000`

  当 VM 中的进程数超过此值时，停止收集数据。


**sysmon.top.db_hostname**

  *类型*: `string`

  *默认值*: `""`

  收集数据点的 PostgreSQL 数据库的主机名。


**sysmon.top.db_port**

  *类型*: `integer`

  *默认值*: `5432`

  收集数据点的 PostgreSQL 数据库的端口。


**sysmon.top.db_username**

  *类型*: `string`

  *默认值*: `system_monitor`

  PostgreSQL 数据库的用户名


**sysmon.top.db_password**

  *类型*: `string`

  *默认值*: `system_monitor_password`

  PostgreSQL 数据库的密码


**sysmon.top.db_name**

  *类型*: `string`

  *默认值*: `postgres`

  PostgreSQL 数据库的数据库名




This part of the configuration is responsible for collecting
 BEAM VM events, such as long garbage collection, traffic congestion in the inter-broker
 communication, etc.

**sysmon.vm.process_check_interval**

  *类型*: `duration`

  *默认值*: `30s`

  定期进程限制检查的时间间隔。


**sysmon.vm.process_high_watermark**

  *类型*: `percent`

  *默认值*: `80%`

  在发出相应警报之前，本地节点上可以同时存在多少进程的阈值（以进程百分比表示）。


**sysmon.vm.process_low_watermark**

  *类型*: `percent`

  *默认值*: `60%`

  在清除相应警报之前，本地节点上可以同时存在多少进程的阈值（以进程百分比表示）。


**sysmon.vm.long_gc**

  *类型*: `disabled | duration`

  *默认值*: `disabled`

  当系统检测到某个 Erlang 进程垃圾回收占用过长时间，会触发一条带有 <code>long_gc</code> 关键字的日志。
同时还会发布一条主题为 <code>$SYS/sysmon/long_gc</code> 的 MQTT 系统消息。


**sysmon.vm.long_schedule**

  *类型*: `disabled | duration`

  *默认值*: `240ms`

  启用后，如果 Erlang VM 调度器出现某个任务占用时间过长时，会触发一条带有 'long_schedule' 关键字的日志。
同时还会发布一条主题为 <code>$SYS/sysmon/long_schedule</code> 的 MQTT 系统消息。


**sysmon.vm.large_heap**

  *类型*: `disabled | bytesize`

  *默认值*: `32MB`

  启用后，当一个 Erlang 进程申请了大量内存，系统会触发一条带有 <code>large_heap</code> 关键字的
warning 级别日志。同时还会发布一条主题为 <code>$SYS/sysmon/busy_dist_port</code> 的 MQTT 系统消息。


**sysmon.vm.busy_dist_port**

  *类型*: `boolean`

  *默认值*: `true`

  启用后，当用于集群接点之间 RPC 的连接过忙时，会触发一条带有 <code>busy_dist_port</code> 关键字的 warning 级别日志。
同时还会发布一条主题为 <code>$SYS/sysmon/busy_dist_port</code> 的 MQTT 系统消息。


**sysmon.vm.busy_port**

  *类型*: `boolean`

  *默认值*: `true`

  当一个系统接口（例如 TCP socket）过忙，会触发一条带有 <code>busy_port</code> 关键字的 warning 级别的日志。
同时还会发布一条主题为 <code>$SYS/sysmon/busy_port</code> 的 MQTT 系统消息。



## 速率限制

<!-- TODO 速率限制的配置比较混乱，需要重构 -->

EMQX 速率限制提供了 **bytes_in**、**message_in**、**connection**、 **message_routing**、**internal**、**client** 六种速率限制器，以及 **节点**、**监听器**、**连接** 三个不同级别的速率限制功能。

有关速率限制的介绍以及使用请参考 [速率限制](../rate-limit/rate-limit.md)。

### 可用的速率限制器


Settings for the rate limiter.

**limiter.bytes_in**

  *类型*: `limiter:node_opts`

  *默认值*: `{}`

  流入字节率控制器。
这个是用来控制当前节点上的数据流入的字节率，每条消息将会消耗和其二进制大小等量的令牌，当达到最大速率后，会话将会被限速甚至被强制挂起一小段时间


**limiter.message_in**

  *类型*: `limiter:node_opts`

  *默认值*: `{}`

  流入速率控制器。
这个用来控制当前节点上的消息流入速率，当达到最大速率后，会话将会被限速甚至被强制挂起一小段时间


**limiter.connection**

  *类型*: `limiter:node_opts`

  *默认值*: `{}`

  连接速率控制器。
这个用来控制当前节点上的连接速率，当达到最大速率后，新的连接将会被拒绝


**limiter.message_routing**

  *类型*: `limiter:node_opts`

  *默认值*: `{}`

  消息派发速率控制器。
这个用来控制当前节点内的消息派发速率，当达到最大速率后，新的推送将会被拒绝


**limiter.internal**

  *类型*: `limiter:node_opts`

  *默认值*: `{}`

  EMQX 内部功能所用限制器。


**limiter.client**

  *类型*: `limiter:client_fields`

  *默认值*: `{"bytes_in":{},"connection":{},"internal":{},"message_in":{},"message_routing":{}}`

  对桶的每个使用者的速率控制设置



### 速率限制器可用配置

每个速率限制器下可用的配置项。

```bash
limiter.message_in {
  rate  =  infinity
  burst  =  0
}
```



### 节点级别速率限制

**注意：** 只有配置了速率限制的监听器，才会受到节点级设置的影响


Settings for the limiter of the node level.

**limiter.$type.rate**

  *类型*: `rate`

  *默认值*: `infinity`

  桶的令牌生成速率


**limiter.$type.burst**

  *类型*: `burst_rate`

  *默认值*: `0`

  突发速率。
突发速率允许短时间内速率超过设置的速率值，突发速率 + 速率 = 当前桶能达到的最大速率值



### 监听器级别速率限制

在监听器中配置中配置速率限制器。

**listeners.\$type.$name.limiter.bytes_in**

  *类型*: `limiter:bucket_infinity`

  流入字节率控制器。
这个是用来控制当前节点上的数据流入的字节率，每条消息将会消耗和其二进制大小等量的令牌，当达到最大速率后，会话将会被限速甚至被强制挂起一小段时间


**listeners.\$type.$name.limiter.message_in**

  *类型*: `limiter:bucket_infinity`

  流入速率控制器。
这个用来控制当前节点上的消息流入速率，当达到最大速率后，会话将会被限速甚至被强制挂起一小段时间


**listeners.\$type.$name.limiter.connection**

  *类型*: `limiter:bucket_limit`

  连接速率控制器。
这个用来控制当前节点上的连接速率，当达到最大速率后，新的连接将会被拒绝


**listeners.\$type.$name.limiter.message_routing**

  *类型*: `limiter:bucket_infinity`

  消息派发速率控制器。
这个用来控制当前节点内的消息派发速率，当达到最大速率后，新的推送将会被拒绝


**listeners.\$type.$name.limiter.client**

  *类型*: `limiter:listener_client_fields`

  对桶的每个使用者的速率控制设置



### 连接级别速率限制

<!-- #limiter:client_fields# -->

#### 对节点单个连接进行限制


Settings for the client in bucket level.

**limiter.client.\$type.rate**

  *类型*: `rate`

  *默认值*: `infinity`

  桶的令牌生成速率


**limiter.client.\$type.initial**

  *类型*: `initial`

  *默认值*: `0`

  桶中的初始令牌数


**limiter.client.\$type.low_watermark**

  *类型*: `initial`

  *默认值*: `0`

  当桶中剩余的令牌数低于这个值，即使令牌申请成功了，也会被强制暂停一会儿


**limiter.client.\$type.capacity**

  *类型*: `capacity`

  *默认值*: `infinity`

  每个使用者的令牌容量上限


**limiter.client.\$type.divisible**

  *类型*: `boolean`

  *默认值*: `false`

  申请的令牌数是否可以被分割


**limiter.client.\$type.max_retry_time**

  *类型*: `duration`

  *默认值*: `10s`

  申请失败后，尝试重新申请的时长最大值


**limiter.client.\$type.failure_strategy**

  *类型*: `failure_strategy`

  *默认值*: `force`

  当所有的重试都失败后的处理策略



#### 指定监听器下单个连接进行限制


Fields of the client level of the listener.

**listeners.\$type.$name.limiter.client.bytes_in**

  *类型*: `limiter:client_opts`

  流入字节率控制器。
这个是用来控制当前节点上的数据流入的字节率，每条消息将会消耗和其二进制大小等量的令牌，当达到最大速率后，会话将会被限速甚至被强制挂起一小段时间


**listeners.\$type.$name.limiter.client.message_in**

  *类型*: `limiter:client_opts`

  流入速率控制器。
这个用来控制当前节点上的消息流入速率，当达到最大速率后，会话将会被限速甚至被强制挂起一小段时间


**listeners.\$type.$name.limiter.client.connection**

  *类型*: `limiter:client_opts`

  连接速率控制器。
这个用来控制当前节点上的连接速率，当达到最大速率后，新的连接将会被拒绝


**listeners.\$type.$name.limiter.client.message_routing**

  *类型*: `limiter:client_opts`

  消息派发速率控制器。
这个用来控制当前节点内的消息派发速率，当达到最大速率后，新的推送将会被拒绝



### 保留消息派发速率限制


Internal limiter.

**retainer.flow_control.batch_deliver_limiter.rate**

  *类型*: `rate`

  *默认值*: `infinity`

  桶的令牌生成速率


**retainer.flow_control.batch_deliver_limiter.capacity**

  *类型*: `capacity`

  *默认值*: `infinity`

  该令牌桶的容量


**retainer.flow_control.batch_deliver_limiter.initial**

  *类型*: `initial`

  *默认值*: `0`

  桶中的初始令牌数


**retainer.flow_control.batch_deliver_limiter.client**

  *类型*: `limiter:client_opts`

  对桶的每个使用者的速率控制设置



## 过载保护


Overload protection mechanism monitors the load of the system and temporarily
disables some features (such as accepting new connections) when the load is high.

**overload_protection.enable**

  *类型*: `boolean`

  *默认值*: `false`

  是否对系统过载做出反应。


**overload_protection.backoff_delay**

  *类型*: `integer`

  *默认值*: `1`

  *可选值*: `0-inf`

  高负载时，一些不重要的任务可能会延迟执行，在这里设置允许延迟的时间。


**overload_protection.backoff_gc**

  *类型*: `boolean`

  *默认值*: `false`

  高负载时，跳过强制 GC。


**overload_protection.backoff_hibernation**

  *类型*: `boolean`

  *默认值*: `true`

  高负载时，跳过进程休眠。


**overload_protection.backoff_new_conn**

  *类型*: `boolean`

  *默认值*: `true`

  高负载时，拒绝新进来的客户端连接。



## 性能优化

### broker_perf


Broker performance tuning parameters.

**broker.perf.route_lock_type**

  *类型*: `enum`

  *默认值*: `key`

  *可选值*: `key | tab | global`

  通配主题订阅/取消订阅性能调优。
建议仅当通配符主题较多时才更改此参数。

注：当从/更改为 `global` 锁时，它要求集群中的所有节点在更改之前停止。
  - `key`：为 Mnesia 事务涉及到的每个 key 上锁，建议单节点时使用。
  - `tab`：为 Mnesia 事务涉及到的表上锁，建议在集群中使用。
  - `global`：所以更新操作都被全局的锁保护，仅建议在超大规模集群中使用。


**broker.perf.trie_compaction**

  *类型*: `boolean`

  *默认值*: `true`

  是否开启主题表压缩存储。
启用它会显着提高通配符主题订阅率，如果通配符主题具有唯一前缀，例如：'sensor/{{id}}/+/'，其中每个订阅者的 ID 是唯一的。
如果消息主要发布到具有大量级别的主题，则主题匹配性能（发布时）可能会降低。

注意：这是一个集群范围的配置。 它要求在更改之前停止所有节点。



### force_gc


Force garbage collection in MQTT connection process after
 they process certain number of messages or bytes of data.

**force_gc.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用强制垃圾回收。


**force_gc.count**

  *类型*: `integer`

  *默认值*: `16000`

  *可选值*: `0-inf`

  在进程收到多少消息之后，对此进程执行垃圾回收。


**force_gc.bytes**

  *类型*: `bytesize`

  *默认值*: `16MB`

  在进程处理过多少个字节之后，对此进程执行垃圾回收。



### force_shutdown


When the process message queue length, or the memory bytes
reaches a certain value, the process is forced to close.

Note: "message queue" here refers to the "message mailbox"
of the Erlang process, not the `mqueue` of QoS 1 and QoS 2.

**force_shutdown.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用 `force_shutdown` 功能。


**force_shutdown.max_message_queue_len**

  *类型*: `integer`

  *默认值*: `1000`

  *可选值*: `0-inf`

  消息队列的最大长度。


**force_shutdown.max_heap_size**

  *类型*: `wordsize`

  *默认值*: `32MB`

  Heap 的总大小。



### conn_congestion


Settings for `conn_congestion` alarm.

Sometimes the MQTT connection (usually an MQTT subscriber) may
get "congested", because there are too many packets to be sent.
The socket tries to buffer the packets until the buffer is
full. If more packets arrive after that, the packets will be
"pending" in the queue, and we consider the connection
congested.

Note: `sndbuf` can be set to larger value if the
alarm is triggered too often.
The name of the alarm is of format `conn_congestion/<ClientID>/<Username>`,
where the `<ClientID>` is the client ID of the congested MQTT connection,
and `<Username>` is the username or `unknown_user`.

**conn_congestion.enable_alarm**

  *类型*: `boolean`

  *默认值*: `true`

  启用或者禁用连接阻塞告警功能。


**conn_congestion.min_alarm_sustain_duration**

  *类型*: `duration`

  *默认值*: `1m`

  清除警报前的最短时间。<br/>只有当队列中没有挂起的数据，并且连接至少被堵塞了 <code>min_alarm_sustain_duration</code> 毫秒时，<br/>报警才会被清除。这是为了避免太频繁地清除和再次发出警报。



### flapping_detect


This config controls the allowed maximum number of `CONNECT` packets received
from the same clientid in a time frame defined by `window_time`.
After the limit is reached, successive `CONNECT` requests are forbidden
(banned) until the end of the time period defined by `ban_time`.

**flapping_detect.enable**

  *类型*: `boolean`

  *默认值*: `false`

  启用抖动检测功能。


**flapping_detect.max_count**

  *类型*: `integer`

  *默认值*: `15`

  MQTT 客户端在“窗口”时间内允许的最大断开次数。


**flapping_detect.window_time**

  *类型*: `duration`

  *默认值*: `1m`

  抖动检测的时间窗口。


**flapping_detect.ban_time**

  *类型*: `duration`

  *默认值*: `5m`

  抖动的客户端将会被禁止登录多长时间。



### stats 统计


Enable/disable statistic data collection.
Statistic data such as message receive/send count/rate etc. It provides insights of system performance and helps to diagnose issues. You can find statistic data from the dashboard, or from the '/stats' API.

**stats.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用统计数据收集功能。



### 持久会话优化


Settings for message persistence.

**persistent_session_store.enabled**

  *类型*: `boolean`

  *默认值*: `false`

  使用数据库存储有关持久会话的信息。
这使得在节点停止时，可以将客户端连接迁移到另一个群集节点。


**persistent_session_store.on_disc**

  *类型*: `boolean`

  *默认值*: `true`

  将持久会话数据保存在磁盘上。如果为 false 则存储在内存中。
如开启， 持久会话数据可在集群重启后恢复。
如关闭， 数据仅存储在内存中， 则在整个集群停止后丢失。


**persistent_session_store.ram_cache**

  *类型*: `boolean`

  *默认值*: `false`

  在内存中保持一份数据的副本，以便更快地访问。


**persistent_session_store.backend**

  *类型*: [broker:persistent_session_builtin](#broker:persistent_session_builtin)

  *默认值*: `{"messages":{"ram_cache":"false"},"session":{"ram_cache":"true"},"session_messages":{"ram_cache":"true"},"type":"builtin"}`

  用于存储持久性会话和信息的数据库管理后端
- `builtin`: 使用内置的数据库（mria）


**persistent_session_store.max_retain_undelivered**

  *类型*: `duration`

  *默认值*: `1h`

  如果重新启动时处理上一个会话的节点已停止，则未传递到持久会话的消息在垃圾收集之前会被存储。


**persistent_session_store.message_gc_interval**

  *类型*: `duration`

  *默认值*: `1h`

  将未送达的消息垃圾收集到持久会话的开始间隔。
这会影响检查 "max_retain_undelivered"（最大保留未送达）的删除频率。


**persistent_session_store.session_message_gc_interval**

  *类型*: `duration`

  *默认值*: `1m`

  持久会话消息的临时数据垃圾收集的开始间隔。
这不会影响持久会话消息的生命周期长度。




Settings for the built-in storage engine of persistent messages.

**persistent_session_store.backend.type**

  *类型*: `enum`

  *默认值*: `builtin`

  *可选值*: `builtin`


**persistent_session_store.backend.session**

  *类型*: `broker:persistent_table_mria_opts`

  用于内建会话表的性能调优参数。


**persistent_session_store.backend.session_messages**

  *类型*: `broker:persistent_table_mria_opts`

  优化内置的会话消息表的配置。


**persistent_session_store.backend.messages**

  *类型*: `broker:persistent_table_mria_opts`

  用于内建消息表的性能调优参数。




Tuning options for the mria table.

**persistent_table_mria_opts.ram_cache**

  *类型*: `boolean`

  *默认值*: `true`

  在内存中保持一份数据的副本，以便更快地访问。



{% emqxce %}

## 遥测


Settings for the telemetry module.

**telemetry.enable**

  *类型*: `boolean`

  *默认值*: `true`

  Enable telemetry.



{% endemqxce %}

<!-- ## zone 配置 -->

<!-- #zone:overload_protection# -->

## Dashboard


EMQX Dashboard 配置。

**dashboard.listeners**

  *类型*: `dashboard:listeners`

  Dashboard 监听器设置。监听器必须有唯一的端口号和IP地址的组合。
例如，可以通过指定IP地址 0.0.0.0 来监听机器上给定端口上的所有配置的IP地址。
或者，可以为每个监听器指定唯一的IP地址，但使用相同的端口。


**dashboard.default_username**

  *类型*: `string`

  *默认值*: `admin`

  Dashboard 的默认用户名。


**dashboard.default_password**

  *类型*: `string`

  *默认值*: `public`

  Dashboard 的默认密码，为了安全，应该尽快修改密码。
当通过网页首次登录 Dashboard 并按提示修改成复杂密码后，此值就会失效。


**dashboard.sample_interval**

  *类型*: `duration_s`

  *默认值*: `10s`

  Dashboard 中图表指标的时间间隔。必须小于60，且被60的整除，默认设置 10s。


**dashboard.token_expired_time**

  *类型*: `duration`

  *默认值*: `60m`

  JWT token 过期时间。默认设置为 60 分钟。


**dashboard.cors**

  *类型*: `boolean`

  *默认值*: `false`

  支持跨域资源共享(CORS)，
允许服务器指示任何来源(域名、协议或端口)，除了本服务器之外的任何浏览器应允许加载资源。


**dashboard.i18n_lang**

  *类型*: `enum`

  *默认值*: `en`

  *可选值*: `en | zh`

  设置 Swagger 多语言的版本，可为 en 或 zh。


**dashboard.bootstrap_users_file**

  *类型*: `string`

  *默认值*: `""`

  已废弃，请使用 api_key.bootstrap_file。




Dashboard 监听器(HTTP)配置。

**dashboard.listeners.http.enable**

  *类型*: `boolean`

  *默认值*: `true`

  忽略或启用该监听器。


**dashboard.listeners.http.bind**

  *类型*: `non_neg_integer | ip_port`

  *默认值*: `18083`

  监听地址和端口，热更新此配置时，会重启 Dashboard 服务。


**dashboard.listeners.http.num_acceptors**

  *类型*: `integer`

  *默认值*: `8`

  TCP协议的Socket acceptor池大小, 默认设置在线的调度器数量（通常为 CPU 核数）


**dashboard.listeners.http.max_connections**

  *类型*: `integer`

  *默认值*: `512`

  同时处理的最大连接数。


**dashboard.listeners.http.backlog**

  *类型*: `integer`

  *默认值*: `1024`

  排队等待连接的队列的最大长度。


**dashboard.listeners.http.send_timeout**

  *类型*: `duration`

  *默认值*: `10s`

  Socket发送超时时间。


**dashboard.listeners.http.inet6**

  *类型*: `boolean`

  *默认值*: `false`

  启用IPv6， 如果机器不支持IPv6，请关闭此选项，否则会导致 Dashboard 无法使用。


**dashboard.listeners.http.ipv6_v6only**

  *类型*: `boolean`

  *默认值*: `false`

  当开启 inet6 功能的同时禁用 IPv4-to-IPv6 映射。该配置仅在 inet6 功能开启时有效。


**dashboard.listeners.http.proxy_header**

  *类型*: `boolean`

  *默认值*: `false`

  开启对  `HAProxy` 的支持，注意：一旦开启了这个功能，就无法再处理普通的 HTTP 请求了。




Dashboard 监听器(HTTPS)配置。

**dashboard.listeners.https.enable**

  *类型*: `boolean`

  *默认值*: `false`

  忽略或启用该监听器。


**dashboard.listeners.https.bind**

  *类型*: `non_neg_integer | ip_port`

  *默认值*: `18084`

  监听地址和端口，热更新此配置时，会重启 Dashboard 服务。


**dashboard.listeners.https.num_acceptors**

  *类型*: `integer`

  *默认值*: `8`

  TCP协议的Socket acceptor池大小, 默认设置在线的调度器数量（通常为 CPU 核数）


**dashboard.listeners.https.max_connections**

  *类型*: `integer`

  *默认值*: `512`

  同时处理的最大连接数。


**dashboard.listeners.https.backlog**

  *类型*: `integer`

  *默认值*: `1024`

  排队等待连接的队列的最大长度。


**dashboard.listeners.https.send_timeout**

  *类型*: `duration`

  *默认值*: `10s`

  Socket发送超时时间。


**dashboard.listeners.https.inet6**

  *类型*: `boolean`

  *默认值*: `false`

  启用IPv6， 如果机器不支持IPv6，请关闭此选项，否则会导致 Dashboard 无法使用。


**dashboard.listeners.https.ipv6_v6only**

  *类型*: `boolean`

  *默认值*: `false`

  当开启 inet6 功能的同时禁用 IPv4-to-IPv6 映射。该配置仅在 inet6 功能开启时有效。


**dashboard.listeners.https.proxy_header**

  *类型*: `boolean`

  *默认值*: `false`

  开启对  `HAProxy` 的支持，注意：一旦开启了这个功能，就无法再处理普通的 HTTP 请求了。


**dashboard.listeners.https.cacertfile**

  *类型*: `string`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**dashboard.listeners.https.certfile**

  *类型*: `string`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**dashboard.listeners.https.keyfile**

  *类型*: `string`

  PEM格式的私钥文件。


**dashboard.listeners.https.verify**

  *类型*: `enum`

  *默认值*: `verify_none`

  *可选值*: `verify_peer | verify_none`

  启用或禁用对等验证。


**dashboard.listeners.https.reuse_sessions**

  *类型*: `boolean`

  *默认值*: `true`

  启用 TLS 会话重用。


**dashboard.listeners.https.depth**

  *类型*: `integer`

  *默认值*: `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
因此，如果深度为0，则对等方必须由受信任的根 CA 直接签名；<br/>
如果是1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
如果是2，则路径可以是PEER、中间 CA1、中间 CA2、ROOT-CA。


**dashboard.listeners.https.password**

  *类型*: `string`

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。


**dashboard.listeners.https.versions**

  *类型*: `array`

  *默认值*: `["tlsv1.3","tlsv1.2","tlsv1.1","tlsv1"]`

  支持所有TLS/DTLS版本<br/>
注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


**dashboard.listeners.https.ciphers**

  *类型*: `array`

  *默认值*: `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
<br/>
密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
不兼容的密码套件将被自动删除。

例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

<br/>
注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**dashboard.listeners.https.user_lookup_fun**

  *类型*: `string`

  *默认值*: `emqx_tls_psk:lookup`

  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


**dashboard.listeners.https.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**dashboard.listeners.https.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

   在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**dashboard.listeners.https.dhfile**

  *类型*: `string`

  如果协商使用Diffie-Hellman密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数。<br/>
注意：TLS 1.3不支持<code>dhfile</code>选项。


**dashboard.listeners.https.honor_cipher_order**

  *类型*: `boolean`

  *默认值*: `true`

  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


**dashboard.listeners.https.client_renegotiation**

  *类型*: `boolean`

  *默认值*: `true`

  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
这可能会成为拒绝服务攻击的载体。
SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。


**dashboard.listeners.https.handshake_timeout**

  *类型*: `duration`

  *默认值*: `15s`

  握手完成所允许的最长时间




Dashboard 监听器配置。

**dashboard.listeners.http**

  *类型*: `dashboard:http`

  TCP listeners


**dashboard.listeners.https**

  *类型*: `dashboard:https`

  SSL listeners



## API 密钥


API 密钥， 可用于请求除管理 API 密钥及 Dashboard 用户管理 API 的其它接口

**api_key.bootstrap_file**

  *类型*: `string`

  *默认值*: `""`

  用于在启动 emqx 时，添加 API 密钥，其格式为：
      ```
      7e729ae70d23144b:2QILI9AcQ9BYlVqLDHQNWN2saIjBV4egr1CZneTNKr9CpK
      ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL
      ```



## 认证

### 密码认证 - 内置数据库数据源


使用内置数据库作为认证数据源的认证器的配置项。

**authn-builtin_db:authentication.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-builtin_db:authentication.backend**

  *类型*: `built_in_database`

  后端类型。


**authn-builtin_db:authentication.user_id_type**

  *类型*: `enum`

  *默认值*: `username`

  *可选值*: `clientid | username`

  指定使用客户端ID `clientid` 还是用户名 `username` 进行认证。


**authn-builtin_db:authentication.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt_rw](#authn-hash:bcrypt_rw) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"name":"sha256","salt_position":"prefix"}`

  Options for password hash creation and verification.


**authn-builtin_db:authentication.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。



### 密码认证 - MySQL


使用 MySQL 作为认证数据源的认证器的配置项。

**authn-mysql:authentication.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-mysql:authentication.backend**

  *类型*: `mysql`

  后端类型。


**authn-mysql:authentication.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"name":"sha256","salt_position":"prefix"}`

  Options for password hash verification.


**authn-mysql:authentication.query**

  *类型*: `string`

  用于查询密码散列等用于认证的数据的 SQL 语句。


**authn-mysql:authentication.query_timeout**

  *类型*: `duration_ms`

  *默认值*: `5s`

  SQL 查询的超时时间。


**authn-mysql:authentication.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn-mysql:authentication.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 MySQL 默认端口 3306。


**authn-mysql:authentication.database**

  *类型*: `string`

  数据库名字。


**authn-mysql:authentication.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn-mysql:authentication.username**

  *类型*: `string`

  *默认值*: `root`

  内部数据库的用户名。


**authn-mysql:authentication.password**

  *类型*: `string`

  内部数据库密码。


**authn-mysql:authentication.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authn-mysql:authentication.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



<!-- #{ "id": "authn-mysql:authentication", "path": "authentication.$INDEX.password-based:mysql" }# -->

### 密码认证 - PostgreSQL


使用 PostgreSQL 作为认证数据源的认证器的配置项。

**authn-postgresql:authentication.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-postgresql:authentication.backend**

  *类型*: `postgresql`

  后端类型。


**authn-postgresql:authentication.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"name":"sha256","salt_position":"prefix"}`

  Options for password hash verification.


**authn-postgresql:authentication.query**

  *类型*: `string`

  用于查询密码散列等用于认证的数据的 SQL 语句。


**authn-postgresql:authentication.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn-postgresql:authentication.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 PostgreSQL 默认端口 5432。


**authn-postgresql:authentication.database**

  *类型*: `string`

  数据库名字。


**authn-postgresql:authentication.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn-postgresql:authentication.username**

  *类型*: `string`

  内部数据库的用户名。


**authn-postgresql:authentication.password**

  *类型*: `string`

  内部数据库密码。


**authn-postgresql:authentication.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authn-postgresql:authentication.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### 密码认证 - Redis

#### Redis 单机


使用 Redis (Standalone) 作为认证数据源的认证器的配置项。

**authn-redis:standalone.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-redis:standalone.backend**

  *类型*: `redis`

  后端类型。


**authn-redis:standalone.cmd**

  *类型*: `string`

  用于查询密码散列等用于认证的数据的 Redis Command，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。


**authn-redis:standalone.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"name":"sha256","salt_position":"prefix"}`

  Options for password hash verification.


**authn-redis:standalone.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn-redis:standalone.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**authn-redis:standalone.redis_type**

  *类型*: `single`

  *默认值*: `single`

  单机模式。当 Redis 服务运行在单机模式下，该配置必须设置为 'single'。


**authn-redis:standalone.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn-redis:standalone.password**

  *类型*: `string`

  内部数据库密码。


**authn-redis:standalone.database**

  *类型*: `integer`

  *默认值*: `0`

  Redis 数据库 ID。


**authn-redis:standalone.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authn-redis:standalone.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### Redis 集群


使用 Redis (Cluster) 作为认证数据源的认证器的配置项。

**authn-redis:cluster.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-redis:cluster.backend**

  *类型*: `redis`

  后端类型。


**authn-redis:cluster.cmd**

  *类型*: `string`

  用于查询密码散列等用于认证的数据的 Redis Command，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。


**authn-redis:cluster.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"name":"sha256","salt_position":"prefix"}`

  Options for password hash verification.


**authn-redis:cluster.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn-redis:cluster.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**authn-redis:cluster.redis_type**

  *类型*: `cluster`

  *默认值*: `cluster`

  集群模式。当 Redis 服务运行在集群模式下，该配置必须设置为 'cluster'。


**authn-redis:cluster.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn-redis:cluster.password**

  *类型*: `string`

  内部数据库密码。


**authn-redis:cluster.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authn-redis:cluster.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### Redis 哨兵


使用 Redis (Sentinel) 作为认证数据源的认证器的配置项。

**authn-redis:sentinel.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-redis:sentinel.backend**

  *类型*: `redis`

  后端类型。


**authn-redis:sentinel.cmd**

  *类型*: `string`

  用于查询密码散列等用于认证的数据的 Redis Command，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。


**authn-redis:sentinel.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"name":"sha256","salt_position":"prefix"}`

  Options for password hash verification.


**authn-redis:sentinel.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn-redis:sentinel.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**authn-redis:sentinel.redis_type**

  *类型*: `sentinel`

  *默认值*: `sentinel`

  哨兵模式。当 Redis 服务运行在哨兵模式下，该配置必须设置为 'sentinel'。


**authn-redis:sentinel.sentinel**

  *类型*: `string`

  Redis 哨兵模式下的集群名称。


**authn-redis:sentinel.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn-redis:sentinel.password**

  *类型*: `string`

  内部数据库密码。


**authn-redis:sentinel.database**

  *类型*: `integer`

  *默认值*: `0`

  Redis 数据库 ID。


**authn-redis:sentinel.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authn-redis:sentinel.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### 密码认证 - MongoDB

#### MongoDB 单机


使用 MongoDB (Standalone) 作为认证数据源的认证器的配置项。

**authn-mongodb:standalone.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-mongodb:standalone.backend**

  *类型*: `mongodb`

  后端类型。


**authn-mongodb:standalone.collection**

  *类型*: `string`

  存储认证数据的集合。


**authn-mongodb:standalone.filter**

  *类型*: `map`

  *默认值*: `{}`

  在查询中定义过滤条件的条件表达式。
过滤器支持如下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


**authn-mongodb:standalone.password_hash_field**

  *类型*: `string`

  *默认值*: `password_hash`

  文档中用于存放密码散列的字段。


**authn-mongodb:standalone.salt_field**

  *类型*: `string`

  *默认值*: `salt`

  文档中用于存放盐值的字段。


**authn-mongodb:standalone.is_superuser_field**

  *类型*: `string`

  *默认值*: `is_superuser`

  文档中用于定义用户是否具有超级用户权限的字段。


**authn-mongodb:standalone.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"name":"sha256","salt_position":"prefix"}`

  Options for password hash verification.


**authn-mongodb:standalone.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn-mongodb:standalone.mongo_type**

  *类型*: `single`

  *默认值*: `single`

  Standalone 模式。当 MongoDB 服务运行在 standalone 模式下，该配置必须设置为 'single'。


**authn-mongodb:standalone.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**authn-mongodb:standalone.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**authn-mongodb:standalone.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**authn-mongodb:standalone.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn-mongodb:standalone.username**

  *类型*: `string`

  内部数据库的用户名。


**authn-mongodb:standalone.password**

  *类型*: `string`

  内部数据库密码。


**authn-mongodb:standalone.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**authn-mongodb:standalone.database**

  *类型*: `string`

  数据库名字。


**authn-mongodb:standalone.topology**

  *类型*: `topology`


**authn-mongodb:standalone.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### MongoDB replica set


使用 MongoDB (Replica Set) 作为认证数据源的认证器的配置项。

**authn-mongodb:replica-set.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-mongodb:replica-set.backend**

  *类型*: `mongodb`

  后端类型。


**authn-mongodb:replica-set.collection**

  *类型*: `string`

  存储认证数据的集合。


**authn-mongodb:replica-set.filter**

  *类型*: `map`

  *默认值*: `{}`

  在查询中定义过滤条件的条件表达式。
过滤器支持如下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


**authn-mongodb:replica-set.password_hash_field**

  *类型*: `string`

  *默认值*: `password_hash`

  文档中用于存放密码散列的字段。


**authn-mongodb:replica-set.salt_field**

  *类型*: `string`

  *默认值*: `salt`

  文档中用于存放盐值的字段。


**authn-mongodb:replica-set.is_superuser_field**

  *类型*: `string`

  *默认值*: `is_superuser`

  文档中用于定义用户是否具有超级用户权限的字段。


**authn-mongodb:replica-set.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"name":"sha256","salt_position":"prefix"}`

  Options for password hash verification.


**authn-mongodb:replica-set.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn-mongodb:replica-set.mongo_type**

  *类型*: `rs`

  *默认值*: `rs`

  Replica set模式。当 MongoDB 服务运行在 replica-set 模式下，该配置必须设置为 'rs'。


**authn-mongodb:replica-set.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**authn-mongodb:replica-set.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**authn-mongodb:replica-set.r_mode**

  *类型*: `enum`

  *默认值*: `master`

  *可选值*: `master | slave_ok`

  读模式。


**authn-mongodb:replica-set.replica_set_name**

  *类型*: `string`

  副本集的名称。


**authn-mongodb:replica-set.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**authn-mongodb:replica-set.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn-mongodb:replica-set.username**

  *类型*: `string`

  内部数据库的用户名。


**authn-mongodb:replica-set.password**

  *类型*: `string`

  内部数据库密码。


**authn-mongodb:replica-set.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**authn-mongodb:replica-set.database**

  *类型*: `string`

  数据库名字。


**authn-mongodb:replica-set.topology**

  *类型*: `topology`


**authn-mongodb:replica-set.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### MongoDD sharded cluster


使用 MongoDB (Sharded Cluster) 作为认证数据源的认证器的配置项。

**authn-mongodb:sharded-cluster.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-mongodb:sharded-cluster.backend**

  *类型*: `mongodb`

  后端类型。


**authn-mongodb:sharded-cluster.collection**

  *类型*: `string`

  存储认证数据的集合。


**authn-mongodb:sharded-cluster.filter**

  *类型*: `map`

  *默认值*: `{}`

  在查询中定义过滤条件的条件表达式。
过滤器支持如下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


**authn-mongodb:sharded-cluster.password_hash_field**

  *类型*: `string`

  *默认值*: `password_hash`

  文档中用于存放密码散列的字段。


**authn-mongodb:sharded-cluster.salt_field**

  *类型*: `string`

  *默认值*: `salt`

  文档中用于存放盐值的字段。


**authn-mongodb:sharded-cluster.is_superuser_field**

  *类型*: `string`

  *默认值*: `is_superuser`

  文档中用于定义用户是否具有超级用户权限的字段。


**authn-mongodb:sharded-cluster.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"name":"sha256","salt_position":"prefix"}`

  Options for password hash verification.


**authn-mongodb:sharded-cluster.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn-mongodb:sharded-cluster.mongo_type**

  *类型*: `sharded`

  *默认值*: `sharded`

  Sharded cluster模式。当 MongoDB 服务运行在 sharded 模式下，该配置必须设置为 'sharded'。


**authn-mongodb:sharded-cluster.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**authn-mongodb:sharded-cluster.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**authn-mongodb:sharded-cluster.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**authn-mongodb:sharded-cluster.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn-mongodb:sharded-cluster.username**

  *类型*: `string`

  内部数据库的用户名。


**authn-mongodb:sharded-cluster.password**

  *类型*: `string`

  内部数据库密码。


**authn-mongodb:sharded-cluster.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**authn-mongodb:sharded-cluster.database**

  *类型*: `string`

  数据库名字。


**authn-mongodb:sharded-cluster.topology**

  *类型*: `topology`


**authn-mongodb:sharded-cluster.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### 密码认证 - HTTP

#### GET 方法


使用 HTTP Server 作为认证服务的认证器的配置项 (使用 GET 请求)。

**authn-http:get.method**

  *类型*: `get`

  HTTP 请求方法。


**authn-http:get.headers**

  *类型*: `map`

  *默认值*: `{"accept":"application/json","cache-control":"no-cache","connection":"keep-alive","keep-alive":"timeout=30, max=1000"}`

  HTTP Headers 列表 (无 <code>content-type</code>) 。


**authn-http:get.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-http:get.backend**

  *类型*: `http`

  后端类型。


**authn-http:get.url**

  *类型*: `string`

  认证 HTTP 服务器地址。


**authn-http:get.body**

  *类型*: `#{term => binary()}`

  HTTP request body。


**authn-http:get.request_timeout**

  *类型*: `duration_ms`

  *默认值*: `5s`

  HTTP 请求超时时长。


**authn-http:get.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn-http:get.connect_timeout**

  *类型*: `duration_ms`

  *默认值*: `15s`

  连接HTTP服务器的超时时间。


**authn-http:get.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。


**authn-http:get.max_retries**

  *类型*: `non_neg_integer`

  Deprecated since 5.0.4.


**authn-http:get.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  连接池大小。


**authn-http:get.request**

  *类型*: `connector-http:request`

  设置 HTTP 请求的参数。


**authn-http:get.retry_interval**

  *类型*: `duration`

  Deprecated since 5.0.4.


**authn-http:get.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### POST 方法


使用 HTTP Server 作为认证服务的认证器的配置项 (使用 POST 请求)。

**authn-http:post.method**

  *类型*: `post`

  HTTP 请求方法。


**authn-http:post.headers**

  *类型*: `map`

  *默认值*: `{"accept":"application/json","cache-control":"no-cache","connection":"keep-alive","content-type":"application/json","keep-alive":"timeout=30, max=1000"}`

  HTTP Headers 列表


**authn-http:post.mechanism**

  *类型*: `password_based`

  认证机制。


**authn-http:post.backend**

  *类型*: `http`

  后端类型。


**authn-http:post.url**

  *类型*: `string`

  认证 HTTP 服务器地址。


**authn-http:post.body**

  *类型*: `#{term => binary()}`

  HTTP request body。


**authn-http:post.request_timeout**

  *类型*: `duration_ms`

  *默认值*: `5s`

  HTTP 请求超时时长。


**authn-http:post.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn-http:post.connect_timeout**

  *类型*: `duration_ms`

  *默认值*: `15s`

  连接HTTP服务器的超时时间。


**authn-http:post.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。


**authn-http:post.max_retries**

  *类型*: `non_neg_integer`

  Deprecated since 5.0.4.


**authn-http:post.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  连接池大小。


**authn-http:post.request**

  *类型*: `connector-http:request`

  设置 HTTP 请求的参数。


**authn-http:post.retry_interval**

  *类型*: `duration`

  Deprecated since 5.0.4.


**authn-http:post.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### JWT 认证


用于认证的 JWT 使用 HMAC 算法签发时的配置。

**authn-jwt:hmac-based.use_jwks**

  *类型*: `enum`

  *可选值*: `false`

  是否使用 JWKS。


**authn-jwt:hmac-based.algorithm**

  *类型*: `enum`

  *可选值*: `hmac-based`

  JWT 签名算法，支持 HMAC (配置为 <code>hmac-based</code>）和 RSA、ECDSA (配置为 <code>public-key</code>)。


**authn-jwt:hmac-based.secret**

  *类型*: `string`

  使用 HMAC 算法时用于验证 JWT 的密钥


**authn-jwt:hmac-based.secret_base64_encoded**

  *类型*: `boolean`

  *默认值*: `false`

  密钥是否为 Base64 编码。


**authn-jwt:hmac-based.mechanism**

  *类型*: `jwt`

  认证机制。


**authn-jwt:hmac-based.acl_claim_name**

  *类型*: `string`

  *默认值*: `acl`

  JWT claim name to use for getting ACL rules.


**authn-jwt:hmac-based.verify_claims**

  *类型*: `[term]`

  *默认值*: `{}`

  需要验证的自定义声明列表，它是一个名称/值对列表。
值可以使用以下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符
认证时将验证 JWT（取自 Password 字段）中 claims 的值是否与 <code>verify_claims</code> 中要求的相匹配。


**authn-jwt:hmac-based.from**

  *类型*: `enum`

  *默认值*: `password`

  *可选值*: `username | password`

  要从中获取 JWT 的字段。


**authn-jwt:hmac-based.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。




用于认证的 JWTs 需要从 JWKS 端点获取时的配置。

**authn-jwt:jwks.use_jwks**

  *类型*: `enum`

  *可选值*: `true`

  是否使用 JWKS。


**authn-jwt:jwks.endpoint**

  *类型*: `string`

  JWKS 端点， 它是一个以 JWKS 格式返回服务端的公钥集的只读端点。


**authn-jwt:jwks.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn-jwt:jwks.refresh_interval**

  *类型*: `integer`

  *默认值*: `300`

  JWKS 刷新间隔。


**authn-jwt:jwks.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  SSL 选项。


**authn-jwt:jwks.mechanism**

  *类型*: `jwt`

  认证机制。


**authn-jwt:jwks.acl_claim_name**

  *类型*: `string`

  *默认值*: `acl`

  JWT claim name to use for getting ACL rules.


**authn-jwt:jwks.verify_claims**

  *类型*: `[term]`

  *默认值*: `{}`

  需要验证的自定义声明列表，它是一个名称/值对列表。
值可以使用以下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符
认证时将验证 JWT（取自 Password 字段）中 claims 的值是否与 <code>verify_claims</code> 中要求的相匹配。


**authn-jwt:jwks.from**

  *类型*: `enum`

  *默认值*: `password`

  *可选值*: `username | password`

  要从中获取 JWT 的字段。


**authn-jwt:jwks.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。




用于认证的 JWT 使用 RSA 或 ECDSA 算法签发时的配置。

**authn-jwt:public-key.use_jwks**

  *类型*: `enum`

  *可选值*: `false`

  是否使用 JWKS。


**authn-jwt:public-key.algorithm**

  *类型*: `enum`

  *可选值*: `public-key`

  JWT 签名算法，支持 HMAC (配置为 <code>hmac-based</code>）和 RSA、ECDSA (配置为 <code>public-key</code>)。


**authn-jwt:public-key.public_key**

  *类型*: `string`

  用于验证 JWT 的公钥。


**authn-jwt:public-key.mechanism**

  *类型*: `jwt`

  认证机制。


**authn-jwt:public-key.acl_claim_name**

  *类型*: `string`

  *默认值*: `acl`

  JWT claim name to use for getting ACL rules.


**authn-jwt:public-key.verify_claims**

  *类型*: `[term]`

  *默认值*: `{}`

  需要验证的自定义声明列表，它是一个名称/值对列表。
值可以使用以下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符
认证时将验证 JWT（取自 Password 字段）中 claims 的值是否与 <code>verify_claims</code> 中要求的相匹配。


**authn-jwt:public-key.from**

  *类型*: `enum`

  *默认值*: `password`

  *可选值*: `username | password`

  要从中获取 JWT 的字段。


**authn-jwt:public-key.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。



### 增强认证


Settings for Salted Challenge Response Authentication Mechanism
(SCRAM) authentication.

**authn-scram-builtin_db:authentication.mechanism**

  *类型*: `scram`

  认证机制。


**authn-scram-builtin_db:authentication.backend**

  *类型*: `built_in_database`

  后端类型。


**authn-scram-builtin_db:authentication.algorithm**

  *类型*: `enum`

  *默认值*: `sha256`

  *可选值*: `sha256 | sha512`

  Hashing algorithm.


**authn-scram-builtin_db:authentication.iteration_count**

  *类型*: `non_neg_integer`

  *默认值*: `4096`

  Iteration count.


**authn-scram-builtin_db:authentication.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。



### PSK 认证


此配置用于启用 TLS-PSK 身份验证。

PSK 是 “Pre-Shared-Keys” 的缩写。

注意: 确保 SSL 监听器仅启用了 'tlsv1.2'，并且配置了PSK 密码套件，例如 'RSA-PSK-AES256-GCM-SHA384'。

可以通过查看监听器中的 SSL 选项，了解更多详细信息。

可以通过配置 'init_file' 来设置初始化的 ID 和 密钥

**psk_authentication.enable**

  *类型*: `boolean`

  *默认值*: `false`

  是否开启 TLS PSK 支持


**psk_authentication.init_file**

  *类型*: `string`

  如果设置了初始化文件，EMQX 将在启动时从初始化文件中导入 PSK 信息到内建数据库中。
这个文件需要按行进行组织，每一行必须遵守如下格式: <code>PSKIdentity:SharedSecret</code>
例如: <code>mydevice1:c2VjcmV0</code>


**psk_authentication.separator**

  *类型*: `string`

  *默认值*: `:`

  PSK 文件中 <code>PSKIdentity</code> 和 <code>SharedSecret</code> 之间的分隔符


**psk_authentication.chunk_size**

  *类型*: `integer`

  *默认值*: `50`

  将 PSK 文件导入到内建数据时每个块的大小



### 密码 Hash 配置


Settings for bcrypt password hashing algorithm.

**authn-hash:bcrypt.name**

  *类型*: `bcrypt`

  BCRYPT password hashing.




Settings for bcrypt password hashing algorithm (for DB backends with write capability).

**authn-hash:bcrypt_rw.name**

  *类型*: `bcrypt`

  BCRYPT password hashing.


**authn-hash:bcrypt_rw.salt_rounds**

  *类型*: `integer`

  *默认值*: `10`

  Salt rounds for BCRYPT password generation.




Settings for PBKDF2 password hashing algorithm.

**authn-hash:pbkdf2.name**

  *类型*: `pbkdf2`

  PBKDF2 password hashing.


**authn-hash:pbkdf2.mac_fun**

  *类型*: `enum`

  *可选值*: `md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512`

  Specifies mac_fun for PBKDF2 hashing algorithm.


**authn-hash:pbkdf2.iterations**

  *类型*: `integer`

  Iteration count for PBKDF2 hashing algorithm.


**authn-hash:pbkdf2.dk_length**

  *类型*: `integer`

  Derived length for PBKDF2 hashing algorithm. If not specified, calculated automatically based on `mac_fun`.




Settings for simple algorithms.

**authn-hash:simple.name**

  *类型*: `enum`

  *可选值*: `plain | md5 | sha | sha256 | sha512`

  Simple password hashing algorithm.


**authn-hash:simple.salt_position**

  *类型*: `enum`

  *默认值*: `prefix`

  *可选值*: `disable | prefix | suffix`

  Salt position for PLAIN, MD5, SHA, SHA256 and SHA512 algorithms.



## 授权

### 默认行为与缓存


授权相关

**authorization.no_match**

  *类型*: `enum`

  *默认值*: `allow`

  *可选值*: `allow | deny`

  如果用户或客户端不匹配ACL规则，或者从可配置授权源(比如内置数据库、HTTP API 或 PostgreSQL 等。)内未找
到此类用户或客户端时，模式的认访问控制操作。
在“授权”中查找更多详细信息。


**authorization.deny_action**

  *类型*: `enum`

  *默认值*: `ignore`

  *可选值*: `ignore | disconnect`

  授权检查拒绝操作时的操作。


**authorization.cache**

  *类型*: `broker:authz_cache`


**authorization.sources**

  *类型*: `array`

  *默认值*: `[]`

  授权数据源。<br/>
授权（ACL）数据源的列表。
它被设计为一个数组，而不是一个散列映射，
所以可以作为链式访问控制。<br/>

当授权一个 'publish' 或 'subscribe' 行为时，
该配置列表中的所有数据源将按顺序进行检查。
如果在某个客户端未找到时(使用 ClientID 或 Username)。
将会移动到下一个数据源。直至得到 'allow' 或 'deny' 的结果。<br/>

如果在任何数据源中都未找到对应的客户端信息。
配置的默认行为 ('authorization.no_match') 将生效。<br/>

注意：
数据源使用 'type' 进行标识。
使用同一类型的数据源多于一次不被允许。




Settings for the authorization cache.

**authorization.cache.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用或禁用授权缓存。


**authorization.cache.max_size**

  *类型*: `integer`

  *默认值*: `32`

  *可选值*: `1-1048576`

  缓存项的最大数量。


**authorization.cache.ttl**

  *类型*: `duration`

  *默认值*: `1m`

  缓存数据的生存时间。



### ACL File


使用静态文件授权

**authorization.sources.$INDEX.type**

  *类型*: `file`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.path**

  *类型*: `string`

  包含 ACL 规则的文件路径。
如果在启动 EMQX 节点前预先配置该路径，
那么可以将该文件置于任何 EMQX 可以访问到的位置。

如果从 EMQX Dashboard 或 HTTP API 创建或修改了规则集，
那么EMQX将会生成一个新的文件并将它存放在 `data_dir` 下的 `authz` 子目录中，
并从此弃用旧的文件。



### 内置数据库


使用内部数据库授权（mnesia）。

**authorization.sources.$INDEX.type**

  *类型*: `built_in_database`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源



### MySQL


使用 MySOL 数据库授权

**authorization.sources.$INDEX.type**

  *类型*: `mysql`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 MySQL 默认端口 3306。


**authorization.sources.$INDEX.database**

  *类型*: `string`

  数据库名字。


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authorization.sources.$INDEX.username**

  *类型*: `string`

  *默认值*: `root`

  内部数据库的用户名。


**authorization.sources.$INDEX.password**

  *类型*: `string`

  内部数据库密码。


**authorization.sources.$INDEX.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**authorization.sources.$INDEX.prepare_statement**

  *类型*: `map`

  SQL 预处理语句列表。


**authorization.sources.$INDEX.query**

  *类型*: `string`

  访问控制数据查询语句/查询命令。



### PostgreSQL


使用 PostgreSQL 数据库授权

**authorization.sources.$INDEX.type**

  *类型*: `postgresql`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 PostgreSQL 默认端口 5432。


**authorization.sources.$INDEX.database**

  *类型*: `string`

  数据库名字。


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authorization.sources.$INDEX.username**

  *类型*: `string`

  内部数据库的用户名。


**authorization.sources.$INDEX.password**

  *类型*: `string`

  内部数据库密码。


**authorization.sources.$INDEX.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**authorization.sources.$INDEX.prepare_statement**

  *类型*: `map`

  SQL 预处理语句列表。


**authorization.sources.$INDEX.query**

  *类型*: `string`

  访问控制数据查询语句/查询命令。



### Redis


使用 Redis 授权（单实例）。

**authorization.sources.$INDEX.type**

  *类型*: `redis`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**authorization.sources.$INDEX.redis_type**

  *类型*: `single`

  *默认值*: `single`

  单机模式。当 Redis 服务运行在单机模式下，该配置必须设置为 'single'。


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authorization.sources.$INDEX.password**

  *类型*: `string`

  内部数据库密码。


**authorization.sources.$INDEX.database**

  *类型*: `integer`

  *默认值*: `0`

  Redis 数据库 ID。


**authorization.sources.$INDEX.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**authorization.sources.$INDEX.cmd**

  *类型*: `string`

  访问控制数据查查询命令




使用 Redis 授权（集群模式）。

**authorization.sources.$INDEX.type**

  *类型*: `redis`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**authorization.sources.$INDEX.redis_type**

  *类型*: `cluster`

  *默认值*: `cluster`

  集群模式。当 Redis 服务运行在集群模式下，该配置必须设置为 'cluster'。


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authorization.sources.$INDEX.password**

  *类型*: `string`

  内部数据库密码。


**authorization.sources.$INDEX.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**authorization.sources.$INDEX.cmd**

  *类型*: `string`

  访问控制数据查查询命令




使用 Redis 授权（哨兵模式）。

**authorization.sources.$INDEX.type**

  *类型*: `redis`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**authorization.sources.$INDEX.redis_type**

  *类型*: `sentinel`

  *默认值*: `sentinel`

  哨兵模式。当 Redis 服务运行在哨兵模式下，该配置必须设置为 'sentinel'。


**authorization.sources.$INDEX.sentinel**

  *类型*: `string`

  Redis 哨兵模式下的集群名称。


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authorization.sources.$INDEX.password**

  *类型*: `string`

  内部数据库密码。


**authorization.sources.$INDEX.database**

  *类型*: `integer`

  *默认值*: `0`

  Redis 数据库 ID。


**authorization.sources.$INDEX.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**authorization.sources.$INDEX.cmd**

  *类型*: `string`

  访问控制数据查查询命令



### MongoDB


使用 MongoDB 授权（单实例）。

**authorization.sources.$INDEX.type**

  *类型*: `mongodb`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.collection**

  *类型*: `string`

  `MongoDB` 授权数据集。


**authorization.sources.$INDEX.filter**

  *类型*: `map`

  *默认值*: `{}`

  在查询中定义过滤条件的条件表达式。
过滤器支持如下占位符：<br/>
- <code>${username}</code>：将在运行时被替换为客户端连接时使用的用户名<br/>
- <code>${clientid}</code>：将在运行时被替换为客户端连接时使用的客户端标识符


**authorization.sources.$INDEX.mongo_type**

  *类型*: `single`

  *默认值*: `single`

  Standalone 模式。当 MongoDB 服务运行在 standalone 模式下，该配置必须设置为 'single'。


**authorization.sources.$INDEX.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**authorization.sources.$INDEX.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**authorization.sources.$INDEX.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authorization.sources.$INDEX.username**

  *类型*: `string`

  内部数据库的用户名。


**authorization.sources.$INDEX.password**

  *类型*: `string`

  内部数据库密码。


**authorization.sources.$INDEX.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**authorization.sources.$INDEX.database**

  *类型*: `string`

  数据库名字。


**authorization.sources.$INDEX.topology**

  *类型*: `topology`


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。




使用 MongoDB 授权（副本集模式）

**authorization.sources.$INDEX.type**

  *类型*: `mongodb`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.collection**

  *类型*: `string`

  `MongoDB` 授权数据集。


**authorization.sources.$INDEX.filter**

  *类型*: `map`

  *默认值*: `{}`

  在查询中定义过滤条件的条件表达式。
过滤器支持如下占位符：<br/>
- <code>${username}</code>：将在运行时被替换为客户端连接时使用的用户名<br/>
- <code>${clientid}</code>：将在运行时被替换为客户端连接时使用的客户端标识符


**authorization.sources.$INDEX.mongo_type**

  *类型*: `rs`

  *默认值*: `rs`

  Replica set模式。当 MongoDB 服务运行在 replica-set 模式下，该配置必须设置为 'rs'。


**authorization.sources.$INDEX.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**authorization.sources.$INDEX.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**authorization.sources.$INDEX.r_mode**

  *类型*: `enum`

  *默认值*: `master`

  *可选值*: `master | slave_ok`

  读模式。


**authorization.sources.$INDEX.replica_set_name**

  *类型*: `string`

  副本集的名称。


**authorization.sources.$INDEX.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authorization.sources.$INDEX.username**

  *类型*: `string`

  内部数据库的用户名。


**authorization.sources.$INDEX.password**

  *类型*: `string`

  内部数据库密码。


**authorization.sources.$INDEX.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**authorization.sources.$INDEX.database**

  *类型*: `string`

  数据库名字。


**authorization.sources.$INDEX.topology**

  *类型*: `topology`


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。




使用 MongoDB 授权（分片集群模式）。

**authorization.sources.$INDEX.type**

  *类型*: `mongodb`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.collection**

  *类型*: `string`

  `MongoDB` 授权数据集。


**authorization.sources.$INDEX.filter**

  *类型*: `map`

  *默认值*: `{}`

  在查询中定义过滤条件的条件表达式。
过滤器支持如下占位符：<br/>
- <code>${username}</code>：将在运行时被替换为客户端连接时使用的用户名<br/>
- <code>${clientid}</code>：将在运行时被替换为客户端连接时使用的客户端标识符


**authorization.sources.$INDEX.mongo_type**

  *类型*: `sharded`

  *默认值*: `sharded`

  Sharded cluster模式。当 MongoDB 服务运行在 sharded 模式下，该配置必须设置为 'sharded'。


**authorization.sources.$INDEX.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**authorization.sources.$INDEX.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**authorization.sources.$INDEX.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authorization.sources.$INDEX.username**

  *类型*: `string`

  内部数据库的用户名。


**authorization.sources.$INDEX.password**

  *类型*: `string`

  内部数据库密码。


**authorization.sources.$INDEX.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**authorization.sources.$INDEX.database**

  *类型*: `string`

  数据库名字。


**authorization.sources.$INDEX.topology**

  *类型*: `topology`


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### HTTP


使用外部 HTTP 服务器授权(GET 请求)。

**authorization.sources.$INDEX.type**

  *类型*: `http`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.url**

  *类型*: `string`

  授权 HTTP 服务器地址。


**authorization.sources.$INDEX.request_timeout**

  *类型*: `string`

  *默认值*: `30s`

  HTTP 请求超时时长。


**authorization.sources.$INDEX.body**

  *类型*: `map`

  HTTP request body。


**authorization.sources.$INDEX.connect_timeout**

  *类型*: `duration_ms`

  *默认值*: `15s`

  连接HTTP服务器的超时时间。


**authorization.sources.$INDEX.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。


**authorization.sources.$INDEX.max_retries**

  *类型*: `non_neg_integer`

  Deprecated since 5.0.4.


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  连接池大小。


**authorization.sources.$INDEX.request**

  *类型*: `connector-http:request`

  设置 HTTP 请求的参数。


**authorization.sources.$INDEX.retry_interval**

  *类型*: `duration`

  Deprecated since 5.0.4.


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**authorization.sources.$INDEX.method**

  *类型*: `get`

  HTTP 请求方法


**authorization.sources.$INDEX.headers**

  *类型*: `[{binary, binary()}]`

  *默认值*: `{"accept":"application/json","cache-control":"no-cache","connection":"keep-alive","keep-alive":"timeout=30, max=1000"}`

  HTTP Headers 列表 (无 <code>content-type</code>) 。



使用外部 HTTP 服务器授权(POST 请求)。

**authorization.sources.$INDEX.type**

  *类型*: `http`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.url**

  *类型*: `string`

  授权 HTTP 服务器地址。


**authorization.sources.$INDEX.request_timeout**

  *类型*: `string`

  *默认值*: `30s`

  HTTP 请求超时时长。


**authorization.sources.$INDEX.body**

  *类型*: `map`

  HTTP request body。


**authorization.sources.$INDEX.connect_timeout**

  *类型*: `duration_ms`

  *默认值*: `15s`

  连接HTTP服务器的超时时间。


**authorization.sources.$INDEX.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。


**authorization.sources.$INDEX.max_retries**

  *类型*: `non_neg_integer`

  Deprecated since 5.0.4.


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  连接池大小。


**authorization.sources.$INDEX.request**

  *类型*: `connector-http:request`

  设置 HTTP 请求的参数。


**authorization.sources.$INDEX.retry_interval**

  *类型*: `duration`

  Deprecated since 5.0.4.


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**authorization.sources.$INDEX.method**

  *类型*: `post`

  HTTP 请求方法


**authorization.sources.$INDEX.headers**

  *类型*: `[{binary, binary()}]`

  *默认值*: `{"accept":"application/json","cache-control":"no-cache","connection":"keep-alive","content-type":"application/json","keep-alive":"timeout=30, max=1000"}`

  HTTP Headers 列表



## 事件主题


Enable or disable client lifecycle event publishing.

The following options affect MQTT clients as well as
gateway clients. The types of the clients
are distinguished by the topic prefix:

- For the MQTT clients, the format is:
`$SYS/broker/<node>/clients/<clientid>/<event>`
- For the Gateway clients, it is
`$SYS/broker/<node>/gateway/<gateway-name>/clients/<clientid>/<event>`


**sys_topics.sys_event_messages.client_connected**

  *类型*: `boolean`

  *默认值*: `true`

  是否开启客户端已连接事件消息。


**sys_topics.sys_event_messages.client_disconnected**

  *类型*: `boolean`

  *默认值*: `true`

  是否开启客户端已断开连接事件消息。


**sys_topics.sys_event_messages.client_subscribed**

  *类型*: `boolean`

  *默认值*: `false`

  是否开启客户端已成功订阅主题事件消息。


**sys_topics.sys_event_messages.client_unsubscribed**

  *类型*: `boolean`

  *默认值*: `false`

  是否开启客户端已成功取消订阅主题事件消息。



## 规则引擎


配置 EMQX 规则引擎。

**rule_engine.ignore_sys_message**

  *类型*: `boolean`

  *默认值*: `true`

  当设置为“true”（默认）时，规则引擎将忽略发布到 $SYS 主题的消息。


**rule_engine.rules**

  *类型*: `id`

  *默认值*: `{}`

  规则


**rule_engine.jq_function_default_timeout**

  *类型*: `duration_ms`

  *默认值*: `10s`

  规则引擎内建函数 `jq` 默认时间限制


**rule_engine.jq_implementation_module**

  *类型*: `enum`

  *默认值*: `jq_nif`

  *可选值*: `jq_nif | jq_port`

  jq 规则引擎功能的实现模块。可用的两个选项是 jq_nif 和 jq_port。jq_nif 使用 Erlang NIF 库访问 jq 库，而 jq_port 使用基于 Erlang Port 的实现。jq_nif 方式（默认选项）是这两个选项中最快的实现，但 jq_port 方式更安全，因为这种情况下 jq 程序不会在 Erlang VM 进程中执行。




配置规则

**rule_engine.rules.$id.name**

  *类型*: `string`

  *默认值*: `""`

  规则名字


**rule_engine.rules.$id.sql**

  *类型*: `string`

  用于处理消息的 SQL 。
示例：<code>SELECT * FROM "test/topic" WHERE payload.x = 1</code>


**rule_engine.rules.$id.actions**

  *类型*: `array`

  *默认值*: `[]`

  规则的动作列表。
动作可以是指向 EMQX bridge 的引用，也可以是一个指向函数的对象。
我们支持一些内置函数，如“republish”和“console”，我们还支持用户提供的函数，它的格式为：“{module}:{function}”。
列表中的动作按顺序执行。这意味着如果其中一个动作执行缓慢，则以下所有动作都不会被执行直到它返回。
如果其中一个动作崩溃，在它之后的所有动作仍然会被按照原始顺序执行。
如果运行动作时出现任何错误，则会出现错误消息，并且相应的计数器会增加。


**rule_engine.rules.$id.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用或禁用规则引擎


**rule_engine.rules.$id.description**

  *类型*: `string`

  *默认值*: `""`

  规则的描述


**rule_engine.rules.$id.metadata**

  *类型*: `map`

  规则的元数据，不要手动修改




配置用户函数

**rule_engine.rules.$id.actions.$INDEX.function**

  *类型*: `string`

  用户提供的函数。 格式应为：'{module}:{function}'。
其中 {module} 是 Erlang 回调模块， {function} 是 Erlang 函数。
要编写自己的函数，请检查源文件：<code>apps/emqx_rule_engine/src/emqx_rule_actions.erl</code> 中的示例函数 <code>console</code> 和<code>republish</code> 。


**rule_engine.rules.$id.actions.$INDEX.args**

  *类型*: `map`

  *默认值*: `{}`

  用户提供的参数将作为函数 module:function/3 的第三个参数，
请检查源文件：<code>apps/emqx_rule_engine/src/emqx_rule_actions.erl</code> 中的示例函数 <code>console</code> 和<code>republish</code> 。



### 规则动作


配置打印到控制台

**rule_engine.rules.$id.actions.$INDEX.function**

  *类型*: `console`

  将输出打印到控制台




配置重新发布。

**rule_engine.rules.$id.actions.$INDEX.function**

  *类型*: `republish`

  将消息重新发布为新的 MQTT 消息


**rule_engine.rules.$id.actions.$INDEX.args**

  *类型*: `rule_engine:republish_args`

  *默认值*: `{}`




内置 'republish' 动作的参数。
可以在参数中使用变量。
变量是规则中选择的字段。 例如规则 SQL 定义如下：
<code>
    SELECT clientid, qos, payload FROM "t/1"
</code>
然后有 3 个变量可用：<code>clientid</code>、<code>qos</code> 和 <code>payload</code>。 如果我们将参数设置为：
<code>
    {
        topic = "t/${clientid}"
        qos = "${qos}"
        payload = "msg: ${payload}"
    }
</code>
当收到一条消息 payload = `hello`, qos = 1, clientid = `Steve` 时，将重新发布一条新的 MQTT 消息到主题 `t/Steve`
消息内容为 payload = `msg: hello`, and `qos = 1

**rule_engine.rules.$id.actions.$INDEX.args.topic**

  *类型*: `string`

  重新发布消息的目标主题。
允许使用带有变量的模板，请参阅“republish_args”的描述。


**rule_engine.rules.$id.actions.$INDEX.args.qos**

  *类型*: `qos | string`

  *默认值*: `${qos}`

  要重新发布的消息的 qos。允许使用带有变量的模板，请参阅“republish_args”的描述。
默认为 ${qos}。 如果从规则的选择结果中没有找到变量 ${qos}，则使用 0。


**rule_engine.rules.$id.actions.$INDEX.args.retain**

  *类型*: `boolean | string`

  *默认值*: `${retain}`

  要重新发布的消息的“保留”标志。允许使用带有变量的模板，请参阅“republish_args”的描述。
默认为 ${retain}。 如果从所选结果中未找到变量 ${retain}，则使用 false。


**rule_engine.rules.$id.actions.$INDEX.args.payload**

  *类型*: `string`

  *默认值*: `${payload}`

  要重新发布的消息的有效负载。允许使用带有变量的模板，请参阅“republish_args”的描述。
默认为 ${payload}。 如果从所选结果中未找到变量 ${payload}，则使用字符串 "undefined"。


**rule_engine.rules.$id.actions.$INDEX.args.user_properties**

  *类型*: `string`

  *默认值*: `${user_properties}`

  指定使用哪个变量来填充 MQTT 消息的 User-Property 列表。这个变量的值必须是一个 map 类型。
可以设置成 <code>${pub_props.'User-Property'}</code> 或者
使用 <code>SELECT *,pub_props.'User-Property' as user_properties</code> 来把源 MQTT 消息
的 User-Property 列表用于填充。
也可以使用 <code>map_put</code> 函数来添加新的 User-Property，
<code>map_put('my-prop-name', 'my-prop-value', user_properties) as user_properties</code>
注意：MQTT 协议允许一个消息中出现多次同一个 property 名，但是 EMQX 的规则引擎不允许。



## 数据桥接

### MQTT


MQTT Bridge 的配置。

**bridges.mqtt.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.mqtt.$name.resource_opts**

  *类型*: `bridge_mqtt:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.mqtt.$name.mode**

  *类型*: `enum`

  *默认值*: `cluster_shareload`

  *可选值*: `cluster_shareload`

  MQTT 桥的模式。 <br/>
- cluster_shareload：在 emqx 集群的每个节点上创建一个 MQTT 连接。<br/>
在“cluster_shareload”模式下，来自远程代理的传入负载通过共享订阅的方式接收。<br/>
请注意，<code>clientid</code> 以节点名称为后缀，这是为了避免不同节点之间的 <code> clientid</code> 冲突。
而且对于入口连接的 <code>remote.topic</code>，我们只能使用共享订阅主题过滤器。


**bridges.mqtt.$name.server**

  *类型*: `string`

  远程 MQTT Broker的主机和端口。


**bridges.mqtt.$name.clientid_prefix**

  *类型*: `string`

  可选的前缀，用于在出口网桥使用的clientid前加上前缀。


**bridges.mqtt.$name.reconnect_interval**

  *类型*: `string`

  Deprecated since v5.0.16.


**bridges.mqtt.$name.proto_ver**

  *类型*: `enum`

  *默认值*: `v4`

  *可选值*: `v3 | v4 | v5`

  MQTT 协议版本


**bridges.mqtt.$name.bridge_mode**

  *类型*: `boolean`

  *默认值*: `false`

  是否启用 Bridge Mode。
注意：此设置只针对 MQTT 协议版本 < 5.0 有效，并且需要远程 MQTT Broker 支持 Bridge Mode。
如果设置为 true ，桥接会告诉远端服务器当前连接是一个桥接而不是一个普通的客户端。
这意味着消息回环检测会更加高效，并且远端服务器收到的保留消息的标志位会透传给本地。


**bridges.mqtt.$name.username**

  *类型*: `string`

  MQTT 协议的用户名


**bridges.mqtt.$name.password**

  *类型*: `string`

  MQTT 协议的密码


**bridges.mqtt.$name.clean_start**

  *类型*: `boolean`

  *默认值*: `true`

  与 ingress MQTT 桥的远程服务器重连时是否清除老的 MQTT 会话。


**bridges.mqtt.$name.keepalive**

  *类型*: `string`

  *默认值*: `300s`

  MQTT Keepalive. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
- `s` for seconds,
- `m` for minutes,
- `h` for hours;
<br/>or combination of whereof: `1h5m0s`


**bridges.mqtt.$name.retry_interval**

  *类型*: `string`

  *默认值*: `15s`

  Message retry interval. Delay for the MQTT bridge to retry sending the QoS1/QoS2 messages in case of ACK not received. Time interval is a string that contains a number followed by time unit:<br/>- `ms` for milliseconds,
- `s` for seconds,
- `m` for minutes,
- `h` for hours;
<br/>or combination of whereof: `1h5m0s`


**bridges.mqtt.$name.max_inflight**

  *类型*: `non_neg_integer`

  *默认值*: `32`

  MQTT 协议的最大飞行（已发送但未确认）消息


**bridges.mqtt.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.mqtt.$name.ingress**

  *类型*: `connector-mqtt:ingress`

  入口配置定义了该桥接如何从远程 MQTT Broker 接收消息，然后将消息发送到本地 Broker。<br/>
        以下字段中允许使用带有变量的模板：'remote.qos', 'local.topic', 'local.qos', 'local.retain', 'local.payload'。<br/>
        注意：如果此桥接被用作规则的输入，并且配置了 'local.topic'，则从远程代理获取的消息将同时被发送到 'local.topic' 和规则。


**bridges.mqtt.$name.egress**

  *类型*: `connector-mqtt:egress`

  出口配置定义了该桥接如何将消息从本地 Broker 转发到远程 Broker。
以下字段中允许使用带有变量的模板：'remote.topic', 'local.qos', 'local.retain', 'local.payload'。<br/>
注意：如果此桥接被用作规则的动作，并且配置了 'local.topic'，则从规则输出的数据以及匹配到 'local.topic' 的 MQTT 消息都会被转发。




资源启动相关的选项。

**bridges.mqtt.$name.resource_opts.worker_pool_size**

  *类型*: `non_neg_integer`

  *默认值*: `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.mqtt.$name.resource_opts.health_check_interval**

  *类型*: `duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.mqtt.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.mqtt.$name.resource_opts.start_timeout**

  *类型*: `duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.mqtt.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  *默认值*: `60s`

  资源断开以后，自动重连的时间间隔。


**bridges.mqtt.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.mqtt.$name.resource_opts.request_timeout**

  *类型*: `infinity | duration_ms`

  *默认值*: `15s`

  从请求进入缓冲区开始计时，如果请求在规定的时间内仍停留在缓冲区内或者已发送但未能及时收到响应或确认，该请求将被视为过期。


**bridges.mqtt.$name.resource_opts.async_inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  异步请求飞行队列窗口大小。


**bridges.mqtt.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.mqtt.$name.resource_opts.max_queue_bytes**

  *类型*: `bytesize`

  *默认值*: `100MB`

  每个缓存 worker 允许使用的最大字节数。



### WebHook


HTTP Bridge 配置

**bridges.webhook.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.webhook.$name.resource_opts**

  *类型*: `bridge_webhook:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.webhook.$name.connect_timeout**

  *类型*: `duration_ms`

  *默认值*: `15s`

  连接HTTP服务器的超时时间。


**bridges.webhook.$name.retry_interval**

  *类型*: `duration`

  Deprecated since 5.0.4.


**bridges.webhook.$name.pool_type**

  *类型*: `emqx_connector_http:pool_type`

  *默认值*: `random`

  连接池的类型，可用类型有`random`, `hash`。


**bridges.webhook.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  连接池大小。


**bridges.webhook.$name.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。


**bridges.webhook.$name.request**

  *类型*: `connector-http:request`

  设置 HTTP 请求的参数。


**bridges.webhook.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.webhook.$name.url**

  *类型*: `string`

  HTTP Bridge 的 URL。<br/>
路径中允许使用带变量的模板，但是 host， port 不允许使用变量模板。<br/>
例如，<code> http://localhost:9901/${topic} </code> 是允许的，
但是<code> http://${host}:9901/message </code>
或 <code> http://localhost:${port}/message </code>
不允许。


**bridges.webhook.$name.direction**

  *类型*: `egress`

  Deprecated since 5.0.12.


**bridges.webhook.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 HTTP 服务器。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 HTTP 服务器。


**bridges.webhook.$name.method**

  *类型*: `enum`

  *默认值*: `post`

  *可选值*: `post | put | get | delete`

  HTTP 请求的方法。 所有可用的方法包括：post、put、get、delete。<br/>
允许使用带有变量的模板。


**bridges.webhook.$name.headers**

  *类型*: `map`

  *默认值*: `{"accept":"application/json","cache-control":"no-cache","connection":"keep-alive","content-type":"application/json","keep-alive":"timeout=5"}`

  HTTP 请求的标头。<br/>
允许使用带有变量的模板。


**bridges.webhook.$name.body**

  *类型*: `string`

  HTTP 请求的正文。<br/>
如果没有设置该字段，请求正文将是包含所有可用字段的 JSON object。<br/>
如果该 webhook 是由于收到 MQTT 消息触发的，'所有可用字段' 将是 MQTT 消息的
上下文信息；如果该 webhook 是由于规则触发的，'所有可用字段' 则为触发事件的上下文信息。<br/>
允许使用带有变量的模板。


**bridges.webhook.$name.max_retries**

  *类型*: `non_neg_integer`

  *默认值*: `2`

  HTTP 请求失败最大重试次数


**bridges.webhook.$name.request_timeout**

  *类型*: `duration_ms`

  *默认值*: `15s`

  HTTP 请求超时




资源启动相关的选项。

**bridges.webhook.$name.resource_opts.worker_pool_size**

  *类型*: `non_neg_integer`

  *默认值*: `16`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.webhook.$name.resource_opts.health_check_interval**

  *类型*: `duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.webhook.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.webhook.$name.resource_opts.start_timeout**

  *类型*: `duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.webhook.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  *默认值*: `60s`

  资源断开以后，自动重连的时间间隔。


**bridges.webhook.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.webhook.$name.resource_opts.request_timeout**

  *类型*: `infinity | duration_ms`

  *默认值*: `15s`

  从请求进入缓冲区开始计时，如果请求在规定的时间内仍停留在缓冲区内或者已发送但未能及时收到响应或确认，该请求将被视为过期。


**bridges.webhook.$name.resource_opts.async_inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  异步请求飞行队列窗口大小。


**bridges.webhook.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.webhook.$name.resource_opts.max_queue_bytes**

  *类型*: `bytesize`

  *默认值*: `100MB`

  每个缓存 worker 允许使用的最大字节数。



### 连接配置




**connector-http:request.method**

  *类型*: `string`

  HTTP 请求方法。


**connector-http:request.path**

  *类型*: `string`

  HTTP请求路径。


**connector-http:request.body**

  *类型*: `string`

  HTTP请求报文主体。


**connector-http:request.headers**

  *类型*: `map`

  HTTP 头字段列表。


**connector-http:request.max_retries**

  *类型*: `non_neg_integer`

  请求出错时的最大重试次数。


**connector-http:request.request_timeout**

  *类型*: `duration_ms`

  HTTP 请求超时。




出口配置定义了该桥接如何将消息从本地 Broker 转发到远程 Broker。
以下字段中允许使用带有变量的模板：'remote.topic', 'local.qos', 'local.retain', 'local.payload'。<br/>
注意：如果此桥接被用作规则的动作，并且配置了 'local.topic'，则从规则输出的数据以及匹配到 'local.topic' 的 MQTT 消息都会被转发。

**bridges.mqtt.$name.egress.local**

  *类型*: `connector-mqtt:egress_local`

  如何从本地 Broker 接收消息相关的配置。


**bridges.mqtt.$name.egress.remote**

  *类型*: `connector-mqtt:egress_remote`

  发送消息到远程 Broker 相关的配置。




如何从本地 Broker 接收消息相关的配置。

**bridges.mqtt.$name.egress.local.topic**

  *类型*: `string`

  要转发到远程broker的本地主题




发送消息到远程 Broker 相关的配置。

**bridges.mqtt.$name.egress.remote.topic**

  *类型*: `string`

  转发到远程broker的哪个topic。<br/>
允许使用带有变量的模板。


**bridges.mqtt.$name.egress.remote.qos**

  *类型*: `qos | string`

  *默认值*: `1`

  待发送 MQTT 消息的 QoS。<br/>
允许使用带有变量的模板。


**bridges.mqtt.$name.egress.remote.retain**

  *类型*: `boolean | string`

  *默认值*: `false`

  要发送的 MQTT 消息的“保留”标志。<br/>
允许使用带有变量的模板。


**bridges.mqtt.$name.egress.remote.payload**

  *类型*: `string`

  要发送的 MQTT 消息的负载。<br/>
允许使用带有变量的模板。




入口配置定义了该桥接如何从远程 MQTT Broker 接收消息，然后将消息发送到本地 Broker。<br/>
        以下字段中允许使用带有变量的模板：'remote.qos', 'local.topic', 'local.qos', 'local.retain', 'local.payload'。<br/>
        注意：如果此桥接被用作规则的输入，并且配置了 'local.topic'，则从远程代理获取的消息将同时被发送到 'local.topic' 和规则。

**bridges.mqtt.$name.ingress.remote**

  *类型*: `connector-mqtt:ingress_remote`

  订阅远程 Broker 相关的配置。


**bridges.mqtt.$name.ingress.local**

  *类型*: `connector-mqtt:ingress_local`

  发送消息到本地 Broker 相关的配置。




发送消息到本地 Broker 相关的配置。

**bridges.mqtt.$name.ingress.local.topic**

  *类型*: `string`

  向本地broker的哪个topic发送消息。<br/>
允许使用带有变量的模板。


**bridges.mqtt.$name.ingress.local.qos**

  *类型*: `qos | string`

  *默认值*: `${qos}`

  待发送 MQTT 消息的 QoS。<br/>
允许使用带有变量的模板。


**bridges.mqtt.$name.ingress.local.retain**

  *类型*: `boolean | string`

  *默认值*: `${retain}`

  要发送的 MQTT 消息的“保留”标志。<br/>
允许使用带有变量的模板。


**bridges.mqtt.$name.ingress.local.payload**

  *类型*: `string`

  要发送的 MQTT 消息的负载。<br/>
允许使用带有变量的模板。




订阅远程 Broker 相关的配置。

**bridges.mqtt.$name.ingress.remote.topic**

  *类型*: `string`

  从远程broker的哪个topic接收消息


**bridges.mqtt.$name.ingress.remote.qos**

  *类型*: `qos`

  *默认值*: `1`

  订阅远程borker时要使用的 QoS 级别



## 网关


EMQX Gateway configuration root.

**gateway.stomp**

  *类型*: `gateway:stomp`

  Stomp 网关配置。当前实现支持 v1.2/1.1/1.0 协议版本


**gateway.mqttsn**

  *类型*: `gateway:mqttsn`

  MQTT-SN 网关配置。当前实现仅支持 v1.2 版本


**gateway.coap**

  *类型*: `gateway:coap`

  CoAP 网关配置。
该网关的实现基于 RFC-7252 和 https://core-wg.github.io/coap-pubsub/draft-ietf-core-pubsub.html


**gateway.lwm2m**

  *类型*: `gateway:lwm2m`

  LwM2M 网关配置。仅支持 v1.0.1 协议。


**gateway.exproto**

  *类型*: `gateway:exproto`

  ExProto 网关




ClientInfo override.

**gateway:clientinfo_override.username**

  *类型*: `string`

  username 重写模板


**gateway:clientinfo_override.password**

  *类型*: `string`

  password 重写模板


**gateway:clientinfo_override.clientid**

  *类型*: `string`

  clientid 重写模板




MQTT topic that corresponds to a particular type of event.

**gateway:translator.topic**

  *类型*: `string`

  主题名称


**gateway:translator.qos**

  *类型*: `qos`

  *默认值*: `0`

  QoS 等级



### CoAP


The CoAP protocol gateway provides EMQX with the access capability of the CoAP protocol.
It allows publishing, subscribing, and receiving messages to EMQX in accordance
with a certain defined CoAP message format.

**gateway.coap.heartbeat**

  *类型*: `emqx_gateway_schema:duration`

  *默认值*: `30s`

  CoAP 网关要求客户端的最小心跳间隔时间。
当 <code>connection_required</code> 开启后，该参数用于检查客户端连接是否存活


**gateway.coap.connection_required**

  *类型*: `boolean`

  *默认值*: `false`

  是否开启连接模式。
连接模式是非标准协议的功能。它维护 CoAP 客户端上线、认证、和连接状态的保持


**gateway.coap.notify_type**

  *类型*: `enum`

  *默认值*: `qos`

  *可选值*: `non | con | qos`

  投递给 CoAP 客户端的通知消息类型。当客户端 Observe 一个资源（或订阅某个主题）时，网关会向客户端推送新产生的消息。其消息类型可设置为：<br/>
  - non: 不需要客户端返回确认消息;<br/>
  - con: 需要客户端返回一个确认消息;<br/>
  - qos: 取决于消息的 QoS 等级; QoS 0 会以 `non` 类型下发，QoS 1/2 会以 `con` 类型下发


**gateway.coap.subscribe_qos**

  *类型*: `enum`

  *默认值*: `coap`

  *可选值*: `qos0 | qos1 | qos2 | coap`

  客户端订阅请求的默认 QoS 等级。
当 CoAP 客户端发起订阅请求时，如果未携带 `qos` 参数则会使用该默认值。默认值可设置为：<br/>
  - qos0、 qos1、qos2: 设置为固定的 QoS 等级<br/>
  - coap: 依据订阅操作的 CoAP 报文类型来动态决定<br/>
    * 当订阅请求为 `non-confirmable` 类型时，取值为 qos0<br/>
    * 当订阅请求为 `confirmable` 类型时，取值为 qos1


**gateway.coap.publish_qos**

  *类型*: `enum`

  *默认值*: `coap`

  *可选值*: `qos0 | qos1 | qos2 | coap`

  客户端发布请求的默认 QoS 等级。
当 CoAP 客户端发起发布请求时，如果未携带 `qos` 参数则会使用该默认值。默认值可设置为：<br>
  - qos0、qos1、qos2: 设置为固定的 QoS 等级<br/>
  - coap: 依据发布操作的 CoAP 报文类型来动态决定<br/>
    * 当发布请求为 `non-confirmable` 类型时，取值为 qos0<br/>
    * 当发布请求为 `confirmable` 类型时，取值为 qos1


**gateway.coap.mountpoint**

  *类型*: `string`

  *默认值*: `""`


**gateway.coap.listeners**

  *类型*: `gateway:udp_listeners`

  配置 UDP 类型的监听器。


**gateway.coap.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用该网关


**gateway.coap.enable_stats**

  *类型*: `boolean`

  *默认值*: `true`

  是否开启客户端统计


**gateway.coap.idle_timeout**

  *类型*: `emqx_gateway_schema:duration`

  *默认值*: `30s`

  客户端连接过程的空闲时间。该配置用于：
  1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
  2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。


**gateway.coap.clientinfo_override**

  *类型*: `gateway:clientinfo_override`

  ClientInfo 重写。


**gateway.coap.authentication**

  *类型*: [authn-builtin_db:authentication](#authn-builtin_db:authentication) | [authn-mysql:authentication](#authn-mysql:authentication) | [authn-postgresql:authentication](#authn-postgresql:authentication) | [authn-mongodb:standalone](#authn-mongodb:standalone) | [authn-mongodb:replica-set](#authn-mongodb:replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb:sharded-cluster) | [authn-redis:standalone](#authn-redis:standalone) | [authn-redis:cluster](#authn-redis:cluster) | [authn-redis:sentinel](#authn-redis:sentinel) | [authn-http:get](#authn-http:get) | [authn-http:post](#authn-http:post) | [authn-jwt:hmac-based](#authn-jwt:hmac-based) | [authn-jwt:public-key](#authn-jwt:public-key) | [authn-jwt:jwks](#authn-jwt:jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db:authentication)

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段。



### ExProto


Settings for EMQX extension protocol (exproto).

**gateway.exproto.server**

  *类型*: `gateway:exproto_grpc_server`

  配置 ExProto 网关需要启动的 <code>ConnectionAdapter</code> 服务。
该服务用于提供客户端的认证、发布、订阅和数据下行等功能。


**gateway.exproto.handler**

  *类型*: `gateway:exproto_grpc_handler`

  配置 ExProto 网关需要请求的 <code>ConnectionHandler</code> 服务地址。
该服务用于给 ExProto 提供客户端的 Socket 事件处理、字节解码、订阅消息接收等功能。


**gateway.exproto.mountpoint**

  *类型*: `string`

  *默认值*: `""`


**gateway.exproto.listeners**

  *类型*: `gateway:tcp_udp_listeners`

  监听器配置。


**gateway.exproto.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用该网关


**gateway.exproto.enable_stats**

  *类型*: `boolean`

  *默认值*: `true`

  是否开启客户端统计


**gateway.exproto.idle_timeout**

  *类型*: `emqx_gateway_schema:duration`

  *默认值*: `30s`

  客户端连接过程的空闲时间。该配置用于：
  1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
  2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。


**gateway.exproto.clientinfo_override**

  *类型*: `gateway:clientinfo_override`

  ClientInfo 重写。


**gateway.exproto.authentication**

  *类型*: [authn-builtin_db:authentication](#authn-builtin_db:authentication) | [authn-mysql:authentication](#authn-mysql:authentication) | [authn-postgresql:authentication](#authn-postgresql:authentication) | [authn-mongodb:standalone](#authn-mongodb:standalone) | [authn-mongodb:replica-set](#authn-mongodb:replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb:sharded-cluster) | [authn-redis:standalone](#authn-redis:standalone) | [authn-redis:cluster](#authn-redis:cluster) | [authn-redis:sentinel](#authn-redis:sentinel) | [authn-http:get](#authn-http:get) | [authn-http:post](#authn-http:post) | [authn-jwt:hmac-based](#authn-jwt:hmac-based) | [authn-jwt:public-key](#authn-jwt:public-key) | [authn-jwt:jwks](#authn-jwt:jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db:authentication)

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段。




Settings for the exproto gRPC connection handler.

**gateway.exproto.handler.address**

  *类型*: `string`

  对端 gRPC 服务器地址。


**gateway.exproto.handler.ssl_options**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  gRPC 客户端的 SSL 配置。




Settings for the exproto gRPC server.

**gateway.exproto.server.bind**

  *类型*: [emqx_gateway_schema:ip_port()](#emqx_gateway_schema:ip_port()) | integer

  服务监听地址和端口。


**gateway.exproto.server.ssl_options**

  *类型*: `gateway:ssl_server_opts`

  服务 SSL 配置。



### LwM2M


The LwM2M protocol gateway.

**gateway.lwm2m.xml_dir**

  *类型*: `string`

  LwM2M Resource 定义的 XML 文件目录路径。


**gateway.lwm2m.lifetime_min**

  *类型*: `emqx_gateway_schema:duration`

  *默认值*: `15s`

  允许 LwM2M 客户端允许设置的心跳最小值。


**gateway.lwm2m.lifetime_max**

  *类型*: `emqx_gateway_schema:duration`

  *默认值*: `86400s`

  允许 LwM2M 客户端允许设置的心跳最大值。


**gateway.lwm2m.qmode_time_window**

  *类型*: `emqx_gateway_schema:duration_s`

  *默认值*: `22s`

  在QMode模式下，LwM2M网关认为网络链接有效的时间窗口的值。
例如，在收到客户端的更新信息后，在这个时间窗口内的任何信息都会直接发送到LwM2M客户端，而超过这个时间窗口的所有信息都会暂时储存在内存中。


**gateway.lwm2m.auto_observe**

  *类型*: `boolean`

  *默认值*: `false`

  自动 Observe REGISTER 数据包的 Object 列表。


**gateway.lwm2m.update_msg_publish_condition**

  *类型*: `enum`

  *默认值*: `contains_object_list`

  *可选值*: `always | contains_object_list`

  发布UPDATE事件消息的策略。<br/>
  - always: 只要收到 UPDATE 请求，就发送更新事件。<br/>
  - contains_object_list: 仅当 UPDATE 请求携带 Object 列表时才发送更新事件。


**gateway.lwm2m.translators**

  *类型*: `gateway:lwm2m_translators`

  LwM2M 网关订阅/发布消息的主题映射配置。


**gateway.lwm2m.mountpoint**

  *类型*: `string`

  *默认值*: `lwm2m/${endpoint_name}/`


**gateway.lwm2m.listeners**

  *类型*: `gateway:udp_listeners`

  配置 UDP 类型的监听器。


**gateway.lwm2m.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用该网关


**gateway.lwm2m.enable_stats**

  *类型*: `boolean`

  *默认值*: `true`

  是否开启客户端统计


**gateway.lwm2m.idle_timeout**

  *类型*: `emqx_gateway_schema:duration`

  *默认值*: `30s`

  客户端连接过程的空闲时间。该配置用于：
  1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
  2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。


**gateway.lwm2m.clientinfo_override**

  *类型*: `gateway:clientinfo_override`

  ClientInfo 重写。


**gateway.lwm2m.authentication**

  *类型*: [authn-builtin_db:authentication](#authn-builtin_db:authentication) | [authn-mysql:authentication](#authn-mysql:authentication) | [authn-postgresql:authentication](#authn-postgresql:authentication) | [authn-mongodb:standalone](#authn-mongodb:standalone) | [authn-mongodb:replica-set](#authn-mongodb:replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb:sharded-cluster) | [authn-redis:standalone](#authn-redis:standalone) | [authn-redis:cluster](#authn-redis:cluster) | [authn-redis:sentinel](#authn-redis:sentinel) | [authn-http:get](#authn-http:get) | [authn-http:post](#authn-http:post) | [authn-jwt:hmac-based](#authn-jwt:hmac-based) | [authn-jwt:public-key](#authn-jwt:public-key) | [authn-jwt:jwks](#authn-jwt:jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db:authentication)

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段。




MQTT topics that correspond to LwM2M events.

**gateway.lwm2m.translators.command**

  *类型*: `gateway:translator`

  下行命令主题。
对于每个成功上线的新 LwM2M 客户端，网关会创建一个订阅关系来接收下行消息并将其发送给客户端。


**gateway.lwm2m.translators.response**

  *类型*: `gateway:translator`

  用于网关发布来自 LwM2M 客户端的确认事件的主题。


**gateway.lwm2m.translators.notify**

  *类型*: `gateway:translator`

  用于发布来自 LwM2M 客户端的通知事件的主题。
在成功 Observe 到 LwM2M 客户端的资源后，如果客户端报告任何资源状态的变化，网关将通过该主题发送通知事件。


**gateway.lwm2m.translators.register**

  *类型*: `gateway:translator`

  用于发布来自 LwM2M 客户端的注册事件的主题。


**gateway.lwm2m.translators.update**

  *类型*: `gateway:translator`

  用于发布来自LwM2M客户端的更新事件的主题。



### MQTT-SN


The MQTT-SN (MQTT for Sensor Networks) protocol gateway.

**gateway.mqttsn.gateway_id**

  *类型*: `integer`

  *默认值*: `1`

  MQTT-SN 网关 ID。
当 <code>broadcast</code> 打开时，MQTT-SN 网关会使用该 ID 来广播 ADVERTISE 消息


**gateway.mqttsn.broadcast**

  *类型*: `boolean`

  *默认值*: `false`

  是否周期性广播 ADVERTISE 消息


**gateway.mqttsn.enable_qos3**

  *类型*: `boolean`

  *默认值*: `true`

  是否允许无连接的客户端发送 QoS 等于 -1 的消息。
该功能主要用于支持轻量的 MQTT-SN 客户端实现，它不会向网关建立连接，注册主题，也不会发起订阅；它只使用 QoS 为 -1 来发布消息


**gateway.mqttsn.subs_resume**

  *类型*: `boolean`

  *默认值*: `false`

  在会话被重用后，网关是否主动向客户端注册对已订阅主题名称


**gateway.mqttsn.predefined**

  *类型*: `array`

  *默认值*: `[]`

  预定义主题列表。
预定义的主题列表，是一组 主题 ID 和 主题名称 的映射关系。使用预先定义的主题列表，可以减少 MQTT-SN 客户端和网关对于固定主题的注册请求


**gateway.mqttsn.mountpoint**

  *类型*: `string`

  *默认值*: `""`


**gateway.mqttsn.listeners**

  *类型*: `gateway:udp_listeners`

  配置 UDP 类型的监听器。


**gateway.mqttsn.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用该网关


**gateway.mqttsn.enable_stats**

  *类型*: `boolean`

  *默认值*: `true`

  是否开启客户端统计


**gateway.mqttsn.idle_timeout**

  *类型*: `emqx_gateway_schema:duration`

  *默认值*: `30s`

  客户端连接过程的空闲时间。该配置用于：
  1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
  2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。


**gateway.mqttsn.clientinfo_override**

  *类型*: `gateway:clientinfo_override`

  ClientInfo 重写。


**gateway.mqttsn.authentication**

  *类型*: [authn-builtin_db:authentication](#authn-builtin_db:authentication) | [authn-mysql:authentication](#authn-mysql:authentication) | [authn-postgresql:authentication](#authn-postgresql:authentication) | [authn-mongodb:standalone](#authn-mongodb:standalone) | [authn-mongodb:replica-set](#authn-mongodb:replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb:sharded-cluster) | [authn-redis:standalone](#authn-redis:standalone) | [authn-redis:cluster](#authn-redis:cluster) | [authn-redis:sentinel](#authn-redis:sentinel) | [authn-http:get](#authn-http:get) | [authn-http:post](#authn-http:post) | [authn-jwt:hmac-based](#authn-jwt:hmac-based) | [authn-jwt:public-key](#authn-jwt:public-key) | [authn-jwt:jwks](#authn-jwt:jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db:authentication)

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段。




The pre-defined topic name corresponding to the pre-defined topic
ID of N.

Note: the pre-defined topic ID of 0 is reserved.

**gateway.mqttsn.predefined.$INDEX.id**

  *类型*: `integer`

  主题 ID。范围：1-65535


**gateway.mqttsn.predefined.$INDEX.topic**

  *类型*: `string`

  主题名称。注：不支持通配符



### STOMP


The STOMP protocol gateway provides EMQX with the ability to access STOMP
(Simple (or Streaming) Text Orientated Messaging Protocol) protocol.

**gateway.stomp.frame**

  *类型*: `gateway:stomp_frame`


**gateway.stomp.mountpoint**

  *类型*: `string`

  *默认值*: `""`


**gateway.stomp.listeners**

  *类型*: `gateway:tcp_listeners`

  配置 TCP 类型的监听器。


**gateway.stomp.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用该网关


**gateway.stomp.enable_stats**

  *类型*: `boolean`

  *默认值*: `true`

  是否开启客户端统计


**gateway.stomp.idle_timeout**

  *类型*: `emqx_gateway_schema:duration`

  *默认值*: `30s`

  客户端连接过程的空闲时间。该配置用于：
  1. 一个新创建的客户端进程如果在该时间间隔内没有收到任何客户端请求，将被直接关闭。
  2. 一个正在运行的客户进程如果在这段时间后没有收到任何客户请求，将进入休眠状态以节省资源。


**gateway.stomp.clientinfo_override**

  *类型*: `gateway:clientinfo_override`

  ClientInfo 重写。


**gateway.stomp.authentication**

  *类型*: [authn-builtin_db:authentication](#authn-builtin_db:authentication) | [authn-mysql:authentication](#authn-mysql:authentication) | [authn-postgresql:authentication](#authn-postgresql:authentication) | [authn-mongodb:standalone](#authn-mongodb:standalone) | [authn-mongodb:replica-set](#authn-mongodb:replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb:sharded-cluster) | [authn-redis:standalone](#authn-redis:standalone) | [authn-redis:cluster](#authn-redis:cluster) | [authn-redis:sentinel](#authn-redis:sentinel) | [authn-http:get](#authn-http:get) | [authn-http:post](#authn-http:post) | [authn-jwt:hmac-based](#authn-jwt:hmac-based) | [authn-jwt:public-key](#authn-jwt:public-key) | [authn-jwt:jwks](#authn-jwt:jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db:authentication)

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段。




Size limits for the STOMP frames.

**gateway.stomp.frame.max_headers**

  *类型*: `non_neg_integer`

  *默认值*: `10`

  允许的 Header 最大数量


**gateway.stomp.frame.max_headers_length**

  *类型*: `non_neg_integer`

  *默认值*: `1024`

  允许的 Header 字符串的最大长度


**gateway.stomp.frame.max_body_length**

  *类型*: `integer`

  *默认值*: `65536`

  允许的 Stomp 报文 Body 的最大字节数



### 网关可用监听器配置


Settings for the TCP listener.

**gateway:tcp_listener.acceptors**

  *类型*: `integer`

  *默认值*: `16`

  Acceptor 进程池大小。


**gateway:tcp_listener.tcp_options**

  *类型*: [broker:tcp_opts](#tcp_opts)

  TCP Socket 配置。


**gateway:tcp_listener.proxy_protocol**

  *类型*: `boolean`

  *默认值*: `false`

  是否开启 Proxy Protocol V1/2。当 EMQX 集群部署在 HAProxy 或 Nginx 后需要获取客户端真实 IP 时常用到该选项。参考：https://www.haproxy.com/blog/haproxy/proxy-protocol/


**gateway:tcp_listener.proxy_protocol_timeout**

  *类型*: `emqx_gateway_schema:duration`

  *默认值*: `15s`

  接收 Proxy Protocol 报文头的超时时间。如果在超时内没有收到 Proxy Protocol 包，EMQX 将关闭 TCP 连接。


**gateway:tcp_listener.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用该监听器。


**gateway:tcp_listener.bind**

  *类型*: [emqx_gateway_schema:ip_port()](#emqx_gateway_schema:ip_port()) | integer

  监听器绑定的 IP 地址或端口。


**gateway:tcp_listener.max_connections**

  *类型*: `integer`

  *默认值*: `1024`

  监听器支持的最大连接数。


**gateway:tcp_listener.max_conn_rate**

  *类型*: `integer`

  *默认值*: `1000`

  监听器支持的最大连接速率。


**gateway:tcp_listener.authentication**

  *类型*: [authn-builtin_db:authentication](#authn-builtin_db:authentication) | [authn-mysql:authentication](#authn-mysql:authentication) | [authn-postgresql:authentication](#authn-postgresql:authentication) | [authn-mongodb:standalone](#authn-mongodb:standalone) | [authn-mongodb:replica-set](#authn-mongodb:replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb:sharded-cluster) | [authn-redis:standalone](#authn-redis:standalone) | [authn-redis:cluster](#authn-redis:cluster) | [authn-redis:sentinel](#authn-redis:sentinel) | [authn-http:get](#authn-http:get) | [authn-http:post](#authn-http:post) | [authn-jwt:hmac-based](#authn-jwt:hmac-based) | [authn-jwt:public-key](#authn-jwt:public-key) | [authn-jwt:jwks](#authn-jwt:jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db:authentication)

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段。


**gateway:tcp_listener.enable_authn**

  *类型*: `boolean`

  *默认值*: `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
配置 <code>false</code> 时，将不对客户端做任何认证。


**gateway:tcp_listener.mountpoint**

  *类型*: `string`

  发布或订阅时，在所有主题前增加前缀字符串。
当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
则客户端实际上订阅了 `some_tenant/t` 主题。
类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
挂载点字符串中可用的变量：<br/>
   - <code>${clientid}</code>：clientid<br/>
   - <code>${username}</code>：用户名


**gateway:tcp_listener.access_rules**

  *类型*: `array`

  *默认值*: `[]`

  配置监听器的访问控制规则。
见：https://github.com/emqtt/esockd#allowdeny




Settings for the TCP listeners.

**gateway.stomp.listeners.tcp**

  *类型*: `name`


**gateway.stomp.listeners.ssl**

  *类型*: `name`




Settings for the listeners.

**gateway.exproto.listeners.tcp**

  *类型*: `name`


**gateway.exproto.listeners.ssl**

  *类型*: `name`


**gateway.exproto.listeners.udp**

  *类型*: `name`


**gateway.exproto.listeners.dtls**

  *类型*: `name`




Settings for the DTLS listener.

**gateway:dtls_listener.acceptors**

  *类型*: `integer`

  *默认值*: `16`

  Acceptor 进程池大小。


**gateway:dtls_listener.udp_options**

  *类型*: `gateway:udp_opts`


**gateway:dtls_listener.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用该监听器。


**gateway:dtls_listener.bind**

  *类型*: [emqx_gateway_schema:ip_port()](#emqx_gateway_schema:ip_port()) | integer

  监听器绑定的 IP 地址或端口。


**gateway:dtls_listener.max_connections**

  *类型*: `integer`

  *默认值*: `1024`

  监听器支持的最大连接数。


**gateway:dtls_listener.max_conn_rate**

  *类型*: `integer`

  *默认值*: `1000`

  监听器支持的最大连接速率。


**gateway:dtls_listener.authentication**

  *类型*: [authn-builtin_db:authentication](#authn-builtin_db:authentication) | [authn-mysql:authentication](#authn-mysql:authentication) | [authn-postgresql:authentication](#authn-postgresql:authentication) | [authn-mongodb:standalone](#authn-mongodb:standalone) | [authn-mongodb:replica-set](#authn-mongodb:replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb:sharded-cluster) | [authn-redis:standalone](#authn-redis:standalone) | [authn-redis:cluster](#authn-redis:cluster) | [authn-redis:sentinel](#authn-redis:sentinel) | [authn-http:get](#authn-http:get) | [authn-http:post](#authn-http:post) | [authn-jwt:hmac-based](#authn-jwt:hmac-based) | [authn-jwt:public-key](#authn-jwt:public-key) | [authn-jwt:jwks](#authn-jwt:jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db:authentication)

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段。


**gateway:dtls_listener.enable_authn**

  *类型*: `boolean`

  *默认值*: `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
配置 <code>false</code> 时，将不对客户端做任何认证。


**gateway:dtls_listener.mountpoint**

  *类型*: `string`

  发布或订阅时，在所有主题前增加前缀字符串。
当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
则客户端实际上订阅了 `some_tenant/t` 主题。
类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
挂载点字符串中可用的变量：<br/>
   - <code>${clientid}</code>：clientid<br/>
   - <code>${username}</code>：用户名


**gateway:dtls_listener.access_rules**

  *类型*: `array`

  *默认值*: `[]`

  配置监听器的访问控制规则。
见：https://github.com/emqtt/esockd#allowdeny


**gateway:dtls_listener.dtls_options**

  *类型*: `gateway:dtls_opts`

  DTLS Socket 配置




Settings for the DTLS protocol.

**gateway:dtls_opts.cacertfile**

  *类型*: `string`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**gateway:dtls_opts.certfile**

  *类型*: `string`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**gateway:dtls_opts.keyfile**

  *类型*: `string`

  PEM格式的私钥文件。


**gateway:dtls_opts.verify**

  *类型*: `enum`

  *默认值*: `verify_none`

  *可选值*: `verify_peer | verify_none`

  启用或禁用对等验证。


**gateway:dtls_opts.reuse_sessions**

  *类型*: `boolean`

  *默认值*: `true`

  启用 TLS 会话重用。


**gateway:dtls_opts.depth**

  *类型*: `integer`

  *默认值*: `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
因此，如果深度为0，则对等方必须由受信任的根 CA 直接签名；<br/>
如果是1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
如果是2，则路径可以是PEER、中间 CA1、中间 CA2、ROOT-CA。


**gateway:dtls_opts.password**

  *类型*: `string`

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。


**gateway:dtls_opts.versions**

  *类型*: `array`

  *默认值*: `["dtlsv1.2","dtlsv1"]`

  支持所有TLS/DTLS版本<br/>
注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


**gateway:dtls_opts.ciphers**

  *类型*: `array`

  *默认值*: `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
<br/>
密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
不兼容的密码套件将被自动删除。

例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

<br/>
注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**gateway:dtls_opts.user_lookup_fun**

  *类型*: `string`

  *默认值*: `emqx_tls_psk:lookup`

  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


**gateway:dtls_opts.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**gateway:dtls_opts.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

   在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**gateway:dtls_opts.dhfile**

  *类型*: `string`

  如果协商使用Diffie-Hellman密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数。<br/>
注意：TLS 1.3不支持<code>dhfile</code>选项。


**gateway:dtls_opts.fail_if_no_peer_cert**

  *类型*: `boolean`

  *默认值*: `false`

  TLS/DTLS 服务器与 {verify，verify_peer} 一起使用。
如果设置为true，则如果客户端没有要发送的证书，即发送空证书，服务器将失败。
如果设置为false，则仅当客户端发送无效证书（空证书被视为有效证书）时才会失败。


**gateway:dtls_opts.honor_cipher_order**

  *类型*: `boolean`

  *默认值*: `true`

  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


**gateway:dtls_opts.client_renegotiation**

  *类型*: `boolean`

  *默认值*: `true`

  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
这可能会成为拒绝服务攻击的载体。
SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。


**gateway:dtls_opts.handshake_timeout**

  *类型*: `duration`

  *默认值*: `15s`

  握手完成所允许的最长时间


**gateway:dtls_opts.gc_after_handshake**

  *类型*: `boolean`

  *默认值*: `false`

  内存使用调优。如果启用，将在TLS/SSL握手完成后立即执行垃圾回收。TLS/SSL握手建立后立即进行GC。




Settings for the UDP listener.

**gateway:udp_listener.udp_options**

  *类型*: `gateway:udp_opts`


**gateway:udp_listener.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用该监听器。


**gateway:udp_listener.bind**

  *类型*: [emqx_gateway_schema:ip_port()](#emqx_gateway_schema:ip_port()) | integer

  监听器绑定的 IP 地址或端口。


**gateway:udp_listener.max_connections**

  *类型*: `integer`

  *默认值*: `1024`

  监听器支持的最大连接数。


**gateway:udp_listener.max_conn_rate**

  *类型*: `integer`

  *默认值*: `1000`

  监听器支持的最大连接速率。


**gateway:udp_listener.authentication**

  *类型*: [authn-builtin_db:authentication](#authn-builtin_db:authentication) | [authn-mysql:authentication](#authn-mysql:authentication) | [authn-postgresql:authentication](#authn-postgresql:authentication) | [authn-mongodb:standalone](#authn-mongodb:standalone) | [authn-mongodb:replica-set](#authn-mongodb:replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb:sharded-cluster) | [authn-redis:standalone](#authn-redis:standalone) | [authn-redis:cluster](#authn-redis:cluster) | [authn-redis:sentinel](#authn-redis:sentinel) | [authn-http:get](#authn-http:get) | [authn-http:post](#authn-http:post) | [authn-jwt:hmac-based](#authn-jwt:hmac-based) | [authn-jwt:public-key](#authn-jwt:public-key) | [authn-jwt:jwks](#authn-jwt:jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db:authentication)

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段。


**gateway:udp_listener.enable_authn**

  *类型*: `boolean`

  *默认值*: `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
配置 <code>false</code> 时，将不对客户端做任何认证。


**gateway:udp_listener.mountpoint**

  *类型*: `string`

  发布或订阅时，在所有主题前增加前缀字符串。
当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
则客户端实际上订阅了 `some_tenant/t` 主题。
类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
挂载点字符串中可用的变量：<br/>
   - <code>${clientid}</code>：clientid<br/>
   - <code>${username}</code>：用户名


**gateway:udp_listener.access_rules**

  *类型*: `array`

  *默认值*: `[]`

  配置监听器的访问控制规则。
见：https://github.com/emqtt/esockd#allowdeny




Settings for the UDP listeners.

**gateway:udp_listeners.udp**

  *类型*: `name`


**gateway:udp_listeners.dtls**

  *类型*: `name`




Settings for the UDP sockets.

**gateway:udp_opts.active_n**

  *类型*: `integer`

  *默认值*: `100`

  为 Socket 指定 {active, N} 选项。
参见：https://erlang.org/doc/man/inet.html#setopts-2


**gateway:udp_opts.recbuf**

  *类型*: `emqx_gateway_schema:bytesize`

  Socket 在内核空间接收缓冲区的大小。


**gateway:udp_opts.sndbuf**

  *类型*: `emqx_gateway_schema:bytesize`

  Socket 在内核空间发送缓冲区的大小。


**gateway:udp_opts.buffer**

  *类型*: `emqx_gateway_schema:bytesize`

  Socket 在用户空间的缓冲区大小。


**gateway:udp_opts.reuseaddr**

  *类型*: `boolean`

  *默认值*: `true`

  允许重用本地处于 TIME_WAIT 的端口号。




Settings for the SSL listener.

**gateway:ssl_listener.acceptors**

  *类型*: `integer`

  *默认值*: `16`

  Acceptor 进程池大小。


**gateway:ssl_listener.tcp_options**

  *类型*: [broker:tcp_opts](#tcp_opts)

  TCP Socket 配置。


**gateway:ssl_listener.proxy_protocol**

  *类型*: `boolean`

  *默认值*: `false`

  是否开启 Proxy Protocol V1/2。当 EMQX 集群部署在 HAProxy 或 Nginx 后需要获取客户端真实 IP 时常用到该选项。参考：https://www.haproxy.com/blog/haproxy/proxy-protocol/


**gateway:ssl_listener.proxy_protocol_timeout**

  *类型*: `emqx_gateway_schema:duration`

  *默认值*: `15s`

  接收 Proxy Protocol 报文头的超时时间。如果在超时内没有收到 Proxy Protocol 包，EMQX 将关闭 TCP 连接。


**gateway:ssl_listener.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用该监听器。


**gateway:ssl_listener.bind**

  *类型*: [emqx_gateway_schema:ip_port()](#emqx_gateway_schema:ip_port()) | integer

  监听器绑定的 IP 地址或端口。


**gateway:ssl_listener.max_connections**

  *类型*: `integer`

  *默认值*: `1024`

  监听器支持的最大连接数。


**gateway:ssl_listener.max_conn_rate**

  *类型*: `integer`

  *默认值*: `1000`

  监听器支持的最大连接速率。


**gateway:ssl_listener.authentication**

  *类型*: [authn-builtin_db:authentication](#authn-builtin_db:authentication) | [authn-mysql:authentication](#authn-mysql:authentication) | [authn-postgresql:authentication](#authn-postgresql:authentication) | [authn-mongodb:standalone](#authn-mongodb:standalone) | [authn-mongodb:replica-set](#authn-mongodb:replica-set) | [authn-mongodb:sharded-cluster](#authn-mongodb:sharded-cluster) | [authn-redis:standalone](#authn-redis:standalone) | [authn-redis:cluster](#authn-redis:cluster) | [authn-redis:sentinel](#authn-redis:sentinel) | [authn-http:get](#authn-http:get) | [authn-http:post](#authn-http:post) | [authn-jwt:hmac-based](#authn-jwt:hmac-based) | [authn-jwt:public-key](#authn-jwt:public-key) | [authn-jwt:jwks](#authn-jwt:jwks) | [authn-scram-builtin_db:authentication](#authn-scram-builtin_db:authentication)

  网关的认证器配置，对该网关下所以的监听器生效。如果每个监听器需要配置不同的认证器，需要配置监听器下的 <code>authentication</code> 字段。


**gateway:ssl_listener.enable_authn**

  *类型*: `boolean`

  *默认值*: `true`

  配置 <code>true</code> （默认值）启用客户端进行身份认证。
配置 <code>false</code> 时，将不对客户端做任何认证。


**gateway:ssl_listener.mountpoint**

  *类型*: `string`

  发布或订阅时，在所有主题前增加前缀字符串。
当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
则客户端实际上订阅了 `some_tenant/t` 主题。
类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
挂载点字符串中可用的变量：<br/>
   - <code>${clientid}</code>：clientid<br/>
   - <code>${username}</code>：用户名


**gateway:ssl_listener.access_rules**

  *类型*: `array`

  *默认值*: `[]`

  配置监听器的访问控制规则。
见：https://github.com/emqtt/esockd#allowdeny


**gateway:ssl_listener.ssl_options**

  *类型*: [listener_ssl_opts](#监听器-ssl-tls-配置)

  SSL Socket 配置。




SSL configuration for the server.

**gateway.exproto.server.ssl_options.cacertfile**

  *类型*: `string`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**gateway.exproto.server.ssl_options.certfile**

  *类型*: `string`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**gateway.exproto.server.ssl_options.keyfile**

  *类型*: `string`

  PEM格式的私钥文件。


**gateway.exproto.server.ssl_options.verify**

  *类型*: `enum`

  *默认值*: `verify_none`

  *可选值*: `verify_peer | verify_none`

  启用或禁用对等验证。


**gateway.exproto.server.ssl_options.reuse_sessions**

  *类型*: `boolean`

  *默认值*: `true`

  启用 TLS 会话重用。


**gateway.exproto.server.ssl_options.depth**

  *类型*: `integer`

  *默认值*: `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
因此，如果深度为0，则对等方必须由受信任的根 CA 直接签名；<br/>
如果是1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
如果是2，则路径可以是PEER、中间 CA1、中间 CA2、ROOT-CA。


**gateway.exproto.server.ssl_options.password**

  *类型*: `string`

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。


**gateway.exproto.server.ssl_options.versions**

  *类型*: `array`

  *默认值*: `["tlsv1.3","tlsv1.2","tlsv1.1","tlsv1"]`

  支持所有TLS/DTLS版本<br/>
注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


**gateway.exproto.server.ssl_options.ciphers**

  *类型*: `array`

  *默认值*: `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
<br/>
密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
不兼容的密码套件将被自动删除。

例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

<br/>
注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**gateway.exproto.server.ssl_options.user_lookup_fun**

  *类型*: `string`

  *默认值*: `emqx_tls_psk:lookup`

  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


**gateway.exproto.server.ssl_options.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**gateway.exproto.server.ssl_options.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

   在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**gateway.exproto.server.ssl_options.dhfile**

  *类型*: `string`

  如果协商使用Diffie-Hellman密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数。<br/>
注意：TLS 1.3不支持<code>dhfile</code>选项。


**gateway.exproto.server.ssl_options.fail_if_no_peer_cert**

  *类型*: `boolean`

  *默认值*: `false`

  TLS/DTLS 服务器与 {verify，verify_peer} 一起使用。
如果设置为true，则如果客户端没有要发送的证书，即发送空证书，服务器将失败。
如果设置为false，则仅当客户端发送无效证书（空证书被视为有效证书）时才会失败。


**gateway.exproto.server.ssl_options.honor_cipher_order**

  *类型*: `boolean`

  *默认值*: `true`

  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


**gateway.exproto.server.ssl_options.client_renegotiation**

  *类型*: `boolean`

  *默认值*: `true`

  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
这可能会成为拒绝服务攻击的载体。
SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。


**gateway.exproto.server.ssl_options.handshake_timeout**

  *类型*: `duration`

  *默认值*: `15s`

  握手完成所允许的最长时间



## 插件


管理EMQX插件。<br/>
插件可以是EMQX安装包中的一部分，也可以是一个独立的安装包。<br/>
独立安装的插件称为“外部插件”。

**plugins.states**

  *类型*: `array`

  *默认值*: `[]`

  一组插件的状态。插件将按照定义的顺序启动


**plugins.install_dir**

  *类型*: `string`

  *默认值*: `plugins`

  插件安装包的目录，出于安全考虑，该目录应该值允许 <code>emqx</code>，或用于运行 EMQX 服务的用户拥有写入权限。


**plugins.check_interval**

  *类型*: `duration`

  *默认值*: `5s`

  检查间隔：检查集群中插件的状态是否一致，<br/>
如果连续3次检查结果不一致，则报警。




描述插件的状态

**plugins.states.$INDEX.name_vsn**

  *类型*: `string`

  插件的名称{name}-{version}。<br/>
它应该与插件的发布包名称一致，如my_plugin-0.1.0。


**plugins.states.$INDEX.enable**

  *类型*: `boolean`

  设置为“true”以启用此插件



## ExHook 多语言钩子


External hook (exhook) configuration.

**exhook.servers**

  *类型*: `array`

  *默认值*: `[]`

  ExHook 服务器列表




gRPC server configuration.

**exhook.servers.$INDEX.name**

  *类型*: `string`

  ExHook 服务器名称


**exhook.servers.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  开启这个 Exhook 服务器


**exhook.servers.$INDEX.url**

  *类型*: `string`

  gRPC 服务器地址


**exhook.servers.$INDEX.request_timeout**

  *类型*: `duration`

  *默认值*: `5s`

  gRPC 服务器请求超时时间


**exhook.servers.$INDEX.failed_action**

  *类型*: `enum`

  *默认值*: `deny`

  *可选值*: `deny | ignore`

  当 gRPC 请求失败后的操作


**exhook.servers.$INDEX.ssl**

  *类型*: `exhook:ssl_conf`


**exhook.servers.$INDEX.socket_options**

  *类型*: `exhook:socket_options`

  *默认值*: `{"keepalive":true,"nodelay":true}`


**exhook.servers.$INDEX.auto_reconnect**

  *类型*: `false | duration`

  *默认值*: `60s`

  自动重连到 gRPC 服务器的设置。
当 gRPC 服务器不可用时，Exhook 将会按照这里设置的间隔时间进行重连，并重新初始化注册的钩子


**exhook.servers.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  gRPC 客户端进程池大小




连接套接字设置

**exhook.servers.$INDEX.socket_options.keepalive**

  *类型*: `boolean`

  *默认值*: `true`

  当没有其他数据交换时，是否向连接的对端套接字定期的发送探测包。如果另一端没有响应，则认为连接断开，并向控制进程发送错误消息


**exhook.servers.$INDEX.socket_options.nodelay**

  *类型*: `boolean`

  *默认值*: `true`

  如果为 true，则为套接字设置 TCP_NODELAY 选项，这意味着会立即发送数据包


**exhook.servers.$INDEX.socket_options.recbuf**

  *类型*: `bytesize`

  套接字的最小接收缓冲区大小


**exhook.servers.$INDEX.socket_options.sndbuf**

  *类型*: `bytesize`

  套接字的最小发送缓冲区大小




SSL client configuration.

**exhook.servers.$INDEX.ssl.cacertfile**

  *类型*: `string`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**exhook.servers.$INDEX.ssl.certfile**

  *类型*: `string`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**exhook.servers.$INDEX.ssl.keyfile**

  *类型*: `string`

  PEM格式的私钥文件。


**exhook.servers.$INDEX.ssl.verify**

  *类型*: `enum`

  *默认值*: `verify_none`

  *可选值*: `verify_peer | verify_none`

  启用或禁用对等验证。


**exhook.servers.$INDEX.ssl.reuse_sessions**

  *类型*: `boolean`

  *默认值*: `true`

  启用 TLS 会话重用。


**exhook.servers.$INDEX.ssl.depth**

  *类型*: `integer`

  *默认值*: `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
因此，如果深度为0，则对等方必须由受信任的根 CA 直接签名；<br/>
如果是1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
如果是2，则路径可以是PEER、中间 CA1、中间 CA2、ROOT-CA。


**exhook.servers.$INDEX.ssl.password**

  *类型*: `string`

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。


**exhook.servers.$INDEX.ssl.versions**

  *类型*: `array`

  *默认值*: `["tlsv1.3","tlsv1.2","tlsv1.1","tlsv1"]`

  支持所有TLS/DTLS版本<br/>
注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


**exhook.servers.$INDEX.ssl.ciphers**

  *类型*: `array`

  *默认值*: `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
<br/>
密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
不兼容的密码套件将被自动删除。

例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

<br/>
注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**exhook.servers.$INDEX.ssl.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**exhook.servers.$INDEX.ssl.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

   在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**exhook.servers.$INDEX.ssl.enable**

  *类型*: `boolean`

  *默认值*: `false`

  启用 TLS。


**exhook.servers.$INDEX.ssl.server_name_indication**

  *类型*: `disable | string`

  指定要在 TLS 服务器名称指示扩展中使用的主机名。<br/>
例如，当连接到 "server.example.net" 时，接受连接并执行 TLS 握手的真正服务器可能与 TLS 客户端最初连接到的主机不同，
例如，当连接到 IP 地址时，或者当主机具有多个可解析的 DNS 记录时<br/>
如果未指定，它将默认为使用的主机名字符串
建立连接，除非使用 IP 地址<br/>
然后，主机名也用于对等机的主机名验证证书<br/>
特殊值 <code>disable</code> 阻止发送服务器名称指示扩展，并禁用主机名验证检查。



## 附录

### 客户端 SSL/TLS 配置


Socket options for SSL clients.

**ssl_client_opts.cacertfile**

  *类型*: `string`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**ssl_client_opts.certfile**

  *类型*: `string`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**ssl_client_opts.keyfile**

  *类型*: `string`

  PEM格式的私钥文件。


**ssl_client_opts.verify**

  *类型*: `enum`

  *默认值*: `verify_none`

  *可选值*: `verify_peer | verify_none`

  启用或禁用对等验证。


**ssl_client_opts.reuse_sessions**

  *类型*: `boolean`

  *默认值*: `true`

  启用 TLS 会话重用。


**ssl_client_opts.depth**

  *类型*: `integer`

  *默认值*: `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
因此，如果深度为0，则对等方必须由受信任的根 CA 直接签名；<br/>
如果是1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
如果是2，则路径可以是PEER、中间 CA1、中间 CA2、ROOT-CA。


**ssl_client_opts.password**

  *类型*: `string`

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。


**ssl_client_opts.versions**

  *类型*: `array`

  *默认值*: `["tlsv1.3","tlsv1.2","tlsv1.1","tlsv1"]`

  支持所有TLS/DTLS版本<br/>
注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


**ssl_client_opts.ciphers**

  *类型*: `array`

  *默认值*: `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
<br/>
密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
不兼容的密码套件将被自动删除。

例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

<br/>
注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**ssl_client_opts.user_lookup_fun**

  *类型*: `string`

  *默认值*: `emqx_tls_psk:lookup`

  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


**ssl_client_opts.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**ssl_client_opts.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

   在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**ssl_client_opts.enable**

  *类型*: `boolean`

  *默认值*: `false`

  启用 TLS。


**ssl_client_opts.server_name_indication**

  *类型*: `disable | string`

  指定要在 TLS 服务器名称指示扩展中使用的主机名。<br/>
例如，当连接到 "server.example.net" 时，接受连接并执行 TLS 握手的真正服务器可能与 TLS 客户端最初连接到的主机不同，
例如，当连接到 IP 地址时，或者当主机具有多个可解析的 DNS 记录时<br/>
如果未指定，它将默认为使用的主机名字符串
建立连接，除非使用 IP 地址<br/>
然后，主机名也用于对等机的主机名验证证书<br/>
特殊值 <code>disable</code> 阻止发送服务器名称指示扩展，并禁用主机名验证检查。



### 监听器 SSL/TLS 配置


Socket options for SSL connections.

**listener_ssl_opts.cacertfile**

  *类型*: `string`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**listener_ssl_opts.certfile**

  *类型*: `string`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**listener_ssl_opts.keyfile**

  *类型*: `string`

  PEM格式的私钥文件。


**listener_ssl_opts.verify**

  *类型*: `enum`

  *默认值*: `verify_none`

  *可选值*: `verify_peer | verify_none`

  启用或禁用对等验证。


**listener_ssl_opts.reuse_sessions**

  *类型*: `boolean`

  *默认值*: `true`

  启用 TLS 会话重用。


**listener_ssl_opts.depth**

  *类型*: `integer`

  *默认值*: `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
因此，如果深度为0，则对等方必须由受信任的根 CA 直接签名；<br/>
如果是1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
如果是2，则路径可以是PEER、中间 CA1、中间 CA2、ROOT-CA。


**listener_ssl_opts.password**

  *类型*: `string`

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。


**listener_ssl_opts.versions**

  *类型*: `array`

  *默认值*: `["tlsv1.3","tlsv1.2","tlsv1.1","tlsv1"]`

  支持所有TLS/DTLS版本<br/>
注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


**listener_ssl_opts.ciphers**

  *类型*: `array`

  *默认值*: `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
<br/>
密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
不兼容的密码套件将被自动删除。

例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

<br/>
注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**listener_ssl_opts.user_lookup_fun**

  *类型*: `string`

  *默认值*: `emqx_tls_psk:lookup`

  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


**listener_ssl_opts.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**listener_ssl_opts.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

   在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**listener_ssl_opts.dhfile**

  *类型*: `string`

  如果协商使用Diffie-Hellman密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数。<br/>
注意：TLS 1.3不支持<code>dhfile</code>选项。


**listener_ssl_opts.fail_if_no_peer_cert**

  *类型*: `boolean`

  *默认值*: `false`

  TLS/DTLS 服务器与 {verify，verify_peer} 一起使用。
如果设置为true，则如果客户端没有要发送的证书，即发送空证书，服务器将失败。
如果设置为false，则仅当客户端发送无效证书（空证书被视为有效证书）时才会失败。


**listener_ssl_opts.honor_cipher_order**

  *类型*: `boolean`

  *默认值*: `true`

  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


**listener_ssl_opts.client_renegotiation**

  *类型*: `boolean`

  *默认值*: `true`

  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
这可能会成为拒绝服务攻击的载体。
SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。


**listener_ssl_opts.handshake_timeout**

  *类型*: `duration`

  *默认值*: `15s`

  握手完成所允许的最长时间


**listener_ssl_opts.gc_after_handshake**

  *类型*: `boolean`

  *默认值*: `false`

  内存使用调优。如果启用，将在TLS/SSL握手完成后立即执行垃圾回收。TLS/SSL握手建立后立即进行GC。



### tcp_opts


TCP listener options.

**tcp_opts.active_n**

  *类型*: `integer`

  *默认值*: `100`

  为此套接字指定{active，N}选项<br/>
See: https://erlang.org/doc/man/inet.html#setopts-2


**tcp_opts.backlog**

  *类型*: `pos_integer`

  *默认值*: `1024`

  TCP backlog 定义了挂起连接队列可以增长到的最大长度。


**tcp_opts.send_timeout**

  *类型*: `duration`

  *默认值*: `15s`

  连接的 TCP 发送超时。


**tcp_opts.send_timeout_close**

  *类型*: `boolean`

  *默认值*: `true`

  如果发送超时，则关闭连接。


**tcp_opts.recbuf**

  *类型*: `bytesize`

  连接的 TCP 接收缓冲区（OS 内核）。


**tcp_opts.sndbuf**

  *类型*: `bytesize`

  连接的 TCP 发送缓冲区（OS 内核）。


**tcp_opts.buffer**

  *类型*: `bytesize`

  *默认值*: `4KB`

  驱动程序使用的用户空间缓冲区的大小。


**tcp_opts.high_watermark**

  *类型*: `bytesize`

  *默认值*: `1MB`

  当 VM 套接字实现内部排队的数据量达到此限制时，套接字将设置为忙碌状态。


**tcp_opts.nodelay**

  *类型*: `boolean`

  *默认值*: `true`

  连接的 TCP_NODELAY 标识


**tcp_opts.reuseaddr**

  *类型*: `boolean`

  *默认值*: `true`

  连接的 SO_REUSEADDR 标识。



### ws_opts


WebSocket listener options.

**ws_opts.mqtt_path**

  *类型*: `string`

  *默认值*: `/mqtt`

  WebSocket 的 MQTT 协议路径。因此，EMQX Broker的WebSocket地址为：
<code>ws://{ip}:{port}/mqtt</code>


**ws_opts.mqtt_piggyback**

  *类型*: `enum`

  *默认值*: `multiple`

  *可选值*: `single | multiple`

  WebSocket消息是否允许包含多个 MQTT 数据包。


**ws_opts.compress**

  *类型*: `boolean`

  *默认值*: `false`

  如果 <code>true</code>，则使用<code>zlib</code> 压缩 WebSocket 消息<br/>
<code>deflate_opts</code> 下的配置项属于压缩相关参数配置。


**ws_opts.idle_timeout**

  *类型*: `duration`

  *默认值*: `7200s`

  关闭在此间隔内未发送 MQTT CONNECT 消息的客户端的传输层连接。


**ws_opts.max_frame_size**

  *类型*: `infinity | integer`

  *默认值*: `infinity`

  单个 MQTT 数据包的最大长度。


**ws_opts.fail_if_no_subprotocol**

  *类型*: `boolean`

  *默认值*: `true`

  如果<code>true</code>，当客户端未携带<code>Sec WebSocket Protocol</code>字段时，服务器将返回一个错误。
<br/>注意：微信小程序需要禁用此验证。


**ws_opts.supported_subprotocols**

  *类型*: `comma_separated_list`

  *默认值*: `mqtt, mqtt-v3, mqtt-v3.1.1, mqtt-v5`

  逗号分隔的 subprotocols 支持列表。


**ws_opts.check_origin_enable**

  *类型*: `boolean`

  *默认值*: `false`

  如果<code>true</code>，<code>origin</code>HTTP 头将根据<code>check_origins</code>参数中配置的允许来源列表进行验证。


**ws_opts.allow_origin_absence**

  *类型*: `boolean`

  *默认值*: `true`

  If <code>false</code> and <code>check_origin_enable</code> is <code>true</code>, the server will reject requests that don't have <code>origin</code> HTTP header.


**ws_opts.check_origins**

  *类型*: `comma_separated_binary`

  *默认值*: `http://localhost:18083, http://127.0.0.1:18083`

  允许的 origins 列表


**ws_opts.proxy_address_header**

  *类型*: `string`

  *默认值*: `x-forwarded-for`

  HTTP 头，用于传递有关客户端 IP 地址的信息。
当 EMQX 集群部署在负载平衡器后面时，这一点非常重要。


**ws_opts.proxy_port_header**

  *类型*: `string`

  *默认值*: `x-forwarded-port`

  HTTP 头，用于传递有关客户端端口的信息。当 EMQX 集群部署在负载平衡器后面时，这一点非常重要。


**ws_opts.deflate_opts**

  *类型*: [broker:deflate_opts](#deflate_opts)



### listener_wss_opts


Socket options for WebSocket/SSL connections.

**listeners.wss.$name.ssl_options.cacertfile**

  *类型*: `string`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**listeners.wss.$name.ssl_options.certfile**

  *类型*: `string`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**listeners.wss.$name.ssl_options.keyfile**

  *类型*: `string`

  PEM格式的私钥文件。


**listeners.wss.$name.ssl_options.verify**

  *类型*: `enum`

  *默认值*: `verify_none`

  *可选值*: `verify_peer | verify_none`

  启用或禁用对等验证。


**listeners.wss.$name.ssl_options.reuse_sessions**

  *类型*: `boolean`

  *默认值*: `true`

  启用 TLS 会话重用。


**listeners.wss.$name.ssl_options.depth**

  *类型*: `integer`

  *默认值*: `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
因此，如果深度为0，则对等方必须由受信任的根 CA 直接签名；<br/>
如果是1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
如果是2，则路径可以是PEER、中间 CA1、中间 CA2、ROOT-CA。


**listeners.wss.$name.ssl_options.password**

  *类型*: `string`

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。


**listeners.wss.$name.ssl_options.versions**

  *类型*: `array`

  *默认值*: `["tlsv1.3","tlsv1.2","tlsv1.1","tlsv1"]`

  支持所有TLS/DTLS版本<br/>
注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


**listeners.wss.$name.ssl_options.ciphers**

  *类型*: `array`

  *默认值*: `[]`

  此配置保存由逗号分隔的 TLS 密码套件名称，或作为字符串数组。例如
<code>"TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256"</code>或
<code>["TLS_AES_256_GCM_SHA384","TLS_AES_128_GCM_SHA256"]</code>。
<br/>
密码（及其顺序）定义了客户端和服务器通过网络连接加密信息的方式。
选择一个好的密码套件对于应用程序的数据安全性、机密性和性能至关重要。

名称应为 OpenSSL 字符串格式（而不是 RFC 格式）。
EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式<br/>
注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
不兼容的密码套件将被自动删除。

例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

<br/>
注：PSK 的 Ciphers 不支持 tlsv1.3<br/>
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**listeners.wss.$name.ssl_options.user_lookup_fun**

  *类型*: `string`

  *默认值*: `emqx_tls_psk:lookup`

  用于查找预共享密钥（PSK）标识的 EMQX 内部回调。


**listeners.wss.$name.ssl_options.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**listeners.wss.$name.ssl_options.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

   在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**listeners.wss.$name.ssl_options.dhfile**

  *类型*: `string`

  如果协商使用Diffie-Hellman密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数。<br/>
注意：TLS 1.3不支持<code>dhfile</code>选项。


**listeners.wss.$name.ssl_options.fail_if_no_peer_cert**

  *类型*: `boolean`

  *默认值*: `false`

  TLS/DTLS 服务器与 {verify，verify_peer} 一起使用。
如果设置为true，则如果客户端没有要发送的证书，即发送空证书，服务器将失败。
如果设置为false，则仅当客户端发送无效证书（空证书被视为有效证书）时才会失败。


**listeners.wss.$name.ssl_options.honor_cipher_order**

  *类型*: `boolean`

  *默认值*: `true`

  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


**listeners.wss.$name.ssl_options.client_renegotiation**

  *类型*: `boolean`

  *默认值*: `true`

  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
这可能会成为拒绝服务攻击的载体。
SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。


**listeners.wss.$name.ssl_options.handshake_timeout**

  *类型*: `duration`

  *默认值*: `15s`

  握手完成所允许的最长时间



### deflate_opts


Compression options.

**deflate_opts.level**

  *类型*: `enum`

  *可选值*: `none | default | best_compression | best_speed`

  压缩级别


**deflate_opts.mem_level**

  *类型*: `integer`

  *默认值*: `8`

  *可选值*: `1-9`

  指定压缩状态的大小<br/>
较低的值会减少每个连接的内存使用。


**deflate_opts.strategy**

  *类型*: `enum`

  *默认值*: `default`

  *可选值*: `default | filtered | huffman_only | rle`

  指定压缩策略。


**deflate_opts.server_context_takeover**

  *类型*: `enum`

  *默认值*: `takeover`

  *可选值*: `takeover | no_takeover`

  接管意味着在服务器消息之间保留压缩状态。


**deflate_opts.client_context_takeover**

  *类型*: `enum`

  *默认值*: `takeover`

  *可选值*: `takeover | no_takeover`

  接管意味着在客户端消息之间保留压缩状态。


**deflate_opts.server_max_window_bits**

  *类型*: `integer`

  *默认值*: `15`

  *可选值*: `8-15`

  指定服务器压缩上下文的大小。


**deflate_opts.client_max_window_bits**

  *类型*: `integer`

  *默认值*: `15`

  *可选值*: `8-15`

  指定客户端压缩上下文的大小。

