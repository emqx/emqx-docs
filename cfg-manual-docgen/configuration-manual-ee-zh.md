# 配置手册

EMQX 配置文件手册。

## 节点设置

设置节点名称以及 Cookie。每个 Erlang 节点(进程)需指配一个节点名，用于节点间通信互访。 所有互相通信的 Erlang 节点(进程)间通过一个共用的 Cookie 进行安全认证。

**node.name**

  *类型*: `string`

  *默认值*: `emqx@127.0.0.1`

  节点名。格式为 \<name>@\<host>。其中 \<host> 可以是 IP 地址或者域名，也可以是 FQDN。
详见 http://erlang.org/doc/reference_manual/distributed.html。


**node.cookie**

  *类型*: `string`

  分布式 Erlang 集群使用的 cookie 值。集群间保持一致


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


**node.role**

  *类型*: `enum`

  *默认值*: `core`

  *可选值*: `core | replicant`

  选择节点的角色。<br/>
<code>core</code> 节点提供数据的持久性，并负责写入。建议将核心节点放置在不同的机架或不同的可用区。<br/>
<code>repliant</code> 节点是临时工作节点。 从集群中删除它们，不影响数据库冗余<br/>
建议复制节点多于核心节点。<br/>
注意：该参数仅在设置<code>backend</code>时生效到 <code>rlog</code>。



## RPC 设置


EMQX 使用 <code>gen_rpc</code> 库来实现跨节点通信。<br/>
大多数情况下，默认的配置应该可以工作，但如果你需要做一些性能优化或者实验，可以尝试调整这些参数。

**rpc.mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  在 <code>sync</code> 模式下，发送端等待接收端的 ack信号。


**rpc.protocol**

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

  *类型*: `timeout_duration_s`

  *默认值*: `15m`

  broker 之间的连接在最后一条消息发送后保持打开的时间。


**rpc.socket_keepalive_interval**

  *类型*: `timeout_duration_s`

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


**rpc.ciphers**

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
如果打算使用PSK密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**rpc.tls_versions**

  *类型*: `array`

  *默认值*: `["tlsv1.3","tlsv1.2"]`

  支持所有TLS/DTLS版本<br/>
注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


**rpc.listen_address**

  *类型*: `string`

  *默认值*: `0.0.0.0`

  Indicates the IP address for the RPC server to listen on. For example, use <code>"0.0.0.0"</code> for IPv4 or <code>"::"</code> for IPv6.


**rpc.ipv6_only**

  *类型*: `boolean`

  *默认值*: `false`

  This setting is effective only when <code>rpc.listen_address</code> is assigned an IPv6 address.
If set to <code>true</code>, the RPC client will exclusively use IPv6 for connections.
Otherwise, the client might opt for IPv4, even if the server is on IPv6.



## 集群设置


EMQX 节点可以组成一个集群，以提高总容量。<br/> 这里指定了节点之间如何连接。

**cluster.name**

  *类型*: `atom`

  *默认值*: `emqxcl`

  EMQX集群名称。每个集群都有一个唯一的名称。服务发现时会用于做路径的一部分。


**cluster.discovery_strategy**

  *类型*: `enum`

  *默认值*: `manual`

  *可选值*: `manual | static | dns | etcd | k8s`

  集群节点发现方式。可选值为:
- manual: 使用 <code>emqx ctl cluster</code> 命令管理集群。<br/>
- static: 配置静态节点。配置几个固定的节点，新节点通过连接固定节点中的某一个来加入集群。<br/>
- dns: 使用 DNS A 记录的方式发现节点。<br/>
- etcd: 使用 etcd 发现节点。<br/>
- k8s: 使用 Kubernetes API 发现节点。


**cluster.autoclean**

  *类型*: `duration`

  *默认值*: `24h`

  指定多久之后从集群中删除离线节点。


**cluster.autoheal**

  *类型*: `boolean`

  *默认值*: `true`

  集群脑裂自动恢复机制开关。


**cluster.proto_dist**

  *类型*: `enum`

  *默认值*: `inet_tcp`

  *可选值*: `inet_tcp | inet6_tcp | inet_tls | inet6_tls`

  分布式 Erlang 集群协议类型。可选值为:<br/>
- inet_tcp: 使用 IPv4 <br/>
- inet_tls: 使用 TLS，需要配合 <code>etc/ssl_dist.conf</code> 一起使用。


**cluster.static**

  *类型*: `cluster_static`


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

  *类型*: `comma_separated_atoms | array`

  *默认值*: `[]`

  集群中的EMQX节点名称列表，
指定固定的节点列表，多个节点间使用逗号 , 分隔。
当 cluster.discovery_strategy 为 static 时，此配置项才有效。
适合于节点数量较少且固定的集群。



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
v2/keys/\<prefix>/\<cluster.name>/\<node.name> <br/>
当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。


**cluster.etcd.node_ttl**

  *类型*: `duration`

  *默认值*: `1m`

  指定 etcd 中节点信息的过期时间。
当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。


**cluster.etcd.ssl_options**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  当使用 TLS 连接 etcd 时的配置选项。
当 cluster.discovery_strategy 为 etcd 时，此配置项才有效。



### 基于 Kubernetes 自动集群


Kubernetes 服务发现。

**cluster.k8s.apiserver**

  *类型*: `string`

  *默认值*: `https://kubernetes.default.svc:443`

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



## 日志参数

配置日志输出位置、日志级别、日志文件存储路径以及日志轮换、过载保护等参数。

### 文件输出日志


日志处理进程将日志事件打印到文件。

**log_file_handler.path**

  *类型*: `file`

  *默认值*: `${EMQX_LOG_DIR}/emqx.log`

  日志文件路径及名字。


**log_file_handler.rotation_count**

  *类型*: `integer`

  *默认值*: `10`

  *可选值*: `1-128`

  轮换的最大日志文件数。


**log_file_handler.rotation_size**

  *类型*: `infinity | bytesize`

  *默认值*: `50MB`

  此参数控制日志文件轮换。 `infinity` 意味着日志文件将无限增长，否则日志文件将在达到 `max_size`（以字节为单位）时进行轮换。
与 rotation count配合使用。如果 counter 为 10，则是10个文件轮换。


**log_file_handler.level**

  *类型*: `log_level`

  *默认值*: `warning`

  当前日志处理进程的日志级别。
默认为 warning 级别。


**log_file_handler.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用此日志处理进程。


**log_file_handler.formatter**

  *类型*: `enum`

  *默认值*: `text`

  *可选值*: `text | json`

  选择日志格式类型。 <code>text</code> 用于纯文本，<code>json</code> 用于结构化日志记录。


**log_file_handler.time_offset**

  *类型*: `string`

  *默认值*: `system`

  日志中的时间戳使用的时间偏移量。
可选值为：
  - <code>system</code>: 本地系统使用的时区偏移量
  - <code>utc</code>: 0 时区的偏移量
  - <code>+-[hh]:[mm]</code>: 自定义偏移量，比如 "-02:00" 或者 "+00:00"
默认值为本地系统的时区偏移量：<code>system</code>。



### Console 输出日志


日志处理进程将日志事件打印到 EMQX 控制台。

**log.console.level**

  *类型*: `log_level`

  *默认值*: `warning`

  当前日志处理进程的日志级别。
默认为 warning 级别。


**log.console.enable**

  *类型*: `boolean`

  *默认值*: `false`

  启用此日志处理进程。


**log.console.formatter**

  *类型*: `enum`

  *默认值*: `text`

  *可选值*: `text | json`

  选择日志格式类型。 <code>text</code> 用于纯文本，<code>json</code> 用于结构化日志记录。


**log.console.time_offset**

  *类型*: `string`

  *默认值*: `system`

  日志中的时间戳使用的时间偏移量。
可选值为：
  - <code>system</code>: 本地系统使用的时区偏移量
  - <code>utc</code>: 0 时区的偏移量
  - <code>+-[hh]:[mm]</code>: 自定义偏移量，比如 "-02:00" 或者 "+00:00"
默认值为本地系统的时区偏移量：<code>system</code>。



### 审计日志


将日志时间输出到文件的审计日志处理进程。

**log.audit.path**

  *类型*: `file`

  *默认值*: `${EMQX_LOG_DIR}/audit.log`

  ----


**log.audit.rotation_count**

  *类型*: `integer`

  *默认值*: `10`

  *可选值*: `1-128`

  轮换的最大日志文件数。


**log.audit.rotation_size**

  *类型*: `infinity | bytesize`

  *默认值*: `50MB`

  此参数控制日志文件轮换。 `infinity` 意味着日志文件将无限增长，否则日志文件将在达到 `max_size`（以字节为单位）时进行轮换。
与 rotation count配合使用。如果 counter 为 10，则是10个文件轮换。


**log.audit.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用此日志处理进程。


**log.audit.time_offset**

  *类型*: `string`

  *默认值*: `system`

  日志中的时间戳使用的时间偏移量。
可选值为：
  - <code>system</code>: 本地系统使用的时区偏移量
  - <code>utc</code>: 0 时区的偏移量
  - <code>+-[hh]:[mm]</code>: 自定义偏移量，比如 "-02:00" 或者 "+00:00"
默认值为本地系统的时区偏移量：<code>system</code>。



<!-- ### 日志轮换

log_rotation
-->

<!-- ### 日志突发限制

log_burst_limit -->

<!-- ### 日志过载终止

log_overload_kill -->

## MQTT/TCP 监听器 - 1883

EMQX 支持配置多个监听器，默认 MQTT/TCP 监听器端口为 `1883`。

**listeners.tcp.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启停监听器。


**listeners.tcp.$name.bind**

  *类型*: `ip_port`

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


**listeners.tcp.$name.enable_authn**

  *类型*: `enum`

  *默认值*: `true`

  *可选值*: `true | false | quick_deny_anonymous`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
客户直接拒绝，不做使用任何认证器对客户端进行身份检查。


**listeners.tcp.$name.max_conn_rate**

  *类型*: `rate`

  最大连接率。<br/> 用于限制该监听器的连接速率，一旦达到限制值，新的连接将被推迟或拒绝。


**listeners.tcp.$name.messages_rate**

  *类型*: `rate`

  消息发布速率。<br/> 用于限制连接到该监听器的每个客户端的入站消息数，一旦达到限制值，受限制的客户端将会减速甚至暂时被挂起。


**listeners.tcp.$name.bytes_rate**

  *类型*: `rate`

  数据发布速率。<br/> 用于限制连接到该监听器的每个客户端的入站字节速率，一旦达到限制值，受限制的客户端将会减速甚至暂时被挂起。


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


**listeners.tcp.$name.tcp_options**

  *类型*: [broker:tcp_opts](#tcp_opts)



## MQTT/SSL 监听器 - 8883


Settings for the MQTT over SSL listener.

**listeners.ssl.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启停监听器。


**listeners.ssl.$name.bind**

  *类型*: `ip_port`

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


**listeners.ssl.$name.enable_authn**

  *类型*: `enum`

  *默认值*: `true`

  *可选值*: `true | false | quick_deny_anonymous`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
客户直接拒绝，不做使用任何认证器对客户端进行身份检查。


**listeners.ssl.$name.max_conn_rate**

  *类型*: `rate`

  最大连接率。<br/> 用于限制该监听器的连接速率，一旦达到限制值，新的连接将被推迟或拒绝。


**listeners.ssl.$name.messages_rate**

  *类型*: `rate`

  消息发布速率。<br/> 用于限制连接到该监听器的每个客户端的入站消息数，一旦达到限制值，受限制的客户端将会减速甚至暂时被挂起。


**listeners.ssl.$name.bytes_rate**

  *类型*: `rate`

  数据发布速率。<br/> 用于限制连接到该监听器的每个客户端的入站字节速率，一旦达到限制值，受限制的客户端将会减速甚至暂时被挂起。


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


**listeners.ssl.$name.tcp_options**

  *类型*: [broker:tcp_opts](#tcp_opts)


**listeners.ssl.$name.ssl_options**

  *类型*: [listener_ssl_opts](#监听器-ssl-tls-配置)



## MQTT Over QUIC/UDP 监听器 - 14567

设置 MQTT over QUIC UDP 监听器，该监听器默认不启用且在某些操作系统中不可用，详情请参考 [MQTT over QUIC 快速开始](../mqtt-over-quic/getting-started.md)


Settings for the MQTT over QUIC listener.

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
EMQX 配置文档提供的所有默认值和示例都是 OpenSSL 格式。<br/>
注意：某些密码套件仅与特定的 TLS <code>版本</code>兼容（'tlsv1.1'、'tlsv1.2'或'tlsv1.3'）。
不兼容的密码套件将被自动删除。

例如，如果只有 <code>versions</code> 仅配置为 <code>tlsv1.3</code>。为其他版本配置密码套件将无效。

<br/>
注：PSK 的 Ciphers 不支持 tlsv1.3。<br/>
如果打算使用 PSK 密码套件，<code>tlsv1.3</code> 应在 <code>ssl.versions</code> 中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code><br/>

注：QUIC 监听器只支持 tlsv1.3 的 ciphers。


**listeners.quic.$name.ssl_options**

  *类型*: `broker:listener_quic_ssl_opts`

  QUIC 传输层的 TLS 选项


**listeners.quic.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启停监听器。


**listeners.quic.$name.bind**

  *类型*: `ip_port`

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


**listeners.quic.$name.enable_authn**

  *类型*: `enum`

  *默认值*: `true`

  *可选值*: `true | false | quick_deny_anonymous`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
客户直接拒绝，不做使用任何认证器对客户端进行身份检查。


**listeners.quic.$name.max_conn_rate**

  *类型*: `rate`

  最大连接率。<br/> 用于限制该监听器的连接速率，一旦达到限制值，新的连接将被推迟或拒绝。


**listeners.quic.$name.messages_rate**

  *类型*: `rate`

  消息发布速率。<br/> 用于限制连接到该监听器的每个客户端的入站消息数，一旦达到限制值，受限制的客户端将会减速甚至暂时被挂起。


**listeners.quic.$name.bytes_rate**

  *类型*: `rate`

  数据发布速率。<br/> 用于限制连接到该监听器的每个客户端的入站字节速率，一旦达到限制值，受限制的客户端将会减速甚至暂时被挂起。



## MQTT/WebSocket 监听器 - 8083


Settings for the MQTT over WebSocket listener.

**listeners.ws.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启停监听器。


**listeners.ws.$name.bind**

  *类型*: `ip_port`

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


**listeners.ws.$name.enable_authn**

  *类型*: `enum`

  *默认值*: `true`

  *可选值*: `true | false | quick_deny_anonymous`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
客户直接拒绝，不做使用任何认证器对客户端进行身份检查。


**listeners.ws.$name.max_conn_rate**

  *类型*: `rate`

  最大连接率。<br/> 用于限制该监听器的连接速率，一旦达到限制值，新的连接将被推迟或拒绝。


**listeners.ws.$name.messages_rate**

  *类型*: `rate`

  消息发布速率。<br/> 用于限制连接到该监听器的每个客户端的入站消息数，一旦达到限制值，受限制的客户端将会减速甚至暂时被挂起。


**listeners.ws.$name.bytes_rate**

  *类型*: `rate`

  数据发布速率。<br/> 用于限制连接到该监听器的每个客户端的入站字节速率，一旦达到限制值，受限制的客户端将会减速甚至暂时被挂起。


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


**listeners.ws.$name.tcp_options**

  *类型*: [broker:tcp_opts](#tcp_opts)


**listeners.ws.$name.websocket**

  *类型*: [broker:ws_opts](#ws_opts)



## MQTT/WebSocket with SSL 监听器 - 8084


Settings for the MQTT over WebSocket/SSL listener.

**listeners.wss.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启停监听器。


**listeners.wss.$name.bind**

  *类型*: `ip_port`

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


**listeners.wss.$name.enable_authn**

  *类型*: `enum`

  *默认值*: `true`

  *可选值*: `true | false | quick_deny_anonymous`

  配置 <code>true</code> （默认值）启用客户端进行身份认证，通过检查认配置的认认证器链来决定是否允许接入。
配置 <code>false</code> 时，将不对客户端做任何认证，任何客户端，不论是不是携带用户名等认证信息，都可以接入。
配置 <code>quick_deny_anonymous</code> 时，行为跟 <code>true</code> 类似，但是会对匿名
客户直接拒绝，不做使用任何认证器对客户端进行身份检查。


**listeners.wss.$name.max_conn_rate**

  *类型*: `rate`

  最大连接率。<br/> 用于限制该监听器的连接速率，一旦达到限制值，新的连接将被推迟或拒绝。


**listeners.wss.$name.messages_rate**

  *类型*: `rate`

  消息发布速率。<br/> 用于限制连接到该监听器的每个客户端的入站消息数，一旦达到限制值，受限制的客户端将会减速甚至暂时被挂起。


**listeners.wss.$name.bytes_rate**

  *类型*: `rate`

  数据发布速率。<br/> 用于限制连接到该监听器的每个客户端的入站字节速率，一旦达到限制值，受限制的客户端将会减速甚至暂时被挂起。


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


**listeners.wss.$name.tcp_options**

  *类型*: [broker:tcp_opts](#tcp_opts)


**listeners.wss.$name.ssl_options**

  *类型*: [broker:listener_wss_opts](#listener_wss_opts)


**listeners.wss.$name.websocket**

  *类型*: [broker:ws_opts](#ws_opts)



## MQTT 基本参数

全局的 MQTT 配置参数。


Global MQTT configuration.

**mqtt.idle_timeout**

  *类型*: `infinity | duration`

  *默认值*: `15s`

  设置连接被断开或进入休眠状态前的等待时间，空闲超时后，
  - 如暂未收到客户端的 CONNECT 报文，连接将断开；
  - 如已收到客户端的 CONNECT 报文，连接将进入休眠模式以节省系统资源。

注意：请合理设置该参数值，如等待时间设置过长，可能造成系统资源的浪费。


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


**mqtt.shared_subscription_strategy**

  *类型*: `enum`

  *默认值*: `round_robin`

  *可选值*: `random | round_robin | round_robin_per_group | sticky | local | hash_topic | hash_clientid`

  共享订阅消息派发策略。
  - `random`：随机挑选一个共享订阅者派发；
  - `round_robin`：使用 round-robin 策略派发；
  - `round_robin_per_group`：在共享组内循环选择下一个成员；
  - `local`：选择随机的本地成员，否则选择随机的集群范围内成员;
  - `sticky`：总是使用上次选中的订阅者派发，直到它断开连接；
  - `hash_clientid`：通过对发送者的客户端 ID 进行 Hash 处理来选择订阅者;
  - `hash_topic`：通过对源主题进行 Hash 处理来选择订阅者。


**mqtt.exclusive_subscription**

  *类型*: `boolean`

  *默认值*: `false`

  是否启用对 MQTT 排它订阅的支持。


**mqtt.ignore_loop_deliver**

  *类型*: `boolean`

  *默认值*: `false`

  设置由 MQTT v3.1.1/v3.1.0 客户端发布的消息是否将转发给其本身；类似 MQTT 5.0 协议中的 <code>No Local</code> 选项。


**mqtt.strict_mode**

  *类型*: `boolean`

  *默认值*: `false`

  是否以严格模式解析 MQTT 消息。
严格模式下，如客户端 ID、主题名称等中包含无效 utf8 字符串，连接将被断开。


**mqtt.response_information**

  *类型*: `string`

  *默认值*: `""`

  UTF-8 字符串，用于指定返回给客户端的响应主题，如 <code>reqrsp/</code>，此时请求和应答客户端都需要使用 <code>reqrsp/</code> 前缀的主题来完成通讯。
如希望禁用此功能，请在下方的文字框中输入<code>""</code>；仅适用于 MQTT 5.0 客户端。


**mqtt.server_keepalive**

  *类型*: `pos_integer | disabled`

  *默认值*: `disabled`

  EMQX 要求的保活时间，如设为 disabled，则将使用客户端指定的保持连接时间；仅适用于 MQTT 5.0 客户端。


**mqtt.keepalive_multiplier**

  *类型*: `number`

  *默认值*: `1.5`

  EMQX 判定客户端 Keep Alive 超时使用的 Keep Alive 倍数。计算公式为：Keep Alive 超时 = Keep Alive 间隔 × Keep Alive 倍数。 默认值 1.5 遵循 MQTT 5.0 规范。此倍数可调整，为系统管理员提供根据特定需求进行定制的灵活性。例如，如果客户端的 10 秒保持连接间隔的 PINGREQ 因为额外的10 秒延迟，将倍数更改为 2 可以让 EMQX 容忍此延迟。


**mqtt.retry_interval**

  *类型*: `duration`

  *默认值*: `30s`

  QoS 1/2 消息的重新投递间隔。


**mqtt.use_username_as_clientid**

  *类型*: `boolean`

  *默认值*: `false`

  是否使用用户名作为客户端 ID。
此设置的作用时间晚于 <code>对端证书作为用户名</code> 和 <code>对端证书作为客户端 ID</code>。


**mqtt.peer_cert_as_username**

  *类型*: `enum`

  *默认值*: `disabled`

  *可选值*: `disabled | cn | dn | crt | pem | md5`

  使用对端证书中的 CN、DN 字段或整个证书内容来作为用户名；仅适用于 TLS 连接。
目前支持：
- <code>cn</code>: 取证书的 CN 字段
- <code>dn</code>: 取证书的 DN 字段
- <code>crt</code>: 取 <code>DER</code> 或 <code>PEM</code> 的证书内容
- <code>pem</code>: 将 <code>DER</code> 证书转换为 <code>PEM</code> 格式作为用户名
- <code>md5</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书内容的 MD5 值


**mqtt.peer_cert_as_clientid**

  *类型*: `enum`

  *默认值*: `disabled`

  *可选值*: `disabled | cn | dn | crt | pem | md5`

  使用对端证书中的 CN、DN 字段或整个证书内容来作为客户端 ID。仅适用于 TLS 连接；
目前支持：
- <code>cn</code>: 取证书的 CN 字段
- <code>dn</code>: 取证书的 DN 字段
- <code>crt</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书的内容
- <code>pem</code>: 将 <code>DER</code> 证书内容转换为 <code>PEM</code> 格式作为客户端 ID
- <code>md5</code>: 取 <code>DER</code> 或 <code>PEM</code> 证书内容的 MD5 值


**mqtt.session_expiry_interval**

  *类型*: `duration`

  *默认值*: `2h`

  指定会话将在连接断开后多久过期，仅适用于非 MQTT 5.0 的连接。


**mqtt.max_awaiting_rel**

  *类型*: `non_neg_integer | infinity`

  *默认值*: `100`

  每个发布者的会话中，都存在一个队列来处理客户端发送的 QoS 2 消息。该队列会存储 QoS 2 消息的报文 ID 直到收到客户端的 PUBREL 或超时，达到队列长度的限制后，新的 QoS 2 消息发布会被拒绝，并返回 `147(0x93)` 错误。


**mqtt.max_qos_allowed**

  *类型*: `qos`

  *默认值*: `2`

  允许的最大 QoS 等级。


**mqtt.mqueue_priorities**

  *类型*: `disabled | map`

  *默认值*: `disabled`

  主题优先级。取值范围 [1-255]
默认优先级表为空，即所有的主题优先级相同。

注：优先主题名称中不支持使用逗号和等号。
注：不在此列表中的主题，被视为最高/最低优先级，这取决于<code>mqtt.mqueue_default_priority</code> 的配置。

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


**mqtt.max_mqueue_len**

  *类型*: `non_neg_integer | infinity`

  *默认值*: `1000`

  消息队列最大长度。持久客户端断开连接或飞行窗口已满时排队的消息长度。


**mqtt.max_inflight**

  *类型*: `integer`

  *默认值*: `32`

  *可选值*: `1-65535`

  允许在完成应答前同时投递的 QoS 1 和 QoS 2 消息的最大数量。


**mqtt.max_subscriptions**

  *类型*: `1..inf | infinity`

  *默认值*: `infinity`

  允许每个客户端建立的最大订阅数量。


**mqtt.upgrade_qos**

  *类型*: `boolean`

  *默认值*: `false`

  投递消息时，是否根据订阅主题时的 QoS 等级来强制提升派发的消息的 QoS 等级。


**mqtt.await_rel_timeout**

  *类型*: `duration`

  *默认值*: `300s`

  客户端发布 QoS 2 消息时，服务器等待 `PUBREL` 的最长时延。超过该时长后服务器会放弃等待，该PACKET ID 会被释放，从而允许后续新的 PUBLISH 消息使用。如果超时后收到 PUBREL，服务器将会产生一条告警日志。注意，向订阅客户端转发消息的动作发生在进入等待之前。



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

  *类型*: `timeout_duration_ms`

  *默认值*: `0s`

  消息清理间隔。0 代表不进行清理


**retainer.max_payload_size**

  *类型*: `bytesize`

  *默认值*: `1MB`

  消息大小最大值


**retainer.stop_publish_clear_msg**

  *类型*: `boolean`

  *默认值*: `false`

  是否不发送保留消息的清理消息，在 MQTT 5.0 中如果一条保留消息的消息体为空，则会清除掉之前存储
的对应的保留消息，通过这个值控制是否停止发送清理消息。


**retainer.delivery_rate**

  *类型*: `rate`

  发送保留消息的最大速率


**retainer.backend**

  *类型*: `retainer:mnesia_config`

  保留消息的存储后端



<!-- retainer:flow_control -->


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

<!-- @broker:shared_subscription_group@ -->

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

  是否启用


**delayed.max_delayed_messages**

  *类型*: `integer`

  *默认值*: `0`

  延迟消息的数量上限(0 代表不限数量)



<!-- ### 主题重写

 -->

<!-- ### 代理订阅



 -->

<!-- ## 日志追踪

 -->

## 集成 Prometheus


Prometheus 监控数据推送

**prometheus.push_gateway_server**

  *类型*: `string`

  *默认值*: `http://127.0.0.1:9091`

  Prometheus 服务器地址


**prometheus.interval**

  *类型*: `timeout_duration_ms`

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


**prometheus.vm_dist_collector**

  *类型*: `enum`

  *默认值*: `disabled`

  *可选值*: `disabled | enabled`

  开启或关闭 VM 分布采集器，收集 Erlang 分布机制中涉及的套接字和进程的信息。


**prometheus.mnesia_collector**

  *类型*: `enum`

  *默认值*: `disabled`

  *可选值*: `enabled | disabled`

  开启或关闭 Mnesia 采集器, 使用 mnesia:system_info/1 收集 Mnesia 相关指标


**prometheus.vm_statistics_collector**

  *类型*: `enum`

  *默认值*: `disabled`

  *可选值*: `enabled | disabled`

  开启或关闭 VM 统计采集器, 使用 erlang:statistics/1 收集 Erlang VM 相关指标


**prometheus.vm_system_info_collector**

  *类型*: `enum`

  *默认值*: `disabled`

  *可选值*: `enabled | disabled`

  开启或关闭 VM 系统信息采集器, 使用 erlang:system_info/1 收集 Erlang VM 相关指标


**prometheus.vm_memory_collector**

  *类型*: `enum`

  *默认值*: `disabled`

  *可选值*: `enabled | disabled`

  开启或关闭 VM 内存采集器, 使用 erlang:memory/0 收集 Erlang 虚拟机动态分配的内存信息，同时提供基本的 (D)ETS 统计信息


**prometheus.vm_msacc_collector**

  *类型*: `enum`

  *默认值*: `disabled`

  *可选值*: `enabled | disabled`

  开启或关闭 VM msacc 采集器, 使用 erlang:statistics(microstate_accounting) 收集微状态计数指标



<!-- ## 集成 StatsD

 -->

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

  *默认值*: `disabled`

  定期内存检查的时间间隔。


**sysmon.os.sysmem_high_watermark**

  *类型*: `percent`

  *默认值*: `70%`

  在发出相应报警之前可以分配多少系统内存的阈值，以系统内存的百分比表示。


**sysmon.os.procmem_high_watermark**

  *类型*: `percent`

  *默认值*: `5%`

  在发出相应警报之前，一个Erlang进程可以分配多少系统内存的阈值，以系统内存的百分比表示。



<!-- @broker:sysmon_top@ -->


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

有关速率限制的介绍以及使用请参考 [速率限制](../rate-limit/rate-limit.md)。

<!-- ## 过载保护

 -->

## 性能优化

<!-- ### broker_perf

 -->

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


**force_shutdown.max_mailbox_size**

  *类型*: `integer`

  *默认值*: `1000`

  *可选值*: `0-inf`

  每个在线客户端在 EMQX 服务器中都是独立的一个进程。该配置可以设为单个进程的邮箱消息队列设置最大长度，当超过该上限时，客户端会被强制下线。


**force_shutdown.max_heap_size**

  *类型*: `wordsize`

  *默认值*: `32MB`

  Heap 的总大小。



<!-- ### conn_congestion

 -->

### flapping_detect


This config controls the allowed maximum number of `CONNECT` packets received
from the same clientid in a time frame defined by `window_time`.
After the limit is reached, successive `CONNECT` requests are forbidden
(banned) until the end of the time period defined by `ban_time`.

**flapping_detect.enable**

  *类型*: `boolean`

  *默认值*: `false`

  启用抖动检测功能。


**flapping_detect.window_time**

  *类型*: `duration`

  *默认值*: `1m`

  抖动检测的时间窗口。


**flapping_detect.max_count**

  *类型*: `non_neg_integer`

  *默认值*: `15`

  MQTT 客户端在“窗口”时间内允许的最大断开次数。


**flapping_detect.ban_time**

  *类型*: `duration`

  *默认值*: `5m`

  抖动的客户端将会被禁止登录多长时间。



<!-- ### stats 统计

 -->

<!-- {% emqxce %} -->

<!-- ## 遥测 -->

<!-- @modules:telemetry@ -->

<!-- {% endemqxce %} -->

<!-- ## zone 配置 -->

<!-- #zone:overload_protection# -->

## Dashboard


EMQX Dashboard 配置。

**dashboard.listeners**

  *类型*: `dashboard:listeners`

  Dashboard 监听器设置。监听器必须有唯一的端口号和IP地址的组合。
例如，可以通过指定IP地址 0.0.0.0 来监听机器上给定端口上的所有配置的IP地址。
或者，可以为每个监听器指定唯一的IP地址，但使用相同的端口。


**dashboard.token_expired_time**

  *类型*: `duration`

  *默认值*: `60m`

  登录成功返回的 JWT token 过期时间，默认为 60 分钟。


**dashboard.cors**

  *类型*: `boolean`

  *默认值*: `false`

  CORS（Cross-Origin Resource Sharing，跨域资源共享）允许服务器响应来自任何来源（域名、协议或端口）的请求，启用后允许另一个域名下的服务直接通过 JavaScript 调用 EMQX REST API。


**dashboard.sso**

  *类型*: `sso`




Dashboard 监听器(HTTP)配置。

**dashboard.listeners.http.bind**

  *类型*: `ip_port`

  *默认值*: `0`

  监听地址和端口，热更新此配置时，会重启 Dashboard 服务。


**dashboard.listeners.http.num_acceptors**

  *类型*: `integer`

  *默认值*: `8`

  TCP 协议的 Socket acceptor 池大小, 通常配置为 CPU 核数


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

  发送响应内容的超时时间。


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

  启用 Proxy Protocol 以提取客户端连接的原始信息，要求使用了代理服务器并且代理服务器也启用 Proxy Protocol。注意：一旦开启了这个功能，就无法再处理普通的 HTTP 请求。




Dashboard 监听器(HTTPS)配置。

**dashboard.listeners.https.bind**

  *类型*: `ip_port`

  *默认值*: `0`

  监听地址和端口，热更新此配置时，会重启 Dashboard 服务。


**dashboard.listeners.https.ssl_options**

  *类型*: `dashboard:ssl_options`

  Dashboard 监听器的 SSL/TLS 选项。


**dashboard.listeners.https.num_acceptors**

  *类型*: `integer`

  *默认值*: `8`

  TCP 协议的 Socket acceptor 池大小, 通常配置为 CPU 核数


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

  发送响应内容的超时时间。


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

  启用 Proxy Protocol 以提取客户端连接的原始信息，要求使用了代理服务器并且代理服务器也启用 Proxy Protocol。注意：一旦开启了这个功能，就无法再处理普通的 HTTP 请求。




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

  用于在启动 EMQX 时，添加 API 密钥，其格式为 {appid}:{secret}，多个密钥用换行分隔。：
      ```
      7e729ae70d23144b:2QILI9AcQ9BYlVqLDHQNWN2saIjBV4egr1CZneTNKr9CpK
      ec3907f865805db0:Ee3taYltUKtoBVD9C3XjQl9C6NXheip8Z9B69BpUv5JxVHL
      ```



## 客户端认证 - 密码认证

### 使用内置数据库进行密码认证



### 使用 MySQL 进行密码认证



### 使用 MongoDB 进行密码认证

#### MongoDB 单节点



#### MongoDB Replica Set 集群



#### MongoDB Sharded 集群



### 使用 PostgreSQL 进行密码认证



### 使用 Redis 进行密码认证

#### Redis 单节点



#### Redis 集群



#### Redis Sentinel 集群



#### 使用 HTTP 服务进行密码认证

#### HTTP GET 方式



#### HTTP POST 方式



{% emqxee %}

### 使用 LDAP 进行密码认证



{% endemqxee %}

### 附录：认证 Hash 配置


Settings for simple algorithms.

**authentication.$INDEX.password_hash_algorithm.name**

  *类型*: `enum`

  *可选值*: `plain | md5 | sha | sha256 | sha512`

  Simple password hashing algorithm.


**authentication.$INDEX.password_hash_algorithm.salt_position**

  *类型*: `enum`

  *默认值*: `prefix`

  *可选值*: `disable | prefix | suffix`

  Salt position for PLAIN, MD5, SHA, SHA256 and SHA512 algorithms.




Settings for bcrypt password hashing algorithm.

**authentication.$INDEX.password_hash_algorithm.name**

  *类型*: `bcrypt`

  BCRYPT password hashing.




Settings for bcrypt password hashing algorithm (for DB backends with write capability).

**authentication.$INDEX.password_hash_algorithm.name**

  *类型*: `bcrypt`

  BCRYPT password hashing.


**authentication.$INDEX.password_hash_algorithm.salt_rounds**

  *类型*: `integer`

  *默认值*: `10`

  *可选值*: `5-10`

  Work factor for BCRYPT password generation.




Settings for PBKDF2 password hashing algorithm.

**authentication.$INDEX.password_hash_algorithm.name**

  *类型*: `pbkdf2`

  PBKDF2 password hashing.


**authentication.$INDEX.password_hash_algorithm.mac_fun**

  *类型*: `enum`

  *可选值*: `md4 | md5 | ripemd160 | sha | sha224 | sha256 | sha384 | sha512`

  Specifies mac_fun for PBKDF2 hashing algorithm.


**authentication.$INDEX.password_hash_algorithm.iterations**

  *类型*: `integer`

  Iteration count for PBKDF2 hashing algorithm.


**authentication.$INDEX.password_hash_algorithm.dk_length**

  *类型*: `integer`

  Derived length for PBKDF2 hashing algorithm. If not specified, calculated automatically based on `mac_fun`.



## 客户端认证 - JWT







## 客户端认证 - MQTT 增强认证



<!-- ### GCP IoT Core 认证 -->
<!-- "@authn:gcp_device@", -->

## 客户端认证 - PSK 认证


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




## 客户端授权

### 授权设置


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

  *默认值*: `[{"type":"file","path":"${EMQX_ETC_DIR}/acl.conf","enable":true}]`

  授权器列表。

配置多个授权器时，将按照顺序依次执行授权检查。
如果在当前授权检查器中未检索到权限数据，将会切换至链上的下一个已启用的授权器继续权限检查，直至得到 'allow' 或 'deny' 的结果。

如果在所有授权器中都未找到对应的客户端信息，则根据 `authorization.no_match` 配置的行为允许或拒绝当前操作。

注意：
- 数据源类型使用 'type' 进行标识；
- 使用同一类型的数据源只能被使用一次。






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



### 基于 ACL 文件进行授权



### 基于内置数据库进行授权



### 基于 MySQL 进行授权


### 基于 PostgreSQL 进行授权



### 基于 MongoDB 进行授权

### MongoDB 单节点



### MongoDB Replica Set 集群



#### MongoDB Sharded 集群



### 基于 Redis 进行授权

### Redis 单节点



### Redis 集群



### Redis Sentinel 集群



{% emqxee %}

### 基于 LDAP 进行授权



{% endemqxee %}

### 基于 HTTP 应用进行授权

#### HTTP GET 方式



#### HTTP POST 方式



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







### 规则动作







## 数据桥接

### MQTT


MQTT数据桥接的配置。

**bridges.mqtt.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用数据桥接


**bridges.mqtt.$name.resource_opts**

  *类型*: `bridge_mqtt:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.mqtt.$name.mode**

  *类型*: `enum`

  *可选值*: `cluster_shareload`

  Deprecated since v5.1.0 & e5.1.0.


**bridges.mqtt.$name.server**

  *类型*: `string`

  The host and port of the remote MQTT broker


**bridges.mqtt.$name.clientid_prefix**

  *类型*: `string`

  Optional prefix to prepend to the clientid used by egress bridges.


**bridges.mqtt.$name.reconnect_interval**

  *类型*: `string`

  Deprecated since v5.0.16.


**bridges.mqtt.$name.proto_ver**

  *类型*: `enum`

  *默认值*: `v4`

  *可选值*: `v3 | v4 | v5`

  The MQTT protocol version


**bridges.mqtt.$name.bridge_mode**

  *类型*: `boolean`

  *默认值*: `false`

  If enable bridge mode.
NOTE: This setting is only for MQTT protocol version older than 5.0, and the remote MQTT
broker MUST support this feature.
If bridge_mode is set to true, the bridge will indicate to the remote broker that it is a bridge not an ordinary client.
This means that loop detection will be more effective and that retained messages will be propagated correctly.


**bridges.mqtt.$name.username**

  *类型*: `string`

  The username of the MQTT protocol


**bridges.mqtt.$name.password**

  *类型*: `string`

  The password of the MQTT protocol


**bridges.mqtt.$name.clean_start**

  *类型*: `boolean`

  *默认值*: `true`

  Whether to start a clean session when reconnecting a remote broker for ingress bridge


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

  Max inflight (sent, but un-acked) messages of the MQTT protocol


**bridges.mqtt.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.mqtt.$name.ingress**

  *类型*: `connector-mqtt:ingress`

  The ingress config defines how this bridge receive messages from the remote MQTT broker, and then
        send them to the local broker.<br/>
        Template with variables is allowed in 'remote.qos', 'local.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
        NOTE: if this bridge is used as the input of a rule, and also 'local.topic' is
        configured, then messages got from the remote broker will be sent to both the 'local.topic' and
        the rule.


**bridges.mqtt.$name.egress**

  *类型*: `connector-mqtt:egress`

  The egress config defines how this bridge forwards messages from the local broker to the remote broker.<br/>
Template with variables is allowed in 'remote.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
NOTE: if this bridge is used as the action of a rule, and also 'local.topic'
is configured, then both the data got from the rule and the MQTT messages that matches
'local.topic' will be forwarded.




资源启动相关的选项。

**bridges.mqtt.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.mqtt.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.mqtt.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.mqtt.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.mqtt.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.mqtt.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.mqtt.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。


**bridges.mqtt.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.mqtt.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.mqtt.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



### WebHook


Configuration for an HTTP bridge.

**bridges.webhook.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  Enable or disable this bridge


**bridges.webhook.$name.resource_opts**

  *类型*: `bridge_webhook:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.webhook.$name.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  The timeout when connecting to the HTTP server.


**bridges.webhook.$name.retry_interval**

  *类型*: `timeout_duration`

  Deprecated since 5.0.4.


**bridges.webhook.$name.pool_type**

  *类型*: `emqx_bridge_http_connector:pool_type`

  *默认值*: `random`

  The type of the pool. Can be one of `random`, `hash`.


**bridges.webhook.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  The pool size.


**bridges.webhook.$name.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**bridges.webhook.$name.request**

  *类型*: `connector-http:request`

  Configure HTTP request parameters.


**bridges.webhook.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.webhook.$name.url**

  *类型*: `string`

  The URL of the HTTP Bridge.<br/>
Template with variables is allowed in the path, but variables cannot be used in the scheme, host,
or port part.<br/>
For example, <code> http://localhost:9901/${topic} </code> is allowed, but
<code> http://${host}:9901/message </code> or <code> http://localhost:${port}/message </code>
is not allowed.


**bridges.webhook.$name.direction**

  *类型*: `egress`

  Deprecated since 5.0.12.


**bridges.webhook.$name.local_topic**

  *类型*: `string`

  The MQTT topic filter to be forwarded to the HTTP server. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.webhook.$name.method**

  *类型*: `enum`

  *默认值*: `post`

  *可选值*: `post | put | get | delete`

  The method of the HTTP request. All the available methods are: post, put, get, delete.<br/>
Template with variables is allowed.


**bridges.webhook.$name.headers**

  *类型*: `map`

  *默认值*: `{"keep-alive":"timeout=5","content-type":"application/json","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  The headers of the HTTP request.<br/>
Template with variables is allowed.


**bridges.webhook.$name.body**

  *类型*: `string`

  The body of the HTTP request.<br/>
If not provided, the body will be a JSON object of all the available fields.<br/>
There, 'all the available fields' means the context of a MQTT message when
this webhook is triggered by receiving a MQTT message (the `local_topic` is set),
or the context of the event when this webhook is triggered by a rule (i.e. this
webhook is used as an action of a rule).<br/>
Template with variables is allowed.


**bridges.webhook.$name.max_retries**

  *类型*: `non_neg_integer`

  *默认值*: `2`

  HTTP request max retry times if failed.


**bridges.webhook.$name.request_timeout**

  *类型*: `duration_ms`

  Deprecated since v5.0.26.




资源启动相关的选项。

**bridges.webhook.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.webhook.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.webhook.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.webhook.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.webhook.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.webhook.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.webhook.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  从请求进入缓冲区的时刻开始，如果请求在指定的时间内仍然停留在缓冲区中，或者已经发送但没有及时收到响应或确认，该请求将被视为过期。


**bridges.webhook.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.webhook.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.webhook.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



### 连接配置




**connector-http:request.method**

  *类型*: `string`

  HTTP method.


**connector-http:request.path**

  *类型*: `string`

  URL path.


**connector-http:request.body**

  *类型*: `string`

  HTTP request body.


**connector-http:request.headers**

  *类型*: `map`

  List of HTTP headers.


**connector-http:request.max_retries**

  *类型*: `non_neg_integer`

  Max retry times if error on sending request.


**connector-http:request.request_timeout**

  *类型*: `timeout_duration_ms`

  HTTP request timeout.




The egress config defines how this bridge forwards messages from the local broker to the remote broker.<br/>
Template with variables is allowed in 'remote.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
NOTE: if this bridge is used as the action of a rule, and also 'local.topic'
is configured, then both the data got from the rule and the MQTT messages that matches
'local.topic' will be forwarded.

**bridges.mqtt.$name.egress.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  Size of the pool of MQTT clients that will publish messages to the remote broker.<br/>
Each MQTT client will be assigned 'clientid' of the form '${clientid_prefix}:${bridge_name}:egress:${node}:${n}'
where 'n' is the number of a client inside the pool.


**bridges.mqtt.$name.egress.local**

  *类型*: `connector-mqtt:egress_local`

  The configs about receiving messages from local broker.


**bridges.mqtt.$name.egress.remote**

  *类型*: `connector-mqtt:egress_remote`

  The configs about sending message to the remote broker.




The configs about receiving messages from local broker.

**bridges.mqtt.$name.egress.local.topic**

  *类型*: `string`

  The local topic to be forwarded to the remote broker




The configs about sending message to the remote broker.

**bridges.mqtt.$name.egress.remote.topic**

  *类型*: `string`

  Forward to which topic of the remote broker.<br/>
Template with variables is allowed.


**bridges.mqtt.$name.egress.remote.qos**

  *类型*: `qos | string`

  *默认值*: `1`

  The QoS of the MQTT message to be sent.<br/>
Template with variables is allowed.


**bridges.mqtt.$name.egress.remote.retain**

  *类型*: `boolean | string`

  *默认值*: `false`

  The 'retain' flag of the MQTT message to be sent.<br/>
Template with variables is allowed.


**bridges.mqtt.$name.egress.remote.payload**

  *类型*: `string`

  The payload of the MQTT message to be sent.<br/>
Template with variables is allowed.




The ingress config defines how this bridge receive messages from the remote MQTT broker, and then
        send them to the local broker.<br/>
        Template with variables is allowed in 'remote.qos', 'local.topic', 'local.qos', 'local.retain', 'local.payload'.<br/>
        NOTE: if this bridge is used as the input of a rule, and also 'local.topic' is
        configured, then messages got from the remote broker will be sent to both the 'local.topic' and
        the rule.

**bridges.mqtt.$name.ingress.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  Size of the pool of MQTT clients that will ingest messages from the remote broker.<br/>
This value will be respected only if 'remote.topic' is a shared subscription topic or topic-filter
(for example `$share/name1/topic1` or `$share/name2/topic2/#`), otherwise only a single MQTT client will be used.
Each MQTT client will be assigned 'clientid' of the form '${clientid_prefix}:${bridge_name}:ingress:${node}:${n}'
where 'n' is the number of a client inside the pool.
NOTE: Non-shared subscription will not work well when EMQX is clustered.


**bridges.mqtt.$name.ingress.remote**

  *类型*: `connector-mqtt:ingress_remote`

  The configs about subscribing to the remote broker.


**bridges.mqtt.$name.ingress.local**

  *类型*: `connector-mqtt:ingress_local`

  The configs about sending message to the local broker.




The configs about sending message to the local broker.

**bridges.mqtt.$name.ingress.local.topic**

  *类型*: `string`

  Send messages to which topic of the local broker.<br/>
Template with variables is allowed.


**bridges.mqtt.$name.ingress.local.qos**

  *类型*: `qos | string`

  *默认值*: `${qos}`

  The QoS of the MQTT message to be sent.<br/>
Template with variables is allowed.


**bridges.mqtt.$name.ingress.local.retain**

  *类型*: `boolean | string`

  *默认值*: `${retain}`

  The 'retain' flag of the MQTT message to be sent.<br/>
Template with variables is allowed.


**bridges.mqtt.$name.ingress.local.payload**

  *类型*: `string`

  The payload of the MQTT message to be sent.<br/>
Template with variables is allowed.




The configs about subscribing to the remote broker.

**bridges.mqtt.$name.ingress.remote.topic**

  *类型*: `string`

  Receive messages from which topic of the remote broker


**bridges.mqtt.$name.ingress.remote.qos**

  *类型*: `qos`

  *默认值*: `1`

  The QoS level to be used when subscribing to the remote broker



## 网关


EMQX Gateway configuration root.

**gateway.exproto**

  *类型*: `exproto`


**gateway.stomp**

  *类型*: `stomp`


**gateway.coap**

  *类型*: `coap`


**gateway.mqttsn**

  *类型*: `mqttsn`


**gateway.lwm2m**

  *类型*: `lwm2m`




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





### CoAP



### ExProto







### LwM2M





### MQTT-SN





### STOMP





### 网关可用监听器配置


Settings for TCP listener.

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

  *类型*: `emqx_gateway_schema:ip_port`

  监听器绑定的 IP 地址或端口。


**gateway:tcp_listener.max_connections**

  *类型*: `pos_integer | infinity`

  *默认值*: `1024`

  监听器支持的最大连接数。


**gateway:tcp_listener.max_conn_rate**

  *类型*: `integer`

  *默认值*: `1000`

  监听器支持的最大连接速率。


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

  从监听器名称到配置参数的映射。


**gateway.stomp.listeners.ssl**

  *类型*: `name`

  从监听器名称到配置参数的映射。




Settings for TCP and UDP listeners.

**gateway.exproto.listeners.tcp**

  *类型*: `name`

  从监听器名称到配置参数的映射。


**gateway.exproto.listeners.ssl**

  *类型*: `name`

  从监听器名称到配置参数的映射。


**gateway.exproto.listeners.udp**

  *类型*: `name`

  从监听器名称到配置参数的映射。


**gateway.exproto.listeners.dtls**

  *类型*: `name`

  从监听器名称到配置参数的映射。




Settings for DTLS listener.

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

  *类型*: `emqx_gateway_schema:ip_port`

  监听器绑定的 IP 地址或端口。


**gateway:dtls_listener.max_connections**

  *类型*: `pos_integer | infinity`

  *默认值*: `1024`

  监听器支持的最大连接数。


**gateway:dtls_listener.max_conn_rate**

  *类型*: `integer`

  *默认值*: `1000`

  监听器支持的最大连接速率。


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




Settings for DTLS protocol.

**gateway:dtls_opts.cacertfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**gateway:dtls_opts.cacerts**

  *类型*: `boolean`

  Deprecated since 5.1.4.


**gateway:dtls_opts.certfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**gateway:dtls_opts.keyfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/key.pem`

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

  *类型*: `non_neg_integer`

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

  *默认值*: `["dtlsv1.2"]`

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
如果打算使用PSK密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**gateway:dtls_opts.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**gateway:dtls_opts.log_level**

  *类型*: `enum`

  *默认值*: `notice`

  *可选值*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。


**gateway:dtls_opts.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**gateway:dtls_opts.dhfile**

  *类型*: `string`

  如果协商使用 Diffie-Hellman 密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数。<br/>
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


**gateway:dtls_opts.ocsp**

  *类型*: `broker:ocsp`


**gateway:dtls_opts.enable_crl_check**

  *类型*: `boolean`

  *默认值*: `false`

  是否为该监听器启用 CRL 检查。




Settings for UDP listener.

**gateway:udp_listener.udp_options**

  *类型*: `gateway:udp_opts`


**gateway:udp_listener.enable**

  *类型*: `boolean`

  *默认值*: `true`

  是否启用该监听器。


**gateway:udp_listener.bind**

  *类型*: `emqx_gateway_schema:ip_port`

  监听器绑定的 IP 地址或端口。


**gateway:udp_listener.max_connections**

  *类型*: `pos_integer | infinity`

  *默认值*: `1024`

  监听器支持的最大连接数。


**gateway:udp_listener.max_conn_rate**

  *类型*: `integer`

  *默认值*: `1000`

  监听器支持的最大连接速率。


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

  从监听器名称到配置参数的映射。


**gateway:udp_listeners.dtls**

  *类型*: `name`

  从监听器名称到配置参数的映射。




Settings for UDP sockets.

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




Settings for SSL listener.

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

  *类型*: `emqx_gateway_schema:ip_port`

  监听器绑定的 IP 地址或端口。


**gateway:ssl_listener.max_connections**

  *类型*: `pos_integer | infinity`

  *默认值*: `1024`

  监听器支持的最大连接数。


**gateway:ssl_listener.max_conn_rate**

  *类型*: `integer`

  *默认值*: `1000`

  监听器支持的最大连接速率。


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

  Deprecated since 5.0.24.




描述插件的状态

**plugins.states.$INDEX.name_vsn**

  *类型*: `string`

  插件的名称{name}-{version}。<br/>
它应该与插件的发布包名称一致，如my_plugin-0.1.0。


**plugins.states.$INDEX.enable**

  *类型*: `boolean`

  设置为“true”以启用此插件。



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

  *类型*: `timeout_duration`

  *默认值*: `5s`

  gRPC 服务器请求超时


**exhook.servers.$INDEX.failed_action**

  *类型*: `enum`

  *默认值*: `deny`

  *可选值*: `deny | ignore`

  当 gRPC 请求失败后的操作


**exhook.servers.$INDEX.ssl**

  *类型*: `exhook:ssl_conf`


**exhook.servers.$INDEX.socket_options**

  *类型*: `exhook:socket_options`

  *默认值*: `{"nodelay":true,"keepalive":true}`


**exhook.servers.$INDEX.auto_reconnect**

  *类型*: `false | timeout_duration`

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

  当没有其他数据交换时，是否向连接的对端套接字定期的发送探测包。如果另一端没有响应，则认为连接断开，并向控制进程发送错误消息。


**exhook.servers.$INDEX.socket_options.nodelay**

  *类型*: `boolean`

  *默认值*: `true`

  如果为 true，则为套接字设置 TCP_NODELAY 选项，这意味着会立即发送数据包。


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


**exhook.servers.$INDEX.ssl.cacerts**

  *类型*: `boolean`

  Deprecated since 5.1.4.


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

  *类型*: `non_neg_integer`

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

  *默认值*: `["tlsv1.3","tlsv1.2"]`

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
如果打算使用PSK密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

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


**exhook.servers.$INDEX.ssl.log_level**

  *类型*: `enum`

  *默认值*: `notice`

  *可选值*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。


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


**ssl_client_opts.cacerts**

  *类型*: `boolean`

  Deprecated since 5.1.4.


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

  *类型*: `non_neg_integer`

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

  *默认值*: `["tlsv1.3","tlsv1.2"]`

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
如果打算使用PSK密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**ssl_client_opts.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**ssl_client_opts.log_level**

  *类型*: `enum`

  *默认值*: `notice`

  *可选值*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。


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

  *默认值*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**listener_ssl_opts.cacerts**

  *类型*: `boolean`

  Deprecated since 5.1.4.


**listener_ssl_opts.certfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**listener_ssl_opts.keyfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/key.pem`

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

  *类型*: `non_neg_integer`

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

  *默认值*: `["tlsv1.3","tlsv1.2"]`

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
如果打算使用PSK密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**listener_ssl_opts.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**listener_ssl_opts.log_level**

  *类型*: `enum`

  *默认值*: `notice`

  *可选值*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。


**listener_ssl_opts.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**listener_ssl_opts.dhfile**

  *类型*: `string`

  如果协商使用 Diffie-Hellman 密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数。<br/>
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


**listener_ssl_opts.ocsp**

  *类型*: `broker:ocsp`


**listener_ssl_opts.enable_crl_check**

  *类型*: `boolean`

  *默认值*: `false`

  是否为该监听器启用 CRL 检查。



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


**tcp_opts.keepalive**

  *类型*: `string`

  *默认值*: `none`

  为 MQTT 连接在 TCP 或 SSL 上启用 TCP 保活。
值是以逗号分隔的三个数字，格式为 'Idle,Interval,Probes'
 - Idle: 在服务器开始发送保活探测之前，连接需要处于空闲状态的秒数（Linux 默认为7200）。
 - Interval: TCP 保活探测间隔的秒数（Linux默认值为75）。
 - Probes: 在放弃并终止连接之前，从另一端未获得响应时要发送的 TCP 保活探测的最大数量（Linux 默认值为 9 次）。
例如 "240,30,5" 表示：在连接空闲 240 秒后发送 TCP 保活探测，每隔 30 秒发送一次，直到收到响应，如果连续丢失 5 个响应，连接应该被关闭。
默认值为 'none'



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

  如果 <code>true</code>，则使用 <code>zlib</code> 压缩 WebSocket 消息<br/>
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

  如果设置为 <code>false</code> 并且 <code>check_origin_enable</code> 为 <code>true</code>，服务器将拒绝没有 <code>origin</code> HTTP头的请求。


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

  *默认值*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**listeners.wss.$name.ssl_options.cacerts**

  *类型*: `boolean`

  Deprecated since 5.1.4.


**listeners.wss.$name.ssl_options.certfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**listeners.wss.$name.ssl_options.keyfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/key.pem`

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

  *类型*: `non_neg_integer`

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

  *默认值*: `["tlsv1.3","tlsv1.2"]`

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
如果打算使用PSK密码套件, <code>tlsv1.3</code> 应在<code>ssl.versions</code>中禁用。

<br/>
PSK 密码套件：
<code>"RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,
RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,
RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA,
RSA-PSK-DES-CBC3-SHA,RSA-PSK-RC4-SHA"</code>


**listeners.wss.$name.ssl_options.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**listeners.wss.$name.ssl_options.log_level**

  *类型*: `enum`

  *默认值*: `notice`

  *可选值*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。


**listeners.wss.$name.ssl_options.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**listeners.wss.$name.ssl_options.dhfile**

  *类型*: `string`

  如果协商使用 Diffie-Hellman 密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数。<br/>
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

