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



## 集群设置


EMQX 节点可以组成一个集群，以提高总容量。<br/> 这里指定了节点之间如何连接。

**cluster.name**

  *类型*: `atom`

  *默认值*: `emqxcl`

  EMQX集群名称。每个集群都有一个唯一的名称。服务发现时会用于做路径的一部分。


**cluster.discovery_strategy**

  *类型*: `enum`

  *默认值*: `manual`

  *可选值*: `manual | static | dns | etcd | k8s | mcast`

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

  *可选值*: `inet_tcp | inet6_tcp | inet_tls`

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



<!-- ### 日志轮换

log_rotation
-->

<!-- ### 日志突发限制

log_burst_limit -->

<!-- ### 日志过载终止

log_overload_kill -->

{% emqxee %}

## License 设置


License provisioned as a string.

**license.key**

  *类型*: `string`

  *默认值*: `MjIwMTExCjAKMTAKRXZhbHVhdGlvbgpjb250YWN0QGVtcXguaW8KZGVmYXVsdAoyMDIzMDEwOQoxODI1CjEwMAo=.MEUCIG62t8W15g05f1cKx3tA3YgJoR0dmyHOPCdbUxBGxgKKAiEAhHKh8dUwhU+OxNEaOn8mgRDtiT3R8RZooqy6dEsOmDI=`

  许可证字符串


**license.connection_low_watermark**

  *类型*: `percent`

  *默认值*: `75%`

  低水位限制，低于此水位线时系统会清除连接配额使用告警


**license.connection_high_watermark**

  *类型*: `percent`

  *默认值*: `80%`

  高水位线，连接数超过这个水位线时，系统会触发许可证连接配额使用告警



{% endemqxee %}

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

  Maximum connection rate.<br/>
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.tcp.$name.messages_rate**

  *类型*: `rate`

  Messages publish rate.<br/>
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.tcp.$name.bytes_rate**

  *类型*: `rate`

  Data publish rate.<br/>
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


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

  *默认值*: `[]`

  Default authentication configs for all MQTT listeners.

For per-listener overrides see <code>authentication</code> in listener configs

This option can be configured with:
<ul>
  <li><code>[]</code>: The default value, it allows *ALL* logins</li>
  <li>one: For example <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
  <li>chain: An array of structs.</li>
</ul>

When a chain is configured, the login credentials are checked against the backends per the configured order, until an 'allow' or 'deny' decision can be made.

If there is no decision after a full chain exhaustion, the login is rejected.


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

  Maximum connection rate.<br/>
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.ssl.$name.messages_rate**

  *类型*: `rate`

  Messages publish rate.<br/>
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.ssl.$name.bytes_rate**

  *类型*: `rate`

  Data publish rate.<br/>
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


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

  *默认值*: `[]`

  Default authentication configs for all MQTT listeners.

For per-listener overrides see <code>authentication</code> in listener configs

This option can be configured with:
<ul>
  <li><code>[]</code>: The default value, it allows *ALL* logins</li>
  <li>one: For example <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
  <li>chain: An array of structs.</li>
</ul>

When a chain is configured, the login credentials are checked against the backends per the configured order, until an 'allow' or 'deny' decision can be made.

If there is no decision after a full chain exhaustion, the login is rejected.


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

  Maximum connection rate.<br/>
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.quic.$name.messages_rate**

  *类型*: `rate`

  Messages publish rate.<br/>
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.quic.$name.bytes_rate**

  *类型*: `rate`

  Data publish rate.<br/>
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.




TLS options for QUIC transport.

**listeners.quic.$name.ssl_options.cacertfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**listeners.quic.$name.ssl_options.certfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**listeners.quic.$name.ssl_options.keyfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/key.pem`

  PEM格式的私钥文件。


**listeners.quic.$name.ssl_options.verify**

  *类型*: `enum`

  *默认值*: `verify_none`

  *可选值*: `verify_peer | verify_none`

  启用或禁用对等验证。


**listeners.quic.$name.ssl_options.password**

  *类型*: `string`

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。



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

  Maximum connection rate.<br/>
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.ws.$name.messages_rate**

  *类型*: `rate`

  Messages publish rate.<br/>
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.ws.$name.bytes_rate**

  *类型*: `rate`

  Data publish rate.<br/>
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


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

  *默认值*: `[]`

  Default authentication configs for all MQTT listeners.

For per-listener overrides see <code>authentication</code> in listener configs

This option can be configured with:
<ul>
  <li><code>[]</code>: The default value, it allows *ALL* logins</li>
  <li>one: For example <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
  <li>chain: An array of structs.</li>
</ul>

When a chain is configured, the login credentials are checked against the backends per the configured order, until an 'allow' or 'deny' decision can be made.

If there is no decision after a full chain exhaustion, the login is rejected.


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

  Maximum connection rate.<br/>
This is used to limit the connection rate for this listener,
once the limit is reached, new connections will be deferred or refused


**listeners.wss.$name.messages_rate**

  *类型*: `rate`

  Messages publish rate.<br/>
This is used to limit the inbound message numbers for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


**listeners.wss.$name.bytes_rate**

  *类型*: `rate`

  Data publish rate.<br/>
This is used to limit the inbound bytes rate for each client connected to this listener,
once the limit is reached, the restricted client will slow down and even be hung for a while.


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

  *默认值*: `[]`

  Default authentication configs for all MQTT listeners.

For per-listener overrides see <code>authentication</code> in listener configs

This option can be configured with:
<ul>
  <li><code>[]</code>: The default value, it allows *ALL* logins</li>
  <li>one: For example <code>{enable:true,backend:"built_in_database",mechanism="password_based"}</code></li>
  <li>chain: An array of structs.</li>
</ul>

When a chain is configured, the login credentials are checked against the backends per the configured order, until an 'allow' or 'deny' decision can be made.

If there is no decision after a full chain exhaustion, the login is rejected.


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

  EMQX 判定客户端保活超时使用的阈值系数。计算公式为：Keep Alive * Backoff * 2


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
的对应的保留消息，通过这个值控制是否停止发送清理消息


**retainer.delivery_rate**

  *类型*: `rate`

  The maximum rate of delivering retained messages


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

<!-- broker:shared_subscription_group@ -->

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



<!-- ### 主题重写

modules:rewrite@ -->

<!-- ### 代理订阅

auto_subscribe@

auto_subscribe:topic@ -->

<!-- ## 日志追踪

broker:trace@ -->

{% emqxee %}

## MQTT 文件传输

### 文件传输设置


File transfer settings

**file_transfer.enable**

  *类型*: `boolean`

  *默认值*: `false`

  启用文件传输（File Transfer）服务。<br/>
文件传输服务允许客户端使用 `$file` 主题将分片上传到 EMQX。
启用文件传输服务表示 `$file` 主题前缀已被预留，用于提供文件传输服务。<br/>
这个开关也会影响文件传输 REST API 的可用性，以及依赖存储的后台任务（例如垃圾回收）。


**file_transfer.init_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `10s`

  指定文件传输初始化的时间限制。在<br/>
EMQX 服务器过载时可能发生初始化超时。超时后给 `init` 的 PUBACK 中包含一个错误码 （0x80）。


**file_transfer.store_segment_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5m`

  文件片段保存超时。<br/>
EMQX 收到文件分片后，会对它进行保存，如果发生超时（例如系统过载），则会给这个发布消息的 PUBACK 中包含一个错误码（0x80）。


**file_transfer.assemble_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5m`

  文件拼接超时。<br/>
在收到 'fin' 消息后 EMQX 会对文件进行拼接，如果发生超时（例如系统过载），则会给 'fin' 的 PUBACK 中包含一个错误码（0x80）。


**file_transfer.storage**

  *类型*: `file_transfer:storage_backend`

  *默认值*: `{"local":{}}`

  文件后端存储配置




File transfer local storage settings

**file_transfer.storage.local.segments**

  *类型*: `file_transfer:local_storage_segments`

  *默认值*: `{"gc":{}}`

  本地文件系统存储配置，包括已上传的文件分片和临时数据。


**file_transfer.storage.local.exporter**

  *类型*: `file_transfer:local_storage_exporter_backend`

  *默认值*: `{"local":{}}`

  将文件导出到本地存储<br/>
该配置项指定所有分片都传输完成了的文件进行导出到本地存储的相关行为。


**file_transfer.storage.local.enable**

  *类型*: `boolean`

  *默认值*: `true`

  Whether to enable this backend.




File transfer local segments storage settings

**file_transfer.storage.local.segments.root**

  *类型*: `string`

  文件分片存储的根目录。


**file_transfer.storage.local.segments.gc**

  *类型*: `file_transfer:local_storage_segments_gc`

  文件系统中临时文件的垃圾回收配置。




Garbage collection settings for the File transfer local segments storage

**file_transfer.storage.local.segments.gc.interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `1h`

  运行垃圾回收的时间间隔。


**file_transfer.storage.local.segments.gc.maximum_segments_ttl**

  *类型*: `duration_s`

  *默认值*: `24h`

  分片的临时存储的最大 TTL。<br/>
该配置为系统全局上限，所有的分片都不会超过这个 TTL，即使某些文件传输指定了一个更大的 TTL。


**file_transfer.storage.local.segments.gc.minimum_segments_ttl**

  *类型*: `duration_s`

  *默认值*: `5m`

  分片的临时存储的最小 TTL。<br/>
分片在超过这个 TTL 之前不被垃圾回收。
该配置为系统全局下限，所有的分片都不会低于这个 TTL，即使某些文件传输指定了一个更小的 TTL。



{% endemqxee %}

### 导出文件到本地磁盘


Local Exporter settings for the File transfer local storage backend

**file_transfer.storage.local.exporter.local.root**

  *类型*: `string`

  导出到本地文件时使用的根目录。


**file_transfer.storage.local.exporter.local.enable**

  *类型*: `boolean`

  *默认值*: `true`

  Whether to enable this backend.




Exporter for the local file system storage backend

**file_transfer.storage.local.exporter.local**

  *类型*: `file_transfer:local_storage_exporter`

  将文件导出到本地存储。


**file_transfer.storage.local.exporter.s3**

  *类型*: `file_transfer:s3_exporter`

  将文件导出到 AWS s3 API 兼容的对象存储服务。



### 导出文件到 S3 存储


S3 Exporter settings for the File transfer local storage backend

**file_transfer.storage.local.exporter.s3.access_key_id**

  *类型*: `string`

  The access key ID of the S3 bucket.


**file_transfer.storage.local.exporter.s3.secret_access_key**

  *类型*: `emqx_s3_schema:secret_access_key`

  The secret access key of the S3 bucket.


**file_transfer.storage.local.exporter.s3.bucket**

  *类型*: `string`

  The name of the S3 bucket.


**file_transfer.storage.local.exporter.s3.host**

  *类型*: `string`

  The host of the S3 endpoint.


**file_transfer.storage.local.exporter.s3.port**

  *类型*: `pos_integer`

  The port of the S3 endpoint.


**file_transfer.storage.local.exporter.s3.url_expire_time**

  *类型*: `duration_s`

  *默认值*: `1h`

  The time in seconds for which the signed URLs to the S3 objects are valid.


**file_transfer.storage.local.exporter.s3.min_part_size**

  *类型*: `bytesize`

  *默认值*: `5mb`

  The minimum part size for multipart uploads.<br/>
Uploaded data will be accumulated in memory until this size is reached.


**file_transfer.storage.local.exporter.s3.max_part_size**

  *类型*: `bytesize`

  *默认值*: `5gb`

  The maximum part size for multipart uploads.<br/>
S3 uploader won't try to upload parts larger than this size.


**file_transfer.storage.local.exporter.s3.acl**

  *类型*: `enum`

  *可选值*: `private | public_read | public_read_write | authenticated_read | bucket_owner_read | bucket_owner_full_control`

  The ACL to use for the uploaded objects.


**file_transfer.storage.local.exporter.s3.transport_options**

  *类型*: `s3:transport_options`

  Options for the HTTP transport layer used by the S3 client.


**file_transfer.storage.local.exporter.s3.enable**

  *类型*: `boolean`

  *默认值*: `true`

  Whether to enable this backend.




Storage backend settings for file transfer

**file_transfer.storage.local**

  *类型*: `file_transfer:local_storage`

  EMQX 节点本地用文件系统于存储文件分片的相关配置。




Options for the HTTP transport layer used by the S3 client

**file_transfer.storage.local.exporter.s3.transport_options.ipv6_probe**

  *类型*: `boolean`

  *默认值*: `false`

  Whether to probe for IPv6 support.


**file_transfer.storage.local.exporter.s3.transport_options.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  The timeout when connecting to the HTTP server.


**file_transfer.storage.local.exporter.s3.transport_options.pool_type**

  *类型*: `emqx_bridge_http_connector:pool_type`

  *默认值*: `random`

  The type of the pool. Can be one of `random`, `hash`.


**file_transfer.storage.local.exporter.s3.transport_options.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  The pool size.


**file_transfer.storage.local.exporter.s3.transport_options.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**file_transfer.storage.local.exporter.s3.transport_options.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**file_transfer.storage.local.exporter.s3.transport_options.headers**

  *类型*: `map`

  List of HTTP headers.


**file_transfer.storage.local.exporter.s3.transport_options.max_retries**

  *类型*: `non_neg_integer`

  Max retry times if error on sending request.


**file_transfer.storage.local.exporter.s3.transport_options.request_timeout**

  *类型*: `timeout_duration_ms`

  HTTP request timeout.



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



<!-- TODO 5.2 -->
## 集成 OpenTelemetry


Open Telemetry Toolkit configuration

**opentelemetry.exporter**

  *类型*: `opentelemetry:exporter`

  Open Telemetry Exporter


**opentelemetry.enable**

  *类型*: `boolean`

  *默认值*: `false`

  Enable or disable open telemetry metrics




Open Telemetry Exporter

**opentelemetry.exporter.endpoint**

  *类型*: `url`

  *默认值*: `http://localhost:4317`

  Open Telemetry Exporter Endpoint


**opentelemetry.exporter.interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `10s`

  The interval of sending metrics to Open Telemetry Endpoint



<!-- ## 集成 StatsD

statsd@ -->

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



<!-- ## 主题统计

配置需要统计详细消息流转数据的主题。

modules:topic_metrics@ -->

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



<!-- broker:sysmon_top@ -->


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

broker:overload_protection@ -->

## 性能优化

<!-- ### broker_perf

broker:broker_perf@ -->

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

broker:conn_congestion@ -->

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

broker:stats@ -->

<!-- {% emqxce %} -->

<!-- ## 遥测 -->

<!-- modules:telemetry@ -->

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

  JWT token 过期时间。默认设置为 60 分钟。


**dashboard.cors**

  *类型*: `boolean`

  *默认值*: `false`

  支持跨域资源共享(CORS)，
允许服务器指示任何来源(域名、协议或端口)，除了本服务器之外的任何浏览器应允许加载资源。




Dashboard 监听器(HTTP)配置。

**dashboard.listeners.http.bind**

  *类型*: `ip_port`

  *默认值*: `0`

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

**dashboard.listeners.https.bind**

  *类型*: `ip_port`

  *默认值*: `0`

  监听地址和端口，热更新此配置时，会重启 Dashboard 服务。


**dashboard.listeners.https.ssl_options**

  *类型*: `dashboard:ssl_options`

  SSL/TLS options for the dashboard listener.


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




Dashboard 监听器配置。

**dashboard.listeners.http**

  *类型*: `dashboard:http`

  TCP listeners


**dashboard.listeners.https**

  *类型*: `dashboard:https`

  SSL listeners




SSL/TLS options for the dashboard listener.

**dashboard.listeners.https.ssl_options.cacertfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**dashboard.listeners.https.ssl_options.cacerts**

  *类型*: `boolean`

  Deprecated since 5.1.4.


**dashboard.listeners.https.ssl_options.certfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**dashboard.listeners.https.ssl_options.keyfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/key.pem`

  PEM格式的私钥文件。


**dashboard.listeners.https.ssl_options.verify**

  *类型*: `enum`

  *默认值*: `verify_none`

  *可选值*: `verify_peer | verify_none`

  启用或禁用对等验证。


**dashboard.listeners.https.ssl_options.reuse_sessions**

  *类型*: `boolean`

  *默认值*: `true`

  启用 TLS 会话重用。


**dashboard.listeners.https.ssl_options.depth**

  *类型*: `non_neg_integer`

  *默认值*: `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
因此，如果深度为0，则对等方必须由受信任的根 CA 直接签名；<br/>
如果是1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
如果是2，则路径可以是PEER、中间 CA1、中间 CA2、ROOT-CA。


**dashboard.listeners.https.ssl_options.password**

  *类型*: `string`

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。


**dashboard.listeners.https.ssl_options.versions**

  *类型*: `array`

  *默认值*: `["tlsv1.3","tlsv1.2"]`

  支持所有TLS/DTLS版本<br/>
注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


**dashboard.listeners.https.ssl_options.ciphers**

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


**dashboard.listeners.https.ssl_options.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**dashboard.listeners.https.ssl_options.log_level**

  *类型*: `enum`

  *默认值*: `notice`

  *可选值*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。


**dashboard.listeners.https.ssl_options.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**dashboard.listeners.https.ssl_options.dhfile**

  *类型*: `string`

  如果协商使用Diffie-Hellman密钥交换的密码套件，则服务器将使用包含PEM编码的Diffie-Hellman参数的文件的路径。如果未指定，则使用默认参数。<br/>
注意：TLS 1.3不支持<code>dhfile</code>选项。


**dashboard.listeners.https.ssl_options.honor_cipher_order**

  *类型*: `boolean`

  *默认值*: `true`

  一个重要的安全设置，它强制根据服务器指定的顺序而不是客户机指定的顺序设置密码，从而强制服务器管理员执行（通常配置得更正确）安全顺序。


**dashboard.listeners.https.ssl_options.client_renegotiation**

  *类型*: `boolean`

  *默认值*: `true`

  在支持客户机发起的重新协商的协议中，这种操作的资源成本对于服务器来说高于客户机。
这可能会成为拒绝服务攻击的载体。
SSL 应用程序已经采取措施来反击此类尝试，但通过将此选项设置为 false，可以严格禁用客户端发起的重新协商。
默认值为 true。请注意，由于基础密码套件可以加密的消息数量有限，禁用重新协商可能会导致长期连接变得不可用。


**dashboard.listeners.https.ssl_options.handshake_timeout**

  *类型*: `duration`

  *默认值*: `15s`

  握手完成所允许的最长时间



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



## 客户端认证 - 密码认证

### 使用内置数据库进行密码认证


使用内置数据库作为认证数据源的认证器的配置项。

**authn:builtin_db.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:builtin_db.backend**

  *类型*: `built_in_database`

  后端类型。


**authn:builtin_db.user_id_type**

  *类型*: `enum`

  *默认值*: `username`

  *可选值*: `clientid | username`

  指定使用客户端ID `clientid` 还是用户名 `username` 进行认证。


**authn:builtin_db.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt_rw](#authn-hash:bcrypt_rw) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash creation and verification.


**authn:builtin_db.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。



### 使用 MySQL 进行密码认证


使用 MySQL 作为认证数据源的认证器的配置项。

**authn:mysql.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:mysql.backend**

  *类型*: `mysql`

  后端类型。


**authn:mysql.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:mysql.query**

  *类型*: `string`

  用于查询密码散列等用于认证的数据的 SQL 语句。


**authn:mysql.query_timeout**

  *类型*: `duration_ms`

  *默认值*: `5s`

  SQL 查询的超时时间。


**authn:mysql.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:mysql.server**

  *类型*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MySQL default port 3306 is used if `[:Port]` is not specified.


**authn:mysql.database**

  *类型*: `string`

  数据库名字。


**authn:mysql.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn:mysql.username**

  *类型*: `string`

  *默认值*: `root`

  内部数据库的用户名。


**authn:mysql.password**

  *类型*: `string`

  内部数据库密码。


**authn:mysql.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authn:mysql.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### 使用 MongoDB 进行密码认证

#### MongoDB 单节点


使用 MongoDB (Standalone) 作为认证数据源的认证器的配置项。

**authn:mongo_single.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:mongo_single.backend**

  *类型*: `mongodb`

  后端类型。


**authn:mongo_single.collection**

  *类型*: `string`

  存储认证数据的集合。


**authn:mongo_single.filter**

  *类型*: `map`

  *默认值*: `{}`

  在查询中定义过滤条件的条件表达式。
过滤器支持如下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


**authn:mongo_single.password_hash_field**

  *类型*: `string`

  *默认值*: `password_hash`

  文档中用于存放密码散列的字段。


**authn:mongo_single.salt_field**

  *类型*: `string`

  *默认值*: `salt`

  文档中用于存放盐值的字段。


**authn:mongo_single.is_superuser_field**

  *类型*: `string`

  *默认值*: `is_superuser`

  文档中用于定义用户是否具有超级用户权限的字段。


**authn:mongo_single.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:mongo_single.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:mongo_single.mongo_type**

  *类型*: `single`

  *默认值*: `single`

  Standalone 模式。当 MongoDB 服务运行在 standalone 模式下，该配置必须设置为 'single'。


**authn:mongo_single.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**authn:mongo_single.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**authn:mongo_single.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**authn:mongo_single.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn:mongo_single.username**

  *类型*: `string`

  内部数据库的用户名。


**authn:mongo_single.password**

  *类型*: `string`

  内部数据库密码。


**authn:mongo_single.use_legacy_protocol**

  *类型*: `enum`

  *默认值*: `auto`

  *可选值*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authn:mongo_single.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**authn:mongo_single.database**

  *类型*: `string`

  数据库名字。


**authn:mongo_single.topology**

  *类型*: `topology`


**authn:mongo_single.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### MongoDB Replica Set 集群


使用 MongoDB (Replica Set) 作为认证数据源的认证器的配置项。

**authn:mongo_rs.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:mongo_rs.backend**

  *类型*: `mongodb`

  后端类型。


**authn:mongo_rs.collection**

  *类型*: `string`

  存储认证数据的集合。


**authn:mongo_rs.filter**

  *类型*: `map`

  *默认值*: `{}`

  在查询中定义过滤条件的条件表达式。
过滤器支持如下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


**authn:mongo_rs.password_hash_field**

  *类型*: `string`

  *默认值*: `password_hash`

  文档中用于存放密码散列的字段。


**authn:mongo_rs.salt_field**

  *类型*: `string`

  *默认值*: `salt`

  文档中用于存放盐值的字段。


**authn:mongo_rs.is_superuser_field**

  *类型*: `string`

  *默认值*: `is_superuser`

  文档中用于定义用户是否具有超级用户权限的字段。


**authn:mongo_rs.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:mongo_rs.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:mongo_rs.mongo_type**

  *类型*: `rs`

  *默认值*: `rs`

  Replica set模式。当 MongoDB 服务运行在 replica-set 模式下，该配置必须设置为 'rs'。


**authn:mongo_rs.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**authn:mongo_rs.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**authn:mongo_rs.r_mode**

  *类型*: `enum`

  *默认值*: `master`

  *可选值*: `master | slave_ok`

  读模式。


**authn:mongo_rs.replica_set_name**

  *类型*: `string`

  副本集的名称。


**authn:mongo_rs.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**authn:mongo_rs.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn:mongo_rs.username**

  *类型*: `string`

  内部数据库的用户名。


**authn:mongo_rs.password**

  *类型*: `string`

  内部数据库密码。


**authn:mongo_rs.use_legacy_protocol**

  *类型*: `enum`

  *默认值*: `auto`

  *可选值*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authn:mongo_rs.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**authn:mongo_rs.database**

  *类型*: `string`

  数据库名字。


**authn:mongo_rs.topology**

  *类型*: `topology`


**authn:mongo_rs.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### MongoDB Sharded 集群


使用 MongoDB (Sharded Cluster) 作为认证数据源的认证器的配置项。

**authn:mongo_sharded.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:mongo_sharded.backend**

  *类型*: `mongodb`

  后端类型。


**authn:mongo_sharded.collection**

  *类型*: `string`

  存储认证数据的集合。


**authn:mongo_sharded.filter**

  *类型*: `map`

  *默认值*: `{}`

  在查询中定义过滤条件的条件表达式。
过滤器支持如下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符


**authn:mongo_sharded.password_hash_field**

  *类型*: `string`

  *默认值*: `password_hash`

  文档中用于存放密码散列的字段。


**authn:mongo_sharded.salt_field**

  *类型*: `string`

  *默认值*: `salt`

  文档中用于存放盐值的字段。


**authn:mongo_sharded.is_superuser_field**

  *类型*: `string`

  *默认值*: `is_superuser`

  文档中用于定义用户是否具有超级用户权限的字段。


**authn:mongo_sharded.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:mongo_sharded.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:mongo_sharded.mongo_type**

  *类型*: `sharded`

  *默认值*: `sharded`

  Sharded cluster模式。当 MongoDB 服务运行在 sharded 模式下，该配置必须设置为 'sharded'。


**authn:mongo_sharded.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**authn:mongo_sharded.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**authn:mongo_sharded.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**authn:mongo_sharded.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn:mongo_sharded.username**

  *类型*: `string`

  内部数据库的用户名。


**authn:mongo_sharded.password**

  *类型*: `string`

  内部数据库密码。


**authn:mongo_sharded.use_legacy_protocol**

  *类型*: `enum`

  *默认值*: `auto`

  *可选值*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**authn:mongo_sharded.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**authn:mongo_sharded.database**

  *类型*: `string`

  数据库名字。


**authn:mongo_sharded.topology**

  *类型*: `topology`


**authn:mongo_sharded.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### 使用 PostgreSQL 进行密码认证


使用 PostgreSQL 作为认证数据源的认证器的配置项。

**authn:postgresql.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:postgresql.backend**

  *类型*: `postgresql`

  后端类型。


**authn:postgresql.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:postgresql.query**

  *类型*: `string`

  用于查询密码散列等用于认证的数据的 SQL 语句。


**authn:postgresql.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:postgresql.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 PostgreSQL 默认端口 5432。


**authn:postgresql.database**

  *类型*: `string`

  数据库名字。


**authn:postgresql.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn:postgresql.username**

  *类型*: `string`

  内部数据库的用户名。


**authn:postgresql.password**

  *类型*: `string`

  内部数据库密码。


**authn:postgresql.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authn:postgresql.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### 使用 Redis 进行密码认证

#### Redis 单节点


使用 Redis (Standalone) 作为认证数据源的认证器的配置项。

**authn:redis_single.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:redis_single.backend**

  *类型*: `redis`

  后端类型。


**authn:redis_single.cmd**

  *类型*: `string`

  用于查询密码散列等用于认证的数据的 Redis Command，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。


**authn:redis_single.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:redis_single.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:redis_single.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**authn:redis_single.redis_type**

  *类型*: `single`

  *默认值*: `single`

  单机模式。当 Redis 服务运行在单机模式下，该配置必须设置为 'single'。


**authn:redis_single.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn:redis_single.username**

  *类型*: `string`

  内部数据库的用户名。


**authn:redis_single.password**

  *类型*: `string`

  内部数据库密码。


**authn:redis_single.database**

  *类型*: `non_neg_integer`

  *默认值*: `0`

  Redis 数据库 ID。


**authn:redis_single.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authn:redis_single.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### Redis 集群


使用 Redis (Cluster) 作为认证数据源的认证器的配置项。

**authn:redis_cluster.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:redis_cluster.backend**

  *类型*: `redis`

  后端类型。


**authn:redis_cluster.cmd**

  *类型*: `string`

  用于查询密码散列等用于认证的数据的 Redis Command，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。


**authn:redis_cluster.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:redis_cluster.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:redis_cluster.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**authn:redis_cluster.redis_type**

  *类型*: `cluster`

  *默认值*: `cluster`

  集群模式。当 Redis 服务运行在集群模式下，该配置必须设置为 'cluster'。


**authn:redis_cluster.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn:redis_cluster.username**

  *类型*: `string`

  内部数据库的用户名。


**authn:redis_cluster.password**

  *类型*: `string`

  内部数据库密码。


**authn:redis_cluster.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authn:redis_cluster.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### Redis Sentinel 集群


使用 Redis (Sentinel) 作为认证数据源的认证器的配置项。

**authn:redis_sentinel.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:redis_sentinel.backend**

  *类型*: `redis`

  后端类型。


**authn:redis_sentinel.cmd**

  *类型*: `string`

  用于查询密码散列等用于认证的数据的 Redis Command，目前仅支持 <code>HGET</code> 与 <code>HMGET</code>。


**authn:redis_sentinel.password_hash_algorithm**

  *类型*: [authn-hash:bcrypt](#authn-hash:bcrypt) | [authn-hash:pbkdf2](#authn-hash:pbkdf2) | [authn-hash:simple](#authn-hash:simple)

  *默认值*: `{"salt_position":"prefix","name":"sha256"}`

  Options for password hash verification.


**authn:redis_sentinel.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:redis_sentinel.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**authn:redis_sentinel.redis_type**

  *类型*: `sentinel`

  *默认值*: `sentinel`

  哨兵模式。当 Redis 服务运行在哨兵模式下，该配置必须设置为 'sentinel'。


**authn:redis_sentinel.sentinel**

  *类型*: `string`

  Redis 哨兵模式下的集群名称。


**authn:redis_sentinel.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn:redis_sentinel.username**

  *类型*: `string`

  内部数据库的用户名。


**authn:redis_sentinel.password**

  *类型*: `string`

  内部数据库密码。


**authn:redis_sentinel.database**

  *类型*: `non_neg_integer`

  *默认值*: `0`

  Redis 数据库 ID。


**authn:redis_sentinel.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**authn:redis_sentinel.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### 使用 HTTP 服务进行密码认证

#### HTTP GET 方式


使用 HTTP Server 作为认证服务的认证器的配置项 (使用 GET 请求)。

**authn:http_get.method**

  *类型*: `get`

  HTTP 请求方法。


**authn:http_get.headers**

  *类型*: `map`

  *默认值*: `{"keep-alive":"timeout=30, max=1000","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  HTTP Headers 列表 (无 <code>content-type</code>) 。


**authn:http_get.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:http_get.backend**

  *类型*: `http`

  后端类型。


**authn:http_get.url**

  *类型*: `string`

  认证 HTTP 服务器地址。


**authn:http_get.body**

  *类型*: `#{term => binary()}`

  HTTP request body，JSON 字符模板，支持使用占位符。
使用 GET 请求时，将被转换为 URL Query。
使用 POST 请求时，转换格式将取决于 content-type 头被设置为 application/json 还是 application/x-www-form-urlencoded。



**authn:http_get.request_timeout**

  *类型*: `duration_ms`

  *默认值*: `5s`

  HTTP 请求超时时长。


**authn:http_get.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:http_get.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  The timeout when connecting to the HTTP server.


**authn:http_get.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**authn:http_get.max_retries**

  *类型*: `non_neg_integer`

  Deprecated since 5.0.4.


**authn:http_get.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  The pool size.


**authn:http_get.request**

  *类型*: `connector-http:request`

  Configure HTTP request parameters.


**authn:http_get.retry_interval**

  *类型*: `timeout_duration`

  Deprecated since 5.0.4.


**authn:http_get.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



#### HTTP POST 方式


使用 HTTP Server 作为认证服务的认证器的配置项 (使用 POST 请求)。

**authn:http_post.method**

  *类型*: `post`

  HTTP 请求方法。


**authn:http_post.headers**

  *类型*: `map`

  *默认值*: `{"keep-alive":"timeout=30, max=1000","content-type":"application/json","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  HTTP Headers 列表


**authn:http_post.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:http_post.backend**

  *类型*: `http`

  后端类型。


**authn:http_post.url**

  *类型*: `string`

  认证 HTTP 服务器地址。


**authn:http_post.body**

  *类型*: `#{term => binary()}`

  HTTP request body，JSON 字符模板，支持使用占位符。
使用 GET 请求时，将被转换为 URL Query。
使用 POST 请求时，转换格式将取决于 content-type 头被设置为 application/json 还是 application/x-www-form-urlencoded。



**authn:http_post.request_timeout**

  *类型*: `duration_ms`

  *默认值*: `5s`

  HTTP 请求超时时长。


**authn:http_post.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:http_post.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  The timeout when connecting to the HTTP server.


**authn:http_post.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**authn:http_post.max_retries**

  *类型*: `non_neg_integer`

  Deprecated since 5.0.4.


**authn:http_post.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  The pool size.


**authn:http_post.request**

  *类型*: `connector-http:request`

  Configure HTTP request parameters.


**authn:http_post.retry_interval**

  *类型*: `timeout_duration`

  Deprecated since 5.0.4.


**authn:http_post.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



{% emqxee %}

### 使用 LDAP 进行密码认证


Configuration of authenticator using LDAP as authentication data source.

**authn:ldap.mechanism**

  *类型*: `password_based`

  认证机制。


**authn:ldap.backend**

  *类型*: `ldap`

  后端类型。


**authn:ldap.password_attribute**

  *类型*: `string`

  *默认值*: `userPassword`

  Indicates which attribute is used to represent the user's password.


**authn:ldap.is_superuser_attribute**

  *类型*: `string`

  *默认值*: `isSuperuser`

  Indicates which attribute is used to represent whether the user is a superuser.


**authn:ldap.query_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  Timeout for the LDAP query.


**authn:ldap.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。


**authn:ldap.server**

  *类型*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The LDAP default port 389 is used if `[:Port]` is not specified.


**authn:ldap.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn:ldap.username**

  *类型*: `string`

  内部数据库的用户名。


**authn:ldap.password**

  *类型*: `string`

  内部数据库密码。


**authn:ldap.base_dn**

  *类型*: `string`

  The name of the base object entry (or possibly the root) relative to
which the Search is to be performed.


**authn:ldap.filter**

  *类型*: `string`

  *默认值*: `(objectClass=mqttUser)`

  The filter that defines the conditions that must be fulfilled in order
for the Search to match a given entry.<br>
The syntax of the filter follows RFC 4515 and also supports placeholders.


**authn:ldap.request_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  Sets the maximum time in milliseconds that is used for each individual request.


**authn:ldap.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



{% endemqxee %}

### 附录：认证 Hash 配置


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

  *可选值*: `5-10`

  Work factor for BCRYPT password generation.




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



## 客户端认证 - JWT


用于认证的 JWT 使用 HMAC 算法签发时的配置。

**authn:jwt_hmac.algorithm**

  *类型*: `enum`

  *可选值*: `hmac-based`

  JWT 签名算法，支持 HMAC (配置为 <code>hmac-based</code>）和 RSA、ECDSA (配置为 <code>public-key</code>)。


**authn:jwt_hmac.secret**

  *类型*: `string`

  使用 HMAC 算法时用于验证 JWT 的密钥


**authn:jwt_hmac.secret_base64_encoded**

  *类型*: `boolean`

  *默认值*: `false`

  密钥是否为 Base64 编码。


**authn:jwt_hmac.mechanism**

  *类型*: `jwt`

  认证机制。


**authn:jwt_hmac.acl_claim_name**

  *类型*: `string`

  *默认值*: `acl`

  JWT claim name to use for getting ACL rules.


**authn:jwt_hmac.verify_claims**

  *类型*: `[term]`

  *默认值*: `[]`

  需要验证的自定义声明列表，它是一个名称/值对列表。
值可以使用以下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符
认证时将验证 JWT（取自 Password 字段）中 claims 的值是否与 <code>verify_claims</code> 中要求的相匹配。


**authn:jwt_hmac.from**

  *类型*: `enum`

  *默认值*: `password`

  *可选值*: `username | password`

  指定客户端连接请求中 JWT 的位置；可选值： password、 username（分别对应于 MQTT 客户端 CONNECT 报文中的 Password 和 Username 字段）


**authn:jwt_hmac.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。




用于认证的 JWTs 需要从 JWKS 端点获取时的配置。

**authn:jwt_jwks.use_jwks**

  *类型*: `enum`

  *可选值*: `true`

  是否使用 JWKS。


**authn:jwt_jwks.endpoint**

  *类型*: `string`

  JWKS 端点， 它是一个以 JWKS 格式返回服务端的公钥集的只读端点。


**authn:jwt_jwks.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**authn:jwt_jwks.refresh_interval**

  *类型*: `integer`

  *默认值*: `300`

  JWKS 刷新间隔。


**authn:jwt_jwks.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  SSL 选项。


**authn:jwt_jwks.mechanism**

  *类型*: `jwt`

  认证机制。


**authn:jwt_jwks.acl_claim_name**

  *类型*: `string`

  *默认值*: `acl`

  JWT claim name to use for getting ACL rules.


**authn:jwt_jwks.verify_claims**

  *类型*: `[term]`

  *默认值*: `[]`

  需要验证的自定义声明列表，它是一个名称/值对列表。
值可以使用以下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符
认证时将验证 JWT（取自 Password 字段）中 claims 的值是否与 <code>verify_claims</code> 中要求的相匹配。


**authn:jwt_jwks.from**

  *类型*: `enum`

  *默认值*: `password`

  *可选值*: `username | password`

  指定客户端连接请求中 JWT 的位置；可选值： password、 username（分别对应于 MQTT 客户端 CONNECT 报文中的 Password 和 Username 字段）


**authn:jwt_jwks.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。




用于认证的 JWT 使用 RSA 或 ECDSA 算法签发时的配置。

**authn:jwt_public_key.algorithm**

  *类型*: `enum`

  *可选值*: `public-key`

  JWT 签名算法，支持 HMAC (配置为 <code>hmac-based</code>）和 RSA、ECDSA (配置为 <code>public-key</code>)。


**authn:jwt_public_key.public_key**

  *类型*: `string`

  用于验证 JWT 的公钥。


**authn:jwt_public_key.mechanism**

  *类型*: `jwt`

  认证机制。


**authn:jwt_public_key.acl_claim_name**

  *类型*: `string`

  *默认值*: `acl`

  JWT claim name to use for getting ACL rules.


**authn:jwt_public_key.verify_claims**

  *类型*: `[term]`

  *默认值*: `[]`

  需要验证的自定义声明列表，它是一个名称/值对列表。
值可以使用以下占位符：
- <code>${username}</code>: 将在运行时被替换为客户端连接时使用的用户名
- <code>${clientid}</code>: 将在运行时被替换为客户端连接时使用的客户端标识符
认证时将验证 JWT（取自 Password 字段）中 claims 的值是否与 <code>verify_claims</code> 中要求的相匹配。


**authn:jwt_public_key.from**

  *类型*: `enum`

  *默认值*: `password`

  *可选值*: `username | password`

  指定客户端连接请求中 JWT 的位置；可选值： password、 username（分别对应于 MQTT 客户端 CONNECT 报文中的 Password 和 Username 字段）


**authn:jwt_public_key.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。



## 客户端认证 - MQTT 增强认证


Settings for Salted Challenge Response Authentication Mechanism
(SCRAM) authentication.

**authn:scram.mechanism**

  *类型*: `scram`

  认证机制。


**authn:scram.backend**

  *类型*: `built_in_database`

  后端类型。


**authn:scram.algorithm**

  *类型*: `enum`

  *默认值*: `sha256`

  *可选值*: `sha256 | sha512`

  Hashing algorithm.


**authn:scram.iteration_count**

  *类型*: `non_neg_integer`

  *默认值*: `4096`

  Iteration count.


**authn:scram.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此认证数据源。



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



### 基于 ACL 文件进行授权


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



### 基于内置数据库进行授权


使用内部数据库授权（mnesia）。

**authorization.sources.$INDEX.type**

  *类型*: `built_in_database`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源



### 基于 MySQL 进行授权


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

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MySQL default port 3306 is used if `[:Port]` is not specified.


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


### 基于 PostgreSQL 进行授权


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



### 基于 MongoDB 进行授权

### MongoDB 单节点


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


**authorization.sources.$INDEX.use_legacy_protocol**

  *类型*: `enum`

  *默认值*: `auto`

  *可选值*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


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



### MongoDB Replica Set 集群


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


**authorization.sources.$INDEX.use_legacy_protocol**

  *类型*: `enum`

  *默认值*: `auto`

  *可选值*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


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



#### MongoDB Sharded 集群


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


**authorization.sources.$INDEX.use_legacy_protocol**

  *类型*: `enum`

  *默认值*: `auto`

  *可选值*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


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



### 基于 Redis 进行授权

### Redis 单节点


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


**authorization.sources.$INDEX.cmd**

  *类型*: `string`

  访问控制数据查查询命令



### Redis 集群


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


**authorization.sources.$INDEX.username**

  *类型*: `string`

  内部数据库的用户名。


**authorization.sources.$INDEX.password**

  *类型*: `string`

  内部数据库密码。


**authorization.sources.$INDEX.database**

  *类型*: `non_neg_integer`

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



### Redis Sentinel 集群


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


**authorization.sources.$INDEX.username**

  *类型*: `string`

  内部数据库的用户名。


**authorization.sources.$INDEX.password**

  *类型*: `string`

  内部数据库密码。


**authorization.sources.$INDEX.database**

  *类型*: `non_neg_integer`

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



{% emqxee %}

### 基于 LDAP 进行授权


AuthZ with LDAP

**authorization.sources.$INDEX.type**

  *类型*: `ldap`

  数据后端类型


**authorization.sources.$INDEX.enable**

  *类型*: `boolean`

  *默认值*: `true`

  设为 <code>true</code> 或 <code>false</code> 以启用或禁用此访问控制数据源


**authorization.sources.$INDEX.publish_attribute**

  *类型*: `string`

  *默认值*: `mqttPublishTopic`

  Indicates which attribute is used to represent the allowed topics list of the `publish`.


**authorization.sources.$INDEX.subscribe_attribute**

  *类型*: `string`

  *默认值*: `mqttSubscriptionTopic`

  Indicates which attribute is used to represent the allowed topics list of the `subscribe`.


**authorization.sources.$INDEX.all_attribute**

  *类型*: `string`

  *默认值*: `mqttPubSubTopic`

  Indicates which attribute is used to represent the both allowed topics list of  `publish` and `subscribe`.


**authorization.sources.$INDEX.query_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  Timeout for the LDAP query.


**authorization.sources.$INDEX.server**

  *类型*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The LDAP default port 389 is used if `[:Port]` is not specified.


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


**authorization.sources.$INDEX.base_dn**

  *类型*: `string`

  The name of the base object entry (or possibly the root) relative to
which the Search is to be performed.


**authorization.sources.$INDEX.filter**

  *类型*: `string`

  *默认值*: `(objectClass=mqttUser)`

  The filter that defines the conditions that must be fulfilled in order
for the Search to match a given entry.<br>
The syntax of the filter follows RFC 4515 and also supports placeholders.


**authorization.sources.$INDEX.request_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  Sets the maximum time in milliseconds that is used for each individual request.


**authorization.sources.$INDEX.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



{% endemqxee %}

### 基于 HTTP 应用进行授权

#### HTTP GET 方式


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

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  The timeout when connecting to the HTTP server.


**authorization.sources.$INDEX.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**authorization.sources.$INDEX.max_retries**

  *类型*: `non_neg_integer`

  Deprecated since 5.0.4.


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  The pool size.


**authorization.sources.$INDEX.request**

  *类型*: `connector-http:request`

  Configure HTTP request parameters.


**authorization.sources.$INDEX.retry_interval**

  *类型*: `timeout_duration`

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

  *默认值*: `{"keep-alive":"timeout=30, max=1000","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  HTTP Headers 列表 (无 <code>content-type</code>) 。



#### HTTP POST 方式


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

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  The timeout when connecting to the HTTP server.


**authorization.sources.$INDEX.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**authorization.sources.$INDEX.max_retries**

  *类型*: `non_neg_integer`

  Deprecated since 5.0.4.


**authorization.sources.$INDEX.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  The pool size.


**authorization.sources.$INDEX.request**

  *类型*: `connector-http:request`

  Configure HTTP request parameters.


**authorization.sources.$INDEX.retry_interval**

  *类型*: `timeout_duration`

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

  *默认值*: `{"keep-alive":"timeout=30, max=1000","content-type":"application/json","connection":"keep-alive","cache-control":"no-cache","accept":"application/json"}`

  HTTP Headers 列表



## 编解码


Schema registry configurations.

**schema_registry.schemas**

  *类型*: `name`

  *默认值*: `{}`

  Registered schemas.



### Protobuf


[Protocol Buffers](https://protobuf.dev/) serialization format.

**schema_registry.schemas.$name.type**

  *类型*: `protobuf`

  Schema type.


**schema_registry.schemas.$name.source**

  *类型*: `string`

  Source text for the schema.


**schema_registry.schemas.$name.description**

  *类型*: `string`

  *默认值*: `""`

  A description for this schema.



### Avro


[Apache Avro](https://avro.apache.org/) serialization format.

**schema_registry.schemas.$name.type**

  *类型*: `avro`

  Schema type.


**schema_registry.schemas.$name.source**

  *类型*: `json_binary`

  Source text for the schema.


**schema_registry.schemas.$name.description**

  *类型*: `string`

  *默认值*: `""`

  A description for this schema.



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

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


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

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


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



<!-- ### 连接配置

  @connector-http:request@

  @connector-mqtt:egress@

  @connector-mqtt:egress_local@

  @connector-mqtt:egress_remote@

  @connector-mqtt:ingress@

  @connector-mqtt:ingress_local@

  @connector-mqtt:ingress_remote@ -->

{% emqxee %}

### Kafka


使用 GSSAPI/Kerberos 认证。

**bridge_kafka:auth_gssapi_kerberos.kerberos_principal**

  *类型*: `string`

  SASL GSSAPI 认证方法的 Kerberos principal，例如 <code>client_name@MY.KERBEROS.REALM.MYDOMAIN.COM</code>注意：这里使用的 realm 需要配置在 EMQX 服务器的 /etc/krb5.conf 中


**bridge_kafka:auth_gssapi_kerberos.kerberos_keytab_file**

  *类型*: `string`

  SASL GSSAPI 认证方法的 Kerberos keytab 文件。注意：该文件需要上传到 EMQX 服务器中，且运行 EMQX 服务的系统账户需要有读取权限。




基于用户名密码的认证。

**bridge_kafka:auth_username_password.mechanism**

  *类型*: `enum`

  *可选值*: `plain | scram_sha_256 | scram_sha_512`

  SASL 认证方法名称。


**bridge_kafka:auth_username_password.username**

  *类型*: `string`

  SASL 认证的用户名。


**bridge_kafka:auth_username_password.password**

  *类型*: `string`

  SASL 认证的密码。




Kafka消费者配置。

**bridges.kafka_consumer.$name.kafka.max_batch_bytes**

  *类型*: `bytesize`

  *默认值*: `896KB`

  设置每次从 Kafka 拉取数据的字节数。如该配置小于 Kafka 消息的大小，可能会影响消费性能。


**bridges.kafka_consumer.$name.kafka.offset_reset_policy**

  *类型*: `enum`

  *默认值*: `latest`

  *可选值*: `latest | earliest`

  如不存在偏移量历史记录或历史记录失效，消费者应使用哪个偏移量开始消费。


**bridges.kafka_consumer.$name.kafka.offset_commit_interval_seconds**

  *类型*: `timeout_duration_s`

  *默认值*: `5s`

  指定 Kafka 消费组偏移量提交的时间间隔。




指定 Kafka 主题和 MQTT 主题之间的映射关系。 应至少包含一项。

**bridges.kafka_consumer.$name.topic_mapping.$INDEX.kafka_topic**

  *类型*: `string`

  指定从哪个 Kafka 主题消费消息。


**bridges.kafka_consumer.$name.topic_mapping.$INDEX.mqtt_topic**

  *类型*: `string`

  设置 Kafka 消息向哪个本地 MQTT 主题转发消息。


**bridges.kafka_consumer.$name.topic_mapping.$INDEX.qos**

  *类型*: `qos`

  *默认值*: `0`

  转发 MQTT 消息时使用的 QoS。


**bridges.kafka_consumer.$name.topic_mapping.$INDEX.payload_template**

  *类型*: `string`

  *默认值*: `${.}`

  用于转换收到的 Kafka 消息的模板。 默认情况下，它将使用 JSON 格式来序列化来自 Kafka 的所有字段。 这些字段包括：<code>headers</code>：一个包含字符串键值对的 JSON 对象。
<code>key</code>：Kafka 消息的键（使用选择的编码方式编码）。
<code>offset</code>：消息的偏移量。
<code>topic</code>：Kafka 主题。
<code>ts</code>: 消息的时间戳。
<code>ts_type</code>：消息的时间戳类型，值可能是： <code>create</code>， <code>append</code> 或 <code>undefined</code>。
<code>value</code>: Kafka 消息值（使用选择的编码方式编码）。




Kafka 消费者配置。

**bridges.kafka_consumer.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用（true）或停用该（false）Kafka 数据桥接。


**bridges.kafka_consumer.$name.bootstrap_hosts**

  *类型*: `string`

  用逗号分隔的 <code>host[:port]</code> 主机列表。默认端口号为 9092。


**bridges.kafka_consumer.$name.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  建立 TCP 连接时的最大等待时长（若启用认证，这个等待时长也包含完成认证所需时间）。


**bridges.kafka_consumer.$name.min_metadata_refresh_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `3s`

  刷新 Kafka broker 和 Kafka 主题元数据段最短时间间隔。设置太小可能会增加 Kafka 压力。


**bridges.kafka_consumer.$name.metadata_request_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  刷新元数据时最大等待时长。


**bridges.kafka_consumer.$name.authentication**

  *类型*: none | [bridge_kafka:auth_username_password](#bridge_kafka:auth_username_password) | [bridge_kafka:auth_gssapi_kerberos](#bridge_kafka:auth_gssapi_kerberos)

  *默认值*: `none`

  认证参数。


**bridges.kafka_consumer.$name.socket_opts**

  *类型*: `bridge_kafka:socket_opts`

  更多 Socket 参数设置。


**bridges.kafka_consumer.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.kafka_consumer.$name.kafka**

  *类型*: `bridge_kafka:consumer_kafka_opts`

  Kafka消费者配置。


**bridges.kafka_consumer.$name.topic_mapping**

  *类型*: `array`

  指定 Kafka 主题和 MQTT 主题之间的映射关系。 应至少包含一项。


**bridges.kafka_consumer.$name.key_encoding_mode**

  *类型*: `enum`

  *默认值*: `none`

  *可选值*: `none | base64`

  通过 MQTT 转发之前，如何处理 Kafka 消息的 Key。<code>none</code> 使用 Kafka 消息中的 Key 原始值，不进行编码。  注意：在这种情况下，Key 必须是一个有效的 UTF-8 字符串。
<code>base64</code> 对收到的密钥或值使用 base-64 编码。


**bridges.kafka_consumer.$name.value_encoding_mode**

  *类型*: `enum`

  *默认值*: `none`

  *可选值*: `none | base64`

  通过 MQTT 转发之前，如何处理 Kafka 消息的 Value。<code>none</code> 使用 Kafka 消息中的 Value 原始值，不进行编码。  注意：在这种情况下，Value 必须是一个有效的 UTF-8 字符串。
<code>base64</code> 对收到的 Value 使用 base-64 编码。


**bridges.kafka_consumer.$name.resource_opts**

  *类型*: `bridge_kafka:resource_opts`

  *默认值*: `{}`




用于生成 Kafka 消息的模版。

**bridges.kafka.$name.kafka.message.key**

  *类型*: `string`

  *默认值*: `${.clientid}`

  生成 Kafka 消息 Key 的模版。如果模版生成后为空值，则会使用 Kafka 的 <code>NULL</code> ，而非空字符串。


**bridges.kafka.$name.kafka.message.value**

  *类型*: `string`

  *默认值*: `${.}`

  生成 Kafka 消息 Value 的模版。如果模版生成后为空值，则会使用 Kafka 的 <code>NULL</code>，而非空字符串。


**bridges.kafka.$name.kafka.message.timestamp**

  *类型*: `string`

  *默认值*: `${.timestamp}`

  生成 Kafka 消息时间戳的模版。该时间必需是一个整型数值（可以是字符串格式）例如 <code>1661326462115</code> 或 <code>'1661326462115'</code>。当所需的输入字段不存在，或不是一个整型时，则会使用当前系统时间。




Kafka Producer 配置。

**bridges.kafka.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用（true）或停用该（false）Kafka 数据桥接。


**bridges.kafka.$name.bootstrap_hosts**

  *类型*: `string`

  用逗号分隔的 <code>host[:port]</code> 主机列表。默认端口号为 9092。


**bridges.kafka.$name.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  建立 TCP 连接时的最大等待时长（若启用认证，这个等待时长也包含完成认证所需时间）。


**bridges.kafka.$name.min_metadata_refresh_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `3s`

  刷新 Kafka broker 和 Kafka 主题元数据段最短时间间隔。设置太小可能会增加 Kafka 压力。


**bridges.kafka.$name.metadata_request_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  刷新元数据时最大等待时长。


**bridges.kafka.$name.authentication**

  *类型*: none | [bridge_kafka:auth_username_password](#bridge_kafka:auth_username_password) | [bridge_kafka:auth_gssapi_kerberos](#bridge_kafka:auth_gssapi_kerberos)

  *默认值*: `none`

  认证参数。


**bridges.kafka.$name.socket_opts**

  *类型*: `bridge_kafka:socket_opts`

  更多 Socket 参数设置。


**bridges.kafka.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.kafka.$name.local_topic**

  *类型*: `string`

  MQTT 主题数据源由桥接指定，或留空由规则动作指定。


**bridges.kafka.$name.kafka**

  *类型*: `bridge_kafka:producer_kafka_opts`

  Kafka 生产者参数。


**bridges.kafka.$name.resource_opts**

  *类型*: `bridge_kafka:resource_opts`

  *默认值*: `{}`




配置消息缓存的相关参数。

当 EMQX 需要发送的消息超过 Kafka 处理能力，或者当 Kafka 临时下线时，EMQX 内部会将消息缓存起来。

**bridge_kafka:producer_buffer.mode**

  *类型*: `enum`

  *默认值*: `memory`

  *可选值*: `memory | disk | hybrid`

  消息缓存模式。
<code>memory</code>: 所有的消息都缓存在内存里。如果 EMQX 服务重启，缓存的消息会丢失。
<code>disk</code>: 缓存到磁盘上。EMQX 重启后会继续发送重启前未发送完成的消息。
<code>hybrid</code>: 先将消息缓存在内存中，当内存中的消息堆积超过一定限制（配置项 <code>segment_bytes</code> 描述了该限制）后，后续的消息会缓存到磁盘上。与 <code>memory</code> 模式一样，如果 EMQX 服务重启，缓存的消息会丢失。


**bridge_kafka:producer_buffer.per_partition_limit**

  *类型*: `bytesize`

  *默认值*: `2GB`

  为每个 Kafka 分区设置的最大缓存字节数。当超过这个上限之后，老的消息会被丢弃，为新的消息腾出空间。


**bridge_kafka:producer_buffer.segment_bytes**

  *类型*: `bytesize`

  *默认值*: `100MB`

  当缓存模式是 <code>disk</code> 或 <code>hybrid</code> 时适用。该配置用于指定缓存到磁盘上的文件的大小。


**bridge_kafka:producer_buffer.memory_overload_protection**

  *类型*: `boolean`

  *默认值*: `false`

  缓存模式是 <code>memory</code> 或 <code>hybrid</code> 时适用。当系统处于高内存压力时，从队列中丢弃旧的消息以减缓内存增长。内存压力值由配置项 <code>sysmon.os.sysmem_high_watermark</code> 决定。注意，该配置仅在 Linux 系统中有效。




Please provide more key-value pairs for Kafka headers<br/>
The key-value pairs here will be combined with the
value of <code>kafka_headers</code> field before sending to Kafka.

**bridge_kafka:producer_kafka_ext_headers.kafka_ext_header_key**

  *类型*: `string`

  Key of the Kafka header. Placeholders in format of ${var} are supported.


**bridge_kafka:producer_kafka_ext_headers.kafka_ext_header_value**

  *类型*: `string`

  Value of the Kafka header. Placeholders in format of ${var} are supported.




Kafka 生产者参数。

**bridges.kafka.$name.kafka.topic**

  *类型*: `string`

  Kafka 主题名称


**bridges.kafka.$name.kafka.message**

  *类型*: `bridge_kafka:kafka_message`

  用于生成 Kafka 消息的模版。


**bridges.kafka.$name.kafka.max_batch_bytes**

  *类型*: `bytesize`

  *默认值*: `896KB`

  最大消息批量字节数。大多数 Kafka 环境的默认最低值是 1 MB，EMQX 的默认值比 1 MB 更小是因为需要补偿 Kafka 消息编码所需要的额外字节（尤其是当每条消息都很小的情况下）。当单个消息的大小超过该限制时，它仍然会被发送，（相当于该批量中只有单个消息）。


**bridges.kafka.$name.kafka.compression**

  *类型*: `enum`

  *默认值*: `no_compression`

  *可选值*: `no_compression | snappy | gzip`

  压缩方法。


**bridges.kafka.$name.kafka.partition_strategy**

  *类型*: `enum`

  *默认值*: `random`

  *可选值*: `random | key_dispatch`

  设置消息发布时应该如何选择 Kafka 分区。

<code>random</code>: 为每个消息随机选择一个分区。
<code>key_dispatch</code>: Hash Kafka message key to a partition number


**bridges.kafka.$name.kafka.required_acks**

  *类型*: `enum`

  *默认值*: `all_isr`

  *可选值*: `all_isr | leader_only | none`

  设置 Kafka leader 在返回给 EMQX 确认之前需要等待多少个 follower 的确认。

<code>all_isr</code>: 需要所有的在线复制者都确认。
<code>leader_only</code>: 仅需要分区 leader 确认。
<code>none</code>: 无需 Kafka 回复任何确认。


**bridges.kafka.$name.kafka.kafka_headers**

  *类型*: `string`

  Please provide a placeholder to be used as Kafka Headers<br/>
e.g. <code>${pub_props}</code><br/>
Notice that the value of the placeholder must either be an object:
<code>{"foo": "bar"}</code>
or an array of key-value pairs:
<code>[{"key": "foo", "value": "bar"}]</code>


**bridges.kafka.$name.kafka.kafka_ext_headers**

  *类型*: `array`

  Please provide more key-value pairs for Kafka headers<br/>
The key-value pairs here will be combined with the
value of <code>kafka_headers</code> field before sending to Kafka.


**bridges.kafka.$name.kafka.kafka_header_value_encode_mode**

  *类型*: `enum`

  *默认值*: `none`

  *可选值*: `none | json`

  Kafka headers value encode mode<br/>
 - NONE: only add binary values to Kafka headers;<br/>
 - JSON: only add JSON values to Kafka headers,
and encode it to JSON strings before sending.


**bridges.kafka.$name.kafka.partition_count_refresh_interval**

  *类型*: `timeout_duration_s`

  *默认值*: `60s`

  配置 Kafka 刷新分区数量的时间间隔。
EMQX 发现 Kafka 分区数量增加后，会开始按 <code>partition_strategy<code> 配置，把消息发送到新的分区中。


**bridges.kafka.$name.kafka.max_inflight**

  *类型*: `pos_integer`

  *默认值*: `10`

  设置 Kafka 生产者（每个分区一个）在收到 Kafka 的确认前最多发送多少个请求（批量）。调大这个值通常可以增加吞吐量，但是，当该值设置大于 1 时存在消息乱序的风险。


**bridges.kafka.$name.kafka.buffer**

  *类型*: `bridge_kafka:producer_buffer`

  配置消息缓存的相关参数。

当 EMQX 需要发送的消息超过 Kafka 处理能力，或者当 Kafka 临时下线时，EMQX 内部会将消息缓存起来。


**bridges.kafka.$name.kafka.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `async | sync`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.kafka.$name.kafka.sync_query_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  This parameter defines the timeout limit for synchronous queries. It applies only when the bridge query mode is configured to 'sync'.




资源相关的选项。

**bridge_kafka:resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。




更多 Socket 参数设置。

**bridge_kafka:socket_opts.sndbuf**

  *类型*: `bytesize`

  *默认值*: `1MB`

  TCP socket 的发送缓存调优。默认值是针对高吞吐量的一个推荐值。


**bridge_kafka:socket_opts.recbuf**

  *类型*: `bytesize`

  *默认值*: `1MB`

  TCP socket 的收包缓存调优。默认值是针对高吞吐量的一个推荐值。


**bridge_kafka:socket_opts.tcp_keepalive**

  *类型*: `string`

  *默认值*: `none`

  Enable TCP keepalive for Kafka bridge connections.
The value is three comma separated numbers in the format of 'Idle,Interval,Probes'
 - Idle: The number of seconds a connection needs to be idle before the server begins to send out keep-alive probes (Linux default 7200).
 - Interval: The number of seconds between TCP keep-alive probes (Linux default 75).
 - Probes: The maximum number of TCP keep-alive probes to send before giving up and killing the connection if no response is obtained from the other end (Linux default 9).
For example "240,30,5" means: TCP keepalive probes are sent after the connection is idle for 240 seconds, and the probes are sent every 30 seconds until a response is received, if it misses 5 consecutive responses, the connection should be closed.
Default: 'none'



### Pulsar


基本认证的参数。

**bridges.pulsar_producer.$name.authentication.username**

  *类型*: `string`

  基本认证用户名。


**bridges.pulsar_producer.$name.authentication.password**

  *类型*: `string`

  基本认证密码。




令牌认证的参数。

**bridges.pulsar_producer.$name.authentication.jwt**

  *类型*: `string`

  JWT认证令牌。




配置消息缓存的相关参数。

当 EMQX 需要发送的消息超过 Pulsar 处理能力，或者当 Pulsar 临时下线时，EMQX 内部会将消息缓存起来。

**bridges.pulsar_producer.$name.buffer.mode**

  *类型*: `enum`

  *默认值*: `memory`

  *可选值*: `memory | disk | hybrid`

  消息缓存模式。
<code>memory</code>: 所有的消息都缓存在内存里。如果 EMQX 服务重启，缓存的消息会丢失。
<code>disk</code>: 缓存到磁盘上。EMQX 重启后会继续发送重启前未发送完成的消息。
<code>hybrid</code>: 先将消息缓存在内存中，当内存中的消息堆积超过一定限制（配置项 <code>segment_bytes</code> 描述了该限制）后，后续的消息会缓存到磁盘上。与 <code>memory</code> 模式一样，如果 EMQX 服务重启，缓存的消息会丢失。


**bridges.pulsar_producer.$name.buffer.per_partition_limit**

  *类型*: `bytesize`

  *默认值*: `2GB`

  为每个 Pulsar 分区设置的最大缓存字节数。当超过这个上限之后，老的消息会被丢弃，为新的消息腾出空间。


**bridges.pulsar_producer.$name.buffer.segment_bytes**

  *类型*: `bytesize`

  *默认值*: `100MB`

  当缓存模式是 <code>disk</code> 或 <code>hybrid</code> 时适用。该配置用于指定缓存到磁盘上的文件的大小。


**bridges.pulsar_producer.$name.buffer.memory_overload_protection**

  *类型*: `boolean`

  *默认值*: `false`

  缓存模式是 <code>memory</code> 或 <code>hybrid</code> 时适用。当系统处于高内存压力时，从队列中丢弃旧的消息以减缓内存增长。内存压力值由配置项 <code>sysmon.os.sysmem_high_watermark</code> 决定。注意，该配置仅在 Linux 系统中有效。




用于生成 Pulsar 消息的模版。

**bridges.pulsar_producer.$name.message.key**

  *类型*: `string`

  *默认值*: `${.clientid}`

  生成 Pulsar 消息 Key 的模版。


**bridges.pulsar_producer.$name.message.value**

  *类型*: `string`

  *默认值*: `${.}`

  生成 Pulsar 消息 Value 的模版。




资源启动相关的选项。

**bridges.pulsar_producer.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `1s`

  健康检查间隔。


**bridges.pulsar_producer.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.pulsar_producer.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。




Pulsar 桥接配置

**bridges.pulsar_producer.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用（true）或停用（false）该 Pulsar 数据桥接。


**bridges.pulsar_producer.$name.servers**

  *类型*: `string`

  以逗号分隔的 <code>scheme://host[:port]</code> 格式的 Pulsar URL 列表，支持的 scheme 有 <code>pulsar://</code> （默认）和<code>pulsar+ssl://</code>。默认的端口是6650。


**bridges.pulsar_producer.$name.authentication**

  *类型*: none | [bridge_pulsar:auth_basic](#bridge_pulsar:auth_basic) | [bridge_pulsar:auth_token](#bridge_pulsar:auth_token)

  *默认值*: `none`

  认证参数。


**bridges.pulsar_producer.$name.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  建立 TCP 连接时的最大等待时长（若启用认证，这个等待时长也包含完成认证所需时间）。


**bridges.pulsar_producer.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.pulsar_producer.$name.batch_size**

  *类型*: `pos_integer`

  *默认值*: `100`

  在一个Pulsar消息中批处理的单个请求的最大数量。


**bridges.pulsar_producer.$name.compression**

  *类型*: `enum`

  *默认值*: `no_compression`

  *可选值*: `no_compression | snappy | zlib`

  压缩方法。


**bridges.pulsar_producer.$name.send_buffer**

  *类型*: `bytesize`

  *默认值*: `1MB`

  TCP socket 的发送缓存调优。默认值是针对高吞吐量的一个推荐值。


**bridges.pulsar_producer.$name.sync_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `3s`

  同步发布时，从Pulsar接收发送回执的最长等待时间。


**bridges.pulsar_producer.$name.retention_period**

  *类型*: `infinity | duration_ms`

  *默认值*: `infinity`

  当没有连接到Pulsar代理时，信息将被缓冲的时间。 较长的时间意味着将使用更多的内存/磁盘


**bridges.pulsar_producer.$name.max_batch_bytes**

  *类型*: `bytesize`

  *默认值*: `900KB`

  最大消息批量字节数。大多数 Pulsar 环境的默认最低值是 5 MB，EMQX 的默认值比 5 MB 更小是因为需要补偿 Pulsar 消息编码所需要的额外字节（尤其是当每条消息都很小的情况下）。当单个消息的大小超过该限制时，它仍然会被发送，（相当于该批量中只有单个消息）。


**bridges.pulsar_producer.$name.local_topic**

  *类型*: `string`

  MQTT 主题数据源由桥接指定，或留空由规则动作指定。


**bridges.pulsar_producer.$name.pulsar_topic**

  *类型*: `string`

  Pulsar 主题名称


**bridges.pulsar_producer.$name.strategy**

  *类型*: `enum`

  *默认值*: `random`

  *可选值*: `random | roundrobin | key_dispatch`

  设置消息发布时应该如何选择 Pulsar 分区。

<code>random</code>: 为每个消息随机选择一个分区。
<code>roundrobin</code>: 依次为每条信息挑选可用的生产商。
<code>key_dispatch</code>: 将一批信息中的第一条信息的Pulsar信息密钥哈希到一个分区编号。


**bridges.pulsar_producer.$name.buffer**

  *类型*: `bridge_pulsar:producer_buffer`

  配置消息缓存的相关参数。

当 EMQX 需要发送的消息超过 Pulsar 处理能力，或者当 Pulsar 临时下线时，EMQX 内部会将消息缓存起来。


**bridges.pulsar_producer.$name.message**

  *类型*: `bridge_pulsar:producer_pulsar_message`

  用于生成 Pulsar 消息的模版。


**bridges.pulsar_producer.$name.resource_opts**

  *类型*: `bridge_pulsar:producer_resource_opts`

  资源启动相关的选项。



### RocketMQ


RocketMQ 桥接配置

**bridges.rocketmq.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用桥接


**bridges.rocketmq.$name.template**

  *类型*: `string`

  *默认值*: `""`

  模板, 默认为空，为空时将会将整个消息转发给 RocketMQ。 <br>
            模板可以是任意带有占位符的合法字符串, 例如:<br>
            - ${id}, ${username}, ${clientid}, ${timestamp}<br>
            - {"id" : ${id}, "username" : ${username}}


**bridges.rocketmq.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 RocketMQ。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。


**bridges.rocketmq.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.rocketmq.$name.servers**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 RocketMQ 默认端口 9876。


**bridges.rocketmq.$name.topic**

  *类型*: `string`

  *默认值*: `TopicTest`

  RocketMQ 主题


**bridges.rocketmq.$name.access_key**

  *类型*: `string`

  *默认值*: `""`

  RocketMQ 服务器的 `accessKey`。


**bridges.rocketmq.$name.secret_key**

  *类型*: `string`

  *默认值*: `""`

  RocketMQ 服务器的 `secretKey`。


**bridges.rocketmq.$name.security_token**

  *类型*: `string`

  *默认值*: `""`

  RocketMQ 服务器安全令牌


**bridges.rocketmq.$name.sync_timeout**

  *类型*: `timeout_duration`

  *默认值*: `3s`

  RocketMQ 驱动同步调用的超时时间。


**bridges.rocketmq.$name.refresh_interval**

  *类型*: `timeout_duration`

  *默认值*: `3s`

  RocketMQ 主题路由更新间隔。


**bridges.rocketmq.$name.send_buffer**

  *类型*: `bytesize`

  *默认值*: `1024KB`

  RocketMQ 驱动的套字节发送消息的缓冲区大小


**bridges.rocketmq.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.rocketmq.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.



### RabbitMQ


RabbitMQ 桥接配置

**bridges.rabbitmq.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用或禁用桥接


**bridges.rabbitmq.$name.local_topic**

  *类型*: `string`

  转发到 RabbitMQ 的消息 MQTT 主题或主题过滤器。对所有的 'PUBLISH' 消息，如果匹配到该配置的主题或主题过滤器时，
注意：如果此桥接被用作规则的动作，同时又配置了 'local_topic'，那么这两份消息都会被转发到 RabbitMQ，这可能会导致消息重复。


**bridges.rabbitmq.$name.resource_opts**

  *类型*: `bridge_rabbitmq:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.rabbitmq.$name.server**

  *类型*: `string`

  *默认值*: `localhost`

  RabbitMQ 服务器的主机名或 IP 地址（例如 localhost）。


**bridges.rabbitmq.$name.port**

  *类型*: `port_number`

  *默认值*: `5672`

  RabbitMQ 服务器的主机名或 IP 地址（例如 localhost）。


**bridges.rabbitmq.$name.username**

  *类型*: `string`

  用于认证的 RabbitMQ 用户名。


**bridges.rabbitmq.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.rabbitmq.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  连接池的大小。


**bridges.rabbitmq.$name.timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  连接超时时间。


**bridges.rabbitmq.$name.wait_for_publish_confirmations**

  *类型*: `boolean`

  *默认值*: `true`

  是否等待 RabbitMQ 确认消息发布。


**bridges.rabbitmq.$name.publish_confirmation_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `30s`

  连接超时时间。


**bridges.rabbitmq.$name.virtual_host**

  *类型*: `string`

  *默认值*: `/`

  RabbitMQ 虚拟主机的名称。


**bridges.rabbitmq.$name.heartbeat**

  *类型*: `timeout_duration_ms`

  *默认值*: `30s`

  发送心跳的间隔时间。


**bridges.rabbitmq.$name.exchange**

  *类型*: `string`

   RabbitMQ Exchange 名称。


**bridges.rabbitmq.$name.routing_key**

  *类型*: `string`

  用于路由消息的 Routing Key。


**bridges.rabbitmq.$name.delivery_mode**

  *类型*: `enum`

  *默认值*: `non_persistent`

  *可选值*: `non_persistent | persistent`

  消息的投递模式。non_persistent(1) 表示非持久化消息，persistent(2) 表示持久化消息。


**bridges.rabbitmq.$name.payload_template**

  *类型*: `string`

  *默认值*: `${.}`

  用于生成 RabbitMQ 消息的模版。模板中的占位符（例如 '${fields.sub_field}'）会被替换成消息的字段值。如果该留空，则表示所有的输入字段都会被转发到 RabbitMQ，与使用 '${.}' 作为占位符的效果等价。


**bridges.rabbitmq.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。




资源启动相关的选项。

**bridges.rabbitmq.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.rabbitmq.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.rabbitmq.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.rabbitmq.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.rabbitmq.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.rabbitmq.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.rabbitmq.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.rabbitmq.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.rabbitmq.$name.resource_opts.batch_size**

  *类型*: `pos_integer`

  *默认值*: `1`

  最大批量请求大小。如果设为1，则无批处理。


**bridges.rabbitmq.$name.resource_opts.batch_time**

  *类型*: `timeout_duration_ms`

  *默认值*: `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。


**bridges.rabbitmq.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.rabbitmq.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



### Azure Event Hubs


Username/password based authentication.

**bridges.azure_event_hub_producer.$name.authentication.password**

  *类型*: `string`

  The Connection String for connecting to Azure Event Hub.  Should be the "connection string-primary key" of a Namespace shared access policy.




Template to render an Azure Event Hub message.

**bridges.azure_event_hub_producer.$name.kafka.message.key**

  *类型*: `string`

  *默认值*: `${.clientid}`

  Template to render Azure Event Hub message key. If the template is rendered into a NULL value (i.e. there is no such data field in Rule Engine context) then Azure Event Hub's <code>NULL</code> (but not empty string) is used.


**bridges.azure_event_hub_producer.$name.kafka.message.value**

  *类型*: `string`

  *默认值*: `${.}`

  Template to render Azure Event Hub message value. If the template is rendered into a NULL value (i.e. there is no such data field in Rule Engine context) then Azure Event Hub's <code>NULL</code> (but not empty string) is used.




Azure Event Hub producer configs.

**bridges.azure_event_hub_producer.$name.kafka.topic**

  *类型*: `string`

  Event Hub name


**bridges.azure_event_hub_producer.$name.kafka.message**

  *类型*: `bridge_azure_event_hub:kafka_message`

  Template to render an Azure Event Hub message.


**bridges.azure_event_hub_producer.$name.kafka.max_batch_bytes**

  *类型*: `bytesize`

  *默认值*: `896KB`

  Maximum bytes to collect in an Azure Event Hub message batch. Most of the Kafka brokers default to a limit of 1 MB batch size. EMQX's default value is less than 1 MB in order to compensate Kafka message encoding overheads (especially when each individual message is very small). When a single message is over the limit, it is still sent (as a single element batch).


**bridges.azure_event_hub_producer.$name.kafka.partition_strategy**

  *类型*: `enum`

  *默认值*: `random`

  *可选值*: `random | key_dispatch`

  Partition strategy is to tell the producer how to dispatch messages to Azure Event Hub partitions.

<code>random</code>: Randomly pick a partition for each message
<code>key_dispatch</code>: Hash Azure Event Hub message key to a partition number


**bridges.azure_event_hub_producer.$name.kafka.required_acks**

  *类型*: `enum`

  *默认值*: `all_isr`

  *可选值*: `all_isr | leader_only`

  Required acknowledgements for Azure Event Hub partition leader to wait for its followers before it sends back the acknowledgement to EMQX Azure Event Hub producer

<code>all_isr</code>: Require all in-sync replicas to acknowledge.
<code>leader_only</code>: Require only the partition-leader's acknowledgement.


**bridges.azure_event_hub_producer.$name.kafka.kafka_headers**

  *类型*: `string`

  Please provide a placeholder to be used as Azure Event Hub Headers<br/>
e.g. <code>${pub_props}</code><br/>
Notice that the value of the placeholder must either be an object:
<code>{"foo": "bar"}</code>
or an array of key-value pairs:
<code>[{"key": "foo", "value": "bar"}]</code>


**bridges.azure_event_hub_producer.$name.kafka.kafka_ext_headers**

  *类型*: `array`

  Please provide more key-value pairs for Azure Event Hub headers<br/>
The key-value pairs here will be combined with the
value of <code>kafka_headers</code> field before sending to Azure Event Hub.


**bridges.azure_event_hub_producer.$name.kafka.kafka_header_value_encode_mode**

  *类型*: `enum`

  *默认值*: `none`

  *可选值*: `none | json`

  Azure Event Hub headers value encode mode<br/>
 - NONE: only add binary values to Azure Event Hub headers;<br/>
 - JSON: only add JSON values to Azure Event Hub headers,
and encode it to JSON strings before sending.


**bridges.azure_event_hub_producer.$name.kafka.partition_count_refresh_interval**

  *类型*: `timeout_duration_s`

  *默认值*: `60s`

  The time interval for Azure Event Hub producer to discover increased number of partitions.
After the number of partitions is increased in Azure Event Hub, EMQX will start taking the
discovered partitions into account when dispatching messages per <code>partition_strategy</code>.


**bridges.azure_event_hub_producer.$name.kafka.max_inflight**

  *类型*: `pos_integer`

  *默认值*: `10`

  Maximum number of batches allowed for Azure Event Hub producer (per-partition) to send before receiving acknowledgement from Azure Event Hub. Greater value typically means better throughput. However, there can be a risk of message reordering when this value is greater than 1.


**bridges.azure_event_hub_producer.$name.kafka.buffer**

  *类型*: `bridge_kafka:producer_buffer`

  Configure producer message buffer.

Tell Azure Event Hub producer how to buffer messages when EMQX has more messages to send than Azure Event Hub can keep up, or when Azure Event Hub is down.


**bridges.azure_event_hub_producer.$name.kafka.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `async | sync`

  Query mode. Optional 'sync/async', default 'async'.


**bridges.azure_event_hub_producer.$name.kafka.sync_query_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  This parameter defines the timeout limit for synchronous queries. It applies only when the bridge query mode is configured to 'sync'.




Configuration for an Azure Event Hub bridge.

**bridges.azure_event_hub_producer.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  Enable (true) or disable (false) this bridge.


**bridges.azure_event_hub_producer.$name.bootstrap_hosts**

  *类型*: `string`

  A comma separated list of Azure Event Hub Kafka <code>host[:port]</code> namespace endpoints to bootstrap the client.  Default port number is 9093.


**bridges.azure_event_hub_producer.$name.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  Maximum wait time for TCP connection establishment (including authentication time if enabled).


**bridges.azure_event_hub_producer.$name.min_metadata_refresh_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `3s`

  Minimum time interval the client has to wait before refreshing Azure Event Hub Kafka broker and topic metadata. Setting too small value may add extra load on Azure Event Hub.


**bridges.azure_event_hub_producer.$name.metadata_request_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  Maximum wait time when fetching metadata from Azure Event Hub.


**bridges.azure_event_hub_producer.$name.authentication**

  *类型*: `bridge_azure_event_hub:auth_username_password`

  *默认值*: `{}`

  Authentication configs.


**bridges.azure_event_hub_producer.$name.socket_opts**

  *类型*: `bridge_kafka:socket_opts`

  Extra socket options.


**bridges.azure_event_hub_producer.$name.ssl**

  *类型*: `bridge_azure_event_hub:ssl_client_opts`

  *默认值*: `{"enable":true}`

  启用 SSL 连接。


**bridges.azure_event_hub_producer.$name.local_topic**

  *类型*: `string`

  MQTT topic or topic filter as data source (bridge input).  If rule action is used as data source, this config should be left empty, otherwise messages will be duplicated in Azure Event Hub.


**bridges.azure_event_hub_producer.$name.kafka**

  *类型*: `bridge_azure_event_hub:producer_kafka_opts`

  Azure Event Hub producer configs.


**bridges.azure_event_hub_producer.$name.resource_opts**

  *类型*: `bridge_kafka:resource_opts`

  *默认值*: `{}`




Socket options for SSL clients.

**bridges.azure_event_hub_producer.$name.ssl.cacertfile**

  *类型*: `string`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**bridges.azure_event_hub_producer.$name.ssl.cacerts**

  *类型*: `boolean`

  Deprecated since 5.1.4.


**bridges.azure_event_hub_producer.$name.ssl.certfile**

  *类型*: `string`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**bridges.azure_event_hub_producer.$name.ssl.keyfile**

  *类型*: `string`

  PEM格式的私钥文件。


**bridges.azure_event_hub_producer.$name.ssl.verify**

  *类型*: `enum`

  *默认值*: `verify_none`

  *可选值*: `verify_peer | verify_none`

  启用或禁用对等验证。


**bridges.azure_event_hub_producer.$name.ssl.reuse_sessions**

  *类型*: `boolean`

  *默认值*: `true`

  启用 TLS 会话重用。


**bridges.azure_event_hub_producer.$name.ssl.depth**

  *类型*: `non_neg_integer`

  *默认值*: `10`

  在有效的证书路径中，可以跟随对等证书的非自颁发中间证书的最大数量。
因此，如果深度为0，则对等方必须由受信任的根 CA 直接签名；<br/>
如果是1，路径可以是 PEER、中间 CA、ROOT-CA；<br/>
如果是2，则路径可以是PEER、中间 CA1、中间 CA2、ROOT-CA。


**bridges.azure_event_hub_producer.$name.ssl.password**

  *类型*: `string`

  包含用户密码的字符串。仅在私钥文件受密码保护时使用。


**bridges.azure_event_hub_producer.$name.ssl.versions**

  *类型*: `array`

  *默认值*: `["tlsv1.3","tlsv1.2"]`

  支持所有TLS/DTLS版本<br/>
注：PSK 的 Ciphers 无法在 <code>tlsv1.3</code> 中使用，如果打算使用 PSK 密码套件，请确保这里配置为 <code>["tlsv1.2","tlsv1.1"]</code>。


**bridges.azure_event_hub_producer.$name.ssl.ciphers**

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


**bridges.azure_event_hub_producer.$name.ssl.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**bridges.azure_event_hub_producer.$name.ssl.log_level**

  *类型*: `enum`

  *默认值*: `notice`

  *可选值*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。


**bridges.azure_event_hub_producer.$name.ssl.hibernate_after**

  *类型*: `duration`

  *默认值*: `5s`

  在闲置一定时间后休眠 SSL 进程，减少其内存占用。


**bridges.azure_event_hub_producer.$name.ssl.enable**

  *类型*: `true`

  *默认值*: `true`

  启用 TLS。


**bridges.azure_event_hub_producer.$name.ssl.server_name_indication**

  *类型*: `disable | auto | string`

  *默认值*: `auto`

  指定要在 TLS 服务器名称指示扩展中使用的主机名。<br/>
例如，当连接到 "server.example.net" 时，接受连接并执行 TLS 握手的真正服务器可能与 TLS 客户端最初连接到的主机不同，
例如，当连接到 IP 地址时，或者当主机具有多个可解析的 DNS 记录时<br/>
如果未指定，它将默认为使用的主机名字符串
建立连接，除非使用 IP 地址<br/>
然后，主机名也用于对等机的主机名验证证书<br/>
特殊值 <code>disable</code> 阻止发送服务器名称指示扩展，并禁用主机名验证检查。



### Amazon Kinesis


Configuration for an Amazon Kinesis bridge.

**bridges.kinesis_producer.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.kinesis_producer.$name.resource_opts**

  *类型*: `bridge_kinesis:creation_opts`

  *默认值*: `{}`

  资源启动相关的选项。


**bridges.kinesis_producer.$name.aws_access_key_id**

  *类型*: `string`

  Access Key ID for connecting to Amazon Kinesis.


**bridges.kinesis_producer.$name.aws_secret_access_key**

  *类型*: `string`

  AWS Secret Access Key for connecting to Amazon Kinesis.


**bridges.kinesis_producer.$name.endpoint**

  *类型*: `string`

  The url of Amazon Kinesis endpoint.


**bridges.kinesis_producer.$name.max_retries**

  *类型*: `non_neg_integer`

  *默认值*: `2`

  Max retry times if an error occurs when sending a request.


**bridges.kinesis_producer.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  The pool size.


**bridges.kinesis_producer.$name.payload_template**

  *类型*: `string`

  *默认值*: `${.}`

  The template for formatting the outgoing messages.  If undefined, will send all the available context in JSON format.


**bridges.kinesis_producer.$name.local_topic**

  *类型*: `string`

  The MQTT topic filter to be forwarded to Amazon Kinesis. All MQTT `PUBLISH` messages with the topic
matching the `local_topic` will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also `local_topic` is
configured, then both the data got from the rule and the MQTT messages that match `local_topic`
will be forwarded.


**bridges.kinesis_producer.$name.stream_name**

  *类型*: `string`

  The Amazon Kinesis Stream to publish messages to.


**bridges.kinesis_producer.$name.partition_key**

  *类型*: `string`

  The Amazon Kinesis Partition Key associated to published message. Placeholders in format of ${var} are supported.




资源启动相关的选项。

**bridges.kinesis_producer.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.kinesis_producer.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.kinesis_producer.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.kinesis_producer.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.kinesis_producer.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.kinesis_producer.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.kinesis_producer.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.kinesis_producer.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.kinesis_producer.$name.resource_opts.batch_size**

  *类型*: `pos_integer`

  *默认值*: `1`

  最大批量请求大小。如果设为1，则无批处理。


**bridges.kinesis_producer.$name.resource_opts.batch_time**

  *类型*: `timeout_duration_ms`

  *默认值*: `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。


**bridges.kinesis_producer.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.kinesis_producer.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



### Google PubSub


GCP PubSub Consumer configuration.

**bridges.gcp_pubsub_consumer.$name.consumer.pull_max_messages**

  *类型*: `pos_integer`

  *默认值*: `100`

  The maximum number of messages to retrieve from GCP PubSub in a single pull request. The actual number may be less than the specified value.


**bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping**

  *类型*: `array`

  Defines the mapping between GCP PubSub topics and MQTT topics. Must contain at least one item.




Defines the mapping between GCP PubSub topics and MQTT topics. Must contain at least one item.

**bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping.$INDEX.pubsub_topic**

  *类型*: `string`

  GCP PubSub topic to consume from.


**bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping.$INDEX.mqtt_topic**

  *类型*: `string`

  Local topic to which consumed GCP PubSub messages should be published to.


**bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping.$INDEX.qos**

  *类型*: `qos`

  *默认值*: `0`

  MQTT QoS used to publish messages consumed from GCP PubSub.


**bridges.gcp_pubsub_consumer.$name.consumer.topic_mapping.$INDEX.payload_template**

  *类型*: `string`

  *默认值*: `${.}`

  The template for transforming the incoming GCP PubSub message.  By default, it will use JSON format to serialize inputs from the GCP PubSub message.  Available fields are:
<code>message_id</code>: the message ID assigned by GCP PubSub.
<code>publish_time</code>: message timestamp assigned by GCP PubSub.
<code>topic</code>: GCP PubSub topic.
<code>value</code>: the payload of the GCP PubSub message.  Omitted if there's no payload.
<code>attributes</code>: an object containing string key-value pairs.  Omitted if there are no attributes.
<code>ordering_key</code>: GCP PubSub message ordering key.  Omitted if there's none.




Key-value pair.

**bridges.gcp_pubsub.$name.attributes_template.$INDEX.key**

  *类型*: `string`

  Key


**bridges.gcp_pubsub.$name.attributes_template.$INDEX.value**

  *类型*: `string`

  Value




GCP PubSub 桥接配置

**bridges.gcp_pubsub_consumer.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.gcp_pubsub_consumer.$name.resource_opts**

  *类型*: `bridge_gcp_pubsub:consumer_resource_opts`

  资源启动相关的选项。


**bridges.gcp_pubsub_consumer.$name.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  连接 HTTP 服务器的超时时间。


**bridges.gcp_pubsub_consumer.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  连接池大小。


**bridges.gcp_pubsub_consumer.$name.pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。


**bridges.gcp_pubsub_consumer.$name.max_retries**

  *类型*: `non_neg_integer`

  *默认值*: `2`

  请求出错时的最大重试次数。


**bridges.gcp_pubsub_consumer.$name.request_timeout**

  *类型*: `timeout_duration_ms`

  Deprecated since e5.0.1.


**bridges.gcp_pubsub_consumer.$name.service_account_json**

  *类型*: `emqx_bridge_gcp_pubsub:service_account_json`

  包含将与 PubSub 一起使用的 GCP 服务账户凭证的 JSON。
当创建GCP服务账户时（如https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount），可以选择下载 JSON 形式的凭证，然后在该配置项中使用。


**bridges.gcp_pubsub_consumer.$name.consumer**

  *类型*: `bridge_gcp_pubsub:consumer`

  Local MQTT publish and GCP PubSub consumer configs.




GCP PubSub 桥接配置

**bridges.gcp_pubsub.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.gcp_pubsub.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.gcp_pubsub.$name.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  连接 HTTP 服务器的超时时间。


**bridges.gcp_pubsub.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  连接池大小。


**bridges.gcp_pubsub.$name.pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  正整数，设置最大可发送的异步 HTTP 请求数量。当设置为 1 时，表示每次发送完成 HTTP 请求后都需要等待服务器返回，再继续发送下一个请求。


**bridges.gcp_pubsub.$name.max_retries**

  *类型*: `non_neg_integer`

  *默认值*: `2`

  请求出错时的最大重试次数。


**bridges.gcp_pubsub.$name.request_timeout**

  *类型*: `timeout_duration_ms`

  Deprecated since e5.0.1.


**bridges.gcp_pubsub.$name.service_account_json**

  *类型*: `emqx_bridge_gcp_pubsub:service_account_json`

  包含将与 PubSub 一起使用的 GCP 服务账户凭证的 JSON。
当创建GCP服务账户时（如https://developers.google.com/identity/protocols/oauth2/service-account#creatinganaccount），可以选择下载 JSON 形式的凭证，然后在该配置项中使用。


**bridges.gcp_pubsub.$name.attributes_template**

  *类型*: `array`

  *默认值*: `[]`

  The template for formatting the outgoing message attributes.  Undefined values will be rendered as empty string values.  Empty keys are removed from the attribute map.


**bridges.gcp_pubsub.$name.ordering_key_template**

  *类型*: `string`

  *默认值*: `""`

  The template for formatting the outgoing message ordering key.  Undefined values will be rendered as empty string values.  This value will not be added to the message if it's empty.


**bridges.gcp_pubsub.$name.payload_template**

  *类型*: `string`

  *默认值*: `""`

  用于格式化外发信息的模板。 如果未定义，将以JSON格式发送所有可用的上下文。


**bridges.gcp_pubsub.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 GCP PubSub。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 GCP PubSub。


**bridges.gcp_pubsub.$name.pubsub_topic**

  *类型*: `string`

  要发布消息的GCP PubSub主题。




资源启动相关的选项。

**bridges.gcp_pubsub_consumer.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `30s`

  健康检查间隔。


**bridges.gcp_pubsub_consumer.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.



### MySQL


Configuration for an HStreamDB bridge.

**bridges.mysql.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  Enable or disable this bridge


**bridges.mysql.$name.sql**

  *类型*: `string`

  *默认值*: `insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, FROM_UNIXTIME(${timestamp}/1000))`

  SQL Template


**bridges.mysql.$name.local_topic**

  *类型*: `string`

  The MQTT topic filter to be forwarded to MySQL. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.mysql.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.mysql.$name.server**

  *类型*: `string`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The MySQL default port 3306 is used if `[:Port]` is not specified.


**bridges.mysql.$name.database**

  *类型*: `string`

  数据库名字。


**bridges.mysql.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.mysql.$name.username**

  *类型*: `string`

  *默认值*: `root`

  内部数据库的用户名。


**bridges.mysql.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.mysql.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**bridges.mysql.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### Redis


集群模式。当 Redis 服务运行在集群模式下，该配置必须设置为 'cluster'。

**bridges.redis_cluster.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.redis_cluster.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 Redis。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 Redis。


**bridges.redis_cluster.$name.command_template**

  *类型*: `[binary]`

  用于推送数据的 Redis 命令模板。 每个列表元素代表一个命令名称或其参数。
例如，要通过键值 `msgs` 将消息体推送到 Redis 列表中，数组元素应该是： `rpush`, `msgs`, `${payload}`。


**bridges.redis_cluster.$name.resource_opts**

  *类型*: `bridge_redis:creation_opts_redis_cluster`

  *默认值*: `{}`

  资源相关的选项。


**bridges.redis_cluster.$name.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**bridges.redis_cluster.$name.redis_type**

  *类型*: `cluster`

  *默认值*: `cluster`

  集群模式。当 Redis 服务运行在集群模式下，该配置必须设置为 'cluster'。


**bridges.redis_cluster.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.redis_cluster.$name.username**

  *类型*: `string`

  内部数据库的用户名。


**bridges.redis_cluster.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.redis_cluster.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**bridges.redis_cluster.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。




哨兵模式。当 Redis 服务运行在哨兵模式下，该配置必须设置为 'sentinel'。

**bridges.redis_sentinel.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.redis_sentinel.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 Redis。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 Redis。


**bridges.redis_sentinel.$name.command_template**

  *类型*: `[binary]`

  用于推送数据的 Redis 命令模板。 每个列表元素代表一个命令名称或其参数。
例如，要通过键值 `msgs` 将消息体推送到 Redis 列表中，数组元素应该是： `rpush`, `msgs`, `${payload}`。


**bridges.redis_sentinel.$name.resource_opts**

  *类型*: `bridge_redis:creation_opts_redis_sentinel`

  *默认值*: `{}`

  资源相关的选项。


**bridges.redis_sentinel.$name.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**bridges.redis_sentinel.$name.redis_type**

  *类型*: `sentinel`

  *默认值*: `sentinel`

  哨兵模式。当 Redis 服务运行在哨兵模式下，该配置必须设置为 'sentinel'。


**bridges.redis_sentinel.$name.sentinel**

  *类型*: `string`

  Redis 哨兵模式下的集群名称。


**bridges.redis_sentinel.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.redis_sentinel.$name.username**

  *类型*: `string`

  内部数据库的用户名。


**bridges.redis_sentinel.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.redis_sentinel.$name.database**

  *类型*: `non_neg_integer`

  *默认值*: `0`

  Redis 数据库 ID。


**bridges.redis_sentinel.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**bridges.redis_sentinel.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。




单机模式。当 Redis 服务运行在单机模式下，该配置必须设置为 'single'。

**bridges.redis_single.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.redis_single.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 Redis。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 Redis。


**bridges.redis_single.$name.command_template**

  *类型*: `[binary]`

  用于推送数据的 Redis 命令模板。 每个列表元素代表一个命令名称或其参数。
例如，要通过键值 `msgs` 将消息体推送到 Redis 列表中，数组元素应该是： `rpush`, `msgs`, `${payload}`。


**bridges.redis_single.$name.resource_opts**

  *类型*: `bridge_redis:creation_opts_redis_single`

  *默认值*: `{}`

  资源相关的选项。


**bridges.redis_single.$name.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 Redis 默认端口 6379。


**bridges.redis_single.$name.redis_type**

  *类型*: `single`

  *默认值*: `single`

  单机模式。当 Redis 服务运行在单机模式下，该配置必须设置为 'single'。


**bridges.redis_single.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.redis_single.$name.username**

  *类型*: `string`

  内部数据库的用户名。


**bridges.redis_single.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.redis_single.$name.database**

  *类型*: `non_neg_integer`

  *默认值*: `0`

  Redis 数据库 ID。


**bridges.redis_single.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**bridges.redis_single.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。




资源启动相关的选项。

**bridges.redis_cluster.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.redis_cluster.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.redis_cluster.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.redis_cluster.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.redis_cluster.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.redis_cluster.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.redis_cluster.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.redis_cluster.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.redis_cluster.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.redis_cluster.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。




资源启动相关的选项。

**bridges.redis_sentinel.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.redis_sentinel.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.redis_sentinel.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.redis_sentinel.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.redis_sentinel.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.redis_sentinel.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.redis_sentinel.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.redis_sentinel.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.redis_sentinel.$name.resource_opts.batch_size**

  *类型*: `pos_integer`

  *默认值*: `1`

  最大批量请求大小。如果设为1，则无批处理。


**bridges.redis_sentinel.$name.resource_opts.batch_time**

  *类型*: `timeout_duration_ms`

  *默认值*: `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。


**bridges.redis_sentinel.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.redis_sentinel.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。




资源启动相关的选项。

**bridges.redis_single.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.redis_single.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.redis_single.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.redis_single.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.redis_single.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.redis_single.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.redis_single.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.redis_single.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.redis_single.$name.resource_opts.batch_size**

  *类型*: `pos_integer`

  *默认值*: `1`

  最大批量请求大小。如果设为1，则无批处理。


**bridges.redis_single.$name.resource_opts.batch_time**

  *类型*: `timeout_duration_ms`

  *默认值*: `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。


**bridges.redis_single.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.redis_single.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



### MongoDB


MongoDB（独立）配置

**bridges.mongodb_single.$name.mongo_type**

  *类型*: `single`

  *默认值*: `single`

  Standalone 模式。当 MongoDB 服务运行在 standalone 模式下，该配置必须设置为 'single'。


**bridges.mongodb_single.$name.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**bridges.mongodb_single.$name.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**bridges.mongodb_single.$name.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**bridges.mongodb_single.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.mongodb_single.$name.username**

  *类型*: `string`

  内部数据库的用户名。


**bridges.mongodb_single.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.mongodb_single.$name.use_legacy_protocol**

  *类型*: `enum`

  *默认值*: `auto`

  *可选值*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**bridges.mongodb_single.$name.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**bridges.mongodb_single.$name.database**

  *类型*: `string`

  数据库名字。


**bridges.mongodb_single.$name.topology**

  *类型*: `topology`


**bridges.mongodb_single.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.mongodb_single.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用或停用该MongoDB桥


**bridges.mongodb_single.$name.collection**

  *类型*: `string`

  *默认值*: `mqtt`

  数据将被存储到的集合


**bridges.mongodb_single.$name.payload_template**

  *类型*: `string`

  用于格式化写入 MongoDB 的消息模板。 如果未定义，规则引擎会使用 JSON 格式序列化所有的可见输入，例如 clientid, topic, payload 等。


**bridges.mongodb_single.$name.resource_opts**

  *类型*: `bridge_mongodb:creation_opts`

  资源启动相关的选项。




MongoDB（Replica Set）配置

**bridges.mongodb_rs.$name.mongo_type**

  *类型*: `rs`

  *默认值*: `rs`

  Replica set模式。当 MongoDB 服务运行在 replica-set 模式下，该配置必须设置为 'rs'。


**bridges.mongodb_rs.$name.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**bridges.mongodb_rs.$name.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**bridges.mongodb_rs.$name.r_mode**

  *类型*: `enum`

  *默认值*: `master`

  *可选值*: `master | slave_ok`

  读模式。


**bridges.mongodb_rs.$name.replica_set_name**

  *类型*: `string`

  副本集的名称。


**bridges.mongodb_rs.$name.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**bridges.mongodb_rs.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.mongodb_rs.$name.username**

  *类型*: `string`

  内部数据库的用户名。


**bridges.mongodb_rs.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.mongodb_rs.$name.use_legacy_protocol**

  *类型*: `enum`

  *默认值*: `auto`

  *可选值*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**bridges.mongodb_rs.$name.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**bridges.mongodb_rs.$name.database**

  *类型*: `string`

  数据库名字。


**bridges.mongodb_rs.$name.topology**

  *类型*: `topology`


**bridges.mongodb_rs.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.mongodb_rs.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用或停用该MongoDB桥


**bridges.mongodb_rs.$name.collection**

  *类型*: `string`

  *默认值*: `mqtt`

  数据将被存储到的集合


**bridges.mongodb_rs.$name.payload_template**

  *类型*: `string`

  用于格式化写入 MongoDB 的消息模板。 如果未定义，规则引擎会使用 JSON 格式序列化所有的可见输入，例如 clientid, topic, payload 等。


**bridges.mongodb_rs.$name.resource_opts**

  *类型*: `bridge_mongodb:creation_opts`

  资源启动相关的选项。




MongoDB (Sharded)配置

**bridges.mongodb_sharded.$name.mongo_type**

  *类型*: `sharded`

  *默认值*: `sharded`

  Sharded cluster模式。当 MongoDB 服务运行在 sharded 模式下，该配置必须设置为 'sharded'。


**bridges.mongodb_sharded.$name.servers**

  *类型*: `string`

  集群将要连接的节点列表。 节点之间用逗号分隔，如：`Node[,Node].`
每个节点的配置为：将要连接的 IPv4 或 IPv6 地址或主机名。
主机名具有以下形式：`Host[:Port]`。
如果未指定 `[:Port]`，则使用 MongoDB 默认端口 27017。


**bridges.mongodb_sharded.$name.w_mode**

  *类型*: `enum`

  *默认值*: `unsafe`

  *可选值*: `unsafe | safe`

  写模式。


**bridges.mongodb_sharded.$name.srv_record**

  *类型*: `boolean`

  *默认值*: `false`

  使用 DNS SRV 记录。


**bridges.mongodb_sharded.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.mongodb_sharded.$name.username**

  *类型*: `string`

  内部数据库的用户名。


**bridges.mongodb_sharded.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.mongodb_sharded.$name.use_legacy_protocol**

  *类型*: `enum`

  *默认值*: `auto`

  *可选值*: `auto | true | false`

  Whether to use MongoDB's legacy protocol for communicating with the database.  The default is to attempt to automatically determine if the newer protocol is supported.


**bridges.mongodb_sharded.$name.auth_source**

  *类型*: `string`

  与用户证书关联的数据库名称。


**bridges.mongodb_sharded.$name.database**

  *类型*: `string`

  数据库名字。


**bridges.mongodb_sharded.$name.topology**

  *类型*: `topology`


**bridges.mongodb_sharded.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.mongodb_sharded.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用或停用该MongoDB桥


**bridges.mongodb_sharded.$name.collection**

  *类型*: `string`

  *默认值*: `mqtt`

  数据将被存储到的集合


**bridges.mongodb_sharded.$name.payload_template**

  *类型*: `string`

  用于格式化写入 MongoDB 的消息模板。 如果未定义，规则引擎会使用 JSON 格式序列化所有的可见输入，例如 clientid, topic, payload 等。


**bridges.mongodb_sharded.$name.resource_opts**

  *类型*: `bridge_mongodb:creation_opts`

  资源启动相关的选项。




资源启动相关的选项。

**bridge_mongodb:creation_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridge_mongodb:creation_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridge_mongodb:creation_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridge_mongodb:creation_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridge_mongodb:creation_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridge_mongodb:creation_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridge_mongodb:creation_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridge_mongodb:creation_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridge_mongodb:creation_opts.batch_time**

  *类型*: `timeout_duration_ms`

  *默认值*: `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。


**bridge_mongodb:creation_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridge_mongodb:creation_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



### InfluxDB


InfluxDB HTTP API 协议。支持 Influxdb v1.8 以及之前的版本。

**bridges.influxdb_api_v1.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.influxdb_api_v1.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 InfluxDB。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 InfluxDB。


**bridges.influxdb_api_v1.$name.write_syntax**

  *类型*: `emqx_bridge_influxdb:write_syntax`

  使用 InfluxDB API Line Protocol 写入 InfluxDB 的数据，支持占位符<br/>
参考 [InfluxDB 2.3 Line Protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) 及
[InfluxDB 1.8 Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) <br/>
TLDR: <br/>
```
\<measurement>[,\<tag_key>=\<tag_value>[,\<tag_key>=\<tag_value>]] \<field_key>=\<field_value>[,\<field_key>=\<field_value>] [\<timestamp>]
```
注意，整形数值占位符后需要添加一个字符 `i` 类型标识。例如 `${payload.int_value}i`


**bridges.influxdb_api_v1.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.influxdb_api_v1.$name.server**

  *类型*: `string`

  *默认值*: `127.0.0.1:8086`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 InfluxDB 默认端口 8086。


**bridges.influxdb_api_v1.$name.precision**

  *类型*: `enum`

  *默认值*: `ms`

  *可选值*: `ns | us | ms | s`

  InfluxDB 时间精度。


**bridges.influxdb_api_v1.$name.database**

  *类型*: `string`

  InfluxDB 数据库。


**bridges.influxdb_api_v1.$name.username**

  *类型*: `string`

  InfluxDB 用户名。


**bridges.influxdb_api_v1.$name.password**

  *类型*: `string`

  InfluxDB 密码。


**bridges.influxdb_api_v1.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。




InfluxDB HTTP API V2 协议。支持 Influxdb v2.0 以及之后的版本。

**bridges.influxdb_api_v2.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.influxdb_api_v2.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 InfluxDB。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发到 InfluxDB。


**bridges.influxdb_api_v2.$name.write_syntax**

  *类型*: `emqx_bridge_influxdb:write_syntax`

  使用 InfluxDB API Line Protocol 写入 InfluxDB 的数据，支持占位符<br/>
参考 [InfluxDB 2.3 Line Protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) 及
[InfluxDB 1.8 Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) <br/>
TLDR: <br/>
```
\<measurement>[,\<tag_key>=\<tag_value>[,\<tag_key>=\<tag_value>]] \<field_key>=\<field_value>[,\<field_key>=\<field_value>] [\<timestamp>]
```
注意，整形数值占位符后需要添加一个字符 `i` 类型标识。例如 `${payload.int_value}i`


**bridges.influxdb_api_v2.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.influxdb_api_v2.$name.server**

  *类型*: `string`

  *默认值*: `127.0.0.1:8086`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 InfluxDB 默认端口 8086。


**bridges.influxdb_api_v2.$name.precision**

  *类型*: `enum`

  *默认值*: `ms`

  *可选值*: `ns | us | ms | s`

  InfluxDB 时间精度。


**bridges.influxdb_api_v2.$name.bucket**

  *类型*: `string`

  InfluxDB bucket 名称。


**bridges.influxdb_api_v2.$name.org**

  *类型*: `string`

  InfluxDB 组织名称。


**bridges.influxdb_api_v2.$name.token**

  *类型*: `string`

  InfluxDB token。


**bridges.influxdb_api_v2.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### PostgreSQL


PostgreSQL 桥接配置

**bridge_pgsql:config.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用桥接


**bridge_pgsql:config.sql**

  *类型*: `string`

  *默认值*: `insert into t_mqtt_msg(msgid, topic, qos, payload, arrived) values (${id}, ${topic}, ${qos}, ${payload}, TO_TIMESTAMP((${timestamp} :: bigint)/1000))`

  SQL 模板


**bridge_pgsql:config.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 PostgreSQL。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。


**bridge_pgsql:config.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridge_pgsql:config.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 PostgreSQL 默认端口 5432。


**bridge_pgsql:config.database**

  *类型*: `string`

  数据库名字。


**bridge_pgsql:config.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridge_pgsql:config.username**

  *类型*: `string`

  内部数据库的用户名。


**bridge_pgsql:config.password**

  *类型*: `string`

  内部数据库密码。


**bridge_pgsql:config.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**bridge_pgsql:config.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### TDengine


TDengine 桥接配置

**bridges.tdengine.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用桥接


**bridges.tdengine.$name.sql**

  *类型*: `string`

  *默认值*: `insert into t_mqtt_msg(ts, msgid, mqtt_topic, qos, payload, arrived) values (${ts}, '${id}', '${topic}', ${qos}, '${payload}', ${timestamp})`

  SQL 模板


**bridges.tdengine.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 TDengine。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。


**bridges.tdengine.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.tdengine.$name.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 TDengine 默认端口 6041。


**bridges.tdengine.$name.database**

  *类型*: `string`

  数据库名字。


**bridges.tdengine.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.tdengine.$name.username**

  *类型*: `string`

  *默认值*: `root`

  内部数据库的用户名。


**bridges.tdengine.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.tdengine.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.



### TimescaleDB

### Apache IoTDB


基本认证（basic auth）的参数。

**bridges.iotdb.$name.authentication.username**

  *类型*: `string`

  Basic auth 用户名。类似 IoTDB REST 接口中的用户名。


**bridges.iotdb.$name.authentication.password**

  *类型*: `string`

  Basic auth 密码。类似 IoTDB REST 接口中的密码。




IoTDB Bridge 配置

**bridges.iotdb.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用或禁用该桥接


**bridges.iotdb.$name.authentication**

  *类型*: [bridge_iotdb:auth_basic](#bridge_iotdb:auth_basic)

  *默认值*: `auth_basic`

  认证信息


**bridges.iotdb.$name.is_aligned**

  *类型*: `boolean`

  *默认值*: `false`

  是否对齐时间序列。


**bridges.iotdb.$name.device_id**

  *类型*: `string`

  IoTDB 的设备ID（DeviceID）。可以使用一个占位符。如果留空则 MQTT 消息体中必须有一个 `device_id` 字段，
或者 EMQX 规则引擎的 SQL 必须输出一个 `device_id` 字段。


**bridges.iotdb.$name.iotdb_version**

  *类型*: `enum`

  *默认值*: `v1.1.x`

  *可选值*: `v1.1.x | v1.0.x | v0.13.x`

  IoTDB 版本。


**bridges.iotdb.$name.resource_opts**

  *类型*: `bridge_iotdb:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.iotdb.$name.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  The timeout when connecting to the HTTP server.


**bridges.iotdb.$name.retry_interval**

  *类型*: `timeout_duration`

  Deprecated since 5.0.4.


**bridges.iotdb.$name.pool_type**

  *类型*: `emqx_bridge_http_connector:pool_type`

  *默认值*: `random`

  The type of the pool. Can be one of `random`, `hash`.


**bridges.iotdb.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  The pool size.


**bridges.iotdb.$name.enable_pipelining**

  *类型*: `pos_integer`

  *默认值*: `100`

  A positive integer. Whether to send HTTP requests continuously, when set to 1, it means that after each HTTP request is sent, you need to wait for the server to return and then continue to send the next request.


**bridges.iotdb.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。


**bridges.iotdb.$name.base_url**

  *类型*: `url`

  IoTDB REST 服务的 URL。


**bridges.iotdb.$name.max_retries**

  *类型*: `non_neg_integer`

  *默认值*: `2`

  HTTP 请求的最大重试次数。




资源启动相关的选项。

**bridges.iotdb.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.iotdb.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.iotdb.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.iotdb.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.iotdb.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.iotdb.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.iotdb.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.iotdb.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.iotdb.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.iotdb.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



### MatrixDB

### OpenTSDB


OpenTSDB 桥接配置

**bridges.opents.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用桥接


**bridges.opents.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.opents.$name.server**

  *类型*: `string`

  服务器的地址。


**bridges.opents.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.opents.$name.summary**

  *类型*: `boolean`

  *默认值*: `true`

  是否返回摘要信息。


**bridges.opents.$name.details**

  *类型*: `boolean`

  *默认值*: `false`

  是否返回详细信息。


**bridges.opents.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.



### GreptimeDB


GreptimeDB's protocol. Support GreptimeDB v1.8 and before.

**bridges.greptimedb.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.greptimedb.$name.local_topic**

  *类型*: `string`

  The MQTT topic filter to be forwarded to the GreptimeDB. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.greptimedb.$name.write_syntax**

  *类型*: `emqx_bridge_influxdb:write_syntax`

  Conf of GreptimeDB gRPC protocol to write data points. Write syntax is a text-based format that provides the measurement, tag set, field set, and timestamp of a data point, and placeholder supported, which is the same as InfluxDB line protocol.
See also [InfluxDB 2.3 Line Protocol](https://docs.influxdata.com/influxdb/v2.3/reference/syntax/line-protocol/) and
[GreptimeDB 1.8 Line Protocol](https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_tutorial/) <br/>
TLDR:<br/>
```
\<measurement>[,\<tag_key>=\<tag_value>[,\<tag_key>=\<tag_value>]] \<field_key>=\<field_value>[,\<field_key>=\<field_value>] [\<timestamp>]
```
Please note that a placeholder for an integer value must be annotated with a suffix `i`. For example `${payload.int_value}i`.


**bridges.greptimedb.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.greptimedb.$name.server**

  *类型*: `string`

  *默认值*: `127.0.0.1:4001`

  The IPv4 or IPv6 address or the hostname to connect to.<br/>
A host entry has the following form: `Host[:Port]`.<br/>
The GreptimeDB default port 8086 is used if `[:Port]` is not specified.


**bridges.greptimedb.$name.precision**

  *类型*: `enum`

  *默认值*: `ms`

  *可选值*: `ns | us | ms | s`

  GreptimeDB time precision.


**bridges.greptimedb.$name.dbname**

  *类型*: `string`

  GreptimeDB database.


**bridges.greptimedb.$name.username**

  *类型*: `string`

  GreptimeDB username.


**bridges.greptimedb.$name.password**

  *类型*: `string`

  GreptimeDB password.


**bridges.greptimedb.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### ClickHouse


Clickhouse 桥接配置

**bridges.clickhouse.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用桥接


**bridges.clickhouse.$name.sql**

  *类型*: `string`

  *默认值*: `INSERT INTO mqtt_test(payload, arrived) VALUES ('${payload}', ${timestamp})`

  可以使用 ${field} 占位符来引用消息与客户端上下文中的变量，请确保对应字段存在且数据格式符合预期。此处不支持 [SQL 预处理](https://docs.emqx.com/zh/enterprise/v5.0/data-integration/data-bridges.html#sql-预处理)。


**bridges.clickhouse.$name.batch_value_separator**

  *类型*: `string`

  *默认值*: `, `

  默认为逗号 ','，适用于 VALUE 格式。您也可以使用其他分隔符， 请参考 [INSERT INTO 语句](https://clickhouse.com/docs/en/sql-reference/statements/insert-into)。


**bridges.clickhouse.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 Clickhouse。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。


**bridges.clickhouse.$name.resource_opts**

  *类型*: `bridge_clickhouse:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.clickhouse.$name.url**

  *类型*: `emqx_bridge_clickhouse_connector:url`

  你想连接到的Clickhouse服务器的HTTP URL（例如http://myhostname:8123）。


**bridges.clickhouse.$name.connect_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  连接HTTP服务器的超时时间。


**bridges.clickhouse.$name.database**

  *类型*: `string`

  数据库名字。


**bridges.clickhouse.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.clickhouse.$name.username**

  *类型*: `string`

  内部数据库的用户名。


**bridges.clickhouse.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.clickhouse.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.




资源启动相关的选项。

**bridges.clickhouse.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.clickhouse.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.clickhouse.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.clickhouse.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.clickhouse.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.clickhouse.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.clickhouse.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.clickhouse.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.clickhouse.$name.resource_opts.batch_size**

  *类型*: `pos_integer`

  *默认值*: `1`

  最大批量请求大小。如果设为1，则无批处理。


**bridges.clickhouse.$name.resource_opts.batch_time**

  *类型*: `timeout_duration_ms`

  *默认值*: `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。


**bridges.clickhouse.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.clickhouse.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



### DynamoDB


DynamoDB 桥接配置

**bridges.dynamo.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用桥接


**bridges.dynamo.$name.template**

  *类型*: `string`

  *默认值*: `""`

  模板, 默认为空，为空时将会将整个消息存入数据库


**bridges.dynamo.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 DynamoDB。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。


**bridges.dynamo.$name.resource_opts**

  *类型*: `bridge_dynamo:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.dynamo.$name.url**

  *类型*: `string`

  DynamoDB 的地址。


**bridges.dynamo.$name.table**

  *类型*: `string`

  DynamoDB 的表。


**bridges.dynamo.$name.aws_access_key_id**

  *类型*: `string`

  DynamoDB 的访问 ID。


**bridges.dynamo.$name.aws_secret_access_key**

  *类型*: `string`

  DynamoDB 的访问密钥。


**bridges.dynamo.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.dynamo.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.




资源启动相关的选项。

**bridges.dynamo.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.dynamo.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.dynamo.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.dynamo.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.dynamo.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.dynamo.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.dynamo.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.dynamo.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.dynamo.$name.resource_opts.batch_size**

  *类型*: `pos_integer`

  *默认值*: `1`

  最大批量请求大小。如果设为1，则无批处理。


**bridges.dynamo.$name.resource_opts.batch_time**

  *类型*: `timeout_duration_ms`

  *默认值*: `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。


**bridges.dynamo.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.dynamo.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



### Cassandra


Cassandra 桥接配置

**bridges.cassandra.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用桥接


**bridges.cassandra.$name.cql**

  *类型*: `string`

  *默认值*: `insert into mqtt_msg(topic, msgid, sender, qos, payload, arrived, retain) values (${topic}, ${id}, ${clientid}, ${qos}, ${payload}, ${timestamp}, ${flags.retain})`

  CQL 模板


**bridges.cassandra.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 Cassandra。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。


**bridges.cassandra.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.cassandra.$name.servers**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port][,Host2:Port]`。<br/>
如果未指定 `[:Port]`，则使用 Cassandra 默认端口 9042。


**bridges.cassandra.$name.keyspace**

  *类型*: `string`

  要连接到的 Keyspace 名称。


**bridges.cassandra.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.cassandra.$name.username**

  *类型*: `string`

  内部数据库的用户名。


**bridges.cassandra.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.cassandra.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.


**bridges.cassandra.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



### Microsoft SQL Server


Microsoft SQL Server 桥接配置

**bridges.sqlserver.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用桥接


**bridges.sqlserver.$name.sql**

  *类型*: `string`

  *默认值*: `insert into t_mqtt_msg(msgid, topic, qos, payload) values ( ${id}, ${topic}, ${qos}, ${payload} )`

  SQL 模板


**bridges.sqlserver.$name.driver**

  *类型*: `string`

  *默认值*: `ms-sql`

  SQL Server Driver 名称


**bridges.sqlserver.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 Microsoft SQL Server。 <br/>
注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。


**bridges.sqlserver.$name.resource_opts**

  *类型*: `bridge_sqlserver:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.sqlserver.$name.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>
主机名具有以下形式：`Host[:Port]`。<br/>
如果未指定 `[:Port]`，则使用 SQL Server 默认端口 1433。


**bridges.sqlserver.$name.database**

  *类型*: `string`

  数据库名字。


**bridges.sqlserver.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.sqlserver.$name.username**

  *类型*: `string`

  *默认值*: `sa`

  内部数据库的用户名。


**bridges.sqlserver.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.sqlserver.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.




资源启动相关的选项。

**bridges.sqlserver.$name.resource_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**bridges.sqlserver.$name.resource_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**bridges.sqlserver.$name.resource_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**bridges.sqlserver.$name.resource_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**bridges.sqlserver.$name.resource_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**bridges.sqlserver.$name.resource_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**bridges.sqlserver.$name.resource_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**bridges.sqlserver.$name.resource_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**bridges.sqlserver.$name.resource_opts.batch_size**

  *类型*: `pos_integer`

  *默认值*: `1`

  最大批量请求大小。如果设为1，则无批处理。


**bridges.sqlserver.$name.resource_opts.batch_time**

  *类型*: `timeout_duration_ms`

  *默认值*: `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。


**bridges.sqlserver.$name.resource_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**bridges.sqlserver.$name.resource_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



### Oracle Database


Oracle Database 桥接配置

**bridges.oracle.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用桥接


**bridges.oracle.$name.sql**

  *类型*: `string`

  *默认值*: `insert into t_mqtt_msgs(msgid, topic, qos, payload) values (${id}, ${topic}, ${qos}, ${payload})`

  SQL模板。模板字符串可以包含消息元数据和有效载荷字段的占位符。占位符的插入不需要任何检查和特殊格式化，因此必须确保插入的数值格式化和转义正确。模板字符串可以包含占位符模板字符串可以包含消息元数据和有效载荷字段的占位符。这些占位符被插入所以必须确保插入的值的格式正确。因此，确保插入的值格式化和转义正确是非常重要的。模板字符串可以包含占位符模板字符串可以包含消息元数据和有效载荷字段的占位符。这些占位符被插入所以必须确保插入的值的格式正确。确保插入的值被正确地格式化和转义。


**bridges.oracle.$name.local_topic**

  *类型*: `string`

  发送到 'local_topic' 的消息都会转发到 Oracle Database。 <br/>注意：如果这个 Bridge 被用作规则（EMQX 规则引擎）的输出，同时也配置了 'local_topic' ，那么这两部分的消息都会被转发。


**bridges.oracle.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.oracle.$name.server**

  *类型*: `string`

  将要连接的 IPv4 或 IPv6 地址，或者主机名。<br/>主机名具有以下形式：`Host[:Port]`。<br/>如果未指定 `[:Port]`，则使用 Oracle Database 默认端口 1521。


**bridges.oracle.$name.sid**

  *类型*: `string`

  Oracle Database Sid 名称


**bridges.oracle.$name.service_name**

  *类型*: `string`

  Oracle Database 服务名称。


**bridges.oracle.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.oracle.$name.username**

  *类型*: `string`

  内部数据库的用户名。


**bridges.oracle.$name.password**

  *类型*: `string`

  内部数据库密码。


**bridges.oracle.$name.auto_reconnect**

  *类型*: `boolean`

  Deprecated since v5.0.15.



### HStreamDB


Configuration for an HStreamDB bridge.

**bridges.hstreamdb.$name.enable**

  *类型*: `boolean`

  *默认值*: `true`

  启用/禁用 Bridge


**bridges.hstreamdb.$name.direction**

  *类型*: `egress`

  *默认值*: `egress`

  The direction of this bridge, MUST be 'egress'


**bridges.hstreamdb.$name.local_topic**

  *类型*: `string`

  The MQTT topic filter to be forwarded to the HStreamDB. All MQTT 'PUBLISH' messages with the topic
matching the local_topic will be forwarded.<br/>
NOTE: if this bridge is used as the action of a rule (EMQX rule engine), and also local_topic is
configured, then both the data got from the rule and the MQTT messages that match local_topic
will be forwarded.


**bridges.hstreamdb.$name.record_template**

  *类型*: `string`

  *默认值*: `${payload}`

  The HStream Record template to be forwarded to the HStreamDB. Placeholders supported.<br>
NOTE: When you use `raw record` template (which means the data is not a valid JSON), you should use `read` or `subscription` in HStream to get the data.


**bridges.hstreamdb.$name.resource_opts**

  *类型*: `resource_schema:creation_opts`

  *默认值*: `{}`

  资源相关的选项。


**bridges.hstreamdb.$name.url**

  *类型*: `string`

  *默认值*: `http://127.0.0.1:6570`

  HStreamDB Server URL. Using gRPC http server address.


**bridges.hstreamdb.$name.stream**

  *类型*: `string`

  HStreamDB Stream Name.


**bridges.hstreamdb.$name.partition_key**

  *类型*: `string`

  HStreamDB Partition Key. Placeholders supported.


**bridges.hstreamdb.$name.pool_size**

  *类型*: `pos_integer`

  *默认值*: `8`

  桥接远端服务时使用的连接池大小。


**bridges.hstreamdb.$name.grpc_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `30s`

  HStreamDB gRPC Timeout.


**bridges.hstreamdb.$name.ssl**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  *默认值*: `{"enable":false}`

  启用 SSL 连接。



{% endemqxee %}

### 附录：公共参数


资源启动相关的选项。

**resource_schema:creation_opts.worker_pool_size**

  *类型*: `integer`

  *默认值*: `16`

  *可选值*: `1-1024`

  缓存队列 worker 数量。仅对 egress 类型的桥接有意义。当桥接仅有 ingress 方向时，可设置为 0，否则必须大于 0。


**resource_schema:creation_opts.health_check_interval**

  *类型*: `timeout_duration_ms`

  *默认值*: `15s`

  健康检查间隔。


**resource_schema:creation_opts.start_after_created**

  *类型*: `boolean`

  *默认值*: `true`

  是否在创建资源后立即启动资源。


**resource_schema:creation_opts.start_timeout**

  *类型*: `timeout_duration_ms`

  *默认值*: `5s`

  在回复资源创建请求前等待资源进入健康状态的时间。


**resource_schema:creation_opts.auto_restart_interval**

  *类型*: `infinity | duration_ms`

  Deprecated since 5.1.0.


**resource_schema:creation_opts.query_mode**

  *类型*: `enum`

  *默认值*: `async`

  *可选值*: `sync | async`

  请求模式。可选 '同步/异步'，默认为'异步'模式。


**resource_schema:creation_opts.request_ttl**

  *类型*: `timeout_duration_ms | infinity`

  *默认值*: `45s`

  Starting from the moment when the request enters the buffer, if the request remains in the buffer for the specified time or is sent but does not receive a response or acknowledgement in time, the request is considered expired.


**resource_schema:creation_opts.inflight_window**

  *类型*: `pos_integer`

  *默认值*: `100`

  请求飞行队列窗口大小。当请求模式为异步时，如果需要严格保证来自同一 MQTT 客户端的消息有序，则必须将此值设为 1。


**resource_schema:creation_opts.batch_size**

  *类型*: `pos_integer`

  *默认值*: `1`

  最大批量请求大小。如果设为1，则无批处理。


**resource_schema:creation_opts.batch_time**

  *类型*: `timeout_duration_ms`

  *默认值*: `0ms`

  在较低消息率情况下尝试累积批量输出时的最大等待间隔，以提高资源的利用率。


**resource_schema:creation_opts.enable_queue**

  *类型*: `boolean`

  Deprecated since v5.0.14.


**resource_schema:creation_opts.max_buffer_bytes**

  *类型*: `bytesize`

  *默认值*: `256MB`

  每个缓存 worker 允许使用的最大字节数。



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

  *类型*: `timeout_duration`

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



## 网关

### CoAP


The CoAP protocol gateway provides EMQX with the access capability of the CoAP protocol.
It allows publishing, subscribing, and receiving messages to EMQX in accordance
with a certain defined CoAP message format.

**gateway.coap.heartbeat**

  *类型*: `emqx_coap_schema:duration`

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

  发布或订阅时，在所有主题前增加前缀字符串。
当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
则客户端实际上订阅了 `some_tenant/t` 主题。
类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
挂载点字符串中可用的变量：<br/>
   - <code>${clientid}</code>：clientid<br/>
   - <code>${username}</code>：用户名


**gateway.coap.listeners**

  *类型*: `gateway:udp_listeners`


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



### LwM2M


The LwM2M protocol gateway.

**gateway.lwm2m.xml_dir**

  *类型*: `string`

  LwM2M Resource 定义的 XML 文件目录路径。


**gateway.lwm2m.lifetime_min**

  *类型*: `emqx_lwm2m_schema:duration`

  *默认值*: `15s`

  允许 LwM2M 客户端允许设置的心跳最小值。


**gateway.lwm2m.lifetime_max**

  *类型*: `emqx_lwm2m_schema:duration`

  *默认值*: `86400s`

  允许 LwM2M 客户端允许设置的心跳最大值。


**gateway.lwm2m.qmode_time_window**

  *类型*: `emqx_lwm2m_schema:duration_s`

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

  *类型*: `lwm2m_translators`

  LwM2M 网关订阅/发布消息的主题映射配置。


**gateway.lwm2m.mountpoint**

  *类型*: `string`

  *默认值*: `lwm2m/${endpoint_name}/`

  发布或订阅时，在所有主题前增加前缀字符串。
当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
则客户端实际上订阅了 `some_tenant/t` 主题。
类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
挂载点字符串中可用的变量：<br/>
   - <code>${clientid}</code>：clientid<br/>
   - <code>${username}</code>：用户名


**gateway.lwm2m.listeners**

  *类型*: `gateway:udp_listeners`


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




MQTT topics that correspond to LwM2M events.

**gateway.lwm2m.translators.command**

  *类型*: `translator`

  下行命令主题。
对于每个成功上线的新 LwM2M 客户端，网关会创建一个订阅关系来接收下行消息并将其发送给客户端。


**gateway.lwm2m.translators.response**

  *类型*: `translator`

  用于网关发布来自 LwM2M 客户端的确认事件的主题。


**gateway.lwm2m.translators.notify**

  *类型*: `translator`

  用于发布来自 LwM2M 客户端的通知事件的主题。
在成功 Observe 到 LwM2M 客户端的资源后，如果客户端报告任何资源状态的变化，网关将通过该主题发送通知事件。


**gateway.lwm2m.translators.register**

  *类型*: `translator`

  用于发布来自 LwM2M 客户端的注册事件的主题。


**gateway.lwm2m.translators.update**

  *类型*: `translator`

  用于发布来自LwM2M客户端的更新事件的主题。




MQTT topic that corresponds to a particular type of event.

**translator.topic**

  *类型*: `string`

  主题名称


**translator.qos**

  *类型*: `qos`

  *默认值*: `0`

  QoS 等级




配置 Topology

**topology.max_overflow**

  *类型*: `non_neg_integer`

  *默认值*: `0`

  最大溢出。


**topology.overflow_ttl**

  *类型*: `timeout_duration_ms`

  当池内工人太多时，等待多久清除多余工人。


**topology.overflow_check_period**

  *类型*: `timeout_duration_ms`

  检查是否有超过配置的工人的周期（"溢出"）。


**topology.local_threshold_ms**

  *类型*: `timeout_duration_ms`

  在多个合适的MongoDB实例中进行选择的延迟窗口的大小。


**topology.connect_timeout_ms**

  *类型*: `timeout_duration_ms`

  超时重连的等待时间。


**topology.socket_timeout_ms**

  *类型*: `timeout_duration_ms`

  在尝试超时之前，在套接字上尝试发送或接收的持续时间。


**topology.server_selection_timeout_ms**

  *类型*: `timeout_duration_ms`

  指定在抛出异常之前为服务器选择阻断多长时间。


**topology.wait_queue_timeout_ms**

  *类型*: `timeout_duration_ms`

  工作者等待连接可用的最长时间。


**topology.heartbeat_frequency_ms**

  *类型*: `timeout_duration_ms`

  *默认值*: `200s`

  控制驱动程序何时检查MongoDB部署的状态。指定检查的间隔时间，从上一次检查结束到下一次检查开始计算。如果连接数增加（例如，如果你增加池子的大小，就会发生这种情况），你可能也需要增加这个周期，以避免在MongoDB日志文件中创建太多的日志条目。


**topology.min_heartbeat_frequency_ms**

  *类型*: `timeout_duration_ms`

  心跳间的最小间隙



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

  发布或订阅时，在所有主题前增加前缀字符串。
当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
则客户端实际上订阅了 `some_tenant/t` 主题。
类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
挂载点字符串中可用的变量：<br/>
   - <code>${clientid}</code>：clientid<br/>
   - <code>${username}</code>：用户名


**gateway.mqttsn.listeners**

  *类型*: `gateway:udp_listeners`


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




The pre-defined topic name corresponding to the pre-defined topic
ID of N.

Note: the pre-defined topic ID of 0 is reserved.

**gateway.mqttsn.predefined.$INDEX.id**

  *类型*: `integer`

  *可选值*: `1-1024`

  主题 ID。范围：1-65535


**gateway.mqttsn.predefined.$INDEX.topic**

  *类型*: `string`

  主题名称。注：不支持通配符



### STOP


The STOMP protocol gateway provides EMQX with the ability to access STOMP
(Simple (or Streaming) Text Orientated Messaging Protocol) protocol.

**gateway.stomp.frame**

  *类型*: `stomp_frame`


**gateway.stomp.mountpoint**

  *类型*: `string`

  *默认值*: `""`

  发布或订阅时，在所有主题前增加前缀字符串。
当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
则客户端实际上订阅了 `some_tenant/t` 主题。
类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
挂载点字符串中可用的变量：<br/>
   - <code>${clientid}</code>：clientid<br/>
   - <code>${username}</code>：用户名


**gateway.stomp.listeners**

  *类型*: `gateway:tcp_listeners`


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



### ExProto


Settings for EMQX extension protocol (exproto).

**gateway.exproto.server**

  *类型*: `exproto_grpc_server`

  配置 ExProto 网关需要启动的 <code>ConnectionAdapter</code> 服务。
该服务用于提供客户端的认证、发布、订阅和数据下行等功能。


**gateway.exproto.handler**

  *类型*: `exproto_grpc_handler`

  配置 ExProto 网关需要请求的 <code>ConnectionHandler</code> 服务地址。
该服务用于给 ExProto 提供客户端的 Socket 事件处理、字节解码、订阅消息接收等功能。


**gateway.exproto.mountpoint**

  *类型*: `string`

  *默认值*: `""`

  发布或订阅时，在所有主题前增加前缀字符串。
当消息投递给订阅者时，前缀字符串将从主题名称中删除。挂载点是用户可以用来实现不同监听器之间的消息路由隔离的一种方式。
例如，如果客户端 A 在 `listeners.tcp.\<name>.mountpoint` 设置为 `some_tenant` 的情况下订阅 `t`，
则客户端实际上订阅了 `some_tenant/t` 主题。
类似地，如果另一个客户端 B（连接到与客户端 A 相同的侦听器）向主题 `t` 发送消息，
则该消息被路由到所有订阅了 `some_tenant/t` 的客户端，因此客户端 A 将收到该消息，带有 主题名称`t`。 设置为 `""` 以禁用该功能。
挂载点字符串中可用的变量：<br/>
   - <code>${clientid}</code>：clientid<br/>
   - <code>${username}</code>：用户名


**gateway.exproto.listeners**

  *类型*: `gateway:tcp_udp_listeners`


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




Settings for the exproto gRPC connection handler.

**gateway.exproto.handler.address**

  *类型*: `string`

  对端 gRPC 服务器地址。


**gateway.exproto.handler.service_name**

  *类型*: `ConnectionHandler | ConnectionUnaryHandler`

  *默认值*: `ConnectionUnaryHandler`

  The service name to handle the connection events.
In the initial version, we expected to use streams to improve the efficiency
of requests in `ConnectionHandler`. But unfortunately, events between different
streams are out of order. It causes the `OnSocketCreated` event to may arrive
later than `OnReceivedBytes`.
So we added the `ConnectionUnaryHandler` service since v5.0.25 and forced
the use of Unary in it to avoid ordering problems.


**gateway.exproto.handler.ssl_options**

  *类型*: [ssl_client_opts](#客户端-ssl-tls-配置)

  gRPC 客户端的 SSL 配置。




Settings for the exproto gRPC server.

**gateway.exproto.server.bind**

  *类型*: `emqx_exproto_schema:ip_port`

  服务监听地址和端口。


**gateway.exproto.server.ssl_options**

  *类型*: `ssl_server_opts`

  服务 SSL 配置。




SSL configuration for the server.

**gateway.exproto.server.ssl_options.cacertfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/cacert.pem`

  受信任的PEM格式 CA  证书捆绑文件<br/>
此文件中的证书用于验证TLS对等方的证书。
如果要信任新 CA，请将新证书附加到文件中。
无需重启EMQX即可加载更新的文件，因为系统会定期检查文件是否已更新（并重新加载）<br/>
注意：从文件中失效（删除）证书不会影响已建立的连接。


**gateway.exproto.server.ssl_options.cacerts**

  *类型*: `boolean`

  Deprecated since 5.1.4.


**gateway.exproto.server.ssl_options.certfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/cert.pem`

  PEM格式证书链文件<br/>
此文件中的证书应与证书颁发链的顺序相反。也就是说，主机的证书应该放在文件的开头，
然后是直接颁发者 CA 证书，依此类推，一直到根 CA 证书。
根 CA 证书是可选的，如果想要添加，应加到文件到最末端。


**gateway.exproto.server.ssl_options.keyfile**

  *类型*: `string`

  *默认值*: `${EMQX_ETC_DIR}/certs/key.pem`

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

  *类型*: `non_neg_integer`

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

  *默认值*: `["tlsv1.3","tlsv1.2"]`

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


**gateway.exproto.server.ssl_options.secure_renegotiate**

  *类型*: `boolean`

  *默认值*: `true`

  SSL 参数重新协商是一种允许客户端和服务器动态重新协商 SSL 连接参数的功能。
RFC 5746 定义了一种更安全的方法。通过启用安全的重新协商，您就失去了对不安全的重新协商的支持，从而容易受到 MitM 攻击。


**gateway.exproto.server.ssl_options.log_level**

  *类型*: `enum`

  *默认值*: `notice`

  *可选值*: `emergency | alert | critical | error | warning | notice | info | debug | none | all`

  SSL 握手的日志级别。默认值是 'notice'，可以设置为 'debug' 用来调查 SSL 握手的问题。


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



### 网关客户端映射


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



### 网关监听器 - TCP


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



### 网关监听器 - SSL


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



### 网关监听器 - UDP


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



### 网关监听器 - DTLS


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
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

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


**gateway:dtls_opts.ocsp**

  *类型*: `broker:ocsp`


**gateway:dtls_opts.enable_crl_check**

  *类型*: `boolean`

  *默认值*: `false`

  是否为该监听器启用 CRL 检查。



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
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

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
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

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

  Enable TCP keepalive for MQTT connections over TCP or SSL.
The value is three comma separated numbers in the format of 'Idle,Interval,Probes'
 - Idle: The number of seconds a connection needs to be idle before the server begins to send out keep-alive probes (Linux default 7200).
 - Interval: The number of seconds between TCP keep-alive probes (Linux default 75).
 - Probes: The maximum number of TCP keep-alive probes to send before giving up and killing the connection if no response is obtained from the other end (Linux default 9).
For example "240,30,5" means: EMQX should start sending TCP keepalive probes after the connection is in idle for 240 seconds, and the probes are sent every 30 seconds until a response is received from the MQTT client, if it misses 5 consecutive responses, EMQX should close the connection.
Default: 'none'



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
如果打算使用PSK密码套件 <code>tlsv1.3</code>。应在<code>ssl.versions</code>中禁用。

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


