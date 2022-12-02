---
# 编写日期
date: 2020-03-03 10:18:36
# 作者 Github 名称
author: tigercl
# 关键字
keywords:
# 描述
description:
# 分类
category:
# 引用
ref:
---

# 配置文件

## EMQX 配置文件

EMQX 通过 `etc/` 目录下配置文件进行设置，主要配置文件包括:

| 配置文件           | 说明                       |
| ------------------ | -------------------------- |
| etc/emqx.conf      | EMQX 消息服务器配置文件    |
| etc/acl.conf       | EMQX 默认 ACL 规则配置文件 |
| etc/plugins/*.conf | EMQX 各类插件配置文件      |

## 集群配置

```
cluster.name = emqxcl
cluster.proto_dist = inet_tcp
cluster.discovery = manual
cluster.autoheal = on
cluster.autoclean = 5m
```

##### cluster.name

  *类型*: **string**

  *默认值*: `emqxcl`



集群名称。


##### cluster.proto_dist

  *类型*: **enum**

  *可选值*: `inet_tcp`, `inet6_tcp`, `inet_tls`

  *默认值*: `inet_tcp`


分布式 Erlang 集群协议类型。可选值为:

- `inet_tcp`: 使用 IPv4
- `inet6_tcp` 使用 IPv6
- `inet_tls`: 使用 TLS，需要与 `node.ssl_dist_optfile` 配置一起使用。


## 集群自动发现

EMQX 支持多种策略的节点自动发现与集群:

##### cluster.discovery

  *类型*: **enum**

  *可选值*: `manual`, `static`, `mcast`, `dns`, `etcd`, `k8s`

  *默认值*: `manual`


集群节点发现方式。可选值为:

- `manual`: 手动加入集群
- `static`: 配置静态节点。配置几个固定的节点，新节点通过连接固定节点中的某一个来加入集群。
- `mcast`: 使用 UDP 多播的方式发现节点。
- `dns`: 使用 DNS A 记录的方式发现节点。
- `etcd`: 使用 etcd 发现节点。
- `k8s`: 使用 Kubernetes 发现节点。

默认配置为手动创建集群，节点通过 `./bin/emqx_ctl join <Node>` 命令加入。


##### cluster.autoheal

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


启用或关闭集群网络分区自动恢复机制。


##### cluster.autoclean

  *类型*: **duration**

  *默认值*: `5m`



指定多久之后从集群中删除离线节点。


### 静态列表自动集群

##### cluster.static.seeds

  *类型*: **string**

  *可选值*: -

  *默认值*: `emqx1@192.168.0.100,emqx2@192.168.0.101`


当使用 static 方式集群时，指定固定的节点列表，多个节点间使用逗号 `,` 分隔。


### mcast 组播自动集群
##### cluster.mcast.addr

  *类型*: **ipaddr**

  *默认值*: `239.192.0.1`



当使用 mcast 方式集群时，指定多播地址。


##### cluster.mcast.ports

  *类型*: **string**

  *默认值*: `4369`



当使用 mcast 方式集群时，指定多播端口。如有多个端口使用逗号 `,` 分隔。


##### cluster.mcast.iface

  *类型*: **ipaddr**

  *默认值*: `0.0.0.0`



当使用 mcast 方式集群时，指定节点发现服务需要绑定到本地哪个 IP 地址。


##### cluster.mcast.ttl

  *类型*: **integer**

  *默认值*: 255



当使用 mcast 方式集群时，指定多播的 Time-To-Live 值。


##### cluster.mcast.loop

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


当使用 mcast 方式集群时，设置多播的报文是否投递到本地回环地址。


### DNS A 记录自动集群

##### cluster.dns.name

  *类型*: **string**

  *可选值*: -

  *默认值*: `mycluster.com`


当使用 dns 方式集群时，指定 DNS A 记录的名字。emqx 会通过访问这个 DNS A 记录来获取 IP 地址列表，然后拼接 `cluster.dns.app` 里指定的 APP 名得到集群中所有节点的列表。

**示例**

设置 `cluster.dns.app = emqx`，并且配置了一个 DNS: `mycluster.com`，其指向 3 个 IP 地址:

```
192.168.0.100
192.168.0.101
192.168.0.102
```

则得到集群节点列表如下：

```
emqx@192.168.0.100
emqx@192.168.0.101
emqx@192.168.0.102
```


##### cluster.dns.app

  *类型*: **string**

  *可选值*: -

  *默认值*: `emqx`


当使用 dns 方式集群时，用来与从 `cluster.dns.name` 获取的 IP 列表拼接得到节点名列表。


##### etcd 自动集群

##### cluster.etcd.server

  *类型*: **string**

  *可选值*: -

  *默认值*: `http://127.0.0.1:2379`


当使用 etcd 方式集群时，指定 etcd 服务的地址。如有多个服务使用逗号 `,` 分隔。


##### cluster.etcd.prefix

  *类型*: **string**

  *可选值*: -

  *默认值*: `emqxcl`


当使用 etcd 方式集群时，指定 etcd 路径的前缀。每个节点在 etcd 中都会创建一个路径:

```
v2/keys/<prefix>/<cluster.name>/<node.name>
```


##### cluster.etcd.node_ttl

  *类型*: **duration**

  *可选值*: -

  *默认值*: `1m`


当使用 etcd 方式集群时，指定 etcd 中节点路径的过期时间。


##### cluster.etcd.ssl.keyfile

  *类型*: **string**

  *可选值*: -

  *默认值*: `etc/certs/client-key.pem`


当使用 SSL 连接 etcd 时，指定客户端的私有 Key 文件。


##### cluster.etcd.ssl.certfile

  *类型*: **string**

  *可选值*: -

  *默认值*: `etc/certs/client.pem`


当使用 SSL 连接 etcd 时，指定 SSL 客户端的证书文件。


##### cluster.etcd.ssl.cacertfile

  *类型*: **string**

  *可选值*: -

  *默认值*: `etc/certs/ca.pem`


当使用 SSL 连接 etcd 时，指定 SSL 的 CA 证书文件。


##### Kubernetes 自动集群
##### cluster.k8s.apiserver

  *类型*: **string**

  *可选值*: -

  *默认值*: `http://10.110.111.204:8080`


当使用 k8s 方式集群时，指定 Kubernetes API Server。如有多个 Server 使用逗号 `,` 分隔。


##### cluster.k8s.service_name

  *类型*: **string**

  *可选值*: -

  *默认值*: `emqx`


当使用 k8s 方式集群时，指定 Kubernetes 中 EMQX 的服务名。


##### cluster.k8s.address_type

  *类型*: **enum**

  *可选值*: `ip`, `dns`, `hostname`

  *默认值*: `ip`


当使用 k8s 方式集群时，address_type 用来从 Kubernetes 接口的应答里获取什么形式的 Host 列表。

指定 `cluster.k8s.address_type` 为 `ip`，则将从 Kubernetes 接口中获取 emqx 服务的 IP 地址列表:

```
172.16.122.31
172.16.122.32
172.16.122.33
```

然后与 `cluster.k8s.app_name` 配置指定的 app name 拼接，得到 emqx 节点列表:

```
emqx@172.16.122.31
emqx@172.16.122.32
emqx@172.16.122.33
```


##### cluster.k8s.app_name

  *类型*: **string**

  *可选值*: -

  *默认值*: `emqx`


当使用 k8s 方式集群时，app_name 用来跟获取的 Host 列表拼接，得到节点列表。


##### cluster.k8s.suffix

  *类型*: **string**

  *可选值*: -

  *默认值*: `pod.cluster.local`


当使用 k8s 方式并且 `cluster.k8s.address_type` 指定为 dns 类型时，可设置 emqx 节点名的后缀。与 `cluster.k8s.namespace` 一起使用用以拼接得到节点名列表。


##### cluster.k8s.namespace

  *类型*: **string**

  *可选值*: -

  *默认值*: `default`


当使用 k8s 方式并且 `cluster.k8s.address_type` 指定为 dns 类型时，可设置 emqx 节点名的命名空间。与 `cluster.k8s.suffix` 一起使用用以拼接得到节点名列表。

**示例**

设置 `cluster.k8s.address_type` 为 `dns`，则将从 Kubernetes 接口中获取 emqx 服务的 dns 列表:

```
172-16-122-31
172-16-122-32
172-16-122-33
```

然后拼接上 `cluster.k8s.app_name = emqx`，`cluster.k8s.suffix = pod.cluster.local`，`cluster.k8s.namespace = default` 得到 dns 形式的 emqx 节点名列表:

```
emqx@172-16-122-31.default.pod.cluster.local
emqx@172-16-122-32.default.pod.cluster.local
emqx@172-16-122-33.default.pod.cluster.local
```


## 节点与 Cookie

```
node.name = emqx@127.0.0.1
node.cookie = emqxsecretcookie
node.data_dir = data
node.global_gc_interval = 15m
node.crash_dump = log/crash.dump
node.dist_use_interface = 0.0.0.0
node.dist_listen_min = 6369
node.dist_listen_max = 6369
node.backtrace_depth = 16
```

##### node.name

  *类型*: **string**

  *默认值*: `emqx@127.0.0.1`



节点名。格式为 `<name>@<host>`。其中 `<host>` 可以是 IP 地址，也可以是 FQDN。详见 [http://erlang.org/doc/reference_manual/distributed.html](http://erlang.org/doc/reference_manual/distributed.html)。


##### node.cookie

  *类型*: **string**

  *默认值*: `emqxsecretcookie`



分布式 Erlang 集群使用的 cookie 值。

:::tip
Erlang/OTP 平台应用多由分布的 Erlang 节点(进程)组成，每个 Erlang 节点(进程)需指配一个节点名，用于节点间通信互访。 所有互相通信的 Erlang 节点(进程)间通过一个共用的 Cookie 进行安全认证。
生产环境中务必更改 Cookie 为其他值。
:::


##### node.data_dir

  *类型*: **folder**

  *默认值*: `./data`



节点的 data 目录，用于存放 Mnesia 数据文件等。


##### node.heartbeat

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


系统调优参数，此配置将覆盖 `vm.args` 文件里的 `-heart` 参数。

启用或关闭 Erlang 运行时检测机制，并在运行时终止时自动重启。需小心使用，以免手动关闭 emqx 时被监控进程重新启动。


##### node.async_threads

  *类型*: **integer**

  *可选值*: 0 - 1024

  *默认值*: 4


系统调优参数，此配置将覆盖 `vm.args` 文件里的 `+A` 参数。

设置 Erlang 运行时异步线程池中的线程数量。详情请参见 [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html)。


##### node.process_limit

  *类型*: **integer**

  *可选值*: 1024 - 134217727

  *默认值*: 2097152


系统调优参数，此配置将覆盖 `vm.args` 文件里的 `+P` 参数。

设置 Erlang 允许的最大进程数，这将影响 emqx 节点能处理的连接数。详情请参见 [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html)。


##### node.max_ports

  *类型*: **integer**

  *可选值*: 1024 - 134217727

  *默认值*: 1048576


系统调优参数，此配置将覆盖 `vm.args` 文件里的 `+Q` 参数。

设置 Erlang 允许的最大 Ports 数量。详情请参见 [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html)。


##### node.dist_buffer_size

  *类型*: **bytesize**

  *可选值*: 1KB - 2GB

  *默认值*: `8MB`


系统调优参数，此配置将覆盖 `vm.args` 文件里的 `+zdbbl` 参数。

设置 Erlang 分布式通信使用的最大缓存大小。详情请参见 [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html)。


##### node.max_ets_tables

  *类型*: **integer**

  *默认值*: 262144



系统调优参数，此配置将覆盖 `vm.args` 文件里的 `+e` 参数。

设置 Erlang 运行时允许的最大 ETS 表数量。详情请参见 [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html)。


##### node.global_gc_interval

  *类型*: **duration**

  *默认值*: `15m`



系统调优参数，设置 Erlang 运行多久强制进行一次全局垃圾回收。


##### node.fullsweep_after

  *类型*: **integer**

  *可选值*: 0 - 65535

  *默认值*: 1000


系统调优参数，此配置将覆盖 `vm.args` 文件里的 `-env ERL_FULLSWEEP_AFTER` 参数。

设置 Erlang 运行时多少次 generational GC 之后才进行一次 fullsweep GC。详情请参见 [http://erlang.org/doc/man/erlang.html#spawn_opt-4](http://erlang.org/doc/man/erlang.html#spawn_opt-4)。


##### node.crash_dump

  *类型*: **string**

  *默认值*: `log/crash.dump`



设置 Erlang crash_dump 文件的存储路径和文件名。


##### node.ssl_dist_optfile

  *类型*: **string**

  *默认值*: `etc/ssl_dist.conf`



此配置将覆盖 `vm.args` 文件里的 `-ssl_dist_optfile` 参数。

如使用 SSL 方式建立 emqx 集群，需指定 SSL 分布式协议的配置文件。需要与 `cluster.proto_dist = inet_tls` 一起使用。


##### node.dist_net_ticktime

  *类型*: **integer**

  *默认值*: 120



系统调优参数，此配置将覆盖 `vm.args` 文件里的 `-kernel net_ticktime` 参数。

当一个节点持续无响应多久之后，认为其已经宕机并断开连接。详情请参见 [http://www.erlang.org/doc/man/kernel_app.html#net_ticktime](http://www.erlang.org/doc/man/kernel_app.html#net_ticktime)。

##### node.dist_use_interface

  *类型*: **ipaddr**

  *默认值*: 0.0.0.0



节点间通讯网卡，默认使用 `0.0.0.0` 指定监听所有的网卡，或在指定需要监听网卡的 IP。


##### node.dist_listen_min

  *类型*: **integer**

  *可选值*: 1024 - 65535

  *默认值*: 6369


与 `node.dist_listen_max` 一起设定一个 TCP 端口段，此端口段用于分配给分布式 Erlang，作为分布式通道的监听端口。注意如果在节点之间设置了防火墙，需要将此端口段放进防火墙的端口白名单里。


##### node.dist_listen_max

  *类型*: **integer**

  *可选值*: 1024 - 65535

  *默认值*: 6369


与 `node.dist_listen_min` 一起设定一个 TCP 端口段，此端口段用于分配给分布式 Erlang，作为分布式通道的监听端口。注意如果在节点之间设置了防火墙，需要将此端口段放进防火墙的端口白名单里。


## Broker 配置

```
broker.sys_interval = 1m
broker.sys_heartbeat = 30s
broker.session_locking_strategy = quorum
broker.shared_subscription_strategy = random
#broker.sample_group.shared_subscription_strategy = local
broker.shared_dispatch_ack_enabled = false
broker.route_batch_clean = off
# broker.perf.route_lock_type = key
# broker.perf.trie_compaction = true
```

##### broker.sys_interval

  *类型*: **duration**

  *默认值*: `1m`



设置系统主题 (`$SYS`) 消息的发布间隔。


##### broker.sys_heartbeat

  *类型*: **duration**

  *默认值*: `30s`



设置系统心跳消息的发布间隔。系统心跳消息包括下面两个主题：

- "$SYS/brokers/\<node>/uptime"
- "$SYS/brokers/\<node>/datetime"


##### broker.enable_session_registry

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


启用或关闭全局会话注册。


##### broker.session_locking_strategy

  *类型*: **enum**

  *可选值*: `local`, `leader`, `quorum`, `all`

  *默认值*: `quorum`


设置会话集群锁的类型。会话的集群锁用来防止同一个客户端在多个不同节点上创建多个会话，常见于客户端频繁切换节点登录的情况。


##### broker.shared_subscription_strategy

  *类型*: **enum**

  *可选值*: `hash_clientid`, `hash_topic`, `local`, `random`, `round_robin`, `sticky`,

  *默认值*: `random`


设置共享订阅的分发策略。可选值为:

- **hash_clientid**: 按照发布者 ClientID 的哈希值
- **hash_topic**: 按照源消息主题的哈希值
- **local**: 优先选择和发布者在同一各节点的共享订阅者来派发消息，否则进行随机派发
- **random**: 在所有订阅者中随机选择
- **round_robin**: 按照一个固定的顺序选择下一个订阅者
- **sticky**: 首次分发时随机选择一个订阅者，后续消息一直发往这一个订阅者直到该订阅者离线或该发布者重连。


##### broker.sample_group.shared_subscription_strategy

  *类型*: **enum**

  *可选值*: `hash_clientid`, `hash_topic`, `local`, `random`, `round_robin`, `sticky`,

  *默认值*: -


重载共享订阅组名为 `sample_group` 的派发策略。不配置则以 `broker.shared_subscription_strategy` 为准。

其中 `sample_group` 可以配置为任何组名称。

其可选策略与 `broker.shared_subscription_strategy` 一致。


##### broker.shared_dispatch_ack_enabled

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


开启或关闭共享订阅对于 qos1/qos2 消息的 ACK 检查功能。开启后，如果投递到某个订阅者但收不到ACK，将尝试投递给订阅组里的下一个订阅者。


##### broker.route_batch_clean

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


开启或关闭批量清理路由信息。批量清理路由可用在短时间内大量客户端掉线的情况，以提高清理效率。


##### broker.perf.route_lock_type = key

  *类型*: **enum**

  *可选值*: `key`, `tab`, `global`

  *默认值*: `key`



选择在数据库中为通配符订阅更新路由信息时锁的粒度。

- `key` (默认值) 为每个前缀拿一次数据库锁。
- `tab` 表锁
- `global` 全局锁

对于较大集群，(如7个node或以上），尤其是node之间网络延迟大的，推荐是用`tab` 和 `global`。
注意：是需要重启整个集群来使得更新生效。


##### broker.perf.trie_compaction

{% emqxee %}

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


{% endemqxee %}

{% emqxce %}

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


{% endemqxce %}


设置为 `true` 时，对通配符订阅表进行压缩。
压缩可优化写操作，降低高并发量的订阅请求响应时间，内存使用量也只有非压缩时的一半。
非压缩优化读操作，适用于发布主题层数较多的场景。

注意: 将该配置从 `fase` 改成 `true` 时，集群中的节点可依次重启来使配置生效。
从 `true` 改为 `false` 时，需要将集群中所有的节点重启，否则会发生有些消息
无法被路由的情况。


## RPC 配置

```
rpc.mode = async
rpc.async_batch_size = 256
rpc.port_discovery = stateless
#rpc.tcp_server_ip = 0.0.0.0
#rpc.tcp_server_port = 5369
#rpc.tcp_client_num = 1
rpc.connect_timeout = 5s
rpc.send_timeout = 5s
rpc.authentication_timeout = 5s
rpc.call_receive_timeout = 15s
rpc.socket_keepalive_idle = 900s
rpc.socket_keepalive_interval = 75s
rpc.socket_keepalive_count = 9
rpc.socket_sndbuf = 1MB
rpc.socket_recbuf = 1MB
rpc.socket_buffer = 1MB
```

##### rpc.mode

  *类型*: **enum**

  *可选值*: `sync`, `async`

  *默认值*: `async`


RPC 模式。可选同步或异步模式。


##### rpc.async_batch_size

  *类型*: **integer**

  *默认值*: 256



异步模式下最大的批量发送消息数。注意此配置在同步模式下不起作用。


##### rpc.port_discovery

| Type | Optional Value Default |
| ---- | ---------------------- |
| enum | `manual`, `stateless`  |

`manual`: 手动指定服务器客户端的端口号 `tcp_server_port` and `tcp_client_port`.
`stateless`: discover ports in a stateless manner. If node name is `emqx<N>@127.0.0.1`, where the `<N>` is an integer,
then the listening port will be `5370 + <N>`

Default is `manual` when started from docker (environment variable override from docker-entrypoint)
otherwise `stateless`.


##### rpc.tcp_server_ip

  *类型*: **ipaddr**

  *可选值*: [0-255].[0-255].[0-255].[0-255]

  *默认值*: 0.0.0.0


设置 RPC 本地服务使用的监听网卡。默认使用 `0.0.0.0` 指定监听所有的网卡，或在指定需要监听网卡的 IP。


##### rpc.tcp_server_port

  *类型*: **integer**

  *可选值*: 1024 - 65535

  *默认值*: 5369


设置 RPC 本地服务使用的监听 port。
注意，该配置仅在 `rpc.port_discovery` 设置成 `manual` 时有效


##### rpc.tcp_client_num

  *类型*: **integer**

  *可选值*: 1 - 256

  *默认值*: CPU 核心数 / 2


设置由本节点发起，通往每个远程节点的 RPC 通信通道数量。设置为 1 可保证消息顺序。保持默认值（CPU 核心数的一半）可提高 RPC 的吞吐能力。


##### rpc.connect_timeout

  *类型*: **duration**

  *默认值*: `5s`



建立 RPC 连接超时时间。建立连接时若远程节点无响应，多久之后放弃尝试。


##### rpc.send_timeout

  *类型*: **duration**

  *默认值*: `5s`



发送超时时间。发送消息多久之后放弃。


##### rpc.authentication_timeout

  *类型*: **duration**

  *默认值*: `5s`



RPC 认证超时时间。尝试认证若远程节点无响应，多久之后放弃。


##### rpc.call_receive_timeout

  *类型*: **duration**

  *默认值*: `15s`



RPC 同步模式的超时时间。RPC 同步调用若收不到回复，用多久之后放弃。


##### rpc.socket_keepalive_idle

  *类型*: **duration**

  *默认值*: `900s`



在最近一次数据包发送多久之后，发送 keepalive 探测报文。


##### rpc.socket_keepalive_interval

  *类型*: **duration**

  *默认值*: `75s`



发送 keepalive 探测报文的间隔。


##### rpc.socket_keepalive_count

  *类型*: **integer**

  *默认值*: 9



连续多少次 keepalive 探测报文都收不到回复的情况下，认为 RPC 连接已丢失。


##### rpc.socket_sndbuf

  *类型*: **bytesize**

  *默认值*: `1MB`



TCP 调优参数。TCP 发送缓冲区大小。


##### rpc.socket_recbuf

  *类型*: **bytesize**

  *默认值*: `1MB`



TCP 调优参数。TCP 接收缓冲区大小。


##### rpc.socket_buffer

  *类型*: **bytesize**

  *默认值*: `1MB`



TCP 调优参数。用户态的 Socket 缓冲区大小。



## 认证授权配置

```
allow_anonymous = false
acl_nomatch = allow
acl_file = etc/acl.conf
enable_acl_cache = on
acl_cache_max_size = 32
acl_cache_ttl = 1m
acl_deny_action = ignore
```

##### allow_anonymous

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


是否允许匿名用户登录系统。

注：生产环境建议关闭此选项。


##### acl_nomatch

  *类型*: **enum**

  *可选值*: `allow`, `deny`

  *默认值*: `allow`


EMQX 支持基于内置 ACL 以及 MySQL、 PostgreSQL 等插件的 ACL，多个 ACL 插件未命中时，允许或者拒绝 发布/订阅 操作。


##### acl_file

  *类型*: **string**

  *默认值*: `etc/acl.conf`



默认 ACL 文件的路径。


##### enable_acl_cache

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


是否启用 ACL 缓存。


##### acl_cache_max_size

  *类型*: **integer**

  *默认值*: 32



ACL 规则最大缓存条数。


##### acl_cache_ttl

  *类型*: **duration**

  *默认值*: `1m`



ACL 规则最大缓存时间。


##### acl_deny_action

  *类型*: **enum**

  *可选值*: `ignore`, `disconnect`

  *默认值*: `ignore`


ACL 检查失败后，执行的操作。

- `ignore`：不做任何操作。
- `disconnect`：断开连接。


## MQTT 配置

```
flapping_detect_policy = 30, 1m, 5m
mqtt.max_packet_size = 1MB
mqtt.max_clientid_len = 65535
mqtt.max_topic_levels = 128
mqtt.max_qos_allowed = 2
mqtt.max_topic_alias = 65535
mqtt.retain_available = true
mqtt.wildcard_subscription = true
mqtt.shared_subscription = true
mqtt.exclusive_subscription = false
mqtt.ignore_loop_deliver = false
mqtt.strict_mode = false
```

##### flapping_detect_policy

  *类型*: **string**

  *默认值*: `30, 1m, 5m`



指定 `Flapping` 检查策略。

格式：`<threshold>,<duration>,<banned>`。

例如，`30, 1m, 5m`，它表示如果客户端在 1 分钟内断开连接 30 次，那么在后续 5 分钟内禁止登录。


##### mqtt.max_packet_size

  *类型*: **bytesize**

  *默认值*: `1MB`



允许的 MQTT 报文最大长度。


##### mqtt.max_clientid_len

  *类型*: **integer**

  *默认值*: 65535



允许的 Client ID 串的最大长度。


##### mqtt.max_topic_levels

  *类型*: **integer**

  *默认值*: 128



允许客户端订阅主题的最大层级。0 表示不限制。

:::
Topic层级过多可能导致订阅时的性能问题。
:::


##### mqtt.max_qos_allowed

  *类型*: **enum**

  *可选值*: `0`, `1`, `2`

  *默认值*: `2`


允许客户端发布的最大 QoS 等级。


##### mqtt.max_topic_alias

  *类型*: **integer**

  *默认值*: 65535



允许最大的主题别名数。0 表示不支持主题别名。


##### mqtt.retain_available

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


是否支持 Retain 消息。


##### mqtt.wildcard_subscription

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


是否支持订阅通配主题。


##### mqtt.shared_subscription

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


是否支持共享订阅。


##### mqtt.ignore_loop_deliver

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否忽略自己发送的消息。如果忽略，则表明 EMQX 不会向消息的发送端投递此消息。


##### mqtt.strict_mode

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否开启严格检查模式。严格检查模式会更细致的检查 MQTT 报文的正确性。



## MQTT 监听器说明

EMQX支持 MQTT、MQTT/SSL、MQTT/WS 协议，可通过 `listener.{tcp}|{ssl}|{ws}|{wss}.*` 设置端口、最大允许连接数等参数。

EMQX 默认开启的 TCP 服务端口包括:

| 端口  | 协议说明                     |
| ----- | ---------------------------- |
| 1883  | MQTT/TCP 端口                |
| 8883  | MQTT/TCP SSL 端口            |
| 8083  | MQTT/WebSocket 端口          |
| 8084  | MQTT/WebSocket with SSL 端口 |
| 11883 | MQTT/TCP 端口                |

## MQTT/TCP 监听器 - 1883

```
listener.tcp.external = 0.0.0.0:1883
listener.tcp.external.acceptors = 8
listener.tcp.external.max_connections = 1024000
listener.tcp.external.max_conn_rate = 1000
listener.tcp.external.active_n = 100
listener.tcp.external.zone = external
listener.tcp.external.access.1 = allow all
listener.tcp.external.backlog = 1024
listener.tcp.external.send_timeout = 15s
listener.tcp.external.send_timeout_close = on
listener.tcp.external.nodelay = true
listener.tcp.external.reuseaddr = true
```

##### listener.tcp.external

  *类型*: **string**

  *默认值*: `0.0.0.0:1883`



配置名称为 `external` 的 MQTT/TCP 监听器的监听地址。

`1883`：表监听 IPv4 的 `0.0.0.0:1883`。
`127.0.0.1:1883`：表监听地址为 `127.0.0.1` 网卡上的 `1883` 端口。
`::1:1883`：表监听 IPv6 地址为 `::1` 网卡上的 `1883` 端口。


##### listener.tcp.external.acceptors

  *类型*: **integer**

  *默认值*: 8



监听器的接收池大小。


##### listener.tcp.external.max_connections

  *类型*: **integer**

  *默认值*: 1024000



监听器允许的最大并发连接数量。


##### listener.tcp.external.max_conn_rate

  *类型*: **integer**

  *默认值*: 1000



监听器允许的最大接入速率。单位：个/秒


##### listener.tcp.external.active_n

  *类型*: **integer**

  *默认值*: 100



监听器持续接收 TCP 报文的次数。


##### listener.tcp.external.zone

  *类型*: **string**

  *默认值*: `external`



监听器所属的配置域 (Zone)。


##### listener.tcp.external.rate_limit

  *类型*: **string**

  *默认值*: -



监听器的速率限制。格式为 `<limit>,<duration>`。

`100KB,10s`：表 *限制 10 秒内的流入字节数不超过 100 KB*。


##### listener.tcp.external.access.1

  *类型*: **string**

  *默认值*: `allow all`



监听器的 ACL 规则列表。它用于设置连接层的白/黑名单。

`allow all`：表允许所有的 TCP 连接接入。
`allow 192.168.0.0/24`：表允许网络地址为 `192.168.0.0/24` 的 TCP 连接接入。

同时，该配置可配置多条规则：
```
listener.tcp.external.access.1 = deny 192.168.0.1
listener.tcp.external.access.2 = allow all
```

它表示，除 `192.168.0.1` 外的 TCP 连接都允许接入。


##### listener.tcp.external.proxy_protocol

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: -


监听器是否开启 `Proxy Protocol` 的支持。

如果 EMQX 集群部署在 HAProxy 或 Nginx 后，且需要拿到客户端真实的源 IP 地址与端口，则需打开此配置。

`Proxy Protcol` 参考: [https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol)。


##### listener.tcp.external.proxy_protocol_timeout

  *类型*: **duration**

  *默认值*: -



设置 Proxy Protocol 解析的超时时间。如果该时间内没收到 Proxy Protocol 的报文，EMQX 会关闭其连接。


##### listener.tcp.external.peer_cert_as_username

  *类型*: **enum**

  *可选值*: `cn`, `dn`, `crt`, `pem`, `md5`

  *默认值*: `cn`


使用客户端证书来覆盖 Username 字段的值。其可选值为：
- cn：客户端证书的 Common Name 字段值
- dn：客户端证书的 Subject Name 字段值
- crt：DER 格式编码的客户端证书二进制
- pem：基于 DER 格式上的 base64 编码后的字符串
- md5：DER 格式证书的 MD5 哈希值

注意：在 TCP 的监听器下，该配置仅在负载均衡服务器终结 SSL 的部署情况下可以用；且负载均衡服务器需要配置
Proxy Protocol 将证书域的内容给发送至 EMQX。例如 HAProxy 的配置可参考
[send-proxy-v2-ssl](http://cbonte.github.io/haproxy-dconv/1.7/configuration.html#5.2-send-proxy-v2-ssl)


##### listener.tcp.external.peer_cert_as_clientid

  *类型*: **enum**

  *可选值*: `cn`, `dn`, `crt`, `pem`, `md5`

  *默认值*: `cn`


使用客户端证书来覆盖 ClientID 字段的值。其可选值的含义同上。


##### listener.tcp.external.backlog

  *类型*: **integer**

  *默认值*: 1024



TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。


##### listener.tcp.external.send_timeout

  *类型*: **duration**

  *默认值*: `15s`



TCP 报文发送超时时间。


##### listener.tcp.external.send_timeout_close

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


TCP 报文发送超时后，是否关闭该连接。


##### listener.tcp.external.recbuf

  *类型*: **bytesize**

  *默认值*: -



TCP 接收缓存区大小（操作系统内核级参数）

参见：http://erlang.org/doc/man/inet.html


##### listener.tcp.external.sndbuf

  *类型*: **bytesize**

  *默认值*: -



TCP 发送缓存区大小（操作系统内核级参数）。

参见：[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html)。


##### listener.tcp.external.buffer

  *类型*: **bytesize**

  *默认值*: -



TCP 缓冲区大小 (用户级)。

该值建议大于等于 `sndbuff` 和 `recbuff` 的最大值，以避免一些性能问题。在不配置的情况下，它默认等于 sndbuff 和 recbuff 的最大值。

参见：[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html)。


##### listener.tcp.external.tune_buffer

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: -


如果打开此配置，请设置该值等于 `sndbuff` 与 `recbuff` 的最大值。


##### listener.tcp.external.nodelay

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


即 `TCP_NODELAY` 参数。开启该选项即允许小的 TCP 数据报文将会立即发送。


##### listener.tcp.external.reuseaddr

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


即 `SO_REUSEADDR` 参数。开启该选项即允许本地重用端口，无需等待 `TIME_WAIT` 状态结束。



## MQTT/SSL 监听器 - 8883

```
listener.ssl.external = 8883
listener.ssl.external.acceptors = 16
listener.ssl.external.max_connections = 102400
listener.ssl.external.max_conn_rate = 500
listener.ssl.external.active_n = 100
listener.ssl.external.zone = external
listener.ssl.external.access.1 = allow all
listener.ssl.external.handshake_timeout = 15s
listener.ssl.external.keyfile = etc/certs/key.pem
listener.ssl.external.certfile = etc/certs/cert.pem
listener.ssl.external.cacertfile = etc/certs/cacert.pem
listener.ssl.external.ciphers = TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256,TLS_CHACHA20_POLY1305_SHA256,TLS_AES_128_CCM_SHA256,TLS_AES_128_CCM_8_SHA256,ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA
#listener.ssl.external.psk_ciphers = RSA-PSK-AES256-GCM-SHA384,RSA-PSK-AES256-CBC-SHA384,RSA-PSK-AES128-GCM-SHA256,RSA-PSK-AES128-CBC-SHA256,RSA-PSK-AES256-CBC-SHA,RSA-PSK-AES128-CBC-SHA
#
listener.ssl.external.recbuf = 4KB
listener.ssl.external.sndbuf = 4KB
listener.ssl.external.reuseaddr = true
```

##### listener.ssl.external

  *类型*: **string**

  *默认值*: `0.0.0.0:8883`



配置名称为 `external` 的 SSL 监听器。


##### listener.ssl.external.acceptors

  *类型*: **integer**

  *默认值*: 16



监听器的接收池大小。


##### listener.ssl.external.max_connections

  *类型*: **integer**

  *默认值*: 102400



监听器允许的最大并发连接数量。


##### listener.ssl.external.max_conn_rate

  *类型*: **integer**

  *默认值*: 500



监听器允许的最大接入速率。单位：个/秒。


##### listener.ssl.external.active_n

  *类型*: **integer**

  *默认值*: 100



监听器持续接收 TCP 报文的次数。


##### listener.ssl.external.zone

  *类型*: **string**

  *默认值*: `external`



监听器所属的配置组 (Zone)。


##### listener.ssl.external.access.1

  *类型*: **string**

  *默认值*: `allow all`



监听器的 ACL 规则列表。它用于设置连接层的白/黑名单。

例如:

`allow all`：表允许所有的 TCP 连接接入。
`allow 192.168.0.0/24`：表允许网络地址为 `192.168.0.0/24` 的 TCP 连接接入。

同时，该配置可配置多条规则:

```
listener.ssl.external.access.1 = deny 192.168.0.1
listener.ssl.external.access.2 = allow all
```


##### listener.ssl.external.rate_limit

  *类型*: **string**

  *默认值*: -



监听器的速率限制。格式为 `<limit>,<duration>`。


##### listener.ssl.external.proxy_protocol

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: -


监听器是否开启 `Proxy Protocol` 的支持。

如果 EMQX 集群部署在 HAProxy 或 Nginx 后，且需要拿到客户端真实的源 IP 地址与端口，则需打开此配置。

`Proxy Protcol` 参考: [https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol)。


##### listener.ssl.external.proxy_protocol_timeout

  *类型*: **duration**

  *默认值*: -



设置 Proxy Protocol 解析的超时时间。如果该时间内没收到 Proxy Protocol 的报文，EMQX 会关闭其连接。


##### listener.ssl.external.tls_versions

  *类型*: **string**

  *默认值*: `tlsv1.3,tlsv1.2,tlsv1.1,tlsv1`



指定服务端支持的 SSL 的版本列表。详情请参见 [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html)。


##### listener.ssl.external.handshake_timeout

  *类型*: **duration**

  *默认值*: `15s`



指定 SSL 握手过程的超时时间。


##### listener.ssl.external.depth

  *类型*: **number**

  *默认值*: `10`



证书链中非自签发的中间证书的最大数量。如果该值为 0 则表示，对端证书必须是根 CA 直接授信的。


##### listener.ssl.external.key_password

  *类型*: **string**

  *默认值*: -



证书密钥文件的密码。如果你的证书密钥设置了密码，则需要配置该选项。


##### listener.ssl.external.keyfile

  *类型*: **string**

  *默认值*: `etc/certs/key.pem`



指定 SSL 的私钥文件 (PEM)。


##### listener.ssl.external.certfile

  *类型*: **string**

  *默认值*: `etc/certs/cert.pem`



指定 SSL 的证书文件 (PEM)。


##### listener.ssl.external.cacertfile

  *类型*: **string**

  *默认值*: `etc/certs/cacert.pem`



指定 SSL 的 CA 证书文件 (PEM)。该文件应包含发布服务器证书的所有中间CA证书以及根证书。
该文件还应包含所有受信CA的证书用以用于验证客户端的证书。


##### listener.ssl.external.dhfile

  *类型*: **string**

  *默认值*: `etc/certs/dh-params.pem`



若使用 Ephemeral Diffie-Helman 算法，指定算法使用的 key 文件。


##### listener.ssl.external.verify

  *类型*: **enum**

  *可选值*: `verify_peer`, `verify_none`

  *默认值*: `verify_peer`


指定握手过程中是否校验客户端。


##### listener.ssl.external.fail_if_no_peer_cert

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


SSL 握手过程中若客户端没有证书，是否让握手失败。


##### listener.ssl.external.ciphers

  *类型*: **string**

  *默认值*: `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA`



指定服务端支持的密码套件。


##### listener.ssl.external.psk_ciphers

  *类型*: **string**

  *默认值*: `PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA`



若使用 PSK 算法，指定服务端支持的 PSK Cipher 列表。注意 'listener.ssl.external.ciphers' 和 'listener.ssl.external.psk_ciphers' 只能配置一个。


##### listener.ssl.external.secure_renegotiate

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


指定在客户端不遵循 RFC 5746 的情况下，是否拒绝 renegotiation 请求。


##### listener.ssl.external.reuse_sessions

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


指定是否支持 SSL session 重用。详情见 [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html)。


##### listener.ssl.external.honor_cipher_order

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


指定是否使用服务端的偏好设置选择 Ciphers。


##### listener.ssl.external.peer_cert_as_username

  *类型*: **enum**

  *可选值*: `cn`, `dn`, `crt`, `pem`, `md5`

  *默认值*: `cn`


使用客户端证书来覆盖 Username 字段的值。其可选值为：
- cn：客户端证书的 Common Name 字段值
- dn：客户端证书的 Subject Name 字段值
- crt：DER 格式编码的客户端证书二进制
- pem：基于 DER 格式上的 base64 编码后的字符串
- md5：DER 格式证书的 MD5 哈希值

注意 `listener.ssl.external.verify` 应当设置为 `verify_peer`。


##### listener.ssl.external.peer_cert_as_clientid

  *类型*: **enum**

  *可选值*: `cn`, `dn`, `crt`, `pem`, `md5`

  *默认值*: `cn`


使用客户端证书来覆盖 ClientID 字段的值。其可选值的含义同上。

注意 `listener.ssl.external.verify` 应当设置为 `verify_peer`。


##### listener.ssl.external.backlog

  *类型*: **integer**

  *默认值*: 1024



TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。


##### listener.ssl.external.send_timeout

  *类型*: **duration**

  *默认值*: `15s`



TCP 报文发送超时时间。


##### listener.ssl.external.send_timeout_close

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


TCP 报文发送超时后，是否关闭该连接。


##### listener.ssl.external.recbuf

  *类型*: **bytesize**

  *默认值*: -



TCP 接收缓存区大小（操作系统内核级参数）。

参见：[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html)。


##### listener.ssl.external.sndbuf

  *类型*: **bytesize**

  *默认值*: -



TCP 发送缓存区大小（操作系统内核级参数）。

参见：[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html)。


##### listener.ssl.external.buffer

  *类型*: **bytesize**

  *默认值*: -



TCP 缓冲区大小 (用户级)。

该值建议大于等于 `sndbuff` 和 `recbuff` 的最大值，以避免一些性能问题。在不配置的情况下，它默认等于 sndbuff 和 recbuff 的最大值。

参见：[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html)。


##### listener.ssl.external.tune_buffer

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: -


如果打开此配置，请设置该值等于 `sndbuff` 与 `recbuff` 的最大值。


##### listener.ssl.external.nodelay

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


即 `TCP_NODELAY` 参数。开启该选项即表示禁用 Nagle 算法，小包将被立即发送。


##### listener.ssl.external.reuseaddr

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


即 `SO_REUSEADDR` 参数。开启该选项即允许本地重用端口，无需等待 `TIME_WAIT` 状态结束。


## MQTT/WebSocket 监听器 - 8083

```
listener.ws.external = 8083
listener.ws.external.mqtt_path = /mqtt
listener.ws.external.acceptors = 4
listener.ws.external.max_connections = 102400
listener.ws.external.max_conn_rate = 1000
listener.ws.external.active_n = 100
listener.ws.external.zone = external
listener.ws.external.access.1 = allow all
listener.ws.external.backlog = 1024
listener.ws.external.send_timeout = 15s
listener.ws.external.send_timeout_close = on
listener.ws.external.nodelay = true
listener.ws.external.mqtt_piggyback = multiple
listener.ws.external.check_origin_enable = false
listener.ws.external.allow_origin_absence = true
listener.ws.external.check_origins = http://localhost:18083, http://127.0.0.1:18083
```

##### listener.ws.external

  *类型*: **string**

  *默认值*: `8083`



配置名称为 `external` 的 MQTT/WS 监听器的监听地址。

`8083`：表监听 IPv4 的 `0.0.0.0:8083`。
`127.0.0.1:8083`：表监听地址为 `127.0.0.1` 网卡上的 `8083` 端口。
`::1:8083`：表监听 IPv6 地址为 `::1` 网卡上的 `8083` 端口。


##### listener.ws.external.mqtt_path

  *类型*: **string**

  *默认值*: `/mqtt`



WebSocket 的 MQTT 协议路径。因此 EMQX 的 WebSocket 的地址是： `ws://{ip}:{port}/mqtt`。


##### listener.ws.external.acceptors

  *类型*: **integer**

  *默认值*: 4



监听器的接收池大小。


##### listener.ws.external.max_connections

  *类型*: **integer**

  *默认值*: 102400



监听器允许的最大并发连接数量。


##### listener.ws.external.max_conn_rate

  *类型*: **integer**

  *默认值*: 1000



监听器允许的最大接入速率。单位：个/秒


##### listener.ws.external.active_n

  *类型*: **integer**

  *默认值*: 100



监听器持续接收 TCP 报文的次数。


##### listener.ws.external.rate_limit

  *类型*: **string**

  *默认值*: `100KB,10s`



监听器的速率限制。格式为 `<limit>,<duration>`。

`100KB,10s`：表 *限制 10 秒内的流入字节数不超过 100 KB*。


##### listener.ws.external.zone

  *类型*: **string**

  *默认值*: `external`



监听器所属的配置域 (Zone)。


##### listener.ws.external.access.1

  *类型*: **string**

  *默认值*: `allow all`



监听器的 ACL 规则列表。它用于设置连接层的白/黑名单。


##### listener.ws.external.fail_if_no_subprotocol

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


如果设置为 true，则服务器将在客户端没有携带 Sec-WebSocket-Protocol 字段时返回错误。**微信小程序需关闭该验证**。


##### listener.ws.external.supported_protocols

  *类型*: **string**

  *默认值*: `mqtt, mqtt-v3, mqtt-v3.1.1, mqtt-v5`



指定支持的子协议，子协议之间以逗号分隔。


##### listener.ws.external.proxy_address_header

  *类型*: **string**

  *可选值*: `X-Forwarded-For`

  *默认值*: -


如果 EMQX 集群部署在 HAProxy 或 Nginx 后，则可打开该配置获取客户端真实的 IP 地址。


##### listener.ws.external.proxy_port_header

  *类型*: **string**

  *可选值*: `X-Forwarded-Port`

  *默认值*: -


如果 EMQX 集群部署在 HAProxy 或 Nginx 后，则可打开该配置获取客户端真实的端口。


##### listener.ws.external.proxy_protocol

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: -


监听器是否开启 `Proxy Protocol` 的支持。

如果 EMQX 集群部署在 HAProxy 或 Nginx 后，且需要拿到客户端真实的源 IP 地址与端口，则需打开此配置。

`Proxy Protcol` 参考: [https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol)。


##### listener.ws.external.proxy_protocol_timeout

  *类型*: **duration**

  *默认值*: -



设置 Proxy Protocol 解析的超时时间。如果该时间内没收到 Proxy Protocol 的报文，EMQX 会关闭其连接。


##### listener.ws.external.backlog

  *类型*: **integer**

  *默认值*: 1024



TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。


##### listener.ws.external.send_timeout

  *类型*: **duration**

  *默认值*: `15s`



TCP 报文发送超时时间。


##### listener.ws.external.send_timeout_close

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


TCP 报文发送超时后，是否关闭该连接。


##### listener.ws.external.recbuf

  *类型*: **bytesize**

  *默认值*: -



TCP 接收缓存区大小（操作系统内核级参数）


##### listener.ws.external.sndbuf

  *类型*: **bytesize**

  *默认值*: -



TCP 发送缓存区大小（操作系统内核级参数）


##### listener.ws.external.buffer

  *类型*: **bytesize**

  *默认值*: -



TCP 缓冲区大小 (用户级)。


##### listener.ws.external.tune_buffer

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: -


如果打开此配置，请设置该值等于 `sndbuff` 与 `recbuff` 的最大值。


##### listener.ws.external.nodelay

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


即 `TCP_NODELAY` 参数。开启该选项即允许小的 TCP 数据报文将会立即发送。


##### listener.ws.external.compress

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: -


是否压缩 WebSocket 消息。压缩的实现依赖 [zlib](http://www.zlib.net)。

`defalte_opts` 下的配置项，都属于压缩相关的参数配置，如无必要请不需要修改它。


##### listener.ws.external.deflate_opts.level

  *类型*: **enum**

  *可选值*: `none`, `default`, `best_compression`, `best_speed`

  *默认值*: -


压缩等级。


##### listener.ws.external.deflate_opts.mem_level

  *类型*: **integer**

  *可选值*: 1 - 9

  *默认值*: -


压缩参数。内存使用限制等级，配置可开辟多少内存来参与压缩过程。

`1`：最少的内存，但会降低压缩率。
`9`：最多的内存，会提高计算速度和压缩率。

不配置，则默认为 `8`。


##### listener.ws.external.deflate_opts.strategy

  *类型*: **enum**

  *可选值*: `default`, `filtered`, `huffman_only`, `rle`

  *默认值*: -


压缩策略，用于调优压缩率：

- `default`：针对普通数据。
- `filtered`：由过滤器或预测器产生的数据，适用于分布随机性强的内容。
- `huffman_only`：强制使用 Huffman 算法。优于 `filtered`。
- `rle`：将匹配距离限制为 1 (Run-Lenght Encoding)，比 `huffman_only` 要快，但主要用于 PNG 图片。

这些策略仅影响压缩率，不会对正确性带来任何影响。


##### listener.ws.external.deflate_opts.server_context_takeover

  *类型*: **enum**

  *可选值*: `takeover`, `no_takeover`

  *默认值*: -


是否允许服务端的压缩上下文在帧之间传递。


##### listener.ws.external.deflate_opts.client_context_takeover

  *类型*: **enum**

  *可选值*: `takeover`, `no_takeover`

  *默认值*: -


是否允许客户端的压缩上下文在帧之间传递。


##### listener.ws.external.deflate_opts.server_max_window_bits

  *类型*: **integer**

  *可选值*: 8 - 15

  *默认值*: -


服务端最大窗口值。设置一个较大的值会有更好的压缩率，但会额外的消耗内存。


##### listener.ws.external.deflate_opts.client_max_window_bits

  *类型*: **integer**

  *可选值*: 8 - 15

  *默认值*: -


客户端最大窗口值。设置一个较大的值会有更好的压缩率，但会额外的消耗内存。


##### listener.ws.external.idle_timeout

  *类型*: **duration**

  *默认值*: -



TCP 连接建立后的发呆时间，如果这段时间内未收到任何报文，则会关闭该连接。


##### listener.ws.external.max_frame_size

  *类型*: **integer**

  *默认值*: -



允许的单个 MQTT 报文长度的最大值。


## MQTT/WebSocket with SSL 监听器 - 8084

```
listener.wss.external = 8084
listener.wss.external.mqtt_path = /mqtt
listener.wss.external.acceptors = 4
listener.wss.external.max_connections = 102400
listener.wss.external.max_conn_rate = 1000
listener.wss.external.active_n = 100
listener.wss.external.zone = external
listener.wss.external.access.1 = allow all
listener.wss.external.keyfile = etc/certs/key.pem
listener.wss.external.certfile = etc/certs/cert.pem
listener.wss.external.ciphers = TLS_AES_256_GCM_SHA384,TLS_AES_128_GCM_SHA256,TLS_CHACHA20_POLY1305_SHA256,TLS_AES_128_CCM_SHA256,TLS_AES_128_CCM_8_SHA256,ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA
listener.wss.external.backlog = 1024
listener.wss.external.send_timeout = 15s
listener.wss.external.send_timeout_close = on
listener.wss.external.mqtt_piggyback = multiple
listener.wss.external.check_origin_enable = false
listener.wss.external.allow_origin_absence = true
listener.wss.external.check_origins = https://localhost:8084, https://127.0.0.1:8084
```

##### listener.wss.external

  *类型*: **string**

  *默认值*: `0.0.0.0:8084`



配置名称为 `external` 的 WSS (MQTT/WebSocket/SSL) 监听器。


##### listener.wss.external.mqtt_path

  *类型*: **string**

  *默认值*: `/mqtt`



WebSocket 的 URL Path。


##### listener.wss.external.acceptors

  *类型*: **integer**

  *默认值*: 4



监听器的接收池大小。


##### listener.wss.external.max_connections

  *类型*: **integer**

  *默认值*: 16



监听器允许的最大并发连接数量。


##### listener.wss.external.max_conn_rate

  *类型*: **integer**

  *默认值*: 1000



监听器允许的最大接入速率。单位：个/秒。


##### listener.wss.external.active_n

  *类型*: **integer**

  *默认值*: 100



监听器持续接收 TCP 报文的次数。


##### listener.wss.external.rate_limit

  *类型*: **string**

  *默认值*: -



监听器的速率限制。格式为 `<limit>,<duration>`。


##### listener.wss.external.zone

  *类型*: **string**

  *默认值*: `external`



监听器所属的配置组 (Zone)。


##### listener.wss.external.access.1

  *类型*: **string**

  *默认值*: `allow all`



监听器的 ACL 规则列表。它用于设置连接层的白/黑名单。

例如:

`allow all`：表允许所有的 TCP 连接接入。
`allow 192.168.0.0/24`：表允许网络地址为 `192.168.0.0/24` 的 TCP 连接接入。

同时，该配置可配置多条规则:

```
listener.wss.external.access.1 = deny 192.168.0.1
listener.wss.external.access.2 = allow all
```


##### listener.wss.external.fail_if_no_subprotocol

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


如果设置为 true，则服务器将在客户端没有携带 Sec-WebSocket-Protocol 字段时返回错误。**微信小程序需关闭该验证**。


##### listener.wss.external.supported_protocols

  *类型*: **string**

  *默认值*: `mqtt, mqtt-v3, mqtt-v3.1.1, mqtt-v5`



指定支持的子协议，子协议之间以逗号分隔。


##### listener.wss.external.proxy_address_header

  *类型*: **string**

  *默认值*: `X-Forwarded-For`



如果 EMQX 集群部署在 HAProxy 或 Nginx，则可打开该配置获取客户端真实的 IP 地址。


##### listener.wss.external.proxy_protocol

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: -


监听器是否开启 `Proxy Protocol` 的支持。

如果 EMQX 集群部署在 HAProxy 或 Nginx 后，且需要拿到客户端真实的源 IP 地址与端口，则需打开此配置。

`Proxy Protcol` 参考：[https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol)。


##### listener.wss.external.proxy_protocol_timeout

  *类型*: **duration**

  *默认值*: -



设置 Proxy Protocol 解析的超时时间。如果该时间内没收到 Proxy Protocol 的报文，EMQX 会关闭其连接。


##### listener.wss.external.tls_versions

  *类型*: **string**

  *默认值*: `tlsv1.3,tlsv1.2,tlsv1.1,tlsv1`



指定服务端支持的 SSL 的版本列表。详情请参见 [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html)。


##### listener.wss.external.keyfile

  *类型*: **string**

  *默认值*: `etc/certs/key.pem`



指定 SSL 的私钥文件 (PEM)。


##### listener.wss.external.certfile

  *类型*: **string**

  *默认值*: `etc/certs/cert.pem`



指定 SSL 的证书文件 (PEM)。


##### listener.wss.external.cacertfile

  *类型*: **string**

  *默认值*: `etc/certs/cacert.pem`



指定 SSL 的 CA 证书文件 (PEM)。该文件应包含发布服务器证书的所有中间CA证书以及根证书。
该文件还应包含所有受信CA的证书用以用于验证客户端的证书。


##### listener.wss.external.depth

  *类型*: **number**

  *默认值*: `10`



证书链中非自签发的中间证书的最大数量。如果该值为 0 则表示，对端证书必须是根 CA 直接授信的。


##### listener.wss.external.key_password

  *类型*: **string**

  *默认值*: -



证书密钥文件的密码。如果你的证书密钥设置了密码，则需要配置该选项。


##### listener.wss.external.dhfile

  *类型*: **string**

  *默认值*: `etc/certs/dh-params.pem`



若使用 Ephemeral Diffie-Helman 算法，指定算法使用的 key 文件。


##### listener.wss.external.verify

  *类型*: **enum**

  *可选值*: `verify_peer`, `verify_none`

  *默认值*: `verify_peer`


指定握手过程中是否校验客户端。


##### listener.wss.external.fail_if_no_peer_cert

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


SSL 握手过程中若客户端没有证书，是否让握手失败。


##### listener.wss.external.ciphers

  *类型*: **string**

  *默认值*: `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA`



指定服务器支持的密码套件。


##### listener.wss.external.psk_ciphers

  *类型*: **string**

  *默认值*: `PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA`



若使用 PSK 算法，指定服务端支持的 PSK Cipher 列表。注意 'listener.wss.external.ciphers' 和 'listener.wss.external.psk_ciphers' 只能配置一个。


##### listener.wss.external.secure_renegotiate

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


指定在客户端不遵循 RFC 5746 的情况下，是否拒绝 renegotiation 请求。


##### listener.wss.external.reuse_sessions

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


指定是否支持 SSL session 重用。详情见 [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html)。


##### listener.wss.external.honor_cipher_order

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


指定是否使用服务端的偏好设置选择 Ciphers。


##### listener.wss.external.peer_cert_as_username

  *类型*: **enum**

  *可选值*: `cn`, `dn`, `crt`, `pem`, `md5`

  *默认值*: `cn`


使用客户端证书来覆盖 Username 字段的值。其可选值为：
- cn：客户端证书的 Common Name 字段值
- dn：客户端证书的 Subject Name 字段值
- crt：DER 格式编码的客户端证书二进制
- pem：基于 DER 格式上的 base64 编码后的字符串
- md5：DER 格式证书的 MD5 哈希值

注意 `listener.wss.external.verify` 应当设置为 `verify_peer`。


##### listener.wss.external.peer_cert_as_clientid

  *类型*: **enum**

  *可选值*: `cn`, `dn`, `crt`, `pem`, `md5`

  *默认值*: `cn`


使用客户端证书来覆盖 ClientID 字段的值。其可选值的含义同上。

注意 `listener.wss.external.verify` 应当设置为 `verify_peer`。


##### listener.wss.external.backlog

  *类型*: **integer**

  *默认值*: 1024



TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。


##### listener.wss.external.send_timeout

  *类型*: **duration**

  *默认值*: `15s`



TCP 报文发送超时时间。


##### listener.wss.external.send_timeout_close

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


TCP 报文发送超时后，是否关闭该连接。


##### listener.wss.external.recbuf

  *类型*: **bytesize**

  *默认值*: -



TCP 接收缓存区大小（操作系统内核级参数）

参见：http://erlang.org/doc/man/inet.html


##### listener.wss.external.sndbuf

  *类型*: **bytesize**

  *默认值*: -



TCP 发送缓存区大小（操作系统内核级参数）

参见：http://erlang.org/doc/man/inet.html


##### listener.wss.external.buffer

  *类型*: **bytesize**

  *默认值*: -



TCP 缓冲区大小 (用户级)。

该值建议大于等于 `sndbuff` 和 `recbuff` 的最大值，以避免一些性能问题。在不配置的情况下，它默认等于 sndbuff 和 recbuff 的最大值

参见：http://erlang.org/doc/man/inet.html


##### listener.wss.external.tune_buffer

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: -


如果打开此配置，请设置该值等于 `sndbuff` 与 `recbuff` 的最大值。


##### listener.wss.external.nodelay

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


即 `TCP_NODELAY` 参数。开启该选项即允许小的 TCP 数据报文将会立即发送。


##### listener.wss.external.compress

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


该选项若设置为 true，Websocket 消息将会被压缩。


##### listener.wss.external.deflate_opts.level

  *类型*: **enum**

  *可选值*: `none`, `default`, `best_compression`, `best_speed`

  *默认值*: `default`


压缩等级。


##### listener.wss.external.deflate_opts.mem_level

  *类型*: **integer**

  *可选值*: 1 - 9

  *默认值*: -


压缩参数。内存使用限制等级，配置可开辟多少内存来参与压缩过程。

`1`：最少的内存，但会降低压缩率。
`9`：最多的内存，会提高计算速度和压缩率。

不配置，则默认为 `8`。


##### listener.wss.external.deflate_opts.strategy

  *类型*: **enum**

  *可选值*: `default`, `filtered`, `huffman_only`, `rle`

  *默认值*: -


压缩策略，用于调优压缩率：

- `default`：针对普通数据。
- `filtered`：由过滤器或预测器产生的数据，适用于分布随机性强的内容。
- `huffman_only`：强制使用 Huffman 算法。优于 `filtered`。
- `rle`：将匹配距离限制为 1 (Run-Lenght Encoding)，比 `huffman_only` 要快，但主要用于 PNG 图片。

这些策略仅影响压缩率，不会对正确性带来任何影响。


##### listener.wss.external.deflate_opts.server_context_takeover

  *类型*: **enum**

  *可选值*: `takeover`, `no_takeover`

  *默认值*: -


是否允许服务端的压缩上下文在帧之间传递。


##### listener.wss.external.deflate_opts.client_context_takeover

  *类型*: **enum**

  *可选值*: `takeover`, `no_takeover`

  *默认值*: -


是否允许客户端的压缩上下文在帧之间传递。


##### listener.wss.external.deflate_opts.server_max_window_bits

  *类型*: **integer**

  *可选值*: 8 - 15

  *默认值*: -


服务端最大窗口值。设置一个较大的值会有更好的压缩率，但会额外的消耗内存。


##### listener.wss.external.deflate_opts.client_max_window_bits

  *类型*: **integer**

  *可选值*: 8 - 15

  *默认值*: -


客户端最大窗口值。设置一个较大的值会有更好的压缩率，但会额外的消耗内存。


##### listener.wss.external.idle_timeout

  *类型*: **duration**

  *默认值*: -



TCP 连接建立后的发呆时间，如果这段时间内未收到任何报文，则会关闭该连接。


##### listener.wss.external.max_frame_size

  *类型*: **integer**

  *默认值*: -



允许的单个 MQTT 报文长度的最大值。


## MQTT/TCP 监听器 - 11883

该监听器优化了高吞吐消息订阅性能，默认仅在本地地址监听，适用于后端应用订阅。

```
listener.tcp.internal = 127.0.0.1:11883
listener.tcp.internal.acceptors = 4
listener.tcp.internal.max_connections = 1024000
listener.tcp.internal.max_conn_rate = 1000
listener.tcp.internal.active_n = 1000
listener.tcp.internal.zone = internal
listener.tcp.internal.backlog = 512
listener.tcp.internal.send_timeout = 5s
listener.tcp.internal.send_timeout_close = on
listener.tcp.internal.recbuf = 64KB
listener.tcp.internal.sndbuf = 64KB
listener.tcp.internal.nodelay = false
listener.tcp.internal.reuseaddr = true
```

##### listener.tcp.internal

  *类型*: **string**

  *默认值*: `127.0.0.1:11883`



配置名称为 `internal` 的 MQTT/TCP 监听器的监听地址。

`11883`：表监听 IPv4 的 `0.0.0.0:11883`。
`127.0.0.1:11883`：表监听地址为 `127.0.0.1` 网卡上的 `11883` 端口。
`::1:11883`：表监听 IPv6 地址为 `::1` 网卡上的 `11883` 端口。


##### listener.tcp.internal.acceptors

  *类型*: **integer**

  *默认值*: 4



监听器的接收池大小。


##### listener.tcp.internal.max_connections

  *类型*: **integer**

  *默认值*: 1024000



监听器允许的最大并发连接数量。


##### listener.tcp.internal.max_conn_rate

  *类型*: **integer**

  *默认值*: 1000



监听器允许的最大接入速率。单位：个/秒


##### listener.tcp.internal.active_n

  *类型*: **integer**

  *默认值*: 1000



监听器持续接收 TCP 报文的次数。


##### listener.tcp.internal.zone

  *类型*: **string**

  *默认值*: `internal`



监听器所属的配置域 (Zone)。


##### listener.tcp.internal.rate_limit

  *类型*: **string**

  *默认值*: -



监听器的速率限制。格式为 `<limit>,<duration>`。

`100KB,10s`：表 *限制 10 秒内的流入字节数不超过 100 KB*。


##### listener.tcp.internal.backlog

  *类型*: **integer**

  *默认值*: 512



TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。


##### listener.tcp.internal.send_timeout

  *类型*: **duration**

  *默认值*: `5s`



TCP 报文发送超时时间。


##### listener.tcp.internal.send_timeout_close

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


TCP 报文发送超时后，是否关闭该连接。


##### listener.tcp.internal.recbuf

  *类型*: **bytesize**

  *默认值*: `64KB`



TCP 接收缓存区大小（操作系统内核级参数）


##### listener.tcp.internal.sndbuf

  *类型*: **bytesize**

  *默认值*: `64KB`



TCP 发送缓存区大小（操作系统内核级参数）


##### listener.tcp.internal.buffer

  *类型*: **bytesize**

  *默认值*: -



TCP 缓冲区大小 (用户级)。


##### listener.tcp.internal.tune_buffer

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: -


如果打开此配置，请设置该值等于 `sndbuff` 与 `recbuff` 的最大值。


##### listener.tcp.internal.nodelay

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


即 `TCP_NODELAY` 参数。开启该选项即允许小的 TCP 数据报文将会立即发送。


##### listener.tcp.internal.reuseaddr

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


即 `SO_REUSEADDR` 参数。开启该选项即允许本地重用端口，无需等待 `TIME_WAIT` 状态结束。


## External Zone 配置

```
zone.external.idle_timeout = 15s
zone.external.enable_acl = on
zone.external.enable_ban = on
zone.external.enable_stats = on
zone.external.acl_deny_action = ignore
zone.external.force_gc_policy = 16000|16MB
#zone.external.force_shutdown_policy = 10000|64MB
zone.external.keepalive_backoff = 0.75
zone.external.max_subscriptions = 0
zone.external.upgrade_qos = off
zone.external.max_inflight = 32
zone.external.retry_interval = 30s
zone.external.max_awaiting_rel = 100
zone.external.await_rel_timeout = 300s
zone.external.session_expiry_interval = 2h
zone.external.max_mqueue_len = 1000
zone.external.mqueue_priorities = none
zone.external.mqueue_default_priority = highest
zone.external.mqueue_store_qos0 = true
zone.external.enable_flapping_detect = off
#zone.external.rate_limit.conn_messages_in = 100,10s
#zone.external.rate_limit.conn_bytes_in = 100KB,10s
#zone.external.conn_congestion.alarm = off
#zone.external.conn_congestion.min_alarm_sustain_duration = 1m
#zone.external.quota.conn_messages_routing = 100,1s
#zone.external.quota.overall_messages_routing = 200000,1s
zone.external.use_username_as_clientid = false
zone.external.ignore_loop_deliver = false
zone.external.strict_mode = false
```

##### zone.external.idle_timeout

  *类型*: **duration**

  *默认值*: `15s`



TCP 连接建立后的发呆时间，如果这段时间内未收到任何报文，则会关闭该连接。


##### zone.external.enable_acl

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


是否开启 ACL 检查。


##### zone.external.enable_ban

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


是否开启黑名单。


##### zone.external.enable_stats

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


是否开启客户端状态统计。


##### zone.external.acl_deny_action

  *类型*: **enum**

  *可选值*: `ignore`, `disconnect`

  *默认值*: `ignore`


ACL 检查失败后，执行的操作。

- `ignore`：不做任何操作。
- `disconnect`：断开连接。


##### zone.external.force_gc_policy

  *类型*: **string**

  *可选值*: `16000

  *默认值*: 16MB`


当收到一定数量的消息，或字节，就强制执行一次垃圾回收。

格式：`<Number>|<Bytes>`。

例如，`16000|16MB` 表示当收到 `16000` 条消息，或 `16MB` 的字节流入就强制执行一次垃圾回收。


##### zone.external.force_shutdown_policy

  *类型*: **string**

  *默认值*: -



当进程消息队列长度，或占用的内存字节到达某值，就强制关闭该进程。

这里的 `消息队列` 指的是 Erlang 进程的 `消息邮箱`，并非 QoS 1 和 QoS 2 的 `mqueue`。

格式：`<Number>|<Bytes>`。

例如，`32000|32MB` 表示当进程堆积了 `32000` 条消息，或进程占用内存达到 `32MB` 则关闭该进程。


##### zone.external.max_packet_size

  *类型*: **bytesize**

  *默认值*: -



允许的 MQTT 报文最大长度。


##### zone.external.max_clientid_len

  *类型*: **integer**

  *默认值*: -



允许的 Client ID 串的最大长度。


##### zone.external.max_topic_levels

  *类型*: **integer**

  *默认值*: -



允许客户端订阅主题的最大层级。0 表示不限制。

::: warning Warning
Topic层级过多可能导致订阅时的性能问题。
:::


##### zone.external.max_qos_allowed

  *类型*: **enum**

  *可选值*: `0`, `1`, `2`

  *默认值*: -


允许客户端发布的最大 QoS 等级。


##### zone.external.max_topic_alias

  *类型*: **integer**

  *默认值*: -



允许最大的主题别名数。0 表示不支持主题别名。


##### zone.external.retain_available

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: -


是否支持 Retain 消息。


##### zone.external.wildcard_subscription

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: -


是否支持订阅通配主题。


##### zone.external.shared_subscription

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: -


是否支持共享订阅。


##### zone.external.server_keepalive

  *类型*: **integer**

  *默认值*: -



服务端指定的 Keepalive 时间。用于 MQTT v5.0 协议的 CONNACK 报文。


##### zone.external.keepalive_backoff

  *类型*: **float**

  *可选值*: > 0.5

  *默认值*: 0.75


Keepalive 退避指数。EMQX 如果在 `Keepalive * backoff * 2` 的时间内未收到客户端的任何数据报文，则认为客户端已心跳超时。


##### zone.external.max_subscriptions

  *类型*: **integer**

  *默认值*: 0



单个客户端允许订阅的最大主题数。`0` 表示不限制。


##### zone.external.upgrade_qos

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


允许 EMQX 在投递消息时，强制升级消息的 QoS 等级为订阅的 QoS 等级。


##### zone.external.max_inflight

  *类型*: **integer**

  *默认值*: 32



飞行窗口大小。飞行窗口用于存储未被应答的 QoS 1 和 QoS 2 消息。


##### zone.external.retry_interval

  *类型*: **duration**

  *默认值*: `30s`



消息重发间隔。EMQX 在每个间隔检查是否需要进行消息重发。


##### zone.external.max_awaiting_rel

  *类型*: **integer**

  *默认值*: 100



QoS 2 消息的最大接收窗口，配置 EMQX 能够同时处理多少从客户端发来的 QoS 2 消息。`0` 表示不限制。


##### zone.external.await_rel_timeout

  *类型*: **duration**

  *默认值*: `300s`



QoS 2 消息处理超时时间，在超时后若还未收到 QoS 的 PUBREL 报文，则将消息从接收窗口中丢弃。


##### zone.external.session_expiry_interval

  *类型*: **duration**

  *默认值*: `2h`



会话默认超时时间，主要用于 MQTT v3.1 和 v3.1.1 协议。在 MQTT v5.0 中，该值通常会携带在客户端的连接报文中。


##### zone.external.max_mqueue_len

  *类型*: **integer**

  *默认值*: 1000



消息队列最大长度。当飞行窗口满，或客户端离线后，消息会被存储至该队列中。0 表示不限制。


##### zone.external.mqueue_priorities

  *类型*: **string**

  *可选值*: `none`, `<Spec>`

  *默认值*: `none`


队列消息优先级配置：

- `none`：表示无优先级区分。
- `<Spec>`：表示为一个消息优先表，它配置了某主题下消息的优先级。例如：
    * `topic/1=10`：表示主题 `topic/1` 的消息优先级为 `10`。
    * `topic/1=10,topic/2=8`：表示配置了两个主题的优先级，其分别为 `10` 和 `8`。
    * 其中，优先级数值越高，优先等级越高。

当消息队列长度有限时，会优先丢弃低优先级的消息。


##### zone.external.mqueue_default_priority

  *类型*: **enum**

  *可选值*: `highest`, `lowest`

  *默认值*: `highest`


消息默认的优先等级。


##### zone.external.mqueue_store_qos0

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


消息队列是否存储 QoS 0 消息。


##### zone.external.enable_flapping_detect

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


是否开启 `Flapping` 检查。


##### zone.external.mountpoint

  *类型*: **string**

  *默认值*: -



主题挂载点。配置后，所有订阅和发布的主题在 EMQX 都会为其增加一个前缀。

其中可用的占位符有：
- `%c`：表示客户端的 Client ID。
- `%u`：表示客户端的 Username。

例如，配置挂载点为 `user/%c/`。那么 Client ID 为 `tom` 的客户端在发布主题 `open` 消息时，实际在 EMQX 中路由的主题是 `user/tom/open`。


##### zone.external.use_username_as_clientid

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否用客户端的 Username 作为其 Client ID。


##### zone.external.ignore_loop_deliver

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否忽略自己发送的消息。如果忽略，则表明 EMQX 不会向消息的发送端投递此消息。


##### zone.external.strict_mode

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否开启严格检查模式。严格检查模式会更细致的检查 MQTT 报文的正确性。



## Internal Zone 配置

```
zone.internal.allow_anonymous = true
zone.internal.enable_stats = on
zone.internal.enable_acl = off
zone.internal.acl_deny_action = ignore
zone.internal.max_subscriptions = 0
zone.internal.max_inflight = 128
zone.internal.max_awaiting_rel = 1000
zone.internal.max_mqueue_len = 10000
zone.internal.mqueue_store_qos0 = true
zone.internal.enable_flapping_detect = off
#zone.internal.force_shutdown_policy = 10000|64MB
zone.internal.ignore_loop_deliver = false
zone.internal.strict_mode = false
zone.internal.bypass_auth_plugins = true
```

##### zone.internal.allow_anonymous

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


是否允许匿名用户登录系统。


##### zone.internal.enable_stats

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


是否开启客户端状态统计。


##### zone.internal.enable_acl

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


是否开启 ACL 检查。


##### zone.internal.acl_deny_action

  *类型*: **enum**

  *可选值*: `ignore`, `disconnect`

  *默认值*: `ignore`


ACL 检查失败后，执行的操作。

- `ignore`：不做任何操作。
- `disconnect`：断开连接。


##### zone.internal.force_gc_policy

  *类型*: **string**

  *默认值*: -



当收到一定数量的消息，或字节，就强制执行一次垃圾回收。

格式：`<Number>|<Bytes>`。

例如，`16000|16MB` 表示当收到 `16000` 条消息，或 `16MB` 的字节流入就强制执行一次垃圾回收。


##### zone.internal.wildcard_subscription

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: -


是否支持订阅通配主题。


##### zone.internal.shared_subscription

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: -


是否支持共享订阅。


##### zone.internal.max_subscriptions

  *类型*: **integer**

  *默认值*: 0



单个客户端允许订阅的最大主题数。`0` 表示不限制。


##### zone.internal.max_inflight

  *类型*: **integer**

  *默认值*: 128



飞行窗口大小。飞行窗口用于存储未被应答的 QoS 1 和 QoS 2 消息。


##### zone.internal.max_awaiting_rel

  *类型*: **integer**

  *默认值*: 1000



QoS 2 消息的最大接收窗口，配置 EMQX 能够同时处理多少从客户端发来的 QoS 2 消息。`0` 表示不限制。


##### zone.internal.max_mqueue_len

  *类型*: **integer**

  *默认值*: 10000



消息队列最大长度。当飞行窗口满，或客户端离线后，消息会被存储至该队列中。`0` 表示不限制。


##### zone.internal.mqueue_store_qos0

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


消息队列是否存储 QoS 0 消息。


##### zone.internal.enable_flapping_detect

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


是否开启 `Flapping` 检查。


##### zone.internal.force_shutdown_policy

  *类型*: **string**

  *默认值*: -



当进程消息队列长度，或占用的内存字节到达某值，就强制关闭该进程。

这里的 `消息队列` 指的是 Erlang 进程的 `消息邮箱`，并非 QoS 1 和 QoS 2 的 `mqueue`。

格式：`<Number>|<Bytes>`。

例如，`32000|32MB` 表示当进程堆积了 `32000` 条消息，或进程占用内存达到 `32MB` 则关闭该进程。


##### zone.internal.mountpoint

  *类型*: **string**

  *默认值*: -



主题挂载点。配置后，所有订阅和发布的主题在 EMQX 都会为其增加一个前缀。

其中可用的占位符有：
- `%c`：表示客户端的 Client ID。
- `%u`：表示客户端的 Username。

例如，配置挂载点为 `user/%c/`。那么 Client ID 为 `tom` 的客户端在发布主题 `open` 消息时，实际在 EMQX 中路由的主题是 `user/tom/open`。


##### zone.internal.ignore_loop_deliver

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否忽略自己发送的消息。如果忽略，则表明 EMQX 不会向消息的发送端投递此消息。


##### zone.internal.strict_mode

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否开启严格检查模式。严格检查模式会更细致的检查 MQTT 报文的正确性。


##### zone.internal.bypass_auth_plugins

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


是否允许该 Zone 下的客户端绕过认证插件的认证步骤。


## 日志配置

```
log.to = file
log.level = warning
log.dir = log
log.file = emqx.log
#log.chars_limit = 8192
#log.max_depth = 100
log.formatter = text
# log.formatter.text.date.format = rfc3339
#log.single_line = true
log.rotation = on
log.rotation.size = 10MB
log.rotation.count = 5
#log.info.file  = info.log
#log.error.file = error.log
#log.sync_mode_qlen = 100
#log.drop_mode_qlen = 3000
#log.flush_qlen = 8000
#log.overload_kill = on
#log.overload_kill_qlen = 20000
#log.overload_kill_mem_size = 30MB
#log.overload_kill_restart_after = 5s
#log.burst_limit = 20000, 1s
```

##### log.to

  *类型*: **enum**

  *可选值*: `off`, `file`, `console`, `both`

  *默认值*: `file`


将日志输出到什么地方。可选值为:

- **off:** 完全关闭日志功能

- **file:** 仅将日志输出到文件

- **console:** 仅将日志输出到标准输出(emqx 控制台)

- **both:** 同时将日志输出到文件和标准输出(emqx 控制台)



##### log.level

  *类型*: **enum**

  *可选值*: `debug`, `info`, `notice`, `warning`<br/>`error`, `critical`, `alert`, `emergency`

  *默认值*: `warning`


全局的日志级别。这包括 primary log level 以及所有的 log handlers。详情请参见 [日志级别和 log handlers](../getting-started/log.md#log-level-and-log-handlers)。


##### log.dir

  *类型*: **dir**

  *默认值*: `./log`



日志文件目录。


##### log.file

  *类型*: **string**

  *默认值*: `emqx.log`



日志文件的前缀。例如，若使用默认值 (`log.file = emqx.log`)，日志文件名将为 `emqx.log.1`，`emqx.log.2`，...。


##### log.chars_limit

  *类型*: **integer**

  *默认值*: -1



设置单个日志消息的最大长度。如超过此长度，日志消息将被截断。`-1` 表示无限制。


##### log.max_depth

  *类型*: **union(integer, 'unlimited')**

  *默认值*: 20




控制 Eralng 数据结构的打印深度，和 Erlang 进程消息队列查看的深度。
或配置成 'unlimited' (不带引号) 不限深度打印。


##### log.rotation.size

  *类型*: **bytesize**

  *默认值*: `10MB`



设置单个日志文件大小。如超过此大小，则进行日志文件滚动，创建新的日志文件。


##### log.rotation.count

  *类型*: **integer**

  *默认值*: 5



设置日志文件总个数。如超过此文件个数，则下一次日志文件滚动将会覆盖第一个文件。


##### log.\<level>.file

  *类型*: **string**

  *默认值*: -



针对某日志级别设置单独的日志文件。

将 info 及 info 以上的日志单独输出到 `info.log.N` 文件中：

```
log.info.file = info.log
```

将 error 及 error 以上的日志单独输出到 `error.log.N` 文件中

```
log.error.file = error.log
```


##### log.max_depth

  *类型*: **integer**

  *默认值*: 20



控制对大的数据结构打印日志时的最大深度。超过深度的部分将被 '...' 代替。



##### log.single_line

  *类型*: **boolean**

  *默认值*: true



设置成 `true` 时，单行打印日志。
如果设置成 `false`, 如 crash 日志中的堆栈信息等将打印多行


##### log.formatter

  *类型*: **enum**

  *可选值*: `text`, `json`

  *默认值*: `text`


选择打印日志的格式

##### log.formatter.text.date.format

  *类型*: **enum**

  *默认值*: `rfc3339`



注意: 这个配置在 EMQX 开源版 4.3.15, 4.4.4 和 EMQX 企业版 4.3.10, 4.4.4 及之后可以使用。

指定 `text` logger 的时间戳格式。可以是 `rfc3339` 或者 FORMAT 字符串。

其中 FORMAT 里支持的控制符如下:

| 控制符 | 说明                        | 格式示例  |
| ------ | --------------------------- | --------- |
| %Y     | 年                          | 2022      |
| %m     | 月 (01..12)                 | 11        |
| %d     | 日                          | 01        |
| %H     | 时 (00..23)                 | 06        |
| %M     | 分 (00..59)                 | 43        |
| %S     | 秒 (00..60)                 | 31        |
| %N     | 纳秒 (000000000..999999999) | 019085000 |
| %6N    | 微秒 (00000..999999)        | 019085    |
| %3N    | 毫秒 (000..999)             | 019       |
| %z     | +HHMM 数字时区              | -0400     |
| %:z    | +HH:MM 数字时区             | -04:00    |
| %::z   | +HH:MM:SS 数字时区          | -04:00:00 |

举例:

```
## 2022-06-02T14:23:09.230000 +08:00
log.formatter.text.date.format = %Y-%m-%dT%H:%M:%S.%6N %:z

## 如下配置可以让时间戳跟 4.2.x 版本的一样 (2022-06-02 14:24:36.124):
log.formatter.text.date.format = %Y-%m-%d %H:%M:%S.%3N
```


## Erlang 虚拟机监控参数

```
sysmon.long_gc = 0
sysmon.long_schedule = 240ms
sysmon.large_heap = 8MB
sysmon.busy_port = false
sysmon.busy_dist_port = true
os_mon.cpu_check_interval = 60s
os_mon.cpu_high_watermark = 80%
os_mon.cpu_low_watermark = 60%
os_mon.mem_check_interval = 60s
os_mon.sysmem_high_watermark = 70%
os_mon.procmem_high_watermark = 5%
vm_mon.check_interval = 30s
vm_mon.process_high_watermark = 80%
vm_mon.process_low_watermark = 60%
```

##### sysmon.long_gc

  *类型*: **duration**

  *默认值*: `0ms`



启用垃圾回收时间监控并在回收时间超过设定值时触发告警，0 表示禁用此监控。


##### sysmon.long_schedule

  *类型*: **duration**

  *默认值*: `240ms`



启用进程调度时间监控并在调度时间超过设定值时触发告警，0 表示禁用此监控。


##### sysmon.large_heap

  *类型*: **bytesize**

  *默认值*: `8MB`



启用堆栈大小监控并在进程执行垃圾回收后堆栈大小仍大于设定值时触发告警，0 表示禁用此监控。


##### sysmon.busy_port

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


指定是否启用进程间消息通道拥塞监控。


##### sysmon.busy_dist_port

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


指定是否启用集群 RPC 通道拥塞监控。


##### os_mon.cpu_check_interval

  *类型*: **duration**

  *默认值*: `60s`



CPU 占用率检查周期。


##### os_mon.cpu_high_watermark

  *类型*: **percent**

  *默认值*: `80%`



CPU 占用率超过 `os_mon.cpu_high_watermark` 时将触发告警。


##### os_mon.cpu_low_watermark

  *类型*: **percent**

  *默认值*: `60%`



CPU 占用率回落到 `os_mon.cpu_low_watermark` 以下时将清除告警。


##### os_mon.mem_check_interval

  *类型*: **duration**

  *默认值*: `60s`



内存占用率检查周期。


##### os_mon.sysmem_high_watermark

  *类型*: **percent**

  *默认值*: `70%`



EMQX 为所有进程分配的内存占系统内存的百分比超过 `os_mon.sysmem_high_watermark` 时将触发告警。


##### os_mon.procmem_high_watermark

  *类型*: **percent**

  *默认值*: `5%`



EMQX 为单个进程分配的内存占系统内存的百分比超过 `os_mon.procmem_high_watermark` 时将触发告警。


##### vm_mon.check_interval

  *类型*: **duration**

  *默认值*: `30s`



进程数量检查周期。


##### vm_mon.process_high_watermark

  *类型*: **percent**

  *默认值*: `80%`



当前进程数量占进程最大数量的百分比超过 `vm_mon.process_high_watermark` 时将触发告警。进程最大数量由 `node.process_limit` 配置项决定。


##### vm_mon.process_low_watermark

  *类型*: **percent**

  *默认值*: `60%`



当前进程数量占进程最大数量的百分比回落到 `vm_mon.process_low_watermark` 以下时将触发告警。进程最大数量由 `node.process_limit` 配置项决定。


## 插件配置

##### plugins.etc_dir

  *类型*: **string**

  *默认值*: `etc/plugins`



插件的配置目录。


##### plugins.loaded_file

  *类型*: **string**

  *默认值*: `etc/loaded_plugins`



插件启动列表的配置文件路径。


##### plugins.expand_plugins_dir

  *类型*: **string**

  *默认值*: `plugins/`



外部插件存放目录。


## 插件 `emqx-auth-http`

##### auth.http.auth_req.url

  *类型*: **string**

  *默认值*: `http://127.0.0.1:80/mqtt/auth`



指定认证请求的目标 URL。


##### auth.http.auth_req.method

  *类型*: **enum**

  *可选值*: `get`, `post`

  *默认值*: `post`


指定认证请求的请求方法。


##### auth.http.auth_req.headers.\<Any\>

```
auth.http.auth_req.headers.content-type = application/x-www-form-urlencoded
auth.http.auth_req.headers.accept = */*
```

指定 HTTP 请求头部中的数据。`<Key>` 指定 HTTP 请求头部中的字段名，此配置项的值为相应的字段值。`<Key>` 可以是标准的 HTTP 请求头部字段，也可以自定义的字段，可以配置多个不同的请求头部字段。


##### auth.http.auth_req.params

  *类型*: **string**

  *可选值*: 以 `,` 分隔的 `k=v` 键值对，`v` 可以是固定内容，也可以是占位符

  *默认值*: `clientid=%c,username=%u,password=%P`


指定认证请求中携带的数据。使用 GET 方法时 `auth.http.auth_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以查询字符串参数的形式发送。使用 POST 方法时 `auth.http.auth_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以 Request Body 的形式发送。所有的占位符都会被运行时数据所替换，可用的占位符如下：

| 占位符 | 替换内容                                                                  |
| ------ | ------------------------------------------------------------------------- |
| `%u`   | 用户名                                                                    |
| `%c`   | MQTT Client ID                                                            |
| `%a`   | 客户端的网络 IP 地址                                                      |
| `%r`   | 客户端使用的协议，可以是：`mqtt`, `mqtt-sn`, `coap`, `lwm2m` 以及 `stomp` |
| `%P`   | 密码                                                                      |
| `%p`   | 客户端连接的服务端端口                                                    |
| `%c`   | 客户端证书中的 Common Name                                                |
| `%d`   | 客户端证书中的 Subject                                                    |


##### auth.http.super_req.url

  *类型*: **string**

  *默认值*: `http://127.0.0.1:80/mqtt/superuser`



指定超级用户认证请求的目标 URL。


##### auth.http.super_req.method

  *类型*: **enum**

  *可选值*: `get`, `post`

  *默认值*: `post`


指定超级用户认证请求的请求方法。


##### auth.http.super_req.headers.\<Any\>

```
auth.http.super_req.headers.content-type = application/x-www-form-urlencoded
auth.http.super_req.headers.accept = */*
```

指定 HTTP 请求头部中的数据。`<Key>` 指定 HTTP 请求头部中的字段名，此配置项的值为相应的字段值。`<Key>` 可以是标准的 HTTP 请求头部字段，也可以自定义的字段，可以配置多个不同的请求头部字段。


##### auth.http.super_req.params

  *类型*: **string**

  *可选值*: 以 `,` 分隔的 `k=v` 键值对，`v` 可以是固定内容，也可以是占位符

  *默认值*: `clientid=%c,username=%u`


指定超级用户认证请求中携带的数据。使用 GET 方法时 `auth.http.super_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以查询字符串参数的形式发送。使用 POST 方法时 `auth.http.super_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以 Request Body 的形式发送。所有的占位符都会被运行时数据所替换，可用的占位符同 `auth.http.auth_req.params`。


##### auth.http.acl_req

  *类型*: **string**

  *默认值*: `http://127.0.0.1:8991/mqtt/acl`



指定 ACL 验证请求的目标 URL。


##### auth.http.acl_req.method

  *类型*: **enum**

  *可选值*: `get`, `post`

  *默认值*: `post`


指定 ACL 验证请求的请求方法。


##### auth.http.acl_req.headers.\<Any\>

```
auth.http.acl_req.headers.content-type = application/x-www-form-urlencoded
auth.http.acl_req.headers.accept = */*
```

指定 HTTP 请求头部中的数据。`<Key>` 指定 HTTP 请求头部中的字段名，此配置项的值为相应的字段值。`<Key>` 可以是标准的 HTTP 请求头部字段，也可以自定义的字段，可以配置多个不同的请求头部字段。


##### auth.http.acl_req.params

  *类型*: **string**

  *可选值*: 以 `,` 分隔的 `k=v` 键值对，`v` 可以是固定内容，也可以是占位符

  *默认值*: `access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t,mountpoint=%m`


指定 ACL 验证请求中携带的数据。使用 GET 方法时 `auth.http.acl_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以查询字符串参数的形式发送。使用 POST 方法时 `auth.http.acl_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以 Request Body 的形式发送。所有的占位符都会被运行时数据所替换，可用的占位符如下：

| 占位符 | 替换内容                                                                  |
| ------ | ------------------------------------------------------------------------- |
| `%A`   | 需要验证的权限，1 表示订阅，2 表示发布                                    |
| `%u`   | MQTT Client ID                                                            |
| `%c`   | 客户端标识符                                                              |
| `%a`   | 客户端的网络 IP 地址                                                      |
| `%r`   | 客户端使用的协议，可以是：`mqtt`, `mqtt-sn`, `coap`, `lwm2m` 以及 `stomp` |
| `%m`   | 挂载点                                                                    |
| `%t`   | 主题                                                                      |


##### auth.http.timeout

  *类型*: **duration**

  *默认值*: `5s`



HTTP 请求超时时间。任何等价于 `0s` 的设定值都表示永不超时。


##### auth.http.connect_timeout

  *类型*: **duration**

  *默认值*: `5s`



HTTP 请求的连接超时时间。任何等价于 `0s` 的设定值都表示永不超时。


##### auth.http.ssl.cacertfile

  *类型*: **string**

  *默认值*: `etc/certs/ca.pem`



CA 证书文件路径。


##### auth.http.ssl.certfile

  *类型*: **string**

  *默认值*: `etc/certs/client-cert.pem`



客户端证书文件路径。


##### auth.http.ssl.keyfile

  *类型*: **string**

  *默认值*: `etc/certs/client.key.pem`



客户端私钥文件路径。


## 插件 `emqx_auth_jwt`

##### auth.jwt.secret

  *类型*: **string**

  *默认值*: `emqxsecret`



设置 HMAC Secret。


##### auth.jwt.from

  *类型*: **enum**

  *可选值*: `username`, `password`

  *默认值*: `password`


从什么地方获取 JWT。可选值为:

- username: MQTT CONNECT 报文的 username 字段作为 JWT。
- password: MQTT CONNECT 报文的 password 字段作为 JWT。


##### auth.jwt.pubkey

  *类型*: **string**

  *默认值*: `etc/certs/jwt_public_key.pem`



若使用 RSA 或者 ECDSA 加密算法，须指定私钥文件。


##### auth.jwt.verify_claims

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


启用或关闭 Claims 校验功能。


##### auth.jwt.verify_claims.\<claims>

  *类型*: **string**

  *默认值*: -



启用 Claims 校验功能时，可设置 JWT 中字段的可选值。

例如，若期望 JWT 中的 Claim 字段 `sub` 的值为 "abc"，则可以配置如下规则:

```
auth.jwt.verify_claims.sub = abc
```

期望值支持两个通配符:

- `%u`: username
- `%c`: clientid

例如，若期望 JWT 中的 Claim 字段 `sub` 的值与 MQTT CONNECT 报文中 username 字段相同，则可以配置如下规则:

```
auth.jwt.verify_claims.sub = %u
```


## 插件 `emqx_auth_ldap`

##### auth.ldap.servers

  *类型*: **string**

  *默认值*: `127.0.0.1`



LDAP 服务地址。


##### auth.ldap.port

  *类型*: **integer**

  *默认值*: 389



LDAP 服务端口。


##### auth.ldap.pool

  *类型*: **integer**

  *可选值*: > 0

  *默认值*: 8


连接池大小。


##### auth.ldap.bind_dn

  *类型*: **string**

  *默认值*: `cn=root,dc=emqx,dc=io`



登入 LDAP 服务的 DN。


##### auth.ldap.bind_password

  *类型*: **string**

  *默认值*: `public`



登入 LDAP 服务的密码。


##### auth.ldap.timeout

  *类型*: **duration**

  *默认值*: `30s`



查询操作的超时时间。


##### auth.ldap.device_dn

  *类型*: **string**

  *默认值*: `ou=device,dc=emqx,dc=io`



客户端隶属的 DN。


##### auth.ldap.match_objectclass

  *类型*: **string**

  *默认值*: `mqttUser`



客户端对象的名称。


##### auth.ldap.username.attributetype

  *类型*: **string**

  *默认值*: `uid`



Username 属性的数据类型。


##### auth.ldap.password.attributetype

  *类型*: **string**

  *默认值*: `userPassword`



Password 属性的数据类型。


##### auth.ldap.ssl

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否开启 SSL。


##### auth.ldap.ssl.certfile

  *类型*: **string**

  *默认值*: -



SSL 服务端证书路径。


##### auth.ldap.ssl.keyfile

  *类型*: **string**

  *默认值*: -



SSL 服务端秘钥文件路径。


##### auth.ldap.ssl.cacertfile

  *类型*: **string**

  *默认值*: -



CA 证书文件路径。


##### auth.ldap.ssl.verify

  *类型*: **enum**

  *可选值*: `verify_peer`, `verify_none`

  *默认值*: -


SSL 认证方式：

- `verify_none`：单向认证。
- `verify_peer`：双向认证。


##### auth.ldap.ssl.fail_if_no_peer_cert

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


如果客户端未提供 SSL 证书，则断开连接。


## 插件 `emqx_auth_mongo`

##### auth.mongo.type

  *类型*: **enum**

  *可选值*: `single`, `unknown`, `sharded`, `rs`

  *默认值*: `single`


设置 MongoDB 的拓扑类型:

- single: 单节点

- unknown: 未知

- sharded: 分片模式

- rs: 副本模式 (replicated set)



##### auth.mongo.rs_set_name

  *类型*: **string**

  *默认值*: `127.0.0.1:27017`



设置 MongoDB 服务的地址。如有多个使用逗号 `,` 分隔。


##### auth.mongo.pool

  *类型*: **integer**

  *默认值*: 8



设置 MongoDB 连接池的进程数。


##### auth.mongo.login

  *类型*: **string**

  *默认值*: -



设置 MongoDB 的用户名。


##### auth.mongo.password

  *类型*: **string**

  *默认值*: -



设置 MongoDB 的密码。


##### auth.mongo.auth_source

  *类型*: **string**

  *默认值*: `mqtt`



设置 MongoDB 的认证源数据库名。


##### auth.mongo.database

  *类型*: **string**

  *默认值*: `mqtt`



设置 MongoDB 的数据库名。


##### auth.mongo.query_timeout

  *类型*: **duration**

  *默认值*: `5s`



设置访问 MongoDB 超时时间。


##### auth.mongo.ssl

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


设置是否使用 SSL 访问 MongoDB。


##### auth.mongo.ssl_opts.keyfile

  *类型*: **string**

  *默认值*: -



若使用 SSL 访问 MongoDB，设置 SSL 客户端的私钥文件。


##### auth.mongo.ssl_opts.certfile

  *类型*: **string**

  *默认值*: -



若使用 SSL 访问 MongoDB，设置 SSL 客户端的证书文件。


##### auth.mongo.ssl_opts.cacertfile

  *类型*: **string**

  *默认值*: -



若使用 SSL 访问 MongoDB，设置 SSL 的 CA 证书文件。


##### auth.mongo.w_mode

  *类型*: **enum**

  *可选值*: `unsafe`, `safe`, `undef`

  *默认值*: `undef`


设置 MongoDB 的写入模式。


##### auth.mongo.r_mode

  *类型*: **enum**

  *可选值*: `master`, `slave_ok`, `undef`

  *默认值*: `undef`


设置 MongoDB 的读取模式。


##### auth.mongo.auth_query.collection

  *类型*: **string**

  *默认值*: `mqtt_user`



认证过程用的 Collection 名字。


##### auth.mongo.auth_query.password_field

  *类型*: **string**

  *默认值*: `password`



认证过程用的主要字段。如需在密码之后加 salt，可以配置为:

```
auth.mongo.auth_query.password_field = password,salt
```


##### auth.mongo.auth_query.password_hash

  *类型*: **enum**

  *可选值*: `plain`, `md5`, `sha`, `sha256`, `bcrypt`

  *默认值*: `sha256`


设置密码字段用的哈希算法。如需在 sha256 密码之后加 salt，可以设置为:

```
auth.mongo.auth_query.password_hash = sha256,salt
```

如需在 sha256 密码之前加 salt，可以设置为:

```
auth.mongo.auth_query.password_hash = salt,sha256
```

如需在 bcrypt 密码之前加 salt，可以设置为:

```
auth.mongo.auth_query.password_hash = salt,bcrypt
```


##### auth.mongo.auth_query.selector

  *类型*: **string**

  *默认值*: `username=%u`



认证过程执行的 MongoDB 语句。命令可支持通配符:

- %u: username
- %c: clientid
- %C: 客户端 TLS 证书里的 Common Name
- %d: 客户端 TLS 证书里的 Subject


##### auth.mongo.auth_query.super_query

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


认证中是否使用 SuperUser。


##### auth.mongo.super_query.collection

  *类型*: **string**

  *默认值*: `mqtt_user`



若使用 SuperUser，指定 SuperUser 的 MongoDB Collection。


##### auth.mongo.super_query.selector

  *类型*: **string**

  *默认值*: `username=%u, clientid=%c`



若使用 SuperUser，指定查询 SuperUser 使用的 MongoDB 语句。


##### auth.mongo.acl_query

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


是否开启 ACL 功能。


##### auth.mongo.acl_query.collection

  *类型*: **string**

  *默认值*: `mqtt_acl`



若使用 ACL 功能，指定查询 ACL 规则的 MongoDB Collection。


##### auth.mongo.acl_query.selector

  *类型*: **string**

  *默认值*: `username=%u`



若使用 ACL 功能，指定查询 ACL 规则使用的 MongoDB 语句。可支持多个 ACL 语句，多个语句之间使用 or 连接。

例如，配置如下两条访问规则：

```
auth.mongo.acl_query.selector.1 = username=%u
auth.mongo.acl_query.selector.2 = username=$all
```

并且客户端的 username='ilyas'，则在查询 acl 规则的时候，会执行如下 MongoDB 语句：

```
db.mqtt_acl.find({$or: [{username: "ilyas"},  {username: "$all"}]});
```


##### auth.mongo.topology.pool_size

  *类型*: **integer**

  *默认值*: 1



MongoDB 拓扑参数，设置线程池大小。


##### auth.mongo.topology.max_overflow

  *类型*: **integer**

  *默认值*: 0



MongoDB 拓扑参数，当线程池中所有 workers 都处于忙碌状态时，允许创建多少额外的 worker 线程。


##### auth.mongo.topology.overflow_ttl

  *类型*: **integer**

  *默认值*: 1000



MongoDB 拓扑参数，当有 worker 空闲时。多久之后释放额外的 worker 线程。单位: 毫秒。


##### auth.mongo.topology.overflow_check_period

  *类型*: **integer**

  *默认值*: 1000



MongoDB 拓扑参数，多长时间检查一次有无空闲线程，以释放额外的 worker。


##### auth.mongo.topology.local_threshold_ms

  *类型*: **integer**

  *默认值*: 1000



MongoDB 拓扑参数，选择用来处理用户请求的 Secondary 节点的策略。记到所有节点的 RTT 中的最小值为 LowestRTT，那么只有那些 RTT < LowestRTT + local_threshold_ms 的 Secondary 节点会被选择。


##### auth.mongo.topology.connect_timeout_ms

  *类型*: **integer**

  *默认值*: 20000



MongoDB 拓扑参数，MongoDB 连接超时时间，单位: 毫秒。


##### auth.mongo.topology.socket_timeout_ms

  *类型*: **integer**

  *默认值*: 100



MongoDB 拓扑参数，MongoDB 消息发送超时时间，单位: 毫秒。


##### auth.mongo.topology.server_selection_timeout_ms

  *类型*: **integer**

  *默认值*: 30000



MongoDB 拓扑参数，选择 MongoDB Server 的超时时间，单位: 毫秒。


##### auth.mongo.topology.wait_queue_timeout_ms

  *类型*: **integer**

  *默认值*: 1000



MongoDB 拓扑参数，从线程池中选取 worker 的等待超时时间，单位: 毫秒。


##### auth.mongo.topology.heartbeat_frequency_ms

  *类型*: **integer**

  *默认值*: 10000



MongoDB 拓扑参数，拓扑扫描之间的间隔时间，单位: 毫秒。


##### auth.mongo.topology.min_heartbeat_frequency_ms

  *类型*: **integer**

  *默认值*: 1000



MongoDB 拓扑参数，`heartbeat_frequency_ms` 允许的最小值，单位: 毫秒。


## 插件 `emqx_auth_mysql`

##### auth.mysql.server

  *类型*: **ip**

  *默认值*: `127.0.0.1:3306`



MySQL 服务器地址。


##### auth.mysql.pool

  *类型*: **integer**

  *默认值*: 8



数据库连接线程池大小。


##### auth.mysql.username

  *类型*: **string**

  *默认值*: -



MySQL 用户名。


##### auth.mysql.password

  *类型*: **string**

  *默认值*: -



MySQL 密码。


##### auth.mysql.database

  *类型*: **string**

  *默认值*: `mqtt`



MySQL 数据库名称。


##### auth.mysql.query_timeout

  *类型*: **duration**

  *默认值*: `5s`



MySQL 数据查询超时时间。查询超时将等同于未找到用户数据处理。

<br >

##### auth.mysql.auth_query

  *类型*: **string**

  *默认值*: `select password from mqtt_user where username = '%u' limit 1`



认证时使用的 MySQL 选取语句，选取出来的数据将与经过由 `auth.mysql.password_hash` 指定的加密方式加密的密码进行比较，比较后内容一致的客户端将被允许登录。加盐后存储的密码需要同时选取盐对应的字段，例如 `select password, salt from mqtt_user where username = '%u' limit 1`。`password` 与 `salt` 字段名不可以修改，表名与 WHERE 子句中的字段名可以视情况变化。WHERE 子句支持以下占位符：

| 占位符 | 说明                                                      |
| ------ | --------------------------------------------------------- |
| `%u`   | 将被替换为 MQTT 客户端在 CONNECT 报文中指定的用户名       |
| `%c`   | 将被替换为 MQTT 客户端在 CONNECT 报文中指定的客户端标识符 |
| `%C`   | 将被替换为 TLS 连接时客户端证书中的 Common Name           |
| `%d`   | 将被替换为 TLS 连接时客户端证书中的 Subject               |


##### auth.mysql.password_hash

  *类型*: **string**

  *默认值*: `sh256`



存储在数据库的密码所使用的加密方式。支持以下加密方式：

- `plain`，支持前后加盐，例如 `salt,plain`
- `md5`，支持前后加盐
- `sha`，支持前后加盐
- `sha256`，支持前后加盐
- `sha512`，支持前后加盐
- `pbkdf2`，格式为 `pbkdf2,<Hashfun>,<Iterations>,<Dklen>`。其中，`<Hashfun>` 为使用的哈希函数，支持 `md4`，`md5`，`ripemd160`，`sha`，`sha224`，`sha256`，`sha384`，`sha512`，`<Iterations>` 为迭代次数，`<Dklen>` 为导出密钥长度。示例：`pbkdf2,sha256,1000,20`
- `bcrypt`，仅支持前向加盐，例如 `salt,bcrypt`


##### auth.mysql.super_query

  *类型*: **string**

  *默认值*: `select is_superuser from mqtt_user where username = '%u' limit 1`



超级用户认证时使用的 SQL 选取语句，此语句中所有表名与字段名都可视情况修改，当且仅当选取得到字段的值为 `1` 时，该用户为超级用户。WHERE 子句中支持的占位符与 `auth.mysql.auth_query` 相同。


##### auth.mysql.acl_query

  *类型*: **string**

  *默认值*: `select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'`



ACL 校验时使用的 SQL 选取语句，此语句中所有表名与字段名都可视情况修改。WHERE 子句中支持的占位符如下：

| 占位符 | 说明                                                      |
| ------ | --------------------------------------------------------- |
| `%a`   | 将被替换为客户端 IP 地址                                  |
| `%u`   | 将被替换为 MQTT 客户端在 CONNECT 报文中指定的用户名       |
| `%c`   | 将被替换为 MQTT 客户端在 CONNECT 报文中指定的客户端标识符 |


## 插件 `emqx_auth_pgsql`

##### auth.pgsql.server

  *类型*: **ip**

  *默认值*: `127.0.0.1:5432`



PostgreSQL 服务器地址。


##### auth.pgsql.pool

  *类型*: **integer**

  *默认值*: 8



数据库连接线程池大小。


##### auth.pgsql.username

  *类型*: **string**

  *默认值*: `root`



PostgreSQL 用户名。


##### auth.pgsql.password

  *类型*: **string**

  *默认值*: -



PostgreSQL 密码。


##### auth.pgsql.database

  *类型*: **string**

  *默认值*: `mqtt`



PostgreSQL 数据库名称。


##### auth.pgsql.encoding

  *类型*: **string**

  *默认值*: `utf8`



PostgreSQL 数据库字符编码格式。


##### auth.pgsql.ssl

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否启用 TLS 连接。


##### auth.pgsql.ssl_opts.keyfile

  *类型*: **string**

  *默认值*: -



客户端私钥文件路径。


##### auth.pgsql.ssl_opts.certfile

  *类型*: **string**

  *默认值*: -



客户端证书文件路径。


##### auth.pgsql.ssl_opts.cacertfile

  *类型*: **string**

  *默认值*: -



客户端 CA 证书文件路径。


##### auth.pgsql.auth_query

  *类型*: **string**

  *默认值*: `select password from mqtt_user where username = '%u' limit 1`



认证时使用的 SQL 选取语句，同 `auth.mysql.auth_query`。


##### auth.pgsql.password_hash

  *类型*: **string**

  *默认值*: `sh256`



存储在数据库的密码所使用的加密方式，同 `auth.mysql.password_hash`。


##### auth.pgsql.super_query

  *类型*: **string**

  *默认值*: `select is_superuser from mqtt_user where username = '%u' limit 1`



超级用户认证时使用的 SQL 选取语句，同 `auth.mysql.super_query`。


##### auth.pgsql.acl_query

  *类型*: **string**

  *默认值*: `select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'`



ACL 校验时使用的 SQL 选取语句，同 `auth.mysql.acl_query`。


## 插件 `emqx_auth_redis`

##### auth.redis.type

  *类型*: **enum**

  *可选值*: `single`, `sentinel`, `cluster`

  *默认值*: `single`


Redis 服务集群类型：
- `single`：单节点服务。
- `sentinel`：哨兵模式。
- `cluster`：集群模式。


##### auth.redis.server

  *类型*: **string**

  *默认值*: `127.0.0.1:6379`



Redis 服务地址，如果有多个则以逗号分隔。例如，`192.168.0.1:6379, 192.168.0.2:6379`。


##### auth.redis.sentinel

  *类型*: **string**

  *默认值*: -



Redis sentinel 模式下的集区名称。如果非 `sentinel` 模式，则不需要配置。


##### auth.redis.pool

  *类型*: **integer**

  *可选值*: > 0

  *默认值*: 8


连接池大小。


##### auth.redis.database

  *类型*: **integer**

  *默认值*: 0



要连接的 Redis 数据库序号。


##### auth.redis.password

  *类型*: **string**

  *默认值*: -



Redis 用户密码。


##### auth.redis.query_timeout

  *类型*: **duration**

  *默认值*: `5s`



Redis 查询超时时间。


##### auth.redis.auth_cmd

  *类型*: **string**

  *默认值*: `HMGET mqtt_user:%u password`



认证查询命令，可用站位符有：
 - `%u`：客户端用户名。
 - `%c`：客户端标识。
 - `%C`：客户端 SSL 证书的 `cn`。
 - `%d`：客户端 SSL 证书的 `dn`。


##### auth.redis.password_hash

  *类型*: **enum**

  *可选值*: `plain`, `md5`, `sha`, `sha256`, `bcrypt`

  *默认值*: `plain`


Redis 存储的 `password` 字段的编码格式。


##### auth.redis.super_cmd

  *类型*: **string**

  *默认值*: `HGET mqtt_user:%u is_superuser`



超级用户查询命令，可用的占位符有：
 - `%u`：客户端用户名。
 - `%c`：客户端标识。
 - `%C`：客户端 SSL 证书的 `cn`。
 - `%d`：客户端 SSL 证书的 `dn`。


##### auth.redis.acl_cmd

  *类型*: **string**

  *默认值*: `HGETALL mqtt_acl:%u`



ACL 查询命令。可用的占位符有：
 - `%u`：客户端用户名。
 - `%c`：客户端标识。


## 插件 `emqx_bridge_mqtt`

##### bridge.mqtt.aws.address

  *类型*: **string**

  *默认值*: `127.0.0.1:1883`



桥接地址，支持两种格式，例如：
- `emqx@192.168.0.100`：EMQX 节点名称，它表示将该节点的消息桥接到另外一个 EMQX 节点。
- `192.168.0.100:1883`：IP 地址和端口，它表示将该节点的消息通过一个 MQTT 连接桥接到另外一个 MQTT 服务器。


##### bridge.mqtt.aws.proto_ver

  *类型*: **enum**

  *可选值*: `mqttv3`, `mqttv4`, `mqttv5`

  *默认值*: `mqttv4`


MQTT 桥接的客户端协议版本。


##### bridge.mqtt.aws.start_type

  *类型*: **eunm**

  *可选值*: `manual`, `auto`

  *默认值*: `manual`


启动类型：
- `auto`：跟随插件自动启动。
- `manual`：手动启动桥接。


##### bridge.mqtt.aws.bridge_mode

  *类型*: **boolean**

  *可选值*: `true`, `false`

  *默认值*: `true`


是否开启桥接模式，仅 MQTT 桥接支持。开启后 `emqx_bridge_mqtt` 启动的 MQTT 客户端在发送连接报文时会携带一个标志位，标识这是一个桥接客户端。

注：RabbitMQ 目前不支持该标志。


##### bridge.mqtt.aws.clientid

  *类型*: **string**

  *默认值*: `bridge_aws`



MQTT 桥接的客户端标识。


##### bridge.mqtt.aws.clean_start

  *类型*: **boolean**

  *可选值*: `true`, `false`

  *默认值*: `true`


MQTT 桥接的 `clean_start` 标志。它表示客户端是否以 `清楚会话` 的方式连接到远程 MQTT Broker。


##### bridge.mqtt.aws.username

  *类型*: **string**

  *默认值*: `user`



MQTT 桥接客户端的用户名。


##### bridge.mqtt.aws.password

  *类型*: **string**

  *默认值*: `passwd`



MQTT 桥接客户端的密码。


##### bridge.mqtt.aws.forwards

  *类型*: **string**

  *默认值*: `topic1/#,topic2/#`



桥接转发规则。例如：
- `topic1/#, topic2/#`：`emqx_bridge_mqtt` 会将 EMQX 中所以与 `topic1/#`，`topic2/#` 匹配的主题消息进行转发。


##### bridge.mqtt.aws.forward_mountpoint

  *类型*: **string**

  *默认值*: `bridge/aws/${node}/`



转发主题的前缀。将消息转发到目标系统时，支持给该主题添加一个统一的前缀。


##### bridge.mqtt.aws.subscription.1.topic

  *类型*: **string**

  *默认值*: -



订阅对端系统的主题。


##### bridge.mqtt.aws.subscription.1.qos

  *类型*: **enum**

  *可选值*: `0`, `1`, `2`

  *默认值*: `1`


订阅对端系统主题的 QoS。


##### bridge.mqtt.aws.receive_mountpoint

  *类型*: **string**

  *默认值*: `receive/aws/`



接收消息的主题前缀。`emqx_bridge_mqtt` 支持给来着对端的消息添加一个统一的主题前缀。


##### bridge.mqtt.aws.ssl

  *类型*: **boolean**

  *可选值*: `true`, `false`

  *默认值*: `true`


MQTT 桥接客户端是否开启 SSL。


##### bridge.mqtt.aws.cacertfile

  *类型*: **string**

  *默认值*: `etc/certs/cacert.pem`



MQTT 桥接客户端的 CA 证书文件路径。


##### bridge.mqtt.aws.certfile

  *类型*: **string**

  *默认值*: `etc/certs/client-cert.pem`



MQTT 桥接客户端的 SSL 证书文件路径。


##### bridge.mqtt.aws.keyfile

  *类型*: **string**

  *默认值*: `etc/certs/client-key.pem`



MQTT 桥接客户端的 SSL 秘钥文件路径。


##### bridge.mqtt.aws.ciphers

  *类型*: **string**

  *默认值*: `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384`



SSL 握手支持的加密套件。


##### bridge.mqtt.aws.psk_ciphers

  *类型*: **string**

  *默认值*: `PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA`



SSL PSK 握手支持的加密套件。


##### bridge.mqtt.aws.keepalive

  *类型*: **duration**

  *默认值*: `60s`



MQTT 桥接客户端的心跳间隔。


##### bridge.mqtt.aws.tls_versions

  *类型*: **string**

  *默认值*: `tlsv1.3,tlsv1.2,tlsv1.1,tlsv1`



MQTT 桥接客户端的 SSL 版本。


##### bridge.mqtt.aws.reconnect_interval

  *类型*: **duration**

  *默认值*: `30s`



重连间隔。


##### bridge.mqtt.aws.retry_interval

  *类型*: **duration**

  *默认值*: `20s`



QoS 1/2 消息重发间隔。


##### bridge.mqtt.aws.batch_size

  *类型*: **integer**

  *默认值*: 32



EMQX 桥接的批处理大小。`emqx_bridge_mqtt` 的 EMQX 桥接模式支持批量发送消息以提搞吞吐。


##### bridge.mqtt.aws.max_inflight_size

  *类型*: **integer**

  *默认值*: 32



飞行窗口大小。


##### bridge.mqtt.aws.queue.replayq_dir

  *类型*: **string**

  *默认值*: `etc/emqx_aws_bridge/`



设置消息队列文件路径。不配置则仅使用内存存储。


##### bridge.mqtt.aws.queue.replayq_seg_bytes

  *类型*: **bytesize**

  *默认值*: `10MB`



消息队列存储在磁盘的单个文件大小。


##### bridge.mqtt.aws.queue.max_total_size

  *类型*: **bytesize**

  *默认值*: `5GB`



消息队列允许存储的最大值。


## 插件 `emqx_coap`

##### coap.port

  *类型*: **integer**

  *默认值*: 5683



指定 CoAP 插件的 UDP 绑定端口。


##### coap.enable_stats

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


启用或关闭 CoAP 的统计功能。


##### coap.dtls.port

  *类型*: **integer**

  *默认值*: 5684



指定 CoAP 插件的 DTLS 绑定端口。


##### coap.dtls.verify

  *类型*: **enum**

  *可选值*: `verify_peer`, `verify_none`

  *默认值*: `verify_peer`


使用 DTLS 时，指定 DTLS 握手过程中是否校验客户端。


##### coap.dtls.keyfile

  *类型*: **string**

  *默认值*: `etc/certs/key.pem`



使用 DTLS 时，指定 DTLS 的私钥文件。


##### coap.dtls.certfile

  *类型*: **string**

  *默认值*: `etc/certs/cert.pem`



使用 DTLS 时，指定 DTLS 的证书文件。


##### coap.dtls.cacertfile

  *类型*: **string**

  *默认值*: `etc/certs/cacert.pem`



使用 DTLS 时，指定 DTLS 的 CA 证书文件。


##### coap.dtls.fail_if_no_peer_cert

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


使用 DTLS 时，DTLS 握手过程中若客户端没有证书，是否让握手失败。


##### coap.dtls.ciphers

  *类型*: **string**

  *默认值*: `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA`



使用 DTLS 时，指定 DTLS 服务端支持的 Cipher 列表。


## 插件 `emqx_dashboard`

##### dashboard.default_user.login` & `dashboard.default_user.password

  *类型*: **string**

  *默认值*: -



Dashboard 默认用户的认证数据。`dashboard.default_user.login` 与 `dashboard.default_user.password` 必须同时存在。


##### dashboard.listener.http

  *类型*: **integer**

  *默认值*: 18083


| string  | 0.0.0.0:18083 |


HTTP 监听器的监听端口。
</br>
使用 `ip:port` 监听指定网卡端口。默认 `0.0.0.0:18083` 会监听所有网卡的 18083 端口。


##### dashboard.listener.http.acceptors

  *类型*: **integer**

  *默认值*: 4



此监听器将创建的监听进程数量。


##### dashboard.listener.http.max_clients

  *类型*: **integer**

  *默认值*: 512



此监听器允许同时建立的最大连接数量限制。


##### dashboard.listener.http.inet6

  *类型*: **enum**

  *可选值*: `ture`, `false`

  *默认值*: `false`


是否设置套接字允许 IPv6 连接。


##### dashboard.listener.http.ipv6_v6only

  *类型*: **enum**

  *可选值*: `ture`, `false`

  *默认值*: `false`


是否限制套接字仅使用 IPv6，禁止任何 IPv4 连接。仅适用于 IPv6 套接字，即仅在 `dashboard.listener.http.inet6` 被设置为 `true` 时此配置项的值有实际意义。需要注意的是，在某些操作系统上，例如 Windows，此配置项唯一允许的值为 `true`。


##### dashboard.listener.https

  *类型*: **integer**

  *默认值*: 18084



HTTPS 监听器的监听端口，**默认此监听器被禁用**。


##### dashboard.listener.https.acceptors

  *类型*: **integer**

  *默认值*: 2



同 `dashboard.listener.http.acceptors`。


##### dashboard.listener.https.max_clients

  *类型*: **integer**

  *默认值*: 512



同 `dashboard.listener.http.max_clients`。


##### dashboard.listener.https.inet6

  *类型*: **enum**

  *可选值*: `ture`, `false`

  *默认值*: `false`


同 `dashboard.listener.http.inet6`。


##### dashboard.listener.https.ipv6_v6only

  *类型*: **enum**

  *可选值*: `ture`, `false`

  *默认值*: `false`


同 `dashboard.listener.http.ipv6_v6only`。


##### dashboard.listener.https.keyfile

  *类型*: **string**

  *默认值*: `etc/certs/key.pem`



服务端私钥文件路径。


##### dashboard.listener.https.certfile

  *类型*: **string**

  *默认值*: `etc/certs/cert.pem`



服务端证书文件路径。


##### dashboard.listener.https.cacertfile

  *类型*: **string**

  *默认值*: `etc/certs/cacert.pem`



指定 SSL 的 CA 证书文件 (PEM)。该文件应包含发布服务器证书的所有中间CA证书以及根证书。
该文件还应包含所有受信CA的证书用以用于验证客户端的证书。



##### dashboard.listener.https.dhfile

  *类型*: **string**

  *默认值*: `etc/certs/dh-params.pem`



如果协商使用 Diffie Hellman 密钥交换的密码套件，则可以通过此配置项指定包含 PEM 编码的 Diffie Hellman 参数的文件路径。 如果未指定，则使用默认参数。


##### dashboard.listener.https.verify

  *类型*: **enum**

  *可选值*: `verify_peer`, `verify_none`

  *默认值*: `verify_peer`


`verify_none` 表示关闭对端证书验证，服务端不会向客户端发出证书请求。`verify_peer` 表示开启对端证书验证，服务端会向客户端发出证书请求。当此配置项被设置为 `verify_peer` 时，通常需要配合 `dashboard.listener.https.fail_if_no_peer_cert` 一起使用，以指定是否强制客户端提供证书。


##### dashboard.listener.https.fail_if_no_peer_cert

  *类型*: **enum**

  *可选值*: `ture`, `false`

  *默认值*: `true`


必须配合 `dashboard.listener.https.verify` 一起使用。如果设置为 `true`，则服务端向客户端请求证书时如果客户端不提供证书将导致握手失败。如果设置为 `false`，则客户端即使不提供证书也能握手成功。


##### dashboard.listener.https.tls_versions

  *类型*: **string**

  *默认值*: `tlsv1.3,tlsv1.2,tlsv1.1,tlsv1`



指定服务端支持的 TLS 协议版本，版本之间由 `,` 分隔，支持的 TLS 协议版本有： `tlsv1.3`, `tlsv1.2`, `tlsv1.1`, `tlsv1`, `sslv3`。


##### dashboard.listener.https.ciphers

  *类型*: **string**

  *默认值*: `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA`



指定服务端支持的加密套件。


##### dashboard.listener.https.secure_renegotiate

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


指定是否启动安全重协商机制。


##### dashboard.listener.https.reuse_sessions

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


指定是否启用会话复用机制。


##### dashboard.listener.https.honor_cipher_order

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


如果设置为 `on`，则使用服务器的首选项进行密码选择。 如果设置为 `off`，则使用客户端的首选项。


## 插件 `emqx_lwm2m`

##### lwm2m.port

  *类型*: **integer**

  *默认值*: 5683



指定 LwM2M 使用的 UDP 端口。


##### lwm2m.lifetime_min

  *类型*: **duration**

  *默认值*: `1s`



指定允许的 LwM2M lifetime 最小值，单位: 秒。


##### lwm2m.lifetime_max

  *类型*: **duration**

  *默认值*: `86400s`



指定允许的 LwM2M lifetime 最大值，单位: 秒。


##### lwm2m.qmode_time_window

  *类型*: **integer**

  *默认值*: 22



指定 LwM2M Q 模式使用的窗口大小，单位: 秒。


这个窗口期之内可以下发执行给 Q 模式的设备，过了窗口期则缓存下行数据。

##### lwm2m.lb

  *类型*: **enum**

  *可选值*: `coaproxy`, `undefined`

  *默认值*: `undefined`


设置是否使用 coaproxy。设置为 `undefined` 则不使用 coaproxy。


##### lwm2m.auto_observe

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


在设备注册后是否自动下发 observe 命令。


##### lwm2m.mountpoint

  *类型*: **string**

  *默认值*: `lwm2m/%e/`



设置 LwM2M 主题的挂载点。支持以下通配符:

- '%e': Endpoint Name
- '%a': IP Address


##### lwm2m.topics.command

  *类型*: **string**

  *默认值*: `dn/#`



设备注册完成后，需要订阅的下行命令主题。


##### lwm2m.topics.response

  *类型*: **string**

  *默认值*: `up/resp`



设备的上行回复需要发布到哪个主题。


##### lwm2m.topics.notify

  *类型*: **string**

  *默认值*: `up/notify`



设备的上行报告消息 (notify) 需要发布到哪个主题。


##### lwm2m.topics.register

  *类型*: **string**

  *默认值*: `up/resp`



设备的上行注册消息 (register) 需要发布到哪个主题。


##### lwm2m.topics.update

  *类型*: **string**

  *默认值*: `up/resp`




##### lwm2m.update_msg_publish_condition

  *类型*: **enum**

  *可选值*: `contains_object_list`, `always`

  *默认值*: `contains_object_list`



发布 UPDATE 事件的条件。可以为下列两种之一：

- contains_object_list: 仅当 UPDATE 消息包含 `object list` 时发布

- always: 总是发布


设备的上行更新消息 (update) 需要发布到哪个主题。


##### lwm2m.opts.buffer

  *类型*: **bytesize**

  *默认值*: `1024KB`



UDP 调优参数，指定 UDP 用户态缓存大小。


##### lwm2m.opts.recbuf

  *类型*: **bytesize**

  *默认值*: `1024KB`



UDP 调优参数，指定 UDP 接收缓存大小。


##### lwm2m.opts.sndbuf

  *类型*: **bytesize**

  *默认值*: `1024KB`



UDP 调优参数，指定 UDP 发送缓存大小。


##### lwm2m.opts.read_packets

  *类型*: **integer**

  *默认值*: 20



UDP 调优参数，指定每次从 UDP socket 读取多少个报文。


##### lwm2m.certfile

  *类型*: **string**

  *默认值*: `etc/certs/cert.pem`



指定 UDP DTLS 使用的证书文件。


##### lwm2m.keyfile

  *类型*: **string**

  *默认值*: `etc/certs/key.pem`



指定 UDP DTLS 使用的私钥文件。


##### lwm2m.xml_dir

  *类型*: **dir**

  *默认值*: `etc/lwm2m_xml`



指定 LwM2M Object 定义文件存放的目录。


## 插件 `emqx_management`

##### management.max_row_limit

  *类型*: **integer**

  *默认值*: 10000



分页查询时返回的最大记录数量。


##### management.default_application.id

  *类型*: **string**

  *默认值*: `admin`



默认应用的 AppId。


##### management.default_application.secret

  *类型*: **string**

  *默认值*: `public`



默认应用的 AppSecret。


##### management.listener.http

  *类型*: **integer**

  *默认值*: 8081


| string  | 0.0.0.0:8081 |

HTTP 监听器的监听端口。
</br>
使用 `ip:port` 监听指定网卡端口。默认 `0.0.0.0:8081` 会监听所有网卡的 8081 端口。


##### management.listener.http.acceptors

  *类型*: **integer**

  *默认值*: 2



此监听器将创建的监听进程数量。


##### management.listener.http.max_clients

  *类型*: **integer**

  *默认值*: 512



此监听器允许同时建立的最大连接数量限制。


##### management.listener.http.backlog

  *类型*: **integer**

  *默认值*: 512



TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。


##### management.listener.http.send_timeout

  *类型*: **duration**

  *默认值*: `15s`



HTTP 报文发送超时时间。


##### management.listener.http.send_timeout_close

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


HTTP 报文发送超时后，是否关闭该连接。


##### management.listener.http.inet6

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否设置套接字允许 IPv6 连接。


##### management.listener.http.ipv6_v6only

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否限制套接字仅使用 IPv6，禁止任何 IPv4 连接。仅适用于 IPv6 套接字，即仅在 `management.listener.http.inet6` 被设置为 `true` 时此配置项的值有实际意义。需要注意的是，在某些操作系统上，例如 Windows，此配置项唯一允许的值为 `true`。


##### management.listener.https

  *类型*: **integer**

  *可选值*: -

  *默认值*: 8081

| string  | -       | 0.0.0.0:8081 |


HTTPS 监听器的监听端口。
</br>
使用 `ip:port` 监听指定网卡端口。默认 `0.0.0.0:8081` 会监听所有网卡的 8081 端口。

##### management.listener.https.acceptors

  *类型*: **integer**

  *默认值*: 2



此监听器将创建的监听进程数量。


##### management.listener.https.max_clients

  *类型*: **integer**

  *默认值*: 512



此监听器允许同时建立的最大连接数量限制。


##### management.listener.https.backlog

  *类型*: **integer**

  *默认值*: 512



TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。


##### management.listener.https.send_timeout

  *类型*: **duration**

  *默认值*: `15s`



HTTPS 报文发送超时时间。


##### management.listener.https.send_timeout_close

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


HTTPS 报文发送超时后，是否关闭该连接。


##### management.listener.https.keyfile

  *类型*: **string**

  *默认值*: `etc/certs/key.pem`



服务端私钥文件路径。


##### management.listener.https.certfile

  *类型*: **string**

  *默认值*: `etc/certs/cert.pem`



服务端证书文件路径。


##### management.listener.https.cacertfile

  *类型*: **string**

  *默认值*: `etc/certs/cacert.pem`



指定 SSL 的 CA 证书文件 (PEM)。该文件应包含发布服务器证书的所有中间CA证书以及根证书。
该文件还应包含所有受信CA的证书用以用于验证客户端的证书。



##### management.listener.https.verify

  *类型*: **enum**

  *可选值*: `verify_peer`, `verify_none`

  *默认值*: `verify_peer`


`verify_none` 表示关闭对端证书验证，服务端不会向客户端发出证书请求。`verify_peer` 表示开启对端证书验证，服务端会向客户端发出证书请求。当此配置项被设置为 `verify_peer` 时，通常需要配合 `management.listener.https.fail_if_no_peer_cert` 一起使用，以指定是否强制客户端提供证书。


##### management.listener.https.fail_if_no_peer_cert

  *类型*: **enum**

  *可选值*: `ture`, `false`

  *默认值*: `true`


必须配合 `management.listener.https.verify` 一起使用。如果设置为 `true`，则服务端向客户端请求证书时如果客户端不提供证书将导致握手失败。如果设置为 `false`，则客户端即使不提供证书也能握手成功。


##### management.listener.https.inet6

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否设置套接字允许 IPv6 连接。


##### management.listener.https.ipv6_v6only

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


是否限制套接字仅使用 IPv6，禁止任何 IPv4 连接。仅适用于 IPv6 套接字，即仅在 `management.listener.https.inet6` 被设置为 `true` 时此配置项的值有实际意义。需要注意的是，在某些操作系统上，例如 Windows，此配置项唯一允许的值为 `true`。


## 插件`emqx_retainer`

##### retainer.storage_type

  *类型*: **enum**

  *可选值*: `ram`, `disc`, `disc_only`

  *默认值*: `ram`


保留消息的存储类型，以下选项可用：

`ram`

保留消息仅存储在内存中。

`disc`

保留消息同时存储在内存和磁盘中。

`disc_only`

保留消息仅存储在磁盘中。


##### retainer.max_retained_messages

  *类型*: **integer**

  *默认值*: 0



保留消息的存储数量限制。一旦存储数量达到限制，可以替换已存在的保留消息，但不能为新的主题存储保留消息。0 表示没有限制。


##### retainer.max_payload_size

  *类型*: **bytesize**

  *默认值*: `1MB`



允许存储的保留消息的 Payload 最大长度限制。如果 Payload 超出最大限制，该保留消息可以被正常处理，但不会存储在服务端。


##### retainer.expiry_interval

  *类型*: **duration**

  *默认值*: `0`



保留消息的过期间隔，仅对协议版本低于 MQTT v5.0 的客户端生效，MQTT v5.0 客户端的保留消息过期间隔将以 `Message Expiry Interval` 的值为准。0 表示永不过期。


## 插件`emqx_rule_engine`

##### rule-engine.ignore_sys_message

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


忽略系统消息 ($SYS)。启用此选项规则引擎将不会处理系统消息。


##### rule-engine.events.\<event-name>

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


设置是否发布事件消息。可指定事件消息的 QoS，例如:

```
rule-engine.events.client_connected = on, qos1

```

若启用此选项，规则引擎会将系统消息使用 `$events/<event-name>` 主题发布出来。可支持的 `<event-name>` 有:

- client_connected: 客户端登录完成
- client_disconnected: 客户端下线
- session_subscribed: 客户端订阅
- session_unsubscribed: 客户端取消订阅
- message_delivered: 消息已投递
- message_acked: 消息已确认
- message_dropped: 消息被丢弃

如果禁用此选项，事件消息将不会发布，但事件规则仍然可以使用。例如，即使 `rule_engine.events.client_connected = off`，以下规则仍然可以使用:

```SQl
SELECT * FROM "$events/client_connected"
```


## 插件 `emqx_sn`

##### mqtt.sn.port

  *类型*: **string**

  *默认值*: `1884`



`emqx_sn` 监听的 UDP 端口。


##### mqtt.sn.advertise_duration

  *类型*: **duration**

  *默认值*: `15s`



ADVERTISE 消息广播间隔，单位：秒。


##### mqtt.sn.gateway_id

  *类型*: **integer**

  *默认值*: 1



ADVERTISE 中的 MQTT-SN 网关 ID。


##### mqtt.sn.enable_stats

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


是否开启客户端状态统计信息。


##### mqtt.sn.enable_qos3

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


是否处理 QoS 为 -1 的消息。


##### mqtt.sn.idle_timeout

  *类型*: **duration**

  *默认值*: `30s`



建立后的发呆时间，如果这段时间内未收到任何报文，则会关闭该连接。


##### mqtt.sn.predefined.topic.0

  *类型*: **string**

  *默认值*: `reserved`



预定义的 Topic 与 TopicId 映射。Id 为 0 的主题是保留项，固定为 `reserved`。例如，预定义主题 `foo/bar` 的 Id 为 `1`：
```
mqtt.sn.predefined.topic.1 = foo/bar
```


##### mqtt.sn.username

  *类型*: **string**

  *默认值*: `mqtt_sn_user`



`emqx_sn` 连接至 EMQX 的用户名。


##### mqtt.sn.password

  *类型*: **string**

  *默认值*: `abc`



`emqx_sn` 连接至 EMQX 的密码。


## 插件 `emqx_prometheus`

##### prometheus.push.gateway.server

  *类型*: **string**

  *默认值*: `http://127.0.0.1:9091`



指定 Prometheus gateway 的 URI。


##### prometheus.interval

  *类型*: **integer**

  *默认值*: 15000



指定 Stats 数据的收集间隔，单位: 毫秒。


##### prometheus.collector.\<N>

  *类型*: **string**

  *默认值*: `emqx_prometheus`



指定 Prometheus 的 Collector。


## 插件 `emqx_stomp`

##### stomp.listener

  *类型*: **integer**

  *默认值*: 61613



指定 Stomp 插件监听的本地端口。


##### stomp.listener.acceptors

  *类型*: **integer**

  *默认值*: 4



指定 Stomp 服务 Acceptor 线程池的大小。


##### stomp.listener.max_connections

  *类型*: **integer**

  *默认值*: 512



指定 Stomp 服务支持的最大连接数。


##### stomp.listener.ssl

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


指定是否使用 SSL。


##### stomp.listener.keyfile

  *类型*: **string**

  *默认值*: `etc/certs/key.pem`



若使用 SSL，指定 SSL 的私钥文件。


##### stomp.listener.certfile

  *类型*: **string**

  *默认值*: `etc/certs/cert.pem`



若使用 SSL，指定 SSL 的证书文件。


##### stomp.listener.cacertfile

  *类型*: **string**

  *默认值*: `etc/certs/cacert.pem`



若使用 SSL，指定 SSL 的 CA 证书文件。


##### stomp.listener.dhfile

  *类型*: **string**

  *默认值*: `etc/certs/dh-params.pem`



若使用 SSL，指定 Ephemeral Diffie-Helman 算法使用的 key 文件。


##### stomp.listener.verify

  *类型*: **enum**

  *可选值*: `verify_peer`, `verify_none`

  *默认值*: `verify_peer`


若使用 SSL，指定握手过程中是否校验客户端。


##### stomp.listener.fail_if_no_peer_cert

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


若使用 SSL，SSL 握手过程中若客户端没有证书，是否让握手失败。


##### stomp.listener.tls_versions

  *类型*: **string**

  *默认值*: `tlsv1.2,tlsv1.1,tlsv1`



若使用 SSL，指定服务端支持的 SSL 的版本列表。


##### stomp.listener.handshake_timeout

  *类型*: **duration**

  *默认值*: `15s`



若使用 SSL，指定 SSL 握手过程的超时时间。


##### stomp.listener.ciphers

  *类型*: **string**

  *默认值*: `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA`



若使用 SSL，指定服务端支持的 Cipher 列表。


##### stomp.listener.secure_renegotiate

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `off`


若使用 SSL，指定在客户端不遵循 RFC 5746 的情况下，是否拒绝 renegotiation 请求。


##### stomp.listener.reuse_sessions

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


若使用 SSL，指定是否支持 SSL session 重用。


##### stomp.listener.honor_cipher_order

  *类型*: **enum**

  *可选值*: `on`, `off`

  *默认值*: `on`


若使用 SSL，指定是否使用服务端的偏好设置选择 Ciphers。


##### stomp.default_user.login

  *类型*: **string**

  *默认值*: `guest`



指定 Stomp 插件登录使用的 Username。


##### stomp.default_user.passcode

  *类型*: **string**

  *默认值*: `guest`



指定 Stomp 插件登录使用的 Password。


##### stomp.allow_anonymous

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `true`


是否允许匿名登录。


##### stomp.frame.max_headers

  *类型*: **integer**

  *默认值*: 10



指定 Stomp 最大报文头数量。


##### stomp.frame.max_header_length

  *类型*: **integer**

  *默认值*: 1024



指定 Stomp 最大报文头长度。


##### stomp.frame.max_body_length

  *类型*: **integer**

  *默认值*: 8192



指定 Stomp 最大报文体长度。


## 插件 `emqx_web_hook`

##### web.hook.url

  *类型*: **string**

  *默认值*: http://127.0.0.1:80



Webhook 请求转发的目的 Web 服务器地址。


##### web.hook.headers.\<Any\>

```
web.hook.headers.content-type = application/json
web.hook.headers.accept = */*
```

指定 HTTP 请求头部中的数据。`<Key>` 指定 HTTP 请求头部中的字段名，此配置项的值为相应的字段值。`<Key>` 可以是标准的 HTTP 请求头部字段，也可以自定义的字段，可以配置多个不同的请求头部字段。


##### web.hook.encoding_of_payload_field

  *类型*: **enum**

  *可选值*: `plain`, `base62`, `base64`

  *默认值*: `plain`


PUBLISH 报文中 Payload 字段的编码格式。


##### web.hook.ssl.cacertfile

  *类型*: **string**

  *默认值*: -



CA 证书文件路径。


##### web.hook.ssl.certfile

  *类型*: **string**

  *默认值*: -



客户端证书文件路径。


##### web.hook.ssl.keyfile

  *类型*: **string**

  *默认值*: -



客户端私钥文件路径。


##### web.hook.ssl.verify

  *类型*: **enum**

  *可选值*: `true`, `false`

  *默认值*: `false`


指定是否校验对端证书。


##### web.hook.ssl.pool_size

  *类型*: **integer**

  *默认值*: 32



HTTP 连接进程池大小。


##### web.hook.rule.client.connect.1

  *类型*: **string**

  *默认值*: `{"action": "on_client_connect"}`



转发 `收到连接报文` 事件。


##### web.hook.rule.client.connack.1

  *类型*: **string**

  *默认值*: `{"action": "on_client_connack"}`



转发 `下发连接应答` 事件。


##### web.hook.rule.client.connected.1

  *类型*: **string**

  *默认值*: `{"action": "on_client_connected"}`



转发 `客户端成功接入` 事件。


##### web.hook.rule.client.disconnected.1

  *类型*: **string**

  *默认值*: `{"action": "on_client_disconnected"}`



转发 `客户端已断开` 事件。


##### web.hook.rule.client.subscribe.1

  *类型*: **string**

  *默认值*: `{"action": "on_client_subscribe"}`



转发 `将订阅` 事件。


##### web.hook.rule.client.unsubscribe.1

  *类型*: **string**

  *默认值*: `{"action": "on_client_unsubscribe"}`



转发 `将取消订阅` 事件。


##### web.hook.rule.session.subscribed.1

  *类型*: **string**

  *默认值*: `{"action": "on_session_subscribed"}`



转发 `已订阅` 事件。


##### web.hook.rule.session.unsubscribed.1

  *类型*: **string**

  *默认值*: `{"action": "on_session_unsubscribed"}`



转发 `已取消订阅` 事件。


##### web.hook.rule.session.terminated.1

  *类型*: **string**

  *默认值*: `{"action": "on_session_terminated"}`



转发 `会话已终止` 事件。


##### web.hook.rule.message.publish.1

  *类型*: **string**

  *默认值*: `{"action": "on_message_publish"}`



转发 `消息发布` 事件。


##### web.hook.rule.message.delivered.1

  *类型*: **string**

  *默认值*: `{"action": "on_message_delivered"}`



转发 `消息已投递` 事件。


##### web.hook.rule.message.acked.1

  *类型*: **string**

  *默认值*: `{"action": "on_message_acked"}`



转发 `消息已应答` 事件。


{% emqxee %}
##### license.file

  *类型*: **string**

  *默认值*: `etc/emqx.lic`



企业版证书存放的路径。

##### license.connection_high_watermark_alarm

  *类型*: **percent**

  *默认值*: 80%



连接数高水位线告警，达到企业版证书允许实时在线连接数的百分比。超出水位线时会产生告警，不影响实际使用，
- 发生告警后，可以参照[如何更新证书？](../faq/use-guide.md#怎样更新 EMQX license?)进行热更新。
- 当连接数超过最大允许值时，新客户端会被拒绝连接，已连接的客户端不受影响。

##### license.connection_low_watermark_alarm

  *类型*: **percent**

  *默认值*: 75%



连接数低水位线告警，低于达到企业版证书允许实时在线连接数的百分比则解除告警。



{% endemqxee %}
