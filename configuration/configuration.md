---
# 标题
title: 配置项
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
ref: undefined
---

# 配置项


## cluster

### cluster.name

| Type   | Default  |
| ------ | -------- |
| string | `emqxcl` |

##### 说明

集群名称。

<br />

### cluster.proto_dist

| Type | Optional Value                      | Default    |
| ---- | ----------------------------------- | ---------- |
| enum | `inet_tcp`, `inet6_tcp`, `inet_tls` | `inet_tcp` |

##### 说明

分布式 Erlang 集群协议类型。可选值为:

- `inet_tcp`: 使用 IPv4
- `inet6_tcp` 使用 IPv6
- `inet_tls`: 使用 TLS，需要与 `node.ssl_dist_optfile` 配置一起使用。

<br />

### cluster.discovery

| Type | Optional Value                                    | Default  |
| ---- | ------------------------------------------------- | -------- |
| enum | `manual`, `static`, `mcast`, `dns`, `etcd`, `k8s` | `manual` |

##### 说明

集群节点发现方式。可选值为:

- `manual`: 手动加入集群
- `static`: 配置静态节点。配置几个固定的节点，新节点通过连接固定节点中的某一个来加入集群。
- `mcast`: 使用 UDP 多播的方式发现节点。
- `dns`: 使用 DNS A 记录的方式发现节点。
- `etcd`: 使用 etcd 发现节点。
- `k8s`: 使用 Kubernetes 发现节点。

<br />

### cluster.autoheal

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

启用或关闭集群脑裂自动恢复机制。

<br />

### cluster.autoclean

| Type     | Default |
| -------- | ------- |
| duration | `5m`    |

##### 说明

指定多久之后从集群中删除短线节点。

<br />

### cluster.static.seeds

| Type   | Default | Example                                   |
| ------ | ------- | ----------------------------------------- |
| string | -       | `emqx1@192.168.0.100,emqx2@192.168.0.101` |

##### 说明

当使用 static 方式集群时，指定固定的节点列表，多个节点间使用逗号 `,` 分隔。

<br />

### cluster.mcast.addr

| Type   | Default       |
| ------ | ------------- |
| ipaddr | `239.192.0.1` |

##### 说明

当使用 mcast 方式集群时，指定多播地址。

<br />

### cluster.mcast.ports

| Type   | Default |
| ------ | ------- |
| string | `4369`  |

##### 说明

当使用 mcast 方式集群时，指定多播端口。如有多个端口使用逗号 `,` 分隔。

<br />

### cluster.mcast.iface

| Type   | Default   |
| ------ | --------- |
| ipaddr | `0.0.0.0` |

##### 说明

当使用 mcast 方式集群时，指定节点发现服务需要绑定到本地哪个 IP 地址。

<br />

### cluster.mcast.ttl

| Type    | Default |
| ------- | ------- |
| integer | 255     |

##### 说明

当使用 mcast 方式集群时，指定多播的 Time-To-Live 值。

<br />

### cluster.mcast.loop

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    |  `on`, `off`   | `on`    |

##### 说明

当使用 mcast 方式集群时，设置多播的报文是否投递到本地回环地址。

<br />

### cluster.dns.name

| Type   | Default | Example         |
| ------ | ------- | --------------- |
| string | -       | `mycluster.com` |

##### 说明

当使用 dns 方式集群时，指定 DNS A 记录的名字。emqx 会通过访问这个 DNS A 记录来获取 IP 地址列表，然后拼接 `cluster.dns.app` 里指定的 APP 名得到集群中所有节点的列表。

##### 示例

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

<br />

### cluster.dns.app

|  Type  | Default | Example |
| ------ | ------- | ------- |
| string | -       | `emqx`  |

##### 说明

当使用 dns 方式集群时，用来与从 `cluster.dns.name` 获取的 IP 列表拼接得到节点名列表。

<br />

### cluster.etcd.server

|  Type  | Default | Example                 |
| ------ | ------- | ----------------------- |
| string | -       | `http://127.0.0.1:2379` |

##### 说明

当使用 etcd 方式集群时，指定 etcd 服务的地址。如有多个服务使用逗号 `,` 分隔。

<br />

### cluster.etcd.prefix

|  Type  | Default | Example  |
| ------ | ------- | -------- |
| string | -       | `emqxcl` |

##### 说明

当使用 etcd 方式集群时，指定 etcd 路径的前缀。每个节点在 etcd 中都会创建一个路径:

```
v2/keys/<prefix>/<cluster.name>/<node.name>
```

<br />

### cluster.etcd.node_ttl

|   Type   | Default | Example |
| -------- | ------- | ------- |
| duration | -       | `1m`    |

##### 说明

当使用 etcd 方式集群时，指定 etcd 中节点路径的过期时间。

<br />

### cluster.etcd.ssl.keyfile

|   Type   | Default | Example                    |
| -------- | ------- | -------------------------- |
| string   |  -      | `etc/certs/client-key.pem` |

##### 说明

当使用 SSL 连接 etcd 时，指定客户端的私有 Key 文件。

<br />

### cluster.etcd.ssl.certfile

|   Type   | Default | Example                |
| -------- | ------- | ---------------------- |
| string   |  -      | `etc/certs/client.pem` |

##### 说明

当使用 SSL 连接 etcd 时，指定 SSL 客户端的证书文件。

<br />

### cluster.etcd.ssl.cacertfile

|   Type   | Default | Example            |
| -------- | ------- | ------------------ |
| string   |  -      | `etc/certs/ca.pem` |

##### 说明

当使用 SSL 连接 etcd 时，指定 SSL 的 CA 证书文件。

<br />

### cluster.k8s.apiserver

|   Type   | Default | Example                      |
| -------- | ------- | ---------------------------- |
| string   | -       | `http://10.110.111.204:8080` |

##### 说明

当使用 k8s 方式集群时，指定 Kubernetes API Server。如有多个 Server 使用逗号 `,` 分隔。

<br />

### cluster.k8s.service_name

|   Type   | Default | Example |
| -------- | ------- | ------- |
| string   | -       | `emqx`  |

##### 说明

当使用 k8s 方式集群时，指定 Kubernetes 中 EMQ X 的服务名。

<br />

### cluster.k8s.address_type

| Type |  Optional Value         | Default |
| ---- | ----------------------- | ------- |
| enum | `ip`, `dns`, `hostname` | `ip`    |

##### 说明

当使用 k8s 方式集群时，address_type 用来从 Kubernetes 接口的应答里获取什么形式的 Host 列表。

##### 示例

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

<br />

### cluster.k8s.app_name

|   Type   | Default | Example |
| -------- | ------- | ------- |
| string   |  -      | `emqx`  |

##### 说明

当使用 k8s 方式集群时，app_name 用来跟获取的 Host 列表拼接，得到节点列表。

<br />

### cluster.k8s.suffix

|   Type   | Default | Example             |
| -------- | ------- | ------------------- |
| string   | -       | `pod.cluster.local` |

##### 说明

当使用 k8s 方式并且 `cluster.k8s.address_type` 指定为 dns 类型时，可设置 emqx 节点名的后缀。与 `cluster.k8s.namespace` 一起使用用以拼接得到节点名列表。

<br />

### cluster.k8s.namespace

| Type   | Default | Example   |
| ------ | ------- | --------- |
| string | -       | `default` |

##### 说明

当使用 k8s 方式并且 `cluster.k8s.address_type` 指定为 dns 类型时，可设置 emqx 节点名的命名空间。与 `cluster.k8s.suffix` 一起使用用以拼接得到节点名列表。

##### 示例

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

<br />

### node.name

| Type   | Default          |
| ------ | ---------------- |
| string | `emqx@127.0.0.1` |

##### 说明

节点名。格式为 `<name>@<host>`。其中 `<host>` 可以是 IP 地址，也可以是 FQDN。详见 [http://erlang.org/doc/reference_manual/distributed.html](http://erlang.org/doc/reference_manual/distributed.html)。

<br />

### node.cookie

| Type   | Default            |
| ------ | ------------------ |
| string | `emqxsecretcookie` |

##### 说明

分布式 Erlang 集群使用的 cookie 值。

<br />

### node.data_dir

| Type   | Default  |
| ------ | -------- |
| folder | `./data` |

##### 说明

节点的 data 目录，用于存放 Mnesia 数据文件等。

<br />

### node.heartbeat

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    |  `on`, `off`   | `off`   |

##### 说明

系统调优参数，此配置将覆盖 `vm.args` 文件里的 `-heart` 参数。

启用或关闭 Erlang 运行时检测机制，并在运行时终止时自动重启。需小心使用，以免手动关闭 emqx 时被监控进程重新启动。

<br />

### node.async_threads

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 0 - 1024       | 4       |

##### 说明

系统调优参数，此配置将覆盖 `vm.args` 文件里的 `+A` 参数。

设置 Erlang 运行时异步线程池中的线程数量。详情请参见 [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html)。

<br />

### node.process_limit

|   Type   | Optional Value   | Default |
| -------- | ---------------- | ------- |
| integer  | 1024 - 134217727 | 2097152 |

##### 说明

系统调优参数，此配置将覆盖 `vm.args` 文件里的 `+P` 参数。

设置 Erlang 允许的最大进程数，这将影响 emqx 节点能处理的连接数。详情请参见 [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html)。

<br />

### node.max_ports

|   Type   | Optional Value   | Default |
| -------- | ---------------- | ------- |
| integer  | 1024 - 134217727 | 1048576 |

##### 说明

系统调优参数，此配置将覆盖 `vm.args` 文件里的 `+Q` 参数。

设置 Erlang 允许的最大 Ports 数量。详情请参见 [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html)。

<br />

### node.dist_buffer_size

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| bytesize | 1KB - 2GB      | `8MB`   |

##### 说明

系统调优参数，此配置将覆盖 `vm.args` 文件里的 `+zdbbl` 参数。

设置 Erlang 分布式通信使用的最大缓存大小。详情请参见 [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html)。

<br />

### node.max_ets_tables

| Type    | Default |
| ------- | ------- |
| integer | 262144  |

##### 说明

系统调优参数，此配置将覆盖 `vm.args` 文件里的 `+e` 参数。

设置 Erlang 运行时允许的最大 ETS 表数量。详情请参见 [http://erlang.org/doc/man/erl.html](http://erlang.org/doc/man/erl.html)。

<br />

### node.global_gc_interval

| Type     | Default |
| -------- | ------- |
| duration | `15m`   |

##### 说明

系统调优参数，设置 Erlang 运行多久强制进行一次全局垃圾回收。

<br />

### node.fullsweep_after

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 0 - 65535      | 1000    |

##### 说明

系统调优参数，此配置将覆盖 `vm.args` 文件里的 `-env ERL_FULLSWEEP_AFTER` 参数。

设置 Erlang 运行时多少次 generational GC 之后才进行一次 fullsweep GC。详情请参见 [http://erlang.org/doc/man/erlang.html#spawn_opt-4](http://erlang.org/doc/man/erlang.html#spawn_opt-4)。

<br />

### node.crash_dump

| Type    | Default          |
| ------- | ---------------- |
| string  | `log/crash.dump` |

##### 说明

设置 Erlang crash_dump 文件的存储路径和文件名。

<br />

### node.ssl_dist_optfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/ssl_dist.conf` |

##### 说明

此配置将覆盖 `vm.args` 文件里的 `-ssl_dist_optfile` 参数。

如使用 SSL 方式建立 emqx 集群，需指定 SSL 分布式协议的配置文件。需要与 `cluster.proto_dist = inet_tls` 一起使用。

<br />

### node.dist_net_ticktime

| Type    | Default |
| --------| ------- |
| integer | 120     |

##### 说明

系统调优参数，此配置将覆盖 `vm.args` 文件里的 `-kernel net_ticktime` 参数。

当一个节点持续无响应多久之后，认为其已经宕机并断开连接。详情请参见 [http://www.erlang.org/doc/man/kernel_app.html#net_ticktime](http://www.erlang.org/doc/man/kernel_app.html#net_ticktime)。

<br />

### node.dist_listen_min

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1024 - 65535   | 6369    |

##### 说明

与 `node.dist_listen_max` 一起设定一个 TCP 端口段，此端口段用于分配给分布式 Erlang，作为分布式通道的监听端口。注意如果在节点之间设置了防火墙，需要将此端口段放进防火墙的端口白名单里。

<br />

### node.dist_listen_max

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1024 - 65535   | 6369    |

##### 说明

与 `node.dist_listen_min` 一起设定一个 TCP 端口段，此端口段用于分配给分布式 Erlang，作为分布式通道的监听端口。注意如果在节点之间设置了防火墙，需要将此端口段放进防火墙的端口白名单里。

<br />

### rpc.mode

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `sync`, `async` | `async` |

##### 说明

RPC 模式。可选同步或异步模式。

<br />

### rpc.async_batch_size

| Type    | Default |
| ------- | ------- |
| integer | 256     |

##### 说明

异步模式下最大的批量发送消息数。注意此配置在同步模式下不起作用。

<br />

### node.tcp_server_port

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1024 - 65535   | 5369    |

##### 说明

设置 RPC 本地服务使用的监听 port。

<br />

### node.tcp_client_port

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1024-65535     | 5369    |

##### 说明

设置远程 RPC 服务的端口。

<br />

### node.tcp_client_num

| Type    | Optional Value | Default         |
| ------- | -------------- | --------------- |
| integer | 1 - 256        | CPU 核心数 / 2   |

##### 说明

设置由本节点发起，通往每个远程节点的 RPC 通信通道数量。设置为 1 可保证消息顺序。保持默认值（CPU 核心数的一半）可提高 RPC 的吞吐能力。

<br />

### rpc.connect_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### 说明

建立 RPC 连接超时时间。建立连接时若远程节点无响应，多久之后放弃尝试。

<br />

### rpc.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### 说明

发送超时时间。发送消息多久之后放弃。

<br />

### rpc.authentication_timeout

|   Type   | Default |
| -------- | ------- |
| duration | `5s`    |

##### 说明

RPC 认证超时时间。尝试认证若远程节点无响应，多久之后放弃。

<br />

### rpc.call_receive_timeout

|   Type   | Default |
| -------- | ------- |
| duration | `15s`   |

##### 说明

RPC 同步模式的超时时间。RPC 同步调用若收不到回复，用多久之后放弃。

<br />

### rpc.socket_keepalive_idle

|   Type   | Default |
| -------- | ------- |
| duration | `900s`  |

##### 说明

在最近一次数据包发送多久之后，发送 keepalive 探测报文。

<br />

### rpc.socket_keepalive_interval

|   Type   | Default |
| -------- | ------- |
| duration | `75s`   |

##### 说明

发送 keepalive 探测报文的间隔。

<br />

### rpc.socket_keepalive_count

| Type    | Default |
| ------- | ------- |
| integer | 9       |

##### 说明

连续多少次 keepalive 探测报文都收不到回复的情况下，认为 RPC 连接已丢失。

<br />

### rpc.socket_sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | `1MB`   |

##### 说明

TCP 调优参数。TCP 发送缓冲区大小。

<br />

### rpc.socket_recbuf

|   Type   | Default |
| -------- | ------- |
| bytesize | `1MB`   |

##### 说明

TCP 调优参数。TCP 接收缓冲区大小。

<br />

### rpc.socket_buffer

|   Type   | Default |
| -------- | ------- |
| bytesize | `1MB`   |

##### 说明

TCP 调优参数。用户态的 Socket 缓冲区大小。

<br />

### log.to

| Type | Optional Value                   | Default |
| ---- | -------------------------------- | ------- |
| enum | `off`, `file`, `console`, `both` | `both`  |

##### 说明

将日志输出到什么地方。可选值为:

- **off:** 完全关闭日志功能

- **file:** 仅将日志输出到文件

- **console:** 仅将日志输出到标准输出(emqx 控制台)

- **both:** 同时将日志输出到文件和标准输出(emqx 控制台)

<br />

### log.level

| Type | Optional Value                                                                     | Default   |
| ---- | ---------------------------------------------------------------------------------- | --------- |
| enum | `debug`, `info`, `notice`, `warning`<br/>`error`, `critical`, `alert`, `emergency` | `warning` |

##### 说明

全局的日志级别。这包括 primary log level 以及所有的 log handlers。详情请参见 [日志级别和 log handlers](../getting-started/log.md#log-level-and-log-handlers)。

<br />

### log.dir

| Type | Default |
| ---- | ------- |
| dir  | `./log` |

##### 说明

日志文件目录。

<br />

### log.file

| Type   | Default    |
| ------ | ---------- |
| string | `emqx.log` |

##### 说明

日志文件的前缀。例如，若使用默认值 (`log.file = emqx.log`)，日志文件名将为 `emqx.log.1`，`emqx.log.2`，...。

<br />

### log.chars_limit

| Type    | Default |
| ------- | ------- |
| integer | -1      |

##### 说明

设置单个日志消息的最大长度。如超过此长度，日志消息将被截断。`-1` 表示无限制。

<br />

### log.rotation.size

| Type     | Default |
| -------- | ------- |
| bytesize | `10MB`  |

##### 说明

设置单个日志文件大小。如超过此大小，则进行日志文件滚动，创建新的日志文件。

<br />

### log.rotation.count

| Type    | Default |
| ------- | ------- |
| integer | 5       |

##### 说明

设置日志文件总个数。如超过此文件个数，则下一次日志文件滚动将会覆盖第一个文件。

<br />

### log.<level>.file

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

针对某日志级别设置单独的日志文件。

##### 示例

将 info 及 info 以上的日志单独输出到 `info.log.N` 文件中：

```
log.info.file = info.log
```

将 error 及 error 以上的日志单独输出到 `error.log.N` 文件中

```
log.error.file = error.log
```

<br />

### allow_anonymous

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

是否允许匿名用户登录系统。

注：生产环境建议关闭此选项。

<br />

### acl_nomatch

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `allow`, `deny` | `allow` |

##### 说明

ACL 未命中时，允许或者拒绝 发布/订阅 操作。

<br />

### acl_file

| Type   | Default        |
| ------ | -------------- |
| string | `etc/acl.conf` |

##### 说明

默认 ACL 文件的路径。

<br />

### enable_acl_cache

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

是否启用 ACL 缓存。

<br />

### acl_cache_max_size

| Type    | Default  |
| ------- | -------- |
| integer | 32       |

##### 说明

ACL 规则最大缓存条数。

<br />

### acl_cache_ttl

| Type     | Default |
| -------- | ------- |
| duration | `1m`    |

##### 说明

ACL 规则最大缓存时间。

<br />

### acl_deny_action

| Type    | Optional Value         | Default  |
| ------- | ---------------------- | -------- |
| enum    | `ignore`, `disconnect` | `ignore` |

##### 说明

ACL 检查失败后，执行的操作。

- `ignore`：不做任何操作。
- `disconnect`：断开连接。

<br />

### flapping_detect_policy

| Type   | Default      |
| ------ | ------------ |
| string | `30, 1m, 5m` |

##### 说明

指定 `Flapping` 检查策略。

格式：`<threshold>,<duration>,<banned>`。

例如，`30, 1m, 5m`，它表示如果客户端在 1 分钟内断开连接 30 次，那么在后续 5 分钟内禁止登录。

<br />

### mqtt.max_packet_size

| Type      | Default |
| --------- | ------- |
| bytesize  | `1MB`   |

##### 说明

允许的 MQTT 报文最大长度。

<br />

### mqtt.max_clientid_len

| Type    | Default |
| ------- | ------- |
| integer | 65535   |

##### 说明

允许的 Client ID 串的最大长度。

<br />

### mqtt.max_topic_levels

| Type    | Default |
| ------- | ------- |
| integer | 0       |

##### 说明

允许客户端订阅主题的最大层级。0 表示不限制。

<br />

### mqtt.max_qos_allowed

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `0`, `1`, `2`  | `2`     |

##### 说明

允许客户端发布的最大 QoS 等级。

<br />

### mqtt.max_topic_alias

| Type    | Default |
| ------- | ------- |
| integer | 65535   |

##### 说明

允许最大的主题别名数。0 表示不支持主题别名。

<br />

### mqtt.retain_available

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

是否支持 Retain 消息。

<br />

### mqtt.wildcard_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

是否支持订阅通配主题。

<br />

### mqtt.shared_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

是否支持共享订阅。

<br />

### mqtt.ignore_loop_deliver

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否忽略自己发送的消息。如果忽略，则表明 EMQ X 不会向消息的发送端投递此消息。

<br />

### mqtt.strict_mode

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否开启严格检查模式。严格检查模式会更细致的检查 MQTT 报文的正确性。

<br />

### zone.external.idle_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### 说明

TCP 连接建立后的发呆时间，如果这段时间内未收到任何报文，则会关闭该连接。

<br />

### zone.external.enable_acl

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

是否开启 ACL 检查。

<br />

### zone.external.enable_ban

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

是否开启黑名单。

<br />

### zone.external.enable_stats

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

是否开启客户端状态统计。

<br />

### zone.external.acl_deny_action

| Type | Optional Value         | Default  |
| ---- | -------------------- - | -------- |
| enum | `ignore`, `disconnect` | `ignore` |

##### 说明

ACL 检查失败后，执行的操作。

- `ignore`：不做任何操作。
- `disconnect`：断开连接。

<br />

### zone.external.force_gc_policy

| Type    | Default      |
| ------- | ------------ |
| string  | `16000|16MB` |

##### 说明

当收到一定数量的消息，或字节，就强制执行一次垃圾回收。

格式：`<Number>|<Bytes>`。

例如，`16000|16MB` 表示当收到 `16000` 条消息，或 `16MB` 的字节流入就强制执行一次垃圾回收。

<br />

### zone.external.force_shutdown_policy

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### 说明

当进程消息队列长度，或占用的内存字节到达某值，就强制关闭该进程。

这里的 `消息队列` 指的是 Erlang 进程的 `消息邮箱`，并非 QoS 1 和 QoS 2 的 `mqueue`。

格式：`<Number>|<Bytes>`。

例如，`32000|32MB` 表示当进程堆积了 `32000` 条消息，或进程占用内存达到 `32MB` 则关闭该进程。

<br />

### zone.external.max_packet_size

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

允许的 MQTT 报文最大长度。

<br />

### zone.external.max_clientid_len

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### 说明

允许的 Client ID 串的最大长度。

<br />

### zone.external.max_topic_levels

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### 说明

允许客户端订阅主题的最大层级。0 表示不限制。

<br />

### zone.external.max_qos_allowed

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `0`, `1`, `2`  | -       |

##### 说明

允许客户端发布的最大 QoS 等级。

<br />

### zone.external.max_topic_alias

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### 说明

允许最大的主题别名数。0 表示不支持主题别名。

<br />

### zone.external.retain_available

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### 说明

是否支持 Retain 消息。

<br />

### zone.external.wildcard_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### 说明

是否支持订阅通配主题。

<br />

### zone.external.shared_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### 说明

是否支持共享订阅。

<br />

### zone.external.server_keepalive

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### 说明

服务端指定的 Keepalive 时间。用于 MQTT v5.0 协议的 CONNACK 报文。

<br />

### zone.external.keepalive_backoff

| Type  | Optional Value | Default |
| ----- | -------------- | ------- |
| float | > 0.5          | 0.75    |

##### 说明

Keepalive 退避指数。EMQ X 如果在 `Keepalive * backoff * 2` 的时间内未收到客户端的任何数据报文，则认为客户端已心跳超时。

<br />

### zone.external.max_subscriptions

| Type    | Default |
| ------- | ------- |
| integer | 0     |

##### 说明

单个客户端允许订阅的最大主题数。`0` 表示不限制。

<br />

### zone.external.upgrade_qos

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `off`   |

##### 说明

允许 EMQ X 在投递消息时，强制升级消息的 QoS 等级为订阅的 QoS 等级。

<br />

### zone.external.max_inflight

| Type    | Default |
| ------- | ------- |
| integer | 32      |

##### 说明

飞行窗口大小。飞行窗口用于存储未被应答的 QoS 1 和 QoS 2 消息。

<br />

### zone.external.retry_interval

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### 说明

消息重发间隔。EMQ X 在每个间隔检查是否需要进行消息重发。

<br />

### zone.external.max_awaiting_rel

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### 说明

QoS 2 消息的最大接收窗口，配置 EMQ X 能够同时处理多少从客户端发来的 QoS 2 消息。`0` 表示不限制。

<br />

### zone.external.await_rel_timeout

| Type     | Default |
| -------- | ------- |
| duration | `300s`  |

##### 说明

QoS 2 消息处理超时时间，在超时后若还未收到 QoS 的 PUBREL 报文，则将消息从接收窗口中丢弃。

<br />

### zone.external.session_expiry_interval

| Type     | Default |
| -------- | ------- |
| duration | `2h`    |

##### 说明

会话默认超时时间，主要用于 MQTT v3.1 和 v3.1.1 协议。在 MQTT v5.0 中，该值通常会携带在客户端的连接报文中。

<br />

### zone.external.max_mqueue_len

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

消息队列最大长度。当飞行窗口满，或客户端离线后，消息会被存储至该队列中。0 表示不限制。

<br />

### zone.external.mqueue_priorities

| Type   | Optional Value   | Default |
| ------ | ---------------- | ------- |
| string | `none`, `<Spec>` | `none`  |

##### 说明

队列消息优先级配置：

- `none`：表示无优先级区分。
- `<Spec>`：表示为一个消息优先表，它配置了某主题下消息的优先级。例如：
    * `topic/1=10`：表示主题 `topic/1` 的消息优先级为 `10`。
    * `topic/1=10,topic/2=8`：表示配置了两个主题的优先级，其分别为 `10` 和 `8`。
    * 其中，优先级数值越高，优先等级越高。

当消息队列长度有限时，会优先丢弃低优先级的消息。

<br />

### zone.external.mqueue_default_priority

| Type    | Optional Value      | Default   |
| ------- | ------------------- | --------- |
| enum    | `highest`, `lowest` | `highest` |

##### 说明

消息默认的优先等级。

<br />

### zone.external.mqueue_store_qos0

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

消息队列是否存储 QoS 0 消息。

<br />

### zone.external.enable_flapping_detect

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### 说明

是否开启 `Flapping` 检查。

<br />

### zone.external.mountpoint

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

主题挂载点。配置后，所有订阅和发布的主题在 EMQ X 都会为其增加一个前缀。

其中可用的占位符有：
- `%c`：表示客户端的 Client ID。
- `%u`：表示客户端的 Username。

例如，配置挂载点为 `user/%c/`。那么 Client ID 为 `tom` 的客户端在发布主题 `open` 消息时，实际在 EMQ X 中路由的主题是 `user/tom/open`。

<br />

### zone.external.use_username_as_clientid

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否用客户端的 Username 作为其 Client ID。

<br />

### zone.external.ignore_loop_deliver

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否忽略自己发送的消息。如果忽略，则表明 EMQ X 不会向消息的发送端投递此消息。

<br />


### zone.external.strict_mode

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否开启严格检查模式。严格检查模式会更细致的检查 MQTT 报文的正确性。

<br />

### zone.internal.allow_anonymous

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

是否允许匿名用户登录系统。

<br />

### zone.internal.enable_stats

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

是否开启客户端状态统计。

<br />

### zone.internal.enable_acl

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `off`   |

##### 说明

是否开启 ACL 检查。

<br />

### zone.internal.acl_deny_action

| Type    | Optional Value         | Default  |
| ------- | ---------------------- | -------- |
| enum    | `ignore`, `disconnect` | `ignore` |

##### 说明

ACL 检查失败后，执行的操作。

- `ignore`：不做任何操作。
- `disconnect`：断开连接。

<br />

### zone.internal.force_gc_policy

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### 说明

当收到一定数量的消息，或字节，就强制执行一次垃圾回收。

格式：`<Number>|<Bytes>`。

例如，`16000|16MB` 表示当收到 `16000` 条消息，或 `16MB` 的字节流入就强制执行一次垃圾回收。

<br />

### zone.internal.wildcard_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### 说明

是否支持订阅通配主题。

<br />

### zone.internal.shared_subscription

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### 说明

是否支持共享订阅。

<br />

### zone.internal.max_subscriptions

| Type    | Default |
| ------- | ------- |
| integer | 0       |

##### 说明

单个客户端允许订阅的最大主题数。`0` 表示不限制。

<br />

### zone.internal.max_inflight

| Type    | Default |
| ------- | ------- |
| integer | 128     |

##### 说明

飞行窗口大小。飞行窗口用于存储未被应答的 QoS 1 和 QoS 2 消息。

<br />

### zone.internal.max_awaiting_rel

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

QoS 2 消息的最大接收窗口，配置 EMQ X 能够同时处理多少从客户端发来的 QoS 2 消息。`0` 表示不限制。

<br />

### zone.internal.max_mqueue_len

| Type    | Default |
| ------- | ------- |
| integer | 10000   |

##### 说明

消息队列最大长度。当飞行窗口满，或客户端离线后，消息会被存储至该队列中。`0` 表示不限制。

<br />

**``zone.internal.mqueue_store_qos0**

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

消息队列是否存储 QoS 0 消息。

<br />

### zone.internal.enable_flapping_detect

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `off`   |

##### 说明

是否开启 `Flapping` 检查。

<br />

### zone.internal.force_shutdown_policy

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### 说明

当进程消息队列长度，或占用的内存字节到达某值，就强制关闭该进程。

这里的 `消息队列` 指的是 Erlang 进程的 `消息邮箱`，并非 QoS 1 和 QoS 2 的 `mqueue`。

格式：`<Number>|<Bytes>`。

例如，`32000|32MB` 表示当进程堆积了 `32000` 条消息，或进程占用内存达到 `32MB` 则关闭该进程。

<br />

### zone.internal.mountpoint

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### 说明

主题挂载点。配置后，所有订阅和发布的主题在 EMQ X 都会为其增加一个前缀。

其中可用的占位符有：
- `%c`：表示客户端的 Client ID。
- `%u`：表示客户端的 Username。

例如，配置挂载点为 `user/%c/`。那么 Client ID 为 `tom` 的客户端在发布主题 `open` 消息时，实际在 EMQ X 中路由的主题是 `user/tom/open`。

<br />

### zone.internal.ignore_loop_deliver

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否忽略自己发送的消息。如果忽略，则表明 EMQ X 不会向消息的发送端投递此消息。

<br />

### zone.internal.strict_mode

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否开启严格检查模式。严格检查模式会更细致的检查 MQTT 报文的正确性。

<br />

### zone.internal.bypass_auth_plugins

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

是否允许该 Zone 下的客户端绕过认证插件的认证步骤。

<br />

### listener.tcp.external

| Type    | Default        |
| ------- | -------------- |
| string  | `0.0.0.0:1883` |

##### 说明

配置名称为 `external` 的 MQTT/TCP 监听器的监听地址。

##### 示例

`1883`：表监听 IPv4 的 `0.0.0.0:1883`。
`127.0.0.1:1883`：表监听地址为 `127.0.0.1` 网卡上的 `1883` 端口。
`::1:1883`：表监听 IPv6 地址为 `::1` 网卡上的 `1883` 端口。

<br />

### listener.tcp.external.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 8       |

##### 说明

监听器的接收池大小。

<br />

### listener.tcp.external.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 1024000 |

##### 说明

监听器允许的最大并发连接数量。

<br />

### listener.tcp.external.max_conn_rate

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

监听器允许的最大接入速率。单位：个/秒

<br />

### listener.tcp.external.active_n

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### 说明

监听器持续接收 TCP 报文的次数。

<br />

### listener.tcp.external.zone

| Type    | Default    |
| ------- | ---------- |
| string  | `external` |

##### 说明

监听器所属的配置域 (Zone)。

<br />

### listener.tcp.external.rate_limit

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### 说明

监听器的速率限制。格式为 `<limit>,<duration>`。

##### 示例

`100KB,10s`：表 *限制 10 秒内的流入字节数不超过 100 KB*。

<br />

### listener.tcp.external.access.1

| Type    | Default     |
| ------- | ----------- |
| string  | `allow all` |

##### 说明

监听器的 ACL 规则列表。它用于设置连接层的白/黑名单。

##### 示例

`allow all`：表允许所有的 TCP 连接接入。
`allow 192.168.0.0/24`：表允许网络地址为 `192.168.0.0/24` 的 TCP 连接接入。

同时，该配置可配置多条规则：
```
listener.tcp.external.access.1 = deny 192.168.0.1
listener.tcp.external.access.2 = allow all
```

它表示，除 `192.168.0.1` 外的 TCP 连接都允许接入。

<br />

### listener.tcp.external.proxy_protocol

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### 说明

监听器是否开启 `Proxy Protocol` 的支持。

如果 EMQ X 集群部署在 HAProxy 或 Nginx 后，且需要拿到客户端真实的源 IP 地址与端口，则需打开此配置。

`Proxy Protcol` 参考: [https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol)。

<br />

### listener.tcp.external.proxy_protocol_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### 说明

设置 Proxy Protocol 解析的超时时间。如果该时间内没收到 Proxy Protocol 的报文，EMQ X 会关闭其连接。

<br />

### listener.tcp.external.backlog

| Type    | Default |
| ------- | ------- |
| integer | 1024    |

##### 说明

TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。

<br />

### listener.tcp.external.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### 说明

TCP 报文发送超时时间。

<br />

### listener.tcp.external.send_timeout_close

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

TCP 报文发送超时后，是否关闭该连接。

<br />

### listener.tcp.external.recbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 接收缓存区大小（操作系统内核级参数）

参见：http://erlang.org/doc/man/inet.html

<br />

### listener.tcp.external.sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 发送缓存区大小（操作系统内核级参数）。

参见：[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html)。

<br />

### listener.tcp.external.buffer

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 缓冲区大小 (用户级)。

该值建议大于等于 `sndbuff` 和 `recbuff` 的最大值，以避免一些性能问题。在不配置的情况下，它默认等于 sndbuff 和 recbuff 的最大值。

参见：[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html)。

<br />

### listener.tcp.external.tune_buffer

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### 说明

如果打开此配置，请设置该值等于 `sndbuff` 与 `recbuff` 的最大值。

<br />

### listener.tcp.external.nodelay

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

即 `TCP_NODELAY` 参数。开启该选项即允许小的 TCP 数据报文将会立即发送。

<br />

### listener.tcp.external.reuseaddr

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

即 `SO_REUSEADDR` 参数。开启该选项即允许本地重用端口，无需等待 `TIME_WAIT` 状态结束。

<br />

### listener.tcp.internal

| Type    | Default           |
| ------- | ----------------- |
| string  | `127.0.0.1:11883` |

##### 说明

配置名称为 `internal` 的 MQTT/TCP 监听器的监听地址。

##### 示例

`11883`：表监听 IPv4 的 `0.0.0.0:11883`。
`127.0.0.1:11883`：表监听地址为 `127.0.0.1` 网卡上的 `11883` 端口。
`::1:11883`：表监听 IPv6 地址为 `::1` 网卡上的 `11883` 端口。

<br />

### listener.tcp.internal.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 4       |

##### 说明

监听器的接收池大小。

<br />

### listener.tcp.internal.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 1024000 |

##### 说明

监听器允许的最大并发连接数量。

<br />

### listener.tcp.internal.max_conn_rate

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

监听器允许的最大接入速率。单位：个/秒

<br />

### listener.tcp.internal.active_n

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

监听器持续接收 TCP 报文的次数。

<br />

### listener.tcp.internal.zone

| Type    | Default    |
| ------- | ---------- |
| string  | `internal` |

##### 说明

监听器所属的配置域 (Zone)。

<br />

### listener.tcp.internal.rate_limit

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### 说明

监听器的速率限制。格式为 `<limit>,<duration>`。

##### 示例

`100KB,10s`：表 *限制 10 秒内的流入字节数不超过 100 KB*。


### listener.tcp.internal.backlog

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### 说明

TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。

<br />

### listener.tcp.internal.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### 说明

TCP 报文发送超时时间。

<br />

### listener.tcp.internal.send_timeout_close

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

TCP 报文发送超时后，是否关闭该连接。

<br />

### listener.tcp.internal.recbuf

| Type     | Default |
| -------- | ------- |
| bytesize | `64KB`  |

##### 说明

TCP 接收缓存区大小（操作系统内核级参数）

<br />

### listener.tcp.internal.sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | `64KB`  |

##### 说明

TCP 发送缓存区大小（操作系统内核级参数）

<br />

### listener.tcp.internal.buffer

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 缓冲区大小 (用户级)。

<br />

### listener.tcp.internal.tune_buffer

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### 说明

如果打开此配置，请设置该值等于 `sndbuff` 与 `recbuff` 的最大值。

<br />

### listener.tcp.internal.nodelay

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

即 `TCP_NODELAY` 参数。开启该选项即允许小的 TCP 数据报文将会立即发送。

<br />

### listener.tcp.internal.reuseaddr

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

即 `SO_REUSEADDR` 参数。开启该选项即允许本地重用端口，无需等待 `TIME_WAIT` 状态结束。

<br />

### listener.ssl.external

| Type    | Default        |
| ------- | -------------- |
| string  | `0.0.0.0:8883` |

##### 说明

配置名称为 `external` 的 SSL 监听器。

<br />

### listener.ssl.external.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 16      |

##### 说明

监听器的接收池大小。

<br />

### listener.ssl.external.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 102400  |

##### 说明

监听器允许的最大并发连接数量。

<br />

### listener.ssl.external.max_conn_rate

| Type    | Default |
| ------- | ------- |
| integer | 500     |

##### 说明

监听器允许的最大接入速率。单位：个/秒。

<br />

### listener.ssl.external.active_n

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### 说明

监听器持续接收 TCP 报文的次数。

<br />

### listener.ssl.external.zone

| Type    | Default    |
| ------- | ---------- |
| string  | `external` |

##### 说明

监听器所属的配置组 (Zone)。

<br />

### listener.ssl.external.access.1

| Type    | Default     |
| ------- | ----------- |
| string  | `allow all` |

##### 说明

监听器的 ACL 规则列表。它用于设置连接层的白/黑名单。

例如:

`allow all`：表允许所有的 TCP 连接接入。
`allow 192.168.0.0/24`：表允许网络地址为 `192.168.0.0/24` 的 TCP 连接接入。

同时，该配置可配置多条规则:

```
listener.ssl.external.access.1 = deny 192.168.0.1
listener.ssl.external.access.2 = allow all
```

<br />

### listener.ssl.external.rate_limit

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### 说明

监听器的速率限制。格式为 `<limit>,<duration>`。

<br />

### listener.ssl.external.proxy_protocol

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### 说明

监听器是否开启 `Proxy Protocol` 的支持。

如果 EMQ X 集群部署在 HAProxy 或 Nginx 后，且需要拿到客户端真实的源 IP 地址与端口，则需打开此配置。

`Proxy Protcol` 参考: [https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol)。

<br />

### listener.ssl.external.proxy_protocol_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### 说明

设置 Proxy Protocol 解析的超时时间。如果该时间内没收到 Proxy Protocol 的报文，EMQ X 会关闭其连接。

<br />

### listener.ssl.external.tls_versions

| Type   | Default                 |
| ------ | ----------------------- |
| string | `tlsv1.2,tlsv1.1,tlsv1` |

##### 说明

指定服务端支持的 SSL 的版本列表。详情请参见 [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html)。

<br />

### listener.ssl.external.handshake_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### 说明

指定 SSL 握手过程的超时时间。

<br />

### listener.ssl.external.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### 说明

指定 SSL 的私钥文件 (PEM)。

<br />

### listener.ssl.external.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### 说明

指定 SSL 的证书文件 (PEM)。

<br />

### listener.ssl.external.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### 说明

指定 SSL 的 CA 证书文件 (PEM)。

<br />

### listener.ssl.external.dhfile

| Type   | Default                   |
| ------ | ------------------------- |
| string | `etc/certs/dh-params.pem` |

##### 说明

若使用 Ephemeral Diffie-Helman 算法，指定算法使用的 key 文件。

<br />

### listener.ssl.external.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------- | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### 说明

指定握手过程中是否校验客户端。

<br />

### listener.ssl.external.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

SSL 握手过程中若客户端没有证书，是否让握手失败。

<br />

### listener.ssl.external.ciphers

| Type   | Default |
| ------ | ------- |
| string | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA` |

##### 说明

指定服务端支持的密码套件。

<br />

### listener.ssl.external.psk_ciphers

| Type   | Default                                                                  |
| ------ | ------------------------------------------------------------------------ |
| string | `PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA` |

##### 说明

若使用 PSK 算法，指定服务端支持的 PSK Cipher 列表。注意 'listener.ssl.external.ciphers' 和 'listener.ssl.external.psk_ciphers' 只能配置一个。

<br />

### listener.ssl.external.secure_renegotiate

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### 说明

指定在客户端不遵循 RFC 5746 的情况下，是否拒绝 renegotiation 请求。

<br />

### listener.ssl.external.reuse_sessions

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

指定是否支持 SSL session 重用。详情见 [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html)。

<br />

### listener.ssl.external.honor_cipher_order

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

指定是否使用服务端的偏好设置选择 Ciphers。

<br />

### listener.ssl.external.peer_cert_as_username

| Type | Optional Value    | Default |
| ---- | ----------------- | ------- |
| enum | `cn`, `dn`, `crt` | `cn`    |

##### 说明

使用客户端证书中的 CN、DN 或者 CRT 字段的值作为 MQTT CONNECT 报文中的 Username 字段的值。
注意 `listener.ssl.external.verify` 应当设置为 `verify_peer`。

<br />

### listener.ssl.external.backlog

| Type    | Default |
| ------- | ------- |
| integer | 1024    |

##### 说明

TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。

<br />

### listener.ssl.external.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### 说明

TCP 报文发送超时时间。

<br />

*`listener.ssl.external.send_timeout_close`**

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

TCP 报文发送超时后，是否关闭该连接。

<br />

### listener.ssl.external.recbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 接收缓存区大小（操作系统内核级参数）。

参见：[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html)。

<br />

### listener.ssl.external.sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 发送缓存区大小（操作系统内核级参数）。

参见：[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html)。

<br />

### listener.ssl.external.buffer

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 缓冲区大小 (用户级)。

该值建议大于等于 `sndbuff` 和 `recbuff` 的最大值，以避免一些性能问题。在不配置的情况下，它默认等于 sndbuff 和 recbuff 的最大值。

参见：[http://erlang.org/doc/man/inet.html](http://erlang.org/doc/man/inet.html)。

<br />

### listener.ssl.external.tune_buffer

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### 说明

如果打开此配置，请设置该值等于 `sndbuff` 与 `recbuff` 的最大值。

<br />

### listener.ssl.external.nodelay

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

即 `TCP_NODELAY` 参数。开启该选项即表示禁用 Nagle 算法，小包将被立即发送。

<br />

### listener.ssl.external.reuseaddr

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

即 `SO_REUSEADDR` 参数。开启该选项即允许本地重用端口，无需等待 `TIME_WAIT` 状态结束。

<br />

### listener.ws.external

| Type    | Default |
| ------- | ------- |
| string  | `8083`  |

##### 说明

配置名称为 `external` 的 MQTT/WS 监听器的监听地址。

##### 示例

`8083`：表监听 IPv4 的 `0.0.0.0:8083`。
`127.0.0.1:8083`：表监听地址为 `127.0.0.1` 网卡上的 `8083` 端口。
`::1:8083`：表监听 IPv6 地址为 `::1` 网卡上的 `8083` 端口。

<br />

### listener.ws.external.mqtt_path

| Type    | Default |
| ------- | ------- |
| string  | `/mqtt` |

##### 说明

WebSocket 的 MQTT 协议路径。因此 EMQ X 的 WebSocket 的地址是： `ws://<ip>:<port>/mqtt`。

<br />

### listener.ws.external.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 4       |

##### 说明

监听器的接收池大小。

<br />

### listener.ws.external.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 102400  |

##### 说明

监听器允许的最大并发连接数量。

<br />

### listener.ws.external.max_conn_rate

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

监听器允许的最大接入速率。单位：个/秒

<br />

### listener.ws.external.active_n

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### 说明

监听器持续接收 TCP 报文的次数。

<br />

### listener.ws.external.rate_limit

| Type    | Default     |
| ------- | ----------- |
| string  | `100KB,10s` |

##### 说明

监听器的速率限制。格式为 `<limit>,<duration>`。

##### 示例

`100KB,10s`：表 *限制 10 秒内的流入字节数不超过 100 KB*。

<br />

### listener.ws.external.zone

| Type    | Default    |
| ------- | ---------- |
| string  | `external` |

##### 说明

监听器所属的配置域 (Zone)。

<br />

### listener.ws.external.access.1

| Type    | Default     |
| ------- | ----------- |
| string  | `allow all` |

##### 说明

监听器的 ACL 规则列表。它用于设置连接层的白/黑名单。

<br />

### listener.ws.external.verify_protocol_header

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

是否验证 WebSocket 携带的 HTTP 头部是否正确。**微信小程序需关闭该验证**。

<br />

### listener.ws.external.proxy_address_header

| Type    | Optional Value    | Default |
| ------- | ----------------- |-------- |
| string  | `X-Forwarded-For` | -       |

##### 说明

如果 EMQ X 集群部署在 HAProxy 或 Nginx 后，则可打开该配置获取客户端真实的 IP 地址。

<br />

### listener.ws.external.proxy_port_header

| Type    | Optional Value     | Default |
| ------- | ------------------ | ------- |
| string  | `X-Forwarded-Port` | -       |

##### 说明

如果 EMQ X 集群部署在 HAProxy 或 Nginx 后，则可打开该配置获取客户端真实的端口。

<br />

### listener.ws.external.proxy_protocol

| Type    | Optional Value      | Default |
| ------- | ------------------- | ------- |
| enum    | `on`, `off`         | -       |

##### 说明

监听器是否开启 `Proxy Protocol` 的支持。

如果 EMQ X 集群部署在 HAProxy 或 Nginx 后，且需要拿到客户端真实的源 IP 地址与端口，则需打开此配置。

`Proxy Protcol` 参考: [https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol)。

<br />

### listener.ws.external.proxy_protocol_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### 说明

设置 Proxy Protocol 解析的超时时间。如果该时间内没收到 Proxy Protocol 的报文，EMQ X 会关闭其连接。

<br />

### listener.ws.external.backlog

| Type    | Default |
| ------- | ------- |
| integer | 1024    |

##### 说明

TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。

<br />

### listener.ws.external.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### 说明

TCP 报文发送超时时间。

<br />

### listener.ws.external.send_timeout_close

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

TCP 报文发送超时后，是否关闭该连接。

<br />

### listener.ws.external.recbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 接收缓存区大小（操作系统内核级参数）

<br />

### listener.ws.external.sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 发送缓存区大小（操作系统内核级参数）

<br />

### listener.ws.external.buffer

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 缓冲区大小 (用户级)。

<br />

### listener.ws.external.tune_buffer

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### 说明

如果打开此配置，请设置该值等于 `sndbuff` 与 `recbuff` 的最大值。

<br />

### listener.ws.external.nodelay

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

即 `TCP_NODELAY` 参数。开启该选项即允许小的 TCP 数据报文将会立即发送。

<br />

### listener.ws.external.compress

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | -       |

##### 说明

是否压缩 WebSocket 消息。压缩的实现依赖 [zlib](http://www.zlib.net)。

`defalte_opts` 下的配置项，都属于压缩相关的参数配置，如无必要请不需要修改它。

<br />

### listener.ws.external.deflate_opts.level

| Type    | Optional Value                                      | Default |
| ------- | --------------------------------------------------- | ------- |
| enum    | `none`, `default`, `best_compression`, `best_speed` | -       |

##### 说明

压缩等级。

<br />

### listener.ws.external.deflate_opts.mem_level

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1 - 9          | -       |

##### 说明

压缩参数。内存使用限制等级，配置可开辟多少内存来参与压缩过程。

`1`：最少的内存，但会降低压缩率。
`9`：最多的内存，会提高计算速度和压缩率。

不配置，则默认为 `8`。

<br />

### listener.ws.external.deflate_opts.strategy

| Type    | Optional Value                                | Default |
| ------- | --------------------------------------------- | ------- |
| enum    | `default`, `filtered`, `huffman_only`, `rle`  | -       |

##### 说明

压缩策略，用于调优压缩率：

- `default`：针对普通数据。
- `filtered`：由过滤器或预测器产生的数据，适用于分布随机性强的内容。
- `huffman_only`：强制使用 Huffman 算法。优于 `filtered`。
- `rle`：将匹配距离限制为 1 (Run-Lenght Encoding)，比 `huffman_only` 要快，但主要用于 PNG 图片。

这些策略仅影响压缩率，不会对正确性带来任何影响。

<br />

### listener.ws.external.deflate_opts.server_context_takeover

| Type    | Optional Value            | Default |
| ------- | ------------------------- | ------- |
| enum    | `takeover`, `no_takeover` | -       |

##### 说明

是否允许服务端的压缩上下文在帧之间传递。

<br />

### listener.ws.external.deflate_opts.client_context_takeover

| Type    | Optional Value            | Default |
| ------- | ------------------------- | ------- |
| enum    | `takeover`, `no_takeover` | -       |

##### 说明

是否允许客户端的压缩上下文在帧之间传递。

<br />

### listener.ws.external.deflate_opts.server_max_window_bits

| Type    | Optional Value  | Default |
| ------- | --------------- | ------- |
| integer | 8 - 15          | -       |

##### 说明

服务端最大窗口值。设置一个较大的值会有更好的压缩率，但会额外的消耗内存。

<br />

### listener.ws.external.deflate_opts.client_max_window_bits

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 8 - 15         | -       |

##### 说明

客户端最大窗口值。设置一个较大的值会有更好的压缩率，但会额外的消耗内存。

<br />

### listener.ws.external.idle_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### 说明

TCP 连接建立后的发呆时间，如果这段时间内未收到任何报文，则会关闭该连接。

<br />

### listener.ws.external.max_frame_size

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### 说明

允许的单个 MQTT 报文长度的最大值。

<br />

### listener.wss.external

| Type    | Default        |
| ------- | -------------- |
| string  | `0.0.0.0:8084` |

##### 说明

配置名称为 `external` 的 WSS (MQTT/WebSocket/SSL) 监听器。

<br />

### listener.wss.external.mqtt_path

| Type    | Default |
| ------- | ------- |
| string  | `/mqtt` |

##### 说明

WebSocket 的 URL Path。

<br />

### listener.wss.external.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 4       |

##### 说明

监听器的接收池大小。

<br />

### listener.wss.external.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 16      |

##### 说明

监听器允许的最大并发连接数量。

<br />

### listener.wss.external.max_conn_rate

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

监听器允许的最大接入速率。单位：个/秒。

<br />

### listener.wss.external.active_n

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### 说明

监听器持续接收 TCP 报文的次数。

<br />

### listener.wss.external.rate_limit

| Type    | Default |
| ------- | ------- |
| string  | -       |

##### 说明

监听器的速率限制。格式为 `<limit>,<duration>`。

<br />

### listener.wss.external.zone

| Type    | Default    |
| ------- | ---------- |
| string  | `external` |

##### 说明

监听器所属的配置组 (Zone)。

<br />

### listener.wss.external.access.1

| Type    | Default     |
| ------- | ----------- |
| string  | `allow all` |

##### 说明

监听器的 ACL 规则列表。它用于设置连接层的白/黑名单。

例如:

`allow all`：表允许所有的 TCP 连接接入。
`allow 192.168.0.0/24`：表允许网络地址为 `192.168.0.0/24` 的 TCP 连接接入。

同时，该配置可配置多条规则:

```
listener.wss.external.access.1 = deny 192.168.0.1
listener.wss.external.access.2 = allow all
```

<br />

### listener.wss.external.verify_protocol_header

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

是否验证 WebSocket 携带的 HTTP 头部是否正确。**微信小程序需关闭该验证**。

<br />

### listener.wss.external.proxy_address_header

| Type   | Default           |
| ------ | ----------------- |
| string | `X-Forwarded-For` |

##### 说明

如果 EMQ X 集群部署在 HAProxy 或 Nginx，则可打开该配置获取客户端真实的 IP 地址。

<br />

### listener.wss.external.proxy_protocol

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### 说明

监听器是否开启 `Proxy Protocol` 的支持。

如果 EMQ X 集群部署在 HAProxy 或 Nginx 后，且需要拿到客户端真实的源 IP 地址与端口，则需打开此配置。

`Proxy Protcol` 参考：[https://www.haproxy.com/blog/haproxy/proxy-protocol](https://www.haproxy.com/blog/haproxy/proxy-protocol)。

<br />

### listener.wss.external.proxy_protocol_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### 说明

设置 Proxy Protocol 解析的超时时间。如果该时间内没收到 Proxy Protocol 的报文，EMQ X 会关闭其连接。

<br />

### listener.wss.external.tls_versions

| Type   | Default                |
| ------ | ----------------------- |
| string | `tlsv1.2,tlsv1.1,tlsv1` |

##### 说明

指定服务端支持的 SSL 的版本列表。详情请参见 [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html)。

<br />

### listener.wss.external.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### 说明

指定 SSL 的私钥文件 (PEM)。

<br />

### listener.wss.external.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### 说明

指定 SSL 的证书文件 (PEM)。

<br />

### listener.wss.external.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### 说明

若使用 SSL，指定 SSL 的 CA 证书文件 (PEM)。

<br />

### listener.wss.external.dhfile

| Type   | Default                   |
| ------ | ------------------------- |
| string | `etc/certs/dh-params.pem` |

##### 说明

若使用 Ephemeral Diffie-Helman 算法，指定算法使用的 key 文件。

<br />

### listener.wss.external.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------  | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### 说明

指定握手过程中是否校验客户端。

<br />

### listener.wss.external.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

SSL 握手过程中若客户端没有证书，是否让握手失败。

<br />

### listener.wss.external.ciphers

| Type   | Default |
| ------ | ------- |
| string | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA` |

##### 说明

指定服务器支持的密码套件。

<br />

### listener.wss.external.psk_ciphers

| Type   | Default                                                                  |
| ------ | ------------------------------------------------------------------------ |
| string | `PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA` |

##### 说明

若使用 PSK 算法，指定服务端支持的 PSK Cipher 列表。注意 'listener.wss.external.ciphers' 和 'listener.wss.external.psk_ciphers' 只能配置一个。

<br />

### listener.wss.external.secure_renegotiate

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `off`   |

##### 说明

指定在客户端不遵循 RFC 5746 的情况下，是否拒绝 renegotiation 请求。

<br />

### listener.wss.external.reuse_sessions

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

指定是否支持 SSL session 重用。详情见 [http://erlang.org/doc/man/ssl.html](http://erlang.org/doc/man/ssl.html)。

<br />

### listener.wss.external.honor_cipher_order

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

指定是否使用服务端的偏好设置选择 Ciphers。

<br />

### listener.wss.external.peer_cert_as_username

| Type | Optional Value    | Default |
| ---- | ----------------- | ------- |
| enum | `cn`, `dn`, `crt` | `cn`    |

##### 说明

使用客户端证书中的 CN、DN 或者 CRT 字段的值作为 MQTT CONNECT 报文中的 Username 字段的值。
注意 `listener.wss.external.verify` 应当设置为 `verify_peer`。

<br />

### listener.wss.external.backlog

| Type    | Default |
| ------- | ------- |
| integer | 1024    |

##### 说明

TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。

<br />

### listener.wss.external.send_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### 说明

TCP 报文发送超时时间。

<br />

*`listener.wss.external.send_timeout_close`**

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

TCP 报文发送超时后，是否关闭该连接。

<br />

### listener.wss.external.recbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 接收缓存区大小（操作系统内核级参数）

参见：http://erlang.org/doc/man/inet.html

<br />

### listener.wss.external.sndbuf

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 发送缓存区大小（操作系统内核级参数）

参见：http://erlang.org/doc/man/inet.html

<br />

### listener.wss.external.buffer

| Type     | Default |
| -------- | ------- |
| bytesize | -       |

##### 说明

TCP 缓冲区大小 (用户级)。

该值建议大于等于 `sndbuff` 和 `recbuff` 的最大值，以避免一些性能问题。在不配置的情况下，它默认等于 sndbuff 和 recbuff 的最大值

参见：http://erlang.org/doc/man/inet.html

<br />

### listener.wss.external.tune_buffer

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | -       |

##### 说明

如果打开此配置，请设置该值等于 `sndbuff` 与 `recbuff` 的最大值。

<br />

### listener.wss.external.nodelay

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

即 `TCP_NODELAY` 参数。开启该选项即允许小的 TCP 数据报文将会立即发送。

<br />

### listener.wss.external.compress

| Type    | Optional Value  | Default |
| ------- | --------------- | ------- |
| enum    | `true`, `false` | `false` |

##### 说明

该选项若设置为 true，Websocket 消息将会被压缩。

<br />

### listener.wss.external.deflate_opts.level

| Type    | Optional Value                                      | Default   |
| ------- | --------------------------------------------------- | --------- |
| enum    | `none`, `default`, `best_compression`, `best_speed` | `default` |

##### 说明

压缩等级。

<br />

### listener.wss.external.deflate_opts.mem_level

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 1 - 9          | -       |

##### 说明

压缩参数。内存使用限制等级，配置可开辟多少内存来参与压缩过程。

`1`：最少的内存，但会降低压缩率。
`9`：最多的内存，会提高计算速度和压缩率。

不配置，则默认为 `8`。

<br />

### listener.wss.external.deflate_opts.strategy

| Type    | Optional Value                                | Default |
| ------- | --------------------------------------------- | ------- |
| enum    | `default`, `filtered`, `huffman_only`, `rle`  | -       |

##### 说明

压缩策略，用于调优压缩率：

- `default`：针对普通数据。
- `filtered`：由过滤器或预测器产生的数据，适用于分布随机性强的内容。
- `huffman_only`：强制使用 Huffman 算法。优于 `filtered`。
- `rle`：将匹配距离限制为 1 (Run-Lenght Encoding)，比 `huffman_only` 要快，但主要用于 PNG 图片。

这些策略仅影响压缩率，不会对正确性带来任何影响。

<br />

### listener.wss.external.deflate_opts.server_context_takeover

| Type    | Optional Value            | Default |
| ------- | ------------------------- | ------- |
| enum    | `takeover`, `no_takeover` | -       |

##### 说明

是否允许服务端的压缩上下文在帧之间传递。

<br />

### listener.wss.external.deflate_opts.client_context_takeover

| Type    | Optional Value            | Default |
| ------- | ------------------------- | ------- |
| enum    | `takeover`, `no_takeover` | -       |

##### 说明

是否允许客户端的压缩上下文在帧之间传递。

<br />

### listener.wss.external.deflate_opts.server_max_window_bits

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 8 - 15         | -       |

##### 说明

服务端最大窗口值。设置一个较大的值会有更好的压缩率，但会额外的消耗内存。

<br />

### listener.wss.external.deflate_opts.client_max_window_bits

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| integer | 8 - 15         | -       |

##### 说明

客户端最大窗口值。设置一个较大的值会有更好的压缩率，但会额外的消耗内存。

<br />

### listener.wss.external.idle_timeout

| Type     | Default |
| -------- | ------- |
| duration | -       |

##### 说明

TCP 连接建立后的发呆时间，如果这段时间内未收到任何报文，则会关闭该连接。

<br />

### listener.wss.external.max_frame_size

| Type    | Default |
| ------- | ------- |
| integer | -       |

##### 说明

允许的单个 MQTT 报文长度的最大值。

<br />

### plugins.etc_dir

| Type    | Default       |
| ------- | ------------- |
| string  | `etc/plugins` |

##### 说明

插件的配置目录。

<br />

### plugins.loaded_file

| Type    | Default              |
| ------- | -------------------- |
| string  | `etc/loaded_plugins` |

##### 说明

插件启动列表的配置文件路径。

<br />

### plugins.expand_plugins_dir

| Type    | Default    |
| ------- | ---------- |
| string  | `plugins/` |

##### 说明

外部插件存放目录。

<br />

### broker.sys_interval

| Type      | Default |
| --------- | ------- |
| duration  | `1m`    |

##### 说明

设置系统主题 (`$SYS`) 消息的发布间隔。

<br />

### broker.sys_heartbeat

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### 说明

设置系统心跳消息的发布间隔。系统心跳消息包括下面两个主题：

- "$SYS/brokers/<node>/uptime"
- "$SYS/brokers/<node>/datetime"

<br />

### broker.enable_session_registry

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `on`    |

##### 说明

启用或关闭全局会话注册。

<br />

### broker.session_locking_strategy

| Type | Optional Value                  | Default  |
| ---- | ------------------------------- | -------- |
| enum | `local`, `one`, `quorum`, `all` | `quorum` |

##### 说明

设置会话集群锁的类型。会话的集群锁用来防止同一个客户端在多个不同节点上创建多个会话，常见于客户端频繁切换节点登录的情况。

<br />

### broker.shared_subscription_strategy

| Type | Optional Value                            | Default  |
| ---- | ----------------------------------------- | -------- |
| enum | `random`, `round_robin`, `sticky`, `hash` | `random` |

##### 说明

设置共享订阅的分发策略。可选值为:

- **random**: 在所有订阅者中随机选择
- **round_robin**: 按照订阅顺序
- **sticky**: 一直发往上次选取的订阅者
- **hash**: 按照发布者 ClientID 的哈希值

<br />

### broker.shared_dispatch_ack_enabled

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

开启或关闭共享订阅对于 qos1/qos2 消息的 ACK 检查功能。开启后，如果投递到某个订阅者但收不到ACK，将尝试投递给订阅组里的下一个订阅者。

<br />

### broker.route_batch_clean

| Type    | Optional Value | Default |
| ------- | -------------- | ------- |
| enum    | `on`, `off`    | `off`   |

##### 说明

开启或关闭批量清理路由信息。批量清理路由可用在短时间内大量客户端掉线的情况，以提高清理效率。

<br />

### sysmon.long_gc

| Type     | Default |
| -------- | ------- |
| duration | `0ms`   |

##### 说明

启用垃圾回收时间监控并在回收时间超过设定值时触发告警，0 表示禁用此监控。

<br />

### sysmon.long_schedule

| Type     | Default |
| -------- | ------- |
| duration | `240ms` |

##### 说明

启用进程调度时间监控并在调度时间超过设定值时触发告警，0 表示禁用此监控。

<br />

### sysmon.large_heap

| Type     | Default |
| -------- | ------- |
| bytesize | `8MB`   |

##### 说明

启用堆栈大小监控并在进程执行垃圾回收后堆栈大小仍大于设定值时触发告警，0 表示禁用此监控。

<br />

### sysmon.busy_port

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

指定是否启用进程间消息通道拥塞监控。

<br />

### sysmon.busy_dist_port

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

指定是否启用集群 RPC 通道拥塞监控。

<br />

### os_mon.cpu_check_interval

| Type     | Default |
| -------- | ------- |
| duration | `60s`   |

##### 说明

CPU 占用率检查周期。

<br />

### os_mon.cpu_high_watermark

| Type    | Default |
| ------- | ------- |
| percent | `80%`   |

##### 说明

CPU 占用率超过 `os_mon.cpu_high_watermark` 时将触发告警。

<br />

### os_mon.cpu_low_watermark

| Type    | Default |
| ------- | ------- |
| percent | `60%`   |

##### 说明

CPU 占用率回落到 `os_mon.cpu_low_watermark` 以下时将清除告警。

<br />

### os_mon.mem_check_interval

| Type     | Default |
| -------- | ------- |
| duration | `60s`   |

##### 说明

内存占用率检查周期。

<br />

### os_mon.sysmem_high_watermark

| Type    | Default |
| ------- | ------- |
| percent | `70%`   |

##### 说明

EMQ X 为所有进程分配的内存占系统内存的百分比超过 `os_mon.sysmem_high_watermark` 时将触发告警。

<br />

### os_mon.procmem_high_watermark

| Type    | Default |
| ------- | ------- |
| percent | `5%`    |

##### 说明

EMQ X 为单个进程分配的内存占系统内存的百分比超过 `os_mon.procmem_high_watermark` 时将触发告警。

<br />

### vm_mon.check_interval

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### 说明

进程数量检查周期。

<br />

### vm_mon.process_high_watermark

| Type    | Default |
| ------- | ------- |
| percent | `80%`   |

##### 说明

当前进程数量占进程最大数量的百分比超过 `vm_mon.process_high_watermark` 时将触发告警。进程最大数量由 `node.process_limit` 配置项决定。

<br />

### vm_mon.process_low_watermark

| Type    | Default |
| ------- | ------- |
| percent | `60%`   |

##### 说明

当前进程数量占进程最大数量的百分比回落到 `vm_mon.process_low_watermark` 以下时将触发告警。进程最大数量由 `node.process_limit` 配置项决定。

<br />

## [emqx-auth-clientid](https://github.com/emqx/emqx-auth-clientid)

### auth.client.<Number>.clientid` & `auth.client.<Number>.password

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

客户端的认证数据，其中 `auth.client.<Number>.password` 为明文密码。`<Number>` 相同的 `auth.client.<Number>.clientid` 与 `auth.client.<Number>.password` 必须成对出现。`<Number>` 是一个整型数字，用于区分多个客户端的认证数据。

<br />

### auth.client.password_hash

| Type | Optional Value                  | Default  |
| ---- | ------------------------------- | -------- |
| enum | `plain`, `md5`, `sha`, `sha256` | `sha256` |

##### 说明

密码存储至数据库时使用的 Hash 算法。以下选项可用：

`plain`

密码以明文形式存储。

`md5`

密码使用 MD5 算法加密后存储。

`sha`

密码使用 SHA-1 算法加密后存储。

`sha256`

密码使用 SHA-256 算法加密后存储。

<br />

## [emqx-auth-http](https://github.com/emqx/emqx-auth-http)

### auth.http.auth_req

| Type   | Default                           |
| ------ | --------------------------------- |
| string | `http://127.0.0.1:8991/mqtt/auth` |

##### 说明

指定认证请求的目标 URL。

<br />

### auth.http.auth_req.method

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `get`, `post`  | `post`  |

##### 说明

指定认证请求的请求方法。

<br />

### auth.http.auth_req.params

| Type   | Format                                                 | Default                               |
| ------ | ------------------------------------------------------ | ------------------------------------- |
| string | 以 `,` 分隔的 `k=v` 键值对，`v` 可以是固定内容，也可以是占位符 | `clientid=%c,username=%u,password=%P` |

##### 说明

指定认证请求中携带的数据。使用 GET 方法时 `auth.http.auth_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以查询字符串参数的形式发送。使用 POST 方法时 `auth.http.auth_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以 Request Body 的形式发送。所有的占位符都会被运行时数据所替换，可用的占位符如下：

| 占位符 | 替换内容             |
| ------ | -------------------- |
| `%u`   | 用户名 |
| `%c`   | MQTT Client ID       |
| `%a`   | 客户端的网络 IP 地址 |
| `%r`   | 客户端使用的协议，可以是：`mqtt`, `mqtt-sn`, `coap`, `lwm2m` 以及 `stomp` |
| `%P`   | 密码 |
| `%p`   | 客户端连接的服务端端口 |
| `%c`   | 客户端证书中的 Common Name |
| `%d`   | 客户端证书中的 Subject |

<br />

### auth.http.super_req

| Type   | Default                                |
| ------ | -------------------------------------- |
| string | `http://127.0.0.1:8991/mqtt/superuser` |

##### 说明

指定超级用户认证请求的目标 URL。

### auth.http.super_req.method

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `get`, `post`  | `post`  |

##### 说明

指定超级用户认证请求的请求方法。

### auth.http.super_req.params

| Type   | Format                                                       | Default                   |
| ------ | ------------------------------------------------------------ | ------------------------- |
| string | 以 `,` 分隔的 `k=v` 键值对，`v` 可以是固定内容，也可以是占位符       | `clientid=%c,username=%u` |

##### 说明

指定超级用户认证请求中携带的数据。使用 GET 方法时 `auth.http.super_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以查询字符串参数的形式发送。使用 POST 方法时 `auth.http.super_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以 Request Body 的形式发送。所有的占位符都会被运行时数据所替换，可用的占位符同 `auth.http.auth_req.params`。

<br />

### auth.http.acl_req

| Type   | Default                          |
| ------ | -------------------------------- |
| string | `http://127.0.0.1:8991/mqtt/acl` |

##### 说明

指定 ACL 验证请求的目标 URL。

<br />

### auth.http.acl_req.method

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `get`, `post`  | `post`  |

##### 说明

指定 ACL 验证请求的请求方法。

<br />

### auth.http.acl_req.params

| Type   | Format                                                       | Default                                                              |
| ------ | ------------------------------------------------------------ | -------------------------------------------------------------------- |
| string | 以 `,` 分隔的 `k=v` 键值对，`v` 可以是固定内容，也可以是占位符       | `access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t,mountpoint=%m` |

##### 说明

指定 ACL 验证请求中携带的数据。使用 GET 方法时 `auth.http.acl_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以查询字符串参数的形式发送。使用 POST 方法时 `auth.http.acl_req.params` 的值将被转换为以 `&` 分隔的 `k=v` 键值对以 Request Body 的形式发送。所有的占位符都会被运行时数据所替换，可用的占位符如下：

| 占位符 | 替换内容                                                     |
| ------ | ------------------------------------------------------------ |
| `%A`   | 需要验证的权限，1 表示订阅，2 表示发布                       |
| `%u`   | MQTT Client ID                                               |
| `%c`   | 客户端标识符                                                 |
| `%a`   | 客户端的网络 IP 地址                                         |
| `%r`   | 客户端使用的协议，可以是：`mqtt`, `mqtt-sn`, `coap`, `lwm2m` 以及 `stomp` |
| `%m`   | 挂载点                                                       |
| `%t`   | 主题                                                         |

<br />

### auth.http.request.timeout

| Type     | Default |
| -------- | ------- |
| duration | `0s`    |

##### 说明

HTTP 请求超时时间。任何等价于 `0s` 的设定值都表示永不超时。

<br />

### auth.http.request.connect_timeout

| Type     | Default |
| -------- | ------- |
| duration | `0s`    |

##### 说明

HTTP 请求的连接超时时间。任何等价于 `0s` 的设定值都表示永不超时。

<br />

### auth.http.request.retry_times

| Type    | Default |
| ------- | ------- |
| integer | 3       |

##### 说明

HTTP 请求失败时的重试次数。

<br />

### auth.http.request.retry_interval

| Type     | Default |
| -------- | ------- |
| duration | `1s`    |

##### 说明

HTTP 请求失败时的重试间隔。

<br />

### auth.http.request.retry_backoff

| Type  | Default |
| ----- | ------- |
| float | 2.0     |

##### 说明

HTTP 请求失败时的重试间隔使用了指数退避算法，此配置项用于指定指数退避算法的退避系数。

<br />

### auth.http.header.<Key>

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

指定 HTTP 请求头部中的数据。`<Key>` 指定 HTTP 请求头部中的字段名，此配置项的值为相应的字段值。`<Key>` 可以是标准的 HTTP 请求头部字段，也可以自定义的字段，可以配置多个不同的请求头部字段。

##### 示例

```
auth.http.header.Accept = */*
auth.http.header.Accept-Encoding = *
```

<br />

### auth.http.ssl.cacertfile

| Type   | Default            |
| ------ | ------------------ |
| string | `etc/certs/ca.pem` |

##### 说明

CA 证书文件路径。

<br />

### auth.http.ssl.certfile

| Type   | Default                     |
| ------ | --------------------------- |
| string | `etc/certs/client-cert.pem` |

##### 说明

客户端证书文件路径。

<br />

### auth.http.ssl.keyfile

| Type   | Default                    |
| ------ | -------------------------- |
| string | `etc/certs/client.key.pem` |

##### 说明

客户端私钥文件路径。

<br />

## [emqx-auth-jwt](https://github.com/emqx/emqx-auth-jwt)

### auth.jwt.secret

| Type    | Default      |
| ------- | ------------ |
| string  | `emqxsecret` |

##### 说明

设置 HMAC Secret。

<br />

### auth.jwt.from

| Type | Optional Value         | Default    |
| ---- | ---------------------- | ---------- |
| enum | `username`, `password` | `password` |

##### 说明

从什么地方获取 JWT。可选值为:

- username: MQTT CONNECT 报文的 username 字段作为 JWT。
- password: MQTT CONNECT 报文的 password 字段作为 JWT。

<br />

### auth.jwt.pubkey

| Type    | Default                        |
| ------- | ------------------------------ |
| string  | `etc/certs/jwt_public_key.pem` |

##### 说明

若使用 RSA 或者 ECDSA 加密算法，须指定私钥文件。

<br />

### auth.jwt.verify_claims

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### 说明

启用或关闭 Claims 校验功能。

<br />

### auth.jwt.verify_claims.<claims>

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

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

<br />

## [emqx-auth-ldap](https://github.com/emqx/emqx-auth-ldap)

### auth.ldap.servers

| Type     | Default     |
| -------- | ----------- |
| string   | `127.0.0.1` |

##### 说明

LDAP 服务地址。

<br />

### auth.ldap.port

| Type     | Default |
| -------- | ------- |
| integer  | 389     |

##### 说明

LDAP 服务端口。

<br />

### auth.ldap.pool

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| integer  | > 0            | 8       |

##### 说明

连接池大小。

<br />

### auth.ldap.bind_dn

| Type     | Default                 |
| -------- | ----------------------- |
| string   | `cn=root,dc=emqx,dc=io` |

##### 说明

登入 LDAP 服务的 DN。

<br />

### auth.ldap.bind_password

| Type     | Default  |
| -------- | -------- |
| string   | `public` |

##### 说明

登入 LDAP 服务的密码。

<br />

### auth.ldap.timeout

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### 说明

查询操作的超时时间。

<br />

### auth.ldap.device_dn

| Type     | Default                    |
| -------- | -------------------------- |
| string   | `ou=device,dc=emqx,dc=io`  |

##### 说明

客户端隶属的 DN。

<br />

### auth.ldap.match_objectclass

| Type     | Default     |
| -------- | ----------- |
| string   | `mqttUser`  |

##### 说明

客户端对象的名称。

<br />

### auth.ldap.username.attributetype

| Type     | Default |
| -------- | ------- |
| string   | `uid`   |

##### 说明

Username 属性的数据类型。

<br />

### auth.ldap.password.attributetype

| Type     | Default          |
| -------- | ---------------- |
| string   | `userPassword`   |

##### 说明

Password 属性的数据类型。

<br />

### auth.ldap.ssl

| Type     | Optional Value   | Default |
| -------- | ---------------- | ------- |
| enum     | `true`, `false`  | `false` |

##### 说明

是否开启 SSL。

<br />

### auth.ldap.ssl.certfile

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### 说明

SSL 服务端证书路径。

<br />

### auth.ldap.ssl.keyfile

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### 说明

SSL 服务端秘钥文件路径。

<br />

### auth.ldap.ssl.cacertfile

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### 说明

CA 证书文件路径。

<br />

### auth.ldap.ssl.verify

| Type     | Optional Value                | Default |
| -------- | ----------------------------- | ------- |
| enum     | `verify_peer`, `verify_none`  | -       |

##### 说明

SSL 认证方式：

- `verify_none`：单向认证。
- `verify_peer`：双向认证。

<br />

### auth.ldap.ssl.fail_if_no_peer_cert

| Type     | Optional Value   | Default |
| -------- | ---------------- | ------- |
| enum     | `true`, `false`  | `false` |

##### 说明

如果客户端未提供 SSL 证书，则断开连接。

<br />

## [emqx-auth-mongo](https://github.com/emqx/emqx-auth-mongo)

### auth.mongo.type

| Type | Optional Value                      | Default  |
| ---- | ----------------------------------- | -------- |
| enum | `single`, `unknown`, `sharded`, `rs`| `single` |

##### 说明

设置 MongoDB 的拓扑类型:

- single: 单节点

- unknown: 未知

- sharded: 分片模式

- rs: 副本模式 (replicated set)

<br />

### auth.mongo.rs_set_name

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

在使用 rs 模式的情况下，设置 rs 的名字。

<br />

### auth.mongo.rs_set_name

| Type   | Default           |
| ------ | ----------------- |
| string | `127.0.0.1:27017` |

##### 说明

设置 MongoDB 服务的地址。如有多个使用逗号 `,` 分隔。

<br />

### auth.mongo.pool

| Type    | Default |
| ------- | ------- |
| integer | 8       |

##### 说明

设置 MongoDB 连接池的进程数。

<br />

### auth.mongo.login

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

设置 MongoDB 的用户名。

<br />

### auth.mongo.password

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

设置 MongoDB 的密码。

<br />

### auth.mongo.auth_source

| Type   | Default |
| ------ | ------- |
| string | `mqtt`  |

##### 说明

设置 MongoDB 的认证源数据库名。

<br />

### auth.mongo.database

| Type   | Default |
| ------ | ------- |
| string | `mqtt`  |

##### 说明

设置 MongoDB 的数据库名。

<br />

### auth.mongo.query_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### 说明

设置访问 MongoDB 超时时间。

<br />

### auth.mongo.ssl

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

设置是否使用 SSL 访问 MongoDB。

<br />

### auth.mongo.ssl_opts.keyfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

若使用 SSL 访问 MongoDB，设置 SSL 客户端的私钥文件。

<br />

### auth.mongo.ssl_opts.certfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

若使用 SSL 访问 MongoDB，设置 SSL 客户端的证书文件。

<br />

### auth.mongo.ssl_opts.cacertfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

若使用 SSL 访问 MongoDB，设置 SSL 的 CA 证书文件。

<br />

### auth.mongo.w_mode

| Type | Optional Value            | Default |
| ---- | ------------------------- | ------- |
| enum | `unsafe`, `safe`, `undef` | `undef` |

##### 说明

设置 MongoDB 的写入模式。

<br />

### auth.mongo.r_mode

| Type | Optional Value                | Default |
| ---- | ----------------------------- | ------- |
| enum | `master`, `slave_ok`, `undef` | `undef` |

##### 说明

设置 MongoDB 的读取模式。

<br />

### auth.mongo.auth_query.collection

| Type   | Default     |
| ------ | ----------- |
| string | `mqtt_user` |

##### 说明

认证过程用的 Collection 名字。

<br />

### auth.mongo.auth_query.password_field

| Type   | Default    |
| ------ | ---------- |
| string | `password` |

##### 说明

认证过程用的主要字段。如需在密码之后加 salt，可以配置为:

```
auth.mongo.auth_query.password_field = password,salt
```

<br />

### auth.mongo.auth_query.password_hash

| Type |               Optional Value              | Default  |
| ---- | ----------------------------------------- | -------- |
| enum | `plain`, `md5`, `sha`, `sha256`, `bcrypt` | `sha256` |

##### 说明

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

<br />

### auth.mongo.auth_query.selector

| Type   | Default       |
| ------ | ------------- |
| string | `username=%u` |

##### 说明

认证过程执行的 MongoDB 语句。命令可支持通配符:

- %u: username
- %c: clientid
- %C: 客户端 TLS 证书里的 Common Name
- %d: 客户端 TLS 证书里的 Subject

<br />

### auth.mongo.auth_query.super_query

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

认证中是否使用 SuperUser。

<br />

### auth.mongo.super_query.collection

| Type   | Default     |
| ------ | ----------- |
| string | `mqtt_user` |

##### 说明

若使用 SuperUser，指定 SuperUser 的 MongoDB Collection。

<br />

### auth.mongo.super_query.selector

| Type   | Default                    |
| ------ | -------------------------- |
| string | `username=%u, clientid=%c` |

##### 说明

若使用 SuperUser，指定查询 SuperUser 使用的 MongoDB 语句。

<br />

### auth.mongo.acl_query

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

是否开启 ACL 功能。

<br />

### auth.mongo.acl_query.collection

| Type   | Default    |
| ------ | ---------- |
| string | `mqtt_acl` |

##### 说明

若使用 ACL 功能，指定查询 ACL 规则的 MongoDB Collection。

<br />

### auth.mongo.acl_query.selector

| Type   | Default       |
| ------ | ------------- |
| string | `username=%u` |

##### 说明

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

<br />

### auth.mongo.topology.pool_size

| Type    | Default |
| ------- | ------- |
| integer | 1       |

##### 说明

MongoDB 拓扑参数，设置线程池大小。

<br />

### auth.mongo.topology.max_overflow

| Type    | Default |
| ------- | ------- |
| integer | 0       |

##### 说明

MongoDB 拓扑参数，当线程池中所有 workers 都处于忙碌状态时，允许创建多少额外的 worker 线程。

<br />

### auth.mongo.topology.overflow_ttl

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

MongoDB 拓扑参数，当有 worker 空闲时。多久之后释放额外的 worker 线程。单位: 毫秒。

<br />

### auth.mongo.topology.overflow_check_period

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

MongoDB 拓扑参数，多长时间检查一次有无空闲线程，以释放额外的 worker。

<br />

### auth.mongo.topology.local_threshold_ms

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

MongoDB 拓扑参数，选择用来处理用户请求的 Secondary 节点的策略。记到所有节点的 RTT 中的最小值为 LowestRTT，那么只有那些 RTT < LowestRTT + local_threshold_ms 的 Secondary 节点会被选择。

<br />

### auth.mongo.topology.connect_timeout_ms

| Type    | Default |
| ------- | ------- |
| integer | 20000   |

##### 说明

MongoDB 拓扑参数，MongoDB 连接超时时间，单位: 毫秒。

<br />

### auth.mongo.topology.socket_timeout_ms

| Type    | Default |
| ------- | ------- |
| integer | 100     |

##### 说明

MongoDB 拓扑参数，MongoDB 消息发送超时时间，单位: 毫秒。

<br />

### auth.mongo.topology.server_selection_timeout_ms

| Type    | Default |
| ------- | ------- |
| integer | 30000   |

##### 说明

MongoDB 拓扑参数，选择 MongoDB Server 的超时时间，单位: 毫秒。

<br />

### auth.mongo.topology.wait_queue_timeout_ms

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

MongoDB 拓扑参数，从线程池中选取 worker 的等待超时时间，单位: 毫秒。

<br />

### auth.mongo.topology.heartbeat_frequency_ms

| Type    | Default |
| ------- | ------- |
| integer | 10000   |

##### 说明

MongoDB 拓扑参数，拓扑扫描之间的间隔时间，单位: 毫秒。

<br />

### auth.mongo.topology.min_heartbeat_frequency_ms

| Type    | Default |
| ------- | ------- |
| integer | 1000    |

##### 说明

MongoDB 拓扑参数，`heartbeat_frequency_ms` 允许的最小值，单位: 毫秒。

<br />

## [emqx-auth-mysql](https://github.com/emqx/emqx-auth-mysql)

### auth.mysql.server

| Type | Default          |
| ---- | ---------------- |
| ip   | `127.0.0.1:3306` |

##### 说明

MySQL 服务器地址。

<br />

### auth.mysql.pool

| Type    | Default |
| ------- | ------- |
| integer | 8       |

##### 说明

数据库连接线程池大小。

<br />

### auth.mysql.username

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

MySQL 用户名。

<br />

### auth.mysql.password

| Type   | Default |
| ------ | ------- |
| string | -      |

##### 说明

MySQL 密码。

<br />

### auth.mysql.database

| Type   | Default |
| ------ | ------- |
| string | `mqtt`  |

##### 说明

MySQL 数据库名称。

<br />

### auth.mysql.query_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### 说明

MySQL 数据查询超时时间。查询超时将等同于未找到用户数据处理。

<br >

### auth.mysql.auth_query

| Type   | Default                                                        |
| ------ | -------------------------------------------------------------- |
| string | `select password from mqtt_user where username = '%u' limit 1` |

##### 说明

认证时使用的 MySQL 选取语句，选取出来的数据将与经过由 `auth.mysql.password_hash` 指定的加密方式加密的密码进行比较，比较后内容一致的客户端将被允许登录。加盐后存储的密码需要同时选取盐对应的字段，例如 `select password, salt from mqtt_user where username = '%u' limit 1`。`password` 与 `salt` 字段名不可以修改，表名与 WHERE 子句中的字段名可以视情况变化。WHERE 子句支持以下占位符：

| 占位符 | 说明                                                      |
| ------ | --------------------------------------------------------- |
| `%u`   | 将被替换为 MQTT 客户端在 CONNECT 报文中指定的用户名       |
| `%c`   | 将被替换为 MQTT 客户端在 CONNECT 报文中指定的客户端标识符 |
| `%C`   | 将被替换为 TLS 连接时客户端证书中的 Common Name           |
| `%d`   | 将被替换为 TLS 连接时客户端证书中的 Subject               |

<br />

### auth.mysql.password_hash

| Type   | Default  |
| ------ | ------- |
| string | `sh256` |

##### 说明

存储在数据库的密码所使用的加密方式。支持以下加密方式：

- `plain`，支持前后加盐，例如 `salt,plain`
- `md5`，支持前后加盐
- `sha`，支持前后加盐
- `sha256`，支持前后加盐
- `sha512`，支持前后加盐
- `pbkdf2`，格式为 `pbkdf2,<Hashfun>,<Iterations>,<Dklen>`。其中，`<Hashfun>` 为使用的哈希函数，支持 `md4`，`md5`，`ripemd160`，`sha`，`sha224`，`sha256`，`sha384`，`sha512`，`<Iterations>` 为迭代次数，`<Dklen>` 为导出密钥长度。示例：`pbkdf2,sha256,1000,20`
- `bcrypt`，仅支持前向加盐，例如 `salt,bcrypt`

<br />

### auth.mysql.super_query

| Type   | Default                                                            |
| ------ | ------------------------------------------------------------------ |
| string | `select is_superuser from mqtt_user where username = '%u' limit 1` |

##### 说明

超级用户认证时使用的 SQL 选取语句，此语句中所有表名与字段名都可视情况修改，当且仅当选取得到字段的值为 `1` 时，该用户为超级用户。WHERE 子句中支持的占位符与 `auth.mysql.auth_query` 相同。

<br />

### auth.mysql.acl_query

| Type   | Default |
| ------ | ------- |
| string | `select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'` |

##### 说明

ACL 校验时使用的 SQL 选取语句，此语句中所有表名与字段名都可视情况修改。WHERE 子句中支持的占位符如下：

| 占位符 | 说明                                                      |
| ------ | --------------------------------------------------------- |
| `%a`   | 将被替换为客户端 IP 地址                                  |
| `%u`   | 将被替换为 MQTT 客户端在 CONNECT 报文中指定的用户名       |
| `%c`   | 将被替换为 MQTT 客户端在 CONNECT 报文中指定的客户端标识符 |

<br />

## [emqx-auth-pgsql](https://github.com/emqx/emqx-auth-pgsql)

### auth.pgsql.server

| Type | Default          |
| ---- | ---------------- |
| ip   | `127.0.0.1:5432` |

##### 说明

PostgreSQL 服务器地址。

<br />

### auth.pgsql.pool

| Type    | Default |
| ------- | ------- |
| integer | 8       |

##### 说明

数据库连接线程池大小。

<br />

### auth.pgsql.username

| Type   | Default |
| ------ | ------- |
| string | `root`  |

##### 说明

PostgreSQL 用户名。

<br />

### auth.pgsql.password

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

PostgreSQL 密码。

<br />

### auth.pgsql.database

| Type   | Default |
| ------ | ------- |
| string | `mqtt`  |

##### 说明

PostgreSQL 数据库名称。

<br />

### auth.pgsql.encoding

| Type   | Default |
| ------ | ------- |
| string | `utf8`  |

##### 说明

PostgreSQL 数据库字符编码格式。

<br />

### auth.pgsql.ssl

| Type   | Optional Value  | Default |
| ------ | --------------- | ------- |
| enum   | `true`, `false` | `false` |

##### 说明

是否启用 TLS 连接。

<br />

### auth.pgsql.ssl_opts.keyfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

客户端私钥文件路径。

<br />

### auth.pgsql.ssl_opts.certfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

客户端证书文件路径。

<br />

### auth.pgsql.ssl_opts.cacertfile

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

客户端 CA 证书文件路径。

<br />

### auth.pgsql.auth_query

| Type   | Default                                                        |
| ------ | -------------------------------------------------------------- |
| string | `select password from mqtt_user where username = '%u' limit 1` |

##### 说明

认证时使用的 SQL 选取语句，同 `auth.mysql.auth_query`。

<br />

### auth.pgsql.password_hash

| Type   | Default |
| ------ | ------- |
| string | `sh256` |

##### 说明

存储在数据库的密码所使用的加密方式，同 `auth.mysql.password_hash`。

<br />

### auth.pgsql.super_query

| Type   | Default |
| ------ | ------- |
| string | `select is_superuser from mqtt_user where username = '%u' limit 1` |

##### 说明

超级用户认证时使用的 SQL 选取语句，同 `auth.mysql.super_query`。

<br />

### auth.pgsql.acl_query

| Type   | Default |
| ------ | ------- |
| string | `select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'` |

##### 说明

ACL 校验时使用的 SQL 选取语句，同 `auth.mysql.acl_query`。

<br />

## [emqx-auth-redis](https://github.com/emqx/emqx-auth-redis)

### auth.redis.type

| Type     | Optional Value                  | Default   |
| -------- | ------------------------------- | --------- |
| enum     | `single`, `sentinel`, `cluster` | `single`  |

##### 说明

Redis 服务集群类型：
- `single`：单节点服务。
- `sentinel`：哨兵模式。
- `cluster`：集群模式。

<br />

### auth.redis.server

| Type     | Default            |
| -------- | ------------------ |
| string   | `127.0.0.1:6379`   |

##### 说明

Redis 服务地址，如果有多个则以逗号分隔。例如，`192.168.0.1:6379, 192.168.0.2:6379`。

<br />

### auth.redis.sentinel

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### 说明

Redis sentinel 模式下的集区名称。如果非 `sentinel` 模式，则不需要配置。

<br />

### auth.redis.pool

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| integer  | > 0            | 8       |

##### 说明

连接池大小。

<br />

### auth.redis.database

| Type     | Default |
| -------- | ------- |
| integer  | 0       |

##### 说明

要连接的 Redis 数据库序号。

<br />

### auth.redis.password

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### 说明

Redis 用户密码。

<br />

### auth.redis.query_timeout

| Type     | Default |
| -------- | ------- |
| duration | `5s`    |

##### 说明

Redis 查询超时时间。

<br />

### auth.redis.auth_cmd

| Type     | Default                       |
| -------- | ----------------------------- |
| string   | `HMGET mqtt_user:%u password` |

##### 说明

认证查询命令，可用站位符有：
 - `%u`：客户端用户名。
 - `%c`：客户端标识。
 - `%C`：客户端 SSL 证书的 `cn`。
 - `%d`：客户端 SSL 证书的 `dn`。

<br />

### auth.redis.password_hash

| Type     | Optional Value                             | Default |
| -------- | ------------------------------------------ | ------- |
| enum     | `plain`, `md5`, `sha`, `sha256`, `bcrypt`  | `plain` |

##### 说明

Redis 存储的 `password` 字段的编码格式。

<br />

### auth.redis.super_cmd

| Type     | Default                          |
| -------- | -------------------------------- |
| string   | `HGET mqtt_user:%u is_superuser` |

##### 说明

超级用户查询命令，可用的占位符有：
 - `%u`：客户端用户名。
 - `%c`：客户端标识。
 - `%C`：客户端 SSL 证书的 `cn`。
 - `%d`：客户端 SSL 证书的 `dn`。

<br />

### auth.redis.acl_cmd

| Type     | Default               |
| -------- | --------------------- |
| string   | `HGETALL mqtt_acl:%u` |

##### 说明

ACL 查询命令。可用的占位符有：
 - `%u`：客户端用户名。
 - `%c`：客户端标识。

<br />

## [emqx-auth-username](https://github.com/emqx/emqx-auth-username)

### auth.user.<Number>.username` & `auth.user.<Number>.password

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

客户端的认证数据，其中 `auth.user.<Number>.password` 为明文密码。`<Number>` 相同的 `auth.user.<Number>.username` 与 `auth.user.<Number>.password` 必须成对出现。`<Number>` 是一个整型数字，用于区分多个客户端的认证数据。

<br />

### auth.user.password_hash

| Type | Optional Value                  | Default  |
| ---- | ------------------------------- | -------- |
| enum | `plain`, `md5`, `sha`, `sha256` | `sha256` |

##### 说明

密码存储至数据库时使用的 Hash 算法。以下选项可用：

`plain`

密码以明文形式存储。

`md5`

密码使用 MD5 算法加密后存储。

`sha`

密码使用 SHA-1 算法加密后存储。

`sha256`

密码使用 SHA-256 算法加密后存储。

<br />

## [emqx-bridge-mqtt](https://github.com/emqx/emqx-bridge-mqtt)

### bridge.mqtt.aws.address

| Type     | Default          |
| -------- | ---------------- |
| string   | `127.0.0.1:1883` |

##### 说明

桥接地址，支持两种格式，例如：
- `emqx@192.168.0.100`：EMQ X 节点名称，它表示将该节点的消息桥接到另外一个 EMQ X 节点。
- `192.168.0.100:1883`：IP 地址和端口，它表示将该节点的消息通过一个 MQTT 连接桥接到另外一个 MQTT 服务器。

<br />

### bridge.mqtt.aws.proto_ver

| Type     | Optional Value               | Default  |
| -------- | ---------------------------- | -------- |
| enum     | `mqttv3`, `mqttv4`, `mqttv5` | `mqttv4` |

##### 说明

MQTT 桥接的客户端协议版本。

<br />

### bridge.mqtt.aws.start_type

| Type     | Optional Value    | Default  |
| -------- | ----------------- | -------- |
| eunm     | `manual`, `auto`  | `manual` |

##### 说明

启动类型：
- `auto`：跟随插件自动启动。
- `manual`：手动启动桥接。

<br />

### bridge.mqtt.aws.bridge_mode

| Type     | Optional Value   | Default |
| -------- | ---------------- | ------- |
| boolean  | `true`, `false`  | `true`  |

##### 说明

是否开启桥接模式，仅 MQTT 桥接支持。开启后 `emqx_bridge_mqtt` 启动的 MQTT 客户端在发送连接报文时会携带一个标志位，标识这是一个桥接客户端。

注：RabbitMQ 目前不支持该标志。

<br />

### bridge.mqtt.aws.clientid

| Type     | Default      |
| -------- | ------------ |
| string   | `bridge_aws` |

##### 说明

MQTT 桥接的客户端标识。

<br />

### bridge.mqtt.aws.clean_start

| Type     | Optional Value   | Default |
| -------- | ---------------- | ------- |
| boolean  | `true`, `false`  | `true`  |

##### 说明

MQTT 桥接的 `clean_start` 标志。它表示客户端是否以 `清楚会话` 的方式连接到远程 MQTT Broker。

<br />

### bridge.mqtt.aws.username

| Type     | Default |
| -------- | ------- |
| string   | `user`  |

##### 说明

MQTT 桥接客户端的用户名。

<br />

### bridge.mqtt.aws.password

| Type     | Default  |
| -------- | -------- |
| string   | `passwd` |

##### 说明

MQTT 桥接客户端的密码。

<br />

### bridge.mqtt.aws.forwards

| Type     | Default             |
| -------- | ------------------- |
| string   | `topic1/#,topic2/#` |

##### 说明

桥接转发规则。例如：
- `topic1/#, topic2/#`：`emqx_bridge_mqtt` 会将 EMQ X 中所以与 `topic1/#`，`topic2/#` 匹配的主题消息进行转发。

<br />

### bridge.mqtt.aws.forward_mountpoint

| Type     | Default               |
| -------- | --------------------- |
| string   | `bridge/aws/${node}/` |

##### 说明

转发主题的前缀。将消息转发到目标系统时，支持给该主题添加一个统一的前缀。

<br />

### bridge.mqtt.aws.subscription.1.topic

| Type     | Default |
| -------- | ------- |
| string   | -       |

##### 说明

订阅对端系统的主题。

<br />

### bridge.mqtt.aws.subscription.1.qos

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| enum     | `0`, `1`, `2`  | `1`     |

##### 说明

订阅对端系统主题的 QoS。

<br />

### bridge.mqtt.aws.receive_mountpoint

| Type     | Default        |
| -------- | -------------- |
| string   | `receive/aws/` |

##### 说明

接收消息的主题前缀。`emqx_bridge_mqtt` 支持给来着对端的消息添加一个统一的主题前缀。

<br />

### bridge.mqtt.aws.ssl

| Type     | Optional Value  | Default |
| -------- | --------------- | ------- |
| boolean  | `true`, `false` | `true`  |

##### 说明

MQTT 桥接客户端是否开启 SSL。

<br />

### bridge.mqtt.aws.cacertfile

| Type     | Default                |
| -------- | ---------------------- |
| string   | `etc/certs/cacert.pem` |

##### 说明

MQTT 桥接客户端的 CA 证书文件路径。

<br />

### bridge.mqtt.aws.certfile

| Type     | Default                     |
| -------- | --------------------------- |
| string   | `etc/certs/client-cert.pem` |

##### 说明

MQTT 桥接客户端的 SSL 证书文件路径。

<br />

### bridge.mqtt.aws.keyfile

| Type     | Default                    |
| -------- | -------------------------- |
| string   | `etc/certs/client-key.pem` |

##### 说明

MQTT 桥接客户端的 SSL 秘钥文件路径。

<br />

### bridge.mqtt.aws.ciphers

| Type     | Default                                                     |
| -------- | ----------------------------------------------------------- |
| string   | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384` |

##### 说明

SSL 握手支持的加密套件。

<br />

### bridge.mqtt.aws.psk_ciphers

| Type     | Default                                                                  |
| -------- | ------------------------------------------------------------------------ |
| string   | `PSK-AES128-CBC-SHA,PSK-AES256-CBC-SHA,PSK-3DES-EDE-CBC-SHA,PSK-RC4-SHA` |

##### 说明

SSL PSK 握手支持的加密套件。

<br />

### bridge.mqtt.aws.keepalive

| Type     | Default |
| -------- | ------- |
| duration | `60s`   |

##### 说明

MQTT 桥接客户端的心跳间隔。

<br />

### bridge.mqtt.aws.tls_versions

| Type     | Default                 |
| -------- | ----------------------- |
| string   | `tlsv1.2,tlsv1.1,tlsv1` |

##### 说明

MQTT 桥接客户端的 SSL 版本。

<br />

### bridge.mqtt.aws.reconnect_interval

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### 说明

重连间隔。

<br />

### bridge.mqtt.aws.retry_interval

| Type     | Default |
| -------- | ------- |
| duration | `20s`   |

##### 说明

QoS 1/2 消息重发间隔。

<br />

### bridge.mqtt.aws.batch_size

| Type     | Default |
| -------- | ------- |
| integer  | 32      |

##### 说明

EMQ X 桥接的批处理大小。`emqx_bridge_mqtt` 的 EMQ X 桥接模式支持批量发送消息以提搞吞吐。

<br />

### bridge.mqtt.aws.max_inflight_size

| Type     | Default |
| -------- | ------- |
| integer  | 32      |

##### 说明

飞行窗口大小。

<br />

### bridge.mqtt.aws.queue.replayq_dir

| Type     | Default                  |
| -------- | ----------------------- |
| string   | `etc/emqx_aws_bridge/`  |

##### 说明

设置消息队列文件路径。不配置则仅使用内存存储。

<br />

### bridge.mqtt.aws.queue.replayq_seg_bytes

| Type     | Default |
| -------- | ------- |
| bytesize | `10MB`  |

##### 说明

消息队列存储在磁盘的单个文件大小。

<br />

### bridge.mqtt.aws.queue.max_total_size

| Type     | Default |
| -------- | ------- |
| bytesize | `5GB`   |

##### 说明

消息队列允许存储的最大值。

<br />

## [emqx-coap](https://github.com/emqx/emqx-coap)

### coap.port

| Type    | Default |
| ------- | ------- |
| integer | 5683    |

##### 说明

指定 CoAP 插件的 UDP 绑定端口。

<br />

### coap.enable_stats

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### 说明

启用或关闭 CoAP 的统计功能。

<br />

### coap.dtls.port

| Type    | Default |
| ------- | ------- |
| integer | 5684    |

##### 说明

指定 CoAP 插件的 DTLS 绑定端口。

<br />

### coap.dtls.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------- | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### 说明

使用 DTLS 时，指定 DTLS 握手过程中是否校验客户端。

<br />

### coap.dtls.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### 说明

使用 DTLS 时，指定 DTLS 的私钥文件。

<br />

### coap.dtls.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### 说明

使用 DTLS 时，指定 DTLS 的证书文件。

<br />

### coap.dtls.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### 说明

使用 DTLS 时，指定 DTLS 的 CA 证书文件。

<br />

### coap.dtls.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

使用 DTLS 时，DTLS 握手过程中若客户端没有证书，是否让握手失败。

<br />

### coap.dtls.ciphers

| Type | Default |
| ---- | ------- |
| string | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA` |

##### 说明

使用 DTLS 时，指定 DTLS 服务端支持的 Cipher 列表。

<br />

## [emqx-dashboard](https://github.com/emqx/emqx-dashboard)

### dashboard.default_user.login` & `dashboard.default_user.password

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

Dashboard 默认用户的认证数据。`dashboard.default_user.login` 与 `dashboard.default_user.password` 必须同时存在。

<br />

### dashboard.listener.http

| Type    | Default |
| ------- | ------- |
| integer | 18083   |

##### 说明

HTTP 监听器的监听端口。

<br />

### dashboard.listener.http.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 4       |

##### 说明

此监听器将创建的监听进程数量。

<br />

### dashboard.listener.http.max_clients

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### 说明

此监听器允许同时建立的最大连接数量限制。

<br />

### dashboard.listener.http.inet6

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `false` |

##### 说明

是否设置套接字允许 IPv6 连接。

<br />

### dashboard.listener.http.ipv6_v6only

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `false` |

##### 说明

是否限制套接字仅使用 IPv6，禁止任何 IPv4 连接。仅适用于 IPv6 套接字，即仅在 `dashboard.listener.http.inet6` 被设置为 `true` 时此配置项的值有实际意义。需要注意的是，在某些操作系统上，例如 Windows，此配置项唯一允许的值为 `true`。

<br />

### dashboard.listener.https

| Type    | Default |
| ------- | ------- |
| integer | 18084   |

##### 说明

HTTPS 监听器的监听端口，**默认此监听器被禁用**。

<br />

### dashboard.listener.https.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 2       |

##### 说明

同 `dashboard.listener.http.acceptors`。

<br />

### dashboard.listener.https.max_clients

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### 说明

同 `dashboard.listener.http.max_clients`。

<br />

### dashboard.listener.https.inet6

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `false` |

##### 说明

同 `dashboard.listener.http.inet6`。

<br />

### dashboard.listener.https.ipv6_v6only

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `false` |

##### 说明

同 `dashboard.listener.http.ipv6_v6only`。

<br />

### dashboard.listener.https.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### 说明

服务端私钥文件路径。

<br />

### dashboard.listener.https.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### 说明

服务端证书文件路径。

<br />

### dashboard.listener.https.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### 说明

CA 证书文件路径。

<br />

### dashboard.listener.https.dhfile

| Type   | Default                   |
| ------ | ------------------------- |
| string | `etc/certs/dh-params.pem` |

##### 说明

如果协商使用 Diffie Hellman 密钥交换的密码套件，则可以通过此配置项指定包含 PEM 编码的 Diffie Hellman 参数的文件路径。 如果未指定，则使用默认参数。

<br />

### dashboard.listener.https.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------- | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### 说明

`verify_none` 表示关闭对端证书验证，服务端不会向客户端发出证书请求。`verify_peer` 表示开启对端证书验证，服务端会向客户端发出证书请求。当此配置项被设置为 `verify_peer` 时，通常需要配合 `dashboard.listener.https.fail_if_no_peer_cert` 一起使用，以指定是否强制客户端提供证书。

<br />

### dashboard.listener.https.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `true`  |

##### 说明

必须配合 `dashboard.listener.https.verify` 一起使用。如果设置为 `true`，则服务端向客户端请求证书时如果客户端不提供证书将导致握手失败。如果设置为 `false`，则客户端即使不提供证书也能握手成功。

<br />

### dashboard.listener.https.tls_versions

| Type   | Default                 |
| ------ | ----------------------- |
| string | `tlsv1.2,tlsv1.1,tlsv1` |

##### 说明

指定服务端支持的 TLS 协议版本，版本之间由 `,` 分隔，支持的 TLS 协议版本有： `tlsv1.3`, `tlsv1.2`, `tlsv1.1`, `tlsv1`, `sslv3`。

<br />

### dashboard.listener.https.ciphers

| Type   | Default                                                      |
| ------ | ------------------------------------------------------------ |
| string | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA` |

##### 说明

指定服务端支持的加密套件。

<br />

### dashboard.listener.https.secure_renegotiate

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### 说明

指定是否启动安全重协商机制。

<br />

### dashboard.listener.https.reuse_sessions

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

指定是否启用会话复用机制。

<br />

### dashboard.listener.https.honor_cipher_order

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

如果设置为 `on`，则使用服务器的首选项进行密码选择。 如果设置为 `off`，则使用客户端的首选项。

<br />

## [emqx-lwm2m](https://github.com/emqx/emqx-lwm2m)

### lwm2m.port

| Type    | Default |
| ------- | ------- |
| integer | 5683    |

##### 说明

指定 LwM2M 使用的 UDP 端口。

<br />

### lwm2m.lifetime_min

| Type     | Default |
| -------- | ------- |
| duration | `1s`    |

##### 说明

指定允许的 LwM2M lifetime 最小值，单位: 秒。

<br />

### lwm2m.lifetime_max

| Type     | Default  |
| -------- | -------- |
| duration | `86400s` |

##### 说明

指定允许的 LwM2M lifetime 最大值，单位: 秒。

<br />

### lwm2m.qmode_time_window

| Type    | Default |
| ------- | ------- |
| integer | 22      |

##### 说明

指定 LwM2M Q 模式使用的窗口大小，单位: 秒。

<br />

这个窗口期之内可以下发执行给 Q 模式的设备，过了窗口期则缓存下行数据。

### lwm2m.lb

| Type | Optional Value          | Default     |
| ---- | ----------------------- | ----------- |
| enum | `coaproxy`, `undefined` | `undefined` |

##### 说明

设置是否使用 coaproxy。设置为 `undefined` 则不使用 coaproxy。

<br />

### lwm2m.auto_observe

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### 说明

在设备注册后是否自动下发 observe 命令。

<br />

### lwm2m.mountpoint

| Type   | Default     |
| ------ | ----------- |
| string | `lwm2m/%e/` |

##### 说明

设置 LwM2M 主题的挂载点。支持以下通配符:

- '%e': Endpoint Name
- '%a': IP Address

<br />

### lwm2m.topics.command

| Type   | Default |
| ------ | ------- |
| string | `dn/#`  |

##### 说明

设备注册完成后，需要订阅的下行命令主题。

<br />

### lwm2m.topics.response

| Type   | Default   |
| ------ | --------- |
| string | `up/resp` |

##### 说明

设备的上行回复需要发布到哪个主题。

<br />

### lwm2m.topics.notify

| Type   | Default     |
| ------ | ----------- |
| string | `up/notify` |

##### 说明

设备的上行报告消息 (notify) 需要发布到哪个主题。

<br />

### lwm2m.topics.register

| Type   | Default   |
| ------ | --------- |
| string | `up/resp` |

##### 说明

设备的上行注册消息 (register) 需要发布到哪个主题。

<br />

### lwm2m.topics.update

| Type   | Default   |
| ------ | --------- |
| string | `up/resp` |

##### 说明

设备的上行更新消息 (update) 需要发布到哪个主题。

<br />

### lwm2m.opts.buffer

| Type     | Default  |
| -------- | -------- |
| bytesize | `1024KB` |

##### 说明

UDP 调优参数，指定 UDP 用户态缓存大小。

<br />

### lwm2m.opts.recbuf

| Type     | Default  |
| -------- | -------- |
| bytesize | `1024KB` |

##### 说明

UDP 调优参数，指定 UDP 接收缓存大小。

<br />

### lwm2m.opts.sndbuf

| Type     | Default  |
| -------- | -------- |
| bytesize | `1024KB` |

##### 说明

UDP 调优参数，指定 UDP 发送缓存大小。

<br />

### lwm2m.opts.read_packets

| Type    | Default |
| ------- | ------- |
| integer | 20      |

##### 说明

UDP 调优参数，指定每次从 UDP socket 读取多少个报文。

<br />

### lwm2m.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### 说明

指定 UDP DTLS 使用的证书文件。

<br />

### lwm2m.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### 说明

指定 UDP DTLS 使用的私钥文件。

<br />

### lwm2m.xml_dir

| Type | Default         |
| ---- | --------------- |
| dir  | `etc/lwm2m_xml` |

##### 说明

指定 LwM2M Object 定义文件存放的目录。

<br />

## [emqx-management](https://github.com/emqx/emqx-management)

### management.max_row_limit

| Type    | Default |
| ------- | ------- |
| integer | 10000   |

##### 说明

分页查询时返回的最大记录数量。

<br />

### management.default_application.id

| Type   | Default |
| ------ | ------- |
| string | `admin` |

##### 说明

默认应用的 AppId。

<br />

### management.default_application.secret

| Type   | Default  |
| ------ | -------- |
| string | `public` |

##### 说明

默认应用的 AppSecret。

<br />

### management.listener.http

| Type    | Default |
| ------- | ------- |
| integer | 8081    |

##### 说明

HTTP 监听器的监听端口。

<br />

### management.listener.http.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 2       |

##### 说明

此监听器将创建的监听进程数量。

<br />

### management.listener.http.max_clients

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### 说明

此监听器允许同时建立的最大连接数量限制。

<br />

### management.listener.http.backlog

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### 说明

TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。

<br />

### management.listener.http.send_timeout

| Type    | Default |
| ------- | ------- |
| duration | `15s`  |

##### 说明

HTTP 报文发送超时时间。

<br />

### management.listener.http.send_timeout_close

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

HTTP 报文发送超时后，是否关闭该连接。

<br />

### management.listener.http.inet6

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否设置套接字允许 IPv6 连接。

<br />

### management.listener.http.ipv6_v6only

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否限制套接字仅使用 IPv6，禁止任何 IPv4 连接。仅适用于 IPv6 套接字，即仅在 `management.listener.http.inet6` 被设置为 `true` 时此配置项的值有实际意义。需要注意的是，在某些操作系统上，例如 Windows，此配置项唯一允许的值为 `true`。

<br />

### management.listener.https

| Type    | Default | Example |
| ------- | ------- | ------- |
| integer | -       | 8081    |

##### 说明

HTTPS 监听器的监听端口。

### management.listener.https.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 2       |

##### 说明

此监听器将创建的监听进程数量。

<br />

### management.listener.https.max_clients

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### 说明

此监听器允许同时建立的最大连接数量限制。

<br />

### management.listener.https.backlog

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### 说明

TCP 连接队列的最大长度。它表明了系统中允许的正在三次握手的 TCP 连接队列最大个数。

<br />

### management.listener.https.send_timeout

| Type    | Default |
| ------- | ------- |
| duration | `15s`  |

##### 说明

HTTPS 报文发送超时时间。

<br />

### management.listener.https.send_timeout_close

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

HTTPS 报文发送超时后，是否关闭该连接。

<br />

### management.listener.https.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### 说明

服务端私钥文件路径。

<br />

### management.listener.https.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### 说明

服务端证书文件路径。

<br />

### management.listener.https.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### 说明

CA 证书文件路径。

<br />

### management.listener.https.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------- | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### 说明

`verify_none` 表示关闭对端证书验证，服务端不会向客户端发出证书请求。`verify_peer` 表示开启对端证书验证，服务端会向客户端发出证书请求。当此配置项被设置为 `verify_peer` 时，通常需要配合 `management.listener.https.fail_if_no_peer_cert` 一起使用，以指定是否强制客户端提供证书。

<br />

### management.listener.https.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `ture`, `false` | `true`  |

##### 说明

必须配合 `management.listener.https.verify` 一起使用。如果设置为 `true`，则服务端向客户端请求证书时如果客户端不提供证书将导致握手失败。如果设置为 `false`，则客户端即使不提供证书也能握手成功。

<br />

### management.listener.https.inet6

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否设置套接字允许 IPv6 连接。

<br />

### management.listener.https.ipv6_v6only

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

是否限制套接字仅使用 IPv6，禁止任何 IPv4 连接。仅适用于 IPv6 套接字，即仅在 `management.listener.https.inet6` 被设置为 `true` 时此配置项的值有实际意义。需要注意的是，在某些操作系统上，例如 Windows，此配置项唯一允许的值为 `true`。

<br />

## [emqx-reloader](https://github.com/emqx/emqx-reloader)

### reloader.interval

| Type     | Default |
| -------- | ------- |
| duration | `60s`   |

##### 说明

每隔多长时间将所有代码代码热更新一次。

<br />

### reloader.logfile

| Type   | Default        |
| ------ | -------------- |
| string | `reloader.log` |

##### 说明

代码热更新的日志文件

<br />

## [emqx-retainer](https://github.com/emqx/emqx-retainer)

### retainer.storage_type

| Type | Optional Value             | Default |
| ---- | -------------------------- | ------- |
| enum | `ram`, `disc`, `disc_only` | `ram`   |

##### 说明

保留消息的存储类型，以下选项可用：

`ram`

保留消息仅存储在内存中。

`disc`

保留消息同时存储在内存和磁盘中。

`disc_only`

保留消息仅存储在磁盘中。

<br />

### retainer.max_retained_messages

| Type    | Default |
| ------- | ------- |
| integer | 0       |

##### 说明

保留消息的存储数量限制。一旦存储数量达到限制，可以替换已存在的保留消息，但不能为新的主题存储保留消息。0 表示没有限制。

<br />

### retainer.max_payload_size

| Type     | Default |
| -------- | ------- |
| bytesize | `1MB`   |

##### 说明

允许存储的保留消息的 Payload 最大长度限制。如果 Payload 超出最大限制，该保留消息可以被正常处理，但不会存储在服务端。

<br />

### retainer.expiry_interval

| Type     | Default |
| -------- | ------- |
| duration | `0`     |

##### 说明

保留消息的过期间隔，仅对协议版本低于 MQTT v5.0 的客户端生效，MQTT v5.0 客户端的保留消息过期间隔将以 `Message Expiry Interval` 的值为准。0 表示永不过期。

<br />

## [emqx-rule-engine](https://github.com/emqx/emqx-rule-engine)

### rule_engine.ignore_sys_message

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

忽略系统消息 ($SYS)。启用此选项规则引擎将不会处理系统消息。

<br />

### rule_engine.events.<event-name>

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### 说明

设置是否发布事件消息。可指定事件消息的 QoS，例如:

```
rule_engine.events.client_connected = on, qos1

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

```
SELECT * FROM "$events/client_connected"
```

<br />

## [emqx-sn](https://github.com/emqx/emqx-sn)

### mqtt.sn.port

| Type     | Default |
| -------- | ------- |
| string   | `1884`  |

##### 说明

`emqx_sn` 监听的 UDP 端口。

<br />

### mqtt.sn.advertise_duration

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### 说明

ADVERTISE 消息广播间隔，单位：秒。

<br />

### mqtt.sn.gateway_id

| Type     | Default |
| -------- | ------- |
| integer  | 1       |

##### 说明

ADVERTISE 中的 MQTT-SN 网关 ID。

<br />

### mqtt.sn.enable_stats

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| enum     | `on`, `off`    | `off`   |

##### 说明

是否开启客户端状态统计信息。

<br />

### mqtt.sn.enable_qos3

| Type     | Optional Value | Default |
| -------- | -------------- | ------- |
| enum     | `on`, `off`    | `off`   |

##### 说明

是否处理 QoS 为 -1 的消息。

<br />

### mqtt.sn.idle_timeout

| Type     | Default |
| -------- | ------- |
| duration | `30s`   |

##### 说明

建立后的发呆时间，如果这段时间内未收到任何报文，则会关闭该连接。

<br />

### mqtt.sn.predefined.topic.0

| Type     | Default    |
| -------- | ---------- |
| string   | `reserved` |

##### 说明

预定义的 Topic 与 TopicId 映射。Id 为 0 的主题是保留项，固定为 `reserved`。例如，预定义主题 `foo/bar` 的 Id 为 `1`：
```
mqtt.sn.predefined.topic.1 = foo/bar
```

<br />

### mqtt.sn.username

| Type     | Default        |
| -------- | -------------- |
| string   | `mqtt_sn_user` |

##### 说明

`emqx_sn` 连接至 EMQ X 的用户名。

<br />

### mqtt.sn.password

| Type     | Default |
| -------- | ------- |
| string   | `abc`   |

##### 说明

`emqx_sn` 连接至 EMQ X 的密码。

<br />

## [emqx-statsd](https://github.com/emqx/emqx-statsd)

### statsd.push.gateway.server

| Type   | Default                 |
| ------ | ----------------------- |
| string | `http://127.0.0.1:9091` |

##### 说明

指定 Statsd gateway 的 URI。

<br />

### statsd.interval

| Type    | Default |
| ------- | ------- |
| integer | 15000   |

##### 说明

指定 Statsd 数据的收集间隔，单位: 毫秒。

<br />

### prometheus.collector.<N>

| Type   | Default       |
| ------ | ------------- |
| string | `emqx_statsd` |

##### 说明

指定 Prometheus 的 Collector。

<br />

## [emqx-stomp](https://github.com/emqx/emqx-stomp)

### stomp.listener

| Type    | Default |
| ------- | ------- |
| integer | 61613   |

##### 说明

指定 Stomp 插件监听的本地端口。

<br />

### stomp.listener.acceptors

| Type    | Default |
| ------- | ------- |
| integer | 4       |

##### 说明

指定 Stomp 服务 Acceptor 线程池的大小。

<br />

### stomp.listener.max_connections

| Type    | Default |
| ------- | ------- |
| integer | 512     |

##### 说明

指定 Stomp 服务支持的最大连接数。

<br />

### stomp.listener.ssl

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### 说明

指定是否使用 SSL。

<br />

### stomp.listener.keyfile

| Type   | Default             |
| ------ | ------------------- |
| string | `etc/certs/key.pem` |

##### 说明

若使用 SSL，指定 SSL 的私钥文件。

<br />

### stomp.listener.certfile

| Type   | Default              |
| ------ | -------------------- |
| string | `etc/certs/cert.pem` |

##### 说明

若使用 SSL，指定 SSL 的证书文件。

<br />

### stomp.listener.cacertfile

| Type   | Default                |
| ------ | ---------------------- |
| string | `etc/certs/cacert.pem` |

##### 说明

若使用 SSL，指定 SSL 的 CA 证书文件。

<br />

### stomp.listener.dhfile

| Type   | Default                   |
| ------ | ------------------------- |
| string | `etc/certs/dh-params.pem` |

##### 说明

若使用 SSL，指定 Ephemeral Diffie-Helman 算法使用的 key 文件。

<br />

### stomp.listener.verify

| Type | Optional Value               | Default       |
| ---- | ---------------------------- | ------------- |
| enum | `verify_peer`, `verify_none` | `verify_peer` |

##### 说明

若使用 SSL，指定握手过程中是否校验客户端。

<br />

### stomp.listener.fail_if_no_peer_cert

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `false` |

##### 说明

若使用 SSL，SSL 握手过程中若客户端没有证书，是否让握手失败。

<br />

### stomp.listener.tls_versions

| Type   | Default                 |
| ------ | ----------------------- |
| string | `tlsv1.2,tlsv1.1,tlsv1` |

##### 说明

若使用 SSL，指定服务端支持的 SSL 的版本列表。

<br />

### stomp.listener.handshake_timeout

| Type     | Default |
| -------- | ------- |
| duration | `15s`   |

##### 说明

若使用 SSL，指定 SSL 握手过程的超时时间。

<br />

### stomp.listener.ciphers

| Type | Default |
| ---- | ------- |
| string | `ECDHE-ECDSA-AES256-GCM-SHA384,ECDHE-RSA-AES256-GCM-SHA384,ECDHE-ECDSA-AES256-SHA384,ECDHE-RSA-AES256-SHA384,ECDHE-ECDSA-DES-CBC3-SHA,ECDH-ECDSA-AES256-GCM-SHA384,ECDH-RSA-AES256-GCM-SHA384,ECDH-ECDSA-AES256-SHA384,ECDH-RSA-AES256-SHA384,DHE-DSS-AES256-GCM-SHA384,DHE-DSS-AES256-SHA256,AES256-GCM-SHA384,AES256-SHA256,ECDHE-ECDSA-AES128-GCM-SHA256,ECDHE-RSA-AES128-GCM-SHA256,ECDHE-ECDSA-AES128-SHA256,ECDHE-RSA-AES128-SHA256,ECDH-ECDSA-AES128-GCM-SHA256,ECDH-RSA-AES128-GCM-SHA256,ECDH-ECDSA-AES128-SHA256,ECDH-RSA-AES128-SHA256,DHE-DSS-AES128-GCM-SHA256,DHE-DSS-AES128-SHA256,AES128-GCM-SHA256,AES128-SHA256,ECDHE-ECDSA-AES256-SHA,ECDHE-RSA-AES256-SHA,DHE-DSS-AES256-SHA,ECDH-ECDSA-AES256-SHA,ECDH-RSA-AES256-SHA,AES256-SHA,ECDHE-ECDSA-AES128-SHA,ECDHE-RSA-AES128-SHA,DHE-DSS-AES128-SHA,ECDH-ECDSA-AES128-SHA,ECDH-RSA-AES128-SHA,AES128-SHA` |

##### 说明

若使用 SSL，指定服务端支持的 Cipher 列表。

<br />

### stomp.listener.secure_renegotiate

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `off`   |

##### 说明

若使用 SSL，指定在客户端不遵循 RFC 5746 的情况下，是否拒绝 renegotiation 请求。

<br />

### stomp.listener.reuse_sessions

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

若使用 SSL，指定是否支持 SSL session 重用。

<br />

### stomp.listener.honor_cipher_order

| Type | Optional Value | Default |
| ---- | -------------- | ------- |
| enum | `on`, `off`    | `on`    |

##### 说明

若使用 SSL，指定是否使用服务端的偏好设置选择 Ciphers。

<br />

### stomp.default_user.login

| Type   | Default |
| ------ | ------- |
| string | `guest` |

##### 说明

指定 Stomp 插件登录使用的 Username。

<br />

### stomp.default_user.passcode

| Type   | Default |
| ------ | ------- |
| string | `guest` |

##### 说明

指定 Stomp 插件登录使用的 Password。

<br />

### stomp.allow_anonymous

| Type | Optional Value  | Default |
| ---- | --------------- | ------- |
| enum | `true`, `false` | `true`  |

##### 说明

是否允许匿名登录。

<br />

### stomp.frame.max_headers

| Type    | Default |
| ------- | ------- |
| integer | 10      |

##### 说明

指定 Stomp 最大报文头数量。

<br />

### stomp.frame.max_header_length

| Type    | Default |
| ------- | ------- |
| integer | 1024    |

##### 说明

指定 Stomp 最大报文头长度。

<br />

### stomp.frame.max_body_length

| Type    | Default |
| ------- | ------- |
| integer | 8192    |

##### 说明

指定 Stomp 最大报文体长度。

<br />

## [emqx-web-hook](https://github.com/emqx/emqx-web-hook)

### web.hook.api.url

| Type   | Default |
| ------ | ------- |
| string | -       |

##### 说明

`emqx_web_hook` 转发的目的 Web 服务器地址。

<br />

### web.hook.encode_payload

| Type     | Optional Value      | Default |
| -------- | ------------------- | ------- |
| enum     | `base62`, `base64`  | -       |

##### 说明

PUBLISH 消息中 Payload 字段的编码格式。

<br />

### web.hook.rule.client.connect.1

| Type     | Default                           |
| -------- | --------------------------------- |
| string   | `{"action": "on_client_connect"}` |

##### 说明

转发 `收到连接报文` 事件。

<br />

### web.hook.rule.client.connack.1

| Type     | Default                           |
| -------- | --------------------------------- |
| string   | `{"action": "on_client_connack"}` |

##### 说明

转发 `下发连接应答` 事件。

<br />

### web.hook.rule.client.connected.1

| Type     | Default                             |
| -------- | ----------------------------------- |
| string   | `{"action": "on_client_connected"}` |

##### 说明

转发 `客户端成功接入` 事件。

<br />

### web.hook.rule.client.disconnected.1

| Type     | Default                                |
| -------- | -------------------------------------- |
| string   | `{"action": "on_client_disconnected"}` |

##### 说明

转发 `客户端已断开` 事件。

<br />

### web.hook.rule.client.subscribe.1

| Type     | Default                             |
| -------- | ----------------------------------- |
| string   | `{"action": "on_client_subscribe"}` |

##### 说明

转发 `将订阅` 事件。

<br />

### web.hook.rule.client.unsubscribe.1

| Type     | Default                               |
| -------- | ------------------------------------- |
| string   | `{"action": "on_client_unsubscribe"}` |

##### 说明

转发 `将取消订阅` 事件。

<br />

### web.hook.rule.session.subscribed.1

| Type     | Default                               |
| -------- | ------------------------------------- |
| string   | `{"action": "on_session_subscribed"}` |

##### 说明

转发 `已订阅` 事件。

<br />

### web.hook.rule.session.unsubscribed.1

| Type     | Default                                 |
| -------- | --------------------------------------- |
| string   | `{"action": "on_session_unsubscribed"}` |

##### 说明

转发 `已取消订阅` 事件。

<br />

### web.hook.rule.session.terminated.1

| Type     | Default                               |
| -------- | ------------------------------------- |
| string   | `{"action": "on_session_terminated"}` |

##### 说明

转发 `会话已终止` 事件。

<br />

### web.hook.rule.message.publish.1

| Type     | Default                            |
| -------- | ---------------------------------- |
| string   | `{"action": "on_message_publish"}` |

##### 说明

转发 `消息发布` 事件。

<br />

### web.hook.rule.message.delivered.1

| Type     | Default                              |
| -------- | ------------------------------------ |
| string   | `{"action": "on_message_delivered"}` |

##### 说明

转发 `消息已投递` 事件。

<br />

### web.hook.rule.message.acked.1

| Type     | Default                          |
| -------- | -------------------------------- |
| string   | `{"action": "on_message_acked"}` |

##### 说明

转发 `消息已应答` 事件。
