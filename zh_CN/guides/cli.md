# 命令行

本节向您介绍 EMQX 支持的各类启动与管理命令，并详细介绍 ctl 管理命令。

## 启动命令

EMQX 支持一些基本的启动和管理命令，您可以通过 `emqx <command>` 命令执行。

以下是常用的启动和管理命令：

| 命令       | 描述                                                         |
| ---------- | ------------------------------------------------------------ |
| start      | 以守护进程模式启动 EMQX，运行期间不需要交互式 shell。        |
| console    | 在 Erlang 或 Elixir 交互式 shell 中启动 EMQX。用于在开发环境中调试 EMQX，需要与 EMQX 进行交互。 |
| foreground | 在前台模式下启动 EMQX，不使用交互式 shell。用于在开发环境中启动 EMQX，但不需要后台运行。 |
| stop       | 停止运行中的 EMQX 节点。                                     |
| ctl        | 管理和监控 EMQX，执行 `emqx ctl help` 可以获取更多详细信息。 |

以下是用于开发调试的高级命令，普通用户通常无需关心：

| 命令           | 描述                                         |
| -------------- | -------------------------------------------- |
| remote_console | 连接到远程 EMQX 节点的交互式 shell。         |
| attach         | 附加到正在运行的 EMQX 节点上执行交互式操作。 |
| ertspath       | 获取 EMQX Erlang 库的路径。                  |
| root_dir       | 获取 EMQX 根目录的路径。                     |
| pid            | 获取正在运行的 EMQX 节点的进程 ID。          |
| ping           | 检查 EMQX 节点是否正在运行。                 |
| check_config   | 验证 EMQX 配置文件是否正确。                 |
| console_clean  | 清空交互式 shell 控制台输出。                |
| escript        | 在 EMQX 节点上执行 Escript 脚本。            |

## ctl 命令介绍

EMQX `ctl` 命令提供了多个用于管理和监控 EMQX 的子命令。`ctl` 命令需要在 EMQX 服务启动之后才能运行。

> EMQX 也提供了 `emqx_ctl` 命令，它是 `emqx ctl` 的别名。
> `ctl` 命令通过启动一个隐藏的 Erlang 节点的方式，远程连接到指定的 EMQX 节点，并执行一个 Erlang 远程调用然后打印返回的结果，因此需要避免大量的使用 `ctl` 命令。

下面列举了所有 `ctl` 命令的子命令和相应的简介，本文档旨在介绍命令的功能，命令的详细参数介绍可以用 `help` 指令查看。

## status

快速查看当前运行的节点是否运行。

```bash
$ emqx ctl status
Node 'emqx@127.0.0.1' 5.8.7 is started
```

## broker

查看当前节点的运行的版本状态以及运行时长。

```bash
$ emqx ctl broker
sysdescr  : EMQX Enterprise
version   : 5.8.7
datetime  : 2025-08-06T10:01:23.857678490+02:00
uptime    : 52 seconds
```

### broker stats

该命令用于查看本地 Broker 的统计信息，包括连接数、会话数、订阅数、主题数等指标。

```bash
$ emqx ctl broker stats
channels.count                : 0
channels.max                  : 0
cluster_sessions.count        : 0
cluster_sessions.max          : 0
connections.count             : 0
connections.max               : 0
delayed.count                 : 0
delayed.max                   : 0
durable_subscriptions.count   : 0
durable_subscriptions.max     : 0
live_connections.count        : 0
live_connections.max          : 0
retained.count                : 3
retained.max                  : 3
sessions.count                : 0
sessions.max                  : 0
suboptions.count              : 0
suboptions.max                : 0
subscribers.count             : 0
subscribers.max               : 0
subscriptions.count           : 0
subscriptions.max             : 0
subscriptions.shared.count    : 0
subscriptions.shared.max      : 0
topics.count                  : 0
topics.max                    : 0
```

### broker metrics

该命令用于查看本地 Broker 的指标信息，包括认证、授权、消息投递、报文处理以及过载保护等指标。

```bash
$ emqx ctl broker metrics
authentication.failure        : 0
authentication.success        : 0
authentication.success.anonymo: 0
authorization.allow           : 0
authorization.cache_hit       : 0
authorization.cache_miss      : 0
authorization.deny            : 0
authorization.matched.allow   : 0
authorization.matched.deny    : 0
authorization.nomatch         : 0
authorization.superuser       : 0
bytes.received                : 0
bytes.sent                    : 0
client.auth.anonymous         : 0
client.authenticate           : 0
client.authorize              : 0
client.connack                : 0
client.connect                : 0
client.connected              : 0
client.disconnected           : 0
client.subscribe              : 0
client.unsubscribe            : 0
delivery.dropped              : 0
delivery.dropped.expired      : 0
delivery.dropped.no_local     : 0
delivery.dropped.qos0_msg     : 0
delivery.dropped.queue_full   : 0
delivery.dropped.too_large    : 0
messages.acked                : 0
messages.delayed              : 0
messages.delivered            : 0
messages.dropped              : 0
messages.dropped.await_pubrel_: 0
messages.dropped.no_subscriber: 0
messages.forward              : 0
messages.persisted            : 0
messages.publish              : 0
messages.qos0.received        : 0
messages.qos0.sent            : 0
messages.qos1.received        : 0
messages.qos1.sent            : 0
messages.qos2.received        : 0
messages.qos2.sent            : 0
messages.received             : 0
messages.sent                 : 0
messages.transformation_failed: 0
messages.transformation_succee: 0
messages.validation_failed    : 0
messages.validation_succeeded : 0
overload_protection.delay.ok  : 0
overload_protection.delay.time: 0
overload_protection.gc        : 0
overload_protection.hibernatio: 0
overload_protection.new_conn  : 0
packets.auth.received         : 0
packets.auth.sent             : 0
packets.connack.auth_error    : 0
packets.connack.error         : 0
packets.connack.sent          : 0
packets.connect.received      : 0
packets.disconnect.received   : 0
packets.disconnect.sent       : 0
packets.pingreq.received      : 0
packets.pingresp.sent         : 0
packets.puback.inuse          : 0
packets.puback.missed         : 0
packets.puback.received       : 0
packets.puback.sent           : 0
packets.pubcomp.inuse         : 0
packets.pubcomp.missed        : 0
packets.pubcomp.received      : 0
packets.pubcomp.sent          : 0
packets.publish.auth_error    : 0
packets.publish.dropped       : 0
packets.publish.error         : 0
packets.publish.inuse         : 0
packets.publish.received      : 0
packets.publish.sent          : 0
packets.pubrec.inuse          : 0
packets.pubrec.missed         : 0
packets.pubrec.received       : 0
packets.pubrec.sent           : 0
packets.pubrel.missed         : 0
packets.pubrel.received       : 0
packets.pubrel.sent           : 0
packets.received              : 0
packets.sent                  : 0
packets.suback.sent           : 0
packets.subscribe.auth_error  : 0
packets.subscribe.error       : 0
packets.subscribe.received    : 0
packets.unsuback.sent         : 0
packets.unsubscribe.error     : 0
packets.unsubscribe.received  : 0
session.created               : 0
session.discarded             : 0
session.resumed               : 0
session.takenover             : 0
session.terminated            : 0
```

## cluster

查看和管理节点的集群状态。需要注意的是，EMQX 加入集群的指令 `join` 是向参数中指定的节点发送一个"请求"，而不是"邀请"。

换句话说，`emqx ctl cluster join <OneOfTheClusteredNodes>` 命令用于向 `OneOfTheClusteredNodes` 所在的集群发送请求以加入，而不是让这个节点加入自身所在的集群。

### cluster join \<Node\>

使用该命令将当前节点加入到指定节点所在的 EMQX 集群中。请确保指定的节点是在线且可访问的。

```bash
$ emqx ctl cluster join emqx2@127.0.0.1
Failed to join the cluster: {node_down,'emqx2@127.0.0.1'}
```

### cluster leave

使用该命令将当前节点从当前 EMQX 集群中移除。

```bash
$ emqx ctl cluster leave
Failed to leave the cluster: node_not_in_cluster
```

### cluster force-leave \<Node\>

强制将指定节点从集群中移除。该命令用于在必要情况下将某个节点从 EMQX 集群中强制移除。

::: tip 注意

此操作可能会导致集群状态不一致，请谨慎使用。

:::

```bash
$ emqx ctl cluster force-leave emqx2@127.0.0.1
Failed to remove the node from cluster: node_not_in_cluster
```

### cluster status [--json]

使用该命令查看 EMQX 集群的状态。

可选参数 `--json` 用于以 JSON 格式展示集群状态。

```bash
$ emqx ctl cluster status
Cluster status: #{running_nodes => ['emqx@127.0.0.1'],stopped_nodes => []}
```

```bash
$ emqx ctl cluster status --json
{
  "stopped_nodes" : [

  ],
  "running_nodes" : [
    "emqx@127.0.0.1"
  ]
}
```

### cluster discovery enable

启用并运行自动集群发现功能（需事先完成配置）。

```bash
$ emqx ctl cluster discovery enable
Automatic cluster discovery enabled.
```

## clients

该命令用于查看和管理已连接的客户端。

### clients list

查看当前连接到 EMQX 的所有客户端。此命令可用于监控活跃客户端及连接数量。

:::tip 提示

如果系统中连接了大量客户端，执行 `list` 命令可能会耗费较多时间和资源。

:::

```bash
$ emqx ctl clients list
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4530, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
Client(emqx_a, username=undefined, peername=127.0.0.1:59444, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4588, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736441613, connected_at=1684736441613)
```

### clients show \<ClientId\>

查看指定客户端的详细连接信息。

```bash
$ emqx ctl clients show emqx_c
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4680, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
```

### clients kick \<ClientId\>

强制断开指定客户端的连接。

```bash
$ emqx ctl clients kick emqx_c
ok
```

## topics

该命令用于查看当前系统中所有订阅的主题。

### topics list

列出所有主题,该命令可用于监视主题的数量和分布。

::: tip 提示

如果集群中有大量的主题订阅，`list` 指令可能会比较耗时且耗资源。

:::

```bash
$ emqx ctl topics list
t/1 -> emqx@127.0.0.1
```

### topics show \<Topic\>

显示特定主题的详细信息。

```bash
$ emqx ctl topics show t/1
t/1 -> emqx@127.0.0.1
```

## subscriptions

查看、增加或者删除某个客户端的订阅。

### subscriptions list

列出所有订阅。

:::tip 提示

当系统中有大量的订阅客户端时，`list` 指令可能比较耗时且耗资源。

:::

```bash
$ emqx ctl subscriptions list
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
emqx_c -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions show \<ClientId\>

显示特定客户端的订阅。

```bash
$ emqx ctl subscriptions show emqx_a
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions add \<ClientId\> \<Topic\> \<QoS\>

手动添加订阅。

```bash
$ emqx ctl subscriptions add emqx_a t/1 1
ok
```

### subscriptions del \<ClientId\> \<Topic\>

手动删除订阅。

```bash
$ emqx ctl subscriptions del emqx_a t/1
ok
```

## plugins

查看和管理插件。

### plugins list

列出所有已安装的插件。

```bash
emqx ctl plugins list
[]
```

### plugins describe \<Name-Vsn\>

描述已安装插件的详细信息。

```bash
emqx ctl plugins describe emqx_auth_mnesia-3.0.1
```

### plugins allow \<Name-Vsn\>

授予通过 Dashboard 安装指定插件的权限。

```bash
emqx ctl plugins allow emqx_auth_mnesia-3.0.1
{
  "result" : "ok",
  "name_vsn" : "emqx_auth_mnesia-3.0.1",
  "action" : "do_allow_installation"
}
```

### plugins disallow \<Name-Vsn\>

撤销通过 Dashboard 安装指定插件的权限。

```bash
emqx ctl plugins disallow emqx_auth_mnesia-3.0.1
{
  "result" : "ok",
  "name_vsn" : "emqx_auth_mnesia-3.0.1",
  "action" : "do_disallow_installation"
}
```

### plugins install \<Name-Vsn\>

安装一个已放置在插件安装目录下的插件包。

```bash
emqx ctl plugins install emqx_auth_mnesia-3.0.1
```

### plugins uninstall \<Name-Vsn\>

卸载指定插件。

```bash
emqx ctl plugins uninstall emqx_auth_mnesia-3.0.1
```

### plugins start \<Name-Vsn\>

启动指定插件。

```bash
emqx ctl plugins start emqx_auth_mnesia-3.0.1
```

### plugins stop \<Name-Vsn\>

停止指定插件。

```bash
emqx ctl plugins stop emqx_auth_mnesia-3.0.1
```

### plugins restart \<Name-Vsn\>

重启指定插件。

```bash
emqx ctl plugins restart emqx_auth_mnesia-3.0.1
```

### plugins disable \<Name-Vsn\>

禁用自动启动插件。

```bash
emqx ctl plugins disable emqx_auth_mnesia-3.0.1
```

### plugins enable \<Name-Vsn\> \[Position\]

启用插件的自动启动，并指定启动位置。

```bash
emqx ctl plugins enable emqx_auth_mnesia-3.0.1 front
```

可以使用 `front`, `rear`, 或 `before Other-Vsn` 来指定一个相对位置用来调整启动顺序。
如果没有给出 Position，已配置好的插件将停留在原来的位置，新的插件会被附加到最后面的位置上。

## vm

用于查看 Erlang 虚拟机的运行时状态和指标。

### vm all

该命令用于查看 Erlang 虚拟机（VM）的全部信息，包括 CPU 负载、内存使用情况等。

```bash
$ emqx ctl vm all
cpu/load1               : 13.16
cpu/load5               : 11.95
cpu/load15              : 9.75
memory/total            : 127648904
memory/processes        : 30427456
memory/processes_used   : 30426744
memory/system           : 97221448
memory/atom             : 2277809
memory/atom_used        : 2259843
memory/binary           : 668072
memory/code             : 48748792
memory/ets              : 10725432
process/limit           : 2097152
process/count           : 626
io/max_fds              : 8192
io/active_fds           : 0
ports/count             : 27
ports/limit             : 1048576
```

### vm load

该命令用于查看 Erlang 虚拟机在过去 1 分钟、5 分钟和 15 分钟内的 CPU 平均负载。

```
$ emqx ctl vm load
cpu/load1               : 0.96
cpu/load5               : 1.03
cpu/load15              : 1.05
```

### vm memory

该命令用于查看 Erlang 虚拟机的内存使用情况，包括总内存、进程内存、原子表内存、二进制内存以及 ETS 内存等。

```
$ emqx ctl vm memory
memory/total            : 218672189
memory/processes        : 70762184
memory/processes_used   : 70760616
memory/system           : 147910005
memory/atom             : 3080769
memory/atom_used        : 3061022
memory/binary           : 1652808
memory/code             : 67620307
memory/ets              : 17414480
```

------

### vm process

该命令用于查看 Erlang 虚拟机的进程信息，包括当前进程数以及进程数量限制。

```
$ emqx ctl vm process
process/limit           : 2097152
process/count           : 870
```

### vm io

该命令用于查看 Erlang 虚拟机的 I/O 信息，包括最大文件描述符数量和当前活动文件描述符数量。

```
$ emqx ctl vm io
io/max_fds              : 1048576
io/active_fds           : 0
```

### vm ports

该命令用于查看 Erlang 虚拟机的端口信息，包括当前端口数量和端口限制。

```
$ emqx ctl vm ports
ports/count             : 12
ports/limit             : 1048576
```

## mnesia

用于查看内置数据库（Mnesia）的运行状态和指标。

```bash
$ emqx ctl mnesia
===> System info in version "4.20.4.1", debug level = none <===
opt_disc. Directory "/Users/emqx/Downloads/emqx-503/data/mnesia/emqx@127.0.0.1" is used.
use fallback at restart = false
running db nodes   = ['emqx@127.0.0.1']
stopped db nodes   = []
master node tables = []
backend types      = null_copies    - mria_mnesia_null_storage
                     rocksdb_copies - mnesia_rocksdb
remote             = []
ram_copies         = [bpapi,emqx_channel_registry,
                      emqx_ee_schema_registry_serde_tab,
                      emqx_exclusive_subscription,
                      emqx_gateway_coap_channel_registry,emqx_retainer_index,
                      emqx_retainer_index_meta,emqx_retainer_message,
                      emqx_route,emqx_routing_node,emqx_shared_subscription,
                      emqx_trie,mria_schema]
disc_copies        = [cluster_rpc_commit,cluster_rpc_mfa,emqx_acl,
                      emqx_activated_alarm,emqx_admin,emqx_admin_jwt,emqx_app,
                      emqx_authn_mnesia,emqx_banned,emqx_dashboard_monitor,
                      emqx_deactivated_alarm,emqx_delayed,
                      emqx_enhanced_authn_scram_mnesia,emqx_psk,
                      emqx_telemetry,emqx_trace,schema]
disc_only_copies   = []
[{'emqx@127.0.0.1',disc_copies}] = [schema,emqx_psk,emqx_delayed,emqx_app,
                                    emqx_admin_jwt,emqx_dashboard_monitor,
                                    emqx_admin,cluster_rpc_mfa,
                                    cluster_rpc_commit,emqx_acl,
                                    emqx_enhanced_authn_scram_mnesia,
                                    emqx_authn_mnesia,emqx_banned,
                                    emqx_activated_alarm,
                                    emqx_deactivated_alarm,emqx_telemetry,
                                    emqx_trace]
[{'emqx@127.0.0.1',ram_copies}] = [mria_schema,emqx_trie,
                                   emqx_shared_subscription,emqx_routing_node,
                                   emqx_route,emqx_exclusive_subscription,
                                   bpapi,emqx_channel_registry,
                                   emqx_retainer_index_meta,
                                   emqx_retainer_message,emqx_retainer_index,
                                   emqx_ee_schema_registry_serde_tab,
                                   emqx_gateway_coap_channel_registry]
414 transactions committed, 32 aborted, 6 restarted, 250 logged to disc
0 held locks, 0 in queue; 0 local transactions, 0 remote
0 transactions waits for other nodes: []
```

## log

用于管理日志参数，例如日志级别等。

### log set-level \<Level\>

设置整体日志级别。

```bash
$ emqx ctl log set-level debug
debug
```

### log primary-level

显示当前主要日志级别。`primary-level`代表 EMQX 的主要日志级别，用于指定整个系统的默认日志级别。设置`primary-level`会影响所有的日志输出，除非特定的日志处理程序有自己独立的日志级别。

```bash
$ emqx ctl log primary-level
debug
```

### log primary-level \<Level\>

设置主要日志级别。

```bash
$ emqx ctl log primary-level info
info
```

### log handlers list

显示日志处理 handlers。`handlers`是指定用于处理日志的日志处理程序的集合。每个日志处理程序可以独立设置自己的日志级别，并定义如何处理和存储日志消息。

```bash
$ emqx ctl log handlers list
LogHandler(id=ssl_handler, level=debug, destination=console, status=started)
LogHandler(id=console, level=debug, destination=console, status=started)
```

### log handlers start \<HandlerId\>

启动某个 handler。

```bash
$ emqx ctl log handlers start console
log handler console started
```

### log handlers stop \<HandlerId\>

停止某个 handler。

```bash
$ emqx ctl log handlers stop console
log handler console stopped
```

### log handlers set-level \<HandlerId\> \<Level\>

设置某个 handler 的日志级别。

```bash
$ emqx ctl log handlers set-level console debug
debug
```

## trace

用于对一个给定的客户端或主题进行日志追踪。

### trace list

列出本地节点上启动的所有跟踪。

```bash
$ emqx ctl trace list
Trace(ip_address=127.0.0.1, level=debug, destination="trace.log")
```

### trace start client \<ClientId\> \<File\> [\<Level\>]

为某个特定客户端启动跟踪。

```bash
$ emqx ctl trace start client emqx_c trace.log debug
trace emqx_c CLI-emqx_c successfully
```

### trace stop client \<ClientId\>

停止对某个特定客户端的跟踪。

```bash
$ emqx ctl trace stop client emqx_c
stop tracing clientid emqx_c successfully
```

### trace start topic \<Topic\> \<File> [\<Level>]

为某个特定主题启动跟踪。

```bash
$ emqx ctl trace start topic t/1 trace.log info
trace t/1 CLI-t/1 successfully
```

### trace stop topic \<Topic\>

停止对某个特定主题的跟踪。

```bash
$ emqx ctl trace stop topic t/1
stop tracing topic t/1 successfully
```

### trace start ip_address \<IP\> \<File> [\<Level>]

为某个特定客户端 IP 地址启动跟踪。

```bash
$ emqx ctl trace start ip_address 127.0.0.1 trace.log debug
trace 127.0.0.1 CLI-127.0.0.1 successfully
```

### trace stop ip_address \<IP\>

停止对某个客户端 IP 地址的跟踪。

```bash
$ emqx ctl trace stop ip_address 127.0.0.1
stop tracing ip_address 127.0.0.1 successfully
```

::: tip
建议在命令行中使用绝对路径指定追踪日志的文件。例如：
`emqx ctl trace start client foobar /abs/path/to/trace.log debug`
:::

::: tip
也可以在控制台界面中管理追踪日志。参考 [日志追踪 (Trace)](./observability/tracer.md)。
:::

## traces

这个命令跟 `trace` 命令一样，但是会在整个集群所有节点中都开始或停止一个 tracer，参照上文的 trace 命令即可。

### traces list

列出所有已启动的集群跟踪。

```bash
$ emqx ctl traces list
Trace(mytraces_ip: ip_address=127.0.0.1, waiting, LogSize:#{'emqx@127.0.0.1' => 0})
```

### traces start \<Name\> client \<ClientId\> [\<Duration\>]

对集群中的一个客户端进行跟踪。

```bash
$ emqx ctl traces start mytraces client emqx_c 1200
cluster_trace clientid emqx_c mytraces successfully
```

### traces start \<Name\> topic \<Topic\> [\<Duration\>]

对集群中的一个主题进行跟踪。

```bash
$ emqx ctl traces start mytraces_ip topic t/1 1200
cluster_trace topic t/1 mytraces_ip successfully
```

### traces start \<Name\> ip_address \<IPAddr\> [\<Duration\>]

对集群中一个客户端 IP 进行跟踪。

```bash
$ emqx ctl traces start mytraces_ip ip_address 127.0.0.1 1200
cluster_trace ip_address 127.0.0.1 mytraces_ip successfully
```

### traces stop \<Name\>

停止集群中的跟踪。

```bash
$ emqx ctl traces stop mytraces_ip
Stop cluster_trace mytraces_ip successfully
```

### traces delete \<Name\>

删除集群中的跟踪。

```bash
$ emqx ctl traces delete mytraces_ip
Del cluster_trace mytraces_ip successfully
```

## listeners

管理监听器。

### listeners

列出所有监听器的信息。

```bash
$ emqx ctl listeners
ssl:default
  listen_on       : 0.0.0.0:8883
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 0
  max_conns       : 5000000
tcp:default
  listen_on       : 0.0.0.0:1883
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 12
  max_conns       : 5000000
  shutdown_count  : [{takenover,2},{discarded,1}]
ws:default
  listen_on       : 0.0.0.0:8083
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 0
  max_conns       : 5000000
wss:default
  listen_on       : 0.0.0.0:8084
  acceptors       : 16
  proxy_protocol  : false
  running         : true
  current_conn    : 0
  max_conns       : 5000000
```

#### 常见连接关闭原因

对于 TCP 监听器，EMQX 会报告一个 `shutdown_count` 字段，用于统计客户端断开连接的次数，并按原因分类。该字段有助于分析客户端为何被断开连接。

```
shutdown_count  : [{takenover,2},{discarded,1}]
```

上述示例中：

- 有 2 个客户端因为新的会话接管了相同的 `clientid` 而被断开连接（`takenover`）。
- 有 1 个客户端因为被新的 clean session 替换而被丢弃（`discarded`）。

以下是 `shutdown_count` 字段中可能出现的一些常见断开原因：

| 原因                          | 描述                                                         |
| ----------------------------- | ------------------------------------------------------------ |
| `banned`                      | 客户端因违反 ACL 规则、触发限流或 IP 被限制而被拉入黑名单并断开连接。 |
| `closed`                      | 连接被服务器或客户端主动关闭。                               |
| `discarded`                   | 新客户端使用相同的 `clientid` 且设置了 `clean_start = true`，在旧会话仍然活跃的情况下连接，导致旧会话被丢弃。 |
| `takenover`                   | 新客户端使用相同的 `clientid` 且设置了 `clean_start = false`，在旧会话仍然活跃的情况下连接，导致旧会话被接管。 |
| `einval`                      | 出现无效参数或 socket 错误，通常是因为尝试向已被系统关闭的 socket 写入数据（可能是 socket 状态变更通知与数据写入之间的竞争条件所致）。 |
| `frame_too_large`             | 收到的 MQTT 报文超过了允许的最大帧大小。                     |
| `idle_timeout`                | TCP/SSL 连接建立后，在配置的超时时间内未收到 `CONNECT` 报文。 |
| `invalid_proto_name`          | `CONNECT` 报文中的协议名称无效或不是 `"MQTT"`。              |
| `invalid_topic`               | 客户端使用了无效或被禁止的主题（如包含非法字符或被 Broker 限制）。 |
| `keepalive_timeout`           | 客户端在 Keep Alive 时间间隔内未发送任何报文。               |
| `malformed_packet`            | MQTT 报文损坏或不符合 MQTT 协议规范。                        |
| `not_authorized`              | 客户端尝试执行未被授权的操作，被 ACL 拒绝。                  |
| `ssl_closed`                  | SSL/TLS 连接被对端关闭。                                     |
| `ssl_error`                   | SSL/TLS 握手或数据传输过程中发生错误。                       |
| `ssl_upgrade_timeout`         | SSL/TLS 握手未在允许时间内完成。                             |
| `unexpected_packet`           | 客户端在当前连接状态下发送了不应出现的报文。                 |
| `zero_remaining_len`          | MQTT 报文的剩余长度字段为 0，在大多数场景下是非法的。        |
| `bad_username_or_password`    | 客户端身份认证失败，用户名或密码错误。                       |
| `client_identifier_not_valid` | 提供的 `clientid` 无效，或被另一个正在登录的客户端锁定。         |
| `protocol_error`              | 出现通用的 MQTT 协议错误。                                   |
| `tcp_closed`                  | TCP 连接被客户端或网络层关闭。                               |
| `timeout`                     | 发生通用超时错误（如在认证或握手过程中）。                   |

### listeners stop \<Identifier\>

停止一个监听器，Identifier 为 `{type}:{name}` 格式，如 `tcp:default`。（临时生效，当 EMQX 重启后将恢复原先状态。）

```bash
$ emqx ctl listeners stop tcp:default
Stop tcp:default listener successfully.
```

::: tip
停止监听器会导致所有通过该监听器接入的客户端都断开连接。
:::

### listeners start \<Identifier\>

启动一个监听器。（临时生效，当 EMQX 重启后将恢复原先状态。）

```bash
$ emqx ctl listeners start tcp:default
Started tcp:default listener successfully.
```

### listeners restart \<Identifier\>

重启一个监听器。

```bash
$ emqx ctl listeners restart tcp:default
Restarted tcp:default listener successfully.
```

::: tip
重启监听器会导致所有通过该监听器接入的客户端都断开连接。
:::

### listeners enable \<Identifier\> <true/false>

启用或禁用一个监听器。（持久化到配置，永久生效）

```bash
$ emqx ctl listeners enable tcp:default true
Enabled tcp:default listener successfully.
```

```bash
$ emqx ctl listeners enable tcp:default false
Disabled tcp:default listener successfully.
```

## authz cache-clean

在需要强制清除缓存的权限（ACL）数据时，可以使用此命令。

### authz cache-clean all

清除所有节点上的权限缓存。

```bash
$ emqx ctl authz cache-clean all
Authorization cache drain started on all nodes OK
```

### authz cache-clean node \<Node\>

清除指定节点上的权限缓存。

```bash
$ emqx ctl authz cache-clean node emqx@127.0.0.1
Authorization cache drain started on node emqx@127.0.0.1 OK
```

### authz cache-clean \<ClientId\>

清除指定客户端的权限缓存。

```bash
$ emqx ctl authz cache-clean mqttx_9502dc8a
Drain mqttx_9502dc8a authz cache OK
```

## pem_cache

该命令用于强制 EMQX 重新加载更新后的 PEM（X.509 密钥和证书）文件。

### pem_cache clean all

清除所有节点上的 X.509 证书缓存。

```bash
$ emqx ctl pem_cache clean all
PEM cache clean OK
```

### pem_cache clean node \<Node\>

清除指定节点上的 X.509 证书缓存。

```bash
$ emqx ctl pem_cache clean emqx@127.0.0.1
emqx@127.0.0.1 PEM cache clean OK
```

## olp

OLP 是 “overload protection” （过载保护）的缩写。
`olp` 命令可以用于检查系统过载的状态，也可以用于关闭或开启系统过载保护。

您可以在 `overload_protection` 的配置文档中查看更多信息。

::: tip 提示
`olp` 是默认开启的，如果从命令行改变这个状态，这个改变只能持续到系统重启。重启之后会回到配置文件中的状态。
:::

### olp status

返回系统是否处于过载状态。若未过载，则返回 "not overloaded"。

```bash
$ emqx ctl olp status
'emqx@172.17.0.3' is not overloaded
```

### olp enable

启用过载保护功能。

```bash
$ emqx ctl olp enable
Enable overload protection 'emqx@127.0.0.1' : {ok,<0.5703.0>}
```

### olp disable

禁用过载保护功能。

```bash
$ emqx ctl olp disable
Disable overload protetion 'emqx@127.0.0.1' : ok
```

## data

该命令用于将节点数据导出到 tar 压缩包文件中，或从 tar 文件中导入数据。

### data export

```bash
data export \
  [--root-keys key1,key2,key3] \
  [--table-sets set1,set2,set3] \
  [--dir out_dir]
```

将 EMQX 节点中的数据导出为 tar 格式的归档文件。此命令适用于数据备份或在节点之间迁移数据。

导出内容包括：

- 集群配置
- EMQX 数据目录中的附加文件（如 SSL 证书）
- 内置数据库

可使用 `--root-keys` 和 `--table-sets` 参数指定需要导出的数据，若不指定，则导出全部数据。

```bash
emqx ctl data export --root-keys listeners,connectors,actions,rule_engine --dir /tmp
Exporting data to "/tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz"...
Exporting cluster configuration...
Exporting additional files from EMQX data_dir: "data"...
Exporting built-in database...
Exporting emqx_banned_rules database table...
Exporting emqx_banned database table...
Exporting emqx_psk database table...
Exporting emqx_authn_mnesia database table...
Exporting emqx_authn_scram_mnesia database table...
Exporting emqx_acl database table...
Exporting emqx_app database table...
Exporting emqx_mt_config database table...
Exporting emqx_admin database table...
Exporting emqx_retainer_message database table...
Data has been successfully exported to /tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz.
```

### data import \<File\>

从指定的 tar 文件中导入数据。此命令适用于从备份恢复数据或将数据迁移至新节点。

```bash
emqx ctl data import /tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz
Importing data from "/tmp/emqx-export-2025-08-06-12-00-19.334.tar.gz"...
Importing cluster configuration for namespace global...
Importing built-in database...
Importing emqx_retainer_message database table...
Starting reindexing retained messages
Reindexed 3 messages
Reindexing retained messages finished
Importing emqx_admin database table...
Importing emqx_mt_config database table...
Importing emqx_app database table...
Importing emqx_acl database table...
Importing emqx_authn_scram_mnesia database table...
Importing emqx_authn_mnesia database table...
Importing emqx_psk database table...
Importing emqx_banned database table...
Importing emqx_banned_rules database table...
Data has been imported successfully.
```

## ds

该命令用于操作持久存储。

### ds info

显示内嵌持久存储的整体状态。

```
emqx ctl ds info
THIS SITE:
EFC84E67230295E2

SITES:
.------------------.----------------.--------.
: Site             : Node           : Status :
:------------------:----------------:--------:
: EFC84E67230295E2 : emqx@127.0.0.1 :     up :
 ------------------ ---------------- --------

SHARDS:
.----------.----------.-------------.
: DB/Shard : Replicas : Transitions :
:----------:----------:-------------:
 ---------- ---------- -------------
```

### ds set-replicas \<storage\> \<site1\> \<site2\> ...

设置包含集群中持久存储副本的站点列表。

### ds join \<storage\> \<site\>

将某个站点添加至指定存储的副本集中。

### ds leave \<storage\> \<site\>

将某个站点从指定存储的副本集中移除。

### ds forget \<site\>

从已知站点列表中移除指定站点。

## exclusive

此命令用于查看当前系统中所有排它订阅的主题或删除一个排它订阅主题。

### exclusive list

列出所有排它订阅的主题。

```bash
$ emqx ctl exclusive list
t/1 -> client1
```

### exclusive delete \<Topic\>

删除排它订阅主题。

```bash
$ emqx ctl exclusive delete t/1
ok
```

## retainer

`retainer` 命令可用于查看或管理保留消息。同时也提供了 `emqx ctl retainer reindex` 命令，用于创建或更新保留消息的索引。

### retainer info

显示当前系统中保留消息的数量。

```bash
$ emqx ctl retainer info
Number of retained messages: 3
```

### retainer topics

列出所有包含保留消息的主题。

```bash
$ emqx ctl retainer topics
$SYS/brokers
$SYS/brokers/emqx@127.0.0.1/sysdescr
$SYS/brokers/emqx@127.0.0.1/version
```

### retainer clean

清除系统中的所有保留消息。

```bash
emqx ctl retainer clean
```

### retainer clean \<Topic\>

根据指定的主题过滤器清除保留消息。

```bash
emqx ctl retainer clean t/1
```

### retainer reindex status

查看保留消息重建索引任务的当前状态。

```bash
$ emqx ctl retainer reindex status
Reindexing is not running
```

### retainer reindex start [force]

根据配置设置为保留消息主题生成新的索引。使用 `true` 作为 `<force>` 参数时，将忽略之前未完成的重建任务。

```bash
$ emqx ctl retainer reindex start true
Starting reindexing
Reindexed 0 messages
Reindexing finished
```

## observer

该命令提供 Erlang 虚拟机的运行时信息视图，类似于 Linux 的 `top` 命令。以下为子命令说明：

### observer status

在当前控制台中启动观察器，用于监控和调试 EMQX 节点的状态和活动。

```bash
$ emqx ctl observer status
```

### observer bin_leak

强制所有进程进行垃圾回收，并输出释放二进制数据最多的前 100 个进程，便于排查潜在的内存泄漏问题。

```bash
$ emqx ctl observer bin_leak
{<0.2140.0>,-48,
 [{current_function,{logger_std_h,file_ctrl_loop,1}},
  {initial_call,{erlang,apply,2}}]}
...
```

### observer load Mod

确保指定模块已在集群所有节点中加载。适用于在整个集群中统一加载调试模块。

```bash
$ emqx ctl observer load Mod
Loaded 'Mod' module on []: ok
```

## conf

该命令用于查看和修改 EMQX 集群的配置信息。

### conf reload --replace | --merge

重新加载本地节点的配置文件 `etc/emqx.conf`。默认会将新配置合并到现有配置中；使用 `--replace` 则替换现有配置项。

### conf show_keys

打印当前正在使用的所有配置键。

------

### conf show [\<key\>]

显示指定配置键下正在使用的配置值（包括默认值）。若未指定 key，则显示全部配置。

### conf load --replace | --merge \<path\>

加载 HOCON 格式的配置文件。默认将配置值合并到现有配置中，使用 `--replace` 替换现有值。
当前节点会发起集群范围内的配置变更事务，将更改同步至所有节点。

注意：在滚动升级过程中，不应修改运行时配置。

## conf cluster_sync

用于排查集群中用于配置同步的 cluster_call 出现问题的情况。

::: tip
在 EMQX 5.0.x 中，该命令名为 `cluster_call`，EMQX 5.1 中依然支持，但不会在帮助信息中显示。
 :::

通过 Dashboard 或 HTTP API 修改配置时，EMQX 首先将修改写入本地的 `data/configs/cluster.hocon` 文件，再将操作记录写入数据库，并异步同步至其他节点。

当由于某些原因，某节点无法成功应用变更时，可使用该命令进行检查和修复。

EMQX 会为每次集群范围内的配置变更生成一个事务 ID（`tnx_id`），该 ID 在集群范围内递增。

::: tip
`skip` 和 `fast_forward` 命令可能导致节点间配置不一致。
 :::

### conf cluster_sync status

查看所有节点的集群配置同步状态摘要。

```bash
$ emqx ctl conf cluster_sync status
-----------------------------------------------
All configuration synchronized(tnx_id=0) successfully
-----------------------------------------------
```

### conf cluster_sync inspect \<tnx_id\>

查看指定事务 ID（`tnx_id`）的配置变更详细信息。

```bash
$ emqx ctl conf cluster_sync inspect 2
{atomic,#{created_at => {{2022,6,21},{21,57,50}},
          initiator => 'emqx@127.0.0.1',
          mfa =>
              {emqx,update_config,
                    [[listeners,ssl,default],
                     {action,stop,#{<<"enabled">> => false}},
                     #{override_to => cluster,rawconf_with_defaults => true}]},
          tnx_id => 2}}
```

### conf cluster_sync skip [node]

跳过指定节点当前失败的配置提交。

::: warning 警告

可能导致配置在节点间不一致。

:::

### conf cluster_sync fast_forward [node] <tnx_id>

将指定节点的配置变更事务跳过并快速前移到给定的 `tnx_id`。

::: warning 警告

可能导致配置在节点间不一致。

:::

### conf cluster_sync fix

将配置最完整的节点（通常是拥有最大 `tnx_id` 的节点）作为参考，向其他节点同步其配置。

## eviction status

该命令用于查看当前节点的连接驱逐（Eviction）状态。

```bash
$ emqx ctl eviction
Eviction status: disabled
```

## rebalance

`rebalance` 命令用于通过迁移连接和会话，将集群中的负载从高负载节点迁移到低负载节点，实现负载均衡。

### rebalance start --evacuation

启动当前节点的撤离流程，可选设置重定向服务器。

```bash
rebalance start --evacuation \
  [--wait-health-check Secs] \
  [--redirect-to "Host1:Port1 Host2:Port2 .."] \
  [--conn-evict-rate CountPerSec] \
  [--migrate-to "node1@host1 node2@host2 .."] \
  [--wait-takeover Secs] \
  [--sess-evict-rate CountPerSec]
```

### rebalance start

在指定节点上启动负载均衡迁移流程，以当前节点为协调者。

```bash
rebalance start \
  [--nodes "node1@host1 node2@host2 .."] \
  [--wait-health-check Secs] \
  [--conn-evict-rate ConnPerSec] \
  [--abs-conn-threshold Count] \
  [--rel-conn-threshold Fraction] \
  [--wait-takeover Secs] \b
  [--sess-evict-rate CountPerSec] \
  [--abs-sess-threshold Count] \
  [--rel-sess-threshold Fraction]
```

### rebalance node-status

查看当前节点负载均衡状态。

### rebalance node-status "node1@host1"

查看远程节点负载均衡状态。

### rebalance status

查看集群所有迁移任务状态。

### rebalance stop

停止当前节点的撤离流程。

## gateway

查看和管理网关的启停状态。

### gateway list

列出所有网关的信息。

```bash
$ emqx ctl gateway list
Gateway(name=coap, status=running, clients=0, started_at=2023-05-22T14:23:50.353+08:00)
Gateway(name=exproto, status=unloaded)
Gateway(name=lwm2m, status=unloaded)
Gateway(name=mqttsn, status=unloaded)
Gateway(name=stomp, status=unloaded)
```

### gateway lookup \<Name\>

查找特定网关的详细信息。

```bash
$ emqx ctl gateway lookup coap
name: coap
status: running
created_at: 2023-05-22T14:23:50.352+08:00
started_at: 2023-05-22T14:23:50.353+08:00
config: #{connection_required => false,enable => true,enable_stats => true,
          heartbeat => 30000,idle_timeout => 30000,
          listeners =>
              #{udp =>
                    #{default =>
                          #{access_rules => [],bind => 5683,enable => true,
                            enable_authn => true,max_conn_rate => 1000,
                            max_connections => 1024000,
                            udp_options =>
                                #{active_n => 100,reuseaddr => true}}}},
          mountpoint => <<>>,notify_type => qos,publish_qos => coap,
          subscribe_qos => coap}
```

### gateway load \<Name\> \<JsonConf\>

加载一个网关并配置参数。

```bash
emqx ctl gateway load coap '{"type":"coap", ...}'
```

### gateway unload \<Name\>

卸载一个网关。

```bash
$ emqx ctl gateway unload coap
ok
```

### gateway stop \<Name\>

停止一个网关。

```bash
$ emqx ctl gateway stop coap
ok
```

### gateway start \<Name\>

启动一个网关。

```bash
$ emqx ctl gateway start coap
ok
```

## gateway-registry

`emqx ctl gateway-registry`

查看当前系统中支持的网关。

当前默认支持的网关有如下 5 种：

- coap
- exproto
- lwm2m
- mqttsn
- stomp

EMQX 的网关设计成可插拔。所以网关应用可以在启动/运行时注册到 EMQX 系统中。
一旦注册之后，就可以用 HTTP API 或者命令行来对网关进行管理了。

## gateway-clients

该命令用于查看和管理网关客户端。

### gateway-clients list \<Name\>

列出指定网关下的所有客户端。

### gateway-clients lookup \<Name\> \<ClientId\>

查询指定网关中某个客户端的详细信息。

### gateway-clients kick \<Name\> \<ClientId\>

将指定客户端从对应网关中强制断开连接。

## gateway-metrics \<Name\>

`emqx ctl gateway-metrics`

查看网关的指标。

## license

::: tip

本节内容仅适用于 EMQX 企业版。

:::

### license info

```bash
$ emqx ctl license info
customer        : Developer
email           : contact@emqx.io
deployment      : Development
max_sessions    : 10000000
start_at        : 2025-03-02
expiry_at       : 2029-03-01
type            : community
customer_type   : 11
expiry          : false
```

### license update License

```bash
emqx ctl license update <YOUR_LICENSE_STRING>
```

请将 `YOUR_LICENSE_STRING` 替换为实际的 License 字符串。

### license update default

恢复为默认社区版 License。

```bash
emqx ctl license update default
```

## admins

`admins` 命令用于管理 EMQX Dashboard 的管理员用户。

### admins add \<Username\> \<Password\> \<Description\>

添加一个 Dashboard 管理用户。

```bash
$ emqx ctl admins add emqx_u EMQemq@1172
ok
```

### admins passwd \<Username\> \<Password\>

重置指定 Dashboard 管理员用户的密码。

```bash
$ emqx ctl admins passwd emqx_u EMQemq@11721
ok
```

### admins del \<Username\>

删除指定的 Dashboard 管理员用户。

```bash
$ emqx ctl admins del emqx_u
ok
```

## rules

查看系统中创建的所有的规则。

| 命令                | 描述                                     |
| ------------------- | ---------------------------------------- |
| rules list          | 列出所有规则,包括规则的 ID、名称等信息。 |
| rules show \<RuleID> | 显示特定规则的详细信息。                 |

请注意，下面是每个命令的执行示例：

### rules list

```bash
$ emqx ctl rules list
Rule{id=my-rule, name=, enabled=true, descr=this is my rule}
```

### rules show \<RuleID>

```bash
$ emqx ctl rules show my-rule
Id:
  my-rule
Name:

Description:
  this is my rule
Enabled:
  true
SQL:
  SELECT
    *
  FROM
    "f/#"
Created at:
  2023-05-22T14:14:27.567+08:00
Updated at:
  2023-05-22T14:14:27.567+08:00
Actions:
  - Name:  republish
    Type:  function
    Args:  #{payload => <<>>,qos => 0,retain => false,topic => <<"t/1">>,
             user_properties => <<"${user_properties}">>}
```

注意，命令行仅仅用于查看，规则的创建和更新等管理操作必需要在控制台的界面中操作。
