# 命令行

本章节向您介绍 EMQX 支持的各类启动与管理命令，并详细介绍 ctl 管理命令。

## 启动命令

EMQX 支持一些基本的启动和管理命令，您可以通过 `emqx <command>` 命令执行。

以下是常用的启动和管理命令：

| 命令       | 介绍                                                                                          |
| ---------- | --------------------------------------------------------------------------------------------- |
| start      | 以守护进程模式启动 EMQX，运行期间不需要交互式 shell                                           |
| console    | 在 Erlang 或 Elixir 交互式 shell 中启动 EMQX。用于在开发环境中调试 EMQX，需要与 EMQX 进行交互 |
| foreground | 在前台模式下启动 EMQX，不使用交互式 shell。用于在开发环境中启动 EMQX，但不需要后台运行        |
| stop       | 停止运行中的 EMQX 节点                                                                        |
| ctl        | 管理和监控 EMQX，执行 'emqx ctl help' 可以获取更多详细信息                                    |

以下是用于开发调试的高级命令，普通用户通常无需关心：

| 命令           | 介绍                                       |
| -------------- | ------------------------------------------ |
| remote_console | 连接到远程 EMQX 节点的交互式 shell         |
| attach         | 附加到正在运行的 EMQX 节点上执行交互式操作 |
| ertspath       | 获取 EMQX Erlang 库的路径                  |
| root_dir       | 获取 EMQX 根目录的路径                     |
| pid            | 获取正在运行的 EMQX 节点的进程 ID          |
| ping           | 检查 EMQX 节点是否正在运行                 |
| check_config   | 验证 EMQX 配置文件是否正确                 |
| console_clean  | 清空交互式 shell 控制台输出                |
| escript        | 在 EMQX 节点上执行 Escript 脚本            |

## ctl 命令介绍

EMQX `ctl` 命令提供了多个用于管理和监控 EMQX 的子命令。`ctl` 命令需要在 EMQX 服务启动之后才能运行。

> EMQX 也提供了 `emqx_ctl` 命令，它是 `emqx ctl` 的别名。
> `ctl` 命令通过启动一个隐藏的 Erlang 节点的方式，远程连接到指定的 EMQX 节点，并执行一个 Erlang 远程调用然后打印返回的结果，因此需要避免大量的使用 `ctl` 命令。

下面列举了所有 `ctl` 命令的子命令和相应的简介，本文档旨在介绍命令的功能，命令的详细参数介绍可以用 `help` 指令查看。

## status

快速查看当前运行的节点是否运行。

```bash
$ emqx ctl status
Node 'emqx@127.0.0.1' 5.0.3 is started
```

## broker

查看当前节点的运行的版本状态以及运行时长。

```bash
$ emqx ctl broker
sysdescr  : EMQX Enterprise
version   : 5.0.3
datetime  : 2023-05-12T10:21:50.095047713+08:00
uptime    : 52 seconds
```

## observer

可以用于查看运行时状态。展示一个类似于 linux 的 `top` 命令的界面，子命令如下：

| 命令              | 描述                                                                                                             |
| ----------------- | ---------------------------------------------------------------------------------------------------------------- |
| observer status   | 在当前控制台启动观察器，用于监视和调试 EMQX 节点的状态和活动。                                                   |
| observer bin_leak | 强制所有进程执行垃圾回收，并打印释放最大数量二进制数据的前 100 个进程，可能会显示出潜在的内存泄漏问题。          |
| observer load Mod | 确保指定的模块在 EMQX 集群中的所有节点上都已加载。当需要确保模块在整个集群中都可用时，可以使用此命令来加载模块。 |

### observer status

```bash
emqx ctl observer status
```

### observer bin_leak

```bash
$ emqx ctl observer bin_leak
{<0.2140.0>,-48,
 [{current_function,{logger_std_h,file_ctrl_loop,1}},
  {initial_call,{erlang,apply,2}}]}
{<0.2093.0>,-29,
 [{current_function,{application_master,main_loop,2}},
  {initial_call,{proc_lib,init_p,5}}]}
{<0.2116.0>,-23,
 [user_drv,
  {current_function,{user_drv,server_loop,6}},
  {initial_call,{user_drv,server,2}}]}
...
```

### observer load Mod

```bash
$ emqx ctl observer load Mod
Loaded 'Mod' module on []: ok
```

## cluster_call

该命令用于查看、调查甚至修改集群配置修改的同步状态。

EMQX 的 HTTP API 可以用于修改很多配置，当一个 API 被调用，例如从控制台界面的操作，来修改配置时，
在收到这个请求的节点会先将修改的内容在本地写入 `data/configs/cluster.hocon`，然后
同样的操作会被记录在数据库中，并异步地转发到集群中的其他节点。

当由于某种原因，无法在另一个节点成功执行同样的修改，那么这个命令就可以很方便的查看这个异步复制的状态，
甚至可以强制跳过一个失败的复制。

EMQX 会为每个集群范围的配置修改生成一个 ID，（tnxid），这个 ID 会在集群范围内严格递增，
每个修改，例如从控制台中修改一个配置之后，都会记录在数据库中。
下面这个例子，展示的是查看第二（tnxid=2）个修改的内容（这是一个启用 TLS 监听器的操作）。

```bash
$ emqx ctl cluster_call tnxid 2
{atomic,#{created_at => {{2022,6,21},{21,57,50}},
          initiator => 'emqx@127.0.0.1',
          mfa =>
              {emqx,update_config,
                    [[listeners,ssl,default],
                     {action,stop,#{<<"enabled">> => false}},
                     #{override_to => cluster,rawconf_with_defaults => true}]},
          tnx_id => 2}}
```

::: tip
`skip` 指令和 `fast_forward` 指令会迫使本地节点跳过一些（失败）的操作
这可能会导致集群内节点之间的配置不一致。
:::

## admins

admins 用于创建，修改，删除管理员账户，子命令如下：

| 命令                                           | 描述                          |
| ---------------------------------------------- | ----------------------------- |
| admins add <Username> <Password> <Description> | 添加 Dashboard 用户           |
| admins passwd <Username> <Password>            | 重置 Dashboard 指定用户的密码 |
| admins del <Username>                          | 删除指定 Dashboard 用户       |

### admins add <Username> <Password> <Description>

```bash
$ emqx ctl admins add emqx_u EMQemq@1172
ok
```

### admins passwd <Username> <Password>

```bash
$ emqx ctl admins passwd emqx_u EMQemq@11721
ok
```

### admins del <Username>

```bash
$ emqx ctl admins del emqx_u
ok
```

## retainer

用于查看和管理 retain 的消息。也可以用于为 retain 表创建索引。

| 命令                           | 描述                                                                                              |
| ------------------------------ | ------------------------------------------------------------------------------------------------- |
| retainer info                  | 显示保留消息的数量                                                                                |
| retainer topics                | 显示所有保留消息的主题                                                                            |
| retainer clean                 | 清除所有保留消息                                                                                  |
| retainer clean <Topic>         | 按指定主题过滤器清除保留消息                                                                      |
| retainer reindex status        | 显示重新索引状态                                                                                  |
| retainer reindex start [force] | 根据配置设置生成新的保留消息主题索引。将 true 作为 <Force> 参数传递以忽略先前启动的重新索引过程。 |

### retainer info

```bash
$ emqx ctl retainer info
Number of retained messages: 3
```

### retainer topics

```bash
$ emqx ctl retainer topics
$SYS/brokers
$SYS/brokers/emqx@127.0.0.1/sysdescr
$SYS/brokers/emqx@127.0.0.1/version
```

### retainer clean

```bash
emqx ctl retainer clean
```

### retainer clean <Topic>

```bash
emqx ctl retainer clean t/1
```

### retainer reindex status

```bash
$ emqx ctl retainer reindex status
Reindexing is not running
```

### retainer reindex start [force]

```bash
$ emqx ctl retainer reindex start true
Starting reindexing
Reindexed 0 messages
Reindexing finished
```

## cluster

查看和管理节点的集群状态。需要注意的是，EMQX 加入集群的指令 `join` 是向参数中指定的节点发送一个"请求"，而不是"邀请"。

换句话说，`emqx ctl cluster join <OneOfTheClusteredNodes>` 命令用于向 `OneOfTheClusteredNodes` 所在的集群发送请求以加入，而不是让这个节点加入自身所在的集群。

| 命令                         | 描述                 | 使用场景和注意事项                                                                                                   |
| ---------------------------- | -------------------- | -------------------------------------------------------------------------------------------------------------------- |
| emqx ctl cluster             | 控制 EMQX 集群的命令 |                                                                                                                      |
| cluster join \<Node\>        | 加入集群             | - 使用该命令将节点加入到指定节点所在的 EMQX 集群<br>- 注意确保指定的节点是活动且可访问的                             |
| cluster leave                | 离开集群             | - 使用该命令将节点从当前 EMQX 集群中移除                                                                             |
| cluster force-leave \<Node\> | 强制节点离开集群     | - 使用该命令强制指定节点离开 EMQX 集群<br>- 注意该操作可能导致集群状态不一致，谨慎使用                               |
| cluster status [--json]      | 查看集群状态         | - 使用该命令查看 EMQX 集群的状态信息<br>- 可选参数`--json`以 JSON 格式显示集群状态<br>- 用于监视和调试集群的健康状况 |

### cluster join \<Node\>

```bash
$ emqx ctl cluster join emqx2@127.0.0.1
Failed to join the cluster: {node_down,'emqx2@127.0.0.1'}
```

### cluster leave

```bash
$ emqx ctl cluster leave
Failed to leave the cluster: node_not_in_cluster
```

### cluster force-leave \<Node\>

```bash
$ emqx ctl cluster force-leave emqx2@127.0.0.1
Failed to remove the node from cluster: node_not_in_cluster
```

### cluster status [--json]

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

## clients

查看和管理客户端。

| 命令                      | 描述                                                                     |
| ------------------------- | ------------------------------------------------------------------------ |
| clients list              | 查看当前连接到 EMQX 的所有客户端，该命令可用于监视活动客户端和连接数量。 |
| clients show \<ClientId\> | 查看特定客户端的详细连接信息。                                           |
| clients kick \<ClientId\> | 强制断开指定客户端的连接。                                               |

### emqx ctl clients list

```bash
$ emqx ctl clients list
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4530, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
Client(emqx_a, username=undefined, peername=127.0.0.1:59444, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4588, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736441613, connected_at=1684736441613)
```

### emqx ctl clients show \<ClientId\>

```bash
$ emqx ctl clients show emqx_c
Client(emqx_c, username=undefined, peername=127.0.0.1:59441, clean_start=true, keepalive=60, session_expiry_interval=0, subscriptions=1, inflight=0, awaiting_rel=0, delivered_msgs=4680, enqueued_msgs=0, dropped_msgs=0, connected=true, created_at=1684736435155, connected_at=1684736435155)
```

### emqx ctl clients kick \<ClientId\>

```bash
$ emqx ctl clients kick emqx_c
ok
```

::: tip
如果系统中连接了大量的客户端 `list` 指令可能会比较耗时且耗资源。
:::

## topics

查看当前系统中所有订阅的主题。

| 命令                  | 描述                                            |
| --------------------- | ----------------------------------------------- |
| topics list           | 列出所有主题,该命令可用于监视主题的数量和分布。 |
| topics show \<Topic\> | 显示特定主题的详细信息。                        |

### topics list

```bash
$ emqx ctl topics list
t/1 -> emqx@127.0.0.1
```

### topics show \<Topic\>

```bash
$ emqx ctl topics show t/1
t/1 -> emqx@127.0.0.1
```

::: tip
如果集群中有大量的主题订阅，`list` 指令可能会比较耗时且耗资源。
:::

## subscriptions

查看，增加或者删除某个客户端的订阅。

| 命令                                             | 描述                   |
| ------------------------------------------------ | ---------------------- |
| subscriptions list                               | 列出所有订阅。         |
| subscriptions show \<ClientId\>                  | 显示特定客户端的订阅。 |
| subscriptions add \<ClientId\> \<Topic\> \<QoS\> | 手动添加订阅。         |
| subscriptions del \<ClientId\> \<Topic\>         | 手动删除订阅。         |

### subscriptions list

```bash
$ emqx ctl subscriptions list
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
emqx_c -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions show \<ClientId\>

```bash
$ emqx ctl subscriptions show emqx_a
emqx_a -> topic:t/1 qos:0 nl:0 rh:0 rap:0
```

### subscriptions add \<ClientId\> \<Topic\> \<QoS\>

```bash
$ emqx ctl subscriptions add emqx_a t/1 1
ok
```

### subscriptions del \<ClientId\> \<Topic\>

```bash
$ emqx ctl subscriptions del emqx_a t/1
ok
```

:::tip
当系统中有大量的订阅客户端时，`list` 指令可能比较耗时且耗资源。
:::

## plugins

查看和管理插件。

| 命令                                     | 描述                                     |
| ---------------------------------------- | ---------------------------------------- |
| plugins list                             | 列出所有已安装的插件。                   |
| plugins describe \<Name-Vsn\>            | 描述已安装插件的详细信息。               |
| plugins install \<Name-Vsn\>             | 安装一个已放置在插件安装目录下的插件包。 |
| plugins uninstall \<Name-Vsn\>           | 卸载指定插件。                           |
| plugins start \<Name-Vsn\>               | 启动指定插件。                           |
| plugins stop \<Name-Vsn\>                | 停止指定插件。                           |
| plugins restart \<Name-Vsn\>             | 重启指定插件。                           |
| plugins disable \<Name-Vsn\>             | 禁用自动启动插件。                       |
| plugins enable \<Name-Vsn\> \[Position\] | 启用插件的自动启动，并指定启动位置。     |

### plugins list

```bash
emqx ctl plugins list
```

### plugins describe \<Name-Vsn\>

```bash
emqx ctl plugins describe emqx_auth_mnesia-3.0.1
```

### plugins install \<Name-Vsn\>

```bash
emqx ctl plugins install emqx_auth_mnesia-3.0.1
```

### plugins uninstall \<Name-Vsn\>

```bash
emqx ctl plugins uninstall emqx_auth_mnesia-3.0.1
```

### plugins start \<Name-Vsn\>

```bash
emqx ctl plugins start emqx_auth_mnesia-3.0.1
```

### plugins stop \<Name-Vsn\>

```bash
emqx ctl plugins stop emqx_auth_mnesia-3.0.1
```

### plugins restart \<Name-Vsn\>

```bash
emqx ctl plugins restart emqx_auth_mnesia-3.0.1
```

### plugins disable \<Name-Vsn\>

```bash
emqx ctl plugins disable emqx_auth_mnesia-3.0.1
```

### plugins enable \<Name-Vsn\> \[Position\]

```bash
emqx ctl plugins enable emqx_auth_mnesia-3.0.1 front
```

可以使用 'front', 'rear', 或 'before Other-Vsn' 来指定一个相对位置用来调整启动顺序。
如果没有给出 Position，已配置好的插件将停留在原来的位置，新的插件会被附加到最后面的位置上。

## vm

用于查看 Erlang 虚拟机的运行时状态和指标。

```bash
$ emqx ctl vm
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

| 命令                                           | 描述                                                                                                                                                                                 |
| ---------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| log set-level \<Level\>                        | 设置整体日志级别。                                                                                                                                                                   |
| log primary-level                              | 显示当前主要日志级别。`primary-level`代表 EMQX 的主要日志级别，用于指定整个系统的默认日志级别。设置`primary-level`会影响所有的日志输出，除非特定的日志处理程序有自己独立的日志级别。 |
| log primary-level \<Level\>                    | 设置主要日志级别。                                                                                                                                                                   |
| log handlers list                              | 显示日志处理 handlers。`handlers`是指定用于处理日志的日志处理程序的集合。每个日志处理程序可以独立设置自己的日志级别，并定义如何处理和存储日志消息。                                  |
| log handlers start \<HandlerId\>               | 启动某个 handler。                                                                                                                                                                   |
| log handlers stop \<HandlerId\>                | 停止某个 handler。                                                                                                                                                                   |
| log handlers set-level \<HandlerId\> \<Level\> | 设置某个 handler 日志级别。                                                                                                                                                          |

### log set-level \<Level\>

```bash
$ emqx ctl log set-level debug
debug
```

### log primary-level

```bash
$ emqx ctl log primary-level
debug
```

### log primary-level \<Level\>

```bash
$ emqx ctl log primary-level info
info
```

### log handlers list

```bash
$ emqx ctl log handlers list
LogHandler(id=ssl_handler, level=debug, destination=console, status=started)
LogHandler(id=console, level=debug, destination=console, status=started)
```

### log handlers start \<HandlerId\>

```bash
$ emqx ctl log handlers start console
log handler console started
```

### log handlers stop \<HandlerId\>

```bash
$ emqx ctl log handlers stop console
log handler console stopped
```

### log handlers set-level \<HandlerId\> \<Level\>

```bash
$ emqx ctl log handlers set-level console debug
debug
```

## trace

用于对一个给定的客户端或主题进行日志追踪。

| 命令                                                 | 描述                           |
| ---------------------------------------------------- | ------------------------------ |
| trace list                                           | 列出本地节点上启动的所有跟踪。 |
| trace start client \<ClientId\> \<File\> [\<Level\>] | 为客户端启动跟踪。             |
| trace stop client \<ClientId\>                       | 停止对客户端的跟踪。           |
| trace start topic \<Topic\> \<File\> [\<Level\>]     | 为主题启动跟踪。               |
| trace stop topic \<Topic\>                           | 停止对主题的跟踪。             |
| trace start ip_address \<IP\> \<File\> [\<Level\>]   | 为客户端 IP 地址启动跟踪。     |
| trace stop ip_address \<IP\>                         | 停止对客户端 IP 地址的跟踪。   |

### trace list

```bash
$ emqx ctl trace list
Trace(ip_address=127.0.0.1, level=debug, destination="trace.log")
```

### trace start client \<ClientId\> \<File\> [\<Level\>]

```bash
$ emqx ctl trace start client emqx_c trace.log debug
trace emqx_c CLI-emqx_c successfully
```

### trace stop client \<ClientId\>

```bash
$ emqx ctl trace stop client emqx_c
stop tracing clientid emqx_c successfully
```

### trace start topic <Topic> <File> [<Level>]

```bash
$ emqx ctl trace start topic t/1 trace.log info
trace t/1 CLI-t/1 successfully
```

### trace stop topic <Topic>

```bash
$ emqx ctl trace stop topic t/1
stop tracing topic t/1 successfully
```

### trace start ip_address <IP> <File> [<Level>]

```bash
$ emqx ctl trace start ip_address 127.0.0.1 trace.log debug
trace 127.0.0.1 CLI-127.0.0.1 successfully
```

### trace stop ip_address <IP>

```bash
$ emqx ctl trace stop ip_address 127.0.0.1
stop tracing ip_address 127.0.0.1 successfully
```

::: tip
建议在命令行中使用绝对路径指定追踪日志的文件。例如：
`emqx ctl trace start client foobar /abs/path/to/trace.log debug`
:::

::: tip
也可以在控制台界面中管理追踪日志。参考[tracer 文档](../observability/tracer.md)
:::

## traces

这个命令跟 `trace` 命令一样，但是会在整个集群所有节点中都开始或停止一个 tracer，参照上文的 trace 命令即可。

## listeners

管理监听器。

| 命令                             | 描述                                                                 |
| -------------------------------- | -------------------------------------------------------------------- |
| listeners                        | 列出所有监听器的信息。                                               |
| listeners stop \<Identifier\>    | 停止一个监听器，Identifier 为 `{type}:{name}` 格式，如 `tcp:default` |
| listeners start \<Identifier\>   | 启动一个监听器。                                                     |
| listeners restart \<Identifier\> | 重启一个监听器。                                                     |

### listeners

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

### listeners stop \<Identifier\>

```bash
$ emqx ctl listeners stop tcp:default
Stop tcp:default listener successfully.
```

### listeners start \<Identifier\>

```bash
$ emqx ctl listeners start tcp:default
Started tcp:default listener successfully.
```

### listeners restart \<Identifier\>

```bash
$ emqx ctl listeners restart tcp:default
Restarted tcp:default listener successfully.
```

::: tip
停止监听器会导致所有通过该监听器接入的客户端都断开连接。
:::

## authz cache-clean

`emqx ctl authz cache-clean`

这个命令用于强制所有客户端的授权（ACL）缓存立刻失效。

## pem_cache

`emqx ctl pem_cache`

这个命令可以用于清除 x509 pem 证书的缓存。

## olp

`emqx ctl olp`

OLP 是 “overload protection” 的缩写。
`olp` 命令可以用于检查系统过载的状态，也可以用于关闭或开启系统过载保护。

您可以在 `overload_protection` 的配置文档中查看更多信息。

::: tip
`olp` 是默认开启的，如果从命令行改变这个状态，这个改变只能持续到系统重启。重启之后会回到配置文件中的状态。
:::

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

## gateway

查看和管理网关的启停状态。

| 命令                               | 描述                     |
| ---------------------------------- | ------------------------ |
| gateway list                       | 列出所有网关的信息。     |
| gateway lookup \<Name\>            | 查找特定网关的详细信息。 |
| gateway load \<Name\> \<JsonConf\> | 加载一个网关并配置参数。 |
| gateway unload \<Name\>            | 卸载一个网关。           |
| gateway stop \<Name\>              | 停止一个网关。           |
| gateway start \<Name\>             | 启动一个网关。           |

### gateway list

```bash
$ emqx ctl gateway list
Gateway(name=coap, status=running, clients=0, started_at=2023-05-22T14:23:50.353+08:00)
Gateway(name=exproto, status=unloaded)
Gateway(name=lwm2m, status=unloaded)
Gateway(name=mqttsn, status=unloaded)
Gateway(name=stomp, status=unloaded)
```

### gateway lookup \<Name\>

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

```bash
emqx ctl gateway load coap '{"type":"coap", ...}'
```

### gateway unload \<Name\>

```bash
$ emqx ctl gateway unload coap
ok
```

### gateway stop \<Name\>

```bash
$ emqx ctl gateway stop coap
ok
```

### gateway start \<Name\>

```bash
$ emqx ctl gateway start coap
ok
```

## gateway-metrics

`emqx ctl gateway-metrics`

查看网关的指标。

## rules

查看系统中创建的所有的规则。

| 命令                | 描述                                     |
| ------------------- | ---------------------------------------- |
| rules list          | 列出所有规则,包括规则的 ID、名称等信息。 |
| rules show <RuleID> | 显示特定规则的详细信息。                 |

请注意，下面是每个命令的执行示例：

### rules list

```bash
$ emqx ctl rules list
Rule{id=my-rule, name=, enabled=true, descr=this is my rule}
```

### rules show <RuleID>

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

{%emqxee%}

## license

| 命令                   | 描述                |
| ---------------------- | ------------------- |
| license info           | 显示 License 信息。 |
| license update License | 更新 License 信息。 |

### license info

```bash
$ emqx ctl license info
customer        : Evaluation
email           : contact@emqx.io
deployment      : default
max_connections : 100
start_at        : 2023-01-09
expiry_at       : 2028-01-08
type            : trial
customer_type   : 10
expiry          : false
```

### license update License

```bash
emqx ctl license update <YOUR_LICENSE_STRING>
```

请将 "YOUR_LICENSE_STRING" 替换为实际的 License 字符串。

{%endemqxee%}
