# 命令行

## 启动脚本

EMQX 的主启动脚本也支持运行一些基本的管理命令。
帮助信息如下：

```bash
$ emqx help
Usage: emqx COMMAND [help]

Commonly used COMMANDs:
  start:      Start EMQX in daemon mode
  console:    Start EMQX in an interactive Erlang or Elixir shell
  foreground: Start EMQX in foreground mode without an interactive shell
  stop:       Stop the running EMQX node
  ctl:        Administration commands, execute 'emqx ctl help' for more details

More:
  Shell attach:  remote_console | attach
  Up/Down-grade: upgrade | downgrade | install | uninstall
  Install info:  ertspath | root_dir | versions | root_dir
  Runtime info:  pid | ping | versions
  Advanced:      console_clean | escript | rpc | rpcterms | eval | eval-erl

Execute 'emqx COMMAND help' for more information
```

## ctl 命令

所有的 `ctl` 命令 `emqx ctl COMMAND ARGS ...` (或者等效地： `emqx_ctl COMMAND ARGS ...`) 
都需要 EMQX 服务启动之后才能运行。

`ctl` 命令通过启动一个隐藏的 Erlang 节点的方式，远程连接到指定的 EMQX 节点，并执行
一个 Erlang 远程调用然后打印返回的结果。

下面列举了所有 `ctl` 命令的子命令，和相应的简介。
本文档旨在介绍命令的功能。命令的详细参数介绍可以用 `help` 指令查看。

### status

`emqx ctl status`

快速查看当前运行的节点是否运行。

### broker

`emqx ctl broker`

查看当前节点的运行的版本状态以及运行时长。

### observer

`emqx ctl observer`

可以用于查看运行时状态。展示一个类似于 linux 的 `top` 命令的界面。

### cluster_call

`emqx ctl cluster_call`

该命令用于查看、调查甚至修改集群配置修改的同步状态。

EMQX 的 HTTP API 可以用于修改很多配置，当一个 API 被调用，例如从控制台界面的操作，来修改配置时，
在收到这个请求的节点会先将修改的内容在本地写入 `data/configs/cluster-override.conf`，然后
同样的操作会被记录在数据库中，并异步地转发到集群中的其他节点。

当由于某种原因，无法在另一个节点成功执行同样的修改，那么这个命令就可以很方便的查看这个异步复制的状态，
甚至可以强制跳过一个失败的复制。

EMQX 会为每个集群范围的配置修改生成一个ID，（tnxid），这个 ID 会在集群范围内严格递增，
每个修改，例如从控制台中修改一个配置之后，都会记录在数据库中。
下面这个例子，展示的是查看第二（tnxid=2）个修改的内容（这是一个启用 TLS 监听器的操作）。

```
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

::: warning Warning
`skip` 指令和 `fast_forward` 指令会迫使本地节点跳过一些（失败）的操作
这可能会导致集群内节点之间的配置不一致。
:::

### admins

`emqx ctl admins`

这个命令用于创建，修改，删除管理员账户。

### retainer

`emqx ctl retainer`

这个命令可以用于查看和管理 retain 的消息。
也可以用于为 retain 表创建索引：`emqx ctl retainer reindex` 。

### cluster

`emqx ctl cluster`

这个命令可以查看和管理节点的集群状态。
请注意，EMQX 加入集群的指令 `join` 是向参数中指定的节点发送一个 “请求”，而不“邀请”。
也就是说，`emqx ctl cluster join <OneOfTheClusteredNodes>` 指令用于请求加入
`OneOfTheClusteredNodes` 所在的集群，而不是让这个节点加入自己所在的集群。

### clients

`emqx ctl clients`

可以用于查看和管理客户端。

::: warning
如果系统中连接了大量的客户端 `list` 指令可能会比较耗时且耗资源。
:::


### topics

`emqx ctl topics`

这个命令在 4.x 系列中是 `route`，可用于查看当前系统中所有订阅的主题。

::: warning
如果集群中有大量的主题订阅，`list` 指令可能会比较耗时且耗资源。
:::

### subscriptions

`emqx ctl subscriptions`

这个命令可以用于查看，增加或者删除某个客户端的订阅。

:::warning
当系统中有大量的订阅客户端时，`list` 指令可能比较耗时且耗资源。
:::

### plugins

`emqx ctl plugins`

该命令用于查看和管理插件。

### vm

`emqx ctl vm`

用于查看 Erlang 虚拟机的运行时状态和指标。

### mnesia

`emqx ctl mnesia`

用于查看内置数据库（Mnesia）的运行状态和指标。

### log

`emqx ctl log`

用于管理日志参数，例如日志级别等。

### trace and traces

`emqx ctl trace`

用于对一个给定的客户端或主题进行日志追踪。

::: tip
建议在命令行中使用绝对路径指定追踪日志的文件。例如：
`emqx ctl trace start client foobar /abs/path/to/trace.log debug`
:::

::: tip
也可以在控制台界面中管理追踪日志。参考[tracer 文档](../observability/tracer.md)
:::

`emqx ctl traces`

这个命令跟 `trace` 命令一样，但是会在整个集群所有节点中都开始或停止一个 tracer。

### listeners

`emqx ctl listeners`

管理监听器。

::: warning
停止监听器会导致所有通过该监听器接入的客户端都断开连接。
:::


### authz cache-clean

`emqx ctl authz cache-clean`

这个命令用于强制所有客户端的授权（ACL）缓存立刻失效。

### pem_cache

`emqx ctl pem_cache`

这个命令可以用于清除 x509 pem 证书的缓存。

### olp

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

* coap
* exproto
* lwm2m
* mqttsn
* stomp

EMQX 的网关设计成可插拔。所以网关应用可以在启动/运行时注册到 EMQX 系统中。
一旦注册之后，就可以用 HTTP API 或者命令行来对网关进行管理了。

### gateway

`emqx ctl gateway`

用于查看和管理网关的启停状态。

### gateway-metrics

`emqx ctl gateway-metrics`

查看网关的指标。

### rules

`emqx ctl rules`

可用于查看系统中创建的所有的规则。
注意，命令行仅仅用于查看，规则的创建和更新等管理操作必需要在控制台的界面中操作。
