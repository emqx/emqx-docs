# 命令行

本章节向您介绍 EMQX 支持的各类启动与管理命令，并详细介绍 ctl 管理命令。

## 启动命令

<!-- TODO  启动命令应该放到单独的一个章节，本章节只介绍 ctl 管理命令 -->

EMQX 支持一些基本的启动和管理命令，您可以通过 `emqx <command>` 命令执行。

以下是常用的启动和管理命令：

| 命令       | 描述                                                         |
| ---------- | ------------------------------------------------------------ |
| start      | 以守护进程模式启动 EMQX，运行期间不需要交互式 shell          |
| console    | 在 Erlang 或 Elixir 交互式 shell 中启动 EMQX。用于在开发环境中调试 EMQX，需要与 EMQX 进行交互 |
| foreground | 在前台模式下启动 EMQX，不使用交互式 shell。用于在开发环境中启动 EMQX，但不需要后台运行 |
| stop       | 停止运行中的 EMQX 节点                                       |
| ctl        | 管理和监控 EMQX，执行 `emqx ctl help` 可以获取更多详细信息   |

以下是用于开发调试的高级命令，普通用户通常无需关心：

| 命令           | 描述                                       |
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

| 命令              | 描述                                                         |
| ----------------- | ------------------------------------------------------------ |
| observer status   | 在当前控制台启动观察器，用于监视和调试 EMQX 节点的状态和活动。 |
| observer bin_leak | 强制所有进程执行垃圾回收，并打印释放最大数量二进制数据的前 100 个进程，可能会显示出潜在的内存泄漏问题。 |
| observer load Mod | 确保指定的模块在 EMQX 集群中的所有节点上都已加载。当需要确保模块在整个集群中都可用时，可以使用此命令来加载模块。 |

### observer status
<!-- TODO -->
### observer bin_leak
<!-- TODO -->

### observer load Mod
<!-- TODO -->

## cluster_call

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

::: tip
`skip` 指令和 `fast_forward` 指令会迫使本地节点跳过一些（失败）的操作
这可能会导致集群内节点之间的配置不一致。
:::

## admins

`emqx ctl admins`

这个命令用于创建，修改，删除管理员账户。

## retainer

`emqx ctl retainer`

这个命令可以用于查看和管理 retain 的消息。
也可以用于为 retain 表创建索引：`emqx ctl retainer reindex` 。

## cluster

`emqx ctl cluster`

这个命令可以查看和管理节点的集群状态。
请注意，EMQX 加入集群的指令 `join` 是向参数中指定的节点发送一个 “请求”，而不“邀请”。
也就是说，`emqx ctl cluster join <OneOfTheClusteredNodes>` 指令用于请求加入
`OneOfTheClusteredNodes` 所在的集群，而不是让这个节点加入自己所在的集群。

## clients

`emqx ctl clients`

可以用于查看和管理客户端。

::: tip
如果系统中连接了大量的客户端 `list` 指令可能会比较耗时且耗资源。
:::

## topics

`emqx ctl topics`

这个命令在 4.x 系列中是 `route`，可用于查看当前系统中所有订阅的主题。

::: tip
如果集群中有大量的主题订阅，`list` 指令可能会比较耗时且耗资源。
:::

## subscriptions

`emqx ctl subscriptions`

这个命令可以用于查看，增加或者删除某个客户端的订阅。

:::tip
当系统中有大量的订阅客户端时，`list` 指令可能比较耗时且耗资源。
:::

## plugins

`emqx ctl plugins`

该命令用于查看和管理插件。

## vm

`emqx ctl vm`

用于查看 Erlang 虚拟机的运行时状态和指标。

## mnesia

`emqx ctl mnesia`

用于查看内置数据库（Mnesia）的运行状态和指标。

## log

`emqx ctl log`

用于管理日志参数，例如日志级别等。

## trace and traces

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

## listeners

`emqx ctl listeners`

管理监听器。

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

* coap
* exproto
* lwm2m
* mqttsn
* stomp

EMQX 的网关设计成可插拔。所以网关应用可以在启动/运行时注册到 EMQX 系统中。
一旦注册之后，就可以用 HTTP API 或者命令行来对网关进行管理了。

## gateway

`emqx ctl gateway`

用于查看和管理网关的启停状态。

## gateway-metrics

`emqx ctl gateway-metrics`

查看网关的指标。

## rules

`emqx ctl rules`

可用于查看系统中创建的所有的规则。
注意，命令行仅仅用于查看，规则的创建和更新等管理操作必需要在控制台的界面中操作。
