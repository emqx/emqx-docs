# 手动管理集群

EMQX 节点由其节点名所标识。

一个节点名由两部分组成，节点部分和主机部分，用`@`分隔。

主机部分必须是 IP 地址或带有点的 FQDN，
例如`myhost.example.domain`。

在本文档中，我们使用两个节点来演示手动集群步骤。

假设要在两台服务器 s1.emqx.io, s2.emqx.io 上部署 EMQX 集群:

| 节点名          | 主机名 (FQDN) |
|-----------------|---------------|
| emqx@s1.emqx.io | s1.emqx.io    |
| emqx@s2.emqx.io | s2.emqx.io    |

或者，如果您为主机分配了静态的 IP。

| 节点名            | IP 地址      |
|-------------------|--------------|
| emqx@192.168.0.10 | 192.168.0.10 |
| emqx@192.168.0.20 | 192.168.0.20 |

**注意：** 节点名格式为 <Name@Host>, Host 必须是 IP 地址或 FQDN (主机名。域名)

::: tip Tip
EMQX 节点名是不可变的，因为它们被内嵌到数据库
和数据文件中。 强烈建议 EMQX 节点名称使用静态 FQDN，
特别是当网络环境提供静态 IP 时。
:::


## 配置 emqx@s1.emqx.io 节点

emqx/etc/emqx.conf:

```bash
node.name = emqx@s1.emqx.io
# 或
node.name = emqx@192.168.0.10
```

也可通过环境变量:

```bash
export EMQX_NODE__NAME=emqx@s1.emqx.io && ./bin/emqx start
```

::: warning Warning
节点启动加入集群后，节点名称不能变更。
:::

## 配置 emqx@s2.emqx.io 节点

emqx/etc/emqx.conf:

```bash
node.name = emqx@s2.emqx.io
# 或
node.name = emqx@192.168.0.20
```

## 节点加入集群

两个节点启动后，在`s2.emqx.io`上执行如下命令：

```bash
$ ./bin/emqx_ctl 集群加入 emqx@s1.emqx.io

Join the cluster successfully.
Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```
::: tip tip
`s2.emqx.io` 加入 `s1.emqx.io` 组成集群后，
它的本地数据将被清除，来自节点`s1.emqx.io`的数据
会同步过来。
:::

EMQX 集群是为外部节点发送加入**请求**
到集群中的一个节点加入。

但是 **NOT** 集群中的节点邀请外部节点
加入。

例如如果一个`s3.emqx.io`要加入`s1`和`s2`的集群，
join 命令应该在 `s3` 上执行，但 **NOT** 在 `s1` 或 `s2` 上执行。

::: warning Warning
加入另一个集群将导致该节点离开它属于的当前集群。
:::

查询任意节点上的集群状态：

```bash
$ ./bin/emqx_ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

## 退出集群

节点退出集群，两种方式:

1. leave: 让本节点退出集群
2. force-leave: 在集群内移除节点

让 `emqx@s2.emqx.io` 主动退出集群:

```bash
$ ./bin/emqx_ctl cluster leave
```

或在 `s1.emqx.io` 上，从集群移除 `emqx@s2.emqx.io` 节点:

```bash
$ ./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io
```

## 单机伪分布式

对于只有单台服务器的用户来说，可以使用伪分布式集群。请注意，我们若要在单机上启动两个或多个 emqx 实例，为避免端口冲突，我们需要对其它节点的监听端口做出调整。

基本思路是复制一份 emqx 文件夹然后命名为 emqx2 ，将原先所有 emqx 节点监听的端口 port 加上一个偏移 offset 作为新的 emqx2 节点的监听端口。例如，将原先 emqx 的MQTT/TCP 监听端口由默认的 1883 改为了 2883 作为 emqx2 的 MQTT/TCP 监听端口。完成以上操作的自动化脚本可以参照 [集群脚本](https://github.com/terry-xiaoyu/one_more_emqx)，具体配置请参见 [配置项](../../admin/cfg.md)。

