# 系统调优

由于物联网应用中的设备数量和数据量通常都很大，而 EMQX 作为消息服务器承担着处理和传递大量设备产生的消息的任务。在这种情况下，对 EMQX 进行系统调优变得尤为重要。

通过调优可以获得以下性能的最大化：

- **消息处理能力**：提高 EMQX 处理消息的速度和效率，确保它能够快速地接收、处理和转发设备产生的消息。

- **吞吐量**：提高吞吐量，确保系统能够及时处理和传递设备产生的消息。

- **稳定性**：减少高负载下的延迟、提高系统响应速度，并且降低系统崩溃或故障的风险。

本页提供生产部署与测试所需的 Linux 内核参数，网络协议栈参数，Erlang 虚拟机参数以及 EMQX 参数调优设置。

## 关闭交换分区

Linux 交换分区可能会导致 Erlang 虚拟机出现不确定的内存延迟，严重影响系统的稳定性。 建议永久关闭交换分区。

- 要立即关闭交换分区，执行命令 `sudo swapoff -a`。 
- 要永久关闭交换分区，在 `/etc/fstab` 文件中注释掉 `swap` 行，然后重新启动主机。

## Linux 操作系统参数

系统全局允许分配的最大文件句柄数:

```bash
# 2 millions system-wide
sysctl -w fs.file-max=2097152
sysctl -w fs.nr_open=2097152
echo 2097152 > /proc/sys/fs/nr_open
```

允许当前会话 / 进程打开文件句柄数:

```bash
ulimit -n 1048576
```

### /etc/sysctl.conf

持久化 `fs.file-max` 设置到 /etc/sysctl.conf 文件:

```bash
fs.file-max = 1048576
```

/etc/systemd/system.conf 设置服务最大文件句柄数:

```bash
DefaultLimitNOFILE=1048576
```

### /etc/security/limits.conf

/etc/security/limits.conf 持久化设置允许用户 / 进程打开文件句柄数:

```bash
*      soft   nofile      1048576
*      hard   nofile      1048576
```

### 禁用透明大页（THP）

EMQX 包含内置数据库工作负载。与其他数据库系统一样，强烈建议在启动 EMQX 前禁用透明大页（Transparent HugePages，THP）。

```bash
echo never > /sys/kernel/mm/transparent_hugepage/enabled
echo never > /sys/kernel/mm/transparent_hugepage/defrag
```

如果 EMQX 在高内存机器（>16 GB）上长时间运行后出现以下现象，请禁用 THP，以排除 THP 相关问题：

- 消息延迟不稳定。
- 内存使用量异常突增。
- EMQX `long_schedule` 警告日志。
- EMQX `runq_overload` 告警。

如果运行的是集群，建议先在部分节点上禁用 THP，以便对比效果。请注意，某些工作负载可能会从启用 THP 中受益。

如需使这些更改在重启后仍然生效，请参考操作系统文档选择合适的方法。

## TCP 协议栈网络参数

并发连接 backlog 设置:

```bash
sysctl -w net.core.somaxconn=32768
sysctl -w net.ipv4.tcp_max_syn_backlog=16384
sysctl -w net.core.netdev_max_backlog=16384
```

可用知名端口范围:

```bash
sysctl -w net.ipv4.ip_local_port_range='1024 65535'
```

TCP Socket 读写 Buffer 设置:

```bash
sysctl -w net.core.rmem_default=262144
sysctl -w net.core.wmem_default=262144
sysctl -w net.core.rmem_max=16777216
sysctl -w net.core.wmem_max=16777216
sysctl -w net.core.optmem_max=16777216
    
#sysctl -w net.ipv4.tcp_mem='16777216 16777216 16777216'
sysctl -w net.ipv4.tcp_rmem='1024 4096 16777216'
sysctl -w net.ipv4.tcp_wmem='1024 4096 16777216'
```

TCP 连接追踪设置:

```bash
sysctl -w net.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_tcp_timeout_time_wait=30
```

TIME-WAIT Socket 最大数量、回收与重用设置:

```bash
sysctl -w net.ipv4.tcp_max_tw_buckets=1048576
    
# 注意：不建议开启該设置，NAT 模式下可能引起连接 RST
# sysctl -w net.ipv4.tcp_tw_recycle=1
# sysctl -w net.ipv4.tcp_tw_reuse=1
```

FIN-WAIT-2 Socket 超时设置:

```bash
sysctl -w net.ipv4.tcp_fin_timeout=15
```

减少 TCP 报文重传次数:

```bash
sysctl -w net.ipv4.tcp_retries2=5
```

## Erlang 虚拟机参数

从 EMQX 6.3.0 开始，EMQX 根据节点可用的 CPU 资源自动设置 Erlang VM 资源限制。在 `etc/emqx.conf` 中配置以下参数。配置在节点重启后生效。

### 端口数和进程数限制

`node.max_ports` 控制 Erlang VM 可以同时打开的最大文件和 Socket 数量。默认值为 `auto`，EMQX 按照以下规则设置 Erlang VM 端口数限制（`+Q`）：

- 当节点有 1 至 8 个可用逻辑 CPU 时，每个 CPU 对应 `65536` 个端口。
- 当节点有超过 8 个可用逻辑 CPU 时，端口数限制为 `1048576`。

::: warning 重要提示
从早期 EMQX 版本升级时，可用逻辑 CPU 不超过 8 个的节点将以较低的端口数限制启动。如果自动计算值无法满足部署的连接数需求，请显式设置 `node.max_ports`，重启节点后再执行升级。
:::

EMQX 将 Erlang 进程数限制（`+P`）设置为解析后的 `node.max_ports` 值的 2 倍。如果显式配置 `node.process_limit`，只有大于计算结果的配置值才会生效。

如果自动计算的端口数限制无法满足高并发工作负载的连接需求，请显式设置 `node.max_ports`。例如：

```hocon
node.max_ports = 2097152
```

增大 `node.max_ports` 前，请确保操作系统的文件描述符限制和可用内存能够支持配置值。您可以在 EMQX Dashboard 的节点监控页面查看实际生效的端口数和进程数限制。

### Erlang 调度器

`node.schedulers` 通过 Erlang VM 的 `+S` 参数控制 Erlang 调度器数量。默认值为 `auto`，即使用 Erlang VM 实际可用的逻辑处理器数量，包括容器可用的 CPU 资源。

仅当需要覆盖检测值时，才将 `node.schedulers` 设置为正整数，例如，为同一主机上的其他工作负载预留 CPU 容量。

## EMQX 消息服务器参数

### 监听器 Acceptor 参数

为了优化连接处理能力，可以通过修改 `etc/emqx.conf` 配置文件，调整监听器 acceptor 池大小和 `max_connections` 限制。

以 TCP 监听器为例：

```bash
## TCP 监听器配置
listeners.tcp.$name.acceptors = 64
listeners.tcp.$name.max_connections = 1024000
```

- `acceptors`：用于处理入站连接的 acceptor 进程数量。
- `max_connections`：允许的最大并发连接数。

### 分布式端口缓冲区大小

对于拥有大量复制节点的大型集群，建议在核心节点上通过配置 `node.dist_buffer_size` 参数，调整分布式端口缓冲区大小。

示例：

```bash
# 缓冲区大小（单位：KB），以下配置将最大值设置为约 2GB
node.dist_buffer_size = 2097151
```

此项配置有助于核心节点在大量客户端同时重连时平稳应对流量高峰。

如果您在日志中看到类似以下的告警信息，增大该缓冲区也能有效缓解相关问题：

```
[warning] msg: busy_dist_port ...
```

### 压缩集群 RPC 流量 <Badge type="tip" text="6.3.3+" />

EMQX 可以压缩集群节点之间通过远程过程调用（RPC）通道传输的数据，包括转发的 MQTT 消息。压缩可以减少节点间 RPC 流量，但会增加发送节点和接收节点的 CPU 使用量。节点间带宽受限或节点间数据传输按流量计费时，可以启用压缩，例如集群跨可用区部署时。

在 `etc/emqx.conf` 中配置 RPC 压缩：

```hocon
rpc {
  compress_level = 1
  compress_threshold = 1KB
}
```

`rpc` 是只读根配置键。请在需要启用 RPC 压缩的每个节点上配置这些参数，并重启相应节点以使配置生效。

`rpc.compress_level` 接受 `0` 到 `9` 之间的整数：

- `0` 为默认值，表示禁用压缩。升级后，现有部署的行为保持不变。
- `1` 的压缩速度最快，建议以此作为初始配置。
- `9` 生成的数据最小。更高的压缩级别通常只能额外减少少量数据，但在处理大消息时会消耗更多 CPU。

`rpc.compress_threshold` 设置需要压缩的 RPC 数据包的最小长度。默认值为 `1KB`，小于该长度的数据包不进行压缩。设置为 `0` 时，EMQX 会压缩每个数据包。仅当 `rpc.compress_level` 大于 `0` 时，此配置项才会生效。

压缩阈值作用于完整的 RPC 数据包，而不是单条 MQTT 消息。RPC 可以批量打包发送到同一节点的多条消息，因此即使每条消息都小于配置的阈值，合并后的数据包仍可能达到压缩条件。

已压缩或已加密的消息载荷不会通过 RPC 压缩进一步缩小。压缩此类载荷只会增加 CPU 使用量。

::: tip
初始配置建议使用压缩级别 `1` 和默认阈值 `1KB`。在提高压缩级别或降低阈值前，先使用有代表性的工作负载评估 CPU 使用量和节点间 RPC 流量。
:::

## 测试客户端设置

测试客户端服务器在一个接口上，最多只能创建 65000 连接:

```bash
sysctl -w net.ipv4.ip_local_port_range="500 65535"
echo 1000000 > /proc/sys/fs/nr_open
ulimit -n 100000
```

### emqtt_bench

并发连接测试工具：[emqtt_bench](http://github.com/emqx/emqtt_bench)
