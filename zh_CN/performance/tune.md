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

## Erlang 虚拟机参数

优化设置 Erlang 虚拟机启动参数，配置文件 etc/emqx.conf:

```bash
## 设置 Erlang 系统同时存在的最大端口数
node.max_ports = 2097152
```

## EMQX 消息服务器参数

设置 TCP 监听器的 Acceptor 池大小，最大允许连接数。

例如，TCP 监听器可使用如下配置。

```bash
## TCP Listener
listeners.tcp.$name.acceptors = 64
listeners.tcp.$name.max_connections = 1024000
```

## 测试客户端设置

测试客户端服务器在一个接口上，最多只能创建 65000 连接:

```bash
sysctl -w net.ipv4.ip_local_port_range="500 65535"
echo 1000000 > /proc/sys/fs/nr_open
ulimit -n 100000
```

### emqtt_bench

并发连接测试工具：[emqtt_bench](http://github.com/emqx/emqtt_bench)
