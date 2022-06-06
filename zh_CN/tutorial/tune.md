---
# 编写日期
date: 2020-02-07 17:15:26
# 作者 Github 名称
author: wivwiv
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# 系统调优

*EMQX* 自 4.2 版本以来，在一台 8 核心、32G 内存的 CentOS 服务器上，MQTT 连接压力测试可达到 130 万。

100 万连接测试所需的 Linux 内核参数，网络协议栈参数，Erlang 虚拟机参数， *EMQX* 消息服务器参数设置如下:

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

持久化 'fs.file-max' 设置到 /etc/sysctl.conf 文件:
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
sysctl -w net.ipv4.ip_local_port_range='1000 65535'
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
sysctl -w net.netfilter.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_tcp_timeout_time_wait=30
```
TIME-WAIT Socket 最大数量、回收与重用设置:
```bash
sysctl -w net.ipv4.tcp_max_tw_buckets=1048576

# 注意：不建议开启该设置，NAT 模式下可能引起连接 RST
# sysctl -w net.ipv4.tcp_tw_recycle=1
# sysctl -w net.ipv4.tcp_tw_reuse=1
```
FIN-WAIT-2 Socket 超时设置:
```bash
sysctl -w net.ipv4.tcp_fin_timeout=15
```

## Erlang 虚拟机参数

优化设置 Erlang 虚拟机启动参数，配置文件 emqx/etc/emqx.conf:

```bash
## Erlang Process Limit
node.process_limit = 2097152

## Sets the maximum number of simultaneously existing ports for this system
node.max_ports = 1048576
```
## docker 参数调优

通常调优应该在docker的主机上做，但是如果一定要从docker内部做，可以参考如下例子:

```
docker run -d --name emqx -p 18083:18083 -p 1883:1883 -p 4369:4369 \
    --sysctl fs.file-max=2097152 \
    --sysctl fs.nr_open=2097152 \
    --sysctl net.core.somaxconn=32768 \
    --sysctl net.ipv4.tcp_max_syn_backlog=16384 \
    --sysctl net.core.netdev_max_backlog=16384 \
    --sysctl net.ipv4.ip_local_port_range=1000 65535 \
    --sysctl net.core.rmem_default=262144 \
    --sysctl net.core.wmem_default=262144 \
    --sysctl net.core.rmem_max=16777216 \
    --sysctl net.core.wmem_max=16777216 \
    --sysctl net.core.optmem_max=16777216 \
    --sysctl net.ipv4.tcp_rmem=1024 4096 16777216 \
    --sysctl net.ipv4.tcp_wmem=1024 4096 16777216 \
    --sysctl net.ipv4.tcp_max_tw_buckets=1048576 \
    --sysctl net.ipv4.tcp_fin_timeout=15 \
    emqx/emqx:latest
```

::: 友情提示
不要使用 `--privileged` 或者将系统内核目录挂载到docker中进行调优。
:::

## EMQX 消息服务器参数

设置 TCP 监听器的 Acceptor 池大小，最大允许连接数。
{% emqxce %}
配置文件 `emqx/etc/emqx.conf`
{% endemqxce %}
{% emqxee %}
配置文件 `emqx/etc/listeners.conf`
{% endemqxee %}

```bash
## TCP Listener
listener.tcp.external = 0.0.0.0:1883
listener.tcp.external.acceptors = 64
listener.tcp.external.max_connections = 1024000
```

## 测试客户端设置

测试客户端服务器在一个接口上，最多只能创建 65000 连接:
```bash
sysctl -w net.ipv4.ip_local_port_range="500 65535"
echo 1000000 > /proc/sys/fs/nr_open
ulimit -n 100000
```
### emqtt_bench

并发连接测试工具: <http://github.com/emqx/emqtt_bench>
