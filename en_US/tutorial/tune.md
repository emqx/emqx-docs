# Tuning guide

This guide includes in general tuning suggestions for Linux and EMQX.

## Turn off swap

Linux swap partitions may cause nondeterministic memory latency to Erlang virtual machine,
which in turn significantly affects the system stability.
It is recommended to turn off swap permanently.

To turn off swap immediately, execute command `sudo swapoff -a`.
To turn off swap permanently, comment out the `swap` line in `/etc/fstab` and reboot the host

## Linux Kernel Tuning

The system-wide limit on max opened file handles:

```
# 2 millions system-wide
sysctl -w fs.file-max=2097152
sysctl -w fs.nr_open=2097152
echo 2097152 > /proc/sys/fs/nr_open
```

The limit on opened file handles for current session:

```
ulimit -n 2097152
```

### /etc/sysctl.conf

Persist 'fs.file-max' configuration to `/etc/sysctl.conf`:

```
fs.file-max = 2097152
```

Set the maximum number of file handles for the service in `/etc/systemd/system.conf`:

```
DefaultLimitNOFILE=2097152
```

### emqx.service

Set the maximum number of file handles for emqx service in e.g. one of below paths depending
on which linux distribution is in use.

- `/usr/lib/systemd/system/emqx.service`
- `/lib/systemd/system/emqx.service`

```
LimitNOFILE=2097152
```

### /etc/security/limits.conf

Persist the maximum number of opened file handles for users in `/etc/security/limits.conf`:

```
*      soft   nofile      2097152
*      hard   nofile      2097152
```

### Disable Transparent HugePages (THP)

EMQX includes a built-in database workload. Therefore, as recommended for other database systems, it is highly recommended to disable Transparent HugePages (THP) before starting EMQX.

```
echo never > /sys/kernel/mm/transparent_hugepage/enabled
echo never > /sys/kernel/mm/transparent_hugepage/defrag
```

If you experience the following symptoms after running EMQX for long period on a high-memory machine (>16 GB), we recommend disabling THP to rule out THP-related issues.

- Unstable message latency.
- Unexpected memory usage spikes.
- EMQX long_schedule warning logs.
- EMQX runq_overload alarm.

If you are running a cluster, we recommend disabling THP on a subset of nodes for comparison. Note that some workloads may actually benefit from having THP enabled.

To make these changes persistent across reboots, consult your OS documentation for the appropriate method.

## TCP Network Tuning

Increase number of incoming connections backlog:

```
sysctl -w net.core.somaxconn=32768
sysctl -w net.ipv4.tcp_max_syn_backlog=16384
sysctl -w net.core.netdev_max_backlog=16384
```

Local port range

```
sysctl -w net.ipv4.ip_local_port_range='1000 65535'
```

TCP Socket read/write buffer:

```
sysctl -w net.core.rmem_default=262144
sysctl -w net.core.wmem_default=262144
sysctl -w net.core.rmem_max=16777216
sysctl -w net.core.wmem_max=16777216
sysctl -w net.core.optmem_max=16777216

#sysctl -w net.ipv4.tcp_mem='16777216 16777216 16777216'
sysctl -w net.ipv4.tcp_rmem='1024 4096 16777216'
sysctl -w net.ipv4.tcp_wmem='1024 4096 16777216'
```

Unless necessary, please DO NOT enable the `nf_conntrack` feature. 
If it is already enabled, please set the maximum allowed number of connections based on the estimated number of connections.

```
sysctl -w net.netfilter.nf_conntrack_max=1000000
```

TIME-WAIT Bucket Pool, Recycling and Reuse:

```
sysctl -w net.ipv4.tcp_max_tw_buckets=1048576

# Enabling following option is not recommended. It could cause connection reset under NAT
# sysctl -w net.ipv4.tcp_tw_recycle=1
# sysctl -w net.ipv4.tcp_tw_reuse=1
```

Timeout for FIN-WAIT-2 Sockets:

```
sysctl -w net.ipv4.tcp_fin_timeout=15
```

## Erlang VM Tuning


Tuning and optimize the Erlang VM in etc/emqx.conf file


```bash
## Erlang Process Limit
node.process_limit = 2097152

## Sets the maximum number of simultaneously existing ports for this system
node.max_ports = 2097152
```

## Cluster Inter-Node Connection Tuning

For clustered deployments, EMQX uses Erlang's distributed communication channels for inter-node RPC. Tuning the TCP socket buffers for these connections can significantly reduce RPC latency and improve cluster stability, especially under high network latency or large message volumes.

The following options control TCP socket buffers for outgoing connections this node makes to other nodes:

```bash
node.dist_connect_options.nodelay = false
node.dist_connect_options.sndbuf = 1MB
node.dist_connect_options.recbuf = 1MB
node.dist_connect_options.buffer = 1MB
```

The following options control the TCP listener that accepts incoming connections from other nodes:

```bash
node.dist_listen_options.nodelay = false
node.dist_listen_options.sndbuf = 1MB
node.dist_listen_options.recbuf = 1MB
node.dist_listen_options.buffer = 1MB
```

It is recommended to keep `buffer` at or above `max(sndbuf, recbuf)`. Increase these values if you observe high RPC latency or instability in clusters with high throughput or inter-datacenter links. For a full parameter reference, see [Configuration](../configuration/configuration.md).

## When running in docker

Usually you should tune the linux docker host by following the above guide.

If you want to tune linux kernel by docker, you must ensure your docker is latest version (>=1.12).

Here is an example to show how it looks.

```
docker run -d --name emqx -p 18083:18083 -p 1883:1883 \
    --sysctl fs.file-max=2097152 \
    --sysctl fs.nr_open=2097152 \
    --sysctl net.core.somaxconn=32768 \
    --sysctl net.ipv4.tcp_max_syn_backlog=16384 \
    --sysctl net.core.netdev_max_backlog=16384 \
    --sysctl net.ipv4.ip_local_port_range='1000 65535' \
    --sysctl net.core.rmem_default=262144 \
    --sysctl net.core.wmem_default=262144 \
    --sysctl net.core.rmem_max=16777216 \
    --sysctl net.core.wmem_max=16777216 \
    --sysctl net.core.optmem_max=16777216 \
    --sysctl net.ipv4.tcp_rmem='1024 4096 16777216' \
    --sysctl net.ipv4.tcp_wmem='1024 4096 16777216\ \
    --sysctl net.ipv4.tcp_max_tw_buckets=1048576 \
    --sysctl net.ipv4.tcp_fin_timeout=15 \
    emqx/emqx:latest
```

::: REMEMBER
The best practice is NOT to run docker `--privileged` and NOT to mount system volumes to the container for kernel tuning.
:::

## EMQX Broker Tuning

Tune the acceptor pool, max_clients limit and socket options.
{% emqxce %}
Find listeners config in `etc/emqx.conf`
{% endemqxce %}
{% emqxee %}
Find listeners config in `etc/listeners.conf`
{% endemqxee %}

```bash
## TCP Listener
listener.tcp.external = 0.0.0.0:1883
listener.tcp.external.acceptors = 64
listener.tcp.external.max_connections = 1024000
```

## Client Machine Tuning

 Tune the client machine to benchmark emqttd broker:
```
sysctl -w net.ipv4.ip_local_port_range="500 65535"
echo 1000000 > /proc/sys/fs/nr_open
ulimit -n 100000
```
### emqtt_bench

 Test tool for concurrent connections:  <http://github.com/emqx/emqtt_bench>
