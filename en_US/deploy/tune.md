# Linux OS Tuning

This guide includes in general tuning suggestions for benckmark and deployment.

## Turn off Swap

Linux swap partitions may cause nondeterministic memory latency to an Erlang virtual machine, significantly affecting the system stability.
It is recommended to turn off the swap permanently.

To turn off swap immediately, execute the command `sudo swapoff -a`.
To turn off swap permanently, comment out the `swap` line in `/etc/fstab` and reboot the host.

## Linux Kernel Tuning

The system-wide limit on max opened file handles:

```bash
# 2 millions system-wide
sysctl -w fs.file-max=2097152
sysctl -w fs.nr_open=2097152
echo 2097152 > /proc/sys/fs/nr_open
```

The limit on opened file handles for the current session:

```bash
ulimit -n 2097152
```

### `/etc/sysctl.conf`

Persist 'fs.file-max' configuration to `/etc/sysctl.conf`:

```bash
fs.file-max = 2097152
```

Set the maximum number of file handles for the service in `/etc/systemd/system.conf`:

```bash
DefaultLimitNOFILE=2097152
```

### `emqx.service`

Set the maximum number of file handles for emqx service in one of the below paths depending on which Linux distribution is used.

- `/usr/lib/systemd/system/emqx.service`
- `/lib/systemd/system/emqx.service`

```bash
LimitNOFILE=2097152
```

### `/etc/security/limits.conf`

Persist the maximum number of opened file handles for users in `/etc/security/limits.conf`:

```bash
*      soft   nofile      2097152
*      hard   nofile      2097152
```

## TCP Network Tuning

Increase the number of incoming connections backlog:

```bash
sysctl -w net.core.somaxconn=32768
sysctl -w net.ipv4.tcp_max_syn_backlog=16384
sysctl -w net.core.netdev_max_backlog=16384
```

Local port range

```bash
sysctl -w net.ipv4.ip_local_port_range='1000 65535'
```

TCP Socket read/write buffer:

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

TCP connection tracking:

```bash
sysctl -w net.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_tcp_timeout_time_wait=30
```

TIME-WAIT Bucket Pool, Recycling, and Reuse:

```bash
sysctl -w net.ipv4.tcp_max_tw_buckets=1048576

# Enabling following option is not recommended. It could cause connection reset under NAT
# sysctl -w net.ipv4.tcp_tw_recycle=1
# sysctl -w net.ipv4.tcp_tw_reuse=1
```

Timeout for FIN-WAIT-2 Sockets:

```bash
sysctl -w net.ipv4.tcp_fin_timeout=15
```

## Erlang VM Tuning

Tune and optimize the Erlang VM in etc/emq.conf file: :

```bash
## Erlang Process Limit
node.process_limit = 2097152

## Sets the maximum number of simultaneously existing ports for this system
node.max_ports = 2097152
```

## EMQX Broker Tuning

Tune the acceptor pool size and `max_connections` limit in `etc/emqx.conf`.

E.g, for TCP listeners:

```bash
## TCP Listener
listeners.tcp.$name.acceptors = 64
listeners.tcp.$name.max_connections = 1024000
```

## Client Machine Tuning

Tune the client machine to benchmark EMQX broker:

```
sysctl -w net.ipv4.ip_local_port_range="500 65535"
echo 1000000 > /proc/sys/fs/nr_open
ulimit -n 100000
```


### MQTT Benchmarking

Test tools for concurrent connections: [emqtt_bench](https://github.com/emqx/emqtt_bench).
