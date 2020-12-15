# Tuning guide

For EMQ X Message Server 4.x version, MQTT connection stress test reached 1.3 million on an 8-core, 32G memory CentOS server.

Linux kernel parameters, network protocol stack parameters, Erlang virtual machine parameters, and EMQ X message server parameter settings required for the 1 million connection test are as follows:

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
ulimit -n 1048576
```
### /etc/sysctl.conf

Persist 'fs.file-max' configuration to /etc/sysctl.conf file:
```
fs.file-max = 1048576
```
Set the maximum number of file handles for the service in /etc/systemd/system.conf:
```
DefaultLimitNOFILE=1048576
```
### /etc/security/limits.conf

 Persist the maximum number of opened file handles for users in /etc/security/limits.conf: 
```
*      soft   nofile      1048576
*      hard   nofile      1048576
```
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
 TCP connection tracking: 
```
sysctl -w net.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_max=1000000
sysctl -w net.netfilter.nf_conntrack_tcp_timeout_time_wait=30
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

 Tuning and optimize the Erlang VM in etc/emq.conf file: :

```bash
## Erlang Process Limit
node.process_limit = 2097152

## Sets the maximum number of simultaneously existing ports for this system
node.max_ports = 1048576
```

## EMQ X Broker Tuning

 Tune the acceptor pool, max_clients limit and sockopts for TCP listener in etc/emqx.conf: 

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
