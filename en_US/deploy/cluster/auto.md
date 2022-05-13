# Auto Clustering

## Node Discovery and Autocluster

::: tip Erlang
EMQX's clustering is based on the Ekka library.
[Ekka] (https://github.com/emqx/ekka) is a cluster management library developed for
Erlang/OTP applications, supporting Erlang Node Auto-Discovery, Autocluster,
Network partition autoheal and autoclean.
:::

EMQX supports multiple node discovery strategies:

| Strategy | Description       |
| -------- | ----------------- |
| manual   | Create a cluster through manual command |
| static   | Autocluster of static node list |
| mcast    | Autocluster with UDP multicast mode |
| dns      | Autocluster of DNS A record |
| etcd     | Autocluster through etcd |
| k8s      | Autocluster of Kubernetes service |

### static
Configure a fixed node list to automatically discover and create clusters:

```bash
cluster.discovery = static
cluster.static.seeds = emqx@s1.emqx.io,emqx@s2.emqx.io
```

### mcast

Automatically discover and create clusters based on UDP multicast:

```bash
cluster.discovery = mcast
cluster.mcast.addr = 239.192.0.1
cluster.mcast.ports = 4369,4370
cluster.mcast.iface = 0.0.0.0
cluster.mcast.ttl = 255
cluster.mcast.loop = on
```

### DNS A records

Automatically discover and create clusters based on DNS A records:

```bash
cluster.discovery = dns
cluster.dns.name = localhost
cluster.dns.app  = ekka
```

### etcd

Automatically discover and create clusters based on [etcd](https://coreos.com/etcd/):

```bash
cluster.discovery = etcd
cluster.etcd.server = http://127.0.0.1:2379
cluster.etcd.prefix = emqcl
cluster.etcd.node_ttl = 1m
```

### kubernetes

Automatically discover and create clusters based on [Kubernetes](https://kubernetes.io/):

```bash
cluster.discovery = k8s
cluster.k8s.apiserver = http://10.110.111.204:8080
cluster.k8s.service_name = ekka
cluster.k8s.address_type = ip
cluster.k8s.app_name = ekka
```
