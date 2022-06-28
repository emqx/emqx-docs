# Auto Clustering

## Node Discovery and Autocluster

::: tip Erlang
EMQX's clustering is based on the Ekka library.
[Ekka] (https://github.com/emqx/ekka) is a cluster management library developed for
Erlang/OTP applications, supporting Erlang Node Auto-Discovery, Autocluster, and
Network partition autoheal and autoclean.
:::

EMQX supports multiple autocluster strategies:

| Strategy | Description                             |
|----------|-----------------------------------------|
| manual   | Create a cluster through manual command |
| static   | Discovery based on a static node list   |
| mcast    | Discovery with UDP multicast mode       |
| dns      | Discovery based on DNS records          |
| etcd     | Discovery via etcd                      |
| k8s      | Discovery via Kubernetes service        |

### static
Configure a fixed node list to automatically discover and create clusters:

```bash
cluster {
    discovery_strategy = static
    static {
        seeds = ["emqx@s1.emqx.io", "emqx@s2.emqx.io"] 
    }
}
```

### mcast

Automatically discover and create clusters based on UDP multicast:

```bash
cluster {
    discovery_strategy = mcast
    mcast {
        addr = "239.192.0.1"
        ports = [4369, 4370]
        iface = "0.0.0.0"
        ttl = 255
        loop = true
    }
}
```

### DNS A records

Automatically discover and create clusters based on DNS A records:

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        app = emqx
        type = a
    }
}
```

### DNS SRV records

Automatically discover and create clusters based on DNS SRV records:

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        app = emqx
        type = srv
    }
}
```

### etcd

Automatically discover and create clusters based on [etcd](https://coreos.com/etcd/):

```bash
cluster {
    discovery_strategy = etcd
    etcd {
        server = "http://127.0.0.1:2379"
        prefix = emqcl
        node_ttl = 1m
    }
}
```

### kubernetes

Automatically discover and create clusters based on [Kubernetes](https://kubernetes.io/):

```bash
cluster {
    discovery_strategy = k8s
    k8s {
        apiserver = "http://10.110.111.204:8080"
        service_name = emqx
        address_type = ip
        app_name = emqx
        suffix = "pod.local"
        namespace = default
    }
}
```
