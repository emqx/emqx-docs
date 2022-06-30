# 自动自发现集群

## 节点发现与自动集群

EMQX 支持基于 Ekka 库的集群自动发现 (Autocluster)。Ekka 是为 Erlang/OTP 应用开发的集群管理库，支持
Erlang 节点自动发现 (Service Discovery)、自动集群 (Autocluster)、脑裂自动愈合 (Network Partition
Autoheal)、自动删除宕机节点 (Autoclean)。

EMQX 支持多种节点发现策略:

| 策略     | 说明                |
| ------ | ----------------- |
| static | 静态节点列表自动集群        |
| mcast  | UDP 组播方式自动集群      |
| dns    | DNS A 记录自动集群      |
| etcd   | 通过 etcd 自动集群      |
| k8s    | Kubernetes 服务自动集群 |

### 基于 static 节点列表自动集群

配置固定的节点列表，自动发现并创建集群:

```bash
cluster {
    discovery_strategy = static
    static {
        seeds = ["emqx@s1.emqx.io", "emqx@s2.emqx.io"] 
    }
}
```

### 基于 mcast 组播自动集群

基于 UDP 组播自动发现并创建集群:

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

### 基于 DNS A 记录自动集群

基于 DNS A 记录自动发现并创建集群:

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        record_type = a
    }
}
```

### 基于 DNS SRV 记录自动集群

基于 DNS SRV 记录自动发现并创建集群:

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        record_type = srv
    }
}
```

### 基于 etcd 自动集群

基于 [etcd](https://coreos.com/etcd/) 自动发现并创建集群:

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

### 基于 kubernetes 自动集群

[Kubernetes](https://kubernetes.io/) 下自动发现并创建集群:

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

::: tip
Kubernetes 不建议使用 Fannel 网络插件，推荐使用 Calico 网络插件。
:::

