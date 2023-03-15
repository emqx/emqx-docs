# Create an EMQX cluster

EMQX supports creating clusters manually and automatically. 

:::tip Prerequisites:

- Knowledge of [Distributed clusters](./introduction.md).
- Knowledge of [Architecture and deployment prerequisites](./mria-introduction.md).
  :::

## Node discovery

EMQX's clustering feature is based on the [Ekka](https://github.com/emqx/ekka) library, a cluster management library developed for Erlang/OTP applications.

One of the crucial step for EMQX clustering is node discovery, which enables individual EMQ X nodes with different IP addresses or locations to discover and communicate with each other. EMQX supports multiple node discovery strategies:

| Strategy | Description                               |
| -------- | ----------------------------------------- |
| `manual` | Create a cluster through manual command   |
| `static` | Create a cluster using a static node list |
| `mcast`* | Create a cluster through UDP multicast    |
| `dns`    | Create a cluster using DNS A records      |
| `etcd`   | Create a cluster via etcd                 |
| `k8s`    | Create a cluster via Kubernetes service   |

[^*]: The multicast discovery strategy has been deprecated and will be removed in future releases.



Naming



## Manual clustering





## Auto clustering

Auto clustering in EMQX is a feature that allows multiple EMQX nodes to form a cluster automatically without manual configuration. Auto clustering simplifies the process of setting up an EMQX cluster and makes it easier to add or remove nodes from the cluster dynamically. It also provides fault tolerance and high availability by allowing the cluster to continue operating even if one or more nodes fail.

[^]: 

## Autocluster by static node list

In EMQX, autocluster by static node list is to use a pre-defined static node list on each node to join the cluster. After starting, the nodes will create a cluster automatically according to the node list.

Static clustering is the easiest way to create an EMQX cluster automatically with no dependencies on other network components or services. As long as each node can communicate with each other through the TCP protocol, they can form an EMQX cluster.

Configure the same cluster mode and node list in `emqx.conf`:

```bash
cluster {
    discovery_strategy = static
    static {
        seeds = ["emqx@s1.emqx.io", "emqx@s2.emqx.io"]
    }
}
```

Start all nodes one by one, and the cluster will be automatically established.

## Autocluster by DNS Record

### How it works

DNS Stands for Domain Name System. A DNS server returns the IP addresses after receiving a domain query, that is, the A records. DNS system allows multiple A records for one domain name, this makes a one-to-many mapping.

EMQX can use this one-to-many mapping to find all nodes that belong to one cluster, so that the nodes can join a cluster automatically.

**Configuration:** Most public cloud services have DNS services. After assigning a domain name, you only need to add the IP address of each EMQX node to the A record of this domain to finish the configuration.

:::tip

If EMQX is deployed in a private cloud or internal network, you will need to deploy your own DNS system, for example, with software [BIND](https://www.isc.org/bind/).

:::

After the preparation, we will configure in `emqx.conf` to let all nodes join the cluster using the same clustering method, `dns`.

EMQX supports DNS automatic clustering using DNS A record and DNS SRV record:

**Auto clustering using DNS A record:**

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        record_type = a
    }
}
```

**Auto clustering using DNS SRV record:**

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        record_type = srv
    }
}
```

Start all nodes one by one after the configuration, and the cluster will be automatically established.

## Autocluster using etcd

[etcd](https://etcd.io/) is an open-source project initiated by CoreOS. It is widely used in distributed systems for service discovery and connection establishing, which is exactly what EMQX autoclustering needs.

If you have an etcd server(cluster) in your network, EMQX can automatically create the cluster via etcd. For how to install and configure etcd, see [etcd Install](https://etcd.io/docs/latest/install/).

**Configuration:** You need to specify the address of the etcd server, multiple etcd servers can be separated with `,`; also the directory prefix and TTL used to specify the EMQX node.

Code example：

```bash
cluster {
    discovery_strategy = etcd
    etcd {
        server = "http://127.0.0.1:2379"
        prefix = emqxcl
        node_ttl = 1m
    }
}
```

After completing the configuration, we can start the EMQX nodes one by one, and use the etcdctl tool to observe the changes on the etcd server:

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

The result shows that all nodes are started normally and joined the cluster automatically.

## Autocluster on Kubernetes

[Kubernetes (K8s)](https://kubernetes.io) is Google's open source container management system. EMQ X can use kubernetes API for node discovery and auto clustering.

**Configuration**: You need to specify the Kubernetes API server for all nodes, the service name of EMQX on K8s, and the address type:

<!-- TODO 补充几个参数的作用介绍 -->

```bash
cluster {
    discovery_strategy = K8s
    K8s {
        apiserver = "http://10.110.111.204:8080"
        service_name = emqx
        address_type = ip
        app_name = emqx
        suffix = "pod.local"
        namespace = default
    }
}
```

Start all nodes one by one after the configuration, and the cluster will be automatically established.

::: tip

Calico rather Fannel plugin is recommended.
:::



## Query

## Leave





## Configure Network protocols

Each Erlang node can be connected via TCP or TLS. 

#### Set EMQX cluster protocols

Each Erlang node can be connected via TCP or TLS, and the connection method can be configured in `etc/emqx.conf`:

| Configuration item    | Type      | Default value       | Description                                                  |
| --------------------- | --------- | ------------------- | ------------------------------------------------------------ |
| cluster.proto_dist    | enum      |                     | Distributed protocol with optional values are: - inet_tcp: use TCP IPv4 - inet6_tcp: use TCP IPv6 - inet_tls: use TLS |
| node.ssl_dist_optfile | file path | `etc/ssl_dist.conf` | When `cluster.proto_dist` is selected as `inet_tls`, you need to configure the `etc/ssl_dist.conf` file and specify the TLS certificate. |
