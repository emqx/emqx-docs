# Create and manage clusters

EMQX supports creating clusters manually and automatically. This chapter will guide you through creating and managing EMQX clusters.

:::tip Prerequisites:

- Knowledge of [Distributed Clusters](./introduction.md).
- Knowledge of [Deployment Architecture and Cluster Requirements](./mria-introduction.md).
  :::

## Preparations before clustering

1. All nodes are set with a unique node name in the format of `name@host`, where host must be an IP address or fully qualified domain name (FQDN).
2. If there is a firewall or security group between nodes, ensure the cluster communication port has been opened. For details, see [Intra-cluster communication port](./security.md).
3. All nodes use the same cookie.

:::tip

EMQX node names are immutable, as they are baked into the database schema and data files. It is strongly recommended to use static FQDNs for EMQX node names, even when the nodes have static IPs.
:::

## Create clusters manually

If you choose a manually create a cluster, you will need to manually configure each node in the cluster, including setting up network connections among them.

Compared with automatic clustering, you can customize the network topology with manual clustering. It is especially suitable when the automatic clustering mechanism is unavailable or inappropriate.

:::tip
Manual clustering only can be only on core nodes. For core-replica deployment architecture, only auto clustering is supported.
:::

Suppose there are two nodes, `emqx@s1.emqx.io` and `emqx@s2.emqx.io`, and we can follow the steps below to create a cluster for these two nodes manually:

1. Set the cluster discovery strategy to `manual`:

   ```bash
   cluster {
       ## Options: manual | static | mcast | dns | etcd | K8s
       discovery_strategy  =  manual
   }
   ```

2. After the two nodes are started, run the join cluster command on one node:

   ```bash
   $ ./bin/emqx_ctl cluster join emqx@s1.emqx.io

   Join the cluster successfully.
   Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
   ```

   :::tip

   1. This command must be run on the node to join the cluster, that is, as a **request** rather than **invite**.
   2. After `emqx@s2.emqx.io` joins `emqx@s1.emqx.io` to form a cluster, it will clear the local data and synchronize the data in `emqx@s1.emqx.io`.
   3. When a node that already joined a cluster joins another cluster, it should first leave the current cluster.
      :::

3. Query the cluster status on any node:

   ```bash
   $ ./bin/emqx_ctl cluster status

   Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
   ```

4. You can let a node leave the cluster with leave or force-leave:

   1. leave: let the node leave the cluster
   2. force-leave: remove the node from the cluster

   Run the command below on `emqx@s2.emqx.io` to let it leave the cluster:

   ```bash
   ./bin/emqx_ctl cluster leave
   ```

   Or run the command below on `emqx@s1.emqx.io` to remove `emqx@s2.emqx.io` from the cluster:

   ```bash
   ./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io
   ```

### Pseudo-distributed cluster

For users with only one server, you can use the pseudo-distributed cluster to test the EMQX cluster.

After starting the first node, use the following command to start the second node and join the cluster manually. To avoid port conflicts, we need to adjust some listening ports:

```bash
EMQX_NODE__NAME='emqx2@127.0.0.1' \
    EMQX_STATSD__SERVER='127.0.0.1:8124' \
    EMQX_LISTENERS__TCP__DEFAULT__BIND='0.0.0.0:1882' \
    EMQX_LISTENERS__SSL__DEFAULT__BIND='0.0.0.0:8882' \
    EMQX_LISTENERS__WS__DEFAULT__BIND='0.0.0.0:8082' \
    EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8085' \
    EMQX_DASHBOARD__LISTENERS__HTTP__BIND='0.0.0.0:18082' \
    EMQX_NODE__DATA_DIR="./data2" \
./bin/emqx start

./bin/emqx_ctl cluster join emqx1@127.0.0.1
```

## Node discovery and auto clustering

Node discovery allows individual EMQX node with different IPs or locations to be discovered and communicated with each other, and it is a crucial step when creating EMQX clusters.

EMQX's autocluster feature is based on the [Ekka](https://github.com/emqx/ekka) library, the cluster management library developed for Erlang/OTP applications, supporting features like Erlang node service discovery, autocluster, network partition autoheal, and autoclean.

EMQX supports multiple autocluster strategies:

| Strategy | Description                               |
| -------- | ----------------------------------------- |
| manual   | Create a cluster through manual command   |
| static   | Create a cluster using a static node list |
| dns      | Create a cluster using DNS A records      |
| etcd     | Create a cluster via etcd                 |
| k8s      | Create a cluster via Kubernetes service   |

By default, EMQX adopts a manual clustering strategy, which can be set in `emqx.conf`:

```bash
cluster {
    ## options: manual | static | dns | etcd | K8s
    discovery_strategy  =  manual
}
```

Note: The mcast discovery strategy has been deprecated and will be removed in future releases.

## Autocluster by static node list

The static clustering of EMQX is to use a static node list pre-configured on each node to join the cluster. After starting, the nodes will create a cluster automatically according to the node list.

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

### Working principles

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
