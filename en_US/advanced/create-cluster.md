# Create a Cluster

With EMQX cluster, you can enjoy the benefits of fault tolerance and high availability by allowing the cluster to continue operating even if one or more nodes fail. You can create an EMQX cluster either manually or automatically. Before the actual clustering step, let's first get familiar with 2 key concepts: node names and node discovery.

## Concepts

### Node Names

Before starting the cluster creation step, let's first get familiar with the concept of node names in EMQX. EMQX nodes are identified by their names. A node name consists of two parts: node name and host, separated with `@`, for example, `emqx@s1.emqx.io`. The host part must either be the IP address or a fully qualified domain name (FQDN), such as `myhost.example.tld`, for example:

- For EMQX node deployed on server `s1.emqx.io`, the node name should be `emqx@s1.emqx.io`;
- If this server has a static IP (`192.168.0.10`), the node name should be `emqx@192.168.0.10`.

:::tip

EMQX node names are immutable, as they are baked into the database schema and data files. Therefore, it is recommended to use static FQDNs for EMQX node names.

:::

### Node Discovery

One of the crucial steps for EMQX clustering is node discovery, which enables individual EMQX nodes with different IP addresses or locations to discover and communicate with each other. EMQX supports multiple node discovery strategies:

| Strategy | Description                                  |
| -------- | -------------------------------------------- |
| `manual` | Create a cluster through manual command      |
| `static` | Create a cluster using a static node list    |
| `mcast`* | Create a cluster through UDP multicast       |
| `dns`    | Create a cluster using DNS A and SRV records |
| `etcd`   | Create a cluster via etcd                    |
| `k8s`    | Create a cluster via Kubernetes service      |

[^*]: The multicast discovery strategy has been deprecated and will be removed in future releases.

## Before You Begin

Before creating an EMQX cluster, the following prerequisites should first be met:

1. All nodes are set with a unique node name in the format of `name@host`, where host must be an IP address or fully qualified domain name (FQDN). For more information on configuring node names, see [Configure Node Names](#configure-node-names).

2. If there is a firewall or security group between nodes, ensure the cluster communication port has been opened. For details, see [Firewall Settings](./cluster-security.md#firewall-settings).

3. For security concerns, you should change the default cookie settings to a Secret cookie in `emqx.conf` on all nodes to join the cluster. Note: All nodes to join the cluster should use the same Secret cookie. For details about the magic cookie used, see [Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security).

   ```text
   node {
     cookie = "<a Secret cookie>"
   }
   ```

Now you can begin your journey to create an EMQX cluster.

## Configure Node Names

Before creating a cluster, you need first to name the nodes to join the cluster. Suppose you want to create a cluster for 2 nodes deployed in `s1.emqx.io` and `s2.emqx.io` respectively, you can follow the steps below to create the cluster.

Configure the node name in the `emqx.conf` configuration file of the 1st node, for example:

```bash
node.name = emqx@s1.emqx.io
```

You can also override the node name with an environment variable:

```bash
export EMQX_NODE_NAME=emqx@s1.emqx.io && ./bin/emqx start
```

Repeat the above step for the other node to join the cluster.

Now you have named 2 nodes to join the cluster, `emqx@s1.emqx.io` and `emqx@s2.emqx.io`. You can create a cluster either manually or automatically.

::: tip Tip
After a node starts to join the cluster, the node name cannot be changed.
:::

## Manual Clustering

### Set Node Discovery Strategy

EMQX supports creating clusters manually and automatically. This section will introduce the manual clustering feature in EMQX.

Manual clustering is the method to configure an EMQX cluster by manually specifying which nodes should be part of the cluster. By default, EMQX adopts a manual clustering strategy, which can also be set in `cluster.conf`. 

```bash
cluster.discovery = manual
```

### Configure Nodes to Join a Cluster

After the nodes are started, you can run the `cluster join` command on the node that you want to join the cluster. For example, you want `emqx@s2.emqx.io` to join `emqx@s1.emqx.io`, run the command below on `emqx@s2.emqx.io`：

```bash
$ ./bin/emqx_ctl cluster join emqx@s1.emqx.io

Join the cluster successfully.
Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

Or executed on s1.emqx.io:

```bash
$ ./bin/emqx_ctl cluster join emqx@s2.emqx.io

Join the cluster successfully.
Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

Query the cluster status on any node:

```bash
$ ./bin/emqx_ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

## Auto Clustering

Auto clustering in EMQX is another feature that allows multiple EMQX nodes to form a cluster automatically without manual configuration. Auto clustering simplifies the process of setting up an EMQX cluster and makes it easier to add or remove nodes from the cluster dynamically.

EMQX supports auto clustering based on static node list, DNS Record, etcd, and Kubernetes. Continue to read to learn how to work with these features.

### Autocluster by Static Node List

Configure a fixed node list to automatically discover and create clusters:

```bash
cluster.discovery = static
cluster.static.seeds = emqx1@127.0.0.1,emqx2@127.0.0.1
```

### Autocluster by mcast  

Automatically discover and create clusters based on UDP multicast:

```bash
cluster.discovery = mcast
cluster.mcast.addr = 239.192.0.1
cluster.mcast.ports = 4369,4370
cluster.mcast.iface = 0.0.0.0
cluster.mcast.ttl = 255
cluster.mcast.loop = on
```

### Autocluster based on DNS Records 

Automatically discover and create clusters based on DNS A records:

```bash
cluster.discovery = dns
cluster.dns.name = localhost
cluster.dns.app  = ekka
```

### Autocluster Using etcd 

Automatically discover and create clusters based on [etcd](https://coreos.com/etcd/):

```bash
cluster.discovery = etcd
cluster.etcd.server = http://127.0.0.1:2379
cluster.etcd.prefix = emqcl
cluster.etcd.node_ttl = 1m
```

### Autocluster on Kubernetes 

Automatically discover and create clusters based on [Kubernetes](https://kubernetes.io/):

```bash
cluster.discovery = k8s
cluster.k8s.apiserver = http://10.110.111.204:8080
cluster.k8s.service_name = ekka
cluster.k8s.address_type = ip
cluster.k8s.app_name = ekka
```

## Manage Cluster Nodes

There are two ways for a node to exit the cluster:

1. leave: This node actively leaves the cluster
2. force-leave: Remove other nodes from the cluster

Let emqx@s2.emqx.io actively exit the cluster:

```bash
$ ./bin/emqx_ctl cluster leave
```

Or remove the emqx@s2.emqx.io node from the cluster on s1.emqx.io:

```bash
$ ./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io
```

## Pseudo-Distributed Cluster

For users who only have one server, the pseudo-distributed starting mode can be used. Please notice that if we want to start two or more nodes on one machine, we must adjust the listening port of the other node to avoid port conflicts.

The basic process is to copy another emqx folder and name it emqx2. After that, we let all the listening ports of the original emqx to be added by an offset as the listening ports of the emqx2 node. For example, we can change the MQTT/TCP listening port from the default 1883 to 2883 as the MQTT/TCP listening port for emqx2. Please refer to [Cluster Script](https://github.com/terry-xiaoyu/one_more_emqx) regarding to the above operations and also refer to [Configuration Instructions](../getting-started/config.md) and  [Configuration Items](../configuration/configuration.md) for details.
