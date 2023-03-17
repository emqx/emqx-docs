# Create an EMQX Cluster

EMQX supports creating clusters manually and automatically. There are 2 key concepts before creating a cluster, node names and node discovery. 

**Node Names**

Before starting the cluster creation step, let's first get familiar with the concept of node names in EMQX. EMQX nodes are identified by their names. A node name consists of two parts, node name and host, separated with `@`, for example, `emqx@s1.emqx.io`. The host part must either be the IP address or FQDN (`myhost.example.domain`), for example:

- For EMQX node deployed on server `s1.emqx.io`, the node name should be `emqx@s1.emqx.io`; 
- If this server has a static IP (`192.168.0.10`), the node name should be `emqx@192.168.0.10`. 

::: tip Tip
EMQX node names are immutable, as they are baked into the database schema and data files. Therefore, it is recommended to use static FQDNs for EMQX node names.
:::

**Node Discovery**

EMQX's clustering feature is based on the [Ekka](https://github.com/emqx/ekka) library, a cluster management library developed for Erlang/OTP applications.

One of the crucial steps for EMQX clustering is node discovery, which enables individual EMQ X nodes with different IP addresses or locations to discover and communicate with each other. EMQX supports multiple node discovery strategies:

| Strategy | Description                               |
| -------- | ----------------------------------------- |
| `manual` | Create a cluster through manual command   |
| `static` | Create a cluster using a static node list |
| `mcast`* | Create a cluster through UDP multicast    |
| `dns`    | Create a cluster using DNS A records      |
| `etcd`   | Create a cluster via etcd                 |
| `k8s`    | Create a cluster via Kubernetes service   |

[^*]: The multicast discovery strategy has been deprecated and will be removed in future releases.

## Before You Begin

Before creating an EMQX cluster, the following prerequisites should first be met:

1. All nodes are set with a unique node name in the format of `name@host`, where host must be an IP address or fully qualified domain name (FQDN). For more information on configuring node names, see [Configure Node Names](#configure-node-names). 
2. If there is a firewall or security group between nodes, ensure the cluster communication port has been opened. For details, see [Intra-cluster communication port](./security.md).
3. All nodes use the same security cookie. For details about the magic cookie used, see [Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security). 

:::

Now you can begin your journey to create an EMQX cluster. 

## Configure Node Names

Before creating a cluster, you need first to name the nodes to join the cluster. Suppose you want to create a cluster for 2 nodes deployed in `s1.emqx.io` and `s2.emqx.io` respectively, you can follow the steps below to create the cluster. 

Configure the node name in the `emqx.conf` configuration file of the 1st node, for example:

```bash
node.name = emqx@s1.emqx.io
# or
node.name = emqx@192.168.0.10
```

You can also override the node name with an environment variable:

```bash
env EMQX_NODE__NAME='emqx@s1.emqx.io' ./bin/emqx start
```

Repeat the above step for the other node to join the cluster. 

Now you have named 2 nodes to join the cluster, `emqx@s1.emqx.io` and `emqx@s2.emqx.io`. You can create a cluster either manually or automatically. 

## Manual Clustering

### Set Node Discovery Strategy

EMQX supports creating clusters manually and automatically. This section will introduce the manual clustering feature in EMQX. 

Set node discovery strategy

Manual clustering is the method to configure an EMQX cluster by manually specifying which nodes should be part of the cluster. By default, EMQX adopts a manual clustering strategy, which can also be set in `emqx.conf`:

```bash
cluster {
    ## options: manual | static | dns | etcd | K8s
    discovery_strategy  =  manual
}
```

This approach provides users with greater control and flexibility over the cluster configuration and more security as you can restrict which nodes can join the cluster. 

:::tip
Manual clustering is not supported in the Core-Replica architecture.
:::

### Configure Nodes to Join a Cluster

After the nodes are started,  you can run the `cluster join` command on the node that you want to join the cluster. For example, you want `emqx@s2.emqx.io` to join  `emqx@s1.emqx.io`, run the command below on `emqx@s2.emqx.io`：

<!--should we add how to start the nodes or how to confirm the nodes are started?-->

```bash
$ ./bin/emqx_ctl cluster join emqx@s1.emqx.io

Join the cluster successfully.
Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

:::tip

1. This command must run on the node to join the cluster, that is, as a **request** rather than **invite**.

2. After `emqx@s2.emqx.io` joins `emqx@s1.emqx.io` to form a cluster, it will clear the local data and synchronize the data in `emqx@s1.emqx.io`.

3. If `emqx@s2.emqx.io`  wants to join another cluster, it must first leave the current cluster. 

   :::

Now you have successfully created a cluster with two nodes, you can read the [Query Cluster Status](#query-cluster-status), [Manage Cluster Nodes](#manage-cluster-nodes), and [Configure Network Protocols](#configure-network-protocols) sections on how to monitor the cluster status and how to manage the cluster. 

## Auto Clustering

Auto clustering in EMQX is another feature that allows multiple EMQX nodes to form a cluster automatically without manual configuration. Auto clustering simplifies the process of setting up an EMQX cluster and makes it easier to add or remove nodes from the cluster dynamically. It also provides fault tolerance and high availability by allowing the cluster to continue operating even if one or more nodes fail.

EMQX supports auto clustering based on static node list, DNS Record, etcd, and Kubernetes. Continue to read to learn how to work with these features. 

## Autocluster by Static Node List

In EMQX, autocluster by static node list is to use a pre-defined static node list on each node to join the cluster. After starting, the nodes will create a cluster automatically according to the node list.

Static clustering is the easiest way to create an EMQX cluster automatically with no dependencies on other network components or services. As long as each node can communicate with each other through the TCP protocol, they can form an EMQX cluster.

To enable this feature, configure the cluster mode and node list in `emqx.conf`:

**Example code:**

```bash
cluster {
    discovery_strategy = static
    static {
        seeds = ["emqx@s1.emqx.io", "emqx@s2.emqx.io"]
    }
}
```

Where, 

- `discovery_strategy` is the node discovery strategy, set it to `static`
- `seeds` is an array, where you can add the node to join the cluster, multiple nodes can be separated with `,`

After all nodes are started, the cluster will be automatically established.

## Autocluster by DNS Records

EMQX supports auto clustering by DNS A records and DNS SRV records. Domain Name System (DNS) record is a type of record used to map domain names to IP addresses. Because multiple DNS records are allowed for one domain name, EMQX leverages this feature to implement auto clustering. <!--Do we need to add more explanations here?-->

#### Configure DNS Services

Most public cloud services have DNS services. After assigning a domain name, you only need to add the IP address of each EMQX node to the A record of this domain to finish the configuration.

:::tip

If EMQX is deployed in a private cloud or internal network, you will need to deploy your own DNS system, for example, with software [BIND](https://www.isc.org/bind/).

:::

### Configure Autocluster by DNS Records

After the DNS service is ready, you can add all nodes to join the cluster in `emqx.conf` with the `cluster.dns` configuration item:

**Example code:**

```bash
cluster {
    discovery_strategy = dns
    dns {
        name = "localhost"
        ## support DNS A record and DNS SRV record 
        record_type = a
    }
}
```

Where, 

- `discovery_strategy` is the node discovery strategy, set it to `dns`
- `cluster.dns.name` is a a string, input the localhost
-  `cluster.dns.record_type` is a enum, optional value: `a` or `srv`

After all nodes are started, the cluster will be automatically established.

## Autocluster Using etcd

[etcd](https://etcd.io/) is an open-source project initiated by CoreOS. It is widely used in distributed systems for service discovery and connection establishing, which is exactly what EMQX auto clustering needs.

After you deploy an etcd server (cluster) in your network, EMQX can automatically create the cluster via etcd. For how to install and configure etcd, see [etcd Install](https://etcd.io/docs/latest/install/).

To enable autocluster using etcd, you can work with the `cluster.etcd` configuration items in `emqx.conf`. 

**Example code:**

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

Where:

- `discovery_strategy` is the node discovery strategy, set it to `etcd`
- `cluster.etcd.server` is the server address, multiple nodes can be separated with `,`
- `cluster.etcd.prefix` is the etcd key prefix used for EMQX service discovery <!--not sure if the explanations is enough-->
-  `cluster.etcd.node_ttl` is a duration, indicating the expiration time of the etcd key associated with the node, default: `1m`

After completing the configuration, you can start the EMQX nodes one by one, and use the etcdctl tool to observe the changes on the etcd server:

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

The result shows that all nodes are started normally and joined the cluster automatically.

## Autocluster on Kubernetes

[Kubernetes (K8s)](https://kubernetes.io) is Google's open-source container management system. EMQX can use kubernetes API for node discovery and auto clustering.

To enable EMQX autocluster on Kubernetes, you can work with the `cluster.k8s` configuration item in `emqx.conf`. 

```bash
cluster {
    discovery_strategy = K8s
    K8s {
        apiserver = "http://10.110.111.204:8080"
        service_name = emqx
        address_type = ip
        namespace = default
    }
}
```

Where:

- `discovery_strategy` is the node discovery strategy, set it to `K8s`
- `cluster.K8s.apiserver` is the Kubernetes API endpoint URL, default: `http://10.110.111.204:8080`
- `cluster.K8s.service_name` is the EMQX service name, default: `emqx`
- `cluster.K8s.address_type` is the address type to connect the discovered nodes, default: `ip`, optional values: `ip`, `dns`, `hostname`
- [optional] `cluster.K8s.suffix` is the node name suffix, only needed when `cluster.K8s.address_type` is set to `dns`,  default: `pod.local`
- `cluster.K8s.namespace` is the Kubernetes namespace, it is a string object, default: `default`

Start all nodes one by one after the configuration, and the cluster will be automatically established.

::: tip

When working EMQX autocluster on Kubernetes, [Calico](https://kubernetes.io/docs/tasks/administer-cluster/network-policy-provider/calico-network-policy/) rather than Fannel plugin is recommended. 
:::

After the cluster is succefully created, you can refer to the section below on how to monitor the cluster status and how to manage cluster nodes. 

## Query Cluster Status

Run the command below on any cluster node to query the cluster status:

```bash
$ ./bin/emqx_ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

## Manage Cluster Nodes

You can remove a node from a cluster with `cluster leave` or ``cluster force-leave``:

When an EMQX node issues the `cluster leave` command, it notifies the other nodes in the cluster that it intends to leave, and it stops participating in cluster operations, it will complete any ongoing tasks before leaving.

When an EMQX node issues the `cluster force-leave` command, a node will be forcefully removed from a cluster. This command is typically used when a node fails or becomes unresponsive. 

For example, in the previously built cluster, if `emqx@s2.emqx.io` wants to leave the cluster, you can run the command below on `emqx@s2.emqx.io`:

```bash
./bin/emqx_ctl cluster leave
```

Or run the command below on `emqx@s1.emqx.io` to remove `emqx@s2.emqx.io` from the cluster:

```bash
./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io
```

After the cluster is created, you can continue to set the network protocols for the nodes. EMQX supports connecting the nodes via TCP or TLS.

## Configure Network Protocols

Each Erlang node can be connected via TCP or TLS, and the connection method can be configured in `emqx.conf`:

To use TCP IPv4 and TCP IPv6, you can set with the `cluster.proto_dist` in `emqx.conf`. 

- TCP IPv4: `inet_tcp ` (Default)
- TCP IPv6: `inet6_tcp`

To enable SSL, you first need to set the `cluster.proto_dist` to `inet_tls`, then configure the `ssl_dist.conf` file in the `etc` folder and specify the TLS certificate. For details, see [Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html). 

<!--need an example code here-->
