# Create and Manage Cluster

You can create an EMQX cluster either manually or automatically. This page will introduce you to both manual and automatic clustering methods and guide you in creating and managing EMQX clusters using these two different approaches.

## Basic Concepts

You can learn about the basic knowledge of the EMQX cluster and how it works in [Cluster](./introduction.md) and [Architecture](./mria-introduction.md). To create a cluster, you also need to be familiar with the following concepts:

### Node Name

EMQX nodes are identified by their names. All nodes are set with a unique node name in the format of `name@host`, where the host must be an IP address or fully qualified domain name (FQDN). For example:

- For EMQX node deployed on server `s1.emqx.io`, the node name should be `emqx@s1.emqx.io`;
- If this server has a static IP (`192.168.0.10`), the node name should be `emqx@192.168.0.10`.

::: tip
EMQX node names are immutable, as they are baked into the database schema and data files. Therefore, it is recommended to use static FQDNs for EMQX node names.
:::

### Node Discovery

Node discovery is a necessary process in creating a cluster, allowing individual EMQX nodes to discover each other and communicate, regardless of their location or IP address. 

### Manual and Auto Clustering

Based on the node discovery strategies, the way to create clusters can be divided into manual clustering and automatic clustering. 

Manual clustering is the method that creates an EMQX cluster by manually specifying which nodes should be part of the cluster. Auto clustering is another method that allows multiple EMQX nodes to form a cluster automatically without manual configuration. Auto clustering simplifies the process of setting up an EMQX cluster and makes it easier to add or remove nodes from the cluster dynamically. EMQX supports auto clustering based on static node list, DNS Record, etcd, and Kubernetes.

The following table shows different node discovery strategies and cluster creation methods supported by EMQX:

| Strategy | Description                               |
| -------- | ----------------------------------------- |
| `manual` | Manually create a cluster with commands   |
| `static` | Autocluster through static node list      |
| `DNS`    | Autocluster through DNS A and SRV records |
| `etcd`   | Autocluster through etcd                  |
| `k8s`    | Autocluster provided by Kubernetes        |

EMQX supports automatic cluster creation based on the [Ekka](https://github.com/emqx/ekka) library. Ekka is a cluster management library developed for Erlang/OTP applications. Except for automatic discovery of Erlang nodes (Service Discovery) and automatic clustering (Autocluster), it also implements functionalities such as automatic healing of network partitions (Network Partition Autoheal) and automatic removal of downed nodes (Autoclean).

You can define the way of clustering by configuring the node discovery strategy in the `emqx.conf` configuration file. The manual clustering is configured by default. 

```bash
cluster {
    ## Options: manual | static | dns | etcd | K8s
    discovery_strategy  =  manual
}
```

## Before You Start

This section provides you with the guidance about how to configure nodes and network environments before creating a cluster.

### Configure Node Names

Before creating a cluster, you need to know how to name the nodes to join the cluster. Suppose you want to create a cluster for 2 nodes deployed in `s1.emqx.io` and `s2.emqx.io` respectively, you can follow the steps below to create the cluster.

Configure the node name in the `emqx.conf` configuration file of the 1st node, for example:

```bash
node.name = emqx@s1.emqx.io

```

You can also override the node name with an environment variable. For example in `docker run` command's `-e` option, or systemd's `emqx.service` file, define the environment variable as below:

```bash
EMQX_NODE__NAME='emqx@s1.emqx.io'
```

Repeat the above step for the other node to join the cluster.

Now you have named 2 nodes to join the cluster, `emqx@s1.emqx.io` and `emqx@s2.emqx.io`. You can create a cluster either manually or automatically.

### Set Node Cookies

For security concerns, you should change the default cookie settings to a Secret cookie in `emqx.conf` on all nodes to join the cluster. All nodes to join the cluster should use the same Secret cookie. For details about the magic cookie used, see [Distributed Erlang - Security](https://www.erlang.org/doc/reference_manual/distributed.html#security).

```
node {
  cookie = "<a Secret cookie>"
}
```

### Configure Network Environment

Ensure that the network connections between nodes are functioning properly. If there is a firewall or security group between nodes, you need to open the ports for internal cluster communication, including:

- **4370**: Erlang distributed transport port
- **5370**: Cluster RPC port, suitable for physical machine environments
- **5369**: Cluster RPC port, suitable for Docker environments

If multiple EMQX nodes are deployed on a single server, each node will use different cluster communication ports. For details on firewall configurations, see [Intra-cluster communication port](./security.md).

## Quick Start

This section demonstrates how to quickly create a cluster in a Docker network using two different clustering methods:

::: tip

If you plan to run EMQX on Docker environments across multiple physical machines and form a cluster, additional setup will be required. Please refer to [Configure Network Environment](#configure-network-environment) to map the necessary cluster communication ports in the container and ensure these ports are open in the firewall.

:::

:::: tabs type:card

::: tab Manual Clustering Example

1. Create a Docker network for node-to-node communication. Containers in the same network can access each other through container names or network aliases:

   ```bash
   docker network create emqx-net
   ```

2. Start the first node and set the node name through environment variables. The default clustering method by EMQX is manual, so no extra settings are needed. Add the node to the Docker network and set a network alias that matches the node host.

   For EMQX Open Source edition:

   ```bash
   docker run -d \
       --name emqx1 \
       -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
       --network emqx-bridge \
       --network-alias node1.emqx.com \
       -p 1883:1883 \
       -p 8083:8083 \
       -p 8084:8084 \
       -p 8883:8883 \
       -p 18083:18083 \
       emqx/emqx:@CE_VERSION@
   ```

   For EMQX Enterprise edition:

   ```bash
   docker run -d \
       --name emqx1 \
       -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
       --network emqx-bridge \
       --network-alias node1.emqx.com \
       -p 1883:1883 \
       -p 8083:8083 \
       -p 8084:8084 \
       -p 8883:8883 \
       -p 18083:18083 \
       emqx/emqx-enterprise:@EE_VERSION@
   ```
   
3. After the first node starts, launch the second node. The new node needs to join the same network as the first node. Since the first node has already occupied ports such as 1883, no port mapping is done here.

   For EMQX Open Source edition:

   ```bash
   docker run -d \
       --name emqx2 \
       -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
       --network emqx-bridge \
       --network-alias node2.emqx.com \
       emqx/emqx:@CE_VERSION@
   ```

   For EMQX Enterprise edition:

   ```bash
   docker run -d \
       --name emqx2 \
       -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
       --network emqx-bridge \
       --network-alias node2.emqx.com \
       emqx/emqx-enterprise:@EE_VERSION@
   ```
   
4. Create a cluster by executing the following command on any node to connect the current node with the other node. For more information on the command, refer to [Manual Clustering](#manual-clustering).

   ```bash
   docker exec -it emqx2 \
       emqx ctl cluster join emqx@node1.emqx.com
   ```

:::

::: tab Automatic Clustering Example (static method)

1. Create a Docker network for node-to-node communication. Containers in the same network can access each other through container names or network aliases:

   ```bash
   docker network create emqx-net
   ```

2. Start the first node, and set the node name and clustering method through environment variables:

   - The `EMQX_NODE_NAME` environment variable is used to set the node name.
   - The `EMQX_CLUSTER__DISCOVERY_STRATEGY` environment variable is used to set the cluster discovery strategy, here using [static clustering](#autocluster-by-static-node-list).
   - The `EMQX_CLUSTER__STATIC__SEEDS` environment variable is used to set the static node list, which should include all node names.

   Also, you need to add the node to the Docker network and set a network alias matching the node host.

   For EMQX Open Source edition:

   ```bash
   docker run -d \
       --name emqx1 \
       -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
       -e "EMQX_CLUSTER__DISCOVERY_STRATEGY=static" \
       -e "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]" \
       --network emqx-bridge \
       --network-alias node1.emqx.com \
       -p 1883:1883 \
       -p 8083:8083 \
       -p 8084:8084 \
       -p 8883:8883 \
       -p 18083:18083 \
       emqx/emqx:@CE_VERSION@
   ```

   For EMQX Enterprise edition:

   ```bash
   docker run -d \
       --name emqx1 \
       -e "EMQX_NODE_NAME=emqx@node1.emqx.com" \
       -e "EMQX_CLUSTER__DISCOVERY_STRATEGY=static" \
       -e "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]" \
       --network emqx-bridge \
       --network-alias node1.emqx.com \
       -p 1883:1883 \
       -p 8083:8083 \
       -p 8084:8084 \
       -p 8883:8883 \
       -p 18083:18083 \
       emqx/emqx-enterprise:@CE_VERSION@
   ```
   
3. After the first node starts, launch the second node. The clustering method and the new node need to join the same network as the first node. Since the first node has already occupied ports such as 1883, no port mapping is done here.

   For EMQX Open Source edition:

   ```bash
   docker run -d \
       --name emqx2 \
       -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
       -e "EMQX_CLUSTER__DISCOVERY_STRATEGY=static" \
       -e "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]" \
       --network emqx-bridge \
       --network-alias node2.emqx.com \
       emqx/emqx:@CE_VERSION@
   ```

   For EMQX Enterprise edition:

   ```bash
   docker run -d \
      --name emqx2 \
      -e "EMQX_NODE_NAME=emqx@node2.emqx.com" \
      -e "EMQX_CLUSTER__DISCOVERY_STRATEGY=static" \
      -e "EMQX_CLUSTER__STATIC__SEEDS=[emqx@node1.emqx.com,emqx@node2.emqx.com]" \
      --network emqx-bridge \
      --network-alias node2.emqx.com \
      emqx/emqx-enterprise:@EE_VERSION@
   ```
   

:::

::::

Execute the `emqx ctl cluster status` command on any node to view the cluster status. If the cluster status is normal, the following information will be output:

```bash
$ docker exec -it emqx1 emqx ctl cluster status
Cluster status: #{running_nodes =>
                    ['emqx@node1.emqx.com','emqx@node2.emqx.com'],
                stopped_nodes => []}
```

Now you have completed a simple cluster creation process. Next, you can modify and deploy according to the instructions in the following sections to select the cluster creation method you need.

## Manual Clustering

This section explains the procedure of creating a cluster manually. During the manual clustering process, you must manually configure each node in the cluster, including setting up network connections between the nodes. Compared to auto clustering, manual clustering allows for finely-tuned configuration of custom network topologies and is very suitable in situations where auto clustering mechanisms are unavailable or inappropriate.

:::tip 

Manual clustering can only be used for core nodes. If you are using a core-replica node deployment architecture, please use auto clustering to manage the cluster. 

:::

Suppose you have two nodes, `emqx@node1.emqx.com` and `emqx@node2.emqx.com`. You can manually create a cluster for them through the following steps:

1. Set the cluster discovery strategy to `manual`:

   ```bash
   cluster {
       ## Options: manual | static | dns | etcd | K8s
       discovery_strategy  =  manual
   }
   ```

2. After starting the two nodes, execute the cluster join command on one of the nodes:

   ```bash
   $ ./bin/emqx ctl cluster join emqx@node1.emqx.com
   
   Join the cluster successfully.
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

   :::tip

   - This command must run on the node to join the cluster, that is, as a **request** rather than **invite**.
   - After `emqx@s2.emqx.io` joins `emqx@s1.emqx.io` to form a cluster, it will clear the local data and synchronize the data in `emqx@s1.emqx.io`.

   - If `emqx@s2.emqx.io`  wants to join another cluster, it must first leave the current cluster. On how to leave the cluster, see  [Leave Cluster](#leave-cluster).

   :::

3. Query the status of the cluster on any node:

   ```bash
   $ ./bin/emqx ctl cluster status
   
   Cluster status: [{running_nodes,['emqx@node1.emqx.com','emqx@node2.emqx.com']}]
   ```

Now you have successfully created a cluster with two nodes, you can read the [Query Cluster Status](#query-cluster-status), [Manage Cluster Nodes](#manage-cluster-nodes), and [Configure Network Protocols](#configure-network-protocols) sections on how to monitor the cluster status and how to manage the cluster.

## Auto Clustering

This section explains how to create a cluster automatically by various auto-clustering methods.  

### Autocluster by Static Node List

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

<!--v5.0.23 e5.0.4 之前仅支持: ["emqx1", "emqx2"]
v5.0.23e5.0.4 之后是都支持-->

Where,

- `discovery_strategy` is the node discovery strategy, set it to `static`.
- `seeds` is an array, where you can add the node to join the cluster, multiple nodes can be separated with `,`.

After all nodes are started, the cluster will be automatically established.

### Autocluster by DNS Records

[DNS](https://tools.ietf.org/html/rfc1034) is short for Domain Name System. When a DNS server receives a domain name query request, it returns the corresponding IP address of that domain name, which is the so-called A (Address) record. DNS allows a domain name to have multiple A records, i.e., multiple IP addresses, thus forming a mapping where one name corresponds to multiple IP addresses. EMQX's DNS auto clustering utilizes this one-to-many mapping to locate all the nodes in the cluster, allowing each independent node to join the cluster.

#### Configure DNS Services

Most public cloud services have DNS services. After assigning a domain name, you only need to add the IP address of each EMQX node to the A record of this domain to finish the configuration. If EMQX is deployed in a private cloud or internal network, you will need to deploy your own DNS system, for example, with software [BIND](https://www.isc.org/bind/).

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

- `discovery_strategy` is the node discovery strategy, set it to `dns`.
- `cluster.dns.name` is a a string, input the localhost.
- `cluster.dns.record_type` is a enum, optional value: `a` or `srv`.

After all nodes are started, the cluster will be automatically established.

### Autocluster Using etcd

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

- `discovery_strategy` is the node discovery strategy, set it to `etcd`.
- `cluster.etcd.server` is the server address, multiple nodes can be separated with `,`.
- `cluster.etcd.prefix` is the etcd key prefix used for EMQX service discovery. <!--not sure if the explanations is enough-->
- `cluster.etcd.node_ttl` is a duration, indicating the expiration time of the etcd key associated with the node, default: `1m`.

After completing the configuration, you can start the EMQX nodes one by one, and use the etcdctl tool to observe the changes on the etcd server:

```bash
$ etcdctl ls /emqxcl/emqxcl --recursive

/emqxcl/emqxcl/nodes
/emqxcl/emqxcl/nodes/emqx@s1.emqx.io
/emqxcl/emqxcl/nodes/emqx@s2.emqx.io
```

The result shows that all nodes are started normally and joined the cluster automatically.

### Autocluster on Kubernetes

The [EMQX Kubernetes Operator](https://docs.emqx.com/en/emqx-operator/latest/) helps you quickly create and manage EMQX clusters on a Kubernetes environment quickly, greatly simplifying the EMQX cluster deployment and management process by turning deployment and management efforts into a low-cost, labeled, repeatable job.

If you want to deploy and manage EMQX by yourself, you can still use Kubernetes API for node discovery and auto clustering. To use this feature,  you need first to create RBAC for the EMQX Pod to allow EMQX to get cluster node information from the Kubernetes APIServer via the endpoints resource. On how to configure, see [Using RBAC Authorization](https://kubernetes.io/docs/reference/access-authn-authz/rbac/).

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

## Manage Cluster

After you create a cluster, you can monitor the cluster status and manage the cluster nodes.

### Query Cluster Status

Run the command below on any cluster node to query the cluster status:

```bash
$ ./bin/emqx ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### Leave Cluster

There are two ways to leave a cluster:

1. Run the `cluster leave` command: This lets the current node leave the cluster. It notifies the other nodes in the cluster and stops participating in cluster operations. The node will complete any ongoing tasks before leaving.
2. Run the `cluster force-leave <node@host>` command: This removes a node from the cluster. The target node will be forcefully removed from the cluster. This command is typically used when a node fails or becomes unresponsive.

For example, in the previously built cluster, if `emqx@s2.emqx.io` wants to leave the cluster, you can run the command below on `emqx@s2.emqx.io`:

```bash
./bin/emqx ctl cluster leave
```

Or run the command below on `emqx@s1.emqx.io` to remove `emqx@s2.emqx.io` from the cluster:

```bash
./bin/emqx ctl cluster force-leave emqx@s2.emqx.io
```

### Configure Network Protocols

After the cluster is created, you can continue to set the network protocols for the nodes. EMQX supports connecting the nodes via TCP or TLS. The connection method can be configured in `emqx.conf`:

To use TCP IPv4 and TCP IPv6, you can set with the `cluster.proto_dist` in `emqx.conf`.

- TCP IPv4: `inet_tcp ` (Default)
- TCP IPv6: `inet6_tcp`

To enable SSL, you first need to set the `cluster.proto_dist` to `inet_tls`, then configure the `ssl_dist.conf` file in the `etc` folder and specify the TLS certificate. For details, see [Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html).

<!--need an example code here-->
