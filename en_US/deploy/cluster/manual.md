# Manual Clustering

EMQX supports creating clusters manually and automatically. This chapter will guide you through creating and managing EMQX clusters manually.

:::tip Prerequisites:

- Knowledge of [Distributed Clusters](./introduction.md).
- Knowledge of [Architecture and deployment prerequisites](./mria-introduction.md).
  :::

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

Before manually creating a cluster, let's first get familiar with the format of node name in EMQX. EMQX nodes are identified by their names. A node name consists of two parts, node name and host, separated with `@`, for example, `emqx@s1.emqx.io`. The host part must either be the IP address or the FQDN (`myhost.example.domain`), for example:

- For EMQX node deployed on server `s1.emqx.io`, the node name should be `emqx@s1.emqx.io`; 
- If this server has a static IP (`192.168.0.10`), the node name should be `emqx@192.168.0.10`. 

::: tip Tip
EMQX node names are immutable, as they are baked into the database schema and data files. Therefore, it is recommended to use static FQDNs for EMQX node names.
:::

To manually create a cluster, you need to configure each node in the cluster, including setting up network connections among them. Suppose you want to create a cluster for 2 nodes deployed in `s1.emqx.io` and `s2.emqx.io` respectively, you can follow the steps below to create the cluster. 

## Configure node names

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

Now you have named 2 nodes to join the cluster, `emqx@s1.emqx.io` and `emqx2@s1.emqx.io`

## Let a node join a cluster

After the two nodes are started, run the `cluster join` command on the node that you want to join the cluster. For example, you want `emqx@s2.emqx.io` to join  `emqx@s1.emqx.io`, run the command below on `emqx@s2.emqx.io`：

```bash
$ ./bin/emqx_ctl cluster join emqx@s1.emqx.io

Join the cluster successfully.
Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```
:::tip

1. This command must be run on the node to join the cluster, that is, as a **request** rather than **invite**.

2. After `emqx@s2.emqx.io` joins `emqx@s1.emqx.io` to form a cluster, it will clear the local data and synchronize the data in `emqx@s1.emqx.io`.

3. If `emqx@s2.emqx.io`  wants to join another cluster, it must first leave the current cluster. 

   :::

## Query cluster running status

Run the command below on any cluster node to query the cluster status:

```bash
$ ./bin/emqx_ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

## Leave a cluster/Remove a node

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

## Configure network protocols

Each Erlang node can be connected via TCP or TLS, and the connection method can be configured in `emqx.conf`:

To use TCP IPv4 and TCP IPv6s, you can set with the `cluster.proto_dist` in `emqx.conf`. 

- TCP IPv4: `inet_tcp ` (Default)
- TCP IPv6: `inet6_tcp`

To enable SSL, you first need to set the `cluster.proto_dist` to `inet_tls`, then configure the `ssl_dist.conf` file in the `etc` folder and specify the TLS certificate. For details, see [Using TLS for Erlang Distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html). 

<!--need some code example here-->
