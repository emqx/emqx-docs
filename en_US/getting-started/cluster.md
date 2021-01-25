# Create a cluster

## Node Discovery and Autocluster 
EMQ X supports Autocluster based on the Ekka library. Ekka is a cluster management library developed for Erlang/OTP applications, supporting Erlang Node Auto-Discovery, Autocluster, Network Partition Autoheal and Autoclean.

EMQ X supports multiple node discovery strategies:

| Strategy | Description      |
| ------ | ----------------- |
| manual | Create a cluster through manual command |
| static | Autocluster of static node list |
| mcast  | Autocluster with UDP multicast mode |
| dns    | Autocluster of DNS A record |
| etcd   | Autocluster through etcd |
| k8s    | Autocluster of Kubernetes service |

### Introduction to cluster management  through manual method
Suppose you are going to deploy an EMQ X cluster on two servers of s1.emqx.io, s2.emqx.io:

|                Node name                | Hostname (FQDN) |   IP address   |
| ------------------------------------ | ------------- | ------------ |
| emqx@s1.emqx.io or emqx@192.168.0.10 | s1.emqx.io    | 192.168.0.10 |
| emqx@s2.emqx.io or emqx@192.168.0.20 | s2.emqx.io    | 192.168.0.20 |

::: tip Tip
The format of node name  is <Name@Host>, and Host must be an IP address or FQDN (host name. domain name)
:::

### Configure emqx@s1.emqx.io node

emqx/etc/emqx.conf:

```bash
node.name = emqx@s1.emqx.io
# or
node.name = emqx@192.168.0.10
```

You can also configure through environment variables:

```bash
export EMQX_NODE_NAME=emqx@s1.emqx.io && ./bin/emqx start
```

::: tip Tip
After the node joins the cluster, the node name cannot be changed.
:::

### Configure emqx@s2.emqx.io Node

emqx/etc/emqx.conf:

```bash
node.name = emqx@s2.emqx.io
# or
node.name = emqx@192.168.0.20
```

### Node joins the cluster

After starting the two nodes, execute the following on s2.emqx.io:

```bash
$ ./bin/emqx_ctl cluster join emqx@s1.emqx.io

Join the cluster successfully.
Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```
::: tip Tip
After s2.emqx.io joins the cluster, all its data will be cleared and the data of the s1.emqx.io node will be synchronized. If there are still s3.emqx.io nodes, you need to execute commands on the s3.emqx.io node to join emqx@s1.emqx.io or emqx@s2.emqx.io, and nodes already existing in the cluster cannot join other nodes. Otherwise, it will exit the current cluster and form a new cluster with the new joined node.
:::


Query the cluster status on any node:

```bash
$ ./bin/emqx_ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

### Exit the cluster

There are two ways for a node to exit the cluster:

1. leave: Make this node leave the cluster
2. force-leave: delete other nodes from the cluster

Make emqx@s2.emqx.io actively exit the cluster:

```bash
$ ./bin/emqx_ctl cluster leave
```

Or delete the emqx@s2.emqx.io node from the cluster on s1.emqx.io:

```bash
$ ./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io
```


## Firewall settings 
If the environment variable WITH_EPMD=1 is set in advance, the epmd (listening port 4369) will be enabled for node discovery when emqx is started, which is called `epmd mode`.
If the environment variable WITH_EPMD is not set, epmd is not enabled when emqx is started, and emqx ekka is used for node discovery, which is also the default method of node discovery  after version 4.0. This is called `ekka mode`.

**epmd mode：**

If there is a firewall between cluster nodes, the firewall needs to open TCP port of 4369 and a TCP port segment. 4369 is used by the epmd port for mapping service. The TCP port segment is used to establish connection and communication between nodes.

After setting up the firewall, you need to configure the same port segment in `emqx/etc/emqx.conf` :

```bash
## Distributed node port range
node.dist_listen_min = 6369
node.dist_listen_max = 7369
```

**ekka mode（Default mode after version 4.0）：**

If there is a firewall between the cluster nodes, only the TCP 4370 port needs to be opened by default.

However, if the node name specified by the node.name configuration has a numeric suffix (Offset), you need to open the 4370 + Offset port.

For example:

```
node.name = emqx-1@192.168.0.12
```

You need to open port 4371.
