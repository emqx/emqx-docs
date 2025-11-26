# Create a Cluster

## Node Discovery and Autocluster 

EMQX supports Autocluster based on the Ekka library. Ekka is a cluster management library developed for Erlang/OTP applications, supporting Erlang Node Auto-Discovery, Autocluster, Network Partition Autoheal, and Autoclean.

EMQX supports multiple node discovery strategies:

| Strategy | Description      |
| ------ | ----------------- |
| manual | Create a cluster through manual command |
| static | Autocluster of static node list |
| mcast  | Autocluster with UDP multicast mode |
| dns    | Autocluster of DNS A record |
| etcd   | Autocluster through etcd |
| k8s    | Autocluster of Kubernetes service |

### Introduction to cluster management  through manual method
Suppose you are going to deploy an EMQX cluster on two servers of s1.emqx.io, s2.emqx.io:

|                Node name                | Hostname (FQDN) |   IP address   |
| ------------------------------------ | ------------- | ------------ |
| emqx@s1.emqx.io or emqx@192.168.0.10 | s1.emqx.io    | 192.168.0.10 |
| emqx@s2.emqx.io or emqx@192.168.0.20 | s2.emqx.io    | 192.168.0.20 |

::: tip Tip
The format of node name is<Name@Host>, and Host must be an IP address or FQDN(`hostname.domain-name`)
The format of FQDN is `hostname.domain-name`, and it cannot be a single-layer `hostname`. For example, `abc` is invalid, while `abc.local` is valid.
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

#### Start a cluster on single machine

For users who only have one server, the pseudo-distributed starting mode can be used. Please notice that if we want to start two or more nodes on one machine, we must adjust the listening port of the other node to avoid the port conflicts.

The basic process is to copy another emqx folder and name it emqx2. After that, we let all the listening ports of the original emqx to be added by an offset as the listening ports of the emqx2 node. For example, we can change the MQTT/TCP listening port from the default 1883 to 2883 as the MQTT/TCP listening port for emqx2. Please refer to [Cluster Script](https://github.com/terry-xiaoyu/one_more_emqx) regarding to the above operations and also refer to [Configuration Instructions](../getting-started/config.md) and  [Configuration Items](../configuration/configuration.md) for details.


## Firewall settings

### The Node Discovery Ports

If the environment variable WITH_EPMD=1 is set in advance, the epmd (listening port 4369) will be enabled for node discovery when emqx is started, which is called `epmd mode`.

If the environment variable WITH_EPMD is not set, epmd is not enabled when emqx is started, and emqx ekka is used for node discovery, which is also the default method of node discovery  since version 4.0. This is called `ekka mode`.

**epmd mode：**

If there is a firewall between cluster nodes, the firewall needs to open TCP port 4369 for each node, to allow peers query each other's listening port. The firewall should also allow nodes connecting to port in configurable range from `node.dist_listen_min` to `node.dist_listen_max` (inclusive, default is `6369` for both)

**ekka mode（Default mode since version 4.0）：**

In `ekka` mode, the port mapping is conventional, but not dynamic as in `epmd` mode.
The configurations `node.dist_listen_min` and `node.dist_listen_max` take no effect in this case.

If there is a firewall between the cluster nodes, the conventional listening port should be allowed
for nodes to connect each other. See below for port mapping rule in `ekka` mode.

Erlang distribution port mapping rule in `ekka` mode: `ListeningPort = BasePort + Offset`,
where `BasePort` is 4370 (which is not made configurable), and `Offset` is the numeric suffix of the node's name. If the node name does not have a numeric suffix, `Offsset` is 0.

For example, having `node.name = emqx@192.168.0.12` in `emqx.conf` should make the
node listen on port `4370`, and port  `4371` for `emqx1` (or `emqx-1`), and so on.

### The Cluster RPC Port

Each emqx node also listens on a (conventional) port for the RPC channels, which should
also be allowed by the firewall. The port mapping rule is similar to the node discovery
ports in `ekka mode`, but with the `BasePort = 5370`. That is, having
`node.name = emqx@192.168.0.12` in `emqx.conf` should make the node listen on port `5370`,
and port `5371` for `emqx1` (or `emqx-1`), and so on.
