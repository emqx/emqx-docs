# Manual Clustering

EMQX nodes are identified by their names.

A node name consists of two parts, the node name part and the host part, separated with `@`.

The host part must either be the IP address or the FQDN, which has dots,
for example, `myhost.example.domain`.

In this document, we use two nodes to demonstrate manual clustering steps.

Suppose you are going to deploy an EMQX cluster on two servers `s1.emqx.io` and `s2.emqx.io`

| FQDN       |   Node name       |
| ---------- | ----------------- |
| s1.emqx.io |  emqx@s1.emqx.io  |
| s2.emqx.io |  emqx@s2.emqx.io  |

Or if you have static IP assignments for the hosts.

| FQDN         |   Node name       |
| ------------ | ----------------- |
| 192.168.0.10 |  emqx@192.168.0.10  |
| 192.168.0.20 |  emqx@192.168.0.20  |

::: tip Tip
EMQX node names are immutable, as they are baked into the database schema and data files. It is strongly recommended to use static FQDNs for EMQX node names, even when the network environment provides static IPs.
:::

## Configure `emqx@s1.emqx.io` Node

In `etc/emqx.conf`:

```bash
node.name = emqx@s1.emqx.io
# or
node.name = emqx@192.168.0.10
```

You can also override the node name with an environment variable:

```bash
env EMQX_NODE__NAME='emqx@s1.emqx.io' ./bin/emqx start
```

::: tip Warning
After the node joins the cluster, the node name must not be changed.
:::

## Configure `emqx@s2.emqx.io` Node

In `etc/emqx.conf`

```bash
node.name = emqx@s2.emqx.io
# or
node.name = emqx@192.168.0.20
```

## Node Joins the Cluster

After the two nodes are started, execute the following command on `s2.emqx.io`:

```bash
$ ./bin/emqx_ctl cluster join emqx@s1.emqx.io

Join the cluster successfully.
Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```
::: tip Tip
After `s2.emqx.io` joins `s1.emqx.io` to form a cluster,
its local data will be cleared, and the data from node `s1.emqx.io`
will be synchronized over.
:::

EMQX `join` command should be run on a node **outside** the cluster. The argument should be a node **inside** the cluster.

The `join` **must not** be run on the nodes inside the cluster, i.e., we can't "invite" an external node to join.

E.g. if a `s3.emqx.io` is to join the cluster of `s1` and `s2`,
the join command should be executed on `s3` but **NOT** on `s1` or `s2`.

::: tip Warning
Joining another cluster will cause the node to leave any current cluster it may be part of.
:::

Query the cluster status on any node:

```bash
$ ./bin/emqx_ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

## Leaving a Cluster

There are two ways for a node to leave a cluster:

1. `leave` command: Make the command calling node leave the cluster
2. `force-leave` command: Force another node to leave the cluster

Make `emqx@s2.emqx.io` leave a cluster by executing the below command on `s2.emqx.io`:

```bash
$ ./bin/emqx_ctl cluster leave
```

Or force `emqx@s2.emqx.io` to leave cluster by executing the command on `s1.emqx.io`:

```bash
$ ./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io
```

### Start a Cluster on a Single Machine

For users who only have one server, the pseudo-distributed starting mode can be used.
Please note that if we want to start two or more nodes on one machine, we must adjust
the listening port of the other node to avoid port conflicts.

The basic process is to copy another emqx folder and name it emqx2.
After that, we offset all the listening ports of the original emqx relative to those of the emqx2 node.
For example, we can change the MQTT/TCP listening port from the default 1883 to 2883 for emqx2.

See a shell [script](https://github.com/terry-xiaoyu/one_more_emqx) that makes this process.

Refer to [cfg](../../configuration/configuration-manual.md).
