# Mmaual Clustering

EMQX nodes are identified by their names.

A node name consists of two parts, node part and host partk, separated with `@`.

The host part must either be the IP address or the FQDN which has dots,
for example `myhost.example.domaon`.

In this document, we use two nodes to demonstrate manual clustering steps.

Suppose you are going to deploy an EMQX cluster on two servers `s1.emqx.io` and `s2.emqx.io`

| FQDN       |   Node name       |
| ---------- | ----------------- |
| s1.emqx.io |  emqx@s1.emqx.io  |
| s2.emqx.io |  emqx@s2.emqx.io  |

Or if you have rather static IP assignments for the hosts.

| FQDN         |   Node name       |
| ------------ | ----------------- |
| 192.168.0.10 |  emqx@s1.emqx.io  |
| 192.168.0.20 |  emqx@s2.emqx.io  |

::: tip Tip
EMQX node names are immutable as it is baked into the database
and data files. It is recommended to use static FQDNs for EMQX node names,
especially when the network environment does provide static IPs.
:::

## Configure emqx@s1.emqx.io node

In `etc/emqx.conf`:

```bash
node.name = emqx@s1.emqx.io
# or
node.name = emqx@192.168.0.10
```

You can also override node name with environment variable:

```bash
env EMQX_NODE__NAME='emqx@s1.emqx.io' ./bin/emqx start
```

::: tip Tip
After the node joins the cluster, the node name cannot be changed.
:::

## Configure emqx@s2.emqx.io Node

In `etc/emqx.conf`

```bash
node.name = emqx@s2.emqx.io
# or
node.name = emqx@192.168.0.20
```

## Node joins the cluster

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

EMQX clustering is for an outsider node to send join **request**
to one of the nodes in the cluster to join.

But **NOT** for the nodes in the cluster to invite an outsider node
to join.

e.g. if a `s3.emqx.io` is to join the clsuter of `s1` and `s2`,
the join command should be executed on `s3` but **NOT** on `s1` or `s2`.

::: warning Warning
Joinning another cluster will cause the node to leave the current.
:::

Query the cluster status on any node:

```bash
$ ./bin/emqx_ctl cluster status

Cluster status: [{running_nodes,['emqx@s1.emqx.io','emqx@s2.emqx.io']}]
```

## Leaving a cluster

There are two ways for a node to leave a cluster:

1. leave: Make self node leave the cluster
2. force-leave: Force another node to leave the cluster

Make `emqx@s2.emqx.io` leave cluster by executing below command on `s2.emqx.io`:

```bash
$ ./bin/emqx_ctl cluster leave
```

Or force `emqx@s2.emqx.io` to leave cluster by exeucting the command on `s1.emqx.io`:

```bash
$ ./bin/emqx_ctl cluster force-leave emqx@s2.emqx.io
```

### Start a cluster on single machine

For users who only have one server, the pseudo-distributed starting mode can be used.
Please notice that if we want to start two or more nodes on one machine, we must adjust
the listening port of the other node to avoid the port conflicts.

The basic process is to copy another emqx folder and name it emqx2.
After that, we let all the listening ports of the original emqx to be added by an offset
as the listening ports of the emqx2 node.
For example, we can change the MQTT/TCP listening port from the default 1883 to 2883 as
the MQTT/TCP listening port for emqx2.
