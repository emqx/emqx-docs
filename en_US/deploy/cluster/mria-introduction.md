# Cluster Scalability - Mria

EMQX uses an embedded [Mria](https://github.com/emqx/mria) database to store the following information:

- routing table
- sessions
- configuration
- alarms
- etc.

Mria tables are replicated across all EMQX nodes.
It also helps with the fault-tolerance: the data is safe as long as at least one node in the cluster is alive.

If the size of the EMQX cluster is below 5 nodes, typically, no database scalability tuning is required.

However, for horizontal scalability, it is recommended to split the nodes in the cluster into two groups:

- core nodes
- replicant nodes

## Node roles

*Core nodes* serve as a source of truth for the database: they are connected in a full mesh, and each one of them contains an up-to-date replica of the data.
Core nodes are expected to be more or less static and persistent. That is, autoscaling the core cluster is not recommended.

*Replicant nodes*, on the other hand, offload all operations mutating the tables to the core nodes.
They connect to one of the core nodes and passively replicate the transactions from it.
This means replicant nodes aren't allowed to perform any write operations on their own.
They instead ask a core node to update the data on their behalf.
At the same time, they have a full local copy of the data, so the read access is just as fast.

This approach solves two problems:

- Horizontal scalability (we've tested EMQX cluster with 23 nodes)

- It enables autoscaling of the replicant cluster

Since replicant nodes don't participate in writes, the efficiency of table updates doesn't suffer when more replicants are added to the cluster.
This allows the creation of larger EMQX clusters.

Also, replicant nodes are designed to be ephemeral.
Adding or removing them won't change the data redundancy, so they can be placed in an autoscaling group, thus enabling better DevOps practices.
Note that initial replication of the data from the core nodes is a relatively heavy operation, depending on the data size, so the autoscaling policy must not be very aggressive.

## Configuration

In EMQX 5.0 all nodes assume core role by default, so without tweaks, the cluster behaves like 4.*.

To use the new replication protocol, set `EMQX_NODE__DB_ROLE` environment variable or `node.db_role` setting in `emqx.conf` to `replicant` on some of the nodes in the cluster. This way, they will assume a replicant role. Note that there must be at least one core node in the cluster. We recommend 3 cores + N replicants setup as the starting point.

Core nodes may accept MQTT traffic, or they can disable all MQTT listeners to serve purely as the database server for the replicants, depending on the use case.

- In a small cluster (3 nodes or less in total) it doesn't make sense perfomance-wise to use replicants, so core nodes take all the traffic. This is a easy way to use EMQX.
- In a very large cluster (10 nodes or more) it makes sense to move away traffic from the core nodes.
- In a medium cluster it really depends on many factors, so experimentation is needed.

## Network and hardware

### Network

The network latency between Core nodes is recommended to be below 10ms, and the cluster will not be available if it is higher than 100ms.
Please deploy Core nodes under the same private network.
It is also suggested to deploy under the same private network between Replicant and Core nodes, but the network requirements can be slightly lower.

### CPU/Memory

Core nodes require more memory and have lower CPU consumption without taking over connections. Replicant nodes, consistent with 4.x, can be deployed on a total connection and message throughput basis.

## Exception Handling

Core nodes are transparent to Replicant nodes, and when a Core node is down, Replicant nodes will automatically connect to other Core nodes.
The client is not disconnect at this mount, but may cause a delay in routing updates.

When a Replicant node is down, all clients connected to that node are disconnected. However, because Replicant is stateless, it does not affect the stability of other nodes, at which point the client can connect to another available Replicant node through the reconnect mechanism.

## Monitoring and troubleshooting

The Mria performance can be monitored using Prometheus metrics or Erlang console.

### Prometheus metrics

#### Core
- `emqx_mria_last_intercepted_trans`: Number of transactions received by the shard since the node start. Note that this value can be different on different core nodes.
- `emqx_mria_weight`: A value used for load balancing. It changes depending on the momentary load of the core node.
- `emqx_mria_replicants`: Number of replicants connected to the core node. Numbers are grouped per shard.
- `emqx_mria_server_mql`: Number of unprocessed transactions waiting to be sent to the replicants. Less is better. If this metric grows, then it's probably time to add more computing resources for the exiting core nodes or even add more core nodes.

#### Replicant

- `emqx_mria_lag`: Replicant lag, indicating how far behind the upstream core node the replicant lags. Less is better.
- `emqx_mria_bootstrap_time`: Time spent during bootstrapping of the replicant. This value doesn't change during normal operation of the replicant
- `emqx_mria_bootstrap_num_keys`: Number of database records copied from the core node during bootstrap. This value doesn't change during normal operation of the replicant
- `emqx_mria_message_queue_len`: Message queue length of the replica process. It should be around 0 all the time.
- `emqx_mria_replayq_len`: Length of the internal replay queue on the replicant. Less is better.

### Console commands

`emqx eval 'mria_rlog:status().'` command can be executed to get more extensive information about the state of the embedded database.
