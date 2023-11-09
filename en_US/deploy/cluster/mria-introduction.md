# Architecture

<!--need to add a section about how users can work with a cluster with all nodes as core nodes-->

EMQX 5.0 redesigns the cluster architecture with [Mria](https://github.com/emqx/mria), which significantly improves EMQX's horizontal scalability. The new design supports 100,000,000 MQTT connections with a single cluster. 

<img src="./assets/EMQX_Mria_architecture.png" alt="EMQX Mria" style="zoom: 40%;" />

In this [Mria](https://github.com/emqx/mria), each node assumes one of two roles: Core node or Replicant.
Core nodes serve as a data layer for the database.
Replicant nodes connect to Core nodes and passively replicate data updates from Core nodes. On how core and replicant node works, you can continue to read the [EMQX clustering](../../design/clustering.md).

By default, all nodes assume the Core node role, so the cluster behaves like that in [EMQX 4.x](https://docs.emqx.com/en/enterprise/v4.4/getting-started/cluster.html#node-discovery-and-autocluster), which is recommended for a small cluster with 3 nodes or fewer. The Core + Replicant mode is only recommended if there are more than 3 nodes in the cluster. 

## Enable Core + Replicant Mode

To enable the  Core + Replicant mode, the backend database (`db_backend`) should be set to `rlog`, some nodes should assume the replicant role (`node.db_role`), and the core node (`core_node`) should be specified, as shown below:

```bash
node { 
    ##To set a node as a replicant node
	db_role = replicant
}
cluster {
		## Default setting, suitable for very large backend
		db_backend = rlog	
		##List of core nodes that the replicant will connect to, different nodes can be separated with a comma
		core_nodes = "emqx1@192.168.0.1, emqx2@192.168.0.2 ..."
}
```

## Monitor and Debug

<!-- TODO 后续补充数值类型 Gauge or Counter -->

The Mria performance can be monitored using Prometheus metrics or Erlang console.

### Prometheus Indicators

You can integrate with Prometheus to monitor the cluster operations. On how to integrate with Prometheus, see [Log and observability - Integrate with Prometheus](../../observability/prometheus.md). 

#### Core Nodes

| Indicators                         | Description                                                  |
| ---------------------------------- | ------------------------------------------------------------ |
| `emqx_mria_last_intercepted_trans` | Transactions received by the shard since the node started    |
| `emqx_mria_weight`                 | Instantaneous load of the Core node                          |
| `emqx_mria_replicants`             | Replicant nodes connected to the Core node Numbers are grouped per shard. |
| `emqx_mria_server_mql`             | Pending transactions waiting to be sent to the replicant nodes. Less is optimal. <br />If this indicator shows a growing trend, more Core nodes are needed. |

#### Replicant Nodes

| Indicators                     | Description                                                  |
| ------------------------------ | ------------------------------------------------------------ |
| `emqx_mria_lag`                | Indicate how far the Replicant lags behind the upstream Core node. Less is better. |
| `emqx_mria_bootstrap_time`     | Startup time of the Replica node. This value should remain stable if the system operates normally. |
| `emqx_mria_bootstrap_num_keys` | Number of database records copied from the Core node during startup. This value should remain stable if the system operates normally. |
| `emqx_mria_message_queue_len`  | Queue length during message replication. Should be around 0. |
| `emqx_mria_replayq_len`        | Internal replay queue length on the Replicant nodes. Less is better. |

### Console Commands

You can also monitor the operating status of the cluster with command `emqx eval 'mria_rlog:status().'`  on the Erlang console.

If EMQX cluster is operating normally, you can get a list of status information, for example, the current log level, the number of messages processed, and the number of messages dropped.

<!--Here we need a query statement and the returned message, and can we link this Erlang console to https://www.erlang.org/doc/man/shell.html -->
