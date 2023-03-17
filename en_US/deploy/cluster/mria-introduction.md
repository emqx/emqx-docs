# Architecture

EMQX 5.0 redesigns the cluster architecture with [Mria](https://github.com/emqx/mria) + RLOG, which significantly improves EMQX's horizontal scalability and is also the key behind 100M MQTT connection support with a single cluster. <!--need to add a section about how users can work with a cluster with all nodes as core nodes-->

<img src="./assets/EMQX_Mria_architecture.png" alt="EMQX Mria" style="zoom: 18%;" />



In this [Mria](https://github.com/emqx/mria) + RLOG mode, each node assumes one of two roles: Core node or Replicant node. Core nodes serve as a data layer for the database. Replicant nodes connect to Core nodes and passively replicate data updates from Core nodes. 

By default, all nodes assume the Core node role, so the cluster behaves like that in [EMQX 4.x](https://docs.emqx.com/en/enterprise/v4.4/getting-started/cluster.html#node-discovery-and-autocluster), which is recommended for a small cluster with 3 nodes or fewer. The Core + Replicant mode is only recommended if there are more than 3 nodes in the cluster. 

## Enable Core + Replicant Mode 

To enable the  Core + Replicant mode, the backend database (`db_backend`) should be set to `rlog`, some nodes should assume the replicant role (`node.db_role`), and the core node (`core_node`) should be specified, as shown below:

```bash
cluster {
		## Default setting, suitable for very large backend
		db_backend = rlog 
		##To set a node as a replicant node
		node.db_role = replicant 
		##List of core nodes that the replicant will connect to, different nodes can be seperated with a comma 
		core_node = [node1, node2, ...] 
}
```

<!--Configure with environment variables should also be added-->

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
| `emqx_mria_server_mql`             | Pending transactions waiting to be sent to the replicant nodes. Less is optimal. <br>If this indicator shows a growing trend, more Core nodes are needed. |

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

## Pseudo-Distributed Cluster 

EMQX also provides a pseudo-distributed cluster feature for testing and development purposes. It refers to a cluster setup where multiple instances of EMQX are running on a single machine, with each instance configured as a node in the cluster. 

After starting the first node, use the following command to start the second node and join the cluster manually. To avoid port conflicts, we need to adjust some listening ports:

```bash
EMQX_NODE__NAME='emqx2@127.0.0.1' \
    EMQX_STATSD__SERVER='127.0.0.1:8124' \
    EMQX_LISTENERS__TCP__DEFAULT__BIND='0.0.0.0:1882' \
    EMQX_LISTENERS__SSL__DEFAULT__BIND='0.0.0.0:8882' \
    EMQX_LISTENERS__WS__DEFAULT__BIND='0.0.0.0:8082' \
    EMQX_LISTENERS__WSS__DEFAULT__BIND='0.0.0.0:8085' \
    EMQX_DASHBOARD__LISTENERS__HTTP__BIND='0.0.0.0:18082' \
    EMQX_NODE__DATA_DIR="./data2" \
./bin/emqx start

./bin/emqx_ctl cluster join emqx1@127.0.0.1
```

The above code example is to create a cluster manually, you can also refer to the [auto clustering](#auto-clustering) section on how to create a cluster automatically. 

<!--to add a quickstart with the pseudo-distributed cluster @WIVWIV -->
