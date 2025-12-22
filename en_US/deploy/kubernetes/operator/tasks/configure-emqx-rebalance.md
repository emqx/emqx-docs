# Rebalance Cluster Load

## Task Target

How to rebalance MQTT connections.

## Why Need Load Rebalancing

Cluster load rebalancing is the act of forcibly migrating client connections and sessions from one set of nodes to another. It will automatically calculate the number of connections that need to be migrated to achieve node balance, and then migrate the corresponding number of connections and sessions from high-load nodes to low-load nodes, thereby achieving load balancing between nodes. This operation is usually required to achieve balance after a new join or a restart of a node.

The value of rebalancing mainly has the following two points:

- **Improve system scalability**: Due to the persistent nature of MQTT connections, connections to the original nodes will not automatically migrate to the new nodes when the cluster scales. To address this, you can use the load rebalancing feature to smoothly transfer connections from overloaded nodes to newly-added ones. This process ensures a more balanced distribution of load across the entire cluster and enhances throughput, response speed, and resource utilization rate.
- **Reduce O&M costs**: For clusters with unevenly distributed loads, where some nodes are overloaded while others remain idle, you can use the load rebalancing feature to automatically adjust the load within the cluster. This helps achieve a more balanced distribution of work and reduces operational and maintenance costs.

For EMQX cluster load rebalancing, please refer to the document: [Rebalancing](../../../cluster/rebalancing.md)

## How to Use Load Rebalancing

The corresponding CRD of the cluster rebalancing in EMQX Operator is `Rebalance`, and its example is as follows:

```yaml
apiVersion: apps.emqx.io/v2beta1
kind: Rebalance
metadata:
   name: rebalance-sample
spec:
   instanceName: emqx-ee
   rebalanceStrategy:
     connEvictRate: 10
     sessEvictRate: 10
     waitTakeover: 10
     waitHealthCheck: 10
     absConnThreshold: 100
     absSessThreshold: 100
     relConnThreshold: "1.1"
     relSessThreshold: "1.1"
```

> For Rebalance configuration, please refer to the document: [Rebalance reference](../reference/v2beta1-reference.md#rebalancestrategy).

## Test Load Rebalancing

### Cluster Load Distribution Before Rebalancing

Before rebalancing, we intentionally created an EMQX cluster with an uneven distribution of connections. We then used Grafana and Prometheus to monitor the cluster load:

![](./assets/configure-emqx-rebalance/before-rebalance.png)

As shown in the graph, the cluster consists of four EMQX nodes. Three nodes each handle 10,000 connections, while one node has **zero** connections.

In the following example, we demonstrate how to perform a rebalancing operation to evenly distribute the load across all four nodes.

#### Submit a Rebalance Task

Create a `Rebalance` resource to initiate the rebalancing process:

```yaml
apiVersion: apps.emqx.io/v1beta4
kind: Rebalance
metadata:
   name: rebalance-sample
spec:
   instanceName: emqx-ee
   instanceKind: EmqxEnterprise
   rebalanceStrategy:
     connEvictRate: 10
     sessEvictRate: 10
     waitTakeover: 10
     waitHealthCheck: 10
     absConnThreshold: 100
     absSessThreshold: 100
     relConnThreshold: "1.1"
     relSessThreshold: "1.1"
```

Save the file as `rebalance.yaml`, and execute the following command to submit the Rebalance task:

```bash
$ kubectl apply -f rebalance.yaml
rebalance.apps.emqx.io/rebalance-sample created
```

#### Check the Rebalance Progress

Execute the following command to inspect the rebalancing status of the EMQX cluster:

```bash
$ kubectl get rebalances rebalance-sample -o json | jq '.status.rebalanceStates'
{
     "state": "wait_health_check",
     "session_eviction_rate": 10,
     "recipients":[
         "emqx-ee@emqx-ee-3.emqx-ee-headless.default.svc.cluster.local",
     ],
     "node": "emqx-ee@emqx-ee-0.emqx-ee-headless.default.svc.cluster.local",
     "donors":[
         "emqx-ee@emqx-ee-0.emqx-ee-headless.default.svc.cluster.local",
         "emqx-ee@emqx-ee-1.emqx-ee-headless.default.svc.cluster.local",
         "emqx-ee@emqx-ee-2.emqx-ee-headless.default.svc.cluster.local"
     ],
     "coordinator_node": "emqx-ee@emqx-ee-0.emqx-ee-headless.default.svc.cluster.local",
     "connection_eviction_rate": 10
}
```
> For a detailed description of the `rebalanceStates` field, refer to the documentation: [rebalanceStates reference](../reference/v2beta1-reference.md#rebalancestate).

#### Wait for Completion

Monitor the task until its status becomes `Completed`:

```bash
$ kubectl get rebalances rebalance-sample
NAME               STATUS      AGE
rebalance-sample   Completed   62s
```

> The `STATUS` field indicates the lifecycle state of the Rebalance task:
>
> | Status         | Meaning                                       |
> | -------------- | --------------------------------------------- |
> | **Processing** | Rebalancing is in progress.                   |
> | **Completed**  | Rebalancing has successfully finished.        |
> | **Failed**     | Rebalancing encountered an error and stopped. |

### Cluster Load Distribution After Rebalancing

![](./assets/configure-emqx-rebalance/after-rebalance.png)

The figure above shows the cluster load after Rebalance has completed. As illustrated, the migration of client connections is smooth and stable throughout the entire operation. The total number of connections in the cluster remains **10,000**, the same as before rebalancing.

Before rebalancing, one node carried **0** connections while three nodes carried **10,000** connections each. After rebalancing, the connections have been redistributed evenly across all four nodes. The load on each node stabilizes around **2,500** connections and remains consistent.

To determine whether the cluster has reached a balanced state, the EMQX Operator evaluates the following conditions:

```
avg(source node connection number) < avg(target node connection number) + abs_conn_threshold
or
avg(source node connection number) < avg(target node connection number) * rel_conn_threshold
```

Using the configured Rebalance thresholds and real connection counts:

- Source node average: `avg(2553 + 2553 + 2554) ≈ 2553`
- Target node average: `2340`
- Condition checked: `2553 < 2340 * 1.1`

Since the condition holds true, the Operator concludes that the cluster has reached a balanced state and the rebalancing task has successfully completed.
