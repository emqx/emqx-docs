# 重新平衡集群负载

## 目标

如何重新平衡 MQTT 连接。

## 为什么需要负载重平衡

集群负载重平衡是将客户端连接和会话从一组节点强制迁移到另一组节点的操作。它将自动计算需要迁移的连接数量以实现节点平衡，然后将相应数量的连接和会话从高负载节点迁移到低负载节点，从而在节点之间实现负载均衡。通常在新节点加入或节点重启后需要此操作来实现平衡。

重平衡的价值主要有以下两点：

- **提高系统可扩展性**：由于 MQTT 连接的持久性，当集群扩展时，对原始节点的连接不会自动迁移到新节点。为了解决这个问题，您可以使用负载重平衡功能将连接从过载节点平滑转移到新添加的节点。此过程确保整个集群的负载分布更加均衡，并提高吞吐量、响应速度和资源利用率。
- **降低运维成本**：对于负载分布不均的集群，某些节点过载而其他节点保持空闲，您可以使用负载重平衡功能自动调整集群内的负载。这有助于实现更均衡的工作分布并降低运维成本。

有关 EMQX 集群负载重平衡，请参阅文档：[重平衡](../../../cluster/rebalancing.md)

## 如何使用负载重平衡

EMQX Operator 中集群重平衡对应的 CRD 是 `Rebalance`，示例如下：

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

> 有关 Rebalance 配置，请参阅文档：[Rebalance reference](../reference/v2beta1-reference.md#rebalancestrategy)。

## 测试负载重平衡

### 重平衡前的集群负载分布

在重平衡之前，我们有意创建了一个连接分布不均的 EMQX 集群。然后使用 Grafana 和 Prometheus 监控集群负载：

![](./assets/configure-emqx-rebalance/before-rebalance.png)

如图所示，集群由四个 EMQX 节点组成。三个节点各自处理 10,000 个连接，而一个节点的连接数为 **零**。

在以下示例中，我们演示如何执行重平衡操作，以在所有四个节点之间均匀分配负载。

#### 提交 Rebalance 任务

创建 `Rebalance` 资源以启动重平衡过程：

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

将文件保存为 `rebalance.yaml`，并执行以下命令提交 Rebalance 任务：

```bash
$ kubectl apply -f rebalance.yaml
rebalance.apps.emqx.io/rebalance-sample created
```

#### 检查重平衡进度

执行以下命令检查 EMQX 集群的重平衡状态：

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

> 有关 `rebalanceStates` 字段的详细描述，请参阅文档：[rebalanceStates reference](../reference/v2beta1-reference.md#rebalancestate)。

#### 等待完成

监控任务直到其状态变为 `Completed`：

```bash
$ kubectl get rebalances rebalance-sample
NAME               STATUS      AGE
rebalance-sample   Completed   62s
```

> `STATUS` 字段表示 Rebalance 任务的生命周期状态：
>
> | 状态         | 含义                                       |
> | -------------- | --------------------------------------------- |
> | **Processing** | 重平衡正在进行中。                   |
> | **Completed**  | 重平衡已成功完成。        |
> | **Failed**     | 重平衡遇到错误并停止。 |

### 重平衡后的集群负载分布

![](./assets/configure-emqx-rebalance/after-rebalance.png)

上图显示了 Rebalance 完成后的集群负载。如图所示，在整个操作过程中，客户端连接的迁移是平滑且稳定的。集群中的连接总数仍为 **10,000**，与重平衡前相同。

在重平衡之前，一个节点承载 **0** 个连接，而三个节点各自承载 **10,000** 个连接。重平衡后，连接已重新分配到所有四个节点。每个节点上的负载稳定在 **2,500** 个连接左右并保持一致。

要确定集群是否已达到平衡状态，EMQX Operator 评估以下条件：

```
avg(源节点连接数) < avg(目标节点连接数) + abs_conn_threshold
或
avg(源节点连接数) < avg(目标节点连接数) * rel_conn_threshold
```

使用配置的 Rebalance 阈值和实际连接数：

- 源节点平均值：`avg(2553 + 2553 + 2554) ≈ 2553`
- 目标节点平均值：`2340`
- 检查的条件：`2553 < 2340 * 1.1`

由于条件成立，Operator 得出结论：集群已达到平衡状态，重平衡任务已成功完成。
