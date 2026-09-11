# 重新平衡集群负载

::: warning
EMQX Operator 3.0.0 不支持 `Rebalance` CRD。本页示例仅适用于安装了 `Rebalance` 的早期 EMQX Operator 版本。
:::

## 任务目标

了解如何重新平衡 MQTT 连接。

## 为什么需要负载重平衡

集群负载重平衡是将客户端连接和会话从一组节点强制迁移到另一组节点的操作。系统会自动计算实现节点平衡所需迁移的连接数，再将相应数量的连接和会话从高负载节点迁移到低负载节点，从而均衡节点间的负载。通常在新节点加入或节点重启后需要执行此操作。

负载重平衡的主要作用如下：

- **提高系统可扩展性**：由于 MQTT 连接具有持久性，集群扩容时，原节点上的连接不会自动迁移到新节点。负载重平衡可以将连接从过载节点平滑迁移到新节点，使整个集群的负载分布更均衡，并提高吞吐量、响应速度和资源利用率。
- **降低运维成本**：如果集群负载分布不均，部分节点过载而其他节点空闲，可以使用负载重平衡自动调整集群负载，使工作负载分布更加均衡，并降低运维成本。

有关 EMQX 集群负载重平衡的详细信息，请参阅[重平衡](../../../cluster/rebalancing.md)。

## EMQX Operator 3.0

EMQX Operator 3.0 在滚动更新和缩容操作期间仍会在内部使用 EMQX 节点疏散，但不再公开独立的 `Rebalance` Kubernetes 资源供用户触发集群重平衡。

如需主动触发负载重平衡，请直接使用 EMQX 重平衡工具。详情请参阅[重平衡](../../../cluster/rebalancing.md)。

## 早期 EMQX Operator 版本

在早期 EMQX Operator 版本中，集群重平衡对应的 CRD 为 `Rebalance`，示例如下：

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

> 有关早期版本中的 Rebalance 配置，请参阅 [Rebalance 参考](../reference/v2beta1-reference.md#rebalancestrategy)。

## 测试负载重平衡

### 重平衡前的集群负载分布

重平衡前，我们有意创建了一个连接分布不均的 EMQX 集群，并使用 Grafana 和 Prometheus 监控集群负载：

![](./assets/configure-emqx-rebalance/before-rebalance.png)

如图所示，集群由四个 EMQX 节点组成。三个节点各自处理 10,000 个连接，另一个节点的连接数为 **0**。

以下示例演示如何执行重平衡操作，使负载均匀分布到四个节点。

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

将文件保存为 `rebalance.yaml`，然后执行以下命令提交 Rebalance 任务：

```bash
$ kubectl apply -f rebalance.yaml
rebalance.apps.emqx.io/rebalance-sample created
```

#### 检查 Rebalance 进度

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
> 有关 `rebalanceStates` 字段的详细说明，请参阅 [rebalanceStates 参考](../reference/v2beta1-reference.md#rebalancestate)。

#### 等待完成

监控任务，直到其状态变为 `Completed`：

```bash
$ kubectl get rebalances rebalance-sample
NAME               STATUS      AGE
rebalance-sample   Completed   62s
```

> `STATUS` 字段表示 Rebalance 任务的生命周期状态：
>
> | 状态           | 含义                                          |
> | -------------- | --------------------------------------------- |
> | **Processing** | 正在进行重平衡。                              |
> | **Completed**  | 重平衡已成功完成。                            |
> | **Failed**     | 重平衡遇到错误并已停止。                      |

### 重平衡后的集群负载分布

![](./assets/configure-emqx-rebalance/after-rebalance.png)

上图显示 Rebalance 完成后的集群负载。如图所示，整个操作过程中的客户端连接迁移平滑且稳定。集群连接总数仍为 **10,000**，与重平衡前相同。

重平衡前，一个节点承载 **0** 个连接，另外三个节点各自承载 **10,000** 个连接。重平衡后，连接均匀地重新分布到四个节点，每个节点的负载稳定在约 **2,500** 个连接并保持一致。

EMQX Operator 通过评估以下条件来确定集群是否已达到平衡状态：

```
avg(源节点连接数) < avg(目标节点连接数) + abs_conn_threshold
或
avg(源节点连接数) < avg(目标节点连接数) * rel_conn_threshold
```

使用配置的 Rebalance 阈值和实际连接数：

- 源节点平均值：`avg(2553 + 2553 + 2554) ≈ 2553`
- 目标节点平均值：`2340`
- 检查条件：`2553 < 2340 * 1.1`

由于该条件成立，Operator 判定集群已达到平衡状态，重平衡任务成功完成。
