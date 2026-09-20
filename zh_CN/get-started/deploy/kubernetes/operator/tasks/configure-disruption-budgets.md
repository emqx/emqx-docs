# 配置 Pod 干扰预算

EMQX Operator 3.0 不会为 EMQX 集群创建 Kubernetes PodDisruptionBudget（PDB）。本页介绍如何分别为 Core 和 Replicant Pod 创建 PDB，以限制节点排空、集群自动伸缩器缩容或 Descheduler 驱逐等操作造成的自愿干扰。

PDB 仅限制通过 Kubernetes Eviction API 请求的驱逐，无法防止非自愿故障、直接删除 Pod，或 EMQX Operator 在滚动更新期间执行的 Pod 替换。

## 准备工作

本示例假定当前 `kubectl` 命名空间中包含一个名为 `emqx` 的 EMQX 资源。如果资源使用其他名称，请将 `apps.emqx.io/instance` 选择算符和 EMQX 资源命令中的 `emqx` 替换为该资源的 `metadata.name`。每个 PDB 必须与其 EMQX 资源位于同一命名空间。PDB 资源名称 `emqx-core` 和 `emqx-replicant` 仅为示例。如果修改了这些名称，请在 PDB 验证命令中使用修改后的名称。

EMQX Operator 会在 EMQX 状态中发布当前的 Pod 选择算符：

- `status.coreSelector`
- `status.replicantSelector`

对于名为 `emqx` 的 EMQX 集群，这些选择算符使用以下稳定标签：

```yaml
apps.emqx.io/instance: emqx # 使用对应 EMQX CR 的 metadata.name。
apps.emqx.io/managed-by: emqx-operator
apps.emqx.io/db-role: core # 对于 Replicant Pod，使用 replicant。
```

请根据每种角色的 Pod 数量和可用性要求选择 `minAvailable` 或 `maxUnavailable`。例如，对于只有一个 Pod 的角色，`maxUnavailable: 1` 无法保证该角色的可用性。如果自愿干扰不能移除该 Pod，请使用 `minAvailable: 1`。

## 创建 PodDisruptionBudget

1. 将以下资源保存为 `emqx-pdb.yaml`。

   本示例定义了两个 PDB，将 Core Pod 和 Replicant Pod 作为两个独立的可用性资源池进行保护。如果使用一个 PDB 同时匹配这两种角色，其中一种角色发生驱逐时，可能会占用另一种角色所需的中断容量。如果集群未使用 Replicant Pod，请省略 `emqx-replicant` PDB。

   ```yaml
   apiVersion: policy/v1
   kind: PodDisruptionBudget
   metadata:
     name: emqx-core
   spec:
     maxUnavailable: 1
     selector:
       matchLabels:
         apps.emqx.io/instance: emqx
         apps.emqx.io/managed-by: emqx-operator
         apps.emqx.io/db-role: core
   ---
   apiVersion: policy/v1
   kind: PodDisruptionBudget
   metadata:
     name: emqx-replicant
   spec:
     maxUnavailable: 1
     selector:
       matchLabels:
         apps.emqx.io/instance: emqx
         apps.emqx.io/managed-by: emqx-operator
         apps.emqx.io/db-role: replicant
   ```

2. 应用 PDB：

   ```bash
   kubectl apply -f emqx-pdb.yaml
   ```

## 验证 PodDisruptionBudget

1. 检查 EMQX Operator 报告的选择算符：

   ```bash
   kubectl get emqx emqx -o jsonpath='{.status.coreSelector}{"\n"}{.status.replicantSelector}{"\n"}'
   ```

2. 验证每个选择算符是否与预期的 Pod 匹配。如果集群未使用 Replicant Pod，请省略第二条命令。

   ```bash
   kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/managed-by=emqx-operator,apps.emqx.io/db-role=core'
   kubectl get pods -l 'apps.emqx.io/instance=emqx,apps.emqx.io/managed-by=emqx-operator,apps.emqx.io/db-role=replicant'
   ```

3. 检查每个 PDB，并查看允许的中断次数。如果集群未使用 Replicant Pod，请省略 `emqx-replicant` 及其 `describe` 命令。

   ```bash
   kubectl get pdb emqx-core emqx-replicant
   kubectl describe pdb emqx-core
   kubectl describe pdb emqx-replicant
   ```

   限制过严的 PDB 可能会无限期阻塞节点排空。开始维护前，请检查每个 PDB 的 `status.disruptionsAllowed` 值，并确保有足够的匹配 Pod 处于就绪状态。
