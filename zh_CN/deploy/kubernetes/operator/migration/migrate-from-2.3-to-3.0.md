# 从 EMQX Operator 2.3 迁移到 3.0

Operator 3.0 使用 `apps.emqx.io/v3beta1` API。它无法转换早期版本的 EMQX 自定义资源，也无法接管由 Operator 2.3 创建的工作负载。因此，此迁移过程会部署一个新的 EMQX 集群，而不是原地更新现有集群，并且不会将新旧集群合并为同一个集群。

::: warning

本操作指南不会迁移 MQTT 会话。请规划维护窗口，并确保客户端能够重新连接到新集群。EMQX 数据备份不包含活动连接、会话状态、离线队列、未确认消息以及持久存储数据。

:::

本操作指南会保留旧工作负载，以便在安装 Operator 3.0 和验证名称不同的新 EMQX 集群期间进行回滚。验证完成后，将客户端流量切换到新集群，再停用旧工作负载。

以下示例使用这些名称。请将其替换为实际部署使用的名称和命名空间：

```bash
export EMQX_NAMESPACE=default
export OLD_EMQX=my-emqx
export NEW_EMQX=my-emqx-v3
```

## 1. 准备迁移

开始前，请完成以下准备工作：

- 在与生产环境相同的环境中演练完整流程。
- 保持 EMQX 镜像和版本不变。仅在新集群由 Operator 3.0 管理并正常运行后再升级 EMQX。
- 确保 Kubernetes 集群有足够的容量同时运行新旧 EMQX 工作负载。
- 规划如何将客户端从旧的监听器 Service 切换到新的 Service，例如更改负载均衡器、Ingress 或 DNS 配置。
- 在执行最终备份前停止更改配置和应用数据。此后产生的更改不会复制到新集群。
- 确认未使用持久存储。如果正在使用，请仅在能够接受其数据丢失的情况下继续，因为本操作指南不会迁移持久存储数据。

EMQX CRD 的作用域为集群级。在删除 Operator 2.3 CRD 前，请列出所有 EMQX 资源：

```bash
kubectl get emqx.apps.emqx.io --all-namespaces
```

列出的所有资源都必须纳入同一次维护操作。如果还有其他 Operator 2.3 集群依赖此 CRD，请勿删除该 CRD。

## 2. 备份现有集群

保存现有自定义资源及其工作负载列表。请将这些文件存放在 Kubernetes 集群外，以便用于回滚：

```bash
kubectl get emqx.apps.emqx.io "$OLD_EMQX" \
  -n "$EMQX_NAMESPACE" -o yaml > emqx-v2.yaml

kubectl get statefulset,replicaset,pod,service,pvc \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$OLD_EMQX" -o yaml \
  > emqx-v2-workloads.yaml
```

选择一个正在运行的 Core Pod，并在其 `/tmp` 目录中创建 EMQX 全局数据备份：

```bash
OLD_EMQX_CORE_POD="$(kubectl get pod \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$OLD_EMQX,apps.emqx.io/db-role=core" \
  --field-selector=status.phase=Running \
  -o jsonpath='{.items[0].metadata.name}')"

kubectl exec -n "$EMQX_NAMESPACE" "$OLD_EMQX_CORE_POD" -c emqx -- \
  emqx ctl data export --dir /tmp
```

导出命令会输出生成的归档文件路径。将 `EMQX_BACKUP_FILE` 设置为该路径中的文件名，然后将归档文件从 Pod 复制到本地并验证：

```bash
export EMQX_BACKUP_FILE='<exported-file-name>.tar.gz'

kubectl cp -c emqx \
  "$EMQX_NAMESPACE/$OLD_EMQX_CORE_POD:/tmp/$EMQX_BACKUP_FILE" \
  "./$EMQX_BACKUP_FILE"

chmod 600 "./$EMQX_BACKUP_FILE"
test -s "./$EMQX_BACKUP_FILE"
tar -tzf "./$EMQX_BACKUP_FILE" >/dev/null
```

也可以在 Dashboard 的 **系统** -> **备份与恢复**页面创建并下载全局备份。有关详情，请参见[备份与恢复](../../../../operations/backup-restore.md)。请勿使用 Operator 生成的 API 密钥执行此备份：通过 API 密钥认证执行的导出不会包含 Dashboard 用户和 API 密钥。

备份包含受支持的配置、EMQX 数据目录中的文件，以及认证记录、API 密钥和保留消息等内置数据库数据，但不包含上述警告中列出的实时 MQTT 状态。建议先在测试环境中恢复归档文件以验证备份，再继续迁移。

## 3. 转换 EMQX 清单

创建名为 `emqx-v3.yaml` 的新清单。为其设置不同的 `metadata.name`，例如 `$NEW_EMQX` 的值，以免 Operator 3.0 将已成为孤立资源的 Operator 2.3 工作负载误认为由自己管理的资源。

对清单进行以下更改：

| Operator 2.3 设置 | Operator 3.0 中的变化 | 所需操作 |
| --- | --- | --- |
| `apiVersion: apps.emqx.io/v2` | 已由 `apps.emqx.io/v3beta1` 取代 | 更改清单中的 `apiVersion`。 |
| `.spec.config.data` | 已由 `.spec.config.roots` 取代 | 将 HOCON 文本转换为结构化 YAML。 |
| `.spec.coreTemplate.spec.volumeClaimTemplates` | 已重命名为 `.spec.coreTemplate.spec.persistentVolumeClaimSpec` | 重命名该字段，并保留其存储设置。 |
| `.spec.coreTemplate.spec.replicas`<br/>`.spec.replicantTemplate.spec.replicas` | 默认值从 `2` 更改为 `1` | 明确设置所需的副本数。启用 Replicant 时，至少配置两个 Core 副本。 |
| `.spec.bootstrapAPIKeys` | 已删除 | 从全局备份恢复现有 API 密钥，之后通过 EMQX 管理这些密钥。 |
| `.spec.updateStrategy.initialDelaySeconds` | 已删除，且没有直接替代字段 | 删除该字段，并重新评估滚动更新的时间安排。请勿将其映射到语义不同的 `minReadySeconds`。 |
| `.spec.updateStrategy.evacuationStrategy.connEvictRate` | 已重命名为 `.spec.updateStrategy.evacuationStrategy.connectionEvictionRate` | 重命名该字段并保留原值。 |
| `.spec.updateStrategy.evacuationStrategy.sessEvictRate` | 已重命名为 `.spec.updateStrategy.evacuationStrategy.sessionEvictionRate` | 重命名该字段并保留原值。 |
| `.spec.coreTemplate.spec.minAvailable`<br/>`.spec.coreTemplate.spec.maxUnavailable`<br/>`.spec.replicantTemplate.spec.minAvailable`<br/>`.spec.replicantTemplate.spec.maxUnavailable` | 已删除 | 迁移后单独创建 PDB；请参见[配置 Pod 干扰预算](../tasks/configure-disruption-budgets.md)。 |
| `Rebalance` 资源 | 已删除 | 删除 Operator 2.3 CRD 前，删除现有的 `Rebalance` 资源。 |

例如，将以下 Operator 2.3 配置：

```yaml
spec:
  config:
    mode: Merge
    data: |
      log.console.level = warning
      dashboard.listeners.http.bind = 18083
```

转换为以下 Operator 3.0 结构：

```yaml
spec:
  config:
    roots:
      log:
        console:
          level: warning
      dashboard:
        listeners:
          http:
            bind: 18083
```

有关配置详情，请参见[配置 EMQX](../tasks/configure-emqx-config.md)。

部署新集群前，复制旧 Operator 的引导 API 密钥 Secret，并将其命名为新 EMQX 资源所需的名称：

```bash
kubectl get secret "$OLD_EMQX-bootstrap-api-key" \
  -n "$EMQX_NAMESPACE" -o jsonpath='{.data.bootstrap_api_key}' \
  | base64 --decode \
  | kubectl create secret generic "$NEW_EMQX-bootstrap-api-key" \
      -n "$EMQX_NAMESPACE" \
      --from-file=bootstrap_api_key=/dev/stdin
```

全局备份包含 Operator 控制器的 API 密钥记录。复用引导 Secret 可以确保恢复该记录时凭据保持一致。请勿复制旧的 node-cookie Secret，也不要配置 `node.cookie`。Operator 3.0 会为本操作指南使用的独立集群创建新的 cookie。

## 4. 停止 Operator 2.3 并保留其工作负载

将 Operator 2.3 控制器的副本数缩减为零。如果使用了自定义安装，请相应调整命名空间和 Deployment 名称：

```bash
kubectl scale deployment emqx-operator-controller-manager \
  -n emqx-operator-system --replicas=0
```

使用孤立级联删除策略删除每个 Operator 2.3 EMQX 资源。此操作会删除自定义资源，但会保留其 StatefulSet、ReplicaSet、Pod、Service 和 PVC 并使其继续运行：

```bash
kubectl delete emqx.apps.emqx.io "$OLD_EMQX" \
  -n "$EMQX_NAMESPACE" --cascade=orphan --wait=true
```

确认旧资源和客户端端点仍然可用：

```bash
kubectl get statefulset,replicaset,pod,service,pvc \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$OLD_EMQX"
```

使用与安装时相同的方式卸载 Operator 2.3，然后删除其 CRD：

```bash
kubectl delete --ignore-not-found crd \
  emqxes.apps.emqx.io rebalances.apps.emqx.io
```

按照[安装 Operator 并部署 EMQX](../getting-started.md)中的步骤安装 Operator 3.0，但不要部署该页面中的 EMQX 示例资源。

## 5. 部署并恢复新集群

验证并应用转换后的清单：

```bash
kubectl apply --dry-run=server -f emqx-v3.yaml
kubectl apply -f emqx-v3.yaml
```

等待工作负载和配置均就绪：

```bash
kubectl wait emqx.apps.emqx.io/"$NEW_EMQX" \
  -n "$EMQX_NAMESPACE" --for=condition=Ready --timeout=15m

kubectl wait emqx.apps.emqx.io/"$NEW_EMQX" \
  -n "$EMQX_NAMESPACE" --for=condition=ConfigApplied --timeout=15m
```

选择一个正在运行的 Core Pod，将备份复制到该 Pod，并恢复数据：

```bash
NEW_EMQX_CORE_POD="$(kubectl get pod \
  -n "$EMQX_NAMESPACE" \
  -l "apps.emqx.io/instance=$NEW_EMQX,apps.emqx.io/db-role=core" \
  --field-selector=status.phase=Running \
  -o jsonpath='{.items[0].metadata.name}')"

kubectl cp -c emqx \
  "./$EMQX_BACKUP_FILE" \
  "$EMQX_NAMESPACE/$NEW_EMQX_CORE_POD:/tmp/$EMQX_BACKUP_FILE"

kubectl exec -n "$EMQX_NAMESPACE" "$NEW_EMQX_CORE_POD" -c emqx -- \
  emqx ctl data import "/tmp/$EMQX_BACKUP_FILE"

kubectl exec -n "$EMQX_NAMESPACE" "$NEW_EMQX_CORE_POD" -c emqx -- \
  rm -f "/tmp/$EMQX_BACKUP_FILE"
```

继续将转换后的 `.spec.config.roots` 作为配置的权威来源。切换客户端流量前，请再次执行上述就绪检查，并确认导入已成功完成。

## 6. 切换客户端流量

针对新的监听器 Service 执行具有代表性的连接、认证、发布、订阅、保留消息、规则和集成测试。然后更新负载均衡器、Ingress 或 DNS 记录，将新的客户端连接发送到 `<new-emqx-name>-listeners`。

将此次流量切换视为新旧会话的分界点。客户端可能会断开连接，并且必须重新连接到新集群。继续操作前，请验证客户端重连行为，并监控认证失败、重复重连和消息流。

## 7. 完成迁移

验收期结束后：

1. 列出已成为孤立资源的 Operator 2.3 StatefulSet 和 ReplicaSet，然后将每个工作负载的副本数缩减为零。
2. 从 Operator 3.0 集群创建新的 EMQX 数据备份。
3. 仅在回滚窗口结束后，删除已成为孤立资源的 Operator 2.3 工作负载。

在验证新集群并将两个备份都存放到 Kubernetes 外部之前，请勿删除旧 PVC 或 node-cookie Secret。删除这些资源后无法恢复。

## 回滚

确认旧的 StatefulSet、ReplicaSet、PVC、配置资源、Service 和 Secret 仍然存在。

如果旧工作负载仍在运行，请测试旧的监听器 Service，并将客户端流量切回该 Service。

如果已将旧工作负载的副本数缩减为零，请按照以下步骤通过蓝绿更新回滚到 Operator 2.3：

1. 将迁移前运行的相同 StatefulSet 和 ReplicaSet 恢复到之前的副本数。使用 `emqx-v2-workloads.yaml` 确定这些工作负载及其副本数。

2. 等待 EMQX 在旧 Core Pod 中启动：

   ```bash
   kubectl exec -n "$EMQX_NAMESPACE" <old-core-pod> -c emqx -- \
     emqx ctl status
   ```

   由于其就绪门控由 Operator 2.3 管理，重新创建的 Pod 可能仍处于未就绪状态。此时请勿切换客户端流量。

3. 使用与安装时相同的方式卸载 Operator 3.0。此操作还会删除新的 EMQX 资源、由其管理的工作负载以及 EMQX CRD。如果单独安装了 CRD，请先将其删除再继续。

4. 重新安装之前管理旧集群的同一 Operator 2.3 版本，然后重新应用已保存的资源：

   ```bash
   kubectl apply -f emqx-v2.yaml
   ```

5. 等待恢复的资源就绪，然后先测试旧的监听器 Service，再切换流量：

   ```bash
   kubectl wait emqx.apps.emqx.io/"$OLD_EMQX" \
     -n "$EMQX_NAMESPACE" --for=condition=Ready --timeout=15m
   ```

   严格来说，Operator 2.3 并不会接管旧工作负载。它会将正在运行的工作负载用作蓝绿更新的起始版本，并执行完整的数据迁移。
