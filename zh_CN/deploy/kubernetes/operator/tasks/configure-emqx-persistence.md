# 为 EMQX 集群启用持久化存储

## 目标

使用 `persistentVolumeClaimSpec` 字段为 EMQX 集群中的 Core 节点配置持久化存储。

## 配置 EMQX 集群持久化存储

`apps.emqx.io/v3beta1` EMQX CRD 中的 `.spec.coreTemplate.spec.persistentVolumeClaimSpec` 字段用于为每个 Core 节点配置持久化存储。该字段采用 Kubernetes `PersistentVolumeClaimSpec` 的模式和语义。

EMQX Operator 3.0 使用单个 StatefulSet 管理 Core 节点。每个 Core Pod 都具有稳定的标识，并在镜像更新和滚动更新期间始终使用同一个 PVC。配置此字段后，EMQX Operator 会使用持久卷声明（Persistent Volume Claim，PVC）作为 EMQX 容器的 `/opt/emqx/data` 卷，并通过指定的 [StorageClass](https://kubernetes.io/docs/concepts/storage/storage-classes/) 制备持久卷（Persistent Volume，PV）。

## PVC 生命周期

Core 节点的 PVC 与 StatefulSet Pod 序号绑定。例如，在镜像更新和滚动更新期间，`emqx-core-0` 的 PVC 始终挂载到 `emqx-core-0`，因此该节点会继续使用同一个数据卷。

EMQX Operator 会配置 Kubernetes，使其在不再需要 Core 节点 PVC 时将其删除：

- 缩容 Core 节点时，会删除已移除 Pod 序号所对应的 PVC。

    例如，将 Core 节点副本数从 5 缩减到 3 时，会删除序号 3 和 4 对应的 PVC。在缩容 StatefulSet 前，EMQX Operator 会将持久存储（Durable Storage）数据从这些 Core 节点重新均衡到其他节点，以免数据丢失或持久性降低。

- 删除 EMQX 自定义资源时，Kubernetes 会删除 Core StatefulSet 及其关联的 PVC。

- 滚动更新期间，StatefulSet 名称和 Pod 序号不会改变，因此 PVC 会被保留。

自动清理功能依赖 Kubernetes `StatefulSetAutoDeletePVC` 特性门控。从 Kubernetes 1.27 开始，该特性门控默认启用。如果集群管理员将其禁用，Kubernetes 会忽略 PVC 删除策略，此时必须手动清理不再使用的 PVC。

有关 PV 和 PVC 的详情，请参见 Kubernetes [持久卷](https://kubernetes.io/docs/concepts/storage/persistent-volumes/)文档。

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 进行部署。

   ```yaml
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       roots:
         license:
           key: "..."
     coreTemplate:
       spec:
         persistentVolumeClaimSpec:
           storageClassName: standard
           resources:
             requests:
               storage: 1Gi
           accessModes:
             - ReadWriteOnce
         replicas: 3
     listenersServiceTemplate:
       spec:
         type: LoadBalancer
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
   ```

   ::: tip

   使用 `storageClassName` 字段为 EMQX 数据选择合适的 [StorageClass](https://kubernetes.io/docs/concepts/storage/storage-classes/)。运行 `kubectl get storageclass` 可列出 Kubernetes 集群中已有的 StorageClass；也可以根据需要创建 StorageClass。

   :::

2. 等待 EMQX 集群就绪。

   运行 `kubectl get` 检查 EMQX 集群状态，并确保 `STATUS` 为 `Ready`。此过程可能需要一些时间。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## 验证持久化存储

验证 Kubernetes 是否会在 Core Pod 被替换后重新挂载同一个 PVC。请勿在此测试中删除 EMQX 资源，因为 EMQX Operator 会将 StatefulSet 配置为：删除 StatefulSet 时一并删除关联的 PVC。

1. 记录第一个 Core Pod 所挂载 PVC 的 UID：

   ```bash
   pvc_name=emqx-core-data-emqx-core-0
   pvc_uid_before=$(kubectl get pvc "${pvc_name}" -o jsonpath='{.metadata.uid}')
   kubectl get pvc "${pvc_name}"
   ```

2. 删除 Pod，并等待 StatefulSet 重新创建该 Pod：

   ```bash
   kubectl delete pod emqx-core-0
   kubectl wait --for=condition=Ready pod/emqx-core-0 --timeout=10m
   ```

3. Pod 就绪后，再次获取并比较 PVC UID：

   ```bash
   pvc_uid_after=$(kubectl get pvc "${pvc_name}" -o jsonpath='{.metadata.uid}')
   test "${pvc_uid_before}" = "${pvc_uid_after}" && echo "The Core Pod reused the same PVC."
   ```

   UID 相同即表示替换后的 Pod 复用了同一个 PVC。
