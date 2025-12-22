# 启用持久化

## 目标

通过 `volumeClaimTemplates` 字段为 EMQX 集群的 Core 节点集配置持久化。

## 配置 EMQX 集群持久化

EMQX CRD `apps.emqx.io/v2beta1` 支持通过 `.spec.coreTemplate.spec.volumeClaimTemplates` 配置每个 Core 节点数据的持久化。

`.spec.coreTemplate.spec.volumeClaimTemplates` 字段的定义和语义与 Kubernetes API 中定义的 `PersistentVolumeClaimSpec` 一致。

当您指定 `.spec.coreTemplate.spec.volumeClaimTemplates` 字段时，EMQX Operator 会将 EMQX 容器的 `/opt/emqx/data` 卷配置为由 Persistent Volume Claim (PVC) 支持，该 PVC 使用指定的 [StorageClass](https://kubernetes.io/zh-cn/docs/concepts/storage/storage-classes/) 提供 Persistent Volume (PV)。因此，当删除 EMQX Pod 时，关联的 PV 和 PVC 会被保留，从而保留 EMQX 运行时数据。

有关 PV 和 PVC 的更多详细信息，请参阅 [Persistent Volumes](https://kubernetes.io/zh-cn/docs/concepts/storage/persistent-volumes/) 文档。

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署。

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       data: |
         license {
           key = "..."
         }
     coreTemplate:
       spec:
         volumeClaimTemplates:
           storageClassName: standard
           resources:
             requests:
               storage: 20Mi
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

   使用 `storageClassName` 字段为 EMQX 数据选择合适的 [StorageClass](https://kubernetes.io/zh-cn/docs/concepts/storage/storage-classes/)。运行 `kubectl get storageclass` 列出 Kubernetes 集群中已存在的 StorageClass，或根据您的需求创建 StorageClass。

   :::

2. 等待 EMQX 集群就绪。使用 `kubectl get` 检查 EMQX 集群的状态，并确保 `STATUS` 为 `Ready`。这可能需要一些时间。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## 验证持久化

1. 在 EMQX Dashboard 中创建测试规则。

   ```bash
   external_ip=$(kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

   - 在 `http://${external_ip}:18083` 登录 EMQX Dashboard。

   - 导航到**集成** -> **规则**创建新规则。

   - 为此规则附加简单动作。

     ![](./assets/configure-emqx-persistent/emqx-core-action.png)

   - 点击**保存**生成规则。规则创建成功后，页面上会出现一条 ID 为 `emqx-persistent-test` 的相应记录，如下图所示：

     ![](./assets/configure-emqx-persistent/emqx-core-rule-old.png)

2. 删除旧 EMQX 集群。

   运行以下命令删除 EMQX 集群，其中 `emqx.yaml` 是您之前用于部署集群的文件：

   ```bash
   $ kubectl delete -f emqx.yaml
   emqx.apps.emqx.io "emqx" deleted
   ```

3. 重新部署 EMQX 集群。

   运行以下命令重新部署 EMQX 集群：

   ```bash
   $ kubectl apply -f emqx.yaml
   emqx.apps.emqx.io/emqx created
   ```

4. 等待 EMQX 集群就绪。通过浏览器访问 EMQX Dashboard 验证之前创建的规则是否仍然存在，如下图所示：

   ![](./assets/configure-emqx-persistent/emqx-core-rule-new.png)

   在旧集群中创建的 `emqx-persistent-test` 规则在新集群中仍然存在，这确认了持久化配置工作正常。
