# 启用 Core-Replicant 部署

## 目标

- 通过 `coreTemplate` 字段配置 EMQX 集群 Core 节点。
- 通过 `replicantTemplate` 字段配置 EMQX 集群 Replicant 节点。

## Core 和 Replicant 节点

EMQX 集群中的节点可以具有两种角色之一：Core 节点和 Replicant 节点。

- Core 节点负责集群中的数据持久化，并作为共享集群状态的权威来源，例如路由表、MQTT 客户端通道、保留消息、集群配置、告警、Dashboard 用户凭据等。
- Replicant 节点被设计为无状态的，不参与数据库操作。添加或删除 Replicant 节点不会影响集群数据的冗余。

典型 EMQX 集群中 Core 和 Replicant 节点之间的通信如下图所示：

  <div style="text-align:center">
  <img src="./assets/configure-core-replicant/mria-core-replicant.png" style="zoom:30%;" />
  </div>

有关 EMQX Core-Replicant 架构的更多信息，请参阅[集群架构](../../../../../develop/cluster/mria-introduction.md)文档。

:::tip
EMQX 集群中必须至少有一个 Core 节点。为了高可用性，EMQX Operator 建议 EMQX 集群至少有三个 Core 节点。
:::

## 配置 EMQX 集群

EMQX CRD `apps.emqx.io/v2beta1` 支持通过 `.spec.coreTemplate` 字段配置 EMQX 集群的 Core 节点，并通过 `.spec.replicantTemplate` 字段配置 EMQX 集群的 Replicant 节点。

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
         replicas: 2
         resources:
           requests:
             cpu: 250m
             memory: 512Mi
     replicantTemplate:
       spec:
         replicas: 3
         resources:
           requests:
             cpu: 250m
             memory: 1Gi
     dashboardServiceTemplate:
       spec:
         type: LoadBalancer
   ```

   在上面的示例中，EMQX CR 定义了一个由两个 Core 节点和三个 Replicant 节点组成的 EMQX 集群。

   Core 节点至少需要 512Mi 内存，Replicant 节点至少需要 1Gi 内存。您可以根据实际业务负载调整这些约束。通常，Replicant 节点接受所有客户端请求，因此 Replicant 节点所需的资源可能更高，以适应许多并发连接。

2. 等待 EMQX 集群就绪。使用 `kubectl get` 检查 EMQX 集群的状态，并确保 `STATUS` 为 `Ready`。这可能需要一些时间。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## 验证 EMQX 集群

您可以通过检查 EMQX CR 的 `.status` 字段查看集群中所有节点的信息。

```bash
$ kubectl get emqx emqx -o json | jq .status.coreNodes
[
  {
    "name": "emqx@emqx-core-adcdef012-0.emqx-headless.default.svc.cluster.local",
    "node_status": "running",
    "otp_release": "27.2-3/15.2",
    "role": "core",
    "version": "@EE_VERSION@"
  },
  {
    "name": "emqx@emqx-core-adcdef012-1.emqx-headless.default.svc.cluster.local",
    "node_status": "running",
    "otp_release": "27.2-3/15.2",
    "role": "core",
    "version": "@EE_VERSION@"
  }
]
```


```bash
$ kubectl get emqx emqx -o json | jq .status.replicantNodes
[
  {
    "name": "emqx@10.244.4.56",
    "node_status": "running",
    "otp_release": "27.2-3/15.2",
    "role": "replicant",
    "version": "@EE_VERSION@"
  },
  {
    "name": "emqx@10.244.4.57",
    "node_status": "running",
    "otp_release": "27.2-3/15.2",
    "role": "replicant",
    "version": "@EE_VERSION@"
  },
  {
    "name": "emqx@10.244.4.58",
    "node_status": "running",
    "otp_release": "27.2-3/15.2",
    "role": "replicant",
    "version": "@EE_VERSION@"
  }
]
```
