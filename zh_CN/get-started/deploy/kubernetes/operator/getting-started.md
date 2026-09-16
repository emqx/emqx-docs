# 安装 Operator 并部署 EMQX

本节将指导您准备 EMQX Operator 环境、安装 Operator 本身，然后使用它部署 EMQX。通过遵循提供的步骤，您可以使用 Operator 高效可靠地安装和管理 EMQX。

## 准备环境

在部署 EMQX Operator 之前，请确保以下组件已准备就绪：

- 运行 Kubernetes 1.24 或更高版本的 [Kubernetes](https://kubernetes.io/docs/concepts/overview/) 环境。
- 一个可以访问 Kubernetes 集群的 [kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl) 工具。您可以使用 `kubectl cluster-info` 命令检查 Kubernetes 集群的状态。

## 安装 EMQX Operator

1. 使用以下命令安装 EMQX Operator：

   ```bash
   $ kubectl apply --server-side=true -f https://github.com/emqx/emqx-operator/releases/latest/download/install.yaml
   ```

   该命令将下载最新的 2.3.x 版本，在整个集群范围内安装 EMQX 的 CRD（自定义资源定义），并将控制器服务部署到独立的 `emqx-operator-system` 命名空间中。

3. 等待 EMQX Operator 就绪：

   ```bash
   $ kubectl wait --for=condition=Ready pods --namespace emqx-operator-system -l "control-plane=controller-manager"
   pod/emqx-operator-controller-manager-57bd7b8bd4-h2mcr condition met
   ```

Operator 运行后，您可以继续部署 EMQX。

## 部署 EMQX

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署。

   ```yaml
   apiVersion: apps.emqx.io/v2
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
   ```

   有关 EMQX CRD 的更多详细信息，请查看 [参考文档](./reference/v2beta1-reference.md)。

2. 等待 EMQX 集群就绪。

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx      Ready     2m55s
   ```

   请确保 `STATUS` 为 `Ready`。EMQX 集群可能需要一些时间才能就绪。

## 故障排查

EMQX Operator 会向 Kubernetes API 暴露有限数量的事件。

```sh
kubectl get events --sort-by=.lastTimestamp
```

另外，如果 EMQX 资源未能进入 `Ready` 状态条件，请查看 controller manager 的日志以获取更多详细信息：

```sh
kubectl logs -l "control-plane=controller-manager" --tail=-1 --namespace emqx-operator-system
```

## 在公有云中部署 EMQX

使用以下指南，通过 EMQX Operator 在托管 Kubernetes 服务上部署 EMQX：

- [在阿里云中部署 EMQX (AKS)](./alibaba-cloud.md)
- [在华为云中部署 EMQX (CCE)](./huawei-cloud.md)
- [在腾讯云中部署 EMQX (TKE)](./tencent-cloud.md)
- [在 AWS 中部署 EMQX](./aws-eks.md)
- [在 GCP 中部署 EMQX](./gcp-gke.md)
- [在 Azure 中部署 EMQX](./azure-aks.md)
