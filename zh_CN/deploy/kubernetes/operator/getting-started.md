# 安装 Operator 并部署 EMQX

本节将指导您准备 EMQX Operator 环境、安装 Operator 本身，然后使用它部署 EMQX。通过遵循提供的步骤，您可以使用 Operator 高效可靠地安装和管理 EMQX。

## 准备环境

在部署 EMQX Operator 之前，请确保以下组件已准备就绪：

- 运行 Kubernetes 1.24 或更高版本的 [Kubernetes](https://kubernetes.io/docs/concepts/overview/) 环境。

- 一个可以访问 Kubernetes 集群的 [kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl) 工具。您可以使用 `kubectl cluster-info` 命令检查 Kubernetes 集群的状态。

- [Helm](https://helm.sh) 3 或更高

## 安装 EMQX Operator

1. 安装 `cert-manger`。

   ::: tip
   需要 `cert-manager` 版本 `1.1.6` 或更高。如果 `cert-manager` 已经安装并启动，请跳过此步骤。
   :::

   你可以使用 Helm 来安装 `cert-manager`。

   ```bash
   $ helm repo add jetstack https://charts.jetstack.io
   $ helm repo update
   $ helm upgrade --install cert-manager jetstack/cert-manager \
     --namespace cert-manager \
     --create-namespace \
     --set crds.enabled=true
   ```

   或者按照官方的 [cert-manager 安装指南](https://cert-manager.io/docs/installation/)来安装它。

2. 运行以下命令来安装 EMQX Operator。

   ```bash
   $ helm repo add emqx https://repos.emqx.io/charts
   $ helm repo update
   $ helm upgrade --install emqx-operator emqx/emqx-operator \
     --namespace emqx-operator-system \
     --create-namespace
   ```

3. 等待 EMQX Operator 就绪：

   ```bash
   $ kubectl wait --for=condition=Ready pods -l "control-plane=controller-manager" -n emqx-operator-system
   pod/emqx-operator-controller-manager-57bd7b8bd4-h2mcr condition met
   ```

Operator 运行后，您可以继续部署 EMQX。

## 部署 EMQX

:::: tabs type:card

::: tab EMQX Enterprise 5

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署。

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
      name: emqx-ee
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
   emqx-ee   Ready     2m55s
   ```

   请确保 `STATUS` 为 `Ready`。EMQX 集群可能需要一些时间才能就绪。
   :::

::: tab EMQX Open Source 5

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署。

   ```yaml
   apiVersion: apps.emqx.io/v2beta1
   kind: EMQX
   metadata:
      name: emqx
   spec:
      image: emqx/emqx:@CE_VERSION@
   ```

   有关 EMQX CRD 的更多详细信息，请查看 [参考文档](./reference/v2beta1-reference.md)。

2. 等待 EMQX 集群就绪。

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx      Ready     2m55s
   ```

   请确保 `STATUS` 为 `Ready`。EMQX 集群可能需要一些时间才能就绪。后台会发生很多事情。
   :::

::::

## 在公有云中部署 EMQX

使用以下指南，通过 EMQX Operator 在托管 Kubernetes 服务上部署 EMQX：

- [在阿里云中部署 EMQX (AKS)](./alibaba-cloud.md)
- [在华为云中部署 EMQX (CCE)](./huawei-cloud.md)
- [在腾讯云中部署 EMQX (TKE)](./tencent-cloud.md)
- [在 AWS 中部署 EMQX](./aws-eks.md)
- [在 GCP 中部署 EMQX](./gcp-gke.md)
- [在 Azure 中部署 EMQX](./azure-aks.md)
