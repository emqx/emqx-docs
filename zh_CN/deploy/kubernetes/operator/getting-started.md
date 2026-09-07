# 安装 EMQX Operator 并部署 EMQX

本节介绍如何准备 EMQX Operator 的运行环境、安装 Operator，以及使用 Operator 部署 EMQX。

## 准备环境

部署 EMQX Operator 前，请准备以下组件：

- 运行 Kubernetes 1.27 或更高版本的 [Kubernetes](https://kubernetes.io/docs/concepts/overview/) 集群，并确保已启用 `StatefulSetAutoDeletePVC` 特性门控。该特性门控在 EMQX Operator 支持的 Kubernetes 版本中默认启用。
- 可访问 Kubernetes 集群的 [kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl) 工具。可运行 `kubectl cluster-info` 检查 Kubernetes 集群状态。

## 安装 EMQX Operator

1. 运行以下命令安装 EMQX Operator：

   ```bash
   $ kubectl apply --server-side=true -f https://github.com/emqx/emqx-operator/releases/latest/download/install.yaml
   ```

   此命令将下载最新的 EMQX Operator 版本、安装集群级 EMQX CRD，并在 `emqx-operator-system` 命名空间中部署控制器。

2. 等待 EMQX Operator 就绪：

   ```bash
   $ kubectl wait --for=condition=Ready pods --namespace emqx-operator-system -l "control-plane=controller-manager"
   pod/emqx-operator-controller-manager-57bd7b8bd4-h2mcr condition met
   ```

Operator 开始运行后，即可部署 EMQX。

## 部署 EMQX

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
           key: "evaluation"
   ```

   此清单将部署一个单节点 EMQX 集群，默认的社区版 License 可支持该部署。配置多节点集群或将现有集群扩容到多个节点前，请配置支持集群功能的 License。本示例将 `license.key` 设置为 `"evaluation"`，用于评估环境。

   有关 EMQX CRD 的详情，请参见 [API 参考](./reference/v3beta1-reference.md)。

2. 等待 EMQX 集群就绪。

   ```bash
   $ kubectl get emqx
   NAME      STATUS    AGE
   emqx      Ready     2m55s
   ```

   确保 `STATUS` 为 `Ready`。EMQX 集群可能需要一段时间才能就绪。

## 故障排查

EMQX Operator 仅向 Kubernetes API 提供有限数量的事件。可运行以下命令查看事件：

```sh
kubectl get events --sort-by=.lastTimestamp
```

如果 EMQX 资源无法进入 `Ready` 状态，请运行以下命令查看 Controller Manager 日志：

```sh
kubectl logs -l "control-plane=controller-manager" --tail=-1 --namespace emqx-operator-system
```

## 在公有云上部署

请参照以下指南，使用 EMQX Operator 在托管 Kubernetes 服务上部署 EMQX：

- [Amazon Elastic Kubernetes Service（EKS）](./aws-eks.md)
- [Google Cloud GKE](./gcp-gke.md)
- [Azure Kubernetes Service（AKS）](./azure-aks.md)
