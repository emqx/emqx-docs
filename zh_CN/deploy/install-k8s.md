# K8s 部署指南

通过 [EMQX Kubernetes Operator](https://www.emqx.com/zh/emqx-kubernetes-operator)，您可在 Kubernetes （K8s）上自动化部署、配置和管理 EMQX 集群。

EMQX Operator 可以大大简化部署和管理 EMQX 集群的流程，对于管理和配置的知识要求也更低。它把部署和管理的工作变成一种低成本的、标注化的、可重复性的能力，实现滚动更新、自动化伸缩、集群和节点监控等常用功能。

:::tip
需要 Kubernetes v1.20.0 及以上版本。
:::

## 部署 EMQX Kubernetes Operator

我们使用 [cert-manager](https://github.com/cert-manager/cert-manager) 来给 Webhook 服务提供证书。你可以通过 [cert manager 文档](https://cert-manager.io/docs/installation/)来安装。

1. 通过 Helm 安装：

  ```bash
  helm repo add emqx https://repos.emqx.io/charts
  helm repo update
  helm install emqx-operator emqx/emqx-operator --namespace emqx-operator-system --create-namespace
  ```

2. 等待 EMQX Kubernetes Operator 控制器就绪

  ```bash
  $ kubectl get pods -l "control-plane=controller-manager" -n emqx-operator-system
  NAME                                                READY   STATUS    RESTARTS   AGE
  emqx-operator-controller-manager-68b866c8bf-kd4g6   1/1     Running   0          15s
  ```

## 部署 EMQX

1. 部署 EMQX 自定义资源

  ```bash
  cat << "EOF" | kubectl apply -f -
    apiVersion: apps.emqx.io/v2alpha1
    kind: EMQX
    metadata:
      name: emqx
    spec:
      image: emqx/emqx:5.0.14
  EOF
  ```

​	您可前往 [EMQX Kubernetes Operator 的 GitHub 页面](https://github.com/emqx/emqx-operator/blob/main/config/samples/emqx/v2alpha1/emqx-full.yaml) 查看完整代码示例；有关每个字段的详细解释，请参考 [EMQX Operator - API](https://docs.emqx.com/en/emqx-operator/latest/reference/v2alpha1-reference.html)。

2. 检查 EMQX 自定义资源状态

  ```bash
  kubectl get pods
  kubectl get emqx emqx -o json | jq ".status.emqxNodes"
  ```

至此您已经通过 EMQX Kubernetes Operator 完成了 EMQX 集群的部署。

您可访问以下链接了解如何进行集群参数配置：

-  {%emqxce%} [EMQX Operator - 通用配置](https://docs.emqx.com/zh/emqx-operator/latest/config/v1beta3/EmqxBroker.html) {%emqxce%}{%emqxee%} [EMQX Operator - 通用配置](https://docs.emqx.com/zh/emqx-operator/latest/config/v1beta3/EmqxEnterprise.html) {%endemqxee%}

- [配置 EMQX logs 采集](https://docs.emqx.com/zh/emqx-operator/latest/tasks/configure-emqx-logs-collection.html)

  [配置 EMQX Core 和 Replicant 节点](https://docs.emqx.com/zh/emqx-operator/latest/tasks/configure-emqx-core-replicant.html)

  [配置 EMQX TLS 证书](https://docs.emqx.com/zh/emqx-operator/latest/tasks/configure-emqx-tls.html)

  [配置 EMQX License](https://docs.emqx.com/zh/emqx-operator/latest/tasks/configure-emqx-license.html)

  [配置 EMQX 持久化](https://docs.emqx.com/zh/emqx-operator/latest/tasks/configure-emqx-persistence.html)

## 公有云平台上部署 EMQX 集群

以下内容将指导您通过 Operator，在公有云 K8s 平台上部署 EMQX 集群：

- [阿里 ACK 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/aliyun-ack-deployment.html)
- [华为 CCE 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/cce-deployment.html)
- [AWS EKS 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/aws-eks-deployment.html)
- [腾讯云 TKE 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/tencent-tke-deployment.html)
- [Azure AKS 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/azure-deployment.html)

## 下一步

使用客户端连接到 EMQX，进行消息收发请参考 [发布订阅操作](../messaging/mqtt-publish-and-subscribe.md)。
