# K8s 部署指南

[EMQX Kubernetes Operator](https://www.emqx.com/zh/emqx-kubernetes-operator) 可以帮助您快速在 Kubernetes 的环境上快速创建和管理 EMQX Enterprise 集群。 通过集成滚动更新、自动化伸缩、集群和节点监控等常用功能，EMQX Operator 可以极大简化 EMQX 集群的部署和管理流程，将部署和管理的工作变成一种低成本的、标注化的、可重复性的能力。

:::tip 环境准备

部署前，请确认以下组件已经安装：

| 软件                                     | 版本要求     |
| ---------------------------------------- | ------------ |
| [Kubernetes](https://kubernetes.io/)     | 1.24 及以上  |
| [Helm](https://helm.sh/)                 | 3.0.0 及以上 |
| [cert-manager](https://cert-manager.io/) | 1.1.6 及以上 |

:::

## 部署 EMQX Operator

我们使用 [cert-manager](https://github.com/cert-manager/cert-manager) 来给 Webhook 服务提供证书。有关如何安装，可参考 [cert manager 手册 - 安装部分](https://cert-manager.io/docs/installation/)。

### 安装 EMQX Operator

1. 通过 Helm 安装 EMQX Operator。

   ```bash
   helm repo add emqx https://repos.emqx.io/charts
   helm repo update
   helm install emqx-operator emqx/emqx-operator --namespace emqx-operator-system --create-namespace
   ```

2. 等待 EMQX Operator 控制器就绪。

   ```bash
   $ kubectl get pods -l "control-plane=controller-manager" -n emqx-operator-system
   ```

   如 EMQX Operator 已就绪，可以看到类似命令返回：

   ```bash
   NAME                                                READY   STATUS    RESTARTS   AGE
   emqx-operator-controller-manager-68b866c8bf-kd4g6   1/1     Running   0          15s
   ```

### 升级 EMQX Operator

执行下面的命令可以升级 EMQX Operator，若想指定到升级版只需要增加 `--version=x.x.x` 参数即可。注意：不支持 1.x.x 版本升级到 2.x.x 版本。

```bash
helm upgrade emqx-operator emqx/emqx-operator -n emqx-operator-system 
```

### 卸载 EMQX Operator

执行如下命令卸载 EMQX Operator

```bash
helm uninstall emqx-operator -n emqx-operator-system
```

## 部署 EMQX

1. 运行以下命令部署 EMQX。

   {% emqxce %}

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

   {% endemqxce %}

   {% emqxee %}

   ```bash
   cat << "EOF" | kubectl apply -f -
     apiVersion: apps.emqx.io/v2alpha1
     kind: EMQX
     metadata:
       name: emqx
     spec:
       image: emqx/emqx-enterprise:5.0.0
   EOF
   ```

   {% endemqxee %}

   如希望查看完整代码示例，可前往 GitHub 查看 [emqx-full.yaml 页面](https://github.com/emqx/emqx-operator/blob/main/config/samples/emqx/v2alpha1/emqx-full.yaml) ；有关每个字段的详细解释，可参考 [EMQX Operator - API](https://docs.emqx.com/en/emqx-operator/latest/reference/v2alpha1-reference.html)。

2. 运行以下命令检查 EMQX 集群是否就绪。

   ```bash
   kubectl get pods
   kubectl get emqx emqx -o json | jq ".status.emqxNodes"
   ```

   由于需要等待所有的 EMQX 节点启动并加入集群，因此需要一定的等待时间，之后可以看到类似如下的代码返回，表示 EMQX 集群已就绪。

   ```bash
   {
     "lastTransitionTime": "2023-02-13T02:38:25Z",
     "lastUpdateTime": "2023-02-13T02:44:19Z",
     "message": "All resources are ready",
     "reason": "ClusterReady",
     "status": "True",
     "type": "Running"
   }
   ```

至此您已经通过 EMQX Kubernetes Operator 完成了 EMQX 集群的部署。

您可访问以下链接了解如何进行集群参数配置：

{% emqxce %}

[EMQX Operator - 通用配置](https://docs.emqx.com/zh/emqx-operator/latest/config/v1beta3/EmqxBroker.html)

{% endemqxce %}

{% emqxee %}
[EMQX Operator - 通用配置](https://docs.emqx.com/zh/emqx-operator/latest/config/v1beta3/EmqxEnterprise.html)

{% endemqxee %}

[配置 EMQX logs 采集](https://docs.emqx.com/zh/emqx-operator/latest/tasks/configure-emqx-logs-collection.html)

[配置 EMQX Core 和 Replicant 节点](https://docs.emqx.com/zh/emqx-operator/latest/tasks/configure-emqx-core-replicant.html)

[配置 EMQX TLS 证书](https://docs.emqx.com/zh/emqx-operator/latest/tasks/configure-emqx-tls.html)

[配置 EMQX License](https://docs.emqx.com/zh/emqx-operator/latest/tasks/configure-emqx-license.html)

[配置 EMQX 持久化](https://docs.emqx.com/zh/emqx-operator/latest/tasks/configure-emqx-persistence.html)

## 在公有云平台上部署 EMQX 集群

如希望在公有云 K8s 平台上部署 EMQX 集群，可阅读如下页面：

- [阿里 ACK 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/aliyun-ack-deployment.html)
- [华为 CCE 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/cce-deployment.html)
- [AWS EKS 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/aws-eks-deployment.html)
- [腾讯云 TKE 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/tencent-tke-deployment.html)
- [Azure AKS 上部署 EMQX 集群](https://docs.emqx.com/zh/emqx-operator/latest/deployment/azure-deployment.html)

## 下一步

使用客户端连接到 EMQX，进行消息收发请参考 [发布订阅操作](../messaging/mqtt-publish-and-subscribe.md)。
