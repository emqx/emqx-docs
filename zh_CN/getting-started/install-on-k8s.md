# 使用 EMQX Operator 部署 EMQX 集群

EMQX Operator 与适用的 EMQX 版本的对应关系如下：

|         EMQX  版本      |                         EMQX Operator  版本   |  APIVersion |
|:----------------------:|:---------------------------------------------:|:-----------:|
|   4.4.6<= EMQX < 4.4.8 |   1.2.5                                       |  v1beta3    |
|   4.4.8 <= EMQX        | 1.2.6，1.2.7，1.2.8，2.0.0，2.0.1，2.0.2（推荐） |  v1beta3    |


关于 EMQX Operator 最新信息，请访问 [EMQX Operator](https://github.com/emqx/emqx-operator/blob/main/docs/zh_CN/getting-started/getting-started.md)。**注意**: EMQX Operator 需要 Kubernetes v1.20.11 或者以上。

## 部署 EMQX Operator 

### 准备

我们使用 [cert-manager](https://github.com/cert-manager/cert-manager) 给 EMQX Operator webhook 服务提供证书。cert-manager 的安装教程可以参考文档：[cert-manager 文档](https://cert-manager.io/docs/installation/)。

### 安装 

1. 安装 EMQX Operator 

- 添加 EMQX helm chart 仓库

```bash
helm repo add emqx https://repos.emqx.io/charts
helm repo update emqx 
```

- 安装 EMQX Operator 

```bash
helm install emqx-operator emqx/emqx-operator --namespace emqx-operator-system --create-namespace --version=2.0.2
```

**说明**： `--version` 用于安装指定版本的 EMQX Operator, 不指定 version 的时默认安装最新版本。请根据 EMQX Operator 与适用的 EMQX 版本对应关系表安装适合的 EMQX Operator 版本。

2. 检查 EMQX Operator 是否就绪

```bash
kubectl get pods -l "control-plane=controller-manager" -n emqx-operator-system
```

输出类似于：

```
NAME                                                READY   STATUS    RESTARTS   AGE
emqx-operator-controller-manager-68b866c8bf-kd4g6   1/1     Running   0          15s
```

**说明**：`NAME` 字段是 EMQX Operator Pod 的名称，`READY` 字段表示 Pod 中就绪的 Container 数量。 `STATUS` 字段是 Pod 的状态。

{% emqxce %}

## 部署 EMQX Broker

1. 部署 EMQX 自定义资源

```bash
cat << "EOF" | kubectl apply -f -
  apiVersion: apps.emqx.io/v1beta3
  kind: EmqxBroker
  metadata:
    name: emqx
  spec:
    emqxTemplate:
      image: emqx/emqx:4.4.9
EOF
```

2. 检查 EMQX 集群是否就绪

```bash
kubectl get emqxbroker emqx -o json | jq ".status.conditions"
```

输出类似于：

```bash
[
  {
    "lastTransitionTime": "2022-12-21T11:59:47Z",
    "lastUpdateTime": "2022-12-21T12:00:28Z",
    "message": "All resources are ready",
    "reason": "ClusterReady",
    "status": "True",
    "type": "Running"
  },
  {
    "lastTransitionTime": "2022-12-21T11:59:34Z",
    "lastUpdateTime": "2022-12-21T11:59:34Z",
    "message": "All default plugins initialized",
    "reason": "PluginInitializeSuccessfully",
    "status": "True",
    "type": "PluginInitialized"
  }
]
```
**说明**：从输出结果可以看到 EMQX 集群已经准备就绪。

{% endemqxce %}

{% emqxee %}

## 部署 EMQX Enterprise 

1. 部署 EMQX 自定义资源

```bash
cat << "EOF" | kubectl apply -f -
  apiVersion: apps.emqx.io/v1beta3
  kind: EmqxEnterprise
  metadata:
    name: emqx-ee
  spec:
    emqxTemplate:
      image: emqx/emqx-ee:4.4.9
EOF
```

2. 检查 EMQX 集群是否就绪

```bash
kubectl get emqxenterprise emqx-ee -o json | jq ".status.conditions"
```

输出类似于：

```bash
[
  {
    "lastTransitionTime": "2022-12-21T12:03:24Z",
    "lastUpdateTime": "2022-12-21T12:03:24Z",
    "message": "All resources are ready",
    "reason": "ClusterReady",
    "status": "True",
    "type": "Running"
  },
  {
    "lastTransitionTime": "2022-12-21T12:03:08Z",
    "lastUpdateTime": "2022-12-21T12:03:08Z",
    "message": "All default plugins initialized",
    "reason": "PluginInitializeSuccessfully",
    "status": "True",
    "type": "PluginInitialized"
  }
]
```

**说明**：从输出结果可以看到 EMQX 集群已经准备就绪。

{% endemqxee %}