# 在 Azure Kubernetes Service 中部署 EMQX

EMQX Operator 支持在 Azure Kubernetes Service (AKS) 上部署 EMQX。AKS 通过将运维开销转移到 Azure 来简化在 Azure 中部署托管 Kubernetes 集群的过程。作为托管的 Kubernetes 服务，Azure 处理关键任务，如健康监控和维护。创建 AKS 集群时，Azure 会自动配置和管理 Kubernetes 控制平面，无需额外费用。

## 前提条件

在 AKS 上部署 EMQX 之前，请确保满足以下先决条件：

- Azure 订阅中的 AKS 集群
  - 有关创建和配置 AKS 集群的指导，请参阅 [Azure Kubernetes Service 文档](https://learn.microsoft.com/zh-cn/azure/aks/)。

- 用于连接到 AKS 集群的有效 `kubectl` 配置
  - 要使用本地安装的 `kubectl` 连接，请按照 [连接到 AKS 集群](https://learn.microsoft.com/zh-cn/azure/aks/learn/quick-kubernetes-deploy-cli) 中的说明操作。
  - 要使用 Azure Cloud Shell 连接，请参阅 [在 Azure CloudShell 中管理 AKS 集群](https://learn.microsoft.com/zh-cn/azure/aks/learn/quick-kubernetes-deploy-portal?tabs=azure-cli)。

- 在集群上安装 EMQX Operator
  - 有关安装详细信息，请参阅 [安装 EMQX Operator](./getting-started.md)。


## 快速部署 EMQX 集群

以下示例显示了 EMQX 自定义资源 (CR) 的基本配置。

1. 将其保存为 YAML 文件，并使用 `kubectl apply` 部署。

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
           ## 有关存储类的更多信息：https://learn.microsoft.com/zh-cn/azure/aks/concepts-storage#storage-classes
           storageClassName: default
           resources:
             requests:
               storage: 10Gi
           accessModes:
           - ReadWriteOnce
     dashboardServiceTemplate:
       spec:
         ## 有关负载均衡器的更多信息：https://learn.microsoft.com/zh-cn/azure/aks/load-balancer-standard
         type: LoadBalancer
     listenersServiceTemplate:
       spec:
         ## 有关负载均衡器的更多信息：https://learn.microsoft.com/zh-cn/azure/aks/load-balancer-standard
         type: LoadBalancer
   ```

2. 等待 EMQX 集群就绪。

   使用 `kubectl get` 检查集群状态，并验证 `STATUS` 为 `Ready`。启动可能需要一些时间。

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     1m5s
   ```

3. 获取 EMQX Dashboard 的外部 IP 并访问它。

   EMQX Operator 会根据 `dashboardServiceTemplate` 配置自动创建 Service。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   20.245.230.91
   ```

4. 在 `http://20.245.230.91:18083` 打开 Dashboard。

    使用默认凭据登录：

     - **用户名：** `admin`
     - **密码：** `public`

## 使用 MQTTX 订阅和发布

本演练使用 [MQTTX CLI](https://mqttx.app/zh/cli)，这是一款开源的 MQTT 5.0 命令行客户端工具，可帮助开发者快速测试 MQTT 服务和应用。

1. 获取 EMQX TCP 监听器的外部 IP。

   EMQX Operator 会为每个配置的监听器自动创建 Service 资源。

   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. 订阅主题。

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   [10:00:25] › …  Connecting...
   [10:00:25] › ✔  Connected
   [10:00:25] › …  Subscribing to hello...
   [10:00:25] › ✔  Subscribed to hello
   ```

3. 在另一个终端中，连接到 EMQX 集群并发布消息。

   ```shell
   $ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'
   [10:00:58] › …  Connecting...
   [10:00:58] › ✔  Connected
   [10:00:58] › …  Message Publishing...
   [10:00:58] › ✔  Message published
   ```

4. 观察订阅者接收消息。

   ```shell
   [10:00:58] › payload: hello world
   ```

## 关于使用 LoadBalancer 进行 TLS 卸载的说明

作为 L3/L4 负载均衡器，Azure LoadBalancer 不支持 TLS 终止。请参阅此[讨论](https://github.com/emqx/emqx-operator/discussions/312)以了解可能的解决方案。
