# 在 Google Kubernetes Engine 中部署 EMQX

EMQX Operator 允许在 Google Kubernetes Engine (GKE) 上部署 EMQX，这简化了在 GCP 中部署托管 Kubernetes 集群的过程。使用 GKE，您可以将运维开销转移到 GCP。通过在 GKE 上部署 EMQX，您可以利用 Kubernetes 的可扩展性和灵活性，同时受益于托管服务的简单性和便利性。使用 GKE 上的 EMQX Operator，您可以轻松地在云中部署和管理 MQTT 代理，并专注于您的业务目标。

## 前提条件

在 GKE 上部署 EMQX 之前，请确保满足以下先决条件：

- Google Cloud Platform 上的 GKE 集群
  - 您必须在项目中启用 GKE API。有关设置说明，请参阅 [Google Kubernetes Engine 文档](https://cloud.google.com/kubernetes-engine/)。

- 用于连接到 GKE 集群的有效 `kubectl` 配置
  - 要使用本地 `kubectl` 安装连接，请参阅 [连接到 GKE 集群](https://cloud.google.com/kubernetes-engine/docs/how-to/cluster-access-for-kubectl)。
  - 要直接从 GCP 控制台使用 Cloud Shell 连接，请参阅 [使用 Cloud Shell 管理 GKE 集群](https://cloud.google.com/code/docs/shell/create-configure-gke-cluster)。
  
- 在集群上安装 EMQX Operator
  - 有关更多详细信息，请参阅 [安装 EMQX Operator](./getting-started.md)。

  ::: warning 注意

  在 GKE 上使用默认设置安装 cert-manager 可能会导致引导问题。添加配置 `--set global.leaderElection.namespace=cert-manager` 以在领导者选举中使用不同的命名空间。有关详细信息，请参阅 [cert-manager 兼容性文档](https://cert-manager.io/docs/installation/compatibility/)。

  :::

## 快速部署 EMQX 集群

以下示例显示了基本的 EMQX 自定义资源 (CR) 配置。

1. 将以下文档保存为 YAML 文件，并使用 `kubectl apply` 部署。

    ::: warning 注意

    如果指定 CPU 和内存限制，请确保至少 250m CPU 和 512Mi 内存。有关详细信息，请参阅 [Autopilot 中的资源请求](https://cloud.google.com/kubernetes-engine/docs/concepts/autopilot-resource-requests)。

    :::

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
         ## 有关存储类的更多信息：https://cloud.google.com/kubernetes-engine/docs/concepts/persistent-volumes#storageclasses
           storageClassName: standard
           resources:
             requests:
               storage: 10Gi
           accessModes:
           - ReadWriteOnce
     dashboardServiceTemplate:
       spec:
         ## 有关负载均衡器的更多信息：https://cloud.google.com/kubernetes-engine/docs/how-to/internal-load-balancing
         type: LoadBalancer
     listenersServiceTemplate:
       spec:
         ## 有关负载均衡器的更多信息：https://cloud.google.com/kubernetes-engine/docs/how-to/internal-load-balancing
         type: LoadBalancer
   ```

2. 等待 EMQX 集群就绪。

   使用 `kubectl get` 检查 EMQX 集群的状态，并确保 `STATUS` 为 `Ready`。这可能需要一些时间。

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     1m2s
   ```

3. 获取 EMQX Dashboard 的外部 IP。

   EMQX Operator 会根据 `dashboardServiceTemplate` 配置创建 Service 资源。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   34.122.174.166
   ```

4. 在 `http://34.122.174.166:18083` 打开 Dashboard。

   使用默认凭据登录：

    - **用户名：** `admin`
    - **密码：** `public`

## 订阅和发布

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

3. 在单独的终端中，连接到 EMQX 集群并发布消息。

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

在撰写本文时，Google LoadBalancer 不支持 TLS 到纯 TCP 流量的终止。请参阅此[讨论](https://github.com/emqx/emqx-operator/discussions/312)以了解可能的解决方案。
