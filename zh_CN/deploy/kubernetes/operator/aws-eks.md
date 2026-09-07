# 在 Amazon Elastic Kubernetes Service 上部署 EMQX

EMQX Operator 支持在 Amazon EKS（Elastic Kubernetes Service）上运行。Amazon EKS 是一项托管 Kubernetes 服务，可简化容器化应用的部署、管理和伸缩。EKS 提供 Kubernetes 控制平面和节点组，并自动处理节点替换、升级和补丁。它支持负载均衡器、RDS 和 IAM 等 AWS 服务，并可与 Kubernetes 生态系统中的其他工具集成。

有关详细介绍，请参阅[什么是 Amazon EKS](https://docs.aws.amazon.com/zh_cn/eks/latest/userguide/what-is-eks.html)。

## 准备工作

在 EKS 上部署 EMQX 前，请完成以下准备工作：

- 创建 EKS 集群。<br/>详情请参阅[创建 Amazon EKS 集群](https://docs.aws.amazon.com/zh_cn/eks/latest/userguide/getting-started.html)。

- 配置 kubectl 以连接 EKS 集群。<br/>详情请参阅[使用 kubectl 连接到集群](https://docs.aws.amazon.com/zh_cn/eks/latest/userguide/getting-started-console.html#eks-configure-kubectl)。

- 在集群中部署 AWS Load Balancer Controller。<br/>详情请参阅[创建网络负载均衡器](https://docs.aws.amazon.com/zh_cn/eks/latest/userguide/network-load-balancing.html)。

- 在集群中安装 Amazon EBS CSI 驱动程序。<br/>详情请参阅 [Amazon EBS CSI 驱动程序](https://docs.aws.amazon.com/zh_cn/eks/latest/userguide/ebs-csi.html)。

- 安装 EMQX Operator。<br/>详情请参阅[安装 EMQX Operator](./getting-started.md)。

## 快速部署 EMQX 集群

以下示例演示在 EKS 上部署 EMQX 所需的自定义资源（CR）配置。

1. 将以下内容保存为 YAML 文件，并使用 `kubectl apply` 部署。

   ```yaml
   # 使用 WaitForFirstConsumer 绑定模式配置 EBS StorageClass
   # 确保卷与使用它们的 Pod 创建在同一可用区
   apiVersion: storage.k8s.io/v1
   kind: StorageClass
   metadata:
     name: ebs-sc
   provisioner: ebs.csi.aws.com
   volumeBindingMode: WaitForFirstConsumer
   ---
   apiVersion: apps.emqx.io/v3beta1
   kind: EMQX
   metadata:
     name: emqx
   spec:
     image: emqx/emqx:@EE_VERSION@
     config:
       roots:
         license:
           key: "..."
     coreTemplate:
       spec:
         ## EMQX 自定义资源不支持在运行时更新此字段
         persistentVolumeClaimSpec:
           storageClassName: ebs-sc
           resources:
             requests:
               storage: 10Gi
           accessModes:
             - ReadWriteOnce
     dashboardServiceTemplate:
       metadata:
         ## 更多信息：https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/annotations/
         annotations:
           ## 指定 NLB 面向互联网还是内部网络。未指定时默认为内部网络。
           service.beta.kubernetes.io/aws-load-balancer-type: external
           service.beta.kubernetes.io/aws-load-balancer-scheme: internet-facing
       spec:
         type: LoadBalancer
         ## 更多信息：https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/nlb/
         loadBalancerClass: service.k8s.aws/nlb
     listenersServiceTemplate:
       metadata:
         ## 更多信息：https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/annotations/
         annotations:
           ## 指定 NLB 面向互联网还是内部网络。未指定时默认为内部网络。
           service.beta.kubernetes.io/aws-load-balancer-type: external
           service.beta.kubernetes.io/aws-load-balancer-scheme: internet-facing
       spec:
         type: LoadBalancer
         ## 更多信息：https://kubernetes-sigs.github.io/aws-load-balancer-controller/v2.4/guide/service/nlb/
         loadBalancerClass: service.k8s.aws/nlb
   ```

2. 等待 EMQX 集群就绪。

   使用以下命令检查状态。`STATUS` 字段必须显示为 `Ready`，此过程可能需要几分钟：

   ```shell
   $ kubectl get emqx
   NAME   STATUS    AGE
   emqx   Ready     55s
   ```

3. 获取 EMQX Dashboard 的外部 IP 地址并进行访问。

   EMQX Operator 根据 `dashboardServiceTemplate` 配置为 EMQX Dashboard 创建 Service。

   ```shell
   $ kubectl get svc emqx-dashboard -o json | jq -r '.status.loadBalancer.ingress[0].ip'
   192.168.1.200
   ```

4. 通过 `http://192.168.1.200:18083` 打开 Dashboard。

   使用默认凭据登录：

     - **用户名：** `admin`
     - **密码：** `public`

## 订阅和发布消息

本示例使用 [MQTTX CLI](https://mqttx.app/cli)。这是一款开源 MQTT 5.0 命令行客户端工具，可帮助开发者快速测试 MQTT 服务和应用。

1. 获取 EMQX TCP 监听器的外部 IP 地址。

   EMQX Operator 会为每个已配置的监听器自动创建 Service 资源。

   ```shell
   external_ip=$(kubectl get svc emqx-listeners -o json | jq -r '.status.loadBalancer.ingress[0].ip')
   ```

2. 订阅主题。

   ```shell
   $ mqttx sub -t 'hello' -h ${external_ip} -p 1883
   
   [10:00:25] › … Connecting...
   [10:00:25] › ✔ Connected
   [10:00:25] › … Subscribing to hello...
   [10:00:25] › ✔ Subscribed to hello
   ```

3. 在另一个终端中连接 EMQX 集群并发布消息。

   ```shell
   $ mqttx pub -t 'hello' -h ${external_ip} -p 1883 -m 'hello world'
   
   [10:00:58] › … Connecting...
   [10:00:58] › ✔ Connected
   [10:00:58] › … Message Publishing...
   [10:00:58] › ✔ Message published
   ```

4. 确认订阅端收到消息。

   ```shell
   [10:00:58] › payload: hello world
   ```

## 使用 LoadBalancer 终止 TLS 加密

可以使用 AWS Network Load Balancer（NLB）终止发往 EMQX 的 TLS 流量。操作步骤如下：

1. 在 [AWS 控制台](https://us-east-2.console.aws.amazon.com/acm/home)中导入相关证书。点击证书 ID 打开证书详情页面，并记录证书 ARN。

    ::: tip
有关证书和密钥的导入格式，请参阅[导入证书](https://docs.aws.amazon.com/zh_cn/acm/latest/userguide/import-certificate-format.html)。
    :::

2. 在 EMQX Service 元数据中添加注解，例如：

    ```yaml
    ## 指定 AWS Certificate Manager 管理的一个或多个证书的 ARN。
    service.beta.kubernetes.io/aws-load-balancer-ssl-cert: arn:aws:acm:us-west-2:xxxxx:certificate/xxxxxxx
    ## 指定负载均衡器与 Kubernetes Pod 之间的后端流量是否使用 TLS。
    service.beta.kubernetes.io/aws-load-balancer-backend-protocol: tcp
    ## 指定使用 TLS 监听器的前端端口。通过 AWS NLB Service 访问端口 1883 时需要 TLS 认证，
    ## 但直接访问 Kubernetes Service 端口时不需要 TLS 认证。
    service.beta.kubernetes.io/aws-load-balancer-ssl-ports: "1883"
    ```

    ::: tip
    `service.beta.kubernetes.io/aws-load-balancer-ssl-cert` 的值应与步骤 1 中记录的 ARN 一致。
    :::
