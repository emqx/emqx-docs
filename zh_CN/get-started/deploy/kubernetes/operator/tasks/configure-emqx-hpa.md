# 使用 HPA 自动伸缩 Replicant 节点

使用 Kubernetes HorizontalPodAutoscaler（HPA）自动伸缩 EMQX Replicant 节点。EMQX Operator 3.0 为 `apps.emqx.io/v3beta1` EMQX 资源提供 Kubernetes `scale` 子资源。该子资源以 `.spec.replicantTemplate.spec.replicas` 为目标，因此 HPA 可以伸缩 Core-Replicant 集群中的 Replicant 节点。

## 准备工作

确保 Kubernetes 资源指标 API 可用。本页中的 CPU 利用率示例需要使用 [Metrics Server](https://kubernetes-sigs.github.io/metrics-server/)，或其他可以提供 `metrics.k8s.io` 数据的指标适配器。

运行以下命令，验证是否可以获取 Pod 指标：

```bash
kubectl top pods
```

如果该命令未返回 CPU 和内存使用情况，请先安装并配置指标提供程序，再创建 HPA。

启用 Replicant 节点时，请至少配置两个 Core 副本。还必须配置 Pod 资源请求，HPA 才能分析利用率并做出伸缩决策。

## 部署 EMQX

1. 将以下内容保存为 `emqx.yaml`：

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
           key: "..."
     coreTemplate:
       spec:
         replicas: 2
     replicantTemplate:
       spec:
         replicas: 4
         resources:
           requests:
             cpu: 500m
             memory: 1Gi
   ```

2. 部署 EMQX 集群：

   ```bash
   kubectl apply -f emqx.yaml
   ```

3. 等待 EMQX 集群就绪：

   ```bash
   kubectl wait --for=condition=Ready emqx/emqx
   ```

## 创建 HPA

1. 将以下内容保存为 `emqx-hpa.yaml`。本示例根据 CPU 利用率，在 2 到 10 个副本之间伸缩 Replicant 节点：

   ```yaml
   apiVersion: autoscaling/v2
   kind: HorizontalPodAutoscaler
   metadata:
     name: emqx-replicants
   spec:
     scaleTargetRef:
       apiVersion: apps.emqx.io/v3beta1
       kind: EMQX
       name: emqx
     minReplicas: 2
     maxReplicas: 10
     metrics:
       - type: Resource
         resource:
           name: cpu
           target:
             type: Utilization
             averageUtilization: 70
     behavior:
       scaleDown:
         stabilizationWindowSeconds: 300
   ```

2. 应用 HPA：

   ```bash
   kubectl apply -f emqx-hpa.yaml
   ```

## 验证自动伸缩

1. 检查 HPA 和 EMQX 状态：

   ```bash
   kubectl get hpa emqx-replicants
   kubectl get emqx emqx -o custom-columns='DESIRED:.spec.replicantTemplate.spec.replicas,CURRENT:.status.replicantReplicas'
   ```

2. 在没有 MQTT 客户端负载的情况下，监控 HPA。如果观测到的 CPU 利用率足够低，使 HPA 计算出的目标副本数为 2，请验证 HPA 是否将 `.spec.replicantTemplate.spec.replicas` 更新为 2。配置的缩容稳定窗口可能会延迟此次更新。随后，EMQX Operator 会将 Replicant 节点数量协调为该副本数。

缩容 Replicant 节点可能触发 MQTT 连接和会话疏散，从而增加集群负载。请使用 HPA 稳定窗口和保守的伸缩策略，避免工作负载波动时频繁改变副本数。
