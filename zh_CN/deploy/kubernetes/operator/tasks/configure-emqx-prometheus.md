# 使用 Prometheus 和 Grafana 监控 EMQX 集群

## 目标

部署 [EMQX Exporter](https://github.com/emqx/emqx-exporter) 并使用 Prometheus 和 Grafana 监控 EMQX 集群。

## 部署 Prometheus 和 Grafana

* 要了解更多关于 Prometheus 部署的信息，请参阅 [Prometheus](https://github.com/prometheus-operator/prometheus-operator) 文档。
* 要了解更多关于 Grafana 部署的信息，请参阅 [Grafana](https://grafana.com/docs/grafana/latest/setup-grafana/installation/kubernetes/) 文档。

## 部署 EMQX 集群

1. EMQX 通过 [Prometheus 兼容的 HTTP API](../../../../observability/prometheus.md) 暴露各种指标。

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

2. 将上述内容保存为 `emqx.yaml` 并执行以下命令部署 EMQX 集群：

   ```bash
   $ kubectl apply -f emqx.yaml
   emqx.apps.emqx.io/emqx created
   ```

3. 检查 EMQX 集群的状态，并确保 `STATUS` 为 `Ready`。这可能需要一些时间。

   ```bash
   $ kubectl get emqx emqx
   NAME   STATUS   AGE
   emqx   Ready    10m
   ```

## 创建 API Secret

Prometheus 将从 EMQX Dashboard API 拉取指标，因此您需要登录 Dashboard [创建 API Secret](../../../../dashboard/system.md#api-密钥)。

## 部署 [EMQX Exporter](https://github.com/emqx/emqx-exporter)

`emqx-exporter` 的设计目的是暴露 EMQX Prometheus API 中未暴露的部分指标。

```yaml
apiVersion: v1
kind: Service
metadata:
  labels:
    app: emqx-exporter
  name: emqx-exporter-service
spec:
  ports:
    - name: metrics
      port: 8085
      targetPort: metrics
  selector:
    app: emqx-exporter
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: emqx-exporter
  labels:
    app: emqx-exporter
spec:
  selector:
    matchLabels:
      app: emqx-exporter
  replicas: 1
  template:
    metadata:
      labels:
        app: emqx-exporter
    spec:
      securityContext:
        runAsUser: 1000
      containers:
        - name: exporter
          image: emqx-exporter:latest
          imagePullPolicy: IfNotPresent
          args:
            # "emqx-dashboard-service-name" 是 Operator 创建的用于暴露 18083 端口的服务名称
            - --emqx.nodes=${emqx-dashboard-service-name}:18083
            - --emqx.auth-username=${paste_your_new_api_key_here}
            - --emqx.auth-password=${paste_your_new_secret_here}
          securityContext:
            allowPrivilegeEscalation: false
            runAsNonRoot: true
          ports:
            - containerPort: 8085
              name: metrics
              protocol: TCP
          resources:
            limits:
              cpu: 100m
              memory: 100Mi
            requests:
              cpu: 100m
              memory: 20Mi
```

> 将参数 "--emqx.nodes" 设置为 Operator 创建的用于暴露 18083 端口的服务名称。通过调用 `kubectl get svc` 查找服务名称。

将上述内容保存为 `emqx-exporter.yaml`，替换 `--emqx.auth-username` 和 `--emqx.auth-password` 为您的新 API Secret。运行以下命令部署 `emqx-exporter`：

```bash
kubectl apply -f emqx-exporter.yaml
```

检查 `emqx-exporter` Pod 的状态。

```bash
$ kubectl get po -l="app=emqx-exporter"
NAME                            STATUS   AGE
emqx-exporter-856564c95-j4q5v   Running  8m33s
```

## 配置 Prometheus Monitor

Prometheus Operator 使用 [PodMonitor](https://github.com/prometheus-operator/prometheus-operator/blob/main/Documentation/getting-started/design.md#podmonitor) 和 [ServiceMonitor](https://github.com/prometheus-operator/prometheus-operator/blob/main/Documentation/getting-started/design.md#servicemonitor) CRD 来定义如何动态监控一组 Pod 或服务。

```yaml
apiVersion: monitoring.coreos.com/v1
kind: PodMonitor
metadata:
  name: emqx
  labels:
    app.kubernetes.io/name: emqx
spec:
  podMetricsEndpoints:
    - interval: 5s
      path: /api/v5/prometheus/stats
      # emqx dashboard containerPort 的名称
      port: dashboard
      relabelings:
        - action: replace
          # 用户定义的集群名称，需要唯一
          replacement: emqx5
          targetLabel: cluster
        - action: replace
          # 固定值，请勿修改
          replacement: emqx
          targetLabel: from
        - action: replace
          # 固定值，请勿修改
          sourceLabels: ['pod']
          targetLabel: "instance"
  selector:
    matchLabels:
      # 标签与 emqx pod 的标签相同
      apps.emqx.io/instance: emqx
      apps.emqx.io/managed-by: emqx-operator
  namespaceSelector:
    matchNames:
      # 如果您的 EMQX 集群部署在其他命名空间中，请修改命名空间
      #- default
---
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: emqx-exporter
  labels:
    app: emqx-exporter
spec:
  selector:
    matchLabels:
      # 标签与 emqx exporter svc 的标签相同
      app: emqx-exporter
  endpoints:
    - port: metrics
      interval: 5s
      path: /metrics
      relabelings:
        - action: replace
          # 用户定义的集群名称，需要唯一
          replacement: emqx5
          targetLabel: cluster
        - action: replace
          # 固定值，请勿修改
          replacement: exporter
          targetLabel: from
        - action: replace
          # 固定值，请勿修改
          sourceLabels: ['pod']
          regex: '(.*)-.*-.*'
          replacement: $1
          targetLabel: "instance"
        - action: labeldrop
          # 固定值，请勿修改
          regex: 'pod'
  namespaceSelector:
    matchNames:
      # 如果您的 exporter 部署在其他命名空间中，请修改命名空间
      #- default
```

`path` 表示指标采集接口的路径。在 EMQX 5 中，路径为：`/api/v5/prometheus/stats`。`selector.matchLabels` 表示匹配 Pod 的标签：`apps.emqx.io/instance: emqx`。

targetLabel `cluster` 的值表示当前集群的名称。请确保它是唯一的。

将上述内容保存为 `monitor.yaml` 并执行以下命令：

```bash
$ kubectl apply -f monitor.yaml
```

## 在 Prometheus 上查看 EMQX 指标

打开 Prometheus 界面，切换到 Graph 页面，输入 `emqx` 显示如下图所示：

![](./assets/configure-emqx-prometheus/emqx-prometheus-metrics.png)

切换到 **Status** → **Targets** 页面，显示如下图所示，您可以看到集群中所有被监控的 EMQX Pod 信息：

![](./assets/configure-emqx-prometheus/emqx-prometheus-target.png)

## 导入 Grafana 模板

导入所有 dashboard [模板](https://github.com/emqx/emqx-exporter/tree/main/grafana-dashboard/template)。打开主 dashboard **EMQX** 并开始使用！

![](./assets/configure-emqx-prometheus/emqx-grafana-dashboard.png)
