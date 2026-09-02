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

## 创建 API 密钥

登录 Dashboard 并[创建两个专用 API 密钥](../../../../dashboard/system.md#api-密钥)：

- 为 EMQX Exporter 创建具有查看者角色的 API 密钥，并保留默认 scope 配置。除 Prometheus 抓取 API 外，EMQX Exporter 还会读取多个管理 API。
- 为 Prometheus 创建具有查看者角色且仅包含 `monitoring` scope 的 API 密钥。`PodMonitor` 使用该密钥抓取 `/api/v5/prometheus/stats`。

分别保存用于这两个集成的 API Key 和 Secret Key。每个 Secret Key 仅显示一次。

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

将上述内容保存为 `emqx-exporter.yaml`。将 `--emqx.auth-username` 设置为 EMQX Exporter 的 API Key，将 `--emqx.auth-password` 设置为对应的 Secret Key。运行以下命令部署 `emqx-exporter`：

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

从 EMQX 6.3.0 开始，Prometheus 抓取 API 默认要求身份认证。在 `PodMonitor` 所在的命名空间中创建 Kubernetes Secret，用于保存为 Prometheus 创建的 API Key 和 Secret Key：

```bash
kubectl create secret generic emqx-prometheus-basic-auth \
  --from-literal=username='<API_KEY>' \
  --from-literal=password='<SECRET_KEY>'
```

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
      basicAuth:
        username:
          name: emqx-prometheus-basic-auth
          key: username
        password:
          name: emqx-prometheus-basic-auth
          key: password
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

`path` 指定指标采集 API 的路径。EMQX 5.0 及后续版本使用 `/api/v5/prometheus/stats`。`basicAuth` 从 Kubernetes Secret 中读取 API Key 和 Secret Key。`selector.matchLabels` 通过 `apps.emqx.io/instance: emqx` 标签匹配 EMQX Pod。

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
