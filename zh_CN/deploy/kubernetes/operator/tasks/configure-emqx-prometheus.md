# 使用 Prometheus 和 Grafana 监控 EMQX

## 目标

配置 Prometheus 抓取 EMQX 集群指标，并在 Grafana 中将指标可视化。

## 部署 Prometheus 和 Grafana

* 有关 Prometheus 部署的详情，请参见 [Prometheus](https://github.com/prometheus-operator/prometheus-operator) 文档。
* 有关 Grafana 部署的详情，请参见 [Grafana](https://grafana.com/docs/grafana/latest/setup-grafana/installation/kubernetes/) 文档。

## 部署 EMQX 集群

EMQX 通过[兼容 Prometheus 的 HTTP API](../../../../observability/prometheus.md) 公开多种指标。

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
```

将以上内容保存为 `emqx.yaml`，然后运行以下命令部署 EMQX 集群：

```bash
$ kubectl apply -f emqx.yaml
emqx.apps.emqx.io/emqx created
```

检查 EMQX 集群状态，并确保 `STATUS` 为 `Ready`。此过程可能需要一些时间。

```bash
$ kubectl get emqx emqx
NAME   STATUS   AGE
emqx   Ready    10m
```

## 创建 API 密钥

登录 Dashboard，并[创建专用 API 密钥](../../../../dashboard/system.md#api-keys)。为 Prometheus 创建 API 密钥时，请选择查看者角色，并仅授予 `monitoring` 权限范围。`PodMonitor` 将使用该密钥抓取 `/api/v5/prometheus/stats`。

请妥善保存 API Key 和 Secret Key。EMQX 仅显示 Secret Key 一次。

## 配置 Prometheus 监控

Prometheus Operator 使用 [PodMonitor](https://prometheus-operator.dev/docs/developer/getting-started/#using-podmonitors) CRD 选择 Pod 并定义抓取端点。EMQX 通过 Dashboard 监听器公开 Prometheus 指标，该监听器的容器端口默认名为 `dashboard`。

以下 PodMonitor 从 `emqx` 集群的每个 Pod 抓取 EMQX 基础指标端点：

从 EMQX 6.3.0 开始，Prometheus 抓取 API 默认要求身份验证。请在 `PodMonitor` 所在的命名空间中创建 Kubernetes Secret，用于存储为 Prometheus 创建的 API Key 和 Secret Key：

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
      # EMQX Dashboard 容器端口的名称。
      port: dashboard
      relabelings:
        - action: replace
          # 每个 EMQX 集群应使用不同的值。
          replacement: emqx5
          targetLabel: cluster
        - action: replace
          # 请勿修改此值。
          replacement: emqx
          targetLabel: from
        - action: replace
          # 使用 Pod 名称作为 Prometheus instance 标签。
          sourceLabels: [pod]
          targetLabel: instance
  selector:
    matchLabels:
      # 匹配由 EMQX Operator 为名为 `emqx` 的 EMQX 资源管理的 Pod。
      apps.emqx.io/instance: emqx
      apps.emqx.io/managed-by: emqx-operator
  namespaceSelector:
    matchNames:
      # 如果 EMQX 集群位于其他命名空间，请修改此值。
      - default
```

`path` 指定指标采集 API 路径。对于 EMQX 5.0 及更高版本，请使用 `/api/v5/prometheus/stats`。`basicAuth` 部分从 Kubernetes Secret 中读取 API Key 和 Secret Key。selector 用于匹配由 Operator 为 `emqx` 资源管理的 Pod。同一个 Prometheus 服务器所监控的每个 EMQX 集群必须使用唯一的 `cluster` 目标标签。

如果明确设置 `prometheus.enable_basic_auth = false` 以禁用身份验证，可以从 `podMetricsEndpoints` 中省略 `basicAuth`。有关所有可用端点和身份验证选项，请参见[集成 Prometheus](../../../../observability/prometheus.md#configure-pull-mode-integration)。

将以上内容保存为 `monitor.yaml`，然后运行以下命令：

```bash
$ kubectl apply -f monitor.yaml
```

## 在 Prometheus 中查看 EMQX 指标

打开 Prometheus 表达式浏览器并输入 `emqx`，即可查看 EMQX 指标，如下图所示：

![](./assets/configure-emqx-prometheus/emqx-prometheus-metrics.png)

打开 **Status** -> **Targets**，查看集群中所有受监控的 EMQX Pod：

![](./assets/configure-emqx-prometheus/emqx-prometheus-target.png)

## 导入 Grafana Dashboard

导入 [EMQX Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/)，并选择用于抓取 EMQX Pod 指标的 Prometheus 数据源。

![](./assets/configure-emqx-prometheus/emqx-grafana-dashboard.png)
