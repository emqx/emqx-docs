# Monitor EMQX Cluster by Prometheus and Grafana

## Objective

Deploy [EMQX Exporter](https://github.com/emqx/emqx-exporter) and monitor an EMQX cluster using Prometheus and Grafana.

## Deploy Prometheus and Grafana

* To learn more about Prometheus deployment, refer to the [Prometheus](https://github.com/prometheus-operator/prometheus-operator) documentation.
* To learn more about Grafana deployment, refer to [Grafana](https://grafana.com/docs/grafana/latest/setup-grafana/installation/kubernetes/) documentation.

## Deploy EMQX Cluster

EMQX exposes various metrics through the [Prometheus-compatible HTTP API](../../../../observability/prometheus.md).

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
```

Save the above content as `emqx.yaml` and execute the following command to deploy the EMQX cluster:

```bash
$ kubectl apply -f emqx.yaml
emqx.apps.emqx.io/emqx created
```

Check the status of the EMQX cluster and make sure that `STATUS` is `Ready`. This may take some time.

```bash
$ kubectl get emqx emqx
NAME   STATUS   AGE
emqx   Ready    10m
```

## Create API secret

Prometheus is going to pull metrics from the EMQX Dashboard API, so you need to sign in to the Dashboard to [create an API secret](../../../../dashboard/system.md#api-keys).

## Deploy [EMQX Exporter](https://github.com/emqx/emqx-exporter)

The `emqx-exporter` is designed to expose partial metrics that are not exposed in the EMQX Prometheus API.

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
            # "emqx-dashboard-service-name" is the service name that creating by operator for exposing 18083 port
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

> Set the arg "--emqx.nodes" to the service name that creating by operator for exposing 18083 port. Look up the service name by calling `kubectl get svc`.

Save the above content as `emqx-exporter.yaml`, replacing `--emqx.auth-username` and `--emqx.auth-password` with your new API secret. Run the following command to deploy the `emqx-exporter`:

```bash
kubectl apply -f emqx-exporter.yaml
```

Check the status of the `emqx-exporter` pod.
```bash
$ kubectl get po -l="app=emqx-exporter"
NAME                            STATUS   AGE
emqx-exporter-856564c95-j4q5v   Running  8m33s
```

## Configure Prometheus Monitor

Prometheus Operator uses [PodMonitor](https://github.com/prometheus-operator/prometheus-operator/blob/main/Documentation/design.md#podmonitor) and [ServiceMonitor](https://github.com/prometheus-operator/prometheus-operator/blob/main/Documentation/design.md#servicemonitor) CRDs to define how to monitor a set of pods or services dynamically.

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
      # the name of emqx dashboard containerPort
      port: dashboard
      relabelings:
        - action: replace
          # user-defined cluster name, requires unique
          replacement: emqx5
          targetLabel: cluster
        - action: replace
          # fix value, don't modify
          replacement: emqx
          targetLabel: from
        - action: replace
          # fix value, don't modify
          sourceLabels: ['pod']
          targetLabel: "instance"
  selector:
    matchLabels:
      # the label is the same as the label of emqx pod
      apps.emqx.io/instance: emqx
      apps.emqx.io/managed-by: emqx-operator
  namespaceSelector:
    matchNames:
      # modify the namespace if your EMQX cluster deployed in other namespace
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
      # the label is the same as the label of emqx exporter svc
      app: emqx-exporter
  endpoints:
    - port: metrics
      interval: 5s
      path: /metrics
      relabelings:
        - action: replace
          # user-defined cluster name, requires unique
          replacement: emqx5
          targetLabel: cluster
        - action: replace
          # fix value, don't modify
          replacement: exporter
          targetLabel: from
        - action: replace
          # fix value, don't modify
          sourceLabels: ['pod']
          regex: '(.*)-.*-.*'
          replacement: $1
          targetLabel: "instance"
        - action: labeldrop
          # fix value, don't modify
          regex: 'pod'
  namespaceSelector:
    matchNames:
      # modify the namespace if your exporter deployed in other namespace
      #- default
```

`path` indicates the path of the indicator collection interface. In EMQX 5, the path is: `/api/v5/prometheus/stats`. `selector.matchLabels` indicates the label of the matching Pod: `apps.emqx.io/instance: emqx`.

The value of the targetLabel `cluster` represents the name of the current cluster. Make sure it is unique.

Save the above content as `monitor.yaml` and execute the following command:

```bash
$ kubectl apply -f monitor.yaml
```

## View EMQX Indicators on Prometheus

Open the Prometheus interface, switch to the Graph page, and enter `emqx` to display as shown in the following figure:

![](./assets/configure-emqx-prometheus/emqx-prometheus-metrics.png)

Switch to the **Status** -> **Targets** page, the following figure is displayed, and you can see all monitored EMQX Pod information in the cluster:

![](./assets/configure-emqx-prometheus/emqx-prometheus-target.png)

## Import Grafana Templates
Import all dashboard [templates](https://github.com/emqx/emqx-exporter/tree/main/grafana-dashboard/template). Open the main dashboard **EMQX** and enjoy yourself!

![](./assets/configure-emqx-prometheus/emqx-grafana-dashboard.png)
