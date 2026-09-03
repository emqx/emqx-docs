# Prometheus と Grafana による EMQX クラスターの監視

## 目的

[EMQX Exporter](https://github.com/emqx/emqx-exporter) をデプロイし、Prometheus と Grafana を使って EMQX クラスターを監視します。

## Prometheus と Grafana のデプロイ

* Prometheus のデプロイ方法については、[Prometheus](https://github.com/prometheus-operator/prometheus-operator) のドキュメントをご参照ください。
* Grafana のデプロイ方法については、[Grafana](https://grafana.com/docs/grafana/latest/setup-grafana/installation/kubernetes/) のドキュメントをご参照ください。

## EMQX クラスターのデプロイ

EMQX は [Prometheus 互換の HTTP API](../../../../observability/prometheus.md) を通じて様々なメトリクスを公開します。

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

上記の内容を `emqx.yaml` として保存し、以下のコマンドを実行して EMQX クラスターをデプロイします。

```bash
$ kubectl apply -f emqx.yaml
emqx.apps.emqx.io/emqx created
```

EMQX クラスターのステータスを確認し、`STATUS` が `Ready` になっていることを確認してください。完了までに時間がかかる場合があります。

```bash
$ kubectl get emqx emqx
NAME   STATUS   AGE
emqx   Ready    10m
```

## API キーの作成

ダッシュボードにサインインし、[専用の API キーを2つ作成](../../../../dashboard/system.md#api-keys)します。

- EMQX Exporter 用には Viewer ロールでデフォルトのスコープのまま API キーを作成します。EMQX Exporter は Prometheus のスクレイプ API に加え、いくつかの管理 API を読み取ります。
- Prometheus 用には Viewer ロールで `monitoring` スコープのみの API キーを作成します。`PodMonitor` はこのキーを使って `/api/v5/prometheus/stats` をスクレイプします。

それぞれの統合用に API キーとシークレットキーを保存してください。EMQX はシークレットキーを一度しか表示しません。

## [EMQX Exporter](https://github.com/emqx/emqx-exporter) のデプロイ

`emqx-exporter` は EMQX の Prometheus API で公開されていない一部のメトリクスを公開するために設計されています。

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
            # "emqx-dashboard-service-name" は operator により 18083 ポートを公開するために作成されたサービス名です
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

> 引数の `--emqx.nodes` には operator により 18083 ポートを公開するために作成されたサービス名を設定してください。`kubectl get svc` コマンドでサービス名を確認できます。

上記の内容を `emqx-exporter.yaml` として保存し、`--emqx.auth-username` に EMQX Exporter 用に作成した API キーを、`--emqx.auth-password` にそのシークレットキーを設定してください。以下のコマンドで `emqx-exporter` をデプロイします。

```bash
kubectl apply -f emqx-exporter.yaml
```

`emqx-exporter` ポッドのステータスを確認します。

```bash
$ kubectl get po -l="app=emqx-exporter"
NAME                            STATUS   AGE
emqx-exporter-856564c95-j4q5v   Running  8m33s
```

## Prometheus モニターの設定

Prometheus Operator は [PodMonitor](https://github.com/prometheus-operator/prometheus-operator/blob/main/Documentation/getting-started/design.md#podmonitor) と [ServiceMonitor](https://github.com/prometheus-operator/prometheus-operator/blob/main/Documentation/getting-started/design.md#servicemonitor) CRD を使って、ポッドやサービスの監視方法を動的に定義します。

EMQX 6.3.0 以降、Prometheus のスクレイプ API はデフォルトで認証が必要です。`PodMonitor` と同じネームスペースに、Prometheus 用に作成した API キーとシークレットキーを格納する Kubernetes Secret を作成します。

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
      # emqx ダッシュボードの containerPort 名
      port: dashboard
      relabelings:
        - action: replace
          # ユーザー定義のクラスター名、一意である必要があります
          replacement: emqx5
          targetLabel: cluster
        - action: replace
          # 固定値、変更しないでください
          replacement: emqx
          targetLabel: from
        - action: replace
          # 固定値、変更しないでください
          sourceLabels: ['pod']
          targetLabel: "instance"
  selector:
    matchLabels:
      # EMQX ポッドのラベルと同じもの
      apps.emqx.io/instance: emqx
      apps.emqx.io/managed-by: emqx-operator
  namespaceSelector:
    matchNames:
      # EMQX クラスターが別のネームスペースにデプロイされている場合は修正してください
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
      # emqx-exporter サービスのラベルと同じもの
      app: emqx-exporter
  endpoints:
    - port: metrics
      interval: 5s
      path: /metrics
      relabelings:
        - action: replace
          # ユーザー定義のクラスター名、一意である必要があります
          replacement: emqx5
          targetLabel: cluster
        - action: replace
          # 固定値、変更しないでください
          replacement: exporter
          targetLabel: from
        - action: replace
          # 固定値、変更しないでください
          sourceLabels: ['pod']
          regex: '(.*)-.*-.*'
          replacement: $1
          targetLabel: "instance"
        - action: labeldrop
          # 固定値、変更しないでください
          regex: 'pod'
  namespaceSelector:
    matchNames:
      # exporter が別のネームスペースにデプロイされている場合は修正してください
      #- default
```

`path` はメトリクス収集用の API パスを指定します。EMQX 5.0 以降は `/api/v5/prometheus/stats` を使用します。`basicAuth` セクションは Kubernetes Secret から API キーとシークレットキーを読み取ります。`selector.matchLabels` は `apps.emqx.io/instance: emqx` ラベルで EMQX ポッドを識別します。

`targetLabel` の `cluster` は現在のクラスター名を表し、一意であることを確認してください。

上記の内容を `monitor.yaml` として保存し、以下のコマンドを実行します。

```bash
$ kubectl apply -f monitor.yaml
```

## Prometheus で EMQX 指標を確認

Prometheus インターフェースを開き、Graph ページに切り替えて `emqx` と入力すると、以下のように表示されます。

![](./assets/configure-emqx-prometheus/emqx-prometheus-metrics.png)

**Status** -> **Targets** ページに切り替えると、以下の画面が表示され、クラスター内の監視対象 EMQX ポッド情報を確認できます。

![](./assets/configure-emqx-prometheus/emqx-prometheus-target.png)

## Grafana テンプレートのインポート

すべてのダッシュボード [テンプレート](https://github.com/emqx/emqx-exporter/tree/main/grafana-dashboard/template) をインポートしてください。メインダッシュボード **EMQX** を開いてお楽しみください。

![](./assets/configure-emqx-prometheus/emqx-grafana-dashboard.png)
