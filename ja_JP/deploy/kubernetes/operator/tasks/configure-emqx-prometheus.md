# Prometheus と Grafana による EMQX クラスターの監視

<<<<<<< HEAD
## 目的

[EMQX Exporter](https://github.com/emqx/emqx-exporter) をデプロイし、Prometheus と Grafana を使って EMQX クラスターを監視します。

## Prometheus と Grafana のデプロイ

* Prometheus のデプロイ方法については、[Prometheus](https://github.com/prometheus-operator/prometheus-operator) のドキュメントを参照してください。
* Grafana のデプロイ方法については、[Grafana](https://grafana.com/docs/grafana/latest/setup-grafana/installation/kubernetes/) のドキュメントを参照してください。

## EMQX クラスターのデプロイ

EMQX は [Prometheus 互換の HTTP API](../../../../observability/prometheus.md) を通じて様々なメトリクスを公開します。
=======
## タスク対象
[EMQX Exporter](https://github.com/emqx/emqx-exporter) をデプロイし、Prometheus と Grafana によって EMQX クラスターを監視します。

## Prometheus と Grafana のデプロイ

Prometheus のデプロイに関しては、[Prometheus](https://github.com/prometheus-operator/prometheus-operator) のドキュメントを参照してください。  
Grafana のデプロイに関しては、[Grafana](https://grafana.com/docs/grafana/latest/setup-grafana/installation/kubernetes/) のドキュメントを参照してください。

## EMQX クラスターのデプロイ

以下は EMQX カスタムリソースの関連設定例です。デプロイしたい EMQX のバージョンに応じて適切な `apiVersion` を選択してください。詳細な対応関係については [EMQX Operator Compatibility](../operator.md) をご参照ください。

EMQX は http インターフェースを通じて指標を公開することをサポートしています。クラスター全体の統計指標については、ドキュメント [Integrate with Prometheus](../../../../observability/prometheus.md) をご参照ください。
>>>>>>> origin/release-5.10

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

<<<<<<< HEAD
上記の内容を `emqx.yaml` として保存し、以下のコマンドを実行して EMQX クラスターをデプロイします。
=======
上記内容を `emqx.yaml` として保存し、以下のコマンドを実行して EMQX クラスターをデプロイします。
>>>>>>> origin/release-5.10

```bash
$ kubectl apply -f emqx.yaml
emqx.apps.emqx.io/emqx created
```

<<<<<<< HEAD
EMQX クラスターのステータスを確認し、`STATUS` が `Ready` になっていることを確認してください。完了までに時間がかかる場合があります。

```bash
$ kubectl get emqx emqx
NAME   STATUS   AGE
emqx   Ready    10m
```

## API シークレットの作成

Prometheus は EMQX ダッシュボード API からメトリクスを取得するため、ダッシュボードにサインインして [API シークレットを作成](../../../../dashboard/system.md#api-keys) する必要があります。

## [EMQX Exporter](https://github.com/emqx/emqx-exporter) のデプロイ

`emqx-exporter` は EMQX の Prometheus API で公開されていない一部のメトリクスを公開するために設計されています。
=======
EMQX クラスターの状態を確認し、`STATUS` が `Running` になるまで待機してください。クラスターの準備には時間がかかる場合があります。

```bash
$ kubectl get emqx emqx
NAME   IMAGE                              STATUS    AGE
emqx   emqx/emqx-enterprise:@EE_VERSION@  Running   10m
```

## API シークレットの作成
emqx-exporter と Prometheus は EMQX ダッシュボード API からメトリクスを取得するため、ダッシュボードにサインインして [API キー](../../../../dashboard/system.md#api-keys) を作成する必要があります。

## [EMQX Exporter](https://github.com/emqx/emqx-exporter) のデプロイ

`emqx-exporter` は EMQX Prometheus API に含まれていない一部のメトリクスを公開するために設計されています。
>>>>>>> origin/release-5.10

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
<<<<<<< HEAD
            # "emqx-dashboard-service-name" はオペレーターが 18083 ポート公開用に作成したサービス名です
=======
            # "emqx-dashboard-service-name" は operator によって作成され、18083 ポートを公開するサービス名です
>>>>>>> origin/release-5.10
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

<<<<<<< HEAD
> 引数の `--emqx.nodes` はオペレーターが 18083 ポート公開用に作成したサービス名に設定してください。サービス名は `kubectl get svc` コマンドで確認できます。

上記の内容を `emqx-exporter.yaml` として保存し、`--emqx.auth-username` と `--emqx.auth-password` を新しく作成した API シークレットに置き換えてください。以下のコマンドで `emqx-exporter` をデプロイします。
=======
> `--emqx.nodes` の引数には、operator によって作成され 18083 ポートを公開しているサービス名を設定してください。サービス名は `kubectl get svc` コマンドで確認できます。

上記内容を `emqx-exporter.yaml` として保存し、`--emqx.auth-username` と `--emqx.auth-password` を作成した API シークレットに置き換えてから、以下のコマンドを実行して emqx-exporter をデプロイします。
>>>>>>> origin/release-5.10

```bash
kubectl apply -f emqx-exporter.yaml
```

<<<<<<< HEAD
`emqx-exporter` の Pod のステータスを確認します。
=======
emqx-exporter Pod の状態を確認します。
>>>>>>> origin/release-5.10

```bash
$ kubectl get po -l="app=emqx-exporter"
NAME                            STATUS   AGE
emqx-exporter-856564c95-j4q5v   Running  8m33s
```

## Prometheus 監視の設定
<<<<<<< HEAD

Prometheus Operator は [PodMonitor](https://github.com/prometheus-operator/prometheus-operator/blob/main/Documentation/getting-started/design.md#podmonitor) と [ServiceMonitor](https://github.com/prometheus-operator/prometheus-operator/blob/main/Documentation/getting-started/design.md#servicemonitor) CRD を使って、Pod やサービスの監視方法を動的に定義します。
=======
Prometheus-operator は [PodMonitor](https://github.com/prometheus-operator/prometheus-operator/blob/main/Documentation/design.md#podmonitor) と [ServiceMonitor](https://github.com/prometheus-operator/prometheus-operator/blob/main/Documentation/design.md#servicemonitor) CRD を使用して、Pod やサービスの監視方法を動的に定義します。
>>>>>>> origin/release-5.10

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
      # emqx ダッシュボードの containerPort 名
      port: dashboard
      relabelings:
        - action: replace
<<<<<<< HEAD
          # ユーザー定義のクラスター名、一意である必要があります
=======
          # ユーザー定義のクラスター名、ユニークである必要があります
>>>>>>> origin/release-5.10
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
<<<<<<< HEAD
      # emqx Pod のラベルと同じにしてください
=======
      # emqx Pod のラベルと同じ
>>>>>>> origin/release-5.10
      apps.emqx.io/instance: emqx
      apps.emqx.io/managed-by: emqx-operator
  namespaceSelector:
    matchNames:
<<<<<<< HEAD
      # EMQX クラスターを他のネームスペースにデプロイしている場合は修正してください
=======
      # EMQX クラスターを別のネームスペースにデプロイしている場合は変更してください
>>>>>>> origin/release-5.10
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
<<<<<<< HEAD
      # emqx-exporter サービスのラベルと同じにしてください
=======
      # emqx-exporter サービスのラベルと同じ
>>>>>>> origin/release-5.10
      app: emqx-exporter
  endpoints:
    - port: metrics
      interval: 5s
      path: /metrics
      relabelings:
        - action: replace
<<<<<<< HEAD
          # ユーザー定義のクラスター名、一意である必要があります
=======
          # ユーザー定義のクラスター名、ユニークである必要があります
>>>>>>> origin/release-5.10
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
<<<<<<< HEAD
      # exporter を他のネームスペースにデプロイしている場合は修正してください
      #- default
```

`path` は指標収集インターフェースのパスを示します。EMQX 5 では `/api/v5/prometheus/stats` です。`selector.matchLabels` はマッチする Pod のラベルを示し、`apps.emqx.io/instance: emqx` となっています。

`targetLabel` の `cluster` は現在のクラスター名を表し、一意である必要があります。

上記の内容を `monitor.yaml` として保存し、以下のコマンドを実行してください。
=======
      # exporter を別のネームスペースにデプロイしている場合は変更してください
      #- default
```

<p> `path` は指標収集インターフェースのパスを示します。EMQX 5 では `/api/v5/prometheus/stats` です。`selector.matchLabels` はマッチする Pod のラベルを示し、`apps.emqx.io/instance: emqx` となっています。</p>  
<p> targetLabel の `cluster` の値は現在のクラスター名を表し、一意である必要があります。</p>

上記内容を `monitor.yaml` として保存し、以下のコマンドを実行します。
>>>>>>> origin/release-5.10

```bash
$ kubectl apply -f monitor.yaml
```

## Prometheus での EMQX 指標の確認

<<<<<<< HEAD
Prometheus インターフェースを開き、Graph ページに切り替えて `emqx` と入力すると、以下のように表示されます。

![](./assets/configure-emqx-prometheus/emqx-prometheus-metrics.png)

**Status** -> **Targets** ページに切り替えると、以下の画面が表示され、クラスター内の監視対象の EMQX Pod 情報を確認できます。
=======
Prometheus インターフェースを開き、Graph ページに切り替えて `emqx` を入力すると、以下の図のように表示されます。

![](./assets/configure-emqx-prometheus/emqx-prometheus-metrics.png)

**Status** -> **Targets** ページに切り替えると、以下の図のようにクラスター内のすべての監視対象 EMQX Pod 情報が表示されます。
>>>>>>> origin/release-5.10

![](./assets/configure-emqx-prometheus/emqx-prometheus-target.png)

## Grafana テンプレートのインポート
<<<<<<< HEAD

すべてのダッシュボード [テンプレート](https://github.com/emqx/emqx-exporter/tree/main/grafana-dashboard/template) をインポートしてください。メインダッシュボードの **EMQX** を開いてお楽しみください。
=======
すべてのダッシュボード [テンプレート](https://github.com/emqx/emqx-exporter/tree/main/grafana-dashboard/template) をインポートしてください。メインダッシュボード **EMQX** を開いてお楽しみください！
>>>>>>> origin/release-5.10

![](./assets/configure-emqx-prometheus/emqx-grafana-dashboard.png)
