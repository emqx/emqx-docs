# OpenTelemetryを統合してメトリクスを表示する
<<<<<<< HEAD
EMQXは、gRPC OTELプロトコルを介してメトリクスを直接OpenTelemetry Collectorにプッシュする機能を内蔵しています。Collectorはその後、データを任意のバックエンドにルーティング、フィルタリング、変換して保存および可視化を行えます。

本ページでは、Dashboardを通じてOpenTelemetryとEMQXを統合し、[Prometheus](../../observability/prometheus.md)でEMQXのメトリクスを表示する方法を紹介します。

## 前提条件

OpenTelemetryとの統合を行う前に、OpenTelemetryおよびPrometheusをデプロイし設定する必要があります。

- [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)をデプロイする。
- CollectorのgRPC受信ポート（デフォルトは4317）およびPrometheusメトリクスエクスポートポート（8889）を設定する。
=======
EMQXは、gRPC OTELプロトコルを介してメトリクスをOpenTelemetry Collectorに直接プッシュする機能を内蔵しています。Collectorは、その後データを任意のバックエンドにルーティング、フィルタリング、変換して保存および可視化が可能です。

このページでは、Dashboardを通じてEMQXとOpenTelemetryを統合し、[Prometheus](../../observability/prometheus.md)でEMQXのメトリクスを表示する方法を紹介します。

## 前提条件

OpenTelemetryとの統合を行う前に、OpenTelemetryとPrometheusをデプロイおよび設定する必要があります。

- [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)をデプロイします。
- CollectorのgRPC受信ポート（デフォルトは4317）およびPrometheusメトリクスのエクスポートポート（8889）を設定します。
>>>>>>> origin/release-6.1

```yaml
# otel-collector-config.yaml
receivers:
  otlp:
    protocols:
      grpc:

exporters:
  prometheus:
    endpoint: "0.0.0.0:8889"

processors:
  batch:

service:
  pipelines:
    metrics:
      receivers: [otlp]
      processors: [batch]
      exporters: [prometheus]
```

<<<<<<< HEAD
- [Prometheus](https://prometheus.io/docs/prometheus/latest/installation)をデプロイする。
- Prometheusを設定し、Collectorが収集したメトリクスをスクレイプする。
=======
- [Prometheus](https://prometheus.io/docs/prometheus/latest/installation)をデプロイします。
- Prometheusを設定し、Collectorが収集したメトリクスをスクレイプします。
>>>>>>> origin/release-6.1

```yaml
# prometheus.yaml
scrape_configs:
  - job_name: 'otel-collector'
    scrape_interval: 10s
    static_configs:
      - targets: ['otel-collector:8889'] # EMQXメトリクス
      - targets: ['otel-collector:8888'] # Collectorメトリクス
```

## EMQXでOpenTelemetryメトリクスを有効化する

<<<<<<< HEAD
EMQX Dashboardまたは設定ファイルを使用して、OpenTelemetryメトリクス機能との統合を設定できます。EMQX Dashboardでは、左側のナビゲーションメニューから **Management** -> **Monitoring** をクリックし、**Integration** タブでメトリクスの設定を行います。

以下の設定をEMQXの `cluster.hocon` ファイルに追加します（EMQXがローカルで稼働している場合）：
=======
EMQXのOpenTelemetryメトリクス機能との統合は、EMQX Dashboardまたは設定ファイルで行えます。EMQX Dashboardでは、左側のナビゲーションメニューから **Management** -> **Monitoring** をクリックし、**Integration** タブでメトリクスの設定を行います。

以下の設定をEMQXの`cluster.hocon`ファイルに追加してください（EMQXがローカルで動作している場合の例です）：
>>>>>>> origin/release-6.1

```bash
opentelemetry {
  exporter {
    endpoint = "http://localhost:4317"
    headers {
      authorization = ""Basic dXNlcjpwYXNzd29yZA=="
    }
  }
  metrics {
     interval = "10s"
  }
}
```

## PrometheusでEMQXメトリクスを可視化する

<<<<<<< HEAD
EMQXのメトリクスは、PrometheusのWebコンソール（http://otel-collector:9090）で確認できます。  
=======
EMQXのメトリクスは、PrometheusのWebコンソール（http://otel-collector:9090）で確認できます：
>>>>>>> origin/release-6.1
![OpenTelemetry-Prometheus](./assets/opentelemetry-prometheus.png)
