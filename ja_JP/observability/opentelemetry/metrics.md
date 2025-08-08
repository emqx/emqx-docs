# OpenTelemetryを統合してメトリクスを表示する
EMQXは、gRPC OTELプロトコルを介してメトリクスを直接OpenTelemetry Collectorにプッシュする機能を標準でサポートしています。Collectorはその後、データを任意のバックエンドにルーティング、フィルタリング、変換して保存および可視化できます。

このページでは、EMQXとOpenTelemetryをダッシュボード経由で統合し、[Prometheus](../../observability/prometheus.md)を通じてEMQXのメトリクスを表示する方法を紹介します。

## 前提条件

OpenTelemetryとPrometheusを統合する前に、これらをデプロイおよび設定する必要があります。

- [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)をデプロイします。
- CollectorのgRPC受信ポート（デフォルトは4317）とPrometheusメトリクスエクスポートポート（8889）を設定します。

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

- [Prometheus](https://prometheus.io/docs/prometheus/latest/installation)をデプロイします。
- Prometheusを設定して、Collectorが収集したメトリクスをスクレイプします。

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

EMQXのOpenTelemetryメトリクス機能との統合は、EMQXダッシュボードまたは設定ファイルで行えます。EMQXダッシュボードの左側ナビゲーションメニューから **Management** -> **Monitoring** をクリックし、**Integration** タブでメトリクスの設定を行います。

以下の設定をEMQXの `cluster.hocon` ファイルに追加してください（EMQXがローカルで動作している場合の例です）：

```bash
opentelemetry {
  exporter {
    endpoint = "http://localhost:4317"
    headers {
      authorization = "Basic dXNlcjpwYXNzd29yZA=="
    }
  }
  metrics {
     interval = "10s"
  }
}
```

## PrometheusでEMQXメトリクスを可視化する

EMQXのメトリクスは、PrometheusのWebコンソール（http://otel-collector:9090）で確認できます：
![OpenTelemetry-Prometheus](./assets/opentelemetry-prometheus.png)
