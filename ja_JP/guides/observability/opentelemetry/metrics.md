# OpenTelemetryを統合してメトリクスを表示する
EMQXは、gRPC OTELプロトコルを介してメトリクスを直接OpenTelemetry Collectorにプッシュする機能を内蔵しています。Collectorはその後、データを任意のバックエンドにルーティング、フィルタリング、変換し、保存および可視化を行えます。

本ページでは、EMQXとOpenTelemetryをダッシュボード経由で統合し、[Prometheus](../prometheus.md)を通じてEMQXのメトリクスを表示する方法を紹介します。

## 前提条件

OpenTelemetryとPrometheusをデプロイおよび設定しておく必要があります。

- [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)をデプロイしてください。
- CollectorのgRPC受信ポート（デフォルト4317）およびPrometheusメトリクスのエクスポートポート（8889）を設定してください。

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

- [Prometheus](https://prometheus.io/docs/prometheus/latest/installation)をデプロイしてください。
- PrometheusがCollectorで収集されたメトリクスをスクレイプするよう設定してください。

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

EMQXがローカルで動作している場合は、以下の設定をEMQXの `cluster.hocon` ファイルに追加してください。

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

EMQXのメトリクスはPrometheusのウェブコンソール（http://otel-collector:9090）で確認できます。  
![OpenTelemetry-Prometheus](./assets/opentelemetry-prometheus.png)
