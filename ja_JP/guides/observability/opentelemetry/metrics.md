# OpenTelemetryを統合してメトリクスを表示する
EMQXは、gRPC OTELプロトコルを介してメトリクスを直接OpenTelemetryコレクターにプッシュする機能を標準でサポートしています。コレクターは、その後データを任意のバックエンドにルーティング、フィルタリング、変換し、保存および可視化を行うことができます。

このページでは、EMQXとOpenTelemetryをダッシュボードを通じて統合し、[Prometheus](../prometheus.md)でEMQXのメトリクスを表示する方法を紹介します。

## 前提条件

OpenTelemetryとPrometheusを統合する前に、これらをデプロイおよび設定する必要があります。

- [OpenTelemetryコレクター](https://opentelemetry.io/docs/collector/getting-started)をデプロイします。
- コレクターのgRPC受信ポート（デフォルトは4317）とPrometheusメトリクスのエクスポートポート（8889）を設定します。

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
- Prometheusを設定し、コレクターが収集したメトリクスをスクレイプします。

```yaml
# prometheus.yaml
scrape_configs:
  - job_name: 'otel-collector'
    scrape_interval: 10s
    static_configs:
      - targets: ['otel-collector:8889'] # EMQXメトリクス
      - targets: ['otel-collector:8888'] # コレクターメトリクス
```

## EMQXでOpenTelemetryメトリクスを有効化する

EMQXのOpenTelemetryメトリクス機能との統合は、EMQXダッシュボードまたは設定ファイルで行えます。EMQXダッシュボードでは、左側のナビゲーションメニューから **Management** -> **Monitoring** をクリックし、**Integration** タブでメトリクスの設定を行います。

EMQXがローカルで動作している場合、以下の設定をEMQXの `cluster.hocon` ファイルに追加してください。

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

EMQXのメトリクスは、PrometheusのWebコンソール（http://otel-collector:9090）で確認できます。  
![OpenTelemetry-Prometheus](./assets/opentelemetry-prometheus.png)
