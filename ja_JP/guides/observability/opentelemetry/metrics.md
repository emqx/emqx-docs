# OpenTelemetryを統合してメトリクスを表示する
EMQXは、gRPC OTELプロトコルを介してメトリクスをOpenTelemetry Collectorに直接プッシュする機能を標準でサポートしています。Collectorは、その後データを任意のバックエンドにルーティング、フィルタリング、変換し、保存および可視化を行えます。

このページでは、EMQXとOpenTelemetryをダッシュボードを通じて統合し、[Prometheus](../prometheus.md)でEMQXのメトリクスを表示する方法を紹介します。

## 前提条件

OpenTelemetryとPrometheusを統合する前に、OpenTelemetryとPrometheusをデプロイおよび設定する必要があります。

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
- PrometheusがCollectorで収集されたメトリクスをスクレイプするよう設定します。

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

EMQXのOpenTelemetryメトリクス機能との統合は、EMQXダッシュボードまたは設定ファイルで行えます。EMQXダッシュボードでは、左側のナビゲーションメニューから **Management** -> **Monitoring** をクリックし、**Integration** タブを開いてメトリクスの設定を行います。

`opentelemetry.exporter.endpoint` は1つのURLを受け入れます。URLは `http` または `https` スキームを使用し、明示的なポート番号を含める必要があります。例えば、`http://localhost:4317` は有効ですが、`localhost:4317` や `http://localhost` は無効です。

以下の設定をEMQXの `cluster.hocon` ファイルに追加してください（EMQXがローカルで動作している場合）：

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

EMQXのメトリクスはPrometheusのウェブコンソール（http://otel-collector:9090）で確認できます：
![OpenTelemetry-Prometheus](./assets/opentelemetry-prometheus.png)
