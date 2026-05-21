# OpenTelemetryを統合してメトリクスを表示する
<<<<<<< HEAD
EMQXは、gRPC OTELプロトコルを介してメトリクスを直接OpenTelemetry Collectorにプッシュする機能を標準でサポートしています。Collectorは、その後データを任意のバックエンドにルーティング、フィルタリング、変換し、保存および可視化を行うことができます。

本ページでは、Dashboardを通じてOpenTelemetryとEMQXを統合する方法と、[Prometheus](../../observability/prometheus.md)を使ってEMQXのメトリクスを表示する方法を紹介します。

## 前提条件

OpenTelemetryとPrometheusを統合する前に、それぞれをデプロイおよび設定する必要があります。

- [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)をデプロイします。
- CollectorのgRPC受信ポート（デフォルト4317）およびPrometheusメトリクスのエクスポートポート（8889）を設定します。
=======
EMQXは、gRPC OTELプロトコルを介してメトリクスをOpenTelemetry Collectorに直接プッシュする機能を標準でサポートしています。Collectorは、そのデータを任意のバックエンドにルーティング、フィルタリング、変換し、保存および可視化が可能です。

本ページでは、Dashboardを通じてOpenTelemetryとEMQXを統合し、[Prometheus](../../observability/prometheus.md)でEMQXのメトリクスを表示する方法を紹介します。

## 前提条件

OpenTelemetryとの統合を行う前に、OpenTelemetryおよびPrometheusをデプロイし、設定する必要があります。

- [OpenTelemetry Collector](https://opentelemetry.io/docs/collector/getting-started)をデプロイします。
- CollectorのgRPC受信ポート（デフォルトは4317）およびPrometheusメトリクスエクスポートポート（8889）を設定します。
>>>>>>> origin/release-5.9

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
- PrometheusがCollectorによって収集されたメトリクスをスクレイプするよう設定します。

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
EMQXのOpenTelemetryメトリクス機能との統合は、EMQX Dashboardまたは設定ファイルで行えます。Dashboardでは左側のナビゲーションメニューから**Management** -> **Monitoring**をクリックし、**Integration**タブでメトリクスの設定を行います。
=======
EMQXのOpenTelemetryメトリクス機能との統合は、EMQX Dashboardまたは設定ファイルで行えます。EMQX Dashboardでは、左側のナビゲーションメニューから**Management** -> **Monitoring**をクリックし、**Integration**タブを選択してメトリクスの設定を行います。
>>>>>>> origin/release-5.9

以下の設定をEMQXの`cluster.hocon`ファイルに追加してください（EMQXがローカルで動作している場合の例です）：

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
