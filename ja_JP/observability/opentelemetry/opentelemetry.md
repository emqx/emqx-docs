# OpenTelemetryとの統合

[OpenTelemetry](https://opentelemetry.io/docs/what-is-opentelemetry/)は、トレース、メトリクス、ログなどのテレメトリデータを生成および管理するためのオブザーバビリティフレームワークおよびツールキットです。重要な点として、OpenTelemetryはベンダーやツールに依存しないため、JaegerやPrometheusなどのオープンソースツールから商用製品まで、幅広いオブザーバビリティバックエンドと連携可能です。

EMQXはgRPC OTELプロトコルを介してOpenTelemetry Collectorへテレメトリデータを直接プッシュすることをサポートしており、その後Collectorを通じてデータの転送、フィルタリング、変換を行い、Jaegerや[Prometheus](../../observability/prometheus.md)などの任意のバックエンドに統合して保存・可視化できます。OpenTelemetryとの統合により、EMQXのメトリクス収集、メッセージパブリッシュの分散トレーシング、ログの統一的な収集とコンテキスト関連付けが最適化されます。この統合は、EMQXの可視化監視やアラート通知の実現、異なるシステムやサービス間のメッセージフローの追跡に役立ちます。これにより、継続的なパフォーマンス最適化、迅速な問題特定、システム監視が可能となります。

<img src="./assets/emqx-opentelemetry.jpg" alt="emqx-opentelemetry" style="zoom:67%;" />

本セクションでは、EMQXがOpenTelemetry Collectorとテレメトリデータを統合する方法を紹介し、以下のオブザーバビリティ情報に対する完全な組み込みOpenTelemetryサポートを実現します。

- [メトリクス](./metrics.md)
- [トレース](./traces.md)
- [ログ](./logs.md)

さらに、EMQXバージョン5.8.3以降では、OpenTelemetryに基づくエンドツーエンドトレーシングもサポートしています。

- [エンドツーエンドトレース](./e2e-traces.md)
