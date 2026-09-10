# OpenTelemetryとの統合

[OpenTelemetry](https://opentelemetry.io/docs/what-is-opentelemetry/)は、トレース、メトリクス、ログなどのテレメトリデータを生成・管理するためのオブザーバビリティフレームワークおよびツールキットです。重要な点として、OpenTelemetryはベンダーやツールに依存しないため、JaegerやPrometheusなどのオープンソースツールから商用製品まで、幅広いオブザーバビリティバックエンドと連携可能です。

EMQXは、gRPC OTELプロトコルを介してテレメトリデータをOpenTelemetry Collectorに直接プッシュし、その後Collectorを通じてデータを転送、フィルタリング、変換し、Jaegerや[Prometheus](../prometheus.md)などの任意のバックエンドに統合して保存および可視化できます。OpenTelemetryとの統合により、EMQXのメトリクス収集、メッセージパブリッシュの分散トレーシング、ログの統合収集およびコンテキスト関連付けが最適化されます。この統合は、EMQXの可視化監視やアラート通知、異なるシステムやサービス間のメッセージフロー追跡を実現し、継続的なパフォーマンス最適化、迅速な問題特定、システム監視に非常に役立ちます。

<img src="./assets/emqx-opentelemetry.jpg" alt="emqx-opentelemetry" style="zoom:67%;" />

本セクションでは、EMQXがOpenTelemetry Collectorとテレメトリデータを統合し、以下のオブザーバビリティ情報に対する完全な組み込みOpenTelemetryサポートを可能にする方法を紹介します。

- [メトリクス](./metrics.md)
- [トレース](./traces.md)
- [ログ](./logs.md)

さらに、EMQXバージョン5.8.3以降では、OpenTelemetryに基づくエンドツーエンドトレーシングもサポートされています。

- [エンドツーエンドトレース](./e2e-traces.md)
