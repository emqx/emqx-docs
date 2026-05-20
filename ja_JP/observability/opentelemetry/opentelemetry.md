# OpenTelemetryとの統合

[OpenTelemetry](https://opentelemetry.io/docs/what-is-opentelemetry/)は、トレース、メトリクス、ログなどのテレメトリデータを作成および管理するためのオブザーバビリティフレームワークおよびツールキットです。重要な点として、OpenTelemetryはベンダーやツールに依存しないため、JaegerやPrometheusなどのオープンソースツールや商用製品を含む幅広いオブザーバビリティバックエンドと連携可能です。

EMQXはgRPC OTELプロトコルを介してテレメトリデータをOpenTelemetryコレクターに直接プッシュすることをサポートしており、その後コレクターを通じてデータを任意のバックエンド（Jaegerや[Prometheus](../../observability/prometheus.md)など）に転送、フィルタリング、変換して保存および可視化に利用できます。OpenTelemetryとの統合により、EMQXのメトリクス収集、メッセージパブリッシュの分散トレーシング、およびログの統合収集とコンテキスト関連付けが最適化されます。この統合は、EMQXの可視化監視やアラート通知の実現、異なるシステムやサービス間のメッセージフローの追跡に役立ちます。これにより、継続的なパフォーマンス最適化、問題の迅速な特定、システム監視が容易になります。

<img src="./assets/emqx-opentelemetry.jpg" alt="emqx-opentelemetry" style="zoom:67%;" />

本セクションでは、EMQXがOpenTelemetryコレクターとテレメトリデータを統合し、以下のオブザーバビリティ情報に対して完全な組み込みOpenTelemetryサポートを実現する方法を紹介します。

- [メトリクス](./metrics.md)
- [トレース](./traces.md)
- [ログ](./logs.md)

さらに、EMQXバージョン5.8.3以降では、OpenTelemetryに基づくエンドツーエンドトレーシングもサポートしています。

- [エンドツーエンドトレース](./e2e-traces.md)
