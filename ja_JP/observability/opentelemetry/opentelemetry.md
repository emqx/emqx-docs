# OpenTelemetryとの統合

[OpenTelemetry](https://opentelemetry.io/docs/what-is-opentelemetry/)は、トレース、メトリクス、ログなどのテレメトリデータを生成・管理するためのオブザーバビリティフレームワークおよびツールキットです。重要な点として、OpenTelemetryはベンダーやツールに依存しない設計となっており、JaegerやPrometheusなどのオープンソースツールから商用製品まで、幅広いオブザーバビリティバックエンドと連携可能です。

EMQXは、OTLP/gRPCを介してOpenTelemetryコレクターにテレメトリデータを直接プッシュし、コレクターを通じてデータを転送、フィルタリング、変換することで、Jaegerや[Prometheus](../../observability/prometheus.md)などの任意のバックエンドに統合し、保存や可視化を行うことをサポートしています。EMQX 6.3.0以降では、OAuth2認証を用いたOTLP HTTP/protobuf経由でOpenTelemetryのログおよびトレースを直接[Dynatrace](./dynatrace.md)にエクスポートすることも可能です。

OpenTelemetryとの統合により、EMQXのメトリクス収集、メッセージパブリッシュの分散トレーシング、ログの統合収集およびコンテキスト関連付けが最適化されます。この統合は、EMQXの可視化監視やアラート通知の実現、異なるシステムやサービス間のメッセージフローの追跡に役立ちます。これにより、継続的なパフォーマンス最適化、迅速な問題特定、システム監視が容易になります。

::: tip
EMQX 6.3.0以降、OpenTelemetryは`opentelemetry`機能ゲートで制御されます。`EMQX_FEATURES`を手動で設定する場合、`opentelemetry`を有効にすると、その依存関係である`dashboard`も自動的に有効になります。詳細は[機能ゲート](../../deploy/feature-gates.md)をご参照ください。
:::

<img src="./assets/emqx-opentelemetry.jpg" alt="emqx-opentelemetry" style="zoom:67%;" />

本節では、EMQXがOpenTelemetryコレクターとテレメトリデータを統合し、以下のオブザーバビリティ情報に対する完全な組み込みOpenTelemetryサポートを実現する方法を紹介します。

- [メトリクス](./metrics.md)
- [トレース](./traces.md)
- [ログ](./logs.md)
- [Dynatrace](./dynatrace.md)

さらに、EMQXバージョン5.8.3以降では、OpenTelemetryに基づくエンドツーエンドトレーシングもサポートしています。

- [エンドツーエンドトレース](./e2e-traces.md)
