---
prev:
  text: 'Telemetry'
  link: '../guides/telemetry/telemetry'
---

# Developer Guides

Developer Guidesは、開発者がEMQXを使い始め、IoTアプリケーションを構築するためのガイドです。本章では、クライアント接続、コマンドラインによるMQTTテスト、データ処理、外部システムとの統合、そして高度なプロトコル機能について解説します。本章の主な内容は以下の通りです。

- [Client SDK](./connect-emqx/introduction.md) は、C、Java、Go、Python、JavaScriptの主要なMQTTクライアントライブラリを使ってEMQXに接続するためのステップバイステップの手順とコードサンプルを提供します。

- [Use curl with EMQX](./connect-emqx/curl.md) では、MQTTまたはMQTTSを使ってコマンドラインからEMQXに接続し、トピックへのメッセージパブリッシュやトピックのサブスクライブを行う方法を説明します。

- [Tutorials](./tutorial/tutorial.md) は、クライアント接続、データ収集、MQTT通信の最適化、統合、セキュリティ、デプロイメントなど幅広いテーマの実践的なガイドを提供します。

- [Rule Engine](./data-integration/rules.md) は、リアルタイムでIoTデータを抽出、フィルタリング、拡充、変換する組み込みのデータ処理エンジンを紹介し、データ統合と連携して動作します。

- [Smart Data Hub](./data-integration/smart-data-hub.md) は、スキーマ管理、データ検証、MQTTメッセージのリアルタイム変換を一元的に行うソリューションを提供します。

- [Data Integration](./data-integration/data-bridges.md) は、SinkおよびSourceコンポーネントを使って、EMQXとデータベース、メッセージキュー、クラウドサービスなどの外部データシステムを接続する方法を説明します。

- [Flow Designer](./flow-designer/introduction.md)（EMQX Enterprise機能）は、ルール、アクション、統合をグラフィカルに接続してデータ処理パイプラインを構築する、ノーコードのビジュアルツールです。

- [EMQX AI](./emqx-ai/overview.md) は、MQTT上のMCP、MCPブリッジ、SDK、リアルタイム音声・映像AIサービスなど、EMQXのAI機能を紹介します。

- [Advanced Features](./advanced-feature.md) では、MQTT over WebSocket、MQTT over QUIC、クラスターリンク、MQTTによるファイル転送、マルチプロトコルゲートウェイ、クライアント属性など、EMQXの追加プロトコル機能を紹介します。

- [Architecture](./architecture-introduction.md) は、クラスターリング、MQTT Durable Session、インフライトウィンドウとメッセージキュー、メッセージ再送など、EMQX内部の設計原則を解説します。

- [MQTT Reference](./mqtt-reference.md) は、MQTTプロトコルのバージョン、用語、機能、理由コードを網羅した包括的なリファレンスです。
