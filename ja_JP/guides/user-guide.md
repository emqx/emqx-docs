---
prev:
  text: 'LLM Resources'
  link: '../get-started/llms-txt'
---

# ユーザーガイド

EMQXのさまざまな操作方法についての説明です。

これらのガイドは、管理者や運用担当者がEMQXのデプロイメントを管理、設定、セキュリティ確保、監視、および保守するのに役立ちます。本章の主な内容は以下の通りです。

- [クラスター管理](./cluster/create-cluster.md) では、EMQXクラスターの作成および管理方法を解説します。クラスターのセキュリティ、ロードバランサーの設定、ノードの退避、クラスター負荷の再分散、パフォーマンスチューニングなどを含みます。
- [EMQXの設定](./configuration/configuration.md) では、設定ファイルの基本情報、設定オプション、および詳細設定のリファレンスを提供します。
- [ネームスペース](./multi-tenancy/namespace-overview.md) では、MQTTクライアントを論理的にグループ化し、共有EMQXクラスター内でテナントレベルの分離、クォータ、レート制限を管理する方法を説明します。
- [REST API](./api.md) では、クライアント、トピック、サブスクリプションなどの管理に利用するEMQXが提供するHTTP管理APIの使い方を案内します。
- [コマンドラインインターフェース](./cli.md) では、EMQXがサポートする各種起動および管理コマンドを紹介します。
- [セキュリティガイド](./security-guide.md) では、ネットワークおよびTLSの設定、認証、認可、禁止クライアント、フラッピング検出、アイデンティティガバナンス、APIキー、監査ログについて解説します。
- [MQTT Durable Sessions](./durability/durability_introduction.md) では、Durable Sessions機能の設定方法と、高可用性のためのデータレプリカのパラメータ設定を案内します。
- [EMQXダッシュボード](./dashboard/introduction.md) では、EMQXに組み込まれた管理コンソールの包括的な紹介を行います。EMQXクラスターの管理と監視、各種機能の設定、必要な機能の利用方法を学べます。
- [ログと可観測性](./observability/overview.md) では、EMQXのメトリクス観測および監視機能を紹介し、システムの監視やデバッグを支援します。
- [プラグインと拡張](./extensions/introduction.md) では、プラグイン開発によるEMQXの機能拡張方法を説明します。
- [テレメトリー](./telemetry/telemetry.md) では、製品改善のために利用状況情報を共有するテレメトリーの有効化方法を解説します。
