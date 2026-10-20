# EMQXクラスターの管理

この章では、Kubernetesクラスター上でEMQXを操作する際の一般的なタスクや手順について、ステップバイステップで説明します。

## 設定とセットアップ

- ライセンスとセキュリティ
  - [ライセンスの管理](./configure-emqx-license.md)
  - [EMQXリスナーのTLS有効化](./configure-emqx-tls.md)
- クラスター設定
  - [EMQX設定の変更](./configure-emqx-config.md)
  - [Core-Replicantデプロイメントの有効化](./configure-emqx-core-replicant.md)
  - [パーシステンスの有効化](./configure-emqx-persistence.md)
  - [Pod Disruption Budgetの設定](./configure-disruption-budgets.md)
  - [ロードバランサー経由でのEMQXクラスターアクセス](./configure-emqx-service.md)
  - [HPAによるReplicantノードのオートスケール](./configure-emqx-hpa.md)

## アップグレードとメンテナンス

- アップグレード
  - [ローリングアップデートの設定](./configure-emqx-rolling-update.md)
- ログ管理
  - [EMQXログの収集](./configure-emqx-log-collection.md)
  - [EMQXログレベルの変更](./configure-emqx-log-level.md)

## 監視とパフォーマンス

- [Prometheusを使ったEMQXクラスターの監視](./configure-emqx-prometheus.md)
