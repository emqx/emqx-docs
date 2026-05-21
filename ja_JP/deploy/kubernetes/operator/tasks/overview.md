# EMQXクラスターの管理

<<<<<<< HEAD
本章では、Kubernetesクラスター上でEMQXを操作・管理するための一般的なタスクと手順を段階的に説明します。
=======
本章では、Kubernetesクラスター上でEMQXを操作・管理する際の一般的なタスクや手順について、ステップバイステップで説明します。
>>>>>>> origin/release-5.9

本章は以下のセクションに分かれています。

**設定とセットアップ**

- ライセンスとセキュリティ
  - [ライセンス設定（EMQX Enterprise）](./configure-emqx-license.md)
  - [EMQXでのTLS有効化](./configure-emqx-tls.md)
- クラスター設定
  - [Operatorを使ったEMQX設定の変更](./configure-emqx-config.md)
  - [Core + Replicantクラスターの有効化（EMQX 5.x）](./configure-emqx-core-replicant.md)
  - [EMQXクラスターでのパーシステンス有効化](./configure-emqx-persistence.md)
  - [Kubernetesサービス経由でのEMQXクラスターへのアクセス](./configure-emqx-service.md)
  - [クラスターのロードリバランシング（EMQX Enterprise）](./configure-emqx-rebalance.md)

**アップグレードとメンテナンス**

- アップグレード
  - [ブルーグリーンアップグレードの設定（EMQX Enterprise）](./configure-emqx-blueGreenUpdate.md)
- ログ管理
  - [KubernetesでのEMQXログ収集](./configure-emqx-log-collection.md)
  - [EMQXログレベルの変更](./configure-emqx-log-level.md)

**監視とパフォーマンス**

- [PrometheusによるEMQXクラスターの監視](./configure-emqx-prometheus.md)
