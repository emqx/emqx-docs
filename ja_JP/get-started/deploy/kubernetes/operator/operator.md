# EMQX Operator 概要

EMQX Operator は、[Kubernetes](https://kubernetes.io/) 上での [EMQX](https://www.emqx.io/) クラスターのデプロイおよび管理をネイティブにサポートするツールです。主な目的は、Kubernetes 環境における EMQX のライフサイクル管理を簡素化し、自動化することです。

EMQX Operator は Kubernetes 1.24 以上が必要です。

EMQX Operator は以下の機能を含みますが、これらに限定されません：

* **簡素化されたデプロイ**：EMQX カスタムリソースを使って EMQX クラスターを宣言し、迅速にデプロイできます。

    詳細は [Getting Started](./getting-started.md) ガイドをご参照ください。

* **クラスター管理**：ワークロードのマイグレーションを伴うクラスターアップグレード、ランタイムデータのパーシステンス、Kubernetes 管理リソースの最新状態維持など、EMQX クラスターの運用・保守を自動化します。

    詳細は [Manage EMQX](./tasks/overview.md) セクションをご参照ください。

<img src="./assets/architecture.png" style="zoom:20%;" />

## EMQX と EMQX Operator の互換性

現在の EMQX Operator リリースシリーズ 2.2.x は、以下の EMQX バージョンと互換性があります：
- EMQX オープンソース版およびエンタープライズ版 5.1.1 ～ 5.8.x
- EMQX 5.9 および 5.10 <sup>*</sup>
- EMQX 6.0 以降 <sup>*</sup>

サポートされている API バージョンは以下の通りです：
- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md)
- apps.emqx.io/v2alpha1（非推奨）
- apps.emqx.io/v1beta4
- apps.emqx.io/v1beta3（非推奨）

::: tip
<sup>*</sup> これらのバージョンでは Durable Storage レプリケーションの自動管理はサポートされておらず、次期 2.3.0 リリースで対応予定です。
:::
