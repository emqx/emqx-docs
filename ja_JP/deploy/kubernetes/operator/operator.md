# EMQX Operator 概要

EMQX Operator は、[Kubernetes](https://kubernetes.io/) 上での [EMQX](https://www.emqx.io/) クラスターのデプロイおよび管理をネイティブにサポートするオペレーターです。主な目的は、Kubernetes 環境における EMQX のライフサイクル管理を簡素化し、自動化することです。

EMQX Operator は Kubernetes 1.24 以降が必要です。

EMQX Operator には、以下を含むがこれに限定されない機能があります：

* **簡素化されたデプロイ**：EMQX カスタムリソースを宣言して EMQX クラスターを迅速にデプロイできます。

    詳細は、[はじめに](./getting-started.md) ガイドをご参照ください。

* **クラスター管理**：ワークロードの移行を伴うクラスターアップグレード、ランタイムデータのパーシステンス、Kubernetes 管理リソースの最新状態維持など、EMQX クラスターの運用・保守を自動化します。

    詳細は、[EMQX の管理](./tasks/overview.md) セクションをご覧ください。

<img src="./assets/architecture.png" style="zoom:20%;" />

## EMQX と EMQX Operator の互換性

現在の EMQX Operator リリースシリーズ 2.2.x は、以下の EMQX バージョンに対応しています：
- EMQX オープンソース版およびエンタープライズ版 5.1.1 ～ 5.8.x
- EMQX 5.9 および 5.10 <sup>*</sup>
- EMQX 6.0 以降 <sup>*</sup>

対応している API バージョンは以下の通りです：
- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md)
- apps.emqx.io/v2alpha1（非推奨）
- apps.emqx.io/v1beta4
- apps.emqx.io/v1beta3（非推奨）

::: tip
<sup>*</sup> Durable Storage レプリケーションの自動管理はこれらのバージョンではサポートされておらず、次期 2.3.0 リリースで対応予定です。
:::
