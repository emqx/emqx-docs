# EMQX Operator 概要

EMQX Operator は、[Kubernetes](https://kubernetes.io/) 上での [EMQX](https://www.emqx.io/) クラスターのデプロイおよび管理をネイティブにサポートするオペレーターです。主な目的は、Kubernetes 環境における EMQX のライフサイクル管理を簡素化および自動化することです。

EMQX Operator は Kubernetes 1.24 以降が必要です。

EMQX Operator には、以下の機能が含まれますが、これらに限定されません：

* **簡素化されたデプロイ**：EMQX カスタムリソースを宣言するだけで、EMQX クラスターを迅速にデプロイできます。

    詳細は、[はじめに](./getting-started.md) ガイドをご参照ください。

* **クラスター管理**：ワークロードの移行を伴うクラスターのアップグレード、ランタイムデータのパーシステンス、Kubernetes 管理リソースの最新状態維持など、EMQX クラスターの運用・保守を自動化します。

    詳細は、[EMQX 管理](./tasks/overview.md) セクションをご参照ください。

<img src="./assets/architecture.png" style="zoom:20%;" alt="EMQX Operator アーキテクチャ" />

## EMQX と EMQX Operator の互換性

### EMQX Operator 2.3.x

EMQX Operator 2.3.x リリースシリーズは、以下の EMQX バージョンと完全に互換性があります：
- EMQX 5.9 および 5.10
- EMQX 6.0 以降

サポートされる API バージョンは以下の通りです：
- [apps.emqx.io/v2](./reference/v2-reference.md)
- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md)（非推奨）

### 過去のリリース

#### EMQX Operator 2.2.x

EMQX Operator 2.2.x リリースシリーズは、以下の EMQX バージョンと互換性があります：
- EMQX オープンソース＆エンタープライズ 5.1.1 ～ 5.8.x
- EMQX 5.9 および 5.10（限定サポート<sup>*</sup>）
- EMQX 6.0 以降（限定サポート<sup>*</sup>）

サポートされる API バージョンは以下の通りです：
- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md)
- apps.emqx.io/v2alpha1（非推奨）
- apps.emqx.io/v1beta4
- apps.emqx.io/v1beta3（非推奨）

::: tip
<sup>*</sup> これらのバージョンでは、Durable Storage レプリケーションの自動管理はサポートされていません。
:::
