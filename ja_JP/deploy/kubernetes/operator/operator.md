# EMQX Operator 概要

<<<<<<< HEAD
EMQX Operator は、[Kubernetes](https://kubernetes.io/) 上での [EMQX](https://www.emqx.io/) クラスターのデプロイおよび管理をネイティブにサポートするオペレーターです。主な目的は、Kubernetes 環境における EMQX のライフサイクル管理を簡素化し、自動化することです。

EMQX Operator は Kubernetes 1.24 以降が必要です。

EMQX Operator には、以下を含むがこれに限定されない機能があります：

* **簡素化されたデプロイ**：EMQX カスタムリソースを宣言して EMQX クラスターを迅速にデプロイできます。

    詳細は、[はじめに](./getting-started.md) ガイドをご参照ください。

* **クラスター管理**：ワークロードの移行を伴うクラスターアップグレード、ランタイムデータのパーシステンス、Kubernetes 管理リソースの最新状態維持など、EMQX クラスターの運用・保守を自動化します。

    詳細は、[EMQX の管理](./tasks/overview.md) セクションをご覧ください。
=======
EMQX Operator は、[Kubernetes](https://kubernetes.io/) ネイティブによる [EMQX](https://www.emqx.io/)（EMQX ブローカーおよび EMQX Enterprise を含む）のデプロイおよび管理を提供します。本プロジェクトの目的は、EMQX クラスターの構成を簡素化し、自動化することです。

EMQX Operator は以下の機能を含みますが、これらに限定されません。

* **簡素化されたデプロイ**：EMQX カスタムリソースで EMQX クラスターを宣言し、迅速にデプロイできます。詳細は [Getting Started](./getting-started.md) をご参照ください。

* **EMQX クラスターの管理**：クラスターのアップグレード、ランタイムデータのパーシステンス、EMQX の状態に基づく Kubernetes リソースの更新など、EMQX の運用・保守を自動化します。詳細は [Manage EMQX](./tasks/overview.md) をご参照ください。
>>>>>>> origin/release-5.10

<img src="./assets/architecture.png" style="zoom:20%;" alt="EMQX Operator アーキテクチャ" />

<<<<<<< HEAD
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
=======
## Kubernetes バージョンの選択方法

EMQX Operator は Kubernetes クラスターのバージョン `>=1.24` を必要とします。

| Kubernetes バージョン       | EMQX Operator の対応状況                                   | 備考                                                         |
| --------------------------- | ---------------------------------------------------------- | ------------------------------------------------------------ |
| 1.24 以上                   | すべての機能がサポートされています                        |                                                              |
| 1.22（含む）～ 1.23         | [MixedProtocolLBService](https://kubernetes.io/docs/reference/command-line-tools-reference/feature-gates/) を除きサポート | EMQX クラスターは `LoadBalancer` タイプの Service で TCP または UDP のいずれか一方のプロトコルのみ使用可能です。 |
| 1.21（含む）～ 1.22         | [pod-deletion-cost](https://kubernetes.io/docs/concepts/workloads/controllers/replicaset/#pod-deletion-cost) を除きサポート | EMQX Core + Replicant モードクラスター使用時、EMQX クラスターの更新で Pod を正確に削除できません。 |
| 1.20（含む）～ 1.21         | サポートされていますが、`NodePort` タイプの Service 使用時は `.spec.ports[].nodePort` の手動割り当てが必要です | 詳細は [Kubernetes changelog](https://github.com/kubernetes/kubernetes/blob/master/CHANGELOG/CHANGELOG-1.20.md#bug-or-regression-4) をご参照ください。 |
| 1.16（含む）～ 1.20         | サポートされていますが、テスト不足のため推奨されません    |                                                              |
| 1.16 未満                   | サポートされていません                                     | `apiextensions/v1` API バージョンがサポートされていません。 |
>>>>>>> origin/release-5.10
