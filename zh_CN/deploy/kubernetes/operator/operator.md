# EMQX Operator 概述

EMQX Operator 为部署和管理 [EMQX](https://www.emqx.io/) 集群提供原生 [Kubernetes](https://kubernetes.io/) 支持。其主要目标是简化和自动化 Kubernetes 环境中 EMQX 的生命周期管理。

EMQX Operator 要求 Kubernetes 1.24 或更高版本。

EMQX Operator 包括但不限于以下功能：

* **简化部署**：通过 EMQX 自定义资源声明 EMQX 集群并快速部署。

    更多详细信息，请参阅[快速开始](./getting-started.md)指南。

* **集群管理**：自动化 EMQX 集群的运维操作，包括带工作负载迁移的集群升级、运行时数据持久化、保持 Kubernetes 管理的资源处于最新状态等。

    更多详细信息，请参阅[管理 EMQX](./tasks/overview.md)部分。

<img src="./assets/architecture.png" style="zoom:20%;" />

## EMQX 和 EMQX Operator 兼容性

当前 EMQX Operator 2.2.x 版本系列与以下 EMQX 版本兼容：
- EMQX 开源版和企业版 5.1.1 ~ 5.8.x
- EMQX 5.9 和 5.10 <sup>*</sup>
- EMQX 6.0 及更高版本 <sup>*</sup>

支持以下 API 版本：
- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md)
- apps.emqx.io/v2alpha1（已弃用）
- apps.emqx.io/v1beta4
- apps.emqx.io/v1beta3（已弃用）

::: tip

<sup>*</sup> 这些版本暂不支持自动管理持久存储（Durable Storage）副本的功能，该功能计划在即将发布的 2.3.0 版本中提供。

:::
