# EMQX Operator 概览

EMQX Operator 为在 [Kubernetes](https://kubernetes.io/) 中部署和管理 [EMQX](https://www.emqx.io/) 集群提供原生支持，主要用于简化 EMQX 在 Kubernetes 环境中的部署，并实现生命周期管理自动化。

EMQX Operator 3.0 要求 Kubernetes 版本为 1.27 或更高版本。从 Kubernetes 1.27 开始，`StatefulSetAutoDeletePVC` 特性门控默认启用。如果集群管理员已将其禁用，请先在 Kubernetes API Server 和 Controller Manager 上启用该特性门控，再使用 EMQX Operator。

EMQX Operator 提供以下功能：

* **简化部署**：通过 EMQX 自定义资源声明 EMQX 集群并快速完成部署。

    详情请参见[安装 EMQX Operator 并部署 EMQX](./getting-started.md)。

* **集群管理**：自动执行 EMQX 集群运维操作，包括滚动更新、持久化存储和 Kubernetes 托管资源同步。

    详情请参见[管理 EMQX 集群](./tasks/overview.md)。

<img src="./assets/architecture.png" style="zoom:20%;" />

## EMQX 与 EMQX Operator 兼容性

### EMQX Operator 3.0.x

EMQX Operator 3.0.x 系列兼容以下 EMQX 版本：

- EMQX 5.9 和 5.10
- EMQX 6.0 及更高版本

支持以下 API 版本：

- [apps.emqx.io/v3beta1](./reference/v3beta1-reference.md)

EMQX Operator 3.0 支持 Core 节点原地滚动更新、Replicant 节点 Deployment 式滚动发布，以及兼容 HPA 的 Core-Replicant 集群伸缩。该版本不向后兼容早期 EMQX CR API 版本。

### 历史版本

#### EMQX Operator 2.3.x

EMQX Operator 2.3.x 系列完全兼容以下 EMQX 版本：

- EMQX 5.9 和 5.10
- EMQX 6.0 及更高版本

支持以下 API 版本：

- [apps.emqx.io/v2](./reference/v2-reference.md)
- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md)（已弃用）

#### EMQX Operator 2.2.x

EMQX Operator 2.2.x 系列兼容以下 EMQX 版本：

- EMQX Open Source 和 EMQX Enterprise 5.1.1–5.8.x
- EMQX 5.9 和 5.10（有限支持<sup>*</sup>）
- EMQX 6.0 及更高版本（有限支持<sup>*</sup>）

支持以下 API 版本：

- [apps.emqx.io/v2beta1](./reference/v2beta1-reference.md)
- apps.emqx.io/v2alpha1（已弃用）
- apps.emqx.io/v1beta4
- apps.emqx.io/v1beta3（已弃用）

::: tip
<sup>*</sup> 这些版本不支持自动管理持久存储（Durable Storage）数据副本。
:::
