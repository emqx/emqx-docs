# 在 Kubernetes 中部署 EMQX

EMQX 支持通过以下两种方式在 Kubernetes 中部署：

- **EMQX Operator**
- **EMQX Helm chart**

这两种方式分别适用于不同的使用场景和运维需求。本文将介绍各自的优劣势，帮助你选择最适合的部署策略。

## 推荐方式：使用 EMQX Operator

EMQX Operator 是在 Kubernetes 上部署和管理 EMQX 集群的推荐方式，特别适用于生产环境或需要高级生命周期自动化的场景。

Operator 由 EMQX 官方团队开发和维护，专为在 Kubernetes 中原生部署、配置和管理 EMQX 集群而设计。它通过自定义资源定义（CRD）扩展 Kubernetes API，支持声明式集群管理，并自动化执行复杂的运维任务，如弹性伸缩、版本升级和故障恢复。

[查看 EMQX Operator 部署指南](./operator/operator.md)

### 主要优势

- **自动化运维：** Operator 可自动完成集群扩容、升级和故障恢复等复杂任务，降低人工操作和错误风险。
- **高级生命周期管理：** 支持蓝绿部署等高级策略，实现无中断升级和连接迁移。
- **配置简化：** 通过高层级的 CRD 管理 EMQX，配置更加声明式且易于维护，相比 Helm 的 `values.yaml` 更简洁。
- **内置最佳实践：** Operator 封装了运行有状态应用（如 EMQX）所需的运维经验，保障部署符合最佳实践。

### 潜在限制

- **需要部署 Operator 控制器：** 需要在集群中额外部署并维护 EMQX Operator 控制器。
- **需要额外的学习成本：** 需要用户了解 Kubernetes Operator 概念及 EMQX 的自定义资源使用方法。

## 替代方式：使用 Helm Chart

EMQX Helm chart 提供了一种灵活且简洁的方式，通过 Kubernetes 生态中最流行的包管理工具 Helm 来部署 EMQX。此方式非常适合快速评估、开发/测试环境，或偏好直接管理 Kubernetes 资源的用户和团队。

该 Helm chart 由 EMQX 官方团队维护，封装了部署所需的全部 Kubernetes 对象，用户可通过配置 `values.yaml` 文件定义部署参数，实现可重复和可定制的部署，而无需手动编写 YAML 文件。

[查看 Helm Chart 部署指南](./chart.md)

### 主要优势

- **简单易用：** Helm 是 Kubernetes 社区广泛使用的工具，用户熟悉度高，易于上手。
- **直接控制：** 通过 `values.yaml` 文件对 StatefulSet、Service、ConfigMap 等 Kubernetes 资源进行细粒度控制。
- **无需额外依赖：** 不需要运行额外的 Operator 控制器，部署更轻量。

### 潜在限制

- **手动管理：** 升级、扩容、复杂配置变更等生命周期操作需要手动完成，自动化程度较低。
- **缺乏高级自动化：** 不支持内置的蓝绿部署等高级策略。所有 Day-2 运维任务（如扩容、升级、维护）都需由用户手动执行。
- **配置复杂度高：** 在生产环境下，`values.yaml` 文件可能变得庞大且难以维护。

## 如何选择部署方式

选择 EMQX Operator 还是 Helm chart 取决于你的部署目标、环境成熟度以及运维偏好。以下建议可帮助你做出决策：

- 对于大多数生产环境或重要的预发布环境，强烈推荐使用 EMQX Operator。 它简化了长期集群管理，降低了运维负担。
- 对于快速评估、开发测试场景，或需要对 Kubernetes 资源进行直接控制的情况，Helm chart 是一个轻量且灵活的选择。
