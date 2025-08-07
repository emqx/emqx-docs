# 在 Kubernetes 上部署 EMQX

在 Kubernetes 上部署 EMQX 主要有两种方法：使用 EMQX Operator 或使用官方的 Helm Chart。两种方法都得到了完全支持，但它们满足了不同的需求和操作复杂性级别。

本文档将通过概述每种方法的优缺点并提供详细文档的链接，帮助您为您的场景选择最佳方法。

## 推荐方法：EMQX Operator

EMQX Operator 是在 Kubernetes 上部署和管理 EMQX 集群的推荐方法，特别是对于生产环境或需要高级生命周期管理的场景。Operator 是一个扩展 Kubernetes API 的软件，代表用户创建、配置和管理 EMQX 实例。

[**了解如何使用 EMQX Operator 部署](./operator/operator.md)**

### 优点

- **自动化运维：** Operator 自动化了集群扩展、升级和故障恢复等复杂任务，减少了手动工作和潜在的错误。
- **高级生命周期管理：** 支持蓝绿部署等复杂的部署策略，确保零停机升级和优雅的连接迁移。
- **简化配置：** 通过高级别的自定义资源定义（CRD）管理 EMQX，使配置比繁杂的 Helm values 更具声明性且更易于管理。
- **封装专业知识：** Operator 封装了运行像 EMQX 这样的有状态应用的运维知识，确保遵循最佳实践。

### 缺点

- **额外组件：** 需要在您的 Kubernetes 集群中安装和维护 Operator 本身。
- **学习曲线较陡：** 用户需要熟悉 Kubernetes Operator 的概念和特定的 EMQX CRD。

## 替代方法：Helm Chart

EMQX Helm Chart 提供了一种使用流行的包管理器 Helm 在 Kubernetes 上部署 EMQX 的直接方法。此方法非常适合快速入门、开发/测试环境，或偏好直接管理 Kubernetes 资源的用户。

[**了解如何使用 Helm 部署](./chart.md)**

### 优点

- **简单和熟悉：** Helm 是 Kubernetes 生态系统中广泛采用的工具，使其成为许多用户的熟悉切入点。
- **直接控制：** 通过 `values.yaml` 文件对生成的 Kubernetes 资源（如 StatefulSets、Services 和 ConfigMaps）提供直接、精细的控制。
- **无额外依赖：** 不需要一个单独的 operator 控制器在集群中运行。

### 缺点

- **手动管理：** 升级、扩展和复杂的配置更改等生命周期操作更加手动化，自动化程度较低。
- **自动化有限：** 缺乏内置的高级功能，如自动化的蓝绿部署。第二天的运维完全是用户的责任。
- **配置复杂性：** 对于生产级别的设置，`values.yaml` 文件可能会变得庞大且难以管理。

## 您应该选择哪种方法？

- **对于大多数生产和重要的预生产用例，强烈推荐使用 EMQX Operator。** 它简化了长期管理并减少了运维开销。
- **对于快速评估、开发、测试，或者如果您非常偏好使用 Helm 直接管理资源，Helm Chart 是一个绝佳的选择。**

