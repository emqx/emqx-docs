# 功能门控

从 EMQX 6.3.0 起，功能门控可用于在部署阶段启用或禁用 EMQX 可选功能。它只在 EMQX 启动时解析，不能在运行时动态修改。

功能门控通过 `EMQX_FEATURES` 环境变量配置，主要用于部署策略控制。它不属于 HOCON 配置，不会持久化到 `cluster.hocon`，也不能通过 Dashboard、REST API 或 CLI 修改。如需变更启用的功能集，需要在部署环境中修改 `EMQX_FEATURES`，然后重启 EMQX。

## 配置功能门控

可以将 `EMQX_FEATURES` 设置为以下值：

| 值 | 说明 |
| --- | --- |
| 未设置或为空 | 使用 `FULL` 预设，保持 EMQX 默认行为。 |
| `FULL` | 启用所有可选功能。 |
| `ESSENTIAL` | 禁用所有可选功能，仅保留 EMQX 作为 MQTT Broker 运行和基础管理所需的核心能力。 |
| 自定义功能列表 | 启用列出的功能及其依赖功能。功能名称使用小写字母，多个功能名可用英文逗号、空格或二者组合分隔。 |

示例：

```bash
export EMQX_FEATURES=FULL
export EMQX_FEATURES=ESSENTIAL
export EMQX_FEATURES=dashboard,auth
export EMQX_FEATURES="dashboard auth"
export EMQX_FEATURES=dashboard,auth,data_integration,metrics
```

`dashboard,auth` 和 `"dashboard auth"` 的效果相同。如果使用空格分隔功能名称，需要用引号包裹整个值，避免 shell 将其拆分。

不要混用预设名称和功能名称。例如，`EMQX_FEATURES=ESSENTIAL,metrics` 是无效值。

::: warning
无效值会阻止 EMQX 启动。如果功能名称未知，EMQX 会记录 `invalid_feature_specification` 日志，其中 `reason` 为 `unknown_feature`，然后以非零状态退出。
:::

## 可用功能

以下可选功能可用于自定义 `EMQX_FEATURES` 列表：

| 功能 | 说明 |
| --- | --- |
| `dashboard` | Dashboard UI、REST API、Dashboard 基于角色的访问控制和 Dashboard 单点登录。 |
| `auth` | 认证和授权链及其后端。 |
| `data_integration` | 规则引擎、连接器、动作、Source 和数据桥接。 |
| `message_transformation` | 消息转换。 |
| `schema_validation` | Schema 验证。 |
| `schema_registry` | Schema Registry 以及相关功能使用的 Schema 定义。 |
| `gateways` | 非 MQTT 协议网关。 |
| `cluster_link` | 集群连接。 |
| `multi_tenancy` | 多租户和命名空间管理。 |
| `ai` | AI 功能，包括 AI Completion 和 Agent-to-Agent Registry。 |
| `metrics` | Prometheus 指标导出。 |
| `mqtt_extensions` | MQTT 扩展功能，例如延迟发布、主题重写、主题指标、自动订阅、慢订阅统计、MQTT Streams 和 Message Queue。 |
| `file_transfer` | MQTT 文件传输。 |
| `gcp_device` | Google IoT Core 迁移兼容功能。 |
| `exhook` | 外部 gRPC Hook。 |
| `opentelemetry` | OpenTelemetry 导出器。 |

## 功能依赖

部分功能需要依赖其他功能才能工作。启用某个功能时，EMQX 会自动启用其依赖功能。

| 功能 | 自动启用的依赖功能 |
| --- | --- |
| `data_integration` | `schema_registry` |
| `message_transformation` | `schema_registry` |
| `schema_validation` | `schema_registry` |
| `ai` | `schema_registry` |
| `gateways` | `auth` |
| `gcp_device` | `auth` |
| `metrics` | `dashboard`、`auth` |
| `opentelemetry` | `dashboard` |

例如，设置 `EMQX_FEATURES=metrics` 会启用 `metrics`、`dashboard` 和 `auth`。

## 核心应用

`ESSENTIAL` 预设会禁用所有可选功能，仅保留 EMQX 作为 MQTT Broker 运行和基础管理所需的核心能力。核心能力包括 MQTT Broker、配置系统、CLI、License 校验、插件框架、持久存储、审计日志、节点重平衡、保留消息、TLS PSK、遥测上报，以及共享的资源和桥接框架应用。

当某个功能门控被禁用时，对应功能的配置项可以继续保留在配置文件中。EMQX 不会启动已禁用功能背后的应用，因此这些配置项不会被使用，直到再次启用对应功能。

## 查看已启用功能

在 Dashboard 中，点击**监控** -> **集群概览** -> **节点**，查看**功能预设**列。该列显示各节点启动时使用的 `full`、`essential` 或 `custom` 预设。`custom` 表示 `EMQX_FEATURES` 包含显式指定的功能列表。停止的节点和运行 6.3 之前版本的节点不会报告功能预设。

Dashboard 仅显示预设。如需查看实际启用和禁用的功能列表，请使用启动日志或 REST API。

EMQX 会在启动时记录解析后的功能状态：

```text
feature_gates_resolved
```

该日志包含解析后的 `preset`、`enabled` 功能列表和 `disabled` 功能列表。

启用 `dashboard` 功能后，也可以通过 REST API 查询解析后的功能状态：

```bash
curl -u <API_KEY>:<SECRET_KEY> http://localhost:18083/api/v5/features
```

响应示例：

```json
{
  "preset": "custom",
  "enabled": ["auth", "dashboard", "metrics"],
  "disabled": ["ai", "cluster_link", "data_integration"]
}
```

具体列表取决于 EMQX 版本、版本类型以及配置的功能集。

::: tip
`GET /api/v5/features` 由 Dashboard 和管理 API 应用提供。如果禁用了 `dashboard` 功能，请通过启动日志查看解析后的功能集。
:::

## 集群部署注意事项

为保持运行行为一致，建议在同一集群的所有节点上配置相同的 `EMQX_FEATURES` 值。混合不同功能集的集群可能导致不同节点暴露的 REST API、后台应用和跨节点行为不一致。

使用 Docker Compose、Kubernetes 或其他编排系统时，请在共享的部署清单中设置 `EMQX_FEATURES`，确保新增节点和重启后的节点使用相同的值。

## 相关链接

- [通过 Docker 运行 EMQX](./install-docker.md)
- [使用 Helm Chart 在 Kubernetes 中部署 EMQX](./kubernetes/chart.md)
- [配置文件](../configuration/configuration.md)
- [REST API](../admin/api.md)
