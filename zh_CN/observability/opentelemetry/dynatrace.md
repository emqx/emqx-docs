# 将 OpenTelemetry 与 Dynatrace 集成

从 EMQX 6.3.0 开始，EMQX 支持将 OpenTelemetry 日志和追踪数据直接导出到 Dynatrace。该集成使用 OTLP HTTP/protobuf 协议，并通过 OAuth2 客户端凭据（client credentials）进行认证。

::: tip 提示

在 EMQX 6.3.0 中，Dynatrace 集成仅支持 OpenTelemetry 日志和追踪。`type = dynatrace` 不支持指标。

:::

## 工作原理

当 `opentelemetry.type` 设置为 `dynatrace` 时，EMQX 将使用 Dynatrace 专用的导出器配置。EMQX 会从配置的 Dynatrace token endpoint 获取 OAuth2 access token，并将其作为 `Bearer` token 添加到 `Authorization` 请求头中，然后通过 OTLP HTTP/protobuf 将已启用的日志和追踪数据导出到 Dynatrace。

对于 OTLP HTTP endpoint，EMQX 会根据导出的信号类型自动追加对应路径。这些路径与 Dynatrace 标准 OTLP ingest 路径一致：

- 日志：`/v1/logs`
- 追踪：`/v1/traces`

因此，`opentelemetry.exporter.endpoint` 应配置为 Dynatrace OTLP 基础 URL，不要包含具体信号路径。

## 前置准备

配置 EMQX 前，请先在 Dynatrace 中准备以下信息。有关 endpoint 格式和 ingest scope，参见 [Dynatrace OTLP API endpoints](https://docs.dynatrace.com/docs/ingest-from/opentelemetry/otlp-api)。

- Dynatrace OTLP 基础 URL。
  - Dynatrace SaaS：`https://{your-environment-id}.live.dynatrace.com/api/v2/otlp`
  - Environment ActiveGate：`https://{your-activegate-domain}:9999/e/{your-environment-id}/api/v2/otlp`
- OAuth2 client ID 和 client secret。
- OAuth2 token endpoint，例如 `https://sso.dynatrace.com/sso/oauth2/token`。
- OAuth2 resource，例如 `urn:dtaccount:{your-account-uuid}`。
- 与要导出的信号匹配的 Dynatrace scope，例如用于追踪的 `openTelemetryTrace.ingest` 和用于日志的 `logs.ingest`。

## 通过 Dashboard 配置 Dynatrace

您可以在 EMQX Dashboard 中配置 Dynatrace 集成：

1. 在左侧导航栏中，点击**管理** -> **监控**。
2. 在**监控**页面，点击**监控集成**标签页。
3. 在**监控平台**中选择 **OpenTelemetry**。
4. 在 **OpenTelemetry 类型**中选择 **Dynatrace**。
5. 在**功能选择**中选择**追踪**、**日志**，或同时选择两者。Dynatrace 集成不支持指标。
6. 在**服务地址**中输入 Dynatrace OTLP 基础 URL。不要追加 `/v1/logs` 或 `/v1/traces`。
7. 可选：在**请求头**中点击**添加**，添加 EMQX 发送到 Dynatrace OTLP endpoint 的额外 HTTP 请求头。无需添加 `Authorization` 请求头；EMQX 会自动获取 access token 并添加该请求头。
8. 如果 Dynatrace OTLP endpoint 使用 HTTPS，打开导出器的**启用 TLS**开关。

在 **OAuth2 认证**区域中，配置以下字段：

| 字段 | 说明 |
| --- | --- |
| **令牌端点** | Dynatrace OAuth2 token endpoint。 |
| **客户端 ID** | OAuth2 client ID。 |
| **客户端密钥** | OAuth2 client secret。 |
| **资源** | Dynatrace 所需的 OAuth2 resource，格式为 `urn:dtaccount:{your-account-uuid}`。 |
| **作用域** | 可选的 OAuth2 scope。当 Dynatrace 要求 scope 时，请根据已启用的信号进行配置。 |
| **超时时间** | token 请求的超时时间。 |
| **启用 TLS** | token endpoint 使用 HTTPS 时，启用 token 请求的 TLS。 |

如果选择了**追踪**，可根据需要配置**追踪模式**、**追踪全部消息**、**追踪导出间隔**和**最大队列大小**。

如果选择了**日志**，可根据需要配置**日志级别**和**日志导出间隔**。

点击**保存修改**应用配置。

## 通过 HOCON 配置 Dynatrace

将以下配置添加到 `etc/base.hocon`，或通过 REST API 应用等效配置：

```hocon
opentelemetry {
  type = dynatrace

  exporter {
    endpoint = "https://{your-environment-id}.live.dynatrace.com/api/v2/otlp"

    auth {
      kind = dynatrace_oauth2
      enable = true
      token_endpoint = "https://sso.dynatrace.com/sso/oauth2/token"
      client_id = "{your-client-id}"
      client_secret = "{your-client-secret}"
      resource = "urn:dtaccount:{your-account-uuid}"
      scope = "openTelemetryTrace.ingest logs.ingest"
    }

    ssl_options {
      enable = true
    }
  }

  logs {
    enable = true
    level = warning
  }

  traces {
    enable = true
    scheduled_delay = "5s"
  }
}
```

请将示例中的占位符替换为您的 Dynatrace 环境、账号和 OAuth2 client 信息。

## 配置说明

- `opentelemetry.type`：设置为 `dynatrace`，表示使用 Dynatrace 专用的 OpenTelemetry 配置。
- `opentelemetry.exporter.endpoint`：设置为 Dynatrace OTLP 基础 URL。不要追加 `/v1/logs` 或 `/v1/traces`；EMQX 会自动追加对应路径。
- `opentelemetry.exporter.auth.kind`：设置为 `dynatrace_oauth2`。
- `opentelemetry.exporter.auth.enable`：设置为 `true`，启用 OAuth2 token 获取。
- `opentelemetry.exporter.auth.resource`：Dynatrace OAuth2 client credentials flow 所需字段。EMQX 在请求 access token 时会将其作为 `resource` 参数发送。
- `opentelemetry.exporter.auth.scope`：在 EMQX 中为可选字段，但 Dynatrace 可能要求 scope 与已启用的信号匹配。
- `opentelemetry.exporter.ssl_options.enable`：导出到 Dynatrace SaaS 或 HTTPS ActiveGate endpoint 时，应启用 TLS。

您可以启用 `logs`、`traces`，或同时启用两者。当 `opentelemetry.type = dynatrace` 时，不要配置 `metrics`。

## 验证集成

重启 EMQX 或应用配置后，生成日志事件或 MQTT 追踪事件，然后在 Dynatrace 中查看是否收到 OpenTelemetry 日志或追踪数据。

如果 Dynatrace 中没有出现数据：

- 确认 endpoint 是 OTLP 基础 URL，且没有包含 `/v1/logs` 或 `/v1/traces`。
- 确认 OAuth2 client ID、client secret、token endpoint、resource 和 scope 配置正确。
- 确认网络允许 EMQX 连接 Dynatrace endpoint。
- 检查 EMQX 日志中是否存在 OAuth2 token 请求或数据导出错误。
