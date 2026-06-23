# Prometheus 配置

[Prometheus](https://prometheus.io/) 是由 SoundCloud 开源的监控解决方案，特点是支持多维数据模型、灵活的查询语言和强大的告警管理。EMQX 支持与 Prometheus 集成，作为第三方监控系统。有关此功能的更多信息，请参见[集成 Prometheus](../observability/prometheus.md)。

您可以通过 `emqx.conf` 配置文件启用和配置 Pushgateway，例如：

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

其中，

- `push_gateway_server` 用于设置 Prometheus Pushgateway 服务器的 URL，用于将指标推送到 Prometheus，默认为：`http://127.0.0.1:9091`。
- `interval` 用于设置将指标收集和导出到 Prometheus 的间隔，默认为：`15s`。
- `headers` 用于设置向 Prometheus Pushgateway 服务器发出的 HTTP 请求中要包含的额外头信息。
- `job_name` 用于设置 Prometheus 中将导出指标的 Job 名称，默认为：`"${name}/instance/${name}~${host}"`

::: tip

EMQX 提供了更多配置项以更好地满足定制化需求。详情请参见 [EMQX 开源版配置手册](https://docs.emqx.com/zh/emqx/v@CE_VERSION@/hocon/)和 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

:::

::: tip

您也可以在 Dashboard 中通过点击左侧导航菜单的 **管理** -> **监控** -> **监控集成**，配置与 Prometheus 的 Push 模式集成。一旦您通过 Dashboard 配置了集成，您的设置将覆盖 `emqx.conf` 中相同的配置项。

:::
