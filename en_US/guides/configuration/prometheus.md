# Prometheus Configuration

[Prometheus](https://prometheus.io/) is the monitoring solution open-sourced by SoundCloud, featuring its support for a multidimensional data model, flexible query language, and powerful alarm management. EMQX supports integration with Prometheus as a third-party monitoring system. For more information about this feature, see [Integrate with Prometheus](../observability/prometheus.md).

You can enable and configure the Pushgateway through the `emqx.conf` configuration file, for example: 

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

where, <!--did not add the Dashboard UI, as it is quite obvious-->

- `push_gateway_server` is to set the URL of a Prometheus push gateway server, which is used to push metrics to Prometheus, default: `http://127.0.0.1:9091`.
- `interval` is to set the interval at which metrics will be collected and exported to Prometheus, default: `15s`.
- `headers` is to set the additional headers to be included in the HTTP request made to the Prometheus push gateway server.
- `job_name` is to set the name of the job in Prometheus to which the metrics will be exported, default: `"${name}/instance/${name}~${host}"`

::: tip

You can also use Dashboard to configure the push mode integration with the Prometheus by clicking **Management** -> **Monitoring** -> **Integration** on the left navigation menu.

Once you configured the integration via the Dashboard, your settings will override the same configuration items in `emqx.conf`.

:::

::: tip

EMQX offers more configuration items to serve customized needs better. For details, see the [EMQX Open Source Configuration Manual](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) and [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::
