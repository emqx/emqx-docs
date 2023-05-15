# Prometheus

[Prometheus](https://prometheus.io/) is the monitoring solution open-sourced by SoundCloud, featuring its support for a multidimensional data model, flexible query language, and powerful alarm management.

You can use Dashboard to confiture monitoring data integration to the Prometheus platform, for detailed operation steps, see [Integrate with Prometheus](../observability/prometheus).

Or work with the `emqx.conf` configuration file: 

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

:::tip

To configure listeners via Dashboard,  click **Configuration** -> **Monitoring** -> **Integration** on the left navigation menu of the Dashboard. Once you configured these items with the Dashboard, your settings will override the same configuration items in `emqx.conf`.

{% emqxee %}

EMQX has offered more configuration items to better serve customized needs. For more configuration items, see [Configuration Manual](https://docs.emqx.com/zh/enterprise/v5.0/configuration/configuration-manual.html).

{% endemqxee %}

{% emqxce %}

EMQX has offered more configuration items to better serve customized needs. For more configuration items, see [Configuration Manual](https://www.emqx.io/docs/zh/v5.0/configuration/configuration-manual.html).

{% endemqxce %}

:::