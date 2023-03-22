# Integrate with Prometheus

[Prometheus](https://prometheus.io/) is the monitoring solution open-sourced by SoundCloud, featuring its support to  multidimensional data model, flexible query language, and powerful alarm management.

EMQX provides the following endpoints to integrate with Prometheus to collect system metrics: `http://127.0.0.1:18083/api/v5/prometheus/stats`. 

EMQX also supports pushing metrics to `pushgateway`, which is disabled by default. You can enable the option by adding the following configurations to `etc/emqx.conf`:

```bash
prometheus {

  ## URL of Prometheus server
  ## @path prometheus.push_gateway_server
  ## @type string()
  ## @default "http://127.0.0.1:9091"
  push_gateway_server: "http://127.0.0.1:9091"


  ## Data reporting interval.
  ## @path prometheus.interval
  ## @type emqx_schema:duration_ms()
  ## @default 15s
  interval: 15s


  ## Turn Prometheus data pushing on or off
  ## @path prometheus.enable
  ## @type boolean()
  ## @default false
  enable: true
}
```
## Configure With EMQX Dashboard

{% emqxce %}

Since EMQX 5.0.4, you can use EMQX Dashboard for configuration. In the EMQX Dashboard, click **Configuration** -> **Monitoring** on the left navigation tree, then click the **Integration** tab for the configuration, which takes effect immediately after saving without needing to restart the node.

{% endemqxce %}

{% emqxee %}

You can use EMQX Dashboard to configure mornitoring data integration to Prometheus platform. In the EMQX Dashboard, click **Configuration** -> **Monitoring** on the left navigation tree, then click the **Integration** tab for the configuration.

{% endemqxee %}


## Use Grafana to Visualize EMQX Metrics

You can also use Grafana with Prometheus to visualize EMQX metrics, which can be achieved by importing the EMQX template files into Grafana. 

To download the template, click [EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/) or click the Help button at the bottom of the **Integration** tab of the Dashboard **Monitoring** page.

:::tip

For detailed operating steps, see [*Monitoring MQTT broker with Prometheus and Grafana*](https://www.emqx.com/en/blog/emqx-prometheus-grafana)

:::
