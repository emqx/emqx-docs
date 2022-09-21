# Prometheus

EMQX provides the following Endpoints for Prometheus to collect system metrics: `http://127.0.0.1:18083/api/v5/prometheus/stats`. This API is always available.

EMQX also supports pushing metrics to `pushgateway`, that can be enabled by adding the following configuration to `etc/emqx.conf`:

```
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
## Configure through dashboard

After v5.0.4, EMQX also supports direct modification via Dashboard‘s **Configuration/Monitoring Integration**, which takes effect directly after saving, without restarting the node.


## Grafana Data Template

EMQX provides template files for Grafana Dashboard. These templates contain the display of all EMQX monitoring data. Users can directly import them into Grafana, and then display the status monitoring chart of EMQX.

The template file is located at: [emqx_prometheus/Grafana_template](https://github.com/emqx/emqx-prometheus/tree/master/grafana_template).
