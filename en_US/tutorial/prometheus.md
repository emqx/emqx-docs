# Prometheus

> Starting from EMQ X v4.2.0, emqx_statsd has been renamed to emqx_prometheus, and the names and directories of related plugins have been changed.

EMQ X Broker provides [emqx_prometheus](https://github.com/emqx/emqx-prometheus) plug-in, which is used to output the monitoring data of the system to the third-party monitoring system.

Take  [Prometheus](https://prometheus.io) as an example:

`emqx_prometheus` supports pushing data to Pushgateway, which is then pulled by Promethues Server for storage.

::: tip Tip
`emqx_prometheus` does not support the pull operation of Prometheus.
:::

## Configuration

The `emqx_prometheus` plugin internally starts a timer to collect the monitoring data in EMQ X Broker every interval.

For the specific fields and meanings of the monitoring data pushed by `emqx_prometheus`, see [Metrics & Stats](../advanced/metrics-and-stats.md)

The configuration file is located in `etc/plugins/emqx_prometheus.conf`, where:

| Configuration       | Type    | Optional value | Default value         | Description                     |
| ------------------- | ------- | -------------- | --------------------- | ------------------------------- |
| push.gateway.server | string  | -              | http://127.0.0.1:9091 | Prometheus' PushGateway address |
| interval            | integer | > 0            | 5000                  | Push interval, unit: ms         |

### Grafana Data template

The `emqx_prometheus` plugin provides Grafana ’s Dashboard template files. These templates contain the display of all EMQ X Broker monitoring data. Users can directly import them into Grafana and select icons that display the monitoring status of EMQ X Broker.

The template file is located:[emqx_prometheus/grafana_template](https://github.com/emqx/emqx-prometheus/tree/master/grafana_template)。
