# Prometheus

EMQX Broker provides [emqx_statsd](https://github.com/emqx/emqx-statsd) plug-in, which is used to output the monitoring data of the system to the third-party monitoring system.

Take  [Prometheus](https://prometheus.io) as an example:

`emqx_statsd` supports pushing data to Pushgateway, which is then pulled by Promethues Server for storage.

::: tip Tip
`emqx_statsd` does not support the pull operation of Prometheus.
:::

## Configuration

The `emqx_statsd` plugin internally starts a timer to collect the monitoring data in EMQX Broker every interval.

For the specific fields and meanings of the monitoring data pushed by `emqx_statsd`, see [Metrics & Stats](../advanced/metrics-and-stats.md)

The configuration file is located in `etc/plugins/emqx_statsd.conf`, where:

| Configuration       | Type    | Optional value | Default value         | Description                     |
| ------------------- | ------- | -------------- | --------------------- | ------------------------------- |
| push.gateway.server | string  | -              | http://127.0.0.1:9091 | Prometheus' PushGateway address |
| interval            | integer | > 0            | 5000                  | Push interval, unit: ms         |

### Grafana Data template

The `emqx_statsd` plugin provides Grafana ’s Dashboard template files. These templates contain the display of all EMQX Broker monitoring data. Users can directly import them into Grafana and select icons that display the monitoring status of EMQX Broker.

The template file is located:[emqx_statsd/grafana_template](https://github.com/emqx/emqx-statsd/tree/master/grafana_template)。

{% emqxee %}

## HTTP API Metrics

> Available since EMQX Enterprise e4.4.34.

EMQX Enterprise exposes HTTP API request metrics through the Prometheus endpoint. These metrics help monitor the health and performance of the REST API.

### Metrics

| Name | Type | Labels | Description |
| ---- | ---- | ------ | ----------- |
| `emqx_http_api_request_total` | Counter | `result`=`success` \| `failure` | Total number of HTTP API requests |
| `emqx_http_api_request_duration_milliseconds` | Histogram | None | HTTP API request duration distribution (milliseconds) |

### Result Classification

The `result` label of `emqx_http_api_request_total` is classified as follows:

- **success**: HTTP status code is 200 and business response code is `code=0`.
- **failure**: All other cases, including:
  - Authentication failure (HTTP 401)
  - Route not found (HTTP 404)
  - Server error (HTTP 500)
  - Non-zero business error code
  - Permission denied

### Duration Histogram Buckets

`emqx_http_api_request_duration_milliseconds` uses the following bucket boundaries (in milliseconds):

`5, 10, 25, 50, 100, 250, 500, 1000`

### Prometheus Queries

```promql
# API request rate (requests per second)
rate(emqx_http_api_request_total[5m])

# Request rate grouped by result
sum by (result) (rate(emqx_http_api_request_total[5m]))

# Failure rate
sum(rate(emqx_http_api_request_total{result="failure"}[5m]))
/
sum(rate(emqx_http_api_request_total[5m]))

# P99 request duration
histogram_quantile(0.99, rate(emqx_http_api_request_duration_milliseconds_bucket[5m]))

# Average request duration
rate(emqx_http_api_request_duration_milliseconds_sum[5m])
/
rate(emqx_http_api_request_duration_milliseconds_count[5m])
```

{% endemqxee %}
