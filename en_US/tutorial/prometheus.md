# Prometheus

{% emqxee %}

> Starting from EMQX Enterprise v4.1.0, `emqx_statsd` has been renamed to `emqx_prometheus`. Related plugin names and directory paths have changed accordingly.

EMQX Enterprise supports two ways to integrate with [Prometheus](https://prometheus.io):

- **Push mode** (all versions): The [`emqx_prometheus`](https://github.com/emqx/emqx-prometheus) plugin periodically pushes metrics to Prometheus Pushgateway, which Prometheus Server then scrapes.
- **Pull mode** (e4.4.34+): Prometheus Server directly scrapes EMQX's built-in HTTP endpoint `/api/v4/emqx_prometheus`, without requiring Pushgateway.

## Push Mode: `emqx_prometheus` Plugin

```
EMQX → [emqx_prometheus plugin] → Pushgateway → Prometheus Server
```

The `emqx_prometheus` plugin starts an internal timer that periodically collects EMQX monitoring data and pushes it to Pushgateway.

> Note: The `emqx_prometheus` plugin does not support being scraped directly by Prometheus. Pushgateway is required.

### Enable

Open EMQX Dashboard, go to **Modules**, and add **[EMQX Prometheus Agent](../modules/prometheus.md)**.

### Configuration

Configuration file: `etc/plugins/emqx_prometheus.conf`

| Configuration       | Type    | Default               | Description                      |
| ------------------- | ------- | --------------------- | -------------------------------- |
| push.gateway.server | string  | http://127.0.0.1:9091 | Prometheus Pushgateway address   |
| interval            | integer | 15000                 | Push interval (milliseconds)     |

For the full list of metrics pushed by `emqx_prometheus`, see [Metrics & Stats](../advanced/metrics-and-stats.md).

### Grafana Dashboard

`emqx_prometheus` provides Grafana Dashboard template files that display all EMQX monitoring data. Import them directly into Grafana to visualize EMQX monitoring status.

Template files: [emqx_prometheus/grafana_template](https://github.com/emqx/emqx-prometheus/tree/master/grafana_template)

## Pull Mode: HTTP Endpoint

> Available since EMQX Enterprise e4.4.34.

```
Prometheus Server → scrape → EMQX HTTP endpoint
```

Configure Prometheus to scrape the following endpoint:

```
http://localhost:8081/api/v4/emqx_prometheus
```

## HTTP API Metrics

> Available since EMQX Enterprise e4.4.34.

Starting from e4.4.34, EMQX Enterprise exposes two additional metrics for monitoring HTTP API request performance. These metrics are automatically included in the `emqx_prometheus` plugin data collection without additional configuration:

- **Push mode**: Metrics are periodically pushed to the PushGateway along with other metrics.
- **Pull mode**: Available via `GET /api/v4/emqx_prometheus?type=prometheus`, which returns data in Prometheus text format.
- **JSON mode**: Available via `GET /api/v4/emqx_prometheus` (default `type=json`), which returns counter data in JSON format.

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

`emqx_http_api_request_duration_milliseconds` uses the following bucket boundaries (milliseconds):

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
