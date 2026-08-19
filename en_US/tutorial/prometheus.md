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
http://localhost:8081/api/v4/emqx_prometheus?type=prometheus
```

> Note: The default response format is JSON (`type=json`). Add `?type=prometheus` to get Prometheus text format.

## HTTP API Metrics

> Available since EMQX Enterprise e4.4.34.

Starting from e4.4.34, EMQX Enterprise exposes two additional metrics for monitoring HTTP API request performance. These metrics are automatically included in the `emqx_prometheus` plugin data collection without additional configuration, and are available via both push mode and pull mode.

To query the request counter directly in JSON format, use:

```
GET /api/v4/http_api_metrics
```

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

## Inter-Node Network Health Probe

> Available since EMQX Enterprise e4.4.38.

The `emqx_erpc_probe` plugin monitors the health of links between cluster nodes. Each node maintains an independent probe process for every other node in the cluster and periodically issues `erpc:call(Peer, erlang, node, [], Timeout)` probes at `erpc_probe.probe_interval` (default `1s`), with a timeout of `erpc_probe.probe_timeout` (default `5s`).

The plugin exposes probe results as standard Prometheus metrics. These metrics are available with EMQX Prometheus metrics in pull mode or through the `emqx_prometheus` plugin:

| Name | Type | Labels | Description |
| ---- | ---- | ------ | ----------- |
| `emqx_erpc_probe_result_total` | Counter | `peer`, `result` | Counts probe results by peer and result. |
| `emqx_erpc_probe_duration_seconds` | Histogram | `peer` | Measures the round-trip time of successful probes. The p99 latency can be used to detect inter-node links that have become slow but have not yet timed out. |

The `result` label can be `ok`, `timeout`, `noconnection`, or `system_limit`. The non-success values have the following meanings:

- `timeout`: The nodes are connected, but the peer does not respond, indicating a degraded link or an overloaded peer.
- `noconnection`: No distributed connection exists, indicating that the peer VM is down or fully isolated.
- `system_limit`: The peer has exhausted its processes.

The configuration file is located at `etc/plugins/emqx_erpc_probe.conf`:

| Configuration | Default | Description |
| ------------- | ------- | ----------- |
| `erpc_probe.probe_enable` | `on` | Whether to enable probe workers. When disabled, the metric series are still exposed but remain at zero. |
| `erpc_probe.probe_interval` | `1s` | Minimum interval between two consecutive probes of one peer |
| `erpc_probe.probe_timeout` | `5s` | Timeout of each `erpc:call` probe |
| `erpc_probe.probe_buckets` | `0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5` | Histogram bucket upper bounds (seconds), strictly increasing |

::: tip Note

- The plugin is enabled by default for new installations (see `data/loaded_plugins`).
- Clusters upgraded from an earlier version keep their existing `data/loaded_plugins`, so the plugin is not started automatically. To enable it, run `./bin/emqx ctl plugins load emqx_erpc_probe`, or add `{emqx_erpc_probe, true}.` to `data/loaded_plugins` and restart the node.
- Configuration is read at plugin startup; restart the plugin or node after making changes.

:::

{% endemqxee %}
