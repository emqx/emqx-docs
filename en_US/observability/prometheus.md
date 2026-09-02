# Monitor EMQX with Prometheus

EMQX exposes runtime metrics that Prometheus can collect for querying, alerting, and visualization. You can collect these metrics in either of the following ways:

- **Pull mode (recommended)**: Prometheus scrapes EMQX metrics directly from REST API endpoints. Use this mode to collect the complete set of available metrics.
- **Push mode**: EMQX sends basic metrics to Pushgateway, and Prometheus scrapes them from Pushgateway. Use this mode when Prometheus cannot connect directly to EMQX.

You can then use Grafana to [visualize the collected EMQX metrics](#visualize-emqx-metrics-in-grafana).

::: tip
Starting from EMQX 6.3.0, Prometheus metrics are controlled by the `metrics` feature gate. If you set `EMQX_FEATURES` manually, enabling `metrics` also enables its required `dashboard` and `auth` dependencies. For more information, see [Feature Gates](../deploy/feature-gates.md).
:::

## Configure Metric Collection in EMQX

Use the Prometheus integration page in Dashboard to control authentication, Pushgateway delivery, latency buckets, and namespace request limits. This section explains what each setting controls. The later sections provide the complete procedures for configuring Pull and Push modes.

To open the Prometheus integration settings:

1. Go to **Management** -> **Monitoring** in the EMQX Dashboard.
2. Select the **Integration** tab.
3. Select **Prometheus**.

<img src="./assets/enable-push-gateway.png" alt="Prometheus integration settings" style="zoom: 67%;" />

For a curated reference of the metric series exposed on the endpoints below (including the ones worth alerting on), see [Broker Health Indicators](./broker-health-indicators.md).

### Require Authentication for Scrape Requests

**Enable Basic Auth** controls authentication for all Prometheus scrape APIs under `/api/v5/prometheus/*`. Despite the Dashboard label, this setting controls both HTTP Basic and Bearer authentication.

Starting from EMQX 6.3.0, authentication is enabled by default. Requests without credentials return `401`. Prometheus can use HTTP Basic authentication with an API key and secret key. EMQX also accepts a Dashboard login token as a Bearer token, but the token expires and is not suitable for persistent scraping.

For a long-running Prometheus server, create a dedicated API key with the `monitoring` scope and [configure Prometheus to authenticate its scrape requests](#authenticate-prometheus-scrape-requests).

To allow unauthenticated scraping, turn off **Enable Basic Auth** or set `prometheus.enable_basic_auth = false`. This setting affects only Pull mode and does not affect Pushgateway delivery.

::: warning Important Notice
Disabling authentication allows any client that can reach the Dashboard listener to scrape EMQX metrics. After an upgrade, configurations that explicitly set `prometheus.enable_basic_auth = false` and legacy-format Prometheus configurations continue to allow unauthenticated scraping. Check **Enable Basic Auth** in the Dashboard after upgrading.
:::

### Configure Metric Delivery to Pushgateway

Turn on **Enable Pushgateway** to make EMQX send metrics to a Pushgateway instance. Push delivery is disabled by default. Configure the following fields:

| Field | Description |
| --- | --- |
| **Interval** | How often EMQX pushes metrics. The default is `15` seconds. |
| **Pushgateway Server** | Pushgateway URL. The default is `http://127.0.0.1:9091`. |
| **Job Name** | Job label for the pushed metrics. The default is `${name}/instance/${name}~${host}`, where `${name}` is the node name before `@` and `${host}` is the host after `@`. For `emqx@127.0.0.1`, the values are `emqx` and `127.0.0.1`. |
| **Headers** | Optional HTTP headers sent to Pushgateway. Add each header as a key-value pair, for example, `Authorization = "some-auth-token"`. |

For the complete procedure, see [Configure EMQX to Push Metrics to Pushgateway](#configure-push-mode-integration).

### Define Latency Histogram Buckets

In **Latency Buckets**, enter a comma-separated list of duration values, for example:

```text
10ms, 100ms, 1s, 5s, 30s
```

These values define the bucket boundaries for latency histograms in both Pull and Push modes. More buckets provide finer granularity but may increase metric cardinality and storage usage.

### Limit Requests That Scrape All Namespaces

**Namespace Data Scraping Rate Limit** sets the maximum request rate for scraping metrics across all namespaces. Requests for a specific namespace are not limited. Enter a value in the format `<requests>/<duration>`. The default value `1/5s` allows one request every 5 seconds; additional requests within that interval are rejected.

## Configure Prometheus to Scrape EMQX Metrics

In Pull mode, Prometheus connects to the EMQX Dashboard listener and scrapes one or more REST API endpoints.

### Select Metrics Endpoints to Scrape

Add a Prometheus scrape job for each metric category that you want to collect:

| Endpoint | Metrics |
| --- | --- |
| `/api/v5/prometheus/stats` | Basic EMQX metrics and counters |
| `/api/v5/prometheus/namespaced_stats` | Metrics aggregated by namespace |
| `/api/v5/prometheus/auth` | Authentication, authorization, and banned-client metrics |
| `/api/v5/prometheus/data_integration` | Rule, connector, action, Sink/Source, and encoding/decoding metrics |
| `/api/v5/prometheus/schema_validation` | Schema validation metrics |
| `/api/v5/prometheus/message_transformation` | Message transformation metrics |
| `/api/v5/prometheus/topic_metrics` | Topic metric collection counters |

For the complete API reference, see the [EMQX Enterprise API documentation](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html).

### Scrape Data Integration Metrics by Namespace

Starting from EMQX 6.3.0, `GET /api/v5/prometheus/data_integration` limits rule, action, and connector metrics according to the namespace of the authenticated user:

- A namespaced user receives metrics only from their assigned namespace. If the user specifies another namespace with `ns=<namespace>`, EMQX returns `403`.
- A global administrator receives metrics from all namespaces by default. Specify `ns=<namespace>` to scrape one namespace, or specify `only_global=true` without `ns` to scrape only the global namespace.
- If authentication is disabled, EMQX applies the same visibility as for a global administrator. Requests return metrics from all namespaces by default.

Per-resource metrics for rules, actions, and connectors in a non-global namespace include a `namespace` label. Per-resource metrics in the global namespace do not include this label. The `emqx_schema_registrys_count` metric remains cluster-wide because Schema Registry resources are not scoped by namespace.

Requests that scrape all namespaces are subject to the [Namespace Data Scraping Rate Limit](#limit-requests-that-scrape-all-namespaces).

### Choose a Metric Collection Mode

For endpoints that support it, use the `mode` query parameter to control whether the endpoint returns metrics for the current node or the cluster.

:::: tabs type: card

::: tab Current Node

```text
mode=node
```

Returns metrics from the node that receives the request. This is the default mode.

:::

::: tab Aggregated Cluster

```text
mode=all_nodes_aggregated
```

Returns metrics for all running nodes with the following aggregation behavior:

- State metrics use logical aggregation. For example, a metric is `1` only when the state is enabled or running on every node; otherwise, it is `0`.
- Node-specific metrics, such as CPU and memory usage, are not aggregated. They retain a `node` label:

  ```text
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123
  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- Metrics that are consistent across the cluster return the value from the node that receives the request. They are not summed and do not include a `node` label:

  ```text
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```

- Other metrics return the arithmetic sum across all running nodes.

:::

::: tab Unaggregated Cluster

```text
mode=all_nodes_unaggregated
```

Returns individual metrics for all running nodes. Node-specific values include a `node` label:

```text
emqx_connections_count{node="emqx@127.0.0.1"} 0
```

Metrics that are consistent across the cluster return one value from the node that receives the request and do not include a `node` label:

```text
emqx_retained_count 3
```

:::

::::

### Topic Metrics

Starting from EMQX 6.3, `GET /api/v5/prometheus/topic_metrics` exposes counters for the named collections created through the Topic Metrics REST API. Create at least one collection before scraping this endpoint. For instructions, see [Manage Topic Metric Collections with the REST API](./topic-metrics.md#manage-topic-metric-collections-with-the-rest-api).

The endpoint exposes the following counters:

| Metric | Description |
| --- | --- |
| `emqx_topic_metric_messages_in_count` | Number of messages published to topics that match the collection filter. |
| `emqx_topic_metric_messages_out_count` | Number of matching messages delivered to subscribers. |
| `emqx_topic_metric_messages_dropped_count` | Number of matching messages dropped by EMQX. |
| `emqx_topic_metric_bytes_in` | Combined size of the topic and payload for matching published messages. |
| `emqx_topic_metric_bytes_out` | Combined size of the topic and payload for matching delivered messages. |

Each time series includes the `name` and `topic_filter` labels. Collections owned by a namespace also include the `namespace` label. When `mode=all_nodes_unaggregated`, each series includes the `node` label.

All Topic Metrics values are monotonic counters. Use a PromQL function such as `rate()` to calculate a per-second rate. For example:

```text
rate(emqx_topic_metric_messages_in_count[5m])
```

::: warning Important Notice

Each collection exposes five counters. In unaggregated mode, EMQX creates a separate time series for each node. Limit the number of collections to avoid creating excessive Prometheus time series.

:::

<a id="authentication"></a>

### Authenticate Prometheus Scrape Requests

Starting from EMQX 6.3.0, Prometheus scrape APIs require authentication by default. For persistent scraping, use HTTP Basic authentication with a dedicated API key:

1. Create an [API key](../admin/api.md#authentication) with the `monitoring` scope in EMQX.
2. Add the API key and secret key to each EMQX scrape job in `prometheus.yaml`:

   ```yaml
   basic_auth:
     username: '<API_KEY>'
     password: '<SECRET_KEY>'
   ```

EMQX also accepts a Bearer token obtained from `POST /api/v5/login`. Dashboard login tokens expire, so use an API key for long-running Prometheus scrapers.

### Add EMQX Scrape Jobs to Prometheus

The following `prometheus.yaml` example collects three common metric categories. Replace the target address and credentials, and add jobs for any other [metrics endpoints](#select-metrics-endpoints-to-scrape) that you need. Restart Prometheus after changing the file.

```yaml
global:
  scrape_interval: 10s
  evaluation_interval: 10s
  external_labels:
    monitor: 'emqx-monitor'

scrape_configs:
  - job_name: 'emqx_stats'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/stats'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'

  - job_name: 'emqx_auth'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/auth'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'

  - job_name: 'emqx_data_integration'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/data_integration'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'

  - job_name: 'emqx_topic_metrics'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/topic_metrics'
    scheme: 'http'
    basic_auth:
      username: '<API_KEY>'
      password: '<SECRET_KEY>'
```

<a id="configure-push-mode-integration"></a>

## Configure EMQX to Push Metrics to Pushgateway

Push mode sends only the basic metrics and counters available from `/api/v5/prometheus/stats`. Use Pull mode if you need metrics from the other endpoints.

### Enable Pushgateway Delivery in Dashboard

1. Open the Prometheus integration settings in Dashboard.
2. Turn on **Enable Pushgateway**.
3. Enter the Pushgateway server, push interval, job name, and any required HTTP headers.
4. Click **Save Changes**.

Prometheus must also be configured to scrape the Pushgateway instance.

### Enable Pushgateway Delivery in the Configuration File

Alternatively, add the following recommended nested configuration to `etc/base.hocon`:

```hocon
prometheus {
  push_gateway {
    enable = true
    url = "http://127.0.0.1:9091"
    interval = 15s
    headers {}
    job_name = "${name}/instance/${name}~${host}"
  }
}
```

## Visualize EMQX Metrics in Grafana

After Prometheus starts collecting EMQX metrics, import the [EMQX Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/) to visualize them. The template is also available from the **Help** page of the Prometheus integration in Dashboard.

For a complete example, see [Monitoring MQTT broker with Prometheus and Grafana](https://www.emqx.com/en/blog/emqx-prometheus-grafana).
