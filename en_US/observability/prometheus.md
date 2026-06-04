# Integrate with Prometheus

EMQX supports integration with third-party monitoring systems such as [Prometheus](https://prometheus.io/), an open-source monitoring solution originally developed by SoundCloud. Prometheus provides a multidimensional data model, flexible query language (PromQL), and powerful alerting capabilities.

Using a third-party monitoring system can bring the following advantages:

- A complete monitoring system, where the monitoring data of EMQX will be integrated with that of the other systems. For example, you can get the monitoring information of the server host.
- More intuitive monitoring report with figures and charts, such as using [Grafana dashboard](#use-grafana-to-visualize-EMQX-metrics) to visualize the EMQX metrics.
- Various alarm notification options, such as using Prometheus Alertmanager to set up alarm rules and notification methods.

EMQX supports two methods for integrating Prometheus metrics monitoring:

- **Pull Mode**: Prometheus directly collects metrics through EMQX's REST API.
- **Push Mode**: EMQX pushes metrics to the Pushgateway service, from which Prometheus collects the metrics.
<<<<<<< HEAD

To configure Prometheus integration:

1. Go to **Management** -> **Monitoring** in the EMQX Dashboard.
2. Switch to the **Integration** tab.
3. Select **Prometheus** as the monitoring platform.

Depending on the selected mode, some configuration options apply only to Pull mode, while others affect both modes. You can click the **Help** button on the Dashboard page to view detailed configuration steps for each mode.

<img src="./assets/enable-push-gateway.png" alt="enable-push-gateway" style="zoom:40%;" />

## Prometheus Configuration Options

This section describes all configuration options available when selecting **Prometheus** in the Dashboard.

### General Options (Affect Both Pull and Push Modes)

#### Latency Buckets

Specify histogram bucket boundaries for latency-related metrics.

**Format**

A comma-separated list of duration values:

```
10ms, 100ms, 1s, 5s, 30s
```

**Description**

These values define how latency metrics are grouped into histogram buckets in Prometheus. Smaller bucket intervals provide finer granularity but may increase metric cardinality and storage usage.

This setting affects how latency histogram metrics are generated internally and applies to:

- Pull mode metrics
- Push mode metrics (via Pushgateway)

### Pull Mode Settings

The following options apply only when Prometheus scrapes EMQX metrics via REST APIs.

#### Enable Basic Auth

Enable or disable HTTP Basic Authentication for Prometheus scrape APIs.

By default, Prometheus Pull mode APIs do not require authentication. When this option is enabled:

- Prometheus must use HTTP Basic Authentication to access:
  - `/api/v5/prometheus/stats`
  - `/api/v5/prometheus/auth`
  - `/api/v5/prometheus/data_integration`
- You must create an [API Key](../admin/api.md#authentication) in EMQX.
- Configure the `basic_auth` section in your `prometheus.yaml`.

This option applies only to Pull mode and does not affect Pushgateway integration. For details, see [Configure Pull Mode Integration](#configure-pull-mode-integration).

#### Namespace Data Scraping Rate Limit

Limit the maximum request rate when scraping namespace-related metrics.

Namespace-level metrics are supported in multi-tenant deployments and can be exposed or aggregated by namespace. For details, see [Prometheus Metrics Isolation](../multi-tenancy/namespace-overview.md#multi-tenancy-capability-support).

**Format**: `<requests>/<duration>`

**Example**: `1/5s`, which means at most 1 request is allowed every 5 seconds. Additional requests within the time window will be rejected.

**Behavior**:

- Applies only to namespace-level metric scraping requests.
- Requests targeting specific namespaces are not limited.
- Applies only to Pull mode.

This option helps prevent excessive load in large-scale or multi-namespace deployments.

### Push Mode Settings

Push mode allows EMQX to send metrics to a Pushgateway instance. By default, Push mode is disabled.

#### Enable Pushgateway

Enable or disable metric pushing to Pushgateway. When enabled, configure the following fields:

#### Interval

Specify how often EMQX pushes metrics to Pushgateway. The default value is `15` seconds.

#### Pushgateway Server

Specify the Pushgateway server URL. It is `http://127.0.0.1:9091` by default.

#### Job Name

Specify the job label used when pushing metrics to Pushgateway.

You can construct the job label using variables derived from the EMQX node name and hostname. The default value is: `${name}/instance/${name}~${host}`.

**Variables**:

- `${name}`: EMQX node name (e.g., `emqx`)
- `${host}`: Host IP address (e.g., `127.0.0.1`)

For example, if the node name is `emqx@127.0.0.1`, then:

- `${name}` = `emqx`
- `${host}` = `127.0.0.1`

#### Headers

Optional HTTP headers sent when pushing metrics to Pushgateway.

The value type is string. You can configure headers as key-value pairs, for example:

```
Authorization = "some-auth-token"
```

Click **Add** to insert additional headers.
=======
This page introduces the configuration steps for both methods. For a curated reference of the metric series exposed on the endpoints below — including the ones worth alerting on — see [Broker Health Indicators](./broker-health-indicators.md). You can click **Management** -> **Monitoring** in the left navigation menu of the EMQX Dashboard, and in the **Integration** tab, select **Prometheus** to perform the configuration. You can also click the **Help** button on the page to view specific configuration steps for each mode.
>>>>>>> origin/release-6.0

## Configure Pull Mode Integration

In Pull mode, Prometheus scrapes metrics from EMQX through REST APIs.

EMQX provides the following endpoints for metric collection:

- `/api/v5/prometheus/stats`: Basic metrics and counters of EMQX.
- `/api/v5/prometheus/auth`: Key metrics and counters in access control, including authentication and authorization.
- `/api/v5/prometheus/data_integration`: Metrics and counters related to the rule engine, connectors, actions, Sink/Source, and encoding/decoding.

### Metric Collection Modes

When calling the above APIs to obtain metrics, you can use the URL query parameter `mode` to get different types of metric data. The meanings of different parameters are as follows:

:::: tabs type: card

::: tab Single Node Mode

```
mode=node
```

The default mode returns the metrics of the current request node. If no specific mode is specified, the system defaults to returning metrics in this mode.

:::

::: tab Cluster Aggregated Mode

```
mode=all_nodes_aggregated
```

Aggregate cluster metrics, returning the *arithmetic sum* or *logical sum* of all running node metrics in the cluster.

- For metrics like "on status" and "running status", the system will return their logical sum, i.e., returns 1 if all nodes are on or running, otherwise returns 0.

- Some metrics are independent on different nodes and will not return aggregated values. For example, CPU and memory usage. The system will add node names as labels to distinguish the metrics of different nodes. For example:

  ```bash
  emqx_vm_cpu_use{node="emqx@172.17.0.2"} 7.6669163995887715
  emqx_vm_cpu_idle{node="emqx@172.17.0.2"} 92.33308360041123
  
  emqx_vm_cpu_use{node="emqx@172.17.0.3"} 7.676007766679973
  emqx_vm_cpu_idle{node="emqx@172.17.0.3"} 92.32399223332003
  ```

- Some metrics should have consistent values on any node in the cluster. For cluster-consistent metrics, the value on the node that accepts the API request will be returned directly. They are not summed and do not include node names as labels. For example:

  ```bash
  emqx_topics_count 3
  emqx_cert_expiry_at{listener_type="ssl",listener_name="default"} 1904285225
  emqx_cert_expiry_at{listener_type="wss",listener_name="default"} 1904285225
  ```

- Other metrics return arithmetic sums, i.e., the returned metrics are the sum of all node metrics.

:::

::: tab Cluster Unaggregated Mode

```
mode=all_nodes_unaggregated
```

This is the cluster unaggregated metric mode, returning the individual metrics of all running nodes in the cluster.

- The system will add node names as labels to distinguish the metrics of different nodes. For example:

  ```bash
  emqx_connections_count{node="emqx@127.0.0.1"} 0
  ```

- Some metrics should have consistent values on any node in the cluster. For example, "blacklist count", "retained message count", etc. For cluster-consistent metrics, the value on the node that accepts the API request will be returned directly. Node names are not included as labels. For example:

  ```bash
  emqx_retained_count 3
  ```

:::

::::

For more information about Prometheus pull endpoints, refer to the [EMQX Enterprise API documentation](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html).

### Authentication (Optional)

By default, Prometheus Pull mode APIs do not require authentication.

If **Enable Basic Auth** is enabled in the EMQX Dashboard, Prometheus must authenticate using HTTP Basic Authentication.

In this case:

1. Create an [API key](../admin/api.md#authentication) in EMQX.
2. Use the generated API Key and Secret Key in the Prometheus configuration.

In the Prometheus configuration:

```yaml
basic_auth:
  username: '<API_KEY>'
  password: '<SECRET_KEY>'
```

Where:

- `username` is the API Key.
- `password` is the corresponding Secret Key.

Prometheus will use these credentials when scraping EMQX metrics.

### Prometheus Server Configuration Example

To enable Prometheus to scrape EMQX metrics, configure the Prometheus server.

Add the following configuration to your Prometheus configuration file, then restart the Prometheus service.

```yaml
# prometheus.yaml
global:
  scrape_interval:     10s # The default scrape interval is every 10 seconds.
  evaluation_interval: 10s # The default evaluation interval is every 10 seconds.
  # On this machine, every time series will be exported by default.
  external_labels:
    monitor: 'emqx-monitor'
scrape_configs:
  - job_name: 'emqx_stats'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/stats'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''

  - job_name: 'emqx_auth'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/auth'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''

  - job_name: 'emqx_data_integration'
    static_configs:
      - targets: ['127.0.0.1:18083']
    metrics_path: '/api/v5/prometheus/data_integration'
    scheme: 'http'
    basic_auth:
      username: ''
      password: ''
```

## Configure Push Mode Integration

Push mode sends metrics from EMQX to Pushgateway.

After enabling **Enable Pushgateway** in the Dashboard and configuring the required fields, click **Save Changes**.

The Push mode currently only includes EMQX's basic metrics and counters from the `/api/v5/prometheus/stats` endpoint. For comprehensive monitoring, Pull mode is generally recommended.

### Configuration File Example

You can also enable and configure the Pushgateway by adding the following configuration to the configuration file. For more information on configuration items, see [Configuration - Prometheus](../configuration/prometheus.md).

```bash
prometheus {
  push_gateway_server = "http://127.0.0.1:9091"
  interval = 15s
  headers {}
  job_name = "${name}/instance/${name}~${host}"
}
```

## Use Grafana to Visualize EMQX Metrics

You can also use Grafana with Prometheus to visualize EMQX metrics, which can be achieved by importing the EMQX template files into Grafana. To download the template, click [EMQX | Grafana Dashboard](https://grafana.com/grafana/dashboards/17446-emqx/) or click the **Help** button at the bottom of the **Integration** tab of the **Monitoring** page.

::: tip

For detailed operating steps, see [Monitoring MQTT broker with Prometheus and Grafana](https://www.emqx.com/en/blog/emqx-prometheus-grafana)

:::
