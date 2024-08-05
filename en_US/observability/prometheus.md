# Integrate with Prometheus

EMQX supports integration with third-party monitoring systems, such as [Prometheus](https://prometheus.io/). It is the monitoring solution open-sourced by SoundCloud. It offers a versatile set of features including support for multidimensional data models, flexible query language, and powerful alarm management.

Using a third-party monitoring system can bring the following advantages:

- A complete monitoring system, where the monitoring data of EMQX will be integrated with that of the other systems. For example, you can get the monitoring information of the server host;
- More intuitive monitoring report with figures and charts, such as using [Grafana dashboard](#use-grafana-to-visualize-EMQX-metrics) to visualize the EMQX metrics;
- Various alarm notification options, such as using Prometheus Alertmanager to set up alarm rules and notification methods.

EMQX supports two methods for integrating Prometheus metrics monitoring:

- **Pull Mode**: Prometheus directly collects metrics through EMQX's REST API.
- **Push Mode**: EMQX pushes metrics to the Pushgateway service, from which Prometheus collects the metrics.

This page introduces the configuration steps for both methods. You can click **Management** -> **Monitoring** in the left navigation menu of the EMQX Dashboard, and in the **Integration** tab, select **Prometheus** to perform the configuration. You can also click the **Help** button on the page to view specific configuration steps for each mode.

## Configure Pull Mode Integration

EMQX provides the following REST APIs for Prometheus to collect system metrics:

- `/api/v5/prometheus/stats`: Basic metrics and counters of EMQX.
- `/api/v5/prometheus/auth`: Key metrics and counters in access control, including authentication and authorization.
- `/api/v5/prometheus/data_integration`: Metrics and counters related to the rule engine, connectors, actions, Sink/Source, and encoding/decoding.

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

For more information about Prometheus pull endpoints, refer to the [EMQX Open Source API documentation](https://docs.emqx.com/en/emqx/v@CE_MINOR_VERSION@/admin/api-docs.html) and [EMQX Enterprise API documentation](https://docs.emqx.com/en/enterprise/v@EE_MINOR_VERSION@/admin/api-docs.html).

::: tip 

By default, the pull mode API does not require authentication. You can configure the **Enable Basic Auth** switch on the page to enable basic authentication for the interface. Once enabled, you need to create an [API key](../admin/api.md#authentication) on EMQX and apply it to the Prometheus configuration to obtain metric data.

:::

### Prometheus Configuration for Reference

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

EMQX supports pushing metrics to Pushgateway, from which Prometheus can then collect these metrics. EMQX supports pushing metrics to Pushgateway, which is disabled by default. To enable the Pushgateway service, you can click the **Enable Pushgateway** toggle switch on the Prometheus configuration page in the Dashboard. 

<img src="./assets/enable-push-gateway.png" alt="enable-push-gateway" style="zoom:40%;" />

Configure the following fields according to your business needs, and then click **Save Changes**. 

- **Interval**: Specify the time interval for reporting the monitoring metrics data to Pushgateway. The default value it `15` seconds.
- **Pushgateway Server**: Type the URL of Prometheus server. It is `http://127.0.0.1:9091` by default.
- **Job Name**: Specify variables that include the EMQX node name and hostname. The default value is `${name}/instance/${name}~${host}`. For example, when the EMQX node name is `emqx@127.0.0.1`, the `name` variable takes the value `emqx` and the `host` variable takes the value `127.0.0.1`.
- **Headers**: Type the key and value of the HTTP headers for the monitoring metrics that are pushed to Pushgateway. You can add a list of headers by clicking the **Add** button. The type is string, for example, { Authorization = "some-authz-tokens"}.

At the same time, you can click the **Help** button and refer to the steps on the **Use Pushgateway** tab for configuration.

::: tip 

The Push mode currently only includes EMQX's basic metrics and counters from the `/api/v5/prometheus/stats` endpoint, so the Pull mode is more recommended.

:::

You can also enable and configure the Pushgateway by adding the following configurations to `etc/emqx.conf`. For more information on configuration items, see [Configuration - Prometheus](../configuration/prometheus.md).

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
