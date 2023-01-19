# Integrate with StatsD

[StatsD](https://github.com/statsd/statsd) is a network daemon that listens for statistics. EMQX supports periodic push system metrics to the [StatsD](https://github.com/statsd/statsd) server. 

The push feature is disabled by default, you can enable the option by adding the following configurations to `etc/emqx.conf`:

```bash
statsd {
  ## Enable or disable Statsd data pushing
  enable = false

  ## Statsd server address
  server = "127.0.0.1:8125"

  ## Data collection interval
  sample_time_interval = 10s

  ## Data push interval
  flush_time_interval = 10s
 }

```

You need to restart EMQX after modifying the configuration items to take effect.

## Configure with EMQX Dashboard

Since EMQX 5.0.4, you can use EMQX Dashboard for configuration. In the EMQX Dashboard, click **Configuration** -> **Monitoring** on the left navigation tree, then click the **Integration** tab for the configuration, which takes effect immediately after saving without needing to restart the node.

