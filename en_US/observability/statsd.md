# StatsD

EMQX can be configured to push system metrics to the  via `etc/emqx.conf`.

EMQX supports periodic push system metrics to the [StatsD](https://github.com/statsd/statsd) server. This function can be opened by configuration file:

```
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

You need to restart EMQX server after modifying the configuration items to take effect.

## Configure Through Dashboard

After v5.0.4, EMQX also supports direct modification via Dashboard‘s **Configuration/Monitoring Integration**, which takes effect directly after saving, without restarting the node.


