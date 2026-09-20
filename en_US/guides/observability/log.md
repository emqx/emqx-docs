# Logs

Logs provide a reliable source of information for troubleshooting and system performance optimization. You can find the record about the access, operating, or network issues from EMQX logs.

EMQX supports both console logs and file logs. There are two different ways of outputting log data. You can choose the output method as needed or keep both. Console log refers to outputting log data to the console or command line interface. It is typically used during development and debugging, as it allows developers to quickly view log data in real-time as EMQX runs. File log refers to outputting log data to a file. This is typically used in production environments, where it is important to persist log data over time for analysis and troubleshooting.

The system's default log output can be configured via the environment variable `EMQX_DEFAULT_LOG_HANDLER`, which accepts the following settings:

- `file`: Directs log output to files.
- `console`: Channels log output to the console.

Environment variable `EMQX_DEFAULT_LOG_HANDLER` defaults `console`, but explicitly set to `file` when EMQX is initiated via systemd's `emqx.service` file.

To minimize the impact of logs on system operation, for example, when the log data is too much or the log writing is too slow, EMQX activates the overload protection mechanism by default to better serve our users.

## Log Level

EMQX log has 6 out of the 8 levels ([RFC 5424](https://www.ietf.org/rfc/rfc5424.txt)), with warning as the default level, from low to high these levels are:

```bash
debug < info < notice < warning < error < critical
```
The table below describes the meaning and output contents for each log level.

| Log Level | Meaning                                                      | Output Examples                                              |
| --------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| debug     | Detailed information about the internal workings of a program, helping to debug and diagnose issues in the code. <br />It is not recommended to output this level of logging directly to the production environment. Instead, enable [Log Trace](./tracer.md) for a specific client. | Variable values, function call stacks, and other detailed debugging data. |
| info      | Useful information that is more general than debug-level logs. | Minor abnormalities such as authorization denial, and management operation results, such as a successful configuration change. |
| notice    | Important system information indicating that an event has occurred, but no action is required. | Component restarted per request from dashboard or CLI |
| warning   | Potential issues or errors that require action; typically used for proactive monitoring and detecting potential problems before they become critical issues. | Disconnections, connection timeouts, authentication failures, and other similar events. |
| error     | The occurrence of an error that requires error handling; typically used to flag errors so that administrators can quickly detect and resolve issues. | Fails to connect to an external database, to subscribe to a non-existent topic, or to parse a configuration file, or other similar events. |
| critical  | Critical error that results in system crashes or prevents it from functioning; typically used to flag severe problems so that administrators can take immediate action. | A component is unable to start or function normally due to incorrect configuration. |

::: warning Important Notice

Raw MQTT packet data in connection and parser-error logs is redacted by default. To temporarily log raw packet data for troubleshooting, add trusted client IP addresses or CIDR ranges to the listener's `allow_log_packet_data_from` option. Enable this option only for trusted clients and only during diagnostics, because raw packet data can contain credentials and other sensitive information.

:::

## Configure Logging via Dashboard

This section mainly describes how to configure logging with EMQX Dashboard. Changes take effect immediately without restarting the node.

Go to EMQX Dashboard. Click **Management** -> **Logging** on the left navigation menu. Select the corresponding tab for configurations on the console log or file log.

### Configure Console Log

On the **Logging** page, select the **Console Log** tab.

<img src="./assets/config-console-log-1-ee.png" alt="config-console-log-1-ee" style="zoom:67%;" />

Configure the following settings for the console log output:

- **Enable Log Output**: Click the toggle switch to enable the console log output.

- **Log Level**: Select the minimum log level to record. Available values are `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, and `emergency`. The default value is `warning`.

- **Log Formatter**: Select the log format. Available values are `text` for free-form text and `json` for structured logs. The default value is `text`.

- **Timestamp Format**: Select the format of the timestamp in the log. Optional values are:
  - `auto`: Automatically determines the timestamp format based on the log formatter being used. Utilizes `rfc3339` format for text formatters, and `epoch` format for JSON formatters.

  - `epoch`: Represents timestamps in microseconds precision Unix epoch format.
  - `rfc3339`: Uses RFC3339 compliant format for date-time strings. For example, `2024-03-26T11:52:19.777087+00:00`.

- **Time Offset**: Set the time offset used to format log timestamps. Enter `system` to use the local system offset, `utc` to use UTC, or a fixed offset in the `+-[hh]:[mm]` format, such as `-02:00` or `+00:00`. The default value is `system`. This setting does not affect JSON logs because their timestamps use the Unix epoch format.

- **Payload Encode**: Select how payload data is encoded in log entries. Available values are:

  - `text`: Uses text encoding. This value is recommended for text-based protocols and JSON-encoded payloads.
  - `hex`: Uses hexadecimal encoding. This value is recommended for custom binary protocols.
  - `hidden`: Replaces the payload with `******`.

  The default value is `text`.

After you finish the configurations, click **Save Changes**.

### Configure File Log

On the **Logging** page, select the **File Log** tab.

<img src="./assets/config-file-log-1-ee.png" alt="config-file-log-1-ee" style="zoom:67%;" />

Configure the following settings for the file log output:

- **Enable Log Output**: Click the toggle switch to enable the file log output.

- **Log File Name**: Enter the path and name of the log file. The default value is `${EMQX_LOG_DIR}/emqx.log`, where `${EMQX_LOG_DIR}` is the EMQX log directory.

- **Max Log Files Number**: Specify the maximum number of rotated log files. The default value is `10`.

- **Rotation Size**: Set the maximum size of a log file before rotation. Enter a value and select `KB`, `MB`, or `GB`. The default value is `50 MB`. If you turn off the toggle, the value becomes `infinity`, and the log file grows without size-based rotation.

- **Log Level**: Select the minimum log level to record. Available values are `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, and `emergency`. The default value is `warning`.

- **Log Formatter**: Select the log format. Available values are `text` for free-form text and `json` for structured logs. The default value is `text`.

- **Timestamp Format**: Select the format of the timestamp in the log. Optional values are:

  - `auto`: Automatically determines the timestamp format based on the log formatter being used. Utilizes `rfc3339` format for text formatters, and `epoch` format for JSON formatters.

  - `epoch`: Represents timestamps in microseconds precision Unix epoch format.

  - `rfc3339`: Uses RFC3339 compliant format for date-time strings. For example, `2024-03-26T11:52:19.777087+00:00`.

- **Time Offset**: Set the time offset used to format log timestamps. Enter `system` to use the local system offset, `utc` to use UTC, or a fixed offset in the `+-[hh]:[mm]` format, such as `-02:00` or `+00:00`. The default value is `system`. This setting does not affect JSON logs because their timestamps use the Unix epoch format.

- **Payload Encode**: Select how payload data is encoded in log entries. Available values are:

  - `text`: Uses text encoding. This value is recommended for text-based protocols and JSON-encoded payloads.
  - `hex`: Uses hexadecimal encoding. This value is recommended for custom binary protocols.
  - `hidden`: Replaces the payload with `******`.

  The default value is `text`.

After you finish the configurations, click **Save Changes**.

When file logging is enabled (log.to = file or both), the following files will appear in the log directory:

- **emqx.log.N:** Log file prefixed with emqx.log, that contains all the log messages of EMQX, such as `emqx.log.1`,` emqx.log.2` ...
- **emqx.log.siz and emqx.log.idx:** System files used to record log rotation information. **Do not change manually**.

## Configure Logging via Configuration File

You can also configure EMQX logging through configuration files. For example, if you want to export the warning-level logs to a file or output with a console, you can modify the configuration items under `log` in `base.hocon` as shown below. The configuration takes effect after the node restarts. For more information on configuring logging with configuration files, see [Configuration - Logs](../configuration/logs.md).

```bash
log {
  file {
    default {
      enable = true
      formatter = text
      level = warning
      path = "/Users/emqx/Downloads/emqx-560/log/emqx.log"
      rotation_count = 10
      rotation_size = 50MB
      time_offset = system
      timestamp_format = auto
  }
  console {
    formatter = json
    level = debug
    time_offset = system
    timestamp_format = auto
  }
}
```

## Log Format

The format of the log message (with different fields separated by spaces) is as follows:

```
**timestamp level tag clientid msg peername username ...**
```

where,

- **timestamp:** An RFC-3339 formatted timestamp indicating when the log entry was created.
- **level:** The severity level of the log, enclosed in brackets. Format: [level], which can be standard log levels such as `info`, `warning`, `error`, etc.
- **tag:** All-uppercase single word used for categorizing logs for easier searching and analysis, e.g., MQTT, AUTHN, AUTHZ
- **clientid:** Included only when the log is about a specific client. Identifies the client related to the log entry.
- **msg:** The content of the log message. To enhance searchability and readability, most messages adopt a `snake_case` formatting style,
      such as `mqtt_packet_received`. Note: Not all messages follow this format; some may vary.
- **peername:** The client's source IP address and port number in `IP:port` format, indicating the connection origin.
- **username:** Present only for logs associated with a client having a specified non-empty username. Indicates the username of the client involved.
- **...:** Additional arbitrary fields may follow the msg field, providing more context or details as needed.

### Log Message Example

```bash
2024-03-20T11:08:39.568980+01:00 [warning] tag: AUTHZ, clientid: client1, msg: cannot_publish_to_topic_due_to_not_authorized, peername: 127.0.0.1:47860, username: user1, topic: republish-event/1, reason: not_authorized
```

## Log Throttling

Log Throttling is a feature designed to mitigate the risk of log flooding by limiting the logging of repeated events within a specified time window. By only logging the first event and suppressing subsequent identical events within this window, log management becomes more efficient without sacrificing observability.

You can configure the throttling time window through the Dashboard by selecting **Management** -> **Logging** and clicking the **Throttling** tab. The default time window is set to 1 minute, with a minimum allowable value of 1 second.

<img src="./assets/log_throttling-ee.png" alt="log_throttling-ee" style="zoom:67%;" />

 You can also directly configure the time window in the configuration file as follows:

```
log {
  throttling {
    time_window = "5m"
  }
}
```

Log throttling is enabled by default and applies to selected log events such as authorization failures or message queue overflows. However, when the log level for `console` or `file` is set to debug, throttling is disabled to ensure detailed logging for troubleshooting.

Throttling is applied only to the following log events:

- "authentication_failure"
- "authorization_permission_denied"
- "cannot_publish_to_topic_due_to_not_authorized"
- "cannot_publish_to_topic_due_to_quota_exceeded"
- "connection_rejected_due_to_license_limit_reached"
- "data_bridge_buffer_overflow"
- "dropped_msg_due_to_mqueue_is_full"
- "dropped_qos0_msg"
- "external_broker_crashed"
- "failed_to_fetch_crl"
- "failed_to_retain_message"
- "handle_resource_metrics_failed"
- "retain_failed_for_payload_size_exceeded_limit"
- "retain_failed_for_rate_exceeded_limit"
- "retained_delete_failed_for_rate_exceeded_limit"
- "socket_receive_paused_by_rate_limit"
- "transformation_failed"
- "unrecoverable_resource_error"
- "validation_failed"

::: tip Note
The list of throttled events is subject to updates.
:::

If any events are throttled within a time window, a summary warning message will log the count of dropped events for each type. For example, if 5 unauthorized subscription attempts occur within a window, the following events will be logged:

```
2024-03-13T15:45:11.707574+02:00 [warning] clientid: test, msg: authorization_permission_denied, peername: 127.0.0.1:54870, username: test, topic: t/#, action: SUBSCRIBE(Q0), source: file
2024-03-13T15:45:53.634909+02:00 [warning] msg: log_events_throttled_during_last_period, period: 1 minutes, 0 seconds, dropped: #{authorization_permission_denied => 4}
```

As you can see, the first "authorization_permission_denied" event is fully logged. The next 4 similar events are dropped but their number is recorded in "log_events_throttled_during_last_period" statistics.

## Centralize Logs in Production

In production, send logs from every EMQX node to a central system outside the EMQX cluster. Logs kept only on the broker host may become unavailable when the node or its storage fails. Central collection also makes it possible to correlate events across Core and Replicant nodes and to alert on conditions that are not exposed as metrics or built-in alarms.

### Choose a Collection Method

Use one of the following collection patterns:

- In a containerized deployment, such as Kubernetes, write JSON logs to the console and use the platform's logging agent to collect the container output.
- For file logging, use a log agent that collects `emqx.log.N` files, handles rotation without duplicating records, and preserves the structured fields.
- Use the [OpenTelemetry log handler](./opentelemetry/logs.md) to export logs to an OpenTelemetry Collector and a compatible backend.

### Add Context and Protect Logs

Add deployment metadata such as cluster, node, node role, EMQX version, and availability zone in the collection pipeline.

Protect centralized logs as operational data. Log fields can contain client IDs, usernames, topics, peer addresses, and error details.

### Monitor the Collection Pipeline

Monitor the collection path by using collector and transport health metrics or an explicit heartbeat that does not depend on application log volume. Configure alerts for the following conditions:

- The collector or transport is unhealthy.
- The collector or transport rejects or drops records.
- The central backend approaches its storage limits.

Do not alert merely because a reachable EMQX node produces no logs. An idle or healthy node may have nothing to report at the configured severity.

### Define a Log Alerting Policy

Create log-based alerts selectively and match stable structured fields such as `level` and `msg`.

- **Warning events:** These events are often useful as early-warning signals, but some can be caused by expected client behavior. Use a rate or deviation from the normal baseline where individual events do not require action.
- **Error or critical events:** Events that indicate loss of replication, configuration synchronization, listener startup, or durable storage should normally alert immediately.

For a recommended set of metric- and log-based alerts, including Mria replication signals, see [Production Monitoring Best Practices](./monitoring-best-practices.md#centralize-logs-and-alert-selectively).
