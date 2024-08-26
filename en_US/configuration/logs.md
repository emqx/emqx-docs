# Logs Configuration

This page introduces how to configure logging behavior for EMQX via the configuration file. You can also configure EMQX logs with Dashboard. To configure with EMQX Dashboard, you can click **Management** -> **Logging** on the left navigation menu to configure. For more detailed descriptions of logs and Dashboard configurations, see [Logs and Observability - Logs](../observability/log.md).

::: tip

This page also introduces the Dashboard UI fields corresponding to the configuration items.
If you configured these items with the Dashboard, the new settings can only temporarily override the same configuration items in `emqx.conf` until the next restart.

:::

EMQX provides support for two primary log handlers: Console Log and File Log, with an additional [Audit Log](../dashboard/audit-log.md) handler specifically designed to always direct logs to files.

The system's default log-handling behavior can be configured via the environment variable `EMQX_DEFAULT_LOG_HANDLER`, which accepts the following settings:

- `file`: Directs log output to files.
- `console`: Channels log output to the console.

Environment variable `EMQX_DEFAULT_LOG_HANDLER` defaults `console`, but explicitly set to `file` when EMQX is initiated via systemd's `emqx.service` file.

## Output Logs as a File

EMQX's log output directory is determined by the environment variable `EMQX_LOG_DIR` which is set to `/var/log/emqx` if installed via RPM or DEB packages. Otherwise, the log directory is `log` in the EMQX installation directory.

For EMQX docker container, the installation directory is `/opt/emqx`, hence the log directory is `/opt/emqx/log`.

To output logs as a file, you may either configure the log handler in the Dashboard or modify the `emqx.conf` file directly as below:

```bash
log {
  file {
    formatter = text
    level = warning
    path = "/var/log/emqx/emqx.log"
    rotation_count = 10
    rotation_size = 50MB
    time_offset = system
    timestamp_format = auto
  }
```

 Where,

| Configuration Item    | Dashboard UI         | Description                                                  | Default Value | Optional Values                                              |
| --------------------- | -------------------- | ------------------------------------------------------------ | ------------- | ------------------------------------------------------------ |
| `formatter`           | Log Formatter        | This sets the log format.                                    | `text`        | `text` is for free text.<br /> `json` is for structured logging. |
| `level`               | Log Level            | This sets the log level of the current log handler, that is, the minimum log level you want to record. | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`                | Log File Name        | This sets the path and name of the log file. <br />By default, EMQX writes the log file to the `emqx.log` file in the `log` directory of the EMQX installation directory. | `emqx.log`    | --                                                           |
| `rotation_count`      | Max Log Files Number | This sets the max number of log files that can be saved.     | `10`          | `1` - `2,048`                                                |
| `rotation_size`       | Rotation Size        | This sets the maximum size of a single log file before it is rotated. The old log file will be renamed and moved to an archive directory once it reached the specified value unless it is set to `infinity`, indicating the log file will not be rotated. | `50MB`        | `1` - `infinity`                                             |
| `time_offset`         | Time Offset          | The time offset relative to UTC in the log.                  | `system`      | --                                                           |
| `timestamp_formatter` | Timestamp Format     | The format of the timestamp in the log.                      | `auto`        | `auto`: Automatically determines the timestamp format based on the log formatter being used. Utilizes `rfc3339` format for text formatters, and `epoch` format for JSON formatters.<br />`epoch`: Microseconds precision Unix epoch format.<br />`rfc3339`: RFC3339 compliant format for date-time strings. |

## Output logs with Console

When EMQX is started in a docker container, the default log handler is `console`.
You can configure the log level and log format with the following configuration items.

```bash
log {
  console {
    formatter = json
    level = warning
    time_offset = system
    timestamp_format = auto
  }
}
```

Where, 

| Configuration Item    | Dashboard UI     | Description                                                  | Default Value | Optional Values                                              |
| --------------------- | ---------------- | ------------------------------------------------------------ | ------------- | ------------------------------------------------------------ |
| `formatter`           | Log Formatter    | This sets the log format.                                    | `text`        | `text` for free text.<br /> `json` for structured logging.   |
| `level`               | Log Level        | This sets the log level of the current log handler, that is, the minimum log level you want to record. | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `time_offset`         | Time Offset      | The time offset relative to UTC in the log.                  | `system`      | --                                                           |
| `timestamp_formatter` | Timestamp Format | The format of the timestamp in the log.                      | `auto`        | `auto`: Automatically determines the timestamp format based on the log formatter being used. Utilizes `rfc3339` format for text formatters, and `epoch` format for JSON formatters.<br />`epoch`: Microseconds precision Unix epoch format.<br />`rfc3339`: RFC3339 compliant format for date-time strings. |

::: tip

EMQX offers more configuration items to serve customized needs better. For details, see the [EMQX Open Source Configuration Manual](https://docs.emqx.com/en/emqx/v@CE_VERSION@/hocon/) and [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::
