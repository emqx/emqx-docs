# Logs Configuration

This page introduces how to configure logging behavior for EMQX via the configuration file. You can also configure EMQX logs with Dashboard. To configure with EMQX Dashboard, you can click **Management** -> **Logging** on the left navigation menu to configure. For more detailed descriptions of logs and Dashboard configurations, see [Logs and Observability - Logs](../observability/log.md).

::: tip

This page also introduces the Dashboard UI fields corresponding to the configuration items.
If you want to configure logs from config files, it is recommended to use `base.hocon` instead of `emqx.conf`.
This is because if the configuration is set in emqx.conf, any changes made through the Dashboard will only be temporary and will be lost when EMQX restarts.

:::

EMQX provides support for two primary log outputs: Console Log and File Log, with an additional [Audit Log](../dashboard/audit-log.md) output that always directs logs to files.

The system's default log output can be configured via the environment variable `EMQX_DEFAULT_LOG_HANDLER`, which accepts the following settings:

- `file`: Directs log output to files.
- `console`: Channels log output to the console.

Environment variable `EMQX_DEFAULT_LOG_HANDLER` defaults `console`, but explicitly set to `file` when EMQX is initiated via systemd's `emqx.service` file.

## Output Logs as a File

For RPM and DEB installations, `EMQX_LOG_DIR` defaults to `/var/log/emqx`. Starting from EMQX 6.3.0, `/opt/emqx/log` is a symlink to this directory. The symlink always points to `/var/log/emqx`. Changing `EMQX_LOG_DIR` does not update it.

For other installation methods, the default log directory is `log` under the EMQX installation directory. In a Docker container, this path is `/opt/emqx/log`.

To output logs as a file, you may either configure the file log output in the Dashboard or modify the `base.hocon` file directly as below:

```bash
log {
  file {
    enable = true
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
| `level`               | Log Level            | This sets the log level of the current log output, that is, the minimum log level you want to record. | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`                | Log File Name        | This sets the path and name of the log file. <br />By default, EMQX writes the log file to the `emqx.log` file in the `log` directory of the EMQX installation directory. | `emqx.log`    | --                                                           |
| `rotation_count`      | Max Log Files Number | This sets the max number of log files that can be saved.     | `10`          | `1` - `2,048`                                                |
| `rotation_size`       | Rotation Size        | This sets the maximum size of a single log file before it is rotated. The old log file will be renamed and moved to an archive directory once it reached the specified value unless it is set to `infinity`, indicating the log file will not be rotated. | `50MB`        | `1` - `infinity`                                             |
| `time_offset`         | Time Offset          | The time offset relative to UTC in the log.                  | `system`      | --                                                           |
| `timestamp_format` | Timestamp Format     | The format of the timestamp in the log.                      | `auto`        | `auto`: Automatically determines the timestamp format based on the log formatter being used. Utilizes `rfc3339` format for text formatters, and `epoch` format for JSON formatters.<br />`epoch`: Microseconds precision Unix epoch format.<br />`rfc3339`: RFC3339 compliant format for date-time strings. |

## Output logs with Console

When EMQX is started in a Docker container, the default log output is `console`.
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
| `level`               | Log Level        | This sets the log level of the current log output, that is, the minimum log level you want to record. | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `time_offset`         | Time Offset      | The time offset relative to UTC in the log.                  | `system`      | --                                                           |
| `timestamp_format` | Timestamp Format | The format of the timestamp in the log.                      | `auto`        | `auto`: Automatically determines the timestamp format based on the log formatter being used. Utilizes `rfc3339` format for text formatters, and `epoch` format for JSON formatters.<br />`epoch`: Microseconds precision Unix epoch format.<br />`rfc3339`: RFC3339 compliant format for date-time strings. |

::: tip

EMQX offers more configuration items to better serve customized needs. For details, see the [EMQX Enterprise Configuration Manual for Enterprise](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::

## Crash Dumps in Docker

When the Erlang VM terminates abnormally, it writes a crash dump to `erl_crash.<timestamp>.dump` in the log directory, which is `/opt/emqx/log` in the container. The file records the state of the node at the moment it went down, and it is the primary evidence for troubleshooting a crash.

Console logging does not preserve the crash dump file. The console log handler writes runtime logs to the container's standard output, which you can view with `docker logs`. Crash dumps are written to files separately. If the log directory is not mounted, the dump is lost when the container is removed.

Before you start EMQX, create a host directory and make it writable by the `emqx` user in the container (UID 1000):

```bash
mkdir -p $PWD/log && sudo chown 1000:1000 $PWD/log
```

Then mount the directory at `/opt/emqx/log` when you start EMQX:

```bash
docker run -d --name emqx \
  -v $PWD/log:/opt/emqx/log \
  emqx/emqx-enterprise:@EE_VERSION@
```

After a crash, run the following command to check the container output:

```bash
docker logs emqx
```

If the following line ends with `done`, the dump file is complete:

```text
Crash dump is being written to: /opt/emqx/log/erl_crash.2026.08.31.06.56.22.dump...done
```

A crash dump can be tens of megabytes. Consider the disk space of the mounted directory, and set `node.crash_dump_bytes` to limit the file size.
