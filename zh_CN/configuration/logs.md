# Logs

This page introduces how to configure logging behaviour for EMQX. You can configure EMQX logs with Dashboard or configuration files. To configure with EMQX Dashboard, you can click **Management** -> **Log** on the left navigation menu to configure. For more detailed descriptions of logs and Dashboard configurations, see [Logs and observability - Logs](../observability/log.md).

::: tip

This page also introduces the Dashboard UI fields corresponding to the configuration items.
If you configured these items with the Dashboard, the new settings can only temporarily override the same configuration items in `emqx.conf` until the next restart.

:::

EMQX provides support for two primary log handlers: `file` and `console`, with an additional `audit` handler specifically designed to always direct logs to files.

The system's default log handling behavior can be configured via the environment variable `EMQX_DEFAULT_LOG_HANDLER`, which accepts the following settings:

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
    enable = true
    level = warning
    path = "/var/log/emqx/emqx.log"
    rotation_count = 10
    rotation_size = 50MB
    formatter = text
  }
```

 Where,

| Configuration Item | Dashboard UI         | Description                                                  | Default Value | Optional Values                                              |
| ------------------ | -------------------- | ------------------------------------------------------------ | ------------- | ------------------------------------------------------------ |
| `level`            | Log Level            | This sets the log level of the current log handler, that is, the minimum log level you want to record. | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`             | Log File Name        | This sets the path and name of the log file. <br />By default, EMQX writes the log file to the `emqx.log` file in the `log` directory of the EMQX installation directory. | `emqx.log`    | --                                                           |
| `rotation_count`   | Max Log Files Number | This sets the max number of log files that can be saved.     | `10`          | `1` ~ `2,048`                                                |
| `rotation_size`    | Rotation Size        | This sets the maximum size of a single log file before it is rotated. The old log file will be renamed and moved to an archive directory once it reached the specified value unless it is set to `infinity`, indicating the log file will not be rotated. | `50MB`        | `1` ~ `infinity`                                             |
| `formatter`        | Log Formatter        | This sets the log format.                                    | `text`        | `text` for free text<br /> `json` for structured logging     |

## Output logs with Console

When EMQX is started in a docker container, the default log handler is `console`.
You can configure the log level and log format with the following configuration items.

```bash
log {
  console {
    enable = true
    level = warning
    formatter = text
  }
}
```

Where, 

| Configuration Item      | Dashboard UI       | Description                                                  | Default Value | Optional Values                                              |
| ----------------------- | ------------------ | ------------------------------------------------------------ | ------------- | ------------------------------------------------------------ |
| `file_handlers.default` | Enable Log Handler | This sets whether to enable outputting logs with the console. | `enabled`     | `enable`, `disable`                                          |
| `level`                 | Log Level          | This sets the log level of the current log handler, that is, the minimum log level you want to record. | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `formatter`             | Log Formatter      | This sets the log format.                                    | `text`        | `text` for free text<br /> `json` for structured logging     |

{% emqxce %}

::: tip

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://www.emqx.io/docs/en/v${CE_VERSION}/hocon/).

:::

{% endemqxce %}

{% emqxee %}

::: tip

EMQX has offered more configuration items to serve customized needs better. For details, see [Configuration Manual](https://docs.emqx.com/en/enterprise/v@EE_VERSION@/hocon/).

:::

{% endemqxee %}
