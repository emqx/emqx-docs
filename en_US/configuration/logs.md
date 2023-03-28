# Logs

Logs provide a reliable source of information for troubleshooting and system performance optimization. You can find the record about the access, operating, or network issues from EMQX logs.

EMQX log has 8 levels, with warning as the default level, from low to high these 8 levels are:

```
debug < info < notice < warning < error < critical < alert < emergency
```

<!-- for a more detailed description of each log level, see [Log and observability - Log](https://docs.emqx.com/en/enterprise/v5.0/observability/log.html). -->

You can configure EMQX logs with Dashboard or configuration files. To configure with EMQX Dashboard, you can click **Configuration** -> **Log** on the left navigation menu to configure. 

This section introduces how to configure logs with configuration items. For example, if you want to export the logs of warning levels as both a file and output with a console, you can work with the following configuration items. 

```bash
log {
  file_handlers.default {
    level = warning
    file = "log/emqx.log"
    count = 10
    max_size = 50MB
    formatter = text
  }
  console_handler {
    level = warning
    formatter = text
  }
}
```

## Output Logs as a File

To output logs as a file, you can work with the command below:

Code example:

```bash
log {
  file_handlers.default {
    level = warning
    file = "log/emqx.log"
    count = 10
    max_size = 50MB
    formatter = text
  }
```

 Where,

| Configuration Item                         | Description                                                  | Default Value | Optional Values                                              |
| ------------------------------------------ | ------------------------------------------------------------ | ------------- | ------------------------------------------------------------ |
| **File Handler** (`file_handlers.default`) | This sets whether to enable using the file-based log handler for logging purposes, once enabled, it will write the log messages to a specified file on the disk (configured by `file`). | `enabled`     | `enable`, `disable`                                          |
| **Log Level** (`level` )                   | This sets the log level of the current log handler, that is, the minimum log level you want to record. | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| **Log File Name** (`file`)                 | This sets the name of the log file, <br>By default, EMQX writes the log file to the `emqx.log` file in the `log` directory of the EMQX installation directory. | `emqx.log`    | --                                                           |
| **Max Log Files Number** (`count`)         | This sets the max number of log files that can be saved.     | `10`          | `1` ~ `2,048`                                                |
| **Rotation Size** (`max_size`)             | This sets the maximum size of a single log file before it is rotated. The old log file will be renamed and moved to an archive directory once it reached the specified value unless it is set to `infinity`, indicating the log file will not be rotated. | `50MB`        | `1` ~ `infinity`                                             |
| **Log Formatter** (`formatted`)            | This sets the log format.                                    | `text`        | `text` for free text<br> `json` for structured logging       |

## Output log with Console

You can also use the command below to configure the logs with the console:

```bash
log {
  console_handler {
    level = warning
    formatter = text
  }
}
```

Where, 

| Configuration Item                               | Description                                                  | Default Value | Optional Values                                              |
| ------------------------------------------------ | ------------------------------------------------------------ | ------------- | ------------------------------------------------------------ |
| **Enable Log Handler** (`file_handlers.default`) | This sets whether to enable outputting logs with the console. | `enabled`     | `enable`, `disable`                                          |
| **Log Level** (`level` )                         | This sets the log level of the current log handler, that is, the minimum log level you want to record. | `warning`     | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| **Log Formatter** (`formatted`)                  | This sets the log format                                     | `text`        | `text` for free text<br> `json` for structured logging       |