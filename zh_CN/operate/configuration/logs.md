# 日志配置

本页主要介绍如何通过配置文件配置 EMQX 的日志记录行为。您也可以通过 Dashboard 来配置 EMQX 日志。如果通过 Dashboard 配置，您可以点击左侧导航菜单的 **管理** -> **日志** 进行配置。有关日志和 Dashboard 配置的更详细描述，请参见[日志与可观测性 - 日志](../observability/log.md)。

::: tip

本页面还介绍了与配置项对应的 Dashboard UI字段。 如果您通过 Dashboard 配置了这些项，新设置将只能临时覆盖 `emqx.conf` 中相同的配置项，直到下次重启。

:::

EMQX 支持两种主要的日志输出方式：控制台输出日志和文件输出日志。另外还一个[审计日志](../dashboard/audit-log.md)，始终将日志输出定向到文件。

系统的默认日志处理行为可以通过环境变量 `EMQX_DEFAULT_LOG_HANDLER` 配置，它接受以下设置：

- `file`：将日志输出定向到文件。
- `console`：将日志输出定向到控制台。

环境变量 `EMQX_DEFAULT_LOG_HANDLER` 的默认值为 `console`，但当通过 systemd 的 `emqx.service` 文件启动EMQX 时，明确设置为 `file`。

## 文件输出日志

EMQX 的日志输出目录由环境变量 `EMQX_LOG_DIR` 确定，如果通过 RPM 或 DEB 包安装，则设置为 `/var/log/emqx`。否则，日志目录为 EMQX 安装目录下的 `log`。

对于 EMQX docker 容器，安装目录为 `/opt/emqx`，因此日志目录为 `/opt/emqx/log`。

要将日志输出为文件，您可以在 Dashboard 上配置日志处理器或直接修改 `emqx.conf` 文件，如下所示：

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
}
```

其中，

| 配置项             | Dashboard UI     | 描述                                                         | 默认值     | 可选值                                                       |
| ------------------ | ---------------- | ------------------------------------------------------------ | ---------- | ------------------------------------------------------------ |
| `formatter`        | 日志格式类型     | 设置日志格式。                                               | `text`     | `text` 为自由文本。<br /> `json` 为结构化日志。              |
| `level`            | 日志级别         | 设置当前日志处理进程的日志级别，即您想要记录的最低日志级别。 | `warning`  | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `path`             | 日志文件名称     | 设置日志文件的路径和名称。<br />默认情况下，EMQX 将日志文件写入EMQX 安装目录下的 `log` 目录中的 `emqx.log` 文件。 | `emqx.log` | --                                                           |
| `rotation_count`   | 最大日志文件数   | 设置可以保存的最大日志文件数量。                             | `10`       | `1` - `2,048`                                                |
| `rotation_size`    | 日志文件轮换大小 | 在轮换前单个日志文件的最大大小。达到指定值时，旧日志文件将被重命名并移动到归档目录，除非设置为 `infinity`，表示日志文件不会被轮换。 | `50MB`     | `1` - `infinity`                                             |
| `time_offset`      | 时间偏移量       | 定义日志中时间相对 UTC 的偏移量，默认情况下跟随系统。        | `system`   | --                                                           |
| `timestamp_format` | 时间戳格式       | 从下拉列表中选择日志时间戳格式。                             | `auto`     | `auto`: 根据所使用的日志格式类型自动确定时间戳格式。对于文本格式类型，使用 `rfc3339` 格式；对于 JSON 格式类型，则使用 `epoch`格式。<br />`epoch`: 以微秒精度的 Unix 纪元时间格式。<br />`rfc3339`: 符合 RFC3339 标准的日期时间字符串格式。 |

## 控制台输出日志

当 EMQX 在 docker 容器中启动时，默认的日志处理器是 `console`。 您可以通过以下配置项配置日志级别和日志格式。

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

其中，

| 配置项             | Dashboard UI | 描述                                                         | 默认值    | 可选值                                                       |
| ------------------ | ------------ | ------------------------------------------------------------ | --------- | ------------------------------------------------------------ |
| `formatter`        | 日志格式类型 | 设置日志格式。                                               | `text`    | `text` 为自由文本。<br /> `json` 为结构化日志。              |
| `level`            | 日志级别     | 设置当前日志处理进程的日志级别，即您想要记录的最低日志级别。 | `warning` | `debug`, `info`, `notice`, `warning`, `error`, `critical`, `alert`, `emergency` |
| `time_offset`      | 时间偏移量   | 定义日志中时间相对 UTC 的偏移量，默认情况下跟随系统。        | `system`  | --                                                           |
| `timestamp_format` | 时间戳格式   | 从下拉列表中选择日志时间戳格式。                             | `auto`    | `auto`: 对于文本格式类型，使用 `rfc3339` 格式；对于 JSON 格式类型，则使用 `epoch`格式。<br />`epoch`: 以微秒精度的 Unix 纪元时间格式。<br />`rfc3339`: 符合 RFC3339 标准的日期时间字符串格式。 |

::: tip

EMQX 提供了更多配置项以更好地满足定制化需求。详情请参见 [EMQX 开源版配置手册](https://docs.emqx.com/zh/emqx/v@CE_VERSION@/hocon/)和 [EMQX 企业版配置手册](https://docs.emqx.com/zh/enterprise/v@EE_VERSION@/hocon/)。

:::
