# 日志

通过 EMQX 的日志功能，您可查看客户端访问、操作系统或网络异常等问题，如登录错误，异常访问，性能故障等等，并基于日志信息进行问题排查或系统性能优化。此外，为避免日志数据过多或日志写入过慢等问题，EMQX 默认开启了过载保护机制，以确保正常业务不被日志影响。

## 日志级别

EMQX 日志包含 8 个等级 ([RFC 5424](https://www.ietf.org/rfc/rfc5424.txt))，默认为 warning 级别，由低到高分别为：

```bash
debug < info < notice < warning < error < critical < alert < emergency
```

您可通过以下两种方式修改日志配置：

1. 【推荐】通过 EMQX Dashboard 配置。点击左侧导航栏的 **功能配置**-> **日志** 进行配置，保存后生效，无需重启节点。
2. 在 `emqx.conf` 中修改 `log`下的配置项，重启节点后生效。

EMQX 支持将日志输出到控制台或日志文件，二者互不影响，您可同时设置。

## 控制台输出日志

当通过命令行`./bin/emqx console` 或`/bin/emqx/foreground` 启动 EMQX 时，默认会开启向控制台输出`warning`级别的日志，关闭日志文件输出。控制台日志便于开发调试工作。它的配置路径为`log.console_handler`，详细的配置项说明如下：

| 配置项              | 类型 | 说明                                                         |
| ------------------- | ---- | ------------------------------------------------------------ |
| log.console_handler |      | 控制台输出日志处理程序                                       |
| ⌞_ enable           |      | 是否开启控制台输出日志                                       |
| ⌞_ level            |      | 日志等级，默认：`warning`                                    |
| ⌞_ time_offset      |      | 日志中的时间戳使用的时间偏移量，可选值：+2:00、system 或utc  |
| ⌞_ chars_limit      |      | 设置单个日志消息的最大长度。如果超过此长度，日志消息将被截断。最小可设置的长度为100。 |
| ⌞_ formatter        |      | 日志格式类型；可选值：text 或 json                           |
| ⌞_ single_line      |      | 是否单行打印日志，如设置为 true，则单行打印日志；否则，日志消息可跨越多行。 |
| ⌞_ sync_mode_qlen   |      | 过载保护机制的触发阙值，默认：100<br />如果缓冲日志的数量低于该值，所有的日志事件都会被异步处理。也就是说，当客户端程序通过调用 Logger API 发送日志函数后，无需等待日志处理程序的响应即可继续其他操作，因此不会影响正常的业务进程；<br />如果缓冲日志的数量高于该值，所有的日志事件都会被同步处理，即当客户端程序发送日志后，必须等待响应结果才可继续其他操作；当消息队列中的消息数量低于触发阙值时，将恢复异步处理模式。 |
| ⌞_ drop_mode_qlen   |      | 当缓冲的日志事件数大于此值时，新的日志事件将被丢弃。         |
| ⌞_ flush_qlen       |      | 如果缓冲日志事件的增长速度大于该阈值，则会发生冲刷（删除）操作。 缓冲的日志消息将被丢弃，避免影响其它业务进程。您可在日志中查看有多少事件被删除。 |
| ⌞_ overload_kill    |      | 当日志处理进程过载时，强制杀死该进程已保证业务的正常进行。   |
| ⌞_ _ enable         |      | 是否启用日志处理进程强制杀死设置                             |
| ⌞_ _ mem_size       |      | 日志处理进程允许使用的最大内存                               |
| ⌞_ _ qlen           |      | 允许的最大队列长度                                           |
| ⌞_ _ restart_after  |      | 如果处理进程终止，它会在以指定的时间后后自动重新启动。 `infinity` 不自动重启。 |
| ⌞_  burst_limit     |      | 日志限流保护机制                                             |
| ⌞_ _ enable         |      | 开启日志限流保护机制                                         |
| ⌞_ _ max_count      |      | 在 `window_time` 间隔内处理的最大日志事件数。 达到限制后，将连续丢弃日志文件，直到 `window_time` 结束。 |
| ⌞_ _ window_time    |      | 窗口时间                                                     |
| ⌞_ max_depth        |      | Erlang 内部格式日志序列化的最大深度。                        |

示例代码：

```
log.console_handler { 
    enable = false
    level = warning
    time_offset =  system
    chars_limit = unlimited
    formatter = text
    single_line = true
    sync_mode_qlen = 100
    drop_mode_qlen = 3000
    flush_qlen = 8000
    overload_kill {
      enable = true
      mem_size = 30MB
      qlen = 20000
      restart_after = 5s
     }
    burst_limit {
      enable = true
      max_count = 10000
      window_time = 1s
     }  
    max_depth = 100
   }
```

## 文件输出日志

EMQX 日志文件目录默认存放在 `./log` （zip包解压安装方式）或 `/var/log/emqx` ( RPM 或 DEB 包安装）路径下。文件输出日志的配置与控制台基本相同的，只是多了文件写入的控制配置：

| 配置项              | 类型 | 说明                                                         |
| ------------------- | ---- | ------------------------------------------------------------ |
| log.console_handler |      | 文件输出日志处理程序                                         |
| ⌞_ enable           |      | 是否开启文件输出日志                                         |
| ⌞_ level            |      | 日志等级，默认：`warning`                                    |
| ⌞_ file             |      | 文件路径                                                     |
| ⌞_ rotation         |      | 日志轮换功能。启动后生成日志文件后缀会加上对应的索引数字，比如：log/emqx.log.1；如不启用则所有日志只写入到一个文件中。<br /> |
| ⌞_ _ enable         |      | 是否启用日志轮换功能                                         |
| ⌞_ _ count          |      | 轮换的最大日志文件数                                         |
| ⌞_ _ max_size       |      | 此参数控制日志文件轮换。 `infinity` 意味着日志文件将无限增长，否则日志文件将在达到 `max_size`（以字节为单位）时进行轮换，与rotation.count 配合使用。 |

示例代码：

```
    log.file_handlers.default {  
      enable = true
      level = warning
      file = "log/emqx.log"     
      rotation {        
        enable = true
        count = 10
        }
      max_size = 50MB      
 }
```

在文件日志启用后，日志目录下会有如下几种文件:

- **emqx.log.N:** 以 emqx.log 为前缀的文件为日志文件，包含了 EMQX 的所有日志消息。比如 `emqx.log.1`、`emqx.log.2` ...
- **emqx.log.siz 和 emqx.log.idx:** 用于记录日志滚动信息的系统文件，**请不要手动修改**。
- **run_erl.log:** 以 `emqx start` 方式后台启动 EMQX 时，用于记录启动信息的系统文件。
- **erlang.log.N:** 以 erlang.log 为前缀的文件为日志文件，是以 `emqx start` 方式后台启动 EMQX 时，控制台日志的副本文件。比如 `erlang.log.1`、`erlang.log.2` ...

## 针对日志级别输出日志文件

用户也可以通过  `emqx.conf` 将大于或等于某个级别的日志写入到单独的文件中。

例如我们希望将 info 及 info 以上的日志单独输出到 `info.log.N` 文件中：

```
   log.file_handlers.my_info_log {  
      enable = true
      level = info
      file = "log/info.log"     
      rotation {        
        enable = true
        count = 10
        }
      max_size = 50MB      
 }
```

## 日志格式

日志消息的格式为（各个字段之间用空格分隔）：

```
**date time level key-value-struct**
```

其中：

- **date-time:** 当地时间的日期。格式为：`RFC3339`
- **level:** 日志级别，使用中括号包裹。格式为：`[Level]`
- **flat log-content**：扁平化日志消息内容。

### 日志消息举例 1：

```bash
2022-06-30T16:07:47.689512+08:00 [debug] clientid: test, line: 792, mfa: emqx_connection:handle_incoming/2, msg: mqtt_packet_received, packet: PINGREQ(Q0, R0, D0), payload: [], peername: 127.0.0.1:64391, tag: MQTT
```

此日志消息里各个字段分别为:

- **datetime:** `2022-06-30T15:59:19.438914+08:00`
- **level:** `[debug]`
- **flat log-content:** `clientid: test, line: 792, mfa: emqx_connection:handle_incoming/2, msg: mqtt_packet_received, packet: PINGREQ(Q0, R0, D0), payload: [], peername: 127.0.0.1:64391, tag: MQTT`

这条日志表示 EMQX 在 `2022-06-30T16:07:47.689512+08:00` 时 Client ID 为 `test` 客户端收到了一个 `PINGREQ(Q0,R0,D0)` 包。对应客户端的 IP 为 `127.0.0.1:64391`。

### 日志消息举例 2：

```bash
2022-06-30T16:25:32.446873+08:00 [debug] line: 150, mfa: emqx_retainer_mnesia:store_retained/2, msg: message_retained, topic: $SYS/brokers/emqx@127.0.0.1/sysdescr
```

此日志消息里各个字段分别为:

- **date-time:** `2022-06-30T16:25:32.446873+08:00`
- **level:** `[debug]`
- **flat log-content:** `line: 150, mfa: emqx_retainer_mnesia:store_retained/2, msg: message_retained, topic: $SYS/brokers/emqx@127.0.0.1/sysdescr`

