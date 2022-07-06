# 日志

日志为系统进行排错，优化系统的性能提供可靠的信息来源，日志即可记录业务问题，比如登录错误，异常访问等。还能记录操作系统或网络中所发生事件的信息，包括性能、故障检测。

EMQX在日志数据过多或日志写入过慢时，默认启动过载保护机制，最大限度的保证正常业务不被日志影响。

## 日志级别

EMQX 的日志分 8 个等级 ([RFC 5424](https://www.ietf.org/rfc/rfc5424.txt))，由低到高分别为：

```bash
debug < info < notice < warning < error < critical < alert < emergency
```

EMQX 的默认日志级别为 warning。

通过以下两种方式进行日志配置修改。

1. 在 `emqx.conf` 中配置`log`下的配置项，重启节点后生效。
2. 在 dashboard 上功能配置/日志中进行配置，保存后生效，无需重启节点，推荐使用。

EMQX 支持将日志输出到控制台或者日志文件，输出控制台和文件互不影响，也可同时设置。

## 控制台输出日志

当使用前台启动( `./bin/emqx console` 或`/bin/emqx/foreground` )时，默认开启控制台输出`warning`级别的日志，关闭日志文件输出。输出到控制台可以方便开发调试。它的配置路径为`log.console_handler`，详细的配置项说明如下：

```
log.console_handler { 
    ## 是否开始控制台输出日志，前台模式下会默认开启   
    enable = false
    ## 日志等级
    level = warning
    ## 日志中的时间戳使用的时间偏移量，比如：+2:00 或system 或utc
    time_offset =  system
    ## 设置单个日志消息的最大长度。 如果超过此长度，则日志消息将被截断。最小可设置的长度为100。
    chars_limit = unlimited
    ## 日志格式类型：text 或者 json格式
    formatter = text
    ## 如果设置为 true，则单行打印日志。 否则，日志消息可能跨越多行。
    single_line = true
    ## 过载保护机制：只要缓冲的日志事件的数量低于这个值，所有的日志事件都会被异步处理。
    ## 这意味着，日志落地速度不会影响正常的业务进程，因为它们不需要等待日志处理进程的响应。
    ## 如果消息队列的增长超过了这个值，处理程序开始同步处理日志事件。
    ## 也就是说，发送事件的客户进程必须等待响应。当处理程序将消息队列减少到低于sync_mode_qlen阈值的水平时，异步操作就会恢复。
    ## 默认为100条信息，当等待的日志事件大于100条时，就开始同步处理日志。
    sync_mode_qlen = 100
    ## 当缓冲的日志事件数大于此值时，新的日志事件将被丢弃。
    drop_mode_qlen = 3000
    ## 如果缓冲日志事件的数量增长大于此阈值，则会发生冲刷（删除）操作。 日志处理进程会丢弃缓冲的日志消息。
    ## 来缓解自身不会由于内存暴涨而影响其它业务进程。日志内容会提醒有多少事件被删除。
    flush_qlen = 8000
    overload_kill {
      ## 日志处理进程过载时为保护业务能正常，强制杀死日志处理进程。
      enable = true
      ## 日志处理进程允许使用的最大内存
      mem_size = 30MB
      ## 允许的最大队列长度
      qlen = 20000
      ## 如果处理进程终止，它会在以指定的时间后后自动重新启动。 `infinity` 不自动重启。
      restart_after = 5s
     }
    burst_limit {
      ## 启用日志限流保护机制
      enable = true
      ## 在 `window_time` 间隔内处理的最大日志事件数。 达到限制后，将丢弃连续事件，直到 `window_time` 结束。
      max_count = 10000
      window_time = 1s
     }  
    ## Erlang 内部格式日志序列化的最大深度。
    max_depth = 100
   }
```

## 文件输出日志

EMQX 的默认日志文件目录在 `./log` (zip包解压安装) 或者 `/var/log/emqx` (二进制包安装)。文件输出日志的配置与控制台输出的配置大部分是相同的，只是多了文件写入的控制配置，下面只列出除控制台输出外新增加的配置项：

```
    log.file_handlers.default {  
      ## 启用此文件日志处理进程
      enable = true
      ## 日志等级
      level = warning
      ## 文件路径
      file = "log/emqx.log"     
      rotation {        
        ## 启用日志轮换功能。启动后生成日志文件后缀会加上对应的索引数字，比如：log/emqx.log.1。
        ## 系统会默认生成emqx.log.siz/emqx.log.idx，用于记录日志位置，请不要手动修改这两个文件。
        ## 如果不启用则所有日志只写入到一个文件中。
        enable = true
        ## 轮换的最大日志文件数。
        count = 10
        }
      ## 此参数控制日志文件轮换。 `infinity` 意味着日志文件将无限增长，否则日志文件将在达到 `max_size`（以字节为单位）时进行轮换。
      ## 与 rotation.count配合使用。如果 count 为 10。
      max_size = 50MB      
 }
```

在文件日志启用后，日志目录下会有如下几种文件:

- **emqx.log.N:** 以 emqx.log 为前缀的文件为日志文件，包含了 EMQX 的所有日志消息。比如 `emqx.log.1`, `emqx.log.2` ...
- **emqx.log.siz 和 emqx.log.idx:** 用于记录日志滚动信息的系统文件，请不要手动修改。
- **run_erl.log:** 以 `emqx start` 方式后台启动 EMQX 时，用于记录启动信息的系统文件。
- **erlang.log.N:** 以 erlang.log 为前缀的文件为日志文件，是以 `emqx start` 方式后台启动 EMQX 时，控制台日志的副本文件。比如 `erlang.log.1`, `erlang.log.2` ...

## 针对日志级别输出日志文件

如果想把大于或等于某个级别的日志写入到单独的文件，可以在 `emqx.conf` 中配置 

将 info 及 info 以上的日志单独输出到 `info.log.N` 文件中：

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

日志消息的格式为(各个字段之间用空格分隔)：

**date time level key-value-struct**

- **date-time:** 当地时间的日期。格式为：`RFC3339`
- **level:** 日志级别，使用中括号包裹。格式为：`[Level]`
- **flat log-content** 扁平化日志消息内容。

### 日志消息举例 1：

```bash
2022-06-30T16:07:47.689512+08:00 [debug] clientid: test, line: 792, mfa: emqx_connection:handle_incoming/2, msg: mqtt_packet_received, packet: PINGREQ(Q0, R0, D0), payload: [], peername: 127.0.0.1:64391, tag: MQTT
```

此日志消息里各个字段分别为:

- **datetime:** `2022-06-30T15:59:19.438914+08:00`
- **level:** `[debug]`
- **flat log-content:** `clientid: test, line: 792, mfa: emqx_connection:handle_incoming/2, msg: mqtt_packet_received, packet: PINGREQ(Q0, R0, D0), payload: [], peername: 127.0.0.1:64391, tag: MQTT`

这条日志表示 EMQX 在`2022-06-30T16:07:47.689512+08:00`时 clientid 为 `test`客户端收到了一个`PINGREQ(Q0,R0,D0)`包。对应客户端的IP为`127.0.0.1:64391`。

### 日志消息举例 2：

```bash
2022-06-30T16:25:32.446873+08:00 [debug] line: 150, mfa: emqx_retainer_mnesia:store_retained/2, msg: message_retained, topic: $SYS/brokers/emqx@127.0.0.1/sysdescr
```

此日志消息里各个字段分别为:

- **date-time:** `2022-06-30T16:25:32.446873+08:00`
- **level:** `[debug]`
- **flat log-content:** `line: 150, mfa: emqx_retainer_mnesia:store_retained/2, msg: message_retained, topic: $SYS/brokers/emqx@127.0.0.1/sysdescr`

