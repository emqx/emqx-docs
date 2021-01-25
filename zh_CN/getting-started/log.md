---
# 编写日期
date: 2020-02-18 13:52:34
# 作者 Github 名称
author: terry-xiaoyu
# 关键字
keywords:
# 描述
description:
# 分类
category: 
# 引用
ref:
---

# 日志与追踪
## 控制日志输出
EMQ X 支持将日志输出到控制台或者日志文件，或者同时使用两者。可在 `emqx.conf` 中配置：
```
log.to = both
```
`log.to` 默认值是 both，可选的值为：

- **off:** 完全关闭日志功能

- **file:** 仅将日志输出到文件

- **console:** 仅将日志输出到标准输出(emqx 控制台)

- **both:** 同时将日志输出到文件和标准输出(emqx 控制台)

## 日志级别
EMQ X 的日志分 8 个等级 ([RFC 5424](https://www.ietf.org/rfc/rfc5424.txt))，由低到高分别为：

```bash
debug < info < notice < warning < error < critical < alert < emergency
```

EMQ X 的默认日志级别为 warning，可在 `emqx.conf` 中修改：

```bash
log.level = warning
```

此配置将所有 log handler 的配置设置为 warning。

## 日志文件和日志滚动
EMQ X 的默认日志文件目录在 `./log` (zip包解压安装) 或者 `/var/log/emqx` (二进制包安装)。可在 `emqx.conf` 中配置：

```bash
log.dir = log
```

在文件日志启用的情况下 (log.to = file 或 both)，日志目录下会有如下几种文件:

- **emqx.log.N:** 以 emqx.log 为前缀的文件为日志文件，包含了 EMQ X 的所有日志消息。比如 `emqx.log.1`, `emqx.log.2` ...
- **emqx.log.siz 和 emqx.log.idx:** 用于记录日志滚动信息的系统文件。
- **run_erl.log:** 以 `emqx start` 方式后台启动 EMQ X 时，用于记录启动信息的系统文件。
- **erlang.log.N:** 以 erlang.log 为前缀的文件为日志文件，是以 `emqx start` 方式后台启动 EMQ X 时，控制台日志的副本文件。比如 `erlang.log.1`, `erlang.log.2` ...

可在 `emqx.conf` 中修改日志文件的前缀，默认为 `emqx.log`：

```bash
log.file = emqx.log
```

EMQ X 默认在单日志文件超过 10MB 的情况下，滚动日志文件，最多可有 5 个日志文件：第 1 个日志文件为 emqx.log.1，第 2 个为 emqx.log.2，并以此类推。当最后一个日志文件也写满 10MB 的时候，将从序号最小的日志的文件开始覆盖。文件大小限制和最大日志文件个数可在 `emqx.conf` 中修改：

```bash
log.rotation.size = 10MB
log.rotation.count = 5
```

## 针对日志级别输出日志文件
如果想把大于或等于某个级别的日志写入到单独的文件，可以在 `emqx.conf` 中配置 `log.<level>.file`：

将 info 及 info 以上的日志单独输出到 `info.log.N` 文件中：

```bash
log.info.file = info.log
```

将 error 及 error 以上的日志单独输出到 `error.log.N` 文件中

```bash
log.error.file = error.log
```

## 日志格式
可在 `emqx.conf` 中修改单个日志消息的最大字符长度，如长度超过限制则截断日志消息并用 `...` 填充。默认不限制长度：

将单个日志消息的最大字符长度设置为 8192:

```bash
log.chars_limit = 8192
```

日志消息的格式为(各个字段之间用空格分隔)：

**date time level client_info module_info msg**

- **date:** 当地时间的日期。格式为：YYYY-MM-DD
- **time:** 当地时间，精确到毫秒。格式为：hh:mm:ss.ms
- **level:** 日志级别，使用中括号包裹。格式为：[Level]
- **client_info:** 可选字段，仅当此日志消息与某个客户端相关时存在。其格式为：ClientId@Peername 或 ClientId 或 Peername
- **module_info:** 可选字段，仅当此日志消息与某个模块相关时存在。其格式为：[Module Info]
- **msg:** 日志消息内容。格式任意，可包含空格。

### 日志消息举例 1：

```bash
2020-02-18 16:10:03.872 [debug] <<"mqttjs_9e49354bb3">>@127.0.0.1:57105 [MQTT/WS] SEND CONNACK(Q0, R0, D0, AckFlags=0, ReasonCode=0)
```

此日志消息里各个字段分别为:

- **date:** `2020-02-18`
- **time:** `16:10:03.872`
- **level:** `[debug]`
- **client_info:** `<<"mqttjs_9e49354bb3">>@127.0.0.1:57105`
- **module_info:** `[MQTT/WS]`
- **msg:** `SEND CONNACK(Q0, R0, D0, AckFlags=0, ReasonCode=0)`

### 日志消息举例 2：

```bash
2020-02-18 16:10:08.474 [warning] [Alarm Handler] New Alarm: system_memory_high_watermark, Alarm Info: []
```

此日志消息里各个字段分别为:

- **date:** `2020-02-18`
- **time:** `16:10:08.474`
- **level:** `[warning]`
- **module_info:** `[Alarm Handler]`
- **msg:** `New Alarm: system_memory_high_watermark, Alarm Info: []`

注意此日志消息中，client_info 字段不存在。

## 日志级别和 log handlers
EMQ X 使用了分层的日志系统，在日志级别上，包括全局日志级别 (primary log level)、以及各 log hanlder 的日志级别。

```bash
     [Primary Level]        -- global log level and filters
           / \
[Handler 1]  [Handler 2]    -- log levels and filters at each handler
```

log handler 是负责日志处理和输出的工作进程，它由 log handler id 唯一标识，并负有如下任务：

- 接收什么级别的日志
- 如何过滤日志消息
- 将日志输出到什么地方

我们来看一下 emqx 默认安装的 log handlers:

```bash
$ emqx_ctl log handlers list

LogHandler(id=ssl_handler, level=debug, destination=console, status=started)
LogHandler(id=file, level=warning, destination=log/emqx.log, status=started)
LogHandler(id=default, level=warning, destination=console, status=started)
```

- file: 负责输出到日志文件的 log handler。它没有设置特殊过滤条件，即所有日志消息只要级别满足要求就输出。输出目的地为日志文件。
- default: 负责输出到控制台的 log handler。它没有设置特殊过滤条件，即所有日志消息只要级别满足要求就输出。输出目的地为控制台。
- ssl_handler: ssl 的 log handler。它的过滤条件设置为当日志是来自 ssl 模块时输出。输出目的地为控制台。

日志消息输出前，首先检查消息是否高于 primary log level，日志消息通过检查后流入各 log handler，再检查各 handler 的日志级别，如果日志消息也高于 handler level，则由对应的 handler 执行相应的过滤条件，过滤条件通过则输出。


设想一个场景，假设 primary log level 设置为 info，log handler `default` (负责输出到控制台) 的级别设置为 debug，log handler `file` (负责输出到文件) 的级别设置为 warning：

- 虽然 console 日志是 debug 级别，但此时 console 日志只能输出 info 以及 info 以上的消息，因为经过 primary level 过滤之后，流到 default 和 file 的日志只剩下 info 及以上的级别；
- emqx.log.N 文件里面，包含了 warning 以及 warning 以上的日志消息。

在 [日志级别](#log-levels) 章节中提到的 `log.level` 是修改了全局的日志级别。这包括 primary log level 和各个 handlers 的日志级别，都设置为了同一个值。

Primary Log Level 相当于一个自来水管道系统的总开关，一旦关闭则各个分支管道都不再有水流通过。这个机制保证了日志系统的高性能运作。

## 运行时修改日志级别
你可以使用 EMQ X 的命令行工具 `emqx_ctl` 在运行时修改 emqx 的日志级别：

### 修改全局日志级别：

例如，将 primary log level 以及所有 log handlers 的级别设置为 debug：

```bash
$ emqx_ctl log set-level debug
```

### 修改主日志级别：

例如，将 primary log level 设置为 debug:

```bash
$ emqx_ctl log primary-level debug
```

### 修改某个 log handler 的日志级别：

例如，将 log handler `file` 设置为 debug:

```bash
$ emqx_ctl log handlers set-level file debug
```

### 停止某个 log handler：

例如，为了让日志不再输出到 console，可以停止 log handler `default`:

```bash
$ emqx_ctl log handlers stop default
```

### 启动某个已经停止的 log handler：

例如，启动上面已停止的 log handler `default`:

```bash
$ emqx_ctl log handlers start default
```

## 日志追踪
EMQ X 支持针对 ClientID 或 Topic 过滤日志并输出到文件。在使用日志追踪功能之前，必须将 primary log level 设置为 debug：

```bash
$ emqx_ctl log primary-level debug
```

开启 ClientID 日志追踪，将所有 ClientID 为 'my_client' 的日志都输出到 log/my_client.log:

```bash
$ emqx_ctl log primary-level debug
debug

$ emqx_ctl trace start client my_client log/my_client.log
trace clientid my_client successfully
```

开启 Topic 日志追踪，将主题能匹配到 't/#' 的消息发布日志输出到 log/topic_t.log:

```bash
$ emqx_ctl log primary-level debug
debug

$ emqx_ctl trace start topic 't/#' log/topic_t.log
trace topic t/# successfully
```

::: tip
即使 `emqx.conf` 中，`log.level` 设置为 error，使用消息追踪功能仍然能够打印出某 client 或 topic 的 debug 级别的信息。这在生产环境中非常有用。
:::

### 日志追踪的原理
日志追踪的原理是给 emqx 安装一个新的 log handler，并设置 handler 的过滤条件。在 [日志级别和 log handlers](#log-level-and-log-handlers) 小节，我们讨论过 log handler 的细节。

比如使用如下命令启用 client 日志追踪：

```bash
$ emqx_ctl log primary-level debug && emqx_ctl trace start client my_client log/my_client.log
```

然后查询已经开启的追踪:

```bash
$ emqx_ctl trace list
Trace(clientid=my_client, level=debug, destination="log/my_client.log", status=started)
```

在后台，emqx 会安装一个新的 log handler，并给其指定过滤条件为：仅当 ClientID 为 "my_client" 的时候，输出日志：

```bash
$ emqx_ctl log handlers list
LogHandler(id=trace_clientid_my_client, level=debug, destination=log/my_client.log, status=started)
...
```

这里看到新添加的 log handler 的 id 为 trace_clientid_my_client，并且 handler level 为 debug。这就是为什么在 trace 之前，我们必须将 primary log level 设置为 debug。

如果使用默认的 primary log level (warning)，这个log handler 永远不会输出 warning 以下的日志消息。

另外，由于我们是启用了一个新的 log handler，所以我们的日志追踪不受控制台日志和 emqx.log.N 文件日志的级别的约束。即使 log.level = warning，我们任然可以追踪到 my_client 的 debug 级别的日志。
