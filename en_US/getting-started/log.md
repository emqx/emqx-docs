# Log & Trace

## Control log output

EMQX Broker supports log output to the console or log file, or both, which can be configured in `emqx.conf`:

```
log.to = file
```

The default value of `log.to` is `file`, which has the following optional values:

- **off:** Disable log function completely

- **file:** Only output log to file

- **console:** Only output logs to emqx console

- **both:** Both output logs to emqx console and output log to file

Starting from version 4.3.0, if you deploy EMQX using Docker, you can only view EMQX logs through the `docker logs` command by default. To continue viewing the log file, you can set the environment variable `EMQX_LOG__TO` to `file` or `both` when starting the container.

## Log level

The log of EMQX Broker is divided into 8 levels ([RFC 5424](https://www.ietf.org/rfc/rfc5424.txt)), which are shown from low to high as follows:

```bash
debug < info < notice < warning < error < critical < alert < emergency
```

The default log level of EMQX Broker is warning, which can be modified in `emqx.conf`:

```bash
log.level = warning
```

This configuration sets all log handler to warning.

## log file and log rotation

The default log file directory of EMQX Broker is in `./log` (zip installation) or `/var/log/emqx` (binary installation). It can be configured in `emqx.conf`:

```bash
log.dir = log
```

When file logging is enabled (log.to = file or both), there will be the following files in the log directory:

- **emqx.log.N:** log file prefixed with emqx.log, that contains all the log messages of EMQX Broker, such as `emqx.log.1`,` emqx.log.2` ...
- **emqx.log.siz and emqx.log.idx:** System files used to record log rotation information。
- **run_erl.log:** The system file used to record startup information when starting EMQX Broker in the background with `emqx start`.
- **erlang.log.N:** log file prefixed with erlang.log, which is a copy file of the console log when EMQX Broker is started in the background with `emqx start` , such as `erlang.log.1`,` erlang.log.2` ...

The prefix of the log file can be modified in `emqx.conf`, the default is` emqx.log`:

```bash
log.file = emqx.log
```

EMQX Broker will rotate log files by default when the single log file exceeds 10MB. There can be up to 5 log files: the first log file is emqx.log.1, the second is emqx.log.2, and so on. When the last log file also reaches 10MB, it will be overwritten from the log file with the smallest sequence number. The file size limit and the maximum number of log files can be modified in `emqx.conf`:

```bash
log.rotation.size = 10MB
log.rotation.count = 5
```

## Output log file for log level

If you want to write logs greater than or equal to a certain level to a separate file, you can configure `log.<level>.file` in `emqx.conf`:

Separately output logs of info and above level  to `info.log.N` file:

```bash
log.info.file = info.log
```

Separately output logs of error and above level  to `error.log.N` file:

```bash
log.error.file = error.log
```

## Log format

The maximum character length of a single log message can be modified in `emqx.conf`. If the length exceeds the limit, the log message is truncated and filled with` ... `. The default configuration is not to limit the length:

Set the maximum character length of a single log message to 8192:

```bash
log.chars_limit = 8192
```

The format of the log message (the fields are separated by spaces):

**date time level client_info module_info msg**

- **date:** Local data. The format is: YYYY-MM-DD
- **time:** Local time, accurate to milliseconds. The format is: hh:mm:ss.ms
- **level:** log level, wrapped in brackets. The format is:[Level]
- **client_info:** optional field, only exists if this log message is related to a client The format is: ClientId@Peername or ClientId or Peername
- **module_info:** optional field, only exists if this log message is related to a module. Its format is:[Module Info]
- **msg:** log message content. The format is arbitrary and can contain spaces.

### Log message example 1:

```bash
2020-02-18 16:10:03.872 [debug] <<"mqttjs_9e49354bb3">>@127.0.0.1:57105 [MQTT/WS] SEND CONNACK(Q0, R0, D0, AckFlags=0, ReasonCode=0)
```

The fields in this log message are:

- **date:** `2020-02-18`
- **time:** `16:10:03.872`
- **level:** `[debug]`
- **client_info:** `<<"mqttjs_9e49354bb3">>@127.0.0.1:57105`
- **module_info:** `[MQTT/WS]`
- **msg:** `SEND CONNACK(Q0, R0, D0, AckFlags=0, ReasonCode=0)`

### Log message example 2:

```bash
2020-02-18 16:10:08.474 [warning] [Alarm Handler] New Alarm: system_memory_high_watermark, Alarm Info: []
```

The fields in this log message are:

- **date:** `2020-02-18`
- **time:** `16:10:08.474`
- **level:** `[warning]`
- **module_info:** `[Alarm Handler]`
- **msg:** `New Alarm: system_memory_high_watermark, Alarm Info: []`

Note that in this log message, the client_info field does not exist.

## log level and log handlers

EMQX Broker uses a hierarchical log system. At the log level, it includes primary log level and the log level of each log handler.

```bash
     [Primary Level]        -- global log level and filters
           / \
[Handler 1]  [Handler 2]    -- log levels and filters at each handler
```

The log handler is the working process responsible for log processing and output. It is uniquely identified by the log handler id and has the following tasks:

- What level of logs to receive
- How to filter log messages
- Where to output logs

The log handlers installed by default in emqx:

```bash
$ emqx_ctl log handlers list

LogHandler(id=ssl_handler, level=debug, destination=console)
LogHandler(id=file, level=debug, destination=log/emqx.log)
LogHandler(id=default, level=debug, destination=console)
```

- file: The log handler responsible for output to the log file. There is no special filtering conditions, that is, all log messages are output as long as the level meets the requirements. The output destination is a log file.
- default: the log handler responsible for output to the console. There is no special filtering conditions, that is, all log messages are output as long as the level meets the requirements. The output destination is the console.
- ssl_handler: ssl's log handler. Its filter condition is set to output when the log is from the ssl module. The output destination is the console.

Before the log message is output, we should check whether the message level is higher than the primary log level. After passing the check, the log message flows into each log handler. Then, we should check the log level of each handler. If the log message is higher than the handler level, the corresponding handler performs Filter conditions. When it is passed, output is performed.


Imagine a scenario where the primary log level is set to info, the log handler `default` is set to debug, and the log handler `file` is set to warning:

- Although the console log is at the debug level, at this time the console log can only output messages to the level info and above. That is because after the primary level filtering, the logs flowing to the default and file only belong to the level of info and above;
- The emqx.log.N file contains log messages at warning and above level .

The "log.level" mentioned in the  [Log Level](#log-levels) section is the modified global log level. This includes the primary log level and the log level of each handler, all of which is set to the same value.

Primary Log Level is equivalent to the main switch of a tap water pipe system. Once closed, no water flow will pass through each branch pipe. This mechanism ensures the high-performance operation of the logging system.

## Modify log level at runtime

You can use EMQX Broker's command line tool `emqx_ctl` to modify the emqx log level at runtime:

### Modify the global log level:

For example, set the level of primary log level and all log handlers to debug:

```bash
$ emqx_ctl log set-level debug
```

### Modify the primary log level:

For example, set the primary log level to debug:

```bash
$ emqx_ctl log primary-level debug
```

### Modify the log level of a log handler:

For example, set log handler `file` to debug:

```bash
$ emqx_ctl log handlers set-level file debug
```

## Log trace

EMQX Broker supports filtering logs for ClientID or Topic and outputting to files. Before using the log tracing function, the primary log level must be set to debug:

```bash
$ emqx_ctl log primary-level debug
```

Enable ClientID log tracing, and output all logs with ClientID 'my_client' to log/my_client.log:

```bash
$ emqx_ctl log primary-level debug
debug

$ emqx_ctl trace start client my_client log/my_client.log
trace clientid my_client successfully
```

Enable the topic log tracing, and output the message publishing log whose topic can match 't/#' to log/topic_t.log:

```bash
$ emqx_ctl log primary-level debug
debug

$ emqx_ctl trace start topic 't/#' log/topic_t.log
trace topic t/# successfully
```

::: tip
Even if `log.level` is set to error in `emqx.conf`,  debug level information of a client or topic can still be printed out with the message tracing function.  This is very useful in a production environment.
:::

### The principle of log tracing

The principle of log tracing is to install a new log handler for emqx and set the filter conditions of the handler. In the [Log Levels and log handlers](#log-level-and-log-handlers) section, we discussed the details of log handlers.

For example, use the following command to enable client log tracing:

```bash
$ emqx_ctl log primary-level debug && emqx_ctl trace start client my_client log/my_client.log
```

Then check the tracing that has been started:

```bash
$ emqx_ctl trace list
Trace(clientid=my_client, level=debug, destination="log/my_client.log")
```

In the background, emqx will install a new log handler and specify the filter conditions as follows: Only when the ClientID is "my_client", the log will be output:

```bash
$ emqx_ctl log handlers list
LogHandler(id=trace_clientid_my_client, level=debug, destination=log/my_client.log)
...
```

The id of the newly added log handler here is trace_clientid_my_client, and the handler level is debug. This is why before trace, we must set the primary log level to debug.

If the default primary log level (warning) is used, this log handler will never output the log messages below warning level.

In addition, since we are enabling a new log handler, our log tracing is not constrained by the level of console logs and emqx.log.N file logs. Even if log.level = warning, we can still trace the debug level log of my_client.
